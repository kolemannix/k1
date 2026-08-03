// Copyright (c) 2026 knix
// All rights reserved.

use std::ffi::c_void;

use crate::debug;
use ahash::HashMapExt;
use colored::Colorize;
use fxhash::FxHashMap;

pub(crate) mod vm_ffi;
#[cfg(test)]
mod vm_test;

use crate::ir;
use crate::typer::types::{
    ContainerKind, FloatType, IntegerType, Layout, POINTER_TYPE_ID, PhysicalType, PhysicalTypeEnum,
    PhysicalTypeResult, ScalarType, Type, TypeId,
};
use crate::typer::{
    ErrorKind, FunctionId, GlobalInitialValue, K1Message, K1Result, MessageLevel, StaticContainer,
    StaticContainerKind, StaticStruct, StaticSum, StaticValue, StaticValueId, StaticValuePool,
    TypedFloatValue, TypedGlobalId, TypedIntValue, TypedProgram,
};
use crate::{
    ice_span, kbail, kerr,
    kmem::{self, MSlice},
    lex::SpanId,
    parse::StringId,
};

macro_rules! vm_ice {
    ($k1:expr, $vm:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            vm_crash($k1, $vm, &s)
        }
    };
}

macro_rules! casted_uop {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            8 => {
                let r = ($lhs as u8).$op($rhs as u8);
                r as u64
            }
            16 => {
                let r = ($lhs as u16).$op($rhs as u16);
                r as u64
            }
            32 => {
                let r = ($lhs as u32).$op($rhs as u32);
                r as u64
            }
            64 => {
                let r = ($lhs as u64).$op($rhs as u64);
                r
            }
            _ => unreachable!(),
        }
    };
}

macro_rules! casted_iop {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            8 => {
                let r = ($lhs as i8).$op($rhs as i8);
                r as i64
            }
            16 => {
                let r = ($lhs as i16).$op($rhs as i16);
                r as i64
            }
            32 => {
                let r = ($lhs as i32).$op($rhs as i32);
                r as i64
            }
            64 => {
                let r = ($lhs as i64).$op($rhs as i64);
                r
            }
            _ => unreachable!(),
        }
    };
}

macro_rules! casted_float_op {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            32 => {
                let r = f32::from_bits($lhs as u32).$op(f32::from_bits($rhs as u32));
                r.to_bits() as u64
            }
            64 => {
                let r = f64::from_bits($lhs).$op(f64::from_bits($rhs));
                r.to_bits()
            }
            _ => unreachable!(),
        }
    };
}

// Shared with the bc VM (bc/exec.rs) so arithmetic semantics cannot drift
pub(crate) use {casted_float_op, casted_iop, casted_uop};

/// Bit-for-bit mappings of K1 types
#[allow(non_snake_case)]
pub mod k1_types {

    #[derive(Clone, Copy)]
    #[repr(C)]
    pub struct Arena {
        pub basePtr: *const u8,
        pub curAddr: u64,
        pub endAddr: u64,
        pub allocKind: u64,
        pub name: K1BufferLike,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1SourceLocation {
        pub filename: K1BufferLike,
        pub line: u64,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    /// Encompasses all 3 'core' contiguous buffer types in k1:
    /// - buffer, span, and string, same layout
    /// - It's even 'list-compatible' since list starts with a buffer.
    pub struct K1BufferLike {
        pub data: *mut u8,
        pub len: i64,
    }

    /// k1 list is { buffer, len }: the buffer's len is the capacity,
    /// the trailing len is the element count
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1List {
        pub k1_buffer: K1BufferLike,
        pub len: i64,
    }

    /// core/code.k1 `code-chunk`
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1CodeChunk {
        pub text: K1BufferLike,
        pub source: u64,
    }

    /// core/code.k1 `code`
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1Code {
        pub chunks: K1List,
    }

    #[derive(Clone, Copy)]
    #[repr(u8)]
    pub enum CompilerMessageLevel {
        Info = 0,
        Warn = 1,
        Error = 2,
    }

    impl CompilerMessageLevel {
        pub fn from_u8(v: u8) -> Option<Self> {
            match v {
                x if x == Self::Info as u8 => Some(Self::Info),
                x if x == Self::Warn as u8 => Some(Self::Warn),
                x if x == Self::Error as u8 => Some(Self::Error),
                _ => None,
            }
        }
    }

    impl K1BufferLike {
        ///# Safety
        /// None of this is safe
        pub unsafe fn to_slice<'a, T>(self) -> &'a [T] {
            unsafe { std::slice::from_raw_parts(self.data as *const T, self.len as usize) }
        }

        ///# Safety
        /// Really make sure its a char buffer
        pub unsafe fn to_str<'a>(self) -> Result<&'a str, &'static str> {
            if self.data.is_null() {
                if self.len == 0 {
                    Ok("")
                } else {
                    Err("Null, non-empty K1 span cannot be converted to Rust str")
                }
            } else {
                unsafe {
                    let slice = self.to_slice();
                    Ok(std::str::from_utf8(slice).unwrap())
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VmFfiHandle {
    #[allow(unused)]
    library_handle: *mut c_void,
    function_pointer: *mut c_void,
    cif: libffi::raw::ffi_cif,
}

#[derive(Clone)]
pub struct CompilerMessage {
    level: MessageLevel,
    message: StringId,
    filename: String,
    line: u32,
}

/// A repl command issued by cell code during execution (`k1/repl/*`
/// builtins); the megarepl engine drains these after each run
#[derive(Clone, Copy)]
pub enum ReplCommand {
    Checkbox { name: StringId, get: FunctionId, set: FunctionId },
}

pub struct Vm {
    pub(crate) globals: FxHashMap<TypedGlobalId, Value>,
    pub static_stack: Stack,
    pub stack: Stack,
    pub(crate) eval_span: SpanId,
    pub(crate) compiler_messages: Vec<CompilerMessage>,
    pub(crate) repl_commands: Vec<ReplCommand>,
    /// When set, emitted messages are only recorded, not printed to the
    /// console; the megarepl harvests them as the cell's stdout/stderr
    pub quiet_messages: bool,
    // This is just the first valid address in `stack`, before the first frame
    pub(crate) overall_return_addr: *mut u8,
    /// (fp, pc) at the point of a bc execution error, for bc stack traces
    pub(crate) bc_fault: Option<(u64, u32)>,
}

impl Vm {
    pub fn reset(&mut self, arena_global_id: Option<TypedGlobalId>) {
        // Note that we don't de-allocate any resources
        // we just zero the memory and reset the stack pointer

        // The arena-tmp global is an 8-byte cell holding a *arena; read the
        // pointer out before we wipe the static stack the cell lives in
        let arena_ptr_to_preserve: Option<*mut k1_types::Arena> = arena_global_id
            .and_then(|gid| self.globals.get(&gid))
            .map(|cell| unsafe { cell.as_ptr().cast::<*mut k1_types::Arena>().read() })
            .filter(|p| !p.is_null());

        self.stack.reset();
        self.static_stack.reset();
        self.globals.clear();
        self.compiler_messages.clear();
        self.repl_commands.clear();
        self.overall_return_addr = core::ptr::null_mut();
        self.bc_fault = None;

        if let Some(arena_ptr) = arena_ptr_to_preserve {
            debug!("Preserving core/mem/arena allocation at {:p}", arena_ptr);
            // Empty and zero the live arena (same as k1 arena/reset(clear=true)) and
            // re-create the pointer cell so init-tmp-arena doesn't re-mmap.
            // Zeroing upholds alloc-mode.alloc's zeroed-memory contract, which fresh
            // mmap pages provide and reuse across static executions would break.
            unsafe {
                let base = (*arena_ptr).basePtr;
                let used = (*arena_ptr).curAddr - base.addr() as u64;
                core::ptr::write_bytes(base.cast_mut(), 0, used as usize);
                (*arena_ptr).curAddr = base.addr() as u64;
            };
            let cell = self.static_stack.push_t(arena_ptr);
            self.globals.insert(arena_global_id.unwrap(), Value::ptr(cell));
        }

        self.eval_span = SpanId::NONE;
    }

    pub fn make() -> Self {
        let stack = Stack::make();
        let static_stack = Stack::make();

        Self {
            globals: FxHashMap::with_capacity(8192),
            static_stack,
            overall_return_addr: core::ptr::null_mut(),
            stack,
            eval_span: SpanId::NONE,
            compiler_messages: Vec::with_capacity(16),
            repl_commands: vec![],
            quiet_messages: false,
            bc_fault: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
// Pure storage to be interpreted
#[repr(transparent)]
pub struct Value(u64);

impl Value {
    pub const TRUE: Value = Self::bool(true);
    pub const NULLPTR: Value = Self(0);

    #[inline(always)]
    pub const fn bool(b: bool) -> Value {
        if b { Value(1) } else { Value(0) }
    }

    #[inline(always)]
    pub(crate) fn as_bool(&self) -> bool {
        #[cfg(debug_assertions)]
        {
            let v = self.bits();
            debug_assert!(
                v == 0 || v == 1,
                "Attempted to extract boolean from non-boolean Value: {}",
                v
            );
        }
        self.0 != 0
    }

    #[inline(always)]
    pub fn ptr(ptr: *const u8) -> Value {
        Value(ptr.addr() as u64)
    }

    #[inline(always)]
    pub(crate) const fn bits(&self) -> u64 {
        self.0
    }

    #[inline(always)]
    pub(crate) const fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub(crate) const fn truncated_raw(&self, to_bits: u32) -> Self {
        match to_bits {
            8 => Value(self.0 as u8 as u64),
            16 => Value(self.0 as u16 as u64),
            32 => Value(self.0 as u32 as u64),
            _ => *self,
        }
    }

    pub(crate) const fn sign_extended_raw(&self, from_bits: u32, to_bits: u32) -> Self {
        match (from_bits, to_bits) {
            (8, 16) => {
                let v = self.0 as i8 as i16 as u16 as u64;
                Value(v)
            }
            (8, 32) => {
                let v = self.0 as i8 as i32 as u32 as u64;
                Value(v)
            }
            (8, 64) => {
                let v = self.0 as i8 as i64 as u64;
                Value(v)
            }
            (16, 32) => {
                let v = self.0 as i16 as i32 as u32 as u64;
                Value(v)
            }
            (16, 64) => {
                let v = self.0 as i16 as i64 as u64;
                Value(v)
            }
            (32, 64) => {
                let v = self.0 as i32 as i64 as u64;
                Value(v)
            }
            _ => *self,
        }
    }

    #[inline(always)]
    pub(crate) const fn u8(u8: u8) -> Value {
        Value(u8 as u64)
    }

    #[inline(always)]
    pub(crate) const fn u16(u16: u16) -> Value {
        Value(u16 as u64)
    }

    #[inline(always)]
    pub(crate) const fn u32(u32: u32) -> Value {
        Value(u32 as u64)
    }

    #[inline(always)]
    pub(crate) const fn u64(u64: u64) -> Value {
        Value(u64)
    }

    #[inline(always)]
    pub(crate) const fn i8(i8: i8) -> Value {
        Value(i8 as u8 as u64)
    }

    #[inline(always)]
    pub(crate) const fn i16(i16: i16) -> Value {
        Value(i16 as u16 as u64)
    }

    #[inline(always)]
    pub(crate) const fn i32(i32: i32) -> Value {
        Value(i32 as u32 as u64)
    }

    #[inline(always)]
    pub(crate) const fn i64(i64: i64) -> Value {
        Value(i64 as u64)
    }

    pub fn typed_int(int: TypedIntValue) -> Value {
        match int {
            TypedIntValue::U8(v) => Value::u8(v),
            TypedIntValue::U16(v) => Value::u16(v),
            TypedIntValue::U32(v) => Value::u32(v),
            TypedIntValue::U64(v) => Value::u64(v),
            TypedIntValue::I8(v) => Value::i8(v),
            TypedIntValue::I16(v) => Value::i16(v),
            TypedIntValue::I32(v) => Value::i32(v),
            TypedIntValue::I64(v) => Value::i64(v),
        }
    }

    pub fn as_typed_int(&self, int_type: IntegerType) -> TypedIntValue {
        let u64 = self.bits();
        match int_type {
            IntegerType::U8 => TypedIntValue::U8(u64 as u8),
            IntegerType::U16 => TypedIntValue::U16(u64 as u16),
            IntegerType::U32 => TypedIntValue::U32(u64 as u32),
            IntegerType::U64 => TypedIntValue::U64(u64),
            IntegerType::I8 => TypedIntValue::I8(u64 as i8),
            IntegerType::I16 => TypedIntValue::I16(u64 as i16),
            IntegerType::I32 => TypedIntValue::I32(u64 as i32),
            IntegerType::I64 => TypedIntValue::I64(u64 as i64),
        }
    }

    #[track_caller]
    pub(crate) fn as_ptr(&self) -> *mut u8 {
        let p = self.0 as *mut u8;
        sanity_check_ptr(p);
        p
    }

    pub(crate) fn as_ptr_unchecked(&self) -> *mut u8 {
        self.0 as *mut u8
    }

    pub fn as_f32(&self) -> f32 {
        let bits = self.bits() as u32;
        f32::from_bits(bits)
    }

    pub fn as_f64(&self) -> f64 {
        let bits = self.bits();
        f64::from_bits(bits)
    }

    pub fn float_value(fv: TypedFloatValue) -> Value {
        match fv {
            TypedFloatValue::F32(f32) => Value::f32(f32),
            TypedFloatValue::F64(f64) => Value::f64(f64),
        }
    }

    pub fn f32(f: f32) -> Self {
        // The lower 32 bits can be relied upon to reconstruct the f32.
        // NOTE!!!: That we do not go through f64!
        Value(f.to_bits() as u64)
    }

    pub fn f64(f: f64) -> Self {
        Value(f.to_bits())
    }

    pub fn as_typed_float(&self, float_type: FloatType) -> TypedFloatValue {
        match float_type {
            FloatType::F32 => TypedFloatValue::F32(self.as_f32()),
            FloatType::F64 => TypedFloatValue::F64(self.as_f64()),
        }
    }

    pub fn as_u8(&self) -> u8 {
        self.0 as u8
    }
    pub fn as_u16(&self) -> u16 {
        self.0 as u16
    }
    pub fn as_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub(crate) fn resolve_global(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    global_id: TypedGlobalId,
    t: PhysicalType,
) -> K1Result<Value> {
    debug!(
        "resolving global {}",
        k1.ident_str(k1.variables.get(k1.globals.get(global_id).variable_id).name)
    );
    // Globals in `ir` always represent an Address, or Storage, of the global, not the value

    // Case 1: It's a constant, already evaluated, stored in the global static space
    if let Some(v) = k1.vm_global_constant_lookups.get(&global_id) {
        return Ok(*v);
    }
    // Case 2: It's instead in this VM because it's mutable and we already evaluated it, return it
    if let Some(v) = vm.globals.get(&global_id) {
        return Ok(*v);
    }

    // Case 3: First use in this VM. If not mutable, put in share global constants. If mutable,
    // generate and store the shared original, but store a copy in our local vm to allow mutation
    let global = k1.globals.get(global_id);
    let is_constant = global.is_constant;
    let initial_value_id = match global.initial_value {
        GlobalInitialValue::Pending => {
            kbail!(
                k1,
                vm.eval_span,
                "VM encountered un-evaluated global '{}'; all globals referenced by compiled code should have been evaluated before execution. This is a compiler bug",
                k1.variables.get(global.variable_id).name
            );
        }
        GlobalInitialValue::Uninit => {
            if global.is_external {
                let name = k1.variables.get(global.variable_id).name;
                let name_cstr = std::ffi::CString::new(k1.ident_str(name)).unwrap();
                let sym = unsafe { libc::dlsym(k1.vm_process_dlopen_handle, name_cstr.as_ptr()) };
                if sym.is_null() {
                    kbail!(
                        k1,
                        vm.eval_span,
                        "Could not resolve external global symbol '{}' at compile time; the platform does not export it dynamically",
                        k1.ident_str(name)
                    );
                }
                let addr = Value::ptr(sym as *const u8);
                vm.globals.insert(global_id, addr);
                return Ok(addr);
            } else {
                None
            }
        }
        GlobalInitialValue::Value(value_id) => Some(value_id),
    };

    let layout = k1.get_pt_layout(t);
    let dst = if is_constant {
        k1.vm_shared_static_stack.push_layout_uninit(layout)
    } else {
        vm.static_stack.push_layout_uninit(layout)
    };
    let addr = Value::ptr(dst);

    if is_constant {
        k1.vm_global_constant_lookups.insert(global_id, addr);
    } else {
        vm.globals.insert(global_id, addr);
    }

    if let Some(initial_value_id) = initial_value_id {
        if !t.is_empty() {
            debug!(
                "shared global is: {}. the `t` of the instr is: {}",
                k1.static_value_to_string(initial_value_id),
                k1.pt_to_string(t)
            );
            let shared_vm_value = static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
            store_value(k1, t, dst, shared_vm_value);
        }
    }
    Ok(addr)
}

#[inline(always)]
pub fn static_value_to_vm_value(
    k1: &mut TypedProgram,
    static_value_id: StaticValueId,
    span: SpanId,
) -> Value {
    if let Some(v) = k1.vm_static_value_lookups.get(&static_value_id) {
        return *v;
    };

    let v = match k1.static_values.get(static_value_id) {
        StaticValue::Empty => Value(0),
        StaticValue::Bool(bool_value) => Value::bool(*bool_value),
        StaticValue::Char(char_byte) => Value(*char_byte as u64),
        StaticValue::Int(iv) => Value(iv.to_u64_bits()),
        StaticValue::Enum(_, iv) => Value(iv.to_u64_bits()),
        StaticValue::Float(fv) => Value::float_value(*fv),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(k1, *string_id);
            value
        }
        StaticValue::Zero(type_id) => static_zero_value(k1, *type_id, span),
        StaticValue::Struct(static_struct) => {
            let layout = k1.get_layout(static_struct.type_id).unwrap();
            let struct_base = k1.vm_shared_static_stack.push_layout_uninit(layout);

            store_static_value(k1, struct_base, static_value_id);

            Value::ptr(struct_base)
        }
        StaticValue::Sum(sum) => {
            let layout = k1.get_layout(sum.sum_type_id).unwrap();
            let sum_base = k1.vm_shared_static_stack.push_layout_uninit(layout);

            store_static_value(k1, sum_base, static_value_id);

            Value::ptr(sum_base)
        }
        StaticValue::LinearContainer(container) => {
            let container_type_id = container.type_id;
            let element_type = k1.get_linear_container_element(container_type_id).unwrap();
            let kind = container.kind;
            let len = container.len();
            let container_elements = container.elements;
            let layout = k1.get_layout(element_type).unwrap();
            // Inline containers (arrays, vectors) use their own layout so vectors
            // get their natural alignment
            let array_allocation_layout = match kind {
                StaticContainerKind::Array | StaticContainerKind::Vector => {
                    k1.get_layout(container_type_id).unwrap()
                }
                _ => layout.array_me(len),
            };

            let array_base_ptr =
                k1.vm_shared_static_stack.push_layout_uninit(array_allocation_layout);

            store_static_array_elements(k1, array_base_ptr, element_type, container_elements);

            match kind {
                // k1 span and buffer have identical layouts; differ only in mutability
                StaticContainerKind::Span | StaticContainerKind::Buffer => {
                    let rust_span =
                        k1_types::K1BufferLike { len: len as i64, data: array_base_ptr };
                    let span_struct_ptr = k1.vm_shared_static_stack.push_t(rust_span);

                    Value::ptr(span_struct_ptr)
                }
                StaticContainerKind::List => {
                    // k1 list is a growable buffer; { buffer, capacity }.
                    let rust_list = k1_types::K1List {
                        k1_buffer: k1_types::K1BufferLike { data: array_base_ptr, len: len as i64 },
                        len: len as i64,
                    };
                    let rust_struct_ptr = k1.vm_shared_static_stack.push_t(rust_list);
                    Value::ptr(rust_struct_ptr)
                }
                // k1 arrays and vectors are aggregate values, represented by base address
                StaticContainerKind::Array | StaticContainerKind::Vector => {
                    Value::ptr(array_base_ptr)
                }
            }
        }
    };
    k1.vm_static_value_lookups.insert(static_value_id, v);
    v
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_static_value(k1: &mut TypedProgram, dst: *mut u8, static_value_id: StaticValueId) {
    match k1.static_values.get(static_value_id) {
        StaticValue::Empty => {}
        StaticValue::Bool(bool_value) => store_byte(dst, *bool_value as u8),
        StaticValue::Char(char_byte) => store_byte(dst, *char_byte),
        StaticValue::Int(int_value) => store_typed_int(dst, *int_value),
        StaticValue::Enum(_, int_value) => store_typed_int(dst, *int_value),
        StaticValue::Float(fv) => store_scalar(fv.get_scalar_type(), dst, Value::float_value(*fv)),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(k1, *string_id);
            let string_type_id = k1.string_type_id();
            let string_pt = k1.get_physical_type(string_type_id).unwrap();
            store_value(k1, string_pt, dst, value);
        }
        StaticValue::Zero(type_id) => {
            let layout = k1.get_layout(*type_id).unwrap();
            unsafe { std::ptr::write_bytes(dst, 0, layout.size as usize) };
        }
        StaticValue::Struct(static_struct) => {
            let struct_fields = static_struct.fields;
            let struct_layout = k1.get_struct_layout(static_struct.type_id);

            for (field, field_value_id) in
                struct_layout.iter().zip(k1.static_values.mem.getn(struct_fields))
            {
                let field_ptr = unsafe { dst.byte_add(field.offset as usize) };
                store_static_value(k1, field_ptr, *field_value_id);
            }
        }
        StaticValue::Sum(e) => {
            let variant_index = e.variant_index;
            let payload = e.payload;
            let sum_agg_id = k1.get_physical_type(e.sum_type_id).unwrap().expect_agg();
            let sum_pt = k1.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let variant_pt = k1.mem.get_nth(sum_pt.variants, variant_index as usize);

            store_typed_int(dst, variant_pt.tag);

            if let Some(payload_value_id) = payload {
                let payload_offset = sum_pt.payload_offset;
                let payload_ptr = unsafe { dst.byte_add(payload_offset as usize) };
                store_static_value(k1, payload_ptr, payload_value_id);
            };
        }
        StaticValue::LinearContainer(container) => {
            let element_type = k1.get_linear_container_element(container.type_id).unwrap();
            let kind = container.kind;
            let len = container.len();
            let container_elements = container.elements;
            let layout = k1.get_layout(element_type).unwrap();
            let array_allocation_layout = layout.array_me(len);

            let array_base_ptr = match kind {
                StaticContainerKind::Span
                | StaticContainerKind::Buffer
                | StaticContainerKind::List => {
                    k1.vm_shared_static_stack.push_layout_uninit(array_allocation_layout)
                }
                StaticContainerKind::Array | StaticContainerKind::Vector => dst,
            };

            store_static_array_elements(k1, array_base_ptr, element_type, container_elements);

            match kind {
                StaticContainerKind::Span | StaticContainerKind::Buffer => {
                    // Store the struct to dst
                    let rust_span =
                        k1_types::K1BufferLike { len: len as i64, data: array_base_ptr };

                    unsafe { *(dst as *mut k1_types::K1BufferLike) = rust_span };
                }
                StaticContainerKind::List => {
                    // Store the struct to dst
                    let rust_list = k1_types::K1List {
                        k1_buffer: k1_types::K1BufferLike { data: array_base_ptr, len: len as i64 },
                        len: len as i64,
                    };

                    unsafe { *(dst as *mut k1_types::K1List) = rust_list };
                }
                StaticContainerKind::Array | StaticContainerKind::Vector => {}
            }
        }
    };
}

fn store_static_array_elements(
    k1: &mut TypedProgram,
    dst: *mut u8,
    element_type: TypeId,
    elements: MSlice<StaticValueId, StaticValuePool>,
) {
    debug!("static_value_to_vm_value storing {} elements", elements.len());
    let element_layout = k1.get_layout(element_type).unwrap();

    for index in 0..elements.len() {
        let elem_value_id = k1.static_values.mem.get_nth(elements, index as usize);
        let offset = element_layout.offset_at_index(index as usize);
        let elem_dst = unsafe { dst.byte_add(offset) };
        debug!(
            "static_value_to_vm_value storing element {} to +{}",
            k1.static_value_to_string(*elem_value_id),
            offset
        );
        store_static_value(k1, elem_dst, *elem_value_id);
    }
}

pub fn string_id_to_value(k1: &mut TypedProgram, string_id: StringId) -> Value {
    let s = k1.get_string(string_id);
    // Points into the ident pool's arena, which never moves
    let k1_string = k1_types::K1BufferLike { len: s.len() as i64, data: s.as_ptr().cast_mut() };
    if cfg!(debug_assertions) {
        let string_type_id = k1.string_type_id();
        let char_span_type_id = k1.get_struct_field(string_type_id, 0).type_id;
        let string_layout = k1.get_layout(string_type_id).unwrap();
        debug_assert_eq!(string_layout, k1.get_layout(char_span_type_id).unwrap());
        debug_assert_eq!(size_of_val(&k1_string), string_layout.size as usize);
    }

    let string_stack_addr =
        k1.vm_shared_static_stack.mem.push(k1_string) as *mut k1_types::K1BufferLike;
    Value::ptr(string_stack_addr.cast())
}

pub(crate) fn allocate(layout: std::alloc::Layout, zero: bool) -> *mut u8 {
    let ptr = if zero {
        unsafe { std::alloc::alloc_zeroed(layout) }
    } else {
        unsafe { std::alloc::alloc(layout) }
    };
    ptr
}

pub fn store_byte(dst: *mut u8, u8: u8) {
    store_scalar(ScalarType::I8, dst, Value::u8(u8))
}

pub fn store_typed_int(dst: *mut u8, int: TypedIntValue) {
    store_scalar(int.get_scalar_type(), dst, Value::typed_int(int))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn rust_atomic_ordering(ord: ir::AtomicOrderingIr) -> std::sync::atomic::Ordering {
    use std::sync::atomic::Ordering as RO;
    match ord {
        ir::AtomicOrderingIr::Relaxed => RO::Relaxed,
        ir::AtomicOrderingIr::Acquire => RO::Acquire,
        ir::AtomicOrderingIr::Release => RO::Release,
        ir::AtomicOrderingIr::AcqRel => RO::AcqRel,
        ir::AtomicOrderingIr::SeqCst => RO::SeqCst,
    }
}

#[cfg(debug_assertions)]
fn check_atomic_align(width_bits: u8, ptr: *const u8) {
    let align = width_bits as usize / 8;
    assert!(
        (ptr as usize).is_multiple_of(align),
        "misaligned atomic .{width_bits} operation on {ptr:p} (requires {align}-byte alignment)"
    );
}

/// Dispatches over width to `std::sync::atomic` types viewed in place over the
/// target memory; used by both the tree-walking and bytecode VMs. Width is all
/// they need: signedness only matters to min/max, and that lives in the rmw op
/// tag; pointers are just word-sized bits.
macro_rules! with_unsigned_atomic {
    ($width_bits:expr, $ptr:expr, |$a:ident, $uty:ident| $body:expr) => {{
        #[cfg(debug_assertions)]
        check_atomic_align($width_bits, $ptr.cast_const());
        match $width_bits {
            8 => {
                type $uty = u8;
                let $a = unsafe { std::sync::atomic::AtomicU8::from_ptr($ptr) };
                Value::u8($body)
            }
            16 => {
                type $uty = u16;
                let $a = unsafe { std::sync::atomic::AtomicU16::from_ptr($ptr.cast()) };
                Value::u16($body)
            }
            32 => {
                type $uty = u32;
                let $a = unsafe { std::sync::atomic::AtomicU32::from_ptr($ptr.cast()) };
                Value::u32($body)
            }
            64 => {
                type $uty = u64;
                let $a = unsafe { std::sync::atomic::AtomicU64::from_ptr($ptr.cast()) };
                Value::u64($body)
            }
            w => unreachable!("atomic op on width {w}"),
        }
    }};
}

pub fn atomic_load_bits(width_bits: u8, src: *mut u8, ord: ir::AtomicOrderingIr) -> Value {
    let ord = rust_atomic_ordering(ord);
    with_unsigned_atomic!(width_bits, src, |a, _U| a.load(ord))
}

pub fn atomic_store_bits(width_bits: u8, dst: *mut u8, value: Value, ord: ir::AtomicOrderingIr) {
    let ord = rust_atomic_ordering(ord);
    let bits = value.bits();
    with_unsigned_atomic!(width_bits, dst, |a, U| {
        a.store(bits as U, ord);
        0 as U
    });
}

pub fn atomic_rmw_bits(
    op: ir::AtomicRmwOpIr,
    width_bits: u8,
    dst: *mut u8,
    operand: Value,
    ord: ir::AtomicOrderingIr,
) -> Value {
    use ir::AtomicRmwOpIr as Op;
    let ord = rust_atomic_ordering(ord);
    let bits = operand.bits();
    // Signed min/max are the only ops whose result depends on signedness;
    // everything else is identical on the unsigned view of the bits
    macro_rules! signed_minmax {
        ($A:ty, $ity:ident, $uty:ident, $uctor:ident) => {{
            #[cfg(debug_assertions)]
            check_atomic_align(width_bits, dst.cast_const());
            let a = unsafe { <$A>::from_ptr(dst.cast()) };
            let v = bits as $ity;
            let prev = if op == Op::MinS { a.fetch_min(v, ord) } else { a.fetch_max(v, ord) };
            Value::$uctor(prev as $uty)
        }};
    }
    match (op, width_bits) {
        (Op::MinS | Op::MaxS, 8) => signed_minmax!(std::sync::atomic::AtomicI8, i8, u8, u8),
        (Op::MinS | Op::MaxS, 16) => signed_minmax!(std::sync::atomic::AtomicI16, i16, u16, u16),
        (Op::MinS | Op::MaxS, 32) => signed_minmax!(std::sync::atomic::AtomicI32, i32, u32, u32),
        (Op::MinS | Op::MaxS, 64) => signed_minmax!(std::sync::atomic::AtomicI64, i64, u64, u64),
        (Op::MinS | Op::MaxS, w) => unreachable!("signed atomic min/max on width {w}"),
        _ => with_unsigned_atomic!(width_bits, dst, |a, U| match op {
            Op::Xchg => a.swap(bits as U, ord),
            Op::Add => a.fetch_add(bits as U, ord),
            Op::Sub => a.fetch_sub(bits as U, ord),
            Op::And => a.fetch_and(bits as U, ord),
            Op::Or => a.fetch_or(bits as U, ord),
            Op::Xor => a.fetch_xor(bits as U, ord),
            Op::MinU => a.fetch_min(bits as U, ord),
            Op::MaxU => a.fetch_max(bits as U, ord),
            Op::MinS | Op::MaxS => unreachable!(),
        }),
    }
}

/// Returns (previous value, success)
pub fn atomic_cmpxchg_bits(
    width_bits: u8,
    dst: *mut u8,
    expected: Value,
    desired: Value,
    success: ir::AtomicOrderingIr,
    failure: ir::AtomicOrderingIr,
    weak: bool,
) -> (Value, bool) {
    let success = rust_atomic_ordering(success);
    let failure = rust_atomic_ordering(failure);
    let expected_bits = expected.bits();
    let desired_bits = desired.bits();
    let ok;
    let prev = with_unsigned_atomic!(width_bits, dst, |a, U| {
        let result = if weak {
            a.compare_exchange_weak(expected_bits as U, desired_bits as U, success, failure)
        } else {
            a.compare_exchange(expected_bits as U, desired_bits as U, success, failure)
        };
        ok = result.is_ok();
        result.unwrap_or_else(|prev| prev)
    });
    (prev, ok)
}

pub fn store_scalar(t: ScalarType, dst: *mut u8, value: Value) {
    #[cfg(debug_assertions)]
    sanity_check_ptr(dst.cast_const());
    #[cfg(debug_assertions)]
    if dst.is_null() {
        panic!("attempted to store {t} {value} to null");
    }
    unsafe {
        match t {
            ScalarType::I8 | ScalarType::U8 => dst.write(value.bits() as u8),
            ScalarType::I16 | ScalarType::U16 => (dst as *mut u16).write(value.bits() as u16),
            ScalarType::I32 | ScalarType::U32 | ScalarType::F32 => {
                (dst as *mut u32).write(value.bits() as u32)
            }
            ScalarType::I64 | ScalarType::U64 | ScalarType::F64 => {
                (dst as *mut u64).write(value.bits())
            }
            ScalarType::Pointer => (dst as *mut usize).write(value.bits() as usize),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_value(k1: &TypedProgram, t: PhysicalType, dst: *mut u8, value: Value) {
    match t.as_enum() {
        // Lowering should never emit a load/store/copy of Empty
        PhysicalTypeEnum::Empty => (),
        PhysicalTypeEnum::Scalar(scalar_type) => store_scalar(scalar_type, dst, value),
        PhysicalTypeEnum::Agg(pt_id) => {
            let record = k1.agg_types.get(pt_id);
            let src = value.as_ptr();
            memmove(src, dst, record.layout.size as usize)
        }
    }
}

pub(crate) fn memmove(src: *const u8, dst: *mut u8, size_bytes: usize) {
    //debug!("memmove src {:?} dst {:?} size {}", src, dst, size_bytes);
    unsafe {
        core::ptr::copy(src, dst, size_bytes);
    }
}

pub(crate) fn memcopy(src: *const u8, dst: *mut u8, size_bytes: usize) {
    //debug!("memcopy src {:?} dst {:?} size {}", src, dst, size_bytes);
    unsafe {
        core::ptr::copy_nonoverlapping(src, dst, size_bytes);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_scalar(t: ScalarType, ptr: *const u8) -> Value {
    unsafe {
        match t {
            ScalarType::U8 | ScalarType::I8 => Value::u8(ptr.read()),
            ScalarType::U16 | ScalarType::I16 => Value::u16((ptr as *const u16).read()),
            ScalarType::U32 | ScalarType::I32 => Value::u32((ptr as *const u32).read()),
            ScalarType::U64 | ScalarType::I64 => Value::u64((ptr as *const u64).read()),
            ScalarType::F32 => Value::u32((ptr as *const u32).read()),
            ScalarType::F64 => Value::u64((ptr as *const u64).read()),
            ScalarType::Pointer => {
                let read_address = (ptr as *const usize).read();
                let ptr = read_address as *const u8;
                Value::ptr(ptr)
            }
        }
    }
}

pub fn load_value(t: PhysicalType, ptr: *const u8) -> Value {
    match t.as_enum() {
        PhysicalTypeEnum::Empty => {
            eprintln!("load_value on Empty");
            Value(0)
        }
        PhysicalTypeEnum::Scalar(st) => load_scalar(st, ptr),
        PhysicalTypeEnum::Agg(_) => Value::ptr(ptr),
    }
}

pub struct Stack {
    pub mem: crate::kmem::Mem<()>,
}

impl Stack {
    pub fn make() -> Stack {
        let mut mem = kmem::Mem::make();
        let expected_values_needed = 10000;
        mem.will_need(expected_values_needed * std::mem::size_of::<Value>());
        Self { mem }
    }

    pub fn reset(&mut self) {
        let zero = cfg!(debug_assertions);
        self.mem.reset(zero);
    }

    pub fn cursor(&self) -> *mut u8 {
        self.mem.cursor()
    }

    #[inline]
    pub fn base_ptr(&self) -> *const u8 {
        self.mem.base_ptr()
    }

    #[inline]
    pub fn end_ptr(&self) -> *const u8 {
        self.mem.cursor()
    }

    #[inline]
    pub fn current_offset_bytes(&self) -> usize {
        self.mem.bytes_used()
    }

    pub fn push_layout_uninit(&mut self, layout: Layout) -> *mut u8 {
        self.mem.push_layout_uninit(layout.size, layout.align)
    }

    pub fn push_t<T>(&mut self, t: T) -> *const u8 {
        let r = self.mem.push(t);
        r as *mut T as *const u8
    }
}

/// Please don't hang on to this reference for very long
pub fn value_to_rust_str<'a>(value: Value) -> Result<&'a str, &'static str> {
    let ptr = value.as_ptr();
    let span_ptr = ptr as *const k1_types::K1BufferLike;
    let k1_buffer_like = unsafe { span_ptr.read() };
    unsafe { k1_buffer_like.to_str() }
}

pub fn value_to_string_id(m: &mut TypedProgram, value: Value) -> Result<StringId, &'static str> {
    let rust_str = value_to_rust_str(value)?;
    Ok(m.ast.idents.intern(rust_str))
}

pub fn value_to_ident(m: &mut TypedProgram, value: Value) -> Result<StringId, &'static str> {
    let rust_str = value_to_rust_str(value)?;
    Ok(m.ast.idents.intern(rust_str))
}

/// VM values contain a lot of pointers to the VM's stack and heap
/// (which is currently just the host's heap)
/// We need to convert these into 'constants' so that we can embed
/// them in a binary, for example LLVM. This function does that
/// by recursively 'loading' all the values out of the VM value.
///
/// Obviously not all types are supported; only things you can reasonably
/// embed in a binary; raw pointers for example are out.
///
/// For complex types (not a char array) like a big slice of structs, I think we may just have to use
/// some sort of 'embed binary data' feature of the backend
pub fn vm_value_to_static_value(
    k1: &mut TypedProgram,
    type_id: TypeId,
    vm_value: Value,
    span: SpanId,
) -> K1Result<StaticValueId> {
    debug!("vm_to_static: {:?}: {}", vm_value, k1.type_id_to_string(type_id));
    let PhysicalTypeResult::Yes(pt) = k1.get_physical_type(type_id) else {
        kbail!(k1, span, "Not a physical type, cannot bake to static: {}", type_id);
    };
    if pt.is_empty() {
        return Ok(k1.static_values.empty_id());
    }
    // We know it is a physical type so can be aggressive with matches
    let static_value_id = match k1.types.get(type_id) {
        Type::Char => k1.static_values.add(StaticValue::Char(vm_value.bits() as u8)),
        Type::Bool => k1.static_values.add(StaticValue::Bool(vm_value.bits() == 1)),
        Type::Pointer => {
            let addr = vm_value.as_ptr();
            if addr.is_null() || addr.addr() == 0 {
                k1.static_values.add(StaticValue::Zero(POINTER_TYPE_ID))
            } else {
                kbail!(
                    k1,
                    span,
                    "Only null Pointers can be statically baked; got {}",
                    format!("{addr:p}")
                );
            }
        }
        Type::Integer(integer_type) => {
            let int_value = vm_value.as_typed_int(*integer_type);
            k1.static_values.add(StaticValue::Int(int_value))
        }
        Type::Enum(se) => {
            let int_value = vm_value.as_typed_int(se.int_type);
            k1.static_values.add(StaticValue::Enum(type_id, int_value))
        }
        Type::Float(float_type) => {
            let float_value = vm_value.as_typed_float(*float_type);
            k1.static_values.add(StaticValue::Float(float_value))
        }
        Type::Array(_) | Type::Vector(_) => {
            let (element_type, size_type, kind) = match k1.types.get(type_id) {
                Type::Array(a) => (a.element_type, a.size_type, StaticContainerKind::Array),
                Type::Vector(v) => (v.element_type, v.size_type, StaticContainerKind::Vector),
                _ => unreachable!(),
            };
            let Some(count) = k1.get_concrete_count_of_array(size_type) else {
                kbail!(k1, span, "Cannot convert container of unknown size to static value");
            };
            let count = count as usize;
            let mut elements = k1.static_values.mem.new_list(count as u32);
            let PhysicalTypeResult::Yes(element_pt) = k1.get_physical_type(element_type) else {
                kbail!(k1, span, "Element type is not physical: {}", element_type);
            };
            for index in 0..count {
                let elem_result = get_span_element(k1, vm_value.as_ptr(), element_pt, index);
                let elem_static = vm_value_to_static_value(k1, element_type, elem_result, span)?;
                elements.push(elem_static);
            }
            let elements_slice = elements.to_slice();
            k1.static_values.add(StaticValue::LinearContainer(StaticContainer {
                elements: elements_slice,
                kind,
                type_id,
            }))
        }
        Type::Struct(struct_type) => {
            if type_id == k1.string_type_id() {
                let string_id = value_to_string_id(k1, vm_value).map_err(|msg| {
                    kerr!(k1, span, "Could not convert string to static value: {msg}")
                })?;
                k1.static_values.add(StaticValue::String(string_id))
            } else if let Some((element_type, container_kind)) =
                k1.get_as_container_instance(type_id)
            {
                match container_kind {
                    ContainerKind::Array(_) => unreachable!(),
                    ContainerKind::Span | ContainerKind::List | ContainerKind::Buffer => {
                        let k1_span: k1_types::K1BufferLike = match container_kind {
                            ContainerKind::List => {
                                let list = value_as_list(vm_value);
                                k1_types::K1BufferLike { data: list.k1_buffer.data, len: list.len }
                            }
                            ContainerKind::Span => value_as_span(vm_value),
                            ContainerKind::Buffer => value_as_span(vm_value),
                            _ => unreachable!(),
                        };
                        let element_pt = k1.get_physical_type(element_type).unwrap();
                        let mut elements = k1.static_values.mem.new_list(k1_span.len as u32);
                        for index in 0..(k1_span.len as usize) {
                            let elem_vm = get_span_element(k1, k1_span.data, element_pt, index);
                            let elem_static =
                                vm_value_to_static_value(k1, element_type, elem_vm, span)?;
                            elements.push(elem_static);
                        }
                        let elements_slice = elements.to_slice();
                        let static_kind = match container_kind {
                            ContainerKind::Array(_) => unreachable!(),
                            ContainerKind::Buffer => StaticContainerKind::Buffer,
                            ContainerKind::Span => StaticContainerKind::Span,
                            ContainerKind::List => StaticContainerKind::List,
                        };
                        k1.static_values.add(StaticValue::LinearContainer(StaticContainer {
                            elements: elements_slice,
                            kind: static_kind,
                            type_id,
                        }))
                    }
                }
            } else {
                let struct_ptr = vm_value.as_ptr();
                let struct_type_fields = struct_type.fields;
                let mut field_value_ids = k1.static_values.mem.new_list(struct_type.fields.len());
                let struct_shape = k1.get_struct_layout(type_id);
                for (physical_field, k1_field) in
                    struct_shape.iter().zip(k1.mem.getn(struct_type_fields))
                {
                    let field_ptr = unsafe { struct_ptr.byte_add(physical_field.offset as usize) };
                    let field_value = load_value(physical_field.field_t, field_ptr);
                    let field_static_value_id =
                        vm_value_to_static_value(k1, k1_field.type_id, field_value, span)?;
                    field_value_ids.push(field_static_value_id)
                }
                k1.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id,
                    fields: field_value_ids.to_slice(),
                }))
            }
        }
        Type::Sum(typed_sum) => {
            let sum_ptr = vm_value.as_ptr();

            let tag_int_type = typed_sum.tag_type;
            let variants = typed_sum.variants;
            let sum_agg_id = k1.get_physical_type(type_id).unwrap().expect_agg();
            let sum_pt = k1.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let payload_offset = sum_pt.payload_offset;

            let tag_scalar_type = sum_pt.tag_type;
            let tag = load_scalar(tag_scalar_type, sum_ptr).as_typed_int(tag_int_type);
            let Some(variant) = k1.mem.getn(variants).iter().find(|v| v.tag_value == tag) else {
                kbail!(k1, span, "No variant found with tag value {}", tag);
            };
            let variant_index = variant.index;

            let payload = match variant.payload {
                None => None,
                Some(payload_type_id) => {
                    let payload_pt = k1.get_physical_type(payload_type_id).unwrap();
                    let payload_ptr = unsafe { sum_ptr.byte_add(payload_offset as usize) };

                    let payload_value = load_value(payload_pt, payload_ptr);
                    let static_value_id =
                        vm_value_to_static_value(k1, payload_type_id, payload_value, span)?;
                    Some(static_value_id)
                }
            };
            k1.static_values.add(StaticValue::Sum(StaticSum {
                sum_type_id: type_id,
                variant_index,
                payload,
            }))
        }
        Type::FunctionPointer(_) => {
            kbail!(k1, span, "Cannot bake function pointers");
        }
        Type::Reference(_) => {
            let addr = vm_value.as_ptr();
            if addr.is_null() || addr.addr() == 0 {
                k1.static_values.add(StaticValue::Zero(type_id))
            } else {
                kbail!(k1, span, "Cannot yet bake non-null references");
            }
        }
        Type::Lambda(_) | Type::LambdaObject(_) | Type::AbilityObject(_) | Type::Opaque(_) => {
            kbail!(
                k1,
                span,
                "Only plain old data (scalars, structs, arrays, eithers, etc) can be statically baked. Got: {}",
                type_id
            );
        }
        Type::Function(_)
        | Type::Never
        | Type::StaticValue(_)
        | Type::Generic(_)
        | Type::TypeParameter(_)
        | Type::FunctionTypeParameter(_)
        | Type::InferenceHole(_) => unreachable!(),
    };
    Ok(static_value_id)
}

pub fn value_as_list(list_value: Value) -> k1_types::K1List {
    let ptr = list_value.as_ptr();
    let list_ptr = ptr as *const k1_types::K1List;
    let k1_list = unsafe { list_ptr.read() };
    k1_list
}

pub fn value_as_span(span_value: Value) -> k1_types::K1BufferLike {
    let ptr = span_value.as_ptr();
    let buffer_ptr = ptr as *const k1_types::K1BufferLike;
    let k1_span_like = unsafe { buffer_ptr.read() };
    k1_span_like
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn get_span_element(
    k1: &TypedProgram,
    data_ptr: *const u8,
    elem_pt: PhysicalType,
    index: usize,
) -> Value {
    let elem_offset = k1.get_pt_layout(elem_pt).offset_at_index(index);
    let elem_ptr = unsafe { data_ptr.byte_add(elem_offset) };
    load_value(elem_pt, elem_ptr)
}

fn static_zero_value(k1: &mut TypedProgram, type_id: TypeId, span: SpanId) -> Value {
    match k1.get_physical_type(type_id) {
        PhysicalTypeResult::No | PhysicalTypeResult::Never | PhysicalTypeResult::Infinite => {
            ice_span!(
                k1,
                span,
                "not a value type; zeroed() for type {} is undefined",
                k1.types.get(type_id).kind_name()
            )
        }
        PhysicalTypeResult::Yes(pt) => match pt.as_enum() {
            PhysicalTypeEnum::Scalar(_) => Value(0),
            PhysicalTypeEnum::Agg(agg_id) => {
                let layout = k1.agg_types.get(agg_id).layout;
                let data: *mut u8 = k1.vm_shared_static_stack.push_layout_uninit(layout);
                unsafe { std::ptr::write_bytes(data, 0, layout.size as usize) };

                Value::ptr(data.cast_const())
            }
            PhysicalTypeEnum::Empty => Value(0),
        },
    }
}

pub(crate) unsafe fn slice_from_raw_parts_checked<'a, T>(
    vm: &Vm,
    k1: &TypedProgram,
    data: *const T,
    len: usize,
) -> &'a [T] {
    let null_ok = data.is_null() && len == 0;
    if cfg!(debug_assertions) {
        if !data.is_aligned() {
            vm_ice!(k1, vm, "slice_from_raw_parts: ptr is unaligned: {:?}", data)
        }
        let is_null = data.is_null();
        let is_zst = std::mem::size_of::<T>() == 0;
        if !null_ok {
            if !is_zst && is_null {
                vm_ice!(
                    k1,
                    vm,
                    "slice_from_raw_parts: data={:?} len={len} size={}",
                    data,
                    std::mem::size_of::<T>()
                );
            }
        }
    }
    let data = if null_ok { core::ptr::dangling() } else { data };
    unsafe { std::slice::from_raw_parts(data, len) }
}

/// The CompilerMessage builtin body, shared with the bc VM. Mirrors the
/// inline arm in `exec_loop` (Inst::Call -> BackendBuiltin::CompilerMessage).
pub(crate) fn builtin_compiler_message(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    location_arg: Value,
    level_arg: Value,
    message_arg: Value,
) -> K1Result<()> {
    let location = unsafe { (location_arg.as_ptr() as *const k1_types::K1SourceLocation).read() };
    let level = match k1_types::CompilerMessageLevel::from_u8(level_arg.as_u8())
        .unwrap_or(k1_types::CompilerMessageLevel::Info)
    {
        k1_types::CompilerMessageLevel::Info => MessageLevel::Info,
        k1_types::CompilerMessageLevel::Warn => MessageLevel::Warn,
        k1_types::CompilerMessageLevel::Error => MessageLevel::Error,
    };
    let message = value_to_string_id(k1, message_arg).map_err(|msg| {
        kerr!(k1, vm.eval_span, "Bad message string passed to EmitCompilerMessage: {msg}")
    })?;
    let filename = unsafe { location.filename.to_str() }.map_err(|msg| {
        kerr!(k1, vm.eval_span, "Bad filename string passed to EmitCompilerMessage: {msg}")
    })?;

    if !vm.quiet_messages {
        eprintln!(
            "[{}:{} {}] {}",
            filename,
            location.line,
            level.name_str().color(level.color()),
            k1.get_string(message)
        );
    }

    vm.compiler_messages.push(CompilerMessage {
        level,
        message,
        filename: filename.to_string(),
        line: location.line as u32,
    });
    Ok(())
}

/// The ReplCheckbox builtin body, shared with the bc VM:
/// `k1/repl/checkbox(name, get, set)`. Function-pointer args carry the
/// FunctionId in their bits (same encoding CallIndirect uses).
pub(crate) fn builtin_repl_checkbox(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    name_arg: Value,
    get_arg: Value,
    set_arg: Value,
) -> K1Result<()> {
    let name = value_to_string_id(k1, name_arg)
        .map_err(|msg| kerr!(k1, vm.eval_span, "Bad name string passed to repl/checkbox: {msg}"))?;
    let Some(get) = FunctionId::from_u32(get_arg.bits() as u32) else {
        kbail!(k1, vm.eval_span, "repl/checkbox: `get` is a null function pointer");
    };
    let Some(set) = FunctionId::from_u32(set_arg.bits() as u32) else {
        kbail!(k1, vm.eval_span, "repl/checkbox: `set` is a null function pointer");
    };
    vm.repl_commands.push(ReplCommand::Checkbox { name, get, set });
    Ok(())
}

/// Removes captured print messages from `from` onward and splits them into
/// (stdout, stderr) text: core's print/eprint emit Info/Warn compiler messages
/// when running on the VM (k1/is-static and k1/capture-prints)
pub fn drain_captured_prints(k1: &TypedProgram, vm: &mut Vm, from: usize) -> (String, String) {
    let mut stdout = String::new();
    let mut stderr = String::new();
    for message in vm.compiler_messages.drain(from..) {
        let s = k1.get_string(message.message);
        match message.level {
            MessageLevel::Info | MessageLevel::Hint => stdout.push_str(s),
            MessageLevel::Warn | MessageLevel::Error => stderr.push_str(s),
        }
    }
    (stdout, stderr)
}

/// Reads a global's current value out of VM memory as a static value
pub fn read_global_as_static(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    global_id: TypedGlobalId,
    type_id: TypeId,
) -> K1Result<StaticValueId> {
    let span = vm.eval_span;
    let pt = match k1.get_physical_type(type_id) {
        PhysicalTypeResult::Yes(pt) => pt,
        _ => return Ok(k1.static_values.empty_id()),
    };
    let addr = resolve_global(k1, vm, global_id, pt)?;
    let loaded = load_value(pt, addr.as_ptr());
    vm_value_to_static_value(k1, type_id, loaded, span)
}

/// A global's current value, if this VM (or the shared constant space)
/// already holds storage for it; unlike `read_global_as_static` this never
/// materializes an untouched global, whose contents would be uninitialized
/// garbage
pub fn peek_global_as_static(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    global_id: TypedGlobalId,
    type_id: TypeId,
) -> Option<StaticValueId> {
    let materialized = vm.globals.contains_key(&global_id)
        || k1.vm_global_constant_lookups.contains_key(&global_id);
    if !materialized {
        return None;
    }
    read_global_as_static(k1, vm, global_id, type_id).ok()
}

pub(crate) fn report_execution_messages(
    k1: &mut TypedProgram,
    vm: &Vm,
    span: SpanId,
    _exit_code: i32,
) {
    if vm.compiler_messages.is_empty() {
        return;
    }

    let mut formatted_messages = String::new();
    let mut max_level = MessageLevel::Hint;
    for message in &vm.compiler_messages {
        use std::fmt::Write;
        let msg_str = k1.get_string(message.message);
        let color = match message.level {
            MessageLevel::Info => colored::Color::BrightWhite,
            MessageLevel::Warn => colored::Color::Yellow,
            MessageLevel::Error => colored::Color::Red,
            MessageLevel::Hint => colored::Color::BrightBlue,
        };
        if message.level > max_level {
            max_level = message.level
        };
        if msg_str == "\n" {
            writeln!(&mut formatted_messages).unwrap()
        } else {
            writeln!(
                &mut formatted_messages,
                "[{}:{} {}] {}",
                message.filename,
                message.line,
                message.level.name_str().color(color),
                msg_str
            )
            .unwrap()
        };
    }
    let level = MessageLevel::Info;
    let message = k1.ast.idents.intern(&formatted_messages);
    k1.report_ext(K1Message { message, span, level, error_kind: ErrorKind::None }, true);
}

#[track_caller]
fn vm_crash(m: &TypedProgram, vm: &Vm, msg: impl AsRef<str>) -> ! {
    m.ice_span(vm.eval_span, msg)
}

const MIN_VALID_PTR_HEUR: usize = 0x10000; // 64 KiB
#[track_caller]
fn sanity_check_ptr(ptr: *const u8) {
    debug_assert!(
        ptr.addr() == 0 || ptr.addr() >= MIN_VALID_PTR_HEUR,
        "Probably not a pointer: {}",
        ptr.addr()
    );
}
