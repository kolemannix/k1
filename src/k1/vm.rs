// Copyright (c) 2025 knix
// All rights reserved.

use std::num::NonZeroU32;

use ahash::HashMapExt;
use colored::Colorize;
use fxhash::FxHashMap;
use log::debug;

mod vm_ffi;
#[cfg(test)]
mod vm_test;

use crate::bc::{
    self, BcCallee, CompilableUnitId, CompiledUnit, Inst, InstId, InstKind, Value as BcValue,
};
use crate::typer::types::{
    self, ContainerKind, FloatType, IntegerType, POINTER_TYPE_ID, PhysicalType, STRING_TYPE_ID,
    ScalarType, Type, TypeId, TypePool,
};
use crate::typer::{
    FunctionId, Layout, MessageLevel, StaticContainer, StaticContainerKind, StaticEnum,
    StaticStruct, StaticValue, StaticValueId, StaticValuePool, TypedExprId, TypedFloatValue,
    TypedGlobalId, TypedIntValue, TypedProgram, TyperError, TyperResult, UNIT_BYTE_VALUE,
    VariableId,
};
use crate::{
    errf, failf, ice_span,
    kmem::{self, MSlice},
    lex::SpanId,
    parse::{Ident, StringId},
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

/// Bit-for-bit mappings of K1 types
#[allow(non_snake_case)]
pub mod k1_types {

    #[derive(Clone, Copy)]
    #[repr(C)]
    pub struct Arena {
        pub basePtr: *const u8,
        pub curAddr: u64,
        pub maxAddr: u64,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1SourceLocation {
        pub filename: K1ViewLike,
        pub line: u64,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    /// Encompasses all 3 'core' contiguous buffer types in k1:
    /// - Buffer, View, and string, same layout
    pub struct K1ViewLike {
        pub len: usize,
        pub data: *const u8,
    }

    #[repr(u8)]
    pub enum CompilerMessageLevel {
        Info = 0,
        Warn = 1,
        Error = 2,
    }

    impl K1ViewLike {
        ///# Safety
        /// None of this is safe
        pub unsafe fn to_slice<'a, T>(self) -> &'a [T] {
            unsafe { std::slice::from_raw_parts(self.data as *const T, self.len) }
        }

        ///# Safety
        /// Really make sure its a char buffer
        pub unsafe fn to_str<'a>(self) -> Result<&'a str, &'static str> {
            if self.data.is_null() {
                if self.len == 0 {
                    Ok("")
                } else {
                    Err("Null, non-empty K1 view cannot be converted to Rust str")
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackSelection {
    StaticSpace,
    CallStackCurrent,
}

#[derive(Clone)]
pub struct CompilerMessage {
    level: MessageLevel,
    message: StringId,
    filename: String,
    line: u32,
}

pub struct Vm {
    globals: FxHashMap<TypedGlobalId, Value>,
    pub static_stack: Stack,
    pub stack: Stack,
    eval_span: SpanId,
    compiler_messages: Vec<CompilerMessage>,
}

impl Vm {
    pub fn reset(&mut self, arena_global_id: Option<TypedGlobalId>) {
        // Note that we don't de-allocate any resources
        // we just zero the memory and reset the stack pointer

        let arena_to_preserve = if let Some(arena_global_id) = arena_global_id {
            if let Some(arena_value) = self.globals.get(&arena_global_id) {
                let arena_ptr: *const k1_types::Arena = arena_value.as_ptr().cast();
                debug!("Preserving core/mem/arena allocation at {:p}", arena_ptr);
                let arena: k1_types::Arena = unsafe { arena_ptr.read() };
                Some(arena)
            } else {
                None
            }
        } else {
            None
        };

        self.stack.reset();
        self.static_stack.reset();
        self.globals.clear();
        self.compiler_messages.clear();

        if let Some(mut arena) = arena_to_preserve {
            // Voila!
            arena.curAddr = arena.basePtr.addr() as u64;
            let arena_ptr = self.static_stack.push_t(arena);
            self.globals.insert(arena_global_id.unwrap(), Value::ptr(arena_ptr));
        }

        self.eval_span = SpanId::NONE;
    }

    pub fn make() -> Self {
        let stack = Stack::make();
        let static_stack = Stack::make();

        Self {
            globals: FxHashMap::with_capacity(8192),
            static_stack,
            stack,
            eval_span: SpanId::NONE,
            compiler_messages: Vec::with_capacity(16),
        }
    }

    pub fn dump(&mut self, k1: &TypedProgram) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for index in 0..self.stack.frames.len() {
            let frame_string = self.dump_frame(k1, index as u32);
            writeln!(w, "{}", frame_string).unwrap();
        }
        s
    }

    pub fn get_destination_stack(&mut self, dst_stack: StackSelection) -> &mut Stack {
        match dst_stack {
            StackSelection::StaticSpace => &mut self.static_stack,
            StackSelection::CallStackCurrent => &mut self.stack,
        }
    }

    pub fn dump_current_frame(&self, k1: &TypedProgram) {
        let s = self.dump_frame(k1, self.stack.current_frame_index());
        eprintln!("{s}");
    }

    pub fn dump_frame(&self, k1: &TypedProgram, frame_index: u32) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        let frame = &self.stack.frames[frame_index as usize];
        writeln!(w, "Frame [{frame_index}] ").unwrap();
        bc::display_unit_name(w, k1, frame.unit).unwrap();
        writeln!(w, "Base:  {:?}", frame.base_ptr).unwrap();
        if let Some(ret_info) = frame.ret_info {
            write!(w, "ret ").unwrap();
            match ret_info.place {
                RetPlace::ScalarCallInst { frame_index, inst_index } => {
                    write!(w, " [{frame_index}] reg {inst_index}").unwrap()
                }
                RetPlace::Addr { addr } => {
                    render_debug_value(w, self, k1, ret_info.t, Value::ptr(addr.cast_const()))
                        .unwrap();
                }
            }
            writeln!(w).unwrap();
        }
        let unit = bc::get_compiled_unit(&k1.bytecode, frame.unit).unwrap();
        for (i, pt) in k1.bytecode.mem.getn(unit.fn_params).iter().enumerate() {
            writeln!(w, "Param {i}: ").unwrap();
            let value = self.stack.get_param_value(frame_index, i as u32);
            render_debug_value(w, self, k1, *pt, value).unwrap();
        }
        writeln!(w, "Locals").unwrap();
        for block in k1.bytecode.mem.getn(unit.blocks) {
            for inst_id in k1.bytecode.mem.getn(block.instrs) {
                let inst_index = inst_to_index(*inst_id, unit.inst_offset);
                let local = self.stack.get_inst_value(frame.index, inst_index);
                let kind = bc::get_inst_kind(&k1.bytecode, &k1.types, *inst_id);
                match kind {
                    InstKind::Value(physical_type) => {
                        write!(w, "  i{}: ", inst_id).unwrap();
                        write!(w, " ").unwrap();
                        let type_to_use = match k1.bytecode.instrs.get(*inst_id) {
                            Inst::Alloca { t, .. } => *t,
                            _ => physical_type,
                        };
                        render_debug_value(w, self, k1, type_to_use, local).unwrap();
                        writeln!(w).unwrap()
                    }
                    InstKind::Void => {
                        continue;
                    }
                    InstKind::Terminator => {
                        continue;
                    }
                };
            }
        }

        let bytes = self.stack.frame_to_bytes(frame.index);
        write!(w, "\nDATA ({} bytes)\n", bytes.len()).unwrap();
        write_bytes(w, bytes).unwrap();
        s
    }
}

fn write_bytes(w: &mut impl std::fmt::Write, bytes: &[u8]) -> std::fmt::Result {
    for bytes in bytes.chunks(8) {
        for b in bytes {
            write!(w, "{:02x} ", b)?;
        }
        writeln!(w)?;
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
// Pure storage to be interpreted
#[repr(transparent)]
pub struct Value(u64);

impl Value {
    pub const UNIT: Value = Self::u8(UNIT_BYTE_VALUE);
    pub const TRUE: Value = Self::bool(true);
    pub const NULLPTR: Value = Self(0);

    pub const fn bool(b: bool) -> Value {
        if b { Value(1) } else { Value(0) }
    }

    fn as_bool(&self) -> bool {
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

    pub fn ptr(ptr: *const u8) -> Value {
        Value(ptr.addr() as u64)
    }

    const fn bits(&self) -> u64 {
        self.0
    }

    const fn as_usize(&self) -> usize {
        self.0 as usize
    }

    const fn u8(u8: u8) -> Value {
        Value(u8 as u64)
    }

    const fn u16(u16: u16) -> Value {
        Value(u16 as u64)
    }

    const fn u32(u32: u32) -> Value {
        Value(u32 as u64)
    }

    const fn u64(u64: u64) -> Value {
        Value(u64)
    }

    const fn i8(i8: i8) -> Value {
        Value(i8 as u64)
    }

    const fn i16(i16: i16) -> Value {
        Value(i16 as u64)
    }

    const fn i32(i32: i32) -> Value {
        Value(i32 as u64)
    }

    const fn i64(i64: i64) -> Value {
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
    fn as_ptr(&self) -> *const u8 {
        let p = self.0 as *const u8;
        sanity_check_ptr(p);
        p
    }

    fn as_ptr_unchecked(&self) -> *const u8 {
        let p = self.0 as *const u8;
        p
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

    pub fn as_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn inst_to_index(inst_id: InstId, offset: u32) -> u32 {
    inst_id.as_u32() - offset
}

pub fn execute_compiled_unit(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    expr_id: TypedExprId,
    args: &[StaticValueId],
    // These should have gotten compiled to StaticValue instructions
    _input_parameters: &[(VariableId, StaticValueId)],
) -> TyperResult<StaticValueId> {
    let start = k1.timing.clock.raw();
    let span = k1.exprs.get_span(expr_id);
    vm.eval_span = span;
    let unit = *k1.bytecode.exprs.get(&expr_id).unwrap();

    if unit.is_debug {
        eprintln!("[vm] Executing Unit\n{}", bc::compiled_unit_to_string(k1, unit.unit_id, true));
    }

    match unit.unit_id {
        CompilableUnitId::Function(_function_id) => {
            return failf!(span, "Cannot execute function from top");
        }
        CompilableUnitId::Expr(_) => {
            // Executing an expr. Execution will just end at some point, and
            // we'll just decode the last value we got I suppose
        }
    };
    let top_ret_info = match unit.expr_ret_kind {
        None => None,
        Some(ret_pt) => {
            let pt_layout = k1.types.get_pt_layout(&ret_pt);
            let ret_addr = vm.stack.push_layout_uninit(pt_layout);
            debug!(
                "Pushing ret place for type {} at addr: {:p}",
                types::pt_to_string(&k1.types, &ret_pt),
                ret_addr
            );
            Some(RetInfo { place: RetPlace::Addr { addr: ret_addr }, t: ret_pt, ip: 0, block: 0 })
        }
    };
    vm.stack.push_new_frame(Some(span), &unit, top_ret_info);
    let top_frame_index = vm.stack.current_frame_index();
    debug_assert_eq!(top_frame_index, 0);

    for (param_index, arg) in args.iter().enumerate() {
        let value = static_value_to_vm_value(k1, *arg, span);
        vm.stack.set_param_value(top_frame_index, param_index as u32, value);
    }

    let exit_code = exec_loop(k1, vm, unit)?;

    let end = k1.timing.clock.raw();
    k1.timing.total_vm_nanos += k1.timing.clock.delta_as_nanos(start, end);

    report_execution_messages(k1, vm, span, exit_code);

    if exit_code != 0 {
        failf!(span, "Static execution exited with code: {}", exit_code)
    } else {
        match top_ret_info {
            None => Ok(k1.static_values.unit_id()),
            Some(info) => {
                let expr_type = k1.exprs.get_type(expr_id);
                let RetPlace::Addr { addr } = info.place else { panic!("We set this to Addr") };
                let final_value = match info.t {
                    PhysicalType::Scalar(scalar_type) => {
                        // Since we stored the return value to this location, we need to load it
                        // before invoking static conversion
                        let loaded = load_scalar(scalar_type, addr.cast_const());
                        loaded
                    }
                    PhysicalType::Agg(_) => Value::ptr(addr.cast_const()),
                };
                let returned_value = vm_value_to_static_value(k1, expr_type, final_value, span)?;
                Ok(returned_value)
            }
        }
    }
}

fn exec_loop(k1: &mut TypedProgram, vm: &mut Vm, original_unit: CompiledUnit) -> TyperResult<i32> {
    let mut prev_b: u32 = 0;
    let mut b: u32 = 0;
    let mut ip: u32 = 0;
    let mut blocks = original_unit.blocks;
    let mut inst_offset = original_unit.inst_offset;
    let mut instrs = k1.bytecode.mem.get_nth(original_unit.blocks, b as usize).instrs;

    macro_rules! goto_unit {
        ($gt_blocks: expr, $inst_offset: expr, $gt_block: expr, $gt_ip: expr) => {{
            blocks = $gt_blocks;
            b = $gt_block;
            instrs = k1.bytecode.mem.get_nth(blocks, b as usize).instrs;
            inst_offset = $inst_offset;
            ip = $gt_ip;
        }};
    }

    macro_rules! jump {
        ($gt_block: expr) => {{
            prev_b = b;
            b = $gt_block;
            instrs = k1.bytecode.mem.get_nth(blocks, b as usize).instrs;
            ip = 0;
        }};
    }

    macro_rules! resolve_value {
        ($v:expr) => {
            resolve_value(k1, vm, vm.stack.current_frame_index(), inst_offset, $v)?
        };
    }

    //let mut line = String::new();
    let exit_code = 'exec: loop {
        // Fetch
        let inst_id = *k1.bytecode.mem.get_nth(instrs, ip as usize);
        vm.eval_span = *k1.bytecode.sources.get(inst_id);
        let inst_index = inst_to_index(inst_id, inst_offset);

        // if debugger {
        //eprintln!(
        //    "{}[{}] i{} {}",
        //    "  ".repeat(vm.stack.current_frame_index() as usize),
        //    vm.stack.current_frame_index(),
        //    inst_id.as_u32(),
        //    bc::inst_to_string(k1, inst_id)
        //);
        //std::io::stdin().read_line(&mut line).unwrap();
        //if &line == "v\n" {
        //    vm.dump_current_frame(k1);
        //}
        //line.clear();
        // }

        // ~Decode~ Execute
        match *k1.bytecode.instrs.get(inst_id) {
            Inst::Data(imm) => {
                let value = match imm {
                    bc::DataInst::U64(v) => Value::u64(v),
                    bc::DataInst::I64(v) => Value::i64(v),
                    bc::DataInst::Float(fv) => match fv {
                        TypedFloatValue::F32(f32) => Value::f32(f32),
                        TypedFloatValue::F64(f64) => Value::f64(f64),
                    },
                };

                vm.stack.set_cur_inst_value(inst_index, value);
                ip += 1
            }
            Inst::Alloca { t: _, vm_layout } => {
                let ptr = vm.stack.push_layout_uninit(vm_layout);

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(ptr));
                ip += 1
            }
            Inst::Store { dst, value, t } => {
                let dst = resolve_value!(dst);

                let vm_value = resolve_value!(value);
                store_scalar(t, dst.as_ptr().cast_mut(), vm_value);

                // Store Produces no value, no need to set
                ip += 1;
            }
            Inst::Load { t, src } => {
                let src_value = resolve_value!(src);
                let src_ptr = src_value.as_ptr();
                let loaded_value = load_scalar(t, src_ptr);

                vm.stack.set_cur_inst_value(inst_index, loaded_value);
                ip += 1;
            }
            Inst::Copy { dst, src, vm_size, .. } => {
                let dst_value = resolve_value!(dst);
                let src_value = resolve_value!(src);
                let dst_ptr = dst_value.as_ptr();
                let src_ptr = src_value.as_ptr();

                memmove(src_ptr, dst_ptr.cast_mut(), vm_size as usize);
                ip += 1
            }
            Inst::StructOffset { base, vm_offset, .. } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_ptr();
                let field_ptr = unsafe { base_ptr.byte_add(vm_offset as usize) };

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(field_ptr));
                ip += 1
            }
            Inst::ArrayOffset { element_t, base, element_index } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_ptr();
                let index_value = resolve_value!(element_index);
                let index = index_value.bits();
                let elem_layout = k1.types.get_pt_layout(&element_t);

                let element_ptr =
                    unsafe { base_ptr.byte_add(elem_layout.offset_at_index(index as usize)) };

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(element_ptr));
                ip += 1
            }
            Inst::Call { id } => {
                let call = k1.bytecode.calls.get(id);
                let ret_inst_kind = call.ret_inst_kind;
                // Figure out where we're returning to
                let return_frame_index = vm.stack.current_frame_index();
                let (return_type, return_place) = match ret_inst_kind {
                    InstKind::Value(ret_type) => {
                        let ret_place = match call.dst {
                            Some(bc_dst) => {
                                RetPlace::Addr { addr: resolve_value!(bc_dst).as_ptr().cast_mut() }
                            }
                            None => RetPlace::ScalarCallInst {
                                inst_index,
                                frame_index: return_frame_index,
                            },
                        };
                        (Some(ret_type), ret_place)
                    }
                    InstKind::Void => (
                        None,
                        RetPlace::ScalarCallInst { inst_index, frame_index: return_frame_index },
                    ),

                    InstKind::Terminator => (
                        None,
                        RetPlace::ScalarCallInst { inst_index, frame_index: return_frame_index },
                    ),
                };
                let ret_info = match return_type {
                    None => None,
                    Some(t) => Some(RetInfo { t, place: return_place, ip: ip + 1, block: b }),
                };

                macro_rules! builtin_return {
                    ($value:expr) => {{
                        fulfill_return(k1, vm, ret_info.unwrap(), $value);
                        ip += 1;
                        continue 'exec;
                    }};
                }
                let call = k1.bytecode.calls.get(id);
                let call_args = call.args;
                let dispatch_function_id = match call.callee {
                    BcCallee::Extern(lib_name, name, function_id) => {
                        let result: Value = vm_ffi::handle_ffi_call(
                            k1,
                            vm,
                            vm.stack.current_frame_index(),
                            inst_offset,
                            ret_inst_kind,
                            call_args,
                            lib_name,
                            name,
                            function_id,
                        )?;
                        builtin_return!(result)
                    }
                    BcCallee::Builtin(bc_builtin) => match bc_builtin {
                        bc::BcBuiltin::TypeSchema => {
                            // intern fn typeSchema(id: u64): TypeSchema
                            let type_id_arg =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0)).bits();
                            let type_id =
                                TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                            let Some(schema_static_value_id) = k1.type_schemas.get(&type_id) else {
                                vm_ice!(
                                    k1,
                                    vm,
                                    "Missing type schema: {}",
                                    k1.type_id_to_string(type_id)
                                )
                            };
                            let schema_vm_value =
                                static_value_to_vm_value(k1, *schema_static_value_id, vm.eval_span);
                            builtin_return!(schema_vm_value);
                        }
                        bc::BcBuiltin::TypeName => {
                            // intern fn typeName(id: u64): string
                            let type_id_arg =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0)).bits();
                            let type_id =
                                TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                            let name_value_id = *k1.type_names.get(&type_id).unwrap();

                            let name_string_value =
                                static_value_to_vm_value(k1, name_value_id, vm.eval_span);
                            builtin_return!(name_string_value);
                        }
                        bc::BcBuiltin::Allocate | bc::BcBuiltin::AllocateZeroed => {
                            // intern fn allocZeroed(size: uword, align: uword): Pointer
                            let zero = bc_builtin == bc::BcBuiltin::AllocateZeroed;
                            let size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let align: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1));
                            let Ok(layout) = std::alloc::Layout::from_size_align(
                                size.0 as usize,
                                align.0 as usize,
                            ) else {
                                vm_crash(
                                    k1,
                                    vm,
                                    format!(
                                        "Rust didn't like this layout: size={size}, align={align}"
                                    ),
                                )
                            };
                            let ptr = allocate(layout, zero);

                            builtin_return!(Value::ptr(ptr.cast_const()));
                        }
                        bc::BcBuiltin::Reallocate => {
                            // intern fn realloc(ptr: Pointer, oldSize: uword, align: uword, newSize: uword): Pointer
                            let old_ptr: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let old_size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1));
                            let align: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2));
                            let new_size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 3));
                            let layout = std::alloc::Layout::from_size_align(
                                old_size.as_usize(),
                                align.as_usize(),
                            )
                            .unwrap();
                            let ptr = unsafe {
                                std::alloc::realloc(
                                    old_ptr.as_ptr().cast_mut(),
                                    layout,
                                    new_size.as_usize(),
                                )
                            };

                            builtin_return!(Value::ptr(ptr.cast_const()));
                        }
                        bc::BcBuiltin::Free => {
                            // intern fn free(ptr: Pointer, size: uword, align: uword): unit
                            let ptr =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0)).as_ptr();
                            let size =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1)).as_usize();
                            let align =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2)).as_usize();

                            let layout =
                                std::alloc::Layout::from_size_align(size as usize, align as usize)
                                    .unwrap();
                            unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };

                            builtin_return!(Value::ptr(ptr))
                        }
                        bc::BcBuiltin::MemCopy => {
                            // intern fn copy( dst: Pointer, src: Pointer, count: uword): unit
                            let dst: Value = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let src: Value = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1));
                            let count: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2));
                            memcopy(src.as_ptr(), dst.as_ptr().cast_mut(), count.as_usize());

                            builtin_return!(Value::UNIT)
                        }
                        bc::BcBuiltin::MemSet => {
                            //intern fn set(dst: Pointer, value: u8, count: uword): unit
                            let dst: Value = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let value: u8 =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1)).bits() as u8;
                            let count: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2));
                            unsafe {
                                std::ptr::write_bytes(
                                    dst.as_ptr().cast_mut(),
                                    value,
                                    count.as_usize(),
                                )
                            };

                            builtin_return!(Value::UNIT)
                        }
                        bc::BcBuiltin::MemEquals => {
                            //intern fn equals(p1: Pointer, p2: Pointer, size: uword): bool
                            let p1: Value = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let p2: Value = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1));
                            let size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2));

                            let p1_ptr = p1.as_ptr();
                            let p2_ptr = p2.as_ptr();
                            let size_usize = size.bits() as usize;

                            let p1_slice =
                                unsafe { slice_from_raw_parts_checked(vm, k1, p1_ptr, size_usize) };
                            let p2_slice =
                                unsafe { slice_from_raw_parts_checked(vm, k1, p2_ptr, size_usize) };

                            let eq = p1_slice == p2_slice;
                            let value = Value::bool(eq);

                            builtin_return!(value)
                        }
                        bc::BcBuiltin::Exit => {
                            let exit_code =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0)).bits();
                            break exit_code as i32;
                        }
                        bc::BcBuiltin::CompilerMessage => {
                            // intern fn emitCompilerMessage(
                            //    locn: compiler/SourceLocation,
                            //    level: (either(u8) Info, Warn, Error),
                            //    msg: string
                            //  ): unit
                            // todo!()
                            let location_arg =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 0));
                            let level_arg = resolve_value!(*k1.bytecode.mem.get_nth(call_args, 1));
                            let message_arg =
                                resolve_value!(*k1.bytecode.mem.get_nth(call_args, 2));
                            let location = unsafe {
                                (location_arg.as_ptr() as *const k1_types::K1SourceLocation).read()
                            };
                            let level = unsafe {
                                (level_arg.as_ptr() as *const k1_types::CompilerMessageLevel).read()
                            };
                            let level = match level {
                                k1_types::CompilerMessageLevel::Info => MessageLevel::Info,
                                k1_types::CompilerMessageLevel::Warn => MessageLevel::Warn,
                                k1_types::CompilerMessageLevel::Error => MessageLevel::Error,
                            };
                            let message = value_to_string_id(k1, message_arg).map_err(|msg| {
                                errf!(
                                    vm.eval_span,
                                    "Bad message string passed to EmitCompilerMessage: {msg}"
                                )
                            })?;
                            let filename =
                                unsafe { location.filename.to_str() }.map_err(|msg| {
                                    errf!(
                                        vm.eval_span,
                                        "Bad filename string passed to EmitCompilerMessage: {msg}"
                                    )
                                })?;

                            eprintln!(
                                "[{}:{} {}] {}",
                                filename,
                                location.line,
                                level.name_str().color(level.color()),
                                k1.get_string(message)
                            );

                            vm.compiler_messages.push(CompilerMessage {
                                level,
                                message,
                                filename: filename.to_string(),
                                line: location.line as u32,
                            });

                            builtin_return!(Value::UNIT)
                        }
                    },
                    BcCallee::Direct(function_id) => function_id,
                    BcCallee::Indirect(value) => {
                        // Decode function id from 'pointer'
                        let callee_value = resolve_value!(value);
                        let function_id_u64 = callee_value.bits();
                        let function_id_nzu32 = NonZeroU32::new(function_id_u64 as u32).unwrap();
                        let function_id = FunctionId::from_nzu32(function_id_nzu32);
                        function_id
                    }
                };
                let Some(compiled_function) = k1.bytecode.functions.get(dispatch_function_id)
                else {
                    return failf!(
                        vm.eval_span,
                        "Call to uncompiled function: {}. ({} are pending)",
                        k1.function_id_to_string(dispatch_function_id, false),
                        k1.bytecode.b_units_pending_compile.len()
                    );
                };
                let caller_frame_index = vm.stack.current_frame_index();
                let caller_inst_offset = inst_offset;
                let called_blocks = compiled_function.blocks;
                let called_inst_offset = compiled_function.inst_offset;
                let new_frame_index = caller_frame_index + 1;

                // - Push a stack frame
                vm.stack.push_new_frame(Some(vm.eval_span), compiled_function, ret_info);
                if vm.stack.frames.len() >= 1024 {
                    eprintln!("vm stack {}", vm.stack.frames.len());
                    eprintln!("{}", make_stack_trace(k1, &vm.stack))
                }
                debug_assert_eq!(new_frame_index, vm.stack.current_frame_index());

                // Prepare the function's arguments
                for (index, arg) in k1.bytecode.mem.getn(call_args).iter().enumerate() {
                    // Note: These need to execute before we push, in case they access this stack's
                    // params or instrs by index, which is basically always! The other option would
                    // be parameterizing 'resolve_value' by stack frame
                    let vm_value =
                        resolve_value(k1, vm, caller_frame_index, caller_inst_offset, *arg)?;

                    vm.stack.set_param_value(new_frame_index, index as u32, vm_value);
                }

                // - Set 'pc' (which is blocks + b + i)
                goto_unit!(called_blocks, called_inst_offset, 0, 0);
            }
            Inst::Ret(bc_value) => {
                let cur_frame = vm.stack.current_frame();
                let Some(ret_info) = cur_frame.ret_info else {
                    return failf!(vm.eval_span, "Return with no ret_info");
                };

                let returned_value = resolve_value!(bc_value);

                //eprintln!("Ret: {}", debug_value_to_string(vm, k1, ret_info.t, returned_value));
                //vm.dump_current_frame(k1);
                fulfill_return(k1, vm, ret_info, returned_value);

                let _popped = vm.stack.pop_frame();

                match vm.stack.current_frame_opt() {
                    None => {
                        break 'exec 0;
                    }
                    Some(current) => {
                        goto_unit!(
                            current.blocks,
                            current.inst_offset,
                            ret_info.block,
                            ret_info.ip
                        );
                    }
                };
            }
            Inst::Jump(block_index) => {
                jump!(block_index);
            }
            Inst::JumpIf { cond, cons, alt } => {
                let cond_value = resolve_value!(cond);
                if cond_value.as_bool() {
                    jump!(cons);
                } else {
                    jump!(alt);
                }
            }
            Inst::Unreachable => {
                return failf!(vm.eval_span, "Reached unreachable instruction");
            }
            Inst::CameFrom { t: _, incomings } => {
                debug_assert!(!incomings.is_empty());
                let mut value: core::mem::MaybeUninit<BcValue> = core::mem::MaybeUninit::uninit();
                'case: for case in k1.bytecode.mem.getn(incomings) {
                    if case.from == prev_b {
                        value = core::mem::MaybeUninit::new(case.value);
                        break 'case;
                    }
                }
                let value = unsafe { value.assume_init() };
                let vm_value = resolve_value!(value);

                vm.stack.set_cur_inst_value(inst_index, vm_value);
                ip += 1;
            }
            Inst::BoolNegate { v } => {
                let b = resolve_value!(v).as_bool();
                vm.stack.set_cur_inst_value(inst_index, Value::bool(!b));
                ip += 1
            }
            Inst::BitNot { v } => {
                // We invert all bits, even the ones not in play for the width;
                // but that is fine since we're operating on a Vm Value not a
                // memory address where we're worried about corrupting data
                let input = resolve_value!(v);
                let result = !input.bits();

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::BitCast { v, to: _ } => {
                let input = resolve_value!(v);

                vm.stack.set_cur_inst_value(inst_index, input);
                ip += 1
            }
            Inst::IntTrunc { v, to: _ } => {
                let input = resolve_value!(v);

                // For now, truncate is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            Inst::IntExtU { v, to: _ } => {
                let input = resolve_value!(v);

                // For now, extend is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            Inst::IntExtS { v, to: _ } => {
                let input = resolve_value!(v);

                // For now, extend is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            Inst::FloatTrunc { v, to: _ } => {
                let from: f64 = resolve_value!(v).as_f64();
                let result = from as f32;

                vm.stack.set_cur_inst_value(inst_index, Value::f32(result));
                ip += 1
            }
            Inst::FloatExt { v, to: _ } => {
                let from: f32 = resolve_value!(v).as_f32();
                let result = from as f64;

                vm.stack.set_cur_inst_value(inst_index, Value::f64(result));
                ip += 1
            }
            Inst::Float32ToIntUnsigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f32();

                let result = match to {
                    ScalarType::U8 => Value::u8(f as u8),
                    ScalarType::U16 => Value::u16(f as u16),
                    ScalarType::U32 => Value::u32(f as u32),
                    ScalarType::U64 => Value::u64(f as u64),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            Inst::Float32ToIntSigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f32();

                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            Inst::Float64ToIntUnsigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f64();

                let result = match to {
                    ScalarType::I8 => Value::u8(f as u8),
                    ScalarType::I16 => Value::u16(f as u16),
                    ScalarType::I32 => Value::u32(f as u32),
                    ScalarType::I64 => Value::u64(f as u64),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            Inst::Float64ToIntSigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f64();

                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            Inst::IntToFloatUnsigned { v, from, to } => {
                let int_value = resolve_value!(v);
                let result = match (from, to) {
                    (ScalarType::U8, ScalarType::F32) => {
                        let i = int_value.bits() as u8;
                        Value::f32(i as f32)
                    }
                    (ScalarType::U16, ScalarType::F32) => {
                        let i = int_value.bits() as u16;
                        Value::f32(i as f32)
                    }
                    (ScalarType::U32, ScalarType::F32) => {
                        let i = int_value.bits() as u32;
                        Value::f32(i as f32)
                    }
                    (ScalarType::U64, ScalarType::F32) => {
                        let i = int_value.bits() as u64;
                        Value::f32(i as f32)
                    }
                    (ScalarType::U8, ScalarType::F64) => {
                        let i = int_value.bits() as u8;
                        Value::f64(i as f64)
                    }
                    (ScalarType::U16, ScalarType::F64) => {
                        let i = int_value.bits() as u16;
                        Value::f64(i as f64)
                    }
                    (ScalarType::U32, ScalarType::F64) => {
                        let i = int_value.bits() as u32;
                        Value::f64(i as f64)
                    }
                    (ScalarType::U64, ScalarType::F64) => {
                        let i = int_value.bits() as u64;
                        Value::f64(i as f64)
                    }
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            Inst::IntToFloatSigned { v, from, to } => {
                let int_value = resolve_value!(v);
                let result = match (from, to) {
                    (ScalarType::I8, ScalarType::F32) => {
                        let i = int_value.bits() as i8;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I16, ScalarType::F32) => {
                        let i = int_value.bits() as i16;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I32, ScalarType::F32) => {
                        let i = int_value.bits() as i32;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I64, ScalarType::F32) => {
                        let i = int_value.bits() as i64;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I8, ScalarType::F64) => {
                        let i = int_value.bits() as i8;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I16, ScalarType::F64) => {
                        let i = int_value.bits() as i16;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I32, ScalarType::F64) => {
                        let i = int_value.bits() as i32;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I64, ScalarType::F64) => {
                        let i = int_value.bits() as i64;
                        Value::f64(i as f64)
                    }
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            Inst::PtrToWord { v } => {
                let v = resolve_value!(v);

                vm.stack.set_cur_inst_value(inst_index, v);
                ip += 1
            }
            Inst::WordToPtr { v } => {
                let v = resolve_value!(v);

                vm.stack.set_cur_inst_value(inst_index, v);
                ip += 1
            }
            Inst::IntAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntDivUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_uop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntDivSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_iop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntRemUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_uop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntRemSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_iop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::IntCmp { lhs, rhs, pred, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let b = match (width, pred) {
                    (_, bc::IntCmpPred::Eq) => lhs == rhs,
                    (8, bc::IntCmpPred::Slt) => (lhs as i8) < (rhs as i8),
                    (8, bc::IntCmpPred::Sle) => (lhs as i8) <= (rhs as i8),
                    (8, bc::IntCmpPred::Sgt) => (lhs as i8) > (rhs as i8),
                    (8, bc::IntCmpPred::Sge) => (lhs as i8) >= (rhs as i8),
                    (8, bc::IntCmpPred::Ult) => (lhs as u8) < (rhs as u8),
                    (8, bc::IntCmpPred::Ule) => (lhs as u8) <= (rhs as u8),
                    (8, bc::IntCmpPred::Ugt) => (lhs as u8) > (rhs as u8),
                    (8, bc::IntCmpPred::Uge) => (lhs as u8) >= (rhs as u8),
                    (16, bc::IntCmpPred::Slt) => (lhs as i16) < (rhs as i16),
                    (16, bc::IntCmpPred::Sle) => (lhs as i16) <= (rhs as i16),
                    (16, bc::IntCmpPred::Sgt) => (lhs as i16) > (rhs as i16),
                    (16, bc::IntCmpPred::Sge) => (lhs as i16) >= (rhs as i16),
                    (16, bc::IntCmpPred::Ult) => (lhs as u16) < (rhs as u16),
                    (16, bc::IntCmpPred::Ule) => (lhs as u16) <= (rhs as u16),
                    (16, bc::IntCmpPred::Ugt) => (lhs as u16) > (rhs as u16),
                    (16, bc::IntCmpPred::Uge) => (lhs as u16) >= (rhs as u16),
                    (32, bc::IntCmpPred::Slt) => (lhs as i32) < (rhs as i32),
                    (32, bc::IntCmpPred::Sle) => (lhs as i32) <= (rhs as i32),
                    (32, bc::IntCmpPred::Sgt) => (lhs as i32) > (rhs as i32),
                    (32, bc::IntCmpPred::Sge) => (lhs as i32) >= (rhs as i32),
                    (32, bc::IntCmpPred::Ult) => (lhs as u32) < (rhs as u32),
                    (32, bc::IntCmpPred::Ule) => (lhs as u32) <= (rhs as u32),
                    (32, bc::IntCmpPred::Ugt) => (lhs as u32) > (rhs as u32),
                    (32, bc::IntCmpPred::Uge) => (lhs as u32) >= (rhs as u32),
                    (64, bc::IntCmpPred::Slt) => (lhs as i64) < (rhs as i64),
                    (64, bc::IntCmpPred::Sle) => (lhs as i64) <= (rhs as i64),
                    (64, bc::IntCmpPred::Sgt) => (lhs as i64) > (rhs as i64),
                    (64, bc::IntCmpPred::Sge) => (lhs as i64) >= (rhs as i64),
                    (64, bc::IntCmpPred::Ult) => lhs < rhs,
                    (64, bc::IntCmpPred::Ule) => lhs <= rhs,
                    (64, bc::IntCmpPred::Ugt) => lhs > rhs,
                    (64, bc::IntCmpPred::Uge) => lhs >= rhs,
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, Value(b as u64));
                ip += 1
            }
            Inst::FloatAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Add;
                let result = casted_float_op!(width, add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::FloatSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Sub;
                let result = casted_float_op!(width, sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::FloatMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Mul;
                let result = casted_float_op!(width, mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::FloatDiv { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Div;
                let result = casted_float_op!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::FloatRem { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Rem;
                let result = casted_float_op!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            Inst::FloatCmp { lhs, rhs, pred, width } => {
                let lhs = resolve_value!(lhs);
                let rhs = resolve_value!(rhs);
                let b = match (width, pred) {
                    (_, bc::FloatCmpPred::Eq) => lhs.bits() == rhs.bits(),
                    (32, bc::FloatCmpPred::Lt) => lhs.as_f32() < rhs.as_f32(),
                    (32, bc::FloatCmpPred::Le) => lhs.as_f32() <= rhs.as_f32(),
                    (32, bc::FloatCmpPred::Gt) => lhs.as_f32() > rhs.as_f32(),
                    (32, bc::FloatCmpPred::Ge) => lhs.as_f32() >= rhs.as_f32(),
                    (64, bc::FloatCmpPred::Lt) => lhs.as_f64() < rhs.as_f64(),
                    (64, bc::FloatCmpPred::Le) => lhs.as_f64() <= rhs.as_f64(),
                    (64, bc::FloatCmpPred::Gt) => lhs.as_f64() > rhs.as_f64(),
                    (64, bc::FloatCmpPred::Ge) => lhs.as_f64() >= rhs.as_f64(),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, Value(b as u64));
                ip += 1
            }
            Inst::BitAnd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::BitAnd;
                let result = casted_uop!(width, bitand, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BitOr { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::BitOr;
                let result = casted_uop!(width, bitor, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BitXor { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::BitXor;
                let result = casted_uop!(width, bitxor, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BitShiftLeft { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).as_u32();
                use std::ops::Shl;
                let result = casted_uop!(width, shl, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).as_u32();
                use std::ops::Shr;
                let result = casted_uop!(width, shr, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BitSignedShiftRight { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).as_u32();
                use std::ops::Shr;
                let result = casted_iop!(width, shr, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            Inst::BakeStaticValue { type_id, value } => {
                // intern fn bakeStaticValue[T](value: T): u64
                let value_value = resolve_value!(value);
                let value_id = vm_value_to_static_value(k1, type_id, value_value, vm.eval_span)?;

                let value_id_u64 = value_id.as_u32() as u64;

                vm.stack.set_cur_inst_value(inst_index, Value::u64(value_id_u64));
                ip += 1;
            }
        }
    };
    Ok(exit_code)
}

#[inline(always)]
fn resolve_value(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    frame_index: u32,
    inst_offset: u32,
    value: BcValue,
) -> TyperResult<Value> {
    match value {
        BcValue::Inst(inst_id) => {
            let inst_index = inst_to_index(inst_id, inst_offset);
            let v = vm.stack.get_inst_value(frame_index, inst_index);
            Ok(v)
        }
        BcValue::StaticValue { t: _, id } => {
            let v = static_value_to_vm_value(k1, id, vm.eval_span);
            Ok(v)
        }
        BcValue::FunctionAddr(function_id) => {
            let value = function_id_to_ref_value(function_id);
            Ok(value)
        }
        BcValue::FnParam { t: _, index } => {
            let value = vm.stack.get_param_value(frame_index, index);
            // eprintln!("Accessed frame{} p{index}: {}", vm.stack.current_frame_index(), value);
            Ok(value)
        }
        BcValue::Imm32 { t, data } => {
            let u32_data = data;
            let value = match t {
                ScalarType::F32 => Value(u32_data as u64),
                ScalarType::F64 => Value::f64(data as f32 as f64),
                ScalarType::Pointer => Value(data as u64),
                ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64 => {
                    Value(u32_data as i32 as i64 as u64)
                }
                ScalarType::U8 | ScalarType::U16 | ScalarType::U32 | ScalarType::U64 => {
                    Value(u32_data as u64)
                }
            };
            Ok(value)
        }
        BcValue::PtrZero => Ok(Value::NULLPTR),
        BcValue::Global { t, id } => resolve_global(k1, vm, id, t),
    }
}

#[inline(always)]
fn resolve_global(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    global_id: TypedGlobalId,
    t: PhysicalType,
) -> TyperResult<Value> {
    // ** If referencing, allocate layout and perform a store to produce a valid address
    // Case 1: It's a constant, already evaluated, stored in the global static space
    if let Some(v) = k1.vm_global_constant_lookups.get(&global_id) {
        return Ok(*v);
    }
    // Case 2: It's instead in this VM because it's mutable and we already evaluated it, return it
    if let Some(v) = vm.globals.get(&global_id) {
        return Ok(*v);
    }

    // Case 3: First evaluation. If not mutable, put in share global constants. If mutable,
    // generate and store the shared original, but store a copy in our local vm to allow mutation
    let global = k1.globals.get(global_id);
    let is_constant = global.is_constant;
    let initial_value_id = match global.initial_value {
        None => {
            k1.eval_global_body(global.ast_id)?;
            let value_id = k1.globals.get(global_id).initial_value.unwrap();
            value_id
        }
        Some(value_id) => value_id,
    };
    debug!(
        "shared global is: {}. the `t` of the instr is: {}",
        k1.static_value_to_string(initial_value_id),
        types::pt_to_string(&k1.types, &t)
    );
    let shared_vm_value = static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
    let layout = k1.types.get_pt_layout(&t);
    if is_constant {
        let dst = k1.vm_static_stack.push_layout_uninit(layout);
        store_value(&k1.types, t, dst, shared_vm_value);
        let addr = Value::ptr(dst.cast_const());
        k1.vm_global_constant_lookups.insert(global_id, addr);
        Ok(addr)
    } else {
        // We need a local copy of this
        let dst = vm.static_stack.push_layout_uninit(layout);
        store_value(&k1.types, t, dst, shared_vm_value);
        let addr = Value::ptr(dst.cast_const());
        vm.globals.insert(global_id, addr);
        Ok(addr)
    }
}

fn fulfill_return(k1: &TypedProgram, vm: &mut Vm, ret_info: RetInfo, returned_value: Value) {
    match ret_info.place {
        RetPlace::ScalarCallInst { inst_index, frame_index } => {
            vm.stack.set_inst_value(frame_index, inst_index, returned_value);
        }
        RetPlace::Addr { addr } => {
            store_value(&k1.types, ret_info.t, addr, returned_value);
        }
    }
}

fn function_id_to_ref_value(function_id: FunctionId) -> Value {
    let function_id_u32 = function_id.as_u32();
    let function_id_as_ptr = function_id_u32 as usize as *const u8;
    debug!(
        "Encoding function id {function_id_u32} into the pointer address of a Reference value {:?}",
        function_id_as_ptr
    );
    let value = Value(function_id_u32 as u64);
    value
}

pub fn static_value_to_vm_value(
    k1: &mut TypedProgram,
    static_value_id: StaticValueId,
    span: SpanId,
) -> Value {
    if let Some(v) = k1.vm_static_value_lookups.get(&static_value_id) {
        return *v;
    };

    let v = match k1.static_values.get(static_value_id) {
        StaticValue::Unit => Value(0),
        StaticValue::Bool(bool_value) => Value::bool(*bool_value),
        StaticValue::Char(char_byte) => Value(*char_byte as u64),
        StaticValue::Int(iv) => Value(iv.to_u64_bits()),
        StaticValue::Float(fv) => Value::float_value(*fv),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(k1, *string_id);
            value
        }
        StaticValue::Zero(type_id) => static_zero_value(k1, *type_id, span),
        StaticValue::Struct(static_struct) => {
            let layout = k1.types.get_layout(static_struct.type_id);
            let struct_base = k1.vm_static_stack.push_layout_uninit(layout);

            store_static_value(k1, struct_base, static_value_id);

            Value::ptr(struct_base.cast_const())
        }
        StaticValue::Enum(e) => {
            let layout = k1.types.get_layout(e.variant_type_id);
            let enum_base = k1.vm_static_stack.push_layout_uninit(layout);

            store_static_value(k1, enum_base, static_value_id);

            Value::ptr(enum_base.cast_const())
        }
        StaticValue::LinearContainer(cont) => {
            let (element_type, _container_kind) =
                k1.types.get_as_container_instance(cont.type_id).unwrap();
            let kind = cont.kind;
            let len = cont.len();
            let layout = k1.types.get_layout(element_type);
            let array_allocation_layout = layout.array_me(cont.len());

            let array_base_ptr = k1.vm_static_stack.push_layout_uninit(array_allocation_layout);

            store_static_array_elements(k1, array_base_ptr, element_type, cont.elements);

            match kind {
                StaticContainerKind::View => {
                    let rust_view = k1_types::K1ViewLike { len, data: array_base_ptr.cast_const() };
                    let view_struct_ptr = k1.vm_static_stack.push_t(rust_view);

                    Value::ptr(view_struct_ptr)
                }
                StaticContainerKind::Array => Value::ptr(array_base_ptr.cast_const()),
            }
        }
    };
    k1.vm_static_value_lookups.insert(static_value_id, v);
    v
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_static_value(k1: &mut TypedProgram, dst: *mut u8, static_value_id: StaticValueId) {
    match k1.static_values.get(static_value_id) {
        StaticValue::Unit => store_byte(dst, 0),
        StaticValue::Bool(bool_value) => store_byte(dst, *bool_value as u8),
        StaticValue::Char(char_byte) => store_byte(dst, *char_byte),
        StaticValue::Int(iv) => store_typed_int(dst, *iv),
        StaticValue::Float(fv) => store_scalar(fv.get_scalar_type(), dst, Value::float_value(*fv)),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(k1, *string_id);
            store_value(&k1.types, k1.types.get_physical_type(STRING_TYPE_ID).unwrap(), dst, value);
        }
        StaticValue::Zero(type_id) => {
            let layout = k1.types.get_layout(*type_id);
            unsafe { std::ptr::write_bytes(dst, 0, layout.size as usize) };
        }
        StaticValue::Struct(static_struct) => {
            let struct_layout = k1.types.get_struct_layout(static_struct.type_id);

            for (field, field_value_id) in
                struct_layout.iter().zip(k1.static_values.mem.getn(static_struct.fields))
            {
                let field_ptr = unsafe { dst.byte_add(field.offset as usize) };
                store_static_value(k1, field_ptr, *field_value_id);
            }
        }
        StaticValue::Enum(e) => {
            let variant_agg_id =
                k1.types.get_physical_type(e.variant_type_id).unwrap().expect_agg();
            let variant_layout =
                k1.types.phys_types.get(variant_agg_id).agg_type.expect_enum_variant();
            let variant_type = k1.types.get(e.variant_type_id).expect_enum_variant();

            store_typed_int(dst, variant_type.tag_value);

            if let Some(payload_value_id) = e.payload {
                let payload_ptr =
                    unsafe { dst.byte_add(variant_layout.payload_offset.unwrap() as usize) };
                store_static_value(k1, payload_ptr, payload_value_id);
            };
        }
        StaticValue::LinearContainer(cont) => {
            let (element_type, _container_kind) =
                k1.types.get_as_container_instance(cont.type_id).unwrap();
            let kind = cont.kind;
            let len = cont.len();
            let layout = k1.types.get_layout(element_type);
            let array_allocation_layout = layout.array_me(cont.len());

            let array_base_ptr = match kind {
                StaticContainerKind::View => {
                    k1.vm_static_stack.push_layout_uninit(array_allocation_layout)
                }
                StaticContainerKind::Array => dst,
            };

            store_static_array_elements(k1, array_base_ptr, element_type, cont.elements);

            match kind {
                StaticContainerKind::View => {
                    // Store the struct to dst
                    let rust_view = k1_types::K1ViewLike { len, data: array_base_ptr.cast_const() };

                    unsafe { *(dst as *mut k1_types::K1ViewLike) = rust_view };
                }
                StaticContainerKind::Array => {}
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
    let element_layout = k1.types.get_layout(element_type);

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
    // This just points into the Rust memory for the string's data. Teehee uwuu
    // I need to guarantee it can't re-allocate because that's still sadly using the library
    let k1_string = k1_types::K1ViewLike { len: s.len(), data: s.as_ptr() };
    if cfg!(debug_assertions) {
        let char_view_type_id = k1.types.get_struct_field(STRING_TYPE_ID, 0).type_id;
        let string_layout = k1.types.get_layout(STRING_TYPE_ID);
        debug_assert_eq!(string_layout, k1.types.get_layout(char_view_type_id));
        debug_assert_eq!(size_of_val(&k1_string), string_layout.size as usize);
    }

    let string_stack_addr = k1.vm_static_stack.mem.push(k1_string) as *mut k1_types::K1ViewLike;
    Value::ptr(string_stack_addr.cast())
}

fn allocate(layout: std::alloc::Layout, zero: bool) -> *mut u8 {
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
    store_scalar(int.get_integer_type().get_scalar_type(), dst, Value::typed_int(int))
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
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
pub fn store_value(types: &TypePool, t: PhysicalType, dst: *mut u8, value: Value) {
    match t {
        PhysicalType::Scalar(scalar_type) => store_scalar(scalar_type, dst, value),
        PhysicalType::Agg(pt_id) => {
            let record = types.phys_types.get(pt_id);
            let src = value.as_ptr();
            memmove(src, dst, record.layout.size as usize)
        }
    }
}

fn memmove(src: *const u8, dst: *mut u8, size_bytes: usize) {
    unsafe {
        core::ptr::copy(src, dst, size_bytes);
    }
}

fn memcopy(src: *const u8, dst: *mut u8, size_bytes: usize) {
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
    match t {
        PhysicalType::Scalar(st) => load_scalar(st, ptr),
        PhysicalType::Agg(_) => Value::ptr(ptr),
    }
}

pub struct Stack {
    pub mem: crate::kmem::Mem<()>,
    frames: Vec<StackFrameRecord>,
}

#[derive(Clone, Copy)]
enum RetPlace {
    ScalarCallInst { frame_index: u32, inst_index: u32 },
    Addr { addr: *mut u8 },
}

#[derive(Clone, Copy)]
pub struct RetInfo {
    t: PhysicalType,
    // Where the return value goes. Either a register or an address
    place: RetPlace,
    ip: u32,
    block: u32,
}

#[derive(Clone)]
pub struct StackFrameRecord {
    index: u32,
    base_ptr: *const u8,
    inst_slice: *mut [Value],
    call_span: Option<SpanId>,
    param_count: u32,
    blocks: MSlice<bc::CompiledBlock, bc::ProgramBytecode>,
    inst_offset: u32,
    unit: CompilableUnitId,
    ret_info: Option<RetInfo>,
}

impl StackFrameRecord {
    fn make(
        index: u32,
        base_ptr: *const u8,
        call_span: Option<SpanId>,
        owner: &CompiledUnit,
        ret_info: Option<RetInfo>,
        inst_slice: *mut [Value],
    ) -> StackFrameRecord {
        // Frames must be 8-byte aligned
        debug_assert!((base_ptr as *const Value).is_aligned());

        StackFrameRecord {
            index,
            base_ptr,
            inst_slice,
            call_span,
            unit: owner.unit_id,
            param_count: owner.fn_params.len(),
            blocks: owner.blocks,
            inst_offset: owner.inst_offset,
            ret_info,
        }
    }
    fn param_slice<'ret>(&self) -> &'ret mut [Value] {
        unsafe {
            core::slice::from_raw_parts_mut(self.base_ptr as *mut Value, self.param_count as usize)
        }
    }
}

impl Stack {
    pub fn make() -> Stack {
        let mut mem = kmem::Mem::make();
        let expected_values_needed = 10000;
        mem.will_need(expected_values_needed * std::mem::size_of::<Value>());
        Self { mem, frames: Vec::with_capacity(512) }
    }

    pub fn reset(&mut self) {
        let zero = cfg!(debug_assertions);
        self.mem.reset(zero);
        self.frames.clear();
    }

    pub fn cursor(&self) -> *const u8 {
        self.mem.cursor()
    }

    /// Frame Layout:
    /// base_ptr: 0
    /// `param_count` x 64-bit function param Values
    /// `inst_count`  x 64-bit Values
    /// Allocas
    fn push_new_frame(
        &mut self,
        call_span: Option<SpanId>,
        owner: &CompiledUnit,
        ret_info: Option<RetInfo>,
    ) {
        let index = self.frames.len() as u32;
        self.mem.align_to_bytes(8);
        let base_ptr = self.cursor();

        let param_count = owner.fn_params.len();
        let inst_count = owner.inst_count;
        let inst_base = unsafe { base_ptr.byte_add(param_count as usize * size_of::<Value>()) };
        let inst_slice =
            core::ptr::slice_from_raw_parts_mut(inst_base as *mut Value, inst_count as usize);
        let frame = StackFrameRecord::make(index, base_ptr, call_span, owner, ret_info, inst_slice);
        self.mem.push_slice_uninit::<Value>(param_count as usize + inst_count as usize);
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) -> StackFrameRecord {
        let f = self.frames.pop().unwrap();
        self.mem.set_cursor(f.base_ptr);
        f
    }

    /// Returns the frame index
    pub fn owns_address(&self, address: usize) -> Option<u32> {
        for frame in self.frames.iter().rev() {
            let frame_base = frame.base_ptr.addr();
            if address >= frame_base && address < self.end_ptr().addr() {
                return Some(frame.index);
            }
        }
        None
    }

    fn current_frame_index(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    #[allow(unused)]
    fn caller_frame_index(&self) -> u32 {
        self.frames.len() as u32 - 2
    }

    #[allow(unused)]
    fn caller_frame(&self) -> &StackFrameRecord {
        self.frames.get(self.caller_frame_index() as usize).unwrap()
    }

    fn current_frame(&self) -> &StackFrameRecord {
        self.frames.last().unwrap()
    }

    fn current_frame_opt(&self) -> Option<&StackFrameRecord> {
        self.frames.last()
    }

    fn param_values_for_frame(&self, frame_index: u32) -> &mut [Value] {
        let f = &self.frames[frame_index as usize];
        f.param_slice()
    }

    fn inst_values_for_frame<'ret>(&self, frame_index: u32) -> &'ret mut [Value] {
        let f = &self.frames[frame_index as usize];
        unsafe { &mut *f.inst_slice }
    }

    fn get_param_value(&self, frame_index: u32, param_index: u32) -> Value {
        let vs = self.param_values_for_frame(frame_index);
        vs[param_index as usize]
    }

    fn set_param_value(&mut self, frame_index: u32, param_index: u32, value: Value) {
        let vs = self.param_values_for_frame(frame_index);
        vs[param_index as usize] = value;
    }

    pub fn get_inst_value(&self, frame_index: u32, inst_index: u32) -> Value {
        let inst_values = self.inst_values_for_frame(frame_index);
        inst_values[inst_index as usize]
    }

    pub fn set_inst_value(&mut self, frame_index: u32, inst_index: u32, value: Value) {
        let inst_values = self.inst_values_for_frame(frame_index);
        inst_values[inst_index as usize] = value;
    }

    pub fn set_cur_inst_value(&mut self, inst_index: u32, value: Value) {
        self.set_inst_value(self.current_frame_index(), inst_index, value)
    }

    pub fn get_cur_inst_value(&self, inst_index: u32) -> Value {
        self.get_inst_value(self.current_frame_index(), inst_index)
    }

    #[inline]
    pub fn base_ptr(&self) -> *const u8 {
        self.mem.base_ptr()
    }

    #[inline]
    pub fn base_addr(&self) -> usize {
        self.base_ptr().addr()
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
        self.mem.push_layout_uninit(layout)
    }

    pub fn push_t<T>(&mut self, t: T) -> *const u8 {
        let r = self.mem.push(t);
        r as *mut T as *const u8
    }

    pub fn frame_to_bytes(&self, frame_index: u32) -> &[u8] {
        let frame_start = self.frames[frame_index as usize].base_ptr;
        if let Some(end) = self.frames.get(frame_index as usize + 1) {
            let diff = end.base_ptr.addr() - frame_start.addr();
            unsafe { std::slice::from_raw_parts(self.base_ptr(), diff) }
        } else {
            unsafe { std::slice::from_raw_parts(self.base_ptr(), self.current_offset_bytes()) }
        }
    }
}

/// Please don't hang on to this reference for very long
pub fn value_to_rust_str<'a>(value: Value) -> Result<&'a str, &'static str> {
    let ptr = value.as_ptr();
    let view_ptr = ptr as *const k1_types::K1ViewLike;
    let k1_view_like = unsafe { view_ptr.read() };
    unsafe { k1_view_like.to_str() }
}

pub fn value_to_string_id(m: &mut TypedProgram, value: Value) -> Result<StringId, &'static str> {
    let rust_str = value_to_rust_str(value)?;
    Ok(m.ast.strings.intern(rust_str))
}

pub fn value_to_ident(m: &mut TypedProgram, value: Value) -> Result<Ident, &'static str> {
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
) -> TyperResult<StaticValueId> {
    debug!("vm_to_static: {:?}: {}", vm_value, k1.type_id_to_string(type_id));
    let Some(_pt) = k1.types.get_physical_type(type_id) else {
        return failf!(
            span,
            "Not a physical type, cannot bake to static: {}",
            k1.type_id_to_string(type_id)
        );
    };
    // We know it is a physical type so can be aggressive with matches
    let static_value_id = match k1.types.get(type_id) {
        Type::Unit => k1.static_values.unit_id(),
        Type::Char => k1.static_values.add(StaticValue::Char(vm_value.bits() as u8)),
        Type::Bool => k1.static_values.add(StaticValue::Bool(vm_value.bits() == 1)),
        Type::Pointer => {
            let addr = vm_value.as_ptr();
            if addr.is_null() || addr.addr() == 0 {
                k1.static_values.add(StaticValue::Zero(POINTER_TYPE_ID))
            } else {
                return failf!(span, "Only null Pointers can be statically baked; got {:p}", addr);
            }
        }
        Type::Integer(integer_type) => {
            let int_value = vm_value.as_typed_int(*integer_type);
            k1.static_values.add(StaticValue::Int(int_value))
        }
        Type::Float(float_type) => {
            let float_value = vm_value.as_typed_float(*float_type);
            k1.static_values.add(StaticValue::Float(float_value))
        }
        Type::Array(array_type) => {
            let element_type = array_type.element_type;
            let Some(count) = array_type.concrete_count else {
                return failf!(span, "Cannot convert array of unknown size to static value");
            };
            let count = count as usize;
            let mut elements = k1.static_values.mem.new_list(count as u32);
            let Some(element_pt) = k1.types.get_physical_type(element_type) else {
                return failf!(
                    span,
                    "Element type is not physical: {}",
                    k1.type_id_to_string(element_type)
                );
            };
            for index in 0..count {
                let elem_result = get_view_element(k1, vm_value.as_ptr(), element_pt, index);
                let elem_static = vm_value_to_static_value(k1, element_type, elem_result, span)?;
                elements.push(elem_static);
            }
            let elements_slice = k1.static_values.mem.list_to_handle(&elements);
            k1.static_values.add(StaticValue::LinearContainer(StaticContainer {
                elements: elements_slice,
                kind: StaticContainerKind::Array,
                type_id,
            }))
        }
        Type::Struct(struct_type) => {
            if type_id == STRING_TYPE_ID {
                let string_id = value_to_string_id(k1, vm_value).map_err(|msg| {
                    errf!(span, "Could not convert string to static value: {msg}")
                })?;
                k1.static_values.add(StaticValue::String(string_id))
            } else if let Some((element_type, container_kind)) =
                k1.types.get_as_container_instance(type_id)
            {
                match container_kind {
                    ContainerKind::Array(_) => unreachable!(),
                    ContainerKind::List | ContainerKind::Buffer => {
                        return failf!(
                            span,
                            "{} cannot be converted to static value; convert to a View first",
                            k1.type_id_to_string(type_id)
                        );
                    }
                    ContainerKind::View => {
                        let view: k1_types::K1ViewLike = value_as_view(vm_value);
                        let element_pt = k1.types.get_physical_type(element_type).unwrap();
                        let mut elements = k1.static_values.mem.new_list(view.len as u32);
                        for index in 0..view.len {
                            let elem_vm = get_view_element(k1, view.data, element_pt, index);
                            let elem_static =
                                vm_value_to_static_value(k1, element_type, elem_vm, span)?;
                            elements.push(elem_static);
                        }
                        let elements_slice = k1.static_values.mem.list_to_handle(&elements);
                        k1.static_values.add(StaticValue::LinearContainer(StaticContainer {
                            elements: elements_slice,
                            kind: StaticContainerKind::View,
                            type_id,
                        }))
                    }
                }
            } else {
                let struct_ptr = vm_value.as_ptr();
                let mut field_value_ids = k1.static_values.mem.new_list(struct_type.fields.len());
                // let struct_agg = k1.types.phys_types.get(struct_agg_id).agg_type;
                let struct_shape = k1.types.get_struct_layout(type_id);
                for (physical_field, k1_field) in
                    struct_shape.iter().zip(k1.types.mem.getn(struct_type.fields))
                {
                    let field_ptr = unsafe { struct_ptr.byte_add(physical_field.offset as usize) };
                    let field_value = load_value(physical_field.field_t, field_ptr);
                    let field_static_value_id =
                        vm_value_to_static_value(k1, k1_field.type_id, field_value, span)?;
                    field_value_ids.push(field_static_value_id)
                }
                k1.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id,
                    fields: k1.static_values.mem.list_to_handle(&field_value_ids),
                }))
            }
        }
        Type::Enum(typed_enum) => {
            let enum_ptr = vm_value.as_ptr();
            let tag_int_type = k1.types.get(typed_enum.tag_type).expect_integer();
            let tag_scalar_type =
                k1.types.get_physical_type(typed_enum.tag_type).unwrap().expect_scalar();
            let tag = load_scalar(tag_scalar_type, enum_ptr).as_typed_int(tag_int_type);
            let variant = typed_enum.variants.iter().find(|v| v.tag_value == tag).unwrap();
            let variant_type_id = variant.my_type_id;
            let variant_index = variant.index;

            let variant_agg_id = k1.types.get_physical_type(variant_type_id).unwrap().expect_agg();
            let variant_agg =
                k1.types.phys_types.get(variant_agg_id).agg_type.expect_enum_variant();
            let payload = match variant_agg.payload {
                None => None,
                Some(payload_pt) => {
                    let payload_offset = variant_agg.payload_offset.unwrap();
                    let payload_ptr = unsafe { enum_ptr.byte_add(payload_offset as usize) };
                    let payload_value = load_value(payload_pt, payload_ptr);
                    let payload_type_id = variant.payload.unwrap();
                    let static_value_id =
                        vm_value_to_static_value(k1, payload_type_id, payload_value, span)?;
                    Some(static_value_id)
                }
            };
            k1.static_values.add(StaticValue::Enum(StaticEnum {
                variant_type_id,
                variant_index,
                typed_as_enum: true,
                payload,
            }))
        }
        Type::EnumVariant(_) => {
            return failf!(span, "Cast the variant to its either type to bake it");
        }
        Type::FunctionPointer(_) => {
            return failf!(span, "Cannot bake function pointers");
        }
        Type::Reference(_) => return failf!(span, "Cannot yet bake pointers"),
        Type::Lambda(_) | Type::LambdaObject(_) => {
            return failf!(
                span,
                "Only scalars, structs, and eithers can be statically baked. Got: {}",
                k1.type_id_to_string(type_id)
            );
        }
        Type::Function(_)
        | Type::Never
        | Type::Static(_)
        | Type::Generic(_)
        | Type::TypeParameter(_)
        | Type::FunctionTypeParameter(_)
        | Type::InferenceHole(_)
        | Type::Unresolved(_)
        | Type::RecursiveReference(_) => unreachable!(),
    };
    Ok(static_value_id)
}

pub fn value_as_view(view_value: Value) -> k1_types::K1ViewLike {
    let ptr = view_value.as_ptr();
    let buffer_ptr = ptr as *const k1_types::K1ViewLike;
    let k1_view_like = unsafe { buffer_ptr.read() };
    k1_view_like
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn get_view_element(
    k1: &TypedProgram,
    data_ptr: *const u8,
    elem_pt: PhysicalType,
    index: usize,
) -> Value {
    let elem_offset = k1.types.get_pt_layout(&elem_pt).offset_at_index(index);
    let elem_ptr = unsafe { data_ptr.byte_add(elem_offset) };
    load_value(elem_pt, elem_ptr)
}

fn render_debug_value(
    w: &mut impl std::fmt::Write,
    vm: &Vm,
    k1: &TypedProgram,
    pt: PhysicalType,
    value: Value,
) -> std::fmt::Result {
    match pt {
        PhysicalType::Scalar(scalar_type) => match scalar_type {
            ScalarType::U8 => write!(w, "u8 {}", value.bits() as u8)?,
            ScalarType::U16 => write!(w, "u16 {}", value.bits() as u16)?,
            ScalarType::U32 => write!(w, "u32 {}", value.bits() as u32)?,
            ScalarType::U64 => write!(w, "u64 {}", value.bits())?,
            ScalarType::I8 => write!(w, "i8 {}", value.bits() as i8)?,
            ScalarType::I16 => write!(w, "i16 {}", value.bits() as i16)?,
            ScalarType::I32 => write!(w, "i32 {}", value.bits() as i32)?,
            ScalarType::I64 => write!(w, "i64 {}", value.bits() as i64)?,
            ScalarType::F32 => write!(w, "f32 {}", value.as_f32())?,
            ScalarType::F64 => write!(w, "f64 {}", value.as_f64())?,
            ScalarType::Pointer => render_debug_address(w, vm, value)?,
        },
        PhysicalType::Agg(_) => {
            write!(w, "agg ")?;
            render_debug_address(w, vm, value)?;
            w.write_str(" ")?;
            types::display_pt(w, &k1.types, &pt)?;
            let layout = k1.types.get_pt_layout(&pt);
            if value.as_usize() == 0 {
                write!(w, "<null>")?;
            } else if value.as_usize() <= MIN_VALID_PTR_HEUR {
                write!(w, "<invalid ptr: {}>", value.bits())?;
            } else {
                let data_bytes =
                    unsafe { std::slice::from_raw_parts(value.as_ptr(), layout.size as usize) };
                writeln!(w, " data")?;
                write_bytes(w, data_bytes)?;
            };
        }
    };
    Ok(())
}

#[allow(unused)]
fn debug_value_to_string(vm: &Vm, k1: &TypedProgram, pt: PhysicalType, value: Value) -> String {
    let mut s = String::new();
    render_debug_value(&mut s, vm, k1, pt, value).unwrap();
    s
}

fn render_debug_address(w: &mut impl std::fmt::Write, vm: &Vm, value: Value) -> std::fmt::Result {
    let p = value.as_usize();
    if let Some(frame_index) = vm.stack.owns_address(p) {
        write!(w, "ptr {:p} (in stack frame [{}])", value.as_ptr(), frame_index)
    } else {
        write!(w, "ptr {:p} (heap/unknown)", value.as_ptr_unchecked())
    }
}

fn static_zero_value(k1: &mut TypedProgram, type_id: TypeId, span: SpanId) -> Value {
    match k1.types.get_physical_type(type_id) {
        None => ice_span!(
            k1,
            span,
            "not a value type; zeroed() for type {} is undefined",
            k1.types.get(type_id).kind_name()
        ),
        Some(PhysicalType::Scalar(_)) => Value(0),
        Some(PhysicalType::Agg(agg_id)) => {
            let layout = k1.types.phys_types.get(agg_id).layout;
            let data: *mut u8 = k1.vm_static_stack.push_layout_uninit(layout);
            unsafe { std::ptr::write_bytes(data, 0, layout.size as usize) };

            Value::ptr(data.cast_const())
        }
    }
}

unsafe fn slice_from_raw_parts_checked<'a, T>(
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
                let frame_names = make_stack_trace(k1, &vm.stack);
                eprintln!("STACK TRACE\n{}", frame_names);
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

pub fn make_stack_trace(k1: &TypedProgram, stack: &Stack) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    for f in stack.frames.iter() {
        write!(&mut s, "[{:02}] ", f.index).unwrap();
        bc::display_unit_name(&mut s, k1, f.unit).unwrap();
        match f.call_span {
            None => {}
            Some(span) => {
                let (source, line) = k1.get_span_location(span);
                write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap()
            }
        };
        writeln!(&mut s).unwrap();
    }
    s
}
fn report_execution_messages(k1: &mut TypedProgram, vm: &Vm, span: SpanId, _exit_code: i32) {
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
    k1.report_ext(TyperError { message: formatted_messages, span, level }, true);
}

#[track_caller]
fn vm_crash(m: &TypedProgram, vm: &Vm, msg: impl AsRef<str>) -> ! {
    vm.dump_current_frame(m);
    eprintln!("{}", make_stack_trace(m, &vm.stack));
    m.ice_with_span(msg, vm.eval_span)
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
