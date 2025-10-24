// Copyright (c) 2025 knix
// All rights reserved.

use std::{
    num::NonZeroU32,
    sync::atomic::{AtomicU64, Ordering},
};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use itertools::Itertools;
use log::debug;
use memmap2::{MmapMut, MmapOptions};

#[cfg(test)]
mod vm_test;

use crate::{
    bc::{self, BcCallee, CompilableUnit, CompiledUnit, InstId, InstKind, Value as BcValue},
    compiler::WordSize,
    errf, failf, ice_span,
    lex::SpanId,
    parse::{Ident, StringId},
    typer::{
        FunctionId, Layout, StaticContainer, StaticContainerKind, StaticEnum, StaticStruct,
        StaticValue, StaticValueId, TypedExprId, TypedFloatValue, TypedGlobalId, TypedIntValue,
        TypedProgram, TyperResult, UNIT_BYTE_VALUE, VariableId,
        types::{
            self, AggType, ContainerKind, FloatType, IntegerType, POINTER_TYPE_ID, PhysicalType,
            STRING_TYPE_ID, ScalarType, Type, TypeId, TypePool,
        },
    },
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
pub mod k1_types {

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

pub struct Vm {
    globals: FxHashMap<TypedGlobalId, Value>,
    pub static_stack: Stack,
    pub stack: Stack,
    eval_depth: AtomicU64,
    eval_span: SpanId,
}

impl Vm {
    pub fn reset(&mut self) {
        // Note that we don't de-allocate any resources
        // we just zero the memory and reset the stack pointer
        self.static_stack.reset();
        self.stack.reset();
        self.globals.clear();

        self.eval_depth.store(0, Ordering::Relaxed);
        self.eval_span = SpanId::NONE;
    }

    pub fn make(stack_size_bytes: usize, static_size_bytes: usize) -> Self {
        let stack = Stack::make(stack_size_bytes);
        let static_stack = Stack::make(static_size_bytes);
        Self {
            // nocommit(4): Pass in upper bound on globals from the AST
            globals: FxHashMap::with_capacity(8192),
            static_stack,
            stack,
            eval_depth: AtomicU64::new(0),
            eval_span: SpanId::NONE,
        }
    }

    pub fn dump(&mut self, k1: &TypedProgram) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for index in 0..self.stack.frames.len() {
            let frame_string = self.dump_frame(k1, index as u32);
            write!(w, "FRAME {index:02} {}", frame_string).unwrap();
        }
        s
    }

    pub fn get_destination_stack(&mut self, dst_stack: StackSelection) -> &mut Stack {
        match dst_stack {
            StackSelection::StaticSpace => &mut self.static_stack,
            StackSelection::CallStackCurrent => &mut self.stack,
        }
    }

    pub fn dump_current_frame(&self, k1: &TypedProgram) -> String {
        self.dump_frame(k1, self.stack.current_frame_index())
    }

    pub fn dump_frame(&self, k1: &TypedProgram, frame_index: u32) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        let frame = self.stack.frames[frame_index as usize];
        writeln!(w, "Frame [{frame_index}] {}", k1.ident_str_opt(frame.debug_name)).unwrap();
        writeln!(w, "Base :  {:?}", frame.base_ptr).unwrap();
        writeln!(w, "Locals").unwrap();
        let locals =
            self.stack.inst_values[frame_index as usize].iter().copied().enumerate().collect_vec();
        for (inst_index, value) in locals.into_iter() {
            let Some(inst_id) = InstId::from_u32(frame.unit.inst_offset + inst_index as u32) else {
                break;
            };
            if k1.bytecode.instrs.get_opt(inst_id).is_none() {
                break;
            };
            let kind = bc::get_inst_kind(&k1.bytecode, &k1.types, inst_id);
            match kind {
                InstKind::Value(_) => {}
                InstKind::Void => {
                    continue;
                }
                InstKind::Terminator => {
                    continue;
                }
            };
            write!(w, "  i{}: ", inst_id).unwrap();
            bc::display_inst_kind(w, &k1.types, &kind).unwrap();
            write!(w, " ").unwrap();
            render_debug_value(w, self, k1, value);
            writeln!(w).unwrap()
        }

        //write!(w, "\nDATA\n").unwrap();
        //let frame = &self.call_stack[frame_index];
        //frame.to_bytes().chunks(8).for_each(|bytes| {
        //    for b in bytes {
        //        write!(w, "{:02x} ", b).unwrap();
        //    }
        //    writeln!(w).unwrap();
        //});
        s
    }
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
            TypedIntValue::U32(v) | TypedIntValue::UWord32(v) => Value::u32(v),
            TypedIntValue::U64(v) | TypedIntValue::UWord64(v) => Value::u64(v),
            TypedIntValue::I8(v) => Value::i8(v),
            TypedIntValue::I16(v) => Value::i16(v),
            TypedIntValue::I32(v) | TypedIntValue::IWord32(v) => Value::i32(v),
            TypedIntValue::I64(v) | TypedIntValue::IWord64(v) => Value::i64(v),
        }
    }

    pub fn as_typed_int(&self, int_type: IntegerType) -> TypedIntValue {
        let u64 = self.bits();
        match int_type {
            IntegerType::U8 => TypedIntValue::U8(u64 as u8),
            IntegerType::U16 => TypedIntValue::U16(u64 as u16),
            IntegerType::U32 => TypedIntValue::U32(u64 as u32),
            IntegerType::U64 => TypedIntValue::U64(u64),
            IntegerType::UWord(WordSize::W32) => TypedIntValue::U32(u64 as u32),
            IntegerType::UWord(WordSize::W64) => TypedIntValue::U64(u64),
            IntegerType::I8 => TypedIntValue::I8(u64 as i8),
            IntegerType::I16 => TypedIntValue::I16(u64 as i16),
            IntegerType::I32 => TypedIntValue::I32(u64 as i32),
            IntegerType::I64 => TypedIntValue::I64(u64 as i64),
            IntegerType::IWord(WordSize::W32) => TypedIntValue::I32(u64 as i32),
            IntegerType::IWord(WordSize::W64) => TypedIntValue::I64(u64 as i64),
        }
    }

    #[track_caller]
    fn as_ptr(&self) -> *const u8 {
        const MIN_VALID_PTR: u64 = 0x10000; // 64 KiB
        debug_assert!(
            self.0 == 0 || self.0 >= MIN_VALID_PTR,
            "Attempted to extract pointer from non-pointer Value: {}",
            self.0
        );
        debug_assert!(
            (self.0 as *const u8).is_aligned(),
            "Attempted to extract unaligned pointer from Value: {:0x}",
            self.0
        );
        self.0 as *const u8
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
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

const GLOBAL_ID_STATIC: TypedGlobalId = TypedGlobalId::from_nzu32(NonZeroU32::new(1).unwrap());

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
    let span = k1.exprs.get(expr_id).get_span();
    let unit = *k1.bytecode.exprs.get(&expr_id).unwrap();

    eprintln!(
        "[vmbc] Executing {}\n{}",
        k1.expr_to_string(expr_id),
        bc::compiled_unit_to_string(k1, &unit, true)
    );

    match unit.unit {
        CompilableUnit::Function(_function_id) => {
            return failf!(span, "Cannot execute function from top");
        }
        CompilableUnit::Expr(_) => {
            // Executing an expr. Execution will just end at some point, and
            // we'll just decode the last value we got I suppose
        }
    };
    let top_frame = vm.stack.push_new_frame(None, Some(span), unit, None);
    let top_frame_index = top_frame.index;

    vm.globals.insert(GLOBAL_ID_STATIC, Value::TRUE);

    for (param_index, arg) in args.iter().enumerate() {
        let value = static_value_to_vm_value(k1, *arg, span);
        vm.stack.param_values[top_frame_index as usize][param_index] = value;
    }

    let (exit_code, exit_value) = exec_loop(k1, vm, unit)?;

    match exit_value {
        None => {
            if exit_code != 0 {
                failf!(span, "Static Execution Failed with code: {}", exit_code)
            } else {
                Ok(k1.static_values.unit_id())
            }
        }
        Some(expr_value) => {
            let expr_type_id = k1.get_expr_type_id(expr_id);
            let baked_value = vm_value_to_static_value(k1, vm, expr_type_id, expr_value, span)?;
            Ok(baked_value)
        }
    }
}

fn exec_loop(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    mut unit: CompiledUnit,
) -> TyperResult<(i32, Option<Value>)> {
    let mut prev_b: u32 = 0;
    let mut b: u32 = 0;
    let mut ip: u32 = 0;
    let mut exit_value: Option<Value> = None;
    let mut span;
    macro_rules! resolve_value {
        ($v:expr) => {
            resolve_value(k1, vm, unit.inst_offset, $v)
        };
    }

    let exit_code = 'exec: loop {
        // Fetch
        let instrs = k1.bytecode.mem.get_nth(unit.blocks, b as usize).instrs;
        let inst_id = *k1.bytecode.mem.get_nth(instrs, ip as usize);
        span = *k1.bytecode.sources.get(inst_id);
        let inst_index = inst_to_index(inst_id, unit.inst_offset);
        eprintln!(
            "{}EXEC: {}",
            "  ".repeat(vm.stack.current_frame_index() as usize),
            bc::inst_to_string(k1, &k1.bytecode, inst_id)
        );

        macro_rules! builtin_return {
            ($value:expr) => {{
                vm.stack.set_cur_inst_value(inst_index, $value);
                ip += 1;
                continue 'exec;
            }};
        }

        // ~Decode~ Execute
        match *k1.bytecode.instrs.get(inst_id) {
            bc::Inst::Imm(imm) => {
                let value = match imm {
                    bc::Imm::I64(v) => Value(v),
                    bc::Imm::Float(fv) => match fv {
                        TypedFloatValue::F32(f32) => Value::f32(f32),
                        TypedFloatValue::F64(f64) => Value::f64(f64),
                    },
                };

                vm.stack.set_cur_inst_value(inst_index, value);
                ip += 1
            }
            bc::Inst::Alloca { t, vm_layout } => {
                let ptr = vm.stack.push_layout_uninit(vm_layout);

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(ptr));
                ip += 1
            }
            bc::Inst::Store { dst, value } => {
                let dst = resolve_value!(dst);

                let t = bc::get_value_kind(&k1.bytecode, &k1.types, &value)
                    .expect_value()
                    .unwrap()
                    .as_scalar()
                    .unwrap();
                let vm_value = resolve_value!(value);
                store_scalar(t, dst.as_ptr().cast_mut(), vm_value);

                // Store Produces no value, no need to set
                ip += 1;
            }
            bc::Inst::Load { t, src } => {
                let src_value = resolve_value!(src);
                let src_ptr = src_value.as_ptr();
                let loaded_value = load_scalar(t, src_ptr);

                vm.stack.set_cur_inst_value(inst_index, loaded_value);
                ip += 1;
            }
            bc::Inst::Copy { dst, src, vm_size, .. } => {
                let dst_value = resolve_value!(dst);
                let src_value = resolve_value!(src);
                let dst_ptr = dst_value.as_ptr();
                let src_ptr = src_value.as_ptr();

                memcopy(src_ptr, dst_ptr.cast_mut(), vm_size as usize);
                ip += 1
            }
            bc::Inst::StructOffset { base, vm_offset, .. } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_ptr();
                let field_ptr = unsafe { base_ptr.byte_add(vm_offset as usize) };

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(field_ptr));
                ip += 1
            }
            bc::Inst::ArrayOffset { element_t, base, element_index } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_ptr();
                let index_value = resolve_value!(element_index);
                let index = index_value.bits();
                let elem_layout = k1.types.get_pt_layout(&element_t);

                let element_ptr =
                    unsafe { base_ptr.byte_add(elem_layout.size as usize * index as usize) };

                vm.stack.set_cur_inst_value(inst_index, Value::ptr(element_ptr));
                ip += 1
            }
            bc::Inst::Call { id } => {
                let call = *k1.bytecode.calls.get(id);
                let (return_type, return_place) = match &call.ret_inst_kind {
                    InstKind::Value(ret_type) => {
                        let ret_place = match call.dst {
                            Some(bc_dst) => {
                                RetPlace::Addr { addr: resolve_value!(bc_dst).as_ptr().cast_mut() }
                            }
                            None => RetPlace::ScalarCallInst { inst_index },
                        };
                        (Some(*ret_type), ret_place)
                    }
                    InstKind::Void => (None, RetPlace::ScalarCallInst { inst_index }),

                    InstKind::Terminator => (None, RetPlace::ScalarCallInst { inst_index }),
                };
                let ret_info = match return_type {
                    None => None,
                    Some(t) => Some(RetInfo { t, place: return_place, ip: ip + 1, block: b }),
                };
                let dispatch_function_id = match call.callee {
                    BcCallee::Builtin(bc_builtin) => match bc_builtin {
                        bc::BcBuiltin::TypeSchema => todo!(),
                        bc::BcBuiltin::TypeName => todo!(),
                        bc::BcBuiltin::Allocate | bc::BcBuiltin::AllocateZeroed => {
                            // intern fn allocZeroed(size: uword, align: uword): Pointer
                            let zero = bc_builtin == bc::BcBuiltin::AllocateZeroed;
                            let size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let align: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1));
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
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let old_size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1));
                            let align: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2));
                            let new_size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 3));
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
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0)).as_ptr();
                            let size =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1)).as_usize();
                            let align =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2)).as_usize();

                            let layout =
                                std::alloc::Layout::from_size_align(size as usize, align as usize)
                                    .unwrap();
                            unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };

                            builtin_return!(Value::ptr(ptr))
                        }
                        bc::BcBuiltin::MemCopy => {
                            // intern fn copy( dst: Pointer, src: Pointer, count: uword): unit
                            let dst: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let src: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1));
                            let count: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2));
                            memcopy(src.as_ptr(), dst.as_ptr().cast_mut(), count.as_usize());

                            builtin_return!(Value::UNIT)
                        }
                        bc::BcBuiltin::MemSet => {
                            //intern fn set(dst: Pointer, value: u8, count: uword): unit
                            let dst: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let value: u8 =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1)).bits() as u8;
                            let count: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2));
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
                            let p1: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let p2: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1));
                            let size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2));

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
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0)).bits();
                            if exit_code == 0 {
                                if let Some(expr_ret) = unit.expr_ret {
                                    exit_value = Some(resolve_value!(expr_ret));
                                }
                            }
                            break exit_code as i32;
                        }
                        bc::BcBuiltin::BakeStaticValue => vm_ice!(k1, vm, "bakestatic"),
                        bc::BcBuiltin::CompilerMessage => vm_ice!(k1, vm, "compilermessage"),
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
                let fn_name = k1.functions.get(dispatch_function_id).name;
                let inst_span = *k1.bytecode.sources.get(inst_id);
                let Some(compiled_function) = k1.bytecode.functions.get(dispatch_function_id)
                else {
                    return failf!(
                        span,
                        "Call to uncompiled function: {}. ({} are pending)",
                        k1.function_id_to_string(dispatch_function_id, false),
                        k1.bytecode.b_units_pending_compile.len()
                    );
                };
                let compiled_function = *compiled_function;
                let frame_index = vm.stack.current_frame_index();
                let new_frame_index = frame_index + 1;
                eprintln!(
                    "{}dispatching call [{}] to {}",
                    "  ".repeat(vm.stack.current_frame_index() as usize),
                    new_frame_index,
                    bc::compiled_unit_to_string(k1, &compiled_function, true)
                );

                // Prepare the function's arguments
                for (index, arg) in k1.bytecode.mem.get_slice(call.args).iter().enumerate() {
                    // Note: These need to execute before we push, in case they access this stack's
                    // params, or instrs by index
                    let vm_value = resolve_value!(*arg);
                    // task(vmbc): Slight optimization to avoid extra accessing the param vec for this frame repeatedly
                    //eprintln!("dispatching frame{} p{} := {}", new_frame_index, index, vm_value);
                    vm.stack.set_param_value(new_frame_index, index as u32, vm_value);
                }

                // 'Dispatch' to the function:
                // - Push a stack frame
                // - Set 'pc' (which is blocks + b + i)
                let _frame = vm.stack.push_new_frame(
                    Some(fn_name),
                    Some(inst_span),
                    compiled_function,
                    ret_info,
                );

                unit = compiled_function;
                b = 0;
                ip = 0;
            }
            bc::Inst::Ret(bc_value) => {
                let cur_frame = vm.stack.current_frame_index();
                let cur_frame = &vm.stack.frames[cur_frame as usize];
                let Some(ret_info) = cur_frame.ret_info else {
                    return failf!(span, "Return from 'never' function");
                };
                // task(bc): Avoid this for scalar returns; the call inst
                //           should just _be_ the value for those
                let return_slot = ret_info.place;

                let returned_value = resolve_value!(bc_value);

                match return_slot {
                    RetPlace::ScalarCallInst { inst_index } => {
                        // This is an instruction index from the caller's stack
                        let caller_frame_index = vm.stack.caller_frame_index();
                        vm.stack.set_inst_value(caller_frame_index, inst_index, returned_value);
                    }
                    RetPlace::Addr { addr } => {
                        store_value(&k1.types, ret_info.t, addr, returned_value);
                    }
                }

                let _popped = vm.stack.pop_frame();

                unit = vm.stack.current_frame().unit;
                b = ret_info.block;
                ip = ret_info.ip;
            }
            bc::Inst::Jump(block_index) => {
                prev_b = b;
                b = block_index;
                ip = 0;
            }
            bc::Inst::JumpIf { cond, cons, alt } => {
                let cond_value = resolve_value!(cond);
                let cond_u64 = cond_value.bits() as u8;
                let as_u8 = cond_u64 as u8;
                if as_u8 == 1 {
                    prev_b = b;
                    b = cons;
                    ip = 0;
                } else if as_u8 == 0 {
                    prev_b = b;
                    b = alt;
                    ip = 0;
                } else {
                    return failf!(span, "Bad boolean value: {cond_u64}");
                }
            }
            bc::Inst::Unreachable => {
                return failf!(span, "Reached unreachable instruction");
            }
            bc::Inst::CameFrom { t: _, incomings } => {
                debug_assert!(incomings.len() > 0);
                let mut value: core::mem::MaybeUninit<BcValue> = core::mem::MaybeUninit::uninit();
                'case: for case in k1.bytecode.mem.get_slice(incomings) {
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
            bc::Inst::BoolNegate { v } => {
                let b = resolve_value!(v).as_bool();
                vm.stack.set_cur_inst_value(inst_index, Value::bool(!b));
                ip += 1
            }
            bc::Inst::BitNot { v } => {
                // We invert all bits, even the ones not in play for the width;
                // but that is fine since we're operating on a Vm Value not a
                // memory address where we're worried about corrupting data
                let input = resolve_value!(v);
                let result = !input.bits();

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::IntTrunc { v, to } => {
                let input = resolve_value!(v);

                // For now, truncate is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            bc::Inst::IntExtU { v, to } => {
                let input = resolve_value!(v);

                // For now, extend is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            bc::Inst::IntExtS { v, to } => {
                let input = resolve_value!(v);

                // For now, extend is a no-op
                let result = input;

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            bc::Inst::FloatTrunc { v, to } => {
                let from: f64 = resolve_value!(v).as_f64();
                let result = from as f32;

                vm.stack.set_cur_inst_value(inst_index, Value::f32(result));
                ip += 1
            }
            bc::Inst::FloatExt { v, to } => {
                let from: f32 = resolve_value!(v).as_f32();
                let result = from as f64;

                vm.stack.set_cur_inst_value(inst_index, Value::f64(result));
                ip += 1
            }
            bc::Inst::Float32ToIntUnsigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f32();

                let result = match to {
                    ScalarType::I8 => Value::u8(f as u8),
                    ScalarType::I16 => Value::u16(f as u16),
                    ScalarType::I32 => Value::u32(f as u32),
                    ScalarType::I64 => Value::u64(f as u64),
                    ScalarType::F32 => unreachable!(),
                    ScalarType::F64 => unreachable!(),
                    ScalarType::Pointer => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            bc::Inst::Float32ToIntSigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f32();

                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    ScalarType::F32 => unreachable!(),
                    ScalarType::F64 => unreachable!(),
                    ScalarType::Pointer => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            bc::Inst::Float64ToIntUnsigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f64();

                let result = match to {
                    ScalarType::I8 => Value::u8(f as u8),
                    ScalarType::I16 => Value::u16(f as u16),
                    ScalarType::I32 => Value::u32(f as u32),
                    ScalarType::I64 => Value::u64(f as u64),
                    ScalarType::F32 => unreachable!(),
                    ScalarType::F64 => unreachable!(),
                    ScalarType::Pointer => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            bc::Inst::Float64ToIntSigned { v, to } => {
                // This changes bits; not a no-op
                let f = resolve_value!(v).as_f64();

                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    ScalarType::F32 => unreachable!(),
                    ScalarType::F64 => unreachable!(),
                    ScalarType::Pointer => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1;
            }
            bc::Inst::IntToFloatUnsigned { v, from, to } => {
                let int_value = resolve_value!(v);
                let result = match (from, to) {
                    (ScalarType::I8, ScalarType::F32) => {
                        let i = int_value.bits() as u8;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I16, ScalarType::F32) => {
                        let i = int_value.bits() as u16;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I32, ScalarType::F32) => {
                        let i = int_value.bits() as u32;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I64, ScalarType::F32) => {
                        let i = int_value.bits() as u64;
                        Value::f32(i as f32)
                    }
                    (ScalarType::I8, ScalarType::F64) => {
                        let i = int_value.bits() as u8;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I16, ScalarType::F64) => {
                        let i = int_value.bits() as u16;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I32, ScalarType::F64) => {
                        let i = int_value.bits() as u32;
                        Value::f64(i as f64)
                    }
                    (ScalarType::I64, ScalarType::F64) => {
                        let i = int_value.bits() as u64;
                        Value::f64(i as f64)
                    }
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, result);
                ip += 1
            }
            bc::Inst::IntToFloatSigned { v, from, to } => {
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
            bc::Inst::PtrToWord { v } => {
                let v = resolve_value!(v);

                vm.stack.set_cur_inst_value(inst_index, v);
                ip += 1
            }
            bc::Inst::WordToPtr { v } => {
                let v = resolve_value!(v);

                vm.stack.set_cur_inst_value(inst_index, v);
                ip += 1
            }
            bc::Inst::BitwiseBin { op, lhs, rhs } => todo!(),
            bc::Inst::IntAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                let result = casted_uop!(width, wrapping_mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntDivUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_uop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntDivSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_iop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntRemUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_uop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntRemSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_iop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result as u64));
                ip += 1
            }
            bc::Inst::IntCmp { lhs, rhs, pred, width } => {
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
            bc::Inst::FloatAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Add;
                let result = casted_float_op!(width, add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::FloatSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Sub;
                let result = casted_float_op!(width, sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::FloatMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Mul;
                let result = casted_float_op!(width, mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::FloatDiv { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Div;
                let result = casted_float_op!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::FloatRem { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).bits();
                let rhs = resolve_value!(rhs).bits();
                use std::ops::Rem;
                let result = casted_float_op!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value(result));
                ip += 1
            }
            bc::Inst::FloatCmp { lhs, rhs, pred, width } => {
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
        }
    };
    Ok((exit_code, exit_value))
}

#[inline(always)]
fn resolve_value(k1: &mut TypedProgram, vm: &mut Vm, inst_offset: u32, value: BcValue) -> Value {
    match value {
        BcValue::Inst(inst_id) => {
            let inst_index = inst_to_index(inst_id, inst_offset);
            let v = vm
                .stack
                .get_cur_inst_value(inst_index)
                .unwrap_or_else(|| vm_ice!(k1, vm, "missing inst: {}", inst_id));
            v
        }
        BcValue::Global { t, id } => {
            // Handle global
            // Case 1: It's a constant, already evaluated, stored in the global static space
            // Case 2: It's in this VM because its mutable, and we already evaluated it during this execution
            // Case 3: First evaluation. If not mutable, put in globals. If mutable, put in this
            // vm's static space.
            // ** If referencing, allocate layout and perform a store to produce a valid address
            match k1.vmbc_global_constant_lookups.get(&id) {
                Some(v) => *v,
                None => {
                    match vm.globals.get(&id) {
                        Some(v) => *v,
                        None => {
                            let global = k1.globals.get(id);
                            let is_constant = global.is_constant;
                            let initial_value_id = match global.initial_value {
                                None => {
                                    debug!("Gotta run global");
                                    k1.eval_global_body(global.ast_id, None, None).unwrap();
                                    let value_id = k1.globals.get(id).initial_value.unwrap();
                                    debug!("RAN GLOBAL: {}", k1.static_value_to_string(value_id));
                                    value_id
                                }
                                Some(value_id) => value_id,
                            };
                            debug!(
                                "shared global is: {}. the `t` of the instr is: {}",
                                k1.static_value_to_string(initial_value_id),
                                types::pt_to_string(&k1.types, &t)
                            );
                            let shared_vm_value =
                                static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
                            let layout = k1.types.get_pt_layout(&t);
                            //Mistake here is that the bytecode says globals are always addresses
                            //We are here currently saying its only an address if its referencing.
                            //We just need to push the layout uninit regardless, and store regardless
                            if is_constant {
                                let dst = k1.vmbc_static_stack.push_layout_uninit(layout);
                                store_value(&k1.types, t, dst, shared_vm_value);
                                let addr = Value::ptr(dst.cast_const());
                                k1.vmbc_global_constant_lookups.insert(id, addr);
                                addr
                            } else {
                                // We need a local copy of this
                                let dst = vm.static_stack.push_layout_uninit(layout);
                                store_value(&k1.types, t, dst, shared_vm_value);
                                let addr = Value::ptr(dst.cast_const());
                                vm.globals.insert(id, addr);
                                addr
                            }
                        }
                    }
                }
            }
        }
        BcValue::StaticValue { t, id } => {
            let v = static_value_to_vm_value(k1, id, vm.eval_span);
            v
        }
        BcValue::FunctionAddr(function_id) => function_id_to_ref_value(function_id),
        BcValue::FnParam { t, index } => {
            let current_params = &vm.stack.param_values[vm.stack.current_frame_index() as usize];
            let value = current_params[index as usize];
            // eprintln!("Accessed frame{} p{index}: {}", vm.stack.current_frame_index(), value);
            value
        }
        BcValue::Imm32 { t, data } => {
            let u32_data = data;
            match t {
                ScalarType::I8 => Value(u32_data as u64),
                ScalarType::I16 => Value(u32_data as u64),
                ScalarType::I32 => Value(u32_data as u64),
                ScalarType::I64 => unreachable!(),
                ScalarType::F32 => Value(u32_data as u64),
                ScalarType::F64 => unreachable!(),
                ScalarType::Pointer => unreachable!(),
            }
        }
        BcValue::PtrZero => Value::NULLPTR,
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
    if let Some(v) = k1.vmbc_static_value_lookups.get(&static_value_id) {
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
            let struct_base = k1.vmbc_static_stack.push_layout_uninit(layout);

            store_static_value(k1, struct_base, static_value_id);

            Value::ptr(struct_base.cast_const())
        }
        StaticValue::Enum(e) => {
            let layout = k1.types.get_layout(e.variant_type_id);
            let enum_base = k1.vmbc_static_stack.push_layout_uninit(layout);

            store_static_value(k1, enum_base, static_value_id);

            Value::ptr(enum_base.cast_const())
        }
        StaticValue::LinearContainer(cont) => {
            todo!()
            // let (element_type, _) = k1.types.get_as_container_instance(cont.type_id).unwrap();
            //
            // let layout = k1.types.get_layout(element_type);
            // let view_allocation_layout = layout.array_me(cont.len());
            //
            // debug!(
            //     "Pushing {} bytes for container {}",
            //     view_allocation_layout.size,
            //     k1.static_value_to_string(static_value_id)
            // );
            // let data_base_mem = vm.static_stack.push_layout_uninit(view_allocation_layout);
            //
            // for (index, elem_value_id) in
            //     k1.static_values.get_slice(cont.elements).iter().enumerate()
            // {
            //     let elem_value = static_value_to_vm_value(vm, dst_stack, k1, *elem_value_id)?;
            //     let elem_dst_ptr = unsafe { data_base_mem.byte_add(index * layout.size as usize) };
            //     store_value(&k1.types, elem_dst_ptr, elem_value);
            // }
            // match cont.kind {
            //     StaticContainerKind::View => {
            //         let rust_view = K1ViewLike {
            //             len: cont.elements.len() as usize,
            //             data: data_base_mem.cast_const(),
            //         };
            //         let view_struct_ptr = vm.get_destination_stack(dst_stack).push_t(rust_view);
            //
            //         Ok(Value::Agg { type_id: cont.type_id, ptr: view_struct_ptr })
            //     }
            //     StaticContainerKind::Array => {
            //         Ok(Value::Agg { type_id: cont.type_id, ptr: data_base_mem })
            //     }
            // }
        }
    };
    k1.vmbc_static_value_lookups.insert(static_value_id, v);
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
                struct_layout.iter().zip(k1.static_values.mem.get_slice(static_struct.fields))
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
            todo!()
            // let (element_type, _) = k1.types.get_as_container_instance(cont.type_id).unwrap();
            //
            // let layout = k1.types.get_layout(element_type);
            // let view_allocation_layout = layout.array_me(cont.len());
            //
            // debug!(
            //     "Pushing {} bytes for container {}",
            //     view_allocation_layout.size,
            //     k1.static_value_to_string(static_value_id)
            // );
            // let data_base_mem = vm.static_stack.push_layout_uninit(view_allocation_layout);
            //
            // for (index, elem_value_id) in
            //     k1.static_values.get_slice(cont.elements).iter().enumerate()
            // {
            //     let elem_value = static_value_to_vm_value(vm, dst_stack, k1, *elem_value_id)?;
            //     let elem_dst_ptr = unsafe { data_base_mem.byte_add(index * layout.size as usize) };
            //     store_value(&k1.types, elem_dst_ptr, elem_value);
            // }
            // match cont.kind {
            //     StaticContainerKind::View => {
            //         let rust_view = K1ViewLike {
            //             len: cont.elements.len() as usize,
            //             data: data_base_mem.cast_const(),
            //         };
            //         let view_struct_ptr = vm.get_destination_stack(dst_stack).push_t(rust_view);
            //
            //         Ok(Value::Agg { type_id: cont.type_id, ptr: view_struct_ptr })
            //     }
            //     StaticContainerKind::Array => {
            //         Ok(Value::Agg { type_id: cont.type_id, ptr: data_base_mem })
            //     }
            // }
        }
    };
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

    let string_stack_addr = k1.vmbc_static_stack.push_t(k1_string);
    Value::ptr(string_stack_addr)
}

// fn execute_intrinsic(
//     vm: &mut Vm,
//     k1: &mut TypedProgram,
//     type_args: SliceHandle<NameAndTypeId>,
//     args: &[TypedExprId],
//     return_type: TypeId,
//     intrinsic_type: IntrinsicOperation,
// ) -> TyperResult<VmResult> {
//     match intrinsic_type {
//         IntrinsicOperation::SizeOf
//         | IntrinsicOperation::SizeOfStride
//         | IntrinsicOperation::AlignOf
//         | IntrinsicOperation::GetStaticValue
//         | IntrinsicOperation::StaticTypeToValue => {
//             unreachable!("Handled by typer phase")
//         }
//         IntrinsicOperation::Zeroed => {
//             let type_id = k1.named_types.get_nth(type_args, 0).type_id;
//             let value = zero_value(vm, k1, type_id)?;
//             Ok(VmResult::Value(value))
//         }
//         IntrinsicOperation::TypeId => {
//             unreachable!("Handled by typer phase")
//         }
//         IntrinsicOperation::TypeName => {
//             let TypedIntValue::U64(type_id_value) =
//                 execute_expr_return_exit!(vm, k1, args[0])?.expect_int()
//             else {
//                 k1.ice_with_span("Malformed TypeName call", vm.eval_span)
//             };
//             let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_value as u32).unwrap());
//             let string_id = *k1.type_names.get(&type_id).unwrap();
//             let value = string_id_to_value(vm, StackSelection::CallStackCurrent, k1, string_id);
//             Ok(VmResult::Value(value))
//         }
//         IntrinsicOperation::TypeSchema => {
//             let TypedIntValue::U64(type_id_value) =
//                 execute_expr_return_exit!(vm, k1, args[0])?.expect_int()
//             else {
//                 k1.ice_with_span("Malformed TypeSchema call", vm.eval_span)
//             };
//             let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_value as u32).unwrap());
//             let Some(schema_static_value_id) = k1.type_schemas.get(&type_id) else {
//                 vm_ice!(k1, vm, "Missing type schema: {}", k1.type_id_to_string(type_id))
//             };
//             let value = static_value_to_vm_value(
//                 vm,
//                 StackSelection::CallStackCurrent,
//                 k1,
//                 *schema_static_value_id,
//             )?;
//             Ok(VmResult::Value(value))
//         }
//         IntrinsicOperation::CompilerSourceLocation => {
//             unreachable!("Operation CompilerSourceLocation is handled in typer")
//         }
//         IntrinsicOperation::BoolNegate => {
//             let b = execute_expr_return_exit!(vm, k1, args[0])?.expect_bool();
//             Ok(Value::Bool(!b).into())
//         }
//         IntrinsicOperation::BitNot => {
//             let int = execute_expr_return_exit!(vm, k1, args[0])?.expect_int();
//             Ok(Value::Int(int.bit_not()).into())
//         }
//         IntrinsicOperation::ArithBinop(kind) => {
//             use IntrinsicArithOpOp as Op;
//             match kind {
//                 IntrinsicArithOpKind { op: Op::Equals, .. } => {
//                     let lhs = execute_expr_return_exit!(vm, k1, args[0])?;
//                     let rhs = execute_expr_return_exit!(vm, k1, args[1])?;
//                     let bool_value = match (lhs, rhs) {
//                         (Value::Unit, Value::Unit) => Ok(true),
//                         (Value::Char(c1), Value::Char(c2)) => Ok(c1 == c2),
//                         (Value::Bool(b1), Value::Bool(b2)) => Ok(b1 == b2),
//                         (Value::Float(f1), Value::Float(f2)) => Ok(f1 == f2),
//                         (Value::Int(i1), Value::Int(i2)) => Ok(i1 == i2),
//                         (lhs, rhs) => {
//                             failf!(
//                                 vm.eval_span,
//                                 "Unexpected kinds for native equals: {} and {}",
//                                 lhs.kind_name(),
//                                 rhs.kind_name()
//                             )
//                         }
//                     }?;
//                     Ok(Value::Bool(bool_value).into())
//                 }
//                 k @ IntrinsicArithOpKind {
//                     op: Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem,
//                     ..
//                 } => {
//                     let lhs = execute_expr_return_exit!(vm, k1, args[0])?;
//                     let rhs = execute_expr_return_exit!(vm, k1, args[1])?;
//                     Ok(binop::execute_arith_op(lhs, rhs, k.op).into())
//                 }
//                 k @ IntrinsicArithOpKind { op: Op::Lt | Op::Le | Op::Gt | Op::Ge, .. } => {
//                     let lhs = execute_expr_return_exit!(vm, k1, args[0])?;
//                     let rhs = execute_expr_return_exit!(vm, k1, args[1])?;
//                     Ok(binop::execute_cmp_op(lhs, rhs, k.op).into())
//                 }
//             }
//         }
//         IntrinsicOperation::BitwiseBinop(kind) => {
//             let inta = execute_expr_return_exit!(vm, k1, args[0])?.expect_int();
//             let intb = execute_expr_return_exit!(vm, k1, args[1])?.expect_int();
//             use std::ops::{Shl, Shr};
//             let int_value = match kind {
//                 IntrinsicBitwiseBinopKind::And => inta.bit_and(&intb),
//                 IntrinsicBitwiseBinopKind::Or => inta.bit_or(&intb),
//                 IntrinsicBitwiseBinopKind::Xor => inta.bit_xor(&intb),
//                 IntrinsicBitwiseBinopKind::ShiftLeft => {
//                     int_shift!(inta, intb.to_u64_unconditional(), shl)
//                 }
//                 // Doesn't matter which, since Rust always does logical shifts on signed values,
//                 // and we're using Rust integers in TypedIntValue
//                 IntrinsicBitwiseBinopKind::SignedShiftRight => {
//                     int_shift!(inta, intb.to_u64_unconditional(), shr)
//                 }
//                 IntrinsicBitwiseBinopKind::UnsignedShiftRight => {
//                     int_shift!(inta, intb.to_u64_unconditional(), shr)
//                 }
//             };
//             Ok(Value::Int(int_value).into())
//         }
//         IntrinsicOperation::PointerIndex => {
//             // intern fn refAtIndex[T](self: Pointer, index: uword): T*
//             let typ = k1.named_types.get_nth(type_args, 0).type_id;
//             let ptr = execute_expr_return_exit!(vm, k1, args[0])?.expect_ptr();
//             let index = execute_expr_return_exit!(vm, k1, args[1])?.expect_int().expect_uword();
//             let result = ptr + offset_at_index(&k1.types, typ, index as usize);
//             Ok(Value::Reference { type_id: return_type, ptr: result as *const u8 }.into())
//         }
//         IntrinsicOperation::Allocate | IntrinsicOperation::AllocateZeroed => {
//             let zero = intrinsic_type == IntrinsicOperation::AllocateZeroed;
//             let size_expr = args[0];
//             let align_expr = args[1];
//             let size = execute_expr_return_exit!(vm, k1, size_expr)?.expect_int().expect_uword();
//             let align = execute_expr_return_exit!(vm, k1, align_expr)?.expect_int().expect_uword();
//             let Ok(layout) = std::alloc::Layout::from_size_align(size, align) else {
//                 vm_crash(
//                     k1,
//                     vm,
//                     format!("Rust didn't like this layout: size={size}, align={align}"),
//                 )
//             };
//             let ptr = allocate(layout, zero);
//
//             Ok(Value::Pointer(ptr.addr()).into())
//         }
//         IntrinsicOperation::Reallocate => {
//             let old_ptr_expr = args[0];
//             let old_size_expr = args[1];
//             let old_align_expr = args[2];
//             let new_size_expr = args[3];
//             let old_ptr = execute_expr_return_exit!(vm, k1, old_ptr_expr)?.expect_ptr();
//             let old_size =
//                 execute_expr_return_exit!(vm, k1, old_size_expr)?.expect_int().expect_uword();
//             let align =
//                 execute_expr_return_exit!(vm, k1, old_align_expr)?.expect_int().expect_uword();
//             let new_size =
//                 execute_expr_return_exit!(vm, k1, new_size_expr)?.expect_int().expect_uword();
//             let layout = std::alloc::Layout::from_size_align(old_size, align).unwrap();
//             let ptr = unsafe { std::alloc::realloc(old_ptr as *mut u8, layout, new_size) };
//             Ok(Value::Pointer(ptr.addr()).into())
//         }
//         IntrinsicOperation::Free => {
//             let ptr_expr = args[0];
//             let size_expr = args[1];
//             let align_expr = args[2];
//             let ptr = execute_expr_return_exit!(vm, k1, ptr_expr)?.expect_ptr();
//             let size = execute_expr_return_exit!(vm, k1, size_expr)?.expect_int().expect_u64();
//             let align = execute_expr_return_exit!(vm, k1, align_expr)?.expect_int().expect_u64();
//
//             let layout =
//                 std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();
//             unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };
//             Ok(VmResult::UNIT)
//         }
//         IntrinsicOperation::MemCopy => {
//             let [dst, src, count] = args[0..3] else { unreachable!() };
//             let dst = execute_expr_return_exit!(vm, k1, dst)?.expect_ptr();
//             let src = execute_expr_return_exit!(vm, k1, src)?.expect_ptr();
//             let count = execute_expr_return_exit!(vm, k1, count)?.expect_int().expect_uword();
//             unsafe {
//                 std::ptr::copy_nonoverlapping(src as *const u8, dst as *mut u8, count as usize)
//             };
//             Ok(VmResult::UNIT)
//         }
//         IntrinsicOperation::MemSet => k1.ice("memset not implemented", None),
//         IntrinsicOperation::MemEquals => {
//             //intern fn compare(p1: Pointer, p2: Pointer, size: uword): i32
//             let [p1, p2, size] = args[0..3] else { unreachable!() };
//             let p1_usize = execute_expr_return_exit!(vm, k1, p1)?.expect_ptr();
//             let p2_usize = execute_expr_return_exit!(vm, k1, p2)?.expect_ptr();
//             let size = execute_expr_return_exit!(vm, k1, size)?.expect_int().expect_uword();
//             let p1 = p1_usize as *const u8;
//             let p2 = p2_usize as *const u8;
//             let p1 = unsafe { slice_from_raw_parts_checked(vm, k1, p1, size as usize) };
//             let p2 = unsafe { slice_from_raw_parts_checked(vm, k1, p2, size as usize) };
//
//             // Slice equality on [u8] results in memcmp
//             let eq = p1 == p2;
//
//             Ok(Value::Bool(eq).into())
//         }
//         IntrinsicOperation::Exit => {
//             let TypedIntValue::I32(code) = execute_expr_return_exit!(vm, k1, args[0])?.expect_int()
//             else {
//                 unreachable!("malformed exit (code type)")
//             };
//             Ok(VmResult::Exit(VmExit { span: vm.eval_span, code }))
//         }
//         IntrinsicOperation::CompilerMessage => {
//             let location_arg = execute_expr_return_exit!(vm, k1, args[0])?;
//             let level_arg = execute_expr_return_exit!(vm, k1, args[1])?.expect_agg();
//             let message_arg = execute_expr_return_exit!(vm, k1, args[2])?;
//             let location = unsafe { (location_arg.expect_agg() as *const K1SourceLocation).read() };
//             let level = unsafe { (level_arg as *const CompilerMessageLevel).read() };
//             let color = match level {
//                 CompilerMessageLevel::Info => colored::Color::White,
//                 CompilerMessageLevel::Warn => colored::Color::Yellow,
//                 CompilerMessageLevel::Error => colored::Color::Red,
//             };
//             let level_str = match level {
//                 CompilerMessageLevel::Info => "info",
//                 CompilerMessageLevel::Warn => "warn",
//                 CompilerMessageLevel::Error => "error",
//             };
//             let message = value_to_string_id(k1, message_arg).map_err(|msg| {
//                 errf!(vm.eval_span, "Bad message string passed to EmitCompilerMessage: {msg}")
//             })?;
//             let filename = unsafe { location.filename.to_str() }.map_err(|msg| {
//                 errf!(vm.eval_span, "Bad filename string passed to EmitCompilerMessage: {msg}")
//             })?;
//             eprintln!(
//                 "[{}:{} {}] {}",
//                 filename,
//                 location.line,
//                 level_str,
//                 k1.get_string(message).color(color)
//             );
//             Ok(VmResult::UNIT)
//         }
//         IntrinsicOperation::BakeStaticValue => {
//             // intern fn bakeStaticValue[T](value: T): u64
//             let value_value = execute_expr_return_exit!(vm, k1, args[0])?;
//             let value_id = vm_value_to_static_value(k1, vm, value_value, vm.eval_span)?;
//             Ok(VmResult::Value(Value::Int(TypedIntValue::U64(value_id.as_u32() as u64))))
//         }
//     }
// }

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
    unsafe {
        match t {
            ScalarType::I8 => dst.write(value.bits() as u8),
            ScalarType::I16 => (dst as *mut u16).write(value.bits() as u16),
            ScalarType::I32 => (dst as *mut u32).write(value.bits() as u32),
            ScalarType::I64 => (dst as *mut u64).write(value.bits()),
            ScalarType::F32 => (dst as *mut u32).write(value.bits() as u32),
            ScalarType::F64 => (dst as *mut u64).write(value.bits()),
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
            memcopy(src, dst, record.layout.size as usize)
        }
    }
}

fn memcopy(src: *const u8, dst: *mut u8, size_bytes: usize) {
    // eprintln!("copy from {:?} -> {:?} {size_bytes}", src, dst);
    unsafe {
        core::ptr::copy_nonoverlapping(src, dst, size_bytes);
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_scalar(t: ScalarType, ptr: *const u8) -> Value {
    unsafe {
        match t {
            ScalarType::I8 => Value::u8(ptr.read()),
            ScalarType::I16 => Value::u16((ptr as *const u16).read()),
            ScalarType::I32 => Value::u32((ptr as *const u32).read()),
            ScalarType::I64 => Value::u64((ptr as *const u64).read()),
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

fn aligned_to(ptr: *const u8, align_bytes: usize) -> *const u8 {
    let bytes_needed = ptr.align_offset(align_bytes);
    unsafe { ptr.byte_add(bytes_needed) }
}

pub struct Stack {
    allocation: MmapMut,
    base_ptr: *const u8,
    frames: Vec<StackFrame>,
    // Indexed by stack frame index, then inst id
    inst_values: Vec<Vec<Value>>,
    // Indexed by stack frame index, then function parameter index
    param_values: Vec<Vec<Value>>,
    cursor: *const u8,
}

#[derive(Clone, Copy)]
enum RetPlace {
    ScalarCallInst { inst_index: u32 },
    Addr { addr: *mut u8 },
}

#[derive(Clone, Copy)]
pub struct RetInfo {
    t: PhysicalType,
    // Where the return value does. Either a register or an address
    place: RetPlace,
    ip: u32,
    block: u32,
}

#[derive(Clone, Copy)]
pub struct StackFrame {
    #[allow(unused)]
    index: u32,
    base_ptr: *const u8,
    debug_name: Option<Ident>,
    call_span: Option<SpanId>,
    unit: CompiledUnit,
    ret_info: Option<RetInfo>,
}

impl StackFrame {
    pub fn make(
        index: u32,
        base_ptr: *const u8,
        debug_name: Option<Ident>,
        call_span: Option<SpanId>,
        owner: CompiledUnit,
        ret_info: Option<RetInfo>,
    ) -> StackFrame {
        StackFrame { index, base_ptr, debug_name, call_span, unit: owner, ret_info }
    }
}

impl Stack {
    pub fn make(size: usize) -> Stack {
        debug!("make stack {size}");
        let mut mmap_options = MmapOptions::new();
        let mmap_mut = mmap_options.len(size).map_anon().unwrap();
        let base_ptr = (*mmap_mut).as_ptr();
        let frames_cap = if size == 0 { 0 } else { 512 };
        let values_cap = 1024;
        let params_cap = 256;
        let mut inst_values = Vec::with_capacity(frames_cap);

        // nocommit(4): The static stacks don't need this stuff
        let empty_values = vec![Value(0); values_cap];
        for _ in 0..frames_cap {
            inst_values.push(empty_values.clone());
        }

        let empty_params = vec![Value(0); params_cap];
        let mut param_values = Vec::with_capacity(frames_cap);
        for _ in 0..frames_cap {
            param_values.push(empty_params.clone())
        }

        Self {
            // Calls libc::munmap(ptr, len as libc::size_t) on Drop
            allocation: mmap_mut,
            base_ptr,
            frames: Vec::with_capacity(frames_cap),
            inst_values,
            param_values,
            cursor: base_ptr,
        }
    }

    pub fn reset(&mut self) {
        let len_to_clear = self.cursor.addr() - self.base_ptr.addr();
        if len_to_clear == 0 {
            return;
        }
        self.cursor = self.base_ptr();
        unsafe {
            core::ptr::write_bytes(self.allocation.as_mut_ptr(), 0, len_to_clear);
        }
        self.frames.clear();
    }

    fn push_new_frame(
        &mut self,
        name: Option<Ident>,
        call_span: Option<SpanId>,
        owner: CompiledUnit,
        ret_info: Option<RetInfo>,
    ) -> StackFrame {
        let index = self.frames.len() as u32;
        let base_ptr = self.cursor;
        let frame = StackFrame::make(index, base_ptr, name, call_span, owner, ret_info);
        self.push_frame(frame)
    }

    fn push_frame(&mut self, frame: StackFrame) -> StackFrame {
        self.frames.push(frame);
        *self.frames.last().unwrap()
    }

    fn pop_frame(&mut self) -> StackFrame {
        let f = self.frames.pop().unwrap();
        self.cursor = f.base_ptr;
        f
    }

    fn current_frame_index(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    fn caller_frame_index(&self) -> u32 {
        self.frames.len() as u32 - 2
    }

    fn caller_frame(&self) -> &StackFrame {
        self.frames.get(self.caller_frame_index() as usize).unwrap()
    }

    fn current_frame(&self) -> &StackFrame {
        self.frames.last().unwrap()
    }

    fn param_values_for_frame(&mut self, frame_index: u32) -> &mut Vec<Value> {
        &mut self.param_values[frame_index as usize]
    }

    fn set_param_value(&mut self, frame_index: u32, param_index: u32, value: Value) {
        let vs = self.param_values_for_frame(frame_index);
        vs[param_index as usize] = value;
    }

    fn get_param_value(&mut self, frame_index: u32, param_index: u32) -> Value {
        let vs = self.param_values_for_frame(frame_index);
        vs[param_index as usize]
    }

    pub fn set_inst_value(&mut self, frame_index: u32, inst_index: u32, value: Value) {
        // match self.inst_values.get_mut(frame_index as usize) {
        //     None => self.inst_values.push(vec![Value(0); 64]),
        //     Some(_) => (),
        // };
        let frame_values = &mut self.inst_values[frame_index as usize];
        match frame_values.get_mut(inst_index as usize) {
            None => frame_values.push(value),
            Some(r) => *r = value,
        }
    }

    pub fn set_cur_inst_value(&mut self, inst_index: u32, value: Value) {
        self.set_inst_value(self.current_frame_index(), inst_index, value)
    }

    pub fn get_cur_inst_value(&mut self, inst_index: u32) -> Option<Value> {
        self.get_frame_inst_value(self.current_frame_index(), inst_index)
    }

    pub fn get_frame_inst_value(&self, frame_index: u32, inst_index: u32) -> Option<Value> {
        self.inst_values[frame_index as usize].get(inst_index as usize).copied()
    }

    #[inline]
    pub fn base_ptr(&self) -> *const u8 {
        self.base_ptr
    }

    #[inline]
    pub fn base_addr(&self) -> usize {
        self.base_ptr().addr()
    }

    #[inline]
    pub fn end_ptr(&self) -> *const u8 {
        self.allocation.as_ptr_range().end
    }

    #[inline]
    pub fn current_offset_bytes(&self) -> usize {
        self.cursor.addr() - self.base_addr()
    }

    #[inline]
    fn cursor_mut(&self) -> *mut u8 {
        self.cursor as *mut u8
    }

    #[inline]
    fn check_bounds(&self, ptr: *const u8) {
        if self.allocation.is_empty() {
            panic!("check_bounds on a stack with no allocation")
        }
        let ptr_addr = ptr.addr();
        let base_addr = self.base_addr();
        let end_addr = self.end_ptr().addr();
        let oob = ptr_addr < base_addr || ptr_addr >= end_addr;
        if oob {
            panic!(
                "Out of bounds access to stack frame: {} < {} < {}",
                base_addr, ptr_addr, end_addr
            );
        }
    }

    pub fn push_slice(&mut self, slice: &[u8]) -> *const u8 {
        let slice_stack_ptr = self.cursor_mut();
        let slice_len = slice.len();
        if cfg!(debug_assertions) {
            self.check_bounds(unsafe { slice_stack_ptr.byte_add(slice_len) })
        }

        unsafe {
            // Copy bytes using memcpy:
            let dst_slice = std::slice::from_raw_parts_mut(slice_stack_ptr, slice_len);
            // eprintln!("copy_from_slice {:?} {:?}", slice.as_ptr(), slice_stack_ptr);
            dst_slice.copy_from_slice(slice);
        }
        // Would be nice to re-use the 'end' of the slice ptr
        self.cursor = unsafe { self.cursor.byte_add(slice_len) };
        slice_stack_ptr.cast_const()
    }

    fn align_to_bytes(&mut self, align_bytes: usize) {
        self.cursor = aligned_to(self.cursor, align_bytes)
    }

    // pub fn push_struct_values(
    //     &mut self,
    //     types: &TypePool,
    //     struct_type_id: TypeId,
    //     members: &[Value],
    // ) -> Value {
    //     let struct_layout = types.get_layout(struct_type_id);
    //     let struct_base = self.push_layout_uninit(struct_layout);
    //
    //     let struct_layout = &types.get_struct_layout(struct_type_id);
    //
    //     for (value, field_offset) in members.iter().zip(struct_layout.field_offsets.iter()) {
    //         // Go to offset
    //         let field_dst = unsafe { dst.byte_add(*field_offset as usize) };
    //         store_value(types, field_dst, *value);
    //     }
    //
    //     self.advance_cursor(struct_layout.size as usize);
    //     Value::Agg { type_id: struct_type_id, ptr: struct_base }
    // }

    // pub fn push_enum(
    //     &mut self,
    //     types: &TypePool,
    //     variant_type_id: TypeId,
    //     payload_value: Option<Value>,
    // ) -> *const u8 {
    //     let dst = self.cursor_mut();
    //     let (enum_base, layout) =
    //         build_enum_at_location(dst, types, variant_type_id, payload_value);
    //     self.advance_cursor(layout.size as usize);
    //     enum_base
    // }

    // pub fn push_value(&mut self, types: &TypePool, value: Value) -> *const u8 {
    //     let layout = types.get_layout(value.get_type());
    //     self.align_to_bytes(layout.align as usize);
    //     self.push_value_no_align(types, value)
    // }

    // We may not want to align, in case we're pushing this value as part of a packed struct.
    // Or if our caller already aligned us
    // pub fn push_value_no_align(&mut self, types: &TypePool, value: Value) -> *const u8 {
    //     let dst: *mut u8 = self.cursor_mut();
    //
    //     let written: usize = store_value(types, dst, value);
    //     self.advance_cursor(written);
    //     dst.cast_const()
    // }

    pub fn advance_cursor(&mut self, count: usize) {
        self.cursor = unsafe { self.cursor.byte_add(count) };
    }

    pub fn push_t<T: Copy>(&mut self, value: T) -> *const u8 {
        let c = self.push_layout_uninit(Layout::from_rust_type::<T>());
        unsafe {
            (c as *mut T).write(value);
        }
        c
    }

    pub fn push_layout_uninit(&mut self, layout: Layout) -> *mut u8 {
        if cfg!(debug_assertions) {
            self.check_bounds(unsafe { self.cursor.byte_add(layout.size as usize) })
        }
        self.align_to_bytes(layout.align as usize);
        let c = self.cursor_mut();
        self.advance_cursor(layout.size as usize);
        c
    }

    #[inline]
    pub fn push_usize(&mut self, value: usize) -> *const u8 {
        self.push_t(value)
    }

    #[inline]
    pub fn push_ptr_uninit(&mut self) -> *mut u8 {
        self.push_usize(0).cast_mut()
    }

    /// Push a raw copy of `size_bytes` from `src_ptr` into the frame buffer.
    /// The copy is aligned to `align_bytes`.
    #[inline]
    fn push_raw_copy(
        &mut self,
        size_bytes: usize,
        align_bytes: usize,
        src_ptr: *const u8,
    ) -> *const u8 {
        self.align_to_bytes(align_bytes);
        unsafe {
            debug!("push_raw_copy {:?} {}", src_ptr, size_bytes);
            let src_slice = std::slice::from_raw_parts(src_ptr, size_bytes);
            self.push_slice(src_slice)
        }
    }

    fn push_raw_copy_layout(&mut self, layout: Layout, src_ptr: *const u8) -> *const u8 {
        self.push_raw_copy(layout.size as usize, layout.align as usize, src_ptr)
    }

    pub fn to_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.base_ptr(), self.current_offset_bytes()) }
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
    vm: &mut Vm,
    type_id: TypeId,
    vm_value: Value,
    span: SpanId,
) -> TyperResult<StaticValueId> {
    debug!("vm_to_static: {:?}", vm_value);
    let Some(pt) = k1.types.get_physical_type(type_id) else {
        return failf!(span, "Not a phyical type: {}", k1.type_id_to_string(type_id));
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
                return failf!(span, "Only null Pointers can be statically baked");
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
            todo!();
            //let element_type = array_type.element_type;
            //let Some(count) = array_type.concrete_count else {
            //    return failf!(span, "Cannot convert array of unknown size to static value");
            //};
            //let count = count as usize;
            //let mut elements = k1.static_values.mem.new_vec(count as u32);
            //for index in 0..count {
            //    let elem_result = get_view_element(vm, k1, ptr, element_type, index);
            //    let elem_vm = elem_result.unwrap_or_else(|e| {
            //        vm_ice!(k1, vm, "Failed to load view element {index}: {}", e)
            //    });
            //    let elem_static = vm_value_to_static_value(k1, vm, elem_vm, span)?;
            //    elements.push(elem_static);
            //}
            //let elements_slice = k1.static_values.mem.vec_to_mslice(&elements);
            //StaticValue::LinearContainer(StaticContainer {
            //    elements: elements_slice,
            //    kind: StaticContainerKind::Array,
            //    type_id,
            //})
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
                        let mut elements = k1.static_values.mem.new_vec(view.len as u32);
                        for index in 0..view.len {
                            let elem_vm = get_view_element(k1, view.data, element_pt, index);
                            let elem_static =
                                vm_value_to_static_value(k1, vm, element_type, elem_vm, span)?;
                            elements.push(elem_static);
                        }
                        let elements_slice = k1.static_values.mem.vec_to_mslice(&elements);
                        k1.static_values.add(StaticValue::LinearContainer(StaticContainer {
                            elements: elements_slice,
                            kind: StaticContainerKind::View,
                            type_id,
                        }))
                    }
                }
            } else {
                let struct_ptr = vm_value.as_ptr();
                let mut field_value_ids = k1.static_values.mem.new_vec(struct_type.fields.len());
                // let struct_agg = k1.types.phys_types.get(struct_agg_id).agg_type;
                let struct_shape = k1.types.get_struct_layout(type_id);
                for (physical_field, k1_field) in
                    struct_shape.iter().zip(k1.types.mem.get_slice(struct_type.fields))
                {
                    let field_ptr = unsafe { struct_ptr.byte_add(physical_field.offset as usize) };
                    let field_value = load_value(physical_field.field_t, field_ptr);
                    let field_static_value_id =
                        vm_value_to_static_value(k1, vm, k1_field.type_id, field_value, span)?;
                    field_value_ids.push(field_static_value_id)
                }
                k1.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id,
                    fields: k1.static_values.mem.vec_to_mslice(&field_value_ids),
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
                        vm_value_to_static_value(k1, vm, payload_type_id, payload_value, span)?;
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

fn render_debug_value(w: &mut impl std::fmt::Write, _vm: &Vm, _k1: &TypedProgram, value: Value) {
    //task(vmbc): Pass type info into here somehow
    write!(w, "{} 0x{:x}", value.0, value.0).unwrap()
}

fn debug_value_to_string(vm: &mut Vm, k1: &TypedProgram, value: Value) -> String {
    let mut w = String::new();
    render_debug_value(&mut w, vm, k1, value);
    w
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
            let data: *mut u8 = k1.vmbc_static_stack.push_layout_uninit(layout);
            unsafe { std::ptr::write_bytes(data, 0, layout.size as usize) };

            Value::ptr(data.cast_const())
        }
    }
}

fn integer_cast(
    types: &TypePool,
    int_to_cast: TypedIntValue,
    target_type_id: TypeId,
) -> TypedIntValue {
    match types.get(target_type_id).expect_integer() {
        IntegerType::U8 => int_to_cast.to_u8().into(),
        IntegerType::U16 => int_to_cast.to_u16().into(),
        IntegerType::U32 => int_to_cast.to_u32().into(),
        IntegerType::U64 => int_to_cast.to_u64_bits().into(),
        IntegerType::I8 => (int_to_cast.to_u8() as i8).into(),
        IntegerType::I16 => (int_to_cast.to_u16() as i16).into(),
        IntegerType::I32 => (int_to_cast.to_u32() as i32).into(),
        IntegerType::I64 => (int_to_cast.to_u64_bits() as i64).into(),
        IntegerType::UWord(WordSize::W32) => unreachable!(),
        IntegerType::UWord(WordSize::W64) => TypedIntValue::UWord64(int_to_cast.to_u64_bits()),
        IntegerType::IWord(WordSize::W32) => unreachable!(),
        IntegerType::IWord(WordSize::W64) => {
            TypedIntValue::IWord64(int_to_cast.to_u64_bits() as i64)
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
    stack.frames.iter().for_each(|f| {
        write!(&mut s, "[{:02}] {:32}", f.index, k1.ident_str_opt(f.debug_name)).unwrap();
        match f.call_span {
            None => {}
            Some(span) => {
                let (source, line) = k1.get_span_location(span);
                write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap()
            }
        };
        writeln!(&mut s).unwrap();
    });
    s
}

#[track_caller]
fn vm_crash(m: &TypedProgram, vm: &Vm, msg: impl AsRef<str>) -> ! {
    eprintln!("{}", vm.dump_current_frame(m));
    eprintln!("{}", make_stack_trace(m, &vm.stack));
    m.ice_with_span(msg, vm.eval_span)
}

//mod c_mem {
//    use std::os::raw::{c_size_t, c_void};
//    extern "C" {
//        fn malloc(size: c_size_t) -> *mut c_void;
//        fn realloc(ptr: *mut c_void, size: c_size_t) -> *mut c_void;
//        fn free(ptr: *mut c_void);
//    }
//}
