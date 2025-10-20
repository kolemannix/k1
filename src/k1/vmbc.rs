// Copyright (c) 2025 knia
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

use crate::{
    bc::{self, BcCallee, CompilableUnit, CompiledUnit, InstId, InstKind, Value as BcValue},
    compiler::WordSize,
    failf, ice_span,
    lex::SpanId,
    parse::{Ident, StringId},
    typer::{
        FunctionId, Layout, StaticValue, StaticValueId, TypedExprId, TypedFloatValue,
        TypedGlobalId, TypedIntValue, TypedProgram, TyperResult, VariableId,
        types::{IntegerType, PhysicalType, STRING_TYPE_ID, ScalarType, TypeId, TypePool},
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
            let inst_id = InstId::from_u32(frame.unit.inst_offset + inst_index as u32).unwrap();
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
pub struct VmExit {
    #[allow(unused)]
    span: SpanId,
    code: i32,
}

#[derive(Debug, Clone, Copy)]
pub enum VmResult {
    Value(Value),
    Break(Value),
    Return(Value),
    Exit(VmExit),
}

impl VmResult {
    pub fn expect_value(&self) -> Value {
        match self {
            VmResult::Value(v) => *v,
            VmResult::Break(_) => unreachable!("expect_value on break"),
            VmResult::Return(_) => unreachable!("expect_value on return"),
            VmResult::Exit(_) => unreachable!("expect_value on exit"),
        }
    }

    fn is_terminating(&self) -> bool {
        match self {
            VmResult::Value(_) => false,
            VmResult::Break(_) => true,
            VmResult::Return(_) => true,
            VmResult::Exit(_) => true,
        }
    }
}

impl From<Value> for VmResult {
    fn from(v: Value) -> Self {
        VmResult::Value(v)
    }
}

impl From<&Value> for VmResult {
    fn from(v: &Value) -> Self {
        VmResult::Value(*v)
    }
}

#[derive(Debug, Clone, Copy)]
//task(vmbc): Consider just a union here to not waste double the space
pub enum Value {
    // 'Immediate' values; pure storage to be interpreted
    D64(u64),
    Addr { ptr: *const u8 },
}

impl Value {
    pub const TRUE: Value = Self::bool(true);

    pub const fn bool(b: bool) -> Value {
        if b { Value::D64(1) } else { Value::D64(0) }
    }

    #[track_caller]
    fn expect_data(&self) -> u64 {
        match self {
            Value::D64(v) => *v,
            Value::Addr { .. } => panic!("expect_data on Addr"),
        }
    }
    #[track_caller]
    fn expect_addr(&self) -> *const u8 {
        match self {
            Value::D64(_) => panic!("expect_addr on D64"),
            Value::Addr { ptr } => *ptr,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::D64(v) => write!(f, "B64({})", v),
            Value::Addr { ptr } => write!(f, "Addr({:?})", ptr),
        }
    }
}

impl From<*const u8> for Value {
    fn from(ptr: *const u8) -> Self {
        Value::Addr { ptr }
    }
}

impl Value {
    pub fn as_addr(&self) -> Option<*const u8> {
        match self {
            Value::Addr { ptr } => Some(*ptr),
            _ => None,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            Value::D64(_) => "b64",
            Value::Addr { .. } => "addr",
        }
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
) -> TyperResult<Value> {
    eprintln!("execute_compiled_unit");
    let mut span = k1.exprs.get(expr_id).get_span();
    let mut unit = *k1.bytecode.exprs.get(&expr_id).unwrap();
    let dst_stack = StackSelection::CallStackCurrent;

    eprintln!(
        "[vmbc] Executing {}\n{}",
        k1.expr_to_string(expr_id),
        bc::compiled_unit_to_string(k1, &unit, true)
    );

    let mut prev_b: u32 = 0;
    let mut b: u32 = 0;
    let mut ip: u32 = 0;

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

    macro_rules! resolve_value {
        ($v:expr) => {
            resolve_value(k1, vm, unit.inst_offset, $v)
        };
    }
    'exec: loop {
        // Fetch
        let instrs = k1.bytecode.mem.get_nth(unit.blocks, b as usize).instrs;
        if instrs.len() == 0 {
            ice_span!(k1, span, "empty block")
        }
        let inst_id = *k1.bytecode.mem.get_nth(instrs, ip as usize);
        span = *k1.bytecode.sources.get(inst_id);
        let inst_index = inst_to_index(inst_id, unit.inst_offset);
        eprintln!("{}", bc::inst_to_string(k1, &k1.bytecode, inst_id, true));

        // ~Decode~ Execute
        match *k1.bytecode.instrs.get(inst_id) {
            bc::Inst::Imm(imm) => {
                let value = match imm {
                    bc::Imm::I64(v) => Value::D64(v),
                    bc::Imm::Float(fv) => match fv {
                        TypedFloatValue::F32(f32) => {
                            // The lower 32 bits can be relied upon to reconstruct the f32.
                            // NOTE!!!: That we do not go through f64!
                            Value::D64(f32.to_bits() as u64)
                        }
                        TypedFloatValue::F64(f64) => Value::D64(f64.to_bits()),
                    },
                };

                vm.stack.set_cur_inst_value(inst_index, value);
                ip += 1
            }
            bc::Inst::Alloca { t, vm_layout } => {
                let s = vm.get_destination_stack(dst_stack);
                let ptr = s.push_layout_uninit(vm_layout);

                vm.stack.set_cur_inst_value(inst_index, Value::Addr { ptr });
                ip += 1
            }
            bc::Inst::Store { dst, value } => {
                let dst = resolve_value!(dst);
                let Some(dst_addr) = dst.as_addr() else {
                    return failf!(span, "store dst is not an addr");
                };

                let t = bc::get_value_kind(&k1.bytecode, &k1.types, &value)
                    .expect_value()
                    .unwrap()
                    .as_scalar()
                    .unwrap();
                let vm_value = resolve_value!(value);
                store_scalar(t, dst_addr.cast_mut(), vm_value);

                // Store Produces no value, no need to set
                ip += 1;
            }
            bc::Inst::Load { t, src } => {
                let src_value = resolve_value!(src);
                let src_ptr = src_value
                    .as_addr()
                    .unwrap_or_else(|| vm_ice!(k1, vm, "Load from non-addr: {}", src_value));
                let loaded_value = load_scalar(t, src_ptr);

                vm.stack.set_cur_inst_value(inst_index, loaded_value);
                ip += 1;
            }
            bc::Inst::Copy { dst, src, vm_size, .. } => {
                let dst_value = resolve_value!(dst);
                let src_value = resolve_value!(src);
                let dst_ptr = dst_value
                    .as_addr()
                    .unwrap_or_else(|| vm_ice!(k1, vm, "Copy to non-addr: {}", dst_value));
                let src_ptr = src_value
                    .as_addr()
                    .unwrap_or_else(|| vm_ice!(k1, vm, "Copy from non-addr: {}", src_value));

                unsafe {
                    std::ptr::copy_nonoverlapping(src_ptr, dst_ptr.cast_mut(), vm_size as usize);
                }
                ip += 1
            }
            bc::Inst::StructOffset { base, vm_offset, .. } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_addr().unwrap_or_else(|| {
                    vm_ice!(k1, vm, "StructOffset from non-addr: {}", base_value)
                });
                let field_ptr = unsafe { base_ptr.byte_add(vm_offset as usize) };

                vm.stack.set_cur_inst_value(inst_index, Value::Addr { ptr: field_ptr });
                ip += 1
            }
            bc::Inst::ArrayOffset { element_t, base, element_index } => {
                let base_value = resolve_value!(base);
                let base_ptr = base_value.as_addr().unwrap_or_else(|| {
                    vm_ice!(k1, vm, "ArrayOffset from non-addr: {}", base_value)
                });
                let index_value = resolve_value!(element_index);
                let Value::D64(index) = index_value else {
                    return failf!(span, "ArrayOffset index is not B64: {}", index_value);
                };
                let elem_layout = k1.types.get_pt_layout(&element_t);

                let element_ptr =
                    unsafe { base_ptr.byte_add(elem_layout.size as usize * index as usize) };

                vm.stack.set_cur_inst_value(inst_index, Value::Addr { ptr: element_ptr });
                ip += 1
            }
            bc::Inst::Call { id } => {
                let call = *k1.bytecode.calls.get(id);
                let (return_type, return_place) = match &call.ret_inst_kind {
                    InstKind::Value(ret_type) => {
                        let ret_place = match call.dst {
                            Some(bc_dst) => RetPlace::Addr {
                                addr: resolve_value!(bc_dst).expect_addr().cast_mut(),
                            },
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
                        bc::BcBuiltin::Allocate => todo!(),
                        bc::BcBuiltin::AllocateZeroed => todo!(),
                        bc::BcBuiltin::Reallocate => todo!(),
                        bc::BcBuiltin::Free => todo!(),
                        bc::BcBuiltin::MemCopy => todo!(),
                        bc::BcBuiltin::MemSet => todo!(),
                        bc::BcBuiltin::MemEquals => {
                            //intern fn equals(p1: Pointer, p2: Pointer, size: uword): bool
                            let p1: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 0));
                            let p2: Value = resolve_value!(*k1.bytecode.mem.get_nth(call.args, 1));
                            let size: Value =
                                resolve_value!(*k1.bytecode.mem.get_nth(call.args, 2));

                            let p1_ptr = p1.expect_addr();
                            let p2_ptr = p2.expect_addr();
                            let size_usize = size.expect_data() as usize;

                            let p1_slice =
                                unsafe { slice_from_raw_parts_checked(vm, k1, p1_ptr, size_usize) };
                            let p2_slice =
                                unsafe { slice_from_raw_parts_checked(vm, k1, p2_ptr, size_usize) };

                            let eq = p1_slice == p2_slice;
                            let value = Value::bool(eq);

                            vm.stack.set_cur_inst_value(inst_index, value);
                            ip += 1;
                            continue 'exec;
                        }
                        bc::BcBuiltin::Exit => vm_ice!(k1, vm, "exit"),
                        bc::BcBuiltin::BakeStaticValue => vm_ice!(k1, vm, "bakestatic"),
                        bc::BcBuiltin::CompilerMessage => vm_ice!(k1, vm, "compilermessage"),
                    },
                    BcCallee::Direct(function_id) => function_id,
                    BcCallee::Indirect(value) => {
                        // Decode function id from 'pointer'
                        let callee_value = resolve_value!(value);
                        let Value::D64(function_id_u64) = callee_value else {
                            return failf!(
                                span,
                                "Indirect call callee is not B64: {}",
                                callee_value
                            );
                        };
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
                    "  dispatching call [{}] to {}",
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

                //task(vmbc): Consider just grabbing these from the frame; or storing the whole
                //            'unit'; just generally make it more elegant
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
                let Value::D64(cond_u64) = cond_value else {
                    return failf!(span, "JumpIf cond is not B64: {}", cond_value);
                };
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
                for case in k1.bytecode.mem.get_slice(incomings) {
                    if case.from == prev_b {
                        value = core::mem::MaybeUninit::new(case.value);
                        break;
                    }
                }
                let value = unsafe { value.assume_init() };
                let vm_value = resolve_value!(value);

                vm.stack.set_cur_inst_value(inst_index, vm_value);
                ip += 1;
            }
            bc::Inst::BoolNegate { v } => {
                let b = resolve_value!(v).expect_data();
                let result = if b == 0 { 1 } else { 0 };

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::BitNot { v } => {
                let b = resolve_value!(v).expect_data();
                let result = !b;

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::IntTrunc { v, to } => todo!(),
            bc::Inst::IntExtU { v, to } => todo!(),
            bc::Inst::IntExtS { v, to } => todo!(),
            bc::Inst::FloatTrunc { v, to } => todo!(),
            bc::Inst::FloatExt { v, to } => todo!(),
            bc::Inst::FloatToIntUnsigned { v, to } => todo!(),
            bc::Inst::FloatToIntSigned { v, to } => todo!(),
            bc::Inst::IntToFloatUnsigned { v, to } => todo!(),
            bc::Inst::IntToFloatSigned { v, to } => todo!(),
            bc::Inst::PtrToWord { v } => todo!(),
            bc::Inst::WordToPtr { v } => todo!(),
            bc::Inst::BitwiseBin { op, lhs, rhs } => todo!(),
            bc::Inst::IntAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                let result = casted_uop!(width, wrapping_add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                let result = casted_uop!(width, wrapping_sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                let result = casted_uop!(width, wrapping_mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntDivUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_uop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntDivSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Div;
                let result = casted_iop!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntRemUnsigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_uop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntRemSigned { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                if rhs == 0 {
                    return failf!(span, "Division by zero");
                }
                use std::ops::Rem;
                let result = casted_iop!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result as u64));
                ip += 1
            }
            bc::Inst::IntCmp { lhs, rhs, pred, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                let b = match (width, pred) {
                    (_, bc::IntCmpPred::Eq) => (lhs == rhs),
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

                vm.stack.set_cur_inst_value(inst_index, Value::D64(b as u64));
                ip += 1
            }
            bc::Inst::FloatAdd { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                use std::ops::Add;
                let result = casted_float_op!(width, add, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::FloatSub { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                use std::ops::Sub;
                let result = casted_float_op!(width, sub, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::FloatMul { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                use std::ops::Mul;
                let result = casted_float_op!(width, mul, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::FloatDiv { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                use std::ops::Div;
                let result = casted_float_op!(width, div, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::FloatRem { lhs, rhs, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                use std::ops::Rem;
                let result = casted_float_op!(width, rem, lhs, rhs);

                vm.stack.set_cur_inst_value(inst_index, Value::D64(result));
                ip += 1
            }
            bc::Inst::FloatCmp { lhs, rhs, pred, width } => {
                let lhs = resolve_value!(lhs).expect_data();
                let rhs = resolve_value!(rhs).expect_data();
                let b = match (width, pred) {
                    (_, bc::FloatCmpPred::Eq) => lhs == rhs,
                    (32, bc::FloatCmpPred::Lt) => {
                        f32::from_bits(lhs as u32) < f32::from_bits(rhs as u32)
                    }
                    (32, bc::FloatCmpPred::Le) => {
                        f32::from_bits(lhs as u32) <= f32::from_bits(rhs as u32)
                    }
                    (32, bc::FloatCmpPred::Gt) => {
                        f32::from_bits(lhs as u32) > f32::from_bits(rhs as u32)
                    }
                    (32, bc::FloatCmpPred::Ge) => {
                        f32::from_bits(lhs as u32) >= f32::from_bits(rhs as u32)
                    }
                    (64, bc::FloatCmpPred::Lt) => f64::from_bits(lhs) < f64::from_bits(rhs),
                    (64, bc::FloatCmpPred::Le) => f64::from_bits(lhs) <= f64::from_bits(rhs),
                    (64, bc::FloatCmpPred::Gt) => f64::from_bits(lhs) > f64::from_bits(rhs),
                    (64, bc::FloatCmpPred::Ge) => f64::from_bits(lhs) >= f64::from_bits(rhs),
                    _ => unreachable!(),
                };

                vm.stack.set_cur_inst_value(inst_index, Value::D64(b as u64));
                ip += 1
            }
        }
    }
}

#[inline(always)]
fn resolve_value(k1: &mut TypedProgram, vm: &mut Vm, inst_offset: u32, value: BcValue) -> Value {
    debug!("  resolving {}", value);
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
                            let is_referencing = global.is_referencing;
                            let is_constant = global.is_constant;
                            let initial_value_id = match global.initial_value {
                                None => {
                                    k1.eval_global_body(global.ast_id, None, None).unwrap();
                                    k1.globals.get(id).initial_value.unwrap()
                                }
                                Some(value_id) => value_id,
                            };
                            let shared_vm_value =
                                static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
                            let layout = k1.types.get_pt_layout(&t);
                            if is_referencing {
                                if is_constant {
                                    let dst = k1.vmbc_static_stack.push_layout_uninit(layout);
                                    store_value(&k1.types, t, dst, shared_vm_value);
                                    let addr = Value::Addr { ptr: dst.cast_const() };
                                    k1.vmbc_global_constant_lookups.insert(id, addr);
                                    addr
                                } else {
                                    // We need a local copy of this
                                    let dst = vm.static_stack.push_layout_uninit(layout);
                                    store_value(&k1.types, t, dst, shared_vm_value);
                                    let addr = Value::Addr { ptr: dst.cast_const() };
                                    vm.globals.insert(id, addr);
                                    addr
                                }
                            } else {
                                vm.globals.insert(id, shared_vm_value);
                                shared_vm_value
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
                ScalarType::I8 => Value::D64(u32_data as u64),
                ScalarType::I16 => Value::D64(u32_data as u64),
                ScalarType::I32 => Value::D64(u32_data as u64),
                ScalarType::I64 => unreachable!(),
                ScalarType::F32 => Value::D64(u32_data as u64),
                ScalarType::F64 => unreachable!(),
                ScalarType::Pointer => unreachable!(),
            }
        }
        BcValue::PtrZero => Value::Addr { ptr: core::ptr::null() },
    }
}

fn function_id_to_ref_value(function_id: FunctionId) -> Value {
    let function_id_u32 = function_id.as_u32();
    let function_id_as_ptr = function_id_u32 as usize as *const u8;
    debug!(
        "Encoding function id {function_id_u32} into the pointer address of a Reference value {:?}",
        function_id_as_ptr
    );
    let value = Value::D64(function_id_u32 as u64);
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

    // task(vmbc): We need to cache these; treat it more like a reference to a constant in static
    // space than a stack literal!
    match k1.static_values.get(static_value_id) {
        StaticValue::Unit => Value::D64(0),
        StaticValue::Bool(bool_value) => Value::D64(*bool_value as u64),
        StaticValue::Char(char_byte) => Value::D64(*char_byte as u64),
        StaticValue::Int(iv) => Value::D64(iv.to_u64_unconditional()),
        StaticValue::Float(fv) => Value::D64(fv.as_f64().to_bits()),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(k1, *string_id);
            value
        }
        StaticValue::Zero(type_id) => {
            let layout = k1.types.get_layout(*type_id);
            static_zero_value(k1, *type_id, span)
        }
        StaticValue::Struct(static_struct) => {
            todo!("struct")
            // let mut values: SmallVec<[Value; 8]> = smallvec![];
            // // This is bad to do the values then copy them into the right place
            // // Let's just re-write for this round
            // let struct_layout = k1.types.get_struct_layout(static_struct.type_id);
            // let struct_base =
            //     vm.get_destination_stack(dst_stack).push_layout_uninit(struct_layout.layout);
            // for (field_index, f) in
            //     k1.static_values.mem.get_slice(static_struct.fields).iter().enumerate()
            // {
            //     let value = static_value_to_vm_value(vm, dst_stack, k1, *f);
            //     // Handle it right here.
            //     let field_offset = struct_layout.field_offsets[field_index];
            //     let field_ptr = unsafe { struct_base.byte_add(field_offset as usize) };
            //     let field_type_id = k1.static_values.get(f).get_type();
            //     store_value(field_type, field_ptr, value);
            //     values.push(value);
            // }
            // //let struct_value = vm.get_destination_stack(dst_stack).push_struct_values(
            // //    &k1.types,
            // //    static_struct.type_id,
            // //    &values,
            // //);
            // Ok(struct_value)
        }
        StaticValue::Enum(e) => {
            // This is bad to do the payload then copy it into the right place
            // Let's just re-write for this round
            // let payload_value = match e.payload {
            //     None => None,
            //     Some(static_value_id) => {
            //         let value = static_value_to_vm_value(vm, dst_stack, k1, static_value_id)?;
            //         Some(value)
            //     }
            // };
            // let enum_ptr = vm.get_destination_stack(dst_stack).push_enum(
            //     &k1.types,
            //     e.variant_type_id,
            //     payload_value,
            // );
            // let type_id = if e.typed_as_enum {
            //     k1.types.get(e.variant_type_id).expect_enum_variant().enum_type_id
            // } else {
            //     e.variant_type_id
            // };
            // Ok(Value::Agg { type_id, ptr: enum_ptr })
            todo!()
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

    let string_stack_addr = k1.vmbc_static_stack.push_t(k1_string);
    Value::Addr { ptr: string_stack_addr }
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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_scalar(t: ScalarType, dst: *mut u8, value: Value) {
    unsafe {
        match t {
            ScalarType::I8 => dst.write(value.expect_data() as u8),
            ScalarType::I16 => (dst as *mut u16).write(value.expect_data() as u16),
            ScalarType::I32 => (dst as *mut u32).write(value.expect_data() as u32),
            ScalarType::I64 => (dst as *mut u64).write(value.expect_data()),
            ScalarType::F32 => (dst as *mut u32).write(value.expect_data() as u32),
            ScalarType::F64 => (dst as *mut u64).write(value.expect_data()),
            ScalarType::Pointer => (dst as *mut usize).write(value.expect_data() as usize),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_value(types: &TypePool, t: PhysicalType, dst: *mut u8, value: Value) {
    match t {
        PhysicalType::Scalar(scalar_type) => store_scalar(scalar_type, dst, value),
        PhysicalType::Agg(pt_id) => {
            let record = types.phys_types.get(pt_id);
            let src = value.expect_addr();
            unsafe {
                core::ptr::copy_nonoverlapping(src, dst, record.layout.size as usize);
            }
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_scalar(t: ScalarType, ptr: *const u8) -> Value {
    unsafe {
        match t {
            ScalarType::I8 => Value::D64(ptr.read() as u64),
            ScalarType::I16 => Value::D64((ptr as *const u16).read() as u64),
            ScalarType::I32 => Value::D64((ptr as *const u32).read() as u64),
            ScalarType::I64 => Value::D64((ptr).read() as u64),
            ScalarType::F32 => Value::D64((ptr as *const u32).read() as u64),
            ScalarType::F64 => Value::D64((ptr).read() as u64),
            ScalarType::Pointer => {
                let read_address = (ptr as *const usize).read();
                let ptr = read_address as *const u8;
                Value::Addr { ptr }
            }
        }
    }
}

fn aligned_to(ptr: *const u8, align_bytes: usize) -> *const u8 {
    let bytes_needed = ptr.align_offset(align_bytes);
    unsafe { ptr.byte_add(bytes_needed) }
}

pub fn offset_at_index(types: &TypePool, type_id: TypeId, index: usize) -> usize {
    let size_bytes = types.get_layout(type_id).size as usize;
    index * size_bytes
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
        let empty_values = vec![Value::D64(0); values_cap];
        for _ in 0..frames_cap {
            inst_values.push(empty_values.clone());
        }

        let empty_params = vec![Value::D64(0); params_cap];
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
        match self.inst_values.get_mut(frame_index as usize) {
            None => self.inst_values.push(vec![Value::D64(0); 64]),
            Some(_) => (),
        };
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
    let ptr = value.expect_addr();
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
    vm_value: Value,
    span: SpanId,
) -> TyperResult<StaticValueId> {
    debug!("vm_to_static: {:?}", vm_value);
    todo!()
    // let v = match vm_value {
    //     Value::Unit => StaticValue::Unit,
    //     Value::Bool(b) => StaticValue::Bool(b),
    //     Value::Char(c) => StaticValue::Char(c),
    //     Value::Int(typed_integer_value) => StaticValue::Int(typed_integer_value),
    //     Value::Float(typed_float_value) => StaticValue::Float(typed_float_value),
    //     Value::Pointer(value) => {
    //         if value == 0 {
    //             StaticValue::Zero(POINTER_TYPE_ID)
    //         } else {
    //             return failf!(
    //                 span,
    //                 "Raw pointer ({:0x}) cannot be converted to a static value; use a Reference or a plain value type instead.",
    //                 value
    //             );
    //         }
    //     }
    //     Value::Reference { .. } => {
    //         // Now this, I can do. Load the value and bake it into the binary
    //         // This is just a de-reference
    //         // Needs to become a global.
    //         // Rely on the VM's code to load it, then make a K1 'global' to hold it?
    //
    //         //let _loaded_value = vm::load_value(vm, m, *type_id, *ptr, true, span).unwrap();
    //         todo!("Introduce StaticValue::Reference");
    //     }
    //     Value::Agg { type_id, ptr } => {
    //         if type_id == STRING_TYPE_ID {
    //             let box_str = value_to_string_id(k1, vm_value).map_err(|msg| {
    //                 errf!(span, "Could not convert string to static value: {msg}")
    //             })?;
    //             StaticValue::String(box_str)
    //         } else if k1.types.get_as_view_instance(type_id).is_some() {
    //             let (view, element_type) = value_as_view(k1, vm_value);
    //             let mut elements = k1.static_values.mem.new_vec(view.len as u32);
    //             for index in 0..view.len {
    //                 let elem_vm = get_view_element(vm, k1, view.data, element_type, index)
    //                     .unwrap_or_else(|e| {
    //                         vm_ice!(k1, vm, "Failed to load view element {index}: {}", e)
    //                     });
    //                 let elem_static = vm_value_to_static_value(k1, vm, elem_vm, span)?;
    //                 elements.push(elem_static);
    //             }
    //             let elements_slice = k1.static_values.mem.vec_to_mslice(&elements);
    //             StaticValue::LinearContainer(StaticContainer {
    //                 elements: elements_slice,
    //                 kind: StaticContainerKind::View,
    //                 type_id,
    //             })
    //         } else if k1.types.get_as_list_instance(type_id).is_some() {
    //             #[allow(clippy::if_same_then_else)]
    //             return failf!(
    //                 span,
    //                 "{} cannot be converted to static value; convert to a View first",
    //                 k1.type_id_to_string(type_id)
    //             );
    //         } else if k1.types.get_as_buffer_instance(type_id).is_some() {
    //             return failf!(
    //                 span,
    //                 "{} cannot be converted to static value; convert to a View first",
    //                 k1.type_id_to_string(type_id)
    //             );
    //         } else {
    //             let typ = k1.types.get(type_id);
    //             match typ {
    //                 Type::Struct(struct_type) => {
    //                     let mut field_value_ids =
    //                         k1.static_values.mem.new_vec(struct_type.fields.len());
    //                     for index in 0..struct_type.fields.len() {
    //                         let field_value =
    //                             load_struct_field(vm, k1, type_id, ptr, index as usize, false)
    //                                 .unwrap();
    //                         let field_static_value_id =
    //                             vm_value_to_static_value(k1, vm, field_value, span)?;
    //                         field_value_ids.push(field_static_value_id)
    //                     }
    //                     StaticValue::Struct(StaticStruct {
    //                         type_id,
    //                         fields: k1.static_values.mem.vec_to_mslice(&field_value_ids),
    //                     })
    //                 }
    //                 Type::Enum(enum_type) => {
    //                     let tag = load_value(vm, k1, enum_type.tag_type, ptr, false)
    //                         .unwrap()
    //                         .expect_int();
    //                     let variant =
    //                         enum_type.variants.iter().find(|v| v.tag_value == tag).unwrap();
    //                     let variant_type_id = variant.my_type_id;
    //                     let variant_index = variant.index;
    //
    //                     let payload = match variant.payload {
    //                         None => None,
    //                         Some(payload_type) => {
    //                             let payload_ptr = gep_enum_payload(&k1.types, variant, ptr);
    //                             let payload_value =
    //                                 load_value(vm, k1, payload_type, payload_ptr, false).unwrap();
    //                             let static_value_id =
    //                                 vm_value_to_static_value(k1, vm, payload_value, span)?;
    //                             Some(static_value_id)
    //                         }
    //                     };
    //                     StaticValue::Enum(StaticEnum {
    //                         variant_type_id,
    //                         variant_index,
    //                         typed_as_enum: true,
    //                         payload,
    //                     })
    //                 }
    //                 Type::EnumVariant(_enum_variant) => {
    //                     todo!("enum variant vm -> static")
    //                 }
    //                 Type::Array(array_type) => {
    //                     let element_type = array_type.element_type;
    //                     let Some(count) = array_type.concrete_count else {
    //                         return failf!(
    //                             span,
    //                             "Cannot convert array of unknown size to static value"
    //                         );
    //                     };
    //                     let count = count as usize;
    //                     let mut elements = k1.static_values.mem.new_vec(count as u32);
    //                     for index in 0..count {
    //                         let elem_result = get_view_element(vm, k1, ptr, element_type, index);
    //                         let elem_vm = elem_result.unwrap_or_else(|e| {
    //                             vm_ice!(k1, vm, "Failed to load view element {index}: {}", e)
    //                         });
    //                         let elem_static = vm_value_to_static_value(k1, vm, elem_vm, span)?;
    //                         elements.push(elem_static);
    //                     }
    //                     let elements_slice = k1.static_values.mem.vec_to_mslice(&elements);
    //                     StaticValue::LinearContainer(StaticContainer {
    //                         elements: elements_slice,
    //                         kind: StaticContainerKind::Array,
    //                         type_id,
    //                     })
    //                 }
    //                 _ => vm_crash(k1, vm, "aggregate should be struct or enum or array"),
    //             }
    //         }
    //     }
    // };
    // let id = k1.static_values.add(v);
    // Ok(id)
}

//pub fn value_as_view(k1: &TypedProgram, view_value: Value) -> (k1_types::K1ViewLike, TypeId) {
//    let element_type = k1.types.get_as_view_instance(view_value.get_type()).unwrap();
//    let ptr = view_value.expect_agg();
//    let buffer_ptr = ptr as *const k1_types::K1ViewLike;
//    let buffer = unsafe { buffer_ptr.read() };
//    (buffer, element_type)
//}

// #[allow(clippy::not_unsafe_ptr_arg_deref)]
// pub fn get_view_element(
//     vm: &mut Vm,
//     k1: &TypedProgram,
//     data_ptr: *const u8,
//     elem_type: TypeId,
//     index: usize,
// ) -> TyperResult<Value> {
//     let elem_offset = offset_at_index(&k1.types, elem_type, index);
//     let elem_ptr = unsafe { data_ptr.byte_add(elem_offset) };
//     load_value(vm, k1, elem_type, elem_ptr, true)
// }

#[allow(unused)]
fn render_debug_vm_result(
    w: &mut impl std::fmt::Write,
    vm: &mut Vm,
    k1: &TypedProgram,
    result: VmResult,
) {
    match result {
        VmResult::Value(value) => {
            w.write_str("VALUE ").unwrap();
            render_debug_value(w, vm, k1, value)
        }
        VmResult::Break(value) => {
            w.write_str("BREAK ").unwrap();
            render_debug_value(w, vm, k1, value)
        }
        VmResult::Return(value) => {
            w.write_str("RETURN ").unwrap();
            render_debug_value(w, vm, k1, value)
        }
        VmResult::Exit(vm_exit) => write!(w, "EXIT {}", vm_exit.code).unwrap(),
    }
}

#[allow(unused)]
fn debug_vm_result_to_string(vm: &mut Vm, k1: &TypedProgram, result: VmResult) -> String {
    let mut s = String::new();
    render_debug_vm_result(&mut s, vm, k1, result);
    s
}

fn render_debug_value(w: &mut impl std::fmt::Write, _vm: &Vm, _k1: &TypedProgram, value: Value) {
    //task(vmbc): Pass type info into here somehow
    match value {
        Value::D64(u64) => write!(w, "{} 0x{:x}", u64, u64).unwrap(),
        Value::Addr { ptr } => write!(w, "{:?}", ptr).unwrap(),
    }
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
        Some(PhysicalType::Scalar(st)) => match st {
            ScalarType::Pointer => Value::Addr { ptr: core::ptr::null() },
            _ => Value::D64(0),
        },
        Some(PhysicalType::Agg(agg_id)) => {
            let layout = k1.types.phys_types.get(agg_id).layout;
            let data: *mut u8 = k1.vmbc_static_stack.push_layout_uninit(layout);
            unsafe { std::ptr::write_bytes(data, 0, layout.size as usize) };

            Value::Addr { ptr: data.cast_const() }
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
        IntegerType::U64 => int_to_cast.to_u64_unconditional().into(),
        IntegerType::I8 => (int_to_cast.to_u8() as i8).into(),
        IntegerType::I16 => (int_to_cast.to_u16() as i16).into(),
        IntegerType::I32 => (int_to_cast.to_u32() as i32).into(),
        IntegerType::I64 => (int_to_cast.to_u64_unconditional() as i64).into(),
        IntegerType::UWord(WordSize::W32) => unreachable!(),
        IntegerType::UWord(WordSize::W64) => {
            TypedIntValue::UWord64(int_to_cast.to_u64_unconditional())
        }
        IntegerType::IWord(WordSize::W32) => unreachable!(),
        IntegerType::IWord(WordSize::W64) => {
            TypedIntValue::IWord64(int_to_cast.to_u64_unconditional() as i64)
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
