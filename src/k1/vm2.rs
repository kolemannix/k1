use std::ffi::c_void;
use std::num::NonZeroU32;

use ahash::HashMapExt;
use colored::Colorize;
use fxhash::FxHashMap;
use libffi::{low::CodePtr, raw};
use log::debug;

use crate::bc::{self, BcCallee, BcInstKind, BcUnit, BcValue};
use crate::ir::{self, IrUnitId};
use crate::kmem;
use crate::lex::SpanId;
use crate::typer::types::{Layout, PhysicalType, ScalarType, TypeId};
use crate::typer::{
    ErrorKind, FunctionId, K1Message, K1Result, MessageLevel, StaticValueId, TypedExprId,
    TypedGlobalId, TypedProgram,
};
use crate::vm::{self, Value, k1_types, vm_ffi};
use crate::{STACK_SIZE, errf, failf};

macro_rules! vm2_ice {
    ($k1:expr, $vm:expr, $($format_args:expr),*) => {{
        let s: String = format!($($format_args),*);
        vm2_crash($k1, $vm, &s)
    }};
}

macro_rules! casted_uop {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            8 => ($lhs as u8).$op($rhs as u8) as u64,
            16 => ($lhs as u16).$op($rhs as u16) as u64,
            32 => ($lhs as u32).$op($rhs as u32) as u64,
            64 => ($lhs as u64).$op($rhs as u64),
            _ => unreachable!(),
        }
    };
}

macro_rules! casted_iop {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            8 => ($lhs as i8).$op($rhs as i8) as i64,
            16 => ($lhs as i16).$op($rhs as i16) as i64,
            32 => ($lhs as i32).$op($rhs as i32) as i64,
            64 => ($lhs as i64).$op($rhs as i64),
            _ => unreachable!(),
        }
    };
}

macro_rules! casted_float_op {
    ($width:expr, $op:ident, $lhs:expr, $rhs:expr) => {
        match $width {
            32 => f32::from_bits($lhs as u32).$op(f32::from_bits($rhs as u32)).to_bits() as u64,
            64 => f64::from_bits($lhs).$op(f64::from_bits($rhs)).to_bits(),
            _ => unreachable!(),
        }
    };
}

macro_rules! bin_int {
    ($k1:expr, $vm:expr, $frame_index:expr, $dst:expr, $lhs:expr, $rhs:expr, $width:expr, $op:ident) => {{
        let lhs = resolve_value($k1, $vm, $frame_index, $lhs)?.bits();
        let rhs = resolve_value($k1, $vm, $frame_index, $rhs)?.bits();
        $vm.stack.set_local_value($frame_index, $dst, Value(casted_uop!($width, $op, lhs, rhs)));
        $vm.stack.frames[$frame_index as usize].pc += 1;
    }};
}

macro_rules! bin_int_op {
    ($k1:expr, $vm:expr, $frame_index:expr, $dst:expr, $lhs:expr, $rhs:expr, $width:expr, $trait:ident, $op:ident) => {{
        let lhs = resolve_value($k1, $vm, $frame_index, $lhs)?.bits();
        let rhs = resolve_value($k1, $vm, $frame_index, $rhs)?.bits();
        use std::ops::$trait;
        $vm.stack.set_local_value($frame_index, $dst, Value(casted_uop!($width, $op, lhs, rhs)));
        $vm.stack.frames[$frame_index as usize].pc += 1;
    }};
}

macro_rules! bin_float {
    ($k1:expr, $vm:expr, $frame_index:expr, $dst:expr, $lhs:expr, $rhs:expr, $width:expr, $trait:ident, $op:ident) => {{
        let lhs = resolve_value($k1, $vm, $frame_index, $lhs)?.bits();
        let rhs = resolve_value($k1, $vm, $frame_index, $rhs)?.bits();
        use std::ops::$trait;
        $vm.stack.set_local_value(
            $frame_index,
            $dst,
            Value(casted_float_op!($width, $op, lhs, rhs)),
        );
        $vm.stack.frames[$frame_index as usize].pc += 1;
    }};
}

#[derive(Clone)]
struct CompilerMessage {
    level: MessageLevel,
    message: crate::parse::StringId,
    filename: String,
    line: u32,
}

pub struct Vm2 {
    globals: FxHashMap<TypedGlobalId, Value>,
    pub stack: Stack2,
    pub static_stack: vm::Stack,
    eval_span: SpanId,
    compiler_messages: Vec<CompilerMessage>,
    overall_return_addr: *mut u8,
}

impl Vm2 {
    pub fn make() -> Self {
        Self {
            globals: FxHashMap::with_capacity(8192),
            stack: Stack2::make(),
            static_stack: vm::Stack::make(),
            eval_span: SpanId::NONE,
            compiler_messages: Vec::with_capacity(16),
            overall_return_addr: core::ptr::null_mut(),
        }
    }

    pub fn reset(&mut self, arena_global_id: Option<TypedGlobalId>) {
        let arena_to_preserve = if let Some(arena_global_id) = arena_global_id {
            if let Some(arena_value) = self.globals.get(&arena_global_id) {
                let arena_ptr: *const k1_types::Arena = arena_value.as_ptr().cast();
                debug!("Preserving core/mem/arena allocation at {:p}", arena_ptr);
                Some(unsafe { arena_ptr.read() })
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
        self.overall_return_addr = core::ptr::null_mut();
        self.eval_span = SpanId::NONE;

        if let Some(mut arena) = arena_to_preserve {
            arena.curAddr = arena.basePtr.addr() as u64;
            let arena_ptr = self.static_stack.push_t(arena);
            self.globals.insert(arena_global_id.unwrap(), Value::ptr(arena_ptr));
        }
    }
}

pub fn execute_compiled_function(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    function_id: FunctionId,
    arguments: &[StaticValueId],
) -> K1Result<StaticValueId> {
    let span = k1.get_function_span(function_id);
    let unit = bc::get_or_compile_function(k1, function_id)?;
    execute_compiled_unit(k1, vm, unit, arguments, span)
}

pub fn execute_compiled_expr(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    expr_id: TypedExprId,
) -> K1Result<StaticValueId> {
    let span = k1.exprs.get_span(expr_id);
    let unit = bc::get_or_compile_expr(k1, expr_id)?;
    execute_compiled_unit(k1, vm, unit, &[], span)
}

fn execute_compiled_unit(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    unit: BcUnit,
    arguments: &[StaticValueId],
    span: SpanId,
) -> K1Result<StaticValueId> {
    let start = k1.timing.clock.raw();
    vm.eval_span = span;

    debug_assert!(vm.stack.frames.is_empty());
    let ret_layout = k1.types.get_pt_layout(unit.return_pt);
    let ret_addr = vm.stack.push_layout_uninit(ret_layout);
    vm.overall_return_addr = ret_addr;
    vm.stack.push_frame(Some(span), unit.clone(), RetInfo2::top(unit.return_pt));

    for (index, arg) in arguments.iter().enumerate() {
        let vm_value = vm::static_value_to_vm_value(k1, *arg, span);
        vm.stack.set_param_value(0, index as u32, vm_value);
    }

    let exit_code = exec_loop(k1, vm)?;
    let elapsed_nanos = k1.timing.elapsed_nanos(start);
    k1.timing.total_vm_nanos += elapsed_nanos as i64;
    report_execution_messages(k1, vm, span);

    if exit_code != 0 {
        failf!(span, "Static execution exited with code: {}", exit_code)
    } else if unit.diverges || unit.return_pt.is_empty() {
        vm.overall_return_addr = core::ptr::null_mut();
        Ok(k1.static_values.empty_id())
    } else {
        let loaded = vm::load_value(unit.return_pt, ret_addr);
        let returned_value = vm::vm_value_to_static_value(k1, unit.result_type_id, loaded, span)?;
        vm.overall_return_addr = core::ptr::null_mut();
        Ok(returned_value)
    }
}

fn exec_loop(k1: &mut TypedProgram, vm: &mut Vm2) -> K1Result<i32> {
    let exit_code = 'exec: loop {
        let frame_index = vm.stack.current_frame_index();
        let pc = vm.stack.frames[frame_index as usize].pc;
        let inst = vm.stack.frames[frame_index as usize].unit.instrs[pc].clone();
        vm.eval_span = vm.stack.frames[frame_index as usize].unit.spans[pc];
        k1.timing.total_vm_instrs += 1;

        macro_rules! resolve {
            ($v:expr) => {
                resolve_value(k1, vm, frame_index, $v)?
            };
        }
        macro_rules! set {
            ($slot:expr, $value:expr) => {
                vm.stack.set_local_value(frame_index, $slot, $value)
            };
        }
        macro_rules! next {
            () => {{
                vm.stack.frames[frame_index as usize].pc += 1;
                continue 'exec;
            }};
        }
        macro_rules! jump {
            ($offset:expr) => {{
                let frame = &mut vm.stack.frames[frame_index as usize];
                frame.prev_block = inst.block;
                frame.pc = ((pc as isize) + ($offset as isize)) as usize;
                continue 'exec;
            }};
        }

        match inst.kind {
            BcInstKind::Data { dst, imm } => {
                let value = match imm {
                    ir::DataInst::U64(v) => Value::u64(v),
                    ir::DataInst::I64(v) => Value::i64(v),
                    ir::DataInst::Float(fv) => Value::float_value(fv),
                };
                set!(dst, value);
                next!();
            }
            BcInstKind::Alloca { dst, vm_layout } => {
                let ptr = vm.stack.push_layout_uninit(vm_layout);
                set!(dst, Value::ptr(ptr));
                next!();
            }
            BcInstKind::Store { dst, value, t } => {
                let dst = resolve!(dst);
                let value = resolve!(value);
                vm::store_scalar(t, dst.as_ptr(), value);
                next!();
            }
            BcInstKind::Load { dst, t, src } => {
                let src = resolve!(src);
                let loaded = vm::load_scalar(t, src.as_ptr());
                set!(dst, loaded);
                next!();
            }
            BcInstKind::Copy { dst, src, vm_size } => {
                let dst = resolve!(dst).as_ptr();
                let src = resolve!(src).as_ptr();
                vm::memmove(src, dst, vm_size as usize);
                next!();
            }
            BcInstKind::StructOffset { dst, base, vm_offset } => {
                let base = resolve!(base).as_ptr();
                let field_ptr = unsafe { base.byte_add(vm_offset as usize) };
                set!(dst, Value::ptr(field_ptr));
                next!();
            }
            BcInstKind::ArrayOffset { dst, element_t, base, element_index } => {
                let base = resolve!(base).as_ptr();
                let index = resolve!(element_index).bits();
                let elem_layout = k1.types.get_pt_layout(element_t);
                let ptr = unsafe { base.byte_add(elem_layout.offset_at_index(index as usize)) };
                set!(dst, Value::ptr(ptr));
                next!();
            }
            BcInstKind::Call { dst, ret_type, callee, args, ret_dst } => {
                let ret_layout = k1.types.get_pt_layout(ret_type);
                let has_dst = match (dst, ret_dst) {
                    (Some(slot), Some(ret_dst)) => {
                        let dst_addr = resolve!(ret_dst);
                        set!(slot, dst_addr);
                        true
                    }
                    (Some(slot), None) if ret_type.is_agg() => {
                        let storage = Value::ptr(vm.stack.push_layout_uninit(ret_layout));
                        set!(slot, storage);
                        true
                    }
                    _ => false,
                };

                macro_rules! builtin_return {
                    ($value:expr) => {{
                        if let Some(slot) = dst {
                            fulfill_return(k1, vm, frame_index, slot, has_dst, ret_type, $value);
                        }
                        next!();
                    }};
                    () => {{
                        next!();
                    }};
                }

                let dispatch_function_id = match callee {
                    BcCallee::Extern { library_name, function_name, function_id } => {
                        let result = handle_ffi_call(
                            k1,
                            vm,
                            frame_index,
                            ret_type,
                            &args,
                            library_name,
                            function_name,
                            function_id,
                        )?;
                        builtin_return!(result);
                    }
                    BcCallee::Direct(function_id) => function_id,
                    BcCallee::Indirect(value) => {
                        let callee_value = resolve!(value);
                        let function_id_u64 = callee_value.bits();
                        let nzu32 = NonZeroU32::new(function_id_u64 as u32).unwrap();
                        FunctionId::from_nzu32(nzu32)
                    }
                    BcCallee::BackendBuiltin(backend_builtin) => match backend_builtin {
                        ir::BackendBuiltin::TypeSchema => {
                            let type_id_arg = resolve!(args[0]).bits();
                            let type_id =
                                TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                            let Some(schema_static_value_id) = k1.type_schemas.get(&type_id) else {
                                vm2_ice!(
                                    k1,
                                    vm,
                                    "Missing type schema: {}",
                                    k1.type_id_to_string(type_id)
                                )
                            };
                            let value = vm::static_value_to_vm_value(
                                k1,
                                *schema_static_value_id,
                                vm.eval_span,
                            );
                            builtin_return!(value);
                        }
                        ir::BackendBuiltin::TypeName => {
                            let type_id_arg = resolve!(args[0]).bits();
                            let type_id =
                                TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                            let name_value_id = *k1.type_names.get(&type_id).unwrap();
                            let value =
                                vm::static_value_to_vm_value(k1, name_value_id, vm.eval_span);
                            builtin_return!(value);
                        }
                        ir::BackendBuiltin::Allocate | ir::BackendBuiltin::AllocateZeroed => {
                            let zero = backend_builtin == ir::BackendBuiltin::AllocateZeroed;
                            let size = resolve!(args[0]).as_usize();
                            let align = resolve!(args[1]).as_usize();
                            let Ok(layout) = std::alloc::Layout::from_size_align(size, align)
                            else {
                                vm2_crash(
                                    k1,
                                    vm,
                                    format!("Bad allocation layout: size={size}, align={align}"),
                                )
                            };
                            let ptr = if zero {
                                unsafe { std::alloc::alloc_zeroed(layout) }
                            } else {
                                unsafe { std::alloc::alloc(layout) }
                            };
                            builtin_return!(Value::ptr(ptr));
                        }
                        ir::BackendBuiltin::Reallocate => {
                            let old_ptr = resolve!(args[0]).as_ptr();
                            let old_size = resolve!(args[1]).as_usize();
                            let align = resolve!(args[2]).as_usize();
                            let new_size = resolve!(args[3]).as_usize();
                            let layout =
                                std::alloc::Layout::from_size_align(old_size, align).unwrap();
                            let ptr = unsafe { std::alloc::realloc(old_ptr, layout, new_size) };
                            builtin_return!(Value::ptr(ptr));
                        }
                        ir::BackendBuiltin::Free => {
                            let ptr = resolve!(args[0]).as_ptr();
                            let size = resolve!(args[1]).as_usize();
                            let align = resolve!(args[2]).as_usize();
                            let layout = std::alloc::Layout::from_size_align(size, align).unwrap();
                            unsafe { std::alloc::dealloc(ptr, layout) };
                            builtin_return!();
                        }
                        ir::BackendBuiltin::MemCopy | ir::BackendBuiltin::MemMove => {
                            let dst = resolve!(args[0]).as_ptr();
                            let src = resolve!(args[1]).as_ptr();
                            let count = resolve!(args[2]).as_usize();
                            if backend_builtin == ir::BackendBuiltin::MemCopy {
                                vm::memcopy(src, dst, count);
                            } else {
                                vm::memmove(src, dst, count);
                            }
                            builtin_return!();
                        }
                        ir::BackendBuiltin::MemSet => {
                            let dst = resolve!(args[0]).as_ptr();
                            let value = resolve!(args[1]).bits() as u8;
                            let count = resolve!(args[2]).as_usize();
                            unsafe { std::ptr::write_bytes(dst, value, count) };
                            builtin_return!();
                        }
                        ir::BackendBuiltin::MemEquals => {
                            let p1 = resolve!(args[0]).as_ptr();
                            let p2 = resolve!(args[1]).as_ptr();
                            let size = resolve!(args[2]).as_usize();
                            let p1_slice = unsafe { std::slice::from_raw_parts(p1, size) };
                            let p2_slice = unsafe { std::slice::from_raw_parts(p2, size) };
                            builtin_return!(Value::bool(p1_slice == p2_slice));
                        }
                        ir::BackendBuiltin::Exit => {
                            let exit_code = resolve!(args[0]).bits();
                            break 'exec exit_code as i32;
                        }
                        ir::BackendBuiltin::CompilerMessage => {
                            let location_arg = resolve!(args[0]);
                            let level_arg = resolve!(args[1]);
                            let message_arg = resolve!(args[2]);
                            let location = unsafe {
                                (location_arg.as_ptr() as *const k1_types::K1SourceLocation).read()
                            };
                            let level =
                                match k1_types::CompilerMessageLevel::from_u8(level_arg.as_u8())
                                    .unwrap_or(k1_types::CompilerMessageLevel::Info)
                                {
                                    k1_types::CompilerMessageLevel::Info => MessageLevel::Info,
                                    k1_types::CompilerMessageLevel::Warn => MessageLevel::Warn,
                                    k1_types::CompilerMessageLevel::Error => MessageLevel::Error,
                                };
                            let message =
                                vm::value_to_string_id(k1, message_arg).map_err(|msg| {
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
                            builtin_return!();
                        }
                    },
                };

                let callee_unit = bc::get_or_compile_function(k1, dispatch_function_id)?;
                let caller_frame_index = vm.stack.current_frame_index();
                vm.stack.frames[caller_frame_index as usize].pc = pc + 1;
                let ret_info = RetInfo2 {
                    pt: ret_type,
                    frame_index: caller_frame_index,
                    result_slot: dst,
                    has_dst,
                    top: false,
                };
                vm.stack.push_frame(Some(vm.eval_span), callee_unit, ret_info);
                let new_frame_index = vm.stack.current_frame_index();
                for (index, arg) in args.iter().enumerate() {
                    let value = resolve_value(k1, vm, caller_frame_index, *arg)?;
                    vm.stack.set_param_value(new_frame_index, index as u32, value);
                }
                continue 'exec;
            }
            BcInstKind::Jump { offset } => jump!(offset),
            BcInstKind::JumpIf { cond, cons_offset, alt_offset } => {
                if resolve!(cond).as_bool() {
                    jump!(cons_offset)
                } else {
                    jump!(alt_offset)
                }
            }
            BcInstKind::Unreachable => {
                return failf!(vm.eval_span, "Reached unreachable instruction");
            }
            BcInstKind::Phi { dst, incomings } => {
                let prev_block = vm.stack.frames[frame_index as usize].prev_block;
                let Some((_, value)) = incomings.iter().find(|(from, _)| *from == prev_block)
                else {
                    return failf!(
                        vm.eval_span,
                        "No phi incoming for predecessor block {}",
                        prev_block
                    );
                };
                let value = resolve!(*value);
                set!(dst, value);
                next!();
            }
            BcInstKind::Ret { v } => {
                let returned_value = resolve!(v);
                let cur_frame_index = vm.stack.current_frame_index();
                let ret_info = vm.stack.frames[cur_frame_index as usize].ret_info;
                if ret_info.top {
                    if !ret_info.pt.is_empty() {
                        if vm.overall_return_addr.is_null() {
                            vm2_ice!(k1, vm, "Top-level return address not initialized");
                        }
                        vm::store_value(
                            &k1.types,
                            ret_info.pt,
                            vm.overall_return_addr,
                            returned_value,
                        );
                    }
                    vm.stack.pop_frame();
                    break 'exec 0;
                } else {
                    let slot = ret_info.result_slot.unwrap();
                    fulfill_return(
                        k1,
                        vm,
                        ret_info.frame_index,
                        slot,
                        ret_info.has_dst,
                        ret_info.pt,
                        returned_value,
                    );
                    vm.stack.pop_frame();
                    continue 'exec;
                }
            }
            BcInstKind::BoolNegate { dst, v } => {
                let b = resolve!(v).as_bool();
                set!(dst, Value::bool(!b));
                next!();
            }
            BcInstKind::BitNot { dst, v } => {
                let input = resolve!(v);
                set!(dst, Value(!input.bits()));
                next!();
            }
            BcInstKind::BitCast { dst, v }
            | BcInstKind::PtrToWord { dst, v }
            | BcInstKind::WordToPtr { dst, v } => {
                let input = resolve!(v);
                set!(dst, input);
                next!();
            }
            BcInstKind::IntTrunc { dst, v, to } => {
                let input = resolve!(v);
                set!(dst, input.truncated(to.width()));
                next!();
            }
            BcInstKind::IntExtU { dst, v } => {
                let input = resolve!(v);
                set!(dst, input);
                next!();
            }
            BcInstKind::IntExtS { dst, v, from, to } => {
                let input = resolve!(v);
                set!(dst, input.sign_extended(from.width(), to.width()));
                next!();
            }
            BcInstKind::FloatTrunc { dst, v } => {
                let input = resolve!(v);
                set!(dst, Value::f32(input.as_f64() as f32));
                next!();
            }
            BcInstKind::FloatExt { dst, v } => {
                let input = resolve!(v);
                set!(dst, Value::f64(input.as_f32() as f64));
                next!();
            }
            BcInstKind::Float32ToIntUnsigned { dst, v, to } => {
                let f = resolve!(v).as_f32();
                let result = match to {
                    ScalarType::U8 => Value::u8(f as u8),
                    ScalarType::U16 => Value::u16(f as u16),
                    ScalarType::U32 => Value::u32(f as u32),
                    ScalarType::U64 => Value::u64(f as u64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::Float32ToIntSigned { dst, v, to } => {
                let f = resolve!(v).as_f32();
                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::Float64ToIntUnsigned { dst, v, to } => {
                let f = resolve!(v).as_f64();
                let result = match to {
                    ScalarType::U8 => Value::u8(f as u8),
                    ScalarType::U16 => Value::u16(f as u16),
                    ScalarType::U32 => Value::u32(f as u32),
                    ScalarType::U64 => Value::u64(f as u64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::Float64ToIntSigned { dst, v, to } => {
                let f = resolve!(v).as_f64();
                let result = match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::IntToFloatUnsigned { dst, v, from, to } => {
                let bits = resolve!(v).bits();
                let result = match (from, to) {
                    (ScalarType::U8, ScalarType::F32) => Value::f32(bits as u8 as f32),
                    (ScalarType::U16, ScalarType::F32) => Value::f32(bits as u16 as f32),
                    (ScalarType::U32, ScalarType::F32) => Value::f32(bits as u32 as f32),
                    (ScalarType::U64, ScalarType::F32) => Value::f32(bits as u64 as f32),
                    (ScalarType::U8, ScalarType::F64) => Value::f64(bits as u8 as f64),
                    (ScalarType::U16, ScalarType::F64) => Value::f64(bits as u16 as f64),
                    (ScalarType::U32, ScalarType::F64) => Value::f64(bits as u32 as f64),
                    (ScalarType::U64, ScalarType::F64) => Value::f64(bits as u64 as f64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::IntToFloatSigned { dst, v, from, to } => {
                let bits = resolve!(v).bits();
                let result = match (from, to) {
                    (ScalarType::I8, ScalarType::F32) => Value::f32(bits as i8 as f32),
                    (ScalarType::I16, ScalarType::F32) => Value::f32(bits as i16 as f32),
                    (ScalarType::I32, ScalarType::F32) => Value::f32(bits as i32 as f32),
                    (ScalarType::I64, ScalarType::F32) => Value::f32(bits as i64 as f32),
                    (ScalarType::I8, ScalarType::F64) => Value::f64(bits as i8 as f64),
                    (ScalarType::I16, ScalarType::F64) => Value::f64(bits as i16 as f64),
                    (ScalarType::I32, ScalarType::F64) => Value::f64(bits as i32 as f64),
                    (ScalarType::I64, ScalarType::F64) => Value::f64(bits as i64 as f64),
                    _ => unreachable!(),
                };
                set!(dst, result);
                next!();
            }
            BcInstKind::IntAdd { dst, lhs, rhs, width } => {
                bin_int!(k1, vm, frame_index, dst, lhs, rhs, width, wrapping_add)
            }
            BcInstKind::IntSub { dst, lhs, rhs, width } => {
                bin_int!(k1, vm, frame_index, dst, lhs, rhs, width, wrapping_sub)
            }
            BcInstKind::IntMul { dst, lhs, rhs, width } => {
                bin_int!(k1, vm, frame_index, dst, lhs, rhs, width, wrapping_mul)
            }
            BcInstKind::IntDivUnsigned { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Div;
                set!(dst, Value(casted_uop!(width, div, lhs, rhs)));
                next!();
            }
            BcInstKind::IntDivSigned { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Div;
                set!(dst, Value(casted_iop!(width, div, lhs, rhs) as u64));
                next!();
            }
            BcInstKind::IntRemUnsigned { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Rem;
                set!(dst, Value(casted_uop!(width, rem, lhs, rhs)));
                next!();
            }
            BcInstKind::IntRemSigned { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                if rhs == 0 {
                    return failf!(vm.eval_span, "Division by zero");
                }
                use std::ops::Rem;
                set!(dst, Value(casted_iop!(width, rem, lhs, rhs) as u64));
                next!();
            }
            BcInstKind::IntCmp { dst, lhs, rhs, pred, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                let b = int_cmp(lhs, rhs, pred, width);
                set!(dst, Value::bool(b));
                next!();
            }
            BcInstKind::FloatAdd { dst, lhs, rhs, width } => {
                bin_float!(k1, vm, frame_index, dst, lhs, rhs, width, Add, add)
            }
            BcInstKind::FloatSub { dst, lhs, rhs, width } => {
                bin_float!(k1, vm, frame_index, dst, lhs, rhs, width, Sub, sub)
            }
            BcInstKind::FloatMul { dst, lhs, rhs, width } => {
                bin_float!(k1, vm, frame_index, dst, lhs, rhs, width, Mul, mul)
            }
            BcInstKind::FloatDiv { dst, lhs, rhs, width } => {
                bin_float!(k1, vm, frame_index, dst, lhs, rhs, width, Div, div)
            }
            BcInstKind::FloatRem { dst, lhs, rhs, width } => {
                bin_float!(k1, vm, frame_index, dst, lhs, rhs, width, Rem, rem)
            }
            BcInstKind::FloatCmp { dst, lhs, rhs, pred, width } => {
                let lhs = resolve!(lhs);
                let rhs = resolve!(rhs);
                let b = match (width, pred) {
                    (_, ir::FloatCmpPred::Eq) => lhs.bits() == rhs.bits(),
                    (32, ir::FloatCmpPred::Lt) => lhs.as_f32() < rhs.as_f32(),
                    (32, ir::FloatCmpPred::Le) => lhs.as_f32() <= rhs.as_f32(),
                    (32, ir::FloatCmpPred::Gt) => lhs.as_f32() > rhs.as_f32(),
                    (32, ir::FloatCmpPred::Ge) => lhs.as_f32() >= rhs.as_f32(),
                    (64, ir::FloatCmpPred::Lt) => lhs.as_f64() < rhs.as_f64(),
                    (64, ir::FloatCmpPred::Le) => lhs.as_f64() <= rhs.as_f64(),
                    (64, ir::FloatCmpPred::Gt) => lhs.as_f64() > rhs.as_f64(),
                    (64, ir::FloatCmpPred::Ge) => lhs.as_f64() >= rhs.as_f64(),
                    _ => unreachable!(),
                };
                set!(dst, Value::bool(b));
                next!();
            }
            BcInstKind::BitAnd { dst, lhs, rhs, width } => {
                bin_int_op!(k1, vm, frame_index, dst, lhs, rhs, width, BitAnd, bitand)
            }
            BcInstKind::BitOr { dst, lhs, rhs, width } => {
                bin_int_op!(k1, vm, frame_index, dst, lhs, rhs, width, BitOr, bitor)
            }
            BcInstKind::BitXor { dst, lhs, rhs, width } => {
                bin_int_op!(k1, vm, frame_index, dst, lhs, rhs, width, BitXor, bitxor)
            }
            BcInstKind::BitShiftLeft { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
                use std::ops::Shl;
                set!(dst, Value(casted_uop!(width, shl, lhs, rhs)));
                next!();
            }
            BcInstKind::BitUnsignedShiftRight { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
                use std::ops::Shr;
                set!(dst, Value(casted_uop!(width, shr, lhs, rhs)));
                next!();
            }
            BcInstKind::BitSignedShiftRight { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
                use std::ops::Shr;
                set!(dst, Value(casted_iop!(width, shr, lhs, rhs) as u64));
                next!();
            }
            BcInstKind::BakeStaticValue { dst, type_id, value } => {
                let value_value = resolve!(value);
                let value_id =
                    vm::vm_value_to_static_value(k1, type_id, value_value, vm.eval_span)?;
                set!(dst, Value::u64(value_id.as_u32() as u64));
                next!();
            }
        }
    };
    Ok(exit_code)
}

#[inline(always)]
fn resolve_value(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    frame_index: u32,
    value: BcValue,
) -> K1Result<Value> {
    match value {
        BcValue::Local(slot) => Ok(vm.stack.get_local_value(frame_index, slot)),
        BcValue::StaticValue { id, .. } => Ok(vm::static_value_to_vm_value(k1, id, vm.eval_span)),
        BcValue::FunctionAddr(function_id) => Ok(vm::function_id_to_ref_value(function_id)),
        BcValue::FnParam { index, .. } => Ok(vm.stack.get_param_value(frame_index, index)),
        BcValue::Data32 { t, data } => {
            let value = match t {
                ScalarType::F32 => Value(data as u64),
                ScalarType::F64 => Value::f64(data as f32 as f64),
                ScalarType::Pointer => Value(data as u64),
                ScalarType::I8 | ScalarType::I16 | ScalarType::I32 => Value(data as u64),
                ScalarType::I64 => Value(data as i32 as i64 as u64),
                ScalarType::U8 | ScalarType::U16 | ScalarType::U32 | ScalarType::U64 => {
                    Value(data as u64)
                }
            };
            Ok(value)
        }
        BcValue::GlobalAddr { storage_pt, id } => resolve_global(k1, vm, id, storage_pt),
        BcValue::Empty => Ok(Value(0)),
    }
}

fn resolve_global(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    global_id: TypedGlobalId,
    t: PhysicalType,
) -> K1Result<Value> {
    if let Some(v) = k1.vm_global_constant_lookups.get(&global_id) {
        return Ok(*v);
    }
    if let Some(v) = vm.globals.get(&global_id) {
        return Ok(*v);
    }

    let global = k1.globals.get(global_id);
    let is_constant = global.is_constant;
    let initial_value_id = match global.initial_value {
        None => {
            let eval_start = k1.timing.raw();
            k1.eval_global_body(global.ast_id)?;
            let eval_time = k1.timing.elapsed_nanos(eval_start);
            k1.timing.total_vm_nanos -= eval_time as i64;
            k1.globals.get(global_id).initial_value.unwrap()
        }
        Some(value_id) => value_id,
    };
    let shared_vm_value = vm::static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
    let layout = k1.types.get_pt_layout(t);
    if is_constant {
        let dst = k1.vm_static_stack.push_layout_uninit(layout);
        vm::store_value(&k1.types, t, dst, shared_vm_value);
        let addr = Value::ptr(dst);
        k1.vm_global_constant_lookups.insert(global_id, addr);
        Ok(addr)
    } else {
        let dst = vm.static_stack.push_layout_uninit(layout);
        vm::store_value(&k1.types, t, dst, shared_vm_value);
        let addr = Value::ptr(dst);
        vm.globals.insert(global_id, addr);
        Ok(addr)
    }
}

fn fulfill_return(
    k1: &TypedProgram,
    vm: &mut Vm2,
    frame_index: u32,
    result_slot: u32,
    has_dst: bool,
    ret_type: PhysicalType,
    returned_value: Value,
) {
    if has_dst {
        let addr = vm.stack.get_local_value(frame_index, result_slot).as_ptr();
        vm::store_value(&k1.types, ret_type, addr, returned_value);
    } else {
        vm.stack.set_local_value(frame_index, result_slot, returned_value);
    }
}

#[derive(Clone, Copy)]
struct RetInfo2 {
    pt: PhysicalType,
    frame_index: u32,
    result_slot: Option<u32>,
    has_dst: bool,
    top: bool,
}

impl RetInfo2 {
    fn top(pt: PhysicalType) -> Self {
        Self { pt, frame_index: 0, result_slot: None, has_dst: false, top: true }
    }
}

pub struct Stack2 {
    mem: kmem::Mem<()>,
    frames: Vec<StackFrame2>,
}

#[derive(Clone)]
struct StackFrame2 {
    index: u32,
    base_ptr: *mut u8,
    call_span: Option<SpanId>,
    unit: BcUnit,
    param_count: u32,
    local_count: u32,
    pc: usize,
    prev_block: u32,
    ret_info: RetInfo2,
}

impl Stack2 {
    fn make() -> Self {
        let mut mem = kmem::Mem::make();
        mem.will_need(STACK_SIZE);
        Self { mem, frames: Vec::with_capacity(512) }
    }

    fn reset(&mut self) {
        self.mem.reset(cfg!(debug_assertions));
        self.frames.clear();
    }

    fn push_frame(&mut self, call_span: Option<SpanId>, unit: BcUnit, ret_info: RetInfo2) {
        let index = self.frames.len() as u32;
        self.mem.align_to_bytes(8);
        let base_ptr = self.mem.cursor();
        let param_count = unit.param_count;
        let local_count = unit.local_count;
        self.mem.push_slice_uninit::<Value>((param_count + local_count) as usize);
        self.frames.push(StackFrame2 {
            index,
            base_ptr,
            call_span,
            unit,
            param_count,
            local_count,
            pc: 0,
            prev_block: u32::MAX,
            ret_info,
        });
    }

    fn pop_frame(&mut self) -> StackFrame2 {
        let frame = self.frames.pop().unwrap();
        self.mem.set_cursor(frame.base_ptr);
        frame
    }

    fn current_frame_index(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    fn values_for_frame(&self, frame_index: u32) -> &mut [Value] {
        let frame = &self.frames[frame_index as usize];
        unsafe {
            core::slice::from_raw_parts_mut(
                frame.base_ptr as *mut Value,
                (frame.param_count + frame.local_count) as usize,
            )
        }
    }

    fn get_param_value(&self, frame_index: u32, param_index: u32) -> Value {
        self.values_for_frame(frame_index)[param_index as usize]
    }

    fn set_param_value(&mut self, frame_index: u32, param_index: u32, value: Value) {
        self.values_for_frame(frame_index)[param_index as usize] = value;
    }

    fn get_local_value(&self, frame_index: u32, slot: u32) -> Value {
        let frame = &self.frames[frame_index as usize];
        self.values_for_frame(frame_index)[(frame.param_count + slot) as usize]
    }

    fn set_local_value(&mut self, frame_index: u32, slot: u32, value: Value) {
        let frame = &self.frames[frame_index as usize];
        self.values_for_frame(frame_index)[(frame.param_count + slot) as usize] = value;
    }

    fn push_layout_uninit(&mut self, layout: Layout) -> *mut u8 {
        self.mem.push_layout_uninit(layout.size, layout.align)
    }
}

fn handle_ffi_call(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    frame_index: u32,
    return_pt: PhysicalType,
    args: &[BcValue],
    lib_name: Option<crate::parse::StringId>,
    fn_name: crate::parse::StringId,
    function_id: FunctionId,
) -> K1Result<Value> {
    let nargs = args.len();
    let mut ffi_args_value_storage: Vec<u64> = Vec::with_capacity(nargs);
    let mut ffi_args_value_ptrs: Vec<*mut c_void> = Vec::with_capacity(nargs);

    let fn_type = k1.ir.functions.get(function_id).unwrap().fn_type;
    let function_params = fn_type.params;
    for (arg_value, param) in args.iter().zip(k1.ir.mem.getn(function_params)) {
        let vm_value = resolve_value(k1, vm, frame_index, *arg_value)?;
        if param.pt.is_agg() {
            ffi_args_value_ptrs.push(vm_value.as_ptr() as *mut c_void);
        } else {
            ffi_args_value_storage.push(vm_value.bits());
            let ptr = ffi_args_value_storage.last_mut().unwrap();
            ffi_args_value_ptrs.push(ptr as *mut _ as *mut c_void);
        }
    }

    let mut ffi_handle = match k1.vm_ffi_functions.get(&function_id).copied() {
        None => {
            let handle_for_search = match lib_name {
                None => k1.vm_process_dlopen_handle,
                Some(lib_name) => k1.get_dylib_handle(function_id, lib_name, vm.eval_span)?,
            };
            let name_cstr = std::ffi::CString::new(k1.ident_str(fn_name)).unwrap();
            let fn_ptr: *mut c_void = unsafe { libc::dlsym(handle_for_search, name_cstr.as_ptr()) };
            if fn_ptr.is_null() {
                return failf!(
                    vm.eval_span,
                    "Could not find extern function symbol: {}",
                    k1.ident_str(fn_name)
                );
            }
            let cif = vm_ffi::prep_ffi_cif(k1, fn_type, vm.eval_span)?;
            let handle = vm::VmFfiHandle {
                library_handle: handle_for_search,
                function_pointer: fn_ptr,
                cif,
            };
            k1.vm_ffi_functions.insert(function_id, handle);
            handle
        }
        Some(ffi_handle) => ffi_handle,
    };

    let result_storage = unsafe {
        let ret_size = (*(ffi_handle.cif.rtype)).size;
        let ret_align = (*(ffi_handle.cif.rtype)).alignment;
        vm.stack.push_layout_uninit(Layout { size: ret_size as u32, align: ret_align as u32 })
    };

    unsafe {
        let args = ffi_args_value_ptrs.as_mut_ptr();
        let code_ptr = CodePtr(ffi_handle.function_pointer);
        raw::ffi_call(
            &mut ffi_handle.cif,
            Some(*code_ptr.as_safe_fun()),
            result_storage as *mut c_void,
            args,
        );
    }

    if return_pt.is_empty() {
        Ok(Value(0))
    } else {
        Ok(vm::load_value(return_pt, result_storage.cast_const()))
    }
}

fn int_cmp(lhs: u64, rhs: u64, pred: ir::IntCmpPred, width: u8) -> bool {
    match (width, pred) {
        (8, ir::IntCmpPred::Eq) => (lhs as u8) == (rhs as u8),
        (16, ir::IntCmpPred::Eq) => (lhs as u16) == (rhs as u16),
        (32, ir::IntCmpPred::Eq) => (lhs as u32) == (rhs as u32),
        (64, ir::IntCmpPred::Eq) => lhs == rhs,
        (8, ir::IntCmpPred::Slt) => (lhs as i8) < (rhs as i8),
        (8, ir::IntCmpPred::Sle) => (lhs as i8) <= (rhs as i8),
        (8, ir::IntCmpPred::Sgt) => (lhs as i8) > (rhs as i8),
        (8, ir::IntCmpPred::Sge) => (lhs as i8) >= (rhs as i8),
        (8, ir::IntCmpPred::Ult) => (lhs as u8) < (rhs as u8),
        (8, ir::IntCmpPred::Ule) => (lhs as u8) <= (rhs as u8),
        (8, ir::IntCmpPred::Ugt) => (lhs as u8) > (rhs as u8),
        (8, ir::IntCmpPred::Uge) => (lhs as u8) >= (rhs as u8),
        (16, ir::IntCmpPred::Slt) => (lhs as i16) < (rhs as i16),
        (16, ir::IntCmpPred::Sle) => (lhs as i16) <= (rhs as i16),
        (16, ir::IntCmpPred::Sgt) => (lhs as i16) > (rhs as i16),
        (16, ir::IntCmpPred::Sge) => (lhs as i16) >= (rhs as i16),
        (16, ir::IntCmpPred::Ult) => (lhs as u16) < (rhs as u16),
        (16, ir::IntCmpPred::Ule) => (lhs as u16) <= (rhs as u16),
        (16, ir::IntCmpPred::Ugt) => (lhs as u16) > (rhs as u16),
        (16, ir::IntCmpPred::Uge) => (lhs as u16) >= (rhs as u16),
        (32, ir::IntCmpPred::Slt) => (lhs as i32) < (rhs as i32),
        (32, ir::IntCmpPred::Sle) => (lhs as i32) <= (rhs as i32),
        (32, ir::IntCmpPred::Sgt) => (lhs as i32) > (rhs as i32),
        (32, ir::IntCmpPred::Sge) => (lhs as i32) >= (rhs as i32),
        (32, ir::IntCmpPred::Ult) => (lhs as u32) < (rhs as u32),
        (32, ir::IntCmpPred::Ule) => (lhs as u32) <= (rhs as u32),
        (32, ir::IntCmpPred::Ugt) => (lhs as u32) > (rhs as u32),
        (32, ir::IntCmpPred::Uge) => (lhs as u32) >= (rhs as u32),
        (64, ir::IntCmpPred::Slt) => (lhs as i64) < (rhs as i64),
        (64, ir::IntCmpPred::Sle) => (lhs as i64) <= (rhs as i64),
        (64, ir::IntCmpPred::Sgt) => (lhs as i64) > (rhs as i64),
        (64, ir::IntCmpPred::Sge) => (lhs as i64) >= (rhs as i64),
        (64, ir::IntCmpPred::Ult) => lhs < rhs,
        (64, ir::IntCmpPred::Ule) => lhs <= rhs,
        (64, ir::IntCmpPred::Ugt) => lhs > rhs,
        (64, ir::IntCmpPred::Uge) => lhs >= rhs,
        _ => unreachable!(),
    }
}

pub fn make_stack_trace(k1: &TypedProgram, stack: &Stack2) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    for f in stack.frames.iter() {
        write!(&mut s, "[{:02}] ", f.index).unwrap();
        display_unit_name(&mut s, k1, f.unit.unit_id).unwrap();
        if let Some(span) = f.call_span {
            let (source, line) = k1.get_span_location(span);
            write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap()
        }
        writeln!(&mut s).unwrap();
    }
    s
}

fn display_unit_name(
    w: &mut impl std::fmt::Write,
    k1: &TypedProgram,
    unit: IrUnitId,
) -> std::fmt::Result {
    match unit {
        IrUnitId::Function(function_id) => {
            write!(w, "{}", k1.function_id_to_string(function_id, false))
        }
        IrUnitId::Expr(expr_id) => write!(w, "expr#{}", expr_id.as_u32()),
    }
}

fn report_execution_messages(k1: &mut TypedProgram, vm: &Vm2, span: SpanId) {
    if vm.compiler_messages.is_empty() {
        return;
    }
    let mut formatted_messages = String::new();
    for message in &vm.compiler_messages {
        use std::fmt::Write;
        let msg_str = k1.get_string(message.message);
        let color = match message.level {
            MessageLevel::Info => colored::Color::BrightWhite,
            MessageLevel::Warn => colored::Color::Yellow,
            MessageLevel::Error => colored::Color::Red,
            MessageLevel::Hint => colored::Color::BrightBlue,
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
    k1.report_ext(
        K1Message {
            message: formatted_messages,
            span,
            level: MessageLevel::Info,
            error_kind: ErrorKind::None,
        },
        true,
    );
}

#[track_caller]
fn vm2_crash(k1: &TypedProgram, vm: &Vm2, msg: impl AsRef<str>) -> ! {
    eprintln!("VM2 STACK TRACE\n{}", make_stack_trace(k1, &vm.stack));
    panic!("{}", msg.as_ref())
}
