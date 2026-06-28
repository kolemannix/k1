use std::ffi::c_void;
use std::num::NonZeroU32;

use ahash::HashMapExt;
use colored::Colorize;
use fxhash::FxHashMap;
use libffi::{low::CodePtr, raw};
use log::debug;

use crate::bc::{
    self, BcFunction, BcFunctionId, BcInstKind, BcReturn, BcReturnMode, BcValue, FrameOffset,
    ProgramBc,
};
use crate::ir::{self, IrUnitId, PhysicalFunctionType};
use crate::kmem;
use crate::lex::SpanId;
use crate::parse::StringId;
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

#[derive(Clone)]
struct CompilerMessage {
    level: MessageLevel,
    message: StringId,
    filename: String,
    line: u32,
}

#[derive(Clone, Copy)]
struct DebugFrame {
    unit_id: IrUnitId,
    call_span: Option<SpanId>,
}

pub struct Vm2 {
    globals: FxHashMap<TypedGlobalId, Value>,
    pub stack: Stack2,
    pub static_stack: vm::Stack,
    eval_span: SpanId,
    compiler_messages: Vec<CompilerMessage>,
    current_frame: *mut FrameHeader,
}

impl Vm2 {
    pub fn make() -> Self {
        Self {
            globals: FxHashMap::with_capacity(8192),
            stack: Stack2::make(),
            static_stack: vm::Stack::make(),
            eval_span: SpanId::NONE,
            compiler_messages: Vec::with_capacity(16),
            current_frame: core::ptr::null_mut(),
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
        self.current_frame = core::ptr::null_mut();
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
    k1.compile_all_pending_ir(span)?;
    let reachable_functions = bc::close_ir_for_function_execution(k1, function_id)?;
    let program = bc::compile_function_program(k1, function_id, &reachable_functions)?;
    let entry = program.function_indexes[&function_id];
    execute_program(k1, vm, program, entry, arguments, span)
}

pub fn execute_compiled_expr(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    expr_id: TypedExprId,
) -> K1Result<StaticValueId> {
    let span = k1.exprs.get_span(expr_id);
    k1.compile_all_pending_ir(span)?;
    let reachable_functions = bc::close_ir_for_expr_execution(k1, expr_id)?;
    let program = bc::compile_expr_program(k1, expr_id, &reachable_functions)?;
    let entry = program.expr_indexes[&expr_id];
    execute_program(k1, vm, program, entry, &[], span)
}

fn execute_program(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    mut program: ProgramBc,
    entry: BcFunctionId,
    arguments: &[StaticValueId],
    span: SpanId,
) -> K1Result<StaticValueId> {
    let start = k1.timing.clock.raw();
    vm.eval_span = span;
    debug_assert!(vm.current_frame.is_null());

    let entry_function = program.get_function(entry).clone();
    let ret_addr = if entry_function.return_layout.size == 0 {
        core::ptr::null_mut()
    } else {
        vm.stack.push_layout_uninit(entry_function.return_layout)
    };
    let top_return = BcReturn {
        pt: entry_function.return_pt,
        layout: entry_function.return_layout,
        dst: None,
        mode: return_mode(entry_function.return_pt),
    };
    let header = vm.stack.push_frame(
        core::ptr::null_mut(),
        entry,
        0,
        entry,
        usize::MAX,
        ret_addr,
        top_return,
        &entry_function,
    );
    vm.current_frame = header;
    vm.stack.debug_frames.push(DebugFrame { unit_id: entry_function.unit_id, call_span: Some(span) });

    for (index, arg) in arguments.iter().enumerate() {
        let vm_value = vm::static_value_to_vm_value(k1, *arg, span);
        let param_offset = entry_function.param_offsets[index];
        store_slot(header, param_offset, vm_value);
    }

    let exit_code = exec_loop(k1, vm, &mut program, entry, 0)?;
    let elapsed_nanos = k1.timing.elapsed_nanos(start);
    k1.timing.total_vm_nanos += elapsed_nanos as i64;
    report_execution_messages(k1, vm, span);

    if exit_code != 0 {
        failf!(span, "Static execution exited with code: {}", exit_code)
    } else if entry_function.diverges || entry_function.return_pt.is_empty() {
        Ok(k1.static_values.empty_id())
    } else {
        let loaded = vm::load_value(entry_function.return_pt, ret_addr);
        vm::vm_value_to_static_value(k1, entry_function.result_type_id, loaded, span)
    }
}

fn exec_loop(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    mut function_id: BcFunctionId,
    mut pc: usize,
) -> K1Result<i32> {
    let exit_code = 'exec: loop {
        let function = program.get_function(function_id);
        let inst = function.instrs[pc].clone();
        vm.eval_span = function.spans[pc];
        k1.timing.total_vm_instrs += 1;

        macro_rules! resolve {
            ($v:expr) => {
                resolve_value(k1, vm, program, vm.current_frame, $v)?
            };
        }
        macro_rules! set {
            ($offset:expr, $value:expr) => {
                store_slot(vm.current_frame, $offset, $value)
            };
        }
        macro_rules! next {
            () => {{
                pc += 1;
                continue 'exec;
            }};
        }
        macro_rules! jump {
            ($offset:expr) => {{
                unsafe {
                    (*vm.current_frame).prev_block = inst.block;
                }
                pc = ((pc as isize) + ($offset as isize)) as usize;
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
            BcInstKind::Store { dst, value, t } => {
                let dst = resolve!(dst);
                let value = resolve!(value);
                vm::store_scalar(t, dst.as_ptr(), value);
                next!();
            }
            BcInstKind::Load { dst, t, src } => {
                let src = resolve!(src);
                set!(dst, vm::load_scalar(t, src.as_ptr()));
                next!();
            }
            BcInstKind::Copy { dst, src, size } => {
                let dst = resolve!(dst).as_ptr();
                let src = resolve!(src).as_ptr();
                vm::memmove(src, dst, size as usize);
                next!();
            }
            BcInstKind::StructOffset { dst, base, offset } => {
                let base = resolve!(base).as_ptr();
                let field_ptr = unsafe { base.byte_add(offset as usize) };
                set!(dst, Value::ptr(field_ptr));
                next!();
            }
            BcInstKind::ArrayOffset { dst, element_stride, base, element_index } => {
                let base = resolve!(base).as_ptr();
                let index = resolve!(element_index).bits();
                let ptr = unsafe { base.byte_add(element_stride as usize * index as usize) };
                set!(dst, Value::ptr(ptr));
                next!();
            }
            BcInstKind::CallDirect { result, callee, args, ret, next_pc } => {
                let return_dst = prepare_call_return(k1, vm, program, result, ret)?;
                push_call_frame(k1, vm, program, function_id, next_pc, callee, return_dst, ret, args, vm.eval_span)?;
                function_id = callee;
                pc = 0;
                continue 'exec;
            }
            BcInstKind::CallIndirect { result, callee, args, ret, next_pc } => {
                let function_ref = resolve!(callee).bits();
                let Some(callee) = program.function_for_ref_bits(function_ref) else {
                    return failf!(vm.eval_span, "Indirect call to function not present in bytecode program");
                };
                let return_dst = prepare_call_return(k1, vm, program, result, ret)?;
                push_call_frame(k1, vm, program, function_id, next_pc, callee, return_dst, ret, args, vm.eval_span)?;
                function_id = callee;
                pc = 0;
                continue 'exec;
            }
            BcInstKind::CallExtern {
                result,
                function_id: extern_function_id,
                library_name,
                function_name,
                fn_type,
                param_pts,
                args,
                ret,
                ..
            } => {
                let value = handle_ffi_call(
                    k1,
                    vm,
                    program,
                    ret.pt,
                    &param_pts,
                    &args,
                    library_name,
                    function_name,
                    extern_function_id,
                    fn_type,
                )?;
                finish_inline_return(k1, vm, program, result, ret, value)?;
                next!();
            }
            BcInstKind::CallBuiltin { result, builtin, args, ret, .. } => {
                match builtin {
                    ir::BackendBuiltin::TypeSchema => {
                        let type_id_arg = resolve!(args[0]).bits();
                        let type_id =
                            TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                        let Some(schema_static_value_id) = k1.type_schemas.get(&type_id) else {
                            vm2_ice!(k1, vm, "Missing type schema: {}", k1.type_id_to_string(type_id))
                        };
                        let value =
                            vm::static_value_to_vm_value(k1, *schema_static_value_id, vm.eval_span);
                        finish_inline_return(k1, vm, program, result, ret, value)?;
                    }
                    ir::BackendBuiltin::TypeName => {
                        let type_id_arg = resolve!(args[0]).bits();
                        let type_id =
                            TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
                        let name_value_id = *k1.type_names.get(&type_id).unwrap();
                        let value = vm::static_value_to_vm_value(k1, name_value_id, vm.eval_span);
                        finish_inline_return(k1, vm, program, result, ret, value)?;
                    }
                    ir::BackendBuiltin::Allocate | ir::BackendBuiltin::AllocateZeroed => {
                        let zero = builtin == ir::BackendBuiltin::AllocateZeroed;
                        let size = resolve!(args[0]).as_usize();
                        let align = resolve!(args[1]).as_usize();
                        let Ok(layout) = std::alloc::Layout::from_size_align(size, align) else {
                            vm2_crash(k1, vm, format!("Bad allocation layout: size={size}, align={align}"))
                        };
                        let ptr = if zero {
                            unsafe { std::alloc::alloc_zeroed(layout) }
                        } else {
                            unsafe { std::alloc::alloc(layout) }
                        };
                        finish_inline_return(k1, vm, program, result, ret, Value::ptr(ptr))?;
                    }
                    ir::BackendBuiltin::Reallocate => {
                        let old_ptr = resolve!(args[0]).as_ptr();
                        let old_size = resolve!(args[1]).as_usize();
                        let align = resolve!(args[2]).as_usize();
                        let new_size = resolve!(args[3]).as_usize();
                        let layout = std::alloc::Layout::from_size_align(old_size, align).unwrap();
                        let ptr = unsafe { std::alloc::realloc(old_ptr, layout, new_size) };
                        finish_inline_return(k1, vm, program, result, ret, Value::ptr(ptr))?;
                    }
                    ir::BackendBuiltin::Free => {
                        let ptr = resolve!(args[0]).as_ptr();
                        let size = resolve!(args[1]).as_usize();
                        let align = resolve!(args[2]).as_usize();
                        let layout = std::alloc::Layout::from_size_align(size, align).unwrap();
                        unsafe { std::alloc::dealloc(ptr, layout) };
                        finish_inline_return(k1, vm, program, result, ret, Value(0))?;
                    }
                    ir::BackendBuiltin::MemCopy | ir::BackendBuiltin::MemMove => {
                        let dst = resolve!(args[0]).as_ptr();
                        let src = resolve!(args[1]).as_ptr();
                        let count = resolve!(args[2]).as_usize();
                        if builtin == ir::BackendBuiltin::MemCopy {
                            vm::memcopy(src, dst, count);
                        } else {
                            vm::memmove(src, dst, count);
                        }
                        finish_inline_return(k1, vm, program, result, ret, Value(0))?;
                    }
                    ir::BackendBuiltin::MemSet => {
                        let dst = resolve!(args[0]).as_ptr();
                        let value = resolve!(args[1]).bits() as u8;
                        let count = resolve!(args[2]).as_usize();
                        unsafe { std::ptr::write_bytes(dst, value, count) };
                        finish_inline_return(k1, vm, program, result, ret, Value(0))?;
                    }
                    ir::BackendBuiltin::MemEquals => {
                        let p1 = resolve!(args[0]).as_ptr();
                        let p2 = resolve!(args[1]).as_ptr();
                        let size = resolve!(args[2]).as_usize();
                        let p1_slice = unsafe { std::slice::from_raw_parts(p1, size) };
                        let p2_slice = unsafe { std::slice::from_raw_parts(p2, size) };
                        finish_inline_return(k1, vm, program, result, ret, Value::bool(p1_slice == p2_slice))?;
                    }
                    ir::BackendBuiltin::Exit => {
                        break 'exec resolve!(args[0]).bits() as i32;
                    }
                    ir::BackendBuiltin::CompilerMessage => {
                        let location_arg = resolve!(args[0]);
                        let level_arg = resolve!(args[1]);
                        let message_arg = resolve!(args[2]);
                        let location = unsafe {
                            (location_arg.as_ptr() as *const k1_types::K1SourceLocation).read()
                        };
                        let level = match k1_types::CompilerMessageLevel::from_u8(level_arg.as_u8())
                            .unwrap_or(k1_types::CompilerMessageLevel::Info)
                        {
                            k1_types::CompilerMessageLevel::Info => MessageLevel::Info,
                            k1_types::CompilerMessageLevel::Warn => MessageLevel::Warn,
                            k1_types::CompilerMessageLevel::Error => MessageLevel::Error,
                        };
                        let message = vm::value_to_string_id(k1, message_arg).map_err(|msg| {
                            errf!(vm.eval_span, "Bad message string passed to EmitCompilerMessage: {msg}")
                        })?;
                        let filename = unsafe { location.filename.to_str() }.map_err(|msg| {
                            errf!(vm.eval_span, "Bad filename string passed to EmitCompilerMessage: {msg}")
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
                        finish_inline_return(k1, vm, program, result, ret, Value(0))?;
                    }
                }
                next!();
            }
            BcInstKind::Jump { offset } => jump!(offset),
            BcInstKind::JumpIf { cond, cons_offset, alt_offset } => {
                if resolve!(cond).as_bool() {
                    jump!(cons_offset)
                } else {
                    jump!(alt_offset)
                }
            }
            BcInstKind::Unreachable => return failf!(vm.eval_span, "Reached unreachable instruction"),
            BcInstKind::Phi { dst, size, incomings } => {
                let prev_block = unsafe { (*vm.current_frame).prev_block };
                let Some((_, value)) = incomings.iter().find(|(from, _)| *from == prev_block)
                else {
                    return failf!(vm.eval_span, "No phi incoming for predecessor block {}", prev_block);
                };
                let value = resolve!(*value);
                match dst {
                    BcValue::Slot(offset) => set!(offset, value),
                    BcValue::Address(offset) => {
                        vm::memmove(value.as_ptr(), frame_ptr(vm.current_frame, offset), size as usize);
                    }
                    BcValue::Empty => {}
                    _ => return failf!(vm.eval_span, "Unsupported phi destination"),
                }
                next!();
            }
            BcInstKind::Ret { v } => {
                let returned_value = resolve!(v);
                let header = unsafe { &*vm.current_frame };
                if header.prev.is_null() {
                    write_return_value(header.return_dst, header.return_mode, header.return_size, returned_value);
                    let old = vm.current_frame;
                    vm.current_frame = core::ptr::null_mut();
                    vm.stack.pop_frame(old);
                    break 'exec 0;
                } else {
                    write_return_value(header.return_dst, header.return_mode, header.return_size, returned_value);
                    let old = vm.current_frame;
                    function_id = BcFunctionId::from_u32(header.caller_function).unwrap();
                    pc = header.return_pc;
                    vm.current_frame = header.prev;
                    vm.stack.pop_frame(old);
                    continue 'exec;
                }
            }
            BcInstKind::BoolNegate { dst, v } => {
                set!(dst, Value::bool(!resolve!(v).as_bool()));
                next!();
            }
            BcInstKind::BitNot { dst, v } => {
                set!(dst, Value(!resolve!(v).bits()));
                next!();
            }
            BcInstKind::BitCast { dst, v }
            | BcInstKind::PtrToWord { dst, v }
            | BcInstKind::WordToPtr { dst, v } => {
                set!(dst, resolve!(v));
                next!();
            }
            BcInstKind::IntTrunc { dst, v, to } => {
                set!(dst, resolve!(v).truncated(to.width()));
                next!();
            }
            BcInstKind::IntExtU { dst, v } => {
                set!(dst, resolve!(v));
                next!();
            }
            BcInstKind::IntExtS { dst, v, from, to } => {
                set!(dst, resolve!(v).sign_extended(from.width(), to.width()));
                next!();
            }
            BcInstKind::FloatTrunc { dst, v } => {
                set!(dst, Value::f32(resolve!(v).as_f64() as f32));
                next!();
            }
            BcInstKind::FloatExt { dst, v } => {
                set!(dst, Value::f64(resolve!(v).as_f32() as f64));
                next!();
            }
            BcInstKind::Float32ToIntUnsigned { dst, v, to } => {
                let f = resolve!(v).as_f32();
                set!(dst, match to {
                    ScalarType::U8 => Value::u8(f as u8),
                    ScalarType::U16 => Value::u16(f as u16),
                    ScalarType::U32 => Value::u32(f as u32),
                    ScalarType::U64 => Value::u64(f as u64),
                    _ => unreachable!(),
                });
                next!();
            }
            BcInstKind::Float32ToIntSigned { dst, v, to } => {
                let f = resolve!(v).as_f32();
                set!(dst, match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                });
                next!();
            }
            BcInstKind::Float64ToIntUnsigned { dst, v, to } => {
                let f = resolve!(v).as_f64();
                set!(dst, match to {
                    ScalarType::U8 => Value::u8(f as u8),
                    ScalarType::U16 => Value::u16(f as u16),
                    ScalarType::U32 => Value::u32(f as u32),
                    ScalarType::U64 => Value::u64(f as u64),
                    _ => unreachable!(),
                });
                next!();
            }
            BcInstKind::Float64ToIntSigned { dst, v, to } => {
                let f = resolve!(v).as_f64();
                set!(dst, match to {
                    ScalarType::I8 => Value::i8(f as i8),
                    ScalarType::I16 => Value::i16(f as i16),
                    ScalarType::I32 => Value::i32(f as i32),
                    ScalarType::I64 => Value::i64(f as i64),
                    _ => unreachable!(),
                });
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
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, wrapping_add, lhs, rhs)));
                next!();
            }
            BcInstKind::IntSub { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, wrapping_sub, lhs, rhs)));
                next!();
            }
            BcInstKind::IntMul { dst, lhs, rhs, width } => {
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, wrapping_mul, lhs, rhs)));
                next!();
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
                set!(dst, Value::bool(int_cmp(lhs, rhs, pred, width)));
                next!();
            }
            BcInstKind::FloatAdd { dst, lhs, rhs, width } => {
                use std::ops::Add;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_float_op!(width, add, lhs, rhs)));
                next!();
            }
            BcInstKind::FloatSub { dst, lhs, rhs, width } => {
                use std::ops::Sub;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_float_op!(width, sub, lhs, rhs)));
                next!();
            }
            BcInstKind::FloatMul { dst, lhs, rhs, width } => {
                use std::ops::Mul;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_float_op!(width, mul, lhs, rhs)));
                next!();
            }
            BcInstKind::FloatDiv { dst, lhs, rhs, width } => {
                use std::ops::Div;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_float_op!(width, div, lhs, rhs)));
                next!();
            }
            BcInstKind::FloatRem { dst, lhs, rhs, width } => {
                use std::ops::Rem;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_float_op!(width, rem, lhs, rhs)));
                next!();
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
                use std::ops::BitAnd;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, bitand, lhs, rhs)));
                next!();
            }
            BcInstKind::BitOr { dst, lhs, rhs, width } => {
                use std::ops::BitOr;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, bitor, lhs, rhs)));
                next!();
            }
            BcInstKind::BitXor { dst, lhs, rhs, width } => {
                use std::ops::BitXor;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).bits();
                set!(dst, Value(casted_uop!(width, bitxor, lhs, rhs)));
                next!();
            }
            BcInstKind::BitShiftLeft { dst, lhs, rhs, width } => {
                use std::ops::Shl;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
                set!(dst, Value(casted_uop!(width, shl, lhs, rhs)));
                next!();
            }
            BcInstKind::BitUnsignedShiftRight { dst, lhs, rhs, width } => {
                use std::ops::Shr;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
                set!(dst, Value(casted_uop!(width, shr, lhs, rhs)));
                next!();
            }
            BcInstKind::BitSignedShiftRight { dst, lhs, rhs, width } => {
                use std::ops::Shr;
                let lhs = resolve!(lhs).bits();
                let rhs = resolve!(rhs).as_u32();
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

fn push_call_frame(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    caller_function: BcFunctionId,
    return_pc: usize,
    callee: BcFunctionId,
    return_dst: *mut u8,
    ret: BcReturn,
    args: Vec<BcValue>,
    span: SpanId,
) -> K1Result<()> {
    let old_frame = vm.current_frame;
    let mut arg_values = Vec::with_capacity(args.len());
    for arg in args {
        arg_values.push(resolve_value(k1, vm, program, old_frame, arg)?);
    }
    let callee_fn = program.get_function(callee);
    let header = vm.stack.push_frame(
        old_frame,
        callee,
        caller_function.as_u32(),
        callee,
        return_pc,
        return_dst,
        ret,
        callee_fn,
    );
    for (index, value) in arg_values.into_iter().enumerate() {
        store_slot(header, callee_fn.param_offsets[index], value);
    }
    vm.current_frame = header;
    vm.stack.debug_frames.push(DebugFrame { unit_id: callee_fn.unit_id, call_span: Some(span) });
    Ok(())
}

fn prepare_call_return(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    result: Option<FrameOffset>,
    ret: BcReturn,
) -> K1Result<*mut u8> {
    let explicit_dst = |k1: &mut TypedProgram, vm: &mut Vm2, program: &mut ProgramBc, dst: BcValue| {
        if let (Some(result), Some(dst_slot)) = (result, dst.as_slot()) {
            if result == dst_slot {
                return Ok(frame_ptr(vm.current_frame, result));
            }
        }
        value_to_ptr(k1, vm, program, vm.current_frame, dst)
    };
    let dst = match ret.mode {
        BcReturnMode::None => core::ptr::null_mut(),
        BcReturnMode::Scalar => match ret.dst {
            Some(dst) => explicit_dst(k1, vm, program, dst)?,
            None => frame_ptr(vm.current_frame, result.unwrap()),
        },
        BcReturnMode::InMemory => match ret.dst {
            Some(dst) => explicit_dst(k1, vm, program, dst)?,
            None => frame_ptr(vm.current_frame, result.unwrap()),
        },
    };
    if let (Some(result), Some(dst_value)) = (result, ret.dst) {
        if dst_value.as_slot() != Some(result) {
            store_slot(vm.current_frame, result, Value::ptr(dst));
        }
    }
    Ok(dst)
}

fn finish_inline_return(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    result: Option<FrameOffset>,
    ret: BcReturn,
    returned_value: Value,
) -> K1Result<()> {
    let dst = prepare_call_return(k1, vm, program, result, ret)?;
    write_return_value(dst, ret.mode, ret.layout.size, returned_value);
    Ok(())
}

fn write_return_value(
    dst: *mut u8,
    mode: BcReturnMode,
    size: u32,
    returned_value: Value,
) {
    match mode {
        BcReturnMode::None => {}
        BcReturnMode::Scalar => store_slot_ptr(dst, returned_value),
        BcReturnMode::InMemory => {
            vm::memmove(returned_value.as_ptr(), dst, size as usize);
        }
    }
}

#[inline(always)]
fn resolve_value(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    _program: &mut ProgramBc,
    frame: *mut FrameHeader,
    value: BcValue,
) -> K1Result<Value> {
    match value {
        BcValue::Slot(offset) => Ok(load_slot(frame, offset)),
        BcValue::Address(offset) => Ok(Value::ptr(frame_ptr(frame, offset))),
        BcValue::StaticValue { id, .. } => Ok(vm::static_value_to_vm_value(k1, id, vm.eval_span)),
        BcValue::FunctionAddr(function_id) => Ok(vm::function_id_to_ref_value(function_id)),
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
        BcValue::GlobalAddr { storage_pt, layout, id } => resolve_global(k1, vm, id, storage_pt, layout),
        BcValue::Empty => Ok(Value(0)),
    }
}

fn value_to_ptr(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    frame: *mut FrameHeader,
    value: BcValue,
) -> K1Result<*mut u8> {
    Ok(resolve_value(k1, vm, program, frame, value)?.as_ptr())
}

fn resolve_global(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    global_id: TypedGlobalId,
    t: PhysicalType,
    layout: Layout,
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
            k1.compile_all_pending_ir(vm.eval_span)?;
            let eval_time = k1.timing.elapsed_nanos(eval_start);
            k1.timing.total_vm_nanos -= eval_time as i64;
            k1.globals.get(global_id).initial_value.unwrap()
        }
        Some(value_id) => value_id,
    };
    let shared_vm_value = vm::static_value_to_vm_value(k1, initial_value_id, vm.eval_span);
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

fn handle_ffi_call(
    k1: &mut TypedProgram,
    vm: &mut Vm2,
    program: &mut ProgramBc,
    return_pt: PhysicalType,
    param_pts: &[PhysicalType],
    args: &[BcValue],
    lib_name: Option<StringId>,
    fn_name: StringId,
    function_id: FunctionId,
    fn_type: PhysicalFunctionType,
) -> K1Result<Value> {
    let nargs = args.len();
    let mut ffi_args_value_storage: Vec<u64> = Vec::with_capacity(nargs);
    let mut ffi_args_value_ptrs: Vec<*mut c_void> = Vec::with_capacity(nargs);

    for (arg_value, param_pt) in args.iter().zip(param_pts.iter()) {
        let vm_value = resolve_value(k1, vm, program, vm.current_frame, *arg_value)?;
        if param_pt.is_agg() {
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

#[repr(C)]
struct FrameHeader {
    prev: *mut FrameHeader,
    payload_base: *mut u8,
    caller_function: u32,
    current_function: u32,
    return_pc: usize,
    return_dst: *mut u8,
    return_mode: BcReturnMode,
    return_size: u32,
    prev_block: u32,
}

pub struct Stack2 {
    mem: kmem::Mem<()>,
    debug_frames: Vec<DebugFrame>,
}

impl Stack2 {
    fn make() -> Self {
        let mut mem = kmem::Mem::make();
        mem.will_need(STACK_SIZE);
        Self { mem, debug_frames: Vec::with_capacity(512) }
    }

    fn reset(&mut self) {
        self.mem.reset(cfg!(debug_assertions));
        self.debug_frames.clear();
    }

    fn push_frame(
        &mut self,
        prev: *mut FrameHeader,
        current: BcFunctionId,
        caller_function: u32,
        current_function: BcFunctionId,
        return_pc: usize,
        return_dst: *mut u8,
        ret: BcReturn,
        function: &BcFunction,
    ) -> *mut FrameHeader {
        self.mem.align_to_bytes(std::mem::align_of::<FrameHeader>());
        let header = self.mem.push(FrameHeader {
            prev,
            payload_base: core::ptr::null_mut(),
            caller_function,
            current_function: current_function.as_u32(),
            return_pc,
            return_dst,
            return_mode: ret.mode,
            return_size: ret.layout.size,
            prev_block: u32::MAX,
        }) as *mut FrameHeader;
        debug_assert_eq!(current.as_u32(), current_function.as_u32());
        self.mem.align_to_bytes(function.frame_align as usize);
        let payload_base = self.mem.push_layout_uninit(function.frame_size, 1);
        unsafe { (*header).payload_base = payload_base };
        header
    }

    fn pop_frame(&mut self, header: *mut FrameHeader) {
        self.debug_frames.pop();
        self.mem.set_cursor(header.cast());
    }

    fn push_layout_uninit(&mut self, layout: Layout) -> *mut u8 {
        self.mem.push_layout_uninit(layout.size, layout.align)
    }
}

fn return_mode(pt: PhysicalType) -> BcReturnMode {
    if pt.is_empty() {
        BcReturnMode::None
    } else if pt.is_agg() {
        BcReturnMode::InMemory
    } else {
        BcReturnMode::Scalar
    }
}

fn frame_ptr(frame: *mut FrameHeader, offset: FrameOffset) -> *mut u8 {
    unsafe { (*frame).payload_base.byte_add(offset.0 as usize) }
}

fn load_slot(frame: *mut FrameHeader, offset: FrameOffset) -> Value {
    unsafe { (frame_ptr(frame, offset) as *const Value).read() }
}

fn store_slot(frame: *mut FrameHeader, offset: FrameOffset, value: Value) {
    store_slot_ptr(frame_ptr(frame, offset), value)
}

fn store_slot_ptr(dst: *mut u8, value: Value) {
    unsafe { (dst as *mut Value).write(value) };
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
    for (index, f) in stack.debug_frames.iter().enumerate() {
        write!(&mut s, "[{:02}] ", index).unwrap();
        display_unit_name(&mut s, k1, f.unit_id).unwrap();
        if let Some(span) = f.call_span {
            let (source, line) = k1.get_span_location(span);
            write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap()
        }
        writeln!(&mut s).unwrap();
    }
    s
}

fn make_active_stack_trace(k1: &TypedProgram, vm: &Vm2) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    for (index, f) in vm.stack.debug_frames.iter().enumerate() {
        write!(&mut s, "[{:02}] ", index).unwrap();
        display_unit_name(&mut s, k1, f.unit_id).unwrap();
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
    eprintln!("VM2 STACK TRACE\n{}", make_active_stack_trace(k1, vm));
    panic!("{}", msg.as_ref())
}
