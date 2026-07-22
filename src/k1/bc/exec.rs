// Copyright (c) 2026 knix
// All rights reserved.

//! The bc VM: executes the flat bytecode stream.
//!
//! Runtime state is `pc` (index into `k1.bc.code`), `fp` (current frame base
//! pointer into the VM stack), and `ret_reg` (the scalar return value
//! register). calls bump `fp` by callee frame size and write a 2-word
//! link (caller_fp, return_pc) into the new frame's header; returns read it
//! back. `code[0]` is `Halt`, so popping the top frame (return_pc 0) lands
//! there and ends execution.
//!
//! Reentrancy: `LoadGlobal` (lazy global evaluation -> nested typechecking ->
//! nested static execution) and the `CallIndirect` cold path can lower more
//! units, growing `k1.bc.code`/`consts`. We therefore never cache a borrow
//! or pointer into either; every fetch re-indexes through `k1.bc`.

use std::num::NonZeroU32;

use crate::failf;
use crate::ir::{self, BackendBuiltin, IrUnitId};
use crate::lex::SpanId;
use crate::typer::types::{PhysicalType, TypeId};
use crate::typer::{FunctionId, K1Result, StaticValueId, TypedExprId, TypedGlobalId, TypedProgram};
use crate::vm::{
    self, Value, Vm, casted_float_op, casted_iop, casted_uop, load_value, store_value,
};

use super::lower;
use super::{
    CastKind, Opcode, SRC_CONST_BIT, UnitKind, builtin_from_tag, float_pred_from_tag, header_a,
    header_b, header_op, int_pred_from_tag,
};

pub fn execute_compiled_expr(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    expr_id: TypedExprId,
    report_messages: bool,
) -> K1Result<StaticValueId> {
    let span = k1.exprs.get_span(expr_id);
    execute_compiled_unit(k1, vm, IrUnitId::Expr(expr_id), &[], span, report_messages)
}

pub fn execute_compiled_function(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    function_id: FunctionId,
    arguments: &[StaticValueId],
    report_messages: bool,
) -> K1Result<StaticValueId> {
    let span = k1.get_function_span(function_id);
    execute_compiled_unit(k1, vm, IrUnitId::Function(function_id), arguments, span, report_messages)
}

pub fn execute_compiled_unit(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    unit_id: IrUnitId,
    arguments: &[StaticValueId],
    span: SpanId,
    report_messages: bool,
) -> K1Result<StaticValueId> {
    vm.eval_span = span;
    vm.bc_fault = None;

    let bcgen_start = k1.timing.clock.raw();
    let info = lower::get_or_lower_unit(k1, unit_id, span)?;
    k1.timing.total_bcgen_nanos += k1.timing.elapsed_nanos(bcgen_start) as i64;
    if info.kind != UnitKind::Body {
        return failf!(span, "Cannot execute a bodyless ({:?}) unit", info.kind);
    }
    let start = k1.timing.clock.raw();
    let ir_unit = ir::get_compiled_unit(&k1.ir, unit_id).unwrap();
    let result_type_id = ir_unit.result_type_id;
    let ret_pt = info.ret_pt;

    // Return-value storage lives just below the first frame
    let ret_layout = k1.types.get_pt_layout(ret_pt);
    let ret_addr = vm.stack.push_layout_uninit(ret_layout);
    vm.overall_return_addr = ret_addr;

    // Top frame: 16-aligned base, header links to Halt (pc 0, fp 0)
    let fp0: *mut u8 = vm.stack.mem.cursor().map_addr(|a| (a + 15) & !15);
    vm.stack.mem.set_cursor(fp0);
    unsafe {
        let words = fp0 as *mut u64;
        words.write(0); // caller_fp: none
        words.add(1).write(0); // return_pc: Halt
        words.add(2).write(ret_addr as u64); // sret for aggregate returns
        debug_assert_eq!(arguments.len(), info.param_count as usize);
        for (i, arg) in arguments.iter().enumerate() {
            let vm_value = vm::static_value_to_vm_value(k1, *arg, span);
            words.add(super::FRAME_HEADER_WORDS as usize + i).write(vm_value.bits());
        }
    }

    let exec_result = exec_loop(k1, vm, info.code_start, fp0, ret_pt);

    let elapsed_nanos = k1.timing.elapsed_nanos(start);
    k1.timing.total_vm_nanos += elapsed_nanos as i64;

    let exit_code = match exec_result {
        Ok(exit_code) => exit_code,
        Err(mut e) => {
            if let Some((fault_fp, fault_pc)) = vm.bc_fault {
                let trace = make_stack_trace(k1, fault_fp as *const u8, fault_pc);
                e.message = format!("{}\nbc Execution Trace\n{}", e.message, trace);
            }
            return Err(e);
        }
    };

    if report_messages {
        vm::report_execution_messages(k1, vm, span, exit_code);
    }

    let result = if exit_code != 0 {
        failf!(span, "Static execution exited with code: {}", exit_code)
    } else if info.diverges || ret_pt.is_empty() {
        Ok(k1.static_values.empty_id())
    } else {
        let loaded = load_value(ret_pt, ret_addr);
        vm::vm_value_to_static_value(k1, result_type_id, loaded, span)
    };
    vm.overall_return_addr = core::ptr::null_mut();
    result
}

/// Walk the caller_fp chain, naming each frame's unit via the pc range table.
pub fn make_stack_trace(k1: &TypedProgram, fault_fp: *const u8, fault_pc: u32) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    let mut fp = fault_fp;
    let mut pc = fault_pc;
    let mut depth = 0;
    while !fp.is_null() && depth < 256 {
        write!(&mut s, "[{:02}] ", depth).unwrap();
        match k1.bc.unit_for_pc(pc) {
            Some(unit_id) => {
                let _ = ir::display_unit_name(&mut s, k1, unit_id);
            }
            None => {
                write!(&mut s, "<unknown pc {}>", pc).unwrap();
            }
        }
        let span = k1.bc.span_for_pc(pc);
        if span != SpanId::NONE {
            let (source, line) = k1.get_span_location(span);
            write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap();
        }
        writeln!(&mut s).unwrap();
        let (caller_fp, ret_pc) =
            unsafe { ((*(fp as *const u64)) as *const u8, (*(fp as *const u64).add(1)) as u32) };
        if ret_pc == 0 {
            break;
        }
        fp = caller_fp;
        pc = ret_pc;
        depth += 1;
    }
    s
}

enum BuiltinOutcome {
    Value(Value),
    Empty,
    Exit(i32),
}

// Scalar memory ops carry their width in bits (8/16/32/64) in `header_a`
#[inline]
fn store_bits(width_bits: u8, dst: *mut u8, v: Value) {
    debug_assert!(!dst.is_null(), "store_bits to null (width {})", width_bits);
    unsafe {
        match width_bits {
            8 => dst.write(v.bits() as u8),
            16 => (dst as *mut u16).write(v.bits() as u16),
            32 => (dst as *mut u32).write(v.bits() as u32),
            _ => (dst as *mut u64).write(v.bits()),
        }
    }
}

#[inline]
fn load_bits(width_bits: u8, src: *const u8) -> Value {
    debug_assert!(!src.is_null(), "load_bits from null (width {})", width_bits);
    unsafe {
        match width_bits {
            8 => Value::u8(src.read()),
            16 => Value::u16((src as *const u16).read()),
            32 => Value::u32((src as *const u32).read()),
            _ => Value::u64((src as *const u64).read()),
        }
    }
}

fn exec_loop(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    start_pc: u32,
    top_fp: *mut u8,
    top_ret_pt: PhysicalType,
) -> K1Result<i32> {
    let mut pc: usize = start_pc as usize;
    let mut fp: *mut u8 = top_fp;
    let mut ret_reg: Value = Value::u64(0);
    let mut instrs_run = 0;

    // Callers wrap uses in `unsafe`; pointer arithmetic + deref together
    macro_rules! word_ptr {
        ($w:expr) => {
            (fp as *mut u64).add($w as usize)
        };
    }
    // Indices are in-bounds by construction of the lowering; skip the bounds
    // checks in release builds (this is the hottest path in the VM).
    macro_rules! code_at {
        ($i:expr) => {{
            let i: usize = $i;
            if cfg!(debug_assertions) {
                k1.bc.code[i]
            } else {
                unsafe { *k1.bc.code.get_unchecked(i) }
            }
        }};
    }
    // Tagged src operand: frame word or constant pool entry
    macro_rules! read_src {
        ($w:expr) => {{
            let w: u32 = $w;
            if w & SRC_CONST_BIT != 0 {
                let idx = (w & !SRC_CONST_BIT) as usize;
                if cfg!(debug_assertions) {
                    Value::u64(k1.bc.consts[idx])
                } else {
                    Value::u64(unsafe { *k1.bc.consts.get_unchecked(idx) })
                }
            } else {
                Value::u64(unsafe { *word_ptr!(w) })
            }
        }};
    }
    macro_rules! write_slot {
        ($w:expr, $v:expr) => {{
            // Evaluate operands outside the unsafe block (they may expand to
            // their own unsafe, e.g. unchecked code reads)
            let w: u32 = $w;
            let v: Value = $v;
            unsafe { *word_ptr!(w) = v.bits() }
        }};
    }
    macro_rules! operand {
        ($i:expr) => {
            code_at!(pc + 1 + $i)
        };
    }
    macro_rules! advance {
        ($op:path) => {
            pc += const { 1 + $op.operand_count() }
        };
    }
    // Error return, recording the fault location for stack traces
    macro_rules! vmerr {
        ($($args:expr),*) => {{
            vm.bc_fault = Some((fp as u64, pc as u32));
            return failf!(k1.bc.span_for_pc(pc as u32), $($args),*);
        }};
    }
    // `?` with fault recording
    macro_rules! vmtry {
        ($e:expr) => {
            match $e {
                Ok(v) => v,
                Err(err) => {
                    vm.bc_fault = Some((fp as u64, pc as u32));
                    return Err(err);
                }
            }
        };
    }
    macro_rules! pop_frame {
        () => {{
            let words = fp as *const u64;
            let (caller_fp, ret_pc) = unsafe { (*words, *words.add(1)) };
            fp = caller_fp as *mut u8;
            pc = ret_pc as usize;
        }};
    }

    loop {
        debug_assert!(pc < k1.bc.code.len(), "bc pc out of bounds");
        let h = code_at!(pc);
        instrs_run += 1;
        let op = Opcode::from_u8(header_op(h));

        match op {
            Opcode::Halt => {
                // Aggregate results were already copied through the top
                // frame's sret; scalars ride ret_reg.
                if !top_ret_pt.is_empty() && !top_ret_pt.is_agg() {
                    store_value(&k1.types, top_ret_pt, vm.overall_return_addr, ret_reg);
                }
                k1.timing.total_vm_instrs += instrs_run;
                return Ok(0);
            }
            Opcode::Enter => {
                let frame_bytes = operand!(0) as usize;
                let end = vm.stack.mem.end_ptr().addr();
                if fp.addr() + frame_bytes + 4096 > end {
                    vmerr!("Comptime stack overflow (frame of {} bytes)", frame_bytes);
                }
                advance!(Opcode::Enter);
            }
            Opcode::Jump => {
                pc = operand!(0) as usize;
            }
            Opcode::JumpIf => {
                let cond = read_src!(operand!(0));
                let target = if cond.as_bool() { operand!(1) } else { operand!(2) };
                pc = target as usize;
            }
            Opcode::Unreachable => {
                vmerr!("Reached unreachable instruction");
            }
            Opcode::Ret => {
                // Covers void returns too: their src is const 0 and no one
                // reads ret_reg for them.
                ret_reg = read_src!(operand!(0));
                pop_frame!();
            }
            Opcode::RetAgg => {
                let src = read_src!(operand!(0));
                let size = operand!(1) as usize;
                let sret = unsafe { *(fp as *const u64).add(2) } as *mut u8;
                vm::memmove(src.as_ptr(), sret, size);
                pop_frame!();
            }
            Opcode::Call => {
                let target = operand!(0);
                let fp_delta = operand!(1) as usize;
                debug_assert_ne!(target, super::PENDING_PC, "unpatched recursive call target");
                let new_fp = unsafe { fp.add(fp_delta) };
                unsafe {
                    let words = new_fp as *mut u64;
                    words.write(fp as u64);
                    words.add(1).write((pc + 3) as u64);
                }
                fp = new_fp;
                pc = target as usize;
            }
            Opcode::CallIndirect => {
                let fn_value = read_src!(operand!(0));
                let fp_delta = operand!(1) as usize;
                let fid_u32 = fn_value.bits() as u32;
                let Some(function_id) = FunctionId::from_u32(fid_u32) else {
                    vmerr!("Indirect call to null function pointer");
                };
                let info = match k1.bc.functions.get(&function_id) {
                    Some(info) => *info,
                    None => {
                        eprintln!(
                            "[bc] UNEXPECTED: lazily lowering indirect-call target at runtime: {}",
                            k1.function_id_to_string(function_id, false)
                        );
                        vm.eval_span = k1.bc.span_for_pc(pc as u32);
                        let bcgen_start = k1.timing.clock.raw();
                        let info =
                            vmtry!(lower::get_or_lower_function(k1, function_id, vm.eval_span));
                        let elapsed = k1.timing.elapsed_nanos(bcgen_start) as i64;
                        k1.timing.total_bcgen_nanos += elapsed;
                        k1.timing.total_vm_nanos -= elapsed;
                        info
                    }
                };
                if info.kind != UnitKind::Body {
                    vmerr!(
                        "Indirect call to bodyless ({:?}) function: {}",
                        info.kind,
                        k1.function_id_to_string(function_id, false)
                    );
                }
                let new_fp = unsafe { fp.add(fp_delta) };
                unsafe {
                    let words = new_fp as *mut u64;
                    words.write(fp as u64);
                    words.add(1).write((pc + 3) as u64);
                }
                fp = new_fp;
                pc = info.code_start as usize;
            }
            Opcode::CallExtern => {
                let function_id = FunctionId::from_u32(operand!(0)).unwrap();
                // StringId operands are biased by 1; 0 means "none"
                let lib_name = match operand!(1) {
                    0 => None,
                    biased => Some(crate::parse::StringId::from_usize(biased as usize - 1)),
                };
                let fn_name = crate::parse::StringId::from_usize(operand!(2) as usize - 1);
                let ret_pt = PhysicalType::from_u32(operand!(3));
                let fp_delta = operand!(4) as usize;
                let nargs = operand!(5) as usize;
                vm.eval_span = k1.bc.span_for_pc(pc as u32);

                let new_fp = unsafe { fp.add(fp_delta) };
                let args: &[Value] = unsafe {
                    core::slice::from_raw_parts(
                        new_fp.add(super::FRAME_HEADER_WORDS as usize * 8) as *const Value,
                        nargs,
                    )
                };
                // Route libffi marshalling scratch above all live frames
                unsafe {
                    let scratch_start =
                        new_fp.add(super::FRAME_HEADER_WORDS as usize * 8 + nargs * 8);
                    vm.stack.mem.set_cursor(scratch_start);
                }
                // Aggregate returns: libffi writes straight into our sret
                // Scalars come back in the value.
                let ret_dst: Option<*mut u8> = if ret_pt.is_agg() {
                    Some(unsafe { *(new_fp as *const u64).add(2) } as *mut u8)
                } else {
                    None
                };
                let result = vmtry!(vm::vm_ffi::handle_ffi_call_resolved(
                    k1,
                    vm,
                    ret_pt,
                    args,
                    lib_name,
                    fn_name,
                    function_id,
                    ret_dst
                ));
                if !ret_pt.is_agg() {
                    ret_reg = result;
                }
                advance!(Opcode::CallExtern);
            }
            Opcode::CallBuiltin => {
                let builtin = builtin_from_tag(header_a(h));
                let ret_pt = PhysicalType::from_u32(operand!(0));
                let fp_delta = operand!(1) as usize;
                let nargs = operand!(2) as usize;
                vm.eval_span = k1.bc.span_for_pc(pc as u32);

                let new_fp = unsafe { fp.add(fp_delta) };
                let args: &[Value] = unsafe {
                    core::slice::from_raw_parts(
                        new_fp.add(super::FRAME_HEADER_WORDS as usize * 8) as *const Value,
                        nargs,
                    )
                };
                let outcome = vmtry!(exec_builtin(k1, vm, builtin, args));
                match outcome {
                    BuiltinOutcome::Exit(code) => return Ok(code),
                    BuiltinOutcome::Value(v) => {
                        if ret_pt.is_agg() {
                            let sret = unsafe { *(new_fp as *const u64).add(2) } as *mut u8;
                            store_value(&k1.types, ret_pt, sret, v);
                        } else {
                            ret_reg = v;
                        }
                    }
                    BuiltinOutcome::Empty => {}
                }
                advance!(Opcode::CallBuiltin);
            }
            Opcode::RetGet => {
                write_slot!(operand!(0), ret_reg);
                advance!(Opcode::RetGet);
            }
            Opcode::RetStore => {
                let addr = read_src!(operand!(0));
                store_bits(header_a(h), addr.as_ptr(), ret_reg);
                advance!(Opcode::RetStore);
            }
            Opcode::Mov => {
                let v = read_src!(operand!(1));
                write_slot!(operand!(0), v);
                advance!(Opcode::Mov);
            }
            Opcode::Lea => {
                let offset = operand!(1) as usize;
                let addr = unsafe { fp.add(offset) };
                write_slot!(operand!(0), Value::ptr(addr));
                advance!(Opcode::Lea);
            }
            Opcode::LoadGlobal => {
                let dst = operand!(0);
                let global_id = TypedGlobalId::from_u32(operand!(1)).unwrap();
                let storage_pt = PhysicalType::from_u32(operand!(2));
                vm.eval_span = k1.bc.span_for_pc(pc as u32);
                // May run the global's initializer: nested typechecking and
                // nested static execution (on alt VMs), possibly more lowering.
                let v = vmtry!(vm::resolve_global(k1, vm, global_id, storage_pt));
                write_slot!(dst, v);
                advance!(Opcode::LoadGlobal);
            }
            Opcode::Load => {
                let addr = read_src!(operand!(1));
                write_slot!(operand!(0), load_bits(header_a(h), addr.as_ptr()));
                advance!(Opcode::Load);
            }
            Opcode::Store => {
                let addr = read_src!(operand!(0));
                let v = read_src!(operand!(1));
                store_bits(header_a(h), addr.as_ptr(), v);
                advance!(Opcode::Store);
            }
            Opcode::AtomicLoad => {
                let ord = ir::AtomicOrderingIr::from_tag(header_b(h) as u8);
                let addr = read_src!(operand!(1));
                write_slot!(operand!(0), vm::atomic_load_bits(header_a(h), addr.as_ptr(), ord));
                advance!(Opcode::AtomicLoad);
            }
            Opcode::AtomicStore => {
                let ord = ir::AtomicOrderingIr::from_tag(header_b(h) as u8);
                let addr = read_src!(operand!(0));
                let v = read_src!(operand!(1));
                vm::atomic_store_bits(header_a(h), addr.as_ptr(), v, ord);
                advance!(Opcode::AtomicStore);
            }
            Opcode::AtomicRmw => {
                let b = header_b(h);
                let op = ir::AtomicRmwOpIr::from_tag((b >> 8) as u8);
                let ord = ir::AtomicOrderingIr::from_tag((b & 0xff) as u8);
                let addr = read_src!(operand!(1));
                let operand = read_src!(operand!(2));
                let prev = vm::atomic_rmw_bits(op, header_a(h), addr.as_ptr(), operand, ord);
                write_slot!(operand!(0), prev);
                advance!(Opcode::AtomicRmw);
            }
            Opcode::AtomicCmpxchg => {
                let width = header_a(h);
                let b = header_b(h);
                let success = ir::AtomicOrderingIr::from_tag((b & 0xf) as u8);
                let failure = ir::AtomicOrderingIr::from_tag((b >> 4 & 0xf) as u8);
                let weak = b >> 8 & 1 == 1;
                let result_ptr = read_src!(operand!(0)).as_ptr();
                let addr = read_src!(operand!(1));
                let expected = read_src!(operand!(2));
                let desired = read_src!(operand!(3));
                let ok_offset = operand!(4) as usize;
                let (prev, ok) = vm::atomic_cmpxchg_bits(
                    width,
                    addr.as_ptr(),
                    expected,
                    desired,
                    success,
                    failure,
                    weak,
                );
                store_bits(width, result_ptr, prev);
                unsafe { result_ptr.add(ok_offset).write(ok as u8) };
                advance!(Opcode::AtomicCmpxchg);
            }
            Opcode::Fence => {
                let ord = ir::AtomicOrderingIr::from_tag(header_b(h) as u8);
                std::sync::atomic::fence(vm::rust_atomic_ordering(ord));
                advance!(Opcode::Fence);
            }
            Opcode::Copy => {
                let size = operand!(2) as usize;
                if size != 0 {
                    let dst = read_src!(operand!(0));
                    let src = read_src!(operand!(1));
                    let dst_ptr = dst.as_ptr();
                    let src_ptr = src.as_ptr();
                    if cfg!(debug_assertions) && (dst_ptr.is_null() || src_ptr.is_null()) {
                        vmerr!("Copy touching null pointer (size {})", size);
                    }
                    vm::memmove(src_ptr, dst_ptr, size);
                }
                advance!(Opcode::Copy);
            }
            Opcode::PtrAddImm => {
                let base = read_src!(operand!(1));
                let offset = operand!(2) as usize;
                // Address computation is legal on any address (e.g. aligning a
                // null-derived pointer); only access is sanity-checked
                let result = base.as_ptr_unchecked().wrapping_byte_add(offset);
                write_slot!(operand!(0), Value::ptr(result));
                advance!(Opcode::PtrAddImm);
            }
            Opcode::PtrIndex => {
                let base = read_src!(operand!(1));
                let index = read_src!(operand!(2)).bits() as usize;
                let stride = operand!(3) as usize;
                let result = base.as_ptr_unchecked().wrapping_byte_add(stride.wrapping_mul(index));
                write_slot!(operand!(0), Value::ptr(result));
                advance!(Opcode::PtrIndex);
            }
            Opcode::IntAdd => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                let r = casted_uop!(width, wrapping_add, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::IntAdd);
            }
            Opcode::IntSub => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                let r = casted_uop!(width, wrapping_sub, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::IntSub);
            }
            Opcode::IntMul => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                let r = casted_uop!(width, wrapping_mul, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::IntMul);
            }
            Opcode::IntDivU => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                if rhs == 0 {
                    vmerr!("Division by zero");
                }
                use std::ops::Div;
                let r = casted_uop!(width, div, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::IntDivU);
            }
            Opcode::IntDivS => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                if rhs == 0 {
                    vmerr!("Division by zero");
                }
                use std::ops::Div;
                let r = casted_iop!(width, div, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r as u64));
                advance!(Opcode::IntDivS);
            }
            Opcode::IntRemU => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                if rhs == 0 {
                    vmerr!("Division by zero");
                }
                use std::ops::Rem;
                let r = casted_uop!(width, rem, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::IntRemU);
            }
            Opcode::IntRemS => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                if rhs == 0 {
                    vmerr!("Division by zero");
                }
                use std::ops::Rem;
                let r = casted_iop!(width, rem, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r as u64));
                advance!(Opcode::IntRemS);
            }
            Opcode::IntCmp => {
                let width = header_a(h);
                let pred = int_pred_from_tag(header_b(h));
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                let b = int_cmp(width, pred, lhs, rhs);
                write_slot!(operand!(0), Value::bool(b));
                advance!(Opcode::IntCmp);
            }
            Opcode::FloatAdd => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::Add;
                let r = casted_float_op!(width, add, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatAdd);
            }
            Opcode::FloatSub => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::Sub;
                let r = casted_float_op!(width, sub, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatSub);
            }
            Opcode::FloatMul => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::Mul;
                let r = casted_float_op!(width, mul, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatMul);
            }
            Opcode::FloatDiv => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::Div;
                let r = casted_float_op!(width, div, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatDiv);
            }
            Opcode::FloatRem => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::Rem;
                let r = casted_float_op!(width, rem, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatRem);
            }
            Opcode::FloatCmp => {
                let width = header_a(h);
                let pred = float_pred_from_tag(header_b(h));
                let lhs = read_src!(operand!(1));
                let rhs = read_src!(operand!(2));
                // NOTE: Eq is bitwise, matching the old VM (NaN == NaN,
                // -0.0 != 0.0). Do not "fix".
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
                write_slot!(operand!(0), Value::bool(b));
                advance!(Opcode::FloatCmp);
            }
            Opcode::BitAnd => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::BitAnd;
                let r = casted_uop!(width, bitand, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::BitAnd);
            }
            Opcode::BitOr => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::BitOr;
                let r = casted_uop!(width, bitor, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::BitOr);
            }
            Opcode::BitXor => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).bits();
                use std::ops::BitXor;
                let r = casted_uop!(width, bitxor, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::BitXor);
            }
            Opcode::Shl => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).as_u32();
                use std::ops::Shl;
                let r = casted_uop!(width, shl, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::Shl);
            }
            Opcode::ShrU => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).as_u32();
                use std::ops::Shr;
                let r = casted_uop!(width, shr, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::ShrU);
            }
            Opcode::ShrS => {
                let width = header_a(h);
                let lhs = read_src!(operand!(1)).bits();
                let rhs = read_src!(operand!(2)).as_u32();
                use std::ops::Shr;
                let r = casted_iop!(width, shr, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r as u64));
                advance!(Opcode::ShrS);
            }
            Opcode::BoolNegate => {
                let b = read_src!(operand!(1)).as_bool();
                write_slot!(operand!(0), Value::bool(!b));
                advance!(Opcode::BoolNegate);
            }
            Opcode::BitNot => {
                // Inverts all 64 bits regardless of width; matches the old VM
                let v = read_src!(operand!(1));
                write_slot!(operand!(0), Value::u64(!v.bits()));
                advance!(Opcode::BitNot);
            }
            Opcode::Cast => {
                let kind = CastKind::from_u8(header_a(h));
                let b = header_b(h);
                let from = (b >> 8) as u32;
                let to = (b & 0xff) as u32;
                let input = read_src!(operand!(1));
                let result = exec_cast(kind, from, to, input);
                write_slot!(operand!(0), result);
                advance!(Opcode::Cast);
            }
            Opcode::BakeStaticValue => {
                let dst = operand!(0);
                let type_id = TypeId::from_nzu32(NonZeroU32::new(operand!(1)).unwrap());
                let input = read_src!(operand!(2));
                vm.eval_span = k1.bc.span_for_pc(pc as u32);
                let value_id =
                    vmtry!(vm::vm_value_to_static_value(k1, type_id, input, vm.eval_span));
                write_slot!(dst, Value::u64(value_id.as_u32() as u64));
                advance!(Opcode::BakeStaticValue);
            }
        }
    }
}

/// `from`/`to` are widths in bits; signedness is carried by the CastKind.
fn exec_cast(kind: CastKind, from: u32, to: u32, input: Value) -> Value {
    match kind {
        CastKind::IntTrunc => input.truncated_raw(to),
        CastKind::IntExtS => input.sign_extended_raw(from, to),
        CastKind::FloatTrunc => Value::f32(input.as_f64() as f32),
        CastKind::FloatExt => Value::f64(input.as_f32() as f64),
        CastKind::F32ToUInt => {
            let f = input.as_f32();
            match to {
                8 => Value::u8(f as u8),
                16 => Value::u16(f as u16),
                32 => Value::u32(f as u32),
                _ => Value::u64(f as u64),
            }
        }
        CastKind::F32ToSInt => {
            let f = input.as_f32();
            match to {
                8 => Value::i8(f as i8),
                16 => Value::i16(f as i16),
                32 => Value::i32(f as i32),
                _ => Value::i64(f as i64),
            }
        }
        CastKind::F64ToUInt => {
            let f = input.as_f64();
            match to {
                8 => Value::u8(f as u8),
                16 => Value::u16(f as u16),
                32 => Value::u32(f as u32),
                _ => Value::u64(f as u64),
            }
        }
        CastKind::F64ToSInt => {
            let f = input.as_f64();
            match to {
                8 => Value::i8(f as i8),
                16 => Value::i16(f as i16),
                32 => Value::i32(f as i32),
                _ => Value::i64(f as i64),
            }
        }
        CastKind::UIntToF32 => {
            let bits = input.bits();
            let f = match from {
                8 => bits as u8 as f32,
                16 => bits as u16 as f32,
                32 => bits as u32 as f32,
                _ => bits as f32,
            };
            Value::f32(f)
        }
        CastKind::UIntToF64 => {
            let bits = input.bits();
            let f = match from {
                8 => bits as u8 as f64,
                16 => bits as u16 as f64,
                32 => bits as u32 as f64,
                _ => bits as f64,
            };
            Value::f64(f)
        }
        CastKind::SIntToF32 => {
            let bits = input.bits();
            let f = match from {
                8 => bits as i8 as f32,
                16 => bits as i16 as f32,
                32 => bits as i32 as f32,
                _ => bits as i64 as f32,
            };
            Value::f32(f)
        }
        CastKind::SIntToF64 => {
            let bits = input.bits();
            let f = match from {
                8 => bits as i8 as f64,
                16 => bits as i16 as f64,
                32 => bits as i32 as f64,
                _ => bits as i64 as f64,
            };
            Value::f64(f)
        }
    }
}

fn int_cmp(width: u8, pred: ir::IntCmpPred, lhs: u64, rhs: u64) -> bool {
    use ir::IntCmpPred as P;
    match (width, pred) {
        (8, P::Eq) => (lhs as u8) == (rhs as u8),
        (16, P::Eq) => (lhs as u16) == (rhs as u16),
        (32, P::Eq) => (lhs as u32) == (rhs as u32),
        (64, P::Eq) => lhs == rhs,

        (8, P::Slt) => (lhs as i8) < (rhs as i8),
        (8, P::Sle) => (lhs as i8) <= (rhs as i8),
        (8, P::Sgt) => (lhs as i8) > (rhs as i8),
        (8, P::Sge) => (lhs as i8) >= (rhs as i8),
        (8, P::Ult) => (lhs as u8) < (rhs as u8),
        (8, P::Ule) => (lhs as u8) <= (rhs as u8),
        (8, P::Ugt) => (lhs as u8) > (rhs as u8),
        (8, P::Uge) => (lhs as u8) >= (rhs as u8),
        (16, P::Slt) => (lhs as i16) < (rhs as i16),
        (16, P::Sle) => (lhs as i16) <= (rhs as i16),
        (16, P::Sgt) => (lhs as i16) > (rhs as i16),
        (16, P::Sge) => (lhs as i16) >= (rhs as i16),
        (16, P::Ult) => (lhs as u16) < (rhs as u16),
        (16, P::Ule) => (lhs as u16) <= (rhs as u16),
        (16, P::Ugt) => (lhs as u16) > (rhs as u16),
        (16, P::Uge) => (lhs as u16) >= (rhs as u16),
        (32, P::Slt) => (lhs as i32) < (rhs as i32),
        (32, P::Sle) => (lhs as i32) <= (rhs as i32),
        (32, P::Sgt) => (lhs as i32) > (rhs as i32),
        (32, P::Sge) => (lhs as i32) >= (rhs as i32),
        (32, P::Ult) => (lhs as u32) < (rhs as u32),
        (32, P::Ule) => (lhs as u32) <= (rhs as u32),
        (32, P::Ugt) => (lhs as u32) > (rhs as u32),
        (32, P::Uge) => (lhs as u32) >= (rhs as u32),
        (64, P::Slt) => (lhs as i64) < (rhs as i64),
        (64, P::Sle) => (lhs as i64) <= (rhs as i64),
        (64, P::Sgt) => (lhs as i64) > (rhs as i64),
        (64, P::Sge) => (lhs as i64) >= (rhs as i64),
        (64, P::Ult) => lhs < rhs,
        (64, P::Ule) => lhs <= rhs,
        (64, P::Ugt) => lhs > rhs,
        (64, P::Uge) => lhs >= rhs,
        _ => unreachable!(),
    }
}

/// Backend builtins, taking pre-resolved args from the out-arg region.
/// Semantics copied from the old VM's inline dispatch (vm.rs Inst::Call,
/// IrCallee::BackendBuiltin arm).
fn exec_builtin(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    builtin: BackendBuiltin,
    args: &[Value],
) -> K1Result<BuiltinOutcome> {
    match builtin {
        BackendBuiltin::TypeSchema => {
            let type_id_arg = args[0].bits();
            let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
            let Some(schema_static_value_id) = k1.type_schemas.get(&type_id) else {
                return failf!(
                    vm.eval_span,
                    "Missing type schema: {}",
                    k1.type_id_to_string(type_id)
                );
            };
            let schema_vm_value =
                vm::static_value_to_vm_value(k1, *schema_static_value_id, vm.eval_span);
            Ok(BuiltinOutcome::Value(schema_vm_value))
        }
        BackendBuiltin::TypeName => {
            let type_id_arg = args[0].bits();
            let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_arg as u32).unwrap());
            let name_value_id = *k1.type_names.get(&type_id).unwrap();
            let name_string_value = vm::static_value_to_vm_value(k1, name_value_id, vm.eval_span);
            Ok(BuiltinOutcome::Value(name_string_value))
        }
        BackendBuiltin::Allocate | BackendBuiltin::AllocateZeroed => {
            let zero = builtin == BackendBuiltin::AllocateZeroed;
            let size = args[0];
            let align = args[1];
            let Ok(layout) =
                std::alloc::Layout::from_size_align(size.bits() as usize, align.bits() as usize)
            else {
                return failf!(
                    vm.eval_span,
                    "Rust didn't like this layout: size={size}, align={align}"
                );
            };
            let ptr = vm::allocate(layout, zero);
            Ok(BuiltinOutcome::Value(Value::ptr(ptr)))
        }
        BackendBuiltin::Reallocate => {
            let old_ptr = args[0];
            let old_size = args[1];
            let align = args[2];
            let new_size = args[3];
            let layout =
                std::alloc::Layout::from_size_align(old_size.as_usize(), align.as_usize()).unwrap();
            let ptr = unsafe { std::alloc::realloc(old_ptr.as_ptr(), layout, new_size.as_usize()) };
            Ok(BuiltinOutcome::Value(Value::ptr(ptr)))
        }
        BackendBuiltin::Free => {
            let ptr = args[0].as_ptr();
            let size = args[1].as_usize();
            let align = args[2].as_usize();
            let layout = std::alloc::Layout::from_size_align(size, align).unwrap();
            unsafe { std::alloc::dealloc(ptr, layout) };
            Ok(BuiltinOutcome::Value(Value::ptr(ptr)))
        }
        BackendBuiltin::MemCopy | BackendBuiltin::MemMove => {
            let dst = args[0];
            let src = args[1];
            let count = args[2];
            if builtin == BackendBuiltin::MemCopy {
                vm::memcopy(src.as_ptr(), dst.as_ptr(), count.as_usize());
            } else {
                vm::memmove(src.as_ptr(), dst.as_ptr(), count.as_usize());
            }
            Ok(BuiltinOutcome::Empty)
        }
        BackendBuiltin::MemSet => {
            let dst = args[0];
            let value = args[1].bits() as u8;
            let count = args[2];
            unsafe { std::ptr::write_bytes(dst.as_ptr(), value, count.as_usize()) };
            Ok(BuiltinOutcome::Empty)
        }
        BackendBuiltin::MemEquals => {
            let p1 = args[0].as_ptr();
            let p2 = args[1].as_ptr();
            let size = args[2].bits() as usize;
            let p1_slice = unsafe { vm::slice_from_raw_parts_checked(vm, k1, p1, size) };
            let p2_slice = unsafe { vm::slice_from_raw_parts_checked(vm, k1, p2, size) };
            Ok(BuiltinOutcome::Value(Value::bool(p1_slice == p2_slice)))
        }
        BackendBuiltin::Exit => {
            let exit_code = args[0].bits();
            Ok(BuiltinOutcome::Exit(exit_code as i32))
        }
        BackendBuiltin::CompilerMessage => {
            vm::builtin_compiler_message(k1, vm, args[0], args[1], args[2])?;
            Ok(BuiltinOutcome::Empty)
        }
        BackendBuiltin::ReplCheckbox => {
            vm::builtin_repl_checkbox(k1, vm, args[0], args[1], args[2])?;
            Ok(BuiltinOutcome::Empty)
        }
    }
}
