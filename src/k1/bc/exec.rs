// Copyright (c) 2026 knix
// All rights reserved.

//! The bc VM: executes the flat bytecode stream.
//!
//! Runtime state is `pc` (pointer into `k1.bc.code`; frames and fault records
//! store it as a u32 index), `fp` (current frame base pointer into the VM
//! stack), and `ret_reg` (the scalar return value register). calls bump `fp`
//! by callee frame size and write a 2-word link (caller_fp, return_pc) into
//! the new frame's header; returns read it back. `code[0]` is `Halt`, so
//! popping the top frame (return_pc 0) lands there and ends execution.

use std::num::NonZeroU32;

use crate::ir::{self, BackendBuiltin, IrUnitId};
use crate::lex::SpanId;
use crate::typer::trace::TraceKind;
use crate::typer::types::{PhysicalType, RecordKind, TypeId};
use crate::typer::{FunctionId, K1Result, StaticValueId, TypedExprId, TypedGlobalId, TypedProgram};
use crate::vm::{
    self, Value, Vm, casted_float_op, casted_iop, casted_uop, load_value, store_value,
};
use crate::{kbail, kerr};

use super::lower;
use super::{
    CastKind, OPCODE_COUNT, Opcode, UnitKind, VALUE_MASK_CONST_ID, VALUE_MASK_FRAME_OFFSET,
    builtin_from_tag, header_a, header_b, header_op,
};

const STATIC_EXEC_INSTR_LIMIT: i64 = 2_000_000_000;

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

pub fn execute_compiled_expr_raw(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    expr_id: TypedExprId,
    report_messages: bool,
) -> K1Result<RawUnitResult> {
    let span = k1.exprs.get_span(expr_id);
    execute_compiled_unit_raw(k1, vm, IrUnitId::Expr(expr_id), &[], span, report_messages)
}

pub fn execute_compiled_function_raw(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    function_id: FunctionId,
    arguments: &[StaticValueId],
    report_messages: bool,
) -> K1Result<RawUnitResult> {
    let span = k1.get_function_span(function_id);
    execute_compiled_unit_raw(
        k1,
        vm,
        IrUnitId::Function(function_id),
        arguments,
        span,
        report_messages,
    )
}

/// A unit's result as raw VM memory; every pointer is valid only until
/// `vm.reset`, so consumers must copy out within the executing VM scope
pub struct RawUnitResult {
    pub ret_addr: *mut u8,
    pub ret_pt: PhysicalType,
    pub result_type_id: TypeId,
    /// false when the unit diverges or returns empty; ret_addr holds nothing
    pub returns_value: bool,
}

pub fn execute_compiled_unit(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    unit_id: IrUnitId,
    arguments: &[StaticValueId],
    span: SpanId,
    report_messages: bool,
) -> K1Result<StaticValueId> {
    let raw = execute_compiled_unit_raw(k1, vm, unit_id, arguments, span, report_messages)?;
    if !raw.returns_value {
        return Ok(k1.static_values.add_empty_typed(raw.result_type_id));
    }
    let frame = k1.trace_push(TraceKind::VmValueFerry, span.as_u32(), 0);
    let loaded = load_value(raw.ret_pt, raw.ret_addr);
    let result = vm::vm_value_to_static_value(k1, raw.result_type_id, loaded, span);
    k1.trace_pop(frame);
    result
}

pub fn execute_compiled_unit_raw(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    unit_id: IrUnitId,
    arguments: &[StaticValueId],
    span: SpanId,
    report_messages: bool,
) -> K1Result<RawUnitResult> {
    vm.eval_span = span;
    vm.bc_fault = None;

    let info = lower::get_or_lower_unit(k1, unit_id, span)?;
    if info.kind != UnitKind::Body {
        kbail!(k1, span, "Cannot execute a bodyless ({}) unit", info.kind);
    }
    let ir_unit = ir::get_compiled_unit(&k1.ir, unit_id).unwrap();
    let result_type_id = ir_unit.result_type_id;
    let ret_pt = info.ret_pt;

    // Return-value storage lives just below the first frame
    let ret_layout = k1.get_pt_layout(ret_pt);
    let ret_addr = vm.stack.push_layout_uninit(ret_layout);
    vm.overall_return_addr = ret_addr;

    // Top frame: FRAME_ALIGN-aligned base, header links to Halt (pc 0, fp 0)
    let align_mask = super::FRAME_ALIGN as usize - 1;
    let fp0: *mut u8 = vm.stack.mem.cursor().map_addr(|a| (a + align_mask) & !align_mask);
    vm.stack.mem.set_cursor(fp0);
    let ferry_frame = k1.trace_push(TraceKind::VmValueFerry, span.as_u32(), 0);
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
    k1.trace_pop(ferry_frame);

    let run_frame = k1.trace_push_unit(TraceKind::VmRun, unit_id, None);
    let exec_result = exec_loop(k1, vm, info.code_start, fp0, ret_pt);
    k1.trace_pop(run_frame);

    let exit_code = match exec_result {
        Ok(exit_code) => exit_code,
        Err(mut e) => {
            if let Some((fault_fp, fault_pc)) = vm.bc_fault {
                let trace = make_stack_trace(k1, fault_fp as *const u8, fault_pc);
                e.message = k1.ast.idents.intern(format!(
                    "{}\nbc Execution Trace\n{}",
                    k1.ident_str(e.message),
                    trace
                ));
            }
            return Err(e);
        }
    };

    if report_messages {
        vm::report_execution_messages(k1, vm, span, exit_code);
    }

    vm.overall_return_addr = core::ptr::null_mut();
    if exit_code != 0 {
        Err(kerr!(k1, span, "Static execution exited with code: {}", exit_code))
    } else {
        Ok(RawUnitResult {
            ret_addr,
            ret_pt,
            result_type_id,
            returns_value: !(info.diverges || ret_pt.is_empty()),
        })
    }
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
            write!(&mut s, " {}:{}", source.filename_str(&k1.ast.idents), line.line_number())
                .unwrap();
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

#[inline]
fn store_bits(width_bits: u8, dst: *mut u8, v: Value) {
    debug_assert!(!dst.is_null(), "store_bits to null (width {})", width_bits);
    unsafe {
        match width_bits {
            8 => dst.write(v.bits() as u8),
            16 => (dst as *mut u16).write_unaligned(v.bits() as u16),
            32 => (dst as *mut u32).write_unaligned(v.bits() as u32),
            _ => (dst as *mut u64).write_unaligned(v.bits()),
        }
    }
}

#[inline]
fn load_bits(width_bits: u8, src: *const u8) -> Value {
    debug_assert!(!src.is_null(), "load_bits from null (width {})", width_bits);
    unsafe {
        match width_bits {
            8 => Value::u8(src.read()),
            16 => Value::u16((src as *const u16).read_unaligned()),
            32 => Value::u32((src as *const u32).read_unaligned()),
            _ => Value::u64((src as *const u64).read_unaligned()),
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
    // VVec's base never moves (growth commits reserved pages), so these stay
    // valid while mid-execution lowering appends to code/consts
    let code_base: *const u32 = k1.bc.code.as_ptr();
    let consts_base: *const u64 = k1.bc.consts.as_ptr();
    let mut pc: *const u32 = unsafe { code_base.add(start_pc as usize) };
    let mut fp: *mut u8 = top_fp;
    let mut ret_reg: Value = Value::u64(0);
    let mut instrs_run = 0;
    let count_ops = k1.config.chatty;
    let mut op_counts = [0u64; OPCODE_COUNT as usize];

    // Callers wrap uses in `unsafe`; pointer arithmetic + deref together
    macro_rules! word_ptr {
        ($w:expr) => {
            (fp as *mut u64).add($w as usize)
        };
    }
    macro_rules! pc_u32 {
        () => {
            unsafe { pc.offset_from(code_base) } as u32
        };
    }
    macro_rules! set_pc {
        ($i:expr) => {
            pc = unsafe { code_base.add($i as usize) }
        };
    }
    // Tagged src operand: frame word, constant pool entry, or fp-relative address
    macro_rules! read_src {
        ($w:expr) => {{
            let w: u32 = $w;
            if w & (VALUE_MASK_CONST_ID | VALUE_MASK_FRAME_OFFSET) == 0 {
                Value::u64(unsafe { *word_ptr!(w) })
            } else if w & VALUE_MASK_CONST_ID != 0 {
                let idx = (w & !VALUE_MASK_CONST_ID) as usize;
                debug_assert!(idx < k1.bc.consts.len(), "const operand out of bounds");
                Value::u64(unsafe { *consts_base.add(idx) })
            } else {
                Value::ptr(unsafe { fp.add((w & !VALUE_MASK_FRAME_OFFSET) as usize) })
            }
        }};
    }
    macro_rules! write_slot {
        ($w:expr, $v:expr) => {{
            // Evaluate operands outside the unsafe block (they may expand to
            // their own unsafe, e.g. unchecked code reads)
            let w: u32 = $w;
            let v: Value = $v;
            debug_assert!(
                w & (VALUE_MASK_CONST_ID | VALUE_MASK_FRAME_OFFSET) == 0,
                "tagged dst operand"
            );
            unsafe { *word_ptr!(w) = v.bits() }
        }};
    }
    macro_rules! operand {
        ($i:expr) => {{
            debug_assert!(
                (pc_u32!() as usize) + 1 + $i < k1.bc.code.len(),
                "operand read out of bounds"
            );
            unsafe { *pc.add(1 + $i) }
        }};
    }
    macro_rules! advance {
        ($op:path) => {
            pc = unsafe { pc.add(const { 1 + $op.operand_count() }) }
        };
    }
    // Error return, recording the fault location for stack traces
    macro_rules! vmerr {
        ($($args:expr),*) => {{
            vm.bc_fault = Some((fp as u64, pc_u32!()));
            kbail!(k1, k1.bc.span_for_pc(pc_u32!()), $($args),*);
        }};
    }
    // `?` with fault recording
    macro_rules! vmtry {
        ($e:expr) => {
            match $e {
                Ok(v) => v,
                Err(err) => {
                    vm.bc_fault = Some((fp as u64, pc_u32!()));
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
            set_pc!(ret_pc);
        }};
    }

    loop {
        debug_assert!((pc_u32!() as usize) < k1.bc.code.len(), "bc pc out of bounds");
        let h = unsafe { *pc };
        instrs_run += 1;
        if instrs_run > STATIC_EXEC_INSTR_LIMIT {
            vmerr!(
                "Static execution exceeded {} instructions; likely an infinite loop",
                STATIC_EXEC_INSTR_LIMIT
            );
        }
        let op = Opcode::from_u8(header_op(h));
        if count_ops {
            op_counts[op as usize] += 1;
        }

        match op {
            Opcode::Halt => {
                // Aggregate results were already copied through the top
                // frame's sret; scalars ride ret_reg.
                if !top_ret_pt.is_empty() && !top_ret_pt.is_agg() {
                    store_value(k1, top_ret_pt, vm.overall_return_addr, ret_reg);
                }
                k1.trace.set_top_count(instrs_run as u64);
                if count_ops {
                    for (i, n) in op_counts.iter().enumerate() {
                        k1.trace.opcode_counts[i] += *n as i64;
                    }
                }
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
                let tgt = operand!(0);
                set_pc!(tgt);
            }
            Opcode::JumpIf => {
                let cond = read_src!(operand!(0));
                let target = if cond.as_bool() { operand!(1) } else { operand!(2) };
                set_pc!(target);
            }
            Opcode::Switch => {
                let width = header_a(h);
                let count = header_b(h) as usize;
                let key = read_src!(operand!(0)).bits() & ir::low_mask_from_u8(width);
                let mut target = operand!(1);
                let mut lo = 0usize;
                let mut hi = count;
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    let base = 2 + mid * 3;
                    let value = operand!(base) as u64 | ((operand!(base + 1) as u64) << 32);
                    if value == key {
                        target = operand!(base + 2);
                        break;
                    }
                    if value < key {
                        lo = mid + 1;
                    } else {
                        hi = mid;
                    }
                }
                set_pc!(target);
            }
            Opcode::JumpIfIntCmp => {
                let width = header_a(h);
                let pred = ir::IntCmpPred::from_u8(header_b(h) as u8);
                let lhs = read_src!(operand!(0)).bits();
                let rhs = read_src!(operand!(1)).bits();
                let target = if int_cmp(width, pred, lhs, rhs) { operand!(2) } else { operand!(3) };
                set_pc!(target);
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
                let nargs = header_b(h) as usize;
                debug_assert_ne!(target, super::PENDING_PC, "unpatched recursive call target");
                let new_fp = unsafe { fp.add(fp_delta) };
                let words = new_fp as *mut u64;
                for k in 0..nargs {
                    let v = read_src!(operand!(3 + k));
                    unsafe { words.add(3 + k).write(v.bits()) };
                }
                let sret = read_src!(operand!(2));
                let ret_pc = pc_u32!() as usize + 4 + nargs;
                unsafe {
                    words.write(fp as u64);
                    words.add(1).write(ret_pc as u64);
                    words.add(2).write(sret.bits());
                }
                fp = new_fp;
                set_pc!(target);
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
                        vm.eval_span = k1.bc.span_for_pc(pc_u32!());
                        vmtry!(lower::get_or_lower_function(k1, function_id, vm.eval_span))
                    }
                };
                if info.kind != UnitKind::Body {
                    vmerr!(
                        "Indirect call to bodyless ({}) function: {}",
                        info.kind,
                        k1.function_id_to_string(function_id, false)
                    );
                }
                let nargs = header_b(h) as usize;
                let new_fp = unsafe { fp.add(fp_delta) };
                let words = new_fp as *mut u64;
                for k in 0..nargs {
                    let v = read_src!(operand!(3 + k));
                    unsafe { words.add(3 + k).write(v.bits()) };
                }
                let sret = read_src!(operand!(2));
                let ret_pc = pc_u32!() as usize + 4 + nargs;
                unsafe {
                    words.write(fp as u64);
                    words.add(1).write(ret_pc as u64);
                    words.add(2).write(sret.bits());
                }
                fp = new_fp;
                set_pc!(info.code_start);
            }
            Opcode::CallExtern => {
                let function_id = FunctionId::from_u32(operand!(0)).unwrap();
                // StringIds are nonzero, so 0 means "none"
                let lib_name = crate::parse::StringId::from_u32(operand!(1));
                let fn_name = crate::parse::StringId::from_u32(operand!(2)).unwrap();
                let ret_pt = PhysicalType::from_u32(operand!(3));
                let fp_delta = operand!(4) as usize;
                let nargs = operand!(5) as usize;
                vm.eval_span = k1.bc.span_for_pc(pc_u32!());

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
            Opcode::CallLlvm => {
                let fn_name = crate::parse::StringId::from_u32(operand!(0)).unwrap();
                let _ret_pt = PhysicalType::from_u32(operand!(1));
                let fp_delta = operand!(2) as usize;
                let nargs = operand!(3) as usize;
                vm.eval_span = k1.bc.span_for_pc(pc_u32!());

                let new_fp = unsafe { fp.add(fp_delta) };
                let args: &[Value] = unsafe {
                    core::slice::from_raw_parts(
                        new_fp.add(super::FRAME_HEADER_WORDS as usize * 8) as *const Value,
                        nargs,
                    )
                };
                let result = match k1.ident_str(fn_name) {
                    "llvm.cttz.i64" => Value::u64(args[0].bits().trailing_zeros() as u64),
                    "llvm.ctlz.i64" => Value::u64(args[0].bits().leading_zeros() as u64),
                    "llvm.ctpop.i64" => Value::u64(args[0].bits().count_ones() as u64),
                    other => {
                        let other = other.to_string();
                        vmerr!(
                            "No comptime emulation for {} — this intrinsic is runtime-only",
                            other
                        )
                    }
                };
                ret_reg = result;
                advance!(Opcode::CallLlvm);
            }
            Opcode::CallBuiltin => {
                let builtin = builtin_from_tag(header_a(h));
                let ret_pt = PhysicalType::from_u32(operand!(0));
                let fp_delta = operand!(1) as usize;
                let nargs = operand!(2) as usize;
                vm.eval_span = k1.bc.span_for_pc(pc_u32!());

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
                            store_value(k1, ret_pt, sret, v);
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
            Opcode::LoadGlobal => {
                let dst = operand!(0);
                let global_id = TypedGlobalId::from_u32(operand!(1)).unwrap();
                let v = match k1.vm_global_constant_lookups.get(&global_id) {
                    Some(v) => *v,
                    None => match vm.globals.get(&global_id) {
                        Some(v) => *v,
                        None => {
                            // First use: may run the global's initializer -> nested
                            // typechecking and nested static execution (on alt VMs),
                            // possibly more lowering.
                            let storage_pt = PhysicalType::from_u32(operand!(2));
                            vm.eval_span = k1.bc.span_for_pc(pc_u32!());
                            vmtry!(vm::resolve_global(k1, vm, global_id, storage_pt))
                        }
                    },
                };
                write_slot!(dst, v);
                advance!(Opcode::LoadGlobal);
            }
            Opcode::Load => {
                // B = folded byte offset (0 when the address is used directly)
                let addr = read_src!(operand!(1)).as_ptr().wrapping_byte_add(header_b(h) as usize);
                write_slot!(operand!(0), load_bits(header_a(h), addr));
                advance!(Opcode::Load);
            }
            Opcode::Store => {
                let addr = read_src!(operand!(0)).as_ptr().wrapping_byte_add(header_b(h) as usize);
                let v = read_src!(operand!(1));
                store_bits(header_a(h), addr, v);
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
                let r = casted_iop!(width, wrapping_div, lhs, rhs);
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
                let r = casted_iop!(width, wrapping_rem, lhs, rhs);
                write_slot!(operand!(0), Value::u64(r as u64));
                advance!(Opcode::IntRemS);
            }
            Opcode::IntCmp => {
                let width = header_a(h);
                let pred = ir::IntCmpPred::from_u8(header_b(h) as u8);
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
                let pred = ir::FloatCmpPred::from_u8(header_b(h) as u8);
                let lhs = read_src!(operand!(1));
                let rhs = read_src!(operand!(2));
                let b = match (width, pred) {
                    (32, ir::FloatCmpPred::Eq) => lhs.as_f32() == rhs.as_f32(),
                    (32, ir::FloatCmpPred::Lt) => lhs.as_f32() < rhs.as_f32(),
                    (32, ir::FloatCmpPred::Le) => lhs.as_f32() <= rhs.as_f32(),
                    (32, ir::FloatCmpPred::Gt) => lhs.as_f32() > rhs.as_f32(),
                    (32, ir::FloatCmpPred::Ge) => lhs.as_f32() >= rhs.as_f32(),
                    (64, ir::FloatCmpPred::Eq) => lhs.as_f64() == rhs.as_f64(),
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
            Opcode::FloatNeg => {
                let v = read_src!(operand!(1)).bits();
                let r = match header_a(h) {
                    32 => (-f32::from_bits(v as u32)).to_bits() as u64,
                    64 => (-f64::from_bits(v)).to_bits(),
                    _ => unreachable!(),
                };
                write_slot!(operand!(0), Value::u64(r));
                advance!(Opcode::FloatNeg);
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
                vm.eval_span = k1.bc.span_for_pc(pc_u32!());
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
    debug_assert!(width > 0 && width <= 64, "bad int cmp width {width}");
    let shift = 64 - width as u32;
    let l = lhs << shift;
    let r = rhs << shift;
    match pred {
        P::Eq => l == r,
        P::Slt => (l as i64) < (r as i64),
        P::Sle => (l as i64) <= (r as i64),
        P::Sgt => (l as i64) > (r as i64),
        P::Sge => (l as i64) >= (r as i64),
        P::Ult => l < r,
        P::Ule => l <= r,
        P::Ugt => l > r,
        P::Uge => l >= r,
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
    fn type_id_outcome(k1: &mut TypedProgram, vm: &Vm, type_id: TypeId) -> BuiltinOutcome {
        let type_id_value_id = k1.add_type_id_value(type_id);
        BuiltinOutcome::Value(vm::static_value_to_vm_value(k1, type_id_value_id, vm.eval_span))
    }
    match builtin {
        BackendBuiltin::TypeInfo => {
            let type_id = vm::value_to_type_id(k1, args[0], vm.eval_span)?;
            let Some(info_value_id) = k1.type_infos.get(&type_id) else {
                kbail!(k1, vm.eval_span, "Missing type info: {}", type_id);
            };
            let info_vm_value = vm::static_value_to_vm_value(k1, *info_value_id, vm.eval_span);
            Ok(BuiltinOutcome::Value(info_vm_value))
        }
        BackendBuiltin::MakeStruct => {
            let Some(record_kind) = RecordKind::from_tag(args[0].as_u8()) else {
                kbail!(
                    k1,
                    vm.eval_span,
                    "make-struct: bad record-kind tag {}",
                    args[0].as_u8() as u32
                );
            };
            let field_descs: &[vm::k1_types::K1MakeStructField] =
                unsafe { vm::value_as_span(args[1]).to_slice() };
            let new_type_id = k1.make_struct_raw(record_kind, field_descs, vm.eval_span)?;
            Ok(type_id_outcome(k1, vm, new_type_id))
        }
        BackendBuiltin::MakeEither => {
            let tag_type_opt =
                unsafe { (args[0].as_ptr() as *const vm::k1_types::K1OptTypeId).read() };
            let explicit_tag_type =
                if tag_type_opt.tag == 0 { None } else { Some(tag_type_opt.payload) };
            let variant_descs: &[vm::k1_types::K1MakeEitherVariant] =
                unsafe { vm::value_as_span(args[1]).to_slice() };
            let new_type_id = k1.make_either_raw(explicit_tag_type, variant_descs, vm.eval_span)?;
            Ok(type_id_outcome(k1, vm, new_type_id))
        }
        BackendBuiltin::MakeReference => {
            let inner = vm::value_to_type_id(k1, args[0], vm.eval_span)?;
            let new_type_id = k1.make_reference_raw(inner);
            Ok(type_id_outcome(k1, vm, new_type_id))
        }
        BackendBuiltin::MakeArray => {
            let element_type = vm::value_to_type_id(k1, args[0], vm.eval_span)?;
            let size = args[1].bits() as i64;
            let new_type_id = k1.make_array_raw(element_type, size, vm.eval_span)?;
            Ok(type_id_outcome(k1, vm, new_type_id))
        }
        BackendBuiltin::MakeFn => {
            let raw_param_types: &[vm::k1_types::TypeId] =
                unsafe { vm::value_as_span(args[0]).to_slice() };
            let return_type = vm::value_to_type_id(k1, args[1], vm.eval_span)?;
            let new_type_id = k1.make_fn_raw(raw_param_types, return_type, vm.eval_span)?;
            Ok(type_id_outcome(k1, vm, new_type_id))
        }
        BackendBuiltin::MakeInstance => {
            let parent = vm::value_to_type_id(k1, args[0], vm.eval_span)?;
            let raw_args: &[vm::k1_types::TypeId] =
                unsafe { vm::value_as_span(args[1]).to_slice() };
            let new_type_id = k1.make_generic_instance_raw(parent, raw_args, vm.eval_span)?;
            Ok(type_id_outcome(k1, vm, new_type_id))
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
