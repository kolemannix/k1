// Copyright (c) 2026 knix
// All rights reserved.

//! Lowering from `ir` units to the flat `bc` stream.
//!
//! Per unit, three passes:
//! - Pass A (analyze): assign a dense frame-word slot to every value-producing
//!   instruction, build the value-forwarding map for no-op insts, collect
//!   phis per block, and lay out the frame (allocas + agg-return temps at
//!   fixed byte offsets).
//! - Pass B (emit): walk blocks in order, emitting into a unit-local buffer
//!   (so recursive callee lowering can append to the shared stream without
//!   interleaving). Phis emit nothing; each CFG edge into a phi-carrying
//!   block gets sequential `Mov` copies — inline before a `Jump`, or in an
//!   appended trampoline for `JumpIf` edges.
//! - Finalize: patch intra-unit jump targets, relocate local pcs by the
//!   append base, splice into `k1.bc.code`, and patch any recursion fixups
//!   that were waiting on this unit's `code_start`.

use fxhash::FxHashMap;
use log::debug;

use crate::failf;
use crate::ir::{self, BlockId, DataInst, Inst, InstId, InstKind, IrCallee, IrUnit, IrUnitId};
use crate::lex::SpanId;
use crate::typer::types::{Layout, PhysicalType, ScalarType};
use crate::typer::{FunctionId, K1Result, TypedExprId, TypedFloatValue, TypedProgram};
use crate::vm;

use super::{
    CastKind, FRAME_HEADER_WORDS, Opcode, PENDING_PC, UnitInfo, UnitKind, builtin_tag,
    float_pred_tag, header, int_pred_tag,
};

pub fn get_or_lower_unit(
    k1: &mut TypedProgram,
    unit_id: IrUnitId,
    span: SpanId,
) -> K1Result<UnitInfo> {
    match unit_id {
        IrUnitId::Function(fid) => get_or_lower_function(k1, fid, span),
        IrUnitId::Expr(eid) => get_or_lower_expr(k1, eid, span),
    }
}

pub fn get_or_lower_function(
    k1: &mut TypedProgram,
    function_id: FunctionId,
    span: SpanId,
) -> K1Result<UnitInfo> {
    if let Some(info) = k1.bc.functions.get(&function_id) {
        return Ok(*info);
    }
    debug_assert!(
        !k1.bc.in_progress.contains(&function_id),
        "get_or_lower_function called on in-progress function; caller must check"
    );
    let Some(unit) = *k1.ir.functions.get(function_id) else {
        return failf!(
            span,
            "Call to uncompiled function: {}. ({} are pending)",
            k1.function_id_to_string(function_id, false),
            k1.ir.units_pending_compile.len()
        );
    };
    // Builtins and externs have no body to lower; record a sentinel so
    // indirect-call resolution can produce a good error.
    let sentinel_kind = if unit.function_builtin_kind.is_some() {
        Some(UnitKind::Builtin)
    } else if unit.blocks.is_empty() {
        Some(UnitKind::Extern)
    } else {
        None
    };
    if let Some(kind) = sentinel_kind {
        let info = UnitInfo {
            kind,
            code_start: 0,
            frame_bytes: 0,
            param_count: unit.fn_type.params.len(),
            ret_pt: unit.fn_type.return_type,
            diverges: unit.fn_type.diverges,
        };
        k1.bc.functions.insert(function_id, info);
        return Ok(info);
    }
    lower_unit(k1, unit)
}

pub fn get_or_lower_expr(
    k1: &mut TypedProgram,
    expr_id: TypedExprId,
    span: SpanId,
) -> K1Result<UnitInfo> {
    if let Some(info) = k1.bc.exprs.get(&expr_id) {
        return Ok(*info);
    }
    let Some(unit) = k1.ir.exprs.get(&expr_id).copied() else {
        return failf!(span, "Expr unit was never compiled to ir");
    };
    lower_unit(k1, unit)
}

struct PendingTramp {
    /// buf index of the JumpIf operand to patch with this trampoline's pc
    fixup_at: u32,
    from: BlockId,
    target: BlockId,
}

pub(crate) struct LowerCtx {
    ret_pt: PhysicalType,
    /// Total frame size in bytes, 16-aligned; known after pass A
    frame_bytes: u32,
    scratch0: u32,
    scratch1: u32,
    scratch_flip: bool,

    slots: FxHashMap<InstId, u32>,
    fwd: FxHashMap<InstId, ir::Value>,
    alloca_offsets: FxHashMap<InstId, u32>,
    call_temp_offsets: FxHashMap<InstId, u32>,
    /// Flat (block, phi) pairs in block-then-phi order; blocks with phis are
    /// rare, so edge-copy emission just scans this
    block_phis: Vec<(BlockId, InstId)>,
    /// Pass A worklists for frame layout
    allocas: Vec<(InstId, Layout)>,
    agg_call_temps: Vec<(InstId, Layout)>,

    buf: Vec<u32>,
    block_pcs: FxHashMap<BlockId, u32>,
    /// buf indices whose value is a local pc that must be shifted by the
    /// final append base
    relocs: Vec<u32>,
    /// buf indices to patch with a block's local pc
    block_fixups: Vec<(u32, BlockId)>,
    /// buf indices to patch with a (recursive) callee's absolute code_start
    call_fixups: Vec<(u32, FunctionId)>,
    trampolines: Vec<PendingTramp>,
    spans: Vec<(u32, SpanId)>,
    cur_span: SpanId,
}

impl LowerCtx {
    pub(crate) fn make() -> LowerCtx {
        LowerCtx {
            ret_pt: PhysicalType::EMPTY,
            frame_bytes: 0,
            scratch0: 0,
            scratch1: 0,
            scratch_flip: false,
            slots: FxHashMap::default(),
            fwd: FxHashMap::default(),
            alloca_offsets: FxHashMap::default(),
            call_temp_offsets: FxHashMap::default(),
            block_phis: Vec::new(),
            allocas: Vec::new(),
            agg_call_temps: Vec::new(),
            buf: Vec::with_capacity(1024),
            block_pcs: FxHashMap::default(),
            relocs: Vec::new(),
            block_fixups: Vec::new(),
            call_fixups: Vec::new(),
            trampolines: Vec::new(),
            spans: Vec::new(),
            cur_span: SpanId::NONE,
        }
    }

    fn reset(&mut self, ret_pt: PhysicalType) {
        self.ret_pt = ret_pt;
        self.frame_bytes = 0;
        self.scratch0 = 0;
        self.scratch1 = 0;
        self.scratch_flip = false;
        self.slots.clear();
        self.fwd.clear();
        self.alloca_offsets.clear();
        self.call_temp_offsets.clear();
        self.block_phis.clear();
        self.allocas.clear();
        self.agg_call_temps.clear();
        self.buf.clear();
        self.block_pcs.clear();
        self.relocs.clear();
        self.block_fixups.clear();
        self.call_fixups.clear();
        self.trampolines.clear();
        self.spans.clear();
        self.cur_span = SpanId::NONE;
    }

    fn pc(&self) -> u32 {
        self.buf.len() as u32
    }

    fn emit(&mut self, op: Opcode, a: u8, b: u16) {
        self.buf.push(header(op, a, b));
    }

    fn push(&mut self, w: u32) {
        self.buf.push(w);
    }

    fn push_block_target(&mut self, b: BlockId) {
        let at = self.buf.len() as u32;
        self.buf.push(0);
        self.block_fixups.push((at, b));
        self.relocs.push(at);
    }

    /// Reset scratch alternation; call before resolving a new instruction's operands
    fn begin_inst(&mut self) {
        self.scratch_flip = false;
    }

    fn next_scratch(&mut self) -> u32 {
        let s = if self.scratch_flip { self.scratch1 } else { self.scratch0 };
        self.scratch_flip = !self.scratch_flip;
        s
    }

    fn slot_of(&self, inst_id: InstId) -> u32 {
        *self
            .slots
            .get(&inst_id)
            .unwrap_or_else(|| panic!("bc lowering: no slot for value inst i{}", inst_id.as_u32()))
    }

    fn block_has_phis(&self, b: BlockId) -> bool {
        self.block_phis.iter().any(|(pb, _)| *pb == b)
    }

    /// Word index of the k-th out-arg (the callee's future param slot k)
    fn out_arg_word(&self, k: u32) -> u32 {
        self.frame_bytes / 8 + FRAME_HEADER_WORDS + k
    }

    /// Word index of the callee's future sret header word
    fn out_sret_word(&self) -> u32 {
        self.frame_bytes / 8 + 2
    }

    fn record_span(&mut self, span: SpanId) {
        if span != self.cur_span {
            self.spans.push((self.pc(), span));
            self.cur_span = span;
        }
    }
}

const fn align_up(v: u32, align: u32) -> u32 {
    v.next_multiple_of(align)
}

/// Data32 semantics copied verbatim from vm.rs `resolve_value` — including
/// the F64 numeric u32->f32->f64 conversion and I64 sign extension.
fn const_of_data32(t: ScalarType, data: u32) -> u64 {
    match t {
        ScalarType::F32 => data as u64,
        ScalarType::F64 => (data as f32 as f64).to_bits(),
        ScalarType::Pointer => data as u64,
        ScalarType::I8 | ScalarType::I16 | ScalarType::I32 => data as u64,
        ScalarType::I64 => data as i32 as i64 as u64,
        ScalarType::U8 | ScalarType::U16 | ScalarType::U32 | ScalarType::U64 => data as u64,
    }
}

fn const_of_data(imm: DataInst) -> u64 {
    match imm {
        DataInst::U64(v) => v,
        DataInst::I64(v) => v as u64,
        DataInst::Float(TypedFloatValue::F32(f)) => f.to_bits() as u64,
        DataInst::Float(TypedFloatValue::F64(f)) => f.to_bits(),
    }
}

/// Resolve an ir operand to a tagged src word. May emit a `LoadGlobal` into a
/// scratch slot (globals are lazy and per-VM-mutable, so they cannot be baked).
/// Everything else becomes a frame word index or a baked constant.
fn resolve_src(k1: &mut TypedProgram, ctx: &mut LowerCtx, value: ir::Value) -> u32 {
    // Chase forwarding chains (BitCast/IntExtU/PtrToWord/WordToPtr are no-ops)
    let mut value = value;
    while let ir::Value::Inst(id) = value {
        match ctx.fwd.get(&id) {
            Some(fwd) => value = *fwd,
            None => break,
        }
    }
    match value {
        ir::Value::Inst(inst_id) => {
            if let Some(slot) = ctx.slots.get(&inst_id) {
                return *slot;
            }
            match *k1.ir.instrs.get(inst_id) {
                Inst::Data(imm) => k1.bc.intern_const(const_of_data(imm)),
                // Value-kind insts without slots are empty-typed; reads are 0
                // (parity with the old VM's default-0 for never-written insts)
                _ => k1.bc.intern_const(0),
            }
        }
        ir::Value::FnParam { index, .. } => FRAME_HEADER_WORDS + index,
        ir::Value::Data32 { t, data } => k1.bc.intern_const(const_of_data32(t, data)),
        ir::Value::StaticValue { id, .. } => {
            // Memoized; materializes into the shared k1.vm_static_stack, so
            // the address is stable and valid across every VM.
            let v = vm::static_value_to_vm_value(k1, id, ctx.cur_span);
            k1.bc.intern_const(v.bits())
        }
        ir::Value::FunctionAddr(function_id) => {
            // Be sure to lower functions whose addresses have been taken
            if !k1.bc.functions.contains_key(&function_id)
                && !k1.bc.in_progress.contains(&function_id)
            {
                if let Err(e) = get_or_lower_function(k1, function_id, ctx.cur_span) {
                    debug!("[bc] deferred: address-taken function failed to lower: {}", e.message);
                }
            }
            k1.bc.intern_const(function_id.as_u32() as u64)
        }
        ir::Value::Empty => k1.bc.intern_const(0),
        ir::Value::GlobalAddr { storage_pt, id } => {
            let scratch = ctx.next_scratch();
            ctx.emit(Opcode::LoadGlobal, 0, 0);
            ctx.push(scratch);
            ctx.push(id.as_u32());
            ctx.push(storage_pt.to_u32());
            scratch
        }
    }
}

fn lower_unit(k1: &mut TypedProgram, unit: IrUnit) -> K1Result<UnitInfo> {
    let mut ctx = k1.bc.lower_ctx_pool.pop().unwrap_or_else(LowerCtx::make);
    let result = lower_unit_with_ctx(k1, unit, &mut ctx);
    k1.bc.lower_ctx_pool.push(ctx);
    if result.is_err() {
        // Don't leave a failed unit marked in-progress; a later attempt (or
        // an unrelated unit's recursion check) must not see stale state.
        if let IrUnitId::Function(fid) = unit.unit_id {
            k1.bc.in_progress.remove(&fid);
        }
    }
    result
}

fn lower_unit_with_ctx(
    k1: &mut TypedProgram,
    unit: IrUnit,
    ctx: &mut LowerCtx,
) -> K1Result<UnitInfo> {
    let unit_id = unit.unit_id;
    if let IrUnitId::Function(fid) = unit_id {
        k1.bc.in_progress.insert(fid);
    }

    #[cfg(debug_assertions)]
    validate_unit_shape(k1, &unit);

    let param_count = unit.fn_type.params.len();
    let ret_pt = unit.fn_type.return_type;
    ctx.reset(ret_pt);
    ctx.buf.reserve(unit.inst_count as usize * 4);

    // ------------------------- Pass A: analyze -------------------------
    // The unit is immutable at this point (ir-gen and iropt have run), so we
    // walk the dlists by cursor, copying each node out — no borrows held.
    let mut next_word: u32 = FRAME_HEADER_WORDS + param_count;

    let mut block_h = unit.blocks.first;
    while !block_h.is_nil() {
        let block_node = *k1.ir.mem.get(block_h);
        let mut inst_h = block_node.data.instrs.first;
        while !inst_h.is_nil() {
            let inst_node = *k1.ir.mem.get(inst_h);
            let inst_id = inst_node.data;
            let inst = *k1.ir.instrs.get(inst_id);
            match inst {
                Inst::Data(_) => {}
                Inst::BitCast { v, .. }
                | Inst::IntExtU { v, .. }
                | Inst::PtrToWord { v }
                | Inst::WordToPtr { v } => {
                    ctx.fwd.insert(inst_id, v);
                }
                Inst::Alloca { vm_layout, .. } => {
                    ctx.slots.insert(inst_id, next_word);
                    next_word += 1;
                    ctx.allocas.push((inst_id, vm_layout));
                }
                Inst::Phi { t, .. } => {
                    ctx.block_phis.push((block_h, inst_id));
                    if !t.is_empty() {
                        ctx.slots.insert(inst_id, next_word);
                        next_word += 1;
                    }
                }
                Inst::Call { call_id } => {
                    let call = *k1.ir.calls.get(call_id);
                    if !call.ret_type.is_empty() {
                        ctx.slots.insert(inst_id, next_word);
                        next_word += 1;
                    } else {
                        debug_assert!(call.dst.is_none(), "call with dst but empty return type");
                    }
                    if call.dst.is_none() && call.ret_type.is_agg() {
                        let layout = k1.types.get_pt_layout(call.ret_type);
                        ctx.agg_call_temps.push((inst_id, layout));
                    }
                }
                _ => {
                    if let InstKind::Value(pt) = ir::get_inst_kind(&k1.ir, &k1.types, inst_id) {
                        if !pt.is_empty() {
                            ctx.slots.insert(inst_id, next_word);
                            next_word += 1;
                        }
                    }
                }
            }
            inst_h = inst_node.next;
        }
        block_h = block_node.next;
    }

    ctx.scratch0 = next_word;
    ctx.scratch1 = next_word + 1;
    next_word += 2;

    // Frame layout: [header|params|slots|scratch] then allocas, then agg temps
    let mut area_bytes: u32 = next_word * 8;
    for i in 0..ctx.allocas.len() {
        let (inst_id, layout) = ctx.allocas[i];
        let align = layout.align.max(1);
        assert!(
            align <= 16,
            "bc: alloca alignment {} > 16 unsupported (frame base is 16-aligned)",
            align
        );
        area_bytes = align_up(area_bytes, align);
        ctx.alloca_offsets.insert(inst_id, area_bytes);
        area_bytes += layout.size;
    }
    for i in 0..ctx.agg_call_temps.len() {
        let (inst_id, layout) = ctx.agg_call_temps[i];
        let align = layout.align.max(1);
        assert!(align <= 16, "bc: agg return temp alignment {} > 16 unsupported", align);
        area_bytes = align_up(area_bytes, align);
        ctx.call_temp_offsets.insert(inst_id, area_bytes);
        area_bytes += layout.size;
    }
    ctx.frame_bytes = align_up(area_bytes.max(16), 16);

    // -------------------------- Pass B: emit ---------------------------
    let mut first = true;
    let mut block_h = unit.blocks.first;
    while !block_h.is_nil() {
        let block_node = *k1.ir.mem.get(block_h);
        ctx.block_pcs.insert(block_h, ctx.pc());
        if first {
            ctx.emit(Opcode::Enter, 0, 0);
            let fb = ctx.frame_bytes;
            ctx.push(fb);
            first = false;
        }
        let mut inst_h = block_node.data.instrs.first;
        while !inst_h.is_nil() {
            let inst_node = *k1.ir.mem.get(inst_h);
            emit_inst(k1, ctx, block_h, inst_node.data)?;
            inst_h = inst_node.next;
        }
        block_h = block_node.next;
    }

    // Trampolines: phi edge copies for conditional jumps
    let mut tramp_i = 0;
    while tramp_i < ctx.trampolines.len() {
        let PendingTramp { fixup_at, from, target } = ctx.trampolines[tramp_i];
        tramp_i += 1;
        let pc = ctx.pc();
        ctx.buf[fixup_at as usize] = pc;
        emit_phi_copies(k1, ctx, from, target);
        ctx.emit(Opcode::Jump, 0, 0);
        ctx.push_block_target(target);
    }

    // -------------------------- Finalize -------------------------------
    for i in 0..ctx.block_fixups.len() {
        let (at, block_id) = ctx.block_fixups[i];
        let Some(target_pc) = ctx.block_pcs.get(&block_id) else {
            panic!("bc lowering: jump to unemitted block b{}", block_id.raw_index());
        };
        ctx.buf[at as usize] = *target_pc;
    }

    let base = k1.bc.code.len() as u32;
    for at in &ctx.relocs {
        ctx.buf[*at as usize] += base;
    }
    k1.bc.code.extend_from_slice(&ctx.buf);
    let end = k1.bc.code.len() as u32;

    for (local_pc, span) in &ctx.spans {
        k1.bc.spans.push((local_pc + base, *span));
    }
    k1.bc.unit_ranges.push((base, end, unit_id));

    for i in 0..ctx.call_fixups.len() {
        let (at, callee) = ctx.call_fixups[i];
        k1.bc.pending_call_fixups.entry(callee).or_default().push(at + base);
    }

    let info = UnitInfo {
        kind: UnitKind::Body,
        code_start: base,
        frame_bytes: ctx.frame_bytes,
        param_count,
        ret_pt,
        diverges: unit.fn_type.diverges,
    };

    match unit_id {
        IrUnitId::Function(fid) => {
            k1.bc.functions.insert(fid, info);
            k1.bc.in_progress.remove(&fid);
            if let Some(waiting) = k1.bc.pending_call_fixups.remove(&fid) {
                for at in waiting {
                    debug_assert_eq!(k1.bc.code[at as usize], PENDING_PC);
                    k1.bc.code[at as usize] = base;
                }
            }
        }
        IrUnitId::Expr(eid) => {
            k1.bc.exprs.insert(eid, info);
        }
    }

    if unit.is_debug {
        eprintln!("[bc] lowered unit ({} words, frame {} bytes)", end - base, ctx.frame_bytes);
        eprintln!("{}", super::disasm::disasm_range(k1, base, end));
    } else {
        debug!("[bc] lowered unit at {}..{} frame_bytes={}", base, end, ctx.frame_bytes);
    }

    Ok(info)
}

fn emit_inst(
    k1: &mut TypedProgram,
    ctx: &mut LowerCtx,
    block_id: BlockId,
    inst_id: InstId,
) -> K1Result<()> {
    let inst = *k1.ir.instrs.get(inst_id);
    let span = *k1.ir.sources.get(inst_id);
    ctx.record_span(span);
    ctx.begin_inst();

    macro_rules! binop {
        ($op:expr, $a:expr, $b:expr, $lhs:expr, $rhs:expr) => {{
            let lhs = resolve_src(k1, ctx, $lhs);
            let rhs = resolve_src(k1, ctx, $rhs);
            let dst = ctx.slot_of(inst_id);
            ctx.emit($op, $a, $b);
            ctx.push(dst);
            ctx.push(lhs);
            ctx.push(rhs);
        }};
    }

    macro_rules! unop {
        ($op:expr, $a:expr, $b:expr, $v:expr) => {{
            let src = resolve_src(k1, ctx, $v);
            let dst = ctx.slot_of(inst_id);
            ctx.emit($op, $a, $b);
            ctx.push(dst);
            ctx.push(src);
        }};
    }

    macro_rules! cast {
        ($kind:expr, $from:expr, $to:expr, $v:expr) => {{
            let b: u16 = (($from as u16) << 8) | ($to as u16);
            unop!(Opcode::Cast, $kind as u8, b, $v)
        }};
    }

    match inst {
        // No code: constants and no-op conversions are handled at operand
        // resolution; phis are handled as edge copies.
        Inst::Data(_) => {}
        Inst::BitCast { .. }
        | Inst::IntExtU { .. }
        | Inst::PtrToWord { .. }
        | Inst::WordToPtr { .. } => {}
        Inst::Phi { .. } => {}

        Inst::Alloca { .. } => {
            let offset = *ctx.alloca_offsets.get(&inst_id).unwrap();
            let dst = ctx.slot_of(inst_id);
            ctx.emit(Opcode::Lea, 0, 0);
            ctx.push(dst);
            ctx.push(offset);
        }
        Inst::Store { dst, value, t } => {
            let addr = resolve_src(k1, ctx, dst);
            let val = resolve_src(k1, ctx, value);
            ctx.emit(Opcode::Store, t.to_tag(), 0);
            ctx.push(addr);
            ctx.push(val);
        }
        Inst::Load { t, src } => {
            let addr = resolve_src(k1, ctx, src);
            let dst = ctx.slot_of(inst_id);
            ctx.emit(Opcode::Load, t.to_tag(), 0);
            ctx.push(dst);
            ctx.push(addr);
        }
        Inst::Copy { dst, src, vm_size, .. } => {
            let dst_addr = resolve_src(k1, ctx, dst);
            let src_addr = resolve_src(k1, ctx, src);
            ctx.emit(Opcode::Copy, 0, 0);
            ctx.push(dst_addr);
            ctx.push(src_addr);
            ctx.push(vm_size);
        }
        Inst::StructOffset { base, vm_offset, .. } => {
            let base_src = resolve_src(k1, ctx, base);
            let dst = ctx.slot_of(inst_id);
            ctx.emit(Opcode::PtrAddImm, 0, 0);
            ctx.push(dst);
            ctx.push(base_src);
            ctx.push(vm_offset);
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            let base_src = resolve_src(k1, ctx, base);
            let idx_src = resolve_src(k1, ctx, element_index);
            let stride = k1.types.get_pt_layout(element_t).stride();
            let dst = ctx.slot_of(inst_id);
            ctx.emit(Opcode::PtrIndex, 0, 0);
            ctx.push(dst);
            ctx.push(base_src);
            ctx.push(idx_src);
            ctx.push(stride);
        }
        Inst::Call { call_id } => {
            emit_call(k1, ctx, inst_id, call_id)?;
        }
        Inst::Jump(target) => {
            emit_phi_copies(k1, ctx, block_id, target);
            ctx.emit(Opcode::Jump, 0, 0);
            ctx.push_block_target(target);
        }
        Inst::JumpIf { cond, cons, alt } => {
            let cond_src = resolve_src(k1, ctx, cond);
            ctx.emit(Opcode::JumpIf, 0, 0);
            ctx.push(cond_src);
            for target in [cons, alt] {
                if ctx.block_has_phis(target) {
                    let at = ctx.buf.len() as u32;
                    ctx.push(0);
                    ctx.relocs.push(at);
                    ctx.trampolines.push(PendingTramp { fixup_at: at, from: block_id, target });
                } else {
                    ctx.push_block_target(target);
                }
            }
        }
        Inst::Unreachable => {
            ctx.emit(Opcode::Unreachable, 0, 0);
        }
        Inst::Ret { v, agg: _ } => {
            let ret_pt = ctx.ret_pt;
            if ret_pt.is_agg() {
                let size = k1.types.get_pt_layout(ret_pt).size;
                let src = resolve_src(k1, ctx, v);
                ctx.emit(Opcode::RetAgg, 0, 0);
                ctx.push(src);
                ctx.push(size);
            } else {
                // Empty returns arrive as `Ret { v: Empty }` and resolve to
                // const 0; nobody reads ret_reg for them
                let src = resolve_src(k1, ctx, v);
                ctx.emit(Opcode::Ret, 0, 0);
                ctx.push(src);
            }
        }

        Inst::BoolNegate { v } => unop!(Opcode::BoolNegate, 0, 0, v),
        Inst::BitNot { v } => unop!(Opcode::BitNot, 0, 0, v),

        Inst::IntTrunc { v, to } => cast!(CastKind::IntTrunc, 0, to.to_tag(), v),
        Inst::IntExtS { v, from, to } => cast!(CastKind::IntExtS, from.to_tag(), to.to_tag(), v),
        Inst::FloatTrunc { v, .. } => cast!(CastKind::FloatTrunc, 0, 0, v),
        Inst::FloatExt { v, .. } => cast!(CastKind::FloatExt, 0, 0, v),
        Inst::Float32ToIntUnsigned { v, to } => cast!(CastKind::F32ToUInt, 0, to.to_tag(), v),
        Inst::Float32ToIntSigned { v, to } => cast!(CastKind::F32ToSInt, 0, to.to_tag(), v),
        Inst::Float64ToIntUnsigned { v, to } => cast!(CastKind::F64ToUInt, 0, to.to_tag(), v),
        Inst::Float64ToIntSigned { v, to } => cast!(CastKind::F64ToSInt, 0, to.to_tag(), v),
        Inst::IntToFloatUnsigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::UIntToF32 } else { CastKind::UIntToF64 };
            cast!(kind, from.to_tag(), to.to_tag(), v)
        }
        Inst::IntToFloatSigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::SIntToF32 } else { CastKind::SIntToF64 };
            cast!(kind, from.to_tag(), to.to_tag(), v)
        }

        Inst::IntAdd { lhs, rhs, width } => binop!(Opcode::IntAdd, width, 0, lhs, rhs),
        Inst::IntSub { lhs, rhs, width } => binop!(Opcode::IntSub, width, 0, lhs, rhs),
        Inst::IntMul { lhs, rhs, width } => binop!(Opcode::IntMul, width, 0, lhs, rhs),
        Inst::IntDivUnsigned { lhs, rhs, width } => binop!(Opcode::IntDivU, width, 0, lhs, rhs),
        Inst::IntDivSigned { lhs, rhs, width } => binop!(Opcode::IntDivS, width, 0, lhs, rhs),
        Inst::IntRemUnsigned { lhs, rhs, width } => binop!(Opcode::IntRemU, width, 0, lhs, rhs),
        Inst::IntRemSigned { lhs, rhs, width } => binop!(Opcode::IntRemS, width, 0, lhs, rhs),
        Inst::IntCmp { lhs, rhs, pred, width } => {
            binop!(Opcode::IntCmp, width, int_pred_tag(pred), lhs, rhs)
        }
        Inst::FloatAdd { lhs, rhs, width } => binop!(Opcode::FloatAdd, width, 0, lhs, rhs),
        Inst::FloatSub { lhs, rhs, width } => binop!(Opcode::FloatSub, width, 0, lhs, rhs),
        Inst::FloatMul { lhs, rhs, width } => binop!(Opcode::FloatMul, width, 0, lhs, rhs),
        Inst::FloatDiv { lhs, rhs, width } => binop!(Opcode::FloatDiv, width, 0, lhs, rhs),
        Inst::FloatRem { lhs, rhs, width } => binop!(Opcode::FloatRem, width, 0, lhs, rhs),
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            binop!(Opcode::FloatCmp, width, float_pred_tag(pred), lhs, rhs)
        }
        Inst::BitAnd { lhs, rhs, width } => binop!(Opcode::BitAnd, width, 0, lhs, rhs),
        Inst::BitOr { lhs, rhs, width } => binop!(Opcode::BitOr, width, 0, lhs, rhs),
        Inst::BitXor { lhs, rhs, width } => binop!(Opcode::BitXor, width, 0, lhs, rhs),
        Inst::BitShiftLeft { lhs, rhs, width } => binop!(Opcode::Shl, width, 0, lhs, rhs),
        Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
            binop!(Opcode::ShrU, width, 0, lhs, rhs)
        }
        Inst::BitSignedShiftRight { lhs, rhs, width } => binop!(Opcode::ShrS, width, 0, lhs, rhs),

        Inst::BakeStaticValue { type_id, value } => {
            let src = resolve_src(k1, ctx, value);
            let dst = ctx.slot_of(inst_id);
            ctx.emit(Opcode::BakeStaticValue, 0, 0);
            ctx.push(dst);
            ctx.push(type_id.as_u32());
            ctx.push(src);
        }
    }
    Ok(())
}

fn emit_call(
    k1: &mut TypedProgram,
    ctx: &mut LowerCtx,
    inst_id: InstId,
    call_id: ir::IrCallId,
) -> K1Result<()> {
    let call = *k1.ir.calls.get(call_id);
    let ret_pt = call.ret_type;
    let is_agg = ret_pt.is_agg();
    let args: &[ir::Value] = k1.ir.mem.getn(call.args);
    let nargs = args.len() as u32;
    let frame_bytes = ctx.frame_bytes;

    // 1. Write args into the callee's future param slots
    for (k, arg) in args.iter().enumerate() {
        ctx.begin_inst();
        let src = resolve_src(k1, ctx, *arg);
        let dst = ctx.out_arg_word(k as u32);
        ctx.emit(Opcode::Mov, 0, 0);
        ctx.push(dst);
        ctx.push(src);
    }

    // 2. Return-destination setup. Mirrors the old VM: the call's slot holds
    //    the destination *address* when there is one (dst given, or agg
    //    return via a statically reserved temp), otherwise the scalar result.
    let call_slot = if ret_pt.is_empty() { None } else { Some(ctx.slot_of(inst_id)) };
    match call.dst {
        Some(dst_value) => {
            ctx.begin_inst();
            let dst_src = resolve_src(k1, ctx, dst_value);
            let call_slot = call_slot.expect("call with dst but empty ret");
            ctx.emit(Opcode::Mov, 0, 0);
            ctx.push(call_slot);
            ctx.push(dst_src);
            if is_agg {
                let sret = ctx.out_sret_word();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(sret);
                ctx.push(call_slot);
            }
        }
        None if is_agg => {
            let temp_off = *ctx.call_temp_offsets.get(&inst_id).unwrap();
            let call_slot = call_slot.expect("agg call ret with no slot");
            ctx.emit(Opcode::Lea, 0, 0);
            ctx.push(call_slot);
            ctx.push(temp_off);
            let sret = ctx.out_sret_word();
            ctx.emit(Opcode::Mov, 0, 0);
            ctx.push(sret);
            ctx.push(call_slot);
        }
        None => {}
    }

    // 3. The call itself
    match call.callee {
        IrCallee::Direct(function_id) => {
            if k1.bc.in_progress.contains(&function_id) {
                // Recursion cycle: patch when the callee's code_start lands
                ctx.emit(Opcode::Call, 0, 0);
                let at = ctx.buf.len() as u32;
                ctx.push(PENDING_PC);
                ctx.call_fixups.push((at, function_id));
                ctx.push(frame_bytes);
            } else {
                let info = get_or_lower_function(k1, function_id, ctx.cur_span)?;
                if info.kind != UnitKind::Body {
                    return failf!(
                        ctx.cur_span,
                        "Direct call to bodyless ({:?}) function: {}",
                        info.kind,
                        k1.function_id_to_string(function_id, false)
                    );
                }
                ctx.emit(Opcode::Call, 0, 0);
                ctx.push(info.code_start);
                ctx.push(frame_bytes);
            }
        }
        IrCallee::Indirect(_, fn_value) => {
            ctx.begin_inst();
            let fn_src = resolve_src(k1, ctx, fn_value);
            ctx.emit(Opcode::CallIndirect, 0, 0);
            ctx.push(fn_src);
            ctx.push(frame_bytes);
        }
        IrCallee::Extern { library_name, function_name, function_id } => {
            ctx.emit(Opcode::CallExtern, 0, 0);
            ctx.push(function_id.as_u32());
            // StringIds are 0-based indices; bias by 1 so 0 can mean "none"
            ctx.push(library_name.map(|s| s.as_usize() as u32 + 1).unwrap_or(0));
            ctx.push(function_name.as_usize() as u32 + 1);
            ctx.push(ret_pt.to_u32());
            ctx.push(frame_bytes);
            ctx.push(nargs);
        }
        IrCallee::BackendBuiltin(_, builtin) => {
            ctx.emit(Opcode::CallBuiltin, builtin_tag(builtin), 0);
            ctx.push(ret_pt.to_u32());
            ctx.push(frame_bytes);
            ctx.push(nargs);
        }
    }

    // 4. Result delivery for scalar returns (agg results were written through
    //    sret by the callee/handler; empty returns produce nothing).
    if !ret_pt.is_empty() && !is_agg {
        let call_slot = call_slot.unwrap();
        match call.dst {
            Some(_) => {
                // The call slot holds the destination address; store through it
                let t = ret_pt.expect_scalar();
                ctx.emit(Opcode::RetStore, t.to_tag(), 0);
                ctx.push(call_slot);
            }
            None => {
                ctx.emit(Opcode::RetGet, 0, 0);
                ctx.push(call_slot);
            }
        }
    }

    Ok(())
}

/// Sequential phi copies for the CFG edge `from -> target`, in phi order —
/// matching the old VM, which executes phis sequentially (vm.rs Inst::Phi).
fn emit_phi_copies(k1: &mut TypedProgram, ctx: &mut LowerCtx, from: BlockId, target: BlockId) {
    let mut i = 0;
    while i < ctx.block_phis.len() {
        let (phi_block, phi_id) = ctx.block_phis[i];
        i += 1;
        if phi_block != target {
            continue;
        }
        let Inst::Phi { incomings, .. } = *k1.ir.instrs.get(phi_id) else {
            unreachable!("non-phi in block_phis")
        };
        let Some(dst) = ctx.slots.get(&phi_id).copied() else {
            continue; // empty-typed phi
        };
        let case = k1.ir.mem.getn(incomings).iter().find(|c| c.from == from).copied();
        ctx.begin_inst();
        let src = match case {
            Some(case) => resolve_src(k1, ctx, case.value),
            None => {
                // The old VM is UB (assume_init) here; we pick 0 and warn.
                if cfg!(debug_assertions) {
                    eprintln!(
                        "[bc] warning: phi i{} in b{} has no incoming for edge from b{}; using 0",
                        phi_id.as_u32(),
                        target.raw_index(),
                        from.raw_index()
                    );
                }
                k1.bc.intern_const(0)
            }
        };
        ctx.emit(Opcode::Mov, 0, 0);
        ctx.push(dst);
        ctx.push(src);
    }
}

/// Debug-only structural checks: phis should sit at the front of their block
/// and allocas in the entry block. Violations are warnings (the lowering
/// handles both shapes defensively) — but they indicate latent iropt bugs.
#[cfg(debug_assertions)]
fn validate_unit_shape(k1: &TypedProgram, unit: &IrUnit) {
    let mut block_h = unit.blocks.first;
    let mut is_entry = true;
    while !block_h.is_nil() {
        let block_node = *k1.ir.mem.get(block_h);
        let mut seen_non_phi = false;
        let mut inst_h = block_node.data.instrs.first;
        while !inst_h.is_nil() {
            let inst_node = *k1.ir.mem.get(inst_h);
            let inst_id = inst_node.data;
            match k1.ir.instrs.get(inst_id) {
                Inst::Phi { .. } => {
                    if seen_non_phi {
                        eprintln!(
                            "[bc] validator: mid-block phi i{} in b{} of {:?}",
                            inst_id.as_u32(),
                            block_h.raw_index(),
                            unit.unit_id
                        );
                    }
                }
                Inst::Alloca { .. } => {
                    seen_non_phi = true;
                    if !is_entry {
                        eprintln!(
                            "[bc] validator: alloca i{} outside entry block in {:?}",
                            inst_id.as_u32(),
                            unit.unit_id
                        );
                    }
                }
                _ => seen_non_phi = true,
            }
            inst_h = inst_node.next;
        }
        is_entry = false;
        block_h = block_node.next;
    }
}
