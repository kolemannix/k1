// Copyright (c) 2026 knix
// All rights reserved.

//! Lowering from `ir` units to the flat `bc` stream.
//!
//! Per unit, three passes:
//! - Pass A (analyze): assign a dense frame-word slot to every value-producing
//!   instruction, map each no-op inst to the value it is the same as, collect
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

use crate::debug;
use fxhash::{FxHashMap, FxHashSet};

use crate::ir::{self, BlockId, DataInst, Inst, InstId, InstKind, IrCallee, IrUnit, IrUnitId};
use crate::kbail;
use crate::lex::SpanId;
use crate::typer::types::{Layout, PhysicalType, PhysicalTypeEnum, ScalarType};
use crate::typer::{FunctionId, K1Result, TypedExprId, TypedFloatValue, TypedProgram};
use crate::vm;

use super::{
    CastKind, FRAME_ALIGN, FRAME_HEADER_WORDS, Opcode, PENDING_PC, UnitInfo, UnitKind,
    VALUE_MASK_FRAME_OFFSET, builtin_tag, header,
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
    let Some(unit) = k1.ir.functions.get(&function_id).copied() else {
        kbail!(
            k1,
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
        kbail!(k1, span, "Expr unit was never compiled to ir");
    };
    lower_unit(k1, unit)
}

struct PendingTramp {
    /// the JumpIf operand to patch with this trampoline's pc
    operand: u32,
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

    /// What each inst lowered to, one rung below `ir::Value`: a frame word
    /// index, or `VALUE_MASK_FRAME_OFFSET | byte offset` for allocas and agg call temps
    /// (their frame address is baked into consumers; nothing materializes it)
    inst_to_frame_value: FxHashMap<InstId, u32>,
    /// Insts that own no bc value
    inst_to_parent_value: FxHashMap<InstId, ir::Value>,
    /// StructOffsets that emit nothing: every use is a scalar Load address or
    /// Store destination, so the byte offset is baked into those instead
    folded_struct_offsets: FxHashMap<InstId, (ir::Value, u16)>,
    /// Uses of each inst within the unit, from `ir::count_uses`
    use_counts: FxHashMap<InstId, u32>,
    /// Of those uses, the ones in a scalar Load address or Store destination
    /// position; a StructOffset folds when the two counts agree
    addr_use_counts: FxHashMap<InstId, u32>,
    /// IntCmps that emit nothing: the sole use is the `JumpIf` terminating
    /// their own block, so the compare rides that branch as `JumpIfIntCmp`
    fused_cmps: FxHashSet<InstId>,
    call_arg_values: Vec<u32>,
    /// Flat (block, phi) pairs in block-then-phi order; blocks with phis are
    /// rare, so edge-copy emission just scans this
    phis: Vec<(BlockId, InstId)>,
    /// Pass A worklists for frame layout
    allocas: Vec<(InstId, Layout)>,
    agg_call_temps: Vec<(InstId, Layout)>,

    bc_out: Vec<u32>,
    block_to_entry_pc: FxHashMap<BlockId, u32>,
    /// Operands holding a local pc that must be shifted by the final append base
    pc_operands_to_rebase: Vec<u32>,
    /// Operands to patch with a block's entry pc
    operand_to_block: Vec<(u32, BlockId)>,
    /// Operands to patch with a (recursive) callee's absolute code_start
    operand_to_callee: Vec<(u32, FunctionId)>,
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
            inst_to_frame_value: FxHashMap::default(),
            inst_to_parent_value: FxHashMap::default(),
            folded_struct_offsets: FxHashMap::default(),
            use_counts: FxHashMap::default(),
            addr_use_counts: FxHashMap::default(),
            fused_cmps: FxHashSet::default(),
            call_arg_values: Vec::new(),
            phis: Vec::new(),
            allocas: Vec::new(),
            agg_call_temps: Vec::new(),
            bc_out: Vec::with_capacity(1024),
            block_to_entry_pc: FxHashMap::default(),
            pc_operands_to_rebase: Vec::new(),
            operand_to_block: Vec::new(),
            operand_to_callee: Vec::new(),
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
        self.inst_to_frame_value.clear();
        self.inst_to_parent_value.clear();
        self.folded_struct_offsets.clear();
        self.use_counts.clear();
        self.addr_use_counts.clear();
        self.fused_cmps.clear();
        self.call_arg_values.clear();
        self.phis.clear();
        self.allocas.clear();
        self.agg_call_temps.clear();
        self.bc_out.clear();
        self.block_to_entry_pc.clear();
        self.pc_operands_to_rebase.clear();
        self.operand_to_block.clear();
        self.operand_to_callee.clear();
        self.trampolines.clear();
        self.spans.clear();
        self.cur_span = SpanId::NONE;
    }

    fn pc(&self) -> u32 {
        self.bc_out.len() as u32
    }

    fn emit(&mut self, op: Opcode, a: u8, b: u16) {
        self.bc_out.push(header(op, a, b));
    }

    fn push(&mut self, w: u32) {
        self.bc_out.push(w);
    }

    fn push_block_target(&mut self, b: BlockId) {
        let at = self.bc_out.len() as u32;
        self.bc_out.push(0);
        self.operand_to_block.push((at, b));
        self.pc_operands_to_rebase.push(at);
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

    /// Walk parent links to the value that owns storage
    fn walk_to_frame_value(&self, mut value: ir::Value) -> ir::Value {
        while let ir::Value::Inst(id) = value {
            match self.inst_to_parent_value.get(&id) {
                Some(f) => value = *f,
                None => break,
            }
        }
        value
    }

    fn bc_value_of(&self, inst_id: InstId) -> u32 {
        *self
            .inst_to_frame_value
            .get(&inst_id)
            .unwrap_or_else(|| panic!("bc lowering: no bc value for inst i{}", inst_id.as_u32()))
    }

    fn block_has_phis(&self, b: BlockId) -> bool {
        self.phis.iter().any(|(pb, _)| *pb == b)
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

fn wbits(t: ScalarType) -> u8 {
    t.width().bits() as u8
}

fn const_of_data32(t: ScalarType, data: u32) -> u64 {
    match t {
        ScalarType::F32 => data as u64,
        ScalarType::F64 => (data as f32 as f64).to_bits(),
        ScalarType::Pointer => data as u64,
        ScalarType::I8 | ScalarType::I16 | ScalarType::I32 => data as u64,
        ScalarType::I64 => data as i32 as i64 as u64,
        ScalarType::U8
        | ScalarType::U16
        | ScalarType::U32
        | ScalarType::U64
        | ScalarType::Char
        | ScalarType::Bool => data as u64,
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

fn fused_cmp_of(ctx: &LowerCtx, cond: ir::Value) -> Option<InstId> {
    let ir::Value::Inst(id) = cond else { return None };
    if ctx.fused_cmps.contains(&id) { Some(id) } else { None }
}

fn resolve_lowered_value(k1: &mut TypedProgram, ctx: &mut LowerCtx, value: ir::Value) -> u32 {
    match ctx.walk_to_frame_value(value) {
        ir::Value::Inst(inst_id) => {
            if let Some(bc_value) = ctx.inst_to_frame_value.get(&inst_id) {
                return *bc_value;
            }
            match *k1.ir.instrs.get(inst_id) {
                Inst::Data(imm) => k1.bc.intern_const(const_of_data(imm)),
                // Value-kind insts without a bc value are empty-typed; reads are 0
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
                    debug!(
                        "[bc] deferred: address-taken function failed to lower: {}",
                        k1.ident_str(e.message)
                    );
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

/// Resolve a memory operand for Load/Store, baking a folded StructOffset's
/// byte offset into the returned B value instead of an address computation.
fn resolve_addr(k1: &mut TypedProgram, ctx: &mut LowerCtx, value: ir::Value) -> (u32, u16) {
    if let ir::Value::Inst(id) = value {
        if let Some((base, off)) = ctx.folded_struct_offsets.get(&id).copied() {
            return (resolve_lowered_value(k1, ctx, base), off);
        }
    }
    (resolve_lowered_value(k1, ctx, value), 0)
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

    // ------------------------- Pass A: analyze -------------------------
    // The unit is immutable at this point (ir-gen and iropt have run), so we
    // walk the dlists by cursor, copying each node out — no borrows held.
    ir::count_uses(&k1.ir, &unit, &mut ctx.use_counts);
    let mut next_word: u32 = FRAME_HEADER_WORDS + param_count;
    let mut call_arg_words: usize = 0;
    let mut block_cmps: Vec<InstId> = Vec::new();

    let mut block_h = unit.blocks.first;
    while !block_h.is_nil() {
        let block_node = *k1.ir.mem.get(block_h);
        let mut inst_h = block_node.data.instrs.first;
        block_cmps.clear();
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
                    ctx.inst_to_parent_value.insert(inst_id, v);
                }
                Inst::Alloca { vm_layout, .. } => {
                    ctx.allocas.push((inst_id, vm_layout));
                }
                Inst::Phi { t, .. } => {
                    ctx.phis.push((block_h, inst_id));
                    if !t.is_empty() {
                        ctx.inst_to_frame_value.insert(inst_id, next_word);
                        next_word += 1;
                    }
                }
                Inst::Call { call_id } => {
                    let call = *k1.ir.calls.get(call_id);
                    call_arg_words += call.args.len() as usize * 4;
                    match call.dst {
                        Some(dst) => {
                            debug_assert!(
                                !call.ret_type.is_empty(),
                                "call with dst but empty return type"
                            );
                            // The call's value IS its destination
                            ctx.inst_to_parent_value.insert(inst_id, dst);
                        }
                        None if call.ret_type.is_agg() => {
                            // Value = the temp's frame address; encoded fp-relative
                            let layout = k1.get_pt_layout(call.ret_type);
                            ctx.agg_call_temps.push((inst_id, layout));
                        }
                        None if !call.ret_type.is_empty() => {
                            ctx.inst_to_frame_value.insert(inst_id, next_word);
                            next_word += 1;
                        }
                        None => {}
                    }
                }
                Inst::StructOffset { base, vm_offset, .. } => {
                    ctx.inst_to_frame_value.insert(inst_id, next_word);
                    next_word += 1;
                    if vm_offset <= u16::MAX as u32 {
                        ctx.folded_struct_offsets.insert(inst_id, (base, vm_offset as u16));
                    }
                }
                Inst::IntCmp { .. } => {
                    ctx.inst_to_frame_value.insert(inst_id, next_word);
                    next_word += 1;
                    block_cmps.push(inst_id);
                }
                Inst::JumpIf { cond, .. } => {
                    if let ir::Value::Inst(id) = cond {
                        if ctx.use_counts.get(&id) == Some(&1) && block_cmps.contains(&id) {
                            ctx.fused_cmps.insert(id);
                        }
                    }
                }
                _ => {
                    if let InstKind::Value(pt) = ir::get_inst_kind(&k1.ir, inst_id) {
                        if !pt.is_empty() {
                            ctx.inst_to_frame_value.insert(inst_id, next_word);
                            next_word += 1;
                        }
                    }
                }
            }
            let addr_use = match inst {
                Inst::Load { t, src, .. } if matches!(t.as_enum(), PhysicalTypeEnum::Scalar(_)) => {
                    Some(src)
                }
                Inst::Store { t, dst, .. }
                    if matches!(t.as_enum(), PhysicalTypeEnum::Scalar(_)) =>
                {
                    Some(dst)
                }
                _ => None,
            };
            if let Some(ir::Value::Inst(id)) = addr_use {
                *ctx.addr_use_counts.entry(id).or_insert(0) += 1;
            }
            inst_h = inst_node.next;
        }
        block_h = block_node.next;
    }

    // Any use outside an address position forces the StructOffset to
    // materialize as PtrAddImm
    let LowerCtx { folded_struct_offsets, addr_use_counts, use_counts, .. } = &mut *ctx;
    folded_struct_offsets.retain(|id, _| addr_use_counts.get(id) == use_counts.get(id));

    ctx.scratch0 = next_word;
    ctx.scratch1 = next_word + 1;
    next_word += 2;

    // Frame layout: [header|params|slots|scratch] then allocas, then agg temps.
    // Frame bases and sizes are 64-aligned so allocas may align up to 64
    // (512-bit vectors get natural alignment)
    let mut area_bytes: u32 = next_word * 8;
    for i in 0..ctx.allocas.len() {
        let (inst_id, layout) = ctx.allocas[i];
        let align = layout.align.max(1);
        assert!(
            align <= FRAME_ALIGN,
            "bc: alloca alignment {} > {FRAME_ALIGN} unsupported (frame base alignment)",
            align
        );
        area_bytes = align_up(area_bytes, align);
        ctx.inst_to_frame_value.insert(inst_id, VALUE_MASK_FRAME_OFFSET | area_bytes);
        area_bytes += layout.size;
    }
    for i in 0..ctx.agg_call_temps.len() {
        let (inst_id, layout) = ctx.agg_call_temps[i];
        let align = layout.align.max(1);
        assert!(
            align <= FRAME_ALIGN,
            "bc: agg return temp alignment {} > {FRAME_ALIGN} unsupported",
            align
        );
        area_bytes = align_up(area_bytes, align);
        ctx.inst_to_frame_value.insert(inst_id, VALUE_MASK_FRAME_OFFSET | area_bytes);
        area_bytes += layout.size;
    }
    ctx.frame_bytes = align_up(area_bytes.max(FRAME_ALIGN), FRAME_ALIGN);
    assert!(
        ctx.frame_bytes < VALUE_MASK_FRAME_OFFSET,
        "bc: frame too large for fp-relative operand encoding"
    );

    // -------------------------- Pass B: emit ---------------------------
    ctx.bc_out.reserve(unit.inst_count as usize * 4 + call_arg_words);
    let mut first = true;
    let mut block_h = unit.blocks.first;
    while !block_h.is_nil() {
        let block_node = *k1.ir.mem.get(block_h);
        ctx.block_to_entry_pc.insert(block_h, ctx.pc());
        if first {
            ctx.emit(Opcode::Enter, 0, 0);
            let fb = ctx.frame_bytes;
            ctx.push(fb);
            first = false;
        }
        let mut inst_h = block_node.data.instrs.first;
        while !inst_h.is_nil() {
            let inst_node = *k1.ir.mem.get(inst_h);
            emit_inst(k1, ctx, block_h, block_node.next, inst_node.data)?;
            inst_h = inst_node.next;
        }
        block_h = block_node.next;
    }

    // Trampolines: phi edge copies for conditional jumps
    let mut tramp_i = 0;
    while tramp_i < ctx.trampolines.len() {
        let PendingTramp { operand, from, target } = ctx.trampolines[tramp_i];
        tramp_i += 1;
        let pc = ctx.pc();
        ctx.bc_out[operand as usize] = pc;
        emit_phi_copies(k1, ctx, from, target);
        ctx.emit(Opcode::Jump, 0, 0);
        ctx.push_block_target(target);
    }

    // -------------------------- Finalize -------------------------------
    for i in 0..ctx.operand_to_block.len() {
        let (at, block_id) = ctx.operand_to_block[i];
        let Some(target_pc) = ctx.block_to_entry_pc.get(&block_id) else {
            panic!("bc lowering: jump to unemitted block b{}", block_id.raw_index());
        };
        ctx.bc_out[at as usize] = *target_pc;
    }

    let base = k1.bc.code.len() as u32;
    for at in &ctx.pc_operands_to_rebase {
        ctx.bc_out[*at as usize] += base;
    }
    k1.bc.code.extend_from_slice(&ctx.bc_out);
    let end = k1.bc.code.len() as u32;

    for (local_pc, span) in &ctx.spans {
        k1.bc.spans.push((local_pc + base, *span));
    }
    k1.bc.unit_ranges.push((base, end, unit_id));

    for i in 0..ctx.operand_to_callee.len() {
        let (at, callee) = ctx.operand_to_callee[i];
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
    next_block: BlockId,
    inst_id: InstId,
) -> K1Result<()> {
    let inst = *k1.ir.instrs.get(inst_id);
    let span = *k1.ir.sources.get(inst_id);
    ctx.record_span(span);
    ctx.begin_inst();

    macro_rules! binop {
        ($op:expr, $a:expr, $b:expr, $lhs:expr, $rhs:expr) => {{
            let lhs = resolve_lowered_value(k1, ctx, $lhs);
            let rhs = resolve_lowered_value(k1, ctx, $rhs);
            let dst = ctx.bc_value_of(inst_id);
            ctx.emit($op, $a, $b);
            ctx.push(dst);
            ctx.push(lhs);
            ctx.push(rhs);
        }};
    }

    macro_rules! unop {
        ($op:expr, $a:expr, $b:expr, $v:expr) => {{
            let src = resolve_lowered_value(k1, ctx, $v);
            let dst = ctx.bc_value_of(inst_id);
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
        // resolution; phis are handled as edge copies; alloca addresses are
        // baked into consumers as fp-relative operands.
        Inst::Data(_) => {}
        Inst::BitCast { .. }
        | Inst::IntExtU { .. }
        | Inst::PtrToWord { .. }
        | Inst::WordToPtr { .. } => {}
        Inst::Phi { .. } => {}
        Inst::Alloca { .. } => {}
        Inst::Store { dst, value, t, volatile: _, unaligned: _ } => match t.as_enum() {
            PhysicalTypeEnum::Scalar(t) => {
                let (addr, off) = resolve_addr(k1, ctx, dst);
                let value = resolve_lowered_value(k1, ctx, value);
                ctx.emit(Opcode::Store, wbits(t), off);
                ctx.push(addr);
                ctx.push(value);
            }
            PhysicalTypeEnum::Agg(_) => {
                let dst = resolve_lowered_value(k1, ctx, dst);
                let value = resolve_lowered_value(k1, ctx, value);
                ctx.emit(Opcode::Copy, 0, 0);
                ctx.push(dst);
                ctx.push(value);
                ctx.push(k1.get_pt_layout(t).size);
            }
            PhysicalTypeEnum::Empty => unreachable!(),
        },
        Inst::Load { t, src, dst, volatile: _, unaligned: _ } => match t.as_enum() {
            PhysicalTypeEnum::Scalar(t) => {
                debug_assert!(dst == ir::Value::Empty);
                let (addr, off) = resolve_addr(k1, ctx, src);
                let result = ctx.bc_value_of(inst_id);
                ctx.emit(Opcode::Load, wbits(t), off);
                ctx.push(result);
                ctx.push(addr);
            }
            PhysicalTypeEnum::Agg(_) => {
                let result = resolve_lowered_value(k1, ctx, dst);
                let src = resolve_lowered_value(k1, ctx, src);
                ctx.emit(Opcode::Copy, 0, 0);
                ctx.push(result);
                ctx.push(src);
                ctx.push(k1.get_pt_layout(t).size);
            }
            PhysicalTypeEnum::Empty => unreachable!(),
        },
        Inst::AtomicLoad { t, src, ord } => {
            let addr = resolve_lowered_value(k1, ctx, src);
            let dst = ctx.bc_value_of(inst_id);
            ctx.emit(Opcode::AtomicLoad, wbits(t), ord.to_tag() as u16);
            ctx.push(dst);
            ctx.push(addr);
        }
        Inst::AtomicStore { dst, value, t, ord } => {
            let addr = resolve_lowered_value(k1, ctx, dst);
            let val = resolve_lowered_value(k1, ctx, value);
            ctx.emit(Opcode::AtomicStore, wbits(t), ord.to_tag() as u16);
            ctx.push(addr);
            ctx.push(val);
        }
        Inst::AtomicRmw { op, t, dst, operand, ord } => {
            let addr = resolve_lowered_value(k1, ctx, dst);
            let operand = resolve_lowered_value(k1, ctx, operand);
            let bc_value = ctx.bc_value_of(inst_id);
            let b = ((op.to_tag() as u16) << 8) | ord.to_tag() as u16;
            ctx.emit(Opcode::AtomicRmw, wbits(t), b);
            ctx.push(bc_value);
            ctx.push(addr);
            ctx.push(operand);
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = *k1.ir.cmpxchgs.get(id);
            let result = resolve_lowered_value(k1, ctx, cas.result);
            let addr = resolve_lowered_value(k1, ctx, cas.dst);
            let expected = resolve_lowered_value(k1, ctx, cas.expected);
            let desired = resolve_lowered_value(k1, ctx, cas.desired);
            let b = cas.success.to_tag() as u16
                | (cas.failure.to_tag() as u16) << 4
                | (cas.weak as u16) << 8;
            ctx.emit(Opcode::AtomicCmpxchg, wbits(cas.t), b);
            ctx.push(result);
            ctx.push(addr);
            ctx.push(expected);
            ctx.push(desired);
            ctx.push(cas.ok_vm_offset);
        }
        // Vector ops unroll to scalar opcodes per lane; no vector opcodes exist
        Inst::VecOp { id } => {
            use ir::VecOpIr;
            let vop = *k1.ir.vec_ops.get(id);
            let elem_bits = wbits(vop.elem);
            let stride = vop.elem.get_layout().stride() as u16;
            let is_float = matches!(vop.elem, ScalarType::F32 | ScalarType::F64);
            let is_signed = matches!(
                vop.elem,
                ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64
            );
            match vop.op {
                VecOpIr::Splat => {
                    let (addr, base_off) = resolve_addr(k1, ctx, vop.dst);
                    let val = resolve_lowered_value(k1, ctx, vop.lhs);
                    for lane in 0..vop.lanes as u16 {
                        ctx.emit(Opcode::Store, elem_bits, base_off + lane * stride);
                        ctx.push(addr);
                        ctx.push(val);
                    }
                }
                VecOpIr::Add
                | VecOpIr::Sub
                | VecOpIr::Mul
                | VecOpIr::BitAnd
                | VecOpIr::BitOr
                | VecOpIr::Xor
                | VecOpIr::EqLanes => {
                    let (lhs_addr, lhs_off) = resolve_addr(k1, ctx, vop.lhs);
                    let (rhs_addr, rhs_off) = resolve_addr(k1, ctx, vop.rhs);
                    let (dst_addr, dst_off) = resolve_addr(k1, ctx, vop.dst);
                    let s0 = ctx.next_scratch();
                    let s1 = ctx.next_scratch();
                    let zero = k1.bc.intern_const(0);
                    for lane in 0..vop.lanes as u16 {
                        let off = lane * stride;
                        ctx.emit(Opcode::Load, elem_bits, lhs_off + off);
                        ctx.push(s0);
                        ctx.push(lhs_addr);
                        ctx.emit(Opcode::Load, elem_bits, rhs_off + off);
                        ctx.push(s1);
                        ctx.push(rhs_addr);
                        match vop.op {
                            VecOpIr::EqLanes => {
                                if is_float {
                                    ctx.emit(
                                        Opcode::FloatCmp,
                                        elem_bits,
                                        ir::FloatCmpPred::Eq as u16,
                                    );
                                } else {
                                    ctx.emit(Opcode::IntCmp, elem_bits, ir::IntCmpPred::Eq as u16);
                                }
                                ctx.push(s0);
                                ctx.push(s0);
                                ctx.push(s1);
                                // Fan the 0/1 out to a 0/all-ones lane: s0 = 0 - s0
                                ctx.emit(Opcode::IntSub, elem_bits, 0);
                                ctx.push(s0);
                                ctx.push(zero);
                                ctx.push(s0);
                            }
                            _ => {
                                let opcode = match (vop.op, is_float) {
                                    (VecOpIr::Add, false) => Opcode::IntAdd,
                                    (VecOpIr::Add, true) => Opcode::FloatAdd,
                                    (VecOpIr::Sub, false) => Opcode::IntSub,
                                    (VecOpIr::Sub, true) => Opcode::FloatSub,
                                    (VecOpIr::Mul, false) => Opcode::IntMul,
                                    (VecOpIr::Mul, true) => Opcode::FloatMul,
                                    (VecOpIr::BitAnd, _) => Opcode::BitAnd,
                                    (VecOpIr::BitOr, _) => Opcode::BitOr,
                                    (VecOpIr::Xor, _) => Opcode::BitXor,
                                    _ => unreachable!(),
                                };
                                ctx.emit(opcode, elem_bits, 0);
                                ctx.push(s0);
                                ctx.push(s0);
                                ctx.push(s1);
                            }
                        }
                        ctx.emit(Opcode::Store, elem_bits, dst_off + off);
                        ctx.push(dst_addr);
                        ctx.push(s0);
                    }
                }
                VecOpIr::BitNot => {
                    let (lhs_addr, lhs_off) = resolve_addr(k1, ctx, vop.lhs);
                    let (dst_addr, dst_off) = resolve_addr(k1, ctx, vop.dst);
                    let s0 = ctx.next_scratch();
                    for lane in 0..vop.lanes as u16 {
                        let off = lane * stride;
                        ctx.emit(Opcode::Load, elem_bits, lhs_off + off);
                        ctx.push(s0);
                        ctx.push(lhs_addr);
                        ctx.emit(Opcode::BitNot, 0, 0);
                        ctx.push(s0);
                        ctx.push(s0);
                        ctx.emit(Opcode::Store, elem_bits, dst_off + off);
                        ctx.push(dst_addr);
                        ctx.push(s0);
                    }
                }
                VecOpIr::Shl | VecOpIr::Shr => {
                    let (lhs_addr, lhs_off) = resolve_addr(k1, ctx, vop.lhs);
                    let (dst_addr, dst_off) = resolve_addr(k1, ctx, vop.dst);
                    let count = resolve_lowered_value(k1, ctx, vop.rhs);
                    let s0 = ctx.next_scratch();
                    let opcode = match vop.op {
                        VecOpIr::Shl => Opcode::Shl,
                        _ if is_signed => Opcode::ShrS,
                        _ => Opcode::ShrU,
                    };
                    for lane in 0..vop.lanes as u16 {
                        let off = lane * stride;
                        ctx.emit(Opcode::Load, elem_bits, lhs_off + off);
                        ctx.push(s0);
                        ctx.push(lhs_addr);
                        ctx.emit(opcode, elem_bits, 0);
                        ctx.push(s0);
                        ctx.push(s0);
                        ctx.push(count);
                        ctx.emit(Opcode::Store, elem_bits, dst_off + off);
                        ctx.push(dst_addr);
                        ctx.push(s0);
                    }
                }
                VecOpIr::ToMask => {
                    // acc |= lane_msb << lane, for each lane
                    let (lhs_addr, lhs_off) = resolve_addr(k1, ctx, vop.lhs);
                    let acc = ctx.bc_value_of(inst_id);
                    let s0 = ctx.next_scratch();
                    let zero = k1.bc.intern_const(0);
                    let msb_shift = k1.bc.intern_const((elem_bits - 1) as u64);
                    ctx.emit(Opcode::Mov, 0, 0);
                    ctx.push(acc);
                    ctx.push(zero);
                    for lane in 0..vop.lanes as u16 {
                        ctx.emit(Opcode::Load, elem_bits, lhs_off + lane * stride);
                        ctx.push(s0);
                        ctx.push(lhs_addr);
                        ctx.emit(Opcode::ShrU, elem_bits, 0);
                        ctx.push(s0);
                        ctx.push(s0);
                        ctx.push(msb_shift);
                        let lane_shift = k1.bc.intern_const(lane as u64);
                        ctx.emit(Opcode::Shl, 64, 0);
                        ctx.push(s0);
                        ctx.push(s0);
                        ctx.push(lane_shift);
                        ctx.emit(Opcode::BitOr, 64, 0);
                        ctx.push(acc);
                        ctx.push(acc);
                        ctx.push(s0);
                    }
                }
            }
        }
        Inst::Fence { ord } => {
            ctx.emit(Opcode::Fence, 0, ord.to_tag() as u16);
        }
        Inst::Copy { dst, src, vm_size, .. } => {
            let dst_addr = resolve_lowered_value(k1, ctx, dst);
            let src_addr = resolve_lowered_value(k1, ctx, src);
            ctx.emit(Opcode::Copy, 0, 0);
            ctx.push(dst_addr);
            ctx.push(src_addr);
            ctx.push(vm_size);
        }
        Inst::StructOffset { base, vm_offset, .. } => {
            if !ctx.folded_struct_offsets.contains_key(&inst_id) {
                let base_lowered = resolve_lowered_value(k1, ctx, base);
                let dst = ctx.bc_value_of(inst_id);
                ctx.emit(Opcode::PtrAddImm, 0, 0);
                ctx.push(dst);
                ctx.push(base_lowered);
                ctx.push(vm_offset);
            }
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            let base_lowered = resolve_lowered_value(k1, ctx, base);
            let index_lowered = resolve_lowered_value(k1, ctx, element_index);
            let stride = k1.get_pt_layout(element_t).stride();
            let dst = ctx.bc_value_of(inst_id);
            ctx.emit(Opcode::PtrIndex, 0, 0);
            ctx.push(dst);
            ctx.push(base_lowered);
            ctx.push(index_lowered);
            ctx.push(stride);
        }
        Inst::Call { call_id } => {
            emit_call(k1, ctx, inst_id, call_id)?;
        }
        Inst::Jump(target) => {
            emit_phi_copies(k1, ctx, block_id, target);
            // Fall through when the target is emitted next; only one pred of
            // a join can be laid out before it, every other pred still jumps
            if target != next_block {
                ctx.emit(Opcode::Jump, 0, 0);
                ctx.push_block_target(target);
            }
        }
        Inst::JumpIf { cond, cons, alt } => {
            match fused_cmp_of(ctx, cond) {
                Some(cmp_id) => {
                    let Inst::IntCmp { lhs, rhs, pred, width } = *k1.ir.instrs.get(cmp_id) else {
                        unreachable!("fused cmp is not an IntCmp")
                    };
                    let lhs_lowered = resolve_lowered_value(k1, ctx, lhs);
                    let rhs_lowered = resolve_lowered_value(k1, ctx, rhs);
                    ctx.emit(Opcode::JumpIfIntCmp, width, pred as u16);
                    ctx.push(lhs_lowered);
                    ctx.push(rhs_lowered);
                }
                None => {
                    let cond_lowered = resolve_lowered_value(k1, ctx, cond);
                    ctx.emit(Opcode::JumpIf, 0, 0);
                    ctx.push(cond_lowered);
                }
            }
            for target in [cons, alt] {
                if ctx.block_has_phis(target) {
                    let at = ctx.bc_out.len() as u32;
                    ctx.push(0);
                    ctx.pc_operands_to_rebase.push(at);
                    ctx.trampolines.push(PendingTramp { operand: at, from: block_id, target });
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
                let size = k1.get_pt_layout(ret_pt).size;
                let src = resolve_lowered_value(k1, ctx, v);
                ctx.emit(Opcode::RetAgg, 0, 0);
                ctx.push(src);
                ctx.push(size);
            } else {
                // Empty returns arrive as `Ret { v: Empty }` and resolve to
                // const 0; nobody reads ret_reg for them
                let src = resolve_lowered_value(k1, ctx, v);
                ctx.emit(Opcode::Ret, 0, 0);
                ctx.push(src);
            }
        }

        Inst::BoolNegate { v } => unop!(Opcode::BoolNegate, 0, 0, v),
        Inst::BitNot { v } => unop!(Opcode::BitNot, 0, 0, v),
        Inst::FloatNeg { v, width } => unop!(Opcode::FloatNeg, width, 0, v),

        Inst::IntTrunc { v, to } => cast!(CastKind::IntTrunc, 0, wbits(to), v),
        Inst::IntExtS { v, from, to } => cast!(CastKind::IntExtS, wbits(from), wbits(to), v),
        Inst::FloatTrunc { v, .. } => cast!(CastKind::FloatTrunc, 0, 0, v),
        Inst::FloatExt { v, .. } => cast!(CastKind::FloatExt, 0, 0, v),
        Inst::Float32ToIntUnsigned { v, to } => cast!(CastKind::F32ToUInt, 0, wbits(to), v),
        Inst::Float32ToIntSigned { v, to } => cast!(CastKind::F32ToSInt, 0, wbits(to), v),
        Inst::Float64ToIntUnsigned { v, to } => cast!(CastKind::F64ToUInt, 0, wbits(to), v),
        Inst::Float64ToIntSigned { v, to } => cast!(CastKind::F64ToSInt, 0, wbits(to), v),
        Inst::IntToFloatUnsigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::UIntToF32 } else { CastKind::UIntToF64 };
            cast!(kind, wbits(from), wbits(to), v)
        }
        Inst::IntToFloatSigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::SIntToF32 } else { CastKind::SIntToF64 };
            cast!(kind, wbits(from), wbits(to), v)
        }

        Inst::IntAdd { lhs, rhs, width } => binop!(Opcode::IntAdd, width, 0, lhs, rhs),
        Inst::IntSub { lhs, rhs, width } => binop!(Opcode::IntSub, width, 0, lhs, rhs),
        Inst::IntMul { lhs, rhs, width } => binop!(Opcode::IntMul, width, 0, lhs, rhs),
        Inst::IntDivUnsigned { lhs, rhs, width } => binop!(Opcode::IntDivU, width, 0, lhs, rhs),
        Inst::IntDivSigned { lhs, rhs, width } => binop!(Opcode::IntDivS, width, 0, lhs, rhs),
        Inst::IntRemUnsigned { lhs, rhs, width } => binop!(Opcode::IntRemU, width, 0, lhs, rhs),
        Inst::IntRemSigned { lhs, rhs, width } => binop!(Opcode::IntRemS, width, 0, lhs, rhs),
        Inst::IntCmp { lhs, rhs, pred, width } => {
            if !ctx.fused_cmps.contains(&inst_id) {
                binop!(Opcode::IntCmp, width, pred as u16, lhs, rhs)
            }
        }
        Inst::FloatAdd { lhs, rhs, width } => binop!(Opcode::FloatAdd, width, 0, lhs, rhs),
        Inst::FloatSub { lhs, rhs, width } => binop!(Opcode::FloatSub, width, 0, lhs, rhs),
        Inst::FloatMul { lhs, rhs, width } => binop!(Opcode::FloatMul, width, 0, lhs, rhs),
        Inst::FloatDiv { lhs, rhs, width } => binop!(Opcode::FloatDiv, width, 0, lhs, rhs),
        Inst::FloatRem { lhs, rhs, width } => binop!(Opcode::FloatRem, width, 0, lhs, rhs),
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            binop!(Opcode::FloatCmp, width, pred as u16, lhs, rhs)
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
            let value_lowered = resolve_lowered_value(k1, ctx, value);
            let dst = ctx.bc_value_of(inst_id);
            ctx.emit(Opcode::BakeStaticValue, 0, 0);
            ctx.push(dst);
            ctx.push(type_id.as_u32());
            ctx.push(value_lowered);
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
    assert!(nargs <= u16::MAX as u32, "call with more than u16::MAX args");
    let frame_bytes = ctx.frame_bytes;

    // The lowered sret: the destination address for agg returns, else const 0
    // (the callee only reads sret when it returns an aggregate)
    let resolve_sret = |k1: &mut TypedProgram, ctx: &mut LowerCtx| -> u32 {
        if !is_agg {
            return k1.bc.intern_const(0);
        }
        ctx.begin_inst();
        match call.dst {
            Some(dst) => resolve_lowered_value(k1, ctx, dst),
            None => ctx.bc_value_of(inst_id), // fp-relative reserved temp
        }
    };

    match call.callee {
        // Direct and indirect calls carry sret + args as operands (nargs in
        // the header B field); the exec arm writes them into the new frame
        IrCallee::Direct(_) | IrCallee::Indirect(_, _) => {
            // Resolve operands up front; they are all live at the Call, so a
            // value in a LoadGlobal scratch slot is parked in the callee slot
            // it is destined for anyway (the Call arm rewrites it in place)
            ctx.call_arg_values.clear();
            for (k, arg) in args.iter().enumerate() {
                ctx.begin_inst();
                let mut arg_lowered = resolve_lowered_value(k1, ctx, *arg);
                if arg_lowered == ctx.scratch0 || arg_lowered == ctx.scratch1 {
                    let park = ctx.out_arg_word(k as u32);
                    ctx.emit(Opcode::Mov, 0, 0);
                    ctx.push(park);
                    ctx.push(arg_lowered);
                    arg_lowered = park;
                }
                ctx.call_arg_values.push(arg_lowered);
            }
            let mut sret_lowered = resolve_sret(k1, ctx);
            if sret_lowered == ctx.scratch0 || sret_lowered == ctx.scratch1 {
                let park = ctx.out_sret_word();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(park);
                ctx.push(sret_lowered);
                sret_lowered = park;
            }

            match call.callee {
                IrCallee::Direct(function_id) => {
                    if k1.bc.in_progress.contains(&function_id) {
                        // Recursion cycle: patch when the callee's code_start lands
                        ctx.emit(Opcode::Call, 0, nargs as u16);
                        let at = ctx.bc_out.len() as u32;
                        ctx.push(PENDING_PC);
                        ctx.operand_to_callee.push((at, function_id));
                        ctx.push(frame_bytes);
                    } else {
                        let info = get_or_lower_function(k1, function_id, ctx.cur_span)?;
                        if info.kind != UnitKind::Body {
                            kbail!(
                                k1,
                                ctx.cur_span,
                                "Direct call to bodyless ({}) function: {}",
                                info.kind,
                                k1.function_id_to_string(function_id, false)
                            );
                        }
                        ctx.emit(Opcode::Call, 0, nargs as u16);
                        ctx.push(info.code_start);
                        ctx.push(frame_bytes);
                    }
                }
                IrCallee::Indirect(_, fn_value) => {
                    // Resolved last: nothing after it can clobber its scratch
                    ctx.begin_inst();
                    let fn_lowered = resolve_lowered_value(k1, ctx, fn_value);
                    ctx.emit(Opcode::CallIndirect, 0, nargs as u16);
                    ctx.push(fn_lowered);
                    ctx.push(frame_bytes);
                }
                _ => unreachable!(),
            }
            ctx.push(sret_lowered);
            for i in 0..ctx.call_arg_values.len() {
                let s = ctx.call_arg_values[i];
                ctx.push(s);
            }
        }
        // Extern/builtin handlers read args from the callee param slots, so
        // those are staged with Movs as before
        IrCallee::Extern { library_name, function_name, function_id } => {
            emit_arg_movs(k1, ctx, args);
            if is_agg {
                let sret_lowered = resolve_sret(k1, ctx);
                let sret = ctx.out_sret_word();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(sret);
                ctx.push(sret_lowered);
            }
            ctx.emit(Opcode::CallExtern, 0, 0);
            ctx.push(function_id.as_u32());
            // StringIds are nonzero, so 0 means "none"
            ctx.push(library_name.map(|s| s.as_u32()).unwrap_or(0));
            ctx.push(function_name.as_u32());
            ctx.push(ret_pt.to_u32());
            ctx.push(frame_bytes);
            ctx.push(nargs);
        }
        IrCallee::BackendBuiltin(_, builtin) => {
            emit_arg_movs(k1, ctx, args);
            if is_agg {
                let sret_lowered = resolve_sret(k1, ctx);
                let sret = ctx.out_sret_word();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(sret);
                ctx.push(sret_lowered);
            }
            ctx.emit(Opcode::CallBuiltin, builtin_tag(builtin), 0);
            ctx.push(ret_pt.to_u32());
            ctx.push(frame_bytes);
            ctx.push(nargs);
        }
        IrCallee::LlvmIntrinsic { name, .. } => {
            emit_arg_movs(k1, ctx, args);
            if is_agg {
                let sret_lowered = resolve_sret(k1, ctx);
                let sret = ctx.out_sret_word();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(sret);
                ctx.push(sret_lowered);
            }
            ctx.emit(Opcode::CallLlvm, 0, 0);
            ctx.push(name.as_u32());
            ctx.push(ret_pt.to_u32());
            ctx.push(frame_bytes);
            ctx.push(nargs);
        }
    }

    // Result delivery for scalar returns (agg results were written through
    // sret by the callee/handler; empty returns produce nothing).
    if !ret_pt.is_empty() && !is_agg {
        match call.dst {
            Some(dst) => {
                ctx.begin_inst();
                let d = resolve_lowered_value(k1, ctx, dst);
                let t = ret_pt.expect_scalar();
                ctx.emit(Opcode::RetStore, wbits(t), 0);
                ctx.push(d);
            }
            None => {
                ctx.emit(Opcode::RetGet, 0, 0);
                ctx.push(ctx.bc_value_of(inst_id));
            }
        }
    }

    Ok(())
}

fn emit_arg_movs(k1: &mut TypedProgram, ctx: &mut LowerCtx, args: &[ir::Value]) {
    for (k, arg) in args.iter().enumerate() {
        ctx.begin_inst();
        let src = resolve_lowered_value(k1, ctx, *arg);
        let dst = ctx.out_arg_word(k as u32);
        ctx.emit(Opcode::Mov, 0, 0);
        ctx.push(dst);
        ctx.push(src);
    }
}

fn emit_phi_copies(k1: &mut TypedProgram, ctx: &mut LowerCtx, from: BlockId, target: BlockId) {
    let mut i = 0;
    while i < ctx.phis.len() {
        let (phi_block, phi_id) = ctx.phis[i];
        i += 1;
        if phi_block != target {
            continue;
        }
        let Inst::Phi { incomings, .. } = *k1.ir.instrs.get(phi_id) else {
            unreachable!("non-phi in block_phis")
        };
        let Some(dst) = ctx.inst_to_frame_value.get(&phi_id).copied() else {
            continue; // empty-typed phi
        };
        let case = k1.ir.mem.getn(incomings).iter().find(|c| c.from == from).copied();
        ctx.begin_inst();
        let src = match case {
            Some(case) => resolve_lowered_value(k1, ctx, case.value),
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
