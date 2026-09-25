// Copyright (c) 2026 knix
// All rights reserved.

use crate::debug;

use crate::ir::{
    self, BlockId, IdMap, Inst, InstId, InstKind, IrCallee, IrUnit, IrUnitId, UnitView,
};
use crate::kbail;
use crate::lex::SpanId;
use crate::typer::trace::TraceKind;
use crate::typer::types::{Layout, PhysicalType, ScalarType};
use crate::typer::{FunctionId, K1Result, TypedExprId, TypedProgram};
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
        !k1.trace.stack_contains_key(TraceKind::Bcgen, function_id.as_u32()),
        "get_or_lower_function called on in-progress function; caller must check"
    );
    if k1.ir.function_unit(function_id).is_none() {
        let requester = k1.trace.top();
        k1.compile_function_for_exec(function_id, requester, span)?;
    }
    let unit = *k1.ir.function_unit(function_id).unwrap();
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
    ir::optimize_unit(k1, IrUnitId::Function(function_id))?;
    let unit = *k1.ir.function_unit(function_id).unwrap();
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
    operand: u32,
    from: BlockId,
    target: BlockId,
}

struct PhiMove {
    dst: u32,
    src: ir::Value,
    src_word: Option<u32>,
}

pub(crate) struct LowerCtx {
    u: UnitView<'static>,
    ret_pt: PhysicalType,
    frame_bytes: u32,
    scratch0: u32,
    scratch1: u32,
    scratch_flip: bool,

    inst_to_frame_value: IdMap<InstId, u32>,
    inst_to_parent_value: IdMap<InstId, ir::Value>,
    folded_struct_offsets: IdMap<InstId, (ir::Value, u16)>,
    use_counts: IdMap<InstId, u32>,
    addr_use_counts: IdMap<InstId, u32>,
    fused_cmps: IdMap<InstId, ()>,
    call_arg_values: Vec<u32>,
    phis: Vec<(BlockId, InstId)>,
    phi_moves: Vec<PhiMove>,
    allocas: Vec<(InstId, Layout)>,
    agg_call_temps: Vec<(InstId, Layout)>,

    bc_out: Vec<u32>,
    block_to_entry_pc: IdMap<BlockId, u32>,
    pc_operands_to_rebase: Vec<u32>,
    operand_to_block: Vec<(u32, BlockId)>,
    operand_to_callee: Vec<(u32, FunctionId)>,
    trampolines: Vec<PendingTramp>,
    spans: Vec<(u32, SpanId)>,
    cur_span: SpanId,
}

impl LowerCtx {
    pub(crate) fn make() -> LowerCtx {
        LowerCtx {
            u: UnitView::EMPTY,
            ret_pt: PhysicalType::EMPTY,
            frame_bytes: 0,
            scratch0: 0,
            scratch1: 0,
            scratch_flip: false,
            inst_to_frame_value: IdMap::default(),
            inst_to_parent_value: IdMap::default(),
            folded_struct_offsets: IdMap::default(),
            use_counts: IdMap::default(),
            addr_use_counts: IdMap::default(),
            fused_cmps: IdMap::default(),
            call_arg_values: Vec::new(),
            phis: Vec::new(),
            phi_moves: Vec::new(),
            allocas: Vec::new(),
            agg_call_temps: Vec::new(),
            bc_out: Vec::with_capacity(1024),
            block_to_entry_pc: IdMap::default(),
            pc_operands_to_rebase: Vec::new(),
            operand_to_block: Vec::new(),
            operand_to_callee: Vec::new(),
            trampolines: Vec::new(),
            spans: Vec::new(),
            cur_span: SpanId::NONE,
        }
    }

    fn reset(&mut self, u: UnitView<'static>, ret_pt: PhysicalType) {
        self.u = u;
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
        self.phi_moves.clear();
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

    fn push_edge_target(&mut self, from: BlockId, target: BlockId) {
        if self.block_has_phis(target) {
            let at = self.bc_out.len() as u32;
            self.push(0);
            self.pc_operands_to_rebase.push(at);
            self.trampolines.push(PendingTramp { operand: at, from, target });
        } else {
            self.push_block_target(target);
        }
    }

    fn begin_inst(&mut self) {
        self.scratch_flip = false;
    }

    fn next_scratch(&mut self) -> u32 {
        let s = if self.scratch_flip { self.scratch1 } else { self.scratch0 };
        self.scratch_flip = !self.scratch_flip;
        s
    }

    fn walk_to_frame_value(&self, mut value: ir::Value) -> ir::Value {
        while let ir::Value::Inst(id) = value {
            match self.inst_to_parent_value.get(id) {
                Some(f) => value = f,
                None => break,
            }
        }
        value
    }

    fn bc_value_of(&self, inst_id: InstId) -> u32 {
        self.inst_to_frame_value
            .get(inst_id)
            .unwrap_or_else(|| panic!("bc lowering: no bc value for inst i{}", inst_id.as_u32()))
    }

    fn block_has_phis(&self, b: BlockId) -> bool {
        self.phis.iter().any(|(pb, _)| *pb == b)
    }

    fn out_arg_word(&self, k: u32) -> u32 {
        self.frame_bytes / 8 + FRAME_HEADER_WORDS + k
    }

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

fn fused_cmp_of(ctx: &LowerCtx, cond: ir::Value) -> Option<InstId> {
    let ir::Value::Inst(id) = cond else { return None };
    if ctx.fused_cmps.contains(id) { Some(id) } else { None }
}

fn resolve_lowered_value(k1: &mut TypedProgram, ctx: &mut LowerCtx, value: ir::Value) -> u32 {
    match ctx.walk_to_frame_value(value) {
        ir::Value::Inst(inst_id) => {
            if let Some(bc_value) = ctx.inst_to_frame_value.get(inst_id) {
                return bc_value;
            }
            match *ctx.u.inst(inst_id) {
                Inst::Data(imm) => k1.bc.intern_const(imm.bits()),
                _ => k1.bc.intern_const(0),
            }
        }
        ir::Value::FnParam { index, .. } => FRAME_HEADER_WORDS + index,
        ir::Value::Data32 { t, data } => k1.bc.intern_const(ir::data32_bits(t, data)),
        ir::Value::IsStatic => k1.bc.intern_const(1),
        ir::Value::StaticValue { id, .. } => {
            let v = vm::static_value_to_vm_value(k1, id, ctx.cur_span);
            k1.bc.intern_const(v.bits())
        }
        ir::Value::FunctionAddr(function_id) => {
            if !k1.bc.functions.contains_key(&function_id)
                && !k1.trace.stack_contains_key(TraceKind::Bcgen, function_id.as_u32())
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

fn resolve_addr(k1: &mut TypedProgram, ctx: &mut LowerCtx, value: ir::Value) -> (u32, u16) {
    if let ir::Value::Inst(id) = value {
        if let Some((base, off)) = ctx.folded_struct_offsets.get(id) {
            return (resolve_lowered_value(k1, ctx, base), off);
        }
    }
    (resolve_lowered_value(k1, ctx, value), 0)
}

fn lower_unit(k1: &mut TypedProgram, unit: IrUnit) -> K1Result<UnitInfo> {
    let mut ctx = k1.bc.lower_ctx_pool.pop().unwrap_or_else(LowerCtx::make);
    let frame = k1.trace_push_unit(TraceKind::Bcgen, unit.unit_id, None);
    let result = lower_unit_with_ctx(k1, unit, &mut ctx);
    k1.trace_pop(frame);
    k1.bc.lower_ctx_pool.push(ctx);
    result
}

fn lower_unit_with_ctx(
    k1: &mut TypedProgram,
    unit: IrUnit,
    ctx: &mut LowerCtx,
) -> K1Result<UnitInfo> {
    let unit_id = unit.unit_id;

    let u = unit.view(&k1.ir.mem);
    let param_count = unit.fn_type.params.len();
    let ret_pt = unit.fn_type.return_type;
    ctx.reset(u, ret_pt);

    ir::count_uses(&u, &mut ctx.use_counts);
    let mut next_word: u32 = FRAME_HEADER_WORDS + param_count;
    let mut call_arg_words: usize = 0;
    let mut block_cmps: Vec<InstId> = Vec::new();

    for block_h in u.block_ids() {
        block_cmps.clear();
        for inst_id in u.block_insts(block_h) {
            let inst = *u.inst(inst_id);
            match inst {
                Inst::Data(_) => {}
                Inst::BitCast { v, .. }
                | Inst::IntExtU { v, .. }
                | Inst::PtrToWord { v, .. }
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
                    let call = *u.call(call_id);
                    call_arg_words += call.args.len() as usize * 4;
                    match call.dst {
                        Some(dst) => {
                            debug_assert!(
                                !call.ret_type.is_empty(),
                                "call with dst but empty return type"
                            );
                            ctx.inst_to_parent_value.insert(inst_id, dst);
                        }
                        None if call.ret_type.is_agg() => {
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
                        if ctx.use_counts.get(id) == Some(1) && block_cmps.contains(&id) {
                            ctx.fused_cmps.insert(id, ());
                        }
                    }
                }
                _ => {
                    if let InstKind::Value(pt) = ir::get_inst_kind(&u, inst_id) {
                        if !pt.is_empty() {
                            ctx.inst_to_frame_value.insert(inst_id, next_word);
                            next_word += 1;
                        }
                    }
                }
            }
            let addr_use = match inst {
                Inst::Load { src, .. } => Some(src),
                Inst::Store { dst, .. } => Some(dst),
                _ => None,
            };
            if let Some(ir::Value::Inst(id)) = addr_use {
                let count = ctx.addr_use_counts.get(id).unwrap_or(0);
                ctx.addr_use_counts.insert(id, count + 1);
            }
        }
    }

    let LowerCtx { folded_struct_offsets, addr_use_counts, use_counts, .. } = &mut *ctx;
    folded_struct_offsets.retain(|id, _| addr_use_counts.get(id) == use_counts.get(id));

    ctx.scratch0 = next_word;
    ctx.scratch1 = next_word + 1;
    next_word += 2;

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

    ctx.bc_out.reserve(u.inst_count() * 4 + call_arg_words);
    let mut first = true;
    for block_h in u.block_ids() {
        ctx.block_to_entry_pc.insert(block_h, ctx.pc());
        if first {
            ctx.emit(Opcode::Enter, 0, 0);
            let fb = ctx.frame_bytes;
            ctx.push(fb);
            first = false;
        }
        let next_block = u.block(block_h).next;
        for inst_id in u.block_insts(block_h) {
            emit_inst(k1, ctx, block_h, next_block, inst_id)?;
        }
    }

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

    for i in 0..ctx.operand_to_block.len() {
        let (at, block_id) = ctx.operand_to_block[i];
        let Some(target_pc) = ctx.block_to_entry_pc.get(block_id) else {
            panic!("bc lowering: jump to unemitted block b{}", block_id);
        };
        ctx.bc_out[at as usize] = target_pc;
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
    next_block: Option<BlockId>,
    inst_id: InstId,
) -> K1Result<()> {
    let inst = *ctx.u.inst(inst_id);
    let span = ctx.u.span(inst_id);
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
        Inst::Data(_) => {}
        Inst::ReloadGlobalAddr { storage_pt, id } => {
            ctx.emit(Opcode::LoadGlobal, 0, 0);
            ctx.push(ctx.bc_value_of(inst_id));
            ctx.push(id.as_u32());
            ctx.push(storage_pt.to_u32());
        }
        Inst::BitCast { .. }
        | Inst::IntExtU { .. }
        | Inst::PtrToWord { .. }
        | Inst::WordToPtr { .. } => {}
        Inst::Phi { .. } => {}
        Inst::Alloca { .. } => {}
        Inst::Store { dst, value, t, volatile: _, unaligned: _ } => {
            let (addr, off) = resolve_addr(k1, ctx, dst);
            let value = resolve_lowered_value(k1, ctx, value);
            ctx.emit(Opcode::Store, t.width_bits(), off);
            ctx.push(addr);
            ctx.push(value);
        }
        Inst::Load { t, src, volatile: _, unaligned: _ } => {
            let (addr, off) = resolve_addr(k1, ctx, src);
            let result = ctx.bc_value_of(inst_id);
            ctx.emit(Opcode::Load, t.width_bits(), off);
            ctx.push(result);
            ctx.push(addr);
        }
        Inst::AtomicLoad { t, src, ord } => {
            let addr = resolve_lowered_value(k1, ctx, src);
            let dst = ctx.bc_value_of(inst_id);
            ctx.emit(Opcode::AtomicLoad, t.width_bits(), ord.to_tag() as u16);
            ctx.push(dst);
            ctx.push(addr);
        }
        Inst::AtomicStore { dst, value, t, ord } => {
            let addr = resolve_lowered_value(k1, ctx, dst);
            let val = resolve_lowered_value(k1, ctx, value);
            ctx.emit(Opcode::AtomicStore, t.width_bits(), ord.to_tag() as u16);
            ctx.push(addr);
            ctx.push(val);
        }
        Inst::AtomicRmw { op, t, dst, operand, ord } => {
            let addr = resolve_lowered_value(k1, ctx, dst);
            let operand = resolve_lowered_value(k1, ctx, operand);
            let bc_value = ctx.bc_value_of(inst_id);
            let b = ((op.to_tag() as u16) << 8) | ord.to_tag() as u16;
            ctx.emit(Opcode::AtomicRmw, t.width_bits(), b);
            ctx.push(bc_value);
            ctx.push(addr);
            ctx.push(operand);
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = *ctx.u.cmpxchg(id);
            let result = resolve_lowered_value(k1, ctx, cas.result);
            let addr = resolve_lowered_value(k1, ctx, cas.dst);
            let expected = resolve_lowered_value(k1, ctx, cas.expected);
            let desired = resolve_lowered_value(k1, ctx, cas.desired);
            let b = cas.success.to_tag() as u16
                | (cas.failure.to_tag() as u16) << 4
                | (cas.weak as u16) << 8;
            ctx.emit(Opcode::AtomicCmpxchg, cas.t.width_bits(), b);
            ctx.push(result);
            ctx.push(addr);
            ctx.push(expected);
            ctx.push(desired);
            ctx.push(cas.ok_vm_offset);
        }
        Inst::VecOp { id } => {
            use ir::VecOpIr;
            let vop = *ctx.u.vec_op(id);
            let elem_bits = vop.elem.width_bits();
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
                        ctx.emit(Opcode::BitNot, elem_bits, 0);
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
            if !ctx.folded_struct_offsets.contains(inst_id) {
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
            if Some(target) != next_block {
                ctx.emit(Opcode::Jump, 0, 0);
                ctx.push_block_target(target);
            }
        }
        Inst::JumpIf { cond, cons, alt } => {
            match fused_cmp_of(ctx, cond) {
                Some(cmp_id) => {
                    let Inst::IntCmp { lhs, rhs, pred, width } = *ctx.u.inst(cmp_id) else {
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
            ctx.push_edge_target(block_id, cons);
            ctx.push_edge_target(block_id, alt);
        }
        Inst::Switch { value, width, cases, default } => {
            let src = resolve_lowered_value(k1, ctx, value);
            let mut sorted = ctx.u.switch_cases(cases).to_vec();
            sorted.sort_by_key(|case| case.value);
            assert!(sorted.len() <= u16::MAX as usize, "switch case count exceeds the bc header");
            ctx.emit(Opcode::Switch, width, sorted.len() as u16);
            ctx.push(src);
            ctx.push_edge_target(block_id, default);
            for case in &sorted {
                ctx.push(case.value as u32);
                ctx.push((case.value >> 32) as u32);
                ctx.push_edge_target(block_id, case.target);
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
                let src = resolve_lowered_value(k1, ctx, v);
                ctx.emit(Opcode::Ret, 0, 0);
                ctx.push(src);
            }
        }

        Inst::BoolNegate { v } => unop!(Opcode::BoolNegate, 0, 0, v),
        Inst::BitNot { v, t } => unop!(Opcode::BitNot, t.width_bits(), 0, v),
        Inst::FloatNeg { v, t } => unop!(Opcode::FloatNeg, t.width_bits(), 0, v),

        Inst::IntTrunc { v, to } => cast!(CastKind::IntTrunc, 0, to.width_bits(), v),
        Inst::IntExtS { v, from, to } => {
            cast!(CastKind::IntExtS, from.width_bits(), to.width_bits(), v)
        }
        Inst::FloatTrunc { v, .. } => cast!(CastKind::FloatTrunc, 0, 0, v),
        Inst::FloatExt { v, .. } => cast!(CastKind::FloatExt, 0, 0, v),
        Inst::Float32ToIntUnsigned { v, to } => cast!(CastKind::F32ToUInt, 0, to.width_bits(), v),
        Inst::Float32ToIntSigned { v, to } => cast!(CastKind::F32ToSInt, 0, to.width_bits(), v),
        Inst::Float64ToIntUnsigned { v, to } => cast!(CastKind::F64ToUInt, 0, to.width_bits(), v),
        Inst::Float64ToIntSigned { v, to } => cast!(CastKind::F64ToSInt, 0, to.width_bits(), v),
        Inst::IntToFloatUnsigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::UIntToF32 } else { CastKind::UIntToF64 };
            cast!(kind, from.width_bits(), to.width_bits(), v)
        }
        Inst::IntToFloatSigned { v, from, to } => {
            let kind =
                if to == ScalarType::F32 { CastKind::SIntToF32 } else { CastKind::SIntToF64 };
            cast!(kind, from.width_bits(), to.width_bits(), v)
        }

        Inst::IntAdd { lhs, rhs, t } => binop!(Opcode::IntAdd, t.width_bits(), 0, lhs, rhs),
        Inst::IntSub { lhs, rhs, t } => binop!(Opcode::IntSub, t.width_bits(), 0, lhs, rhs),
        Inst::IntMul { lhs, rhs, t } => binop!(Opcode::IntMul, t.width_bits(), 0, lhs, rhs),
        Inst::IntDivUnsigned { lhs, rhs, t } => binop!(Opcode::IntDivU, t.width_bits(), 0, lhs, rhs),
        Inst::IntDivSigned { lhs, rhs, t } => binop!(Opcode::IntDivS, t.width_bits(), 0, lhs, rhs),
        Inst::IntRemUnsigned { lhs, rhs, t } => binop!(Opcode::IntRemU, t.width_bits(), 0, lhs, rhs),
        Inst::IntRemSigned { lhs, rhs, t } => binop!(Opcode::IntRemS, t.width_bits(), 0, lhs, rhs),
        Inst::IntCmp { lhs, rhs, pred, width } => {
            if !ctx.fused_cmps.contains(inst_id) {
                binop!(Opcode::IntCmp, width, pred as u16, lhs, rhs)
            }
        }
        Inst::FloatAdd { lhs, rhs, t } => binop!(Opcode::FloatAdd, t.width_bits(), 0, lhs, rhs),
        Inst::FloatSub { lhs, rhs, t } => binop!(Opcode::FloatSub, t.width_bits(), 0, lhs, rhs),
        Inst::FloatMul { lhs, rhs, t } => binop!(Opcode::FloatMul, t.width_bits(), 0, lhs, rhs),
        Inst::FloatDiv { lhs, rhs, t } => binop!(Opcode::FloatDiv, t.width_bits(), 0, lhs, rhs),
        Inst::FloatRem { lhs, rhs, t } => binop!(Opcode::FloatRem, t.width_bits(), 0, lhs, rhs),
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            binop!(Opcode::FloatCmp, width, pred as u16, lhs, rhs)
        }
        Inst::BitAnd { lhs, rhs, t } => binop!(Opcode::BitAnd, t.width_bits(), 0, lhs, rhs),
        Inst::BitOr { lhs, rhs, t } => binop!(Opcode::BitOr, t.width_bits(), 0, lhs, rhs),
        Inst::BitXor { lhs, rhs, t } => binop!(Opcode::BitXor, t.width_bits(), 0, lhs, rhs),
        Inst::BitShiftLeft { lhs, rhs, t } => binop!(Opcode::Shl, t.width_bits(), 0, lhs, rhs),
        Inst::BitUnsignedShiftRight { lhs, rhs, t } => {
            binop!(Opcode::ShrU, t.width_bits(), 0, lhs, rhs)
        }
        Inst::BitSignedShiftRight { lhs, rhs, t } => binop!(Opcode::ShrS, t.width_bits(), 0, lhs, rhs),

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
    let call = *ctx.u.call(call_id);
    let ret_pt = call.ret_type;
    let is_agg = ret_pt.is_agg();
    let args: &[ir::Value] = ctx.u.args(call.args);
    let nargs = args.len() as u32;
    assert!(nargs <= u16::MAX as u32, "call with more than u16::MAX args");
    let frame_bytes = ctx.frame_bytes;

    let resolve_sret = |k1: &mut TypedProgram, ctx: &mut LowerCtx| -> u32 {
        if !is_agg {
            return k1.bc.intern_const(0);
        }
        ctx.begin_inst();
        match call.dst {
            Some(dst) => resolve_lowered_value(k1, ctx, dst),
            None => ctx.bc_value_of(inst_id),
        }
    };

    match call.callee {
        IrCallee::Direct(_) | IrCallee::Indirect(_, _) => {
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
                    if k1.trace.stack_contains_key(TraceKind::Bcgen, function_id.as_u32()) {
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
        IrCallee::Extern { library_name, function_name, function_id } => {
            get_or_lower_function(k1, function_id, ctx.cur_span)?;
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

    if !ret_pt.is_empty() && !is_agg {
        match call.dst {
            Some(dst) => {
                ctx.begin_inst();
                let d = resolve_lowered_value(k1, ctx, dst);
                let t = ret_pt.expect_scalar();
                ctx.emit(Opcode::RetStore, t.width_bits(), 0);
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
    ctx.phi_moves.clear();
    let mut i = 0;
    while i < ctx.phis.len() {
        let (phi_block, phi_id) = ctx.phis[i];
        i += 1;
        if phi_block != target {
            continue;
        }
        let Inst::Phi { incomings, .. } = *ctx.u.inst(phi_id) else {
            unreachable!("non-phi in block_phis")
        };
        let Some(dst) = ctx.inst_to_frame_value.get(phi_id) else {
            continue;
        };
        let case = ctx.u.phi_cases(incomings).iter().find(|c| c.from == from);
        let Some(case) = case else {
            unreachable!("phi i{phi_id} in b{target} has no incoming for the edge from b{from}")
        };
        let src = case.value;
        let src_word = match ctx.walk_to_frame_value(src) {
            ir::Value::GlobalAddr { .. } => None,
            _ => Some(resolve_lowered_value(k1, ctx, src)),
        };
        if src_word == Some(dst) {
            continue;
        }
        ctx.phi_moves.push(PhiMove { dst, src, src_word });
    }

    while !ctx.phi_moves.is_empty() {
        let mut ready = None;
        for i in 0..ctx.phi_moves.len() {
            let dst = ctx.phi_moves[i].dst;
            if !ctx.phi_moves.iter().any(|m| m.src_word == Some(dst)) {
                ready = Some(i);
                break;
            }
        }
        ctx.begin_inst();
        match ready {
            Some(i) => {
                let PhiMove { dst, src, src_word } = ctx.phi_moves.swap_remove(i);
                let src = match src_word {
                    Some(word) => word,
                    None => resolve_lowered_value(k1, ctx, src),
                };
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(dst);
                ctx.push(src);
            }
            None => {
                debug_assert!(ctx.phi_moves.iter().all(|m| m.src_word.is_some()));
                let blocked = ctx.phi_moves[0].dst;
                let scratch = ctx.next_scratch();
                ctx.emit(Opcode::Mov, 0, 0);
                ctx.push(scratch);
                ctx.push(blocked);
                for m in ctx.phi_moves.iter_mut() {
                    if m.src_word == Some(blocked) {
                        m.src_word = Some(scratch);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::test_support::{compile_source, function_named};
    use crate::ir::{
        BlockSourceKind, IntCmpPred, IrComment, IrRange, PhiCase, UnitBuf, commit_unit, data32_bits,
    };
    use crate::typer::types::ScalarType;

    fn u64_value(n: u32) -> ir::Value {
        ir::Value::Data32 { t: ScalarType::U64, data: n }
    }

    fn push(u: &mut UnitBuf, block: BlockId, inst: Inst) -> InstId {
        let id = u.new_inst(inst, SpanId::NONE, IrComment::None);
        u.push_inst(block, id);
        id
    }

    fn run_phi_rotation(
        name: &str,
        inits: &[u32],
        sources: &[usize],
        trips: u32,
        body_block: bool,
    ) -> u64 {
        let mut k1 = compile_source(name, "fn f(): u64 { 0 }\nfn main(): i32 { 0 }\n");
        let f = function_named(&k1, "f");
        k1.compile_function_for_exec(f, None, SpanId::NONE).unwrap();
        let mut unit = *k1.ir.function_unit(f).unwrap();
        let mut u = k1.ir.take_unit_buf();
        let t = PhysicalType::scalar(ScalarType::U64);
        let entry = u.add_block(BlockSourceKind::Entry);
        let header = u.add_block(BlockSourceKind::WhileLoopCondition);
        let body = if body_block { u.add_block(BlockSourceKind::WhileLoopBody) } else { header };
        let exit = u.add_block(BlockSourceKind::WhileLoopEnd);
        push(&mut u, entry, Inst::Jump(header));
        let mut phis = Vec::new();
        for _ in inits {
            phis.push(push(&mut u, header, Inst::Phi { t, incomings: IrRange::EMPTY }));
        }
        let counter = push(&mut u, header, Inst::Phi { t, incomings: IrRange::EMPTY });
        for (k, phi) in phis.iter().enumerate() {
            let incomings = u.push_phi_cases(&[
                PhiCase { from: entry, value: u64_value(inits[k]) },
                PhiCase { from: body, value: ir::Value::Inst(phis[sources[k]]) },
            ]);
            *u.inst_mut(*phi) = Inst::Phi { t, incomings };
        }
        let next = push(
            &mut u,
            body,
            Inst::IntAdd { lhs: ir::Value::Inst(counter), rhs: u64_value(1), t: ScalarType::U64 },
        );
        let incomings = u.push_phi_cases(&[
            PhiCase { from: entry, value: u64_value(0) },
            PhiCase { from: body, value: ir::Value::Inst(next) },
        ]);
        *u.inst_mut(counter) = Inst::Phi { t, incomings };
        let again = push(
            &mut u,
            body,
            Inst::IntCmp {
                lhs: ir::Value::Inst(counter),
                rhs: u64_value(trips),
                pred: IntCmpPred::Ult,
                width: 64,
            },
        );
        push(&mut u, body, Inst::JumpIf { cond: ir::Value::Inst(again), cons: header, alt: exit });
        let mut acc = u64_value(0);
        for phi in &phis {
            let scaled = push(
                &mut u,
                exit,
                Inst::IntMul { lhs: acc, rhs: u64_value(100), t: ScalarType::U64 },
            );
            let summed = push(
                &mut u,
                exit,
                Inst::IntAdd {
                    lhs: ir::Value::Inst(scaled),
                    rhs: ir::Value::Inst(*phi),
                    t: ScalarType::U64,
                },
            );
            acc = ir::Value::Inst(summed);
        }
        push(&mut u, exit, Inst::Ret { v: acc, agg: false });
        commit_unit(&mut k1.ir, &u, &mut unit);
        k1.ir.release_unit_buf(u);
        unit.is_optimized = true;
        *k1.ir.function_unit_mut(f).unwrap() = unit;

        let mut vm = vm::Vm::make();
        let raw =
            super::super::exec::execute_compiled_function_raw(&mut k1, &mut vm, f, &[], false)
                .unwrap();
        vm::load_value(raw.ret_pt, raw.ret_addr).bits()
    }

    fn expected_rotation(inits: &[u32], sources: &[usize], trips: u32) -> u64 {
        let mut values: Vec<u64> = Vec::new();
        for init in inits {
            values.push(*init as u64);
        }
        for _ in 0..trips {
            let mut next = Vec::new();
            for source in sources {
                next.push(values[*source]);
            }
            values = next;
        }
        let mut acc = 0;
        for value in values {
            acc = acc * 100 + value;
        }
        acc
    }

    #[test]
    fn phi_swap_copies_in_parallel() {
        for body_block in [false, true] {
            let name = if body_block { "phi_swap_body" } else { "phi_swap_header" };
            let expected = expected_rotation(&[1, 2], &[1, 0], 3);
            assert_eq!(expected, 201);
            assert_eq!(run_phi_rotation(name, &[1, 2], &[1, 0], 3, body_block), expected);
        }
    }

    #[test]
    fn phi_three_cycle_with_chain_copies_in_parallel() {
        for body_block in [false, true] {
            let name = if body_block { "phi_cycle_body" } else { "phi_cycle_header" };
            let expected = expected_rotation(&[1, 2, 3, 4], &[2, 0, 1, 0], 1);
            assert_eq!(expected, 3_01_02_01);
            assert_eq!(
                run_phi_rotation(name, &[1, 2, 3, 4], &[2, 0, 1, 0], 1, body_block),
                expected
            );
            let expected = expected_rotation(&[1, 2, 3, 4], &[2, 0, 1, 0], 5);
            assert_eq!(
                run_phi_rotation(name, &[1, 2, 3, 4], &[2, 0, 1, 0], 5, body_block),
                expected
            );
        }
    }

    #[test]
    fn phi_chain_without_cycle_orders_reads_before_writes() {
        let expected = expected_rotation(&[7, 8, 9], &[0, 0, 1], 1);
        assert_eq!(expected, 7_07_08);
        assert_eq!(run_phi_rotation("phi_chain", &[7, 8, 9], &[0, 0, 1], 1, true), expected);
    }

    #[test]
    fn float_immediates_are_bit_patterns() {
        for value in [0.0f32, -0.0, 0.1, -1.5, f32::INFINITY, f32::NEG_INFINITY] {
            assert_eq!(data32_bits(ScalarType::F32, value.to_bits()), value.to_bits() as u64);
            assert_eq!(data32_bits(ScalarType::F64, value.to_bits()), (value as f64).to_bits());
        }
    }
}
