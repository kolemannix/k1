use super::*;

mod promote;
pub(super) use promote::promote_unit;
pub use promote::{LocalUses, PromoteState};

#[cfg(test)]
mod tests;

pub enum OptVisit {
    Enter(IrUnitId),
    Leave(IrUnitId),
}

pub fn optimize_unit(k1: &mut TypedProgram, root: IrUnitId) -> K1Result<()> {
    let Some(&unit) = get_compiled_unit(&k1.ir, root) else {
        return Ok(());
    };
    if unit.is_optimized {
        return Ok(());
    }
    let frame = k1.trace_push_unit(TraceKind::IrOptimize, root, None);
    let result = optimize_unit_body(k1, root, unit);
    k1.trace_pop(frame);
    result
}

fn optimize_unit_body(k1: &mut TypedProgram, root: IrUnitId, unit: IrUnit) -> K1Result<()> {
    if unit.is_debug {
        eprintln!("optimizing {}", unit_to_string(k1, root, true));
    }

    let skip_inline = !k1.optimize_ir();

    let mut visit_stack = std::mem::take(&mut k1.ir.opt_buf_visit_stack);
    let mut visited = std::mem::take(&mut k1.ir.opt_buf_visited);
    let mut callees = std::mem::take(&mut k1.ir.opt_buf_callees);
    let mut insts_created = 0u64;
    visit_stack.push(OptVisit::Enter(root));
    let mut result = Ok(());
    'walk: while let Some(visit) = visit_stack.pop() {
        match visit {
            OptVisit::Enter(unit_id) => {
                if !visited.insert(unit_id) {
                    continue;
                }
                if get_compiled_unit(&k1.ir, unit_id).unwrap().is_optimized {
                    continue;
                }
                visit_stack.push(OptVisit::Leave(unit_id));
                callees.clear();
                collect_direct_callees(k1, &mut callees, unit_id);
                let span = get_unit_span(k1, unit_id);
                for callee_id in &callees {
                    let callee = IrUnitId::Function(*callee_id);
                    if k1.ir.function_unit(*callee_id).is_none() {
                        let requester = k1.trace.top();
                        if let Err(e) = k1.compile_function_for_exec(*callee_id, requester, span) {
                            result = Err(e);
                            break 'walk;
                        }
                    }
                    if !get_compiled_unit(&k1.ir, callee).unwrap().is_optimized {
                        visit_stack.push(OptVisit::Enter(callee));
                    }
                }
            }
            OptVisit::Leave(unit_id) => {
                let mut unit = *get_compiled_unit(&k1.ir, unit_id).unwrap();
                if unit.is_optimized {
                    continue;
                }
                unit.is_optimized = true;
                if !skip_inline && has_inline_target(k1, &unit.view(&k1.ir.mem)) {
                    let mut u = k1.ir.take_unit_buf();
                    u.load(&k1.ir.mem, &unit);
                    let loaded_insts = u.inst_count();

                    let inline_frame = k1.trace_push_unit(TraceKind::IrInline, unit_id, None);
                    let exposes_locals = inline_calls_in_unit(k1, &mut u, unit_id);
                    k1.trace_pop(inline_frame);

                    let simplify_frame = k1.trace_push_unit(TraceKind::IrSimplify, unit_id, None);
                    remove_unreachable_blocks(k1, &mut u);
                    let passes = merge_blocks(k1, &mut u);
                    k1.trace.set_top_count(passes);
                    k1.trace_pop(simplify_frame);

                    if exposes_locals {
                        promote_unit(k1, &mut u, unit_id, None);
                    }

                    insts_created += (u.inst_count() - loaded_insts) as u64;
                    commit_unit(&mut k1.ir, &mut u, &mut unit);
                    k1.ir.release_unit_buf(u);
                }
                *get_compiled_unit_mut(&mut k1.ir, unit_id).unwrap() = unit;
                if cfg!(debug_assertions)
                    && let Err(e) = validate_unit(k1, unit_id)
                {
                    result = Err(e);
                    break 'walk;
                }
            }
        }
    }
    visit_stack.clear();
    visited.clear();
    callees.clear();
    k1.ir.opt_buf_visit_stack = visit_stack;
    k1.ir.opt_buf_visited = visited;
    k1.ir.opt_buf_callees = callees;
    k1.trace.set_top_count(insts_created);
    result
}

fn collect_direct_callees(k1: &TypedProgram, callees: &mut Vec<FunctionId>, unit_id: IrUnitId) {
    let u = get_compiled_unit(&k1.ir, unit_id).unwrap().view(&k1.ir.mem);
    for block in u.block_ids() {
        for inst_id in u.block_insts(block) {
            if let Inst::Call { call_id } = *u.inst(inst_id) {
                if let IrCallee::Direct(function_id) = u.call(call_id).callee {
                    callees.push(function_id)
                }
            }
        }
    }
}

fn inline_target(k1: &TypedProgram, u: &UnitView, inst: &Inst) -> Option<IrCall> {
    let Inst::Call { call_id } = *inst else { return None };
    let call = *u.call(call_id);
    let IrCallee::Direct(function_id) = call.callee else { return None };
    let function = k1.functions.get(function_id);
    if function.is_reloadable() || function.is_noinline() {
        return None;
    }
    let callee = get_compiled_unit(&k1.ir, IrUnitId::Function(function_id)).unwrap();
    if callee.is_optimized && !callee.is_cold(k1) && callee.inst_count() < 20 {
        Some(call)
    } else {
        None
    }
}

fn has_inline_target(k1: &TypedProgram, u: &UnitView) -> bool {
    for block in u.block_ids() {
        for inst_id in u.block_insts(block) {
            if inline_target(k1, u, u.inst(inst_id)).is_some() {
                return true;
            }
        }
    }
    false
}

fn inline_calls_in_unit(k1: &mut TypedProgram, u: &mut UnitBuf, unit_id: IrUnitId) -> bool {
    debug!("Inlining calls in {}", unit_name_to_string(k1, unit_id));
    let mut inlined_inst_count = 0u64;
    let mut exposes_locals = false;
    let mut cur_block = u.body.first_block;
    'scan: while let Some(block) = cur_block {
        let mut cur_inst = u.block(block).first;
        while let Some(inst_id) = cur_inst {
            if let Some(call) = inline_target(k1, &u.view(), u.inst(inst_id)) {
                exposes_locals |= inline_exposes_locals(k1, u, call);
                cur_block = inline_call(k1, u, unit_id, block, inst_id, call);
                inlined_inst_count += 1;
                continue 'scan;
            }
            cur_inst = u.next_inst(inst_id);
        }
        cur_block = u.block(block).next;
    }

    k1.trace.set_top_count(inlined_inst_count);
    exposes_locals
}

fn inline_exposes_locals(k1: &TypedProgram, u: &UnitBuf, call: IrCall) -> bool {
    let IrCallee::Direct(callee_fn_id) = call.callee else { return false };
    let callee = k1.ir.function_unit(callee_fn_id).unwrap().view(&k1.ir.mem);
    let callee_has_allocas = callee
        .first_block()
        .and_then(|entry| callee.block(entry).first)
        .is_some_and(|first| matches!(callee.inst(first), Inst::Alloca { .. }));
    if callee_has_allocas || (call.dst.is_none() && call.ret_type.is_agg()) {
        return true;
    }
    let is_local_addr = |v: Value| match u.subst.resolve(v) {
        Value::Inst(id) => matches!(
            u.inst(id),
            Inst::Alloca { .. } | Inst::StructOffset { .. } | Inst::ArrayOffset { .. }
        ),
        _ => false,
    };
    call.dst.is_some_and(is_local_addr)
        || u.view().args(call.args).iter().any(|a| is_local_addr(*a))
}

fn inline_call(
    k1: &mut TypedProgram,
    u: &mut UnitBuf,
    self_unit_id: IrUnitId,
    call_block: BlockId,
    call_inst_id: InstId,
    call: IrCall,
) -> Option<BlockId> {
    debug!("Inlining call i{} {}", call_inst_id, inst_to_string(k1, &u.view(), call_inst_id));
    let self_fn_type = get_compiled_unit(&k1.ir, self_unit_id).unwrap().fn_type;
    let IrCallee::Direct(callee_fn_id) = call.callee else { panic!() };
    let call_span = u.view().span(call_inst_id);
    let callee_unit = *k1.ir.function_unit(callee_fn_id).unwrap();

    let entry_span = match self_unit_id {
        IrUnitId::Expr(e) => k1.exprs.get_span(e),
        IrUnitId::Function(id) => k1.get_function_span(id),
    };

    let call_next = u.next_inst(call_inst_id);
    u.remove_inst(call_inst_id);

    let call_post_block = call_next.map(|next| u.split_block_at(call_block, next));

    if let Some(call_post_block) = call_post_block {
        retarget_successors(u, call_post_block, call_block);
    }

    let mut b = Builder {
        k1,
        u,
        fn_type: self_fn_type,
        returned_alloca: None,
        cur_block: call_block,
        cur_span: call_span,
        entry_span,
        local_uses: None,
    };
    let inlined = inline_body(&mut b, callee_unit, call, call_post_block);
    if call.dst.is_none()
        && let Some(result) = inlined.result
    {
        b.u.subst.insert(call_inst_id, result);
    }

    match inlined.last_block {
        None => match call_post_block {
            Some(post) => Some(post),
            None => b.u.block(call_block).next,
        },
        Some(last) => b.u.block(last).next,
    }
}

pub(super) fn compile_inline_call(
    b: &mut Builder,
    callee_id: FunctionId,
    call: IrCall,
) -> K1Result<Option<Value>> {
    if b.k1.trace.stack_contains_key(TraceKind::IrLower, callee_id.as_u32()) {
        kbail!(
            b.k1,
            b.cur_span,
            "Cannot inline {}: it is recursive through fn(inline) calls, so the inlining would never end",
            b.k1.ident_str(b.k1.get_function(callee_id).name)
        );
    }
    let caller_variables = std::mem::take(&mut b.k1.ir.b_variables);
    let caller_loops = std::mem::take(&mut b.k1.ir.b_loops);
    let compiled =
        b.k1.require_function_body(callee_id, b.cur_span)
            .and_then(|_| compile_function(b.k1, callee_id, None));
    b.k1.ir.b_variables = caller_variables;
    b.k1.ir.b_loops = caller_loops;
    compiled?;
    let callee_unit = *b.k1.ir.function_unit(callee_id).unwrap();
    let inlined = inline_body(b, callee_unit, call, None);
    if inlined.returns == 0 {
        let dead = b.push_block(BlockSourceKind::InlineExit);
        b.goto_block(dead);
        return Ok(None);
    }
    Ok(Some(match call.dst {
        Some(dst) => dst,
        None => inlined.result.unwrap_or(Value::Empty),
    }))
}

struct InlinedBody {
    result: Option<Value>,
    last_block: Option<BlockId>,
    returns: u32,
}

#[derive(Default)]
pub struct InlineState {
    callee: UnitView<'static>,
    args: &'static [Value],
    values: IdMap<InstId, Value>,
    blocks: IdMap<BlockId, BlockId>,
    pending: Vec<BlockId>,
    phis: Vec<(InstId, BlockId)>,
    layout_block: BlockId,
}

impl InlineState {
    fn mapped_value(&self, v: Value) -> Value {
        match v {
            Value::Inst(id) => self.values.get(id).expect("inlined use precedes its def"),
            Value::FnParam { index, .. } => self.args[index as usize],
            v => v,
        }
    }

    fn map_inst(&self, u: &mut UnitBuf, inst: &mut Inst) {
        u.map_refs_of(inst, &|v| *v = self.mapped_value(*v), &|b| {
            *b = self.blocks.get(*b).expect("inlined edge to an uncopied block")
        });
        u.mark_unaligned(inst);
    }

    fn block(&mut self, u: &mut UnitBuf, callee_block: BlockId) -> BlockId {
        if let Some(block) = self.blocks.get(callee_block) {
            return block;
        }
        let block = u.insert_block_after(self.layout_block, self.callee.block(callee_block).kind);
        self.layout_block = block;
        self.blocks.insert(callee_block, block);
        self.pending.push(callee_block);
        block
    }
}

fn inline_branch(b: &mut Builder, st: &mut InlineState, inst: Inst, comment: IrComment) {
    let known = match inst {
        Inst::Jump(target) => Some(target),
        Inst::JumpIf { cond, cons, alt } => {
            b.known_bool(st.mapped_value(cond)).map(|taken| if taken { cons } else { alt })
        }
        Inst::Switch { value, cases, default } => {
            let value = st.mapped_value(value);
            b.known_bits(value).map(|bits| {
                let width = b.get_value_kind(value).expect_scalar().width_bits();
                switch_target(st.callee.switch_cases(cases), default, width, bits)
            })
        }
        _ => unreachable!("not a branch"),
    };
    if let Some(target) = known {
        let target = st.block(b.u, target);
        b.push_jump(target, comment);
        return;
    }
    match inst {
        Inst::JumpIf { cond, cons, alt } => {
            let cons = st.block(b.u, cons);
            let alt = st.block(b.u, alt);
            b.push_jump_if(st.mapped_value(cond), cons, alt, comment);
        }
        Inst::Switch { value, cases, default } => {
            let cases = st.callee.switch_cases(cases);
            let mut mapped = b.k1.tmp.new_list(cases.len() as u32);
            for case in cases {
                mapped.push(SwitchCase { value: case.value, target: st.block(b.u, case.target) });
            }
            let default = st.block(b.u, default);
            b.push_switch(st.mapped_value(value), mapped.as_slice(), default, comment);
        }
        _ => unreachable!("not a branch"),
    }
}

fn inline_body(
    b: &mut Builder,
    callee_unit: IrUnit,
    call: IrCall,
    exit_block: Option<BlockId>,
) -> InlinedBody {
    let call_block = b.cur_block;
    let mut inline_state = std::mem::take(&mut b.k1.ir.opt_buf_inline);
    let callee = callee_unit.view(&b.k1.ir.mem);
    let call_args = b.k1.tmp.pushn(b.u.view().args(call.args));
    inline_state.callee = callee;
    inline_state.args = b.k1.tmp.getn(call_args);
    inline_state.layout_block = call_block;

    let return_storage = match call.dst {
        Some(dst) => Some(dst),
        None => match call.ret_type.as_enum() {
            PhysicalTypeEnum::Agg(_) => {
                Some(b.push_alloca(call.ret_type, IrComment::InlineRet).as_value())
            }
            PhysicalTypeEnum::Scalar(_) | PhysicalTypeEnum::Empty => None,
        },
    };

    let mut returned_allocas: crate::SV4<InstId> = smallvec::smallvec![];
    let mut ret_values: crate::SV4<Value> = smallvec::smallvec![];
    for callee_block_id in callee.block_ids() {
        for callee_inst in callee.block_insts(callee_block_id) {
            match *callee.inst(callee_inst) {
                Inst::Ret { v, .. } => ret_values.push(v),
                Inst::Alloca { returned: true, .. } => returned_allocas.push(callee_inst),
                _ => {}
            }
        }
    }
    let nrvo = match (return_storage, returned_allocas.as_slice()) {
        (Some(storage), [returned]) if ret_values.iter().all(|v| *v == Value::Inst(*returned)) => {
            let view = b.u.view();
            let storage_is_local =
                matches!(storage, Value::Inst(id) if matches!(view.inst(id), Inst::Alloca { .. }));
            let arg_aliases = inline_state.args.iter().any(|arg| addr_root(&view, *arg) == storage);
            let target = if storage_is_local && !arg_aliases {
                storage
            } else {
                b.push_alloca(call.ret_type, IrComment::InlineRet).as_value()
            };
            inline_state.values.insert(*returned, target);
            Some(*returned)
        }
        _ => None,
    };

    let entry = callee.first_block().unwrap();
    inline_state.blocks.insert(entry, call_block);
    inline_state.pending.push(entry);
    let mut exit = exit_block;
    let mut phi_cases = b.k1.tmp.new_list(ret_values.len() as u32);
    let mut single_result = None;
    let mut return_block = None;
    let mut returns = 0u32;
    while let Some(callee_block_id) = inline_state.pending.pop() {
        let inlined_block = inline_state.blocks.get(callee_block_id).unwrap();
        b.cur_block = inlined_block;
        for callee_inst in callee.block_insts(callee_block_id) {
            let mut inst = *callee.inst(callee_inst);
            let comment = callee.comment(callee_inst);
            match &mut inst {
                Inst::Ret { v, .. } => {
                    let v = inline_state.mapped_value(*v);
                    returns += 1;
                    match return_storage {
                        Some(storage) if storage == v => {}
                        Some(storage) => {
                            store_value(b, call.ret_type, storage, v, IrComment::InlinedAggRet)
                                .expect("call.ret_type is not Empty");
                        }
                        None if call.ret_type.is_scalar() => {
                            phi_cases.push(PhiCase { from: inlined_block, value: v });
                            single_result = Some(v);
                        }
                        None => {}
                    }
                    if exit.is_none() && returns == 2 {
                        let new_exit = b.u.insert_block_after(
                            inline_state.layout_block,
                            BlockSourceKind::InlineExit,
                        );
                        b.cur_block = return_block.unwrap();
                        b.push_jump(new_exit, IrComment::ExitInlinedCode);
                        b.cur_block = inlined_block;
                        exit = Some(new_exit);
                    }
                    return_block = Some(inlined_block);
                    if let Some(exit) = exit {
                        b.push_jump(exit, IrComment::ExitInlinedCode);
                    }
                    continue;
                }
                Inst::Alloca { .. } if nrvo == Some(callee_inst) => continue,
                Inst::Alloca { returned, .. } => {
                    *returned = false;
                    let id = b.u.new_inst(inst, b.entry_span, comment);
                    b.link_alloca(id);
                    inline_state.values.insert(callee_inst, Value::Inst(id));
                    continue;
                }
                Inst::Phi { .. } => {
                    b.u.clone_payload(&callee, &mut inst);
                    let id = b.u.new_inst(inst, b.cur_span, comment);
                    b.u.push_inst(inlined_block, id);
                    inline_state.phis.push((id, inlined_block));
                    inline_state.values.insert(callee_inst, Value::Inst(id));
                    continue;
                }
                Inst::Jump(_) | Inst::JumpIf { .. } | Inst::Switch { .. } => {
                    inline_branch(b, &mut inline_state, inst, comment);
                    continue;
                }
                _ => {}
            }
            b.u.clone_payload(&callee, &mut inst);
            inline_state.map_inst(b.u, &mut inst);
            let value = b.push_value(inst, comment);
            inline_state.values.insert(callee_inst, value);
        }
    }

    for &(phi, block) in &inline_state.phis {
        b.u.retain_phi_cases(phi, |u, case| {
            inline_state.blocks.get(case.from).is_some_and(|from| u.is_pred(block, from))
        });
        let mut inst = *b.u.inst(phi);
        inline_state.map_inst(b.u, &mut inst);
        *b.u.inst_mut(phi) = inst;
        b.record_local_uses(&inst, phi, block);
    }

    let result = match return_storage {
        Some(storage) => Some(storage),
        None if !call.ret_type.is_scalar() => None,
        None if returns <= 1 => single_result,
        None => {
            let exit = exit.unwrap();
            let incomings = b.u.push_phi_cases(phi_cases.as_slice());
            b.cur_block = exit;
            let phi = b.push_inst_front(
                Inst::Phi { t: call.ret_type, incomings },
                IrComment::InlinedScalarReturn,
            );
            Some(Value::Inst(phi))
        }
    };

    if let Some(continue_block) = exit.or(return_block) {
        b.cur_block = continue_block;
    }
    let last_block = if inline_state.layout_block == call_block {
        None
    } else {
        Some(inline_state.layout_block)
    };
    inline_state.values.clear();
    inline_state.blocks.clear();
    inline_state.phis.clear();
    b.k1.ir.opt_buf_inline = inline_state;
    InlinedBody { result, last_block, returns }
}

pub fn remove_unreachable_blocks(k1: &mut TypedProgram, u: &mut UnitBuf) {
    u.mark_reachable();
    let mut remove = k1.tmp.new_list(0);
    let mut cur = u.body.first_block;
    while let Some(block_id) = cur {
        if !u.is_reachable(block_id) {
            remove.push_grow(&mut k1.tmp, block_id);
        }
        cur = u.block(block_id).next;
    }

    if !remove.is_empty() {
        let mut cur = u.body.first_block;
        while let Some(block_id) = cur {
            remove_phi_incomings(u, block_id, remove.as_slice());
            cur = u.block(block_id).next;
        }
        for block_id in remove.as_slice() {
            u.drop_edges(*block_id);
            u.remove_block(*block_id);
        }
    }
}

pub fn merge_blocks(k1: &mut TypedProgram, u: &mut UnitBuf) -> u64 {
    let mut passes = 1u64;
    while do_pass(u) {
        passes += 1;
    }

    fn do_pass(u: &mut UnitBuf) -> bool {
        let mut noop = true;

        let mut cur = u.body.first_block;
        while let Some(block_id) = cur {
            if let Some(pred) = u.single_pred(block_id)
                && strip_single_pred_phis(u, block_id, pred)
            {
                noop = false;
            }
            let node = *u.block(block_id);

            if let Some(pred) = u.single_pred(block_id)
                && node.first.is_some()
                && node.first == node.last
                && let Inst::Jump(succ) = *u.inst(node.first.unwrap())
                && (!u.inst(u.block(succ).first.unwrap()).is_phi() || !u.is_pred(succ, pred))
            {
                let pred_last = u.block(pred).last.unwrap();
                u.map_inst_refs(pred_last, &|_| {}, &|target| {
                    if *target == block_id {
                        *target = succ;
                    }
                });

                rewrite_phi_incoming(u, succ, block_id, pred);
                u.replace_pred(succ, block_id, pred);
                u.remove_block(block_id);
                noop = false;
                cur = node.next;
                continue;
            }

            if let Some(jump) = node.last
                && let Inst::Jump(succ) = *u.inst(jump)
                && succ != block_id
                && u.single_pred(succ) == Some(block_id)
            {
                u.remove_inst(jump);
                strip_single_pred_phis(u, succ, block_id);

                let mut succ_cur = u.block(succ).first;
                while let Some(inst_id) = succ_cur {
                    succ_cur = u.next_inst(inst_id);
                    u.remove_inst(inst_id);
                    u.move_inst_to_end(block_id, inst_id);
                }

                retarget_successors(u, block_id, succ);
                u.remove_block(succ);

                noop = false;
                continue;
            }

            cur = node.next;
        }
        !noop
    }
    debug!("merge_blocks end\n{}", blocks_to_string(k1, &u.view(), false));
    passes
}

fn strip_single_pred_phis(u: &mut UnitBuf, block_id: BlockId, pred: BlockId) -> bool {
    let mut stripped = false;
    while let Some(inst_id) = u.block(block_id).first
        && let Inst::Phi { incomings, .. } = *u.inst(inst_id)
    {
        debug_assert_eq!(incomings.len(), 1);
        let only_case = u.view().phi_cases(incomings)[0];
        debug_assert_eq!(only_case.from, pred);
        u.subst.insert(inst_id, only_case.value);
        u.remove_inst(inst_id);
        stripped = true;
    }
    stripped
}

fn retarget_successors(u: &mut UnitBuf, block: BlockId, old_source: BlockId) {
    u.for_each_successor(block, |u, target| {
        rewrite_phi_incoming(u, target, old_source, block);
        u.replace_pred(target, old_source, block);
    });
}

fn rewrite_phi_incoming(u: &mut UnitBuf, phi_block_id: BlockId, from: BlockId, to: BlockId) {
    let mut cur = u.block(phi_block_id).first;
    while let Some(inst_id) = cur {
        let Inst::Phi { incomings, .. } = *u.inst(inst_id) else { break };
        for phi_case in u.phi_cases_mut(incomings) {
            if phi_case.from == from {
                phi_case.from = to;
            }
        }
        cur = u.next_inst(inst_id);
    }
}

fn remove_phi_incomings(u: &mut UnitBuf, phi_block_id: BlockId, dead_block_ids: &[BlockId]) {
    let mut cur = u.block(phi_block_id).first;
    while let Some(inst_id) = cur {
        if !u.inst(inst_id).is_phi() {
            break;
        }
        u.retain_phi_cases(inst_id, |_, case| !dead_block_ids.contains(&case.from));
        cur = u.next_inst(inst_id);
    }
}
