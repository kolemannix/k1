use super::*;

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
                    if !k1.ir.functions.contains_key(callee_id) {
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
                    inline_calls_in_unit(k1, &mut u, unit_id);
                    k1.trace_pop(inline_frame);

                    let simplify_frame = k1.trace_push_unit(TraceKind::IrSimplify, unit_id, None);
                    let passes = cfg_simplify(k1, &mut u);
                    k1.trace.set_top_count(passes);
                    k1.trace_pop(simplify_frame);

                    insts_created += (u.inst_count() - loaded_insts) as u64;
                    commit_unit(&mut k1.ir, &u, &mut unit);
                    k1.ir.release_unit_buf(u);
                }
                *get_compiled_unit_mut(&mut k1.ir, unit_id).unwrap() = unit;
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
    (callee.is_optimized && !callee.is_cold(k1) && callee.inst_count() < 20).then_some(call)
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

fn inline_calls_in_unit(k1: &mut TypedProgram, u: &mut UnitBuf, unit_id: IrUnitId) {
    debug!("Inlining calls in {}", unit_name_to_string(k1, unit_id));
    let mut result_rewrites = std::mem::take(&mut k1.ir.opt_buf_inline_self_rewrites);
    let mut inlined = 0u64;
    let mut cur_block = u.body.first_block;
    'scan: while let Some(block) = cur_block {
        let mut cur_inst = u.block(block).first;
        while let Some(inst_id) = cur_inst {
            if let Some(call) = inline_target(k1, &u.view(), u.inst(inst_id)) {
                cur_block = inline_call(k1, u, unit_id, block, inst_id, call, &mut result_rewrites);
                inlined += 1;
                continue 'scan;
            }
            cur_inst = u.next_inst(inst_id);
        }
        cur_block = u.block(block).next;
    }

    if !result_rewrites.values.is_empty() {
        let mut cur = u.body.first_block;
        while let Some(block) = cur {
            rewrite_in_block(u, block, &result_rewrites);
            cur = u.block(block).next;
        }
    }
    result_rewrites.clear();
    k1.ir.opt_buf_inline_self_rewrites = result_rewrites;
    k1.trace.set_top_count(inlined);
}

fn inline_call(
    k1: &mut TypedProgram,
    u: &mut UnitBuf,
    self_unit_id: IrUnitId,
    call_block: BlockId,
    call_inst_id: InstId,
    call: IrCall,
    result_rewrites: &mut RewriteMappings,
) -> Option<BlockId> {
    debug!("Inlining call i{} {}", call_inst_id, inst_to_string(k1, &u.view(), call_inst_id));
    let self_fn_type = get_compiled_unit(&k1.ir, self_unit_id).unwrap().fn_type;
    let IrCallee::Direct(callee_fn_id) = call.callee else { panic!() };
    let call_span = u.view().span(call_inst_id);
    let callee_unit = *k1.ir.functions.get(&callee_fn_id).unwrap();

    let entry_span = match self_unit_id {
        IrUnitId::Expr(e) => k1.exprs.get_span(e),
        IrUnitId::Function(id) => k1.get_function_span(id),
    };

    let call_next = u.next_inst(call_inst_id);
    u.remove_inst(call_inst_id);

    let call_post_block = call_next.map(|next| u.split_block_at(call_block, next));

    if let Some(call_post_block) = call_post_block {
        let terminator = u.block(call_post_block).last.unwrap();
        retarget_successors(u, terminator, call_block, call_post_block, false);
    }

    let mut b = Builder {
        k1,
        u,
        fn_type: self_fn_type,
        returned_alloca: None,
        cur_block: call_block,
        cur_span: call_span,
        entry_span,
    };
    let inlined = inline_body(&mut b, callee_unit, call, call_post_block);
    if call.dst.is_none()
        && let Some(result) = inlined.result
    {
        result_rewrites.values.insert(call_inst_id, result);
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
) -> K1Result<Value> {
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
    let callee_unit = *b.k1.ir.functions.get(&callee_id).unwrap();
    let exit_block = b.push_block(BlockSourceKind::InlineExit);
    let inlined = inline_body(b, callee_unit, call, Some(exit_block));
    b.goto_block(exit_block);
    if callee_unit.fn_type.diverges {
        return Ok(b.push_inst_anon(Inst::Unreachable).as_value());
    }
    Ok(match call.dst {
        Some(dst) => dst,
        None => inlined.result.unwrap_or(Value::Empty),
    })
}

struct InlinedBody {
    result: Option<Value>,
    last_block: Option<BlockId>,
}

fn inline_body(
    b: &mut Builder,
    callee_unit: IrUnit,
    call: IrCall,
    exit_block: Option<BlockId>,
) -> InlinedBody {
    let call_block = b.cur_block;
    let callee = callee_unit.view(&b.k1.ir.mem);
    let call_args = b.k1.tmp.pushn(b.u.view().args(call.args));
    let mut inlined_rewrites = std::mem::take(&mut b.k1.ir.opt_buf_inline_inlined_rewrites);
    inlined_rewrites.fn_params = Some(b.k1.tmp.getn(call_args));

    enum InlinedReturnInfo {
        Empty,
        AggInStorage(Value),
        ScalarInPhi(InstId, List<PhiCase, MemTmp>),
    }
    let mut return_info = match call.dst {
        None => match call.ret_type.as_enum() {
            PhysicalTypeEnum::Scalar(_) => {
                let incomings = b.k1.tmp.new_list(0);
                b.goto_block(exit_block.unwrap());
                let phi = b.push_inst_front(
                    Inst::Phi { t: call.ret_type, incomings: IrRange::EMPTY },
                    IrComment::InlinedScalarReturn,
                );
                b.cur_block = call_block;
                InlinedReturnInfo::ScalarInPhi(phi, incomings)
            }
            PhysicalTypeEnum::Agg(_) => {
                let dst_alloca = b.push_alloca(call.ret_type, IrComment::InlineRet);
                InlinedReturnInfo::AggInStorage(dst_alloca.as_value())
            }
            PhysicalTypeEnum::Empty => InlinedReturnInfo::Empty,
        },
        Some(dst) => InlinedReturnInfo::AggInStorage(dst),
    };

    let mut inlined_first: Option<BlockId> = None;
    let mut inlined_last: Option<BlockId> = None;
    let mut returns = b.k1.tmp.new_list(0);
    for (index, callee_block_id) in callee.block_ids().enumerate() {
        let inlined_block = b.u.insert_block_after(b.cur_block, callee.block(callee_block_id).kind);
        if inlined_first.is_none() {
            inlined_first = Some(inlined_block);
        }
        inlined_last = Some(inlined_block);

        inlined_rewrites.blocks.insert(callee_block_id, inlined_block);
        if index == 0 {
            b.push_jump(inlined_block, IrComment::EnterInlinedCode);
        }
        b.cur_block = inlined_block;
        for callee_inst in callee.block_insts(callee_block_id) {
            let mut inst = *callee.inst(callee_inst);
            match &mut inst {
                Inst::Ret { v, .. } => {
                    returns.push_grow(&mut b.k1.tmp, (inlined_block, *v));
                    continue;
                }
                Inst::Alloca { returned, .. } => {
                    *returned = false;
                }
                _ => {}
            }
            b.u.clone_payload(&callee, &mut inst);
            let comment = callee.comment(callee_inst);
            let new_inst = if let Inst::Alloca { t, .. } = inst {
                b.push_alloca(t, comment)
            } else {
                b.push_inst(inst, comment)
            };
            inlined_rewrites.values.insert(callee_inst, Value::Inst(new_inst));
        }
    }

    let mut cur = inlined_first;
    while let Some(block) = cur {
        rewrite_in_block(b.u, block, &inlined_rewrites);
        if cur == inlined_last {
            break;
        }
        cur = b.u.block(block).next;
    }

    for (inlined_block, mut v) in returns.as_slice().iter().copied() {
        rewrite_value(&inlined_rewrites, &mut v);
        b.cur_block = inlined_block;
        match &mut return_info {
            InlinedReturnInfo::Empty => {}
            InlinedReturnInfo::AggInStorage(dst_storage) => {
                store_value(b, call.ret_type, *dst_storage, v, IrComment::InlinedAggRet)
                    .expect("call.ret_type is not Empty");
            }
            InlinedReturnInfo::ScalarInPhi(_, cases) => {
                cases.push_grow(&mut b.k1.tmp, PhiCase { from: inlined_block, value: v })
            }
        }
        if let Some(exit_block) = exit_block {
            b.push_jump(exit_block, IrComment::ExitInlinedCode);
        }
    }

    let result = match return_info {
        InlinedReturnInfo::Empty => None,
        InlinedReturnInfo::AggInStorage(storage) => Some(storage),
        InlinedReturnInfo::ScalarInPhi(phi, cases) => {
            let range = b.u.push_phi_cases(cases.as_slice());
            let Inst::Phi { incomings, .. } = b.u.inst_mut(phi) else { panic!() };
            *incomings = range;
            Some(Value::Inst(phi))
        }
    };

    inlined_rewrites.clear();
    b.k1.ir.opt_buf_inline_inlined_rewrites = inlined_rewrites;
    b.cur_block = call_block;
    InlinedBody { result, last_block: inlined_last }
}

fn rewrite_in_block(u: &mut UnitBuf, block: BlockId, mappings: &RewriteMappings) {
    let mut cur = u.block(block).first;
    while let Some(inst_id) = cur {
        rewrite_instr(u, mappings, inst_id);
        cur = u.next_inst(inst_id);
    }
}

#[derive(Default)]
pub struct RewriteMappings {
    values: IdMap<InstId, Value>,
    fn_params: Option<&'static [Value]>,
    blocks: IdMap<BlockId, BlockId>,
}

impl RewriteMappings {
    pub fn clear(&mut self) {
        self.values.clear();
        self.fn_params = None;
        self.blocks.clear();
    }
}

fn rewrite_instr(u: &mut UnitBuf, mappings: &RewriteMappings, inst_id: InstId) {
    u.map_inst_refs(
        inst_id,
        &|v| {
            rewrite_value(mappings, v);
        },
        &|b| {
            if let Some(new) = mappings.blocks.get(*b) {
                *b = new;
            }
        },
    );
    let view = u.view();
    let now_unaligned = match *view.inst(inst_id) {
        Inst::Store { dst, value, t, .. } => {
            is_addr_unaligned(&view, dst) || (t.is_agg() && is_addr_unaligned(&view, value))
        }
        Inst::Load { src, dst, .. } => {
            (dst != Value::Empty && is_addr_unaligned(&view, dst)) || is_addr_unaligned(&view, src)
        }
        Inst::Copy { dst, src, .. } => {
            is_addr_unaligned(&view, dst) || is_addr_unaligned(&view, src)
        }
        Inst::StructOffset { base, .. } => is_addr_unaligned(&view, base),
        _ => false,
    };
    if now_unaligned {
        match u.inst_mut(inst_id) {
            Inst::Store { unaligned, .. }
            | Inst::Load { unaligned, .. }
            | Inst::Copy { unaligned, .. }
            | Inst::StructOffset { unaligned, .. } => *unaligned = true,
            _ => {}
        }
    }
}

fn rewrite_value(mappings: &RewriteMappings, value: &mut Value) -> bool {
    match *value {
        Value::Inst(inst_id) => {
            if let Some(new) = mappings.values.get(inst_id) {
                *value = new;
                true
            } else {
                false
            }
        }
        Value::FnParam { index, .. } => {
            if let Some(new_params) = mappings.fn_params {
                *value = new_params[index as usize];
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

pub fn cfg_simplify(k1: &mut TypedProgram, u: &mut UnitBuf) -> u64 {
    u.compute_preds();

    let mut remove = k1.tmp.new_list(0);
    let entry = u.body.first_block;
    let mut cur = entry;
    while let Some(block_id) = cur {
        if cur != entry && u.preds(block_id).is_empty() {
            debug!("dead b{}", block_id);
            remove.push_grow(&mut k1.tmp, block_id);
        }
        cur = u.block(block_id).next;
    }

    if !remove.is_empty() {
        let mut cur = entry;
        while let Some(block_id) = cur {
            remove_phi_incomings(u, block_id, remove.as_slice());
            cur = u.block(block_id).next;
        }
        for block_id in remove.as_slice() {
            debug!("removing b{}", block_id);
            u.remove_block(*block_id);
        }
    }

    let mut rewrites = std::mem::take(&mut k1.ir.opt_buf_cfg_simpl_rewrites);

    let mut passes = 1u64;
    while do_pass(u, &mut rewrites) {
        passes += 1;
    }

    if !rewrites.values.is_empty() {
        let mut cur = u.body.first_block;
        while let Some(block_id) = cur {
            rewrite_in_block(u, block_id, &rewrites);
            cur = u.block(block_id).next;
        }
    }
    rewrites.clear();
    k1.ir.opt_buf_cfg_simpl_rewrites = rewrites;

    fn do_pass(u: &mut UnitBuf, rewrites: &mut RewriteMappings) -> bool {
        let mut noop = true;

        let mut cur = u.body.first_block;
        while let Some(block_id) = cur {
            let node = *u.block(block_id);

            if u.preds(block_id).len() == 1
                && node.first.is_some()
                && node.first == node.last
                && let Inst::Jump(succ) = *u.inst(node.first.unwrap())
                && !u.inst(u.block(succ).first.unwrap()).is_phi()
            {
                let pred = u.preds(block_id)[0];
                debug!("trampoline b{}", block_id);
                let pred_last = u.block(pred).last.unwrap();
                u.map_inst_refs(pred_last, &|_| {}, &|target| {
                    if *target == block_id {
                        *target = succ;
                    }
                });

                rewrite_phi_incoming(u, succ, block_id, pred);
                replace_pred(u, succ, block_id, pred);
                u.remove_block(block_id);
                noop = false;
                cur = node.next;
                continue;
            }

            if let Some(jump) = node.last
                && let Inst::Jump(succ) = *u.inst(jump)
                && succ != block_id
                && u.preds(succ) == [block_id]
            {
                debug!("merge case");
                u.remove_inst(jump);

                let mut succ_cur = u.block(succ).first;
                while let Some(inst_id) = succ_cur {
                    succ_cur = u.next_inst(inst_id);
                    let mut keep = true;
                    if let Inst::Phi { incomings, .. } = *u.inst(inst_id) {
                        if incomings.len() == 1 {
                            let only_case = u.view().phi_cases(incomings)[0];
                            debug_assert_eq!(only_case.from, block_id);
                            let mut resolved = only_case.value;
                            if let Value::Inst(id) = resolved {
                                if let Some(v) = rewrites.values.get(id) {
                                    resolved = v;
                                }
                            }
                            debug_assert!(!matches!(resolved, Value::Inst(id) if id == inst_id));
                            rewrites.values.update_values(|v| {
                                if matches!(*v, Value::Inst(id) if id == inst_id) {
                                    *v = resolved;
                                }
                            });
                            rewrites.values.insert(inst_id, resolved);
                            debug!("block_merge: skipping single phi i{}", inst_id);
                            keep = false;
                        } else {
                            debug!("block_merge: keeping multi phi i{}", inst_id);
                        }
                    }
                    u.remove_inst(inst_id);
                    if keep {
                        u.push_inst(block_id, inst_id);
                    }
                }

                let terminator = u.block(block_id).last.unwrap();
                retarget_successors(u, terminator, succ, block_id, true);
                u.remove_block(succ);

                noop = false;
                continue;
            }

            cur = node.next;
        }
        !noop
    }
    debug!("cfg_simplify end\n{}", blocks_to_string(k1, &u.view(), false));
    passes
}

fn retarget_successors(
    u: &mut UnitBuf,
    terminator: InstId,
    old: BlockId,
    new: BlockId,
    with_preds: bool,
) {
    let retarget = |u: &mut UnitBuf, target: BlockId| {
        rewrite_phi_incoming(u, target, old, new);
        if with_preds {
            replace_pred(u, target, old, new);
        }
    };
    match *u.inst(terminator) {
        Inst::Jump(target) => retarget(u, target),
        Inst::JumpIf { cons, alt, .. } => {
            retarget(u, cons);
            retarget(u, alt);
        }
        Inst::Switch { cases, default, .. } => {
            for i in 0..cases.len() as usize {
                let target = u.view().switch_cases(cases)[i].target;
                retarget(u, target);
            }
            retarget(u, default);
        }
        _ => {}
    }
}

fn replace_pred(u: &mut UnitBuf, block: BlockId, old: BlockId, new: BlockId) {
    for pred in u.preds_mut(block) {
        if *pred == old {
            *pred = new;
        }
    }
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
        u.retain_phi_cases(inst_id, |case| !dead_block_ids.contains(&case.from));
        cur = u.next_inst(inst_id);
    }
}
