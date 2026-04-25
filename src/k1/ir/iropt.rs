use super::*;

pub enum OptVisit {
    Enter(IrUnitId),
    Leave(IrUnitId),
}

pub fn optimize_unit(k1: &mut TypedProgram, unit_id: IrUnitId) {
    let start = k1.timing.raw();
    let Some(_unit) = get_compiled_unit(&k1.ir, unit_id) else {
        return;
    };

    let mut visit_stack = std::mem::take(&mut k1.ir.opt_buf_stack);
    visit_stack.push(OptVisit::Enter(unit_id));
    let mut order: Vec<IrUnitId> = std::mem::take(&mut k1.ir.opt_buf_order);
    let mut visited = std::mem::take(&mut k1.ir.opt_buf_visited);
    let mut callees = std::mem::take(&mut k1.ir.opt_buf_callees);

    while let Some(visit) = visit_stack.pop() {
        match visit {
            OptVisit::Enter(unit_id) => {
                if !visited.insert(unit_id) {
                    continue;
                }
                visit_stack.push(OptVisit::Leave(unit_id)); // come back after children

                callees.clear();
                collect_direct_unoptimized_callees(k1, &mut callees, unit_id);
                for callee_id in &callees {
                    visit_stack.push(OptVisit::Enter(IrUnitId::Function(*callee_id)));
                }
            }
            OptVisit::Leave(unit_id) => {
                order.push(unit_id);
            }
        }
    }

    for unit_id in order.iter() {
        inline_calls_in_unit(k1, *unit_id)
    }

    {
        visit_stack.clear();
        order.clear();
        visited.clear();
        callees.clear();
        k1.ir.opt_buf_stack = visit_stack;
        k1.ir.opt_buf_order = order;
        k1.ir.opt_buf_visited = visited;
        k1.ir.opt_buf_callees = callees;
    }

    cfg_compute_unit(&mut k1.ir, unit_id);
    cfg_simplify(k1, unit_id);

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_iropt_nanos += elapsed as i64;
}

fn collect_direct_unoptimized_callees(
    k1: &TypedProgram,
    callees: &mut Vec<FunctionId>,
    unit_id: IrUnitId,
) {
    let unit = get_compiled_unit(&k1.ir, unit_id).unwrap();
    for block in k1.ir.mem.dlist_iter(unit.blocks) {
        for inst_id in k1.ir.mem.dlist_iter(block.instrs) {
            let inst = k1.ir.instrs.get(*inst_id);
            if let Inst::Call { call_id } = inst {
                // Inline calls
                let call = k1.ir.calls.get(*call_id);
                match call.callee {
                    IrCallee::Direct(function_id) => {
                        let callee_unit =
                            get_compiled_unit(&k1.ir, IrUnitId::Function(function_id)).unwrap();
                        if !callee_unit.inline_done {
                            callees.push(function_id)
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

fn inline_calls_in_unit(k1: &mut TypedProgram, unit_id: IrUnitId) {
    debug!("Inlining calls in {}", unit_name_to_string(k1, unit_id));
    while do_pass(k1, unit_id) {}
    let blocks = get_compiled_unit(&k1.ir, unit_id).unwrap().blocks;
    let inst_count = k1
        .ir
        .mem
        .dlist_iter(blocks)
        .map(|block| k1.ir.mem.dlist_compute_len(block.instrs) as u32)
        .sum();
    cfg_compute_unit(&mut k1.ir, unit_id);
    let unit = get_compiled_unit_mut(&mut k1.ir, unit_id).unwrap();
    unit.inline_done = true;
    unit.inst_count = inst_count;

    fn do_pass(k1: &mut TypedProgram, unit_id: IrUnitId) -> bool {
        let self_unit = get_compiled_unit(&k1.ir, unit_id).unwrap();
        for block in k1.ir.mem.dlist_iter(self_unit.blocks) {
            for (inst_node_handle, inst_node) in k1.ir.mem.dlist_iter_handles(block.instrs) {
                let inst_id = inst_node.data;
                let inst = k1.ir.instrs.get(inst_id);
                if let Inst::Call { call_id } = inst {
                    // Inline calls
                    let call = *k1.ir.calls.get(*call_id);
                    match call.callee {
                        IrCallee::Direct(function_id) => {
                            // FIXME: inlining This only prevents direct recursion, corecursive
                            // units still cause us to fail. We need to detect these cycles and
                            // avoid inlining them
                            if unit_id == IrUnitId::Function(function_id) {
                                continue;
                            }
                            if k1.functions.get(function_id).is_recursive {
                                continue;
                            }
                            let callee_unit =
                                get_compiled_unit(&k1.ir, IrUnitId::Function(function_id)).unwrap();

                            if callee_unit.inst_count < 20 {
                                inline_call(k1, unit_id, inst_node_handle, call);
                                return true;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        false
    }

    fn inline_call(
        k1: &mut TypedProgram,
        self_unit_id: IrUnitId,
        call_inst_node: IrHandle<InstNode>,
        call: IrCall,
    ) {
        let call_inst = *k1.ir.mem.get(call_inst_node);
        let call_inst_id = call_inst.data;
        // eprintln!("Inlining call i{}", call_inst_id.as_u32());
        let self_unit = get_compiled_unit(&k1.ir, self_unit_id).unwrap();
        let IrCallee::Direct(callee_fn_id) = call.callee else { panic!() };
        let call_span = *k1.ir.sources.get(call_inst_id);
        let callee_unit = k1.ir.functions.get(callee_fn_id).unwrap();

        let entry_span = match self_unit_id {
            IrUnitId::Expr(e) => k1.exprs.get_span(e),
            IrUnitId::Function(id) => k1.get_function_span(id),
        };
        let mut b = Builder {
            k1,
            blocks: self_unit.blocks,
            fn_type: self_unit.fn_type,
            returned_alloca: None,
            last_alloca_index: self_unit.last_alloca_index,
            cur_block: Handle::nil(),
            cur_span: call_span,
            entry_span,
        };

        let call_block_handle = b.get_instr_block(call_inst_id);
        let mut call_block_node_ref = b.k1.ir.mem.get_raw_ref(call_block_handle);
        let self_entry_block = b.blocks.first;

        let mut self_rewrites = std::mem::take(&mut b.k1.ir.opt_buf_inline_self_rewrites);

        let call_args = b.k1.ir.mem.getn(call.args);
        let mut inlined_rewrites = std::mem::take(&mut b.k1.ir.opt_buf_inline_inlined_rewrites);
        inlined_rewrites.fn_params = Some(call_args);

        // Remove the call
        let call_next = b.k1.ir.mem.get(call_inst_node).next;
        b.k1.ir.mem.dlist_remove(&mut call_block_node_ref.data.instrs, call_inst_node);
        let call_next = if callee_unit.fn_type.diverges {
            // There will be an 'unreachable' here, remove it too
            let next_next = b.k1.ir.mem.get(call_next).next;
            b.k1.ir.mem.dlist_remove(&mut call_block_node_ref.data.instrs, call_next);
            next_next
        } else {
            call_next
        };
        debug!("removed call: {}", inst_to_string(b.k1, call_inst_id));

        // None when the call is the last instruction in the block
        let call_post_block: Option<BlockId> = match call_next.is_nil() {
            false => Some(b.split_block_at_inst(call_block_handle, call_next)),
            true => None,
        };
        enum InlinedReturnInfo {
            Empty,
            AggInStorage(Value),
            ScalarInPhi(InstId, List<PhiCase, ProgramIr>),
        }
        let ret_layout = b.k1.types.get_pt_layout(call.ret_type);
        let mut return_info = match call.dst {
            None => {
                match call.ret_type.as_enum() {
                    PhysicalTypeEnum::Scalar(_) => {
                        let incomings = b.k1.ir.mem.new_list(0);
                        // There's always a post block that contains at least a ret if the return
                        // type is not 'void' or 'never' (its a scalar here)
                        b.goto_block(call_post_block.unwrap());
                        // phi instead
                        let phi = b.push_inst_front(
                            Inst::Phi { t: call.ret_type, incomings: MSlice::empty() },
                            "inlined scalar return",
                        );
                        self_rewrites.values.insert(call_inst_id, Value::Inst(phi));
                        InlinedReturnInfo::ScalarInPhi(phi, incomings)
                    }
                    PhysicalTypeEnum::Agg(_) => {
                        let dst_alloca = b.k1.ir.add_inst(
                            Inst::Alloca {
                                t: call.ret_type,
                                vm_layout: ret_layout,
                                returned: false,
                            },
                            "inline ret".into(),
                            IrDebugInfo::default(),
                            call_span,
                        );
                        let self_entry_instrs =
                            &mut b.k1.ir.mem.get_raw_ref(self_entry_block).as_mut().data.instrs;
                        b.k1.ir.mem.dlist_insert(self_entry_instrs, 0, dst_alloca);
                        self_rewrites.values.insert(call_inst_id, Value::Inst(dst_alloca));
                        InlinedReturnInfo::AggInStorage(dst_alloca.as_value())
                    }
                    PhysicalTypeEnum::Empty => InlinedReturnInfo::Empty,
                }
            }
            Some(dst) => InlinedReturnInfo::AggInStorage(dst),
        };

        if let Some(call_post_block) = call_post_block {
            self_rewrites.block_exits.insert(call_block_handle, call_post_block);
        }
        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles(b.blocks) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut self_rewrites);
        }

        b.cur_block = call_block_handle;
        // eprintln!("post split\n{}", blocks_to_string(b.k1, b.blocks, false));

        let mut inlined_first: BlockId = Handle::nil();
        let mut inlined_last: BlockId = Handle::nil();
        for (index, (callee_block_id, callee_block)) in
            b.k1.ir.mem.dlist_iter_handles(callee_unit.blocks).enumerate()
        {
            let inlined_block = b.k1.ir.mem.dlist_insert_after(
                &mut b.blocks,
                b.cur_block,
                Block {
                    name: callee_block.data.name,
                    instrs: Dlist::empty(),
                    preds: Dlist::empty(),
                    succs: Dlist::empty(),
                },
            );
            if inlined_first.is_nil() {
                inlined_first = inlined_block;
            }
            inlined_last = inlined_block;

            inlined_rewrites.block_enters.insert(callee_block_id, inlined_block);
            inlined_rewrites.block_exits.insert(callee_block_id, inlined_block);
            if index == 0 {
                b.push_jump(inlined_block, "enter inlined code");
            }
            b.cur_block = inlined_block;
            for callee_inst in b.k1.ir.mem.dlist_iter(callee_block.data.instrs) {
                let mut inst = *b.k1.ir.instrs.get(*callee_inst);
                let mut include = true;
                // We delay rewriting most instructions until we know about all the blocks; just
                // makes it easier to have on rewrite routine for everything, and not to worry about
                // using an incomplete rewrite mappings set
                match &mut inst {
                    Inst::Alloca { returned, .. } => {
                        *returned = false;
                    }
                    Inst::Ret { v, .. } => {
                        // Store to dst_alloca, and jmp to after block
                        match &mut return_info {
                            InlinedReturnInfo::Empty => {}
                            InlinedReturnInfo::AggInStorage(dst_storage) => {
                                store_value(
                                    &mut b,
                                    call.ret_type,
                                    *dst_storage,
                                    *v,
                                    "inlined agg ret",
                                );
                            }
                            InlinedReturnInfo::ScalarInPhi(_, cases) => cases.push_grow(
                                &mut b.k1.ir.mem,
                                PhiCase { from: inlined_block, value: *v },
                            ),
                        }
                        if let Some(call_post_block) = call_post_block {
                            b.push_jump(call_post_block, "exit inlined code");
                        }
                        include = false;
                    }
                    _ => {}
                }
                if include {
                    let comment = *b.k1.ir.comments.get(*callee_inst);
                    let new_inst = if let Inst::Alloca { t, .. } = inst {
                        b.push_alloca(t, comment)
                    } else {
                        b.push_inst(inst, comment)
                    };
                    inlined_rewrites.values.insert(*callee_inst, Value::Inst(new_inst));
                }
            }
        }
        if let InlinedReturnInfo::ScalarInPhi(phi, actual_incomings) = return_info {
            let mut inst = b.k1.ir.instrs.get_raw(phi);
            let Inst::Phi { incomings, .. } = inst.as_mut() else { panic!() };
            *incomings = b.k1.ir.mem.list_to_handle(actual_incomings);
            rewrite_instr(&mut b.k1.ir, &mut inlined_rewrites, &mut inst);
        }

        // eprintln!("pre rewrite\n{}", blocks_to_string(b.k1, b.blocks, false));
        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles_from(b.blocks, inlined_first) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut inlined_rewrites);
            if self_block == inlined_last {
                break;
            }
        }

        let unit = get_compiled_unit_mut(&mut b.k1.ir, self_unit_id).unwrap();
        unit.blocks = b.blocks;
        unit.cfg_valid = false;

        self_rewrites.clear();
        inlined_rewrites.clear();
        b.k1.ir.opt_buf_inline_self_rewrites = self_rewrites;
        b.k1.ir.opt_buf_inline_inlined_rewrites = inlined_rewrites;

        // eprintln!("post inline\n{}", unit_to_string(b.k1, self_unit_id, false));
        // if let Err(e) = validate_unit(b.k1, self_unit_id) {
        //     k1.report(e)
        // };
    }
}

fn rewrite_in_block(ir: &mut ProgramIr, block: BlockId, mappings: &mut RewriteMappings) {
    let block = ir.mem.get_raw_ref(block);
    for callee_inst_id in ir.mem.dlist_iter(block.data.instrs) {
        let mut inst = ir.instrs.get_raw(*callee_inst_id);
        rewrite_instr(ir, mappings, &mut inst)
    }
}

#[derive(Default)]
pub struct RewriteMappings {
    values: FxHashMap<InstId, Value>,
    fn_params: Option<&'static [Value]>,
    block_enters: FxHashMap<BlockId, BlockId>,
    block_exits: FxHashMap<BlockId, BlockId>,
    block_deletes: FxHashSet<BlockId>,
}

impl RewriteMappings {
    pub fn clear(&mut self) {
        self.values.clear();
        self.fn_params = None;
        self.block_enters.clear();
        self.block_exits.clear();
        self.block_deletes.clear();
    }

    #[allow(unused)]
    fn eprint(&self) {
        eprintln!(
            "inst map: {}",
            self.values.iter().map(|(old, new)| format!("{} -> {}", old, new)).join(", ")
        );
        eprintln!(
            "block enter: {}",
            self.block_enters
                .iter()
                .map(|(old, new)| format!("b{} -> b{}", old.raw_index(), new.raw_index()))
                .join(", ")
        );
        eprintln!(
            "block exit: {}",
            self.block_exits
                .iter()
                .map(|(old, new)| format!("b{} -> b{}", old.raw_index(), new.raw_index()))
                .join(", ")
        );
    }
}

fn rewrite_instr(ir: &mut ProgramIr, mappings: &mut RewriteMappings, inst: &mut Inst) {
    match inst {
        Inst::Alloca { .. } => {}
        Inst::Store { value, dst, .. } => {
            rewrite_value(mappings, dst);
            rewrite_value(mappings, value);
        }
        Inst::Load { src, .. } => {
            rewrite_value(mappings, src);
        }
        Inst::Copy { dst, src, .. } => {
            rewrite_value(mappings, dst);
            rewrite_value(mappings, src);
        }
        Inst::StructOffset { base, .. } => {
            rewrite_value(mappings, base);
        }
        Inst::ArrayOffset { base, element_index, .. } => {
            rewrite_value(mappings, base);
            rewrite_value(mappings, element_index);
        }
        Inst::Call { call_id } => {
            if mappings.fn_params.is_some() || !mappings.values.is_empty() {
                let call = ir.calls.get(*call_id);
                let mut new_args = ir.mem.getn_sv8(call.args);
                let mut changed = false;
                for arg in &mut new_args {
                    if rewrite_value(mappings, arg) {
                        changed = true;
                    }
                }
                let mut new_callee = call.callee;
                if let IrCallee::Indirect(_, value) = &mut new_callee {
                    if rewrite_value(mappings, value) {
                        changed = true
                    }
                };
                let mut new_dst = call.dst;
                match &mut new_dst {
                    None => {}
                    Some(v) => {
                        if rewrite_value(mappings, v) {
                            changed = true
                        }
                    }
                };
                if changed {
                    let new_call_id = ir.calls.add(IrCall {
                        ret_type: call.ret_type,
                        callee: new_callee,
                        args: ir.mem.pushn(&new_args),
                        dst: new_dst,
                    });
                    *call_id = new_call_id;
                }
            }
        }
        Inst::Jump(block_id) => {
            if let Some(new) = mappings.block_enters.get(block_id) {
                *block_id = *new;
            }
        }
        Inst::JumpIf { cond, cons, alt } => {
            rewrite_value(mappings, cond);
            if let Some(new) = mappings.block_enters.get(cons) {
                *cons = *new;
            }
            if let Some(new) = mappings.block_enters.get(alt) {
                *alt = *new;
            }
        }
        Inst::Unreachable => {}
        Inst::Phi { incomings, .. } => {
            let mut new_incomings = ir.mem.new_list(incomings.len());
            for phi_case in ir.mem.getn(*incomings) {
                let mut new_case = *phi_case;
                rewrite_value(mappings, &mut new_case.value);
                if let Some(new) = mappings.block_exits.get(&new_case.from) {
                    new_case.from = *new;
                }

                if mappings.block_deletes.contains(&new_case.from) {
                    // eprintln!("deleting phi inc {}", phi_case.from.raw_index());
                } else {
                    new_incomings.push(new_case);
                }
            }

            // Note: this can produce degenerate phis (1 incoming)
            //       but that's not considered invalid; the problem comes once
            //       we inline those!
            *incomings = ir.mem.list_to_handle(new_incomings);
        }
        Inst::Ret { v, .. } => {
            // Store to dst_alloca, and jmp to after block
            rewrite_value(mappings, v);
        }
        Inst::BoolNegate { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::BitNot { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::BitCast { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntAdd { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::Data(_) => {}
        Inst::IntTrunc { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntExtU { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntExtS { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::FloatTrunc { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::FloatExt { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::Float32ToIntUnsigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::Float64ToIntUnsigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::Float32ToIntSigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::Float64ToIntSigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntToFloatUnsigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntToFloatSigned { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::PtrToWord { v } => {
            rewrite_value(mappings, v);
        }
        Inst::WordToPtr { v } => {
            rewrite_value(mappings, v);
        }
        Inst::IntSub { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntMul { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntDivUnsigned { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntDivSigned { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntRemUnsigned { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntRemSigned { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::IntCmp { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatAdd { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatSub { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatMul { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatDiv { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatRem { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::FloatCmp { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitAnd { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitOr { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitXor { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitShiftLeft { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitUnsignedShiftRight { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BitSignedShiftRight { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::BakeStaticValue { value, .. } => {
            rewrite_value(mappings, value);
        }
    }
}

fn rewrite_value(mappings: &mut RewriteMappings, value: &mut Value) -> bool {
    match *value {
        Value::Inst(inst_id) => {
            if let Some(new) = mappings.values.get(&inst_id) {
                *value = *new;
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

pub fn cfg_compute_unit(ir: &mut ProgramIr, unit_id: IrUnitId) {
    let unit = get_compiled_unit(ir, unit_id).unwrap();
    cfg_compute(ir, unit.blocks);
    let unit = get_compiled_unit_mut(ir, unit_id).unwrap();
    unit.cfg_valid = true;
}

pub fn cfg_compute(ir: &mut ProgramIr, blocks: IrList<Block>) {
    if blocks.is_empty() {
        return;
    }
    for (_, mut node) in ir.mem.dlist_iter_handles(blocks) {
        let preds = &mut node.data.preds;
        *preds = Dlist::empty();
        let succs = &mut node.data.succs;
        *succs = Dlist::empty();
    }
    let mut work_stack = std::mem::take(&mut ir.opt_buf_cfg_compute_work_stack);
    work_stack.push(blocks.first);
    let mut visited: FxHashSet<_> = std::mem::take(&mut ir.opt_buf_cfg_compute_visited);
    while let Some(block_id) = work_stack.pop() {
        if !visited.insert(block_id) {
            continue;
        }
        let mut block_node = ir.mem.get_raw_ref(block_id);
        let instrs = block_node.data.instrs;
        let succs = &mut block_node.data.succs;
        let terminator_inst_id = ir.mem.get(instrs.last).data;
        match ir.instrs.get(terminator_inst_id) {
            Inst::Jump(block) => {
                ir.mem.dlist_push(succs, *block);
                let mut dst_block = ir.mem.get_raw_ref(*block);
                ir.mem.dlist_push(&mut dst_block.data.preds, block_id);
                work_stack.push(*block);
            }
            Inst::JumpIf { cons, alt, .. } => {
                ir.mem.dlist_push(succs, *cons);
                let mut cons_block = ir.mem.get_raw_ref(*cons);
                ir.mem.dlist_push(&mut cons_block.data.preds, block_id);

                ir.mem.dlist_push(succs, *alt);
                let mut alt_block = ir.mem.get_raw_ref(*alt);
                ir.mem.dlist_push(&mut alt_block.data.preds, block_id);

                work_stack.push(*cons);
                work_stack.push(*alt);
            }
            _ => {}
        }
        // for (_inst_handle, inst_node) in ir.mem.dlist_iter_handles(instrs) {
        //     match ir.instrs.get(inst_node.data) {
        //         Inst::Jump(block) => {
        //             ir.mem.dlist_push(succs, *block);
        //             let mut dst_block = ir.mem.get_raw_ref(*block);
        //             ir.mem.dlist_push(&mut dst_block.data.preds, block_id);
        //             work_stack.push(*block);
        //         }
        //         Inst::JumpIf { cons, alt, .. } => {
        //             ir.mem.dlist_push(succs, *cons);
        //             let mut cons_block = ir.mem.get_raw_ref(*cons);
        //             ir.mem.dlist_push(&mut cons_block.data.preds, block_id);
        //
        //             ir.mem.dlist_push(succs, *alt);
        //             let mut alt_block = ir.mem.get_raw_ref(*alt);
        //             ir.mem.dlist_push(&mut alt_block.data.preds, block_id);
        //
        //             work_stack.push(*cons);
        //             work_stack.push(*alt);
        //         }
        //         _ => {}
        //         _ => {}
        //     }
        // }
    }
    visited.clear();
    ir.opt_buf_cfg_compute_work_stack = work_stack;
    ir.opt_buf_cfg_compute_visited = visited;
}

pub fn cfg_simplify(k1: &mut TypedProgram, unit_id: IrUnitId) {
    let unit = get_compiled_unit(&k1.ir, unit_id).unwrap();
    debug_assert!(unit.cfg_valid, "cfg is not computed");
    let mut blocks = unit.blocks;
    cfg_simplify_blocks(k1, &mut blocks);

    let unit = get_compiled_unit_mut(&mut k1.ir, unit_id).unwrap();
    unit.blocks = blocks;
    unit.cfg_valid = true;

    if let Err(e) = validate_unit(k1, unit_id) {
        k1.report(e)
    }
}

fn cfg_simplify_blocks(k1: &mut TypedProgram, blocks: &mut IrList<Block>) {
    // eprintln!("cfg_simplify on {}", blocks_to_string(k1, *blocks, true, false));
    while do_pass(k1, blocks) {}

    fn do_pass(k1: &mut TypedProgram, blocks: &mut IrList<Block>) -> bool {
        debug!("simplify pass");
        let ir = &mut k1.ir;
        let mut noop = true;
        let mut remove = k1.tmp.new_list(0);
        let mut rewrites = std::mem::take(&mut ir.opt_buf_cfg_simpl_rewrites);
        for (block_id, node) in ir.mem.dlist_iter_handles(*blocks) {
            let is_entry = node.prev.is_nil();

            // Dead case
            if !is_entry && node.data.preds.is_empty() {
                noop = false;
                debug!("dead b{}", block_id.raw_index());
                remove.push_grow(&mut k1.tmp, block_id);
                break;
            }

            // Trampoline case
            if let Some(pred) = ir.mem.dlist_get_singleton(node.data.preds) {
                if let Some(succ) = ir.mem.dlist_get_singleton(node.data.succs) {
                    let succ_has_phi = ir
                        .mem
                        .dlist_iter(ir.mem.get(succ).data.instrs)
                        .any(|inst_id| matches!(ir.instrs.get(*inst_id), Inst::Phi { .. }));
                    if !succ_has_phi {
                        if let Some(inst_id) = ir.mem.dlist_get_singleton(node.data.instrs) {
                            if let Inst::Jump(jmp_block_id) = ir.instrs.get(inst_id) {
                                debug_assert!(*jmp_block_id == succ);
                                debug!("trampoline b{}", block_id.raw_index());
                                remove.push_grow(&mut k1.tmp, block_id);

                                // Jumps to me become jumps to my only succ
                                rewrites.block_enters.insert(block_id, succ);
                                // Incomings from me become incomings from my pred
                                rewrites.block_exits.insert(block_id, pred);

                                noop = false;
                                break;
                            };
                        }
                    }
                }
            }

            // Merge successor into self case
            if node.data.succs.is_singleton() {
                let succ_node_cfg = ir.mem.get_raw_ref(node.data.succs.first);
                let succ_block_id: BlockId = succ_node_cfg.data;
                let succ_node = ir.mem.get_raw_ref(succ_block_id);

                if succ_node.data.preds.is_singleton() {
                    let pred_node_cfg = ir.mem.get(succ_node.data.preds.first);
                    if pred_node_cfg.data == block_id {
                        debug!(
                            "merge b{} into b{}",
                            succ_block_id.raw_index(),
                            block_id.raw_index()
                        );
                        // Merge block_id with its successor, because we dominate it and we have no
                        // other successors
                        rewrites.block_exits.insert(succ_block_id, block_id);
                        // Remove the jump from block_id to succ
                        let mut block_node = ir.mem.get_raw_ref(block_id);
                        let popped_jump =
                            ir.mem.dlist_pop_last(&mut block_node.data.instrs).unwrap();
                        debug_assert!(matches!(ir.instrs.get(popped_jump.data), Inst::Jump { .. }));
                        // Move all instructions from succ to block_id
                        for (_inst_handle, inst_node) in
                            ir.mem.dlist_iter_handles(succ_node.data.instrs)
                        {
                            if let Inst::Phi { incomings, .. } = ir.instrs.get(inst_node.data) {
                                if incomings.len() == 1 {
                                    let only_case = ir.mem.get_nth(*incomings, 0);
                                    rewrites.values.insert(inst_node.data, only_case.value);
                                }
                            }
                            ir.mem.dlist_push(&mut block_node.data.instrs, inst_node.data);
                        }
                        noop = false;
                        break;
                    }
                }
            }
        }
        for block_id in remove.as_slice() {
            debug!("removing b{}", block_id.raw_index());
            rewrites.block_deletes.insert(*block_id);
            ir.mem.dlist_remove(blocks, *block_id);
        }

        if !noop {
            //rewrites.eprint();
            for (block_id, _) in ir.mem.dlist_iter_handles(*blocks) {
                rewrite_in_block(ir, block_id, &mut rewrites)
            }
            cfg_compute(ir, *blocks);
        }
        rewrites.clear();
        ir.opt_buf_cfg_simpl_rewrites = rewrites;
        !noop
    }
    debug!("cfg_simplify end\n{}", blocks_to_string(k1, *blocks, true, false));
}
