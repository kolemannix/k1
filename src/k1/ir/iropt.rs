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

    visit_stack.clear();
    order.clear();
    visited.clear();
    callees.clear();
    k1.ir.opt_buf_stack = visit_stack;
    k1.ir.opt_buf_order = order;
    k1.ir.opt_buf_visited = visited;
    k1.ir.opt_buf_callees = callees;

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
            last_alloca_index: None,
            cur_block: Handle::nil(),
            cur_span: call_span,
            entry_span,
        };

        let call_block_handle = b.get_instr_block(call_inst_id);
        let mut call_block_node_ref = b.k1.ir.mem.get_raw_ref(call_block_handle);
        let self_entry_block = b.blocks.first;

        // Allocate the destination slot
        let ret_layout = b.k1.types.get_pt_layout(call.ret_type);
        let mut self_rewrites = RewriteMappings {
            instrs: FxHashMap::new(),
            block_enters: FxHashMap::new(),
            block_exits: FxHashMap::new(),
            fn_params: None,
        };

        let call_args = b.k1.ir.mem.getn(call.args);
        let mut rewrite_map = RewriteMappings {
            instrs: FxHashMap::new(),
            block_enters: FxHashMap::new(),
            block_exits: FxHashMap::new(),
            fn_params: Some(call_args),
        };

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
        // eprintln!("removed call: {}", inst_to_string(b.k1, call_inst_id));

        // None when the call is the last instruction in the block
        let call_post_block: Option<BlockId> = match call_next.is_nil() {
            false => Some(b.split_block_at_inst(call_block_handle, call_next)),
            true => None,
        };
        let dst_storage = if call.ret_type.is_empty() {
            None
        } else {
            match call.dst {
                None => {
                    // nocommit: for inlined scalar returns, use a phi instead of an alloca
                    // match call.ret_type().as_enum() {
                    //
                    // }
                    let dst_alloca = b.k1.ir.add_inst(
                        Inst::Alloca { t: call.ret_type, vm_layout: ret_layout, returned: false },
                        "inline ret".into(),
                        IrDebugInfo::default(),
                        call_span,
                    );
                    let self_entry_instrs =
                        &mut b.k1.ir.mem.get_raw_ref(self_entry_block).as_mut().data.instrs;
                    b.k1.ir.mem.dlist_insert(self_entry_instrs, 0, dst_alloca);

                    let dst_inst = if call.ret_type.is_scalar() {
                        // There's always a post block that contains at least a ret if the return
                        // type is not 'void' or 'never' (its a scalar here)
                        b.goto_block(call_post_block.unwrap());
                        let loaded = b.push_inst_front(
                            Inst::Load {
                                t: call.ret_type.expect_scalar(),
                                src: dst_alloca.as_value(),
                            },
                            "load inlined dst value",
                        );
                        loaded
                    } else {
                        dst_alloca
                    };
                    self_rewrites.instrs.insert(call_inst_id, dst_inst);
                    Some(dst_alloca.as_value())
                }
                Some(dst) => Some(dst),
            }
        };

        if let Some(call_post_block) = call_post_block {
            self_rewrites.block_exits.insert(call_block_handle, call_post_block);
        }
        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles(b.blocks) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut self_rewrites);
        }

        b.cur_block = call_block_handle;
        // eprintln!("post split\n{}", blocks_to_string(b.k1, b.blocks, false));

        // Walk the inlined code, rewriting instructions, and hoisting allocas
        let mut inlined_first: BlockId = Handle::nil();
        let mut inlined_last: BlockId = Handle::nil();
        for (index, (callee_block_id, callee_block)) in
            b.k1.ir.mem.dlist_iter_handles(callee_unit.blocks).enumerate()
        {
            let inlined_block = b.k1.ir.mem.dlist_insert_after(
                &mut b.blocks,
                b.cur_block,
                Block { name: callee_block.data.name, instrs: Dlist::empty() },
            );
            if inlined_first.is_nil() {
                inlined_first = inlined_block;
            }
            inlined_last = inlined_block;

            rewrite_map.block_enters.insert(callee_block_id, inlined_block);
            rewrite_map.block_exits.insert(callee_block_id, inlined_block);
            if index == 0 {
                b.push_jump(inlined_block, "goto inlined");
            }
            b.cur_block = inlined_block;
            for callee_inst in b.k1.ir.mem.dlist_iter(callee_block.data.instrs) {
                let mut inst = *b.k1.ir.instrs.get(*callee_inst);
                let mut include = true;
                match &mut inst {
                    Inst::Alloca { returned, .. } => {
                        *returned = false;
                    }
                    Inst::Ret { v, .. } => {
                        // Store to dst_alloca, and jmp to after block
                        if let Some(dst_storage) = dst_storage {
                            store_value(&mut b, call.ret_type, dst_storage, *v, "inlined ret");
                        }
                        if let Some(call_post_block) = call_post_block {
                            b.push_jump(call_post_block, "inlined ret");
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
                    rewrite_map.instrs.insert(*callee_inst, new_inst);
                }
            }
            // rewrite_map.eprint();
        }

        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles_from(b.blocks, inlined_first) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut rewrite_map);
            if self_block == inlined_last {
                break;
            }
        }

        let unit = get_compiled_unit_mut(&mut b.k1.ir, self_unit_id).unwrap();
        unit.blocks = b.blocks;
        // eprintln!("post inline\n{}", unit_to_string(b.k1, self_unit_id, false));
        if let Err(e) = validate_unit(b.k1, self_unit_id) {
            k1.report(e)
        };
    }
}

fn rewrite_in_block(ir: &mut ProgramIr, block: BlockId, mappings: &mut RewriteMappings) {
    let block = ir.mem.get_raw_ref(block);
    for callee_inst_id in ir.mem.dlist_iter(block.data.instrs) {
        let mut inst = ir.instrs.get_raw(*callee_inst_id);
        rewrite_instr(ir, mappings, &mut inst)
    }
}

struct RewriteMappings {
    instrs: FxHashMap<InstId, InstId>,
    block_enters: FxHashMap<BlockId, BlockId>,
    block_exits: FxHashMap<BlockId, BlockId>,
    fn_params: Option<&'static [Value]>,
}

impl RewriteMappings {
    #[allow(unused)]
    fn eprint(&self) {
        eprintln!(
            "inst map: {}",
            self.instrs
                .iter()
                .map(|(old, new)| format!("i{} -> i{}", old.as_u32(), new.as_u32()))
                .join(", ")
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
        if let Some(params) = self.fn_params {
            eprintln!(
                "fn params: {}",
                params
                    .iter()
                    .enumerate()
                    .map(|(index, value)| format!("p{} -> {}", index, *value))
                    .join(", ")
            );
        }
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
            let call = ir.calls.get(*call_id);
            let new_args = ir.mem.dupn(call.args);
            for arg in ir.mem.getn_mut(new_args) {
                rewrite_value(mappings, arg);
            }
            let mut new_callee = call.callee;
            if let IrCallee::Indirect(_, value) = &mut new_callee {
                rewrite_value(mappings, value);
            };
            let mut new_dst = call.dst;
            match &mut new_dst {
                None => {}
                Some(v) => rewrite_value(mappings, v),
            };
            let new_call_id = ir.calls.add(IrCall {
                ret_type: call.ret_type,
                callee: new_callee,
                args: new_args,
                dst: new_dst,
            });
            *call_id = new_call_id;
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
        Inst::CameFrom { incomings, .. } => {
            let new_incomings = ir.mem.dupn(*incomings);
            for inc in ir.mem.getn_mut(new_incomings) {
                rewrite_value(mappings, &mut inc.value);
                if let Some(new) = mappings.block_exits.get(&inc.from) {
                    inc.from = *new;
                }
            }
            *incomings = new_incomings;
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

fn rewrite_value(mappings: &mut RewriteMappings, value: &mut Value) {
    match value {
        Value::Inst(inst) => {
            if let Some(new) = mappings.instrs.get(inst) {
                *inst = *new;
            }
        }
        Value::FnParam { index, .. } => {
            if let Some(new_params) = mappings.fn_params {
                *value = new_params[*index as usize];
            }
        }
        _ => {}
    };
}
