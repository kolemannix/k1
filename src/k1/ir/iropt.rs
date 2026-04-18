use crate::kmem::Mem;

use super::*;

pub fn optimize_unit(k1: &mut TypedProgram, unit_id: IrUnitId) {
    let Some(_unit) = get_compiled_unit(&k1.ir, unit_id) else {
        return;
    };

    enum Visit {
        Enter(IrUnitId),
        Leave(IrUnitId),
    }
    // nocommit reuse them buffers
    let mut stack = vec![Visit::Enter(unit_id)];
    let mut order: Vec<IrUnitId> = Vec::new();
    let mut visited = FxHashSet::new();

    let mut callee_buffer = Vec::new();

    while let Some(visit) = stack.pop() {
        match visit {
            Visit::Enter(unit_id) => {
                if !visited.insert(unit_id) {
                    continue;
                }
                stack.push(Visit::Leave(unit_id)); // come back after children

                callee_buffer.clear();
                collect_direct_unoptimized_callees(k1, &mut callee_buffer, unit_id);
                for callee_id in &callee_buffer {
                    stack.push(Visit::Enter(IrUnitId::Function(*callee_id)));
                }
            }
            Visit::Leave(unit_id) => {
                order.push(unit_id); // true post-order
            }
        }
    }

    for unit_id in order.iter() {
        eprintln!("Optimizing {}", unit_name_to_string(k1, *unit_id));
        inline_calls_in_unit(k1, *unit_id)
    }
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
    let name = unit_name_to_string(k1, unit_id);
    eprintln!("Inlining calls in {}", name);
    if name != "a/add" {
        return;
    }
    while do_pass(k1, unit_id) {}

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
                            let callee_unit =
                                get_compiled_unit(&k1.ir, IrUnitId::Function(function_id)).unwrap();

                            if callee_unit.inst_count < 20 {
                                inline_call(k1, unit_id, inst_node_handle, call);
                                // We want to just start the iteration completely over
                                // at this point, since we've mutated the unit, and stop when
                                // we find nothing to inline
                                return false;
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
        eprintln!("{}", unit_to_string(k1, self_unit_id, false));
        let call_inst = *k1.ir.mem.get(call_inst_node);
        let call_inst_id = call_inst.data;
        eprintln!("Inlining call i{}", call_inst_id.as_u32());
        let self_unit = get_compiled_unit(&k1.ir, self_unit_id).unwrap();
        let IrCallee::Direct(callee_fn_id) = call.callee else { panic!() };
        let call_span = *k1.ir.sources.get(call_inst_id);
        let callee_unit = k1.ir.functions.get(callee_fn_id).unwrap();

        let mut b = Builder {
            k1,
            blocks: self_unit.blocks,
            fn_type: self_unit.fn_type,
            last_alloca_index: None,
            cur_block: Handle::nil(),
            cur_span: call_span,
            entry_span: SpanId::NONE, // nocommit entry_span for inlined code; codegen skips dbg
                                      // info when span is SpanId::NONE
        };

        let call_block_handle = b.get_instr_block(call_inst_id);
        let mut call_block_node_ref = b.k1.ir.mem.get_raw_ref(call_block_handle);
        let self_entry_block = b.blocks.first;

        // Allocate the destination slot
        let ret_layout = b.k1.types.get_pt_layout(call.ret_type);
        let dst_alloca = b.k1.ir.add_inst(
            Inst::Alloca { t: call.ret_type, vm_layout: ret_layout, returned: false },
            "inline ret".into(),
            IrDebugInfo::default(),
            call_span,
        );
        let call_args = b.k1.ir.mem.getn(call.args);
        let mut rewrite_map = RewriteMappings {
            instrs: FxHashMap::new(),
            block_enters: FxHashMap::new(),
            block_exits: FxHashMap::new(),
            fn_params: Some(call_args),
        };
        rewrite_map.instrs.insert(call_inst_id, dst_alloca);
        let self_entry_instrs = &mut b.k1.ir.mem.get_raw_ref(self_entry_block).as_mut().data.instrs;
        b.k1.ir.mem.dlist_insert(self_entry_instrs, 0, dst_alloca);

        // Remove the call
        let call_next = b.k1.ir.mem.get(call_inst_node).next;
        b.k1.ir.mem.dlist_remove(&mut call_block_node_ref.data.instrs, call_inst_node);
        let call_next = if callee_unit.fn_type.diverges {
            // There will be an 'unreachable' here, remove it too
            let next_next = b.k1.ir.mem.get(call_next).next;
            eprintln!(
                "removing unreachable: {}",
                inst_to_string(b.k1, b.k1.ir.mem.get(next_next).data)
            );
            b.k1.ir.mem.dlist_remove(&mut call_block_node_ref.data.instrs, call_next);
            next_next
        } else {
            call_next
        };
        eprintln!("removed call: {}", inst_to_string(b.k1, call_inst_id));

        let call_post_block = b.split_block_at_inst(call_block_handle, call_next);
        // All phis that came from call_block will now come from call_post_block
        // So that's a rewrite we will be doing on the self blocks
        // Might as well go ahead and do that.
        let mut self_rewrites = RewriteMappings {
            instrs: FxHashMap::new(),
            block_enters: FxHashMap::new(),
            block_exits: FxHashMap::new(),
            fn_params: None,
        };
        self_rewrites.block_exits.insert(call_block_handle, call_post_block);
        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles(b.blocks) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut self_rewrites);
        }

        b.cur_block = call_block_handle;
        eprintln!("post split\n{}", blocks_to_string(b.k1, b.blocks, false));

        // Walk the inlined code, rewriting instructions, and hoisting allocas
        for (index, (callee_block_id, callee_block)) in
            b.k1.ir.mem.dlist_iter_handles(callee_unit.blocks).enumerate()
        {
            let inlined_block = b.k1.ir.mem.dlist_insert_after(
                &mut b.blocks,
                b.cur_block,
                Block { name: "inlined something todo", instrs: Dlist::empty() },
            );
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
                        store_value(
                            &mut b,
                            call.ret_type,
                            dst_alloca.as_value(),
                            *v,
                            "inlined ret",
                        );
                        b.push_jump(call_post_block, "inlined ret");

                        include = false;
                    }
                    _ => {}
                }
                if include {
                    let new_inst = if let Inst::Alloca { t, .. } = inst {
                        b.push_alloca(t, "inlined alloca")
                    } else {
                        b.push_inst(inst, "inlined")
                    };
                    rewrite_map.instrs.insert(*callee_inst, new_inst);
                }
            }
            rewrite_map.eprint();
        }

        for (self_block, _) in b.k1.ir.mem.dlist_iter_handles(b.blocks) {
            rewrite_in_block(&mut b.k1.ir, self_block, &mut self_rewrites);
        }

        get_compiled_unit_mut(&mut b.k1.ir, self_unit_id).unwrap().blocks = b.blocks;
        eprintln!("post inline\n{}", unit_to_string(b.k1, self_unit_id, false));
        if let Err(e) = validate_unit(b.k1, self_unit_id) {
            k1.report(e)
        };
        // - Replace old with new alloca ids
        // - Replace param values (p0, p1, ..) with the 'Value's of the corresponding param from the call
        // - Replace 'return' instructions with stores/copies (fulfills) to the destination slot
        // All parameter references must be replaced by the parameter values
        // - Downstream block references (phis or jumps) must be updated to be the final block
        // of the inlined function
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
        Inst::ArrayOffset { base, .. } => {
            rewrite_value(mappings, base);
        }
        Inst::Jump(block_id) => {
            if let Some(new) = mappings.block_enters.get(block_id) {
                *block_id = *new;
            }
        }
        Inst::JumpIf { cons, alt, .. } => {
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
        Inst::Call { call_id } => {
            let call = ir.calls.get(*call_id);
            let new_args = ir.mem.dupn(call.args);
            for arg in ir.mem.getn_mut(new_args) {
                rewrite_value(mappings, arg);
            }
            let mut new_callee = call.callee;
            if let IrCallee::Indirect(_, value) = &mut new_callee {
                eprintln!("rewriting call args");
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
        Inst::BitCast { v, .. } => {
            rewrite_value(mappings, v);
        }
        Inst::IntAdd { lhs, rhs, .. } => {
            rewrite_value(mappings, lhs);
            rewrite_value(mappings, rhs);
        }
        Inst::Ret { v, .. } => {
            // Store to dst_alloca, and jmp to after block
            rewrite_value(mappings, v);
        }
        _ => {}
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
                eprintln!("rewriting p{}", *index);
                *value = new_params[*index as usize];
            }
        }
        _ => {}
    };
}
