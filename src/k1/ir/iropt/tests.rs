use super::*;

fn block(ir: &mut ProgramIr, blocks: &mut IrList<Block>) -> BlockId {
    ir.mem.dlist_push(blocks, Block::empty(BlockSourceKind::Entry))
}

fn inst(ir: &mut ProgramIr, block: BlockId, inst: Inst) -> InstId {
    let id = ir.instrs.add(inst);
    let mut block = ir.mem.get_raw_ref(block);
    ir.mem.dlist_push(&mut block.data.instrs, id);
    id
}

fn edges(ir: &ProgramIr, edges: IrList<BlockId>) -> Vec<BlockId> {
    ir.mem.dlist_assert_valid(edges);
    ir.mem.dlist_iter(edges).map(|edge| *edge).collect()
}

#[test]
fn cfg_recomputations_reuse_storage_across_reachability_changes() {
    let mut ir = ProgramIr::make();
    let mut blocks = Dlist::empty();
    let entry = block(&mut ir, &mut blocks);
    let loop_block = block(&mut ir, &mut blocks);
    let exit = block(&mut ir, &mut blocks);
    let dead = block(&mut ir, &mut blocks);
    let cases = ir.mem.pushn(&[
        SwitchCase { value: 0, target: loop_block },
        SwitchCase { value: 1, target: loop_block },
    ]);
    let switch = Inst::Switch { value: Value::byte(0), width: 8, cases, default: exit };
    let entry_inst = inst(&mut ir, entry, switch);
    inst(
        &mut ir,
        loop_block,
        Inst::JumpIf { cond: Value::zero(ScalarType::Bool), cons: entry, alt: exit },
    );
    inst(&mut ir, exit, Inst::Ret { v: Value::Empty, agg: false });
    inst(&mut ir, dead, Inst::Jump(exit));
    cfg_compute(&mut ir, blocks);
    let used = ir.mem.bytes_used();

    for _ in 0..32 {
        *ir.instrs.get_mut(entry_inst) = switch;
        cfg_compute(&mut ir, blocks);
        assert_eq!(edges(&ir, ir.mem.get(entry).data.succs), [loop_block, loop_block, exit]);
        assert_eq!(edges(&ir, ir.mem.get(loop_block).data.preds), [entry, entry]);
        assert_eq!(edges(&ir, ir.mem.get(entry).data.preds), [loop_block]);
        assert_eq!(edges(&ir, ir.mem.get(exit).data.preds), [entry, loop_block]);
        assert!(edges(&ir, ir.mem.get(dead).data.succs).is_empty());

        *ir.instrs.get_mut(entry_inst) = Inst::Ret { v: Value::Empty, agg: false };
        cfg_compute(&mut ir, blocks);
        for block in ir.mem.dlist_iter(blocks) {
            assert!(edges(&ir, block.preds).is_empty());
            assert!(edges(&ir, block.succs).is_empty());
        }
        assert_eq!(ir.mem.bytes_used(), used);
    }
}

#[test]
fn cloned_phi_and_switch_payloads_are_owned_and_edits_do_not_allocate() {
    let mut ir = ProgramIr::make();
    let mut blocks = Dlist::empty();
    let from = block(&mut ir, &mut blocks);
    let to = block(&mut ir, &mut blocks);
    let dead = block(&mut ir, &mut blocks);
    let source = block(&mut ir, &mut blocks);
    let first = block(&mut ir, &mut blocks);
    let second = block(&mut ir, &mut blocks);
    let t = PhysicalType::scalar(ScalarType::U8);
    let incomings = ir.mem.pushn(&[
        PhiCase { from: dead, value: Value::byte(1) },
        PhiCase { from, value: Value::FnParam { t, index: 0 } },
        PhiCase { from: dead, value: Value::byte(2) },
        PhiCase { from, value: Value::byte(3) },
    ]);
    let original = inst(&mut ir, source, Inst::Phi { t, incomings });
    let mut clones = Vec::new();
    for target in [first, first, second] {
        let cloned = clone_inline_inst(&mut ir, original);
        clones.push(inst(&mut ir, target, cloned));
    }
    let cases = ir.mem.pushn(&[SwitchCase { value: 0, target: from }]);
    let switch = inst(
        &mut ir,
        source,
        Inst::Switch { value: Value::byte(0), width: 8, cases, default: dead },
    );
    let cloned = clone_inline_inst(&mut ir, switch);
    let cloned_switch = inst(&mut ir, first, cloned);
    let mut mappings = RewriteMappings::default();
    const ARGS: &[Value] = &[Value::byte(9)];
    mappings.fn_params = Some(ARGS);
    mappings.block_enters.insert(from, to);
    mappings.block_exits.insert(from, to);
    let used = ir.mem.bytes_used();

    for _ in 0..32 {
        rewrite_in_block(&mut ir, first, &mut mappings, &[]);
        rewrite_phi_incoming(&mut ir, first, to, from);
        rewrite_phi_incoming(&mut ir, first, from, to);
        remove_phi_incomings(&mut ir, first, &[dead]);
        assert_eq!(ir.mem.bytes_used(), used);
    }
    for id in &clones[..2] {
        let Inst::Phi { incomings: edited, .. } = *ir.instrs.get(*id) else { panic!() };
        let actual = ir.mem.getn(edited);
        assert_eq!(actual.len(), 2);
        assert_eq!(actual[0].from, to);
        assert_eq!(actual[1].from, to);
        assert!(actual[0].value == Value::byte(9));
        assert!(actual[1].value == Value::byte(3));
    }
    let Inst::Phi { incomings: untouched, .. } = *ir.instrs.get(clones[2]) else { panic!() };
    for payload in [incomings, untouched] {
        let actual = ir.mem.getn(payload);
        assert_eq!(actual.len(), 4);
        assert_eq!(actual[1].from, from);
        assert!(actual[1].value == Value::FnParam { t, index: 0 });
    }
    let Inst::Switch { cases: edited, .. } = *ir.instrs.get(cloned_switch) else { panic!() };
    assert_eq!(ir.mem.getn(edited)[0].target, to);
    assert_eq!(ir.mem.getn(cases)[0].target, from);
    remove_phi_incomings(&mut ir, first, &[to]);
    remove_phi_incomings(&mut ir, first, &[to]);
    for id in &clones[..2] {
        let Inst::Phi { incomings, .. } = *ir.instrs.get(*id) else { panic!() };
        assert!(incomings.is_empty());
    }
    assert_eq!(ir.mem.bytes_used(), used);
}
