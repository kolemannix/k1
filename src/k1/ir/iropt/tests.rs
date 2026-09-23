use super::*;

fn inst(u: &mut UnitBuf, block: BlockId, inst: Inst) -> InstId {
    let id = u.new_inst(inst, SpanId::NONE, IrComment::None);
    u.push_inst(block, id);
    id
}

fn insts(u: &UnitBuf, block: BlockId) -> Vec<InstId> {
    let mut out = Vec::new();
    for id in u.view().block_insts(block) {
        out.push(id);
    }
    out
}

fn iid(n: u32) -> InstId {
    InstId::from_u32(n).unwrap()
}

fn bid(n: u32) -> BlockId {
    BlockId::from_u32(n).unwrap()
}

#[test]
fn cfg_preds_follow_terminators_with_duplicate_edges() {
    let mut u = UnitBuf::default();
    let entry = u.add_block(BlockSourceKind::Entry);
    let loop_block = u.add_block(BlockSourceKind::LoopBody);
    let exit = u.add_block(BlockSourceKind::LoopEnd);
    let dead = u.add_block(BlockSourceKind::MatchFail);
    let cases = u.push_switch_cases(&[
        SwitchCase { value: 0, target: loop_block },
        SwitchCase { value: 1, target: loop_block },
    ]);
    let switch = Inst::Switch { value: Value::byte(0), width: 8, cases, default: exit };
    let entry_inst = inst(&mut u, entry, switch);
    inst(
        &mut u,
        loop_block,
        Inst::JumpIf { cond: Value::zero(ScalarType::Bool), cons: entry, alt: exit },
    );
    inst(&mut u, exit, Inst::Ret { v: Value::Empty, agg: false });
    inst(&mut u, dead, Inst::Jump(exit));

    for _ in 0..4 {
        *u.inst_mut(entry_inst) = switch;
        u.cfg_compute();
        assert_eq!(u.preds(loop_block), [entry, entry]);
        assert_eq!(u.preds(entry), [loop_block]);
        assert_eq!(u.preds(exit), [entry, loop_block]);
        assert!(u.preds(dead).is_empty());

        *u.inst_mut(entry_inst) = Inst::Ret { v: Value::Empty, agg: false };
        u.cfg_compute();
        for b in [entry, loop_block, exit, dead] {
            assert!(u.preds(b).is_empty());
        }
    }
}

#[test]
fn commit_renumbers_into_layout_order_and_drops_unlinked() {
    let mut ir = ProgramIr::make();
    let mut u = ir.take_unit_buf();
    let entry = u.add_block(BlockSourceKind::Entry);
    let exit = u.add_block(BlockSourceKind::LoopEnd);
    let middle = u.insert_block_after(entry, BlockSourceKind::LoopBody);
    let t = PhysicalType::scalar(ScalarType::U64);
    let ret = inst(&mut u, exit, Inst::Ret { v: Value::Empty, agg: false });
    let dead = inst(&mut u, middle, Inst::Data(DataInst::U64(7)));
    let sum = inst(
        &mut u,
        middle,
        Inst::IntAdd { lhs: Value::FnParam { t, index: 0 }, rhs: Value::byte(1), width: 64 },
    );
    inst(&mut u, middle, Inst::Jump(exit));
    inst(&mut u, entry, Inst::Jump(middle));
    let front = u.new_inst(
        Inst::IntSub { lhs: Value::Inst(sum), rhs: Value::byte(2), width: 64 },
        SpanId::NONE,
        IrComment::None,
    );
    u.insert_inst_after(sum, front);
    u.remove_inst(dead);
    *u.inst_mut(ret) = Inst::Ret { v: Value::Inst(front), agg: false };

    let mut unit = IrUnit::new(
        TypeId::PENDING,
        IrUnitId::Function(FunctionId::PENDING),
        PhysicalFunctionType::nil(),
        None,
        false,
    );
    commit_unit(&mut ir, &mut u, &mut unit);
    let v = unit.view(&ir.mem);
    assert_eq!(v.inst_count(), 5);
    let mut kinds = Vec::new();
    let mut expected = 1;
    for b in v.block_ids() {
        kinds.push(v.block(b).kind);
        assert_eq!(b.as_u32(), kinds.len() as u32);
        for id in v.block_insts(b) {
            assert_eq!(id.as_u32(), expected);
            expected += 1;
        }
    }
    assert!(kinds == [BlockSourceKind::Entry, BlockSourceKind::LoopBody, BlockSourceKind::LoopEnd]);
    let at = |n: u32| *v.inst(iid(n));
    assert!(matches!(at(1), Inst::Jump(target) if target == bid(2)));
    assert!(matches!(at(2), Inst::IntAdd { .. }));
    let Inst::IntSub { lhs, .. } = at(3) else { panic!() };
    assert!(lhs == Value::Inst(iid(2)));
    assert!(matches!(at(4), Inst::Jump(target) if target == bid(3)));
    let Inst::Ret { v: returned, .. } = at(5) else { panic!() };
    assert!(returned == Value::Inst(iid(3)));

    u.load(&ir.mem, &unit);
    assert_eq!(insts(&u, bid(2)), [iid(2), iid(3), iid(4)]);
    let split = u.split_block_at(bid(2), iid(3));
    assert_eq!(insts(&u, bid(2)), [iid(2)]);
    assert_eq!(insts(&u, split), [iid(3), iid(4)]);
    let after = u.new_inst(Inst::Unreachable, SpanId::NONE, IrComment::None);
    u.insert_inst_after(iid(4), after);
    assert_eq!(insts(&u, split), [iid(3), iid(4), after]);
    ir.release_unit_buf(u);
}

#[test]
fn cloned_phi_and_switch_payloads_are_owned_by_the_clone() {
    let mut src = UnitBuf::default();
    let mut u = UnitBuf::default();
    let from = u.add_block(BlockSourceKind::Entry);
    let to = u.add_block(BlockSourceKind::Entry);
    let dead = u.add_block(BlockSourceKind::Entry);
    let first = u.add_block(BlockSourceKind::Entry);
    let second = u.add_block(BlockSourceKind::Entry);
    let source = src.add_block(BlockSourceKind::Entry);
    let t = PhysicalType::scalar(ScalarType::U8);
    let incomings = src.push_phi_cases(&[
        PhiCase { from: dead, value: Value::byte(1) },
        PhiCase { from, value: Value::FnParam { t, index: 0 } },
        PhiCase { from: dead, value: Value::byte(2) },
        PhiCase { from, value: Value::byte(3) },
    ]);
    let original = inst(&mut src, source, Inst::Phi { t, incomings });
    let cases = src.push_switch_cases(&[SwitchCase { value: 0, target: from }]);
    let switch = inst(
        &mut src,
        source,
        Inst::Switch { value: Value::byte(0), width: 8, cases, default: dead },
    );
    let mut clones = Vec::new();
    for target in [first, first, second] {
        let mut cloned = *src.inst(original);
        u.clone_payload(&src.view(), &mut cloned);
        clones.push(inst(&mut u, target, cloned));
    }
    let mut cloned = *src.inst(switch);
    u.clone_payload(&src.view(), &mut cloned);
    let cloned_switch = inst(&mut u, first, cloned);
    let mut mappings = RewriteMappings::default();
    const ARGS: &[Value] = &[Value::byte(9)];
    mappings.fn_params = Some(ARGS);
    mappings.blocks.insert(from, to);

    for _ in 0..4 {
        rewrite_in_block(&mut u, first, &mappings);
        rewrite_phi_incoming(&mut u, first, to, from);
        rewrite_phi_incoming(&mut u, first, from, to);
        remove_phi_incomings(&mut u, first, &[dead]);
    }
    let view = u.view();
    for id in &clones[..2] {
        let Inst::Phi { incomings: edited, .. } = *view.inst(*id) else { panic!() };
        let actual = view.phi_cases(edited);
        assert_eq!(actual.len(), 2);
        assert_eq!(actual[0].from, to);
        assert_eq!(actual[1].from, to);
        assert!(actual[0].value == Value::byte(9));
        assert!(actual[1].value == Value::byte(3));
    }
    let Inst::Phi { incomings: untouched, .. } = *view.inst(clones[2]) else { panic!() };
    for payload in [src.view().phi_cases(incomings), view.phi_cases(untouched)] {
        assert_eq!(payload.len(), 4);
        assert_eq!(payload[1].from, from);
        assert!(payload[1].value == Value::FnParam { t, index: 0 });
    }
    let Inst::Switch { cases: edited, .. } = *view.inst(cloned_switch) else { panic!() };
    assert_eq!(view.switch_cases(edited)[0].target, to);
    assert_eq!(src.view().switch_cases(cases)[0].target, from);
    remove_phi_incomings(&mut u, first, &[to]);
    for id in &clones[..2] {
        let Inst::Phi { incomings, .. } = *u.inst(*id) else { panic!() };
        assert!(incomings.is_empty());
    }
}
