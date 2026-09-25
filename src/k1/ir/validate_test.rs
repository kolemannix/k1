use super::*;
use crate::compiler::test_support::{compile_source, function_named};

fn u64_value(n: u32) -> Value {
    Value::Data32 { t: ScalarType::U64, data: n }
}

fn push(u: &mut UnitBuf, block: BlockId, inst: Inst) -> InstId {
    let id = u.new_inst(inst, SpanId::NONE, IrComment::None);
    u.push_inst(block, id);
    id
}

fn validate_handmade(name: &str, build: impl FnOnce(&mut UnitBuf)) -> Result<(), String> {
    let mut k1 = compile_source(name, "fn f(): u64 { 0 }\nfn main(): i32 { 0 }\n");
    let f = function_named(&k1, "f");
    k1.compile_function_for_exec(f, None, SpanId::NONE).unwrap();
    let mut unit = *k1.ir.function_unit(f).unwrap();
    let mut u = k1.ir.take_unit_buf();
    build(&mut u);
    commit_unit(&mut k1.ir, &u, &mut unit);
    k1.ir.release_unit_buf(u);
    *k1.ir.function_unit_mut(f).unwrap() = unit;
    match validate_unit(&k1, IrUnitId::Function(f)) {
        Ok(()) => Ok(()),
        Err(e) => Err(k1.ident_str(e.message).to_string()),
    }
}

fn self_loop(u: &mut UnitBuf, extra_case: Option<fn(BlockId, BlockId, BlockId) -> PhiCase>) {
    let t = PhysicalType::scalar(ScalarType::U64);
    let entry = u.add_block(BlockSourceKind::Entry);
    let body = u.add_block(BlockSourceKind::LoopBody);
    let exit = u.add_block(BlockSourceKind::LoopEnd);
    push(u, entry, Inst::Jump(body));
    let counter = push(u, body, Inst::Phi { t, incomings: IrRange::EMPTY });
    let next = push(
        u,
        body,
        Inst::IntAdd { lhs: Value::Inst(counter), rhs: u64_value(1), t: ScalarType::U64 },
    );
    let mut cases = vec![PhiCase { from: entry, value: u64_value(0) }];
    match extra_case {
        None => cases.push(PhiCase { from: body, value: Value::Inst(next) }),
        Some(make) => cases.push(make(entry, body, exit)),
    }
    let incomings = u.push_phi_cases(&cases);
    *u.inst_mut(counter) = Inst::Phi { t, incomings };
    let again = push(
        u,
        body,
        Inst::IntCmp {
            lhs: Value::Inst(next),
            rhs: u64_value(10),
            pred: IntCmpPred::Ult,
            width: 64,
        },
    );
    push(u, body, Inst::JumpIf { cond: Value::Inst(again), cons: body, alt: exit });
    push(u, exit, Inst::Ret { v: Value::Inst(next), agg: false });
}

#[test]
fn single_block_self_loop_phi_validates() {
    validate_handmade("validate_self_loop", |u| self_loop(u, None)).unwrap();
}

#[test]
fn phi_missing_an_incoming_is_rejected() {
    let error = validate_handmade("validate_missing_incoming", |u| {
        self_loop(u, Some(|entry, _, _| PhiCase { from: entry, value: u64_value(5) }))
    })
    .unwrap_err();
    assert!(error.contains("phi has 2 incomings for predecessor b1"), "{error}");
    assert!(error.contains("phi has 0 incomings for predecessor b2"), "{error}");
}

#[test]
fn phi_incoming_from_non_predecessor_is_rejected() {
    let error = validate_handmade("validate_extra_incoming", |u| {
        self_loop(u, Some(|_, _, exit| PhiCase { from: exit, value: u64_value(5) }))
    })
    .unwrap_err();
    assert!(error.contains("phi incoming from non-predecessor b3"), "{error}");
    assert!(error.contains("phi has 0 incomings for predecessor b2"), "{error}");
}

#[test]
fn entry_with_predecessor_is_rejected() {
    let error = validate_handmade("validate_entry_pred", |u| {
        let entry = u.add_block(BlockSourceKind::Entry);
        let exit = u.add_block(BlockSourceKind::LoopEnd);
        push(
            u,
            entry,
            Inst::JumpIf { cond: Value::zero(ScalarType::Bool), cons: entry, alt: exit },
        );
        push(u, exit, Inst::Ret { v: u64_value(0), agg: false });
    })
    .unwrap_err();
    assert!(error.contains("b1: entry block has predecessors"), "{error}");
}

#[test]
fn use_that_does_not_dominate_is_rejected() {
    let error = validate_handmade("validate_dominance", |u| {
        let entry = u.add_block(BlockSourceKind::Entry);
        let left = u.add_block(BlockSourceKind::ArmCons);
        let right = u.add_block(BlockSourceKind::ArmCons);
        let join = u.add_block(BlockSourceKind::MatchEnd);
        push(
            u,
            entry,
            Inst::JumpIf { cond: Value::zero(ScalarType::Bool), cons: left, alt: right },
        );
        let only_left = push(
            u,
            left,
            Inst::IntAdd { lhs: u64_value(1), rhs: u64_value(2), t: ScalarType::U64 },
        );
        push(u, left, Inst::Jump(join));
        let only_right = push(
            u,
            right,
            Inst::IntAdd { lhs: u64_value(3), rhs: u64_value(4), t: ScalarType::U64 },
        );
        push(u, right, Inst::Jump(join));
        let t = PhysicalType::scalar(ScalarType::U64);
        let incomings = u.push_phi_cases(&[
            PhiCase { from: left, value: Value::Inst(only_right) },
            PhiCase { from: right, value: Value::Inst(only_right) },
        ]);
        let joined = push(u, join, Inst::Phi { t, incomings });
        let sum = push(
            u,
            join,
            Inst::IntAdd {
                lhs: Value::Inst(only_left),
                rhs: Value::Inst(joined),
                t: ScalarType::U64,
            },
        );
        push(u, join, Inst::Ret { v: Value::Inst(sum), agg: false });
    })
    .unwrap_err();
    assert!(error.contains("phi incoming i4 does not dominate the end of b2"), "{error}");
    assert!(error.contains("use of i2 which does not dominate it"), "{error}");
    assert!(!error.contains("phi incoming i4 does not dominate the end of b3"), "{error}");
}
