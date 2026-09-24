use super::*;
use crate::arith::{self, FloatOp, IntOp};

pub(super) enum Simplified {
    Const(ScalarType, u64),
    Value(Value),
}

pub(super) fn fold_inst(u: &UnitView, inst: &Inst) -> Option<Simplified> {
    match *inst {
        Inst::IntAdd { lhs, rhs, width } => int_binop(u, IntOp::Add, lhs, rhs, width),
        Inst::IntSub { lhs, rhs, width } => int_binop(u, IntOp::Sub, lhs, rhs, width),
        Inst::IntMul { lhs, rhs, width } => int_binop(u, IntOp::Mul, lhs, rhs, width),
        Inst::IntDivUnsigned { lhs, rhs, width } => int_binop(u, IntOp::DivU, lhs, rhs, width),
        Inst::IntDivSigned { lhs, rhs, width } => int_binop(u, IntOp::DivS, lhs, rhs, width),
        Inst::IntRemUnsigned { lhs, rhs, width } => int_binop(u, IntOp::RemU, lhs, rhs, width),
        Inst::IntRemSigned { lhs, rhs, width } => int_binop(u, IntOp::RemS, lhs, rhs, width),
        Inst::BitAnd { lhs, rhs, width } => int_binop(u, IntOp::And, lhs, rhs, width),
        Inst::BitOr { lhs, rhs, width } => int_binop(u, IntOp::Or, lhs, rhs, width),
        Inst::BitXor { lhs, rhs, width } => int_binop(u, IntOp::Xor, lhs, rhs, width),
        Inst::BitShiftLeft { lhs, rhs, width } => int_binop(u, IntOp::Shl, lhs, rhs, width),
        Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
            int_binop(u, IntOp::ShrU, lhs, rhs, width)
        }
        Inst::BitSignedShiftRight { lhs, rhs, width } => int_binop(u, IntOp::ShrS, lhs, rhs, width),
        Inst::IntCmp { lhs, rhs, pred, width } => {
            if let (Some(l), Some(r)) = (lhs.const_bits(u), rhs.const_bits(u)) {
                let b = arith::int_cmp(width, pred, l, r);
                return Some(Simplified::Const(ScalarType::Bool, b as u64));
            }
            if lhs != rhs {
                return None;
            }
            let b = matches!(
                pred,
                IntCmpPred::Eq
                    | IntCmpPred::Sle
                    | IntCmpPred::Sge
                    | IntCmpPred::Ule
                    | IntCmpPred::Uge
            );
            Some(Simplified::Const(ScalarType::Bool, b as u64))
        }
        Inst::FloatAdd { lhs, rhs, width } => float_binop(u, FloatOp::Add, lhs, rhs, width),
        Inst::FloatSub { lhs, rhs, width } => float_binop(u, FloatOp::Sub, lhs, rhs, width),
        Inst::FloatMul { lhs, rhs, width } => float_binop(u, FloatOp::Mul, lhs, rhs, width),
        Inst::FloatDiv { lhs, rhs, width } => float_binop(u, FloatOp::Div, lhs, rhs, width),
        Inst::FloatRem { lhs, rhs, width } => float_binop(u, FloatOp::Rem, lhs, rhs, width),
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            let b = arith::float_cmp(width, pred, lhs.const_bits(u)?, rhs.const_bits(u)?);
            Some(Simplified::Const(ScalarType::Bool, b as u64))
        }
        Inst::FloatNeg { v, width } => {
            let bits = arith::float_neg(width, v.const_bits(u)?);
            Some(Simplified::Const(get_value_kind(u, v).expect_scalar(), bits))
        }
        Inst::BoolNegate { v } => {
            Some(Simplified::Const(ScalarType::Bool, (v.const_bits(u)? == 0) as u64))
        }
        Inst::BitNot { v } => {
            let t = get_value_kind(u, v).expect_scalar();
            Some(Simplified::Const(t, arith::bit_not(t.width_bits(), v.const_bits(u)?)))
        }
        Inst::IntTrunc { v, to } => {
            Some(Simplified::Const(to, arith::int_trunc(to.width_bits(), v.const_bits(u)?)))
        }
        Inst::IntExtU { v, to } => Some(Simplified::Const(to, v.const_bits(u)?)),
        Inst::IntExtS { v, from, to } => {
            let bits = arith::int_ext_s(from.width_bits(), to.width_bits(), v.const_bits(u)?);
            Some(Simplified::Const(to, bits))
        }
        _ => None,
    }
}

fn int_binop(u: &UnitView, op: IntOp, lhs: Value, rhs: Value, width: u8) -> Option<Simplified> {
    let t = get_value_kind(u, lhs).expect_scalar();
    if t == ScalarType::Pointer {
        return None;
    }
    let l = lhs.const_bits(u);
    let r = rhs.const_bits(u);
    if let (Some(l), Some(r)) = (l, r) {
        return arith::int_op(op, width, l, r).ok().map(|bits| Simplified::Const(t, bits));
    }
    let mask = arith::width_mask(width);
    let zero = Simplified::Const(t, 0);
    let is_shift = matches!(op, IntOp::Shl | IntOp::ShrU | IntOp::ShrS);
    match (op, l, r) {
        (_, _, Some(r)) if is_shift && (r as u32) & (width as u32 - 1) == 0 => {
            Some(Simplified::Value(lhs))
        }
        (_, Some(0), _) if is_shift => Some(zero),
        (IntOp::Add | IntOp::Or | IntOp::Xor, Some(0), _) => Some(Simplified::Value(rhs)),
        (IntOp::Add | IntOp::Sub | IntOp::Or | IntOp::Xor, _, Some(0)) => {
            Some(Simplified::Value(lhs))
        }
        (IntOp::Mul, Some(1), _) => Some(Simplified::Value(rhs)),
        (IntOp::Mul | IntOp::DivU | IntOp::DivS, _, Some(1)) => Some(Simplified::Value(lhs)),
        (IntOp::Mul | IntOp::And, Some(0), _) | (IntOp::Mul | IntOp::And, _, Some(0)) => Some(zero),
        (IntOp::And, Some(m), _) if m & mask == mask => Some(Simplified::Value(rhs)),
        (IntOp::And, _, Some(m)) if m & mask == mask => Some(Simplified::Value(lhs)),
        (IntOp::Or, Some(m), _) | (IntOp::Or, _, Some(m)) if m & mask == mask => {
            Some(Simplified::Const(t, mask))
        }
        (IntOp::Sub | IntOp::Xor, _, _) if lhs == rhs => Some(zero),
        (IntOp::And | IntOp::Or, _, _) if lhs == rhs => Some(Simplified::Value(lhs)),
        _ => None,
    }
}

fn float_binop(u: &UnitView, op: FloatOp, lhs: Value, rhs: Value, width: u8) -> Option<Simplified> {
    let bits = arith::float_op(op, width, lhs.const_bits(u)?, rhs.const_bits(u)?);
    Some(Simplified::Const(get_value_kind(u, lhs).expect_scalar(), bits))
}
