use super::*;
use crate::arith::{self, FloatOp, IntOp};

pub(super) enum Simplified {
    Const(ScalarType, u64),
    Value(Value),
}

pub(super) fn fold_inst(u: &UnitView, inst: &Inst) -> Option<Simplified> {
    match *inst {
        Inst::IntAdd { lhs, rhs, t } => int_binop(u, IntOp::Add, lhs, rhs, t),
        Inst::IntSub { lhs, rhs, t } => int_binop(u, IntOp::Sub, lhs, rhs, t),
        Inst::IntMul { lhs, rhs, t } => int_binop(u, IntOp::Mul, lhs, rhs, t),
        Inst::IntDivUnsigned { lhs, rhs, t } => int_binop(u, IntOp::DivU, lhs, rhs, t),
        Inst::IntDivSigned { lhs, rhs, t } => int_binop(u, IntOp::DivS, lhs, rhs, t),
        Inst::IntRemUnsigned { lhs, rhs, t } => int_binop(u, IntOp::RemU, lhs, rhs, t),
        Inst::IntRemSigned { lhs, rhs, t } => int_binop(u, IntOp::RemS, lhs, rhs, t),
        Inst::BitAnd { lhs, rhs, t } => int_binop(u, IntOp::And, lhs, rhs, t),
        Inst::BitOr { lhs, rhs, t } => int_binop(u, IntOp::Or, lhs, rhs, t),
        Inst::BitXor { lhs, rhs, t } => int_binop(u, IntOp::Xor, lhs, rhs, t),
        Inst::BitShiftLeft { lhs, rhs, t } => int_binop(u, IntOp::Shl, lhs, rhs, t),
        Inst::BitUnsignedShiftRight { lhs, rhs, t } => {
            int_binop(u, IntOp::ShrU, lhs, rhs, t)
        }
        Inst::BitSignedShiftRight { lhs, rhs, t } => int_binop(u, IntOp::ShrS, lhs, rhs, t),
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
        Inst::FloatAdd { lhs, rhs, t } => float_binop(u, FloatOp::Add, lhs, rhs, t),
        Inst::FloatSub { lhs, rhs, t } => float_binop(u, FloatOp::Sub, lhs, rhs, t),
        Inst::FloatMul { lhs, rhs, t } => float_binop(u, FloatOp::Mul, lhs, rhs, t),
        Inst::FloatDiv { lhs, rhs, t } => float_binop(u, FloatOp::Div, lhs, rhs, t),
        Inst::FloatRem { lhs, rhs, t } => float_binop(u, FloatOp::Rem, lhs, rhs, t),
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            let b = arith::float_cmp(width, pred, lhs.const_bits(u)?, rhs.const_bits(u)?);
            Some(Simplified::Const(ScalarType::Bool, b as u64))
        }
        Inst::FloatNeg { v, t } => {
            Some(Simplified::Const(t, arith::float_neg(t.width_bits(), v.const_bits(u)?)))
        }
        Inst::BoolNegate { v } => {
            Some(Simplified::Const(ScalarType::Bool, (v.const_bits(u)? == 0) as u64))
        }
        Inst::BitNot { v, t } => {
            Some(Simplified::Const(t, arith::bit_not(t.width_bits(), v.const_bits(u)?)))
        }
        Inst::IntTrunc { v, to } => {
            Some(Simplified::Const(to, arith::int_trunc(to.width_bits(), v.const_bits(u)?)))
        }
        Inst::IntExtU { v, to } => Some(Simplified::Const(to, v.const_bits(u)?)),
        Inst::BitCast { v, to } => {
            if get_value_kind(u, v).as_value() == Some(to) {
                return Some(Simplified::Value(v));
            }
            Some(Simplified::Const(to.as_scalar()?, v.const_bits(u)?))
        }
        Inst::IntExtS { v, from, to } => {
            let bits = arith::int_ext_s(from.width_bits(), to.width_bits(), v.const_bits(u)?);
            Some(Simplified::Const(to, bits))
        }
        _ => None,
    }
}

fn int_binop(u: &UnitView, op: IntOp, lhs: Value, rhs: Value, t: ScalarType) -> Option<Simplified> {
    let width = t.width_bits();
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

fn float_binop(u: &UnitView, op: FloatOp, lhs: Value, rhs: Value, t: ScalarType) -> Option<Simplified> {
    let bits = arith::float_op(op, t.width_bits(), lhs.const_bits(u)?, rhs.const_bits(u)?);
    Some(Simplified::Const(t, bits))
}
