use super::*;
use crate::arith;

pub(super) enum Simplified {
    Const(ScalarType, u64),
    Value(Value),
}

pub(super) fn fold_inst(u: &UnitView, inst: &Inst) -> Option<Simplified> {
    match *inst {
        Inst::IntOverflows { op, lhs, rhs, t } => {
            let (l, r) = (lhs.const_bits(u)?, rhs.const_bits(u)?);
            let b = arith::int_overflows(op, t.width_bits(), l, r);
            Some(Simplified::Const(ScalarType::I8, b as u64))
        }
        Inst::IntBinop { op, lhs, rhs, t } => {
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
            let is_shift_or_rotate = matches!(
                op,
                IntOp::Shl | IntOp::ShrU | IntOp::ShrS | IntOp::RotateLeft | IntOp::RotateRight
            );
            match (op, l, r) {
                (_, _, Some(r)) if is_shift_or_rotate && (r as u32) & (width as u32 - 1) == 0 => {
                    Some(Simplified::Value(lhs))
                }
                (_, Some(0), _) if is_shift_or_rotate => Some(zero),
                (IntOp::Add | IntOp::Or | IntOp::Xor, Some(0), _) => Some(Simplified::Value(rhs)),
                (IntOp::Add | IntOp::Sub | IntOp::Or | IntOp::Xor, _, Some(0)) => {
                    Some(Simplified::Value(lhs))
                }
                (IntOp::Mul, Some(1), _) => Some(Simplified::Value(rhs)),
                (IntOp::Mul | IntOp::DivU | IntOp::DivS, _, Some(1)) => {
                    Some(Simplified::Value(lhs))
                }
                (IntOp::Mul | IntOp::And, Some(0), _) | (IntOp::Mul | IntOp::And, _, Some(0)) => {
                    Some(zero)
                }
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
        Inst::IntCmp3 { op, lhs, rhs, width } => {
            if lhs == rhs {
                return Some(Simplified::Const(ScalarType::I8, 0));
            }
            let order = arith::int_cmp3(op, width, lhs.const_bits(u)?, rhs.const_bits(u)?);
            Some(Simplified::Const(ScalarType::I8, order))
        }
        Inst::IntCmp { lhs, rhs, pred, width } => {
            if let (Some(l), Some(r)) = (lhs.const_bits(u), rhs.const_bits(u)) {
                let b = arith::int_cmp(width, pred, l, r);
                return Some(Simplified::Const(ScalarType::I8, b as u64));
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
            Some(Simplified::Const(ScalarType::I8, b as u64))
        }
        Inst::FloatBinop { op, lhs, rhs, t } => {
            let bits = arith::float_op(op, t.width_bits(), lhs.const_bits(u)?, rhs.const_bits(u)?);
            Some(Simplified::Const(t, bits))
        }
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            let b = arith::float_cmp(width, pred, lhs.const_bits(u)?, rhs.const_bits(u)?);
            Some(Simplified::Const(ScalarType::I8, b as u64))
        }
        Inst::FloatUnop { op, v, t } => {
            Some(Simplified::Const(t, arith::float_unop(op, t.width_bits(), v.const_bits(u)?)))
        }
        Inst::FloatFma { a, b, c, t } => {
            let (a, b, c) = (a.const_bits(u)?, b.const_bits(u)?, c.const_bits(u)?);
            Some(Simplified::Const(t, arith::float_fma(t.width_bits(), a, b, c)))
        }
        Inst::Select { cond, cons, alt, t } => {
            if let Some(c) = cond.const_bits(u) {
                return Some(Simplified::Value(if c != 0 { cons } else { alt }));
            }
            if cons == alt {
                return Some(Simplified::Value(cons));
            }
            match (t, cons.const_bits(u), alt.const_bits(u)) {
                (ScalarType::I8, Some(1), Some(0)) => Some(Simplified::Value(cond)),
                _ => None,
            }
        }
        Inst::BoolNegate { v } => {
            Some(Simplified::Const(ScalarType::I8, (v.const_bits(u)? == 0) as u64))
        }
        Inst::IntUnop { op, v, t } => {
            Some(Simplified::Const(t, arith::int_unop(op, t.width_bits(), v.const_bits(u)?)))
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
