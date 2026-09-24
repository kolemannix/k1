use crate::ir::{FloatCmpPred, IntCmpPred};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntOp {
    Add,
    Sub,
    Mul,
    DivU,
    DivS,
    RemU,
    RemS,
    And,
    Or,
    Xor,
    Shl,
    ShrU,
    ShrS,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntOpError {
    DivisionByZero,
    SignedDivisionOverflow,
}

impl IntOpError {
    pub fn message(self) -> &'static str {
        match self {
            IntOpError::DivisionByZero => "Division by zero",
            IntOpError::SignedDivisionOverflow => "Integer division overflow: min-value / -1",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[inline(always)]
pub const fn width_mask(width: u8) -> u64 {
    u64::MAX >> (64 - width as u32)
}

#[inline(always)]
pub const fn sign_extend(width: u8, bits: u64) -> i64 {
    let shift = 64 - width as u32;
    ((bits << shift) as i64) >> shift
}

#[inline(always)]
pub fn int_op(op: IntOp, width: u8, lhs: u64, rhs: u64) -> Result<u64, IntOpError> {
    let mask = width_mask(width);
    let l = lhs & mask;
    let r = rhs & mask;
    let shift = (rhs as u32) & (width as u32 - 1);
    let bits = match op {
        IntOp::Add => l.wrapping_add(r),
        IntOp::Sub => l.wrapping_sub(r),
        IntOp::Mul => l.wrapping_mul(r),
        IntOp::DivU => l.checked_div(r).ok_or(IntOpError::DivisionByZero)?,
        IntOp::RemU => l.checked_rem(r).ok_or(IntOpError::DivisionByZero)?,
        IntOp::DivS | IntOp::RemS => {
            if r == 0 {
                return Err(IntOpError::DivisionByZero);
            }
            if r == mask && l == 1 << (width - 1) {
                return Err(IntOpError::SignedDivisionOverflow);
            }
            let sl = sign_extend(width, l);
            let sr = sign_extend(width, r);
            (if op == IntOp::DivS { sl / sr } else { sl % sr }) as u64
        }
        IntOp::And => l & r,
        IntOp::Or => l | r,
        IntOp::Xor => l ^ r,
        IntOp::Shl => l << shift,
        IntOp::ShrU => l >> shift,
        IntOp::ShrS => (sign_extend(width, l) >> shift) as u64,
    };
    Ok(bits & mask)
}

#[inline(always)]
pub fn int_cmp(width: u8, pred: IntCmpPred, lhs: u64, rhs: u64) -> bool {
    let shift = 64 - width as u32;
    let l = lhs << shift;
    let r = rhs << shift;
    match pred {
        IntCmpPred::Eq => l == r,
        IntCmpPred::Slt => (l as i64) < (r as i64),
        IntCmpPred::Sle => (l as i64) <= (r as i64),
        IntCmpPred::Sgt => (l as i64) > (r as i64),
        IntCmpPred::Sge => (l as i64) >= (r as i64),
        IntCmpPred::Ult => l < r,
        IntCmpPred::Ule => l <= r,
        IntCmpPred::Ugt => l > r,
        IntCmpPred::Uge => l >= r,
    }
}

#[inline(always)]
pub fn bit_not(width: u8, bits: u64) -> u64 {
    !bits & width_mask(width)
}

#[inline(always)]
pub fn int_trunc(to_width: u8, bits: u64) -> u64 {
    bits & width_mask(to_width)
}

#[inline(always)]
pub fn int_ext_s(from_width: u8, to_width: u8, bits: u64) -> u64 {
    sign_extend(from_width, bits) as u64 & width_mask(to_width)
}

#[inline(always)]
pub fn float_op(op: FloatOp, width: u8, lhs: u64, rhs: u64) -> u64 {
    if width == 32 {
        let l = f32::from_bits(lhs as u32);
        let r = f32::from_bits(rhs as u32);
        let v = match op {
            FloatOp::Add => l + r,
            FloatOp::Sub => l - r,
            FloatOp::Mul => l * r,
            FloatOp::Div => l / r,
            FloatOp::Rem => l % r,
        };
        v.to_bits() as u64
    } else {
        let l = f64::from_bits(lhs);
        let r = f64::from_bits(rhs);
        let v = match op {
            FloatOp::Add => l + r,
            FloatOp::Sub => l - r,
            FloatOp::Mul => l * r,
            FloatOp::Div => l / r,
            FloatOp::Rem => l % r,
        };
        v.to_bits()
    }
}

#[inline(always)]
pub fn float_neg(width: u8, bits: u64) -> u64 {
    if width == 32 {
        (-f32::from_bits(bits as u32)).to_bits() as u64
    } else {
        (-f64::from_bits(bits)).to_bits()
    }
}

#[inline(always)]
pub fn float_cmp(width: u8, pred: FloatCmpPred, lhs: u64, rhs: u64) -> bool {
    if width == 32 {
        let l = f32::from_bits(lhs as u32);
        let r = f32::from_bits(rhs as u32);
        match pred {
            FloatCmpPred::Eq => l == r,
            FloatCmpPred::Lt => l < r,
            FloatCmpPred::Le => l <= r,
            FloatCmpPred::Gt => l > r,
            FloatCmpPred::Ge => l >= r,
        }
    } else {
        let l = f64::from_bits(lhs);
        let r = f64::from_bits(rhs);
        match pred {
            FloatCmpPred::Eq => l == r,
            FloatCmpPred::Lt => l < r,
            FloatCmpPred::Le => l <= r,
            FloatCmpPred::Gt => l > r,
            FloatCmpPred::Ge => l >= r,
        }
    }
}

#[cfg(test)]
mod arith_test;
