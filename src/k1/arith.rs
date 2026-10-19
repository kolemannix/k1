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
    MulHighU,
    MulHighS,
}

impl IntOp {
    pub fn name(self) -> &'static str {
        match self {
            IntOp::Add => "add",
            IntOp::Sub => "sub",
            IntOp::Mul => "mul",
            IntOp::DivU => "udiv",
            IntOp::DivS => "sdiv",
            IntOp::RemU => "urem",
            IntOp::RemS => "srem",
            IntOp::And => "and",
            IntOp::Or => "or",
            IntOp::Xor => "xor",
            IntOp::Shl => "shl",
            IntOp::ShrU => "lshr",
            IntOp::ShrS => "ashr",
            IntOp::MulHighU => "umulh",
            IntOp::MulHighS => "smulh",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum OverflowOp {
    Add,
    Sub,
    Mul,
}

impl OverflowOp {
    pub fn from_u8(v: u8) -> OverflowOp {
        debug_assert!(v <= OverflowOp::Mul as u8, "bad overflow op tag {v}");
        unsafe { core::mem::transmute::<u8, OverflowOp>(v) }
    }

    pub fn name(self) -> &'static str {
        match self {
            OverflowOp::Add => "add_overflows",
            OverflowOp::Sub => "sub_overflows",
            OverflowOp::Mul => "mul_overflows",
        }
    }
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
#[repr(u8)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Min,
    Max,
}

impl FloatOp {
    pub fn from_u8(v: u8) -> FloatOp {
        debug_assert!(v <= FloatOp::Max as u8, "bad float op tag {v}");
        unsafe { core::mem::transmute::<u8, FloatOp>(v) }
    }

    pub fn name(self) -> &'static str {
        match self {
            FloatOp::Add => "fadd",
            FloatOp::Sub => "fsub",
            FloatOp::Mul => "fmul",
            FloatOp::Div => "fdiv",
            FloatOp::Rem => "frem",
            FloatOp::Min => "fmin",
            FloatOp::Max => "fmax",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum FloatUnop {
    Neg,
    Sqrt,
    Floor,
    Ceil,
    RoundToZero,
    RoundEven,
}

impl FloatUnop {
    pub fn from_u8(v: u8) -> FloatUnop {
        debug_assert!(v <= FloatUnop::RoundEven as u8, "bad float unop tag {v}");
        unsafe { core::mem::transmute::<u8, FloatUnop>(v) }
    }

    pub fn name(self) -> &'static str {
        match self {
            FloatUnop::Neg => "fneg",
            FloatUnop::Sqrt => "fsqrt",
            FloatUnop::Floor => "ffloor",
            FloatUnop::Ceil => "fceil",
            FloatUnop::RoundToZero => "fround_zero",
            FloatUnop::RoundEven => "fround_even",
        }
    }
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
        IntOp::MulHighU => ((l as u128 * r as u128) >> width) as u64,
        IntOp::MulHighS => {
            ((sign_extend(width, l) as i128 * sign_extend(width, r) as i128) >> width) as u64
        }
    };
    Ok(bits & mask)
}

#[inline(always)]
pub fn int_overflows(op: OverflowOp, signed: bool, width: u8, lhs: u64, rhs: u64) -> bool {
    let mask = width_mask(width);
    let (l, r, min, max) = if signed {
        let half = 1i128 << (width - 1);
        (sign_extend(width, lhs) as i128, sign_extend(width, rhs) as i128, -half, half - 1)
    } else {
        ((lhs & mask) as i128, (rhs & mask) as i128, 0, mask as i128)
    };
    let exact = match op {
        OverflowOp::Add => Some(l + r),
        OverflowOp::Sub => Some(l - r),
        OverflowOp::Mul => l.checked_mul(r),
    };
    match exact {
        Some(v) => v < min || v > max,
        None => true,
    }
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
        IntCmpPred::Ne => l != r,
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

macro_rules! float_op_in {
    ($op:expr, $l:expr, $r:expr) => {{
        let (l, r) = ($l, $r);
        match $op {
            FloatOp::Add => l + r,
            FloatOp::Sub => l - r,
            FloatOp::Mul => l * r,
            FloatOp::Div => l / r,
            FloatOp::Rem => l % r,
            FloatOp::Min if l.is_nan() || r.is_nan() => l + r,
            FloatOp::Min if l == r => {
                if l.is_sign_negative() {
                    l
                } else {
                    r
                }
            }
            FloatOp::Min => {
                if l < r {
                    l
                } else {
                    r
                }
            }
            FloatOp::Max if l.is_nan() || r.is_nan() => l + r,
            FloatOp::Max if l == r => {
                if l.is_sign_positive() {
                    l
                } else {
                    r
                }
            }
            FloatOp::Max => {
                if l > r {
                    l
                } else {
                    r
                }
            }
        }
    }};
}

macro_rules! float_unop_in {
    ($op:expr, $v:expr) => {{
        let v = $v;
        match $op {
            FloatUnop::Neg => -v,
            FloatUnop::Sqrt => v.sqrt(),
            FloatUnop::Floor => v.floor(),
            FloatUnop::Ceil => v.ceil(),
            FloatUnop::RoundToZero => v.trunc(),
            FloatUnop::RoundEven => v.round_ties_even(),
        }
    }};
}

#[inline(always)]
pub fn float_op(op: FloatOp, width: u8, lhs: u64, rhs: u64) -> u64 {
    if width == 32 {
        float_op_in!(op, f32::from_bits(lhs as u32), f32::from_bits(rhs as u32)).to_bits() as u64
    } else {
        float_op_in!(op, f64::from_bits(lhs), f64::from_bits(rhs)).to_bits()
    }
}

#[inline(always)]
pub fn float_unop(op: FloatUnop, width: u8, bits: u64) -> u64 {
    if width == 32 {
        float_unop_in!(op, f32::from_bits(bits as u32)).to_bits() as u64
    } else {
        float_unop_in!(op, f64::from_bits(bits)).to_bits()
    }
}

#[inline(always)]
pub fn float_fma(width: u8, a: u64, b: u64, c: u64) -> u64 {
    if width == 32 {
        let (a, b, c) =
            (f32::from_bits(a as u32), f32::from_bits(b as u32), f32::from_bits(c as u32));
        a.mul_add(b, c).to_bits() as u64
    } else {
        f64::from_bits(a).mul_add(f64::from_bits(b), f64::from_bits(c)).to_bits()
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
            FloatCmpPred::Ne => l != r,
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
            FloatCmpPred::Ne => l != r,
        }
    }
}

#[cfg(test)]
mod arith_test;
