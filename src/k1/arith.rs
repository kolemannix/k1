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
    RotateLeft,
    RotateRight,
}

impl IntOp {
    pub fn takes_count(self) -> bool {
        matches!(
            self,
            IntOp::Shl | IntOp::ShrU | IntOp::ShrS | IntOp::RotateLeft | IntOp::RotateRight
        )
    }

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
            IntOp::RotateLeft => "rotl",
            IntOp::RotateRight => "rotr",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntUnop {
    Not,
    Neg,
    Ctz,
    Clz,
    Popcount,
    Bswap,
}

impl IntUnop {
    pub fn name(self) -> &'static str {
        match self {
            IntUnop::Not => "not",
            IntUnop::Neg => "neg",
            IntUnop::Ctz => "ctz",
            IntUnop::Clz => "clz",
            IntUnop::Popcount => "popcnt",
            IntUnop::Bswap => "bswap",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OverflowOp {
    AddU,
    AddS,
    SubU,
    SubS,
    MulU,
    MulS,
}

impl OverflowOp {
    pub fn name(self) -> &'static str {
        match self {
            OverflowOp::AddU => "uadd_overflows",
            OverflowOp::AddS => "sadd_overflows",
            OverflowOp::SubU => "usub_overflows",
            OverflowOp::SubS => "ssub_overflows",
            OverflowOp::MulU => "umul_overflows",
            OverflowOp::MulS => "smul_overflows",
        }
    }

    pub fn wrapping_op(self) -> IntOp {
        match self {
            OverflowOp::AddU | OverflowOp::AddS => IntOp::Add,
            OverflowOp::SubU | OverflowOp::SubS => IntOp::Sub,
            OverflowOp::MulU | OverflowOp::MulS => IntOp::Mul,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Cmp3Op {
    Ucmp,
    Scmp,
}

impl Cmp3Op {
    pub fn name(self) -> &'static str {
        match self {
            Cmp3Op::Ucmp => "ucmp",
            Cmp3Op::Scmp => "scmp",
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
pub enum FloatUnop {
    Neg,
    Sqrt,
    Floor,
    Ceil,
    RoundToZero,
    RoundEven,
}

impl FloatUnop {
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IntCmpPred {
    Eq,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
    Ne,
}

impl IntCmpPred {
    pub fn name(self) -> &'static str {
        match self {
            IntCmpPred::Eq => "eq",
            IntCmpPred::Slt => "slt",
            IntCmpPred::Sle => "sle",
            IntCmpPred::Sgt => "sgt",
            IntCmpPred::Sge => "sge",
            IntCmpPred::Ult => "ult",
            IntCmpPred::Ule => "ule",
            IntCmpPred::Ugt => "ugt",
            IntCmpPred::Uge => "uge",
            IntCmpPred::Ne => "ne",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatCmpPred {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    Ne,
}

impl FloatCmpPred {
    pub fn name(self) -> &'static str {
        match self {
            FloatCmpPred::Eq => "oeq",
            FloatCmpPred::Lt => "olt",
            FloatCmpPred::Le => "ole",
            FloatCmpPred::Gt => "ogt",
            FloatCmpPred::Ge => "oge",
            FloatCmpPred::Ne => "une",
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
        IntOp::RotateLeft if shift == 0 => l,
        IntOp::RotateLeft => (l << shift) | (l >> (width as u32 - shift)),
        IntOp::RotateRight if shift == 0 => l,
        IntOp::RotateRight => (l >> shift) | (l << (width as u32 - shift)),
    };
    Ok(bits & mask)
}

#[inline(always)]
pub fn int_unop(op: IntUnop, width: u8, bits: u64) -> u64 {
    let mask = width_mask(width);
    let v = bits & mask;
    match op {
        IntUnop::Not => !v & mask,
        IntUnop::Neg => v.wrapping_neg() & mask,
        IntUnop::Ctz if v == 0 => width as u64,
        IntUnop::Ctz => v.trailing_zeros() as u64,
        IntUnop::Clz => (v.leading_zeros() - (64 - width as u32)) as u64,
        IntUnop::Popcount => v.count_ones() as u64,
        IntUnop::Bswap => v.swap_bytes() >> (64 - width as u32),
    }
}

#[inline(always)]
pub fn int_cmp3(op: Cmp3Op, width: u8, lhs: u64, rhs: u64) -> u64 {
    let order = match op {
        Cmp3Op::Scmp => sign_extend(width, lhs).cmp(&sign_extend(width, rhs)),
        Cmp3Op::Ucmp => {
            let mask = width_mask(width);
            (lhs & mask).cmp(&(rhs & mask))
        }
    };
    order as i8 as u8 as u64
}

#[inline(always)]
pub fn int_overflows(op: OverflowOp, width: u8, lhs: u64, rhs: u64) -> bool {
    let mask = width_mask(width);
    let signed = matches!(op, OverflowOp::AddS | OverflowOp::SubS | OverflowOp::MulS);
    let (l, r, min, max) = if signed {
        let half = 1i128 << (width - 1);
        (sign_extend(width, lhs) as i128, sign_extend(width, rhs) as i128, -half, half - 1)
    } else {
        ((lhs & mask) as i128, (rhs & mask) as i128, 0, mask as i128)
    };
    let exact = match op {
        OverflowOp::AddU | OverflowOp::AddS => Some(l + r),
        OverflowOp::SubU | OverflowOp::SubS => Some(l - r),
        OverflowOp::MulU | OverflowOp::MulS => l.checked_mul(r),
    };
    match exact {
        Some(v) => v < min || v > max,
        None => true,
    }
}

#[inline(always)]
pub fn int_cmp(pred: IntCmpPred, width: u8, lhs: u64, rhs: u64) -> bool {
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
pub fn float_cmp(pred: FloatCmpPred, width: u8, lhs: u64, rhs: u64) -> bool {
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
