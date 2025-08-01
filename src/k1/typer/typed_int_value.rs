use std::fmt::{Display, Formatter};

use super::{IntegerType, TypeId, WordSize};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedIntValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    UWord32(u32),
    UWord64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    IWord32(i32),
    IWord64(i64),
}

#[macro_export]
macro_rules! int_binop {
    ($self:expr, $other:expr, $method:ident) => {
        match ($self, $other) {
            (TypedIntValue::U8(a), TypedIntValue::U8(b)) => TypedIntValue::U8(a.$method(*b)),
            (TypedIntValue::U16(a), TypedIntValue::U16(b)) => TypedIntValue::U16(a.$method(*b)),
            (TypedIntValue::U32(a), TypedIntValue::U32(b)) => TypedIntValue::U32(a.$method(*b)),
            (TypedIntValue::U64(a), TypedIntValue::U64(b)) => TypedIntValue::U64(a.$method(*b)),
            (TypedIntValue::UWord32(a), TypedIntValue::UWord32(b)) => {
                TypedIntValue::UWord32(a.$method(*b))
            }
            (TypedIntValue::UWord64(a), TypedIntValue::UWord64(b)) => {
                TypedIntValue::UWord64(a.$method(*b))
            }
            (TypedIntValue::I8(a), TypedIntValue::I8(b)) => TypedIntValue::I8(a.$method(*b)),
            (TypedIntValue::I16(a), TypedIntValue::I16(b)) => TypedIntValue::I16(a.$method(*b)),
            (TypedIntValue::I32(a), TypedIntValue::I32(b)) => TypedIntValue::I32(a.$method(*b)),
            (TypedIntValue::I64(a), TypedIntValue::I64(b)) => TypedIntValue::I64(a.$method(*b)),
            (TypedIntValue::IWord32(a), TypedIntValue::IWord32(b)) => {
                TypedIntValue::IWord32(a.$method(*b))
            }
            (TypedIntValue::IWord64(a), TypedIntValue::IWord64(b)) => {
                TypedIntValue::IWord64(a.$method(*b))
            }
            _ => panic!("mismatched int binop types"),
        }
    };
}

impl TypedIntValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            TypedIntValue::U8(_) => "u8",
            TypedIntValue::U16(_) => "u16",
            TypedIntValue::U32(_) => "u32",
            TypedIntValue::U64(_) => "u64",
            TypedIntValue::UWord32(_) => "uword",
            TypedIntValue::UWord64(_) => "uword",
            TypedIntValue::I8(_) => "i8",
            TypedIntValue::I16(_) => "i16",
            TypedIntValue::I32(_) => "i32",
            TypedIntValue::I64(_) => "i64",
            TypedIntValue::IWord32(_) => "iword",
            TypedIntValue::IWord64(_) => "iword",
        }
    }

    pub fn get_type(&self) -> TypeId {
        self.get_integer_type().type_id()
    }

    pub fn bit_not(&self) -> Self {
        use std::ops::Not;

        match *self {
            TypedIntValue::U8(x) => TypedIntValue::U8(x.not()),
            TypedIntValue::U16(x) => TypedIntValue::U16(x.not()),
            TypedIntValue::U32(x) => TypedIntValue::U32(x.not()),
            TypedIntValue::U64(x) => TypedIntValue::U64(x.not()),
            TypedIntValue::UWord32(x) => TypedIntValue::UWord32(x.not()),
            TypedIntValue::UWord64(x) => TypedIntValue::UWord64(x.not()),
            TypedIntValue::I8(x) => TypedIntValue::I8(x.not()),
            TypedIntValue::I16(x) => TypedIntValue::I16(x.not()),
            TypedIntValue::I32(x) => TypedIntValue::I32(x.not()),
            TypedIntValue::I64(x) => TypedIntValue::I64(x.not()),
            TypedIntValue::IWord32(x) => TypedIntValue::IWord32(x.not()),
            TypedIntValue::IWord64(x) => TypedIntValue::IWord64(x.not()),
        }
    }

    pub fn bit_and(&self, other: &Self) -> Self {
        use std::ops::BitAnd;
        int_binop!(self, other, bitand)
    }

    pub fn bit_or(&self, other: &Self) -> Self {
        use std::ops::BitOr;
        int_binop!(self, other, bitor)
    }

    pub fn bit_xor(&self, other: &Self) -> Self {
        use std::ops::BitXor;
        int_binop!(self, other, bitxor)
    }

    pub fn to_u64_unconditional(&self) -> u64 {
        match self {
            TypedIntValue::U8(v) => *v as u64,
            TypedIntValue::U16(v) => *v as u64,
            TypedIntValue::U32(v) => *v as u64,
            TypedIntValue::U64(v) => *v,
            TypedIntValue::UWord32(v) => *v as u64,
            TypedIntValue::UWord64(v) => *v,
            TypedIntValue::I8(v) => *v as u64,
            TypedIntValue::I16(v) => *v as u64,
            TypedIntValue::I32(v) => *v as u64,
            TypedIntValue::I64(v) => *v as u64,
            TypedIntValue::IWord32(v) => *v as u64,
            TypedIntValue::IWord64(v) => *v as u64,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u32(&self) -> u32 {
        match self {
            TypedIntValue::U8(v) => *v as u32,
            TypedIntValue::U16(v) => *v as u32,
            TypedIntValue::U32(v) => *v,
            TypedIntValue::U64(v) => *v as u32,
            TypedIntValue::UWord32(v) => *v,
            TypedIntValue::UWord64(v) => *v as u32,
            TypedIntValue::I8(v) => *v as u32,
            TypedIntValue::I16(v) => *v as u32,
            TypedIntValue::I32(v) => *v as u32,
            TypedIntValue::I64(v) => *v as u32,
            TypedIntValue::IWord32(v) => *v as u32,
            TypedIntValue::IWord64(v) => *v as u32,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u16(&self) -> u16 {
        match self {
            TypedIntValue::U8(v) => *v as u16,
            TypedIntValue::U16(v) => *v,
            TypedIntValue::U32(v) => *v as u16,
            TypedIntValue::U64(v) => *v as u16,
            TypedIntValue::UWord32(v) => *v as u16,
            TypedIntValue::UWord64(v) => *v as u16,
            TypedIntValue::I8(v) => *v as u16,
            TypedIntValue::I16(v) => *v as u16,
            TypedIntValue::I32(v) => *v as u16,
            TypedIntValue::I64(v) => *v as u16,
            TypedIntValue::IWord32(v) => *v as u16,
            TypedIntValue::IWord64(v) => *v as u16,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u8(&self) -> u8 {
        match self {
            TypedIntValue::U8(v) => *v,
            TypedIntValue::U16(v) => *v as u8,
            TypedIntValue::U32(v) => *v as u8,
            TypedIntValue::U64(v) => *v as u8,
            TypedIntValue::UWord32(v) => *v as u8,
            TypedIntValue::UWord64(v) => *v as u8,
            TypedIntValue::I8(v) => *v as u8,
            TypedIntValue::I16(v) => *v as u8,
            TypedIntValue::I32(v) => *v as u8,
            TypedIntValue::I64(v) => *v as u8,
            TypedIntValue::IWord32(v) => *v as u8,
            TypedIntValue::IWord64(v) => *v as u8,
        }
    }

    pub fn to_i32(&self) -> i32 {
        match self {
            TypedIntValue::U8(v) => *v as i32,
            TypedIntValue::U16(v) => *v as i32,
            TypedIntValue::U32(v) => *v as i32,
            TypedIntValue::U64(v) => *v as i32,
            TypedIntValue::UWord32(v) => *v as i32,
            TypedIntValue::UWord64(v) => *v as i32,
            TypedIntValue::I8(v) => *v as i32,
            TypedIntValue::I16(v) => *v as i32,
            TypedIntValue::I32(v) => *v,
            TypedIntValue::I64(v) => *v as i32,
            TypedIntValue::IWord32(v) => *v,
            TypedIntValue::IWord64(v) => *v as i32,
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            TypedIntValue::U8(v) => *v as i64,
            TypedIntValue::U16(v) => *v as i64,
            TypedIntValue::U32(v) => *v as i64,
            TypedIntValue::U64(v) => *v as i64,
            TypedIntValue::UWord32(v) => *v as i64,
            TypedIntValue::UWord64(v) => *v as i64,
            TypedIntValue::I8(v) => *v as i64,
            TypedIntValue::I16(v) => *v as i64,
            TypedIntValue::I32(v) => *v as i64,
            TypedIntValue::I64(v) => *v,
            TypedIntValue::IWord32(v) => *v as i64,
            TypedIntValue::IWord64(v) => *v,
        }
    }

    pub fn get_integer_type(&self) -> IntegerType {
        match self {
            TypedIntValue::U8(_) => IntegerType::U8,
            TypedIntValue::U16(_) => IntegerType::U16,
            TypedIntValue::U32(_) => IntegerType::U32,
            TypedIntValue::U64(_) => IntegerType::U64,
            TypedIntValue::UWord32(_) => IntegerType::UWord(WordSize::W32),
            TypedIntValue::UWord64(_) => IntegerType::UWord(WordSize::W64),
            TypedIntValue::I8(_) => IntegerType::I8,
            TypedIntValue::I16(_) => IntegerType::I16,
            TypedIntValue::I32(_) => IntegerType::I32,
            TypedIntValue::I64(_) => IntegerType::I64,
            TypedIntValue::IWord32(_) => IntegerType::IWord(WordSize::W32),
            TypedIntValue::IWord64(_) => IntegerType::IWord(WordSize::W64),
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            TypedIntValue::I8(_)
            | TypedIntValue::I16(_)
            | TypedIntValue::I32(_)
            | TypedIntValue::I64(_)
            | TypedIntValue::IWord32(_)
            | TypedIntValue::IWord64(_) => true,
            TypedIntValue::U8(_)
            | TypedIntValue::U16(_)
            | TypedIntValue::U32(_)
            | TypedIntValue::U64(_)
            | TypedIntValue::UWord32(_)
            | TypedIntValue::UWord64(_) => false,
        }
    }

    pub fn expect_uword(&self) -> usize {
        match self {
            TypedIntValue::UWord64(v) => *v as usize,
            TypedIntValue::UWord32(v) => *v as usize,
            _ => unreachable!(),
        }
    }

    pub fn expect_u64(&self) -> u64 {
        match self {
            TypedIntValue::U64(v) => *v,
            _ => unreachable!(),
        }
    }

    pub fn as_uword(&self) -> Option<usize> {
        match self {
            TypedIntValue::UWord64(v) => Some(*v as usize),
            TypedIntValue::UWord32(v) => Some(*v as usize),
            _ => None,
        }
    }
}

impl From<u64> for TypedIntValue {
    fn from(value: u64) -> Self {
        TypedIntValue::U64(value)
    }
}
impl From<u32> for TypedIntValue {
    fn from(value: u32) -> Self {
        TypedIntValue::U32(value)
    }
}
impl From<u16> for TypedIntValue {
    fn from(value: u16) -> Self {
        TypedIntValue::U16(value)
    }
}
impl From<u8> for TypedIntValue {
    fn from(value: u8) -> Self {
        TypedIntValue::U8(value)
    }
}
impl From<i64> for TypedIntValue {
    fn from(value: i64) -> Self {
        TypedIntValue::I64(value)
    }
}
impl From<i32> for TypedIntValue {
    fn from(value: i32) -> Self {
        TypedIntValue::I32(value)
    }
}
impl From<i16> for TypedIntValue {
    fn from(value: i16) -> Self {
        TypedIntValue::I16(value)
    }
}
impl From<i8> for TypedIntValue {
    fn from(value: i8) -> Self {
        TypedIntValue::I8(value)
    }
}

impl Display for TypedIntValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedIntValue::U8(v) => write!(f, "{}u8", v),
            TypedIntValue::U16(v) => write!(f, "{}u16", v),
            TypedIntValue::U32(v) => write!(f, "{}u32", v),
            TypedIntValue::U64(v) => write!(f, "{}u64", v),
            TypedIntValue::UWord32(v) => write!(f, "{}uword", v),
            TypedIntValue::UWord64(v) => write!(f, "{}uword", v),
            TypedIntValue::I8(v) => write!(f, "{}i8", v),
            TypedIntValue::I16(v) => write!(f, "{}i16", v),
            TypedIntValue::I32(v) => write!(f, "{}i32", v),
            TypedIntValue::I64(v) => write!(f, "{}i64", v),
            TypedIntValue::IWord32(v) => write!(f, "{}iword", v),
            TypedIntValue::IWord64(v) => write!(f, "{}iword", v),
        }
    }
}
