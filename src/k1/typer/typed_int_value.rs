// Copyright (c) 2025 knix
// All rights reserved.

use std::fmt::{Display, Formatter};

use crate::typer::types::ScalarType;

use super::{IntegerType, TypeId};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedIntValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl TypedIntValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            TypedIntValue::U8(_) => "u8",
            TypedIntValue::U16(_) => "u16",
            TypedIntValue::U32(_) => "u32",
            TypedIntValue::U64(_) => "u64",
            TypedIntValue::I8(_) => "i8",
            TypedIntValue::I16(_) => "i16",
            TypedIntValue::I32(_) => "i32",
            TypedIntValue::I64(_) => "i64",
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
            TypedIntValue::I8(x) => TypedIntValue::I8(x.not()),
            TypedIntValue::I16(x) => TypedIntValue::I16(x.not()),
            TypedIntValue::I32(x) => TypedIntValue::I32(x.not()),
            TypedIntValue::I64(x) => TypedIntValue::I64(x.not()),
        }
    }

    pub fn to_u64_bits(&self) -> u64 {
        match self {
            TypedIntValue::U8(v) => *v as u64,
            TypedIntValue::U16(v) => *v as u64,
            TypedIntValue::U32(v) => *v as u64,
            TypedIntValue::U64(v) => *v,
            TypedIntValue::I8(v) => *v as u64,
            TypedIntValue::I16(v) => *v as u64,
            TypedIntValue::I32(v) => *v as u64,
            TypedIntValue::I64(v) => *v as u64,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u32(&self) -> u32 {
        match self {
            TypedIntValue::U8(v) => *v as u32,
            TypedIntValue::U16(v) => *v as u32,
            TypedIntValue::U32(v) => *v,
            TypedIntValue::U64(v) => *v as u32,
            TypedIntValue::I8(v) => *v as u32,
            TypedIntValue::I16(v) => *v as u32,
            TypedIntValue::I32(v) => *v as u32,
            TypedIntValue::I64(v) => *v as u32,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u16(&self) -> u16 {
        match self {
            TypedIntValue::U8(v) => *v as u16,
            TypedIntValue::U16(v) => *v,
            TypedIntValue::U32(v) => *v as u16,
            TypedIntValue::U64(v) => *v as u16,
            TypedIntValue::I8(v) => *v as u16,
            TypedIntValue::I16(v) => *v as u16,
            TypedIntValue::I32(v) => *v as u16,
            TypedIntValue::I64(v) => *v as u16,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u8(&self) -> u8 {
        match self {
            TypedIntValue::U8(v) => *v,
            TypedIntValue::U16(v) => *v as u8,
            TypedIntValue::U32(v) => *v as u8,
            TypedIntValue::U64(v) => *v as u8,
            TypedIntValue::I8(v) => *v as u8,
            TypedIntValue::I16(v) => *v as u8,
            TypedIntValue::I32(v) => *v as u8,
            TypedIntValue::I64(v) => *v as u8,
        }
    }

    pub fn to_i32(&self) -> i32 {
        match self {
            TypedIntValue::U8(v) => *v as i32,
            TypedIntValue::U16(v) => *v as i32,
            TypedIntValue::U32(v) => *v as i32,
            TypedIntValue::U64(v) => *v as i32,
            TypedIntValue::I8(v) => *v as i32,
            TypedIntValue::I16(v) => *v as i32,
            TypedIntValue::I32(v) => *v,
            TypedIntValue::I64(v) => *v as i32,
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            TypedIntValue::U8(v) => *v as i64,
            TypedIntValue::U16(v) => *v as i64,
            TypedIntValue::U32(v) => *v as i64,
            TypedIntValue::U64(v) => *v as i64,
            TypedIntValue::I8(v) => *v as i64,
            TypedIntValue::I16(v) => *v as i64,
            TypedIntValue::I32(v) => *v as i64,
            TypedIntValue::I64(v) => *v,
        }
    }

    pub fn get_integer_type(&self) -> IntegerType {
        match self {
            TypedIntValue::U8(_) => IntegerType::U8,
            TypedIntValue::U16(_) => IntegerType::U16,
            TypedIntValue::U32(_) => IntegerType::U32,
            TypedIntValue::U64(_) => IntegerType::U64,
            TypedIntValue::I8(_) => IntegerType::I8,
            TypedIntValue::I16(_) => IntegerType::I16,
            TypedIntValue::I32(_) => IntegerType::I32,
            TypedIntValue::I64(_) => IntegerType::I64,
        }
    }

    pub fn get_scalar_type(&self) -> ScalarType {
        self.get_integer_type().get_scalar_type()
    }

    pub fn is_signed(&self) -> bool {
        match self {
            TypedIntValue::I8(_)
            | TypedIntValue::I16(_)
            | TypedIntValue::I32(_)
            | TypedIntValue::I64(_) => true,
            TypedIntValue::U8(_)
            | TypedIntValue::U16(_)
            | TypedIntValue::U32(_)
            | TypedIntValue::U64(_) => false,
        }
    }

    pub fn expect_u64(&self) -> u64 {
        match self {
            TypedIntValue::U64(v) => *v,
            _ => unreachable!(),
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
            TypedIntValue::I8(v) => write!(f, "{}i8", v),
            TypedIntValue::I16(v) => write!(f, "{}i16", v),
            TypedIntValue::I32(v) => write!(f, "{}i32", v),
            TypedIntValue::I64(v) => write!(f, "{}i64", v),
        }
    }
}
