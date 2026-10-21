// Copyright (c) 2026 knix
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

    pub fn is_zero(&self) -> bool {
        match self {
            TypedIntValue::U8(x) => *x == 0,
            TypedIntValue::U16(x) => *x == 0,
            TypedIntValue::U32(x) => *x == 0,
            TypedIntValue::U64(x) => *x == 0,
            TypedIntValue::I8(x) => *x == 0,
            TypedIntValue::I16(x) => *x == 0,
            TypedIntValue::I32(x) => *x == 0,
            TypedIntValue::I64(x) => *x == 0,
        }
    }

    pub fn incr(&self) -> Option<Self> {
        match *self {
            TypedIntValue::U8(x) => x.checked_add(1).map(TypedIntValue::U8),
            TypedIntValue::U16(x) => x.checked_add(1).map(TypedIntValue::U16),
            TypedIntValue::U32(x) => x.checked_add(1).map(TypedIntValue::U32),
            TypedIntValue::U64(x) => x.checked_add(1).map(TypedIntValue::U64),
            TypedIntValue::I8(x) => x.checked_add(1).map(TypedIntValue::I8),
            TypedIntValue::I16(x) => x.checked_add(1).map(TypedIntValue::I16),
            TypedIntValue::I32(x) => x.checked_add(1).map(TypedIntValue::I32),
            TypedIntValue::I64(x) => x.checked_add(1).map(TypedIntValue::I64),
        }
    }

    pub fn get_type(&self) -> TypeId {
        self.get_integer_type().type_id()
    }

    pub fn from_u64_bits(integer_type: IntegerType, bits: u64) -> Self {
        match integer_type {
            IntegerType::U8 => TypedIntValue::U8(bits as u8),
            IntegerType::U16 => TypedIntValue::U16(bits as u16),
            IntegerType::U32 => TypedIntValue::U32(bits as u32),
            IntegerType::U64 => TypedIntValue::U64(bits),
            IntegerType::I8 => TypedIntValue::I8(bits as i8),
            IntegerType::I16 => TypedIntValue::I16(bits as i16),
            IntegerType::I32 => TypedIntValue::I32(bits as i32),
            IntegerType::I64 => TypedIntValue::I64(bits as i64),
        }
    }

    pub fn to_zext_bits(&self) -> u64 {
        match self {
            TypedIntValue::U8(v) => *v as u64,
            TypedIntValue::U16(v) => *v as u64,
            TypedIntValue::U32(v) => *v as u64,
            TypedIntValue::U64(v) => *v,
            TypedIntValue::I8(v) => *v as u8 as u64,
            TypedIntValue::I16(v) => *v as u16 as u64,
            TypedIntValue::I32(v) => *v as u32 as u64,
            TypedIntValue::I64(v) => *v as u64,
        }
    }

    pub fn to_i128(&self) -> i128 {
        match self {
            TypedIntValue::U8(v) => *v as i128,
            TypedIntValue::U16(v) => *v as i128,
            TypedIntValue::U32(v) => *v as i128,
            TypedIntValue::U64(v) => *v as i128,
            TypedIntValue::I8(v) => *v as i128,
            TypedIntValue::I16(v) => *v as i128,
            TypedIntValue::I32(v) => *v as i128,
            TypedIntValue::I64(v) => *v as i128,
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

    pub fn scalar_bits(&self) -> (ScalarType, u64) {
        (self.get_scalar_type(), self.to_zext_bits())
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
        let (val, suffix) = (self.to_i128(), self.get_integer_type());
        if f.width().is_none() && f.precision().is_none() {
            write!(f, "{val}{suffix}")
        } else {
            f.pad(&format!("{val}{suffix}"))
        }
    }
}
