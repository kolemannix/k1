use crate::typer::{BinaryOpKind, TypedFloatValue, TypedIntValue};

use crate::vm::Value;

pub fn execute_arith_op(lhs: Value, rhs: Value, op: BinaryOpKind) -> Value {
    use BinaryOpKind as K;
    debug_assert!(matches!(op, K::Add | K::Subtract | K::Multiply | K::Divide | K::Rem));
    match (lhs, rhs) {
        (Value::Int(iv1), Value::Int(iv2)) => match (iv1, iv2) {
            (TypedIntValue::U8(v1), TypedIntValue::U8(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::U8(result))
            }
            (TypedIntValue::U16(v1), TypedIntValue::U16(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::U16(result))
            }
            (TypedIntValue::U32(v1), TypedIntValue::U32(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::U32(result))
            }
            (TypedIntValue::U64(v1), TypedIntValue::U64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::U64(result))
            }
            (TypedIntValue::UWord64(v1), TypedIntValue::UWord64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::UWord64(result))
            }
            (TypedIntValue::I8(v1), TypedIntValue::I8(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::I8(result))
            }
            (TypedIntValue::I16(v1), TypedIntValue::I16(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::I16(result))
            }
            (TypedIntValue::I32(v1), TypedIntValue::I32(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::I32(result))
            }
            (TypedIntValue::I64(v1), TypedIntValue::I64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::I64(result))
            }
            (TypedIntValue::IWord64(v1), TypedIntValue::IWord64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Int(TypedIntValue::IWord64(result))
            }
            _ => unreachable!("Invalid integer operation: {:?} {:?}", iv1, iv2),
        },
        (Value::Float(fv1), Value::Float(fv2)) => match (fv1, fv2) {
            (TypedFloatValue::F32(v1), TypedFloatValue::F32(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Float(TypedFloatValue::F32(result))
            }
            (TypedFloatValue::F64(v1), TypedFloatValue::F64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Value::Float(TypedFloatValue::F64(result))
            }
            _ => unreachable!(),
        },
        (a, b) => unreachable!("Malformed binop: {:?} {:?}", a, b),
    }
}

pub fn execute_cmp_op(lhs: Value, rhs: Value, op: BinaryOpKind) -> Value {
    use BinaryOpKind as K;
    debug_assert!(matches!(op, K::Less | K::LessEqual | K::Greater | K::GreaterEqual));
    match (lhs, rhs) {
        (Value::Int(iv1), Value::Int(iv2)) => match (iv1, iv2) {
            (TypedIntValue::U8(v1), TypedIntValue::U8(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::U16(v1), TypedIntValue::U16(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::U32(v1), TypedIntValue::U32(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::U64(v1), TypedIntValue::U64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::UWord64(v1), TypedIntValue::UWord64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::I8(v1), TypedIntValue::I8(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::I16(v1), TypedIntValue::I16(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::I32(v1), TypedIntValue::I32(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::I64(v1), TypedIntValue::I64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedIntValue::IWord64(v1), TypedIntValue::IWord64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            _ => unreachable!("Invalid integer cmp operation: {:?} {:?}", iv1, iv2),
        },
        (Value::Float(fv1), Value::Float(fv2)) => match (fv1, fv2) {
            (TypedFloatValue::F32(v1), TypedFloatValue::F32(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            (TypedFloatValue::F64(v1), TypedFloatValue::F64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Value::Bool(result)
            }
            _ => unreachable!(),
        },
        (a, b) => unreachable!("Malformed binop: {:?} {:?}", a, b),
    }
}
