use crate::failf;
use crate::lex::SpanId;
use crate::typer::{make_fail_span, BinaryOpKind, TypedFloatValue, TypedIntegerValue, TyperResult};

use crate::vm::Value;

pub fn execute_arith_op(
    lhs: Value,
    rhs: Value,
    op: BinaryOpKind,
    span: SpanId,
) -> TyperResult<Value> {
    use BinaryOpKind as K;
    debug_assert!(matches!(op, K::Add | K::Subtract | K::Multiply | K::Divide | K::Rem));
    match (lhs, rhs) {
        (Value::Integer(iv1), Value::Integer(iv2)) => match (iv1, iv2) {
            (TypedIntegerValue::U8(v1), TypedIntegerValue::U8(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::U8(result)))
            }
            (TypedIntegerValue::U16(v1), TypedIntegerValue::U16(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::U16(result)))
            }
            (TypedIntegerValue::U32(v1), TypedIntegerValue::U32(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::U32(result)))
            }
            (TypedIntegerValue::U64(v1), TypedIntegerValue::U64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::U64(result)))
            }
            (TypedIntegerValue::I8(v1), TypedIntegerValue::I8(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::I8(result)))
            }
            (TypedIntegerValue::I16(v1), TypedIntegerValue::I16(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::I16(result)))
            }
            (TypedIntegerValue::I32(v1), TypedIntegerValue::I32(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::I32(result)))
            }
            (TypedIntegerValue::I64(v1), TypedIntegerValue::I64(v2)) => {
                let result = match op {
                    K::Add => v1 + v2,
                    K::Subtract => v1 - v2,
                    K::Multiply => v1 * v2,
                    K::Divide => v1 / v2,
                    K::Rem => v1 % v2,
                    _ => unreachable!(),
                };
                Ok(Value::Integer(TypedIntegerValue::I64(result)))
            }
            _ => failf!(span, "Invalid integer operation: {:?} {:?}", iv1, iv2),
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
                Ok(Value::Float(TypedFloatValue::F32(result)))
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
                Ok(Value::Float(TypedFloatValue::F64(result)))
            }
            _ => unreachable!(),
        },
        (a, b) => failf!(span, "Malformed binop: {:?} {:?}", a, b),
    }
}

pub fn execute_cmp_op(
    lhs: Value,
    rhs: Value,
    op: BinaryOpKind,
    span: SpanId,
) -> TyperResult<Value> {
    use BinaryOpKind as K;
    debug_assert!(matches!(op, K::Less | K::LessEqual | K::Greater | K::GreaterEqual));
    match (lhs, rhs) {
        (Value::Integer(iv1), Value::Integer(iv2)) => match (iv1, iv2) {
            (TypedIntegerValue::U8(v1), TypedIntegerValue::U8(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::U16(v1), TypedIntegerValue::U16(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::U32(v1), TypedIntegerValue::U32(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::U64(v1), TypedIntegerValue::U64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::I8(v1), TypedIntegerValue::I8(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::I16(v1), TypedIntegerValue::I16(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::I32(v1), TypedIntegerValue::I32(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            (TypedIntegerValue::I64(v1), TypedIntegerValue::I64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            _ => failf!(span, "Invalid integer operation: {:?} {:?}", iv1, iv2),
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
                Ok(Value::Bool(result))
            }
            (TypedFloatValue::F64(v1), TypedFloatValue::F64(v2)) => {
                let result = match op {
                    K::Less => v1 < v2,
                    K::LessEqual => v1 <= v2,
                    K::Greater => v1 > v2,
                    K::GreaterEqual => v1 >= v2,
                    _ => unreachable!(),
                };
                Ok(Value::Bool(result))
            }
            _ => unreachable!(),
        },
        (a, b) => failf!(span, "Malformed binop: {:?} {:?}", a, b),
    }
}
