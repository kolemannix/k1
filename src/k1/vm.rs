use crate::typer::{FunctionId, TypedExpr, TypedExprId, TypedModule, TyperResult};

pub struct Vm {

}

pub fn execute_expr(m: &mut TypedModule, expr: TypedExprId) -> TyperResult<()> {
    match m.exprs.get(expr) {
        TypedExpr::Unit(span_id) => todo!(),
        TypedExpr::Char(_, span_id) => todo!(),
        TypedExpr::Bool(_, span_id) => todo!(),
        TypedExpr::Integer(typed_integer_expr) => todo!(),
        TypedExpr::Float(typed_float_expr) => todo!(),
        TypedExpr::String(_, span_id) => todo!(),
        TypedExpr::Struct(_) => todo!(),
        TypedExpr::StructFieldAccess(field_access) => todo!(),
        TypedExpr::Variable(variable_expr) => todo!(),
        TypedExpr::UnaryOp(unary_op) => todo!(),
        TypedExpr::BinaryOp(binary_op) => todo!(),
        TypedExpr::Block(typed_block) => todo!(),
        TypedExpr::Call(call) => todo!(),
        TypedExpr::Match(typed_match_expr) => todo!(),
        TypedExpr::WhileLoop(while_loop) => todo!(),
        TypedExpr::LoopExpr(loop_expr) => todo!(),
        TypedExpr::EnumConstructor(typed_enum_constructor) => todo!(),
        TypedExpr::EnumIsVariant(typed_enum_is_variant_expr) => todo!(),
        TypedExpr::EnumGetPayload(get_enum_payload) => todo!(),
        TypedExpr::Cast(typed_cast) => todo!(),
        TypedExpr::Return(typed_return) => todo!(),
        TypedExpr::Break(typed_break) => todo!(),
        TypedExpr::Lambda(lambda_expr) => todo!(),
        TypedExpr::FunctionReference(function_reference_expr) => todo!(),
        TypedExpr::FunctionToLambdaObject(function_to_lambda_object_expr) => todo!(),
        TypedExpr::PendingCapture(pending_capture_expr) => todo!(),
    }
}

pub fn execute_function(m: &mut TypedModule, function_id: FunctionId) -> TyperResult<()> {
    let function = m.get_function(function_id);
    let Some(body) = function.body_block else {
        return failf!()
    }
}
