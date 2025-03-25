use crate::{
    failf,
    parse::Identifier,
    typer::{
        make_fail_span, types::TypeId, FunctionId, TypedExpr, TypedExprId, TypedFloatValue,
        TypedIntegerValue, TypedModule, TypedStmtId, TyperResult,
    },
};

pub struct Vm {}

//static_assert_size!(Value, 32);
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Boolean(bool),
    Char(u8),
    Integer(TypedIntegerValue),
    Float(TypedFloatValue),
    String(Box<str>),
    Pointer(u64),
    Buffer { type_id: TypeId, data: Vec<Value> },
    Struct { type_id: TypeId, fields: Vec<Value> },
    Enum { type_id: TypeId, variant: Identifier },
}

pub fn execute_expr(vm: &mut Vm, m: &mut TypedModule, expr: TypedExprId) -> TyperResult<()> {
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

pub fn execute_function(
    vm: &mut Vm,
    m: &mut TypedModule,
    function_id: FunctionId,
) -> TyperResult<()> {
    let function = m.get_function(function_id);
    let function_span = m.ast.get_span_for_id(function.parsed_id);
    let Some(body_id) = function.body_block else {
        return failf!(function_span, "Cannot execute function: no body");
    };
    let TypedExpr::Block(body) = m.exprs.get(body_id) else {
        return failf!(function_span, "Cannot execute function: body is not a block");
    };
    // TODO(clone): Cloning just because of the borrow
    //              Maybe EcoVec since its cheaper to copy is the way
    let stmts = body.statements.clone();
    for stmt_id in &stmts {
        execute_stmt(vm, m, *stmt_id)?;
    }

    todo!()
}

pub fn execute_stmt(vm: &mut Vm, m: &mut TypedModule, stmt_id: TypedStmtId) -> TyperResult<()> {
    match m.stmts.get(stmt_id) {
        crate::typer::TypedStmt::Expr(typed_expr_id, type_id) => {
            let e = execute_expr(vm, m, *typed_expr_id)?;
            Ok(())
        }
        crate::typer::TypedStmt::Let(let_stmt) => todo!(),
        crate::typer::TypedStmt::Assignment(assignment_stmt) => todo!(),
        crate::typer::TypedStmt::Require(typed_require_stmt) => todo!(),
    }
}
