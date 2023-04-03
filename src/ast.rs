use crate::{
    ir::IrType,
    lex::{Span, TokenKind},
};

#[derive(Debug)]
pub enum Literal {
    Numeric(String, Span),
    Bool(bool, Span),
    String(String, Span),
}

impl Literal {
    pub fn get_span(&self) -> Span {
        match self {
            Literal::Numeric(_, span) => *span,
            Literal::Bool(_, span) => *span,
            Literal::String(_, span) => *span,
        }
    }
}

#[derive(Debug)]
pub struct Ident(pub String);

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub struct FnArg {
    pub name: Option<String>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct FnCall {
    pub name: Ident,
    pub args: Vec<FnArg>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ValDef {
    pub name: Ident,
    pub typ: Option<TypeExpression>,
    pub value: Expression,
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Multiply,
    And,
    Or,
}

impl BinaryOpKind {
    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Asterisk => Some(BinaryOpKind::Multiply),
            TokenKind::KeywordAnd => Some(BinaryOpKind::And),
            TokenKind::KeywordOr => Some(BinaryOpKind::Or),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    pub operation: BinaryOpKind,
    pub operand1: Box<Expression>,
    pub operand2: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Variable {
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expression {
    BinaryOp(BinaryOp),
    Literal(Literal),
    FnCall(FnCall),
    Variable(Variable),
    Block(Block),
    If(IfExpr),
}

impl Expression {
    pub fn is_literal(e: &Expression) -> bool {
        matches!(e, Expression::Literal(_))
    }
    pub fn get_span(&self) -> Span {
        match self {
            Expression::BinaryOp(op) => op.span,
            Expression::Literal(lit) => lit.get_span(),
            Expression::FnCall(call) => call.span,
            Expression::Variable(var) => var.span,
            Expression::Block(block) => block.span,
            Expression::If(if_expr) => if_expr.span,
        }
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: Ident,
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub cons: Box<Expression>,
    // TODO: Add 'binding' Ifs, for optionals and failures
    // if some_optional { value => }
    // if get_file() { result => }
    pub alt: Option<Box<Expression>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub enum BlockStmt {
    ValDef(ValDef),
    /// return keyword will only be allowed to denote explicit early returns
    ReturnStmt(ReturnStmt),
    If(IfExpr),
    Assignment(Assignment),
    LoneExpression(Expression),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<BlockStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum TypePrimitive {
    Int,
    Bool,
}

#[derive(Debug)]
pub enum TypeExpression {
    Primitive(TypePrimitive),
}

#[derive(Debug)]
pub struct FnDef {
    pub name: Ident,
    pub args: Vec<FnArgDef>,
    pub ret_type: Option<TypeExpression>,
    pub block: Option<Block>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FnArgDef {
    pub name: Ident,
    pub typ: TypeExpression,
}

#[derive(Debug)]
pub struct ConstVal {
    pub name: Ident,
    pub typ: TypeExpression,
    pub value_expr: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(FnDef),
    Const(ConstVal),
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub defs: Vec<Definition>,
}
