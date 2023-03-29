use crate::lex::TokenKind;

#[derive(Debug)]
pub enum Literal {
    Numeric(String),
    Bool(bool),
    String(String),
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
}

#[derive(Debug)]
pub struct ValDef {
    pub name: Ident,
    pub typ: Option<TypeExpression>,
    pub value: Expression,
    pub is_mutable: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Mult,
}

impl BinaryOpKind {
    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Asterisk => Some(BinaryOpKind::Mult),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    pub operation: BinaryOpKind,
    pub operand1: Box<Expression>,
    pub operand2: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    BinaryOp(BinaryOp),
    Literal(Literal),
    FnCall(FnCall),
    Variable(Ident),
    Block(Block),
}

impl Expression {
    pub fn is_literal(e: &Expression) -> bool {
        matches!(e, Expression::Literal(_))
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: Ident,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Expression,
    // TODO: Add var binding; cons is more like a lambda syntactically
    pub cons: Expression,
    pub alt: Option<Expression>,
}

#[derive(Debug)]
pub enum BlockStmt {
    ValDef(ValDef),
    /// return keyword will only be allowed to denote explicit early returns
    ReturnStmt(Expression),
    If(IfExpr),
    Assignment(Assignment),
    LoneExpression(Expression),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<BlockStmt>,
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
    pub value: Expression,
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
