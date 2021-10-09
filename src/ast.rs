#[derive(Debug)]
pub enum Literal {
    I32(i32),
}

#[derive(Debug)]
pub struct Ident(pub String);

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
}

#[derive(Debug)]
pub struct MutDef {
    pub name: Ident,
    pub typ: Option<TypeExpression>,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    FnCall(FnCall),
    Variable(Ident),
    // Block(Block),
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: Ident,
    pub expr: Expression,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Expression,
    pub a: Expression,
    pub b: Option<Expression>
}

#[derive(Debug)]
pub enum BlockStmt {
    MutDef(MutDef),
    ValDef(ValDef),
    Assignment(Assignment),
    /// return keyword will only be allowed to denote explicit early returns
    ReturnStmt(Expression),
    If(IfExpr),
    LoneExpression(Expression),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<BlockStmt>,
}

#[derive(Debug)]
pub enum TypePrimitive {
    I32,
    I64,
    F32,
    F64,
    Char,
}

#[derive(Debug)]
pub enum TypeExpression {
    Primitive(TypePrimitive)
}

#[derive(Debug)]
pub struct FnDef {
    pub name: Ident,
    pub args: Vec<FnArgDef>,
    pub ret_type: Option<TypeExpression>,
    pub type_args: Option<Vec<()>>,
    pub block: Block,
}

#[derive(Debug)]
pub struct FnArgDef {
    pub name: Ident,
    pub typ: TypeExpression,
    pub default: Option<Expression>,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(FnDef),
    ValDef(ValDef),
}

// trait TopLevelDecl {}
// impl TopLevelDecl for FnDef {}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub defs: Vec<Definition>,
}

impl Module {}

#[cfg(test)]
mod ast_test;