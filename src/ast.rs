#[derive(Debug)]
pub enum Literal {
    I32(i32),
}

#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug)]
pub struct FnArg {
    name: Option<String>,
    value: Expression,
}

#[derive(Debug)]
pub struct FnCall {
    name: Ident,
    args: Vec<FnArg>,
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
    Deref(Ident),
    Block(Block)
}

#[derive(Debug)]
pub struct Block {
    exprs: Vec<Expression>
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
    name: Ident,
    args: Vec<FnArgDef>,
    ret_type: TypeExpression,
    type_args: Option<Vec<()>>,
    block: Block
}

#[derive(Debug)]
pub struct FnArgDef {
    name: Ident,
    typ: TypeExpression,
    default: Option<Expression>,
}

#[derive(Debug)]
pub enum Definition {
    FnDef(FnDef),
    ValDef(ValDef)
}

// trait TopLevelDecl {}
// impl TopLevelDecl for FnDef {}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub defs: Vec<Definition>,
}

impl Module {

}

#[cfg(test)]
mod ast_test;