use crate::ast::Ident;

type Index = u32;

pub enum Ref {
    IntType,
    StringType,
    Instr(Index),
}

// pub struct MutDef {
//     pub name: Ident,
//     pub typ: TypeExpression,
//     pub value: Expression,
// }
pub enum Instr {
    Add(Ref, Ref),
    Block { instrs: Vec<Index> },
    Func { param_block: Index },
}
