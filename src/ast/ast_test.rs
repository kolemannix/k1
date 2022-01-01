use crate::ast::*;

#[test]
fn example() {
    let m = Module {
        name: Ident("module1".to_string()),
        defs: vec![Definition::FnDef(FnDef {
            name: Ident("foo".to_string()),
            args: vec![],
            ret_type: Some(TypeExpression::Primitive(TypePrimitive::I32)),
            type_args: None,
            block: None,
        })],
    };
    println!("{:?}", m);
}
