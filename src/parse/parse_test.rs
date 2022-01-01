use crate::ast::*;
use crate::parse::*;
use std::ops::Deref;

#[test]
fn hello_world() -> Result<(), String> {
    let parse_result = parse_file("resources/nixsample/hello_world.nx")?;
    println!("{:?}", parse_result);
    assert_eq!(parse_result.name.0, "module1".to_string());
    Ok(())
}

#[test]
fn basic_fn() -> Result<(), String> {
    let Module { name, defs } = parse_file("resources/nixsample/basic_fn.nx")?;
    assert_eq!(name.0, "module1".to_string());
    if let Some(Definition::FnDef(fndef)) = defs.first() {
        println!("def {:?}", fndef);
        assert_eq!(fndef.name.0, "basic")
    } else {
        println!("defs {:?}", defs);
        panic!("no definitions for basic_fn")
    }
    Ok(())
}

#[test]
fn fn_args_recurse() -> Result<(), String> {
    let input = "f(42,42,42)";
    let mut lexer = Lexer::make(&input);
    let token_vec = crate::lex::tokenize(&mut lexer);
    let tokens = Tokens::make(token_vec);
    let mut parser = Parser::make(tokens, input);
    let result = parser.parse_expression();
    if let Ok(Some(Expression::FnCall(FnCall { name, args }))) = result {
        assert_eq!(name.0, "f");
        assert_eq!(args.len(), 3);
    } else {
        panic!("fail")
    }

    Ok(())
}
