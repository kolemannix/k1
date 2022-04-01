use crate::parse::*;

// #[test]
// fn hello_world() -> Result<(), ParseError> {
//     let parse_result = parse_file("resources/nexsample/hello_world.nx")?;
//     println!("{:?}", parse_result);
//     assert_eq!(parse_result.name.0, "resources/nexsample/hello_world.nx".to_string());
//     Ok(())
// }

#[test]
fn basic_fn() -> Result<(), ParseError> {
    let Module { name, defs } = parse_file("resources/nexsample/basic_fn.nx")?;
    assert_eq!(name.0, "resources/nexsample/basic_fn.nx".to_string());
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
fn fn_args_literal() -> Result<(), String> {
    let input = "f(myarg = 42,42,\"abc\")";
    let mut lexer = Lexer::make(&input);
    let token_vec = crate::lex::tokenize(&mut lexer);
    let tokens = Tokens::make(token_vec);
    let mut parser = Parser::make(tokens, input);
    let result = parser.parse_expression();
    if let Ok(Some(Expression::FnCall(FnCall { name, args }))) = result {
        println!("Parsed: {}, {:?}", name.0, args);
        assert_eq!(name.0, "f");
        assert_eq!(args.len(), 3);
    } else {
        panic!("fail");
    }

    Ok(())
}
