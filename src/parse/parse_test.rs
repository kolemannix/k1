use crate::parse::*;

// #[test]
// fn hello_world() -> Result<(), ParseError> {
//     let parse_result = parse_file("resources/test_src/hello_world.nx")?;
//     println!("{:?}", parse_result);
//     assert_eq!(parse_result.name.0, "resources/test_src/hello_world.nx".to_string());
//     Ok(())
// }

fn setup(input: &str) -> Parser {
    let mut lexer = Lexer::make(&input);
    let token_vec = lexer.run();
    print_tokens(input, &token_vec[..]);
    let tokens = Tokens::make(token_vec);
    Parser::make(tokens, input)
}

#[test]
fn basic_fn() -> Result<(), ParseError> {
    let src = r#"
    fn basic(x: Int, y: Int): Int {
      println(42, 42, 42);
      val x: Int = 0;
      mut y: Int = 1;
      y = { 1; 2; 3 };
      y = add(42, 42);
      return add(x, y);
    }"#;
    let Module { name, defs } = parse_text(src, "basic_fn.nx")?;
    println!("defs {:?}", defs);
    assert_eq!(name.0, "basic_fn.nx".to_string());
    if let Some(Definition::FnDef(fndef)) = defs.first() {
        assert_eq!(fndef.name.0, "basic")
    } else {
        panic!("no definitions for basic_fn")
    }
    Ok(())
}

#[test]
fn infix1() -> Result<(), ParseError> {
    let mut parser = setup("val x = a + b");
    let result = parser.parse_statement()?;
    println!("{:?}", result);
    assert!(matches!(
        result,
        Some(BlockStmt::ValDef(ValDef { value: Expression::InfixOp(InfixOp { operation: InfixOpKind::Add, .. }), .. }))
    ));
    Ok(())
}

#[test]
fn infix2() -> Result<(), ParseError> {
    let mut parser = setup("val x = a + b * doStuff(1, 2)");
    let result = parser.parse_statement()?;
    println!("{:?}", result);
    if let Some(BlockStmt::ValDef(ValDef { value: Expression::InfixOp(op), .. })) = &result {
        assert_eq!(op.operation, InfixOpKind::Add);
        assert!(matches!(*op.operand1, Expression::Variable(Ident(_))));
        if let Expression::InfixOp(InfixOp { operation, operand1, operand2 }) = &*op.operand2 {
            assert_eq!(*operation, InfixOpKind::Mult);
            assert!(matches!(**operand1, Expression::Variable(Ident(_))));
            assert!(matches!(**operand2, Expression::FnCall(_)));
        } else {
            panic!("Expected nested infix ops; got {:?}", result);
        }
    } else {
        panic!("Expected nested infix ops; got {:?}", result);
    }
    Ok(())
}

#[test]
fn parse_eof() -> Result<(), ParseError> {
    let mut parser = setup("");
    let result = parser.parse_expression()?;
    println!("{:?}", result);
    assert!(matches!(result, None));
    Ok(())
}

#[test]
fn fn_args_literal() -> Result<(), String> {
    let input = "f(myarg = 42,42,\"abc\")";
    let mut lexer = Lexer::make(&input);
    let token_vec = lexer.run();
    let tokens = Tokens::make(token_vec);
    let mut parser = Parser::make(tokens, input);
    let result = parser.parse_expression();
    if let Ok(Some(Expression::FnCall(FnCall { name, args }))) = result {
        println!("Parsed: {}, {:?}", name.0, args);
        assert_eq!(name.0, "f");
        assert_eq!(args[0].name.as_deref(), Some("myarg"));
        assert!(Expression::is_literal(&args[0].value));
        assert!(Expression::is_literal(&args[1].value));
        assert!(Expression::is_literal(&args[2].value));
        assert_eq!(args.len(), 3);
    } else {
        panic!("fail");
    }

    Ok(())
}
