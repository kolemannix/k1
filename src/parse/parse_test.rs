use crate::parse::*;

fn setup(input: &str) -> Parser {
    let mut lexer = Lexer::make(input);
    let token_vec = lexer.run();
    print_tokens(input, &token_vec[..]);
    let tokens = TokenIter::make(token_vec);
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
    assert!(matches!(
        result,
        Some(BlockStmt::ValDef(ValDef {
            value: Expression::BinaryOp(BinaryOp { operation: BinaryOpKind::Add, .. }),
            ..
        }))
    ));
    Ok(())
}

#[test]
fn infix2() -> Result<(), ParseError> {
    let mut parser = setup("val x = a + b * doStuff(1, 2)");
    let result = parser.parse_statement()?;
    if let Some(BlockStmt::ValDef(ValDef { value: Expression::BinaryOp(op), .. })) = &result {
        assert_eq!(op.operation, BinaryOpKind::Add);
        assert!(matches!(*op.operand1, Expression::Variable(_)));
        if let Expression::BinaryOp(BinaryOp { operation, operand1, operand2, .. }) = &*op.operand2
        {
            assert_eq!(*operation, BinaryOpKind::Multiply);
            assert!(matches!(**operand1, Expression::Variable(_)));
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
    assert!(matches!(result, None));
    Ok(())
}

#[test]
fn fn_args_literal() -> Result<(), String> {
    let input = "f(myarg = 42,42,\"abc\")";
    let mut parser = setup(input);
    let result = parser.parse_expression();
    if let Ok(Some(Expression::FnCall(FnCall { name, args, span }))) = result {
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

#[test]
fn if_no_else() -> ParseResult<()> {
    let input = "if x a";
    let mut parser = setup(input);
    let result = parser.parse_expression()?.unwrap();
    println!("{result:?}");
    Ok(())
}

#[test]
fn dot_accessor() -> ParseResult<()> {
    let input = "a.b.c";
    let mut parser = setup(input);
    let result = parser.parse_expression()?.unwrap();
    let Expression::FieldAccess(acc) = result else { panic!() };
    assert_eq!(acc.target.0, "c");
    let Expression::FieldAccess(acc2) = *acc.base else {panic!() };
    assert_eq!(acc2.target.0, "b");
    let Expression::Variable(v) = *acc2.base else { panic!() };
    assert_eq!(v.ident.0, "a");
    Ok(())
}
