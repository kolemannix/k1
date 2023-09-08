use crate::parse::*;

#[cfg(test)]
fn set_up(input: &str) -> Parser<'static> {
    let mut lexer = Lexer::make(input);
    let token_vec: &'static mut [Token] = lexer.run().leak();
    print_tokens(input, token_vec);
    let parser = Parser::make(token_vec, input.to_string(), false);
    parser
}

#[test]
fn basic_fn() -> Result<(), ParseError> {
    let src = r#"
    fn basic(x: int, y: int): int {
      println(42, 42, 42);
      val x: int = 0;
      mut y: int = 1;
      y = { 1; 2; 3 };
      y = add(42, 42);
      return add(x, y);
    }"#;
    let module = parse_text(src, "basic_fn.nx", false)?;
    assert_eq!(&module.name, "basic_fn.nx");
    if let Some(Definition::FnDef(fndef)) = module.defs.first() {
        assert_eq!(*module.get_ident_name(fndef.name), *"basic")
    } else {
        println!("defs {:?}", module.defs);
        panic!("no definitions for basic_fn")
    }
    Ok(())
}

#[test]
fn infix1() -> Result<(), ParseError> {
    let mut parser = set_up("val x = a + b");
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
    let mut parser = set_up("val x = a + b * doStuff(1, 2)");
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
    let mut parser = set_up("");
    let result = parser.parse_expression()?;
    assert!(matches!(result, None));
    Ok(())
}

#[test]
fn fn_args_literal() -> Result<(), String> {
    let input = "f(myarg = 42,42,\"abc\")";
    let mut parser = set_up(input);
    let result = parser.parse_expression();
    if let Ok(Some(Expression::FnCall(fn_call))) = result {
        let args = &fn_call.args;
        assert_eq!(&*parser.get_ident_name(fn_call.name), "f");
        assert_eq!(&*parser.get_ident_name(args[0].name.unwrap()), "myarg");
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
    let mut parser = set_up(input);
    let result = parser.parse_expression()?.unwrap();
    println!("{result:?}");
    Ok(())
}

#[test]
fn dot_accessor() -> ParseResult<()> {
    let input = "a.b.c";
    let mut parser = set_up(input);
    let result = parser.parse_expression()?.unwrap();
    let Expression::FieldAccess(acc) = result else { panic!() };
    assert_eq!(acc.target.0.to_usize(), 2);
    let Expression::FieldAccess(acc2) = *acc.base else {panic!() };
    assert_eq!(acc2.target.0.to_usize(), 1);
    let Expression::Variable(v) = *acc2.base else { panic!() };
    assert_eq!(v.ident.0.to_usize(), 0);
    Ok(())
}

#[test]
fn type_parameter_single() -> ParseResult<()> {
    let input = "Array<int>";
    let mut parser = set_up(input);
    let result = parser.parse_type_expression();
    assert!(matches!(result, Ok(Some(TypeExpression::TypeApplication(_)))));
    Ok(())
}

#[test]
fn type_parameter_multi() -> ParseResult<()> {
    let input = "Map<int, Array<int>>";
    let mut parser = set_up(input);
    let result = parser.parse_type_expression();
    let Ok(Some(TypeExpression::TypeApplication(app))) = result else {
        panic!("Expected type application")
    };
    assert_eq!(app.params.len(), 2);
    let TypeExpression::TypeApplication(inner_app) = &app.params[1] else {
        panic!("Expected second param to be a type application");
    };
    assert!(matches!(inner_app.params[0], TypeExpression::Int(_)));
    Ok(())
}

#[test]
fn prelude_only() -> Result<(), ParseError> {
    env_logger::init();
    let module = parse_text("", "prelude_only.nx", true)?;
    assert_eq!(&module.name, "prelude_only.nx");
    if let Some(Definition::FnDef(fndef)) = module.defs.first() {
        assert_eq!(*module.get_ident_name(fndef.name), *"printInt")
    } else {
        println!("{module:?}");
        panic!("no definitions in prelude");
    }
    Ok(())
}

#[test]
fn precedence() -> Result<(), ParseError> {
    let input = "2 * 1 + 3";
    let mut parser = set_up(input);
    let result = parser.parse_expression()?;
    let result2 = parser.parse_expression()?;
    println!("{result:#?}");
    println!("{result2:#?}");
    let Some(Expression::BinaryOp(bin_op)) = result else {
        panic!()
    };
    let Expression::BinaryOp(lhs) = bin_op.operand1.as_ref() else {
        panic!()
    };
    let Expression::Literal(rhs) = bin_op.operand2.as_ref() else {
        panic!()
    };
    assert_eq!(bin_op.operation, BinaryOpKind::Add);
    assert_eq!(lhs.operation, BinaryOpKind::Multiply);
    assert!(matches!(rhs, Literal::Numeric(_, _)));
    Ok(())
}
