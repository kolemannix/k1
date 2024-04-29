use crate::parse::*;
use std::fs;

fn make_test_module() -> ParsedModule {
    ParsedModule::make("unit_test".to_string())
}

fn set_up<'module>(input: &str, module: &'module mut ParsedModule) -> Parser<'static, 'module> {
    let source = Rc::new(Source::make(
        0,
        "unit_test".to_string(),
        "unit_test.bfl".to_string(),
        input.to_string(),
    ));
    let mut lexer = Lexer::make(&source.content, 0);
    let token_vec: &'static mut [Token] = lexer.run().unwrap().leak();
    let parser = Parser::make(token_vec, source, module);
    parser
}

fn test_single_expr(input: &str) -> Result<(ParsedModule, ParsedExpression), ParseError> {
    test_single_expr_with_id(input).map(|(m, e, _)| (m, e))
}

fn test_single_expr_with_id(
    input: &str,
) -> Result<(ParsedModule, ParsedExpression, ExpressionId), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let expr_id = parser.expect_expression()?;
    let expr = (*parser.get_expression(expr_id)).clone();
    Ok((module, expr, expr_id))
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
      add(x, y)
    }"#;
    let source = Rc::new(Source::make(
        0,
        "test_src".to_string(),
        "test_case.bfl".to_string(),
        src.to_string(),
    ));
    let module = parse_module(source)?;
    let fndef = module.get_function(0);
    assert_eq!(fndef.name, module.ident_id("basic"));
    Ok(())
}

#[test]
fn string_literal() -> ParseResult<()> {
    let (_parser, result) = test_single_expr(r#""hello world""#)?;
    let ParsedExpression::Literal(Literal::String(s, span)) = result else { panic!() };
    assert_eq!(&s, "hello world");
    assert_eq!(span.start, 1);
    assert_eq!(span.end, 12);
    Ok(())
}

#[test]
fn infix() -> Result<(), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up("val x = a + b * doStuff(1, 2)", &mut module);
    let result = parser.parse_statement()?;
    if let Some(BlockStmt::ValDef(ValDef { value: expr_id, .. })) = result {
        let ParsedExpression::BinaryOp(op) = &*parser.get_expression(expr_id) else { panic!() };
        assert_eq!(op.op_kind, BinaryOpKind::Add);
        assert!(matches!(*parser.get_expression(op.lhs), ParsedExpression::Variable(_)));
        if let ParsedExpression::BinaryOp(BinaryOp {
            op_kind: operation,
            lhs: operand1,
            rhs: operand2,
            ..
        }) = &*parser.get_expression(op.rhs)
        {
            assert_eq!(*operation, BinaryOpKind::Multiply);
            assert!(matches!(*parser.get_expression(*operand1), ParsedExpression::Variable(_)));
            assert!(matches!(*parser.get_expression(*operand2), ParsedExpression::FnCall(_)));
        } else {
            panic!("Expected nested infix ops; got {:?}", result);
        }
    } else {
        panic!("Expected nested infix ops; got {:?}", result);
    }
    Ok(())
}

#[test]
fn record() -> Result<(), ParseError> {
    let (_parser, result) = test_single_expr("{ a: 4, b: x[42], c: true }")?;
    assert!(matches!(result, ParsedExpression::Record(_)));
    Ok(())
}

#[test]
fn parse_eof() -> Result<(), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up("", &mut module);
    let result = parser.parse_expression()?;
    assert!(result.is_none());
    Ok(())
}

#[test]
fn fn_args_literal() -> Result<(), ParseError> {
    let input = "f(myarg = 42,42,\"abc\")";
    let (parser, result) = test_single_expr(input)?;
    if let ParsedExpression::FnCall(fn_call) = result {
        let args = &fn_call.args;
        let idents = parser.identifiers.clone();
        assert_eq!(idents.borrow().get_name(fn_call.name), "f");
        assert_eq!(idents.borrow().get_name(args[0].name.unwrap()), "myarg");
        assert!(ParsedExpression::is_literal(&parser.get_expression(args[0].value)));
        assert!(ParsedExpression::is_literal(&parser.get_expression(args[1].value)));
        assert!(ParsedExpression::is_literal(&parser.get_expression(args[2].value)));
        assert_eq!(args.len(), 3);
    } else {
        panic!("fail");
    }

    Ok(())
}

#[test]
fn if_no_else() -> ParseResult<()> {
    let input = "if x a";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let result = parser.parse_expression()?.unwrap();
    Ok(())
}

#[test]
fn dot_accessor() -> ParseResult<()> {
    let input = "a.b.c";
    let (pm, result) = test_single_expr(input)?;
    let ParsedExpression::FieldAccess(access_op) = result else { panic!() };
    assert_eq!(&*pm.get_ident_str(access_op.target), "c");
    let ParsedExpression::FieldAccess(acc2) = &*pm.get_expression(access_op.base) else { panic!() };
    assert_eq!(&*pm.get_ident_str(acc2.target), "b");
    let ParsedExpression::Variable(v) = &*pm.get_expression(acc2.base) else { panic!() };
    assert_eq!(&*pm.get_ident_str(v.name), "a");
    Ok(())
}

#[test]
fn type_parameter_single() -> ParseResult<()> {
    let input = "Array<int>";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let result = parser.parse_type_expression();
    assert!(matches!(result, Ok(Some(ParsedTypeExpression::TypeApplication(_)))));
    Ok(())
}

#[test]
fn type_parameter_multi() -> ParseResult<()> {
    let input = "Map<int, Array<int>>";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let result = parser.parse_type_expression();
    let Ok(Some(ParsedTypeExpression::TypeApplication(app))) = result else {
        panic!("Expected type application")
    };
    assert_eq!(app.params.len(), 2);
    let ParsedTypeExpression::TypeApplication(inner_app) = &app.params[1] else {
        panic!("Expected second param to be a type application");
    };
    assert!(matches!(inner_app.params[0], ParsedTypeExpression::Int(_)));
    Ok(())
}

#[test]
fn prelude_only() -> Result<(), ParseError> {
    env_logger::init();
    let prelude_source = Rc::new(Source::make(
        0,
        "builtins".to_string(),
        "prelude.bfl".to_string(),
        fs::read_to_string("builtins/prelude.bfl").unwrap(),
    ));
    let module = parse_module(prelude_source)?;
    assert_eq!(&module.name, "prelude");
    assert_eq!(&module.sources.get_main().filename, "prelude.bfl");
    assert_eq!(&module.sources.get_main().directory, "builtins");
    assert!(!module.get_root_namespace().definitions.is_empty());
    Ok(())
}

#[test]
fn precedence() -> Result<(), ParseError> {
    let input = "2 * 1 + 3";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let expr = parser.parse_expression()?.unwrap();
    let result = parser.get_expression(expr).clone();
    let ParsedExpression::BinaryOp(bin_op) = result else { panic!() };
    let ParsedExpression::BinaryOp(lhs) = &*parser.get_expression(bin_op.lhs) else { panic!() };
    let ParsedExpression::Literal(rhs) = &*parser.get_expression(bin_op.rhs) else { panic!() };
    assert_eq!(bin_op.op_kind, BinaryOpKind::Add);
    assert_eq!(lhs.op_kind, BinaryOpKind::Multiply);
    assert!(matches!(rhs, Literal::Numeric(_, _)));
    Ok(())
}

#[test]
fn paren_expression() -> Result<(), ParseError> {
    let input = "(1 + 2[i][i + 4]) * 3";
    let (_parser, result) = test_single_expr(input)?;
    if let ParsedExpression::BinaryOp(bin_op) = &result {
        assert_eq!(bin_op.op_kind, BinaryOpKind::Multiply);
        return Ok(());
    }
    panic!()
}

#[test]
fn while_loop_1() -> Result<(), ParseError> {
    let input = "while true { (); (); 42 }";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let result = parser.parse_statement()?.unwrap();
    if let BlockStmt::While(while_stmt) = result {
        assert_eq!(while_stmt.block.stmts.len(), 3);
        return Ok(());
    }
    panic!()
}

#[test]
fn cmp_operators() -> Result<(), ParseError> {
    let input = "a < b <= c > d >= e";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let _result = parser.parse_expression()?.unwrap();
    Ok(())
}

#[test]
fn generic_fn_call() -> Result<(), ParseError> {
    let input = "square<int>(42)";
    let (_parser, result) = test_single_expr(input)?;
    assert!(matches!(result, ParsedExpression::FnCall(_)));
    Ok(())
}

#[test]
fn generic_method_call_lhs_expr() -> Result<(), ParseError> {
    let input = "getFn().baz<int>(42)";
    let (parser, result) = test_single_expr(input)?;
    let ParsedExpression::MethodCall(call) = result else { panic!() };
    let ParsedExpression::FnCall(fn_call) = parser.get_expression(call.base).clone() else {
        panic!()
    };
    assert_eq!(fn_call.name, parser.ident_id("getFn"));
    assert_eq!(call.call.name, parser.ident_id("baz"));
    assert!(call.call.type_args.unwrap()[0].type_expr.is_int());
    assert!(matches!(
        &*parser.get_expression(call.call.args[0].value),
        ParsedExpression::Literal(_)
    ));
    Ok(())
}

#[test]
fn char_type() -> ParseResult<()> {
    let input = "char";
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let result = parser.parse_type_expression()?.unwrap();
    assert!(matches!(result, ParsedTypeExpression::Char(_)));
    Ok(())
}

#[test]
fn char_value() -> ParseResult<()> {
    let input = "'x'";
    let (_parser, result) = test_single_expr(input)?;
    assert!(matches!(result, ParsedExpression::Literal(Literal::Char(b, _)) if b == b'x'));
    Ok(())
}

#[test]
fn namespaced_fncall() -> ParseResult<()> {
    let input = "foo::bar::baz()";
    let (parser, result) = test_single_expr(input)?;
    let ParsedExpression::FnCall(fn_call) = result else {
        dbg!(result);
        panic!("not fncall")
    };
    assert_eq!(fn_call.namespaces[0], parser.ident_id("foo"));
    assert_eq!(fn_call.namespaces[1], parser.ident_id("bar"));
    assert_eq!(fn_call.name, parser.ident_id("baz"));
    assert!(fn_call.args.is_empty());
    Ok(())
}
#[test]
fn namespaced_val() -> ParseResult<()> {
    let input = "foo::bar::baz";
    let (parser, result) = test_single_expr(input)?;
    let ParsedExpression::Variable(variable) = result else {
        dbg!(result);
        panic!("not variable")
    };
    assert_eq!(variable.namespaces[0], parser.ident_id("foo"));
    assert_eq!(variable.namespaces[1], parser.ident_id("bar"));
    assert_eq!(variable.name, parser.ident_id("baz"));
    Ok(())
}

#[test]
fn type_hint() -> ParseResult<()> {
    let input = "None: int?";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(module.type_expression_to_string(&type_hint), "int?");
    Ok(())
}

#[test]
fn type_hint_binop() -> ParseResult<()> {
    let input = "(3!: int + 4: Array<bool>): int";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(module.type_expression_to_string(&type_hint), "int");
    Ok(())
}

#[test]
fn tag_literals() -> ParseResult<()> {
    let input = ".Foo: .Foo";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(&module.expression_to_string(expr_id), ".Foo");
    assert_eq!(module.type_expression_to_string(&type_hint), ".Foo");
    Ok(())
}
