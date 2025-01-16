use crate::parse::*;
use std::fs;

fn make_test_module() -> ParsedModule {
    ParsedModule::make("unit_test".to_string())
}

fn set_up<'module>(input: &str, module: &'module mut ParsedModule) -> Parser<'static, 'module> {
    let source =
        Source::make(0, "unit_test".to_string(), "unit_test.k1".to_string(), input.to_string());
    let file_id = source.file_id;
    let token_vec: &'static mut [Token] = lex_text(module, source).unwrap().leak();
    println!("{:#?}", token_vec);
    let parser = Parser::make(token_vec, file_id, module);
    parser
}

fn test_single_expr(input: &str) -> Result<(ParsedModule, ParsedExpression), ParseError> {
    test_single_expr_with_id(input).map(|(m, e, _)| (m, e))
}

fn test_single_type_expr(input: &str) -> Result<(ParsedModule, ParsedTypeExpression), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let type_expr_id = parser.expect_type_expression()?;
    let expr = (*module.type_expressions.get(type_expr_id)).clone();
    Ok((module, expr))
}

fn test_single_expr_with_id(
    input: &str,
) -> Result<(ParsedModule, ParsedExpression, ParsedExpressionId), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let expr_id = parser.expect_expression()?;
    let expr = (*module.expressions.get(expr_id)).clone();
    Ok((module, expr, expr_id))
}

#[test]
fn basic_fn() -> Result<(), ParseError> {
    let src = r#"
    fn basic(x: u64, y: u64): u64 {
      println(42, 42, 42);
      let x = 0;
      let mut y = 2;
      y = { 1; 2; 3 };
      y = add(42, 42);
      add(x, y)
    }"#;
    let source =
        Source::make(0, "test_src".to_string(), "test_case.k1".to_string(), src.to_string());
    let mut module = test_parse_module(source)?;
    let fndef = module.functions.first().unwrap();
    assert_eq!(fndef.name, module.identifiers.intern("basic"));
    Ok(())
}

#[test]
fn string_literal() -> ParseResult<()> {
    let (module, result) = test_single_expr(r#""hello world""#)?;
    let ParsedExpression::Literal(Literal::String(s, span_id)) = result else { panic!() };
    let span = module.spans.get(span_id);
    assert_eq!(&s, "hello world");
    assert_eq!(span.start, 1);
    assert_eq!(span.len, 11);
    assert_eq!(span.end(), 12);
    Ok(())
}

#[test]
fn infix() -> Result<(), ParseError> {
    let mut module = make_test_module();
    let mut parser = set_up("let x = a + b * doStuff(1, 2)", &mut module);
    let result = parser.parse_statement()?;
    if let Some(ParsedStmt::ValDef(ValDef { value: expr_id, .. })) = result {
        let ParsedExpression::BinaryOp(op) = module.expressions.get(expr_id) else { panic!() };
        assert_eq!(op.op_kind, BinaryOpKind::Add);
        assert!(matches!(*module.expressions.get(op.lhs), ParsedExpression::Variable(_)));
        if let ParsedExpression::BinaryOp(BinaryOp {
            op_kind: operation,
            lhs: operand1,
            rhs: operand2,
            ..
        }) = module.expressions.get(op.rhs)
        {
            assert_eq!(*operation, BinaryOpKind::Multiply);
            assert!(matches!(*module.expressions.get(*operand1), ParsedExpression::Variable(_)));
            assert!(matches!(*module.expressions.get(*operand2), ParsedExpression::FnCall(_)));
        } else {
            panic!("Expected nested infix ops; got {:?}", result);
        }
    } else {
        panic!("Expected nested infix ops; got {:?}", result);
    }
    Ok(())
}

#[test]
fn struct_1() -> Result<(), ParseError> {
    let (_parser, result) = test_single_expr("{ a: 4, b: x.a, c: true }")?;
    assert!(matches!(result, ParsedExpression::Struct(_)));
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
    let (module, result) = test_single_expr(input)?;
    if let ParsedExpression::FnCall(fn_call) = result {
        let args = &fn_call.args;
        let idents = &module.identifiers;
        assert_eq!(idents.get_name(fn_call.name.name), "f");
        assert_eq!(idents.get_name(args[0].name.unwrap()), "myarg");
        assert!(ParsedExpression::is_literal(module.expressions.get(args[0].value)));
        assert!(ParsedExpression::is_literal(module.expressions.get(args[1].value)));
        assert!(ParsedExpression::is_literal(module.expressions.get(args[1].value)));
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
    let _result = parser.parse_expression()?.unwrap();
    Ok(())
}

#[test]
fn dot_accessor() -> ParseResult<()> {
    let input = "a.b.c";
    let (module, result) = test_single_expr(input)?;
    let ParsedExpression::FieldAccess(access_op) = result else { panic!() };
    assert_eq!(module.identifiers.get_name(access_op.target), "c");
    let ParsedExpression::FieldAccess(acc2) = module.expressions.get(access_op.base) else {
        panic!()
    };
    assert_eq!(module.identifiers.get_name(acc2.target), "b");
    let ParsedExpression::Variable(v) = module.expressions.get(acc2.base) else { panic!() };
    assert_eq!(module.identifiers.get_name(v.name.name), "a");
    Ok(())
}

#[test]
fn type_parameter_single() -> ParseResult<()> {
    let (mut _module, type_expr) = test_single_type_expr("List[unit]")?;
    assert!(matches!(type_expr, ParsedTypeExpression::TypeApplication(_)));
    Ok(())
}

#[test]
fn type_parameter_multi() -> ParseResult<()> {
    let (module, type_expr) = test_single_type_expr("Map[u8, List[u8]]")?;
    let ParsedTypeExpression::TypeApplication(app) = type_expr else {
        panic!("Expected type application")
    };
    assert_eq!(app.args.len(), 2);
    let ParsedTypeExpression::TypeApplication(inner_app) =
        module.type_expressions.get(app.args[1].type_expr)
    else {
        panic!("Expected second param to be a type application");
    };
    assert!(matches!(
        module.type_expressions.get(inner_app.args[0].type_expr),
        ParsedTypeExpression::Integer(ParsedNumericType {
            width: NumericWidth::B8,
            signed: false,
            ..
        })
    ));
    Ok(())
}

#[test]
fn core_only() -> Result<(), ParseError> {
    env_logger::init();
    let core_source = Source::make(
        0,
        "stdlib".to_string(),
        "core.k1".to_string(),
        fs::read_to_string("stdlib/core.k1").unwrap(),
    );
    let module = test_parse_module(core_source)?;
    assert_eq!(&module.name, "core");
    assert_eq!(&module.sources.get_main().filename, "core.k1");
    assert_eq!(&module.sources.get_main().directory, "stdlib");
    assert!(!module.get_root_namespace().definitions.is_empty());
    Ok(())
}

#[test]
fn precedence() -> Result<(), ParseError> {
    let input = "2 * 1 + 3";
    let (module, result) = test_single_expr(input)?;
    let ParsedExpression::BinaryOp(bin_op) = result else { panic!() };
    let ParsedExpression::BinaryOp(lhs) = module.expressions.get(bin_op.lhs) else { panic!() };
    let ParsedExpression::Literal(rhs) = module.expressions.get(bin_op.rhs) else { panic!() };
    assert_eq!(bin_op.op_kind, BinaryOpKind::Add);
    assert_eq!(lhs.op_kind, BinaryOpKind::Multiply);
    assert!(matches!(rhs, Literal::Numeric(_)));
    Ok(())
}

#[test]
fn paren_expression() -> Result<(), ParseError> {
    let input = "(1 + a(i, i + 4)) * 3";
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
    let (module, result) = test_single_expr(input)?;
    if let ParsedExpression::While(while_expr) = result {
        let ParsedExpression::Block(block) = module.expressions.get(while_expr.body) else {
            panic!("expected block")
        };
        assert_eq!(block.stmts.len(), 3);
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
    let input = "square[int](42)";
    let (_parser, result) = test_single_expr(input)?;
    assert!(matches!(result, ParsedExpression::FnCall(_)));
    Ok(())
}

#[test]
fn generic_method_call_lhs_expr() -> Result<(), ParseError> {
    let input = "getFn().baz[u64](42)";
    let (mut parser, result) = test_single_expr(input)?;
    let ParsedExpression::FnCall(call) = result else { panic!() };
    let ParsedExpression::FnCall(fn_call) = parser.expressions.get(call.args[0].value).clone()
    else {
        panic!()
    };
    assert_eq!(fn_call.name.name, parser.identifiers.intern("getFn"));
    assert_eq!(call.name.name, parser.identifiers.intern("baz"));
    let type_arg = parser.type_expressions.get(call.type_args[0].type_expr);
    assert!(type_arg.is_integer());
    assert!(matches!(parser.expressions.get(call.args[1].value), ParsedExpression::Literal(_)));
    Ok(())
}

#[test]
fn char_type() -> ParseResult<()> {
    let (_module, result) = test_single_type_expr("u8")?;
    assert!(matches!(result, ParsedTypeExpression::Integer(_)));
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
    let input = "foo/bar/baz()";
    let (mut parser, result) = test_single_expr(input)?;
    let ParsedExpression::FnCall(fn_call) = result else {
        dbg!(result);
        panic!("not fncall")
    };
    assert_eq!(fn_call.name.namespaces[0], parser.identifiers.intern("foo"));
    assert_eq!(fn_call.name.namespaces[1], parser.identifiers.intern("bar"));
    assert_eq!(fn_call.name.name, parser.identifiers.intern("baz"));
    assert!(fn_call.args.is_empty());
    Ok(())
}
#[test]
fn namespaced_val() -> ParseResult<()> {
    let input = "foo/bar/baz";
    let (mut parser, result) = test_single_expr(input)?;
    let ParsedExpression::Variable(variable) = result else {
        dbg!(result);
        panic!("not variable")
    };
    assert_eq!(variable.name.namespaces[0], parser.identifiers.intern("foo"));
    assert_eq!(variable.name.namespaces[1], parser.identifiers.intern("bar"));
    assert_eq!(variable.name.name, parser.identifiers.intern("baz"));
    Ok(())
}

#[test]
fn type_hint() -> ParseResult<()> {
    let input = "None: u64?";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(module.type_expression_to_string(type_hint), "u64?");
    Ok(())
}

#[test]
fn type_hint_binop() -> ParseResult<()> {
    let input = "(3.!: int + 4: List[bool]): int";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(module.type_expression_to_string(type_hint), "int");
    Ok(())
}

#[test]
fn tag_literals() -> ParseResult<()> {
    let input = ".Foo: A.Foo";
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = module.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(&module.expr_id_to_string(expr_id), ".Foo");
    assert_eq!(module.type_expression_to_string(type_hint), "A.Foo");
    Ok(())
}

#[test]
fn unclosed_ability_impl() -> ParseResult<()> {
    let input = r#"impl Equals for List[int] {
  fn equals(self: List[int], other: List[int]): bool { self.buffer == other.buffer }

impl Show for bool {
  fn show(self: bool): string { if self "true" else "false" }
}
  "#;
    let mut module = make_test_module();
    let mut parser = set_up(input, &mut module);
    let ability_result = parser.parse_definition();
    eprintln!("{:?}", ability_result);
    assert!(ability_result.is_err());
    Ok(())
}

#[test]
fn when_pattern() -> ParseResult<()> {
    let input = r#"
        when x {
           { x: 1, y: 2 } -> 1,
           { x: 2, y: 2 } -> { 2 },
           _ -> { 3 }
        };
"#;
    let (module, _expr, expr_id) = test_single_expr_with_id(input)?;
    println!("{}", &module.expr_id_to_string(expr_id));
    Ok(())
}

#[test]
fn empty_struct() -> ParseResult<()> {
    let input = r#"{}"#;
    let (module, expr, expr_id) = test_single_expr_with_id(input)?;
    eprintln!("{:?}", &expr);
    assert!(matches!(expr, ParsedExpression::Struct(_)));
    Ok(())
}
