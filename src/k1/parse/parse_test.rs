// Copyright (c) 2025 knix
// All rights reserved.

use crate::parse::*;
use std::fs;
use itertools::Itertools;

fn make_test_ast() -> ParsedProgram {
    ParsedProgram::make("unit_test".to_string(), false)
}

fn set_up<'ast>(input: &str, ast: &'ast mut ParsedProgram) -> Parser<'static, 'ast> {
    let source =
        Source::make(0, "unit_test".to_string(), "unit_test.k1".to_string(), input.to_string());
    let file_id = source.file_id;
    let module_id = ModuleId::from_u32(1).unwrap();
    let module_name = ast.idents.intern("unit_test");
    let mut token_vec = vec![];
    lex_file_into_program(ast, source, &mut token_vec).unwrap();
    let token_vec = token_vec.leak();
    println!("{:#?}", token_vec);
    println!("{:#?}", token_vec.iter().map(|t| t.kind).collect_vec());
    let ns_id = init_module(module_name, ast);
    let parser = Parser::make_for_file(module_id, module_name, ns_id, ast, token_vec, file_id);
    parser
}

fn test_single_expr(input: &str) -> Result<(ParsedProgram, ParsedExpr), ParseError> {
    test_single_expr_with_id(input).map(|(m, e, _)| (m, e))
}

fn test_single_type_expr(input: &str) -> Result<(ParsedProgram, ParsedTypeExpr), ParseError> {
    let mut module = make_test_ast();
    let mut parser = set_up(input, &mut module);
    let type_expr_id = parser.expect_type_expression()?;
    let expr = (*module.type_exprs.get(type_expr_id)).clone();
    Ok((module, expr))
}

fn test_single_expr_with_id(
    input: &str,
) -> Result<(ParsedProgram, ParsedExpr, ParsedExprId), ParseError> {
    let mut ast = make_test_ast();
    let mut parser = set_up(input, &mut ast);
    let result = parser.expect_expression();
    let expr_id = result?;
    let expr = (*ast.exprs.get(expr_id)).clone();
    Ok((ast, expr, expr_id))
}

#[test]
fn basic_fn() -> Result<(), ParseError> {
    let src = r#"
    fn basic(x: u64, y: u64): u64 {
      println(42, 42, 42);
      let x = 0;
      let y = 2;
      y := { 1; 2; 3 };
      y := add(42, 42);
      add(x, y)
    }"#;
    let mut module = test_parse_input("basic_fn".to_string(), src.to_string())?;
    let fndef = module.functions.get_opt(ParsedFunctionId::from_u32(1).unwrap()).unwrap();
    assert_eq!(fndef.name, module.idents.intern("basic"));
    Ok(())
}

#[test]
fn string_literal() -> ParseResult<()> {
    let (ast, result) = test_single_expr(r#""Hello, World!""#)?;
    let ParsedExpr::Literal(ParsedLiteral::String(s, span_id)) = result else { panic!() };
    let span = ast.spans.get(span_id);
    assert_eq!(ast.get_string(s), "Hello, World!");
    assert_eq!(span.start, 0);
    assert_eq!(span.len, 13 + 2);
    assert_eq!(span.end(), 15);
    Ok(())
}

#[test]
fn infix() -> Result<(), ParseError> {
    let mut ast = make_test_ast();
    let mut parser = set_up("let x = a + b * doStuff(1, 2)", &mut ast);
    let result = parser.parse_statement()?.unwrap();
    if let ParsedStmt::Let(ParsedLet { value: expr_id, .. }) = ast.stmts.get(result) {
        let ParsedExpr::BinaryOp(op) = ast.exprs.get(expr_id.unwrap()) else { panic!() };
        assert_eq!(op.op_kind, BinaryOpKind::Add);
        assert!(matches!(*ast.exprs.get(op.lhs), ParsedExpr::Variable(_)));
        if let ParsedExpr::BinaryOp(BinaryOp {
            op_kind: operation,
            lhs: operand1,
            rhs: operand2,
            ..
        }) = ast.exprs.get(op.rhs)
        {
            assert_eq!(*operation, BinaryOpKind::Multiply);
            assert!(matches!(*ast.exprs.get(*operand1), ParsedExpr::Variable(_)));
            assert!(matches!(*ast.exprs.get(*operand2), ParsedExpr::Call(_)));
        } else {
            panic!("Expected nested infix ops; got {:?}", result);
        }
    }
    Ok(())
}

#[test]
fn struct_1() -> Result<(), ParseError> {
    let (_ast, result) = test_single_expr("{ a: 4, b: x.a, c: true }")?;
    assert!(matches!(result, ParsedExpr::Struct(_)));
    Ok(())
}

#[test]
fn parse_eof() -> Result<(), ParseError> {
    let mut ast = make_test_ast();
    let mut parser = set_up("", &mut ast);
    let result = parser.parse_expression()?;
    assert!(result.is_none());
    Ok(())
}

#[test]
fn fn_args_literal() -> Result<(), ParseError> {
    let input = "f(myarg = 42,42,\"abc\")";
    let (ast, result) = test_single_expr(input)?;
    if let ParsedExpr::Call(fn_call) = result {
        let args = ast.mem.getn(fn_call.args);
        let idents = &ast.idents;
        assert_eq!(idents.get_name(fn_call.name.name), "f");
        assert_eq!(idents.get_name(args[0].name.unwrap()), "myarg");
        assert!(ParsedExpr::is_literal(ast.exprs.get(args[0].value)));
        assert!(ParsedExpr::is_literal(ast.exprs.get(args[1].value)));
        assert!(ParsedExpr::is_literal(ast.exprs.get(args[1].value)));
        assert_eq!(args.len(), 3);
    } else {
        panic!("fail");
    }

    Ok(())
}

#[test]
fn if_no_else() -> ParseResult<()> {
    let input = "if x a";
    let mut ast = make_test_ast();
    let mut parser = set_up(input, &mut ast);
    let _result = parser.expect_expression()?;
    Ok(())
}

#[test]
fn dot_accessor() -> ParseResult<()> {
    let input = "a.b.c";
    let (ast, result) = test_single_expr(input)?;
    let ParsedExpr::FieldAccess(access_op) = result else { panic!() };
    assert_eq!(ast.idents.get_name(access_op.field_name), "c");
    let ParsedExpr::FieldAccess(acc2) = ast.exprs.get(access_op.base) else { panic!() };
    assert_eq!(ast.idents.get_name(acc2.field_name), "b");
    let ParsedExpr::Variable(v) = ast.exprs.get(acc2.base) else { panic!() };
    assert_eq!(ast.idents.get_name(v.name.name), "a");
    Ok(())
}

#[test]
fn type_parameter_single() -> ParseResult<()> {
    let (mut _ast, type_expr) = test_single_type_expr("List[unit]")?;
    assert!(matches!(type_expr, ParsedTypeExpr::TypeApplication(_)));
    Ok(())
}

#[test]
fn type_parameter_multi() -> ParseResult<()> {
    let (ast, type_expr) = test_single_type_expr("Map[bool, List[bool]]")?;
    let ParsedTypeExpr::TypeApplication(app) = type_expr else {
        panic!("Expected type application")
    };
    assert_eq!(app.args.len(), 2);
    let args = ast.mem.getn(app.args);
    let ParsedTypeExpr::TypeApplication(inner_app) = ast.type_exprs.get(args[1].type_expr.unwrap())
    else {
        panic!("Expected second param to be a type application");
    };
    let inner_args = ast.mem.getn(inner_app.args);
    assert!(matches!(
        ast.type_exprs.get(inner_args[0].type_expr.unwrap()),
        ParsedTypeExpr::TypeApplication(_)
    ));
    Ok(())
}

#[test]
fn builtin_only() -> Result<(), ParseError> {
    let builtin_source = fs::read_to_string("k1lib/core/builtin.k1").unwrap();
    let ast = test_parse_input("builtin.k1".to_string(), builtin_source)?;
    assert!(!ast.get_root_namespace().definitions.is_empty());
    Ok(())
}

#[test]
fn core_only() -> Result<(), ParseError> {
    let core_source = fs::read_to_string("k1lib/core/core.k1").unwrap();
    let ast = test_parse_input("core.k1".to_string(), core_source)?;
    assert!(!ast.get_root_namespace().definitions.is_empty());
    Ok(())
}

#[test]
fn precedence() -> Result<(), ParseError> {
    let input = "2 * 1 + 3";
    let (ast, result) = test_single_expr(input)?;
    let ParsedExpr::BinaryOp(bin_op) = result else { panic!() };
    let ParsedExpr::BinaryOp(lhs) = ast.exprs.get(bin_op.lhs) else { panic!() };
    let ParsedExpr::Literal(rhs) = ast.exprs.get(bin_op.rhs) else { panic!() };
    assert_eq!(bin_op.op_kind, BinaryOpKind::Add);
    assert_eq!(lhs.op_kind, BinaryOpKind::Multiply);
    assert!(matches!(rhs, ParsedLiteral::Numeric(_)));
    Ok(())
}

#[test]
fn paren_expression() -> Result<(), ParseError> {
    let input = "(1 + a(i, i + 4)) * 3";
    let (_parser, result) = test_single_expr(input)?;
    if let ParsedExpr::BinaryOp(bin_op) = &result {
        assert_eq!(bin_op.op_kind, BinaryOpKind::Multiply);
        return Ok(());
    }
    panic!()
}

#[test]
fn while_loop_1() -> Result<(), ParseError> {
    let input = "while true { {}; {}; 42 }";
    let (ast, result) = test_single_expr(input)?;
    if let ParsedExpr::While(while_expr) = result {
        let ParsedExpr::Block(block) = ast.exprs.get(while_expr.body) else {
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
    let mut ast = make_test_ast();
    let mut parser = set_up(input, &mut ast);
    let _result = parser.expect_expression()?;
    Ok(())
}

#[test]
fn generic_fn_call() -> Result<(), ParseError> {
    let input = "square[int](42)";
    let (_ast, result) = test_single_expr(input)?;
    assert!(matches!(result, ParsedExpr::Call(_)));
    Ok(())
}

#[test]
fn generic_method_call_lhs_expr() -> Result<(), ParseError> {
    let input = "getFn().baz[u64](42)";
    let (mut ast, result) = test_single_expr(input)?;
    let ParsedExpr::Call(call) = result else { panic!() };
    let args = ast.mem.getn(call.args);
    let ParsedExpr::Call(fn_call) = ast.exprs.get(args[0].value).clone() else { panic!() };
    assert_eq!(fn_call.name.name, ast.idents.intern("getFn"));
    assert_eq!(call.name.name, ast.idents.intern("baz"));
    let type_args = ast.mem.getn(call.type_args);
    let type_arg = ast.type_exprs.get(type_args[0].type_expr.unwrap());
    assert!(matches!(type_arg, ParsedTypeExpr::TypeApplication(_)));
    assert!(matches!(ast.exprs.get(args[1].value), ParsedExpr::Literal(_)));
    Ok(())
}

#[test]
fn char_value() -> ParseResult<()> {
    let input = "'x'";
    let (_ast, result) = test_single_expr(input)?;
    assert!(matches!(result, ParsedExpr::Literal(ParsedLiteral::Char(b, _)) if b == b'x'));
    Ok(())
}

#[test]
fn namespaced_fncall() -> ParseResult<()> {
    let input = "foo/bar/baz()";
    let (mut ast, result) = test_single_expr(input)?;
    let ParsedExpr::Call(fn_call) = result else { panic!("not fncall") };
    assert_eq!(ast.idents.get_name(*ast.idents.slices.get_nth(fn_call.name.path, 0)), "foo");
    assert_eq!(ast.idents.get_name(*ast.idents.slices.get_nth(fn_call.name.path, 1)), "bar");
    assert_eq!(fn_call.name.name, ast.idents.intern("baz"));
    assert!(fn_call.args.is_empty());
    Ok(())
}
#[test]
fn namespaced_val() -> ParseResult<()> {
    let input = "foo/bar/baz";
    let (mut ast, result) = test_single_expr(input)?;
    let ParsedExpr::Variable(variable) = result else { panic!("not variable") };
    assert_eq!(ast.idents.get_name(*ast.idents.slices.get_nth(variable.name.path, 0)), "foo");
    assert_eq!(ast.idents.get_name(*ast.idents.slices.get_nth(variable.name.path, 1)), "bar");
    assert_eq!(variable.name.name, ast.idents.intern("baz"));
    Ok(())
}

#[test]
fn type_hint() -> ParseResult<()> {
    let input = "None: ?u64";
    let (ast, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = ast.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(ast.type_expr_to_string(type_hint), "?u64");
    Ok(())
}

#[test]
fn type_hint_binop() -> ParseResult<()> {
    let input = "(3.!: int + 4: List[bool]): int";
    let (ast, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = ast.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(ast.type_expr_to_string(type_hint), "int");
    Ok(())
}

#[test]
fn variants_no_payload() -> ParseResult<()> {
    let input = ":foo: t";
    let (ast, _expr, expr_id) = test_single_expr_with_id(input)?;
    let type_hint = ast.get_expression_type_hint(expr_id).unwrap();
    assert_eq!(&ast.expr_id_to_string(expr_id), ":foo");
    assert_eq!(ast.type_expr_to_string(type_hint), "t");
    Ok(())
}

#[test]
fn variants_payload() -> ParseResult<()> {
    let input = "veni/vidi/vici:foo[t, u](bar.bar)";
    let (ast, expr, expr_id) = test_single_expr_with_id(input)?;
    let ParsedExpr::Variant(v) = expr else { panic!() };
    assert!(v.payload.is_some());
    assert!(v.type_name.is_some());
    assert_eq!(ast.idents.get_name(v.variant_name), "foo");
    assert_eq!(v.type_args.len(), 2);
    assert_eq!(&ast.expr_id_to_string(expr_id), "veni/vidi/vici:foo[t, u](bar.bar)");
    Ok(())
}

#[test]
fn unclosed_ability_impl() -> ParseResult<()> {
    let input = r#"impl Equals for List[int] {
  fn equals(self: List[int], other: List[int]): bool { self.buffer == other.buffer }

impl print for bool {
  fn print(self: bool): string { if self "true" else "false" }
}
  "#;
    let mut ast = make_test_ast();
    let mut parser = set_up(input, &mut ast);
    let ability_result = parser.parse_definition(K::Eof);
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
    let (ast, _expr, expr_id) = test_single_expr_with_id(input)?;
    println!("{}", &ast.expr_id_to_string(expr_id));
    Ok(())
}

#[test]
fn empty_struct() -> ParseResult<()> {
    let input = r#"{}"#;
    let (_ast, expr, _expr_id) = test_single_expr_with_id(input)?;
    assert!(matches!(expr, ParsedExpr::Struct(_)));
    Ok(())
}

#[test]
fn integer_suffix() -> ParseResult<()> {
    let input_u8 = r#"42u8"#;
    let input_i64 = r#"-42i64"#;
    let (ast, expr, _expr_id) = test_single_expr_with_id(input_u8)?;
    let ParsedExpr::Literal(ParsedLiteral::Numeric(_)) = expr else {
        panic!("`{input_u8}` did not parse as expected")
    };
    let text = ast.get_span_content(expr.get_span());
    assert_eq!(text, "42u8");

    let (ast, expr, _expr_id) = test_single_expr_with_id(input_i64)?;
    let ParsedExpr::Literal(ParsedLiteral::Numeric(_)) = expr else {
        panic!("`{input_i64}` did not parse as expected")
    };
    let text = ast.get_span_content(expr.get_span());
    assert_eq!(text, "-42i64");
    Ok(())
}

#[test]
fn tag_no_type_hint() -> ParseResult<()> {
    let input = r#"if self == other :equal else :less"#;
    let (ast, expr, _expr_id) = test_single_expr_with_id(input)?;
    let ParsedExpr::If(parsed_if) = expr else { panic!("`{input}` did not parse as expected") };

    let ParsedExpr::Variant(_) = ast.exprs.get(parsed_if.cons) else {
        panic!("");
    };

    Ok(())
}

#[test]
fn consecutive_strings() -> ParseResult<()> {
    let input = r#""Hello, " "World!""#;
    let (ast, expr, _expr_id) = test_single_expr_with_id(input)?;
    let ParsedExpr::InterpolatedString(is) = expr else { panic!() };
    let span = ast.spans.get(is.span);
    assert_eq!(is.parts.len(), 2);
    let InterpolatedStringPart::String(s1) = ast.mem.get_nth(is.parts, 0) else { panic!() };
    let InterpolatedStringPart::String(s2) = ast.mem.get_nth(is.parts, 1) else { panic!() };
    assert_eq!(ast.get_string(*s1), "Hello, ");
    assert_eq!(ast.get_string(*s2), "World!");
    assert_eq!(span.start, 0);
    assert_eq!(span.len, 18);
    Ok(())
}
