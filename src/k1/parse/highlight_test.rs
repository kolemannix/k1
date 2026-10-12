// Copyright (c) 2026 knix
// All rights reserved.
use crate::parse::highlight::highlight_text;
use crate::parse::parse_standalone;

fn lines(source: &str) -> Vec<String> {
    let (ast, lexed) = parse_standalone("highlight_test.k1".to_string(), source);
    assert!(ast.errors.is_empty(), "{:?}", ast.errors);
    let mut lines = Vec::new();
    for line in highlight_text(&ast, &lexed).lines() {
        lines.push(line.to_string());
    }
    lines
}

#[test]
fn roles_literals_and_comments() {
    let source = "// head\nfn add(a: u32): u32 { a + 1 }\nlet x: u32 = #static add(2)\n";
    let got = lines(source);
    let want = [
        "0 7 comment",
        "8 2 kw",
        "11 3 fn.decl",
        "15 1 var.decl",
        "18 3 type",
        "30 1 var",
        "32 1 op",
        "34 1 number",
        "38 3 kw",
        "42 1 var.decl",
        "45 3 type",
        "51 7 directive",
        "59 3 fn",
        "63 1 number",
    ];
    for line in want {
        assert!(got.contains(&line.to_string()), "missing {line} in {got:#?}");
    }
    let mut end = 0;
    for line in &got {
        let mut parts = line.split(' ');
        let start: u32 = parts.next().unwrap().parse().unwrap();
        let len: u32 = parts.next().unwrap().parse().unwrap();
        assert!(start >= end, "overlap at {line}");
        end = start + len;
    }
}

#[test]
fn interpolated_string_segments() {
    let got = lines("fn f(a: u32): string { \"x=${a} y=$a!\" }\n");
    for line in ["23 3 string", "28 1 var", "30 3 string", "34 1 var", "35 2 string"] {
        assert!(got.contains(&line.to_string()), "missing {line} in {got:#?}");
    }
}

#[test]
fn parse_error_keeps_lexical_classes() {
    let (ast, lexed) = parse_standalone("bad.k1".to_string(), "fn (: u32 {\n");
    assert!(!ast.errors.is_empty());
    let text = highlight_text(&ast, &lexed);
    assert!(text.starts_with("0 2 kw\n3 1 punct\n"), "{text}");
}
