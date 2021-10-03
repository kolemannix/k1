use std::fs::File;
use std::io::{BufReader, Read};

use crate::ast::*;
use crate::lex::*;

#[cfg(test)]
mod parse_test;

fn check_eat_token(tokens: &mut Tokens, target_token: TokenKind) -> bool {
    let tok = tokens.peek();
    if tok.kind == target_token {
        tokens.next();
        true
    } else {
        false
    }
}

fn check_eat_ident(tokens: &mut Tokens) -> Option<String> {
    let ident;
    let mut found = false;
    if let TokenKind::Text(s) = &tokens.peek.kind {
       ident = s.clone();
       found = true;
    }
    if found {
        tokens.advance();
        Some(ident)
    }
    if let TokenKind::Text(s) = &tokens.peek().kind {
        tokens.advance();
        Some(s.clone())
    } else {
        None
    }
}

fn eat_type_expression(tokens: &mut Tokens) -> Option<TypeExpression> {
    if let TokenKind::Text(t) = &tokens.peek().kind {
        if t == "i32" {
            tokens.next();
            Some(TypeExpression::Primitive(TypePrimitive::I32))
        } else {
            None
        }
    } else {
        None
    }
}

fn eat_val(tokens: &mut Tokens) -> Option<ValDef> {
    let _val = if !check_eat_token(tokens, TokenKind::Keyword(Keyword::Val)) { return None; };
    let ident = check_eat_ident(tokens)?;
    let _colon = if !check_eat_token(tokens, TokenKind::Colon) { return None; };
    let typ = eat_type_expression(tokens)?;
    return Some(ValDef {
        name: Ident(ident),
        typ: Some(typ),
        value: Expression::Literal(Literal::I32(1)),
    });
}

fn eat_fndef(tokens: &mut Tokens) -> Option<FnDef> {
    println!("eat_fndef is unimplemented");
    None
}

fn eat_definition(tokens: &mut Tokens) -> Result<Definition, String> {
    // Perf note: Try the most common things alternates first, or the cheapest to try
    if let Some(r#const) = eat_val(tokens) {
        Ok(Definition::ValDef(r#const))
    } else if let Some(fn_def) = eat_fndef(tokens) {
        Ok(Definition::FnDef(fn_def))
    } else {
        Err("Expected Const or FnDef; got somethin' else".to_string())
    }
}

// struct TokenCursor {
//     tokens: Vec<Token>,
//     cursor: i32,
// }

/// Rough Grammar
/// module -> definition *
/// definition -> const | fn | struct
fn parse_module(tokens: &[Token]) -> Result<Module, String> {
    let mut defs: Vec<Definition> = vec![];

    let mut tok_iter: Tokens = Tokens::make(tokens);
    while let Ok(def) = eat_definition(&mut tok_iter) {
        defs.push(def)
    }
    Ok(Module {
        name: Ident("module1".to_string()),
        defs: defs,
    })
}

pub fn parse_file(path: &str) -> Result<Module, String> {
    let file = File::open(path).expect(&format!("file not found {}", path));
    let mut buf_read = BufReader::new(file);
    let mut content = String::new();
    buf_read.read_to_string(&mut content);
    let mut lexer = Lexer::make(&content);

    let tokens = tokenize(&mut lexer);
    println!("toks {:?}", &tokens);

    let module = parse_module(&tokens);

    module
}