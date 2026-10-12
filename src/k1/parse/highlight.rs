// Copyright (c) 2026 knix
// All rights reserved.
use std::fmt::Write;

use crate::lex::{Lexed, TokenKind as K, TokenTriviaKind};
use crate::parse::{ParsedProgram, SemanticToken, SemanticTokenKind};

struct Classed {
    start: u32,
    len: u32,
    class: &'static str,
}

fn lexer_class(kind: K) -> Option<&'static str> {
    use K::*;
    Some(match kind {
        Eof => return None,
        Ident => "ident",
        Numeric => "number",
        StringDoneDq | StringDoneBt | StringOpenDq | StringOpenBt | FmtSpec => "string",
        Char => "char",
        KeywordFn | KeywordLet | KeywordAnd | KeywordOr | KeywordIf | KeywordElse
        | KeywordWhile | KeywordLoop | KeywordNs | KeywordIntern | KeywordFor | KeywordIn
        | KeywordAbility | KeywordImpl | KeywordIs | KeywordNot | KeywordBuiltin | KeywordWhere
        | KeywordContext | KeywordUse | KeywordRequire | KeywordDefer => "kw",
        OpenParen | CloseParen | OpenBracket | CloseBracket | OpenBrace | CloseBrace | Colon
        | Semicolon | Dot | Comma | Hash => "punct",
        _ => "op",
    })
}

fn parser_class(token: &SemanticToken) -> &'static str {
    let decl = token.mods & (SemanticToken::DECLARATION | SemanticToken::DEFINITION) != 0;
    match token.kind {
        SemanticTokenKind::Type => {
            if decl {
                "type.decl"
            } else {
                "type"
            }
        }
        SemanticTokenKind::Variable => {
            if decl {
                "var.decl"
            } else {
                "var"
            }
        }
        SemanticTokenKind::Function => {
            if decl {
                "fn.decl"
            } else {
                "fn"
            }
        }
        SemanticTokenKind::Namespace => {
            if decl {
                "ns.decl"
            } else {
                "ns"
            }
        }
        SemanticTokenKind::Property => "prop",
        SemanticTokenKind::EnumMember => "variant",
        SemanticTokenKind::String => "string",
        SemanticTokenKind::Keyword => "kw",
        SemanticTokenKind::Operator => "op",
        SemanticTokenKind::Comment => "comment",
    }
}

pub fn highlight_text(ast: &ParsedProgram, lexed: &Lexed) -> String {
    let mut classed: Vec<Classed> =
        Vec::with_capacity(lexed.tokens.len() + lexed.trivia.entries().len());
    let mut index = 0;
    while index < lexed.tokens.len() {
        let token = lexed.tokens[index];
        let (start, len) = lexed.get_token_start_len(token);
        index += 1;
        let Some(class) = lexer_class(token.kind) else { continue };
        if token.kind == K::Hash && index < lexed.tokens.len() {
            let next = lexed.tokens[index];
            let glued = !next.is_whitespace_preceded()
                && lexer_class(next.kind).is_some_and(|c| c == "ident" || c == "kw");
            if glued {
                let (_, next_len) = lexed.get_token_start_len(next);
                classed.push(Classed { start, len: len + next_len, class: "directive" });
                index += 1;
                continue;
            }
        }
        classed.push(Classed { start, len, class });
    }
    let source = ast.sources.get_main();
    for token in source.semantic_tokens.as_slice(&ast.mem) {
        let start = source.local_from_global_offset(token.span.global_start);
        let found = classed.binary_search_by_key(&start, |c| c.start);
        if let Ok(at) = found
            && classed[at].len == token.span.len
        {
            classed[at].class = parser_class(token);
        }
    }
    for entry in lexed.trivia.entries() {
        if entry.trivia.kind == TokenTriviaKind::LineComment {
            let t = entry.trivia;
            classed.push(Classed { start: t.start, len: t.len, class: "comment" });
        }
    }
    classed.sort_unstable_by_key(|c| c.start);
    let mut out = String::with_capacity(classed.len() * 16);
    for c in &classed {
        writeln!(out, "{} {} {}", c.start, c.len, c.class).unwrap();
    }
    out
}
