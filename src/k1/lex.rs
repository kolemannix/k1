use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::Chars;

use crate::parse::FileId;
use crate::typer::BinaryOpKind;
use log::trace;
use TokenKind as K;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token = Token { span: SpanId::NONE, kind: TokenKind::Eof, flags: 0 };

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
    pub line_index: u32,
}

pub type LexResult<A> = anyhow::Result<A, LexError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SpanId(u32);
impl SpanId {
    pub const NONE: SpanId = SpanId(0);
}

#[derive(Debug, Clone)]
pub struct Spans {
    spans: Vec<Span>,
}

impl Spans {
    pub fn new() -> Spans {
        Spans { spans: vec![Span::NONE] }
    }

    pub fn default_span_id(&self) -> SpanId {
        SpanId::NONE
    }

    pub fn add(&mut self, span: Span) -> SpanId {
        let id = self.spans.len();
        self.spans.push(span);
        SpanId(id as u32)
    }

    pub fn get(&self, id: SpanId) -> Span {
        self.spans[id.0 as usize]
    }

    pub fn extend(&mut self, span1: SpanId, span2: SpanId) -> SpanId {
        let span1 = self.get(span1);
        let span2 = self.get(span2);
        self.add(span1.extended(span2))
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("LexError at line {}: {}", self.line_index, self.msg))
    }
}
impl std::error::Error for LexError {}

pub struct TokenIter<'toks> {
    iter: std::slice::Iter<'toks, Token>,
}

impl<'toks> TokenIter<'toks> {
    pub fn make(data: &'toks [Token]) -> TokenIter<'toks> {
        TokenIter { iter: data.iter() }
    }

    #[inline]
    pub fn next(&mut self) -> Token {
        *self.iter.next().unwrap_or(&EOF_TOKEN)
    }
    #[inline]
    pub fn advance(&mut self) {
        self.next();
    }
    #[inline]
    pub fn peek(&self) -> Token {
        let peeked = self.iter.clone().next();
        *peeked.unwrap_or(&EOF_TOKEN)
    }

    #[inline]
    pub fn peek_two(&self) -> (Token, Token) {
        let mut peek_iter = self.iter.clone();
        let p1 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        let p2 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        (p1, p2)
    }

    #[inline]
    pub fn peek_three(&self) -> (Token, Token, Token) {
        let mut peek_iter = self.iter.clone();
        let p1 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        let p2 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        let p3 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        (p1, p2, p3)
    }
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Ident,
    String,

    Char,

    KeywordFn,
    KeywordVal,
    KeywordMut,
    KeywordAnd,
    KeywordOr,
    KeywordIf,
    KeywordElse,
    KeywordRecord,
    KeywordType,
    KeywordWhile,
    KeywordNamespace,
    KeywordIntern,
    KeywordExtern,
    KeywordFor,
    KeywordIn,
    KeywordDo,
    KeywordYield,
    KeywordEnum,
    KeywordAbility,
    KeywordImpl,
    KeywordAuto,
    KeywordIs,
    KeywordWhen,
    KeywordAs,
    KeywordNot,
    KeywordBuiltin,

    Slash,
    LineComment,

    // Symbols
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenAngle,
    CloseAngle,
    Colon,
    Semicolon,
    Equals,
    EqualsEquals,
    BangEquals,
    Dot,
    Comma,
    Bang,
    QuestionMark,
    Pipe,
    Ampersand,
    Percent,

    DoubleQuote,
    SingleQuote,

    Plus,
    Minus,
    Asterisk,
    LessThanEqual,
    GreaterThanEqual,

    /// Not really a token but allows us to avoid Option<Token> everywhere
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl AsRef<str> for TokenKind {
    fn as_ref(&self) -> &str {
        self.get_repr().unwrap_or("<ident>")
    }
}

impl TokenKind {
    pub fn get_repr(&self) -> Option<&'static str> {
        match self {
            K::KeywordFn => Some("fn"),
            K::KeywordVal => Some("val"),
            K::KeywordMut => Some("mut"),
            K::KeywordAnd => Some("and"),
            K::KeywordOr => Some("or"),
            K::KeywordIf => Some("if"),
            K::KeywordElse => Some("else"),
            K::KeywordRecord => Some("record"),
            K::KeywordType => Some("type"),
            K::KeywordWhile => Some("while"),
            K::KeywordNamespace => Some("namespace"),
            K::KeywordIntern => Some("intern"),
            K::KeywordExtern => Some("extern"),
            K::KeywordFor => Some("for"),
            K::KeywordIn => Some("in"),
            K::KeywordDo => Some("do"),
            K::KeywordYield => Some("yield"),
            K::KeywordEnum => Some("enum"),
            K::KeywordAbility => Some("ability"),
            K::KeywordImpl => Some("impl"),
            K::KeywordAuto => Some("auto"),
            K::KeywordIs => Some("is"),
            K::KeywordWhen => Some("when"),
            K::KeywordAs => Some("as"),
            K::KeywordNot => Some("not"),
            K::KeywordBuiltin => Some("builtin"),

            K::Slash => Some("/"),
            K::LineComment => Some("//"),

            K::OpenParen => Some("("),
            K::CloseParen => Some(")"),
            K::OpenBracket => Some("["),
            K::CloseBracket => Some("]"),
            K::OpenBrace => Some("{"),
            K::CloseBrace => Some("}"),
            K::OpenAngle => Some("<"),
            K::CloseAngle => Some(">"),
            K::Colon => Some(":"),
            K::Semicolon => Some(";"),
            K::Equals => Some("="),
            K::EqualsEquals => Some("=="),
            K::BangEquals => Some("!="),
            K::Dot => Some("."),
            K::Comma => Some(","),
            K::Bang => Some("!"),
            K::QuestionMark => Some("?"),
            K::Pipe => Some("|"),
            K::Ampersand => Some("&"),
            K::Percent => Some("%"),

            K::Plus => Some("+"),
            K::Minus => Some("-"),
            K::Asterisk => Some("*"),
            K::LessThanEqual => Some("<="),
            K::GreaterThanEqual => Some(">="),

            K::DoubleQuote => Some("\""),
            K::SingleQuote => Some("'"),

            K::Ident => None,
            K::String => Some("\"?\""),
            K::Char => Some("'?'"),

            K::Eof => Some("<EOF>"),
        }
    }
    pub fn from_char(c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(K::OpenParen),
            ')' => Some(K::CloseParen),
            '[' => Some(K::OpenBracket),
            ']' => Some(K::CloseBracket),
            '{' => Some(K::OpenBrace),
            '}' => Some(K::CloseBrace),
            '<' => Some(K::OpenAngle),
            '>' => Some(K::CloseAngle),
            ':' => Some(K::Colon),
            ';' => Some(K::Semicolon),
            '=' => Some(K::Equals),
            '.' => Some(K::Dot),
            ',' => Some(K::Comma),
            '"' => Some(K::DoubleQuote),
            '\'' => Some(K::SingleQuote),
            '+' => Some(K::Plus),
            '-' => Some(K::Minus),
            '*' => Some(K::Asterisk),
            '/' => Some(K::Slash),
            '!' => Some(K::Bang),
            '?' => Some(K::QuestionMark),
            '|' => Some(K::Pipe),
            '&' => Some(K::Ampersand),
            '%' => Some(K::Percent),
            _ => None,
        }
    }
    pub fn token_from_str(str: &str) -> Option<TokenKind> {
        match str {
            "fn" => Some(K::KeywordFn),
            "val" => Some(K::KeywordVal),
            "mut" => Some(K::KeywordMut),
            "and" => Some(K::KeywordAnd),
            "or" => Some(K::KeywordOr),
            "if" => Some(K::KeywordIf),
            "else" => Some(K::KeywordElse),
            "record" => Some(K::KeywordRecord),
            "type" => Some(K::KeywordType),
            "while" => Some(K::KeywordWhile),
            "namespace" => Some(K::KeywordNamespace),
            "intern" => Some(K::KeywordIntern),
            "extern" => Some(K::KeywordExtern),
            "for" => Some(K::KeywordFor),
            "in" => Some(K::KeywordIn),
            "do" => Some(K::KeywordDo),
            "yield" => Some(K::KeywordYield),
            "enum" => Some(K::KeywordEnum),
            "ability" => Some(K::KeywordAbility),
            "impl" => Some(K::KeywordImpl),
            "auto" => Some(K::KeywordAuto),
            "when" => Some(K::KeywordWhen),
            "not" => Some(K::KeywordNot),
            "as" => Some(K::KeywordAs),
            "is" => Some(K::KeywordIs),
            "builtin" => Some(K::KeywordBuiltin),
            "==" => Some(K::EqualsEquals),
            "!=" => Some(K::BangEquals),
            "<=" => Some(K::LessThanEqual),
            ">=" => Some(K::GreaterThanEqual),
            _ => None,
        }
    }
    pub fn is_keyword(&self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match self {
            K::KeywordFn => true,
            K::KeywordVal => true,
            K::KeywordMut => true,
            K::KeywordAnd => true,
            K::KeywordOr => true,
            K::KeywordIf => true,
            K::KeywordWhile => true,
            K::KeywordNamespace => true,
            K::KeywordIntern => true,
            K::KeywordExtern => true,
            K::KeywordFor => true,
            K::KeywordIn => true,
            K::KeywordDo => true,
            K::KeywordYield => true,
            K::KeywordEnum => true,
            K::KeywordAbility => true,
            K::KeywordImpl => true,
            K::KeywordAuto => true,
            K::KeywordIs => true,
            K::KeywordWhen => true,
            K::KeywordAs => true,
            K::KeywordNot => true,
            K::KeywordBuiltin => true,
            _ => false,
        }
    }
    pub fn is_binary_operator(&self) -> bool {
        BinaryOpKind::from_tokenkind(*self).is_some()
    }
    pub fn is_prefix_operator(&self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match self {
            K::Asterisk => true,
            K::KeywordNot => true,
            K::Ampersand => true,
            _ => false,
        }
    }
    pub fn is_postfix_operator(&self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match self {
            K::Dot => true,
            K::OpenBracket => true,
            K::Bang => true,
            K::KeywordAs => true,
            _ => false,
        }
    }
    pub fn is_postfix_type_operator(&self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match self {
            K::QuestionMark => true,
            K::Asterisk => true,
            K::Dot => true,
            _ => false,
        }
    }
}

///
/// https://www.forrestthewoods.com/blog/should-small-rust-structs-be-passed-by-copy-or-by-borrow/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub file_id: u32,
    pub start: u32,
    pub len: u32,
}

impl Span {
    pub const NONE: Span = Span { file_id: 0, start: 0, len: 0 };

    pub fn end(&self) -> u32 {
        self.start + self.len
    }

    pub fn extended(&self, other: Span) -> Span {
        let mut copied = *self;
        let new_end = other.end();
        let new_len = new_end - copied.start;
        copied.len = new_len;
        copied
    }
}

const TOKEN_FLAG_IS_WHITESPACE_PRECEEDED: u64 = 0x01;
#[allow(unused)]
const TOKEN_FLAG_IS_WHITESPACE_FOLLOWED: u64 = 0x02;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: SpanId,
    pub kind: TokenKind,
    pub flags: u64,
}

impl Token {
    pub fn new(kind: TokenKind, span_id: SpanId, whitespace_preceeded: bool) -> Token {
        let flags = if whitespace_preceeded { TOKEN_FLAG_IS_WHITESPACE_PRECEEDED } else { 0 };

        Token { span: span_id, kind, flags }
    }
    pub fn is_whitespace_preceeded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_WHITESPACE_PRECEEDED == TOKEN_FLAG_IS_WHITESPACE_PRECEEDED
    }
}

pub struct Lexer<'a, 'spans> {
    pub file_id: FileId,
    content: Chars<'a>,
    pub spans: &'spans mut Spans,
    pub line_index: u32,
    pub pos: u32,
}

impl<'content, 'spans> Lexer<'content, 'spans> {
    pub fn make(
        input: &'content str,
        spans: &'spans mut Spans,
        file_id: FileId,
    ) -> Lexer<'content, 'spans> {
        Lexer { file_id, content: input.chars(), spans, line_index: 0, pos: 0 }
    }

    fn err(&self, msg: impl Into<String>) -> LexError {
        LexError { msg: msg.into(), line_index: self.line_index }
    }

    pub fn run(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(1024);
        while let Some(tok) = self.eat_token()? {
            tokens.push(tok);
        }
        Ok(tokens)
    }

    fn add_span(&mut self, start: u32, len: u32) -> SpanId {
        self.spans.add(Span { start, len, file_id: self.file_id })
    }

    fn eat_token(&mut self) -> LexResult<Option<Token>> {
        let mut tok_buf = String::new();
        let mut tok_len = 0;
        let mut is_line_comment = false;
        let mut line_comment_start = 0;
        let mut is_string = false;
        let peeked_whitespace = self.peek().is_whitespace();
        let make_token = |lex: &mut Lexer, kind: TokenKind, start: u32, len: u32| {
            let span = lex.add_span(start, len);
            Token::new(kind, span, peeked_whitespace)
        };
        trace!("lex starting new token with prev_skip=false");
        let token = loop {
            let (c, n) = self.peek_with_pos();
            trace!("LEX line={} char={} '{}' buf={}", self.line_index, n, c, tok_buf);
            if is_line_comment {
                if c == '\n' || c == EOF_CHAR {
                    let len = n - line_comment_start - 1;
                    let comment_tok = make_token(self, K::LineComment, line_comment_start, len);
                    break Some(comment_tok);
                } else {
                    self.advance();
                    continue;
                }
            }
            if is_string {
                if c == '"' {
                    self.advance();
                    break Some(make_token(self, K::String, n - tok_len, tok_len));
                } else if c == '\n' {
                    return Err(self.err("No newlines inside strings"));
                } else {
                    tok_len += 1;
                    tok_buf.push(c);
                    self.advance();
                    continue;
                }
            }
            if c == '"' {
                is_string = true;
                self.advance();
                continue;
            }
            if c == EOF_CHAR {
                if !tok_buf.is_empty() {
                    break Some(make_token(self, K::Ident, n - tok_len, tok_len));
                } else {
                    break None;
                }
            }
            if let Some(single_char_tok) = TokenKind::from_char(c) {
                let (_, next) = self.peek_two();
                if !tok_buf.is_empty() {
                    // Break without advancing; we'll have a clear buffer next time
                    // and will advance

                    // Watch out for peeked_whitespace regressions in this case; we were passing 'false'
                    // but I think it should be just using the current value
                    break Some(make_token(self, K::Ident, n - tok_len, tok_len));
                } else if single_char_tok == K::SingleQuote {
                    self.advance(); // eat opening '
                    let mut len = 1;
                    loop {
                        let c = self.next();
                        len += 1;
                        if c == '\'' {
                            break;
                        }
                    }
                    // `n` is the index of the opening quote
                    break Some(make_token(self, TokenKind::Char, n, len));
                } else if single_char_tok == TokenKind::Equals && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(make_token(self, K::EqualsEquals, n, 2));
                } else if single_char_tok == TokenKind::Bang && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(make_token(self, K::BangEquals, n, 2));
                } else if single_char_tok == TokenKind::OpenAngle && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(make_token(self, K::LessThanEqual, n, 2));
                } else if single_char_tok == TokenKind::CloseAngle && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(make_token(self, K::GreaterThanEqual, n, 2));
                } else if single_char_tok == TokenKind::Slash && next == '/' {
                    is_line_comment = true;
                    line_comment_start = n;
                    self.advance();
                    self.advance();
                } else {
                    self.advance();
                    break Some(make_token(self, single_char_tok, n, 1));
                }
            }
            if c.is_whitespace() && !tok_buf.is_empty() {
                if let Some(tok) = TokenKind::token_from_str(&tok_buf) {
                    break Some(make_token(self, tok, n - tok_len, tok_len));
                } else {
                    break Some(make_token(self, TokenKind::Ident, n - tok_len, tok_len));
                }
            }
            if (tok_buf.is_empty() && is_ident_or_num_start(c)) || is_ident_char(c) {
                if tok_buf.len() == 1 && tok_buf.starts_with('_') && c == '_' {
                    return Err(self.err("Identifiers cannot begin with __"));
                }
                tok_len += 1;
                tok_buf.push(c);
            } else if let Some(tok) = TokenKind::token_from_str(&tok_buf) {
                // No longer eat this so the next eat_token call can see it.
                // self.advance();
                break Some(make_token(self, tok, n - tok_len, tok_len));
            }
            self.advance();
        };
        Ok(token)
    }

    fn next(&mut self) -> char {
        self.pos += 1;
        let c = self.content.next().unwrap_or(EOF_CHAR);
        if c == '\n' {
            self.line_index += 1;
        }
        if c == '\r' && self.peek() == '\n' {
            self.content.next();
            self.pos += 1;
        }
        c
    }
    fn peek(&self) -> char {
        self.content.clone().next().unwrap_or(EOF_CHAR)
    }
    fn peek_two(&self) -> (char, char) {
        let mut peek_iter = self.content.clone();
        (peek_iter.next().unwrap_or(EOF_CHAR), peek_iter.next().unwrap_or(EOF_CHAR))
    }
    fn peek_with_pos(&self) -> (char, u32) {
        (self.peek(), self.pos)
    }
    fn advance(&mut self) {
        self.next();
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.'
}

fn is_ident_or_num_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '-'
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexer, Span, TokenKind};
    use crate::lex::{Spans, TokenKind as K};

    use super::Token;

    fn set_up(input: &str) -> anyhow::Result<(Spans, Vec<Token>)> {
        let mut spans = Spans::new();
        let token_vec = Lexer::make(input, &mut spans, 0).run()?;
        Ok((spans, token_vec))
    }

    fn expect_token_kinds(input: &str, expected: Vec<TokenKind>) -> anyhow::Result<()> {
        let mut spans = Spans::new();
        let result = Lexer::make(input, &mut spans, 0).run()?;
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, expected);
        Ok(())
    }

    #[test]
    fn case1() -> anyhow::Result<()> {
        let input = "val x = println(4)";
        expect_token_kinds(
            input,
            vec![
                K::KeywordVal,
                K::Ident,
                K::Equals,
                K::Ident,
                K::OpenParen,
                K::Ident,
                K::CloseParen,
            ],
        )
    }

    #[test]
    fn signed_int() -> anyhow::Result<()> {
        let input = "-43";
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        let span0 = spans.get(tokens[0].span);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);
        let span1 = spans.get(tokens[1].span);
        assert_eq!(span1.start, 1);
        assert_eq!(span1.len, 2);
        assert_eq!(span1.end(), 3);
        assert!(!tokens[1].is_whitespace_preceeded());
        Ok(())
    }

    #[test]
    fn minus_int() -> anyhow::Result<()> {
        let input = "- 43";
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        let span0 = spans.get(tokens[0].span);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);

        let span1 = spans.get(tokens[1].span);
        assert_eq!(span1.start, 2);
        assert_eq!(span1.len, 2);
        assert_eq!(span1.end(), 4);
        assert!(tokens[1].is_whitespace_preceeded());
        Ok(())
    }

    #[test]
    fn literal_string() -> anyhow::Result<()> {
        let input = "val x = println(\"foobear\")";
        expect_token_kinds(
            input,
            vec![
                K::KeywordVal,
                K::Ident,
                K::Equals,
                K::Ident,
                K::OpenParen,
                K::String,
                K::CloseParen,
            ],
        )
    }

    #[test]
    fn ending_ident() -> anyhow::Result<()> {
        let input = "val x = a + b";
        expect_token_kinds(
            input,
            vec![K::KeywordVal, K::Ident, K::Equals, K::Ident, K::Plus, K::Ident],
        )
    }

    #[test]
    fn double_equals() -> anyhow::Result<()> {
        let input = "a == b";
        expect_token_kinds(input, vec![K::Ident, K::EqualsEquals, K::Ident])
    }

    #[test]
    fn line_comment() -> anyhow::Result<()> {
        let input = r#"// Hello, world
        val foo: int = 74;
        // <test harness> expected output
        //
        "#;
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(spans.get(tokens[0].span), Span { start: 0, len: 14, file_id: 0 });
        assert_eq!(&input[0..5], "// He");
        assert_eq!(
            vec![
                K::LineComment,
                K::KeywordVal,
                K::Ident,
                K::Colon,
                K::Ident,
                K::Equals,
                K::Ident,
                K::Semicolon,
                K::LineComment,
                K::LineComment
            ],
            kinds
        );
        Ok(())
    }
}
