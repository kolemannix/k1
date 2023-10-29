use std::fmt;
use std::str::Chars;

use crate::typer::BinaryOpKind;
use log::trace;
use TokenKind as K;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token =
    Token { span: Span { file_id: 0, start: 0, end: 0, line: 0 }, kind: TokenKind::Eof, flags: 0 };

pub struct TokenIter<'toks> {
    iter: std::slice::Iter<'toks, Token>,
}

impl<'toks> TokenIter<'toks> {
    pub fn make(data: &'toks [Token]) -> TokenIter<'toks> {
        TokenIter { iter: data.iter() }
    }
    pub fn next(&mut self) -> Token {
        *self.iter.next().unwrap_or(&EOF_TOKEN)
    }
    pub fn advance(&mut self) {
        self.next();
    }
    pub fn peek(&self) -> Token {
        let peeked = self.iter.clone().next();
        *peeked.unwrap_or(&EOF_TOKEN)
    }
    pub fn peek_two(&self) -> (Token, Token) {
        let mut peek_iter = self.iter.clone();
        let p1 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        let p2 = *peek_iter.next().unwrap_or(&EOF_TOKEN);
        (p1, p2)
    }
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

    Slash,
    LineComment,

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
    Dot,
    Comma,
    Bang,

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
            K::Dot => Some("."),
            K::Comma => Some(","),
            K::Bang => Some("!"),

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
            "==" => Some(K::EqualsEquals),
            "<=" => Some(K::LessThanEqual),
            ">=" => Some(K::GreaterThanEqual),
            _ => None,
        }
    }
    pub fn is_keyword(&self) -> bool {
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
            _ => false,
        }
    }
    pub fn is_binary_operator(&self) -> bool {
        BinaryOpKind::from_tokenkind(*self).is_some()
    }
    pub fn is_postfix_operator(&self) -> bool {
        match self {
            K::Dot => true,
            K::OpenBracket => true,
            K::Bang => true,
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
    pub end: u32,
    pub line: u32,
}

impl Span {
    pub const NONE: Span = Span { file_id: 0, start: 0, end: 0, line: 0 };

    pub fn extended(&self, other: Span) -> Span {
        let mut copied = *self;
        copied.end = other.end;
        copied
    }
}

const TOKEN_FLAG_IS_WHITESPACE_PRECEEDED: u64 = 0x01;
const TOKEN_FLAG_IS_WHITESPACE_FOLLOWED: u64 = 0x02;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    pub flags: u64,
}

impl Token {
    pub fn new(
        kind: TokenKind,
        line_index: u32,
        start: u32,
        len: u32,
        whitespace_preceeded: bool,
    ) -> Token {
        let span = Span { start, end: start + len, line: line_index, file_id: 0 };
        let flags = if whitespace_preceeded { TOKEN_FLAG_IS_WHITESPACE_PRECEEDED } else { 0 };

        Token { span, kind, flags }
    }
    pub fn is_whitespace_preceeded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_WHITESPACE_PRECEEDED == TOKEN_FLAG_IS_WHITESPACE_PRECEEDED
    }
}

pub struct Lexer<'a> {
    content: Chars<'a>,
    pub line_index: u32,
    pub pos: u32,
}

impl Lexer<'_> {
    pub fn make(input: &str) -> Lexer {
        Lexer { content: input.chars(), line_index: 0, pos: 0 }
    }
    pub fn run(&mut self) -> Vec<Token> {
        let mut tokens = Vec::with_capacity(1024);
        while let Some(tok) = self.eat_token() {
            tokens.push(tok);
        }
        tokens
    }

    fn eat_token(&mut self) -> Option<Token> {
        let mut tok_buf = String::new();
        let mut tok_len = 0;
        let mut is_line_comment = false;
        let mut line_comment_start = 0;
        let mut is_string = false;
        let peeked_whitespace = self.peek().is_whitespace();
        log::trace!("lex starting new token with prev_skip=false");
        loop {
            let (c, n) = self.peek_with_pos();
            trace!("LEX line={} char={} '{}' buf={}", self.line_index, n, c, tok_buf);
            if is_line_comment {
                if c == '\n' || c == EOF_CHAR {
                    let len = n - line_comment_start - 1;
                    let comment_tok = Token::new(
                        K::LineComment,
                        self.line_index,
                        line_comment_start,
                        len,
                        peeked_whitespace,
                    );
                    break Some(comment_tok);
                } else {
                    self.advance();
                    continue;
                }
            }
            if is_string {
                if c == '"' {
                    self.advance();
                    break Some(Token::new(
                        K::String,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                        peeked_whitespace,
                    ));
                } else if c == '\n' {
                    panic!("No newlines inside strings")
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
                    break Some(Token::new(
                        TokenKind::Ident,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                        peeked_whitespace,
                    ));
                } else {
                    break None;
                }
            }
            if let Some(single_char_tok) = TokenKind::from_char(c) {
                let (_, next) = self.peek_two();
                if !tok_buf.is_empty() {
                    // Break without advancing; we'll have a clear buffer next time
                    // and will advance
                    break Some(Token::new(
                        TokenKind::Ident,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                        false,
                    ));
                } else if single_char_tok == TokenKind::SingleQuote {
                    self.advance(); // eat opening '
                    let c = self.next(); // eat the char itself
                                         //
                                         //
                                         // TODO: Support escapes in char literal
                    let quote = self.next(); // eat closing '
                    assert!(quote == '\''); // lmao that's meta
                                            //
                                            // `n` is the index of the opening quote
                                            // n + 1 will be the index of the char we care about
                                            // length will be 1
                    break Some(Token::new(
                        TokenKind::Char,
                        self.line_index,
                        n + 1,
                        1,
                        peeked_whitespace,
                    ));
                } else if single_char_tok == TokenKind::Equals && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(Token::new(
                        K::EqualsEquals,
                        self.line_index,
                        n,
                        2,
                        peeked_whitespace,
                    ));
                } else if single_char_tok == TokenKind::OpenAngle && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(Token::new(
                        K::LessThanEqual,
                        self.line_index,
                        n,
                        2,
                        peeked_whitespace,
                    ));
                } else if single_char_tok == TokenKind::CloseAngle && next == '=' {
                    self.advance();
                    self.advance();
                    break Some(Token::new(
                        K::GreaterThanEqual,
                        self.line_index,
                        n,
                        2,
                        peeked_whitespace,
                    ));
                } else if single_char_tok == TokenKind::Slash && next == '/' {
                    is_line_comment = true;
                    line_comment_start = n;
                    self.advance();
                    self.advance();
                } else {
                    self.advance();
                    break Some(Token::new(
                        single_char_tok,
                        self.line_index,
                        n,
                        1,
                        peeked_whitespace,
                    ));
                }
            }
            if c.is_whitespace() && !tok_buf.is_empty() {
                // No longer eat this so the next eat_token call can see it.
                // self.advance();
                if let Some(tok) = TokenKind::token_from_str(&tok_buf) {
                    break Some(Token::new(
                        tok,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                        peeked_whitespace,
                    ));
                } else {
                    break Some(Token::new(
                        TokenKind::Ident,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                        peeked_whitespace,
                    ));
                }
            }
            if (tok_buf.is_empty() && is_ident_or_num_start(c)) || is_ident_char(c) {
                tok_len += 1;
                tok_buf.push(c);
            } else if let Some(tok) = TokenKind::token_from_str(&tok_buf) {
                // No longer eat this so the next eat_token call can see it.
                // self.advance();
                break Some(Token::new(
                    tok,
                    self.line_index,
                    n - tok_len,
                    tok_len,
                    peeked_whitespace,
                ));
            }
            self.advance();
        }
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
    fn next_with_pos(&mut self) -> (char, u32) {
        let old_pos = self.pos;
        (self.next(), old_pos)
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
    use crate::lex::TokenKind as K;
    use crate::lex::{Lexer, Span, TokenKind};

    #[test]
    fn case1() {
        let input = "val x = println(4)";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                K::KeywordVal,
                K::Ident,
                K::Equals,
                K::Ident,
                K::OpenParen,
                K::Ident,
                K::CloseParen
            ]
        )
    }

    #[test]
    fn signed_int() {
        let input = "-43";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        assert_eq!(result[0].span.start, 0);
        assert_eq!(result[0].span.end, 1);
        assert_eq!(result[1].span.start, 1);
        assert_eq!(result[1].span.end, 3);
        assert!(!result[1].is_whitespace_preceeded());
    }

    #[test]
    fn minus_int() {
        let input = "- 43";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        assert_eq!(result[0].span.start, 0);
        assert_eq!(result[0].span.end, 1);
        assert_eq!(result[1].span.start, 2);
        assert_eq!(result[1].span.end, 4);
        assert!(result[1].is_whitespace_preceeded());
    }

    #[test]
    fn literal_string() {
        let input = "val x = println(\"foobear\")";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                K::KeywordVal,
                K::Ident,
                K::Equals,
                K::Ident,
                K::OpenParen,
                K::String,
                K::CloseParen
            ]
        )
    }

    #[test]
    fn ending_ident() {
        let input = "val x = a + b";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::KeywordVal, K::Ident, K::Equals, K::Ident, K::Plus, K::Ident])
    }

    #[test]
    fn double_equals() {
        let input = "a == b";
        let result = Lexer::make(input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Ident, K::EqualsEquals, K::Ident])
    }

    #[test]
    fn line_comment() {
        let input = r#"// Hello, world
        val foo: int = 74;
        // <test harness> expected output
        //
        "#;
        let mut lexer = Lexer::make(input);
        let result = lexer.run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(result[0].span, Span { start: 0, end: 14, line: 0, file_id: 0 });
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
        )
    }
}
