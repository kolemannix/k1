use std::fmt;
use std::str::Chars;

use crate::parse::BinaryOpKind;
use log::trace;
use TokenKind::*;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token =
    Token { span: Span { file_id: 0, start: 0, end: 0, line: 0 }, kind: TokenKind::Eof };

// TODO Take any iterator of Tokens, not just one that came from a Vec
pub struct TokenIter {
    iter: std::vec::IntoIter<Token>,
}

impl TokenIter {
    pub fn make(data: Vec<Token>) -> TokenIter {
        TokenIter { iter: data.into_iter() }
    }
    pub fn next(&mut self) -> Token {
        self.iter.next().unwrap_or(EOF_TOKEN)
    }
    pub fn advance(&mut self) {
        self.next();
    }
    pub fn peek(&self) -> Token {
        let peeked = self.iter.clone().next();
        peeked.unwrap_or(EOF_TOKEN)
    }
    pub fn peek_two(&self) -> (Token, Token) {
        let mut peek_iter = self.iter.clone();
        let p1 = peek_iter.next().unwrap_or(EOF_TOKEN);
        let p2 = peek_iter.next().unwrap_or(EOF_TOKEN);
        (p1, p2)
    }
    pub fn peek_three(&self) -> (Token, Token, Token) {
        let mut peek_iter = self.iter.clone();
        let p1 = peek_iter.next().unwrap_or(EOF_TOKEN);
        let p2 = peek_iter.next().unwrap_or(EOF_TOKEN);
        let p3 = peek_iter.next().unwrap_or(EOF_TOKEN);
        (p1, p2, p3)
    }
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Text,

    KeywordFn,
    KeywordReturn,
    KeywordVal,
    KeywordMut,
    KeywordAnd,
    KeywordOr,
    KeywordIf,
    KeywordElse,
    KeywordRecord,
    KeywordType,

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
    Dot,
    Comma,

    DoubleQuote,
    SingleQuote,

    // Infix Operators, hardcoded precedence
    Plus,
    Asterisk,

    /// Not really a token but allows us to avoid Option<Token> everywhere
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.get_repr().unwrap_or("<ident>"))
    }
}

impl TokenKind {
    pub fn get_repr(&self) -> Option<&'static str> {
        match self {
            KeywordFn => Some("fn"),
            KeywordReturn => Some("return"),
            KeywordVal => Some("val"),
            KeywordMut => Some("mut"),
            KeywordAnd => Some("and"),
            KeywordOr => Some("or"),
            KeywordIf => Some("if"),
            KeywordElse => Some("else"),
            KeywordType => Some("type"),
            KeywordRecord => Some("record"),

            Slash => Some("/"),
            LineComment => Some("//"),

            OpenParen => Some("("),
            CloseParen => Some(")"),
            OpenBracket => Some("["),
            CloseBracket => Some("]"),
            OpenBrace => Some("{"),
            CloseBrace => Some("}"),
            OpenAngle => Some("<"),
            CloseAngle => Some(">"),
            Colon => Some(":"),
            Semicolon => Some(";"),
            Equals => Some("="),
            Dot => Some("."),
            Comma => Some(","),

            Plus => Some("+"),
            Asterisk => Some("*"),

            DoubleQuote => Some("\""),
            SingleQuote => Some("'"),

            Text => None,

            Eof => Some("<EOF>"),
        }
    }
    pub fn from_char(c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '[' => Some(OpenBracket),
            ']' => Some(CloseBracket),
            '{' => Some(OpenBrace),
            '}' => Some(CloseBrace),
            '<' => Some(OpenAngle),
            '>' => Some(CloseAngle),
            ':' => Some(Colon),
            ';' => Some(Semicolon),
            '=' => Some(Equals),
            '.' => Some(Dot),
            ',' => Some(Comma),
            '"' => Some(DoubleQuote),
            '\'' => Some(SingleQuote),
            '+' => Some(Plus),
            '*' => Some(Asterisk),
            '/' => Some(Slash),
            _ => None,
        }
    }
    pub fn keyword_from_str(str: &str) -> Option<TokenKind> {
        match str {
            "fn" => Some(KeywordFn),
            "return" => Some(KeywordReturn),
            "val" => Some(KeywordVal),
            "mut" => Some(KeywordMut),
            "and" => Some(KeywordAnd),
            "or" => Some(KeywordOr),
            "if" => Some(KeywordIf),
            "else" => Some(KeywordElse),
            "type" => Some(KeywordType),
            "record" => Some(KeywordRecord),
            _ => None,
        }
    }
    pub fn is_keyword(&self) -> bool {
        match self {
            KeywordFn => true,
            KeywordReturn => true,
            KeywordVal => true,
            KeywordMut => true,
            KeywordAnd => true,
            KeywordOr => true,
            KeywordIf => true,
            _ => false,
        }
    }
    pub fn is_binary_operator(&self) -> bool {
        BinaryOpKind::from_tokenkind(*self).is_some()
    }
    pub fn is_postfix_operator(&self) -> bool {
        match self {
            Dot => true,
            OpenBracket => true,
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
        return copied;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn make(kind: TokenKind, line_index: u32, start: u32, len: u32) -> Token {
        let span = Span { start, end: start + len, line: line_index, file_id: 0 };
        Token { span, kind }
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
        loop {
            let (c, n) = self.peek_with_pos();
            trace!("LEX line={} char={} '{}' buf={}", self.line_index, n, c, tok_buf);
            if is_line_comment {
                if c == '\n' || c == EOF_CHAR {
                    let len = n - line_comment_start - 1;
                    let comment_tok = Token::make(
                        TokenKind::LineComment,
                        self.line_index,
                        line_comment_start,
                        len,
                    );
                    self.advance();
                    break Some(comment_tok);
                } else {
                    self.advance();
                    continue;
                }
            }
            if c == EOF_CHAR {
                if !tok_buf.is_empty() {
                    break Some(Token::make(
                        TokenKind::Text,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                    ));
                } else {
                    break None;
                }
            }
            if let Some(single_char_tok) = TokenKind::from_char(c) {
                if !tok_buf.is_empty() {
                    // Break without advancing; we'll have a clear buffer next time
                    // and will advance
                    break Some(Token::make(
                        TokenKind::Text,
                        self.line_index,
                        n - tok_len,
                        tok_len,
                    ));
                } else if single_char_tok == TokenKind::Slash && self.peek() == '/' {
                    is_line_comment = true;
                    line_comment_start = n;
                    self.advance();
                    self.advance();
                } else {
                    self.advance();
                    break Some(Token::make(single_char_tok, self.line_index, n, 1));
                }
            }
            if c.is_whitespace() {
                if !tok_buf.is_empty() {
                    self.advance();
                    if let Some(tok) = TokenKind::keyword_from_str(&tok_buf) {
                        break Some(Token::make(tok, self.line_index, n - tok_len, tok_len));
                    } else {
                        break Some(Token::make(
                            TokenKind::Text,
                            self.line_index,
                            n - tok_len,
                            tok_len,
                        ));
                    }
                }
            }
            if (tok_buf.is_empty() && is_ident_start(c)) || is_ident_char(c) {
                tok_len += 1;
                tok_buf.push(c);
            } else if let Some(tok) = TokenKind::keyword_from_str(&tok_buf) {
                self.advance();
                break Some(Token::make(tok, self.line_index, n - tok_len, tok_len));
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

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexer, Span, TokenKind};
    use crate::lex::{TokenIter, TokenKind::*};

    #[test]
    fn case1() {
        let input = "val x = println(4)";
        let result = Lexer::make(&input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![KeywordVal, Text, Equals, Text, OpenParen, Text, CloseParen])
    }

    #[test]
    fn literal_string() {
        let input = "val x = println(\"foobear\")";
        let result = Lexer::make(&input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                KeywordVal,
                Text,
                Equals,
                Text,
                OpenParen,
                DoubleQuote,
                Text,
                DoubleQuote,
                CloseParen
            ]
        )
    }

    #[test]
    fn ending_ident() {
        let input = "val x = a + b";
        let result = Lexer::make(&input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![KeywordVal, Text, Equals, Text, Plus, Text])
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
        let _token_iter = TokenIter::make(result.clone());
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(result[0].span, Span { start: 0, end: 14, line: 0, file_id: 0 });
        assert_eq!(&input[0..5], "// He");
        assert_eq!(
            vec![
                LineComment,
                KeywordVal,
                Text,
                Colon,
                Text,
                Equals,
                Text,
                Semicolon,
                LineComment,
                LineComment
            ],
            kinds
        )
    }
}
