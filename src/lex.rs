use std::fmt;
use std::str::Chars;
use std::vec::IntoIter;

use crate::trace;
use TokenKind::*;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token = Token { start: 0, len: 0, line_num: 0, kind: TokenKind::EOF };

pub struct Tokens {
    iter: IntoIter<Token>,
}

impl Tokens {
    pub fn make(data: Vec<Token>) -> Tokens {
        Tokens { iter: data.into_iter() }
    }
    pub fn next(&mut self) -> Token {
        self.iter.next().unwrap_or(EOF_TOKEN)
    }
    pub fn advance(&mut self) -> () {
        self.next();
        ()
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

    LineComment,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
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
    EOF,
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

            OpenParen => Some("("),
            CloseParen => Some(")"),
            OpenBracket => Some("["),
            CloseBracket => Some("]"),
            OpenBrace => Some("{"),
            CloseBrace => Some("}"),
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

            LineComment => None,

            EOF => Some("<EOF>"),
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
            ':' => Some(Colon),
            ';' => Some(Semicolon),
            '=' => Some(Equals),
            '.' => Some(Dot),
            ',' => Some(Comma),
            '"' => Some(DoubleQuote),
            '\'' => Some(SingleQuote),
            '+' => Some(Plus),
            '*' => Some(Asterisk),
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
    pub fn is_infix_operator(&self) -> bool {
        match self {
            Plus => true,
            Asterisk => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub start: u32,
    pub len: u32,
    pub line_num: u32,
    pub kind: TokenKind,
}

impl Token {
    pub fn make(kind: TokenKind, line_index: u32, start: u32, len: u32) -> Token {
        Token { start, len, line_num: line_index, kind }
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
        loop {
            let (c, n) = self.peek_with_pos();
            trace!("LEX line={} char={} '{}' buf={}", self.line_index, n, c, tok_buf);
            if c == EOF_CHAR {
                if !tok_buf.is_empty() {
                    break Some(Token::make(TokenKind::Text, self.line_index, n - tok_len, tok_len));
                } else {
                    break None;
                }
            }
            if let Some(single_char_tok) = TokenKind::from_char(c) {
                if !tok_buf.is_empty() {
                    break Some(Token::make(TokenKind::Text, self.line_index, n - tok_len, tok_len));
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
                        break Some(Token::make(TokenKind::Text, self.line_index, n - tok_len, tok_len));
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
    fn advance(&mut self) -> () {
        self.next();
        ()
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
    use crate::lex::TokenKind::*;
    use crate::lex::{Lexer, TokenKind};

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
        assert_eq!(kinds, vec![KeywordVal, Text, Equals, Text, OpenParen, DoubleQuote, Text, DoubleQuote, CloseParen])
    }

    #[test]
    fn ending_ident() {
        let input = "val x = a + b";
        let result = Lexer::make(&input).run();
        let kinds: Vec<TokenKind> = result.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![KeywordVal, Text, Equals, Text, Plus, Text])
    }
}
