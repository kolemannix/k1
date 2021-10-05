use std::slice::Iter;
use std::str::Chars;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token = Token {
    start: 0,
    len: 0,
    line_num: 0,
    kind: TokenKind::EOF,
};

pub struct Tokens<'a, 'b> {
    source: &'b str,
    iter: Iter<'a, Token>,
}

impl<'a, 'b> Tokens<'a, 'b> {
    pub fn chars_at(&self, pos_start: usize, len: usize) -> &str {
        &self.source[pos_start..pos_start + len]
    }
    pub fn make(source: &'b str, data: &'a [Token]) -> Tokens<'a, 'b> {
        Tokens { iter: data.iter(), source }
    }
    pub fn next(&mut self) -> &Token {
        self.iter.next().unwrap_or(&EOF_TOKEN)
    }
    pub fn advance(&mut self) -> () {
        self.next();
        ()
    }
    pub fn peek(&self) -> &Token {
        let peeked = self.iter.clone().next();
        peeked.unwrap_or(&EOF_TOKEN)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Keyword {
    Fn,
    Return,
    Val,
    Mut,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
    Text,
    Literal,

    KeywordFn,
    KeywordReturn,
    KeywordVal,
    KeywordMut,

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

    /// Not really a token but allows us to avoid Option<Token> everywhere
    EOF,
}

impl TokenKind {
    pub fn from_char(c: char) -> Option<TokenKind> {
        match c {
            '(' => Some(TokenKind::OpenParen),
            ')' => Some(TokenKind::CloseParen),
            '[' => Some(TokenKind::OpenBracket),
            ']' => Some(TokenKind::CloseBracket),
            '{' => Some(TokenKind::OpenBrace),
            '}' => Some(TokenKind::CloseBrace),
            ':' => Some(TokenKind::Colon),
            ';' => Some(TokenKind::Semicolon),
            '=' => Some(TokenKind::Equals),
            _ => None
        }
    }
    pub fn keyword_from_str(str: &str) -> Option<TokenKind> {
        match str {
            "fn" => Some(TokenKind::KeywordFn),
            "return" => Some(TokenKind::KeywordReturn),
            "val" => Some(TokenKind::KeywordVal),
            "mut" => Some(TokenKind::KeywordMut),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub start: usize,
    pub len: usize,
    pub line_num: usize,
    pub kind: TokenKind,
}

impl Token {
    pub fn make(kind: TokenKind, line_num: usize, start: usize, len: usize) -> Token {
        Token {
            start,
            len,
            line_num,
            kind,
        }
    }
}

pub struct Lexer<'a> {
    content: Chars<'a>,
    pub line_index: usize,
    pub pos: usize,
}

impl Lexer<'_> {
    pub fn make(input: &str) -> Lexer {
        Lexer {
            content: input.chars(),
            line_index: 0,
            pos: 0,
        }
    }
    pub fn next(&mut self) -> char {
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
    pub fn next_with_pos(&mut self) -> (char, usize) {
        let old_pos = self.pos;
        (self.next(), old_pos)
    }
    pub fn peek(&self) -> char {
        self.content.clone().next().unwrap_or(EOF_CHAR)
    }
    pub fn peek_two(&self) -> (char, char) {
        let mut peek_iter = self.content.clone();
        (peek_iter.next().unwrap_or(EOF_CHAR), peek_iter.next().unwrap_or(EOF_CHAR))
    }
    pub fn peek_with_pos(&self) -> (char, usize) {
        (self.peek(), self.pos)
    }
    pub fn advance(&mut self) -> () {
        self.next();
        ()
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn eat_token(lexer: &mut Lexer) -> Option<Token> {
    let mut tok_buf = String::new();
    let mut tok_len = 0;
    loop {
        let (c, n) = lexer.peek_with_pos();
        if c == EOF_CHAR {
            break None;
        }
        if let Some(single_char_tok) = TokenKind::from_char(c) {
            if !tok_buf.is_empty() {
                break Some(Token::make(TokenKind::Text, lexer.line_index, n - tok_len, tok_len));
            } else {
                lexer.advance();
                break Some(Token::make(single_char_tok, lexer.line_index, n, 1));
            }
        }
        if c.is_whitespace() {
            if !tok_buf.is_empty() {
                break Some(Token::make(TokenKind::Text, lexer.line_index, n - tok_len, tok_len));
            }
        }
        if is_ident_char(c) {
            println!("{} {}", n, c);
            if (tok_buf.is_empty() && is_ident_start(c)) || is_ident_char(c) {
                lexer.advance();
                tok_len += 1;
                tok_buf.push(c);
            }
            if let Some(tok) = TokenKind::keyword_from_str(&tok_buf) {
                lexer.advance();
                break Some(Token::make(tok, lexer.line_index, n - tok_buf.len(), tok_buf.len()));
            }
        }
        lexer.advance();
        println!("Skipping {} {}", n, c);
    }
}

pub fn tokenize(lexer: &mut Lexer) -> Vec<Token> {
    let mut tokens = Vec::with_capacity(1024);
    while let Some(tok) = eat_token(lexer) {
        tokens.push(tok);
    }
    tokens
}