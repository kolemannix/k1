// Copyright (c) 2025 knix
// All rights reserved.

use std::fmt;
use std::fmt::{Display, Formatter};

use crate::nz_u32_id;
use crate::parse::BinaryOpKind;
use crate::parse::FileId;
use crate::pool::VPool;
use TokenKind as K;
use log::debug;

pub const EOF_CHAR: char = '\0';
pub const EOF_TOKEN: Token = Token { span: SpanId::NONE, kind: TokenKind::Eof, flags: 0 };

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub file_id: FileId,
    pub span: SpanId,
}

pub type LexResult<A> = anyhow::Result<A, LexError>;

nz_u32_id!(SpanId);
impl SpanId {
    pub const NONE: SpanId = Self::ONE;
}

pub struct Spans {
    pub span_pool: VPool<Span, SpanId>,
}

impl Spans {
    pub fn new() -> Spans {
        let mut pool = VPool::make_with_hint("spans", 131072);
        pool.add(Span::NONE);
        Spans { span_pool: pool }
    }

    pub fn add(&mut self, span: Span) -> SpanId {
        self.span_pool.add(span)
    }

    pub fn get(&self, id: SpanId) -> Span {
        *self.span_pool.get(id)
    }

    pub fn get_end(&self, id: SpanId) -> u32 {
        let s = self.span_pool.get(id);
        s.end()
    }

    #[inline]
    pub fn extend(&mut self, span1: SpanId, span2: SpanId) -> SpanId {
        let mut span1 = self.get(span1);
        span1.extend_to(self.get_end(span2));
        self.add(span1)
    }
}

impl Default for Spans {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "LexError in file {}: {}", self.file_id, self.message)
    }
}
impl std::error::Error for LexError {}

pub struct TokenIter<'toks> {
    cursor: usize,
    tokens: &'toks [Token],
}

impl<'toks> TokenIter<'toks> {
    pub fn make(data: &'toks [Token]) -> TokenIter<'toks> {
        TokenIter { cursor: 0, tokens: data }
    }

    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Token {
        let tok = self.tokens[self.cursor];
        self.cursor += 1;
        tok
    }

    pub fn cursor_position(&self) -> usize {
        self.cursor
    }

    #[inline]
    pub fn advance_n(&mut self, n: usize) {
        self.cursor += n
    }

    #[inline]
    pub fn advance(&mut self) {
        self.cursor += 1;
    }

    #[inline]
    pub fn retreat(&mut self) {
        self.cursor -= 1;
    }

    #[inline]
    fn peek_n(&self, n: i64) -> Token {
        let pos = self.cursor as i64 + n;
        self.tokens.get(pos as usize).copied().unwrap_or(EOF_TOKEN)
    }

    #[inline]
    pub fn peek(&self) -> Token {
        self.peek_n(0)
    }

    #[inline]
    pub fn peek_two(&self) -> (Token, Token) {
        (self.peek_n(0), self.peek_n(1))
    }

    #[inline]
    pub fn peek_three(&self) -> (Token, Token, Token) {
        (self.peek_n(0), self.peek_n(1), self.peek_n(2))
    }

    #[inline]
    pub fn peek_back(&self) -> Token {
        self.peek_n(-1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringDelimKind {
    Backtick,
    DoubleQuote,
}

impl StringDelimKind {
    pub fn char(&self) -> char {
        match self {
            StringDelimKind::Backtick => '`',
            StringDelimKind::DoubleQuote => '"',
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Ident,
    /// A completed string
    String {
        delim: StringDelimKind,
        interp_exprs: bool,
    },
    /// Used in string interpolation; any not-fully-completed string, for example:
    /// Could be the initial segment, a connecting segment between 2 interpolations,
    StringUnterminated {
        delim: StringDelimKind,
        interp_exprs: bool,
    },

    Char,

    KeywordFn,
    KeywordLet,
    KeywordMut,
    KeywordAnd,
    KeywordOr,
    KeywordIf,
    KeywordElse,
    KeywordDefType,
    KeywordWhile,
    KeywordLoop,
    KeywordNamespace,
    KeywordIntern,
    KeywordFor,
    KeywordIn,
    KeywordAbility,
    KeywordImpl,
    KeywordIs,
    KeywordSwitch,
    KeywordNot,
    KeywordBuiltin,
    KeywordWhere,
    KeywordContext,
    KeywordUse,
    KeywordRequire,
    KeywordDefer,

    Slash,
    LineComment,

    // Symbols
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    LAngle,
    LAngleLAngle,
    RAngle,
    RAngleRAngle,
    Colon,
    ColonEquals,
    Semicolon,
    Equals,
    EqualsEquals,
    BangEquals,
    Dot,
    Comma,
    Bang,
    QuestionMark,
    Pipe,
    PipePipe,
    Amp,
    AmpAmp,
    Percent,
    BackSlash,
    Hash,
    At,

    Caret,

    DoubleQuote,
    SingleQuote,

    Plus,
    Minus,
    Asterisk,
    LessEqual,
    GreaterEqual,
    LThinArrow,
    RThinArrow,

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
        self.get_repr()
    }
}

impl TokenKind {
    pub const STRING_DQ_INTERP: TokenKind =
        TokenKind::String { delim: StringDelimKind::DoubleQuote, interp_exprs: true };
    pub const STRING_DQ_PLAIN: TokenKind =
        TokenKind::String { delim: StringDelimKind::DoubleQuote, interp_exprs: false };

    pub const STRING_BT_INTERP: TokenKind =
        TokenKind::String { delim: StringDelimKind::Backtick, interp_exprs: true };
    pub const STRING_BT_PLAIN: TokenKind =
        TokenKind::String { delim: StringDelimKind::Backtick, interp_exprs: false };

    pub const STRING_UNTERM_DQ_INTERP: TokenKind =
        TokenKind::StringUnterminated { delim: StringDelimKind::DoubleQuote, interp_exprs: true };

    pub const STRING_UNTERM_BT_INTERP: TokenKind =
        TokenKind::StringUnterminated { delim: StringDelimKind::Backtick, interp_exprs: true };
    pub const STRING_UNTERM_BT_PLAIN: TokenKind =
        TokenKind::StringUnterminated { delim: StringDelimKind::Backtick, interp_exprs: false };

    pub fn get_repr(&self) -> &'static str {
        match self {
            K::KeywordFn => "fn",
            K::KeywordLet => "let",
            K::KeywordMut => "mut",
            K::KeywordAnd => "and",
            K::KeywordOr => "or",
            K::KeywordIf => "if",
            K::KeywordElse => "else",
            K::KeywordDefType => "deftype",
            K::KeywordWhile => "while",
            K::KeywordLoop => "loop",
            K::KeywordNamespace => "ns",
            K::KeywordIntern => "intern",
            K::KeywordFor => "for",
            K::KeywordIn => "in",
            K::KeywordAbility => "ability",
            K::KeywordImpl => "impl",
            K::KeywordIs => "is",
            K::KeywordSwitch => "switch",
            K::KeywordNot => "not",
            K::KeywordBuiltin => "builtin",
            K::KeywordWhere => "where",
            K::KeywordContext => "context",
            K::KeywordUse => "use",
            K::KeywordRequire => "require",
            K::KeywordDefer => "defer",

            K::Slash => "/",
            K::LineComment => "//",

            K::OpenParen => "(",
            K::CloseParen => ")",
            K::OpenBracket => "[",
            K::CloseBracket => "]",
            K::OpenBrace => "{",
            K::CloseBrace => "}",
            K::LAngle => "<",
            K::LAngleLAngle => "<<",
            K::RAngle => ">",
            K::RAngleRAngle => ">>",
            K::Colon => ":",
            K::ColonEquals => ":=",
            K::Semicolon => ";",
            K::Equals => "=",
            K::EqualsEquals => "==",
            K::BangEquals => "!=",
            K::Dot => ".",
            K::Comma => ",",
            K::Bang => "!",
            K::QuestionMark => "?",
            K::Pipe => "|",
            K::PipePipe => "||",
            K::Amp => "&",
            K::AmpAmp => "&&",
            K::Percent => "%",
            K::BackSlash => "\\",
            K::Hash => "#",
            K::At => "@",
            K::Caret => "^",

            K::Plus => "+",
            K::Minus => "-",
            K::Asterisk => "*",
            K::LessEqual => "<=",
            K::GreaterEqual => ">=",
            K::LThinArrow => "<-",
            K::RThinArrow => "->",

            K::DoubleQuote => "\"",
            K::SingleQuote => "singleq",

            K::Ident => "<ident>",
            K::String { delim: StringDelimKind::Backtick, interp_exprs } => {
                if *interp_exprs {
                    "<f`string`>"
                } else {
                    "<`string`>"
                }
            }
            K::String { delim: StringDelimKind::DoubleQuote, interp_exprs } => {
                if *interp_exprs {
                    "<f\"string\">"
                } else {
                    "<\"string\">"
                }
            }
            K::StringUnterminated { delim: StringDelimKind::Backtick, interp_exprs } => {
                if *interp_exprs {
                    "<f`string...>"
                } else {
                    "<`string...>"
                }
            }
            K::StringUnterminated { delim: StringDelimKind::DoubleQuote, interp_exprs } => {
                if *interp_exprs {
                    "<f\"string...>"
                } else {
                    "<\"string...>"
                }
            }
            K::Char => "<char>",

            K::Eof => "<EOF>",
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
            '<' => Some(K::LAngle),
            '>' => Some(K::RAngle),
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
            '&' => Some(K::Amp),
            '%' => Some(K::Percent),
            '\\' => Some(K::BackSlash),
            '#' => Some(K::Hash),
            '@' => Some(K::At),
            '^' => Some(K::Caret),
            _ => None,
        }
    }

    pub fn compound_from_start_and_char(start: TokenKind, c: char) -> Option<TokenKind> {
        match (start, c) {
            (TokenKind::Equals, '=') => Some(K::EqualsEquals),
            (TokenKind::Bang, '=') => Some(K::BangEquals),
            (TokenKind::LAngle, '=') => Some(K::LessEqual),
            (TokenKind::RAngle, '=') => Some(K::GreaterEqual),
            (TokenKind::Colon, '=') => Some(K::ColonEquals),

            (TokenKind::Slash, '/') => Some(TokenKind::LineComment),

            (TokenKind::Pipe, '|') => Some(TokenKind::PipePipe),

            // Thin Arrows
            (TokenKind::LAngle, '-') => Some(K::LThinArrow),
            (TokenKind::Minus, '>') => Some(K::RThinArrow),

            // Shifts
            (TokenKind::LAngle, '<') => Some(K::LAngleLAngle),
            (TokenKind::RAngle, '>') => Some(K::RAngleRAngle),

            (TokenKind::Amp, '&') => Some(K::AmpAmp),
            _ => None,
        }
    }
    pub fn token_from_bytes(bytes: &[u8]) -> Option<TokenKind> {
        match bytes {
            b"fn" => Some(K::KeywordFn),
            b"let" => Some(K::KeywordLet),
            b"mut" => Some(K::KeywordMut),
            b"and" => Some(K::KeywordAnd),
            b"or" => Some(K::KeywordOr),
            b"if" => Some(K::KeywordIf),
            b"else" => Some(K::KeywordElse),
            b"deftype" => Some(K::KeywordDefType),
            b"while" => Some(K::KeywordWhile),
            b"loop" => Some(K::KeywordLoop),
            b"ns" => Some(K::KeywordNamespace),
            b"intern" => Some(K::KeywordIntern),
            b"for" => Some(K::KeywordFor),
            b"in" => Some(K::KeywordIn),
            b"ability" => Some(K::KeywordAbility),
            b"impl" => Some(K::KeywordImpl),
            b"switch" => Some(K::KeywordSwitch),
            b"not" => Some(K::KeywordNot),
            b"is" => Some(K::KeywordIs),
            b"builtin" => Some(K::KeywordBuiltin),
            b"where" => Some(K::KeywordWhere),
            b"context" => Some(K::KeywordContext),
            b"use" => Some(K::KeywordUse),
            b"require" => Some(K::KeywordRequire),
            b"defer" => Some(K::KeywordDefer),
            b"==" => Some(K::EqualsEquals),
            b"!=" => Some(K::BangEquals),
            b"<=" => Some(K::LessEqual),
            b">=" => Some(K::GreaterEqual),
            b":=" => Some(K::ColonEquals),
            _ => None,
        }
    }
    pub fn is_keyword(&self) -> bool {
        match self {
            K::KeywordFn => true,
            K::KeywordLet => true,
            K::KeywordMut => true,
            K::KeywordAnd => true,
            K::KeywordOr => true,
            K::KeywordIf => true,
            K::KeywordWhile => true,
            K::KeywordLoop => true,
            K::KeywordNamespace => true,
            K::KeywordIntern => true,
            K::KeywordFor => true,
            K::KeywordIn => true,
            K::KeywordAbility => true,
            K::KeywordImpl => true,
            K::KeywordIs => true,
            K::KeywordSwitch => true,
            K::KeywordNot => true,
            K::KeywordBuiltin => true,
            K::KeywordWhere => true,
            K::KeywordContext => true,
            K::KeywordUse => true,
            K::KeywordRequire => true,
            K::KeywordDefer => true,
            _ => false,
        }
    }
    pub fn is_binary_operator(&self) -> bool {
        BinaryOpKind::from_tokenkind(*self).is_some()
    }
    pub fn is_prefix_operator(&self) -> bool {
        match self {
            K::KeywordNot => true,
            _ => false,
        }
    }
    pub fn is_postfix_operator(&self) -> bool {
        match self {
            K::Dot => true,
            K::OpenBracket => true,
            K::Bang => true,
            K::QuestionMark => true,
            _ => false,
        }
    }
    pub fn is_postfix_type_operator(&self) -> bool {
        match self {
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

    #[inline]
    pub fn end(&self) -> u32 {
        self.start + self.len
    }

    #[track_caller]
    #[inline]
    pub fn extend_to(&mut self, new_end: u32) {
        if cfg!(debug_assertions) {
            if new_end < self.end() {
                panic!("Attempt to extend span from {} to {}", self.end(), new_end)
            }
        }
        let new_len = new_end - self.start;
        self.len = new_len;
    }
}

pub struct EscapedChar {
    pub sentinel: char,
    pub output: u8,
}
pub const SHARED_STRING_ESCAPED_CHARS: [EscapedChar; 5] = [
    EscapedChar { sentinel: 'n', output: b'\n' },
    EscapedChar { sentinel: '0', output: b'\0' },
    EscapedChar { sentinel: 't', output: b'\t' },
    EscapedChar { sentinel: 'r', output: b'\r' },
    EscapedChar { sentinel: '\\', output: b'\\' },
];

pub const DOUBLE_QUOTE_STRING_ESCAPED_CHARS: [EscapedChar; 1] =
    [EscapedChar { sentinel: '"', output: b'\"' }];

pub const BACKTICK_STRING_ESCAPED_CHARS: [EscapedChar; 1] =
    [EscapedChar { sentinel: '`', output: b'`' }];

pub const CHAR_ESCAPED_CHARS: [EscapedChar; 6] = [
    EscapedChar { sentinel: 'n', output: b'\n' },
    EscapedChar { sentinel: '0', output: b'\0' },
    EscapedChar { sentinel: 't', output: b'\t' },
    EscapedChar { sentinel: 'r', output: b'\r' },
    EscapedChar { sentinel: '\'', output: b'\'' },
    EscapedChar { sentinel: '\\', output: b'\\' },
];

const TOKEN_FLAG_IS_WHITESPACE_PRECEDED: u64 = 0x01;
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
        let flags = if whitespace_preceeded { TOKEN_FLAG_IS_WHITESPACE_PRECEDED } else { 0 };

        Token { span: span_id, kind, flags }
    }
    pub fn is_whitespace_preceded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_WHITESPACE_PRECEDED == TOKEN_FLAG_IS_WHITESPACE_PRECEDED
    }
}

macro_rules! errf {
    ($self:expr, $pos:expr, $($format_args:expr),* $(,)?) => {
        {
            let s: String = format!($($format_args),*);
            Lexer::make_error($self, s, $pos, 1)
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexMode {
    /// The standard mode; we're lexing code
    Tokens,
    /// We're lexing code inside a string; we track brace depth to know which
    /// closing delimiter is the final one, this takes advantage of the fact
    /// (and requires!) that the language grammar has matched bracing
    Interp { brace_depth: u32 },
    /// A double-quote string. Escape patterns are different
    DoubleQuoteString { exprs: bool },
    /// A backtick string; only backticks must be escaped
    BacktickString { exprs: bool },
}

impl LexMode {
    pub fn is_dq_string(&self) -> bool {
        matches!(self, LexMode::DoubleQuoteString { .. })
    }
    pub fn is_bt_string(&self) -> bool {
        matches!(self, LexMode::BacktickString { .. })
    }
    pub fn string_delim_kind(&self) -> Option<StringDelimKind> {
        match self {
            LexMode::DoubleQuoteString { .. } => Some(StringDelimKind::DoubleQuote),
            LexMode::BacktickString { .. } => Some(StringDelimKind::Backtick),
            LexMode::Tokens => None,
            LexMode::Interp { .. } => None,
        }
    }
}

#[derive(Debug)]
struct LexState {
    mode_stack: Vec<LexMode>,
}
pub struct Lexer<'a, 'spans> {
    pub file_id: FileId,
    // Known valid utf8; see `make`
    content: &'a [u8],
    pub spans: &'spans mut Spans,
    pub pos: u32,
}

impl<'content, 'spans> Lexer<'content, 'spans> {
    pub fn make(
        input: &'content str,
        spans: &'spans mut Spans,
        file_id: FileId,
    ) -> Lexer<'content, 'spans> {
        Lexer { file_id, content: input.as_bytes(), spans, pos: 0 }
    }

    fn make_error(&mut self, message: String, start: u32, len: u32) -> LexError {
        let span = self.add_span(start, len);
        LexError { message, file_id: self.file_id, span }
    }

    fn add_span(&mut self, start: u32, len: u32) -> SpanId {
        self.spans.add(Span { start, len, file_id: self.file_id })
    }

    pub fn run(&mut self, tokens: &mut Vec<Token>) -> LexResult<()> {
        let mut state = LexState { mode_stack: vec![LexMode::Tokens] };
        while self.eat_token(tokens, &mut state)?.is_some() {
            {}
        }
        Ok(())
    }

    fn eat_token(
        &mut self,
        tokens: &mut Vec<Token>,
        state: &mut LexState,
    ) -> LexResult<Option<()>> {
        let mut tok_len = 0;
        let mut is_number = false;

        #[inline]
        fn make_token(lex: &mut Lexer, kind: TokenKind, start: u32, len: u32) -> Token {
            let span = lex.add_span(start, len);
            // nocommit
            let whitespace_preceded = lex
                .content
                .get((start as usize).saturating_sub(1))
                .is_some_and(|c| (*c as char).is_whitespace());
            Token::new(kind, span, whitespace_preceded)
        }

        #[inline]
        fn make_buffered_token(lex: &mut Lexer, kind: TokenKind, n: u32, tok_len: u32) -> Token {
            make_token(lex, kind, n - tok_len, tok_len)
        }

        let make_keyword_or_ident = |lex: &mut Lexer, n: u32, tok_len: u32| {
            let start = n - tok_len;
            let len = tok_len;
            let tok_bytes: &[u8] = &self.content[start as usize..(start + len) as usize];
            if let Some(kind) = TokenKind::token_from_bytes(tok_bytes) {
                make_token(lex, kind, start, len)
            } else {
                make_token(lex, TokenKind::Ident, start, len)
            }
        };
        loop {
            let (c, n) = self.peek_with_pos();
            debug!("LEX char='{}' n={} tok_len={} state={:?}", c, n, tok_len, state);
            let lex_mode = state.mode_stack.last_mut().unwrap();
            match lex_mode {
                LexMode::DoubleQuoteString { exprs: interp_exprs }
                | LexMode::BacktickString { exprs: interp_exprs } => {
                    let interp_exprs = *interp_exprs;
                    match c {
                        EOF_CHAR => {
                            return Err(self.make_error(
                                "Encountered EOF inside string".to_string(),
                                n - tok_len,
                                tok_len + 1,
                            ));
                        }
                        '\\' => {
                            let next = self.peek_n(1);
                            #[allow(clippy::if_same_then_else)]
                            if SHARED_STRING_ESCAPED_CHARS.iter().any(|c| c.sentinel == next) {
                                tok_len += 2;
                                self.advance();
                                self.advance();
                            } else if lex_mode.is_dq_string() && next == '"' {
                                tok_len += 2;
                                self.advance();
                                self.advance();
                            } else if lex_mode.is_bt_string() && next == '`' {
                                tok_len += 2;
                                self.advance();
                                self.advance();
                            } else {
                                tok_len += 1;
                                self.advance();
                            }
                        }
                        // { is only special when interp_exprs is true
                        '{' if interp_exprs => {
                            let next = self.peek_n(1);
                            if next == '{' {
                                self.advance();
                                self.advance();
                                tok_len += 2;
                            } else {
                                self.advance();
                                // Track brace depth and done when == 0
                                debug!("[lex] starting code at {n} with tok_len = {tok_len}");
                                let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                                state.mode_stack.push(LexMode::Interp { brace_depth: 1 });
                                tokens.push(make_buffered_token(
                                    self,
                                    K::StringUnterminated {
                                        delim: string_delim_kind,
                                        interp_exprs,
                                    },
                                    n,
                                    tok_len,
                                ));
                                tokens.push(make_token(self, K::OpenBrace, n, 1));
                                return Ok(Some(()));
                            }
                        }
                        '"' if lex_mode.is_dq_string() => {
                            // Terminates a double-quoted string
                            tok_len += 1;
                            self.advance();
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.mode_stack.pop();
                            tokens.push(make_buffered_token(
                                self,
                                K::String { delim: string_delim_kind, interp_exprs },
                                n + 1,
                                tok_len,
                            ));
                            return Ok(Some(()));
                        }
                        '`' if lex_mode.is_bt_string() => {
                            // Terminates a backtick string
                            tok_len += 1;
                            self.advance();
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.mode_stack.pop();
                            tokens.push(make_buffered_token(
                                self,
                                K::String { delim: string_delim_kind, interp_exprs },
                                n + 1,
                                tok_len,
                            ));
                            return Ok(Some(()));
                        }
                        '\n' if !lex_mode.is_bt_string() => {
                            let string_start_quote = n - tok_len - 1;
                            return Err(self.make_error(
                                    "Encountered newline inside string; Try a backtick string (`) instead".to_string(),
                                    string_start_quote,
                                    tok_len + 1,
                                ));
                        }
                        _ => {
                            tok_len += 1;
                            self.advance();
                        }
                    };
                    continue;
                }
                LexMode::Tokens | LexMode::Interp { .. } => {
                    let next = self.peek_n(1);
                    if tok_len == 0 {
                        macro_rules! return_single {
                            ($kind: expr) => {{
                                self.advance();
                                tokens.push(make_token(self, $kind, n, 1));
                                return Ok(Some(()));
                            }};
                        }
                        macro_rules! return_double {
                            ($kind: expr) => {{
                                self.advance();
                                self.advance();
                                tokens.push(make_token(self, $kind, n, 2));
                                return Ok(Some(()));
                            }};
                        }
                        match c {
                            EOF_CHAR => return Ok(None),
                            'p' if next == '"' => {
                                state.mode_stack.push(LexMode::DoubleQuoteString { exprs: false });
                                tok_len += 2;
                                self.advance();
                                self.advance();
                                continue;
                            }
                            '"' => {
                                state.mode_stack.push(LexMode::DoubleQuoteString { exprs: true });
                                tok_len += 1;
                                self.advance();
                                continue;
                            }
                            'p' if next == '`' => {
                                state.mode_stack.push(LexMode::BacktickString { exprs: false });
                                tok_len += 2;
                                self.advance();
                                self.advance();
                                continue;
                            }
                            '`' => {
                                state.mode_stack.push(LexMode::BacktickString { exprs: true });
                                tok_len += 1;
                                self.advance();
                                continue;
                            }
                            '(' => return_single!(K::OpenParen),
                            ')' => return_single!(K::CloseParen),
                            '[' => return_single!(K::OpenBracket),
                            ']' => return_single!(K::CloseBracket),
                            '{' => {
                                if let LexMode::Interp { brace_depth } = lex_mode {
                                    *brace_depth += 1;
                                }
                                return_single!(K::OpenBrace)
                            }
                            '}' => {
                                if let LexMode::Interp { brace_depth } = lex_mode {
                                    *brace_depth -= 1;
                                    if *brace_depth == 0 {
                                        debug!("[lex] *pop* code end");
                                        state.mode_stack.pop();

                                        // Should always be in a string state after finishing an
                                        // expression interpolation
                                        debug_assert!(
                                            state
                                                .mode_stack
                                                .last()
                                                .unwrap()
                                                .string_delim_kind()
                                                .is_some()
                                        );
                                    }
                                }
                                return_single!(K::CloseBrace)
                            }
                            '<' => {
                                if next == '=' {
                                    return_double!(K::LessEqual)
                                } else if next == '-' {
                                    return_double!(K::LThinArrow)
                                } else if next == '<' {
                                    return_double!(K::LAngleLAngle)
                                } else {
                                    return_single!(K::LAngle)
                                }
                            }
                            '>' => {
                                if next == '=' {
                                    return_double!(K::GreaterEqual)
                                } else if next == '>' {
                                    return_double!(K::RAngleRAngle)
                                } else {
                                    return_single!(K::RAngle)
                                }
                            }
                            ':' => {
                                if next == '=' {
                                    return_double!(K::ColonEquals)
                                } else {
                                    return_single!(K::Colon)
                                }
                            }
                            ';' => return_single!(K::Semicolon),
                            '=' => {
                                if next == '=' {
                                    return_double!(K::EqualsEquals)
                                } else {
                                    return_single!(K::Equals)
                                }
                            }
                            '.' => return_single!(K::Dot),
                            ',' => return_single!(K::Comma),
                            '\'' => {
                                // char literal
                                // Eat opening '
                                self.advance();
                                let c = self.next();
                                if c == '\\' {
                                    // Eat Escaped char
                                    self.advance();

                                    // Eat Closing '
                                    let q = self.next();
                                    if q != '\'' {
                                        return Err(errf!(
                                            self,
                                            n + 3,
                                            "Expected closing ' for char literal at {q}"
                                        ));
                                    }
                                    tokens.push(make_token(self, TokenKind::Char, n, 4));
                                    return Ok(Some(()));
                                } else {
                                    // Eat Closing ''
                                    let q = self.next();
                                    if q != '\'' {
                                        return Err(errf!(
                                            self,
                                            n + 2,
                                            "Expected closing ' for char literal at {q}"
                                        ));
                                    }
                                    // `n` is the index of the opening quote
                                    tokens.push(make_token(self, TokenKind::Char, n, 3));
                                    return Ok(Some(()));
                                }
                            }
                            '+' => return_single!(K::Plus),
                            '-' => {
                                if next == '>' {
                                    return_double!(K::RThinArrow)
                                } else {
                                    return_single!(K::Minus)
                                }
                            }
                            '*' => return_single!(K::Asterisk),
                            '/' => {
                                if next == '/' {
                                    // Immediately handle this to either the next line or the EOF,
                                    // so that the main loop doesn't have to check for this state
                                    self.advance();
                                    self.advance();
                                    let mut comment_c;
                                    loop {
                                        comment_c = self.next();
                                        if comment_c == '\n'
                                            || comment_c == EOF_CHAR
                                            || comment_c == '\r' && self.peek() == '\n'
                                        {
                                            let comment_tok =
                                                make_token(self, K::LineComment, n, self.pos - n);
                                            tokens.push(comment_tok);
                                            return Ok(Some(()));
                                        }
                                    }
                                } else {
                                    return_single!(K::Slash)
                                }
                            }
                            '!' => {
                                if next == '=' {
                                    return_double!(K::BangEquals)
                                } else {
                                    return_single!(K::Bang)
                                }
                            }
                            '?' => return_single!(K::QuestionMark),
                            '|' => {
                                if next == '|' {
                                    return_double!(K::PipePipe)
                                } else {
                                    return_single!(K::Pipe)
                                }
                            }
                            '&' => {
                                if next == '&' {
                                    return_double!(K::AmpAmp)
                                } else {
                                    return_single!(K::Amp)
                                }
                            }
                            '%' => return_single!(K::Percent),
                            '\\' => return_single!(K::BackSlash),
                            '#' => return_single!(K::Hash),
                            '@' => return_single!(K::At),
                            '^' => return_single!(K::Caret),
                            ' ' | '\x09'..='\x0d' => {
                                // simply eat whitespace
                                self.advance();
                            }
                            _ if is_ident_or_num_start(c) => {
                                // Enter number submode of ident mode
                                if c == '_' && next == '_' {
                                    return Err(errf!(
                                        self,
                                        n,
                                        "the __ prefix is reserved for internals"
                                    ));
                                }
                                if c == '-' || c.is_numeric() {
                                    is_number = true;
                                }
                                tok_len += 1;
                                self.advance();
                            }
                            _ => {
                                return Err(self.make_error(
                                    format!("Unexpected character {c}"),
                                    n,
                                    1,
                                ));
                            }
                        };
                    } else {
                        // tok_len != 0
                        match c {
                            EOF_CHAR => {
                                tokens.push(make_keyword_or_ident(self, n, tok_len));
                                return Ok(Some(()));
                            }
                            '.' => {
                                // Dot is a token, but not inside a number, where:
                                // If followed by a digit, its just part of the Ident stream
                                // Otherwise, its a 'Dot' token.
                                // Example:
                                // 100.42 -> Ident(100.42)
                                // 100.toInt() -> Ident(100), Dot, Ident(toInt)
                                if is_number {
                                    if next.is_numeric() {
                                        tok_len += 1;
                                        self.advance();
                                    } else {
                                        // Conclude the number ident; we'll eat the dot next token w/ an empty buffer
                                        tokens.push(make_keyword_or_ident(self, n, tok_len));
                                        return Ok(Some(()));
                                    }
                                } else {
                                    // Flush the ident buffer
                                    tokens.push(make_keyword_or_ident(self, n, tok_len));
                                    // Lex the dot
                                    tokens.push(make_token(self, K::Dot, n, 1));
                                    self.advance();
                                    return Ok(Some(()));
                                }
                            }
                            ' ' | '\x09'..='\x0d' => {
                                // Flush the ident
                                tokens.push(make_keyword_or_ident(self, n, tok_len));
                                // Eat the whitespace too
                                self.advance();
                                return Ok(Some(()));
                            }
                            _ => {
                                if is_ident_char(c) {
                                    tok_len += 1;
                                    self.advance();
                                } else {
                                    tokens.push(make_keyword_or_ident(self, n, tok_len));
                                    return Ok(Some(()));
                                }
                            }
                        }
                    };
                }
            }
        }
    }

    fn next(&mut self) -> char {
        let c = self.peek();
        self.pos += 1;
        c
    }

    #[inline]
    fn peek(&self) -> char {
        self.peek_n(0)
    }

    #[inline]
    fn peek_n(&self, n: usize) -> char {
        self.content.get(self.pos as usize + n).copied().unwrap_or(0) as char
    }

    fn peek_with_pos(&self) -> (char, u32) {
        (self.peek(), self.pos)
    }

    #[inline]
    fn advance(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    fn advance_n(&mut self, n: u32) {
        self.pos += n;
    }
}

pub fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

fn is_ident_or_num_start(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexer, Span, SpanId, Spans, Token, TokenKind as K};

    fn set_up(input: &str) -> anyhow::Result<(Spans, Vec<Token>)> {
        let mut spans = Spans::new();
        let mut token_vec = vec![];
        Lexer::make(input, &mut spans, 0).run(&mut token_vec)?;
        Ok((spans, token_vec))
    }

    fn expect_token_kinds(input: &str, expected: Vec<K>) -> anyhow::Result<()> {
        let mut spans = Spans::new();
        let mut token_vec = vec![];
        Lexer::make(input, &mut spans, 0).run(&mut token_vec)?;
        let kinds: Vec<K> = token_vec.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, expected);
        Ok(())
    }

    fn assert_token(
        spans: &Spans,
        tokens: &[Token],
        index: usize,
        kind: K,
        start: u32,
        len: u32,
        is_whitespace_preceded: bool,
    ) {
        let span = spans.get(SpanId::from_u32(index as u32 + 2).unwrap());
        assert_eq!(tokens[index].kind, kind);
        assert_eq!(span.start, start);
        assert_eq!(span.len, len);
        assert_eq!(tokens[index].is_whitespace_preceded(), is_whitespace_preceded);
    }

    #[test]
    fn case1() -> anyhow::Result<()> {
        let input = "let x = println(4)";
        expect_token_kinds(
            input,
            vec![
                K::KeywordLet,
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
        let kinds: Vec<K> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        let span0 = spans.get(tokens[0].span);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);
        let span1 = spans.get(tokens[1].span);
        assert_eq!(span1.start, 1);
        assert_eq!(span1.len, 2);
        assert_eq!(span1.end(), 3);
        assert!(!tokens[1].is_whitespace_preceded());
        Ok(())
    }

    #[test]
    fn minus_int() -> anyhow::Result<()> {
        let input = "- 43";
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<K> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Ident]);
        let span0 = spans.get(tokens[0].span);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);

        let span1 = spans.get(tokens[1].span);
        assert_eq!(span1.start, 2);
        assert_eq!(span1.len, 2);
        assert_eq!(span1.end(), 4);
        assert!(tokens[1].is_whitespace_preceded());
        Ok(())
    }

    #[test]
    fn ending_ident() -> anyhow::Result<()> {
        let input = "let x = a + b";
        expect_token_kinds(
            input,
            vec![K::KeywordLet, K::Ident, K::Equals, K::Ident, K::Plus, K::Ident],
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
        let foo: int = 74;
        // <test harness> expected output
        //
        "#;
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<K> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(spans.get(tokens[0].span), Span { start: 0, len: 16, file_id: 0 });
        assert_eq!(&input[0..5], "// He");
        assert_eq!(
            vec![
                K::LineComment,
                K::KeywordLet,
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

    #[test]
    fn extern_fn_name() -> anyhow::Result<()> {
        let input = r#"extern(printf)"#;
        expect_token_kinds(input, vec![K::Ident, K::OpenParen, K::Ident, K::CloseParen])
    }

    #[test]
    fn dot_access() -> anyhow::Result<()> {
        let input = r#"self.buffer"#;
        expect_token_kinds(input, vec![K::Ident, K::Dot, K::Ident])
    }

    #[test]
    fn simple_block() -> anyhow::Result<()> {
        let input = r#"{
            self.buffer
        }"#;
        expect_token_kinds(input, vec![K::OpenBrace, K::Ident, K::Dot, K::Ident, K::CloseBrace])
    }

    #[test]
    fn literal_string_simple() -> anyhow::Result<()> {
        let (spans, tokens) = set_up("\"foobear\"")?;
        assert_token(&spans, &tokens, 0, K::STRING_DQ_INTERP, 0, 9, false);
        Ok(())
    }

    #[test]
    fn literal_string_in_call() -> anyhow::Result<()> {
        let (spans, tokens) = set_up("let x = println(p\"foobear\")")?;
        assert_token(&spans, &tokens, 0, K::KeywordLet, 0, 3, false);
        assert_token(&spans, &tokens, 1, K::Ident, 4, 1, true);
        assert_token(&spans, &tokens, 2, K::Equals, 6, 1, true);
        assert_token(&spans, &tokens, 3, K::Ident, 8, 7, true);
        assert_token(&spans, &tokens, 4, K::OpenParen, 15, 1, false);
        assert_token(&spans, &tokens, 5, K::STRING_DQ_PLAIN, 16, 10, false);
        assert_token(&spans, &tokens, 6, K::CloseParen, 26, 1, false);
        Ok(())
    }

    #[test]
    fn interpolation_1() -> anyhow::Result<()> {
        let input = r#""Hello, {world}""#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
            ],
        )
    }

    #[test]
    fn interpolation_start_end() -> anyhow::Result<()> {
        let input = r#" "{foo()}, {world}" "#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::Ident,
                K::OpenParen,
                K::CloseParen,
                K::CloseBrace,
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
            ],
        )
    }

    #[test]
    fn interpolation_string() -> anyhow::Result<()> {
        let input = r#""{p"hello"}""#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::STRING_DQ_PLAIN,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
            ],
        )
    }

    #[test]
    fn interpolation_nested() -> anyhow::Result<()> {
        let input = r#" "{"hello {var}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
            ],
        )
    }

    #[test]
    fn interpolation_escape_doublebrace() -> anyhow::Result<()> {
        let input = "\"Method 'sum' does not exist on type: '{{ x: iword, y: iword }'\"";
        expect_token_kinds(input, vec![K::STRING_DQ_INTERP])
    }

    #[test]
    fn interpolation_nested_with_braces() -> anyhow::Result<()> {
        let input = r#" "{"hello {({ x: 42 }).x}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::OpenParen,
                K::OpenBrace,
                K::Ident,
                K::Colon,
                K::Ident,
                K::CloseBrace,
                K::CloseParen,
                K::Dot,
                K::Ident, // .x
                K::CloseBrace,
                K::STRING_DQ_INTERP,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
            ],
        )
    }

    #[test]
    fn backtick_string_1() -> anyhow::Result<()> {
        let input = "`Method 'sum' does not exist on type: '{{ x: iword, y: iword }'`";
        expect_token_kinds(input, vec![K::STRING_BT_INTERP])
    }

    #[test]
    fn keyword_substring_is_correct() -> anyhow::Result<()> {
        let input = "mutt";
        expect_token_kinds(input, vec![K::Ident])?;
        let input2 = "mut";
        expect_token_kinds(input2, vec![K::KeywordMut])?;
        Ok(())
    }

    #[test]
    fn backtick_interpolation_mixed() -> anyhow::Result<()> {
        // Tests mode stack by mixing string types
        let input = r#"`
        {"hello {var}"}
        `"#;
        expect_token_kinds(
            input,
            vec![
                K::STRING_UNTERM_BT_INTERP,
                K::OpenBrace,
                K::STRING_UNTERM_DQ_INTERP,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::STRING_DQ_INTERP,
                K::CloseBrace,
                K::STRING_BT_INTERP,
            ],
        )
    }
}
