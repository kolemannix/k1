// Copyright (c) 2026 knix
// All rights reserved.

use std::fmt;
use std::fmt::{Display, Formatter};

use crate::debug;
use crate::nz_u32_id;
use crate::parse::BinaryOpKind;
use crate::parse::FileId;
use crate::vpool::VPool;
use crate::{static_assert_niched, static_assert_size};
use TokenKind as K;

pub const EOF_CHAR: char = 27 as char; // esc
// pub const EOF_CHAR: char = '\0' as char;
// EOF acts like a line end: whitespace- and newline-preceded
pub const EOF_TOKEN: Token =
    Token { kind: TokenKind::Eof, span: SpanId::NONE, flags: 0x01 | 0x04 };

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub file_id: FileId,
    pub span: SpanId,
}

pub type LexResult<A> = anyhow::Result<A, LexError>;

nz_u32_id!(SpanId);
impl SpanId {
    pub fn is_none(self) -> bool {
        self == Self::NONE
    }
    pub const NONE: SpanId = Self::ONE;
}

pub struct Spans {
    pub span_pool: VPool<Span, SpanId>,
}

impl Spans {
    pub fn new() -> Spans {
        let mut span_pool = VPool::make("spans");
        span_pool.add(Span::NONE);
        Spans { span_pool }
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
        // peek_n clamps every index onto the final token instead of
        // bounds-checking, so the stream must end with an EOF sentinel
        // (Lexer::run guarantees this).
        assert!(
            data.last().is_some_and(|t| t.kind == TokenKind::Eof),
            "TokenIter requires an EOF-terminated token stream"
        );
        TokenIter { cursor: 0, tokens: data }
    }

    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Token {
        let tok = self.peek_n(0);
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
    pub fn peek_n(&self, n: i64) -> Token {
        // Branchless: any out-of-range index, including a negative `pos`,
        // which wraps to a huge usize, clamps onto the trailing EOF
        // sentinel. Compiles to cmp/cmov/load with no EOF fallback branch.
        let pos = self.cursor.wrapping_add(n as usize);
        let idx = pos.min(self.tokens.len() - 1);
        // SAFETY: `make` asserts `tokens` is non-empty, so `len - 1` cannot
        // wrap and `idx` is always in bounds.
        unsafe { *self.tokens.get_unchecked(idx) }
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
    Numeric,
    // The 8 string variants must stay contiguous so is_string() lowers to a range check.
    /// A completed string
    StringDoneDqInterp,
    StringDoneDqNoInterp,
    StringDoneBtInterp,
    StringDoneBtNoInterp,
    /// Used in string interpolation; any not-fully-completed string:
    /// the initial segment, or a connecting segment between 2 interpolations
    StringOpenDqInterp,
    StringOpenDqNoInterp,
    StringOpenBtInterp,
    StringOpenBtNoInterp,

    Char,

    KeywordFn,
    KeywordLet,
    KeywordMut,
    KeywordAnd,
    KeywordOr,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordLoop,
    KeywordNamespace,
    KeywordIntern,
    KeywordFor,
    KeywordIn,
    KeywordAbility,
    KeywordImpl,
    KeywordIs,
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
    Dollar,
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

static_assert_size!(TokenKind, 1);
static_assert_niched!(TokenKind);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringTokenInfo {
    pub delim: StringDelimKind,
    pub interp_exprs: bool,
    /// false = unterminated segment (ends at a `{` interpolation hole)
    pub done: bool,
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
    pub const fn string(delim: StringDelimKind, interp_exprs: bool, done: bool) -> TokenKind {
        use StringDelimKind as D;
        match (done, delim, interp_exprs) {
            (true, D::DoubleQuote, true) => K::StringDoneDqInterp,
            (true, D::DoubleQuote, false) => K::StringDoneDqNoInterp,
            (true, D::Backtick, true) => K::StringDoneBtInterp,
            (true, D::Backtick, false) => K::StringDoneBtNoInterp,
            (false, D::DoubleQuote, true) => K::StringOpenDqInterp,
            (false, D::DoubleQuote, false) => K::StringOpenDqNoInterp,
            (false, D::Backtick, true) => K::StringOpenBtInterp,
            (false, D::Backtick, false) => K::StringOpenBtNoInterp,
        }
    }

    pub const fn as_string(self) -> Option<StringTokenInfo> {
        use StringDelimKind as D;
        let (done, delim, interp_exprs) = match self {
            K::StringDoneDqInterp => (true, D::DoubleQuote, true),
            K::StringDoneDqNoInterp => (true, D::DoubleQuote, false),
            K::StringDoneBtInterp => (true, D::Backtick, true),
            K::StringDoneBtNoInterp => (true, D::Backtick, false),
            K::StringOpenDqInterp => (false, D::DoubleQuote, true),
            K::StringOpenDqNoInterp => (false, D::DoubleQuote, false),
            K::StringOpenBtInterp => (false, D::Backtick, true),
            K::StringOpenBtNoInterp => (false, D::Backtick, false),
            _ => return None,
        };
        Some(StringTokenInfo { delim, interp_exprs, done })
    }

    pub const fn is_string(self) -> bool {
        matches!(
            self,
            K::StringDoneDqInterp
                | K::StringDoneDqNoInterp
                | K::StringDoneBtInterp
                | K::StringDoneBtNoInterp
                | K::StringOpenDqInterp
                | K::StringOpenDqNoInterp
                | K::StringOpenBtInterp
                | K::StringOpenBtNoInterp
        )
    }

    pub fn get_repr(&self) -> &'static str {
        match self {
            K::KeywordFn => "fn",
            K::KeywordLet => "let",
            K::KeywordMut => "mut",
            K::KeywordAnd => "and",
            K::KeywordOr => "or",
            K::KeywordIf => "if",
            K::KeywordElse => "else",
            K::KeywordWhile => "while",
            K::KeywordLoop => "loop",
            K::KeywordNamespace => "ns",
            K::KeywordIntern => "intern",
            K::KeywordFor => "for",
            K::KeywordIn => "in",
            K::KeywordAbility => "ability",
            K::KeywordImpl => "impl",
            K::KeywordIs => "is",
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
            K::Dollar => "$",
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
            K::Numeric => "<numeric>",
            K::StringDoneBtInterp => "<f`string`>",
            K::StringDoneBtNoInterp => "<`string`>",
            K::StringDoneDqInterp => "<f\"string\">",
            K::StringDoneDqNoInterp => "<\"string\">",
            K::StringOpenBtInterp => "<f`string...>",
            K::StringOpenBtNoInterp => "<`string...>",
            K::StringOpenDqInterp => "<f\"string...>",
            K::StringOpenDqNoInterp => "<\"string...>",
            K::Char => "<char>",

            K::Eof => "<EOF>",
        }
    }

    pub fn token_from_bytes(bytes: &[u8]) -> Option<TokenKind> {
        // TODO: Fewer lexed keywords; more context-aware idents-as-keywords
        // This prevents 'name-squatting' on things like 'type', 'in', 'for'
        match bytes {
            b"fn" => Some(K::KeywordFn),
            b"let" => Some(K::KeywordLet),
            b"mut" => Some(K::KeywordMut),
            b"and" => Some(K::KeywordAnd),
            b"or" => Some(K::KeywordOr),
            b"if" => Some(K::KeywordIf),
            b"else" => Some(K::KeywordElse),
            b"while" => Some(K::KeywordWhile),
            b"loop" => Some(K::KeywordLoop),
            b"ns" => Some(K::KeywordNamespace),
            b"intern" => Some(K::KeywordIntern),
            b"for" => Some(K::KeywordFor),
            b"in" => Some(K::KeywordIn),
            b"ability" => Some(K::KeywordAbility),
            b"impl" => Some(K::KeywordImpl),
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
}

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
pub const SHARED_STRING_ESCAPED_CHARS: [EscapedChar; 6] = [
    EscapedChar { sentinel: 'n', output: b'\n' },
    EscapedChar { sentinel: '0', output: b'\0' },
    EscapedChar { sentinel: 't', output: b'\t' },
    EscapedChar { sentinel: 'r', output: b'\r' },
    EscapedChar { sentinel: '\\', output: b'\\' },
    EscapedChar { sentinel: '{', output: b'{' },
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

const TOKEN_FLAG_IS_WHITESPACE_PRECEDED: u8 = 0x01;
#[allow(unused)]
const TOKEN_FLAG_IS_WHITESPACE_FOLLOWED: u8 = 0x02;
const TOKEN_FLAG_IS_NEWLINE_PRECEDED: u8 = 0x04;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SpanId,
    pub flags: u8,
}
static_assert_size!(Token, 8);

impl Token {
    pub fn new(
        kind: TokenKind,
        span_id: SpanId,
        whitespace_preceeded: bool,
        newline_preceded: bool,
    ) -> Token {
        let mut flags = 0;
        if whitespace_preceeded {
            flags |= TOKEN_FLAG_IS_WHITESPACE_PRECEDED
        };
        if newline_preceded {
            flags |= TOKEN_FLAG_IS_NEWLINE_PRECEDED
        };
        Token { kind, span: span_id, flags }
    }
    pub fn is_whitespace_preceded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_WHITESPACE_PRECEDED == TOKEN_FLAG_IS_WHITESPACE_PRECEDED
    }
    /// True when a line break (or start of file) separates this token from
    /// the previous one; line comments count since they run to end of line
    pub fn is_newline_preceded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_NEWLINE_PRECEDED == TOKEN_FLAG_IS_NEWLINE_PRECEDED
    }

    pub fn is_kind_nonspaced(&self, kind: TokenKind) -> bool {
        self.kind == kind && !self.is_whitespace_preceded()
    }

    pub fn is_kind_spaced(&self, kind: TokenKind) -> bool {
        self.kind == kind && self.is_whitespace_preceded()
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
pub enum TokenTriviaKind {
    Whitespace,
    LineComment,
}

#[derive(Debug, Clone, Copy)]
pub struct TokenTrivia {
    pub span: SpanId,
    pub kind: TokenTriviaKind,
}

#[derive(Debug, Clone, Copy)]
pub struct TriviaEntry {
    /// Index into the file's tokens vec of the token this trivia precedes;
    /// trailing trivia attaches to the final EOF sentinel token
    /// (index tokens.len() - 1)
    pub token_idx: u32,
    pub trivia: TokenTrivia,
}

/// Sparse token -> trivia attachment; entries are sorted by token_idx
/// by construction since the lexer emits them in source order
#[derive(Debug, Default, Clone)]
pub struct TokenTriviaTable {
    entries: Vec<TriviaEntry>,
}

impl TokenTriviaTable {
    pub fn push(&mut self, entry: TriviaEntry) {
        debug_assert!(self.entries.last().is_none_or(|last| last.token_idx <= entry.token_idx));
        self.entries.push(entry)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &TriviaEntry> {
        self.entries.iter()
    }

    /// All trivia attached to (immediately preceding) the token at `token_idx`
    pub fn for_token(&self, token_idx: u32) -> &[TriviaEntry] {
        let start = self.entries.partition_point(|e| e.token_idx < token_idx);
        let end = self.entries.partition_point(|e| e.token_idx <= token_idx);
        &self.entries[start..end]
    }
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
    pub trivia: TokenTriviaTable,
}

impl<'content, 'spans> Lexer<'content, 'spans> {
    pub fn make(
        input: &'content str,
        spans: &'spans mut Spans,
        file_id: FileId,
    ) -> Lexer<'content, 'spans> {
        Lexer { file_id, content: input.as_bytes(), spans, pos: 0, trivia: TokenTriviaTable::default() }
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
        let result = loop {
            match self.eat_token(tokens, &mut state) {
                Ok(Some(())) => {}
                Ok(None) => break Ok(()),
                Err(e) => break Err(e),
            }
        };
        // Terminate with an EOF sentinel even on error; TokenIter's branchless
        // peeks clamp onto it instead of bounds-checking every access.
        tokens.push(EOF_TOKEN);
        result
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
            let whitespace_preceded = lex
                .content
                .get((start as usize).saturating_sub(1))
                .is_some_and(|c| (*c as char).is_whitespace());
            // Walk back over the whitespace run; comment text stops the walk,
            // but a line comment's own terminating newline sits after it, so
            // any token after a comment is still seen as newline-preceded
            let mut newline_preceded = false;
            let mut i = start as usize;
            loop {
                if i == 0 {
                    newline_preceded = true;
                    break;
                }
                let c = lex.content[i - 1];
                if c == b'\n' || c == b'\r' {
                    newline_preceded = true;
                    break;
                }
                if !(c as char).is_whitespace() {
                    break;
                }
                i -= 1;
            }
            Token::new(kind, span, whitespace_preceded, newline_preceded)
        }

        #[inline]
        fn make_buffered_token(lex: &mut Lexer, kind: TokenKind, end: u32, tok_len: u32) -> Token {
            make_token(lex, kind, end - tok_len, tok_len)
        }

        #[inline]
        fn make_keyword_or_ident(
            lex: &mut Lexer,
            end: u32,
            tok_len: u32,
            is_number: bool,
        ) -> Token {
            let start = end - tok_len;
            let len = tok_len;
            if is_number {
                make_token(lex, K::Numeric, start, len)
            } else if let Some(kind) = TokenKind::token_from_bytes(&lex.content[start as usize..(start + len) as usize]) {
                make_token(lex, kind, start, len)
            } else {
                make_token(lex, K::Ident, start, len)
            }
        }
        macro_rules! make_from_buffer {
            ($end: expr) => {
                make_keyword_or_ident(self, $end, tok_len, is_number)
            };
        }
        loop {
            let (c, n) = self.peek_with_pos();
            if cfg!(feature = "dbg") {
                debug!("LEX char='{}' n={} tok_len={} state={:?}", c, n, tok_len, state);
            }
            let lex_mode = state.mode_stack.last_mut().unwrap();
            match lex_mode {
                LexMode::DoubleQuoteString { exprs: interp_exprs }
                | LexMode::BacktickString { exprs: interp_exprs } => {
                    let interp_exprs = *interp_exprs;
                    match c {
                        EOF_CHAR => {
                            // FIXME: We need to encode EOF_CHAR as something other than \0
                            return Err(self.make_error(
                                "Encountered EOF, or null byte, inside string".to_string(),
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
                        // { is only special when interp_exprs is true; \{ escapes it
                        '{' if interp_exprs => {
                            self.advance();
                            // Track brace depth and done when == 0
                            debug!("[lex] starting code at {n} with tok_len = {tok_len}");
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.mode_stack.push(LexMode::Interp { brace_depth: 1 });
                            tokens.push(make_buffered_token(
                                self,
                                K::string(string_delim_kind, interp_exprs, false),
                                n,
                                tok_len,
                            ));
                            tokens.push(make_token(
                                self,
                                K::OpenBrace,
                                n,
                                1,
                            ));
                            return Ok(Some(()));
                        }
                        '"' if lex_mode.is_dq_string() => {
                            // Terminates a double-quoted string
                            tok_len += 1;
                            self.advance();
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.mode_stack.pop();
                            tokens.push(make_buffered_token(
                                self,
                                K::string(string_delim_kind, interp_exprs, true),
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
                                K::string(string_delim_kind, interp_exprs, true),
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
                                tokens.push(make_token(
                                    self,
                                    $kind,
                                    n,
                                    1,
                                ));
                                return Ok(Some(()));
                            }};
                        }
                        macro_rules! return_double {
                            ($kind: expr) => {{
                                self.advance();
                                self.advance();
                                tokens.push(make_token(
                                    self,
                                    $kind,
                                    n,
                                    2,
                                ));
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
                                    tokens.push(make_token(
                                        self,
                                        TokenKind::Char,
                                        n,
                                        4,
                                    ));
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
                                    tokens.push(make_token(
                                        self,
                                        TokenKind::Char,
                                        n,
                                        3,
                                    ));
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
                                    let rest = &self.content[self.pos as usize..];
                                    self.pos = match memchr::memchr(b'\n', rest) {
                                        // Stop on the '\r' of a "\r\n", otherwise consume the '\n'
                                        Some(i) if i > 0 && rest[i - 1] == b'\r' => {
                                            self.pos + i as u32
                                        }
                                        Some(i) => self.pos + i as u32 + 1,
                                        // Matches the old char loop, which consumed one EOF too
                                        None => self.content.len() as u32 + 1,
                                    };
                                    let span = self.add_span(n, self.pos - n);
                                    self.trivia.push(TriviaEntry {
                                        token_idx: tokens.len() as u32,
                                        trivia: TokenTrivia {
                                            span,
                                            kind: TokenTriviaKind::LineComment,
                                        },
                                    });
                                    return Ok(Some(()));
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
                            '$' => return_single!(K::Dollar),
                            '^' => return_single!(K::Caret),
                            ' ' | '\x09'..='\x0d' => {
                                // simply eat whitespace; consume the whole run
                                // here rather than re-dispatching per char
                                self.advance();
                                while matches!(self.peek(), ' ' | '\x09'..='\x0d') {
                                    self.advance();
                                }
                            }
                            _ if is_ident_or_num_start(c) => {
                                // Enter number submode of ident mode
                                if c == '-' || is_numeric_char(c) {
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
                                tokens.push(make_from_buffer!(n));
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
                                    if is_numeric_char(next) {
                                        tok_len += 1;
                                        self.advance();
                                    } else {
                                        // Conclude the number ident; we'll eat the dot next token w/ an empty buffer
                                        tokens.push(make_from_buffer!(n));
                                        return Ok(Some(()));
                                    }
                                } else {
                                    // Flush the ident buffer
                                    tokens.push(make_from_buffer!(n));
                                    // Lex the dot
                                    tokens.push(make_token(
                                        self,
                                        K::Dot,
                                        n,
                                        1,
                                    ));
                                    self.advance();
                                    return Ok(Some(()));
                                }
                            }
                            ' ' | '\x09'..='\x0d' => {
                                // Flush the ident
                                tokens.push(make_from_buffer!(n));
                                // Eat the whitespace too
                                self.advance();
                                return Ok(Some(()));
                            }
                            _ => {
                                if is_ident_char(c) {
                                    tok_len += 1;
                                    self.advance();
                                    // Consume the rest of the ident run here
                                    // rather than re-dispatching per char
                                    while is_ident_char(self.peek()) {
                                        tok_len += 1;
                                        self.advance();
                                    }
                                } else {
                                    tokens.push(make_from_buffer!(n));
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
        match self.content.get(self.pos as usize + n) {
            None => EOF_CHAR,
            Some(c) => *c as char,
        }
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

const CLASS_IDENT: u8 = 1;
const CLASS_NUMERIC: u8 = 2;
/// Byte-indexed classification for the chars the lexer actually sees
/// Notably marks hyphen - as an ident char
#[rustfmt::skip]
static BYTE_CLASS: [u8; 256] = [
//  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00 control
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 control
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, // 0x20 sp ! " # $ % & ' ( ) * + , - . /
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, // 0x30 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x40 @ A B C D E F G H I J K L M N O
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 0x50 P Q R S T U V W X Y Z [ \ ] ^ _
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x60 ` a b c d e f g h i j k l m n o
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, // 0x70 p q r s t u v w x y z { | } ~ del
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x80 Latin-1 control
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x90 Latin-1 control
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, // 0xa0 punctuation, except ª (0xaa) is alphabetic
    0, 0, 3, 3, 0, 1, 0, 0, 0, 3, 1, 0, 3, 3, 3, 0, // 0xb0 punctuation, except ² ³ µ ¹ º ¼ ½ ¾
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0xc0 À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï
    1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, // 0xd0 Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß (× is not)
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0xe0 à á â ã ä å æ ç è é ê ë ì í î ï
    1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, // 0xf0 ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ (÷ is not)
];

#[inline]
pub fn is_ident_char(c: char) -> bool {
    match BYTE_CLASS.get(c as usize) {
        Some(class) => class & CLASS_IDENT != 0,
        None => c.is_alphanumeric(),
    }
}

#[inline]
fn is_ident_or_num_start(c: char) -> bool {
    is_ident_char(c)
}

#[inline]
fn is_numeric_char(c: char) -> bool {
    match BYTE_CLASS.get(c as usize) {
        Some(class) => class & CLASS_NUMERIC != 0,
        None => c.is_numeric(),
    }
}

pub fn lex_standalone(content: &str) -> (Spans, Vec<Token>, Option<LexError>) {
    let mut spans = Spans::new();
    let mut token_vec = vec![];
    match Lexer::make(content, &mut spans, 1).run(&mut token_vec) {
        Err(e) => (spans, token_vec, Some(e)),
        Ok(()) => (spans, token_vec, None),
    }
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexer, Span, SpanId, Spans, Token, TokenKind as K, TokenTriviaKind};

    #[test]
    fn byte_class_matches_char_methods() {
        for b in 0..=255u8 {
            let c = b as char;
            let class = super::BYTE_CLASS[b as usize];
            let ident = c.is_alphanumeric() || c == '_' || c == '-';
            let numeric = c.is_numeric();
            assert_eq!(class & super::CLASS_IDENT != 0, ident, "ident class for byte {b:#x}");
            assert_eq!(class & super::CLASS_NUMERIC != 0, numeric, "numeric class for byte {b:#x}");
        }
    }

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
        let mut kinds: Vec<K> = token_vec.iter().map(|t| t.kind).collect();
        assert_eq!(kinds.pop(), Some(K::Eof));
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
                K::Numeric,
                K::CloseParen,
            ],
        )
    }

    #[test]
    fn signed_int() -> anyhow::Result<()> {
        let input = "-43";
        let (spans, tokens) = set_up(input)?;
        let kinds: Vec<K> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
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
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
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
        // I am here
        let foo: int = 74;
        // <test harness> expected output
        //
        "#;
        let mut spans = Spans::new();
        let mut tokens = vec![];
        let mut lexer = Lexer::make(input, &mut spans, 0);
        lexer.run(&mut tokens)?;
        let trivia = lexer.trivia;

        let kinds: Vec<K> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            vec![
                K::KeywordLet,
                K::Ident,
                K::Colon,
                K::Ident,
                K::Equals,
                K::Numeric,
                K::Semicolon,
                K::Eof,
            ],
            kinds
        );
        assert_eq!(spans.get(tokens[0].span), Span { start: 45, len: 3, file_id: 0 });

        let let_trivia = trivia.for_token(0);
        assert_eq!(let_trivia.len(), 2);
        assert_eq!(let_trivia[0].trivia.kind, TokenTriviaKind::LineComment);
        assert_eq!(spans.get(let_trivia[0].trivia.span), Span { start: 0, len: 16, file_id: 0 });
        assert_eq!(let_trivia[1].trivia.kind, TokenTriviaKind::LineComment);
        assert_eq!(spans.get(let_trivia[1].trivia.span), Span { start: 24, len: 13, file_id: 0 });

        assert!(trivia.for_token(3).is_empty());

        // Trailing comments attach to the EOF sentinel, the last token
        let trailing = trivia.for_token(tokens.len() as u32 - 1);
        assert_eq!(trailing.len(), 2);
        assert_eq!(spans.get(trailing[0].trivia.span), Span { start: 72, len: 34, file_id: 0 });
        assert_eq!(spans.get(trailing[1].trivia.span), Span { start: 114, len: 3, file_id: 0 });
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
        assert_token(&spans, &tokens, 0, K::StringDoneDqInterp, 0, 9, false);
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
        assert_token(&spans, &tokens, 5, K::StringDoneDqNoInterp, 16, 10, false);
        assert_token(&spans, &tokens, 6, K::CloseParen, 26, 1, false);
        Ok(())
    }

    #[test]
    fn interpolation_1() -> anyhow::Result<()> {
        let input = r#""Hello, {world}""#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDqInterp,
            ],
        )
    }

    #[test]
    fn interpolation_start_end() -> anyhow::Result<()> {
        let input = r#" "{foo()}, {world}" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::Ident,
                K::OpenParen,
                K::CloseParen,
                K::CloseBrace,
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDqInterp,
            ],
        )
    }

    #[test]
    fn interpolation_string() -> anyhow::Result<()> {
        let input = r#""{p"hello"}""#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::StringDoneDqNoInterp,
                K::CloseBrace,
                K::StringDoneDqInterp,
            ],
        )
    }

    #[test]
    fn interpolation_nested() -> anyhow::Result<()> {
        let input = r#" "{"hello {var}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDqInterp,
                K::CloseBrace,
                K::StringDoneDqInterp,
            ],
        )
    }

    #[test]
    fn interpolation_escape_brace() -> anyhow::Result<()> {
        let input = r#""Method 'sum' does not exist on type: '\{ x: iword, y: iword }'""#;
        expect_token_kinds(input, vec![K::StringDoneDqInterp])
    }

    #[test]
    fn interpolation_nested_with_braces() -> anyhow::Result<()> {
        let input = r#" "{"hello {({ x: 42 }).x}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::OpenParen,
                K::OpenBrace,
                K::Ident,
                K::Colon,
                K::Numeric,
                K::CloseBrace,
                K::CloseParen,
                K::Dot,
                K::Ident, // .x
                K::CloseBrace,
                K::StringDoneDqInterp,
                K::CloseBrace,
                K::StringDoneDqInterp,
            ],
        )
    }

    #[test]
    fn backtick_string_1() -> anyhow::Result<()> {
        let input = r#"`Method 'sum' does not exist on type: '\{ x: iword, y: iword }'`"#;
        expect_token_kinds(input, vec![K::StringDoneBtInterp])
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
                K::StringOpenBtInterp,
                K::OpenBrace,
                K::StringOpenDqInterp,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDqInterp,
                K::CloseBrace,
                K::StringDoneBtInterp,
            ],
        )
    }
}
