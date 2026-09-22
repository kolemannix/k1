// Copyright (c) 2026 knix
// All rights reserved.

use std::fmt;
use std::fmt::{Display, Formatter};

use crate::debug;
use crate::nz_u32_id;
use crate::parse::BinaryOpKind;
use crate::vpool::VPool;
use crate::{static_assert_niched, static_assert_size};
use TokenKind as K;

pub const EOF_CHAR: char = 27 as char; // esc

// EOF acts like a line end: whitespace- and newline-preceded
pub const EOF_TOKEN: Token = Token { kind: TokenKind::Eof, flags: 0x01 | 0x04, len: 0, start: 0 };

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub start: u32,
    pub len: u32,
}

pub type LexResult<A> = anyhow::Result<A, LexError>;

/// A token too long for the positional Token; carried under TOKEN_FLAG_SPAN_ID
/// until `materialize_lexed_file` gives it a SpanId
#[derive(Debug, Clone, Copy)]
pub struct LongToken {
    pub start: u32,
    pub len: u32,
}

#[derive(Default)]
pub struct Lexed {
    pub tokens: Vec<Token>,
    pub kinds: Vec<TokenKind>,
    pub trivia: TokenTriviaTable,
    /// In token order, one per TOKEN_FLAG_SPAN_ID token
    pub long_tokens: Vec<LongToken>,
    pub error: Option<LexError>,
}

impl Lexed {
    fn clear(&mut self) {
        self.tokens.clear();
        self.kinds.clear();
        self.trivia.entries.clear();
        self.long_tokens.clear();
        self.error = None;
    }

    #[inline]
    fn push(&mut self, token: Token) {
        self.tokens.push(token);
        self.kinds.push(token.kind);
    }
}

/// No pool, file id, or span in hand, so it runs on any thread; `out` is
/// the buffer to fill and comes back in the result
pub fn lex(content: &str, mut out: Lexed) -> Lexed {
    out.clear();
    let mut lexer = Lexer::make(content);
    out.error = lexer.run(&mut out).err();
    out
}

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
        write!(f, "LexError at {}: {}", self.start, self.message)
    }
}
impl std::error::Error for LexError {}

pub const TOKEN_LOOKAHEAD: usize = 2;

pub struct TokenIter<'toks> {
    cursor: usize,
    end: usize,
    tokens: &'toks [Token],
    kinds: &'toks [TokenKind],
}

impl<'toks> TokenIter<'toks> {
    pub fn make(lexed: &'toks Lexed) -> TokenIter<'toks> {
        let tokens = &lexed.tokens;
        let kinds = &lexed.kinds;
        assert_eq!(tokens.len(), kinds.len());
        let end = tokens.len() - (TOKEN_LOOKAHEAD + 1);
        assert!(
            kinds[end..].iter().all(|k| *k == TokenKind::Eof),
            "TokenIter requires an EOF-padded token stream"
        );
        TokenIter { cursor: 0, end, tokens, kinds }
    }

    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Token {
        let tok = self.peek_n(0);
        self.advance();
        tok
    }

    pub fn cursor_position(&self) -> usize {
        self.cursor
    }

    #[inline]
    pub fn advance_n(&mut self, n: usize) {
        self.cursor = (self.cursor + n).min(self.end);
    }

    #[inline]
    pub fn advance(&mut self) {
        self.advance_n(1);
    }

    #[inline]
    pub fn peek_n(&self, n: usize) -> Token {
        debug_assert!(n <= TOKEN_LOOKAHEAD);
        unsafe { *self.tokens.get_unchecked(self.cursor + n) }
    }

    #[inline]
    pub fn peek(&self) -> Token {
        self.peek_n(0)
    }

    #[inline]
    pub fn peek_kind_n(&self, n: usize) -> TokenKind {
        debug_assert!(n <= TOKEN_LOOKAHEAD);
        unsafe { *self.kinds.get_unchecked(self.cursor + n) }
    }

    #[inline]
    pub fn peek_kind(&self) -> TokenKind {
        self.peek_kind_n(0)
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
        let idx = self.cursor.wrapping_sub(1).min(self.end);
        unsafe { *self.tokens.get_unchecked(idx) }
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
    // The 4 string variants must stay contiguous so is_string() lowers to a range check.
    /// A completed string
    StringDoneDq,
    StringDoneBt,
    /// Used in string interpolation; any not-fully-completed string:
    /// the initial segment, or a connecting segment between 2 interpolations
    StringOpenDq,
    StringOpenBt,

    Char,

    KeywordFn,
    KeywordLet,
    KeywordAnd,
    KeywordOr,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordLoop,
    KeywordNs,
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
    RThinArrow,

    /// Not really a token but allows us to avoid Option<Token> everywhere
    Eof,
}

static_assert_size!(TokenKind, 1);
static_assert_niched!(TokenKind);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringTokenInfo {
    pub delim: StringDelimKind,
    /// false = unterminated segment (ends at a `$` interpolation hole)
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

const fn make_keyword_key(bytes: &[u8]) -> u64 {
    let mut key = 0u64;
    let mut i = 0;
    while i < bytes.len() {
        key |= (bytes[i] as u64) << (8 * i);
        i += 1;
    }
    key
}

const KW_FN: u64 = make_keyword_key(b"fn");
const KW_LET: u64 = make_keyword_key(b"let");
const KW_AND: u64 = make_keyword_key(b"and");
const KW_OR: u64 = make_keyword_key(b"or");
const KW_IF: u64 = make_keyword_key(b"if");
const KW_ELSE: u64 = make_keyword_key(b"else");
const KW_WHILE: u64 = make_keyword_key(b"while");
const KW_LOOP: u64 = make_keyword_key(b"loop");
const KW_NS: u64 = make_keyword_key(b"ns");
const KW_INTERN: u64 = make_keyword_key(b"intern");
const KW_FOR: u64 = make_keyword_key(b"for");
const KW_IN: u64 = make_keyword_key(b"in");
const KW_ABILITY: u64 = make_keyword_key(b"ability");
const KW_IMPL: u64 = make_keyword_key(b"impl");
const KW_NOT: u64 = make_keyword_key(b"not");
const KW_IS: u64 = make_keyword_key(b"is");
const KW_BUILTIN: u64 = make_keyword_key(b"builtin");
const KW_WHERE: u64 = make_keyword_key(b"where");
const KW_CONTEXT: u64 = make_keyword_key(b"context");
const KW_USE: u64 = make_keyword_key(b"use");
const KW_REQUIRE: u64 = make_keyword_key(b"require");
const KW_DEFER: u64 = make_keyword_key(b"defer");

impl TokenKind {
    pub const fn string(delim: StringDelimKind, done: bool) -> TokenKind {
        use StringDelimKind as D;
        match (done, delim) {
            (true, D::DoubleQuote) => K::StringDoneDq,
            (true, D::Backtick) => K::StringDoneBt,
            (false, D::DoubleQuote) => K::StringOpenDq,
            (false, D::Backtick) => K::StringOpenBt,
        }
    }

    pub const fn as_string(self) -> Option<StringTokenInfo> {
        use StringDelimKind as D;
        let (done, delim) = match self {
            K::StringDoneDq => (true, D::DoubleQuote),
            K::StringDoneBt => (true, D::Backtick),
            K::StringOpenDq => (false, D::DoubleQuote),
            K::StringOpenBt => (false, D::Backtick),
            _ => return None,
        };
        Some(StringTokenInfo { delim, done })
    }

    pub const fn is_string(self) -> bool {
        matches!(self, K::StringDoneDq | K::StringDoneBt | K::StringOpenDq | K::StringOpenBt)
    }

    pub fn get_repr(&self) -> &'static str {
        match self {
            K::KeywordFn => "fn",
            K::KeywordLet => "let",
            K::KeywordAnd => "and",
            K::KeywordOr => "or",
            K::KeywordIf => "if",
            K::KeywordElse => "else",
            K::KeywordWhile => "while",
            K::KeywordLoop => "loop",
            K::KeywordNs => "ns",
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
            K::RThinArrow => "->",

            K::DoubleQuote => "\"",
            K::SingleQuote => "singleq",

            K::Ident => "<ident>",
            K::Numeric => "<numeric>",
            K::StringDoneBt => "<`string`>",
            K::StringDoneDq => "<\"string\">",
            K::StringOpenBt => "<`string...>",
            K::StringOpenDq => "<\"string...>",
            K::Char => "<char>",

            K::Eof => "<EOF>",
        }
    }

    pub fn from_keyword_key(key: u64) -> Option<TokenKind> {
        match key {
            KW_FN => Some(K::KeywordFn),
            KW_LET => Some(K::KeywordLet),
            KW_AND => Some(K::KeywordAnd),
            KW_OR => Some(K::KeywordOr),
            KW_IF => Some(K::KeywordIf),
            KW_ELSE => Some(K::KeywordElse),
            KW_WHILE => Some(K::KeywordWhile),
            KW_LOOP => Some(K::KeywordLoop),
            KW_NS => Some(K::KeywordNs),
            KW_INTERN => Some(K::KeywordIntern),
            KW_FOR => Some(K::KeywordFor),
            KW_IN => Some(K::KeywordIn),
            KW_ABILITY => Some(K::KeywordAbility),
            KW_IMPL => Some(K::KeywordImpl),
            KW_NOT => Some(K::KeywordNot),
            KW_IS => Some(K::KeywordIs),
            KW_BUILTIN => Some(K::KeywordBuiltin),
            KW_WHERE => Some(K::KeywordWhere),
            KW_CONTEXT => Some(K::KeywordContext),
            KW_USE => Some(K::KeywordUse),
            KW_REQUIRE => Some(K::KeywordRequire),
            KW_DEFER => Some(K::KeywordDefer),
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
    EscapedChar { sentinel: '$', output: b'$' },
];

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
pub const TOKEN_FLAG_SPAN_ID: u8 = 0x08;

fn nzu32(v: u32) -> std::num::NonZeroU32 {
    std::num::NonZeroU32::new(v).unwrap()
}

/// A token longer than u16::MAX (giant string literals) gets an interned SpanId instead
/// TOKEN_FLAG_SPAN_ID is set and `start` holds the SpanId.
#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub flags: u8,
    pub len: u16,
    pub start: u32,
}
static_assert_size!(Token, 8);

impl Token {
    pub fn new(
        kind: TokenKind,
        start: u32,
        len: u32,
        flags: u8,
        long_tokens: &mut Vec<LongToken>,
    ) -> Token {
        match u16::try_from(len) {
            Ok(len16) => Token { kind, flags, len: len16, start },
            Err(_) => {
                long_tokens.push(LongToken { start, len });
                Token { kind, flags: flags | TOKEN_FLAG_SPAN_ID, len: 0, start: 0 }
            }
        }
    }

    pub fn from_span_id(kind: TokenKind, span_id: SpanId) -> Token {
        Token {
            kind,
            flags: TOKEN_FLAG_SPAN_ID,
            len: 0,
            start: Into::<std::num::NonZeroU32>::into(span_id).get(),
        }
    }

    /// Convert to a span-id-backed token; error constructors call this so a
    /// stored error can recover its span with no pool or file in hand
    pub fn materialize(self, file_id: u32, spans: &mut Spans) -> Token {
        if self.flags & TOKEN_FLAG_SPAN_ID != 0 {
            self
        } else {
            let span_id = spans.add(Span { file_id, start: self.start, len: self.len as u32 });
            Token {
                kind: self.kind,
                flags: self.flags | TOKEN_FLAG_SPAN_ID,
                len: 0,
                start: Into::<std::num::NonZeroU32>::into(span_id).get(),
            }
        }
    }

    /// The pooled span of a materialized token; NONE (with a debug panic) if
    /// this token never went through `materialize`
    pub fn materialized_span_id(&self) -> SpanId {
        if self.flags & TOKEN_FLAG_SPAN_ID != 0 {
            SpanId::from(nzu32(self.start))
        } else {
            debug_assert!(false, "materialized_span_id on a positional token");
            SpanId::NONE
        }
    }

    pub fn span(&self, file_id: u32, spans: &Spans) -> Span {
        if self.flags & TOKEN_FLAG_SPAN_ID != 0 {
            spans.get(SpanId::from(nzu32(self.start)))
        } else {
            Span { file_id, start: self.start, len: self.len as u32 }
        }
    }

    pub fn span_id(&self, file_id: u32, spans: &mut Spans) -> SpanId {
        if self.flags & TOKEN_FLAG_SPAN_ID != 0 {
            SpanId::from(nzu32(self.start))
        } else {
            spans.add(Span { file_id, start: self.start, len: self.len as u32 })
        }
    }
    pub fn is_whitespace_preceded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_WHITESPACE_PRECEDED == TOKEN_FLAG_IS_WHITESPACE_PRECEDED
    }
    /// True when a line break (or start of file) separates this token from
    /// the previous one; line comments count since they run to end of line
    pub fn is_newline_preceded(&self) -> bool {
        self.flags & TOKEN_FLAG_IS_NEWLINE_PRECEDED == TOKEN_FLAG_IS_NEWLINE_PRECEDED
    }

    pub fn can_start_expression(&self) -> bool {
        self.kind == K::Minus
    }

    pub fn is_newline_starter(&self) -> bool {
        self.is_newline_preceded() && self.can_start_expression()
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
    pub start: u32,
    pub len: u32,
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
    pub fn entries(&self) -> &[TriviaEntry] {
        &self.entries
    }

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
    DoubleQuoteString,
    /// A backtick string; only backticks must be escaped
    BacktickString,
}

impl LexMode {
    pub fn is_dq_string(&self) -> bool {
        matches!(self, LexMode::DoubleQuoteString)
    }
    pub fn is_bt_string(&self) -> bool {
        matches!(self, LexMode::BacktickString)
    }
    pub fn string_delim_kind(&self) -> Option<StringDelimKind> {
        match self {
            LexMode::DoubleQuoteString => Some(StringDelimKind::DoubleQuote),
            LexMode::BacktickString => Some(StringDelimKind::Backtick),
            LexMode::Tokens => None,
            LexMode::Interp { .. } => None,
        }
    }
}

#[derive(Debug)]
struct LexState {
    mode: LexMode,
    mode_stack: Vec<LexMode>,
}

impl LexState {
    fn push_mode(&mut self, mode: LexMode) {
        self.mode_stack.push(self.mode);
        self.mode = mode;
    }

    fn pop_mode(&mut self) {
        self.mode = self.mode_stack.pop().unwrap();
    }
}
struct Lexer<'a> {
    // Known valid utf8; see `make`
    content: &'a [u8],
    pos: u32,
    next_token_flags: u8,
}

impl<'content> Lexer<'content> {
    fn make(input: &'content str) -> Lexer<'content> {
        Lexer {
            content: input.as_bytes(),
            pos: 0,
            next_token_flags: TOKEN_FLAG_IS_NEWLINE_PRECEDED,
        }
    }

    fn make_error(&mut self, message: String, start: u32, len: u32) -> LexError {
        LexError { message, start, len }
    }

    fn run(&mut self, out: &mut Lexed) -> LexResult<()> {
        let mut state = LexState { mode: LexMode::Tokens, mode_stack: Vec::new() };
        let estimate = self.content.len() / 3 + 1;
        out.tokens.reserve(estimate);
        out.kinds.reserve(estimate);
        let result = loop {
            match self.eat_token(out, &mut state) {
                Ok(Some(())) => {}
                Ok(None) => break Ok(()),
                Err(e) => break Err(e),
            }
        };
        for _ in 0..=TOKEN_LOOKAHEAD {
            out.push(EOF_TOKEN);
        }
        result
    }

    fn eat_token(&mut self, out: &mut Lexed, state: &mut LexState) -> LexResult<Option<()>> {
        let mut tok_len = 0;

        #[inline]
        fn push_token(lex: &mut Lexer, out: &mut Lexed, kind: TokenKind, start: u32, len: u32) {
            let flags = lex.next_token_flags;
            lex.next_token_flags = 0;
            let token = Token::new(kind, start, len, flags, &mut out.long_tokens);
            out.push(token)
        }

        #[inline]
        fn push_buffered_token(
            lex: &mut Lexer,
            out: &mut Lexed,
            kind: TokenKind,
            end: u32,
            tok_len: u32,
        ) {
            push_token(lex, out, kind, end - tok_len, tok_len)
        }

        #[inline]
        fn push_keyword_or_ident(
            lex: &mut Lexer,
            out: &mut Lexed,
            start: u32,
            len: u32,
            is_number: bool,
        ) {
            let kind = if is_number {
                K::Numeric
            } else if len > 7 {
                K::Ident
            } else {
                let start = start as usize;
                let key = match lex.content.get(start..start + 8) {
                    Some(w) => {
                        u64::from_le_bytes(w.try_into().unwrap()) & (u64::MAX >> (64 - 8 * len))
                    }
                    None => make_keyword_key(&lex.content[start..start + len as usize]),
                };
                TokenKind::from_keyword_key(key).unwrap_or(K::Ident)
            };
            push_token(lex, out, kind, start, len)
        }
        loop {
            let (c, n) = self.peek_with_pos();
            if cfg!(feature = "dbg") {
                debug!("LEX char='{}' n={} tok_len={} state={:?}", c, n, tok_len, state);
            }
            let lex_mode = &mut state.mode;
            match lex_mode {
                LexMode::DoubleQuoteString | LexMode::BacktickString => {
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
                        // $ opens an interpolation hole: ${expr} or $ident; \$ escapes it.
                        // A $ followed by anything else is a literal dollar.
                        '$' if self.peek_n(1) == '{' => {
                            debug!("[lex] starting code at {n} with tok_len = {tok_len}");
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            // Track brace depth and done when == 0
                            state.push_mode(LexMode::Interp { brace_depth: 1 });
                            push_buffered_token(
                                self,
                                out,
                                K::string(string_delim_kind, false),
                                n,
                                tok_len,
                            );
                            self.advance();
                            self.advance();
                            push_token(self, out, K::OpenBrace, n, 2);
                            return Ok(Some(()));
                        }
                        '$' if is_ident_start(self.peek_n(1)) => {
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            push_buffered_token(
                                self,
                                out,
                                K::string(string_delim_kind, false),
                                n,
                                tok_len,
                            );
                            // Eat the $, then the ident run; we stay in string mode.
                            // A hyphen continues the ident only when followed by another
                            // ident char, so "$n-th" is `n-th` but "$a-$b" is `a`, "-", `b`
                            self.advance();
                            let mut ident_len = 0;
                            while is_ident_char(self.peek()) {
                                if self.peek() == '-' && !is_ident_char(self.peek_n(1)) {
                                    break;
                                }
                                ident_len += 1;
                                self.advance();
                            }
                            push_token(self, out, K::Ident, n + 1, ident_len);
                            return Ok(Some(()));
                        }
                        '"' if lex_mode.is_dq_string() => {
                            // Terminates a double-quoted string
                            tok_len += 1;
                            self.advance();
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.pop_mode();
                            push_buffered_token(
                                self,
                                out,
                                K::string(string_delim_kind, true),
                                n + 1,
                                tok_len,
                            );
                            return Ok(Some(()));
                        }
                        '`' if lex_mode.is_bt_string() => {
                            // Terminates a backtick string
                            tok_len += 1;
                            self.advance();
                            let string_delim_kind = lex_mode.string_delim_kind().unwrap();
                            state.pop_mode();
                            push_buffered_token(
                                self,
                                out,
                                K::string(string_delim_kind, true),
                                n + 1,
                                tok_len,
                            );
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
                    macro_rules! return_single {
                        ($kind: expr) => {{
                            self.advance();
                            push_token(self, out, $kind, n, 1);
                            return Ok(Some(()));
                        }};
                    }
                    macro_rules! return_double {
                        ($kind: expr) => {{
                            self.advance();
                            self.advance();
                            push_token(self, out, $kind, n, 2);
                            return Ok(Some(()));
                        }};
                    }
                    match c {
                        EOF_CHAR => return Ok(None),
                        '"' => {
                            state.push_mode(LexMode::DoubleQuoteString);
                            tok_len += 1;
                            self.advance();
                            continue;
                        }
                        '`' => {
                            state.push_mode(LexMode::BacktickString);
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
                                    state.pop_mode();
                                    debug_assert!(state.mode.string_delim_kind().is_some());
                                }
                            }
                            return_single!(K::CloseBrace)
                        }
                        '<' => {
                            if self.peek_n(1) == '=' {
                                return_double!(K::LessEqual)
                            } else if self.peek_n(1) == '<' {
                                return_double!(K::LAngleLAngle)
                            } else {
                                return_single!(K::LAngle)
                            }
                        }
                        '>' => {
                            if self.peek_n(1) == '=' {
                                return_double!(K::GreaterEqual)
                            } else if self.peek_n(1) == '>' {
                                return_double!(K::RAngleRAngle)
                            } else {
                                return_single!(K::RAngle)
                            }
                        }
                        ':' => return_single!(K::Colon),
                        ';' => return_single!(K::Semicolon),
                        '=' => {
                            if self.peek_n(1) == '=' {
                                return_double!(K::EqualsEquals)
                            } else {
                                return_single!(K::Equals)
                            }
                        }
                        '.' => return_single!(K::Dot),
                        ',' => return_single!(K::Comma),
                        '\'' => {
                            self.advance();
                            let c = self.next();
                            if c == '\\' {
                                self.advance();

                                let q = self.next();
                                if q != '\'' {
                                    return Err(errf!(
                                        self,
                                        n + 3,
                                        "Expected closing ' for char literal at {q}"
                                    ));
                                }
                                push_token(self, out, TokenKind::Char, n, 4);
                                return Ok(Some(()));
                            } else {
                                let q = self.next();
                                if q != '\'' {
                                    return Err(errf!(
                                        self,
                                        n + 2,
                                        "Expected closing ' for char literal at {q}"
                                    ));
                                }
                                push_token(self, out, TokenKind::Char, n, 3);
                                return Ok(Some(()));
                            }
                        }
                        '+' => return_single!(K::Plus),
                        '-' => {
                            if self.peek_n(1) == '>' {
                                return_double!(K::RThinArrow)
                            } else {
                                return_single!(K::Minus)
                            }
                        }
                        '*' => return_single!(K::Asterisk),
                        '/' => {
                            if self.peek_n(1) == '/' {
                                self.advance();
                                self.advance();
                                let rest = &self.content[self.pos as usize..];
                                self.pos = match memchr::memchr(b'\n', rest) {
                                    Some(i) if i > 0 && rest[i - 1] == b'\r' => self.pos + i as u32,
                                    Some(i) => {
                                        self.next_token_flags |= TOKEN_FLAG_IS_WHITESPACE_PRECEDED
                                            | TOKEN_FLAG_IS_NEWLINE_PRECEDED;
                                        self.pos + i as u32 + 1
                                    }
                                    None => self.content.len() as u32 + 1,
                                };
                                out.trivia.push(TriviaEntry {
                                    token_idx: out.tokens.len() as u32,
                                    trivia: TokenTrivia {
                                        start: n,
                                        len: self.pos - n,
                                        kind: TokenTriviaKind::LineComment,
                                    },
                                });
                                return Ok(Some(()));
                            } else {
                                return_single!(K::Slash)
                            }
                        }
                        '!' => {
                            if self.peek_n(1) == '=' {
                                return_double!(K::BangEquals)
                            } else {
                                return_single!(K::Bang)
                            }
                        }
                        '?' => return_single!(K::QuestionMark),
                        '|' => {
                            if self.peek_n(1) == '|' {
                                return_double!(K::PipePipe)
                            } else {
                                return_single!(K::Pipe)
                            }
                        }
                        '&' => {
                            if self.peek_n(1) == '&' {
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
                            self.eat_whitespace_run();
                        }
                        _ if is_ident_char(c) => {
                            let is_number = is_numeric_char(c);
                            self.advance();
                            loop {
                                self.eat_ident_run();
                                if is_number
                                    && self.peek() == '.'
                                    && is_numeric_char(self.peek_n(1))
                                {
                                    self.advance();
                                    continue;
                                }
                                break;
                            }
                            push_keyword_or_ident(self, out, n, self.pos - n, is_number);
                            return Ok(Some(()));
                        }
                        _ => {
                            return Err(self.make_error(format!("Unexpected character {c}"), n, 1));
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
    fn eat_whitespace_run(&mut self) {
        let mut flags = TOKEN_FLAG_IS_WHITESPACE_PRECEDED;
        let mut pos = self.pos as usize;
        while let Some(&b) = self.content.get(pos) {
            let class = BYTE_CLASS[b as usize];
            if class & CLASS_SPACE == 0 {
                break;
            }
            if class & CLASS_NEWLINE != 0 {
                flags |= TOKEN_FLAG_IS_NEWLINE_PRECEDED;
            }
            pos += 1;
        }
        self.pos = pos as u32;
        self.next_token_flags |= flags;
    }

    #[inline]
    fn eat_ident_run(&mut self) {
        let mut pos = self.pos as usize;
        while let Some(&b) = self.content.get(pos) {
            if BYTE_CLASS[b as usize] & CLASS_IDENT == 0 {
                break;
            }
            pos += 1;
        }
        self.pos = pos as u32;
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
const CLASS_SPACE: u8 = 4;
const CLASS_NEWLINE: u8 = 8;
/// Byte-indexed classification for the chars the lexer actually sees
/// Notably marks hyphen - as an ident char
#[rustfmt::skip]
static BYTE_CLASS: [u8; 256] = [
//  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 12,4, 4, 12,0, 0, // 0x00 control; \t \n \v \f \r
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 control
    4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, // 0x20 sp ! " # $ % & ' ( ) * + , - . /
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

/// Whether c can start a bare `$ident` interpolation hole: an ident char that
/// could not begin a number, so `"$5.99"` stays literal text
#[inline]
fn is_ident_start(c: char) -> bool {
    is_ident_char(c) && c != '-' && !is_numeric_char(c)
}

#[inline]
fn is_numeric_char(c: char) -> bool {
    match BYTE_CLASS.get(c as usize) {
        Some(class) => class & CLASS_NUMERIC != 0,
        None => c.is_numeric(),
    }
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexed, Spans, TOKEN_LOOKAHEAD, Token, TokenKind as K, TokenTriviaKind, lex};

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

    fn lex_ok(input: &str) -> anyhow::Result<Lexed> {
        let mut lexed = lex(input, Lexed::default());
        if let Some(e) = lexed.error {
            anyhow::bail!("{}", e.message);
        }
        lexed.tokens.truncate(lexed.tokens.len() - TOKEN_LOOKAHEAD);
        Ok(lexed)
    }

    fn set_up(input: &str) -> anyhow::Result<(Spans, Vec<Token>)> {
        Ok((Spans::new(), lex_ok(input)?.tokens))
    }

    fn expect_token_kinds(input: &str, expected: Vec<K>) -> anyhow::Result<()> {
        let (_, token_vec) = set_up(input)?;
        let mut kinds: Vec<K> = Vec::with_capacity(token_vec.len());
        for t in &token_vec {
            kinds.push(t.kind);
        }
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
        let span = tokens[index].span(0, spans);
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
        let mut kinds: Vec<K> = Vec::with_capacity(tokens.len());
        for t in &tokens {
            kinds.push(t.kind);
        }
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
        let span0 = tokens[0].span(0, &spans);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);
        let span1 = tokens[1].span(0, &spans);
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
        let mut kinds: Vec<K> = Vec::with_capacity(tokens.len());
        for t in &tokens {
            kinds.push(t.kind);
        }
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
        let span0 = tokens[0].span(0, &spans);
        assert_eq!(span0.start, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);

        let span1 = tokens[1].span(0, &spans);
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
        let lexed = lex_ok(input)?;
        let (tokens, trivia) = (lexed.tokens, lexed.trivia);

        let mut kinds: Vec<K> = Vec::with_capacity(tokens.len());
        for t in &tokens {
            kinds.push(t.kind);
        }
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
        let let_span = tokens[0].span(0, &Spans::new());
        assert_eq!((let_span.start, let_span.len), (45, 3));

        let let_trivia = trivia.for_token(0);
        assert_eq!(let_trivia.len(), 2);
        assert_eq!(let_trivia[0].trivia.kind, TokenTriviaKind::LineComment);
        assert_eq!((let_trivia[0].trivia.start, let_trivia[0].trivia.len), (0, 16));
        assert_eq!(let_trivia[1].trivia.kind, TokenTriviaKind::LineComment);
        assert_eq!((let_trivia[1].trivia.start, let_trivia[1].trivia.len), (24, 13));

        assert!(trivia.for_token(3).is_empty());

        // Trailing comments attach to the EOF sentinel, the last token
        let trailing = trivia.for_token(tokens.len() as u32 - 1);
        assert_eq!(trailing.len(), 2);
        assert_eq!((trailing[0].trivia.start, trailing[0].trivia.len), (72, 34));
        assert_eq!((trailing[1].trivia.start, trailing[1].trivia.len), (114, 3));
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
        assert_token(&spans, &tokens, 0, K::StringDoneDq, 0, 9, false);
        Ok(())
    }

    #[test]
    fn literal_string_in_call() -> anyhow::Result<()> {
        let (spans, tokens) = set_up("let x = println(\"foobear\")")?;
        assert_token(&spans, &tokens, 0, K::KeywordLet, 0, 3, false);
        assert_token(&spans, &tokens, 1, K::Ident, 4, 1, true);
        assert_token(&spans, &tokens, 2, K::Equals, 6, 1, true);
        assert_token(&spans, &tokens, 3, K::Ident, 8, 7, true);
        assert_token(&spans, &tokens, 4, K::OpenParen, 15, 1, false);
        assert_token(&spans, &tokens, 5, K::StringDoneDq, 16, 9, false);
        assert_token(&spans, &tokens, 6, K::CloseParen, 25, 1, false);
        Ok(())
    }

    #[test]
    fn interpolation_1() -> anyhow::Result<()> {
        let input = r#""Hello, ${world}""#;
        expect_token_kinds(
            input,
            vec![K::StringOpenDq, K::OpenBrace, K::Ident, K::CloseBrace, K::StringDoneDq],
        )
    }

    #[test]
    fn interpolation_bare_ident() -> anyhow::Result<()> {
        let (spans, tokens) = set_up(r#""Hello, $wide-world!""#)?;
        assert_token(&spans, &tokens, 0, K::StringOpenDq, 0, 8, false);
        assert_token(&spans, &tokens, 1, K::Ident, 9, 10, false);
        assert_token(&spans, &tokens, 2, K::StringDoneDq, 19, 2, false);
        Ok(())
    }

    #[test]
    fn interpolation_bare_ident_trailing_hyphen() -> anyhow::Result<()> {
        let input = r#""$yyyy-$mm""#;
        expect_token_kinds(
            input,
            vec![K::StringOpenDq, K::Ident, K::StringOpenDq, K::Ident, K::StringDoneDq],
        )
    }

    #[test]
    fn interpolation_literal_dollars() -> anyhow::Result<()> {
        // $ before a non-ident-start char is literal; \$ is always literal
        let input = r#""$5.99 + 100$ + \$x""#;
        expect_token_kinds(input, vec![K::StringDoneDq])
    }

    #[test]
    fn interpolation_start_end() -> anyhow::Result<()> {
        let input = r#" "${foo()}, $world" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDq,
                K::OpenBrace,
                K::Ident,
                K::OpenParen,
                K::CloseParen,
                K::CloseBrace,
                K::StringOpenDq,
                K::Ident,
                K::StringDoneDq,
            ],
        )
    }

    #[test]
    fn interpolation_string() -> anyhow::Result<()> {
        let input = r#""${"hello"}""#;
        expect_token_kinds(
            input,
            vec![K::StringOpenDq, K::OpenBrace, K::StringDoneDq, K::CloseBrace, K::StringDoneDq],
        )
    }

    #[test]
    fn interpolation_nested() -> anyhow::Result<()> {
        let input = r#" "${"hello ${var}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDq,
                K::OpenBrace,
                K::StringOpenDq,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDq,
                K::CloseBrace,
                K::StringDoneDq,
            ],
        )
    }

    #[test]
    fn interpolation_literal_braces() -> anyhow::Result<()> {
        let input = r#""Method 'sum' does not exist on type: '{ x: iword, y: iword }'""#;
        expect_token_kinds(input, vec![K::StringDoneDq])
    }

    #[test]
    fn interpolation_nested_with_braces() -> anyhow::Result<()> {
        let input = r#" "${"hello ${({ x: 42 }).x}"}" "#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenDq,
                K::OpenBrace,
                K::StringOpenDq,
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
                K::StringDoneDq,
                K::CloseBrace,
                K::StringDoneDq,
            ],
        )
    }

    #[test]
    fn backtick_string_1() -> anyhow::Result<()> {
        let input = r#"`Method 'sum' does not exist on type: '{ x: iword, y: iword }'`"#;
        expect_token_kinds(input, vec![K::StringDoneBt])
    }

    #[test]
    fn keyword_substring_is_correct() -> anyhow::Result<()> {
        let input = "lett";
        expect_token_kinds(input, vec![K::Ident])?;
        let input2 = "let";
        expect_token_kinds(input2, vec![K::KeywordLet])?;
        Ok(())
    }

    #[test]
    fn backtick_interpolation_mixed() -> anyhow::Result<()> {
        // Tests mode stack by mixing string types
        let input = r#"`
        ${"hello ${var}"}
        `"#;
        expect_token_kinds(
            input,
            vec![
                K::StringOpenBt,
                K::OpenBrace,
                K::StringOpenDq,
                K::OpenBrace,
                K::Ident,
                K::CloseBrace,
                K::StringDoneDq,
                K::CloseBrace,
                K::StringDoneBt,
            ],
        )
    }
}
