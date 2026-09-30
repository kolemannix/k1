// Copyright (c) 2026 knix
// All rights reserved.

use std::fmt;
use std::fmt::{Display, Formatter};

use crate::debug;
use crate::parse::BinaryOpKind;
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

/// A token too long for the positional Token; its `start` indexes this table
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
    /// In token order, one per TOKEN_FLAG_LONG token
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

    pub fn get_token_start_len(&self, token: Token) -> (u32, u32) {
        if token.flags & TOKEN_FLAG_LONG != 0 {
            let long = self.long_tokens[token.start as usize];
            (long.start, long.len)
        } else {
            (token.start, token.len as u32)
        }
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
    FmtSpec,

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FmtAlign {
    Left,
    Right,
    Center,
    SignZeros,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FmtSpec {
    pub fill: char,
    pub align: Option<FmtAlign>,
    pub width: u32,
    pub precision: Option<u32>,
}

impl FmtSpec {
    pub fn parse(text: &str) -> Option<FmtSpec> {
        fn align_of(c: char) -> Option<FmtAlign> {
            match c {
                '<' => Some(FmtAlign::Left),
                '>' => Some(FmtAlign::Right),
                '^' => Some(FmtAlign::Center),
                _ => None,
            }
        }
        fn digits(s: &str) -> (&str, &str) {
            let end = s.bytes().position(|b| !b.is_ascii_digit()).unwrap_or(s.len());
            s.split_at(end)
        }
        let mut chars = text.chars();
        let first = chars.next()?;
        let (fill, mut align, rest) = match chars.next().and_then(align_of) {
            Some(align) => (first, Some(align), chars.as_str()),
            None => match align_of(first) {
                Some(align) => (' ', Some(align), &text[1..]),
                None => (' ', None, text),
            },
        };
        if fill == '{' {
            return None;
        }
        let (width_text, rest) = digits(rest);
        let fill = if width_text.len() > 1 && width_text.starts_with('0') {
            if align.is_some() {
                return None;
            }
            align = Some(FmtAlign::SignZeros);
            '0'
        } else {
            fill
        };
        let width = if width_text.is_empty() { 0 } else { width_text.parse().ok()? };
        if (width == 0) != width_text.is_empty() || (align.is_some() && width == 0) {
            return None;
        }
        let precision = match rest.strip_prefix('.') {
            None if rest.is_empty() => None,
            None => return None,
            Some(rest) => {
                let (precision_text, rest) = digits(rest);
                if precision_text.is_empty() || !rest.is_empty() {
                    return None;
                }
                Some(precision_text.parse().ok()?)
            }
        };
        Some(FmtSpec { fill, align, width, precision })
    }
}

impl Display for FmtSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(":")?;
        match self.align {
            None => {}
            Some(FmtAlign::SignZeros) => f.write_str("0")?,
            Some(align) => {
                if self.fill != ' ' {
                    write!(f, "{}", self.fill)?;
                }
                let c = match align {
                    FmtAlign::Left => '<',
                    FmtAlign::Right => '>',
                    _ => '^',
                };
                write!(f, "{c}")?;
            }
        }
        if self.width != 0 {
            write!(f, "{}", self.width)?;
        }
        if let Some(precision) = self.precision {
            write!(f, ".{precision}")?;
        }
        Ok(())
    }
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
            K::FmtSpec => "<fmt spec>",

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
    pub fn is_keyword(&self) -> bool {
        (K::KeywordFn as u8..=K::KeywordDefer as u8).contains(&(*self as u8))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub lo: u32,
    pub len: u32,
}
static_assert_size!(Span, 8);

impl Span {
    pub const NONE: Span = Span { lo: 0, len: 0 };

    pub fn is_none(self) -> bool {
        self == Self::NONE
    }

    #[inline]
    pub fn end(&self) -> u32 {
        self.lo + self.len
    }

    pub fn to_u64(self) -> u64 {
        self.lo as u64 | (self.len as u64) << 32
    }

    pub fn from_u64(v: u64) -> Option<Span> {
        if v == 0 { None } else { Some(Span { lo: v as u32, len: (v >> 32) as u32 }) }
    }

    #[track_caller]
    #[inline]
    pub fn extend_to(&mut self, new_end: u32) {
        if cfg!(debug_assertions) {
            if new_end < self.end() {
                panic!("Attempt to extend span from {} to {}", self.end(), new_end)
            }
        }
        self.len = new_end - self.lo;
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
pub const TOKEN_FLAG_LONG: u8 = 0x08;

/// A token longer than u16::MAX (giant string literals) sets TOKEN_FLAG_LONG; `start`
/// then indexes `Lexed::long_tokens`.
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
                let index = long_tokens.len() as u32;
                long_tokens.push(LongToken { start, len });
                Token { kind, flags: flags | TOKEN_FLAG_LONG, len: 0, start: index }
            }
        }
    }

    pub fn span(&self, base: u32, long_tokens: &[LongToken]) -> Span {
        if self.flags & TOKEN_FLAG_LONG != 0 {
            let long = long_tokens[self.start as usize];
            Span { lo: base + long.start, len: long.len }
        } else {
            Span { lo: base + self.start, len: self.len as u32 }
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
                        ':' => {
                            if let LexMode::Interp { brace_depth: 1 } = lex_mode
                                && let Some(len) = self.fmt_spec_len()
                            {
                                self.advance_n(len + 1);
                                push_token(self, out, K::FmtSpec, n, len + 1);
                                return Ok(Some(()));
                            }
                            return_single!(K::Colon)
                        }
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
                            let start = n as usize;
                            let width = (c as u8).leading_ones().max(1) as usize;
                            let end = (start + width).min(self.content.len());
                            let text = String::from_utf8_lossy(&self.content[start..end]);
                            let message = if c.is_ascii() {
                                format!("Unexpected character {text}")
                            } else {
                                format!(
                                    "Unexpected character {text}: only ASCII is allowed outside strings and comments"
                                )
                            };
                            return Err(self.make_error(message, n, (end - start) as u32));
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

    fn fmt_spec_len(&self) -> Option<u32> {
        let start = self.pos as usize + 1;
        let rest = self.content.get(start..)?;
        let len = rest.iter().position(|b| *b == b'}' || *b == b'\n')?;
        if rest[len] != b'}' {
            return None;
        }
        let text = std::str::from_utf8(&rest[..len]).ok()?;
        FmtSpec::parse(text)?;
        Some(len as u32)
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
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

#[inline]
pub fn is_ident_char(c: char) -> bool {
    BYTE_CLASS.get(c as usize).is_some_and(|class| class & CLASS_IDENT != 0)
}

/// Whether c can start a bare `$ident` interpolation hole: an ident char that
/// could not begin a number, so `"$5.99"` stays literal text
#[inline]
fn is_ident_start(c: char) -> bool {
    is_ident_char(c) && c != '-' && !is_numeric_char(c)
}

#[inline]
fn is_numeric_char(c: char) -> bool {
    BYTE_CLASS.get(c as usize).is_some_and(|class| class & CLASS_NUMERIC != 0)
}

#[cfg(test)]
mod test {
    use crate::lex::{Lexed, TOKEN_LOOKAHEAD, Token, TokenKind as K, TokenTriviaKind, lex};

    #[test]
    fn byte_class_matches_char_methods() {
        for b in 0..=255u8 {
            let c = b as char;
            let class = super::BYTE_CLASS[b as usize];
            let ident = c.is_ascii_alphanumeric() || c == '_' || c == '-';
            let numeric = c.is_ascii_digit();
            assert_eq!(class & super::CLASS_IDENT != 0, ident, "ident class for byte {b:#x}");
            assert_eq!(class & super::CLASS_NUMERIC != 0, numeric, "numeric class for byte {b:#x}");
        }
    }

    #[test]
    fn non_ascii_outside_strings_is_rejected() {
        for input in ["let é = 1", "let aé = 1", "let ê = 1", "let x = 日", "x²"] {
            let lexed = lex(input, Lexed::default());
            let e = lexed.error.expect(input);
            assert!(e.message.contains("only ASCII"), "{input}: {}", e.message);
            assert!(input.is_char_boundary(e.start as usize), "{input}");
            assert!(input.is_char_boundary((e.start + e.len) as usize), "{input}");
        }
    }

    #[test]
    fn token_bounds_are_char_bounds() -> anyhow::Result<()> {
        let input = "let s = \"héllo $name wörld ${a + 1}日本\" // ç\nlet t = `é$x-ç` + 'a'";
        let lexed = lex_ok(input)?;
        for token in lexed.tokens.iter() {
            let (start, len) = lexed.get_token_start_len(*token);
            assert!(input.is_char_boundary(start as usize), "{token:?}");
            assert!(input.is_char_boundary((start + len) as usize), "{token:?}");
        }
        Ok(())
    }

    fn lex_ok(input: &str) -> anyhow::Result<Lexed> {
        let mut lexed = lex(input, Lexed::default());
        if let Some(e) = lexed.error {
            anyhow::bail!("{}", e.message);
        }
        lexed.tokens.truncate(lexed.tokens.len() - TOKEN_LOOKAHEAD);
        Ok(lexed)
    }

    fn set_up(input: &str) -> anyhow::Result<Vec<Token>> {
        Ok(lex_ok(input)?.tokens)
    }

    fn expect_token_kinds(input: &str, expected: Vec<K>) -> anyhow::Result<()> {
        let token_vec = set_up(input)?;
        let mut kinds: Vec<K> = Vec::with_capacity(token_vec.len());
        for t in &token_vec {
            kinds.push(t.kind);
        }
        assert_eq!(kinds.pop(), Some(K::Eof));
        assert_eq!(kinds, expected);
        Ok(())
    }

    fn assert_token(
        tokens: &[Token],
        index: usize,
        kind: K,
        start: u32,
        len: u32,
        is_whitespace_preceded: bool,
    ) {
        let span = tokens[index].span(0, &[]);
        assert_eq!(tokens[index].kind, kind);
        assert_eq!(span.lo, start);
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
        let tokens = set_up(input)?;
        let mut kinds: Vec<K> = Vec::with_capacity(tokens.len());
        for t in &tokens {
            kinds.push(t.kind);
        }
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
        let span0 = tokens[0].span(0, &[]);
        assert_eq!(span0.lo, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);
        let span1 = tokens[1].span(0, &[]);
        assert_eq!(span1.lo, 1);
        assert_eq!(span1.len, 2);
        assert_eq!(span1.end(), 3);
        assert!(!tokens[1].is_whitespace_preceded());
        Ok(())
    }

    #[test]
    fn minus_int() -> anyhow::Result<()> {
        let input = "- 43";
        let tokens = set_up(input)?;
        let mut kinds: Vec<K> = Vec::with_capacity(tokens.len());
        for t in &tokens {
            kinds.push(t.kind);
        }
        assert_eq!(kinds, vec![K::Minus, K::Numeric, K::Eof]);
        let span0 = tokens[0].span(0, &[]);
        assert_eq!(span0.lo, 0);
        assert_eq!(span0.len, 1);
        assert_eq!(span0.end(), 1);

        let span1 = tokens[1].span(0, &[]);
        assert_eq!(span1.lo, 2);
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
        let let_span = tokens[0].span(0, &[]);
        assert_eq!((let_span.lo, let_span.len), (45, 3));

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
        let tokens = set_up("\"foobear\"")?;
        assert_token(&tokens, 0, K::StringDoneDq, 0, 9, false);
        Ok(())
    }

    #[test]
    fn literal_string_in_call() -> anyhow::Result<()> {
        let tokens = set_up("let x = println(\"foobear\")")?;
        assert_token(&tokens, 0, K::KeywordLet, 0, 3, false);
        assert_token(&tokens, 1, K::Ident, 4, 1, true);
        assert_token(&tokens, 2, K::Equals, 6, 1, true);
        assert_token(&tokens, 3, K::Ident, 8, 7, true);
        assert_token(&tokens, 4, K::OpenParen, 15, 1, false);
        assert_token(&tokens, 5, K::StringDoneDq, 16, 9, false);
        assert_token(&tokens, 6, K::CloseParen, 25, 1, false);
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
    fn interpolation_fmt_spec() -> anyhow::Result<()> {
        let tokens = set_up(r#""${x:>8.2}${: f64}""#)?;
        assert_token(&tokens, 2, K::Ident, 3, 1, false);
        assert_token(&tokens, 3, K::FmtSpec, 4, 5, false);
        assert_token(&tokens, 6, K::OpenBrace, 10, 2, false);
        assert_token(&tokens, 7, K::Colon, 12, 1, false);
        Ok(())
    }

    #[test]
    fn fmt_spec_parse() {
        use super::{FmtAlign as A, FmtSpec};
        let spec = |fill, align, width, precision| Some(FmtSpec { fill, align, width, precision });
        assert_eq!(FmtSpec::parse(".2"), spec(' ', None, 0, Some(2)));
        assert_eq!(FmtSpec::parse("8"), spec(' ', None, 8, None));
        assert_eq!(FmtSpec::parse(">8.3"), spec(' ', Some(A::Right), 8, Some(3)));
        assert_eq!(FmtSpec::parse("*^10"), spec('*', Some(A::Center), 10, None));
        assert_eq!(FmtSpec::parse("·<4"), spec('·', Some(A::Left), 4, None));
        assert_eq!(FmtSpec::parse("0<5"), spec('0', Some(A::Left), 5, None));
        assert_eq!(FmtSpec::parse("08.1"), spec('0', Some(A::SignZeros), 8, Some(1)));
        for bad in
            ["", "0", "00", "<", "<05", "8.", ".x", "x", "none", " f64", "{<3", "99999999999"]
        {
            assert_eq!(FmtSpec::parse(bad), None, "{bad}");
        }
        assert_eq!(FmtSpec::parse("*^10").unwrap().to_string(), ":*^10");
        assert_eq!(FmtSpec::parse("08.1").unwrap().to_string(), ":08.1");
    }

    #[test]
    fn interpolation_bare_ident() -> anyhow::Result<()> {
        let tokens = set_up(r#""Hello, $wide-world!""#)?;
        assert_token(&tokens, 0, K::StringOpenDq, 0, 8, false);
        assert_token(&tokens, 1, K::Ident, 9, 10, false);
        assert_token(&tokens, 2, K::StringDoneDq, 19, 2, false);
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
