use std::fmt::{Display, Formatter, Write};
use std::num::NonZeroU32;

use crate::compiler::CompilerConfig;
use crate::pool::Pool;
use crate::typer::{BinaryOpKind, ErrorLevel, Linkage};
use crate::{lex::*, static_assert_size};
use fxhash::FxHashMap;
use log::trace;
use string_interner::backend::StringBackend;
use string_interner::Symbol;
use TokenKind as K;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedTypeDefnId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedFunctionId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedConstantId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedAbilityId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedAbilityImplId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedNamespaceId(u32);
#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub struct ParsedExpressionId(NonZeroU32);
impl ParsedExpressionId {
    pub const PENDING: ParsedExpressionId = ParsedExpressionId(NonZeroU32::MAX);
}
impl From<NonZeroU32> for ParsedExpressionId {
    fn from(value: NonZeroU32) -> Self {
        ParsedExpressionId(value)
    }
}
impl From<ParsedExpressionId> for NonZeroU32 {
    fn from(val: ParsedExpressionId) -> Self {
        val.0
    }
}
impl Display for ParsedExpressionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub struct ParsedStmtId(NonZeroU32);
impl From<NonZeroU32> for ParsedStmtId {
    fn from(value: NonZeroU32) -> Self {
        ParsedStmtId(value)
    }
}

impl From<ParsedStmtId> for NonZeroU32 {
    fn from(val: ParsedStmtId) -> Self {
        val.0
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub struct ParsedTypeExprId(NonZeroU32);
impl From<NonZeroU32> for ParsedTypeExprId {
    fn from(value: NonZeroU32) -> Self {
        ParsedTypeExprId(value)
    }
}
impl From<ParsedTypeExprId> for NonZeroU32 {
    fn from(val: ParsedTypeExprId) -> Self {
        val.0
    }
}

impl Display for ParsedTypeExprId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedPatternId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedUseId(u32);

#[derive(Debug, Clone)]
pub enum ParsedDirective {
    ConditionalCompile { condition: ParsedExpressionId, span: SpanId },
    CompilerDebug { span: SpanId },
}

#[derive(Debug, Clone)]
pub struct ParsedUse {
    pub target: NamespacedIdentifier,
    pub alias: Option<Identifier>,
    pub span: SpanId,
}

pub type FileId = u32;

#[cfg(test)]
mod parse_test;

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum ParsedId {
    Use(ParsedUseId),
    Function(ParsedFunctionId),
    TypeDefn(ParsedTypeDefnId),
    Namespace(ParsedNamespaceId),
    Ability(ParsedAbilityId),
    AbilityImpl(ParsedAbilityImplId),
    Constant(ParsedConstantId),
    Expression(ParsedExpressionId),
    TypeExpression(ParsedTypeExprId),
    Pattern(ParsedPatternId),
}

impl Display for ParsedId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedId::Use(id) => write!(f, "use#{}", id.0),
            ParsedId::Function(id) => write!(f, "fn#{}", id.0),
            ParsedId::TypeDefn(id) => write!(f, "type#{}", id.0),
            ParsedId::Namespace(id) => write!(f, "ns#{}", id.0),
            ParsedId::Ability(id) => write!(f, "ability#{}", id.0),
            ParsedId::AbilityImpl(id) => write!(f, "ability_impl#{}", id.0),
            ParsedId::Constant(id) => write!(f, "const#{}", id.0),
            ParsedId::Expression(id) => write!(f, "expr#{}", id.0),
            ParsedId::TypeExpression(id) => write!(f, "type_expr#{}", id.0),
            ParsedId::Pattern(id) => write!(f, "pattern#{}", id.0),
        }
    }
}

impl ParsedId {
    pub fn expect_type_defn(self) -> ParsedTypeDefnId {
        match self {
            ParsedId::TypeDefn(id) => id,
            _ => panic!("Expected type definition"),
        }
    }

    pub fn as_function_id(&self) -> Option<ParsedFunctionId> {
        match self {
            ParsedId::Function(fn_id) => Some(*fn_id),
            _ => None,
        }
    }
}

impl From<ParsedTypeExprId> for ParsedId {
    fn from(id: ParsedTypeExprId) -> Self {
        ParsedId::TypeExpression(id)
    }
}

impl From<ParsedExpressionId> for ParsedId {
    fn from(id: ParsedExpressionId) -> Self {
        ParsedId::Expression(id)
    }
}

impl From<ParsedTypeDefnId> for ParsedId {
    fn from(id: ParsedTypeDefnId) -> Self {
        ParsedId::TypeDefn(id)
    }
}

impl From<ParsedFunctionId> for ParsedId {
    fn from(id: ParsedFunctionId) -> Self {
        ParsedId::Function(id)
    }
}

impl ParsedId {
    pub fn is_valid_definition(&self) -> bool {
        match self {
            ParsedId::Function(_) => true,
            ParsedId::TypeDefn(_) => true,
            ParsedId::Ability(_) => true,
            ParsedId::AbilityImpl(_) => true,
            ParsedId::Constant(_) => true,
            ParsedId::Namespace(_) => true,
            ParsedId::Expression(_) => false,
            ParsedId::TypeExpression(_) => false,
            ParsedId::Pattern(_) => false,
            ParsedId::Use(_) => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListExpr {
    pub elements: Vec<ParsedExpressionId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedNumericLiteral {
    pub text: String,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Unit(SpanId),
    Char(u8, SpanId),
    Numeric(ParsedNumericLiteral),
    Bool(bool, SpanId),
    String(String, SpanId),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit(_) => f.write_str("()"),
            Literal::Char(byte, _) => {
                f.write_char('\'')?;
                f.write_char(*byte as char)?;
                f.write_char('\'')
            }
            Literal::Numeric(i) => f.write_str(&i.text),
            Literal::Bool(true, _) => f.write_str("true"),
            Literal::Bool(false, _) => f.write_str("false"),
            Literal::String(s, _) => {
                f.write_char('"')?;
                f.write_str(s)?;
                f.write_char('"')
            }
        }
    }
}

impl Literal {
    pub fn get_span(&self) -> SpanId {
        match self {
            Literal::Unit(span) => *span,
            Literal::Char(_, span) => *span,
            Literal::Numeric(i) => i.span,
            Literal::Bool(_, span) => *span,
            Literal::String(_, span) => *span,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Identifier(string_interner::symbol::SymbolU32);

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Identifier> for usize {
    fn from(value: Identifier) -> Self {
        value.0.to_usize()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
    }
}

#[derive(Debug, Clone)]
pub struct NamespacedIdentifier {
    pub namespaces: Vec<Identifier>,
    pub name: Identifier,
    pub span: SpanId,
}
impl NamespacedIdentifier {
    pub fn naked(name: Identifier, span: SpanId) -> NamespacedIdentifier {
        NamespacedIdentifier { namespaces: Vec::new(), name, span }
    }
}

// We use the default StringInterner, which uses a contiguous string as its backend
// and u32 symbols
#[derive(Debug, Clone)]
pub struct Identifiers {
    intern_pool: string_interner::StringInterner<StringBackend>,
}
impl Identifiers {
    pub fn intern(&mut self, s: impl AsRef<str>) -> Identifier {
        let s = self.intern_pool.get_or_intern(&s);
        Identifier(s)
    }
    pub fn get(&self, s: impl AsRef<str>) -> Option<Identifier> {
        self.intern_pool.get(&s).map(Identifier)
    }
    pub fn get_name(&self, id: Identifier) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve identifier")
    }
}

impl Default for Identifiers {
    fn default() -> Self {
        let intern_pool = string_interner::StringInterner::with_capacity(65536);
        let mut this = Identifiers { intern_pool };
        for builtin_ident in Identifiers::BUILTIN_IDENTS.iter() {
            this.intern(builtin_ident);
        }
        this
    }
}

#[derive(Debug, Clone)]
pub struct FnCallArg {
    pub name: Option<Identifier>,
    pub value: ParsedExpressionId,
}

impl FnCallArg {
    pub fn unnamed(value: ParsedExpressionId) -> FnCallArg {
        FnCallArg { value, name: None }
    }
}

#[derive(Debug, Clone)]
pub struct NamedTypeArg {
    pub name: Option<Identifier>,
    pub type_expr: ParsedTypeExprId,
}

impl NamedTypeArg {
    pub fn unnamed(type_expr: ParsedTypeExprId) -> NamedTypeArg {
        NamedTypeArg { type_expr, name: None }
    }
}

// TOOD(perf): FnCall is huge
static_assert_size!(FnCall, 120);
#[derive(Debug, Clone)]
pub struct FnCall {
    pub name: NamespacedIdentifier,
    pub type_args: Vec<NamedTypeArg>,
    pub args: Vec<FnCallArg>,
    pub explicit_context_args: Vec<FnCallArg>,
    pub span: SpanId,
    pub is_method: bool,
    pub id: ParsedExpressionId,
}

impl FnCall {
    pub fn arg_by_name(&self, name: Identifier) -> Option<(usize, &FnCallArg)> {
        self.args.iter().enumerate().find(|(_, arg)| arg.name.is_some_and(|n| n == name))
    }
}

#[derive(Debug, Clone)]
pub struct ValDef {
    pub name: Identifier,
    pub type_expr: Option<ParsedTypeExprId>,
    pub value: ParsedExpressionId,
    pub span: SpanId,
    flags: u8,
}

impl ValDef {
    pub const FLAG_MUTABLE: u8 = 1;
    pub const FLAG_CONTEXT: u8 = 2;
    pub const FLAG_REFERENCING: u8 = 4;

    pub fn make_flags(mutable: bool, context: bool, referencing: bool) -> u8 {
        let mut flags = 0;
        if mutable {
            flags |= Self::FLAG_MUTABLE;
        }
        if context {
            flags |= Self::FLAG_CONTEXT;
        }
        if referencing {
            flags |= Self::FLAG_REFERENCING;
        }
        flags
    }

    pub fn set_mutable(&mut self) {
        self.flags |= Self::FLAG_MUTABLE;
    }
    pub fn is_mutable(&self) -> bool {
        self.flags & Self::FLAG_MUTABLE != 0
    }

    pub fn set_context(&mut self) {
        self.flags |= Self::FLAG_CONTEXT;
    }
    pub fn is_context(&self) -> bool {
        self.flags & Self::FLAG_CONTEXT != 0
    }

    pub fn set_referencing(&mut self) {
        self.flags |= Self::FLAG_REFERENCING;
    }
    pub fn is_referencing(&self) -> bool {
        self.flags & Self::FLAG_REFERENCING != 0
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub op_kind: BinaryOpKind,
    pub lhs: ParsedExpressionId,
    pub rhs: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParsedUnaryOpKind {
    BooleanNegation,
}

impl Display for ParsedUnaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedUnaryOpKind::BooleanNegation => f.write_str("not "),
        }
    }
}

impl ParsedUnaryOpKind {
    pub fn from_tokenkind(kind: TokenKind) -> Option<ParsedUnaryOpKind> {
        match kind {
            TokenKind::KeywordNot => Some(ParsedUnaryOpKind::BooleanNegation),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op_kind: ParsedUnaryOpKind,
    pub expr: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: NamespacedIdentifier,
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("var#{}", self.name.name))
    }
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: ParsedExpressionId,
    pub field_name: Identifier,
    pub type_args: Vec<NamedTypeArg>,
    pub is_coalescing: bool,  // ?.
    pub is_referencing: bool, // *.
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructValueField {
    pub name: Identifier,
    pub span: SpanId,
    /// expr is optional due to shorthand syntax
    pub expr: Option<ParsedExpressionId>,
}

#[derive(Debug, Clone)]
/// Example:
/// { foo: 1, bar: false }
///   ^................^ fields
pub struct Struct {
    pub fields: Vec<StructValueField>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
/// Example: users  [42]
///          ^target ^index_value
pub struct IndexOperation {
    pub target: ParsedExpressionId,
    pub index_expr: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct MethodCall {
    pub base: ParsedExpressionId,
    pub call: Box<FnCall>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct OptionalGet {
    pub base: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct AnonEnumConstructor {
    pub variant_name: Identifier,
    pub payload: Option<ParsedExpressionId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumConstructor {
    pub variant_name: Identifier,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedIsExpression {
    pub target_expression: ParsedExpressionId,
    pub pattern: ParsedPatternId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedMatchCase {
    pub patterns: smallvec::SmallVec<[ParsedPatternId; 1]>,
    pub guard_condition_expr: Option<ParsedExpressionId>,
    pub expression: ParsedExpressionId,
}

#[derive(Debug, Clone)]
pub struct ParsedMatchExpression {
    pub match_subject: ParsedExpressionId,
    pub cases: Vec<ParsedMatchCase>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedAsCast {
    pub base_expr: ParsedExpressionId,
    pub dest_type: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct LambdaArgDefn {
    pub binding: Identifier,
    pub ty: Option<ParsedTypeExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedLambda {
    pub arguments: Vec<LambdaArgDefn>,
    pub return_type: Option<ParsedTypeExprId>,
    pub body: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum InterpolatedStringPart {
    // TODO: Put spans on each string part
    String(String),
    Identifier(Identifier),
}

#[derive(Debug, Clone)]
pub struct ParsedInterpolatedString {
    pub parts: Vec<InterpolatedStringPart>,
    pub span: SpanId,
}

static_assert_size!(ParsedExpression, 120); // Get back down ideally below 50
#[derive(Debug, Clone)]
pub enum ParsedExpression {
    /// ```md
    /// <lhs: expr> == <rhs: expr>
    /// ```
    BinaryOp(BinaryOp),
    /// ```md
    /// !b, *b
    /// ```
    UnaryOp(UnaryOp),
    /// ```md
    /// 42, "asdf"
    /// ```
    Literal(Literal),
    /// ```md
    /// "hello, \{x}"
    /// ```
    InterpolatedString(ParsedInterpolatedString),
    /// ```md
    /// square(1, 2)
    /// ```
    FnCall(FnCall),
    /// ```md
    /// x
    /// ```
    Variable(Variable),
    /// ```md
    /// x.b, Opt.None[i32] (overloaded to handle enum constrs)
    /// ```
    FieldAccess(FieldAccess),
    /// ```md
    /// { <a: stmt>; <b: stmt>; <c: stmt> }
    /// ```
    Block(Block),
    /// ```md
    /// if <cond: expr> <cons: expr> else <alt: expr>
    /// ```
    If(IfExpr),
    /// ```md
    /// while <cond: expr> <body: expr>
    /// ```
    While(ParsedWhileExpr),
    /// ```md
    /// loop <body: block>
    /// ```
    Loop(ParsedLoopExpr),
    /// ```md
    /// { x: <expr>, y: <expr> }
    /// ```
    Struct(Struct),
    /// ```md
    /// [<expr>, <expr>, <expr>]
    /// ```
    ListLiteral(ListExpr),
    /// ```md
    /// for <ident> in <coll: expr> do <body: expr>
    /// ```
    For(ForExpr),
    /// ```md
    /// .<ident>
    /// .<ident>(<expr>)
    /// ```
    AnonEnumConstructor(AnonEnumConstructor),
    /// ```md
    /// <expr> is <pat>
    /// ```
    Is(ParsedIsExpression),
    /// ```md
    /// when <expr> is {
    /// <a: pat> -> <expr>,
    /// <b: pat> -> <expr>
    /// }
    /// ```
    Match(ParsedMatchExpression),
    /// ```md
    /// x as u64, y as .Color
    /// ```
    AsCast(ParsedAsCast),
    Lambda(ParsedLambda),
    Builtin(SpanId),
}

impl ParsedExpression {
    pub fn is_literal(e: &ParsedExpression) -> bool {
        matches!(e, ParsedExpression::Literal(_))
    }
    pub fn expect_literal(&self) -> &Literal {
        match self {
            ParsedExpression::Literal(lit) => lit,
            _ => panic!("expected literal"),
        }
    }
    #[inline]
    pub fn get_span(&self) -> SpanId {
        match self {
            Self::BinaryOp(op) => op.span,
            Self::UnaryOp(op) => op.span,
            Self::Literal(lit) => lit.get_span(),
            Self::InterpolatedString(is) => is.span,
            Self::FnCall(call) => call.span,
            Self::Variable(var) => var.name.span,
            Self::FieldAccess(acc) => acc.span,
            Self::Block(block) => block.span,
            Self::If(if_expr) => if_expr.span,
            Self::While(while_expr) => while_expr.span,
            Self::Loop(loop_expr) => loop_expr.span,
            Self::Struct(struc) => struc.span,
            Self::ListLiteral(list_expr) => list_expr.span,
            Self::For(for_expr) => for_expr.span,
            Self::AnonEnumConstructor(tag_expr) => tag_expr.span,
            Self::Is(is_expr) => is_expr.span,
            Self::Match(match_expr) => match_expr.span,
            Self::AsCast(as_cast) => as_cast.span,
            Self::Lambda(lambda) => lambda.span,
            Self::Builtin(span) => *span,
        }
    }

    pub fn as_match(&self) -> Option<&ParsedMatchExpression> {
        if let Self::Match(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn expect_cast(&self) -> &ParsedAsCast {
        match self {
            ParsedExpression::AsCast(as_cast) => as_cast,
            _ => panic!("expected cast expression"),
        }
    }

    pub fn expect_fn_call(&self) -> &FnCall {
        match self {
            ParsedExpression::FnCall(call) => call,
            _ => panic!("expected fn call"),
        }
    }

    pub fn expect_lambda(&self) -> &ParsedLambda {
        match self {
            ParsedExpression::Lambda(c) => c,
            _ => panic!("expected lambda"),
        }
    }
}

enum ExprStackMember {
    Operator(BinaryOpKind, SpanId),
    Expr(ParsedExpressionId),
}

impl ExprStackMember {
    fn expect_expr(self) -> ParsedExpressionId {
        match self {
            ExprStackMember::Expr(expr) => expr,
            _ => panic!("expected expr"),
        }
    }
    fn expect_operator(self) -> (BinaryOpKind, SpanId) {
        match self {
            ExprStackMember::Operator(kind, span) => (kind, span),
            _ => panic!("expected operator"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStructPattern {
    pub fields: Vec<(Identifier, ParsedPatternId)>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumPattern {
    pub variant_tag: Identifier,
    pub payload_pattern: Option<ParsedPatternId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedReferencePattern {
    pub inner: ParsedPatternId,
    pub span: SpanId,
}

// https://bnfplayground.pauliankline.com/?bnf=%3Cpattern%3E%20%3A%3A%3D%20%3Cliteral%3E%20%7C%20%3Cvariable%3E%20%7C%20%3Cenum%3E%20%7C%20%3Cstruct%3E%0A%3Cliteral%3E%20%3A%3A%3D%20%22(%22%20%22)%22%20%7C%20%22%5C%22%22%20%3Cident%3E%20%22%5C%22%22%20%7C%20%5B0-9%5D%2B%20%7C%20%22%27%22%20%5Ba-z%5D%20%22%27%22%20%7C%20%22None%22%0A%3Cvariable%3E%20%3A%3A%3D%20%3Cident%3E%0A%3Cident%3E%20%3A%3A%3D%20%5Ba-z%5D*%0A%3Cenum%3E%20%3A%3A%3D%20%22.%22%20%3Cident%3E%20(%20%22(%22%20%3Cpattern%3E%20%22)%22%20)%3F%0A%3Cstruct%3E%20%3A%3A%3D%20%22%7B%22%20(%20%3Cident%3E%20%22%3A%20%22%20%3Cpattern%3E%20%22%2C%22%3F%20)*%20%22%7D%22%20&name=
// <pattern> ::= <literal> | <variable> | <enum> | <struct>
// <literal> ::= "(" ")" | "\"" <ident> "\"" | [0-9]+ | "'" [a-z] "'" | "None"
// <variable> ::= <ident>
// <ident> ::= [a-z]*
// <enum> ::= "." <ident> ( "(" <pattern> ")" )?
// <struct> ::= "{" ( <ident> ": " <pattern> ","? )* "}"

#[derive(Debug, Clone)]
pub enum ParsedPattern {
    Literal(ParsedExpressionId),
    Variable(Identifier, SpanId),
    Struct(ParsedStructPattern),
    Enum(ParsedEnumPattern),
    Wildcard(SpanId),
    Reference(ParsedReferencePattern),
}

impl ParsedPattern {}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: ParsedExpressionId,
    pub rhs: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct SetStmt {
    pub lhs: ParsedExpressionId,
    pub rhs: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: ParsedExpressionId,
    pub cons: ParsedExpressionId,
    pub alt: Option<ParsedExpressionId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedWhileExpr {
    pub cond: ParsedExpressionId,
    pub body: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedLoopExpr {
    pub body: Block,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForExprType {
    Yield,
    Do,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
    pub iterable_expr: ParsedExpressionId,
    pub binding: Option<Identifier>,
    pub body_block: Block,
    pub expr_type: ForExprType,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct UseStmt {
    pub use_id: ParsedUseId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Use(UseStmt),                       // use core/list/new as foo
    ValDef(ValDef),                     // let x = 42
    Assignment(Assignment),             // x = 42
    SetRef(SetStmt),                    // x <- 42
    LoneExpression(ParsedExpressionId), // println("asdfasdf")
}
static_assert_size!(ParsedStmt, 24);

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<ParsedStmtId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Identifier,
    pub ty: ParsedTypeExprId,
    pub private: bool,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Vec<StructTypeField>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypeApplication {
    pub name: NamespacedIdentifier,
    pub args: Vec<NamedTypeArg>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedOptional {
    pub base: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedReference {
    pub base: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumVariant {
    pub tag_name: Identifier,
    pub payload_expression: Option<ParsedTypeExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumType {
    pub variants: Vec<ParsedEnumVariant>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedDotMemberAccess {
    pub base: ParsedTypeExprId,
    pub member_name: Identifier,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeModifier {
    Alias,
    Opaque,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum NumericWidth {
    B8,
    B16,
    B32,
    B64,
}

impl NumericWidth {
    pub fn bit_width(&self) -> u32 {
        match self {
            NumericWidth::B8 => 8,
            NumericWidth::B16 => 16,
            NumericWidth::B32 => 32,
            NumericWidth::B64 => 64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedNumericType {
    pub width: NumericWidth,
    pub signed: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedFunctionType {
    pub params: Vec<ParsedTypeExprId>,
    pub return_type: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedTypeOf {
    pub target_expr: ParsedExpressionId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct SomeQuantifier {
    pub inner: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedTypeExpr {
    Builtin(SpanId),
    Struct(StructType),
    TypeApplication(TypeApplication),
    Optional(ParsedOptional),
    Reference(ParsedReference),
    Enum(ParsedEnumType),
    DotMemberAccess(ParsedDotMemberAccess),
    Function(ParsedFunctionType),
    TypeOf(ParsedTypeOf),
    SomeQuant(SomeQuantifier),
}

impl ParsedTypeExpr {
    #[inline]
    pub fn get_span(&self) -> SpanId {
        match self {
            ParsedTypeExpr::Builtin(span) => *span,
            ParsedTypeExpr::Struct(struc) => struc.span,
            ParsedTypeExpr::TypeApplication(app) => app.span,
            ParsedTypeExpr::Optional(opt) => opt.span,
            ParsedTypeExpr::Reference(r) => r.span,
            ParsedTypeExpr::Enum(e) => e.span,
            ParsedTypeExpr::DotMemberAccess(a) => a.span,
            ParsedTypeExpr::Function(f) => f.span,
            ParsedTypeExpr::TypeOf(tof) => tof.span,
            ParsedTypeExpr::SomeQuant(q) => q.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeParam {
    pub name: Identifier,
    pub constraints: Vec<ParsedTypeConstraintExpr>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedTypeConstraintExpr {
    Ability(ParsedAbilityExpr),
}

impl ParsedTypeConstraintExpr {
    pub fn span(&self) -> SpanId {
        match self {
            ParsedTypeConstraintExpr::Ability(app) => app.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeConstraint {
    pub name: Identifier,
    pub constraint_expr: ParsedTypeConstraintExpr,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub name: Identifier,
    pub type_params: Vec<ParsedTypeParam>,
    pub params: Vec<FnArgDef>,
    pub context_params: Vec<FnArgDef>,
    pub ret_type: ParsedTypeExprId,
    pub block: Option<Block>,
    pub signature_span: SpanId,
    pub span: SpanId,
    pub linkage: Linkage,
    pub directives: Vec<ParsedDirective>,
    pub additional_where_constraints: Vec<ParsedTypeConstraint>,
    pub id: ParsedFunctionId,
}

#[derive(Debug, Clone)]
pub struct FnArgDef {
    pub name: Identifier,
    pub ty: ParsedTypeExprId,
    pub span: SpanId,
    pub modifiers: FnArgDefModifiers,
}

#[derive(Debug, Clone, Copy)]
pub struct FnArgDefModifiers(u32);
impl FnArgDefModifiers {
    pub fn new(context: bool) -> Self {
        let mut s = Self(0);
        if context {
            s.set_context();
        }
        s
    }

    pub fn set_context(&mut self) {
        self.0 |= 1;
    }

    pub fn is_context(&self) -> bool {
        self.0 & 1 != 0
    }
}

#[derive(Debug, Clone)]
pub struct ParsedConstant {
    pub name: Identifier,
    pub ty: ParsedTypeExprId,
    pub value_expr: ParsedExpressionId,
    pub span: SpanId,
    pub id: ParsedConstantId,
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDefnFlags(u32);
impl ParsedTypeDefnFlags {
    pub fn new(alias: bool, opaque: bool) -> Self {
        let mut s = Self(0);
        if alias {
            s.set_alias();
        }
        if opaque {
            s.set_opaque();
        }
        s
    }

    pub fn set_alias(&mut self) {
        self.0 |= 1;
    }

    pub fn set_opaque(&mut self) {
        self.0 |= 2;
    }

    pub fn is_alias(&self) -> bool {
        self.0 & 1 != 0
    }

    pub fn is_opaque(&self) -> bool {
        self.0 & 2 != 0
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDefn {
    pub name: Identifier,
    pub value_expr: ParsedTypeExprId,
    pub type_params: Vec<ParsedTypeParam>,
    pub span: SpanId,
    pub id: ParsedTypeDefnId,
    pub flags: ParsedTypeDefnFlags,
}

#[derive(Debug, Clone)]
pub struct ParsedAbilityParameter {
    pub name: Identifier,
    pub is_impl_param: bool,
    pub constraints: Vec<ParsedTypeConstraintExpr>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedAbility {
    pub name: Identifier,
    pub functions: Vec<ParsedFunctionId>,
    pub span: SpanId,
    pub params: Vec<ParsedAbilityParameter>,
    pub id: ParsedAbilityId,
}

#[derive(Debug, Clone)]
pub struct ParsedAbilityExpr {
    pub name: NamespacedIdentifier,
    pub arguments: Vec<AbilityTypeArgument>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct AbilityTypeArgument {
    pub name: Identifier,
    pub value: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedAbilityImplementation {
    pub ability_expr: ParsedAbilityExpr,
    pub generic_impl_params: Vec<ParsedTypeParam>,
    pub self_type: ParsedTypeExprId,
    pub functions: Vec<ParsedFunctionId>,
    pub id: ParsedAbilityImplId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedNamespace {
    pub name: Identifier,
    pub definitions: Vec<ParsedId>,
    pub id: ParsedNamespaceId,
    pub span: SpanId,
}

pub struct ParsedExpressionPool {
    // `expressions` and `type_hints` form a Struct-of-Arrays relationship
    expressions: Pool<ParsedExpression, ParsedExpressionId>,
    type_hints: Pool<Option<ParsedTypeExprId>, ParsedExpressionId>,
    directives: FxHashMap<ParsedExpressionId, Vec<ParsedDirective>>,
}
impl ParsedExpressionPool {
    pub fn new(capacity: usize) -> Self {
        ParsedExpressionPool {
            expressions: Pool::with_capacity("parsed_expr", capacity),
            type_hints: Pool::with_capacity("parsed_expr_type_hint", capacity),
            directives: FxHashMap::default(),
        }
    }

    pub fn set_type_hint(&mut self, id: ParsedExpressionId, ty: ParsedTypeExprId) {
        *self.type_hints.get_mut(id) = Some(ty)
    }

    pub fn get_type_hint(&self, id: ParsedExpressionId) -> Option<ParsedTypeExprId> {
        *self.type_hints.get(id)
    }

    pub fn add_directives(&mut self, id: ParsedExpressionId, directives: Vec<ParsedDirective>) {
        self.directives.insert(id, directives);
    }

    pub fn get_directives(&self, id: ParsedExpressionId) -> &[ParsedDirective] {
        self.directives.get(&id).map(|v| &v[..]).unwrap_or(&[])
    }

    pub fn add_expression(&mut self, mut expression: ParsedExpression) -> ParsedExpressionId {
        let id: ParsedExpressionId = self.expressions.next_id();
        if let ParsedExpression::FnCall(call) = &mut expression {
            call.id = id;
        }
        self.expressions.add(expression);
        self.type_hints.add(None);
        id
    }

    pub fn get(&self, id: ParsedExpressionId) -> &ParsedExpression {
        self.expressions.get(id)
    }

    pub fn get_span(&self, id: ParsedExpressionId) -> SpanId {
        self.get(id).get_span()
    }

    pub fn count(&self) -> usize {
        self.expressions.len()
    }
}

pub struct ParsedTypeExpressionPool {
    type_expressions: Pool<ParsedTypeExpr, ParsedTypeExprId>,
}
impl ParsedTypeExpressionPool {
    fn new(capacity: usize) -> Self {
        ParsedTypeExpressionPool {
            type_expressions: Pool::with_capacity("parsed_type_expr", capacity),
        }
    }

    pub fn add(&mut self, expression: ParsedTypeExpr) -> ParsedTypeExprId {
        self.type_expressions.add(expression)
    }
    pub fn get(&self, id: ParsedTypeExprId) -> &ParsedTypeExpr {
        self.type_expressions.get(id)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ParsedUsePool {
    uses: Vec<ParsedUse>,
}
impl ParsedUsePool {
    pub fn add_use(&mut self, r#use: ParsedUse) -> ParsedUseId {
        let id = self.uses.len();
        self.uses.push(r#use);
        ParsedUseId(id as u32)
    }
    pub fn get_use(&self, id: ParsedUseId) -> &ParsedUse {
        &self.uses[id.0 as usize]
    }
}

#[derive(Debug, Default, Clone)]
pub struct ParsedPatternPool {
    patterns: Vec<ParsedPattern>,
}
impl ParsedPatternPool {
    pub fn add_pattern(&mut self, pattern: ParsedPattern) -> ParsedPatternId {
        let id = self.patterns.len();
        self.patterns.push(pattern);
        ParsedPatternId(id as u32)
    }
    pub fn get_pattern(&self, id: ParsedPatternId) -> &ParsedPattern {
        &self.patterns[id.0 as usize]
    }
}

#[derive(Debug, Default, Clone)]
pub struct Sources {
    sources: Vec<Source>,
}

impl Sources {
    pub fn insert(&mut self, mut source: Source) {
        let id = self.next_file_id();
        source.file_id = id;
        self.sources.push(source);
    }

    pub fn get_main(&self) -> &Source {
        &self.sources[0]
    }

    pub fn get_source(&self, file_id: FileId) -> &Source {
        &self.sources[file_id as usize]
    }

    pub fn get_line_for_span_start(&self, span: Span) -> Option<&Line> {
        self.sources[span.file_id as usize].get_line_for_span_start(span)
    }

    pub fn get_lines_for_span(&self, span: Span) -> Option<(&Line, &Line)> {
        let start = self.sources[span.file_id as usize].get_line_for_offset(span.start)?;
        let end = self.sources[span.file_id as usize].get_line_for_offset(span.end())?;
        Some((start, end))
    }

    pub fn get_span_content(&self, span: Span) -> &str {
        self.sources[span.file_id as usize].get_span_content(span)
    }

    pub fn source_by_span(&self, span: Span) -> &Source {
        self.get_source(span.file_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &Source)> {
        self.sources.iter().map(|source| (source.file_id, source))
    }

    pub fn next_file_id(&self) -> FileId {
        self.sources.len() as u32
    }
}

pub struct ParsedModule {
    pub name: String,
    pub name_id: Identifier,
    pub config: CompilerConfig,
    pub spans: Spans,
    pub functions: Vec<ParsedFunction>,
    pub constants: Vec<ParsedConstant>,
    pub type_defns: Vec<ParsedTypeDefn>,
    pub namespaces: Vec<ParsedNamespace>,
    pub abilities: Vec<ParsedAbility>,
    pub ability_impls: Vec<ParsedAbilityImplementation>,
    pub sources: Sources,
    pub identifiers: Identifiers,
    pub exprs: ParsedExpressionPool,
    pub type_exprs: ParsedTypeExpressionPool,
    pub patterns: ParsedPatternPool,
    pub stmts: Pool<ParsedStmt, ParsedStmtId>,
    pub uses: ParsedUsePool,
    pub errors: Vec<ParseError>,
}

impl ParsedModule {
    pub fn make(name: String, config: CompilerConfig) -> ParsedModule {
        let mut identifiers = Identifiers::default();
        let name_id = identifiers.intern(&name);
        ParsedModule {
            name,
            name_id,
            config,
            spans: Spans::new(),
            functions: Vec::new(),
            constants: Vec::new(),
            type_defns: Vec::new(),
            namespaces: Vec::new(),
            abilities: Vec::new(),
            ability_impls: Vec::new(),
            sources: Sources::default(),
            identifiers,
            exprs: ParsedExpressionPool::new(16384),
            type_exprs: ParsedTypeExpressionPool::new(8192),
            patterns: ParsedPatternPool::default(),
            stmts: Pool::with_capacity("parsed_stmts", 8192),
            uses: ParsedUsePool::default(),
            errors: Vec::new(),
        }
    }

    pub fn get_span_content(&self, span_id: SpanId) -> &str {
        let span = self.spans.get(span_id);
        self.sources.get_span_content(span)
    }

    pub fn get_function(&self, id: ParsedFunctionId) -> &ParsedFunction {
        &self.functions[id.0 as usize]
    }

    pub fn add_function(&mut self, mut function: ParsedFunction) -> ParsedFunctionId {
        let id = self.functions.len();
        let id = ParsedFunctionId(id as u32);
        function.id = id;
        self.functions.push(function);
        id
    }

    pub fn get_namespace(&self, id: ParsedNamespaceId) -> &ParsedNamespace {
        &self.namespaces[id.0 as usize]
    }

    pub fn get_namespace_mut(&mut self, id: ParsedNamespaceId) -> &mut ParsedNamespace {
        &mut self.namespaces[id.0 as usize]
    }

    pub fn add_namespace(&mut self, mut namespace: ParsedNamespace) -> ParsedNamespaceId {
        let id = ParsedNamespaceId(self.namespaces.len() as u32);
        namespace.id = id;
        self.namespaces.push(namespace);
        id
    }

    pub fn get_pattern_span(&self, id: ParsedPatternId) -> SpanId {
        match self.patterns.get_pattern(id) {
            ParsedPattern::Literal(literal_id) => self.exprs.get(*literal_id).get_span(),
            ParsedPattern::Enum(enum_pattern) => enum_pattern.span,
            ParsedPattern::Variable(_var_pattern, span) => *span,
            ParsedPattern::Struct(struct_pattern) => struct_pattern.span,
            ParsedPattern::Wildcard(span) => *span,
            ParsedPattern::Reference(r) => r.span,
        }
    }

    pub fn get_stmt_span(&self, stmt: ParsedStmtId) -> SpanId {
        match self.stmts.get(stmt) {
            ParsedStmt::Use(u) => u.span,
            ParsedStmt::ValDef(v) => v.span,
            ParsedStmt::Assignment(a) => a.span,
            ParsedStmt::SetRef(s) => s.span,
            ParsedStmt::LoneExpression(expr_id) => self.exprs.get_span(*expr_id),
        }
    }

    pub fn get_constant(&self, id: ParsedConstantId) -> &ParsedConstant {
        &self.constants[id.0 as usize]
    }

    pub fn add_constant(&mut self, mut constant: ParsedConstant) -> ParsedConstantId {
        let id = ParsedConstantId(self.constants.len() as u32);
        constant.id = id;
        self.constants.push(constant);
        id
    }

    pub fn get_ability(&self, id: ParsedAbilityId) -> &ParsedAbility {
        &self.abilities[id.0 as usize]
    }

    pub fn add_ability(&mut self, mut ability: ParsedAbility) -> ParsedAbilityId {
        let id = ParsedAbilityId(self.abilities.len() as u32);
        ability.id = id;
        self.abilities.push(ability);
        id
    }

    pub fn get_ability_impl(&self, id: ParsedAbilityImplId) -> &ParsedAbilityImplementation {
        &self.ability_impls[id.0 as usize]
    }

    pub fn add_ability_impl(
        &mut self,
        mut ability_impl: ParsedAbilityImplementation,
    ) -> ParsedAbilityImplId {
        let id = ParsedAbilityImplId(self.ability_impls.len() as u32);
        ability_impl.id = id;
        self.ability_impls.push(ability_impl);
        id
    }

    pub fn get_type_defn(&self, id: ParsedTypeDefnId) -> &ParsedTypeDefn {
        &self.type_defns[id.0 as usize]
    }

    pub fn add_typedefn(&mut self, mut type_defn: ParsedTypeDefn) -> ParsedTypeDefnId {
        let id = ParsedTypeDefnId(self.type_defns.len() as u32);
        type_defn.id = id;
        self.type_defns.push(type_defn);
        id
    }

    pub fn get_root_namespace(&self) -> &ParsedNamespace {
        &self.namespaces[0]
    }

    pub fn get_expression_type_hint(&self, id: ParsedExpressionId) -> Option<ParsedTypeExprId> {
        self.exprs.get_type_hint(id)
    }

    pub fn get_type_expression_span(&self, type_expression_id: ParsedTypeExprId) -> SpanId {
        self.type_exprs.get(type_expression_id).get_span()
    }

    pub fn get_span_for_maybe_id(&self, parsed_id: Option<ParsedId>) -> SpanId {
        match parsed_id {
            Some(parsed_id) => self.get_span_for_id(parsed_id),
            None => SpanId::NONE,
        }
    }

    pub fn get_lines_for_span_id(&self, span_id: SpanId) -> Option<(&Line, &Line)> {
        self.sources.get_lines_for_span(self.spans.get(span_id))
    }

    pub fn get_span_for_id(&self, parsed_id: ParsedId) -> SpanId {
        match parsed_id {
            ParsedId::Function(id) => self.get_function(id).span,
            ParsedId::Namespace(id) => self.get_namespace(id).span,
            ParsedId::Constant(id) => self.get_constant(id).span,
            ParsedId::Ability(id) => self.get_ability(id).span,
            ParsedId::AbilityImpl(id) => self.get_ability_impl(id).span,
            ParsedId::TypeDefn(id) => self.get_type_defn(id).span,
            ParsedId::Expression(id) => self.exprs.get_span(id),
            ParsedId::TypeExpression(id) => self.get_type_expression_span(id),
            ParsedId::Pattern(id) => self.get_pattern_span(id),
            ParsedId::Use(id) => self.uses.get_use(id).span,
        }
    }
}

pub type ParseResult<A> = anyhow::Result<A, ParseError>;

#[derive(Debug, Clone)]
pub enum ParseError {
    Parse { message: String, token: Token, cause: Option<Box<ParseError>> },
    Lex(LexError),
}

impl ParseError {
    pub fn span(&self) -> SpanId {
        match self {
            ParseError::Lex(lex_error) => lex_error.span,
            ParseError::Parse { token, .. } => token.span,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Lex(lex_error) => {
                write!(f, "LexError: {}", lex_error.message)
            }
            ParseError::Parse { message, token, cause } => {
                if let Some(cause) = &cause {
                    cause.fmt(f)?;
                    writeln!(f)?;
                }
                write!(f, "ParseError: {}, at {}", message, token.kind)
            }
        }
    }
}
impl std::error::Error for ParseError {}

fn error(message: impl AsRef<str>, token: Token) -> ParseError {
    ParseError::Parse { message: message.as_ref().to_string(), token, cause: None }
}

fn error_expected(expected: impl AsRef<str>, token: Token) -> ParseError {
    ParseError::Parse { message: format!("Expected {}", expected.as_ref()), token, cause: None }
}

fn error_cause(message: impl AsRef<str>, token: Token, cause: ParseError) -> ParseError {
    ParseError::Parse { message: message.as_ref().to_owned(), token, cause: Some(Box::new(cause)) }
}

pub fn get_span_source_line<'sources>(
    spans: &Spans,
    sources: &'sources Sources,
    span_id: SpanId,
) -> &'sources Line {
    let span = spans.get(span_id);
    let source = sources.source_by_span(span);
    let Some(line) = source.get_line_for_span_start(span) else {
        panic!("Error: could not find line for span {:?}", span)
    };
    line
}

pub fn print_error(module: &ParsedModule, parse_error: &ParseError) {
    let mut stderr = std::io::stderr();

    match parse_error {
        ParseError::Lex(lex_error) => {
            write_error_location(
                &mut stderr,
                &module.spans,
                &module.sources,
                lex_error.span,
                ErrorLevel::Error,
            )
            .unwrap();
            eprintln!("{}", lex_error.message);
        }
        ParseError::Parse { message, token, cause } => {
            let span = parse_error.span();

            if let Some(cause) = cause {
                print_error(module, cause);
            }
            let got_str = if token.kind == K::Ident {
                let span = module.spans.get(token.span);
                let source = module.sources.source_by_span(span);
                Parser::tok_chars(&module.spans, source, *token).to_string()
            } else {
                token.kind.to_string()
            };

            eprintln!("{message} at '{}'\n", got_str);
            write_error_location(
                &mut stderr,
                &module.spans,
                &module.sources,
                span,
                ErrorLevel::Error,
            )
            .unwrap();
            eprintln!();
        }
    }
}

pub fn write_error_location(
    w: &mut impl std::io::Write,
    spans: &Spans,
    sources: &Sources,
    span_id: SpanId,
    level: ErrorLevel,
) -> std::io::Result<()> {
    let span = spans.get(span_id);
    let source = sources.source_by_span(span);
    let Some(line) = source.get_line_for_span_start(span) else {
        writeln!(w, "Critical Error: could not find line for span {:?}", span)?;
        return Ok(());
    };
    use colored::*;

    // If the span is longer than the line, just highlight the whole line
    let highlight_length =
        if span.len as usize > line.content.len() { line.content.len() } else { span.len as usize };
    let thingies = "^".repeat(highlight_length);
    let spaces = " ".repeat((span.start - line.start_char) as usize);
    let code = format!("  ->{}\n  ->{spaces}{thingies}", &line.content);
    writeln!(
        w,
        "  {} at {}/{}:{}\n\n{code}",
        match level {
            ErrorLevel::Error => "Error".red(),
            ErrorLevel::Warn => "Warning".yellow(),
            ErrorLevel::Info => "Info".yellow(),
            ErrorLevel::Hint => "Hint".yellow(),
        },
        source.directory,
        source.filename,
        line.line_index + 1,
    )
}

#[derive(Debug, Clone)]
pub struct Line {
    pub start_char: u32,
    pub line_index: u32,
    pub content: String,
}

impl Line {
    pub fn line_number(&self) -> u32 {
        self.line_index + 1
    }
    pub fn end_char(&self) -> u32 {
        self.start_char + self.content.len() as u32
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    pub file_id: FileId,
    pub directory: String,
    pub filename: String,
    pub content: String,
    /// This is an inefficient copy but we need the lines cached because utf8
    /// Eventually it can be references not copies
    pub lines: Vec<Line>,
}

impl Source {
    pub fn make(file_id: FileId, directory: String, filename: String, content: String) -> Source {
        let mut lines = Vec::with_capacity(128);
        let mut iter = content.chars().enumerate().peekable();
        let mut buf: String = String::with_capacity(content.len());
        // We compute lines ourselves because we need to know the start offset of each line
        // in chars
        while let Some((c_index, c)) = iter.next() {
            let mut push_line = || {
                let start: u32 = (c_index - buf.len()) as u32;
                lines.push(Line {
                    start_char: start,
                    line_index: lines.len() as u32,
                    content: buf.clone(),
                });
                buf.clear();
            };
            if c == '\n' {
                push_line();
            } else if c == '\r' && iter.peek().is_some_and(|(_, c)| *c == '\n') {
                // Skip over the \n
                iter.next();
                push_line();
            } else {
                buf.push(c);
            }
        }
        Source { file_id, directory, filename, content, lines }
    }

    pub fn get_content(&self, start: u32, len: u32) -> &str {
        &self.content[start as usize..(start + len) as usize]
    }

    pub fn get_span_content(&self, span: Span) -> &str {
        self.get_content(span.start, span.len)
    }

    pub fn get_line(&self, line_index: usize) -> Option<&Line> {
        self.lines.get(line_index)
    }

    pub fn get_line_for_offset(&self, offset: u32) -> Option<&Line> {
        self.lines.iter().find(|line| (line.start_char..=line.end_char()).contains(&offset))
    }

    pub fn get_line_for_span_start(&self, span: Span) -> Option<&Line> {
        self.lines.iter().find(|line| {
            let line_end = line.end_char();
            line.start_char <= span.start && line_end >= span.start
        })
    }
}

pub struct Parser<'toks, 'module> {
    tokens: TokenIter<'toks>,
    file_id: FileId,
    pub module: &'module mut ParsedModule,
}

impl<'toks, 'module> Parser<'toks, 'module> {
    pub fn make(
        tokens: &'toks [Token],
        file_id: FileId,
        module: &'module mut ParsedModule,
    ) -> Parser<'toks, 'module> {
        let parser = Parser { tokens: TokenIter::make(tokens), file_id, module };
        parser
    }

    fn source(&self) -> &Source {
        self.module.sources.get_source(self.file_id)
    }

    fn expect<A>(what: &str, current: Token, value: ParseResult<Option<A>>) -> ParseResult<A> {
        match value {
            Ok(None) => Err(ParseError::Parse {
                message: format!("Expected {what}"),
                token: current,
                cause: None,
            }),
            Ok(Some(a)) => Ok(a),
            Err(e) => Err(e),
        }
    }

    fn extend_token_span(&mut self, tok1: Token, tok2: Token) -> SpanId {
        self.extend_span(tok1.span, tok2.span)
    }

    fn extend_to_here(&mut self, span: SpanId) -> SpanId {
        self.extend_span(span, self.peek_back().span)
    }

    fn extend_span(&mut self, span1: SpanId, span2: SpanId) -> SpanId {
        self.module.spans.extend(span1, span2)
    }

    fn extend_span_maybe(&mut self, span1: SpanId, span2: Option<SpanId>) -> SpanId {
        match span2 {
            None => span1,
            Some(span2) => self.extend_span(span1, span2),
        }
    }

    fn expect_parse_pattern(&mut self) -> ParseResult<ParsedPatternId> {
        let mut pattern_id: ParsedPatternId = self.expect_pattern_base()?;
        // Loop for postfix operations
        loop {
            if let Some(asterisk) = self.maybe_consume_next(K::Asterisk) {
                let inner_span = self.module.get_pattern_span(pattern_id);
                let span = self.extend_span(inner_span, asterisk.span);
                pattern_id = self.module.patterns.add_pattern(ParsedPattern::Reference(
                    ParsedReferencePattern { inner: pattern_id, span },
                ))
            } else {
                break;
            }
        }
        Ok(pattern_id)
    }

    fn expect_pattern_base(&mut self) -> ParseResult<ParsedPatternId> {
        let first = self.peek();
        if let Some(literal_id) = self.parse_literal()? {
            let pattern = ParsedPattern::Literal(literal_id);
            let id = self.module.patterns.add_pattern(pattern);
            Ok(id)
        } else if first.kind == K::OpenBrace {
            // Struct
            let open_brace = self.tokens.next();
            let mut fields = Vec::new();
            while self.peek().kind != K::CloseBrace {
                let ident_token = self.expect_eat_token(K::Ident)?;
                let ident = self.intern_ident_token(ident_token);
                let maybe_colon = self.peek();
                let pattern_id = if maybe_colon.kind == K::Colon {
                    self.advance();
                    self.expect_parse_pattern()?
                } else {
                    // Assume variable binding pattern with same name as field
                    let pattern = ParsedPattern::Variable(ident, ident_token.span);
                    self.module.patterns.add_pattern(pattern)
                };
                fields.push((ident, pattern_id));
                let next = self.peek();
                if next.kind == K::Comma {
                    self.advance();
                } else if next.kind != K::CloseBrace {
                    return Err(error_expected("comma or close brace", next));
                }
            }
            let end = self.expect_eat_token(K::CloseBrace)?;
            let span = self.extend_token_span(open_brace, end);
            let pattern = ParsedStructPattern { fields, span };
            let pattern_id = self.module.patterns.add_pattern(ParsedPattern::Struct(pattern));
            Ok(pattern_id)
        } else if first.kind == K::Dot {
            let dot = self.tokens.next();
            let ident_token = self.expect_eat_token(K::Ident)?;
            let ident = self.intern_ident_token(ident_token);
            let (payload_pattern, span) = if self.peek().kind == K::OpenParen {
                self.advance();
                let payload_pattern_id = self.expect_parse_pattern()?;
                let close_paren = self.expect_eat_token(K::CloseParen)?;
                (Some(payload_pattern_id), self.module.spans.extend(dot.span, close_paren.span))
            } else {
                (None, ident_token.span)
            };
            let pattern_id =
                self.module.patterns.add_pattern(ParsedPattern::Enum(ParsedEnumPattern {
                    variant_tag: ident,
                    payload_pattern,
                    span,
                }));
            Ok(pattern_id)
        } else if first.kind == K::Ident {
            // Variable
            let ident_token = self.expect_eat_token(K::Ident)?;
            let ident = self.intern_ident_token(ident_token);
            if self.module.identifiers.get_name(ident) == "_" {
                let pattern_id =
                    self.module.patterns.add_pattern(ParsedPattern::Wildcard(ident_token.span));
                Ok(pattern_id)
            } else {
                let pattern_id = self
                    .module
                    .patterns
                    .add_pattern(ParsedPattern::Variable(ident, ident_token.span));
                Ok(pattern_id)
            }
        } else {
            Err(error_expected("pattern expression", self.peek()))
        }
    }
}

impl<'toks, 'module> Parser<'toks, 'module> {
    #[inline]
    fn advance(&mut self) {
        self.tokens.advance()
    }

    #[inline]
    fn peek(&self) -> Token {
        self.tokens.peek()
    }

    #[inline]
    fn cursor_position(&self) -> usize {
        self.tokens.cursor_position()
    }

    #[inline]
    fn peek_two(&self) -> (Token, Token) {
        self.tokens.peek_two()
    }

    #[inline]
    fn peek_three(&self) -> (Token, Token, Token) {
        self.tokens.peek_three()
    }

    #[inline]
    fn peek_back(&self) -> Token {
        self.tokens.peek_back()
    }

    fn chars_at_span<'source>(
        spans: &Spans,
        source: &'source Source,
        span_id: SpanId,
    ) -> &'source str {
        let span = spans.get(span_id);
        source.get_span_content(span)
    }

    fn tok_chars<'source>(spans: &Spans, source: &'source Source, tok: Token) -> &'source str {
        let s = Parser::chars_at_span(spans, source, tok.span);
        trace!("{}.chars='{}'", tok.kind, s);
        s
    }

    fn token_chars(&self, tok: Token) -> &str {
        Parser::tok_chars(&self.module.spans, self.source(), tok)
    }

    fn maybe_consume_next(&mut self, target_token: TokenKind) -> Option<Token> {
        let tok = self.peek();
        if tok.kind == target_token {
            self.advance();
            trace!("eat_token SUCCESS '{}'", target_token);
            Some(tok)
        } else {
            trace!("eat_token MISS '{}'", target_token);
            None
        }
    }

    fn maybe_consume_next_no_whitespace(&mut self, target_token: TokenKind) -> Option<Token> {
        let tok = self.peek();
        if tok.kind == target_token && !tok.is_whitespace_preceeded() {
            self.advance();
            trace!("eat_token SUCCESS '{}'", target_token);
            Some(tok)
        } else {
            trace!("eat_token MISS '{}'", target_token);
            None
        }
    }

    fn expect_eat_token(&mut self, target_token: TokenKind) -> ParseResult<Token> {
        let result = self.maybe_consume_next(target_token);
        match result {
            None => {
                let actual = self.peek();
                Err(error_expected(target_token, actual))
            }
            Some(t) => Ok(t),
        }
    }

    fn intern_ident_token(&mut self, token: Token) -> Identifier {
        let tok_chars = Parser::tok_chars(
            &self.module.spans,
            self.module.sources.get_source(self.file_id),
            token,
        );
        self.module.identifiers.intern(tok_chars)
    }

    pub fn add_expression(&mut self, expression: ParsedExpression) -> ParsedExpressionId {
        self.module.exprs.add_expression(expression)
    }

    pub fn add_expression_with_directives(
        &mut self,
        expression: ParsedExpression,
        directives: Vec<ParsedDirective>,
    ) -> ParsedExpressionId {
        let id = self.module.exprs.add_expression(expression);
        self.module.exprs.add_directives(id, directives);
        id
    }

    pub fn get_expression(&self, id: ParsedExpressionId) -> &ParsedExpression {
        self.module.exprs.get(id)
    }

    pub fn get_expression_span(&self, id: ParsedExpressionId) -> SpanId {
        self.module.exprs.get(id).get_span()
    }

    pub fn get_type_expression_span(&self, id: ParsedTypeExprId) -> SpanId {
        self.module.type_exprs.get(id).get_span()
    }

    fn parse_literal(&mut self) -> ParseResult<Option<ParsedExpressionId>> {
        let (first, second) = self.tokens.peek_two();
        trace!("parse_literal {} {}", first.kind, second.kind);
        match (first.kind, second.kind) {
            (K::KeywordBuiltin, _) => {
                self.advance();
                Ok(Some(self.add_expression(ParsedExpression::Builtin(first.span))))
            }
            (K::OpenParen, K::CloseParen) => {
                trace!("parse_literal unit");
                self.advance();
                self.advance();
                let span = self.extend_token_span(first, second);
                Ok(Some(self.add_expression(ParsedExpression::Literal(Literal::Unit(span)))))
            }
            (K::Char, _) => {
                trace!("parse_literal char");
                self.advance();
                let text = self.token_chars(first);
                assert!(text.starts_with('\''));
                assert!(text.ends_with('\''));
                let bytes = text.as_bytes();
                if bytes[1] == b'\\' {
                    debug_assert_eq!(bytes.len(), 4);
                    let esc_char = bytes[2];
                    let literal =
                        match CHAR_ESCAPED_CHARS.iter().find(|c| c.sentinel == esc_char as char) {
                            Some(c) => Ok(Literal::Char(c.output, first.span)),
                            None => Err(error(
                                format!(
                                    "Invalid escaped char following escape sequence: {}",
                                    char::from(esc_char)
                                ),
                                first,
                            )),
                        }?;
                    Ok(Some(self.add_expression(ParsedExpression::Literal(literal))))
                } else {
                    debug_assert_eq!(bytes.len(), 3);
                    let byte = bytes[1];
                    Ok(Some(self.add_expression(ParsedExpression::Literal(Literal::Char(
                        byte, first.span,
                    )))))
                }
            }
            (K::String, _) => {
                trace!("parse_literal string");
                self.advance();
                // Accessing the tok_chars this way achieves a partial borrow of self
                let text = Parser::tok_chars(
                    &self.module.spans,
                    self.module.sources.get_source(self.file_id),
                    first,
                );
                let mut buf = String::with_capacity(text.len());
                let mut chars = text.chars();
                enum Mode {
                    Base,
                    InterpIdent(String),
                }
                let mut mode = Mode::Base;
                let mut parts: Vec<InterpolatedStringPart> = Vec::new();
                while let Some(c) = chars.next() {
                    match &mut mode {
                        Mode::InterpIdent(ident_str) => {
                            if crate::lex::is_ident_char(c) {
                                ident_str.push(c)
                            } else if c == '}' {
                                let buf = std::mem::take(ident_str);
                                let ident = self.module.identifiers.intern(buf);
                                parts.push(InterpolatedStringPart::Identifier(ident));
                                mode = Mode::Base;
                            } else {
                                return Err(error(
                                    format!("Unexpected character inside interpolated identifier: {c}, expected more or '}}'"),
                                    first,
                                ));
                            }
                        }
                        Mode::Base => {
                            if c == '\\' {
                                let Some(next) = chars.next() else {
                                    return Err(error("String ended with '\\'", first));
                                };
                                if next == '{' {
                                    let b = std::mem::take(&mut buf);
                                    parts.push(InterpolatedStringPart::String(b));
                                    mode = Mode::InterpIdent(String::with_capacity(16));
                                } else if let Some(c) =
                                    STRING_ESCAPED_CHARS.iter().find(|c| c.sentinel == next)
                                {
                                    buf.push(c.output as char)
                                } else {
                                    return Err(error(
                                        format!("Invalid escape sequence: '\\{next}'"),
                                        first,
                                    ));
                                };
                            } else {
                                buf.push(c)
                            }
                        }
                    }
                }
                match mode {
                    Mode::Base => {
                        if !buf.is_empty() {
                            parts.push(InterpolatedStringPart::String(buf))
                        }
                    }
                    Mode::InterpIdent(s) => {
                        return Err(error(
                            format!("Unterminated interpolated identifier: {s}"),
                            first,
                        ));
                    }
                }
                if parts.len() == 1 {
                    let InterpolatedStringPart::String(s) = parts.into_iter().next().unwrap()
                    else {
                        panic!()
                    };
                    let literal = Literal::String(s, first.span);
                    Ok(Some(self.add_expression(ParsedExpression::Literal(literal))))
                } else {
                    let string_interp = ParsedInterpolatedString { parts, span: first.span };
                    Ok(Some(
                        self.add_expression(ParsedExpression::InterpolatedString(string_interp)),
                    ))
                }
            }
            (K::Minus, K::Ident) if !second.is_whitespace_preceeded() => {
                let text = self.token_chars(second);
                if text.chars().next().unwrap().is_numeric() {
                    let mut s = "-".to_string();
                    s.push_str(text);
                    self.advance();
                    self.advance();
                    let span = self.extend_token_span(first, second);
                    let numeric = Literal::Numeric(ParsedNumericLiteral { text: s, span });
                    Ok(Some(self.add_expression(ParsedExpression::Literal(numeric))))
                } else {
                    Err(error_expected("number following '-'", second))
                }
            }
            (K::Ident, _) => {
                let text = self.token_chars(first);
                if text == "true" {
                    self.advance();
                    Ok(Some(self.add_expression(ParsedExpression::Literal(Literal::Bool(
                        true, first.span,
                    )))))
                } else if text == "false" {
                    self.advance();
                    Ok(Some(self.add_expression(ParsedExpression::Literal(Literal::Bool(
                        false, first.span,
                    )))))
                } else {
                    match text.chars().next() {
                        Some(c) if c.is_numeric() => {
                            let s = text.to_string();
                            self.advance();
                            Ok(Some(self.add_expression(ParsedExpression::Literal(
                                Literal::Numeric(ParsedNumericLiteral {
                                    text: s,
                                    span: first.span,
                                }),
                            ))))
                        }
                        _ => Ok(None),
                    }
                }
            }
            _ => Ok(None),
        }
    }

    fn parse_struct_type_field(&mut self) -> ParseResult<Option<StructTypeField>> {
        let peeked = self.peek();
        let private = if peeked.kind == K::Ident && self.get_token_chars(self.peek()) == "private" {
            self.advance();
            true
        } else {
            false
        };
        let name_token = self.expect_eat_token(K::Ident)?;
        let ident_id = self.intern_ident_token(name_token);
        self.expect_eat_token(K::Colon)?;
        let typ_expr =
            Parser::expect("Type expression", self.peek(), self.parse_type_expression())?;
        Ok(Some(StructTypeField { name: ident_id, ty: typ_expr, private }))
    }

    fn expect_type_expression(&mut self) -> ParseResult<ParsedTypeExprId> {
        Parser::expect("type_expression", self.peek(), self.parse_type_expression())
    }

    fn parse_type_expression(&mut self) -> ParseResult<Option<ParsedTypeExprId>> {
        let Some(mut result) = self.parse_base_type_expression()? else {
            return Ok(None);
        };
        loop {
            let next = self.peek();
            if next.kind.is_postfix_type_operator() {
                if next.kind == K::Dot {
                    self.advance();
                    let ident_token = self.expect_eat_token(K::Ident)?;
                    let ident = self.intern_ident_token(ident_token);
                    let span = self.extend_span(
                        self.module.get_type_expression_span(result),
                        ident_token.span,
                    );
                    let new = ParsedTypeExpr::DotMemberAccess(ParsedDotMemberAccess {
                        base: result,
                        member_name: ident,
                        span,
                    });
                    let new_id = self.module.type_exprs.add(new);
                    result = new_id;
                } else if next.kind == K::QuestionMark {
                    // Optional Type
                    self.advance();
                    result = self.module.type_exprs.add(ParsedTypeExpr::Optional(ParsedOptional {
                        base: result,
                        span: next.span,
                    }));
                } else if next.kind == K::Asterisk {
                    // Reference Type
                    self.advance();
                    result =
                        self.module.type_exprs.add(ParsedTypeExpr::Reference(ParsedReference {
                            base: result,
                            span: next.span,
                        }));
                } else {
                    panic!("unhandled postfix type operator {:?}", next.kind);
                }
            } else {
                break;
            }
        }
        Ok(Some(result))
    }

    fn get_token_chars(&self, token: Token) -> &str {
        Parser::tok_chars(&self.module.spans, self.source(), token)
    }

    fn parse_base_type_expression(&mut self) -> ParseResult<Option<ParsedTypeExprId>> {
        let first = self.peek();
        if first.kind == K::OpenParen {
            self.advance();
            let expr = self.expect_type_expression()?;
            // Note: Here would be where we would support tuples (if we did paren tuples)
            self.expect_eat_token(K::CloseParen)?;
            Ok(Some(expr))
        } else if first.kind == K::KeywordEither {
            let enumm = self.expect_enum_type_expression()?;
            let type_expr_id = self.module.type_exprs.add(ParsedTypeExpr::Enum(enumm));
            Ok(Some(type_expr_id))
        } else if first.kind == K::BackSlash {
            let fun = self.expect_function_type()?;
            Ok(Some(fun))
        } else if first.kind == K::KeywordBuiltin {
            self.advance();
            let builtin_id = self.module.type_exprs.add(ParsedTypeExpr::Builtin(first.span));
            Ok(Some(builtin_id))
        } else if first.kind == K::Ident {
            let ident_chars = self.get_token_chars(first);
            if ident_chars == "typeOf" {
                self.advance();
                self.expect_eat_token(K::OpenParen)?;
                let target_expr = self.expect_expression()?;
                let end = self.expect_eat_token(K::CloseParen)?;
                let span = self.extend_token_span(first, end);
                let type_of = ParsedTypeExpr::TypeOf(ParsedTypeOf { target_expr, span });
                Ok(Some(self.module.type_exprs.add(type_of)))
            } else if ident_chars == "some" {
                // nocommit: Disallow the type name 'some'
                // nocommit: Disallow 'some' type exprs in other places
                self.advance();
                let inner_expr = self.expect_type_expression()?;
                let span = self.extend_to_here(first.span);
                let quantifier =
                    ParsedTypeExpr::SomeQuant(SomeQuantifier { inner: inner_expr, span });
                Ok(Some(self.module.type_exprs.add(quantifier)))
            } else {
                let base_name = self.expect_namespaced_ident()?;
                // parameterized, namespaced type. Examples:
                // int,
                // Box[Point],
                // std::Map[int, int]
                let (type_params, type_params_span) = self.parse_optional_type_args()?;
                let span = self.extend_span_maybe(first.span, type_params_span);
                Ok(Some(self.module.type_exprs.add(ParsedTypeExpr::TypeApplication(
                    TypeApplication { name: base_name, args: type_params, span },
                ))))
            }
        } else if first.kind == K::OpenBrace {
            let open_brace = self.expect_eat_token(K::OpenBrace)?;
            let (fields, fields_span) =
                self.eat_delimited("Struct fields", K::Comma, &[K::CloseBrace], |p| {
                    let field_res = Parser::parse_struct_type_field(p);
                    Parser::expect("Struct field", open_brace, field_res)
                })?;
            let span = self.extend_span(first.span, fields_span);
            let struc = StructType { fields, span };
            Ok(Some(self.module.type_exprs.add(ParsedTypeExpr::Struct(struc))))
        } else {
            Ok(None)
        }
    }

    fn expect_function_type(&mut self) -> ParseResult<ParsedTypeExprId> {
        let (params, params_span) = self.eat_delimited_expect_opener(
            "Function parameters",
            K::BackSlash,
            K::Comma,
            &[K::RThinArrow],
            Parser::expect_type_expression,
        )?;
        let return_type = self.expect_type_expression()?;
        let span = self.extend_span(params_span, self.get_type_expression_span(return_type));
        let function_type = ParsedFunctionType { params, return_type, span };
        Ok(self.module.type_exprs.add(ParsedTypeExpr::Function(function_type)))
    }

    fn expect_enum_type_expression(&mut self) -> ParseResult<ParsedEnumType> {
        let keyword = self.expect_eat_token(K::KeywordEither)?;
        let mut variants = Vec::new();
        let mut first = true;
        loop {
            // Expect comma
            if !first {
                if self.peek().kind == K::Comma {
                    self.advance();
                } else {
                    break;
                }
            }

            let tag = self.peek();
            if tag.kind != K::Ident {
                return Err(error_expected("Identifier for enum variant", tag));
            }
            let tag_name = self.intern_ident_token(tag);
            self.advance();
            let maybe_payload_paren = self.peek();
            let payload_expression = if maybe_payload_paren.kind == K::OpenParen {
                self.advance();
                let payload_expr = self.expect_type_expression()?;
                let _close_paren = self.expect_eat_token(K::CloseParen)?;
                Some(payload_expr)
            } else {
                None
            };
            let span = match payload_expression {
                None => tag.span,
                Some(expr) => self.module.type_exprs.get(expr).get_span(),
            };
            variants.push(ParsedEnumVariant { tag_name, payload_expression, span });
            first = false;
        }
        let last_variant_span =
            variants.last().ok_or_else(|| error_expected("At least one variant", keyword))?.span;
        let span = self.extend_span(keyword.span, last_variant_span);
        Ok(ParsedEnumType { variants, span })
    }

    fn expect_fn_arg(&mut self) -> ParseResult<FnCallArg> {
        let (first, second) = self.tokens.peek_two();
        let named = if first.kind == K::Ident && second.kind == K::Equals {
            self.advance();
            self.advance();
            true
        } else {
            false
        };
        let expr = self.expect_expression()?;
        let name = if named { Some(self.intern_ident_token(first)) } else { None };
        Ok(FnCallArg { name, value: expr })
    }

    fn expect_struct_field(&mut self) -> ParseResult<StructValueField> {
        let name = self.expect_eat_token(K::Ident)?;
        let expr = if let Some(_colon) = self.maybe_consume_next(K::Colon) {
            Some(self.expect_expression()?)
        } else {
            None
        };
        let span =
            self.extend_span_maybe(name.span, expr.map(|expr| self.get_expression_span(expr)));
        Ok(StructValueField { name: self.intern_ident_token(name), expr, span })
    }

    fn parse_struct_value(&mut self) -> ParseResult<Option<Struct>> {
        let (first, second, third) = self.peek_three();
        let is_empty_struct = first.kind == K::OpenBrace && second.kind == K::CloseBrace;
        let is_non_empty_struct = first.kind == K::OpenBrace
            && second.kind == K::Ident
            // Covers single-field shorthand struct and regular structs
            && (third.kind == K::Comma || third.kind == K::CloseBrace || third.kind == K::Colon);
        let is_struct = is_empty_struct || is_non_empty_struct;
        if !is_struct {
            return Ok(None);
        };

        self.advance();

        let (fields, span) =
            self.eat_delimited("Struct", K::Comma, &[K::CloseBrace], Parser::expect_struct_field)?;
        Ok(Some(Struct { fields, span }))
    }

    fn parse_expression_with_postfix_ops(&mut self) -> ParseResult<Option<ParsedExpressionId>> {
        let Some(mut result) = self.parse_base_expression()? else { return Ok(None) };
        // Looping for postfix ops inspired by Jakt's parser
        let with_postfix: ParsedExpressionId = loop {
            let (next, second) = self.peek_two();
            let new_result = if next.kind == K::KeywordAs {
                self.advance();
                let type_expr_id = self.expect_type_expression()?;
                let span = self.extend_span(
                    self.get_expression_span(result),
                    self.module.get_type_expression_span(type_expr_id),
                );
                Some(self.add_expression(ParsedExpression::AsCast(ParsedAsCast {
                    base_expr: result,
                    dest_type: type_expr_id,
                    span,
                })))
            } else if next.kind == K::KeywordIs {
                self.advance();
                let pattern = self.expect_parse_pattern()?;

                let original_span = self.get_expression_span(result);
                let pattern_span = self.module.get_pattern_span(pattern);
                let span = self.extend_span(original_span, pattern_span);
                let is_expression_id =
                    self.add_expression(ParsedExpression::Is(ParsedIsExpression {
                        target_expression: result,
                        pattern,
                        span,
                    }));
                Some(is_expression_id)
            } else if (next.kind == K::Dot && !next.is_whitespace_preceeded())
                || (next.kind == K::QuestionMark
                    && (second.kind == K::Dot && !second.is_whitespace_preceeded()))
            {
                let is_coalescing = next.kind == K::QuestionMark;
                // Field access syntax; a.b with optional bracketed type args []
                self.advance();
                if is_coalescing {
                    self.advance();
                }
                let target = match self.peek().kind {
                    K::Ident => self.tokens.next(),
                    K::Ampersand => self.tokens.next(),
                    K::Asterisk => self.tokens.next(),
                    K::Bang => self.tokens.next(),
                    _k => {
                        return Err(error_expected(
                            "Field name, or postfix &, *, or !",
                            self.peek(),
                        ))
                    }
                };
                let (type_args, type_args_span) = self.parse_optional_type_args()?;
                let next = self.peek();
                // a.b[int](...)
                if next.kind == K::OpenParen {
                    let (context_args, args, args_span) = self.expect_fn_call_args()?;
                    let self_arg = result;
                    let span = self.extend_span(self.get_expression_span(self_arg), args_span);
                    let name = self.intern_ident_token(target);
                    let mut all_args = vec![FnCallArg { name: None, value: self_arg }];
                    all_args.extend(args);
                    Some(self.add_expression(ParsedExpression::FnCall(FnCall {
                        name: NamespacedIdentifier::naked(name, target.span),
                        type_args,
                        args: all_args,
                        explicit_context_args: context_args,
                        span,
                        is_method: true,
                        id: ParsedExpressionId::PENDING,
                    })))
                } else {
                    // a.b[int] <complete expression>
                    let span = self.extend_span(
                        self.get_expression_span(result),
                        type_args_span.unwrap_or(target.span),
                    );
                    let trailing_asterisk = self.maybe_consume_next_no_whitespace(K::Asterisk);
                    let target = self.intern_ident_token(target);
                    Some(self.add_expression(ParsedExpression::FieldAccess(FieldAccess {
                        base: result,
                        field_name: target,
                        type_args,
                        is_coalescing,
                        is_referencing: trailing_asterisk.is_some(),
                        span,
                    })))
                }
            } else {
                None
            };
            if let Some(new_result) = new_result {
                result = new_result;
            } else {
                break result;
            }
        };
        if self.peek().kind == K::Colon {
            self.advance();
            let type_hint = self.expect_type_expression()?;
            self.module.exprs.set_type_hint(with_postfix, type_hint);
        }
        Ok(Some(with_postfix))
    }

    fn expect_block(&mut self) -> ParseResult<Block> {
        Parser::expect("block", self.peek(), self.parse_block())
    }

    fn expect_expression(&mut self) -> ParseResult<ParsedExpressionId> {
        Parser::expect("expression", self.peek(), self.parse_expression())
    }

    fn parse_expression(&mut self) -> ParseResult<Option<ParsedExpressionId>> {
        let Some(mut expr) = self.parse_expression_with_postfix_ops()? else {
            return Ok(None);
        };
        if self.peek().kind.is_binary_operator() {
            let mut expr_stack: Vec<ExprStackMember> = vec![ExprStackMember::Expr(expr)];
            let mut last_precedence = 100_000;
            loop {
                let tok = self.peek();
                let Some(op_kind) = BinaryOpKind::from_tokenkind(tok.kind) else {
                    break;
                };
                let precedence = op_kind.precedence();
                self.advance();
                let rhs = Parser::expect(
                    "rhs of binary op",
                    self.peek(),
                    self.parse_expression_with_postfix_ops(),
                )?;
                while precedence <= last_precedence && expr_stack.len() > 1 {
                    trace!(
                        "expr_stack at {:?}, precedence={}, last={}, stacklen={}",
                        op_kind,
                        precedence,
                        last_precedence,
                        expr_stack.len()
                    );
                    let rhs = expr_stack.pop().unwrap().expect_expr();
                    let (op_kind, op_span) = expr_stack.pop().unwrap().expect_operator();
                    last_precedence = op_kind.precedence();
                    if last_precedence < precedence {
                        expr_stack.push(ExprStackMember::Operator(op_kind, op_span));
                        expr_stack.push(ExprStackMember::Expr(rhs));
                        break;
                    }
                    let ExprStackMember::Expr(lhs) = expr_stack.pop().unwrap() else {
                        panic!("expected expr on stack")
                    };
                    let span = self.extend_expr_span(lhs, rhs);
                    let bin_op = self.add_expression(ParsedExpression::BinaryOp(BinaryOp {
                        op_kind,
                        lhs,
                        rhs,
                        span,
                    }));
                    expr_stack.push(ExprStackMember::Expr(bin_op))
                }
                expr_stack.push(ExprStackMember::Operator(op_kind, tok.span));
                expr_stack.push(ExprStackMember::Expr(rhs));

                last_precedence = precedence;
            }

            // Pop and build now that everything is right
            while expr_stack.len() > 1 {
                let ExprStackMember::Expr(rhs) = expr_stack.pop().unwrap() else {
                    panic!("expected expr")
                };
                let ExprStackMember::Operator(op_kind, _) = expr_stack.pop().unwrap() else {
                    panic!("expected operator")
                };
                let ExprStackMember::Expr(lhs) = expr_stack.pop().unwrap() else {
                    panic!("expected expr")
                };
                let new_span = self.extend_expr_span(lhs, rhs);
                let bin_op = self.add_expression(ParsedExpression::BinaryOp(BinaryOp {
                    op_kind,
                    lhs,
                    rhs,
                    span: new_span,
                }));
                expr_stack.push(ExprStackMember::Expr(bin_op));
            }
            let with_correct_binops = expr_stack.pop().unwrap().expect_expr();
            expr = with_correct_binops;
        };
        Ok(Some(expr))
    }

    fn extend_expr_span(&mut self, expr1: ParsedExpressionId, expr2: ParsedExpressionId) -> SpanId {
        self.extend_span(self.get_expression_span(expr1), self.get_expression_span(expr2))
    }

    fn parse_optional_type_args(&mut self) -> ParseResult<(Vec<NamedTypeArg>, Option<SpanId>)> {
        // TODO(perf): we spend a lot of time here in flamegraph
        let Some((type_expressions, type_args_span)) = self.eat_delimited_if_opener(
            "Type arguments",
            K::OpenBracket,
            K::Comma,
            &[K::CloseBracket],
            Parser::expect_type_expression,
        )?
        else {
            return Ok((vec![], None));
        };
        let type_args: Vec<_> = type_expressions
            .into_iter()
            .map(|type_expr| NamedTypeArg { name: None, type_expr })
            .collect();
        Ok((type_args, Some(type_args_span)))
    }

    fn expect_namespaced_ident(&mut self) -> ParseResult<NamespacedIdentifier> {
        let (first, second) = self.tokens.peek_two();
        let mut namespaces = Vec::new();
        if second.kind == K::Slash && !second.is_whitespace_preceeded() {
            // Namespaced expression; foo/
            // Loop until we don't see a /
            namespaces.push(self.intern_ident_token(first));
            self.advance(); // ident
            self.advance(); // coloncolon
            loop {
                trace!("Parsing namespaces {:?}", namespaces);
                let (a, b) = self.tokens.peek_two();
                trace!("Parsing namespaces peeked 3 {} {}", a.kind, b.kind);
                if a.kind == K::Ident && b.kind == K::Slash {
                    self.advance(); // ident
                    self.advance(); // slash
                    namespaces.push(self.intern_ident_token(a));
                } else {
                    break;
                }
            }
        }
        let name = self.expect_eat_token(K::Ident)?;
        let name_ident = self.intern_ident_token(name);
        let span = self.extend_span(first.span, name.span);
        Ok(NamespacedIdentifier { namespaces, name: name_ident, span })
    }

    /// "Base" in "base expression" simply means ignoring postfix and
    /// binary operations, in terms of recursion or induction, its an atom,
    /// or a 'base case'
    fn parse_base_expression(&mut self) -> ParseResult<Option<ParsedExpressionId>> {
        let directives = self.parse_directives()?;
        let (first, second, third) = self.tokens.peek_three();
        trace!("parse_base_expression {} {} {}", first.kind, second.kind, third.kind);
        let resulting_expression = if first.kind == K::OpenParen {
            self.advance();
            if self.peek().kind == K::CloseParen {
                let end = self.tokens.next();
                let span = self.extend_token_span(first, end);
                Ok(Some(self.add_expression(ParsedExpression::Literal(Literal::Unit(span)))))
            } else {
                // Note: Here would be where we would support tuples
                let expr = self.expect_expression()?;
                self.expect_eat_token(K::CloseParen)?;
                Ok(Some(expr))
            }
        } else if first.kind == K::BackSlash {
            Ok(Some(self.expect_lambda()?))
        } else if first.kind == K::KeywordWhile {
            let while_result = Parser::expect("while loop", first, self.parse_while_loop())?;
            Ok(Some(self.add_expression(ParsedExpression::While(while_result))))
        } else if first.kind == K::KeywordLoop {
            self.advance();
            let body = self.expect_block()?;
            let span = self.extend_span(first.span, body.span);
            Ok(Some(self.add_expression(ParsedExpression::Loop(ParsedLoopExpr { body, span }))))
        } else if first.kind == K::KeywordSwitch {
            let when_keyword = self.tokens.next();
            let target_expression = self.expect_expression()?;

            self.expect_eat_token(K::OpenBrace)?;

            // Allow an opening comma for symmetry
            if self.peek().kind == K::Comma {
                self.advance();
            }
            let mut cases = Vec::new();
            while self.peek().kind != K::CloseBrace {
                let mut arm_pattern_ids = smallvec::smallvec![];
                loop {
                    let arm_pattern_id = self.expect_parse_pattern()?;
                    arm_pattern_ids.push(arm_pattern_id);
                    if self.maybe_consume_next(K::KeywordOr).is_none() {
                        break;
                    }
                }

                let guard_condition_expr = if self.maybe_consume_next(K::KeywordIf).is_some() {
                    Some(self.expect_expression()?)
                } else {
                    None
                };

                self.expect_eat_token(K::RThinArrow)?;

                let arm_expr_id = self.expect_expression()?;
                cases.push(ParsedMatchCase {
                    patterns: arm_pattern_ids,
                    guard_condition_expr,
                    expression: arm_expr_id,
                });
                let next = self.peek();
                if next.kind == K::Comma {
                    self.advance();
                } else if next.kind != K::CloseBrace {
                    return Err(error_expected("comma or close brace", next));
                }
            }
            let close = self.expect_eat_token(K::CloseBrace)?;
            let span = self.extend_token_span(when_keyword, close);
            let match_expr =
                ParsedMatchExpression { match_subject: target_expression, cases, span };
            Ok(Some(self.add_expression(ParsedExpression::Match(match_expr))))
        } else if first.kind == K::KeywordFor {
            self.advance();
            let binding = if third.kind == K::KeywordIn {
                if second.kind != K::Ident {
                    return Err(error("Expected identifiers between for and in keywords", second));
                }
                let binding_ident = self.intern_ident_token(second);
                self.advance();
                self.advance();
                Some(binding_ident)
            } else {
                None
            };
            let iterable_expr = self.expect_expression()?;
            let expr_type_keyword = self.tokens.peek();
            let for_expr_type = if expr_type_keyword.kind == K::KeywordYield {
                Ok(ForExprType::Yield)
            } else if expr_type_keyword.kind == K::KeywordDo {
                Ok(ForExprType::Do)
            } else {
                Err(error("Expected yield or do keyword", expr_type_keyword))
            }?;
            self.advance();
            let body_expr = self.expect_block()?;
            let span = self.extend_span(first.span, body_expr.span);
            Ok(Some(self.add_expression(ParsedExpression::For(ForExpr {
                iterable_expr,
                binding,
                body_block: body_expr,
                expr_type: for_expr_type,
                span,
            }))))
        } else if first.kind.is_prefix_operator() {
            let Some(op_kind) = ParsedUnaryOpKind::from_tokenkind(first.kind) else {
                return Err(error("unexpected prefix operator", first));
            };
            self.advance();
            let expr = self.expect_expression()?;
            let span = self.extend_span(first.span, self.get_expression_span(expr));
            Ok(Some(self.add_expression(ParsedExpression::UnaryOp(UnaryOp {
                expr,
                op_kind,
                span,
            }))))
        } else if first.kind == K::Dot && second.kind == K::Ident {
            // <dot> <ident> for example .Red
            self.advance();
            self.advance();

            if self.token_chars(second).chars().next().unwrap().is_lowercase() {
                return Err(error("Variant names must be uppercase", second));
            }
            let variant_name = self.intern_ident_token(second);

            if third.kind == K::OpenParen {
                // Enum Constructor
                self.advance();
                let payload = self.expect_expression()?;
                let close_paren = self.expect_eat_token(K::CloseParen)?;
                let span = self.extend_token_span(first, close_paren);
                Ok(Some(self.add_expression(ParsedExpression::AnonEnumConstructor(
                    AnonEnumConstructor { variant_name, payload: Some(payload), span },
                ))))
            } else {
                // Tag Literal
                let span = self.extend_token_span(first, second);
                Ok(Some(self.add_expression(ParsedExpression::AnonEnumConstructor(
                    AnonEnumConstructor { variant_name, payload: None, span },
                ))))
            }
        } else if let Some(literal_id) = self.parse_literal()? {
            Ok(Some(literal_id))
        } else if first.kind == K::Ident {
            let namespaced_ident = self.expect_namespaced_ident()?;
            let second = self.tokens.peek();
            // FnCall
            if second.kind == K::OpenBracket || second.kind == K::OpenParen {
                trace!("parse_expression FnCall");
                let (type_args, _type_args_span) = self.parse_optional_type_args()?;
                let (context_args, args, args_span) = self.expect_fn_call_args()?;
                let span = self.extend_span(namespaced_ident.span, args_span);
                Ok(Some(self.add_expression(ParsedExpression::FnCall(FnCall {
                    name: namespaced_ident,
                    type_args,
                    args,
                    explicit_context_args: context_args,
                    span,
                    is_method: false,
                    id: ParsedExpressionId::PENDING,
                }))))
            } else {
                // The last thing it can be is a simple variable reference expression
                Ok(Some(self.add_expression(ParsedExpression::Variable(Variable {
                    name: namespaced_ident,
                }))))
            }
        } else if first.kind == K::OpenBrace {
            // The syntax {} means empty struct, not empty block
            // If you want an block, use a unit block { () }
            trace!("parse_expr {:?} {:?} {:?}", first, second, third);
            if let Some(struct_value) = self.parse_struct_value()? {
                Ok(Some(self.add_expression(ParsedExpression::Struct(struct_value))))
            } else {
                match self.parse_block()? {
                    None => Err(error_expected("block", self.peek())),
                    Some(block) => Ok(Some(self.add_expression(ParsedExpression::Block(block)))),
                }
            }
        } else if first.kind == K::KeywordIf {
            let if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
            Ok(Some(self.add_expression(ParsedExpression::If(if_expr))))
        } else if first.kind == K::OpenBracket {
            // list
            let start = self.expect_eat_token(K::OpenBracket)?;
            let (elements, elements_span) = self.eat_delimited(
                "list elements",
                TokenKind::Comma,
                &[TokenKind::CloseBracket],
                |p| Parser::expect("expression", start, p.parse_expression()),
            )?;
            let span = self.extend_span(start.span, elements_span);
            Ok(Some(
                self.add_expression(ParsedExpression::ListLiteral(ListExpr { elements, span })),
            ))
        } else {
            // More expression types
            if directives.is_empty() {
                Ok(None)
            } else {
                Err(error_expected("expression following directives", first))
            }
        }?;
        if let Some(expression_id) = resulting_expression {
            self.module.exprs.add_directives(expression_id, directives);
        }
        Ok(resulting_expression)
    }

    fn expect_lambda_arg_defn(&mut self) -> ParseResult<LambdaArgDefn> {
        let name = self.expect_eat_token(K::Ident)?;
        let binding = self.intern_ident_token(name);
        let ty = if self.peek().kind == K::Colon {
            self.advance();
            Some(self.expect_type_expression()?)
        } else {
            None
        };
        Ok(LambdaArgDefn { ty, binding, span: name.span })
    }

    fn expect_lambda(&mut self) -> ParseResult<ParsedExpressionId> {
        let start = self.expect_eat_token(K::BackSlash)?;
        let _start = self.expect_eat_token(K::OpenParen)?;
        let mut arguments: Vec<LambdaArgDefn> = Vec::with_capacity(8);
        let (_args_span, closing_delimeter) = self.eat_delimited_ext(
            "Lambda args",
            &mut arguments,
            K::Comma,
            &[K::RThinArrow, K::CloseParen],
            Parser::expect_lambda_arg_defn,
        )?;
        let return_type = if closing_delimeter.kind == K::RThinArrow {
            let return_type_expr = self.expect_type_expression()?;
            self.expect_eat_token(K::CloseParen)?;
            Some(return_type_expr)
        } else {
            None
        };

        let body = self.expect_expression()?;
        let span = self.extend_span(start.span, self.get_expression_span(body));
        let lambda = ParsedLambda { arguments, return_type, body, span };
        Ok(self.add_expression(ParsedExpression::Lambda(lambda)))
    }

    fn expect_fn_call_args(&mut self) -> ParseResult<(Vec<FnCallArg>, Vec<FnCallArg>, SpanId)> {
        let (first, second) = self.tokens.peek_two();
        let is_context = second.kind == K::KeywordContext;

        let (context_args, _) = if is_context {
            self.expect_eat_token(K::OpenParen)?;
            self.expect_eat_token(K::KeywordContext)?;
            self.eat_delimited(
                "Function context arguments",
                K::Comma,
                &[K::CloseParen],
                Parser::expect_fn_arg,
            )?
        } else {
            (vec![], self.peek().span)
        };
        let (args, args_span) = self.eat_delimited_expect_opener(
            "Function arguments",
            K::OpenParen,
            K::Comma,
            &[K::CloseParen],
            Parser::expect_fn_arg,
        )?;
        let span = self.extend_span(first.span, args_span);
        Ok((context_args, args, span))
    }

    fn parse_val_def(&mut self) -> ParseResult<Option<ValDef>> {
        trace!("parse_val_def");

        let eaten_keyword = match self.peek() {
            t if t.kind == K::KeywordLet => {
                self.advance();
                t
            }
            _ => {
                return Ok(None);
            }
        };
        let is_reference = self.maybe_consume_next_no_whitespace(K::Asterisk).is_some();
        let is_context = self.maybe_consume_next(K::KeywordContext).is_some();
        let is_mutable = self.maybe_consume_next(K::KeywordMut).is_some();
        let name_token = self.expect_eat_token(K::Ident)?;
        let typ = match self.maybe_consume_next(K::Colon) {
            None => Ok(None),
            Some(_) => self.parse_type_expression(),
        }?;
        self.expect_eat_token(K::Equals)?;
        let initializer_expression =
            Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span =
            self.extend_span(eaten_keyword.span, self.get_expression_span(initializer_expression));
        Ok(Some(ValDef {
            name: self.intern_ident_token(name_token),
            type_expr: typ,
            value: initializer_expression,
            flags: ValDef::make_flags(is_mutable, is_context, is_reference),
            span,
        }))
    }

    fn parse_const(&mut self) -> ParseResult<Option<ParsedConstantId>> {
        trace!("parse_const");
        let Some(keyword_let_token) = self.maybe_consume_next(K::KeywordLet) else {
            return Ok(None);
        };
        let name_token = self.expect_eat_token(K::Ident)?;
        let _colon = self.expect_eat_token(K::Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(K::Equals)?;
        let value_expr = Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span = self.extend_span(keyword_let_token.span, self.get_expression_span(value_expr));
        let name = self.intern_ident_token(name_token);
        let constant_id = self.module.add_constant(ParsedConstant {
            name,
            ty: typ,
            value_expr,
            span,
            id: ParsedConstantId(0),
        });
        Ok(Some(constant_id))
    }

    fn expect_assignment(&mut self, lhs: ParsedExpressionId) -> ParseResult<Assignment> {
        self.expect_eat_token(K::Equals)?;
        let rhs = self.expect_expression()?;
        let span = self.extend_expr_span(lhs, rhs);
        Ok(Assignment { lhs, rhs, span })
    }

    fn expect_set_stmt(&mut self, lhs: ParsedExpressionId) -> ParseResult<SetStmt> {
        self.expect_eat_token(K::LThinArrow)?;
        let rhs = self.expect_expression()?;
        let span = self.extend_expr_span(lhs, rhs);
        Ok(SetStmt { lhs, rhs, span })
    }

    fn eat_fn_arg_def(&mut self, is_context: bool) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let name_token = self.expect_eat_token(K::Ident)?;
        self.expect_eat_token(K::Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        let span = self.extend_span(name_token.span, self.module.type_exprs.get(typ).get_span());
        let modifiers = FnArgDefModifiers::new(is_context);
        Ok(FnArgDef { name: self.intern_ident_token(name_token), ty: typ, span, modifiers })
    }

    // Returns: context_args, args, span
    fn eat_fndef_args(&mut self) -> ParseResult<(Vec<FnArgDef>, Vec<FnArgDef>, SpanId)> {
        let (first, next) = self.tokens.peek_two();
        let is_context = next.kind == K::KeywordContext;
        let (context_args, _context_args_span) = if is_context {
            self.expect_eat_token(K::OpenParen)?;
            self.expect_eat_token(K::KeywordContext)?;
            self.eat_delimited("Function context arguments", K::Comma, &[K::CloseParen], |p| {
                Parser::eat_fn_arg_def(p, true)
            })?
        } else {
            (vec![], self.peek().span)
        };
        let (args, fn_args_span) = self.eat_delimited_expect_opener(
            "Function arguments",
            K::OpenParen,
            K::Comma,
            &[K::CloseParen],
            |p| Parser::eat_fn_arg_def(p, false),
        )?;
        let span = self.extend_span(first.span, fn_args_span);
        Ok((context_args, args, span))
    }

    fn eat_delimited_if_opener<T, F>(
        &mut self,
        name: &str,
        opener: TokenKind,
        delim: TokenKind,
        terminators: &[TokenKind],
        parse: F,
    ) -> ParseResult<Option<(Vec<T>, SpanId)>>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        let next = self.peek();
        if next.kind == opener {
            self.advance();
            let (result, result_span) = self.eat_delimited(name, delim, terminators, parse)?;
            let span = self.extend_span(next.span, result_span);
            Ok(Some((result, span)))
        } else {
            Ok(None)
        }
    }

    #[allow(unused)]
    fn eat_delimited_expect_opener<T, F>(
        &mut self,
        name: &str,
        opener: TokenKind,
        delim: TokenKind,
        terminators: &'static [TokenKind],
        parse: F,
    ) -> ParseResult<(Vec<T>, SpanId)>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        match self.eat_delimited_if_opener(name, opener, delim, terminators, parse) {
            Ok(None) => Err(error(opener, self.peek())),
            Ok(Some(res)) => Ok(res),
            Err(err) => Err(err),
        }
    }

    fn eat_delimited<T, F>(
        &mut self,
        name: &str,
        delim: TokenKind,
        terminators: &[TokenKind],
        parse: F,
    ) -> ParseResult<(Vec<T>, SpanId)>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        let mut destination = Vec::with_capacity(8);
        let (span, _terminator) =
            self.eat_delimited_ext(name, &mut destination, delim, terminators, parse)?;
        Ok((destination, span))
    }

    fn eat_delimited_ext<T, F>(
        &mut self,
        name: &str,
        destination: &mut Vec<T>,
        delim: TokenKind,
        terminators: &[TokenKind],
        parse: F,
    ) -> ParseResult<(SpanId, Token)>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        trace!("eat_delimited delim='{}'", delim);

        let start_span = self.peek().span;

        loop {
            let terminator = self.peek();
            if terminators.contains(&terminator.kind) {
                self.advance();
                trace!("eat_delimited found terminator after {} results.", destination.len());
                let span = self.module.spans.extend(start_span, terminator.span);
                break Ok((span, terminator));
            }
            match parse(self) {
                Ok(parsed) => {
                    destination.push(parsed);
                    trace!("eat_delimited got result {}", destination.len());
                    let terminator = self.peek();
                    if terminators.contains(&terminator.kind) {
                        self.advance();
                        trace!(
                            "eat_delimited found terminator after {} results.",
                            destination.len()
                        );
                        let span = self.module.spans.extend(start_span, terminator.span);
                        break Ok((span, terminator));
                    }
                    let found_delim = self.maybe_consume_next(delim);
                    if found_delim.is_none() {
                        trace!("eat_delimited missing delimiter.");
                        break Err(error_expected(delim, self.peek()));
                    }
                }
                Err(e) => {
                    // trace!("eat_delimited got err from 'parse': {}", e);
                    break Err(error_cause(
                        format!(
                            "Failed to parse {} separated by '{delim}' and terminated by: {}",
                            name,
                            terminators
                                .iter()
                                .map(|kind| format!("{}", *kind))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                        self.peek(),
                        e,
                    ));
                }
            }
        }
    }

    fn parse_if_expr(&mut self) -> ParseResult<Option<IfExpr>> {
        let Some(if_keyword) = self.maybe_consume_next(TokenKind::KeywordIf) else {
            return Ok(None);
        };
        let condition_expr =
            Parser::expect("conditional expression", if_keyword, self.parse_expression())?;
        let consequent_expr =
            Parser::expect("expression following condition", if_keyword, self.parse_expression())?;
        let else_peek = self.peek();
        let alt = if else_peek.kind == K::KeywordElse {
            self.advance();
            let alt_result = Parser::expect("else expression", else_peek, self.parse_expression())?;
            Some(alt_result)
        } else {
            None
        };
        let end_span = alt
            .as_ref()
            .map(|a| self.get_expression_span(*a))
            .unwrap_or(self.get_expression_span(consequent_expr));
        let span = self.extend_span(if_keyword.span, end_span);
        let if_expr = IfExpr { cond: condition_expr, cons: consequent_expr, alt, span };
        Ok(Some(if_expr))
    }

    fn parse_while_loop(&mut self) -> ParseResult<Option<ParsedWhileExpr>> {
        let while_token = self.peek();
        if while_token.kind != K::KeywordWhile {
            return Ok(None);
        }
        self.advance();
        let cond = self.expect_expression()?;
        let body =
            Parser::expect("body expr for while loop", while_token, self.parse_expression())?;
        let span = self.extend_span(while_token.span, self.get_expression_span(body));
        Ok(Some(ParsedWhileExpr { cond, body, span }))
    }

    fn parse_statement(&mut self) -> ParseResult<Option<ParsedStmtId>> {
        trace!("eat_statement {:?}", self.peek());
        if let Some(use_id) = self.parse_use()? {
            let span = self.module.uses.get_use(use_id).span;
            Ok(Some(self.module.stmts.add(ParsedStmt::Use(UseStmt { span, use_id }))))
        } else if let Some(val_def) = self.parse_val_def()? {
            Ok(Some(self.module.stmts.add(ParsedStmt::ValDef(val_def))))
        } else if let Some(expr) = self.parse_expression()? {
            let peeked = self.peek();
            // Assignment:
            // - Validate expr type, since only some exprs can be LHS of an assignment
            // - Build assignment
            if peeked.kind == K::Equals {
                let assgn = self.expect_assignment(expr)?;
                Ok(Some(self.module.stmts.add(ParsedStmt::Assignment(assgn))))
            } else if peeked.kind == K::LThinArrow {
                let set = self.expect_set_stmt(expr)?;
                Ok(Some(self.module.stmts.add(ParsedStmt::SetRef(set))))
            } else {
                Ok(Some(self.module.stmts.add(ParsedStmt::LoneExpression(expr))))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_block(&mut self) -> ParseResult<Option<Block>> {
        let Some(block_start) = self.maybe_consume_next(K::OpenBrace) else {
            return Ok(None);
        };
        let parse_statement =
            |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
        let (block_statements, statements_span) = self.eat_delimited(
            "Block statements",
            K::Semicolon,
            &[K::CloseBrace],
            parse_statement,
        )?;
        let span = self.extend_span(block_start.span, statements_span);
        Ok(Some(Block { stmts: block_statements, span }))
    }

    fn expect_type_param(&mut self) -> ParseResult<ParsedTypeParam> {
        // fn foo[T: Into[bool] and Into[Baz], U: Into[int]]
        //        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //           ^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^
        //           constraints
        let name_token = self.expect_eat_token(K::Ident)?;
        let name = self.intern_ident_token(name_token);
        let mut constraints = vec![];
        if self.maybe_consume_next(K::Colon).is_some() {
            loop {
                constraints.push(self.expect_type_constraint_expr()?);
                if self.peek().kind != K::KeywordAnd {
                    break;
                } else {
                    self.advance()
                }
            }
        }
        let span = self.extend_to_here(name_token.span);
        Ok(ParsedTypeParam { name, span, constraints })
    }

    /// Directives look like this: #<directive kind: ident>(<directive arg>, ...)
    fn parse_directives(&mut self) -> ParseResult<Vec<ParsedDirective>> {
        let mut directives: Vec<ParsedDirective> = vec![];
        while let Some(_hash) = self.maybe_consume_next(K::Hash) {
            let directive = if let Some(keyword_if) = self.maybe_consume_next(K::KeywordIf) {
                let condition = self.expect_expression()?;
                let span = self.extend_span(keyword_if.span, self.get_expression_span(condition));
                ParsedDirective::ConditionalCompile { condition, span }
            } else {
                let kind_token = self.expect_eat_token(K::Ident)?;
                match self.token_chars(kind_token) {
                    "debug" => ParsedDirective::CompilerDebug { span: kind_token.span },
                    s => return Err(error(format!("Invalid directive kind: '{s}'"), kind_token)),
                }
            };
            directives.push(directive)
        }
        Ok(directives)
    }

    fn parse_function(&mut self) -> ParseResult<Option<ParsedFunctionId>> {
        trace!("parse_function");
        let directives = self.parse_directives()?;
        let initial_pos = self.cursor_position();
        let is_intrinsic = if self.peek().kind == K::KeywordIntern {
            self.advance();
            true
        } else {
            false
        };
        let linkage = if is_intrinsic {
            Linkage::Intrinsic
        } else if self.peek().kind == K::KeywordExtern {
            self.advance();
            let external_name = if self.peek().kind == K::OpenParen {
                self.advance();
                // Parse the external name
                let external_name = self.expect_eat_token(K::Ident)?;
                self.expect_eat_token(K::CloseParen)?;
                Some(self.intern_ident_token(external_name))
            } else {
                None
            };
            Linkage::External(external_name)
        } else {
            Linkage::Standard
        };

        let fn_keyword = match self.expect_eat_token(K::KeywordFn) {
            Ok(f) => f,
            Err(e) => {
                return if self.cursor_position() != initial_pos { Err(e) } else { Ok(None) };
            }
        };
        let func_name = self.expect_eat_token(K::Ident)?;
        let func_name_id = self.intern_ident_token(func_name);
        let type_arguments: Vec<ParsedTypeParam> =
            if self.maybe_consume_next(K::OpenBracket).is_some() {
                let (type_args, _type_arg_span) = self.eat_delimited(
                    "Type arguments",
                    TokenKind::Comma,
                    &[TokenKind::CloseBracket],
                    |p| p.expect_type_param(),
                )?;
                type_args
            } else {
                vec![]
            };
        let (context_args, args, args_span) = self.eat_fndef_args()?;
        self.expect_eat_token(K::Colon)?;
        let ret_type = self.expect_type_expression()?;
        let additional_type_constraints = if self.maybe_consume_next(K::KeywordWhere).is_some() {
            // FIXME(brittle parsing): Has to backtrack to un-consume the next token in the fn call;
            // the open brace
            let (additional_type_constraints, _span) = self.eat_delimited(
                "Type variable constraints",
                K::Comma,
                &[K::OpenBrace],
                Parser::expect_named_type_constraint,
            )?;
            self.tokens.retreat(); // Un-eat the close sentinel
            additional_type_constraints
        } else {
            vec![]
        };
        let signature_span = self.extend_to_here(func_name.span);
        let block = self.parse_block()?;
        let end_span = block.as_ref().map(|b| b.span).unwrap_or(args_span);
        let span = self.extend_span(fn_keyword.span, end_span);
        let function_id = self.module.add_function(ParsedFunction {
            name: func_name_id,
            type_params: type_arguments,
            params: args,
            context_params: context_args,
            ret_type,
            block,
            signature_span,
            span,
            linkage,
            directives,
            additional_where_constraints: additional_type_constraints,
            id: ParsedFunctionId(u32::MAX),
        });
        Ok(Some(function_id))
    }

    fn expect_type_constraint_expr(&mut self) -> ParseResult<ParsedTypeConstraintExpr> {
        let ability_expr = self.expect_ability_expr()?;
        Ok(ParsedTypeConstraintExpr::Ability(ability_expr))
    }

    fn expect_named_type_constraint(&mut self) -> ParseResult<ParsedTypeConstraint> {
        let (name_token, name) = self.expect_ident_upper()?;
        self.expect_eat_token(K::Colon)?;
        let constraint_expr = self.expect_type_constraint_expr()?;
        let span = self.extend_span(name_token.span, constraint_expr.span());
        Ok(ParsedTypeConstraint { name, constraint_expr, span })
    }

    fn expect_ident_ext(&mut self, upper: bool, lower: bool) -> ParseResult<(Token, Identifier)> {
        let token = self.expect_eat_token(K::Ident)?;
        let tok_chars = Parser::tok_chars(
            &self.module.spans,
            self.module.sources.get_source(self.file_id),
            token,
        );
        if upper && !tok_chars.chars().next().unwrap().is_uppercase() {
            return Err(error("This name must be capitalized", token));
        }
        if lower && !tok_chars.chars().next().unwrap().is_lowercase() {
            return Err(error("This name must be capitalized", token));
        }
        Ok((token, self.module.identifiers.intern(tok_chars)))
    }

    fn expect_ident_upper(&mut self) -> ParseResult<(Token, Identifier)> {
        self.expect_ident_ext(true, false)
    }

    fn parse_ability_defn(&mut self) -> ParseResult<Option<ParsedAbilityId>> {
        fn expect_ability_type_param(p: &mut Parser) -> ParseResult<ParsedAbilityParameter> {
            let start = p.peek().span;
            let is_impl_param = p.maybe_consume_next(K::KeywordImpl).is_some();
            let name_token = p.expect_eat_token(K::Ident)?;
            let name = p.intern_ident_token(name_token);
            // The parser only supports one type constraint for now
            let constraints = if p.maybe_consume_next(K::Colon).is_some() {
                let constraint = p.expect_type_constraint_expr()?;
                vec![constraint]
            } else {
                vec![]
            };
            let span = p.extend_span(start, name_token.span);
            Ok(ParsedAbilityParameter { name, is_impl_param, constraints, span })
        }
        let keyword_ability = self.maybe_consume_next(K::KeywordAbility);
        let Some(keyword_ability) = keyword_ability else {
            return Ok(None);
        };
        let name_token = self.expect_eat_token(K::Ident)?;
        let name_identifier = self.intern_ident_token(name_token);
        let ability_params = if let Some(_params_open) = self.maybe_consume_next(K::OpenBracket) {
            let (params, _) = self.eat_delimited(
                "Ability Parameter",
                K::Comma,
                &[K::CloseBracket],
                expect_ability_type_param,
            )?;
            params
        } else {
            vec![]
        };
        self.expect_eat_token(K::OpenBrace)?;
        let mut functions = Vec::new();
        while let Some(parsed_function) = self.parse_function()? {
            functions.push(parsed_function);
        }
        let close_token = self.expect_eat_token(K::CloseBrace)?;
        let span = self.extend_token_span(keyword_ability, close_token);
        let ability_id = self.module.add_ability(ParsedAbility {
            name: name_identifier,
            functions,
            params: ability_params,
            span,
            id: ParsedAbilityId(0),
        });
        Ok(Some(ability_id))
    }

    fn expect_ability_type_argument(&mut self) -> ParseResult<AbilityTypeArgument> {
        let name_token = self.expect_eat_token(K::Ident)?;
        let name = self.intern_ident_token(name_token);
        self.expect_eat_token(K::Equals)?;
        let value = self.expect_type_expression()?;
        let span = self.extend_span(name_token.span, self.get_type_expression_span(value));
        Ok(AbilityTypeArgument { name, value, span })
    }

    fn expect_ability_expr(&mut self) -> ParseResult<ParsedAbilityExpr> {
        let name = self.expect_namespaced_ident()?;
        let (arguments, span) = match self.eat_delimited_if_opener(
            "Ability Arguments",
            K::OpenBracket,
            K::Comma,
            &[K::CloseBracket],
            Parser::expect_ability_type_argument,
        )? {
            None => (vec![], name.span),
            Some((arguments, args_span)) => (arguments, self.extend_span(name.span, args_span)),
        };
        Ok(ParsedAbilityExpr { name, arguments, span })
    }

    fn parse_ability_impl(&mut self) -> ParseResult<Option<ParsedAbilityImplId>> {
        let Some(keyword_impl) = self.maybe_consume_next(K::KeywordImpl) else { return Ok(None) };
        let generic_impl_params = self
            .eat_delimited_if_opener(
                "Generic implementation parameters",
                K::OpenBracket,
                K::Comma,
                &[K::CloseBracket],
                |p| p.expect_type_param(),
            )?
            .map(|res| res.0)
            .unwrap_or_default();
        let ability = self.expect_ability_expr()?;
        self.expect_eat_token(K::KeywordFor)?;
        let target_type = self.expect_type_expression()?;

        // Read the functions inside block; one day also associated constants
        let mut functions = Vec::with_capacity(2);
        self.expect_eat_token(K::OpenBrace)?;

        while let Some(parsed_function) = self.parse_function()? {
            functions.push(parsed_function);
        }

        let close_brace = self.expect_eat_token(K::CloseBrace)?;
        let span = self.extend_token_span(keyword_impl, close_brace);

        let ability_impl_id = self.module.add_ability_impl(ParsedAbilityImplementation {
            ability_expr: ability,
            generic_impl_params,
            self_type: target_type,
            functions,
            id: ParsedAbilityImplId(u32::MAX),
            span,
        });
        Ok(Some(ability_impl_id))
    }

    fn parse_type_defn(&mut self) -> ParseResult<Option<ParsedTypeDefnId>> {
        let keyword_type = self.maybe_consume_next(K::KeywordDefType);
        let Some(keyword_type) = keyword_type else {
            return Ok(None);
        };

        // Parse modifiers
        let mut flags = ParsedTypeDefnFlags::new(false, false);
        loop {
            let name_or_modifier = self.peek();

            let text = self.get_token_chars(name_or_modifier);
            if text == "alias" {
                flags.set_alias();
                self.advance()
            } else {
                break;
            }
        }

        let name = self.expect_eat_token(K::Ident)?;

        let type_params: Vec<ParsedTypeParam> = if let TokenKind::OpenBracket = self.peek().kind {
            self.advance();
            let (type_args, _type_arg_span) =
                self.eat_delimited("Type arguments", K::Comma, &[K::CloseBracket], |p| {
                    p.expect_type_param()
                })?;
            type_args
        } else {
            Vec::new()
        };

        let equals = self.expect_eat_token(K::Equals)?;
        let type_expr = Parser::expect("Type expression", equals, self.parse_type_expression())?;
        let span =
            self.extend_span(keyword_type.span, self.module.get_type_expression_span(type_expr));
        let name = self.intern_ident_token(name);
        let type_defn_id = self.module.add_typedefn(ParsedTypeDefn {
            name,
            value_expr: type_expr,
            span,
            type_params,
            id: ParsedTypeDefnId(u32::MAX), // The id is set by add_typedefn
            flags,
        });
        Ok(Some(type_defn_id))
    }

    fn parse_namespace(&mut self) -> ParseResult<Option<ParsedNamespaceId>> {
        let keyword = self.peek();
        if keyword.kind != K::KeywordNamespace {
            return Ok(None);
        };
        self.advance();
        let ident = self.expect_eat_token(K::Ident)?;
        let is_braced = match self.tokens.next() {
            t if t.kind == K::OpenBrace => true,  // namespace asdf {
            t if t.kind == K::Semicolon => false, // namespace asdf;
            t => return Err(error_expected("{ or ;", t)),
        };
        let mut definitions = Vec::new();
        while let Some(def) = self.parse_definition()? {
            definitions.push(def);
        }
        if is_braced {
            self.expect_eat_token(K::CloseBrace)?;
        } else {
            let is_end = self.peek().kind == K::Eof;
            if !is_end {
                return Err(error_expected("end of file", self.peek()));
            }
        }
        let name = self.intern_ident_token(ident);
        let span = self.extend_to_here(keyword.span);
        let namespace_id = self.module.add_namespace(ParsedNamespace {
            name,
            definitions,
            id: ParsedNamespaceId(0),
            span,
        });
        Ok(Some(namespace_id))
    }

    fn parse_use(&mut self) -> ParseResult<Option<ParsedUseId>> {
        let Some(use_token) = self.maybe_consume_next(K::KeywordUse) else {
            return Ok(None);
        };

        let namespaced_ident = self.expect_namespaced_ident()?;
        let alias = if let Some(_as_token) = self.maybe_consume_next(K::KeywordAs) {
            let alias_token = self.expect_eat_token(K::Ident)?;
            let alias_ident = self.intern_ident_token(alias_token);
            Some(alias_ident)
        } else {
            None
        };
        let span = self.extend_to_here(use_token.span);
        let parsed_use_id =
            self.module.uses.add_use(ParsedUse { target: namespaced_ident, alias, span });
        Ok(Some(parsed_use_id))
    }

    fn parse_definition(&mut self) -> ParseResult<Option<ParsedId>> {
        if let Some(use_id) = self.parse_use()? {
            Ok(Some(ParsedId::Use(use_id)))
        } else if let Some(ns) = self.parse_namespace()? {
            Ok(Some(ParsedId::Namespace(ns)))
        } else if let Some(constant_id) = self.parse_const()? {
            self.expect_eat_token(K::Semicolon)?;
            Ok(Some(ParsedId::Constant(constant_id)))
        } else if let Some(function_id) = self.parse_function()? {
            Ok(Some(ParsedId::Function(function_id)))
        } else if let Some(type_defn_id) = self.parse_type_defn()? {
            Ok(Some(ParsedId::TypeDefn(type_defn_id)))
        } else if let Some(ability_id) = self.parse_ability_defn()? {
            Ok(Some(ParsedId::Ability(ability_id)))
        } else if let Some(ability_impl_id) = self.parse_ability_impl()? {
            Ok(Some(ParsedId::AbilityImpl(ability_impl_id)))
        } else {
            Ok(None)
        }
    }

    pub fn parse_module(&mut self) {
        let root_namespace_id = if self.module.namespaces.is_empty() {
            let name = self.module.identifiers.intern("_root");
            self.module.add_namespace(ParsedNamespace {
                name,
                definitions: Vec::new(),
                id: ParsedNamespaceId(0),
                span: self.peek().span,
            })
        } else {
            self.module.get_root_namespace().id
        };

        let mut new_definitions: Vec<ParsedId> = vec![];
        loop {
            match self.parse_definition() {
                Ok(Some(def)) => new_definitions.push(def),
                Err(err) => {
                    print_error(self.module, &err);
                    self.module.errors.push(err);
                    // For now, break on first parse error
                    break;
                }
                Ok(None) => break,
            }
        }
        if self.tokens.peek().kind != K::Eof && self.module.errors.is_empty() {
            let err = error_expected("End of file or start of new definition", self.tokens.peek());
            self.module.errors.push(err.clone());
        }

        self.module.get_namespace_mut(root_namespace_id).definitions.extend(new_definitions);
    }
}

// Display
impl ParsedModule {
    pub fn expr_id_to_string(&self, expr: ParsedExpressionId) -> String {
        let mut buffer = String::new();
        self.display_expr_id(expr, &mut buffer).unwrap();
        buffer
    }

    pub fn display_expr_id(
        &self,
        expr: ParsedExpressionId,
        f: &mut impl Write,
    ) -> std::fmt::Result {
        match self.exprs.get(expr) {
            ParsedExpression::Builtin(_span) => f.write_str("builtin"),
            ParsedExpression::BinaryOp(op) => {
                f.write_str("(")?;
                self.display_expr_id(op.lhs, f)?;
                write!(f, " {} ", op.op_kind)?;
                self.display_expr_id(op.rhs, f)?;
                f.write_str(")")
            }
            ParsedExpression::UnaryOp(op) => {
                write!(f, "{}", op.op_kind)?;
                self.display_expr_id(op.expr, f)
            }
            ParsedExpression::Literal(lit) => f.write_fmt(format_args!("{}", lit)),
            ParsedExpression::InterpolatedString(is) => {
                f.write_char('"')?;
                for part in &is.parts {
                    match part {
                        InterpolatedStringPart::String(s) => f.write_str(s)?,
                        InterpolatedStringPart::Identifier(ident) => {
                            f.write_char('{')?;
                            f.write_str(self.identifiers.get_name(*ident))?;
                            f.write_char('{')?;
                        }
                    }
                }
                f.write_char('"')?;
                Ok(())
            }
            ParsedExpression::FnCall(call) => {
                f.write_str(self.identifiers.get_name(call.name.name))?;
                f.write_str("(...)")?;
                Ok(())
            }
            ParsedExpression::Variable(var) => {
                f.write_str("var#")?;
                f.write_str(self.identifiers.get_name(var.name.name))?;
                Ok(())
            }
            ParsedExpression::FieldAccess(acc) => {
                self.display_expr_id(acc.base, f)?;
                f.write_str(".")?;
                f.write_str(self.identifiers.get_name(acc.field_name))?;
                Ok(())
            }
            ParsedExpression::Block(block) => f.write_fmt(format_args!("{:?}", block)),
            ParsedExpression::If(if_expr) => f.write_fmt(format_args!("{:?}", if_expr)),
            ParsedExpression::While(while_expr) => {
                f.write_str("while ")?;
                self.display_expr_id(while_expr.cond, f)?;
                self.display_expr_id(while_expr.body, f)?;
                Ok(())
            }
            ParsedExpression::Loop(loop_expr) => {
                f.write_str("loop ")?;
                write!(f, "{:?}", loop_expr.body)?;
                Ok(())
            }
            ParsedExpression::Struct(struc) => f.write_fmt(format_args!("{:?}", struc)),
            ParsedExpression::ListLiteral(list_expr) => {
                f.write_fmt(format_args!("{:?}", list_expr))
            }
            ParsedExpression::For(for_expr) => f.write_fmt(format_args!("{:?}", for_expr)),
            ParsedExpression::AnonEnumConstructor(anon_enum) => {
                f.write_char('.')?;
                f.write_str(self.identifiers.get_name(anon_enum.variant_name))?;
                if let Some(payload) = anon_enum.payload.as_ref() {
                    f.write_str("(")?;
                    self.display_expr_id(*payload, f)?;
                    f.write_str(")")?;
                }
                Ok(())
            }
            ParsedExpression::Is(is_expr) => {
                self.display_expr_id(is_expr.target_expression, f)?;
                f.write_str(" is ")?;
                self.display_pattern_expression_id(is_expr.pattern, f)
            }
            ParsedExpression::Match(match_expr) => {
                f.write_str("switch ")?;
                self.display_expr_id(match_expr.match_subject, f)?;
                f.write_str(" {")?;
                for ParsedMatchCase { patterns, guard_condition_expr, expression } in
                    match_expr.cases.iter()
                {
                    f.write_str("")?;
                    for (pattern_index, pattern_id) in patterns.iter().enumerate() {
                        self.display_pattern_expression_id(*pattern_id, f)?;
                        let is_last = pattern_index == patterns.len() - 1;
                        if !is_last {
                            f.write_str(" or ")?;
                        }
                    }
                    if let Some(guard_condition_expr) = guard_condition_expr {
                        f.write_str(" if ")?;
                        self.display_expr_id(*guard_condition_expr, f)?;
                    }
                    f.write_str(" -> ")?;
                    self.display_expr_id(*expression, f)?;
                    f.write_str(",\n")?;
                }
                f.write_str(" }")
            }
            ParsedExpression::AsCast(cast) => {
                self.display_expr_id(cast.base_expr, f)?;
                f.write_str(" as ")?;
                self.display_type_expr_id(cast.dest_type, f)
            }
            ParsedExpression::Lambda(lambda) => {
                f.write_char('\\')?;
                for (index, arg) in lambda.arguments.iter().enumerate() {
                    f.write_str(self.identifiers.get_name(arg.binding))?;
                    if let Some(ty) = arg.ty {
                        f.write_str(": ")?;
                        self.display_type_expr_id(ty, f)?;
                    }
                    let last = index == lambda.arguments.len() - 1;
                    if !last {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(" -> ")?;
                self.display_expr_id(lambda.body, f)?;
                Ok(())
            }
        }
    }

    pub fn display_pattern_expression_id(
        &self,
        _pattern_expr_id: ParsedPatternId,
        f: &mut impl Write,
    ) -> std::fmt::Result {
        f.write_str("<pattern expr todo>")
    }

    pub fn type_expr_to_string(&self, type_expr_id: ParsedTypeExprId) -> String {
        let mut buffer = String::new();
        self.display_type_expr_id(type_expr_id, &mut buffer).unwrap();
        buffer
    }

    pub fn display_type_expr_id(
        &self,
        ty_expr_id: ParsedTypeExprId,
        f: &mut impl Write,
    ) -> std::fmt::Result {
        match self.type_exprs.get(ty_expr_id) {
            ParsedTypeExpr::Struct(struct_type) => {
                f.write_str("{ ")?;
                for field in struct_type.fields.iter() {
                    f.write_str(self.identifiers.get_name(field.name))?;
                    f.write_str(": ")?;
                    self.display_type_expr_id(ty_expr_id, f)?;
                    f.write_str(", ")?;
                }
                f.write_str(" }")
            }
            ParsedTypeExpr::TypeApplication(tapp) => {
                display_namespaced_identifier(f, &self.identifiers, &tapp.name, "::")?;
                if !tapp.args.is_empty() {
                    f.write_str("[")?;
                    for tparam in tapp.args.iter() {
                        self.display_type_expr_id(tparam.type_expr, f)?;
                        f.write_str(", ")?;
                    }
                    f.write_str("]")?;
                }
                Ok(())
            }
            ParsedTypeExpr::Optional(opt) => {
                self.display_type_expr_id(opt.base, f)?;
                f.write_str("?")
            }
            ParsedTypeExpr::Reference(refer) => {
                self.display_type_expr_id(refer.base, f)?;
                f.write_str("*")
            }
            ParsedTypeExpr::Enum(e) => {
                f.write_str("enum ")?;
                for variant in &e.variants {
                    f.write_str(self.identifiers.get_name(variant.tag_name))?;
                    if let Some(payload) = &variant.payload_expression {
                        f.write_str("(")?;
                        self.display_type_expr_id(*payload, f)?;
                        f.write_str(")")?;
                    }
                }
                Ok(())
            }
            ParsedTypeExpr::DotMemberAccess(acc) => {
                self.display_type_expr_id(acc.base, f)?;
                f.write_char('.')?;
                f.write_str(self.identifiers.get_name(acc.member_name))
            }
            ParsedTypeExpr::Builtin(_builtin) => f.write_str("builtin"),
            ParsedTypeExpr::Function(fun) => {
                f.write_char('\\')?;
                for (index, t) in fun.params.iter().enumerate() {
                    self.display_type_expr_id(*t, f)?;
                    let last = index == fun.params.len() - 1;
                    if !last {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(" -> ")?;
                self.display_type_expr_id(fun.return_type, f)?;
                Ok(())
            }
            ParsedTypeExpr::TypeOf(tof) => {
                f.write_str("typeOf(")?;
                self.display_expr_id(tof.target_expr, f)?;
                f.write_str(")")?;
                Ok(())
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                f.write_str("some ")?;
                self.display_type_expr_id(quant.inner, f)?;
                Ok(())
            }
        }
    }
}

pub fn display_namespaced_identifier(
    writ: &mut impl Write,
    idents: &Identifiers,
    ns_ident: &NamespacedIdentifier,
    delim: &'static str,
) -> std::fmt::Result {
    for ident in ns_ident.namespaces.iter() {
        writ.write_str(idents.get_name(*ident))?;
        writ.write_str(delim)?;
    }
    writ.write_str(idents.get_name(ns_ident.name))?;
    Ok(())
}

pub fn lex_text(module: &mut ParsedModule, source: Source) -> ParseResult<Vec<Token>> {
    let file_id = source.file_id;
    module.sources.insert(source);
    let text = &module.sources.get_source(file_id).content;
    let mut lexer = Lexer::make(text, &mut module.spans, file_id);
    let tokens = lexer.run().map_err(ParseError::Lex)?;

    let token_vec: Vec<Token> =
        tokens.into_iter().filter(|token| token.kind != K::LineComment).collect();
    Ok(token_vec)
}

#[cfg(test)]
pub fn test_parse_module(source: Source) -> ParseResult<ParsedModule> {
    let module_name = source.filename.split('.').next().unwrap().to_string();
    let mut module = ParsedModule::make(
        module_name,
        CompilerConfig {
            is_test_build: false,
            no_std: true,
            target: crate::compiler::detect_host_target().unwrap(),
        },
    );

    let file_id = source.file_id;
    let token_vec = lex_text(&mut module, source)?;
    let mut parser = Parser::make(&token_vec, file_id, &mut module);

    parser.parse_module();
    if let Some(e) = module.errors.first() {
        print_error(&module, e);
        Err(e.clone())
    } else {
        Ok(module)
    }
}

impl Identifiers {
    pub const BUILTIN_IDENTS: [&'static str; 36] = [
        "main",
        "self",
        "it",
        "unit",
        "char",
        "string",
        "length",
        "hasValue",
        "get",
        "not",
        "iter",
        "iteree",
        "it_index",
        "as",
        "list_lit",
        "withCapacity",
        "yieldedColl",
        "iteree_length",
        "block_expr_val",
        "optelse_lhs",
        "list_literal",
        "SourceLocation",
        "__lambda_env",
        "fn_ptr",
        "env_ptr",
        "&",
        "*",
        "!",
        "sb",
        "payload",
        "try",
        "try_value",
        "if_target",
        "crash",
        "toRef",
        "toDyn",
    ];
}
