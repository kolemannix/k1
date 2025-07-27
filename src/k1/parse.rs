use std::fmt::{Display, Formatter, Write};
use std::num::NonZeroU32;

use crate::compiler::CompilerConfig;
use crate::pool::{Pool, SliceHandle};
use crate::typer::{BinaryOpKind, ErrorLevel, Linkage};
use crate::{SV4, SV8, impl_copy_if_small, lex::*, nz_u32_id, static_assert_size};
use TokenKind as K;
use ecow::{EcoVec, eco_vec};
use fxhash::FxHashMap;
use log::trace;
use smallvec::{SmallVec, smallvec};
use string_interner::Symbol;
use string_interner::backend::StringBackend;

trait CanPush<T> {
    fn push_it(&mut self, value: T);
}

impl<T> CanPush<T> for Vec<T> {
    fn push_it(&mut self, value: T) {
        self.push(value)
    }
}

impl<T, const N: usize> CanPush<T> for SmallVec<[T; N]>
where
    [T; N]: smallvec::Array<Item = T>,
{
    fn push_it(&mut self, value: T) {
        self.push(value)
    }
}

impl<T: Clone> CanPush<T> for EcoVec<T> {
    fn push_it(&mut self, value: T) {
        self.push(value)
    }
}

nz_u32_id!(NamedTypeArgId);
nz_u32_id!(CallArgId);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedTypeDefnId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedFunctionId(u32);
nz_u32_id!(ParsedGlobalId);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedAbilityId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedAbilityImplId(u32);

nz_u32_id!(ParsedNamespaceId);
nz_u32_id!(ParsedExprId);
nz_u32_id!(ParsedStmtId);
nz_u32_id!(ParsedTypeExprId);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedPatternId(u32);
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct ParsedUseId(u32);

#[derive(Debug, Clone)]
pub enum ParsedDirective {
    ConditionalCompile { condition: ParsedExprId, span: SpanId },
    CompilerDebug { span: SpanId },
}

#[derive(Debug, Clone)]
pub struct ParsedUse {
    pub target: NamespacedIdentifier,
    pub alias: Option<Ident>,
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
    Global(ParsedGlobalId),
    Expression(ParsedExprId),
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
            ParsedId::Global(id) => write!(f, "global#{}", id.0),
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

    pub fn as_global_id(&self) -> Option<ParsedGlobalId> {
        match self {
            ParsedId::Global(global_id) => Some(*global_id),
            _ => None,
        }
    }

    pub fn as_namespace_id(&self) -> Option<ParsedNamespaceId> {
        match self {
            ParsedId::Namespace(ns_id) => Some(*ns_id),
            _ => None,
        }
    }
}

impl From<ParsedTypeExprId> for ParsedId {
    fn from(id: ParsedTypeExprId) -> Self {
        ParsedId::TypeExpression(id)
    }
}

impl From<ParsedExprId> for ParsedId {
    fn from(id: ParsedExprId) -> Self {
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
            ParsedId::Global(_) => true,
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
    pub elements: EcoVec<ParsedExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedNumericLiteral {
    pub text: String,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedLiteral {
    Unit(SpanId),
    Char(u8, SpanId),
    Numeric(ParsedNumericLiteral),
    Bool(bool, SpanId),
    String(StringId, SpanId),
}

impl Display for ParsedLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedLiteral::Unit(_) => f.write_str("()"),
            ParsedLiteral::Char(byte, _) => {
                f.write_char('\'')?;
                f.write_char(*byte as char)?;
                f.write_char('\'')
            }
            ParsedLiteral::Numeric(i) => f.write_str(&i.text),
            ParsedLiteral::Bool(true, _) => f.write_str("true"),
            ParsedLiteral::Bool(false, _) => f.write_str("false"),
            ParsedLiteral::String(s, _) => {
                f.write_char('"')?;
                write!(f, "{}", s).unwrap();
                f.write_char('"')
            }
        }
    }
}

impl ParsedLiteral {
    pub fn get_span(&self) -> SpanId {
        match self {
            ParsedLiteral::Unit(span) => *span,
            ParsedLiteral::Char(_, span) => *span,
            ParsedLiteral::Numeric(i) => i.span,
            ParsedLiteral::Bool(_, span) => *span,
            ParsedLiteral::String(_, span) => *span,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct StringId(string_interner::symbol::SymbolU32);

impl Display for StringId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
    }
}

pub struct StringPool {
    intern_pool: string_interner::StringInterner<StringBackend>,
}
impl StringPool {
    pub fn make() -> StringPool {
        let pool = string_interner::StringInterner::with_capacity(65536);
        StringPool { intern_pool: pool }
    }

    pub fn intern(&mut self, s: impl AsRef<str>) -> StringId {
        let s = self.intern_pool.get_or_intern(&s);
        StringId(s)
    }
    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.intern_pool.get(&s).map(StringId)
    }
    pub fn get_string(&self, id: StringId) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve string id")
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Ident(string_interner::symbol::SymbolU32);

#[cfg(test)]
impl Ident {
    pub fn forged() -> Ident {
        Ident(string_interner::symbol::SymbolU32::try_from_usize(1).unwrap())
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<Ident> for usize {
    fn from(value: Ident) -> Self {
        value.0.to_usize()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_usize())
    }
}

#[derive(Debug, Clone)]
pub struct NamespacedIdentifier {
    pub namespaces: EcoVec<Ident>,
    pub name: Ident,
    pub span: SpanId,
}
impl NamespacedIdentifier {
    pub fn naked(name: Ident, span: SpanId) -> NamespacedIdentifier {
        NamespacedIdentifier { namespaces: EcoVec::new(), name, span }
    }
}

#[allow(non_snake_case)]
pub struct BuiltinIdentifiers {
    pub main: Ident,
    pub self_: Ident,
    pub Self_: Ident,
    pub it: Ident,
    pub unit: Ident,
    pub char: Ident,
    pub string: Ident,
    pub length: Ident,
    pub has_value: Ident,
    pub get: Ident,
    pub not: Ident,
    pub iter: Ident,
    pub iteree: Ident,
    pub it_index: Ident,
    pub as_: Ident,
    pub list_lit: Ident,
    pub with_capacity: Ident,
    pub yielded_coll: Ident,
    pub iteree_length: Ident,
    pub block_expr_val: Ident,
    pub optelse_lhs: Ident,
    pub list_literal: Ident,
    pub source_location_typename: Ident,
    pub lambda_env_var_name: Ident,
    pub env: Ident,
    pub fn_ptr: Ident,
    pub env_ptr: Ident,
    pub amp: Ident,
    pub asterisk: Ident,
    pub bang: Ident,
    pub sb: Ident,
    pub payload: Ident,
    pub try_: Ident,
    pub try_value: Ident,
    pub if_target: Ident,
    pub crash: Ident,
    pub toRef: Ident,
    pub toDyn: Ident,
    pub toStatic: Ident,
    pub fromStatic: Ident,
    pub filename: Ident,
    pub line: Ident,
    pub equals: Ident,
    pub tag: Ident,
    pub MODULE_INFO: Ident,
    pub root_module_name: Ident,
    pub core: Ident,
    pub k1: Ident,
    pub types: Ident,
    pub TypeSchema: Ident,
    pub IntKind: Ident,
    pub IntValue: Ident,
    pub Layout: Ident,
    pub param_0: Ident,
    pub param_1: Ident,
    pub param_2: Ident,
    pub param_3: Ident,
    pub param_4: Ident,
    pub param_5: Ident,
    pub param_6: Ident,
    pub param_7: Ident,
    pub param_8: Ident,
}

// We use the default StringInterner, which uses a contiguous string as its backend
// and u32 symbols
pub struct Identifiers {
    intern_pool: string_interner::StringInterner<StringBackend>,
    pub builtins: BuiltinIdentifiers,
}
impl Identifiers {
    pub fn intern(&mut self, s: impl AsRef<str>) -> Ident {
        let s = self.intern_pool.get_or_intern(&s);
        Ident(s)
    }
    pub fn get(&self, s: impl AsRef<str>) -> Option<Ident> {
        self.intern_pool.get(&s).map(Ident)
    }
    pub fn get_name(&self, id: Ident) -> &str {
        self.intern_pool.resolve(id.0).expect("failed to resolve identifier")
    }
}

impl Default for Identifiers {
    #[allow(non_snake_case)]
    fn default() -> Self {
        let mut pool = string_interner::StringInterner::with_capacity(65536);

        let main = Ident(pool.get_or_intern_static("main"));
        let self_ = Ident(pool.get_or_intern_static("self"));
        let self_cap = Ident(pool.get_or_intern_static("Self"));
        let it = Ident(pool.get_or_intern_static("it"));
        let unit = Ident(pool.get_or_intern_static("unit"));
        let char = Ident(pool.get_or_intern_static("char"));
        let string = Ident(pool.get_or_intern_static("string"));
        let length = Ident(pool.get_or_intern_static("length"));
        let has_value = Ident(pool.get_or_intern_static("hasValue"));
        let get = Ident(pool.get_or_intern_static("get"));
        let not = Ident(pool.get_or_intern_static("not"));
        let iter = Ident(pool.get_or_intern_static("iter"));
        let iteree = Ident(pool.get_or_intern_static("iteree"));
        let it_index = Ident(pool.get_or_intern_static("itIndex"));
        let as_ = Ident(pool.get_or_intern_static("as"));
        let list_lit = Ident(pool.get_or_intern_static("list_lit"));
        let with_capacity = Ident(pool.get_or_intern_static("withCapacity"));
        let yielded_coll = Ident(pool.get_or_intern_static("yieldedColl"));
        let iteree_length = Ident(pool.get_or_intern_static("iteree_length"));
        let block_expr_val = Ident(pool.get_or_intern_static("block_expr_val"));
        let optelse_lhs = Ident(pool.get_or_intern_static("optelse_lhs"));
        let list_literal = Ident(pool.get_or_intern_static("list_literal"));
        let source_location_typename = Ident(pool.get_or_intern_static("SourceLocation"));
        let lambda_env_var_name = Ident(pool.get_or_intern_static("__lambda_env"));
        let env = Ident(pool.get_or_intern_static("env"));
        let fn_ptr = Ident(pool.get_or_intern_static("fn_ptr"));
        let env_ptr = Ident(pool.get_or_intern_static("env_ptr"));
        let amp = Ident(pool.get_or_intern_static("&"));
        let ast = Ident(pool.get_or_intern_static("*"));
        let bang = Ident(pool.get_or_intern_static("!"));
        let sb = Ident(pool.get_or_intern_static("sb"));
        let payload = Ident(pool.get_or_intern_static("payload"));
        let try_ = Ident(pool.get_or_intern_static("try"));
        let try_value = Ident(pool.get_or_intern_static("try_value"));
        let if_target = Ident(pool.get_or_intern_static("if_target"));
        let crash = Ident(pool.get_or_intern_static("crash"));
        let toRef = Ident(pool.get_or_intern_static("toRef"));
        let toDyn = Ident(pool.get_or_intern_static("toDyn"));
        let toStatic = Ident(pool.get_or_intern_static("toStatic"));
        let fromStatic = Ident(pool.get_or_intern_static("fromStatic"));
        let filename = Ident(pool.get_or_intern_static("filename"));
        let line = Ident(pool.get_or_intern_static("line"));
        let equals = Ident(pool.get_or_intern_static("equals"));
        let tag = Ident(pool.get_or_intern_static("tag"));
        let MODULE_INFO = Ident(pool.get_or_intern_static("MODULE_INFO"));
        let root_module_name = Ident(pool.get_or_intern_static("_root"));
        let core = Ident(pool.get_or_intern_static("core"));
        let k1 = Ident(pool.get_or_intern_static("k1"));
        let types = Ident(pool.get_or_intern_static("types"));
        let TypeSchema = Ident(pool.get_or_intern_static("TypeSchema"));
        let IntKind = Ident(pool.get_or_intern_static("IntKind"));
        let IntValue = Ident(pool.get_or_intern_static("IntValue"));
        let Layout = Ident(pool.get_or_intern_static("Layout"));

        let param_0 = Ident(pool.get_or_intern_static("param_0"));
        let param_1 = Ident(pool.get_or_intern_static("param_1"));
        let param_2 = Ident(pool.get_or_intern_static("param_2"));
        let param_3 = Ident(pool.get_or_intern_static("param_3"));
        let param_4 = Ident(pool.get_or_intern_static("param_4"));
        let param_5 = Ident(pool.get_or_intern_static("param_5"));
        let param_6 = Ident(pool.get_or_intern_static("param_6"));
        let param_7 = Ident(pool.get_or_intern_static("param_7"));
        let param_8 = Ident(pool.get_or_intern_static("param_8"));

        Self {
            intern_pool: pool,
            builtins: BuiltinIdentifiers {
                main,
                self_,
                Self_: self_cap,
                it,
                unit,
                char,
                string,
                length,
                has_value,
                get,
                not,
                iter,
                iteree,
                it_index,
                as_,
                list_lit,
                with_capacity,
                yielded_coll,
                iteree_length,
                block_expr_val,
                optelse_lhs,
                list_literal,
                source_location_typename,
                lambda_env_var_name,
                env,
                fn_ptr,
                env_ptr,
                amp,
                asterisk: ast,
                bang,
                sb,
                payload,
                try_,
                try_value,
                if_target,
                crash,
                toRef,
                toDyn,
                toStatic,
                fromStatic,
                filename,
                line,
                equals,
                tag,
                MODULE_INFO,
                root_module_name,
                core,
                k1,
                types,
                TypeSchema,
                IntKind,
                IntValue,
                Layout,
                param_0,
                param_1,
                param_2,
                param_3,
                param_4,
                param_5,
                param_6,
                param_7,
                param_8,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedCallArg {
    pub name: Option<Ident>,
    pub value: ParsedExprId,
    pub is_explicit_context: bool,
}
impl_copy_if_small!(12, ParsedCallArg);

impl ParsedCallArg {
    pub fn unnamed(value: ParsedExprId) -> ParsedCallArg {
        ParsedCallArg { value, name: None, is_explicit_context: false }
    }
}

#[derive(Debug, Clone)]
pub struct NamedTypeArg {
    pub name: Option<Ident>,
    pub type_expr: ParsedTypeExprId,
    pub span: SpanId,
}
impl_copy_if_small!(12, NamedTypeArg);

impl NamedTypeArg {
    pub fn unnamed(type_expr: ParsedTypeExprId, span: SpanId) -> NamedTypeArg {
        NamedTypeArg { type_expr, name: None, span }
    }
}

static_assert_size!(ParsedCall, 56);
#[derive(Debug, Clone)]
pub struct ParsedCall {
    pub name: NamespacedIdentifier,
    pub type_args: SliceHandle<NamedTypeArgId>,
    pub args: SliceHandle<CallArgId>,
    pub span: SpanId,
    pub is_method: bool,
    pub id: ParsedExprId,
}

#[derive(Debug, Clone)]
pub struct ParsedLet {
    pub name: Ident,
    pub type_expr: Option<ParsedTypeExprId>,
    pub value: ParsedExprId,
    pub span: SpanId,
    flags: u8,
}

impl ParsedLet {
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
    pub lhs: ParsedExprId,
    pub rhs: ParsedExprId,
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
    pub expr: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedVariable {
    pub name: NamespacedIdentifier,
}

impl Display for ParsedVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("var#{}", self.name.name))
    }
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: ParsedExprId,
    pub field_name: Ident,
    pub type_args: SliceHandle<NamedTypeArgId>,
    pub is_coalescing: bool,  // ?.
    pub is_referencing: bool, // *.
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructValueField {
    pub name: Ident,
    pub span: SpanId,
    /// expr is optional due to shorthand syntax
    pub expr: Option<ParsedExprId>,
}

#[derive(Debug, Clone)]
/// Example:
/// { foo: 1, bar: false }
/// ^....................^ fields
pub struct ParsedStruct {
    pub fields: Vec<StructValueField>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct AnonEnumConstructor {
    pub variant_name: Ident,
    pub payload: Option<ParsedExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumConstructor {
    pub variant_name: Ident,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedIsExpr {
    pub target_expression: ParsedExprId,
    pub pattern: ParsedPatternId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedMatchCase {
    pub patterns: smallvec::SmallVec<[ParsedPatternId; 2]>,
    pub guard_condition_expr: Option<ParsedExprId>,
    pub expression: ParsedExprId,
}

#[derive(Debug, Clone)]
pub struct ParsedMatchExpression {
    pub match_subject: ParsedExprId,
    pub cases: Vec<ParsedMatchCase>,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedCast {
    pub base_expr: ParsedExprId,
    pub dest_type: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct LambdaArgDefn {
    pub binding: Ident,
    pub ty: Option<ParsedTypeExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedLambda {
    pub arguments: Vec<LambdaArgDefn>,
    pub return_type: Option<ParsedTypeExprId>,
    pub body: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum InterpolatedStringPart {
    // TODO: Put spans on each string part
    String(StringId),
    Expr(ParsedExprId),
}

#[derive(Debug, Clone)]
pub struct ParsedInterpolatedString {
    pub parts: Vec<InterpolatedStringPart>,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub enum ParsedStaticBlockKind {
    /// Kind: Value. The statically executed code is intended to produce a value
    /// Its main purpose is the value it produces
    Value,
    /// Kind: Metaprogram. The statically executed code is intended to emit code
    /// and be replaced by the emitted code. The main purpose is
    /// code-generating metaprograms, like derivations of pretty-printers
    /// or codecs
    Metaprogram,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedStaticExpr {
    pub base_expr: ParsedExprId,
    pub kind: ParsedStaticBlockKind,
    pub parameter_names: SliceHandle<IdentSliceId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
/// While ParsedCode is an expression type, it can hold
/// any `statement`, since you may want to metaprogram with
/// statements; the value it contains is independent from the
/// AST node type used to create it
pub struct ParsedCode {
    pub parsed_stmt: ParsedStmtId,
    pub span: SpanId,
}

/// When you need to refer to a specific ability implementation:
/// `(Show @ int)/show()`
#[derive(Debug, Clone, Copy)]
pub struct ParsedQAbilityCall {
    pub ability_expr: ParsedAbilityExprId,
    pub self_name: ParsedTypeExprId,
    pub call_expr: ParsedExprId,
    pub span: SpanId,
}

static_assert_size!(ParsedExpr, 56);
#[derive(Debug, Clone)]
pub enum ParsedExpr {
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
    Literal(ParsedLiteral),
    /// ```md
    /// "hello, \{x}"
    /// ```
    InterpolatedString(ParsedInterpolatedString),
    /// ```md
    /// square(1, 2)
    /// ```
    Call(ParsedCall),
    /// ```md
    /// x
    /// ```
    Variable(ParsedVariable),
    /// ```md
    /// x.b, Opt.None[i32] (overloaded to handle enum constrs)
    /// ```
    FieldAccess(FieldAccess),
    /// ```md
    /// { <a: stmt>; <b: stmt>; <c: stmt> }
    /// ```
    Block(ParsedBlock),
    /// ```md
    /// if <cond: expr> <cons: expr> else <alt: expr>
    /// ```
    If(ParsedIfExpr),
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
    Struct(ParsedStruct),
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
    Is(ParsedIsExpr),
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
    Cast(ParsedCast),
    Lambda(ParsedLambda),
    Builtin(SpanId),
    Static(ParsedStaticExpr),
    Code(ParsedCode),
    QualifiedAbilityCall(ParsedQAbilityCall),
}

impl ParsedExpr {
    pub fn is_literal(e: &ParsedExpr) -> bool {
        matches!(e, ParsedExpr::Literal(_))
    }
    pub fn expect_literal(&self) -> &ParsedLiteral {
        match self {
            ParsedExpr::Literal(lit) => lit,
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
            Self::Call(call) => call.span,
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
            Self::Cast(as_cast) => as_cast.span,
            Self::Lambda(lambda) => lambda.span,
            Self::Builtin(span) => *span,
            Self::Static(s) => s.span,
            Self::Code(c) => c.span,
            Self::QualifiedAbilityCall(c) => c.span,
        }
    }

    pub fn as_match(&self) -> Option<&ParsedMatchExpression> {
        if let Self::Match(v) = self { Some(v) } else { None }
    }

    #[track_caller]
    pub fn expect_cast(&self) -> &ParsedCast {
        match self {
            ParsedExpr::Cast(as_cast) => as_cast,
            _ => panic!("expected cast expression"),
        }
    }

    #[track_caller]
    pub fn expect_call(&self) -> &ParsedCall {
        match self {
            ParsedExpr::Call(call) => call,
            _ => panic!("expected fn call"),
        }
    }

    #[track_caller]
    pub fn expect_lambda(&self) -> &ParsedLambda {
        match self {
            ParsedExpr::Lambda(c) => c,
            _ => panic!("expected lambda"),
        }
    }
}

enum ExprStackMember {
    Operator(BinaryOpKind, SpanId),
    Expr(ParsedExprId),
}

impl ExprStackMember {
    fn expect_expr(self) -> ParsedExprId {
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
    pub fields: Vec<(Ident, ParsedPatternId)>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumPattern {
    pub enum_name: Option<Ident>,
    pub variant_name: Ident,
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
    Literal(ParsedExprId),
    Variable(Ident, SpanId),
    Struct(ParsedStructPattern),
    Enum(ParsedEnumPattern),
    Wildcard(SpanId),
    Reference(ParsedReferencePattern),
}

impl ParsedPattern {}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: ParsedExprId,
    pub rhs: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct SetStmt {
    pub lhs: ParsedExprId,
    pub rhs: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedIfExpr {
    pub cond: ParsedExprId,
    pub cons: ParsedExprId,
    pub alt: Option<ParsedExprId>,
    pub span: SpanId,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct ParsedWhileExpr {
    pub cond: ParsedExprId,
    pub body: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedLoopExpr {
    pub body: ParsedBlock,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForExprType {
    Yield,
    Do,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
    pub iterable_expr: ParsedExprId,
    pub binding: Option<Ident>,
    pub body_block: ParsedBlock,
    pub expr_type: ForExprType,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct UseStmt {
    pub use_id: ParsedUseId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedRequire {
    pub condition_expr: ParsedExprId,
    pub else_body: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Use(UseStmt),                 // use core/list/new as foo
    Let(ParsedLet),               // let x = 42
    Require(ParsedRequire),       // require x is .Some(foo) else crash()
    Assignment(Assignment),       // x = 42
    SetRef(SetStmt),              // x <- 42
    LoneExpression(ParsedExprId), // println("asdfasdf")
}
static_assert_size!(ParsedStmt, 24);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParsedBlockKind {
    FunctionBody,
    LexicalBlock,
    LoopBody,
}

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: EcoVec<ParsedStmtId>,
    pub kind: ParsedBlockKind,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Ident,
    pub ty: ParsedTypeExprId,
    pub private: bool,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: EcoVec<StructTypeField>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypeApplication {
    pub name: NamespacedIdentifier,
    pub args: SliceHandle<NamedTypeArgId>,
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
pub struct ParsedArrayType {
    pub size_expr: ParsedTypeExprId,
    pub element_type: ParsedTypeExprId,
    pub span: SpanId,
}
impl_copy_if_small!(12, ParsedArrayType);

#[derive(Debug, Clone)]
pub struct ParsedEnumVariant {
    pub tag_name: Ident,
    pub payload_expression: Option<ParsedTypeExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumType {
    pub variants: Vec<ParsedEnumVariant>,
    pub tag_type: Option<ParsedTypeExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedDotMemberAccess {
    pub base: ParsedTypeExprId,
    pub member_name: Ident,
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
    pub fn bits(&self) -> u32 {
        match self {
            NumericWidth::B8 => 8,
            NumericWidth::B16 => 16,
            NumericWidth::B32 => 32,
            NumericWidth::B64 => 64,
        }
    }

    pub fn bytes(&self) -> usize {
        match self {
            NumericWidth::B8 => 1,
            NumericWidth::B16 => 2,
            NumericWidth::B32 => 4,
            NumericWidth::B64 => 8,
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
    pub params: SmallVec<[ParsedTypeExprId; 8]>,
    pub return_type: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedTypeOf {
    pub target_expr: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedTypeFromId {
    pub id_expr: ParsedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct SomeQuantifier {
    pub inner_type_expr: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedStaticTypeExpr {
    pub inner_type_expr: ParsedTypeExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum ParsedTypeExpr {
    Builtin(SpanId),
    Struct(StructType),
    TypeApplication(TypeApplication),
    Optional(ParsedOptional),
    Reference(ParsedReference),
    Array(ParsedArrayType),
    Enum(ParsedEnumType),
    DotMemberAccess(ParsedDotMemberAccess),
    Function(ParsedFunctionType),
    TypeOf(ParsedTypeOf),
    SomeQuant(SomeQuantifier),
    Static(ParsedStaticTypeExpr),
    /// Used only by compiler-generated code, currently
    TypeFromId(ParsedTypeFromId),
    StaticLiteral(ParsedLiteral),
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
            ParsedTypeExpr::Array(arr) => arr.span,
            ParsedTypeExpr::Enum(e) => e.span,
            ParsedTypeExpr::DotMemberAccess(a) => a.span,
            ParsedTypeExpr::Function(f) => f.span,
            ParsedTypeExpr::TypeOf(tof) => tof.span,
            ParsedTypeExpr::SomeQuant(q) => q.span,
            ParsedTypeExpr::Static(s) => s.span,
            ParsedTypeExpr::TypeFromId(tfi) => tfi.span,
            ParsedTypeExpr::StaticLiteral(l) => l.get_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeParam {
    pub name: Ident,
    pub constraints: EcoVec<ParsedTypeConstraintExpr>,
    pub span: SpanId,
}

impl ParsedTypeParam {}

#[derive(Debug, Clone, Copy)]
pub enum ParsedTypeConstraintExpr {
    Ability(ParsedAbilityExprId),
    Static(ParsedTypeExprId),
    // Predicate(NamespaceIdentifier)
}

impl ParsedTypeConstraintExpr {
    pub fn as_ability(&self) -> Option<ParsedAbilityExprId> {
        if let ParsedTypeConstraintExpr::Ability(ability) = self { Some(*ability) } else { None }
    }

    pub fn single_static_constraint_or_fail(
        constraints: &[ParsedTypeConstraintExpr],
    ) -> Result<Option<ParsedTypeExprId>, &'static str> {
        let mut constraint: Option<ParsedTypeExprId> = None;
        for c in constraints.iter() {
            if let ParsedTypeConstraintExpr::Static(s) = c {
                match &constraint {
                    None => constraint = Some(*s),
                    Some(_) => {
                        return Err("Type parameter has more than one static constraint");
                    }
                }
            }
        }
        Ok(constraint)
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeConstraint {
    pub name: Ident,
    pub constraint_expr: ParsedTypeConstraintExpr,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub name: Ident,
    pub type_params: EcoVec<ParsedTypeParam>,
    // TODO(perf, efficient ast): Migrate params and context_params to a single SliceHandle
    pub params: EcoVec<FnArgDef>,
    pub context_params: EcoVec<FnArgDef>,
    pub ret_type: ParsedTypeExprId,
    pub block: Option<ParsedExprId>,
    pub signature_span: SpanId,
    pub span: SpanId,
    pub linkage: Linkage,
    pub directives: EcoVec<ParsedDirective>,
    pub additional_where_constraints: EcoVec<ParsedTypeConstraint>,
    pub condition: Option<ParsedExprId>,
    pub id: ParsedFunctionId,
}

impl ParsedFunction {}

#[derive(Debug, Clone)]
pub struct FnArgDef {
    pub name: Ident,
    pub type_expr: ParsedTypeExprId,
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

    #[inline]
    pub fn set_context(&mut self) {
        self.0 |= 1;
    }

    #[inline]
    pub fn is_context(&self) -> bool {
        self.0 & 1 != 0
    }
}

#[derive(Debug, Clone)]
pub struct ParsedGlobal {
    pub name: Ident,
    pub ty: ParsedTypeExprId,
    pub value_expr: ParsedExprId,
    pub span: SpanId,
    pub id: ParsedGlobalId,
    pub is_mutable: bool,
    pub is_referencing: bool,
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDefnFlags(u32);
impl ParsedTypeDefnFlags {
    pub fn new(alias: bool) -> Self {
        let mut s = Self(0);
        if alias {
            s.set_alias();
        }
        s
    }

    pub fn set_alias(&mut self) {
        self.0 |= 1;
    }

    pub fn is_alias(&self) -> bool {
        self.0 & 1 != 0
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDefn {
    pub name: Ident,
    pub value_expr: ParsedTypeExprId,
    pub type_params: Vec<ParsedTypeParam>,
    pub span: SpanId,
    pub id: ParsedTypeDefnId,
    pub flags: ParsedTypeDefnFlags,
}

#[derive(Debug, Clone)]
pub struct ParsedAbilityParameter {
    pub name: Ident,
    pub is_impl_param: bool,
    pub constraints: EcoVec<ParsedTypeConstraintExpr>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedAbility {
    pub name: Ident,
    pub functions: Vec<ParsedFunctionId>,
    pub span: SpanId,
    pub params: Vec<ParsedAbilityParameter>,
    pub id: ParsedAbilityId,
}

nz_u32_id!(ParsedAbilityExprId);

#[derive(Debug, Clone)]
pub struct ParsedAbilityExpr {
    pub name: NamespacedIdentifier,
    pub arguments: SliceHandle<NamedTypeArgId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedAbilityImplementation {
    pub ability_expr: ParsedAbilityExprId,
    pub generic_impl_params: EcoVec<ParsedTypeParam>,
    pub self_type: ParsedTypeExprId,
    pub functions: EcoVec<ParsedFunctionId>,
    pub id: ParsedAbilityImplId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ParsedNamespace {
    pub name: Ident,
    pub definitions: EcoVec<ParsedId>,
    pub id: ParsedNamespaceId,
    pub span: SpanId,
}

pub struct ParsedExpressionPool {
    // `expressions` and `type_hints` form a Struct-of-Arrays relationship
    expressions: Pool<ParsedExpr, ParsedExprId>,
    type_hints: Pool<Option<ParsedTypeExprId>, ParsedExprId>,
    directives: FxHashMap<ParsedExprId, EcoVec<ParsedDirective>>,
}
impl ParsedExpressionPool {
    pub fn new(capacity: usize) -> Self {
        ParsedExpressionPool {
            expressions: Pool::with_capacity("parsed_expr", capacity),
            type_hints: Pool::with_capacity("parsed_expr_type_hint", capacity),
            directives: FxHashMap::default(),
        }
    }

    pub fn set_type_hint(&mut self, id: ParsedExprId, ty: ParsedTypeExprId) {
        *self.type_hints.get_mut(id) = Some(ty)
    }

    pub fn get_type_hint(&self, id: ParsedExprId) -> Option<ParsedTypeExprId> {
        *self.type_hints.get(id)
    }

    pub fn add_directives(&mut self, id: ParsedExprId, directives: EcoVec<ParsedDirective>) {
        self.directives.insert(id, directives);
    }

    pub fn get_directives(&self, id: ParsedExprId) -> &[ParsedDirective] {
        self.directives.get(&id).map(|v| &v[..]).unwrap_or(&[])
    }

    pub fn add_expression(&mut self, mut expression: ParsedExpr) -> ParsedExprId {
        let id: ParsedExprId = self.expressions.next_id();
        if let ParsedExpr::Call(call) = &mut expression {
            call.id = id;
        }
        self.expressions.add(expression);
        self.type_hints.add(None);
        id
    }

    pub fn get(&self, id: ParsedExprId) -> &ParsedExpr {
        self.expressions.get(id)
    }

    pub fn get_span(&self, id: ParsedExprId) -> SpanId {
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
    pub fn add_source(&mut self, mut source: Source) -> FileId {
        let id = self.next_file_id();
        source.file_id = id;
        self.sources.push(source);
        id
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

nz_u32_id!(IdentSliceId);

pub struct ParsedProgram {
    pub name: String,
    pub name_id: Ident,
    pub config: CompilerConfig,
    pub spans: Spans,
    pub functions: Vec<ParsedFunction>,
    pub globals: Pool<ParsedGlobal, ParsedGlobalId>,
    pub type_defns: Vec<ParsedTypeDefn>,
    pub namespaces: Pool<ParsedNamespace, ParsedNamespaceId>,
    pub abilities: Vec<ParsedAbility>,
    pub ability_impls: Vec<ParsedAbilityImplementation>,
    pub sources: Sources,
    pub idents: Identifiers,
    pub strings: StringPool,
    pub exprs: ParsedExpressionPool,
    pub type_exprs: ParsedTypeExpressionPool,
    pub patterns: ParsedPatternPool,
    pub stmts: Pool<ParsedStmt, ParsedStmtId>,
    pub uses: ParsedUsePool,
    pub errors: Vec<ParseError>,
    // p_ prefix means 'pool'; used to delineate secondary pools from primary language concepts
    pub p_type_args: Pool<NamedTypeArg, NamedTypeArgId>,
    pub p_call_args: Pool<ParsedCallArg, CallArgId>,
    pub p_idents: Pool<Ident, IdentSliceId>,
    pub p_ability_exprs: Pool<ParsedAbilityExpr, ParsedAbilityExprId>,
}

impl ParsedProgram {
    pub fn make(name: String, config: CompilerConfig) -> ParsedProgram {
        let mut idents = Identifiers::default();
        let name_id = idents.intern(&name);
        ParsedProgram {
            name,
            name_id,
            config,
            spans: Spans::new(),
            functions: Vec::new(),
            globals: Pool::with_capacity("parsed_globals", 4096),
            type_defns: Vec::new(),
            namespaces: Pool::with_capacity("parsed_namespaces", 4096),
            abilities: Vec::new(),
            ability_impls: Vec::new(),
            sources: Sources::default(),
            idents,
            strings: StringPool::make(),
            exprs: ParsedExpressionPool::new(16384),
            type_exprs: ParsedTypeExpressionPool::new(8192),
            patterns: ParsedPatternPool::default(),
            stmts: Pool::with_capacity("parsed_stmts", 8192),
            uses: ParsedUsePool::default(),
            errors: Vec::new(),
            p_type_args: Pool::with_capacity("parsed_named_type_args", 8192),
            p_call_args: Pool::with_capacity("parsed_call_args", 8192),
            p_idents: Pool::with_capacity("ident_slices", 8192),
            p_ability_exprs: Pool::with_capacity("ability_exprs", 8192),
        }
    }

    pub fn push_error(&mut self, e: ParseError) {
        print_error(self, &e);
        self.errors.push(e);
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

    pub fn add_namespace(&mut self, mut namespace: ParsedNamespace) -> ParsedNamespaceId {
        let id = self.namespaces.next_id();
        namespace.id = id;
        let real_id = self.namespaces.add(namespace);
        debug_assert_eq!(real_id, id);
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
            ParsedStmt::Let(v) => v.span,
            ParsedStmt::Require(g) => g.span,
            ParsedStmt::Assignment(a) => a.span,
            ParsedStmt::SetRef(s) => s.span,
            ParsedStmt::LoneExpression(expr_id) => self.exprs.get_span(*expr_id),
        }
    }

    pub fn get_global(&self, id: ParsedGlobalId) -> &ParsedGlobal {
        self.globals.get(id)
    }

    pub fn add_global(&mut self, mut global: ParsedGlobal) -> ParsedGlobalId {
        let id = self.globals.next_id();
        global.id = id;
        let id2 = self.globals.add(global);
        debug_assert!(id == id2);
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
        self.namespaces.get(ParsedNamespaceId::ONE)
    }

    pub fn get_expression_type_hint(&self, id: ParsedExprId) -> Option<ParsedTypeExprId> {
        self.exprs.get_type_hint(id)
    }

    pub fn get_type_expr_span(&self, type_expression_id: ParsedTypeExprId) -> SpanId {
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
            ParsedId::Namespace(_) => SpanId::NONE,
            ParsedId::Global(id) => self.get_global(id).span,
            ParsedId::Ability(id) => self.get_ability(id).span,
            ParsedId::AbilityImpl(id) => self.get_ability_impl(id).span,
            ParsedId::TypeDefn(id) => self.get_type_defn(id).span,
            ParsedId::Expression(id) => self.exprs.get_span(id),
            ParsedId::TypeExpression(id) => self.get_type_expr_span(id),
            ParsedId::Pattern(id) => self.get_pattern_span(id),
            ParsedId::Use(id) => self.uses.get_use(id).span,
        }
    }

    pub fn get_expr_span(&self, cond: ParsedExprId) -> SpanId {
        self.exprs.get_span(cond)
    }
}

pub type ParseResult<A> = anyhow::Result<A, ParseError>;

#[derive(Debug, Clone)]
pub enum ParseError {
    Parse { message: String, token: Token, cause: Option<Box<ParseError>> },
    Lex(LexError),
}

impl ParseError {
    pub fn message(&self) -> &str {
        match self {
            ParseError::Lex(lex_error) => &lex_error.message,
            ParseError::Parse { message, .. } => message,
        }
    }

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

pub fn print_error(module: &ParsedProgram, parse_error: &ParseError) {
    let mut stderr = std::io::stderr();

    match parse_error {
        ParseError::Lex(lex_error) => {
            write_source_location(
                &mut stderr,
                &module.spans,
                &module.sources,
                lex_error.span,
                ErrorLevel::Error,
                6,
                Some(&lex_error.message),
            )
            .unwrap();
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

            write_source_location(
                &mut stderr,
                &module.spans,
                &module.sources,
                span,
                ErrorLevel::Error,
                6,
                Some(&format!("{message} at '{}'\n", got_str)),
            )
            .unwrap();
            eprintln!();
        }
    }
}

pub fn write_source_location(
    w: &mut impl std::io::Write,
    spans: &Spans,
    sources: &Sources,
    span_id: SpanId,
    level: ErrorLevel,
    context_lines: usize,
    message: Option<&str>,
) -> std::io::Result<()> {
    let span = spans.get(span_id);
    let source = sources.source_by_span(span);
    let Some(line) = source.get_line_for_span_start(span) else {
        writeln!(w, "Critical Error: could not find line for span {:?}", span)?;
        return Ok(());
    };
    use colored::*;

    let color = match level {
        ErrorLevel::Error => Color::Red,
        ErrorLevel::Warn => Color::Yellow,
        ErrorLevel::Info => Color::Yellow,
        ErrorLevel::Hint => Color::Yellow,
    };
    let level_name = match level {
        ErrorLevel::Error => "Error",
        ErrorLevel::Warn => "Warning",
        ErrorLevel::Info => "Info",
        ErrorLevel::Hint => "Hint",
    }
    .color(color);

    // If the span is longer than the line, just highlight the whole line
    let highlight_length = if span.len > line.len { line.len as usize } else { span.len as usize };
    let thingies = "^".repeat(highlight_length).red();
    let spaces = " ".repeat((span.start - line.start_char) as usize);
    let line_start = line.line_index as i32 - context_lines as i32 / 2;
    let line_end = line_start + context_lines as i32;
    let red_arrow = "->".color(color);
    writeln!(w, "┌────────────────────────────────────────╴")?;
    writeln!(
        w,
        "│{} at {}/{}:{}",
        level_name,
        source.directory,
        source.filename,
        line.line_index + 1,
    )?;
    writeln!(w, "├─────")?;
    for line_index in line_start..line_end {
        if line_index >= 0 {
            if let Some(this_line) = source.get_line(line_index as usize) {
                let line_content = source.get_line_content(this_line);
                if line_index == line.line_index as i32 {
                    writeln!(w, "│ {red_arrow}{}\n│   {spaces}{thingies}", line_content).unwrap();
                } else {
                    writeln!(w, "│   {}", line_content).unwrap();
                }
            }
        }
    }
    if message.is_some() {
        writeln!(w, "├─────")?;
    } else {
        writeln!(w, "└─────")?;
    }
    if let Some(msg) = message {
        writeln!(w, "│  {msg}")?;
        writeln!(w, "└────────────────────────────────────────╴")?;
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub struct Line {
    pub start_char: u32,
    pub len: u32,
    pub line_index: u32,
}

impl Line {
    pub fn line_number(&self) -> u32 {
        self.line_index + 1
    }
    pub fn end_char(&self) -> u32 {
        self.start_char + self.len
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    pub file_id: FileId,
    pub directory: String,
    pub filename: String,
    pub content: String,
    pub lines: Vec<Line>,
}

impl Source {
    pub fn make(file_id: FileId, directory: String, filename: String, content: String) -> Source {
        let mut lines = Vec::with_capacity(128);
        let mut iter = content.chars().enumerate().peekable();
        let mut line_len: usize = 0;
        // We compute lines ourselves because we need to know the start offset of each line
        // in chars
        while let Some((c_index, c)) = iter.next() {
            let mut push_line = || {
                let start: u32 = (c_index - line_len) as u32;
                let len = c_index as u32 - start;
                debug_assert_eq!(len, line_len as u32);
                lines.push(Line { start_char: start, len, line_index: lines.len() as u32 });
                line_len = 0;
            };
            if c == '\n' {
                push_line();
            } else if c == '\r' && iter.peek().is_some_and(|(_, c)| *c == '\n') {
                // Skip over the \n
                iter.next();
                push_line();
            } else {
                line_len += 1;
            }
        }
        if line_len != 0 {
            // Push the last line
            let start: u32 = (content.len() - line_len) as u32;
            lines.push(Line {
                start_char: start,
                len: content.len() as u32 - start,
                line_index: lines.len() as u32,
            });
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

    pub fn get_line_content(&self, line: &Line) -> &str {
        self.get_content(line.start_char, line.len)
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

pub fn init_module(module_name: Ident, ast: &mut ParsedProgram) -> ParsedNamespaceId {
    let root_namespace_id = if ast.namespaces.is_empty() {
        let name = ast.idents.intern("_root");
        ast.add_namespace(ParsedNamespace {
            name,
            definitions: EcoVec::new(),
            id: ParsedNamespaceId::ONE,
            span: SpanId::NONE,
        })
    } else {
        ast.get_root_namespace().id
    };

    let module_namespace_id = ast.add_namespace(ParsedNamespace {
        name: module_name,
        definitions: eco_vec![],
        id: root_namespace_id,
        span: SpanId::NONE,
    });
    ast.namespaces
        .get_mut(root_namespace_id)
        .definitions
        .push(ParsedId::Namespace(module_namespace_id));
    module_namespace_id
}

pub fn parse_file(
    ast: &mut ParsedProgram,
    module_name: Ident,
    module_namespace_id: ParsedNamespaceId,
    file_id: FileId,
    tokens: &[Token],
) -> ParseResult<()> {
    let mut parser = Parser::make_for_file(module_name, module_namespace_id, ast, tokens, file_id);
    parser.parse_file();
    Ok(())
}

pub struct Parser<'toks, 'module> {
    pub module_name: Ident,
    pub module_namespace_id: ParsedNamespaceId,
    pub ast: &'module mut ParsedProgram,
    tokens: TokenIter<'toks>,
    file_id: FileId,
    string_buffer: String,
}

impl<'toks, 'ast> Parser<'toks, 'ast> {
    pub fn make_for_file(
        module_name: Ident,
        module_namespace_id: ParsedNamespaceId,
        ast: &'ast mut ParsedProgram,
        tokens: &'toks [Token],
        file_id: FileId,
    ) -> Parser<'toks, 'ast> {
        Parser {
            module_name,
            module_namespace_id,
            ast,
            tokens: TokenIter::make(tokens),
            file_id,
            string_buffer: String::with_capacity(1024),
        }
    }
    pub fn parse_file(&mut self) {
        let mut new_definitions: Vec<ParsedId> = vec![];
        loop {
            match self.parse_definition(K::Eof) {
                Ok(Some(def)) => new_definitions.push(def),
                Err(err) => {
                    self.ast.push_error(err);
                    // For now, break on first parse error
                    break;
                }
                Ok(None) => break,
            }
        }

        self.ast.namespaces.get_mut(self.module_namespace_id).definitions.extend(new_definitions);
    }

    pub fn parse_definition(&mut self, terminator: TokenKind) -> ParseResult<Option<ParsedId>> {
        let condition = if self.maybe_consume_next(K::Hash).is_some() {
            if self.maybe_consume_next(K::KeywordIf).is_some() {
                let condition_expr = self.expect_expression()?;
                Some(condition_expr)
            } else {
                return Err(error_expected("#if", self.peek()));
            }
        } else {
            None
        };
        if let Some(use_id) = self.parse_use()? {
            Ok(Some(ParsedId::Use(use_id)))
        } else if let Some(ns) = self.parse_namespace()? {
            Ok(Some(ParsedId::Namespace(ns)))
        } else if let Some(global_id) = self.parse_global()? {
            self.expect_eat_token(K::Semicolon)?;
            Ok(Some(ParsedId::Global(global_id)))
        } else if let Some(function_id) = self.parse_function(condition)? {
            Ok(Some(ParsedId::Function(function_id)))
        } else if let Some(type_defn_id) = self.parse_type_defn()? {
            Ok(Some(ParsedId::TypeDefn(type_defn_id)))
        } else if let Some(ability_id) = self.parse_ability_defn()? {
            Ok(Some(ParsedId::Ability(ability_id)))
        } else if let Some(ability_impl_id) = self.parse_ability_impl()? {
            Ok(Some(ParsedId::AbilityImpl(ability_impl_id)))
        } else {
            if condition.is_some() {
                return Err(error_expected(
                    "Some definition following condition directive #if",
                    self.peek(),
                ));
            } else if self.peek().kind == terminator {
                self.advance();
                Ok(None)
            } else {
                let err = error_expected(
                    format!("Definition (fn, deftype, or ns) or {terminator}"),
                    self.peek(),
                );
                Err(err)
            }
        }
    }

    fn source(&self) -> &Source {
        self.ast.sources.get_source(self.file_id)
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
        let here = self.peek_back();
        if here.kind == K::Eof { span } else { self.extend_span(span, here.span) }
    }

    fn extend_span(&mut self, span1: SpanId, span2: SpanId) -> SpanId {
        self.ast.spans.extend(span1, span2)
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
        #[allow(clippy::while_let_loop)] // Since we'll add more stuff later
        loop {
            if let Some(asterisk) = self.maybe_consume_next(K::Asterisk) {
                let inner_span = self.ast.get_pattern_span(pattern_id);
                let span = self.extend_span(inner_span, asterisk.span);
                pattern_id = self.ast.patterns.add_pattern(ParsedPattern::Reference(
                    ParsedReferencePattern { inner: pattern_id, span },
                ))
            } else {
                break;
            }
        }
        Ok(pattern_id)
    }

    fn expect_pattern_base(&mut self) -> ParseResult<ParsedPatternId> {
        let (first, second) = self.peek_two();
        if let Some(literal_id) = self.parse_literal_atom()? {
            let pattern = ParsedPattern::Literal(literal_id);
            let id = self.ast.patterns.add_pattern(pattern);
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
                    self.ast.patterns.add_pattern(pattern)
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
            let pattern_id = self.ast.patterns.add_pattern(ParsedPattern::Struct(pattern));
            Ok(pattern_id)
        } else if first.kind == K::Dot || (first.kind == K::Ident && second.kind == K::Dot) {
            let enum_name = if first.kind == K::Ident {
                // Eats the Dot
                self.advance();
                let enum_name = self.intern_ident_token(first);
                self.expect_eat_token(K::Dot)?;
                Some(enum_name)
            } else {
                // Eats the Dot
                self.advance();
                None
            };
            let variant_name_token = self.expect_eat_token(K::Ident)?;
            let variant_name_ident = self.intern_ident_token(variant_name_token);
            let (payload_pattern, span) = if self.peek().kind == K::OpenParen {
                self.advance();
                let payload_pattern_id = self.expect_parse_pattern()?;
                let close_paren = self.expect_eat_token(K::CloseParen)?;
                (Some(payload_pattern_id), self.ast.spans.extend(first.span, close_paren.span))
            } else {
                (None, self.ast.spans.extend(first.span, variant_name_token.span))
            };
            let pattern_id =
                self.ast.patterns.add_pattern(ParsedPattern::Enum(ParsedEnumPattern {
                    enum_name,
                    variant_name: variant_name_ident,
                    payload_pattern,
                    span,
                }));
            Ok(pattern_id)
        } else if first.kind == K::Ident {
            // Variable
            let ident_token = self.expect_eat_token(K::Ident)?;
            let ident = self.intern_ident_token(ident_token);
            if self.ast.idents.get_name(ident) == "_" {
                let pattern_id =
                    self.ast.patterns.add_pattern(ParsedPattern::Wildcard(ident_token.span));
                Ok(pattern_id)
            } else {
                let pattern_id =
                    self.ast.patterns.add_pattern(ParsedPattern::Variable(ident, ident_token.span));
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
        Parser::tok_chars(&self.ast.spans, self.source(), tok)
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

    fn intern_ident_token(&mut self, token: Token) -> Ident {
        let tok_chars =
            Parser::tok_chars(&self.ast.spans, self.ast.sources.get_source(self.file_id), token);
        self.ast.idents.intern(tok_chars)
    }

    pub fn add_expression(&mut self, expression: ParsedExpr) -> ParsedExprId {
        self.ast.exprs.add_expression(expression)
    }

    pub fn add_expression_with_directives(
        &mut self,
        expression: ParsedExpr,
        directives: EcoVec<ParsedDirective>,
    ) -> ParsedExprId {
        let id = self.ast.exprs.add_expression(expression);
        self.ast.exprs.add_directives(id, directives);
        id
    }

    pub fn get_expression(&self, id: ParsedExprId) -> &ParsedExpr {
        self.ast.exprs.get(id)
    }

    pub fn get_expression_span(&self, id: ParsedExprId) -> SpanId {
        self.ast.exprs.get(id).get_span()
    }

    pub fn get_type_expression_span(&self, id: ParsedTypeExprId) -> SpanId {
        self.ast.type_exprs.get(id).get_span()
    }

    fn parse_literal_atom(&mut self) -> ParseResult<Option<ParsedExprId>> {
        let (first, second) = self.tokens.peek_two();
        trace!("parse_literal {} {}", first.kind, second.kind);
        match (first.kind, second.kind) {
            (K::KeywordBuiltin, _) => {
                self.advance();
                Ok(Some(self.add_expression(ParsedExpr::Builtin(first.span))))
            }
            (K::OpenParen, K::CloseParen) => {
                trace!("parse_literal unit");
                self.advance();
                self.advance();
                let span = self.extend_token_span(first, second);
                Ok(Some(self.add_expression(ParsedExpr::Literal(ParsedLiteral::Unit(span)))))
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
                            Some(c) => Ok(ParsedLiteral::Char(c.output, first.span)),
                            None => Err(error(
                                format!(
                                    "Invalid escaped char following escape sequence: {}",
                                    char::from(esc_char)
                                ),
                                first,
                            )),
                        }?;
                    Ok(Some(self.add_expression(ParsedExpr::Literal(literal))))
                } else {
                    debug_assert_eq!(bytes.len(), 3);
                    let byte = bytes[1];
                    Ok(Some(self.add_expression(ParsedExpr::Literal(ParsedLiteral::Char(
                        byte, first.span,
                    )))))
                }
            }
            (K::String { .. } | K::StringUnterminated { .. }, _) => Ok(Some(self.expect_string()?)),
            (K::Minus, K::Ident) if !second.is_whitespace_preceeded() => {
                let text = self.token_chars(second);
                if text.chars().next().unwrap().is_numeric() {
                    let mut s = "-".to_string();
                    s.push_str(text);
                    self.advance();
                    self.advance();
                    let span = self.extend_token_span(first, second);
                    let numeric = ParsedLiteral::Numeric(ParsedNumericLiteral { text: s, span });
                    Ok(Some(self.add_expression(ParsedExpr::Literal(numeric))))
                } else {
                    Err(error_expected("number following '-'", second))
                }
            }
            (K::Ident, _) => {
                let text = self.token_chars(first);
                if text == "true" {
                    self.advance();
                    Ok(Some(self.add_expression(ParsedExpr::Literal(ParsedLiteral::Bool(
                        true, first.span,
                    )))))
                } else if text == "false" {
                    self.advance();
                    Ok(Some(self.add_expression(ParsedExpr::Literal(ParsedLiteral::Bool(
                        false, first.span,
                    )))))
                } else {
                    match text.chars().next() {
                        Some(c) if c.is_numeric() => {
                            let s = text.to_string();
                            self.advance();
                            Ok(Some(self.add_expression(ParsedExpr::Literal(
                                ParsedLiteral::Numeric(ParsedNumericLiteral {
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

    fn expect_string(&mut self) -> ParseResult<ParsedExprId> {
        let first = self.tokens.peek();
        trace!("expect_string");

        let mut buf = std::mem::take(&mut self.string_buffer);
        let mut parts: Vec<InterpolatedStringPart> = Vec::new();
        let mut first_segment = true;
        loop {
            let current_token = self.tokens.next();
            match current_token.kind {
                // Interpolation case
                K::OpenBrace => {
                    let expr_id = self.expect_expression()?;
                    parts.push(InterpolatedStringPart::Expr(expr_id));
                    self.expect_eat_token(K::CloseBrace)?;
                }
                K::StringUnterminated { delim } | K::String { delim } => {
                    // Accessing the tok_chars this way achieves a partial borrow of self
                    let text = Parser::tok_chars(
                        &self.ast.spans,
                        self.ast.sources.get_source(self.file_id),
                        current_token,
                    );

                    buf.clear();
                    let mut chars = text.chars().peekable();
                    if first_segment {
                        // Skip opening " or `
                        let c = chars.next();
                        if c != Some('"') && c != Some('`') {
                            return Err(error(
                                "Internal Error: should start with a \" or a `",
                                first,
                            ));
                        }
                        first_segment = false;
                    }
                    let is_terminated = matches!(current_token.kind, K::String { .. });
                    while let Some(c) = chars.next() {
                        if c == '{' {
                            let Some(next) = chars.next() else {
                                return Err(error("String ended with '{'", first));
                            };
                            if next == '{' {
                                buf.push('{');
                            } else {
                                return Err(error("ICE: contains non-escaped '{'", first));
                            }
                        } else if c == '\\' {
                            let Some(next) = chars.next() else {
                                return Err(error("String ended with '\\'", first));
                            };
                            if let Some(c) =
                                SHARED_STRING_ESCAPED_CHARS.iter().find(|c| c.sentinel == next)
                            {
                                buf.push(c.output as char)
                            } else if next == delim.char() {
                                // Escaped closing delimiter ` or "
                                buf.push(delim.char())
                            } else {
                                // Push both; not an escape sequence
                                buf.push('\\');
                                buf.push(next);
                            };
                        } else if c == delim.char() {
                            // Skip closing delimiters of terminated string tokens
                            if chars.peek().is_none() && is_terminated {
                            } else {
                                buf.push(c)
                            }
                        } else {
                            buf.push(c)
                        }
                    }
                    let string_id = self.ast.strings.intern(&buf);

                    parts.push(InterpolatedStringPart::String(string_id));
                    // StringUnterminated means there are more segments
                    // String means we're done;
                    if is_terminated {
                        break;
                    }
                }
                _k => {
                    return Err(error("Unexpected token kind in string sequence", current_token));
                }
            }
        }
        let result = if parts.len() == 1 {
            let InterpolatedStringPart::String(s) = parts.into_iter().next().unwrap() else {
                panic!()
            };
            let literal = ParsedLiteral::String(s, first.span);
            Ok(self.add_expression(ParsedExpr::Literal(literal)))
        } else {
            let span = self.extend_to_here(first.span);
            let string_interp = ParsedInterpolatedString { parts, span };
            Ok(self.add_expression(ParsedExpr::InterpolatedString(string_interp)))
        };
        self.string_buffer = buf;
        result
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
                    let span =
                        self.extend_span(self.ast.get_type_expr_span(result), ident_token.span);
                    let new = ParsedTypeExpr::DotMemberAccess(ParsedDotMemberAccess {
                        base: result,
                        member_name: ident,
                        span,
                    });
                    let new_id = self.ast.type_exprs.add(new);
                    result = new_id;
                } else if next.kind == K::QuestionMark {
                    // Optional Type
                    self.advance();
                    result = self.ast.type_exprs.add(ParsedTypeExpr::Optional(ParsedOptional {
                        base: result,
                        span: next.span,
                    }));
                } else if next.kind == K::Asterisk {
                    // Reference Type
                    self.advance();
                    result = self.ast.type_exprs.add(ParsedTypeExpr::Reference(ParsedReference {
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
        Parser::tok_chars(&self.ast.spans, self.source(), token)
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
            let type_expr_id = self.ast.type_exprs.add(ParsedTypeExpr::Enum(enumm));
            Ok(Some(type_expr_id))
        } else if first.kind == K::BackSlash {
            let fun = self.expect_function_type()?;
            Ok(Some(fun))
        } else if first.kind == K::KeywordBuiltin {
            self.advance();
            let builtin_id = self.ast.type_exprs.add(ParsedTypeExpr::Builtin(first.span));
            Ok(Some(builtin_id))
        } else if let Some(literal_expr_id) = self.parse_literal_atom()? {
            match self.ast.exprs.get(literal_expr_id) {
                ParsedExpr::Literal(l) => {
                    let type_expr_id =
                        self.ast.type_exprs.add(ParsedTypeExpr::StaticLiteral(l.clone()));
                    Ok(Some(type_expr_id))
                }
                _ => unreachable!("parse_literal returned non-literal"),
            }
        } else if first.kind == K::Ident {
            let ident_chars = self.get_token_chars(first);
            if ident_chars == "typeOf" {
                self.advance();
                self.expect_eat_token(K::OpenParen)?;
                let target_expr = self.expect_expression()?;
                let end = self.expect_eat_token(K::CloseParen)?;
                let span = self.extend_token_span(first, end);
                let type_of = ParsedTypeExpr::TypeOf(ParsedTypeOf { target_expr, span });
                Ok(Some(self.ast.type_exprs.add(type_of)))
            } else if ident_chars == "typeFromId" {
                self.advance();
                self.expect_eat_token(K::OpenParen)?;
                let target_expr = self.expect_expression()?;
                let end = self.expect_eat_token(K::CloseParen)?;
                let span = self.extend_token_span(first, end);
                let type_from_id =
                    ParsedTypeExpr::TypeFromId(ParsedTypeFromId { id_expr: target_expr, span });
                Ok(Some(self.ast.type_exprs.add(type_from_id)))
            } else if ident_chars == "static" {
                self.advance();
                let inner_type_expr = self.expect_type_expression()?;
                let span = self.extend_to_here(first.span);
                let static_expr =
                    ParsedTypeExpr::Static(ParsedStaticTypeExpr { inner_type_expr, span });
                Ok(Some(self.ast.type_exprs.add(static_expr)))
            } else if ident_chars == "some" {
                self.advance();
                let inner_expr = self.expect_type_expression()?;
                let span = self.extend_to_here(first.span);
                let quantifier =
                    ParsedTypeExpr::SomeQuant(SomeQuantifier { inner_type_expr: inner_expr, span });
                Ok(Some(self.ast.type_exprs.add(quantifier)))
            } else {
                let base_name = self.expect_namespaced_ident()?;

                // Special case for Array[N x T] syntax
                if base_name.namespaces.is_empty() {
                    if self.ast.idents.get_name(base_name.name) == "Array" {
                        // Array must always have bracket syntax
                        self.expect_eat_token(K::OpenBracket)?;
                        let size_expr = self.expect_type_expression()?;

                        // Expect 'x' keyword
                        let x_token = self.peek();
                        if x_token.kind == K::Ident && self.get_token_chars(x_token) == "x" {
                            self.advance();
                        } else {
                            return Err(error(
                                "Expected 'x' in Array type, example Array[4 x u8]",
                                x_token,
                            ));
                        }

                        let element_type = self.expect_type_expression()?;
                        let end_bracket = self.expect_eat_token(K::CloseBracket)?;
                        let span = self.extend_token_span(first, end_bracket);

                        let array_type = ParsedArrayType { size_expr, element_type, span };
                        return Ok(Some(
                            self.ast.type_exprs.add(ParsedTypeExpr::Array(array_type)),
                        ));
                    }
                }

                // parameterized, namespaced type. Examples:
                // int,
                // Box[Point],
                // std::Map[int, int]
                let (type_params, type_params_span) = self.parse_bracketed_type_args()?;
                let span = self.extend_span(first.span, type_params_span);
                Ok(Some(self.ast.type_exprs.add(ParsedTypeExpr::TypeApplication(
                    TypeApplication { name: base_name, args: type_params, span },
                ))))
            }
        } else if first.kind == K::OpenBrace {
            let open_brace = self.expect_eat_token(K::OpenBrace)?;
            let mut fields = eco_vec![];
            let (fields_span, _) = self.eat_delimited_ext(
                "Struct fields",
                &mut fields,
                K::Comma,
                &[K::CloseBrace],
                |p| {
                    let field_res = Parser::parse_struct_type_field(p);
                    Parser::expect("Struct field", open_brace, field_res)
                },
            )?;
            let span = self.extend_span(first.span, fields_span);
            let struc = StructType { fields, span };
            Ok(Some(self.ast.type_exprs.add(ParsedTypeExpr::Struct(struc))))
        } else {
            Ok(None)
        }
    }

    fn expect_function_type(&mut self) -> ParseResult<ParsedTypeExprId> {
        let start = self.expect_eat_token(K::BackSlash)?;
        let mut params: SmallVec<[ParsedTypeExprId; 8]> = smallvec![];
        let open_paren = self.maybe_consume_next(K::OpenParen).is_some();
        let loop_end_kind = if open_paren { K::CloseParen } else { K::RThinArrow };
        let no_params = open_paren && self.peek().kind == K::CloseParen;
        if no_params {
            self.advance();
        } else {
            loop {
                let expr = self.expect_type_expression()?;
                params.push(expr);
                if self.peek().kind == loop_end_kind {
                    self.advance();
                    break;
                } else {
                    // If not terminated, expect a separator
                    self.expect_eat_token(K::Comma)?;
                }
            }
        }
        if open_paren {
            self.expect_eat_token(K::RThinArrow)?;
        }

        let return_type = self.expect_type_expression()?;
        let span = self.extend_span(start.span, self.get_type_expression_span(return_type));
        let function_type = ParsedFunctionType { params, return_type, span };
        Ok(self.ast.type_exprs.add(ParsedTypeExpr::Function(function_type)))
    }

    fn expect_enum_type_expression(&mut self) -> ParseResult<ParsedEnumType> {
        let keyword = self.expect_eat_token(K::KeywordEither)?;
        let explicit_tag_type_expr =
            if self.maybe_consume_next_no_whitespace(K::OpenParen).is_some() {
                let tag_expr = self.expect_type_expression()?;
                self.expect_eat_token(K::CloseParen)?;
                Some(tag_expr)
            } else {
                None
            };
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
                Some(expr) => self.ast.type_exprs.get(expr).get_span(),
            };
            variants.push(ParsedEnumVariant { tag_name, payload_expression, span });
            first = false;
        }
        let last_variant_span =
            variants.last().ok_or_else(|| error_expected("At least one variant", keyword))?.span;
        let span = self.extend_span(keyword.span, last_variant_span);
        Ok(ParsedEnumType { variants, tag_type: explicit_tag_type_expr, span })
    }

    fn expect_fn_arg(&mut self, is_explicit_context: bool) -> ParseResult<ParsedCallArg> {
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
        Ok(ParsedCallArg { name, value: expr, is_explicit_context })
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

    fn parse_struct_value(&mut self) -> ParseResult<Option<ParsedStruct>> {
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

        let (fields, fields_span) =
            self.eat_delimited("Struct", K::Comma, &[K::CloseBrace], Parser::expect_struct_field)?;
        let span = self.extend_span(first.span, fields_span);
        Ok(Some(ParsedStruct { fields, span }))
    }

    fn parse_expression_with_postfix_ops(&mut self) -> ParseResult<Option<ParsedExprId>> {
        let Some(mut result) = self.parse_base_expression()? else { return Ok(None) };
        // Looping for postfix ops inspired by Jakt's parser
        let with_postfix: ParsedExprId = loop {
            let (next, second) = self.peek_two();
            let new_result = if next.kind == K::KeywordAs {
                self.advance();
                let type_expr_id = self.expect_type_expression()?;
                let span = self.extend_span(
                    self.get_expression_span(result),
                    self.ast.get_type_expr_span(type_expr_id),
                );
                Some(self.add_expression(ParsedExpr::Cast(ParsedCast {
                    base_expr: result,
                    dest_type: type_expr_id,
                    span,
                })))
            } else if next.kind == K::KeywordIs {
                self.advance();
                let pattern = self.expect_parse_pattern()?;

                let original_span = self.get_expression_span(result);
                let span = self.extend_to_here(original_span);
                let is_expression_id = self.add_expression(ParsedExpr::Is(ParsedIsExpr {
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
                        ));
                    }
                };
                let (type_args, _) = self.parse_bracketed_type_args()?;
                let next = self.peek();
                // a.b[int](...)
                if next.kind == K::OpenParen {
                    let (mut args, args_span) = self.expect_fn_call_args()?;
                    let self_arg = result;
                    let span = self.extend_span(self.get_expression_span(self_arg), args_span);
                    let name = self.intern_ident_token(target);

                    //let args_iter =
                    //    [ParsedCallArg { name: None, value: self_arg, is_explicit_context: false }]
                    //        .into_iter()
                    //        .chain(args);

                    let index_of_first_explicit_arg =
                        args.iter().position(|a| !a.is_explicit_context).unwrap_or(args.len());
                    args.insert(
                        index_of_first_explicit_arg,
                        ParsedCallArg { name: None, value: self_arg, is_explicit_context: false },
                    );
                    let args_handle = self.ast.p_call_args.add_slice_from_copy_slice(&args);

                    Some(self.add_expression(ParsedExpr::Call(ParsedCall {
                        name: NamespacedIdentifier::naked(name, target.span),
                        type_args,
                        args: args_handle,
                        span,
                        is_method: true,
                        id: ParsedExprId::PENDING,
                    })))
                } else {
                    // a.b[int] <complete expression>
                    let trailing_asterisk = self.maybe_consume_next_no_whitespace(K::Asterisk);
                    let target = self.intern_ident_token(target);
                    let span = self.extend_to_here(self.get_expression_span(result));
                    Some(self.add_expression(ParsedExpr::FieldAccess(FieldAccess {
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
            self.ast.exprs.set_type_hint(with_postfix, type_hint);
        }
        Ok(Some(with_postfix))
    }

    pub fn expect_block(&mut self, kind: ParsedBlockKind) -> ParseResult<ParsedBlock> {
        Parser::expect("block", self.peek(), self.parse_block(kind))
    }

    pub fn expect_expression(&mut self) -> ParseResult<ParsedExprId> {
        Parser::expect("expression", self.peek(), self.parse_expression())
    }

    pub fn parse_expression(&mut self) -> ParseResult<Option<ParsedExprId>> {
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
                    let bin_op = self.add_expression(ParsedExpr::BinaryOp(BinaryOp {
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
                let bin_op = self.add_expression(ParsedExpr::BinaryOp(BinaryOp {
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

    fn extend_expr_span(&mut self, expr1: ParsedExprId, expr2: ParsedExprId) -> SpanId {
        self.extend_span(self.get_expression_span(expr1), self.get_expression_span(expr2))
    }

    fn parse_bracketed_type_args(&mut self) -> ParseResult<(SliceHandle<NamedTypeArgId>, SpanId)> {
        let Some(open_bracket) = self.maybe_consume_next(K::OpenBracket) else {
            return Ok((SliceHandle::Empty, self.peek_back().span));
        };

        let mut type_args: SmallVec<[NamedTypeArg; 8]> = SmallVec::new();
        let (args_span, _terminator) = self.eat_delimited_ext(
            "Type Arguments",
            &mut type_args,
            K::Comma,
            &[K::CloseBracket],
            |p| {
                let (one, two) = p.peek_two();
                let name = if one.kind == K::Ident && two.kind == K::Equals {
                    let (_, name_ident) = p.expect_ident()?;
                    p.expect_eat_token(K::Equals)?;
                    Some(name_ident)
                } else {
                    None
                };
                let type_expr = p.expect_type_expression()?;
                let span = p.extend_span(one.span, p.get_type_expression_span(type_expr));
                Ok(NamedTypeArg { name, type_expr, span })
            },
        )?;
        let slice = self.ast.p_type_args.add_slice_from_copy_slice(&type_args);
        let span = self.extend_span(open_bracket.span, args_span);
        Ok((slice, span))
    }

    fn expect_namespaced_ident(&mut self) -> ParseResult<NamespacedIdentifier> {
        let (first, second) = self.tokens.peek_two();
        let mut namespaces = EcoVec::new();
        if second.kind == K::Slash && !second.is_whitespace_preceeded() {
            // Namespaced expression; foo/
            // Loop until we don't see a /
            namespaces.push(self.intern_ident_token(first));
            self.advance(); // ident
            self.advance(); // slash
            loop {
                let (a, b) = self.tokens.peek_two();
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
    /// or a 'base case'; it doesn't have any real meaning at the language level
    fn parse_base_expression(&mut self) -> ParseResult<Option<ParsedExprId>> {
        let directives = self.parse_directives()?;
        let (first, second, third) = self.tokens.peek_three();
        trace!("parse_base_expression {} {} {}", first.kind, second.kind, third.kind);
        let resulting_expression = if first.kind == K::OpenParen {
            self.advance();
            if self.peek().kind == K::CloseParen {
                let end = self.tokens.next();
                let span = self.extend_token_span(first, end);
                Ok(Some(self.add_expression(ParsedExpr::Literal(ParsedLiteral::Unit(span)))))
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
            Ok(Some(self.add_expression(ParsedExpr::While(while_result))))
        } else if first.kind == K::KeywordLoop {
            self.advance();
            let body = self.expect_block(ParsedBlockKind::LoopBody)?;
            let span = self.extend_span(first.span, body.span);
            Ok(Some(self.add_expression(ParsedExpr::Loop(ParsedLoopExpr { body, span }))))
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
            Ok(Some(self.add_expression(ParsedExpr::Match(match_expr))))
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
            let body_expr = self.expect_block(ParsedBlockKind::LoopBody)?;
            let span = self.extend_span(first.span, body_expr.span);
            Ok(Some(self.add_expression(ParsedExpr::For(ForExpr {
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
            Ok(Some(self.add_expression(ParsedExpr::UnaryOp(UnaryOp { expr, op_kind, span }))))
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
                Ok(Some(self.add_expression(ParsedExpr::AnonEnumConstructor(
                    AnonEnumConstructor { variant_name, payload: Some(payload), span },
                ))))
            } else {
                // Tag Literal
                let span = self.extend_token_span(first, second);
                Ok(Some(self.add_expression(ParsedExpr::AnonEnumConstructor(
                    AnonEnumConstructor { variant_name, payload: None, span },
                ))))
            }
        } else if let Some(literal_id) = self.parse_literal_atom()? {
            Ok(Some(literal_id))
        } else if first.kind == K::Ident {
            let namespaced_ident = self.expect_namespaced_ident()?;
            let second = self.tokens.peek();
            // FnCall
            if second.kind == K::At || second.kind == K::OpenBracket || second.kind == K::OpenParen
            {
                let first_type_args = match second.kind {
                    K::OpenBracket => self.parse_bracketed_type_args()?.0,
                    K::At => SliceHandle::Empty,
                    K::OpenParen => SliceHandle::Empty,
                    _ => unreachable!(),
                };
                let next_after_tparams = self.peek();
                match next_after_tparams.kind {
                    K::OpenParen => {
                        // Call with type params above
                        let (args, args_span) = self.expect_fn_call_args()?;
                        let args_handle =
                            self.ast.p_call_args.add_slice_from_iter(args.into_iter());
                        let span = self.extend_span(namespaced_ident.span, args_span);
                        Ok(Some(self.add_expression(ParsedExpr::Call(ParsedCall {
                            name: namespaced_ident,
                            type_args: first_type_args,
                            args: args_handle,
                            span,
                            is_method: false,
                            id: ParsedExprId::PENDING,
                        }))))
                    }
                    K::At => {
                        // Qualified ability call with first_type_args: Ability[<first_type_args>]@(int)/baz()
                        self.advance();
                        self.expect_eat_token(K::OpenParen)?;
                        let target_type = self.expect_type_expression()?;
                        self.expect_eat_token(K::CloseParen)?;
                        self.expect_eat_token(K::Slash)?;
                        let (call_name_token, call_name) = self.expect_ident()?;
                        let (call_type_args, _) = self.parse_bracketed_type_args()?;
                        let (args, _) = self.expect_fn_call_args()?;
                        let args_handle =
                            self.ast.p_call_args.add_slice_from_iter(args.into_iter());

                        let span = self.extend_to_here(first.span);

                        let call_expr_id = self.add_expression(ParsedExpr::Call(ParsedCall {
                            name: NamespacedIdentifier::naked(call_name, call_name_token.span),
                            type_args: call_type_args,
                            args: args_handle,
                            span,
                            is_method: false,
                            id: ParsedExprId::PENDING,
                        }));

                        let ability_type_arguments = first_type_args;
                        let ab_expr_span = self.extend_to_here(namespaced_ident.span);
                        let ability_expr_id = self.ast.p_ability_exprs.add(ParsedAbilityExpr {
                            name: namespaced_ident,
                            arguments: ability_type_arguments,
                            span: ab_expr_span,
                        });
                        Ok(Some(self.add_expression(ParsedExpr::QualifiedAbilityCall(
                            ParsedQAbilityCall {
                                ability_expr: ability_expr_id,
                                self_name: target_type,
                                call_expr: call_expr_id,
                                span,
                            },
                        ))))
                    }
                    _ => Err(error(
                        "Expected '(' for function call; or '@' for qualified ability impl",
                        self.peek(),
                    )),
                }
            } else {
                // The last thing it can be is a simple variable reference expression
                Ok(Some(self.add_expression(ParsedExpr::Variable(ParsedVariable {
                    name: namespaced_ident,
                }))))
            }
        } else if first.kind == K::OpenBrace {
            // The syntax {} means empty struct, not empty block
            // If you want an block, use a unit block { () }
            trace!("parse_expr {:?} {:?} {:?}", first, second, third);
            if let Some(struct_value) = self.parse_struct_value()? {
                Ok(Some(self.add_expression(ParsedExpr::Struct(struct_value))))
            } else {
                match self.parse_block(ParsedBlockKind::LexicalBlock)? {
                    None => Err(error_expected("block", self.peek())),
                    Some(block) => Ok(Some(self.add_expression(ParsedExpr::Block(block)))),
                }
            }
        } else if first.kind == K::KeywordIf {
            let if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
            Ok(Some(self.add_expression(ParsedExpr::If(if_expr))))
        } else if first.kind == K::OpenBracket {
            // list
            let start = self.expect_eat_token(K::OpenBracket)?;
            let mut elements = eco_vec![];
            let (elements_span, _terminator) = self.eat_delimited_ext(
                "list elements",
                &mut elements,
                TokenKind::Comma,
                &[TokenKind::CloseBracket],
                |p| Parser::expect("expression", start, p.parse_expression()),
            )?;
            let span = self.extend_span(start.span, elements_span);
            Ok(Some(self.add_expression(ParsedExpr::ListLiteral(ListExpr { elements, span }))))
        } else if first.kind == K::Hash {
            self.advance();

            let maybe_directive = self.peek();
            match maybe_directive.kind {
                K::Ident if !maybe_directive.is_whitespace_preceeded() => {
                    let chars = self.token_chars(maybe_directive);
                    match chars {
                        "meta" | "meat" | "static" => {
                            let kind = match chars {
                                "meta" | "meat" => ParsedStaticBlockKind::Metaprogram,
                                "static" => ParsedStaticBlockKind::Value,
                                _ => unreachable!(),
                            };
                            self.advance();
                            let mut parameter_names: SV4<Ident> = smallvec![];
                            if let Some(_open_token) =
                                self.maybe_consume_next_no_whitespace(K::OpenParen)
                            {
                                self.eat_delimited_ext(
                                    "params",
                                    &mut parameter_names,
                                    K::Comma,
                                    &[K::CloseParen],
                                    |p| {
                                        Parser::expect_ident_ext(p, false, false)
                                            .map(|(_token, ident)| ident)
                                    },
                                )?;
                            }
                            let parameter_names_handle =
                                self.ast.p_idents.add_slice_from_copy_slice(&parameter_names);
                            let base_expr = self.expect_expression()?;
                            let expr_span = self.get_expression_span(base_expr);
                            let span = self.extend_span(first.span, expr_span);
                            Ok(Some(self.add_expression(ParsedExpr::Static(ParsedStaticExpr {
                                base_expr,
                                kind,
                                parameter_names: parameter_names_handle,
                                span,
                            }))))
                        }
                        "code" => {
                            self.advance();
                            let parsed_stmt = self.expect_statement()?;
                            let stmt_span = self.ast.get_stmt_span(parsed_stmt);
                            let span = self.extend_span(first.span, stmt_span);
                            Ok(Some(self.add_expression(ParsedExpr::Code(ParsedCode {
                                parsed_stmt,
                                span,
                            }))))
                        }
                        _ => Err(error("Unknown directive following #", self.peek())),
                    }
                }
                K::KeywordIf => {
                    let mut if_expr = Parser::expect("If Expression", first, self.parse_if_expr())?;
                    if_expr.is_static = true;
                    Ok(Some(self.add_expression(ParsedExpr::If(if_expr))))
                }
                _ => Err(error("Unknown directive following #", self.peek())),
            }
        } else {
            // More expression types
            if directives.is_empty() {
                Ok(None)
            } else {
                Err(error_expected("expression following directives", first))
            }
        }?;
        if let Some(expression_id) = resulting_expression {
            self.ast.exprs.add_directives(expression_id, directives);

            if self.peek().kind == K::Colon {
                self.advance();
                let type_hint = self.expect_type_expression()?;
                self.ast.exprs.set_type_hint(expression_id, type_hint);
            }
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

    fn expect_lambda(&mut self) -> ParseResult<ParsedExprId> {
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
        Ok(self.add_expression(ParsedExpr::Lambda(lambda)))
    }

    fn expect_fn_call_args(&mut self) -> ParseResult<(SmallVec<[ParsedCallArg; 8]>, SpanId)> {
        let (first, second) = self.tokens.peek_two();
        let is_context = second.kind == K::KeywordContext;

        let mut args: SmallVec<[ParsedCallArg; 8]> = smallvec![];
        if is_context {
            self.expect_eat_token(K::OpenParen)?;
            self.expect_eat_token(K::KeywordContext)?;
            self.eat_delimited_ext(
                "Function context arguments",
                &mut args,
                K::Comma,
                &[K::CloseParen],
                |p| p.expect_fn_arg(true),
            )?;
        };
        let args_span = self.eat_delimited_expect_opener(
            "Function arguments",
            &mut args,
            K::OpenParen,
            K::Comma,
            &[K::CloseParen],
            |p| p.expect_fn_arg(false),
        )?;
        let span = self.extend_span(first.span, args_span);
        Ok((args, span))
    }

    fn parse_let(&mut self) -> ParseResult<Option<ParsedLet>> {
        trace!("parse_let");

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
        Ok(Some(ParsedLet {
            name: self.intern_ident_token(name_token),
            type_expr: typ,
            value: initializer_expression,
            flags: ParsedLet::make_flags(is_mutable, is_context, is_reference),
            span,
        }))
    }

    fn parse_require(&mut self) -> ParseResult<Option<ParsedRequire>> {
        let Some(keyword_require_token) = self.maybe_consume_next(K::KeywordRequire) else {
            return Ok(None);
        };

        let condition_expr = self.expect_expression()?;
        self.expect_eat_token(K::KeywordElse)?;

        let else_body = self.expect_expression()?;

        let span = self.extend_to_here(keyword_require_token.span);
        Ok(Some(ParsedRequire { condition_expr, else_body, span }))
    }

    fn parse_global(&mut self) -> ParseResult<Option<ParsedGlobalId>> {
        trace!("parse_global");
        let Some(keyword_let_token) = self.maybe_consume_next(K::KeywordLet) else {
            return Ok(None);
        };
        let is_referencing = self.maybe_consume_next_no_whitespace(K::Asterisk).is_some();
        let is_mutable = self.maybe_consume_next(K::KeywordMut).is_some();
        let name_token = self.expect_eat_token(K::Ident)?;
        let _colon = self.expect_eat_token(K::Colon);
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        self.expect_eat_token(K::Equals)?;
        let value_expr = Parser::expect("expression", self.peek(), self.parse_expression())?;
        let span = self.extend_span(keyword_let_token.span, self.get_expression_span(value_expr));
        let name = self.intern_ident_token(name_token);
        let constant_id = self.ast.add_global(ParsedGlobal {
            name,
            ty: typ,
            value_expr,
            span,
            id: ParsedGlobalId::PENDING,
            is_mutable,
            is_referencing,
        });
        Ok(Some(constant_id))
    }

    fn expect_assignment(&mut self, lhs: ParsedExprId) -> ParseResult<Assignment> {
        self.expect_eat_token(K::Equals)?;
        let rhs = self.expect_expression()?;
        let span = self.extend_expr_span(lhs, rhs);
        Ok(Assignment { lhs, rhs, span })
    }

    fn expect_set_stmt(&mut self, lhs: ParsedExprId) -> ParseResult<SetStmt> {
        self.expect_eat_token(K::LThinArrow)?;
        let rhs = self.expect_expression()?;
        let span = self.extend_expr_span(lhs, rhs);
        Ok(SetStmt { lhs, rhs, span })
    }

    fn eat_fn_param(&mut self, is_context: bool) -> ParseResult<FnArgDef> {
        trace!("eat_fn_arg_def");
        let name_token = self.expect_eat_token(K::Ident)?;
        self.expect_eat_token(K::Colon)?;
        let typ = Parser::expect("type_expression", self.peek(), self.parse_type_expression())?;
        let span = self.extend_span(name_token.span, self.ast.type_exprs.get(typ).get_span());
        let modifiers = FnArgDefModifiers::new(is_context);
        Ok(FnArgDef { name: self.intern_ident_token(name_token), type_expr: typ, span, modifiers })
    }

    fn eat_fn_params(&mut self) -> ParseResult<(SV8<FnArgDef>, SpanId)> {
        let (first, next) = self.tokens.peek_two();
        let is_context = next.kind == K::KeywordContext;
        let mut params: SmallVec<[FnArgDef; 8]> = smallvec![];
        if is_context {
            self.expect_eat_token(K::OpenParen)?;
            self.expect_eat_token(K::KeywordContext)?;
            self.eat_delimited_ext(
                "Function context parameters",
                &mut params,
                K::Comma,
                &[K::CloseParen],
                |p| Parser::eat_fn_param(p, true),
            )?;
        };
        let fn_params_span = self.eat_delimited_expect_opener(
            "Function parameters",
            &mut params,
            K::OpenParen,
            K::Comma,
            &[K::CloseParen],
            |p| Parser::eat_fn_param(p, false),
        )?;
        let span = self.extend_span(first.span, fn_params_span);
        Ok((params, span))
    }

    fn eat_delimited_if_opener<T, F>(
        &mut self,
        name: &str,
        destination: &mut impl CanPush<T>,
        opener: TokenKind,
        delim: TokenKind,
        terminators: &[TokenKind],
        parse: F,
    ) -> ParseResult<Option<SpanId>>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        let next = self.peek();
        if next.kind == opener {
            self.advance();
            let (result_span, _) =
                self.eat_delimited_ext(name, destination, delim, terminators, parse)?;
            let span = self.extend_span(next.span, result_span);
            Ok(Some(span))
        } else {
            Ok(None)
        }
    }

    fn eat_delimited_expect_opener<T, F>(
        &mut self,
        name: &str,
        destination: &mut impl CanPush<T>,
        opener: TokenKind,
        delim: TokenKind,
        terminators: &'static [TokenKind],
        parse: F,
    ) -> ParseResult<SpanId>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        match self.eat_delimited_if_opener(name, destination, opener, delim, terminators, parse) {
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
        destination: &mut impl CanPush<T>,
        delim: TokenKind,
        terminators: &[TokenKind],
        parse: F,
    ) -> ParseResult<(SpanId, Token)>
    where
        F: Fn(&mut Parser<'toks, 'module>) -> ParseResult<T>,
    {
        let start_span = self.peek().span;

        loop {
            let terminator = self.peek();
            if terminators.contains(&terminator.kind) {
                self.advance();
                let span = self.ast.spans.extend(start_span, terminator.span);
                break Ok((span, terminator));
            }
            let parsed = parse(self)?;

            destination.push_it(parsed);

            let terminator = self.peek();
            if terminators.contains(&terminator.kind) {
                self.advance();
                let span = self.ast.spans.extend(start_span, terminator.span);
                break Ok((span, terminator));
            }
            let found_delim = self.maybe_consume_next(delim);
            if found_delim.is_none() {
                break Err(error(
                    format!("Expected delimiter '{delim}' while parsing {name}"),
                    self.peek(),
                ));
            }
        }
    }

    fn parse_if_expr(&mut self) -> ParseResult<Option<ParsedIfExpr>> {
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
        let if_expr = ParsedIfExpr {
            cond: condition_expr,
            cons: consequent_expr,
            alt,
            span,
            is_static: false,
        };
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

    pub fn expect_statement(&mut self) -> ParseResult<ParsedStmtId> {
        Parser::expect("statement", self.peek(), self.parse_statement())
    }

    pub fn parse_statement(&mut self) -> ParseResult<Option<ParsedStmtId>> {
        trace!("parse_statement {:?}", self.peek());
        if let Some(use_id) = self.parse_use()? {
            let span = self.ast.uses.get_use(use_id).span;
            Ok(Some(self.ast.stmts.add(ParsedStmt::Use(UseStmt { span, use_id }))))
        } else if let Some(let_stmt) = self.parse_let()? {
            Ok(Some(self.ast.stmts.add(ParsedStmt::Let(let_stmt))))
        } else if let Some(require_stmt) = self.parse_require()? {
            Ok(Some(self.ast.stmts.add(ParsedStmt::Require(require_stmt))))
        } else if let Some(expr) = self.parse_expression()? {
            let peeked = self.peek();
            // Assignment:
            // - Validate expr type, since only some exprs can be LHS of an assignment
            // - Build assignment
            if peeked.kind == K::Equals {
                let assgn = self.expect_assignment(expr)?;
                Ok(Some(self.ast.stmts.add(ParsedStmt::Assignment(assgn))))
            } else if peeked.kind == K::LThinArrow {
                let set = self.expect_set_stmt(expr)?;
                Ok(Some(self.ast.stmts.add(ParsedStmt::SetRef(set))))
            } else {
                Ok(Some(self.ast.stmts.add(ParsedStmt::LoneExpression(expr))))
            }
        } else {
            Ok(None)
        }
    }

    pub fn parse_block(&mut self, kind: ParsedBlockKind) -> ParseResult<Option<ParsedBlock>> {
        let Some(block_start) = self.maybe_consume_next(K::OpenBrace) else {
            return Ok(None);
        };
        let parse_statement =
            |p: &mut Parser| Parser::expect("statement", p.peek(), Parser::parse_statement(p));
        let mut stmts = eco_vec![];
        let (statements_span, _) = self.eat_delimited_ext(
            "Block statements",
            &mut stmts,
            K::Semicolon,
            &[K::CloseBrace],
            parse_statement,
        )?;
        let span = self.extend_span(block_start.span, statements_span);
        Ok(Some(ParsedBlock { stmts, kind, span }))
    }

    fn parse_type_constraints(&mut self) -> ParseResult<EcoVec<ParsedTypeConstraintExpr>> {
        let mut constraints = eco_vec![];
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
        Ok(constraints)
    }

    fn expect_type_param(&mut self) -> ParseResult<ParsedTypeParam> {
        // fn foo[T: Into[bool] and Into[Baz], U: Into[int]]
        //        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //           ^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^
        //           constraints
        let name_token = self.expect_eat_token(K::Ident)?;
        let name = self.intern_ident_token(name_token);
        let constraints = self.parse_type_constraints()?;
        let span = self.extend_to_here(name_token.span);
        Ok(ParsedTypeParam { name, span, constraints })
    }

    /// Directives look like this: @<directive kind: ident>(<directive arg>, ...)
    fn parse_directives(&mut self) -> ParseResult<EcoVec<ParsedDirective>> {
        let mut directives: EcoVec<ParsedDirective> = eco_vec![];
        while let Some(_at) = self.maybe_consume_next(K::At) {
            let directive = {
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

    fn parse_function(
        &mut self,
        preexisting_condition: Option<ParsedExprId>,
    ) -> ParseResult<Option<ParsedFunctionId>> {
        trace!("parse_function");
        let directives = self.parse_directives()?;
        let condition = match preexisting_condition {
            None => {
                if self.maybe_consume_next(K::Hash).is_some() {
                    self.expect_eat_token(K::KeywordIf)?;
                    let condition_expr = self.expect_expression()?;
                    Some(condition_expr)
                } else {
                    None
                }
            }
            Some(condition) => Some(condition),
        };
        let initial_pos = self.cursor_position();
        let is_intrinsic = self.maybe_consume_next(K::KeywordIntern).is_some();
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
        let type_arguments: EcoVec<ParsedTypeParam> =
            if self.maybe_consume_next(K::OpenBracket).is_some() {
                let mut type_args = eco_vec![];
                let _ = self.eat_delimited_ext(
                    "Type arguments",
                    &mut type_args,
                    TokenKind::Comma,
                    &[TokenKind::CloseBracket],
                    |p| p.expect_type_param(),
                )?;
                type_args
            } else {
                eco_vec![]
            };
        let (params, params_span) = self.eat_fn_params()?;
        self.expect_eat_token(K::Colon)?;
        let ret_type = self.expect_type_expression()?;
        let additional_type_constraints = if self.maybe_consume_next(K::KeywordWhere).is_some() {
            // FIXME(brittle parsing): Has to backtrack to un-consume the next token in the fn call;
            // the open brace
            let mut additional_type_constraints = eco_vec![];
            let _ = self.eat_delimited_ext(
                "Type variable constraints",
                &mut additional_type_constraints,
                K::Comma,
                &[K::OpenBrace],
                Parser::expect_named_type_constraint,
            )?;
            self.tokens.retreat(); // Un-eat the close sentinel
            additional_type_constraints
        } else {
            eco_vec![]
        };
        let signature_span = self.extend_to_here(func_name.span);
        let block = self.parse_block(ParsedBlockKind::FunctionBody)?;
        let end_span = block.as_ref().map(|b| b.span).unwrap_or(params_span);
        let block = match block {
            None => None,
            Some(block) => Some(self.add_expression(ParsedExpr::Block(block))),
        };
        let span = self.extend_span(fn_keyword.span, end_span);
        let function_id = self.ast.add_function(ParsedFunction {
            name: func_name_id,
            type_params: type_arguments,
            // TODO(perf, efficient ast): Migrate params and context_params to a single SliceHandle
            params: params.clone().into_iter().filter(|a| !a.modifiers.is_context()).collect(),
            context_params: params
                .clone()
                .into_iter()
                .filter(|a| a.modifiers.is_context())
                .collect(),
            ret_type,
            block,
            signature_span,
            span,
            linkage,
            directives,
            additional_where_constraints: additional_type_constraints,
            condition,
            id: ParsedFunctionId(u32::MAX),
        });
        Ok(Some(function_id))
    }

    fn expect_type_constraint_expr(&mut self) -> ParseResult<ParsedTypeConstraintExpr> {
        let next = self.peek();
        if next.kind == K::Ident && self.token_chars(next) == "static" {
            let static_type_expr = self.expect_type_expression()?;
            Ok(ParsedTypeConstraintExpr::Static(static_type_expr))
        } else {
            let ability_expr = self.expect_ability_expr()?;
            let id = self.ast.p_ability_exprs.add(ability_expr);
            Ok(ParsedTypeConstraintExpr::Ability(id))
        }
    }

    fn expect_named_type_constraint(&mut self) -> ParseResult<ParsedTypeConstraint> {
        let (name_token, name) = self.expect_ident_upper()?;
        self.expect_eat_token(K::Colon)?;
        let constraint_expr = self.expect_type_constraint_expr()?;
        let span = self.extend_to_here(name_token.span);
        Ok(ParsedTypeConstraint { name, constraint_expr, span })
    }

    fn expect_ident_ext(&mut self, upper: bool, lower: bool) -> ParseResult<(Token, Ident)> {
        let token = self.expect_eat_token(K::Ident)?;
        let tok_chars =
            Parser::tok_chars(&self.ast.spans, self.ast.sources.get_source(self.file_id), token);
        if upper && !tok_chars.chars().next().unwrap().is_uppercase() {
            return Err(error("This name must be capitalized", token));
        }
        if lower && !tok_chars.chars().next().unwrap().is_lowercase() {
            return Err(error("This name must not be capitalized", token));
        }
        Ok((token, self.ast.idents.intern(tok_chars)))
    }

    fn expect_ident_upper(&mut self) -> ParseResult<(Token, Ident)> {
        self.expect_ident_ext(true, false)
    }

    fn expect_ident(&mut self) -> ParseResult<(Token, Ident)> {
        self.expect_ident_ext(false, false)
    }

    fn parse_ability_defn(&mut self) -> ParseResult<Option<ParsedAbilityId>> {
        fn expect_ability_type_param(p: &mut Parser) -> ParseResult<ParsedAbilityParameter> {
            let start = p.peek().span;
            let is_impl_param = p.maybe_consume_next(K::KeywordImpl).is_some();
            let name_token = p.expect_eat_token(K::Ident)?;
            let name = p.intern_ident_token(name_token);
            let constraints = p.parse_type_constraints()?;
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
        while let Some(parsed_function) = self.parse_function(None)? {
            functions.push(parsed_function);
        }
        let close_token = self.expect_eat_token(K::CloseBrace)?;
        let span = self.extend_token_span(keyword_ability, close_token);
        let ability_id = self.ast.add_ability(ParsedAbility {
            name: name_identifier,
            functions,
            params: ability_params,
            span,
            id: ParsedAbilityId(0),
        });
        Ok(Some(ability_id))
    }

    fn expect_ability_type_argument(&mut self) -> ParseResult<NamedTypeArg> {
        let name_token = self.expect_eat_token(K::Ident)?;
        let name = self.intern_ident_token(name_token);
        self.expect_eat_token(K::Equals)?;
        let value = self.expect_type_expression()?;
        let span = self.extend_span(name_token.span, self.get_type_expression_span(value));
        Ok(NamedTypeArg { name: Some(name), type_expr: value, span })
    }

    fn expect_ability_expr(&mut self) -> ParseResult<ParsedAbilityExpr> {
        let name = self.expect_namespaced_ident()?;
        let mut arguments: SV4<NamedTypeArg> = smallvec![];
        let span = match self.eat_delimited_if_opener(
            "Ability Arguments",
            &mut arguments,
            K::OpenBracket,
            K::Comma,
            &[K::CloseBracket],
            Parser::expect_ability_type_argument,
        )? {
            None => name.span,
            Some(args_span) => self.extend_span(name.span, args_span),
        };
        let arguments_handle = self.ast.p_type_args.add_slice_from_copy_slice(&arguments);
        Ok(ParsedAbilityExpr { name, arguments: arguments_handle, span })
    }

    fn parse_ability_impl(&mut self) -> ParseResult<Option<ParsedAbilityImplId>> {
        let Some(keyword_impl) = self.maybe_consume_next(K::KeywordImpl) else { return Ok(None) };
        let mut generic_impl_params = eco_vec![];
        self.eat_delimited_if_opener(
            "Generic implementation parameters",
            &mut generic_impl_params,
            K::OpenBracket,
            K::Comma,
            &[K::CloseBracket],
            |p| p.expect_type_param(),
        )?;
        let ability = self.expect_ability_expr()?;
        let ability_expr_id = self.ast.p_ability_exprs.add(ability);
        self.expect_eat_token(K::KeywordFor)?;
        let target_type = self.expect_type_expression()?;

        // Read the functions inside block; one day also associated constants
        let mut functions = eco_vec![];
        self.expect_eat_token(K::OpenBrace)?;

        while let Some(parsed_function) = self.parse_function(None)? {
            functions.push(parsed_function);
        }

        let close_brace = self.expect_eat_token(K::CloseBrace)?;
        let span = self.extend_token_span(keyword_impl, close_brace);

        let ability_impl_id = self.ast.add_ability_impl(ParsedAbilityImplementation {
            ability_expr: ability_expr_id,
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
        let mut flags = ParsedTypeDefnFlags::new(false);
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
        let span = self.extend_span(keyword_type.span, self.ast.get_type_expr_span(type_expr));
        let name = self.intern_ident_token(name);
        let type_defn_id = self.ast.add_typedefn(ParsedTypeDefn {
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
        let mut definitions = EcoVec::new();
        let terminator = if is_braced { K::CloseBrace } else { K::Eof };
        while let Some(def) = self.parse_definition(terminator)? {
            definitions.push(def);
        }

        let name = self.intern_ident_token(ident);
        let span = self.extend_to_here(keyword.span);
        let namespace_id = self.ast.add_namespace(ParsedNamespace {
            name,
            definitions,
            id: ParsedNamespaceId::ONE,
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
            self.ast.uses.add_use(ParsedUse { target: namespaced_ident, alias, span });
        Ok(Some(parsed_use_id))
    }
}

// Display
impl ParsedProgram {
    fn get_string(&self, id: StringId) -> &str {
        self.strings.get_string(id)
    }

    pub fn expr_id_to_string(&self, expr: ParsedExprId) -> String {
        let mut buffer = String::new();
        self.display_expr_id(&mut buffer, expr).unwrap();
        buffer
    }

    pub fn display_expr_id(&self, w: &mut impl Write, expr: ParsedExprId) -> std::fmt::Result {
        match self.exprs.get(expr) {
            ParsedExpr::Builtin(_span) => w.write_str("builtin"),
            ParsedExpr::BinaryOp(op) => {
                w.write_str("(")?;
                self.display_expr_id(w, op.lhs)?;
                write!(w, " {} ", op.op_kind)?;
                self.display_expr_id(w, op.rhs)?;
                w.write_str(")")
            }
            ParsedExpr::UnaryOp(op) => {
                write!(w, "{}", op.op_kind)?;
                self.display_expr_id(w, op.expr)
            }
            ParsedExpr::Literal(lit) => w.write_fmt(format_args!("{}", lit)),
            ParsedExpr::InterpolatedString(is) => {
                w.write_char('"')?;
                for part in &is.parts {
                    match part {
                        InterpolatedStringPart::String(s) => w.write_str(self.get_string(*s))?,
                        InterpolatedStringPart::Expr(expr_id) => {
                            w.write_char('{')?;
                            self.display_expr_id(w, *expr_id)?;
                            w.write_char('{')?;
                        }
                    }
                }
                w.write_char('"')?;
                Ok(())
            }
            ParsedExpr::Call(call) => {
                w.write_str(self.idents.get_name(call.name.name))?;
                w.write_str("(...)")?;
                Ok(())
            }
            ParsedExpr::Variable(var) => {
                w.write_str("var#")?;
                w.write_str(self.idents.get_name(var.name.name))?;
                Ok(())
            }
            ParsedExpr::FieldAccess(acc) => {
                self.display_expr_id(w, acc.base)?;
                w.write_str(".")?;
                w.write_str(self.idents.get_name(acc.field_name))?;
                Ok(())
            }
            ParsedExpr::Block(block) => w.write_fmt(format_args!("{:?}", block)),
            ParsedExpr::If(if_expr) => w.write_fmt(format_args!("{:?}", if_expr)),
            ParsedExpr::While(while_expr) => {
                w.write_str("while ")?;
                self.display_expr_id(w, while_expr.cond)?;
                self.display_expr_id(w, while_expr.body)?;
                Ok(())
            }
            ParsedExpr::Loop(loop_expr) => {
                w.write_str("loop ")?;
                write!(w, "{:?}", loop_expr.body)?;
                Ok(())
            }
            ParsedExpr::Struct(struc) => w.write_fmt(format_args!("{:?}", struc)),
            ParsedExpr::ListLiteral(list_expr) => w.write_fmt(format_args!("{:?}", list_expr)),
            ParsedExpr::For(for_expr) => w.write_fmt(format_args!("{:?}", for_expr)),
            ParsedExpr::AnonEnumConstructor(anon_enum) => {
                w.write_char('.')?;
                w.write_str(self.idents.get_name(anon_enum.variant_name))?;
                if let Some(payload) = anon_enum.payload.as_ref() {
                    w.write_str("(")?;
                    self.display_expr_id(w, *payload)?;
                    w.write_str(")")?;
                }
                Ok(())
            }
            ParsedExpr::Is(is_expr) => {
                self.display_expr_id(w, is_expr.target_expression)?;
                w.write_str(" is ")?;
                self.display_pattern_expression_id(is_expr.pattern, w)
            }
            ParsedExpr::Match(match_expr) => {
                w.write_str("switch ")?;
                self.display_expr_id(w, match_expr.match_subject)?;
                w.write_str(" {")?;
                for ParsedMatchCase { patterns, guard_condition_expr, expression } in
                    match_expr.cases.iter()
                {
                    w.write_str("")?;
                    for (pattern_index, pattern_id) in patterns.iter().enumerate() {
                        self.display_pattern_expression_id(*pattern_id, w)?;
                        let is_last = pattern_index == patterns.len() - 1;
                        if !is_last {
                            w.write_str(" or ")?;
                        }
                    }
                    if let Some(guard_condition_expr) = guard_condition_expr {
                        w.write_str(" if ")?;
                        self.display_expr_id(w, *guard_condition_expr)?;
                    }
                    w.write_str(" -> ")?;
                    self.display_expr_id(w, *expression)?;
                    w.write_str(",\n")?;
                }
                w.write_str(" }")
            }
            ParsedExpr::Cast(cast) => {
                self.display_expr_id(w, cast.base_expr)?;
                w.write_str(" as ")?;
                self.display_type_expr_id(cast.dest_type, w)
            }
            ParsedExpr::Lambda(lambda) => {
                w.write_char('\\')?;
                for (index, arg) in lambda.arguments.iter().enumerate() {
                    w.write_str(self.idents.get_name(arg.binding))?;
                    if let Some(ty) = arg.ty {
                        w.write_str(": ")?;
                        self.display_type_expr_id(ty, w)?;
                    }
                    let last = index == lambda.arguments.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(" -> ")?;
                self.display_expr_id(w, lambda.body)?;
                Ok(())
            }
            ParsedExpr::Static(stat) => {
                w.write_str("#static ")?;
                self.display_expr_id(w, stat.base_expr)?;
                Ok(())
            }
            ParsedExpr::Code(code) => {
                w.write_str("#code ")?;
                self.display_stmt_id(w, code.parsed_stmt)?;
                Ok(())
            }
            ParsedExpr::QualifiedAbilityCall(qcall) => {
                // Qualified ability call: foo/Ability@int/baz()
                self.display_ability_expr(w, qcall.ability_expr)?;
                w.write_char('@')?;
                self.display_type_expr_id(qcall.self_name, w)?;
                w.write_char('/')?;
                self.display_expr_id(w, qcall.call_expr)?;
                Ok(())
            }
        }
    }

    fn display_namespaced_identifier(
        &self,
        w: &mut impl Write,
        ns_id: &NamespacedIdentifier,
    ) -> std::fmt::Result {
        if !ns_id.namespaces.is_empty() {
            for (index, ns) in ns_id.namespaces.iter().enumerate() {
                w.write_str(self.idents.get_name(*ns))?;
                if index < ns_id.namespaces.len() - 1 {
                    w.write_str("/")?;
                }
            }
        }
        w.write_str(self.idents.get_name(ns_id.name))
    }

    fn display_ability_expr(
        &self,
        w: &mut impl Write,
        ability_expr_id: ParsedAbilityExprId,
    ) -> std::fmt::Result {
        let e = self.p_ability_exprs.get(ability_expr_id);
        self.display_namespaced_identifier(w, &e.name)?;
        if !e.arguments.is_empty() {
            w.write_str("[")?;
            for (idx, arg) in self.p_type_args.get_slice(e.arguments).iter().enumerate() {
                self.display_type_expr_id(arg.type_expr, w)?;
                if idx < e.arguments.len() - 1 {
                    w.write_str(", ")?;
                }
            }
            w.write_str("]")?;
        }
        Ok(())
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
        w: &mut impl Write,
    ) -> std::fmt::Result {
        match self.type_exprs.get(ty_expr_id) {
            ParsedTypeExpr::Struct(struct_type) => {
                w.write_str("{ ")?;
                for field in struct_type.fields.iter() {
                    w.write_str(self.idents.get_name(field.name))?;
                    w.write_str(": ")?;
                    self.display_type_expr_id(ty_expr_id, w)?;
                    w.write_str(", ")?;
                }
                w.write_str(" }")
            }
            ParsedTypeExpr::TypeApplication(tapp) => {
                display_namespaced_identifier(w, &self.idents, &tapp.name, "::")?;
                if !tapp.args.is_empty() {
                    w.write_str("[")?;
                    for tparam in self.p_type_args.get_slice(tapp.args) {
                        self.display_type_expr_id(tparam.type_expr, w)?;
                        w.write_str(", ")?;
                    }
                    w.write_str("]")?;
                }
                Ok(())
            }
            ParsedTypeExpr::Optional(opt) => {
                self.display_type_expr_id(opt.base, w)?;
                w.write_str("?")
            }
            ParsedTypeExpr::Reference(refer) => {
                self.display_type_expr_id(refer.base, w)?;
                w.write_str("*")
            }
            ParsedTypeExpr::Enum(e) => {
                w.write_str("enum ")?;
                for variant in &e.variants {
                    w.write_str(self.idents.get_name(variant.tag_name))?;
                    if let Some(payload) = &variant.payload_expression {
                        w.write_str("(")?;
                        self.display_type_expr_id(*payload, w)?;
                        w.write_str(")")?;
                    }
                }
                Ok(())
            }
            ParsedTypeExpr::DotMemberAccess(acc) => {
                self.display_type_expr_id(acc.base, w)?;
                w.write_char('.')?;
                w.write_str(self.idents.get_name(acc.member_name))
            }
            ParsedTypeExpr::Builtin(_builtin) => w.write_str("builtin"),
            ParsedTypeExpr::Function(fun) => {
                w.write_char('\\')?;
                for (index, t) in fun.params.iter().enumerate() {
                    self.display_type_expr_id(*t, w)?;
                    let last = index == fun.params.len() - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(" -> ")?;
                self.display_type_expr_id(fun.return_type, w)?;
                Ok(())
            }
            ParsedTypeExpr::TypeOf(tof) => {
                w.write_str("typeOf(")?;
                self.display_expr_id(w, tof.target_expr)?;
                w.write_str(")")?;
                Ok(())
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                w.write_str("some ")?;
                self.display_type_expr_id(quant.inner_type_expr, w)?;
                Ok(())
            }
            ParsedTypeExpr::Static(s) => {
                w.write_str("static ")?;
                self.display_type_expr_id(s.inner_type_expr, w)?;
                Ok(())
            }
            ParsedTypeExpr::TypeFromId(tfi) => {
                w.write_str("typeFromId(")?;
                self.display_expr_id(w, tfi.id_expr)?;
                w.write_str(")")?;
                Ok(())
            }
            ParsedTypeExpr::Array(array_type) => {
                w.write_str("Array[")?;
                self.display_type_expr_id(array_type.size_expr, w)?;
                w.write_str(" x ")?;
                self.display_type_expr_id(array_type.element_type, w)?;
                w.write_str("]")
            }
            ParsedTypeExpr::StaticLiteral(parsed_literal) => {
                write!(w, "{}", parsed_literal)?;
                Ok(())
            }
        }
    }

    pub fn display_stmt_id(&self, w: &mut impl Write, stmt_id: ParsedStmtId) -> std::fmt::Result {
        match self.stmts.get(stmt_id) {
            ParsedStmt::Use(_use_stmt) => {
                write!(w, "use ")?;
                todo!()
            }
            ParsedStmt::Let(let_stmt) => {
                write!(
                    w,
                    "let{}{}{} {} = ",
                    if let_stmt.is_mutable() { "mut " } else { " " },
                    if let_stmt.is_referencing() { "* " } else { " " },
                    if let_stmt.is_context() { "context " } else { " " },
                    self.idents.get_name(let_stmt.name)
                )?;
                self.display_expr_id(w, let_stmt.value)?;
                Ok(())
            }
            ParsedStmt::Require(require_stmt) => {
                write!(w, "require ")?;
                self.display_expr_id(w, require_stmt.condition_expr)?;
                write!(w, " else ")?;
                self.display_expr_id(w, require_stmt.else_body)?;
                Ok(())
            }
            ParsedStmt::Assignment(_assignment) => todo!(),
            ParsedStmt::SetRef(_set_stmt) => todo!(),
            ParsedStmt::LoneExpression(parsed_expression_id) => {
                self.display_expr_id(w, *parsed_expression_id)
            }
        }
    }

    pub fn stmt_id_to_string(&self, stmt_id: ParsedStmtId) -> String {
        let mut buffer = String::new();
        self.display_stmt_id(&mut buffer, stmt_id).unwrap();
        buffer
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

pub fn lex_text(
    module: &mut ParsedProgram,
    source: Source,
    tokens: &mut Vec<Token>,
) -> ParseResult<()> {
    let file_id = source.file_id;
    module.sources.add_source(source);
    let text = &module.sources.get_source(file_id).content;
    let mut lexer = Lexer::make(text, &mut module.spans, file_id);
    lexer.run(tokens).map_err(ParseError::Lex)?;
    tokens.retain(|token| token.kind != K::LineComment);

    Ok(())
}

#[cfg(test)]
pub fn test_parse_module(source: Source) -> ParseResult<ParsedProgram> {
    let program_name = source.filename.split('.').next().unwrap().to_string();
    let mut ast = ParsedProgram::make(
        program_name,
        CompilerConfig {
            is_test_build: false,
            no_std: true,
            target: crate::compiler::detect_host_target().unwrap(),
            debug: true,
        },
    );

    let file_id = source.file_id;
    let mut token_vec = vec![];
    lex_text(&mut ast, source, &mut token_vec)?;
    let module_name = ast.idents.intern("test_module");
    let module_ns_id = init_module(module_name, &mut ast);

    let mut parser =
        Parser::make_for_file(module_name, module_ns_id, &mut ast, &token_vec, file_id);
    parser.parse_file();
    if let Some(e) = ast.errors.first() {
        print_error(&ast, e);
        Err(e.clone())
    } else {
        Ok(ast)
    }
}
