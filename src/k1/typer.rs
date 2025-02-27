pub mod derive;
pub mod dump;
pub mod scopes;
pub mod types;

use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::stderr;
use std::num::NonZeroU32;

use ahash::HashMapExt;
use anyhow::bail;
use colored::Colorize;
use either::Either;
use fxhash::FxHashMap;
use log::{debug, trace};
use smallvec::{smallvec, SmallVec};

use scopes::*;
use types::*;

use crate::lex::{SpanId, Spans, TokenKind};
use crate::parse::{
    self, ForExpr, ForExprType, Identifiers, IfExpr, NamedTypeArg, NamespacedIdentifier,
    NumericWidth, ParsedAbilityId, ParsedAbilityImplId, ParsedConstantId, ParsedDirective,
    ParsedExpressionId, ParsedFunctionId, ParsedId, ParsedLoopExpr, ParsedNamespaceId,
    ParsedPattern, ParsedPatternId, ParsedStmtId, ParsedTypeDefnId, ParsedTypeExpr,
    ParsedTypeExprId, ParsedUnaryOpKind, ParsedUseId, ParsedWhileExpr, Sources,
};
use crate::parse::{
    Block, FnCall, Identifier, Literal, ParsedExpression, ParsedModule, ParsedStmt,
};
use crate::pool::Pool;
use crate::{pool, static_assert_size, strings};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);
impl FunctionId {
    pub const PENDING: FunctionId = FunctionId(u32::MAX);
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NamespaceId(NonZeroU32);
pub const ROOT_NAMESPACE_ID: NamespaceId = NamespaceId(NonZeroU32::new(1).unwrap());

impl From<NonZeroU32> for NamespaceId {
    fn from(value: NonZeroU32) -> Self {
        NamespaceId(value)
    }
}
impl From<NamespaceId> for NonZeroU32 {
    fn from(val: NamespaceId) -> Self {
        val.0
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbilityId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbilityImplId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedStmtId(NonZeroU32);
impl From<NonZeroU32> for TypedStmtId {
    fn from(value: NonZeroU32) -> Self {
        TypedStmtId(value)
    }
}
impl From<TypedStmtId> for NonZeroU32 {
    fn from(val: TypedStmtId) -> Self {
        val.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedExprId(NonZeroU32);
impl From<NonZeroU32> for TypedExprId {
    fn from(value: NonZeroU32) -> Self {
        TypedExprId(value)
    }
}
impl From<TypedExprId> for NonZeroU32 {
    fn from(val: TypedExprId) -> Self {
        val.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Standard,
    External(Option<Identifier>),
    Intrinsic,
}

#[derive(Debug, Clone)]
pub struct TypedAbilityFunctionRef {
    pub function_name: Identifier,
    pub function_id: FunctionId,
}

pub const EQUALS_ABILITY_ID: AbilityId = AbilityId(0);
pub const SHOW_ABILITY_ID: AbilityId = AbilityId(1);
pub const BITWISE_ABILITY_ID: AbilityId = AbilityId(2);
pub const COMPARABLE_ABILITY_ID: AbilityId = AbilityId(3);
pub const UNWRAP_ABILITY_ID: AbilityId = AbilityId(4);
pub const TRY_ABILITY_ID: AbilityId = AbilityId(5);
pub const ITERATOR_ABILITY_ID: AbilityId = AbilityId(6);
pub const ITERABLE_ABILITY_ID: AbilityId = AbilityId(7);

pub const LAMBDA_ENV_PARAM_NAME: &str = "__lambda_env";

pub const FUNC_PARAM_OPT_COUNT: usize = 6;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSubstitutionPair {
    from: TypeId,
    to: TypeId,
}

#[derive(Default)]
pub struct InferenceContext {
    pub origin_stack: Vec<SpanId>,
    pub vars: Vec<TypeId>,
    pub constraints: Vec<TypeSubstitutionPair>,
    pub substitutions: FxHashMap<TypeId, TypeId>,
    pub substitutions_vec: Vec<TypeSubstitutionPair>,
}

#[derive(Debug, Clone, Copy)]
pub struct EvalExprContext {
    scope_id: ScopeId,
    expected_type_id: Option<TypeId>,
    is_inference: bool,
}
impl EvalExprContext {
    pub fn with_expected_type(&self, expected_element_type: Option<TypeId>) -> EvalExprContext {
        EvalExprContext {
            scope_id: self.scope_id,
            expected_type_id: expected_element_type,
            is_inference: self.is_inference,
        }
    }

    pub fn with_no_expected_type(&self) -> EvalExprContext {
        EvalExprContext {
            scope_id: self.scope_id,
            expected_type_id: None,
            is_inference: self.is_inference,
        }
    }

    fn from_scope(scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext { scope_id, expected_type_id: None, is_inference: false }
    }

    fn with_inference(&self, is_inference: bool) -> EvalExprContext {
        EvalExprContext { is_inference, ..*self }
    }

    fn with_scope(&self, scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext { scope_id, ..*self }
    }
}

enum CoerceResult {
    Fail(TypedExprId),
    Coerced(&'static str, TypedExprId),
}

#[derive(Debug, Clone)]
enum MaybeTypedExpr {
    Parsed(ParsedExpressionId),
    Typed(TypedExprId),
}

#[derive(Debug, Clone)]
enum TypeOrParsedExpr {
    TypeId(TypeId),
    ParsedExpr(ParsedExpressionId),
}

#[derive(Debug, Clone)]
pub enum CompileTimeValue {
    Boolean(bool),
    Char(u8),
    Integer(TypedIntegerValue),
    Float(TypedFloatValue),
    String(Box<str>),
}
static_assert_size!(CompileTimeValue, 24);

/// Used for analyzing pattern matching
#[derive(Debug, Clone)]
pub enum PatternConstructor {
    Unit,
    BoolFalse,
    BoolTrue,
    /// Char, String, Int, Float will become more interesting if we implement exhaustive range-based matching like Rust's
    /// For now they exist as placeholders to indicate to the algorithm that something needs to be matched. We treat
    /// exact literals as NOT matching because they do not completely eliminate the pattern, and ignore those exact
    /// literal patterns when we report on 'Useless' patterns
    Char,
    String,
    Int,
    Float,
    /// This one is also kinda a nothing burger, and can only be matched by Wildcards and Bindings; it's here for
    /// the sake of being explicit; we could collapse all these into a 'Anything' constructor but the fact I can't
    /// think of a good name means we shouldn't, probably
    TypeVariable,
    Reference(Box<PatternConstructor>),
    Struct {
        fields: Vec<(Identifier, PatternConstructor)>,
    },
    // Binding(Identifier),
    Enum {
        variant_name: Identifier,
        inner: Option<Box<PatternConstructor>>,
    },
}

#[derive(Debug, Clone)]
pub struct AbilitySpec9nInfo {
    generic_parent: AbilityId,
    specialized_child: AbilityId,
    arguments: Vec<SimpleNamedType>,
}

#[derive(Debug, Clone)]
pub enum TypedAbilityKind {
    Concrete,
    Generic { specializations: Vec<AbilitySpec9nInfo> },
    Specialized(AbilitySpec9nInfo),
}

impl TypedAbilityKind {
    pub fn arguments(&self) -> &[SimpleNamedType] {
        match self {
            TypedAbilityKind::Specialized(specialization) => specialization.arguments.as_slice(),
            TypedAbilityKind::Concrete => &[],
            TypedAbilityKind::Generic { .. } => &[],
        }
    }

    pub fn specializations(&self) -> &[AbilitySpec9nInfo] {
        match self {
            TypedAbilityKind::Concrete => &[],
            TypedAbilityKind::Generic { specializations } => specializations,
            TypedAbilityKind::Specialized(_) => &[],
        }
    }

    pub fn is_specialized(&self) -> bool {
        matches!(self, TypedAbilityKind::Specialized(_))
    }

    pub fn is_concrete(&self) -> bool {
        matches!(self, TypedAbilityKind::Concrete)
    }

    pub fn is_generic(&self) -> bool {
        matches!(self, TypedAbilityKind::Generic { .. })
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbilityParam {
    name: Identifier,
    type_variable_id: TypeId,
    is_impl_param: bool,
    #[allow(unused)]
    span: SpanId,
}

impl NamedType for &TypedAbilityParam {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_variable_id
    }
}

impl TypedAbilityParam {
    fn is_ability_side_param(&self) -> bool {
        !self.is_impl_param
    }
}

impl From<&TypedAbilityParam> for SimpleNamedType {
    fn from(value: &TypedAbilityParam) -> Self {
        SimpleNamedType { name: value.name, type_id: value.type_variable_id }
    }
}

#[derive(Debug, Clone)]
/// An ability signature encompasses an ability's entire 'type' story:
/// - Base type, generic type params, and impl-provided type params
/// Example: Add[Rhs = Int]
///                    ^ impl argument
///              ^ ability argument
///          ^
///          Ability Id
pub struct TypedAbilitySignature {
    ability_id: AbilityId,
    impl_arguments: SmallVec<[SimpleNamedType; 4]>,
}

#[derive(Debug, Clone)]
pub struct TypedAbility {
    pub name: Identifier,
    pub self_type_id: TypeId,
    pub parameters: Vec<TypedAbilityParam>,
    pub functions: Vec<TypedAbilityFunctionRef>,
    pub scope_id: ScopeId,
    pub ast_id: ParsedAbilityId,
    pub namespace_id: NamespaceId,
    pub kind: TypedAbilityKind,
}

impl TypedAbility {
    pub fn find_function_by_name(
        &self,
        name: Identifier,
    ) -> Option<(usize, &TypedAbilityFunctionRef)> {
        self.functions.iter().enumerate().find(|(_, f)| f.function_name == name)
    }

    pub fn parent_ability_id(&self) -> Option<AbilityId> {
        match &self.kind {
            TypedAbilityKind::Concrete => None,
            TypedAbilityKind::Generic { .. } => None,
            TypedAbilityKind::Specialized(specialization) => Some(specialization.generic_parent),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedEnumPattern {
    pub enum_type_id: TypeId,
    pub variant_tag_name: Identifier,
    pub variant_index: u32,
    pub payload: Option<Box<TypedPattern>>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedStructPatternField {
    pub name: Identifier,
    pub pattern: TypedPattern,
    pub field_index: u32,
    pub field_type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypedStructPattern {
    pub struct_type_id: TypeId,
    pub fields: Vec<TypedStructPatternField>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct VariablePattern {
    pub name: Identifier,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedReferencePattern {
    pub inner_pattern: Box<TypedPattern>,
    pub span: SpanId,
}

// <pattern> ::= <literal> | <variable> | <enum> | <struc>
// <literal> ::= "(" ")" | "\"" <ident> "\"" | [0-9]+ | "'" [a-z] "'" | "None"
// <variable> ::= <ident>
// <ident> ::= [a-z]*
// <enum> ::= "." <ident> ( "(" <pattern> ")" )?
// <struc> ::= "{" ( <ident> ": " <pattern> ","? )* "}"
#[derive(Debug, Clone)]
pub enum TypedPattern {
    LiteralUnit(SpanId),
    LiteralChar(u8, SpanId),
    LiteralInteger(TypedIntegerValue, SpanId),
    LiteralFloat(TypedFloatValue, SpanId),
    LiteralBool(bool, SpanId),
    LiteralString(String, SpanId),
    Variable(VariablePattern),
    Enum(TypedEnumPattern),
    Struct(TypedStructPattern),
    Wildcard(SpanId),
    Reference(TypedReferencePattern),
}

impl TypedPattern {
    pub fn all_bindings(&self) -> Vec<VariablePattern> {
        let mut v: Vec<VariablePattern> = vec![];
        // This sorts by the Identifier id, not the name itself, but that's absolutely fine
        self.all_bindings_rec(&mut v);
        v.sort_by_key(|vp| vp.name);
        v
    }
    fn all_bindings_rec(&self, bindings: &mut Vec<VariablePattern>) {
        match self {
            TypedPattern::LiteralUnit(_) => (),
            TypedPattern::LiteralChar(_, _) => (),
            TypedPattern::LiteralInteger(_, _) => (),
            TypedPattern::LiteralFloat(_, _) => (),
            TypedPattern::LiteralBool(_, _) => (),
            TypedPattern::LiteralString(_, _) => (),
            TypedPattern::Variable(variable_pattern) => bindings.push(variable_pattern.clone()),
            TypedPattern::Enum(enum_pattern) => {
                if let Some(payload_pattern) = enum_pattern.payload.as_ref() {
                    payload_pattern.all_bindings_rec(bindings)
                }
            }
            TypedPattern::Struct(struct_pattern) => {
                for field_pattern in struct_pattern.fields.iter() {
                    field_pattern.pattern.all_bindings_rec(bindings)
                }
            }
            TypedPattern::Wildcard(_) => (),
            TypedPattern::Reference(refer) => refer.inner_pattern.all_bindings_rec(bindings),
        }
    }
    pub fn has_innumerable_literal(&self) -> bool {
        match self {
            TypedPattern::LiteralChar(_, _span) => true,
            TypedPattern::LiteralInteger(_, _span) => true,
            TypedPattern::LiteralFloat(_, _span) => true,
            TypedPattern::LiteralString(_, _span) => true,
            TypedPattern::LiteralUnit(_span_id) => false,
            TypedPattern::LiteralBool(_, _span_id) => false,
            TypedPattern::Variable(_variable_pattern) => false,
            TypedPattern::Enum(typed_enum_pattern) => {
                typed_enum_pattern.payload.as_ref().is_some_and(|p| p.has_innumerable_literal())
            }
            TypedPattern::Struct(typed_struct_pattern) => typed_struct_pattern
                .fields
                .iter()
                .any(|field_pattern| field_pattern.pattern.has_innumerable_literal()),
            TypedPattern::Wildcard(_span_id) => false,
            TypedPattern::Reference(refer) => refer.inner_pattern.has_innumerable_literal(),
        }
    }
    pub fn span_id(&self) -> SpanId {
        match self {
            TypedPattern::LiteralUnit(span) => *span,
            TypedPattern::LiteralChar(_, span) => *span,
            TypedPattern::LiteralInteger(_, span) => *span,
            TypedPattern::LiteralFloat(_, span) => *span,
            TypedPattern::LiteralBool(_, span) => *span,
            TypedPattern::LiteralString(_, span) => *span,
            TypedPattern::Variable(variable_pattern) => variable_pattern.span,
            TypedPattern::Enum(enum_pattern) => enum_pattern.span,
            TypedPattern::Struct(struct_pattern) => struct_pattern.span,
            TypedPattern::Wildcard(span) => *span,
            TypedPattern::Reference(refer) => refer.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedLambda {
    pub scope: ScopeId,
    pub environment_struct_reference_type: TypeId,
    pub parsed_expression_id: ParsedExpressionId,
    pub captures: Vec<VariableId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub expr_type: TypeId,
    pub scope_id: ScopeId,
    pub statements: Vec<TypedStmtId>,
    pub span: SpanId,
}

impl TypedBlock {}

#[derive(Debug, Clone)]
pub struct FnArgDefn {
    pub name: Identifier,
    pub variable_id: VariableId,
    // TODO: Consider dropping these explicit position fields
    pub position: u32,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_lambda_env: bool,
    pub span: SpanId,
}

impl FnArgDefn {
    pub fn to_fn_arg_type(&self) -> FnParamType {
        FnParamType {
            name: self.name,
            type_id: self.type_id,
            is_context: self.is_context,
            is_lambda_env: self.is_lambda_env,
            span: self.span,
        }
    }
}

#[derive(Debug)]
pub struct SpecializationParams {
    pub fn_scope_id: ScopeId,
    pub new_name: Identifier,
    pub known_intrinsic: Option<IntrinsicFunction>,
    pub generic_parent_function: FunctionId,
    pub passed_type_ids: Vec<SimpleNamedType>,
}

#[derive(Debug, Clone)]
pub struct SpecializationInfo {
    pub parent_function: FunctionId,
    pub type_arguments: Vec<SimpleNamedType>,
    pub specialized_function_id: FunctionId,
    pub specialized_function_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub enum TypedFunctionKind {
    Standard,
    Lambda,
    AbilityDefn(AbilityId),
    AbilityImpl(AbilityId, TypeId),
    AbilityImplDerivedBlanket(FunctionId, AbilityId, TypeId),
}
impl TypedFunctionKind {
    pub fn blanket_parent_function_id(&self) -> Option<FunctionId> {
        match self {
            TypedFunctionKind::Standard => None,
            TypedFunctionKind::Lambda => None,
            TypedFunctionKind::AbilityDefn(_) => None,
            TypedFunctionKind::AbilityImpl(_, _) => None,
            TypedFunctionKind::AbilityImplDerivedBlanket(function_id, _, _) => Some(*function_id),
        }
    }
    pub fn ability_id(&self) -> Option<AbilityId> {
        match self {
            TypedFunctionKind::Standard => None,
            TypedFunctionKind::Lambda => None,
            TypedFunctionKind::AbilityDefn(ability_id) => Some(*ability_id),
            TypedFunctionKind::AbilityImpl(ability_id, _) => Some(*ability_id),
            TypedFunctionKind::AbilityImplDerivedBlanket(_, ability_id, _) => Some(*ability_id),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: Identifier,
    pub scope: ScopeId,
    pub param_variables: Vec<VariableId>,
    pub type_params: SmallVec<[TypeParam; 6]>,
    pub body_block: Option<TypedExprId>,
    pub intrinsic_type: Option<IntrinsicFunction>,
    pub linkage: Linkage,
    pub child_specializations: Vec<SpecializationInfo>,
    pub specialization_info: Option<SpecializationInfo>,
    pub parsed_id: ParsedId,
    pub type_id: TypeId,
    pub compiler_debug: bool,
    pub kind: TypedFunctionKind,
    pub is_concrete: bool,
}

impl TypedFunction {
    fn is_generic(&self) -> bool {
        !self.type_params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: Identifier,
    pub type_id: TypeId,
    pub span: SpanId,
}

impl NamedType for &TypeParam {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl NamedType for TypeParam {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

pub trait NamedType {
    fn name(&self) -> Identifier;
    fn type_id(&self) -> TypeId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SimpleNamedType {
    pub name: Identifier,
    pub type_id: TypeId,
}

impl NamedType for &SimpleNamedType {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl NamedType for SimpleNamedType {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

enum TypeUnificationResult {
    Matching,
    NoHoles,
    NonMatching(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpannedNamedType {
    pub name: Identifier,
    pub type_id: TypeId,
    pub span: SpanId,
}

impl NamedType for &SpannedNamedType {
    fn name(&self) -> Identifier {
        self.name
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub variable_id: VariableId,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    // Integer operations
    Add,
    Subtract,
    Multiply,
    Divide,
    Rem,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Boolean operations
    And,
    Or,

    // Equality
    Equals,
    NotEquals,

    // Other
    OptionalElse,
    Pipe,
}

impl Display for BinaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            BinaryOpKind::Add => f.write_char('+'),
            BinaryOpKind::Subtract => f.write_char('-'),
            BinaryOpKind::Multiply => f.write_char('*'),
            BinaryOpKind::Divide => f.write_char('/'),
            BinaryOpKind::Rem => f.write_char('%'),
            BinaryOpKind::Less => f.write_char('<'),
            BinaryOpKind::Greater => f.write_char('>'),
            BinaryOpKind::LessEqual => f.write_str("<="),
            BinaryOpKind::GreaterEqual => f.write_str(">="),
            BinaryOpKind::And => f.write_str("and"),
            BinaryOpKind::Or => f.write_str("or"),
            BinaryOpKind::Equals => f.write_str("=="),
            BinaryOpKind::NotEquals => f.write_str("!="),
            BinaryOpKind::OptionalElse => f.write_str("?"),
            BinaryOpKind::Pipe => f.write_str("|"),
        }
    }
}

impl BinaryOpKind {
    pub fn precedence(&self) -> usize {
        use BinaryOpKind as B;
        match self {
            B::Pipe => 102,
            B::Rem => 101,
            B::Multiply | B::Divide => 100,
            B::Add | B::Subtract => 90,
            B::Less | B::LessEqual | B::Greater | B::GreaterEqual | B::Equals | B::NotEquals => 80,
            B::And => 70,
            B::Or => 66,
            B::OptionalElse => 65,
        }
    }

    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Minus => Some(BinaryOpKind::Subtract),
            TokenKind::Asterisk => Some(BinaryOpKind::Multiply),
            TokenKind::Slash => Some(BinaryOpKind::Divide),
            TokenKind::LeftAngle => Some(BinaryOpKind::Less),
            TokenKind::RightAngle => Some(BinaryOpKind::Greater),
            TokenKind::LessThanEqual => Some(BinaryOpKind::LessEqual),
            TokenKind::GreaterThanEqual => Some(BinaryOpKind::GreaterEqual),
            TokenKind::KeywordAnd => Some(BinaryOpKind::And),
            TokenKind::KeywordOr => Some(BinaryOpKind::Or),
            TokenKind::EqualsEquals => Some(BinaryOpKind::Equals),
            TokenKind::BangEquals => Some(BinaryOpKind::NotEquals),
            TokenKind::QuestionMark => Some(BinaryOpKind::OptionalElse),
            TokenKind::Percent => Some(BinaryOpKind::Rem),
            TokenKind::Pipe => Some(BinaryOpKind::Pipe),
            _ => None,
        }
    }

    fn is_symmetric_binop(&self) -> bool {
        match self {
            BinaryOpKind::Add => true,
            BinaryOpKind::Subtract => true,
            BinaryOpKind::Multiply => true,
            BinaryOpKind::Divide => true,
            BinaryOpKind::Rem => true,
            BinaryOpKind::Less => true,
            BinaryOpKind::LessEqual => true,
            BinaryOpKind::Greater => true,
            BinaryOpKind::GreaterEqual => true,
            BinaryOpKind::And => true,
            BinaryOpKind::Or => true,
            BinaryOpKind::Equals => true,
            BinaryOpKind::NotEquals => true,
            BinaryOpKind::OptionalElse => false,
            BinaryOpKind::Pipe => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub ty: TypeId,
    pub lhs: TypedExprId,
    pub rhs: TypedExprId,
    pub span: SpanId,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnaryOpKind {
    Dereference,
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            UnaryOpKind::Dereference => f.write_char('*'),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub type_id: TypeId,
    pub expr: TypedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum Callee {
    StaticFunction(FunctionId),
    StaticLambda {
        function_id: FunctionId,
        lambda_type_id: TypeId,
    },
    /// Must contain a LambdaObject
    DynamicLambda(TypedExprId),
    /// Must contain a Function reference
    DynamicFunction(TypedExprId),
    /// Used by function type parameters
    Abstract {
        variable_id: VariableId,
        function_type: TypeId,
    },
}

impl Callee {
    pub fn make_static(function_id: FunctionId) -> Callee {
        Callee::StaticFunction(function_id)
    }
    pub fn maybe_function_id(&self) -> Option<FunctionId> {
        match self {
            Callee::StaticFunction(function_id) => Some(*function_id),
            Callee::StaticLambda { function_id, .. } => Some(*function_id),
            Callee::DynamicLambda(_) => None,
            Callee::DynamicFunction(_) => None,
            Callee::Abstract { .. } => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Callee,
    pub args: Vec<TypedExprId>,
    /// type_args remain unerased for some intrinsics where we want codegen to see the types.
    /// Specifically sizeOf[T], there's no actual value to specialize on, kinda of a hack would be
    /// better to specialize anyway and inline? idk
    pub type_args: SmallVec<[SimpleNamedType; 6]>,
    pub return_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub expr: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Vec<StructField>,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ListLiteral {
    pub elements: Vec<TypedExprId>,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: TypedExprId,
    pub consequent: TypedExprId,
    pub alternate: TypedExprId,
    pub ty: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: TypedExprId,
    pub target_field: Identifier,
    pub target_field_index: u32,
    pub result_type: TypeId,
    pub struct_type: TypeId,
    pub is_referencing: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedEnumConstructor {
    pub type_id: TypeId,
    pub variant_name: Identifier,
    pub variant_index: u32,
    pub payload: Option<TypedExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct GetEnumPayload {
    pub target_expr: TypedExprId,
    pub result_type_id: TypeId,
    pub variant_name: Identifier,
    pub variant_index: u32,
    pub is_referencing: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedEnumIsVariantExpr {
    pub target_expr: TypedExprId,
    pub variant_name: Identifier,
    pub variant_index: u32,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    /// Multiple patterns does _not_ mean the user provided multiple patterns in a switch,
    /// that case currently gets compiled to N arms, where N is the number of alternate patterns
    /// Though that's something I could now consider changing, the trick would be to generate
    /// a single set of variables for all the bindings that each point to the unique variables from each
    /// pattern. We already check for exact number and name, so this is possible
    ///
    /// But, if we have multiple patterns here currently, it means this was an if statement with
    /// multiple patterns joined by 'and' operations
    pub patterns: SmallVec<[TypedPattern; 1]>,
    pub setup_statements: SmallVec<[TypedStmtId; 4]>,
    pub pattern_condition: TypedExprId,
    pub pattern_bindings: SmallVec<[TypedStmtId; 4]>,
    pub guard_condition: Option<TypedExprId>,
    pub consequent_expr: TypedExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedIntegerValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl TypedIntegerValue {
    pub fn get_type(&self) -> TypeId {
        match self {
            TypedIntegerValue::U8(_) => U8_TYPE_ID,
            TypedIntegerValue::U16(_) => U16_TYPE_ID,
            TypedIntegerValue::U32(_) => U32_TYPE_ID,
            TypedIntegerValue::U64(_) => U64_TYPE_ID,
            TypedIntegerValue::I8(_) => I8_TYPE_ID,
            TypedIntegerValue::I16(_) => I16_TYPE_ID,
            TypedIntegerValue::I32(_) => I32_TYPE_ID,
            TypedIntegerValue::I64(_) => I64_TYPE_ID,
        }
    }

    pub fn as_u64(&self) -> u64 {
        match self {
            TypedIntegerValue::U8(v) => *v as u64,
            TypedIntegerValue::U16(v) => *v as u64,
            TypedIntegerValue::U32(v) => *v as u64,
            TypedIntegerValue::U64(v) => *v,
            TypedIntegerValue::I8(v) => *v as u64,
            TypedIntegerValue::I16(v) => *v as u64,
            TypedIntegerValue::I32(v) => *v as u64,
            TypedIntegerValue::I64(v) => *v as u64,
        }
    }
}

impl Display for TypedIntegerValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedIntegerValue::U8(v) => write!(f, "{}u8", v),
            TypedIntegerValue::U16(v) => write!(f, "{}u16", v),
            TypedIntegerValue::U32(v) => write!(f, "{}u32", v),
            TypedIntegerValue::U64(v) => write!(f, "{}u64", v),
            TypedIntegerValue::I8(v) => write!(f, "{}i8", v),
            TypedIntegerValue::I16(v) => write!(f, "{}i16", v),
            TypedIntegerValue::I32(v) => write!(f, "{}i32", v),
            TypedIntegerValue::I64(v) => write!(f, "{}i64", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIntegerExpr {
    pub value: TypedIntegerValue,
    pub span: SpanId,
}

impl TypedIntegerExpr {
    pub fn get_type(&self) -> TypeId {
        self.value.get_type()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypedFloatValue {
    F32(f32),
    F64(f64),
}

impl TypedFloatValue {
    pub fn get_type(&self) -> TypeId {
        match self {
            TypedFloatValue::F32(_) => F32_TYPE_ID,
            TypedFloatValue::F64(_) => F64_TYPE_ID,
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            TypedFloatValue::F32(v) => *v as f64,
            TypedFloatValue::F64(v) => *v,
        }
    }
}

impl Display for TypedFloatValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedFloatValue::F32(v) => write!(f, "{}f32", v),
            TypedFloatValue::F64(v) => write!(f, "{}f64", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedFloatExpr {
    pub value: TypedFloatValue,
    pub span: SpanId,
}

impl TypedFloatExpr {
    pub fn get_type(&self) -> TypeId {
        self.value.get_type()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CastType {
    IntegerExtend,
    IntegerTruncate,
    Integer8ToChar,
    IntegerExtendFromChar,
    KnownNoOp,
    PointerToReference,
    ReferenceToPointer,
    PointerToInteger,
    IntegerToPointer,
    FloatExtend,
    FloatTruncate,
    FloatToInteger,
    IntegerToFloat,
}

impl CastType {
    pub fn is_unsafe(&self) -> bool {
        match self {
            CastType::IntegerExtend => false,
            CastType::IntegerTruncate => false,
            CastType::Integer8ToChar => false,
            CastType::IntegerExtendFromChar => false,
            CastType::KnownNoOp => false,
            CastType::PointerToReference => true,
            CastType::ReferenceToPointer => true,
            CastType::PointerToInteger => true,
            CastType::IntegerToPointer => true,
            CastType::FloatExtend => false,
            CastType::FloatTruncate => false,
            CastType::FloatToInteger => false,
            CastType::IntegerToFloat => false,
        }
    }
}

impl Display for CastType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CastType::IntegerExtend => write!(f, "iext"),
            CastType::IntegerTruncate => write!(f, "itrunc"),
            CastType::Integer8ToChar => write!(f, "i8tochar"),
            CastType::IntegerExtendFromChar => write!(f, "iextfromchar"),
            CastType::KnownNoOp => write!(f, "noop"),
            CastType::PointerToReference => write!(f, "ptrtoref"),
            CastType::ReferenceToPointer => write!(f, "reftoptr"),
            CastType::PointerToInteger => write!(f, "ptrtoint"),
            CastType::IntegerToPointer => write!(f, "inttoptr"),
            CastType::FloatExtend => write!(f, "fext"),
            CastType::FloatTruncate => write!(f, "ftrunc"),
            CastType::FloatToInteger => write!(f, "ftoint"),
            CastType::IntegerToFloat => write!(f, "inttof"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedCast {
    pub cast_type: CastType,
    pub base_expr: TypedExprId,
    pub target_type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub value: TypedExprId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedBreak {
    pub value: TypedExprId,
    pub loop_scope: ScopeId,
    pub loop_type: LoopType,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct LambdaExpr {
    pub lambda_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FunctionReferenceExpr {
    pub function_id: FunctionId,
    pub span: SpanId,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct PendingCaptureExpr {
    pub captured_variable_id: VariableId,
    pub type_id: TypeId,
    pub resolved_expr: Option<TypedExprId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub cond: TypedExprId,
    pub body: Box<TypedBlock>,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body: Box<TypedBlock>,
    pub break_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
struct MatchingCondition {
    #[allow(unused)]
    all_patterns: SmallVec<[TypedPattern; 4]>,
    combined_condition: TypedExprId,
    setup_statements: SmallVec<[TypedStmtId; 4]>,
    binding_statements: SmallVec<[TypedStmtId; 4]>,
    #[allow(unused)]
    binding_eligible: bool,
}

#[derive(Debug, Clone)]
/// The last arm's condition is guaranteed to always evaluate to 'true'
pub struct TypedMatchExpr {
    pub initial_let_statements: Vec<TypedStmtId>,
    pub result_type: TypeId,
    pub arms: Vec<TypedMatchArm>,
    pub span: SpanId,
}

// nocommit: Size: 112 bytes
static_assert_size!(TypedExpr, 112);
#[derive(Debug, Clone)]
pub enum TypedExpr {
    Unit(SpanId),
    Char(u8, SpanId),
    Bool(bool, SpanId),
    Integer(TypedIntegerExpr),
    Float(TypedFloatExpr),
    Str(String, SpanId),
    Struct(Struct),
    StructFieldAccess(FieldAccess),
    Variable(VariableExpr),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Block(TypedBlock),
    Call(Call),
    If(TypedIf),
    /// In the past, we lowered match to an if/else chain. This proves not quite powerful enough
    /// of a representation to do everything we want
    Match(TypedMatchExpr),
    WhileLoop(WhileLoop),
    LoopExpr(LoopExpr),
    EnumConstructor(TypedEnumConstructor),
    EnumIsVariant(TypedEnumIsVariantExpr),
    EnumGetPayload(GetEnumPayload),
    Cast(TypedCast),
    /// Explicit returns are syntactically like function calls, but are their own instruction type
    /// return(<expr>)
    /// It has the expression type of 'never', but is bound by the return type of the nearest
    /// enclosing function or lambda
    Return(TypedReturn),
    /// Breaks are syntactically like function calls, but are their own instruction type
    /// break(<expr>)
    /// It has the expression type of 'never', but influences the return type of the enclosing loop
    Break(TypedBreak),
    /// Creating a lambda results in a Lambda expr.
    /// - A function is created
    /// - An environment capture expr is created
    /// - An expression is returned that is really just a pointer to the unique Closure it points
    ///   to; this can either be called directly or turned into a dynamic function object if needed
    Lambda(LambdaExpr),
    /// Referring to a function by name, if there are no variables in scope with the same name
    /// results in taking a function pointer
    FunctionName(FunctionReferenceExpr),
    /// These get re-written into struct access expressions once we know all captures and have
    /// generated the lambda's capture struct
    PendingCapture(PendingCaptureExpr),
}

impl From<VariableExpr> for TypedExpr {
    fn from(value: VariableExpr) -> Self {
        TypedExpr::Variable(value)
    }
}

impl TypedExpr {
    pub fn get_type(&self) -> TypeId {
        match self {
            TypedExpr::Unit(_) => UNIT_TYPE_ID,
            TypedExpr::Char(_, _) => CHAR_TYPE_ID,
            TypedExpr::Str(_, _) => STRING_TYPE_ID,
            TypedExpr::Integer(integer) => integer.get_type(),
            TypedExpr::Float(float) => float.get_type(),
            TypedExpr::Bool(_, _) => BOOL_TYPE_ID,
            TypedExpr::Struct(struc) => struc.type_id,
            TypedExpr::Variable(var) => var.type_id,
            TypedExpr::StructFieldAccess(field_access) => field_access.result_type,
            TypedExpr::BinaryOp(binary_op) => binary_op.ty,
            TypedExpr::UnaryOp(unary_op) => unary_op.type_id,
            TypedExpr::Block(b) => b.expr_type,
            TypedExpr::Call(call) => call.return_type,
            TypedExpr::If(typed_if) => typed_if.ty,
            TypedExpr::Match(match_) => match_.result_type,
            TypedExpr::WhileLoop(while_loop) => while_loop.type_id,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.break_type,
            TypedExpr::EnumConstructor(enum_cons) => enum_cons.type_id,
            TypedExpr::EnumIsVariant(_is_variant) => BOOL_TYPE_ID,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.result_type_id,
            TypedExpr::Cast(c) => c.target_type_id,
            TypedExpr::Return(_ret) => NEVER_TYPE_ID,
            TypedExpr::Break(_break) => NEVER_TYPE_ID,
            TypedExpr::Lambda(lambda) => lambda.lambda_type,
            TypedExpr::FunctionName(f) => f.type_id,
            TypedExpr::PendingCapture(pc) => pc.type_id,
        }
    }

    pub fn get_span(&self) -> SpanId {
        match self {
            TypedExpr::Unit(span) => *span,
            TypedExpr::Char(_, span) => *span,
            TypedExpr::Bool(_, span) => *span,
            TypedExpr::Integer(int) => int.span,
            TypedExpr::Float(float) => float.span,
            TypedExpr::Str(_, span) => *span,
            TypedExpr::Struct(struc) => struc.span,
            TypedExpr::Variable(var) => var.span,
            TypedExpr::StructFieldAccess(field_access) => field_access.span,
            TypedExpr::BinaryOp(binary_op) => binary_op.span,
            TypedExpr::UnaryOp(unary_op) => unary_op.span,
            TypedExpr::Block(b) => b.span,
            TypedExpr::Call(call) => call.span,
            TypedExpr::If(typed_if) => typed_if.span,
            TypedExpr::Match(match_) => match_.span,
            TypedExpr::WhileLoop(while_loop) => while_loop.span,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.span,
            TypedExpr::EnumConstructor(e) => e.span,
            TypedExpr::EnumIsVariant(is_variant) => is_variant.span,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.span,
            TypedExpr::Cast(c) => c.span,
            TypedExpr::Return(ret) => ret.span,
            TypedExpr::Break(brk) => brk.span,
            TypedExpr::Lambda(lambda) => lambda.span,
            TypedExpr::FunctionName(f) => f.span,
            TypedExpr::PendingCapture(pc) => pc.span,
        }
    }

    pub fn expect_variable(self) -> VariableExpr {
        if let Self::Variable(v) = self {
            v
        } else {
            panic!("Expected variable expression")
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, TypedExpr::Unit(_))
    }
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub variable_id: VariableId,
    pub variable_type: TypeId,
    pub initializer: TypedExprId,
    pub is_referencing: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    Value,
    Reference,
}

#[derive(Debug, Clone)]
pub struct AssignmentStmt {
    pub destination: TypedExprId,
    pub value: TypedExprId,
    pub span: SpanId,
    pub kind: AssignmentKind,
}

static_assert_size!(TypedStmt, 20);
#[derive(Debug, Clone)]
pub enum TypedStmt {
    Expr(TypedExprId, TypeId),
    Let(LetStmt),
    Assignment(AssignmentStmt),
}

impl TypedStmt {
    pub fn as_let(&self) -> Option<&LetStmt> {
        match self {
            TypedStmt::Let(let_stmt) => Some(let_stmt),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorLevel {
    Error,
    Warn,
    Info,
    Hint,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorLevel::Error => f.write_str("error"),
            ErrorLevel::Warn => f.write_str("warn"),
            ErrorLevel::Info => f.write_str("info"),
            ErrorLevel::Hint => f.write_str("hint"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyperError {
    pub message: String,
    pub span: SpanId,
    pub level: ErrorLevel,
}

impl TyperError {
    fn make(message: impl AsRef<str>, span: SpanId) -> TyperError {
        TyperError { message: message.as_ref().to_owned(), span, level: ErrorLevel::Error }
    }
}

impl Display for TyperError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("typer error {}: {:?}", self.message, self))
    }
}

impl Error for TyperError {}

pub type TyperResult<A> = Result<A, TyperError>;

#[derive(Debug, Clone)]
struct SynthedVariable {
    #[allow(unused)]
    pub variable_id: VariableId,
    pub defn_stmt: TypedStmtId,
    pub variable_expr: TypedExprId,
    #[allow(unused)]
    pub parsed_expr: ParsedExpressionId,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Identifier,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub owner_scope: ScopeId,
    pub is_context: bool,
    pub constant_id: Option<ConstantId>,
}

#[derive(Debug)]
pub struct Constant {
    pub variable_id: VariableId,
    pub parsed_expr: ParsedExpressionId,
    pub value: CompileTimeValue,
    pub ty: TypeId,
    pub span: SpanId,
}

#[derive(Debug)]
pub enum NamespaceType {
    User,
    Ability,
    Root,
}

#[derive(Debug)]
pub struct Namespace {
    pub name: Identifier,
    pub scope_id: ScopeId,
    pub namespace_type: NamespaceType,
    pub companion_type_id: Option<TypeId>,
    pub parent_id: Option<NamespaceId>,
}

pub struct Namespaces {
    pub namespaces: Pool<Namespace, NamespaceId>,
}

impl Namespaces {
    pub fn get(&self, id: NamespaceId) -> &Namespace {
        self.namespaces.get(id)
    }

    pub fn get_mut(&mut self, id: NamespaceId) -> &mut Namespace {
        self.namespaces.get_mut(id)
    }

    pub fn add(&mut self, namespace: Namespace) -> NamespaceId {
        self.namespaces.add(namespace)
    }

    pub fn iter(&self) -> std::slice::Iter<Namespace> {
        self.namespaces.iter()
    }

    pub fn find_child_by_name(
        &self,
        parent_id: NamespaceId,
        name: Identifier,
    ) -> Option<&Namespace> {
        self.iter()
            .find(|ns| ns.parent_id.is_some_and(|parent| parent == parent_id) && ns.name == name)
    }

    pub fn name_chain(&self, id: NamespaceId) -> VecDeque<Identifier> {
        let mut chain = VecDeque::with_capacity(8);
        let mut id = id;
        loop {
            let namespace = &self.get(id);
            chain.push_front(namespace.name);
            if let Some(parent_id) = namespace.parent_id {
                id = parent_id;
            } else {
                break;
            }
        }
        chain
    }

    pub fn get_scope(&self, namespace_id: NamespaceId) -> ScopeId {
        self.get(namespace_id).scope_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicFunction {
    SizeOf,
    SizeOfStride,
    AlignOf,
    TypeId,
    BoolNegate,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    PointerIndex,
    CompilerSourceLocation,
}

impl IntrinsicFunction {
    pub fn is_inlined(self) -> bool {
        match self {
            IntrinsicFunction::SizeOf => true,
            IntrinsicFunction::SizeOfStride => true,
            IntrinsicFunction::AlignOf => true,
            IntrinsicFunction::TypeId => true,
            IntrinsicFunction::BoolNegate => true,
            IntrinsicFunction::BitNot => true,
            IntrinsicFunction::BitAnd => true,
            IntrinsicFunction::BitOr => true,
            IntrinsicFunction::BitXor => true,
            IntrinsicFunction::BitShiftLeft => true,
            IntrinsicFunction::BitShiftRight => true,
            IntrinsicFunction::PointerIndex => true,
            IntrinsicFunction::CompilerSourceLocation => true,
        }
    }
}

pub fn make_error<T: AsRef<str>>(message: T, span: SpanId) -> TyperError {
    TyperError::make(message.as_ref(), span)
}

pub fn make_fail_span<A, T: AsRef<str>>(message: T, span: SpanId) -> TyperResult<A> {
    Err(make_error(message, span))
}

/// thanks heather
#[macro_export]
macro_rules! panic_at_disco {
    ($($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            panic!("Panic!! at disco!!!: {}", s)
        }
    };
}

#[macro_export]
macro_rules! errf {
    ($span:expr, $($format_args:expr),* $(,)?) => {
        {
            let s: String = format!($($format_args),*);
            make_error(&s, $span)
        }
    };
}

#[macro_export]
macro_rules! failf {
    ($span:expr, $($format_args:expr),* $(,)?) => {
        {
            let s: String = format!($($format_args),*);
            make_fail_span(&s, $span)
        }
    };
}

macro_rules! get_ident {
    ($self:ident, $name:expr) => {
        $self
            .ast
            .identifiers
            .get($name)
            .unwrap_or_else(|| panic!("Missing identifier '{}' in pool", $name))
    };
}

/// Make a qualified, `NamespacedIdentifier` from components
macro_rules! qident {
    ($self:ident, $span:expr, $namespaces:expr, $name:literal $(,)?) => {{
        let idents: Vec<Identifier> = ($namespaces).iter().map(|n| get_ident!($self, n)).collect();
        NamespacedIdentifier { namespaces: idents, name: get_ident!($self, $name), span: $span }
    }};
    ($self:ident, $span:expr, $name:literal) => {{
        NamespacedIdentifier { namespaces: vec![], name: get_ident!($self, $name), span: $span }
    }};
}

fn make_fail_ast_id<A, T: AsRef<str>>(
    ast: &ParsedModule,
    message: T,
    parsed_id: ParsedId,
) -> TyperResult<A> {
    let span = ast.get_span_for_id(parsed_id);
    Err(make_error(message, span))
}

pub fn write_error(
    w: &mut impl std::io::Write,
    spans: &Spans,
    sources: &Sources,
    message: impl AsRef<str>,
    level: ErrorLevel,
    span: SpanId,
) -> std::io::Result<()> {
    parse::write_error_location(w, spans, sources, span, level)?;
    writeln!(w, "\t{}\n", message.as_ref())?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbilityImplKind {
    Concrete,
    Blanket { base_ability: AbilityId, parsed_id: ParsedAbilityImplId },
    DerivedFromBlanket { blanket_impl_id: AbilityImplId },
    VariableConstraint,
}

impl AbilityImplKind {
    pub fn blanket_parent(&self) -> Option<AbilityId> {
        match self {
            AbilityImplKind::Blanket { base_ability: parent_ability, .. } => Some(*parent_ability),
            _ => None,
        }
    }

    pub fn is_blanket(&self) -> bool {
        matches!(self, AbilityImplKind::Blanket { .. })
    }

    pub fn is_concrete(&self) -> bool {
        matches!(self, AbilityImplKind::Concrete)
    }

    pub fn is_variable_constraint(&self) -> bool {
        matches!(self, AbilityImplKind::VariableConstraint)
    }

    pub fn is_derived_from_blanket(&self) -> bool {
        matches!(self, AbilityImplKind::DerivedFromBlanket { .. })
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbilityImpl {
    pub kind: AbilityImplKind,
    pub type_params: Vec<TypeParam>,
    pub self_type_id: TypeId,
    pub ability_id: AbilityId,
    /// The values for the types that the implementation is responsible for providing.
    /// Yes, they are already baked into the functions but I need them explicitly in order
    /// to do constraint checking
    pub impl_arguments: SmallVec<[SimpleNamedType; 4]>,
    /// Invariant: These functions are ordered how they are defined in the ability, NOT how they appear in
    /// the impl code
    pub functions: Vec<FunctionId>,
    pub scope_id: ScopeId,
    pub span: SpanId,
    /// I need this so that I don't try to instantiate blanket implementations that fail
    /// typechecking
    pub compile_errors: Vec<TyperError>,
}

impl TypedAbilityImpl {
    pub fn function_at_index(&self, index: usize) -> FunctionId {
        self.functions[index]
    }
}

pub struct FunctionAbilityImplContextInfo {
    pub self_type_id: TypeId,
    pub impl_kind: AbilityImplKind,
    pub blanket_parent_function: Option<FunctionId>,
}

// Passed to eval_function_declaration to inform
// behavior
pub struct FunctionAbilityContextInfo {
    ability_id: AbilityId,
    impl_info: Option<FunctionAbilityImplContextInfo>,
}

impl FunctionAbilityContextInfo {
    pub fn ability_id_only(ability_id: AbilityId) -> Self {
        FunctionAbilityContextInfo { ability_id, impl_info: None }
    }

    pub fn ability_impl(
        ability_id: AbilityId,
        self_type_id: TypeId,
        impl_kind: AbilityImplKind,
        blanket_parent_function: Option<FunctionId>,
    ) -> Self {
        FunctionAbilityContextInfo {
            ability_id,
            impl_info: Some(FunctionAbilityImplContextInfo {
                self_type_id,
                impl_kind,
                blanket_parent_function,
            }),
        }
    }
}

#[derive(Default, Debug)]
pub struct Variables {
    variables: Vec<Variable>,
}

impl Variables {
    fn add_variable(&mut self, typ: Variable) -> VariableId {
        let index = self.variables.len();
        self.variables.push(typ);
        VariableId(index as u32)
    }

    pub fn get(&self, variable_id: VariableId) -> &Variable {
        &self.variables[variable_id.0 as usize]
    }

    pub fn iter(&self) -> impl Iterator<Item = (VariableId, &Variable)> {
        self.variables.iter().enumerate().map(|(i, v)| (VariableId(i as u32), v))
    }
}

#[derive(Debug, Clone)]
struct EvalTypeExprContext {
    should_attach_defn_info: bool,
    /// If this is a type definition, this is the type definition info
    /// that should be attached to the type, if `should_attach_defn_info` is true
    pub inner_type_defn_info: Option<TypeDefnInfo>,
}

impl EvalTypeExprContext {
    pub fn attached_type_defn_info(&self) -> Option<TypeDefnInfo> {
        if self.should_attach_defn_info {
            self.inner_type_defn_info.clone()
        } else {
            None
        }
    }

    pub fn no_attach_defn_info(&self) -> Self {
        let mut copy = self.clone();
        copy.should_attach_defn_info = false;
        copy
    }

    pub const EMPTY: Self =
        EvalTypeExprContext { should_attach_defn_info: false, inner_type_defn_info: None };
}

// Not using this yet but probably need to be
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub namespace: NamespaceId,
    pub identifier: Identifier,
}

#[derive(Debug, Clone, Copy)]
pub enum UseStatus {
    Unresolved,
    Resolved(UseableSymbol),
}

impl UseStatus {
    pub fn is_resolved(&self) -> bool {
        match self {
            UseStatus::Resolved(_) => true,
            UseStatus::Unresolved => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AbilityImplHandle {
    ability_id: AbilityId,
    full_impl_id: AbilityImplId,
}

pub struct TypedModule {
    pub ast: ParsedModule,
    functions: Vec<TypedFunction>,
    pub variables: Variables,
    pub types: Types,
    pub constants: Vec<Constant>,
    pub exprs: pool::Pool<TypedExpr, TypedExprId>,
    pub stmts: pool::Pool<TypedStmt, TypedStmtId>,
    pub scopes: Scopes,
    pub errors: Vec<TyperError>,
    pub namespaces: Namespaces,
    pub abilities: Vec<TypedAbility>,
    pub ability_impls: Vec<TypedAbilityImpl>,
    /// Key is 'self' type
    pub ability_impl_table: FxHashMap<TypeId, Vec<AbilityImplHandle>>,
    /// Key is base ability id
    pub blanket_impls: FxHashMap<AbilityId, Vec<AbilityImplId>>,
    pub namespace_ast_mappings: FxHashMap<ParsedNamespaceId, NamespaceId>,
    pub function_ast_mappings: FxHashMap<ParsedFunctionId, FunctionId>,
    pub ability_impl_ast_mappings: FxHashMap<ParsedAbilityImplId, AbilityImplId>,
    /// We don't know about functions during the type discovery phase, so a 'use'
    /// that targets a function could miss. Rather than make the user
    /// specify 'use type' vs 'use fn', etc, we just keep track of
    /// whether a 'use' ever hits. If a use never hits, we'll report
    /// an error after the last phase where handle them, which should be
    /// function declarations
    pub use_statuses: FxHashMap<ParsedUseId, UseStatus>,
    pub debug_level_stack: Vec<log::LevelFilter>,
    pub functions_pending_body_specialization: Vec<FunctionId>,
    inference_context: InferenceContext,
}

impl TypedModule {
    pub fn new(parsed_module: ParsedModule) -> TypedModule {
        let types = Types {
            types: Vec::with_capacity(8192),
            existing_types_mapping: FxHashMap::new(),
            type_defn_mapping: FxHashMap::new(),
            type_variable_counts: FxHashMap::new(),
            ability_mapping: FxHashMap::new(),
            placeholder_mapping: FxHashMap::new(),
        };

        let scopes = Scopes::make();
        let namespaces = Namespaces { namespaces: Pool::with_capacity("namespaces", 256) };
        TypedModule {
            functions: Vec::with_capacity(parsed_module.functions.len() * 4),
            variables: Variables::default(),
            types,
            constants: Vec::new(),
            exprs: Pool::with_capacity("typed_exprs", 32768),
            stmts: Pool::with_capacity("typed_stmts", 8192),
            scopes,
            errors: Vec::new(),
            namespaces,
            abilities: Vec::with_capacity(parsed_module.abilities.len() * 2),
            ability_impls: Vec::with_capacity(parsed_module.ability_impls.len() * 2),
            ability_impl_table: FxHashMap::new(),
            blanket_impls: FxHashMap::new(),
            namespace_ast_mappings: FxHashMap::with_capacity(parsed_module.namespaces.len() * 2),
            function_ast_mappings: FxHashMap::with_capacity(parsed_module.functions.len() * 2),
            ability_impl_ast_mappings: FxHashMap::new(),
            use_statuses: FxHashMap::new(),
            debug_level_stack: vec![log::max_level()],
            functions_pending_body_specialization: vec![],
            ast: parsed_module,
            inference_context: InferenceContext {
                origin_stack: Vec::with_capacity(64),
                vars: Vec::with_capacity(128),
                constraints: Vec::with_capacity(128),
                substitutions: FxHashMap::with_capacity(256),
                substitutions_vec: Vec::with_capacity(256),
            },
        }
    }

    fn push_debug_level(&mut self) {
        let level = log::LevelFilter::Debug;
        self.debug_level_stack.push(level);
        log::set_max_level(level);
        debug!("push max_level is now {}", log::max_level())
    }

    fn pop_debug_level(&mut self) {
        self.debug_level_stack.pop();
        log::set_max_level(*self.debug_level_stack.last().unwrap());
        debug!("pop max_level is now {}", log::max_level())
    }

    pub fn function_iter(&self) -> impl Iterator<Item = (FunctionId, &TypedFunction)> {
        self.functions.iter().enumerate().map(|(idx, f)| (FunctionId(idx as u32), f))
    }

    pub fn name(&self) -> &str {
        &self.ast.name
    }

    pub fn name_of(&self, id: Identifier) -> &str {
        self.ast.identifiers.get_name(id)
    }

    pub fn get_identifier(&self, name: &str) -> Option<Identifier> {
        self.ast.identifiers.get(name)
    }

    pub fn get_namespace_scope(&self, namespace_id: NamespaceId) -> &Scope {
        let scope_id = self.namespaces.get_scope(namespace_id);
        self.scopes.get_scope(scope_id)
    }

    pub fn get_main_function_id(&self) -> Option<FunctionId> {
        self.scopes.get_root_scope().find_function(get_ident!(self, "main"))
    }

    fn push_block_stmt_id(&self, block: &mut TypedBlock, stmt: TypedStmtId) {
        block.expr_type = self.get_stmt_type(stmt);
        block.statements.push(stmt);
    }

    fn push_block_stmt(&mut self, block: &mut TypedBlock, stmt: TypedStmt) {
        let id = self.stmts.add(stmt);
        block.expr_type = self.get_stmt_type(id);
        block.statements.push(id);
    }

    pub fn get_stmt_type(&self, stmt: TypedStmtId) -> TypeId {
        match self.stmts.get(stmt) {
            TypedStmt::Expr(_, ty) => *ty,
            TypedStmt::Let(val_def) => {
                if val_def.variable_type == NEVER_TYPE_ID {
                    NEVER_TYPE_ID
                } else {
                    UNIT_TYPE_ID
                }
            }
            TypedStmt::Assignment(assgn) => {
                if self.exprs.get(assgn.value).get_type() == NEVER_TYPE_ID {
                    NEVER_TYPE_ID
                } else {
                    UNIT_TYPE_ID
                }
            }
        }
    }

    pub fn get_stmt_span(&self, stmt: TypedStmtId) -> SpanId {
        match self.stmts.get(stmt) {
            // Ugh 2 lookups for a span
            TypedStmt::Expr(e, _ty) => self.exprs.get(*e).get_span(),
            TypedStmt::Let(val_def) => val_def.span,
            TypedStmt::Assignment(assgn) => assgn.span,
        }
    }

    fn add_expr_stmt(&mut self, expr: TypedExpr) -> TypedStmtId {
        let type_id = expr.get_type();
        let id = self.exprs.add(expr);
        self.stmts.add(TypedStmt::Expr(id, type_id))
    }

    fn add_expr_to_block(&mut self, block: &mut TypedBlock, expr: TypedExpr) {
        let stmt_id = self.add_expr_stmt(expr);
        self.push_block_stmt_id(block, stmt_id)
    }

    fn add_expr_id_to_block(&mut self, block: &mut TypedBlock, expr: TypedExprId) {
        let ty = self.exprs.get(expr).get_type();
        self.push_block_stmt(block, TypedStmt::Expr(expr, ty))
    }

    fn next_ability_id(&self) -> AbilityId {
        AbilityId(self.abilities.len() as u32)
    }

    fn add_ability(&mut self, ability: TypedAbility) -> AbilityId {
        let ability_id = self.next_ability_id();
        self.abilities.push(ability);
        ability_id
    }

    fn get_ability(&self, ability_id: AbilityId) -> &TypedAbility {
        &self.abilities[ability_id.0 as usize]
    }

    fn get_ability_mut(&mut self, ability_id: AbilityId) -> &mut TypedAbility {
        &mut self.abilities[ability_id.0 as usize]
    }

    // New recursive types design:
    // - recursive references stay, because its actually better for codegen
    // - Use ParsedTypeDefnId as payload for uniqueness of the RecursiveReference type
    // - codegen_type will stop at recursive references and do an opaque struct
    // - Require that we're inside a struct or enum root in order to do a recursive definition
    // - Update the RecursiveReference to point to the real type_id once eval comes back
    fn eval_type_defn(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        let parsed_type_defn = self.ast.get_type_defn(parsed_type_defn_id).clone();
        let existing_defn = self.types.find_type_defn_mapping(parsed_type_defn_id);
        if let Some(existing_defn) = existing_defn {
            return Ok(existing_defn);
        }

        // Find companion namespace if exists and update type_defn_info
        let companion_namespace_id =
            self.scopes.get_scope(scope_id).find_namespace(parsed_type_defn.name);

        // The type defn info is about a lot more than just definition site.
        // It differentiates between simple names for shapes of structs and
        // structs with an actual named-based identity that can have methods, implement traits, etc.
        let type_defn_info = TypeDefnInfo {
            name: parsed_type_defn.name,
            scope: scope_id,
            companion_namespace: companion_namespace_id,
            ast_id: parsed_type_defn_id.into(),
        };

        let has_type_params = !parsed_type_defn.type_params.is_empty();

        let should_not_attach_defn_info = has_type_params
            || (parsed_type_defn.flags.is_alias() && !parsed_type_defn.flags.is_opaque());
        let should_attach_defn_info = !should_not_attach_defn_info;

        let defn_scope_id = self.scopes.add_child_scope(
            scope_id,
            ScopeType::TypeDefn,
            None,
            Some(parsed_type_defn.name),
        );
        let mut type_params: Vec<GenericTypeParam> =
            Vec::with_capacity(parsed_type_defn.type_params.len());
        // let mut type_params: BVec<GenericTypeParam> =
        //     BVec::with_capacity_in(parsed_type_defn.type_params.len(), &self.bump);
        for type_param in parsed_type_defn.type_params.iter() {
            let type_variable_id = self.add_type_variable(
                TypeParameter {
                    name: type_param.name,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                    function_type: None,
                },
                smallvec![],
            );
            type_params.push(GenericTypeParam {
                name: type_param.name,
                type_id: type_variable_id,
                span: type_param.span,
            });
            let added = self
                .scopes
                .get_scope_mut(defn_scope_id)
                .add_type(type_param.name, type_variable_id);
            if !added {
                return failf!(
                    type_param.span,
                    "Type variable name '{}' is taken",
                    self.name_of(type_param.name).blue()
                );
            }
        }

        let type_eval_context = EvalTypeExprContext {
            should_attach_defn_info,
            inner_type_defn_info: Some(type_defn_info.clone()),
        };

        // The big evaluation!
        let resulting_type_id =
            self.eval_type_expr_ext(parsed_type_defn.value_expr, defn_scope_id, type_eval_context)?;

        // If this was a recursive definition, do a replacement
        if let Some(placeholder_id) = self.types.placeholder_mapping.get(&parsed_type_defn_id) {
            self.types.get_mut(*placeholder_id).as_recursive_reference().root_type_id =
                Some(resulting_type_id);
        }

        let type_id = if has_type_params {
            let gen = GenericType {
                params: type_params,
                inner: resulting_type_id,
                type_defn_info,
                specializations: HashMap::new(),
            };
            Ok(self.types.add_type(Type::Generic(gen)))
        } else if parsed_type_defn.flags.is_alias() {
            // Transparent alias
            match self.types.get(resulting_type_id) {
                Type::Never(_) => {
                    failf!(parsed_type_defn.span, "Why would you alias 'never'")
                }
                _ => Ok(resulting_type_id),
            }
        } else {
            // 'New type' territory; must be a named struct/enum OR a builtin
            match self.types.get(resulting_type_id) {
                Type::Unit(_)
                | Type::Char(_)
                | Type::Bool(_)
                | Type::Never(_)
                | Type::Pointer(_)
                | Type::Integer(_)
                | Type::Float(_) => Ok(resulting_type_id),
                Type::Struct(_s) => Ok(resulting_type_id),
                Type::Enum(_e) => Ok(resulting_type_id),
                _other => {
                    failf!(parsed_type_defn.span, "Non-alias type definition must be a struct or enum or builtin; perhaps you meant to create an alias `deftype alias <name> = <type>`")
                }
            }
        }?;
        self.types.add_type_defn_mapping(parsed_type_defn_id, type_id);

        let type_added = self.scopes.add_type(scope_id, parsed_type_defn.name, type_id);
        if !type_added {
            return failf!(
                parsed_type_defn.span,
                "Duplicate type definition: {}",
                self.name_of(parsed_type_defn.name)
            );
        };
        if let Some(companion_namespace_id) = companion_namespace_id {
            self.namespaces.get_mut(companion_namespace_id).companion_type_id = Some(type_id);
        }

        let removed = self.scopes.remove_pending_type_defn(scope_id, parsed_type_defn.name);
        if !removed {
            panic_at_disco!("Failed to remove pending type defn");
        }

        Ok(type_id)
    }

    fn eval_type_expr(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        self.eval_type_expr_ext(type_expr_id, scope_id, EvalTypeExprContext::EMPTY.clone())
    }

    fn eval_type_expr_ext(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
        // The context is mostly for when we are evaluating a type expression that is part of a type
        // definition. It talks about self_name for recursive definitions, and the definition info
        // like companion namespace, etc for a type definition.
        context: EvalTypeExprContext,
    ) -> TyperResult<TypeId> {
        let base = match self.ast.type_exprs.get(type_expr_id) {
            ParsedTypeExpr::Builtin(span) => {
                let defn_info =
                    context.inner_type_defn_info.expect("required defn info for builtin");
                let name = defn_info.name;
                match self.name_of(name) {
                    "unit" => {
                        let id = self.types.add_type(Type::Unit(defn_info));
                        assert!(id == UNIT_TYPE_ID);
                        Ok(id)
                    }
                    "char" => {
                        let id = self.types.add_type(Type::Char(defn_info));
                        assert!(id == CHAR_TYPE_ID);
                        Ok(id)
                    }
                    "bool" => {
                        let id = self.types.add_type(Type::Bool(defn_info));
                        assert!(id == BOOL_TYPE_ID);
                        Ok(id)
                    }
                    "never" => {
                        let id = self.types.add_type(Type::Never(defn_info));
                        assert!(id == NEVER_TYPE_ID);
                        Ok(id)
                    }
                    "Pointer" => {
                        let id = self.types.add_type(Type::Pointer(defn_info));
                        assert!(id == POINTER_TYPE_ID);
                        Ok(id)
                    }
                    "f32" => {
                        let id = self.types.add_type(Type::Float(FloatType {
                            size: NumericWidth::B32,
                            defn_info,
                        }));
                        assert!(id == F32_TYPE_ID);
                        Ok(id)
                    }
                    "f64" => {
                        let id = self.types.add_type(Type::Float(FloatType {
                            size: NumericWidth::B64,
                            defn_info,
                        }));
                        assert!(id == F64_TYPE_ID);
                        Ok(id)
                    }
                    "u8" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::U8));
                        assert!(id == U8_TYPE_ID);
                        Ok(id)
                    }
                    "u16" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::U16));
                        assert!(id == U16_TYPE_ID);
                        Ok(id)
                    }
                    "u32" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::U32));
                        assert!(id == U32_TYPE_ID);
                        Ok(id)
                    }
                    "u64" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::U64));
                        assert!(id == U64_TYPE_ID);
                        Ok(id)
                    }
                    "i8" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::I8));
                        assert!(id == I8_TYPE_ID);
                        Ok(id)
                    }
                    "i16" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::I16));
                        assert!(id == I16_TYPE_ID);
                        Ok(id)
                    }
                    "i32" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::I32));
                        assert!(id == I32_TYPE_ID);
                        Ok(id)
                    }
                    "i64" => {
                        let id = self.types.add_type(Type::Integer(IntegerType::I64));
                        assert!(id == I64_TYPE_ID);
                        Ok(id)
                    }
                    _ => failf!(*span, "Unknown builtin type '{}'", name),
                }
            }
            ParsedTypeExpr::Struct(struct_defn) => {
                let struct_defn = struct_defn.clone();
                let mut fields: Vec<StructTypeField> = Vec::new();
                for (index, ast_field) in struct_defn.fields.iter().enumerate() {
                    let ty = self.eval_type_expr_ext(
                        ast_field.ty,
                        scope_id,
                        context.no_attach_defn_info(),
                    )?;
                    // TODO(infinite recursive types): This is kinda how we'd prevent this
                    // if let Type::RecursiveReference(rr) = self.types.get_no_follow(ty) {
                    //     if Some(rr.parsed_id)
                    //         == context.inner_type_defn_info.as_ref().map(|i| i.ast_id)
                    //     {
                    //         return make_fail_span(
                    //             "Recursive references require some indirection",
                    //             self.ast.get_type_expression_span(ast_field.ty),
                    //         );
                    //     }
                    // }
                    fields.push(StructTypeField {
                        name: ast_field.name,
                        type_id: ty,
                        index: index as u32,
                        private: ast_field.private,
                    })
                }
                let struct_defn = StructType {
                    fields,
                    type_defn_info: context.attached_type_defn_info(),
                    generic_instance_info: None,
                    ast_node: type_expr_id.into(),
                };
                let type_id = self.types.add_type(Type::Struct(struct_defn));
                Ok(type_id)
            }
            ParsedTypeExpr::TypeApplication(_ty_app) => {
                let type_op_result =
                    self.detect_and_eval_type_operator(type_expr_id, scope_id, context.clone())?;
                match type_op_result {
                    None => self.eval_type_application(type_expr_id, scope_id, context),
                    Some(type_op_result) => Ok(type_op_result),
                }
            }
            ParsedTypeExpr::Optional(opt) => {
                let inner_ty =
                    self.eval_type_expr_ext(opt.base, scope_id, context.no_attach_defn_info())?;
                let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, vec![inner_ty]);
                Ok(optional_type)
            }
            ParsedTypeExpr::Reference(r) => {
                let inner_ty =
                    self.eval_type_expr_ext(r.base, scope_id, context.no_attach_defn_info())?;
                let reference_type = Type::Reference(ReferenceType { inner_type: inner_ty });
                let type_id = self.types.add_type(reference_type);
                Ok(type_id)
            }
            ParsedTypeExpr::Enum(e) => {
                let e = e.clone();
                let mut variants = Vec::with_capacity(e.variants.len());
                for (index, v) in e.variants.iter().enumerate() {
                    let payload_type_id = match &v.payload_expression {
                        None => None,
                        Some(payload_type_expr) => {
                            let type_id = self.eval_type_expr_ext(
                                *payload_type_expr,
                                scope_id,
                                context.no_attach_defn_info(),
                            )?;
                            // TODO(infinite recursive types): This is kinda how we'd prevent infinite recursion
                            // if let Type::RecursiveReference(rr) = self.types.get_no_follow(type_id)
                            // {
                            //     if Some(rr.parsed_id)
                            //         == context.inner_type_defn_info.as_ref().map(|i| i.ast_id)
                            //     {
                            //         return make_fail_span(
                            //             "Recursive references requires some indirection",
                            //             self.ast.get_type_expression_span(*payload_type_expr),
                            //         );
                            //     }
                            // }
                            Some(type_id)
                        }
                    };
                    let variant = TypedEnumVariant {
                        enum_type_id: TypeId::PENDING,
                        my_type_id: TypeId::PENDING,
                        name: v.tag_name,
                        index: index as u32,
                        payload: payload_type_id,
                        type_defn_info: context.attached_type_defn_info().clone(),
                    };
                    variants.push(variant);
                }
                let enum_type = Type::Enum(TypedEnum {
                    variants,
                    type_defn_info: context.attached_type_defn_info(),
                    generic_instance_info: None,
                    ast_node: type_expr_id.into(),
                    explicit_tag_type: None,
                });
                let enum_type_id = self.types.add_type(enum_type);
                Ok(enum_type_id)
            }
            ParsedTypeExpr::DotMemberAccess(dot_acc) => {
                let dot_acc = dot_acc.clone();
                let base_type =
                    self.eval_type_expr_ext(dot_acc.base, scope_id, context.no_attach_defn_info())?;
                if let Some(spec_info) = self.types.get_generic_instance_info(base_type) {
                    let generic = self.types.get(spec_info.generic_parent).expect_generic();
                    let type_params = &generic.params;
                    if let Some(matching_type_var_pos) =
                        type_params.iter().position(|tp| tp.name == dot_acc.member_name)
                    {
                        let actual_type = &spec_info.param_values[matching_type_var_pos];
                        return Ok(*actual_type);
                    }
                }
                match self.types.get(base_type) {
                    // You can do dot access on enums to get their variants
                    Type::Enum(e) => {
                        let Some(matching_variant) =
                            e.variants.iter().find(|v| v.name == dot_acc.member_name)
                        else {
                            return failf!(
                                dot_acc.span,
                                "Variant '{}' does not exist on Enum '{}'",
                                self.name_of(dot_acc.member_name),
                                self.type_id_to_string(base_type)
                            );
                        };
                        let variant_type = matching_variant.my_type_id;
                        Ok(variant_type)
                    }
                    Type::EnumVariant(ev) => {
                        if self.ast.identifiers.get_name(dot_acc.member_name) != "value" {
                            failf!(
                                dot_acc.span,
                                "Invalid member access on EnumVariant type; try '.value'",
                            )
                        } else if let Some(payload) = ev.payload {
                            Ok(payload)
                        } else {
                            failf!(dot_acc.span, "Variant has no payload value")
                        }
                    }
                    // You can do dot access on structs to get their members!
                    Type::Struct(s) => {
                        let Some(field) = s.find_field(dot_acc.member_name) else {
                            return failf!(
                                dot_acc.span,
                                "Field {} does not exist on struct {}",
                                self.name_of(dot_acc.member_name),
                                self.type_id_to_string(base_type)
                            );
                        };
                        Ok(field.1.type_id)
                    }
                    // You can do dot access on References to get out their 'value' types
                    Type::Reference(r) => {
                        if self.ast.identifiers.get_name(dot_acc.member_name) != "value" {
                            return make_fail_ast_id(
                                &self.ast,
                                "Invalid member access on Optional type; try '.value'",
                                type_expr_id.into(),
                            );
                        }
                        Ok(r.inner_type)
                    }
                    Type::Function(fun) => {
                        let member_name = self.ast.identifiers.get_name(dot_acc.member_name);
                        match member_name {
                            "return" => Ok(fun.return_type),
                            _other => {
                                if let Some(param) =
                                    fun.params.iter().find(|p| p.name == dot_acc.member_name)
                                {
                                    Ok(param.type_id)
                                } else {
                                    return make_fail_ast_id(
                                        &self.ast,
                                        format!("Function has no parameter named {}", member_name),
                                        type_expr_id.into(),
                                    );
                                }
                            }
                        }
                    }
                    _ => {
                        return make_fail_ast_id(
                            &self.ast,
                            format!(
                                "Invalid type for '.' access: {}",
                                self.type_id_to_string(base_type)
                            ),
                            type_expr_id.into(),
                        )
                    }
                }
            }
            // When a function type is written explicitly, it is interpreted as
            // a function object type, which is always a lambda, which is a
            // 2 member struct with a function pointer and an environment pointer
            ParsedTypeExpr::Function(fun_type) => {
                let fun_type = fun_type.clone();
                let mut params: Vec<FnParamType> = Vec::with_capacity(fun_type.params.len());

                //let empty_struct_id = self.types.add_type(Type::Struct(StructType {
                //    type_defn_info: None,
                //    fields: vec![],
                //    ast_node: type_expr_id.into(),
                //    generic_instance_info: None,
                //}));
                // let empty_struct_reference_id = self.types.add_reference_type(empty_struct_id);
                // params.push(FnParamType {
                //     type_id: empty_struct_reference_id,
                //     name: get_ident!(self, LAMBDA_ENV_PARAM_NAME),
                //     is_context: false,
                //     is_lambda_env: true,
                //     span: fun_type.span,
                // });

                for (index, param) in fun_type.params.iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;
                    let span = self.ast.get_type_expression_span(*param);
                    let name = self.ast.identifiers.intern(format!("param_{}", index));
                    params.push(FnParamType {
                        type_id,
                        name,
                        is_context: false,
                        is_lambda_env: false,
                        span,
                    });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                let function_type_id = self.types.add_type(Type::Function(FunctionType {
                    params,
                    return_type,
                    defn_info: None,
                }));
                Ok(function_type_id)
            }
            ParsedTypeExpr::TypeOf(tof) => {
                let expr =
                    self.eval_expr(tof.target_expr, EvalExprContext::from_scope(scope_id))?;
                let ty = self.exprs.get(expr);
                Ok(ty.get_type())
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                let span = quant.span;
                let inner = self.eval_type_expr(quant.inner, scope_id)?;
                let Type::Function(_function_type) = self.types.get(inner) else {
                    return failf!(span, "Expected a function type following 'some'");
                };
                let name = self.ast.identifiers.intern(format!("some_fn_{}", inner));
                let function_type_variable = self.add_type_variable(
                    TypeParameter { name, scope_id, span, function_type: Some(inner) },
                    smallvec![],
                );
                Ok(function_type_variable)
            }
        }?;
        Ok(base)
    }

    /// Temporary home for our type operators until I decide on syntax
    fn detect_and_eval_type_operator(
        &mut self,
        ty_app_id: ParsedTypeExprId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> TyperResult<Option<TypeId>> {
        let ParsedTypeExpr::TypeApplication(ty_app) = self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };
        if !ty_app.name.namespaces.is_empty() {
            return Ok(None);
        }
        let ty_app = ty_app.clone();
        match self.name_of(ty_app.name.name) {
            "dyn" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 1 type parameter for dyn");
                }
                let inner = self.eval_type_expr_ext(
                    ty_app.args[0].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
                if self.types.get(inner).as_function().is_none() {
                    return failf!(
                        ty_app.span,
                        "Expected function type, or eventually, ability name, for dyn"
                    );
                }
                let lambda_object_type =
                    self.types.add_lambda_object(&self.ast.identifiers, inner, ty_app_id.into());
                Ok(Some(lambda_object_type))
            }
            "_struct_combine" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_combine");
                }
                let arg1 = self.eval_type_expr_ext(
                    ty_app.args[0].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
                let arg2 = self.eval_type_expr_ext(
                    ty_app.args[1].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?;

                let mut combined_fields =
                    Vec::with_capacity(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(struct1.fields.clone());
                for field in struct2.fields.iter() {
                    let collision = combined_fields.iter().find(|f| f.name == field.name);
                    if let Some(collision) = collision {
                        if collision.type_id != field.type_id {
                            return failf!(
                                ty_app.span,
                                "Field '{}' has conflicting types in the two structs",
                                self.name_of(field.name).blue()
                            );
                        }
                    }
                    let mut field = field.clone();
                    field.index = combined_fields.len() as u32;
                    combined_fields.push(field);
                }

                let new_struct = Type::Struct(StructType {
                    fields: combined_fields,
                    type_defn_info: context.attached_type_defn_info(),
                    generic_instance_info: None,
                    ast_node: ParsedId::TypeExpression(ty_app_id),
                });
                let type_id = self.types.add_type(new_struct);
                eprintln!("Combined struct: {}", self.type_id_to_string(type_id));

                Ok(Some(type_id))
            }
            "_struct_remove" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_remove");
                }
                let arg1 = self.eval_type_expr_ext(
                    ty_app.args[0].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
                let arg2 = self.eval_type_expr_ext(
                    ty_app.args[1].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?;
                let mut new_fields = struct1.fields.clone();
                new_fields.retain(|f| !struct2.fields.iter().any(|sf| sf.name == f.name));
                let mut new_struct = StructType {
                    fields: new_fields,
                    type_defn_info: context.attached_type_defn_info(),
                    generic_instance_info: None,
                    ast_node: ParsedId::TypeExpression(ty_app_id),
                };
                for (i, field) in new_struct.fields.iter_mut().enumerate() {
                    field.index = i as u32;
                }
                let type_id = self.types.add_type(Type::Struct(new_struct));
                Ok(Some(type_id))
            }
            _ => Ok(None),
        }
    }

    fn eval_type_application(
        &mut self,
        ty_app_id: ParsedTypeExprId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> TyperResult<TypeId> {
        let ParsedTypeExpr::TypeApplication(ty_app) = self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };

        let ty_app_name = &ty_app.name;
        let name = ty_app_name.name;
        match self.scopes.find_type_namespaced(
            scope_id,
            ty_app_name,
            &self.namespaces,
            &self.ast.identifiers,
        )? {
            Some((type_id, _)) => {
                if let Type::Generic(g) = self.types.get(type_id) {
                    if ty_app.args.len() != g.params.len() {
                        return failf!(
                            ty_app.span,
                            "Type {} expects {} type arguments, got {}",
                            self.namespaced_identifier_to_string(&ty_app.name),
                            g.params.len(),
                            ty_app.args.len()
                        );
                    }
                    let mut type_arguments: Vec<TypeId> = Vec::with_capacity(ty_app.args.len());
                    for parsed_param in ty_app.args.clone().iter() {
                        let param_type_id = self.eval_type_expr_ext(
                            parsed_param.type_expr,
                            scope_id,
                            context.no_attach_defn_info(),
                        )?;
                        type_arguments.push(param_type_id);
                    }
                    Ok(self.instantiate_generic_type(type_id, type_arguments))
                } else {
                    Ok(self.get_type_id_resolved(type_id, scope_id))
                }
            }
            None => {
                match self.scopes.find_function_namespaced(
                    scope_id,
                    ty_app_name,
                    &self.namespaces,
                    &self.ast.identifiers,
                )? {
                    Some(function_id) => Ok(self.get_function(function_id).type_id),
                    None => {
                        // Checks for recursion by name; does not work with namespaced names (?)
                        // , JsList(List[Json]) -> , JsList(_root::List[Json])
                        if context.inner_type_defn_info.as_ref().map(|info| info.name) == Some(name)
                        {
                            let type_defn_info = context.inner_type_defn_info.as_ref().unwrap();
                            let type_defn_id = type_defn_info.ast_id.expect_type_defn();
                            let placeholder_type_id =
                                match self.types.placeholder_mapping.get(&type_defn_id) {
                                    None => {
                                        eprintln!(
                                            "Inserting recursive reference for {}",
                                            self.name_of(name)
                                        );
                                        let type_id = self.types.add_type(
                                            Type::RecursiveReference(RecursiveReference {
                                                parsed_id: type_defn_id,
                                                root_type_id: None,
                                            }),
                                        );
                                        self.types
                                            .placeholder_mapping
                                            .insert(type_defn_id, type_id);
                                        type_id
                                    }
                                    Some(type_id) => *type_id,
                                };
                            Ok(placeholder_type_id)
                        } else {
                            match self.scopes.find_pending_type_defn(scope_id, name) {
                                None => {
                                    failf!(
                                        ty_app.span,
                                        "No type named {} is in scope",
                                        self.name_of(name),
                                    )
                                }
                                Some((pending_defn_id, pending_defn_scope_id)) => {
                                    eprintln!(
                                        "Recursing into pending type defn {}",
                                        self.name_of(self.ast.get_type_defn(pending_defn_id).name)
                                    );
                                    let type_id = self
                                        .eval_type_defn(pending_defn_id, pending_defn_scope_id)?;
                                    Ok(type_id)
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn instantiate_generic_type(
        &mut self,
        generic_type: TypeId,
        type_arguments: Vec<TypeId>,
    ) -> TypeId {
        let gen = self.types.get(generic_type).expect_generic();
        match gen.specializations.get(&type_arguments) {
            Some(existing) => {
                debug!(
                    "Using cached generic instance {} for {} args {:?}",
                    self.type_id_to_string(*existing),
                    self.name_of(gen.type_defn_info.name),
                    type_arguments
                        .clone()
                        .iter()
                        .map(|p| self.type_id_to_string_ext(*p, false))
                        .collect::<Vec<_>>(),
                );
                *existing
            }
            None => {
                debug_assert!(gen.params.len() == type_arguments.len());
                let type_defn_info = gen.type_defn_info.clone();
                // Note: This is where we'd check constraints on the pairs:
                // that each passed params meets the constraints of the generic param
                let substitution_pairs: Vec<TypeSubstitutionPair> = gen
                    .params
                    .iter()
                    .zip(&type_arguments)
                    .map(|(type_param, passed_type_arg)| TypeSubstitutionPair {
                        from: type_param.type_id,
                        to: *passed_type_arg,
                    })
                    .collect();
                let inner = gen.inner;

                let specialized_type = self.substitute_in_type(
                    inner,
                    &substitution_pairs,
                    Some(generic_type),
                    Some(type_defn_info),
                );
                if log::log_enabled!(log::Level::Debug) {
                    let gen = self.types.get(generic_type).expect_generic();
                    let inst_info = &self
                        .types
                        .get_generic_instance_info(specialized_type)
                        .unwrap()
                        .param_values;
                    debug!(
                        "instantiated {} with params {} got expanded type: {}",
                        self.name_of(gen.type_defn_info.name),
                        self.pretty_print_types(inst_info, ", "),
                        self.type_id_to_string_ext(specialized_type, true)
                    );
                }
                if let Type::Generic(gen) = self.types.get_mut(generic_type) {
                    gen.specializations.insert(type_arguments, specialized_type);
                };
                specialized_type
            }
        }
    }

    fn substitute_in_type(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        generic_parent_to_attach: Option<TypeId>,
        defn_info_to_attach: Option<TypeDefnInfo>,
    ) -> TypeId {
        // nocommit: Optimize the case where all the pairs are type variables or inference holes,
        // and the target type has none, it's a no-op!

        let force_new = defn_info_to_attach.is_some();
        // If this type is already a generic instance of something, just
        // re-specialize it on the right inputs. So find out what the new value
        // of each type param should be and call instantiate_generic_type
        //
        // This happens when specializing a type that contains an Opt[T], for example.
        // This lets us hit our cache as well

        if let Some(spec_info) = self.types.get_generic_instance_info(type_id) {
            // A,   B,    T
            // int, bool, char
            // Opt[T] -> Opt[char]
            let generic_parent = spec_info.generic_parent;
            let new_parameter_values: Vec<TypeId> = spec_info
                .param_values
                .clone()
                .iter()
                .map(|prev_type_id| {
                    self.substitute_in_type(*prev_type_id, substitution_pairs, None, None)
                })
                .collect();
            return self.instantiate_generic_type(generic_parent, new_parameter_values);
        };

        let matching_subst_pair = substitution_pairs.iter().find(|pair| pair.from == type_id);
        if let Some(matching_pair) = matching_subst_pair {
            return matching_pair.to;
        }

        match self.types.get(type_id) {
            Type::Struct(struc) => {
                let mut new_fields = struc.fields.clone();
                let mut any_change = false;
                let struc_ast_node = struc.ast_node;
                let original_defn_info = struc.type_defn_info.clone();
                let original_instance_info = struc.generic_instance_info.clone();
                for field in new_fields.iter_mut() {
                    let new_field_type_id =
                        self.substitute_in_type(field.type_id, substitution_pairs, None, None);
                    if new_field_type_id != field.type_id {
                        any_change = true;
                    }
                    field.type_id = new_field_type_id;
                }
                if force_new || any_change {
                    let generic_instance_info = generic_parent_to_attach
                        .map(|parent| GenericInstanceInfo {
                            generic_parent: parent,
                            param_values: substitution_pairs.iter().map(|p| p.to).collect(),
                        })
                        .or(original_instance_info);
                    let specialized_struct = StructType {
                        fields: new_fields,
                        type_defn_info: defn_info_to_attach.or(original_defn_info),
                        generic_instance_info,
                        ast_node: struc_ast_node,
                    };
                    self.types.add_type(Type::Struct(specialized_struct))
                } else {
                    type_id
                }
            }
            Type::Enum(e) => {
                let mut new_variants = e.variants.clone();
                let mut any_changed = false;
                let original_ast_node = e.ast_node;
                let original_defn_info = e.type_defn_info.clone();
                let original_instance_info = e.generic_instance_info.clone();
                let original_explicit_tag_type = e.explicit_tag_type;
                for variant in new_variants.iter_mut() {
                    let new_payload_id = variant.payload.map(|payload_type_id| {
                        self.substitute_in_type(payload_type_id, substitution_pairs, None, None)
                    });
                    if force_new || new_payload_id != variant.payload {
                        any_changed = true;
                        variant.payload = new_payload_id;
                        if let Some(defn_info_to_attach) = defn_info_to_attach.as_ref() {
                            variant.type_defn_info = Some(defn_info_to_attach.clone());
                        };
                    }
                }
                if force_new || any_changed {
                    let generic_instance_info = generic_parent_to_attach
                        .map(|parent| GenericInstanceInfo {
                            generic_parent: parent,
                            param_values: substitution_pairs.iter().map(|p| p.to).collect(),
                        })
                        .or(original_instance_info);
                    let new_enum = TypedEnum {
                        variants: new_variants,
                        ast_node: original_ast_node,
                        generic_instance_info,
                        type_defn_info: defn_info_to_attach.or(original_defn_info),
                        explicit_tag_type: original_explicit_tag_type,
                    };
                    let new_enum_id = self.types.add_type(Type::Enum(new_enum));
                    new_enum_id
                } else {
                    type_id
                }
            }
            Type::Reference(reference) => {
                let ref_inner = reference.inner_type;
                let new_inner = self.substitute_in_type(ref_inner, substitution_pairs, None, None);
                if force_new || new_inner != ref_inner {
                    let specialized_reference = ReferenceType { inner_type: new_inner };
                    self.types.add_type(Type::Reference(specialized_reference))
                } else {
                    type_id
                }
            }
            Type::TypeParameter(type_param) => {
                // nocommit: This seems like it wants to be a new type of type. We probably pull it
                // out like we did for inference hole
                debug!("substitute in type_param {:?}", type_param.function_type);
                match type_param.function_type {
                    None => type_id,
                    Some(function_type_id) => {
                        let new_fn_type = self.substitute_in_type(
                            function_type_id,
                            substitution_pairs,
                            None,
                            None,
                        );
                        debug!("substituted function {}", self.type_id_to_string(new_fn_type));
                        if new_fn_type != function_type_id {
                            let type_param = self.types.get(type_id).as_type_parameter().unwrap();
                            self.types.add_type(Type::TypeParameter(TypeParameter {
                                name: type_param.name,
                                scope_id: type_param.scope_id,
                                span: type_param.span,
                                function_type: Some(new_fn_type),
                            }))
                        } else {
                            type_id
                        }
                    }
                }
            }
            Type::InferenceHole(_) => type_id,
            Type::Unit(_)
            | Type::Char(_)
            | Type::Integer(_)
            | Type::Float(_)
            | Type::Bool(_)
            | Type::Pointer(_) => type_id,
            Type::EnumVariant(_) => {
                unreachable!("substitute_in_type is not expected to be called on an EnumVariant")
            }
            Type::Generic(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Generic")
            }
            Type::Function(fun_type) => {
                let mut new_fun_type = fun_type.clone();
                let mut any_new = false;
                let new_return_type =
                    self.substitute_in_type(fun_type.return_type, substitution_pairs, None, None);
                if new_return_type != new_fun_type.return_type {
                    any_new = true
                };
                new_fun_type.return_type = new_return_type;
                for param in new_fun_type.params.iter_mut() {
                    let new_param_type =
                        self.substitute_in_type(param.type_id, substitution_pairs, None, None);
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    param.type_id = new_param_type;
                }
                if force_new || any_new {
                    if defn_info_to_attach.as_ref().is_some() {
                        new_fun_type.defn_info = defn_info_to_attach;
                    }
                    let new_function_type_id = self.types.add_type(Type::Function(new_fun_type));
                    new_function_type_id
                } else {
                    type_id
                }
            }
            Type::Lambda(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Lambda")
            }
            Type::LambdaObject(lam_obj) => {
                let co_fn_type = lam_obj.function_type;
                let co_parsed_id = lam_obj.parsed_id;
                let new_fn_type =
                    self.substitute_in_type(lam_obj.function_type, substitution_pairs, None, None);
                if new_fn_type != co_fn_type || force_new {
                    self.types.add_lambda_object(&self.ast.identifiers, new_fn_type, co_parsed_id)
                } else {
                    type_id
                }
            }
            Type::Never(_) => {
                unreachable!("substitute_in_type is not expected to be called on never")
            }
            Type::RecursiveReference(_) => unreachable!(
                "substitute_in_type is not expected to be called on RecursiveReference"
            ),
        }
    }

    fn eval_const_type_expr(
        &mut self,
        parsed_type_expr: ParsedTypeExprId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        let ty = self.eval_type_expr(parsed_type_expr, scope_id)?;
        match self.types.get(ty) {
            Type::Unit(_) => Ok(ty),
            Type::Char(_) => Ok(ty),
            Type::Bool(_) => Ok(ty),
            Type::Pointer(_) => Ok(ty),
            Type::Integer(_) => Ok(ty),
            _t if ty == STRING_TYPE_ID => Ok(ty),
            _ => failf!(
                self.ast.get_type_expression_span(parsed_type_expr),
                "Only scalar types allowed in constants",
            ),
        }
    }

    fn compile_pattern(
        &self,
        pat_expr: ParsedPatternId,
        target_type_id: TypeId,
        scope_id: ScopeId,
        allow_bindings: bool,
    ) -> TyperResult<TypedPattern> {
        let parsed_pattern_expr = self.ast.patterns.get_pattern(pat_expr);
        match parsed_pattern_expr {
            ParsedPattern::Wildcard(span) => Ok(TypedPattern::Wildcard(*span)),
            ParsedPattern::Literal(literal_expr_id) => {
                match self.ast.exprs.get(*literal_expr_id).expect_literal() {
                    Literal::Unit(span) => match self.types.get(target_type_id) {
                        Type::Unit(_) => Ok(TypedPattern::LiteralUnit(*span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type unit will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    Literal::Char(c, span) => match self.types.get(target_type_id) {
                        Type::Char(_) => Ok(TypedPattern::LiteralChar(*c, *span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type char will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    Literal::Numeric(num_lit) => {
                        match self.eval_numeric_value(
                            &num_lit.text,
                            num_lit.span,
                            EvalExprContext::from_scope(scope_id)
                                .with_expected_type(Some(target_type_id)),
                        )? {
                            TypedExpr::Integer(value) => match self.types.get(target_type_id) {
                                Type::Integer(_integer_type) => {
                                    Ok(TypedPattern::LiteralInteger(value.value, num_lit.span))
                                }
                                _ => failf!(
                                    self.ast.get_pattern_span(pat_expr),
                                    "integer literal pattern will never match {}",
                                    self.type_id_to_string(target_type_id)
                                ),
                            },
                            TypedExpr::Float(value) => match self.types.get(target_type_id) {
                                Type::Float(_integer_type) => {
                                    Ok(TypedPattern::LiteralFloat(value.value, num_lit.span))
                                }
                                _ => failf!(
                                    self.ast.get_pattern_span(pat_expr),
                                    "float literal pattern will never match {}",
                                    self.type_id_to_string(target_type_id)
                                ),
                            },
                            _ => {
                                unreachable!("eval_numeric_value should produce only Integer and Float exprs")
                            }
                        }
                    }
                    Literal::Bool(b, span) => match self.types.get(target_type_id) {
                        Type::Bool(_) => Ok(TypedPattern::LiteralBool(*b, *span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "bool literal pattern will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    Literal::String(s, span) => match target_type_id {
                        STRING_TYPE_ID => Ok(TypedPattern::LiteralString(s.clone(), *span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "string literal pattern will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                }
            }
            ParsedPattern::Variable(ident_id, span) => {
                if !allow_bindings {
                    return failf!(*span, "Bindings are not allowed here");
                }
                Ok(TypedPattern::Variable(VariablePattern {
                    name: *ident_id,
                    type_id: target_type_id,
                    span: *span,
                }))
            }
            ParsedPattern::Enum(enum_pattern) => {
                let Some((enum_type, _variant)) = self.types.get_as_enum(target_type_id) else {
                    return failf!(
                        enum_pattern.span,
                        "Enum pattern will never match {}",
                        self.type_id_to_string(target_type_id)
                    );
                };
                let Some(matching_variant) =
                    enum_type.variants.iter().find(|v| v.name == enum_pattern.variant_tag)
                else {
                    return failf!(
                        enum_pattern.span,
                        "Impossible pattern: No variant named '{}'",
                        self.name_of(enum_pattern.variant_tag).blue()
                    );
                };

                let payload_pattern = match &enum_pattern.payload_pattern {
                    None => None,
                    Some(payload_expr) => {
                        let payload_type_id = matching_variant.payload.ok_or_else(|| {
                            make_error(
                                "Impossible pattern: Enum variant has no payload",
                                enum_pattern.span,
                            )
                        })?;
                        let payload_pattern = self.compile_pattern(
                            *payload_expr,
                            payload_type_id,
                            scope_id,
                            allow_bindings,
                        )?;
                        Some(Box::new(payload_pattern))
                    }
                };

                let enum_pattern = TypedEnumPattern {
                    enum_type_id: matching_variant.enum_type_id,
                    variant_index: matching_variant.index,
                    variant_tag_name: matching_variant.name,
                    payload: payload_pattern,
                    span: enum_pattern.span,
                };
                Ok(TypedPattern::Enum(enum_pattern))
            }
            ParsedPattern::Struct(struct_pattern) => {
                let target_type = self.types.get(target_type_id);
                if struct_pattern.fields.is_empty() {
                    return failf!(
                        struct_pattern.span,
                        "Useless pattern: Struct pattern has no fields; use wildcard pattern '_' instead",
                    );
                }
                let expected_struct = target_type.as_struct().ok_or_else(|| {
                    errf!(
                        struct_pattern.span,
                        "Impossible pattern: Match target '{}' is not a struct",
                        self.type_id_to_string(target_type_id)
                    )
                })?;
                let mut fields = Vec::with_capacity(struct_pattern.fields.len());
                for (field_name, field_parsed_pattern_id) in &struct_pattern.fields {
                    let expected_field =
                        expected_struct.fields.iter().find(|f| f.name == *field_name).ok_or_else(
                            || {
                                errf!(
                                    self.ast.get_pattern_span(*field_parsed_pattern_id),
                                    "Impossible pattern: Struct has no field named '{}'",
                                    self.name_of(*field_name).blue()
                                )
                            },
                        )?;
                    let field_type_id = expected_field.type_id;
                    let field_pattern = self.compile_pattern(
                        *field_parsed_pattern_id,
                        field_type_id,
                        scope_id,
                        allow_bindings,
                    )?;
                    fields.push(TypedStructPatternField {
                        name: *field_name,
                        pattern: field_pattern,
                        field_index: expected_field.index,
                        field_type_id: expected_field.type_id,
                    });
                }
                let struct_pattern = TypedStructPattern {
                    struct_type_id: target_type_id,
                    fields,
                    span: struct_pattern.span,
                };
                Ok(TypedPattern::Struct(struct_pattern))
            }
            ParsedPattern::Reference(reference_pattern) => {
                let Type::Reference(r) = self.types.get(target_type_id) else {
                    return failf!(
                        reference_pattern.span,
                        "Reference pattern will never match non-reference {}",
                        self.type_id_to_string(target_type_id)
                    );
                };
                let inner_pattern = self.compile_pattern(
                    reference_pattern.inner,
                    r.inner_type,
                    scope_id,
                    allow_bindings,
                )?;
                Ok(TypedPattern::Reference(TypedReferencePattern {
                    inner_pattern: Box::new(inner_pattern),
                    span: reference_pattern.span,
                }))
            }
        }
    }

    fn typecheck_struct(
        &self,
        expected: &StructType,
        actual: &StructType,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        if expected.fields.len() != actual.fields.len() {
            return Err(format!(
                "expected struct with {} fields, got {}",
                expected.fields.len(),
                actual.fields.len()
            ));
        }
        for (expected_field, actual_field) in expected.fields.iter().zip(actual.fields.iter()) {
            trace!("typechecking struct field {:?}", expected_field);
            if actual_field.name != expected_field.name {
                return Err(format!(
                    "expected field name {} but got {}",
                    self.name_of(expected_field.name),
                    self.name_of(actual_field.name)
                ));
            }
            self.check_types(expected_field.type_id, actual_field.type_id, scope_id).map_err(
                |msg| {
                    format!(
                        "Struct type mismatch on field '{}': {}",
                        self.name_of(actual_field.name),
                        msg
                    )
                },
            )?;
        }
        Ok(())
    }

    /// This implements 'duck-typing' for structs, which is really cool
    /// but I do not want to do this by default since the codegen involves
    /// either v-tables or monomorphization of functions that accept structs
    /// Maybe a <: syntax to opt-in to dynamic stuff like this, read as "conforms to"
    /// input <: {quack: () -> ()} means that it has at least a quack function
    /// fn takes_quacker = (input <: {quack: () -> ()}) -> ()
    ///
    /// "Conforms To" would mean that it has at least the same fields as the expected type, and
    /// it has them at least as strongly. If an optional is expected, actual can optional or required
    /// If a required is expected, actual must be required, etc. Basically TypeScripts structural typing
    #[allow(unused)]
    fn typecheck_struct_duck(
        &self,
        expected: &StructType,
        actual: &StructType,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        for expected_field in &expected.fields {
            trace!("typechecking struc field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name)
            else {
                return Err(format!("expected field {}", expected_field.name));
            };
            self.check_types(expected_field.type_id, matching_field.type_id, scope_id)?;
        }
        Ok(())
    }

    fn get_type_id_resolved(&self, type_id: TypeId, scope_id: ScopeId) -> TypeId {
        if let Type::TypeParameter(tvar) = self.types.get(type_id) {
            match self.scopes.find_type(scope_id, tvar.name) {
                None => {
                    debug!("Unresolved type variable. {}", self.name_of(tvar.name));
                    type_id
                }
                Some((resolved, _)) => {
                    if resolved == type_id {
                        type_id
                    } else {
                        self.get_type_id_resolved(resolved, scope_id)
                    }
                }
            }
        } else {
            type_id
        }
    }

    fn check_types(
        &self,
        expected: TypeId,
        actual: TypeId,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        debug!(
            "typecheck: {} <: {}",
            self.type_id_to_string(actual).blue(),
            self.type_id_to_string(expected).blue(),
        );
        if expected == actual {
            return Ok(());
        }

        if let (Some(spec1), Some(spec2)) = (
            self.types.get_generic_instance_info(expected),
            self.types.get_generic_instance_info(actual),
        ) {
            return if spec1.generic_parent == spec2.generic_parent {
                for (index, (exp_param, act_param)) in
                    spec1.param_values.iter().zip(spec2.param_values.iter()).enumerate()
                {
                    debug!(
                        "Comparing params {} and {} inside {}",
                        self.type_id_to_string(*exp_param),
                        self.type_id_to_string(*act_param),
                        self.name_of(
                            self.types
                                .get(spec1.generic_parent)
                                .expect_generic()
                                .type_defn_info
                                .name
                        )
                    );
                    if let Err(msg) = self.check_types(*exp_param, *act_param, scope_id) {
                        let generic = self.types.get(spec1.generic_parent).expect_generic();
                        let param = &generic.params[index];
                        let base_msg = format!(
                            "Expected {}, but got {}",
                            self.type_id_to_string(expected),
                            self.type_id_to_string(actual),
                        );
                        let detail =
                            format!("Param '{}' is incorrect: {}", self.name_of(param.name), msg);
                        return Err(format!("{base_msg}: {detail}"));
                    }
                }
                Ok(())
            } else {
                Err(format!(
                    "Expected {}, but got {}",
                    self.type_id_to_string(expected),
                    self.type_id_to_string(actual),
                ))
            };
        }

        let expected = self.get_type_id_resolved(expected, scope_id);
        let actual = self.get_type_id_resolved(actual, scope_id);

        if expected == actual {
            return Ok(());
        }

        match (self.types.get(expected), self.types.get(actual)) {
            //(Type::InferenceHole(_hole), any) => Ok(()),
            (Type::Struct(r1), Type::Struct(r2)) => self.typecheck_struct(r1, r2, scope_id),
            (Type::Reference(o1), Type::Reference(o2)) => {
                self.check_types(o1.inner_type, o2.inner_type, scope_id)
            }
            (Type::Enum(_exp_enum), Type::Enum(_act_enum)) => Err(format!(
                "expected enum {} but got enum {}",
                self.type_id_to_string(expected),
                self.type_id_to_string(actual)
            )),
            (Type::Enum(_expected_enum), Type::EnumVariant(actual_variant)) => {
                // This requires some more consideration, we need to actually insert a cast
                // instruction to make the physical types exactly the same?
                // We avoid this by representing the variant as the envelope type in codegen.
                // This is actually probably the move?
                if actual_variant.enum_type_id == expected {
                    Ok(())
                } else {
                    Err(format!(
                        "expected enum {} but got variant {} of a different enum",
                        self.type_id_to_string(expected),
                        self.name_of(actual_variant.name)
                    ))
                }
            }
            (Type::LambdaObject(_lambda_object), Type::Lambda(_lambda_type)) => Err(format!(
                "expected lambda object but got lambda; need to call toDyn() for now. {} vs {}",
                self.type_id_to_string(expected),
                self.type_id_to_string(actual),
            )),
            (Type::LambdaObject(exp_lambda_object), Type::LambdaObject(act_lambda_object)) => self
                .check_types(
                    exp_lambda_object.function_type,
                    act_lambda_object.function_type,
                    scope_id,
                ),
            (Type::TypeParameter(expected_abstract_function), act)
                if expected_abstract_function.is_function() =>
            {
                let expected_function_type = expected_abstract_function.function_type.unwrap();
                let actual_function_type = self.extract_function_type_from_functionlike(act);
                if let Some(actual_function_type) = actual_function_type {
                    self.check_types(expected_function_type, actual_function_type, scope_id)
                } else {
                    Err(format!(
                        "Expected some function-like with type: {} but got {}",
                        self.type_id_to_string(expected_function_type),
                        self.type_id_to_string(actual),
                    ))
                }
            }
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.params.len() != f2.params.len() {
                    return Err(format!(
                        "Wrong parameter count: expected {} but got {}",
                        f1.params.len(),
                        f2.params.len()
                    ));
                }
                if let Err(msg) = self.check_types(f1.return_type, f2.return_type, scope_id) {
                    Err(format!(
                        "Wrong return type: expected {} but got {}: {}",
                        self.type_id_to_string(expected),
                        self.type_id_to_string(actual),
                        msg
                    ))
                } else {
                    for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
                        if p1.is_context && p2.is_context {
                            continue;
                        }
                        if let Err(msg) = self.check_types(p1.type_id, p2.type_id, scope_id) {
                            return Err(format!(
                                "Incorrect type for parameter '{}': {}",
                                self.name_of(p1.name),
                                msg
                            ));
                        }
                    }
                    Ok(())
                }
            }
            (_expected, Type::Never(_)) => Ok(()),
            (_exp, _act) => Err(format!(
                "Expected {} but got {}",
                self.type_id_to_string_ext(expected, false),
                self.type_id_to_string_ext(actual, false),
            )),
        }
    }

    fn eval_const_expr(
        &mut self,
        expr: ParsedExpressionId,
        expected_type_id: Option<TypeId>,
        scope_id: ScopeId,
        constant_name: Option<Identifier>,
    ) -> TyperResult<CompileTimeValue> {
        let eval_ctx = EvalExprContext::from_scope(scope_id).with_expected_type(expected_type_id);
        match self.ast.exprs.get(expr) {
            ParsedExpression::Literal(Literal::Numeric(integer)) => {
                match self.eval_numeric_value(&integer.text, integer.span, eval_ctx)? {
                    TypedExpr::Float(f) => Ok(CompileTimeValue::Float(f.value)),
                    TypedExpr::Integer(i) => Ok(CompileTimeValue::Integer(i.value)),
                    _ => unreachable!(),
                }
            }
            ParsedExpression::Literal(Literal::Bool(b, _span)) => Ok(CompileTimeValue::Boolean(*b)),
            ParsedExpression::Literal(Literal::Char(c, _span)) => Ok(CompileTimeValue::Char(*c)),
            ParsedExpression::Builtin(span) => {
                if scope_id != self.scopes.get_root_scope_id() {
                    return failf!(
                        *span,
                        "All the known builtins constants live in the root scope"
                    );
                }
                match constant_name.map(|n| self.name_of(n)) {
                    None => {
                        failf!(*span, "builtin can only be used as a top-level expression")
                    }
                    Some("K1_TEST") => {
                        let is_test_build = self.ast.config.is_test_build;
                        Ok(CompileTimeValue::Boolean(is_test_build))
                    }
                    Some("K1_OS") => {
                        // TODO: Ideally this is an enum! But we don't support comptime enums yet
                        let os_str = self.ast.config.target.target_os().to_str();
                        Ok(CompileTimeValue::String(os_str.to_owned().into_boxed_str()))
                    }
                    Some("K1_NO_STD") => Ok(CompileTimeValue::Boolean(self.ast.config.no_std)),
                    Some(s) => failf!(*span, "Unknown builtin name: {s}"),
                }
            }
            ParsedExpression::Variable(variable) => {
                let Some((v, _)) = self.scopes.find_variable_namespaced(
                    scope_id,
                    &variable.name,
                    &self.namespaces,
                    &self.ast.identifiers,
                )?
                else {
                    return failf!(variable.name.span, "not found fixme");
                };
                let typed_variable = self.variables.get(v);
                let Some(constant_id) = typed_variable.constant_id else {
                    return failf!(variable.name.span, "var is not constant");
                };
                let constant = &self.constants[constant_id.0 as usize];
                Ok(constant.value.clone())
            }
            ParsedExpression::UnaryOp(op) => match op.op_kind {
                ParsedUnaryOpKind::BooleanNegation => {
                    let op_expr = op.expr;
                    let inner =
                        self.eval_const_expr(op_expr, Some(BOOL_TYPE_ID), eval_ctx.scope_id, None)?;
                    let CompileTimeValue::Boolean(b) = &inner else {
                        return failf!(
                            self.ast.exprs.get_span(op_expr),
                            "Expected constant boolean, got {:?}",
                            inner
                        );
                    };
                    Ok(CompileTimeValue::Boolean(!b))
                }
            },
            _other => {
                failf!(
                    self.ast.exprs.get_span(expr),
                    "Unsupported expression in 'constant' context",
                )
            }
        }
    }

    fn eval_constant(
        &mut self,
        parsed_constant_id: ParsedConstantId,
        scope_id: ScopeId,
    ) -> TyperResult<VariableId> {
        let parsed_constant = self.ast.get_constant(parsed_constant_id);
        let type_id = self.eval_const_type_expr(parsed_constant.ty, scope_id)?;
        let parsed_constant = self.ast.get_constant(parsed_constant_id);
        let constant_name = parsed_constant.name;
        let constant_span = parsed_constant.span;
        let parsed_expr_id = parsed_constant.value_expr;
        let expr =
            self.eval_const_expr(parsed_expr_id, Some(type_id), scope_id, Some(constant_name))?;
        let constant_id = ConstantId(self.constants.len() as u32);
        let variable_id = self.variables.add_variable(Variable {
            name: constant_name,
            type_id,
            is_mutable: false,
            owner_scope: scope_id,
            is_context: false,
            constant_id: Some(constant_id),
        });
        self.constants.push(Constant {
            variable_id,
            value: expr,
            parsed_expr: parsed_expr_id,
            ty: type_id,
            span: constant_span,
        });
        self.scopes.add_variable(scope_id, constant_name, variable_id);
        Ok(variable_id)
    }

    fn next_function_id(&self) -> FunctionId {
        let id = self.functions.len();
        FunctionId(id as u32)
    }

    fn add_function(&mut self, mut function: TypedFunction) -> FunctionId {
        let id = self.next_function_id();
        if let Some(specialization_info) = &mut function.specialization_info {
            specialization_info.specialized_function_id = id;
            self.get_function_mut(specialization_info.parent_function)
                .child_specializations
                .push(specialization_info.clone());
        }
        let is_concrete = self.is_function_concrete(&function);
        function.is_concrete = is_concrete;
        self.functions.push(function);
        id
    }

    pub fn get_function(&self, function_id: FunctionId) -> &TypedFunction {
        &self.functions[function_id.0 as usize]
    }

    pub fn get_function_mut(&mut self, function_id: FunctionId) -> &mut TypedFunction {
        &mut self.functions[function_id.0 as usize]
    }

    pub fn get_function_type(&self, function_id: FunctionId) -> &FunctionType {
        self.types.get(self.functions[function_id.0 as usize].type_id).as_function().unwrap()
    }

    pub fn add_ability_impl(&mut self, ability_impl: TypedAbilityImpl) -> AbilityImplId {
        let id = AbilityImplId(self.ability_impls.len() as u32);
        self.ability_impl_table
            .entry(ability_impl.self_type_id)
            .or_default()
            .push(AbilityImplHandle { ability_id: ability_impl.ability_id, full_impl_id: id });
        self.ability_impls.push(ability_impl);
        id
    }

    fn add_constrained_ability_impl(
        &mut self,
        type_variable_id: TypeId,
        implemented_ability: TypedAbilitySignature,
        scope_id: ScopeId,
        span: SpanId,
    ) {
        let ability = self.get_ability(implemented_ability.ability_id);
        let ability_args = ability.kind.arguments().to_vec();
        // Add Self
        let _ = self.scopes.add_type(scope_id, get_ident!(self, "Self"), type_variable_id);
        // Add ability params
        for ability_arg in ability_args.iter() {
            let _ = self.scopes.add_type(scope_id, ability_arg.name, ability_arg.type_id);
        }
        // Add impl params
        for impl_arg in implemented_ability.impl_arguments.clone().iter() {
            let _ = self.scopes.add_type(scope_id, impl_arg.name, impl_arg.type_id);
        }
        let functions = self.get_ability(implemented_ability.ability_id).functions.clone();
        let impl_kind = AbilityImplKind::VariableConstraint;
        let functions = functions
            .iter()
            .map(|f| {
                let generic_fn = self.get_function(f.function_id);
                let parsed_fn = generic_fn.parsed_id.as_function_id().unwrap();
                let specialized_function_id = self.eval_function_declaration(
                    parsed_fn,
                    scope_id,
                    Some(FunctionAbilityContextInfo::ability_impl(
                        implemented_ability.ability_id,
                        type_variable_id,
                        impl_kind,
                        None,
                    )),
                    ROOT_NAMESPACE_ID,
                );
                specialized_function_id
            })
            .collect::<TyperResult<Vec<_>>>();
        let functions = functions.unwrap_or_else(|err| {
            self.ice(
                "Failed while specializing an impl function for a type variable ability constraint",
                Some(&err),
            )
        });
        self.add_ability_impl(TypedAbilityImpl {
            kind: impl_kind,
            type_params: vec![],
            self_type_id: type_variable_id,
            ability_id: implemented_ability.ability_id,
            impl_arguments: implemented_ability.impl_arguments,
            functions,
            scope_id,
            span,
            compile_errors: vec![],
        });
    }

    fn add_type_variable(
        &mut self,
        value: TypeParameter,
        ability_impls: SmallVec<[TypedAbilitySignature; 4]>,
    ) -> TypeId {
        let span = value.span;
        let constrained_impl_scope =
            self.scopes.add_child_scope(value.scope_id, ScopeType::AbilityImpl, None, None);
        let type_id = self.types.add_type(Type::TypeParameter(value));
        for ability_sig in ability_impls.into_iter() {
            self.add_constrained_ability_impl(type_id, ability_sig, constrained_impl_scope, span);
        }
        type_id
    }

    // Hard to avoid returning a Vec here without returning an impl Iterator which I don't wanna
    // mess with
    pub fn get_constrained_ability_impls_for_type(
        &self,
        type_id: TypeId,
    ) -> Vec<AbilityImplHandle> {
        match self.ability_impl_table.get(&type_id) {
            None => vec![],
            Some(v) => v
                .iter()
                .filter(|handle| {
                    self.get_ability_impl(handle.full_impl_id).kind.is_variable_constraint()
                })
                .copied()
                .collect(),
        }
    }

    pub fn get_ability_base(&self, ability_id: AbilityId) -> AbilityId {
        self.get_ability(ability_id).parent_ability_id().unwrap_or(ability_id)
    }

    pub fn find_ability_impl_for_type(
        &mut self,
        self_type_id: TypeId,
        target_ability_id: AbilityId,
        span: SpanId,
    ) -> Option<AbilityImplHandle> {
        let maybe_concrete_impl = self
            .ability_impl_table
            .get(&self_type_id)
            .and_then(|impl_handles| {
                debug!(
                    "Ability dump for {} in search of {} {:02}\n{}",
                    self.type_id_to_string(self_type_id),
                    self.name_of(self.get_ability(target_ability_id).name),
                    target_ability_id.0,
                    impl_handles
                        .iter()
                        .map(|h| format!(
                            "IMPL {} {:02} {}",
                            self.name_of(self.get_ability(h.ability_id).name),
                            h.ability_id.0,
                            self.pretty_print_named_types(
                                &self.get_ability_impl(h.full_impl_id).impl_arguments,
                                ", "
                            )
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                impl_handles.iter().find(|handle| handle.ability_id == target_ability_id)
            })
            .copied();
        if let Some(concrete_impl) = maybe_concrete_impl {
            return Some(concrete_impl);
        };

        debug!("Blanket search for {}", self.name_of(self.get_ability(target_ability_id).name));
        let target_base_ability_id = self.get_ability_base(target_ability_id);
        if let Some(blanket_impls_for_base) = self.blanket_impls.get(&target_base_ability_id) {
            for blanket_impl_id in blanket_impls_for_base.clone() {
                match self.try_apply_blanket_implementation(
                    blanket_impl_id,
                    self_type_id,
                    target_ability_id,
                    span,
                ) {
                    None => debug!("Blanket impl didn't work"),
                    Some(impl_handle) => return Some(impl_handle),
                }
            }
        }

        None
    }

    pub fn try_apply_blanket_implementation(
        &mut self,
        blanket_impl_id: AbilityImplId,
        self_type_id: TypeId,
        target_ability_id: AbilityId,
        span: SpanId,
    ) -> Option<AbilityImplHandle> {
        let old_inference_context = std::mem::take(&mut self.inference_context);
        let mut self_ = scopeguard::guard(self, |s| s.inference_context = old_inference_context);
        let blanket_impl = self_.get_ability_impl(blanket_impl_id);
        let blanket_impl_ability_id = blanket_impl.ability_id;
        let blanket_impl_scope_id = blanket_impl.scope_id;
        let blanket_impl_self_type_id = blanket_impl.self_type_id;
        if !blanket_impl.compile_errors.is_empty() {
            debug!("Blanket impl failed compile; skipping");
            return None;
        }
        let target_ability = self_.get_ability(target_ability_id);
        let target_ability_args = target_ability.kind.arguments();
        let AbilityImplKind::Blanket { parsed_id, .. } = blanket_impl.kind else {
            unreachable!("Expected a blanket impl")
        };
        let target_base = target_ability.parent_ability_id().unwrap_or(target_ability_id);

        let blanket_ability = self_.get_ability(blanket_impl.ability_id);
        let blanket_base = blanket_ability.parent_ability_id().unwrap_or(blanket_impl.ability_id);

        if blanket_base != target_base {
            debug!("Wrong blanket base {}", self_.name_of(blanket_ability.name));
            return None;
        }

        let blanket_arguments = blanket_ability.kind.arguments();

        debug!(
            "Trying blanket impl {} with blanket arguments {}, impl arguments {}",
            self_.name_of(blanket_ability.name),
            self_.pretty_print_named_types(blanket_arguments, ", "),
            self_.pretty_print_named_types(&blanket_impl.impl_arguments, ", "),
        );

        if blanket_arguments.len() != target_ability_args.len() {
            debug!(
                "Wrong arg count {} vs {}",
                self_.pretty_print_named_types(blanket_arguments, ", "),
                self_.pretty_print_named_types(target_ability_args, ", ")
            );
            return None;
        }

        // Reborrows
        let blanket_ability = self_.get_ability(blanket_impl_ability_id);
        let blanket_arguments = blanket_ability.kind.arguments();

        //let mut solution_set = TypeSolutionSet::from(blanket_impl.type_params.iter());
        let mut args_and_params = Vec::with_capacity(blanket_arguments.len() + 1);
        //
        // For each argument A to the blanket impl, solve for [Self, ...Params] using
        args_and_params.push((
            TypeOrParsedExpr::TypeId(self_type_id),
            blanket_impl_self_type_id,
            true,
        ));
        for (arg_to_blanket, arg_to_target) in blanket_arguments.iter().zip(target_ability_args) {
            args_and_params.push((
                TypeOrParsedExpr::TypeId(arg_to_target.type_id),
                arg_to_blanket.type_id,
                true,
            ));
            // debug!(
            //     "Solving with slot: {} | passed: {}",
            //     self.type_id_to_string(arg_to_target.type_id),
            //     self.type_id_to_string(arg_to_blanket.type_id),
            // );
            // if let Err(solve_error) = self.solve_generic_params(
            //     &mut solution_set,
            //     arg_to_target.type_id,
            //     arg_to_blanket.type_id,
            //     span,
            // ) {
            //     debug!("Bailing due to error; {solve_error}");
            //     return None;
            // }
        }
        //if let Err(solve_error) = self.solve_generic_params(
        //    &mut solution_set,
        //    self_type_id,
        //    blanket_impl.self_type_id,
        //    span,
        //) {
        //    debug!("Bailing due to error; {solve_error}");
        //    return None;
        //};
        let blanket_impl_type_params = self_.get_ability_impl(blanket_impl_id).type_params.clone();
        let root_scope_id = self_.scopes.get_root_scope_id();
        let solutions =
            self_.infer_types(&blanket_impl_type_params, &args_and_params, span, root_scope_id);
        let solutions = match solutions {
            Err(e) => {
                debug!("Could not solve all blanket impl params: {e}");
                return None;
            }
            Ok(solutions) => solutions,
        };

        // 'Specialize' the constraints:
        // - For each constraint, run the expression with the binding for T from a child
        //   scope of the blanket impl scope
        // - Then check if the solution implements _that_ ability, by factoring
        //   out the actual inner check from check_type_constraints
        let constraint_checking_scope = self_.scopes.add_sibling_scope(
            blanket_impl_scope_id,
            ScopeType::AbilityImpl,
            None,
            None,
        );
        let parsed_blanket_impl = self_.ast.get_ability_impl(parsed_id);
        for (parsed_param, solution) in
            parsed_blanket_impl.generic_impl_params.clone().iter().zip(solutions.iter())
        {
            let _ = self_.scopes.add_type(
                constraint_checking_scope,
                parsed_param.name,
                solution.type_id,
            );
            for constraint in parsed_param.constraints.iter() {
                let constraint_signature = match constraint {
                    parse::ParsedTypeConstraintExpr::Ability(parsed_ability_expr) => self_
                        .eval_ability_expr(parsed_ability_expr, false, constraint_checking_scope)
                        .unwrap(),
                };
                if let Err(mut e) = self_.check_type_constraint(
                    solution.type_id,
                    constraint_signature,
                    parsed_param.name,
                    constraint_checking_scope,
                    span,
                ) {
                    e.message = format!(
                        "Blanket impl almost matched but a constraint was unsatisfied; {}",
                        e.message
                    );
                    e.level = ErrorLevel::Info;
                    self_.write_error(&mut std::io::stderr(), &e).unwrap();
                    return None;
                }
            }
        }

        // 'Run' the blanket ability using 'solutions'
        let impl_handle = self_
            .instantiate_blanket_impl(self_type_id, blanket_impl_id, &solutions)
            .unwrap_or_else(|e| self_.ice("Failed to instantiate blanket impl", Some(&e)));
        Some(impl_handle)
    }

    fn instantiate_blanket_impl(
        &mut self,
        self_type_id: TypeId,
        blanket_impl_id: AbilityImplId,
        solutions: &[SimpleNamedType],
    ) -> TyperResult<AbilityImplHandle> {
        let blanket_impl = self.get_ability_impl(blanket_impl_id).clone();
        let blanket_ability_args =
            self.get_ability(blanket_impl.ability_id).kind.arguments().to_vec();

        let generic_parent = blanket_impl.kind.blanket_parent().unwrap();

        let new_impl_scope = self.scopes.add_sibling_scope(
            blanket_impl.scope_id,
            ScopeType::AbilityImpl,
            None,
            None,
        );

        let pairs: Vec<TypeSubstitutionPair> = blanket_impl
            .type_params
            .iter()
            .zip(solutions.iter())
            .map(|(param, solution)| {
                let _ = self.scopes.add_type(new_impl_scope, param.name, solution.type_id);
                TypeSubstitutionPair { from: param.type_id, to: solution.type_id }
            })
            .collect();

        let mut substituted_ability_args = Vec::with_capacity(blanket_ability_args.len());
        for blanket_arg in blanket_ability_args {
            // Substitute T, U, V, in for each
            let substituted_type = self.substitute_in_type(blanket_arg.type_id, &pairs, None, None);
            let nt = SimpleNamedType { name: blanket_arg.name, type_id: substituted_type };
            substituted_ability_args.push(nt);
            if !self.scopes.add_type(new_impl_scope, blanket_arg.name, substituted_type) {
                panic!("uh oh")
            };
        }
        let concrete_ability_id =
            self.specialize_ability(generic_parent, substituted_ability_args, blanket_impl.span)?;

        let mut substituted_impl_arguments =
            SmallVec::with_capacity(blanket_impl.impl_arguments.len());
        for blanket_impl_arg in &blanket_impl.impl_arguments {
            // Substitute T, U, V, in for each
            let substituted_type =
                self.substitute_in_type(blanket_impl_arg.type_id, &pairs, None, None);
            let nt = SimpleNamedType { name: blanket_impl_arg.name, type_id: substituted_type };
            substituted_impl_arguments.push(nt);
            if !self.scopes.add_type(new_impl_scope, blanket_impl_arg.name, substituted_type) {
                panic!("uh oh")
            };
        }

        let _ = self.scopes.add_type(new_impl_scope, get_ident!(self, "Self"), self_type_id);

        let mut specialized_function_ids = Vec::new();
        let kind = AbilityImplKind::DerivedFromBlanket { blanket_impl_id };
        debug!(
            "blanket impl instance scope before function specialization: {}",
            self.scope_id_to_string(new_impl_scope)
        );
        for blanket_fn_id in &blanket_impl.functions {
            let blanket_fn = self.get_function(*blanket_fn_id);
            let parsed_fn = blanket_fn.parsed_id.as_function_id().unwrap();
            let specialized_function_id = self.eval_function_declaration(
                parsed_fn,
                new_impl_scope,
                Some(FunctionAbilityContextInfo::ability_impl(
                    concrete_ability_id,
                    self_type_id,
                    kind,
                    Some(*blanket_fn_id),
                )),
                ROOT_NAMESPACE_ID,
            )?;
            // HEADS UP --------> Recently swapped these; this fixes an issue
            // where blanket bodies were running too early but may have broken things
            self.functions_pending_body_specialization.push(specialized_function_id);
            // self.eval_function_body(specialized_function_id)?;
            specialized_function_ids.push(specialized_function_id);
        }

        let id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            type_params: vec![],
            self_type_id,
            ability_id: concrete_ability_id,
            impl_arguments: substituted_impl_arguments,
            functions: specialized_function_ids,
            scope_id: new_impl_scope,
            span: blanket_impl.span,
            compile_errors: vec![],
        });
        Ok(AbilityImplHandle { ability_id: concrete_ability_id, full_impl_id: id })
    }

    pub fn get_ability_impl(&self, ability_impl_id: AbilityImplId) -> &TypedAbilityImpl {
        &self.ability_impls[ability_impl_id.0 as usize]
    }

    pub fn get_ability_impl_mut(
        &mut self,
        ability_impl_id: AbilityImplId,
    ) -> &mut TypedAbilityImpl {
        &mut self.ability_impls[ability_impl_id.0 as usize]
    }

    fn eval_numeric_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExpr> {
        if parsed_text.contains('.') {
            Ok(TypedExpr::Float(TypedFloatExpr {
                value: self.eval_float_value(parsed_text, span, ctx)?,
                span,
            }))
        } else {
            Ok(TypedExpr::Integer(TypedIntegerExpr {
                value: self.eval_integer_value(parsed_text, span, ctx)?,
                span,
            }))
        }
    }

    fn eval_float_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedFloatValue> {
        let expected_width = match ctx.expected_type_id {
            None => NumericWidth::B32,
            Some(F64_TYPE_ID) => NumericWidth::B64,
            Some(F32_TYPE_ID) => NumericWidth::B32,
            Some(_) => {
                // Parse as f32 and let typechecking fail
                NumericWidth::B32
            }
        };
        let value: Result<TypedFloatValue, std::num::ParseFloatError> = match expected_width {
            NumericWidth::B32 => parsed_text.parse::<f32>().map(TypedFloatValue::F32),
            NumericWidth::B64 => parsed_text.parse::<f64>().map(TypedFloatValue::F64),
            _ => unreachable!("unreachable float width"),
        };
        let value =
            value.map_err(|e| errf!(span, "Invalid f{}: {e}", expected_width.bit_width()))?;
        Ok(value)
    }

    fn eval_integer_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedIntegerValue> {
        //eprintln!(
        //    "eval_integer_value hint {}",
        //    self.type_id_option_to_string(ctx.expected_type_id)
        //);
        let expected_int_type = match ctx.expected_type_id {
            None => IntegerType::I64,
            Some(U8_TYPE_ID) => IntegerType::U8,
            Some(U16_TYPE_ID) => IntegerType::U16,
            Some(U32_TYPE_ID) => IntegerType::U32,
            Some(U64_TYPE_ID) => IntegerType::U64,
            Some(I8_TYPE_ID) => IntegerType::I8,
            Some(I16_TYPE_ID) => IntegerType::I16,
            Some(I32_TYPE_ID) => IntegerType::I32,
            Some(I64_TYPE_ID) => IntegerType::I64,
            Some(_other) => {
                // Parse as i64 and let typechecking fail
                IntegerType::I64
            }
        };
        macro_rules! parse_int {
            ($int_type:ident, $rust_int_type:ty, $base: expr, $offset: expr) => {{
                let result = <$rust_int_type>::from_str_radix(&parsed_text[$offset..], $base);
                result.map(|int| TypedIntegerValue::$int_type(int))
            }};
        }
        if parsed_text.starts_with("0x") {
            let hex_base = 16;
            let offset = 2;
            let value: Result<TypedIntegerValue, std::num::ParseIntError> = match expected_int_type
            {
                IntegerType::U8 => parse_int!(U8, u8, hex_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, hex_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, hex_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, hex_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, hex_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, hex_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, hex_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, hex_base, offset),
            };
            let value = value
                .map_err(|e| make_error(format!("Invalid hex {expected_int_type}: {e}"), span))?;
            Ok(value)
        } else if parsed_text.starts_with("0b") {
            let bin_base = 2;
            let offset = 2;
            let value: Result<TypedIntegerValue, std::num::ParseIntError> = match expected_int_type
            {
                IntegerType::U8 => parse_int!(U8, u8, bin_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, bin_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, bin_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, bin_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, bin_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, bin_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, bin_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, bin_base, offset),
            };
            let value = value.map_err(|e| {
                make_error(format!("Invalid binary {expected_int_type}: {e}"), span)
            })?;
            Ok(value)
        } else {
            let dec_base = 10;
            let offset = 0;
            let value: Result<TypedIntegerValue, std::num::ParseIntError> = match expected_int_type
            {
                IntegerType::U8 => parse_int!(U8, u8, dec_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, dec_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, dec_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, dec_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, dec_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, dec_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, dec_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, dec_base, offset),
            };
            let value = value.map_err(|e| {
                errf!(
                    span,
                    "Invalid {} integer {expected_int_type}: {e}",
                    if expected_int_type.is_signed() { "signed" } else { "unsigned" }
                )
            })?;
            Ok(value)
        }
    }

    fn eval_variable(
        &mut self,
        variable_expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpression::Variable(variable) = self.ast.exprs.get(variable_expr_id) else {
            panic!()
        };
        let variable_name_span = variable.name.span;
        let variable_id = self.scopes.find_variable_namespaced(
            scope_id,
            &variable.name,
            &self.namespaces,
            &self.ast.identifiers,
        )?;
        match variable_id {
            None => {
                // function lookup
                let function_id = self.scopes.find_function_namespaced(
                    scope_id,
                    &variable.name,
                    &self.namespaces,
                    &self.ast.identifiers,
                )?;
                match function_id {
                    Some(function_id) => {
                        let id = self.exprs.add(TypedExpr::FunctionName(FunctionReferenceExpr {
                            function_id,
                            span: variable.name.span,
                            type_id: self
                                .types
                                .add_reference_type(self.get_function(function_id).type_id),
                        }));
                        Ok(id)
                    }
                    None => {
                        failf!(
                            variable.name.span,
                            "Variable '{}' is not defined",
                            self.ast.identifiers.get_name(variable.name.name),
                        )
                    }
                }
            }
            Some((variable_id, variable_scope_id)) => {
                let parent_lambda_scope_id = self.scopes.nearest_parent_lambda(scope_id);
                let is_capture = if let Some(nearest_parent_lambda_scope) = parent_lambda_scope_id {
                    let variable_is_above_lambda = self
                        .scopes
                        .scope_has_ancestor(nearest_parent_lambda_scope, variable_scope_id);
                    let variable_is_global = self.variables.get(variable_id).constant_id.is_some();

                    let is_capture = variable_is_above_lambda && !variable_is_global;
                    debug!("{}, is_capture={is_capture}", self.name_of(variable.name.name));
                    is_capture
                } else {
                    false
                };

                let v = self.variables.get(variable_id);
                if is_assignment_lhs && !v.is_mutable {
                    return failf!(
                        variable_name_span,
                        "Cannot assign to immutable variable {}",
                        self.ast.identifiers.get_name(v.name)
                    );
                }
                if is_capture {
                    if !variable.name.namespaces.is_empty() {
                        return failf!(
                            variable_name_span,
                            "Should not capture namespaced things, I think?"
                        );
                    }
                    let fixup_expr_id =
                        self.exprs.add(TypedExpr::PendingCapture(PendingCaptureExpr {
                            captured_variable_id: variable_id,
                            type_id: v.type_id,
                            resolved_expr: None,
                            span: variable_name_span,
                        }));
                    self.scopes.add_capture(
                        parent_lambda_scope_id.unwrap(),
                        variable_id,
                        fixup_expr_id,
                    );
                    Ok(fixup_expr_id)
                } else {
                    let expr = self.exprs.add(TypedExpr::Variable(VariableExpr {
                        type_id: v.type_id,
                        variable_id,
                        span: variable_name_span,
                    }));
                    Ok(expr)
                }
            }
        }
    }

    pub fn get_expr_type_id(&self, expr_id: TypedExprId) -> TypeId {
        self.exprs.get(expr_id).get_type()
    }

    pub fn get_expr_type(&self, expr_id: TypedExprId) -> &Type {
        self.types.get(self.exprs.get(expr_id).get_type())
    }

    fn eval_field_access(
        &mut self,
        field_access: &parse::FieldAccess,
        ctx: EvalExprContext,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExprId> {
        // Bailout case: Enum Constructor
        let span = field_access.span;
        let base_span = self.ast.exprs.get_span(field_access.base);
        if let Some(enum_result) = self.handle_enum_constructor(
            field_access.base,
            field_access.field_name,
            None,
            &field_access.type_args,
            ctx,
            span,
        )? {
            return Ok(enum_result);
        }

        // Bailout case: .* dereference operation
        if field_access.field_name == get_ident!(self, "*") {
            return self.eval_dereference(field_access.base, ctx, span);
        }

        // Bailout case: .! unwrap operation
        if field_access.field_name == get_ident!(self, "!") {
            if field_access.is_coalescing {
                return failf!(field_access.span, "Cannot use ?. with unwrap operator");
            }
            if field_access.is_referencing {
                return failf!(field_access.span, "Cannot use * with unwrap operator");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to unwrap operator");
            }
            return self.eval_unwrap_operator(field_access.base, ctx, field_access.span);
        }

        if field_access.field_name == get_ident!(self, "try") {
            if field_access.is_coalescing {
                return failf!(field_access.span, "Cannot use ?. with try operator");
            }
            if field_access.is_referencing {
                return failf!(field_access.span, "Cannot use * with try operator");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to try operator");
            }
            return self.eval_try_operator(field_access.base, ctx, field_access.span);
        }

        let mut base_expr = self.eval_expr(field_access.base, ctx.with_no_expected_type())?;
        let original_base_expr_type = self.exprs.get(base_expr).get_type();

        // Perform auto-dereference for accesses that are not 'lvalue'-style or 'referencing' style
        let (base_type, is_reference) = match self.get_expr_type(base_expr) {
            Type::Reference(reference_type) => {
                let inner_type = reference_type.inner_type;
                if !is_assignment_lhs && !field_access.is_referencing {
                    // Dereference the base expression
                    base_expr = self.exprs.add(TypedExpr::UnaryOp(UnaryOp {
                        kind: UnaryOpKind::Dereference,
                        type_id: reference_type.inner_type,
                        span: field_access.span,
                        expr: base_expr,
                    }));
                }
                (inner_type, true)
            }
            _other => {
                if is_assignment_lhs {
                    return failf!(base_span, "Cannot assign to member of non-reference struct");
                } else if field_access.is_referencing && !field_access.is_coalescing {
                    return failf!(base_span, "Field access target is not a pointer, so referencing access with * cannot be used");
                } else {
                    (original_base_expr_type, false)
                }
            }
        };
        match self.types.get(base_type) {
            t @ Type::Enum(_opt_type) => {
                if let Some(opt) = t.as_optional() {
                    if !field_access.is_coalescing {
                        return failf!(span, "Optionals have no direct fields; did you mean to use the '?.' operator?");
                    }
                    if field_access.is_referencing {
                        return failf!(
                            span,
                            "Cannot use referencing access with optional chaining"
                        );
                    }
                    // TODO: This can be re-written in terms of 'Unwrap' if we supply a
                    //           'wrap' function that wraps (optional Some equivalent)
                    // It doesn't really support chaining yet, kinda useless so I also won't make it ability-based yet
                    // See coalescing_v2.wip for plan
                    let mut block = self.synth_block(ctx.scope_id, span);
                    let block_scope = block.scope_id;
                    let base_expr_var = self.synth_variable_defn_simple(
                        field_access.field_name,
                        base_expr,
                        block_scope,
                    );
                    let has_value = self.synth_typed_function_call(
                        self.ident_opt_has_value(span),
                        &[opt.inner_type],
                        &[base_expr_var.variable_expr],
                        ctx.with_scope(block_scope).with_no_expected_type(),
                    )?;

                    let st @ Type::Struct(struct_type) = self.types.get(opt.inner_type) else {
                        return failf!(
                            span,
                            "?. must be used on optional structs, got {}",
                            self.type_id_to_string(original_base_expr_type)
                        );
                    };
                    let (field_index, target_field) =
                        struct_type.find_field(field_access.field_name).ok_or_else(|| {
                            errf!(
                                span,
                                "Field {} not found on struct {}",
                                self.ast.identifiers.get_name(field_access.field_name),
                                self.type_to_string(st, false)
                            )
                        })?;
                    let field_type = target_field.type_id;
                    let field_name = target_field.name;
                    let opt_unwrap = self.synth_typed_function_call(
                        self.ident_opt_get(span),
                        &[opt.inner_type],
                        &[base_expr_var.variable_expr],
                        ctx.with_scope(block_scope).with_no_expected_type(),
                    )?;
                    let (consequent, consequent_type_id) =
                        self.synth_optional_some(TypedExpr::StructFieldAccess(FieldAccess {
                            base: opt_unwrap,
                            target_field: field_name,
                            target_field_index: field_index as u32,
                            span,
                            is_referencing: false,
                            result_type: field_type,
                            struct_type: opt.inner_type,
                        }));
                    let alternate = self.synth_optional_none(field_type, span);
                    let if_expr = TypedExpr::If(TypedIf {
                        condition: has_value,
                        ty: consequent_type_id,
                        consequent,
                        alternate,
                        span,
                    });
                    self.push_block_stmt_id(&mut block, base_expr_var.defn_stmt);
                    self.add_expr_to_block(&mut block, if_expr);
                    Ok(self.exprs.add(TypedExpr::Block(block)))
                } else {
                    failf!(
                        span,
                        "Field {} does not exist",
                        self.ast.identifiers.get_name(field_access.field_name)
                    )
                }
            }
            st @ Type::Struct(struct_type) => {
                if is_assignment_lhs && !is_reference {
                    return failf!(span, "Struct must be a reference to be assignable");
                }
                let (field_index, target_field) =
                    struct_type.find_field(field_access.field_name).ok_or_else(|| {
                        errf!(
                            span,
                            "Field {} not found on struct {}",
                            self.ast.identifiers.get_name(field_access.field_name),
                            self.type_to_string(st, false)
                        )
                    })?;
                if target_field.private {
                    let companion_namespace =
                        struct_type.type_defn_info.as_ref().and_then(|d| d.companion_namespace);

                    if !self.is_inside_companion_scope(companion_namespace, ctx.scope_id) {
                        return failf!(
                            span,
                            "Field {} is inaccessible from here",
                            self.name_of(target_field.name)
                        );
                    }
                }
                let result_type = if field_access.is_referencing {
                    self.types.add_reference_type(target_field.type_id)
                } else {
                    target_field.type_id
                };
                Ok(self.exprs.add(TypedExpr::StructFieldAccess(FieldAccess {
                    base: base_expr,
                    target_field: field_access.field_name,
                    target_field_index: field_index as u32,
                    result_type,
                    is_referencing: field_access.is_referencing,
                    struct_type: base_type,
                    span,
                })))
            }
            Type::EnumVariant(ev) => {
                if self.ast.identifiers.get_name(field_access.field_name) != "value" {
                    return failf!(
                        span,
                        "Field {} does not exist; try .value",
                        self.ast.identifiers.get_name(field_access.field_name)
                    );
                }
                let Some(payload_type_id) = ev.payload else {
                    return failf!(
                        span,
                        "Variant {} has no payload value",
                        self.ast.identifiers.get_name(ev.name)
                    );
                };
                if is_assignment_lhs && !is_reference {
                    return failf!(span, "Enum must be a reference to be assignable");
                }
                let variant_name = ev.name;
                let variant_index = ev.index;
                let result_type_id = if field_access.is_referencing {
                    self.types.add_reference_type(payload_type_id)
                } else {
                    payload_type_id
                };
                Ok(self.exprs.add(TypedExpr::EnumGetPayload(GetEnumPayload {
                    target_expr: base_expr,
                    result_type_id,
                    variant_name,
                    variant_index,
                    is_referencing: field_access.is_referencing,
                    span,
                })))
            }
            _ => failf!(
                span,
                "Field {} does not exist on type {}",
                self.ast.identifiers.get_name(field_access.field_name),
                self.type_id_to_string(original_base_expr_type)
            ),
        }
    }

    fn eval_try_operator(
        &mut self,
        operand: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let scope_id = ctx.scope_id;
        let block_return_type = self.get_expected_return_type(scope_id, span)?;
        let block_try_impl = self.expect_ability_implementation(
                    block_return_type,
                    TRY_ABILITY_ID,
                    span,
                ).map_err(|mut e| {
                        e.message = format!("`.try` can only be used from a function or lambda that returns a type implementing `Try`. {}", e.message);
                        e
                    })?;
        let try_value_original_expr = self.eval_expr(operand, ctx.with_no_expected_type())?;
        let try_value_type = self.get_expr_type_id(try_value_original_expr);
        let value_try_impl =
            self.expect_ability_implementation(try_value_type, TRY_ABILITY_ID, span)?;
        let block_impl_args = &self.get_ability_impl(block_try_impl.full_impl_id).impl_arguments;
        let value_impl_args = &self.get_ability_impl(value_try_impl.full_impl_id).impl_arguments;
        let block_error_type = block_impl_args
            .iter()
            .find(|nt| nt.name == get_ident!(self, "E"))
            .map(|nt| nt.type_id)
            .unwrap();
        let error_type = value_impl_args
            .iter()
            .find(|nt| nt.name == get_ident!(self, "E"))
            .map(|nt| nt.type_id)
            .unwrap();
        if let Err(msg) = self.check_types(block_error_type, error_type, scope_id) {
            return failf!(span, "This function expects a Try, but with a different Error type than the value: {msg}");
        };
        let value_success_type = value_impl_args
            .iter()
            .find(|nt| nt.name == get_ident!(self, "T"))
            .map(|nt| nt.type_id)
            .unwrap();
        let mut result_block = self.synth_block(scope_id, span);
        let try_value_var = self.synth_variable_defn_simple(
            get_ident!(self, "try_value"),
            try_value_original_expr,
            result_block.scope_id,
        );
        let result_block_ctx = ctx.with_scope(result_block.scope_id).with_no_expected_type();
        let is_ok_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "isOk"),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
        )?;
        let get_ok_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "getOk"),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
        )?;
        // FIXME: Consider alternatives for calling the block's makeError function
        //        in a less brittle way?
        let block_make_error_fn = self.get_ability_impl(block_try_impl.full_impl_id).functions[0];
        debug!(
            "type of make_error: {}",
            self.type_id_to_string(self.get_function(block_make_error_fn).type_id)
        );

        let get_error_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "getError"),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
        )?;
        let make_error_call = self.exprs.add(TypedExpr::Call(Call {
            callee: Callee::StaticFunction(block_make_error_fn),
            args: vec![get_error_call],
            type_args: smallvec![],
            return_type: block_error_type,
            span,
        }));
        let return_error_expr =
            self.exprs.add(TypedExpr::Return(TypedReturn { value: make_error_call, span }));
        let if_expr = self.exprs.add(TypedExpr::If(TypedIf {
            condition: is_ok_call,
            consequent: get_ok_call,
            alternate: return_error_expr,
            ty: value_success_type,
            span,
        }));

        self.push_block_stmt_id(&mut result_block, try_value_var.defn_stmt);
        self.add_expr_id_to_block(&mut result_block, if_expr);

        Ok(self.exprs.add(TypedExpr::Block(result_block)))
    }

    fn eval_unwrap_operator(
        &mut self,
        operand: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let operand_expr = self.eval_expr_inner(operand, ctx.with_no_expected_type())?;
        let operand_type = self.exprs.get(operand_expr).get_type();
        let _unwrap_impl =
            self.expect_ability_implementation(operand_type, UNWRAP_ABILITY_ID, span)?;
        self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "unwrap"),
            &[],
            &[operand_expr],
            ctx,
        )
    }

    fn eval_dereference(
        &mut self,
        operand: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        // Example:
        // let x: int = intptr.*
        // The expected_type when we get `*intptr` is int, so
        // the expected_type when we get `intptr` should be *int
        let inner_expected_type = match ctx.expected_type_id {
            Some(expected) => Some(self.types.add_reference_type(expected)),
            None => None,
        };
        let base_expr = self.eval_expr(operand, ctx.with_expected_type(inner_expected_type))?;
        let base_expr_type = self.exprs.get(base_expr).get_type();
        let reference_type = self.types.get(base_expr_type).as_reference().ok_or_else(|| {
            errf!(
                span,
                "Cannot dereference non-reference type: {}",
                self.type_id_to_string(base_expr_type)
            )
        })?;
        Ok(self.exprs.add(TypedExpr::UnaryOp(UnaryOp {
            kind: UnaryOpKind::Dereference,
            type_id: reference_type.inner_type,
            expr: base_expr,
            span,
        })))
    }

    // Used for
    // - de-referencing,
    // Probably unsound. I'd like to remove and re-introduce coercion as part of subtyping.
    // However, not sure if we should do it for references, because I'd like to move that to an
    // ability so that we can have different types of references written in userspace
    fn coerce_expression_to_expected_type(
        &mut self,
        expected_type_id: TypeId,
        expression: TypedExprId,
        scope_id: ScopeId,
        _parsed_id: ParsedId,
    ) -> CoerceResult {
        if self
            .types
            .get(self.get_type_id_resolved(expected_type_id, scope_id))
            .as_reference()
            .is_none()
        {
            // We only do this if the expected type is not a reference at all. Meaning,
            // if your expected type is T*, and you pass a T**, you need to de-reference that yourself.
            // This rule won't help you or do anything for nested references

            // Note: We could also introduce a 'check_kinds' which only cares about the _shape_ of the type!
            if let Some(reference) = self.get_expr_type(expression).as_reference() {
                let span = self.exprs.get(expression).get_span();
                return CoerceResult::Coerced(
                    "deref",
                    self.exprs.add(TypedExpr::UnaryOp(UnaryOp {
                        kind: UnaryOpKind::Dereference,
                        type_id: reference.inner_type,
                        span,
                        expr: expression,
                    })),
                );
            }
        };
        CoerceResult::Fail(expression)
    }

    fn is_inside_companion_scope(
        &self,
        companion_namespace: Option<NamespaceId>,
        scope_id: ScopeId,
    ) -> bool {
        if let Some(companion_namespace) = companion_namespace {
            self.is_scope_inside_namespace(companion_namespace, scope_id)
        } else {
            false
        }
    }

    fn is_scope_inside_namespace(&self, namespace_id: NamespaceId, scope_id: ScopeId) -> bool {
        let ns_scope_id = self.namespaces.get_scope(namespace_id);
        self.scopes.scope_has_ancestor(scope_id, ns_scope_id)
    }

    fn eval_expr(
        &mut self,
        expr_id: ParsedExpressionId,
        mut ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let directives = self.ast.exprs.get_directives(expr_id);
        let debug_directive =
            directives.iter().find(|p| matches!(p, ParsedDirective::CompilerDebug { .. }));
        let conditional_compile_expr = directives.iter().find_map(|p| match p {
            ParsedDirective::ConditionalCompile { condition, .. } => Some(*condition),
            _ => None,
        });
        let is_debug = debug_directive.is_some();
        if is_debug {
            self.push_debug_level();
        }
        let mut self_ = scopeguard::guard(self, |s| {
            if is_debug {
                s.pop_debug_level()
            }
        });
        let should_compile = match conditional_compile_expr {
            None => true,
            Some(condition) => {
                let typed_condition =
                    self_.eval_const_expr(condition, Some(BOOL_TYPE_ID), ctx.scope_id, None)?;
                match typed_condition {
                    CompileTimeValue::Boolean(b) => b,
                    _ => {
                        return failf!(
                            self_.ast.exprs.get_span(condition),
                            "Condition must be a compile-time-known boolean"
                        )
                    }
                }
            }
        };
        if !should_compile {
            eprintln!("#if was false; yeeting in a unit for now");
            let span = self_.ast.exprs.get_span(expr_id);
            return Ok(self_.exprs.add(TypedExpr::Unit(span)));
        }

        let mut hinted_type = false;
        ctx.expected_type_id = match self_.ast.exprs.get_type_hint(expr_id) {
            Some(t) => {
                let type_id = self_.eval_type_expr(t, ctx.scope_id)?;
                hinted_type = true;
                Some(type_id)
            }
            None => ctx.expected_type_id,
        };
        let base_result = self_.eval_expr_inner(expr_id, ctx)?;
        if hinted_type {
            if let Some(expected_type_id) = ctx.expected_type_id {
                if let Err(msg) = self_.check_types(
                    expected_type_id,
                    self_.exprs.get(base_result).get_type(),
                    ctx.scope_id,
                ) {
                    return failf!(
                        self_.ast.exprs.get_span(expr_id),
                        "Expression had incorrect type: {msg}"
                    );
                }
            }
        }

        // FIXME: We gotta get rid of this coerce step its involved in every bug
        //        We can do instead a principled subtyping relation where a subtype decision
        //        is accompanied by a transformation expression
        let result = if let Some(expected_type_id) = &ctx.expected_type_id {
            // Try to coerce if types don't match
            let new_expr = match self_.coerce_expression_to_expected_type(
                *expected_type_id,
                base_result,
                ctx.scope_id,
                expr_id.into(),
            ) {
                CoerceResult::Fail(base_result) => base_result,
                CoerceResult::Coerced(reason, new_expr) => {
                    debug!(
                        "coerce succeeded with rule {reason} and resulted in expr {}",
                        self_.expr_to_string_with_type(new_expr),
                    );
                    new_expr
                }
            };
            Ok(new_expr)
        } else {
            Ok(base_result)
        }?;
        if is_debug {
            let expr_span = self_.ast.exprs.get_span(expr_id);
            eprintln!(
                "DEBUG EXPR\n{} hint {}\nRESULT\n{}",
                self_.ast.get_span_content(expr_span),
                self_.type_id_option_to_string(ctx.expected_type_id),
                self_.expr_to_string_with_type(result)
            );
        };
        Ok(result)
    }

    fn eval_expr_inner(
        &mut self,
        expr_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        debug!(
            "{}\neval_expr_inner: {}: hint {}",
            self.ast.get_span_content(self.ast.exprs.get_span(expr_id)),
            &self.ast.expr_id_to_string(expr_id),
            self.type_id_option_to_string(ctx.expected_type_id),
        );
        let expr = self.ast.exprs.get(expr_id);
        match expr {
            ParsedExpression::ListLiteral(list_expr) => {
                let expected_element_type: Option<TypeId> = match &ctx.expected_type_id {
                    Some(type_id) => match self.types.get(*type_id).as_list_instance() {
                        Some(arr) => Ok(Some(arr.element_type)),
                        None => Ok(None),
                    },
                    None => Ok(None),
                }?;
                let span = list_expr.span;
                let parsed_elements = list_expr.elements.clone();
                let element_count = parsed_elements.len();

                let mut list_lit_block = self.synth_block(ctx.scope_id, span);
                list_lit_block.statements = Vec::with_capacity(2 + element_count);
                let list_lit_scope = list_lit_block.scope_id;
                let mut element_type = None;
                let elements: Vec<TypedExprId> = {
                    let mut elements = Vec::with_capacity(element_count);
                    for elem in parsed_elements.iter() {
                        let element_expr = self.eval_expr(
                            *elem,
                            ctx.with_expected_type(element_type.or(expected_element_type)),
                        )?;
                        let this_element_type = self.exprs.get(element_expr).get_type();
                        if element_type.is_none() {
                            element_type = Some(this_element_type)
                        } else if let Err(msg) = self.check_types(
                            element_type.unwrap(),
                            this_element_type,
                            list_lit_scope,
                        ) {
                            return failf!(span, "List element had incorrect type: {msg}");
                        };
                        elements.push(element_expr);
                    }
                    elements
                };
                // Note: Typing of list literals is very suspicious, I'm not sure how to type it when there
                //        are no elements, I don't have an 'Unknown' type but maybe that's the
                //        ticket.
                //
                //        Trying is_inference here to use UNIT or fail.
                //        Failing during inference is like producing a type hole
                //        But if I report UNIT during inference we won't keep searching
                //        for a 'real' solution, yaknow?
                let element_type = match element_type.or(expected_element_type) {
                    Some(et) => et,
                    None => {
                        if ctx.is_inference {
                            return failf!(
                                span,
                                "Not enough information to determine empty list type"
                            );
                        } else {
                            UNIT_TYPE_ID
                        }
                    }
                };
                let list_lit_ctx = ctx.with_scope(list_lit_scope).with_no_expected_type();
                let count_expr = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                    value: TypedIntegerValue::U64(element_count as u64),
                    span,
                }));
                let list_new_fn_call = self.synth_typed_function_call(
                    qident!(self, span, ["List"], "withCapacity"),
                    &[element_type],
                    &[count_expr],
                    list_lit_ctx,
                )?;
                let list_variable = self.synth_variable_defn(
                    get_ident!(self, "list_literal"),
                    list_new_fn_call,
                    false,
                    false,
                    true,
                    list_lit_scope,
                );
                let mut set_elements = Vec::with_capacity(element_count);
                for element_value_expr in elements.into_iter() {
                    let push_call = self.synth_typed_function_call(
                        qident!(self, span, ["List"], "push"),
                        &[element_type],
                        &[list_variable.variable_expr, element_value_expr],
                        list_lit_ctx,
                    )?;
                    let type_id = self.exprs.get(push_call).get_type();
                    let push_stmt = self.stmts.add(TypedStmt::Expr(push_call, type_id));
                    set_elements.push(push_stmt);
                }
                self.push_block_stmt_id(&mut list_lit_block, list_variable.defn_stmt);
                list_lit_block.statements.extend(set_elements);
                let dereference_list_literal = self.synth_dereference(list_variable.variable_expr);
                self.add_expr_id_to_block(&mut list_lit_block, dereference_list_literal);
                Ok(self.exprs.add(TypedExpr::Block(list_lit_block)))
            }
            ParsedExpression::Struct(ast_struct) => {
                let mut field_values = Vec::new();
                let mut field_defns = Vec::new();
                let ast_struct = ast_struct.clone();
                let expected_struct = if let Some(expected_type) = ctx.expected_type_id {
                    match self.types.get(expected_type) {
                        Type::Struct(struc) => Some((expected_type, struc.clone())),
                        Type::Reference(refer) => match self.types.get(refer.inner_type) {
                            Type::Struct(struc) => Some((refer.inner_type, struc.clone())),
                            _other_reference => None,
                        },
                        other_ty => match other_ty.as_optional() {
                            Some(opt) => match self.types.get(opt.inner_type) {
                                Type::Struct(struc) => Some((opt.inner_type, struc.clone())),
                                _other_optional => None,
                            },
                            None => None,
                        },
                    }
                } else {
                    None
                };
                for (index, ast_field) in ast_struct.fields.iter().enumerate() {
                    let expected_field = expected_struct
                        .as_ref()
                        .and_then(|(_, rec)| rec.find_field(ast_field.name));
                    let expected_type_id = expected_field.map(|(_, f)| f.type_id);
                    let parsed_expr = match ast_field.expr.as_ref() {
                        None => self.ast.exprs.add_expression(ParsedExpression::Variable(
                            parse::Variable {
                                name: NamespacedIdentifier::naked(ast_field.name, ast_field.span),
                            },
                        )),
                        Some(expr) => *expr,
                    };
                    let expr =
                        self.eval_expr(parsed_expr, ctx.with_expected_type(expected_type_id))?;
                    let expr_type = self.exprs.get(expr).get_type();
                    field_defns.push(StructTypeField {
                        name: ast_field.name,
                        type_id: expr_type,
                        index: index as u32,
                        private: false,
                    });
                    field_values.push(StructField { name: ast_field.name, expr });
                }
                let struct_type = StructType {
                    fields: field_defns,
                    type_defn_info: None,
                    generic_instance_info: None,
                    ast_node: expr_id.into(),
                };
                let struct_type_id = match expected_struct {
                    None => {
                        let anon_struct_type_id = self.types.add_type(Type::Struct(struct_type));
                        Ok(anon_struct_type_id)
                    }
                    Some((expected_type_id, expected_struct)) => {
                        match self.typecheck_struct(&expected_struct, &struct_type, ctx.scope_id) {
                            Ok(_) => Ok(expected_type_id),
                            Err(_s) => {
                                let anon_struct_type_id =
                                    self.types.add_type(Type::Struct(struct_type));
                                Ok(anon_struct_type_id)
                            }
                        }
                    }
                }?;
                let typed_struct =
                    Struct { fields: field_values, span: ast_struct.span, type_id: struct_type_id };
                let expr = TypedExpr::Struct(typed_struct);
                Ok(self.exprs.add(expr))
            }
            ParsedExpression::If(if_expr) => self.eval_if_expr(&if_expr.clone(), ctx),
            ParsedExpression::While(while_expr) => self.eval_while_loop(&while_expr.clone(), ctx),
            ParsedExpression::Loop(loop_expr) => self.eval_loop_expr(&loop_expr.clone(), ctx),
            ParsedExpression::BinaryOp(_binary_op) => self.eval_binary_op(expr_id, ctx),
            ParsedExpression::UnaryOp(op) => {
                let op = op.clone();
                match op.op_kind {
                    ParsedUnaryOpKind::BooleanNegation => {
                        let negated_expr = self.synth_parsed_bool_not(op.expr);
                        self.eval_expr(negated_expr, ctx)
                    }
                }
            }
            ParsedExpression::Literal(Literal::Unit(span)) => {
                Ok(self.exprs.add(TypedExpr::Unit(*span)))
            }
            ParsedExpression::Literal(Literal::Char(byte, span)) => {
                Ok(self.exprs.add(TypedExpr::Char(*byte, *span)))
            }
            ParsedExpression::Literal(Literal::Numeric(int)) => {
                let numeric_expr = self.eval_numeric_value(&int.text, int.span, ctx)?;
                Ok(self.exprs.add(numeric_expr))
            }
            ParsedExpression::Literal(Literal::Bool(b, span)) => {
                let expr = TypedExpr::Bool(*b, *span);
                Ok(self.exprs.add(expr))
            }
            ParsedExpression::Literal(Literal::String(s, span)) => {
                let expr = TypedExpr::Str(s.clone(), *span);
                Ok(self.exprs.add(expr))
            }
            ParsedExpression::Variable(_variable) => {
                self.eval_variable(expr_id, ctx.scope_id, false)
            }
            ParsedExpression::FieldAccess(field_access) => {
                let field_access = field_access.clone();
                self.eval_field_access(&field_access, ctx, false)
            }
            ParsedExpression::Block(block) => {
                // TODO(clone big) This clone is actually sad because Block is still big. We need to intern blocks
                let block = block.clone();
                let block_scope =
                    self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);
                let block_ctx = ctx.with_scope(block_scope);
                let block = self.eval_block(&block, block_ctx, false)?;
                Ok(self.exprs.add(TypedExpr::Block(block)))
            }
            ParsedExpression::FnCall(fn_call) => {
                self.eval_function_call(&fn_call.clone(), None, ctx)
            }
            ParsedExpression::For(for_expr) => self.eval_for_expr(&for_expr.clone(), ctx),
            ParsedExpression::AnonEnumConstructor(anon_enum) => {
                let span = anon_enum.span;
                let expected_type = ctx.expected_type_id.ok_or_else(|| {
                    make_error(
                        "Could not infer enum type from context; try supplying the name",
                        anon_enum.span,
                    )
                })?;
                let enum_type_id = {
                    match self.types.get(expected_type) {
                        Type::Enum(_e) => Ok(expected_type),
                        Type::EnumVariant(ev) => Ok(ev.enum_type_id),
                        _ => failf!(
                            anon_enum.span,
                            "Could not infer expected enum type for '.' shorthand; expected type was {}",
                            self.type_id_to_string(expected_type)
                        ),
                    }
                }?;
                let enum_ctx = ctx.with_expected_type(Some(expected_type));
                self.eval_enum_constructor(
                    enum_type_id,
                    anon_enum.variant_name,
                    anon_enum.payload,
                    enum_ctx,
                    span,
                )
            }
            ParsedExpression::Is(is_expr) => {
                let is_expr = is_expr.clone();
                // If the 'is' is attached to an if/else, that is handled by if/else
                // This is just the case of the detached 'is' where we want to return a boolean
                // indicating whether or not the pattern matched only
                let true_expression = self.ast.exprs.add_expression(
                    parse::ParsedExpression::Literal(parse::Literal::Bool(true, is_expr.span)),
                );
                let false_expression = self.ast.exprs.add_expression(
                    parse::ParsedExpression::Literal(parse::Literal::Bool(false, is_expr.span)),
                );
                let true_case = parse::ParsedMatchCase {
                    patterns: smallvec![is_expr.pattern],
                    guard_condition_expr: None,
                    expression: true_expression,
                };
                let wildcard_pattern =
                    self.ast.patterns.add_pattern(parse::ParsedPattern::Wildcard(is_expr.span));
                let false_case = parse::ParsedMatchCase {
                    patterns: smallvec![wildcard_pattern],
                    guard_condition_expr: None,
                    expression: false_expression,
                };
                let as_match_expr = parse::ParsedMatchExpression {
                    match_subject: is_expr.target_expression,
                    cases: vec![true_case, false_case],
                    span: is_expr.span,
                };
                let match_expr_id =
                    self.ast.exprs.add_expression(parse::ParsedExpression::Match(as_match_expr));
                let partial_match = true;
                // For standalone 'is', we don't allow binding to patterns since they won't work
                let allow_bindings = false;
                self.eval_match_expr(match_expr_id, ctx, partial_match, allow_bindings)
            }
            ParsedExpression::Match(_match_expr) => {
                let partial_match = false;
                let allow_bindings = true;
                self.eval_match_expr(expr_id, ctx, partial_match, allow_bindings)
            }
            ParsedExpression::AsCast(_cast) => self.eval_cast(expr_id, ctx),
            ParsedExpression::Lambda(_lambda) => self.eval_lambda(expr_id, ctx),
            ParsedExpression::InterpolatedString(_is) => {
                let res = self.eval_interpolated_string(expr_id, ctx)?;
                Ok(res)
            }
            ParsedExpression::Builtin(span) => {
                failf!(*span, "builtins are all currently constant")
            }
        }
    }

    fn eval_while_loop(
        &mut self,
        while_expr: &ParsedWhileExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpression::Block(parsed_block) = self.ast.exprs.get(while_expr.body).clone()
        else {
            return failf!(while_expr.span, "'while' body must be a block");
        };
        let condition_span = self.ast.exprs.get_span(while_expr.cond);

        let mut condition_block = self.synth_block(ctx.scope_id, condition_span);

        let MatchingCondition { combined_condition, setup_statements, binding_statements, .. } =
            self.eval_matching_condition(
                while_expr.cond,
                ctx.with_scope(condition_block.scope_id),
            )?;

        condition_block.statements = Vec::with_capacity(setup_statements.len() + 1);
        condition_block.statements.extend_from_slice(&setup_statements);
        self.add_expr_id_to_block(&mut condition_block, combined_condition);
        let body_block_scope_id = self.scopes.add_child_scope(
            condition_block.scope_id,
            ScopeType::WhileLoopBody,
            None,
            None,
        );
        self.scopes
            .add_loop_info(body_block_scope_id, ScopeLoopInfo { break_type: Some(UNIT_TYPE_ID) });
        let mut body_block = TypedBlock {
            expr_type: UNIT_TYPE_ID,
            statements: Vec::with_capacity(binding_statements.len() + 1),
            scope_id: body_block_scope_id,
            span: parsed_block.span,
        };
        body_block.statements.extend(binding_statements.iter());

        let user_block =
            self.eval_block(&parsed_block, ctx.with_scope(body_block.scope_id), false)?;
        self.add_expr_to_block(&mut body_block, TypedExpr::Block(user_block));

        let loop_type =
            if condition_block.expr_type == NEVER_TYPE_ID { NEVER_TYPE_ID } else { UNIT_TYPE_ID };

        let condition_block_id = self.exprs.add(TypedExpr::Block(condition_block));
        Ok(self.exprs.add(TypedExpr::WhileLoop(WhileLoop {
            cond: condition_block_id,
            body: Box::new(body_block),
            type_id: loop_type,
            span: while_expr.span,
        })))
    }

    fn eval_loop_expr(
        &mut self,
        loop_expr: &ParsedLoopExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let body_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LoopExprBody, None, None);
        self.scopes.add_loop_info(body_scope, ScopeLoopInfo { break_type: ctx.expected_type_id });

        // Expected type is handled by loop info above, its needed by 'break's but notably we do not
        // want to require the loop's block to return a type other than Unit, so we pass None.
        let expected_expression_type_for_block = None;
        let block = self.eval_block(
            &loop_expr.body.clone(),
            ctx.with_scope(body_scope).with_expected_type(expected_expression_type_for_block),
            false,
        )?;

        let loop_info = self.scopes.get_loop_info(body_scope).unwrap();

        Ok(self.exprs.add(TypedExpr::LoopExpr(LoopExpr {
            body: Box::new(block),
            break_type: loop_info.break_type.unwrap_or(UNIT_TYPE_ID),
            span: loop_expr.span,
        })))
    }

    fn eval_interpolated_string(
        &mut self,
        expr_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.ast.exprs.get_span(expr_id);
        let ParsedExpression::InterpolatedString(is) = self.ast.exprs.get(expr_id) else {
            panic!()
        };
        let is = is.clone();

        let mut block = self.synth_block(ctx.scope_id, span);
        let block_scope = block.scope_id;
        let part_count = is.parts.len();
        let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
        let part_count_expr = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
            value: TypedIntegerValue::U64(part_count as u64),
            span,
        }));
        let new_string_builder = self.synth_typed_function_call(
            qident!(self, span, ["StringBuilder"], "withCapacity"),
            &[],
            &[part_count_expr],
            block_ctx,
        )?;
        let string_builder_var = self.synth_variable_defn_simple(
            get_ident!(self, "sb"),
            new_string_builder,
            block.scope_id,
        );
        self.push_block_stmt_id(&mut block, string_builder_var.defn_stmt);
        for part in is.parts.into_iter() {
            let string_expr = match part {
                parse::InterpolatedStringPart::String(s) => self.exprs.add(TypedExpr::Str(s, span)),
                parse::InterpolatedStringPart::Identifier(ident) => {
                    let variable_expr_id =
                        self.ast.exprs.add_expression(ParsedExpression::Variable(
                            parse::Variable { name: NamespacedIdentifier::naked(ident, span) },
                        ));
                    self.synth_show_ident_call(variable_expr_id, block_ctx)?
                }
            };
            debug_assert!(self.exprs.get(string_expr).get_type() == STRING_TYPE_ID);
            let push_call = self.synth_typed_function_call(
                qident!(self, span, ["StringBuilder"], "putString"),
                &[],
                &[string_builder_var.variable_expr, string_expr],
                block_ctx,
            )?;
            self.add_expr_id_to_block(&mut block, push_call);
        }
        let build_call = self.synth_typed_function_call(
            qident!(self, span, ["StringBuilder"], "build"),
            &[],
            &[string_builder_var.variable_expr],
            block_ctx,
        )?;
        self.add_expr_id_to_block(&mut block, build_call);
        Ok(self.exprs.add(TypedExpr::Block(block)))
    }

    #[cfg(any())]
    fn visit_inner_stmt_exprs_mut(
        module: &mut TypedModule,
        stmt_id: TypedStmtId,
        action: &mut impl FnMut(&mut TypedModule, TypedExprId),
    ) {
        let stmt = module.stmts.get(stmt_id);
        match stmt {
            TypedStmt::Expr(e, _) => action(module, *e),
            TypedStmt::Let(val_def) => action(module, val_def.initializer),
            TypedStmt::Assignment(assgn) => {
                let value = assgn.value;
                action(module, assgn.destination);
                action(module, value)
            }
        };
    }

    // Currently only used for fixup_capture_expr
    #[cfg(any())]
    fn visit_inner_exprs_mut(
        module: &mut TypedModule,
        expr: TypedExprId,
        mut action: impl FnMut(&mut TypedModule, TypedExprId),
    ) {
        // Try implementing a mutable iterator instead
        match module.exprs.get(expr) {
            TypedExpr::Unit(_) => (),
            TypedExpr::Char(_, _) => (),
            TypedExpr::Bool(_, _) => (),
            TypedExpr::Integer(_) => (),
            TypedExpr::Float(_) => (),
            TypedExpr::Str(_, _) => (),
            TypedExpr::Struct(s) => {
                for f in s.fields.clone().iter() {
                    action(module, f.expr);
                }
            }
            TypedExpr::Variable(_) => (),
            TypedExpr::StructFieldAccess(field_access) => {
                action(module, field_access.base);
            }
            TypedExpr::BinaryOp(binary_op) => {
                let lhs = binary_op.lhs;
                let rhs = binary_op.rhs;
                action(module, lhs);
                action(module, rhs);
            }
            TypedExpr::UnaryOp(unary_op) => {
                action(module, unary_op.expr);
            }
            TypedExpr::Block(block) => {
                for stmt in block.statements.clone().iter() {
                    TypedModule::visit_inner_stmt_exprs_mut(module, *stmt, &mut action);
                }
            }
            TypedExpr::Call(call) => {
                let args = call.args.clone();
                match call.callee {
                    Callee::DynamicLambda(expr) => action(module, expr),
                    Callee::DynamicFunction(expr) => action(module, expr),
                    _ => {}
                };
                for arg in args.clone().iter() {
                    action(module, *arg)
                }
            }
            TypedExpr::If(typed_if) => {
                let condition = typed_if.condition;
                let consequent = typed_if.consequent;
                let alternate = typed_if.alternate;
                action(module, condition);
                action(module, consequent);
                action(module, alternate);
            }
            TypedExpr::WhileLoop(while_loop) => {
                let stmts = while_loop.body.statements.clone();
                action(module, while_loop.cond);
                for stmt in stmts.iter() {
                    TypedModule::visit_inner_stmt_exprs_mut(module, *stmt, &mut action);
                }
            }
            TypedExpr::Match(typed_match) => {
                let typed_match = typed_match.clone();
                for let_stmt in &typed_match.initial_let_statements {
                    TypedModule::visit_inner_stmt_exprs_mut(module, *let_stmt, &mut action);
                }
                for arm in typed_match.arms.iter() {
                    action(module, arm.pattern_condition);
                    if let Some(guard_condition) = arm.guard_condition {
                        action(module, guard_condition);
                    };
                    for binding_stmt in &arm.pattern_bindings {
                        TypedModule::visit_inner_stmt_exprs_mut(module, *binding_stmt, &mut action);
                    }
                    action(module, arm.consequent_expr)
                }
            }
            TypedExpr::LoopExpr(loop_expr) => {
                for stmt in loop_expr.body.statements.clone().iter() {
                    TypedModule::visit_inner_stmt_exprs_mut(module, *stmt, &mut action);
                }
            }
            TypedExpr::EnumConstructor(_) => (),
            TypedExpr::EnumIsVariant(enum_is_variant) => {
                action(module, enum_is_variant.target_expr)
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                action(module, enum_get_payload.target_expr)
            }
            TypedExpr::Cast(cast) => action(module, cast.base_expr),
            TypedExpr::Return(ret) => action(module, ret.value),
            TypedExpr::Break(brk) => action(module, brk.value),
            TypedExpr::Lambda(_) => (),
            TypedExpr::FunctionName(_) => (),
            TypedExpr::PendingCapture(_) => (),
        }
    }

    fn eval_lambda(
        &mut self,
        expr_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        fn fixup_capture_expr_new(
            module: &mut TypedModule,
            environment_param_variable_id: VariableId,
            captured_variable_id: VariableId,
            env_struct_type: TypeId,
            span: SpanId,
        ) -> TypedExpr {
            let v = module.variables.get(captured_variable_id);
            let variable_type = v.type_id;
            let env_struct_reference_type = module.types.add_reference_type(env_struct_type);
            // Note: Can't capture 2 variables of the same name in a lambda. Might not
            //       actually be a problem
            let (_field_index, env_struct_field) =
                module.types.get(env_struct_type).expect_struct().find_field(v.name).unwrap();
            let field_name = env_struct_field.name;
            let field_index = env_struct_field.index;
            let env_variable_expr = module.exprs.add(TypedExpr::Variable(VariableExpr {
                variable_id: environment_param_variable_id,
                type_id: env_struct_reference_type,
                span,
            }));
            let env_field_access = TypedExpr::StructFieldAccess(FieldAccess {
                base: module.synth_dereference(env_variable_expr),
                target_field: field_name,
                target_field_index: field_index,
                result_type: variable_type,
                struct_type: env_struct_type,
                is_referencing: false,
                span,
            });
            env_field_access
        }
        let lambda = self.ast.exprs.get(expr_id).expect_lambda();
        let lambda_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LambdaScope, None, None);
        let lambda_arguments = lambda.arguments.clone();
        let lambda_body = lambda.body;
        let span = lambda.span;
        let body_span = self.ast.exprs.get_span(lambda.body);
        let mut typed_params = VecDeque::with_capacity(lambda_arguments.len() + 1);
        if let Some(t) = ctx.expected_type_id {
            debug!(
                "lambda: expected type is {} {}",
                self.types.get(t).kind_name(),
                self.type_id_to_string(t)
            );
        }

        // nocommit: clean this up; formalize and centralized what counts as a 'function-like'
        // In this case, we're looking at 'hinted' or 'expected' types. So these have to be types
        // that you can write, or express, so that throws out Lambda, since you can't 'expect'
        // a unique lambda. Lambda object is fine, as in I expect dyn[\T -> T], and a some quant
        // function is fine. Function reference is interesting. I don't think a closure can be
        // 'lifted' to a function reference because it may capture things, they get called
        // differently. You can only go the other way (function to closure that captures nothing)
        // Then there's the actual FunctionType, which you can never actually have a value of, so
        // will probably never be the hinted type
        let expected_function_type = ctx
            .expected_type_id
            .and_then(|et| match self.types.get(et) {
                Type::TypeParameter(tp) if tp.is_function() => {
                    Some(self.types.get(tp.function_type.unwrap()))
                }
                Type::LambdaObject(lam_obj) => Some(self.types.get(lam_obj.function_type)),
                _ => None,
            })
            .map(|ft| ft.as_function().unwrap().clone());
        let declared_expected_return_type = match lambda.return_type {
            None => None,
            Some(return_type_expr) => Some(self.eval_type_expr(return_type_expr, ctx.scope_id)?),
        };
        let expected_return_type = declared_expected_return_type
            .or(expected_function_type.as_ref().map(|f| f.return_type));

        self.scopes.add_lambda_info(
            lambda_scope_id,
            ScopeLambdaInfo {
                expected_return_type,
                captured_variables: smallvec![],
                capture_exprs_for_fixup: smallvec![],
            },
        );

        for (index, arg) in lambda_arguments.iter().enumerate() {
            let arg_type_id = match arg.ty {
                Some(type_expr) => self.eval_type_expr(type_expr, ctx.scope_id)?,
                None => {
                    let Some(expected_function_type) = expected_function_type.as_ref() else {
                        return failf!(
                            arg.span,
                            "Cannot infer lambda parameter type {} without more context",
                            self.name_of(arg.binding)
                        );
                    };
                    let Some(expected_ty) = expected_function_type.params.get(index) else {
                        return failf!(arg.span, "Cannot infer lambda parameter type {}: expected type has fewer parameters than lambda", self.name_of(arg.binding));
                    };
                    expected_ty.type_id
                }
            };
            typed_params.push_back(FnParamType {
                name: arg.binding,
                type_id: arg_type_id,
                is_context: false,
                is_lambda_env: false,
                span: arg.span,
            });
        }

        let mut param_variables = VecDeque::with_capacity(typed_params.len());
        let lambda_scope = self.scopes.get_scope_mut(lambda_scope_id);
        for typed_arg in typed_params.iter() {
            let name = typed_arg.name;
            let variable_id = self.variables.add_variable(Variable {
                name,
                type_id: typed_arg.type_id,
                is_mutable: false,
                owner_scope: lambda_scope_id,
                is_context: false,
                constant_id: None,
            });
            lambda_scope.add_variable(name, variable_id);
            param_variables.push_back(variable_id)
        }

        // Coerce parsed expr to block, call eval_block with needs_terminator = true
        let ast_body_block = match self.ast.exprs.get(lambda_body) {
            ParsedExpression::Block(b) => b.clone(),
            other_expr => {
                let block = parse::Block {
                    span: other_expr.get_span(),
                    stmts: vec![self.ast.stmts.add(parse::ParsedStmt::LoneExpression(lambda_body))],
                };
                block
            }
        };
        let body = self.eval_block(
            &ast_body_block,
            ctx.with_scope(lambda_scope_id).with_expected_type(expected_return_type),
            true,
        )?;
        if let Some(expected_return_type) = expected_return_type {
            if let Err(msg) = self.check_types(expected_return_type, body.expr_type, ctx.scope_id) {
                return failf!(body.span, "Closure returns incorrect type: {msg}");
            }
        }

        // Note: NEVER hardcoded stuff that would probably prefer to be some
        // sort of principled call to 'unify_types'
        let return_type = match body.expr_type {
            NEVER_TYPE_ID => expected_return_type.unwrap_or(NEVER_TYPE_ID),
            _ => body.expr_type,
        };

        let lambda_info = self.scopes.get_lambda_info(lambda_scope_id);
        let env_fields = lambda_info
            .captured_variables
            .iter()
            .enumerate()
            .map(|(index, captured_variable_id)| {
                let v = self.variables.get(*captured_variable_id);
                StructTypeField {
                    type_id: v.type_id,
                    name: v.name,
                    index: index as u32,
                    private: false,
                }
            })
            .collect();
        let env_field_exprs = lambda_info
            .captured_variables
            .iter()
            .map(|captured_variable_id| {
                let v = self.variables.get(*captured_variable_id);
                self.exprs.add(TypedExpr::Variable(VariableExpr {
                    type_id: v.type_id,
                    variable_id: *captured_variable_id,
                    span,
                }))
            })
            .collect();
        let environment_struct_type = self.types.add_type(Type::Struct(StructType {
            fields: env_fields,
            type_defn_info: None,
            generic_instance_info: None,
            ast_node: expr_id.into(),
        }));
        let environment_struct = self.synth_struct_expr(
            environment_struct_type,
            env_field_exprs,
            ctx.scope_id,
            body_span,
        );
        let environment_struct_reference_type =
            self.types.add_reference_type(environment_struct_type);
        let environment_param = FnParamType {
            name: get_ident!(self, LAMBDA_ENV_PARAM_NAME),
            type_id: environment_struct_reference_type,
            is_context: false,
            is_lambda_env: true,
            span: body_span,
        };
        let environment_param_variable_id = self.variables.add_variable(Variable {
            name: environment_param.name,
            type_id: environment_struct_reference_type,
            is_mutable: false,
            owner_scope: lambda_scope_id,
            is_context: false,
            constant_id: None,
        });
        typed_params.push_front(environment_param);
        param_variables.push_front(environment_param_variable_id);

        let environment_param_variable_id = param_variables[0];
        let body_expr_id = self.exprs.add(TypedExpr::Block(body));

        let pending_fixups =
            self.scopes.get_lambda_info(lambda_scope_id).capture_exprs_for_fixup.clone();

        for pending_fixup in pending_fixups {
            let TypedExpr::PendingCapture(pc) = self.exprs.get(pending_fixup) else {
                unreachable!()
            };
            let field_access_expr = fixup_capture_expr_new(
                self,
                environment_param_variable_id,
                pc.captured_variable_id,
                environment_struct_type,
                pc.span,
            );
            *self.exprs.get_mut(pending_fixup) = field_access_expr;
        }

        let function_type = self.types.add_type(Type::Function(FunctionType {
            params: typed_params.into(),
            return_type,
            defn_info: None,
        }));
        let encl_fn_name = self
            .get_function(
                self.scopes
                    .nearest_parent_function(ctx.scope_id)
                    .expect("lambda to be inside a function"),
            )
            .name;
        let name = self.ast.identifiers.intern(format!(
            "{}_{{lambda}}_{}",
            self.name_of(encl_fn_name),
            lambda_scope_id,
        ));
        let name_string = self.make_qualified_name(ctx.scope_id, name, "__", true);
        let name = self.ast.identifiers.intern(name_string);

        let body_function_id = self.add_function(TypedFunction {
            name,
            scope: lambda_scope_id,
            param_variables: param_variables.into(),
            type_params: smallvec![],
            body_block: Some(body_expr_id),
            intrinsic_type: None,
            linkage: Linkage::Standard,
            child_specializations: vec![],
            specialization_info: None,
            parsed_id: expr_id.into(),
            type_id: function_type,
            compiler_debug: false,
            kind: TypedFunctionKind::Lambda,
            is_concrete: false,
        });
        let lambda_type_id = self.types.add_lambda(
            &self.ast.identifiers,
            function_type,
            environment_struct,
            environment_struct_type,
            body_function_id,
            expr_id.into(),
        );
        Ok(self.exprs.add(TypedExpr::Lambda(LambdaExpr { lambda_type: lambda_type_id, span })))
    }

    fn eval_match_expr(
        &mut self,
        match_expr_id: ParsedExpressionId,
        ctx: EvalExprContext,
        partial: bool,
        allow_bindings: bool,
    ) -> TyperResult<TypedExprId> {
        let match_parsed_expr = self.ast.exprs.get(match_expr_id).as_match().unwrap().clone();
        if match_parsed_expr.cases.is_empty() {
            return Err(make_error("Match expression with no arms", match_parsed_expr.span));
        }
        let match_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);
        let subject_expr =
            self.eval_expr(match_parsed_expr.match_subject, ctx.with_no_expected_type())?;

        // Mangled; not a user-facing binding
        let match_subject_ident = self.ast.identifiers.intern("match_subject");
        let match_subject_variable =
            self.synth_variable_defn_simple(match_subject_ident, subject_expr, ctx.scope_id);

        let match_expr_span = match_parsed_expr.span;
        let arms_ctx = ctx.with_scope(match_scope_id);
        let mut arms = self.eval_match_arms(
            match_subject_variable.variable_expr,
            &match_parsed_expr.cases,
            arms_ctx,
            partial,
            allow_bindings,
        )?;

        let fallback_arm = TypedMatchArm {
            patterns: smallvec![TypedPattern::LiteralBool(true, match_expr_span)],
            setup_statements: smallvec![],
            pattern_condition: self.exprs.add(TypedExpr::Bool(true, match_expr_span)),
            pattern_bindings: smallvec![],
            guard_condition: None,
            consequent_expr: self.synth_crash_call(
                "Match Error".to_string(),
                match_expr_span,
                ctx.with_no_expected_type(),
            )?,
        };
        arms.push(fallback_arm);

        // TODO: Principled handling of never, which means calling into some unit of code
        // whose job it is to unify or otherwise reduce two or more types into 1, or produce an
        // error?
        //
        // The result type of the match is the type of the first non-never arm, or never
        // They've already been typechecked against each other.
        let match_result_type = arms
            .iter()
            .find_map(|arm| {
                let conseqent_type = self.exprs.get(arm.consequent_expr).get_type();
                if conseqent_type != NEVER_TYPE_ID {
                    Some(conseqent_type)
                } else {
                    None
                }
            })
            .unwrap_or(NEVER_TYPE_ID);
        Ok(self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: vec![match_subject_variable.defn_stmt],
            result_type: match_result_type,
            arms,
            span: match_expr_span,
        })))
    }

    fn eval_match_arms(
        &mut self,
        target_expr: TypedExprId,
        cases: &[parse::ParsedMatchCase],
        ctx: EvalExprContext,
        partial_match: bool,
        allow_bindings: bool,
    ) -> TyperResult<Vec<TypedMatchArm>> {
        let mut typed_arms: Vec<TypedMatchArm> = Vec::new();

        let mut expected_arm_type_id = ctx.expected_type_id;
        let match_scope_id = ctx.scope_id;
        let mut all_unguarded_patterns = Vec::with_capacity(cases.len());
        let target_expr_type = self.exprs.get(target_expr).get_type();
        let target_expr_span = self.exprs.get(target_expr).get_span();
        for parsed_case in cases.iter() {
            let arm_expr_span = self.ast.exprs.get_span(parsed_case.expression);
            let mut arm_patterns = Vec::with_capacity(parsed_case.patterns.len());
            let multi_pattern = parsed_case.patterns.len() > 1;
            let mut expected_bindings: Option<Vec<VariablePattern>> = None;
            for pattern_id in parsed_case.patterns.iter() {
                let pattern = self.compile_pattern(
                    *pattern_id,
                    target_expr_type,
                    match_scope_id,
                    allow_bindings,
                )?;

                // If a match arm has multiple patterns, they must produce the exact same
                // set of variable bindings: matching name and type
                if multi_pattern {
                    match &expected_bindings {
                        None => {
                            expected_bindings = Some(pattern.all_bindings());
                        }
                        Some(expected_bindings) => {
                            let this_pattern_bindings = pattern.all_bindings();
                            for (exp_binding, this_binding) in
                                expected_bindings.iter().zip(this_pattern_bindings.iter())
                            {
                                if exp_binding.name != this_binding.name {
                                    return failf!(this_binding.span, "Patterns in a multiple pattern arm must have the exact same bindings");
                                }
                                if exp_binding.type_id != this_binding.type_id {
                                    return failf!(
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings; but the type differs for {}: {} vs {}",
                                        self.name_of(exp_binding.name),
                                        self.type_id_to_string(exp_binding.type_id),
                                        self.type_id_to_string(this_binding.type_id)
                                    );
                                }
                            }
                        }
                    }
                }

                arm_patterns.push(pattern.clone());
                if parsed_case.guard_condition_expr.is_none() {
                    all_unguarded_patterns.push(pattern);
                }
            }

            // Note: We compile the arm expression (and clone the guard condition) as many times as there are patterns, since each
            // one has its own scope. To get around this we'd have to create only one compiled arm even for
            // multi-pattern binding arms, and have the condition be a boolean OR of the various
            // arms, and somehow compile in the right variables defns based on which one passed.
            // Which isn't possible to know at compile time. So I think this is just where we are.
            // It'd be nice to re-use the typed expr across different scopes, but we can't do that
            for pattern in arm_patterns.into_iter() {
                let arm_scope_id =
                    self.scopes.add_child_scope(match_scope_id, ScopeType::MatchArm, None, None);
                let mut setup_statements = smallvec![];
                let mut binding_statements = smallvec![];
                let condition = self.compile_pattern_in_scope(
                    &pattern,
                    target_expr,
                    &mut setup_statements,
                    &mut binding_statements,
                    false,
                    arm_scope_id,
                )?;

                let guard_condition = match parsed_case.guard_condition_expr {
                    None => None,
                    Some(guard_condition_expr_id) => Some(self.eval_expr(
                        guard_condition_expr_id,
                        ctx.with_scope(arm_scope_id).with_expected_type(Some(BOOL_TYPE_ID)),
                    )?),
                };

                // Once we've evaluated the arm pattern, we can eval the consequent expression inside of it,
                // since the bindings are now in scope inside arm_block
                let consequent_expr = self.eval_expr(
                    parsed_case.expression,
                    ctx.with_scope(arm_scope_id).with_expected_type(expected_arm_type_id),
                )?;
                let consequent_expr_type = self.exprs.get(consequent_expr).get_type();

                // TODO: principled unify of never
                if let Some(expected_arm_type_id) = expected_arm_type_id.as_ref() {
                    // Never is divergent so need not contribute to the overall type of the pattern
                    if consequent_expr_type != NEVER_TYPE_ID {
                        if let Err(msg) = self.check_types(
                            *expected_arm_type_id,
                            consequent_expr_type,
                            match_scope_id,
                        ) {
                            return failf!(arm_expr_span, "Match arm has wrong type. {}", msg);
                        }
                    }
                }

                if consequent_expr_type != NEVER_TYPE_ID {
                    expected_arm_type_id = Some(consequent_expr_type);
                }
                typed_arms.push(TypedMatchArm {
                    patterns: smallvec![pattern],
                    setup_statements,
                    pattern_condition: condition,
                    pattern_bindings: binding_statements,
                    guard_condition,
                    consequent_expr,
                });
            }
        }

        // Exhaustiveness Checking
        if !partial_match {
            let trial_constructors: Vec<PatternConstructor> =
                self.generate_constructors_for_type(target_expr_type, target_expr_span);
            let mut trial_alives: Vec<bool> = vec![true; trial_constructors.len()];
            let mut pattern_kill_counts: Vec<usize> = vec![0; all_unguarded_patterns.len()];
            'trial: for (trial_index, trial_expr) in trial_constructors.iter().enumerate() {
                '_pattern: for (index, pattern) in all_unguarded_patterns.iter().enumerate() {
                    if TypedModule::pattern_matches(pattern, trial_expr) {
                        pattern_kill_counts[index] += 1;
                        trial_alives[trial_index] = false;
                        continue 'trial;
                    }
                }
            }

            if let Some(alive_index) = trial_alives.iter().position(|p| *p) {
                let pattern = &trial_constructors[alive_index];
                return failf!(
                    target_expr_span,
                    "Unhandled pattern: {}",
                    self.pattern_ctor_to_string(pattern)
                );
            }

            if let Some(useless_index) = pattern_kill_counts.iter().position(|p| *p == 0) {
                // patterns[0]: For actual match expressions, which this is, we'll always have
                // exactly 1 pattern per arm
                let pattern = &typed_arms[useless_index].patterns[0];
                if !pattern.has_innumerable_literal() {
                    return failf!(
                        pattern.span_id(),
                        "Useless pattern: {}",
                        self.pattern_to_string(pattern)
                    );
                }
            }
        }

        Ok(typed_arms)
    }

    /// For each match case we output
    /// 1) a series of statements to bind variables that are used by the condition and by the
    ///    consequent code, if the condition and guard pass
    /// 2) An expr of type boolean, this is the result of the Pattern, compiled into a boolean
    ///    expression
    /// 3) We push statements into `binding_statements` which will get run before our pattern and
    ///    guard and consequent run
    fn compile_pattern_in_scope(
        &mut self,
        pattern: &TypedPattern,
        target_expr: TypedExprId,
        setup_statements: &mut SmallVec<[TypedStmtId; 4]>,
        binding_statements: &mut SmallVec<[TypedStmtId; 4]>,
        is_immediately_inside_reference_pattern: bool,
        arm_scope_id: ScopeId,
    ) -> TyperResult<TypedExprId> {
        let target_expr_type = self.exprs.get(target_expr).get_type();
        match pattern {
            TypedPattern::Struct(struct_pattern) => {
                let mut boolean_exprs: Vec<TypedExprId> =
                    Vec::with_capacity(struct_pattern.fields.len());
                for pattern_field in struct_pattern.fields.iter() {
                    let struct_type = self.types.get_type_id_dereferenced(target_expr_type);
                    let is_referencing = is_immediately_inside_reference_pattern;
                    let result_type = if is_referencing {
                        self.types.add_reference_type(pattern_field.field_type_id)
                    } else {
                        pattern_field.field_type_id
                    };
                    let get_struct_field =
                        self.exprs.add(TypedExpr::StructFieldAccess(FieldAccess {
                            base: target_expr,
                            target_field: pattern_field.name,
                            target_field_index: pattern_field.field_index,
                            result_type,
                            struct_type,
                            is_referencing,
                            span: struct_pattern.span,
                        }));
                    let var_name = self
                        .ast
                        .identifiers
                        .intern(format!("field_{}", self.name_of(pattern_field.name)));
                    let struct_field_variable =
                        self.synth_variable_defn_simple(var_name, get_struct_field, arm_scope_id);
                    setup_statements.push(struct_field_variable.defn_stmt);
                    let condition = self.compile_pattern_in_scope(
                        &pattern_field.pattern,
                        struct_field_variable.variable_expr,
                        setup_statements,
                        binding_statements,
                        is_referencing,
                        arm_scope_id,
                    )?;
                    boolean_exprs.push(condition);
                }
                let final_condition = boolean_exprs
                    .into_iter()
                    .reduce(|a, b| self.synth_binary_bool_op(BinaryOpKind::And, a, b))
                    .unwrap();
                Ok(final_condition)
            }
            TypedPattern::Enum(enum_pattern) => {
                let is_referencing = is_immediately_inside_reference_pattern;
                let is_variant_target =
                    if is_referencing { self.synth_dereference(target_expr) } else { target_expr };
                let is_variant_condition =
                    self.exprs.add(TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                        target_expr: is_variant_target,
                        variant_name: enum_pattern.variant_tag_name,
                        variant_index: enum_pattern.variant_index,
                        span: enum_pattern.span,
                    }));
                let inner_condition = if let Some(payload_pattern) = enum_pattern.payload.as_ref() {
                    let enum_type = self.types.get(enum_pattern.enum_type_id).expect_enum();
                    let variant = enum_type.variant_by_index(enum_pattern.variant_index).unwrap();
                    let variant_name = variant.name;
                    let variant_index = variant.index;
                    let Some(payload_type_id) = variant.payload else {
                        return failf!(
                            enum_pattern.span,
                            "Impossible pattern: variant does not have payload",
                        );
                    };
                    let result_type_id = if is_referencing {
                        self.types.add_reference_type(payload_type_id)
                    } else {
                        payload_type_id
                    };
                    let get_payload_expr =
                        self.exprs.add(TypedExpr::EnumGetPayload(GetEnumPayload {
                            target_expr,
                            result_type_id,
                            variant_name,
                            variant_index,
                            is_referencing,
                            span: enum_pattern.span,
                        }));
                    let var_name = self
                        .ast
                        .identifiers
                        .intern(format!("payload_{}", self.name_of(variant_name)));
                    let payload_variable =
                        self.synth_variable_defn_simple(var_name, get_payload_expr, arm_scope_id);
                    setup_statements.push(payload_variable.defn_stmt);
                    let condition = self.compile_pattern_in_scope(
                        payload_pattern,
                        payload_variable.variable_expr,
                        setup_statements,
                        binding_statements,
                        is_referencing,
                        arm_scope_id,
                    )?;
                    Some(condition)
                } else {
                    None
                };
                let final_condition = match inner_condition.map(|expr_id| self.exprs.get(expr_id)) {
                    None => is_variant_condition,
                    Some(TypedExpr::Bool(true, _)) => is_variant_condition,
                    Some(_inner_condition) => self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
                        kind: BinaryOpKind::And,
                        ty: BOOL_TYPE_ID,
                        lhs: is_variant_condition,
                        rhs: inner_condition.unwrap(),
                        span: enum_pattern.span,
                    })),
                };
                Ok(final_condition)
            }
            TypedPattern::Variable(variable_pattern) => {
                let variable_ident = variable_pattern.name;
                let binding_variable =
                    self.synth_variable_defn_visible(variable_ident, target_expr, arm_scope_id);
                binding_statements.push(binding_variable.defn_stmt);
                Ok(self.exprs.add(TypedExpr::Bool(true, variable_pattern.span)))
            }
            TypedPattern::Wildcard(span) => Ok(self.exprs.add(TypedExpr::Bool(true, *span))),
            TypedPattern::Reference(reference_pattern) => {
                let target_expr = if is_immediately_inside_reference_pattern {
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                let inner_condition = self.compile_pattern_in_scope(
                    &reference_pattern.inner_pattern,
                    target_expr,
                    setup_statements,
                    binding_statements,
                    true,
                    arm_scope_id,
                )?;
                Ok(inner_condition)
            }
            pat => {
                match pat {
                    TypedPattern::LiteralUnit(_) => true,
                    TypedPattern::LiteralChar(_, _) => true,
                    TypedPattern::LiteralInteger(_, _) => true,
                    TypedPattern::LiteralFloat(_, _) => true,
                    TypedPattern::LiteralBool(_, _) => true,
                    TypedPattern::LiteralString(_, _) => true,
                    _ => unreachable!("all non-literals should be handled by now"),
                };
                let target_expr = if is_immediately_inside_reference_pattern {
                    // Literal patterns don't do anything special for references; they just need to
                    // function on the de-rereferenced target. Whereas structs, enums, even
                    // reference patterns do different and unique things when matching on
                    // references
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                match pat {
                    TypedPattern::LiteralUnit(span) => {
                        Ok(self.exprs.add(TypedExpr::Bool(true, *span)))
                    }
                    TypedPattern::LiteralChar(byte, span) => {
                        let char_expr = self.exprs.add(TypedExpr::Char(*byte, *span));
                        let bin_op = self.synth_equals_binop(target_expr, char_expr, *span);
                        Ok(bin_op)
                    }
                    TypedPattern::LiteralInteger(int_value, span) => {
                        let bin_op = BinaryOp {
                            kind: BinaryOpKind::Equals,
                            ty: BOOL_TYPE_ID,
                            lhs: target_expr,
                            rhs: self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                                value: *int_value,
                                span: *span,
                            })),
                            span: *span,
                        };
                        Ok(self.exprs.add(TypedExpr::BinaryOp(bin_op)))
                    }
                    TypedPattern::LiteralFloat(float_value, span) => {
                        let bin_op = BinaryOp {
                            kind: BinaryOpKind::Equals,
                            ty: BOOL_TYPE_ID,
                            lhs: target_expr,
                            rhs: self.exprs.add(TypedExpr::Float(TypedFloatExpr {
                                value: *float_value,
                                span: *span,
                            })),
                            span: *span,
                        };
                        Ok(self.exprs.add(TypedExpr::BinaryOp(bin_op)))
                    }
                    TypedPattern::LiteralBool(bool_value, span) => {
                        let bool_expr = self.exprs.add(TypedExpr::Bool(*bool_value, *span));
                        let bin_op = self.synth_equals_binop(target_expr, bool_expr, *span);
                        Ok(bin_op)
                    }
                    TypedPattern::LiteralString(string_value, span) => {
                        let string_expr =
                            self.exprs.add(TypedExpr::Str(string_value.clone(), *span));
                        let condition = self.synth_equals_call(target_expr, string_expr, *span)?;
                        Ok(condition)
                    }
                    other_pat => {
                        unreachable!("should only be literal patterns from here: {other_pat:?}")
                    }
                }
            }
        }
    }

    fn eval_cast(
        &mut self,
        expr_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let cast = self.ast.exprs.get(expr_id).expect_cast();
        let cast = cast.clone();
        let base_expr = self.eval_expr(cast.base_expr, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get(base_expr).get_type();
        let target_type = self.eval_type_expr(cast.dest_type, ctx.scope_id)?;
        if base_expr_type == target_type {
            return failf!(cast.span, "Useless cast");
        }
        let (cast_type, output_type) = match self.types.get(base_expr_type) {
            Type::Integer(from_integer_type) => match self.types.get(target_type) {
                Type::Integer(to_integer_type) => {
                    let cast_type = match from_integer_type.width().cmp(&to_integer_type.width()) {
                        Ordering::Less => CastType::IntegerExtend,
                        Ordering::Greater => CastType::IntegerTruncate,
                        Ordering::Equal => CastType::KnownNoOp,
                    };
                    Ok((cast_type, target_type))
                }
                Type::Char(_) => {
                    if from_integer_type.width() == NumericWidth::B8 {
                        Ok((CastType::Integer8ToChar, target_type))
                    } else {
                        failf!(
                            cast.span,
                            "Cannot cast integer '{}' to char, must be 8 bits",
                            from_integer_type
                        )
                    }
                }
                Type::Pointer(_) => {
                    if *from_integer_type == IntegerType::U64 {
                        Ok((CastType::IntegerToPointer, target_type))
                    } else {
                        failf!(
                            cast.span,
                            "Cannot cast integer '{}' to Pointer (must be u64; one day usize)",
                            from_integer_type
                        )
                    }
                }
                Type::Float(_to_float_type) => {
                    // We're just going to allow these casts and make it UB if it doesn't fit, the LLVM
                    // default. If I find a saturating version in LLVM I'll use that instead
                    Ok((CastType::IntegerToFloat, target_type))
                }
                _ => failf!(
                    cast.span,
                    "Cannot cast integer '{}' to '{}'",
                    from_integer_type,
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Float(from_float_type) => match self.types.get(target_type) {
                Type::Float(to_float_type) => {
                    let cast_type = match from_float_type.size.cmp(&to_float_type.size) {
                        Ordering::Less => CastType::FloatTruncate,
                        Ordering::Greater => CastType::FloatExtend,
                        Ordering::Equal => CastType::KnownNoOp,
                    };
                    Ok((cast_type, target_type))
                }
                // We're just going to allow these casts and make it UB if it doesn't fit, the LLVM
                // default. If I find a saturating version in LLVM I'll use that instead
                Type::Integer(_to_int_type) => Ok((CastType::FloatToInteger, target_type)),
                _ => failf!(
                    cast.span,
                    "Cannot cast float to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Char(_) => match self.types.get(target_type) {
                Type::Integer(_to_integer_type) => {
                    Ok((CastType::IntegerExtendFromChar, target_type))
                }
                _ => failf!(
                    cast.span,
                    "Cannot cast char to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Reference(_refer) => match self.types.get(target_type) {
                Type::Pointer(_) => Ok((CastType::ReferenceToPointer, POINTER_TYPE_ID)),
                _ => failf!(
                    cast.span,
                    "Cannot cast reference to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Pointer(_) => match self.types.get(target_type) {
                Type::Reference(_refer) => Ok((CastType::PointerToReference, target_type)),
                Type::Integer(IntegerType::U64) => Ok((CastType::PointerToInteger, target_type)),
                _ => failf!(
                    cast.span,
                    "Cannot cast Pointer to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            _ => failf!(
                cast.span,
                "Cannot cast '{}' to '{}'",
                self.type_id_to_string(base_expr_type).blue(),
                self.type_id_to_string(target_type).blue()
            ),
        }?;
        Ok(self.exprs.add(TypedExpr::Cast(TypedCast {
            base_expr,
            target_type_id: output_type,
            cast_type,
            span: cast.span,
        })))
    }

    fn eval_for_expr(
        &mut self,
        for_expr: &ForExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let binding_ident = for_expr.binding.unwrap_or(get_ident!(self, "it"));
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, ctx.with_no_expected_type())?;
        let iterable_type = self.exprs.get(iterable_expr).get_type();
        let iterable_span = self.exprs.get(iterable_expr).get_span();
        let body_span = for_expr.body_block.span;

        let (target_is_iterator, item_type) = match self.expect_ability_implementation(
            iterable_type,
            ITERABLE_ABILITY_ID,
            iterable_span,
        ) {
            Err(_not_iterable) => {
                match self.expect_ability_implementation(
                    iterable_type,
                    ITERATOR_ABILITY_ID,
                    iterable_span,
                ) {
                    Err(_not_iterator) => {
                        return failf!(
                            iterable_span,
                            "`for` loop target must be Iterable or an Iterator"
                        )
                    }
                    Ok(iterator_impl) => (
                        true,
                        self.get_ability_impl(iterator_impl.full_impl_id).impl_arguments[0].type_id,
                    ),
                }
            }
            Ok(iterable_impl) => {
                (false, self.get_ability_impl(iterable_impl.full_impl_id).impl_arguments[0].type_id)
            }
        };

        let is_do_block = for_expr.expr_type == ForExprType::Do;

        // We de-sugar the 'for ... do' expr into a typed while loop, synthesizing
        // a few local variables in order to achieve this.

        let outer_for_expr_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::ForExpr, None, None);

        let zero_expr = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
            value: TypedIntegerValue::U64(0),
            span: for_expr.body_block.span,
        }));
        let index_variable = self.synth_variable_defn(
            get_ident!(self, "itIndex"),
            zero_expr,
            true,
            true,
            false,
            outer_for_expr_scope,
        );
        let iterator_initializer = if target_is_iterator {
            iterable_expr
        } else {
            self.synth_typed_function_call(
                qident!(self, body_span, ["Iterable"], "iterator"),
                &[],
                &[iterable_expr],
                ctx.with_scope(outer_for_expr_scope).with_no_expected_type(),
            )?
        };
        let iterator_variable = self.synth_variable_defn(
            get_ident!(self, "iter"),
            iterator_initializer,
            false,
            false,
            true, //is_referencing
            outer_for_expr_scope,
        );
        let mut loop_block = self.synth_block(outer_for_expr_scope, body_span);
        let loop_scope_id = loop_block.scope_id;
        let expected_block_type = ctx
            .expected_type_id
            .and_then(|t| self.types.get(t).as_list_instance())
            .map(|list_type| list_type.element_type);

        let mut consequent_block = self.synth_block(loop_scope_id, iterable_span);

        let loop_scope_ctx = ctx.with_scope(loop_scope_id).with_no_expected_type();
        let iterator_next_call = self.synth_typed_function_call(
            qident!(self, body_span, ["Iterator"], "next"),
            &[],
            &[iterator_variable.variable_expr],
            loop_scope_ctx,
        )?;
        let next_variable = self.synth_variable_defn_simple(
            get_ident!(self, "next"),
            iterator_next_call,
            loop_scope_id,
        );
        let next_unwrap_call = self.synth_typed_function_call(
            qident!(self, iterable_span, ["Unwrap"], "unwrap"),
            &[],
            &[next_variable.variable_expr],
            ctx.with_scope(consequent_block.scope_id).with_no_expected_type(),
        )?;
        let binding_variable = self.synth_variable_defn_visible(
            binding_ident,
            next_unwrap_call,
            consequent_block.scope_id,
        );
        let body_block = self.eval_block(
            &for_expr.body_block,
            ctx.with_scope(consequent_block.scope_id).with_expected_type(expected_block_type),
            false,
        )?;
        let body_block_result_type = body_block.expr_type;

        let resulting_type = if is_do_block {
            UNIT_TYPE_ID
        } else {
            self.instantiate_generic_type(LIST_TYPE_ID, vec![body_block_result_type])
        };
        let outer_for_expr_ctx = ctx.with_scope(outer_for_expr_scope).with_no_expected_type();
        let yielded_coll_variable = if !is_do_block {
            let iterator_deref = self.synth_dereference(iterator_variable.variable_expr);
            let size_hint_call = self.synth_typed_function_call(
                qident!(self, body_span, ["Iterator"], "sizeHint"),
                &[],
                &[iterator_deref],
                outer_for_expr_ctx,
            )?;
            let size_hint_ret_type = self.exprs.get(size_hint_call).get_type();
            let size_hint_lower_bound = self.exprs.add(TypedExpr::StructFieldAccess(FieldAccess {
                struct_type: size_hint_ret_type,
                base: size_hint_call,
                target_field: get_ident!(self, "atLeast"),
                target_field_index: 0,
                result_type: U64_TYPE_ID,
                is_referencing: false,
                span: iterable_span,
            }));
            let synth_function_call = self.synth_typed_function_call(
                qident!(self, body_span, ["List"], "withCapacity"),
                &[body_block_result_type],
                &[size_hint_lower_bound],
                outer_for_expr_ctx,
            )?;
            Some(self.synth_variable_defn(
                get_ident!(self, "yieldedColl"),
                synth_function_call,
                false,
                false,
                true,
                outer_for_expr_scope,
            ))
        } else {
            None
        };

        self.push_block_stmt_id(&mut loop_block, next_variable.defn_stmt); // let next = iter.next();

        let user_body_block_id = self.exprs.add(TypedExpr::Block(body_block));
        let user_block_variable = self.synth_variable_defn_simple(
            get_ident!(self, "block_expr_val"),
            user_body_block_id,
            consequent_block.scope_id,
        );

        consequent_block.statements.push(binding_variable.defn_stmt);
        consequent_block.statements.push(user_block_variable.defn_stmt);
        // Push element to yielded list
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            let list_push_call = self.synth_typed_function_call(
                qident!(self, body_span, ["List"], "push"),
                &[body_block_result_type],
                &[yielded_coll_variable.variable_expr, user_block_variable.variable_expr],
                outer_for_expr_ctx,
            )?;
            self.add_expr_id_to_block(&mut consequent_block, list_push_call);
        }

        let next_is_some_call = self.synth_typed_function_call(
            qident!(self, body_span, ["Opt"], "isSome"),
            &[item_type],
            &[next_variable.variable_expr],
            loop_scope_ctx,
        )?;
        let unit_break = self.exprs.add(TypedExpr::Unit(body_span));
        let break_expr = self.add_expr_stmt(TypedExpr::Break(TypedBreak {
            value: unit_break,
            loop_scope: loop_scope_id,
            loop_type: LoopType::Loop,
            span: body_span,
        }));
        let mut break_block = self.synth_block(loop_scope_id, body_span);
        self.push_block_stmt_id(&mut break_block, break_expr);
        let consequent_block_id = self.exprs.add(TypedExpr::Block(consequent_block));
        let break_block_id = self.exprs.add(TypedExpr::Block(break_block));
        self.add_expr_to_block(
            &mut loop_block,
            TypedExpr::If(TypedIf {
                condition: next_is_some_call,
                consequent: consequent_block_id,
                alternate: break_block_id,
                ty: UNIT_TYPE_ID,
                span: body_span,
            }),
        );

        // Append the index increment to the body block
        let one_expr = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
            value: TypedIntegerValue::U64(1),
            span: iterable_span,
        }));
        let add_operation = self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
            kind: BinaryOpKind::Add,
            ty: U64_TYPE_ID,
            lhs: index_variable.variable_expr,
            rhs: one_expr,
            span: iterable_span,
        }));
        let index_increment_statement = TypedStmt::Assignment(AssignmentStmt {
            destination: index_variable.variable_expr,
            value: add_operation,
            span: iterable_span,
            kind: AssignmentKind::Value,
        });
        self.push_block_stmt(&mut loop_block, index_increment_statement);

        let loop_expr = TypedExpr::LoopExpr(LoopExpr {
            body: Box::new(loop_block),
            break_type: UNIT_TYPE_ID,
            span: for_expr.span,
        });

        let mut for_expr_initial_statements = Vec::with_capacity(4);
        for_expr_initial_statements.push(index_variable.defn_stmt);
        for_expr_initial_statements.push(iterator_variable.defn_stmt);
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            for_expr_initial_statements.push(yielded_coll_variable.defn_stmt);
        }
        let mut for_expr_block = TypedBlock {
            expr_type: resulting_type,
            scope_id: outer_for_expr_scope,
            statements: for_expr_initial_statements,
            span: for_expr.body_block.span,
        };

        self.add_expr_to_block(&mut for_expr_block, loop_expr);
        if let Some(yielded_coll_variable) = yielded_coll_variable {
            let yield_expr = self.synth_dereference(yielded_coll_variable.variable_expr);
            self.add_expr_id_to_block(&mut for_expr_block, yield_expr);
        }

        let final_expr = self.exprs.add(TypedExpr::Block(for_expr_block));
        Ok(final_expr)
    }

    fn expect_ability_implementation(
        &mut self,
        type_id: TypeId,
        ability_id: AbilityId,
        span_for_error: SpanId,
    ) -> TyperResult<AbilityImplHandle> {
        self.find_ability_impl_for_type(type_id, ability_id, span_for_error).ok_or_else(|| {
            errf!(
                span_for_error,
                "Missing ability '{}' for '{}'. It implements the following abilities:\n{}",
                self.name_of(self.get_ability(ability_id).name),
                self.type_id_to_string(type_id),
                &self
                    .ability_impl_table
                    .get(&type_id)
                    .unwrap_or(&vec![])
                    .iter()
                    .map(|h| self.ability_signature_to_string(
                        h.ability_id,
                        &self.get_ability_impl(h.full_impl_id).impl_arguments
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })
    }

    // "if" in k1 can do pattern matching, on multiple targets, chained with arbitrary boolean
    // expressions, so this is not a simple one.
    // if x is .Some(v) and y is .Some("bar") and foo == 3
    fn eval_if_expr(&mut self, if_expr: &IfExpr, ctx: EvalExprContext) -> TyperResult<TypedExprId> {
        let match_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);

        let MatchingCondition { combined_condition, setup_statements, binding_statements, .. } =
            self.eval_matching_condition(
                if_expr.cond,
                ctx.with_scope(match_scope_id).with_no_expected_type(),
            )?;

        let consequent = self.eval_expr(if_expr.cons, ctx.with_scope(match_scope_id))?;
        let consequent_type = self.exprs.get(consequent).get_type();

        let cons_never = consequent_type == NEVER_TYPE_ID;

        // if without else:
        // If there is no alternate, we coerce the consequent to return Unit, so both
        // branches have a matching type, making codegen simpler
        // However, if the consequent is a never type, we don't need to do this, in
        // fact we can't because then we'd have an expression following a never expression
        let consequent = if if_expr.alt.is_none() && !cons_never && consequent_type != UNIT_TYPE_ID
        {
            self.synth_discard_call(consequent, ctx.with_no_expected_type())?
        } else {
            consequent
        };
        let alternate = if let Some(parsed_alt) = if_expr.alt {
            let type_hint = if cons_never { ctx.expected_type_id } else { Some(consequent_type) };
            self.eval_expr(parsed_alt, ctx.with_expected_type(type_hint))?
        } else {
            self.exprs.add(TypedExpr::Unit(if_expr.span))
        };
        let alternate_type = self.exprs.get(alternate).get_type();
        let alternate_span = self.exprs.get(alternate).get_span();

        let cons_never = consequent_type == NEVER_TYPE_ID; // By now, consequent could be 'unit'
        let alt_never = alternate_type == NEVER_TYPE_ID;
        let no_never = !cons_never && !alt_never;

        let overall_type = if no_never {
            if let Err(msg) = self.check_types(consequent_type, alternate_type, ctx.scope_id) {
                return failf!(
                    alternate_span,
                    "else branch type did not match then branch type: {}",
                    msg,
                );
            };
            consequent_type
        } else {
            if cons_never {
                alternate_type
            } else {
                consequent_type
            }
        };

        let cons_arm = TypedMatchArm {
            patterns: smallvec![],
            setup_statements,
            pattern_condition: combined_condition,
            pattern_bindings: binding_statements,
            guard_condition: None,
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            patterns: smallvec![],
            setup_statements: smallvec![],
            pattern_condition: self.exprs.add(TypedExpr::Bool(true, alternate_span)),
            pattern_bindings: smallvec![],
            guard_condition: None,
            consequent_expr: alternate,
        };
        Ok(self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: vec![],
            result_type: overall_type,
            arms: vec![cons_arm, alt_arm],
            span: if_expr.span,
        })))
    }

    fn eval_matching_condition(
        &mut self,
        condition: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<MatchingCondition> {
        let mut conditions: SmallVec<[TypedExprId; 4]> = smallvec![];
        let mut setup_statements: SmallVec<[_; 4]> = smallvec![];
        let mut binding_statements: SmallVec<[_; 4]> = smallvec![];
        let mut all_patterns: SmallVec<[TypedPattern; 4]> = smallvec![];
        let mut allow_bindings: bool = true;
        self.handle_matching_if_rec(
            condition,
            &mut allow_bindings,
            &mut all_patterns,
            &mut conditions,
            &mut setup_statements,
            &mut binding_statements,
            ctx,
        )?;

        // TODO: allocations
        let mut all_bindings = Vec::new();
        for pattern in all_patterns.iter() {
            pattern.all_bindings_rec(&mut all_bindings);
        }
        if allow_bindings {
            // Bindings are allowed, fail if there are any duplicates
            all_bindings.sort_by_key(|p| p.name);
            let dupe_binding =
                all_bindings.windows(2).find(|window| window[0].name == window[1].name);
            if let Some(dupe_binding) = dupe_binding {
                return failf!(
                    dupe_binding[1].span,
                    "Duplicate binding of name '{}' within same matching if; normally we like shadowing but this is probably never good.",
                    self.name_of(dupe_binding[1].name)
                );
            }
        } else {
            // Bindings are disallowed due to the structure of the expressions
            // but there is at least one binding
            if let Some(b) = all_bindings.first() {
                return failf!(
                    b.span,
                    "Cannot create bindings unless all patterns are connected by 'and'"
                );
            }
        }

        let patterns_combined_with_and = conditions
            .into_iter()
            .reduce(|a, b| self.synth_binary_bool_op(BinaryOpKind::And, a, b))
            .unwrap();
        Ok(MatchingCondition {
            all_patterns,
            combined_condition: patterns_combined_with_and,
            setup_statements,
            binding_statements,
            binding_eligible: allow_bindings,
        })
    }

    fn handle_matching_if_rec(
        &mut self,
        parsed_expr_id: ParsedExpressionId,
        allow_bindings: &mut bool,
        all_patterns: &mut SmallVec<[TypedPattern; 4]>,
        conditions: &mut SmallVec<[TypedExprId; 4]>,
        setup_statements: &mut SmallVec<[TypedStmtId; 4]>,
        binding_statements: &mut SmallVec<[TypedStmtId; 4]>,
        ctx: EvalExprContext,
    ) -> TyperResult<()> {
        debug!("hmirec {allow_bindings}: {}", self.ast.expr_id_to_string(parsed_expr_id));
        match self.ast.exprs.get(parsed_expr_id) {
            ParsedExpression::Is(is_expr) => {
                let target_expr = is_expr.target_expression;
                let pattern = is_expr.pattern;
                let target = self.eval_expr(target_expr, ctx)?;
                let target_type = self.exprs.get(target).get_type();
                let target_var = self.synth_variable_defn_simple(
                    get_ident!(self, "if_target"),
                    target,
                    ctx.scope_id,
                );
                let pattern =
                    self.compile_pattern(pattern, target_type, ctx.scope_id, *allow_bindings)?;
                setup_statements.push(target_var.defn_stmt);
                let condition = self.compile_pattern_in_scope(
                    &pattern,
                    target_var.variable_expr,
                    setup_statements,
                    binding_statements,
                    false,
                    ctx.scope_id,
                )?;
                all_patterns.push(pattern);

                conditions.push(condition);
                Ok(())
            }
            ParsedExpression::BinaryOp(binary_op) if binary_op.op_kind == BinaryOpKind::And => {
                let rhs = binary_op.rhs;
                self.handle_matching_if_rec(
                    binary_op.lhs,
                    allow_bindings,
                    all_patterns,
                    conditions,
                    setup_statements,
                    binding_statements,
                    ctx,
                )?;
                self.handle_matching_if_rec(
                    rhs,
                    allow_bindings,
                    all_patterns,
                    conditions,
                    setup_statements,
                    binding_statements,
                    ctx,
                )?;
                Ok(())
            }
            other => {
                let is_or_binop =
                    matches!(other, ParsedExpression::BinaryOp(b) if b.op_kind == BinaryOpKind::Or);
                // At the top-level of the 'if', if there are any 'or's, we cannot allow patterns
                if is_or_binop {
                    *allow_bindings = false;
                };
                let span = other.get_span();
                let condition =
                    self.eval_expr(parsed_expr_id, ctx.with_expected_type(Some(BOOL_TYPE_ID)))?;
                let condition_type = self.exprs.get(condition).get_type();
                if let Err(msg) = self.check_types(BOOL_TYPE_ID, condition_type, ctx.scope_id) {
                    return failf!(span, "Expected boolean condition: {msg}");
                };
                conditions.push(condition);
                Ok(())
            }
        }
    }

    fn eval_binary_op(
        &mut self,
        binary_op_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        fn is_scalar_for_equals(type_id: TypeId) -> bool {
            match type_id {
                UNIT_TYPE_ID | CHAR_TYPE_ID | U8_TYPE_ID | U16_TYPE_ID | U32_TYPE_ID
                | U64_TYPE_ID | I8_TYPE_ID | I16_TYPE_ID | I32_TYPE_ID | I64_TYPE_ID
                | BOOL_TYPE_ID => true,
                F32_TYPE_ID | F64_TYPE_ID => true,
                _other => false,
            }
        }
        let ParsedExpression::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };
        // Special cases: Equality, OptionalElse, and Pipe
        match binary_op.op_kind {
            BinaryOpKind::Pipe => {
                return self.eval_pipe_expr(binary_op.lhs, binary_op.rhs, ctx, binary_op.span);
            }
            BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                let lhs = self.eval_expr(binary_op.lhs, ctx.with_no_expected_type())?;
                let lhs_type = self.exprs.get(lhs).get_type();
                if !is_scalar_for_equals(lhs_type) {
                    return self.eval_equality_expr(binary_op_id, ctx.with_no_expected_type());
                }
            }
            BinaryOpKind::OptionalElse => {
                return self.eval_optional_else(binary_op.lhs, binary_op.rhs, ctx, binary_op.span)
            }
            _ => {}
        };

        // Rest of the binary ops

        // FIXME: We could figure out better hinting here so that the following would compile to u64 on the rhs:
        // assert(sizeOf[Text]() == 16 + 32);
        //                          ^^^^^^^
        //
        // If everything was just a function in disguise, we could leverage all of our inference
        // and keep everything in one place
        //
        // Well yes Koleman but these binops would be very polymorphic functions requiring generic abilities
        // Which we have!
        let lhs = self.eval_expr(binary_op.lhs, ctx.with_no_expected_type())?;
        let kind = binary_op.op_kind;
        let lhs_type = self.exprs.get(lhs).get_type();
        let result_type = match self.types.get(lhs_type) {
            Type::Never(_) => Ok(NEVER_TYPE_ID),
            Type::Float(_) | Type::Integer(_) => match kind {
                BinaryOpKind::Add => Ok(lhs_type),
                BinaryOpKind::Subtract => Ok(lhs_type),
                BinaryOpKind::Multiply => Ok(lhs_type),
                BinaryOpKind::Divide => Ok(lhs_type),
                BinaryOpKind::Rem => Ok(lhs_type),
                BinaryOpKind::Less => Ok(BOOL_TYPE_ID),
                BinaryOpKind::LessEqual => Ok(BOOL_TYPE_ID),
                BinaryOpKind::Greater => Ok(BOOL_TYPE_ID),
                BinaryOpKind::GreaterEqual => Ok(BOOL_TYPE_ID),
                BinaryOpKind::And => failf!(binary_op.span, "Invalid left-hand side for and"),
                BinaryOpKind::Or => failf!(binary_op.span, "Invalid left-hand side for or"),
                BinaryOpKind::Equals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::NotEquals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::OptionalElse | BinaryOpKind::Pipe => unreachable!(),
            },
            Type::Bool(_) => match kind {
                BinaryOpKind::Add
                | BinaryOpKind::Subtract
                | BinaryOpKind::Multiply
                | BinaryOpKind::Divide
                | BinaryOpKind::Rem
                | BinaryOpKind::Less
                | BinaryOpKind::LessEqual
                | BinaryOpKind::Greater
                | BinaryOpKind::GreaterEqual => {
                    failf!(binary_op.span, "Invalid operation on bool: {}", kind)
                }
                BinaryOpKind::And => Ok(BOOL_TYPE_ID),
                BinaryOpKind::Or => Ok(BOOL_TYPE_ID),
                BinaryOpKind::Equals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::NotEquals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::OptionalElse | BinaryOpKind::Pipe => unreachable!(),
            },
            Type::Char(_) => match kind {
                BinaryOpKind::Add
                | BinaryOpKind::Subtract
                | BinaryOpKind::Multiply
                | BinaryOpKind::Divide
                | BinaryOpKind::Rem
                | BinaryOpKind::Less
                | BinaryOpKind::LessEqual
                | BinaryOpKind::Greater
                | BinaryOpKind::GreaterEqual
                | BinaryOpKind::And
                | BinaryOpKind::Or => failf!(binary_op.span, "Invalid operation on char: {}", kind),
                BinaryOpKind::Equals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::NotEquals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::OptionalElse | BinaryOpKind::Pipe => unreachable!(),
            },
            Type::Unit(_) => match kind {
                BinaryOpKind::Equals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::NotEquals => Ok(BOOL_TYPE_ID),
                _ => failf!(binary_op.span, "Invalid operation on unit: {}", kind),
            },
            other => {
                failf!(
                    binary_op.span,
                    "Invalid left-hand side of binary operation {}: {}",
                    kind,
                    self.type_to_string(other, false)
                )
            }
        }?;

        debug_assert!(kind.is_symmetric_binop());
        // At this point I think all operations are symmetric but we'll leave this here
        // to signal that invariant and in case things change
        let rhs = self.eval_expr(binary_op.rhs, ctx.with_expected_type(Some(lhs_type)))?;
        let rhs_type = self.exprs.get(rhs).get_type();

        let result_type = match rhs_type {
            NEVER_TYPE_ID => NEVER_TYPE_ID,
            _other => result_type,
        };

        // We already confirmed that the LHS is valid for this operation, and
        // if the op is symmetric, we just have to check the RHS matches
        if let Err(msg) = self.check_types(lhs_type, rhs_type, ctx.scope_id) {
            let rhs_span = self.exprs.get(rhs).get_span();
            return failf!(rhs_span, "operand types did not match: {msg}");
        }

        let expr =
            TypedExpr::BinaryOp(BinaryOp { kind, ty: result_type, lhs, rhs, span: binary_op.span });
        Ok(self.exprs.add(expr))
    }

    fn eval_optional_else(
        &mut self,
        lhs: ParsedExpressionId,
        rhs: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        // LHS must implement Unwrap and RHS must be its contained type
        let lhs = self.eval_expr(lhs, ctx.with_no_expected_type())?;
        let lhs_type = self.exprs.get(lhs).get_type();
        let unwrap_impl =
            self.expect_ability_implementation(lhs_type, UNWRAP_ABILITY_ID, span).map_err(|e| {
                errf!(
                    span,
                    "'?' operator can only be used on a type that implements `Unwrap`. {}",
                    e.message,
                )
            })?;
        let unwrap_impl = self.get_ability_impl(unwrap_impl.full_impl_id);
        let output_type = unwrap_impl.impl_arguments[0].type_id;

        let rhs = self.eval_expr(rhs, ctx.with_expected_type(Some(output_type)))?;
        let rhs_type = self.exprs.get(rhs).get_type();
        if let Err(msg) = self.check_types(output_type, rhs_type, ctx.scope_id) {
            return failf!(span, "RHS value incompatible with `Unwrap` output of LHS: {}", msg);
        }
        let mut coalesce_block = self.synth_block(ctx.scope_id, span);
        let lhs_variable = self.synth_variable_defn_simple(
            get_ident!(self, "optelse_lhs"),
            lhs,
            coalesce_block.scope_id,
        );
        let coalesce_ctx = ctx.with_scope(coalesce_block.scope_id).with_no_expected_type();
        let lhs_has_value = self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "hasValue"),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
        )?;
        let lhs_get_expr = self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "unwrap"),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
        )?;

        let if_else = self.exprs.add(TypedExpr::If(TypedIf {
            condition: lhs_has_value,
            consequent: lhs_get_expr,
            alternate: rhs,
            ty: output_type,
            span,
        }));
        self.push_block_stmt_id(&mut coalesce_block, lhs_variable.defn_stmt);
        self.add_expr_id_to_block(&mut coalesce_block, if_else);
        Ok(self.exprs.add(TypedExpr::Block(coalesce_block)))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op_id: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpression::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };

        let lhs = self.eval_expr(binary_op.lhs, ctx.with_no_expected_type())?;
        let lhs_type = self.exprs.get(lhs).get_type();
        let equality_result = match self.types.get(lhs_type) {
            Type::TypeParameter(_type_var) => {
                let rhs = self.eval_expr(binary_op.rhs, ctx.with_expected_type(Some(lhs_type)))?;
                let equals = self.synth_equals_binop(lhs, rhs, binary_op.span);
                Ok(equals)
            }
            other_lhs_type => {
                if other_lhs_type.is_scalar_int_value() {
                    panic!("Scalar ints shouldnt be passed to eval_equality_expr")
                }
                let rhs = self.eval_expr(binary_op.rhs, ctx.with_expected_type(Some(lhs_type)))?;
                let rhs_type = self.exprs.get(rhs).get_type();
                if rhs_type != lhs_type {
                    failf!(
                        binary_op.span,
                        "Right hand side type '{}' did not match {}",
                        self.type_id_to_string(rhs_type),
                        self.type_id_to_string(lhs_type)
                    )
                } else {
                    let call_expr = self.synth_equals_call(lhs, rhs, binary_op.span)?;
                    Ok(call_expr)
                }
            }
        }?;
        let final_result = match binary_op.op_kind {
            BinaryOpKind::Equals => equality_result,
            BinaryOpKind::NotEquals => self.synth_typed_function_call(
                qident!(self, binary_op.span, ["bool"], "negated"),
                &[],
                &[equality_result],
                ctx,
            )?,
            _ => unreachable!(),
        };
        Ok(final_result)
    }

    fn eval_pipe_expr(
        &mut self,
        lhs: ParsedExpressionId,
        rhs: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let new_fn_call = match self.ast.exprs.get(rhs) {
            ParsedExpression::Variable(var) => {
                let args = vec![parse::FnCallArg { name: None, value: lhs }];
                FnCall {
                    name: var.name.clone(),
                    type_args: vec![],
                    args,
                    explicit_context_args: vec![],
                    span,
                    is_method: false,
                    id: ParsedExpressionId::PENDING,
                }
            }
            ParsedExpression::FnCall(fn_call) => {
                let mut args = Vec::with_capacity(fn_call.args.len() + 1);
                args.push(parse::FnCallArg { name: None, value: lhs });
                args.extend(fn_call.args.clone());
                FnCall {
                    name: fn_call.name.clone(),
                    type_args: fn_call.type_args.clone(),
                    args,
                    explicit_context_args: vec![],
                    span,
                    is_method: false,
                    id: ParsedExpressionId::PENDING,
                }
            }
            _ => {
                return failf!(
                    self.ast.exprs.get_span(rhs),
                    "rhs of pipe must be function call or function name",
                )
            }
        };
        let new_fn_call_id = self.ast.exprs.add_expression(ParsedExpression::FnCall(new_fn_call));
        let new_fn_call_clone = self.ast.exprs.get(new_fn_call_id).expect_fn_call().clone();
        self.eval_function_call(&new_fn_call_clone, None, ctx)
    }

    fn synth_equals_call(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let lhs_type = self.exprs.get(lhs).get_type();
        let implementation =
            self.expect_ability_implementation(lhs_type, EQUALS_ABILITY_ID, span)?;
        let implementation = self.get_ability_impl(implementation.full_impl_id);
        let ability = self.get_ability(EQUALS_ABILITY_ID);
        let equals_index = ability.find_function_by_name(get_ident!(self, "equals")).unwrap().0;
        let equals_implementation_function_id = implementation.function_at_index(equals_index);
        let call_expr = self.exprs.add(TypedExpr::Call(Call {
            callee: Callee::make_static(equals_implementation_function_id),
            args: vec![lhs, rhs],
            type_args: smallvec![],
            return_type: BOOL_TYPE_ID,
            span,
        }));
        Ok(call_expr)
    }

    /// Can 'shortcircuit' with Left if the function call to resolve
    /// is actually a builtin
    fn resolve_parsed_function_call(
        &mut self,
        fn_call: &FnCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        let call_span = fn_call.span;
        if let Some(builtin_result) = self.handle_builtin_function_call_lookalikes(fn_call, ctx)? {
            return Ok(Either::Left(builtin_result));
        }

        let first_arg_expr: Option<MaybeTypedExpr> = match known_args.as_ref() {
            Some((_, args)) if !args.is_empty() => {
                Some(MaybeTypedExpr::Typed(*args.first().unwrap()))
            }
            _ => match fn_call.args.first() {
                None => None,
                Some(first) => Some(MaybeTypedExpr::Parsed(first.value)),
            },
        };

        // Two resolution paths:
        // 1. "method" call (aka known first arg type so we can check for ability impls and methods)
        // 2. "function" call, no abilities or methods to check, pure scoping-based resolution

        match fn_call.is_method {
            true => self.resolve_parsed_function_call_method(
                first_arg_expr.unwrap(),
                fn_call,
                known_args,
                ctx,
            ),
            false => {
                if let Some(function_id) = self.scopes.find_function_namespaced(
                    ctx.scope_id,
                    &fn_call.name,
                    &self.namespaces,
                    &self.ast.identifiers,
                )? {
                    if let Some(function_ability_id) =
                        self.get_function(function_id).kind.ability_id()
                    {
                        let function_ability_index = self
                            .get_ability(function_ability_id)
                            .find_function_by_name(fn_call.name.name)
                            .unwrap()
                            .0;
                        let function_id = self.resolve_ability_call(
                            function_id,
                            function_ability_index,
                            function_ability_id,
                            fn_call,
                            known_args,
                            ctx,
                        )?;
                        Ok(Either::Right(Callee::make_static(function_id)))
                    } else {
                        Ok(Either::Right(Callee::make_static(function_id)))
                    }
                } else {
                    // Function lookup failed, now we deal with lower priority 'callable' things
                    // Such as lambda objects or function pointers
                    let fn_not_found = || {
                        failf!(
                            call_span,
                            "Function not found: '{}'",
                            self.name_of(fn_call.name.name)
                        )
                    };
                    if !fn_call.name.namespaces.is_empty() {
                        return fn_not_found();
                    }
                    if let Some((variable_id, _scope_id)) =
                        self.scopes.find_variable(ctx.scope_id, fn_call.name.name)
                    {
                        let function_variable = self.variables.get(variable_id);
                        match self.types.get(function_variable.type_id) {
                            Type::Lambda(lambda_type) => Ok(Either::Right(Callee::StaticLambda {
                                function_id: lambda_type.body_function_id,
                                lambda_type_id: function_variable.type_id,
                            })),
                            Type::LambdaObject(_lambda_object) => {
                                Ok(Either::Right(Callee::DynamicLambda(self.exprs.add(
                                    TypedExpr::Variable(VariableExpr {
                                        variable_id,
                                        type_id: function_variable.type_id,
                                        span: fn_call.name.span,
                                    }),
                                ))))
                            }
                            Type::TypeParameter(tp) => {
                                if let Some(function_type) = tp.function_type {
                                    let callee = Callee::Abstract { function_type, variable_id };
                                    Ok(Either::Right(callee))
                                } else {
                                    fn_not_found()
                                }
                            }
                            Type::Reference(function_reference) => {
                                if self
                                    .types
                                    .get(function_reference.inner_type)
                                    .as_function()
                                    .is_some()
                                {
                                    Ok(Either::Right(Callee::DynamicFunction(self.exprs.add(
                                        TypedExpr::Variable(VariableExpr {
                                            variable_id,
                                            type_id: function_variable.type_id,
                                            span: fn_call.name.span,
                                        }),
                                    ))))
                                } else {
                                    fn_not_found()
                                }
                            }
                            _ => fn_not_found(),
                        }
                    } else {
                        fn_not_found()
                    }
                }
            }
        }
    }

    fn get_expected_return_type(&self, scope_id: ScopeId, span: SpanId) -> TyperResult<TypeId> {
        if let Some(enclosing_lambda) = self.scopes.nearest_parent_lambda(scope_id) {
            let Some(expected_return_type) =
                self.scopes.get_lambda_info(enclosing_lambda).expected_return_type
            else {
                return failf!(span, "Closure must have explicit return type, or known return type from context, to use early returns.");
            };
            Ok(expected_return_type)
        } else {
            let Some(enclosing_function) = self.scopes.nearest_parent_function(scope_id) else {
                return failf!(span, "No parent function; cannot return");
            };
            let expected_return_type = self.get_function_type(enclosing_function).return_type;
            Ok(expected_return_type)
        }
    }

    fn eval_return(
        &mut self,
        parsed_expr: ParsedExpressionId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let expected_return_type = self.get_expected_return_type(ctx.scope_id, span)?;
        let return_value =
            self.eval_expr(parsed_expr, ctx.with_expected_type(Some(expected_return_type)))?;
        let return_value_type = self.exprs.get(return_value).get_type();
        if let Err(msg) = self.check_types(expected_return_type, return_value_type, ctx.scope_id) {
            return failf!(span, "Returned wrong type: {msg}");
        }
        Ok(self.exprs.add(TypedExpr::Return(TypedReturn { value: return_value, span })))
    }

    fn handle_builtin_function_call_lookalikes(
        &mut self,
        fn_call: &FnCall,
        ctx: EvalExprContext,
    ) -> TyperResult<Option<TypedExprId>> {
        let call_span = fn_call.span;
        let calling_scope = ctx.scope_id;
        match self.name_of(fn_call.name.name) {
            "return" => {
                if fn_call.args.len() != 1 {
                    return failf!(fn_call.span, "return(...) must have exactly one argument",);
                }
                Ok(Some(self.eval_return(fn_call.args[0].value, ctx, call_span)?))
            }
            "break" => {
                if fn_call.args.len() > 1 {
                    return failf!(call_span, "break(...) must have 0 or 1 argument");
                }
                // Determine based on loop type if break with value is allowed
                let Some((enclosing_loop_scope_id, loop_type)) =
                    self.scopes.nearest_parent_loop(calling_scope)
                else {
                    return failf!(call_span, "break(...) outside of loop");
                };
                let expected_break_type: Option<TypeId> =
                    self.scopes.get_loop_info(enclosing_loop_scope_id).unwrap().break_type;

                let break_value = match fn_call.args.first() {
                    None => self.exprs.add(TypedExpr::Unit(call_span)),
                    Some(fn_call_arg) => {
                        // ALTERNATIVE: Allow break with value from `while` loops but require the type to implement the `Default` trait
                        match loop_type {
                            LoopType::Loop => self.eval_expr(
                                fn_call_arg.value,
                                ctx.with_expected_type(expected_break_type),
                            )?,
                            LoopType::While => {
                                return failf!(
                                    call_span,
                                    "break with value is only allowed in `loop` loops, because loop body may not ever be executed"
                                )
                            }
                        }
                    }
                };
                let actual_break_type = self.exprs.get(break_value).get_type();

                // If we have an expected type already,
                // - check it
                // - else, set it
                if let Some(expected_break_type) = expected_break_type {
                    if let Err(msg) =
                        self.check_types(expected_break_type, actual_break_type, calling_scope)
                    {
                        return failf!(call_span, "Break with wrong type: {msg}");
                    }
                } else {
                    self.scopes.add_loop_info(
                        enclosing_loop_scope_id,
                        ScopeLoopInfo { break_type: Some(actual_break_type) },
                    )
                }

                Ok(Some(self.exprs.add(TypedExpr::Break(TypedBreak {
                    value: break_value,
                    loop_scope: enclosing_loop_scope_id,
                    loop_type,
                    span: call_span,
                }))))
            }
            "continue" => {
                todo!("implement continue")
            }
            "testCompile" => {
                if fn_call.args.len() != 1 {
                    return failf!(call_span, "testCompile takes one argument");
                }
                let arg = &fn_call.args[0];
                let result = self.eval_expr(arg.value, ctx.with_no_expected_type());
                let expr = match result {
                    Err(typer_error) => {
                        self.synth_optional_some(TypedExpr::Str(typer_error.message, call_span)).0
                    }
                    Ok(_expr) => self.synth_optional_none(STRING_TYPE_ID, call_span),
                };
                Ok(Some(expr))
            }
            _ => Ok(None),
        }
    }

    fn resolve_parsed_function_call_method(
        &mut self,
        base_expr: MaybeTypedExpr,
        fn_call: &FnCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        debug_assert!(fn_call.name.namespaces.is_empty());
        let fn_name = fn_call.name.name;
        let second_arg = fn_call.args.get(1).cloned();

        if let Some(base_arg) = fn_call.args.first() {
            let type_args = fn_call.type_args.clone();
            if let Some(enum_constr) = self.handle_enum_constructor(
                base_arg.value,
                fn_name,
                second_arg.map(|param| param.value),
                &type_args,
                ctx,
                fn_call.span,
            )? {
                return Ok(Either::Left(enum_constr));
            }
        }

        let base_expr = match base_expr {
            MaybeTypedExpr::Typed(expr) => expr,
            MaybeTypedExpr::Parsed(parsed_expr_id) => {
                self.eval_expr(parsed_expr_id, ctx.with_no_expected_type())?
            }
        };

        // Handle the special case of the synthesized enum 'as{Variant}' methods
        if let Some(enum_as_result) = self.handle_enum_as(base_expr, fn_call)? {
            return Ok(Either::Left(enum_as_result));
        }

        let call_span = fn_call.span;

        let base_expr_type = self.exprs.get(base_expr).get_type();
        let base_for_method_derefed = self.types.get_type_id_dereferenced(base_expr_type);
        if let Some(companion_ns) = self
            .types
            .get_type_defn_info(base_for_method_derefed)
            .and_then(|d| d.companion_namespace)
        {
            let companion_scope = self.get_namespace_scope(companion_ns);
            if let Some(method_id) = companion_scope.find_function(fn_name) {
                return Ok(Either::Right(Callee::make_static(method_id)));
            }
        };

        let method_not_found = || {
            failf!(
                call_span,
                "Method '{}' does not exist on type: '{}'",
                self.name_of(fn_call.name.name),
                self.type_id_to_string(base_expr_type),
            )
        };

        let abilities_in_scope = self.scopes.find_abilities_in_scope(ctx.scope_id);
        debug!(
            "abilities_in_scope: {:?}",
            abilities_in_scope
                .iter()
                .map(|a| self.name_of(self.get_ability(*a).name))
                .collect::<Vec<_>>()
        );
        let Some((ability_function_index, ability_function_ref)) = abilities_in_scope
            .iter()
            .find_map(|ability_id| self.get_ability(*ability_id).find_function_by_name(fn_name))
        else {
            return method_not_found();
        };
        let Some(ability_id) =
            self.get_function(ability_function_ref.function_id).kind.ability_id()
        else {
            return method_not_found();
        };
        let ability_impl_fn_id = self.resolve_ability_call(
            ability_function_ref.function_id,
            ability_function_index,
            ability_id,
            fn_call,
            known_args,
            ctx,
        )?;
        Ok(Either::Right(Callee::make_static(ability_impl_fn_id)))
    }

    fn resolve_ability_call(
        &mut self,
        function_id: FunctionId,
        function_ability_index: usize,
        ability_id: AbilityId,
        fn_call: &FnCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<FunctionId> {
        let call_span = fn_call.span;
        let ability_fn_signature = self.get_function_type(function_id);
        let ability_fn_return_type = ability_fn_signature.return_type;
        let ability_fn_params = ability_fn_signature.params.clone();
        let ability_params = self.get_ability(ability_id).parameters.clone();
        let ability_self_type_id = self.get_ability(ability_id).self_type_id;

        let passed_len = known_args.map(|ka| ka.1.len()).unwrap_or(fn_call.args.len());
        if passed_len != ability_fn_signature.params.len() {
            return failf!(call_span, "Mismatching arg count when trying to resolve ability call to {} (this probably doesn't handle context params properly)", self.name_of(fn_call.name.name));
        }

        // First, we need to solve for 'Self' and all of the ability's other type parameters
        // using the expressions and (context + return type) of the called function
        //
        // Future TODO: Make sure we handle context params in ability functions correctly,
        // Probably by: skipping them if not passed explicitly, and utilizing them if passed explicitly
        let type_params: SmallVec<[SimpleNamedType; 6]> = {
            let mut type_params = SmallVec::with_capacity(ability_params.len() + 1);
            type_params.push(SimpleNamedType {
                name: get_ident!(self, "Self"),
                type_id: ability_self_type_id,
            });
            type_params.extend(
                ability_params
                    .iter()
                    .filter(|p| !p.is_impl_param)
                    .map(|p| SimpleNamedType { name: p.name, type_id: p.type_variable_id }),
            );
            type_params
        };

        let passed_args: &mut dyn Iterator<Item = MaybeTypedExpr> = match known_args {
            Some(ka) => &mut ka.1.iter().map(|t| MaybeTypedExpr::Typed(*t)),
            None => &mut fn_call.args.iter().map(|arg| MaybeTypedExpr::Parsed(arg.value)),
        };
        let mut args_and_params = Vec::with_capacity(passed_len);
        if let Some(expected_type) = ctx.expected_type_id {
            args_and_params.push((
                TypeOrParsedExpr::TypeId(expected_type),
                ability_fn_return_type,
                false,
            ));
        }
        for (arg, param) in passed_args.zip(ability_fn_params.iter()) {
            let arg_and_param = match arg {
                MaybeTypedExpr::Typed(expr) => {
                    let type_id = self.exprs.get(expr).get_type();
                    (TypeOrParsedExpr::TypeId(type_id), param.type_id, false)
                }
                MaybeTypedExpr::Parsed(parsed_expr) => {
                    (TypeOrParsedExpr::ParsedExpr(parsed_expr), param.type_id, false)
                }
            };
            args_and_params.push(arg_and_param);
        }

        let solved_params =
            self.infer_types(&type_params, &args_and_params, fn_call.span, ctx.scope_id)?;

        let (solved_self, solved_rest) = solved_params.split_at(1);
        let solved_self = solved_self.first().unwrap();

        let does_not_implement = |m: &TypedModule| {
            failf!(
                call_span,
                "Call to {} with type Self = {} does not work, since it does not implement ability {}",
                m.name_of(m.get_function(function_id).name),
                m.type_id_to_string(solved_self.type_id),
                m.ability_signature_to_string(ability_id, &[]),
            )
        };

        // We only solve for the ability-side params, so we pass a flag to check_ability_arguments
        // indicating that we aren't providing the impl params
        let skip_impl_check = true;
        self.check_ability_arguments(ability_id, solved_rest, call_span, ctx.scope_id, skip_impl_check).map_err(|e| {
            errf!(
                e.span,
                "I thought I found a matching ability call, but the arguments didn't check out. This is likely a bug {}",
                e.message
            )
        })?;
        let actual_ability_id =
            self.specialize_ability(ability_id, solved_rest.to_vec(), call_span)?;

        // 2) Find impl based on solved Self + Params
        // 2a) Generate auto impl if that's what we find, cache it at low prio
        // 3) Return impl fn id, which can be used for nice type inference since
        //    its got the trait's generics baked in already
        let Some(impl_handle) =
            self.find_ability_impl_for_type(solved_self.type_id, actual_ability_id, call_span)
        else {
            return does_not_implement(self);
        };
        let impl_fn_id = self
            .get_ability_impl(impl_handle.full_impl_id)
            .function_at_index(function_ability_index);
        Ok(impl_fn_id)
    }

    fn handle_enum_as(
        &mut self,
        base_expr: TypedExprId,
        fn_call: &FnCall,
    ) -> TyperResult<Option<TypedExprId>> {
        let (e, is_reference) = match self.get_expr_type(base_expr) {
            Type::Reference(r) => {
                if let Type::Enum(e) = self.types.get(r.inner_type) {
                    (e, true)
                } else {
                    return Ok(None);
                }
            }
            Type::Enum(e) => (e, false),
            _ => return Ok(None),
        };
        let fn_name = self.name_of(fn_call.name.name);
        if fn_name.starts_with("as") && fn_call.type_args.is_empty() && fn_call.args.len() == 1 {
            let span = fn_call.span;
            let Some(variant) = e.variants.iter().find(|v| {
                let mut s = String::with_capacity(16);
                s.push_str("as");
                let name = self.name_of(v.name);
                let name_capitalized = strings::capitalize_first(name);
                s.push_str(&name_capitalized);

                fn_name == s
            }) else {
                let base_expr_type = self.exprs.get(base_expr).get_type();
                return failf!(
                    span,
                    "Method '{}' does not exist on type {}",
                    fn_name,
                    self.type_id_to_string(base_expr_type)
                );
            };
            let variant_type_id = variant.my_type_id;
            let variant_name = variant.name;
            let variant_index = variant.index;
            let resulting_type_id = if is_reference {
                self.types.add_reference_type(variant_type_id)
            } else {
                variant_type_id
            };
            let base_expr_dereferenced =
                if is_reference { self.synth_dereference(base_expr) } else { base_expr };
            let condition = self.exprs.add(TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                target_expr: base_expr_dereferenced,
                variant_name,
                variant_index,
                span,
            }));
            let (consequent, consequent_type_id) =
                self.synth_optional_some(TypedExpr::Cast(TypedCast {
                    cast_type: CastType::KnownNoOp,
                    base_expr,
                    target_type_id: resulting_type_id,
                    span,
                }));
            let alternate = self.synth_optional_none(resulting_type_id, span);

            Ok(Some(self.exprs.add(TypedExpr::If(TypedIf {
                ty: consequent_type_id,
                condition,
                consequent,
                alternate,
                span,
            }))))
        } else {
            Ok(None)
        }
    }

    fn handle_enum_constructor(
        &mut self,
        base_expr: ParsedExpressionId,
        variant_name: Identifier,
        payload_parsed_expr: Option<ParsedExpressionId>,
        type_args: &[NamedTypeArg],
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<Option<TypedExprId>> {
        // We only do this _if_ the name matches a variant of the enum
        // Otherwise we are happy to fall through to a regular method call
        let ParsedExpression::Variable(v) = self.ast.exprs.get(base_expr) else {
            return Ok(None);
        };
        let Some((base_type_in_scope, _)) = self.scopes.find_type_namespaced(
            ctx.scope_id,
            &v.name,
            &self.namespaces,
            &self.ast.identifiers,
        )?
        else {
            return Ok(None);
        };
        let base_type = self.types.get(base_type_in_scope);
        if let Type::Enum(e) = base_type {
            if let Some(_variant) = e.variant_by_name(variant_name) {
                Ok(Some(self.eval_enum_constructor(
                    base_type_in_scope,
                    variant_name,
                    payload_parsed_expr,
                    ctx,
                    span,
                )?))
            } else {
                Ok(None)
            }
        } else if let Type::Generic(g) = base_type {
            let Some(inner_enum) = self.types.get(g.inner).as_enum() else { return Ok(None) };
            let Some(generic_variant) = inner_enum.variant_by_name(variant_name) else {
                return Ok(None);
            };
            let g_params = g.params.clone();
            let g_name = g.type_defn_info.name;

            let payload_if_needed = match generic_variant.payload {
                Some(generic_payload_type_id) => match payload_parsed_expr {
                    None => {
                        return failf!(
                            span,
                            "Variant {} requires a payload",
                            self.name_of(generic_variant.name)
                        )
                    }
                    Some(payload_parsed_expr) => {
                        Some((generic_payload_type_id, payload_parsed_expr))
                    }
                },
                None => match payload_parsed_expr {
                    None => None,
                    Some(_payload_expr) => {
                        return failf!(
                            span,
                            "Variant {} does not take a payload",
                            self.name_of(generic_variant.name)
                        )
                    }
                },
            };

            let solved_or_passed_type_params: SmallVec<[SimpleNamedType; 6]> = if type_args
                .is_empty()
            {
                match payload_if_needed {
                    None => {
                        match ctx
                            .expected_type_id
                            .map(|t| (t, self.types.get_generic_instance_info(t)))
                        {
                            Some((expected_type, Some(spec_info))) => {
                                if spec_info.generic_parent == base_type_in_scope {
                                    // Solved params
                                    g_params
                                        .iter()
                                        .zip(spec_info.param_values.iter())
                                        .map(|(g_param, expected_specialized_type)| {
                                            SimpleNamedType {
                                                name: g_param.name,
                                                type_id: *expected_specialized_type,
                                            }
                                        })
                                        .collect()
                                } else {
                                    return failf!(
                                        span,
                                        "Cannot infer a type for {}; expected mismatching generic type {}",
                                        self.name_of(g_name), self.type_id_to_string(expected_type)
                                    );
                                }
                            }
                            _ => {
                                return failf!(
                                    span,
                                    "Cannot infer a type for {}",
                                    self.name_of(g_name)
                                )
                            }
                        }
                    }
                    Some((generic_variant_payload, payload)) => {
                        let mut args_and_params = Vec::with_capacity(2);

                        // There are only ever up to 2 'cases' to power inference
                        // - The expected return type together with the type of the enum itself
                        // - The passed payload together with the type of the payload itself
                        if let Some(expected) = ctx.expected_type_id {
                            args_and_params.push((
                                TypeOrParsedExpr::TypeId(expected),
                                g.inner,
                                true,
                            ))
                        };
                        args_and_params.push((
                            TypeOrParsedExpr::ParsedExpr(payload),
                            generic_variant_payload,
                            false,
                        ));
                        let solutions =
                            self.infer_types(&g_params, &args_and_params, span, ctx.scope_id)?;
                        solutions
                    }
                }
            } else {
                let mut passed_params = SmallVec::with_capacity(g_params.len());
                for (generic_param, passed_type_expr) in
                    g_params.clone().iter().zip(type_args.iter())
                {
                    let type_id = self.eval_type_expr(passed_type_expr.type_expr, ctx.scope_id)?;
                    passed_params.push(SimpleNamedType { name: generic_param.name, type_id });
                }
                passed_params
            };

            let concrete_type = self.instantiate_generic_type(
                base_type_in_scope,
                solved_or_passed_type_params.iter().map(|type_param| type_param.type_id).collect(),
            );
            let enum_constr = self.eval_enum_constructor(
                concrete_type,
                variant_name,
                payload_parsed_expr,
                ctx,
                span,
            )?;
            Ok(Some(enum_constr))
        } else {
            Ok(None)
        }
    }

    fn align_call_arguments_with_parameters<'params>(
        &mut self,
        fn_call: &FnCall,
        params: &'params [FnParamType],
        pre_evaled_params: Option<&[TypedExprId]>,
        calling_scope: ScopeId,
        tolerate_missing_context_args: bool,
    ) -> TyperResult<(
        SmallVec<[MaybeTypedExpr; FUNC_PARAM_OPT_COUNT]>,
        SmallVec<[&'params FnParamType; FUNC_PARAM_OPT_COUNT]>,
    )> {
        let fn_name = fn_call.name.name;
        let span = fn_call.span;
        let explicit_context_args = !fn_call.explicit_context_args.is_empty();
        let named = fn_call.args.first().is_some_and(|arg| arg.name.is_some());
        let mut final_args: SmallVec<[MaybeTypedExpr; FUNC_PARAM_OPT_COUNT]> = SmallVec::new();
        let mut final_params: SmallVec<[&FnParamType; FUNC_PARAM_OPT_COUNT]> = SmallVec::new();
        if !explicit_context_args {
            for context_param in params.iter().filter(|p| p.is_context) {
                if let Some(found_id) =
                    self.scopes.find_context_variable_by_type(calling_scope, context_param.type_id)
                {
                    let found = self.variables.get(found_id);
                    final_args.push(MaybeTypedExpr::Typed(self.exprs.add(TypedExpr::Variable(
                        VariableExpr { variable_id: found_id, type_id: found.type_id, span },
                    ))));
                    final_params.push(context_param);
                } else {
                    let is_source_loc = self
                        .types
                        .get_type_defn_info(context_param.type_id)
                        .is_some_and(|defn_info| {
                            let compiler_namespace = self
                                .namespaces
                                .find_child_by_name(ROOT_NAMESPACE_ID, get_ident!(self, "compiler"))
                                .unwrap();
                            defn_info.name == get_ident!(self, "SourceLocation")
                                && defn_info.scope == compiler_namespace.scope_id
                        });
                    if is_source_loc {
                        let expr = self.synth_source_location(span);
                        final_args.push(MaybeTypedExpr::Typed(expr));
                        final_params.push(context_param);
                    } else if !tolerate_missing_context_args {
                        return failf!(
                            span,
                            "Failed to find context parameter '{}'. No context variables of type {} are in scope",
                            self.name_of(context_param.name),
                            self.type_id_to_string(context_param.type_id)
                        );
                    } else {
                        continue;
                    }
                };
            }
        }

        let is_lambda =
            params.first().is_some_and(|p| p.name == get_ident!(self, LAMBDA_ENV_PARAM_NAME));
        let params = if is_lambda { &params[1..] } else { params };
        let explicit_param_count = params.iter().filter(|p| !p.is_context).count();
        let total_expected =
            if explicit_context_args { params.len() } else { explicit_param_count };
        let actual_passed_args = fn_call.explicit_context_args.iter().chain(fn_call.args.iter());
        let total_passed = pre_evaled_params
            .as_ref()
            .map(|v| v.len())
            .unwrap_or(actual_passed_args.clone().count());
        if total_passed != total_expected {
            return failf!(
                span,
                "Incorrect number of arguments to {}: expected {}, got {}",
                self.name_of(fn_call.name.name),
                total_expected,
                total_passed
            );
        }

        // If the user opted to pass context params explicitly, then check all params
        // If the user did not, then just check the non-context params, since the compiler is responsible
        // for looking up context params
        let expected_literal_params: &mut dyn Iterator<Item = &FnParamType> =
            if explicit_context_args {
                &mut params.iter()
            } else {
                &mut params.iter().filter(|p| !p.is_context)
            };

        if let Some(pre_evaled_params) = pre_evaled_params {
            for (expr, param) in pre_evaled_params.iter().zip(expected_literal_params) {
                final_args.push(MaybeTypedExpr::Typed(*expr));
                final_params.push(param)
            }
        } else {
            for (param_index, fn_param) in expected_literal_params.enumerate() {
                let matching_argument = if named {
                    let Some(name_match) =
                        actual_passed_args.clone().find(|arg| arg.name == Some(fn_param.name))
                    else {
                        return failf!(
                            fn_call.span,
                            "Missing named argument for parameter {}",
                            self.name_of(fn_param.name)
                        );
                    };
                    if let Some(dupe) =
                        final_params.iter().find(|param| param.name == name_match.name.unwrap())
                    {
                        let span = self.ast.exprs.get_span(name_match.value);
                        return failf!(span, "Duplicate named argument: {}", dupe.name);
                    };
                    Some(name_match)
                } else {
                    actual_passed_args.clone().nth(param_index)
                };
                let Some(param) = matching_argument else {
                    return failf!(
                        span,
                        "Missing argument to {}: {}",
                        self.name_of(fn_name).blue(),
                        self.name_of(fn_param.name).red()
                    );
                };
                final_args.push(MaybeTypedExpr::Parsed(param.value));
                final_params.push(fn_param);
            }
        }
        Ok((final_args, final_params))
    }

    fn check_call_argument(
        &self,
        _call_name: Identifier,
        param: &FnParamType,
        arg: TypedExprId,
        calling_scope: ScopeId,
    ) -> TyperResult<()> {
        if let Err(e) =
            self.check_types(param.type_id, self.exprs.get(arg).get_type(), calling_scope)
        {
            return failf!(
                self.exprs.get(arg).get_span(),
                "Invalid type for parameter {}: {}",
                self.name_of(param.name),
                e
            );
        };
        Ok(())
    }

    fn check_call_arguments(
        &self,
        _call_name: Identifier,
        aligned_params: &[FnParamType],
        aligned_args: &[TypedExprId],
        calling_scope: ScopeId,
    ) -> TyperResult<()> {
        for (param, expr) in aligned_params.iter().zip(aligned_args.iter()) {
            self.check_call_argument(_call_name, param, *expr, calling_scope)?;
        }
        Ok(())
    }

    pub fn get_callee_function_type(&self, callee: &Callee) -> TypeId {
        match callee {
            Callee::StaticFunction(function_id) | Callee::StaticLambda { function_id, .. } => {
                self.get_function(*function_id).type_id
            }
            Callee::DynamicFunction(function_reference_expr) => {
                let function_reference_type =
                    self.get_expr_type(*function_reference_expr).expect_reference().inner_type;
                function_reference_type
            }
            Callee::DynamicLambda(dynamic) => match self.get_expr_type(*dynamic) {
                Type::LambdaObject(lambda_object) => lambda_object.function_type,
                t => {
                    panic!("Invalid dynamic function callee: {}", self.type_to_string(t, false))
                }
            },
            Callee::Abstract { function_type, .. } => *function_type,
        }
    }

    fn eval_function_call(
        &mut self,
        fn_call: &FnCall,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = fn_call.span;
        assert!(
            fn_call.args.is_empty() || known_args.is_none(),
            "cannot pass both typed value args and parsed value args to eval_function_call"
        );
        let callee = match self.resolve_parsed_function_call(fn_call, known_args.as_ref(), ctx)? {
            Either::Left(expr) => return Ok(expr),
            Either::Right(callee) => callee,
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let original_function = callee.maybe_function_id().map(|f| self.get_function(f));
        let is_generic = original_function.is_some_and(|f| f.is_generic());

        let (callee, typechecked_arguments, type_args, return_type) = match is_generic {
            false => {
                let original_function_type =
                    self.types.get(self.get_callee_function_type(&callee)).as_function().unwrap();
                let original_function_return_type = original_function_type.return_type;
                let params = &original_function_type.params.clone();
                let (aligned_args, aligned_params) = self.align_call_arguments_with_parameters(
                    fn_call,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                )?;
                let mut typechecked_args = Vec::with_capacity(aligned_args.len());
                for (maybe_typed_expr, param) in aligned_args.iter().zip(aligned_params.iter()) {
                    let expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, ctx.with_expected_type(Some(param.type_id)))?
                        }
                    };
                    self.check_call_argument(fn_call.name.name, param, expr, ctx.scope_id)?;
                    typechecked_args.push(expr);
                }
                (callee, typechecked_args, smallvec![], original_function_return_type)
            }
            true => {
                let original_function = original_function.unwrap();
                let function_id = callee.maybe_function_id().unwrap();
                let type_params = &original_function.type_params;

                // We infer the type arguments, or just use them if the user has supplied them
                let type_args = match &known_args {
                    Some((ta, _va)) => {
                        // Need the ident
                        ta.iter()
                            .enumerate()
                            .map(|(idx, type_id)| SimpleNamedType {
                                name: type_params[idx].name,
                                type_id: *type_id,
                            })
                            .collect()
                    }
                    None => self.infer_and_constrain_call_type_args(fn_call, function_id, ctx)?,
                };

                let function_id = self.specialize_function_signature(&type_args, function_id)?;

                let specialized_fn_type = &self.get_function_type(function_id);
                let specialized_params = specialized_fn_type.params.clone();
                let specialized_return_type = specialized_fn_type.return_type;
                let (aligned_args, aligned_params) = self.align_call_arguments_with_parameters(
                    fn_call,
                    &specialized_params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                )?;
                //
                let mut typechecked_args = Vec::with_capacity(aligned_args.len());
                for (maybe_typed_expr, param) in aligned_args.iter().zip(aligned_params.iter()) {
                    let expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, ctx.with_expected_type(Some(param.type_id)))?
                        }
                    };
                    self.check_call_argument(fn_call.name.name, param, expr, ctx.scope_id)?;
                    typechecked_args.push(expr);
                }

                (
                    Callee::make_static(function_id),
                    typechecked_args,
                    type_args,
                    specialized_return_type,
                )
            }
        };

        let call_return_type = if typechecked_arguments
            .iter()
            .any(|arg| self.exprs.get(*arg).get_type() == NEVER_TYPE_ID)
        {
            NEVER_TYPE_ID
        } else {
            return_type
        };

        let call = Call {
            callee,
            args: typechecked_arguments,
            type_args,
            return_type: call_return_type,
            span,
        };

        if let Some(intrinsic_type) =
            call.callee.maybe_function_id().and_then(|id| self.get_function(id).intrinsic_type)
        {
            match intrinsic_type {
                IntrinsicFunction::CompilerSourceLocation => {
                    let source_location = self.synth_source_location(span);
                    Ok(source_location)
                }
                _ => Ok(self.exprs.add(TypedExpr::Call(call))),
            }
        } else {
            Ok(self.exprs.add(TypedExpr::Call(call)))
        }
    }

    fn infer_types(
        &mut self,
        unsolved_type_params: &[impl NamedType],
        inference_pairs: &[(TypeOrParsedExpr, TypeId, bool)],
        span: SpanId,
        scope_id: ScopeId,
    ) -> TyperResult<SmallVec<[SimpleNamedType; 6]>> {
        debug!("INFER LEVEL {}", self.inference_context.origin_stack.len());

        self.inference_context.origin_stack.push(span);
        let mut self_ = scopeguard::guard(self, |self_| {
            let id = self_.inference_context.origin_stack.pop().unwrap();
            debug_assert!(id == span);
            if self_.inference_context.origin_stack.is_empty() {
                debug!("Resetting inference buffer");
                self_.inference_context.constraints.clear();
                self_.inference_context.vars.clear();
                self_.inference_context.substitutions.clear();
                self_.inference_context.substitutions_vec.clear();
            } else {
                debug!(
                    "Not resetting inference buffer: {}",
                    self_.inference_context.origin_stack.len()
                );
            }
        });

        // Stores the mapping from the function (or type's) type parameters to their
        // corresponding instantiated type holes for this inference context
        let mut instantiation_set: SmallVec<[TypeSubstitutionPair; 6]> =
            SmallVec::with_capacity(unsolved_type_params.len());
        let mut solutions: SmallVec<[SimpleNamedType; 6]> =
            SmallVec::with_capacity(unsolved_type_params.len());

        let inference_var_count = self_.inference_context.vars.len();
        for (idx, param) in unsolved_type_params.iter().enumerate() {
            let hole_index = idx + inference_var_count;

            // nocommit note to self
            // Do not add a hole here for function type parameters, because they aren't really type
            // parameters, treat them just like params that _use_ the real type params. Example:
            // fn map[T, U, <FN>](a: List[T], f: some \T -> U): List[U]
            // Use what's passed for f to constrain T and U, we only have to solve for T and U; f
            // isn't really a "type" parameter because the function shape is already known
            // it's just a generic type to reflect that the function needs to know it to be
            // concrete
            // Maybe move it out of type params to some other place so its not a special case?
            if let Some(type_param) = self_.types.get(param.type_id()).as_type_parameter() {
                if type_param.is_function() {
                    continue;
                }
            }
            let type_hole = self_
                .types
                .add_type(Type::InferenceHole(InferenceHoleType { index: hole_index as u32 }));
            self_.inference_context.vars.push(type_hole);
            instantiation_set.push(TypeSubstitutionPair { from: param.type_id(), to: type_hole });
        }

        for (expr, gen_param, allow_mismatch) in inference_pairs.iter() {
            let instantiated_param_type =
                self_.substitute_in_type(*gen_param, &instantiation_set, None, None);
            debug!(
                "Instantiated type for inference with set: {}. Was: {}, is: {}",
                self_.pretty_print_type_substitutions(&instantiation_set, ", "),
                self_.type_id_to_string(*gen_param),
                self_.type_id_to_string(instantiated_param_type)
            );
            self_.calculate_inference_substitutions(span)?;

            let s = std::mem::take(&mut self_.inference_context.substitutions_vec);
            let expected_type_so_far =
                self_.substitute_in_type(instantiated_param_type, &s, None, None);
            self_.inference_context.substitutions_vec = s;

            let (argument_type, argument_span) = match expr {
                TypeOrParsedExpr::TypeId(type_id) => (*type_id, span),
                TypeOrParsedExpr::ParsedExpr(parsed_expr) => {
                    let inference_context = EvalExprContext::from_scope(scope_id)
                        .with_inference(true)
                        .with_expected_type(Some(expected_type_so_far));
                    let expr_id = self_.eval_expr(*parsed_expr, inference_context)?;
                    let expr = self_.exprs.get(expr_id);
                    (expr.get_type(), expr.get_span())
                }
            };
            debug!(
                "unify {} =:= {}",
                self_.type_id_to_string(argument_type),
                self_.type_id_to_string(expected_type_so_far),
            );
            if let TypeUnificationResult::NonMatching(msg) =
                self_.unify_and_find_substitutions(argument_type, expected_type_so_far)
            {
                // allow_mismatch is used to avoid reporting a mismatch on the return type,
                // before we're able to learn more about the rest of the inference. We get a better
                // error message if we wait to report the mismatch until the end
                if !allow_mismatch {
                    return failf!(
                        argument_span,
                        "Passed value does not match expected type: expected {} but got {}. {msg}",
                        self_.type_id_to_string(expected_type_so_far),
                        self_.type_id_to_string(argument_type)
                    );
                }
            };
            debug!(
                "subst\n\t{}",
                self_.pretty_print_type_substitutions(&self_.inference_context.constraints, "\n\t"),
            );
        }

        // TODO: enrich error
        self_.calculate_inference_substitutions(span)?;
        let final_substitutions = &self_.inference_context.substitutions;

        for (param_to_hole, param) in instantiation_set.iter().zip(unsolved_type_params.iter()) {
            let corresponding_hole = param_to_hole.to;
            let Some(solution) = final_substitutions.get(&corresponding_hole) else {
                return failf!(
                    span,
                    "Could not find a type for {}. What I know: {}",
                    self_.name_of(param.name()),
                    self_.pretty_print_named_types(&solutions, ", ")
                );
            };
            solutions.push(SimpleNamedType { name: param.name(), type_id: *solution });
        }
        debug!("INFER DONE {}", self_.pretty_print_named_types(&solutions, ", "));
        Ok(solutions)
    }

    fn infer_and_constrain_call_type_args(
        &mut self,
        fn_call: &FnCall,
        generic_function_id: FunctionId,
        ctx: EvalExprContext,
    ) -> TyperResult<SmallVec<[SimpleNamedType; 6]>> {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_type = self.get_function_type(generic_function_id);
        debug_assert!(generic_function.is_generic());
        let generic_type_params = generic_function.type_params.clone();
        let generic_function_params = generic_function_type.params.clone();
        let generic_function_return_type = generic_function_type.return_type;
        let passed_type_args = &fn_call.type_args;
        let solved_type_params = match passed_type_args.is_empty() {
            false => {
                if passed_type_args.len() != generic_type_params.len() {
                    return failf!(
                        fn_call.span,
                        "Expected {} type arguments but got {}",
                        generic_type_params.len(),
                        passed_type_args.len()
                    );
                }
                let mut evaled_params = SmallVec::with_capacity(passed_type_args.len());
                for (type_arg, param) in passed_type_args.iter().zip(generic_type_params.iter()) {
                    let passed_type = self.eval_type_expr(type_arg.type_expr, ctx.scope_id)?;
                    evaled_params.push(SimpleNamedType { name: param.name, type_id: passed_type });
                }
                evaled_params
            }
            true => {
                let (args, params) = self.align_call_arguments_with_parameters(
                    fn_call,
                    &generic_function_params,
                    None,
                    ctx.scope_id,
                    true,
                )?;
                let mut inference_pairs: SmallVec<[_; 6]> = match ctx.expected_type_id {
                    None => SmallVec::with_capacity(args.len()),
                    Some(expected) => {
                        let mut v = SmallVec::with_capacity(args.len() + 1);
                        v.push((
                            TypeOrParsedExpr::TypeId(expected),
                            generic_function_return_type,
                            true,
                        ));
                        v
                    }
                };
                inference_pairs.extend(args.iter().zip(params.iter()).map(|(expr, param)| {
                    let passed_type = match expr {
                        MaybeTypedExpr::Parsed(expr_id) => TypeOrParsedExpr::ParsedExpr(*expr_id),
                        MaybeTypedExpr::Typed(expr) => {
                            TypeOrParsedExpr::TypeId(self.exprs.get(*expr).get_type())
                        }
                    };
                    (passed_type, param.type_id, false)
                }));

                let solutions = self
                    .infer_types(&generic_type_params, &inference_pairs, fn_call.span, ctx.scope_id)
                    .map_err(|e| {
                        errf!(
                            e.span,
                            "Failed to solve for type parameters in call to {}. Detail:\n{}",
                            self.name_of(fn_call.name.name),
                            e.message
                        )
                    })?;
                solutions
            }
        };

        // Enforce ability constraints
        for (solution, type_param) in solved_type_params.iter().zip(generic_type_params.iter()) {
            self.check_type_constraints(
                type_param.name,
                type_param.type_id,
                solution.type_id,
                ctx.scope_id,
                fn_call.span,
            )
            .map_err(|e| {
                errf!(
                    e.span,
                    "Cannot invoke function '{}' with given types. {}",
                    self.name_of(fn_call.name.name),
                    e.message
                )
            })?;
        }
        Ok(solved_type_params)
    }

    // nocommit
    // One major issue I'm seeing is that we're going to specialize functions on types like '?1'
    // and '?2' all over the place. Is this necessary during inference, or can we finally do
    // a different sort of pass just focused on learning and inferring types. We still have
    // to resolve functions, and we still have to evaluate (untyped) blocks.
    // For anything annotated we can skip evaluation!!!
    //
    // We can save the types in some tree or lookup table for the lowering pass
    // I think we should hold off on this until we are interning expressions and blocks
    //fn get_type_of_expr(&mut self, expr: ParsedExpressionId, scope_id: ScopeId) -> Option<TypeId> {
    //    match self.ast.expressions.get(expr) {
    //        ParsedExpression::BinaryOp(binary_op) => todo!(),
    //        ParsedExpression::UnaryOp(unary_op) => todo!(),
    //        ParsedExpression::Literal(literal) => todo!(),
    //        ParsedExpression::InterpolatedString(parsed_interpolated_string) => todo!(),
    //        ParsedExpression::FnCall(fn_call) => todo!(),
    //        ParsedExpression::Variable(variable) => todo!(),
    //        ParsedExpression::FieldAccess(field_access) => todo!(),
    //        ParsedExpression::Block(block) => todo!(),
    //        ParsedExpression::If(if_expr) => todo!(),
    //        ParsedExpression::While(parsed_while_expr) => todo!(),
    //        ParsedExpression::Loop(parsed_loop_expr) => todo!(),
    //        ParsedExpression::Struct(_) => todo!(),
    //        ParsedExpression::ListLiteral(list_expr) => todo!(),
    //        ParsedExpression::For(for_expr) => todo!(),
    //        ParsedExpression::AnonEnumConstructor(anon_enum_constructor) => todo!(),
    //        ParsedExpression::Is(parsed_is_expression) => todo!(),
    //        ParsedExpression::Match(parsed_match_expression) => todo!(),
    //        ParsedExpression::AsCast(parsed_as_cast) => todo!(),
    //        ParsedExpression::lambda(parsed_lambda) => todo!(),
    //        ParsedExpression::Builtin(span_id) => todo!(),
    //    }
    //}

    fn add_substitution(&self, set: &mut Vec<TypeSubstitutionPair>, pair: TypeSubstitutionPair) {
        debug!(
            "Applying substitution {} -> {} to set {}",
            self.type_id_to_string(pair.from),
            self.type_id_to_string(pair.to),
            self.pretty_print_type_substitutions(set, ", ")
        );
        if pair.from == pair.to {
            return;
        }
        set.iter_mut().for_each(|existing| {
            if existing.from == pair.from {
                existing.from = pair.to
            } else if existing.to == pair.from {
                existing.to = pair.to
            };
        });
        set.retain(|pair| pair.from != pair.to);
        set.push(pair);

        debug!("Got set {}", self.pretty_print_type_substitutions(set, ", "));
    }

    fn calculate_inference_substitutions(&mut self, span: SpanId) -> TyperResult<()> {
        let mut ctx = std::mem::take(&mut self.inference_context);
        debug!(
            "calculate_inference_substitutions. constraints: [{}]",
            self.pretty_print_type_substitutions(&ctx.constraints, ", ")
        );
        ctx.substitutions.clear();
        ctx.substitutions_vec.clear();

        let final_pairs = &mut ctx.substitutions;
        for subst in &ctx.constraints {
            // 1. Validity
            // This may be unnecessary since we are passing in our 'current guess'
            // as the expected type once we have one, so we'll just get a failure when
            // evaluating that node rather than an inconsistent substitution

            //match self.types.get(subst.from) {
            //    Type::TypeVariable(tv) if tv.is_inference_variable => {}
            //    from => match self.types.get(subst.to) {
            //        Type::TypeVariable(tv) if tv.is_inference_variable => {}
            //        to => {
            //            if subst.from != subst.to {
            //                return failf!(
            //                    span,
            //                    "Contradicting substitution: {} -> {}",
            //                    self.type_id_to_string(subst.from),
            //                    self.type_id_to_string(subst.to)
            //                );
            //            }
            //        }
            //    },
            //}

            // 2. Consistency
            match final_pairs.entry(subst.from) {
                std::collections::hash_map::Entry::Vacant(e) => {
                    ctx.substitutions_vec.push(*subst);
                    e.insert(subst.to);
                }
                std::collections::hash_map::Entry::Occupied(occ) => {
                    let dest = occ.get();
                    if *dest != subst.to {
                        // TODO: We should include attribution spans on substitutions so that we
                        // can report to the user _why_ we expect such and such a value to be of
                        // a certain type
                        let e = failf!(
                            span,
                            "Type {} needs to be {} but also needs to be {}",
                            self.type_id_to_string(subst.from),
                            self.type_id_to_string(subst.to),
                            self.type_id_to_string(*dest),
                        );
                        self.inference_context = ctx;
                        return e;
                    }
                }
            }
        }
        for p in final_pairs.iter() {
            debug!(
                "final_pair {} -> {}",
                self.type_id_to_string(*p.0),
                self.type_id_to_string(*p.1)
            );
        }
        self.inference_context = ctx;
        Ok(())
    }

    fn unify_and_find_substitutions(
        &mut self,
        passed_type: TypeId,
        slot_type: TypeId,
    ) -> TypeUnificationResult {
        // eprintln!("unify_and_find_substitutions slot {}", self.type_id_to_string(slot_type));
        let mut inference_substitutions = std::mem::take(&mut self.inference_context.constraints);
        let result = self.unify_and_find_substitutions_rec(
            &mut inference_substitutions,
            passed_type,
            slot_type,
        );
        self.inference_context.constraints = inference_substitutions;
        result
    }

    fn unify_and_find_substitutions_rec(
        &self,
        substitutions: &mut Vec<TypeSubstitutionPair>,
        passed_type: TypeId,
        slot_type: TypeId,
    ) -> TypeUnificationResult {
        // passed_type           slot_type          -> result
        //
        // int                    '0                 -> '0 := int
        // List[int]              List['0]           -> '0 := int
        // Pair[int, string]      Pair['0, '1]        -> '0 := int, '1 := string
        // fn(int) -> int         Fn('0) -> '0        -> '0 := int
        // fn() -> List[string]   Fn() -> List['0]   -> '0 := string
        //
        // Recursive
        //
        // one day: Higher-order types (I dont see why not?)
        // List[int]              F[int]            -> F := List
        debug!(
            "unify_and_find_substitutions passed {} in slot {}",
            self.type_id_to_string(passed_type).blue(),
            self.type_id_to_string(slot_type).blue()
        );
        let counts = self.types.type_variable_counts.get(&slot_type).unwrap();
        if counts.inference_variable_count == 0 {
            debug!(
                "I should skip this because it has no type holes in it: {}",
                self.type_id_to_string(slot_type)
            );
            return TypeUnificationResult::NoHoles;
        }

        // This special case is removable, all tests pass, but I believe its currently
        // a slight optimization, and would be more of one with more complex types
        if let (Some(passed_info), Some(arg_info)) = (
            self.types.get_generic_instance_info(passed_type),
            self.types.get_generic_instance_info(slot_type),
        ) {
            // expr: NewList[int] arg: NewList['0]
            if passed_info.generic_parent == arg_info.generic_parent {
                debug!(
                    "comparing generic instances of {}",
                    self.type_id_to_string(arg_info.generic_parent)
                );
                // We can directly 'solve' every appearance of a type param here
                for (passed_type, arg_slot) in
                    passed_info.param_values.iter().zip(arg_info.param_values.iter())
                {
                    self.unify_and_find_substitutions_rec(substitutions, *passed_type, *arg_slot);
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return TypeUnificationResult::Matching;
        }

        match (self.types.get_no_follow(passed_type), self.types.get_no_follow(slot_type)) {
            (Type::InferenceHole(_actual_hole), _expected_type) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: passed_type, to: slot_type },
                );
                TypeUnificationResult::Matching
            }
            (_actual_type, Type::InferenceHole(_expected_hole)) => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: slot_type, to: passed_type },
                );
                TypeUnificationResult::Matching
            }
            (Type::Reference(passed_refer), Type::Reference(refer)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_refer.inner_type,
                    refer.inner_type,
                ),
            (Type::Struct(passed_struct), Type::Struct(struc)) => {
                // Struct example:
                // type Pair<T, U> = { a: T, b: U }
                // fn get_first<T, U>(p: Pair<T, U>): T { p.a }
                // get_first({ a: 1, b: 2})
                // passed_expr: Pair<int, int>, argument_type: Pair<T, U>
                // passed expr: { a: int, b: int }, argument_type: { a: T, b: U }
                //
                // Structs must have all same field names in same order
                let passed_fields = &passed_struct.fields;
                let fields = &struc.fields;
                if passed_fields.len() != fields.len() {
                    return TypeUnificationResult::NonMatching("field count");
                }
                for (idx, field) in fields.iter().enumerate() {
                    let passed_field = &passed_fields[idx];
                    if field.name != passed_field.name {
                        return TypeUnificationResult::NonMatching("field names");
                    }
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_field.type_id,
                        field.type_id,
                    );
                }
                TypeUnificationResult::Matching
            }
            (Type::Enum(passed_enum), Type::Enum(param_enum_type)) => {
                // Enum example
                // type Result<T, E> = enum Ok(T) | Err(E)
                // fn unwrap<T, E>(self: Result<T, E>): T {
                //  (self as Result<T,E>.Ok).payload
                // }
                // unwrap(Result<int, string>.Ok(1))
                // passed_expr: Result<int, string>, argument_type: Result<T, E>
                // passed_expr: enum Ok(int), Err(string), argument_type: enum Ok(T), Err(E)
                // Enum must have same variants with same tags, walk each variant and recurse on its payload
                let passed_variants = &passed_enum.variants;
                let variants = &param_enum_type.variants;
                if passed_variants.len() != variants.len() {
                    return TypeUnificationResult::NonMatching("variant count");
                }
                for (idx, variant) in variants.iter().enumerate() {
                    let passed_variant = &passed_variants[idx];
                    if variant.name != passed_variant.name {
                        return TypeUnificationResult::NonMatching("variant names");
                    }
                    if let Some(passed_payload) = passed_variant.payload {
                        if let Some(param_payload) = variant.payload {
                            self.unify_and_find_substitutions_rec(
                                substitutions,
                                passed_payload,
                                param_payload,
                            );
                        } else {
                            return TypeUnificationResult::NonMatching("payloads");
                        }
                    }
                }

                TypeUnificationResult::Matching
            }
            (Type::EnumVariant(passed_enum_variant), Type::Enum(_param_enum_type_variant)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_enum_variant.enum_type_id,
                    slot_type,
                ),
            (passed, Type::TypeParameter(slot_type_param))
                if slot_type_param.function_type.is_some() =>
            {
                if let Some(passed_function_type) =
                    self.extract_function_type_from_functionlike(passed)
                {
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_function_type,
                        slot_type_param.function_type.unwrap(),
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Expected a function type parameter; passed unrelated",
                    )
                }
            }
            (Type::Function(passed_fn), Type::Function(param_fn)) => {
                if passed_fn.params.len() == param_fn.params.len() {
                    for (passed_param, param_param) in
                        passed_fn.params.iter().zip(param_fn.params.iter())
                    {
                        if passed_param.is_lambda_env && param_param.is_lambda_env {
                            continue;
                        }
                        self.unify_and_find_substitutions_rec(
                            substitutions,
                            passed_param.type_id,
                            param_param.type_id,
                        );
                    }
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_fn.return_type,
                        param_fn.return_type,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Functions take a different number of arguments",
                    )
                }
            }
            (Type::Lambda(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_lambda.function_type,
                    param_lambda.function_type,
                ),
            (Type::LambdaObject(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_lambda.function_type,
                    param_lambda.function_type,
                ),
            (_, _) if passed_type == slot_type => TypeUnificationResult::Matching,
            _ => TypeUnificationResult::NonMatching("Unrelated types"),
        }
    }

    fn extract_function_type_from_functionlike(&self, typ: &Type) -> Option<TypeId> {
        // What can we pass when we expect a function type parameter?
        // A Function reference: fn_name.pointer()
        // A lambda: \x -> x + 1
        // A lambda-object: dyn[A -> B]
        match typ {
            Type::Reference(r) => {
                if self.types.get(r.inner_type).as_function().is_some() {
                    Some(r.inner_type)
                } else {
                    None
                }
            }
            Type::Lambda(lam) => Some(lam.function_type),
            Type::LambdaObject(lambda_object) => Some(lambda_object.function_type),
            _ => None,
        }
    }

    fn specialize_function_signature(
        &mut self,
        type_arguments: &[SimpleNamedType],
        generic_function_id: FunctionId,
    ) -> TyperResult<FunctionId> {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_param_variables = generic_function.param_variables.clone();
        let generic_function_scope = generic_function.scope;

        for existing_specialization in &generic_function.child_specializations {
            if existing_specialization.type_arguments == type_arguments {
                debug!(
                    "Found existing specialization for function {} with types: {}",
                    self.name_of(generic_function.name),
                    existing_specialization
                        .type_arguments
                        .iter()
                        .map(|named_type| self.type_id_to_string(named_type.type_id))
                        .collect::<Vec<_>>()
                        .join("_")
                );
                return Ok(existing_specialization.specialized_function_id);
            }
        }
        let specialized_name_string = {
            use std::fmt::Write;
            let mut new_name = String::with_capacity(256);
            let spec_num = generic_function.child_specializations.len() + 1;
            write!(new_name, "{}_spec_", self.name_of(generic_function.name)).unwrap();
            for nt in type_arguments {
                self.display_type_id(nt.type_id, false, &mut new_name).unwrap()
            }
            write!(new_name, "_{spec_num}").unwrap();
            new_name
        };

        // Transform the signature of the generic function by substituting
        let generic_function_type_id = generic_function.type_id;
        let pairs: Vec<_> = generic_function
            .type_params
            .iter()
            .zip(type_arguments)
            .map(|(gen_param, type_arg)| TypeSubstitutionPair {
                from: gen_param.type_id,
                to: type_arg.type_id,
            })
            .collect();
        let specialized_function_type_id =
            self.substitute_in_type(generic_function_type_id, &pairs, None, None);
        let specialized_function_type =
            self.types.get(specialized_function_type_id).as_function().unwrap();

        let specialized_name = self.ast.identifiers.intern(&specialized_name_string);
        let spec_fn_scope = self.scopes.add_sibling_scope(
            generic_function_scope,
            ScopeType::FunctionScope,
            None,
            Some(specialized_name),
        );

        for nt in type_arguments {
            let _ = self.scopes.add_type(spec_fn_scope, nt.name, nt.type_id);
        }

        // The issue is the function type gets de-duped w/ another w/ different names.
        let param_variables: Vec<VariableId> = specialized_function_type
            .params
            .iter()
            .zip(generic_function_param_variables.iter())
            .map(|(specialized_param_type, generic_param)| {
                let name = self.variables.get(*generic_param).name;
                let variable_id = self.variables.add_variable(Variable {
                    type_id: specialized_param_type.type_id,
                    name,
                    is_mutable: false,
                    owner_scope: spec_fn_scope,
                    is_context: specialized_param_type.is_context,
                    constant_id: None,
                });
                self.scopes.add_variable(spec_fn_scope, name, variable_id);
                variable_id
            })
            .collect();
        let specialization_info = SpecializationInfo {
            parent_function: generic_function_id,
            type_arguments: Vec::from(type_arguments),
            specialized_function_id: FunctionId::PENDING,
            specialized_function_type: specialized_function_type_id,
        };
        let generic_function = self.get_function(generic_function_id);
        let has_body = self
            .ast
            .get_function(generic_function.parsed_id.as_function_id().unwrap())
            .block
            .is_some();
        let specialized_function = TypedFunction {
            name: specialized_name,
            scope: spec_fn_scope,
            param_variables,
            type_params: smallvec![],
            body_block: None,
            intrinsic_type: generic_function.intrinsic_type,
            linkage: generic_function.linkage,
            child_specializations: vec![],
            specialization_info: Some(specialization_info),
            parsed_id: generic_function.parsed_id,
            type_id: specialized_function_type_id,
            compiler_debug: generic_function.compiler_debug,
            kind: generic_function.kind,
            is_concrete: false,
        };
        let specialized_function_id = self.add_function(specialized_function);
        let is_concrete = self.get_function(specialized_function_id).is_concrete;

        self.scopes
            .set_scope_owner_id(spec_fn_scope, ScopeOwnerId::Function(specialized_function_id));

        debug!(
            "Specializing sig (has_body={has_body}, is_concrete={is_concrete}) of {}",
            self.function_id_to_string(generic_function_id, false)
        );
        if has_body && is_concrete {
            self.functions_pending_body_specialization.push(specialized_function_id);
        }

        Ok(specialized_function_id)
    }

    fn specialize_function_body(&mut self, function_id: FunctionId) -> TyperResult<()> {
        let specialized_function = self.get_function(function_id);
        let specialized_return_type = self.get_function_type(function_id).return_type;
        let parent_function = specialized_function
            .specialization_info
            .as_ref()
            .map(|spec_info| spec_info.parent_function)
            .or(specialized_function.kind.blanket_parent_function_id())
            .expect(
                "specialize_function_body wants a normal specialization or a blanket impl defn",
            );
        let parent_function = self.get_function(parent_function);
        debug_assert!(parent_function.body_block.is_some());

        // Approach 1: Re-run whole body w/ bound types
        // Downside: cloning, extra work, etc
        // Upside: way way less code.
        // Have to bind type names that shouldn't exist, kinda
        let block_ast = self
            .ast
            .get_function(parent_function.parsed_id.as_function_id().unwrap())
            .block
            .as_ref()
            .unwrap()
            .clone();
        let block = self.eval_block(
            &block_ast,
            EvalExprContext::from_scope(specialized_function.scope)
                .with_expected_type(Some(specialized_return_type)),
            true,
        )?;
        let block_id = self.exprs.add(TypedExpr::Block(block));

        self.get_function_mut(function_id).body_block = Some(block_id);
        Ok(())
    }

    pub fn is_function_concrete(&self, function: &TypedFunction) -> bool {
        match function.intrinsic_type {
            Some(intrinsic) if intrinsic.is_inlined() => false,
            _ => match &function.kind {
                TypedFunctionKind::AbilityDefn(_) => false,
                _other => {
                    let is_concrete = match &function.specialization_info {
                        None => function.type_params.is_empty(),
                        Some(_spec_info) => {
                            let info = self.types.get_type_variable_info(function.type_id);
                            info.type_parameter_count == 0 && info.inference_variable_count == 0
                        }
                    };
                    if function.compiler_debug {
                        eprintln!(
                            "is_function_concrete={is_concrete} for {}",
                            self.function_to_string(function, false)
                        );
                    }
                    is_concrete
                }
            },
        }
    }

    fn eval_stmt(
        &mut self,
        stmt: ParsedStmtId,
        ctx: EvalExprContext,
    ) -> TyperResult<Option<TypedStmtId>> {
        match self.ast.stmts.get(stmt) {
            ParsedStmt::Use(use_stmt) => {
                let parsed_use = self.ast.uses.get_use(use_stmt.use_id);
                // These uses should always hit since we only do 1 pass inside function bodies, and
                // at that point all symbols are resolvable
                let Some(useable_symbol) = self.scopes.find_useable_symbol(
                    ctx.scope_id,
                    &parsed_use.target,
                    &self.namespaces,
                    &self.ast.identifiers,
                )?
                else {
                    return failf!(
                        parsed_use.target.span,
                        "Could not find {}",
                        self.name_of(parsed_use.target.name)
                    );
                };
                self.scopes.add_use_binding(
                    ctx.scope_id,
                    useable_symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                Ok(None)
            }
            ParsedStmt::ValDef(val_def) => {
                static_assert_size!(parse::ValDef, 20);
                let val_def = val_def.clone();
                let provided_type = match val_def.type_expr.as_ref() {
                    None => None,
                    Some(&type_expr) => Some(self.eval_type_expr(type_expr, ctx.scope_id)?),
                };
                let expected_type = match provided_type {
                    Some(provided_type) => {
                        if val_def.is_referencing() {
                            let Type::Reference(expected_reference_type) =
                                self.types.get(provided_type)
                            else {
                                return failf!(
                                    val_def.span,
                                    "Expected type must be a reference type when using let*"
                                );
                            };
                            Some(expected_reference_type.inner_type)
                        } else {
                            Some(provided_type)
                        }
                    }
                    None => None,
                };
                let value_expr =
                    self.eval_expr(val_def.value, ctx.with_expected_type(expected_type))?;
                let actual_type = self.exprs.get(value_expr).get_type();

                if let Some(expected_type) = expected_type {
                    if let Err(msg) = self.check_types(expected_type, actual_type, ctx.scope_id) {
                        return failf!(val_def.span, "Local variable type mismatch: {}", msg,);
                    }
                };

                let variable_type = if val_def.is_referencing() {
                    self.types.add_reference_type(actual_type)
                } else {
                    actual_type
                };

                let variable_id = self.variables.add_variable(Variable {
                    is_mutable: val_def.is_mutable(),
                    name: val_def.name,
                    type_id: variable_type,
                    owner_scope: ctx.scope_id,
                    is_context: val_def.is_context(),
                    constant_id: None,
                });
                let val_def_stmt = TypedStmt::Let(LetStmt {
                    variable_type,
                    variable_id,
                    initializer: value_expr,
                    is_referencing: val_def.is_referencing(),
                    span: val_def.span,
                });
                if val_def.is_context() {
                    self.scopes.add_context_variable(
                        ctx.scope_id,
                        val_def.name,
                        variable_id,
                        variable_type,
                    );
                } else {
                    self.scopes.add_variable(ctx.scope_id, val_def.name, variable_id);
                }
                let stmt_id = self.stmts.add(val_def_stmt);
                Ok(Some(stmt_id))
            }
            ParsedStmt::Assignment(assignment) => {
                static_assert_size!(parse::Assignment, 12);
                let assignment = assignment.clone();
                let ParsedExpression::Variable(_) = self.ast.exprs.get(assignment.lhs) else {
                    return failf!(
                        self.ast.exprs.get_span(assignment.lhs),
                        "Value assignment destination must be a variable"
                    );
                };
                let lhs = self.eval_variable(assignment.lhs, ctx.scope_id, true)?;
                let lhs_type = self.exprs.get(lhs).get_type();
                let rhs = self.eval_expr(assignment.rhs, ctx.with_expected_type(Some(lhs_type)))?;
                let rhs_type = self.exprs.get(rhs).get_type();
                if let Err(msg) = self.check_types(lhs_type, rhs_type, ctx.scope_id) {
                    return failf!(assignment.span, "Invalid type for assignment: {}", msg,);
                }
                let stmt_id = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                    destination: lhs,
                    value: rhs,
                    span: assignment.span,
                    kind: AssignmentKind::Value,
                }));
                Ok(Some(stmt_id))
            }
            ParsedStmt::SetRef(set_stmt) => {
                static_assert_size!(parse::SetStmt, 12);
                let set_stmt = set_stmt.clone();
                let lhs = self.eval_expr(set_stmt.lhs, ctx.with_no_expected_type())?;
                let lhs_type = self.exprs.get(lhs).get_type();
                let Some(lhs_type) = self.types.get(lhs_type).as_reference() else {
                    return failf!(
                        self.ast.exprs.get_span(set_stmt.lhs),
                        "Expected a reference type; got {}",
                        self.type_id_to_string(lhs_type)
                    );
                };
                let expected_rhs = lhs_type.inner_type;
                let rhs =
                    self.eval_expr(set_stmt.rhs, ctx.with_expected_type(Some(expected_rhs)))?;
                let rhs_type = self.exprs.get(rhs).get_type();
                if let Err(msg) = self.check_types(expected_rhs, rhs_type, ctx.scope_id) {
                    return failf!(set_stmt.span, "Invalid type for assignment: {}", msg,);
                }
                let stmt_id = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                    destination: lhs,
                    value: rhs,
                    span: set_stmt.span,
                    kind: AssignmentKind::Reference,
                }));
                Ok(Some(stmt_id))
            }
            ParsedStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(*expression, ctx)?;
                let expr_type = self.exprs.get(expr).get_type();
                let stmt_id = self.stmts.add(TypedStmt::Expr(expr, expr_type));
                Ok(Some(stmt_id))
            }
        }
    }
    fn eval_block(
        &mut self,
        block: &Block,
        ctx: EvalExprContext,
        needs_terminator: bool,
    ) -> TyperResult<TypedBlock> {
        let mut statements: Vec<TypedStmtId> = Vec::with_capacity(block.stmts.len());
        let mut last_expr_type: TypeId = UNIT_TYPE_ID;
        let mut last_stmt_is_divergent = false;
        for (index, stmt) in block.stmts.iter().enumerate() {
            if last_stmt_is_divergent {
                return failf!(
                    self.ast.get_stmt_span(*stmt),
                    "Dead code following divergent statement",
                );
            }
            let is_last = index == block.stmts.len() - 1;
            let expected_type = if is_last { ctx.expected_type_id } else { None };

            let Some(stmt_id) = self.eval_stmt(*stmt, ctx.with_expected_type(expected_type))?
            else {
                continue;
            };
            let stmt = self.stmts.get(stmt_id);
            let stmt_span = self.get_stmt_span(stmt_id);
            last_expr_type = self.get_stmt_type(stmt_id);
            last_stmt_is_divergent = last_expr_type == NEVER_TYPE_ID;

            if is_last && needs_terminator {
                if last_stmt_is_divergent {
                    // No action needed; terminator exists
                    statements.push(stmt_id);
                } else {
                    match stmt {
                        TypedStmt::Expr(expr, _expr_type_id) => {
                            // Return this expr
                            let expr_span = self.exprs.get(*expr).get_span();
                            let return_expr = self.exprs.add(TypedExpr::Return(TypedReturn {
                                span: expr_span,
                                value: *expr,
                            }));
                            let return_stmt =
                                self.stmts.add(TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
                            statements.push(return_stmt);
                        }
                        TypedStmt::Assignment(_) | TypedStmt::Let(_) => {
                            let unit = self.exprs.add(TypedExpr::Unit(stmt_span));
                            let return_unit_expr = self.exprs.add(TypedExpr::Return(TypedReturn {
                                span: stmt_span,
                                value: unit,
                            }));
                            let return_unit = TypedStmt::Expr(return_unit_expr, NEVER_TYPE_ID);
                            let return_unit_id = self.stmts.add(return_unit);
                            statements.push(stmt_id);
                            statements.push(return_unit_id);
                        }
                    };
                }
            } else {
                statements.push(stmt_id);
            }
        }

        let typed_block = TypedBlock {
            expr_type: last_expr_type,
            scope_id: ctx.scope_id,
            statements,
            span: block.span,
        };
        Ok(typed_block)
    }

    fn resolve_intrinsic_function_type(
        &self,
        fn_name: Identifier,
        namespace_chain: &[Identifier],
        ability_impl_info: Option<(AbilityId, TypeId)>,
    ) -> Result<IntrinsicFunction, String> {
        let fn_name_str = self.ast.identifiers.get_name(fn_name);
        let second = namespace_chain.get(1).map(|id| self.name_of(*id));
        let result = if let Some((ability_id, ability_impl_type_id)) = ability_impl_info {
            match (ability_id, self.types.get(ability_impl_type_id)) {
                // Leaving this example of how to do intrinsic ability fns
                // Even though we have bitwise below
                // (EQUALS_ABILITY_ID, _t) if ability_impl_type_id == STRING_TYPE_ID => {
                //     if fn_name_str == "equals" {
                //         Some(IntrinsicFunction::StringEquals)
                //     } else {
                //         None
                //     }
                // }
                (BITWISE_ABILITY_ID, Type::Integer(_)) => match fn_name_str {
                    "bitNot" => Some(IntrinsicFunction::BitNot),
                    "bitAnd" => Some(IntrinsicFunction::BitAnd),
                    "bitOr" => Some(IntrinsicFunction::BitOr),
                    "xor" => Some(IntrinsicFunction::BitXor),
                    "shiftLeft" => Some(IntrinsicFunction::BitShiftLeft),
                    "shiftRight" => Some(IntrinsicFunction::BitShiftRight),
                    _ => None,
                },
                _ => None,
            }
        } else {
            match second {
                // _root
                None => match fn_name_str {
                    "sizeOf" => Some(IntrinsicFunction::SizeOf),
                    "sizeOfStride" => Some(IntrinsicFunction::SizeOfStride),
                    "alignOf" => Some(IntrinsicFunction::AlignOf),
                    _ => None,
                },
                Some("types") => match fn_name_str {
                    "typeId" => Some(IntrinsicFunction::TypeId),
                    _ => None,
                },
                Some("compiler") => match fn_name_str {
                    "location" => Some(IntrinsicFunction::CompilerSourceLocation),
                    _ => None,
                },
                Some("bool") => match fn_name_str {
                    "negated" => Some(IntrinsicFunction::BoolNegate),
                    _ => None,
                },
                Some("string") => None,
                Some("List") => None,
                Some("char") => None,
                Some("Pointer") => match fn_name_str {
                    "refAtIndex" => Some(IntrinsicFunction::PointerIndex),
                    _ => None,
                },
                Some("Bits") => match fn_name_str {
                    "bitNot" => Some(IntrinsicFunction::BitNot),
                    "bitAnd" => Some(IntrinsicFunction::BitAnd),
                    "bitOr" => Some(IntrinsicFunction::BitOr),
                    "xor" => Some(IntrinsicFunction::BitXor),
                    "shiftLeft" => Some(IntrinsicFunction::BitShiftLeft),
                    "shiftRight" => Some(IntrinsicFunction::BitShiftRight),
                    _ => None,
                },
                Some(_) => None,
            }
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Could not resolve intrinsic function type for function {}",
                self.name_of(fn_name),
            )),
        }
    }

    fn eval_enum_constructor(
        &mut self,
        enum_type: TypeId,
        variant_name: Identifier,
        payload: Option<ParsedExpressionId>,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let e = self.types.get(enum_type).expect_enum();
        let Some(variant) = e.variant_by_name(variant_name) else {
            return failf!(
                span,
                "No variant '{}' exists in enum '{}'",
                self.name_of(variant_name).blue(),
                self.type_id_to_string(enum_type)
            );
        };
        let variant_type_id = variant.my_type_id;
        let variant_index = variant.index;
        let payload = match variant.payload {
            None => {
                if let Some(_payload_arg) = payload {
                    failf!(
                        span,
                        "Variant '{}' does not have a payload",
                        self.name_of(variant_name).blue()
                    )
                } else {
                    Ok(None)
                }
            }
            Some(payload_type) => {
                if let Some(payload_arg) = payload {
                    let payload_value =
                        self.eval_expr(payload_arg, ctx.with_expected_type(Some(payload_type)))?;
                    let payload_value_type = self.exprs.get(payload_value).get_type();
                    if let Err(msg) =
                        self.check_types(payload_type, payload_value_type, ctx.scope_id)
                    {
                        return failf!(span, "Variant payload type mismatch: {}", msg);
                    }
                    Ok(Some(payload_value))
                } else {
                    failf!(
                        span,
                        "Variant '{}' requires a payload",
                        self.name_of(variant_name).blue()
                    )
                }
            }
        }?;
        let never_payload = payload.is_some_and(|p| self.exprs.get(p).get_type() == NEVER_TYPE_ID);
        let output_type = if never_payload {
            NEVER_TYPE_ID
        } else {
            match ctx.expected_type_id.map(|t| self.types.get(t)) {
                Some(Type::EnumVariant(ev)) if ev.my_type_id == variant_type_id => variant_type_id,
                _ => enum_type,
            }
        };
        Ok(self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: output_type,
            variant_name,
            variant_index,
            payload,
            span,
        })))
    }

    fn check_type_constraint(
        &mut self,
        target_type: TypeId,
        signature: TypedAbilitySignature,
        name: Identifier,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        if let Some(impl_handle) =
            self.find_ability_impl_for_type(target_type, signature.ability_id, span)
        {
            let found_impl = self.get_ability_impl(impl_handle.full_impl_id);
            debug_assert!(signature.impl_arguments.len() == found_impl.impl_arguments.len());
            for (constraint_arg, passed_arg) in
                signature.impl_arguments.iter().zip(found_impl.impl_arguments.iter())
            {
                debug_assert!(constraint_arg.name == passed_arg.name);

                if self.get_type_id_resolved(constraint_arg.type_id, scope_id)
                    != self.get_type_id_resolved(passed_arg.type_id, scope_id)
                {
                    return failf!(
                            span,
                            "Provided type {} := {} does implement required ability {}, but the implementation parameter {} is wrong: Expected type was {} but the actual implementation uses {}",
                            self.name_of(name),
                            self.type_id_to_string(target_type),
                            self.name_of(self.get_ability(signature.ability_id).name),
                            self.name_of(constraint_arg.name),
                            self.type_id_to_string(constraint_arg.type_id),
                            self.type_id_to_string(passed_arg.type_id),
                        );
                }
            }
            Ok(())
        } else {
            failf!(
                span,
                "Provided type for {} is {} which does not implement required ability {}",
                self.name_of(name),
                self.type_id_to_string(target_type),
                self.name_of(self.get_ability(signature.ability_id).name)
            )
        }
    }

    fn check_type_constraints(
        &mut self,
        param_name: Identifier,
        param_type: TypeId,
        passed_type: TypeId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        let constraints = self.get_constrained_ability_impls_for_type(param_type).to_vec();
        for constraint in &constraints {
            let signature = TypedAbilitySignature {
                ability_id: constraint.ability_id,
                impl_arguments: self
                    .get_ability_impl(constraint.full_impl_id)
                    .impl_arguments
                    .clone(),
            };
            self.check_type_constraint(passed_type, signature, param_name, scope_id, span)?;
        }
        Ok(())
    }

    fn check_ability_arguments(
        &mut self,
        ability_id: AbilityId,
        arguments: &[SimpleNamedType],
        span: SpanId,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> TyperResult<(Vec<SimpleNamedType>, SmallVec<[SimpleNamedType; 4]>)> {
        let ability = self.get_ability(ability_id);
        let ability_parameters = ability.parameters.clone();

        // Catch unrecognized arguments first
        for arg in arguments {
            let has_matching_param = ability_parameters.iter().any(|param| param.name == arg.name);
            if !has_matching_param {
                return failf!(span, "No parameter named {}", self.name_of(arg.name));
            }
        }

        let mut ability_arguments = Vec::with_capacity(arguments.len());
        let mut impl_arguments = SmallVec::with_capacity(arguments.len());
        for param in
            ability_parameters.iter().filter(|p| !skip_impl_check || p.is_ability_side_param())
        {
            let Some(matching_arg) = arguments.iter().find(|a| a.name == param.name) else {
                return failf!(
                    span,
                    "Missing argument for ability parameter {}",
                    self.name_of(param.name)
                );
            };
            // Ensure that the passed type meets the parameter's declared constraints
            self.check_type_constraints(
                param.name,
                param.type_variable_id,
                matching_arg.type_id,
                scope_id,
                span,
            )?;
            if param.is_impl_param {
                impl_arguments.push(*matching_arg)
            } else {
                ability_arguments.push(*matching_arg)
            };
        }

        Ok((ability_arguments, impl_arguments))
    }

    fn specialize_ability(
        &mut self,
        ability_id: AbilityId,
        arguments: Vec<SimpleNamedType>,
        span: SpanId,
    ) -> TyperResult<AbilityId> {
        let ability = self.get_ability(ability_id);
        if ability.kind.is_concrete() {
            return Ok(ability_id);
        }
        let generic_ability_id = ability_id;
        let ability_ast_id = ability.ast_id;
        let ability_scope_id = ability.scope_id;
        let ability_name = ability.name;
        let ability_parameters = ability.parameters.clone();
        let ability_namespace_id = ability.namespace_id;
        let specializations = self.get_ability(generic_ability_id).kind.specializations();
        if arguments.len() > ability_parameters.len() {
            panic!("Passed too many arguments to specialize_ability; probably passed impl args");
        }
        if let Some(cached_specialization) =
            specializations.iter().find(|spec| spec.arguments == arguments)
        {
            debug!(
                "Using cached ability specialization for {}",
                self.name_of(self.get_ability(cached_specialization.specialized_child).name)
            );
            return Ok(cached_specialization.specialized_child);
        };

        let specialized_ability_name = {
            use std::fmt::Write;
            let mut s = String::with_capacity(64);
            write!(&mut s, "{}_", self.name_of(ability_name)).unwrap();
            for (index, arg) in arguments.iter().enumerate() {
                self.write_ident(&mut s, arg.name).unwrap();
                write!(&mut s, "_").unwrap();
                self.display_type_id(arg.type_id, false, &mut s).unwrap();

                let is_last = index == arguments.len() - 1;
                if !is_last {
                    write!(&mut s, "_").unwrap();
                }
            }
            self.ast.identifiers.intern(s)
        };

        let specialized_ability_scope = self.scopes.add_sibling_scope(
            ability_scope_id,
            ScopeType::AbilityDefn,
            None,
            Some(specialized_ability_name),
        );

        for (arg_type, param) in arguments.iter().zip(ability_parameters.iter()) {
            let _ = self.scopes.add_type(specialized_ability_scope, param.name, arg_type.type_id);
        }

        // The implementor is responsible for providing the impl_params, so those are the
        // only parameters that the specialized ability should now take
        // ... It also takes 'Self', of course, but we don't treat that as a 'parameter'
        // since we take that for granted in the definition of 'ability'
        let mut impl_params = ability_parameters;
        impl_params.retain(|p| p.is_impl_param);
        for impl_param in &impl_params {
            let _ = self.scopes.add_type(
                specialized_ability_scope,
                impl_param.name,
                impl_param.type_variable_id,
            );
        }

        let specialized_ability_id = self.next_ability_id();
        let spec_info = AbilitySpec9nInfo {
            generic_parent: generic_ability_id,
            specialized_child: specialized_ability_id,
            arguments,
        };
        let self_ident = get_ident!(self, "Self");
        let new_self_type_id = self.add_type_variable(
            TypeParameter {
                name: self_ident,
                scope_id: specialized_ability_scope,
                span,
                function_type: None,
            },
            smallvec![],
        );
        let _ = self
            .scopes
            .get_scope_mut(specialized_ability_scope)
            .add_type(self_ident, new_self_type_id);

        let specialized_ability_id = self.add_ability(TypedAbility {
            name: specialized_ability_name,
            self_type_id: new_self_type_id,
            parameters: impl_params,
            functions: vec![],
            scope_id: specialized_ability_scope,
            ast_id: ability_ast_id,
            namespace_id: ability_namespace_id,
            kind: TypedAbilityKind::Specialized(spec_info.clone()),
        });

        let parsed_ability = self.ast.get_ability(ability_ast_id);
        let mut specialized_functions = Vec::with_capacity(parsed_ability.functions.len());
        for parsed_fn in parsed_ability.functions.clone().iter() {
            let function_id = self.eval_function_declaration(
                *parsed_fn,
                specialized_ability_scope,
                Some(FunctionAbilityContextInfo::ability_id_only(specialized_ability_id)),
                ability_namespace_id,
            )?;
            let function_name = self.get_function(function_id).name;
            specialized_functions.push(TypedAbilityFunctionRef { function_id, function_name });
        }

        self.get_ability_mut(specialized_ability_id).functions = specialized_functions;
        {
            let parent_ability = self.get_ability_mut(generic_ability_id);
            let TypedAbilityKind::Generic { specializations } = &mut parent_ability.kind else {
                panic!("expected generic ability while specializing")
            };
            specializations.push(spec_info);
        }

        self.scopes.set_scope_owner_id(
            specialized_ability_scope,
            ScopeOwnerId::Ability(specialized_ability_id),
        );

        Ok(specialized_ability_id)
    }

    fn check_ability_expr(
        &mut self,
        ability_expr: &parse::ParsedAbilityExpr,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> TyperResult<(AbilityId, Vec<SimpleNamedType>, SmallVec<[SimpleNamedType; 4]>)> {
        let ability_id = self.find_ability_or_declare(&ability_expr.name, scope_id)?;

        let mut arguments = Vec::with_capacity(ability_expr.arguments.len());
        for arg in ability_expr.arguments.iter() {
            let arg_type = self.eval_type_expr(arg.value, scope_id)?;
            arguments.push(SimpleNamedType { name: arg.name, type_id: arg_type });
        }

        let (ability_args, impl_args) = self.check_ability_arguments(
            ability_id,
            &arguments,
            ability_expr.span,
            scope_id,
            skip_impl_check,
        )?;
        Ok((ability_id, ability_args, impl_args))
    }

    fn eval_ability_expr(
        &mut self,
        ability_expr: &parse::ParsedAbilityExpr,
        skip_impl_check: bool,
        scope_id: ScopeId,
    ) -> TyperResult<TypedAbilitySignature> {
        let (base_ability_id, ability_arguments, impl_arguments) =
            self.check_ability_expr(ability_expr, scope_id, skip_impl_check)?;
        let new_ability_id =
            self.specialize_ability(base_ability_id, ability_arguments, ability_expr.span)?;
        Ok(TypedAbilitySignature { ability_id: new_ability_id, impl_arguments })
    }

    fn eval_function_declaration(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        parent_scope_id: ScopeId,
        ability_info: Option<FunctionAbilityContextInfo>,
        namespace_id: NamespaceId,
    ) -> TyperResult<FunctionId> {
        let namespace = self.namespaces.get(namespace_id);
        let companion_type_id = namespace.companion_type_id;
        // TODO(perf): clone of ParsedFunction
        let parsed_function = self.ast.get_function(parsed_function_id).clone();
        let debug_directive = parsed_function
            .directives
            .iter()
            .find(|p| matches!(p, ParsedDirective::CompilerDebug { .. }));
        let is_debug = debug_directive.is_some();
        if is_debug {
            self.push_debug_level();
        }
        let mut self_ = scopeguard::guard(self, |s| {
            if is_debug {
                s.pop_debug_level()
            }
        });
        let parsed_function_linkage = parsed_function.linkage;
        let parsed_function_name = parsed_function.name;
        let parsed_function_span = parsed_function.span;
        let parsed_function_params = &parsed_function.params;
        let parsed_function_context_params = &parsed_function.context_params;
        let parsed_function_type_params = &parsed_function.type_params;

        let is_ability_decl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_none());
        let is_ability_impl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_some());
        let ability_id = ability_info.as_ref().map(|info| info.ability_id);
        let impl_info = ability_info.as_ref().and_then(|info| info.impl_info.as_ref());
        let ability_kind = ability_id.map(|id| &self_.get_ability(id).kind);
        let impl_self_type = impl_info.map(|impl_info| impl_info.self_type_id);
        let ability_kind_is_specialized = ability_kind.is_some_and(|kind| kind.is_specialized());
        let skip_ast_mapping = ability_kind_is_specialized
            || ability_info.as_ref().is_some_and(|info| {
                info.impl_info.as_ref().is_some_and(|impl_info| {
                    impl_info.impl_kind.is_derived_from_blanket()
                        || impl_info.impl_kind.is_variable_constraint()
                })
            });
        let resolvable_by_name = !is_ability_impl && !ability_kind_is_specialized;

        let name = match impl_self_type {
            Some(target_type) => {
                use std::fmt::Write;
                let mut s = String::with_capacity(256);
                write!(
                    &mut s,
                    "{}_{}_{}",
                    self_.name_of(self_.get_ability(ability_id.unwrap()).name),
                    self_.type_id_to_string(target_type),
                    self_.name_of(parsed_function_name),
                )
                .unwrap();
                self_.ast.identifiers.intern(s)
            }
            None => parsed_function.name,
        };

        let fn_scope_id = self_.scopes.add_child_scope(
            parent_scope_id,
            ScopeType::FunctionScope,
            None,
            Some(name),
        );

        // Instantiate type arguments.
        let mut type_params: SmallVec<[TypeParam; 6]> =
            SmallVec::with_capacity(parsed_function_type_params.len());

        // Inject the 'Self' type parameter
        if is_ability_decl {
            let self_type_id = self_.get_ability(ability_id.unwrap()).self_type_id;
            type_params.push(TypeParam {
                name: get_ident!(self_, "Self"),
                type_id: self_type_id,
                span: parsed_function_span,
            })
        }
        for type_parameter in parsed_function_type_params.iter() {
            let mut ability_constraints = SmallVec::new();
            for parsed_constraint in type_parameter.constraints.iter() {
                let ability_sig = match parsed_constraint {
                    parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        self_.eval_ability_expr(ability_expr, false, fn_scope_id)?
                    }
                };
                ability_constraints.push(ability_sig);
            }
            for param in &parsed_function.additional_where_constraints {
                if param.name == type_parameter.name {
                    let ability_id = match &param.constraint_expr {
                        parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            self_.eval_ability_expr(ability_expr, false, fn_scope_id)?
                        }
                    };
                    ability_constraints.push(ability_id);
                }
            }
            let type_variable_id = self_.add_type_variable(
                TypeParameter {
                    name: type_parameter.name,
                    scope_id: fn_scope_id,
                    span: type_parameter.span,
                    function_type: None,
                },
                ability_constraints,
            );
            let fn_scope = self_.scopes.get_scope_mut(fn_scope_id);
            let type_param = TypeParam {
                name: type_parameter.name,
                type_id: type_variable_id,
                span: type_parameter.span,
            };
            type_params.push(type_param);
            if !fn_scope.add_type(type_parameter.name, type_variable_id) {
                return failf!(
                    type_parameter.span,
                    "Duplicate type variable name: {}",
                    type_parameter.name
                );
            }
        }

        // Process parameters
        let mut param_types: Vec<FnParamType> = Vec::with_capacity(parsed_function_params.len());
        let mut param_variables = Vec::with_capacity(parsed_function_params.len());
        for (idx, fn_param) in
            parsed_function_context_params.iter().chain(parsed_function_params.iter()).enumerate()
        {
            let type_id = self_.eval_type_expr(fn_param.ty, fn_scope_id)?;

            // If its a some ... function type parameter, inject the type parameter into the
            // function
            if let Type::TypeParameter(tp) = self_.types.get(type_id) {
                if let Some(_function_type) = tp.function_type {
                    type_params.push(TypeParam { name: tp.name, type_id, span: tp.span });
                    // There's actually no way to refer to these types by name,
                    // so we don't need to add a name to the scope
                }
            }

            // First arg Self shenanigans
            if idx == 0 {
                let name_is_self = self_.ast.identifiers.get_name(fn_param.name) == "self";

                // If the first argument is named self, check if it's a method of the companion type
                let is_ability_fn = ability_id.is_some();
                if name_is_self && !is_ability_fn {
                    if let Some(companion_type_id) = companion_type_id {
                        if self_.types.get_type_id_dereferenced(type_id) != companion_type_id {
                            match (
                                self_.types.get(companion_type_id),
                                self_.types.get_generic_instance_info(
                                    self_.types.get_type_id_dereferenced(type_id),
                                ),
                            ) {
                                (Type::Generic(_g), Some(spec_info)) => {
                                    let ok = spec_info.generic_parent == companion_type_id;
                                    if !ok {
                                        return failf!(
                                            fn_param.span,
                                            "First parameter named 'self' did not have a companion type",
                                        );
                                    }
                                }
                                _other => {
                                    return failf!(
                                        fn_param.span,
                                        "First parameter named 'self' must be of the companion type, expected {} got {}, {} vs {}",
                                        self_.type_id_to_string(companion_type_id),
                                        self_.type_id_to_string(type_id),
                                        companion_type_id,
                                        type_id
                                    );
                                }
                            }
                        }
                    } else {
                        return failf!(
                            fn_param.span,
                            "Cannot use name 'self' unless defining a method",
                        );
                    }
                };
            }

            let variable = Variable {
                name: fn_param.name,
                type_id,
                is_mutable: false,
                owner_scope: fn_scope_id,
                is_context: fn_param.modifiers.is_context(),
                constant_id: None,
            };

            let is_context = fn_param.modifiers.is_context();
            let variable_id = self_.variables.add_variable(variable);
            param_types.push(FnParamType {
                name: fn_param.name,
                type_id,
                is_context,
                is_lambda_env: false,
                span: fn_param.span,
            });
            param_variables.push(variable_id);
            if is_context {
                let inserted = self_.scopes.add_context_variable(
                    fn_scope_id,
                    fn_param.name,
                    variable_id,
                    type_id,
                );
                if !inserted {
                    return failf!(
                        fn_param.span,
                        "Duplicate context parameters for type {}",
                        self_.type_id_to_string(type_id)
                    );
                }
            } else {
                self_.scopes.add_variable(fn_scope_id, fn_param.name, variable_id)
            }
        }

        let intrinsic_type = if parsed_function_linkage == Linkage::Intrinsic {
            let mut namespace_chain = self_.namespaces.name_chain(namespace_id);
            let resolved = self_
                .resolve_intrinsic_function_type(
                    parsed_function_name,
                    namespace_chain.make_contiguous(),
                    ability_id.zip(impl_self_type),
                )
                .map_err(|msg| {
                    make_error(
                        format!("Error typechecking function: {}", msg),
                        parsed_function_span,
                    )
                })?;
            Some(resolved)
        } else {
            None
        };
        let return_type = self_.eval_type_expr(parsed_function.ret_type, fn_scope_id)?;

        // Typecheck 'main': It must take argc and argv of correct types, or nothing
        // And it must return an integer, or an Unwrap[Inner = i32]
        let is_main_fn =
            namespace_id == ROOT_NAMESPACE_ID && parsed_function_name == get_ident!(self_, "main");
        if is_main_fn {
            match param_types.len() {
                0 => {}
                2 => {
                    let count = param_types[0].type_id == U32_TYPE_ID;
                    let values = param_types[1].type_id == POINTER_TYPE_ID;
                    if !count {
                        return failf!(
                            param_types[0].span,
                            "First parameter must be {}",
                            self_.type_id_to_string(U32_TYPE_ID)
                        );
                    } else if !values {
                        return failf!(
                            param_types[1].span,
                            "Second parameter must be {}",
                            self_.type_id_to_string(POINTER_TYPE_ID)
                        );
                    }
                }
                n => {
                    return failf!(
                        param_types[0].span,
                        "start must take exactly 0 or 2 parameters, got {}",
                        n
                    )
                }
            };
            match return_type {
                I64_TYPE_ID => {}
                I32_TYPE_ID => {}
                _other => {
                    let result_impl = self_.expect_ability_implementation(
                        return_type,
                        UNWRAP_ABILITY_ID,
                        parsed_function_span,
                    )?;
                    let ok_type =
                        self_.get_ability_impl(result_impl.full_impl_id).impl_arguments[0].type_id;
                    if let Err(msg) = self_.check_types(I32_TYPE_ID, ok_type, fn_scope_id) {
                        return failf!(parsed_function_span, "Incorrect result type; {}", msg);
                    };
                }
            }
        };

        let kind = match ability_info.as_ref() {
            None => TypedFunctionKind::Standard,
            Some(ability_info) => match ability_info.impl_info.as_ref() {
                None => TypedFunctionKind::AbilityDefn(ability_info.ability_id),
                Some(impl_info) => match impl_info.impl_kind {
                    AbilityImplKind::Concrete
                    | AbilityImplKind::Blanket { .. }
                    | AbilityImplKind::VariableConstraint => TypedFunctionKind::AbilityImpl(
                        ability_info.ability_id,
                        impl_info.self_type_id,
                    ),
                    AbilityImplKind::DerivedFromBlanket { .. } => {
                        TypedFunctionKind::AbilityImplDerivedBlanket(
                            impl_info.blanket_parent_function.unwrap(),
                            ability_info.ability_id,
                            impl_info.self_type_id,
                        )
                    }
                },
            },
        };

        let function_type_id = self_.types.add_type(Type::Function(FunctionType {
            params: param_types,
            return_type,
            defn_info: Some(TypeDefnInfo {
                name,
                scope: parent_scope_id,
                companion_namespace: None,
                ast_id: parsed_function_id.into(),
            }),
        }));

        let function_id = self_.next_function_id();

        let actual_function_id = self_.add_function(TypedFunction {
            name,
            scope: fn_scope_id,
            param_variables,
            type_params,
            body_block: None,
            intrinsic_type,
            linkage: parsed_function_linkage,
            child_specializations: vec![],
            specialization_info: None,
            parsed_id: parsed_function_id.into(),
            kind,
            compiler_debug: is_debug,
            type_id: function_type_id,
            is_concrete: false,
        });
        debug_assert!(actual_function_id == function_id);

        if resolvable_by_name {
            if !self_.scopes.add_function(parent_scope_id, parsed_function_name, function_id) {
                let signature_span = parsed_function.signature_span;
                let error = errf!(
                    signature_span,
                    "Function name {} is taken",
                    self_.name_of(parsed_function_name)
                );
                self_.push_error(error);
            }
        };

        // In this case, we re-evaluate the ast-node for the ability specialization, so we expect
        // to run it more than once, and don't want to fail
        if !skip_ast_mapping {
            let existed =
                self_.function_ast_mappings.insert(parsed_function_id, function_id).is_some();
            debug_assert!(!existed);
        }

        self_.scopes.set_scope_owner_id(fn_scope_id, ScopeOwnerId::Function(function_id));

        if is_debug {
            eprintln!("DEBUG\n{}", self_.function_id_to_string(function_id, false));
            eprintln!("FUNCTION SCOPE\n{}", self_.scope_id_to_string(fn_scope_id));
        }

        Ok(function_id)
    }

    fn eval_function_body(&mut self, declaration_id: FunctionId) -> TyperResult<()> {
        let function = self.get_function(declaration_id);
        let is_debug = function.compiler_debug;
        if is_debug {
            self.push_debug_level();
        }
        let function = self.get_function(declaration_id);
        let function_name = function.name;
        let fn_scope_id = function.scope;
        let return_type = self.get_function_type(declaration_id).return_type;
        let is_extern = matches!(function.linkage, Linkage::External(_));
        let ast_id = function.parsed_id.as_function_id().expect("expected function id");
        let is_intrinsic = function.intrinsic_type.is_some();
        let is_ability_defn = matches!(function.kind, TypedFunctionKind::AbilityDefn(_));

        let parsed_function = self.ast.get_function(ast_id);
        let parsed_function_ret_type = parsed_function.ret_type;
        let function_signature_span = parsed_function.signature_span;

        let is_abstract = is_intrinsic || is_extern || is_ability_defn;

        let body_block = match parsed_function.block.as_ref() {
            None if is_abstract => None,
            None => return failf!(function_signature_span, "function is missing implementation"),
            Some(_) if is_abstract => {
                return failf!(function_signature_span, "unexpected function implementation")
            }
            Some(block_ast) => {
                if function.specialization_info.is_some() && !function.is_concrete {
                    debug!(
                        "Skipping typecheck of body for non-concrete specialization of {}",
                        self.function_id_to_string(declaration_id, true),
                    );
                    return Ok(());
                };
                // Note(clone): Intern blocks
                let block_ast = block_ast.clone();
                let block = self.eval_block(
                    &block_ast,
                    EvalExprContext::from_scope(fn_scope_id).with_expected_type(Some(return_type)),
                    true,
                )?;
                debug!(
                    "evaled function block with expected type {} and got type {}",
                    self.type_id_to_string(return_type),
                    self.type_id_to_string(block.expr_type)
                );
                if let Err(msg) = self.check_types(return_type, block.expr_type, fn_scope_id) {
                    let return_type_span =
                        self.ast.get_type_expression_span(parsed_function_ret_type);
                    return failf!(
                        return_type_span,
                        "Function {} return type mismatch: {}",
                        self.name_of(function_name),
                        msg
                    );
                } else {
                    Some(self.exprs.add(TypedExpr::Block(block)))
                }
            }
        };
        // Add the body now
        if let Some(body_block) = body_block {
            self.get_function_mut(declaration_id).body_block = Some(body_block);
        }
        if is_debug {
            eprintln!("DEBUG\n{}", self.function_id_to_string(declaration_id, true));
            self.pop_debug_level();
        }
        Ok(())
    }

    fn eval_ability(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
        scope_id: ScopeId,
    ) -> TyperResult<AbilityId> {
        if let Some(ability_id) = self.types.find_ability_mapping(parsed_ability_id) {
            return Ok(ability_id);
        }
        let parsed_ability = self.ast.get_ability(parsed_ability_id).clone();
        let parent_namespace_id =
            self.scopes.get_scope_owner(scope_id).unwrap().as_namespace().unwrap();
        let ability_scope_id = self.scopes.add_child_scope(
            scope_id,
            ScopeType::AbilityDefn,
            None,
            Some(parsed_ability.name),
        );

        let self_ident_id = get_ident!(self, "Self");
        let mut ability_params: Vec<TypedAbilityParam> =
            Vec::with_capacity(parsed_ability.params.len() + 1);
        let self_type_id = self.add_type_variable(
            TypeParameter {
                name: self_ident_id,
                scope_id: ability_scope_id,
                span: parsed_ability.span,
                function_type: None,
            },
            smallvec![],
        );
        let _ = self.scopes.get_scope_mut(ability_scope_id).add_type(self_ident_id, self_type_id);
        for ability_param in parsed_ability.params.clone().iter() {
            let ability_impls: TyperResult<SmallVec<[TypedAbilitySignature; 4]>> = ability_param
                .constraints
                .iter()
                .map(|constraint| match constraint {
                    parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        self.eval_ability_expr(ability_expr, false, ability_scope_id)
                    }
                })
                .collect();
            let ability_impls = ability_impls?;
            let param_type_id = self.add_type_variable(
                TypeParameter {
                    name: ability_param.name,
                    scope_id: ability_scope_id,
                    span: ability_param.span,
                    function_type: None,
                },
                ability_impls,
            );

            if !self
                .scopes
                .get_scope_mut(ability_scope_id)
                .add_type(ability_param.name, param_type_id)
            {
                return failf!(
                    ability_param.span,
                    "Duplicate type variable: {}",
                    self.name_of(ability_param.name)
                );
            };
            ability_params.push(TypedAbilityParam {
                name: ability_param.name,
                type_variable_id: param_type_id,
                is_impl_param: ability_param.is_impl_param,
                span: ability_param.span,
            })
        }
        let has_ability_side_params = ability_params.iter().any(|p| p.is_ability_side_param());
        let kind = if has_ability_side_params {
            TypedAbilityKind::Generic { specializations: vec![] }
        } else {
            TypedAbilityKind::Concrete
        };

        // Make a namespace for the ability
        let ability_namespace = Namespace {
            name: parsed_ability.name,
            scope_id: ability_scope_id,
            namespace_type: NamespaceType::Ability,
            companion_type_id: None,
            parent_id: Some(parent_namespace_id),
        };
        let namespace_id = self.namespaces.add(ability_namespace);
        let ns_added =
            self.scopes.get_scope_mut(scope_id).add_namespace(parsed_ability.name, namespace_id);
        if !ns_added {
            return failf!(
                parsed_ability.span,
                "Namespace with name {} already exists",
                self.name_of(parsed_ability.name)
            );
        }

        let typed_ability = TypedAbility {
            name: parsed_ability.name,
            self_type_id,
            parameters: ability_params,
            functions: Vec::new(),
            scope_id: ability_scope_id,
            ast_id: parsed_ability.id,
            namespace_id,
            kind,
        };
        let ability_id = self.add_ability(typed_ability);
        let added =
            self.scopes.get_scope_mut(scope_id).add_ability(parsed_ability.name, ability_id);
        if !added {
            return failf!(
                parsed_ability.span,
                "Ability with name {} already exists",
                self.name_of(parsed_ability.name)
            );
        }
        self.types.add_ability_mapping(parsed_ability_id, ability_id);
        self.scopes.set_scope_owner_id(ability_scope_id, ScopeOwnerId::Ability(ability_id));

        let mut typed_functions: Vec<TypedAbilityFunctionRef> =
            Vec::with_capacity(parsed_ability.functions.len());
        for parsed_function_id in parsed_ability.functions.iter() {
            let function_id = self.eval_function_declaration(
                *parsed_function_id,
                ability_scope_id,
                Some(FunctionAbilityContextInfo::ability_id_only(ability_id)),
                namespace_id,
            )?;
            let function_name = self.get_function(function_id).name;
            typed_functions.push(TypedAbilityFunctionRef { function_name, function_id });
        }
        self.abilities[ability_id.0 as usize].functions = typed_functions;
        Ok(ability_id)
    }

    fn find_ability_or_declare(
        &mut self,
        ability_name: &NamespacedIdentifier,
        scope_id: ScopeId,
    ) -> TyperResult<AbilityId> {
        let found_ability_id = self.scopes.find_ability_namespaced(
            scope_id,
            ability_name,
            &self.namespaces,
            &self.ast.identifiers,
        )?;
        found_ability_id.map(Ok).unwrap_or({
            match self.scopes.find_pending_ability(scope_id, ability_name.name) {
                None => {
                    failf!(
                        ability_name.span,
                        "No ability '{}' is in scope",
                        self.name_of(ability_name.name)
                    )
                }
                Some((pending_ability, ability_scope)) => {
                    let (line, _) = self.ast.get_lines_for_span_id(ability_name.span).unwrap();
                    debug!(
                        "Recursing into pending ability {} from {}",
                        self.name_of(ability_name.name),
                        line.content
                    );
                    let ability_id = self.eval_ability(pending_ability, ability_scope)?;
                    Ok(ability_id)
                }
            }
        })
    }

    fn eval_ability_impl_decl(
        &mut self,
        parsed_id: ParsedAbilityImplId,
        scope_id: ScopeId,
    ) -> TyperResult<AbilityImplId> {
        // TODO(clone): Very coarse clone of ast impl node
        let parsed_ability_impl = self.ast.get_ability_impl(parsed_id).clone();
        let span = parsed_ability_impl.span;
        let ability_expr = &parsed_ability_impl.ability_expr;
        let parsed_functions = &parsed_ability_impl.functions;

        let impl_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::AbilityImpl, None, None);

        let mut type_params = Vec::with_capacity(parsed_ability_impl.generic_impl_params.len());
        for generic_impl_param in &parsed_ability_impl.generic_impl_params {
            let type_variable_id = self.add_type_variable(
                TypeParameter {
                    name: generic_impl_param.name,
                    scope_id: impl_scope_id,
                    span: generic_impl_param.span,
                    function_type: None,
                },
                // We create the variable with no constraints, then add them later, so that its
                // constraints can reference itself
                // Example: impl[T] Add[Rhs = T where T: Num]
                // The constraints need T to exist
                smallvec![],
            );
            if !self
                .scopes
                .get_scope_mut(impl_scope_id)
                .add_type(generic_impl_param.name, type_variable_id)
            {
                return failf!(
                    generic_impl_param.span,
                    "Duplicate generic impl parameter name: {}",
                    self.name_of(generic_impl_param.name)
                );
            }

            if !generic_impl_param.constraints.is_empty() {
                let param_constraints_scope_id =
                    self.scopes.add_child_scope(impl_scope_id, ScopeType::AbilityImpl, None, None);
                for parsed_constraint in &generic_impl_param.constraints {
                    let constraint_ability_sig = match parsed_constraint {
                        parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            self.eval_ability_expr(ability_expr, false, impl_scope_id)?
                        }
                    };
                    self.add_constrained_ability_impl(
                        type_variable_id,
                        constraint_ability_sig,
                        param_constraints_scope_id,
                        parsed_constraint.span(),
                    )
                }
            }
            type_params.push(TypeParam {
                name: generic_impl_param.name,
                type_id: type_variable_id,
                span: generic_impl_param.span,
            });
        }

        let impl_self_type = self.eval_type_expr(parsed_ability_impl.self_type, impl_scope_id)?;
        let ability_sig = self.eval_ability_expr(ability_expr, true, impl_scope_id)?;
        let ability_id = ability_sig.ability_id;

        // Uniqueness of implementation:
        // We allow only one implementation per Ability (+ unique params set)
        // Check for existing implementation
        for existing_impl in &self.ability_impls {
            if existing_impl.ability_id == ability_id
                && existing_impl.self_type_id == impl_self_type
            {
                return failf!(
                    span,
                    "Ability '{}' already implemented for type: {}",
                    self.name_of(self.get_ability(ability_id).name).blue(),
                    self.type_id_to_string(impl_self_type).blue()
                );
            }
        }

        let ability = self.get_ability(ability_id).clone();
        let ability_name = ability.name;
        let ability_self_type = ability.self_type_id;
        let impl_scope_name = self.ast.identifiers.intern(format!(
            "{}_impl_{}",
            self.name_of(ability_name),
            self.type_id_to_string(impl_self_type)
        ));
        self.scopes.get_scope_mut(impl_scope_id).name = Some(impl_scope_name);
        // Bind 'Self' = target_type
        // Discarded because we just made this scope
        let _ = self
            .scopes
            .get_scope_mut(impl_scope_id)
            .add_type(get_ident!(self, "Self"), impl_self_type);

        // We also need to bind any ability parameters that this
        // ability is already specialized on; they aren't in our fresh scope
        for argument in ability.kind.arguments() {
            if !self.scopes.get_scope_mut(impl_scope_id).add_type(argument.name, argument.type_id) {
                return failf!(
                    span,
                    "Type parameter name {} is already used by an ability parameter",
                    self.name_of(argument.name)
                );
            }
        }

        let mut impl_arguments: SmallVec<[SimpleNamedType; 4]> =
            SmallVec::with_capacity(ability.parameters.len());
        for impl_param in ability.parameters.iter().filter(|p| p.is_impl_param) {
            let Some(matching_arg) =
                ability_expr.arguments.iter().find(|arg| arg.name == impl_param.name)
            else {
                return failf!(
                    ability_expr.span,
                    "Missing implementation-side parameter for Ability {}: {}",
                    self.name_of(ability_name),
                    self.name_of(impl_param.name)
                );
            };

            let arg_type = self.eval_type_expr(matching_arg.value, impl_scope_id)?;

            self.check_type_constraints(
                impl_param.name,
                impl_param.type_variable_id,
                arg_type,
                impl_scope_id,
                matching_arg.span,
            )?;

            debug!(
                "Binding impl param {} to {}",
                self.name_of(impl_param.name),
                self.type_id_to_string(arg_type)
            );
            let added =
                self.scopes.get_scope_mut(impl_scope_id).add_type(impl_param.name, arg_type);
            if !added {
                panic!("shit")
            }
            impl_arguments.push(SimpleNamedType { name: impl_param.name, type_id: arg_type })
        }

        let base_ability_id = self.get_ability_base(ability_id);
        let kind = if parsed_ability_impl.generic_impl_params.is_empty() {
            AbilityImplKind::Concrete
        } else {
            AbilityImplKind::Blanket { base_ability: base_ability_id, parsed_id }
        };

        let mut typed_functions = Vec::with_capacity(ability.functions.len());
        for ability_function_ref in &ability.functions {
            let Some((parsed_impl_function_id, impl_function_span)) =
                parsed_functions.iter().find_map(|&fn_id| {
                    let the_fn = self.ast.get_function(fn_id);
                    if the_fn.name == ability_function_ref.function_name {
                        Some((fn_id, the_fn.span))
                    } else {
                        None
                    }
                })
            else {
                return failf!(
                    span,
                    "Missing implementation for function '{}' in ability '{}'",
                    self.name_of(ability_function_ref.function_name).blue(),
                    self.name_of(ability_name).blue()
                );
            };
            // Report extra functions too
            for &parsed_fn in parsed_functions {
                let parsed_fn_name = self.ast.get_function(parsed_fn).name;
                let Some(_ability_function_ref) =
                    ability.functions.iter().find(|f| f.function_name == parsed_fn_name)
                else {
                    return failf!(
                        span,
                        "Extra function in ability impl: {}",
                        self.name_of(parsed_fn_name)
                    );
                };
            }

            let function_impl = self.eval_function_declaration(
                parsed_impl_function_id,
                impl_scope_id,
                Some(FunctionAbilityContextInfo::ability_impl(
                    ability_id,
                    impl_self_type,
                    kind,
                    None,
                )),
                // fixme: Root namespace?! A: namespace is only used for companion type stuff, so
                // this isn't doing any harm for now
                ROOT_NAMESPACE_ID,
            )?;

            let specialized = self.get_function(function_impl).type_id;

            let generic_type = self.get_function(ability_function_ref.function_id).type_id;

            // We check that the signature of the provided impl function matches
            // the signature of the generic function with target_type substituted for Self
            let substituted_root_type = self.substitute_in_type(
                generic_type,
                &[TypeSubstitutionPair { from: ability_self_type, to: impl_self_type }],
                None,
                None,
            );

            if let Err(msg) = self.check_types(substituted_root_type, specialized, impl_scope_id) {
                eprintln!("{}", self.scope_id_to_string(impl_scope_id));
                return failf!(
                    impl_function_span,
                    "Invalid implementation of {} in ability {}: {msg}",
                    self.ast.identifiers.get_name(ability_function_ref.function_name),
                    self.ast.identifiers.get_name(ability_name)
                );
            }
            typed_functions.push(function_impl);
        }

        let typed_impl_id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            type_params,
            self_type_id: impl_self_type,
            ability_id,
            impl_arguments,
            functions: typed_functions,
            scope_id: impl_scope_id,
            span,
            compile_errors: vec![],
        });

        if kind.is_blanket() {
            self.blanket_impls.entry(base_ability_id).or_default().push(typed_impl_id)
        }

        self.ability_impl_ast_mappings.insert(parsed_id, typed_impl_id);
        Ok(typed_impl_id)
    }

    /// All we have to do is fill in the function bodies; the prior phase has already done all
    /// the work
    fn eval_ability_impl(
        &mut self,
        parsed_ability_impl_id: ParsedAbilityImplId,
        _scope_id: ScopeId,
    ) -> TyperResult<()> {
        let ability_impl_id = *self.ability_impl_ast_mappings.get(&parsed_ability_impl_id).unwrap();
        let ability_impl = self.get_ability_impl(ability_impl_id);

        for impl_fn in ability_impl.functions.clone().iter() {
            if let Err(e) = self.eval_function_body(*impl_fn) {
                self.get_ability_impl_mut(ability_impl_id).compile_errors.push(e.clone());
                self.push_error(e);
            }
        }

        Ok(())
    }

    fn eval_definition(&mut self, def: ParsedId, scope_id: ScopeId) {
        match def {
            ParsedId::Use(parsed_use_id) => {
                if let Err(e) = self.eval_use_definition(scope_id, parsed_use_id) {
                    self.push_error(e)
                }
            }
            ParsedId::Namespace(namespace) => {
                if let Err(e) = self.eval_namespace(namespace) {
                    self.push_error(e);
                };
            }
            ParsedId::Constant(_const_val) => {
                // Nothing to do in this phase for a const
            }
            ParsedId::Function(parsed_function_id) => {
                let function_declaration_id = self
                    .function_ast_mappings
                    .get(&parsed_function_id)
                    .expect("function predecl lookup failed");
                if let Err(e) = self.eval_function_body(*function_declaration_id) {
                    self.push_error(e);
                };
            }
            ParsedId::TypeDefn(_type_defn_id) => {
                // Done in prior phase
            }
            ParsedId::Ability(_ability) => {
                // Nothing to do in this phase for an ability
            }
            ParsedId::AbilityImpl(ability_impl) => {
                if let Err(e) = self.eval_ability_impl(ability_impl, scope_id) {
                    self.push_error(e);
                };
            }
            other_id => {
                panic!("Was asked to eval definition of a non-definition ast node {:?}", other_id)
            }
        }
    }

    fn eval_use_definition(
        &mut self,
        scope_id: ScopeId,
        parsed_use_id: ParsedUseId,
    ) -> TyperResult<()> {
        let parsed_use = self.ast.uses.get_use(parsed_use_id);
        let status_entry = self.use_statuses.get(&parsed_use_id);
        let is_fulfilled = match status_entry {
            Some(se) if se.is_resolved() => true,
            _ => false,
        };
        if !is_fulfilled {
            eprintln!(
                "Handling unfulfilled use {}",
                self.namespaced_identifier_to_string(&parsed_use.target)
            );
            if let Some(symbol) = self.scopes.find_useable_symbol(
                scope_id,
                &parsed_use.target,
                &self.namespaces,
                &self.ast.identifiers,
            )? {
                self.scopes.add_use_binding(
                    scope_id,
                    symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                self.use_statuses.insert(parsed_use_id, UseStatus::Resolved(symbol));
                eprintln!("Inserting resolved use");
            } else {
                self.use_statuses.insert(parsed_use_id, UseStatus::Unresolved);
                eprintln!("Inserting unresolved use");
            }
        }
        Ok(())
    }

    // Evaluate a namespace during the Type Declaration phase:
    // This means finding all the type declarations in the namespace and registering their names,
    // then recursing down into child namespaces and doing the same
    fn eval_namespace_type_decl_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<()> {
        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;
        for &parsed_definition_id in
            self.ast.get_namespace(parsed_namespace_id).definitions.clone().iter()
        {
            if let ParsedId::Use(parsed_use_id) = parsed_definition_id {
                if let Err(e) = self.eval_use_definition(namespace_scope_id, parsed_use_id) {
                    self.push_error(e);
                }
            }
            if let ParsedId::TypeDefn(type_defn_id) = parsed_definition_id {
                let parsed_type_defn = self.ast.get_type_defn(type_defn_id);
                let name = parsed_type_defn.name;
                let span = parsed_type_defn.span;
                let added = self
                    .scopes
                    .get_scope_mut(namespace_scope_id)
                    .add_pending_type_defn(name, type_defn_id);
                if !added {
                    self.push_error(errf!(span, "Type {} exists", self.name_of(name)));
                }
            }
            if let ParsedId::Ability(parsed_ability_id) = parsed_definition_id {
                let parsed_ability_defn = self.ast.get_ability(parsed_ability_id);
                let name = parsed_ability_defn.name;
                let span = parsed_ability_defn.span;
                let added = self
                    .scopes
                    .get_scope_mut(namespace_scope_id)
                    .add_pending_ability_defn(name, parsed_ability_id);
                if !added {
                    self.push_error(errf!(span, "Ability {} exists", self.name_of(name)));
                }
            }
            if let ParsedId::Namespace(namespace_id) = parsed_definition_id {
                if let Err(e) = self.eval_namespace_type_decl_phase(namespace_id) {
                    self.push_error(e);
                }
            }
        }
        Ok(())
    }

    fn eval_namespace_type_eval_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<()> {
        let namespace_id = self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace = self.namespaces.get(*namespace_id);
        let namespace_scope_id = namespace.scope_id;
        let parsed_namespace = self.ast.get_namespace(parsed_namespace_id);

        for parsed_definition_id in parsed_namespace.definitions.clone().iter() {
            if let ParsedId::TypeDefn(type_defn_id) = parsed_definition_id {
                self.eval_type_defn(*type_defn_id, namespace_scope_id)?;
            }
            if let ParsedId::Namespace(namespace_id) = parsed_definition_id {
                self.eval_namespace_type_eval_phase(*namespace_id)?;
            }
        }
        Ok(())
    }

    fn eval_namespace_declaration_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<()> {
        let parsed_namespace = self.ast.get_namespace(parsed_namespace_id);
        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace = self.namespaces.get(namespace_id);
        let namespace_scope_id = namespace.scope_id;
        for defn in &parsed_namespace.definitions.clone() {
            self.eval_definition_declaration_phase(*defn, namespace_scope_id, namespace_id)?;
        }
        Ok(())
    }

    fn eval_namespace(&mut self, ast_namespace_id: ParsedNamespaceId) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.get_namespace(ast_namespace_id).clone();
        let namespace_id = *self.namespace_ast_mappings.get(&ast_namespace.id).unwrap();
        let ns_scope_id = self.namespaces.get(namespace_id).scope_id;
        for defn in &ast_namespace.definitions {
            self.eval_definition(*defn, ns_scope_id);
        }
        Ok(namespace_id)
    }

    fn eval_definition_declaration_phase(
        &mut self,
        defn_id: ParsedId,
        scope_id: ScopeId,
        namespace_id: NamespaceId,
    ) -> TyperResult<()> {
        match defn_id {
            ParsedId::Use(_use_id) => Ok(()),
            ParsedId::Namespace(namespace_id) => {
                self.eval_namespace_declaration_phase(namespace_id)?;
                Ok(())
            }
            ParsedId::Constant(constant_id) => {
                let _variable_id: VariableId = self.eval_constant(constant_id, scope_id)?;
                Ok(())
            }
            ParsedId::Function(parsed_function_id) => {
                self.eval_function_declaration(parsed_function_id, scope_id, None, namespace_id)?;
                Ok(())
            }
            ParsedId::TypeDefn(_type_defn_id) => {
                // Handled by prior phase
                Ok(())
            }
            ParsedId::Ability(parsed_ability_id) => {
                self.eval_ability(parsed_ability_id, scope_id)?;
                Ok(())
            }
            ParsedId::AbilityImpl(ability_impl) => {
                let _impl_id = self.eval_ability_impl_decl(ability_impl, scope_id)?;
                Ok(())
            }
            other_id => {
                panic!("Was asked to eval definition of a non-definition ast node {:?}", other_id)
            }
        }
    }

    fn create_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: Option<ScopeId>,
    ) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.get_namespace(parsed_namespace_id);
        let name = ast_namespace.name;
        let span = ast_namespace.span;

        match parent_scope {
            None => {
                let root_scope_id = self.scopes.add_root_scope(Some(name));
                let namespace = Namespace {
                    name,
                    scope_id: root_scope_id,
                    namespace_type: NamespaceType::Root,
                    companion_type_id: None,
                    parent_id: None,
                };
                let root_namespace_id = self.namespaces.add(namespace);
                self.scopes
                    .set_scope_owner_id(root_scope_id, ScopeOwnerId::Namespace(root_namespace_id));

                // Add _root ns to the root scope as well so users can use it
                let root_scope = self.scopes.get_scope_mut(root_scope_id);
                if !root_scope.add_namespace(name, root_namespace_id) {
                    return failf!(span, "Root namespace was taken, hmmmm");
                }

                self.namespace_ast_mappings.insert(parsed_namespace_id, root_namespace_id);
                Ok(root_namespace_id)
            }
            Some(parent_scope_id) => {
                let ns_scope_id = self.scopes.add_child_scope(
                    parent_scope_id,
                    ScopeType::Namespace,
                    None,
                    Some(name),
                );
                let parent_ns_id = self
                    .scopes
                    .get_scope_owner(parent_scope_id)
                    .and_then(|owner| owner.as_namespace())
                    .expect("namespace must be defined directly inside another namespace");

                let namespace = Namespace {
                    name,
                    scope_id: ns_scope_id,
                    namespace_type: NamespaceType::User,
                    companion_type_id: None,
                    parent_id: Some(parent_ns_id),
                };
                let namespace_id = self.namespaces.add(namespace);
                self.scopes.set_scope_owner_id(ns_scope_id, ScopeOwnerId::Namespace(namespace_id));

                let parent_scope = self.scopes.get_scope_mut(parent_scope_id);
                if !parent_scope.add_namespace(name, namespace_id) {
                    return failf!(span, "Namespace name {} is taken", self.name_of(name).blue());
                }

                self.namespace_ast_mappings.insert(parsed_namespace_id, namespace_id);
                Ok(namespace_id)
            }
        }
    }

    fn eval_namespace_namespace_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: Option<ScopeId>,
    ) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.get_namespace(parsed_namespace_id).clone();

        // Detect extension case, which cannot happen for root
        let namespace_id = if let Some(parent_scope) = parent_scope {
            if let Some(existing) = self.scopes.find_namespace(parent_scope, ast_namespace.name) {
                // Map this separate namespace AST node to the same semantic namespace
                self.namespace_ast_mappings.insert(parsed_namespace_id, existing);
                eprintln!(
                    "Inserting re-definition node for ns {}",
                    self.name_of(ast_namespace.name)
                );
                existing
            } else {
                self.create_namespace(parsed_namespace_id, Some(parent_scope))?
            }
        } else {
            self.create_namespace(parsed_namespace_id, parent_scope)?
        };

        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;

        for defn in &ast_namespace.definitions {
            if let ParsedId::Namespace(namespace_id) = defn {
                let _namespace_id =
                    self.eval_namespace_namespace_phase(*namespace_id, Some(namespace_scope_id))?;
            }
        }
        Ok(namespace_id)
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        let root_scope_id = self.scopes.get_root_scope_id();

        let root_namespace_id = self.ast.get_root_namespace().id;

        let mut err_writer = stderr();

        // Namespace phase
        eprintln!(">> Phase 1 declare namespaces");
        let ns_phase_res = self.eval_namespace_namespace_phase(root_namespace_id, None);
        if let Err(e) = ns_phase_res {
            self.write_error(&mut err_writer, &e)?;
            self.errors.push(e);
        }
        if !self.errors.is_empty() {
            bail!(
                "{} failed namespace declaration phase with {} errors",
                self.name(),
                self.errors.len()
            )
        }

        // Pending Type declaration phase
        eprintln!(">> Phase 2 declare types");
        let type_defn_result = self.eval_namespace_type_decl_phase(root_namespace_id);
        if let Err(e) = type_defn_result {
            self.write_error(&mut err_writer, &e)?;
            self.errors.push(e);
        }
        if !self.errors.is_empty() {
            bail!("{} failed type definition phase with {} errors", self.name(), self.errors.len())
        }
        // Type evaluation phase
        eprintln!(">> Phase 3 evaluate types");
        let type_eval_result = self.eval_namespace_type_eval_phase(root_namespace_id);
        if let Err(e) = type_eval_result {
            self.write_error(&mut err_writer, &e)?;
            self.errors.push(e);
        }
        if !self.errors.is_empty() {
            bail!("{} failed type evaluation phase with {} errors", self.name(), self.errors.len())
        }
        let pendings = self.scopes.all_pending_type_defns_below(self.scopes.get_root_scope_id());
        if !pendings.is_empty() {
            for pending in pendings.iter() {
                let defn = self.ast.get_type_defn(*pending);
                dbg!(self.name_of(defn.name));
            }
            panic!("Unevaluated type defns!!!")
        }

        //
        // This just ensures our BUFFER_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let buffer_generic = self.types.get(BUFFER_TYPE_ID).expect_generic();
            let buffer_struct = self.types.get(buffer_generic.inner).expect_struct();
            debug_assert!(buffer_generic.type_defn_info.scope == self.scopes.get_root_scope_id());
            debug_assert!(buffer_generic.type_defn_info.name == get_ident!(self, "Buffer"));
            debug_assert!(buffer_struct.fields.len() == 2);
            debug_assert!(
                buffer_struct.fields.iter().map(|f| self.name_of(f.name)).collect::<Vec<_>>()
                    == vec!["len", BUFFER_DATA_FIELD_NAME]
            );
        }

        // This just ensures our LIST_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let list_generic = self.types.get(LIST_TYPE_ID).expect_generic();
            let list_struct = self.types.get(list_generic.inner).expect_struct();
            debug_assert!(list_generic.type_defn_info.scope == self.scopes.get_root_scope_id());
            debug_assert!(list_generic.type_defn_info.name == get_ident!(self, "List"));
            debug_assert!(
                list_struct.fields.iter().map(|f| self.name_of(f.name)).collect::<Vec<_>>()
                    == vec!["len", "buffer"]
            );
        }

        // This just ensures our STRING_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let string_struct = self.types.get(STRING_TYPE_ID).expect_struct();
            debug_assert!(
                string_struct.type_defn_info.as_ref().unwrap().scope
                    == self.scopes.get_root_scope_id()
            );
            debug_assert!(
                string_struct.type_defn_info.as_ref().unwrap().name == get_ident!(self, "string")
            );
            debug_assert!(string_struct.fields.len() == 1);
        }

        // This just ensures our OPTIONAL_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let optional_generic = self.types.get(OPTIONAL_TYPE_ID).expect_generic();
            let inner = self.types.get(optional_generic.inner);
            debug_assert!(optional_generic.type_defn_info.scope == self.scopes.get_root_scope_id());
            debug_assert!(optional_generic.type_defn_info.name == get_ident!(self, "Opt"));
            debug_assert!(inner.as_enum().unwrap().variants.len() == 2);
        }
        //
        // This just ensures our ORDERING_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let ordering_enum = self.types.get(ORDERING_TYPE_ID).expect_enum();
            debug_assert!(ordering_enum.variants.len() == 3);
            debug_assert!(
                ordering_enum.type_defn_info.as_ref().unwrap().name == get_ident!(self, "Ordering")
            );
        }

        // Everything else declaration phase
        let root_ns_id = NamespaceId(NonZeroU32::new(1).unwrap());
        eprintln!(">> Phase 4 declare rest of definitions (functions, constants, abilities)");
        for &parsed_definition_id in self.ast.get_root_namespace().definitions.clone().iter() {
            let result = self.eval_definition_declaration_phase(
                parsed_definition_id,
                root_scope_id,
                root_ns_id,
            );
            if let Err(e) = result {
                self.write_error(&mut err_writer, &e)?;
                self.errors.push(e);
            }
        }
        if !self.errors.is_empty() {
            bail!("{} failed declaration phase with {} errors", self.name(), self.errors.len())
        }

        debug_assert!(self.get_ability(EQUALS_ABILITY_ID).name == get_ident!(self, "Equals"));
        debug_assert!(self.get_ability(BITWISE_ABILITY_ID).name == get_ident!(self, "Bitwise"));
        debug_assert!(
            self.get_ability(COMPARABLE_ABILITY_ID).name == get_ident!(self, "Comparable")
        );

        // Everything else evaluation phase
        eprintln!(">> Phase 5 evaluate rest of definitions (functions, constants, abilities)");
        for &parsed_definition_id in self.ast.get_root_namespace().definitions.clone().iter() {
            self.eval_definition(parsed_definition_id, root_scope_id);
        }
        let unresolved_uses: Vec<_> =
            self.use_statuses.iter().filter(|use_status| !use_status.1.is_resolved()).collect();
        if !unresolved_uses.is_empty() {
            for (parsed_use_id, _status) in &unresolved_uses {
                let parsed_use = self.ast.uses.get_use(**parsed_use_id);
                let error = errf!(
                    parsed_use.span,
                    "Unresolved use of {}",
                    self.name_of(parsed_use.target.name)
                );
                self.write_error(&mut err_writer, &error)?;
                self.errors.push(error)
            }
        }
        if !self.errors.is_empty() {
            bail!("Module {} failed typechecking with {} errors", self.name(), self.errors.len())
        }

        eprintln!(">> Phase 6 specialize function bodies");
        while !self.functions_pending_body_specialization.is_empty() {
            let clone = self.functions_pending_body_specialization.clone();
            self.functions_pending_body_specialization.clear();
            for function_id in &clone {
                let result = self.specialize_function_body(*function_id);
                if let Err(e) = result {
                    self.write_error(&mut err_writer, &e)?;
                    self.errors.push(e);
                }
            }
        }
        if !self.errors.is_empty() {
            bail!("{} failed specialize with {} errors", self.name(), self.errors.len())
        }

        Ok(())
    }

    pub fn get_span_for_type_id(&self, type_id: TypeId) -> Option<SpanId> {
        let t = self.types.get(type_id);
        match t {
            Type::TypeParameter(tv) => Some(tv.span),
            t => t.ast_node().map(|parsed_id| self.ast.get_span_for_id(parsed_id)),
        }
    }

    fn generate_constructors_for_type(
        &self,
        type_id: TypeId,
        _span_id: SpanId,
    ) -> Vec<PatternConstructor> {
        if type_id == STRING_TYPE_ID {
            return vec![PatternConstructor::String];
        }
        match self.types.get(type_id) {
            Type::Unit(_) => vec![PatternConstructor::Unit],
            Type::Char(_) => vec![PatternConstructor::Char],
            Type::TypeParameter(_) => vec![PatternConstructor::TypeVariable],
            Type::Integer(_) => vec![PatternConstructor::Int],
            Type::Float(_) => vec![PatternConstructor::Float],
            Type::Bool(_) => {
                vec![PatternConstructor::BoolFalse, PatternConstructor::BoolTrue]
            }
            Type::Reference(refer) => {
                let inner = self.generate_constructors_for_type(refer.inner_type, _span_id);
                inner
                    .into_iter()
                    .map(|pointee_pattern| PatternConstructor::Reference(Box::new(pointee_pattern)))
                    .collect()
            }
            Type::Enum(enum_type) => enum_type
                .variants
                .iter()
                .flat_map(|v| match v.payload.as_ref() {
                    None => {
                        vec![PatternConstructor::Enum { variant_name: v.name, inner: None }]
                    }
                    Some(payload) => self
                        .generate_constructors_for_type(*payload, _span_id)
                        .into_iter()
                        .map(|inner| PatternConstructor::Enum {
                            variant_name: v.name,
                            inner: Some(Box::new(inner)),
                        })
                        .collect(),
                })
                .collect(),
            Type::Struct(struc) => {
                debug_assert!(type_id != STRING_TYPE_ID);
                let mut all_field_ctors: Vec<Vec<(Identifier, PatternConstructor)>> = vec![];
                for field in struc.fields.iter() {
                    let field_ctors_iter = self
                        .generate_constructors_for_type(field.type_id, _span_id)
                        .into_iter()
                        .map(|pat| (field.name, pat))
                        .collect::<Vec<_>>();
                    all_field_ctors.push(field_ctors_iter)
                }
                // Generate cross product of all field combinations
                let mut result = vec![PatternConstructor::Struct { fields: Vec::new() }];
                // For each individual field's constructors, we iterate over all previously accumulated results
                // Pushing this field's constructors onto each previous results' field vec
                for field_ctors in all_field_ctors.into_iter() {
                    let mut new_result = Vec::new();
                    for ctor in result {
                        for (field_name, field_ctor) in &field_ctors {
                            if let PatternConstructor::Struct { mut fields } = ctor.clone() {
                                fields.push((*field_name, field_ctor.clone()));
                                new_result.push(PatternConstructor::Struct { fields });
                            }
                        }
                    }
                    result = new_result;
                }
                result
            }
            _ => {
                eprintln!("unhandled match type {}", self.type_id_to_string(type_id));
                vec![]
            }
        }
    }

    fn pattern_matches(pattern: &TypedPattern, ctor: &PatternConstructor) -> bool {
        match (pattern, ctor) {
            (TypedPattern::Wildcard(_), _) => true,
            (TypedPattern::Variable(_), _) => true,
            (TypedPattern::LiteralUnit(_), PatternConstructor::Unit) => true,
            (TypedPattern::LiteralBool(true, _), PatternConstructor::BoolTrue) => true,
            (TypedPattern::LiteralBool(false, _), PatternConstructor::BoolFalse) => true,
            (TypedPattern::Enum(enum_pat), PatternConstructor::Enum { variant_name, inner }) => {
                if *variant_name == enum_pat.variant_tag_name {
                    match (enum_pat.payload.as_ref(), inner) {
                        (Some(payload), Some(inner)) => {
                            TypedModule::pattern_matches(payload, inner)
                        }
                        (None, None) => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            (TypedPattern::Struct(struc), PatternConstructor::Struct { fields }) => {
                // Because we treat all struct patterns as caring only about the fields they mention,
                // an empty pattern already matches. So we iterate over the fields this pattern does
                // care about, and if any do not match, we'll consider the whole pattern not to match
                let mut matches = true;
                for field_pattern in struc.fields.iter() {
                    let matching_field_pattern = fields
                        .iter()
                        .find(|(name, _ctor_pattern)| *name == field_pattern.name)
                        .map(|(_, ctor_pattern)| ctor_pattern)
                        .expect("Field not in struct; pattern should have failed typecheck by now");
                    if !TypedModule::pattern_matches(&field_pattern.pattern, matching_field_pattern)
                    {
                        matches = false;
                        break;
                    }
                }
                matches
            }
            _ => {
                // eprintln!("Unhandled pattern_matches case: {:?} {:?}", pattern, ctor);
                false
            }
        }
    }

    /******************************
     ** Synthesis of Typed nodes **
     *****************************/

    fn synth_optional_type(&mut self, inner_type: TypeId) -> TypeId {
        self.instantiate_generic_type(OPTIONAL_TYPE_ID, vec![inner_type])
    }

    fn synth_optional_some(&mut self, expression: TypedExpr) -> (TypedExprId, TypeId) {
        let optional_type = self.synth_optional_type(expression.get_type());
        let span = expression.get_span();
        let expr_id = self.exprs.add(expression);
        let some_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(get_ident!(self, "Some"))
            .unwrap();

        let id = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: some_variant.enum_type_id,
            variant_name: some_variant.name,
            variant_index: some_variant.index,
            span,
            payload: Some(expr_id),
        }));
        (id, optional_type)
    }

    fn synth_optional_none(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, vec![type_id]);
        let none_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(get_ident!(self, "None"))
            .unwrap();
        self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: none_variant.enum_type_id,
            variant_name: none_variant.name,
            variant_index: none_variant.index,
            span,
            payload: None,
        }))
    }

    fn synth_equals_binop(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
            kind: BinaryOpKind::Equals,
            ty: BOOL_TYPE_ID,
            span,
            lhs,
            rhs,
        }))
    }

    fn synth_dereference(&mut self, base: TypedExprId) -> TypedExprId {
        let base_expr = self.exprs.get(base);
        let span = base_expr.get_span();
        let type_id = self.types.get(base_expr.get_type()).expect_reference().inner_type;
        self.exprs.add(TypedExpr::UnaryOp(UnaryOp {
            kind: UnaryOpKind::Dereference,
            type_id,
            span,
            expr: base,
        }))
    }

    fn synth_block(&mut self, parent_scope: ScopeId, span: SpanId) -> TypedBlock {
        let block_scope_id =
            self.scopes.add_child_scope(parent_scope, ScopeType::LexicalBlock, None, None);
        TypedBlock { expr_type: UNIT_TYPE_ID, statements: vec![], scope_id: block_scope_id, span }
    }

    /// Creates a non-mutable, mangled, non-referencing variable defn.
    /// This is the vastly most common case
    fn synth_variable_defn_simple(
        &mut self,
        name: Identifier,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, false, false, false, owner_scope)
    }

    /// Creates a user-code-visible variable
    fn synth_variable_defn_visible(
        &mut self,
        name: Identifier,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, true, false, false, owner_scope)
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    fn synth_variable_defn(
        &mut self,
        name: Identifier,
        initializer_id: TypedExprId,
        no_mangle: bool,
        is_mutable: bool,
        is_referencing: bool,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        let initializer = self.exprs.get(initializer_id);
        let initializer_type = initializer.get_type();
        let span = initializer.get_span();
        let type_id = if is_referencing {
            self.types.add_reference_type(initializer_type)
        } else {
            initializer_type
        };
        let new_ident = if no_mangle {
            name
        } else {
            // TODO(perf): this registers on flamegraph
            let new_ident_name = format!(
                "__{}_{}",
                self.ast.identifiers.get_name(name),
                self.variables.variables.len()
            );
            self.ast.identifiers.intern(new_ident_name)
        };
        let variable = Variable {
            name: new_ident,
            is_mutable,
            owner_scope,
            type_id,
            is_context: false,
            constant_id: None,
        };
        let variable_id = self.variables.add_variable(variable);
        let variable_expr =
            self.exprs.add(TypedExpr::Variable(VariableExpr { type_id, variable_id, span }));
        let defn_stmt = self.stmts.add(TypedStmt::Let(LetStmt {
            variable_id,
            variable_type: type_id,
            initializer: initializer_id,
            is_referencing,
            span,
        }));
        let parsed_expr =
            self.ast.exprs.add_expression(ParsedExpression::Variable(parse::Variable {
                name: NamespacedIdentifier::naked(name, span),
            }));
        self.scopes.add_variable(owner_scope, new_ident, variable_id);
        SynthedVariable { variable_id, defn_stmt, variable_expr, parsed_expr }
    }

    fn synth_parsed_function_call(
        &mut self,
        name: NamespacedIdentifier,
        type_args: Vec<ParsedTypeExprId>,
        args: Vec<ParsedExpressionId>,
    ) -> ParsedExpressionId {
        let span = name.span;
        let type_args = type_args.iter().map(|id| NamedTypeArg::unnamed(*id)).collect();
        let args = args.iter().map(|id| parse::FnCallArg::unnamed(*id)).collect();
        self.ast.exprs.add_expression(ParsedExpression::FnCall(FnCall {
            name,
            type_args,
            args,
            explicit_context_args: vec![],
            span,
            is_method: false,
            id: ParsedExpressionId::PENDING,
        }))
    }

    fn synth_typed_function_call(
        &mut self,
        name: NamespacedIdentifier,
        type_args: &[TypeId],
        args: &[TypedExprId],
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let call_id = self.synth_parsed_function_call(name, vec![], vec![]);
        let call = self.ast.exprs.get(call_id).expect_fn_call().clone();
        self.eval_function_call(&call, Some((type_args, args)), ctx)
    }

    // These are only used by the old coalescing accessor and should be removed when its rebuilt
    fn ident_opt_has_value(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "isSome")
    }

    fn ident_opt_get(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "get")
    }

    #[allow(unused)]
    fn synth_type_of_expr(&mut self, expr: ParsedExpressionId) -> ParsedTypeExprId {
        let span = self.ast.exprs.get_span(expr);
        self.ast
            .type_exprs
            .add(ParsedTypeExpr::TypeOf(parse::ParsedTypeOf { target_expr: expr, span }))
    }

    fn synth_binary_bool_op(
        &mut self,
        kind: BinaryOpKind,
        lhs: TypedExprId,
        rhs: TypedExprId,
    ) -> TypedExprId {
        let lhs_span = self.exprs.get(lhs).get_span();
        let rhs_span = self.exprs.get(rhs).get_span();
        let span = self.ast.spans.extend(lhs_span, rhs_span);
        self.exprs.add(TypedExpr::BinaryOp(BinaryOp { kind, ty: BOOL_TYPE_ID, lhs, rhs, span }))
    }

    fn synth_parsed_bool_not(&mut self, base: ParsedExpressionId) -> ParsedExpressionId {
        let span = self.ast.exprs.get_span(base);
        self.synth_parsed_function_call(
            qident!(self, span, ["bool"], "negated"),
            vec![],
            vec![base],
        )
    }

    fn synth_show_ident_call(
        &mut self,
        caller: ParsedExpressionId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.ast.exprs.get_span(caller);
        let call_id = self.synth_parsed_function_call(
            qident!(self, span, ["Show"], "show"),
            vec![],
            vec![caller],
        );
        let call = self.ast.exprs.get(call_id).expect_fn_call().clone();
        self.eval_function_call(&call, None, ctx)
    }

    fn synth_struct_expr(
        &mut self,
        struct_type_id: TypeId,
        field_exprs: Vec<TypedExprId>,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let struct_type = self.types.get(struct_type_id).expect_struct();
        if struct_type.fields.len() != field_exprs.len() {
            panic!("Mismatching field count: {} / {}", struct_type.fields.len(), field_exprs.len());
        }
        let mut fields: Vec<StructField> = Vec::with_capacity(struct_type.fields.len());
        for (index, field_expr) in field_exprs.into_iter().enumerate() {
            let field = &struct_type.fields[index];
            #[cfg(debug_assertions)]
            {
                let field_expr_type = self.exprs.get(field_expr).get_type();
                if let Err(msg) = self.check_types(field.type_id, field_expr_type, scope_id) {
                    panic!("synthed struct fields failed typechecking: {}", msg)
                }
            }
            fields.push(StructField { name: field.name, expr: field_expr });
        }
        self.exprs.add(TypedExpr::Struct(Struct { fields, type_id: struct_type_id, span }))
    }

    fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get_source(the_span.file_id);
        let line = source.get_line_for_span_start(the_span).unwrap();
        let struct_expr = TypedExpr::Struct(Struct {
            fields: vec![
                StructField {
                    name: get_ident!(self, "filename"),
                    expr: self.exprs.add(TypedExpr::Str(source.filename.clone(), span)),
                },
                StructField {
                    name: get_ident!(self, "line"),
                    expr: self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                        value: TypedIntegerValue::U64(line.line_number() as u64),
                        span,
                    })),
                },
            ],
            type_id: COMPILER_SOURCE_LOC_TYPE_ID,
            span,
        });
        self.exprs.add(struct_expr)
    }

    fn synth_crash_call(
        &mut self,
        message: String,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let message_expr = self.exprs.add(TypedExpr::Str(message, span));
        self.synth_typed_function_call(qident!(self, span, "crash"), &[], &[message_expr], ctx)
    }

    fn synth_discard_call(
        &mut self,
        value: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.exprs.get(value).get_span();
        self.synth_typed_function_call(qident!(self, span, "discard"), &[], &[value], ctx)
    }

    pub fn push_error(&mut self, e: TyperError) {
        self.write_error(&mut std::io::stderr(), &e).unwrap();
        self.errors.push(e);
    }

    pub fn write_qualified_name(
        &self,
        w: &mut impl std::io::Write,
        scope: ScopeId,
        name: &str,
        delimiter: &str,
        skip_root: bool,
    ) {
        let starting_namespace = self.scopes.nearest_parent_namespace(scope);
        let namespace_chain = self.namespaces.name_chain(starting_namespace);
        for identifier in namespace_chain.iter() {
            let ident_str = self.name_of(*identifier);

            let is_root = ident_str == "_root";
            if !(skip_root && is_root) {
                write!(w, "{ident_str}").unwrap();
                write!(w, "{delimiter}").unwrap();
            }
        }
        write!(w, "{}", name).unwrap();
    }

    pub fn make_qualified_name(
        &self,
        scope: ScopeId,
        name: Identifier,
        delimiter: &str,
        skip_root: bool,
    ) -> String {
        let mut buf = Vec::with_capacity(64);
        self.write_qualified_name(&mut buf, scope, self.name_of(name), delimiter, skip_root);
        String::from_utf8(buf).unwrap()
    }

    pub fn write_error(
        &self,
        w: &mut impl std::io::Write,
        error: &TyperError,
    ) -> std::io::Result<()> {
        write_error(w, &self.ast.spans, &self.ast.sources, &error.message, error.level, error.span)
    }

    pub fn write_location(&self, w: &mut impl std::io::Write, span: SpanId) -> std::io::Result<()> {
        parse::write_error_location(w, &self.ast.spans, &self.ast.sources, span, ErrorLevel::Error)
    }

    pub fn ice(&self, msg: impl AsRef<str>, error: Option<&TyperError>) -> ! {
        if let Some(error) = error {
            self.write_error(&mut std::io::stderr(), error).unwrap();
        }
        panic!("Internal Compiler Error: {}", msg.as_ref())
    }
}
