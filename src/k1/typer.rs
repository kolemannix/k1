pub mod derive;
pub mod dump;
pub mod scopes;
pub mod types;

use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::stderr;

use ahash::HashMapExt;
use anyhow::bail;
use colored::Colorize;
use either::Either;
use fxhash::FxHashMap;
use log::{debug, trace};

use scopes::*;
use types::*;

use crate::lex::{SpanId, Spans, TokenKind};
use crate::parse::{
    self, DirectiveKind, ForExpr, ForExprType, Identifiers, IfExpr, NamedTypeArg,
    NamespacedIdentifier, NumericWidth, ParsedAbilityId, ParsedAbilityImplId, ParsedConstantId,
    ParsedExpressionId, ParsedFunctionId, ParsedId, ParsedLoopExpr, ParsedNamespaceId,
    ParsedPattern, ParsedPatternId, ParsedTypeDefnId, ParsedTypeExpression, ParsedTypeExpressionId,
    ParsedUnaryOpKind, ParsedUseId, ParsedWhileExpr, Sources,
};
use crate::parse::{
    Block, FnCall, Identifier, Literal, ParsedExpression, ParsedModule, ParsedStmt,
};
use crate::strings;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);
impl FunctionId {
    pub const PENDING: FunctionId = FunctionId(u32::MAX);
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NamespaceId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbilityId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbilityImplId(u32);

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

pub const CLOSURE_ENV_PARAM_NAME: &str = "__clos_env";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSubstitutionPair {
    from: TypeId,
    to: TypeId,
}

enum CoerceResult {
    Fail(TypedExpr),
    Coerced(&'static str, TypedExpr),
}

#[derive(Debug, Clone)]
enum MaybeTypedExpr {
    Parsed(ParsedExpressionId),
    Typed(TypedExpr),
}

/// Used for analyzing pattern matching
#[derive(Debug, Clone)]
pub enum PatternConstructor {
    Unit,
    BoolFalse,
    BoolTrue,
    /// Note: These 4 (Char, String, Int, Float) will become more interesting if we implement exhaustive range-based matching like Rust's
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
pub struct TypedAbilitySignature {
    ability_id: AbilityId,
    impl_arguments: Vec<SimpleNamedType>,
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
}

impl TypedPattern {
    pub fn is_innumerable_literal(&self) -> bool {
        match self {
            TypedPattern::LiteralChar(_, _span) => true,
            TypedPattern::LiteralInteger(_, _span) => true,
            TypedPattern::LiteralFloat(_, _span) => true,
            TypedPattern::LiteralString(_, _span) => true,
            _ => false,
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedClosure {
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
    pub statements: Vec<TypedStmt>,
    pub span: SpanId,
}

impl TypedBlock {
    fn push_stmt(&mut self, stmt: TypedStmt) {
        self.expr_type = stmt.get_type();
        self.statements.push(stmt);
    }

    fn push_expr(&mut self, expr: TypedExpr) {
        self.push_stmt(TypedStmt::Expr(Box::new(expr)))
    }

    pub fn is_single_unit_block(&self) -> bool {
        self.statements.len() == 1 && self.statements[0].is_unit_expr()
    }
}

#[derive(Debug, Clone)]
pub struct FnArgDefn {
    pub name: Identifier,
    pub variable_id: VariableId,
    // TODO: Consider dropping these explicit position fields
    pub position: u32,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_closure_env: bool,
    pub span: SpanId,
}

impl FnArgDefn {
    pub fn to_fn_arg_type(&self) -> FnParamType {
        FnParamType {
            name: self.name,
            type_id: self.type_id,
            is_context: self.is_context,
            is_closure_env: self.is_closure_env,
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
    Closure,
    AbilityDefn(AbilityId),
    AbilityImpl(AbilityId, TypeId),
    AbilityImplDerivedBlanket(FunctionId, AbilityId, TypeId),
}
impl TypedFunctionKind {
    pub fn blanket_parent_function_id(&self) -> Option<FunctionId> {
        match self {
            TypedFunctionKind::Standard => None,
            TypedFunctionKind::Closure => None,
            TypedFunctionKind::AbilityDefn(_) => None,
            TypedFunctionKind::AbilityImpl(_, _) => None,
            TypedFunctionKind::AbilityImplDerivedBlanket(function_id, _, _) => Some(*function_id),
        }
    }
    pub fn ability_id(&self) -> Option<AbilityId> {
        match self {
            TypedFunctionKind::Standard => None,
            TypedFunctionKind::Closure => None,
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
    pub type_params: Vec<TypeParam>,
    pub body_block: Option<TypedBlock>,
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

pub struct TypeSolutionSet {
    pub solutions: Vec<PendingTypeSolution>,
}

impl<I, T> From<T> for TypeSolutionSet
where
    I: NamedType,
    T: Iterator<Item = I>,
{
    fn from(value: T) -> Self {
        TypeSolutionSet {
            solutions: value
                .map(|v| PendingTypeSolution::unsolved(v.name(), v.type_id()))
                .collect(),
        }
    }
}

impl TypeSolutionSet {
    pub fn all_solved(&self) -> bool {
        self.solutions.iter().all(|s| s.solved_type_id.is_some())
    }

    pub fn get_solution_mut(&mut self, type_id: TypeId) -> Option<&mut PendingTypeSolution> {
        self.solutions.iter_mut().find(|s| s.variable_type_id == type_id)
    }

    pub fn get_solutions(&self) -> Option<Vec<SimpleNamedType>> {
        self.solutions
            .iter()
            .map(|s| s.solved_type_id.map(|type_id| SimpleNamedType { name: s.name, type_id }))
            .collect()
    }

    pub fn get_unsolved(&self) -> Vec<SimpleNamedType> {
        self.solutions
            .iter()
            .filter(|s| s.solved_type_id.is_none())
            .map(|s| SimpleNamedType { name: s.name, type_id: s.variable_type_id })
            .collect()
    }
}

enum TypeSolutionResult {
    Matching,
    NonMatching(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PendingTypeSolution {
    pub name: Identifier,
    pub variable_type_id: TypeId,
    pub solved_type_id: Option<TypeId>,
}

impl PendingTypeSolution {
    pub fn unsolved(name: Identifier, type_id: TypeId) -> Self {
        PendingTypeSolution { name, variable_type_id: type_id, solved_type_id: None }
    }
}

impl<T: NamedType> From<&T> for PendingTypeSolution {
    fn from(value: &T) -> Self {
        PendingTypeSolution::unsolved(value.name(), value.type_id())
    }
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
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
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
    pub expr: Box<TypedExpr>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub enum Callee {
    StaticFunction(FunctionId),
    StaticClosure {
        function_id: FunctionId,
        closure_type_id: TypeId,
    },
    /// Must contain a ClosureObject
    DynamicClosure(Box<TypedExpr>),
    /// Must contain a Function reference
    DynamicFunction(Box<TypedExpr>),
}

impl Callee {
    pub fn make_static(function_id: FunctionId) -> Callee {
        Callee::StaticFunction(function_id)
    }
    pub fn maybe_function_id(&self) -> Option<FunctionId> {
        match self {
            Callee::StaticFunction(function_id) => Some(*function_id),
            Callee::StaticClosure { function_id, .. } => Some(*function_id),
            Callee::DynamicClosure(_) => None,
            Callee::DynamicFunction(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Callee,
    pub args: Vec<TypedExpr>,
    /// type_args remain unerased for some intrinsics where we want codegen to see the types.
    /// Specifically sizeOf[T], there's no actual value to specialize on, kinda of a hack would be
    /// better to specialize anyway and inline? idk
    pub type_args: Vec<SimpleNamedType>,
    pub return_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub expr: TypedExpr,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Vec<StructField>,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ListLiteral {
    pub elements: Vec<TypedExpr>,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: TypedExpr,
    pub consequent: TypedExpr,
    pub alternate: TypedExpr,
    pub ty: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: Box<TypedExpr>,
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
    pub payload: Option<Box<TypedExpr>>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct GetEnumPayload {
    pub target_expr: Box<TypedExpr>,
    pub result_type_id: TypeId,
    pub variant_name: Identifier,
    pub variant_index: u32,
    pub is_referencing: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedEnumIsVariantExpr {
    pub target_expr: Box<TypedExpr>,
    pub variant_name: Identifier,
    pub variant_index: u32,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
struct TypedMatchCase {
    pattern: TypedPattern,
    pre_stmts: Vec<TypedStmt>,
    condition: TypedExpr,
    arm_block: TypedBlock,
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
        match self.value {
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
    pub base_expr: Box<TypedExpr>,
    pub target_type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub value: Box<TypedExpr>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedBreak {
    pub value: Box<TypedExpr>,
    pub loop_scope: ScopeId,
    pub loop_type: LoopType,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct ClosureExpr {
    pub closure_type: TypeId,
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
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub cond: Box<TypedExpr>,
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
pub enum TypedExpr {
    Unit(SpanId),
    Char(u8, SpanId),
    Bool(bool, SpanId),
    Integer(TypedIntegerExpr),
    Float(TypedFloatExpr),
    Str(String, SpanId),
    Struct(Struct),
    Variable(VariableExpr),
    StructFieldAccess(FieldAccess),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Block(TypedBlock),
    Call(Call),
    If(Box<TypedIf>),
    WhileLoop(WhileLoop),
    LoopExpr(LoopExpr),
    EnumConstructor(TypedEnumConstructor),
    EnumIsVariant(TypedEnumIsVariantExpr),
    EnumGetPayload(GetEnumPayload),
    Cast(TypedCast),
    /// Explicit returns are syntactically like function calls, but are their own instruction type
    /// return(<expr>)
    /// It has the expression type of 'never', but is bound by the return type of the nearest
    /// enclosing function or closure
    Return(TypedReturn),
    /// Breaks are syntactically like function calls, but are their own instruction type
    /// break(<expr>)
    /// It has the expression type of 'never', but influences the return type of the enclosing loop
    Break(TypedBreak),
    /// Creating a closure results in a Closure expr.
    /// - A function is created
    /// - An environment capture expr is created
    /// - An expression is returned that is really just a pointer to the unique Closure it points
    ///   to; this can either be called directly or turned into a dynamic function object if needed
    Closure(ClosureExpr),
    /// Referring to a function by name, if there are no variables in scope with the same name
    /// results in taking a function pointer
    FunctionName(FunctionReferenceExpr),
    /// These get re-written into struct access expressions once we know all captures and have
    /// generated the closure's capture struct
    PendingCapture(PendingCaptureExpr),
}

impl From<VariableExpr> for TypedExpr {
    fn from(value: VariableExpr) -> Self {
        TypedExpr::Variable(value)
    }
}

impl TypedExpr {
    #[inline]
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
            TypedExpr::If(ir_if) => ir_if.ty,
            TypedExpr::WhileLoop(while_loop) => while_loop.type_id,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.break_type,
            TypedExpr::EnumConstructor(enum_cons) => enum_cons.type_id,
            TypedExpr::EnumIsVariant(_is_variant) => BOOL_TYPE_ID,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.result_type_id,
            TypedExpr::Cast(c) => c.target_type_id,
            TypedExpr::Return(_ret) => NEVER_TYPE_ID,
            TypedExpr::Break(_break) => NEVER_TYPE_ID,
            TypedExpr::Closure(closure) => closure.closure_type,
            TypedExpr::FunctionName(f) => f.type_id,
            TypedExpr::PendingCapture(pc) => pc.type_id,
        }
    }
    #[inline]
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
            TypedExpr::If(ir_if) => ir_if.span,
            TypedExpr::WhileLoop(while_loop) => while_loop.span,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.span,
            TypedExpr::EnumConstructor(e) => e.span,
            TypedExpr::EnumIsVariant(is_variant) => is_variant.span,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.span,
            TypedExpr::Cast(c) => c.span,
            TypedExpr::Return(ret) => ret.span,
            TypedExpr::Break(brk) => brk.span,
            TypedExpr::Closure(closure) => closure.span,
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
    pub initializer: TypedExpr,
    pub is_referencing: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    Value,
    Reference,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub destination: Box<TypedExpr>,
    pub value: Box<TypedExpr>,
    pub span: SpanId,
    pub kind: AssignmentKind,
}

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Expr(Box<TypedExpr>),
    Let(Box<LetStmt>),
    Assignment(Box<Assignment>),
}

impl From<TypedExpr> for TypedStmt {
    fn from(expr: TypedExpr) -> Self {
        TypedStmt::Expr(Box::new(expr))
    }
}

impl TypedStmt {
    #[inline]
    pub fn get_span(&self) -> SpanId {
        match self {
            TypedStmt::Expr(e) => e.get_span(),
            TypedStmt::Let(v) => v.span,
            TypedStmt::Assignment(ass) => ass.span,
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            TypedStmt::Expr(expr) => expr.get_type(),
            TypedStmt::Let(val_def) => {
                if val_def.variable_type == NEVER_TYPE_ID {
                    NEVER_TYPE_ID
                } else {
                    UNIT_TYPE_ID
                }
            }
            TypedStmt::Assignment(assgn) => {
                if assgn.value.get_type() == NEVER_TYPE_ID {
                    NEVER_TYPE_ID
                } else {
                    UNIT_TYPE_ID
                }
            }
        }
    }

    pub fn is_unit_expr(&self) -> bool {
        if let TypedStmt::Expr(expr) = self {
            return expr.is_unit();
        }
        false
    }

    pub fn is_divergent(&self) -> bool {
        self.get_type() == NEVER_TYPE_ID
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
    pub defn_stmt: TypedStmt,
    pub variable_expr: TypedExpr,
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
    pub is_global: bool,
}

#[derive(Debug)]
pub struct Constant {
    pub variable_id: VariableId,
    pub expr: TypedExpr,
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
    pub namespaces: Vec<Namespace>,
}

impl Namespaces {
    pub fn get(&self, id: NamespaceId) -> &Namespace {
        &self.namespaces[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: NamespaceId) -> &mut Namespace {
        &mut self.namespaces[id.0 as usize]
    }

    pub fn add(&mut self, namespace: Namespace) -> NamespaceId {
        let id = NamespaceId(self.namespaces.len() as u32);
        self.namespaces.push(namespace);
        id
    }

    pub fn iter(&self) -> impl Iterator<Item = (NamespaceId, &Namespace)> {
        self.namespaces
            .iter()
            .enumerate()
            .map(|(index, namespace)| (NamespaceId(index as u32), namespace))
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

    pub fn namespace_for_scope(&self, scope_id: ScopeId) -> Option<NamespaceId> {
        self.namespaces
            .iter()
            .position(|namespace| namespace.scope_id == scope_id)
            .map(|id| NamespaceId(id as u32))
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
    writeln!(w, "\t{}", message.as_ref())?;
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
    pub impl_arguments: Vec<SimpleNamedType>,
    /// Invariant: These functions are ordered how they are defined in the ability, NOT how they appear in
    /// the impl code
    pub functions: Vec<FunctionId>,
    pub scope_id: ScopeId,
    pub span: SpanId,
    /// I need this so that I don't try to instantiate blanket implementations that fail
    /// typechecking nocommit we can just skip the body
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
}

impl TypedModule {
    pub fn new(parsed_module: ParsedModule) -> TypedModule {
        let types = Types {
            types: vec![
                Type::Integer(IntegerType::U8),
                Type::Integer(IntegerType::U16),
                Type::Integer(IntegerType::U32),
                Type::Integer(IntegerType::U64),
                Type::Integer(IntegerType::I8),
                Type::Integer(IntegerType::I16),
                Type::Integer(IntegerType::I32),
                Type::Integer(IntegerType::I64),
            ],
            existing_types_mapping: FxHashMap::new(),
            type_defn_mapping: FxHashMap::new(),
            ability_mapping: FxHashMap::new(),
            placeholder_mapping: FxHashMap::new(),
        };
        debug_assert!(matches!(*types.get(U8_TYPE_ID), Type::Integer(IntegerType::U8)));
        debug_assert!(matches!(*types.get(U16_TYPE_ID), Type::Integer(IntegerType::U16)));
        debug_assert!(matches!(*types.get(U32_TYPE_ID), Type::Integer(IntegerType::U32)));
        debug_assert!(matches!(*types.get(U64_TYPE_ID), Type::Integer(IntegerType::U64)));
        debug_assert!(matches!(*types.get(I8_TYPE_ID), Type::Integer(IntegerType::I8)));
        debug_assert!(matches!(*types.get(I16_TYPE_ID), Type::Integer(IntegerType::I16)));
        debug_assert!(matches!(*types.get(I32_TYPE_ID), Type::Integer(IntegerType::I32)));
        debug_assert!(matches!(*types.get(I64_TYPE_ID), Type::Integer(IntegerType::I64)));

        let scopes = Scopes::make();
        let namespaces = Namespaces { namespaces: Vec::new() };
        TypedModule {
            functions: Vec::with_capacity(parsed_module.functions.len() * 4),
            variables: Variables::default(),
            types,
            constants: Vec::new(),
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
        }
    }

    fn push_debug_level(&mut self) {
        let level = log::LevelFilter::Debug;
        self.debug_level_stack.push(level);
        log::set_max_level(level);
        eprintln!("push max_level is now {}", log::max_level())
    }

    fn pop_debug_level(&mut self) {
        self.debug_level_stack.pop();
        log::set_max_level(*self.debug_level_stack.last().unwrap());
        eprintln!("pop max_level is now {}", log::max_level())
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

    /// Recursively checks if given type contains any type variables
    /// Note: We could cache whether or not a type is generic on insertion into the type pool
    ///       But types are not immutable so this could be a dangerous idea!
    pub fn does_type_reference_type_variables(&self, type_id: TypeId) -> bool {
        if let Some(spec_info) = self.types.get_generic_instance_info(type_id) {
            return spec_info
                .param_values
                .iter()
                .any(|t| self.does_type_reference_type_variables(*t));
        }
        match self.types.get_no_follow(type_id) {
            Type::TypeVariable(_) => true,
            Type::Unit(_) => false,
            Type::Char(_) => false,
            Type::Integer(_) => false,
            Type::Float(_) => false,
            Type::Bool(_) => false,
            Type::Pointer(_) => false,
            Type::Struct(struc) => {
                for field in struc.fields.iter() {
                    if self.does_type_reference_type_variables(field.type_id) {
                        return true;
                    }
                }
                false
            }
            Type::Reference(refer) => self.does_type_reference_type_variables(refer.inner_type),
            Type::Enum(e) => {
                for v in e.variants.iter() {
                    if let Some(payload) = v.payload {
                        if self.does_type_reference_type_variables(payload) {
                            return true;
                        }
                    }
                }
                false
            }
            Type::EnumVariant(ev) => {
                if let Some(payload) = ev.payload {
                    if self.does_type_reference_type_variables(payload) {
                        return true;
                    }
                }
                false
            }
            Type::Never(_) => false,
            Type::Generic(_gen) => true,
            Type::Function(fun) => {
                for param in fun.params.iter() {
                    if self.does_type_reference_type_variables(param.type_id) {
                        return true;
                    }
                }
                if self.does_type_reference_type_variables(fun.return_type) {
                    return true;
                }
                false
            }
            Type::Closure(closure) => {
                self.does_type_reference_type_variables(closure.function_type)
                    || self.does_type_reference_type_variables(closure.env_type)
            }
            // But a closure object is generic if its function is generic
            Type::ClosureObject(co) => self.does_type_reference_type_variables(co.function_type),
            Type::RecursiveReference(_rr) => false,
        }
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
        for type_param in parsed_type_defn.type_params.iter() {
            let type_variable_id = self.add_type_variable(
                TypeVariable {
                    name: type_param.name,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                },
                vec![],
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
        let resulting_type_id = self.eval_type_expr_defn(
            parsed_type_defn.value_expr,
            defn_scope_id,
            type_eval_context,
        )?;

        // If this was a recursive definition, do a replacement
        if let Some(placeholder_id) = self.types.placeholder_mapping.get(&parsed_type_defn_id) {
            self.types.get_mut(*placeholder_id).as_recursive_reference().root_type_id =
                resulting_type_id;
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
        type_expr_id: ParsedTypeExpressionId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        self.eval_type_expr_defn(type_expr_id, scope_id, EvalTypeExprContext::EMPTY.clone())
    }

    fn eval_type_expr_defn(
        &mut self,
        type_expr_id: ParsedTypeExpressionId,
        scope_id: ScopeId,
        // The context is mostly for when we are evaluating a type expression that is part of a type
        // definition. It talks about self_name for recursive definitions, and the definition info
        // like companion namespace, etc for a type definition.
        context: EvalTypeExprContext,
    ) -> TyperResult<TypeId> {
        let base = match self.ast.type_expressions.get(type_expr_id) {
            ParsedTypeExpression::Builtin(span) => {
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
                    _ => failf!(*span, "Unknown builtin type '{}'", name),
                }
            }
            ParsedTypeExpression::Integer(num_type) => match (num_type.width, num_type.signed) {
                (NumericWidth::B8, false) => Ok(U8_TYPE_ID),
                (NumericWidth::B16, false) => Ok(U16_TYPE_ID),
                (NumericWidth::B32, false) => Ok(U32_TYPE_ID),
                (NumericWidth::B64, false) => Ok(U64_TYPE_ID),
                (NumericWidth::B8, true) => Ok(I8_TYPE_ID),
                (NumericWidth::B16, true) => Ok(I16_TYPE_ID),
                (NumericWidth::B32, true) => Ok(I32_TYPE_ID),
                (NumericWidth::B64, true) => Ok(I64_TYPE_ID),
            },
            ParsedTypeExpression::Struct(struct_defn) => {
                let struct_defn = struct_defn.clone();
                let mut fields: Vec<StructTypeField> = Vec::new();
                for (index, ast_field) in struct_defn.fields.iter().enumerate() {
                    let ty = self.eval_type_expr_defn(
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
            ParsedTypeExpression::TypeApplication(_ty_app) => {
                let type_op_result =
                    self.detect_and_eval_type_operator(type_expr_id, scope_id, context.clone())?;
                match type_op_result {
                    None => self.eval_type_application(type_expr_id, scope_id, context),
                    Some(type_op_result) => Ok(type_op_result),
                }
            }
            ParsedTypeExpression::Optional(opt) => {
                let inner_ty =
                    self.eval_type_expr_defn(opt.base, scope_id, context.no_attach_defn_info())?;
                let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, vec![inner_ty]);
                Ok(optional_type)
            }
            ParsedTypeExpression::Reference(r) => {
                let inner_ty =
                    self.eval_type_expr_defn(r.base, scope_id, context.no_attach_defn_info())?;
                let reference_type = Type::Reference(ReferenceType { inner_type: inner_ty });
                let type_id = self.types.add_type(reference_type);
                Ok(type_id)
            }
            ParsedTypeExpression::Enum(e) => {
                let e = e.clone();
                let mut variants = Vec::with_capacity(e.variants.len());
                for (index, v) in e.variants.iter().enumerate() {
                    let payload_type_id = match &v.payload_expression {
                        None => None,
                        Some(payload_type_expr) => {
                            let type_id = self.eval_type_expr_defn(
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
                });
                let enum_type_id = self.types.add_type(enum_type);
                Ok(enum_type_id)
            }
            ParsedTypeExpression::DotMemberAccess(dot_acc) => {
                let dot_acc = dot_acc.clone();
                let base_type = self.eval_type_expr_defn(
                    dot_acc.base,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
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
            // a function object type, which is always a closure, which is a
            // 2 member struct with a function pointer and an environment pointer
            ParsedTypeExpression::Function(fun_type) => {
                let fun_type = fun_type.clone();
                let mut params: Vec<FnParamType> = Vec::with_capacity(fun_type.params.len());
                let empty_struct_id = self.types.add_type(Type::Struct(StructType {
                    type_defn_info: None,
                    fields: vec![],
                    ast_node: type_expr_id.into(),
                    generic_instance_info: None,
                }));
                let empty_struct_reference_id = self.types.add_reference_type(empty_struct_id);
                params.push(FnParamType {
                    type_id: empty_struct_reference_id,
                    name: get_ident!(self, CLOSURE_ENV_PARAM_NAME),
                    is_context: false,
                    is_closure_env: true,
                    span: fun_type.span,
                });
                for (index, param) in fun_type.params.iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;
                    let span = self.ast.get_type_expression_span(*param);
                    let name = self.ast.identifiers.intern(format!("param_{}", index));
                    params.push(FnParamType {
                        type_id,
                        name,
                        is_context: false,
                        is_closure_env: false,
                        span,
                    });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                let function_type_id = self.types.add_type(Type::Function(FunctionType {
                    params,
                    return_type,
                    defn_info: None,
                }));

                let closure_object_type = self.types.add_closure_object(
                    &self.ast.identifiers,
                    function_type_id,
                    type_expr_id.into(),
                );
                Ok(closure_object_type)
            }
            ParsedTypeExpression::TypeOf(tof) => {
                let expr = self.eval_expr(tof.target_expr, scope_id, None)?;
                Ok(expr.get_type())
            }
        }?;
        Ok(base)
    }

    /// Temporary home for our type operators until I decide on syntax
    fn detect_and_eval_type_operator(
        &mut self,
        ty_app_id: ParsedTypeExpressionId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> TyperResult<Option<TypeId>> {
        let ParsedTypeExpression::TypeApplication(ty_app) =
            self.ast.type_expressions.get(ty_app_id)
        else {
            panic_at_disco!("Expected TypeApplication")
        };
        let ty_app = ty_app.clone();
        match self.name_of(ty_app.name.name) {
            "_struct_combine" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_combine");
                }
                let arg1 = self.eval_type_expr_defn(
                    ty_app.args[0].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
                let arg2 = self.eval_type_expr_defn(
                    ty_app.args[1].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or(errf!(ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or(errf!(ty_app.span, "Expected struct"))?;

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
                let arg1 = self.eval_type_expr_defn(
                    ty_app.args[0].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;
                let arg2 = self.eval_type_expr_defn(
                    ty_app.args[1].type_expr,
                    scope_id,
                    context.no_attach_defn_info(),
                )?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or(errf!(ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or(errf!(ty_app.span, "Expected struct"))?;
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
        ty_app_id: ParsedTypeExpressionId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> TyperResult<TypeId> {
        let ParsedTypeExpression::TypeApplication(ty_app) =
            self.ast.type_expressions.get(ty_app_id)
        else {
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
                        let param_type_id = self.eval_type_expr_defn(
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
                                                root_type_id: TypeId::PENDING,
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
                    Some(generic_type),
                    inner,
                    Some(type_defn_info),
                    &substitution_pairs,
                );
                if log::log_enabled!(log::Level::Debug) {
                    let gen = self.types.get(generic_type).expect_generic();
                    let inst_info = &self
                        .types
                        .get_generic_instance_info(specialized_type)
                        .unwrap()
                        .param_values;
                    debug!(
                        "instantiated\n{} with params\n{}\ngot expanded type: {}",
                        self.name_of(gen.type_defn_info.name),
                        self.pretty_print_types(inst_info),
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
        generic_parent_to_attach: Option<TypeId>,
        type_id: TypeId,
        defn_info_to_attach: Option<TypeDefnInfo>,
        substitution_pairs: &[TypeSubstitutionPair],
    ) -> TypeId {
        let force_new = defn_info_to_attach.is_some();
        // If this type is already a generic instance of something, just
        // re-specialize it on the right inputs. So find out what the new value
        // of each type param should be and call instantiate_generic_type
        //
        // This happens when specializing a type that contains an Opt[T], for example.
        //
        // Things don't seem to work without this special case, but I kinda think they should.
        // I guess the problem is that we don't 'inherit' generic instance info
        //
        // Ok now doing that, and not wrongly setting generic instance info on the nested types,
        // but stuff still doesn't compile. Should figure out why

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
                    self.substitute_in_type(None, *prev_type_id, None, substitution_pairs)
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
                        self.substitute_in_type(None, field.type_id, None, substitution_pairs);
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
                for variant in new_variants.iter_mut() {
                    let new_payload_id = variant.payload.map(|payload_type_id| {
                        self.substitute_in_type(None, payload_type_id, None, substitution_pairs)
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
                    };
                    let new_enum_id = self.types.add_type(Type::Enum(new_enum));
                    new_enum_id
                } else {
                    type_id
                }
            }
            Type::Reference(reference) => {
                let ref_inner = reference.inner_type;
                let new_inner = self.substitute_in_type(None, ref_inner, None, substitution_pairs);
                if force_new || new_inner != ref_inner {
                    let specialized_reference = ReferenceType { inner_type: new_inner };
                    self.types.add_type(Type::Reference(specialized_reference))
                } else {
                    type_id
                }
            }
            Type::TypeVariable(_) => type_id,
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
                    self.substitute_in_type(None, fun_type.return_type, None, substitution_pairs);
                if new_return_type != new_fun_type.return_type {
                    any_new = true
                };
                new_fun_type.return_type = new_return_type;
                for param in new_fun_type.params.iter_mut() {
                    let new_param_type =
                        self.substitute_in_type(None, param.type_id, None, substitution_pairs);
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
            Type::Closure(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Closure")
            }
            Type::ClosureObject(co) => {
                let co_fn_type = co.function_type;
                let co_parsed_id = co.parsed_id;
                let new_fn_type =
                    self.substitute_in_type(None, co.function_type, None, substitution_pairs);
                if new_fn_type != co_fn_type || force_new {
                    self.types.add_closure_object(&self.ast.identifiers, new_fn_type, co_parsed_id)
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
        parsed_type_expr: ParsedTypeExpressionId,
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

    fn eval_pattern(
        &self,
        pat_expr: ParsedPatternId,
        target_type_id: TypeId,
        _scope_id: ScopeId,
    ) -> TyperResult<TypedPattern> {
        let parsed_pattern_expr = self.ast.patterns.get_pattern(pat_expr);
        match parsed_pattern_expr {
            ParsedPattern::Wildcard(span) => Ok(TypedPattern::Wildcard(*span)),
            ParsedPattern::Literal(literal_expr_id) => {
                match self.ast.expressions.get(*literal_expr_id).expect_literal() {
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
                            Some(target_type_id),
                        )? {
                            TypedExpr::Integer(value) => match self.types.get(target_type_id) {
                                Type::Integer(_integer_type) => {
                                    Ok(TypedPattern::LiteralInteger(value.value, num_lit.span))
                                }
                                _ => failf!(
                                    self.ast.get_pattern_span(pat_expr),
                                    "unrelated pattern type int will never match {}",
                                    self.type_id_to_string(target_type_id)
                                ),
                            },
                            TypedExpr::Float(value) => match self.types.get(target_type_id) {
                                Type::Float(_integer_type) => {
                                    Ok(TypedPattern::LiteralFloat(value.value, num_lit.span))
                                }
                                _ => failf!(
                                    self.ast.get_pattern_span(pat_expr),
                                    "unrelated pattern type float will never match {}",
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
                            "unrelated pattern type bool will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    // Clone would go away if we intern string literals
                    // But I think this is where we'd interpolate and handle escapes and stuff so maybe there's always going
                    // to be an allocation here. Should be same handling as non-pattern string literals
                    Literal::String(s, span) => match target_type_id {
                        STRING_TYPE_ID => Ok(TypedPattern::LiteralString(s.clone(), *span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type string will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                }
            }
            ParsedPattern::Variable(ident_id, span) => {
                Ok(TypedPattern::Variable(VariablePattern { name: *ident_id, span: *span }))
            }
            ParsedPattern::Enum(enum_pattern) => {
                let Some((enum_type, _variant)) = self.types.get_as_enum(target_type_id) else {
                    return failf!(
                        enum_pattern.span,
                        "Impossible pattern: Expected an enum type; but got {}",
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
                        let payload_type_id = matching_variant.payload.ok_or(make_error(
                            "Impossible pattern: Enum variant has no payload",
                            enum_pattern.span,
                        ))?;
                        let payload_pattern =
                            self.eval_pattern(*payload_expr, payload_type_id, _scope_id)?;
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
                    return make_fail_span(
                        "Useless pattern: Struct pattern has no fields; use wildcard pattern '_' instead",
                        struct_pattern.span,
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
                        expected_struct.fields.iter().find(|f| f.name == *field_name).ok_or(
                            errf!(
                                self.ast.get_pattern_span(*field_parsed_pattern_id),
                                "Impossible pattern: Struct has no field named '{}'",
                                self.name_of(*field_name).blue()
                            ),
                        )?;
                    let field_type_id = expected_field.type_id;
                    let field_pattern =
                        self.eval_pattern(*field_parsed_pattern_id, field_type_id, _scope_id)?;
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
        if let Type::TypeVariable(tvar) = self.types.get(type_id) {
            match self.scopes.find_type(scope_id, tvar.name) {
                None => {
                    debug!("Unresolved type_id. {}", self.name_of(tvar.name));
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
            (Type::Struct(r1), Type::Struct(r2)) => self.typecheck_struct(r1, r2, scope_id),
            (Type::Reference(o1), Type::Reference(o2)) => {
                self.check_types(o1.inner_type, o2.inner_type, scope_id)
            }
            (Type::Enum(_exp_enum), Type::Enum(_act_enum)) => {
                if expected == actual {
                    Ok(())
                } else {
                    Err(format!(
                        "expected enum {} but got enum {}",
                        self.type_id_to_string(expected),
                        self.type_id_to_string(actual)
                    ))
                }
            }
            (Type::Enum(_expected_enum), Type::EnumVariant(actual_variant)) => {
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
            (Type::ClosureObject(closure_object), Type::Closure(closure_type)) => {
                self.check_types(closure_object.function_type, closure_type.function_type, scope_id)
            }
            (Type::ClosureObject(exp_closure_object), Type::ClosureObject(act_closure_object)) => {
                self.check_types(
                    exp_closure_object.function_type,
                    act_closure_object.function_type,
                    scope_id,
                )
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
            (_exp, _act) => {
                // Resolve type variables
                Err(format!(
                    "Expected {} but got {}",
                    self.type_id_to_string_ext(expected, false),
                    self.type_id_to_string_ext(actual, false),
                ))
            }
        }
    }

    fn eval_const(
        &mut self,
        parsed_constant_id: ParsedConstantId,
        scope_id: ScopeId,
    ) -> TyperResult<VariableId> {
        let parsed_constant = self.ast.get_constant(parsed_constant_id);
        let type_id = self.eval_const_type_expr(parsed_constant.ty, scope_id)?;
        let parsed_constant = self.ast.get_constant(parsed_constant_id);
        let constant_name = parsed_constant.name;
        let constant_span = parsed_constant.span;
        let expr = match self.ast.expressions.get(parsed_constant.value_expr) {
            ParsedExpression::Literal(Literal::Numeric(integer)) => {
                self.eval_numeric_value(&integer.text, integer.span, Some(type_id))?
            }
            ParsedExpression::Literal(Literal::Bool(b, span)) => TypedExpr::Bool(*b, *span),
            ParsedExpression::Literal(Literal::Char(c, span)) => TypedExpr::Char(*c, *span),
            _other => {
                return make_fail_span(
                    "Only literals are currently supported as constants",
                    parsed_constant.span,
                );
            }
        };
        let variable_id = self.variables.add_variable(Variable {
            name: constant_name,
            type_id,
            is_mutable: false,
            owner_scope: scope_id,
            is_context: false,
            is_global: true,
        });
        self.constants.push(Constant { variable_id, expr, ty: type_id, span: constant_span });
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
                    self.get_root_namespace_id(),
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
        value: TypeVariable,
        ability_impls: Vec<TypedAbilitySignature>,
    ) -> TypeId {
        let span = value.span;
        let constrained_impl_scope =
            self.scopes.add_child_scope(value.scope_id, ScopeType::AbilityImpl, None, None);
        let type_id = self.types.add_type(Type::TypeVariable(value));
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
        let blanket_impl = self.get_ability_impl(blanket_impl_id);
        if !blanket_impl.compile_errors.is_empty() {
            debug!("Blanket impl failed compile; skipping");
            return None;
        }
        let target_ability = self.get_ability(target_ability_id);
        let target_ability_args = target_ability.kind.arguments();
        let AbilityImplKind::Blanket { parsed_id, .. } = blanket_impl.kind else {
            unreachable!("Expected a blanket impl")
        };
        let target_base = target_ability.parent_ability_id().unwrap_or(target_ability_id);
        // The 'self' type of the generic impl might contain type parameters that we need to solve.
        // But what if it doesn't? Need examples
        // impl[T: Add[Rhs = T, Output = T]] Add[Rhs = Point[T], Output = Point[T]] for Point[T] {
        // impl[T] Add[Rhs = Point[T], Output = Point[T]] for Point[int] {
        // Ability: Add[Point[T]], Self: Point[T]
        // We have solved Self and all other params, so we know what Rhs and Self are
        // Self := A, Rhs := B
        // Add[Rhs = Point[T], Self = Point[T]] -> Rhs := A, Self := B
        // Solve for T in Point[T] <-> A, Point[T] <-> B --> T := Solution
        // If consistent and solved, 'run' the blanket impl with T := Solution, returning an
        // ability impl handle
        let blanket_ability = self.get_ability(blanket_impl.ability_id);
        let blanket_base = blanket_ability.parent_ability_id().unwrap_or(blanket_impl.ability_id);

        debug!("Trying blanket impl {}", self.name_of(blanket_ability.name));

        if blanket_base != target_base {
            debug!("Wrong blanket base {}", self.name_of(blanket_ability.name));
        }

        let blanket_arguments = blanket_ability.kind.arguments();
        if blanket_arguments.len() != target_ability_args.len() {
            debug!(
                "Wrong arg count {} vs {}",
                self.pretty_print_named_types(blanket_arguments, ", "),
                self.pretty_print_named_types(target_ability_args, ", ")
            );
        }

        let mut solution_set = TypeSolutionSet::from(blanket_impl.type_params.iter());
        // For each argument A to the blanket impl, solve for [Self, ...Params] using
        for (arg_to_blanket, arg_to_target) in blanket_arguments.iter().zip(target_ability_args) {
            debug!(
                "Solving with slot: {} | passed: {}",
                self.type_id_to_string(arg_to_target.type_id),
                self.type_id_to_string(arg_to_blanket.type_id),
            );
            if let Err(solve_error) = self.solve_generic_params(
                &mut solution_set,
                arg_to_target.type_id,
                arg_to_blanket.type_id,
                span,
            ) {
                debug!("Bailing due to error; {solve_error}");
                return None;
            }
        }
        if let Err(solve_error) = self.solve_generic_params(
            &mut solution_set,
            self_type_id,
            blanket_impl.self_type_id,
            span,
        ) {
            debug!("Bailing due to error; {solve_error}");
            return None;
        };
        match solution_set.get_solutions() {
            None => {
                debug!(
                    "Could not solve all blanket impl params: {}",
                    self.pretty_print_named_types(&solution_set.get_unsolved(), ", ")
                );
                None
            }
            Some(solutions) => {
                // 'Specialize' the constraints:
                // - For each constraint, run the expression with the binding for T from a child
                //   scope of the blanket impl scope
                // - Then check if the solution implements _that_ ability, by factoring
                //   out the actual inner check from check_type_constraints
                let constraint_checking_scope = self.scopes.add_sibling_scope(
                    blanket_impl.scope_id,
                    ScopeType::AbilityImpl,
                    None,
                    None,
                );
                let parsed_blanket_impl = self.ast.get_ability_impl(parsed_id);
                for (parsed_param, solution) in
                    parsed_blanket_impl.generic_impl_params.clone().iter().zip(solutions.iter())
                {
                    let _ = self.scopes.add_type(
                        constraint_checking_scope,
                        parsed_param.name,
                        solution.type_id,
                    );
                    for constraint in parsed_param.constraints.iter() {
                        let constraint_signature = match constraint {
                            parse::ParsedTypeConstraintExpr::Ability(parsed_ability_expr) => self
                                .eval_ability_expr(
                                    parsed_ability_expr,
                                    false,
                                    constraint_checking_scope,
                                )
                                .unwrap(),
                        };
                        if let Err(mut e) = self.check_type_constraint(
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
                            self.write_error(&mut std::io::stderr(), &e).unwrap();
                            return None;
                        }
                    }
                }

                // 'Run' the blanket ability using 'solutions'
                let impl_handle = self
                    .instantiate_blanket_impl(self_type_id, blanket_impl_id, &solutions)
                    .unwrap_or_else(|e| self.ice("Failed to instantiate blanket impl", Some(&e)));
                Some(impl_handle)
            }
        }
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
            let substituted_type = self.substitute_in_type(None, blanket_arg.type_id, None, &pairs);
            let nt = SimpleNamedType { name: blanket_arg.name, type_id: substituted_type };
            substituted_ability_args.push(nt);
            if !self.scopes.add_type(new_impl_scope, blanket_arg.name, substituted_type) {
                panic!("uh oh")
            };
        }
        let concrete_ability_id =
            self.specialize_ability(generic_parent, substituted_ability_args, blanket_impl.span)?;

        let mut substituted_impl_arguments = Vec::with_capacity(blanket_impl.impl_arguments.len());
        for blanket_impl_arg in &blanket_impl.impl_arguments {
            // Substitute T, U, V, in for each
            let substituted_type =
                self.substitute_in_type(None, blanket_impl_arg.type_id, None, &pairs);
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
                self.get_root_namespace_id(),
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

    // If the expr is already a block, do nothing
    // If it is not, make a new block with just this expression inside.
    // Used main for if/else
    fn coerce_expr_to_block(&mut self, expr: TypedExpr, scope: ScopeId) -> TypedBlock {
        match expr {
            TypedExpr::Block(b) => b,
            expr => {
                let expr_span = expr.get_span();
                let statements = vec![TypedStmt::Expr(Box::new(expr))];
                self.synth_block(statements, scope, expr_span)
            }
        }
    }

    fn coerce_block_to_unit_block(&mut self, block: &mut TypedBlock) {
        if block.expr_type != UNIT_TYPE_ID {
            let span = block.statements.last().map(|s| s.get_span()).unwrap_or(block.span);
            let unit_literal = TypedExpr::Unit(span);
            block.push_expr(unit_literal);
        }
    }

    fn eval_numeric_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        if parsed_text.contains('.') {
            Ok(TypedExpr::Float(TypedFloatExpr {
                value: self.eval_float_value(parsed_text, span, expected_type_id)?,
                span,
            }))
        } else {
            Ok(TypedExpr::Integer(TypedIntegerExpr {
                value: self.eval_integer_value(parsed_text, span, expected_type_id)?,
                span,
            }))
        }
    }

    fn eval_float_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<TypedFloatValue> {
        let expected_width = match expected_type_id {
            None => NumericWidth::B32,
            Some(F64_TYPE_ID) => NumericWidth::B64,
            // Parse as f32 and let typechecking fail
            Some(_) => NumericWidth::B32,
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
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<TypedIntegerValue> {
        let expected_int_type = match expected_type_id {
            None => IntegerType::I64,
            Some(U8_TYPE_ID) => IntegerType::U8,
            Some(U16_TYPE_ID) => IntegerType::U16,
            Some(U32_TYPE_ID) => IntegerType::U32,
            Some(U64_TYPE_ID) => IntegerType::U64,
            Some(I8_TYPE_ID) => IntegerType::I8,
            Some(I16_TYPE_ID) => IntegerType::I16,
            Some(I32_TYPE_ID) => IntegerType::I32,
            Some(I64_TYPE_ID) => IntegerType::I64,
            // Parse as i64 and let typechecking fail
            Some(_) => IntegerType::I64,
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
                make_error(format!("Invalid integer {expected_int_type}: {e}"), span)
            })?;
            Ok(value)
        }
    }

    fn eval_variable(
        &mut self,
        variable_expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExpr> {
        let ParsedExpression::Variable(variable) = self.ast.expressions.get(variable_expr_id)
        else {
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
                    Some(function_id) => Ok(TypedExpr::FunctionName(FunctionReferenceExpr {
                        function_id,
                        span: variable.name.span,
                        type_id: self
                            .types
                            .add_reference_type(self.get_function(function_id).type_id),
                    })),
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
                let is_capture = if let Some(nearest_parent_closure_scope) =
                    self.scopes.nearest_parent_closure(scope_id)
                {
                    let variable_is_above_closure = self
                        .scopes
                        .scope_has_ancestor(nearest_parent_closure_scope, variable_scope_id);
                    let variable_is_global = self.variables.get(variable_id).is_global;

                    let is_capture = variable_is_above_closure && !variable_is_global;
                    debug!("{}, is_capture={is_capture}", self.name_of(variable.name.name));
                    if is_capture {
                        self.scopes.add_capture(nearest_parent_closure_scope, variable_id);
                        true
                    } else {
                        false
                    }
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
                    let expr = TypedExpr::PendingCapture(PendingCaptureExpr {
                        captured_variable_id: variable_id,
                        type_id: v.type_id,
                        span: variable_name_span,
                    });
                    Ok(expr)
                } else {
                    let expr = TypedExpr::Variable(VariableExpr {
                        type_id: v.type_id,
                        variable_id,
                        span: variable_name_span,
                    });
                    Ok(expr)
                }
            }
        }
    }

    fn eval_field_access(
        &mut self,
        field_access: &parse::FieldAccess,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        // Bailout case: Enum Constructor
        let span = field_access.span;
        let base_span = self.ast.expressions.get_span(field_access.base);
        if let Some(enum_result) = self.handle_enum_constructor(
            field_access.base,
            field_access.target,
            None,
            expected_type,
            &field_access.type_args,
            scope_id,
            span,
        )? {
            return Ok(enum_result);
        }

        // Bailout case: .* dereference operation
        if field_access.target == get_ident!(self, "*") {
            return self.eval_dereference(field_access.base, scope_id, expected_type, span);
        }

        // Bailout case: .! unwrap operation
        if field_access.target == get_ident!(self, "!") {
            if field_access.is_coalescing {
                return failf!(field_access.span, "Cannot use ?. with unwrap operator");
            }
            if field_access.is_referencing {
                return failf!(field_access.span, "Cannot use * with unwrap operator");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to unwrap operator");
            }
            return self.eval_unwrap_operator(
                field_access.base,
                scope_id,
                expected_type,
                field_access.span,
            );
        }

        if field_access.target == get_ident!(self, "try") {
            if field_access.is_coalescing {
                return failf!(field_access.span, "Cannot use ?. with try operator");
            }
            if field_access.is_referencing {
                return failf!(field_access.span, "Cannot use * with try operator");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to try operator");
            }
            return self.eval_try_operator(
                field_access.base,
                scope_id,
                expected_type,
                field_access.span,
            );
        }

        let mut base_expr = self.eval_expr(field_access.base, scope_id, None)?;
        let original_base_expr_type = base_expr.get_type();

        // Perform auto-dereference for accesses that are not 'lvalue'-style or 'referencing' style
        let (base_type, is_reference) = match self.types.get(base_expr.get_type()) {
            Type::Reference(reference_type) => {
                if !is_assignment_lhs && !field_access.is_referencing {
                    // Dereference the base expression
                    base_expr = TypedExpr::UnaryOp(UnaryOp {
                        kind: UnaryOpKind::Dereference,
                        type_id: reference_type.inner_type,
                        span: base_expr.get_span(),
                        expr: Box::new(base_expr),
                    });
                }
                (reference_type.inner_type, true)
            }
            _other => {
                if is_assignment_lhs {
                    return failf!(base_span, "Cannot assign to member of non-reference struct");
                } else if field_access.is_referencing && !field_access.is_coalescing {
                    return failf!(base_span, "Field access target is not a pointer, so referencing access with * cannot be used");
                } else {
                    (base_expr.get_type(), false)
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
                    let mut block = self.synth_block(vec![], scope_id, span);
                    let block_scope = block.scope_id;
                    let base_expr_var = self.synth_variable_defn_simple(
                        field_access.target,
                        base_expr,
                        block_scope,
                    );
                    let has_value = self.synth_typed_function_call(
                        self.ident_opt_has_value(span),
                        vec![opt.inner_type],
                        vec![base_expr_var.variable_expr.clone()],
                        block_scope,
                        None,
                    )?;

                    let st @ Type::Struct(struct_type) = self.types.get(opt.inner_type) else {
                        return failf!(
                            span,
                            "?. must be used on optional structs, got {}",
                            self.type_id_to_string(original_base_expr_type)
                        );
                    };
                    let (field_index, target_field) =
                        struct_type.find_field(field_access.target).ok_or(make_error(
                            format!(
                                "Field {} not found on struct {}",
                                self.ast.identifiers.get_name(field_access.target),
                                self.type_to_string(st, false)
                            ),
                            span,
                        ))?;
                    let field_type = target_field.type_id;
                    let field_name = target_field.name;
                    let opt_unwrap = self.synth_typed_function_call(
                        self.ident_opt_get(span),
                        vec![opt.inner_type],
                        vec![base_expr_var.variable_expr],
                        block_scope,
                        None,
                    )?;
                    let consequent =
                        self.synth_optional_some(TypedExpr::StructFieldAccess(FieldAccess {
                            base: Box::new(opt_unwrap),
                            target_field: field_name,
                            target_field_index: field_index as u32,
                            span,
                            is_referencing: false,
                            result_type: field_type,
                            struct_type: opt.inner_type,
                        }));
                    let alternate = self.synth_optional_none(field_type, span);
                    let if_expr = TypedExpr::If(Box::new(TypedIf {
                        condition: has_value,
                        ty: consequent.get_type(),
                        consequent,
                        alternate,
                        span,
                    }));
                    block.push_stmt(base_expr_var.defn_stmt);
                    block.push_expr(if_expr);
                    Ok(TypedExpr::Block(block))
                } else {
                    failf!(
                        span,
                        "Field {} does not exist",
                        self.ast.identifiers.get_name(field_access.target)
                    )
                }
            }
            st @ Type::Struct(struct_type) => {
                if is_assignment_lhs && !is_reference {
                    return failf!(span, "Struct must be a reference to be assignable");
                }
                let (field_index, target_field) =
                    struct_type.find_field(field_access.target).ok_or(make_error(
                        format!(
                            "Field {} not found on struct {}",
                            self.ast.identifiers.get_name(field_access.target),
                            self.type_to_string(st, false)
                        ),
                        span,
                    ))?;
                if target_field.private {
                    let companion_namespace =
                        struct_type.type_defn_info.as_ref().and_then(|d| d.companion_namespace);

                    if !self.is_inside_companion_scope(companion_namespace, scope_id) {
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
                Ok(TypedExpr::StructFieldAccess(FieldAccess {
                    base: Box::new(base_expr),
                    target_field: field_access.target,
                    target_field_index: field_index as u32,
                    result_type,
                    is_referencing: field_access.is_referencing,
                    struct_type: base_type,
                    span,
                }))
            }
            Type::EnumVariant(ev) => {
                if self.ast.identifiers.get_name(field_access.target) != "value" {
                    return failf!(
                        span,
                        "Field {} does not exist; try .value",
                        self.ast.identifiers.get_name(field_access.target)
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
                Ok(TypedExpr::EnumGetPayload(GetEnumPayload {
                    target_expr: Box::new(base_expr),
                    result_type_id,
                    variant_name,
                    variant_index,
                    is_referencing: field_access.is_referencing,
                    span,
                }))
            }
            _ => failf!(
                span,
                "Field {} does not exist on type {}",
                self.ast.identifiers.get_name(field_access.target),
                self.type_id_to_string(base_expr.get_type())
            ),
        }
    }

    fn eval_try_operator(
        &mut self,
        operand: ParsedExpressionId,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let block_return_type = self.get_expected_return_type(scope_id, span)?;
        let block_try_impl = self.expect_ability_implementation(
                    block_return_type,
                    TRY_ABILITY_ID,
                    span,
                ).map_err(|mut e| {
                        e.message = format!("`.try` can only be used from a function or closure that returns a type implementing `Try`. {}", e.message);
                        e
                    })?;
        let try_value_original_expr = self.eval_expr(operand, scope_id, None)?;
        let try_value_type = try_value_original_expr.get_type();
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
        let mut result_block = self.synth_block(vec![], scope_id, span);
        let try_value_var = self.synth_variable_defn_simple(
            get_ident!(self, "try_value"),
            try_value_original_expr,
            result_block.scope_id,
        );
        let is_ok_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "isOk"),
            vec![],
            vec![try_value_var.variable_expr.clone()],
            result_block.scope_id,
            None,
        )?;
        let get_ok_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "getOk"),
            vec![],
            vec![try_value_var.variable_expr.clone()],
            result_block.scope_id,
            None,
        )?;
        // FIXME: Consider alternatives for calling the block's makeError function
        //        in a less brittle way?
        let block_make_error_fn = self.get_ability_impl(block_try_impl.full_impl_id).functions[0];
        eprintln!(
            "TYPE OF make_error: {}",
            self.type_id_to_string(self.get_function(block_make_error_fn).type_id)
        );

        let get_error_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "getError"),
            vec![],
            vec![try_value_var.variable_expr],
            result_block.scope_id,
            None,
        )?;
        let make_error_call = TypedExpr::Call(Call {
            callee: Callee::StaticFunction(block_make_error_fn),
            args: vec![get_error_call],
            type_args: vec![],
            return_type: block_error_type,
            span,
        });
        let return_error_expr =
            TypedExpr::Return(TypedReturn { value: Box::new(make_error_call), span });
        let if_expr = TypedExpr::If(Box::new(TypedIf {
            condition: is_ok_call,
            consequent: get_ok_call,
            alternate: return_error_expr,
            ty: value_success_type,
            span,
        }));

        result_block.push_stmt(try_value_var.defn_stmt);
        result_block.push_expr(if_expr);

        eprintln!("{}", self.block_to_string(&result_block));
        Ok(TypedExpr::Block(result_block))
    }

    fn eval_unwrap_operator(
        &mut self,
        operand: ParsedExpressionId,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let operand_expr = self.eval_expr_inner(operand, scope_id, None)?;
        let _unwrap_impl =
            self.expect_ability_implementation(operand_expr.get_type(), UNWRAP_ABILITY_ID, span)?;
        self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "unwrap"),
            vec![],
            vec![operand_expr],
            scope_id,
            None,
        )
    }

    fn eval_dereference(
        &mut self,
        operand: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        // Example:
        // let x: int = intptr.*
        // The expected_type when we get `*intptr` is int, so
        // the expected_type when we get `intptr` should be *int
        let expected_type = match expected_type {
            Some(expected) => Some(self.types.add_reference_type(expected)),
            None => None,
        };
        let base_expr = self.eval_expr(operand, scope_id, expected_type)?;
        let reference_type =
            self.types.get(base_expr.get_type()).as_reference().ok_or(make_error(
                format!(
                    "Cannot dereference non-reference type: {}",
                    self.type_id_to_string(base_expr.get_type())
                ),
                span,
            ))?;
        Ok(TypedExpr::UnaryOp(UnaryOp {
            kind: UnaryOpKind::Dereference,
            type_id: reference_type.inner_type,
            expr: Box::new(base_expr),
            span,
        }))
    }

    /// Used for
    /// - de-referencing,
    /// Probably unsound. I'd like to remove and re-introduce coercion as part of subtyping.
    /// However, not sure if we should do it for references, because I'd like to move that to an
    /// ability so that we can have different types of references written in userspace
    fn coerce_expression_to_expected_type(
        &mut self,
        expected_type_id: TypeId,
        expression: TypedExpr,
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
            if let Some(reference) = self.types.get(expression.get_type()).as_reference() {
                return CoerceResult::Coerced(
                    "deref",
                    TypedExpr::UnaryOp(UnaryOp {
                        kind: UnaryOpKind::Dereference,
                        type_id: reference.inner_type,
                        span: expression.get_span(),
                        expr: Box::new(expression),
                    }),
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
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let directives = self.ast.expressions.get_directives(expr_id);
        let debug_directive =
            directives.iter().find(|p| matches!(p.kind, DirectiveKind::CompilerDebug));
        let is_debug = debug_directive.is_some();
        if is_debug {
            self.push_debug_level();
        }
        let expected_type = match self.ast.expressions.get_type_hint(expr_id) {
            Some(t) => {
                let type_id = self.eval_type_expr(t, scope_id)?;
                Some(type_id)
            }
            None => expected_type,
        };
        let base_result = self.eval_expr_inner(expr_id, scope_id, expected_type)?;

        // We gotta get rid of this coerce step its involved in every bug
        let result = if let Some(expected_type_id) = expected_type {
            // Try to coerce if types don't match
            let new_expr = match self.coerce_expression_to_expected_type(
                expected_type_id,
                base_result,
                scope_id,
                expr_id.into(),
            ) {
                CoerceResult::Fail(base_result) => base_result,
                CoerceResult::Coerced(reason, new_expr) => {
                    debug!(
                        "coerce succeeded with rule {reason} and resulted in expr {}: {}",
                        self.expr_to_string(&new_expr),
                        self.type_id_to_string(new_expr.get_type())
                    );
                    new_expr
                }
            };
            Ok(new_expr)
        } else {
            Ok(base_result)
        }?;
        if is_debug {
            let expr_span = self.ast.expressions.get_span(expr_id);
            eprintln!(
                "DEBUG EXPR\n{}\nRESULT\n{}",
                self.ast.get_span_content(expr_span),
                self.expr_to_string_with_type(&result)
            );
            self.pop_debug_level();
        };
        Ok(result)
    }

    fn eval_expr_inner(
        &mut self,
        expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        debug!(
            "{}\neval_expr_inner: {}: {}",
            self.ast.get_span_content(self.ast.expressions.get_span(expr_id)),
            &self.ast.expr_id_to_string(expr_id),
            expected_type.map(|t| self.type_id_to_string(t)).unwrap_or("nohint".to_string())
        );
        let expr = self.ast.expressions.get(expr_id);
        match expr {
            ParsedExpression::ListLiteral(list_expr) => {
                let expected_element_type: Option<TypeId> = match expected_type {
                    Some(type_id) => match self.types.get(type_id).as_list_instance() {
                        Some(arr) => Ok(Some(arr.element_type)),
                        None => Ok(None),
                    },
                    None => Ok(None),
                }?;
                let span = list_expr.span;
                let parsed_elements = list_expr.elements.clone();
                let element_count = parsed_elements.len();

                let mut list_lit_block = self.synth_block(vec![], scope_id, span);
                let list_lit_scope = list_lit_block.scope_id;
                let mut element_type = None;
                let elements: Vec<TypedExpr> = {
                    let mut elements = Vec::with_capacity(element_count);
                    for elem in parsed_elements.iter() {
                        let element_expr = self.eval_expr(
                            *elem,
                            scope_id,
                            element_type.or(expected_element_type),
                        )?;
                        if element_type.is_none() {
                            element_type = Some(element_expr.get_type())
                        } else if let Err(msg) = self.check_types(
                            element_type.unwrap(),
                            element_expr.get_type(),
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
                let Some(element_type) = element_type.or(expected_element_type) else {
                    return failf!(span, "Not enough information to determine empty list type");
                };
                let list_new_fn_call = self.synth_typed_function_call(
                    qident!(self, span, ["List"], "withCapacity"),
                    vec![element_type],
                    vec![TypedExpr::Integer(TypedIntegerExpr {
                        value: TypedIntegerValue::U64(element_count as u64),
                        span,
                    })],
                    list_lit_scope,
                    None,
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
                    let push_element = TypedStmt::Expr(Box::new(self.synth_typed_function_call(
                        qident!(self, span, ["List"], "push"),
                        vec![element_type],
                        vec![list_variable.variable_expr.clone(), element_value_expr],
                        list_lit_scope,
                        None,
                    )?));
                    set_elements.push(push_element);
                }
                list_lit_block.statements.push(list_variable.defn_stmt);
                list_lit_block.statements.extend(set_elements);
                let dereference_list_literal = self.synth_dereference(list_variable.variable_expr);
                list_lit_block.push_expr(dereference_list_literal);
                Ok(TypedExpr::Block(list_lit_block))
            }
            ParsedExpression::Struct(ast_struct) => {
                let mut field_values = Vec::new();
                let mut field_defns = Vec::new();
                let ast_struct = ast_struct.clone();
                let expected_struct = if let Some(expected_type) = expected_type {
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
                        None => self.ast.expressions.add_expression(ParsedExpression::Variable(
                            parse::Variable {
                                name: NamespacedIdentifier::naked(ast_field.name, ast_field.span),
                            },
                        )),
                        Some(expr) => *expr,
                    };
                    let expr = self.eval_expr(parsed_expr, scope_id, expected_type_id)?;
                    field_defns.push(StructTypeField {
                        name: ast_field.name,
                        type_id: expr.get_type(),
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
                        match self.typecheck_struct(&expected_struct, &struct_type, scope_id) {
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
                Ok(expr)
            }
            ParsedExpression::If(if_expr) => {
                self.eval_if_expr(&if_expr.clone(), scope_id, expected_type)
            }
            ParsedExpression::While(while_expr) => {
                self.eval_while_loop(&while_expr.clone(), scope_id, expected_type)
            }
            ParsedExpression::Loop(loop_expr) => {
                self.eval_loop_expr(&loop_expr.clone(), scope_id, expected_type)
            }
            ParsedExpression::BinaryOp(_binary_op) => {
                self.eval_binary_op(expr_id, scope_id, expected_type)
            }
            ParsedExpression::UnaryOp(op) => {
                let op = op.clone();
                match op.op_kind {
                    ParsedUnaryOpKind::BooleanNegation => {
                        let negated_expr = self.synth_bool_not(op.expr);
                        self.eval_expr(negated_expr, scope_id, expected_type)
                    }
                }
            }
            ParsedExpression::Literal(Literal::Unit(span)) => Ok(TypedExpr::Unit(*span)),
            ParsedExpression::Literal(Literal::Char(byte, span)) => {
                Ok(TypedExpr::Char(*byte, *span))
            }
            ParsedExpression::Literal(Literal::Numeric(int)) => {
                self.eval_numeric_value(&int.text, int.span, expected_type)
            }
            ParsedExpression::Literal(Literal::Bool(b, span)) => {
                let expr = TypedExpr::Bool(*b, *span);
                Ok(expr)
            }
            ParsedExpression::Literal(Literal::String(s, span)) => {
                let expr = TypedExpr::Str(s.clone(), *span);
                Ok(expr)
            }
            ParsedExpression::Variable(_variable) => self.eval_variable(expr_id, scope_id, false),
            ParsedExpression::FieldAccess(field_access) => {
                let field_access = field_access.clone();
                self.eval_field_access(&field_access, scope_id, false, expected_type)
            }
            ParsedExpression::Block(block) => {
                // TODO(clone big) This clone is actually sad because Block is still big. We need to intern blocks
                let block = block.clone();
                let block_scope =
                    self.scopes.add_child_scope(scope_id, ScopeType::LexicalBlock, None, None);
                let block = self.eval_block(&block, block_scope, expected_type, false)?;
                Ok(TypedExpr::Block(block))
            }
            ParsedExpression::FnCall(fn_call) => {
                self.eval_function_call(&fn_call.clone(), scope_id, expected_type, None)
            }
            ParsedExpression::For(for_expr) => {
                self.eval_for_expr(&for_expr.clone(), scope_id, expected_type)
            }
            ParsedExpression::AnonEnumVariant(anon_enum) => {
                let Some(expected_type) = expected_type else {
                    return failf!(anon_enum.span, "Could not infer enum type from context");
                };
                let (expected_enum, expected_variant_name) = match self.types.get(expected_type) {
                    Type::EnumVariant(ev) => {
                        (self.types.get(ev.enum_type_id).expect_enum(), Some(ev.name))
                    }
                    Type::Enum(e) => (e, None),
                    _ => return failf!(anon_enum.span, "Expected type is not enum"),
                };

                if let Some(matching_variant) = expected_enum.variant_by_name(anon_enum.name) {
                    let None = matching_variant.payload else {
                        return failf!(anon_enum.span, "Enum variant requires payload");
                    };
                    if let Some(expected_variant_name) = expected_variant_name {
                        if matching_variant.name != expected_variant_name {
                            return failf!(
                                anon_enum.span,
                                "Expected variant {}",
                                self.name_of(expected_variant_name)
                            );
                        }
                    }
                    let span = anon_enum.span;
                    Ok(TypedExpr::EnumConstructor(TypedEnumConstructor {
                        type_id: expected_type,
                        variant_name: matching_variant.name,
                        variant_index: matching_variant.index,
                        payload: None,
                        span,
                    }))
                } else {
                    failf!(anon_enum.span, "No variant named {}", self.name_of(anon_enum.name))
                }
            }
            ParsedExpression::EnumDotConstructor(e) => {
                let span = e.span;
                let expected_type = expected_type.ok_or(make_error(
                    "Could not infer enum type from context; try supplying the name",
                    e.span,
                ))?;
                let enum_type_id = {
                    match self.types.get(expected_type) {
                        Type::Enum(_e) => Ok(expected_type),
                        Type::EnumVariant(ev) => Ok(ev.enum_type_id),
                        _ => failf!(
                            e.span,
                            "Could not infer expected enum type for '.' shorthand; expected type was {}",
                            self.type_id_to_string(expected_type)
                        ),
                    }
                }?;
                self.eval_enum_constructor(
                    enum_type_id,
                    e.variant_name,
                    Some(e.payload),
                    Some(expected_type),
                    scope_id,
                    span,
                )
            }
            ParsedExpression::Is(is_expr) => {
                let is_expr = is_expr.clone();
                // If the 'is' is attached to an if/else, that is handled by if/else
                // This is just the case of the detached 'is' where we want to return a boolean
                // indicating whether or not the pattern matched only
                let true_expression = self.ast.expressions.add_expression(
                    parse::ParsedExpression::Literal(parse::Literal::Bool(true, is_expr.span)),
                );
                let false_expression = self.ast.expressions.add_expression(
                    parse::ParsedExpression::Literal(parse::Literal::Bool(false, is_expr.span)),
                );
                let true_case = parse::ParsedMatchCase {
                    pattern: is_expr.pattern,
                    expression: true_expression,
                };
                let wildcard_pattern =
                    self.ast.patterns.add_pattern(parse::ParsedPattern::Wildcard(is_expr.span));
                let false_case = parse::ParsedMatchCase {
                    pattern: wildcard_pattern,
                    expression: false_expression,
                };
                let as_match_expr = parse::ParsedMatchExpression {
                    target_expression: is_expr.target_expression,
                    cases: vec![true_case, false_case],
                    span: is_expr.span,
                };
                let match_expr_id = self
                    .ast
                    .expressions
                    .add_expression(parse::ParsedExpression::Match(as_match_expr));
                let partial_match = true;
                self.eval_match_expr(match_expr_id, scope_id, expected_type, partial_match)
            }
            ParsedExpression::Match(_match_expr) => {
                let partial_match = false;
                self.eval_match_expr(expr_id, scope_id, expected_type, partial_match)
            }
            ParsedExpression::AsCast(_cast) => self.eval_cast(expr_id, scope_id),
            ParsedExpression::Closure(_closure) => {
                self.eval_closure(expr_id, scope_id, expected_type)
            }
            ParsedExpression::InterpolatedString(_is) => {
                let res = self.eval_interpolated_string(expr_id, scope_id, expected_type)?;
                Ok(res)
            }
        }
    }

    fn eval_while_loop(
        &mut self,
        while_expr: &ParsedWhileExpr,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let cond = self.eval_expr(while_expr.cond, scope_id, Some(BOOL_TYPE_ID))?;
        if let Err(msg) = self.check_types(BOOL_TYPE_ID, cond.get_type(), scope_id) {
            return failf!(cond.get_span(), "Invalid while condition type: {msg}");
        }

        let ParsedExpression::Block(parsed_block) = self.ast.expressions.get(while_expr.body)
        else {
            return failf!(while_expr.span, "'while' body must be a block");
        };
        let body_scope =
            self.scopes.add_child_scope(scope_id, ScopeType::WhileLoopBody, None, None);
        self.scopes.add_loop_info(body_scope, ScopeLoopInfo { break_type: Some(UNIT_TYPE_ID) });

        let block = self.eval_block(&parsed_block.clone(), body_scope, expected_type, false)?;
        let loop_type = if cond.get_type() == NEVER_TYPE_ID { NEVER_TYPE_ID } else { UNIT_TYPE_ID };
        Ok(TypedExpr::WhileLoop(WhileLoop {
            cond: Box::new(cond),
            body: Box::new(block),
            type_id: loop_type,
            span: while_expr.span,
        }))
    }

    fn eval_loop_expr(
        &mut self,
        loop_expr: &ParsedLoopExpr,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let body_scope = self.scopes.add_child_scope(scope_id, ScopeType::LoopExprBody, None, None);
        self.scopes.add_loop_info(body_scope, ScopeLoopInfo { break_type: expected_type });

        // Expected type is handled by loop info above, its needed by 'break's but notably we do not
        // want to require the loop's block to return a type other than Unit, so we pass None.
        let expected_expression_type_for_block = None;
        let block = self.eval_block(
            &loop_expr.body.clone(),
            body_scope,
            expected_expression_type_for_block,
            false,
        )?;

        let loop_info = self.scopes.get_loop_info(body_scope).unwrap();

        Ok(TypedExpr::LoopExpr(LoopExpr {
            body: Box::new(block),
            break_type: loop_info.break_type.unwrap_or(UNIT_TYPE_ID),
            span: loop_expr.span,
        }))
    }

    fn eval_interpolated_string(
        &mut self,
        expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let span = self.ast.expressions.get_span(expr_id);
        let ParsedExpression::InterpolatedString(is) = self.ast.expressions.get(expr_id) else {
            panic!()
        };
        let is = is.clone();

        let mut block = self.synth_block(vec![], scope_id, span);
        let block_scope = block.scope_id;
        let part_count = is.parts.len();
        let new_string_builder = self.synth_typed_function_call(
            qident!(self, span, ["StringBuilder"], "withCapacity"),
            vec![],
            vec![TypedExpr::Integer(TypedIntegerExpr {
                value: TypedIntegerValue::U64(part_count as u64),
                span,
            })],
            block_scope,
            None,
        )?;
        let string_builder_var = self.synth_variable_defn_simple(
            get_ident!(self, "sb"),
            new_string_builder,
            block.scope_id,
        );
        block.push_stmt(string_builder_var.defn_stmt);
        for part in is.parts.into_iter() {
            let string_expr = match part {
                parse::InterpolatedStringPart::String(s) => TypedExpr::Str(s, span),
                parse::InterpolatedStringPart::Identifier(ident) => {
                    let variable_expr_id =
                        self.ast.expressions.add_expression(ParsedExpression::Variable(
                            parse::Variable { name: NamespacedIdentifier::naked(ident, span) },
                        ));
                    self.synth_show_ident_call(variable_expr_id, block_scope)?
                }
            };
            debug_assert!(string_expr.get_type() == STRING_TYPE_ID);
            let push_call = self.synth_typed_function_call(
                qident!(self, span, ["StringBuilder"], "putString"),
                vec![],
                vec![string_builder_var.variable_expr.clone(), string_expr],
                block_scope,
                None,
            )?;
            block.push_expr(push_call);
        }
        let build_call = self.synth_typed_function_call(
            qident!(self, span, ["StringBuilder"], "build"),
            vec![],
            vec![string_builder_var.variable_expr],
            block_scope,
            None,
        )?;
        block.push_expr(build_call);
        Ok(TypedExpr::Block(block))
    }

    fn visit_inner_stmt_exprs_mut(stmt: &mut TypedStmt, action: &mut impl FnMut(&mut TypedExpr)) {
        match stmt {
            TypedStmt::Expr(e) => action(e),
            TypedStmt::Let(val_def) => action(&mut val_def.initializer),
            TypedStmt::Assignment(assgn) => {
                action(&mut assgn.destination);
                action(&mut assgn.value)
            }
        }
    }

    fn visit_inner_exprs_mut(expr: &mut TypedExpr, mut action: impl FnMut(&mut TypedExpr)) {
        // Try implementing a mutable iterator instead
        match expr {
            TypedExpr::Unit(_) => (),
            TypedExpr::Char(_, _) => (),
            TypedExpr::Bool(_, _) => (),
            TypedExpr::Integer(_) => (),
            TypedExpr::Float(_) => (),
            TypedExpr::Str(_, _) => (),
            TypedExpr::Struct(s) => {
                for f in s.fields.iter_mut() {
                    action(&mut f.expr);
                }
            }
            TypedExpr::Variable(_) => (),
            TypedExpr::StructFieldAccess(field_access) => {
                action(&mut field_access.base);
            }
            TypedExpr::BinaryOp(binary_op) => {
                action(&mut binary_op.lhs);
                action(&mut binary_op.rhs);
            }
            TypedExpr::UnaryOp(unary_op) => {
                action(&mut unary_op.expr);
            }
            TypedExpr::Block(block) => {
                for stmt in block.statements.iter_mut() {
                    TypedModule::visit_inner_stmt_exprs_mut(stmt, &mut action);
                }
            }
            TypedExpr::Call(call) => {
                match &mut call.callee {
                    Callee::DynamicClosure(expr) => action(expr),
                    Callee::DynamicFunction(expr) => action(expr),
                    _ => {}
                };
                for arg in call.args.iter_mut() {
                    action(arg)
                }
            }
            TypedExpr::If(typed_if) => {
                action(&mut typed_if.condition);
                action(&mut typed_if.consequent);
                action(&mut typed_if.alternate);
            }
            TypedExpr::WhileLoop(while_loop) => {
                action(&mut while_loop.cond);
                for stmt in while_loop.body.statements.iter_mut() {
                    TypedModule::visit_inner_stmt_exprs_mut(stmt, &mut action);
                }
            }
            TypedExpr::LoopExpr(loop_expr) => {
                for stmt in loop_expr.body.statements.iter_mut() {
                    TypedModule::visit_inner_stmt_exprs_mut(stmt, &mut action);
                }
            }
            TypedExpr::EnumConstructor(_) => (),
            TypedExpr::EnumIsVariant(enum_is_variant) => action(&mut enum_is_variant.target_expr),
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                action(&mut enum_get_payload.target_expr)
            }
            TypedExpr::Cast(cast) => action(&mut cast.base_expr),
            TypedExpr::Return(ret) => action(&mut ret.value),
            TypedExpr::Break(brk) => action(&mut brk.value),
            TypedExpr::Closure(_) => (),
            TypedExpr::FunctionName(_) => (),
            TypedExpr::PendingCapture(_) => (),
        }
    }

    fn eval_closure(
        &mut self,
        expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        fn fixup_capture_expr(
            sself: &mut TypedModule,
            body: &mut TypedExpr,
            environment_param: VariableId,
            env_struct_type: TypeId,
        ) {
            match body {
                TypedExpr::PendingCapture(pc) => {
                    let v = sself.variables.get(pc.captured_variable_id);
                    let env_struct_reference_type = sself.types.add_reference_type(env_struct_type);
                    let (_field_index, env_struct_field) = sself
                        .types
                        .get(env_struct_type)
                        .expect_struct()
                        .find_field(v.name)
                        .unwrap();
                    let env_field_access = TypedExpr::StructFieldAccess(FieldAccess {
                        base: Box::new(sself.synth_dereference(TypedExpr::Variable(
                            VariableExpr {
                                variable_id: environment_param,
                                type_id: env_struct_reference_type,
                                span: pc.span,
                            },
                        ))),
                        target_field: env_struct_field.name,
                        target_field_index: env_struct_field.index,
                        result_type: v.type_id,
                        struct_type: env_struct_type,
                        is_referencing: false,
                        span: pc.span,
                    });
                    let _ = std::mem::replace(body, env_field_access);
                }
                other => TypedModule::visit_inner_exprs_mut(other, |expr| {
                    fixup_capture_expr(sself, expr, environment_param, env_struct_type)
                }),
            }
        }
        let closure = self.ast.expressions.get(expr_id).expect_closure();
        let closure_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::ClosureScope, None, None);
        let closure_arguments = closure.arguments.clone();
        let closure_body = closure.body;
        let span = closure.span;
        let body_span = self.ast.expressions.get_span(closure.body);
        let mut typed_params = VecDeque::with_capacity(closure_arguments.len() + 1);
        let expected_function_type =
            expected_type.and_then(|et| self.types.get(et).as_function()).cloned();
        let declared_expected_return_type = match closure.return_type {
            None => None,
            Some(return_type_expr) => Some(self.eval_type_expr(return_type_expr, scope_id)?),
        };
        let expected_return_type = declared_expected_return_type
            .or(expected_function_type.as_ref().map(|f| f.return_type));

        self.scopes.add_closure_info(closure_scope_id, ScopeClosureInfo { expected_return_type });

        for (index, arg) in closure_arguments.iter().enumerate() {
            let arg_type_id = match arg.ty {
                Some(type_expr) => self.eval_type_expr(type_expr, scope_id)?,
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
                is_closure_env: false,
                span: arg.span,
            });
        }

        let mut param_variables = VecDeque::with_capacity(typed_params.len());
        let closure_scope = self.scopes.get_scope_mut(closure_scope_id);
        for typed_arg in typed_params.iter() {
            let name = typed_arg.name;
            let variable_id = self.variables.add_variable(Variable {
                name,
                type_id: typed_arg.type_id,
                is_mutable: false,
                owner_scope: closure_scope_id,
                is_context: false,
                is_global: false,
            });
            closure_scope.add_variable(name, variable_id);
            param_variables.push_back(variable_id)
        }

        // Coerce parsed expr to block, call eval_block with needs_terminator = true
        let ast_body_block = match self.ast.expressions.get(closure_body) {
            ParsedExpression::Block(b) => b.clone(),
            other_expr => {
                let block = parse::Block {
                    span: other_expr.get_span(),
                    stmts: vec![parse::ParsedStmt::LoneExpression(closure_body)],
                };
                block
            }
        };
        let body =
            self.eval_block(&ast_body_block, closure_scope_id, expected_return_type, true)?;
        if let Some(expected_return_type) = expected_return_type {
            if let Err(msg) = self.check_types(expected_return_type, body.expr_type, scope_id) {
                return failf!(body.span, "Closure returns incorrect type: {msg}");
            }
        }

        // Note: NEVER hardcoded stuff that would probably prefer to be some
        // sort of principled call to 'unify_types'
        let return_type = match body.expr_type {
            NEVER_TYPE_ID => expected_return_type.unwrap_or(NEVER_TYPE_ID),
            _ => body.expr_type,
        };

        let closure_captures = self.scopes.get_captures(closure_scope_id);
        let env_fields = closure_captures
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
        let env_field_exprs = closure_captures
            .iter()
            .map(|captured_variable_id| {
                let v = self.variables.get(*captured_variable_id);
                TypedExpr::Variable(VariableExpr {
                    type_id: v.type_id,
                    variable_id: *captured_variable_id,
                    span,
                })
            })
            .collect();
        let environment_struct_type = self.types.add_type(Type::Struct(StructType {
            fields: env_fields,
            type_defn_info: None,
            generic_instance_info: None,
            ast_node: expr_id.into(),
        }));
        let environment_struct =
            self.synth_struct_expr(environment_struct_type, env_field_exprs, scope_id, body_span);
        let environment_struct_reference_type =
            self.types.add_reference_type(environment_struct_type);
        let environment_param = FnParamType {
            name: get_ident!(self, CLOSURE_ENV_PARAM_NAME),
            type_id: environment_struct_reference_type,
            is_context: false,
            is_closure_env: true,
            span: body_span,
        };
        let environment_param_variable_id = self.variables.add_variable(Variable {
            name: environment_param.name,
            type_id: environment_struct_reference_type,
            is_mutable: false,
            owner_scope: closure_scope_id,
            is_context: false,
            is_global: false,
        });
        typed_params.push_front(environment_param);
        param_variables.push_front(environment_param_variable_id);

        let environment_param_variable_id = param_variables[0];
        let body = {
            let mut body_as_expr = TypedExpr::Block(body);
            fixup_capture_expr(
                self,
                &mut body_as_expr,
                environment_param_variable_id,
                environment_struct_type,
            );
            let TypedExpr::Block(body) = body_as_expr else { panic!() };
            body
        };

        let function_type = self.types.add_type(Type::Function(FunctionType {
            params: typed_params.into(),
            return_type,
            defn_info: None,
        }));
        let encl_fn_name = self
            .get_function(
                self.scopes
                    .nearest_parent_function(scope_id)
                    .expect("Closure to be inside a function"),
            )
            .name;
        let name = self.ast.identifiers.intern(format!(
            "{}_{{closure}}_{}",
            self.name_of(encl_fn_name),
            closure_scope_id,
        ));
        let name_string = self.make_qualified_name(scope_id, name, "__", true);
        let name = self.ast.identifiers.intern(name_string);

        let body_function_id = self.add_function(TypedFunction {
            name,
            scope: closure_scope_id,
            param_variables: param_variables.into(),
            type_params: vec![],
            body_block: Some(body),
            intrinsic_type: None,
            linkage: Linkage::Standard,
            child_specializations: vec![],
            specialization_info: None,
            parsed_id: expr_id.into(),
            type_id: function_type,
            compiler_debug: false,
            kind: TypedFunctionKind::Closure,
            is_concrete: false,
        });
        let closure_type_id = self.types.add_closure(
            &self.ast.identifiers,
            function_type,
            environment_struct,
            body_function_id,
            expr_id.into(),
        );
        Ok(TypedExpr::Closure(ClosureExpr { closure_type: closure_type_id, span }))
    }

    fn eval_match_expr(
        &mut self,
        match_expr_id: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type_id: Option<TypeId>,
        partial: bool,
    ) -> TyperResult<TypedExpr> {
        let target_expr = {
            let match_expr = self.ast.expressions.get(match_expr_id).as_match().unwrap();
            if match_expr.cases.is_empty() {
                return Err(make_error("Match expression with no arms", match_expr.span));
            }
            self.eval_expr(match_expr.target_expression, scope_id, None)?
        };

        let match_block_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::LexicalBlock, None, None);

        // Mangled; not a user-facing binding
        let match_target_ident = self.ast.identifiers.intern("match_target");
        let target_expr_variable =
            self.synth_variable_defn_simple(match_target_ident, target_expr, scope_id);

        // Reborrow from ast
        let match_expr = self.ast.expressions.get(match_expr_id).as_match().unwrap();
        let match_expr_span = match_expr.span;
        let arms = self.eval_match_arms(
            target_expr_variable.variable_expr.expect_variable(),
            &match_expr.cases.clone(),
            match_block_scope_id,
            expected_type_id,
            partial,
        )?;

        let match_result_type = arms
            .iter()
            .find_map(|arm| {
                if arm.arm_block.expr_type != NEVER_TYPE_ID {
                    Some(arm.arm_block.expr_type)
                } else {
                    None
                }
            })
            .unwrap_or(NEVER_TYPE_ID);
        let mut resulting_block = TypedBlock {
            expr_type: match_result_type,
            scope_id: match_block_scope_id,
            statements: vec![target_expr_variable.defn_stmt],
            span: match_expr_span,
        };
        let mut pre_stmts: Vec<TypedStmt> = Vec::new();
        let mut the_arms = VecDeque::new();
        for match_case in arms.into_iter() {
            pre_stmts.extend(match_case.pre_stmts);
            the_arms.push_back((match_case.condition, match_case.arm_block));
        }
        let if_chain = self.chain_match_cases(
            match_result_type,
            the_arms,
            match_block_scope_id,
            match_expr_span,
        )?;
        resulting_block.statements.extend(pre_stmts);
        resulting_block.push_stmt(TypedStmt::Expr(Box::new(if_chain)));
        debug!("match result\n{}", self.block_to_string(&resulting_block));
        let result = TypedExpr::Block(resulting_block);
        Ok(result)
    }

    fn chain_match_cases(
        &mut self,
        match_result_type: TypeId,
        mut cases: VecDeque<(TypedExpr, TypedBlock)>,
        scope_id: ScopeId,
        span_if_no_cases: SpanId,
    ) -> TyperResult<TypedExpr> {
        if cases.is_empty() {
            let message_expr = TypedExpr::Str("Match Error".to_string(), span_if_no_cases);
            self.synth_typed_function_call(
                qident!(self, span_if_no_cases, "crash"),
                vec![],
                vec![message_expr],
                scope_id,
                None,
            )
        } else {
            let (case_condition, case_body) = cases.pop_front().unwrap();
            // This optimizes out 'if true' and 'if false' which happen quite a lot
            // when evaluating pattern match conditions
            if let TypedExpr::Bool(b, _) = case_condition {
                if b {
                    Ok(TypedExpr::Block(case_body))
                } else {
                    self.chain_match_cases(
                        match_result_type,
                        cases,
                        scope_id,
                        case_condition.get_span(),
                    )
                }
            } else {
                let res = self.chain_match_cases(
                    match_result_type,
                    cases,
                    scope_id,
                    case_condition.get_span(),
                )?;
                // TODO(never unify): Unify types here
                let if_result_type = match (case_body.expr_type, res.get_type()) {
                    (NEVER_TYPE_ID, NEVER_TYPE_ID) => NEVER_TYPE_ID,
                    _ => match_result_type,
                };
                Ok(TypedExpr::If(Box::new(TypedIf {
                    span: case_condition.get_span(),
                    condition: case_condition,
                    consequent: TypedExpr::Block(case_body),
                    alternate: res,
                    ty: if_result_type,
                })))
            }
        }
    }

    fn eval_match_arms(
        &mut self,
        target_expr: VariableExpr,
        cases: &[parse::ParsedMatchCase],
        match_scope_id: ScopeId,
        expected_type_id: Option<TypeId>,
        partial: bool,
    ) -> TyperResult<Vec<TypedMatchCase>> {
        let mut typed_cases: Vec<TypedMatchCase> = Vec::new();

        let mut expected_arm_type_id = expected_type_id;

        for parsed_case in cases.iter() {
            let pattern =
                self.eval_pattern(parsed_case.pattern, target_expr.type_id, match_scope_id)?;
            let arm_expr_span = self.ast.expressions.get_span(parsed_case.expression);
            let arm_pattern_span = self.ast.get_pattern_span(parsed_case.pattern);
            let mut arm_block = self.synth_block(vec![], match_scope_id, arm_expr_span);
            let (pre_stmts, condition) =
                self.eval_match_arm(&pattern, target_expr.clone(), &mut arm_block, match_scope_id)?;

            // Once we've evaluated the arm pattern, we can elet the consequent expression inside of it,
            // since the bindings are now in scope inside arm_block
            let arm_expr =
                self.eval_expr(parsed_case.expression, arm_block.scope_id, expected_arm_type_id)?;

            if let Some(expected_arm_type_id) = expected_arm_type_id.as_ref() {
                // Never is divergent so need not contribute to the overall type of the pattern
                if arm_expr.get_type() != NEVER_TYPE_ID {
                    if let Err(msg) =
                        self.check_types(*expected_arm_type_id, arm_expr.get_type(), match_scope_id)
                    {
                        return failf!(arm_pattern_span, "Match arm has wrong type. {}", msg);
                    }
                }
            }

            arm_block.push_expr(arm_expr);

            if arm_block.expr_type != NEVER_TYPE_ID {
                expected_arm_type_id = Some(arm_block.expr_type);
            }
            typed_cases.push(TypedMatchCase { pattern, pre_stmts, condition, arm_block });
        }

        // Exhaustiveness Checking
        if !partial {
            let trial_constructors: Vec<PatternConstructor> =
                self.generate_constructors_for_type(target_expr.type_id, target_expr.span);
            let mut trial_alives: Vec<bool> = vec![true; trial_constructors.len()];
            let mut pattern_scores: Vec<usize> = vec![0; typed_cases.len()];
            'trial: for (trial_index, trial_expr) in trial_constructors.iter().enumerate() {
                '_pattern: for (index, pattern) in typed_cases.iter().enumerate() {
                    let pattern = &pattern.pattern;
                    if TypedModule::pattern_matches(pattern, trial_expr) {
                        pattern_scores[index] += 1;
                        trial_alives[trial_index] = false;
                        continue 'trial;
                    }
                }
            }

            if let Some(alive_index) = trial_alives.iter().position(|p| *p) {
                let pattern = &trial_constructors[alive_index];
                return failf!(
                    target_expr.span,
                    "Unhandled pattern: {}",
                    self.pattern_ctor_to_string(pattern)
                );
            }

            if let Some(useless_index) = pattern_scores.iter().position(|p| *p == 0) {
                let pattern = &typed_cases[useless_index].pattern;
                if !pattern.is_innumerable_literal() {
                    return failf!(
                        pattern.span_id(),
                        "Useless pattern: {}",
                        self.pattern_to_string(pattern)
                    );
                }
            }
        }

        Ok(typed_cases)
    }

    /// For each match case we output
    /// 1) a series of statements to bind variables that are used by the condition
    ///    FIXME!: These conditions should be in their own block; currently they pollute the match block
    /// 2) an expr that is expected to a boolean, representing the condition of the branch,
    fn eval_match_arm(
        &mut self,
        pattern: &TypedPattern,
        target_expr_variable_expr: VariableExpr,
        arm_block: &mut TypedBlock,
        match_scope_id: ScopeId,
    ) -> TyperResult<(Vec<TypedStmt>, TypedExpr)> {
        match pattern {
            TypedPattern::Struct(struct_pattern) => {
                let mut boolean_exprs: Vec<TypedExpr> =
                    Vec::with_capacity(struct_pattern.fields.len());
                let mut condition_statements: Vec<TypedStmt> =
                    Vec::with_capacity(struct_pattern.fields.len());
                for pattern_field in struct_pattern.fields.iter() {
                    let target_value = TypedExpr::StructFieldAccess(FieldAccess {
                        base: Box::new(TypedExpr::Variable(target_expr_variable_expr.clone())),
                        target_field: pattern_field.name,
                        target_field_index: pattern_field.field_index,
                        result_type: pattern_field.field_type_id,
                        struct_type: target_expr_variable_expr.type_id,
                        is_referencing: false,
                        span: struct_pattern.span,
                    });
                    // I'm putting condition variables in the match scope id, but they really belong in the arms condition scope id, which
                    // we'll make later. This is just more hygienic since the variables needed for each arms condition shouldn't be visible
                    // to the other arms, even though mangled and unique... Maybe this is better because its harmless and more efficient, idk
                    // As long as the conditions are provably never side-effecting!
                    let struct_member_variable = self.synth_variable_defn_simple(
                        pattern_field.name,
                        target_value,
                        match_scope_id,
                    );
                    condition_statements.push(struct_member_variable.defn_stmt);

                    let (inner_condition_stmts, condition) = self.eval_match_arm(
                        &pattern_field.pattern,
                        struct_member_variable.variable_expr.expect_variable(),
                        arm_block,
                        arm_block.scope_id,
                    )?;
                    condition_statements.extend(inner_condition_stmts);

                    boolean_exprs.push(condition);
                }
                let final_condition: TypedExpr = boolean_exprs
                    .into_iter()
                    .reduce(|acc, expr| {
                        TypedExpr::BinaryOp(BinaryOp {
                            kind: BinaryOpKind::And,
                            ty: BOOL_TYPE_ID,
                            lhs: Box::new(acc),
                            rhs: Box::new(expr),
                            span: struct_pattern.span,
                        })
                    })
                    .unwrap();
                Ok((condition_statements, final_condition))
            }
            TypedPattern::Enum(enum_pattern) => {
                // Check tag, that's our condition. Recurse on nested pattern if true
                let is_variant_condition = TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                    target_expr: Box::new(target_expr_variable_expr.clone().into()),
                    variant_name: enum_pattern.variant_tag_name,
                    variant_index: enum_pattern.variant_index,
                    span: enum_pattern.span,
                });
                let mut condition_statements = vec![];
                let (inner_condition_stmts, inner_condition) = if let Some(payload_pattern) =
                    enum_pattern.payload.as_ref()
                {
                    let enum_type = self.types.get(enum_pattern.enum_type_id).expect_enum();
                    let variant = enum_type.variant_by_index(enum_pattern.variant_index).unwrap();
                    let Some(payload_type_id) = variant.payload else {
                        return make_fail_span(
                            "Impossible pattern: variant does not have payload",
                            enum_pattern.span,
                        );
                    };
                    let payload_value = TypedExpr::EnumGetPayload(GetEnumPayload {
                        target_expr: Box::new(target_expr_variable_expr.into()),
                        result_type_id: payload_type_id,
                        variant_name: variant.name,
                        variant_index: variant.index,
                        is_referencing: false,
                        span: enum_pattern.span,
                    });
                    let payload_value_synth_name = get_ident!(self, "payload");
                    let payload_variable = self.synth_variable_defn_simple(
                        payload_value_synth_name,
                        payload_value,
                        arm_block.scope_id,
                    );
                    let mut stmts = vec![payload_variable.defn_stmt];
                    let (inner_pattern_stmts, cond) = self.eval_match_arm(
                        payload_pattern,
                        payload_variable.variable_expr.expect_variable(),
                        arm_block,
                        arm_block.scope_id,
                    )?;
                    stmts.extend(inner_pattern_stmts);
                    (stmts, Some(cond))
                } else {
                    (vec![], None)
                };
                condition_statements.extend(inner_condition_stmts);
                let final_condition = match inner_condition {
                    None => is_variant_condition,
                    Some(TypedExpr::Bool(true, _)) => is_variant_condition,
                    Some(inner_condition) => TypedExpr::BinaryOp(BinaryOp {
                        kind: BinaryOpKind::And,
                        ty: BOOL_TYPE_ID,
                        lhs: Box::new(is_variant_condition),
                        rhs: Box::new(inner_condition),
                        span: enum_pattern.span,
                    }),
                };
                Ok((condition_statements, final_condition))
            }
            TypedPattern::Variable(variable_pattern) => {
                let variable_ident = variable_pattern.name;
                let binding_variable = self.synth_variable_defn_visible(
                    variable_ident,
                    target_expr_variable_expr.into(),
                    arm_block.scope_id,
                );
                arm_block.push_stmt(binding_variable.defn_stmt);
                // `true` because variable patterns always match
                Ok((vec![], TypedExpr::Bool(true, variable_pattern.span)))
            }
            TypedPattern::LiteralUnit(span) => Ok((vec![], TypedExpr::Bool(true, *span))),
            TypedPattern::LiteralChar(byte, span) => {
                let bin_op = self.synth_equals_binop(
                    target_expr_variable_expr.into(),
                    TypedExpr::Char(*byte, *span),
                    *span,
                );
                Ok((vec![], bin_op))
            }
            TypedPattern::LiteralInteger(int_value, span) => {
                let bin_op = BinaryOp {
                    kind: BinaryOpKind::Equals,
                    ty: BOOL_TYPE_ID,
                    lhs: Box::new(target_expr_variable_expr.into()),
                    rhs: Box::new(TypedExpr::Integer(TypedIntegerExpr {
                        value: *int_value,
                        span: *span,
                    })),
                    span: *span,
                };
                Ok((vec![], TypedExpr::BinaryOp(bin_op)))
            }
            TypedPattern::LiteralFloat(float_value, span) => {
                let bin_op = BinaryOp {
                    kind: BinaryOpKind::Equals,
                    ty: BOOL_TYPE_ID,
                    lhs: Box::new(target_expr_variable_expr.into()),
                    rhs: Box::new(TypedExpr::Float(TypedFloatExpr {
                        value: *float_value,
                        span: *span,
                    })),
                    span: *span,
                };
                Ok((vec![], TypedExpr::BinaryOp(bin_op)))
            }
            TypedPattern::LiteralBool(bool_value, span) => {
                let bin_op = self.synth_equals_binop(
                    target_expr_variable_expr.into(),
                    TypedExpr::Bool(*bool_value, *span),
                    *span,
                );
                Ok((vec![], bin_op))
            }
            TypedPattern::LiteralString(string_value, span) => {
                let condition = self.synth_equals_call(
                    target_expr_variable_expr.into(),
                    TypedExpr::Str(string_value.clone(), *span),
                    *span,
                )?;
                Ok((vec![], condition))
            }
            TypedPattern::Wildcard(span) => Ok((vec![], TypedExpr::Bool(true, *span))),
        }
    }

    fn eval_cast(
        &mut self,
        expr_id: ParsedExpressionId,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExpr> {
        let cast = self.ast.expressions.get(expr_id).expect_cast();
        let cast = cast.clone();
        let base_expr = self.eval_expr(cast.base_expr, scope_id, None)?;
        let base_expr_type = base_expr.get_type();
        let target_type = self.eval_type_expr(cast.dest_type, scope_id)?;
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
        Ok(TypedExpr::Cast(TypedCast {
            base_expr: Box::new(base_expr),
            target_type_id: output_type,
            cast_type,
            span: cast.span,
        }))
    }

    fn eval_for_expr(
        &mut self,
        for_expr: &ForExpr,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let binding_ident = for_expr.binding.unwrap_or(get_ident!(self, "it"));
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, scope_id, None)?;
        let iteree_type = iterable_expr.get_type();
        let iterable_span = iterable_expr.get_span();
        let body_span = for_expr.body_block.span;

        let iter_impl =
            self.expect_ability_implementation(iteree_type, ITERABLE_ABILITY_ID, iterable_span)?;
        let item_type = self.get_ability_impl(iter_impl.full_impl_id).impl_arguments[0].type_id;

        let is_do_block = for_expr.expr_type == ForExprType::Do;

        // We de-sugar the 'for ... do' expr into a typed while loop, synthesizing
        // a few local variables in order to achieve this.

        let outer_for_expr_scope =
            self.scopes.add_child_scope(scope_id, ScopeType::ForExpr, None, None);

        let index_variable = self.synth_variable_defn(
            get_ident!(self, "itIndex"),
            TypedExpr::Integer(TypedIntegerExpr {
                value: TypedIntegerValue::U64(0),
                span: for_expr.body_block.span,
            }),
            true,
            true,
            false,
            outer_for_expr_scope,
        );
        let get_iterator_call = self.synth_typed_function_call(
            qident!(self, body_span, ["Iterable"], "iterator"),
            vec![],
            vec![iterable_expr],
            outer_for_expr_scope,
            None,
        )?;
        let iterator_variable = self.synth_variable_defn(
            get_ident!(self, "iter"),
            get_iterator_call,
            false,
            false,
            true, //is_referencing
            outer_for_expr_scope,
        );
        let mut loop_block = self.synth_block(vec![], outer_for_expr_scope, body_span);
        let loop_scope_id = loop_block.scope_id;
        let expected_block_type = expected_type
            .and_then(|t| self.types.get(t).as_list_instance())
            .map(|list_type| list_type.element_type);

        let mut consequent_block = self.synth_block(vec![], loop_scope_id, iterable_span);

        let iterator_next_call = self.synth_typed_function_call(
            qident!(self, body_span, ["Iterator"], "next"),
            vec![],
            vec![iterator_variable.variable_expr.clone()],
            loop_scope_id,
            None,
        )?;
        let next_variable = self.synth_variable_defn_simple(
            get_ident!(self, "next"),
            iterator_next_call,
            loop_scope_id,
        );
        let next_unwrap_call = self.synth_typed_function_call(
            qident!(self, iterable_span, ["Unwrap"], "unwrap"),
            vec![],
            vec![next_variable.variable_expr.clone()],
            consequent_block.scope_id,
            None,
        )?;
        let binding_variable = self.synth_variable_defn_visible(
            binding_ident,
            next_unwrap_call,
            consequent_block.scope_id,
        );
        let body_block = self.eval_block(
            &for_expr.body_block,
            consequent_block.scope_id,
            expected_block_type,
            false,
        )?;
        let body_block_result_type = body_block.expr_type;

        let resulting_type = if is_do_block {
            UNIT_TYPE_ID
        } else {
            self.instantiate_generic_type(LIST_TYPE_ID, vec![body_block_result_type])
        };
        let yielded_coll_variable = if !is_do_block {
            let iterator_deref = self.synth_dereference(iterator_variable.variable_expr.clone());
            let size_hint_call = self.synth_typed_function_call(
                qident!(self, body_span, ["Iterator"], "sizeHint"),
                vec![],
                vec![iterator_deref],
                outer_for_expr_scope,
                None,
            )?;
            let size_hint_lower_bound = TypedExpr::StructFieldAccess(FieldAccess {
                struct_type: size_hint_call.get_type(),
                base: Box::new(size_hint_call),
                target_field: get_ident!(self, "atLeast"),
                target_field_index: 0,
                result_type: U64_TYPE_ID,
                is_referencing: false,
                span: iterable_span,
            });
            let synth_function_call = self.synth_typed_function_call(
                qident!(self, body_span, ["List"], "withCapacity"),
                vec![body_block_result_type],
                vec![size_hint_lower_bound],
                outer_for_expr_scope,
                None,
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

        loop_block.push_stmt(next_variable.defn_stmt); // let next = iter.next();

        let user_block_variable = self.synth_variable_defn_simple(
            get_ident!(self, "block_expr_val"),
            TypedExpr::Block(body_block),
            consequent_block.scope_id,
        );

        consequent_block.statements.push(binding_variable.defn_stmt);
        consequent_block.statements.push(user_block_variable.defn_stmt);
        // Assign element to yielded list
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            let element_assign = TypedStmt::Expr(Box::new(self.synth_typed_function_call(
                qident!(self, body_span, ["List"], "push"),
                vec![body_block_result_type],
                vec![
                    yielded_coll_variable.variable_expr.clone(),
                    user_block_variable.variable_expr,
                ],
                outer_for_expr_scope,
                None,
            )?));
            consequent_block.statements.push(element_assign);
        }

        let next_is_some_call = self.synth_typed_function_call(
            qident!(self, body_span, ["Opt"], "isSome"),
            vec![item_type],
            vec![next_variable.variable_expr],
            loop_scope_id,
            None,
        )?;
        let break_block = self.synth_block(
            vec![TypedStmt::Expr(Box::new(TypedExpr::Break(TypedBreak {
                value: Box::new(TypedExpr::Unit(body_span)),
                loop_scope: loop_scope_id,
                loop_type: LoopType::Loop,
                span: body_span,
            })))],
            loop_scope_id,
            body_span,
        );
        loop_block.push_expr(TypedExpr::If(Box::new(TypedIf {
            condition: next_is_some_call,
            consequent: TypedExpr::Block(consequent_block),
            alternate: TypedExpr::Block(break_block),
            ty: UNIT_TYPE_ID,
            span: body_span,
        })));

        // Append the index increment to the body block
        let index_increment_statement = TypedStmt::Assignment(Box::new(Assignment {
            destination: Box::new(index_variable.variable_expr.clone()),
            value: Box::new(TypedExpr::BinaryOp(BinaryOp {
                kind: BinaryOpKind::Add,
                ty: U64_TYPE_ID,
                lhs: Box::new(index_variable.variable_expr.clone()),
                rhs: Box::new(TypedExpr::Integer(TypedIntegerExpr {
                    value: TypedIntegerValue::U64(1),
                    span: iterable_span,
                })),
                span: iterable_span,
            })),
            span: iterable_span,
            kind: AssignmentKind::Value,
        }));
        loop_block.statements.push(index_increment_statement);

        let loop_expr = TypedExpr::LoopExpr(LoopExpr {
            body: Box::new(loop_block),
            break_type: UNIT_TYPE_ID,
            span: for_expr.span,
        });

        let mut for_expr_initial_statements = Vec::with_capacity(4);
        for_expr_initial_statements.push(index_variable.defn_stmt);
        for_expr_initial_statements.push(iterator_variable.defn_stmt);
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            for_expr_initial_statements.push(yielded_coll_variable.defn_stmt.clone());
        }
        let mut for_expr_block = TypedBlock {
            expr_type: resulting_type,
            scope_id: outer_for_expr_scope,
            statements: for_expr_initial_statements,
            span: for_expr.body_block.span,
        };

        for_expr_block.push_expr(loop_expr);
        if let Some(yielded_coll_variable) = yielded_coll_variable {
            let yield_expr = self.synth_dereference(yielded_coll_variable.variable_expr);
            for_expr_block.push_expr(yield_expr);
        }

        let final_expr = TypedExpr::Block(for_expr_block);
        Ok(final_expr)
    }

    fn expect_ability_implementation(
        &mut self,
        type_id: TypeId,
        ability_id: AbilityId,
        span_for_error: SpanId,
    ) -> TyperResult<AbilityImplHandle> {
        self.find_ability_impl_for_type(type_id, ability_id, span_for_error).ok_or(errf!(
                span_for_error,
                    "Missing ability '{}' implementation for '{}'. It implements the following abilities:\n{}",
                    self.name_of(self.get_ability(ability_id).name),
                    self.type_id_to_string(type_id),
        &self
            .ability_impl_table
            .get(&type_id)
            .unwrap_or(&vec![])
            .iter()
            .map(|h| self.ability_signature_to_string(h.ability_id, &self.get_ability_impl(h.full_impl_id).impl_arguments))
            .collect::<Vec<_>>()
            .join("\n")
                    )
            )
    }

    fn eval_binary_op(
        &mut self,
        binary_op_id: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        fn is_scalar_for_equals(type_id: TypeId) -> bool {
            match type_id {
                UNIT_TYPE_ID | CHAR_TYPE_ID | U8_TYPE_ID | U16_TYPE_ID | U32_TYPE_ID
                | U64_TYPE_ID | I8_TYPE_ID | I16_TYPE_ID | I32_TYPE_ID | I64_TYPE_ID
                | BOOL_TYPE_ID => true,
                F32_TYPE_ID | F64_TYPE_ID => true,
                _other => false,
            }
        }
        // Special cases: Equality, OptionalElse, and Pipe
        let ParsedExpression::BinaryOp(binary_op) = self.ast.expressions.get(binary_op_id).clone()
        else {
            unreachable!()
        };
        match binary_op.op_kind {
            BinaryOpKind::Pipe => {
                return self.eval_pipe_expr(
                    binary_op.lhs,
                    binary_op.rhs,
                    scope_id,
                    expected_type,
                    binary_op.span,
                );
            }
            BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                let lhs = self.eval_expr(binary_op.lhs, scope_id, None)?;
                if !is_scalar_for_equals(lhs.get_type()) {
                    return self.eval_equality_expr(binary_op_id, scope_id, expected_type);
                }
            }
            BinaryOpKind::OptionalElse => {
                return self.eval_optional_else(
                    binary_op.lhs,
                    binary_op.rhs,
                    scope_id,
                    binary_op.span,
                )
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
        let lhs = self.eval_expr(binary_op.lhs, scope_id, None)?;
        let kind = binary_op.op_kind;
        let lhs_type_id = lhs.get_type();
        let result_type = match self.types.get(lhs_type_id) {
            Type::Never(_) => Ok(NEVER_TYPE_ID),
            Type::Float(_) | Type::Integer(_) => match kind {
                BinaryOpKind::Add => Ok(lhs_type_id),
                BinaryOpKind::Subtract => Ok(lhs_type_id),
                BinaryOpKind::Multiply => Ok(lhs_type_id),
                BinaryOpKind::Divide => Ok(lhs_type_id),
                BinaryOpKind::Rem => Ok(lhs_type_id),
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
            _other => {
                failf!(binary_op.span, "Invalid left-hand side of binary operation {}", kind)
            }
        }?;

        debug_assert!(kind.is_symmetric_binop());
        // At this point I think all operations are symmetric but we'll leave this here
        // to signal that invariant and in case things change
        let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs.get_type()))?;

        let result_type = match rhs.get_type() {
            NEVER_TYPE_ID => NEVER_TYPE_ID,
            _other => result_type,
        };

        // We already confirmed that the LHS is valid for this operation, and
        // if the op is symmetric, we just have to check the RHS matches
        if let Err(msg) = self.check_types(lhs.get_type(), rhs.get_type(), scope_id) {
            return failf!(rhs.get_span(), "operand types did not match: {msg}");
        }

        let expr = TypedExpr::BinaryOp(BinaryOp {
            kind,
            ty: result_type,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: binary_op.span,
        });
        Ok(expr)
    }

    fn eval_optional_else(
        &mut self,
        lhs: ParsedExpressionId,
        rhs: ParsedExpressionId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        // LHS must implement Unwrap and RHS must be its contained type
        let lhs = self.eval_expr(lhs, scope_id, None)?;
        let unwrap_impl = self
            .expect_ability_implementation(lhs.get_type(), UNWRAP_ABILITY_ID, span)
            .map_err(|e| {
                errf!(
                    span,
                    "'?' operator can only be used on a type that implements `Unwrap`. {}",
                    e.message,
                )
            })?;
        let unwrap_impl = self.get_ability_impl(unwrap_impl.full_impl_id);
        let output_type = unwrap_impl.impl_arguments[0].type_id;

        let rhs = self.eval_expr(rhs, scope_id, Some(output_type))?;
        let rhs_type = rhs.get_type();
        if let Err(msg) = self.check_types(output_type, rhs_type, scope_id) {
            return failf!(span, "RHS value incompatible with `Unwrap` output of LHS: {}", msg);
        }
        let mut coalesce_block = self.synth_block(vec![], scope_id, span);
        let lhs_variable = self.synth_variable_defn_simple(
            get_ident!(self, "optelse_lhs"),
            lhs,
            coalesce_block.scope_id,
        );
        let lhs_has_value = self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "hasValue"),
            vec![],
            vec![lhs_variable.variable_expr.clone()],
            coalesce_block.scope_id,
            None,
        )?;
        let lhs_get_expr = self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "unwrap"),
            vec![],
            vec![lhs_variable.variable_expr],
            coalesce_block.scope_id,
            None,
        )?;

        let if_else = TypedExpr::If(Box::new(TypedIf {
            condition: lhs_has_value,
            consequent: lhs_get_expr,
            alternate: rhs,
            ty: output_type,
            span,
        }));
        coalesce_block.push_stmt(lhs_variable.defn_stmt);
        coalesce_block.push_expr(if_else);
        Ok(TypedExpr::Block(coalesce_block))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op_id: ParsedExpressionId,
        scope_id: ScopeId,
        _expected_type_id: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let ParsedExpression::BinaryOp(binary_op) = self.ast.expressions.get(binary_op_id).clone()
        else {
            unreachable!()
        };

        let lhs = self.eval_expr(binary_op.lhs, scope_id, None)?;
        let lhs_type = lhs.get_type();
        let equality_result = match self.types.get(lhs_type) {
            Type::TypeVariable(_type_var) => {
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs_type))?;
                Ok(TypedExpr::BinaryOp(BinaryOp {
                    kind: BinaryOpKind::Equals,
                    ty: BOOL_TYPE_ID,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                }))
            }
            other_lhs_type => {
                if other_lhs_type.is_scalar_int_value() {
                    panic!("Scalar ints shouldnt be passed to eval_equality_expr")
                }
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs_type))?;
                if rhs.get_type() != lhs_type {
                    failf!(
                        binary_op.span,
                        "Right hand side type '{}' did not match {}",
                        self.type_id_to_string(rhs.get_type()),
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
                vec![],
                vec![equality_result],
                scope_id,
                _expected_type_id,
            )?,
            _ => unreachable!(),
        };
        Ok(final_result)
    }

    fn eval_pipe_expr(
        &mut self,
        lhs: ParsedExpressionId,
        rhs: ParsedExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let new_fn_call = match self.ast.expressions.get(rhs) {
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
                    self.ast.expressions.get_span(rhs),
                    "rhs of pipe must be function call or function name",
                )
            }
        };
        let new_fn_call_id =
            self.ast.expressions.add_expression(ParsedExpression::FnCall(new_fn_call));
        let new_fn_call_clone = self.ast.expressions.get(new_fn_call_id).expect_fn_call().clone();
        self.eval_function_call(&new_fn_call_clone, scope_id, expected_type, None)
    }

    fn synth_equals_call(
        &mut self,
        lhs: TypedExpr,
        rhs: TypedExpr,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let implementation =
            self.expect_ability_implementation(lhs.get_type(), EQUALS_ABILITY_ID, span)?;
        let implementation = self.get_ability_impl(implementation.full_impl_id);
        let ability = self.get_ability(EQUALS_ABILITY_ID);
        let equals_index = ability.find_function_by_name(get_ident!(self, "equals")).unwrap().0;
        let equals_implementation_function_id = implementation.function_at_index(equals_index);
        let call_expr = TypedExpr::Call(Call {
            callee: Callee::make_static(equals_implementation_function_id),
            args: vec![lhs, rhs],
            type_args: Vec::new(),
            return_type: BOOL_TYPE_ID,
            span,
        });
        Ok(call_expr)
    }

    fn eval_if_expr(
        &mut self,
        if_expr: &IfExpr,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        // If the condition is an IsExpr, we desugar to a match
        let cond = self.ast.expressions.get(if_expr.cond);
        if let ParsedExpression::Is(is_expr) = cond {
            let cond_pattern_id = is_expr.pattern;
            let cond_expr_id = is_expr.target_expression;
            let alternate_expr = match if_expr.alt {
                None => {
                    let id = self
                        .ast
                        .expressions
                        .add_expression(ParsedExpression::Literal(Literal::Unit(if_expr.span)));
                    id
                }
                Some(alt) => alt,
            };
            let wildcard_pattern_id =
                self.ast.patterns.add_pattern(parse::ParsedPattern::Wildcard(if_expr.span));
            let cases = vec![
                parse::ParsedMatchCase { pattern: cond_pattern_id, expression: if_expr.cons },
                parse::ParsedMatchCase { pattern: wildcard_pattern_id, expression: alternate_expr },
            ];
            let match_expr = parse::ParsedMatchExpression {
                target_expression: cond_expr_id,
                cases,
                span: if_expr.span,
            };
            // This could definitely double-borrow expressions
            let match_expr_id =
                self.ast.expressions.add_expression(ParsedExpression::Match(match_expr));
            // eprintln!("desugared if to match {}", self.ast.expression_to_string(match_expr_id));
            return self.eval_match_expr(match_expr_id, scope_id, expected_type, true);
        }
        //
        // End of match desugar case

        let condition = self.eval_expr(if_expr.cond, scope_id, None)?;

        let mut consequent = {
            if let Err(msg) = self.check_types(BOOL_TYPE_ID, condition.get_type(), scope_id) {
                return make_fail_span(
                    format!("Invalid if condition type: {}.", msg),
                    condition.get_span(),
                );
            }
            self.eval_expr(if_expr.cons, scope_id, expected_type)?
        };
        // De-sugar if without else:
        // If there is no alternate, we coerce the consequent to return Unit, so both
        // branches have a matching type, making codegen simpler
        // However, if the consequent is a never type, we don't need to do this, in
        // fact we can't because then we'd have an expression following a never expression
        let cons_never = consequent.get_type() == NEVER_TYPE_ID;

        if if_expr.alt.is_none() && !cons_never {
            let mut consequent_as_block = self.coerce_expr_to_block(consequent, scope_id);
            self.coerce_block_to_unit_block(&mut consequent_as_block);
            consequent = TypedExpr::Block(consequent_as_block)
        };
        let alternate = if let Some(alt) = if_expr.alt {
            let type_hint = if cons_never { None } else { Some(consequent.get_type()) };
            self.eval_expr(alt, scope_id, type_hint)?
        } else {
            let unit_expr = TypedExpr::Unit(if_expr.span);
            unit_expr
        };

        // Special typechecking that accounts for 'never'.
        // I don't think this scales; we probably need to typecheck this way in other places.
        // But I don't yet fully understand the pattern or how to extract it;
        // perhaps its a parameter to typecheck_types, or a function that delegates to typecheck types.
        // I think its different in that it takes in a list of types (think pattern matching where we have more than 2 branches)

        // Then essentially discards the 'never' types and runs resolution as if they weren't there. imagine
        // switch x {
        //   , A -> crash()
        //   , B -> crash()
        //   , C -> 42
        //   , D -> crash()
        // }: int
        let cons_never = consequent.get_type() == NEVER_TYPE_ID;
        let alt_never = alternate.get_type() == NEVER_TYPE_ID;
        let no_never = !cons_never && !alt_never;

        let overall_type = if no_never {
            if let Err(msg) =
                self.check_types(consequent.get_type(), alternate.get_type(), scope_id)
            {
                return make_fail_span(
                    format!("else branch type did not match then branch type: {}", msg),
                    alternate.get_span(),
                );
            };
            consequent.get_type()
        } else {
            if cons_never {
                alternate.get_type()
            } else {
                consequent.get_type()
            }
        };
        Ok(TypedExpr::If(Box::new(TypedIf {
            condition,
            consequent,
            alternate,
            ty: overall_type,
            span: if_expr.span,
        })))
    }

    fn _make_unit_block(&mut self, scope_id: ScopeId, span: SpanId) -> TypedBlock {
        let unit_expr = TypedExpr::Unit(span);
        let b = self.synth_block(vec![TypedStmt::Expr(Box::new(unit_expr))], scope_id, span);
        b
    }

    /// Can 'shortcircuit' with Left if the function call to resolve
    /// is actually a builtin
    fn resolve_parsed_function_call(
        &mut self,
        fn_call: &FnCall,
        known_args: Option<&(Vec<TypeId>, Vec<TypedExpr>)>,
        calling_scope: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<Either<TypedExpr, Callee>> {
        let call_span = fn_call.span;
        if let Some(builtin_result) =
            self.handle_builtin_function_call_lookalikes(fn_call, calling_scope)?
        {
            return Ok(Either::Left(builtin_result));
        }

        let first_arg_expr: Option<MaybeTypedExpr> = match known_args.as_ref() {
            Some((_, args)) if !args.is_empty() => {
                Some(MaybeTypedExpr::Typed(args.first().unwrap().clone()))
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
                calling_scope,
                expected_type,
            ),
            false => {
                if let Some(function_id) = self.scopes.find_function_namespaced(
                    calling_scope,
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
                            calling_scope,
                            expected_type,
                        )?;
                        Ok(Either::Right(Callee::make_static(function_id)))
                    } else {
                        Ok(Either::Right(Callee::make_static(function_id)))
                    }
                } else {
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
                        self.scopes.find_variable(calling_scope, fn_call.name.name)
                    {
                        let function_variable = self.variables.get(variable_id);
                        match self.types.get(function_variable.type_id) {
                            Type::Closure(closure_type) => {
                                Ok(Either::Right(Callee::StaticClosure {
                                    function_id: closure_type.body_function_id,
                                    closure_type_id: function_variable.type_id,
                                }))
                            }
                            Type::ClosureObject(_closure_object) => {
                                Ok(Either::Right(Callee::DynamicClosure(Box::new(
                                    TypedExpr::Variable(VariableExpr {
                                        variable_id,
                                        type_id: function_variable.type_id,
                                        span: fn_call.name.span,
                                    }),
                                ))))
                            }
                            Type::Reference(function_reference) => {
                                if self
                                    .types
                                    .get(function_reference.inner_type)
                                    .as_function()
                                    .is_some()
                                {
                                    Ok(Either::Right(Callee::DynamicFunction(Box::new(
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
        if let Some(enclosing_closure) = self.scopes.nearest_parent_closure(scope_id) {
            let Some(expected_return_type) =
                self.scopes.get_closure_info(enclosing_closure).unwrap().expected_return_type
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
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let expected_return_type = self.get_expected_return_type(scope_id, span)?;
        let return_value = self.eval_expr(parsed_expr, scope_id, Some(expected_return_type))?;
        if let Err(msg) = self.check_types(expected_return_type, return_value.get_type(), scope_id)
        {
            return failf!(span, "Returned wrong type: {msg}");
        }
        Ok(TypedExpr::Return(TypedReturn { value: Box::new(return_value), span }))
    }

    fn handle_builtin_function_call_lookalikes(
        &mut self,
        fn_call: &FnCall,
        calling_scope: ScopeId,
    ) -> TyperResult<Option<TypedExpr>> {
        let call_span = fn_call.span;
        match self.name_of(fn_call.name.name) {
            "return" => {
                if fn_call.args.len() != 1 {
                    return make_fail_span(
                        "return(...) must have exactly one argument",
                        fn_call.span,
                    );
                }
                Ok(Some(self.eval_return(fn_call.args[0].value, calling_scope, call_span)?))
            }
            "break" => {
                if fn_call.args.len() > 1 {
                    return make_fail_span("break(...) must have 0 or 1 argument", call_span);
                }
                // Determine based on loop type if break with value is allowed
                let Some((enclosing_loop_scope_id, loop_type)) =
                    self.scopes.nearest_parent_loop(calling_scope)
                else {
                    return failf!(call_span, "break(...) outside of while loop");
                };
                let expected_break_type: Option<TypeId> =
                    self.scopes.get_loop_info(enclosing_loop_scope_id).unwrap().break_type;

                let break_value = match fn_call.args.first() {
                    None => TypedExpr::Unit(call_span),
                    Some(fn_call_arg) => {
                        // ALTERNATIVE: Allow break with value from `while` loops but require the type to implement the `Default` trait
                        match loop_type {
                            LoopType::Loop => self.eval_expr(
                                fn_call_arg.value,
                                calling_scope,
                                expected_break_type,
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
                let actual_break_type = break_value.get_type();

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

                Ok(Some(TypedExpr::Break(TypedBreak {
                    value: Box::new(break_value),
                    loop_scope: enclosing_loop_scope_id,
                    loop_type,
                    span: call_span,
                })))
            }
            "continue" => {
                todo!("implement continue")
            }
            "testCompile" => {
                if fn_call.args.len() != 1 {
                    return make_fail_span("testCompile takes one argument", call_span);
                }
                let arg = &fn_call.args[0];
                let result = self.eval_expr(arg.value, calling_scope, None);
                let expr = match result {
                    Err(typer_error) => {
                        self.synth_optional_some(TypedExpr::Str(typer_error.message, call_span))
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
        known_args: Option<&(Vec<TypeId>, Vec<TypedExpr>)>,
        calling_scope: ScopeId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<Either<TypedExpr, Callee>> {
        debug_assert!(fn_call.name.namespaces.is_empty());
        let fn_name = fn_call.name.name;
        let second_arg = fn_call.args.get(1).cloned();

        if let Some(base_arg) = fn_call.args.first() {
            let type_args = fn_call.type_args.clone();
            if let Some(enum_constr) = self.handle_enum_constructor(
                base_arg.value,
                fn_name,
                second_arg.map(|param| param.value),
                expected_type_id,
                &type_args,
                calling_scope,
                fn_call.span,
            )? {
                return Ok(Either::Left(enum_constr));
            }
        }

        let base_expr = match base_expr {
            MaybeTypedExpr::Typed(expr) => expr,
            MaybeTypedExpr::Parsed(parsed_expr_id) => {
                self.eval_expr(parsed_expr_id, calling_scope, None)?
            }
        };

        // Handle the special case of the synthesized enum 'as{Variant}' methods
        if let Some(enum_as_result) = self.handle_enum_as(&base_expr, fn_call)? {
            return Ok(Either::Left(enum_as_result));
        }

        let call_span = fn_call.span;

        let type_id = base_expr.get_type();
        let base_for_method_derefed = self.types.get_type_id_dereferenced(type_id);
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
                self.type_id_to_string(type_id),
            )
        };

        let abilities_in_scope = self.scopes.find_abilities_in_scope(calling_scope);
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
            calling_scope,
            expected_type_id,
        )?;
        Ok(Either::Right(Callee::make_static(ability_impl_fn_id)))
    }

    fn resolve_ability_call(
        &mut self,
        function_id: FunctionId,
        function_ability_index: usize,
        ability_id: AbilityId,
        fn_call: &FnCall,
        known_args: Option<&(Vec<TypeId>, Vec<TypedExpr>)>,
        calling_scope: ScopeId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<FunctionId> {
        let call_span = fn_call.span;
        let ability_fn_signature = self.get_function_type(function_id);
        let ability_fn_return_type = ability_fn_signature.return_type;
        let ability_fn_params = ability_fn_signature.params.clone();
        let ability_params = self.get_ability(ability_id).parameters.clone();
        let ability_self_type_id = self.get_ability(ability_id).self_type_id;
        //
        // 1) Solve for 'self'
        let passed_len = known_args.map(|ka| ka.1.len()).unwrap_or(fn_call.args.len());
        if passed_len != ability_fn_signature.params.len() {
            return failf!(call_span, "Mismatching arg count when trying to resolve ability call to {} (this probably doesn't handle context params properly)", self.name_of(fn_call.name.name));
        }
        // Future TODO: Make sure we handle context params correctly,
        // skipping them if not passed explicitly, and utilizing them if passed explicitly
        let mut solution_set = TypeSolutionSet {
            solutions: ({
                let mut v = Vec::with_capacity(ability_params.len() + 1);
                v.push(PendingTypeSolution::unsolved(
                    get_ident!(self, "Self"),
                    ability_self_type_id,
                ));
                v.extend(
                    ability_params
                        .iter()
                        .filter(|p| !p.is_impl_param)
                        .map(|p| PendingTypeSolution::unsolved(p.name, p.type_variable_id)),
                );
                v
            }),
        };
        let mut failed_exprs: Vec<TyperError> = Vec::new();
        let passed_args: &mut dyn Iterator<Item = MaybeTypedExpr> = match known_args {
            Some(ka) => &mut ka.1.clone().into_iter().map(MaybeTypedExpr::Typed),
            None => &mut fn_call.args.iter().map(|arg| MaybeTypedExpr::Parsed(arg.value)),
        };
        for (arg, param) in passed_args.zip(ability_fn_params.iter()) {
            let expr = match arg {
                MaybeTypedExpr::Typed(expr) => Ok(expr),
                MaybeTypedExpr::Parsed(parsed_expr) => {
                    self.eval_expr(parsed_expr, calling_scope, Some(param.type_id))
                }
            };
            match expr {
                Err(error) => failed_exprs.push(error),
                Ok(expr) => {
                    if let TypeSolutionResult::NonMatching(msg) = self.solve_generic_params(
                        &mut solution_set,
                        expr.get_type(),
                        param.type_id,
                        expr.get_span(),
                    )? {
                        return failf!(
                            expr.get_span(),
                            "Invalid argument {msg}: expected {} but got {}",
                            self.type_id_to_string(param.type_id),
                            self.type_id_to_string(expr.get_type())
                        );
                    };
                }
            }
            // We break early since there's no point continuing to evaluate code
            // once we've solved all params
            if solution_set.all_solved() {
                break;
            }
        }
        if !solution_set.all_solved() {
            if let Some(expected_type) = expected_type_id {
                self.solve_generic_params(
                    &mut solution_set,
                    expected_type,
                    ability_fn_return_type,
                    call_span,
                )?;
            }
        };
        let Some(solved_params) = solution_set.get_solutions() else {
            if !failed_exprs.is_empty() {
                return Err(failed_exprs.into_iter().next().unwrap());
            } else {
                use std::fmt::Write;
                let mut detail = String::new();
                writeln!(&mut detail, "Failed to resolve ability call:").unwrap();
                self.display_solution_set(&mut detail, &solution_set);
                return failf!(call_span, "{}", detail);
            }
        };
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
        self.check_ability_arguments(ability_id, solved_rest, call_span, calling_scope, skip_impl_check).map_err(|e| {
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
        base_expr: &TypedExpr,
        fn_call: &FnCall,
    ) -> TyperResult<Option<TypedExpr>> {
        let base_type_id: TypeId = base_expr.get_type();
        let (e, is_reference) = match self.types.get(base_type_id) {
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
                return failf!(
                    span,
                    "Method '{}' does not exist on type {}",
                    fn_name,
                    self.type_id_to_string(base_expr.get_type())
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
            let base_expr_dereferenced = if is_reference {
                self.synth_dereference(base_expr.clone())
            } else {
                base_expr.clone()
            };
            let condition = TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                target_expr: Box::new(base_expr_dereferenced.clone()),
                variant_name,
                variant_index,
                span,
            });
            let consequent = self.synth_optional_some(TypedExpr::Cast(TypedCast {
                cast_type: CastType::KnownNoOp,
                base_expr: Box::new(base_expr.clone()),
                target_type_id: resulting_type_id,
                span,
            }));
            let alternate = self.synth_optional_none(resulting_type_id, span);

            Ok(Some(TypedExpr::If(Box::new(TypedIf {
                ty: consequent.get_type(),
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
        expected_type: Option<TypeId>,
        type_args: &[NamedTypeArg],
        scope: ScopeId,
        span: SpanId,
    ) -> TyperResult<Option<TypedExpr>> {
        let ParsedExpression::Variable(v) = self.ast.expressions.get(base_expr) else {
            return Ok(None);
        };
        let Some((base_type_in_scope, _)) = self.scopes.find_type_namespaced(
            scope,
            &v.name,
            &self.namespaces,
            &self.ast.identifiers,
        )?
        else {
            return Ok(None);
        };
        match self.types.get(base_type_in_scope) {
            Type::Enum(e) => {
                // We only do this _if_ the name matches a variant of the enum
                // Otherwise we are happy to fall through to a regular method call
                if let Some(_variant) = e.variant_by_name(variant_name) {
                    Ok(Some(self.eval_enum_constructor(
                        base_type_in_scope,
                        variant_name,
                        payload_parsed_expr,
                        expected_type,
                        scope,
                        span,
                    )?))
                } else {
                    Ok(None)
                }
            }
            Type::Generic(g) => {
                let Some(inner_enum) = self.types.get(g.inner).as_enum() else { return Ok(None) };
                let Some(generic_variant) = inner_enum.variant_by_name(variant_name) else {
                    return Ok(None);
                };
                let g_params = g.params.clone();
                let g_name = g.type_defn_info.name;
                let generic_payload = match generic_variant.payload {
                    Some(generic_variant_payload) => match payload_parsed_expr {
                        None => {
                            return failf!(
                                span,
                                "Variant {} requires a payload",
                                self.name_of(generic_variant.name)
                            )
                        }
                        Some(payload_parsed_expr) => {
                            let payload_expr = self.eval_expr(
                                payload_parsed_expr,
                                scope,
                                Some(generic_variant_payload),
                            )?;
                            Some((generic_variant_payload, payload_expr))
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

                let solved_or_passed_type_params: Vec<SimpleNamedType> = if type_args.is_empty() {
                    match generic_payload {
                        None => {
                            match expected_type
                                .map(|t| (t, self.types.get_generic_instance_info(t)))
                            {
                                Some((expected_type, Some(spec_info))) => {
                                    if spec_info.generic_parent == base_type_in_scope {
                                        // Solved params
                                        g_params
                                            .iter()
                                            .zip(spec_info.param_values.iter())
                                            .map(|(g_param, type_id)| SimpleNamedType {
                                                name: g_param.name,
                                                type_id: *type_id,
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
                            let passed_expr_type = payload.get_type();

                            let mut solution_set = TypeSolutionSet {
                                solutions: g_params.iter().map(PendingTypeSolution::from).collect(),
                            };
                            if let TypeSolutionResult::NonMatching(msg) = self
                                .solve_generic_params(
                                    &mut solution_set,
                                    passed_expr_type,
                                    generic_variant_payload,
                                    span,
                                )?
                            {
                                return failf!(
                                    payload.get_span(),
                                    "Invalid enum payload: {msg}: expected {} but got {}",
                                    self.type_id_to_string(generic_variant_payload),
                                    self.type_id_to_string(passed_expr_type)
                                );
                            };
                            let Some(solutions) = solution_set.get_solutions() else {
                                return failf!(span, "Could not infer type for generic enum");
                            };
                            solutions
                        }
                    }
                } else {
                    let mut passed_params = Vec::with_capacity(g_params.len());
                    for (generic_param, passed_type_expr) in
                        g_params.clone().iter().zip(type_args.iter())
                    {
                        let type_id = self.eval_type_expr(passed_type_expr.type_expr, scope)?;
                        passed_params.push(SimpleNamedType { name: generic_param.name, type_id });
                    }
                    passed_params
                };

                let concrete_type = self.instantiate_generic_type(
                    base_type_in_scope,
                    solved_or_passed_type_params
                        .iter()
                        .map(|type_param| type_param.type_id)
                        .collect(),
                );
                let enum_constr = self.eval_enum_constructor(
                    concrete_type,
                    variant_name,
                    payload_parsed_expr,
                    expected_type,
                    scope,
                    span,
                )?;
                Ok(Some(enum_constr))
            }
            _ => Ok(None),
        }
    }

    fn align_call_arguments_with_parameters<'params>(
        &mut self,
        fn_call: &FnCall,
        params: &'params [FnParamType],
        pre_evaled_params: Option<Vec<TypedExpr>>,
        calling_scope: ScopeId,
        tolerate_missing_context_args: bool,
    ) -> TyperResult<Vec<(MaybeTypedExpr, &'params FnParamType)>> {
        let fn_name = fn_call.name.name;
        let span = fn_call.span;
        let explicit_context_args = !fn_call.explicit_context_args.is_empty();
        let mut final_args: Vec<(MaybeTypedExpr, &FnParamType)> = Vec::new();
        if !explicit_context_args {
            for context_param in params.iter().filter(|p| p.is_context) {
                if let Some(found_id) =
                    self.scopes.find_context_variable_by_type(calling_scope, context_param.type_id)
                {
                    let found = self.variables.get(found_id);
                    final_args.push((
                        MaybeTypedExpr::Typed(TypedExpr::Variable(VariableExpr {
                            variable_id: found_id,
                            type_id: found.type_id,
                            span,
                        })),
                        context_param,
                    ));
                } else {
                    let is_source_loc = self
                        .types
                        .get_type_defn_info(context_param.type_id)
                        .is_some_and(|defn_info| {
                            defn_info.name == get_ident!(self, "CompilerSourceLoc")
                                && defn_info.scope == self.scopes.get_root_scope_id()
                        });
                    if is_source_loc {
                        let expr = self.synth_source_location(span);
                        final_args.push((MaybeTypedExpr::Typed(expr), context_param));
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

        let is_closure =
            params.first().is_some_and(|p| p.name == get_ident!(self, CLOSURE_ENV_PARAM_NAME));
        let params = if is_closure { &params[1..] } else { params };
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
            for (expr, param) in pre_evaled_params.into_iter().zip(expected_literal_params) {
                final_args.push((MaybeTypedExpr::Typed(expr), param))
            }
        } else {
            for (param_index, fn_param) in expected_literal_params.enumerate() {
                let matching_arg_by_name =
                    actual_passed_args.clone().find(|arg| arg.name == Some(fn_param.name));
                let matching_arg =
                    matching_arg_by_name.or(actual_passed_args.clone().nth(param_index));
                let Some(param) = matching_arg else {
                    return failf!(
                        span,
                        "Missing argument to {}: {}",
                        self.ast.identifiers.get_name(fn_name).blue(),
                        self.name_of(fn_param.name).red()
                    );
                };
                final_args.push((MaybeTypedExpr::Parsed(param.value), fn_param));
            }
        }
        Ok(final_args)
    }

    fn check_call_arguments(
        &mut self,
        call_name: Identifier,
        aligned_args: Vec<(TypedExpr, &FnParamType)>,
        calling_scope: ScopeId,
    ) -> TyperResult<Vec<TypedExpr>> {
        let mut successful_args = Vec::with_capacity(aligned_args.len());
        for (expr, param) in aligned_args.into_iter() {
            if let Err(e) = self.check_types(param.type_id, expr.get_type(), calling_scope) {
                return failf!(
                    expr.get_span(),
                    "Invalid type passed for parameter {} to function {}: {}",
                    self.name_of(param.name),
                    self.name_of(call_name),
                    e
                );
            };
            successful_args.push(expr);
        }
        Ok(successful_args)
    }

    pub fn get_callee_function_type(&self, callee: &Callee) -> TypeId {
        match callee {
            Callee::StaticFunction(function_id) | Callee::StaticClosure { function_id, .. } => {
                self.get_function(*function_id).type_id
            }
            Callee::DynamicFunction(function_reference_expr) => {
                let function_reference_type = self
                    .types
                    .get(function_reference_expr.get_type())
                    .expect_reference()
                    .inner_type;
                function_reference_type
            }
            Callee::DynamicClosure(dynamic) => match self.types.get(dynamic.get_type()) {
                Type::ClosureObject(closure_object) => closure_object.function_type,
                _ => {
                    panic!(
                        "Invalid dynamic function callee: {}",
                        self.type_id_to_string(dynamic.get_type())
                    )
                }
            },
        }
    }

    fn eval_function_call(
        &mut self,
        fn_call: &FnCall,
        scope_id: ScopeId,
        expected_type_id: Option<TypeId>,
        known_args: Option<(Vec<TypeId>, Vec<TypedExpr>)>,
    ) -> TyperResult<TypedExpr> {
        let span = fn_call.span;
        assert!(
            fn_call.args.is_empty() || known_args.is_none(),
            "cannot pass both typed value args and parsed value args to eval_function_call"
        );
        let callee = match self.resolve_parsed_function_call(
            fn_call,
            known_args.as_ref(),
            scope_id,
            expected_type_id,
        )? {
            Either::Left(expr) => return Ok(expr),
            Either::Right(callee) => callee,
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let original_function = callee.maybe_function_id().map(|f| self.get_function(f));
        let is_generic = original_function.is_some_and(|f| !f.type_params.is_empty());

        let (callee, typechecked_arguments, type_args, return_type) = match is_generic {
            false => {
                let original_function_type =
                    self.types.get(self.get_callee_function_type(&callee)).as_function().unwrap();
                let original_function_return_type = original_function_type.return_type;
                let params = &original_function_type.params.clone();
                let aligned_args = self.align_call_arguments_with_parameters(
                    fn_call,
                    params,
                    known_args.map(|ka| ka.1),
                    scope_id,
                    false,
                )?;
                // Easy mode: just evaluate them
                let mut typed_args = Vec::with_capacity(aligned_args.len());
                for (maybe_typed_expr, fn_arg_type) in aligned_args.into_iter() {
                    let expr = match maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, scope_id, Some(fn_arg_type.type_id))?
                        }
                    };
                    typed_args.push((expr, fn_arg_type))
                }
                (
                    callee,
                    self.check_call_arguments(fn_call.name.name, typed_args, scope_id)?,
                    Vec::new(),
                    original_function_return_type,
                )
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
                    None => self.infer_and_constrain_call_type_args(
                        fn_call,
                        function_id,
                        scope_id,
                        expected_type_id,
                    )?,
                };

                let function_id = self.specialize_function_signature(&type_args, function_id)?;

                let specialized_fn_type = &self.get_function_type(function_id);
                let specialized_params = specialized_fn_type.params.clone();
                let specialized_return_type = specialized_fn_type.return_type;
                let aligned_args = self.align_call_arguments_with_parameters(
                    fn_call,
                    &specialized_params,
                    known_args.map(|ka| ka.1),
                    scope_id,
                    false,
                )?;
                let mut typed_args = Vec::with_capacity(aligned_args.len());
                for (maybe_typed_expr, fn_arg_type) in aligned_args.into_iter() {
                    let expr = match maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, scope_id, Some(fn_arg_type.type_id))?
                        }
                    };
                    typed_args.push((expr, fn_arg_type))
                }
                let typechecked_args =
                    self.check_call_arguments(fn_call.name.name, typed_args, scope_id)?;

                (
                    Callee::make_static(function_id),
                    typechecked_args,
                    type_args,
                    specialized_return_type,
                )
            }
        };

        let call_return_type =
            if typechecked_arguments.iter().any(|arg| arg.get_type() == NEVER_TYPE_ID) {
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
                _ => Ok(TypedExpr::Call(call)),
            }
        } else {
            Ok(TypedExpr::Call(call))
        }
    }

    fn infer_and_constrain_call_type_args(
        &mut self,
        fn_call: &FnCall,
        generic_function_id: FunctionId,
        calling_scope: ScopeId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<Vec<SimpleNamedType>> {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_type = self.get_function_type(generic_function_id);
        let function_return_type = generic_function_type.return_type;
        let generic_type_params = generic_function.type_params.clone();
        debug_assert!(!generic_type_params.is_empty());
        let generic_params = generic_function_type.params.clone();
        let passed_type_args = &fn_call.type_args;
        let type_params = match passed_type_args.is_empty() {
            false => {
                if passed_type_args.len() != generic_type_params.len() {
                    return make_fail_span(
                        format!(
                            "Expected {} type arguments but got {}",
                            generic_type_params.len(),
                            passed_type_args.len()
                        ),
                        fn_call.span,
                    );
                }
                let mut evaled_params = Vec::with_capacity(passed_type_args.len());
                for (idx, type_arg) in passed_type_args.iter().enumerate() {
                    let param = &generic_type_params[idx];
                    let type_id = self.eval_type_expr(type_arg.type_expr, calling_scope)?;
                    evaled_params.push(SimpleNamedType { name: param.name, type_id });
                }
                evaled_params
            }
            true => {
                let mut solution_set = TypeSolutionSet::from(generic_function.type_params.iter());

                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    &generic_params,
                    None,
                    calling_scope,
                    true,
                )?;

                let mut failed_exprs = Vec::new();
                for (expr, gen_param) in args_and_params.into_iter() {
                    if solution_set.all_solved() {
                        break;
                    }
                    // We don't care if this fails; sometimes we can't
                    // evaluate an expression before inferring types
                    // That's ok because
                    // 1) We hope that other arguments will have more luck
                    // 2) We will evaluate these expressions again when we actually
                    //    do typechecking, if we manage to solve the generics
                    let expr = match expr {
                        MaybeTypedExpr::Typed(typed) => Ok(typed),
                        MaybeTypedExpr::Parsed(parsed_expr) => {
                            self.eval_expr(parsed_expr, calling_scope, Some(gen_param.type_id))
                        }
                    };
                    match expr {
                        Ok(expr) => {
                            debug!(
                                "solving {}: {} w/ param {}",
                                self.name_of(gen_param.name),
                                self.type_id_to_string(gen_param.type_id),
                                self.expr_to_string(&expr)
                            );
                            if let TypeSolutionResult::NonMatching(msg) = self
                                .solve_generic_params(
                                    &mut solution_set,
                                    expr.get_type(),
                                    gen_param.type_id,
                                    fn_call.span,
                                )?
                            {
                                return failf!(
                                    expr.get_span(),
                                    "Invalid argument: {msg}: expected {} but got {}",
                                    self.type_id_to_string(gen_param.type_id),
                                    self.type_id_to_string(expr.get_type())
                                );
                            };
                        }
                        Err(e) => {
                            eprintln!("Just fyi eval_expr failed during inference: {e}");
                            failed_exprs.push(e);
                            // return failf!(
                            //     expr.unwrap_err().span,
                            //     "Just fyi eval_expr failed during inference: {expr:?}"
                            // );
                        }
                    }
                }

                if !solution_set.all_solved() {
                    if let Some(call_expected_type) = expected_type_id {
                        debug!(
                            "Using expected type {} to try to infer call to {}",
                            self.type_id_to_string(call_expected_type),
                            self.name_of(fn_call.name.name)
                        );
                        self.solve_generic_params(
                            &mut solution_set,
                            call_expected_type,
                            function_return_type,
                            fn_call.span,
                        )?;
                    }
                }

                match solution_set.get_solutions() {
                    None => {
                        if !failed_exprs.is_empty() {
                            return Err(failed_exprs.into_iter().next().unwrap());
                        }
                        let mut detail: String = String::new();
                        self.display_solution_set(&mut detail, &solution_set);
                        return failf!(
                            fn_call.span,
                            "Not enough information to resolved this call\n:{}",
                            detail
                        );
                    }
                    Some(solutions) => solutions,
                }
            }
        };

        // Enforce ability constraints, or other constraints in the future?
        for (type_param, type_arg) in generic_type_params.iter().zip(type_params.iter()) {
            self.check_type_constraints(
                type_param.name,
                type_param.type_id,
                type_arg.type_id,
                calling_scope,
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
        Ok(type_params)
    }

    fn solve_generic_params(
        &self,
        solved_params: &mut TypeSolutionSet,
        passed_type: TypeId,
        slot_type: TypeId,
        span: SpanId,
    ) -> TyperResult<TypeSolutionResult> {
        // passed_expr              slot_type -> result
        //
        // int                    T                 -> T := int
        // List[int]             List[T]          -> T := int
        // Pair[int, string]      Pair[T, U]        -> T := int, U := string
        // fn(int) -> int         Fn(T) -> T        -> T := int
        // fn() -> List[string]  Fn() -> List[T]  -> T := string
        //
        // ???
        // List[int]             F[int]          -> F := List
        debug!(
            "solve_generic_params passed {} in slot {}",
            self.type_id_to_string(passed_type).blue(),
            self.type_id_to_string(slot_type).blue()
        );

        if let (Some(passed_info), Some(arg_info)) = (
            self.types.get_generic_instance_info(passed_type),
            self.types.get_generic_instance_info(slot_type),
        ) {
            // expr: NewList[int] arg: NewList[T]
            if passed_info.generic_parent == arg_info.generic_parent {
                debug!(
                    "comparing generic instances of {}",
                    self.type_id_to_string(arg_info.generic_parent)
                );
                // We can directly 'solve' every appearance of a type param here
                for (passed_type, arg_slot) in
                    passed_info.param_values.iter().zip(arg_info.param_values.iter())
                {
                    self.solve_generic_params(solved_params, *passed_type, *arg_slot, span)?;
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return Ok(TypeSolutionResult::Matching);
        }

        match (self.types.get(passed_type), self.types.get(slot_type)) {
            (_, Type::TypeVariable(_tv)) => {
                // If the type param is used in the type of the argument, we can infer
                // the type param from the type of the argument
                match solved_params.get_solution_mut(slot_type) {
                    None => {}
                    Some(solution) => match solution.solved_type_id {
                        Some(existing_solution) => {
                            if existing_solution != passed_type {
                                return failf!(
                            span,
                            "Conflicting type parameters for type param {} in call: {} and {}",
                            self.name_of(solution.name),
                            self.type_id_to_string(existing_solution),
                            self.type_id_to_string(passed_type)
                        );
                            } else {
                                debug!(
                            "We double-solved type param {} but that's ok because it matched",
                            self.name_of(solution.name)
                        )
                            }
                        }
                        None => {
                            debug!(
                                "\tsolve_generic_params solved {} := {}",
                                self.name_of(solution.name),
                                self.type_id_to_string(passed_type)
                            );
                            solution.solved_type_id = Some(passed_type)
                        }
                    },
                }
                Ok(TypeSolutionResult::Matching)
            }
            (Type::Reference(passed_refer), Type::Reference(refer)) => self.solve_generic_params(
                solved_params,
                passed_refer.inner_type,
                refer.inner_type,
                span,
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
                    return Ok(TypeSolutionResult::NonMatching("field count"));
                }
                for (idx, field) in fields.iter().enumerate() {
                    let passed_field = &passed_fields[idx];
                    if field.name != passed_field.name {
                        return Ok(TypeSolutionResult::NonMatching("field names"));
                    }
                    self.solve_generic_params(
                        solved_params,
                        passed_field.type_id,
                        field.type_id,
                        span,
                    )?;
                }
                Ok(TypeSolutionResult::Matching)
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
                    return Ok(TypeSolutionResult::NonMatching("variant count"));
                }
                for (idx, variant) in variants.iter().enumerate() {
                    let passed_variant = &passed_variants[idx];
                    if variant.name != passed_variant.name {
                        return Ok(TypeSolutionResult::NonMatching("variant names"));
                    }
                    if let Some(passed_payload) = passed_variant.payload {
                        if let Some(param_payload) = variant.payload {
                            self.solve_generic_params(
                                solved_params,
                                passed_payload,
                                param_payload,
                                span,
                            )?;
                        } else {
                            return Ok(TypeSolutionResult::NonMatching("payloads"));
                        }
                    }
                }

                Ok(TypeSolutionResult::Matching)
            }
            (Type::EnumVariant(passed_enum_variant), Type::Enum(_param_enum_type_variant)) => self
                .solve_generic_params(
                    solved_params,
                    passed_enum_variant.enum_type_id,
                    slot_type,
                    span,
                ),
            (Type::Function(passed_fn), Type::Function(param_fn)) => {
                if passed_fn.params.len() == param_fn.params.len() {
                    for (passed_param, param_param) in
                        passed_fn.params.iter().zip(param_fn.params.iter())
                    {
                        if passed_param.is_closure_env && param_param.is_closure_env {
                            continue;
                        }
                        self.solve_generic_params(
                            solved_params,
                            passed_param.type_id,
                            param_param.type_id,
                            span,
                        )?;
                    }
                    self.solve_generic_params(
                        solved_params,
                        passed_fn.return_type,
                        param_fn.return_type,
                        span,
                    )
                } else {
                    Ok(TypeSolutionResult::NonMatching("parameter count"))
                }
            }
            (Type::Closure(passed_closure), Type::ClosureObject(param_closure)) => self
                .solve_generic_params(
                    solved_params,
                    passed_closure.function_type,
                    param_closure.function_type,
                    span,
                ),
            (Type::ClosureObject(passed_closure), Type::ClosureObject(param_closure)) => self
                .solve_generic_params(
                    solved_params,
                    passed_closure.function_type,
                    param_closure.function_type,
                    span,
                ),
            (_, _) if passed_type == slot_type => Ok(TypeSolutionResult::Matching),
            _ => Ok(TypeSolutionResult::NonMatching("Unrelated types")),
        }
    }

    fn display_solution_set(&self, w: &mut impl std::fmt::Write, solution_set: &TypeSolutionSet) {
        let mut first = true;
        for solution in &solution_set.solutions {
            write!(
                w,
                "{} := {}",
                self.name_of(solution.name),
                solution
                    .solved_type_id
                    .map(|t| self.type_id_to_string(t))
                    .unwrap_or("?".to_string())
            )
            .unwrap();
            if first {
                first = false;
            } else {
                w.write_char('\n').unwrap();
            };
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
            self.substitute_in_type(None, generic_function_type_id, None, &pairs);
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
                    is_global: false,
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
            type_params: vec![],
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
            specialized_function.scope,
            Some(specialized_return_type),
            true,
        )?;

        self.get_function_mut(function_id).body_block = Some(block);
        Ok(())
    }

    pub fn is_function_concrete(&self, function: &TypedFunction) -> bool {
        match function.intrinsic_type {
            Some(intrinsic) if intrinsic.is_inlined() => false,
            _ => match &function.kind {
                TypedFunctionKind::AbilityDefn(_) => false,
                _other => {
                    let b = match &function.specialization_info {
                        None => function.type_params.is_empty(),
                        Some(spec_info) => !spec_info
                            .type_arguments
                            .iter()
                            .any(|nt| self.does_type_reference_type_variables(nt.type_id)),
                    };
                    if function.compiler_debug {
                        eprintln!(
                            "is_function_concrete={b} for {}",
                            self.function_to_string(function, false)
                        );
                    }
                    b
                }
            },
        }
    }

    fn eval_stmt(
        &mut self,
        stmt: &ParsedStmt,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<Option<TypedStmt>> {
        match stmt {
            ParsedStmt::Use(use_stmt) => {
                let parsed_use = self.ast.uses.get_use(use_stmt.use_id);
                // These uses should always hit since we only do 1 pass inside function bodies, and
                // at that point all symbols are resolvable
                let Some(useable_symbol) = self.scopes.find_useable_symbol(
                    scope_id,
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
                    scope_id,
                    useable_symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                Ok(None)
            }
            ParsedStmt::ValDef(val_def) => {
                let provided_type = match val_def.type_expr.as_ref() {
                    None => None,
                    Some(&type_expr) => Some(self.eval_type_expr(type_expr, scope_id)?),
                };
                let expected_type = match provided_type {
                    Some(provided_type) => {
                        if val_def.is_referencing {
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
                let value_expr = self.eval_expr(val_def.value, scope_id, expected_type)?;
                let actual_type = value_expr.get_type();

                if let Some(expected_type) = expected_type {
                    if let Err(msg) = self.check_types(expected_type, actual_type, scope_id) {
                        return make_fail_span(
                            format!("Local variable type mismatch: {}", msg),
                            val_def.span,
                        );
                    }
                };

                let variable_type = if val_def.is_referencing {
                    self.types.add_reference_type(actual_type)
                } else {
                    actual_type
                };

                let variable_id = self.variables.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name,
                    type_id: variable_type,
                    owner_scope: scope_id,
                    is_context: val_def.is_context,
                    is_global: false,
                });
                let val_def_stmt = TypedStmt::Let(Box::new(LetStmt {
                    variable_type,
                    variable_id,
                    initializer: value_expr,
                    is_referencing: val_def.is_referencing,
                    span: val_def.span,
                }));
                if val_def.is_context {
                    self.scopes.add_context_variable(
                        scope_id,
                        val_def.name,
                        variable_id,
                        variable_type,
                    );
                } else {
                    self.scopes.add_variable(scope_id, val_def.name, variable_id);
                }
                Ok(Some(val_def_stmt))
            }
            ParsedStmt::Assignment(assignment) => {
                let ParsedExpression::Variable(_) = self.ast.expressions.get(assignment.lhs) else {
                    return failf!(
                        self.ast.expressions.get_span(assignment.lhs),
                        "Value assignment destination must be a variable"
                    );
                };
                let lhs = self.eval_variable(assignment.lhs, scope_id, true)?;
                let rhs = self.eval_expr(assignment.rhs, scope_id, Some(lhs.get_type()))?;
                if let Err(msg) = self.check_types(lhs.get_type(), rhs.get_type(), scope_id) {
                    return failf!(assignment.span, "Invalid type for assignment: {}", msg,);
                }
                Ok(Some(TypedStmt::Assignment(Box::new(Assignment {
                    destination: Box::new(lhs),
                    value: Box::new(rhs),
                    span: assignment.span,
                    kind: AssignmentKind::Value,
                }))))
            }
            ParsedStmt::SetRef(set_stmt) => {
                let lhs = self.eval_expr(set_stmt.lhs, scope_id, None)?;
                let Some(lhs_type) = self.types.get(lhs.get_type()).as_reference() else {
                    return failf!(
                        self.ast.expressions.get_span(set_stmt.lhs),
                        "Expected a reference type; got {}",
                        self.type_id_to_string(lhs.get_type())
                    );
                };
                let expected_rhs = lhs_type.inner_type;
                let rhs = self.eval_expr(set_stmt.rhs, scope_id, Some(expected_rhs))?;
                if let Err(msg) = self.check_types(expected_rhs, rhs.get_type(), scope_id) {
                    return failf!(set_stmt.span, "Invalid type for assignment: {}", msg,);
                }
                Ok(Some(TypedStmt::Assignment(Box::new(Assignment {
                    destination: Box::new(lhs),
                    value: Box::new(rhs),
                    span: set_stmt.span,
                    kind: AssignmentKind::Reference,
                }))))
            }
            ParsedStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(*expression, scope_id, expected_type)?;
                Ok(Some(TypedStmt::Expr(Box::new(expr))))
            }
        }
    }
    fn eval_block(
        &mut self,
        block: &Block,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
        needs_terminator: bool,
    ) -> TyperResult<TypedBlock> {
        let mut statements = Vec::with_capacity(block.stmts.len());
        let mut last_expr_type: TypeId = UNIT_TYPE_ID;
        let mut last_stmt_is_divergent = false;
        for (index, stmt) in block.stmts.iter().enumerate() {
            if last_stmt_is_divergent {
                return failf!(
                    self.ast.get_stmt_span(stmt),
                    "Dead code following divergent statement",
                );
            }
            let is_last = index == block.stmts.len() - 1;
            let expected_type = if is_last { expected_type } else { None };

            let Some(stmt) = self.eval_stmt(stmt, scope_id, expected_type)? else {
                continue;
            };
            last_stmt_is_divergent = stmt.is_divergent();
            last_expr_type = stmt.get_type();

            if is_last && needs_terminator {
                if last_stmt_is_divergent {
                    // No action needed; terminator exists
                    statements.push(stmt);
                } else {
                    match stmt {
                        TypedStmt::Expr(expr) => {
                            // Return this expr
                            let return_stmt =
                                TypedStmt::Expr(Box::new(TypedExpr::Return(TypedReturn {
                                    span: expr.get_span(),
                                    value: expr,
                                })));
                            statements.push(return_stmt);
                        }
                        TypedStmt::Assignment(_) | TypedStmt::Let(_) => {
                            let return_unit =
                                TypedStmt::Expr(Box::new(TypedExpr::Return(TypedReturn {
                                    span: stmt.get_span(),
                                    value: Box::new(TypedExpr::Unit(stmt.get_span())),
                                })));
                            statements.push(stmt);
                            statements.push(return_unit);
                        }
                    };
                }
            } else {
                statements.push(stmt);
            }
        }

        let typed_block =
            TypedBlock { expr_type: last_expr_type, scope_id, statements, span: block.span };
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
        expected_type: Option<TypeId>,
        scope: ScopeId,
        span: SpanId,
    ) -> TyperResult<TypedExpr> {
        let e = self.types.get(enum_type).expect_enum();
        let variant = e.variant_by_name(variant_name).ok_or(errf!(
            span,
            "No variant '{}' exists in enum '{}'",
            self.name_of(variant_name).blue(),
            self.type_id_to_string(enum_type)
        ))?;
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
                    let payload_value = self.eval_expr(payload_arg, scope, Some(payload_type))?;
                    if let Err(msg) =
                        self.check_types(payload_type, payload_value.get_type(), scope)
                    {
                        return failf!(span, "Variant payload type mismatch: {}", msg);
                    }
                    Ok(Some(Box::new(payload_value)))
                } else {
                    failf!(
                        span,
                        "Variant '{}' requires a payload",
                        self.name_of(variant_name).blue()
                    )
                }
            }
        }?;
        let never_payload = payload.as_ref().is_some_and(|p| p.get_type() == NEVER_TYPE_ID);
        let output_type = if never_payload {
            NEVER_TYPE_ID
        } else {
            match expected_type.map(|t| self.types.get(t)) {
                Some(Type::EnumVariant(ev)) if ev.my_type_id == variant_type_id => variant_type_id,
                _ => enum_type,
            }
        };
        Ok(TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: output_type,
            variant_name,
            variant_index,
            payload,
            span,
        }))
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
    ) -> TyperResult<(Vec<SimpleNamedType>, Vec<SimpleNamedType>)> {
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
        let mut impl_arguments = Vec::with_capacity(arguments.len());
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
            TypeVariable { name: self_ident, scope_id: specialized_ability_scope, span },
            vec![],
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
    ) -> TyperResult<(AbilityId, Vec<SimpleNamedType>, Vec<SimpleNamedType>)> {
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
        let parsed_function = self.ast.get_function(parsed_function_id).clone();
        let debug_directive = parsed_function
            .directives
            .iter()
            .find(|p| matches!(p.kind, DirectiveKind::CompilerDebug));
        let is_debug = debug_directive.is_some();
        if is_debug {
            self.push_debug_level();
        }
        let parsed_function_linkage = parsed_function.linkage;
        let parsed_function_ret_type = parsed_function.ret_type;
        let parsed_function_name = parsed_function.name;
        let parsed_function_span = parsed_function.span;
        let parsed_function_params = parsed_function.params.clone();
        let parsed_function_context_params = parsed_function.context_params.clone();
        let parsed_function_type_params = parsed_function.type_params.clone();

        let is_ability_decl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_none());
        let is_ability_impl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_some());
        let ability_id = ability_info.as_ref().map(|info| info.ability_id);
        let impl_info = ability_info.as_ref().and_then(|info| info.impl_info.as_ref());
        let ability_kind = ability_id.map(|id| &self.get_ability(id).kind);
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
            Some(target_type) => self.ast.identifiers.intern(format!(
                "{}_{}_{}",
                self.name_of(self.get_ability(ability_id.unwrap()).name),
                self.type_id_to_string(target_type),
                self.ast.identifiers.get_name(parsed_function_name),
            )),
            None => parsed_function.name,
        };

        let fn_scope_id = self.scopes.add_child_scope(
            parent_scope_id,
            ScopeType::FunctionScope,
            None,
            Some(name),
        );

        // Instantiate type arguments.
        let mut type_params: Vec<TypeParam> = Vec::with_capacity(parsed_function_type_params.len());

        // Inject the 'Self' type parameter
        if is_ability_decl {
            let self_type_id = self.get_ability(ability_id.unwrap()).self_type_id;
            type_params.push(TypeParam {
                name: get_ident!(self, "Self"),
                type_id: self_type_id,
                span: parsed_function_span,
            })
        }
        for type_parameter in parsed_function_type_params.iter() {
            let mut ability_constraints = Vec::new();
            for parsed_constraint in type_parameter.constraints.iter() {
                let ability_sig = match parsed_constraint {
                    parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        self.eval_ability_expr(ability_expr, false, fn_scope_id)?
                    }
                };
                ability_constraints.push(ability_sig);
            }
            for param in &parsed_function.additional_where_constraints {
                if param.name == type_parameter.name {
                    let ability_id = match &param.constraint_expr {
                        parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            self.eval_ability_expr(ability_expr, false, fn_scope_id)?
                        }
                    };
                    ability_constraints.push(ability_id);
                }
            }
            let type_variable_id = self.add_type_variable(
                TypeVariable {
                    name: type_parameter.name,
                    scope_id: fn_scope_id,
                    span: type_parameter.span,
                },
                ability_constraints,
            );
            let fn_scope = self.scopes.get_scope_mut(fn_scope_id);
            let type_param = TypeParam {
                name: type_parameter.name,
                type_id: type_variable_id,
                span: type_parameter.span,
            };
            type_params.push(type_param);
            if !fn_scope.add_type(type_parameter.name, type_variable_id) {
                return make_fail_span("Duplicate type variable name: {}", type_parameter.span);
            }
        }

        // Process parameters
        let mut param_types: Vec<FnParamType> = Vec::with_capacity(parsed_function_params.len());
        let mut param_variables = Vec::with_capacity(parsed_function_params.len());
        for (idx, fn_param) in
            parsed_function_context_params.iter().chain(parsed_function_params.iter()).enumerate()
        {
            let type_id = self.eval_type_expr(fn_param.ty, fn_scope_id)?;

            // First arg Self shenanigans
            if idx == 0 {
                let name_is_self = self.ast.identifiers.get_name(fn_param.name) == "self";

                // If the first argument is named self, check if it's a method of the companion type
                let is_ability_fn = ability_id.is_some();
                if name_is_self && !is_ability_fn {
                    if let Some(companion_type_id) = companion_type_id {
                        if self.types.get_type_id_dereferenced(type_id) != companion_type_id {
                            match (
                                self.types.get(companion_type_id),
                                self.types.get_generic_instance_info(
                                    self.types.get_type_id_dereferenced(type_id),
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
                                        self.type_id_to_string(companion_type_id),
                                        self.type_id_to_string(type_id),
                                        companion_type_id,
                                        type_id
                                    );
                                }
                            }
                        }
                    } else {
                        return make_fail_span(
                            "Cannot use name 'self' unless defining a method",
                            fn_param.span,
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
                is_global: false,
            };

            let is_context = fn_param.modifiers.is_context();
            let variable_id = self.variables.add_variable(variable);
            param_types.push(FnParamType {
                name: fn_param.name,
                type_id,
                is_context,
                is_closure_env: false,
                span: fn_param.span,
            });
            param_variables.push(variable_id);
            if is_context {
                let inserted = self.scopes.add_context_variable(
                    fn_scope_id,
                    fn_param.name,
                    variable_id,
                    type_id,
                );
                if !inserted {
                    return failf!(
                        fn_param.span,
                        "Duplicate context parameters for type {}",
                        self.type_id_to_string(type_id)
                    );
                }
            } else {
                self.scopes.add_variable(fn_scope_id, fn_param.name, variable_id)
            }
        }

        let intrinsic_type = if parsed_function_linkage == Linkage::Intrinsic {
            let mut namespace_chain = self.namespaces.name_chain(namespace_id);
            let resolved = self
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
        let return_type = match parsed_function_ret_type {
            None => UNIT_TYPE_ID,
            Some(type_expr) => self.eval_type_expr(type_expr, fn_scope_id)?,
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

        let function_type_id = self.types.add_type(Type::Function(FunctionType {
            params: param_types,
            return_type,
            defn_info: Some(TypeDefnInfo {
                name,
                scope: parent_scope_id,
                companion_namespace: None,
                ast_id: parsed_function_id.into(),
            }),
        }));

        let function_id = self.next_function_id();

        let actual_function_id = self.add_function(TypedFunction {
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
            // debug!(
            //     "Adding function to scope since it should be the original resolvable copy: {}",
            //     self.name_of(name)
            // );
            if !self.scopes.add_function(parent_scope_id, parsed_function_name, function_id) {
                return failf!(
                    parsed_function_span,
                    "Function name {} is taken",
                    self.name_of(parsed_function_name)
                );
            }
        };

        // In this case, we re-evaluate the ast-node for the ability specialization, so we expect
        // to run it more than once, and don't want to fail
        if !skip_ast_mapping {
            let existed =
                self.function_ast_mappings.insert(parsed_function_id, function_id).is_some();
            debug_assert!(!existed);
        }

        self.scopes.set_scope_owner_id(fn_scope_id, ScopeOwnerId::Function(function_id));

        if is_debug {
            eprintln!("DEBUG\n{}", self.function_id_to_string(function_id, false));
            eprintln!("FUNCTION SCOPE\n{}", self.scope_id_to_string(fn_scope_id));
            self.pop_debug_level();
        }

        Ok(function_id)
    }

    fn get_root_namespace_id(&self) -> NamespaceId {
        NamespaceId(0)
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

        let ast_fn_def = self.ast.get_function(ast_id);
        let function_span = ast_fn_def.span;

        let is_abstract = is_intrinsic || is_extern || is_ability_defn;

        let body_block = match ast_fn_def.block.as_ref() {
            None if is_abstract => None,
            None => return make_fail_span("function is missing implementation", function_span),
            Some(_) if is_abstract => {
                return make_fail_span("unexpected function implementation", function_span)
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
                let block = self.eval_block(&block_ast, fn_scope_id, Some(return_type), true)?;
                debug!(
                    "evaled function block with expected type {} and got type {}",
                    self.type_id_to_string(return_type),
                    self.type_id_to_string(block.expr_type)
                );
                if let Err(msg) = self.check_types(return_type, block.expr_type, fn_scope_id) {
                    return failf!(
                        function_span,
                        "Function {} return type mismatch: {}",
                        self.name_of(function_name),
                        msg
                    );
                } else {
                    Some(block)
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
            TypeVariable {
                name: self_ident_id,
                scope_id: ability_scope_id,
                span: parsed_ability.span,
            },
            vec![],
        );
        let _ = self.scopes.get_scope_mut(ability_scope_id).add_type(self_ident_id, self_type_id);
        for ability_param in parsed_ability.params.clone().iter() {
            let ability_impls: TyperResult<Vec<TypedAbilitySignature>> = ability_param
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
                TypeVariable {
                    name: ability_param.name,
                    scope_id: ability_scope_id,
                    span: ability_param.span,
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
                TypeVariable {
                    name: generic_impl_param.name,
                    scope_id: impl_scope_id,
                    span: generic_impl_param.span,
                },
                // We create the variable with no constraints, then add them later, so that its
                // constraints can reference itself
                vec![],
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

        let mut impl_arguments: Vec<SimpleNamedType> = Vec::with_capacity(ability.parameters.len());
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
                return make_fail_span(
                    format!(
                        "Missing implementation for function '{}' in ability '{}'",
                        &*self.name_of(ability_function_ref.function_name).blue(),
                        &*self.name_of(ability_name).blue()
                    ),
                    span,
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
                self.get_root_namespace_id(),
            )?;

            let specialized = self.get_function(function_impl).type_id;

            let generic_type = self.get_function(ability_function_ref.function_id).type_id;

            // We check that the signature of the provided impl function matches
            // the signature of the generic function with target_type substituted for Self
            let substituted_root_type = self.substitute_in_type(
                None,
                generic_type,
                None,
                &[TypeSubstitutionPair { from: ability_self_type, to: impl_self_type }],
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
                let _variable_id: VariableId = self.eval_const(constant_id, scope_id)?;
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
        let root_ns_id = NamespaceId(0);
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

        let mut pass = 0;
        while !self.functions_pending_body_specialization.is_empty() {
            eprintln!(
                ">> Phase 6 specialize function bodies: {} (pass {pass})",
                self.functions_pending_body_specialization.len()
            );
            let clone = self.functions_pending_body_specialization.clone();
            self.functions_pending_body_specialization.clear();
            for function_id in &clone {
                let result = self.specialize_function_body(*function_id);
                if let Err(e) = result {
                    self.write_error(&mut err_writer, &e)?;
                    self.errors.push(e);
                }
            }
            pass += 1;
        }
        if !self.errors.is_empty() {
            bail!("{} failed specialize with {} errors", self.name(), self.errors.len())
        }

        Ok(())
    }

    pub fn get_span_for_type_id(&self, type_id: TypeId) -> Option<SpanId> {
        let t = self.types.get(type_id);
        match t {
            Type::TypeVariable(tv) => Some(tv.span),
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
            Type::TypeVariable(_) => vec![PatternConstructor::TypeVariable],
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

    fn synth_optional_some(&mut self, expression: TypedExpr) -> TypedExpr {
        let optional_type = self.synth_optional_type(expression.get_type());
        let span = expression.get_span();
        let some_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(get_ident!(self, "Some"))
            .unwrap();

        TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: some_variant.enum_type_id,
            variant_name: some_variant.name,
            variant_index: some_variant.index,
            span,
            payload: Some(Box::new(expression)),
        })
    }

    fn synth_optional_none(&mut self, type_id: TypeId, span: SpanId) -> TypedExpr {
        let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, vec![type_id]);
        let none_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(get_ident!(self, "None"))
            .unwrap();
        TypedExpr::EnumConstructor(TypedEnumConstructor {
            type_id: none_variant.enum_type_id,
            variant_name: none_variant.name,
            variant_index: none_variant.index,
            span,
            payload: None,
        })
    }

    fn synth_equals_binop(&self, lhs: TypedExpr, rhs: TypedExpr, span: SpanId) -> TypedExpr {
        TypedExpr::BinaryOp(BinaryOp {
            kind: BinaryOpKind::Equals,
            ty: BOOL_TYPE_ID,
            span,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn synth_dereference(&self, base: TypedExpr) -> TypedExpr {
        TypedExpr::UnaryOp(UnaryOp {
            kind: UnaryOpKind::Dereference,
            type_id: self.types.get(base.get_type()).expect_reference().inner_type,
            span: base.get_span(),
            expr: Box::new(base),
        })
    }

    fn synth_block(
        &mut self,
        statements: Vec<TypedStmt>,
        parent_scope: ScopeId,
        span: SpanId,
    ) -> TypedBlock {
        let expr_type = match statements.last() {
            Some(stmt) => stmt.get_type(),
            _ => UNIT_TYPE_ID,
        };
        let block_scope_id =
            self.scopes.add_child_scope(parent_scope, ScopeType::LexicalBlock, None, None);
        let block = TypedBlock {
            expr_type,
            statements: statements.to_vec(),
            scope_id: block_scope_id,
            span,
        };
        block
    }

    /// Creates a non-mutable, mangled, non-referencing variable defn.
    /// This is the vastly most common case
    fn synth_variable_defn_simple(
        &mut self,
        name: Identifier,
        initializer: TypedExpr,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, false, false, false, owner_scope)
    }

    /// Creates a user-code-visible variable
    fn synth_variable_defn_visible(
        &mut self,
        name: Identifier,
        initializer: TypedExpr,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, true, false, false, owner_scope)
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    fn synth_variable_defn(
        &mut self,
        name: Identifier,
        initializer: TypedExpr,
        no_mangle: bool,
        is_mutable: bool,
        is_referencing: bool,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        let type_id = if is_referencing {
            self.types.add_reference_type(initializer.get_type())
        } else {
            initializer.get_type()
        };
        let span = initializer.get_span();
        let new_ident = if no_mangle {
            name
        } else {
            let new_ident_name =
                format!("__{}_{}", self.ast.identifiers.get_name(name), owner_scope);
            self.ast.identifiers.intern(new_ident_name)
        };
        let variable = Variable {
            name: new_ident,
            is_mutable,
            owner_scope,
            type_id,
            is_context: false,
            is_global: false,
        };
        let variable_id = self.variables.add_variable(variable);
        let variable_expr = TypedExpr::Variable(VariableExpr { type_id, variable_id, span });
        let defn_stmt = TypedStmt::Let(Box::new(LetStmt {
            variable_id,
            variable_type: type_id,
            initializer,
            is_referencing,
            span,
        }));
        let parsed_expr =
            self.ast.expressions.add_expression(ParsedExpression::Variable(parse::Variable {
                name: NamespacedIdentifier::naked(name, span),
            }));
        self.scopes.add_variable(owner_scope, new_ident, variable_id);
        SynthedVariable { variable_id, defn_stmt, variable_expr, parsed_expr }
    }

    fn synth_parsed_function_call(
        &mut self,
        name: NamespacedIdentifier,
        type_args: Vec<ParsedTypeExpressionId>,
        args: Vec<ParsedExpressionId>,
    ) -> ParsedExpressionId {
        let span = name.span;
        let type_args = type_args.iter().map(|id| NamedTypeArg::unnamed(*id)).collect();
        let args = args.iter().map(|id| parse::FnCallArg::unnamed(*id)).collect();
        self.ast.expressions.add_expression(ParsedExpression::FnCall(FnCall {
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
        type_args: Vec<TypeId>,
        args: Vec<TypedExpr>,
        scope_id: ScopeId,
        expected_type_id: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let call_id = self.synth_parsed_function_call(name, vec![], vec![]);
        let call = self.ast.expressions.get(call_id).expect_fn_call().clone();
        self.eval_function_call(&call, scope_id, expected_type_id, Some((type_args, args)))
    }

    // These are only used by the old coalescing accessor and should be removed when its rebuilt
    fn ident_opt_has_value(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "isSome")
    }

    fn ident_opt_get(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "get")
    }

    #[allow(unused)]
    fn synth_type_of_expr(&mut self, expr: ParsedExpressionId) -> ParsedTypeExpressionId {
        let span = self.ast.expressions.get_span(expr);
        self.ast
            .type_expressions
            .add(ParsedTypeExpression::TypeOf(parse::ParsedTypeOf { target_expr: expr, span }))
    }

    fn synth_bool_not(&mut self, base: ParsedExpressionId) -> ParsedExpressionId {
        let span = self.ast.expressions.get_span(base);
        self.synth_parsed_function_call(
            qident!(self, span, ["bool"], "negated"),
            vec![],
            vec![base],
        )
    }

    fn synth_show_ident_call(
        &mut self,
        caller: ParsedExpressionId,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExpr> {
        let span = self.ast.expressions.get_span(caller);
        let call_id = self.synth_parsed_function_call(
            qident!(self, span, ["Show"], "show"),
            vec![],
            vec![caller],
        );
        let call = self.ast.expressions.get(call_id).expect_fn_call().clone();
        self.eval_function_call(&call, scope_id, None, None)
    }

    fn synth_struct_expr(
        &mut self,
        struct_type_id: TypeId,
        field_exprs: Vec<TypedExpr>,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExpr {
        let struct_type = self.types.get(struct_type_id).expect_struct();
        if struct_type.fields.len() != field_exprs.len() {
            panic!("Mismatching field count: {} / {}", struct_type.fields.len(), field_exprs.len());
        }
        let mut fields: Vec<StructField> = Vec::with_capacity(struct_type.fields.len());
        for (index, field_expr) in field_exprs.into_iter().enumerate() {
            let field = &struct_type.fields[index];
            if let Err(msg) = self.check_types(field.type_id, field_expr.get_type(), scope_id) {
                panic!("synthed struct fields failed typechecking: {}", msg)
            }
            fields.push(StructField { name: field.name, expr: field_expr });
        }
        TypedExpr::Struct(Struct { fields, type_id: struct_type_id, span })
    }

    fn synth_source_location(&self, span: SpanId) -> TypedExpr {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get_source(the_span.file_id);
        let line = source.get_line_for_span_start(the_span).unwrap();
        TypedExpr::Struct(Struct {
            fields: vec![
                StructField {
                    name: get_ident!(self, "filename"),
                    expr: TypedExpr::Str(source.filename.clone(), span),
                },
                StructField {
                    name: get_ident!(self, "line"),
                    expr: TypedExpr::Integer(TypedIntegerExpr {
                        value: TypedIntegerValue::U64(line.line_number() as u64),
                        span,
                    }),
                },
            ],
            type_id: COMPILER_SOURCE_LOC_TYPE_ID,
            span,
        })
    }

    pub fn push_error(&mut self, e: TyperError) {
        self.write_error(&mut std::io::stderr(), &e).unwrap();
        self.errors.push(e);
    }

    pub fn write_qualified_name(
        &self,
        w: &mut impl std::io::Write,
        scope: ScopeId,
        name: Identifier,
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
        write!(w, "{}", self.name_of(name)).unwrap();
    }

    pub fn make_qualified_name(
        &self,
        scope: ScopeId,
        name: Identifier,
        delimiter: &str,
        skip_root: bool,
    ) -> String {
        let mut buf = Vec::with_capacity(64);
        self.write_qualified_name(&mut buf, scope, name, delimiter, skip_root);
        String::from_utf8(buf).unwrap()
    }

    pub fn write_error(
        &self,
        w: &mut impl std::io::Write,
        error: &TyperError,
    ) -> std::io::Result<()> {
        write_error(w, &self.ast.spans, &self.ast.sources, &error.message, error.level, error.span)
    }

    pub fn ice(&self, msg: impl AsRef<str>, error: Option<&TyperError>) -> ! {
        if let Some(error) = error {
            self.write_error(&mut std::io::stderr(), error).unwrap();
        }
        panic!("Internal Compiler Error: {}", msg.as_ref())
    }
}
