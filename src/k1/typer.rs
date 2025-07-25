pub mod derive;
pub mod dump;
pub mod scopes;
pub mod types;

use ecow::{EcoVec, eco_vec};
use itertools::Itertools;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::stderr;
use std::num::NonZeroU32;
use std::path::{Path, PathBuf};
use std::time::Instant;

use ahash::HashMapExt;
use anyhow::bail;
use colored::Colorize;
use either::Either;
use fxhash::FxHashMap;
use log::{debug, trace};
use smallvec::{SmallVec, smallvec};

use scopes::*;
use types::*;

use crate::compiler::{CompilerConfig, WordSize};
use crate::lex::{self, SpanId, Spans, TokenKind};
use crate::parse::{
    self, FileId, ForExpr, ForExprType, Identifiers, NamedTypeArg, NamedTypeArgId,
    NamespacedIdentifier, NumericWidth, ParseError, ParsedAbilityId, ParsedAbilityImplId,
    ParsedBlockKind, ParsedCallArg, ParsedCast, ParsedDirective, ParsedExprId, ParsedFunctionId,
    ParsedGlobalId, ParsedId, ParsedIfExpr, ParsedLoopExpr, ParsedNamespaceId, ParsedPattern,
    ParsedPatternId, ParsedStaticBlockKind, ParsedStaticExpr, ParsedStmtId, ParsedTypeDefnId,
    ParsedTypeExpr, ParsedTypeExprId, ParsedUnaryOpKind, ParsedUseId, ParsedVariable,
    ParsedWhileExpr, Sources, StringId, StructValueField,
};
use crate::parse::{
    Ident, Literal, ParsedBlock, ParsedCall, ParsedExpr, ParsedProgram, ParsedStmt,
};
use crate::pool::{Pool, SliceHandle};
use crate::{SV4, impl_copy_if_small, nz_u32_id, static_assert_size, strings};
use crate::{SV8, vm};

#[cfg(test)]
mod layout_test;

nz_u32_id!(FunctionId);

nz_u32_id!(VariableId);

nz_u32_id!(NamespaceId);

pub const ROOT_NAMESPACE_ID: NamespaceId = NamespaceId(NonZeroU32::new(1).unwrap());

nz_u32_id!(AbilityId);

nz_u32_id!(AbilityImplId);

nz_u32_id!(TypedGlobalId);
nz_u32_id!(TypedStmtId);
nz_u32_id!(TypedExprId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Standard,
    External(Option<Ident>),
    Intrinsic,
}

#[derive(Debug, Clone)]
pub struct TypedAbilityFunctionRef {
    pub function_name: Ident,
    pub ability_id: AbilityId,
    pub function_id: FunctionId,
}
impl_copy_if_small!(12, TypedAbilityFunctionRef);

pub const EQUALS_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(1).unwrap());
pub const WRITER_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(2).unwrap());
pub const WRITE_TEXT_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(3).unwrap());
pub const SHOW_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(4).unwrap());
pub const BITWISE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(5).unwrap());
pub const COMPARABLE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(6).unwrap());
pub const UNWRAP_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(7).unwrap());
pub const TRY_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(8).unwrap());
pub const ITERATOR_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(9).unwrap());
pub const ITERABLE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(10).unwrap());

pub const FUNC_PARAM_IDEAL_COUNT: usize = 8;
pub const FUNC_TYPE_PARAM_IDEAL_COUNT: usize = 4;

pub const UNIT_BYTE_VALUE: u8 = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSubstitutionPair {
    from: TypeId,
    to: TypeId,
}

// Allows syntax spair! { a -> b }
macro_rules! spair {
    ($from:expr => $to:expr) => {
        TypeSubstitutionPair { from: $from, to: $to }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct InferenceInputPair {
    arg: TypeOrParsedExpr,
    param_type: TypeId,
    allow_mismatch: bool,
}

#[derive(Default)]
pub struct InferenceContext {
    pub origin_stack: Vec<SpanId>,
    pub params: Vec<TypeId>,
    pub solutions_so_far: EcoVec<TypeSubstitutionPair>,
    pub inference_vars: Vec<TypeId>,
    pub constraints: Vec<TypeSubstitutionPair>,
    pub substitutions: FxHashMap<TypeId, TypeId>,
    pub substitutions_vec: Vec<TypeSubstitutionPair>,
}

#[derive(Debug, Clone, Copy)]
pub struct StaticExecContext {
    is_metaprogram: bool,
    /// If a `return` is used, what type is expected
    /// This is needed because `return` usually looks at the
    /// enclosing function, but for #static blocks it shouldn't
    /// So this type can be different than the usual ctx.expected_type
    expected_return_type: Option<TypeId>,
}

#[derive(Debug, Clone, Copy)]
pub struct EvalExprContext {
    scope_id: ScopeId,
    expected_type_id: Option<TypeId>,
    is_inference: bool,
    static_ctx: Option<StaticExecContext>,
    global_defn_name: Option<Ident>,
    /// Indicates whether we are typechecking generic code
    /// Most commonly, the body of a generic function
    is_generic_pass: bool,
}
impl EvalExprContext {
    fn make(scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext {
            scope_id,
            expected_type_id: None,
            is_inference: false,
            static_ctx: None,
            global_defn_name: None,
            is_generic_pass: false,
        }
    }

    pub fn is_static(&self) -> bool {
        self.static_ctx.is_some()
    }

    pub fn with_expected_type(&self, expected_element_type: Option<TypeId>) -> EvalExprContext {
        EvalExprContext { expected_type_id: expected_element_type, ..*self }
    }

    pub fn with_no_expected_type(&self) -> EvalExprContext {
        EvalExprContext { expected_type_id: None, ..*self }
    }

    pub fn with_static_ctx(&self, static_ctx: Option<StaticExecContext>) -> EvalExprContext {
        EvalExprContext { static_ctx, ..*self }
    }

    fn with_inference(&self, is_inference: bool) -> EvalExprContext {
        EvalExprContext { is_inference, ..*self }
    }

    fn with_scope(&self, scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext { scope_id, ..*self }
    }

    pub fn with_is_generic_pass(&self, is_generic_pass: bool) -> EvalExprContext {
        EvalExprContext { is_generic_pass, ..*self }
    }
}

enum CoerceResult {
    Fail(TypedExprId),
    Coerced(&'static str, TypedExprId),
}

#[derive(Debug, Clone)]
enum MaybeTypedExpr {
    Parsed(ParsedExprId),
    Typed(TypedExprId),
}

#[derive(Debug, Clone, Copy)]
enum TypeOrParsedExpr {
    Type(TypeId),
    Parsed(ParsedExprId),
}

#[derive(Debug, Clone)]
pub struct StaticStruct {
    pub type_id: TypeId,
    pub fields: EcoVec<StaticValueId>,
}

#[derive(Debug, Clone)]
pub struct StaticEnum {
    pub variant_type_id: TypeId,
    pub variant_index: u32,
    /// Whether or not this thing is typed as the variant itself
    /// or as its enum
    pub typed_as_enum: bool,
    pub payload: Option<StaticValueId>,
}

#[derive(Debug, Clone)]
pub struct StaticBuffer {
    pub elements: EcoVec<StaticValueId>,
    pub type_id: TypeId,
}

impl StaticBuffer {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }
}

nz_u32_id!(StaticValueId);

static_assert_size!(StaticValue, 32);
#[derive(Debug, Clone)]
pub enum StaticValue {
    Unit,
    Boolean(bool),
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
    NullPointer,
    Struct(StaticStruct),
    Enum(StaticEnum),
    Buffer(StaticBuffer),
}

impl StaticValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            StaticValue::Unit => "unit",
            StaticValue::Boolean(_) => "bool",
            StaticValue::Char(_) => "char",
            StaticValue::Int(i) => i.kind_name(),
            StaticValue::Float(_) => "float",
            StaticValue::String(_) => "string",
            StaticValue::NullPointer => "nullptr",
            StaticValue::Struct(_) => "struct",
            StaticValue::Enum(_) => "enum",
            StaticValue::Buffer(_) => "buffer",
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            StaticValue::Unit => UNIT_TYPE_ID,
            StaticValue::Boolean(_) => BOOL_TYPE_ID,
            StaticValue::Char(_) => CHAR_TYPE_ID,
            StaticValue::Int(typed_integer_value) => typed_integer_value.get_type(),
            StaticValue::Float(typed_float_value) => typed_float_value.get_type(),
            StaticValue::String(_) => STRING_TYPE_ID,
            StaticValue::NullPointer => POINTER_TYPE_ID,
            StaticValue::Struct(s) => s.type_id,
            StaticValue::Enum(e) => e.variant_type_id,
            StaticValue::Buffer(b) => b.type_id,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            StaticValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    fn as_enum(&self) -> Option<&StaticEnum> {
        match self {
            StaticValue::Enum(e) => Some(e),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
}

impl Layout {
    pub fn from_rust_type<T>() -> Layout {
        let size = std::mem::size_of::<T>() as u32;
        let align = std::mem::align_of::<T>() as u32;
        Layout { size, align }
    }

    pub fn from_scalar_bytes(bytes: u32) -> Layout {
        Layout { size: bytes, align: bytes }
    }

    pub fn from_scalar_bits(bits: u32) -> Layout {
        Layout { size: bits / 8, align: bits / 8 }
    }
    pub const ZERO: Layout = Layout { size: 0, align: 1 };

    pub fn stride(&self) -> u32 {
        self.size.next_multiple_of(self.align)
    }

    // Returns: the start, or offset, of the new field
    pub fn append_to_aggregate(&mut self, layout: Layout) -> u32 {
        debug_assert_ne!(layout.align, 0);
        let offset = self.size;
        let new_field_start = offset.next_multiple_of(layout.align);
        if cfg!(debug_assertions) {
            let padding = new_field_start - offset;
            if padding != 0 {
                debug!("Aggregate padding: {padding}");
            }
        };
        let new_end_unaligned = new_field_start + layout.size;
        let new_align = std::cmp::max(self.align, layout.align);
        self.size = new_end_unaligned;
        self.align = new_align;
        new_field_start
    }

    pub fn size_bits(&self) -> u32 {
        self.size * 8
    }

    pub fn align_bits(&self) -> u32 {
        self.align * 8
    }

    pub fn array_me(&self, len: usize) -> Layout {
        let element_size_padded = self.stride();
        let array_size = element_size_padded * (len as u32);
        Layout { size: array_size, align: if array_size == 0 { 1 } else { self.align } }
    }
}

pub struct AggregateLayout {
    pub offsets: SmallVec<[u32; 8]>,
}

// nocommit(2): Switch to pool; make pattern matching code it less allocatey period
nz_u32_id!(PatternCtorId);

/// Used for analyzing pattern matching
#[derive(Debug, Clone)]
pub enum PatternCtor {
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
    Pointer,
    /// This one is also kinda a nothing burger, and can only be matched by Wildcards and Bindings; it's here for
    /// the sake of being explicit; we could collapse all these into a 'Anything' constructor but the fact I can't
    /// think of a good name means we shouldn't, probably
    TypeVariable,
    FunctionReference,
    Reference(Box<PatternCtor>),
    Struct {
        fields: Vec<(Ident, PatternCtor)>,
    },
    // Binding(Identifier),
    Enum {
        variant_name: Ident,
        inner: Option<Box<PatternCtor>>,
    },
}

#[derive(Debug, Clone)]
pub struct AbilitySpec9nInfo {
    generic_parent: AbilityId,
    specialized_child: AbilityId,
    arguments: NamedTypeSlice,
}
impl_copy_if_small!(16, AbilitySpec9nInfo);

#[derive(Debug, Clone)]
pub enum TypedAbilityKind {
    Concrete,
    Generic { specializations: Vec<AbilitySpec9nInfo> },
    Specialized(AbilitySpec9nInfo),
}

impl TypedAbilityKind {
    pub fn arguments(&self) -> NamedTypeSlice {
        match self {
            TypedAbilityKind::Specialized(specialization) => specialization.arguments,
            TypedAbilityKind::Concrete => SliceHandle::Empty,
            TypedAbilityKind::Generic { .. } => SliceHandle::Empty,
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
    name: Ident,
    type_variable_id: TypeId,
    is_impl_param: bool,
    #[allow(unused)]
    span: SpanId,
}
impl_copy_if_small!(16, TypedAbilityParam);

impl HasName for &TypedAbilityParam {
    fn name(&self) -> Ident {
        self.name
    }
}
impl HasTypeId for &TypedAbilityParam {
    fn type_id(&self) -> TypeId {
        self.type_variable_id
    }
}

impl HasName for TypedAbilityParam {
    fn name(&self) -> Ident {
        self.name
    }
}
impl HasTypeId for TypedAbilityParam {
    fn type_id(&self) -> TypeId {
        self.type_variable_id
    }
}

impl TypedAbilityParam {
    fn is_ability_side_param(&self) -> bool {
        !self.is_impl_param
    }
}

impl From<&TypedAbilityParam> for NameAndType {
    fn from(value: &TypedAbilityParam) -> Self {
        NameAndType { name: value.name, type_id: value.type_variable_id }
    }
}

#[derive(Debug, Clone, Copy)]
/// An ability signature encompasses an ability's entire 'type' story:
/// - Base type, generic type params, and impl-provided type params
///```md
///Example: Add[Rhs = Int]
///                   ^ impl argument
///             ^ ability argument
///         ^
///         Ability Id
///```
pub struct TypedAbilitySignature {
    specialized_ability_id: AbilityId,
    impl_arguments: NamedTypeSlice,
}

struct ArgsAndParams<'params> {
    args: SmallVec<[MaybeTypedExpr; FUNC_PARAM_IDEAL_COUNT]>,
    params: SmallVec<[&'params FnParamType; FUNC_PARAM_IDEAL_COUNT]>,
}

impl ArgsAndParams<'_> {
    fn iter(&self) -> impl Iterator<Item = (&MaybeTypedExpr, &&FnParamType)> {
        self.args.iter().zip(self.params.iter())
    }

    fn get(&self, index: usize) -> (&MaybeTypedExpr, &&FnParamType) {
        (&self.args[index], &self.params[index])
    }

    fn len(&self) -> usize {
        debug_assert!(self.args.len() == self.params.len());
        self.args.len()
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbility {
    pub name: Ident,
    pub base_ability_id: AbilityId,
    pub self_type_id: TypeId,
    pub parameters: EcoVec<TypedAbilityParam>,
    pub functions: EcoVec<TypedAbilityFunctionRef>,
    pub scope_id: ScopeId,
    pub ast_id: ParsedAbilityId,
    pub namespace_id: NamespaceId,
    pub kind: TypedAbilityKind,
}

impl TypedAbility {
    pub fn find_function_by_name(&self, name: Ident) -> Option<(usize, &TypedAbilityFunctionRef)> {
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
    pub variant_tag_name: Ident,
    pub variant_index: u32,
    pub payload: Option<Box<TypedPattern>>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedStructPatternField {
    pub name: Ident,
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
    pub name: Ident,
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
    LiteralInteger(TypedIntValue, SpanId),
    LiteralFloat(TypedFloatValue, SpanId),
    LiteralBool(bool, SpanId),
    LiteralString(StringId, SpanId),
    Variable(VariablePattern),
    Enum(TypedEnumPattern),
    Struct(TypedStructPattern),
    Wildcard(SpanId),
    Reference(TypedReferencePattern),
}

impl TypedPattern {
    pub fn all_bindings(&self) -> SmallVec<[VariablePattern; 8]> {
        let mut v = smallvec![];
        self.all_bindings_rec(&mut v);
        // This sorts by the Identifier id, not the name itself, but that's absolutely fine
        v.sort_by_key(|vp| vp.name);
        v
    }
    fn all_bindings_rec(&self, bindings: &mut SmallVec<[VariablePattern; 8]>) {
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
    pub parsed_expression_id: ParsedExprId,
    pub captures: Vec<VariableId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub expr_type: TypeId,
    pub scope_id: ScopeId,
    pub statements: EcoVec<TypedStmtId>,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct SpecializationInfo {
    pub parent_function: FunctionId,
    pub type_arguments: NamedTypeSlice,
    pub function_type_arguments: NamedTypeSlice,
    pub static_type_arguments: NamedTypeSlice,
    pub specialized_function_id: FunctionId,
    pub specialized_function_type: TypeId,
}
impl_copy_if_small!(36, SpecializationInfo);

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
pub struct FunctionSignature {
    pub name: Option<Ident>,
    pub function_type: TypeId,
    pub type_params: NamedTypeSlice,
    pub function_type_params: SliceHandle<ExistentialTypeParamId>,
    pub static_type_params: SliceHandle<ExistentialTypeParamId>,
}
impl_copy_if_small!(32, FunctionSignature);

impl FunctionSignature {
    pub fn make_no_generics(name: Option<Ident>, function_type: TypeId) -> FunctionSignature {
        FunctionSignature {
            name,
            function_type,
            type_params: SliceHandle::Empty,
            function_type_params: SliceHandle::Empty,
            static_type_params: SliceHandle::Empty,
        }
    }

    pub fn is_generic(&self) -> bool {
        !self.type_params.is_empty()
            || !self.function_type_params.is_empty()
            || !self.static_type_params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: Ident,
    pub scope: ScopeId,
    pub param_variables: EcoVec<VariableId>,
    pub type_params: NamedTypeSlice,
    pub function_type_params: SliceHandle<ExistentialTypeParamId>,
    pub static_type_params: SliceHandle<ExistentialTypeParamId>,
    pub body_block: Option<TypedExprId>,
    pub intrinsic_type: Option<IntrinsicOperation>,
    pub linkage: Linkage,
    pub child_specializations: Vec<SpecializationInfo>,
    pub specialization_info: Option<SpecializationInfo>,
    pub parsed_id: ParsedId,
    pub type_id: TypeId,
    pub compiler_debug: bool,
    pub kind: TypedFunctionKind,
    pub is_concrete: bool,
    /// If we've generated a 'dyn' copy of this function, we store its id
    pub dyn_fn_id: Option<FunctionId>,
}

impl TypedFunction {
    fn signature(&self) -> FunctionSignature {
        FunctionSignature {
            name: Some(self.name),
            function_type: self.type_id,
            type_params: self.type_params,
            function_type_params: self.function_type_params,
            static_type_params: self.static_type_params,
        }
    }

    fn is_generic(&self) -> bool {
        matches!(self.kind, TypedFunctionKind::AbilityDefn(_)) || self.signature().is_generic()
    }
}

nz_u32_id!(ExistentialTypeParamId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExistentialTypeParamKind {
    Function,
    Static,
}
#[derive(Debug, Clone, Copy)]
/// When a function takes a special type parameter, either a 'function_like'
/// or a 'static'. This ties the type parameter to its value param, which
/// is always a 1-1 relationship. As in:
/// fn example(knownInt: static int, some thunk: () -> ())
///            ^ existential type param 1, a static
///                                  ^ existential type param 2, a function type param
pub struct ExistentialTypeParam {
    pub name: Ident,
    pub type_id: TypeId,
    pub value_param_index: u32,
    pub span: SpanId,
    pub kind: ExistentialTypeParamKind,
}

#[derive(Debug, Clone)]
/// Used when a function takes a static parameter.
/// We represent this as a type-constraint in the function declaration
pub struct StaticTypeParam {
    pub name: Ident,
    pub type_id: TypeId,
    pub value_param_index: u32,
    pub span: SpanId,
}

impl HasName for &ExistentialTypeParam {
    fn name(&self) -> Ident {
        self.name
    }
}

impl HasTypeId for &ExistentialTypeParam {
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl HasName for ExistentialTypeParam {
    fn name(&self) -> Ident {
        self.name
    }
}

impl HasTypeId for ExistentialTypeParam {
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

pub trait HasName {
    fn name(&self) -> Ident;
}

pub trait HasTypeId {
    fn type_id(&self) -> TypeId;
}

pub trait NamedType: HasTypeId + HasName {}
impl<T> NamedType for T where T: HasTypeId + HasName {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// TODO(perf): We could certainly do a pass where we remove the name from tons and tons of places;
// this would cut size in half which is meaningful considering how many Vecs of these we have; and
// would like to use smallvec
pub struct NameAndType {
    pub name: Ident,
    pub type_id: TypeId,
}

nz_u32_id!(NameAndTypeId);

type NamedTypeSlice = SliceHandle<NameAndTypeId>;

impl HasName for &NameAndType {
    fn name(&self) -> Ident {
        self.name
    }
}
impl HasTypeId for &NameAndType {
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl HasName for NameAndType {
    fn name(&self) -> Ident {
        self.name
    }
}
impl HasTypeId for NameAndType {
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

enum TypeUnificationResult {
    Matching,
    NoHoles,
    NonMatching(&'static str),
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub variable_id: VariableId,
    pub type_id: TypeId,
    pub span: SpanId,
}
impl_copy_if_small!(12, VariableExpr);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    // Arith
    Add,
    Subtract,
    Multiply,
    Divide,
    Rem,

    // Cmp
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

impl BinaryOpKind {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BinaryOpKind::Add => "add",
            BinaryOpKind::Subtract => "sub",
            BinaryOpKind::Multiply => "mul",
            BinaryOpKind::Divide => "div",
            BinaryOpKind::Rem => "rem",
            BinaryOpKind::Less => "lt",
            BinaryOpKind::LessEqual => "le",
            BinaryOpKind::Greater => "gt",
            BinaryOpKind::GreaterEqual => "ge",
            BinaryOpKind::And => "and",
            BinaryOpKind::Or => "or",
            BinaryOpKind::Equals => "eq",
            BinaryOpKind::NotEquals => "neq",
            BinaryOpKind::OptionalElse => "else",
            BinaryOpKind::Pipe => "pipe",
        }
    }
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
impl_copy_if_small!(20, BinaryOp);

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
        lambda_value_expr: TypedExprId,
        lambda_type_id: TypeId,
    },
    /// When we're doing generic code that is never going to physically need to exist
    /// it's far cheaper and simpler to just say "Abstract call of this function type" than
    /// to specialize a copy of a function over a bunch of type params
    Abstract {
        function_sig: FunctionSignature,
    },
    /// Must contain a LambdaObject
    DynamicLambda(TypedExprId),
    /// Must contain a Function reference
    DynamicFunction {
        function_reference_expr: TypedExprId,
    },
    /// Used by function type parameters
    DynamicAbstract {
        variable_id: VariableId,
        function_sig: FunctionSignature,
    },
}
impl_copy_if_small!(40, Callee);

impl Callee {
    pub fn make_static(function_id: FunctionId) -> Callee {
        Callee::StaticFunction(function_id)
    }

    pub fn from_ability_impl_fn(ability_impl_fn: AbilityImplFunction) -> Callee {
        match ability_impl_fn {
            AbilityImplFunction::FunctionId(function_id) => Callee::StaticFunction(function_id),
            AbilityImplFunction::Abstract(sig) => Callee::Abstract { function_sig: sig },
        }
    }

    pub fn maybe_function_id(&self) -> Option<FunctionId> {
        match self {
            Callee::StaticFunction(function_id) => Some(*function_id),
            Callee::StaticLambda { function_id, .. } => Some(*function_id),
            Callee::Abstract { .. } => None,
            Callee::DynamicLambda(_) => None,
            Callee::DynamicFunction { .. } => None,
            Callee::DynamicAbstract { .. } => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Callee,
    pub args: SmallVec<[TypedExprId; FUNC_PARAM_IDEAL_COUNT]>,
    /// type_args remain unerased for some intrinsics where we want codegen to see the types.
    /// Specifically sizeOf[T], there's no actual value to specialize on, kinda of a hack would be
    /// better to specialize anyway and inline? idk
    pub type_args: NamedTypeSlice,
    pub return_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    pub expr: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
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
pub struct FieldAccess {
    pub base: TypedExprId,
    pub target_field: Ident,
    pub field_index: u32,
    pub result_type: TypeId,
    pub struct_type: TypeId,
    pub is_referencing: bool,
    pub span: SpanId,
}
impl_copy_if_small!(28, FieldAccess);

#[derive(Debug, Clone)]
pub struct TypedEnumConstructor {
    pub variant_type_id: TypeId,
    pub variant_index: u32,
    pub payload: Option<TypedExprId>,
    pub span: SpanId,
}
impl_copy_if_small!(16, TypedEnumConstructor);

#[derive(Debug, Clone)]
pub struct GetEnumVariantPayload {
    pub enum_variant_expr: TypedExprId,
    pub result_type_id: TypeId,
    pub variant_index: u32,
    pub is_referencing: bool,
    pub span: SpanId,
}
impl_copy_if_small!(20, GetEnumVariantPayload);

#[derive(Debug, Clone)]
pub struct GetEnumTag {
    pub enum_expr: TypedExprId,
    pub result_type_id: TypeId,
    pub span: SpanId,
}
impl_copy_if_small!(12, GetEnumTag);

#[derive(Debug, Clone)]
pub struct TypedEnumIsVariantExpr {
    pub enum_expr: TypedExprId,
    pub variant_index: u32,
    pub span: SpanId,
}
impl_copy_if_small!(16, TypedEnumIsVariantExpr);

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub condition: MatchingCondition,
    pub consequent_expr: TypedExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedIntValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    UWord32(u32),
    UWord64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    IWord32(i32),
    IWord64(i64),
}

#[macro_export]
macro_rules! int_binop {
    ($self:expr, $other:expr, $method:ident) => {
        match ($self, $other) {
            (TypedIntValue::U8(a), TypedIntValue::U8(b)) => TypedIntValue::U8(a.$method(*b)),
            (TypedIntValue::U16(a), TypedIntValue::U16(b)) => TypedIntValue::U16(a.$method(*b)),
            (TypedIntValue::U32(a), TypedIntValue::U32(b)) => TypedIntValue::U32(a.$method(*b)),
            (TypedIntValue::U64(a), TypedIntValue::U64(b)) => TypedIntValue::U64(a.$method(*b)),
            (TypedIntValue::UWord32(a), TypedIntValue::UWord32(b)) => {
                TypedIntValue::UWord32(a.$method(*b))
            }
            (TypedIntValue::UWord64(a), TypedIntValue::UWord64(b)) => {
                TypedIntValue::UWord64(a.$method(*b))
            }
            (TypedIntValue::I8(a), TypedIntValue::I8(b)) => TypedIntValue::I8(a.$method(*b)),
            (TypedIntValue::I16(a), TypedIntValue::I16(b)) => TypedIntValue::I16(a.$method(*b)),
            (TypedIntValue::I32(a), TypedIntValue::I32(b)) => TypedIntValue::I32(a.$method(*b)),
            (TypedIntValue::I64(a), TypedIntValue::I64(b)) => TypedIntValue::I64(a.$method(*b)),
            (TypedIntValue::IWord32(a), TypedIntValue::IWord32(b)) => {
                TypedIntValue::IWord32(a.$method(*b))
            }
            (TypedIntValue::IWord64(a), TypedIntValue::IWord64(b)) => {
                TypedIntValue::IWord64(a.$method(*b))
            }
            _ => panic!("mismatched int binop types"),
        }
    };
}

impl TypedIntValue {
    pub fn kind_name(&self) -> &'static str {
        match self {
            TypedIntValue::U8(_) => "u8",
            TypedIntValue::U16(_) => "u16",
            TypedIntValue::U32(_) => "u32",
            TypedIntValue::U64(_) => "u64",
            TypedIntValue::UWord32(_) => "uword",
            TypedIntValue::UWord64(_) => "uword",
            TypedIntValue::I8(_) => "i8",
            TypedIntValue::I16(_) => "i16",
            TypedIntValue::I32(_) => "i32",
            TypedIntValue::I64(_) => "i64",
            TypedIntValue::IWord32(_) => "iword",
            TypedIntValue::IWord64(_) => "iword",
        }
    }

    pub fn get_type(&self) -> TypeId {
        self.get_integer_type().type_id()
    }

    pub fn bit_not(&self) -> Self {
        use std::ops::Not;

        match *self {
            TypedIntValue::U8(x) => TypedIntValue::U8(x.not()),
            TypedIntValue::U16(x) => TypedIntValue::U16(x.not()),
            TypedIntValue::U32(x) => TypedIntValue::U32(x.not()),
            TypedIntValue::U64(x) => TypedIntValue::U64(x.not()),
            TypedIntValue::UWord32(x) => TypedIntValue::UWord32(x.not()),
            TypedIntValue::UWord64(x) => TypedIntValue::UWord64(x.not()),
            TypedIntValue::I8(x) => TypedIntValue::I8(x.not()),
            TypedIntValue::I16(x) => TypedIntValue::I16(x.not()),
            TypedIntValue::I32(x) => TypedIntValue::I32(x.not()),
            TypedIntValue::I64(x) => TypedIntValue::I64(x.not()),
            TypedIntValue::IWord32(x) => TypedIntValue::IWord32(x.not()),
            TypedIntValue::IWord64(x) => TypedIntValue::IWord64(x.not()),
        }
    }

    pub fn bit_and(&self, other: &Self) -> Self {
        use std::ops::BitAnd;
        int_binop!(self, other, bitand)
    }

    pub fn bit_or(&self, other: &Self) -> Self {
        use std::ops::BitOr;
        int_binop!(self, other, bitor)
    }

    pub fn bit_xor(&self, other: &Self) -> Self {
        use std::ops::BitXor;
        int_binop!(self, other, bitxor)
    }

    pub fn to_u64(&self) -> u64 {
        match self {
            TypedIntValue::U8(v) => *v as u64,
            TypedIntValue::U16(v) => *v as u64,
            TypedIntValue::U32(v) => *v as u64,
            TypedIntValue::U64(v) => *v,
            TypedIntValue::UWord32(v) => *v as u64,
            TypedIntValue::UWord64(v) => *v,
            TypedIntValue::I8(v) => *v as u64,
            TypedIntValue::I16(v) => *v as u64,
            TypedIntValue::I32(v) => *v as u64,
            TypedIntValue::I64(v) => *v as u64,
            TypedIntValue::IWord32(v) => *v as u64,
            TypedIntValue::IWord64(v) => *v as u64,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u32(&self) -> u32 {
        match self {
            TypedIntValue::U8(v) => *v as u32,
            TypedIntValue::U16(v) => *v as u32,
            TypedIntValue::U32(v) => *v,
            TypedIntValue::U64(v) => *v as u32,
            TypedIntValue::UWord32(v) => *v,
            TypedIntValue::UWord64(v) => *v as u32,
            TypedIntValue::I8(v) => *v as u32,
            TypedIntValue::I16(v) => *v as u32,
            TypedIntValue::I32(v) => *v as u32,
            TypedIntValue::I64(v) => *v as u32,
            TypedIntValue::IWord32(v) => *v as u32,
            TypedIntValue::IWord64(v) => *v as u32,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u16(&self) -> u16 {
        match self {
            TypedIntValue::U8(v) => *v as u16,
            TypedIntValue::U16(v) => *v,
            TypedIntValue::U32(v) => *v as u16,
            TypedIntValue::U64(v) => *v as u16,
            TypedIntValue::UWord32(v) => *v as u16,
            TypedIntValue::UWord64(v) => *v as u16,
            TypedIntValue::I8(v) => *v as u16,
            TypedIntValue::I16(v) => *v as u16,
            TypedIntValue::I32(v) => *v as u16,
            TypedIntValue::I64(v) => *v as u16,
            TypedIntValue::IWord32(v) => *v as u16,
            TypedIntValue::IWord64(v) => *v as u16,
        }
    }

    // Used for truncation/extension in vm
    pub fn to_u8(&self) -> u8 {
        match self {
            TypedIntValue::U8(v) => *v,
            TypedIntValue::U16(v) => *v as u8,
            TypedIntValue::U32(v) => *v as u8,
            TypedIntValue::U64(v) => *v as u8,
            TypedIntValue::UWord32(v) => *v as u8,
            TypedIntValue::UWord64(v) => *v as u8,
            TypedIntValue::I8(v) => *v as u8,
            TypedIntValue::I16(v) => *v as u8,
            TypedIntValue::I32(v) => *v as u8,
            TypedIntValue::I64(v) => *v as u8,
            TypedIntValue::IWord32(v) => *v as u8,
            TypedIntValue::IWord64(v) => *v as u8,
        }
    }

    pub fn to_i32(&self) -> i32 {
        match self {
            TypedIntValue::U8(v) => *v as i32,
            TypedIntValue::U16(v) => *v as i32,
            TypedIntValue::U32(v) => *v as i32,
            TypedIntValue::U64(v) => *v as i32,
            TypedIntValue::UWord32(v) => *v as i32,
            TypedIntValue::UWord64(v) => *v as i32,
            TypedIntValue::I8(v) => *v as i32,
            TypedIntValue::I16(v) => *v as i32,
            TypedIntValue::I32(v) => *v,
            TypedIntValue::I64(v) => *v as i32,
            TypedIntValue::IWord32(v) => *v,
            TypedIntValue::IWord64(v) => *v as i32,
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            TypedIntValue::U8(v) => *v as i64,
            TypedIntValue::U16(v) => *v as i64,
            TypedIntValue::U32(v) => *v as i64,
            TypedIntValue::U64(v) => *v as i64,
            TypedIntValue::UWord32(v) => *v as i64,
            TypedIntValue::UWord64(v) => *v as i64,
            TypedIntValue::I8(v) => *v as i64,
            TypedIntValue::I16(v) => *v as i64,
            TypedIntValue::I32(v) => *v as i64,
            TypedIntValue::I64(v) => *v,
            TypedIntValue::IWord32(v) => *v as i64,
            TypedIntValue::IWord64(v) => *v,
        }
    }

    pub fn get_integer_type(&self) -> IntegerType {
        match self {
            TypedIntValue::U8(_) => IntegerType::U8,
            TypedIntValue::U16(_) => IntegerType::U16,
            TypedIntValue::U32(_) => IntegerType::U32,
            TypedIntValue::U64(_) => IntegerType::U64,
            TypedIntValue::UWord32(_) => IntegerType::UWord(WordSize::W32),
            TypedIntValue::UWord64(_) => IntegerType::UWord(WordSize::W64),
            TypedIntValue::I8(_) => IntegerType::I8,
            TypedIntValue::I16(_) => IntegerType::I16,
            TypedIntValue::I32(_) => IntegerType::I32,
            TypedIntValue::I64(_) => IntegerType::I64,
            TypedIntValue::IWord32(_) => IntegerType::IWord(WordSize::W32),
            TypedIntValue::IWord64(_) => IntegerType::IWord(WordSize::W64),
        }
    }

    pub fn expect_uword(&self) -> usize {
        match self {
            TypedIntValue::UWord64(v) => *v as usize,
            TypedIntValue::UWord32(v) => *v as usize,
            _ => unreachable!(),
        }
    }

    pub fn expect_u64(&self) -> u64 {
        match self {
            TypedIntValue::U64(v) => *v,
            _ => unreachable!(),
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            TypedIntValue::I8(_)
            | TypedIntValue::I16(_)
            | TypedIntValue::I32(_)
            | TypedIntValue::I64(_)
            | TypedIntValue::IWord32(_)
            | TypedIntValue::IWord64(_) => true,
            TypedIntValue::U8(_)
            | TypedIntValue::U16(_)
            | TypedIntValue::U32(_)
            | TypedIntValue::U64(_)
            | TypedIntValue::UWord32(_)
            | TypedIntValue::UWord64(_) => false,
        }
    }
}

impl From<u64> for TypedIntValue {
    fn from(value: u64) -> Self {
        TypedIntValue::U64(value)
    }
}
impl From<u32> for TypedIntValue {
    fn from(value: u32) -> Self {
        TypedIntValue::U32(value)
    }
}
impl From<u16> for TypedIntValue {
    fn from(value: u16) -> Self {
        TypedIntValue::U16(value)
    }
}
impl From<u8> for TypedIntValue {
    fn from(value: u8) -> Self {
        TypedIntValue::U8(value)
    }
}
impl From<i64> for TypedIntValue {
    fn from(value: i64) -> Self {
        TypedIntValue::I64(value)
    }
}
impl From<i32> for TypedIntValue {
    fn from(value: i32) -> Self {
        TypedIntValue::I32(value)
    }
}
impl From<i16> for TypedIntValue {
    fn from(value: i16) -> Self {
        TypedIntValue::I16(value)
    }
}
impl From<i8> for TypedIntValue {
    fn from(value: i8) -> Self {
        TypedIntValue::I8(value)
    }
}

impl Display for TypedIntValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedIntValue::U8(v) => write!(f, "{}u8", v),
            TypedIntValue::U16(v) => write!(f, "{}u16", v),
            TypedIntValue::U32(v) => write!(f, "{}u32", v),
            TypedIntValue::U64(v) => write!(f, "{}u64", v),
            TypedIntValue::UWord32(v) => write!(f, "{}uword", v),
            TypedIntValue::UWord64(v) => write!(f, "{}uword", v),
            TypedIntValue::I8(v) => write!(f, "{}i8", v),
            TypedIntValue::I16(v) => write!(f, "{}i16", v),
            TypedIntValue::I32(v) => write!(f, "{}i32", v),
            TypedIntValue::I64(v) => write!(f, "{}i64", v),
            TypedIntValue::IWord32(v) => write!(f, "{}iword", v),
            TypedIntValue::IWord64(v) => write!(f, "{}iword", v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIntegerExpr {
    pub value: TypedIntValue,
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

    pub fn get_width(&self) -> NumericWidth {
        match self {
            TypedFloatValue::F32(_) => NumericWidth::B32,
            TypedFloatValue::F64(_) => NumericWidth::B64,
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
pub enum IntegerCastDirection {
    Extend,
    Truncate,
    NoOp,
}

#[derive(Debug, Clone, Copy)]
pub enum CastType {
    IntegerCast(IntegerCastDirection),
    Integer8ToChar,
    IntegerExtendFromChar,
    EnumToVariant,
    VariantToEnum,
    ReferenceToReference,
    PointerToReference,
    ReferenceToPointer,
    /// Destination type can only be uword and iword
    PointerToWord,
    IntegerToPointer,
    FloatExtend,
    FloatTruncate,
    FloatToInteger,
    IntegerToFloat,
    LambdaToLambdaObject,
    ToNever,
    StaticErase,
}

impl Display for CastType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CastType::IntegerCast(_dir) => write!(f, "intcast"),
            CastType::Integer8ToChar => write!(f, "i8tochar"),
            CastType::IntegerExtendFromChar => write!(f, "iextfromchar"),
            CastType::EnumToVariant => write!(f, "enum2variant"),
            CastType::VariantToEnum => write!(f, "variant2enum"),
            CastType::ReferenceToReference => write!(f, "reftoref"),
            CastType::PointerToReference => write!(f, "ptrtoref"),
            CastType::ReferenceToPointer => write!(f, "reftoptr"),
            CastType::PointerToWord => write!(f, "ptrtoword"),
            CastType::IntegerToPointer => write!(f, "inttoptr"),
            CastType::FloatExtend => write!(f, "fext"),
            CastType::FloatTruncate => write!(f, "ftrunc"),
            CastType::FloatToInteger => write!(f, "ftoint"),
            CastType::IntegerToFloat => write!(f, "inttof"),
            CastType::LambdaToLambdaObject => write!(f, "lam2dyn"),
            CastType::ToNever => write!(f, "never"),
            CastType::StaticErase => write!(f, "staticerase"),
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
pub struct FunctionToLambdaObjectExpr {
    pub function_id: FunctionId,
    pub span: SpanId,
    pub lambda_object_type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct FunctionReferenceExpr {
    pub function_id: FunctionId,
    pub function_reference_type: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct PendingCaptureExpr {
    pub captured_variable_id: VariableId,
    pub type_id: TypeId,
    pub resolved_expr: Option<TypedExprId>,
    pub span: SpanId,
}

// TOOD(perf): Intern 'patterns'
#[derive(Debug, Clone)]
pub struct MatchingCondition {
    /// Multiple patterns does _not_ mean the user provided multiple patterns in a switch,
    /// that case currently gets compiled to N arms, where N is the number of alternate patterns
    /// Though that's something I could now consider changing, the trick would be to generate
    /// a single set of variables for all the bindings that each point to the unique variables from each
    /// pattern. We already check for exact number and name, so this is possible
    pub patterns: SmallVec<[TypedPattern; 1]>,
    pub instrs: EcoVec<MatchingConditionInstr>,
    #[allow(unused)]
    pub binding_eligible: bool,
    pub diverges: bool,
}

#[derive(Debug, Clone)]
pub enum MatchingConditionInstr {
    Binding { let_stmt: TypedStmtId, variable_id: VariableId },
    Cond { value: TypedExprId },
}
impl_copy_if_small!(8, MatchingConditionInstr);

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub condition_block: MatchingCondition,
    pub body: TypedExprId,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body_block: TypedExprId,
    pub break_type: TypeId,
    pub span: SpanId,
}
impl_copy_if_small!(12, LoopExpr);

#[derive(Debug, Clone)]
/// Invariant: The last arm's condition must always evaluate to 'true'
pub struct TypedMatchExpr {
    pub initial_let_statements: EcoVec<TypedStmtId>,
    pub result_type: TypeId,
    pub arms: EcoVec<TypedMatchArm>,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy)]
pub enum ToEmit {
    Parsed(ParsedStmtId),
    /// Expression evaluating to a string, to be parsed, if executed at compile-time
    String(TypedExprId),
    Variable {
        typed_let_stmt: TypedStmtId,
    },
}

// TODO(perf): TypedExpr is very big
static_assert_size!(TypedExpr, 104);
#[derive(Debug, Clone)]
pub enum TypedExpr {
    Unit(SpanId),
    Char(u8, SpanId),
    Bool(bool, SpanId),
    Integer(TypedIntegerExpr),
    Float(TypedFloatExpr),
    String(StringId, SpanId),
    Struct(StructLiteral),
    StructFieldAccess(FieldAccess),
    Variable(VariableExpr),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Block(TypedBlock),
    Call(Call),
    /// In the past, we lowered match to an if/else chain. This proves not quite powerful enough
    /// of a representation to do everything we want
    Match(TypedMatchExpr),
    WhileLoop(WhileLoop),
    LoopExpr(LoopExpr),
    EnumConstructor(TypedEnumConstructor),
    EnumIsVariant(TypedEnumIsVariantExpr),
    EnumGetTag(GetEnumTag),
    EnumGetPayload(GetEnumVariantPayload),
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
    /// Calling .toDyn() on a function by name
    FunctionReference(FunctionReferenceExpr),
    /// # Evaluation
    /// To evaluate a FunctionToLambdaObject,
    /// you must create an empty struct of the specified type
    /// and then construct a lambda object struct using
    /// - the address function pointed at by function_id as the function ptr
    /// - the (empty) environment struct; it will not be accessed
    FunctionToLambdaObject(FunctionToLambdaObjectExpr),
    /// These get re-written into struct access expressions once we know all captures and have
    /// generated the lambda's capture struct
    PendingCapture(PendingCaptureExpr),
    StaticValue(StaticValueId, TypeId, SpanId),
}

impl From<VariableExpr> for TypedExpr {
    fn from(value: VariableExpr) -> Self {
        TypedExpr::Variable(value)
    }
}

impl TypedExpr {
    pub fn kind_str(&self) -> &'static str {
        match self {
            TypedExpr::Unit(_) => "unit",
            TypedExpr::Char(_, _) => "char",
            TypedExpr::String(_, _) => "string",
            TypedExpr::Integer(_) => "integer",
            TypedExpr::Float(_) => "float",
            TypedExpr::Bool(_, _) => "bool",
            TypedExpr::Struct(_) => "struct",
            TypedExpr::Variable(_) => "variable",
            TypedExpr::StructFieldAccess(_) => "struct_field_access",
            TypedExpr::UnaryOp(_) => "unary_op",
            TypedExpr::BinaryOp(_) => "binary_op",
            TypedExpr::Block(_) => "block",
            TypedExpr::Call(_) => "call",
            TypedExpr::Match(_) => "match",
            TypedExpr::WhileLoop(_) => "while_loop",
            TypedExpr::LoopExpr(_) => "loop",
            TypedExpr::EnumConstructor(_) => "enum_constructor",
            TypedExpr::EnumIsVariant(_) => "enum_is_variant",
            TypedExpr::EnumGetTag(_) => "enum_get_tag",
            TypedExpr::EnumGetPayload(_) => "enum_get_payload",
            TypedExpr::Cast(_) => "cast",
            TypedExpr::Return(_) => "return",
            TypedExpr::Break(_) => "break",
            TypedExpr::Lambda(_) => "lambda",
            TypedExpr::FunctionReference(_) => "function_reference",
            TypedExpr::FunctionToLambdaObject(_) => "function_to_lambda_object",
            TypedExpr::PendingCapture(_) => "pending_capture",
            TypedExpr::StaticValue(_, _, _) => "static_value",
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            TypedExpr::Unit(_) => UNIT_TYPE_ID,
            TypedExpr::Char(_, _) => CHAR_TYPE_ID,
            TypedExpr::String(_, _) => STRING_TYPE_ID,
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
            TypedExpr::Match(match_) => match_.result_type,
            TypedExpr::WhileLoop(while_loop) => while_loop.type_id,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.break_type,
            TypedExpr::EnumConstructor(enum_cons) => enum_cons.variant_type_id,
            TypedExpr::EnumIsVariant(_is_variant) => BOOL_TYPE_ID,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.result_type_id,
            TypedExpr::EnumGetTag(get_tag) => get_tag.result_type_id,
            TypedExpr::Cast(c) => c.target_type_id,
            TypedExpr::Return(_ret) => NEVER_TYPE_ID,
            TypedExpr::Break(_break) => NEVER_TYPE_ID,
            TypedExpr::Lambda(lambda) => lambda.lambda_type,
            TypedExpr::FunctionReference(f) => f.function_reference_type,
            TypedExpr::FunctionToLambdaObject(f) => f.lambda_object_type_id,
            TypedExpr::PendingCapture(pc) => pc.type_id,
            TypedExpr::StaticValue(_, type_id, _) => *type_id,
        }
    }

    pub fn get_span(&self) -> SpanId {
        match self {
            TypedExpr::Unit(span) => *span,
            TypedExpr::Char(_, span) => *span,
            TypedExpr::Bool(_, span) => *span,
            TypedExpr::Integer(int) => int.span,
            TypedExpr::Float(float) => float.span,
            TypedExpr::String(_, span) => *span,
            TypedExpr::Struct(struc) => struc.span,
            TypedExpr::Variable(var) => var.span,
            TypedExpr::StructFieldAccess(field_access) => field_access.span,
            TypedExpr::BinaryOp(binary_op) => binary_op.span,
            TypedExpr::UnaryOp(unary_op) => unary_op.span,
            TypedExpr::Block(b) => b.span,
            TypedExpr::Call(call) => call.span,
            TypedExpr::Match(match_) => match_.span,
            TypedExpr::WhileLoop(while_loop) => while_loop.span,
            TypedExpr::LoopExpr(loop_expr) => loop_expr.span,
            TypedExpr::EnumConstructor(e) => e.span,
            TypedExpr::EnumIsVariant(is_variant) => is_variant.span,
            TypedExpr::EnumGetPayload(as_variant) => as_variant.span,
            TypedExpr::EnumGetTag(get_tag) => get_tag.span,
            TypedExpr::Cast(c) => c.span,
            TypedExpr::Return(ret) => ret.span,
            TypedExpr::Break(brk) => brk.span,
            TypedExpr::Lambda(lambda) => lambda.span,
            TypedExpr::FunctionReference(f) => f.span,
            TypedExpr::FunctionToLambdaObject(f) => f.span,
            TypedExpr::PendingCapture(pc) => pc.span,
            TypedExpr::StaticValue(_, _, span) => *span,
        }
    }

    pub fn expect_variable(self) -> VariableExpr {
        if let Self::Variable(v) = self { v } else { panic!("Expected variable expression") }
    }

    pub fn expect_call(&self) -> &Call {
        if let Self::Call(c) = self { c } else { panic!("Expected call expression") }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, TypedExpr::Unit(_))
    }
}

enum CheckExprTypeResult {
    Ok,
    Err(String),
    Coerce(TypedExprId),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub variable_id: VariableId,
    pub variable_type: TypeId,
    pub initializer: TypedExprId,
    pub is_referencing: bool,
    pub span: SpanId,
}
impl_copy_if_small!(20, LetStmt);

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
impl_copy_if_small!(16, AssignmentStmt);

#[derive(Debug, Clone)]
pub struct TypedRequireStmt {
    pub condition: Box<MatchingCondition>,
    pub else_body: TypedExprId,
    pub span: SpanId,
}

static_assert_size!(TypedStmt, 24);
#[derive(Debug, Clone)]
pub enum TypedStmt {
    Expr(TypedExprId, TypeId),
    Let(LetStmt),
    Assignment(AssignmentStmt),
    Require(TypedRequireStmt),
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
    pub parsed_expr: ParsedExprId,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Ident,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub owner_scope: ScopeId,
    pub is_context: bool,
    pub global_id: Option<TypedGlobalId>,
    pub user_hidden: bool,
}

#[derive(Debug)]
pub struct TypedGlobal {
    pub variable_id: VariableId,
    pub parsed_expr: ParsedExprId,
    pub initial_value: Option<StaticValueId>,
    pub ty: TypeId,
    pub span: SpanId,
    pub is_constant: bool,
    pub is_referencing: bool,
    pub ast_id: ParsedGlobalId,
    pub parent_scope: ScopeId,
}

#[derive(Debug)]
pub enum NamespaceType {
    User,
    Ability,
    Root,
}

#[derive(Debug)]
pub struct Namespace {
    pub name: Ident,
    pub scope_id: ScopeId,
    pub namespace_type: NamespaceType,
    pub companion_type_id: Option<TypeId>,
    pub parent_id: Option<NamespaceId>,
    pub owner_module: Option<ModuleId>,
    pub parsed_id: ParsedId,
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

    pub fn find_child_by_name(&self, parent_id: NamespaceId, name: Ident) -> Option<&Namespace> {
        self.iter()
            .find(|ns| ns.parent_id.is_some_and(|parent| parent == parent_id) && ns.name == name)
    }

    pub fn name_chain(&self, id: NamespaceId) -> VecDeque<Ident> {
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
pub enum IntrinsicBitwiseBinopKind {
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOperation {
    SizeOf,
    SizeOfStride,
    AlignOf,
    Zeroed,
    TypeId,
    TypeName,
    CompilerSourceLocation,
    // Actual function
    TypeSchema,

    BoolNegate,
    BitNot,
    BitwiseBinop(IntrinsicBitwiseBinopKind),
    PointerIndex,

    // Actual functions
    Allocate,
    AllocateZeroed,
    Reallocate,
    Free,
    MemCopy,
    MemSet,
    MemEquals,
    Exit,
    // Static-only
    CompilerMessage,
    EmitString,
    BakeStaticValue,
    GetStaticValue,
}

impl IntrinsicOperation {
    pub fn is_typer_phase(self) -> bool {
        match self {
            IntrinsicOperation::SizeOf => true,
            IntrinsicOperation::SizeOfStride => true,
            IntrinsicOperation::AlignOf => true,
            IntrinsicOperation::TypeId => true,
            IntrinsicOperation::CompilerSourceLocation => true,
            IntrinsicOperation::GetStaticValue => true,
            //
            IntrinsicOperation::Zeroed => false,
            IntrinsicOperation::BoolNegate => false,
            IntrinsicOperation::BitNot => false,
            IntrinsicOperation::BitwiseBinop(_) => false,
            IntrinsicOperation::PointerIndex => false,
            IntrinsicOperation::CompilerMessage => false,
            IntrinsicOperation::Allocate => false,
            IntrinsicOperation::AllocateZeroed => false,
            IntrinsicOperation::Reallocate => false,
            IntrinsicOperation::Free => false,
            IntrinsicOperation::MemCopy => false,
            IntrinsicOperation::MemSet => false,
            IntrinsicOperation::MemEquals => false,
            IntrinsicOperation::Exit => false,
            IntrinsicOperation::TypeName => false,
            IntrinsicOperation::TypeSchema => false,
            IntrinsicOperation::EmitString => false,
            IntrinsicOperation::BakeStaticValue => false,
        }
    }

    /// Determines whether a physical function should be generated for this
    /// operation; currently used by the LLVM backend
    pub fn is_inlined(self) -> bool {
        match self {
            IntrinsicOperation::SizeOf => true,
            IntrinsicOperation::SizeOfStride => true,
            IntrinsicOperation::AlignOf => true,
            IntrinsicOperation::Zeroed => true,
            IntrinsicOperation::TypeId => true,
            IntrinsicOperation::BoolNegate => true,
            IntrinsicOperation::BitNot => true,
            IntrinsicOperation::BitwiseBinop(_) => true,
            IntrinsicOperation::PointerIndex => true,
            IntrinsicOperation::CompilerSourceLocation => true,
            IntrinsicOperation::CompilerMessage => true,
            // System-level
            IntrinsicOperation::Allocate => false,
            IntrinsicOperation::AllocateZeroed => false,
            IntrinsicOperation::Reallocate => false,
            IntrinsicOperation::Free => false,
            IntrinsicOperation::MemCopy => false,
            IntrinsicOperation::MemSet => false,
            IntrinsicOperation::MemEquals => false,
            IntrinsicOperation::Exit => false,
            // Runtime switch on typeId, likely inlined
            IntrinsicOperation::TypeName => false,
            IntrinsicOperation::TypeSchema => false,
            // Metaprogramming
            IntrinsicOperation::EmitString => true,
            IntrinsicOperation::GetStaticValue => true,
            IntrinsicOperation::BakeStaticValue => true,
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

#[macro_export]
macro_rules! format_ident {
    ($self: ident, $($format_args:expr),* $(,)?) => {
        {
            use std::fmt::Write;
            let mut s = std::mem::take(&mut $self.buffers.name_builder);
            s.write_fmt(format_args!($($format_args),*)).unwrap();
            let ident = $self.ast.idents.intern(&s);
            s.clear();
            $self.buffers.name_builder = s;
            ident
        }
    }
}

#[macro_export]
macro_rules! get_ident {
    ($self:ident, $name:expr) => {
        $self
            .ast
            .idents
            .get($name)
            .unwrap_or_else(|| panic!("Missing identifier '{}' in pool", $name))
    };
}

/// Make a qualified, `NamespacedIdentifier` from components
macro_rules! qident {
    ($self:ident, $span:expr, $namespaces:expr, $name:expr $(,)?) => {{
        let idents: EcoVec<Ident> = ($namespaces).iter().map(|n| get_ident!($self, n)).collect();
        NamespacedIdentifier { namespaces: idents, name: get_ident!($self, $name), span: $span }
    }};
    ($self:ident, $span:expr, $name:expr) => {{ NamespacedIdentifier { namespaces: eco_vec![], name: get_ident!($self, $name), span: $span } }};
}

fn make_fail_ast_id<A, T: AsRef<str>>(
    ast: &ParsedProgram,
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
    parse::write_source_location(w, spans, sources, span, level, 6, Some(message.as_ref()))?;
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

#[derive(Clone, Copy)]
pub enum AbilityImplFunction {
    FunctionId(FunctionId),
    Abstract(FunctionSignature),
}

#[derive(Clone)]
pub struct TypedAbilityImpl {
    pub kind: AbilityImplKind,
    pub blanket_type_params: NamedTypeSlice,
    pub self_type_id: TypeId,
    pub base_ability_id: AbilityId,
    pub ability_id: AbilityId,
    /// The values for the types that the implementation is responsible for providing.
    /// Yes, they are already baked into the functions but I need them explicitly in order
    /// to do constraint checking
    pub impl_arguments: NamedTypeSlice,
    /// Invariant: These functions are ordered how they are defined in the ability, NOT how they appear in
    /// the impl code
    pub functions: EcoVec<AbilityImplFunction>,
    pub scope_id: ScopeId,
    pub span: SpanId,
    /// I need this so that I don't try to instantiate blanket implementations that fail
    /// typechecking
    pub compile_errors: Vec<TyperError>,
}

impl TypedAbilityImpl {
    pub fn function_at_index(&self, index: usize) -> AbilityImplFunction {
        self.functions[index]
    }

    pub fn signature(&self) -> TypedAbilitySignature {
        TypedAbilitySignature {
            specialized_ability_id: self.ability_id,
            impl_arguments: self.impl_arguments,
        }
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

#[derive(Debug, Clone)]
struct EvalTypeExprContext {
    /// `direct_*` locations mean top-level of the thing they describe
    /// If this is a type definition, this is its type id (probably a Type::Unresolved)
    /// If set, we're supposed to store our result in this type_id
    direct_unresolved_target_type: Option<TypeId>,
    is_direct_function_parameter: bool,
    #[allow(unused)]
    is_direct_variable_binding: bool,

    /// `inside_*` locations mean anywhere inside of the thing they describe, including top-level
    is_inside_type_definition_rhs: bool,
    is_inside_static_type: bool,
}
impl_copy_if_small!(28, EvalTypeExprContext);

impl EvalTypeExprContext {
    /// If we descend into a type, we can categorically clear all `direct_`
    /// fields, by definition
    pub fn descended(&self) -> EvalTypeExprContext {
        EvalTypeExprContext {
            direct_unresolved_target_type: None,
            is_direct_function_parameter: false,
            is_direct_variable_binding: false,
            ..*self
        }
    }

    pub const EMPTY: Self = EvalTypeExprContext {
        direct_unresolved_target_type: None,
        is_direct_function_parameter: false,
        is_direct_variable_binding: false,
        is_inside_type_definition_rhs: false,
        is_inside_static_type: false,
    };

    pub const VARIABLE_BINDING: Self = EvalTypeExprContext {
        direct_unresolved_target_type: None,
        is_direct_function_parameter: false,
        is_direct_variable_binding: true,
        is_inside_type_definition_rhs: false,
        is_inside_static_type: false,
    };
}

// Not using this yet but probably need to be
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub namespace: NamespaceId,
    pub identifier: Ident,
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
    base_ability_id: AbilityId,
    specialized_ability_id: AbilityId,
    full_impl_id: AbilityImplId,
}

pub struct TypedModuleBuffers {
    name_builder: String,
    emitted_code: String,
    emit_lexer_tokens: Vec<lex::Token>,
}

#[derive(Clone, Copy)]
pub enum CodeEmission {
    String(StringId),
}

pub struct VmExecuteResult {
    pub type_id: TypeId,
    pub static_value_id: StaticValueId,
    pub emits: Vec<CodeEmission>,
}

nz_u32_id!(ModuleId);

pub const MODULE_ID_CORE: ModuleId = ModuleId::ONE;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModuleKind {
    Library,
    Executable,
}

pub enum ModuleRef {
    Github { url: String, branch: String },
    Local { path: PathBuf },
}

pub struct ModuleManifest {
    kind: ModuleKind,
    deps: Vec<ModuleRef>,
}

pub struct Module {
    pub id: ModuleId,
    pub name: Ident,
    pub dependencies: Vec<ModuleId>,
    pub kind: ModuleKind,
    pub namespace_id: NamespaceId,
    pub namespace_scope_id: ScopeId,
}

pub struct TypedProgram {
    pub modules: Pool<Module, ModuleId>,
    pub ast: ParsedProgram,
    functions: Pool<TypedFunction, FunctionId>,
    pub variables: Pool<Variable, VariableId>,
    pub types: Types,
    pub globals: Pool<TypedGlobal, TypedGlobalId>,
    pub exprs: Pool<TypedExpr, TypedExprId>,
    pub stmts: Pool<TypedStmt, TypedStmtId>,
    pub static_values: Pool<StaticValue, StaticValueId>,
    pub type_schemas: FxHashMap<TypeId, StaticValueId>,
    pub type_names: FxHashMap<TypeId, StringId>,
    pub scopes: Scopes,
    pub errors: Vec<TyperError>,
    pub namespaces: Namespaces,
    pub abilities: Pool<TypedAbility, AbilityId>,
    pub ability_impls: Pool<TypedAbilityImpl, AbilityImplId>,
    /// Key is 'self' type
    pub ability_impl_table: FxHashMap<TypeId, Vec<AbilityImplHandle>>,
    /// Key is base ability id
    pub blanket_impls: FxHashMap<AbilityId, EcoVec<AbilityImplId>>,
    pub namespace_ast_mappings: FxHashMap<ParsedNamespaceId, NamespaceId>,
    pub function_ast_mappings: FxHashMap<ParsedFunctionId, FunctionId>,
    pub global_ast_mappings: FxHashMap<ParsedGlobalId, TypedGlobalId>,
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
    module_in_progress: Option<ModuleId>,
    inference_context: InferenceContext,
    type_defn_stack: Vec<TypeId>,

    // Buffers that we prefer to re-use to avoid thousands of allocations
    // Clear them after you use them, but leave the memory allocated
    buffers: TypedModuleBuffers,

    pub named_types: Pool<NameAndType, NameAndTypeId>,
    pub existential_type_params: Pool<ExistentialTypeParam, ExistentialTypeParamId>,
    pub pattern_ctors: Pool<PatternCtor, PatternCtorId>,

    // Can execute code statically; primary VM; gets 'rented out'
    // from the TypedProgram to avoid borrow bullshit
    pub vm: Box<Option<vm::Vm>>,
    // Used to execute static code if it is first encountered
    // while excuting the surrounding code statically
    // It should be run in its own environment; as it should
    // not see any of the values from its environment.
    pub alt_vms: Vec<vm::Vm>,
}

impl TypedProgram {
    pub fn new(program_name: String, config: CompilerConfig) -> TypedProgram {
        let types = Types {
            types: Vec::with_capacity(8192),
            existing_types_mapping: FxHashMap::new(),
            layouts: Pool::with_capacity("type_layouts", 8192),
            type_variable_counts: Pool::with_capacity("type_variable_counts", 8192),
            types_to_defns: FxHashMap::new(),
            existing_defns: FxHashMap::new(),
            defns: Pool::with_capacity("type_defns", 4096),
            ast_type_defn_mapping: FxHashMap::new(),
            ast_ability_mapping: FxHashMap::new(),
            builtins: BuiltinTypes {
                string: None,
                buffer: None,
                types_layout: None,
                types_type_schema: None,
                types_int_kind: None,
                types_int_value: None,
            },
            config: TypesConfig { ptr_size_bits: config.target.word_size().bits() },
        };

        let ast = ParsedProgram::make(program_name, config);
        let root_ident = ast.idents.builtins.root_module_name;
        let mut scopes = Scopes::make(root_ident);
        let mut namespaces = Namespaces { namespaces: Pool::with_capacity("namespaces", 256) };
        let root_namespace = Namespace {
            name: root_ident,
            scope_id: Scopes::ROOT_SCOPE_ID,
            namespace_type: NamespaceType::Root,
            companion_type_id: None,
            parent_id: None,
            owner_module: None,
            parsed_id: ParsedId::Namespace(ParsedNamespaceId::ONE),
        };
        let root_namespace_id = namespaces.add(root_namespace);
        scopes
            .set_scope_owner_id(Scopes::ROOT_SCOPE_ID, ScopeOwnerId::Namespace(root_namespace_id));

        // Add _root ns to the root scope as well so users can use it
        if !scopes.get_scope_mut(Scopes::ROOT_SCOPE_ID).add_namespace(root_ident, root_namespace_id)
        {
            panic!("Root namespace was taken, hmmmm");
        }
        let vm_stack_size = crate::MEGABYTE * 10;
        let vm_static_stack_size = crate::MEGABYTE;
        TypedProgram {
            modules: Pool::with_capacity("modules", 32),
            functions: Pool::with_capacity("typed_functions", 8192),
            variables: Pool::with_capacity("typed_variables", 8192),
            types,
            globals: Pool::with_capacity("typed_globals", 4096),
            exprs: Pool::with_capacity("typed_exprs", 65536),
            stmts: Pool::with_capacity("typed_stmts", 8192),
            // TODO: De-dupe the static values pool a bit: for example unit, ints, btrue and
            //       bfalse, strings are easy due to string id, ...
            static_values: Pool::with_capacity("compile_time_values", 8192),
            type_schemas: FxHashMap::new(),
            type_names: FxHashMap::new(),
            scopes,
            errors: Vec::new(),
            namespaces,
            abilities: Pool::with_capacity("abilities", 2048),
            ability_impls: Pool::with_capacity("ability_impls", 4096),
            ability_impl_table: FxHashMap::new(),
            blanket_impls: FxHashMap::new(),
            namespace_ast_mappings: FxHashMap::with_capacity(512),
            function_ast_mappings: FxHashMap::with_capacity(512),
            global_ast_mappings: FxHashMap::new(),
            ability_impl_ast_mappings: FxHashMap::new(),
            use_statuses: FxHashMap::new(),
            debug_level_stack: vec![log::max_level()],
            functions_pending_body_specialization: vec![],
            ast,
            module_in_progress: None,
            inference_context: InferenceContext {
                origin_stack: Vec::with_capacity(64),
                params: Vec::with_capacity(128),
                solutions_so_far: EcoVec::with_capacity(32),
                inference_vars: Vec::with_capacity(32),
                constraints: Vec::with_capacity(256),
                substitutions: FxHashMap::with_capacity(256),
                substitutions_vec: Vec::with_capacity(256),
            },
            type_defn_stack: Vec::with_capacity(8),
            buffers: TypedModuleBuffers {
                name_builder: String::with_capacity(4096),
                emitted_code: String::with_capacity(8192),
                emit_lexer_tokens: Vec::new(),
            },
            named_types: Pool::with_capacity("named_types", 8192),
            existential_type_params: Pool::with_capacity("function_type_params", 4096),
            pattern_ctors: Pool::with_capacity("pattern_ctors", 8192),
            vm: Box::new(Some(vm::Vm::make(vm_stack_size, vm_static_stack_size))),
            alt_vms: vec![
                vm::Vm::make(vm_stack_size, vm_static_stack_size),
                vm::Vm::make(vm_stack_size, vm_static_stack_size),
            ],
        }
    }

    pub fn add_module(&mut self, src_path: &Path) -> anyhow::Result<ModuleId> {
        log::info!("Loading module {:?}...", src_path);
        let src_path = src_path.canonicalize().map_err(|e| {
            anyhow::anyhow!("Error loading module '{}': {}", src_path.to_string_lossy(), e)
        })?;
        let src_path_name = src_path.file_stem().unwrap().to_string_lossy();
        let module_name = self.ast.idents.intern(&src_path_name);
        if let Some(m) = self.modules.iter().find(|m| m.name == module_name) {
            eprintln!("Module already included: {}", self.ident_str(m.name),);
            return Ok(m.id);
        }
        let is_core = self.modules.is_empty();
        if is_core {
            debug_assert_eq!(self.modules.next_id(), MODULE_ID_CORE);
        }

        let (parent_dir, mut files_to_compile) = crate::compiler::discover_source_files(&src_path);
        if is_core {
            let builtin_index = files_to_compile
                .iter()
                .position(|path| path.file_name().unwrap() == "builtin.k1")
                .unwrap();
            files_to_compile.swap(0, builtin_index);
        };
        let directory_string = parent_dir.to_str().unwrap().to_string();
        let start_parse = std::time::Instant::now();

        let parsed_namespace_id = parse::init_module(module_name, &mut self.ast);
        let mut token_buffer = Vec::with_capacity(16384);
        for path in &files_to_compile {
            let content = std::fs::read_to_string(path)
                .unwrap_or_else(|_| panic!("Failed to open file to parse: {:?}", path));
            let name = path.file_name().unwrap();
            let name_str = name.to_string_lossy();
            log::info!("  Parsing {}", name_str);
            let file_id = self.ast.sources.next_file_id();
            let source = parse::Source::make(
                file_id,
                directory_string.clone(),
                name.to_str().unwrap().to_string(),
                content,
            );
            token_buffer.clear();
            match parse::lex_text(&mut self.ast, source, &mut token_buffer) {
                Err(e) => {
                    self.ast.push_error(e);
                    // Keep going man! to the next file
                    continue;
                }
                Ok(_) => {}
            };
            let mut parser = parse::Parser::make_for_file(
                module_name,
                parsed_namespace_id,
                &mut self.ast,
                &token_buffer,
                file_id,
            );
            parser.parse_file();
        }

        let parsing_elapsed = start_parse.elapsed();
        log::info!("parsing took {}ms", parsing_elapsed.as_millis());

        if !self.ast.errors.is_empty() {
            bail!("Parsing module {} failed", src_path_name);
        }

        let module_manifest = if is_core {
            ModuleManifest { kind: ModuleKind::Library, deps: vec![] }
        } else {
            match self.get_module_manifest(parsed_namespace_id)? {
                None => ModuleManifest { kind: ModuleKind::Executable, deps: vec![] },
                Some(manifest) => manifest,
            }
        };

        if module_manifest.kind == ModuleKind::Executable {
            if let Some(m) = self.modules.iter().find(|m| m.kind == ModuleKind::Executable) {
                bail!(
                    "Cannot compile a program with 2 executable modules. {} and {}",
                    self.ident_str(m.name),
                    self.ident_str(module_name)
                );
            }
        }

        for dep in module_manifest.deps.iter() {
            let src_path = match dep {
                ModuleRef::Github { .. } => todo!(),
                ModuleRef::Local { path } => path,
            };
            // Actually, module names need to be unique identifiers and provided up front
            self.add_module(src_path)?;
        }

        let type_start = Instant::now();
        let module_id = self.run(module_name, parsed_namespace_id, module_manifest.kind)?;
        if is_core {
            debug_assert_eq!(module_id, MODULE_ID_CORE);
        }
        let typing_elapsed = type_start.elapsed();
        //if args.dump_module {
        //    println!("{}", typed_program);
        //}
        log::info!(
            "typing took {}ms (\n\t{} expressions,\n\t{} functions,\n\t{} types\n)",
            typing_elapsed.as_millis(),
            self.exprs.len(),
            self.function_iter().count(),
            self.types.type_count()
        );
        Ok(module_id)
    }

    pub fn push_debug_level(&mut self) {
        let level = log::LevelFilter::Debug;
        self.debug_level_stack.push(level);
        log::set_max_level(level);
        debug!("push max_level is now {}", log::max_level())
    }

    pub fn pop_debug_level(&mut self) {
        self.debug_level_stack.pop();
        log::set_max_level(*self.debug_level_stack.last().unwrap());
        debug!("pop max_level is now {}", log::max_level())
    }

    pub fn function_iter(&self) -> impl Iterator<Item = (FunctionId, &TypedFunction)> {
        self.functions.iter_with_ids()
    }

    pub fn program_name(&self) -> &str {
        &self.ast.name
    }

    pub fn get_string(&self, string_id: StringId) -> &str {
        self.ast.strings.get_string(string_id)
    }

    pub fn name_of_type(&self, type_id: TypeId) -> &str {
        match self.types.get_defn_info(type_id) {
            None => self.types.get(type_id).kind_name(),
            Some(info) => self.ident_str(info.name),
        }
    }

    pub fn ident_str(&self, id: Ident) -> &str {
        self.ast.idents.get_name(id)
    }

    pub fn ident_str_opt(&self, id: Option<Ident>) -> &str {
        match id {
            Some(id) => self.ast.idents.get_name(id),
            None => "<no name>",
        }
    }

    pub fn build_ident_with(&mut self, mut f: impl FnMut(&mut TypedProgram, &mut String)) -> Ident {
        let mut name_buffer = std::mem::take(&mut self.buffers.name_builder);
        f(self, &mut name_buffer);
        let new_name_ident = self.ast.idents.intern(&name_buffer);
        name_buffer.clear();
        self.buffers.name_builder = name_buffer;
        new_name_ident
    }

    pub fn get_namespace_scope(&self, namespace_id: NamespaceId) -> &Scope {
        let scope_id = self.namespaces.get_scope(namespace_id);
        self.scopes.get_scope(scope_id)
    }

    pub fn get_main_function_id(&self) -> Option<FunctionId> {
        if let Some(exec_module) = self.modules.iter().find(|m| m.kind == ModuleKind::Executable) {
            self.scopes
                .get_scope(exec_module.namespace_scope_id)
                .find_function(self.ast.idents.builtins.main)
        } else {
            None
        }
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
            TypedStmt::Require(req) => {
                if req.condition.diverges {
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
            TypedStmt::Require(req) => req.span,
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
        let parsed_type_defn = self.ast.get_type_defn(parsed_type_defn_id);
        let is_generic_defn = !parsed_type_defn.type_params.is_empty();

        if parsed_type_defn.flags.is_alias() {
            if is_generic_defn {
                return failf!(
                    parsed_type_defn.span,
                    "Type alias cannot be generic: {}",
                    self.ident_str(parsed_type_defn.name)
                );
            }
            let parsed_type_defn = self.ast.get_type_defn(parsed_type_defn_id).clone();
            let rhs = self.eval_type_expr(parsed_type_defn.value_expr, scope_id)?;
            if !self.scopes.add_type(scope_id, parsed_type_defn.name, rhs) {
                return failf!(
                    parsed_type_defn.span,
                    "Type name '{}' is taken",
                    self.ident_str(parsed_type_defn.name).blue()
                );
            };
            return Ok(rhs);
        }

        // TODO(perf): clone of ParsedTypeDefn clones a Vec which contains a Vec!
        let parsed_type_defn = self.ast.get_type_defn(parsed_type_defn_id).clone();
        debug_assert!(!parsed_type_defn.flags.is_alias());

        // TODO: ident lookup
        if self.ident_str(parsed_type_defn.name) == "some" {
            return failf!(parsed_type_defn.span, "'some' is not a valid type name");
        }
        let my_type_id = self.types.find_type_defn_mapping(parsed_type_defn_id).unwrap();
        self.type_defn_stack.push(my_type_id);

        if self.types.get(my_type_id).as_unresolved().is_none() {
            return Ok(my_type_id);
        }

        let is_generic_defn = !parsed_type_defn.type_params.is_empty();

        let defn_scope_id = self.scopes.add_child_scope(
            scope_id,
            ScopeType::TypeDefn,
            None,
            Some(parsed_type_defn.name),
        );
        let mut type_params: SV4<NameAndType> =
            SmallVec::with_capacity(parsed_type_defn.type_params.len());
        for type_param in parsed_type_defn.type_params.iter() {
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: type_param.name,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                },
                smallvec![],
            );
            type_params.push(NameAndType { name: type_param.name, type_id: type_variable_id });
            let added = self
                .scopes
                .get_scope_mut(defn_scope_id)
                .add_type(type_param.name, type_variable_id);
            if !added {
                return failf!(
                    type_param.span,
                    "Type variable name '{}' is taken",
                    self.ident_str(type_param.name).blue()
                );
            }
        }
        let type_params_handle = self.named_types.add_slice_from_copy_slice(&type_params);

        let type_eval_context = EvalTypeExprContext {
            // For generics, we want the RHS to be its own type, then we make a Generic wrapper
            // that points to it, so we pass None for `direct_unresolved_target_type`
            direct_unresolved_target_type: if is_generic_defn { None } else { Some(my_type_id) },
            is_inside_type_definition_rhs: true,
            is_direct_function_parameter: false,
            is_direct_variable_binding: false,
            is_inside_static_type: false,
        };

        // Actually compile the RHS
        let rhs_type_id =
            self.eval_type_expr_ext(parsed_type_defn.value_expr, defn_scope_id, type_eval_context)?;

        // Not a generic and not an alias, so its a "New Type"
        // A named struct or enum or a builtin
        match self.types.get(rhs_type_id) {
            Type::Unit
            | Type::Char
            | Type::Bool
            | Type::Never
            | Type::Pointer
            | Type::Integer(_)
            | Type::Float(_) => {}
            Type::Struct(_s) => {}
            Type::Enum(_e) => {}
            _other => {
                return failf!(
                    parsed_type_defn.span,
                    "Non-alias type definition must be a struct or enum or builtin; perhaps you meant to create an alias `deftype alias <name> = <type>`"
                );
            }
        };

        // Recursive generic types do not work.
        // The recursive reference needs to point to the generic type, which it does
        // look like we're doing.
        //
        // deftype Tree[T] = { info: int, child: Tree[T]* }
        // ->  let* t1: Tree[unit]* = { info: 0, child: core/Pointer/NULL as Tree[unit]* };
        // ->                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //       Field child has incorrect type: Expected Tree but got { info: iword, child: Tree* }
        // Interestingly, Expected Tree means expected RecursiveReference to Tree
        // I think the problem then is in instantiate, we need to update the reference
        // to point to an instantiated type, not to the generic anymore!
        //
        // We aren't even properly running the TypeApplication expr at all
        // if its recursive, we just point at the generic, which is very wrong
        let final_type_id = if is_generic_defn {
            let gen_type = GenericType {
                params: type_params_handle,
                inner: rhs_type_id,
                specializations: FxHashMap::with_capacity(16),
            };
            self.types.resolve_unresolved(my_type_id, Type::Generic(gen_type));
            my_type_id
        } else {
            my_type_id
        };

        let popped_type = self.type_defn_stack.pop();
        debug_assert_eq!(popped_type, Some(my_type_id));
        Ok(final_type_id)
    }

    fn eval_type_expr(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        self.eval_type_expr_ext(type_expr_id, scope_id, EvalTypeExprContext::EMPTY)
    }

    fn eval_type_expr_ext(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
        // The context is mostly about 'where are are' - where this type expression is located -
        // with respect to the source language. Is this a type definition? A function parameter
        // type? etc.
        context: EvalTypeExprContext,
    ) -> TyperResult<TypeId> {
        let base = match self.ast.type_exprs.get(type_expr_id) {
            ParsedTypeExpr::Builtin(span) => {
                let defn_type_id =
                    context.direct_unresolved_target_type.expect("required defn info for builtin");
                let type_value = match defn_type_id {
                    UNIT_TYPE_ID => Type::Unit,
                    CHAR_TYPE_ID => Type::Char,
                    BOOL_TYPE_ID => Type::Bool,
                    NEVER_TYPE_ID => Type::Never,
                    POINTER_TYPE_ID => Type::Pointer,
                    F32_TYPE_ID => Type::Float(FloatType::F32),
                    F64_TYPE_ID => Type::Float(FloatType::F64),
                    U8_TYPE_ID => Type::Integer(IntegerType::U8),
                    U16_TYPE_ID => Type::Integer(IntegerType::U16),
                    U32_TYPE_ID => Type::Integer(IntegerType::U32),
                    U64_TYPE_ID => Type::Integer(IntegerType::U64),
                    UWORD_TYPE_ID => {
                        let word_size = self.ast.config.target.word_size();
                        Type::Integer(IntegerType::UWord(word_size))
                    }
                    I8_TYPE_ID => Type::Integer(IntegerType::I8),
                    I16_TYPE_ID => Type::Integer(IntegerType::I16),
                    I32_TYPE_ID => Type::Integer(IntegerType::I32),
                    I64_TYPE_ID => Type::Integer(IntegerType::I64),
                    IWORD_TYPE_ID => {
                        let word_size = self.ast.config.target.word_size();
                        Type::Integer(IntegerType::IWord(word_size))
                    }
                    _ => return failf!(*span, "Unknown builtin type id '{}'", defn_type_id),
                };
                self.types.resolve_unresolved(defn_type_id, type_value);
                Ok(defn_type_id)
            }
            ParsedTypeExpr::Struct(struct_defn) => {
                let struct_defn = struct_defn.clone();
                let mut fields: EcoVec<StructTypeField> =
                    EcoVec::with_capacity(struct_defn.fields.len());
                for ast_field in struct_defn.fields.iter() {
                    if let Some(existing_field) = fields.iter().find(|f| f.name == ast_field.name) {
                        return failf!(
                            struct_defn.span,
                            "Duplicate field name '{}' in struct definition",
                            self.ident_str(existing_field.name)
                        );
                    }
                    let ty =
                        self.eval_type_expr_ext(ast_field.ty, scope_id, context.descended())?;
                    // TODO(infinite recursive types): This is kinda how we'd prevent this
                    // Need to just put in the context "are we indirected or not"
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
                        private: ast_field.private,
                    })
                }

                let defn_id = context
                    .direct_unresolved_target_type
                    .and_then(|t| self.types.types_to_defns.get(&t))
                    .copied();
                let struct_defn =
                    Type::Struct(StructType { fields, defn_id, generic_instance_info: None });
                let type_id =
                    self.add_or_resolve_type(context.direct_unresolved_target_type, struct_defn);
                Ok(type_id)
            }
            ParsedTypeExpr::TypeApplication(_ty_app) => {
                let type_op_result =
                    self.detect_and_eval_type_operator(type_expr_id, scope_id, context)?;
                match type_op_result {
                    None => self.eval_type_application(type_expr_id, scope_id, context),
                    Some(type_op_result) => Ok(type_op_result),
                }
            }
            ParsedTypeExpr::Optional(opt) => {
                let inner_ty = self.eval_type_expr_ext(opt.base, scope_id, context.descended())?;
                let optional_type =
                    self.instantiate_generic_type(OPTIONAL_TYPE_ID, smallvec![inner_ty]);
                Ok(optional_type)
            }
            ParsedTypeExpr::Reference(r) => {
                let inner_ty = self.eval_type_expr_ext(r.base, scope_id, context.descended())?;
                let reference_type = Type::Reference(ReferenceType { inner_type: inner_ty });
                let type_id = self.types.add_anon(reference_type);
                Ok(type_id)
            }
            ParsedTypeExpr::Array(arr) => {
                let arr = *arr; // Copy to avoid borrow checker issues
                let element_type =
                    self.eval_type_expr_ext(arr.element_type, scope_id, context.descended())?;

                // Evaluate the size expression at compile time with uword type hint
                let size_expr_ctx = EvalExprContext {
                    scope_id,
                    expected_type_id: Some(UWORD_TYPE_ID),
                    is_inference: false,
                    static_ctx: None,
                    global_defn_name: None,
                    is_generic_pass: false,
                };
                let size_expr_result = self.eval_expr(arr.size_expr, size_expr_ctx)?;

                // Try to lift the expression to a static value
                let static_size_expr = self.attempt_static_lift(size_expr_result).map_err(|e| {
                    errf!(
                        arr.span,
                        "The value for the array size must be knowable at compile time: {e}",
                    )
                })?;

                // Extract the static value from the lifted expression
                let static_value_id = match self.exprs.get(static_size_expr) {
                    TypedExpr::StaticValue(static_id, _, _) => *static_id,
                    _ => {
                        self.ice_with_span("static lift did not produce StaticValue", arr.span);
                    }
                };
                let static_value = self.static_values.get(static_value_id);

                let size = match static_value {
                    StaticValue::Int(TypedIntValue::UWord64(u64)) => *u64,
                    StaticValue::Int(TypedIntValue::UWord32(u32)) => *u32 as u64,
                    _ => {
                        return failf!(
                            arr.span,
                            "Array size must be a uword; got {}",
                            self.type_id_to_string(static_value.get_type())
                        );
                    }
                };

                let array_type = Type::Array(ArrayType { element_type, size });
                let type_id = self.types.add_anon(array_type);
                Ok(type_id)
            }
            ParsedTypeExpr::Enum(e) => {
                let e = e.clone();
                let variant_count = e.variants.len();

                let tag_type = match e.tag_type {
                    None => {
                        const U8_MAX_VARIANTS: usize = u8::MAX as usize + 1;
                        const MAX_VARIANTS: usize = u16::MAX as usize + 1;
                        let min_viable = match variant_count {
                            c if c <= U8_MAX_VARIANTS => U8_TYPE_ID,
                            c if c <= MAX_VARIANTS => U16_TYPE_ID,
                            _ => {
                                return failf!(
                                    e.span,
                                    "Enum cannot have more than {MAX_VARIANTS} variants"
                                );
                            }
                        };
                        min_viable
                    }
                    Some(tag_type_expr) => {
                        let tag_type = self.eval_type_expr(tag_type_expr, scope_id)?;
                        match tag_type {
                            U8_TYPE_ID | U16_TYPE_ID | U32_TYPE_ID | U64_TYPE_ID => {}
                            _ => {
                                return failf!(
                                    self.ast.get_type_expr_span(tag_type_expr),
                                    "Unsupported tag type"
                                );
                            }
                        };
                        tag_type
                    }
                };

                let mut variants = EcoVec::with_capacity(variant_count);
                for (index, v) in e.variants.iter().enumerate() {
                    let payload_type_id = match &v.payload_expression {
                        None => None,
                        Some(payload_type_expr) => {
                            let type_id = self.eval_type_expr_ext(
                                *payload_type_expr,
                                scope_id,
                                context.descended(),
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
                    let tag_value = match tag_type {
                        U8_TYPE_ID => TypedIntValue::U8(index as u8),
                        U16_TYPE_ID => TypedIntValue::U16(index as u16),
                        U32_TYPE_ID => TypedIntValue::U32(index as u32),
                        U64_TYPE_ID => TypedIntValue::U64(index as u64),
                        _ => unreachable!(),
                    };
                    let variant = TypedEnumVariant {
                        enum_type_id: TypeId::PENDING,
                        my_type_id: TypeId::PENDING,
                        name: v.tag_name,
                        index: index as u32,
                        payload: payload_type_id,
                        tag_value,
                    };
                    variants.push(variant);
                }
                let defn_id = context
                    .direct_unresolved_target_type
                    .and_then(|t| self.types.types_to_defns.get(&t))
                    .copied();
                let enum_type = Type::Enum(TypedEnum {
                    variants,
                    defn_id,
                    generic_instance_info: None,
                    ast_node: type_expr_id.into(),
                    tag_type,
                });
                let enum_type_id =
                    self.add_or_resolve_type(context.direct_unresolved_target_type, enum_type);
                Ok(enum_type_id)
            }
            ParsedTypeExpr::DotMemberAccess(dot_acc) => {
                let dot_acc = dot_acc.clone();
                let base_type =
                    self.eval_type_expr_ext(dot_acc.base, scope_id, context.descended())?;
                if let Some(spec_info) = self.types.get_generic_instance_info(base_type) {
                    let generic = self.types.get(spec_info.generic_parent).expect_generic();
                    if let Some(matching_type_var_pos) = self
                        .named_types
                        .get_slice(generic.params)
                        .iter()
                        .position(|tp| tp.name == dot_acc.member_name)
                    {
                        let actual_type = &spec_info.type_args[matching_type_var_pos];
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
                                self.ident_str(dot_acc.member_name),
                                self.type_id_to_string(base_type)
                            );
                        };
                        let variant_type = matching_variant.my_type_id;
                        Ok(variant_type)
                    }
                    Type::EnumVariant(ev) => {
                        if self.ast.idents.get_name(dot_acc.member_name) != "value" {
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
                                self.ident_str(dot_acc.member_name),
                                self.type_id_to_string(base_type)
                            );
                        };
                        Ok(field.1.type_id)
                    }
                    // You can do dot access on References to get out their 'value' types
                    Type::Reference(r) => {
                        if self.ast.idents.get_name(dot_acc.member_name) != "value" {
                            return make_fail_ast_id(
                                &self.ast,
                                "Invalid member access on Optional type; try '.value'",
                                type_expr_id.into(),
                            );
                        }
                        Ok(r.inner_type)
                    }
                    Type::Function(fun) => {
                        let member_name = self.ast.idents.get_name(dot_acc.member_name);
                        match member_name {
                            "return" => Ok(fun.return_type),
                            _other => {
                                if let Some(param) = fun
                                    .logical_params()
                                    .iter()
                                    .find(|p| p.name == dot_acc.member_name)
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
                    Type::Array(array_type) => {
                        let member_name = self.ast.idents.get_name(dot_acc.member_name);
                        match member_name {
                            "element" => Ok(array_type.element_type),
                            _ => {
                                return failf!(
                                    dot_acc.span,
                                    "Array type has no member named {}; try '.element'",
                                    member_name
                                );
                            }
                        }
                    }
                    _ => {
                        return failf!(
                            dot_acc.span,
                            "Invalid type for '.' access: {}",
                            self.type_id_to_string(base_type)
                        );
                    }
                }
            }
            ParsedTypeExpr::Function(fun_type) => {
                let fun_type = fun_type.clone();
                let mut params: EcoVec<FnParamType> = EcoVec::with_capacity(fun_type.params.len());

                for (index, param) in fun_type.params.iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;
                    let span = self.ast.get_type_expr_span(*param);

                    let name = match index {
                        0 => self.ast.idents.builtins.param_0,
                        1 => self.ast.idents.builtins.param_1,
                        2 => self.ast.idents.builtins.param_2,
                        3 => self.ast.idents.builtins.param_3,
                        4 => self.ast.idents.builtins.param_4,
                        5 => self.ast.idents.builtins.param_5,
                        6 => self.ast.idents.builtins.param_6,
                        7 => self.ast.idents.builtins.param_7,
                        8 => self.ast.idents.builtins.param_8,
                        i => format_ident!(self, "param_{}", i),
                    };
                    params.push(FnParamType {
                        type_id,
                        name,
                        is_context: false,
                        is_lambda_env: false,
                        span,
                    });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                let function_type_id = self.types.add_anon(Type::Function(FunctionType {
                    physical_params: params,
                    return_type,
                }));
                Ok(function_type_id)
            }
            ParsedTypeExpr::TypeOf(tof) => {
                let expr = self.eval_expr(tof.target_expr, EvalExprContext::make(scope_id))?;
                let ty = self.exprs.get(expr);
                Ok(ty.get_type())
            }
            ParsedTypeExpr::TypeFromId(type_from_id) => {
                let type_from_id = *type_from_id;
                let id_expr_id = self.eval_expr(
                    type_from_id.id_expr,
                    EvalExprContext::make(scope_id).with_expected_type(Some(U64_TYPE_ID)),
                )?;
                let TypedExpr::Integer(typed_int) = self.exprs.get(id_expr_id) else {
                    self.ice_with_span(
                        "Expected an integer expression for TypeFromId",
                        type_from_id.span,
                    );
                };
                let TypedIntValue::U64(u64_value) = typed_int.value else {
                    self.ice_with_span("Expected a u64 value for TypeFromId", type_from_id.span);
                };
                let Some(type_id) = TypeId::from_u32(u64_value as u32) else {
                    return failf!(type_from_id.span, "Type ID {} is out of bounds", u64_value);
                };
                Ok(type_id)
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                if !context.is_direct_function_parameter {
                    return failf!(
                        quant.span,
                        "some quantifier is only allowed in function parameters"
                    );
                }
                let span = quant.span;
                let inner = self.eval_type_expr(quant.inner_type_expr, scope_id)?;
                let Type::Function(_function_type) = self.types.get(inner) else {
                    return failf!(span, "Expected a function type following 'some'");
                };
                let name = self.ast.idents.intern(format!("some_fn_{}", type_expr_id));
                let function_type_variable =
                    self.add_function_type_parameter(FunctionTypeParameter {
                        name,
                        scope_id,
                        span,
                        function_type: inner,
                    });
                Ok(function_type_variable)
            }
            ParsedTypeExpr::Static(parsed_static) => {
                if context.is_inside_static_type {
                    return failf!(
                        parsed_static.span,
                        "Static type cannot appear inside static type"
                    );
                }
                if context.is_inside_type_definition_rhs {
                    return failf!(
                        parsed_static.span,
                        "Static type cannot appear in type definitions"
                    );
                }
                let parsed_static = *parsed_static;
                let inner_type_id = self.eval_type_expr_ext(
                    parsed_static.inner_type_expr,
                    scope_id,
                    EvalTypeExprContext { is_inside_static_type: true, ..context },
                )?;
                if let Type::Static(_) = self.types.get_no_follow_static(inner_type_id) {
                    return failf!(
                        parsed_static.span,
                        "Static type cannot be nested inside another static type"
                    );
                };
                let static_type = StaticType { inner_type_id, value_id: None };
                let static_type_id = self.types.add_anon(Type::Static(static_type));
                Ok(static_type_id)
            }
        }?;
        Ok(base)
    }

    fn add_or_resolve_type(&mut self, defn_type_id: Option<TypeId>, type_value: Type) -> TypeId {
        match defn_type_id {
            None => self.types.add_type(type_value),
            Some(t) => {
                self.types.resolve_unresolved(t, type_value);
                t
            }
        }
    }

    pub fn target_word_size(&self) -> WordSize {
        self.ast.config.target.word_size()
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
        match self.ident_str(ty_app.name.name) {
            "dyn" => {
                if ty_app.args.len() != 1 {
                    return failf!(ty_app.span, "Expected 1 type parameter for dyn");
                }
                let fn_type_expr_id = self.ast.p_type_args.get_nth(ty_app.args, 0).type_expr;
                let inner =
                    self.eval_type_expr_ext(fn_type_expr_id, scope_id, context.descended())?;
                let inner_span = self.ast.get_type_expr_span(fn_type_expr_id);
                if self.types.get(inner).as_function().is_none() {
                    return failf!(
                        ty_app.span,
                        "Expected function type, or eventually, ability name, for dyn"
                    );
                }
                let new_function_type = self.add_lambda_env_to_function_type(inner, inner_span);
                let lambda_object_type = self.types.add_lambda_object(
                    &self.ast.idents,
                    new_function_type,
                    ty_app_id.into(),
                );

                Ok(Some(lambda_object_type))
            }
            "_struct_combine" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_combine");
                }
                let args = self.ast.p_type_args.get_slice(ty_app.args);
                let arg1_expr = args[0].type_expr;
                let arg2_expr = args[1].type_expr;
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

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
                    EcoVec::with_capacity(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(struct1.fields.clone());
                for field in struct2.fields.iter() {
                    let collision = combined_fields.iter().find(|f| f.name == field.name);
                    if let Some(collision) = collision {
                        if collision.type_id != field.type_id {
                            return failf!(
                                ty_app.span,
                                "Field '{}' has conflicting types in the two structs",
                                self.ident_str(field.name).blue()
                            );
                        }
                    }
                    combined_fields.push(*field);
                }

                let defn_id = context
                    .direct_unresolved_target_type
                    .and_then(|t| self.types.types_to_defns.get(&t))
                    .copied();
                let new_struct = Type::Struct(StructType {
                    fields: combined_fields,
                    defn_id,
                    generic_instance_info: None,
                });
                let type_id =
                    self.add_or_resolve_type(context.direct_unresolved_target_type, new_struct);

                Ok(Some(type_id))
            }
            "_struct_remove" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_remove");
                }
                let args = self.ast.p_type_args.get_slice(ty_app.args);
                let arg1_expr = args[0].type_expr;
                let arg2_expr = args[1].type_expr;
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

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

                let defn_id = context
                    .direct_unresolved_target_type
                    .and_then(|t| self.types.types_to_defns.get(&t))
                    .copied();
                let new_struct = Type::Struct(StructType {
                    fields: new_fields,
                    defn_id,
                    generic_instance_info: None,
                });
                let type_id =
                    self.add_or_resolve_type(context.direct_unresolved_target_type, new_struct);
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
        match self.scopes.find_type_namespaced(
            scope_id,
            ty_app_name,
            &self.namespaces,
            &self.ast.idents,
        )? {
            Some((type_id, _)) => {
                match self.types.get(type_id) {
                    Type::Unresolved(parsed_type_defn_id) => {
                        // We recurse into unresolved types, so that we can detect corecursion,
                        // but only if we're doing a type definition
                        if self.type_defn_stack.contains(&type_id) {
                            let recursive_reference =
                                self.types.add_anon(Type::RecursiveReference(RecursiveReference {
                                    root_type_id: type_id,
                                    // TODO: Support generic recursive types
                                    // by storing this
                                    type_args: smallvec![],
                                }));
                            Ok(recursive_reference)
                        } else {
                            self.eval_type_defn(*parsed_type_defn_id, scope_id)
                        }
                    }
                    Type::Generic(g) => {
                        if ty_app.args.len() != g.params.len() {
                            return failf!(
                                ty_app.span,
                                "Type {} expects {} type arguments, got {}",
                                self.namespaced_identifier_to_string(&ty_app.name),
                                g.params.len(),
                                ty_app.args.len()
                            );
                        }
                        let mut type_arguments: SV4<TypeId> =
                            SmallVec::with_capacity(ty_app.args.len());
                        for parsed_param in self.ast.p_type_args.copy_slice_sv::<8>(ty_app.args) {
                            let param_type_id = self.eval_type_expr_ext(
                                parsed_param.type_expr,
                                scope_id,
                                context.descended(),
                            )?;
                            type_arguments.push(param_type_id);
                        }
                        Ok(self.instantiate_generic_type(type_id, type_arguments))
                    }
                    _other => Ok(self.get_type_id_resolved(type_id, scope_id)),
                }
            }
            None => {
                match self.scopes.find_function_namespaced(
                    scope_id,
                    ty_app_name,
                    &self.namespaces,
                    &self.ast.idents,
                )? {
                    Some(function_id) => Ok(self.get_function(function_id).type_id),
                    None => {
                        failf!(
                            ty_app.span,
                            "Type '{}' not found",
                            self.namespaced_identifier_to_string(ty_app_name),
                        )
                    }
                }
            }
        }
    }

    fn instantiate_generic_type(
        &mut self,
        generic_type: TypeId,
        type_arguments: SV4<TypeId>,
    ) -> TypeId {
        let gen_type = self.types.get(generic_type).expect_generic();
        match gen_type.specializations.get(&type_arguments) {
            Some(existing) => {
                debug!(
                    "Using cached generic instance {} for {} args {:?}",
                    self.type_id_to_string(*existing),
                    self.name_of_type(generic_type),
                    type_arguments
                        .clone()
                        .iter()
                        .map(|p| self.type_id_to_string_ext(*p, false))
                        .collect::<Vec<_>>(),
                );
                *existing
            }
            None => {
                debug_assert!(gen_type.params.len() == type_arguments.len());
                let defn_id = self.types.get_defn_id(generic_type).unwrap();
                // Note: This is where we'd check constraints on the pairs:
                // that each passed params meets the constraints of the generic param
                let substitution_pairs: SV8<TypeSubstitutionPair> = self
                    .named_types
                    .get_slice(gen_type.params)
                    .iter()
                    .zip(&type_arguments)
                    .map(|(type_param, passed_type_arg)| TypeSubstitutionPair {
                        from: type_param.type_id,
                        to: *passed_type_arg,
                    })
                    .collect();
                let inner = gen_type.inner;

                debug!("Instantiating Generic rhs {}", self.type_id_to_string_ext(inner, true));
                let specialized_type = self.substitute_in_type_ext(
                    inner,
                    &substitution_pairs,
                    Some(generic_type),
                    Some(defn_id),
                );
                //For working on recursive generics
                //if specialized_type == inner {
                //    panic!("Instantiate should never no-op")
                //}
                if log::log_enabled!(log::Level::Debug) {
                    let inst_info =
                        &self.types.get_generic_instance_info(specialized_type).unwrap().type_args;
                    eprintln!(
                        "instantiated {} with params {} got expanded type: {}",
                        self.ident_str(self.types.defns.get(defn_id).name),
                        self.pretty_print_types(inst_info, ", "),
                        self.type_id_to_string_ext(specialized_type, true)
                    );
                }
                if let Type::Generic(gen_type) = self.types.get_mut(generic_type) {
                    gen_type.specializations.insert(type_arguments, specialized_type);
                };
                specialized_type
            }
        }
    }

    fn substitute_in_type(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
    ) -> TypeId {
        self.substitute_in_type_ext(type_id, substitution_pairs, None, None)
    }

    fn substitute_in_type_ext(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        generic_parent_to_attach: Option<TypeId>,
        defn_info_to_attach: Option<TypeDefnId>,
        // TODO: full support
        // breadcrumbs: &mut Vec<TypeId>,
    ) -> TypeId {
        // TODO: New strategy to support co-recursive types aka recursive families
        // 1. Remember all visited type ids in this substitute (re-use a buf of TypeId)
        // 2. When you encounter a RecursiveReference, check if its in the visited set
        // 3. If it is, no-op
        // 4. If it is not, visit it
        // That should be all we need, don't even need to know the whole set in the family,
        // we get that for 'free' by 'tagging' with RR everywhere vs just inferring from structure
        //
        // We have to work this way everywhere that we visit types. So
        // - substitute_in_type
        // - count_type_variables
        // - Layout (?) although due to indirection requirements, less critical
        // - Other places I am sure
        let all_holes = substitution_pairs
            .iter()
            .all(|p| self.types.type_variable_counts.get(p.from).inference_variable_count > 0);
        if all_holes {
            let no_holes =
                self.types.type_variable_counts.get(type_id).inference_variable_count == 0;
            // Optimization: if every 'from' type is an inference hole, and the type
            // contains no inference holes, which we compute on creation, its a no-op
            // This prevents useless deep type traversals
            if all_holes && no_holes {
                debug!(
                    "detected inference substitution noop for {} {}",
                    self.pretty_print_type_substitutions(substitution_pairs, ", "),
                    self.type_id_to_string(type_id)
                );
                return type_id;
            }
        }
        let all_params = substitution_pairs
            .iter()
            .all(|p| self.types.type_variable_counts.get(p.from).type_parameter_count > 0);
        if all_params {
            let no_params = self.types.type_variable_counts.get(type_id).type_parameter_count == 0;
            // Optimization: if every 'from' type is a type param, and the type
            // contains no type params, which we compute on creation, its a no-op
            // This prevents useless deep type traversals
            if all_params && no_params {
                debug!(
                    "detected type parameter substitution noop for {} {}",
                    self.pretty_print_type_substitutions(substitution_pairs, ", "),
                    self.type_id_to_string(type_id)
                );
                return type_id;
            }
        }

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
            let new_parameter_values: SV4<TypeId> = spec_info
                .type_args
                .clone()
                .iter()
                .map(|prev_type_id| self.substitute_in_type(*prev_type_id, substitution_pairs))
                .collect();
            return self.instantiate_generic_type(generic_parent, new_parameter_values);
        };

        let matching_subst_pair = substitution_pairs.iter().find(|pair| pair.from == type_id);
        if let Some(matching_pair) = matching_subst_pair {
            return matching_pair.to;
        }

        let res = match self.types.get_no_follow(type_id) {
            Type::InferenceHole(_) => type_id,
            Type::Unit
            | Type::Char
            | Type::Integer(_)
            | Type::Float(_)
            | Type::Bool
            | Type::Pointer
            | Type::Never => type_id,
            Type::Struct(struc) => {
                let mut new_fields = struc.fields.clone();
                let mut any_change = false;
                let original_defn_info = struc.defn_id;
                let defn_id_to_use = defn_info_to_attach.or(original_defn_info);
                let original_instance_info = struc.generic_instance_info.clone();
                for field in new_fields.make_mut().iter_mut() {
                    let new_field_type_id =
                        self.substitute_in_type(field.type_id, substitution_pairs);
                    if new_field_type_id != field.type_id {
                        any_change = true;
                    }
                    field.type_id = new_field_type_id;
                }
                if any_change {
                    let generic_instance_info = generic_parent_to_attach
                        .map(|parent| GenericInstanceInfo {
                            generic_parent: parent,
                            type_args: substitution_pairs.iter().map(|p| p.to).collect(),
                        })
                        .or(original_instance_info);

                    let specialized_struct = StructType {
                        fields: new_fields,
                        defn_id: defn_id_to_use,
                        generic_instance_info,
                    };
                    self.types.add(Type::Struct(specialized_struct), defn_id_to_use)
                } else {
                    type_id
                }
            }
            Type::Enum(e) => {
                let mut new_variants = e.variants.clone();
                let mut any_changed = false;
                let original_ast_node = e.ast_node;
                let original_defn_info = self.types.get_defn_id(type_id);
                let defn_id_to_use = defn_info_to_attach.or(original_defn_info);
                let original_instance_info = e.generic_instance_info.clone();
                let original_explicit_tag_type = e.tag_type;
                for variant in new_variants.make_mut().iter_mut() {
                    let new_payload_id = variant.payload.map(|payload_type_id| {
                        self.substitute_in_type(payload_type_id, substitution_pairs)
                    });
                    if new_payload_id != variant.payload {
                        any_changed = true;
                        variant.payload = new_payload_id;
                    }
                }
                if any_changed {
                    let generic_instance_info = generic_parent_to_attach
                        .map(|parent| GenericInstanceInfo {
                            generic_parent: parent,
                            type_args: substitution_pairs.iter().map(|p| p.to).collect(),
                        })
                        .or(original_instance_info);
                    let new_enum = TypedEnum {
                        variants: new_variants,
                        defn_id: defn_id_to_use,
                        ast_node: original_ast_node,
                        generic_instance_info,
                        tag_type: original_explicit_tag_type,
                    };
                    let new_enum_id = self.types.add(Type::Enum(new_enum), defn_id_to_use);
                    new_enum_id
                } else {
                    type_id
                }
            }
            Type::Reference(reference) => {
                let ref_inner = reference.inner_type;
                let new_inner = self.substitute_in_type(ref_inner, substitution_pairs);
                if new_inner != ref_inner {
                    let specialized_reference = ReferenceType { inner_type: new_inner };
                    self.types.add_anon(Type::Reference(specialized_reference))
                } else {
                    type_id
                }
            }
            Type::TypeParameter(_type_param) => type_id,
            Type::FunctionTypeParameter(ftp) => {
                let function_type_id = ftp.function_type;
                let new_fn_type = self.substitute_in_type(function_type_id, substitution_pairs);
                if new_fn_type != function_type_id {
                    let type_param = self.types.get(type_id).as_function_type_parameter().unwrap();
                    self.types.add_anon(Type::FunctionTypeParameter(FunctionTypeParameter {
                        name: type_param.name,
                        scope_id: type_param.scope_id,
                        span: type_param.span,
                        function_type: new_fn_type,
                    }))
                } else {
                    type_id
                }
            }
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
                    self.substitute_in_type(fun_type.return_type, substitution_pairs);
                if new_return_type != new_fun_type.return_type {
                    any_new = true
                };
                new_fun_type.return_type = new_return_type;
                for param in new_fun_type.physical_params.make_mut().iter_mut() {
                    let new_param_type = self.substitute_in_type(param.type_id, substitution_pairs);
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    param.type_id = new_param_type;
                }
                if any_new {
                    let new_function_type_id = self.types.add_anon(Type::Function(new_fun_type));
                    new_function_type_id
                } else {
                    type_id
                }
            }
            Type::Lambda(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Lambda")
            }
            Type::LambdaObject(lam_obj) => {
                let fn_type = lam_obj.function_type;
                let parsed_id = lam_obj.parsed_id;
                let new_fn_type = self.substitute_in_type(fn_type, substitution_pairs);
                if new_fn_type != fn_type {
                    self.types.add_lambda_object(&self.ast.idents, new_fn_type, parsed_id)
                } else {
                    type_id
                }
            }
            Type::Static(_stat) => {
                unreachable!(
                    "substitute_in_type for static; generic statics (`static T`) should be impossible"
                )
            }
            Type::Unresolved(_) => {
                unreachable!("substitute_in_type is not expected to be called on Unresolved")
            }
            Type::RecursiveReference(_rr) => unreachable!(
                "substitute_in_type is not expected to be called on RecursiveReference"
            ),
            Type::Array(arr) => {
                let element_type = arr.element_type;
                let array_size = arr.size;
                let new_element_type = self.substitute_in_type(element_type, substitution_pairs);
                if new_element_type == element_type {
                    type_id // No change needed
                } else {
                    // Create new Array type with substituted element type
                    let new_array_type =
                        Type::Array(ArrayType { element_type: new_element_type, size: array_size });
                    self.types.add_anon(new_array_type)
                }
            }
        };
        debug!(
            "substitute in type on {}.\npairs: {}.\nGot: {}",
            self.type_id_to_string(type_id),
            self.pretty_print_type_substitutions(substitution_pairs, ", "),
            self.dump_type_id_to_string(res)
        );
        res
    }

    fn get_module_manifest(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<Option<ModuleManifest>> {
        let namespace = self.ast.namespaces.get(parsed_namespace_id);
        let Some(manifest_function) = namespace
            .definitions
            .iter()
            .filter_map(|defn| defn.as_global_id())
            .find(|id| self.ast.get_global(*id).name == self.ast.idents.builtins.MODULE_INFO)
        else {
            return Ok(None);
        };
        let manifest_global = self.ast.get_global(manifest_function).clone();
        let type_id = self.eval_type_expr(manifest_global.ty, Scopes::ROOT_SCOPE_ID)?;
        let manifest_result = self.execute_static_expr(
            manifest_global.value_expr,
            EvalExprContext {
                scope_id: Scopes::ROOT_SCOPE_ID,
                expected_type_id: Some(type_id),
                is_inference: false,
                static_ctx: Some(StaticExecContext {
                    is_metaprogram: false,
                    expected_return_type: None,
                }),
                global_defn_name: None,
                is_generic_pass: false,
            },
            false,
            &[],
        )?;

        let StaticValue::Struct(value) = self.static_values.get(manifest_result.static_value_id)
        else {
            panic!("Expected module manifest to be a struct")
        };
        let kind = self.static_values.get(value.fields[0]).as_enum().unwrap();
        let kind = match kind.variant_index {
            0 => ModuleKind::Library,
            1 => ModuleKind::Executable,
            _ => panic!(),
        };
        let deps = vec![];

        Ok(Some(ModuleManifest { kind, deps }))
    }

    fn eval_pattern(
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
                        Type::Unit => Ok(TypedPattern::LiteralUnit(*span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type unit will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    Literal::Char(c, span) => match self.types.get(target_type_id) {
                        Type::Char => Ok(TypedPattern::LiteralChar(*c, *span)),
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
                            EvalExprContext::make(scope_id)
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
                                unreachable!(
                                    "eval_numeric_value should produce only Integer and Float exprs"
                                )
                            }
                        }
                    }
                    Literal::Bool(b, span) => match self.types.get(target_type_id) {
                        Type::Bool => Ok(TypedPattern::LiteralBool(*b, *span)),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "bool literal pattern will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    Literal::String(string_id, span) => match target_type_id {
                        STRING_TYPE_ID => Ok(TypedPattern::LiteralString(*string_id, *span)),
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
                if let Some(name) = enum_pattern.enum_name {
                    match self.scopes.find_type(scope_id, name) {
                        None => {
                            return failf!(
                                enum_pattern.span,
                                "No type named '{}'",
                                self.ident_str(name).blue()
                            );
                        }
                        Some((named_type, _)) => {
                            // Consider generics: 'Opt.Some' applies to all Opt[T]s, so we consider
                            // the 'base' type
                            let base_type =
                                match self.types.get_generic_instance_info(target_type_id) {
                                    Some(info) => info.generic_parent,
                                    None => target_type_id,
                                };
                            if base_type != named_type {
                                return failf!(
                                    enum_pattern.span,
                                    "Impossible pattern: Enum pattern refers to type '{}' which is not the same as match target '{}'",
                                    self.type_id_to_string_ext(named_type, true).blue(),
                                    self.type_id_to_string_ext(base_type, true).blue()
                                );
                            }
                        }
                    }
                }
                let Some(matching_variant) =
                    enum_type.variants.iter().find(|v| v.name == enum_pattern.variant_name)
                else {
                    return failf!(
                        enum_pattern.span,
                        "Impossible pattern: No variant named '{}'",
                        self.ident_str(enum_pattern.variant_name).blue()
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
                        let payload_pattern = self.eval_pattern(
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
                    let (expected_field_index, expected_field) =
                        expected_struct.find_field(*field_name).ok_or_else(|| {
                            errf!(
                                self.ast.get_pattern_span(*field_parsed_pattern_id),
                                "Impossible pattern: Struct has no field named '{}'",
                                self.ident_str(*field_name).blue()
                            )
                        })?;
                    let field_type_id = expected_field.type_id;
                    let field_pattern = self.eval_pattern(
                        *field_parsed_pattern_id,
                        field_type_id,
                        scope_id,
                        allow_bindings,
                    )?;
                    fields.push(TypedStructPatternField {
                        name: *field_name,
                        pattern: field_pattern,
                        field_index: expected_field_index as u32,
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
                let inner_pattern = self.eval_pattern(
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
                    self.ident_str(expected_field.name),
                    self.ident_str(actual_field.name)
                ));
            }
            self.check_types(expected_field.type_id, actual_field.type_id, scope_id).map_err(
                |msg| {
                    format!(
                        "Struct type mismatch on field '{}': {}",
                        self.ident_str(actual_field.name),
                        msg
                    )
                },
            )?;
        }
        Ok(())
    }

    fn get_type_id_resolved(&self, type_id: TypeId, scope_id: ScopeId) -> TypeId {
        if let Type::TypeParameter(tvar) = self.types.get(type_id) {
            match self.scopes.find_type(scope_id, tvar.name) {
                None => {
                    eprintln!(
                        "*********************\nUnresolved type parameter. {} in {}",
                        self.ident_str(tvar.name),
                        self.scope_id_to_string(scope_id)
                    );
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

    /// Checks the type of `expr`. Will attempt to coerce expr to fulfill
    /// the expected type in some known cases.
    /// 1. Expected: static[T, <value>], Actual: T.
    /// 2. ...
    ///
    /// Current coercion sites:
    /// - Function arguments
    fn check_expr_type(
        &mut self,
        expected: TypeId,
        expr: TypedExprId,
        scope_id: ScopeId,
    ) -> CheckExprTypeResult {
        let actual_type_id = self.exprs.get(expr).get_type();
        // Expected Buffer[Buffer['2]] but got static[Buffer[Buffer[iword]]
        if let Err(msg) = self.check_types(expected, actual_type_id, scope_id) {
            let result = match (
                self.types.get_no_follow_static(expected),
                self.types.get_no_follow_static(actual_type_id),
            ) {
                // If the type inside this static would pass muster under the 'expected type',
                // Erase the static
                (_, Type::Static(actual_static))
                    if self
                        .check_types(expected, actual_static.inner_type_id, scope_id)
                        .is_ok() =>
                {
                    CheckExprTypeResult::Coerce(self.synth_cast(
                        expr,
                        expected,
                        CastType::StaticErase,
                    ))
                }
                (Type::Static(expected_static), actual) if actual.as_static().is_none() => {
                    if expected_static.inner_type_id == actual_type_id {
                        if let Ok(static_lifted) = self.attempt_static_lift(expr) {
                            CheckExprTypeResult::Coerce(static_lifted)
                        } else {
                            CheckExprTypeResult::Err(msg)
                        }
                    } else {
                        CheckExprTypeResult::Err(msg)
                    }
                }
                _ => CheckExprTypeResult::Err(msg),
            };
            if let CheckExprTypeResult::Coerce(coerced_expr) = result {
                debug!(
                    "Coercing {} to {}",
                    self.expr_to_string(expr),
                    self.expr_to_string(coerced_expr),
                );
            }
            result
        } else {
            CheckExprTypeResult::Ok
        }
    }

    pub fn check_types(
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
                    spec1.type_args.iter().zip(spec2.type_args.iter()).enumerate()
                {
                    debug!(
                        "Comparing params {} and {} inside {}",
                        self.type_id_to_string(*exp_param),
                        self.type_id_to_string(*act_param),
                        self.ident_str(
                            self.types.get_defn_info(spec1.generic_parent).unwrap().name
                        )
                    );
                    if let Err(msg) = self.check_types(*exp_param, *act_param, scope_id) {
                        let generic = self.types.get(spec1.generic_parent).expect_generic();
                        let param = self.named_types.get_nth(generic.params, index);
                        let base_msg = format!(
                            "Expected {}, but got {}",
                            self.type_id_to_string(expected),
                            self.type_id_to_string(actual),
                        );
                        let detail =
                            format!("Param '{}' is incorrect: {}", self.ident_str(param.name), msg);
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

        match (self.types.get_no_follow_static(expected), self.types.get_no_follow_static(actual)) {
            (Type::InferenceHole(_hole), _any) => Ok(()),
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
                        self.ident_str(actual_variant.name)
                    ))
                }
            }
            (Type::Lambda(expected_lambda), Type::Lambda(actual_lambda)) => {
                if expected_lambda.parsed_id == actual_lambda.parsed_id
                    && expected_lambda.function_type == actual_lambda.function_type
                {
                    Ok(())
                } else {
                    Err("Expected a unique lambda, but got a different one. This probably shouldn't happen".to_string())
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
            (Type::Static(exp_static_type), Type::Static(act_static_type)) => {
                if exp_static_type.inner_type_id == act_static_type.inner_type_id {
                    match (exp_static_type.value_id, act_static_type.value_id) {
                        (None, None) => Ok(()),    // Both unresolved
                        (None, Some(_)) => Ok(()), // Expected unresolved, actual has a value
                        (Some(exp_value_id), Some(act_value_id)) => {
                            if self.static_value_eq(exp_value_id, act_value_id) {
                                Ok(())
                            } else {
                                Err(format!(
                                    "Different static values of same type: {} vs {}",
                                    self.static_value_to_string(exp_value_id),
                                    self.static_value_to_string(act_value_id)
                                ))
                            }
                        }
                        (Some(_), None) => Err(format!(
                            "Expected a specific static but got an unresolved one: {} vs {}",
                            self.type_id_to_string_ext(expected, false),
                            self.type_id_to_string_ext(actual, false),
                        )),
                    }
                } else {
                    Err(format!(
                        "Expected static {} but got static {}",
                        self.type_id_to_string(expected),
                        self.type_id_to_string(actual)
                    ))
                }
            }
            (Type::FunctionTypeParameter(expected_abstract_function), act) => {
                let expected_function_type = expected_abstract_function.function_type;
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
                if f1.logical_params().len() != f2.logical_params().len() {
                    return Err(format!(
                        "Wrong parameter count: expected {} but got {}",
                        f1.logical_params().len(),
                        f2.logical_params().len()
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
                    for (p1, p2) in f1.logical_params().iter().zip(f2.logical_params().iter()) {
                        if let Err(msg) = self.check_types(p1.type_id, p2.type_id, scope_id) {
                            return Err(format!(
                                "Incorrect type for parameter '{}': {}",
                                self.ident_str(p1.name),
                                msg
                            ));
                        }
                    }
                    Ok(())
                }
            }
            (_expected, Type::Never) => Ok(()),
            (_exp, _act) => Err(format!(
                "Expected {} but got {}",
                self.type_id_to_string_ext(expected, false),
                self.type_id_to_string_ext(actual, false),
            )),
        }
    }

    fn static_value_eq(&self, va: StaticValueId, vb: StaticValueId) -> bool {
        match (self.static_values.get(va), self.static_values.get(vb)) {
            (StaticValue::Unit, StaticValue::Unit) => true,
            (StaticValue::Boolean(b1), StaticValue::Boolean(b2)) => b1 == b2,
            (StaticValue::Char(c1), StaticValue::Char(c2)) => c1 == c2,
            (StaticValue::Int(i1), StaticValue::Int(i2)) => i1 == i2,
            (StaticValue::Float(f1), StaticValue::Float(f2)) => f1 == f2,
            (StaticValue::String(s1), StaticValue::String(s2)) => s1 == s2,
            (StaticValue::NullPointer, StaticValue::NullPointer) => true,
            (StaticValue::Struct(_s1), StaticValue::Struct(_s2)) => {
                eprintln!("WARNING STATIC EQ TODO; RETURNING FALSE");
                false
            }
            (StaticValue::Enum(_e1), StaticValue::Enum(_e2)) => {
                eprintln!("WARNING STATIC EQ TODO; RETURNING FALSE");
                false
            }
            (StaticValue::Buffer(_b1), StaticValue::Buffer(_b2)) => {
                eprintln!("WARNING STATIC EQ TODO; RETURNING FALSE");
                false
            }
            _ => self.ice("Comparing statics of different, or unhandled, kinds", None),
        }
    }

    fn eval_trivial_static_expr(
        &mut self,
        expr_id: TypedExprId,
        _scope_id: ScopeId,
    ) -> TyperResult<Option<StaticValueId>> {
        match self.exprs.get(expr_id) {
            TypedExpr::Unit(_) => Ok(Some(self.static_values.add(StaticValue::Unit))),
            TypedExpr::Char(byte, _) => Ok(Some(self.static_values.add(StaticValue::Char(*byte)))),
            TypedExpr::Bool(b, _) => Ok(Some(self.static_values.add(StaticValue::Boolean(*b)))),
            TypedExpr::Integer(typed_integer_expr) => {
                Ok(Some(self.static_values.add(StaticValue::Int(typed_integer_expr.value))))
            }
            TypedExpr::Float(typed_float_expr) => {
                Ok(Some(self.static_values.add(StaticValue::Float(typed_float_expr.value))))
            }
            TypedExpr::String(s, _) => Ok(Some(self.static_values.add(StaticValue::String(*s)))),
            TypedExpr::Variable(v) => {
                let typed_variable = self.variables.get(v.variable_id);
                let Some(global_id) = typed_variable.global_id else {
                    return failf!(
                        v.span,
                        "Variable cannot be evaluated at compile time: {}",
                        self.ident_str(typed_variable.name)
                    );
                };
                let global = self.globals.get(global_id);
                if let Some(value) = global.initial_value {
                    let value = self.static_values.get(value).clone();
                    Ok(Some(self.static_values.add(value)))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    /// no_reset: Do not wipe the VM on completion; this is passed when the global is evaluated
    /// from an existing static execution already
    fn execute_static_expr_with_vm(
        &mut self,
        vm: &mut vm::Vm,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        no_reset: bool,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> TyperResult<VmExecuteResult> {
        if ctx.is_inference {
            return failf!(
                self.ast.get_expr_span(parsed_expr),
                "#static cannot be used directly in generic calls. Try supplying the types to the call, or moving the static block outside the call"
            );
        }
        let static_ctx = ctx.static_ctx.unwrap();

        let expr_original = self.eval_expr(parsed_expr, ctx)?;
        let span = self.exprs.get(expr_original).get_span();
        let expr = if static_ctx.is_metaprogram {
            // We cast to Never because a value of this type will never be seen;
            // for #meta blocks we do not use the result value, only the emitted code
            // This avoids us failing typechecking;
            // Philosophical note: Never here is not even a hack, since its 'true' purpose
            // is to represent the types of expressions that never yield a value, and code
            // whose value we never insert into the program fits that description perfectly,
            // better than Unit
            self.synth_cast(expr_original, NEVER_TYPE_ID, CastType::ToNever)
        } else {
            expr_original
        };
        let required_type_id = self.exprs.get(expr_original).get_type();
        let output_type_id =
            if static_ctx.is_metaprogram { NEVER_TYPE_ID } else { required_type_id };

        if let Some(shortcut_value_id) = self.eval_trivial_static_expr(expr, ctx.scope_id)? {
            return Ok(VmExecuteResult {
                type_id: required_type_id,
                static_value_id: shortcut_value_id,
                emits: vec![],
            });
        }

        vm.allow_emits = static_ctx.is_metaprogram;

        let value = vm::execute_single_expr_with_vm(self, expr, vm, input_parameters)?;

        if cfg!(debug_assertions) {
            if let Err(msg) = self.check_types(required_type_id, value.get_type(), ctx.scope_id) {
                return failf!(span, "static value type mismatch: {msg}");
            }
        }

        let static_value_id = vm::vm_value_to_static_value(self, vm, value, span)?;

        let emits = vm.emits.clone();

        if !no_reset {
            vm.reset();
        }
        Ok(VmExecuteResult { type_id: output_type_id, static_value_id, emits })
    }

    fn execute_static_expr(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        no_reset: bool,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> TyperResult<VmExecuteResult> {
        let (mut vm, used_alt) = match *std::mem::take(&mut self.vm) {
            None => {
                let span = self.ast.get_expr_span(parsed_expr);
                let maybe_alt = self.alt_vms.pop();
                let (source, location) = self.get_span_location(span);
                let alt_vm = match maybe_alt {
                    None => {
                        eprintln!(
                            "Had to make a new alt VM at {}:{}. We should make them faster to make (virtual alloc?)",
                            source.filename,
                            location.line_number()
                        );
                        let new_vm = vm::Vm::make(10 * crate::MEGABYTE, crate::MEGABYTE);
                        new_vm
                    }
                    Some(alt_vm) => {
                        eprintln!(
                            "We have PLENTY of VMs! at {}:{}",
                            source.filename,
                            location.line_number()
                        );
                        alt_vm
                    }
                };
                (alt_vm, true)
            }
            Some(vm) => (vm, false),
        };
        let res =
            self.execute_static_expr_with_vm(&mut vm, parsed_expr, ctx, no_reset, input_parameters);
        if !used_alt {
            *self.vm = Some(vm);
        } else {
            eprintln!("Restoring an alt VM");
            self.alt_vms.push(vm)
        }
        res
    }

    fn execute_static_bool(
        &mut self,
        cond: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<bool> {
        let vm_cond_result = self.execute_static_expr(
            cond,
            ctx.with_expected_type(Some(BOOL_TYPE_ID)).with_static_ctx(Some(StaticExecContext {
                is_metaprogram: false,
                expected_return_type: Some(BOOL_TYPE_ID),
            })),
            false,
            &[],
        )?;
        let StaticValue::Boolean(condition_bool) =
            self.static_values.get(vm_cond_result.static_value_id)
        else {
            let cond_span = self.ast.get_expr_span(cond);
            return failf!(cond_span, "Condition is not a boolean");
        };
        Ok(*condition_bool)
    }

    fn eval_global_declaration_phase(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        scope_id: ScopeId,
    ) -> TyperResult<VariableId> {
        let parsed_global = self.ast.get_global(parsed_global_id).clone();
        let type_id = self.eval_type_expr(parsed_global.ty, scope_id)?;

        let is_referencing = parsed_global.is_referencing;
        let global_name = parsed_global.name;
        let global_span = parsed_global.span;
        let value_expr_id = parsed_global.value_expr;
        let is_mutable = parsed_global.is_mutable;

        let global_id = self.globals.next_id();
        let variable_id = self.variables.add(Variable {
            name: global_name,
            type_id,
            is_mutable: false,
            owner_scope: scope_id,
            is_context: false,
            global_id: Some(global_id),
            user_hidden: false,
        });
        let actual_global_id = self.globals.add(TypedGlobal {
            variable_id,
            initial_value: None,
            parsed_expr: value_expr_id,
            ty: type_id,
            span: global_span,
            is_referencing,
            is_constant: !is_mutable,
            ast_id: parsed_global_id,
            parent_scope: scope_id,
        });

        debug_assert_eq!(actual_global_id, global_id);
        self.global_ast_mappings.insert(parsed_global_id, global_id);
        self.scopes.add_variable(scope_id, global_name, variable_id);
        Ok(variable_id)
    }

    pub fn eval_global_body(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        vm_to_use: Option<&mut vm::Vm>,
    ) -> TyperResult<()> {
        let parsed_global = self.ast.get_global(parsed_global_id).clone();
        let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id).copied() else {
            self.ice_with_span("ast mapping for global is missing", parsed_global.span)
        };
        let typed_global = self.globals.get(global_id);
        let scope_id = typed_global.parent_scope;
        let type_id = typed_global.ty;

        let is_referencing = parsed_global.is_referencing;
        let type_to_check = if is_referencing {
            let Type::Reference(r) = self.types.get(type_id) else {
                return failf!(parsed_global.span, "Global references must have a reference type");
            };
            r.inner_type
        } else {
            type_id
        };
        let global_name = parsed_global.name;
        let global_span = parsed_global.span;
        let value_expr_id = parsed_global.value_expr;

        let ctx = EvalExprContext {
            scope_id,
            expected_type_id: Some(type_to_check),
            is_inference: false,
            static_ctx: Some(StaticExecContext {
                is_metaprogram: false,
                expected_return_type: Some(type_to_check),
            }),
            global_defn_name: Some(global_name),
            is_generic_pass: false,
        };
        let vm_result = match vm_to_use {
            None => self.execute_static_expr(value_expr_id, ctx, false, &[]),
            Some(vm) => {
                let no_reset = true;
                self.execute_static_expr_with_vm(vm, value_expr_id, ctx, no_reset, &[])
            }
        }?;

        if let Err(msg) = self.check_types(
            type_to_check,
            self.static_values.get(vm_result.static_value_id).get_type(),
            scope_id,
        ) {
            return failf!(
                global_span,
                "Type mismatch for global {}: {}",
                self.ident_str(global_name),
                msg
            );
        }

        self.globals.get_mut(global_id).initial_value = Some(vm_result.static_value_id);

        Ok(())
    }

    fn add_function(&mut self, mut function: TypedFunction) -> FunctionId {
        let id = self.functions.next_id();
        if let Some(specialization_info) = &mut function.specialization_info {
            specialization_info.specialized_function_id = id;
            self.get_function_mut(specialization_info.parent_function)
                .child_specializations
                .push(*specialization_info);
        }
        let is_concrete = self.is_function_concrete(&function);
        if function.compiler_debug {
            eprintln!(
                "is_function_concrete={is_concrete} for {}",
                self.function_to_string(&function, false)
            );
        }
        function.is_concrete = is_concrete;
        self.functions.add(function);
        id
    }

    pub fn get_function(&self, function_id: FunctionId) -> &TypedFunction {
        self.functions.get(function_id)
    }

    pub fn get_function_mut(&mut self, function_id: FunctionId) -> &mut TypedFunction {
        self.functions.get_mut(function_id)
    }

    pub fn get_function_type(&self, function_id: FunctionId) -> &FunctionType {
        self.types.get(self.get_function(function_id).type_id).as_function().unwrap()
    }

    pub fn add_ability_impl(&mut self, ability_impl: TypedAbilityImpl) -> AbilityImplId {
        let id = self.ability_impls.next_id();
        self.ability_impl_table.entry(ability_impl.self_type_id).or_default().push(
            AbilityImplHandle {
                base_ability_id: ability_impl.base_ability_id,
                specialized_ability_id: ability_impl.ability_id,
                full_impl_id: id,
            },
        );
        self.ability_impls.add(ability_impl);
        id
    }

    fn add_constrained_ability_impl(
        &mut self,
        type_variable_id: TypeId,
        impl_signature: TypedAbilitySignature,
        scope_id: ScopeId,
        span: SpanId,
    ) -> AbilityImplId {
        let ability = self.abilities.get(impl_signature.specialized_ability_id);
        let base_ability_id = ability.base_ability_id;
        let ability_self_type = ability.self_type_id;
        let all_params = match ability.parent_ability_id() {
            None => ability.parameters.clone(),
            Some(parent) => self.abilities.get(parent).parameters.clone(),
        };
        let ability_args = self.named_types.get_slice(ability.kind.arguments());
        let mut subst_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        // Add Self
        subst_pairs.push(spair! {ability.self_type_id => type_variable_id});
        let _ = self.scopes.add_type(scope_id, self.ast.idents.builtins.Self_, type_variable_id);
        // Add ability params
        for (parent_ability_param, ability_arg) in
            all_params.iter().filter(|p| !p.is_impl_param).zip(ability_args.iter())
        {
            subst_pairs.push(spair! {parent_ability_param.type_variable_id => ability_arg.type_id});
            let _ = self.scopes.add_type(scope_id, ability_arg.name, ability_arg.type_id);
        }
        // Add impl params
        for (parent_impl_param, impl_arg) in all_params
            .iter()
            .filter(|p| p.is_impl_param)
            .zip(self.named_types.get_slice(impl_signature.impl_arguments).iter())
        {
            subst_pairs.push(spair! {parent_impl_param.type_variable_id => impl_arg.type_id});
            let _ = self.scopes.add_type(scope_id, impl_arg.name, impl_arg.type_id);
        }
        let functions = self.abilities.get(impl_signature.specialized_ability_id).functions.clone();
        let impl_kind = AbilityImplKind::VariableConstraint;
        let functions = functions
            .iter()
            .map(|f| {
                let generic_fn = self.get_function(f.function_id);
                let generic_sig = generic_fn.signature();
                let generic_fn_type_id = generic_fn.type_id;
                let specialized_function_type =
                    self.substitute_in_type(generic_fn_type_id, &subst_pairs);

                // We have to directly remove 'Self' from the type parameters of the signature
                // since it's the only one of the ability params that gets 'encoded' as a type
                // parameter to the function
                let type_params_minus_self = {
                    let mut type_params_minus_self =
                        self.named_types.copy_slice_sv::<4>(generic_sig.type_params);
                    type_params_minus_self.retain(|tp| tp.type_id != ability_self_type);
                    self.named_types.add_slice_from_copy_slice(&type_params_minus_self)
                };
                debug_assert_eq!(type_params_minus_self.len() + 1, generic_sig.type_params.len());
                let specialized_signature = FunctionSignature {
                    function_type: specialized_function_type,
                    type_params: type_params_minus_self,
                    ..generic_sig
                };
                debug!(
                    "specialized constraint ability function signature {}: {}",
                    self.function_signature_to_string(generic_sig),
                    self.function_signature_to_string(specialized_signature),
                );
                Ok(AbilityImplFunction::Abstract(specialized_signature))
            })
            .collect::<TyperResult<EcoVec<_>>>();
        let functions = functions.unwrap_or_else(|err| {
            self.ice(
                "Failed while specializing an impl function for a type variable ability constraint",
                Some(&err),
            )
        });
        self.add_ability_impl(TypedAbilityImpl {
            kind: impl_kind,
            blanket_type_params: SliceHandle::Empty,
            self_type_id: type_variable_id,
            base_ability_id,
            ability_id: impl_signature.specialized_ability_id,
            impl_arguments: impl_signature.impl_arguments,
            functions,
            scope_id,
            span,
            compile_errors: vec![],
        })
    }

    fn add_type_parameter(
        &mut self,
        value: TypeParameter,
        ability_impls: SmallVec<[TypedAbilitySignature; 4]>,
    ) -> TypeId {
        let type_id = self.types.add_anon(Type::TypeParameter(value));
        if !ability_impls.is_empty() {
            for ability_sig in ability_impls.into_iter() {
                let constrained_impl_scope =
                    self.scopes.add_child_scope(value.scope_id, ScopeType::AbilityImpl, None, None);
                let _ =
                    self.scopes.get_scope_mut(constrained_impl_scope).add_type(value.name, type_id);
                self.add_constrained_ability_impl(
                    type_id,
                    ability_sig,
                    constrained_impl_scope,
                    value.span,
                );
            }
        }
        type_id
    }

    fn add_function_type_parameter(&mut self, value: FunctionTypeParameter) -> TypeId {
        let type_id = self.types.add_anon(Type::FunctionTypeParameter(value));
        type_id
    }

    pub fn get_constrained_ability_impls_for_type(
        &self,
        type_id: TypeId,
    ) -> SV4<AbilityImplHandle> {
        match self.ability_impl_table.get(&type_id) {
            None => smallvec![],
            Some(v) => v
                .iter()
                .filter(|handle| {
                    self.ability_impls.get(handle.full_impl_id).kind.is_variable_constraint()
                })
                .copied()
                .collect(),
        }
    }

    /// New resolution works on the base ability
    pub fn find_ability_impl_for_type_or_generate_new(
        &mut self,
        self_type_id: TypeId,
        target_base_ability_id: AbilityId,
        // For each ability parameter, the type it must conform to,
        // or None for if we didn't solve for it, meaning anything is fine
        parameter_constraints: &[Option<TypeId>],
        scope_id: ScopeId,
        span: SpanId,
    ) -> Result<AbilityImplHandle, Cow<'static, str>> {
        let impl_handles_for_self = self.ability_impl_table.get(&self_type_id);
        if let Some(impl_handles) = impl_handles_for_self {
            debug!(
                "NEW Ability dump for {} {:02} in search of {} {:02}\n{}",
                self.type_id_to_string(self_type_id),
                self_type_id,
                self.ident_str(self.abilities.get(target_base_ability_id).name),
                target_base_ability_id.0,
                impl_handles
                    .iter()
                    .map(|h| {
                        format!(
                            "IMPL {:02} {} with args {}",
                            h.specialized_ability_id.0,
                            self.ident_str(self.abilities.get(h.specialized_ability_id).name),
                            self.pretty_print_named_type_slice(
                                self.ability_impls.get(h.full_impl_id).impl_arguments,
                                ", "
                            )
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            let mut valid_impls: SV4<AbilityImplHandle> = smallvec![];
            for impl_handle in impl_handles {
                if let Ok(()) = self.check_ability_impl(
                    target_base_ability_id,
                    *impl_handle,
                    parameter_constraints,
                    scope_id,
                ) {
                    valid_impls.push(*impl_handle);
                }
            }
            match valid_impls.len() {
                0 => {
                    // Fall through to trying blanket implementations
                }
                1 => return Ok(valid_impls[0]),
                _ => {
                    let has_holes = parameter_constraints.iter().any(|c| c.is_none());
                    if has_holes {
                        return Ok(valid_impls[0]);
                    } else {
                        let impls_formatted = valid_impls
                            .iter()
                            .map(|i| {
                                let imp = self.ability_impls.get(i.full_impl_id);
                                format!(
                                    "- IMPL {:02} {:?} {}",
                                    i.full_impl_id.0,
                                    imp.kind,
                                    self.ability_signature_to_string(imp.signature(),)
                                )
                            })
                            .join("\n");
                        eprintln!(
                            "Multiple matching implementations found for constraints {}:\n{}",
                            parameter_constraints
                                .iter()
                                .map(|maybe_type| maybe_type
                                    .map(|t| self.type_id_to_string(t))
                                    .unwrap_or("_".to_string()))
                                .join(", "),
                            impls_formatted
                        );
                        return Err(format!(
                            "Multiple matching implementations found:\n{}",
                            impls_formatted
                        )
                        .into());
                    }
                }
            }
        };
        // Blanket
        debug!(
            "Blanket search for impl {} for {} with constraints {}",
            self.ident_str(self.abilities.get(target_base_ability_id).name),
            self.type_id_to_string(self_type_id),
            parameter_constraints
                .iter()
                .map(|maybe_type| maybe_type
                    .map(|t| self.type_id_to_string(t))
                    .unwrap_or("_".to_string()))
                .join(", ")
        );
        // let mut s = String::new();
        // self.dump_blanket_impls(&mut s).unwrap();
        // eprintln!("{s}");
        if let Some(blanket_impls_for_base) = self.blanket_impls.get(&target_base_ability_id) {
            for blanket_impl_id in blanket_impls_for_base.clone() {
                match self.try_apply_blanket_implementation(
                    blanket_impl_id,
                    self_type_id,
                    target_base_ability_id,
                    parameter_constraints,
                    span,
                ) {
                    None => debug!("Blanket impl didn't work"),
                    Some(impl_handle) => return Ok(impl_handle),
                }
            }
        };
        Err("No matching implementations found".into())
    }

    fn check_ability_impl(
        &self,
        target_base_ability_id: AbilityId,
        impl_handle: AbilityImplHandle,
        parameter_requirements: &[Option<TypeId>],
        scope_id: ScopeId,
    ) -> Result<(), Cow<str>> {
        if impl_handle.base_ability_id != target_base_ability_id {
            return Err(Cow::Borrowed(""));
        }
        let specialized_ability = self.abilities.get(impl_handle.specialized_ability_id);
        for (impl_arg, maybe_constraint) in self
            .named_types
            .get_slice(specialized_ability.kind.arguments())
            .iter()
            .zip(parameter_requirements.iter())
        {
            if let Some(constraint) = maybe_constraint {
                if let Err(msg) = self.check_types(*constraint, impl_arg.type_id, scope_id) {
                    return Err(Cow::Owned(format!(
                        "Implementation has {} := {}, but context requires it to be {}: {msg}",
                        self.ident_str(impl_arg.name),
                        self.type_id_to_string(impl_arg.type_id),
                        self.type_id_to_string(*constraint),
                    )));
                }
            }
        }
        Ok(())
    }

    pub fn find_or_generate_specialized_ability_impl_for_type(
        &mut self,
        self_type_id: TypeId,
        target_specialized_ability_id: AbilityId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> Result<AbilityImplHandle, Cow<'static, str>> {
        let specialized_ability = self.abilities.get(target_specialized_ability_id);
        let base_ability = specialized_ability.base_ability_id;
        let args = specialized_ability.kind.arguments();
        let parameter_constraints: SV4<Option<TypeId>> =
            self.named_types.get_slice(args).iter().map(|nt| Some(nt.type_id)).collect();
        self.find_ability_impl_for_type_or_generate_new(
            self_type_id,
            base_ability,
            &parameter_constraints,
            scope_id,
            span,
        )
    }

    pub fn try_apply_blanket_implementation(
        &mut self,
        blanket_impl_id: AbilityImplId,
        self_type_id: TypeId,
        target_base_ability_id: AbilityId,
        target_ability_args: &[Option<TypeId>],
        span: SpanId,
    ) -> Option<AbilityImplHandle> {
        let old_inference_context = std::mem::take(&mut self.inference_context);
        let mut self_ = scopeguard::guard(self, |s| s.inference_context = old_inference_context);
        let blanket_impl = self_.ability_impls.get(blanket_impl_id);
        let blanket_impl_ability_id = blanket_impl.ability_id;
        let blanket_impl_scope_id = blanket_impl.scope_id;
        let blanket_impl_self_type_id = blanket_impl.self_type_id;
        if !blanket_impl.compile_errors.is_empty() {
            debug!("Blanket impl failed compile; skipping");
            return None;
        }
        let AbilityImplKind::Blanket { parsed_id, .. } = blanket_impl.kind else {
            unreachable!("Expected a blanket impl")
        };

        let blanket_ability = self_.abilities.get(blanket_impl.ability_id);
        let blanket_base = blanket_ability.base_ability_id;

        if blanket_base != target_base_ability_id {
            debug!("Wrong blanket base {}", self_.ident_str(blanket_ability.name));
            return None;
        }

        let blanket_arguments = blanket_ability.kind.arguments();

        debug!(
            "Trying blanket impl {} with blanket arguments {}, impl arguments {}",
            self_.ident_str(blanket_ability.name),
            self_.pretty_print_named_types(self_.named_types.get_slice(blanket_arguments), ", "),
            self_.pretty_print_named_type_slice(blanket_impl.impl_arguments, ", "),
        );

        if blanket_arguments.len() != target_ability_args.len() {
            debug!("Wrong arg count {} vs {}", blanket_arguments.len(), target_ability_args.len());
            return None;
        }

        // Reborrows
        let blanket_ability = self_.abilities.get(blanket_impl_ability_id);
        let blanket_arguments = blanket_ability.kind.arguments();

        //let mut solution_set = TypeSolutionSet::from(blanket_impl.type_params.iter());
        let mut args_and_params: SV8<InferenceInputPair> =
            SmallVec::with_capacity(blanket_arguments.len() + 1);
        //
        // For each argument A to the blanket impl, solve for [Self, ...Params] using
        args_and_params.push(InferenceInputPair {
            arg: TypeOrParsedExpr::Type(self_type_id),
            param_type: blanket_impl_self_type_id,
            allow_mismatch: true,
        });
        for (arg_to_blanket, arg_to_target) in
            self_.named_types.get_slice(blanket_arguments).iter().zip(target_ability_args)
        {
            match arg_to_target {
                None => {
                    eprintln!("No arg for ability param; probably can't solve it")
                }
                Some(arg_to_target) => args_and_params.push(InferenceInputPair {
                    arg: TypeOrParsedExpr::Type(*arg_to_target),
                    param_type: arg_to_blanket.type_id,
                    allow_mismatch: true,
                }),
            };
        }

        let blanket_impl_type_params_handle =
            self_.ability_impls.get(blanket_impl_id).blanket_type_params;
        let root_scope_id = self_.scopes.get_root_scope_id();
        let solutions_result = self_.infer_types(
            blanket_impl_type_params_handle,
            blanket_impl_type_params_handle,
            &args_and_params,
            span,
            root_scope_id,
        );
        let (solutions, _all_solutions) = match solutions_result {
            Err(e) => {
                debug!("Could not solve all blanket impl params: {e}");
                return None;
            }
            Ok(solutions) => solutions,
        };

        // Before going to the trouble of checking constraints and instantiating the blanket
        // impl, we need to check if the resulting implemented ability even works for us.
        // Example: impl[A, B] AsPair[AA = A, BB = B] for Pair[A, B]
        // We now know A and B, so we know we'd get an AsPair[A, B] out.
        // See if that is even what is needed, which is in parameter_constraints.
        let solutions_as_pairs: SV4<TypeSubstitutionPair> =
            self_.zip_named_types_to_subst_pairs(blanket_impl_type_params_handle, solutions);
        for (blanket_arg, required_arg) in
            self_.named_types.copy_slice_sv4(blanket_arguments).iter().zip(target_ability_args)
        {
            if let Some(required_arg) = required_arg {
                let actual_value =
                    self_.substitute_in_type(blanket_arg.type_id, &solutions_as_pairs);
                if let Err(msg) = self_.check_types(*required_arg, actual_value, root_scope_id) {
                    debug!(
                        "blanket impl, if applied, would result in the wrong type for param {}. {}",
                        self_.ident_str(blanket_arg.name),
                        msg
                    );
                    return None;
                }
            }
        }

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
        for (parsed_param, solution) in parsed_blanket_impl
            .generic_impl_params
            .clone()
            .iter()
            .zip(self_.named_types.copy_slice_sv::<4>(solutions).iter())
        {
            let _ = self_.scopes.add_type(
                constraint_checking_scope,
                parsed_param.name,
                solution.type_id,
            );
            for constraint in parsed_param.constraints.iter() {
                let constraint_signature = match constraint {
                    parse::ParsedTypeConstraintExpr::Ability(parsed_ability_expr) => self_
                        .eval_ability_expr(*parsed_ability_expr, false, constraint_checking_scope)
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
            .instantiate_blanket_impl(self_type_id, blanket_impl_id, solutions)
            .unwrap_or_else(|e| self_.ice("Failed to instantiate blanket impl", Some(&e)));
        Some(impl_handle)
    }

    fn instantiate_blanket_impl(
        &mut self,
        self_type_id: TypeId,
        blanket_impl_id: AbilityImplId,
        solutions: NamedTypeSlice,
    ) -> TyperResult<AbilityImplHandle> {
        let blanket_impl = self.ability_impls.get(blanket_impl_id).clone();

        let generic_base_ability_id = blanket_impl.kind.blanket_parent().unwrap();
        debug!(
            "instantiate_blanket_impl: impl {} for {} with {}",
            self.ident_str(self.abilities.get(generic_base_ability_id).name),
            self.type_id_to_string(self_type_id),
            self.pretty_print_named_type_slice(solutions, ", ")
        );

        let new_impl_scope = self.scopes.add_sibling_scope(
            blanket_impl.scope_id,
            ScopeType::AbilityImpl,
            None,
            None,
        );

        let pairs: SV4<TypeSubstitutionPair> = self
            .named_types
            .get_slice(blanket_impl.blanket_type_params)
            .iter()
            .enumerate()
            .map(|(index, param)| {
                let solution = self.named_types.get_nth(solutions, index);
                let _ = self.scopes.add_type(new_impl_scope, param.name, solution.type_id);
                TypeSubstitutionPair { from: param.type_id, to: solution.type_id }
            })
            .collect();

        let blanket_ability_args_handle =
            self.abilities.get(blanket_impl.ability_id).kind.arguments();
        let mut substituted_ability_args: SV4<NameAndType> =
            SmallVec::with_capacity(blanket_ability_args_handle.len());
        for blanket_arg in self.named_types.copy_slice_sv::<4>(blanket_ability_args_handle) {
            // Substitute T, U, V, in for each
            let substituted_type = self.substitute_in_type(blanket_arg.type_id, &pairs);
            let nt = NameAndType { name: blanket_arg.name, type_id: substituted_type };
            substituted_ability_args.push(nt);
            if !self.scopes.add_type(new_impl_scope, blanket_arg.name, substituted_type) {
                panic!("blanket impl scope unexpectedly contained type name")
            };
        }
        let substituted_ability_args_handle =
            self.named_types.add_slice_from_copy_slice(&substituted_ability_args);
        let concrete_ability_id = self.specialize_ability(
            generic_base_ability_id,
            substituted_ability_args_handle,
            blanket_impl.span,
            blanket_impl.scope_id,
        );

        let mut substituted_impl_arguments: SV8<NameAndType> =
            SmallVec::with_capacity(blanket_impl.impl_arguments.len());
        for blanket_impl_arg in self.named_types.copy_slice_sv::<8>(blanket_impl.impl_arguments) {
            // Substitute T, U, V, in for each
            let substituted_type = self.substitute_in_type(blanket_impl_arg.type_id, &pairs);
            let nt = NameAndType { name: blanket_impl_arg.name, type_id: substituted_type };
            substituted_impl_arguments.push(nt);
            if !self.scopes.add_type(new_impl_scope, blanket_impl_arg.name, substituted_type) {
                panic!("uh oh")
            };
        }

        let _ = self.scopes.add_type(new_impl_scope, self.ast.idents.builtins.Self_, self_type_id);

        let mut specialized_functions = EcoVec::with_capacity(blanket_impl.functions.len());
        let kind = AbilityImplKind::DerivedFromBlanket { blanket_impl_id };
        debug!(
            "blanket impl instance scope before function specialization: {}",
            self.scope_id_to_string(new_impl_scope)
        );
        for blanket_impl_function in &blanket_impl.functions {
            // If the functions are abstract, just the type ids
            // If concrete do the declaration thing
            //
            let specialized_function = match *blanket_impl_function {
                AbilityImplFunction::FunctionId(blanket_impl_function_id) => {
                    let blanket_fn = self.get_function(blanket_impl_function_id);
                    let parsed_fn = blanket_fn.parsed_id.as_function_id().unwrap();
                    let specialized_function_id = self
                        .eval_function_declaration(
                            parsed_fn,
                            new_impl_scope,
                            Some(FunctionAbilityContextInfo::ability_impl(
                                concrete_ability_id,
                                self_type_id,
                                kind,
                                Some(blanket_impl_function_id),
                            )),
                            ROOT_NAMESPACE_ID,
                        )?
                        .unwrap();
                    self.functions_pending_body_specialization.push(specialized_function_id);
                    AbilityImplFunction::FunctionId(specialized_function_id)
                }
                AbilityImplFunction::Abstract(_function_type_id) => {
                    todo!("abstract ability impl function in instantiate blanket impl")
                }
            };
            specialized_functions.push(specialized_function);
        }

        let substituted_impl_arguments_handle =
            self.named_types.add_slice_from_copy_slice(&substituted_impl_arguments);
        let id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: SliceHandle::Empty,
            self_type_id,
            ability_id: concrete_ability_id,
            base_ability_id: generic_base_ability_id,
            impl_arguments: substituted_impl_arguments_handle,
            functions: specialized_functions,
            scope_id: new_impl_scope,
            span: blanket_impl.span,
            compile_errors: vec![],
        });
        Ok(AbilityImplHandle {
            base_ability_id: generic_base_ability_id,
            specialized_ability_id: concrete_ability_id,
            full_impl_id: id,
        })
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
        let value = value.map_err(|e| errf!(span, "Invalid f{}: {e}", expected_width.bits()))?;
        Ok(value)
    }

    fn eval_integer_value(
        &self,
        parsed_text: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedIntValue> {
        let default = IntegerType::IWord(self.target_word_size());
        let expected_int_type = match ctx.expected_type_id {
            None => default,
            Some(U8_TYPE_ID) => IntegerType::U8,
            Some(U16_TYPE_ID) => IntegerType::U16,
            Some(U32_TYPE_ID) => IntegerType::U32,
            Some(U64_TYPE_ID) => IntegerType::U64,
            Some(UWORD_TYPE_ID) => IntegerType::UWord(self.target_word_size()),
            Some(I8_TYPE_ID) => IntegerType::I8,
            Some(I16_TYPE_ID) => IntegerType::I16,
            Some(I32_TYPE_ID) => IntegerType::I32,
            Some(I64_TYPE_ID) => IntegerType::I64,
            Some(IWORD_TYPE_ID) => IntegerType::IWord(self.target_word_size()),
            Some(_other) => {
                // Parse as default and let typechecking fail
                default
            }
        };
        macro_rules! parse_int {
            ($int_type:ident, $rust_int_type:ty, $base: expr, $offset: expr) => {{
                let result = <$rust_int_type>::from_str_radix(&parsed_text[$offset..], $base);
                result.map(|int| TypedIntValue::$int_type(int))
            }};
        }
        if parsed_text.starts_with("0x") {
            let hex_base = 16;
            let offset = 2;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, hex_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, hex_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, hex_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, hex_base, offset),
                IntegerType::UWord(WordSize::W32) => parse_int!(UWord32, u32, hex_base, offset),
                IntegerType::UWord(WordSize::W64) => parse_int!(UWord64, u64, hex_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, hex_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, hex_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, hex_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, hex_base, offset),
                IntegerType::IWord(WordSize::W32) => parse_int!(IWord32, i32, hex_base, offset),
                IntegerType::IWord(WordSize::W64) => parse_int!(IWord64, i64, hex_base, offset),
            };
            let value = value
                .map_err(|e| make_error(format!("Invalid hex {expected_int_type}: {e}"), span))?;
            Ok(value)
        } else if parsed_text.starts_with("0b") {
            let bin_base = 2;
            let offset = 2;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, bin_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, bin_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, bin_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, bin_base, offset),
                IntegerType::UWord(WordSize::W32) => parse_int!(UWord32, u32, bin_base, offset),
                IntegerType::UWord(WordSize::W64) => parse_int!(UWord64, u64, bin_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, bin_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, bin_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, bin_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, bin_base, offset),
                IntegerType::IWord(WordSize::W32) => parse_int!(IWord32, i32, bin_base, offset),
                IntegerType::IWord(WordSize::W64) => parse_int!(IWord64, i64, bin_base, offset),
            };
            let value = value.map_err(|e| {
                make_error(format!("Invalid binary {expected_int_type}: {e}"), span)
            })?;
            Ok(value)
        } else {
            let dec_base = 10;
            let offset = 0;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, dec_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, dec_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, dec_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, dec_base, offset),
                IntegerType::UWord(WordSize::W32) => parse_int!(UWord32, u32, dec_base, offset),
                IntegerType::UWord(WordSize::W64) => parse_int!(UWord64, u64, dec_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, dec_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, dec_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, dec_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, dec_base, offset),
                IntegerType::IWord(WordSize::W32) => parse_int!(IWord32, i32, dec_base, offset),
                IntegerType::IWord(WordSize::W64) => parse_int!(IWord64, i64, dec_base, offset),
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
        variable_expr_id: ParsedExprId,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
    ) -> TyperResult<(VariableId, TypedExprId)> {
        let ParsedExpr::Variable(variable) = self.ast.exprs.get(variable_expr_id) else { panic!() };
        let variable_name_span = variable.name.span;
        let variable_id = self.scopes.find_variable_namespaced(
            scope_id,
            &variable.name,
            &self.namespaces,
            &self.ast.idents,
        )?;
        match variable_id {
            None => {
                failf!(
                    variable.name.span,
                    "Variable '{}' is not defined",
                    self.ast.idents.get_name(variable.name.name),
                )
            }
            Some((variable_id, variable_scope_id)) => {
                let parent_lambda_scope_id = self.scopes.nearest_parent_lambda(scope_id);
                let is_capture = if let Some(nearest_parent_lambda_scope) = parent_lambda_scope_id {
                    let variable_is_above_lambda = self
                        .scopes
                        .scope_has_ancestor(nearest_parent_lambda_scope, variable_scope_id);
                    let variable_is_global = self.variables.get(variable_id).global_id.is_some();

                    let is_capture = variable_is_above_lambda && !variable_is_global;
                    debug!("{}, is_capture={is_capture}", self.ident_str(variable.name.name));
                    is_capture
                } else {
                    false
                };

                let v = self.variables.get(variable_id);
                if is_assignment_lhs && !v.is_mutable {
                    return failf!(
                        variable_name_span,
                        "Cannot assign to immutable variable {}",
                        self.ast.idents.get_name(v.name)
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
                    Ok((variable_id, fixup_expr_id))
                } else {
                    let expr = self.exprs.add(TypedExpr::Variable(VariableExpr {
                        type_id: v.type_id,
                        variable_id,
                        span: variable_name_span,
                    }));
                    Ok((variable_id, expr))
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
            Some(field_access.base),
            field_access.field_name,
            None,
            field_access.type_args,
            ctx,
            span,
        )? {
            return Ok(enum_result);
        }

        // Bailout case: .* dereference operation
        if field_access.field_name == self.ast.idents.builtins.asterisk {
            return self.eval_dereference(field_access.base, ctx, span);
        }

        // Bailout case: .! unwrap operation
        if field_access.field_name == self.ast.idents.builtins.bang {
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

        // Bailout case: .try unwrap operation
        if field_access.field_name == self.ast.idents.builtins.try_ {
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

        let base_expr = self.eval_expr(field_access.base, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get(base_expr).get_type();

        // Optional fork case: .tag enum special accessor
        if field_access.field_name == self.ast.idents.builtins.tag {
            if field_access.is_coalescing {
                return failf!(field_access.span, "TODO: tag access on nullish values");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to tag");
            }
            if let Some(get_tag) = self.handle_enum_get_tag(base_expr, field_access.span)? {
                return Ok(get_tag);
            }
        }

        // Perform auto-dereference for accesses that are not 'lvalue'-style or 'referencing' style
        let (struct_type_id, is_reference) = match self.get_expr_type(base_expr) {
            Type::Reference(reference_type) => {
                let inner_type = reference_type.inner_type;
                (inner_type, true)
            }
            _other => {
                if is_assignment_lhs {
                    return failf!(base_span, "Cannot assign to member of non-reference struct");
                } else if field_access.is_referencing && !field_access.is_coalescing {
                    return failf!(
                        base_span,
                        "Field access target is not a pointer, so referencing access with * cannot be used"
                    );
                } else {
                    (base_expr_type, false)
                }
            }
        };
        match self.types.get(struct_type_id) {
            Type::Struct(struct_type) => {
                if is_assignment_lhs && !is_reference {
                    return failf!(span, "Struct must be a reference to be assignable");
                }
                let (field_index, target_field) =
                    struct_type.find_field(field_access.field_name).ok_or_else(|| {
                        errf!(
                            span,
                            "Field {} not found on struct {}",
                            self.ast.idents.get_name(field_access.field_name),
                            self.type_id_to_string(struct_type_id)
                        )
                    })?;
                if target_field.private {
                    let companion_namespace = self.types.get_companion_namespace(struct_type_id);

                    if !self.is_inside_companion_scope(companion_namespace, ctx.scope_id) {
                        return failf!(
                            span,
                            "Field {} is inaccessible from here",
                            self.ident_str(target_field.name)
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
                    field_index: field_index as u32,
                    result_type,
                    is_referencing: field_access.is_referencing,
                    struct_type: struct_type_id,
                    span,
                })))
            }
            Type::EnumVariant(ev) => {
                if self.ast.idents.get_name(field_access.field_name) != "value" {
                    return failf!(
                        span,
                        "Field {} does not exist; try .value",
                        self.ast.idents.get_name(field_access.field_name)
                    );
                }
                let Some(payload_type_id) = ev.payload else {
                    return failf!(
                        span,
                        "Variant {} has no payload value",
                        self.ast.idents.get_name(ev.name)
                    );
                };
                if is_assignment_lhs && !is_reference {
                    return failf!(span, "Enum must be a reference to be assignable");
                }
                let variant_index = ev.index;
                let result_type_id = if field_access.is_referencing {
                    self.types.add_reference_type(payload_type_id)
                } else {
                    payload_type_id
                };
                Ok(self.exprs.add(TypedExpr::EnumGetPayload(GetEnumVariantPayload {
                    enum_variant_expr: base_expr,
                    result_type_id,
                    variant_index,
                    is_referencing: field_access.is_referencing,
                    span,
                })))
            }
            t @ Type::Enum(_opt_type) => {
                if let Some(opt) = t.as_opt_instance() {
                    if !field_access.is_coalescing {
                        return failf!(
                            span,
                            "Optionals have no direct fields; did you mean to use the '?.' operator?"
                        );
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

                    let Type::Struct(struct_type) = self.types.get(opt.inner_type) else {
                        return failf!(
                            span,
                            "?. must be used on optional structs, got {}",
                            self.type_id_to_string(base_expr_type)
                        );
                    };
                    let (field_index, target_field) =
                        struct_type.find_field(field_access.field_name).ok_or_else(|| {
                            errf!(
                                span,
                                "Field {} not found on struct {}",
                                self.ast.idents.get_name(field_access.field_name),
                                self.type_id_to_string(opt.inner_type)
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
                            field_index: field_index as u32,
                            span,
                            is_referencing: false,
                            result_type: field_type,
                            struct_type: opt.inner_type,
                        }));
                    let alternate = self.synth_optional_none(field_type, span);
                    let if_expr = self.synth_if_else(
                        smallvec![],
                        consequent_type_id,
                        has_value,
                        consequent,
                        alternate,
                        span,
                    );
                    self.push_block_stmt_id(&mut block, base_expr_var.defn_stmt);
                    self.add_expr_id_to_block(&mut block, if_expr);
                    Ok(self.exprs.add(TypedExpr::Block(block)))
                } else {
                    failf!(
                        span,
                        "Field {} does not exist",
                        self.ast.idents.get_name(field_access.field_name)
                    )
                }
            }
            _ => failf!(
                span,
                "Field {} does not exist on type {}",
                self.ast.idents.get_name(field_access.field_name),
                self.type_id_to_string(base_expr_type)
            ),
        }
    }

    fn eval_try_operator(
        &mut self,
        operand: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let scope_id = ctx.scope_id;
        let block_return_type = self.get_return_type_for_scope(scope_id, span)?;
        let block_try_impl = self.expect_ability_implementation(
                    block_return_type,
                    TRY_ABILITY_ID,
                    scope_id,
                    span,
                ).map_err(|mut e| {
                        e.message = format!("`.try` can only be used from a function or lambda that returns a type implementing `Try`. {}", e.message);
                        e
                    })?;
        let try_value_original_expr = self.eval_expr(operand, ctx.with_no_expected_type())?;
        let try_value_type = self.get_expr_type_id(try_value_original_expr);
        let value_try_impl =
            self.expect_ability_implementation(try_value_type, TRY_ABILITY_ID, scope_id, span)?;
        let block_impl_args = self.ability_impls.get(block_try_impl.full_impl_id).impl_arguments;
        let value_impl_args = self.ability_impls.get(value_try_impl.full_impl_id).impl_arguments;
        let block_error_type = self
            .named_types
            .get_slice(block_impl_args)
            .iter()
            .find(|nt| nt.name == get_ident!(self, "E"))
            .map(|nt| nt.type_id)
            .unwrap();
        let error_type = self
            .named_types
            .get_slice(value_impl_args)
            .iter()
            .find(|nt| nt.name == get_ident!(self, "E"))
            .map(|nt| nt.type_id)
            .unwrap();
        if let Err(msg) = self.check_types(block_error_type, error_type, scope_id) {
            return failf!(
                span,
                "This function expects a Try, but with a different Error type than the value: {msg}"
            );
        };
        let value_success_type = self
            .named_types
            .get_slice(value_impl_args)
            .iter()
            .find(|nt| nt.name == get_ident!(self, "T"))
            .map(|nt| nt.type_id)
            .unwrap();
        let mut result_block = self.synth_block(scope_id, span);
        let try_value_var = self.synth_variable_defn_simple(
            self.ast.idents.builtins.try_value,
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
        // FIXME: Consider alternatives for locating the block's makeError function
        //        in a less brittle way
        let block_make_error_fn =
            self.ability_impls.get(block_try_impl.full_impl_id).function_at_index(0);

        let get_error_call = self.synth_typed_function_call(
            qident!(self, span, ["Try"], "getError"),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
        )?;
        let make_error_call = self.exprs.add(TypedExpr::Call(Call {
            callee: Callee::from_ability_impl_fn(block_make_error_fn),
            args: smallvec![get_error_call],
            type_args: SliceHandle::Empty,
            return_type: block_return_type,
            span,
        }));
        let return_error_expr =
            self.exprs.add(TypedExpr::Return(TypedReturn { value: make_error_call, span }));
        let if_expr = self.synth_if_else(
            smallvec![],
            value_success_type,
            is_ok_call,
            get_ok_call,
            return_error_expr,
            span,
        );

        self.push_block_stmt_id(&mut result_block, try_value_var.defn_stmt);
        self.add_expr_id_to_block(&mut result_block, if_expr);

        Ok(self.exprs.add(TypedExpr::Block(result_block)))
    }

    fn eval_unwrap_operator(
        &mut self,
        operand: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let operand_expr = self.eval_expr_inner(operand, ctx.with_no_expected_type())?;
        let operand_type = self.exprs.get(operand_expr).get_type();
        let _unwrap_impl = self.expect_ability_implementation(
            operand_type,
            UNWRAP_ABILITY_ID,
            ctx.scope_id,
            span,
        )?;
        self.synth_typed_function_call(
            qident!(self, span, ["Unwrap"], "unwrap"),
            &[],
            &[operand_expr],
            ctx,
        )
    }

    fn eval_dereference(
        &mut self,
        operand: ParsedExprId,
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
    // - lambda to lambda object conversion
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
        // If we expect a lambda object and you pass a lambda
        if let Type::LambdaObject(_lam_obj_type) = self.types.get(expected_type_id) {
            if let Type::Lambda(lambda_type) = self.get_expr_type(expression) {
                let span = self.exprs.get(expression).get_span();
                let lambda_object_type = self.types.add_lambda_object(
                    &self.ast.idents,
                    lambda_type.function_type,
                    lambda_type.parsed_id,
                );
                return CoerceResult::Coerced(
                    "lam->lamobj",
                    self.exprs.add(TypedExpr::Cast(TypedCast {
                        cast_type: CastType::LambdaToLambdaObject,
                        base_expr: expression,
                        target_type_id: lambda_object_type,
                        span,
                    })),
                );
            }
        }

        // If we expect a lambda object and you pass a function reference... (optimized lambda)
        if let Type::LambdaObject(_lam_obj_type) = self.types.get(expected_type_id) {
            if let TypedExpr::FunctionReference(fun_ref) = self.exprs.get(expression) {
                let lambda_object =
                    self.function_to_lambda_object(fun_ref.function_id, fun_ref.span);
                return CoerceResult::Coerced("funref->lamobj", lambda_object);
            }
        }

        // If we don't expect a reference
        let expected = self.types.get(self.get_type_id_resolved(expected_type_id, scope_id));
        if expected.as_reference().is_none() {
            // And we don't expect a function-like type parameter
            if let Type::FunctionTypeParameter(_tp) = expected {
                return CoerceResult::Fail(expression);
            }

            // But you pass a reference
            if let Some(reference) = self.get_expr_type(expression).as_reference() {
                let span = self.exprs.get(expression).get_span();

                // We only do this if the expected type is not a reference at all. Meaning,
                // if your expected type is T*, and you pass a T**, you need to de-reference that yourself.
                // This rule won't help you or do anything for nested references
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
        expr_id: ParsedExprId,
        mut ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let directives = self.ast.exprs.get_directives(expr_id);
        let debug_directive =
            directives.iter().find(|p| matches!(p, ParsedDirective::CompilerDebug { .. }));
        let is_debug = debug_directive.is_some();
        let conditional_compile_expr = directives.iter().find_map(|p| match p {
            ParsedDirective::ConditionalCompile { condition, .. } => Some(*condition),
            _ => None,
        });
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
            Some(condition) => self_.execute_static_bool(condition, ctx)?,
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
        if log::log_enabled!(log::Level::Debug) {
            let expr_span = self_.ast.exprs.get_span(expr_id);
            debug!(
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
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        debug!(
            "eval_expr_inner: {} (hint {})",
            self.ast.get_span_content(self.ast.exprs.get_span(expr_id)),
            //&self.ast.expr_id_to_string(expr_id),
            self.type_id_option_to_string(ctx.expected_type_id),
        );
        let expr = self.ast.exprs.get(expr_id);
        match expr {
            ParsedExpr::ListLiteral(list_expr) => {
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
                list_lit_block.statements = EcoVec::with_capacity(2 + element_count);
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
                let count_expr = self.synth_uword(element_count, span);
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
            ParsedExpr::Struct(_ast_struct) => {
                if let Some(expected_type) = ctx.expected_type_id {
                    if let Type::Struct(_s) = self.types.get(expected_type) {
                        self.eval_expected_struct(expr_id, ctx)
                    } else {
                        self.eval_anonymous_struct(expr_id, ctx)
                    }
                } else {
                    self.eval_anonymous_struct(expr_id, ctx)
                }
            }
            ParsedExpr::If(if_expr) => self.eval_if_expr(&if_expr.clone(), ctx),
            ParsedExpr::While(while_expr) => self.eval_while_loop(&while_expr.clone(), ctx),
            ParsedExpr::Loop(loop_expr) => self.eval_loop_expr(&loop_expr.clone(), ctx),
            ParsedExpr::BinaryOp(_binary_op) => self.eval_binary_op(expr_id, ctx),
            ParsedExpr::UnaryOp(op) => {
                let op = op.clone();
                match op.op_kind {
                    ParsedUnaryOpKind::BooleanNegation => {
                        let negated_expr = self.synth_parsed_bool_not(op.expr);
                        self.eval_expr(negated_expr, ctx)
                    }
                }
            }
            ParsedExpr::Literal(Literal::Unit(span)) => Ok(self.exprs.add(TypedExpr::Unit(*span))),
            ParsedExpr::Literal(Literal::Char(byte, span)) => {
                Ok(self.exprs.add(TypedExpr::Char(*byte, *span)))
            }
            ParsedExpr::Literal(Literal::Numeric(int)) => {
                let numeric_expr = self.eval_numeric_value(&int.text, int.span, ctx)?;
                Ok(self.exprs.add(numeric_expr))
            }
            ParsedExpr::Literal(Literal::Bool(b, span)) => {
                let expr = TypedExpr::Bool(*b, *span);
                Ok(self.exprs.add(expr))
            }
            ParsedExpr::Literal(Literal::String(s, span)) => {
                let expr = TypedExpr::String(*s, *span);
                Ok(self.exprs.add(expr))
            }
            ParsedExpr::Variable(_variable) => {
                Ok(self.eval_variable(expr_id, ctx.scope_id, false)?.1)
            }
            ParsedExpr::FieldAccess(field_access) => {
                let field_access = field_access.clone();
                self.eval_field_access(&field_access, ctx, false)
            }
            ParsedExpr::Block(block) => {
                // TODO(clone big) This clone is actually sad because Block is still big. We need to intern blocks
                let block = block.clone();
                let block_scope =
                    self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);
                let block_ctx = ctx.with_scope(block_scope);
                let needs_terminator = match block.kind {
                    ParsedBlockKind::FunctionBody => true,
                    ParsedBlockKind::LexicalBlock => false,
                    ParsedBlockKind::LoopBody => false,
                };
                let block = self.eval_block(&block, block_ctx, needs_terminator)?;
                Ok(self.exprs.add(TypedExpr::Block(block)))
            }
            ParsedExpr::Call(fn_call) => self.eval_function_call(&fn_call.clone(), None, ctx, None),
            ParsedExpr::For(for_expr) => self.eval_for_expr(&for_expr.clone(), ctx),
            ParsedExpr::AnonEnumConstructor(anon_enum) => {
                let span = anon_enum.span;
                let expected_type = ctx.expected_type_id.ok_or_else(|| {
                    make_error(
                        "Could not infer enum type from context; try supplying the name or providing a type ascription",
                        anon_enum.span,
                    )
                })?;
                match self.types.get(expected_type) {
                    Type::Enum(_e) => Ok(expected_type),
                    Type::EnumVariant(ev) => Ok(ev.enum_type_id),
                    _ => failf!(
                        anon_enum.span,
                        "Could not infer expected enum type for '.' shorthand; expected type was a {} type: {}",
                        self.type_kind_to_string(expected_type),
                        self.type_id_to_string(expected_type)
                    ),
                }?;
                let enum_ctx = ctx.with_expected_type(Some(expected_type));

                let Some(result) = self.handle_enum_constructor(
                    None,
                    anon_enum.variant_name,
                    anon_enum.payload,
                    SliceHandle::Empty,
                    enum_ctx,
                    span,
                )?
                else {
                    self.ice_with_span("handle_enum_constructor should never return Ok(None) when called in anonymous mode", span)
                };
                Ok(result)
            }
            ParsedExpr::Is(is_expr) => {
                let is_expr = *is_expr;
                // If the 'is' is attached to an if/else, that is handled by if/else
                // This is just the case of the detached 'is' where we want to return a boolean
                // indicating whether or not the pattern matched only
                let true_expression = self.ast.exprs.add_expression(parse::ParsedExpr::Literal(
                    parse::Literal::Bool(true, is_expr.span),
                ));
                let false_expression = self.ast.exprs.add_expression(parse::ParsedExpr::Literal(
                    parse::Literal::Bool(false, is_expr.span),
                ));
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
                    self.ast.exprs.add_expression(parse::ParsedExpr::Match(as_match_expr));
                let partial_match = true;
                // For standalone 'is', we don't allow binding to patterns since they won't work
                let allow_bindings = false;
                self.eval_match_expr(match_expr_id, ctx, partial_match, allow_bindings)
            }
            ParsedExpr::Match(_match_expr) => {
                let partial_match = false;
                let allow_bindings = true;
                self.eval_match_expr(expr_id, ctx, partial_match, allow_bindings)
            }
            ParsedExpr::Cast(cast) => self.eval_cast(expr_id, *cast, ctx),
            ParsedExpr::Lambda(_lambda) => self.eval_lambda(expr_id, ctx),
            ParsedExpr::InterpolatedString(_is) => {
                let res = self.eval_interpolated_string(expr_id, ctx)?;
                Ok(res)
            }
            ParsedExpr::Builtin(span) => {
                let Some(defn_name) = ctx.global_defn_name else {
                    return failf!(*span, "builtin can only be used as a top-level expression");
                };
                if ctx.scope_id != self.get_k1_scope_id() {
                    return failf!(*span, "All the known builtins constants live in the k1 scope");
                }
                match self.ident_str(defn_name) {
                    "TEST" => {
                        let is_test_build = self.ast.config.is_test_build;
                        Ok(self.exprs.add(TypedExpr::Bool(is_test_build, *span)))
                    }
                    "OS" => {
                        // TODO: Ideally this is an enum! But we don't support comptime enums yet
                        let os_str = self.ast.config.target.target_os().to_str();
                        let string_id = self.ast.strings.intern(os_str);
                        Ok(self.exprs.add(TypedExpr::String(string_id, *span)))
                    }
                    "NO_STD" => {
                        let no_std = self.ast.config.no_std;
                        Ok(self.exprs.add(TypedExpr::Bool(no_std, *span)))
                    }
                    "DEBUG" => {
                        let debug = self.ast.config.debug;
                        Ok(self.exprs.add(TypedExpr::Bool(debug, *span)))
                    }
                    "IS_STATIC" => Ok(self.exprs.add(TypedExpr::Bool(false, *span))),
                    s => failf!(*span, "Unknown builtin name: {s}"),
                }
            }
            ParsedExpr::Static(stat) => self.eval_static_expr(expr_id, *stat, ctx),
            ParsedExpr::Code(code) => {
                let parsed_stmt_span = self.ast.get_stmt_span(code.parsed_stmt);
                let span_content =
                    self.ast.sources.get_span_content(self.ast.spans.get(parsed_stmt_span));
                let string_id = self.ast.strings.intern(span_content);
                // debug!("content for #code is exactly: `{span_content}`");
                let parsed_code_string = TypedExpr::String(string_id, code.span);
                let id = self.exprs.add(parsed_code_string);
                Ok(id)
            }
            ParsedExpr::QualifiedAbilityCall(qcall) => {
                let qcall = *qcall;
                let signature = self.eval_ability_expr(qcall.ability_expr, true, ctx.scope_id)?;
                // Locate the precise impl
                let self_type_id = self.eval_type_expr(qcall.self_name, ctx.scope_id)?;
                let impl_handle = self
                    .find_or_generate_specialized_ability_impl_for_type(
                        self_type_id,
                        signature.specialized_ability_id,
                        ctx.scope_id,
                        qcall.span,
                    )
                    .map_err(|msg| errf!(qcall.span, "{}", msg))?;

                // Get the function id from it by name I guess
                let call_ast_expr = self.ast.exprs.get(qcall.call_expr).expect_call().clone();
                let call_name = call_ast_expr.name.name;
                let Some((function_index, tafr)) = self
                    .abilities
                    .get(signature.specialized_ability_id)
                    .find_function_by_name(call_name)
                else {
                    return failf!(
                        call_ast_expr.name.span,
                        "No such function `{}` in ability `{}`",
                        self.ident_str(call_name),
                        self.ability_signature_to_string(signature)
                    );
                };
                let impl_ = self.ability_impls.get(impl_handle.full_impl_id);
                let impl_function = impl_.function_at_index(function_index);
                self.eval_function_call(
                    &call_ast_expr,
                    None,
                    ctx,
                    Some(Callee::from_ability_impl_fn(impl_function)),
                )
            }
        }
    }

    /// Compiles `#static <expr>` and `#meta <expr>` constructs
    fn eval_static_expr(
        &mut self,
        expr_id: ParsedExprId,
        stat: ParsedStaticExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = stat.span;
        let base_expr = stat.base_expr;

        // We don't execute statics during the generic pass, since there's no point
        // since we don't know the real types or values
        debug!("eval_static_expr ctx.is_generic_pass={}", ctx.is_generic_pass);
        if ctx.is_generic_pass {
            let typed_expr = match ctx.expected_type_id {
                None => TypedExpr::Unit(span),
                Some(_) => {
                    let unit_expr = self.exprs.add(TypedExpr::Unit(span));
                    TypedExpr::Cast(TypedCast {
                        cast_type: CastType::ToNever,
                        base_expr: unit_expr,
                        target_type_id: NEVER_TYPE_ID,
                        span,
                    })
                }
            };
            return Ok(self.exprs.add(typed_expr));
        }

        let kind = stat.kind;
        let expected_type_for_execution = match kind {
            ParsedStaticBlockKind::Value => match ctx.expected_type_id {
                None => None,
                Some(expected_type_id) => match self.types.get_no_follow_static(expected_type_id) {
                    Type::Static(s) => Some(s.inner_type_id),
                    _ => Some(expected_type_id),
                },
            },
            ParsedStaticBlockKind::Metaprogram => Some(UNIT_TYPE_ID),
        };
        let is_metaprogram = match kind {
            ParsedStaticBlockKind::Value => false,
            ParsedStaticBlockKind::Metaprogram => true,
        };
        let mut static_parameters: SV4<(VariableId, StaticValueId)> = smallvec![];
        for param in self.ast.p_idents.copy_slice_sv::<4>(stat.parameter_names) {
            let variable_expr =
                self.ast.exprs.add_expression(ParsedExpr::Variable(ParsedVariable {
                    name: NamespacedIdentifier::naked(param, span),
                }));
            let (variable_id, variable_expr) =
                self.eval_variable(variable_expr, ctx.scope_id, false)?;
            let variable_type = self.exprs.get(variable_expr).get_type();
            if let Type::Static(stat) = self.types.get_no_follow_static(variable_type) {
                if let Some(value_id) = stat.value_id {
                    static_parameters.push((variable_id, value_id));
                } else {
                    return failf!(
                        span,
                        "Static parameter `{}` is unresolved",
                        self.ident_str(param)
                    );
                }
            } else {
                return failf!(
                    span,
                    "Non-static parameters aren't supported yet: {}",
                    self.ident_str(param)
                );
            }
        }
        for s in &static_parameters {
            eprintln!(
                "Variable {} := `{}` will be passed in to static execution",
                self.ident_str(self.variables.get(s.0).name),
                self.static_value_to_string(s.1)
            );
        }
        let vm_result = self.execute_static_expr(
            base_expr,
            ctx.with_expected_type(expected_type_for_execution).with_static_ctx(Some(
                StaticExecContext {
                    is_metaprogram,
                    expected_return_type: expected_type_for_execution,
                },
            )),
            false,
            &static_parameters,
        )?;

        match kind {
            ParsedStaticBlockKind::Metaprogram => {
                if vm_result.emits.is_empty() {
                    Ok(self.exprs.add(TypedExpr::Unit(span)))
                } else {
                    // First, we write the emitted code to a text buffer
                    //
                    // Then we parse the code anew (so that we have cohesive spans and a source
                    // containing the full code)
                    //
                    // Then we typecheck the code and emit a block in place of this #meta
                    // invocation
                    let mut content = std::mem::take(&mut self.buffers.emitted_code);

                    content.push('{');
                    for emit in vm_result.emits.iter() {
                        match emit {
                            CodeEmission::String(string_id) => {
                                content.push_str(self.ast.strings.get_string(*string_id));
                            }
                        }
                    }
                    content.push('}');
                    debug!(
                        "Emitted raw content after {} emits:\n---\n{content}\n---",
                        vm_result.emits.len()
                    );
                    let generated_filename = format!("static_{}.k1g", expr_id.as_u32());
                    let origin_file = self.ast.spans.get(span).file_id;
                    let origin_source = self.ast.sources.get_source(origin_file);
                    let source_for_emission =
                        self.ast.sources.add_source(crate::parse::Source::make(
                            0,
                            origin_source.directory.clone(),
                            generated_filename,
                            content.clone(),
                        ));

                    let parsed_metaprogram_result =
                        self.parse_ad_hoc_expr(source_for_emission, &content);
                    content.clear();
                    self.buffers.emitted_code = content;
                    let parsed_metaprogram = parsed_metaprogram_result?;
                    let typed_metaprogram = self.eval_expr(parsed_metaprogram, ctx)?;
                    debug!("Emitted compiled expr:\n{}", self.expr_to_string(typed_metaprogram));

                    // Typecheck the metaprogram!

                    Ok(typed_metaprogram)
                }
            }
            ParsedStaticBlockKind::Value => {
                //match ctx.expected_type_id {
                //    None => None,
                //    Some(expected_type_id) => {
                //        match self.types.get_no_follow_static(expected_type_id) {
                //            Type::Static(s) => Some(s.inner_type_id),
                //            _ => Some(expected_type_id),
                //        }
                //    }
                //};
                let static_type_id =
                    self.types.add_static_type(vm_result.type_id, Some(vm_result.static_value_id));
                let static_value_expr = self.exprs.add(TypedExpr::StaticValue(
                    vm_result.static_value_id,
                    static_type_id,
                    span,
                ));
                Ok(static_value_expr)
            }
        }
    }

    fn parse_ad_hoc_expr(&mut self, file_id: FileId, code_str: &str) -> TyperResult<ParsedExprId> {
        let module = self.modules.get(self.module_in_progress.unwrap());
        let parsed_namespace_id =
            self.namespaces.get(module.namespace_id).parsed_id.as_namespace_id().unwrap();
        // nocommit(2): Write out a file for each static emission, that way debugger can see
        // Maybe in .k1-out?
        let mut lexer = crate::lex::Lexer::make(code_str, &mut self.ast.spans, file_id);
        let mut tokens = std::mem::take(&mut self.buffers.emit_lexer_tokens);
        if let Err(e) = lexer.run(&mut tokens) {
            let e = ParseError::Lex(e);
            parse::print_error(&self.ast, &e);
            tokens.clear();
            return failf!(e.span(), "Failed to lex code emitted from here");
        };

        let mut p = crate::parse::Parser::make_for_file(
            module.name,
            parsed_namespace_id,
            &mut self.ast,
            &tokens,
            file_id,
        );

        let result = match p.expect_expression() {
            Err(e) => {
                failf!(e.span(), "Failed to parse your emitted code: {}", e)
            }
            Ok(parsed_block) => Ok(parsed_block),
        };

        tokens.clear();
        self.buffers.emit_lexer_tokens = tokens;

        result
    }

    fn eval_anonymous_struct(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let mut field_values = Vec::new();
        let mut field_defns = EcoVec::new();
        let ParsedExpr::Struct(parsed_struct) = self.ast.exprs.get(expr_id) else {
            self.ice_with_span("expected struct", self.ast.get_expr_span(expr_id))
        };
        let ast_struct = parsed_struct.clone();
        for ast_field in ast_struct.fields.iter() {
            let parsed_expr = match ast_field.expr.as_ref() {
                None => {
                    self.ast.exprs.add_expression(ParsedExpr::Variable(parse::ParsedVariable {
                        name: NamespacedIdentifier::naked(ast_field.name, ast_field.span),
                    }))
                }
                Some(expr) => *expr,
            };
            let expr = self.eval_expr(parsed_expr, ctx.with_expected_type(None))?;
            let expr_type = self.exprs.get(expr).get_type();
            field_defns.push(StructTypeField {
                name: ast_field.name,
                type_id: expr_type,
                private: false,
            });
            field_values.push(StructField { name: ast_field.name, expr });
        }

        let struct_type =
            StructType { fields: field_defns, defn_id: None, generic_instance_info: None };
        let struct_type_id = self.types.add_anon(Type::Struct(struct_type));
        let typed_struct =
            StructLiteral { fields: field_values, span: ast_struct.span, type_id: struct_type_id };
        Ok(self.exprs.add(TypedExpr::Struct(typed_struct)))
    }

    fn eval_expected_struct(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::Struct(parsed_struct) = self.ast.exprs.get(expr_id) else {
            self.ice_with_span("expected struct", self.ast.get_expr_span(expr_id))
        };
        let original_expected_struct_id = ctx.expected_type_id.unwrap();
        let Type::Struct(original_expected_struct) = self.types.get(original_expected_struct_id)
        else {
            self.ice_with_span("expected an expected struct type", self.ast.get_expr_span(expr_id))
        };
        let expected_struct_id = original_expected_struct_id;
        let expected_struct = self.types.get(expected_struct_id).expect_struct().clone();
        let expected_struct_defn_id = expected_struct.defn_id;
        let field_count = expected_struct.fields.len();

        let ast_struct = parsed_struct.clone();

        // Try to use just stack space for this scratch data structure
        let mut passed_fields_aligned: SmallVec<
            [(ParsedExprId, &StructValueField, &StructTypeField); 8],
        > = SmallVec::with_capacity(field_count);

        let struct_span = ast_struct.span;
        for expected_field in expected_struct.fields.iter() {
            let Some(passed_field) =
                &ast_struct.fields.iter().find(|f| f.name == expected_field.name)
            else {
                return failf!(
                    struct_span,
                    "Struct is missing expected field '{}'",
                    self.ident_str(expected_field.name)
                );
            };
            let parsed_expr = match passed_field.expr.as_ref() {
                None => {
                    self.ast.exprs.add_expression(ParsedExpr::Variable(parse::ParsedVariable {
                        name: NamespacedIdentifier::naked(passed_field.name, passed_field.span),
                    }))
                }
                Some(expr) => *expr,
            };
            passed_fields_aligned.push((parsed_expr, passed_field, expected_field))
        }

        if let Some(unknown_field) = ast_struct
            .fields
            .iter()
            .find(|passed_field| original_expected_struct.find_field(passed_field.name).is_none())
        {
            return failf!(
                struct_span,
                "Struct has an unexpected field '{}'",
                self.ident_str(unknown_field.name)
            );
        }

        let mut field_values: Vec<StructField> = Vec::with_capacity(field_count);
        let mut field_types: EcoVec<StructTypeField> = EcoVec::with_capacity(field_count);
        for ((passed_expr, passed_field, _), expected_field) in
            passed_fields_aligned.iter().zip(expected_struct.fields.iter())
        {
            let expr =
                self.eval_expr(*passed_expr, ctx.with_expected_type(Some(expected_field.type_id)))?;
            let expr_type = self.exprs.get(expr).get_type();
            if ctx.is_inference {
                debug!(
                    "[infer] Checking struct field {} against {}",
                    self.type_id_to_string(expr_type),
                    self.type_id_to_string(expected_field.type_id)
                );
            }
            if let Err(msg) = self.check_types(expected_field.type_id, expr_type, ctx.scope_id) {
                return failf!(
                    passed_field.span,
                    "Field '{}' has incorrect type: {msg}",
                    self.ident_str(passed_field.name)
                );
            }
            field_types.push(StructTypeField {
                name: expected_field.name,
                type_id: expr_type,
                private: expected_field.private,
            });
            field_values.push(StructField { name: expected_field.name, expr });
        }

        let output_instance_info = match expected_struct.generic_instance_info {
            None => None,
            Some(mut gi) => {
                if ctx.is_inference {
                    debug!(
                        "I need to set the right info for {} from expected [{}] and my literal values [{}]",
                        self.type_id_to_string_ext(gi.generic_parent, true),
                        self.pretty_print_types(&gi.type_args, ", "),
                        self.pretty_print_types(
                            &field_types.iter().map(|ft| ft.type_id).collect::<Vec<_>>(),
                            ", "
                        )
                    );
                    let mut substs: Vec<TypeSubstitutionPair> = vec![];
                    // We're effectively reverse-engineering what params were used to get to this type
                    // So we start with: { a: int, b: true } and definition Pair[A, B] = { a: A, b: B }
                    // And we need to solve for A and B as int and bool.
                    let generic_type = self.types.get(gi.generic_parent).expect_generic();
                    let generic_struct_id = generic_type.inner;
                    let generic_fields = &self.types.get(generic_struct_id).expect_struct().fields;
                    const TYPE_PARAM_MODE: bool = true;
                    for (value, generic_field) in field_types.iter().zip(generic_fields.iter()) {
                        let res = self.unify_and_find_substitutions_rec(
                            &mut substs,
                            value.type_id,
                            generic_field.type_id,
                            TYPE_PARAM_MODE,
                        );
                        debug_assert!(!matches!(res, TypeUnificationResult::NonMatching(_)))
                    }
                    let mut type_args_to_use: SV4<TypeId> =
                        SmallVec::with_capacity(generic_type.params.len());
                    for gp in self.named_types.get_slice(generic_type.params) {
                        let Some(matching) = substs.iter().find_map(|pair| {
                            if pair.from == gp.type_id { Some(pair.to) } else { None }
                        }) else {
                            self.ice_with_span(
                                "couldnt reverse engineer generic type params for struct literal",
                                struct_span,
                            );
                        };
                        type_args_to_use.push(matching)
                    }
                    debug!(
                        "I reverse-engineered these: {}",
                        self.pretty_print_types(&type_args_to_use, ", ")
                    );
                    gi.type_args = type_args_to_use;
                    Some(gi)
                } else {
                    Some(gi)
                }
            }
        };
        let output_struct = StructType {
            fields: field_types,
            defn_id: expected_struct_defn_id,
            generic_instance_info: output_instance_info,
        };
        let output_struct_type_id =
            self.types.add(Type::Struct(output_struct), expected_struct_defn_id);

        let typed_struct = StructLiteral {
            fields: field_values,
            span: ast_struct.span,
            type_id: output_struct_type_id,
        };
        let expr = TypedExpr::Struct(typed_struct);
        Ok(self.exprs.add(expr))
    }

    fn eval_while_loop(
        &mut self,
        while_expr: &ParsedWhileExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::Block(parsed_block) = self.ast.exprs.get(while_expr.body).clone() else {
            return failf!(while_expr.span, "'while' body must be a block");
        };

        let condition_block_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);

        let condition = self
            .eval_matching_condition(while_expr.cond, ctx.with_scope(condition_block_scope_id))?;

        let body_block_scope_id = self.scopes.add_child_scope(
            condition_block_scope_id,
            ScopeType::WhileLoopBody,
            None,
            None,
        );
        self.scopes
            .add_loop_info(body_block_scope_id, ScopeLoopInfo { break_type: Some(UNIT_TYPE_ID) });

        let body_block =
            self.eval_block(&parsed_block, ctx.with_scope(body_block_scope_id), false)?;

        // TODO: Detect divergent loops:
        // if loop has no breaks or returns, can we type is as never?
        //
        // Loop Info should be able to track this, if we report every
        // break and return
        let loop_type = if condition.diverges { NEVER_TYPE_ID } else { UNIT_TYPE_ID };

        let body_block = self.exprs.add(TypedExpr::Block(body_block));
        Ok(self.exprs.add(TypedExpr::WhileLoop(WhileLoop {
            condition_block: condition,
            body: body_block,
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

        let body_block = self.exprs.add(TypedExpr::Block(block));
        Ok(self.exprs.add(TypedExpr::LoopExpr(LoopExpr {
            body_block,
            break_type: loop_info.break_type.unwrap_or(UNIT_TYPE_ID),
            span: loop_expr.span,
        })))
    }

    fn eval_interpolated_string(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.ast.exprs.get_span(expr_id);
        let ParsedExpr::InterpolatedString(interpolated_string) = self.ast.exprs.get(expr_id)
        else {
            panic!()
        };

        let part_count = interpolated_string.parts.len();
        if part_count == 1 {
            let parse::InterpolatedStringPart::String(s) =
                interpolated_string.parts.first().unwrap().clone()
            else {
                self.ice_with_span("String had only one part that was not a string", span)
            };
            let s = self.exprs.add(TypedExpr::String(s, span));
            Ok(s)
        } else {
            let interpolated_string = interpolated_string.clone();
            let mut block = self.synth_block(ctx.scope_id, span);
            let block_scope = block.scope_id;
            let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
            let part_count_expr = self.synth_uword(part_count, span);
            if self.ast.config.no_std {
                return failf!(span, "Interpolated strings are not supported in no_std mode");
            }
            let new_string_builder = self.synth_typed_function_call(
                qident!(self, span, ["core", "List"], "withCapacity"),
                &[CHAR_TYPE_ID],
                &[part_count_expr],
                block_ctx,
            )?;
            let string_builder_var = self.synth_variable_defn(
                get_ident!(self, "sb"),
                new_string_builder,
                false,
                false,
                true,
                block.scope_id,
            );
            self.push_block_stmt_id(&mut block, string_builder_var.defn_stmt);
            for part in interpolated_string.parts.into_iter() {
                let print_expr = match part {
                    parse::InterpolatedStringPart::String(s) => {
                        let string_expr = self.exprs.add(TypedExpr::String(s, span));
                        self.synth_printto_call(string_expr, string_builder_var.variable_expr, ctx)?
                    }
                    parse::InterpolatedStringPart::Expr(expr_id) => {
                        let typed_expr_to_stringify = self.eval_expr(expr_id, ctx)?;
                        self.synth_printto_call(
                            typed_expr_to_stringify,
                            string_builder_var.variable_expr,
                            ctx,
                        )?
                    }
                };
                self.add_expr_id_to_block(&mut block, print_expr);
            }
            let sb_deref = self.synth_dereference(string_builder_var.variable_expr);
            let build_call = self.synth_typed_function_call(
                qident!(self, span, ["core", "string"], "wrapList"),
                &[],
                &[sb_deref],
                block_ctx,
            )?;
            self.add_expr_id_to_block(&mut block, build_call);
            Ok(self.exprs.add(TypedExpr::Block(block)))
        }
    }

    #[allow(unused)]
    fn visit_stmt_tree<S, R>(
        &self,
        stmt_id: TypedStmtId,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        let stmt = self.stmts.get(stmt_id);
        match stmt {
            TypedStmt::Expr(e, _) => self.visit_expr_tree(*e, state, action),
            TypedStmt::Let(val_def) => self.visit_expr_tree(val_def.initializer, state, action),
            TypedStmt::Assignment(assgn) => {
                if let Some(r) = self.visit_expr_tree(assgn.destination, state, action) {
                    return Some(r);
                };
                if let Some(r) = self.visit_expr_tree(assgn.value, state, action) {
                    return Some(r);
                };
                None
            }
            TypedStmt::Require(typed_require_stmt) => {
                if let Some(r) =
                    self.visit_matching_condition(&typed_require_stmt.condition, state, action)
                {
                    return Some(r);
                };
                self.visit_expr_tree(typed_require_stmt.else_body, state, action)
            }
        }
    }

    fn visit_matching_condition<S, R>(
        &self,
        cond: &MatchingCondition,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        for instr in &cond.instrs {
            let result = match instr {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    self.visit_stmt_tree(*let_stmt, state, action)
                }
                MatchingConditionInstr::Cond { value } => {
                    self.visit_expr_tree(*value, state, action)
                }
            };
            if let Some(r) = result {
                return Some(r);
            }
        }
        None
    }

    // The task of 'visiting' each expr involves 2 things
    // - Call action on it(self)
    // - Call visit on its children.
    //
    // It is not the job of action to recurse to its children
    fn visit_expr_tree<S, R>(
        &self,
        expr: TypedExprId,
        state: &mut S,
        action: &mut impl FnMut(&TypedProgram, TypedExprId, &mut S) -> Option<R>,
    ) -> Option<R> {
        eprintln!("VISITING {}", self.expr_to_string(expr));
        macro_rules! recurse {
            ($expr_id:expr) => {
                if let Some(r) = self.visit_expr_tree($expr_id, state, action) {
                    return Some(r);
                }
            };
        }
        macro_rules! recurse_stmt {
            ($stmt_id:expr) => {
                if let Some(r) = self.visit_stmt_tree($stmt_id, state, action) {
                    return Some(r);
                }
            };
        }

        if let Some(r) = action(self, expr, state) {
            return Some(r);
        }

        match self.exprs.get(expr) {
            TypedExpr::Unit(_) => (),
            TypedExpr::Char(_, _) => (),
            TypedExpr::Bool(_, _) => (),
            TypedExpr::Integer(_) => (),
            TypedExpr::Float(_) => (),
            TypedExpr::String(_, _) => (),
            TypedExpr::Struct(s) => {
                for f in s.fields.clone().iter() {
                    recurse!(f.expr);
                }
            }
            TypedExpr::Variable(_) => (),
            TypedExpr::StructFieldAccess(field_access) => {
                recurse!(field_access.base);
            }
            TypedExpr::BinaryOp(binary_op) => {
                let lhs = binary_op.lhs;
                let rhs = binary_op.rhs;
                recurse!(lhs);
                recurse!(rhs);
            }
            TypedExpr::UnaryOp(unary_op) => {
                recurse!(unary_op.expr);
            }
            TypedExpr::Block(block) => {
                for stmt in block.statements.iter() {
                    recurse_stmt!(*stmt);
                }
            }
            TypedExpr::Call(call) => {
                match call.callee {
                    Callee::DynamicLambda(callee_expr) => recurse!(callee_expr),
                    Callee::DynamicFunction { function_reference_expr } => {
                        recurse!(function_reference_expr)
                    }
                    _ => {}
                };
                for arg in call.args.iter() {
                    recurse!(*arg);
                }
            }
            TypedExpr::Match(typed_match) => {
                for stmt in &typed_match.initial_let_statements {
                    recurse_stmt!(*stmt);
                }
                for arm in &typed_match.arms {
                    if let Some(r) = self.visit_matching_condition(&arm.condition, state, action) {
                        return Some(r);
                    };
                    recurse!(arm.consequent_expr);
                }
            }
            TypedExpr::WhileLoop(while_loop) => {
                if let Some(r) =
                    self.visit_matching_condition(&while_loop.condition_block, state, action)
                {
                    return Some(r);
                };
                recurse!(while_loop.body);
            }
            TypedExpr::LoopExpr(loop_expr) => {
                recurse!(loop_expr.body_block);
            }
            TypedExpr::EnumConstructor(constr) => {
                if let Some(payload) = constr.payload {
                    recurse!(payload)
                }
            }
            TypedExpr::EnumIsVariant(enum_is_variant) => {
                recurse!(enum_is_variant.enum_expr)
            }
            TypedExpr::EnumGetTag(get_enum_tag) => {
                recurse!(get_enum_tag.enum_expr);
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                recurse!(enum_get_payload.enum_variant_expr);
            }
            TypedExpr::Cast(cast) => recurse!(cast.base_expr),
            TypedExpr::Return(ret) => recurse!(ret.value),
            TypedExpr::Break(brk) => recurse!(brk.value),
            TypedExpr::Lambda(lam) => {
                let lambda_type = self.types.get(lam.lambda_type).as_lambda().unwrap();
                let function = self.get_function(lambda_type.body_function_id);
                recurse!(function.body_block.expect("lambdas have bodies"));
            }
            TypedExpr::FunctionReference(_) => {}
            TypedExpr::FunctionToLambdaObject(_) => {}
            TypedExpr::PendingCapture(_) => {}
            TypedExpr::StaticValue(_, _, _) => {}
        };
        None
    }

    fn eval_lambda(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        fn fixup_capture_expr_new(
            module: &mut TypedProgram,
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
            let (field_index, env_struct_field) =
                module.types.get(env_struct_type).expect_struct().find_field(v.name).unwrap();
            let field_name = env_struct_field.name;
            let env_variable_expr = module.exprs.add(TypedExpr::Variable(VariableExpr {
                variable_id: environment_param_variable_id,
                type_id: env_struct_reference_type,
                span,
            }));
            let env_field_access = TypedExpr::StructFieldAccess(FieldAccess {
                base: env_variable_expr,
                target_field: field_name,
                field_index: field_index as u32,
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
                "lambda expected type is {} {}",
                self.types.get(t).kind_name(),
                self.type_id_to_string(t)
            );
        }

        let expected_function_type = ctx
            .expected_type_id
            .and_then(|et| self.extract_function_type_from_functionlike(self.types.get(et)))
            .map(|ft| self.types.get(ft).as_function().unwrap().clone());
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
                            self.ident_str(arg.binding)
                        );
                    };
                    let Some(expected_ty) = expected_function_type.logical_params().get(index)
                    else {
                        return failf!(
                            arg.span,
                            "Cannot infer lambda parameter type {}: expected type has fewer parameters than lambda",
                            self.ident_str(arg.binding)
                        );
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

        let mut param_variables = EcoVec::with_capacity(typed_params.len());
        let lambda_scope = self.scopes.get_scope_mut(lambda_scope_id);
        for typed_arg in typed_params.iter() {
            let name = typed_arg.name;
            let variable_id = self.variables.add(Variable {
                name,
                type_id: typed_arg.type_id,
                is_mutable: false,
                owner_scope: lambda_scope_id,
                is_context: false,
                global_id: None,
                user_hidden: false,
            });
            lambda_scope.add_variable(name, variable_id);
            param_variables.push(variable_id)
        }

        // Coerce parsed expr to block, call eval_block with needs_terminator = true
        let ast_body_block = match self.ast.exprs.get(lambda_body) {
            ParsedExpr::Block(b) => b.clone(),
            other_expr => {
                let block = parse::ParsedBlock {
                    span: other_expr.get_span(),
                    kind: ParsedBlockKind::FunctionBody,
                    stmts: eco_vec![
                        self.ast.stmts.add(parse::ParsedStmt::LoneExpression(lambda_body))
                    ],
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

        let return_type = match body.expr_type {
            NEVER_TYPE_ID => expected_return_type.unwrap_or(NEVER_TYPE_ID),
            _ => body.expr_type,
        };

        let encl_fn_name = self
            .get_function(
                self.scopes
                    .nearest_parent_function(ctx.scope_id)
                    .expect("lambda to be inside a function"),
            )
            .name;
        let name = self.ast.idents.intern(format!(
            "{}_{{lambda}}_{}",
            self.ident_str(encl_fn_name),
            lambda_scope_id,
        ));
        let name_string = self.make_qualified_name(ctx.scope_id, name, "__", true);
        let name = self.ast.idents.intern(name_string);

        let body_expr_id = self.exprs.add(TypedExpr::Block(body));

        let lambda_info = self.scopes.get_lambda_info(lambda_scope_id);

        // NO CAPTURES! Optimize to a regular function
        if lambda_info.captured_variables.is_empty() {
            let function_type = self.types.add_anon(Type::Function(FunctionType {
                physical_params: EcoVec::from_iter(typed_params.into_iter()),
                return_type,
            }));

            self.scopes.get_scope_mut(lambda_scope_id).scope_type = ScopeType::FunctionScope;
            let body_function_id = self.add_function(TypedFunction {
                name,
                scope: lambda_scope_id,
                param_variables,
                type_params: SliceHandle::Empty,
                function_type_params: SliceHandle::Empty,
                static_type_params: SliceHandle::Empty,
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
                dyn_fn_id: None,
            });
            // What type of expression to return? A function reference seems perfect.
            let function_reference_type = self.types.add_reference_type(function_type);
            let expr_id = self.exprs.add(TypedExpr::FunctionReference(FunctionReferenceExpr {
                function_id: body_function_id,
                function_reference_type,
                span,
            }));
            return Ok(expr_id);
        }

        let env_fields = lambda_info
            .captured_variables
            .iter()
            .map(|captured_variable_id| {
                let v = self.variables.get(*captured_variable_id);
                StructTypeField { type_id: v.type_id, name: v.name, private: false }
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
        let environment_struct_type = self.types.add_anon(Type::Struct(StructType {
            fields: env_fields,
            defn_id: None,
            generic_instance_info: None,
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
            name: self.ast.idents.builtins.lambda_env_var_name,
            type_id: POINTER_TYPE_ID,
            is_context: false,
            is_lambda_env: true,
            span: body_span,
        };
        let environment_param_variable_id = self.variables.add(Variable {
            name: environment_param.name,
            type_id: POINTER_TYPE_ID,
            is_mutable: false,
            owner_scope: lambda_scope_id,
            is_context: false,
            global_id: None,
            user_hidden: false,
        });
        typed_params.push_front(environment_param);
        param_variables.insert(0, environment_param_variable_id);

        let environment_param_access_expr = self.exprs.add(TypedExpr::Variable(VariableExpr {
            variable_id: param_variables[0],
            type_id: POINTER_TYPE_ID,
            span: body_span,
        }));
        let cast_env_param = self.synth_cast(
            environment_param_access_expr,
            environment_struct_reference_type,
            CastType::PointerToReference,
        );
        let environment_casted_variable = self.synth_variable_defn(
            self.ast.idents.builtins.env,
            cast_env_param,
            false,
            false,
            false,
            lambda_scope_id,
        );
        if let TypedExpr::Block(body) = self.exprs.get_mut(body_expr_id) {
            body.statements.insert(0, environment_casted_variable.defn_stmt);
        } else {
            panic!()
        }

        let pending_fixups =
            self.scopes.get_lambda_info(lambda_scope_id).capture_exprs_for_fixup.clone();

        for pending_fixup in pending_fixups {
            let TypedExpr::PendingCapture(pc) = self.exprs.get(pending_fixup) else {
                unreachable!()
            };
            let field_access_expr = fixup_capture_expr_new(
                self,
                environment_casted_variable.variable_id,
                pc.captured_variable_id,
                environment_struct_type,
                pc.span,
            );
            *self.exprs.get_mut(pending_fixup) = field_access_expr;
        }

        let function_type = self.types.add_anon(Type::Function(FunctionType {
            physical_params: EcoVec::from_iter(typed_params.into_iter()),
            return_type,
        }));

        let body_function_id = self.add_function(TypedFunction {
            name,
            scope: lambda_scope_id,
            param_variables,
            type_params: SliceHandle::Empty,
            function_type_params: SliceHandle::Empty,
            static_type_params: SliceHandle::Empty,
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
            dyn_fn_id: None,
        });
        let lambda_type_id = self.types.add_lambda(
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
        match_expr_id: ParsedExprId,
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
        let match_subject_ident = self.ast.idents.intern("match_subject");
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
            condition: MatchingCondition {
                patterns: smallvec![],
                instrs: eco_vec![],
                binding_eligible: true,
                diverges: false,
            },
            consequent_expr: self.synth_crash_call(
                "Match Error",
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
                if conseqent_type != NEVER_TYPE_ID { Some(conseqent_type) } else { None }
            })
            .unwrap_or(NEVER_TYPE_ID);
        Ok(self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: eco_vec![match_subject_variable.defn_stmt],
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
    ) -> TyperResult<EcoVec<TypedMatchArm>> {
        let mut typed_arms: EcoVec<TypedMatchArm> = EcoVec::new();

        let mut expected_arm_type_id = ctx.expected_type_id;
        let match_scope_id = ctx.scope_id;
        let mut all_unguarded_patterns = Vec::with_capacity(cases.len());
        let target_expr_type = self.exprs.get(target_expr).get_type();
        let target_expr_span = self.exprs.get(target_expr).get_span();
        for parsed_case in cases.iter() {
            let arm_expr_span = self.ast.exprs.get_span(parsed_case.expression);
            let mut arm_patterns = Vec::with_capacity(parsed_case.patterns.len());
            let multi_pattern = parsed_case.patterns.len() > 1;
            let mut expected_bindings: Option<SmallVec<[VariablePattern; 8]>> = None;
            for pattern_id in parsed_case.patterns.iter() {
                let pattern = self.eval_pattern(
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
                            if this_pattern_bindings.is_empty() && !expected_bindings.is_empty() {
                                return failf!(
                                    pattern.span_id(),
                                    "Patterns in a multiple pattern arm must have the exact same bindings; but this one has none"
                                );
                            }
                            for (exp_binding, this_binding) in
                                expected_bindings.iter().zip(this_pattern_bindings.iter())
                            {
                                if exp_binding.name != this_binding.name {
                                    return failf!(
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings"
                                    );
                                }
                                if exp_binding.type_id != this_binding.type_id {
                                    return failf!(
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings; but the type differs for {}: {} vs {}",
                                        self.ident_str(exp_binding.name),
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

            // Note: We compile the arm expression and the guard condition as many times as there are patterns, since each
            // one has its own scope. To get around this we'd have to create only one compiled arm even for
            // multi-pattern binding arms, and have the condition be a boolean OR of the various
            // arms, and somehow compile in the right variables defns based on which one passed.
            // Which isn't possible to know at compile time. So I think this is just where we are.
            // It'd be nice to re-use the typed expr across different scopes, but we can't do that
            for pattern in arm_patterns.into_iter() {
                let arm_scope_id =
                    self.scopes.add_child_scope(match_scope_id, ScopeType::MatchArm, None, None);
                //let mut setup_statements = smallvec![];
                //let mut binding_statements = smallvec![];
                let mut instrs = eco_vec![];
                self.compile_pattern_into_values(
                    &pattern,
                    target_expr,
                    &mut instrs,
                    //&mut setup_statements,
                    //&mut binding_statements,
                    false,
                    arm_scope_id,
                )?;

                match parsed_case.guard_condition_expr {
                    None => {}
                    Some(guard_condition_expr_id) => {
                        let guard_condition_expr = self.eval_expr(
                            guard_condition_expr_id,
                            ctx.with_scope(arm_scope_id).with_expected_type(Some(BOOL_TYPE_ID)),
                        )?;
                        instrs.push(MatchingConditionInstr::Cond { value: guard_condition_expr });
                    }
                };

                // Once we've evaluated the conditions, we can eval the consequent expression inside of it,
                // since the bindings are now available
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

                let condition_diverges = self.matching_condition_diverges(&instrs);

                typed_arms.push(TypedMatchArm {
                    condition: MatchingCondition {
                        patterns: smallvec![pattern],
                        instrs,
                        binding_eligible: true,
                        diverges: condition_diverges,
                    },
                    consequent_expr,
                });
            }
        }

        // Exhaustiveness Checking
        if !partial_match {
            let trial_constructors: Vec<PatternCtor> =
                self.generate_constructors_for_type(target_expr_type, target_expr_span);
            let mut trial_alives: Vec<bool> = vec![true; trial_constructors.len()];
            let mut pattern_kill_counts: Vec<usize> = vec![0; all_unguarded_patterns.len()];
            'trial: for (trial_index, trial_expr) in trial_constructors.iter().enumerate() {
                '_pattern: for (index, pattern) in all_unguarded_patterns.iter().enumerate() {
                    if TypedProgram::pattern_matches(pattern, trial_expr) {
                        pattern_kill_counts[index] += 1;
                        trial_alives[trial_index] = false;
                        continue 'trial;
                    }
                }
            }

            let alive_indexes: SV4<usize> = trial_alives
                .iter()
                .enumerate()
                .filter(|(_i, alive)| **alive)
                .map(|(i, _alive)| i)
                .collect();
            if !alive_indexes.is_empty() {
                let patterns: String = alive_indexes
                    .iter()
                    .map(|i| self.pattern_ctor_to_string(&trial_constructors[*i]))
                    .join("\n- ");
                return failf!(
                    target_expr_span,
                    "{} Unhandled patterns:\n- {}",
                    alive_indexes.len(),
                    patterns
                );
            }

            if let Some(useless_index) = pattern_kill_counts.iter().position(|p| *p == 0) {
                // patterns[0]: For actual match expressions, which this is, we'll always have
                // exactly 1 pattern per arm
                let pattern = &typed_arms[useless_index].condition.patterns[0];
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

    /// Accumulates a list of 'BindingOrCond' while 'compiling' a pattern match.
    /// Basically, every part of a pattern match boils down to either
    /// - A boolean condition to be evaluated
    /// - A new variable binding
    fn compile_pattern_into_values(
        &mut self,
        pattern: &TypedPattern,
        target_expr: TypedExprId,
        instrs: &mut EcoVec<MatchingConditionInstr>,
        is_immediately_inside_reference_pattern: bool,
        arm_scope_id: ScopeId,
    ) -> TyperResult<()> {
        let target_expr_type = self.exprs.get(target_expr).get_type();
        match pattern {
            TypedPattern::Struct(struct_pattern) => {
                //let mut boolean_exprs: Vec<TypedExprId> =
                //    Vec::with_capacity(struct_pattern.fields.len());
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
                            field_index: pattern_field.field_index,
                            result_type,
                            struct_type,
                            is_referencing,
                            span: struct_pattern.span,
                        }));
                    let var_name = self
                        .ast
                        .idents
                        .intern(format!("field_{}", self.ident_str(pattern_field.name)));
                    let struct_field_variable =
                        self.synth_variable_defn_simple(var_name, get_struct_field, arm_scope_id);
                    instrs.push(MatchingConditionInstr::Binding {
                        let_stmt: struct_field_variable.defn_stmt,
                        variable_id: struct_field_variable.variable_id,
                    });
                    self.compile_pattern_into_values(
                        &pattern_field.pattern,
                        struct_field_variable.variable_expr,
                        instrs,
                        is_referencing,
                        arm_scope_id,
                    )?;
                    //boolean_exprs.push(condition);
                }
                //let final_condition = boolean_exprs
                //    .into_iter()
                //    .reduce(|a, b| self.synth_binary_bool_op(BinaryOpKind::And, a, b))
                //    .unwrap();
                Ok(())
            }
            TypedPattern::Enum(enum_pattern) => {
                let is_referencing = is_immediately_inside_reference_pattern;
                let is_variant_target =
                    if is_referencing { self.synth_dereference(target_expr) } else { target_expr };
                let is_variant_condition =
                    self.exprs.add(TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                        enum_expr: is_variant_target,
                        variant_index: enum_pattern.variant_index,
                        span: enum_pattern.span,
                    }));
                instrs.push(MatchingConditionInstr::Cond { value: is_variant_condition });

                if let Some(payload_pattern) = enum_pattern.payload.as_ref() {
                    let enum_type = self.types.get(enum_pattern.enum_type_id).expect_enum();
                    let variant = enum_type.variant_by_index(enum_pattern.variant_index);
                    let variant_type_id = variant.my_type_id;
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
                    let enum_as_variant = self.exprs.add(TypedExpr::Cast(TypedCast {
                        cast_type: if is_referencing {
                            CastType::ReferenceToReference
                        } else {
                            CastType::EnumToVariant
                        },
                        target_type_id: variant_type_id,
                        base_expr: target_expr,
                        span: enum_pattern.span,
                    }));
                    let get_payload_expr =
                        self.exprs.add(TypedExpr::EnumGetPayload(GetEnumVariantPayload {
                            enum_variant_expr: enum_as_variant,
                            result_type_id,
                            variant_index,
                            is_referencing,
                            span: enum_pattern.span,
                        }));
                    let var_name =
                        self.ast.idents.intern(format!("payload_{}", self.ident_str(variant_name)));
                    let payload_variable =
                        self.synth_variable_defn_simple(var_name, get_payload_expr, arm_scope_id);
                    instrs.push(MatchingConditionInstr::Binding {
                        let_stmt: payload_variable.defn_stmt,
                        variable_id: payload_variable.variable_id,
                    });
                    //setup_statements.push(payload_variable.defn_stmt);
                    self.compile_pattern_into_values(
                        payload_pattern,
                        payload_variable.variable_expr,
                        instrs,
                        is_referencing,
                        arm_scope_id,
                    )?;
                };
                //let final_condition = match inner_condition.map(|expr_id| self.exprs.get(expr_id)) {
                //    None => is_variant_condition,
                //    Some(TypedExpr::Bool(true, _)) => is_variant_condition,
                //    Some(_inner_condition) => self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
                //        kind: BinaryOpKind::And,
                //        ty: BOOL_TYPE_ID,
                //        lhs: is_variant_condition,
                //        rhs: inner_condition.unwrap(),
                //        span: enum_pattern.span,
                //    })),
                //};
                Ok(())
            }
            TypedPattern::Variable(variable_pattern) => {
                let variable_ident = variable_pattern.name;
                let binding_variable =
                    self.synth_variable_defn_visible(variable_ident, target_expr, arm_scope_id);
                //binding_statements.push(binding_variable.defn_stmt);
                instrs.push(MatchingConditionInstr::Binding {
                    let_stmt: binding_variable.defn_stmt,
                    variable_id: binding_variable.variable_id,
                });
                // Don't even need a condition?!?! would just be if false goto next???
                //Ok(self.exprs.add(TypedExpr::Bool(true, variable_pattern.span)))
                Ok(())
            }
            TypedPattern::Wildcard(_span) => Ok(()),
            TypedPattern::Reference(reference_pattern) => {
                let target_expr = if is_immediately_inside_reference_pattern {
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                self.compile_pattern_into_values(
                    &reference_pattern.inner_pattern,
                    target_expr,
                    instrs,
                    true,
                    arm_scope_id,
                )?;
                Ok(())
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
                    TypedPattern::LiteralUnit(_span) => Ok(()),
                    TypedPattern::LiteralChar(byte, span) => {
                        let char_expr = self.exprs.add(TypedExpr::Char(*byte, *span));
                        let equals_pattern_char =
                            self.synth_equals_binop(target_expr, char_expr, *span);
                        instrs.push(MatchingConditionInstr::Cond { value: equals_pattern_char });
                        Ok(())
                    }
                    TypedPattern::LiteralInteger(int_value, span) => {
                        let equals_pattern_int = BinaryOp {
                            kind: BinaryOpKind::Equals,
                            ty: BOOL_TYPE_ID,
                            lhs: target_expr,
                            rhs: self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                                value: *int_value,
                                span: *span,
                            })),
                            span: *span,
                        };
                        let equals_pattern_int =
                            self.exprs.add(TypedExpr::BinaryOp(equals_pattern_int));
                        instrs.push(MatchingConditionInstr::Cond { value: equals_pattern_int });
                        Ok(())
                    }
                    TypedPattern::LiteralFloat(float_value, span) => {
                        let equals_pattern_char = BinaryOp {
                            kind: BinaryOpKind::Equals,
                            ty: BOOL_TYPE_ID,
                            lhs: target_expr,
                            rhs: self.exprs.add(TypedExpr::Float(TypedFloatExpr {
                                value: *float_value,
                                span: *span,
                            })),
                            span: *span,
                        };
                        let equals_pattern_char =
                            self.exprs.add(TypedExpr::BinaryOp(equals_pattern_char));
                        instrs.push(MatchingConditionInstr::Cond { value: equals_pattern_char });
                        Ok(())
                    }
                    TypedPattern::LiteralBool(bool_value, span) => {
                        let bool_expr = self.exprs.add(TypedExpr::Bool(*bool_value, *span));
                        let equals_pattern_bool =
                            self.synth_equals_binop(target_expr, bool_expr, *span);
                        instrs.push(MatchingConditionInstr::Cond { value: equals_pattern_bool });
                        Ok(())
                    }
                    TypedPattern::LiteralString(string_id, span) => {
                        let string_expr = self.exprs.add(TypedExpr::String(*string_id, *span));
                        let condition =
                            self.synth_equals_call(target_expr, string_expr, arm_scope_id, *span)?;
                        instrs.push(MatchingConditionInstr::Cond { value: condition });
                        Ok(())
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
        _expr_id: ParsedExprId,
        cast: ParsedCast,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let base_expr = self.eval_expr(cast.base_expr, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get(base_expr).get_type();
        let target_type = self.eval_type_expr(cast.dest_type, ctx.scope_id)?;
        if base_expr_type == target_type {
            return failf!(cast.span, "Useless cast");
        }
        let cast_type = match self.types.get_no_follow_static(base_expr_type) {
            Type::Integer(from_integer_type) => match self.types.get(target_type) {
                Type::Integer(to_integer_type) => {
                    let cast_type = match from_integer_type.width().cmp(&to_integer_type.width()) {
                        Ordering::Less => CastType::IntegerCast(IntegerCastDirection::Extend),
                        Ordering::Greater => CastType::IntegerCast(IntegerCastDirection::Truncate),
                        // Likely a sign change
                        Ordering::Equal => CastType::IntegerCast(IntegerCastDirection::NoOp),
                    };
                    Ok(cast_type)
                }
                Type::Char => {
                    if from_integer_type.width() == NumericWidth::B8 {
                        Ok(CastType::Integer8ToChar)
                    } else {
                        failf!(
                            cast.span,
                            "Cannot cast integer '{}' to char, must be 8 bits",
                            from_integer_type
                        )
                    }
                }
                Type::Pointer => {
                    if matches!(from_integer_type, IntegerType::UWord(_)) {
                        Ok(CastType::IntegerToPointer)
                    } else {
                        failf!(
                            cast.span,
                            "Cannot cast integer '{}' to Pointer (must be uword)",
                            from_integer_type
                        )
                    }
                }
                Type::Float(_to_float_type) => {
                    // We're just going to allow these casts and make it UB if it doesn't fit, the LLVM
                    // default. If I find a saturating version in LLVM I'll use that instead
                    Ok(CastType::IntegerToFloat)
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
                    match from_float_type.size().cmp(&to_float_type.size()) {
                        Ordering::Less => Ok(CastType::FloatExtend),
                        Ordering::Greater => Ok(CastType::FloatTruncate),
                        Ordering::Equal => failf!(cast.span, "Useless float cast"),
                    }
                }
                Type::Integer(to_int_type) => match to_int_type {
                    IntegerType::U32 => Ok(CastType::FloatToInteger),
                    IntegerType::U64 => Ok(CastType::FloatToInteger),
                    IntegerType::I32 => Ok(CastType::FloatToInteger),
                    IntegerType::I64 => Ok(CastType::FloatToInteger),
                    _ => failf!(
                        cast.span,
                        "Cannot cast float to integer '{}'",
                        self.type_id_to_string(target_type).blue()
                    ),
                },
                _ => failf!(
                    cast.span,
                    "Cannot cast float to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Char => match self.types.get(target_type) {
                Type::Integer(_to_integer_type) => Ok(CastType::IntegerExtendFromChar),
                _ => failf!(
                    cast.span,
                    "Cannot cast char to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Reference(_refer) => match self.types.get(target_type) {
                Type::Pointer => Ok(CastType::ReferenceToPointer),
                Type::Reference(_) => Ok(CastType::ReferenceToReference),
                _ => failf!(
                    cast.span,
                    "Cannot cast reference to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Pointer => match self.types.get(target_type) {
                Type::Reference(_refer) => Ok(CastType::PointerToReference),
                Type::Integer(IntegerType::UWord(_)) => Ok(CastType::PointerToWord),
                Type::Integer(IntegerType::IWord(_)) => Ok(CastType::PointerToWord),
                _ => failf!(
                    cast.span,
                    "Cannot cast Pointer to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Static(stat) => {
                if target_type == stat.inner_type_id {
                    Ok(CastType::StaticErase)
                } else {
                    failf!(
                        cast.span,
                        "Cannot cast static '{}' to '{}'",
                        self.type_id_to_string(base_expr_type).blue(),
                        self.type_id_to_string(target_type).blue()
                    )
                }
            }
            _ => failf!(
                cast.span,
                "Cannot cast '{}' to '{}'",
                self.type_id_to_string(base_expr_type).blue(),
                self.type_id_to_string(target_type).blue()
            ),
        }?;
        Ok(self.exprs.add(TypedExpr::Cast(TypedCast {
            base_expr,
            target_type_id: target_type,
            cast_type,
            span: cast.span,
        })))
    }

    fn eval_for_expr(
        &mut self,
        for_expr: &ForExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let binding_ident = for_expr.binding.unwrap_or(self.ast.idents.builtins.it);
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, ctx.with_no_expected_type())?;
        let iterable_type = self.exprs.get(iterable_expr).get_type();
        let iterable_span = self.exprs.get(iterable_expr).get_span();
        let body_span = for_expr.body_block.span;

        let (target_is_iterator, item_type) = match self.expect_ability_implementation(
            iterable_type,
            ITERABLE_ABILITY_ID,
            ctx.scope_id,
            iterable_span,
        ) {
            Err(_not_iterable) => {
                match self.expect_ability_implementation(
                    iterable_type,
                    ITERATOR_ABILITY_ID,
                    ctx.scope_id,
                    iterable_span,
                ) {
                    Err(_not_iterator) => {
                        return failf!(
                            iterable_span,
                            "`for` loop target must be Iterable or an Iterator"
                        );
                    }
                    Ok(iterator_impl) => {
                        let impl_args =
                            self.ability_impls.get(iterator_impl.full_impl_id).impl_arguments;
                        let item_type = self.named_types.get_nth(impl_args, 0).type_id;
                        (true, item_type)
                    }
                }
            }
            Ok(iterable_impl) => {
                let impl_args = self.ability_impls.get(iterable_impl.full_impl_id).impl_arguments;
                let item_type = self.named_types.get_nth(impl_args, 0).type_id;
                (false, item_type)
            }
        };

        let is_do_block = for_expr.expr_type == ForExprType::Do;

        // We de-sugar the 'for ... do' expr into a typed while loop, synthesizing
        // a few local variables in order to achieve this.

        let outer_for_expr_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::ForExpr, None, None);

        let zero_expr = self.synth_uword(0, for_expr.body_block.span);
        let index_variable = self.synth_variable_defn(
            self.ast.idents.builtins.it_index,
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
            self.ast.idents.builtins.iter,
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
            self.instantiate_generic_type(LIST_TYPE_ID, smallvec![body_block_result_type])
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
                field_index: 0,
                result_type: UWORD_TYPE_ID,
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
        let if_next_loop_else_break_expr = self.synth_if_else(
            smallvec![],
            UNIT_TYPE_ID,
            next_is_some_call,
            consequent_block_id,
            break_block_id,
            body_span,
        );
        self.add_expr_id_to_block(&mut loop_block, if_next_loop_else_break_expr);

        // Append the index increment to the body block
        let one_expr = self.synth_uword(1, iterable_span);
        let add_operation = self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
            kind: BinaryOpKind::Add,
            ty: UWORD_TYPE_ID,
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

        let body_block = self.exprs.add(TypedExpr::Block(loop_block));
        let loop_expr = TypedExpr::LoopExpr(LoopExpr {
            body_block,
            break_type: UNIT_TYPE_ID,
            span: for_expr.span,
        });

        let mut for_expr_initial_statements = EcoVec::with_capacity(4);
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
        base_ability_id: AbilityId,
        scope_id: ScopeId,
        span_for_error: SpanId,
    ) -> TyperResult<AbilityImplHandle> {
        self.find_ability_impl_for_type_or_generate_new(
            type_id,
            base_ability_id,
            &[],
            scope_id,
            span_for_error,
        )
        .map_err(|msg| {
            errf!(
                span_for_error,
                "Missing ability '{}' for '{}': {msg}. It implements the following abilities:\n{}",
                self.ident_str(self.abilities.get(base_ability_id).name),
                self.type_id_to_string(type_id),
                &self
                    .ability_impl_table
                    .get(&type_id)
                    .unwrap_or(&vec![])
                    .iter()
                    .map(|h| self.ability_impl_signature_to_string(
                        h.specialized_ability_id,
                        self.ability_impls.get(h.full_impl_id).impl_arguments
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        })
    }

    fn eval_static_if_expr(
        &mut self,
        if_expr: &ParsedIfExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        // We just proceed as if it yielded 'true' in the generic case
        let condition_bool = match ctx.is_generic_pass {
            false => self.execute_static_bool(if_expr.cond, ctx)?,
            true => true,
        };

        // Should we still compile both expressions; I think all the code should get typechecked
        // even if the path isn't taken? Or does this prevent important use-cases like
        // platform-dependent code?
        //let cons_expr = self.eval_expr(if_expr.cons, ctx)?;
        //let alt_expr =
        //    if let Some(alt) = if_expr.alt { Some(self.eval_expr(alt, ctx)?) } else { None };

        let expr = if condition_bool {
            let cons_expr = self.eval_expr(if_expr.cons, ctx)?;
            cons_expr
        } else {
            let alt_expr =
                if let Some(alt) = if_expr.alt { Some(self.eval_expr(alt, ctx)?) } else { None };
            if let Some(alt) = alt_expr {
                alt
            } else {
                self.exprs.add(TypedExpr::Unit(if_expr.span))
            }
        };
        Ok(expr)
    }

    // "if" in k1 can do pattern matching, on multiple targets, chained with arbitrary boolean
    // expressions, so this is not a simple one.
    // if x is .Some(v) and y is .Some("bar") and foo == 3
    fn eval_if_expr(
        &mut self,
        if_expr: &ParsedIfExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        if if_expr.is_static {
            return self.eval_static_if_expr(if_expr, ctx);
        }
        let match_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);

        let condition = self.eval_matching_condition(
            if_expr.cond,
            ctx.with_scope(match_scope_id).with_no_expected_type(),
        )?;

        let consequent = self.eval_expr(if_expr.cons, ctx.with_scope(match_scope_id))?;
        let consequent_type = self.exprs.get(consequent).get_type();

        let cons_never = consequent_type == NEVER_TYPE_ID;

        // if without else:
        // If there is no alternate, we coerce the consequent to return Unit, so both
        // branches have a matching type, making codegen simpler
        // However, if the consequent is a never type (does not return), we don't need to do this, in
        // fact we can't because then we'd have an expression following a never expression
        let consequent = if if_expr.alt.is_none() && !cons_never && consequent_type != UNIT_TYPE_ID
        {
            self.synth_discard_call(consequent, ctx.with_no_expected_type())?
        } else {
            consequent
        };
        let consequent_type = self.exprs.get(consequent).get_type();
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
            if cons_never { alternate_type } else { consequent_type }
        };

        let cons_arm = TypedMatchArm { condition, consequent_expr: consequent };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition {
                patterns: smallvec![],
                instrs: eco_vec![],
                binding_eligible: true,
                diverges: false,
            },
            consequent_expr: alternate,
        };
        Ok(self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: eco_vec![],
            result_type: overall_type,
            arms: eco_vec![cons_arm, alt_arm],
            span: if_expr.span,
        })))
    }

    fn eval_matching_condition(
        &mut self,
        condition: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<MatchingCondition> {
        let mut all_patterns: SmallVec<[TypedPattern; 1]> = smallvec![];
        let mut allow_bindings: bool = true;
        let mut instrs: EcoVec<MatchingConditionInstr> = EcoVec::new();
        self.handle_matching_condition_rec(
            condition,
            &mut allow_bindings,
            &mut all_patterns,
            &mut instrs,
            ctx,
        )?;

        let mut all_bindings: SmallVec<[VariablePattern; 8]> = SmallVec::new();
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
                    self.ident_str(dupe_binding[1].name)
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

        let diverges = self.matching_condition_diverges(&instrs);

        Ok(MatchingCondition {
            patterns: all_patterns,
            instrs,
            binding_eligible: allow_bindings,
            diverges,
        })
    }

    fn matching_condition_diverges(&self, instrs: &[MatchingConditionInstr]) -> bool {
        for instr in instrs {
            match instr {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    if self.get_stmt_type(*let_stmt) == NEVER_TYPE_ID {
                        return true;
                    }
                }
                MatchingConditionInstr::Cond { value } => {
                    if self.exprs.get(*value).get_type() == NEVER_TYPE_ID {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Handles chains of booleans and pattern statements (IsExprs).
    /// Does so by compiling the patterns (or boolean conditions) contained
    /// in them into `conditions`, `setup_statements`, and `binding_statements`.
    ///
    /// Stores all patterns seen for later analysis (conflicting bindings)
    /// Reports whether bindings are allowed based on the following rule:
    /// At the top-level of the 'if', if there are any 'or's, we cannot allow patterns, because it
    /// makes no sense:
    /// if a is .Some(aa) or b is .Some(aa) -> which one?
    /// if a is .Some(aa) or b is .Some(bb) -> which one?
    fn handle_matching_condition_rec(
        &mut self,
        parsed_expr_id: ParsedExprId,
        allow_bindings: &mut bool,
        all_patterns: &mut SmallVec<[TypedPattern; 1]>,
        instrs: &mut EcoVec<MatchingConditionInstr>,
        ctx: EvalExprContext,
    ) -> TyperResult<()> {
        debug!("hmirec {allow_bindings}: {}", self.ast.expr_id_to_string(parsed_expr_id));
        match self.ast.exprs.get(parsed_expr_id) {
            ParsedExpr::Is(is_expr) => {
                let target_expr = is_expr.target_expression;
                let pattern = is_expr.pattern;
                let target = self.eval_expr(target_expr, ctx)?;
                let target_type = self.exprs.get(target).get_type();
                let target_var = self.synth_variable_defn_simple(
                    self.ast.idents.builtins.if_target,
                    target,
                    ctx.scope_id,
                );
                let pattern =
                    self.eval_pattern(pattern, target_type, ctx.scope_id, *allow_bindings)?;
                instrs.push(MatchingConditionInstr::Binding {
                    let_stmt: target_var.defn_stmt,
                    variable_id: target_var.variable_id,
                });
                self.compile_pattern_into_values(
                    &pattern,
                    target_var.variable_expr,
                    instrs,
                    false,
                    ctx.scope_id,
                )?;
                all_patterns.push(pattern);

                Ok(())
            }
            ParsedExpr::BinaryOp(binary_op) if binary_op.op_kind == BinaryOpKind::And => {
                let rhs = binary_op.rhs;
                // It's important that the lhs comes first
                // because expressions to the right can see bindings from
                // expressions on the left, and the order of execution, and
                // short-circuiting, is guaranteed
                self.handle_matching_condition_rec(
                    binary_op.lhs,
                    allow_bindings,
                    all_patterns,
                    instrs,
                    ctx,
                )?;
                self.handle_matching_condition_rec(rhs, allow_bindings, all_patterns, instrs, ctx)?;
                Ok(())
            }
            other => {
                let is_or_binop =
                    matches!(other, ParsedExpr::BinaryOp(b) if b.op_kind == BinaryOpKind::Or);
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
                instrs.push(MatchingConditionInstr::Cond { value: condition });
                Ok(())
            }
        }
    }

    fn eval_binary_op(
        &mut self,
        binary_op_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        fn is_scalar_for_equals(type_id: TypeId) -> bool {
            match type_id {
                UNIT_TYPE_ID | CHAR_TYPE_ID | U8_TYPE_ID | U16_TYPE_ID | U32_TYPE_ID
                | U64_TYPE_ID | I8_TYPE_ID | I16_TYPE_ID | I32_TYPE_ID | I64_TYPE_ID
                | UWORD_TYPE_ID | IWORD_TYPE_ID | BOOL_TYPE_ID => true,
                F32_TYPE_ID | F64_TYPE_ID => true,
                _other => false,
            }
        }
        let ParsedExpr::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };

        // if ctx.is_static {
        //     if binary_op.op_kind == BinaryOpKind::Equals {
        //         let lhs = self.eval_expr(binary_op.lhs, ctx.with_no_expected_type())?;
        //         let lhs_type = self.exprs.get(lhs).get_type();
        //         let rhs = self.eval_expr(binary_op.rhs, ctx.with_expected_type(Some(lhs_type)))?;
        //         let rhs_type = self.exprs.get(rhs).get_type();
        //         if let Err(msg) = self.check_types(lhs_type, rhs_type, ctx.scope_id) {
        //             return failf!(binary_op.span, "comptime equals type mismatch: {msg}");
        //         }
        //         return Ok(self.exprs.add(TypedExpr::BinaryOp(BinaryOp {
        //             kind: BinaryOpKind::Equals,
        //             ty: BOOL_TYPE_ID,
        //             lhs,
        //             rhs,
        //             span: binary_op.span,
        //         })));
        //     }
        // }

        // Special cases: Equality, OptionalElse, and Pipe
        match binary_op.op_kind {
            BinaryOpKind::Pipe => {
                return self.eval_pipe_expr(binary_op.lhs, binary_op.rhs, ctx, binary_op.span);
            }
            BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                let lhs = self.eval_expr(binary_op.lhs, ctx.with_no_expected_type())?;
                let lhs_type = self.exprs.get(lhs).get_type();
                if !is_scalar_for_equals(lhs_type) {
                    return self.eval_equality_expr(binary_op_id, lhs, ctx.with_no_expected_type());
                }
            }
            BinaryOpKind::OptionalElse => {
                return self.eval_optional_else(binary_op.lhs, binary_op.rhs, ctx, binary_op.span);
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
            Type::Never => Ok(NEVER_TYPE_ID),
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
            Type::Bool => match kind {
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
            Type::Char => match kind {
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
            Type::Unit => match kind {
                BinaryOpKind::Equals => Ok(BOOL_TYPE_ID),
                BinaryOpKind::NotEquals => Ok(BOOL_TYPE_ID),
                _ => failf!(binary_op.span, "Invalid operation on unit: {}", kind),
            },
            _ => {
                failf!(
                    binary_op.span,
                    "Invalid left-hand side of binary operation {}: {}",
                    kind,
                    self.type_id_to_string(lhs_type)
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
        lhs: ParsedExprId,
        rhs: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        // LHS must implement Unwrap and RHS must be its contained type
        let lhs = self.eval_expr(lhs, ctx.with_no_expected_type())?;
        let lhs_type = self.exprs.get(lhs).get_type();
        let unwrap_impl = self
            .expect_ability_implementation(lhs_type, UNWRAP_ABILITY_ID, ctx.scope_id, span)
            .map_err(|e| {
                errf!(
                    span,
                    "'?' operator can only be used on a type that implements `Unwrap`. {}",
                    e.message,
                )
            })?;
        let unwrap_impl = self.ability_impls.get(unwrap_impl.full_impl_id);
        let output_type = self.named_types.get_nth(unwrap_impl.impl_arguments, 0).type_id;

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

        let if_else =
            self.synth_if_else(smallvec![], output_type, lhs_has_value, lhs_get_expr, rhs, span);
        self.push_block_stmt_id(&mut coalesce_block, lhs_variable.defn_stmt);
        self.add_expr_id_to_block(&mut coalesce_block, if_else);
        Ok(self.exprs.add(TypedExpr::Block(coalesce_block)))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op_id: ParsedExprId,
        lhs: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };

        let lhs_type_id = self.exprs.get(lhs).get_type();
        let lhs_type = self.types.get(lhs_type_id);
        if lhs_type.is_scalar_int_value() {
            panic!("Scalar ints shouldnt be passed to eval_equality_expr")
        }
        let rhs = self.eval_expr(binary_op.rhs, ctx.with_expected_type(Some(lhs_type_id)))?;
        let rhs_type = self.exprs.get(rhs).get_type();
        let equality_result = if rhs_type != lhs_type_id {
            failf!(
                binary_op.span,
                "Right hand side type '{}' did not match {}",
                self.type_id_to_string(rhs_type),
                self.type_id_to_string(lhs_type_id)
            )
        } else {
            let call_expr = self.synth_equals_call(lhs, rhs, ctx.scope_id, binary_op.span)?;
            Ok(call_expr)
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
        lhs: ParsedExprId,
        rhs: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let new_fn_call = match self.ast.exprs.get(rhs) {
            ParsedExpr::Variable(var) => {
                let args = self
                    .ast
                    .p_call_args
                    .add_slice_from_iter([ParsedCallArg::unnamed(lhs)].into_iter());
                ParsedCall {
                    name: var.name.clone(),
                    type_args: SliceHandle::Empty,
                    args,
                    span,
                    is_method: false,
                    id: ParsedExprId::PENDING,
                }
            }
            ParsedExpr::Call(fn_call) => {
                let mut args: SV8<ParsedCallArg> = SmallVec::with_capacity(fn_call.args.len() + 1);
                args.push(ParsedCallArg::unnamed(lhs));
                args.extend_from_slice(self.ast.p_call_args.get_slice(fn_call.args));
                let args_with_piped = self.ast.p_call_args.add_slice_from_copy_slice(&args);
                ParsedCall {
                    name: fn_call.name.clone(),
                    type_args: fn_call.type_args,
                    args: args_with_piped,
                    span,
                    is_method: false,
                    id: ParsedExprId::PENDING,
                }
            }
            _ => {
                return failf!(
                    self.ast.exprs.get_span(rhs),
                    "rhs of pipe must be function call or function name",
                );
            }
        };
        let new_fn_call_id = self.ast.exprs.add_expression(ParsedExpr::Call(new_fn_call));
        let new_fn_call_clone = self.ast.exprs.get(new_fn_call_id).expect_call().clone();
        self.eval_function_call(&new_fn_call_clone, None, ctx, None)
    }

    /// Can 'shortcircuit' with Left if the function call to resolve
    /// is actually a builtin
    fn resolve_parsed_function_call(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        let call_span = fn_call.span;
        if let Some(builtin_result) = self.handle_builtin_function_call_lookalikes(fn_call, ctx)? {
            return Ok(Either::Left(builtin_result));
        }

        let self_arg_expr: Option<MaybeTypedExpr> = match known_args.as_ref() {
            Some((_, args)) if !args.is_empty() => {
                Some(MaybeTypedExpr::Typed(*args.first().unwrap()))
            }
            _ => match self
                .ast
                .p_call_args
                .get_slice(fn_call.args)
                .iter()
                .find(|a| !a.is_explicit_context)
            {
                None => None,
                Some(first) => Some(MaybeTypedExpr::Parsed(first.value)),
            },
        };

        // Two resolution paths:
        // 1. "method" call (aka known first arg type so we can check for ability impls and methods)
        // 2. "function" call, no abilities or methods to check, pure scoping-based resolution

        match fn_call.is_method {
            true => self.resolve_parsed_function_call_method(
                self_arg_expr.unwrap(),
                fn_call,
                known_args,
                ctx,
            ),
            false => {
                if let Some(function_id) = self.scopes.find_function_namespaced(
                    ctx.scope_id,
                    &fn_call.name,
                    &self.namespaces,
                    &self.ast.idents,
                )? {
                    if let Some(function_ability_id) =
                        self.get_function(function_id).kind.ability_id()
                    {
                        let function_ability_index = self
                            .abilities
                            .get(function_ability_id)
                            .find_function_by_name(fn_call.name.name)
                            .unwrap()
                            .0;
                        let function_type_id = self.get_function(function_id).type_id;
                        let ability_impl_function = self.resolve_ability_call(
                            function_type_id,
                            function_ability_index,
                            function_ability_id,
                            fn_call,
                            known_args,
                            ctx,
                        )?;
                        Ok(Either::Right(Callee::from_ability_impl_fn(ability_impl_function)))
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
                            self.ident_str(fn_call.name.name)
                        )
                    };
                    if !fn_call.name.namespaces.is_empty() {
                        return fn_not_found();
                    }
                    if let Some((variable_id, _scope_id)) =
                        self.scopes.find_variable(ctx.scope_id, fn_call.name.name)
                    {
                        let function_variable = self.variables.get(variable_id);
                        debug!(
                            "Variable {} has type {}",
                            self.ident_str(fn_call.name.name),
                            self.type_id_to_string(function_variable.type_id)
                        );
                        match self.types.get(function_variable.type_id) {
                            Type::Lambda(lambda_type) => Ok(Either::Right(Callee::StaticLambda {
                                function_id: lambda_type.body_function_id,
                                lambda_value_expr: self.exprs.add(TypedExpr::Variable(
                                    VariableExpr {
                                        variable_id,
                                        type_id: function_variable.type_id,
                                        span: fn_call.span,
                                    },
                                )),
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
                            Type::FunctionTypeParameter(ftp) => {
                                let callee = Callee::DynamicAbstract {
                                    function_sig: FunctionSignature::make_no_generics(
                                        Some(ftp.name),
                                        ftp.function_type,
                                    ),
                                    variable_id,
                                };
                                Ok(Either::Right(callee))
                            }
                            Type::Reference(function_reference) => {
                                if self
                                    .types
                                    .get(function_reference.inner_type)
                                    .as_function()
                                    .is_some()
                                {
                                    let function_reference_expr =
                                        self.exprs.add(TypedExpr::Variable(VariableExpr {
                                            variable_id,
                                            type_id: function_variable.type_id,
                                            span: fn_call.name.span,
                                        }));
                                    Ok(Either::Right(Callee::DynamicFunction {
                                        function_reference_expr,
                                    }))
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

    fn get_return_type_for_scope(&self, scope_id: ScopeId, span: SpanId) -> TyperResult<TypeId> {
        if let Some(enclosing_lambda) = self.scopes.nearest_parent_lambda(scope_id) {
            let Some(expected_return_type) =
                self.scopes.get_lambda_info(enclosing_lambda).expected_return_type
            else {
                return failf!(
                    span,
                    "Closure must have explicit return type, or known return type from context, to use early returns."
                );
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
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let expected_return_type = if ctx.is_static() {
            // When _typechecking_, not executing, inside #static blocks
            // The expected return type should just be the expected type of the static block
            ctx.static_ctx.unwrap().expected_return_type
        } else {
            Some(self.get_return_type_for_scope(ctx.scope_id, span)?)
        };
        let return_value =
            self.eval_expr(parsed_expr, ctx.with_expected_type(expected_return_type))?;
        let return_value_type = self.exprs.get(return_value).get_type();
        if return_value_type == NEVER_TYPE_ID {
            return failf!(
                span,
                "return is dead since returned expression is divergent; remove the return"
            );
        }
        if let Some(expected_return_type) = expected_return_type {
            if let Err(msg) =
                self.check_types(expected_return_type, return_value_type, ctx.scope_id)
            {
                return failf!(span, "Returned wrong type: {msg}");
            }
        }
        Ok(self.exprs.add(TypedExpr::Return(TypedReturn { value: return_value, span })))
    }

    fn handle_builtin_function_call_lookalikes(
        &mut self,
        fn_call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> TyperResult<Option<TypedExprId>> {
        let call_span = fn_call.span;
        let calling_scope = ctx.scope_id;
        match self.ident_str(fn_call.name.name) {
            "return" => match fn_call.args.len() {
                0 => {
                    let unit_value_id = self.exprs.add(TypedExpr::Unit(call_span));
                    Ok(Some(self.exprs.add(TypedExpr::Return(TypedReturn {
                        value: unit_value_id,
                        span: call_span,
                    }))))
                }
                1 => {
                    let arg = self.ast.p_call_args.get_first(fn_call.args).unwrap();
                    let return_result = self.eval_return(arg.value, ctx, call_span)?;
                    Ok(Some(return_result))
                }
                _ => failf!(fn_call.span, "return(...) must have 0 or 1 arguments"),
            },
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

                let arg = self.ast.p_call_args.get_first(fn_call.args);
                let break_value = match arg {
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
                                );
                            }
                        }
                    }
                };
                let actual_break_type = self.exprs.get(break_value).get_type();
                if actual_break_type == NEVER_TYPE_ID {
                    return failf!(
                        call_span,
                        "break is dead since returned expression is divergent; consider removing the 'break'"
                    );
                }

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
                let arg = self.ast.p_call_args.get_first(fn_call.args).unwrap();
                let result = self.eval_expr(arg.value, ctx.with_no_expected_type());
                let expr = match result {
                    Err(typer_error) => {
                        let string_id = self.ast.strings.intern(typer_error.message);
                        self.synth_optional_some(TypedExpr::String(string_id, call_span)).0
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
        fn_call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        debug_assert!(fn_call.name.namespaces.is_empty());
        let fn_name = fn_call.name.name;
        let call_span = fn_call.span;

        let args = self.ast.p_call_args.get_slice(fn_call.args);
        let first_arg = args.first().copied();
        let second_arg = args.get(1).copied();

        // Special cases of this syntax that aren't really method calls
        if let Some(base_arg) = first_arg {
            if let Some(enum_constr) = self.handle_enum_constructor(
                Some(base_arg.value),
                fn_name,
                second_arg.map(|param| param.value),
                fn_call.type_args,
                ctx,
                fn_call.span,
            )? {
                return Ok(Either::Left(enum_constr));
            }

            let is_fn_convert = fn_name == self.ast.idents.builtins.toRef
                || fn_name == self.ast.idents.builtins.toDyn;
            if is_fn_convert {
                // TODO: this isn't algebraically sound since you can _only_ use toRef and toDyn
                //       if you literally name the function on the lhs of the dot; you can't store
                //       it in a variable, because the function name on its own isn't a valid
                //       expression. So it's not the most satisfying, but it works for now. Easy to
                //       clean up by making function expressions a thing of their own, typed
                //       uniquely as the function's type, but having no physical representation;
                //       (consider it an abstract type)
                if let ParsedExpr::Variable(v) = self.ast.exprs.get(base_arg.value) {
                    let function_name = &v.name;
                    let function_id = self.scopes.find_function_namespaced(
                        ctx.scope_id,
                        function_name,
                        &self.namespaces,
                        &self.ast.idents,
                    )?;
                    if let Some(function_id) = function_id {
                        let function = self.get_function(function_id);
                        if function.is_generic() {
                            return failf!(
                                call_span,
                                "Cannot do toDyn or toRef with a generic function"
                            );
                        }
                        if function.intrinsic_type.is_some_and(|t| t.is_inlined()) {
                            return failf!(
                                call_span,
                                "Cannot do toDyn or toRef with an intrinsic operation"
                            );
                        }
                        return if fn_name == self.ast.idents.builtins.toDyn {
                            Ok(Either::Left(self.function_to_lambda_object(function_id, call_span)))
                        } else if fn_name == self.ast.idents.builtins.toRef {
                            Ok(Either::Left(self.function_to_reference(function_id, call_span)))
                        } else {
                            unreachable!()
                        };
                    }
                }
            } else if fn_name == self.ast.idents.builtins.toStatic {
                if fn_call.args.len() != 1 {
                    return failf!(call_span, ".toStatic() takes no additional arguments");
                }
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                return match self.attempt_static_lift(base_value) {
                    Err(msg) => {
                        return failf!(call_span, "Failed to lift value to static: {}", msg);
                    }
                    Ok(static_expr_id) => Ok(Either::Left(static_expr_id)),
                };
            } else if fn_name == self.ast.idents.builtins.fromStatic {
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                let base_type_id = self.exprs.get(base_value).get_type();
                let Type::Static(static_type) = self.types.get_no_follow_static(base_type_id)
                else {
                    return failf!(
                        call_span,
                        "Cannot use .fromStatic() on non-static type: {}",
                        self.type_id_to_string(base_type_id)
                    );
                };
                return Ok(Either::Left(self.synth_cast(
                    base_value,
                    static_type.inner_type_id,
                    CastType::StaticErase,
                )));
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

        let base_expr_type = self.exprs.get(base_expr).get_type();
        let base_for_method_derefed = match self.types.get_no_follow_static(base_expr_type) {
            Type::Reference(r) => r.inner_type,
            Type::Static(stat) => stat.inner_type_id,
            _ => base_expr_type,
        };
        if let Some(companion_ns) =
            self.types.get_defn_info(base_for_method_derefed).and_then(|d| d.companion_namespace)
        {
            let companion_scope = self.get_namespace_scope(companion_ns);
            if let Some(method_id) = companion_scope.find_function(fn_name) {
                return Ok(Either::Right(Callee::make_static(method_id)));
            }
        };

        let abilities_in_scope = self.scopes.find_abilities_in_scope(ctx.scope_id);
        debug!(
            "abilities_in_scope: {:?}",
            abilities_in_scope
                .iter()
                .map(|a| self.ident_str(self.abilities.get(*a).name))
                .collect::<Vec<_>>()
        );

        // FIXME: ability call resolution is pretty expensive, it may be worth maintaining an index of function names -> ability,
        //        then if we hit, just ensure its in scope.
        let Some((ability_function_index, ability_function_ref)) = abilities_in_scope
            .iter()
            .find_map(|ability_id| self.abilities.get(*ability_id).find_function_by_name(fn_name))
        else {
            return failf!(
                call_span,
                "Method '{}' does not exist on type: '{}'",
                self.ident_str(fn_call.name.name),
                self.type_id_to_string(base_expr_type),
            );
        };
        let ability_id = ability_function_ref.ability_id;
        let ability_function_type = self.get_function(ability_function_ref.function_id).type_id;
        let ability_impl_fn = self.resolve_ability_call(
            ability_function_type,
            ability_function_index,
            ability_id,
            fn_call,
            known_args,
            ctx,
        )?;
        Ok(Either::Right(Callee::from_ability_impl_fn(ability_impl_fn)))
    }

    pub fn function_to_reference(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let function_reference_type = self.types.add_reference_type(function.type_id);
        self.exprs.add(TypedExpr::FunctionReference(FunctionReferenceExpr {
            function_id,
            function_reference_type,
            span: call_span,
        }))
    }

    pub fn function_to_lambda_object(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let dyn_function_id = if let Some(dyn_fn_id) = function.dyn_fn_id {
            dyn_fn_id
        } else {
            let function_defn_span = self.ast.get_span_for_id(function.parsed_id);
            let mut new_function = function.clone();
            let empty_env_variable = self.variables.add(Variable {
                name: self.ast.idents.builtins.lambda_env_var_name,
                type_id: POINTER_TYPE_ID,
                is_mutable: false,
                // Wrong scope, and its not actually added, but we know its not used
                owner_scope: new_function.scope,
                is_context: false,
                global_id: None,
                user_hidden: false,
            });
            new_function.param_variables.insert(0, empty_env_variable);
            let new_function_type =
                self.add_lambda_env_to_function_type(new_function.type_id, function_defn_span);
            new_function.type_id = new_function_type;
            let old_name = self.ident_str(new_function.name);
            new_function.name = self.ast.idents.intern(format!("{}__dyn", old_name));
            let new_function_id = self.add_function(new_function);
            self.get_function_mut(function_id).dyn_fn_id = Some(new_function_id);
            new_function_id
        };
        let dyn_function = self.get_function(dyn_function_id);
        let lambda_object_type_id = self.types.add_lambda_object(
            &self.ast.idents,
            dyn_function.type_id,
            dyn_function.parsed_id,
        );
        let function_to_lam_obj_id =
            self.exprs.add(TypedExpr::FunctionToLambdaObject(FunctionToLambdaObjectExpr {
                function_id: dyn_function_id,
                span: call_span,
                lambda_object_type_id,
            }));
        function_to_lam_obj_id
    }

    fn add_lambda_env_to_function_type(
        &mut self,
        function_type_id: TypeId,
        span: SpanId,
    ) -> TypeId {
        let mut function_type = self.types.get(function_type_id).as_function().unwrap().clone();
        let empty_env_struct_type = self.types.add_empty_struct();
        let empty_env_struct_ref = self.types.add_reference_type(empty_env_struct_type);
        function_type.physical_params.insert(
            0,
            FnParamType {
                name: self.ast.idents.builtins.lambda_env_var_name,
                type_id: empty_env_struct_ref,
                is_context: false,
                is_lambda_env: true,
                span,
            },
        );
        let defn_info = self.types.get_defn_id(function_type_id);
        self.types.add(Type::Function(function_type), defn_info)
    }

    fn resolve_ability_call(
        &mut self,
        function_type_id: TypeId,
        function_ability_index: usize,
        base_ability_id: AbilityId,
        fn_call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<AbilityImplFunction> {
        let call_span = fn_call.span;
        let ability_fn_type = self.types.get(function_type_id).as_function().unwrap();
        let ability_fn_return_type = ability_fn_type.return_type;
        let ability_fn_params: Vec<FnParamType> = ability_fn_type.logical_params().to_vec();
        let ability_params = self.abilities.get(base_ability_id).parameters.clone();
        let ability_self_type_id = self.abilities.get(base_ability_id).self_type_id;

        let passed_len = known_args.map(|ka| ka.1.len()).unwrap_or(fn_call.args.len());
        if passed_len != ability_fn_type.logical_params().len() {
            return failf!(
                call_span,
                "Mismatching arg count when trying to resolve ability call to {} (this probably doesn't handle context params properly)",
                self.ident_str(fn_call.name.name)
            );
        }

        // First, we need to solve for 'Self' using the arguments and expected return type
        //
        // TODO: Make sure we handle context params in ability functions correctly,
        // Probably by: skipping them if not passed explicitly, and utilizing them if passed explicitly

        let all_type_params: SmallVec<[NameAndType; 8]> = {
            let mut type_params = SmallVec::with_capacity(ability_params.len() + 1);
            type_params.push(NameAndType {
                name: self.ast.idents.builtins.Self_,
                type_id: ability_self_type_id,
            });
            type_params.extend(
                ability_params
                    .iter()
                    .map(|p| NameAndType { name: p.name, type_id: p.type_variable_id }),
            );
            type_params
        };
        let all_type_params_handle = self.named_types.add_slice_from_copy_slice(&all_type_params);
        let must_solve_type_params: SmallVec<[NameAndType; 8]> = {
            let mut type_params = SmallVec::with_capacity(ability_params.len() + 1);
            type_params.push(NameAndType {
                name: self.ast.idents.builtins.Self_,
                type_id: ability_self_type_id,
            });
            type_params.extend(
                ability_params
                    .iter()
                    .filter(|p| !p.is_impl_param)
                    .map(|p| NameAndType { name: p.name, type_id: p.type_variable_id }),
            );
            type_params
        };
        let must_solve_type_params_handle =
            self.named_types.add_slice_from_copy_slice(&must_solve_type_params);

        let self_only_type_params_handle =
            self.named_types.add_slice_from_copy_slice(&[NameAndType {
                name: self.ast.idents.builtins.Self_,
                type_id: ability_self_type_id,
            }]);

        let passed_args: &mut dyn Iterator<Item = MaybeTypedExpr> = match known_args {
            Some(ka) => &mut ka.1.iter().map(|t| MaybeTypedExpr::Typed(*t)),
            None => {
                let args = self.ast.p_call_args.get_slice(fn_call.args);
                &mut args.iter().map(|arg| MaybeTypedExpr::Parsed(arg.value))
            }
        };
        let mut args_and_params = Vec::with_capacity(passed_len);
        if let Some(expected_type) = ctx.expected_type_id {
            args_and_params.push(InferenceInputPair {
                arg: TypeOrParsedExpr::Type(expected_type),
                param_type: ability_fn_return_type,
                allow_mismatch: false,
            });
        }
        for (arg, param) in passed_args.zip(ability_fn_params.iter()) {
            let arg_and_param = match arg {
                MaybeTypedExpr::Typed(expr) => {
                    let type_id = self.exprs.get(expr).get_type();
                    InferenceInputPair {
                        arg: TypeOrParsedExpr::Type(type_id),
                        param_type: param.type_id,
                        allow_mismatch: false,
                    }
                }
                MaybeTypedExpr::Parsed(parsed_expr) => InferenceInputPair {
                    arg: TypeOrParsedExpr::Parsed(parsed_expr),
                    param_type: param.type_id,
                    allow_mismatch: false,
                },
            };
            args_and_params.push(arg_and_param);
        }

        debug!(
            "all ability params: {}",
            self.pretty_print_named_type_slice(all_type_params_handle, ", ")
        );
        debug!(
            "to solve: {}",
            self.pretty_print_named_type_slice(must_solve_type_params_handle, ", ")
        );

        let (self_only, other_solved) = self.infer_types(
            all_type_params_handle,
            self_only_type_params_handle,
            &args_and_params,
            fn_call.span,
            ctx.scope_id,
        )?;
        let mut parameter_constraints: SV8<Option<TypeId>> = smallvec![];
        for ab_param in &ability_params {
            if ab_param.is_impl_param {
                continue;
            }
            let solution =
                self.named_types.get_slice(other_solved).iter().find(|nt| nt.name == ab_param.name);
            parameter_constraints.push(solution.map(|nt| nt.type_id));
        }
        let solved_self = self.named_types.get_nth(self_only, 0).type_id;
        let impl_handle = self.find_ability_impl_for_type_or_generate_new(
            solved_self,
            base_ability_id,
            &parameter_constraints,
            ctx.scope_id,
            call_span,
        ).map_err(|msg| {
           errf!(
               call_span,
               "Call to `{}` with type Self = {} does not work, since it does not implement ability {}. {}",
               self.type_id_to_string(function_type_id),
               self.type_id_to_string(solved_self),
               self.ability_impl_signature_to_string(base_ability_id, SliceHandle::Empty),
               msg
           )
        })?;

        let impl_function = self
            .ability_impls
            .get(impl_handle.full_impl_id)
            .function_at_index(function_ability_index);
        Ok(impl_function)
    }

    fn handle_enum_get_tag(
        &mut self,
        base_expr_id: TypedExprId,
        span: SpanId,
    ) -> TyperResult<Option<TypedExprId>> {
        let original_type = self.exprs.get(base_expr_id).get_type();
        let (enum_type, is_reference) = match self.types.get(original_type) {
            Type::Enum(e) => (e, false),
            Type::EnumVariant(ev) => (self.types.get(ev.enum_type_id).expect_enum(), false),
            Type::Reference(refer) => match self.types.get(refer.inner_type) {
                Type::Enum(e) => (e, true),
                Type::EnumVariant(ev) => (self.types.get(ev.enum_type_id).expect_enum(), true),
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };

        let tag_type = enum_type.tag_type;

        let base_expr =
            if is_reference { self.synth_dereference(base_expr_id) } else { base_expr_id };
        Ok(Some(self.exprs.add(TypedExpr::EnumGetTag(GetEnumTag {
            enum_expr: base_expr,
            result_type_id: tag_type,
            span,
        }))))
    }

    fn handle_enum_as(
        &mut self,
        base_expr: TypedExprId,
        fn_call: &ParsedCall,
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
        let fn_name = self.ident_str(fn_call.name.name);
        if fn_name.starts_with("as") && fn_call.type_args.is_empty() && fn_call.args.len() == 1 {
            let span = fn_call.span;
            let Some(variant) = e.variants.iter().find(|v| {
                let mut s = String::with_capacity(16);
                s.push_str("as");
                let name = self.ident_str(v.name);
                let name_capitalized = strings::capitalize_first(name);
                s.push_str(&name_capitalized);

                fn_name == s
            }) else {
                let base_expr_type = self.exprs.get(base_expr).get_type();
                return failf!(
                    span,
                    "Method '{}' does not exist on type '{}'",
                    fn_name,
                    self.type_id_to_string(base_expr_type)
                );
            };
            let variant_type_id = variant.my_type_id;
            let variant_index = variant.index;
            let resulting_type_id = if is_reference {
                self.types.add_reference_type(variant_type_id)
            } else {
                variant_type_id
            };
            let base_expr_dereferenced =
                if is_reference { self.synth_dereference(base_expr) } else { base_expr };
            let condition = self.exprs.add(TypedExpr::EnumIsVariant(TypedEnumIsVariantExpr {
                enum_expr: base_expr_dereferenced,
                variant_index,
                span,
            }));
            let cast_type =
                if is_reference { CastType::ReferenceToReference } else { CastType::EnumToVariant };
            let (consequent, consequent_type_id) =
                // base_expr is the enum type, or a reference to it
                // resulting_type_id is the variant, or a reference to it
                self.synth_optional_some(TypedExpr::Cast(TypedCast {
                    cast_type,
                    base_expr,
                    target_type_id: resulting_type_id,
                    span,
                }));
            let alternate = self.synth_optional_none(resulting_type_id, span);

            Ok(Some(self.synth_if_else(
                smallvec![],
                consequent_type_id,
                condition,
                consequent,
                alternate,
                span,
            )))
        } else {
            Ok(None)
        }
    }

    fn handle_enum_constructor(
        &mut self,
        base_expr: Option<ParsedExprId>,
        variant_name: Ident,
        payload_parsed_expr: Option<ParsedExprId>,
        type_args: SliceHandle<NamedTypeArgId>,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<Option<TypedExprId>> {
        let base_type_id = match base_expr {
            Some(base_expr) => {
                let ParsedExpr::Variable(v) = self.ast.exprs.get(base_expr) else {
                    return Ok(None);
                };
                let Some((base_type_in_scope, _)) = self.scopes.find_type_namespaced(
                    ctx.scope_id,
                    &v.name,
                    &self.namespaces,
                    &self.ast.idents,
                )?
                else {
                    return Ok(None);
                };
                match self.types.get(base_type_in_scope) {
                    Type::Enum(_e) => base_type_in_scope,
                    Type::Generic(g) => {
                        let Some(_inner_enum) = self.types.get(g.inner).as_enum() else {
                            return Ok(None);
                        };
                        base_type_in_scope
                    }
                    _ => return Ok(None),
                }
            }
            None => {
                // This case can just fail if the expected type is not an enum
                let expected_type = ctx.expected_type_id.ok_or_else(|| {
                    make_error(
                        "Could not infer enum type from context; try supplying the name or providing a type ascription",
                        span,
                    )
                })?;
                let enum_type_id = {
                    match self.types.get(expected_type) {
                        Type::Enum(_e) => Ok(expected_type),
                        Type::EnumVariant(ev) => Ok(ev.enum_type_id),
                        _ => failf!(
                            span,
                            "Could not infer expected enum type for '.' shorthand; expected type was a {} type: {}",
                            self.type_kind_to_string(expected_type),
                            self.type_id_to_string(expected_type)
                        ),
                    }
                }?;
                let base_enum_type_id = match self.types.get_generic_instance_info(enum_type_id) {
                    None => enum_type_id,
                    Some(gi) => gi.generic_parent,
                };
                base_enum_type_id
            }
        };
        let base_enum_or_generic_enum = self.types.get(base_type_id);
        if let Type::Enum(e) = base_enum_or_generic_enum {
            if let Some(_variant) = e.variant_by_name(variant_name) {
                Ok(Some(self.eval_enum_constructor(
                    base_type_id,
                    variant_name,
                    payload_parsed_expr,
                    ctx,
                    span,
                )?))
            } else {
                failf!(span, "No such variant: {}", self.ident_str(variant_name))
            }
        } else if let Type::Generic(g) = base_enum_or_generic_enum {
            let Some(inner_enum) = self.types.get(g.inner).as_enum() else { return Ok(None) };
            let Some(generic_variant) = inner_enum.variant_by_name(variant_name) else {
                return Ok(None);
            };
            let g_params = g.params;

            let payload_if_needed = match generic_variant.payload {
                Some(generic_payload_type_id) => match payload_parsed_expr {
                    None => {
                        return failf!(
                            span,
                            "Variant {} requires a payload",
                            self.ident_str(generic_variant.name)
                        );
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
                            self.ident_str(generic_variant.name)
                        );
                    }
                },
            };

            let solved_or_passed_type_params: NamedTypeSlice = if type_args.is_empty() {
                match payload_if_needed {
                    None => {
                        match ctx
                            .expected_type_id
                            .map(|t| (t, self.types.get_generic_instance_info(t)))
                        {
                            Some((expected_type, Some(spec_info))) => {
                                // We're expecting a specific instance of a generic enum
                                if spec_info.generic_parent == base_type_id {
                                    // Solved params
                                    let solved_params_iter: SV4<NameAndType> = self
                                        .named_types
                                        .get_slice(g_params)
                                        .iter()
                                        .zip(spec_info.type_args.iter())
                                        .map(|(g_param, expected_specialized_type)| NameAndType {
                                            name: g_param.name,
                                            type_id: *expected_specialized_type,
                                        })
                                        .collect();
                                    self.named_types.add_slice_from_copy_slice(&solved_params_iter)
                                } else {
                                    return failf!(
                                        span,
                                        "Cannot infer a type for {}; expected mismatching generic type {}",
                                        self.name_of_type(base_type_id),
                                        self.type_id_to_string(expected_type)
                                    );
                                }
                            }
                            _ => {
                                return failf!(
                                    span,
                                    "Cannot infer a type for {}",
                                    self.name_of_type(base_type_id)
                                );
                            }
                        }
                    }
                    Some((generic_variant_payload, payload)) => {
                        let mut args_and_params: SV4<InferenceInputPair> = smallvec![];

                        // There are only ever up to 2 'cases' to power inference
                        // - The expected return type together with the type of the enum itself
                        // - The passed payload together with the type of the payload itself
                        if let Some(expected) = ctx.expected_type_id {
                            args_and_params.push(InferenceInputPair {
                                arg: TypeOrParsedExpr::Type(expected),
                                param_type: g.inner,
                                allow_mismatch: true,
                            })
                        };
                        args_and_params.push(InferenceInputPair {
                            arg: TypeOrParsedExpr::Parsed(payload),
                            param_type: generic_variant_payload,
                            allow_mismatch: false,
                        });
                        let (solutions, _all_solutions) = self.infer_types(
                            g_params,
                            g_params,
                            &args_and_params,
                            span,
                            ctx.scope_id,
                        )?;
                        solutions
                    }
                }
            } else {
                let mut passed_params: SV4<NameAndType> = SmallVec::with_capacity(g_params.len());
                let type_args_owned = self.ast.p_type_args.copy_slice_sv::<4>(type_args);
                for (generic_param, passed_type_expr) in
                    self.named_types.copy_slice_sv::<4>(g_params).iter().zip(type_args_owned.iter())
                {
                    let type_id = self.eval_type_expr(passed_type_expr.type_expr, ctx.scope_id)?;
                    passed_params.push(NameAndType { name: generic_param.name, type_id });
                }
                self.named_types.add_slice_from_copy_slice(&passed_params)
            };

            let passed_type_ids = self
                .named_types
                .get_slice(solved_or_passed_type_params)
                .iter()
                .map(|type_param| type_param.type_id)
                .collect();
            let concrete_type = self.instantiate_generic_type(base_type_id, passed_type_ids);
            let enum_constr = self.eval_enum_constructor(
                concrete_type,
                variant_name,
                payload_parsed_expr,
                ctx,
                span,
            )?;
            Ok(Some(enum_constr))
        } else {
            unreachable!()
        }
    }

    fn align_call_arguments_with_parameters<'params>(
        &mut self,
        fn_call: &ParsedCall,
        params: &'params [FnParamType],
        pre_evaled_params: Option<&[TypedExprId]>,
        calling_scope: ScopeId,
        tolerate_missing_context_args: bool,
    ) -> TyperResult<ArgsAndParams<'params>> {
        let fn_name = fn_call.name.name;
        let span = fn_call.span;
        let args_slice = self.ast.p_call_args.get_slice(fn_call.args);
        //eprintln!(
        //    "params_slice! {:?}",
        //    params.iter().map(|p| (self.name_of(p.name), p.is_context)).collect::<Vec<_>>()
        //);
        //eprintln!(
        //    "args_slice! {:?}",
        //    args_slice.iter().map(|a| self.ast.expr_id_to_string(a.value)).collect::<Vec<_>>()
        //);
        let explicit_context_args = args_slice.iter().any(|a| a.is_explicit_context);
        let named = args_slice.first().is_some_and(|arg| arg.name.is_some());
        let mut final_args: SmallVec<[MaybeTypedExpr; FUNC_PARAM_IDEAL_COUNT]> = SmallVec::new();
        let mut final_params: SmallVec<[&FnParamType; FUNC_PARAM_IDEAL_COUNT]> = SmallVec::new();
        if !explicit_context_args {
            for context_param in params.iter().filter(|p| p.is_context) {
                let matching_context_variable =
                    self.scopes.find_context_variable_by_type(calling_scope, context_param.type_id);
                if let Some(matching_context_variable) = matching_context_variable {
                    let found = self.variables.get(matching_context_variable);
                    final_args.push(MaybeTypedExpr::Typed(self.exprs.add(TypedExpr::Variable(
                        VariableExpr {
                            variable_id: matching_context_variable,
                            type_id: found.type_id,
                            span,
                        },
                    ))));
                    final_params.push(context_param);
                } else {
                    let is_source_loc = context_param.type_id == COMPILER_SOURCE_LOC_TYPE_ID;
                    if is_source_loc {
                        let expr = self.synth_source_location(span);
                        final_args.push(MaybeTypedExpr::Typed(expr));
                        final_params.push(context_param);
                    } else if !tolerate_missing_context_args {
                        return failf!(
                            span,
                            "Failed to find context parameter '{}'. No context variables of type {} are in scope",
                            self.ident_str(context_param.name),
                            self.type_id_to_string(context_param.type_id)
                        );
                    } else {
                        debug!(
                            "Tolerating a missing context argument of type {}",
                            self.type_id_to_string(context_param.type_id)
                        );
                        continue;
                    }
                };
            }
        }

        let args_slice = self.ast.p_call_args.get_slice(fn_call.args);
        let is_lambda =
            params.first().is_some_and(|p| p.name == self.ast.idents.builtins.lambda_env_var_name);
        let params = if is_lambda { &params[1..] } else { params };
        let explicit_param_count = params.iter().filter(|p| !p.is_context).count();
        let total_expected =
            if explicit_context_args { params.len() } else { explicit_param_count };
        let actual_passed_args = args_slice;
        let total_passed = match pre_evaled_params {
            None => actual_passed_args.len(),
            Some(pre_evaled_params) => pre_evaled_params.len(),
        };
        if total_passed != total_expected {
            return failf!(
                span,
                "Incorrect number of arguments to {}: expected {}, got {}",
                self.ident_str(fn_call.name.name),
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
                        actual_passed_args.iter().find(|arg| arg.name == Some(fn_param.name))
                    else {
                        return failf!(
                            fn_call.span,
                            "Missing named argument for parameter {}",
                            self.ident_str(fn_param.name)
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
                    actual_passed_args.get(param_index)
                };
                let Some(param) = matching_argument else {
                    return failf!(
                        span,
                        "Missing argument to {}: {}",
                        self.ident_str(fn_name).blue(),
                        self.ident_str(fn_param.name).red()
                    );
                };
                final_args.push(MaybeTypedExpr::Parsed(param.value));
                final_params.push(fn_param);
            }
        }
        Ok(ArgsAndParams { args: final_args, params: final_params })
    }

    fn check_call_argument(
        &mut self,
        param: &FnParamType,
        arg: TypedExprId,
        calling_scope: ScopeId,
    ) -> TyperResult<Option<TypedExprId>> {
        debug!(
            "Checking that argument {} has type {}",
            self.expr_to_string(arg),
            self.type_id_to_string(param.type_id)
        );
        match self.check_expr_type(param.type_id, arg, calling_scope) {
            CheckExprTypeResult::Coerce(new_expr_id) => Ok(Some(new_expr_id)),
            CheckExprTypeResult::Ok => Ok(None),
            CheckExprTypeResult::Err(msg) => failf!(
                self.exprs.get(arg).get_span(),
                "Invalid type for parameter {}: {}",
                self.ident_str(param.name),
                msg
            ),
        }
    }

    pub fn is_callee_generic(&self, callee: &Callee) -> bool {
        match callee {
            Callee::StaticFunction(function_id) => self.get_function(*function_id).is_generic(),
            Callee::StaticLambda { .. } => false,
            Callee::Abstract { function_sig, .. } => function_sig.is_generic(),
            Callee::DynamicFunction { .. } => false,
            Callee::DynamicLambda(_) => false,
            // Should always be false...
            Callee::DynamicAbstract { function_sig, .. } => function_sig.is_generic(),
        }
    }

    pub fn get_callee_function_signature(&self, callee: &Callee) -> FunctionSignature {
        match callee {
            Callee::StaticFunction(function_id) => self.get_function(*function_id).signature(),
            Callee::StaticLambda { function_id, .. } => self.get_function(*function_id).signature(),
            Callee::Abstract { function_sig, .. } => *function_sig,
            Callee::DynamicFunction { .. } => {
                let function_type = self.get_callee_function_type(callee);
                FunctionSignature::make_no_generics(None, function_type)
            }
            Callee::DynamicLambda(_) => {
                let function_type = self.get_callee_function_type(callee);
                FunctionSignature::make_no_generics(None, function_type)
            }
            // Should always be false...
            Callee::DynamicAbstract { function_sig, .. } => *function_sig,
        }
    }

    pub fn get_callee_function_type(&self, callee: &Callee) -> TypeId {
        match callee {
            Callee::StaticFunction(function_id) | Callee::StaticLambda { function_id, .. } => {
                self.get_function(*function_id).type_id
            }
            Callee::Abstract { function_sig, .. } => function_sig.function_type,
            Callee::DynamicFunction { function_reference_expr } => {
                let function_reference_type =
                    self.get_expr_type(*function_reference_expr).expect_reference().inner_type;
                function_reference_type
            }
            Callee::DynamicLambda(dynamic) => match self.get_expr_type(*dynamic) {
                Type::LambdaObject(lambda_object) => lambda_object.function_type,
                _ => {
                    panic!(
                        "Invalid dynamic function callee: {}",
                        self.type_id_to_string(self.exprs.get(*dynamic).get_type())
                    )
                }
            },
            Callee::DynamicAbstract { function_sig, .. } => function_sig.function_type,
        }
    }

    fn eval_function_call(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        known_callee: Option<Callee>,
    ) -> TyperResult<TypedExprId> {
        let span = fn_call.span;
        assert!(
            fn_call.args.is_empty() || known_args.is_none(),
            "cannot pass both typed value args and parsed value args to eval_function_call"
        );
        let callee = match known_callee {
            None => match self.resolve_parsed_function_call(fn_call, known_args.as_ref(), ctx)? {
                Either::Left(expr) => return Ok(expr),
                Either::Right(callee) => callee,
            },
            Some(callee) => callee,
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let callee_function_type_id = self.get_callee_function_type(&callee);
        let signature = self.get_callee_function_signature(&callee);
        let is_generic = signature.is_generic();

        let original_function_type = self.types.get(callee_function_type_id).as_function().unwrap();
        let params = &original_function_type.logical_params().to_vec();

        let (callee, typechecked_arguments, type_args) = match is_generic {
            false => {
                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                )?;
                let mut typechecked_args = SmallVec::with_capacity(args_and_params.len());
                for (maybe_typed_expr, param) in args_and_params.iter() {
                    let expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, ctx.with_expected_type(Some(param.type_id)))?
                        }
                    };
                    let checked_expr = match self.check_call_argument(param, expr, ctx.scope_id)? {
                        None => expr,
                        Some(coerced_expr) => coerced_expr,
                    };
                    typechecked_args.push(checked_expr);
                }
                (callee, typechecked_args, SliceHandle::Empty)
            }
            true => {
                let original_args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    true,
                )?;
                // If a function is generic, we have a function id. Lambdas and function pointer
                // calls can't take type arguments
                // let function_id = callee.maybe_function_id().unwrap();

                // We infer the type arguments, or just use them if the user has supplied them
                let type_args = match &known_args {
                    Some((type_args, _va)) if !type_args.is_empty() => {
                        // Need the name
                        if type_args.len() != signature.type_params.len() {
                            self.ice_with_span("Bad known type args, wrong count", span)
                        }
                        let args: SV4<NameAndType> = type_args
                            .iter()
                            .zip(self.named_types.get_slice(signature.type_params).iter())
                            .map(|(type_arg, type_param)| NameAndType {
                                name: type_param.name,
                                type_id: *type_arg,
                            })
                            .collect();
                        self.named_types.add_slice_from_copy_slice(&args)
                    }
                    _ => self
                        .infer_and_constrain_call_type_args(fn_call, signature, known_args, ctx)?,
                };

                let function_type_args = self.determine_function_type_args_for_call(
                    signature,
                    type_args,
                    &original_args_and_params,
                    ctx,
                )?;

                let static_type_args = self.determine_static_type_args_for_call(
                    signature,
                    &original_args_and_params,
                    ctx,
                )?;

                let specialized_function_type = self.substitute_in_function_signature(
                    type_args,
                    function_type_args,
                    static_type_args,
                    signature,
                );
                let is_abstract = self
                    .types
                    .get_contained_type_variable_counts(specialized_function_type)
                    .is_abstract();

                let final_callee = if is_abstract {
                    Callee::Abstract {
                        function_sig: FunctionSignature::make_no_generics(
                            signature.name,
                            specialized_function_type,
                        ),
                    }
                } else {
                    match callee {
                        Callee::StaticFunction(function_id) => {
                            let function_id = self.specialize_function_signature(
                                type_args,
                                function_type_args,
                                static_type_args,
                                function_id,
                            )?;
                            Callee::StaticFunction(function_id)
                        }
                        Callee::Abstract { function_sig } => Callee::Abstract {
                            function_sig: FunctionSignature::make_no_generics(
                                function_sig.name,
                                specialized_function_type,
                            ),
                        },
                        _ => self.ice(
                            "Unexpected Callee type for a generic that required specialization",
                            None,
                        ),
                    }
                };

                let specialized_fn_type =
                    self.types.get(specialized_function_type).as_function().unwrap();
                let specialized_params = specialized_fn_type.physical_params.clone();
                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    &specialized_params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                )?;

                let mut typechecked_args = SmallVec::with_capacity(args_and_params.len());
                for (maybe_typed_expr, param) in args_and_params.iter() {
                    let expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => {
                            self.eval_expr(parsed, ctx.with_expected_type(Some(param.type_id)))?
                        }
                    };

                    let checked_expr = match self.check_call_argument(param, expr, ctx.scope_id)? {
                        None => expr,
                        Some(coerced_expr) => coerced_expr,
                    };
                    typechecked_args.push(checked_expr);
                }

                (final_callee, typechecked_args, type_args)
            }
        };

        let call_return_type = if typechecked_arguments
            .iter()
            .any(|arg| self.exprs.get(*arg).get_type() == NEVER_TYPE_ID)
        {
            NEVER_TYPE_ID
        } else {
            self.types
                .get(self.get_callee_function_type(&callee))
                .as_function()
                .unwrap()
                .return_type
        };

        let call = Call {
            callee,
            args: typechecked_arguments,
            type_args,
            return_type: call_return_type,
            span,
        };

        // Intrinsics that are handled by the typechecking phase are implemented here.
        // Currently only CompilerSourceLocation.
        if let Some(intrinsic_type) = call
            .callee
            .maybe_function_id()
            .and_then(|id| self.get_function(id).intrinsic_type)
            .filter(|op| op.is_typer_phase())
        {
            self.handle_intrinsic(call, intrinsic_type, ctx)
        } else {
            Ok(self.exprs.add(TypedExpr::Call(call)))
        }
    }

    fn attempt_static_lift(&mut self, expr_id: TypedExprId) -> TyperResult<TypedExprId> {
        // Take an arbitrary expression and do our very best to turn it into a statically-known
        // value. Easy examples that must work include:
        // - all scalar literals,
        // - string literals,
        // - Perhaps struct and enum literals, if we're feeling motivated
        // - LIST literals are tricky because they've already been compiled to a block of
        //   imperative code

        // We match on the node type, not its type, since the point is to hoist literals, not
        // follow variables around and implement a whole extra damn compiler
        let result = match self.exprs.get(expr_id) {
            TypedExpr::Unit(span) => {
                let static_value_id = self.static_values.add(StaticValue::Unit);
                let static_type = self.types.add_static_type(UNIT_TYPE_ID, Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(static_value_id, static_type, *span)))
            }
            TypedExpr::Char(c, span) => {
                let static_value_id = self.static_values.add(StaticValue::Char(*c));
                let static_type = self.types.add_static_type(CHAR_TYPE_ID, Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(static_value_id, static_type, *span)))
            }
            TypedExpr::Bool(b, span) => {
                let static_value_id = self.static_values.add(StaticValue::Boolean(*b));
                let static_type = self.types.add_static_type(BOOL_TYPE_ID, Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(static_value_id, static_type, *span)))
            }
            TypedExpr::Integer(int_expr) => {
                let static_value_id = self.static_values.add(StaticValue::Int(int_expr.value));
                let static_type =
                    self.types.add_static_type(int_expr.get_type(), Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(
                    static_value_id,
                    static_type,
                    int_expr.span,
                )))
            }
            TypedExpr::Float(float_expr) => {
                let static_value_id = self.static_values.add(StaticValue::Float(float_expr.value));
                let static_type =
                    self.types.add_static_type(float_expr.get_type(), Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(
                    static_value_id,
                    static_type,
                    float_expr.span,
                )))
            }
            TypedExpr::String(string_id, span) => {
                let static_value_id = self.static_values.add(StaticValue::String(*string_id));
                let static_type = self.types.add_static_type(STRING_TYPE_ID, Some(static_value_id));
                Ok(self.exprs.add(TypedExpr::StaticValue(static_value_id, static_type, *span)))
            }
            e => {
                failf!(
                    self.exprs.get(expr_id).get_span(),
                    "Expression type is unsupported for static lift: {}. For more complex values, use a #static expression instead",
                    e.kind_str()
                )
            }
        };
        if let Ok(result) = result {
            debug_assert_eq!(
                self.types
                    .get_no_follow_static(self.exprs.get(result).get_type())
                    .as_static()
                    .unwrap()
                    .inner_type_id,
                self.exprs.get(expr_id).get_type()
            );
        }
        result
    }

    fn handle_intrinsic(
        &mut self,
        call: Call,
        intrinsic: IntrinsicOperation,
        _ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = call.span;
        match intrinsic {
            IntrinsicOperation::GetStaticValue => {
                let static_value_id_arg = self.exprs.get(call.args[0]);
                // For now, require a literal. We could relax this and evaluate it
                // if that's useful
                let TypedExpr::Integer(int_expr) = static_value_id_arg else {
                    return failf!(span, "Argument must be an integer literal");
                };
                let TypedIntValue::U64(u64_value) = int_expr.value else {
                    return failf!(span, "Argument must be a u64 literal");
                };
                let Some(static_value_id) = StaticValueId::from_u32(u64_value as u32) else {
                    return failf!(span, "Invalid static value id: {}. Must be a u32", u64_value);
                };
                let Some(value) = self.static_values.get_opt(static_value_id) else {
                    return failf!(span, "No static value with id {}", static_value_id);
                };
                let static_value_expr =
                    TypedExpr::StaticValue(static_value_id, value.get_type(), span);
                Ok(self.exprs.add(static_value_expr))
            }
            IntrinsicOperation::CompilerSourceLocation => {
                let source_location = self.synth_source_location(span);
                Ok(source_location)
            }
            IntrinsicOperation::TypeId => {
                let type_arg = self.named_types.get_nth(call.type_args, 0);
                let type_id = type_arg.type_id;
                let type_id_u64 = type_arg.type_id.as_u32() as u64;

                // We generate a schema for every type for which a typeId is requested
                // This guarantees that we have it available at runtime when typeSchema is
                // called
                // Same for typeName
                self.register_type_metainfo(type_id, span);

                let int_expr = TypedExpr::Integer(TypedIntegerExpr {
                    value: TypedIntValue::U64(type_id_u64),
                    span,
                });
                Ok(self.exprs.add(int_expr))
            }
            IntrinsicOperation::SizeOf
            | IntrinsicOperation::SizeOfStride
            | IntrinsicOperation::AlignOf => {
                let type_id = self.named_types.get_nth(call.type_args, 0).type_id;
                let layout = self.types.get_layout(type_id);
                let value_bytes = match intrinsic {
                    IntrinsicOperation::SizeOf => layout.size as u64,
                    IntrinsicOperation::SizeOfStride => layout.stride() as u64,
                    IntrinsicOperation::AlignOf => layout.align as u64,
                    _ => unreachable!(),
                };
                Ok(self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                    value: TypedIntValue::UWord64(value_bytes),
                    span,
                })))
            }
            _ => self.ice(format!("Unexpected intrinsic in type phase: {:?}", intrinsic), None),
        }
    }

    /// Performs type inference given an input of, essentially, (expected type, actual type) and
    /// a series of parameters to solve for, and performs unification/matching on those pairs.
    ///
    /// allow_mismatch allows inference to continue when a term does not match with its position's
    /// type.
    /// It is currently used to avoid reporting a mismatch on the return type
    /// before we're able to learn more about the rest of the inference.
    /// We get a better error message if we wait to report the mismatch until the end
    fn infer_types(
        &mut self,
        all_type_params: NamedTypeSlice,
        // Sometimes, we don't need to solve everything. An example is when resolving
        // an ability call, we don't need to solve for the impl-side params, as they will come
        // once we look up the impl.
        //
        // But in this case we still instantiate all the params as type holes, so that we can
        // detect inconsistencies, and so that we don't have to bind type variables to themselves
        // as a separate step
        must_solve_params: NamedTypeSlice,
        inference_pairs: &[InferenceInputPair],
        span: SpanId,
        scope_id: ScopeId,
    ) -> TyperResult<(NamedTypeSlice, NamedTypeSlice)> {
        self.inference_context.origin_stack.push(span);
        let infer_depth = self.inference_context.origin_stack.len();

        let mut self_ = scopeguard::guard(self, |self_| {
            let id = self_.inference_context.origin_stack.pop().unwrap();
            debug_assert!(id == span);
            if self_.inference_context.origin_stack.is_empty() {
                debug!("Resetting inference buffers since stack is empty");
                self_.inference_context.constraints.clear();
                self_.inference_context.params.clear();
                self_.inference_context.solutions_so_far.clear();
                self_.inference_context.inference_vars.clear();
                self_.inference_context.substitutions.clear();
                self_.inference_context.substitutions_vec.clear();
            } else {
                debug!(
                    "Not resetting inference buffers: inference depth is nonzero: {}",
                    self_.inference_context.origin_stack.len()
                );
            }
        });

        // Stores the mapping from the function (or type's) type parameters to their
        // corresponding instantiated type holes for this inference context
        let mut instantiation_set: SV8<TypeSubstitutionPair> =
            SmallVec::with_capacity(all_type_params.len());

        let inference_var_count = self_.inference_context.inference_vars.len();
        let all_type_params_slice = self_.named_types.copy_slice_sv::<8>(all_type_params);
        self_.inference_context.params.extend(all_type_params_slice.iter().map(|nt| nt.type_id));
        for (idx, param) in all_type_params_slice.iter().enumerate() {
            let hole_index = idx + inference_var_count;

            let type_hole = self_
                .types
                .add_anon(Type::InferenceHole(InferenceHoleType { index: hole_index as u32 }));
            self_.inference_context.inference_vars.push(type_hole);
            instantiation_set.push(TypeSubstitutionPair { from: param.type_id(), to: type_hole });
        }

        // Used for the error message, mainly
        let mut argument_types: SmallVec<[TypeId; 8]> = smallvec![];
        // The heart of the inference process:
        // - For each pair of (<actual type> <expected type>), we 'pattern match' on the types
        //   and learn what we can about all of the type holes
        // - We have to 'instantiate' the parameter type first, which means replacing occurrences
        //   of type parameters like 'T' with inference holes like '0
        for (index, InferenceInputPair { arg: expr, param_type: gen_param, allow_mismatch }) in
            inference_pairs.iter().enumerate()
        {
            let instantiated_param_type = self_.substitute_in_type(*gen_param, &instantiation_set);
            debug!(
                "[infer {infer_depth}] Instantiated parameter type for inference. Was: {}, is: {}",
                self_.type_id_to_string(*gen_param),
                self_.type_id_to_string(instantiated_param_type)
            );

            debug!(
                "[infer {infer_depth}] Inferring call argument {} / {}",
                index + 1,
                inference_pairs.len()
            );

            let s = std::mem::take(&mut self_.inference_context.substitutions_vec);
            // Calculating the 'expected_type_so_far' is an extra step that gives us better
            // results; we're able to get 'better' types from later arguments if we've already
            // learned some things from prior ones.
            let expected_type_so_far = self_.substitute_in_type(instantiated_param_type, &s);
            debug!(
                "[infer {infer_depth}] Set is \n{}",
                self_.pretty_print_type_substitutions(&s, "\n"),
            );
            debug!(
                "[infer {infer_depth}] Expected type is: {}",
                self_.type_id_to_string(expected_type_so_far)
            );
            self_.inference_context.substitutions_vec = s;

            let (argument_type, argument_span) = match expr {
                TypeOrParsedExpr::Type(type_id) => (*type_id, span),
                TypeOrParsedExpr::Parsed(parsed_expr) => {
                    let inference_context = EvalExprContext::make(scope_id)
                        .with_inference(true)
                        .with_expected_type(Some(expected_type_so_far));

                    let evaluation_result = self_.eval_expr(*parsed_expr, inference_context);
                    match evaluation_result {
                        Ok(expr_id) => {
                            let expr = self_.exprs.get(expr_id);
                            debug!(
                                "[infer {infer_depth}] Actual type is: {}",
                                self_.type_id_to_string(expr.get_type())
                            );
                            (expr.get_type(), expr.get_span())
                        }
                        Err(e) => {
                            // Some expression types, like lambdas, fail really easily if we don't
                            // know the types of the inputs yet. When a lambda fails here, we
                            // choose to continue and report a different, likely more informative,
                            // error
                            let should_skip = match self_.ast.exprs.get(*parsed_expr) {
                                ParsedExpr::Lambda(_) => true,
                                _ => false,
                            };
                            if should_skip {
                                continue;
                            } else {
                                return failf!(
                                    e.span,
                                    "Failed to determine type of argument for inference. {} (used expected type: {})",
                                    e.message,
                                    self_.type_id_to_string(expected_type_so_far)
                                );
                            }
                        }
                    }
                }
            };
            argument_types.push(argument_type);
            debug!(
                "[infer {infer_depth}] unify {} =:= {}",
                self_.type_id_to_string(argument_type),
                self_.type_id_to_string(expected_type_so_far),
            );
            // unify_and_find_substitutions populates self.inferences_context.constraints
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

            // After each pair is 'walked', we 'apply' what we learned by calling calculate_inference_substitutions
            let newly_solved_params = self_.calculate_inference_substitutions(span)?;
            for newly_solved_param in &newly_solved_params {
                debug!(
                    "[infer {infer_depth}] ****** GOT NEWLY SOLVED PARAM {} -> {}",
                    self_.type_id_to_string(newly_solved_param.from),
                    self_.type_id_to_string(newly_solved_param.to)
                );
                self_.apply_constraints_to_inferred_type(
                    newly_solved_param.from,
                    newly_solved_param.to,
                    scope_id,
                    argument_span,
                )?;
            }

            debug!(
                "[infer {infer_depth}] substitutions\n\t{}",
                self_.pretty_print_type_substitutions(&self_.inference_context.constraints, "\n\t"),
            );
        }

        // TODO: enrich this error, probably do the same thing we're doing below for unsolved
        let final_substitutions = &self_.inference_context.substitutions;

        let mut solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        let mut unsolved_params: SV8<NameAndType> = smallvec![];
        let mut all_solutions: SV4<NameAndType> = SmallVec::with_capacity(must_solve_params.len());
        for param in self_.named_types.get_slice(all_type_params) {
            let param_to_hole =
                instantiation_set.iter().find(|p| p.from == param.type_id()).unwrap();
            let corresponding_hole = param_to_hole.to;
            let is_must_solve = self_.named_types.slice_contains(must_solve_params, param);
            if let Some(solution) = final_substitutions.get(&corresponding_hole) {
                if is_must_solve {
                    solutions.push(NameAndType { name: param.name(), type_id: *solution });
                };
                all_solutions.push(NameAndType { name: param.name(), type_id: *solution });
            } else {
                if is_must_solve {
                    unsolved_params.push(*param);
                }
            }
        }
        if !unsolved_params.is_empty() {
            return failf!(
                span,
                "Could not solve for {} given arguments:\n{}",
                unsolved_params
                    .iter()
                    .map(|p| self_.ident_str(p.name()))
                    .collect::<Vec<_>>()
                    .join(", "),
                argument_types
                    .iter()
                    .zip(inference_pairs.iter())
                    .map(|(passed_type, pair)| {
                        format!(
                            "{}: {}",
                            self_.type_id_to_string_ext(*passed_type, false),
                            self_.type_id_to_string_ext(pair.param_type, false),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
        debug!("INFER DONE {}", self_.pretty_print_named_types(&solutions, ", "));
        let solutions_handle = self_.named_types.add_slice_from_copy_slice(&solutions);
        let all_solutions_handle = self_.named_types.add_slice_from_copy_slice(&all_solutions);
        Ok((solutions_handle, all_solutions_handle))
    }

    /// Called from infer_types
    ///
    /// Each time we get a _concrete_ substitution for a param,
    /// if the param has constraints,
    /// we need to 'learn' from the constraints, adding a pair to the mix
    /// for each ability param and ability impl param in the constraint
    /// Consider:
    /// fn find[T, I: Iterator[Item = T]](i: I, fn: \T -> bool)
    /// find(myList, \i i.isEven())
    ///      ^
    /// We need to learn T from the ability impl for Iterator for myList, so that we can infer the
    /// type of the function param in \i i.isEven()
    fn apply_constraints_to_inferred_type(
        &mut self,
        type_param_id: TypeId,
        solution_type_id: TypeId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        let infer_depth = self.inference_context.origin_stack.len();
        let constraint_impls = self.get_constrained_ability_impls_for_type(type_param_id);
        if !constraint_impls.is_empty() {
            debug!(
                "[infer {infer_depth}] > learning from param {} := {}. constraints: {}",
                self.type_id_to_string(type_param_id),
                self.type_id_to_string(solution_type_id),
                constraint_impls.len()
            );
        }

        // Used for fixing up the constraint signatures:
        // For each param they mention, if its solved, use the solution, otherwise use the
        // inference hole so we can learn more about it
        // This is probably a re-usable concept for inference context, I think its probably quite
        // often that we want this mapping for each param
        let mut subst_set: SV8<TypeSubstitutionPair> = smallvec![];
        for (param, inference_hole) in
            self.inference_context.params.iter().zip(self.inference_context.inference_vars.iter())
        {
            if let Some(solution) =
                self.inference_context.solutions_so_far.iter().find(|p| p.from == *param)
            {
                subst_set.push(*solution)
            } else {
                subst_set.push(spair! { *param => *inference_hole })
            }
        }
        for constraint in &constraint_impls {
            let constraint_signature = self.ability_impls.get(constraint.full_impl_id).signature();

            let sig = self.substitute_in_ability_signature(
                &subst_set,
                constraint_signature,
                scope_id,
                span,
            );
            debug!(
                "[infer {infer_depth}] trying to learn from sig: {}",
                self.ability_signature_to_string(sig)
            );
            match self.find_or_generate_specialized_ability_impl_for_type(
                solution_type_id,
                sig.specialized_ability_id,
                scope_id,
                span,
            ) {
                Ok(impl_id) => {
                    let the_impl = self.ability_impls.get(impl_id.full_impl_id);
                    let ability_arg_iterator = self
                        .named_types
                        .copy_slice_sv::<4>(
                            self.abilities.get(sig.specialized_ability_id).kind.arguments(),
                        )
                        .into_iter()
                        .zip(self.named_types.copy_slice_sv::<4>(
                            self.abilities.get(the_impl.ability_id).kind.arguments(),
                        ));
                    let impl_arg_iterator = self
                        .named_types
                        .copy_slice_sv::<4>(sig.impl_arguments)
                        .into_iter()
                        .zip(self.named_types.copy_slice_sv::<4>(the_impl.impl_arguments));
                    for (constrained_type, found_impl_type) in
                        ability_arg_iterator.chain(impl_arg_iterator)
                    {
                        debug!(
                            "[infer {infer_depth}] I will unify impl args {} and {}",
                            self.named_type_to_string(constrained_type),
                            self.named_type_to_string(found_impl_type)
                        );
                        match self.unify_and_find_substitutions(
                            found_impl_type.type_id,
                            constrained_type.type_id,
                        ) {
                            TypeUnificationResult::Matching => {
                                debug!("[infer {infer_depth}] unify succeeded",);
                                self.calculate_inference_substitutions(span)?;
                            }
                            TypeUnificationResult::NoHoles => {}
                            TypeUnificationResult::NonMatching(_) => {
                                eprintln!("did not match; likely we'll fail later")
                            }
                        }
                    }
                }
                Err(msg) => {
                    let signature = self.ability_impls.get(constraint.full_impl_id).signature();
                    return failf!(
                        span,
                        "Could not satisfy ability constraint {} for given type {} := {} due to: {msg}",
                        self.ability_signature_to_string(signature),
                        self.type_id_to_string(type_param_id),
                        self.type_id_to_string(solution_type_id)
                    );
                }
            };
        }
        Ok(())
    }

    fn infer_and_constrain_call_type_args(
        &mut self,
        fn_call: &ParsedCall,
        generic_function_sig: FunctionSignature,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        debug_assert!(generic_function_sig.is_generic());
        let passed_type_args = fn_call.type_args;
        let passed_type_args_count = passed_type_args.len();
        let solved_type_params = if !passed_type_args.is_empty() {
            if passed_type_args_count != generic_function_sig.type_params.len() {
                return failf!(
                    fn_call.span,
                    "Expected {} type arguments but got {}",
                    generic_function_sig.type_params.len(),
                    passed_type_args_count
                );
            }
            let mut evaled_params: SV4<NameAndType> =
                SmallVec::with_capacity(passed_type_args_count);
            let type_args_owned = self.ast.p_type_args.copy_slice_sv::<4>(passed_type_args);
            for (type_param, type_arg) in self
                .named_types
                .copy_slice_sv::<4>(generic_function_sig.type_params)
                .iter()
                .zip(type_args_owned.iter())
            {
                let passed_type = self.eval_type_expr(type_arg.type_expr, ctx.scope_id)?;
                evaled_params.push(NameAndType { name: type_param.name, type_id: passed_type });
            }
            self.named_types.add_slice_from_copy_slice(&evaled_params)
        } else {
            let generic_function_type =
                self.types.get(generic_function_sig.function_type).as_function().unwrap().clone();
            let generic_function_return_type = generic_function_type.return_type;
            let generic_function_params = generic_function_type.logical_params();
            let args_and_params = self.align_call_arguments_with_parameters(
                fn_call,
                generic_function_params,
                known_args.map(|(_known_types, known_args)| known_args),
                ctx.scope_id,
                true,
            )?;
            let mut inference_pairs: SmallVec<[_; 8]> = match ctx.expected_type_id {
                None => SmallVec::with_capacity(args_and_params.len()),
                Some(expected) => {
                    let mut v = SmallVec::with_capacity(args_and_params.len() + 1);
                    // One Inference Pair for the return type
                    v.push(InferenceInputPair {
                        arg: TypeOrParsedExpr::Type(expected),
                        param_type: generic_function_return_type,
                        allow_mismatch: true,
                    });
                    v
                }
            };
            // An Inference Pair for each parameter/argument pair
            inference_pairs.extend(args_and_params.iter().map(|(expr, param)| {
                let passed_type = match expr {
                    MaybeTypedExpr::Parsed(expr_id) => TypeOrParsedExpr::Parsed(*expr_id),
                    MaybeTypedExpr::Typed(expr) => {
                        TypeOrParsedExpr::Type(self.exprs.get(*expr).get_type())
                    }
                };
                InferenceInputPair {
                    arg: passed_type,
                    param_type: param.type_id,
                    allow_mismatch: false,
                }
            }));

            // Inference pairs for everything we learn from the constraints

            let (solutions, _all_solutions) = self
                .infer_types(
                    generic_function_sig.type_params,
                    generic_function_sig.type_params,
                    &inference_pairs,
                    fn_call.span,
                    ctx.scope_id,
                )
                .map_err(|e| {
                    errf!(
                        e.span,
                        "Invalid call to {}. {}",
                        self.ident_str_opt(generic_function_sig.name),
                        e.message,
                    )
                })?;
            solutions
        };

        // Enforce ability constraints
        let params_to_solutions_pairs: SV4<TypeSubstitutionPair> = self
            .zip_named_types_to_subst_pairs(generic_function_sig.type_params, solved_type_params);
        for (solution, type_param) in self
            .named_types
            .copy_slice_sv::<4>(solved_type_params)
            .iter()
            .zip(self.named_types.copy_slice_sv::<4>(generic_function_sig.type_params).iter())
        {
            self.check_type_constraints(
                type_param.name,
                type_param.type_id,
                solution.type_id,
                &params_to_solutions_pairs,
                ctx.scope_id,
                fn_call.span,
            )
            .map_err(|e| {
                errf!(
                    e.span,
                    "{}. Therefore, cannot call function '{}' with given types: {}",
                    e.message,
                    self.ident_str(fn_call.name.name),
                    self.pretty_print_named_type_slice(solved_type_params, ", ")
                )
            })?;
        }
        Ok(solved_type_params)
    }

    fn substitute_in_ability_signature(
        &mut self,
        set: &[TypeSubstitutionPair],
        signature: TypedAbilitySignature,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedAbilitySignature {
        let specialized_ability = self.abilities.get(signature.specialized_ability_id);
        let base_ability_id = specialized_ability.base_ability_id;
        let base_ability = self.abilities.get(base_ability_id);
        let all_base_params = base_ability.parameters.clone();
        if all_base_params.is_empty() {
            // Special case if the ability has no params at all, e.g., Comparable
            return signature;
        }
        let old_impl_arguments = signature.impl_arguments;
        let old_impl_ability_arguments = specialized_ability.kind.arguments();
        debug!(
            "Specializing constraint sig: {} on set {}",
            self.ability_impl_signature_to_string(
                signature.specialized_ability_id,
                old_impl_arguments
            ),
            self.pretty_print_type_substitutions(set, ", ")
        );
        let mut ability_args_new: SV8<NameAndType> = smallvec![];
        let mut impl_args_new: SV4<NameAndType> = smallvec![];
        for (index, ability_param) in
            all_base_params.iter().filter(|p| !p.is_impl_param).enumerate()
        {
            let previous_value = self.named_types.get_nth(old_impl_ability_arguments, index);
            let substituted = self.substitute_in_type(previous_value.type_id, set);
            ability_args_new.push(NameAndType { name: ability_param.name, type_id: substituted });
            debug!(
                "> Did ability param {} -> {}",
                self.type_id_to_string_ext(ability_param.type_variable_id, true),
                self.type_id_to_string_ext(substituted, true)
            );
        }
        for (index, impl_param) in all_base_params.iter().filter(|p| p.is_impl_param).enumerate() {
            let previous_value = self.named_types.get_nth(old_impl_arguments, index);
            let substituted = self.substitute_in_type(previous_value.type_id, set);
            impl_args_new.push(NameAndType { name: impl_param.name, type_id: substituted });
            debug!(
                "> Did impl param {} -> {}",
                self.type_id_to_string_ext(impl_param.type_variable_id, true),
                self.type_id_to_string_ext(substituted, true)
            );
        }
        let ability_args_new_handle = self.named_types.add_slice_from_copy_slice(&ability_args_new);
        let impl_args_new_handle = self.named_types.add_slice_from_copy_slice(&impl_args_new);
        let specialized_base =
            self.specialize_ability(base_ability_id, ability_args_new_handle, span, scope_id);
        TypedAbilitySignature {
            specialized_ability_id: specialized_base,
            impl_arguments: impl_args_new_handle,
        }
    }

    fn determine_function_type_args_for_call(
        &mut self,
        original_function_sig: FunctionSignature,
        type_args: NamedTypeSlice,
        args_and_params: &ArgsAndParams,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        // Ok here's what we need for function params. We need to know just the _kind_ of function that
        // was passed: ref, lambda, or lambda obj, and we need to specialize the function shape on
        // the other type params, as in: some (T -> T) -> some (int -> int), THEN just create
        // a type using the _kind_ we need:
        // fn ref: some (int -> int) -> (int -> int)*
        // lambda: some (int -> int) -> unique(\int -> int) (is this the zero-sized param? Oh, its physically just passing the environment!)
        // lambda obj: some (int -> int) -> dyn[\int -> int] (passing environment AND fn ptr)
        //
        // This method is risk-free in terms of 'leaking' inference types out
        let mut function_type_args: SmallVec<[NameAndType; 8]> = SmallVec::new();
        if !original_function_sig.function_type_params.is_empty() {
            let subst_pairs: SmallVec<[_; 8]> = self
                .named_types
                .get_slice(original_function_sig.type_params)
                .iter()
                .zip(self.named_types.get_slice(type_args).iter())
                .map(|(param, arg)| TypeSubstitutionPair { from: param.type_id, to: arg.type_id })
                .collect();
            for function_type_param in self
                .existential_type_params
                .get_slice_to_smallvec::<4>(original_function_sig.function_type_params)
                .iter()
            {
                let (corresponding_arg, corresponding_value_param) =
                    args_and_params.get(function_type_param.value_param_index as usize);
                debug!(
                    "The param for function_type_param {} {} is {} and passed: {:?}",
                    function_type_param.type_id,
                    self.ident_str(function_type_param.name),
                    self.ident_str(corresponding_value_param.name),
                    corresponding_arg
                );

                enum PhysicalPassedFunction {
                    Lambda(TypeId),
                    FunctionReference,
                    LambdaObject(TypeId),
                }
                let physical_passed_function = match corresponding_arg {
                    MaybeTypedExpr::Typed(_) => {
                        unreachable!("Synthesizing calls with function type params is unsupported")
                    }
                    MaybeTypedExpr::Parsed(p) => match self.ast.exprs.get(*p) {
                        ParsedExpr::Lambda(_lam) => {
                            debug!("substituting type for an ftp lambda so that it can infer");
                            let substituted_param_type =
                                self.substitute_in_type(function_type_param.type_id, &subst_pairs);
                            let the_lambda = self.eval_expr(
                                *p,
                                ctx.with_expected_type(Some(substituted_param_type)),
                            )?;
                            let lambda_type = self.exprs.get(the_lambda).get_type();
                            debug!(
                                "Using a Lambda as an ftp: {}",
                                self.type_id_to_string(lambda_type)
                            );
                            PhysicalPassedFunction::Lambda(lambda_type)
                        }
                        _other => {
                            let t = self.eval_expr(*p, ctx.with_no_expected_type())?;
                            let type_id = self.exprs.get(t).get_type();
                            match self.types.get(type_id) {
                                Type::Lambda(_) => PhysicalPassedFunction::Lambda(type_id),
                                Type::LambdaObject(_) => {
                                    debug!("Using a LambdaObject as an ftp");
                                    PhysicalPassedFunction::LambdaObject(type_id)
                                }
                                Type::Reference(_) => {
                                    debug!("Using a LambdaObject as an ftp");
                                    PhysicalPassedFunction::FunctionReference
                                }
                                _ => {
                                    let span = self.exprs.get(t).get_span();
                                    return failf!(
                                        span,
                                        "Expected {}, which is an existential function type (lambdas, dynamic lambdas, and function pointers all work), but got: {}",
                                        self.type_id_to_string(function_type_param.type_id),
                                        self.type_id_to_string(type_id)
                                    );
                                }
                            }
                        }
                    },
                };
                let final_parameter_type = match physical_passed_function {
                    PhysicalPassedFunction::Lambda(lambda) => {
                        // Can use as-is since we rebuilt this lambda already
                        lambda
                    }
                    PhysicalPassedFunction::FunctionReference => {
                        let ftp = self
                            .types
                            .get(function_type_param.type_id)
                            .as_function_type_parameter()
                            .unwrap();
                        let original_param_function_type = ftp.function_type;
                        let substituted_function_type =
                            self.substitute_in_type(original_param_function_type, &subst_pairs);
                        self.types.add_reference_type(substituted_function_type)
                    }
                    PhysicalPassedFunction::LambdaObject(lambda_object_type) => {
                        // Replace the function type
                        let substituted_lambda_object_type =
                            self.substitute_in_type(lambda_object_type, &subst_pairs);
                        substituted_lambda_object_type
                    }
                };
                function_type_args.push(NameAndType {
                    name: function_type_param.name,
                    type_id: final_parameter_type,
                });
            }
        }
        if !function_type_args.is_empty() {
            debug!(
                "We're passing function_type_args! {}",
                self.pretty_print_named_types(&function_type_args, ", ")
            );
        }
        let function_type_args_handle =
            self.named_types.add_slice_from_copy_slice(&function_type_args);
        Ok(function_type_args_handle)
    }

    fn determine_static_type_args_for_call(
        &mut self,
        original_function_sig: FunctionSignature,
        args_and_params: &ArgsAndParams,
        ctx: EvalExprContext,
    ) -> TyperResult<NamedTypeSlice> {
        let mut static_type_args: SmallVec<[NameAndType; 8]> = SmallVec::new();
        if !original_function_sig.static_type_params.is_empty() {
            for static_type_param in self
                .existential_type_params
                .get_slice_to_smallvec::<4>(original_function_sig.static_type_params)
                .iter()
            {
                let (corresponding_arg, corresponding_value_param) =
                    args_and_params.get(static_type_param.value_param_index as usize);
                debug!(
                    "The param for static_type_param {} {} is {} and passed: {:?}",
                    static_type_param.type_id,
                    self.ident_str(static_type_param.name),
                    self.ident_str(corresponding_value_param.name),
                    corresponding_arg
                );

                let expr_id = match corresponding_arg {
                    MaybeTypedExpr::Parsed(p) => {
                        self.eval_expr(*p, ctx.with_expected_type(Some(static_type_param.type_id)))?
                    }
                    MaybeTypedExpr::Typed(typed_expr_id) => *typed_expr_id,
                };
                let expr_type_id = self.exprs.get(expr_id).get_type();
                let final_parameter_type = if let Type::Static(_passed_static_type) =
                    self.types.get_no_follow_static(expr_type_id)
                {
                    expr_type_id
                } else if let Ok(lifted) = self.attempt_static_lift(expr_id) {
                    self.exprs.get(lifted).get_type()
                } else {
                    return failf!(
                        self.exprs.get(expr_id).get_span(),
                        "Expected a static value for static type parameter {}, but got {}",
                        self.ident_str(static_type_param.name),
                        self.type_id_to_string(expr_type_id)
                    );
                };
                static_type_args.push(NameAndType {
                    name: static_type_param.name,
                    type_id: final_parameter_type,
                });
            }
        }
        if !static_type_args.is_empty() {
            debug!(
                "We're passing function_type_args! {}",
                self.pretty_print_named_types(&static_type_args, ", ")
            );
        }
        let static_type_args_handle = self.named_types.add_slice_from_copy_slice(&static_type_args);
        Ok(static_type_args_handle)
    }

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

    /// Returns: Any newly, fully solved params after applying constraints
    fn calculate_inference_substitutions(
        &mut self,
        span: SpanId,
    ) -> TyperResult<SV4<TypeSubstitutionPair>> {
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

        // Look for new 'fully solved' params among the set. This is used to trigger behaviors
        // sometimes. For example, once we solve a type parameter, we then look up its constraints
        // and add information from them into the inference context
        let mut newly_solved_params: SV4<TypeSubstitutionPair> = smallvec![];
        for (solved_from, solved_to) in final_pairs.iter() {
            let is_fully_solved =
                self.types.get_contained_type_variable_counts(*solved_to).inference_variable_count
                    == 0;
            if !is_fully_solved {
                continue;
            }

            // If the thing we've solved is one of the param holes themselves
            // e.g., '0
            let Some(inference_var_index) =
                ctx.inference_vars.iter().position(|t| *t == *solved_from)
            else {
                continue;
            };
            let original_param = ctx.params[inference_var_index];

            debug!(
                "final_pair {} -> {}",
                self.type_id_to_string(*solved_from),
                self.type_id_to_string(*solved_to)
            );

            // Then find the type param matching the 'from' type, T
            // let Some(type_param) = self
            //     .named_types
            //     .get_slice(all_type_params)
            //     .iter()
            //     .find(|nt| nt.type_id == original_param.from)
            //     .copied()
            // else {
            //     continue;
            // };
            //

            if !ctx.solutions_so_far.iter().any(|pair| pair.from == original_param) {
                newly_solved_params.push(spair! { original_param => *solved_to });
            }
            ctx.solutions_so_far.push(spair! { original_param => *solved_to })
        }
        self.inference_context = ctx;
        Ok(newly_solved_params)
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
            false,
        );
        self.inference_context.constraints = inference_substitutions;
        result
    }

    fn unify_and_find_substitutions_rec(
        &self,
        substitutions: &mut Vec<TypeSubstitutionPair>,
        passed_type: TypeId,
        slot_type: TypeId,
        // `type_param_enabled`: Whether or not we should look for TypeParameters. By default,
        // we just look for InferenceHoles. But there are some scenarios
        // where its useful to apply this algorithm over type parameters
        type_param_enabled: bool,
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
        let counts = self.types.type_variable_counts.get(slot_type);
        if type_param_enabled {
            if counts.type_parameter_count == 0 {
                return TypeUnificationResult::NoHoles;
            }
        } else if counts.inference_variable_count == 0 {
            debug!("no type holes: {}", self.type_id_to_string(slot_type));
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
                    passed_info.type_args.iter().zip(arg_info.type_args.iter())
                {
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        *passed_type,
                        *arg_slot,
                        type_param_enabled,
                    );
                }
            } else {
                debug!("compared generic instances but they didn't match parent types");
            }
            return TypeUnificationResult::Matching;
        }

        match (self.types.get_no_follow(passed_type), self.types.get_no_follow(slot_type)) {
            (Type::Static(static_type), _) => self.unify_and_find_substitutions_rec(
                substitutions,
                static_type.inner_type_id,
                slot_type,
                type_param_enabled,
            ),
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
                    type_param_enabled,
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
                        type_param_enabled,
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
                                type_param_enabled,
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
                    type_param_enabled,
                ),
            (passed, Type::FunctionTypeParameter(slot_function_type_param)) => {
                if let Some(passed_function_type) =
                    self.extract_function_type_from_functionlike(passed)
                {
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_function_type,
                        slot_function_type_param.function_type,
                        type_param_enabled,
                    )
                } else {
                    TypeUnificationResult::NonMatching(
                        "Expected a function type parameter; passed unrelated",
                    )
                }
            }
            (Type::Function(passed_fn), Type::Function(param_fn)) => {
                if passed_fn.logical_params().len() == param_fn.logical_params().len() {
                    for (passed_param, param_param) in
                        passed_fn.logical_params().iter().zip(param_fn.logical_params().iter())
                    {
                        self.unify_and_find_substitutions_rec(
                            substitutions,
                            passed_param.type_id,
                            param_param.type_id,
                            type_param_enabled,
                        );
                    }
                    self.unify_and_find_substitutions_rec(
                        substitutions,
                        passed_fn.return_type,
                        param_fn.return_type,
                        type_param_enabled,
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
                    type_param_enabled,
                ),
            (Type::LambdaObject(passed_lambda), Type::LambdaObject(param_lambda)) => self
                .unify_and_find_substitutions_rec(
                    substitutions,
                    passed_lambda.function_type,
                    param_lambda.function_type,
                    type_param_enabled,
                ),
            (Type::TypeParameter(_actual_param), _expected_type) if type_param_enabled => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: passed_type, to: slot_type },
                );
                TypeUnificationResult::Matching
            }
            (_actual_type, Type::TypeParameter(_expected_param)) if type_param_enabled => {
                // Note: We may eventually need an 'occurs' check to prevent recursive
                // substitutions; for now they don't seem to be occurring though, and it'll
                // be obvious if they ever do
                self.add_substitution(
                    substitutions,
                    TypeSubstitutionPair { from: slot_type, to: passed_type },
                );
                TypeUnificationResult::Matching
            }
            _ if passed_type == slot_type => TypeUnificationResult::Matching,
            _ => TypeUnificationResult::NonMatching("Unrelated types"),
        }
    }

    fn extract_function_type_from_functionlike(&self, typ: &Type) -> Option<TypeId> {
        // What can we pass when we expect a function type parameter?
        // A Function reference: fn_name.pointer()
        // A lambda: \x -> x + 1
        // A lambda-object: dyn[A -> B]
        // A function-type-parameter, written 'some \A -> B'
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
            Type::FunctionTypeParameter(ftp) => Some(ftp.function_type),
            _ => None,
        }
    }

    pub fn get_core_scope_id(&self) -> ScopeId {
        debug_assert_ne!(self.scopes.core_scope_id, ScopeId::PENDING);
        self.scopes.core_scope_id
    }

    pub fn get_k1_scope_id(&self) -> ScopeId {
        debug_assert_ne!(self.scopes.k1_scope_id, ScopeId::PENDING);
        self.scopes.k1_scope_id
    }

    fn substitute_in_function_signature(
        &mut self,
        // Must 'zip' up with each type param
        type_arguments: NamedTypeSlice,
        // Must 'zip' up with each function type param
        function_type_arguments: NamedTypeSlice,
        // Must 'zip' up with each static type param
        static_type_arguments: NamedTypeSlice,
        generic_function_sig: FunctionSignature,
    ) -> TypeId {
        //let generic_function = self.get_function(generic_function_id);
        let generic_function_type_id = generic_function_sig.function_type;
        let mut subst_pairs: SmallVec<[TypeSubstitutionPair; 8]> = SmallVec::with_capacity(
            generic_function_sig.function_type_params.len()
                + generic_function_sig.type_params.len(),
        );

        // Here, we're substituting **the entire function type params** for the function types we
        // have. The pairs look like "some T -> T" -> "(int -> int)*"
        for (function_type_param, function_type_arg) in self
            .existential_type_params
            .get_slice(generic_function_sig.function_type_params)
            .iter()
            .zip(self.named_types.get_slice(function_type_arguments))
        {
            subst_pairs.push(TypeSubstitutionPair {
                from: function_type_param.type_id,
                to: function_type_arg.type_id,
            })
        }
        // Here, we add substitution pairs for each static type param.
        // Examples:
        // static string -> static[string, "Hello, World"];
        // static int -> static[int, 42]
        for (static_type_param, static_type_arg) in self
            .existential_type_params
            .get_slice(generic_function_sig.static_type_params)
            .iter()
            .zip(self.named_types.get_slice(static_type_arguments))
        {
            subst_pairs.push(TypeSubstitutionPair {
                from: static_type_param.type_id,
                to: static_type_arg.type_id,
            })
        }

        // Here, we're substituting the actual 'normal' type params as well,
        // such as T, U in fn makePair[T, U](t: T, u: U)
        subst_pairs.extend(
            self.named_types
                .get_slice(generic_function_sig.type_params)
                .iter()
                .zip(self.named_types.get_slice(type_arguments))
                .map(|(gen_param, type_arg)| TypeSubstitutionPair {
                    from: gen_param.type_id,
                    to: type_arg.type_id,
                }),
        );
        let specialized_function_type_id =
            self.substitute_in_type(generic_function_type_id, &subst_pairs);
        debug!(
            "specialized function type: {}",
            self.type_id_to_string(specialized_function_type_id)
        );
        specialized_function_type_id
    }

    fn specialize_function_signature(
        &mut self,
        // Must 'zip' up with each type param
        type_arguments: NamedTypeSlice,
        // Must 'zip' up with each function type param
        function_type_arguments: NamedTypeSlice,
        // Must 'zip' up with each static type param
        static_type_arguments: NamedTypeSlice,
        generic_function_id: FunctionId,
    ) -> TyperResult<FunctionId> {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_param_variables = generic_function.param_variables.clone();
        let generic_function_scope = generic_function.scope;

        for existing_specialization in &generic_function.child_specializations {
            if self
                .named_types
                .slices_equal_copy(existing_specialization.type_arguments, type_arguments)
                && self.named_types.slices_equal_copy(
                    existing_specialization.function_type_arguments,
                    function_type_arguments,
                )
                && self.named_types.slices_equal_copy(
                    existing_specialization.static_type_arguments,
                    static_type_arguments,
                )
            {
                debug!(
                    "Found existing specialization for function {} with types: {}, functions: {}",
                    self.ident_str(generic_function.name),
                    self.pretty_print_named_type_slice(
                        existing_specialization.type_arguments,
                        ", "
                    ),
                    self.pretty_print_named_type_slice(
                        existing_specialization.function_type_arguments,
                        ", "
                    ),
                );
                return Ok(existing_specialization.specialized_function_id);
            }
        }
        let specialized_function_type_id = self.substitute_in_function_signature(
            type_arguments,
            function_type_arguments,
            static_type_arguments,
            generic_function.signature(),
        );
        debug!(
            "specialized function type: {}",
            self.type_id_to_string(specialized_function_type_id)
        );
        let specialized_name_ident = self.build_ident_with(|m, s| {
            use std::fmt::Write;
            let generic_function = m.get_function(generic_function_id);
            let spec_num = generic_function.child_specializations.len() + 1;
            write!(s, "{}__", m.ident_str(generic_function.name)).unwrap();
            for nt in m.named_types.get_slice(type_arguments) {
                m.display_type_id(nt.type_id, false, s).unwrap()
            }
            write!(s, "_{spec_num}").unwrap();
        });

        let specialized_function_type =
            self.types.get(specialized_function_type_id).as_function().unwrap();

        let spec_fn_scope = self.scopes.add_sibling_scope(
            generic_function_scope,
            ScopeType::FunctionScope,
            None,
            Some(specialized_name_ident),
        );

        for nt in self.named_types.get_slice(type_arguments) {
            let _ = self.scopes.add_type(spec_fn_scope, nt.name, nt.type_id);
        }

        let param_variables: EcoVec<VariableId> = specialized_function_type
            .physical_params
            .iter()
            .zip(generic_function_param_variables.iter())
            .map(|(specialized_param_type, generic_param)| {
                let name = self.variables.get(*generic_param).name;
                let variable_id = self.variables.add(Variable {
                    type_id: specialized_param_type.type_id,
                    name,
                    is_mutable: false,
                    owner_scope: spec_fn_scope,
                    is_context: specialized_param_type.is_context,
                    global_id: None,
                    user_hidden: false,
                });
                if specialized_param_type.is_context {
                    self.scopes.add_context_variable(
                        spec_fn_scope,
                        name,
                        variable_id,
                        specialized_param_type.type_id,
                    );
                }
                self.scopes.add_variable(spec_fn_scope, name, variable_id);
                variable_id
            })
            .collect();
        let specialization_info = SpecializationInfo {
            parent_function: generic_function_id,
            type_arguments,
            function_type_arguments,
            static_type_arguments,
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
            name: specialized_name_ident,
            scope: spec_fn_scope,
            param_variables,
            // Must be empty for correctness; a specialized function has no type parameters!
            type_params: SliceHandle::Empty,
            // Must be empty for correctness; a specialized function has no function type parameters!
            function_type_params: SliceHandle::Empty,
            // Must be empty for correctness; a specialized function has no static type parameters!
            static_type_params: SliceHandle::Empty,
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
            dyn_fn_id: None,
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
        if specialized_function.body_block.is_some() {
            return Ok(());
        }
        let specialized_return_type = self.get_function_type(function_id).return_type;
        let specialized_function_type = specialized_function.type_id;
        let specialized_function_scope_id = specialized_function.scope;
        let parent_function = specialized_function
            .specialization_info
            .as_ref()
            .map(|spec_info| spec_info.parent_function)
            .or(specialized_function.kind.blanket_parent_function_id())
            .expect(
                "specialize_function_body wants a normal specialization or a blanket impl defn",
            );
        //eprintln!("specialize function {}", self.function_id_to_string(parent_function, true));
        let parent_function = self.get_function(parent_function);
        debug_assert!(parent_function.body_block.is_some());
        debug_assert!(specialized_function.body_block.is_none());

        // Approach 1: Re-run whole body w/ bound types
        // Downside: cloning, extra work, etc
        // Upside: way way less code.
        // Have to bind type names that shouldn't exist, kinda
        let parsed_body = *self
            .ast
            .get_function(parent_function.parsed_id.as_function_id().unwrap())
            .block
            .as_ref()
            .unwrap();
        let typed_body = self.eval_expr(
            parsed_body,
            EvalExprContext::make(specialized_function_scope_id)
                .with_expected_type(Some(specialized_return_type)),
        )?;

        let body_type = self.exprs.get(typed_body).get_type();
        if let Err(msg) =
            self.check_types(specialized_return_type, body_type, specialized_function_scope_id)
        {
            return failf!(
                self.get_span_for_expr_type(typed_body),
                "Function body type mismatch: {}\n specialized signature is: {}",
                msg,
                self.type_id_to_string(specialized_function_type)
            );
        }

        self.get_function_mut(function_id).body_block = Some(typed_body);
        Ok(())
    }

    /// Used to drill down to the span that is responsible for the type of the given expression
    /// For example, the last statement of a block, rather than the entire span
    pub fn get_span_for_expr_type(&self, typed_expr_id: TypedExprId) -> SpanId {
        match self.exprs.get(typed_expr_id) {
            TypedExpr::Block(typed_block) => match typed_block.statements.last() {
                None => typed_block.span,
                Some(stmt) => match self.stmts.get(*stmt) {
                    TypedStmt::Expr(typed_expr_id, _) => {
                        self.get_span_for_expr_type(*typed_expr_id)
                    }
                    TypedStmt::Let(let_stmt) => let_stmt.span,
                    TypedStmt::Assignment(a) => a.span,
                    TypedStmt::Require(r) => r.span,
                },
            },
            TypedExpr::Match(typed_match_expr) => {
                self.get_span_for_expr_type(typed_match_expr.arms.first().unwrap().consequent_expr)
            }
            TypedExpr::Return(r) => self.get_span_for_expr_type(r.value),
            e => e.get_span(),
        }
    }

    pub fn is_function_concrete(&self, function: &TypedFunction) -> bool {
        if let Some(intrinsic) = function.intrinsic_type {
            if intrinsic.is_inlined() {
                return false;
            }
        }
        if function.is_generic() {
            return false;
        }
        let info = self.types.get_contained_type_variable_counts(function.type_id);
        let has_no_abstract_types_in_signature =
            info.type_parameter_count == 0 && info.inference_variable_count == 0;
        has_no_abstract_types_in_signature
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
                let Some(useable_symbol) =
                    self.find_useable_symbol(ctx.scope_id, &parsed_use.target)?
                else {
                    return failf!(
                        parsed_use.target.span,
                        "Could not find {}",
                        self.ident_str(parsed_use.target.name)
                    );
                };
                self.scopes.add_use_binding(
                    ctx.scope_id,
                    useable_symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                Ok(None)
            }
            ParsedStmt::Let(parsed_let) => {
                static_assert_size!(parse::ParsedLet, 20);
                let parsed_let = parsed_let.clone();
                let provided_type = match parsed_let.type_expr.as_ref() {
                    None => None,
                    Some(&type_expr) => Some(self.eval_type_expr_ext(
                        type_expr,
                        ctx.scope_id,
                        EvalTypeExprContext::VARIABLE_BINDING,
                    )?),
                };
                let expected_rhs_type = match provided_type {
                    Some(provided_type) => {
                        if parsed_let.is_referencing() {
                            let Type::Reference(expected_reference_type) =
                                self.types.get(provided_type)
                            else {
                                let expected_type_span =
                                    self.ast.get_type_expr_span(parsed_let.type_expr.unwrap());
                                return failf!(
                                    expected_type_span,
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
                    self.eval_expr(parsed_let.value, ctx.with_expected_type(expected_rhs_type))?;
                let actual_type = self.exprs.get(value_expr).get_type();

                if let Some(expected_type) = expected_rhs_type {
                    if let Err(msg) = self.check_types(expected_type, actual_type, ctx.scope_id) {
                        return failf!(parsed_let.span, "Local variable type mismatch: {}", msg,);
                    }
                };

                let variable_type = if parsed_let.is_referencing() {
                    self.types.add_reference_type(actual_type)
                } else {
                    actual_type
                };

                let variable_id = self.variables.add(Variable {
                    is_mutable: parsed_let.is_mutable(),
                    name: parsed_let.name,
                    type_id: variable_type,
                    owner_scope: ctx.scope_id,
                    is_context: parsed_let.is_context(),
                    global_id: None,
                    user_hidden: false,
                });
                let val_def_stmt = TypedStmt::Let(LetStmt {
                    variable_type,
                    variable_id,
                    initializer: value_expr,
                    is_referencing: parsed_let.is_referencing(),
                    span: parsed_let.span,
                });
                if parsed_let.is_context() {
                    self.scopes.add_context_variable(
                        ctx.scope_id,
                        parsed_let.name,
                        variable_id,
                        variable_type,
                    );
                } else {
                    self.scopes.add_variable(ctx.scope_id, parsed_let.name, variable_id);
                }
                let stmt_id = self.stmts.add(val_def_stmt);
                Ok(Some(stmt_id))
            }
            ParsedStmt::Require(require) => {
                static_assert_size!(parse::ParsedRequire, 12);
                let require = require.clone();
                let condition = self
                    .eval_matching_condition(require.condition_expr, ctx.with_no_expected_type())?;

                let else_scope =
                    self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);

                // Make the binding variables unavailable in the else scope
                for instr in &condition.instrs {
                    if let MatchingConditionInstr::Binding { let_stmt, variable_id: _variable_id } =
                        instr
                    {
                        let stmt = self.stmts.get(*let_stmt).as_let().unwrap();
                        let variable = self.variables.get(stmt.variable_id);

                        if !variable.user_hidden {
                            let else_scope = self.scopes.get_scope_mut(else_scope);
                            else_scope.mask_variable(variable.name);
                        }
                    }
                }

                let else_body = self.eval_expr(require.else_body, ctx.with_scope(else_scope))?;
                if self.exprs.get(else_body).get_type() != NEVER_TYPE_ID {
                    let else_span = self.exprs.get(else_body).get_span();
                    return failf!(else_span, "else branch must diverge; try returning or exiting");
                }

                let id = self.stmts.add(TypedStmt::Require(TypedRequireStmt {
                    condition: Box::new(condition),
                    else_body,
                    span: require.span,
                }));
                Ok(Some(id))
            }
            ParsedStmt::Assignment(assignment) => {
                static_assert_size!(parse::Assignment, 12);
                let assignment = assignment.clone();
                let ParsedExpr::Variable(_) = self.ast.exprs.get(assignment.lhs) else {
                    return failf!(
                        self.ast.exprs.get_span(assignment.lhs),
                        "Value assignment destination must be a variable"
                    );
                };
                let (_, lhs) = self.eval_variable(assignment.lhs, ctx.scope_id, true)?;
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
        block: &ParsedBlock,
        ctx: EvalExprContext,
        needs_terminator: bool,
    ) -> TyperResult<TypedBlock> {
        if block.stmts.is_empty() {
            return failf!(block.span, "Blocks must contain at least one statement or expression",);
        }
        let mut stmts: EcoVec<TypedStmtId> = EcoVec::with_capacity(block.stmts.len());
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

            // Ensure termination because this is a 'real' control flow block
            // Not just a lexical block the user made. For example, a function body.
            if is_last && needs_terminator {
                if last_stmt_is_divergent {
                    // No action needed; terminator exists
                    stmts.push(stmt_id);
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
                            stmts.push(return_stmt);
                        }
                        TypedStmt::Assignment(_) | TypedStmt::Let(_) | TypedStmt::Require(_) => {
                            let unit = self.exprs.add(TypedExpr::Unit(stmt_span));
                            let return_unit_expr = self.exprs.add(TypedExpr::Return(TypedReturn {
                                span: stmt_span,
                                value: unit,
                            }));
                            let return_unit = TypedStmt::Expr(return_unit_expr, NEVER_TYPE_ID);
                            let return_unit_id = self.stmts.add(return_unit);
                            stmts.push(stmt_id);
                            stmts.push(return_unit_id);
                        }
                    };
                }
            } else {
                stmts.push(stmt_id);
            }
        }

        let typed_block = TypedBlock {
            expr_type: last_expr_type,
            scope_id: ctx.scope_id,
            statements: stmts,
            span: block.span,
        };
        Ok(typed_block)
    }

    fn resolve_intrinsic_function_type(
        &self,
        fn_name: Ident,
        namespace_chain: &[Ident],
        ability_impl_info: Option<(AbilityId, TypeId)>,
    ) -> Result<IntrinsicOperation, String> {
        let fn_name_str = self.ast.idents.get_name(fn_name);
        let second = namespace_chain.get(2).map(|id| self.ident_str(*id));
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
                    "bitNot" => Some(IntrinsicOperation::BitNot),
                    "bitAnd" => {
                        Some(IntrinsicOperation::BitwiseBinop(IntrinsicBitwiseBinopKind::And))
                    }
                    "bitOr" => {
                        Some(IntrinsicOperation::BitwiseBinop(IntrinsicBitwiseBinopKind::Or))
                    }
                    "xor" => Some(IntrinsicOperation::BitwiseBinop(IntrinsicBitwiseBinopKind::Xor)),
                    "shiftLeft" => {
                        Some(IntrinsicOperation::BitwiseBinop(IntrinsicBitwiseBinopKind::ShiftLeft))
                    }
                    "shiftRight" => Some(IntrinsicOperation::BitwiseBinop(
                        IntrinsicBitwiseBinopKind::ShiftRight,
                    )),
                    _ => None,
                },
                _ => None,
            }
        } else {
            #[allow(clippy::match_single_binding)]
            match second {
                // _root
                None => match fn_name_str {
                    _ => None,
                },
                Some("sys") => match fn_name_str {
                    "exit" => Some(IntrinsicOperation::Exit),
                    _ => None,
                },
                Some("mem") => match fn_name_str {
                    "alloc" => Some(IntrinsicOperation::Allocate),
                    "allocZeroed" => Some(IntrinsicOperation::AllocateZeroed),
                    "realloc" => Some(IntrinsicOperation::Reallocate),
                    "free" => Some(IntrinsicOperation::Free),
                    "zeroed" => Some(IntrinsicOperation::Zeroed),
                    "copy" => Some(IntrinsicOperation::MemCopy),
                    "set" => Some(IntrinsicOperation::MemSet),
                    "equals" => Some(IntrinsicOperation::MemEquals),
                    _ => None,
                },
                Some("types") => match fn_name_str {
                    "typeId" => Some(IntrinsicOperation::TypeId),
                    "typeName" => Some(IntrinsicOperation::TypeName),
                    "typeSchema" => Some(IntrinsicOperation::TypeSchema),
                    "sizeOf" => Some(IntrinsicOperation::SizeOf),
                    "sizeOfStride" => Some(IntrinsicOperation::SizeOfStride),
                    "alignOf" => Some(IntrinsicOperation::AlignOf),
                    _ => None,
                },
                Some("compiler") => match fn_name_str {
                    "location" => Some(IntrinsicOperation::CompilerSourceLocation),
                    _ => None,
                },
                Some("bool") => match fn_name_str {
                    "negated" => Some(IntrinsicOperation::BoolNegate),
                    _ => None,
                },
                Some("string") => None,
                Some("List") => None,
                Some("char") => None,
                Some("Pointer") => match fn_name_str {
                    "refAtIndex" => Some(IntrinsicOperation::PointerIndex),
                    _ => None,
                },
                Some("meta") => match fn_name_str {
                    "emit" => Some(IntrinsicOperation::EmitString),
                    "bakeStaticValue" => Some(IntrinsicOperation::BakeStaticValue),
                    "getStaticValue" => Some(IntrinsicOperation::GetStaticValue),
                    _ => None,
                },
                Some("k1") => match fn_name_str {
                    "emitCompilerMessage" => Some(IntrinsicOperation::CompilerMessage),
                    _ => None,
                },
                Some(_) => None,
            }
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Could not resolve intrinsic function type for function {}/{}",
                namespace_chain
                    .iter()
                    .map(|i| self.ident_str(*i).to_string())
                    .collect::<Vec<_>>()
                    .join("/"),
                fn_name_str,
            )),
        }
    }

    fn eval_enum_constructor(
        &mut self,
        concrete_enum_type: TypeId,
        variant_name: Ident,
        payload: Option<ParsedExprId>,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let e = self.types.get(concrete_enum_type).expect_enum();
        let Some(variant) = e.variant_by_name(variant_name) else {
            return failf!(
                span,
                "No variant '{}' exists in enum '{}'",
                self.ident_str(variant_name).blue(),
                self.type_id_to_string(concrete_enum_type)
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
                        self.ident_str(variant_name).blue()
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
                        self.ident_str(variant_name).blue()
                    )
                }
            }
        }?;
        let never_payload = payload.is_some_and(|p| self.exprs.get(p).get_type() == NEVER_TYPE_ID);
        let output_type = if never_payload { NEVER_TYPE_ID } else { variant_type_id };

        let enum_constructor = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            variant_type_id: output_type,
            variant_index,
            payload,
            span,
        }));
        let casted_expr = match ctx.expected_type_id.map(|t| self.types.get(t)) {
            Some(Type::EnumVariant(ev)) if ev.my_type_id == variant_type_id => {
                debug!(
                    "enum constructor output type is the variant type: {}",
                    self.type_id_to_string(variant_type_id)
                );
                enum_constructor
            }
            _ => {
                if never_payload {
                    // No need to cast it anyway
                    enum_constructor
                } else {
                    debug!(
                        "casted enum constructor to its enum type: {}",
                        self.type_id_to_string(concrete_enum_type)
                    );
                    self.synth_cast(enum_constructor, concrete_enum_type, CastType::VariantToEnum)
                }
            }
        };

        Ok(casted_expr)
    }

    fn check_type_constraint(
        &mut self,
        target_type: TypeId,
        signature: TypedAbilitySignature,
        name: Ident,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        debug!(
            "Checking constraint {}: {}",
            self.type_id_to_string(target_type,),
            self.ability_impl_signature_to_string(
                signature.specialized_ability_id,
                signature.impl_arguments
            )
        );
        if let Ok(impl_handle) = self.find_or_generate_specialized_ability_impl_for_type(
            target_type,
            signature.specialized_ability_id,
            scope_id,
            span,
        ) {
            let found_impl = self.ability_impls.get(impl_handle.full_impl_id);
            debug_assert!(signature.impl_arguments.len() == found_impl.impl_arguments.len());
            for (constraint_arg, passed_arg) in self
                .named_types
                .get_slice(signature.impl_arguments)
                .iter()
                .zip(self.named_types.get_slice(found_impl.impl_arguments).iter())
            {
                debug_assert!(constraint_arg.name == passed_arg.name);

                if self.get_type_id_resolved(constraint_arg.type_id, scope_id)
                    != self.get_type_id_resolved(passed_arg.type_id, scope_id)
                {
                    return failf!(
                        span,
                        "Provided type {} := {} does implement required ability {}, but the implementation parameter {} is wrong: Expected type was {} but the actual implementation uses {}",
                        self.ident_str(name),
                        self.type_id_to_string(target_type),
                        self.ident_str(self.abilities.get(signature.specialized_ability_id).name),
                        self.ident_str(constraint_arg.name),
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
                self.ident_str(name),
                self.type_id_to_string(target_type),
                self.ident_str(self.abilities.get(signature.specialized_ability_id).name)
            )
        }
    }

    fn check_type_constraints(
        &mut self,
        param_name: Ident,
        param_type: TypeId,
        passed_type: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<()> {
        let constraints = self.get_constrained_ability_impls_for_type(param_type);
        for constraint in &constraints {
            let original_signature = TypedAbilitySignature {
                specialized_ability_id: constraint.specialized_ability_id,
                impl_arguments: self.ability_impls.get(constraint.full_impl_id).impl_arguments,
            };
            let signature = if substitution_pairs.is_empty() {
                original_signature
            } else {
                let s = self.substitute_in_ability_signature(
                    substitution_pairs,
                    original_signature,
                    scope_id,
                    span,
                );
                debug!(
                    "I specialized an ability constraint for checking: {}",
                    self.ability_signature_to_string(s)
                );
                s
            };
            self.check_type_constraint(passed_type, signature, param_name, scope_id, span)?;
        }
        Ok(())
    }

    fn check_ability_arguments(
        &mut self,
        ability_id: AbilityId,
        arguments: NamedTypeSlice,
        span: SpanId,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> TyperResult<(NamedTypeSlice, NamedTypeSlice)> {
        let ability = self.abilities.get(ability_id);
        let ability_parameters = ability.parameters.clone();

        // Catch unrecognized arguments first
        for arg in self.named_types.get_slice(arguments) {
            let has_matching_param = ability_parameters.iter().any(|param| param.name == arg.name);
            if !has_matching_param {
                return failf!(span, "No parameter named {}", self.ident_str(arg.name));
            }
        }

        let mut ability_arguments: SV8<NameAndType> = smallvec![];
        let mut impl_arguments: SV8<NameAndType> = SmallVec::with_capacity(arguments.len());
        let mut subst_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        for param in
            ability_parameters.iter().filter(|p| !skip_impl_check || p.is_ability_side_param())
        {
            let Some(matching_arg) = self
                .named_types
                .get_slice(arguments)
                .iter()
                .find(|a| a.name == param.name)
                .copied()
            else {
                return failf!(
                    span,
                    "Missing argument for ability parameter {}",
                    self.ident_str(param.name)
                );
            };
            if param.is_impl_param {
                impl_arguments.push(matching_arg)
            } else {
                ability_arguments.push(matching_arg)
            };
            subst_pairs.push(spair! { param.type_variable_id => matching_arg.type_id });
        }

        for (param, pair) in ability_parameters
            .iter()
            .filter(|p| !skip_impl_check || p.is_ability_side_param())
            .zip(subst_pairs.iter())
        {
            // Ensure that the passed type meets the parameter's declared constraints
            // We have to pass in the substitution set to 'instantiate' the constraint for the
            // actual types provided. Example:
            // ability Iterable[impl Item, impl I: Iterator[Item = Item]]
            // And Iterable[bool, ListIterator[bool]]
            // Obviously we don't want to check that I: Iterator[Item],
            // but that I: Iterator[bool], so we have to replace 'Item' with 'bool'
            // which is exactly the mapping contained in subst_pairs
            debug!("check_ability_arguments check_type_constraints");
            self.check_type_constraints(
                param.name,
                param.type_variable_id,
                pair.to,
                &subst_pairs,
                scope_id,
                span,
            )?;
        }

        let ability_arguments_handle =
            self.named_types.add_slice_from_copy_slice(&ability_arguments);
        let impl_arguments_handle = self.named_types.add_slice_from_copy_slice(&impl_arguments);
        Ok((ability_arguments_handle, impl_arguments_handle))
    }

    fn specialize_ability(
        &mut self,
        ability_id: AbilityId,
        arguments: NamedTypeSlice,
        span: SpanId,
        parent_scope_id: ScopeId,
    ) -> AbilityId {
        let ability = self.abilities.get(ability_id);
        if ability.kind.is_concrete() {
            return ability_id;
        }
        let generic_ability_id = ability_id;
        let ability_ast_id = ability.ast_id;
        let ability_name = ability.name;
        let ability_parameters = ability.parameters.clone();
        let ability_namespace_id = ability.namespace_id;
        let specializations = self.abilities.get(generic_ability_id).kind.specializations();
        if arguments.len() > ability_parameters.len() {
            panic!("Passed too many arguments to specialize_ability; probably passed impl args");
        }
        if let Some(cached_specialization) = specializations
            .iter()
            .find(|spec| self.named_types.slices_equal_copy(spec.arguments, arguments))
        {
            debug!(
                "Using cached ability specialization for {}",
                self.ident_str(self.abilities.get(cached_specialization.specialized_child).name)
            );
            return cached_specialization.specialized_child;
        };

        let specialized_ability_scope = self.scopes.add_child_scope(
            parent_scope_id,
            ScopeType::AbilityDefn,
            None,
            Some(ability_name),
        );

        for (arg_type, param) in
            self.named_types.copy_slice_sv::<4>(arguments).iter().zip(ability_parameters.iter())
        {
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

        let specialized_ability_id = self.abilities.next_id();
        let spec_info = AbilitySpec9nInfo {
            generic_parent: generic_ability_id,
            specialized_child: specialized_ability_id,
            arguments,
        };
        let self_ident = self.ast.idents.builtins.Self_;
        let new_self_type_id = self.add_type_parameter(
            TypeParameter { name: self_ident, scope_id: specialized_ability_scope, span },
            smallvec![],
        );
        let _ = self
            .scopes
            .get_scope_mut(specialized_ability_scope)
            .add_type(self_ident, new_self_type_id);

        let specialized_ability_id = self.abilities.add(TypedAbility {
            name: ability_name,
            base_ability_id: generic_ability_id,
            self_type_id: new_self_type_id,
            parameters: impl_params,
            functions: eco_vec![],
            scope_id: specialized_ability_scope,
            ast_id: ability_ast_id,
            namespace_id: ability_namespace_id,
            kind: TypedAbilityKind::Specialized(spec_info),
        });

        let parsed_ability = self.ast.get_ability(ability_ast_id);
        let mut specialized_functions = EcoVec::with_capacity(parsed_ability.functions.len());
        for parsed_fn in parsed_ability.functions.clone().iter() {
            let result = self.eval_function_declaration(
                *parsed_fn,
                specialized_ability_scope,
                Some(FunctionAbilityContextInfo::ability_id_only(specialized_ability_id)),
                ability_namespace_id,
            );
            if let Err(e) = result {
                self.ice("Failed while specializing ability", Some(&e))
            }
            let Ok(Some(function_id)) = result else {
                continue;
            };
            let function_name = self.get_function(function_id).name;
            specialized_functions.push(TypedAbilityFunctionRef {
                function_id,
                ability_id: specialized_ability_id,
                function_name,
            });
        }

        self.abilities.get_mut(specialized_ability_id).functions = specialized_functions;
        {
            let parent_ability = self.abilities.get_mut(generic_ability_id);
            let TypedAbilityKind::Generic { specializations } = &mut parent_ability.kind else {
                panic!("expected generic ability while specializing")
            };
            specializations.push(spec_info);
        }

        self.scopes.set_scope_owner_id(
            specialized_ability_scope,
            ScopeOwnerId::Ability(specialized_ability_id),
        );

        specialized_ability_id
    }

    fn check_ability_expr(
        &mut self,
        ability_expr_id: parse::ParsedAbilityExprId,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> TyperResult<(AbilityId, NamedTypeSlice, NamedTypeSlice)> {
        let ability_expr = self.ast.p_ability_exprs.get(ability_expr_id).clone();
        let ability_id = self.find_ability_or_declare(&ability_expr.name, scope_id)?;

        let mut arguments: SV4<NameAndType> = SmallVec::with_capacity(ability_expr.arguments.len());
        for arg in self.ast.p_type_args.copy_slice_sv4(ability_expr.arguments).iter() {
            let arg_type = self.eval_type_expr(arg.type_expr, scope_id)?;
            let Some(name) = arg.name else {
                return failf!(arg.span, "Ability arguments must all be named, for now");
            };
            arguments.push(NameAndType { name, type_id: arg_type });
        }

        let arguments_handle = self.named_types.add_slice_from_copy_slice(&arguments);
        let (ability_args, impl_args) = self.check_ability_arguments(
            ability_id,
            arguments_handle,
            ability_expr.span,
            scope_id,
            skip_impl_check,
        )?;
        Ok((ability_id, ability_args, impl_args))
    }

    fn eval_ability_expr(
        &mut self,
        ability_expr_id: parse::ParsedAbilityExprId,
        skip_impl_check: bool,
        scope_id: ScopeId,
    ) -> TyperResult<TypedAbilitySignature> {
        let (base_ability_id, ability_arguments, impl_arguments) =
            self.check_ability_expr(ability_expr_id, scope_id, skip_impl_check)?;
        let span = self.ast.p_ability_exprs.get(ability_expr_id).span;
        let new_ability_id =
            self.specialize_ability(base_ability_id, ability_arguments, span, scope_id);
        Ok(TypedAbilitySignature { specialized_ability_id: new_ability_id, impl_arguments })
    }

    fn eval_function_declaration(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        parent_scope_id: ScopeId,
        ability_info: Option<FunctionAbilityContextInfo>,
        namespace_id: NamespaceId,
    ) -> TyperResult<Option<FunctionId>> {
        let namespace = self.namespaces.get(namespace_id);
        let companion_type_id = namespace.companion_type_id;
        let parsed_function = self.ast.get_function(parsed_function_id).clone();
        if let Some(condition_expr) = parsed_function.condition {
            let condition_value =
                self.execute_static_bool(condition_expr, EvalExprContext::make(parent_scope_id))?;
            if !condition_value {
                return Ok(None);
            }
        }
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
        let parsed_type_params = &parsed_function.type_params;

        let is_ability_decl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_none());
        let is_ability_impl = ability_info.as_ref().is_some_and(|info| info.impl_info.is_some());
        let ability_id = ability_info.as_ref().map(|info| info.ability_id);
        let impl_info = ability_info.as_ref().and_then(|info| info.impl_info.as_ref());
        let ability_kind = ability_id.map(|id| &self_.abilities.get(id).kind);
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

        let name = match impl_info.as_ref() {
            Some(impl_info) => {
                use std::fmt::Write;
                let mut s = String::with_capacity(256);
                write!(
                    &mut s,
                    "{}_{}_{}_{}",
                    self_.ident_str(self_.abilities.get(ability_id.unwrap()).name),
                    self_.type_id_to_string(impl_info.self_type_id),
                    ability_id.unwrap().0,
                    self_.ident_str(parsed_function_name),
                )
                .unwrap();
                self_.ast.idents.intern(s)
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
        let mut type_params: SmallVec<[NameAndType; 8]> =
            SmallVec::with_capacity(parsed_type_params.len());
        let mut function_type_params: SmallVec<[ExistentialTypeParam; 4]> = SmallVec::new();
        let mut static_type_params: SmallVec<[ExistentialTypeParam; 4]> = SmallVec::new();

        // Inject the 'Self' type parameter
        if is_ability_decl {
            let self_type_id = self_.abilities.get(ability_id.unwrap()).self_type_id;
            type_params
                .push(NameAndType { name: self_.ast.idents.builtins.Self_, type_id: self_type_id })
        }
        for type_parameter in parsed_type_params.iter() {
            let mut ability_constraints = SmallVec::new();
            for parsed_constraint in type_parameter.constraints.iter() {
                let ability_sig = match parsed_constraint {
                    parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        self_.eval_ability_expr(*ability_expr, false, fn_scope_id)?
                    }
                };
                ability_constraints.push(ability_sig);
            }
            for param in &parsed_function.additional_where_constraints {
                if param.name == type_parameter.name {
                    let ability_id = match &param.constraint_expr {
                        parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            self_.eval_ability_expr(*ability_expr, false, fn_scope_id)?
                        }
                    };
                    ability_constraints.push(ability_id);
                }
            }
            let type_variable_id = self_.add_type_parameter(
                TypeParameter {
                    name: type_parameter.name,
                    scope_id: fn_scope_id,
                    span: type_parameter.span,
                },
                ability_constraints,
            );
            let fn_scope = self_.scopes.get_scope_mut(fn_scope_id);
            let type_param = NameAndType { name: type_parameter.name, type_id: type_variable_id };
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
        let mut param_types: EcoVec<FnParamType> =
            EcoVec::with_capacity(parsed_function_params.len());
        let mut param_variables = EcoVec::with_capacity(parsed_function_params.len());
        for (idx, fn_param) in
            parsed_function_context_params.iter().chain(parsed_function_params.iter()).enumerate()
        {
            let type_id = self_.eval_type_expr_ext(
                fn_param.type_expr,
                fn_scope_id,
                EvalTypeExprContext {
                    is_direct_function_parameter: true,
                    ..EvalTypeExprContext::EMPTY
                },
            )?;

            // Handle 'existential' type parameters. These are value parameters that
            // introduce a type parameter 'for free' inline.
            // - `some ty` function type parameter, inject the type parameter into the
            // - or a `static ty`
            match self_.types.get_no_follow_static(type_id) {
                Type::Static(_stat) => {
                    let type_expr_span = self_.ast.get_type_expr_span(fn_param.type_expr);
                    static_type_params.push(ExistentialTypeParam {
                        name: fn_param.name,
                        type_id,
                        value_param_index: idx as u32,
                        span: type_expr_span,
                        kind: ExistentialTypeParamKind::Static,
                    });
                    // There's actually no way to refer to these types by name,
                    // so we don't need to add a name to the scope
                }
                Type::FunctionTypeParameter(ftp) => {
                    function_type_params.push(ExistentialTypeParam {
                        name: ftp.name,
                        type_id,
                        value_param_index: idx as u32,
                        span: ftp.span,
                        kind: ExistentialTypeParamKind::Function,
                    });
                    // There's actually no way to refer to these types by name,
                    // so we don't need to add a name to the scope
                }
                _ => {}
            }

            // First arg Self shenanigans
            if idx == 0 {
                let name_is_self = self_.ast.idents.get_name(fn_param.name) == "self";

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

            let is_context = fn_param.modifiers.is_context();
            let variable = Variable {
                name: fn_param.name,
                type_id,
                is_mutable: false,
                owner_scope: fn_scope_id,
                is_context,
                global_id: None,
                user_hidden: false,
            };

            let variable_id = self_.variables.add(variable);
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
                if !self_.scopes.add_variable(fn_scope_id, fn_param.name, variable_id) {
                    return failf!(
                        fn_param.span,
                        "Duplicate parameter name: {}",
                        self_.ident_str(fn_param.name)
                    );
                }
            }
        }

        let intrinsic_type = if parsed_function_linkage == Linkage::Intrinsic {
            // Note(perf): name_chain isn't efficient,
            // but we don't have a lot of intrinsics
            let mut namespace_chain = self_.namespaces.name_chain(namespace_id);
            let resolved = self_
                .resolve_intrinsic_function_type(
                    parsed_function_name,
                    namespace_chain.make_contiguous(),
                    ability_id.zip(impl_self_type),
                )
                .map_err(|msg| {
                    errf!(parsed_function_span, "Error typechecking function: {}", msg,)
                })?;
            Some(resolved)
        } else {
            None
        };
        let return_type = self_.eval_type_expr(parsed_function.ret_type, fn_scope_id)?;

        // Typecheck 'main': It must take argc and argv of correct types, or nothing
        // And it must return an integer, or an Unwrap[Inner = i32]
        let is_main_fn = namespace_id == ROOT_NAMESPACE_ID
            && parsed_function_name == self_.ast.idents.builtins.main;
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
                    );
                }
            };
            match return_type {
                I64_TYPE_ID => {}
                I32_TYPE_ID => {}
                _other => {
                    let result_impl = self_.expect_ability_implementation(
                        return_type,
                        UNWRAP_ABILITY_ID,
                        fn_scope_id,
                        parsed_function_span,
                    )?;
                    let impl_args =
                        self_.ability_impls.get(result_impl.full_impl_id).impl_arguments;
                    let ok_type = self_.named_types.get_nth(impl_args, 0).type_id;
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

        let function_type_id = self_
            .types
            .add_anon(Type::Function(FunctionType { physical_params: param_types, return_type }));

        let function_id = self_.functions.next_id();

        let type_params_handle = self_.named_types.add_slice_from_copy_slice(&type_params);
        let function_type_params_handle =
            self_.existential_type_params.add_slice_from_slice(&function_type_params);
        let static_type_params_handle =
            self_.existential_type_params.add_slice_from_slice(&static_type_params);
        let actual_function_id = self_.add_function(TypedFunction {
            name,
            scope: fn_scope_id,
            param_variables,
            type_params: type_params_handle,
            function_type_params: function_type_params_handle,
            static_type_params: static_type_params_handle,
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
            dyn_fn_id: None,
        });
        debug_assert!(actual_function_id == function_id);

        if resolvable_by_name {
            if !self_.scopes.add_function(parent_scope_id, parsed_function_name, function_id) {
                let signature_span = parsed_function.signature_span;
                let error = errf!(
                    signature_span,
                    "Function name {} is taken",
                    self_.ident_str(parsed_function_name)
                );
                self_.report_error(error);
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

        Ok(Some(function_id))
    }

    pub fn eval_function_body(&mut self, declaration_id: FunctionId) -> TyperResult<()> {
        let function = self.get_function(declaration_id);
        if function.body_block.is_some() {
            return Ok(());
        }
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
        let is_generic = function.is_generic();

        let body_block = match parsed_function.block.as_ref() {
            None if is_abstract => None,
            None => return failf!(function_signature_span, "function is missing implementation"),
            Some(_) if is_abstract => {
                return failf!(function_signature_span, "unexpected function implementation");
            }
            Some(block_ast) => {
                if function.specialization_info.is_some() && !function.is_concrete {
                    debug!(
                        "Skipping typecheck of body for non-concrete specialization of {}",
                        self.function_id_to_string(declaration_id, true),
                    );
                    return Ok(());
                };
                let block = self.eval_expr(
                    *block_ast,
                    EvalExprContext::make(fn_scope_id)
                        .with_expected_type(Some(return_type))
                        // Why do we care to indicate if the function is generic?
                        // Currently, its because we want to avoid running #static and #meta blocks
                        // until the types and static values are provided
                        .with_is_generic_pass(is_generic),
                )?;
                if let Err(msg) =
                    self.check_types(return_type, self.exprs.get(block).get_type(), fn_scope_id)
                {
                    let return_type_span = self.ast.get_type_expr_span(parsed_function_ret_type);
                    return failf!(
                        return_type_span,
                        "Function {} return type mismatch: {}",
                        self.ident_str(function_name),
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

        let self_ident_id = self.ast.idents.builtins.Self_;
        let mut ability_params: EcoVec<TypedAbilityParam> =
            EcoVec::with_capacity(parsed_ability.params.len() + 1);
        let self_type_id = self.add_type_parameter(
            TypeParameter {
                name: self_ident_id,
                scope_id: ability_scope_id,
                span: parsed_ability.span,
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
                        self.eval_ability_expr(*ability_expr, false, ability_scope_id)
                    }
                })
                .collect();
            let ability_impls = ability_impls?;
            let param_type_id = self.add_type_parameter(
                TypeParameter {
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
                    self.ident_str(ability_param.name)
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
            owner_module: Some(self.module_in_progress.unwrap()),
            parsed_id: ParsedId::Ability(parsed_ability_id),
        };
        let namespace_id = self.namespaces.add(ability_namespace);
        let ns_added =
            self.scopes.get_scope_mut(scope_id).add_namespace(parsed_ability.name, namespace_id);
        if !ns_added {
            return failf!(
                parsed_ability.span,
                "Namespace with name {} already exists",
                self.ident_str(parsed_ability.name)
            );
        }

        let ability_id = self.abilities.next_id();
        let typed_ability = TypedAbility {
            name: parsed_ability.name,
            base_ability_id: ability_id,
            self_type_id,
            parameters: ability_params,
            functions: eco_vec![],
            scope_id: ability_scope_id,
            ast_id: parsed_ability.id,
            namespace_id,
            kind,
        };
        let ability_id = self.abilities.add(typed_ability);
        let added =
            self.scopes.get_scope_mut(scope_id).add_ability(parsed_ability.name, ability_id);
        if !added {
            return failf!(
                parsed_ability.span,
                "Ability with name {} already exists",
                self.ident_str(parsed_ability.name)
            );
        }
        self.types.add_ability_mapping(parsed_ability_id, ability_id);
        self.scopes.set_scope_owner_id(ability_scope_id, ScopeOwnerId::Ability(ability_id));

        let mut typed_functions: EcoVec<TypedAbilityFunctionRef> =
            EcoVec::with_capacity(parsed_ability.functions.len());
        for parsed_function_id in parsed_ability.functions.iter() {
            let Some(function_id) = self.eval_function_declaration(
                *parsed_function_id,
                ability_scope_id,
                Some(FunctionAbilityContextInfo::ability_id_only(ability_id)),
                namespace_id,
            )?
            else {
                // TODO: Possibly, disable conditional compilation of ability functions.
                continue;
            };
            let function_name = self.get_function(function_id).name;
            typed_functions.push(TypedAbilityFunctionRef {
                function_name,
                ability_id,
                function_id,
            });
        }
        self.abilities.get_mut(ability_id).functions = typed_functions;
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
            &self.ast.idents,
        )?;
        found_ability_id.map(Ok).unwrap_or({
            match self.scopes.find_pending_ability(scope_id, ability_name.name) {
                None => {
                    failf!(
                        ability_name.span,
                        "No ability '{}' is in scope",
                        self.ident_str(ability_name.name)
                    )
                }
                Some((pending_ability, ability_scope)) => {
                    debug!(
                        "Recursing into pending ability {} from {}",
                        self.ident_str(ability_name.name),
                        self.ast.get_span_content(ability_name.span)
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
        let parsed_ability_impl = self.ast.get_ability_impl(parsed_id).clone();
        let span = parsed_ability_impl.span;
        let ability_expr = self.ast.p_ability_exprs.get(parsed_ability_impl.ability_expr).clone();
        let parsed_functions = &parsed_ability_impl.functions;

        let impl_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::AbilityImpl, None, None);

        let mut blanket_type_params: SV4<NameAndType> =
            SmallVec::with_capacity(parsed_ability_impl.generic_impl_params.len());
        for generic_impl_param in &parsed_ability_impl.generic_impl_params {
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: generic_impl_param.name,
                    scope_id: impl_scope_id,
                    span: generic_impl_param.span,
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
                    self.ident_str(generic_impl_param.name)
                );
            }

            if !generic_impl_param.constraints.is_empty() {
                let param_constraints_scope_id =
                    self.scopes.add_child_scope(impl_scope_id, ScopeType::AbilityImpl, None, None);
                for parsed_constraint in &generic_impl_param.constraints {
                    let (constraint_ability_sig, constraint_span) = match parsed_constraint {
                        parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            let sig =
                                self.eval_ability_expr(*ability_expr, false, impl_scope_id)?;
                            let constraint_span = self.ast.p_ability_exprs.get(*ability_expr).span;
                            (sig, constraint_span)
                        }
                    };
                    self.add_constrained_ability_impl(
                        type_variable_id,
                        constraint_ability_sig,
                        param_constraints_scope_id,
                        constraint_span,
                    );
                }
            }
            blanket_type_params
                .push(NameAndType { name: generic_impl_param.name, type_id: type_variable_id });
        }

        let impl_self_type = self.eval_type_expr(parsed_ability_impl.self_type, impl_scope_id)?;
        let ability_sig =
            self.eval_ability_expr(parsed_ability_impl.ability_expr, true, impl_scope_id)?;
        let ability_id = ability_sig.specialized_ability_id;

        // Uniqueness of implementation:
        // We allow only one implementation per Ability (+ unique params set)
        // Check for existing implementation
        for existing_impl in self.ability_impls.iter() {
            if existing_impl.ability_id == ability_id
                && existing_impl.self_type_id == impl_self_type
            {
                return failf!(
                    span,
                    "Ability '{}' already implemented for type: {}",
                    self.ident_str(self.abilities.get(ability_id).name).blue(),
                    self.type_id_to_string(impl_self_type).blue()
                );
            }
        }

        let ability = self.abilities.get(ability_id).clone();
        let ability_name = ability.name;
        let ability_self_type = ability.self_type_id;
        let impl_scope_name = format_ident!(
            self,
            "{}_impl_{}",
            self.ident_str(ability_name),
            self.type_id_to_string(impl_self_type)
        );
        self.scopes.get_scope_mut(impl_scope_id).name = Some(impl_scope_name);
        // Bind 'Self' = target_type
        // Discarded because we just made this scope
        let _ = self
            .scopes
            .get_scope_mut(impl_scope_id)
            .add_type(self.ast.idents.builtins.Self_, impl_self_type);

        // We also need to bind any ability parameters that this
        // ability is already specialized on; they aren't in our fresh scope
        for argument in self.named_types.get_slice(ability.kind.arguments()) {
            if !self.scopes.get_scope_mut(impl_scope_id).add_type(argument.name, argument.type_id) {
                return failf!(
                    span,
                    "Type parameter name {} is already used by an ability parameter",
                    self.ident_str(argument.name)
                );
            }
        }

        let mut impl_arguments: SV8<NameAndType> =
            SmallVec::with_capacity(ability.parameters.len());
        for impl_param in ability.parameters.iter().filter(|p| p.is_impl_param) {
            let Some(&matching_arg) = self
                .ast
                .p_type_args
                .get_slice(ability_expr.arguments)
                .iter()
                .find(|arg| arg.name == Some(impl_param.name))
            else {
                return failf!(
                    ability_expr.span,
                    "Missing implementation-side parameter for Ability {}: {}",
                    self.ident_str(ability_name),
                    self.ident_str(impl_param.name)
                );
            };

            let arg_type = self.eval_type_expr(matching_arg.type_expr, impl_scope_id)?;

            self.check_type_constraints(
                impl_param.name,
                impl_param.type_variable_id,
                arg_type,
                &[],
                impl_scope_id,
                matching_arg.span,
            )?;

            debug!(
                "Binding impl param {} to {}",
                self.ident_str(impl_param.name),
                self.type_id_to_string(arg_type)
            );
            let added =
                self.scopes.get_scope_mut(impl_scope_id).add_type(impl_param.name, arg_type);
            if !added {
                panic!("shit")
            }
            impl_arguments.push(NameAndType { name: impl_param.name, type_id: arg_type })
        }

        let base_ability_id = ability.base_ability_id;
        let kind = if parsed_ability_impl.generic_impl_params.is_empty() {
            AbilityImplKind::Concrete
        } else {
            AbilityImplKind::Blanket { base_ability: base_ability_id, parsed_id }
        };

        let mut typed_functions = EcoVec::with_capacity(ability.functions.len());
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
                    self.ident_str(ability_function_ref.function_name).blue(),
                    self.ident_str(ability_name).blue()
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
                        self.ident_str(parsed_fn_name)
                    );
                };
            }

            let impl_function_id = self
                .eval_function_declaration(
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
                )?
                .expect("an ability impl cannot be conditionally compiled");

            let specialized_fun = self.get_function(impl_function_id);
            let specialized_fn_type = specialized_fun.type_id;

            let spec_fn_scope = specialized_fun.scope;

            let generic_type = self.get_function(ability_function_ref.function_id).type_id;

            // We check that the signature of the provided impl function matches
            // the signature of the generic function with target_type substituted for Self
            let substituted_root_type = self.substitute_in_type(
                generic_type,
                &[TypeSubstitutionPair { from: ability_self_type, to: impl_self_type }],
            );

            if let Err(msg) =
                self.check_types(substituted_root_type, specialized_fn_type, spec_fn_scope)
            {
                eprintln!("{}", self.scope_id_to_string(spec_fn_scope));
                return failf!(
                    impl_function_span,
                    "Invalid implementation of {} in ability {}: {msg}",
                    self.ast.idents.get_name(ability_function_ref.function_name),
                    self.ast.idents.get_name(ability_name)
                );
            }
            typed_functions.push(AbilityImplFunction::FunctionId(impl_function_id));
        }

        let blanked_type_params_handle =
            self.named_types.add_slice_from_copy_slice(&blanket_type_params);
        let impl_arguments_handle = self.named_types.add_slice_from_copy_slice(&impl_arguments);
        let typed_impl_id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: blanked_type_params_handle,
            self_type_id: impl_self_type,
            ability_id,
            base_ability_id,
            impl_arguments: impl_arguments_handle,
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
        let Some(&ability_impl_id) = self.ability_impl_ast_mappings.get(&parsed_ability_impl_id)
        else {
            // Missing mapping means, likely, we failed to compile the signature
            // Just do nothing. TODO: flag when defns have failed compilation so we don't
            // mask real bugs
            return Ok(());
        };
        let ability_impl = self.ability_impls.get(ability_impl_id);

        for impl_fn in ability_impl.functions.clone().iter() {
            let AbilityImplFunction::FunctionId(impl_fn) = *impl_fn else {
                self.ice("Expected impl function id, not abstract, in eval_ability_impl", None);
            };
            if let Err(e) = self.eval_function_body(impl_fn) {
                self.ability_impls.get_mut(ability_impl_id).compile_errors.push(e.clone());
                self.report_error(e);
            }
        }

        Ok(())
    }

    fn eval_definition_body_phase(&mut self, def: ParsedId, scope_id: ScopeId) {
        match def {
            ParsedId::Use(parsed_use_id) => {
                if let Err(e) = self.eval_use_definition(scope_id, parsed_use_id) {
                    self.report_error(e)
                }
            }
            ParsedId::Namespace(namespace) => {
                if let Err(e) = self.eval_namespace_body_phase(namespace) {
                    self.report_error(e);
                };
            }
            ParsedId::Global(global_id) => {
                if let Err(e) = self.eval_global_body(global_id, None) {
                    self.report_error(e)
                };
            }
            ParsedId::Function(parsed_function_id) => {
                if let Some(function_declaration_id) =
                    self.function_ast_mappings.get(&parsed_function_id)
                {
                    if let Err(e) = self.eval_function_body(*function_declaration_id) {
                        self.report_error(e);
                    };
                }
            }
            ParsedId::TypeDefn(_type_defn_id) => {
                // Done in prior phase
            }
            ParsedId::Ability(_ability) => {
                // Nothing to do in this phase for an ability
            }
            ParsedId::AbilityImpl(ability_impl) => {
                if let Err(e) = self.eval_ability_impl(ability_impl, scope_id) {
                    self.report_error(e);
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
            debug!(
                "Handling unfulfilled use {}",
                self.namespaced_identifier_to_string(&parsed_use.target)
            );
            if let Some(symbol) = self.find_useable_symbol(scope_id, &parsed_use.target)? {
                self.scopes.add_use_binding(
                    scope_id,
                    symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                self.use_statuses.insert(parsed_use_id, UseStatus::Resolved(symbol));
                debug!("Inserting resolved use of {:?}", symbol);
            } else {
                self.use_statuses.insert(parsed_use_id, UseStatus::Unresolved);
                debug!("Inserting unresolved use");
            }
        }
        Ok(())
    }

    fn find_useable_symbol(
        &self,
        scope_id: ScopeId,
        name: &NamespacedIdentifier,
    ) -> TyperResult<Option<UseableSymbol>> {
        let scope_id_to_search = self.scopes.traverse_namespace_chain(
            scope_id,
            &name.namespaces,
            &self.namespaces,
            &self.ast.idents,
            name.span,
        )?;
        let scope_to_search = self.scopes.get_scope(scope_id_to_search);

        debug!(
            "Searching scope for useable symbol: {}, Functions:\n{:?}",
            self.scopes.make_scope_name(scope_to_search, &self.ast.idents),
            scope_to_search.functions.iter().collect::<Vec<_>>()
        );

        // TODO(MODULES): Validate modules cannot use something from a module they don't depend on
        // even if its in the program

        if let Some(function_id) = scope_to_search.find_function(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Function(function_id),
            }))
        } else if let Some(type_id) = scope_to_search.find_type(name.name) {
            let companion_namespace = self.types.get_companion_namespace(type_id);
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type { type_id, companion_namespace },
            }))
        } else if let Some(variable_id) =
            scope_to_search.find_variable(name.name).and_then(|vis| vis.variable_id())
        {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Constant(variable_id),
            }))
        } else if let Some(ability_id) = scope_to_search.find_ability(name.name) {
            let namespace_id = self.abilities.get(ability_id).namespace_id;
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Ability(ability_id, namespace_id),
            }))
        } else if let Some(ns_id) = scope_to_search.find_namespace(name.name) {
            Ok(Some(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Namespace(ns_id),
            }))
        } else {
            Ok(None)
        }
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
            self.ast.namespaces.get(parsed_namespace_id).definitions.clone().iter()
        {
            if let ParsedId::Use(parsed_use_id) = parsed_definition_id {
                if let Err(e) = self.eval_use_definition(namespace_scope_id, parsed_use_id) {
                    self.report_error(e);
                }
            }
            if let ParsedId::TypeDefn(type_defn_id) = parsed_definition_id {
                let parsed_type_defn = self.ast.get_type_defn(type_defn_id);
                if parsed_type_defn.flags.is_alias() {
                    // Do nothing for aliases in the decl phase
                } else {
                    // Find companion namespace if exists and update type_defn_info
                    let companion_namespace_id = self
                        .scopes
                        .get_scope(namespace_scope_id)
                        .find_namespace(parsed_type_defn.name);
                    let defn_info = TypeDefnInfo {
                        name: parsed_type_defn.name,
                        scope: namespace_scope_id,
                        companion_namespace: companion_namespace_id,
                        ast_id: ParsedId::TypeDefn(type_defn_id),
                    };
                    let type_id = self.types.add_unresolved_type_defn(type_defn_id, defn_info);

                    if let Some(companion_namespace_id) = companion_namespace_id {
                        self.namespaces.get_mut(companion_namespace_id).companion_type_id =
                            Some(type_id);
                    }
                    let name = parsed_type_defn.name;
                    let added =
                        self.scopes.get_scope_mut(namespace_scope_id).add_type(name, type_id);
                    if !added {
                        let span = parsed_type_defn.span;
                        self.report_error(errf!(span, "Type {} exists", self.ident_str(name)));
                    }

                    // Detect builtin types and store their IDs for fast lookups
                    if namespace_scope_id == self.scopes.types_scope_id {
                        if name == self.ast.idents.builtins.TypeSchema {
                            self.types.builtins.types_type_schema = Some(type_id);
                        } else if name == self.ast.idents.builtins.IntKind {
                            self.types.builtins.types_int_kind = Some(type_id);
                        } else if name == self.ast.idents.builtins.IntValue {
                            self.types.builtins.types_int_value = Some(type_id)
                        }
                    }
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
                    self.report_error(errf!(span, "Ability {} exists", self.ident_str(name)));
                }
            }
            if let ParsedId::Namespace(namespace_id) = parsed_definition_id {
                if let Err(e) = self.eval_namespace_type_decl_phase(namespace_id) {
                    self.report_error(e);
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
        let parsed_namespace = self.ast.namespaces.get(parsed_namespace_id);

        for parsed_definition_id in parsed_namespace.definitions.clone().iter() {
            if let ParsedId::TypeDefn(type_defn_id) = parsed_definition_id {
                if let Err(e) = self.eval_type_defn(*type_defn_id, namespace_scope_id) {
                    self.type_defn_stack.clear();
                    self.report_error(e);
                };
            }
            if let ParsedId::Namespace(namespace_id) = parsed_definition_id {
                if let Err(e) = self.eval_namespace_type_eval_phase(*namespace_id) {
                    self.report_error(e);
                }
            }
        }
        Ok(())
    }

    fn eval_namespace_declaration_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<()> {
        let parsed_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace = self.namespaces.get(namespace_id);
        let namespace_scope_id = namespace.scope_id;
        for defn in &parsed_namespace.definitions.clone() {
            self.eval_definition_declaration_phase(*defn, namespace_scope_id, namespace_id)?;
        }
        Ok(())
    }

    fn eval_namespace_body_phase(
        &mut self,
        ast_namespace_id: ParsedNamespaceId,
    ) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(ast_namespace_id).clone();
        let namespace_id = *self.namespace_ast_mappings.get(&ast_namespace.id).unwrap();
        let ns_scope_id = self.namespaces.get(namespace_id).scope_id;
        for defn in &ast_namespace.definitions {
            self.eval_definition_body_phase(*defn, ns_scope_id);
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
                if let Err(e) = self.eval_namespace_declaration_phase(namespace_id) {
                    self.report_error(e);
                }
                Ok(())
            }
            ParsedId::Global(constant_id) => {
                let _variable_id: VariableId =
                    self.eval_global_declaration_phase(constant_id, scope_id)?;
                Ok(())
            }
            ParsedId::Function(parsed_function_id) => {
                if let Err(e) =
                    self.eval_function_declaration(parsed_function_id, scope_id, None, namespace_id)
                {
                    self.report_error(e);
                }
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
        parent_scope_id: ScopeId,
    ) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let name = ast_namespace.name;

        let ns_scope_id =
            self.scopes.add_child_scope(parent_scope_id, ScopeType::Namespace, None, Some(name));
        let parent_ns_id = self
            .scopes
            .get_scope_owner(parent_scope_id)
            .and_then(|owner| owner.as_namespace())
            .expect("namespace must be defined directly inside another namespace");

        let is_core =
            parent_scope_id == Scopes::ROOT_SCOPE_ID && name == self.ast.idents.builtins.core;
        if is_core {
            self.scopes.core_scope_id = ns_scope_id;
        }
        let is_k1 =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.builtins.k1;
        if is_k1 {
            self.scopes.k1_scope_id = ns_scope_id;
        }
        let is_types =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.builtins.types;
        if is_types {
            self.scopes.types_scope_id = ns_scope_id;
        }

        let namespace = Namespace {
            name,
            scope_id: ns_scope_id,
            namespace_type: NamespaceType::User,
            companion_type_id: None,
            parent_id: Some(parent_ns_id),
            owner_module: Some(self.module_in_progress.unwrap()),
            parsed_id: ParsedId::Namespace(parsed_namespace_id),
        };
        let namespace_id = self.namespaces.add(namespace);
        self.scopes.set_scope_owner_id(ns_scope_id, ScopeOwnerId::Namespace(namespace_id));

        let parent_scope = self.scopes.get_scope_mut(parent_scope_id);
        if !parent_scope.add_namespace(name, namespace_id) {
            return failf!(
                ast_namespace.span,
                "Namespace name {} is taken",
                self.ident_str(name).blue()
            );
        }

        self.namespace_ast_mappings.insert(parsed_namespace_id, namespace_id);
        Ok(namespace_id)
    }

    fn eval_namespace_namespace_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: ScopeId,
    ) -> TyperResult<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id).clone();

        let namespace_id = if let Some(existing) =
            self.scopes.find_namespace(parent_scope, ast_namespace.name)
        {
            if self.module_in_progress.unwrap()
                != self.namespaces.get(existing).owner_module.unwrap()
            {
                return failf!(
                    ast_namespace.span,
                    "Cannot extend definition of namespace from another module"
                );
            }
            // Namespace extension
            // Map this separate namespace AST node to the same semantic namespace
            self.namespace_ast_mappings.insert(parsed_namespace_id, existing);
            debug!("Inserting re-definition node for ns {}", self.ident_str(ast_namespace.name));
            existing
        } else {
            self.create_namespace(parsed_namespace_id, parent_scope)?
        };

        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;

        for defn in &ast_namespace.definitions {
            if let ParsedId::Namespace(namespace_id) = defn {
                let _namespace_id =
                    self.eval_namespace_namespace_phase(*namespace_id, namespace_scope_id)?;
            }
        }
        Ok(namespace_id)
    }

    pub fn run(
        &mut self,
        module_name: Ident,
        module_namespace_id: ParsedNamespaceId,
        kind: ModuleKind,
    ) -> anyhow::Result<ModuleId> {
        let module_id = self.modules.next_id();
        self.module_in_progress = Some(module_id);
        //let module_namespace = self.ast.get_namespace(module_namespace_id);

        let mut err_writer = stderr();

        // Namespace phase
        eprintln!(">> Phase 1 declare namespaces");
        let ns_phase_res =
            self.eval_namespace_namespace_phase(module_namespace_id, Scopes::ROOT_SCOPE_ID);

        if let Err(e) = &ns_phase_res {
            self.write_error(&mut err_writer, e)?;
            self.errors.push(e.clone());
        }
        if !self.errors.is_empty() {
            bail!(
                "{} failed namespace declaration phase with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }

        let typed_namespace_id = ns_phase_res.unwrap();
        let namespace_scope_id = self.namespaces.get(typed_namespace_id).scope_id;
        let real_module_id = self.modules.add(Module {
            id: module_id,
            name: module_name,
            dependencies: vec![],
            kind,
            namespace_id: typed_namespace_id,
            namespace_scope_id,
        });
        debug_assert_eq!(module_id, real_module_id);
        let is_core = module_id == MODULE_ID_CORE;
        if !is_core {
            self.add_default_uses_to_scope(namespace_scope_id, SpanId::NONE)?;
        }

        // Pending Type declaration phase
        eprintln!(">> Phase 2 declare types");
        let type_defn_result = self.eval_namespace_type_decl_phase(module_namespace_id);
        if let Err(e) = type_defn_result {
            self.write_error(&mut err_writer, &e)?;
            self.errors.push(e);
        }
        if !self.errors.is_empty() {
            bail!(
                "{} failed type definition phase with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }
        // Type evaluation phase
        eprintln!(">> Phase 3 evaluate types");
        let type_eval_result = self.eval_namespace_type_eval_phase(module_namespace_id);
        if let Err(e) = type_eval_result {
            self.write_error(&mut err_writer, &e)?;
            self.errors.push(e);
        }
        if !self.errors.is_empty() {
            bail!(
                "{} failed type evaluation phase with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }

        debug_assert_eq!(self.types.types.len(), self.types.layouts.len());
        debug_assert_eq!(self.types.types.len(), self.types.type_variable_counts.len());

        // eprintln!("{}", self.dump_types_to_string());

        for type_id in self.types.iter_ids().collect_vec() {
            if let Type::Unresolved(ast_id) = self.types.get(type_id) {
                let span = self.ast.get_span_for_id(ParsedId::TypeDefn(*ast_id));
                self.ice_with_span("Unresolved type definition", span)
            }
        }

        if module_id == MODULE_ID_CORE {
            self.assert_builtin_types_correct();
        }

        // Everything else declaration phase
        eprintln!(">> Phase 4 declare rest of definitions (functions, globals, abilities)");
        for &parsed_definition_id in
            self.ast.namespaces.get(module_namespace_id).definitions.clone().iter()
        {
            let result = self.eval_definition_declaration_phase(
                parsed_definition_id,
                namespace_scope_id,
                typed_namespace_id,
            );
            if let Err(e) = result {
                self.write_error(&mut err_writer, &e)?;
                self.errors.push(e);
            }
        }
        if !self.errors.is_empty() {
            eprintln!(
                "{} failed declaration phase with {} errors, but I will soldier on.",
                self.program_name(),
                self.errors.len()
            )
        }

        debug_assert!(self.abilities.get(EQUALS_ABILITY_ID).name == get_ident!(self, "Equals"));
        debug_assert!(self.abilities.get(BITWISE_ABILITY_ID).name == get_ident!(self, "Bitwise"));
        debug_assert!(
            self.abilities.get(COMPARABLE_ABILITY_ID).name == get_ident!(self, "Comparable")
        );

        // Everything else evaluation phase
        eprintln!(">> Phase 5 bodies (functions, globals, abilities)");
        for &parsed_definition_id in
            self.ast.namespaces.get(module_namespace_id).definitions.clone().iter()
        {
            self.eval_definition_body_phase(parsed_definition_id, namespace_scope_id);
        }
        let unresolved_uses: Vec<_> =
            self.use_statuses.iter().filter(|use_status| !use_status.1.is_resolved()).collect();
        if !unresolved_uses.is_empty() {
            for (parsed_use_id, _status) in &unresolved_uses {
                let parsed_use = self.ast.uses.get_use(**parsed_use_id);
                let error = errf!(
                    parsed_use.span,
                    "Unresolved use of {}",
                    self.ident_str(parsed_use.target.name)
                );
                self.write_error(&mut err_writer, &error)?;
                self.errors.push(error)
            }
        }
        if !self.errors.is_empty() {
            bail!(
                "Module {} failed typechecking with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }

        eprintln!(">> Phase 6 specialize function bodies");
        self.specialize_pending_function_bodies(&mut err_writer)?;

        if !self.errors.is_empty() {
            bail!("{} failed specialize with {} errors", self.program_name(), self.errors.len())
        }

        Ok(module_id)
    }

    fn assert_builtin_types_correct(&self) {
        //eprintln!("{}", self);

        //
        // This just ensures our BUFFER_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let buffer_generic = self.types.get(BUFFER_TYPE_ID).expect_generic();
            let buffer_struct = self.types.get(buffer_generic.inner).expect_struct();
            debug_assert!(buffer_struct.fields.len() == 2);
            debug_assert!(
                buffer_struct.fields.iter().map(|f| self.ident_str(f.name)).collect::<Vec<_>>()
                    == vec!["len", BUFFER_DATA_FIELD_NAME]
            );
        }

        // This just ensures our LIST_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let list_generic = self.types.get(LIST_TYPE_ID).expect_generic();
            let info = self.types.get_defn_info(LIST_TYPE_ID).unwrap();
            let list_struct = self.types.get(list_generic.inner).expect_struct();
            debug_assert!(info.name == get_ident!(self, "List"));
            debug_assert!(
                list_struct.fields.iter().map(|f| self.ident_str(f.name)).collect::<Vec<_>>()
                    == vec!["len", "buffer"]
            );
        }

        // This just ensures our STRING_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let string_struct = self.types.get(STRING_TYPE_ID).expect_struct();
            let info = self.types.get_defn_info(STRING_TYPE_ID).unwrap();
            debug_assert!(info.name == get_ident!(self, "string"));
            debug_assert!(string_struct.fields.len() == 1);
        }

        // This just ensures our OPTIONAL_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let optional_generic = self.types.get(OPTIONAL_TYPE_ID).expect_generic();
            let info = self.types.get_defn_info(OPTIONAL_TYPE_ID).unwrap();
            let inner = self.types.get(optional_generic.inner);
            debug_assert!(info.name == get_ident!(self, "Opt"));
            debug_assert!(inner.as_enum().unwrap().variants.len() == 2);
        }
        //
        // This just ensures our ORDERING_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let ordering_enum = self.types.get(ORDERING_TYPE_ID).expect_enum();
            let info = self.types.get_defn_info(ORDERING_TYPE_ID).unwrap();
            debug_assert!(ordering_enum.variants.len() == 3);
            debug_assert!(info.name == get_ident!(self, "Ordering"));
        }
    }

    fn specialize_pending_function_bodies(
        &mut self,
        err_writer: &mut impl std::io::Write,
    ) -> anyhow::Result<()> {
        let mut function_ids: Vec<FunctionId> =
            Vec::with_capacity(self.functions_pending_body_specialization.len());
        while !self.functions_pending_body_specialization.is_empty() {
            function_ids.extend(&self.functions_pending_body_specialization);
            self.functions_pending_body_specialization.clear();
            for function_id in &function_ids {
                let result = self.specialize_function_body(*function_id);
                if let Err(e) = result {
                    self.write_error(err_writer, &e)?;
                    self.errors.push(e);
                }
            }
            function_ids.clear()
        }
        Ok(())
    }

    pub fn get_span_for_type_id(&self, type_id: TypeId) -> Option<SpanId> {
        match self.types.get(type_id) {
            Type::TypeParameter(tv) => Some(tv.span),
            _ => self
                .types
                .get_ast_node(type_id)
                .map(|parsed_id| self.ast.get_span_for_id(parsed_id)),
        }
    }

    fn generate_constructors_for_type(
        &self,
        type_id: TypeId,
        _span_id: SpanId,
    ) -> Vec<PatternCtor> {
        if type_id == STRING_TYPE_ID {
            return vec![PatternCtor::String];
        }
        match self.types.get(type_id) {
            Type::Unit => vec![PatternCtor::Unit],
            Type::Char => vec![PatternCtor::Char],
            Type::TypeParameter(_) => vec![PatternCtor::TypeVariable],
            Type::Integer(_) => vec![PatternCtor::Int],
            Type::Float(_) => vec![PatternCtor::Float],
            Type::Bool => {
                vec![PatternCtor::BoolFalse, PatternCtor::BoolTrue]
            }
            Type::Pointer => vec![PatternCtor::Pointer], // Just an opaque atom
            Type::Reference(refer) => {
                match self.types.get(refer.inner_type).as_function() {
                    Some(_) => vec![PatternCtor::FunctionReference], // Function Reference, opaque atom
                    None => {
                        // Follow the pointer
                        let inner = self.generate_constructors_for_type(refer.inner_type, _span_id);
                        inner
                            .into_iter()
                            .map(|pointee_pattern| {
                                PatternCtor::Reference(Box::new(pointee_pattern))
                            })
                            .collect()
                    }
                }
            }
            Type::Enum(enum_type) => enum_type
                .variants
                .iter()
                .flat_map(|v| match v.payload.as_ref() {
                    None => {
                        vec![PatternCtor::Enum { variant_name: v.name, inner: None }]
                    }
                    Some(payload) => self
                        .generate_constructors_for_type(*payload, _span_id)
                        .into_iter()
                        .map(|inner| PatternCtor::Enum {
                            variant_name: v.name,
                            inner: Some(Box::new(inner)),
                        })
                        .collect(),
                })
                .collect(),
            Type::Struct(struc) => {
                debug_assert!(type_id != STRING_TYPE_ID);
                let mut all_field_ctors: Vec<Vec<(Ident, PatternCtor)>> = vec![];
                // TODO(perf): Tons of allocations here and probably worth a reusable buffer
                //             Its all those expensive 'drop' calls that don't get us anything!
                for field in struc.fields.iter() {
                    let field_ctors_iter = self
                        .generate_constructors_for_type(field.type_id, _span_id)
                        .into_iter()
                        .map(|pat| (field.name, pat))
                        .collect::<Vec<_>>();
                    all_field_ctors.push(field_ctors_iter)
                }
                // Generate cross product of all field combinations
                // Example:
                // { x: bool, y: bool }
                // all_field_ctors: [[(x, false), (x, true)], [(y, false), (y, true)]]
                // result: [{}]
                // field_ctors: x
                // for ctor in result: {}
                // field_ctor: (x, false)
                // field_ctor: (x, true)
                // { x: false }, { x: true }
                // field_ctors: y
                // for ctor in result: [{ x: false }, { x: true }]
                //   field_ctor: (y, false)
                //   field_ctor: (y, true)
                // augment { x: false } -> { x: false, y: false}, { x: false, y: true }
                // augment { x: true } -> { x: true, y: false}, { x: true, y: true }
                //
                // For each individual field's constructors, we iterate over all previously accumulated results
                // Pushing this field's constructors onto each previous results' field vec

                // FIXME: Re-write this to be more sensible; assigning to result from new_result
                // every time just feels bad, its a triply nested 'for', idk, does not feel optimal
                // TODO(perf): Also very allocatey
                let mut result =
                    vec![PatternCtor::Struct { fields: Vec::with_capacity(struc.fields.len()) }];
                for field_ctors in all_field_ctors.into_iter() {
                    let mut new_result = Vec::new();
                    for full_struct_ctor in result.iter_mut() {
                        for (field_name, field_ctor) in &field_ctors {
                            if let PatternCtor::Struct { mut fields } = full_struct_ctor.clone() {
                                fields.push((*field_name, field_ctor.clone()));
                                new_result.push(PatternCtor::Struct { fields });
                            }
                        }
                    }
                    result = new_result;
                }
                result
            }
            Type::Function(_f) => {
                debug!("function is probably unmatchable");
                vec![]
            }
            _ => {
                eprintln!(
                    "unhandled type in generate_constructors_for_type {}",
                    self.type_id_to_string(type_id)
                );
                vec![]
            }
        }
    }

    fn pattern_matches(pattern: &TypedPattern, ctor: &PatternCtor) -> bool {
        match (pattern, ctor) {
            (TypedPattern::Wildcard(_), _) => true,
            (TypedPattern::Variable(_), _) => true,
            (TypedPattern::LiteralUnit(_), PatternCtor::Unit) => true,
            (TypedPattern::LiteralBool(true, _), PatternCtor::BoolTrue) => true,
            (TypedPattern::LiteralBool(false, _), PatternCtor::BoolFalse) => true,
            (TypedPattern::Enum(enum_pat), PatternCtor::Enum { variant_name, inner }) => {
                if *variant_name == enum_pat.variant_tag_name {
                    match (enum_pat.payload.as_ref(), inner) {
                        (Some(payload), Some(inner)) => {
                            TypedProgram::pattern_matches(payload, inner)
                        }
                        (None, None) => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            (TypedPattern::Struct(struc), PatternCtor::Struct { fields }) => {
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
                    if !TypedProgram::pattern_matches(
                        &field_pattern.pattern,
                        matching_field_pattern,
                    ) {
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

    fn zip_named_types_to_subst_pairs<const N: usize>(
        &self,
        from: SliceHandle<NameAndTypeId>,
        to: SliceHandle<NameAndTypeId>,
    ) -> SmallVec<[TypeSubstitutionPair; N]>
    where
        [TypeSubstitutionPair; N]: smallvec::Array<Item = TypeSubstitutionPair>,
    {
        let mut pairs = smallvec![];
        for (from, to) in
            self.named_types.get_slice(from).iter().zip(self.named_types.get_slice(to))
        {
            pairs.push(spair! { from.type_id => to.type_id });
        }
        pairs
    }

    /******************************
     ** Synthesis of Typed nodes **
     *****************************/

    fn add_default_uses_to_scope(&mut self, scope: ScopeId, span: SpanId) -> TyperResult<()> {
        let default_uses = [
            qident!(self, span, ["core"], "u8"),
            qident!(self, span, ["core"], "u16"),
            qident!(self, span, ["core"], "u32"),
            qident!(self, span, ["core"], "u64"),
            qident!(self, span, ["core"], "i8"),
            qident!(self, span, ["core"], "i16"),
            qident!(self, span, ["core"], "i32"),
            qident!(self, span, ["core"], "i64"),
            qident!(self, span, ["core"], "uword"),
            qident!(self, span, ["core"], "iword"),
            qident!(self, span, ["core"], "unit"),
            qident!(self, span, ["core"], "char"),
            qident!(self, span, ["core"], "bool"),
            qident!(self, span, ["core"], "never"),
            qident!(self, span, ["core"], "Pointer"),
            qident!(self, span, ["core"], "f32"),
            qident!(self, span, ["core"], "f64"),
            qident!(self, span, ["core"], "Buffer"),
            qident!(self, span, ["core"], "List"),
            qident!(self, span, ["core"], "string"),
            qident!(self, span, ["core"], "Opt"),
            qident!(self, span, ["core"], "some"),
            qident!(self, span, ["core"], "none"),
            qident!(self, span, ["core"], "Ordering"),
            qident!(self, span, ["core"], "Result"),
            qident!(self, span, ["core"], "int"),
            qident!(self, span, ["core"], "uint"),
            qident!(self, span, ["core"], "byte"),
            qident!(self, span, ["core"], "Equals"),
            qident!(self, span, ["core"], "Writer"),
            qident!(self, span, ["core"], "Print"),
            qident!(self, span, ["core"], "Show"),
            qident!(self, span, ["core"], "Bitwise"),
            qident!(self, span, ["core"], "Comparable"),
            qident!(self, span, ["core"], "Unwrap"),
            qident!(self, span, ["core"], "Try"),
            qident!(self, span, ["core"], "Iterator"),
            qident!(self, span, ["core"], "Iterable"),
            qident!(self, span, ["core"], "println"),
            qident!(self, span, ["core"], "print"),
            qident!(self, span, ["core"], "printIt"),
            qident!(self, span, ["core"], "assert"),
            qident!(self, span, ["core"], "assertEquals"),
            qident!(self, span, ["core"], "assertMsg"),
            qident!(self, span, ["core"], "crash"),
            qident!(self, span, ["_root"], "core"),
            qident!(self, span, ["core"], "mem"),
            qident!(self, span, ["core"], "types"),
        ];
        for du in default_uses.into_iter() {
            let use_id = self.ast.uses.add_use(parse::ParsedUse { target: du, alias: None, span });
            self.eval_use_definition(scope, use_id)?;
        }
        Ok(())
    }

    fn get_type_schema(&mut self, type_id: TypeId, span: SpanId) -> StaticValueId {
        if let Some(static_value_id) = self.type_schemas.get(&type_id) {
            return *static_value_id;
        }

        let type_schema =
            self.types.get(self.types.builtins.types_type_schema.unwrap()).expect_enum().clone();
        let int_kind_enum =
            self.types.get(self.types.builtins.types_int_kind.unwrap()).expect_enum();
        let get_schema_variant = |ident| type_schema.variant_by_name(ident).unwrap();
        let word_enum = self
            .types
            .get(get_schema_variant(get_ident!(self, "Word")).payload.unwrap())
            .expect_enum();
        let make_variant = |name: Ident, payload: Option<StaticValueId>| {
            let v = get_schema_variant(name);
            StaticEnum {
                variant_type_id: v.my_type_id,
                variant_index: v.index,
                typed_as_enum: true,
                payload,
            }
        };

        // .get will follow statics and recursives
        let typ = self.types.get(type_id);
        let static_enum = match typ {
            Type::Unit => make_variant(get_ident!(self, "Unit"), None),
            Type::Char => make_variant(get_ident!(self, "Char"), None),
            Type::Bool => make_variant(get_ident!(self, "Bool"), None),
            Type::Pointer => make_variant(get_ident!(self, "Pointer"), None),
            Type::Integer(integer_type) => {
                let int_kind_enum_value =
                    TypedProgram::make_int_kind(int_kind_enum, word_enum, *integer_type);

                let payload_value_id =
                    self.static_values.add(StaticValue::Enum(int_kind_enum_value));
                let enum_value = make_variant(get_ident!(self, "Int"), Some(payload_value_id));
                enum_value
            }
            Type::Float(_float_type) => todo!("float schema"),
            Type::Struct(struct_type) => {
                let struct_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Struct")).payload.unwrap();
                // { fields: Buffer[{}] }
                let struct_schema_payload_struct =
                    self.types.get(struct_schema_payload_type_id).expect_struct();
                // { fields: Buffer[{ ... }] }
                let struct_schema_fields_buffer_type_id =
                    struct_schema_payload_struct.fields[0].type_id;
                let struct_schema_field_item_struct_type_id = self
                    .types
                    .get(struct_schema_fields_buffer_type_id)
                    .as_buffer_instance()
                    .unwrap()
                    .type_args[0];
                // { name: string), typeId: u64, offset: uword }
                // let struct_schema_field_item_struct =
                //     self.types.get(struct_schema_field_item_struct_type_id).expect_struct();
                let struct_layout = self.types.get_struct_layout(type_id);
                let mut field_values: EcoVec<StaticValueId> =
                    EcoVec::with_capacity(struct_type.fields.len());
                for (index, f) in struct_type.fields.clone().iter().enumerate() {
                    let name_string_id = self.ast.strings.intern(self.ast.idents.get_name(f.name));
                    let name_string_value_id =
                        self.static_values.add(StaticValue::String(name_string_id));
                    let type_id_u32 = f.type_id.as_u32();

                    // We need to ensure that any and all typeIds that we share with the user
                    // are available at runtime, by calling these functions at least once.
                    self.register_type_metainfo(f.type_id, span);

                    let type_id_value_id = self
                        .static_values
                        .add(StaticValue::Int(TypedIntValue::U64(type_id_u32 as u64)));
                    let offset_u32 = struct_layout.field_offsets[index];
                    let offset_value_id = self
                        .static_values
                        .add(StaticValue::Int(TypedIntValue::UWord64(offset_u32 as u64)));
                    let field_struct = StaticValue::Struct(StaticStruct {
                        type_id: struct_schema_field_item_struct_type_id,
                        fields: eco_vec![
                            // name: string
                            name_string_value_id,
                            // typeId: u64
                            type_id_value_id,
                            // offset: uword
                            offset_value_id
                        ],
                    });
                    field_values.push(self.static_values.add(field_struct));
                }
                let buffer = self.static_values.add(StaticValue::Buffer(StaticBuffer {
                    elements: field_values,
                    type_id: struct_schema_fields_buffer_type_id,
                }));
                let payload = self.static_values.add(StaticValue::Struct(StaticStruct {
                    fields: eco_vec![buffer],
                    type_id: struct_schema_payload_type_id,
                }));
                make_variant(get_ident!(self, "Struct"), Some(payload))
            }
            Type::Reference(reference_type) => {
                let reference_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Reference")).payload.unwrap();
                // { innerTypeId: u64 }
                let inner_type_id_value_id = self.static_values.add(StaticValue::Int(
                    TypedIntValue::U64(reference_type.inner_type.as_u32() as u64),
                ));
                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(reference_type.inner_type, span);

                let payload_struct_id = self.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id: reference_schema_payload_type_id,
                    fields: eco_vec![inner_type_id_value_id],
                }));
                make_variant(get_ident!(self, "Reference"), Some(payload_struct_id))
            }
            Type::Array(array_type) => {
                let array_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Array")).payload.unwrap();
                // { elementTypeId: u64, size: uword }
                let element_type_id_value_id = self.static_values.add(StaticValue::Int(
                    TypedIntValue::U64(array_type.element_type.as_u32() as u64),
                ));
                let size_value_id = self
                    .static_values
                    .add(StaticValue::Int(TypedIntValue::UWord64(array_type.size)));
                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(array_type.element_type, span);

                let payload_struct_id = self.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id: array_schema_payload_type_id,
                    fields: eco_vec![element_type_id_value_id, size_value_id],
                }));
                make_variant(get_ident!(self, "Array"), Some(payload_struct_id))
            }
            Type::Enum(typed_enum) => {
                let either_payload_type_id =
                    get_schema_variant(get_ident!(self, "Either")).payload.unwrap();
                let either_payload_struct = self.types.get(either_payload_type_id).expect_struct();
                let variants_buffer_type_id = either_payload_struct.fields[1].type_id;
                let variant_struct_type_id =
                    self.types.get(variants_buffer_type_id).as_buffer_instance().unwrap().type_args
                        [0];
                let tag_type = self.types.get(typed_enum.tag_type).expect_integer();
                let tag_type_value_id = self.static_values.add(StaticValue::Enum(
                    TypedProgram::make_int_kind(int_kind_enum, word_enum, *tag_type),
                ));
                let mut variant_values = EcoVec::with_capacity(typed_enum.variants.len());
                for variant in typed_enum.variants.clone().iter() {
                    let name_string_id =
                        self.ast.strings.intern(self.ast.idents.get_name(variant.name));
                    let name_value_id = self.static_values.add(StaticValue::String(name_string_id));

                    let int_value_enum =
                        self.types.get(self.types.builtins.types_int_value.unwrap()).expect_enum();
                    let tag_value_enum_value = TypedProgram::make_int_value(
                        &mut self.static_values,
                        int_value_enum,
                        variant.tag_value,
                    );
                    let tag_value_id =
                        self.static_values.add(StaticValue::Enum(tag_value_enum_value));

                    let payload_info_opt_type_id =
                        self.types.get(variant_struct_type_id).expect_struct().fields[2].type_id;
                    let payload_info_struct_id = self
                        .types
                        .get(payload_info_opt_type_id)
                        .as_opt_instance()
                        .unwrap()
                        .inner_type;
                    let payload_info_value_id = match variant.payload {
                        None => synth_static_option(
                            &self.types,
                            &mut self.static_values,
                            payload_info_opt_type_id,
                            None,
                        ),
                        Some(payload_type_id) => {
                            let type_id_value_id = self.static_values.add(StaticValue::Int(
                                TypedIntValue::U64(payload_type_id.as_u32() as u64),
                            ));
                            // We need to ensure that any and all typeIds that we share with the user
                            // are available at runtime, by calling these functions at least once.
                            self.register_type_metainfo(payload_type_id, span);

                            let payload_offset_value_id =
                                self.static_values.add(StaticValue::Int(TypedIntValue::UWord64(
                                    self.types.enum_variant_payload_offset_bytes(variant) as u64,
                                )));
                            let payload_info_struct_id =
                                self.static_values.add(StaticValue::Struct(StaticStruct {
                                    type_id: payload_info_struct_id,
                                    fields: eco_vec![type_id_value_id, payload_offset_value_id],
                                }));
                            synth_static_option(
                                &self.types,
                                &mut self.static_values,
                                payload_info_opt_type_id,
                                Some(payload_info_struct_id),
                            )
                        }
                    };

                    variant_values.push(self.static_values.add(StaticValue::Struct(StaticStruct {
                        type_id: variant_struct_type_id,
                        fields: eco_vec![
                            // name: string,
                            name_value_id,
                            // tag: IntValue,
                            tag_value_id,
                            // payload: { typeId: u64, offset: uword }?,
                            payload_info_value_id,
                        ],
                    })))
                }
                let variants_buffer_value_id =
                    self.static_values.add(StaticValue::Buffer(StaticBuffer {
                        elements: variant_values,
                        type_id: variants_buffer_type_id,
                    }));
                let payload_value_id = self.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id: either_payload_type_id,
                    fields: eco_vec![tag_type_value_id, variants_buffer_value_id],
                }));
                make_variant(get_ident!(self, "Either"), Some(payload_value_id))
            }
            Type::EnumVariant(variant) => {
                // {
                //   enumTypeId: u64,
                //   name: string,
                // }
                let variant_payload_type_id =
                    get_schema_variant(get_ident!(self, "Variant")).payload.unwrap();
                let variant_name = variant.name;
                let enum_type_id_value_id = self.static_values.add(StaticValue::Int(
                    TypedIntValue::U64(variant.enum_type_id.as_u32() as u64),
                ));

                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(variant.enum_type_id, span);

                let name_string_id =
                    self.ast.strings.intern(self.ast.idents.get_name(variant_name));
                let name_value_id = self.static_values.add(StaticValue::String(name_string_id));
                let payload_value_id = self.static_values.add(StaticValue::Struct(StaticStruct {
                    type_id: variant_payload_type_id,
                    fields: eco_vec![enum_type_id_value_id, name_value_id],
                }));
                make_variant(get_ident!(self, "Variant"), Some(payload_value_id))
            }
            Type::Never => make_variant(get_ident!(self, "Never"), None),
            Type::Function(_)
            | Type::Lambda(_)
            | Type::LambdaObject(_)
            | Type::TypeParameter(_) => make_variant(
                get_ident!(self, "Other"),
                Some(
                    self.static_values
                        .add(StaticValue::String(self.ast.strings.intern(typ.kind_name()))),
                ),
            ),
            Type::Generic(_)
            | Type::FunctionTypeParameter(_)
            | Type::InferenceHole(_)
            | Type::Unresolved(_) => {
                self.ice_with_span(format!("TypeSchema on {}", typ.kind_name()), span)
            }
            Type::RecursiveReference(_) | Type::Static(_) => self.ice_with_span(
                format!(
                    "TypeSchema on recursive or static type should be unreachable {}",
                    typ.kind_name()
                ),
                span,
            ),
        };

        let static_value_id = self.static_values.add(StaticValue::Enum(static_enum));
        self.type_schemas.insert(type_id, static_value_id);
        static_value_id
    }

    fn get_type_name(&mut self, type_id: TypeId) -> StringId {
        if let Some(existing) = self.type_names.get(&type_id) {
            return *existing;
        }

        let type_string = self.type_id_to_string(type_id);
        let string_id = self.ast.strings.intern(type_string);

        self.type_names.insert(type_id, string_id);
        string_id
    }

    fn register_type_metainfo(&mut self, type_id: TypeId, span: SpanId) {
        let _ = self.get_type_schema(type_id, span);
        let _ = self.get_type_name(type_id);
    }

    fn make_int_kind(
        int_kind_enum: &TypedEnum,
        word_enum: &TypedEnum,
        integer_type: IntegerType,
    ) -> StaticEnum {
        let int_kind_variant = match integer_type {
            IntegerType::U8 => int_kind_enum.variant_by_index(0),
            IntegerType::U16 => int_kind_enum.variant_by_index(1),
            IntegerType::U32 => int_kind_enum.variant_by_index(2),
            IntegerType::U64 => int_kind_enum.variant_by_index(3),
            IntegerType::I8 => int_kind_enum.variant_by_index(4),
            IntegerType::I16 => int_kind_enum.variant_by_index(5),
            IntegerType::I32 => int_kind_enum.variant_by_index(6),
            IntegerType::I64 => int_kind_enum.variant_by_index(7),
            IntegerType::UWord(_) => word_enum.variant_by_index(0),
            IntegerType::IWord(_) => word_enum.variant_by_index(1),
        };
        StaticEnum {
            variant_type_id: int_kind_variant.my_type_id,
            variant_index: int_kind_variant.index,
            typed_as_enum: true,
            payload: None,
        }
    }

    fn make_int_value(
        static_values: &mut Pool<StaticValue, StaticValueId>,
        int_value_enum: &TypedEnum,
        integer_value: TypedIntValue,
    ) -> StaticEnum {
        let variant = match integer_value {
            TypedIntValue::U8(_) => int_value_enum.variant_by_index(0),
            TypedIntValue::U16(_) => int_value_enum.variant_by_index(1),
            TypedIntValue::U32(_) => int_value_enum.variant_by_index(2),
            TypedIntValue::U64(_) => int_value_enum.variant_by_index(3),
            TypedIntValue::UWord32(_) => unreachable!(),
            TypedIntValue::UWord64(_) => unreachable!(),
            TypedIntValue::I8(_) => int_value_enum.variant_by_index(4),
            TypedIntValue::I16(_) => int_value_enum.variant_by_index(5),
            TypedIntValue::I32(_) => int_value_enum.variant_by_index(6),
            TypedIntValue::I64(_) => int_value_enum.variant_by_index(7),
            TypedIntValue::IWord32(_) => unreachable!(),
            TypedIntValue::IWord64(_) => unreachable!(),
        };
        StaticEnum {
            variant_type_id: variant.my_type_id,
            variant_index: variant.index,
            typed_as_enum: false,
            payload: Some(static_values.add(StaticValue::Int(integer_value))),
        }
    }

    fn synth_uword(&mut self, value: usize, span: SpanId) -> TypedExprId {
        let value = match self.target_word_size() {
            WordSize::W32 => TypedIntValue::UWord32(value as u32),
            WordSize::W64 => TypedIntValue::UWord64(value as u64),
        };
        let expr_id = self.exprs.add(TypedExpr::Integer(TypedIntegerExpr { value, span }));
        expr_id
    }

    fn synth_equals_call(
        &mut self,
        lhs: TypedExprId,
        rhs: TypedExprId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let lhs_type = self.exprs.get(lhs).get_type();
        let implementation =
            self.expect_ability_implementation(lhs_type, EQUALS_ABILITY_ID, scope_id, span)?;
        let implementation = self.ability_impls.get(implementation.full_impl_id);
        let ability = self.abilities.get(EQUALS_ABILITY_ID);
        let equals_index =
            ability.find_function_by_name(self.ast.idents.builtins.equals).unwrap().0;
        let equals_implementation = implementation.function_at_index(equals_index);
        let call_expr = self.exprs.add(TypedExpr::Call(Call {
            callee: Callee::from_ability_impl_fn(equals_implementation),
            args: smallvec![lhs, rhs],
            type_args: SliceHandle::Empty,
            return_type: BOOL_TYPE_ID,
            span,
        }));
        Ok(call_expr)
    }

    fn synth_if_else(
        &mut self,
        patterns: SmallVec<[TypedPattern; 1]>,
        result_type: TypeId,
        condition: TypedExprId,
        consequent: TypedExprId,
        alternate: TypedExprId,
        span: SpanId,
    ) -> TypedExprId {
        let condition_diverges = self.exprs.get(condition).get_type() == NEVER_TYPE_ID;
        let cons_arm = TypedMatchArm {
            condition: MatchingCondition {
                patterns,
                instrs: eco_vec![MatchingConditionInstr::Cond { value: condition }],
                binding_eligible: true,
                diverges: condition_diverges,
            },
            consequent_expr: consequent,
        };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition {
                patterns: smallvec![],
                instrs: eco_vec![],
                binding_eligible: true,
                diverges: false,
            },
            consequent_expr: alternate,
        };
        self.exprs.add(TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: eco_vec![],
            result_type,
            arms: eco_vec![cons_arm, alt_arm],
            span,
        }))
    }

    fn synth_cast(
        &mut self,
        expr: TypedExprId,
        target_type: TypeId,
        cast_type: CastType,
    ) -> TypedExprId {
        let span = self.exprs.get(expr).get_span();
        self.exprs.add(TypedExpr::Cast(TypedCast {
            cast_type,
            base_expr: expr,
            target_type_id: target_type,
            span,
        }))
    }

    fn synth_optional_type(&mut self, inner_type: TypeId) -> TypeId {
        self.instantiate_generic_type(OPTIONAL_TYPE_ID, smallvec![inner_type])
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

        let some_expr = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            variant_type_id: some_variant.my_type_id,
            variant_index: some_variant.index,
            span,
            payload: Some(expr_id),
        }));
        let casted = self.synth_cast(some_expr, some_variant.enum_type_id, CastType::VariantToEnum);
        (casted, optional_type)
    }

    fn synth_optional_none(&mut self, type_id: TypeId, span: SpanId) -> TypedExprId {
        let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, smallvec![type_id]);
        let none_variant = self
            .types
            .get(optional_type)
            .expect_enum()
            .variant_by_name(get_ident!(self, "None"))
            .unwrap();
        let none_expr = self.exprs.add(TypedExpr::EnumConstructor(TypedEnumConstructor {
            variant_type_id: none_variant.my_type_id,
            variant_index: none_variant.index,
            span,
            payload: None,
        }));
        let casted = self.synth_cast(none_expr, none_variant.enum_type_id, CastType::VariantToEnum);
        casted
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
        TypedBlock {
            expr_type: UNIT_TYPE_ID,
            statements: eco_vec![],
            scope_id: block_scope_id,
            span,
        }
    }

    /// Creates a non-mutable, mangled, non-referencing variable defn.
    /// This is the vastly most common case
    fn synth_variable_defn_simple(
        &mut self,
        name: Ident,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, false, false, false, owner_scope)
    }

    /// Creates a user-code-visible variable
    fn synth_variable_defn_visible(
        &mut self,
        name: Ident,
        initializer: TypedExprId,
        owner_scope: ScopeId,
    ) -> SynthedVariable {
        self.synth_variable_defn(name, initializer, true, false, false, owner_scope)
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    fn synth_variable_defn(
        &mut self,
        name: Ident,
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
            // TODO(perf): this registers on flamegraph. Re-use a buffer for it
            let new_ident_name =
                format!("__{}_{}", self.ast.idents.get_name(name), self.variables.len());
            self.ast.idents.intern(new_ident_name)
        };
        let variable = Variable {
            name: new_ident,
            is_mutable,
            owner_scope,
            type_id,
            is_context: false,
            global_id: None,
            user_hidden: !no_mangle,
        };
        let variable_id = self.variables.add(variable);
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
            self.ast.exprs.add_expression(ParsedExpr::Variable(parse::ParsedVariable {
                name: NamespacedIdentifier::naked(name, span),
            }));
        self.scopes.add_variable(owner_scope, new_ident, variable_id);
        SynthedVariable { variable_id, defn_stmt, variable_expr, parsed_expr }
    }

    fn synth_parsed_function_call(
        &mut self,
        name: NamespacedIdentifier,
        type_args: &[ParsedTypeExprId],
        args: &[ParsedExprId],
    ) -> ParsedExprId {
        let span = name.span;
        let type_args_iter = type_args.iter().map(|id| NamedTypeArg::unnamed(*id, span));
        let type_args = self.ast.p_type_args.add_slice_from_iter(type_args_iter);
        let args = self
            .ast
            .p_call_args
            .add_slice_from_iter(args.iter().map(|id| parse::ParsedCallArg::unnamed(*id)));
        self.ast.exprs.add_expression(ParsedExpr::Call(ParsedCall {
            name,
            type_args,
            args,
            span,
            is_method: false,
            id: ParsedExprId::PENDING,
        }))
    }

    fn synth_typed_function_call(
        &mut self,
        name: NamespacedIdentifier,
        type_args: &[TypeId],
        args: &[TypedExprId],
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let call_id = self.synth_parsed_function_call(name, &[], &[]);
        let call = self.ast.exprs.get(call_id).expect_call().clone();
        self.eval_function_call(&call, Some((type_args, args)), ctx, None)
    }

    // These are only used by the old coalescing accessor and should be removed when its rebuilt
    fn ident_opt_has_value(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "isSome")
    }

    fn ident_opt_get(&self, span: SpanId) -> NamespacedIdentifier {
        qident!(self, span, ["Opt"], "get")
    }

    #[allow(unused)]
    fn synth_type_of_expr(&mut self, expr: ParsedExprId) -> ParsedTypeExprId {
        let span = self.ast.exprs.get_span(expr);
        self.ast
            .type_exprs
            .add(ParsedTypeExpr::TypeOf(parse::ParsedTypeOf { target_expr: expr, span }))
    }

    #[allow(unused)]
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

    fn synth_parsed_bool_not(&mut self, base: ParsedExprId) -> ParsedExprId {
        let span = self.ast.exprs.get_span(base);
        self.synth_parsed_function_call(qident!(self, span, ["bool"], "negated"), &[], &[base])
    }

    fn synth_printto_call(
        &mut self,
        to_print: TypedExprId,
        writer: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.exprs.get(to_print).get_span();
        let writer_type_id_deref =
            self.types.get_type_id_dereferenced(self.exprs.get(writer).get_type());
        self.synth_typed_function_call(
            qident!(self, span, ["core", "Print"], "printTo"),
            &[writer_type_id_deref],
            &[to_print, writer],
            ctx.with_no_expected_type(),
        )
    }

    pub fn synth_struct_expr(
        &mut self,
        struct_type_id: TypeId,
        field_exprs: Vec<TypedExprId>,
        scope_id: ScopeId,
        span: SpanId,
    ) -> TypedExprId {
        let struct_type = self.types.get(struct_type_id).expect_struct();
        debug_assert_eq!(struct_type.fields.len(), field_exprs.len());
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
        self.exprs.add(TypedExpr::Struct(StructLiteral { fields, type_id: struct_type_id, span }))
    }

    pub fn get_span_location(&self, span: SpanId) -> (&parse::Source, &parse::Line) {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get_source(the_span.file_id);
        let line = source.get_line_for_span_start(the_span).unwrap();
        (source, line)
    }

    pub fn synth_source_location(&mut self, span: SpanId) -> TypedExprId {
        let (_, line) = self.get_span_location(span);
        let line_number = line.line_number();

        let source = self.ast.sources.source_by_span(self.ast.spans.get(span));
        let filename_string_id = self.ast.strings.intern(&source.filename);

        let struct_expr = TypedExpr::Struct(StructLiteral {
            fields: vec![
                StructField {
                    name: self.ast.idents.builtins.filename,
                    expr: self.exprs.add(TypedExpr::String(filename_string_id, span)),
                },
                StructField {
                    name: self.ast.idents.builtins.line,
                    expr: self.exprs.add(TypedExpr::Integer(TypedIntegerExpr {
                        value: TypedIntValue::U64(line_number as u64),
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
        message: &str,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let message_string_id = self.ast.strings.intern(message);
        let message_expr = self.exprs.add(TypedExpr::String(message_string_id, span));
        self.synth_typed_function_call(qident!(self, span, "crash"), &[], &[message_expr], ctx)
    }

    fn synth_discard_call(
        &mut self,
        value: TypedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let span = self.exprs.get(value).get_span();
        self.synth_typed_function_call(qident!(self, span, ["core"], "discard"), &[], &[value], ctx)
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
            let ident_str = self.ident_str(*identifier);

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
        name: Ident,
        delimiter: &str,
        skip_root: bool,
    ) -> String {
        let mut buf = Vec::with_capacity(64);
        self.write_qualified_name(&mut buf, scope, self.ident_str(name), delimiter, skip_root);
        String::from_utf8(buf).unwrap()
    }

    // Errors and logging

    pub fn report_error(&mut self, e: TyperError) {
        self.write_error(&mut std::io::stderr(), &e).unwrap();
        self.errors.push(e);
    }

    pub fn write_error(
        &self,
        w: &mut impl std::io::Write,
        error: &TyperError,
    ) -> std::io::Result<()> {
        write_error(w, &self.ast.spans, &self.ast.sources, &error.message, error.level, error.span)
    }

    pub fn write_location(&self, w: &mut impl std::io::Write, span: SpanId) {
        parse::write_source_location(
            w,
            &self.ast.spans,
            &self.ast.sources,
            span,
            ErrorLevel::Info,
            6,
            None,
        )
        .unwrap()
    }

    pub fn write_location_error(&self, w: &mut impl std::io::Write, span: SpanId) {
        parse::write_source_location(
            w,
            &self.ast.spans,
            &self.ast.sources,
            span,
            ErrorLevel::Error,
            6,
            None,
        )
        .unwrap()
    }

    #[track_caller]
    pub fn ice_with_span(&self, msg: impl AsRef<str>, span: SpanId) -> ! {
        self.write_location_error(&mut std::io::stderr(), span);
        panic!("Internal Compiler Error: {}", msg.as_ref())
    }

    #[track_caller]
    pub fn ice(&self, msg: impl AsRef<str>, error: Option<&TyperError>) -> ! {
        if let Some(error) = error {
            self.write_error(&mut std::io::stderr(), error).unwrap();
        }
        panic!("Internal Compiler Error at: {}", msg.as_ref())
    }

    #[track_caller]
    pub fn todo_with_span(&self, msg: impl AsRef<str>, span: SpanId) -> ! {
        self.write_location_error(&mut std::io::stderr(), span);
        panic!("not yet implemented: {}", msg.as_ref())
    }
}

fn synth_static_option(
    types: &Types,
    static_values: &mut Pool<StaticValue, StaticValueId>,
    option_type_id: TypeId,
    value_id: Option<StaticValueId>,
) -> StaticValueId {
    let opt_enum_type = types.get(option_type_id).expect_enum();
    let static_enum = match value_id {
        None => StaticEnum {
            variant_type_id: opt_enum_type.variant_by_index(0).my_type_id,
            variant_index: 0,
            typed_as_enum: true,
            payload: None,
        },
        Some(value_id) => StaticEnum {
            variant_type_id: opt_enum_type.variant_by_index(1).my_type_id,
            variant_index: 1,
            typed_as_enum: true,
            payload: Some(value_id),
        },
    };

    static_values.add(StaticValue::Enum(static_enum))
}
