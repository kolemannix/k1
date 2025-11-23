// Copyright (c) 2025 knix
// All rights reserved.

pub(crate) mod derive;
pub(crate) mod dump;
pub(crate) mod infer;
pub(crate) mod scopes;
pub(crate) mod static_value;
pub(crate) mod synth;
pub(crate) mod typed_int_value;
pub(crate) mod types;
pub(crate) mod visit;

use crate::{bc, vm};
use bitflags::bitflags;
use ecow::{EcoVec, eco_vec};
use itertools::Itertools;
pub use static_value::{
    StaticContainer, StaticContainerKind, StaticEnum, StaticStruct, StaticValue, StaticValueId,
    StaticValuePool,
};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::collections::hash_map::Entry;
use std::error::Error;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::io::stderr;
use std::num::NonZeroU32;
use std::path::{Path, PathBuf};
use synth::synth_static_option;
pub use typed_int_value::TypedIntValue;

use crate::kmem::{MHandle, MList, MSlice, Mem};
use crate::{DepEq, DepHash, SV2, kmem};
use ahash::{HashMapExt, HashSetExt};
use anyhow::bail;
use colored::Colorize;
use either::Either;
use fxhash::{FxHashMap, FxHashSet};
use log::{debug, error, trace};
use smallvec::{SmallVec, smallvec};

use scopes::*;
use types::*;

use crate::compiler::CompilerConfig;
use crate::lex::{self, SpanId, Spans, TokenKind};
use crate::parse::{
    self, BinaryOpKind, FileId, ForExpr, ForExprType, Ident, IdentSlice, NamedTypeArg,
    NamedTypeArgId, NumericWidth, ParseError, ParsedAbilityId, ParsedAbilityImplId, ParsedBlock,
    ParsedBlockKind, ParsedCall, ParsedCallArg, ParsedCast, ParsedExpr, ParsedExprId,
    ParsedFunctionId, ParsedGlobalId, ParsedId, ParsedIfExpr, ParsedList, ParsedLiteral,
    ParsedLoopExpr, ParsedNamespaceId, ParsedPattern, ParsedPatternId, ParsedProgram,
    ParsedStaticBlockKind, ParsedStaticExpr, ParsedStmt, ParsedStmtId, ParsedTypeConstraintExpr,
    ParsedTypeDefnId, ParsedTypeExpr, ParsedTypeExprId, ParsedUnaryOpKind, ParsedUseId,
    ParsedVariable, ParsedWhileExpr, QIdent, Sources, StringId, StructValueField,
};
use crate::pool::{SliceHandle, VPool};
use crate::{SV4, SV8, impl_copy_if_small, nz_u32_id, static_assert_size, strings};

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
    External { lib_name: Option<Ident>, fn_name: Option<Ident> },
    Intrinsic,
}

impl Linkage {
    pub fn is_external(&self) -> bool {
        matches!(self, Linkage::External { .. })
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbilityFunctionRef {
    pub function_name: Ident,
    pub index: u32,
    pub ability_id: AbilityId,
    pub function_id: FunctionId,
}
impl_copy_if_small!(16, TypedAbilityFunctionRef);

pub const GLOBAL_ID_IS_STATIC: TypedGlobalId =
    TypedGlobalId::from_nzu32(NonZeroU32::new(1).unwrap());

pub const EQUALS_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(1).unwrap());
pub const WRITER_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(2).unwrap());
pub const WRITE_TEXT_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(3).unwrap());
pub const SHOW_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(4).unwrap());
pub const BITWISE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(5).unwrap());
pub const ADD_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(6).unwrap());
pub const SUB_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(7).unwrap());
pub const MUL_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(8).unwrap());
pub const DIV_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(9).unwrap());
pub const REM_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(10).unwrap());
pub const SCALAR_CMP_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(11).unwrap());
pub const COMPARABLE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(12).unwrap());
pub const UNWRAP_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(13).unwrap());
pub const TRY_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(14).unwrap());
pub const ITERATOR_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(15).unwrap());
pub const ITERABLE_ABILITY_ID: AbilityId = AbilityId(NonZeroU32::new(16).unwrap());

pub const FUNC_PARAM_IDEAL_COUNT: usize = 8;
pub const FUNC_TYPE_PARAM_IDEAL_COUNT: usize = 4;

pub const UNIT_BYTE_VALUE: u8 = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSubstitutionPair {
    from: TypeId,
    to: TypeId,
}

// Allows syntax spair! { a -> b }
#[macro_export]
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
    pub solutions_so_far: Vec<TypeSubstitutionPair>,
    pub inference_vars: Vec<TypeId>,
    pub constraints: Vec<TypeSubstitutionPair>,
    pub substitutions: FxHashMap<TypeId, TypeId>,
    pub substitutions_vec: Vec<TypeSubstitutionPair>,
    pub start_raw: u64,
}

impl InferenceContext {
    pub fn make() -> Self {
        InferenceContext {
            origin_stack: Vec::with_capacity(64),
            params: Vec::with_capacity(128),
            solutions_so_far: Vec::with_capacity(64),
            inference_vars: Vec::with_capacity(32),
            constraints: Vec::with_capacity(256),
            substitutions: FxHashMap::with_capacity(256),
            substitutions_vec: Vec::with_capacity(256),
            start_raw: 0,
        }
    }

    pub fn reset(&mut self) {
        self.constraints.clear();
        self.params.clear();
        self.solutions_so_far.clear();
        self.inference_vars.clear();
        self.substitutions.clear();
        self.substitutions_vec.clear();
        self.start_raw = 0;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StaticExecContext {
    #[allow(unused)]
    is_metaprogram: bool,
    /// If a `return` is used, what type is expected
    /// This is needed because `return` usually looks at the
    /// enclosing function, but for #static blocks it shouldn't
    /// So this type can be different than the usual ctx.expected_type
    expected_return_type: Option<TypeId>,
}

pub enum StaticExecutionResult {
    TypedExpr(TypedExprId),
    Definitions(EcoVec<ParsedId>),
}

pub enum ParseAdHocKind {
    Expr,
    Definitions,
}

pub enum ParseAdHocResult {
    Expr(ParsedExprId),
    Definitions(EcoVec<ParsedId>),
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct EvalExprFlags: u8 {
        const Inference = 1;
        /// Indicates whether we are typechecking generic code
        /// Most commonly, the body of a generic function
        const GenericPass = 1 << 1;
        const Defer = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EvalExprContext {
    scope_id: ScopeId,
    expected_type_id: Option<TypeId>,
    static_ctx: Option<StaticExecContext>,
    global_defn_name: Option<Ident>,
    flags: EvalExprFlags,
}
impl EvalExprContext {
    fn make(scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext {
            scope_id,
            expected_type_id: None,
            static_ctx: None,
            global_defn_name: None,
            flags: EvalExprFlags::empty(),
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
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Inference, is_inference);
        EvalExprContext { flags, ..*self }
    }

    fn is_inference(&self) -> bool {
        self.flags.contains(EvalExprFlags::Inference)
    }

    fn with_scope(&self, scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext { scope_id, ..*self }
    }

    pub fn with_is_generic_pass(&self, is_generic_pass: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::GenericPass, is_generic_pass);
        EvalExprContext { flags, ..*self }
    }

    fn is_generic_pass(&self) -> bool {
        self.flags.contains(EvalExprFlags::GenericPass)
    }

    pub fn with_is_defer(&self, defer: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Defer, defer);
        EvalExprContext { flags, ..*self }
    }
}

#[derive(Debug, Clone, Copy)]
enum MaybeTypedExpr {
    Parsed(ParsedExprId),
    Typed(TypedExprId),
}

#[derive(Debug, Clone, Copy)]
enum TypeOrParsedExpr {
    Type(TypeId),
    Parsed(ParsedExprId),
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

    pub fn offset_at_index(&self, index: usize) -> usize {
        self.stride() as usize * index
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

nz_u32_id!(PatternCtorId);

impl PatternCtorId {
    pub const UNIT: PatternCtorId = PatternCtorId::from_u32(1).unwrap();
    pub const B_FALSE: PatternCtorId = PatternCtorId::from_u32(2).unwrap();
    pub const B_TRUE: PatternCtorId = PatternCtorId::from_u32(3).unwrap();
    pub const CHAR: PatternCtorId = PatternCtorId::from_u32(4).unwrap();
    pub const STRING: PatternCtorId = PatternCtorId::from_u32(5).unwrap();
    pub const INT: PatternCtorId = PatternCtorId::from_u32(6).unwrap();
    pub const FLOAT: PatternCtorId = PatternCtorId::from_u32(7).unwrap();
    pub const POINTER: PatternCtorId = PatternCtorId::from_u32(8).unwrap();

    pub const TYPE_VARIABLE: PatternCtorId = PatternCtorId::from_u32(9).unwrap();
    pub const FUNCTION_POINTER: PatternCtorId = PatternCtorId::from_u32(10).unwrap();
}

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
    FunctionPointer,
    /// In the future, we should do real array patterns since length is statically known
    Array,
    Reference(PatternCtorId),
    Struct {
        fields: SV4<(Ident, PatternCtorId)>,
    },
    Enum {
        variant_name: Ident,
        inner: Option<PatternCtorId>,
    },
}

impl PatternCtor {
    pub fn push_field(&mut self, field: (Ident, PatternCtorId)) {
        match self {
            PatternCtor::Struct { fields } => fields.push(field),
            _ => {}
        }
    }
}

#[derive(Clone, Copy)]
struct PatternCtorTrialEntry {
    ctor: PatternCtorId,
    alive: bool,
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
            TypedAbilityKind::Concrete => SliceHandle::empty(),
            TypedAbilityKind::Generic { .. } => SliceHandle::empty(),
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

pub(crate) struct ArgsAndParams {
    args: SV8<MaybeTypedExpr>,
    params: SV8<FnParamType>,
}

impl ArgsAndParams {
    fn iter(&self) -> impl Iterator<Item = (&MaybeTypedExpr, &FnParamType)> {
        self.args.iter().zip(self.params.iter())
    }

    fn get(&self, index: usize) -> (MaybeTypedExpr, FnParamType) {
        (self.args[index], self.params[index])
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
    pub fn find_function_by_name(&self, name: Ident) -> Option<TypedAbilityFunctionRef> {
        self.functions.iter().find(|f| f.function_name == name).copied()
    }

    pub fn parent_ability_id(&self) -> Option<AbilityId> {
        match &self.kind {
            TypedAbilityKind::Concrete => None,
            TypedAbilityKind::Generic { .. } => None,
            TypedAbilityKind::Specialized(specialization) => Some(specialization.generic_parent),
        }
    }
}

#[derive(Clone, Copy)]
pub struct TypedEnumPattern {
    pub enum_type_id: TypeId,
    pub variant_tag_name: Ident,
    pub variant_index: u32,
    pub payload: Option<TypedPatternId>,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedStructPatternField {
    pub name: Ident,
    pub pattern: TypedPatternId,
    pub field_index: u32,
    pub field_type_id: TypeId,
}

#[derive(Clone, Copy)]
pub struct TypedStructPattern {
    pub struct_type_id: TypeId,
    pub fields: MSlice<TypedStructPatternField, TypedPatternPool>,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct VariablePattern {
    pub name: Ident,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedReferencePattern {
    pub inner_pattern: TypedPatternId,
    pub span: SpanId,
}

// <pattern> ::= <literal> | <variable> | <enum> | <struc>
// <literal> ::= "(" ")" | "\"" <ident> "\"" | [0-9]+ | "'" [a-z] "'" | "None"
// <variable> ::= <ident>
// <ident> ::= [a-z]*
// <enum> ::= "." <ident> ( "(" <pattern> ")" )?
// <struc> ::= "{" ( <ident> ": " <pattern> ","? )* "}"

type TypedPatternId = MHandle<TypedPattern, TypedPatternPool>;
type TypedPatternSlice = MSlice<TypedPatternId, TypedPatternPool>;

pub struct TypedPatternPool {
    mem: kmem::Mem<TypedPatternPool>,
}

impl TypedPatternPool {
    pub fn make() -> TypedPatternPool {
        Self { mem: Mem::make() }
    }

    pub fn get(&self, pattern_id: TypedPatternId) -> &TypedPattern {
        self.mem.get(pattern_id)
    }

    pub fn get_slice<T>(&self, slice: MSlice<T, TypedPatternPool>) -> &'static [T] {
        self.mem.getn(slice)
    }

    pub fn add(&mut self, pattern: TypedPattern) -> TypedPatternId {
        self.mem.push_h(pattern)
    }

    pub fn add_pattern_slice(&mut self, data: &[TypedPatternId]) -> TypedPatternSlice {
        self.mem.pushn(data)
    }

    pub fn get_pattern_bindings(
        &self,
        pattern_id: TypedPatternId,
    ) -> SmallVec<[VariablePattern; 8]> {
        let mut v = smallvec![];
        self.get_pattern_bindings_rec(pattern_id, &mut v);
        // This sorts by the Identifier id, not the name itself, but that's absolutely fine
        v.sort_by_key(|vp| vp.name);
        v
    }
    fn get_pattern_bindings_rec(
        &self,
        pattern_id: TypedPatternId,
        bindings: &mut SmallVec<[VariablePattern; 8]>,
    ) {
        match self.mem.get(pattern_id) {
            TypedPattern::LiteralUnit(_) => (),
            TypedPattern::LiteralChar(_, _) => (),
            TypedPattern::LiteralInteger(_, _) => (),
            TypedPattern::LiteralFloat(_, _) => (),
            TypedPattern::LiteralBool(_, _) => (),
            TypedPattern::LiteralString(_, _) => (),
            TypedPattern::Variable(variable_pattern) => bindings.push(*variable_pattern),
            TypedPattern::Enum(enum_pattern) => {
                if let Some(payload_pattern_id) = enum_pattern.payload.as_ref() {
                    self.get_pattern_bindings_rec(*payload_pattern_id, bindings)
                }
            }
            TypedPattern::Struct(struct_pattern) => {
                for field_pattern in self.mem.getn(struct_pattern.fields).iter() {
                    self.get_pattern_bindings_rec(field_pattern.pattern, bindings)
                }
            }
            TypedPattern::Wildcard(_) => (),
            TypedPattern::Reference(refer) => {
                self.get_pattern_bindings_rec(refer.inner_pattern, bindings)
            }
        }
    }
    pub fn pattern_has_innumerable_literal(&self, pattern_id: TypedPatternId) -> bool {
        match self.mem.get(pattern_id) {
            TypedPattern::LiteralChar(_, _span) => true,
            TypedPattern::LiteralInteger(_, _span) => true,
            TypedPattern::LiteralFloat(_, _span) => true,
            TypedPattern::LiteralString(_, _span) => true,
            TypedPattern::LiteralUnit(_span_id) => false,
            TypedPattern::LiteralBool(_, _span_id) => false,
            TypedPattern::Variable(_variable_pattern) => false,
            TypedPattern::Enum(typed_enum_pattern) => typed_enum_pattern
                .payload
                .as_ref()
                .is_some_and(|p| self.pattern_has_innumerable_literal(*p)),
            TypedPattern::Struct(typed_struct_pattern) => {
                self.mem.getn(typed_struct_pattern.fields).iter().any(|field_pattern| {
                    self.pattern_has_innumerable_literal(field_pattern.pattern)
                })
            }
            TypedPattern::Wildcard(_span_id) => false,
            TypedPattern::Reference(refer) => {
                self.pattern_has_innumerable_literal(refer.inner_pattern)
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum TypedPattern {
    // Consider replacing with a single Literal using StaticValue
    LiteralUnit(SpanId),
    LiteralChar(u8, SpanId),
    LiteralInteger(StaticValueId, SpanId),
    LiteralFloat(StaticValueId, SpanId),
    LiteralBool(bool, SpanId),
    LiteralString(StringId, SpanId),
    Variable(VariablePattern),
    Enum(TypedEnumPattern),
    Struct(TypedStructPattern),
    Wildcard(SpanId),
    Reference(TypedReferencePattern),
}

impl TypedPattern {
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

pub struct BlockBuilder {
    pub scope_id: ScopeId,
    pub statements: MList<TypedStmtId, TypedProgram>,
    pub span: SpanId,
}

#[derive(Clone)]
pub struct TypedBlock {
    pub scope_id: ScopeId,
    pub statements: MSlice<TypedStmtId, TypedProgram>,
}

#[derive(Debug, Clone)]
pub struct SpecializationInfo {
    pub parent_function: FunctionId,
    pub type_arguments: NamedTypeSlice,
    pub function_type_arguments: NamedTypeSlice,
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
    pub function_type_params: SliceHandle<FunctionTypeParamId>,
}
impl_copy_if_small!(32, FunctionSignature);

impl FunctionSignature {
    pub fn make_no_generics(name: Option<Ident>, function_type: TypeId) -> FunctionSignature {
        FunctionSignature {
            name,
            function_type,
            type_params: SliceHandle::empty(),
            function_type_params: SliceHandle::empty(),
        }
    }

    pub fn has_type_params(&self) -> bool {
        !self.type_params.is_empty() || !self.function_type_params.is_empty()
    }
}

#[derive(Clone)]
pub struct TypedFunction {
    pub name: Ident,
    pub scope: ScopeId,
    pub param_variables: MSlice<VariableId, TypedProgram>,
    pub type_params: NamedTypeSlice,
    pub function_type_params: SliceHandle<FunctionTypeParamId>,
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
        }
    }

    fn is_generic(&self) -> bool {
        matches!(self.kind, TypedFunctionKind::AbilityDefn(_)) || self.signature().has_type_params()
    }
}

nz_u32_id!(FunctionTypeParamId);
#[derive(Debug, Clone, Copy)]
/// When a function takes a special type parameter, either a 'function_like'
/// or a 'static'. This ties the type parameter to its value param, which
/// is always a 1-1 relationship. As in:
/// fn example(knownInt: static int, some thunk: () -> ())
///            ^ existential type param 1, a static
///                                  ^ existential type param 2, a function type param
pub struct FunctionTypeParam {
    pub name: Ident,
    pub type_id: TypeId,
    pub value_param_index: u32,
    pub span: SpanId,
}

impl HasName for &FunctionTypeParam {
    fn name(&self) -> Ident {
        self.name
    }
}

impl HasTypeId for &FunctionTypeParam {
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl HasName for FunctionTypeParam {
    fn name(&self) -> Ident {
        self.name
    }
}

impl HasTypeId for FunctionTypeParam {
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

pub type NamedTypeSlice = SliceHandle<NameAndTypeId>;

nz_u32_id!(TypeSliceId);

pub type TypeIdSlice = MSlice<TypeId, TypePool>;

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

pub(crate) enum TypeUnificationResult {
    Matching,
    NoHoles,
    NonMatching(&'static str),
}

#[derive(Clone)]
pub struct VariableExpr {
    pub variable_id: VariableId,
}
impl_copy_if_small!(4, VariableExpr);

#[derive(Clone, Copy)]
pub struct DerefExpr {
    pub target: TypedExprId,
}

#[derive(Clone)]
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
    /// Must contain a Function pointer
    DynamicFunction {
        function_pointer_expr: TypedExprId,
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

    pub fn from_ability_impl_fn(ability_impl_fn: &AbilityImplFunction) -> Callee {
        match ability_impl_fn {
            AbilityImplFunction::FunctionId(function_id) => Callee::StaticFunction(*function_id),
            AbilityImplFunction::Abstract(sig) => Callee::Abstract { function_sig: *sig },
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

#[derive(Clone)]
pub struct Call {
    pub callee: Callee,
    pub args: SmallVec<[TypedExprId; FUNC_PARAM_IDEAL_COUNT]>,
    /// type_args remain unerased for some intrinsics where we want codegen to see the types.
    /// Specifically sizeOf[T], since there's no actual value to specialize on. kinda a hack would be
    /// better to specialize anyway and inline? idk
    pub type_args: NamedTypeSlice,
    pub return_type: TypeId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct StructLiteralField {
    pub name: Ident,
    pub expr: TypedExprId,
}

#[derive(Clone)]
pub struct StructLiteral {
    pub fields: MSlice<StructLiteralField, TypedProgram>,
}

#[derive(Debug, Clone)]
pub enum ArrayLiteralElements {
    Filled(TypedExprId, u64),
    Listed(EcoVec<TypedExprId>),
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: ArrayLiteralElements,
    pub type_id: TypeId,
    pub span: SpanId,
}

impl ArrayLiteral {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> u64 {
        match &self.elements {
            ArrayLiteralElements::Filled(_, len) => *len,
            ArrayLiteralElements::Listed(elements) => elements.len() as u64,
        }
    }
}

#[derive(Clone)]
pub struct ArrayGetElement {
    pub base: TypedExprId,
    pub index: TypedExprId,
    pub array_type: TypeId,
    // This is really just a field access by number instead of name
    pub access_kind: FieldAccessKind,
}
impl_copy_if_small!(16, ArrayGetElement);

/// Also used for EnumGetPayload.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FieldAccessKind {
    ValueToValue,
    Dereference,
    ReferenceThrough,
}

/// `base` can be a reference to a struct, or a struct.
/// `is_referencing` talks about the _result_ type. If the base
/// is a reference, you can take the field either as a value or a reference itself
/// is_referencing = true means you're getting a pointer to the field
/// is_referencing = false means you're getting a loaded value.
///
/// If the base is a reference, but is-referencing is false, then that's a
/// "dereferencing" field access, which requires a copy of the value.
/// So maybe is_referencing should be instead some flags or style that also
/// encode whether the base is a reference
#[derive(Clone)]
pub struct FieldAccess {
    pub base: TypedExprId,
    pub field_index: u32,
    pub struct_type: TypeId,
    pub access_kind: FieldAccessKind,
}

impl FieldAccess {
    pub fn is_reference_through(&self) -> bool {
        matches!(self.access_kind, FieldAccessKind::ReferenceThrough)
    }
}

#[derive(Clone)]
pub struct TypedEnumConstructor {
    pub variant_index: u32,
    pub payload: Option<TypedExprId>,
}
impl_copy_if_small!(16, TypedEnumConstructor);

#[derive(Clone)]
pub struct GetEnumVariantPayload {
    pub enum_variant_expr: TypedExprId,
    pub variant_index: u32,
    pub access_kind: FieldAccessKind,
}
impl_copy_if_small!(12, GetEnumVariantPayload);

/// enum_expr can be an enum value, or a Type::Reference to an enum
/// This saves us having to generate a dereference of the entire value
/// Since we know we are just interested in the tag
#[derive(Debug, Clone)]
pub struct GetEnumTag {
    pub enum_expr_or_reference: TypedExprId,
}
impl_copy_if_small!(4, GetEnumTag);

#[derive(Debug, Clone)]
pub struct TypedEnumIsVariantExpr {
    pub enum_expr: TypedExprId,
    pub variant_index: u32,
    pub span: SpanId,
}
impl_copy_if_small!(16, TypedEnumIsVariantExpr);

#[derive(Clone, Copy)]
pub struct TypedMatchArm {
    pub condition: MatchingCondition,
    pub consequent_expr: TypedExprId,
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

    pub fn get_scalar_type(&self) -> ScalarType {
        match self {
            TypedFloatValue::F32(_) => ScalarType::F32,
            TypedFloatValue::F64(_) => ScalarType::F64,
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
    SignChange,
}

#[derive(Debug, Clone, Copy)]
pub enum CastType {
    IntegerCast(IntegerCastDirection),
    Integer8ToChar,
    IntegerExtendFromChar,
    EnumToVariant,
    VariantToEnum,
    ReferenceToMut,
    ReferenceUnMut,
    ReferenceToReference,
    PointerToReference,
    ReferenceToPointer,
    /// Destination type can only be u64 and i64
    PointerToWord,
    WordToPointer,
    FloatExtend,
    FloatTruncate,
    FloatToUnsignedInteger,
    FloatToSignedInteger,
    IntegerUnsignedToFloat,
    IntegerSignedToFloat,
    LambdaToLambdaObject,
    Transmute,
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
            CastType::ReferenceToMut => write!(f, "reference2mut"),
            CastType::ReferenceUnMut => write!(f, "referenceUnmut"),
            CastType::ReferenceToReference => write!(f, "reftoref"),
            CastType::PointerToReference => write!(f, "ptrtoref"),
            CastType::ReferenceToPointer => write!(f, "reftoptr"),
            CastType::PointerToWord => write!(f, "ptrtoword"),
            CastType::WordToPointer => write!(f, "wordtoptr"),
            CastType::FloatExtend => write!(f, "fext"),
            CastType::FloatTruncate => write!(f, "ftrunc"),
            CastType::FloatToUnsignedInteger => write!(f, "ftouint"),
            CastType::FloatToSignedInteger => write!(f, "ftosint"),
            CastType::IntegerUnsignedToFloat => write!(f, "uinttof"),
            CastType::IntegerSignedToFloat => write!(f, "sinttof"),
            CastType::LambdaToLambdaObject => write!(f, "lam2dyn"),
            CastType::Transmute => write!(f, "never"),
            CastType::StaticErase => write!(f, "staticerase"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedCast {
    pub cast_type: CastType,
    pub base_expr: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub value: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct TypedBreak {
    pub value: TypedExprId,
    pub loop_scope: ScopeId,
    pub loop_type: LoopType,
}

#[derive(Debug, Clone)]
pub struct LambdaExpr {
    pub lambda_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct FunctionToLambdaObjectExpr {
    pub function_id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct FunctionPointerExpr {
    pub function_id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct PendingCaptureExpr {
    pub captured_variable_id: VariableId,
    pub resolved_expr: Option<TypedExprId>,
}

#[derive(Clone, Copy)]
pub struct MatchingCondition {
    pub instrs: MSlice<MatchingConditionInstr, TypedProgram>,
}

#[derive(Debug, Clone)]
pub enum MatchingConditionInstr {
    Binding { let_stmt: TypedStmtId },
    Cond { value: TypedExprId },
}
impl_copy_if_small!(8, MatchingConditionInstr);

#[derive(Clone)]
pub struct WhileLoop {
    pub condition: MatchingCondition,
    pub body: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body_block: TypedExprId,
}
impl_copy_if_small!(4, LoopExpr);

#[derive(Clone)]
/// Invariant: The last arm's condition must always evaluate to 'true'
pub struct TypedMatchExpr {
    pub initial_let_statements: MSlice<TypedStmtId, TypedProgram>,
    pub arms: MSlice<TypedMatchArm, TypedProgram>,
}

#[derive(Clone, Copy)]
pub struct StaticConstantExpr {
    pub value_id: StaticValueId,
    pub is_typed_as_static: bool,
}

nz_u32_id!(CallId);

static_assert_size!(TypedExpr, 20);
#[derive(Clone)]
pub enum TypedExpr {
    StaticValue(StaticConstantExpr),
    Struct(StructLiteral),
    // Current largest variant at 20 bytes
    StructFieldAccess(FieldAccess),
    ArrayGetElement(ArrayGetElement),
    Variable(VariableExpr),
    Deref(DerefExpr),
    Block(TypedBlock),
    Call {
        call_id: CallId,
    },
    /// In the past, we lowered match to an if/else chain. This proves not quite powerful enough
    /// of a representation to do everything we want
    Match(TypedMatchExpr),
    WhileLoop(WhileLoop),
    LoopExpr(LoopExpr),
    EnumConstructor(TypedEnumConstructor),
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
    /// Calling .toRef() on a function by name
    FunctionPointer(FunctionPointerExpr),
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
}

impl From<VariableExpr> for TypedExpr {
    fn from(value: VariableExpr) -> Self {
        TypedExpr::Variable(value)
    }
}

impl TypedExpr {
    pub fn kind_str(&self) -> &'static str {
        match self {
            TypedExpr::Struct(_) => "struct",
            TypedExpr::StructFieldAccess(_) => "struct_field_access",
            TypedExpr::ArrayGetElement(_) => "array_get_element",
            TypedExpr::Variable(_) => "variable",
            TypedExpr::Deref(_) => "deref",
            TypedExpr::Block(_) => "block",
            TypedExpr::Call { .. } => "call",
            TypedExpr::Match(_) => "match",
            TypedExpr::WhileLoop(_) => "while_loop",
            TypedExpr::LoopExpr(_) => "loop",
            TypedExpr::EnumConstructor(_) => "enum_constructor",
            TypedExpr::EnumGetTag(_) => "enum_get_tag",
            TypedExpr::EnumGetPayload(_) => "enum_get_payload",
            TypedExpr::Cast(_) => "cast",
            TypedExpr::Return(_) => "return",
            TypedExpr::Break(_) => "break",
            TypedExpr::Lambda(_) => "lambda",
            TypedExpr::FunctionPointer(_) => "function_pointer",
            TypedExpr::FunctionToLambdaObject(_) => "function_to_lambda_object",
            TypedExpr::PendingCapture(_) => "pending_capture",
            TypedExpr::StaticValue(_) => "static_value",
        }
    }

    pub fn expect_variable(self) -> VariableExpr {
        if let Self::Variable(v) = self { v } else { panic!("Expected variable expression") }
    }

    pub fn expect_call_id(&self) -> CallId {
        if let Self::Call { call_id, .. } = self {
            *call_id
        } else {
            panic!("Expected call expression")
        }
    }
}

enum CheckExprTypeResult<'a> {
    Ok,
    Err(String),
    Coerce(TypedExprId, Cow<'a, str>),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub variable_id: VariableId,
    pub variable_type: TypeId,
    pub initializer: Option<TypedExprId>,
    pub is_referencing: bool,
    pub span: SpanId,
}
impl_copy_if_small!(20, LetStmt);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentKind {
    Set,
    Store,
}

#[derive(Debug, Clone)]
pub struct AssignmentStmt {
    pub destination: TypedExprId,
    pub value: TypedExprId,
    pub span: SpanId,
    pub kind: AssignmentKind,
}
impl_copy_if_small!(16, AssignmentStmt);

#[derive(Clone)]
pub struct TypedRequireStmt {
    pub condition: Box<MatchingCondition>,
    pub else_body: TypedExprId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedDeferStmt {
    pub parsed_expr: ParsedExprId,
    pub span: SpanId,
}

static_assert_size!(TypedStmt, 24);
#[derive(Clone)]
pub enum TypedStmt {
    Expr(TypedExprId, TypeId),
    Let(LetStmt),
    Assignment(AssignmentStmt),
    Require(TypedRequireStmt),
    Defer(TypedDeferStmt),
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

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct VariableFlags: u16 {
        const Reassigned = 1;
        const Context = 1 << 1;
        const UserHidden = 1 << 2;
    }
}

#[derive(Clone, Copy)]
pub enum VariableKind {
    FnParam(FunctionId),
    Let(TypedStmtId),
    Global(TypedGlobalId),
}

#[derive(Clone)]
pub struct Variable {
    pub name: Ident,
    pub type_id: TypeId,
    pub owner_scope: ScopeId,
    pub flags: VariableFlags,
    pub usage_count: u32,
    pub kind: VariableKind,
}

impl Variable {
    pub fn global_id(&self) -> Option<TypedGlobalId> {
        match self.kind {
            VariableKind::Global(global_id) => Some(global_id),
            _ => None,
        }
    }

    pub fn reassigned(&self) -> bool {
        self.flags.contains(VariableFlags::Reassigned)
    }
    pub fn context(&self) -> bool {
        self.flags.contains(VariableFlags::Context)
    }
    pub fn user_hidden(&self) -> bool {
        self.flags.contains(VariableFlags::UserHidden)
    }
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
    pub is_tls: bool,
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
    pub namespaces: VPool<Namespace, NamespaceId>,
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

    pub fn iter(&self) -> std::slice::Iter<'_, Namespace> {
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
    SignedShiftRight,
    UnsignedShiftRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicArithOpClass {
    Float,
    UnsignedInt,
    SignedInt,
}

impl IntrinsicArithOpClass {
    pub fn is_signed_int(self) -> bool {
        matches!(self, IntrinsicArithOpClass::SignedInt)
    }
    pub fn from_int_type(i: IntegerType) -> Self {
        if i.is_signed() {
            IntrinsicArithOpClass::SignedInt
        } else {
            IntrinsicArithOpClass::UnsignedInt
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicArithOpOp {
    Equals,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntrinsicArithOpKind {
    pub class: IntrinsicArithOpClass,
    pub op: IntrinsicArithOpOp,
}

impl IntrinsicArithOpKind {
    pub fn uint(op: IntrinsicArithOpOp) -> Self {
        IntrinsicArithOpKind { class: IntrinsicArithOpClass::UnsignedInt, op }
    }
    pub fn sint(op: IntrinsicArithOpOp) -> Self {
        IntrinsicArithOpKind { class: IntrinsicArithOpClass::SignedInt, op }
    }
    pub fn float(op: IntrinsicArithOpOp) -> Self {
        IntrinsicArithOpKind { class: IntrinsicArithOpClass::Float, op }
    }
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
    ArithBinop(IntrinsicArithOpKind),
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
    BakeStaticValue,
    GetStaticValue,
    StaticTypeToValue,
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
            IntrinsicOperation::StaticTypeToValue => true,
            //
            IntrinsicOperation::Zeroed => false,
            IntrinsicOperation::BoolNegate => false,
            IntrinsicOperation::BitNot => false,
            IntrinsicOperation::ArithBinop(_) => false,
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
            IntrinsicOperation::ArithBinop(_) => true,
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
            IntrinsicOperation::GetStaticValue => true,
            IntrinsicOperation::BakeStaticValue => true,
            IntrinsicOperation::StaticTypeToValue => true,
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
            $crate::typer::make_error(&s, $span)
        }
    };
}

#[macro_export]
macro_rules! ice_span {
    ($k1:expr, $span:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            $k1.ice_with_span(&s, $span)
        }
    };
}

#[macro_export]
macro_rules! failf {
    ($span:expr, $($format_args:expr),* $(,)?) => {
        {
            let s: String = format!($($format_args),*);
            $crate::typer::make_fail_span(&s, $span)
        }
    };
}

#[macro_export]
macro_rules! format_ident {
    ($self: ident, $($format_args:expr),* $(,)?) => {
        {
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
    TypeParamConstraint,
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

    pub fn is_type_param_constraint(&self) -> bool {
        matches!(self, AbilityImplKind::TypeParamConstraint)
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
    pub functions: MSlice<AbilityImplFunction, TypedProgram>,
    pub scope_id: ScopeId,
    pub span: SpanId,
    /// I need this so that I don't try to instantiate blanket implementations that fail
    /// typechecking
    pub compile_errors: Vec<TyperError>,
}

impl TypedAbilityImpl {
    pub fn function_at_index(
        &self,
        mem: &kmem::Mem<TypedProgram>,
        index: u32,
    ) -> &AbilityImplFunction {
        mem.get_nth(self.functions, index as usize)
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
    pub is_default: bool,
}

// Passed to compile_function_declaration to inform
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
        is_default: bool,
    ) -> Self {
        FunctionAbilityContextInfo {
            ability_id,
            impl_info: Some(FunctionAbilityImplContextInfo {
                self_type_id,
                impl_kind,
                blanket_parent_function,
                is_default,
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
    lexer_tokens: Vec<lex::Token>,
    /// For Pattern matching trials
    trial_ctors: Vec<PatternCtorTrialEntry>,
    field_ctors: Vec<Vec<(Ident, PatternCtorId)>>,
    int_parse: String,
}

nz_u32_id!(ModuleId);

pub const MODULE_ID_CORE: ModuleId = ModuleId::ONE;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModuleKind {
    Library,
    Executable,
    Script,
}

pub enum ModuleRef {
    Github { url: String, branch: String },
    Local { path: PathBuf },
}

#[derive(Clone, Copy)]
pub enum LibRefLinkType {
    /// Will result in a normal linker flag passed to search by just logical name
    Default,
    /// Will result in an explicit filepath passed to linker at module's libs/
    Static,
    /// Will result in an explicit filepath passed to linker at module's libs/
    Dynamic,
}

#[derive(Clone, Copy)]
pub struct LibRef {
    pub name: StringId,
    pub link_type: LibRefLinkType,
}

pub struct ModuleManifest {
    pub kind: ModuleKind,
    pub deps: Vec<ModuleRef>,
    pub multithreading: bool,
    pub libs: Vec<LibRef>,
}

pub struct Module {
    pub id: ModuleId,
    pub name: Ident,
    pub home_dir: PathBuf,
    pub manifest: ModuleManifest,
    pub namespace_id: NamespaceId,
    pub namespace_scope_id: ScopeId,
}

#[derive(Clone, Copy)]
pub struct ProgramSettings {
    pub multithreaded: bool,
    pub executable: bool,
}

pub struct MemTmp;

pub struct TypedExprPool {
    // SoA pools
    pub exprs: VPool<TypedExpr, TypedExprId>,
    pub type_ids: VPool<TypeId, TypedExprId>,
    pub spans: VPool<SpanId, TypedExprId>,
}

impl TypedExprPool {
    pub fn make_with_hint(hint: usize) -> Self {
        TypedExprPool {
            exprs: VPool::make_with_hint("typed_exprs", hint),
            type_ids: VPool::make_with_hint("typed_expr_type_ids", hint),
            spans: VPool::make_with_hint("typed_expr_spans", hint),
        }
    }

    pub fn len(&self) -> usize {
        self.exprs.len()
    }

    pub fn add_return(&mut self, return_value: TypedExprId, span: SpanId) -> TypedExprId {
        self.add(TypedExpr::Return(TypedReturn { value: return_value }), NEVER_TYPE_ID, span)
    }

    pub fn add_block(
        &mut self,
        mem: &mut Mem<TypedProgram>,
        builder: BlockBuilder,
        type_id: TypeId,
    ) -> TypedExprId {
        self.add(
            TypedExpr::Block(TypedBlock {
                scope_id: builder.scope_id,
                statements: mem.vec_to_mslice(&builder.statements),
            }),
            type_id,
            builder.span,
        )
    }

    pub fn add_static(
        &mut self,
        value_id: StaticValueId,
        type_id: TypeId,
        is_typed_as_static: bool,
        span: SpanId,
    ) -> TypedExprId {
        self.add(
            TypedExpr::StaticValue(StaticConstantExpr { value_id, is_typed_as_static }),
            type_id,
            span,
        )
    }

    pub fn add(&mut self, expr: TypedExpr, type_id: TypeId, span: SpanId) -> TypedExprId {
        let id = self.exprs.next_id();
        let id0 = self.type_ids.add(type_id);
        let id1 = self.spans.add(span);
        let id2 = self.exprs.add(expr);
        debug_assert_eq!(id, id0);
        debug_assert_eq!(id, id1);
        debug_assert_eq!(id, id2);
        id
    }

    pub fn get(&self, id: TypedExprId) -> &TypedExpr {
        self.exprs.get(id)
    }

    pub fn set_full(&mut self, id: TypedExprId, expr: TypedExpr, type_id: TypeId, span: SpanId) {
        *self.exprs.get_mut(id) = expr;
        *self.type_ids.get_mut(id) = type_id;
        *self.spans.get_mut(id) = span;
    }

    pub fn get_mut(&mut self, id: TypedExprId) -> &mut TypedExpr {
        self.exprs.get_mut(id)
    }

    pub fn get_type(&self, id: TypedExprId) -> TypeId {
        *self.type_ids.get(id)
    }

    pub fn get_span(&self, id: TypedExprId) -> SpanId {
        *self.spans.get(id)
    }
}

pub struct TypedProgram {
    pub modules: VPool<Module, ModuleId>,
    pub program_settings: ProgramSettings,
    pub ast: ParsedProgram,

    pub functions: VPool<TypedFunction, FunctionId>,

    pub variables: VPool<Variable, VariableId>,
    pub types: TypePool,
    pub globals: VPool<TypedGlobal, TypedGlobalId>,
    pub exprs: TypedExprPool,
    pub calls: VPool<Call, CallId>,
    pub stmts: VPool<TypedStmt, TypedStmtId>,
    pub static_values: StaticValuePool,
    pub type_schemas: FxHashMap<TypeId, StaticValueId>,
    pub type_names: FxHashMap<TypeId, StaticValueId>,
    pub scopes: Scopes,
    pub errors: Vec<TyperError>,
    pub warnings: Vec<TyperError>,
    pub namespaces: Namespaces,
    pub abilities: VPool<TypedAbility, AbilityId>,
    pub ability_impls: VPool<TypedAbilityImpl, AbilityImplId>,
    /// Key is 'self' type
    pub ability_impl_table: FxHashMap<TypeId, Vec<AbilityImplHandle>>,
    /// Key is base ability id; the order per base is important; we want earlier
    /// blanket impls to be more specific, and to be tried first
    pub blanket_impls: FxHashMap<AbilityId, EcoVec<AbilityImplId>>,
    pub function_name_to_ability: FxHashMap<Ident, EcoVec<AbilityId>>,
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

    // Status and phases
    module_in_progress: Option<ModuleId>,
    phase: u32,

    inference_context_stack: Vec<InferenceContext>,
    inference_context_extras: Vec<InferenceContext>,
    type_defn_stack: Vec<TypeId>,

    // Buffers that we prefer to re-use to avoid thousands of allocations
    // Clear them after you use them, but leave the memory allocated
    buffers: TypedModuleBuffers,

    pub named_types: VPool<NameAndType, NameAndTypeId>,
    pub function_type_params: VPool<FunctionTypeParam, FunctionTypeParamId>,

    pub patterns: TypedPatternPool,
    pub pattern_ctors: VPool<PatternCtor, PatternCtorId>,

    // `vm`: Can execute code statically; primary VM; gets 'rented out'
    // from the TypedProgram to avoid borrow bullshit
    pub vm: Box<Option<vm::Vm>>,

    // Used to execute static code if it is first encountered
    // while executing the surrounding code statically
    // It should be run in its own environment; as it should
    // not see any of the values from its calling environment, just
    // like how comptime code can't see runtime values. Each level
    // of static execution has the same relationship with its outer caller
    pub vm_alts: Vec<vm::Vm>,

    // For every static value, once evaluated, we store its runtime representation
    // here; the data lives in vm_static_stack
    pub vm_static_stack: vm::Stack,
    pub vm_global_constant_lookups: FxHashMap<TypedGlobalId, vm::Value>,
    pub vm_static_value_lookups: FxHashMap<StaticValueId, vm::Value>,
    pub vm_process_dlopen_handle: *mut std::ffi::c_void,
    pub vm_library_dlopens: FxHashMap<Ident, *mut std::ffi::c_void>,
    pub vm_ffi_functions: FxHashMap<FunctionId, libffi::raw::ffi_cif>,

    /// Perm arena space
    pub mem: kmem::Mem<TypedProgram>,
    /// tmp arena space
    pub tmp: kmem::Mem<MemTmp>,

    pub bytecode: bc::ProgramBytecode,

    pub timing: Timing,

    pub global_id_k1_arena: Option<TypedGlobalId>,
}

pub struct Timing {
    pub clock: quanta::Clock,
    pub total_infers: usize,
    pub total_infer_nanos: u64,
    pub total_vm_nanos: u64,
    pub total_bytecode_nanos: u64,
}

impl Timing {
    pub fn time_raw(&self) -> u64 {
        self.clock.raw()
    }

    pub fn elapsed_nanos(&self, since: u64) -> u64 {
        self.clock.delta_as_nanos(since, self.clock.raw())
    }

    pub fn elapsed_ms(&self, since: u64) -> u64 {
        self.elapsed_nanos(since) / 1_000_000
    }
}

impl TypedProgram {
    pub fn new(program_name: String, config: CompilerConfig) -> TypedProgram {
        let types = TypePool::empty();

        let ast = ParsedProgram::make(program_name, config);
        let root_ident = ast.idents.b.root_module_name;
        let mut scopes = Scopes::make(root_ident, 8192);
        let mut namespaces = Namespaces { namespaces: VPool::make_with_hint("namespaces", 1024) };
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
        let mut pattern_ctors = VPool::make_with_hint("pattern_ctors", 8192);
        pattern_ctors.add(PatternCtor::Unit);
        pattern_ctors.add(PatternCtor::BoolFalse);
        pattern_ctors.add(PatternCtor::BoolTrue);
        pattern_ctors.add(PatternCtor::Char);
        pattern_ctors.add(PatternCtor::String);
        pattern_ctors.add(PatternCtor::Int);
        pattern_ctors.add(PatternCtor::Float);
        pattern_ctors.add(PatternCtor::Pointer);
        pattern_ctors.add(PatternCtor::TypeVariable);
        pattern_ctors.add(PatternCtor::FunctionPointer);

        debug!("clock init");
        let init_start = std::time::Instant::now();
        let clock =
            if cfg!(feature = "profile") { quanta::Clock::new() } else { quanta::Clock::mock().0 };
        debug!("clock calibration done in {}ms", init_start.elapsed().as_millis());

        let mut vm_static_stack = vm::Stack::make();
        let addr = vm_static_stack.push_t(true as u8);
        let mut vm_global_constant_lookups = FxHashMap::new();
        vm_global_constant_lookups.insert(GLOBAL_ID_IS_STATIC, vm::Value::ptr(addr));
        let process_dlopen_handle =
            unsafe { libc::dlopen(core::ptr::null(), libc::RTLD_LAZY | libc::RTLD_NOLOAD) };
        if process_dlopen_handle.is_null() {
            panic!("Failed to get process dlopen handle");
        }

        TypedProgram {
            modules: VPool::make_with_hint("modules", 32),
            program_settings: ProgramSettings { multithreaded: false, executable: false },
            functions: VPool::make_with_hint("typed_functions", 8192),
            variables: VPool::make_with_hint("typed_variables", 8192),
            types,
            globals: VPool::make_with_hint("typed_globals", 4096),
            exprs: TypedExprPool::make_with_hint(65536),
            calls: VPool::make_with_hint("typed_calls", 32768),
            stmts: VPool::make_with_hint("typed_stmts", 8192 << 1),
            static_values: StaticValuePool::make_with_hint(8192),
            type_schemas: FxHashMap::new(),
            type_names: FxHashMap::new(),
            scopes,
            errors: vec![],
            warnings: vec![],
            namespaces,
            abilities: VPool::make_with_hint("abilities", 2048),
            ability_impls: VPool::make_with_hint("ability_impls", 4096),
            ability_impl_table: FxHashMap::new(),
            blanket_impls: FxHashMap::new(),
            function_name_to_ability: FxHashMap::with_capacity(1024),
            namespace_ast_mappings: FxHashMap::with_capacity(512),
            function_ast_mappings: FxHashMap::with_capacity(512),
            global_ast_mappings: FxHashMap::new(),
            ability_impl_ast_mappings: FxHashMap::new(),
            use_statuses: FxHashMap::new(),
            debug_level_stack: vec![log::max_level()],
            functions_pending_body_specialization: vec![],
            ast,
            module_in_progress: None,
            phase: 0,
            inference_context_stack: Vec::with_capacity(8),
            inference_context_extras: (0..8).map(|_| InferenceContext::make()).collect(),
            type_defn_stack: Vec::with_capacity(8),
            buffers: TypedModuleBuffers {
                name_builder: String::with_capacity(4096),
                emitted_code: String::with_capacity(8192),
                lexer_tokens: Vec::with_capacity(16384),
                trial_ctors: Vec::with_capacity(1024),
                field_ctors: (0..128).map(|_| Vec::with_capacity(128)).collect::<Vec<_>>(),
                int_parse: String::with_capacity(128),
            },
            named_types: VPool::make_with_hint("named_types", 32768),
            function_type_params: VPool::make_with_hint("function_type_params", 8192),
            patterns: TypedPatternPool::make(),
            pattern_ctors,
            vm: Box::new(Some(vm::Vm::make())),
            vm_alts: vec![
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
            ],
            vm_static_stack,
            vm_global_constant_lookups,
            vm_static_value_lookups: FxHashMap::default(),
            vm_process_dlopen_handle: process_dlopen_handle,
            vm_library_dlopens: FxHashMap::with_capacity(32),
            vm_ffi_functions: FxHashMap::with_capacity(64),

            mem: kmem::Mem::make(),
            tmp: kmem::Mem::make(),

            bytecode: bc::ProgramBytecode::make(32768),

            timing: Timing {
                clock,
                total_infers: 0,
                total_infer_nanos: 0,
                total_vm_nanos: 0,
                total_bytecode_nanos: 0,
            },
            global_id_k1_arena: None,
        }
    }

    pub fn add_module(
        &mut self,
        src_path: &Path,
        primary_module: bool,
    ) -> anyhow::Result<ModuleId> {
        eprintln!("Loading module {:?}...", src_path);
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

        let (module_dir, mut files_to_compile) = crate::compiler::discover_source_files(&src_path);
        if is_core {
            let builtin_index = files_to_compile
                .iter()
                .position(|path| path.file_name().unwrap() == "builtin.k1")
                .unwrap();
            files_to_compile.swap(0, builtin_index);
        };
        let directory_string = module_dir.to_str().unwrap().to_string();
        let parse_start = self.timing.time_raw();

        let parsed_namespace_id = parse::init_module(module_name, &mut self.ast);
        let mut token_buffer = std::mem::take(&mut self.buffers.lexer_tokens);
        eprintln!("Parsing {} discovered files for module {src_path_name}", files_to_compile.len());
        for path in &files_to_compile {
            let content = std::fs::read_to_string(path)
                .unwrap_or_else(|_| panic!("Failed to open file to parse: {:?}", path));
            let name = path.file_name().unwrap();
            let file_id = self.ast.sources.next_file_id();
            let source = parse::Source::make(
                file_id,
                directory_string.clone(),
                name.to_str().unwrap().to_string(),
                content,
            );
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
            token_buffer.clear();
        }
        self.buffers.lexer_tokens = token_buffer;

        let parse_elapsed_us = self.timing.elapsed_nanos(parse_start) / 1_000;
        let parse_elapsed_ms = parse_elapsed_us / 1_000;
        let lines: usize = self.ast.sources.iter().map(|s| s.1.lines.len()).sum();
        eprintln!(
            "parsing took {}ms. {}us/line incl. io",
            parse_elapsed_ms,
            parse_elapsed_us / lines as u64
        );

        if !self.ast.errors.is_empty() {
            bail!("Parsing module {} failed with {} errors", src_path_name, self.ast.errors.len());
        }

        let module_manifest = if is_core {
            ModuleManifest {
                kind: ModuleKind::Library,
                deps: vec![],
                multithreading: false,
                // TODO: Programmer just specifies the path; I didn't like the 'libs/' thing anyway
                //       as it feels inflexible. If extension included, it'll be static. If no
                //       slashes, libs/, if slashes
                libs: vec![LibRef {
                    name: self.ast.strings.intern("k1rt"),
                    link_type: LibRefLinkType::Static,
                }],
            }
        } else {
            let manifest_result = self.get_module_manifest(parsed_namespace_id);
            if let Err(e) = manifest_result {
                self.report_error(e);
                bail!("Failed to compile module manifest")
            }
            match manifest_result.unwrap() {
                None => ModuleManifest {
                    kind: ModuleKind::Executable,
                    deps: vec![],
                    multithreading: false,
                    libs: vec![],
                },
                Some(manifest) => manifest,
            }
        };

        if module_manifest.kind == ModuleKind::Executable {
            if let Some(m) = self.modules.iter().find(|m| m.manifest.kind == ModuleKind::Executable)
            {
                bail!(
                    "Cannot compile a program with 2 executable modules. {} and {}",
                    self.ident_str(m.name),
                    self.ident_str(module_name)
                );
            }
        }

        // We take various program-wide settings from the primary module's manifest
        // So that we can compile dependencies with them applied
        if primary_module {
            self.program_settings.multithreaded = module_manifest.multithreading;
            self.program_settings.executable = module_manifest.kind == ModuleKind::Executable;
        }

        for dep in module_manifest.deps.iter() {
            let src_path = match dep {
                ModuleRef::Github { .. } => todo!(),
                ModuleRef::Local { path } => path,
            };
            // Actually, module names need to be unique identifiers and provided up front
            self.add_module(src_path, false)?;
        }

        let type_start = self.timing.clock.raw();
        let module_id =
            self.run_on_module(module_name, parsed_namespace_id, module_manifest, module_dir)?;
        if is_core {
            debug_assert_eq!(module_id, MODULE_ID_CORE);
        }
        let typing_elapsed_ms =
            self.timing.clock.delta_as_nanos(type_start, self.timing.clock.raw()) / 1_000_000;
        // if self.ast.config.profile {
        // }
        // self.named_types.print_size_info();
        // self.types.types.print_size_info();
        eprintln!("\ttyping took {}ms", typing_elapsed_ms,);

        #[cfg(feature = "profile")]
        {
            let mut exprs_by_kind = FxHashMap::new();
            for expr in self.exprs.exprs.iter() {
                let i = exprs_by_kind.entry(expr.kind_str()).or_insert(0);
                *i += 1
            }
            eprintln!("\tExpression kinds:");
            let exprs_by_kind_sorted =
                exprs_by_kind.iter().sorted_by_key(|i| -i.1).collect::<Vec<_>>();
            for (k, v) in exprs_by_kind_sorted.iter() {
                eprintln!("\t\t{}: {}", k, v);
            }
        }

        Ok(module_id)
    }

    /// Retrieve the current inference context
    fn ictx(&self) -> &InferenceContext {
        self.inference_context_stack.last().as_ref().unwrap()
    }

    /// Retrieve the current inference context
    fn ictx_mut(&mut self) -> &mut InferenceContext {
        self.inference_context_stack.last_mut().unwrap()
    }

    fn ictx_push(&mut self) {
        debug!("pushing from {} extras", self.inference_context_extras.len());
        let c = match self.inference_context_extras.pop() {
            None => InferenceContext::make(),
            Some(c) => c,
        };
        self.inference_context_stack.push(c);
    }

    fn ictx_pop(&mut self) {
        debug!("popping off from {}", self.inference_context_stack.len());
        let mut c = self.inference_context_stack.pop().unwrap();
        c.reset();
        self.inference_context_extras.push(c);
    }

    /// Used to temporarily take and avoid a long mut self borrow
    fn ictx_take(&mut self) -> InferenceContext {
        self.inference_context_stack.pop().unwrap()
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

    pub fn get_function_span(&self, function_id: FunctionId) -> SpanId {
        self.ast.get_span_for_id(self.functions.get(function_id).parsed_id)
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
        if let Some(exec_module) =
            self.modules.iter().find(|m| m.manifest.kind == ModuleKind::Executable)
        {
            self.scopes
                .get_scope(exec_module.namespace_scope_id)
                .find_function(self.ast.idents.b.main)
        } else {
            None
        }
    }

    fn push_block_stmt_id(&self, block: &mut BlockBuilder, stmt: TypedStmtId) {
        block.statements.push(stmt);
    }

    fn push_block_stmt(&mut self, block: &mut BlockBuilder, stmt: TypedStmt) {
        let id = self.stmts.add(stmt);
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
                if self.exprs.get_type(assgn.value) == NEVER_TYPE_ID {
                    NEVER_TYPE_ID
                } else {
                    UNIT_TYPE_ID
                }
            }
            TypedStmt::Require(_req) => UNIT_TYPE_ID,
            TypedStmt::Defer(_defer) => UNIT_TYPE_ID,
        }
    }

    pub fn get_stmt_span(&self, stmt: TypedStmtId) -> SpanId {
        match self.stmts.get(stmt) {
            TypedStmt::Expr(e, _ty) => self.exprs.get_span(*e),
            TypedStmt::Let(val_def) => val_def.span,
            TypedStmt::Assignment(assgn) => assgn.span,
            TypedStmt::Require(req) => req.span,
            TypedStmt::Defer(defer) => defer.span,
        }
    }

    fn add_expr_stmt(&mut self, expr: TypedExprId) -> TypedStmtId {
        let type_id = self.exprs.get_type(expr);
        self.stmts.add(TypedStmt::Expr(expr, type_id))
    }

    fn push_expr_id_to_block(&mut self, block: &mut BlockBuilder, expr: TypedExprId) {
        let ty = self.exprs.get_type(expr);
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
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &type_param.constraints,
                ) {
                    Ok(Some(parsed_static_constraint)) => {
                        Some(self.eval_type_expr(parsed_static_constraint, defn_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => return failf!(type_param.span, "{}", msg),
                };
            let mut ability_constraints = smallvec![];
            for parsed_constraint in type_param.constraints.iter() {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig =
                            self.eval_ability_expr(*ability_expr, false, defn_scope_id)?;
                        ability_constraints.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Static(_) => {}
                };
            }
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: type_param.name,
                    static_constraint: maybe_static_constraint,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                },
                ability_constraints,
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
        let type_params_handle = self.named_types.add_slice_copy(&type_params);

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
                    "Non-alias type definition must be a struct or enum or builtin; not a {}. Perhaps you meant to create an alias `deftype alias <name> = <type>`",
                    self.type_id_to_string(rhs_type_id)
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
            let gen_type = GenericType { params: type_params_handle, inner: rhs_type_id };
            self.types.resolve_unresolved(my_type_id, Type::Generic(gen_type), None);
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
                    I8_TYPE_ID => Type::Integer(IntegerType::I8),
                    I16_TYPE_ID => Type::Integer(IntegerType::I16),
                    I32_TYPE_ID => Type::Integer(IntegerType::I32),
                    I64_TYPE_ID => Type::Integer(IntegerType::I64),
                    _ => return failf!(*span, "Unknown builtin type id '{}'", defn_type_id),
                };
                self.types.resolve_unresolved(defn_type_id, type_value, None);
                Ok(defn_type_id)
            }
            ParsedTypeExpr::Struct(struct_defn) => {
                let struct_defn = struct_defn.clone();
                let mut fields: MList<StructTypeField, TypePool> =
                    self.types.mem.new_list(struct_defn.fields.len() as u32);
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
                    fields.push(StructTypeField { name: ast_field.name, type_id: ty })
                }

                let defn_info =
                    context.direct_unresolved_target_type.and_then(|t| self.types.get_defn_info(t));
                let struct_defn =
                    Type::Struct(StructType { fields: self.types.mem.vec_to_mslice(&fields) });
                let type_id = self.add_or_resolve_type(
                    context.direct_unresolved_target_type,
                    struct_defn,
                    defn_info,
                    None,
                );
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
                let type_args = self.types.mem.pushn(&[inner_ty]);
                let optional_type = self.instantiate_generic_type(OPTIONAL_TYPE_ID, type_args);
                Ok(optional_type)
            }
            ParsedTypeExpr::Reference(r) => {
                let span = r.span;
                let mutable = match r.kind {
                    parse::ReferenceKind::Read => false,
                    parse::ReferenceKind::Mut => true,
                };
                let inner_ty = self.eval_type_expr_ext(r.base, scope_id, context.descended())?;
                match self.types.get(inner_ty) {
                    Type::Function(_) => {
                        if mutable {
                            return failf!(span, "Function pointers cannot be *write");
                        }
                        let type_id = self.types.add_function_pointer_type(inner_ty);
                        Ok(type_id)
                    }
                    _ => {
                        let reference_type =
                            Type::Reference(ReferenceType { inner_type: inner_ty, mutable });
                        let type_id = self.types.add_anon(reference_type);
                        Ok(type_id)
                    }
                }
            }
            ParsedTypeExpr::Array(arr) => {
                let arr = *arr;
                let element_type =
                    self.eval_type_expr_ext(arr.element_type, scope_id, context.descended())?;

                let size_type_id = match self.ast.type_exprs.get(arr.size_expr).clone() {
                    ParsedTypeExpr::StaticLiteral(parsed_literal) => {
                        // For array sizes, we want numeric literals to be interpreted as size (i64)
                        let (static_value_id, inner_type_id) = self
                            .literal_to_static_value_and_type(
                                &parsed_literal,
                                scope_id,
                                Some(I64_TYPE_ID),
                            )?;
                        let static_type =
                            StaticType { inner_type_id, value_id: Some(static_value_id) };
                        self.types.add_anon(Type::Static(static_type))
                    }
                    _ => {
                        // For other expressions, evaluate normally
                        self.eval_type_expr_ext(arr.size_expr, scope_id, context.descended())?
                    }
                };

                let Some(static_type) = self.types.get_static_type_of_type(size_type_id) else {
                    return failf!(arr.span, "Array size must be a static type");
                };
                if static_type.inner_type_id != I64_TYPE_ID {
                    return failf!(
                        arr.span,
                        "Array size must be an int; got {}",
                        self.type_id_to_string(static_type.inner_type_id)
                    );
                }
                let concrete_size = self.get_concrete_count_of_array(size_type_id);

                let array_type = Type::Array(ArrayType {
                    element_type,
                    size_type: size_type_id,
                    concrete_count: concrete_size,
                });
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
                let defn_info =
                    context.direct_unresolved_target_type.and_then(|t| self.types.get_defn_info(t));
                let enum_type =
                    Type::Enum(TypedEnum { variants, ast_node: type_expr_id.into(), tag_type });
                let enum_type_id = self.add_or_resolve_type(
                    context.direct_unresolved_target_type,
                    enum_type,
                    defn_info,
                    None,
                );
                Ok(enum_type_id)
            }
            ParsedTypeExpr::DotMemberAccess(dot_acc) => {
                let dot_acc = dot_acc.clone();
                let base_type =
                    self.eval_type_expr_ext(dot_acc.base, scope_id, context.descended())?;
                if let Some(spec_info) = self.types.get_instance_info(base_type) {
                    let generic = self.types.get(spec_info.generic_parent).expect_generic();
                    if let Some(matching_type_var_pos) = self
                        .named_types
                        .get_slice(generic.params)
                        .iter()
                        .position(|tp| tp.name == dot_acc.member_name)
                    {
                        let actual_type =
                            self.types.mem.get_nth(spec_info.type_args, matching_type_var_pos);
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
                        let Some(field) = s.find_field(&self.types.mem, dot_acc.member_name) else {
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
                                if let Some(param) = self
                                    .types
                                    .mem
                                    .getn(fun.logical_params())
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
                let mut params: MList<FnParamType, _> =
                    self.types.mem.new_list(fun_type.params.len() as u32);

                for (index, param) in fun_type.params.iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;
                    let span = self.ast.get_type_expr_span(*param);

                    let name = match index {
                        0 => self.ast.idents.b.param_0,
                        1 => self.ast.idents.b.param_1,
                        2 => self.ast.idents.b.param_2,
                        3 => self.ast.idents.b.param_3,
                        4 => self.ast.idents.b.param_4,
                        5 => self.ast.idents.b.param_5,
                        6 => self.ast.idents.b.param_6,
                        7 => self.ast.idents.b.param_7,
                        8 => self.ast.idents.b.param_8,
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
                let params_handle = self.types.mem.vec_to_mslice(&params);
                let function_type_id = self.types.add_anon(Type::Function(FunctionType {
                    physical_params: params_handle,
                    is_lambda: false,
                    return_type,
                }));
                Ok(function_type_id)
            }
            ParsedTypeExpr::TypeOf(tof) => {
                let expr = self.eval_expr(tof.target_expr, EvalExprContext::make(scope_id))?;
                let ty = self.exprs.get_type(expr);
                Ok(ty)
            }
            ParsedTypeExpr::TypeFromId(type_from_id) => {
                let type_from_id = *type_from_id;
                let id_expr_id = self.eval_expr(
                    type_from_id.id_expr,
                    EvalExprContext::make(scope_id).with_expected_type(Some(U64_TYPE_ID)),
                )?;
                let TypedExpr::StaticValue(static_const) = self.exprs.get(id_expr_id) else {
                    self.ice_with_span(
                        "Expected an integer expression for TypeFromId",
                        type_from_id.span,
                    );
                };
                let StaticValue::Int(TypedIntValue::U64(u64_value)) =
                    self.static_values.get(static_const.value_id)
                else {
                    self.ice_with_span("Expected a u64 value for TypeFromId", type_from_id.span)
                };
                let Some(type_id) = TypeId::from_u32(*u64_value as u32) else {
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
            ParsedTypeExpr::StaticLiteral(parsed_literal) => {
                let parsed_literal = *parsed_literal;
                let (static_value_id, inner_type_id) =
                    self.literal_to_static_value_and_type(&parsed_literal, scope_id, None)?;
                let static_type_id =
                    self.types.add_static_type(inner_type_id, Some(static_value_id));
                Ok(static_type_id)
            }
        }?;
        Ok(base)
    }

    fn literal_to_static_value_and_type(
        &mut self,
        parsed_literal: &ParsedLiteral,
        scope_id: ScopeId,
        expected_type_hint: Option<TypeId>,
    ) -> TyperResult<(StaticValueId, TypeId)> {
        match parsed_literal {
            ParsedLiteral::Unit(_) => Ok((self.static_values.add(StaticValue::Unit), UNIT_TYPE_ID)),
            ParsedLiteral::Char(byte, _) => {
                Ok((self.static_values.add(StaticValue::Char(*byte)), CHAR_TYPE_ID))
            }
            ParsedLiteral::Bool(b, _) => {
                Ok((self.static_values.add(StaticValue::Bool(*b)), BOOL_TYPE_ID))
            }
            ParsedLiteral::String(s, _) => {
                Ok((self.static_values.add(StaticValue::String(*s)), STRING_TYPE_ID))
            }
            ParsedLiteral::Numeric(numeric) => {
                // Parse the numeric literal and determine its type and value
                // Use the expected type hint if provided (e.g., i64 for array sizes)
                let eval_context =
                    EvalExprContext::make(scope_id).with_expected_type(expected_type_hint);
                let num_static_value_id = self.eval_numeric_value(numeric.span, eval_context)?;
                Ok((num_static_value_id, self.static_values.get(num_static_value_id).get_type()))
            }
        }
    }

    fn add_or_resolve_type(
        &mut self,
        defn_type_id: Option<TypeId>,
        type_value: Type,
        defn_info: Option<TypeDefnInfo>,
        instance_info: Option<GenericInstanceInfo>,
    ) -> TypeId {
        match defn_type_id {
            None => self.types.add(type_value, defn_info, instance_info),
            Some(t) => {
                self.types.resolve_unresolved(t, type_value, instance_info);
                t
            }
        }
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
        if !ty_app.name.path.is_empty() {
            return Ok(None);
        }
        let ty_app = ty_app.clone();
        match self.ident_str(ty_app.name.name) {
            "dyn" => {
                if ty_app.args.len() != 1 {
                    return failf!(ty_app.span, "Expected 1 type parameter for dyn");
                }
                let Some(fn_type_expr_id) = self.ast.p_type_args.get_nth(ty_app.args, 0).type_expr
                else {
                    return failf!(ty_app.span, "Wildcard type `_` not accepted here");
                };
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
                let Some(arg1_expr) = args[0].type_expr else {
                    return failf!(ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    return failf!(ty_app.span, "Wildcard type `_` not accepted here");
                };
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?
                    .clone();
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| errf!(ty_app.span, "Expected struct"))?
                    .clone();

                let mut combined_fields =
                    self.types.mem.new_list(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(self.types.mem.getn(struct1.fields));
                for field in self.types.mem.getn(struct2.fields).iter() {
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

                let defn_info =
                    context.direct_unresolved_target_type.and_then(|t| self.types.get_defn_info(t));
                let new_struct = Type::Struct(StructType {
                    fields: self.types.mem.vec_to_mslice(&combined_fields),
                });
                let type_id = self.add_or_resolve_type(
                    context.direct_unresolved_target_type,
                    new_struct,
                    defn_info,
                    None,
                );

                Ok(Some(type_id))
            }
            "_struct_remove" => {
                if ty_app.args.len() != 2 {
                    return failf!(ty_app.span, "Expected 2 type parameters for _struct_remove");
                }
                let args = self.ast.p_type_args.get_slice(ty_app.args);
                let Some(arg1_expr) = args[0].type_expr else {
                    return failf!(ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    return failf!(ty_app.span, "Wildcard type `_` not accepted here");
                };
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
                let struct2_fields = self.types.mem.getn(struct2.fields);
                let new_fields = self
                    .types
                    .mem
                    .getn(struct1.fields)
                    .iter()
                    .filter(|f| !struct2_fields.iter().any(|sf| sf.name == f.name))
                    .cloned();
                let new_fields = self.types.mem.pushn_iter(new_fields);

                let defn_info =
                    context.direct_unresolved_target_type.and_then(|t| self.types.get_defn_info(t));
                let new_struct = Type::Struct(StructType { fields: new_fields });
                let type_id = self.add_or_resolve_type(
                    context.direct_unresolved_target_type,
                    new_struct,
                    defn_info,
                    None,
                );
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
        let ty_app_span = ty_app.span;
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
                            debug!(
                                "Evaluating {} inside {} because I haven't seen it",
                                self.ident_str(self.ast.get_type_defn(*parsed_type_defn_id).name),
                                self.scope_id_to_string(scope_id)
                            );
                            // Unresolved types are always named definitions
                            let defn_info = self.types.get_defn_info(type_id).unwrap();

                            let _result =
                                self.eval_type_defn(*parsed_type_defn_id, defn_info.scope)?;

                            // Just re-call this function from the top now that the type exists. (hack? idk)
                            self.eval_type_application(ty_app_id, scope_id, context)
                        }
                    }
                    Type::Generic(g) => {
                        if ty_app.args.len() != g.params.len() {
                            return failf!(
                                ty_app_span,
                                "Type {} expects {} type arguments, got {}",
                                self.qident_to_string(&ty_app.name),
                                g.params.len(),
                                ty_app.args.len()
                            );
                        }
                        let g_params = g.params;
                        let mut type_arguments = self.tmp.new_list(ty_app.args.len() as u32);
                        let mut subst_pairs: MList<TypeSubstitutionPair, MemTmp> =
                            self.tmp.new_list(ty_app.args.len() as u32);
                        for (param, parsed_arg) in self
                            .named_types
                            .copy_slice_sv4(g_params)
                            .iter()
                            .zip(self.ast.p_type_args.copy_slice_sv8(ty_app.args))
                        {
                            let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                                return failf!(
                                    parsed_arg.span,
                                    "Wildcard _ type not accepted here"
                                );
                            };
                            let arg_type_id = self.eval_type_expr_ext(
                                parsed_arg_expr,
                                scope_id,
                                context.descended(),
                            )?;
                            subst_pairs.push(spair! { param.type_id => arg_type_id });
                            type_arguments.push(arg_type_id);
                        }
                        // Repeat the loop, this time checking constraints
                        for (param, arg_type) in self
                            .named_types
                            .copy_slice_sv4(g_params)
                            .iter()
                            .zip(type_arguments.as_slice())
                        {
                            self.check_type_constraints(
                                param.name,
                                param.type_id,
                                *arg_type,
                                subst_pairs.as_slice(),
                                scope_id,
                                ty_app_span,
                            )?;
                        }

                        let type_arguments_slice = self.types.mem.pushn(&type_arguments);
                        Ok(self.instantiate_generic_type(type_id, type_arguments_slice))
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
                            self.qident_to_string(ty_app_name),
                        )
                    }
                }
            }
        }
    }

    fn instantiate_generic_type(
        &mut self,
        generic_type: TypeId,
        type_arguments: TypeIdSlice,
    ) -> TypeId {
        let gen_type = self.types.get(generic_type).expect_generic();
        match self.types.get_specialization(generic_type, type_arguments) {
            Some(existing) => {
                debug!(
                    "Using cached generic instance {} for {} args {:?}",
                    self.type_id_to_string(existing),
                    self.name_of_type(generic_type),
                    self.types
                        .mem
                        .getn(type_arguments)
                        .iter()
                        .map(|p| self.type_id_to_string(*p))
                        .collect::<Vec<_>>(),
                );
                existing
            }
            None => {
                debug_assert!(gen_type.params.len() == type_arguments.len() as usize);
                let defn_info = self.types.get_defn_info(generic_type).unwrap();
                // Note: This is where we'd check constraints on the pairs:
                // that each passed params meets the constraints of the generic param
                let substitution_pairs: SV8<TypeSubstitutionPair> = self
                    .named_types
                    .get_slice(gen_type.params)
                    .iter()
                    .zip(self.types.mem.getn(type_arguments))
                    .map(|(type_param, passed_type_arg)| TypeSubstitutionPair {
                        from: type_param.type_id,
                        to: *passed_type_arg,
                    })
                    .collect();
                let inner = gen_type.inner;

                let specialized_type = self.substitute_in_type_ext(
                    inner,
                    &substitution_pairs,
                    Some(generic_type),
                    Some(defn_info),
                );
                //For working on recursive generics
                //if specialized_type == inner {
                //    panic!("Instantiate should never no-op")
                //}
                // let is_sl = self.ident_str(defn_info.name) == "SpillList";
                if log::log_enabled!(log::Level::Debug) {
                    let inst_info =
                        self.types.get_instance_info(specialized_type).unwrap().type_args;
                    eprintln!(
                        "instantiated\n{} with params\n{} got expanded type:\n{}\n\n",
                        self.type_id_to_string_ext(inner, true),
                        self.pretty_print_type_slice(inst_info, ", "),
                        self.type_id_to_string_ext(specialized_type, true)
                    );
                }
                self.types.insert_specialization(generic_type, type_arguments, specialized_type);
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
        defn_info_to_attach: Option<TypeDefnInfo>,
        // TODO: for full recursive types support, we need breadcrumbs
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
        if let Some(spec_info) = self.types.get_instance_info(type_id) {
            // A,   B,    T
            // int, bool, char
            // Opt[T] -> Opt[char]
            let generic_parent = spec_info.generic_parent;
            let mut new_type_args = self.tmp.new_list(spec_info.type_args.len());
            for prev_arg in self.types.mem.getn_sv4(spec_info.type_args) {
                let new_type = self.substitute_in_type(prev_arg, substitution_pairs);
                new_type_args.push(new_type);
            }
            let new_type_args_slice = self.types.mem.pushn(&new_type_args);
            return self.instantiate_generic_type(generic_parent, new_type_args_slice);
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
                let new_fields_handle = self.types.mem.dupn(struc.fields);
                let new_fields = self.types.mem.getn_mut(new_fields_handle);
                let mut any_change = false;
                let original_defn_info = self.types.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
                for field in new_fields.iter_mut() {
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
                            type_args: self
                                .types
                                .mem
                                .pushn_iter(substitution_pairs.iter().map(|p| p.to)),
                        })
                        .or_else(|| self.types.get_instance_info(type_id).cloned());

                    let specialized_struct = StructType { fields: new_fields_handle };
                    self.types.add(
                        Type::Struct(specialized_struct),
                        defn_info_to_use,
                        generic_instance_info,
                    )
                } else {
                    type_id
                }
            }
            Type::Enum(e) => {
                let mut new_variants = e.variants.clone();
                let mut any_changed = false;
                let original_ast_node = e.ast_node;
                let original_defn_info = self.types.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
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
                            type_args: self
                                .types
                                .mem
                                .pushn_iter(substitution_pairs.iter().map(|p| p.to)),
                        })
                        .or_else(|| self.types.get_instance_info(type_id).cloned());
                    let new_enum = TypedEnum {
                        variants: new_variants,
                        ast_node: original_ast_node,
                        tag_type: original_explicit_tag_type,
                    };
                    let new_enum_id = self.types.add(
                        Type::Enum(new_enum),
                        defn_info_to_use,
                        generic_instance_info,
                    );
                    new_enum_id
                } else {
                    type_id
                }
            }
            Type::Reference(reference) => {
                let reference = *reference;
                let new_inner = self.substitute_in_type(reference.inner_type, substitution_pairs);
                if new_inner != reference.inner_type {
                    let specialized_reference =
                        ReferenceType { inner_type: new_inner, mutable: reference.mutable };
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
                let mut any_new = false;
                let is_lambda = fun_type.is_lambda;
                let old_return_type = fun_type.return_type;
                let old_params = fun_type.physical_params;
                let new_return_type = self.substitute_in_type(old_return_type, substitution_pairs);
                if new_return_type != old_return_type {
                    any_new = true
                };
                let mut new_params: MList<FnParamType, _> = self.tmp.new_list(old_params.len());
                for param in self.types.mem.getn(old_params) {
                    let new_param_type = self.substitute_in_type(param.type_id, substitution_pairs);
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    let new_param = FnParamType {
                        name: param.name,
                        type_id: new_param_type,
                        is_context: param.is_context,
                        is_lambda_env: param.is_lambda_env,
                        span: param.span,
                    };
                    new_params.push(new_param);
                }
                if any_new {
                    let new_params_handle = self.types.mem.pushn(new_params.as_slice());
                    let new_fun_type = FunctionType {
                        physical_params: new_params_handle,
                        return_type: new_return_type,
                        is_lambda,
                    };
                    let new_function_type_id = self.types.add_anon(Type::Function(new_fun_type));
                    new_function_type_id
                } else {
                    type_id
                }
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let new_fn_type = self.substitute_in_type(fp.function_type_id, substitution_pairs);
                if new_fn_type != fp.function_type_id {
                    self.types.add_function_pointer_type(new_fn_type)
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
            Type::Static(stat) => {
                if stat.value_id.is_some() {
                    // Can't substitute inside the type "static[string; "hello"]"
                    // But you can inside type "static[T; _]"
                    type_id
                } else {
                    let inner_type = stat.inner_type_id;
                    let new_inner_type = self.substitute_in_type(inner_type, substitution_pairs);
                    if new_inner_type == inner_type {
                        type_id
                    } else {
                        self.types.add_anon(Type::Static(StaticType {
                            inner_type_id: new_inner_type,
                            value_id: None,
                        }))
                    }
                }
            }
            Type::Unresolved(_) => {
                unreachable!("substitute_in_type is not expected to be called on Unresolved")
            }
            Type::RecursiveReference(_rr) => unreachable!(
                "substitute_in_type is not expected to be called on RecursiveReference"
            ),
            Type::Array(arr) => {
                let arr = *arr;
                let element_type = arr.element_type;
                let new_element_type = self.substitute_in_type(element_type, substitution_pairs);
                let new_size_type = self.substitute_in_type(arr.size_type, substitution_pairs);
                if new_element_type == element_type && new_size_type == arr.size_type {
                    type_id // No change needed
                } else {
                    // Create new Array type with substituted element type
                    let concrete_size = self.get_concrete_count_of_array(new_size_type);
                    let new_array_type = Type::Array(ArrayType {
                        element_type: new_element_type,
                        size_type: new_size_type,
                        concrete_count: concrete_size,
                    });
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

    /// Based on the 'size_type' of an array, determine what its concrete count is
    /// Size type can be a type parameter, in which case its 0
    /// But it can also be a known static size (i64), in which case its the value of it
    pub fn get_concrete_count_of_array(&self, size_type: TypeId) -> Option<u64> {
        match self.get_value_of_static_type(size_type) {
            Some(StaticValue::Int(TypedIntValue::I64(i))) => Some(*i as u64),
            _ => None,
        }
    }

    pub fn get_value_of_static_type(&self, type_id: TypeId) -> Option<&StaticValue> {
        match self.types.get_no_follow_static(type_id) {
            Type::Static(StaticType { value_id: Some(value_id), .. }) => {
                Some(self.static_values.get(*value_id))
            }
            _ => None,
        }
    }

    pub fn synth_static_option(
        &mut self,
        option_type_id: TypeId,
        value_id: Option<StaticValueId>,
    ) -> StaticValueId {
        let opt_enum_type = self.types.get(option_type_id).expect_enum();
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

        self.static_values.add(StaticValue::Enum(static_enum))
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
            .find(|id| self.ast.get_global(*id).name == self.ast.idents.b.MODULE_INFO)
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
                static_ctx: Some(StaticExecContext {
                    is_metaprogram: false,
                    expected_return_type: Some(type_id),
                }),
                global_defn_name: None,
                flags: EvalExprFlags::empty(),
            },
            &[],
        )?;

        let manifest_value = self.static_values.get(manifest_result);
        if let Err(msg) =
            self.check_types(type_id, manifest_value.get_type(), Scopes::ROOT_SCOPE_ID)
        {
            return failf!(manifest_global.span, "Module manifest type mismatch: {}", msg);
        }

        match manifest_value {
            StaticValue::Struct(value) => {
                let value_fields = self.static_values.mem.getn(value.fields);
                let kind = self.static_values.get(value_fields[0]).as_enum().unwrap();
                let kind = match kind.variant_index {
                    0 => ModuleKind::Library,
                    1 => ModuleKind::Executable,
                    2 => ModuleKind::Script,
                    i => panic!("Unrecognized module kind index: {}", i),
                };

                // Maybe, one day libs and deps are the same thing
                // Currently, though, no common needs
                let deps = vec![];

                let multithreading = self.static_values.get(value_fields[2]).as_boolean().unwrap();

                let libs = self.static_values.get(value_fields[3]).as_view().unwrap();
                let mut lib_refs = vec![];
                for lib in self.static_values.get_slice(libs.elements) {
                    // Grab the strings out for now. Eventually we can maybe just
                    // mmap this thing.
                    let string_id = self.static_values.get(*lib).as_string().unwrap();
                    lib_refs.push(LibRef { name: string_id, link_type: LibRefLinkType::Static })
                }

                Ok(Some(ModuleManifest { kind, deps, multithreading, libs: lib_refs }))
            }
            StaticValue::Zero(_) => Ok(Some(ModuleManifest {
                kind: ModuleKind::Library,
                deps: vec![],
                multithreading: false,
                libs: vec![],
            })),
            _ => panic!(
                "Expected module manifest to be a struct, got: {}",
                self.static_value_to_string(manifest_result)
            ),
        }
    }

    fn eval_pattern(
        &mut self,
        pat_expr: ParsedPatternId,
        target_type_id: TypeId,
        scope_id: ScopeId,
        allow_bindings: bool,
    ) -> TyperResult<TypedPatternId> {
        let parsed_pattern_expr = self.ast.patterns.get_pattern(pat_expr);
        match parsed_pattern_expr {
            ParsedPattern::Wildcard(span) => Ok(self.patterns.add(TypedPattern::Wildcard(*span))),
            ParsedPattern::Literal(literal_expr_id) => {
                match self.ast.exprs.get(*literal_expr_id).expect_literal() {
                    ParsedLiteral::Unit(span) => match self.types.get(target_type_id) {
                        Type::Unit => Ok(self.patterns.add(TypedPattern::LiteralUnit(*span))),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type unit will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    ParsedLiteral::Char(c, span) => match self.types.get(target_type_id) {
                        Type::Char => Ok(self.patterns.add(TypedPattern::LiteralChar(*c, *span))),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type char will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    ParsedLiteral::Numeric(num_lit) => {
                        let num_lit = *num_lit;
                        let num_value_id = self.eval_numeric_value(
                            num_lit.span,
                            EvalExprContext::make(scope_id)
                                .with_expected_type(Some(target_type_id)),
                        )?;
                        match self.static_values.get(num_value_id) {
                            StaticValue::Int(_) => match self.types.get(target_type_id) {
                                Type::Integer(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralInteger(num_value_id, num_lit.span))),
                                _ => failf!(
                                    self.ast.get_pattern_span(pat_expr),
                                    "integer literal pattern will never match {}",
                                    self.type_id_to_string(target_type_id)
                                ),
                            },
                            StaticValue::Float(_) => match self.types.get(target_type_id) {
                                Type::Float(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralFloat(num_value_id, num_lit.span))),
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
                    ParsedLiteral::Bool(b, span) => match self.types.get(target_type_id) {
                        Type::Bool => Ok(self.patterns.add(TypedPattern::LiteralBool(*b, *span))),
                        _ => failf!(
                            self.ast.get_pattern_span(pat_expr),
                            "bool literal pattern will never match {}",
                            self.type_id_to_string(target_type_id)
                        ),
                    },
                    ParsedLiteral::String(string_id, span) => {
                        match self.types.get_no_follow_static(target_type_id) {
                            Type::Static(s) if s.inner_type_id == STRING_TYPE_ID => Ok(()),
                            _ if target_type_id == STRING_TYPE_ID => Ok(()),
                            _ => failf!(
                                self.ast.get_pattern_span(pat_expr),
                                "string literal pattern will never match {}",
                                self.type_id_to_string(target_type_id)
                            ),
                        }?;
                        Ok(self.patterns.add(TypedPattern::LiteralString(*string_id, *span)))
                    }
                }
            }
            ParsedPattern::Variable(ident_id, span) => {
                if !allow_bindings {
                    return failf!(*span, "Bindings are not allowed here");
                }
                Ok(self.patterns.add(TypedPattern::Variable(VariablePattern {
                    name: *ident_id,
                    type_id: target_type_id,
                    span: *span,
                })))
            }
            ParsedPattern::Enum(enum_pattern) => {
                let enum_pattern_span = enum_pattern.span;
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
                            let base_type = match self.types.get_instance_info(target_type_id) {
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
                let matching_variant_index = matching_variant.index;
                let matching_variant_name = matching_variant.name;

                let payload_pattern = match &enum_pattern.payload_pattern {
                    None => None,
                    Some(payload_expr) => {
                        let payload_type_id = matching_variant.payload.ok_or_else(|| {
                            errf!(
                                enum_pattern.span,
                                "Impossible pattern: Variant '{}' has no payload",
                                self.ident_str(matching_variant.name)
                            )
                        })?;
                        let payload_pattern = self.eval_pattern(
                            *payload_expr,
                            payload_type_id,
                            scope_id,
                            allow_bindings,
                        )?;
                        Some(payload_pattern)
                    }
                };

                let enum_pattern = TypedEnumPattern {
                    enum_type_id: target_type_id,
                    variant_index: matching_variant_index,
                    variant_tag_name: matching_variant_name,
                    payload: payload_pattern,
                    span: enum_pattern_span,
                };
                Ok(self.patterns.add(TypedPattern::Enum(enum_pattern)))
            }
            ParsedPattern::Struct(struct_pattern) => {
                let target_type = self.types.get(target_type_id);
                if struct_pattern.fields.is_empty() {
                    return failf!(
                        struct_pattern.span,
                        "Useless pattern: Struct pattern has no fields; use wildcard pattern '_' instead",
                    );
                }
                let struct_pattern = struct_pattern.clone();
                let expected_struct = target_type
                    .as_struct()
                    .ok_or_else(|| {
                        errf!(
                            struct_pattern.span,
                            "Impossible pattern: Match target '{}' is not a struct",
                            self.type_id_to_string(target_type_id)
                        )
                    })?
                    .clone();
                let mut fields = self.patterns.mem.new_list(struct_pattern.fields.len() as u32);
                for (field_name, field_parsed_pattern_id) in &struct_pattern.fields {
                    let (expected_field_index, expected_field) = expected_struct
                        .find_field(&self.types.mem, *field_name)
                        .ok_or_else(|| {
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
                    fields: self.patterns.mem.vec_to_mslice(&fields),
                    span: struct_pattern.span,
                };
                Ok(self.patterns.add(TypedPattern::Struct(struct_pattern)))
            }
            ParsedPattern::Reference(reference_pattern) => {
                let Type::Reference(r) = self.types.get(target_type_id) else {
                    return failf!(
                        reference_pattern.span,
                        "Reference pattern will never match non-reference {}",
                        self.type_id_to_string(target_type_id)
                    );
                };
                let reference_pattern_span = reference_pattern.span;
                let inner_pattern = self.eval_pattern(
                    reference_pattern.inner,
                    r.inner_type,
                    scope_id,
                    allow_bindings,
                )?;
                Ok(self.patterns.add(TypedPattern::Reference(TypedReferencePattern {
                    inner_pattern,
                    span: reference_pattern_span,
                })))
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
        for (expected_field, actual_field) in self
            .types
            .mem
            .getn(expected.fields)
            .iter()
            .zip(self.types.mem.getn(actual.fields).iter())
        {
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
        let lookup_result = self.types.get_no_follow(type_id);
        match lookup_result {
            Type::TypeParameter(tvar) => match self.scopes.find_type(scope_id, tvar.name) {
                None => {
                    debug!(
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
            },
            Type::RecursiveReference(rr) => rr.root_type_id,
            _ => type_id,
        }
    }

    /// Checks the type of `expr`. Will attempt to coerce expr to fulfill
    /// the expected type in some known cases.
    /// 1. Expected: static[T, <value>], Actual: T.
    /// 2. ...
    ///
    /// Current coercion sites:
    /// - Function arguments
    /// - Variable declarations
    /// - List literal elements
    fn check_expr_type<'a>(
        &mut self,
        expected: TypeId,
        expr: TypedExprId,
        scope_id: ScopeId,
    ) -> CheckExprTypeResult<'a> {
        let actual_type_id = self.exprs.get_type(expr);

        let check_result = self.check_types(expected, actual_type_id, scope_id);
        let Err(msg) = check_result else { return CheckExprTypeResult::Ok };

        // Static lifting and erasing
        match (
            self.types.get_no_follow_static(expected),
            self.types.get_static_type_of_type(actual_type_id),
        ) {
            // If we failed typechecking, and passed a static, then see
            // whether the type inside this static would pass muster under the 'expected type',
            // If so, erase the static
            (_, Some(actual_static))
                if self.check_types(expected, actual_static.inner_type_id, scope_id).is_ok() =>
            {
                return CheckExprTypeResult::Coerce(
                    self.synth_cast(expr, actual_static.inner_type_id, CastType::StaticErase, None),
                    "static_erase".into(),
                );
            }
            // If we failed typechecking, and we expected a static, and we passed a non-static
            // Try to lift it
            (Type::Static(expected_static), None) => {
                if expected_static.inner_type_id == actual_type_id {
                    if let Ok(static_lifted) = self.attempt_static_lift(expr) {
                        let static_lifted_type = self.exprs.get_type(static_lifted);
                        return match self.check_types(expected, static_lifted_type, scope_id) {
                            Err(msg) => CheckExprTypeResult::Err(format!(
                                "Static lift resulted in wrong value: {msg}"
                            )),
                            Ok(_) => {
                                CheckExprTypeResult::Coerce(static_lifted, "static_lift".into())
                            }
                        };
                    }
                }
            }
            _ => {}
        };

        // If we expect a lambda object and you pass a lambda
        if let Type::LambdaObject(_lam_obj_type) = self.types.get(expected) {
            if let Type::Lambda(lambda_type) = self.get_expr_type(expr) {
                let span = self.exprs.get_span(expr);
                let lambda_object_type = self.types.add_lambda_object(
                    &self.ast.idents,
                    lambda_type.function_type,
                    lambda_type.parsed_id,
                );
                match self.check_types(expected, lambda_object_type, scope_id) {
                    Ok(_) => {
                        return CheckExprTypeResult::Coerce(
                            self.synth_cast(
                                expr,
                                lambda_object_type,
                                CastType::LambdaToLambdaObject,
                                Some(span),
                            ),
                            "lam->lamobj".into(),
                        );
                    }
                    Err(msg) => {
                        eprintln!("coerce: detected lam obj case failed: {msg}");
                    }
                }
            }
        }

        // If we expect a lambda object and you pass a function reference... (optimized lambda)
        if let Type::LambdaObject(_lam_obj_type) = self.types.get(expected) {
            if let TypedExpr::FunctionPointer(fun_ref) = self.exprs.get(expr) {
                let expr_span = self.exprs.get_span(expr);
                let lambda_object = self.function_to_lambda_object(fun_ref.function_id, expr_span);
                let lambda_object_type = self.exprs.get_type(lambda_object);
                if self.check_types(expected, lambda_object_type, scope_id).is_ok() {
                    return CheckExprTypeResult::Coerce(lambda_object, "funref->lamobj".into());
                }
            }
        }

        if let Type::Reference(exp_ref) = self.types.get(expected) {
            if let Type::Reference(actual_ref) = self.types.get(actual_type_id) {
                // If we expect a readonly reference but have a mutable one
                if !exp_ref.mutable && actual_ref.mutable {
                    match self.check_types(exp_ref.inner_type, actual_ref.inner_type, scope_id) {
                        Ok(_) => {
                            let readonly_reference =
                                self.types.add_reference_type(actual_ref.inner_type, false);
                            let casted_reference = self.synth_cast(
                                expr,
                                readonly_reference,
                                CastType::ReferenceToReference,
                                None,
                            );
                            return CheckExprTypeResult::Coerce(
                                casted_reference,
                                "write->read".into(),
                            );
                        }
                        Err(_) => {}
                    }
                }
            }
        }

        if let Type::Integer(expected_int) = self.types.get(expected) {
            if let Type::Integer(actual_int) = self.types.get(actual_type_id) {
                return {
                    let needs_widen = expected_int.width() > actual_int.width();
                    // Let's do auto widening when signedness doesnt change
                    // And also unsigned -> signed widening
                    if needs_widen {
                        match (expected_int.is_signed(), actual_int.is_signed()) {
                            (true, true) => {
                                let widened = self.synth_cast(
                                    expr,
                                    expected,
                                    CastType::IntegerCast(IntegerCastDirection::Extend),
                                    None,
                                );
                                CheckExprTypeResult::Coerce(widened, "widen signed".into())
                            }
                            (false, false) => {
                                let widened = self.synth_cast(
                                    expr,
                                    expected,
                                    CastType::IntegerCast(IntegerCastDirection::Extend),
                                    None,
                                );
                                CheckExprTypeResult::Coerce(widened, "widen unsigned".into())
                            }
                            (false, true) => {
                                // Could lose signedness if negative; no no
                                CheckExprTypeResult::Err("widen signed->unsigned".into())
                            }
                            (true, false) => {
                                let widened = self.synth_cast(
                                    expr,
                                    expected,
                                    CastType::IntegerCast(IntegerCastDirection::Extend),
                                    None,
                                );
                                let to_signed = self.synth_cast(
                                    widened,
                                    expected,
                                    CastType::IntegerCast(IntegerCastDirection::SignChange),
                                    None,
                                );
                                CheckExprTypeResult::Coerce(
                                    to_signed,
                                    "widen->unsigned->signed".into(),
                                )
                            }
                        }
                    } else {
                        // We never truncate automatically, or change signedness without extension
                        CheckExprTypeResult::Err("incompatible integer types".into())
                    }
                };
            }
        }

        // Auto-deref: We only do this if the expected type is not a reference at all. Meaning,
        // if your expected type is T*, and you pass a T**, you need to de-reference that yourself.
        // This rule won't help you or do anything for nested references
        let expected_resolved = self.types.get(self.get_type_id_resolved(expected, scope_id));
        // If we don't expect a reference
        if expected_resolved.as_reference().is_none() {
            // And we don't expect a function-like type parameter (function pointers don't really work like references)
            if let Type::FunctionTypeParameter(_tp) = expected_resolved {
            } else {
                // But you pass a reference
                if let Some(_reference) = self.get_expr_type(expr).as_reference() {
                    // We want this final check to benefit from coercion (which we are currrently implementing)
                    let dereferenced = self.synth_dereference(expr);
                    match self.check_expr_type(expected, dereferenced, scope_id) {
                        CheckExprTypeResult::Ok => {
                            return CheckExprTypeResult::Coerce(dereferenced, "deref".into());
                        }
                        CheckExprTypeResult::Err(_) => {}
                        CheckExprTypeResult::Coerce(typed_expr_id, reason1) => {
                            return CheckExprTypeResult::Coerce(
                                typed_expr_id,
                                format!("deref -> {reason1}").into(),
                            );
                        }
                    }
                }
            }
        };

        CheckExprTypeResult::Err(msg)
    }

    pub fn check_and_coerce_expr(
        &mut self,
        expected: TypeId,
        expr: TypedExprId,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExprId> {
        debug!(
            "check_and_coerce `{}`, expected: {}",
            self.expr_to_string(expr),
            self.type_id_to_string(expected)
        );
        match self.check_expr_type(expected, expr, scope_id) {
            CheckExprTypeResult::Err(msg) => {
                let span = self.exprs.get_span(expr);
                Err(TyperError { message: msg, span, level: ErrorLevel::Error })
            }
            CheckExprTypeResult::Coerce(new_expr, rule_kind) => {
                debug!(
                    "Coerced with rule {rule_kind} {} -> {}",
                    self.expr_to_string(expr),
                    self.expr_to_string(new_expr)
                );
                Ok(new_expr)
            }
            CheckExprTypeResult::Ok => Ok(expr),
        }
    }

    // nocommit(3): We're now generating errors as a matter of course in successful compilation
    //              due to how 'coerce' works. We need to make sure generating these error strings
    //              is performant, currently it is very much not
    //
    //              Passing in a buffer for the errors to go is 1 thing
    //              But often check_types calls 'type_id_to_string', which is an entire
    //              other can of worms to optimize!
    //
    //              Got type_id_to_string pretty well optimized; we also have a format!'ed print to
    //              arena now
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

        if let (Some(spec1), Some(spec2)) =
            (self.types.get_instance_info(expected), self.types.get_instance_info(actual))
        {
            return if spec1.generic_parent == spec2.generic_parent {
                for (index, (exp_param, act_param)) in self
                    .types
                    .mem
                    .getn_sv4(spec1.type_args)
                    .iter()
                    .zip(self.types.mem.getn_sv4(spec2.type_args).iter())
                    .enumerate()
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

        debug!(
            "typecheck resolved: {} <: {}",
            self.type_id_to_string(actual).blue(),
            self.type_id_to_string(expected).blue(),
        );

        let expected = self.types.get_static_type_id_of_type(expected).unwrap_or(expected);
        let actual = self.types.get_static_type_id_of_type(actual).unwrap_or(actual);

        debug!(
            "typecheck resolved: {} <: {}",
            self.type_id_to_string(actual).blue(),
            self.type_id_to_string(expected).blue(),
        );

        match (self.types.get_no_follow_static(expected), self.types.get_no_follow_static(actual)) {
            (Type::InferenceHole(_hole), _any) => Ok(()),
            (Type::Struct(r1), Type::Struct(r2)) => self.typecheck_struct(r1, r2, scope_id),
            (Type::Reference(exp_ref), Type::Reference(act_ref)) => {
                match self.check_types(exp_ref.inner_type, act_ref.inner_type, scope_id) {
                    e @ Err(_) => e,
                    Ok(()) => {
                        if exp_ref.mutable == act_ref.mutable {
                            Ok(())
                        } else {
                            Err(format!(
                                "References differ in mutability. expected {} but got {}",
                                if exp_ref.mutable { "write" } else { "read" },
                                if act_ref.mutable { "write" } else { "read" }
                            ))
                        }
                    }
                }
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
                    for (p1, p2) in self
                        .types
                        .mem
                        .getn(f1.logical_params())
                        .iter()
                        .zip(self.types.mem.getn(f2.logical_params()).iter())
                    {
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
            (Type::FunctionPointer(fp1), Type::FunctionPointer(fp2)) => {
                self.check_types(fp1.function_type_id, fp2.function_type_id, scope_id)
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
                            if exp_value_id == act_value_id {
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
                            self.type_id_to_string(expected),
                            self.type_id_to_string(actual),
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
            (Type::Array(expected_array), Type::Array(actual_array)) => {
                let elem_check = self.check_types(
                    expected_array.element_type,
                    actual_array.element_type,
                    scope_id,
                );
                let size_check =
                    self.check_types(expected_array.size_type, actual_array.size_type, scope_id);
                if let Err(msg) = elem_check {
                    Err(format!("Arrays have different element types: {msg}"))
                } else if let Err(msg) = size_check {
                    Err(format!("Arrays have different size types: {msg}"))
                } else {
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

    fn eval_trivial_static_expr(
        &mut self,
        expr_id: TypedExprId,
        _scope_id: ScopeId,
    ) -> TyperResult<Option<StaticValueId>> {
        match self.exprs.get(expr_id) {
            TypedExpr::StaticValue(s) => Ok(Some(s.value_id)),
            TypedExpr::Variable(v) => {
                let typed_variable = self.variables.get(v.variable_id);
                let Some(global_id) = typed_variable.global_id() else {
                    return failf!(
                        self.exprs.get_span(expr_id),
                        "Variable cannot be evaluated at compile time: {}",
                        self.ident_str(typed_variable.name)
                    );
                };
                let global = self.globals.get(global_id);
                if let Some(value) = global.initial_value { Ok(Some(value)) } else { Ok(None) }
            }
            TypedExpr::Call { call_id, .. } => {
                // If a call to zeroed(), we can use the StaticValue::Zeroed() shortcut to avoid
                // running silly code

                let call = self.calls.get(*call_id);
                if let Some(function_id) = call.callee.maybe_function_id() {
                    let function = self.functions.get(function_id);
                    if let Some(IntrinsicOperation::Zeroed) = function.intrinsic_type {
                        let return_type_id = self.exprs.get_type(expr_id);
                        let static_value_id =
                            self.static_values.add(StaticValue::Zero(return_type_id));
                        Ok(Some(static_value_id))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn compile_all_pending_bytecode(&mut self) -> TyperResult<()> {
        loop {
            // eprintln!(
            //     "compile_all_pending_bytecode {}",
            //     self.bytecode.b_units_pending_compile.len()
            // );
            // for p in &self.bytecode.b_units_pending_compile {
            //     eprintln!("PENDING: {} {}", p.as_u32(), self.function_id_to_string(*p, false));
            // }
            if let Some(function_id) = self.bytecode.b_units_pending_compile.pop() {
                self.eval_function_body(function_id)?;
                // let is_concrete = self.functions.get(function_id).is_concrete;
                // if !is_concrete {
                //     eprintln!("Someone's asking me to compile this non-concrete function")
                // }
                // if self.functions.get(function_id).body_block.is_none() {
                //     debug!(
                //         "Function with no body (after compile) made it into pending: {}",
                //         self.function_id_to_string(function_id, false)
                //     );
                //     continue;
                // }
                if let Err(e) = bc::compile_function(self, function_id) {
                    return failf!(
                        e.span,
                        "Failed to compile bytecode for function: {}",
                        e.message
                    );
                };
            } else {
                break;
            }
        }
        Ok(())
    }

    fn execute_static_expr_with_vm(
        &mut self,
        vm: &mut vm::Vm,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> TyperResult<StaticValueId> {
        if ctx.is_inference() {
            return failf!(
                self.ast.get_expr_span(parsed_expr),
                "#static cannot be used directly in generic calls. Try supplying the types to the call, or moving the static block outside the call"
            );
        }

        // Note: We need to mask access from inside a static to outside variables!
        //       Currently we'll just fail in bytecode gen with "missing variable"
        let parsed_expr_as_block =
            self.ensure_parsed_expr_to_block(parsed_expr, ParsedBlockKind::FunctionBody);
        let expr = self.eval_block(&parsed_expr_as_block, ctx, true)?;
        let expr_metadata = self.ast.exprs.get_metadata(parsed_expr);
        let is_debug = expr_metadata.is_debug;

        if let Some(shortcut_value_id) = self.eval_trivial_static_expr(expr, ctx.scope_id)? {
            return Ok(shortcut_value_id);
        }

        bc::compile_top_level_expr(self, expr, input_parameters, is_debug)?;
        self.compile_all_pending_bytecode()?;

        let execution_result = vm::execute_compiled_unit(self, vm, expr, &[], input_parameters)
            .map_err(|mut e| {
                let stack_trace = vm::make_stack_trace(self, &vm.stack);
                e.message = format!("{}\nExecution Trace\n{}", e.message, stack_trace);
                e
            });

        vm.reset(self.global_id_k1_arena);

        let static_value_id = execution_result?;

        Ok(static_value_id)
    }

    fn execute_static_expr(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> TyperResult<StaticValueId> {
        let (mut vm, used_alt) = match *std::mem::take(&mut self.vm) {
            None => {
                let span = self.ast.get_expr_span(parsed_expr);
                let maybe_alt = self.vm_alts.pop();
                let (source, location) = self.get_span_location(span);
                let alt_vm = match maybe_alt {
                    None => {
                        eprintln!(
                            "Had to make a new alt VM at {}:{}",
                            source.filename,
                            location.line_number()
                        );
                        let new_vm = vm::Vm::make();
                        new_vm
                    }
                    Some(alt_vm) => {
                        debug!(
                            "Serving up nested at {}:{}",
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
        let res = self.execute_static_expr_with_vm(&mut vm, parsed_expr, ctx, input_parameters);
        if !used_alt {
            *self.vm = Some(vm);
        } else {
            debug!("Restoring alt VM to pool");
            self.vm_alts.push(vm);
        }

        res
    }

    fn execute_static_condition(&mut self, cond: Option<ParsedExprId>, scope_id: ScopeId) -> bool {
        if let Some(condition_expr) = cond {
            match self.execute_static_bool(condition_expr, EvalExprContext::make(scope_id)) {
                Err(e) => {
                    self.report_error(e);
                    false
                }
                Ok(b) => b,
            }
        } else {
            true
        }
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
            &[],
        )?;
        let StaticValue::Bool(condition_bool) = self.static_values.get(vm_cond_result) else {
            let cond_span = self.ast.get_expr_span(cond);
            return failf!(cond_span, "Condition is not a boolean");
        };
        Ok(*condition_bool)
    }

    fn declare_global(
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
        let is_mutable = if is_referencing {
            let Some(reference_type) = self.types.get(type_id).as_reference() else {
                return failf!(global_span, "Global references must have a reference type");
            };
            reference_type.mutable
        } else {
            false
        };

        let global_id = self.globals.next_id();
        let variable_id = self.variables.add(Variable {
            name: global_name,
            type_id,
            owner_scope: scope_id,
            kind: VariableKind::Global(global_id),
            flags: VariableFlags::empty(),
            usage_count: 0,
        });
        let actual_global_id = self.globals.add(TypedGlobal {
            variable_id,
            initial_value: None,
            parsed_expr: value_expr_id,
            ty: type_id,
            span: global_span,
            is_referencing,
            is_constant: !is_mutable,
            is_tls: parsed_global.thread_local,
            ast_id: parsed_global_id,
            parent_scope: scope_id,
        });

        debug_assert_eq!(actual_global_id, global_id);

        if scope_id == self.scopes.mem_scope_id && parsed_global.name == self.ast.idents.b.arena {
            self.global_id_k1_arena = Some(global_id)
        };
        self.global_ast_mappings.insert(parsed_global_id, global_id);
        self.scopes.add_variable(scope_id, global_name, variable_id);
        Ok(variable_id)
    }

    pub fn eval_global_body(&mut self, parsed_global_id: ParsedGlobalId) -> TyperResult<()> {
        let parsed_global = self.ast.get_global(parsed_global_id).clone();
        let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id).copied() else {
            // This means we failed to compile the definition; or we have a bug!
            // TODO: Store failures so we can be certain which is true!
            debug!("skipping rest of global body");
            return Ok(());
        };
        let typed_global = self.globals.get(global_id);
        let scope_id = typed_global.parent_scope;
        let declared_type = typed_global.ty;

        let is_referencing = parsed_global.is_referencing;
        let expected_type = if is_referencing {
            let Type::Reference(r) = self.types.get(declared_type) else {
                return failf!(parsed_global.span, "Global references must have a reference type");
            };
            r.inner_type
        } else {
            declared_type
        };
        let global_name = parsed_global.name;
        let global_span = parsed_global.span;
        let value_expr_id = parsed_global.value_expr;

        let ctx = EvalExprContext {
            scope_id,
            expected_type_id: Some(expected_type),
            static_ctx: Some(StaticExecContext {
                is_metaprogram: false,
                expected_return_type: Some(expected_type),
            }),
            global_defn_name: Some(global_name),
            flags: EvalExprFlags::empty(),
        };
        let StaticExecutionResult::TypedExpr(static_expr_id) = self.eval_static_expr_and_exec(
            value_expr_id,
            ParsedStaticExpr {
                base_expr: value_expr_id,
                kind: ParsedStaticBlockKind::Value,
                is_definition: false,
                condition_if_definition: None,
                parameter_names: SliceHandle::empty(),
                span: parsed_global.span,
            },
            ctx,
        )?
        else {
            ice_span!(self, global_span, "Expected a typed expr from static execution")
        };
        let TypedExpr::StaticValue(sce) = self.exprs.get(static_expr_id) else {
            ice_span!(self, global_span, "Got a non-static expr")
        };
        let static_value_id = sce.value_id;

        match self.check_expr_type(expected_type, static_expr_id, scope_id) {
            CheckExprTypeResult::Ok => {}
            CheckExprTypeResult::Err(msg) => {
                return failf!(
                    global_span,
                    "Type mismatch for global {}: {}",
                    self.ident_str(global_name),
                    msg
                );
            }
            CheckExprTypeResult::Coerce(_, cow) => {
                panic!("Global would be coerced {cow}!")
            }
        }

        self.globals.get_mut(global_id).initial_value = Some(static_value_id);

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
        self.bytecode.functions.add(None);
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

    // TODO(perf) registers in profile, EcoVecs
    // We could put ability impl functions in a pool, done, since we make so many in generic code
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
        let _ = self.scopes.add_type(scope_id, self.ast.idents.b.Self_, type_variable_id);
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
        let impl_kind = AbilityImplKind::TypeParamConstraint;
        let functions = self.abilities.get(impl_signature.specialized_ability_id).functions.clone();
        let mut impl_functions = self.mem.new_list(functions.len() as u32);
        for f in functions.iter() {
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
                self.named_types.add_slice_copy(&type_params_minus_self)
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
            impl_functions.push(AbilityImplFunction::Abstract(specialized_signature))
        }
        self.add_ability_impl(TypedAbilityImpl {
            kind: impl_kind,
            blanket_type_params: SliceHandle::empty(),
            self_type_id: type_variable_id,
            base_ability_id,
            ability_id: impl_signature.specialized_ability_id,
            impl_arguments: impl_signature.impl_arguments,
            functions: self.mem.vec_to_mslice(&impl_functions),
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
        for ability_sig in ability_impls.into_iter() {
            let constrained_impl_scope =
                self.scopes.add_child_scope(value.scope_id, ScopeType::AbilityImpl, None, None);
            let _ = self.scopes.get_scope_mut(constrained_impl_scope).add_type(value.name, type_id);
            self.add_constrained_ability_impl(
                type_id,
                ability_sig,
                constrained_impl_scope,
                value.span,
            );
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
                    self.ability_impls.get(handle.full_impl_id).kind.is_type_param_constraint()
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
    ) -> Result<(), Cow<'_, str>> {
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

        // Follow 'statics' since we're never going to be implementing abilities for the
        // specific static values but instead for the inner types
        let self_type_id = match self.types.get_no_follow(self_type_id) {
            Type::Static(stat) => stat.inner_type_id,
            _ => self_type_id,
        };
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
        // Push an inference context so our ability inference doesn't clash with any
        // already ongoing inference. We also can end up applying blanket implementations
        // recursively, so we need an entire stack not just a double buffer situation
        // There is no need to pop this on our return because the infer_types machinery
        // will pop it when it completes. We just have to push one to ensure inference
        // runs on a clean state, and so that once popped, the old state is restored
        self.ictx_push();
        // NOT NEEDED: let mut self_ = scopeguard::guard(self, |s| self_.ictx_pop());

        let blanket_impl = self.ability_impls.get(blanket_impl_id);
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

        let blanket_ability = self.abilities.get(blanket_impl.ability_id);
        let blanket_base = blanket_ability.base_ability_id;

        if blanket_base != target_base_ability_id {
            debug!("Wrong blanket base {}", self.ident_str(blanket_ability.name));
            return None;
        }

        let blanket_arguments = blanket_ability.kind.arguments();

        debug!(
            "Trying blanket impl {} with blanket arguments {}, impl arguments {}",
            self.ident_str(blanket_ability.name),
            self.pretty_print_named_types(self.named_types.get_slice(blanket_arguments), ", "),
            self.pretty_print_named_type_slice(blanket_impl.impl_arguments, ", "),
        );

        if blanket_arguments.len() != target_ability_args.len() {
            debug!("Wrong arg count {} vs {}", blanket_arguments.len(), target_ability_args.len());
            return None;
        }

        // Reborrows
        let blanket_ability = self.abilities.get(blanket_impl_ability_id);
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
            self.named_types.get_slice(blanket_arguments).iter().zip(target_ability_args)
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
            self.ability_impls.get(blanket_impl_id).blanket_type_params;
        let root_scope_id = self.scopes.get_root_scope_id();
        let blanket_impl_type_params =
            self.named_types.copy_slice_sv8(blanket_impl_type_params_handle);
        let solutions_result = self.infer_types(
            &blanket_impl_type_params,
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
            self.zip_named_types_to_subst_pairs(blanket_impl_type_params_handle, solutions);
        for (blanket_arg, required_arg) in
            self.named_types.copy_slice_sv4(blanket_arguments).iter().zip(target_ability_args)
        {
            if let Some(required_arg) = required_arg {
                let actual_value =
                    self.substitute_in_type(blanket_arg.type_id, &solutions_as_pairs);
                if let Err(msg) = self.check_types(*required_arg, actual_value, root_scope_id) {
                    debug!(
                        "blanket impl, if applied, would result in the wrong type for param {}. {}",
                        self.ident_str(blanket_arg.name),
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
        let constraint_checking_scope = self.scopes.add_sibling_scope(
            blanket_impl_scope_id,
            ScopeType::AbilityImpl,
            None,
            None,
        );
        let parsed_blanket_impl = self.ast.get_ability_impl(parsed_id);

        for ((typed_param, parsed_param), solution) in self
            .named_types
            .copy_slice_sv4(blanket_impl_type_params_handle)
            .iter()
            .zip(parsed_blanket_impl.generic_impl_params.clone().iter())
            .zip(self.named_types.copy_slice_sv4(solutions).iter())
        {
            let _ = self.scopes.add_type(
                constraint_checking_scope,
                parsed_param.name,
                solution.type_id,
            );
            let tp = self.types.get_type_parameter(typed_param.type_id);
            if let Some(static_constraint) = tp.static_constraint {
                let static_type =
                    self.types.get_no_follow_static(static_constraint).as_static().unwrap();
                if static_type.inner_type_id != solution.type_id {
                    eprintln!(
                        "Blanket impl almost matched but a static constraint failed: {} != {}",
                        self.type_id_to_string(static_type.inner_type_id),
                        self.type_id_to_string(solution.type_id)
                    );
                    return None;
                }
            }
            for parsed_ability_expr in
                parsed_param.constraints.iter().filter_map(|p| p.as_ability())
            {
                let constraint_signature = self
                    .eval_ability_expr(parsed_ability_expr, false, constraint_checking_scope)
                    .unwrap();
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
            .instantiate_blanket_impl(self_type_id, blanket_impl_id, solutions)
            .unwrap_or_else(|e| self.ice("Failed to instantiate blanket impl", Some(&e)));
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
            self.named_types.add_slice_copy(&substituted_ability_args);
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

        let _ = self.scopes.add_type(new_impl_scope, self.ast.idents.b.Self_, self_type_id);

        let mut specialized_functions = self.mem.new_list(blanket_impl.functions.len());
        let kind = AbilityImplKind::DerivedFromBlanket { blanket_impl_id };
        debug!(
            "blanket impl instance scope before function specialization: {}",
            self.scope_id_to_string(new_impl_scope)
        );
        for blanket_impl_function in self.mem.getn(blanket_impl.functions) {
            // If the functions are abstract, just the type ids
            // If concrete do the declaration thing
            //
            let specialized_function = match *blanket_impl_function {
                AbilityImplFunction::FunctionId(blanket_impl_function_id) => {
                    let blanket_fn = self.get_function(blanket_impl_function_id);
                    let parsed_fn = blanket_fn.parsed_id.as_function_id().unwrap();
                    let specialized_function_id = self
                        .compile_function_declaration(
                            parsed_fn,
                            new_impl_scope,
                            Some(FunctionAbilityContextInfo::ability_impl(
                                concrete_ability_id,
                                self_type_id,
                                kind,
                                Some(blanket_impl_function_id),
                                false,
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
            self.named_types.add_slice_copy(&substituted_impl_arguments);
        let id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: SliceHandle::empty(),
            self_type_id,
            ability_id: concrete_ability_id,
            base_ability_id: generic_base_ability_id,
            impl_arguments: substituted_impl_arguments_handle,
            functions: self.mem.vec_to_mslice(&specialized_functions),
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
        &mut self,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<StaticValueId> {
        let parsed_text = self.ast.get_span_content(span);
        let is_float = parsed_text.contains('.');
        if is_float {
            let float_value = self.eval_float_value(span, ctx)?;
            let value_id = self.static_values.add(StaticValue::Float(float_value));
            Ok(value_id)
        } else {
            let int_value = self.eval_integer_value(span, ctx)?;
            let value_id = self.static_values.add(StaticValue::Int(int_value));
            Ok(value_id)
        }
    }

    fn eval_float_value(&self, span: SpanId, ctx: EvalExprContext) -> TyperResult<TypedFloatValue> {
        let parsed_text = self.ast.get_span_content(span);
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
        &mut self,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedIntValue> {
        let parsed_text = self.ast.get_span_content(span);

        let maybe_suffix_char = parsed_text.chars().find_position(|&c| c == 'u' || c == 'i');
        let suffix_result = match maybe_suffix_char {
            None => None,
            Some((offset, _)) => {
                let (num, suffix) = parsed_text.split_at(offset);
                let int_type = match suffix {
                    "u8" => IntegerType::U8,
                    "u16" => IntegerType::U16,
                    "u32" => IntegerType::U32,
                    "u64" => IntegerType::U64,
                    "uint" => IntegerType::U64,
                    "usize" => IntegerType::U64,
                    "i8" => IntegerType::I8,
                    "i16" => IntegerType::I16,
                    "i32" => IntegerType::I32,
                    "i64" => IntegerType::I64,
                    "int" => IntegerType::I64,
                    "size" => IntegerType::I64,
                    _ => {
                        return Err(errf!(
                            span,
                            "Invalid integer suffix '{}'; expected u8, u16, u32, u64, uint, usize, i8, i16, i32, i64, int, size",
                            suffix
                        ));
                    }
                };
                //eprintln!("num_text is {num}, itype is {}", int_type);
                Some((int_type, num))
            }
        };

        let (suffix_int_type, num_to_parse) = match suffix_result {
            None => (None, parsed_text),
            Some((int_type, num_text)) => (Some(int_type), num_text),
        };

        self.buffers.int_parse.push_str(num_to_parse);
        if num_to_parse.contains('_') {
            self.buffers.int_parse.retain(|c| c != '_');
        };
        let num_to_parse = &self.buffers.int_parse;

        let expected_type_id = match ctx.expected_type_id {
            None => None,
            Some(t) => Some(match self.types.get_no_follow_static(t) {
                Type::Static(stat) => stat.inner_type_id,
                _ => t,
            }),
        };
        let default_int_type = IntegerType::I64;
        let expected_int_type = suffix_int_type.unwrap_or(match expected_type_id {
            None => default_int_type,
            Some(U8_TYPE_ID) => IntegerType::U8,
            Some(U16_TYPE_ID) => IntegerType::U16,
            Some(U32_TYPE_ID) => IntegerType::U32,
            Some(U64_TYPE_ID) => IntegerType::U64,
            Some(I8_TYPE_ID) => IntegerType::I8,
            Some(I16_TYPE_ID) => IntegerType::I16,
            Some(I32_TYPE_ID) => IntegerType::I32,
            Some(I64_TYPE_ID) => IntegerType::I64,
            Some(_other) => {
                // Parse as default and let typechecking fail
                default_int_type
            }
        });
        macro_rules! parse_int {
            ($int_type:ident, $rust_int_type:ty, $base: expr, $offset: expr) => {{
                let result = <$rust_int_type>::from_str_radix(&num_to_parse[$offset..], $base);
                result.map(|int| TypedIntValue::$int_type(int))
            }};
        }
        let ret = if num_to_parse.starts_with("0x") {
            let hex_base = 16;
            let offset = 2;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, hex_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, hex_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, hex_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, hex_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, hex_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, hex_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, hex_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, hex_base, offset),
            };
            value.map_err(|e| make_error(format!("Invalid hex {expected_int_type}: {e}"), span))
        } else if num_to_parse.starts_with("0b") {
            let bin_base = 2;
            let offset = 2;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, bin_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, bin_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, bin_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, bin_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, bin_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, bin_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, bin_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, bin_base, offset),
            };
            value.map_err(|e| make_error(format!("Invalid binary {expected_int_type}: {e}"), span))
        } else {
            let dec_base = 10;
            let offset = 0;
            let value: Result<TypedIntValue, std::num::ParseIntError> = match expected_int_type {
                IntegerType::U8 => parse_int!(U8, u8, dec_base, offset),
                IntegerType::U16 => parse_int!(U16, u16, dec_base, offset),
                IntegerType::U32 => parse_int!(U32, u32, dec_base, offset),
                IntegerType::U64 => parse_int!(U64, u64, dec_base, offset),
                IntegerType::I8 => parse_int!(I8, i8, dec_base, offset),
                IntegerType::I16 => parse_int!(I16, i16, dec_base, offset),
                IntegerType::I32 => parse_int!(I32, i32, dec_base, offset),
                IntegerType::I64 => parse_int!(I64, i64, dec_base, offset),
            };
            value.map_err(|e| {
                errf!(
                    span,
                    "Invalid {} integer {expected_int_type}: `{num_to_parse}` {e}",
                    if expected_int_type.is_signed() { "signed" } else { "unsigned" }
                )
            })
        };
        self.buffers.int_parse.clear();
        ret
    }

    fn eval_variable(
        &mut self,
        variable_expr_id: ParsedExprId,
        scope_id: ScopeId,
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
                let parent_lambda = self.scopes.enclosing_functions.get(scope_id).lambda;
                let (is_capture, lambda_scope_id) = if let Some(lambda_scope_id) = parent_lambda {
                    let variable_is_above_lambda =
                        self.scopes.scope_has_ancestor(lambda_scope_id, variable_scope_id);
                    let variable_is_global = self.variables.get(variable_id).global_id().is_some();

                    let is_capture = variable_is_above_lambda && !variable_is_global;
                    debug!("{}, is_capture={is_capture}", self.ident_str(variable.name.name));
                    (is_capture, Some(lambda_scope_id))
                } else {
                    (false, None)
                };

                let v = self.variables.get(variable_id);
                if is_capture {
                    if !variable.name.path.is_empty() {
                        return failf!(
                            variable_name_span,
                            "Should not capture namespaced things, I think?"
                        );
                    }
                    let fixup_expr_id = self.exprs.add(
                        TypedExpr::PendingCapture(PendingCaptureExpr {
                            captured_variable_id: variable_id,
                            resolved_expr: None,
                        }),
                        v.type_id,
                        variable_name_span,
                    );
                    self.scopes.add_capture(lambda_scope_id.unwrap(), variable_id, fixup_expr_id);
                    Ok((variable_id, fixup_expr_id))
                } else {
                    let expr = self.exprs.add(
                        TypedExpr::Variable(VariableExpr { variable_id }),
                        v.type_id,
                        variable_name_span,
                    );
                    self.variables.get_mut(variable_id).usage_count += 1;
                    Ok((variable_id, expr))
                }
            }
        }
    }

    pub fn get_expr_type(&self, expr_id: TypedExprId) -> &Type {
        self.types.get(self.exprs.get_type(expr_id))
    }

    fn eval_field_access(
        &mut self,
        field_access: &parse::FieldAccess,
        ctx: EvalExprContext,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExprId> {
        // Special case: Enum Constructor
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

        // Special case: .* dereference operation
        if field_access.field_name == self.ast.idents.b.asterisk {
            return self.eval_dereference(field_access.base, ctx, span);
        }

        // Special case: .! unwrap operation
        if field_access.field_name == self.ast.idents.b.bang {
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

        // Special case: .try unwrap operation
        if field_access.field_name == self.ast.idents.b.try_ {
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

        // Special case: .toMut / .unMut reference unwrap operations
        if field_access.field_name == self.ast.idents.b.toMut
            || field_access.field_name == self.ast.idents.b.unMut
        {
            let to_mut = field_access.field_name == self.ast.idents.b.toMut;
            let name = if to_mut { "toMut" } else { "unMut" };
            let base_expr = self.eval_expr(field_access.base, ctx)?;
            let reference_type = match self.get_expr_type(base_expr) {
                Type::Reference(reference_type) => *reference_type,
                _ => {
                    return failf!(
                        field_access.span,
                        "{name} must be used on a reference; this is a {}",
                        self.type_id_to_string(self.exprs.get_type(base_expr))
                    );
                }
            };
            if to_mut {
                if reference_type.mutable {
                    return failf!(
                        field_access.span,
                        "{name} must be used on a non-mutable reference"
                    );
                }
                let mut_reference_type = self.types.add_anon(Type::Reference(ReferenceType {
                    inner_type: reference_type.inner_type,
                    mutable: true,
                }));
                return Ok(self.synth_cast(
                    base_expr,
                    mut_reference_type,
                    CastType::ReferenceToMut,
                    Some(field_access.span),
                ));
            } else {
                if reference_type.is_read_only() {
                    return failf!(field_access.span, "{name} must be used on a mutable reference");
                }
                let nonmut_reference_type = self.types.add_anon(Type::Reference(ReferenceType {
                    inner_type: reference_type.inner_type,
                    mutable: false,
                }));
                return Ok(self.synth_cast(
                    base_expr,
                    nonmut_reference_type,
                    CastType::ReferenceUnMut,
                    Some(field_access.span),
                ));
            }
        }

        let base_expr = self.eval_expr(field_access.base, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get_type(base_expr);

        // Optional fork case: .tag enum special accessor
        if field_access.field_name == self.ast.idents.b.tag {
            if field_access.is_coalescing {
                return failf!(field_access.span, "TODO: support coalesce for .tag");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to tag");
            }
            if let Some(get_tag) = self.handle_enum_get_tag(base_expr, field_access.span)? {
                return Ok(get_tag);
            }
        }

        if field_access.field_name == self.ast.idents.b.variantName {
            if field_access.is_coalescing {
                return failf!(field_access.span, "TODO: support coalesce for .variantName");
            }
            if is_assignment_lhs {
                return failf!(field_access.span, "Cannot assign to variantName");
            }
            return failf!(
                field_access.span,
                "TODO: Generate variantName() code (for only enums that it is called on!)"
            );
        }

        // Perform auto-dereference for accesses that are not 'lvalue'-style or 'referencing' style
        let (struct_type_id, base_reference_type) = match self.get_expr_type(base_expr) {
            Type::Reference(reference_type) => {
                let inner_type = reference_type.inner_type;
                (inner_type, Some(*reference_type))
            }
            _other => {
                if is_assignment_lhs {
                    return failf!(base_span, "Cannot assign to member of non-reference struct");
                }
                if field_access.is_referencing && !field_access.is_coalescing {
                    return failf!(
                        base_span,
                        "Field access target is not a pointer, so referencing access with * cannot be used"
                    );
                }
                (base_expr_type, None)
            }
        };
        let base_is_reference = base_reference_type.is_some();
        let access_kind = if base_is_reference {
            if field_access.is_referencing {
                FieldAccessKind::ReferenceThrough
            } else {
                FieldAccessKind::Dereference
            }
        } else {
            FieldAccessKind::ValueToValue
        };
        match self.types.get(struct_type_id) {
            Type::Struct(struct_type) => {
                if is_assignment_lhs && !base_is_reference {
                    return failf!(span, "Struct must be a reference to be assignable");
                }
                let (field_index, target_field) = struct_type
                    .find_field(&self.types.mem, field_access.field_name)
                    .ok_or_else(|| {
                        errf!(
                            span,
                            "Field {} not found on struct {}",
                            self.ast.idents.get_name(field_access.field_name),
                            self.type_id_to_string(struct_type_id)
                        )
                    })?;
                let result_type = if field_access.is_referencing {
                    let reference_type = base_reference_type.unwrap();
                    self.types.add_reference_type(target_field.type_id, reference_type.mutable)
                } else {
                    target_field.type_id
                };
                Ok(self.exprs.add(
                    TypedExpr::StructFieldAccess(FieldAccess {
                        base: base_expr,
                        field_index: field_index as u32,
                        access_kind,
                        struct_type: struct_type_id,
                    }),
                    result_type,
                    span,
                ))
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
                if is_assignment_lhs && !base_is_reference {
                    return failf!(span, "Enum must be a reference to be assignable");
                }
                let variant_index = ev.index;
                let result_type_id = if field_access.is_referencing {
                    let reference_type = base_reference_type.unwrap();
                    self.types.add_reference_type(payload_type_id, reference_type.mutable)
                } else {
                    payload_type_id
                };

                Ok(self.exprs.add(
                    TypedExpr::EnumGetPayload(GetEnumVariantPayload {
                        enum_variant_expr: base_expr,
                        variant_index,
                        access_kind,
                    }),
                    result_type_id,
                    span,
                ))
            }
            Type::Enum(_opt_type) => {
                if let Some(opt_inner_type) = self.types.get_as_opt_instance(struct_type_id) {
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
                    let mut block =
                        self.synth_block(ctx.scope_id, ScopeType::LexicalBlock, span, 2);
                    let block_scope = block.scope_id;
                    let base_expr_var = self.synth_variable_defn_simple(
                        field_access.field_name,
                        base_expr,
                        block_scope,
                    );
                    let has_value = self.synth_typed_call_typed_args(
                        self.ast.idents.f.Opt_isSome.with_span(span),
                        &[opt_inner_type],
                        &[base_expr_var.variable_expr],
                        ctx.with_scope(block_scope).with_no_expected_type(),
                        false,
                    )?;

                    let Type::Struct(struct_type) = self.types.get(opt_inner_type) else {
                        return failf!(
                            span,
                            "?. must be used on optional structs, got {}",
                            self.type_id_to_string(base_expr_type)
                        );
                    };
                    let (field_index, target_field) = struct_type
                        .find_field(&self.types.mem, field_access.field_name)
                        .ok_or_else(|| {
                            errf!(
                                span,
                                "Field {} not found on struct {}",
                                self.ast.idents.get_name(field_access.field_name),
                                self.type_id_to_string(opt_inner_type)
                            )
                        })?;
                    let field_type = target_field.type_id;
                    let opt_unwrap = self.synth_typed_call_typed_args(
                        self.ast.idents.f.Opt_get.with_span(span),
                        &[opt_inner_type],
                        &[base_expr_var.variable_expr],
                        ctx.with_scope(block_scope).with_no_expected_type(),
                        false,
                    )?;
                    let field_access = self.exprs.add(
                        TypedExpr::StructFieldAccess(FieldAccess {
                            base: opt_unwrap,
                            field_index: field_index as u32,
                            access_kind: FieldAccessKind::ValueToValue,
                            struct_type: opt_inner_type,
                        }),
                        field_type,
                        span,
                    );
                    let (consequent, consequent_type_id) = self.synth_optional_some(field_access);
                    let alternate = self.synth_optional_none(field_type, span);
                    let if_expr = self.synth_if_else(
                        consequent_type_id,
                        has_value,
                        consequent,
                        alternate,
                        span,
                    );
                    self.push_block_stmt_id(&mut block, base_expr_var.defn_stmt);
                    self.push_expr_id_to_block(&mut block, if_expr);
                    let ty = self.exprs.get_type(if_expr);
                    Ok(self.exprs.add_block(&mut self.mem, block, ty))
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
        let try_value_type = self.exprs.get_type(try_value_original_expr);
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
        let mut result_block = self.synth_block(scope_id, ScopeType::LexicalBlock, span, 2);
        let try_value_var = self.synth_variable_defn_simple(
            self.ast.idents.b.try_value,
            try_value_original_expr,
            result_block.scope_id,
        );
        let result_block_ctx = ctx.with_scope(result_block.scope_id).with_no_expected_type();
        let is_ok_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Try_isOk.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        let get_ok_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Try_getOk.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        let get_error_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Try_getError.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        // FIXME: Consider alternatives for locating the block's makeError function
        //        in a less brittle way
        let block_make_error_fn =
            self.ability_impls.get(block_try_impl.full_impl_id).function_at_index(&self.mem, 0);
        let call_id = self.calls.add(Call {
            callee: Callee::from_ability_impl_fn(block_make_error_fn),
            args: smallvec![get_error_call],
            type_args: SliceHandle::empty(),
            return_type: block_return_type,
            span,
        });
        let make_error_call = self.exprs.add(TypedExpr::Call { call_id }, block_return_type, span);
        let return_error_expr = self.exprs.add(
            TypedExpr::Return(TypedReturn { value: make_error_call }),
            NEVER_TYPE_ID,
            span,
        );
        let if_expr = self.synth_if_else(
            value_success_type,
            is_ok_call,
            get_ok_call,
            return_error_expr,
            span,
        );

        self.push_block_stmt_id(&mut result_block, try_value_var.defn_stmt);
        self.push_expr_id_to_block(&mut result_block, if_expr);

        Ok(self.exprs.add_block(&mut self.mem, result_block, value_success_type))
    }

    fn eval_unwrap_operator(
        &mut self,
        operand: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> TyperResult<TypedExprId> {
        let operand_expr = self.eval_expr_inner(operand, ctx.with_no_expected_type())?;
        let operand_type = self.exprs.get_type(operand_expr);
        let _unwrap_impl = self.expect_ability_implementation(
            operand_type,
            UNWRAP_ABILITY_ID,
            ctx.scope_id,
            span,
        )?;
        self.synth_typed_call_typed_args(
            self.ast.idents.f.Unwrap_unwrap.with_span(span),
            &[],
            &[operand_expr],
            ctx,
            false,
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
        // The expected_type when we get `intptr.*` is int, so
        // the expected_type when we get `intptr` should be *int
        let inner_expected_type = match ctx.expected_type_id {
            Some(expected) => Some(self.types.add_reference_type(expected, false)),
            None => None,
        };
        let base_expr = self.eval_expr(operand, ctx.with_expected_type(inner_expected_type))?;
        let base_expr_type = self.exprs.get_type(base_expr);
        let reference_type = self.types.get(base_expr_type).as_reference().ok_or_else(|| {
            errf!(
                span,
                "Cannot dereference non-reference type: {}",
                self.type_id_to_string(base_expr_type)
            )
        })?;
        Ok(self.exprs.add(
            TypedExpr::Deref(DerefExpr { target: base_expr }),
            reference_type.inner_type,
            span,
        ))
    }

    #[allow(unused)]
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

    #[allow(unused)]
    fn is_scope_inside_namespace(&self, namespace_id: NamespaceId, scope_id: ScopeId) -> bool {
        let ns_scope_id = self.namespaces.get_scope(namespace_id);
        self.scopes.scope_has_ancestor(scope_id, ns_scope_id)
    }

    fn eval_expr(
        &mut self,
        expr_id: ParsedExprId,
        mut ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let expr_metadata = self.ast.exprs.get_metadata(expr_id);
        let is_debug = expr_metadata.is_debug;
        if is_debug {
            self.push_debug_level();
        }
        let mut self_ = scopeguard::guard(self, |s| {
            if is_debug {
                s.pop_debug_level()
            }
        });

        let mut explicit_hint = false;
        ctx.expected_type_id = match expr_metadata.type_hint {
            Some(t) => {
                let type_id = self_.eval_type_expr(t, ctx.scope_id)?;
                explicit_hint = true;
                Some(type_id)
            }
            None => ctx.expected_type_id,
        };
        let result_expr = self_.eval_expr_inner(expr_id, ctx)?;
        let result_expr = if explicit_hint {
            let expected_type_id = ctx.expected_type_id.unwrap();
            let coerced_expr = self_
                .check_and_coerce_expr(expected_type_id, result_expr, ctx.scope_id)
                .map_err(|e| {
                    errf!(
                        self_.ast.exprs.get_span(expr_id),
                        "Expression did not conform to hint: {}",
                        e.message
                    )
                })?;
            coerced_expr
        } else {
            result_expr
        };

        if log::log_enabled!(log::Level::Debug) {
            let expr_span = self_.ast.exprs.get_span(expr_id);
            debug!(
                "COMPILED `{}` (hint {})\n`{}`",
                self_.ast.get_span_content(expr_span),
                self_.type_id_to_string_opt(ctx.expected_type_id),
                self_.expr_to_string_with_type(result_expr)
            );
        };
        Ok(result_expr)
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
            self.type_id_to_string_opt(ctx.expected_type_id),
        );
        let expr = self.ast.exprs.get(expr_id);
        match expr {
            ParsedExpr::ListLiteral(list_expr) => {
                self.eval_list_literal(expr_id, &list_expr.clone(), ctx)
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
            ParsedExpr::Literal(ParsedLiteral::Unit(span)) => Ok(self.synth_unit(*span)),
            ParsedExpr::Literal(ParsedLiteral::Char(byte, span)) => {
                let value_id = self.static_values.add(StaticValue::Char(*byte));
                let expr_id = self.add_static_constant_expr(value_id, *span);
                Ok(expr_id)
            }
            ParsedExpr::Literal(ParsedLiteral::Numeric(int)) => {
                let span = int.span;
                let value_id = self.eval_numeric_value(span, ctx)?;
                let expr_id = self.add_static_constant_expr(value_id, span);
                Ok(expr_id)
            }
            ParsedExpr::Literal(ParsedLiteral::Bool(b, span)) => Ok(self.synth_bool(*b, *span)),
            ParsedExpr::Literal(ParsedLiteral::String(string_id, span)) => {
                let static_value_id = self.static_values.add_string(*string_id);
                let is_typed_as_static = ctx.is_inference();
                let type_to_use = if is_typed_as_static {
                    self.types.add_static_type(STRING_TYPE_ID, Some(static_value_id))
                } else {
                    STRING_TYPE_ID
                };
                let static_expr =
                    self.exprs.add_static(static_value_id, type_to_use, is_typed_as_static, *span);
                Ok(static_expr)
            }
            ParsedExpr::Variable(_variable) => Ok(self.eval_variable(expr_id, ctx.scope_id)?.1),
            ParsedExpr::FieldAccess(field_access) => {
                let field_access = field_access.clone();
                self.eval_field_access(&field_access, ctx, false)
            }
            ParsedExpr::Block(block) => {
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
                Ok(block)
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
                    SliceHandle::empty(),
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
                let true_expression = self.ast.exprs.add_expression(
                    parse::ParsedExpr::Literal(parse::ParsedLiteral::Bool(true, is_expr.span)),
                    false,
                    None,
                );
                let false_expression = self.ast.exprs.add_expression(
                    parse::ParsedExpr::Literal(parse::ParsedLiteral::Bool(false, is_expr.span)),
                    false,
                    None,
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
                let match_expr_id = self.ast.exprs.add_expression(
                    parse::ParsedExpr::Match(as_match_expr),
                    false,
                    None,
                );
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
                        Ok(self.synth_bool(is_test_build, *span))
                    }
                    "OS" => {
                        // TODO: Ideally this is an enum!
                        let os_str = self.ast.config.target.target_os().to_str();
                        let string_id = self.ast.strings.intern(os_str);
                        Ok(self.synth_string_literal(string_id, *span))
                    }
                    "NO_STD" => {
                        let no_std = self.ast.config.no_std;
                        Ok(self.synth_bool(no_std, *span))
                    }
                    "DEBUG" => {
                        let debug = self.ast.config.debug;
                        Ok(self.synth_bool(debug, *span))
                    }
                    "IS_STATIC" => Ok(self.synth_bool(false, *span)),
                    "MULTITHREADING" => {
                        let bool_value = self.program_settings.multithreaded;
                        Ok(self.synth_bool(bool_value, *span))
                    }
                    s => failf!(*span, "Unknown builtin name: {s}"),
                }
            }
            ParsedExpr::Static(stat) => {
                let stat = *stat;
                match self.eval_static_expr_and_exec(expr_id, stat, ctx)? {
                    StaticExecutionResult::TypedExpr(typed_expr) => Ok(typed_expr),
                    StaticExecutionResult::Definitions(_) => {
                        self.ice_with_span("Got static definitions from an expression", stat.span)
                    }
                }
            }
            ParsedExpr::Code(code) => {
                let parsed_stmt_span = self.ast.get_stmt_span(code.parsed_stmt);
                let span_content =
                    self.ast.sources.get_span_content(self.ast.spans.get(parsed_stmt_span));
                let string_id = self.ast.strings.intern(span_content);
                let id = self.synth_string_literal(string_id, code.span);
                // debug!("content for #code is exactly: `{span_content}`");
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
                let Some(tafr) = self
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
                let impl_function = impl_.function_at_index(&self.mem, tafr.index);
                self.eval_function_call(
                    &call_ast_expr,
                    None,
                    ctx,
                    Some(Callee::from_ability_impl_fn(impl_function)),
                )
            }
        }
    }

    fn eval_expr_with_coercion(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
        fail: bool,
    ) -> TyperResult<TypedExprId> {
        let expr = self.eval_expr(expr_id, ctx)?;
        match ctx.expected_type_id {
            None => Ok(expr),
            Some(expected_type) => {
                match self.check_and_coerce_expr(expected_type, expr, ctx.scope_id) {
                    Ok(expr) => Ok(expr),
                    error @ Err(_) => {
                        if fail {
                            error
                        } else {
                            Ok(expr)
                        }
                    }
                }
            }
        }
    }

    fn add_static_value_expr(&mut self, value_id: StaticValueId, span: SpanId) -> TypedExprId {
        let inner_type_id = self.static_values.get(value_id).get_type();
        let static_type_id = self.types.add_static_type(inner_type_id, Some(value_id));
        self.exprs.add_static(value_id, static_type_id, true, span)
    }

    fn add_static_constant_expr(&mut self, value_id: StaticValueId, span: SpanId) -> TypedExprId {
        let type_id = self.static_values.get(value_id).get_type();
        self.exprs.add_static(value_id, type_id, false, span)
    }

    fn eval_list_literal(
        &mut self,
        _expr_id: ParsedExprId,
        list_expr: &ParsedList,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let (expected_element_type, list_kind) = match ctx.expected_type_id.as_ref() {
            Some(&type_id) => {
                if let Some((element_type, container_kind)) =
                    self.types.get_as_container_instance(type_id)
                {
                    (Some(element_type), container_kind)
                } else {
                    (None, ContainerKind::List)
                }
            }
            None => (None, ContainerKind::List),
        };
        let span = list_expr.span;
        let parsed_elements = &list_expr.elements;
        let element_count = parsed_elements.len();

        let mut list_lit_block =
            self.synth_block(ctx.scope_id, ScopeType::LexicalBlock, span, 2 + element_count as u32);
        let list_lit_scope = list_lit_block.scope_id;
        let mut element_type = None;
        let elements: MList<TypedExprId, MemTmp> = {
            let mut elements = self.tmp.new_list(element_count as u32);
            for elem in parsed_elements.iter() {
                let current_expected_type = element_type.or(expected_element_type);
                let element_expr =
                    self.eval_expr(*elem, ctx.with_expected_type(current_expected_type))?;
                let element_expr_checked = match current_expected_type {
                    None => element_expr,
                    Some(current_expected_type) => self
                        .check_and_coerce_expr(current_expected_type, element_expr, list_lit_scope)
                        .map_err(|e| {
                            errf!(e.span, "List element had incorrect type: {}", e.message)
                        })?,
                };
                let this_element_type = self.exprs.get_type(element_expr_checked);
                if element_type.is_none() {
                    // Erase static type info
                    let chased_type = self.types.get_chased_id(this_element_type);
                    element_type = Some(chased_type)
                };
                elements.push(element_expr_checked);
            }
            elements
        };
        // Note: Typing of empty list literals with no expected type is tricky
        //        We use is_inference here to use UNIT or fail.
        //        If I report UNIT during inference it leads to incorrect failures
        //        Failing during inference is like producing a type hole
        let element_type = match element_type.or(expected_element_type) {
            Some(et) => et,
            None => {
                if ctx.is_inference() {
                    return failf!(span, "Not enough information to determine empty list type");
                } else {
                    UNIT_TYPE_ID
                }
            }
        };
        let list_lit_ctx = ctx.with_scope(list_lit_scope).with_no_expected_type();
        let count_expr = self.synth_i64(element_count as i64, span);
        let make_dest_coll = match list_kind {
            ContainerKind::List => self.synth_typed_call_typed_args(
                self.ast.idents.f.List_withCapacity.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            ContainerKind::Buffer | ContainerKind::View => self.synth_typed_call_typed_args(
                self.ast.idents.f.Buffer__allocate.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            // Unlike the others, the array literal should go on the stack!
            ContainerKind::Array(array_type_id) => self.synth_typed_call_typed_args(
                self.ast.idents.f.mem_zeroed.with_span(span),
                &[array_type_id],
                &[],
                list_lit_ctx,
                false,
            )?,
        };
        let is_referencing_let = match list_kind {
            ContainerKind::Array(_) => true,
            ContainerKind::Buffer | ContainerKind::View => false,
            ContainerKind::List => true,
        };
        let dest_coll_variable = self.synth_variable_defn(
            self.ast.idents.b.dest,
            make_dest_coll,
            false,
            true, // is_mutable
            is_referencing_let,
            list_lit_scope,
        );

        list_lit_block.statements.push(dest_coll_variable.defn_stmt);
        for (index, element_value_expr) in elements.iter().enumerate() {
            let index_expr = self.synth_i64(index as i64, span);
            let push_call = match list_kind {
                ContainerKind::List => self.synth_typed_call_typed_args(
                    self.ast.idents.f.List_push.with_span(span),
                    &[element_type],
                    &[dest_coll_variable.variable_expr, *element_value_expr],
                    list_lit_ctx,
                    false,
                )?,
                ContainerKind::Buffer | ContainerKind::View => self.synth_typed_call_typed_args(
                    self.ast.idents.f.Buffer_set.with_span(span),
                    &[element_type],
                    &[dest_coll_variable.variable_expr, index_expr, *element_value_expr],
                    list_lit_ctx,
                    false,
                )?,
                ContainerKind::Array(array_type_id) => {
                    // fn set[T, N: static size](array: Array[T, N]*, index: size, value: T): unit
                    let size_type = self.types.get(array_type_id).as_array().unwrap().size_type;
                    self.synth_typed_call_typed_args(
                        self.ast.idents.f.Array_set.with_span(span),
                        &[element_type, size_type],
                        &[dest_coll_variable.variable_expr, index_expr, *element_value_expr],
                        list_lit_ctx,
                        false,
                    )?
                }
            };
            let type_id = self.exprs.get_type(push_call);
            let push_stmt = self.stmts.add(TypedStmt::Expr(push_call, type_id));
            self.push_block_stmt_id(&mut list_lit_block, push_stmt);
        }
        let final_expr = match list_kind {
            ContainerKind::List => self.synth_dereference(dest_coll_variable.variable_expr),
            ContainerKind::Buffer => dest_coll_variable.variable_expr,
            ContainerKind::View => self.synth_typed_call_typed_args(
                self.ast.idents.f.View_wrapBuffer.with_span(span),
                &[element_type],
                &[dest_coll_variable.variable_expr],
                ctx.with_no_expected_type(),
                false,
            )?,
            ContainerKind::Array(_array_type_id) => dest_coll_variable.variable_expr,
        };
        self.push_expr_id_to_block(&mut list_lit_block, final_expr);
        let final_expr_type = self.exprs.get_type(final_expr);
        Ok(self.exprs.add_block(&mut self.mem, list_lit_block, final_expr_type))
    }

    /// Compiles `#static <expr>` and `#meta <expr>` constructs
    fn eval_static_expr_and_exec(
        &mut self,
        _expr_id: ParsedExprId,
        stat: ParsedStaticExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<StaticExecutionResult> {
        let span = stat.span;
        let base_expr = stat.base_expr;

        // We don't execute statics during the generic pass, since there's no point
        // 1. we don't know the real types of generics, thus values of things like schemas, etc
        // 2. There's not really a use-case for it, metaprograms always want to generate
        //    real code
        //
        // So we just return the expected type, or a unit
        debug!(
            "eval_static_expr ctx.is_generic_pass={}",
            ctx.flags.contains(EvalExprFlags::GenericPass)
        );
        if ctx.flags.contains(EvalExprFlags::GenericPass) {
            let typed_expr = match ctx.expected_type_id {
                None => self.synth_unit(span),
                Some(expected) => {
                    let unit_expr = self.synth_unit(span);
                    self.synth_cast(unit_expr, expected, CastType::Transmute, Some(span))
                }
            };
            return Ok(StaticExecutionResult::TypedExpr(typed_expr));
        }

        let kind = stat.kind;
        let (expected_type_for_execution, expected_is_static) = match kind {
            ParsedStaticBlockKind::Value => match ctx.expected_type_id {
                None => (None, false),
                Some(expected_type_id) => match self.types.get_no_follow_static(expected_type_id) {
                    Type::Static(s) => (Some(s.inner_type_id), true),
                    _ => (Some(expected_type_id), false),
                },
            },
            ParsedStaticBlockKind::Metaprogram => (Some(STRING_TYPE_ID), false),
        };
        let is_metaprogram = kind.is_metaprogram();
        let mut static_parameters: SV4<(VariableId, StaticValueId)> = smallvec![];
        for param in self.ast.idents.slices.copy_slice_sv4(stat.parameter_names) {
            let variable_expr = self.ast.exprs.add_expression(
                ParsedExpr::Variable(ParsedVariable { name: QIdent::naked(param, span) }),
                false,
                None,
            );
            let (variable_id, variable_expr) = self.eval_variable(variable_expr, ctx.scope_id)?;
            let variable_type = self.exprs.get_type(variable_expr);
            match self.types.get_no_follow_static(variable_type) {
                Type::Static(stat) => {
                    if let Some(value_id) = stat.value_id {
                        static_parameters.push((variable_id, value_id));
                    } else {
                        return failf!(
                            span,
                            "Static parameter `{}` is unresolved",
                            self.ident_str(param)
                        );
                    }
                }
                Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                    let static_type = self
                        .types
                        .get_no_follow_static(tp.static_constraint.unwrap())
                        .as_static()
                        .unwrap();
                    let Some(value_id) = static_type.value_id else {
                        return failf!(
                            span,
                            "Expected a resolved static type for argument {}",
                            self.ident_str(param),
                        );
                    };
                    static_parameters.push((variable_id, value_id));
                }
                _ => {
                    return failf!(
                        span,
                        "Non-static parameters aren't supported: {}: {}",
                        self.ident_str(param),
                        self.type_id_to_string(variable_type)
                    );
                }
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
            &static_parameters,
        )?;

        match kind {
            ParsedStaticBlockKind::Value => {
                let expr = if expected_is_static {
                    self.add_static_value_expr(vm_result, span)
                } else {
                    self.add_static_constant_expr(vm_result, span)
                };
                Ok(StaticExecutionResult::TypedExpr(expr))
            }
            ParsedStaticBlockKind::Metaprogram => {
                let StaticValue::String(string_id) = self.static_values.get(vm_result) else {
                    return failf!(span, "#meta block did not evaluate to a string");
                };
                let emitted_string = self.ast.strings.get_string(*string_id);
                if emitted_string.is_empty() {
                    if stat.is_definition {
                        Ok(StaticExecutionResult::Definitions(eco_vec![]))
                    } else {
                        Ok(StaticExecutionResult::TypedExpr(self.synth_unit(span)))
                    }
                } else {
                    // First, we write the emitted code to a text buffer
                    //
                    // Then we parse the code anew (so that we have cohesive spans and a source
                    // containing the full code)
                    //
                    // Then we typecheck the code and emit a block in place of this #meta
                    // invocation
                    let mut content = std::mem::take(&mut self.buffers.emitted_code);
                    let (source, line) = self.get_span_location(span);
                    writeln!(
                        &mut content,
                        "// generated by #meta block at {}/{}:{}",
                        source.directory,
                        source.filename,
                        line.line_number(),
                    )
                    .unwrap();
                    if !stat.is_definition {
                        content.push_str("{\n");
                    }
                    // FIXME: generated_filename is not unique if we specialized on multiple types
                    //        We need a specialization context, for both debugging and logging
                    //        and for this
                    //        This could be rolled in with `is_generic_pass` ->
                    //        If its not generic pass, provide specialization info payload
                    // TODO: if specializing, print what the types are at the top of the file.
                    //       this is actually really important debugging context
                    // TODO: stem source.filename too
                    let generated_filename =
                        format!("meta_{}_{}.k1", source.filename, line.line_number());

                    content.push_str(emitted_string);
                    if !stat.is_definition {
                        content.push('}');
                    }
                    debug!("Emitted raw content:\n---\n{content}\n---");
                    let generated_path = self.ast.config.out_dir.join(&generated_filename);
                    let source_for_emission =
                        self.ast.sources.add_source(crate::parse::Source::make(
                            0,
                            self.ast
                                .config
                                .out_dir
                                .canonicalize()
                                .unwrap()
                                .to_str()
                                .unwrap()
                                .to_owned(),
                            generated_filename,
                            content.clone(),
                        ));
                    if let Err(e) = std::fs::write(&generated_path, &content) {
                        eprintln!(
                            "Failed to write out generated metaprogram at {}. {e}",
                            generated_path.display()
                        )
                    }

                    let parse_kind = if stat.is_definition {
                        ParseAdHocKind::Definitions
                    } else {
                        ParseAdHocKind::Expr
                    };
                    let parsed_metaprogram_result =
                        self.parse_ad_hoc(source_for_emission, &content, parse_kind);
                    content.clear();
                    self.buffers.emitted_code = content;
                    let parsed_metaprogram = parsed_metaprogram_result?;
                    match parsed_metaprogram {
                        ParseAdHocResult::Expr(parsed_expr_id) => {
                            let typed_metaprogram = self.eval_expr(parsed_expr_id, ctx)?;
                            debug!(
                                "Emitted compiled expr:\n{}",
                                self.expr_to_string(typed_metaprogram)
                            );
                            Ok(StaticExecutionResult::TypedExpr(typed_metaprogram))
                        }
                        ParseAdHocResult::Definitions(defns_slice) => {
                            Ok(StaticExecutionResult::Definitions(defns_slice))
                        }
                    }
                }
            }
        }
    }

    fn parse_ad_hoc(
        &mut self,
        file_id: FileId,
        code_str: &str,
        kind: ParseAdHocKind,
    ) -> TyperResult<ParseAdHocResult> {
        let module = self.modules.get(self.module_in_progress.unwrap());
        let parsed_namespace_id =
            self.namespaces.get(module.namespace_id).parsed_id.as_namespace_id().unwrap();
        let mut lexer = crate::lex::Lexer::make(code_str, &mut self.ast.spans, file_id);
        let mut tokens = std::mem::take(&mut self.buffers.lexer_tokens);
        if let Err(e) = lexer.run(&mut tokens) {
            let e = ParseError::Lex(e);
            parse::print_error(&self.ast, &e);
            tokens.clear();
            return failf!(e.span(), "Failed to lex code emitted from here");
        };
        // FIXME: we just filter out comment tokens before parsing to this day
        tokens.retain(|token| token.kind != TokenKind::LineComment);

        let mut p = crate::parse::Parser::make_for_file(
            module.name,
            parsed_namespace_id,
            &mut self.ast,
            &tokens,
            file_id,
        );

        let result = match kind {
            ParseAdHocKind::Expr => match p.expect_expression() {
                Err(e) => {
                    failf!(e.span(), "Failed to parse your emitted code: {}", e)
                }
                Ok(parsed_expr) => Ok(ParseAdHocResult::Expr(parsed_expr)),
            },
            ParseAdHocKind::Definitions => match p.parse_definitions(TokenKind::Eof) {
                Err(e) => failf!(e.span(), "Failed to parse your emitted code: {}", e),
                Ok(defns) => Ok(ParseAdHocResult::Definitions(defns)),
            },
        };

        tokens.clear();
        self.buffers.lexer_tokens = tokens;

        result
    }

    fn eval_anonymous_struct(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::Struct(parsed_struct) = self.ast.exprs.get(expr_id) else {
            self.ice_with_span("expected struct", self.ast.get_expr_span(expr_id))
        };
        let mut field_values = self.mem.new_list(parsed_struct.fields.len() as u32);
        let mut field_defns = self.types.mem.new_list(parsed_struct.fields.len() as u32);
        let ast_struct = parsed_struct.clone();
        for ast_field in ast_struct.fields.iter() {
            let parsed_expr = match ast_field.expr.as_ref() {
                None => self.ast.exprs.add_expression(
                    ParsedExpr::Variable(parse::ParsedVariable {
                        name: QIdent::naked(ast_field.name, ast_field.span),
                    }),
                    false,
                    None,
                ),
                Some(expr) => *expr,
            };
            let expr = self.eval_expr(parsed_expr, ctx.with_expected_type(None))?;
            let expr_type = self.exprs.get_type(expr);
            if expr_type == NEVER_TYPE_ID {
                return failf!(ast_field.span, "never is not allowed in struct literals");
            }
            field_defns.push(StructTypeField { name: ast_field.name, type_id: expr_type });
            field_values.push(StructLiteralField { name: ast_field.name, expr });
        }

        let struct_type = StructType { fields: self.types.mem.vec_to_mslice(&field_defns) };
        let struct_type_id = self.types.add_anon(Type::Struct(struct_type));
        let typed_struct = StructLiteral { fields: self.mem.vec_to_mslice(&field_values) };
        Ok(self.exprs.add(TypedExpr::Struct(typed_struct), struct_type_id, ast_struct.span))
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
        let expected_struct_defn_info = self.types.get_defn_info(expected_struct_id);
        let field_count = expected_struct.fields.len();

        let ast_struct = parsed_struct.clone();

        // Try to use just stack space for this scratch data structure
        let mut passed_fields_aligned: SmallVec<
            [(ParsedExprId, &StructValueField, &StructTypeField); 8],
        > = SmallVec::with_capacity(field_count as usize);

        let struct_span = ast_struct.span;
        for expected_field in self.types.mem.getn(expected_struct.fields).iter() {
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
                None => self.ast.exprs.add_expression(
                    ParsedExpr::Variable(parse::ParsedVariable {
                        name: QIdent::naked(passed_field.name, passed_field.span),
                    }),
                    false,
                    None,
                ),
                Some(expr) => *expr,
            };
            passed_fields_aligned.push((parsed_expr, passed_field, expected_field))
        }

        if let Some(unknown_field) = ast_struct.fields.iter().find(|passed_field| {
            original_expected_struct.find_field(&self.types.mem, passed_field.name).is_none()
        }) {
            return failf!(
                struct_span,
                "Struct has an unexpected field '{}'",
                self.ident_str(unknown_field.name)
            );
        }

        let mut field_values: MList<StructLiteralField, _> = self.mem.new_list(field_count);
        let mut field_types: MList<StructTypeField, _> = self.types.mem.new_list(field_count);
        for ((passed_expr, passed_field, _), expected_field) in
            passed_fields_aligned.iter().zip(self.types.mem.getn(expected_struct.fields).iter())
        {
            let expr = self
                .eval_expr_with_coercion(
                    *passed_expr,
                    ctx.with_expected_type(Some(expected_field.type_id)),
                    true,
                )
                .map_err(|e| {
                    errf!(
                        passed_field.span,
                        "Field '{}' has an issue\n    {}",
                        self.ident_str(passed_field.name),
                        e.message
                    )
                })?;
            let expr_type = self.exprs.get_type(expr);
            if expr_type == NEVER_TYPE_ID {
                return failf!(passed_field.span, "never is not allowed in struct literals");
            }
            field_types.push(StructTypeField { name: expected_field.name, type_id: expr_type });
            field_values.push(StructLiteralField { name: expected_field.name, expr });
        }

        let output_instance_info = match self.types.get_instance_info(expected_struct_id).cloned() {
            None => None,
            Some(mut gi) => {
                if ctx.is_inference() {
                    debug!(
                        "I need to set the right info for {} from expected [{}] and my literal values [{}]",
                        self.type_id_to_string_ext(gi.generic_parent, true),
                        self.pretty_print_type_slice(gi.type_args, ", "),
                        self.pretty_print_types(
                            &field_types.iter().map(|ft| ft.type_id).collect::<Vec<_>>(),
                            ", "
                        )
                    );
                    // We're effectively reverse-engineering what params were used to get to this type
                    // So we start with: { a: int, b: bool } and definition Pair[A, B] = { a: A, b: B }
                    // And we need to solve for A and B as int and bool.
                    let generic_type = self.types.get(gi.generic_parent).expect_generic();
                    let generic_params = generic_type.params;
                    let generic_struct_id = generic_type.inner;
                    let generic_fields = self.types.get(generic_struct_id).expect_struct().fields;

                    // Run this inference on a fresh state; will be popped by infer()
                    self.ictx_push();
                    let mut subst_pairs = self.tmp.new_list(generic_fields.len());
                    for (value, generic_field) in
                        field_types.iter().zip(self.types.mem.getn(generic_fields).iter())
                    {
                        subst_pairs.push(InferenceInputPair {
                            param_type: generic_field.type_id,
                            arg: TypeOrParsedExpr::Type(value.type_id),
                            allow_mismatch: true,
                        });
                    }
                    let generic_params_owned: SV8<NameAndType> =
                        self.named_types.copy_slice_sv8(generic_params);
                    let (solutions, _all_solutions) = self.infer_types(
                        &generic_params_owned,
                        generic_params,
                        &subst_pairs,
                        struct_span,
                        ctx.scope_id,
                    )?;
                    debug!(
                        "I reverse-engineered these: {}",
                        self.pretty_print_named_type_slice(solutions, ", ")
                    );
                    gi.type_args = self.types.mem.pushn_iter(
                        self.named_types.get_slice(solutions).iter().map(|s| s.type_id),
                    );
                    Some(gi)
                } else {
                    Some(gi)
                }
            }
        };
        let output_struct = StructType { fields: self.types.mem.vec_to_mslice(&field_types) };
        let output_struct_type_id = self.types.add(
            Type::Struct(output_struct),
            expected_struct_defn_info,
            output_instance_info,
        );

        let typed_struct = StructLiteral { fields: self.mem.vec_to_mslice(&field_values) };
        Ok(self.exprs.add(TypedExpr::Struct(typed_struct), output_struct_type_id, ast_struct.span))
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

        let condition_or_block = self
            .eval_matching_condition(while_expr.cond, ctx.with_scope(condition_block_scope_id))?;
        let condition = match condition_or_block {
            Either::Right(mc) => mc,
            Either::Left(crash_block) => return Ok(crash_block),
        };

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

        // TODO: detect divergent loops: if loop has no breaks or returns, can we type is as never?
        //
        // Loop Info should be able to track this, if we report every
        // break and return
        let loop_type = UNIT_TYPE_ID;

        Ok(self.exprs.add(
            TypedExpr::WhileLoop(WhileLoop { condition, body: body_block }),
            loop_type,
            while_expr.span,
        ))
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
        let body_block = self.eval_block(
            &loop_expr.body.clone(),
            ctx.with_scope(body_scope).with_expected_type(expected_expression_type_for_block),
            false,
        )?;

        let loop_info = self.scopes.get_loop_info(body_scope).unwrap();

        let break_type = loop_info.break_type.unwrap_or(UNIT_TYPE_ID);
        Ok(self.exprs.add(TypedExpr::LoopExpr(LoopExpr { body_block }), break_type, loop_expr.span))
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
            let parse::InterpolatedStringPart::String(string_id) =
                interpolated_string.parts.first().unwrap().clone()
            else {
                self.ice_with_span("String had only one part that was not a string", span)
            };
            let e = self.synth_string_literal(string_id, span);
            Ok(e)
        } else {
            let interpolated_string = interpolated_string.clone();
            let mut block = self.synth_block(
                ctx.scope_id,
                ScopeType::LexicalBlock,
                span,
                part_count as u32 + 2,
            );
            let block_scope = block.scope_id;
            let block_ctx = ctx.with_scope(block_scope).with_no_expected_type();
            if self.ast.config.no_std {
                return failf!(span, "Interpolated strings are not supported in no_std mode");
            }
            let new_string_builder = self.synth_typed_call_typed_args(
                self.ast.idents.f.StringBuilder_new.with_span(span),
                &[],
                &[],
                block_ctx,
                false,
            )?;
            let string_builder_var = self.synth_variable_defn(
                get_ident!(self, "sb"),
                new_string_builder,
                false,
                true, // is_mutable
                true, // is_referencing
                block.scope_id,
            );
            self.push_block_stmt_id(&mut block, string_builder_var.defn_stmt);
            for part in interpolated_string.parts.into_iter() {
                match part {
                    parse::InterpolatedStringPart::String(string_id) => {
                        let rust_str = self.ast.strings.get_string(string_id);
                        if !rust_str.is_empty() {
                            let string_expr = self.synth_string_literal(string_id, span);
                            let print_literal_call = self.synth_printto_call(
                                string_expr,
                                string_builder_var.variable_expr,
                                block_ctx,
                            )?;
                            self.push_expr_id_to_block(&mut block, print_literal_call);
                        }
                    }
                    parse::InterpolatedStringPart::Expr(expr_id) => {
                        let typed_expr_to_stringify = self.eval_expr(expr_id, block_ctx)?;
                        let print_expr_call = self.synth_printto_call(
                            typed_expr_to_stringify,
                            string_builder_var.variable_expr,
                            ctx,
                        )?;
                        self.push_expr_id_to_block(&mut block, print_expr_call);
                    }
                };
            }
            let build_call = self.synth_typed_call_typed_args(
                self.ast.idents.f.StringBuilder_buildTmp.with_span(span),
                &[],
                &[string_builder_var.variable_expr],
                block_ctx,
                false,
            )?;
            self.push_expr_id_to_block(&mut block, build_call);
            let build_call_type = self.exprs.get_type(build_call);
            Ok(self.exprs.add_block(&mut self.mem, block, build_call_type))
        }
    }

    fn eval_lambda(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        fn fixup_capture_expr_new(
            k1: &mut TypedProgram,
            environment_param_variable_id: VariableId,
            captured_variable_id: VariableId,
            env_struct_type: TypeId,
            span: SpanId,
        ) -> (TypedExpr, TypeId, SpanId) {
            let v = k1.variables.get(captured_variable_id);
            let variable_type = v.type_id;
            let env_struct_reference_type = k1.types.add_reference_type(env_struct_type, false);
            // Note: Can't capture 2 variables of the same name in a lambda. Might not
            //       actually be a problem
            let (field_index, _env_struct_field) =
                k1.types.get_struct_field_by_name(env_struct_type, v.name).unwrap();
            let env_variable_expr = k1.exprs.add(
                TypedExpr::Variable(VariableExpr { variable_id: environment_param_variable_id }),
                env_struct_reference_type,
                span,
            );
            k1.variables.get_mut(environment_param_variable_id).usage_count += 1;
            let env_field_access = TypedExpr::StructFieldAccess(FieldAccess {
                base: env_variable_expr,
                field_index: field_index as u32,
                struct_type: env_struct_type,
                access_kind: FieldAccessKind::ValueToValue,
            });
            (env_field_access, variable_type, span)
        }
        let lambda = self.ast.exprs.get(expr_id).expect_lambda();
        let lambda_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LambdaScope, None, None);
        let lambda_arguments = lambda.arguments.clone();
        let lambda_body = lambda.body;
        let span = lambda.span;
        let body_span = self.ast.exprs.get_span(lambda.body);
        let mut typed_params = self.types.mem.new_list(lambda_arguments.len() as u32 + 1);
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
                    let Some(expected_ty) =
                        self.types.mem.get_nth_opt(expected_function_type.logical_params(), index)
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
            typed_params.push(FnParamType {
                name: arg.binding,
                type_id: arg_type_id,
                is_context: false,
                is_lambda_env: false,
                span: arg.span,
            });
        }

        let mut param_variables = self.mem.new_list(lambda_arguments.len() as u32 + 1);

        let lambda_scope = self.scopes.get_scope_mut(lambda_scope_id);
        for typed_arg in typed_params.iter() {
            let name = typed_arg.name;
            let variable_id = self.variables.add(Variable {
                name,
                type_id: typed_arg.type_id,
                owner_scope: lambda_scope_id,
                kind: VariableKind::FnParam(FunctionId::PENDING),
                flags: VariableFlags::empty(),
                usage_count: 0,
            });
            lambda_scope.add_variable(name, variable_id);
            param_variables.push(variable_id)
        }

        // Coerce parsed expr to block, call eval_block with needs_terminator = true
        let ast_body_block =
            self.ensure_parsed_expr_to_block(lambda_body, ParsedBlockKind::FunctionBody);
        let body_expr_id = self.eval_block(
            &ast_body_block,
            ctx.with_scope(lambda_scope_id).with_expected_type(expected_return_type),
            true,
        )?;
        let body_type = self.exprs.get_type(body_expr_id);
        if let Some(expected_return_type) = expected_return_type {
            if let Err(msg) = self.check_types(expected_return_type, body_type, ctx.scope_id) {
                return failf!(body_span, "Lambda returns incorrect type: {msg}");
            }
        }

        let return_type = match body_type {
            NEVER_TYPE_ID => expected_return_type.unwrap_or(NEVER_TYPE_ID),
            _ => body_type,
        };

        let encl_fn_id = self
            .scopes
            .enclosing_functions
            .get(ctx.scope_id)
            .function
            .expect("lambda to be inside a function");
        let encl_fn_name = self.get_function(encl_fn_id).name;
        let name = self.build_ident_with(|k1, s| {
            s.push_str(k1.ident_str(encl_fn_name));
            s.push_str("_{lambda}_");
            write!(s, "{}", lambda_scope_id.as_u32()).unwrap();
        });
        let name_string = self.make_qualified_name(ctx.scope_id, name, "__", true);
        let name = self.ast.idents.intern(name_string);

        let lambda_info = self.scopes.get_lambda_info(lambda_scope_id);

        // NO CAPTURES! Optimize this lambda down into a regular function
        if lambda_info.captured_variables.is_empty() {
            let function_type = self.types.add_anon(Type::Function(FunctionType {
                physical_params: self.types.mem.vec_to_mslice(&typed_params),
                return_type,
                is_lambda: false,
            }));

            self.scopes.get_scope_mut(lambda_scope_id).scope_type = ScopeType::FunctionScope;
            let body_function_id = self.functions.next_id();
            for v in param_variables.iter() {
                self.variables.get_mut(*v).kind = VariableKind::FnParam(body_function_id)
            }
            self.add_function(TypedFunction {
                name,
                scope: lambda_scope_id,
                param_variables: self.mem.vec_to_mslice(&param_variables),
                type_params: SliceHandle::empty(),
                function_type_params: SliceHandle::empty(),
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

            let function_pointer_type = self.types.add_function_pointer_type(function_type);
            let expr_id = self.exprs.add(
                TypedExpr::FunctionPointer(FunctionPointerExpr { function_id: body_function_id }),
                function_pointer_type,
                span,
            );
            return Ok(expr_id);
        }

        let mut env_field_types =
            self.types.mem.new_list(lambda_info.captured_variables.len() as u32);
        let mut env_exprs = self.mem.new_list(lambda_info.captured_variables.len() as u32);
        for captured_variable_id in lambda_info.captured_variables.iter() {
            let v = self.variables.get(*captured_variable_id);
            env_field_types.push(StructTypeField { type_id: v.type_id, name: v.name });
            let var_expr = self.exprs.add(
                TypedExpr::Variable(VariableExpr { variable_id: *captured_variable_id }),
                v.type_id,
                span,
            );
            env_exprs.push(StructLiteralField { name: v.name, expr: var_expr });
        }
        let env_fields_handle = self.types.mem.vec_to_mslice(&env_field_types);
        let environment_struct_type =
            self.types.add_anon(Type::Struct(StructType { fields: env_fields_handle }));

        let environment_struct = self.exprs.add(
            TypedExpr::Struct(StructLiteral { fields: self.mem.vec_to_mslice(&env_exprs) }),
            environment_struct_type,
            body_span,
        );

        let environment_struct_reference_type =
            self.types.add_reference_type(environment_struct_type, false);
        // We decay down to POINTER so that the function calls typecheck
        let environment_param = FnParamType {
            name: self.ast.idents.b.lambda_env_var_name,
            type_id: POINTER_TYPE_ID,
            is_context: false,
            is_lambda_env: true,
            span: body_span,
        };
        let body_function_id = self.functions.next_id();
        let environment_param_variable_id = self.variables.add(Variable {
            name: environment_param.name,
            type_id: POINTER_TYPE_ID,
            owner_scope: lambda_scope_id,
            kind: VariableKind::FnParam(body_function_id),
            flags: VariableFlags::empty(),
            usage_count: 0,
        });
        typed_params.insert(0, environment_param);
        param_variables.insert(0, environment_param_variable_id);

        // We decay down to POINTER so that the function calls typecheck
        let environment_param_access_expr = self.exprs.add(
            TypedExpr::Variable(VariableExpr { variable_id: param_variables[0] }),
            POINTER_TYPE_ID,
            body_span,
        );
        let cast_env_param = self.synth_cast(
            environment_param_access_expr,
            environment_struct_reference_type,
            CastType::PointerToReference,
            None,
        );
        let environment_casted_variable = self.synth_variable_defn(
            self.ast.idents.b.env,
            cast_env_param,
            false,
            false,
            false,
            lambda_scope_id,
        );
        if let TypedExpr::Block(body) = self.exprs.get_mut(body_expr_id) {
            let mut new_stmts = self.mem.new_list(body.statements.len() + 1);
            new_stmts.push(environment_casted_variable.defn_stmt);
            new_stmts.extend(self.mem.getn(body.statements));

            body.statements = self.mem.vec_to_mslice(&new_stmts);
        } else {
            panic!()
        }

        let pending_fixups =
            self.scopes.get_lambda_info(lambda_scope_id).capture_exprs_for_fixup.clone();

        for pending_fixup in pending_fixups {
            let TypedExpr::PendingCapture(pc) = self.exprs.get(pending_fixup) else {
                unreachable!()
            };
            let pc_span = self.exprs.get_span(pending_fixup);
            let (field_access_expr, type_id, span_id) = fixup_capture_expr_new(
                self,
                environment_casted_variable.variable_id,
                pc.captured_variable_id,
                environment_struct_type,
                pc_span,
            );
            self.exprs.set_full(pending_fixup, field_access_expr, type_id, span_id);
        }

        let function_type = self.types.add_anon(Type::Function(FunctionType {
            physical_params: self.types.mem.vec_to_mslice(&typed_params),
            return_type,
            is_lambda: true,
        }));

        let actual_body_function_id = self.add_function(TypedFunction {
            name,
            scope: lambda_scope_id,
            param_variables: self.mem.vec_to_mslice(&param_variables),
            type_params: SliceHandle::empty(),
            function_type_params: SliceHandle::empty(),
            body_block: Some(body_expr_id),
            intrinsic_type: None,
            linkage: Linkage::Standard,
            child_specializations: vec![],
            specialization_info: None,
            parsed_id: expr_id.into(),
            type_id: function_type,
            compiler_debug: false,
            kind: TypedFunctionKind::Lambda,
            // Set by add_function
            is_concrete: false,
            dyn_fn_id: None,
        });
        debug_assert_eq!(actual_body_function_id, body_function_id);

        let lambda_type_id = self.types.add_lambda(
            function_type,
            environment_struct,
            environment_struct_type,
            body_function_id,
            expr_id.into(),
        );
        self.scopes.set_scope_owner_id(lambda_scope_id, ScopeOwnerId::Lambda(lambda_type_id));
        debug!(
            "end eval_lambda {} with is_inference {}. Function id is: {}",
            lambda_type_id.as_u32(),
            ctx.is_inference(),
            body_function_id
        );
        Ok(self.exprs.add(
            // Seems lambda is the only TypedExpr that is representable as only its type!
            TypedExpr::Lambda(LambdaExpr { lambda_type: lambda_type_id }),
            lambda_type_id,
            span,
        ))
    }

    fn ensure_parsed_expr_to_block(
        &mut self,
        body: ParsedExprId,
        kind: ParsedBlockKind,
    ) -> ParsedBlock {
        match self.ast.exprs.get(body) {
            ParsedExpr::Block(b) => b.clone(),
            other_expr => {
                let block = parse::ParsedBlock {
                    span: other_expr.get_span(),
                    kind,
                    stmts: eco_vec![self.ast.stmts.add(parse::ParsedStmt::LoneExpression(body))],
                };
                block
            }
        }
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

        let parsed_cases = &match_parsed_expr.cases;
        let total_arms: usize =
            parsed_cases.iter().map(|parsed_case| parsed_case.patterns.len()).sum();

        let mut typed_arms: MList<TypedMatchArm, _> = self.mem.new_list(total_arms as u32 + 1); // Add one for fallback arm

        let mut expected_arm_type_id = ctx.expected_type_id;

        let mut all_unguarded_patterns: MList<(TypedPatternId, usize), MemTmp> =
            self.tmp.new_list(parsed_cases.iter().map(|pc| pc.patterns.len() as u32).sum());
        let target_expr_type = self.exprs.get_type(match_subject_variable.variable_expr);
        let target_expr_span = self.exprs.get_span(match_subject_variable.variable_expr);

        // Core loop to build up the typed, compiled match arms
        for parsed_case in parsed_cases.iter() {
            let multi_pattern = parsed_case.patterns.len() > 1;
            let mut expected_bindings: Option<SmallVec<[VariablePattern; 8]>> = None;
            for parsed_pattern_id in parsed_case.patterns.iter() {
                let pattern = self.eval_pattern(
                    *parsed_pattern_id,
                    target_expr_type,
                    match_scope_id,
                    allow_bindings,
                )?;

                // If a match arm has multiple patterns, they must produce the exact same
                // set of variable bindings: matching name and type
                if multi_pattern {
                    match &expected_bindings {
                        None => {
                            expected_bindings = Some(self.patterns.get_pattern_bindings(pattern));
                        }
                        Some(expected_bindings) => {
                            let this_pattern_bindings = self.patterns.get_pattern_bindings(pattern);
                            if this_pattern_bindings.is_empty() && !expected_bindings.is_empty() {
                                return failf!(
                                    self.patterns.get(pattern).span_id(),
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

                if parsed_case.guard_condition_expr.is_none() {
                    all_unguarded_patterns.push((pattern, 0));
                }

                // Note: We compile the arm's consequent expression and the guard condition as many times as there are patterns, since each
                // one has its own scope. To get around this we'd have to create only one compiled arm even for
                // multi-pattern binding arms, and have the condition be a boolean OR of the various
                // arms, and somehow compile in the right variables defns based on which one passed.
                // Which isn't possible to know at compile time. So I think this is just where we are.
                // It'd be nice to re-use the typed expr across different scopes, but we can't do that
                //
                // The solution once again is to compile things multiple times if needed, and just make
                // compilation fast
                {
                    let arm_scope_id = self.scopes.add_child_scope(
                        match_scope_id,
                        ScopeType::MatchArm,
                        None,
                        None,
                    );
                    let pattern_eval_ctx =
                        arms_ctx.with_scope(arm_scope_id).with_no_expected_type();
                    let mut instrs = self.mem.new_list(8);
                    self.compile_pattern_into_values(
                        pattern,
                        match_subject_variable.variable_expr,
                        &mut instrs,
                        false,
                        pattern_eval_ctx,
                    )?;

                    match parsed_case.guard_condition_expr {
                        None => {}
                        Some(guard_condition_expr_id) => {
                            let guard_condition_expr = self.eval_expr(
                                guard_condition_expr_id,
                                pattern_eval_ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                            )?;
                            instrs
                                .push(MatchingConditionInstr::Cond { value: guard_condition_expr });
                        }
                    };

                    // Once we've evaluated the conditions, we can eval the consequent expression inside of it,
                    // since the bindings are now available
                    let consequent_expr = self.eval_expr_with_coercion(
                        parsed_case.expression,
                        pattern_eval_ctx.with_expected_type(expected_arm_type_id),
                        true,
                    )?;
                    let consequent_expr_type = self.exprs.get_type(consequent_expr);

                    if expected_arm_type_id.is_none() && consequent_expr_type != NEVER_TYPE_ID {
                        expected_arm_type_id = Some(consequent_expr_type);
                    }

                    let match_arm = TypedMatchArm {
                        condition: MatchingCondition { instrs: self.mem.vec_to_mslice(&instrs) },
                        consequent_expr,
                    };
                    typed_arms.push(match_arm);
                }
            }
        }

        // Exhaustiveness Checking
        if !partial {
            let mut trial_constructors = std::mem::take(&mut self.buffers.trial_ctors);
            let mut field_ctors_buf = std::mem::take(&mut self.buffers.field_ctors);
            trial_constructors.clear();
            self.generate_constructors_for_type(
                target_expr_type,
                &mut trial_constructors,
                &mut field_ctors_buf,
                target_expr_span,
            );
            'trial: for trial_entry in trial_constructors.iter_mut() {
                '_pattern: for (pattern, kill_count) in all_unguarded_patterns.iter_mut() {
                    if TypedProgram::pattern_matches(
                        &self.pattern_ctors,
                        &self.patterns,
                        *pattern,
                        trial_entry.ctor,
                    ) {
                        *kill_count += 1;
                        trial_entry.alive = false;
                        continue 'trial;
                    }
                }
            }

            let alive_count = trial_constructors.iter().filter(|entry| entry.alive).count();
            self.buffers.trial_ctors = trial_constructors;
            self.buffers.field_ctors = field_ctors_buf;
            if alive_count != 0 {
                let patterns = self
                    .buffers
                    .trial_ctors
                    .iter()
                    .filter(|entry| entry.alive)
                    .map(|entry| self.pattern_ctor_to_string(entry.ctor))
                    .join("\n- ");
                return failf!(
                    target_expr_span,
                    "{} Unhandled patterns:\n- {}",
                    alive_count,
                    patterns
                );
            }

            if let Some((useless_pattern, _useless_index)) =
                all_unguarded_patterns.iter().find(|(_, kill_count)| *kill_count == 0)
            {
                if !self.patterns.pattern_has_innumerable_literal(*useless_pattern) {
                    return failf!(
                        self.patterns.get(*useless_pattern).span_id(),
                        "This pattern handled no cases: {}",
                        self.pattern_to_string(*useless_pattern)
                    );
                }
            }
        }

        let fallback_arm = TypedMatchArm {
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: self.synth_crash_call(
                "No cases matched",
                match_expr_span,
                arms_ctx.with_no_expected_type(),
            )?,
        };
        typed_arms.push(fallback_arm);

        // The result type of the match is the type of the first non-never arm, or never
        // They've already been typechecked against each other.
        let match_result_type = typed_arms
            .iter()
            .find_map(|arm| {
                let conseqent_type = self.exprs.get_type(arm.consequent_expr);
                if conseqent_type != NEVER_TYPE_ID { Some(conseqent_type) } else { None }
            })
            .unwrap_or(NEVER_TYPE_ID);
        Ok(self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                initial_let_statements: self.mem.pushn(&[match_subject_variable.defn_stmt]),
                arms: self.mem.vec_to_mslice(&typed_arms),
            }),
            match_result_type,
            match_expr_span,
        ))
    }

    /// Accumulates a list of 'BindingOrCond' while 'compiling' a pattern match.
    /// Basically, every part of a pattern match boils down to either
    /// - A boolean condition to be evaluated
    /// - A new variable binding
    fn compile_pattern_into_values(
        &mut self,
        pattern: TypedPatternId,
        target_expr: TypedExprId,
        instrs: &mut MList<MatchingConditionInstr, TypedProgram>,
        is_immediately_inside_reference_pattern: bool,
        ctx: EvalExprContext,
    ) -> TyperResult<()> {
        let target_expr_type = self.exprs.get_type(target_expr);
        let pat = self.patterns.get(pattern);
        match pat {
            TypedPattern::Struct(struct_pattern) => {
                let is_referencing = is_immediately_inside_reference_pattern;
                let struct_type = struct_pattern.struct_type_id;
                let struct_pattern_span = struct_pattern.span;
                for pattern_field in self.patterns.get_slice(struct_pattern.fields).iter() {
                    let struct_reference_type = if is_referencing {
                        Some(self.types.get(target_expr_type).expect_reference())
                    } else {
                        None
                    };
                    let result_type = if is_referencing {
                        self.types.add_reference_type(
                            pattern_field.field_type_id,
                            struct_reference_type.unwrap().mutable,
                        )
                    } else {
                        pattern_field.field_type_id
                    };
                    let get_struct_field = self.exprs.add(
                        TypedExpr::StructFieldAccess(FieldAccess {
                            base: target_expr,
                            field_index: pattern_field.field_index,
                            struct_type,
                            access_kind: if is_referencing {
                                FieldAccessKind::ReferenceThrough
                            } else {
                                FieldAccessKind::ValueToValue
                            },
                        }),
                        result_type,
                        struct_pattern_span,
                    );
                    let var_name = self.build_ident_with(|k1, s| {
                        write!(s, "field_{}", k1.ident_str(pattern_field.name)).unwrap();
                    });
                    let struct_field_variable =
                        self.synth_variable_defn_simple(var_name, get_struct_field, ctx.scope_id);
                    instrs.push_grow(
                        &mut self.mem,
                        MatchingConditionInstr::Binding {
                            let_stmt: struct_field_variable.defn_stmt,
                        },
                    );
                    self.compile_pattern_into_values(
                        pattern_field.pattern,
                        struct_field_variable.variable_expr,
                        instrs,
                        is_referencing,
                        ctx,
                    )?;
                }
                Ok(())
            }
            TypedPattern::Enum(enum_pattern) => {
                let enum_pattern = *enum_pattern;
                let is_referencing = is_immediately_inside_reference_pattern;
                let is_variant_condition = self.synth_enum_is_variant(
                    target_expr,
                    enum_pattern.variant_index,
                    ctx,
                    Some(enum_pattern.span),
                )?;
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::Cond { value: is_variant_condition },
                );

                if let Some(payload_pattern) = enum_pattern.payload {
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
                    let (result_type_id, is_mutable) = if is_referencing {
                        let mutable = self.types.get(target_expr_type).expect_reference().mutable;
                        let r = self.types.add_reference_type(payload_type_id, mutable);
                        (r, mutable)
                    } else {
                        (payload_type_id, false)
                    };
                    let variant_target_type = if is_referencing {
                        self.types.add_reference_type(variant_type_id, is_mutable)
                    } else {
                        variant_type_id
                    };
                    let enum_as_variant = self.synth_cast(
                        target_expr,
                        variant_target_type,
                        if is_referencing {
                            CastType::ReferenceToReference
                        } else {
                            CastType::EnumToVariant
                        },
                        Some(enum_pattern.span),
                    );
                    let get_payload_expr = self.exprs.add(
                        TypedExpr::EnumGetPayload(GetEnumVariantPayload {
                            enum_variant_expr: enum_as_variant,
                            variant_index,
                            access_kind: if is_referencing {
                                FieldAccessKind::ReferenceThrough
                            } else {
                                FieldAccessKind::ValueToValue
                            },
                        }),
                        result_type_id,
                        enum_pattern.span,
                    );
                    let var_name =
                        self.ast.idents.intern(format!("payload_{}", self.ident_str(variant_name)));
                    let payload_variable =
                        self.synth_variable_defn_simple(var_name, get_payload_expr, ctx.scope_id);
                    instrs.push_grow(
                        &mut self.mem,
                        MatchingConditionInstr::Binding { let_stmt: payload_variable.defn_stmt },
                    );
                    self.compile_pattern_into_values(
                        payload_pattern,
                        payload_variable.variable_expr,
                        instrs,
                        is_referencing,
                        ctx,
                    )?;
                };
                Ok(())
            }
            TypedPattern::Variable(variable_pattern) => {
                let variable_ident = variable_pattern.name;
                let binding_variable =
                    self.synth_variable_defn_visible(variable_ident, target_expr, ctx.scope_id);
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::Binding { let_stmt: binding_variable.defn_stmt },
                );
                Ok(())
            }
            TypedPattern::Wildcard(_span) => Ok(()),
            TypedPattern::Reference(reference_pattern) => {
                let inner_pattern = reference_pattern.inner_pattern;
                let target_expr = if is_immediately_inside_reference_pattern {
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                self.compile_pattern_into_values(inner_pattern, target_expr, instrs, true, ctx)?;
                Ok(())
            }
            literal_pat => {
                match literal_pat {
                    TypedPattern::LiteralUnit(_) => {}
                    TypedPattern::LiteralChar(_, _) => {}
                    TypedPattern::LiteralInteger(_, _) => {}
                    TypedPattern::LiteralFloat(_, _) => {}
                    TypedPattern::LiteralBool(_, _) => {}
                    TypedPattern::LiteralString(_, _) => {}
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
                // A good ole reborrow; typed pattern is only 24 bytes but it does have some RCs so
                // this should be faster
                match self.patterns.get(pattern) {
                    TypedPattern::LiteralUnit(_span) => Ok(()),
                    TypedPattern::LiteralChar(byte, span) => {
                        let char_value = self.static_values.add(StaticValue::Char(*byte));
                        let span = *span;
                        let char_expr = self.add_static_constant_expr(char_value, span);
                        let equals_pattern_char =
                            self.synth_equals_call(target_expr, char_expr, ctx, span)?;
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::Cond { value: equals_pattern_char },
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralInteger(int_value, span) => {
                        let span = *span;
                        let pattern_integer_literal =
                            self.add_static_constant_expr(*int_value, span);
                        let equals_pattern_int = self.synth_equals_call(
                            target_expr,
                            pattern_integer_literal,
                            ctx,
                            span,
                        )?;
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::Cond { value: equals_pattern_int },
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralFloat(float_value, span) => {
                        let span = *span;
                        let pattern_float_literal =
                            self.add_static_constant_expr(*float_value, span);
                        let equals_pattern_float =
                            self.synth_equals_call(target_expr, pattern_float_literal, ctx, span)?;
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::Cond { value: equals_pattern_float },
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralBool(bool_value, span) => {
                        let span = *span;
                        let bool_expr = self.synth_bool(*bool_value, span);
                        let equals_pattern_bool =
                            self.synth_equals_call(target_expr, bool_expr, ctx, span)?;
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::Cond { value: equals_pattern_bool },
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralString(string_id, span) => {
                        let span = *span;
                        let string_expr = self.synth_string_literal(*string_id, span);
                        let condition =
                            self.synth_equals_call(target_expr, string_expr, ctx, span)?;
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::Cond { value: condition },
                        );
                        Ok(())
                    }
                    _ => {
                        unreachable!(
                            "should only be literal patterns from here: {}",
                            self.pattern_to_string(pattern)
                        )
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
        let base_expr_type = self.exprs.get_type(base_expr);
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
                Type::Pointer => match from_integer_type {
                    IntegerType::U64 | IntegerType::I64 => Ok(CastType::WordToPointer),
                    _ => failf!(
                        cast.span,
                        "Cannot cast integer '{}' to Pointer (must be word-sized (64-bit))",
                        from_integer_type
                    ),
                },
                Type::Float(_to_float_type) => {
                    // We're just going to allow these casts and make it UB if it doesn't fit, the LLVM
                    // default. If I find a saturating version in LLVM I'll use that instead
                    match from_integer_type {
                        IntegerType::U8
                        | IntegerType::U16
                        | IntegerType::U32
                        | IntegerType::U64 => Ok(CastType::IntegerUnsignedToFloat),
                        IntegerType::I8 => Ok(CastType::IntegerSignedToFloat),
                        IntegerType::I16 => Ok(CastType::IntegerSignedToFloat),
                        IntegerType::I32 => Ok(CastType::IntegerSignedToFloat),
                        IntegerType::I64 => Ok(CastType::IntegerSignedToFloat),
                    }
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
                    IntegerType::U32 => Ok(CastType::FloatToUnsignedInteger),
                    IntegerType::U64 => Ok(CastType::FloatToUnsignedInteger),
                    IntegerType::I32 => Ok(CastType::FloatToSignedInteger),
                    IntegerType::I64 => Ok(CastType::FloatToSignedInteger),
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
            Type::FunctionPointer(_fp) => match self.types.get(target_type) {
                Type::Pointer => Ok(CastType::ReferenceToPointer),
                _ => failf!(
                    cast.span,
                    "Cannot cast Function Pointer to '{}'",
                    self.type_id_to_string(target_type).blue()
                ),
            },
            Type::Pointer => match self.types.get(target_type) {
                Type::Reference(_refer) => Ok(CastType::PointerToReference),
                Type::Integer(IntegerType::U64) => Ok(CastType::PointerToWord),
                Type::Integer(IntegerType::I64) => Ok(CastType::PointerToWord),
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
        Ok(self.synth_cast(base_expr, target_type, cast_type, Some(cast.span)))
    }

    fn eval_for_expr(
        &mut self,
        for_expr: &ForExpr,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let binding_ident = for_expr.binding.unwrap_or(self.ast.idents.b.it);
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, ctx.with_no_expected_type())?;
        let iterable_type = self.exprs.get_type(iterable_expr);
        let iterable_span = self.exprs.get_span(iterable_expr);
        let body_span = for_expr.body_block.span;

        // Project: Kill all this with the macro system
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
                            "for loop target {} must be Iterable or an Iterator",
                            self.type_id_to_string(iterable_type)
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

        let zero_expr = self.synth_i64(0, for_expr.body_block.span);
        let index_variable = self.synth_variable_defn(
            self.ast.idents.b.itIndex,
            zero_expr,
            true,
            true, // mutable = true
            false,
            outer_for_expr_scope,
        );
        let iterator_initializer = if target_is_iterator {
            iterable_expr
        } else {
            self.synth_typed_call_typed_args(
                self.ast.idents.f.Iterable_iterator.with_span(body_span),
                &[],
                &[iterable_expr],
                ctx.with_scope(outer_for_expr_scope).with_no_expected_type(),
                false,
            )?
        };
        let iterator_variable = self.synth_variable_defn(
            self.ast.idents.b.iter,
            iterator_initializer,
            false,
            true, //is_mutable
            true, //is_referencing
            outer_for_expr_scope,
        );
        let mut loop_block =
            self.synth_block(outer_for_expr_scope, ScopeType::LexicalBlock, body_span, 3);
        let loop_scope_id = loop_block.scope_id;
        let expected_block_type = ctx
            .expected_type_id
            .and_then(|t| self.types.get_as_list_instance(t))
            .map(|list_type| list_type.element_type);

        let mut consequent_block =
            self.synth_block(loop_scope_id, ScopeType::LexicalBlock, iterable_span, 3);

        let loop_scope_ctx = ctx.with_scope(loop_scope_id).with_no_expected_type();
        let iterator_next_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Iterator_next.with_span(body_span),
            &[],
            &[iterator_variable.variable_expr],
            loop_scope_ctx,
            false,
        )?;
        let next_variable = self.synth_variable_defn_simple(
            get_ident!(self, "next"),
            iterator_next_call,
            loop_scope_id,
        );
        let next_unwrap_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Unwrap_unwrap.with_span(iterable_span),
            &[],
            &[next_variable.variable_expr],
            ctx.with_scope(consequent_block.scope_id).with_no_expected_type(),
            false,
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
        let body_block_result_type = self.exprs.get_type(body_block);

        let outer_for_expr_ctx = ctx.with_scope(outer_for_expr_scope).with_no_expected_type();
        let yielded_coll_variable = if !is_do_block {
            let size_hint_call = self.synth_typed_call_typed_args(
                self.ast.idents.f.Iterator_sizeHint.with_span(body_span),
                &[],
                &[iterator_variable.variable_expr],
                outer_for_expr_ctx,
                false,
            )?;
            let size_hint_ret_type = self.exprs.get_type(size_hint_call);
            let size_hint_lower_bound = self.exprs.add(
                TypedExpr::StructFieldAccess(FieldAccess {
                    struct_type: size_hint_ret_type,
                    base: size_hint_call,
                    field_index: 0,
                    access_kind: FieldAccessKind::ValueToValue,
                }),
                SIZE_TYPE_ID,
                iterable_span,
            );
            let synth_function_call = self.synth_typed_call_typed_args(
                self.ast.idents.f.List_withCapacity.with_span(body_span),
                &[body_block_result_type],
                &[size_hint_lower_bound],
                outer_for_expr_ctx,
                false,
            )?;
            Some(self.synth_variable_defn(
                self.ast.idents.b.yieldDest,
                synth_function_call,
                false,
                true, //is_mutable
                true,
                outer_for_expr_scope,
            ))
        } else {
            None
        };

        self.push_block_stmt_id(&mut loop_block, next_variable.defn_stmt); // let next = iter.next();

        let user_body_block_id = body_block;
        let user_block_variable = self.synth_variable_defn_simple(
            get_ident!(self, "block_expr_val"),
            user_body_block_id,
            consequent_block.scope_id,
        );

        consequent_block.statements.push(binding_variable.defn_stmt);
        consequent_block.statements.push(user_block_variable.defn_stmt);
        // Push element to yielded list
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            let list_push_call = self.synth_typed_call_typed_args(
                self.ast.idents.f.List_push.with_span(body_span),
                &[body_block_result_type],
                &[yielded_coll_variable.variable_expr, user_block_variable.variable_expr],
                outer_for_expr_ctx,
                false,
            )?;
            self.push_expr_id_to_block(&mut consequent_block, list_push_call);
        }

        let next_is_some_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Opt_isSome.with_span(body_span),
            &[item_type],
            &[next_variable.variable_expr],
            loop_scope_ctx,
            false,
        )?;
        let unit_break = self.synth_unit(body_span);
        let break_expr = self.exprs.add(
            TypedExpr::Break(TypedBreak {
                value: unit_break,
                loop_scope: loop_scope_id,
                loop_type: LoopType::Loop,
            }),
            NEVER_TYPE_ID,
            body_span,
        );
        let consequent_block_id =
            self.exprs.add_block(&mut self.mem, consequent_block, UNIT_TYPE_ID);
        let if_next_loop_else_break_expr = self.synth_if_else(
            UNIT_TYPE_ID,
            next_is_some_call,
            consequent_block_id,
            break_expr,
            body_span,
        );
        self.push_expr_id_to_block(&mut loop_block, if_next_loop_else_break_expr);

        // Append the index increment to the body block
        let one_expr = self.synth_i64(1, iterable_span);
        let add_operation = self.synth_add_call(
            index_variable.variable_expr,
            one_expr,
            ctx.with_no_expected_type(),
            iterable_span,
        )?;
        let index_increment_statement = TypedStmt::Assignment(AssignmentStmt {
            destination: index_variable.variable_expr,
            value: add_operation,
            span: iterable_span,
            kind: AssignmentKind::Set,
        });
        self.push_block_stmt(&mut loop_block, index_increment_statement);

        let body_block = self.exprs.add_block(&mut self.mem, loop_block, UNIT_TYPE_ID);
        let loop_expr = self.exprs.add(
            TypedExpr::LoopExpr(LoopExpr { body_block }),
            UNIT_TYPE_ID,
            for_expr.span,
        );

        let mut for_expr_initial_statements = self.mem.new_list(5);
        for_expr_initial_statements.push(index_variable.defn_stmt);
        for_expr_initial_statements.push(iterator_variable.defn_stmt);
        if let Some(yielded_coll_variable) = &yielded_coll_variable {
            for_expr_initial_statements.push(yielded_coll_variable.defn_stmt);
        }
        let loop_stmt_id = self.add_expr_stmt(loop_expr);
        for_expr_initial_statements.push(loop_stmt_id);

        if let Some(yielded_coll_variable) = yielded_coll_variable {
            let yield_expr = self.synth_dereference(yielded_coll_variable.variable_expr);
            let yield_expr_type = self.exprs.get_type(yield_expr);
            let yield_stmt_id = self.stmts.add(TypedStmt::Expr(yield_expr, yield_expr_type));
            for_expr_initial_statements.push(yield_stmt_id);
        }

        let final_type =
            self.get_stmt_type(*for_expr_initial_statements.as_slice().last().unwrap());
        let final_expr = self.exprs.add_block(
            &mut self.mem,
            BlockBuilder {
                scope_id: outer_for_expr_scope,
                statements: for_expr_initial_statements,
                span: for_expr.body_block.span,
            },
            final_type,
        );
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
        let condition_bool = match ctx.is_generic_pass() {
            false => self.execute_static_bool(if_expr.cond, ctx)?,
            // We just proceed as if it yielded 'true' in the generic case
            true => true,
        };

        let expr = if condition_bool {
            let cons_expr = self.eval_expr(if_expr.cons, ctx)?;
            cons_expr
        } else {
            let alt_expr =
                if let Some(alt) = if_expr.alt { Some(self.eval_expr(alt, ctx)?) } else { None };
            if let Some(alt) = alt_expr { alt } else { self.synth_unit(if_expr.span) }
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

        let condition_or_block = self.eval_matching_condition(
            if_expr.cond,
            ctx.with_scope(match_scope_id).with_no_expected_type(),
        )?;
        let condition = match condition_or_block {
            Either::Left(block) => return Ok(block),
            Either::Right(mc) => mc,
        };

        let consequent = self.eval_expr(if_expr.cons, ctx.with_scope(match_scope_id))?;
        let consequent_type = self.exprs.get_type(consequent);

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
        let consequent_type = self.exprs.get_type(consequent);

        let alternate = if let Some(parsed_alt) = if_expr.alt {
            let type_hint = if cons_never { ctx.expected_type_id } else { Some(consequent_type) };
            self.eval_expr(parsed_alt, ctx.with_expected_type(type_hint))?
        } else {
            self.synth_unit(if_expr.span)
        };
        let alternate_type = self.exprs.get_type(alternate);
        let alternate_span = self.exprs.get_span(alternate);

        let cons_never = consequent_type == NEVER_TYPE_ID; // By now, consequent could be 'unit'
        let alt_never = alternate_type == NEVER_TYPE_ID;
        let no_never = !cons_never && !alt_never;

        let overall_type = if no_never {
            consequent_type
        } else {
            if cons_never { alternate_type } else { consequent_type }
        };

        let alternate = if no_never {
            self.check_and_coerce_expr(consequent_type, alternate, ctx.scope_id).map_err(|e| {
                errf!(
                    alternate_span,
                    "else branch type did not match then branch type: {}",
                    e.message,
                )
            })?
        } else {
            alternate
        };

        let cons_arm = TypedMatchArm { condition, consequent_expr: consequent };
        let alt_arm = TypedMatchArm {
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: alternate,
        };
        Ok(self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                initial_let_statements: MSlice::empty(),
                arms: self.mem.pushn(&[cons_arm, alt_arm]),
            }),
            overall_type,
            if_expr.span,
        ))
    }

    fn eval_matching_condition(
        &mut self,
        condition: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, MatchingCondition>> {
        debug!("matching condition: {}", self.ast.expr_id_to_string(condition));
        let mut all_patterns: SmallVec<[TypedPatternId; 1]> = smallvec![];
        let mut allow_bindings: bool = true;
        let mut instrs: MList<MatchingConditionInstr, _> = self.mem.new_list(16);
        self.handle_matching_condition_rec(
            condition,
            &mut allow_bindings,
            &mut all_patterns,
            &mut instrs,
            ctx,
        )?;

        let mut all_bindings: SmallVec<[VariablePattern; 8]> = smallvec![];
        for pattern in all_patterns.iter() {
            self.patterns.get_pattern_bindings_rec(*pattern, &mut all_bindings);
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

        let diverges_at = self.matching_condition_diverges(&instrs);
        if let Some(diverge_index) = diverges_at {
            let condition_span = self.ast.get_expr_span(condition);
            let never_block = self.make_never_condition_block(
                &instrs[0..=diverge_index],
                ctx.scope_id,
                condition_span,
            );
            Ok(Either::Left(never_block))
        } else {
            Ok(Either::Right(MatchingCondition { instrs: self.mem.pushn(&instrs) }))
        }
    }

    fn matching_condition_diverges(&self, instrs: &[MatchingConditionInstr]) -> Option<usize> {
        for (index, instr) in instrs.iter().enumerate() {
            match instr {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    if self.get_stmt_type(*let_stmt) == NEVER_TYPE_ID {
                        return Some(index);
                    }
                }
                MatchingConditionInstr::Cond { value } => {
                    if self.exprs.get_type(*value) == NEVER_TYPE_ID {
                        return Some(index);
                    }
                }
            }
        }
        None
    }

    /// Handles chains of booleans and pattern statements (IsExprs).
    /// Does so by compiling the patterns (or boolean conditions) contained
    /// in them into either `conditions` or `bindings`.
    /// Conditions are boolean expressions that, after failing, exit the matching condition.
    /// Bindings are let statements
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
        all_patterns: &mut SmallVec<[TypedPatternId; 1]>,
        instrs: &mut MList<MatchingConditionInstr, TypedProgram>,
        ctx: EvalExprContext,
    ) -> TyperResult<()> {
        debug!("hmirec: {}", self.ast.expr_id_to_string(parsed_expr_id));
        match self.ast.exprs.get(parsed_expr_id) {
            ParsedExpr::Is(is_expr) => {
                let target_expr = is_expr.target_expression;
                let pattern = is_expr.pattern;
                let target = self.eval_expr(target_expr, ctx)?;
                let target_type = self.exprs.get_type(target);
                let target_var = self.synth_variable_defn_simple(
                    self.ast.idents.b.if_target,
                    target,
                    ctx.scope_id,
                );
                let pattern =
                    self.eval_pattern(pattern, target_type, ctx.scope_id, *allow_bindings)?;
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::Binding { let_stmt: target_var.defn_stmt },
                );
                self.compile_pattern_into_values(
                    pattern,
                    target_var.variable_expr,
                    instrs,
                    false,
                    ctx,
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
                let condition_type = self.exprs.get_type(condition);
                if let Err(msg) = self.check_types(BOOL_TYPE_ID, condition_type, ctx.scope_id) {
                    return failf!(span, "Expected boolean condition: {msg}");
                };
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::Cond { value: condition });
                Ok(())
            }
        }
    }

    fn eval_standalone_matching_condition(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let condition_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, None, None);
        let condition_ctx = ctx.with_scope(condition_scope).with_no_expected_type();
        let matching_condition = match self.eval_matching_condition(expr_id, condition_ctx)? {
            Either::Left(block) => return Ok(block),
            Either::Right(mc) => mc,
        };
        let span = self.ast.exprs.get_span(expr_id);
        let true_arm = TypedMatchArm {
            condition: matching_condition,
            consequent_expr: self.synth_bool(true, span),
        };
        let false_arm = TypedMatchArm {
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: self.synth_bool(false, span),
        };
        let match_expr = TypedExpr::Match(TypedMatchExpr {
            initial_let_statements: MSlice::empty(),
            arms: self.mem.pushn(&[true_arm, false_arm]),
        });
        Ok(self.exprs.add(match_expr, BOOL_TYPE_ID, span))
    }

    fn eval_binary_op(
        &mut self,
        binary_op_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };
        use BinaryOpKind as K;
        match binary_op.op_kind {
            K::Pipe => self.eval_pipe_expr(binary_op.lhs, binary_op.rhs, ctx, binary_op.span),
            K::OptionalElse => {
                self.eval_optional_else(binary_op.lhs, binary_op.rhs, ctx, binary_op.span)
            }
            K::And => {
                let lhs_is = match self.ast.exprs.get(binary_op.lhs) {
                    ParsedExpr::Is(_) => true,
                    _ => false,
                };
                if lhs_is {
                    self.eval_standalone_matching_condition(binary_op_id, ctx)
                } else {
                    let lhs = self.eval_expr_with_coercion(
                        binary_op.lhs,
                        ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                        true,
                    )?;
                    let rhs = self.eval_expr_with_coercion(
                        binary_op.rhs,
                        ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                        true,
                    )?;
                    let false_expr = self.synth_bool(false, binary_op.span);
                    let and =
                        self.synth_if_else(BOOL_TYPE_ID, lhs, rhs, false_expr, binary_op.span);
                    Ok(and)
                }
            }
            K::Or => {
                let lhs = self.eval_expr_with_coercion(
                    binary_op.lhs,
                    ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                    true,
                )?;
                let rhs = self.eval_expr_with_coercion(
                    binary_op.rhs,
                    ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                    true,
                )?;
                let true_expr = self.synth_bool(true, binary_op.span);
                let or = self.synth_if_else(BOOL_TYPE_ID, lhs, true_expr, rhs, binary_op.span);
                Ok(or)
            }
            // We convert most binary ops into ability function calls by rewriting to parsed calls
            // and compiling the code
            K::Equals | K::NotEquals => self.eval_equality_expr(binary_op_id, ctx),
            K::Add
            | K::Subtract
            | K::Multiply
            | K::Divide
            | K::Rem
            | K::Less
            | K::LessEqual
            | K::Greater
            | K::GreaterEqual => {
                let fn_ident = match binary_op.op_kind {
                    K::Add => self.ast.idents.f.Add_add,
                    K::Subtract => self.ast.idents.f.Sub_sub,
                    K::Multiply => self.ast.idents.f.Mul_mul,
                    K::Divide => self.ast.idents.f.Div_div,
                    K::Rem => self.ast.idents.f.Rem_rem,
                    K::Less => self.ast.idents.f.ScalarCmp_lt,
                    K::LessEqual => self.ast.idents.f.ScalarCmp_le,
                    K::Greater => self.ast.idents.f.ScalarCmp_gt,
                    K::GreaterEqual => self.ast.idents.f.ScalarCmp_ge,
                    _ => unreachable!(),
                };
                self.synth_typed_call_parsed_args(
                    fn_ident.with_span(binary_op.span),
                    &[],
                    &[binary_op.lhs, binary_op.rhs],
                    ctx,
                )
            }
        }
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
        let lhs_type = self.exprs.get_type(lhs);
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
        let rhs_type = self.exprs.get_type(rhs);
        if let Err(msg) = self.check_types(output_type, rhs_type, ctx.scope_id) {
            return failf!(span, "RHS value incompatible with `Unwrap` output of LHS: {}", msg);
        }
        let mut coalesce_block = self.synth_block(ctx.scope_id, ScopeType::LexicalBlock, span, 2);
        let lhs_variable = self.synth_variable_defn_simple(
            get_ident!(self, "optelse_lhs"),
            lhs,
            coalesce_block.scope_id,
        );
        let coalesce_ctx = ctx.with_scope(coalesce_block.scope_id).with_no_expected_type();
        let lhs_has_value = self.synth_typed_call_typed_args(
            self.ast.idents.f.Unwrap_hasValue.with_span(span),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
            false,
        )?;
        let lhs_get_expr = self.synth_typed_call_typed_args(
            self.ast.idents.f.Unwrap_unwrap.with_span(span),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
            false,
        )?;

        let if_else = self.synth_if_else(output_type, lhs_has_value, lhs_get_expr, rhs, span);
        self.push_block_stmt_id(&mut coalesce_block, lhs_variable.defn_stmt);
        self.push_expr_id_to_block(&mut coalesce_block, if_else);
        Ok(self.exprs.add_block(&mut self.mem, coalesce_block, output_type))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> TyperResult<TypedExprId> {
        let ParsedExpr::BinaryOp(binary_op) = self.ast.exprs.get(binary_op_id).clone() else {
            unreachable!()
        };

        let parsed_equals_call = self.synth_parsed_function_call(
            self.ast.idents.f.Equals_equals.with_span(binary_op.span),
            &[],
            &[binary_op.lhs, binary_op.rhs],
            false,
        );
        let call = self.ast.exprs.get(parsed_equals_call).expect_call().clone();
        let equality_result =
            self.eval_function_call(&call, None, ctx.with_expected_type(Some(BOOL_TYPE_ID)), None)?;
        let final_result = match binary_op.op_kind {
            BinaryOpKind::Equals => equality_result,
            BinaryOpKind::NotEquals => self.synth_typed_call_typed_args(
                self.ast.idents.f.bool_negated.with_span(binary_op.span),
                &[],
                &[equality_result],
                ctx.with_no_expected_type(),
                false,
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
                    name: var.name,
                    type_args: SliceHandle::empty(),
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
                let args_with_piped = self.ast.p_call_args.add_slice_copy(&args);
                ParsedCall {
                    name: fn_call.name,
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
        let new_fn_call_id =
            self.ast.exprs.add_expression(ParsedExpr::Call(new_fn_call), false, None);
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
                            .unwrap();
                        let ability_impl_function = self.solve_ability_call(
                            function_ability_index,
                            fn_call,
                            known_args,
                            ctx,
                        )?;
                        Ok(Either::Right(Callee::from_ability_impl_fn(&ability_impl_function)))
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
                    if !fn_call.name.path.is_empty() {
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
                                function_id: lambda_type.function_id,
                                lambda_value_expr: self.exprs.add(
                                    TypedExpr::Variable(VariableExpr { variable_id }),
                                    function_variable.type_id,
                                    fn_call.span,
                                ),
                                lambda_type_id: function_variable.type_id,
                            })),
                            Type::LambdaObject(_lambda_object) => {
                                Ok(Either::Right(Callee::DynamicLambda(self.exprs.add(
                                    TypedExpr::Variable(VariableExpr { variable_id }),
                                    function_variable.type_id,
                                    fn_call.name.span,
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
                            Type::FunctionPointer(_function_pointer) => {
                                let function_pointer_expr = self.exprs.add(
                                    TypedExpr::Variable(VariableExpr { variable_id }),
                                    function_variable.type_id,
                                    fn_call.name.span,
                                );
                                Ok(Either::Right(Callee::DynamicFunction { function_pointer_expr }))
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
        match self.scopes.enclosing_functions.get(scope_id) {
            ScopeEnclosingFunctions { lambda: Some(lambda_scope), .. } => {
                let Some(expected_return_type) =
                    self.scopes.get_lambda_info(*lambda_scope).expected_return_type
                else {
                    return failf!(
                        span,
                        "Closure must have explicit return type, or known return type from context, to use early returns."
                    );
                };
                Ok(expected_return_type)
            }
            ScopeEnclosingFunctions { function: Some(function_id), .. } => {
                let expected_return_type = self.get_function_type(*function_id).return_type;
                Ok(expected_return_type)
            }
            _ => failf!(span, "No parent function; cannot return"),
        }
    }

    fn eval_return(
        &mut self,
        parsed_expr: Option<ParsedExprId>,
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
        let return_value = match parsed_expr {
            None => self.synth_unit(span),
            Some(parsed_expr) => self.eval_expr_with_coercion(
                parsed_expr,
                ctx.with_expected_type(expected_return_type),
                true,
            )?,
        };
        let return_value_type = self.exprs.get_type(return_value);
        if return_value_type == NEVER_TYPE_ID {
            return failf!(
                span,
                "return is dead since returned expression is divergent; remove the return"
            );
        }
        let mut gathered_defers: SV4<ParsedExprId> = smallvec![];
        let mut search_scope_id = ctx.scope_id;
        loop {
            if let Some(defers) = self.scopes.block_defers.get(&search_scope_id) {
                gathered_defers.extend(defers.deferred_exprs.iter().copied());
            }
            let current_scope = self.scopes.get_scope(search_scope_id);
            if current_scope.scope_type.is_top_of_function() {
                break;
            } else {
                if let Some(parent_scope_id) = current_scope.parent {
                    search_scope_id = parent_scope_id;
                } else {
                    self.ice_with_span(
                        "Recursed all the way up without finding a Function or Lambda scope",
                        span,
                    )
                }
            }
        }
        let return_expr = self.exprs.add_return(return_value, span);
        if gathered_defers.is_empty() {
            Ok(return_expr)
        } else {
            let mut block = self.synth_block(
                ctx.scope_id,
                ScopeType::LexicalBlock,
                span,
                gathered_defers.len() as u32 + 1,
            );
            // No need to reverse; they are already in fifo order due to the way
            // we traverse upwards when gathering them
            for deferred_parsed_expr in gathered_defers.into_iter() {
                let deferred_expr =
                    self.eval_expr(deferred_parsed_expr, ctx.with_no_expected_type())?;
                let deferred_expr_type_id = self.exprs.get_type(deferred_expr);
                self.push_block_stmt(
                    &mut block,
                    TypedStmt::Expr(deferred_expr, deferred_expr_type_id),
                );
            }
            self.push_block_stmt(&mut block, TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
            Ok(self.exprs.add_block(&mut self.mem, block, NEVER_TYPE_ID))
        }
    }

    ////////////////////////////////////////
    // Handling function calls and function-call lookalikes

    fn handle_builtin_function_call_lookalikes(
        &mut self,
        fn_call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> TyperResult<Option<TypedExprId>> {
        let call_span = fn_call.span;
        let calling_scope = ctx.scope_id;
        match self.ident_str(fn_call.name.name) {
            "return" => {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    return failf!(fn_call.span, "return cannot be used inside `defer` blocks");
                }
                let ret_value = match fn_call.args.len() {
                    0 => Ok(None),
                    1 => {
                        let arg = self.ast.p_call_args.get_first(fn_call.args).unwrap();
                        Ok(Some(arg.value))
                    }
                    _ => failf!(fn_call.span, "return(...) must have 0 or 1 arguments"),
                }?;
                let return_expr_id = self.eval_return(ret_value, ctx, call_span)?;
                Ok(Some(return_expr_id))
            }
            "break" => {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    return failf!(fn_call.span, "break cannot be used inside `defer` blocks");
                }
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
                    None => self.synth_unit(call_span),
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
                let actual_break_type = self.exprs.get_type(break_value);
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

                Ok(Some(self.exprs.add(
                    TypedExpr::Break(TypedBreak {
                        value: break_value,
                        loop_scope: enclosing_loop_scope_id,
                        loop_type,
                    }),
                    NEVER_TYPE_ID,
                    call_span,
                )))
            }
            "continue" => {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    return failf!(fn_call.span, "continue cannot be used inside `defer` blocks");
                }
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
                        let string_expr = self.synth_string_literal(string_id, call_span);
                        self.synth_optional_some(string_expr).0
                    }
                    Ok(_expr) => self.synth_optional_none(STRING_TYPE_ID, call_span),
                };
                Ok(Some(expr))
            }
            _ => Ok(None),
        }
    }

    fn handle_array_method_call(
        &mut self,
        base: TypedExprId,
        array_type_id: TypeId,
        call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        let span = call.span;
        let array_type = self.types.get(array_type_id).as_array().unwrap();
        match call.name.name {
            n if n == self.ast.idents.b.len => {
                let array_length = match array_type.concrete_count {
                    None => {
                        // We have some generic Array type like arr: Array[T, N] where N is a type
                        // parameter
                        // And someone called arr.len. The value does not matter as it will never
                        // be seen at runtime, or even compile-time since we don't execute during
                        // the 'generic' pass. So we just provide a validly-typed value of type 'N'.
                        // We do it with a transmute cast
                        let unit = self.synth_unit(span);
                        self.synth_cast(unit, array_type.size_type, CastType::Transmute, None)
                    }
                    Some(s) => self.synth_i64(s as i64, span),
                };
                Ok(Either::Left(array_length))
            }
            n if n == self.ast.idents.b.get || n == self.ast.idents.b.getRef => {
                if call.args.len() != 2 {
                    return failf!(span, "Array get takes 1 argument, the index");
                }
                let index_arg = self.ast.p_call_args.get_nth(call.args, 1);
                let index_expr =
                    self.eval_expr(index_arg.value, ctx.with_expected_type(Some(SIZE_TYPE_ID)))?;
                let index_expr = self
                    .check_and_coerce_expr(SIZE_TYPE_ID, index_expr, ctx.scope_id)
                    .map_err(|e| errf!(span, "Array get index type error: {}", e.message))?;

                let is_referencing = n == self.ast.idents.b.getRef;
                let array_reference_type = self.get_expr_type(base).as_reference();
                if is_referencing && array_reference_type.is_none() {
                    return failf!(
                        span,
                        "Cannot use .getRef() on this Array since it is not a reference: {}",
                        self.type_id_to_string(self.exprs.get_type(base))
                    );
                }
                let access_kind = if array_reference_type.is_some() {
                    if is_referencing {
                        FieldAccessKind::ReferenceThrough
                    } else {
                        FieldAccessKind::Dereference
                    }
                } else {
                    FieldAccessKind::ValueToValue
                };
                let result_type = if is_referencing {
                    let mutable = array_reference_type.unwrap().mutable;
                    self.types.add_reference_type(array_type.element_type, mutable)
                } else {
                    array_type.element_type
                };
                if let Ok(static_index_expr) = self.attempt_static_lift(index_expr) {
                    let static_index_type = self.exprs.get_type(static_index_expr);
                    if let Some(index_usize) =
                        self.get_value_of_static_type(static_index_type).and_then(|sv| sv.as_size())
                    {
                        if let Some(concrete_size) = array_type.concrete_count {
                            if index_usize as u64 >= concrete_size {
                                return failf!(
                                    span,
                                    "Array index out of bounds: {} >= {}",
                                    index_usize,
                                    concrete_size
                                );
                            }
                        }
                    }
                }
                let get_element_expr = self.exprs.add(
                    TypedExpr::ArrayGetElement(ArrayGetElement {
                        base,
                        index: index_expr,
                        array_type: array_type_id,
                        access_kind,
                    }),
                    result_type,
                    span,
                );
                let array_length_expr = self.synth_typed_call_typed_args(
                    QIdent::naked(self.ast.idents.b.len, span),
                    &[],
                    &[base],
                    ctx.with_no_expected_type(),
                    true,
                )?;
                let is_in_bounds = self.synth_typed_call_typed_args(
                    self.ast.idents.f.ScalarCmp_lt,
                    &[],
                    &[index_expr, array_length_expr],
                    ctx,
                    false,
                )?;
                let string_id = self.ast.strings.intern("Array index out of bounds");
                let crash_message = self.synth_string_literal(string_id, span);
                let crash_oob = self.synth_typed_call_typed_args(
                    self.ast.idents.f.core_crashBounds.with_span(span),
                    &[],
                    &[array_length_expr, index_expr, crash_message],
                    ctx.with_no_expected_type(),
                    false,
                )?;
                let if_else_expr = self.synth_if_else(
                    result_type,
                    is_in_bounds,
                    get_element_expr,
                    crash_oob,
                    span,
                );
                Ok(Either::Left(if_else_expr))
            }
            _ => {
                let array_scope = self.scopes.get_scope(self.scopes.array_scope_id);
                if let Some(method_id) = array_scope.find_function(call.name.name) {
                    Ok(Either::Right(Callee::make_static(method_id)))
                } else {
                    failf!(span, "No such method on Array: {}", self.qident_to_string(&call.name))
                }
            }
        }
    }

    fn resolve_parsed_function_call_method(
        &mut self,
        base_expr: MaybeTypedExpr,
        call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<Either<TypedExprId, Callee>> {
        debug_assert!(call.name.path.is_empty());
        let fn_name = call.name.name;
        let call_span = call.span;

        let args = self.ast.p_call_args.get_slice(call.args);
        let first_arg = args.first().copied();
        let second_arg = args.get(1).copied();

        // Special cases of this syntax that aren't really method calls
        if let Some(base_arg) = first_arg {
            if let Some(enum_constr) = self.handle_enum_constructor(
                Some(base_arg.value),
                fn_name,
                second_arg.map(|param| param.value),
                call.type_args,
                ctx,
                call.span,
            )? {
                return Ok(Either::Left(enum_constr));
            }

            let is_fn_convert =
                fn_name == self.ast.idents.b.toRef || fn_name == self.ast.idents.b.toDyn;
            if is_fn_convert {
                // TODO: this isn't algebraically sound since you can _only_ use toRef and toDyn
                //       if you literally name the function on the lhs of the dot; you can't store
                //       it in a variable, because the function name on its own isn't a valid
                //       expression. So it's not the most satisfying, but it works for now.
                //
                //       I think this is a fair compromise; I don't see the value in putting a
                //       'function' in a variable abstractly only to toRef() it later
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
                                "Cannot call toDyn or toRef with a generic function"
                            );
                        }
                        if function.intrinsic_type.is_some_and(|t| t.is_inlined()) {
                            return failf!(
                                call_span,
                                "Cannot call toDyn or toRef with an intrinsic operation"
                            );
                        }
                        return if fn_name == self.ast.idents.b.toDyn {
                            Ok(Either::Left(self.function_to_lambda_object(function_id, call_span)))
                        } else if fn_name == self.ast.idents.b.toRef {
                            Ok(Either::Left(self.function_to_reference(function_id, call_span)))
                        } else {
                            unreachable!()
                        };
                    }
                }
            } else if fn_name == self.ast.idents.b.toStatic {
                if call.args.len() != 1 {
                    return failf!(call_span, ".toStatic() takes no additional arguments");
                }
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                return match self.attempt_static_lift(base_value) {
                    Err(msg) => {
                        return failf!(call_span, "Failed to lift value to static: {}", msg);
                    }
                    Ok(static_expr_id) => Ok(Either::Left(static_expr_id)),
                };
            } else if fn_name == self.ast.idents.b.fromStatic {
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                let base_type_id = self.exprs.get_type(base_value);
                let Some(static_type) = self.types.get_static_type_of_type(base_type_id) else {
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
                    Some(call_span),
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
        if let Some(enum_as_result) =
            self.handle_enum_as(base_expr, call, ctx.with_no_expected_type())?
        {
            return Ok(Either::Left(enum_as_result));
        }

        let base_expr_type = self.exprs.get_type(base_expr);
        let base_for_method = self.types.get_base_for_method(base_expr_type);

        if let Type::Array(_array_type) = self.types.get(base_for_method) {
            return self.handle_array_method_call(base_expr, base_for_method, call, ctx);
        }

        if let Some(companion_ns) =
            self.types.get_defn_info(base_for_method).and_then(|d| d.companion_namespace)
        {
            let companion_scope = self.get_namespace_scope(companion_ns);
            debug!(
                "functions in companion scope: {:?}",
                companion_scope
                    .functions
                    .values()
                    .map(|f| self.ident_str(self.functions.get(*f).name).to_string())
                    .join(", ")
            );
            if let Some(method_id) = companion_scope.find_function(fn_name) {
                return Ok(Either::Right(Callee::make_static(method_id)));
            }
        } else {
            debug!("companion scope not found for call to {}", self.ident_str(fn_name))
        };

        let Some(abilities_for_function) = self.function_name_to_ability.get(&fn_name) else {
            return failf!(
                call_span,
                "Method '{}' does not exist on type: '{}'",
                self.ident_str(call.name.name),
                self.type_id_to_string(base_expr_type),
            );
        };

        let mut abilities_in_scope = FxHashSet::new();
        self.scopes.find_abilities_in_scope(&mut abilities_in_scope, ctx.scope_id);
        debug!(
            "abilities_in_scope: {:?}",
            abilities_in_scope
                .iter()
                .map(|a| self.ident_str(self.abilities.get(*a).name))
                .collect::<Vec<_>>()
        );

        let mut errors: SV4<TyperError> = smallvec![];
        for ability_id in abilities_for_function.clone().iter() {
            let in_scope = abilities_in_scope.contains(ability_id);
            if in_scope {
                let ability_function_ref =
                    self.abilities.get(*ability_id).find_function_by_name(fn_name).unwrap();
                match self.solve_ability_call(ability_function_ref, call, known_args, ctx) {
                    Ok(ability_impl_fn) => {
                        return Ok(Either::Right(Callee::from_ability_impl_fn(&ability_impl_fn)));
                    }
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                }
            }
        }
        if errors.is_empty() {
            failf!(
                call_span,
                "Method '{}' does not exist on type: '{}'",
                self.ident_str(call.name.name),
                self.type_id_to_string(base_expr_type),
            )
        } else {
            Err(errors.into_iter().next().unwrap())
        }
    }

    pub fn function_to_reference(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let function_pointer_type = self.types.add_function_pointer_type(function.type_id);
        self.exprs.add(
            TypedExpr::FunctionPointer(FunctionPointerExpr { function_id }),
            function_pointer_type,
            call_span,
        )
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
            let new_function_id = self.functions.next_id();

            let empty_env_variable = self.variables.add(Variable {
                name: self.ast.idents.b.lambda_env_var_name,
                type_id: POINTER_TYPE_ID,
                // Wrong scope, and its not actually added, but we know its not used
                owner_scope: new_function.scope,
                kind: VariableKind::FnParam(new_function_id),
                flags: VariableFlags::empty(),
                usage_count: 0,
            });
            let mut new_variables = self.mem.new_list(new_function.param_variables.len() + 1);
            new_variables.push(empty_env_variable);
            new_variables.extend(self.mem.getn(new_function.param_variables));
            new_function.param_variables = self.mem.vec_to_mslice(&new_variables);

            let new_function_type =
                self.add_lambda_env_to_function_type(new_function.type_id, function_defn_span);
            new_function.type_id = new_function_type;
            let old_name = self.ident_str(new_function.name);
            new_function.name = self.ast.idents.intern(format!("{}__dyn", old_name));
            let actual_new_function_id = self.add_function(new_function);
            debug_assert_eq!(actual_new_function_id, new_function_id);
            self.get_function_mut(function_id).dyn_fn_id = Some(new_function_id);
            //bc::compile_function(self, new_function_id)
            //    .unwrap_or_else(|e| self.ice_with_span(e.message, e.span));
            new_function_id
        };
        let dyn_function = self.get_function(dyn_function_id);
        let lambda_object_type_id = self.types.add_lambda_object(
            &self.ast.idents,
            dyn_function.type_id,
            dyn_function.parsed_id,
        );
        let function_to_lam_obj_id = self.exprs.add(
            TypedExpr::FunctionToLambdaObject(FunctionToLambdaObjectExpr {
                function_id: dyn_function_id,
            }),
            lambda_object_type_id,
            call_span,
        );
        function_to_lam_obj_id
    }

    fn add_lambda_env_to_function_type(
        &mut self,
        function_type_id: TypeId,
        span: SpanId,
    ) -> TypeId {
        let function_type = self.types.get(function_type_id).as_function().unwrap();
        let return_type = function_type.return_type;
        let physical_params = function_type.physical_params;
        let empty_env_struct_type = self.types.add_empty_struct();
        let empty_env_struct_ref = self.types.add_reference_type(empty_env_struct_type, false);
        let mut new_params = self.types.mem.new_list(physical_params.len() + 1);

        new_params.push(FnParamType {
            name: self.ast.idents.b.lambda_env_var_name,
            type_id: empty_env_struct_ref,
            is_context: false,
            is_lambda_env: true,
            span,
        });
        new_params.extend(self.types.mem.getn(physical_params));

        let new_function_type = FunctionType {
            physical_params: self.types.mem.vec_to_mslice(&new_params),
            return_type,
            is_lambda: true,
        };

        let defn_info = self.types.get_defn_info(function_type_id);
        self.types.add(Type::Function(new_function_type), defn_info, None)
    }

    /// After resolving to a particular root AbilityId + function index using just names,
    /// we have to use the information in the call to 'solve' for Self using the rest
    /// of the information available in the `ParsedCall`, and ultimately come back with either
    /// an error, such as 'couldnt solve', 'not implemented' or: an exact physical function id of the correct AbilityImpl
    fn solve_ability_call(
        &mut self,
        ability_function_ref: TypedAbilityFunctionRef,
        fn_call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
    ) -> TyperResult<AbilityImplFunction> {
        let call_span = fn_call.span;
        let function_type_id = self.get_function(ability_function_ref.function_id).type_id;
        let base_ability_id = ability_function_ref.ability_id;
        let ability_fn_type = self.types.get(function_type_id).as_function().unwrap().clone();
        let ability_fn_return_type = ability_fn_type.return_type;
        let ability_params = self.abilities.get(base_ability_id).parameters.clone();
        let ability_self_type_id = self.abilities.get(base_ability_id).self_type_id;

        let passed_len = known_args.map(|ka| ka.1.len()).unwrap_or(fn_call.args.len());
        if passed_len != ability_fn_type.logical_params().len() as usize {
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
            type_params
                .push(NameAndType { name: self.ast.idents.b.Self_, type_id: ability_self_type_id });
            type_params.extend(
                ability_params
                    .iter()
                    .map(|p| NameAndType { name: p.name, type_id: p.type_variable_id }),
            );
            type_params
        };
        let all_type_params_handle = self.named_types.add_slice_copy(&all_type_params);
        let must_solve_type_params: SmallVec<[NameAndType; 8]> = {
            let mut type_params = SmallVec::with_capacity(ability_params.len() + 1);
            type_params
                .push(NameAndType { name: self.ast.idents.b.Self_, type_id: ability_self_type_id });
            type_params.extend(
                ability_params
                    .iter()
                    .filter(|p| !p.is_impl_param)
                    .map(|p| NameAndType { name: p.name, type_id: p.type_variable_id }),
            );
            type_params
        };
        let must_solve_type_params_handle =
            self.named_types.add_slice_copy(&must_solve_type_params);

        let self_only_type_params_handle = self.named_types.add_slice_copy(&[NameAndType {
            name: self.ast.idents.b.Self_,
            type_id: ability_self_type_id,
        }]);

        let passed_args: &mut dyn Iterator<Item = MaybeTypedExpr> = match known_args {
            Some(ka) => &mut ka.1.iter().map(|t| MaybeTypedExpr::Typed(*t)),
            None => {
                let args = self.ast.p_call_args.get_slice(fn_call.args);
                &mut args.iter().map(|arg| MaybeTypedExpr::Parsed(arg.value))
            }
        };
        let mut args_and_params = self.tmp.new_list(passed_len as u32 + 1);
        if let Some(expected_type) = ctx.expected_type_id {
            args_and_params.push(InferenceInputPair {
                arg: TypeOrParsedExpr::Type(expected_type),
                param_type: ability_fn_return_type,
                allow_mismatch: false,
            });
        }
        for (arg, param) in passed_args.zip(self.types.mem.getn(ability_fn_type.logical_params())) {
            let arg_and_param = match arg {
                MaybeTypedExpr::Typed(expr) => {
                    let type_id = self.exprs.get_type(expr);
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
            &all_type_params,
            self_only_type_params_handle,
            &args_and_params,
            fn_call.span,
            ctx.scope_id,
        )?;
        let mut parameter_constraints: MList<Option<TypeId>, MemTmp> =
            self.tmp.new_list(ability_params.len() as u32);
        for ab_param in &ability_params {
            if ab_param.is_impl_param {
                continue;
            }
            let solution =
                self.named_types.get_slice(other_solved).iter().find(|nt| nt.name == ab_param.name);
            parameter_constraints.push(solution.map(|nt| nt.type_id));
        }
        let solved_self = self.named_types.get_nth(self_only, 0).type_id;

        // Follow 'statics' since we're never going to be implementing abilities for the
        // specific static values but instead for the inner types
        let solved_self = match self.types.get_no_follow(solved_self) {
            Type::Static(stat) => stat.inner_type_id,
            _ => solved_self,
        };
        let impl_handle = self
            .find_ability_impl_for_type_or_generate_new(
                solved_self,
                base_ability_id,
                &parameter_constraints,
                ctx.scope_id,
                call_span,
            )
            .map_err(|msg| {
                errf!(
                    call_span,
                    "Call to {}/{} with Self := {} does not work\n{}\nFunction type: {}",
                    self.ability_impl_signature_to_string(base_ability_id, SliceHandle::empty()),
                    self.ident_str(fn_call.name.name),
                    self.type_id_to_string(solved_self),
                    msg,
                    self.type_id_to_string(function_type_id),
                )
            })?;

        let impl_function = self
            .ability_impls
            .get(impl_handle.full_impl_id)
            .function_at_index(&self.mem, ability_function_ref.index);
        Ok(*impl_function)
    }

    fn handle_enum_get_tag(
        &mut self,
        base_expr_id: TypedExprId,
        span: SpanId,
    ) -> TyperResult<Option<TypedExprId>> {
        let original_type = self.exprs.get_type(base_expr_id);
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
        Ok(Some(self.exprs.add(
            TypedExpr::EnumGetTag(GetEnumTag { enum_expr_or_reference: base_expr }),
            tag_type,
            span,
        )))
    }

    fn handle_enum_as(
        &mut self,
        base_expr: TypedExprId,
        fn_call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> TyperResult<Option<TypedExprId>> {
        let fn_name = self.ident_str(fn_call.name.name);
        let preconditions =
            fn_name.starts_with("as") && fn_call.type_args.is_empty() && fn_call.args.len() == 1;
        if !preconditions {
            return Ok(None);
        }
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
        let base_expr_type = self.exprs.get_type(base_expr);
        let span = fn_call.span;
        let variants = e.variants.clone();
        let mut s = std::mem::take(&mut self.buffers.name_builder);
        let Some(variant) = variants.iter().find(|v| {
            s.push_str("as");
            let name = self.ident_str(v.name);
            let name_capitalized = strings::capitalize_first(name);
            s.push_str(&name_capitalized);

            let fn_name = self.ident_str(fn_call.name.name);
            let is_match = fn_name == s;
            s.clear();
            is_match
        }) else {
            self.buffers.name_builder = s;
            return Ok(None);
        };
        self.buffers.name_builder = s;
        let variant_type_id = variant.my_type_id;
        let variant_index = variant.index;
        let resulting_type_id = if is_reference {
            let ref_type = self.types.get(base_expr_type).expect_reference();
            self.types.add_reference_type(variant_type_id, ref_type.mutable)
        } else {
            variant_type_id
        };
        let condition = self.synth_enum_is_variant(base_expr, variant_index, ctx, Some(span))?;
        let cast_type =
            if is_reference { CastType::ReferenceToReference } else { CastType::EnumToVariant };
        let cast_expr = self.synth_cast(base_expr, resulting_type_id, cast_type, Some(span));
        let (consequent, consequent_type_id) =
                // base_expr is the enum type, or a reference to it
                // resulting_type_id is the variant, or a reference to it
                self.synth_optional_some(cast_expr);
        let alternate = self.synth_optional_none(resulting_type_id, span);

        Ok(Some(self.synth_if_else(consequent_type_id, condition, consequent, alternate, span)))
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
                let base_enum_type_id = match self.types.get_instance_info(enum_type_id) {
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
                        match ctx.expected_type_id.map(|t| (t, self.types.get_instance_info(t))) {
                            Some((expected_type, Some(spec_info))) => {
                                // We're expecting a specific instance of a generic enum
                                if spec_info.generic_parent == base_type_id {
                                    // Solved params
                                    let solved_params_iter: SV4<NameAndType> = self
                                        .named_types
                                        .get_slice(g_params)
                                        .iter()
                                        .zip(self.types.mem.getn(spec_info.type_args))
                                        .map(|(g_param, expected_specialized_type)| NameAndType {
                                            name: g_param.name,
                                            type_id: *expected_specialized_type,
                                        })
                                        .collect();
                                    self.named_types.add_slice_copy(&solved_params_iter)
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
                        let g_params_owned = self.named_types.copy_slice_sv8(g_params);
                        let (solutions, _all_solutions) = self.infer_types(
                            &g_params_owned,
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
                let type_args_owned = self.ast.p_type_args.copy_slice_sv4(type_args);
                for (generic_param, passed_type_arg) in
                    self.named_types.copy_slice_sv4(g_params).iter().zip(type_args_owned.iter())
                {
                    let Some(passed_type_expr) = passed_type_arg.type_expr else {
                        return failf!(span, "Wildcard type _ is not yet supported here");
                    };
                    let type_id = self.eval_type_expr(passed_type_expr, ctx.scope_id)?;
                    passed_params.push(NameAndType { name: generic_param.name, type_id });
                }
                self.named_types.add_slice_copy(&passed_params)
            };

            let passed_type_ids = self.types.mem.pushn_iter(
                self.named_types
                    .get_slice(solved_or_passed_type_params)
                    .iter()
                    .map(|type_param| type_param.type_id),
            );
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

    fn align_call_arguments_with_parameters(
        &mut self,
        fn_call: &ParsedCall,
        params: MSlice<FnParamType, TypePool>,
        pre_evaled_params: Option<&[TypedExprId]>,
        calling_scope: ScopeId,
        tolerate_missing_context_args: bool,
    ) -> TyperResult<ArgsAndParams> {
        let fn_name = fn_call.name.name;
        let span = fn_call.span;
        let args_slice = self.ast.p_call_args.get_slice(fn_call.args);
        let explicit_context_args = args_slice.iter().any(|a| a.is_explicit_context);
        let named = args_slice.first().is_some_and(|arg| arg.name.is_some());
        let mut final_args: SV8<MaybeTypedExpr> = SmallVec::new();
        let mut final_params: SV8<FnParamType> = SmallVec::new();
        if !explicit_context_args {
            for context_param in self.types.mem.getn(params).iter().filter(|p| p.is_context) {
                let matching_context_variable =
                    self.scopes.find_context_variable_by_type(calling_scope, context_param.type_id);
                if let Some(matching_context_variable) = matching_context_variable {
                    let found = self.variables.get(matching_context_variable);
                    // nocommit 6: Add context ability signatures
                    final_args.push(MaybeTypedExpr::Typed(self.exprs.add(
                        TypedExpr::Variable(VariableExpr {
                            variable_id: matching_context_variable,
                        }),
                        found.type_id,
                        span,
                    )));
                    final_params.push(*context_param);
                } else {
                    let is_source_loc = context_param.type_id == COMPILER_SOURCE_LOC_TYPE_ID;
                    if is_source_loc {
                        let expr = self.synth_source_location(span);
                        final_args.push(MaybeTypedExpr::Typed(expr));
                        final_params.push(*context_param);
                    } else if !tolerate_missing_context_args {
                        return failf!(
                            span,
                            "Failed to find context parameter '{}'. No context variables of type {} are in scope",
                            self.ident_str(context_param.name),
                            self.type_id_to_string(context_param.type_id)
                        );
                    } else {
                        debug!(
                            "Tolerating a missing context argument of type {}. Let's try to infer one, one day",
                            self.type_id_to_string(context_param.type_id)
                        );
                        continue;
                    }
                };
            }
        }

        let args_slice = self.ast.p_call_args.get_slice(fn_call.args);
        let is_lambda = self.types.mem.get_nth_opt(params, 0).is_some_and(|p| p.is_lambda_env);
        let params = if is_lambda { params.skip(1) } else { params };
        let explicit_param_count =
            self.types.mem.getn(params).iter().filter(|p| !p.is_context).count();
        let total_expected =
            if explicit_context_args { params.len() as usize } else { explicit_param_count };
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

        let expected_literal_params = self
            .types
            .mem
            .getn(params)
            .iter()
            // If the user opted to pass context params explicitly, then check all params
            // If the user did not, then just check the non-context params, since the compiler is responsible
            // for looking up context params
            .filter(|p| explicit_context_args || !p.is_context);

        if let Some(pre_evaled_params) = pre_evaled_params {
            for (expr, param) in pre_evaled_params.iter().zip(expected_literal_params) {
                final_args.push(MaybeTypedExpr::Typed(*expr));
                final_params.push(*param)
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
                final_params.push(*fn_param);
            }
        }
        Ok(ArgsAndParams { args: final_args, params: final_params })
    }

    pub fn is_callee_generic(&self, callee: &Callee) -> bool {
        match callee {
            Callee::StaticFunction(function_id) => self.get_function(*function_id).is_generic(),
            Callee::StaticLambda { .. } => false,
            Callee::Abstract { function_sig, .. } => function_sig.has_type_params(),
            Callee::DynamicFunction { .. } => false,
            Callee::DynamicLambda(_) => false,
            // Should always be false...
            Callee::DynamicAbstract { function_sig, .. } => function_sig.has_type_params(),
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
            Callee::DynamicFunction { function_pointer_expr } => {
                let function_pointer_type =
                    self.get_expr_type(*function_pointer_expr).as_function_pointer().unwrap();
                function_pointer_type.function_type_id
            }
            Callee::DynamicLambda(dynamic) => match self.get_expr_type(*dynamic) {
                Type::LambdaObject(lambda_object) => lambda_object.function_type,
                _ => {
                    panic!(
                        "Invalid dynamic function callee: {}",
                        self.type_id_to_string(self.exprs.get_type(*dynamic))
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
        debug!("eval_function_call {}", self.qident_to_string(&fn_call.name));
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
        let is_generic = signature.has_type_params();

        let original_function_type = self.types.get(callee_function_type_id).as_function().unwrap();
        let params = original_function_type.logical_params();

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
                    let checked_expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => typed,
                        MaybeTypedExpr::Parsed(parsed) => self
                            .eval_expr_with_coercion(
                                parsed,
                                ctx.with_expected_type(Some(param.type_id)),
                                true,
                            )
                            .map_err(|err| {
                                errf!(
                                    err.span,
                                    "Invalid call to {}\nInvalid type for parameter '{}': {}",
                                    self.qident_to_string(&fn_call.name),
                                    self.ident_str(param.name),
                                    err.message
                                )
                            })?,
                    };
                    typechecked_args.push(checked_expr);
                }
                (callee, typechecked_args, SliceHandle::empty())
            }
            true => {
                let original_args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    true,
                )?;

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
                        self.named_types.add_slice_copy(&args)
                    }
                    _ => self.infer_and_constrain_call_type_args(
                        fn_call,
                        signature,
                        ctx,
                        &original_args_and_params,
                    )?,
                };

                let (function_type_args, function_type_arg_values) = self
                    .determine_function_type_args_for_call(
                        signature,
                        type_args,
                        &original_args_and_params,
                        ctx,
                    )?;

                let specialized_function_type =
                    self.substitute_in_function_signature(type_args, function_type_args, signature);
                let is_abstract = self
                    .types
                    .get_contained_type_variable_counts(specialized_function_type)
                    .is_abstract();

                let final_callee = if is_abstract || ctx.is_inference() {
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
                let specialized_params = specialized_fn_type.physical_params;
                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    specialized_params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                )?;

                // We've finished inference and all types are known; we now compile all the expressions
                // again to generate code with no holes and fully concrete types.
                let mut typechecked_args = SmallVec::with_capacity(args_and_params.len());

                // We can skip re-evaluating everything if we're just here to learn the return types
                if !ctx.is_inference() {
                    for (param_index, (maybe_typed_expr, param)) in
                        args_and_params.iter().enumerate()
                    {
                        // Is this parameter a function type parameter? If so, we already have the
                        // typechecked value expression
                        let matching_ftp_index = self
                            .function_type_params
                            .get_slice(signature.function_type_params)
                            .iter()
                            .position(|ftp| ftp.value_param_index as usize == param_index);
                        let expr = match matching_ftp_index {
                        Some(ftp_index) => {
                            let value = function_type_arg_values[ftp_index];
                            value
                        }
                        None => match *maybe_typed_expr {
                            MaybeTypedExpr::Typed(typed) => typed,
                            MaybeTypedExpr::Parsed(parsed) => self
                                .eval_expr_with_coercion(
                                    parsed,
                                    ctx.with_expected_type(Some(param.type_id)),
                                    true,
                                )
                                .map_err(|err| {
                                    errf!(
                                        err.span,
                                        "Invalid call to {}\nInvalid type for parameter '{}': {}",
                                        self.qident_to_string(&fn_call.name),
                                        self.ident_str(param.name),
                                        err.message
                                    )
                                })?,
                        },
                    };
                        typechecked_args.push(expr);
                    }
                }

                (final_callee, typechecked_args, type_args)
            }
        };

        // If any arguments definitely crash, we aren't calling the function at all.
        // So let's not generate a `Call`, but rather just the arguments expressions that should be
        // evaluated. This simplifies later compiler stages to not have to carve out special cases
        // for divergent expressions
        for (index, arg) in typechecked_arguments.iter().enumerate() {
            if self.exprs.get_type(*arg) == NEVER_TYPE_ID {
                let exprs_so_far = &typechecked_arguments[0..=index];
                return Ok(self.make_never_block(exprs_so_far, ctx.scope_id, span));
            }
        }

        let call_return_type = self
            .types
            .get(self.get_callee_function_type(&callee))
            .as_function()
            .unwrap()
            .return_type;

        let call = Call {
            callee,
            args: typechecked_arguments,
            type_args,
            return_type: call_return_type,
            span,
        };

        // Intrinsics that are handled by the typechecking phase are implemented here.
        if let Some(intrinsic_type) = call
            .callee
            .maybe_function_id()
            .and_then(|id| self.get_function(id).intrinsic_type)
            .filter(|op| op.is_typer_phase())
        {
            self.handle_intrinsic(call, intrinsic_type, ctx)
        } else {
            let call_id = self.calls.add(call);
            Ok(self.exprs.add(TypedExpr::Call { call_id }, call_return_type, span))
        }
    }

    ////////////////////////////////
    // End of handling function calls

    fn attempt_static_lift(&mut self, expr_id: TypedExprId) -> TyperResult<TypedExprId> {
        // Take an arbitrary expression and do our very best to turn it into a statically-known
        // value. Easy examples that must work include:
        // - all scalar literals,
        // - string literals,
        // - Perhaps struct and enum literals, if all of their fields also meet the other criteria
        // recursively
        // - LIST literals are tricky because they've already been compiled to a block of
        //   imperative code

        // We match on the node type, not its type, since the point is to hoist literals, not
        // follow variables around and implement a whole extra damn compiler
        let result = match self.exprs.get(expr_id) {
            TypedExpr::StaticValue(static_constant) if !static_constant.is_typed_as_static => {
                let value_id = static_constant.value_id;
                let span = self.exprs.get_span(expr_id);
                Ok(self.add_static_value_expr(value_id, span))
            }
            e => {
                failf!(
                    self.exprs.get_span(expr_id),
                    "Expression type is unsupported for static lift: {}. For more complex values, use a #static expression instead",
                    e.kind_str()
                )
            }
        };
        if let Ok(result) = result {
            debug_assert_eq!(
                self.types
                    .get_no_follow_static(self.exprs.get_type(result))
                    .as_static()
                    .unwrap()
                    .inner_type_id,
                self.exprs.get_type(expr_id)
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
                // This is fun because we take a static value id pointing to an integer and interpret that
                // integer as a static value id!
                let TypedExpr::StaticValue(static_const) = static_value_id_arg else {
                    return failf!(span, "Argument must be an integer literal");
                };
                let StaticValue::Int(TypedIntValue::U64(u64_value)) =
                    self.static_values.get(static_const.value_id)
                else {
                    return failf!(span, "Argument must be a u64 literal");
                };
                let Some(static_value_id) = StaticValueId::from_u32(*u64_value as u32) else {
                    return failf!(span, "Invalid static value id: {}. Cannot be zero", u64_value);
                };
                let Some(value) = self.static_values.get_opt(static_value_id) else {
                    return failf!(span, "No static value with given id: {}", static_value_id);
                };
                Ok(self.exprs.add_static(static_value_id, value.get_type(), false, span))
            }
            IntrinsicOperation::StaticTypeToValue => {
                // intern fn staticTypeToValue[T, ST: static T](): T
                // let inner_type_arg = self.named_types.get_nth(call.type_args, 0);
                let static_type_arg = self.named_types.get_nth(call.type_args, 1);

                let return_type = call.return_type;
                let Type::Static(static_type) =
                    self.types.get_no_follow_static(static_type_arg.type_id)
                else {
                    return failf!(
                        span,
                        "Internal Error: 2nd type arg should be static: {}",
                        self.type_id_to_string(static_type_arg.type_id)
                    );
                };
                if let Some(static_value_id) = static_type.value_id {
                    Ok(self.add_static_constant_expr(static_value_id, span))
                } else {
                    // Since the static type has no value, we know this is generic code
                    // and we just need to generate a term that typechecks, so a
                    // unit casted to the generic static type. We should probably invent
                    // a way to do this that doesn't require 2 nodes
                    let unit_expr = self.synth_unit(span);
                    Ok(self.synth_cast(unit_expr, return_type, CastType::Transmute, None))
                }
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

                let int_expr = self.synth_int(TypedIntValue::U64(type_id_u64), span);
                Ok(int_expr)
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
                Ok(self.synth_i64(to_k1_size_u64(value_bytes), span))
            }
            _ => self.ice(format!("Unexpected intrinsic in type phase: {:?}", intrinsic), None),
        }
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
        let ability_args_new_handle = self.named_types.add_slice_copy(&ability_args_new);
        let impl_args_new_handle = self.named_types.add_slice_copy(&impl_args_new);
        let specialized_base =
            self.specialize_ability(base_ability_id, ability_args_new_handle, span, scope_id);
        TypedAbilitySignature {
            specialized_ability_id: specialized_base,
            impl_arguments: impl_args_new_handle,
        }
    }

    fn extract_function_type_from_functionlike(&self, typ: &Type) -> Option<TypeId> {
        // What can we pass when we expect a function type parameter?
        // A FunctionPointer: fn_name.toRef()
        // A lambda: \x -> x + 1
        // A lambda-object: dyn[A -> B]
        // A function-type-parameter, written 'some \A -> B'
        match typ {
            Type::FunctionPointer(fp) => Some(fp.function_type_id),
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

    pub fn get_array_scope_id(&self) -> ScopeId {
        debug_assert_ne!(self.scopes.array_scope_id, ScopeId::PENDING);
        self.scopes.array_scope_id
    }

    fn substitute_in_function_signature(
        &mut self,
        // Must 'zip' up with each type param
        type_arguments: NamedTypeSlice,
        // Must 'zip' up with each function type param
        function_type_arguments: NamedTypeSlice,
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
            .function_type_params
            .get_slice(generic_function_sig.function_type_params)
            .iter()
            .zip(self.named_types.get_slice(function_type_arguments))
        {
            subst_pairs.push(TypeSubstitutionPair {
                from: function_type_param.type_id,
                to: function_type_arg.type_id,
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
        generic_function_id: FunctionId,
    ) -> TyperResult<FunctionId> {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_param_variables = generic_function.param_variables;
        let generic_function_scope = generic_function.scope;

        for existing_specialization in &generic_function.child_specializations {
            if self
                .named_types
                .slices_equal_copy(existing_specialization.type_arguments, type_arguments)
                && self.named_types.slices_equal_copy(
                    existing_specialization.function_type_arguments,
                    function_type_arguments,
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
            generic_function.signature(),
        );
        let specialized_function_id = self.functions.next_id();
        debug!(
            "specialized function type: {}",
            self.type_id_to_string(specialized_function_type_id)
        );
        let specialized_name_ident = self.build_ident_with(|m, s| {
            let generic_function = m.get_function(generic_function_id);
            let spec_num = generic_function.child_specializations.len() + 1;
            write!(s, "{}__", m.ident_str(generic_function.name)).unwrap();
            for nt in m.named_types.get_slice(type_arguments) {
                m.display_type_id(s, nt.type_id, false).unwrap()
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

        let mut param_variables: MList<VariableId, _> =
            self.mem.new_list(specialized_function_type.physical_params.len());
        for (specialized_param_type, generic_param) in self
            .types
            .mem
            .getn(specialized_function_type.physical_params)
            .iter()
            .zip(self.mem.getn(generic_function_param_variables))
        {
            let name = self.variables.get(*generic_param).name;
            let mut flags = VariableFlags::empty();
            flags.set(VariableFlags::Context, specialized_param_type.is_context);
            let variable_id = self.variables.add(Variable {
                type_id: specialized_param_type.type_id,
                name,
                owner_scope: spec_fn_scope,
                kind: VariableKind::FnParam(specialized_function_id),
                flags,
                usage_count: 0,
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
            param_variables.push(variable_id)
        }
        let specialization_info = SpecializationInfo {
            parent_function: generic_function_id,
            type_arguments,
            function_type_arguments,
            specialized_function_id: FunctionId::PENDING,
            specialized_function_type: specialized_function_type_id,
        };
        let generic_function = self.get_function(generic_function_id);
        let has_body = self
            .ast
            .get_function(generic_function.parsed_id.as_function_id().unwrap())
            .body
            .is_some();
        let specialized_function = TypedFunction {
            name: specialized_name_ident,
            scope: spec_fn_scope,
            param_variables: self.mem.vec_to_mslice(&param_variables),
            // Must be empty for correctness; a specialized function has no type parameters!
            type_params: SliceHandle::empty(),
            // Must be empty for correctness; a specialized function has no function type parameters!
            function_type_params: SliceHandle::empty(),
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
        let actual_specialized_function_id = self.add_function(specialized_function);
        debug_assert_eq!(specialized_function_id, actual_specialized_function_id);
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
        // eprintln!("specialize_function_body\n  {}", self.function_id_to_string(function_id, false));
        // eprintln!(
        //     "specialize_function_body with: {}",
        //     self.pretty_print_named_type_slice(
        //         specialized_function.specialization_info.unwrap().type_arguments,
        //         ", "
        //     )
        // );
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
            .body
            .as_ref()
            .unwrap();
        let typed_body = self.eval_expr(
            parsed_body,
            EvalExprContext::make(specialized_function_scope_id)
                .with_expected_type(Some(specialized_return_type)),
        )?;

        let body_type = self.exprs.get_type(typed_body);
        if let Err(msg) =
            self.check_types(specialized_return_type, body_type, specialized_function_scope_id)
        {
            return failf!(
                self.get_span_responsible_for_expr_type(typed_body),
                "Function body type mismatch: {}\n specialized signature is: {}",
                msg,
                self.type_id_to_string(specialized_function_type)
            );
        }

        self.get_function_mut(function_id).body_block = Some(typed_body);

        //if is_concrete {
        //    if let Err(e) = bc::compile_function(self, function_id) {
        //        return failf!(e.span, "Failed to compile bytecode for function: {}", e.message);
        //    }
        //}

        Ok(())
    }

    /// Used to drill down to the span that is responsible for the type of the given expression
    /// For example, the last statement of a block, rather than the entire span
    pub fn get_span_responsible_for_expr_type(&self, typed_expr_id: TypedExprId) -> SpanId {
        match self.exprs.get(typed_expr_id) {
            TypedExpr::Block(typed_block) => match self.mem.get_last_opt(typed_block.statements) {
                None => self.exprs.get_span(typed_expr_id),
                Some(stmt) => match self.stmts.get(*stmt) {
                    TypedStmt::Expr(typed_expr_id, _) => {
                        self.get_span_responsible_for_expr_type(*typed_expr_id)
                    }
                    TypedStmt::Let(let_stmt) => let_stmt.span,
                    TypedStmt::Assignment(a) => a.span,
                    TypedStmt::Require(r) => r.span,
                    TypedStmt::Defer(defer) => defer.span,
                },
            },
            TypedExpr::Match(typed_match_expr) => self.get_span_responsible_for_expr_type(
                self.mem.get_nth_opt(typed_match_expr.arms, 0).unwrap().consequent_expr,
            ),
            TypedExpr::Return(r) => self.get_span_responsible_for_expr_type(r.value),
            _e => self.exprs.get_span(typed_expr_id),
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
        // If we specialized on something generic, but we don't accept or return it in our
        // signature, we won't catch it by checking the signature!
        // Example: fn typeOnly[T: static u32](): unit
        // If specialized on static[u32, <none>], wouldn't have any generics in its signature
        if let Some(spec_info) = function.specialization_info {
            for t in self.named_types.get_slice(spec_info.type_arguments) {
                if self.types.type_variable_counts.get(t.type_id).is_abstract() {
                    return false;
                }
            }
            for t in self.named_types.get_slice(spec_info.function_type_arguments) {
                if self.types.type_variable_counts.get(t.type_id).is_abstract() {
                    return false;
                }
            }
        }
        let info = self.types.get_contained_type_variable_counts(function.type_id);
        let has_no_abstract_types_in_signature = !info.is_abstract();
        has_no_abstract_types_in_signature
    }

    fn eval_stmt(
        &mut self,
        stmt: ParsedStmtId,
        ctx: EvalExprContext,
        coerce_expr: bool,
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
                let (expected_rhs_type, provided_reference_mutability) = match provided_type {
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
                            (
                                Some(expected_reference_type.inner_type),
                                Some(expected_reference_type.mutable),
                            )
                        } else {
                            (Some(provided_type), None)
                        }
                    }
                    None => (None, None),
                };
                let value_expr = match parsed_let.value {
                    None => None,
                    Some(value) => Some(self.eval_expr_with_coercion(
                        value,
                        ctx.with_expected_type(expected_rhs_type),
                        true,
                    )?),
                };
                let actual_type = match value_expr {
                    None => None,
                    Some(value_expr) => Some(self.exprs.get_type(value_expr)),
                };

                let variable_type = match actual_type {
                    None => match provided_type {
                        None => return failf!(parsed_let.span, "Uninit let requires a type"),
                        Some(t) => t,
                    },
                    Some(actual_type) => {
                        if parsed_let.is_referencing() {
                            let mutable = provided_reference_mutability.unwrap_or(true);
                            self.types.add_reference_type(actual_type, mutable)
                        } else {
                            actual_type
                        }
                    }
                };

                let mut flags = VariableFlags::empty();

                flags.set(VariableFlags::Context, parsed_let.is_context());
                let stmt_id = self.stmts.next_id();
                let variable_id = self.variables.add(Variable {
                    name: parsed_let.name,
                    type_id: variable_type,
                    owner_scope: ctx.scope_id,
                    kind: VariableKind::Let(stmt_id),
                    flags,
                    usage_count: 0,
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
                self.stmts.add_expected_id(val_def_stmt, stmt_id);
                Ok(Some(stmt_id))
            }
            ParsedStmt::Require(require) => {
                static_assert_size!(parse::ParsedRequire, 12);
                let require = require.clone();
                match self
                    .eval_matching_condition(require.condition_expr, ctx.with_no_expected_type())?
                {
                    Either::Left(block) => {
                        let stmt = self.add_expr_stmt(block);
                        Ok(Some(stmt))
                    }
                    Either::Right(condition) => {
                        let else_scope = self.scopes.add_child_scope(
                            ctx.scope_id,
                            ScopeType::LexicalBlock,
                            None,
                            None,
                        );

                        // Make the binding variables unavailable in the else scope
                        for instr in self.mem.getn(condition.instrs) {
                            if let MatchingConditionInstr::Binding { let_stmt } = instr {
                                let stmt = self.stmts.get(*let_stmt).as_let().unwrap();
                                let variable = self.variables.get(stmt.variable_id);

                                if !variable.user_hidden() {
                                    let else_scope = self.scopes.get_scope_mut(else_scope);
                                    else_scope.mask_variable(variable.name);
                                }
                            }
                        }

                        let else_body =
                            self.eval_expr(require.else_body, ctx.with_scope(else_scope))?;
                        if self.exprs.get_type(else_body) != NEVER_TYPE_ID {
                            let else_span = self.exprs.get_span(else_body);
                            return failf!(
                                else_span,
                                "else branch must diverge; try returning or exiting"
                            );
                        }

                        let id = self.stmts.add(TypedStmt::Require(TypedRequireStmt {
                            condition: Box::new(condition),
                            else_body,
                            span: require.span,
                        }));
                        Ok(Some(id))
                    }
                }
            }
            ParsedStmt::Assign(assign) => {
                static_assert_size!(parse::AssignStmt, 12);
                let assignment = assign.clone();
                let ParsedExpr::Variable(_) = self.ast.exprs.get(assignment.lhs) else {
                    return failf!(
                        self.ast.exprs.get_span(assignment.lhs),
                        "Value assignment destination must be a plain variable expression"
                    );
                };
                let (typed_variable_id, lhs) = self.eval_variable(assignment.lhs, ctx.scope_id)?;
                let lhs_type = self.exprs.get_type(lhs);
                let rhs = self.eval_expr(assignment.rhs, ctx.with_expected_type(Some(lhs_type)))?;
                let rhs_type = self.exprs.get_type(rhs);
                if let Err(msg) = self.check_types(lhs_type, rhs_type, ctx.scope_id) {
                    return failf!(assignment.span, "Invalid type for assignment: {}", msg,);
                }
                self.variables
                    .get_mut(typed_variable_id)
                    .flags
                    .set(VariableFlags::Reassigned, true);
                let stmt_id = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                    destination: lhs,
                    value: rhs,
                    span: assignment.span,
                    kind: AssignmentKind::Set,
                }));
                Ok(Some(stmt_id))
            }
            ParsedStmt::Store(set_stmt) => {
                static_assert_size!(parse::StoreStmt, 12);
                let set_stmt = set_stmt.clone();
                let lhs = self.eval_expr(set_stmt.lhs, ctx.with_no_expected_type())?;
                let lhs_type = self.exprs.get_type(lhs);
                let Some(lhs_type) = self.types.get(lhs_type).as_reference() else {
                    return failf!(
                        self.ast.exprs.get_span(set_stmt.lhs),
                        "Expected a reference type; got {}",
                        self.type_id_to_string(lhs_type)
                    );
                };
                if lhs_type.is_read_only() {
                    return failf!(
                        self.ast.exprs.get_span(set_stmt.lhs),
                        "Cannot write to a read-only reference"
                    );
                }
                let expected_rhs = lhs_type.inner_type;
                let rhs =
                    self.eval_expr(set_stmt.rhs, ctx.with_expected_type(Some(expected_rhs)))?;
                let rhs_type = self.exprs.get_type(rhs);
                if let Err(msg) = self.check_types(expected_rhs, rhs_type, ctx.scope_id) {
                    return failf!(set_stmt.span, "Invalid type for assignment: {}", msg,);
                }
                let stmt_id = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                    destination: lhs,
                    value: rhs,
                    span: set_stmt.span,
                    kind: AssignmentKind::Store,
                }));
                Ok(Some(stmt_id))
            }
            ParsedStmt::Defer(defer) => {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    return failf!(defer.span, "defer cannot be used inside `defer` blocks");
                }
                let defer = *defer;
                let defer_stmt = self.stmts.add(TypedStmt::Defer(TypedDeferStmt {
                    parsed_expr: defer.expr,
                    span: defer.span,
                }));

                Ok(Some(defer_stmt))
            }
            ParsedStmt::LoneExpression(expression) => {
                let expr = if coerce_expr {
                    self.eval_expr_with_coercion(*expression, ctx, false)?
                } else {
                    self.eval_expr(*expression, ctx)?
                };
                let expr_type = self.exprs.get_type(expr);
                let stmt_id = self.stmts.add(TypedStmt::Expr(expr, expr_type));
                Ok(Some(stmt_id))
            }
        }
    }

    /// This block's scope is ALREADY PROVIDED AND SET IN CTX
    fn eval_block(
        &mut self,
        block: &ParsedBlock,
        // This block's scope is ALREADY PROVIDED AND SET IN CTX
        ctx: EvalExprContext,
        needs_terminator: bool,
    ) -> TyperResult<TypedExprId> {
        let block_scope = ctx.scope_id;
        if block.stmts.is_empty() {
            return failf!(block.span, "Blocks must contain at least one statement or expression");
        }
        let mut stmts = self.mem.new_list(block.stmts.len() as u32 + 1);
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

            let coerce = expected_type.is_some();
            debug!("eval_stmt {index} with type {}", self.type_id_to_string_opt(expected_type));
            let Some(stmt_id) =
                self.eval_stmt(*stmt, ctx.with_expected_type(expected_type), coerce)?
            else {
                continue;
            };

            let stmt = self.stmts.get(stmt_id);
            if let TypedStmt::Defer(defer) = stmt {
                match self.scopes.block_defers.entry(block_scope) {
                    Entry::Occupied(mut defers) => {
                        defers.get_mut().deferred_exprs.push(defer.parsed_expr);
                    }
                    Entry::Vacant(vacant) => {
                        vacant.insert(ScopeDefers { deferred_exprs: smallvec![defer.parsed_expr] });
                    }
                }
            }

            // If this statement early returns, we need to insert deferred code.
            let is_return =
                if let TypedStmt::Expr(id, _) = stmt { self.expr_is_return(*id) } else { false };

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
                    match self.stmts.get(stmt_id) {
                        TypedStmt::Expr(expr, _expr_type_id) => {
                            // Return this expr
                            let expr_span = self.exprs.get_span(*expr);
                            let return_expr = self.exprs.add_return(*expr, expr_span);
                            let return_stmt =
                                self.stmts.add(TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
                            stmts.push(return_stmt);
                        }
                        TypedStmt::Assignment(_)
                        | TypedStmt::Let(_)
                        | TypedStmt::Require(_)
                        | TypedStmt::Defer(_) => {
                            let unit = self.synth_unit(stmt_span);
                            let return_unit_expr = self.exprs.add_return(unit, stmt_span);
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

            // Generate deferred expressions. We typecheck them NOW, meaning
            // its as if the deferred code were textually pasted to the end of the function.
            // There are more robust ways to handle this; namely, allow a closure like Go, where
            // the user can decide exactly what to capture at defer-time and what to evaluate at
            // block close time
            // eval_return(...) handles deferred expressions itself
            if is_last && !is_return {
                let terminating = self.get_stmt_type(*stmts.last().unwrap()) == NEVER_TYPE_ID;
                if let Some(this_scope_defers) = self.scopes.block_defers.get(&block_scope) {
                    let deferred_exprs = this_scope_defers.deferred_exprs.clone();
                    for deferred_parsed_expr in deferred_exprs.iter().rev() {
                        let deferred_code = self.eval_expr(
                            *deferred_parsed_expr,
                            ctx.with_no_expected_type().with_is_defer(true),
                        )?;
                        let defer_type = self.exprs.get_type(deferred_code);
                        let defer_stmt_id =
                            self.stmts.add(TypedStmt::Expr(deferred_code, defer_type));
                        if terminating {
                            stmts.insert(stmts.len() - 1, defer_stmt_id);
                        } else {
                            stmts.push(defer_stmt_id)
                        }
                    }
                };
            }
        }

        let id = self.exprs.add_block(
            &mut self.mem,
            BlockBuilder { scope_id: block_scope, statements: stmts, span: block.span },
            last_expr_type,
        );
        //eprintln!("  finished block with type:\n{}", self.expr_to_string_with_type(id));
        //eprintln!("  last_expr_type was: {}", self.type_id_to_string(last_expr_type));
        Ok(id)
    }

    fn expr_is_return(&self, mut expr_id: TypedExprId) -> bool {
        loop {
            match self.exprs.get(expr_id) {
                TypedExpr::Return(_) => return true,
                TypedExpr::Block(b) => {
                    if let Some(s) = self.mem.get_last_opt(b.statements) {
                        if let TypedStmt::Expr(e2, _) = self.stmts.get(*s) {
                            expr_id = *e2;
                            continue;
                        }
                    }
                    return false;
                }
                _ => return false,
            }
        }
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
            let base_ability_id = self.abilities.get(ability_id).base_ability_id;
            use IntrinsicArithOpClass as Class;
            use IntrinsicArithOpKind as OpKind;
            use IntrinsicArithOpOp as Op;
            macro_rules! mk_arith {
                ($e: expr) => {
                    Some(IntrinsicOperation::ArithBinop($e))
                };
            }
            macro_rules! mk_bitwise {
                ($e: expr) => {
                    Some(IntrinsicOperation::BitwiseBinop($e))
                };
            }
            let t = self.types.get(ability_impl_type_id);
            let is_integer = t.as_integer().is_some();
            match (base_ability_id, fn_name_str) {
                // Leaving this example of how to do intrinsic ability fns
                // Even though we have bitwise below
                (EQUALS_ABILITY_ID, "equals") => match t {
                    Type::Unit | Type::Char | Type::Bool | Type::Pointer => {
                        mk_arith!(OpKind::uint(Op::Equals))
                    }
                    Type::Integer(i) => {
                        let o = if i.is_signed() {
                            OpKind::sint(Op::Equals)
                        } else {
                            OpKind::uint(Op::Equals)
                        };
                        mk_arith!(o)
                    }
                    Type::Float(_) => mk_arith!(OpKind::float(Op::Equals)),
                    _ => None,
                },
                (BITWISE_ABILITY_ID, "bitNot") if is_integer => Some(IntrinsicOperation::BitNot),
                (BITWISE_ABILITY_ID, "bitAnd") if is_integer => {
                    mk_bitwise!(IntrinsicBitwiseBinopKind::And)
                }
                (BITWISE_ABILITY_ID, "bitOr") if is_integer => {
                    mk_bitwise!(IntrinsicBitwiseBinopKind::Or)
                }
                (BITWISE_ABILITY_ID, "xor") if is_integer => {
                    mk_bitwise!(IntrinsicBitwiseBinopKind::Xor)
                }
                (BITWISE_ABILITY_ID, "shiftLeft") if is_integer => {
                    mk_bitwise!(IntrinsicBitwiseBinopKind::ShiftLeft)
                }
                (BITWISE_ABILITY_ID, "shiftRight") if is_integer => {
                    let int_type = t.expect_integer();
                    if int_type.is_signed() {
                        mk_bitwise!(IntrinsicBitwiseBinopKind::SignedShiftRight)
                    } else {
                        mk_bitwise!(IntrinsicBitwiseBinopKind::UnsignedShiftRight)
                    }
                }
                (ADD_ABILITY_ID, "add") => match t {
                    Type::Integer(i) => {
                        // Even though signedness is irrelevant here, we still set it properly
                        // just in case it ever is, (for example if we want to make signed wrap UB
                        // instead of wrapping)
                        if i.is_signed() {
                            mk_arith!(IntrinsicArithOpKind::sint(Op::Add))
                        } else {
                            mk_arith!(IntrinsicArithOpKind::uint(Op::Add))
                        }
                    }
                    Type::Float(_) => {
                        mk_arith!(IntrinsicArithOpKind::float(Op::Add))
                    }
                    _ => None,
                },
                (SUB_ABILITY_ID, "sub") => {
                    let class = if let Type::Integer(i) = t {
                        IntrinsicArithOpClass::from_int_type(*i)
                    } else {
                        IntrinsicArithOpClass::Float
                    };
                    mk_arith!(OpKind { class, op: Op::Sub })
                }
                (MUL_ABILITY_ID, "mul") => {
                    let class = if let Type::Integer(i) = t {
                        Class::from_int_type(*i)
                    } else {
                        Class::Float
                    };
                    mk_arith!(OpKind { class, op: Op::Mul })
                }
                (DIV_ABILITY_ID, "div") => {
                    let class = if let Type::Integer(i) = t {
                        IntrinsicArithOpClass::from_int_type(*i)
                    } else {
                        IntrinsicArithOpClass::Float
                    };
                    mk_arith!(OpKind { class, op: Op::Div })
                }
                (REM_ABILITY_ID, "rem") => {
                    let class = if let Type::Integer(i) = t {
                        IntrinsicArithOpClass::from_int_type(*i)
                    } else {
                        IntrinsicArithOpClass::Float
                    };
                    mk_arith!(OpKind { class, op: Op::Rem })
                }
                (SCALAR_CMP_ABILITY_ID, _) => {
                    let class = if let Type::Integer(i) = t {
                        IntrinsicArithOpClass::from_int_type(*i)
                    } else {
                        IntrinsicArithOpClass::Float
                    };
                    match fn_name_str {
                        "lt" => mk_arith!(OpKind { class, op: Op::Lt }),
                        "le" => mk_arith!(OpKind { class, op: Op::Le }),
                        "gt" => mk_arith!(OpKind { class, op: Op::Gt }),
                        "ge" => mk_arith!(OpKind { class, op: Op::Ge }),
                        _ => None,
                    }
                }
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
                Some("ptr") => match fn_name_str {
                    "refAtIndex" => Some(IntrinsicOperation::PointerIndex),
                    _ => None,
                },
                Some("meta") => match fn_name_str {
                    "bakeStaticValue" => Some(IntrinsicOperation::BakeStaticValue),
                    "getStaticValue" => Some(IntrinsicOperation::GetStaticValue),
                    "staticTypeToValue" => Some(IntrinsicOperation::StaticTypeToValue),
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
                    let payload_value = self
                        .check_and_coerce_expr(payload_type, payload_value, ctx.scope_id)
                        .map_err(|e| errf!(span, "Variant payload type mismatch: {}", e.message))?;
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
        let never_payload = payload.is_some_and(|p| self.exprs.get_type(p) == NEVER_TYPE_ID);
        if never_payload {
            // Might as well just codegen the payload expr that wants to exit; we can't put it in
            // a variant and now all downstream code doesn't have to worry about the 'crash
            // payload' scenario
            let never_payload_expr = payload.unwrap();
            Ok(never_payload_expr)
        } else {
            let enum_constructor = self.exprs.add(
                TypedExpr::EnumConstructor(TypedEnumConstructor { variant_index, payload }),
                variant_type_id,
                span,
            );
            let casted_expr = match ctx.expected_type_id.map(|t| self.types.get(t)) {
                Some(Type::EnumVariant(ev)) if ev.my_type_id == variant_type_id => {
                    debug!(
                        "enum constructor output type is the variant type: {}",
                        self.type_id_to_string(variant_type_id)
                    );
                    enum_constructor
                }
                _ => {
                    debug!(
                        "casted enum constructor to its enum type: {}",
                        self.type_id_to_string(concrete_enum_type)
                    );
                    self.synth_cast(
                        enum_constructor,
                        concrete_enum_type,
                        CastType::VariantToEnum,
                        None,
                    )
                }
            };
            Ok(casted_expr)
        }
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
        let tp = self.types.get_type_parameter(param_type);
        if let Some(static_constraint) = tp.static_constraint {
            let specialized_constraint =
                self.substitute_in_type(static_constraint, substitution_pairs);
            if let Err(msg) = self.check_types(specialized_constraint, passed_type, scope_id) {
                return failf!(
                    span,
                    "Provided type for {} didn't satisfy the static constraint: {msg}",
                    self.ident_str(param_name),
                );
            }
        }
        let ability_constraints = self.get_constrained_ability_impls_for_type(param_type);
        for constraint in &ability_constraints {
            let original_signature = TypedAbilitySignature {
                specialized_ability_id: constraint.specialized_ability_id,
                impl_arguments: self.ability_impls.get(constraint.full_impl_id).impl_arguments,
            };
            let signature = if substitution_pairs.is_empty() {
                original_signature
            } else {
                let specialized_constraint_signature = self.substitute_in_ability_signature(
                    substitution_pairs,
                    original_signature,
                    scope_id,
                    span,
                );
                debug!(
                    "I specialized an ability constraint for checking: {}",
                    self.ability_signature_to_string(specialized_constraint_signature)
                );
                specialized_constraint_signature
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

        let ability_arguments_handle = self.named_types.add_slice_copy(&ability_arguments);
        let impl_arguments_handle = self.named_types.add_slice_copy(&impl_arguments);
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
        let self_ident = self.ast.idents.b.Self_;
        let new_self_type_id = self.add_type_parameter(
            TypeParameter {
                name: self_ident,
                static_constraint: None,
                scope_id: specialized_ability_scope,
                span,
            },
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
        for (index, parsed_fn) in parsed_ability.functions.clone().iter().enumerate() {
            let result = self.compile_function_declaration(
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
                index: index as u32,
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
            // TODO: Possible now to pass 'dont cares'. I think we allow them in ability exprs that are constraints but
            //       nowhere else
            let Some(arg_type_expr) = arg.type_expr else {
                return failf!(arg.span, "_ is not yet supported as an ability type argument");
            };
            let arg_type = self.eval_type_expr(arg_type_expr, scope_id)?;
            let Some(name) = arg.name else {
                return failf!(arg.span, "Ability arguments must all be named, for now");
            };
            arguments.push(NameAndType { name, type_id: arg_type });
        }

        let arguments_handle = self.named_types.add_slice_copy(&arguments);
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

    fn compile_function_declaration(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        parent_scope_id: ScopeId,
        ability_info: Option<FunctionAbilityContextInfo>,
        namespace_id: NamespaceId,
    ) -> TyperResult<Option<FunctionId>> {
        let namespace = self.namespaces.get(namespace_id);
        let companion_type_id = namespace.companion_type_id;
        let parsed_function = self.ast.get_function(parsed_function_id).clone();
        let is_debug = parsed_function.compiler_debug;
        let should_compile =
            self.execute_static_condition(parsed_function.condition, parent_scope_id);
        if !should_compile {
            return Ok(None);
        }
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

        // In all of these scenarios, we've seen the function before, so we shouldn't do the AST
        // mapping; there's a more appropriate 'original' that already has it
        let skip_ast_mapping = ability_kind_is_specialized
            || ability_info.as_ref().is_some_and(|info| {
                info.impl_info.as_ref().is_some_and(|impl_info| {
                    impl_info.is_default
                        || impl_info.impl_kind.is_derived_from_blanket()
                        || impl_info.impl_kind.is_type_param_constraint()
                })
            });
        let resolvable_by_name = !is_ability_impl && !ability_kind_is_specialized;

        let name = match impl_info.as_ref() {
            Some(impl_info) => {
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
        let mut function_type_params: SV4<FunctionTypeParam> = SmallVec::new();

        // Inject the 'Self' type parameter
        if is_ability_decl {
            let self_type_id = self_.abilities.get(ability_id.unwrap()).self_type_id;
            type_params.push(NameAndType { name: self_.ast.idents.b.Self_, type_id: self_type_id })
        }
        for type_parameter in parsed_type_params.iter() {
            let mut ability_constraints = SmallVec::new();
            let mut static_constraint: Option<TypeId> = None;

            for parsed_constraint in type_parameter.constraints.iter().chain(
                parsed_function
                    .additional_where_constraints
                    .iter()
                    .filter(|c| c.name == type_parameter.name)
                    .map(|c| &c.constraint_expr),
            ) {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig =
                            self_.eval_ability_expr(*ability_expr, false, fn_scope_id)?;
                        ability_constraints.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Static(static_expr) => {
                        let static_type = self_.eval_type_expr(*static_expr, fn_scope_id)?;
                        match &static_constraint {
                            None => static_constraint = Some(static_type),
                            Some(_) => {
                                return failf!(
                                    type_parameter.span,
                                    "Cannot specify more than one static constraint for a parameter"
                                );
                            }
                        }
                    }
                };
            }
            let type_variable_id = self_.add_type_parameter(
                TypeParameter {
                    name: type_parameter.name,
                    static_constraint,
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
        let param_count = parsed_function_context_params.len() + parsed_function_params.len();
        let mut param_types: MList<FnParamType, _> = self_.types.mem.new_list(param_count as u32);
        let mut param_variables = self_.mem.new_list(param_count as u32);
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
            match self_.types.get_no_follow_static(type_id) {
                Type::FunctionTypeParameter(ftp) => {
                    function_type_params.push(FunctionTypeParam {
                        name: ftp.name,
                        type_id,
                        value_param_index: idx as u32,
                        span: ftp.span,
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
                                self_.types.get_instance_info(
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
                owner_scope: fn_scope_id,
                flags: if is_context { VariableFlags::Context } else { VariableFlags::empty() },
                usage_count: 0,
                kind: VariableKind::FnParam(FunctionId::PENDING),
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
        // And it must return an i32
        let is_main_fn =
            namespace_id == ROOT_NAMESPACE_ID && parsed_function_name == self_.ast.idents.b.main;
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
                        "main must take exactly 0 or 2 parameters, got {}",
                        n
                    );
                }
            };
            match return_type {
                I32_TYPE_ID => {}
                _other => {
                    return failf!(parsed_function_span, "main must return i32");
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
                    | AbilityImplKind::TypeParamConstraint => TypedFunctionKind::AbilityImpl(
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

        let param_types_handle = self_.types.mem.vec_to_mslice(&param_types);
        let function_type_id = self_.types.add_anon(Type::Function(FunctionType {
            physical_params: param_types_handle,
            return_type,
            is_lambda: false,
        }));

        let type_params_handle = self_.named_types.add_slice_copy(&type_params);
        let function_type_params_handle =
            self_.function_type_params.add_slice_copy(&function_type_params);
        let function_id = self_.functions.next_id();
        for v in param_variables.iter() {
            self_.variables.get_mut(*v).kind = VariableKind::FnParam(function_id);
        }
        let param_variables_handle = self_.mem.vec_to_mslice(&param_variables);
        let function_id = self_.add_function(TypedFunction {
            name,
            scope: fn_scope_id,
            param_variables: param_variables_handle,
            type_params: type_params_handle,
            function_type_params: function_type_params_handle,
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
        let is_extern = matches!(function.linkage, Linkage::External { .. });
        let is_concrete = function.is_concrete;
        let ast_id = function.parsed_id.as_function_id().expect("expected function id");
        let is_intrinsic = function.intrinsic_type.is_some();
        let is_ability_defn = matches!(function.kind, TypedFunctionKind::AbilityDefn(_));

        let parsed_function = self.ast.get_function(ast_id);
        let parsed_function_ret_type = parsed_function.ret_type;
        let function_signature_span = parsed_function.signature_span;

        let is_abstract = is_intrinsic || is_extern || is_ability_defn;
        let is_generic = function.is_generic();

        let body_block = match parsed_function.body.as_ref() {
            None if is_abstract => None,
            None => return failf!(function_signature_span, "function is missing implementation"),
            Some(_) if is_abstract => {
                return failf!(function_signature_span, "unexpected function implementation");
            }
            Some(block_ast) => {
                if function.specialization_info.is_some() && !is_concrete {
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
                    self.check_types(return_type, self.exprs.get_type(block), fn_scope_id)
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

    fn compile_ability_definition(
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

        let self_ident_id = self.ast.idents.b.Self_;
        let mut ability_params: EcoVec<TypedAbilityParam> =
            EcoVec::with_capacity(parsed_ability.params.len() + 1);
        let self_type_id = self.add_type_parameter(
            TypeParameter {
                name: self_ident_id,
                static_constraint: None,
                scope_id: ability_scope_id,
                span: parsed_ability.span,
            },
            smallvec![],
        );
        let _ = self.scopes.get_scope_mut(ability_scope_id).add_type(self_ident_id, self_type_id);
        for ability_param in parsed_ability.params.clone().iter() {
            let mut ability_constraints: SV4<TypedAbilitySignature> = smallvec![];
            for constraint in ability_param.constraints.iter() {
                match constraint {
                    parse::ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let signature =
                            self.eval_ability_expr(*ability_expr, false, ability_scope_id)?;
                        ability_constraints.push(signature);
                    }
                    _ => {}
                }
            }
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &ability_param.constraints,
                ) {
                    Ok(Some(parsed_constraint)) => {
                        Some(self.eval_type_expr(parsed_constraint, ability_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => return failf!(ability_param.span, "{}", msg),
                };

            let param_type_id = self.add_type_parameter(
                TypeParameter {
                    name: ability_param.name,
                    static_constraint: maybe_static_constraint,
                    scope_id: ability_scope_id,
                    span: ability_param.span,
                },
                ability_constraints,
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
        for (index, parsed_function_id) in parsed_ability.functions.iter().enumerate() {
            let Some(function_id) = self.compile_function_declaration(
                *parsed_function_id,
                ability_scope_id,
                Some(FunctionAbilityContextInfo::ability_id_only(ability_id)),
                namespace_id,
            )?
            else {
                // Note: compile_function_declaration only returns None when conditional
                // compilation disables it, but I don't think we should allow conditionally
                // including or excluding ability functions? Or maybe its fine, an ability could
                // have an extra function on Windows only for example? Still, maybe you'd rather push
                // platform differences down into implementations, but who am I to say?
                continue;
            };
            let function_name = self.get_function(function_id).name;
            match self.function_name_to_ability.entry(function_name) {
                Entry::Occupied(mut ability_ids) => {
                    ability_ids.get_mut().push(ability_id);
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(eco_vec![ability_id]);
                }
            }
            typed_functions.push(TypedAbilityFunctionRef {
                function_name,
                index: index as u32,
                ability_id,
                function_id,
            });
        }
        self.abilities.get_mut(ability_id).functions = typed_functions;
        Ok(ability_id)
    }

    fn find_ability_or_declare(
        &mut self,
        ability_name: &QIdent,
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
                    let ability_id =
                        self.compile_ability_definition(pending_ability, ability_scope)?;
                    Ok(ability_id)
                }
            }
        })
    }

    fn declare_ability_impl(
        &mut self,
        parsed_id: ParsedAbilityImplId,
        scope_id: ScopeId,
    ) -> TyperResult<AbilityImplId> {
        let parsed_ability_impl = self.ast.get_ability_impl(parsed_id).clone();
        let span = parsed_ability_impl.span;
        let ability_expr = self.ast.p_ability_exprs.get(parsed_ability_impl.ability_expr).clone();
        let parsed_impl_functions = &parsed_ability_impl.functions;

        let impl_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::AbilityImpl, None, None);

        let mut blanket_type_params: SV4<NameAndType> =
            SmallVec::with_capacity(parsed_ability_impl.generic_impl_params.len());
        for blanket_impl_param in &parsed_ability_impl.generic_impl_params {
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &blanket_impl_param.constraints,
                ) {
                    Ok(Some(parsed_constraint)) => {
                        Some(self.eval_type_expr(parsed_constraint, impl_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => return failf!(blanket_impl_param.span, "{}", msg),
                };
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: blanket_impl_param.name,
                    static_constraint: maybe_static_constraint,
                    scope_id: impl_scope_id,
                    span: blanket_impl_param.span,
                },
                // We create the variable with no ability constraints, then add them later, so that its
                // constraints can reference itself
                // Example: impl[T] Add[Rhs = T where T: Num]
                // The constraints need T to exist
                smallvec![],
            );
            if !self
                .scopes
                .get_scope_mut(impl_scope_id)
                .add_type(blanket_impl_param.name, type_variable_id)
            {
                return failf!(
                    blanket_impl_param.span,
                    "Duplicate generic impl parameter name: {}",
                    self.ident_str(blanket_impl_param.name)
                );
            }

            if !blanket_impl_param.constraints.is_empty() {
                let param_constraints_scope_id =
                    self.scopes.add_child_scope(impl_scope_id, ScopeType::AbilityImpl, None, None);
                for ability_expr in
                    blanket_impl_param.constraints.iter().filter_map(|c| c.as_ability())
                {
                    let constrained_ability_sig =
                        self.eval_ability_expr(ability_expr, false, impl_scope_id)?;
                    let constraint_span = self.ast.p_ability_exprs.get(ability_expr).span;
                    self.add_constrained_ability_impl(
                        type_variable_id,
                        constrained_ability_sig,
                        param_constraints_scope_id,
                        constraint_span,
                    );
                }
            }
            blanket_type_params
                .push(NameAndType { name: blanket_impl_param.name, type_id: type_variable_id });
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
            .add_type(self.ast.idents.b.Self_, impl_self_type);

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

            let Some(matching_arg_type_expr) = matching_arg.type_expr else {
                return failf!(matching_arg.span, "_ is supported here");
            };
            let arg_type = self.eval_type_expr(matching_arg_type_expr, impl_scope_id)?;

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

        // Report extra functions first
        for &parsed_fn in parsed_impl_functions {
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

        let mut typed_functions = self.mem.new_list(ability.functions.len() as u32);
        for ability_function_ref in &ability.functions {
            let matching_impl_function = parsed_impl_functions.iter().find_map(|&fn_id| {
                let the_fn = self.ast.get_function(fn_id);
                if the_fn.name == ability_function_ref.function_name {
                    Some((fn_id, false))
                } else {
                    None
                }
            });
            let (parsed_impl_function_id, is_default) = match matching_impl_function {
                Some(id) => id,
                None => {
                    let defn_fn = self.get_function(ability_function_ref.function_id);
                    let parsed_function =
                        self.ast.get_function(defn_fn.parsed_id.as_function_id().unwrap());

                    // If the ability declaration itself has a default implementation
                    // for this function, compile that
                    match parsed_function.body {
                        Some(_) => (defn_fn.parsed_id.as_function_id().unwrap(), true),
                        None => {
                            return failf!(
                                span,
                                "Missing implementation for function '{}' in ability '{}'",
                                self.ident_str(ability_function_ref.function_name).blue(),
                                self.ident_str(ability_name).blue()
                            );
                        }
                    }
                }
            };

            let impl_function_id = self
                .compile_function_declaration(
                    parsed_impl_function_id,
                    impl_scope_id,
                    Some(FunctionAbilityContextInfo::ability_impl(
                        ability_id,
                        impl_self_type,
                        kind,
                        None,
                        is_default,
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
                let impl_function_span = self.ast.get_function(parsed_impl_function_id).span;
                return failf!(
                    impl_function_span,
                    "Invalid implementation of {} in ability {}: {msg}",
                    self.ast.idents.get_name(ability_function_ref.function_name),
                    self.ast.idents.get_name(ability_name)
                );
            }
            typed_functions.push(AbilityImplFunction::FunctionId(impl_function_id));
        }

        let blanked_type_params_handle = self.named_types.add_slice_copy(&blanket_type_params);
        let impl_arguments_handle = self.named_types.add_slice_copy(&impl_arguments);
        let typed_impl_id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: blanked_type_params_handle,
            self_type_id: impl_self_type,
            ability_id,
            base_ability_id,
            impl_arguments: impl_arguments_handle,
            functions: self.mem.vec_to_mslice(&typed_functions),
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
    fn compile_ability_impl_bodies(
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

        for impl_fn in self.mem.getn(ability_impl.functions).iter() {
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

    fn compile_definition_body(
        &mut self,
        def: ParsedId,
        scope_id: ScopeId,
        skip_defns: &[ParsedId],
    ) {
        match def {
            ParsedId::Use(parsed_use_id) => {
                if let Err(e) = self.eval_use_definition(scope_id, parsed_use_id) {
                    self.report_error(e)
                }
            }
            ParsedId::Namespace(namespace) => {
                self.compile_ns_body(namespace, skip_defns);
            }
            ParsedId::Global(global_id) => {
                if let Err(e) = self.eval_global_body(global_id) {
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
                if let Err(e) = self.compile_ability_impl_bodies(ability_impl, scope_id) {
                    self.report_error(e);
                };
            }
            ParsedId::StaticDefn(static_expr_id) => {
                let ParsedExpr::Static(s) = self.ast.exprs.get(static_expr_id) else {
                    unreachable!()
                };
                let s = *s;
                let is_metaprogram = s.kind.is_metaprogram();
                debug_assert!(s.is_definition);
                // For value programs, we want to run them in the body phase
                // so that they have access to as much code as possible
                if !is_metaprogram {
                    let should_compile =
                        self.execute_static_condition(s.condition_if_definition, scope_id);

                    if should_compile {
                        let static_ctx =
                            StaticExecContext { is_metaprogram, expected_return_type: None };
                        let eval_expr_ctx = EvalExprContext {
                            scope_id,
                            expected_type_id: None,
                            static_ctx: Some(static_ctx),
                            global_defn_name: None,
                            flags: EvalExprFlags::empty(),
                        };
                        if let Err(e) =
                            self.eval_static_expr_and_exec(static_expr_id, s, eval_expr_ctx)
                        {
                            self.report_error(e);
                        };
                    }
                }
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
            debug!("Handling unfulfilled use {}", self.qident_to_string(&parsed_use.target));
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
        name: &QIdent,
    ) -> TyperResult<Option<UseableSymbol>> {
        let scope_id_to_search = self.scopes.traverse_namespace_chain(
            scope_id,
            name.path,
            &self.namespaces,
            &self.ast.idents,
            name.span,
        )?;
        let scope_to_search = self.scopes.get_scope(scope_id_to_search);

        debug!(
            "Searching scope for useable symbol: {}, Functions:\n{:?}",
            self.scopes.scope_name_to_string(scope_to_search, &self.ast.idents),
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
    // as well as ability defns, which are like types, and registering their names
    // then recursing down into child namespaces and doing the same
    fn declare_types_in_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) -> TyperResult<()> {
        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;
        for &parsed_definition_id in
            self.ast.namespaces.get(parsed_namespace_id).definitions.clone().iter()
        {
            if skip_defns.contains(&parsed_definition_id) {
                continue;
            }
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
                    if namespace_scope_id == self.scopes.core_scope_id {
                        if name == self.ast.idents.b.string {
                            self.types.builtins.string = Some(type_id);
                        } else if name == self.ast.idents.b.Buffer {
                            self.types.builtins.buffer = Some(type_id);
                        }
                    } else if namespace_scope_id == self.scopes.types_scope_id {
                        if name == self.ast.idents.b.TypeSchema {
                            self.types.builtins.types_type_schema = Some(type_id);
                        } else if name == self.ast.idents.b.IntKind {
                            self.types.builtins.types_int_kind = Some(type_id);
                        } else if name == self.ast.idents.b.IntValue {
                            self.types.builtins.types_int_value = Some(type_id)
                        } else if name == self.ast.idents.b.Layout {
                            self.types.builtins.types_layout = Some(type_id)
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
                if let Err(e) = self.declare_types_in_namespace(namespace_id, skip_defns) {
                    self.report_error(e);
                }
            }
        }
        Ok(())
    }

    fn eval_namespace_type_eval_phase(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) -> TyperResult<()> {
        let namespace_id = self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace = self.namespaces.get(*namespace_id);
        let namespace_scope_id = namespace.scope_id;
        let parsed_namespace = self.ast.namespaces.get(parsed_namespace_id);

        for parsed_definition_id in parsed_namespace.definitions.clone().iter() {
            if skip_defns.contains(parsed_definition_id) {
                continue;
            }
            if let ParsedId::TypeDefn(type_defn_id) = parsed_definition_id {
                if let Err(e) = self.eval_type_defn(*type_defn_id, namespace_scope_id) {
                    self.type_defn_stack.clear();
                    self.report_error(e);
                };
            }
            if let ParsedId::Namespace(namespace_id) = parsed_definition_id {
                if let Err(e) = self.eval_namespace_type_eval_phase(*namespace_id, skip_defns) {
                    self.report_error(e);
                }
            }
        }
        Ok(())
    }

    fn declare_namespace_definitions(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) {
        let parsed_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace = self.namespaces.get(namespace_id);
        let namespace_scope_id = namespace.scope_id;
        for defn in &parsed_namespace.definitions.clone() {
            if skip_defns.contains(defn) {
                continue;
            }
            match *defn {
                ParsedId::Use(_use_id) => {}
                ParsedId::Namespace(namespace_id) => {
                    self.declare_namespace_definitions(namespace_id, skip_defns)
                }
                ParsedId::Global(constant_id) => {
                    if let Err(e) = self.declare_global(constant_id, namespace_scope_id) {
                        self.report_error(e);
                    }
                }
                ParsedId::Function(parsed_function_id) => {
                    if let Err(e) = self.compile_function_declaration(
                        parsed_function_id,
                        namespace_scope_id,
                        None,
                        namespace_id,
                    ) {
                        self.report_error(e);
                    }
                }
                ParsedId::TypeDefn(_type_defn_id) => {
                    // Handled by prior phase
                }
                ParsedId::Ability(parsed_ability_id) => {
                    if let Err(e) =
                        self.compile_ability_definition(parsed_ability_id, namespace_scope_id)
                    {
                        self.report_error(e)
                    };
                }
                ParsedId::AbilityImpl(ability_impl) => {
                    if let Err(e) = self.declare_ability_impl(ability_impl, namespace_scope_id) {
                        self.report_error(e)
                    }
                }
                ParsedId::StaticDefn(_) => {
                    // StaticDefns are handled in either the namespace declaration phase (for
                    // metaprograms) or the body phase (for value programs)
                }
                other_id => {
                    panic!(
                        "Was asked to eval definition of a non-definition ast node {:?}",
                        other_id
                    )
                }
            }
        }
    }

    fn compile_ns_body(&mut self, ast_namespace_id: ParsedNamespaceId, skip_defns: &[ParsedId]) {
        let ast_namespace = self.ast.namespaces.get(ast_namespace_id).clone();
        let namespace_id = *self.namespace_ast_mappings.get(&ast_namespace.id).unwrap();
        let ns_scope_id = self.namespaces.get(namespace_id).scope_id;
        for defn in &ast_namespace.definitions {
            if skip_defns.contains(defn) {
                continue;
            }
            self.compile_definition_body(*defn, ns_scope_id, skip_defns);
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

        let is_core = parent_scope_id == Scopes::ROOT_SCOPE_ID && name == self.ast.idents.b.core;
        if is_core {
            self.scopes.core_scope_id = ns_scope_id;
        }
        let is_k1 = parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.k1;
        if is_k1 {
            self.scopes.k1_scope_id = ns_scope_id;
        }
        let is_mem = parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.mem;
        if is_mem {
            self.scopes.mem_scope_id = ns_scope_id
        }
        let is_types =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.types;
        if is_types {
            self.scopes.types_scope_id = ns_scope_id;
        }
        let is_array =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.Array;
        if is_array {
            self.scopes.array_scope_id = ns_scope_id;
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

    fn declare_namespace(
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
        Ok(namespace_id)
    }

    fn declare_namespaces_in_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id).clone();

        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;

        let mut defns_to_add: SV4<(usize, EcoVec<ParsedId>)> = smallvec![];
        for (index, defn) in ast_namespace.definitions.iter().enumerate() {
            if skip_defns.contains(defn) {
                continue;
            }
            match *defn {
                ParsedId::Namespace(namespace_id) => {
                    if let Err(e) = self.declare_namespace_recursive(
                        namespace_id,
                        namespace_scope_id,
                        skip_defns,
                    ) {
                        self.report_error(e)
                    }
                }
                ParsedId::StaticDefn(static_expr_id) => {
                    let ParsedExpr::Static(s) = self.ast.exprs.get(static_expr_id) else {
                        unreachable!()
                    };
                    let s = *s;
                    let is_metaprogram = s.kind.is_metaprogram();
                    if !is_metaprogram {
                        continue;
                    }
                    debug_assert!(s.is_definition);

                    let should_compile = self
                        .execute_static_condition(s.condition_if_definition, namespace_scope_id);

                    if !should_compile {
                        continue;
                    }

                    // Metaprogram top-level evaluation
                    // We simply run the program, getting a string,
                    // parse it as definitions, then we load those definitions
                    // injecting them into the AST, as if they appeared right here.
                    // If it is a namespace, we ensure we handle it right now, as this is the phase that
                    // they should be handled. Everything else will get handled naturally
                    // as we iterate over the namespace's definitions in the future phases
                    let static_ctx = StaticExecContext {
                        is_metaprogram,
                        expected_return_type: Some(I32_TYPE_ID),
                    };
                    let eval_expr_ctx = EvalExprContext {
                        scope_id: namespace_scope_id,
                        expected_type_id: None,
                        static_ctx: Some(static_ctx),
                        global_defn_name: None,
                        flags: EvalExprFlags::empty(),
                    };
                    let newly_parsed_defns =
                        match self.eval_static_expr_and_exec(static_expr_id, s, eval_expr_ctx) {
                            Err(e) => {
                                self.report_error(e);
                                eco_vec![]
                            }
                            Ok(StaticExecutionResult::Definitions(defns)) => defns,
                            Ok(StaticExecutionResult::TypedExpr(_)) => unreachable!(),
                        };
                    // If any of the meta definitions are themselves namespaces,
                    // we need to run them now, in-loop, so that the program behaves
                    // exactly as if they had been literally written in place
                    //
                    // We'll add them to the AST definitions later so that further
                    // passes can find them
                    for &d in newly_parsed_defns.iter() {
                        if let ParsedId::Namespace(ns) = d {
                            if let Err(e) =
                                self.declare_namespace_recursive(ns, namespace_scope_id, skip_defns)
                            {
                                self.report_error(e)
                            };
                        }
                    }
                    // We want to insert a given #meta's definitions right
                    // after that meta, hence the +1
                    defns_to_add.push((index + 1, newly_parsed_defns));
                }
                _ => {}
            }
        }
        // Reduce EcoVec refcount explicitly before we mutate
        drop(ast_namespace);

        let ast_namespace = self.ast.namespaces.get_mut(parsed_namespace_id);
        for (insertion_index, defns) in defns_to_add.iter() {
            for (i, defn) in defns.iter().enumerate() {
                ast_namespace.definitions.insert(*insertion_index + i, *defn);
            }
        }
    }

    fn declare_namespace_recursive(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: ScopeId,
        skip_defns: &[ParsedId],
    ) -> TyperResult<NamespaceId> {
        let ns_id = self.declare_namespace(parsed_namespace_id, parent_scope)?;
        self.declare_namespaces_in_namespace(parsed_namespace_id, skip_defns);
        Ok(ns_id)
    }

    pub fn run_on_module(
        &mut self,
        module_name: Ident,
        module_root_parsed_namespace: ParsedNamespaceId,
        manifest: ModuleManifest,
        home_dir: PathBuf,
    ) -> anyhow::Result<ModuleId> {
        let module_id = self.modules.next_id();
        self.module_in_progress = Some(module_id);

        // Namespace phase
        debug!(">> Phase 0 declare root namespace");
        let root_namespace_declare_result =
            self.declare_namespace(module_root_parsed_namespace, Scopes::ROOT_SCOPE_ID);

        if let Err(e) = root_namespace_declare_result {
            self.report_error(e);
            bail!(
                "{} failed namespace declaration phase with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }
        let typed_namespace_id = root_namespace_declare_result.unwrap();
        let root_namespace_scope_id = self.namespaces.get(typed_namespace_id).scope_id;
        let real_module_id = self.modules.add(Module {
            id: module_id,
            name: module_name,
            home_dir,
            manifest,
            namespace_id: typed_namespace_id,
            namespace_scope_id: root_namespace_scope_id,
        });
        debug_assert_eq!(module_id, real_module_id);

        let is_core = module_id == MODULE_ID_CORE;
        if !is_core {
            self.add_core_uses_to_scope(root_namespace_scope_id, SpanId::NONE)?;
        }

        // Meta phase: Find pre namespace, if exists, and fully compile it
        let mut pre_ns_id: Option<ParsedId> = None;
        let parsed_ns = self.ast.namespaces.get(module_root_parsed_namespace);
        if !is_core {
            if let Some(pre_ns_parsed_id) = parsed_ns
                .definitions
                .iter()
                .filter_map(|d| d.as_namespace_id())
                .find(|id| self.ast.namespaces.get(*id).name == self.ast.idents.b.pre)
            {
                // Phase 1
                // nocommit 4 finish or remove the sticky message attempt
                // self.phase = 0;
                // self.msg(ErrorLevel::Info, format_args!(""));
                debug!(">> Phase 0.5 compile pre namespace");
                self.declare_namespace(pre_ns_parsed_id, root_namespace_scope_id)?;
                self.run_all_phases_on_ns(pre_ns_parsed_id, module_id, &[])?;
                pre_ns_id = Some(ParsedId::Namespace(pre_ns_parsed_id));
            }
        }

        let skip_defns = match pre_ns_id {
            None => &[][..],
            Some(id) => &[id],
        };
        self.run_all_phases_on_ns(module_root_parsed_namespace, module_id, skip_defns)?;

        Ok(module_id)
    }

    fn run_all_phases_on_ns(
        &mut self,
        module_root_parsed_namespace: ParsedNamespaceId,
        module_id: ModuleId,
        skip_defns: &[ParsedId],
    ) -> anyhow::Result<()> {
        let mut err_writer = stderr();
        debug!(">> Phase 1 declare namespaces and run global #meta programs");
        self.declare_namespaces_in_namespace(module_root_parsed_namespace, skip_defns);
        if !self.errors.is_empty() {
            bail!(
                "{} failed namespace declaration phase with {} errors",
                self.program_name(),
                self.errors.len()
            )
        }

        // Pending Type declaration phase
        debug!(">> Phase 2 declare types");
        let type_defn_result =
            self.declare_types_in_namespace(module_root_parsed_namespace, skip_defns);
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

        //self.phase = 3;
        // self.info(format_args!(""));
        debug!(">> Phase 3 evaluate types");
        let type_eval_result =
            self.eval_namespace_type_eval_phase(module_root_parsed_namespace, skip_defns);
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

        debug_assert_eq!(self.types.types.len(), self.types.type_phys_type_lookup.len());
        debug_assert_eq!(self.types.types.len(), self.types.type_variable_counts.len());

        for type_id in self.types.iter_ids().collect_vec() {
            if let Type::Unresolved(ast_id) = self.types.get(type_id) {
                let span = self.ast.get_span_for_id(ParsedId::TypeDefn(*ast_id));
                self.ice_with_span("Unresolved type definition", span)
            }
        }

        if module_id == MODULE_ID_CORE {
            let fields = self.types.mem.pushn(&[
                StructTypeField { name: self.ast.idents.b.env, type_id: POINTER_TYPE_ID },
                StructTypeField { name: self.ast.idents.b.fn_ptr, type_id: POINTER_TYPE_ID },
            ]);
            let t = self.types.add_anon(Type::Struct(StructType { fields }));
            self.types.builtins.dyn_lambda_obj = Some(t);
            self.assert_builtin_types_correct();
        }

        // Everything else declaration phase
        debug!(">> Phase 4 declare rest of definitions (functions, globals, abilities)");
        self.declare_namespace_definitions(module_root_parsed_namespace, skip_defns);
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

        debug!(">> Phase 5 bodies (functions, globals, abilities)");
        self.compile_ns_body(module_root_parsed_namespace, skip_defns);

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

        debug!(">> Phase 6 specialize function bodies");
        self.specialize_pending_function_bodies(&mut err_writer)?;
        if !self.errors.is_empty() {
            bail!("{} failed specialize with {} errors", self.program_name(), self.errors.len())
        }

        Ok(())
    }

    fn assert_builtin_types_correct(&self) {
        debug_assert!(self.types.builtins.string.is_some());
        debug_assert!(self.types.builtins.buffer.is_some());
        debug_assert!(self.types.builtins.dyn_lambda_obj.is_some());
        debug_assert!(self.types.builtins.types_layout.is_some());
        debug_assert!(self.types.builtins.types_type_schema.is_some());
        debug_assert!(self.types.builtins.types_int_kind.is_some());
        debug_assert!(self.types.builtins.types_int_value.is_some());
        //
        // This just ensures our BUFFER_TYPE_ID constant is correct
        // Eventually we need a better way of doing this
        {
            let buffer_generic = self.types.get(BUFFER_TYPE_ID).expect_generic();
            let buffer_struct = self.types.get(buffer_generic.inner).expect_struct();
            // debug_assert_eq!(
            //     self.types.get_layout(BUFFER_TYPE_ID),
            //     Layout::from_rust_type::<vm::k1_types::K1ViewLike>()
            // );
            debug_assert!(buffer_struct.fields.len() == 2);
            debug_assert!(
                self.types
                    .mem
                    .getn(buffer_struct.fields)
                    .iter()
                    .map(|f| self.ident_str(f.name))
                    .collect::<SV2<_>>()[..]
                    == ["len", BUFFER_DATA_FIELD_NAME]
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
                self.types
                    .mem
                    .getn(list_struct.fields)
                    .iter()
                    .map(|f| self.ident_str(f.name))
                    .collect::<SV2<_>>()[..]
                    == ["len", "buffer"]
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
        &mut self,
        type_id: TypeId,
        dst: &mut Vec<PatternCtorTrialEntry>,
        field_ctors_buf: &mut Vec<Vec<(Ident, PatternCtorId)>>,
        span_id: SpanId,
    ) {
        #[inline]
        fn alive(ctor: PatternCtorId) -> PatternCtorTrialEntry {
            PatternCtorTrialEntry { ctor, alive: true }
        }
        if type_id == STRING_TYPE_ID {
            dst.push(alive(PatternCtorId::STRING));
            return;
        }
        match self.types.get(type_id) {
            Type::Unit => dst.push(alive(PatternCtorId::UNIT)),
            Type::Char => dst.push(alive(PatternCtorId::CHAR)),
            Type::TypeParameter(_) => dst.push(alive(PatternCtorId::TYPE_VARIABLE)),
            Type::Integer(_) => dst.push(alive(PatternCtorId::INT)),
            Type::Float(_) => dst.push(alive(PatternCtorId::FLOAT)),
            Type::Bool => {
                dst.extend(&[alive(PatternCtorId::B_FALSE), alive(PatternCtorId::B_TRUE)])
            }
            Type::Pointer => dst.push(alive(PatternCtorId::POINTER)), // Just an opaque atom
            Type::FunctionPointer(_) => {
                dst.push(alive(PatternCtorId::FUNCTION_POINTER)) // FunctionPointer is an opaque atom pattern
            }
            Type::Reference(refer) => {
                // Follow the pointer
                let prev_len = dst.len();
                self.generate_constructors_for_type(
                    refer.inner_type,
                    dst,
                    field_ctors_buf,
                    span_id,
                );
                for pointee_pattern_id in dst[prev_len..].iter_mut() {
                    pointee_pattern_id.ctor =
                        self.pattern_ctors.add(PatternCtor::Reference(pointee_pattern_id.ctor));
                }
            }
            Type::Array(_array_type) => dst.push(alive(self.pattern_ctors.add(PatternCtor::Array))),
            Type::Enum(enum_type) => {
                for v in enum_type.variants.clone().iter() {
                    match v.payload.as_ref() {
                        None => dst.push(alive(
                            self.pattern_ctors
                                .add(PatternCtor::Enum { variant_name: v.name, inner: None }),
                        )),
                        Some(payload) => {
                            let prev_len = dst.len();
                            self.generate_constructors_for_type(
                                *payload,
                                dst,
                                field_ctors_buf,
                                span_id,
                            );
                            for payload_pattern_id in dst[prev_len..].iter_mut() {
                                payload_pattern_id.ctor =
                                    self.pattern_ctors.add(PatternCtor::Enum {
                                        variant_name: v.name,
                                        inner: Some(payload_pattern_id.ctor),
                                    })
                            }
                        }
                    }
                }
            }
            Type::Struct(struc) => {
                debug_assert!(type_id != STRING_TYPE_ID);
                let field_count = struc.fields.len();
                for (index, field) in self.types.mem.getn(struc.fields).iter().enumerate() {
                    let prev_len = dst.len();
                    self.generate_constructors_for_type(
                        field.type_id,
                        dst,
                        field_ctors_buf,
                        span_id,
                    );
                    match field_ctors_buf.get_mut(index) {
                        None => field_ctors_buf.push(Vec::with_capacity(128)),
                        Some(buf) => buf.clear(),
                    };
                    for field_ctor in dst[prev_len..].iter() {
                        field_ctors_buf[index].push((field.name, field_ctor.ctor));
                    }
                    debug!(
                        "Pushed {} constructors for field {}; resetting dst to {prev_len}",
                        field_ctors_buf[index].len(),
                        self.ident_str(field.name)
                    );
                    dst.truncate(prev_len);
                }
                let final_count = field_ctors_buf[0..field_count as usize]
                    .iter()
                    .map(|v| v.len())
                    .reduce(|t, v| t * v)
                    .unwrap_or(0);

                debug!(
                    "Processing {} ctors; expecting {final_count} final struct combinations for type: {}",
                    field_ctors_buf.len(),
                    self.type_id_to_string(type_id)
                );
                let dst_start = dst.len();
                for _ in 0..final_count {
                    let primed_struct_ctor_id = self.pattern_ctors.add(PatternCtor::Struct {
                        fields: SmallVec::with_capacity(field_count as usize),
                    });
                    dst.push(alive(primed_struct_ctor_id));
                }
                let result_struct_ids = &mut dst[dst_start..];

                // This entire loop is just about taking the cross-product of all the fields'
                // respective constructors in an efficient way; by populating slots in
                // a pre-allocated table, and doing a bit of math to decide how many times
                // a pattern should repeat or cycle. Example
                // {  a: bool, b: bool, c: either A, B, C }
                // 0  f        f        A
                // 1  f        f        B
                // 2  f        f        C
                // 3  f        t        A
                // 4  f        t        B
                // 5  f        t        C
                // 6  t        f        A
                // 7  t        f        B
                // 8  t        f        C
                // 9  t        t        A
                // 10 t        t        B
                // 11 t        t        C
                // a has 2 patterns, and is in the first (meaningful) position, so we do 12 / 2 * 1 to get 6 as its 'repeat count', and repeat each pattern 6 times
                // b has 2 patterns, and is in the second (meaningful) position, so we do 12 / 2 * 2 to get 3 as its 'repeat count', and repeat each pattern 3 times (fff, ttt)
                // c has 3 patterns, and is in the third (meaningful) position, so we do 12 / 3 * 4 to get 1 as its 'repeat count', and repeat each pattern 1 time (abc, abc, abc)
                let mut field_index_w_multi_ctor = 0;
                for ctors in field_ctors_buf[0..field_count as usize].iter() {
                    if ctors.len() == 1 {
                        for result_struct in result_struct_ids.iter_mut() {
                            self.pattern_ctors.get_mut(result_struct.ctor).push_field(ctors[0]);
                        }
                    } else {
                        // multiplier = 2 ^ field_index but only for fields that have more than
                        // 1 pattern
                        let multiplier = if field_index_w_multi_ctor == 0 {
                            1
                        } else {
                            field_index_w_multi_ctor * 2
                        };
                        let repeat_count = final_count / (ctors.len() * multiplier);
                        for (row, result_struct) in result_struct_ids.iter_mut().enumerate() {
                            let pattern_index = (row / repeat_count) % ctors.len();
                            let pattern = ctors[pattern_index];
                            self.pattern_ctors.get_mut(result_struct.ctor).push_field(pattern);
                        }
                        field_index_w_multi_ctor += 1;
                    }
                }
            }
            Type::Function(_f) => {
                debug!("function is probably unmatchable");
            }
            _ => self
                .write_error(
                    &mut stderr(),
                    &errf!(
                        span_id,
                        "unhandled type in generate_constructors_for_type {}",
                        self.type_id_to_string(type_id)
                    ),
                )
                .unwrap(),
        }
    }

    fn pattern_matches(
        ctors: &VPool<PatternCtor, PatternCtorId>,
        patterns: &TypedPatternPool,
        pattern: TypedPatternId,
        ctor: PatternCtorId,
    ) -> bool {
        match (patterns.get(pattern), ctors.get(ctor)) {
            (TypedPattern::Wildcard(_), _) => true,
            (TypedPattern::Variable(_), _) => true,
            (TypedPattern::LiteralUnit(_), PatternCtor::Unit) => true,
            (TypedPattern::LiteralBool(true, _), PatternCtor::BoolTrue) => true,
            (TypedPattern::LiteralBool(false, _), PatternCtor::BoolFalse) => true,
            (TypedPattern::Enum(enum_pat), PatternCtor::Enum { variant_name, inner }) => {
                if *variant_name == enum_pat.variant_tag_name {
                    match (enum_pat.payload, inner) {
                        (Some(payload), Some(inner)) => {
                            TypedProgram::pattern_matches(ctors, patterns, payload, *inner)
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
                for field_pattern in patterns.get_slice(struc.fields).iter() {
                    let matching_field_pattern = fields
                        .iter()
                        .find(|(name, _ctor_pattern)| *name == field_pattern.name)
                        .map(|(_, ctor_pattern)| ctor_pattern)
                        .expect("Field not in struct; pattern should have failed typecheck by now");
                    if !TypedProgram::pattern_matches(
                        ctors,
                        patterns,
                        field_pattern.pattern,
                        *matching_field_pattern,
                    ) {
                        matches = false;
                        break;
                    }
                }
                matches
            }
            (TypedPattern::Reference(ref_pattern), PatternCtor::Reference(ref_ctor)) => {
                TypedProgram::pattern_matches(ctors, patterns, ref_pattern.inner_pattern, *ref_ctor)
            }
            _ => {
                // eprintln!(
                //     "Unhandled pattern_matches case: {:?} {:?}",
                //     self.pattern_to_string(pattern),
                //     ctor
                // );
                false
            }
        }
    }

    fn add_core_uses_to_scope(&mut self, scope: ScopeId, span: SpanId) -> TyperResult<()> {
        let root_ns: IdentSlice =
            self.ast.idents.slices.add_slice_copy(&[self.ast.idents.b.root_module_name]);
        let core_ns: IdentSlice = self.ast.idents.slices.add_slice_copy(&[self.ast.idents.b.core]);

        let core_mem: IdentSlice =
            self.ast.idents.slices.add_slice_copy(&[self.ast.idents.b.core, self.ast.idents.b.mem]);

        macro_rules! core {
            ($name: expr) => {
                QIdent { path: core_ns, name: get_ident!(self, $name), span }
            };
        }

        let idents_to_use = [
            QIdent { path: root_ns, name: self.ast.idents.b.core, span }, // use _root/core;
            QIdent { path: root_ns, name: self.ast.idents.b.std, span },  // use _root/std;
            core!("u8"),
            core!("u16"),
            core!("u32"),
            core!("u64"),
            core!("i8"),
            core!("i16"),
            core!("i32"),
            core!("i64"),
            core!("byte"),
            core!("int"),
            core!("uint"),
            core!("size"),
            core!("usize"),
            core!("unit"),
            core!("char"),
            core!("bool"),
            core!("never"),
            core!("ptr"),
            core!("f32"),
            core!("f64"),
            core!("Buffer"),
            core!("View"),
            core!("Array"),
            core!("List"),
            core!("string"),
            core!("Opt"),
            core!("some"),
            core!("none"),
            core!("Ordering"),
            core!("Result"),
            core!("Equals"),
            core!("Writer"),
            core!("Print"),
            core!("Show"),
            core!("Bitwise"),
            core!("Comparable"),
            core!("Unwrap"),
            core!("Try"),
            core!("Iterator"),
            core!("Iterable"),
            core!("println"),
            core!("print"),
            core!("eprint"),
            core!("eprintln"),
            core!("printIt"),
            core!("identity"),
            core!("assert"),
            core!("assertEquals"),
            core!("assertMsg"),
            core!("crash"),
            core!("meta"),
            core!("mem"),
            core!("types"),
            core!("k1"),
            core!("IntRange"),
            core!("Add"),
            core!("Sub"),
            core!("Mul"),
            core!("Div"),
            core!("Rem"),
            core!("ScalarCmp"),
            QIdent { path: core_mem, name: get_ident!(self, "zeroed"), span },
        ];
        for qid in idents_to_use.into_iter() {
            let use_id = self.ast.uses.add_use(parse::ParsedUse { target: qid, alias: None, span });
            self.eval_use_definition(scope, use_id)?;
        }
        Ok(())
    }

    // fn add_std_uses_to_scope(&mut self, scope: ScopeId, span: SpanId) -> TyperResult<()> {
    //     let std_ns: IdentSlice = self.ast.idents.slices.add_slice_copy(&[self.ast.idents.b.std]);
    //     let idents_to_use = [QIdent { path: std_ns, name: get_ident!(self, "HashMap"), span }];
    //     for qid in idents_to_use.into_iter() {
    //         let use_id = self.ast.uses.add_use(parse::ParsedUse { target: qid, alias: None, span });
    //         self.eval_use_definition(scope, use_id)?;
    //     }
    //     Ok(())
    // }

    fn get_type_schema(&mut self, type_id: TypeId, span: SpanId) -> StaticValueId {
        if let Some(static_value_id) = self.type_schemas.get(&type_id) {
            return *static_value_id;
        }

        let type_schema =
            self.types.get(self.types.builtins.types_type_schema.unwrap()).expect_enum().clone();
        let int_kind_enum =
            self.types.get(self.types.builtins.types_int_kind.unwrap()).expect_enum();
        let get_schema_variant = |ident| type_schema.variant_by_name(ident).unwrap();
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
        let chased_type_id = self.types.get_chased_id(type_id);
        let typ = self.types.get_no_follow(chased_type_id);
        let static_enum = match typ {
            Type::Unit => make_variant(get_ident!(self, "Unit"), None),
            Type::Char => make_variant(get_ident!(self, "Char"), None),
            Type::Bool => make_variant(get_ident!(self, "Bool"), None),
            Type::Pointer => make_variant(get_ident!(self, "Ptr"), None),
            Type::Integer(integer_type) => {
                let int_kind_enum_value = TypedProgram::make_int_kind(int_kind_enum, *integer_type);

                let payload_value_id =
                    self.static_values.add(StaticValue::Enum(int_kind_enum_value));
                let enum_value = make_variant(get_ident!(self, "Int"), Some(payload_value_id));
                enum_value
            }
            Type::Float(_float_type) => todo!("float schema"),
            Type::Struct(_struct_type) if chased_type_id == STRING_TYPE_ID => {
                make_variant(get_ident!(self, "String"), None)
            }
            Type::Struct(struct_type) => {
                let struct_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Struct")).payload.unwrap();
                // { fields: View[{}] }
                let struct_schema_payload_struct =
                    self.types.get(struct_schema_payload_type_id).expect_struct();
                // { fields: View[{ ... }] }
                let struct_schema_fields_view_type_id =
                    self.types.mem.get_nth(struct_schema_payload_struct.fields, 0).type_id;
                let struct_schema_field_item_struct_type_id =
                    self.types.get_as_view_instance(struct_schema_fields_view_type_id).unwrap();
                // { name: string), typeId: u64, offset: size }
                let struct_layout = self.types.get_struct_layout(type_id);
                let mut field_values: MList<StaticValueId, StaticValuePool> =
                    self.static_values.mem.new_list(struct_type.fields.len());
                for (index, f) in self.types.mem.getn(struct_type.fields).iter().enumerate() {
                    let name_string_id = self.ast.strings.intern(self.ast.idents.get_name(f.name));
                    let name_string_value_id = self.static_values.add_string(name_string_id);

                    // We need to ensure that any and all typeIds that we share with the user
                    // are available at runtime, by calling these functions at least once.
                    self.register_type_metainfo(f.type_id, span);

                    let type_id_value_id = self.static_values.add_type_id_int_value(f.type_id);
                    let offset_u32 = struct_layout[index].offset;
                    let offset_value_id = self.static_values.add_size(offset_u32 as i64);
                    let field_struct_fields = self.static_values.mem.pushn(&[
                        // name: string
                        name_string_value_id,
                        // typeId: u64
                        type_id_value_id,
                        // offset: size
                        offset_value_id,
                    ]);
                    field_values.push(
                        self.static_values.add_struct(
                            struct_schema_field_item_struct_type_id,
                            field_struct_fields,
                        ),
                    );
                }
                let values_slice = self.static_values.mem.vec_to_mslice(&field_values);
                let view =
                    self.static_values.add_view(struct_schema_fields_view_type_id, values_slice);
                let payload = self
                    .static_values
                    .add_struct_from_slice(struct_schema_payload_type_id, &[view]);
                make_variant(get_ident!(self, "Struct"), Some(payload))
            }
            Type::Reference(reference_type) => {
                let reference_type = *reference_type;
                let reference_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Reference")).payload.unwrap();
                // { innerTypeId: u64, mutable: bool }
                let inner_type_id_value_id =
                    self.static_values.add_type_id_int_value(reference_type.inner_type);

                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(reference_type.inner_type, span);

                let mutable_value_id =
                    self.static_values.add(StaticValue::Bool(reference_type.is_mutable()));

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    reference_schema_payload_type_id,
                    &[inner_type_id_value_id, mutable_value_id],
                );
                make_variant(get_ident!(self, "Reference"), Some(payload_struct_id))
            }
            Type::Array(array_type) => {
                let array_type = *array_type;
                let array_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Array")).payload.unwrap();
                // { elementTypeId: u64, size: size }
                let element_type_id_value_id =
                    self.static_values.add_type_id_int_value(array_type.element_type);
                self.register_type_metainfo(array_type.element_type, span);

                let maybe_concrete_size_value_id = match array_type.concrete_count {
                    None => None,
                    Some(size) => Some(self.static_values.add_size(to_k1_size_u64(size))),
                };
                let option_size = self.synth_optional_type(SIZE_TYPE_ID);
                let size_value_id =
                    self.synth_static_option(option_size, maybe_concrete_size_value_id);

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    array_schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(get_ident!(self, "Array"), Some(payload_struct_id))
            }
            Type::Enum(typed_enum) => {
                let either_payload_type_id =
                    get_schema_variant(get_ident!(self, "Either")).payload.unwrap();
                let variants_view_type_id =
                    self.types.get_struct_field(either_payload_type_id, 1).type_id;
                let variant_struct_type_id =
                    self.types.get_as_view_instance(variants_view_type_id).unwrap();
                let tag_type = self.types.get(typed_enum.tag_type).expect_integer();
                let tag_type_value_id = self
                    .static_values
                    .add(StaticValue::Enum(TypedProgram::make_int_kind(int_kind_enum, tag_type)));
                let mut variant_values =
                    self.static_values.mem.new_list(typed_enum.variants.len() as u32);
                for variant in typed_enum.variants.clone().iter() {
                    let name_string_id =
                        self.ast.strings.intern(self.ast.idents.get_name(variant.name));
                    let name_value_id = self.static_values.add_string(name_string_id);

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
                        self.types.get_struct_field(variant_struct_type_id, 2).type_id;
                    let payload_info_struct_id =
                        self.types.get_as_opt_instance(payload_info_opt_type_id).unwrap();

                    let payload_info_value_id = match variant.payload {
                        None => synth_static_option(
                            &self.types,
                            &mut self.static_values,
                            payload_info_opt_type_id,
                            None,
                        ),
                        Some(payload_type_id) => {
                            let type_id_value_id =
                                self.static_values.add_type_id_int_value(payload_type_id);
                            // We need to ensure that any and all typeIds that we share with the user
                            // are available at runtime, by calling these functions at least once.
                            self.register_type_metainfo(payload_type_id, span);

                            let offset = self.types.enum_variant_payload_offset_bytes(variant);
                            let payload_offset_value_id =
                                self.static_values.add_size(to_k1_size_usize(offset));
                            let payload_info_struct_id = self.static_values.add_struct_from_slice(
                                payload_info_struct_id,
                                &[type_id_value_id, payload_offset_value_id],
                            );
                            synth_static_option(
                                &self.types,
                                &mut self.static_values,
                                payload_info_opt_type_id,
                                Some(payload_info_struct_id),
                            )
                        }
                    };

                    variant_values.push(self.static_values.add_struct_from_slice(
                        variant_struct_type_id,
                        &[
                            // name: string,
                            name_value_id,
                            // tag: IntValue,
                            tag_value_id,
                            // payload: { typeId: u64, offset: size }?,
                            payload_info_value_id,
                        ],
                    ))
                }
                let variant_values_slice = self.static_values.mem.vec_to_mslice(&variant_values);
                let variants_view_value_id =
                    self.static_values.add_view(variants_view_type_id, variant_values_slice);
                let payload_value_id = self.static_values.add_struct_from_slice(
                    either_payload_type_id,
                    &[tag_type_value_id, variants_view_value_id],
                );
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
                let enum_type_id_value_id =
                    self.static_values.add_type_id_int_value(variant.enum_type_id);

                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(variant.enum_type_id, span);

                let name_string_id =
                    self.ast.strings.intern(self.ast.idents.get_name(variant_name));
                let name_value_id = self.static_values.add_string(name_string_id);
                let payload_value_id = self.static_values.add_struct_from_slice(
                    variant_payload_type_id,
                    &[enum_type_id_value_id, name_value_id],
                );
                make_variant(get_ident!(self, "Variant"), Some(payload_value_id))
            }
            Type::Never => make_variant(get_ident!(self, "Never"), None),
            Type::Function(fn_type) => {
                let fn_type = fn_type.clone();
                let function_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "Function")).payload.unwrap();
                //Function({
                //  params: View[{ name: string, typeId: u64 }],
                //  returnTypeId: u64,
                //}),
                let function_schema_payload_struct =
                    self.types.get(function_schema_payload_type_id).expect_struct();
                let function_params_view_field =
                    self.types.mem.get_nth(function_schema_payload_struct.fields, 0);
                let function_params_view_type_id = function_params_view_field.type_id;
                let function_param_struct_type_id =
                    self.types.get_as_view_instance(function_params_view_type_id).unwrap();

                let mut params_value_ids =
                    self.static_values.mem.new_list(fn_type.logical_params().len());
                // Skipping lambda environment parameters;
                // knowing what is a lambda is covered by the type
                // kind the function appears within

                for param in self.types.mem.getn(fn_type.logical_params()) {
                    self.register_type_metainfo(param.type_id, span);

                    let param_name_string_id =
                        self.ast.strings.intern(self.ast.idents.get_name(param.name));
                    let param_name_value_id =
                        self.static_values.add(StaticValue::String(param_name_string_id));
                    let param_type_id_value_id =
                        self.static_values.add_type_id_int_value(param.type_id);
                    let param_struct_value_id = self.static_values.add_struct_from_slice(
                        function_param_struct_type_id,
                        &[
                            // name: string
                            param_name_value_id,
                            // typeId: u64
                            param_type_id_value_id,
                        ],
                    );
                    params_value_ids.push(param_struct_value_id)
                }

                let params_value_ids_slice =
                    self.static_values.mem.vec_to_mslice(&params_value_ids);
                let params_view_value_id = self
                    .static_values
                    .add_view(function_params_view_type_id, params_value_ids_slice);

                self.register_type_metainfo(fn_type.return_type, span);
                let return_type_id_value_id =
                    self.static_values.add_type_id_int_value(fn_type.return_type);

                let payload = self.static_values.add_struct_from_slice(
                    function_schema_payload_type_id,
                    &[
                        // params
                        params_view_value_id,
                        // returnTypeId
                        return_type_id_value_id,
                    ],
                );
                make_variant(get_ident!(self, "Function"), Some(payload))
            }
            Type::FunctionPointer(fp) => {
                let function_pointer_schema_payload_type_id =
                    get_schema_variant(get_ident!(self, "FunctionPointer")).payload.unwrap();

                let function_type_id_value_id =
                    self.static_values.add_type_id_int_value(fp.function_type_id);
                self.register_type_metainfo(fp.function_type_id, span);

                let payload = self.static_values.add_struct_from_slice(
                    function_pointer_schema_payload_type_id,
                    &[function_type_id_value_id],
                );
                make_variant(get_ident!(self, "FunctionPointer"), Some(payload))
            }
            Type::Lambda(_) | Type::LambdaObject(_) | Type::TypeParameter(_) => make_variant(
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
                ice_span!(self, span, "TypeSchema on {}", typ.kind_name())
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

    fn get_type_name(&mut self, type_id: TypeId) -> StaticValueId {
        if let Some(existing) = self.type_names.get(&type_id) {
            return *existing;
        }

        let type_string = self.type_id_to_string(type_id);
        let string_id = self.ast.strings.intern(type_string);
        let value_id = self.static_values.add_string(string_id);

        self.type_names.insert(type_id, value_id);
        value_id
    }

    fn register_type_metainfo(&mut self, type_id: TypeId, span: SpanId) {
        let _ = self.get_type_schema(type_id, span);
        let _ = self.get_type_name(type_id);
    }

    fn make_int_kind(int_kind_enum: &TypedEnum, integer_type: IntegerType) -> StaticEnum {
        let int_kind_variant = match integer_type {
            IntegerType::U8 => int_kind_enum.variant_by_index(0),
            IntegerType::U16 => int_kind_enum.variant_by_index(1),
            IntegerType::U32 => int_kind_enum.variant_by_index(2),
            IntegerType::U64 => int_kind_enum.variant_by_index(3),
            IntegerType::I8 => int_kind_enum.variant_by_index(4),
            IntegerType::I16 => int_kind_enum.variant_by_index(5),
            IntegerType::I32 => int_kind_enum.variant_by_index(6),
            IntegerType::I64 => int_kind_enum.variant_by_index(7),
        };
        StaticEnum {
            variant_type_id: int_kind_variant.my_type_id,
            variant_index: int_kind_variant.index,
            typed_as_enum: true,
            payload: None,
        }
    }

    fn make_int_value(
        static_values: &mut StaticValuePool,
        int_value_enum: &TypedEnum,
        integer_value: TypedIntValue,
    ) -> StaticEnum {
        let variant = match integer_value {
            TypedIntValue::U8(_) => int_value_enum.variant_by_index(0),
            TypedIntValue::U16(_) => int_value_enum.variant_by_index(1),
            TypedIntValue::U32(_) => int_value_enum.variant_by_index(2),
            TypedIntValue::U64(_) => int_value_enum.variant_by_index(3),
            TypedIntValue::I8(_) => int_value_enum.variant_by_index(4),
            TypedIntValue::I16(_) => int_value_enum.variant_by_index(5),
            TypedIntValue::I32(_) => int_value_enum.variant_by_index(6),
            TypedIntValue::I64(_) => int_value_enum.variant_by_index(7),
        };
        StaticEnum {
            variant_type_id: variant.my_type_id,
            variant_index: variant.index,
            typed_as_enum: false,
            payload: Some(static_values.add(StaticValue::Int(integer_value))),
        }
    }

    pub fn get_dlopen_handle(
        &mut self,
        lib_name: Ident,
        span: SpanId,
    ) -> TyperResult<*mut std::ffi::c_void> {
        if let Some(handle) = self.vm_library_dlopens.get(&lib_name) {
            return Ok(*handle);
        }

        let lib_name_str = self.ast.idents.get_name(lib_name);
        let lib_filename: PathBuf = match self.ast.config.target.target_os() {
            crate::compiler::TargetOs::Linux => {
                Path::new(&format!("lib{}", lib_name_str)).with_extension("so")
            }
            crate::compiler::TargetOs::MacOs => {
                Path::new(&format!("lib{}", lib_name_str)).with_extension("dylib")
            }
            crate::compiler::TargetOs::Wasm => {
                return failf!(span, "Dynamic libraries are not supported on the wasm target");
            }
        };
        eprintln!("cwd is: {}", std::env::current_dir().unwrap().display());
        eprintln!("src_path is: {}", self.ast.config.src_path.display());
        let lib_path = if lib_name_str == "k1rt" {
            self.ast.config.k1_lib_dir.join(lib_filename)
        } else {
            let mut lib_path = self.ast.config.src_path.clone();
            lib_path.push("libs");
            lib_path.push(lib_filename);
            lib_path
        };
        let c_lib_name = std::ffi::CString::new(lib_path.to_string_lossy().as_bytes()).unwrap();
        let handle = unsafe { libc::dlopen(c_lib_name.as_ptr(), libc::RTLD_LAZY) };
        if handle.is_null() {
            eprintln!(
                "Failed to dlopen library '{}': {}",
                lib_path.display(),
                unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) }.to_string_lossy()
            );
            return failf!(span, "Failed to dlopen library '{}'", lib_name_str);
        }
        self.vm_library_dlopens.insert(lib_name, handle);
        Ok(handle)
    }

    pub fn all_manifest_libs(&self) -> Vec<LibRef> {
        let mut names = vec![];
        for module in self.modules.iter() {
            names.extend(&module.manifest.libs);
        }
        names
    }

    pub fn get_span_location(&self, span: SpanId) -> (&parse::Source, &parse::Line) {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get_source(the_span.file_id);
        let line = source.get_line_for_span_start(the_span).unwrap();
        (source, line)
    }

    pub fn write_qualified_name(
        &self,
        w: &mut impl std::fmt::Write,
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
        let mut buf = String::with_capacity(64);
        self.write_qualified_name(&mut buf, scope, self.ident_str(name), delimiter, skip_root);
        buf
    }

    // Errors and logging

    pub fn report_warning(&mut self, e: TyperError) {
        self.write_error(&mut std::io::stderr(), &e).unwrap();
        self.warnings.push(e);
    }

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

    pub fn sticky_update(&self) {
        // Sticky line
        use std::io::Write;
        write!(std::io::stderr(), "\r>> Phase {}", self.phase).ok();
    }

    pub fn info(&self, format_args: std::fmt::Arguments<'_>) {
        self.msg(ErrorLevel::Info, format_args)
    }

    pub fn msg(&self, level: ErrorLevel, format_args: std::fmt::Arguments<'_>) {
        use std::io::Write;
        let mut err = std::io::stderr();

        // Clear line
        writeln!(err, "\r{}\r", " ".repeat(80)).ok();

        write!(err, "[{}] {}", level, format_args).ok();

        // Sticky line
        write!(err, "\r>> Phase {}", self.phase).ok();
    }

    // Timing
    //
    pub fn print_timing_info(
        &self,
        module_name: &str,
        module_elapsed_ms: u64,
        out: &mut impl std::io::Write,
    ) -> std::io::Result<()> {
        let infer_ms = self.timing.total_infer_nanos as f64 / 1_000_000.0;
        let vm_ms = self.timing.total_vm_nanos as f64 / 1_000_000.0;
        eprintln!("module {} took {}ms", module_name, module_elapsed_ms);
        eprintln!("\t{} expressions", self.exprs.len());
        eprintln!("\t{} statements", self.stmts.len());
        eprintln!("\t{} functions", self.functions.len());
        eprintln!("\t{} types", self.types.type_count());
        eprintln!("\t{} idents", self.ast.idents.len());
        eprintln!(
            "\t{} instructions, {}ms bc",
            self.bytecode.instrs.len(),
            self.timing.total_bytecode_nanos / 1_000_000
        );
        self.tmp.print_usage("\tmem tmp used");
        self.mem.print_usage("\tmem mem used");
        self.types.mem.print_usage("\tmem types used");
        self.bytecode.mem.print_usage("\tmem bytecode used");
        writeln!(
            out,
            "\t{} infers: {:.2}ms. avg: {:.2}ms ",
            self.timing.total_infers,
            infer_ms,
            if self.timing.total_infers > 0 {
                infer_ms / self.timing.total_infers as f64
            } else {
                0.0
            }
        )?;
        writeln!(out, "\t{:.2}ms vm", vm_ms)?;
        Ok(())
    }
}

fn to_k1_size_u64(value: u64) -> i64 {
    let v = value as i64;
    if v < 0 {
        panic!("Negative k1 size: {value}");
    };
    v
}

fn to_k1_size_usize(value: usize) -> i64 {
    let v = value as i64;
    if v < 0 {
        panic!("Negative k1 size: {value}");
    };
    v
}

#[cfg(test)]
mod static_value_test;
