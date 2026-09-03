// Copyright (c) 2026 knix
// All rights reserved.

pub(crate) mod derive;
pub(crate) mod dump;
pub(crate) mod infer;
pub(crate) mod megarepl;
mod pattern_match;
pub(crate) mod reflect;
pub(crate) mod report;
pub(crate) mod scopes;
pub(crate) mod snapshot;
pub(crate) mod static_exec;
pub(crate) mod static_value;
pub(crate) mod synth;
pub(crate) mod type_eval;
pub(crate) mod typed_int_value;
pub(crate) mod types;
pub(crate) mod visit;

use crate::ir::{AtomicOrderingIr, BackendBuiltin, IrUnitId};
use crate::typer::megarepl::MegareplState;
use crate::{bc, clock, compiler, debug, ir, k1_format, k1_format_user, kbail, kerr, kwarn, vm};
use bitflags::bitflags;
use itertools::Itertools;
pub use static_value::{
    StaticContainer, StaticContainerKind, StaticRawContainer, StaticStruct, StaticSum, StaticValue,
    StaticValueId, StaticValuePool,
};
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::collections::hash_map::Entry;
use std::ffi::c_void;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::io::IsTerminal;
use std::num::NonZeroU32;
use std::path::Path;
use synth::synth_static_option;
pub use typed_int_value::TypedIntValue;

use crate::kmem::{Dlist, Handle, List, MList, MSS2, MSlice, MSpillSlice, MStr, Mem};
use crate::{DepEq, DepHash, SV2, kmem};
use ahash::HashMapExt;
use anyhow::bail;
use colored::Colorize;
use fxhash::FxHashMap;
use log::error;
use smallvec::{SmallVec, smallvec};

use scopes::*;
use types::*;

use crate::compiler::CompilerConfig;
use crate::kpath;
use crate::lex::{self, Span, SpanId, TokenKind};
use crate::parse::{
    self, AstHandle, AstSlice, BinaryOpKind, FileId, ForExpr, IdentPool, IdentSpanned,
    InterpolatedStringPart, NamedTypeArg, NumericWidth, ParseError, ParsedAbilityExpr,
    ParsedAbilityId, ParsedAbilityImplId, ParsedBlock, ParsedBlockKind, ParsedBreak, ParsedCall,
    ParsedCallArg, ParsedContinue, ParsedExpr, ParsedExprId, ParsedFnParamType, ParsedFunctionId,
    ParsedGlobalId, ParsedId, ParsedIfExpr, ParsedListLiteral, ParsedLiteral, ParsedLoopExpr,
    ParsedNamespaceId, ParsedPattern, ParsedPatternId, ParsedProgram, ParsedStaticBlockKind,
    ParsedStaticExpr, ParsedStmt, ParsedStmtId, ParsedTypeConstraint, ParsedTypeConstraintExpr,
    ParsedTypeDefnId, ParsedTypeExpr, ParsedTypeExprId, ParsedTypeParam, ParsedUnaryOpKind,
    ParsedUseId, ParsedVariable, ParsedVariant, ParsedWhileExpr, QIdent, StringId,
    StructValueField, StructValueFieldKind,
};
use crate::vpool::VPool;
use crate::{SV4, SV8, impl_copy_if_small, nz_u32_id, static_assert_size};

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
    External {
        module_id: ModuleId,
        lib_name: Option<StringId>,
        fn_name: Option<StringId>,
    },
    Exported {
        fn_name: Option<StringId>,
    },
    Intrinsic,
    /// `intern("llvm.cttz.i64")`
    LlvmIntrinsic(StringId),
}

impl Linkage {
    pub fn is_external(&self) -> bool {
        matches!(self, Linkage::External { .. })
    }

    pub fn is_exported(&self) -> bool {
        matches!(self, Linkage::Exported { .. })
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbilityFunctionRef {
    pub function_name: StringId,
    pub index: u32,
    pub ability_id: AbilityId,
    pub function_id: FunctionId,
}
impl_copy_if_small!(16, TypedAbilityFunctionRef);

#[derive(Clone, Copy)]
pub struct AbilityFnWhereConstraint {
    pub target: TypeId,
    pub signature: TypedAbilitySignature,
    pub span: SpanId,
}

pub const GLOBAL_ID_COMPILER_CAPTURE_PRINTS: TypedGlobalId =
    TypedGlobalId::from_nzu32(NonZeroU32::new(1).unwrap());
pub const GLOBAL_ID_K1_IS_STATIC: TypedGlobalId =
    TypedGlobalId::from_nzu32(NonZeroU32::new(2).unwrap());

//rustfmt: off
pub const ABILITY_ID_ENUM: AbilityId = AbilityId(NonZeroU32::new(1).unwrap());
pub const ABILITY_ID_SUM: AbilityId = AbilityId(NonZeroU32::new(2).unwrap());
pub const ABILITY_ID_EQUALS: AbilityId = AbilityId(NonZeroU32::new(3).unwrap());
pub const ABILITY_ID_WRITER: AbilityId = AbilityId(NonZeroU32::new(4).unwrap());
pub const ABILITY_ID_PRINT: AbilityId = AbilityId(NonZeroU32::new(5).unwrap());
pub const ABILITY_ID_SHOW: AbilityId = AbilityId(NonZeroU32::new(6).unwrap());
pub const ABILITY_ID_BITWISE: AbilityId = AbilityId(NonZeroU32::new(7).unwrap());
pub const ABILITY_ID_ADD: AbilityId = AbilityId(NonZeroU32::new(8).unwrap());
pub const ABILITY_ID_SUB: AbilityId = AbilityId(NonZeroU32::new(9).unwrap());
pub const ABILITY_ID_MUL: AbilityId = AbilityId(NonZeroU32::new(10).unwrap());
pub const ABILITY_ID_DIV: AbilityId = AbilityId(NonZeroU32::new(11).unwrap());
pub const ABILITY_ID_REM: AbilityId = AbilityId(NonZeroU32::new(12).unwrap());
pub const ABILITY_ID_NEG: AbilityId = AbilityId(NonZeroU32::new(13).unwrap());
pub const ABILITY_ID_SCALAR_CMP: AbilityId = AbilityId(NonZeroU32::new(14).unwrap());
pub const ABILITY_ID_COMPARABLE: AbilityId = AbilityId(NonZeroU32::new(15).unwrap());
pub const ABILITY_ID_TRY: AbilityId = AbilityId(NonZeroU32::new(16).unwrap());
pub const ABILITY_ID_ITERATOR: AbilityId = AbilityId(NonZeroU32::new(17).unwrap());
pub const ABILITY_ID_ITERABLE: AbilityId = AbilityId(NonZeroU32::new(18).unwrap());
//rustfmt: on

pub const FUNC_PARAM_IDEAL_COUNT: usize = 8;
pub const FUNC_TYPE_PARAM_IDEAL_COUNT: usize = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeSubstitutionPair {
    from: TypeId,
    to: TypeId,
}

#[derive(Clone, Copy, Default)]
struct SubstitutionFromKinds {
    holes: bool,
    params: bool,
    holes_and_params: bool,
    other: bool,
}

impl SubstitutionFromKinds {
    fn no_from_occurs_in(&self, counts: &TypeInfo) -> bool {
        let no_holes = counts.inference_hole_count == 0;
        let no_params = counts.type_parameter_count == 0;
        !self.other
            && (!self.holes || no_holes)
            && (!self.params || no_params)
            && (!self.holes_and_params || no_holes || no_params)
    }
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

/// One entry per instantiated type parameter of an inference context,
/// indexed by its inference hole's index
#[derive(Debug, Clone, Copy)]
pub struct InferenceSlot {
    /// The type parameter being solved, e.g. 'T'
    pub param_type: TypeId,
    /// The instantiated inference hole standing in for it, e.g. ''0'
    pub hole_type: TypeId,
    /// Current best solution; may still mention other holes while partial,
    /// and may be overwritten by a later better binding
    pub solution: Option<TypeId>,
    /// True once solution first becomes hole-free; never unset
    pub fully_solved: bool,
}

#[derive(Default)]
pub struct InferenceContext {
    pub origin_stack: Vec<SpanId>,
    /// slot index == hole index
    pub slots: Vec<InferenceSlot>,
    /// Slot indices whose solution just became hole-free
    pub newly_solved: Vec<u32>,
    pub start_raw: u64,
}

impl InferenceContext {
    pub fn make() -> Self {
        InferenceContext {
            origin_stack: Vec::new(),
            slots: Vec::new(),
            newly_solved: Vec::new(),
            start_raw: 0,
        }
    }

    pub fn print_lengths(&self) {
        eprintln!("s origin_stack {}", self.origin_stack.len());
        eprintln!("s slots {}", self.slots.len());
        eprintln!("s newly_solved {}", self.newly_solved.len());
    }

    pub fn reset(&mut self) {
        self.origin_stack.clear();
        self.slots.clear();
        self.newly_solved.clear();
        self.start_raw = 0;
    }
}

// For LSP / Tooling
#[derive(Clone, Copy)]
pub enum LsEntityKind {
    Namespace(NamespaceId),
    Function { function_id: FunctionId, is_defn: bool },
    Variable { variable_id: VariableId },
    Type { type_id: TypeId, applied_type_id: Option<TypeId> },
    Variant { type_id: TypeId, variant_index: u32 },
    StructField { type_id: TypeId, field_index: u32 },
}

#[derive(Clone, Copy)]
pub struct LsEntity {
    pub kind: LsEntityKind,
    pub span: lex::Span,
}

/// Spliced into the buffer at the cursor by the LSP so the file parses; the
/// typer records the CompletionSite when it evaluates the marker ident
pub const COMPLETION_MARKER: &str = "__k1_completion__";

pub struct CompletionState {
    pub marker: StringId,
    pub site: Option<CompletionSite>,
}

/// Where in the program the completion cursor sits, and what completes there
#[derive(Clone, Copy)]
pub enum CompletionSite {
    /// value.<cursor>
    Member { raw_base_type_id: TypeId, base_type_id: TypeId, scope_id: ScopeId },
    /// ns/path/<cursor>
    Path { path_scope_id: ScopeId },
    /// bare <cursor>
    Scope { scope_id: ScopeId },
    /// type:<cursor> or :<cursor>, in expressions and patterns
    Variant { type_id: TypeId },
    /// f(a, <cursor>): the cursor is a direct call argument; arg_index counts
    /// parsed args, so it includes the receiver of a method call
    CallArg { function_id: FunctionId, arg_index: u32, scope_id: ScopeId },
    /// .{ a = 1, <cursor> } with an expected struct type; the parsed literal
    /// says which fields are already present
    StructField { type_id: TypeId, parsed_struct_id: ParsedExprId },
}

#[derive(Clone, Copy)]
pub struct TypeDefnStackEntry {
    pub parsed_id: ParsedTypeDefnId,
    /// None for `type(alias)` defns: an alias is transparent, so it has no id of its own
    /// until its rhs is evaluated; re-entrant references resolve by expanding the rhs
    pub reserved_type_id: Option<TypeId>,
}

#[derive(Clone, Copy)]
pub struct StaticExecContext {
    /// If a `return` is used, what type is expected
    /// This is needed because `return` usually looks at the
    /// enclosing function, but for #static blocks it shouldn't
    /// So this type can be different than the usual ctx.expected_type
    expected_return_type: Option<TypeId>,
}

pub enum StaticExecutionResult {
    TypedExpr(TypedExprId),
    Definitions(AstSlice<ParsedId>),
}

pub enum ParseAdHocKind {
    Expr,
    Definitions,
}

pub enum ParseMetaprogramResult {
    Expr(ParsedExprId),
    Definitions(AstSlice<ParsedId>),
}

pub enum ParseReplSourceResult {
    Stmts(List<ParsedStmtId, ParsedProgram>),
    Defn(ParsedId),
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
        /// Indicates that we are generating code
        /// that the user shouldn't see or care about; should
        /// pevent emission of language server entities, perhaps
        /// some warnings, et. al
        const Hidden = 1 << 3;
        /// a.foo()
        /// ^ We do automatic address-of coercion on
        /// a.
        const IsMethodReceiver = 1 << 4;
        /// We are compiling code for compile-time (static) execution
        const Static = 1 << 5;
        /// Compiling a module manifest (fn module) body; enables the
        /// dep-params capture special form
        const ManifestEval = 1 << 6;
        /// The completion cursor is a direct argument of the enclosing call,
        /// which records the CallArg site itself
        const CompletionCursorOwnedByCall = 1 << 7;
    }
}

#[derive(Clone, Copy)]
pub struct EvalExprContext {
    // Always required
    scope_id: ScopeId,
    // Always required
    expected_type_id: Option<TypeId>,
    /// Meaningful only when flags contains Static: the `return` type
    /// expectation inside #static blocks
    static_expected_return_type: Option<TypeId>,
    // Each flag is almost always none, but that's good
    flags: EvalExprFlags,
}

// small enough for reg passing
static_assert_size!(EvalExprContext, 16);
impl EvalExprContext {
    #[inline(always)]
    fn make(scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext {
            scope_id,
            expected_type_id: None,
            static_expected_return_type: None,
            flags: EvalExprFlags::empty(),
        }
    }

    #[inline(always)]
    pub fn is_static(&self) -> bool {
        self.flags.contains(EvalExprFlags::Static)
    }

    #[inline(always)]
    pub fn with_is_static(&self, is_static: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Static, is_static);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    pub fn with_expected_type(&self, expected_type_id: Option<TypeId>) -> EvalExprContext {
        EvalExprContext { expected_type_id, ..*self }
    }

    #[inline(always)]
    pub fn with_no_expected_type(&self) -> EvalExprContext {
        EvalExprContext { expected_type_id: None, ..*self }
    }

    #[inline(always)]
    pub fn with_static_ctx(&self, static_ctx: Option<StaticExecContext>) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Static, static_ctx.is_some());
        EvalExprContext {
            static_expected_return_type: static_ctx.and_then(|s| s.expected_return_type),
            flags,
            ..*self
        }
    }

    #[inline(always)]
    fn with_manifest_eval(&self, manifest_eval: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::ManifestEval, manifest_eval);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    fn is_manifest_eval(&self) -> bool {
        self.flags.contains(EvalExprFlags::ManifestEval)
    }

    #[inline(always)]
    fn with_ccursor_owned_by_call(&self) -> EvalExprContext {
        EvalExprContext { flags: self.flags | EvalExprFlags::CompletionCursorOwnedByCall, ..*self }
    }

    #[inline(always)]
    fn is_marker_owned_by_call(&self) -> bool {
        self.flags.contains(EvalExprFlags::CompletionCursorOwnedByCall)
    }

    #[inline(always)]
    fn with_inference(&self, is_inference: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Inference, is_inference);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    fn is_inference(&self) -> bool {
        self.flags.contains(EvalExprFlags::Inference)
    }

    #[inline(always)]
    fn with_scope(&self, scope_id: ScopeId) -> EvalExprContext {
        EvalExprContext { scope_id, ..*self }
    }

    #[inline(always)]
    pub fn with_is_generic_pass(&self, is_generic_pass: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::GenericPass, is_generic_pass);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    fn is_generic_pass(&self) -> bool {
        self.flags.contains(EvalExprFlags::GenericPass)
    }

    #[inline(always)]
    pub fn with_is_defer(&self, defer: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Defer, defer);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    fn is_hidden_calls(&self) -> bool {
        self.flags.contains(EvalExprFlags::Hidden)
    }

    #[inline(always)]
    fn with_hidden_calls(&self, hidden: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::Hidden, hidden);
        EvalExprContext { flags, ..*self }
    }

    #[inline(always)]
    fn is_method_receiver(&self) -> bool {
        self.flags.contains(EvalExprFlags::IsMethodReceiver)
    }

    #[inline(always)]
    fn with_is_method_receiver(&self, is_method_receiver: bool) -> EvalExprContext {
        let mut flags = self.flags;
        flags.set(EvalExprFlags::IsMethodReceiver, is_method_receiver);
        EvalExprContext { flags, ..*self }
    }
}

/// How the target's bytes relate to the source's: the view covers exactly the source's bytes,
/// reads a leading part of them, or claims bytes past the end of them
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ViewExtent {
    Exact,
    ReadsPrefix,
    ReadsBeyond,
}

/// `total`: every bit pattern of the source is valid in the target; a non-total view makes
/// a claim and must be spelled `.narrow`
#[derive(Debug, Clone, Copy)]
struct CastView {
    total: bool,
    extent: ViewExtent,
}

impl CastView {
    const TOTAL: CastView = CastView { total: true, extent: ViewExtent::Exact };
    const CLAIMS: CastView = CastView { total: false, extent: ViewExtent::Exact };
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

#[derive(Clone)]
pub struct AbilitySpec9nInfo {
    generic_parent: AbilityId,
    specialized_child: AbilityId,
    arguments: TypeSliceId,
}
impl_copy_if_small!(12, AbilitySpec9nInfo);

#[derive(Clone, Copy)]
pub enum TypedAbilityKind {
    Concrete,
    Generic { specializations: PermList<AbilitySpec9nInfo> },
    Specialized(AbilitySpec9nInfo),
}

impl TypedAbilityKind {
    pub fn arguments(&self, k1: &TypedProgram) -> &'static [TypeId] {
        match self {
            TypedAbilityKind::Specialized(specialization) => {
                k1.get_type_slice(specialization.arguments)
            }
            TypedAbilityKind::Concrete | TypedAbilityKind::Generic { .. } => &[],
        }
    }

    pub fn specializations(&self) -> PermList<AbilitySpec9nInfo> {
        match self {
            TypedAbilityKind::Concrete => MList::empty(),
            TypedAbilityKind::Generic { specializations } => *specializations,
            TypedAbilityKind::Specialized(_) => MList::empty(),
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
    name: StringId,
    type_variable_id: TypeId,
    is_impl_param: bool,
    #[allow(unused)]
    span: SpanId,
}
impl_copy_if_small!(16, TypedAbilityParam);

impl TypedAbilityParam {
    fn is_ability_side_param(&self) -> bool {
        !self.is_impl_param
    }
}

#[derive(Clone, Copy)]
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
    pub specialized_ability_id: AbilityId,
    pub impl_arguments: TypeIdSlice,
}

pub(crate) struct ArgsAndParams {
    args: TmpSlice<MaybeTypedExpr>,
    params: TmpSlice<FnParamType>,
}

impl ArgsAndParams {
    fn get(&self, index: usize, mem: &Mem<MemTmp>) -> (MaybeTypedExpr, FnParamType) {
        (*mem.get_nth(self.args, index), *mem.get_nth(self.params, index))
    }

    fn iter(
        &self,
        mem: &Mem<MemTmp>,
    ) -> impl Iterator<Item = (&'static MaybeTypedExpr, &'static FnParamType)> + 'static {
        mem.getn_zip(self.args, self.params)
    }

    fn len(&self) -> u32 {
        debug_assert!(self.args.len() == self.params.len());
        self.args.len()
    }
}

#[derive(Clone, Copy)]
pub struct TypedAbility {
    pub name: StringId,
    pub base_ability_id: AbilityId,
    pub self_type_id: TypeId,
    pub parameters: PermSlice<TypedAbilityParam>,
    pub functions: PermSlice<TypedAbilityFunctionRef>,
    pub scope_id: ScopeId,
    pub ast_id: ParsedAbilityId,
    pub namespace_id: NamespaceId,
    pub kind: TypedAbilityKind,
}

impl TypedAbility {
    pub fn find_function_by_name(
        &self,
        mem: &Mem<TypedProgram>,
        name: StringId,
    ) -> Option<TypedAbilityFunctionRef> {
        mem.getn(self.functions).iter().find(|f| f.function_name == name).copied()
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
pub struct TypedSumPattern {
    pub sum_type_id: TypeId,
    pub variant_name: StringId,
    pub variant_index: u32,
    pub payload: Option<TypedPatternId>,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedEnumPattern {
    pub enum_type_id: TypeId,
    pub member_name: StringId,
    pub index: u32,
    pub int_value: TypedIntValue,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedStructPatternField {
    pub name: StringId,
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
    pub name: StringId,
    pub type_id: TypeId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedReferencePattern {
    pub inner_pattern: TypedPatternId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
/// This pattern kind is only useful when run at compile-time
/// It can inspect the type of a value of a generic type, like T or Opt[T],
/// and provide a typed, bound value of that type if the match succeeds. This
/// lets you do statically known specializations within the same function body
/// based on types
pub struct TypePattern {
    pub inner_pattern: TypedPatternId,
    pub type_id: TypeId,
    pub span: SpanId,
}

// <pattern> ::= <literal> | <variable> | <sum> | <struc>
// <literal> ::= "(" ")" | "\"" <ident> "\"" | [0-9]+ | "'" [a-z] "'" | "None"
// <variable> ::= <ident>
// <ident> ::= [a-z]*
// <sum> ::= "." <ident> ( "(" <pattern> ")" )?
// <struc> ::= "{" ( <ident> ": " <pattern> ","? )* "}"
// <type> ::= "type["<ty expr>"]("<pattern>")"

type TypedPatternId = Handle<TypedPattern, TypedPatternPool>;

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
            TypedPattern::LiteralChar(_, _) => (),
            TypedPattern::LiteralInteger(_, _) => (),
            TypedPattern::LiteralFloat(_, _) => (),
            TypedPattern::LiteralBool(_, _) => (),
            TypedPattern::LiteralString(_, _) => (),
            TypedPattern::Enum(_) => (),
            TypedPattern::Variable(variable_pattern) => bindings.push(*variable_pattern),
            TypedPattern::Sum(sum_pattern) => {
                if let Some(payload_pattern_id) = sum_pattern.payload.as_ref() {
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
            TypedPattern::RefNull(_, _) => (),
            TypedPattern::PointerNull(_) => (),
            TypedPattern::Type(t) => self.get_pattern_bindings_rec(t.inner_pattern, bindings),
        }
    }
    pub fn pattern_never_useless(&self, pattern_id: TypedPatternId) -> bool {
        match self.mem.get(pattern_id) {
            TypedPattern::LiteralChar(_, _span) => true,
            TypedPattern::LiteralInteger(_, _span) => true,
            TypedPattern::LiteralFloat(_, _span) => true,
            TypedPattern::LiteralString(_, _span) => true,
            TypedPattern::LiteralBool(_, _span_id) => false,
            TypedPattern::Variable(_variable_pattern) => false,
            TypedPattern::Sum(typed_sum_pattern) => {
                typed_sum_pattern.payload.as_ref().is_some_and(|p| self.pattern_never_useless(*p))
            }
            TypedPattern::Enum(_) => false,
            TypedPattern::Struct(typed_struct_pattern) => self
                .mem
                .getn(typed_struct_pattern.fields)
                .iter()
                .any(|field_pattern| self.pattern_never_useless(field_pattern.pattern)),
            TypedPattern::Wildcard(_span_id) => false,
            TypedPattern::Reference(refer) => self.pattern_never_useless(refer.inner_pattern),
            TypedPattern::RefNull(_, _) => true,
            TypedPattern::PointerNull(_) => true,
            TypedPattern::Type(_) => true,
        }
    }
}

#[derive(Clone, Copy)]
pub enum TypedPattern {
    // Consider replacing with a single Literal using StaticValue
    LiteralChar(u8, SpanId),
    LiteralInteger(StaticValueId, SpanId),
    LiteralFloat(StaticValueId, SpanId),
    LiteralBool(bool, SpanId),
    LiteralString(StringId, SpanId),
    Variable(VariablePattern),
    Sum(TypedSumPattern),
    Enum(TypedEnumPattern),
    Struct(TypedStructPattern),
    Wildcard(SpanId),
    Reference(TypedReferencePattern),
    RefNull(TypeId, SpanId),
    PointerNull(SpanId),
    Type(TypePattern),
}

impl TypedPattern {
    pub fn kind_name(&self) -> &'static str {
        match self {
            TypedPattern::LiteralChar(_, _) => "char",
            TypedPattern::LiteralInteger(_, _) => "integer",
            TypedPattern::LiteralFloat(_, _) => "float",
            TypedPattern::LiteralBool(_, _) => "bool",
            TypedPattern::LiteralString(_, _) => "string",
            TypedPattern::Enum(_) => "enum",
            TypedPattern::Variable(_) => "variable",
            TypedPattern::Sum(_) => "variant",
            TypedPattern::Struct(_) => "struct",
            TypedPattern::Wildcard(_) => "_",
            TypedPattern::Reference(_) => "reference",
            TypedPattern::RefNull(_, _) => "null reference",
            TypedPattern::PointerNull(_) => "null ptr",
            TypedPattern::Type(_) => "type pattern",
        }
    }
    pub fn span_id(&self) -> SpanId {
        match self {
            TypedPattern::LiteralChar(_, span) => *span,
            TypedPattern::LiteralInteger(_, span) => *span,
            TypedPattern::LiteralFloat(_, span) => *span,
            TypedPattern::LiteralBool(_, span) => *span,
            TypedPattern::LiteralString(_, span) => *span,
            TypedPattern::Enum(e) => e.span,
            TypedPattern::Variable(variable_pattern) => variable_pattern.span,
            TypedPattern::Sum(sum_pattern) => sum_pattern.span,
            TypedPattern::Struct(struct_pattern) => struct_pattern.span,
            TypedPattern::Wildcard(span) => *span,
            TypedPattern::Reference(refer) => refer.span,
            TypedPattern::RefNull(_, span) => *span,
            TypedPattern::PointerNull(span) => *span,
            TypedPattern::Type(t) => t.span,
        }
    }
}

enum MatchingConditionResult {
    NeverBlock(TypedExprId),
    MatchingCondition(MatchingCondition),
}

pub struct BlockBuilder {
    pub scope_id: ScopeId,
    pub statements: List<TypedStmtId, TypedProgram>,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedBlock {
    pub scope_id: ScopeId,
    pub statements: PermSlice<TypedStmtId>,
}

#[derive(Clone)]
pub struct SpecializationInfo {
    pub parent_function: FunctionId,
    pub type_arguments: TypeSliceId,
    pub fnlike_type_arguments: TypeSliceId,
    pub specialized_function_id: FunctionId,
    pub specialized_function_type: TypeId,
}
impl_copy_if_small!(20, SpecializationInfo);

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

#[derive(Clone)]
pub struct FunctionSignature {
    pub name: Option<StringId>,
    pub function_type: TypeId,
    pub type_params: TypeIdSlice,
    pub fnlike_type_params: PermSlice<FnlikeTypeParam>,
}
impl_copy_if_small!(24, FunctionSignature);

impl FunctionSignature {
    pub fn make_no_generics(name: Option<StringId>, function_type: TypeId) -> FunctionSignature {
        FunctionSignature {
            name,
            function_type,
            type_params: MSlice::empty(),
            fnlike_type_params: MSlice::empty(),
        }
    }

    pub fn has_type_params(&self) -> bool {
        !self.type_params.is_empty() || !self.fnlike_type_params.is_empty()
    }
}

#[derive(Clone, Copy)]
pub struct TypedFunctionParam {
    pub variable_id: VariableId,
    pub span: SpanId,
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct TypedFunctionFlags: u8 {
        const CompilerDebug = 1;
        const Concrete = 1 << 1;
        const Macro = 1 << 2;
        const ModuleManifest = 1 << 3;
        const Reloadable = 1 << 4;
        const AbiNative = 1 << 5;
        const AddressTaken = 1 << 6;
    }
}

#[derive(Clone, Copy)]
pub struct TypedFunction {
    pub name: StringId,
    pub scope: ScopeId,
    pub namespace_id: NamespaceId,
    pub params: PermSlice<TypedFunctionParam>,
    pub type_params: TypeIdSlice,
    pub fnlike_type_params: PermSlice<FnlikeTypeParam>,
    /// Constraints on self and ability params have to go here, since they aren't among the function's type params
    pub ability_where_constraints: PermSlice<AbilityFnWhereConstraint>,
    pub body_block: Option<TypedExprId>,
    pub builtin_type: Option<Builtin>,
    pub linkage: Linkage,
    /// If I am specialization myself
    pub specialization_info: Option<SpecializationInfo>,
    pub parsed_id: ParsedId,
    pub type_id: TypeId,
    pub kind: TypedFunctionKind,
    flags: TypedFunctionFlags,
    /// If we've generated a 'dyn' copy of this function, we store its id
    pub dyn_fn_id: Option<FunctionId>,
    /// 'let(returned)', RVO
    pub returned_variable: Option<VariableId>,
    pub body_failure: Option<K1Message>,
}

impl TypedFunction {
    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature {
            name: Some(self.name),
            function_type: self.type_id,
            type_params: self.type_params,
            fnlike_type_params: self.fnlike_type_params,
        }
    }

    pub fn is_generic(&self) -> bool {
        matches!(self.kind, TypedFunctionKind::AbilityDefn(_)) || self.signature().has_type_params()
    }

    pub fn compiler_debug(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::CompilerDebug)
    }

    pub fn is_concrete(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::Concrete)
    }

    pub fn is_macro(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::Macro)
    }

    pub fn is_module_manifest_fn(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::ModuleManifest)
    }

    pub fn is_reloadable(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::Reloadable)
    }

    pub fn abi_native(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::AbiNative)
    }

    pub fn address_taken(&self) -> bool {
        self.flags.contains(TypedFunctionFlags::AddressTaken)
    }
}

#[derive(Debug, Clone, Copy)]
/// When a function takes a special type parameter, either a 'function_like'
/// or a 'static'. This ties the type parameter to its value param, which
/// is always a 1-1 relationship. As in:
/// fn example(knownInt: static int, some thunk: () -> ())
///            ^ existential type param 1, a static
///                                  ^ existential type param 2, a function type param
pub struct FnlikeTypeParam {
    pub name: StringId,
    pub type_id: TypeId,
    pub value_param_index: u32,
    pub span: SpanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NameAndType {
    pub name: StringId,
    pub type_id: TypeId,
}

pub type TypeIdSlice = MSlice<TypeId, TypedProgram>;

pub type TypeArgs = MSS2<TypeId, TypedProgram>;

nz_u32_id!(TypeSliceId);

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

#[derive(Clone, Copy)]
pub enum AddressOfKind {
    StackVariable(VariableId),
    GlobalVariable(VariableId),
    ReferenceExpr,
}

#[derive(Clone, Copy)]
pub struct AddressOfExpr {
    pub target_expr: TypedExprId,
    pub kind: AddressOfKind,
}

#[derive(Clone)]
pub enum CallResolution {
    OtherExpr(TypedExprId),
    Call(Callee),
    MethodCall { callee: Callee, receiver: TypedExprId },
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
    Builtin {
        function_sig: FunctionSignature,
        builtin: Builtin,
    },
    /// Must contain a LambdaObject
    DynamicLambda(TypedExprId),
    /// Dynamic dispatch through an ability object. object_expr is typed
    /// Type::AbilityObject; slot_function_type is the Function type inside the
    /// object's fn-ptr field (is_lambda, param 0 = state ptr); field_index is
    /// the object struct field holding the fn ptr (state is field 0)
    DynamicAbilityFn {
        object_expr: TypedExprId,
        field_index: u32,
        slot_function_type: TypeId,
    },
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
impl_copy_if_small!(32, Callee);

impl Callee {
    pub fn make_static(function_id: FunctionId) -> Callee {
        Callee::StaticFunction(function_id)
    }

    pub fn from_ability_impl_fn(ability_impl_fn: &AbilityImplFunction) -> Callee {
        match *ability_impl_fn {
            AbilityImplFunction::FunctionId(function_id) => Callee::StaticFunction(function_id),
            AbilityImplFunction::Abstract(function_sig) => Callee::Abstract { function_sig },
            AbilityImplFunction::Unavailable => {
                unreachable!("callers check availability before building a Callee")
            }
        }
    }

    pub fn maybe_function_id(&self) -> Option<FunctionId> {
        match self {
            Callee::StaticFunction(function_id) => Some(*function_id),
            Callee::StaticLambda { function_id, .. } => Some(*function_id),
            Callee::Abstract { .. } => None,
            Callee::Builtin { .. } => None,
            Callee::DynamicLambda(_) => None,
            Callee::DynamicAbilityFn { .. } => None,
            Callee::DynamicFunction { .. } => None,
            Callee::DynamicAbstract { .. } => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Call {
    pub callee: Callee,
    pub args: PermSlice<TypedExprId>,
    /// type_args remain unerased for some intrinsics where we want codegen to see the types.
    /// Specifically sizeOf[T], since there's no actual value to specialize on. kinda a hack would be
    /// better to specialize anyway and inline? idk
    pub type_args: TypeArgs,
    pub return_type: TypeId,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct StructLiteralField {
    pub name: StringId,
    // None means uninitialized
    pub expr: Option<TypedExprId>,
}

#[derive(Clone, Copy)]
pub struct StructLiteral {
    pub fields: PermSlice<StructLiteralField>,
}

#[derive(Clone)]
pub struct ArrayGetElement {
    pub base_array: TypedExprId,
    pub index: TypedExprId,
    pub packed: bool,
}
impl_copy_if_small!(12, ArrayGetElement);

#[derive(Clone, Copy)]
pub struct FieldAccess {
    pub base_struct: TypedExprId,
    pub field_index: u32,
    pub packed: bool,
}

#[derive(Clone)]
pub struct TypedSumConstructor {
    pub variant_index: u32,
    pub payload: Option<TypedExprId>,
}
impl_copy_if_small!(16, TypedSumConstructor);

#[derive(Clone)]
pub struct GetSumPayload {
    pub sum_expr: TypedExprId,
    pub variant_index: u32,
    pub packed: bool,
}
impl_copy_if_small!(12, GetSumPayload);

#[derive(Clone)]
pub struct GetSumTag {
    pub sum_expr: TypedExprId,
}
impl_copy_if_small!(4, GetSumTag);

#[derive(Clone, Copy)]
pub struct EnumConstructor {
    pub value_index: u32,
}

#[derive(Clone, Copy)]
pub struct EnumGetValue {
    pub enum_expr: TypedExprId,
}

#[derive(Clone, Copy)]
pub struct TypedMatchArm {
    pub case: Option<StaticValueId>,
    pub condition: MatchingCondition,
    pub consequent_expr: TypedExprId,
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

#[derive(Debug, Clone, Copy)]
pub enum IntegerCastDirection {
    Extend,
    Truncate,
    SignChange,
}

#[derive(Debug, Clone, Copy)]
pub enum CastType {
    IntegerCast(IntegerCastDirection),
    FloatExtend,
    FloatTruncate,
    FloatToUnsignedInteger,
    FloatToSignedInteger,
    IntegerUnsignedToFloat,
    IntegerSignedToFloat,
    // NoOps
    ReferenceToReference,
    PointerToReference,
    ReferenceToPointer,
}

impl Display for CastType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CastType::IntegerCast(_dir) => write!(f, "intcast"),
            CastType::ReferenceToReference => write!(f, "reftoref"),
            CastType::PointerToReference => write!(f, "ptrtoref"),
            CastType::ReferenceToPointer => write!(f, "reftoptr"),
            CastType::FloatExtend => write!(f, "fext"),
            CastType::FloatTruncate => write!(f, "ftrunc"),
            CastType::FloatToUnsignedInteger => write!(f, "ftouint"),
            CastType::FloatToSignedInteger => write!(f, "ftosint"),
            CastType::IntegerUnsignedToFloat => write!(f, "uinttof"),
            CastType::IntegerSignedToFloat => write!(f, "sinttof"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypedCast {
    pub cast_type: CastType,
    pub base_expr: TypedExprId,
}

#[derive(Debug, Clone, Copy)]
pub struct TypedReturn {
    pub value: TypedExprId,
    pub returned_variable: Option<VariableId>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypedBreak {
    pub value: TypedExprId,
    pub loop_scope: ScopeId,
}

#[derive(Debug, Clone, Copy)]
pub struct LambdaExpr {
    pub lambda_type: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionPointerExpr {
    pub function_id: FunctionId,
}

#[derive(Clone, Copy)]
pub struct MatchingCondition {
    pub instrs: PermSlice<MatchingConditionInstr>,
}

#[derive(Debug, Clone)]
pub enum MatchingConditionInstr {
    Binding { let_stmt: TypedStmtId },
    Cond { value: TypedExprId },
    IntEquals { subject: TypedExprId, value: StaticValueId },
}

impl MatchingConditionInstr {
    pub fn cond(value: TypedExprId) -> Self {
        MatchingConditionInstr::Cond { value }
    }
    pub fn binding(let_stmt: TypedStmtId) -> Self {
        MatchingConditionInstr::Binding { let_stmt }
    }
}

impl_copy_if_small!(12, MatchingConditionInstr);

#[derive(Clone, Copy)]
pub struct WhileLoop {
    pub condition: MatchingCondition,
    pub body: TypedExprId,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body_block: TypedExprId,
}
impl_copy_if_small!(4, LoopExpr);

#[derive(Clone, Copy)]
/// Invariant: The last arm's condition must always evaluate to 'true'
pub struct TypedMatchExpr {
    pub subject_defn: Option<TypedStmtId>,
    pub scrutinee: Option<TypedExprId>,
    pub arms: PermSlice<TypedMatchArm>,
}

#[derive(Clone, Copy)]
pub struct StaticConstantExpr {
    pub value_id: StaticValueId,
    pub is_typed_as_static: bool,
}

nz_u32_id!(CallId);

static_assert_size!(TypedExpr, 20);
#[derive(Clone, Copy)]
pub enum TypedExpr {
    StaticValue(StaticConstantExpr),
    Struct(StructLiteral),
    // Current largest variant at 20 bytes
    StructFieldAccess(FieldAccess),
    ArrayGetElement(ArrayGetElement),
    Variable(VariableExpr),
    Deref(DerefExpr),
    AddressOf(AddressOfExpr),
    Block(TypedBlock),
    Call {
        call_id: CallId,
    },
    /// In the past, we lowered match to an if/else chain. This proves not quite powerful enough
    /// of a representation to do everything we want
    /// Now we lower if/else to a match!
    Match(TypedMatchExpr),
    WhileLoop(WhileLoop),
    LoopExpr(LoopExpr),
    SumConstructor(TypedSumConstructor),
    SumGetTag(GetSumTag),
    SumGetPayload(GetSumPayload),
    Enum(EnumConstructor),
    EnumGetValue(EnumGetValue),
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
    /// continue jumps to the top of the next iteration of the enclosing loop:
    /// the condition check of a `while`, the body top of a `loop`
    Continue {
        loop_scope: ScopeId,
    },
    /// Creating a lambda results in a Lambda expr.
    /// - A function is created
    /// - An environment capture expr is created
    /// - An expression is returned that is really just a pointer to the unique Closure it points
    ///   to; this can either be called directly or turned into a dynamic function object if needed
    Lambda(LambdaExpr),
    /// Calling .toRef() on a function by name
    FunctionPointer(FunctionPointerExpr),
}

impl From<VariableExpr> for TypedExpr {
    fn from(value: VariableExpr) -> Self {
        TypedExpr::Variable(value)
    }
}

impl TypedExpr {
    pub fn kind_name(&self) -> &'static str {
        match self {
            TypedExpr::Struct(_) => "struct",
            TypedExpr::StructFieldAccess(_) => "struct_field_access",
            TypedExpr::ArrayGetElement(_) => "array_get_element",
            TypedExpr::Variable(_) => "variable",
            TypedExpr::Deref(_) => "deref",
            TypedExpr::AddressOf(_) => "addressof",
            TypedExpr::Block(_) => "block",
            TypedExpr::Call { .. } => "call",
            TypedExpr::Match(_) => "match",
            TypedExpr::WhileLoop(_) => "while_loop",
            TypedExpr::LoopExpr(_) => "loop",
            TypedExpr::SumConstructor(_) => "sum_constructor",
            TypedExpr::SumGetTag(_) => "sum_get_tag",
            TypedExpr::SumGetPayload(_) => "sum_get_payload",
            TypedExpr::Enum(_) => "enum_constructor",
            TypedExpr::EnumGetValue(_) => "enum_get_value",
            TypedExpr::Cast(_) => "cast",
            TypedExpr::Return(_) => "return",
            TypedExpr::Break(_) => "break",
            TypedExpr::Continue { .. } => "continue",
            TypedExpr::Lambda(_) => "lambda",
            TypedExpr::FunctionPointer(_) => "function_pointer",
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

enum CheckExprTypeResult {
    Ok,
    Err(MStr<MemTmp>),
    Coerce(TypedExprId, &'static str),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub variable_id: VariableId,
    pub variable_type: TypeId,
    pub initializer: Option<TypedExprId>,
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

#[derive(Clone, Copy)]
pub struct TypedRequireStmt {
    pub condition: MatchingCondition,
    pub else_body: Option<TypedExprId>,
    pub span: SpanId,
}

#[derive(Clone, Copy)]
pub struct TypedDeferStmt {
    pub parsed_expr: ParsedExprId,
    pub span: SpanId,
}

/// How far up the scope tree a scope exit travels, for defer gathering:
/// a `return` leaves every scope up to the function top, a `break` leaves
/// every scope up to its enclosing loop.
#[derive(Clone, Copy)]
enum DeferExtent {
    FunctionTop,
    LoopScope(ScopeId),
}

static_assert_size!(TypedStmt, 20);
#[derive(Clone, Copy)]
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

    fn as_expr(&self) -> Option<TypedExprId> {
        match self {
            TypedStmt::Expr(expr_id, _) => Some(*expr_id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessageLevel {
    Hint,
    Info,
    Warn,
    Error,
}

impl MessageLevel {
    pub fn color(&self) -> colored::Color {
        match self {
            MessageLevel::Hint => colored::Color::BrightBlue,
            MessageLevel::Info => colored::Color::Cyan,
            MessageLevel::Warn => colored::Color::Yellow,
            MessageLevel::Error => colored::Color::Red,
        }
    }
    pub fn name_str(&self) -> &'static str {
        match self {
            MessageLevel::Hint => "hint",
            MessageLevel::Info => "info",
            MessageLevel::Warn => "warn",
            MessageLevel::Error => "error",
        }
    }
}

impl Display for MessageLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    None,
    ParseError,
    TypeError,
    Malformed,
    Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct K1Message {
    pub message: StringId,
    pub span: SpanId,
    pub error_kind: ErrorKind,
    pub level: MessageLevel,
}

pub type K1Result<A> = Result<A, K1Message>;

#[derive(Debug, Clone)]
struct SynthedVariable {
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
        const Returned = 1 << 3;
        const AddressTaken = 1 << 4;
    }
}

#[derive(Clone, Copy)]
pub enum VariableKind {
    FnParam(FunctionId),
    Stack(TypedStmtId),
    StackSynthetic(TypedStmtId),
    Global(TypedGlobalId),
}

#[derive(Clone, Copy)]
pub struct Variable {
    pub name: StringId,
    pub type_id: TypeId,
    pub owner_scope: ScopeId,
    pub flags: VariableFlags,
    pub usage_count: u32,
    pub kind: VariableKind,
    pub defn_span: SpanId,
}

impl Variable {
    pub fn global_id(&self) -> Option<TypedGlobalId> {
        match self.kind {
            VariableKind::Global(global_id) => Some(global_id),
            _ => None,
        }
    }

    pub fn is_reassigned(&self) -> bool {
        self.flags.contains(VariableFlags::Reassigned)
    }
    pub fn is_context(&self) -> bool {
        self.flags.contains(VariableFlags::Context)
    }
    pub fn is_user_hidden(&self) -> bool {
        self.flags.contains(VariableFlags::UserHidden)
    }
    pub fn is_returned(&self) -> bool {
        self.flags.contains(VariableFlags::Returned)
    }
    pub fn is_address_taken(&self) -> bool {
        self.flags.contains(VariableFlags::AddressTaken)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalInitialValue {
    /// The initializer has not been evaluated yet
    Pending,
    /// Evaluated: this global has no compile-time value. Currently this means an
    /// external global, whose storage arrives at link time
    Uninit,
    /// Evaluated to a value
    Value(StaticValueId),
    Failed(K1Message),
}

impl GlobalInitialValue {
    pub fn as_value(&self) -> Option<StaticValueId> {
        match self {
            GlobalInitialValue::Value(v) => Some(*v),
            GlobalInitialValue::Pending
            | GlobalInitialValue::Uninit
            | GlobalInitialValue::Failed(_) => None,
        }
    }

    pub fn is_pending(&self) -> bool {
        matches!(self, GlobalInitialValue::Pending)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypedGlobal {
    pub variable_id: VariableId,
    pub parsed_expr: Option<ParsedExprId>,
    pub initial_value: GlobalInitialValue,
    pub type_id: TypeId,
    pub span: SpanId,
    pub is_constant: bool,
    pub is_tls: bool,
    pub is_exported: bool,
    pub is_external: bool,
    pub link_name: Option<StringId>,
    pub ast_id: ParsedGlobalId,
    pub parent_scope: ScopeId,
    /// Set when the global is declared in an ns(reload)
    pub reload_ns: Option<NamespaceId>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NamespaceKind {
    User,
    TypeCompanion,
    Ability,
    Root,
}

#[derive(Debug, Clone, Copy)]
pub struct Namespace {
    pub name: StringId,
    pub scope_id: ScopeId,
    pub namespace_type: NamespaceKind,
    pub companion_type_id: Option<TypeId>,
    pub parent_id: Option<NamespaceId>,
    pub owner_module: Option<ModuleId>,
    pub parsed_id: ParsedId,
    /// default library for extern fns in this namespace.
    /// re-opened ns: any opening may declare it; disagreeing openings are an error.
    pub lib_name: Option<StringId>,
    /// Set when the function is declared in an ns(reload)
    /// re-opened ns: any opening may declare it; all openings share it.
    pub reload: bool,
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

    pub fn find_child_by_name(
        &self,
        parent_id: NamespaceId,
        name: StringId,
    ) -> Option<NamespaceId> {
        for (id, ns) in self.namespaces.iter_with_ids() {
            if ns.parent_id == Some(parent_id) && ns.name == name {
                return Some(id);
            }
        }
        None
    }

    pub fn get_scope(&self, namespace_id: NamespaceId) -> ScopeId {
        self.get(namespace_id).scope_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BitwiseBinopKind {
    And,
    Or,
    Xor,
    ShiftLeft,
    SignedShiftRight,
    UnsignedShiftRight,
}

impl BitwiseBinopKind {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BitwiseBinopKind::And => "bitwise_and",
            BitwiseBinopKind::Or => "bitwise_or",
            BitwiseBinopKind::Xor => "bitwise_xor",
            BitwiseBinopKind::ShiftLeft => "bitwise_shift_left",
            BitwiseBinopKind::SignedShiftRight => "bitwise_signed_shr",
            BitwiseBinopKind::UnsignedShiftRight => "bitwise_unsigned_shl",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithOpClass {
    Float,
    UnsignedInt,
    SignedInt,
}

impl ArithOpClass {
    pub fn is_signed_int(self) -> bool {
        matches!(self, ArithOpClass::SignedInt)
    }
    pub fn from_int_type(i: IntegerType) -> Self {
        if i.is_signed() { ArithOpClass::SignedInt } else { ArithOpClass::UnsignedInt }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithOpOp {
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

impl ArithOpOp {
    pub fn kind_name(&self) -> &'static str {
        match self {
            ArithOpOp::Equals => "eq",
            ArithOpOp::Add => "add",
            ArithOpOp::Sub => "sub",
            ArithOpOp::Mul => "mul",
            ArithOpOp::Div => "div",
            ArithOpOp::Rem => "rem",
            ArithOpOp::Lt => "lt",
            ArithOpOp::Le => "le",
            ArithOpOp::Gt => "gt",
            ArithOpOp::Ge => "ge",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArithOpKind {
    pub class: ArithOpClass,
    pub op: ArithOpOp,
}

impl ArithOpKind {
    pub fn kind_name(&self) -> &'static str {
        match self.class {
            ArithOpClass::Float => match self.op {
                ArithOpOp::Equals => "float_eq",
                ArithOpOp::Add => "float_add",
                ArithOpOp::Sub => "float_sub",
                ArithOpOp::Mul => "float_mul",
                ArithOpOp::Div => "float_div",
                ArithOpOp::Rem => "float_rem",
                ArithOpOp::Lt => "float_lt",
                ArithOpOp::Le => "float_le",
                ArithOpOp::Gt => "float_gt",
                ArithOpOp::Ge => "float_ge",
            },
            ArithOpClass::UnsignedInt | ArithOpClass::SignedInt => match self.op {
                ArithOpOp::Equals => "int_eq",
                ArithOpOp::Add => "int_add",
                ArithOpOp::Sub => "int_sub",
                ArithOpOp::Mul => "int_mul",
                ArithOpOp::Div => "int_div",
                ArithOpOp::Rem => "int_rem",
                ArithOpOp::Lt => "int_lt",
                ArithOpOp::Le => "int_le",
                ArithOpOp::Gt => "int_gt",
                ArithOpOp::Ge => "int_ge",
            },
        }
    }

    pub fn uint(op: ArithOpOp) -> Self {
        ArithOpKind { class: ArithOpClass::UnsignedInt, op }
    }
    pub fn sint(op: ArithOpOp) -> Self {
        ArithOpKind { class: ArithOpClass::SignedInt, op }
    }
    pub fn float(op: ArithOpOp) -> Self {
        ArithOpKind { class: ArithOpClass::Float, op }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinAbility {
    Enum,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTyperInline {
    TypeSize,
    TypeStride,
    TypeAlign,
    CompilerSourceLocation,
    GetStaticValue,
    StaticTypeToValue,
    TypeId,
    EnumEquals,
}

impl BuiltinTyperInline {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BuiltinTyperInline::TypeSize => "type_size",
            BuiltinTyperInline::TypeStride => "type_stride",
            BuiltinTyperInline::TypeAlign => "type_align",
            BuiltinTyperInline::CompilerSourceLocation => "compiler_source_location",
            BuiltinTyperInline::GetStaticValue => "get_static_value",
            BuiltinTyperInline::StaticTypeToValue => "static_type_to_value",
            BuiltinTyperInline::TypeId => "type_id",
            BuiltinTyperInline::EnumEquals => "enum_equals",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTyperFunction {
    EnumAbilityGetValue,
    EnumAbilityGetTagName,
    SumAbilityGetTag,
    SumAbilityGetName,
    SumEquals,
    StructEquals,
    StructPrintTo,
    SumPrintTo,
    EnumPrintTo,
}

impl BuiltinTyperFunction {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BuiltinTyperFunction::EnumAbilityGetValue => "enum_ability_get_value",
            BuiltinTyperFunction::EnumAbilityGetTagName => "enum_ability_get_tag_name",
            BuiltinTyperFunction::SumAbilityGetTag => "sum_ability_get_tag",
            BuiltinTyperFunction::SumAbilityGetName => "sum_ability_get_name",
            BuiltinTyperFunction::SumEquals => "sum_equals",
            BuiltinTyperFunction::StructEquals => "struct_equals",
            BuiltinTyperFunction::StructPrintTo => "struct_print_to",
            BuiltinTyperFunction::SumPrintTo => "sum_print_to",
            BuiltinTyperFunction::EnumPrintTo => "enum_print_to",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinIr {
    BakeStaticValue,
    Zeroed,
    Negate,
    BitNot,
    Bitcast,
    ArithBinop(ArithOpKind),
    BitwiseBinop(BitwiseBinopKind),
    PointerIndex,
    VolatileLoad,
    VolatileStore,
    AtomicLoad,
    AtomicStore,
    AtomicRmw(AtomicRmwOp),
    AtomicCmpxchg { weak: bool },
    AtomicFence,
    VectorOp(VecOpKind),
}

/// Signedness/float class of lane ops is resolved at IR lowering from the element type
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VecOpKind {
    Splat,
    Load,
    Store,
    GetLane,
    WithLane,
    Add,
    Sub,
    Mul,
    BitNot,
    BitAnd,
    BitOr,
    Xor,
    ShiftLeft,
    ShiftRight,
    EqLanes,
    ToMask,
}

impl VecOpKind {
    pub fn kind_name(&self) -> &'static str {
        match self {
            VecOpKind::Splat => "vector_splat",
            VecOpKind::Load => "vector_load",
            VecOpKind::Store => "vector_store",
            VecOpKind::GetLane => "vector_get_lane",
            VecOpKind::WithLane => "vector_with_lane",
            VecOpKind::Add => "vector_add",
            VecOpKind::Sub => "vector_sub",
            VecOpKind::Mul => "vector_mul",
            VecOpKind::BitNot => "vector_bit_not",
            VecOpKind::BitAnd => "vector_bit_and",
            VecOpKind::BitOr => "vector_bit_or",
            VecOpKind::Xor => "vector_xor",
            VecOpKind::ShiftLeft => "vector_shift_left",
            VecOpKind::ShiftRight => "vector_shift_right",
            VecOpKind::EqLanes => "vector_eq_lanes",
            VecOpKind::ToMask => "vector_to_mask",
        }
    }
}

/// Signedness of Min/Max is resolved at IR lowering from the element type
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AtomicRmwOp {
    Xchg,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Min,
    Max,
}

impl BuiltinIr {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BuiltinIr::BakeStaticValue => "bake_static_value",
            BuiltinIr::Zeroed => "zeroed",
            BuiltinIr::Negate => "negate",
            BuiltinIr::BitNot => "bit_not",
            BuiltinIr::Bitcast => "bitcast",
            BuiltinIr::ArithBinop(op_kind) => op_kind.kind_name(),
            BuiltinIr::BitwiseBinop(op_kind) => op_kind.kind_name(),
            BuiltinIr::PointerIndex => "pointer_index",
            BuiltinIr::VolatileLoad => "volatile_load",
            BuiltinIr::VolatileStore => "volatile_store",
            BuiltinIr::AtomicLoad => "atomic_load",
            BuiltinIr::AtomicStore => "atomic_store",
            BuiltinIr::AtomicRmw(op) => op.kind_name(),
            BuiltinIr::AtomicCmpxchg { weak: false } => "atomic_cmpxchg",
            BuiltinIr::AtomicCmpxchg { weak: true } => "atomic_cmpxchg_weak",
            BuiltinIr::AtomicFence => "atomic_fence",
            BuiltinIr::VectorOp(op) => op.kind_name(),
        }
    }
}

impl AtomicRmwOp {
    pub fn kind_name(&self) -> &'static str {
        match self {
            AtomicRmwOp::Xchg => "atomic_xchg",
            AtomicRmwOp::Add => "atomic_fetch_add",
            AtomicRmwOp::Sub => "atomic_fetch_sub",
            AtomicRmwOp::And => "atomic_fetch_and",
            AtomicRmwOp::Or => "atomic_fetch_or",
            AtomicRmwOp::Xor => "atomic_fetch_xor",
            AtomicRmwOp::Min => "atomic_fetch_min",
            AtomicRmwOp::Max => "atomic_fetch_max",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Ir(BuiltinIr),
    /// The typer phase will dissolve this call into some other expression
    TyperInline(BuiltinTyperInline),
    /// The typer phase will provide the implementation of the function
    TyperPhysicalFunction(BuiltinTyperFunction),
    /// The Backend will do this; current backends include: [llvm, vm]
    Backend(BackendBuiltin),
    /// An LLVM intrinsic named verbatim by `intern("llvm.*")`
    /// llvm calls it directly, the VM emulates it by name
    LlvmIntrinsic(StringId),
}

impl Builtin {
    pub fn as_backend_builtin(&self) -> Option<BackendBuiltin> {
        match self {
            Builtin::Backend(k) => Some(*k),
            _ => None,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            Builtin::TyperInline(k) => k.kind_name(),
            Builtin::TyperPhysicalFunction(f) => f.kind_name(),
            Builtin::Backend(backend_builtin) => backend_builtin.kind_name(),
            Builtin::Ir(kind) => kind.kind_name(),
            Builtin::LlvmIntrinsic(_) => "llvm_intrinsic",
        }
    }
}

pub fn make_message(
    idents: &IdentPool,
    message: impl AsRef<str>,
    span: SpanId,
    level: MessageLevel,
) -> K1Message {
    let error_kind = match level {
        MessageLevel::Hint => ErrorKind::None,
        MessageLevel::Info => ErrorKind::None,
        MessageLevel::Warn => ErrorKind::None,
        MessageLevel::Error => ErrorKind::Malformed,
    };
    K1Message { message: idents.intern(message), span, level, error_kind }
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
macro_rules! ice_span {
    ($k1:expr, $span:expr, $($format_args:expr),* $(,)?) => {
        {
            let s: String = format!($($format_args),*);
            $k1.ice_span($span, &s)
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
            .lookup($name)
            .unwrap_or_else(|| panic!("Missing identifier '{}' in pool", $name))
    };
}

fn make_fail_ast_id<A>(ast: &ParsedProgram, message: &str, parsed_id: ParsedId) -> K1Result<A> {
    let span = ast.get_span_for_id(parsed_id);
    Err(make_message(&ast.idents, message, span, MessageLevel::Error))
}

pub fn write_error(
    w: &mut impl std::io::Write,
    ast: &ParsedProgram,
    message: impl AsRef<str>,
    level: MessageLevel,
    span: SpanId,
    use_color: bool,
) -> std::io::Result<()> {
    parse::write_source_location(w, ast, span, level, 6, Some(message.as_ref()), use_color)?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbilityImplKind {
    /// A normal, concrete, typically user-code provided, ability implementation
    Concrete,
    /// A blanket implementation. It's essentially a runnable prototype
    /// that can generate implementations when conditions are met
    Blanket {
        base_ability: AbilityId,
        parsed_id: ParsedAbilityImplId,
    },
    /// An ability impl that was derived from a blanket implementation
    DerivedFromBlanket {
        blanket_impl_id: AbilityImplId,
    },
    /// A 'fake' implementation kind that is cheap to build;
    /// since it never needs to be called; we skip generating functions
    /// for it
    TypeParamConstraint,
    BuiltinDerived,
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

    pub fn is_builtin_derived(&self) -> bool {
        matches!(self, AbilityImplKind::BuiltinDerived)
    }
}

#[derive(Clone, Copy)]
pub enum AbilityImplFunction {
    FunctionId(FunctionId),
    Abstract(FunctionSignature),
    /// The function is not available in this particular impl for this ability for this type.
    /// Currently only used when a default fn has constraints that aren't satisfied
    Unavailable,
}

#[derive(Clone, Copy)]
pub struct TypedAbilityImpl {
    pub kind: AbilityImplKind,
    /// If this is a blanket impl, these are the blanket-level type params
    pub blanket_type_params: TypeIdSlice,
    pub self_type_id: TypeId,
    pub base_ability_id: AbilityId,
    /// base ability id _with_ ability parameters applied
    pub ability_id: AbilityId,
    /// The values for the types that the implementation is responsible for providing.
    /// Yes, they are already baked into the functions but are needed explicitly in order
    /// to do constraint checking
    pub impl_arguments: TypeIdSlice,
    /// Invariant: These functions are ordered how they are defined in the ability, NOT how they appear in
    /// the impl code
    pub functions: PermSlice<AbilityImplFunction>,
    pub scope_id: ScopeId,
    pub span: SpanId,
    /// I need this so that I don't try to instantiate blanket implementations that fail
    /// typechecking
    pub compile_errors: PermList<K1Message>,
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
    is_direct_function_parameter: bool,

    /// `inside_*` locations mean anywhere inside of the thing they describe, including top-level
    is_inside_type_definition_rhs: bool,
    is_inside_static_type: bool,
}
impl_copy_if_small!(12, EvalTypeExprContext);

impl EvalTypeExprContext {
    /// If we descend into a type, we can categorically clear all `direct_`
    /// fields, by definition
    pub fn descended(&self) -> Self {
        EvalTypeExprContext { is_direct_function_parameter: false, ..*self }
    }

    pub const EMPTY: Self = EvalTypeExprContext {
        is_direct_function_parameter: false,
        is_inside_type_definition_rhs: false,
        is_inside_static_type: false,
    };

    pub const VARIABLE_BINDING: Self = EvalTypeExprContext {
        is_direct_function_parameter: false,
        is_inside_type_definition_rhs: false,
        is_inside_static_type: false,
    };
}

// Not using this yet but probably need to be
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub namespace: NamespaceId,
    pub identifier: StringId,
}

#[derive(Debug, Clone)]
pub enum UseStatus {
    Unresolved(ScopeId),
    Resolved(SV4<UseableSymbol>),
}

impl UseStatus {
    pub fn is_resolved(&self) -> bool {
        match self {
            UseStatus::Resolved(_) => true,
            UseStatus::Unresolved(_) => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AbilityImplHandle {
    pub base_ability_id: AbilityId,
    pub specialized_ability_id: AbilityId,
    pub full_impl_id: AbilityImplId,
}

/// How the receiver must be adjusted to reach the impl that was found for it
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfAdjust {
    None,
    AddrOf,
    Deref,
}

/// Allocations that we re-use
pub struct TypedModuleBuffers {
    name_builder: String,
    lexer_tokens: Vec<lex::Token>,
    int_parse: String,
}

nz_u32_id!(ModuleId);

pub const MODULE_ID_CORE: ModuleId = ModuleId::ONE;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ModuleKind {
    Library,
    Executable,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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

#[derive(Clone, Copy)]
pub struct DepEntry {
    pub name: StringId,
    /// Captured (not evaluated) at manifest typecheck; bound when the dep
    /// module evaluates its k1/module-params declaration
    pub params_struct_literal: Option<ParsedExprId>,
}

#[derive(Debug, Clone, Copy)]
pub struct SetupDecl {
    pub outputs: PermSlice<StringId>,
    pub inputs: PermSlice<StringId>,
}

#[derive(Clone, Copy)]
pub struct ModuleManifest {
    pub kind: ModuleKind,
    pub deps: PermSlice<DepEntry>,
    pub libs: PermSlice<LibRef>,
    pub link_args: PermSlice<StringId>,
    pub setup: Option<SetupDecl>,
}

impl ModuleManifest {
    fn defaulted(kind: ModuleKind) -> Self {
        Self {
            kind,
            deps: MSlice::empty(),
            libs: MSlice::empty(),
            link_args: MSlice::empty(),
            setup: None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct ModuleParams {
    pub schema_type: TypeId,
    pub value_id: StaticValueId,
}

#[derive(Clone, Copy)]
pub struct SourceFileHash {
    file_id: FileId,
    hash: u64,
}

#[derive(Clone, Copy)]
pub struct Module {
    pub id: ModuleId,
    pub name: StringId,
    pub home_dir: StringId,
    pub root_file_path: StringId,
    pub manifest: ModuleManifest,
    pub namespace_id: NamespaceId,
    pub namespace_scope_id: ScopeId,
    /// Schema type and merged value of the module's k1/module-params declaration
    pub params: Option<ModuleParams>,
    /// One entry per source file in compile order
    pub source_file_hashes: PermList<SourceFileHash>,
    /// The module's parsed root namespace
    pub parsed_namespace_id: ParsedNamespaceId,
    /// `ns build`, compiled and run at load; the module's own passes skip it
    pub build_ns_defn: Option<ParsedId>,
    /// Directory module vs single file
    pub is_dir: bool,
}

/// The module's compiled `ns build`, live only while the module loads
#[derive(Clone, Copy)]
struct BuildNs {
    parsed_namespace_id: ParsedNamespaceId,
    scope_id: ScopeId,
}

impl BuildNs {
    fn parsed_id(&self) -> ParsedId {
        ParsedId::Namespace(self.parsed_namespace_id)
    }
}

impl Module {
    // Used to 'reserve' a spot for module so that parser can know its module id
    pub fn pending(
        id: ModuleId,
        name: StringId,
        home_dir: StringId,
        root_file_path: StringId,
    ) -> Self {
        Module {
            id,
            name,
            home_dir,
            root_file_path,
            manifest: ModuleManifest::defaulted(ModuleKind::Library),
            namespace_id: NamespaceId::PENDING,
            namespace_scope_id: ScopeId::PENDING,
            params: None,
            source_file_hashes: MList::empty(),
            parsed_namespace_id: ParsedNamespaceId::PENDING,
            build_ns_defn: None,
            is_dir: false,
        }
    }

    /// The root file (module.k1 / <name>.k1 / the single file) is always parsed first
    pub fn root_file_id(&self, mem: &kmem::Mem<TypedProgram>) -> FileId {
        self.source_file_hashes.as_slice(mem)[0].file_id
    }
}

#[derive(Clone, Copy)]
pub struct ProgramSettings {
    pub executable: bool,
}

pub struct MemTmp;
type TmpList<T> = List<T, MemTmp>;
type TmpSlice<T> = MSlice<T, MemTmp>;
type PermSlice<T> = MSlice<T, TypedProgram>;
type PermList<T> = MList<T, TypedProgram>;

pub struct TypedExprPool {
    // SoA pools
    pub exprs: VPool<TypedExpr, TypedExprId>,
    pub type_ids: VPool<TypeId, TypedExprId>,
    pub spans: VPool<SpanId, TypedExprId>,
}

impl TypedExprPool {
    pub fn make() -> Self {
        TypedExprPool {
            exprs: VPool::make("typed_exprs"),
            type_ids: VPool::make("typed_expr_type_ids"),
            spans: VPool::make("typed_expr_spans"),
        }
    }

    pub fn len(&self) -> usize {
        self.exprs.len()
    }

    pub fn add_return(
        &mut self,
        return_value: TypedExprId,
        returned_variable: Option<VariableId>,
        span: SpanId,
    ) -> TypedExprId {
        self.add(
            TypedExpr::Return(TypedReturn { value: return_value, returned_variable }),
            NEVER_TYPE_ID,
            span,
        )
    }

    pub fn add_block(&mut self, builder: BlockBuilder, type_id: TypeId) -> TypedExprId {
        self.add(
            TypedExpr::Block(TypedBlock {
                scope_id: builder.scope_id,
                statements: builder.statements.to_slice(),
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

    pub fn get_full(&self, id: TypedExprId) -> (&TypedExpr, TypeId, SpanId) {
        let expr = self.exprs.get(id);
        let type_id = *self.type_ids.get(id);
        let span = *self.spans.get(id);
        (expr, type_id, span)
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

#[derive(Clone, Copy)]
pub struct TypePendingDefinition {
    pub scope_id: ScopeId,
    pub parsed_id: ParsedTypeDefnId,
}

#[derive(Clone, Copy)]
pub struct UsePendingResolution {
    pub namespace_id: NamespaceId,
    pub scope_id: ScopeId,
    pub use_id: ParsedUseId,
}

#[derive(Clone, Copy)]
pub struct PendingRecursiveInstance {
    pub type_id: TypeId,
    pub generic_parent: TypeId,
    pub type_args: TypeSliceId,
}

#[derive(Clone, Default)]
pub struct TypeDefnContext {
    /// Each entry in this stack contains a set of type ids that are being defined
    /// This is used not only to detect and generate recursive types, but also to
    /// track when recursion has occurred so that we can check for proper layout indirection,
    /// avoiding infinite types
    pub stack: Vec<TypeDefnStackEntry>,

    /// Includes co-recursive mentions, like Foo -> Bar -> Foo
    pub recursive_mentions: Vec<TypeId>,

    /// Reserved instance ids for recursive generic applications, e.g. the `node[t]` inside
    /// node's own definition; filled by substitution once every defn in the cluster is complete
    pub pending_instances: Vec<PendingRecursiveInstance>,

    /// Defns completed in this cluster; finiteness-checked once pending instances are resolved
    pub completed: Vec<(TypeId, SpanId)>,

    /// Aliases whose rhs is being expanded to resolve a re-entrant reference; hitting one
    /// again means the alias cycle contains no nominal type, which cannot terminate
    pub expanding_aliases: Vec<ParsedTypeDefnId>,
}
impl TypeDefnContext {
    fn reset(&mut self) {
        self.stack.clear();
        self.recursive_mentions.clear();
        self.pending_instances.clear();
        self.completed.clear();
        self.expanding_aliases.clear();
    }
}

#[derive(Clone, Copy)]
pub struct CodeChunkPos {
    start: u32,
    end: u32,
    source: SpanId,
}

#[derive(Clone, Copy)]
pub struct EmittedSource {
    pub file_id: FileId,
    pub call_span: SpanId,
    pub entries: MSlice<CodeChunkPos, TypedProgram>,
    has_diagnostic: bool,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct NameInNamespace {
    ns_id: NamespaceId,
    name: StringId,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct TypeAbilityPair {
    self_type_id: TypeId,
    base_ability_id: AbilityId,
}

pub struct TypedProgram {
    pub modules: VPool<Module, ModuleId>,
    /// Fully typechecked modules, in completion order (deps before dependents)
    pub modules_completed: Vec<ModuleId>,
    pub config: CompilerConfig,
    pub program_settings: ProgramSettings,
    pub ast: ParsedProgram,

    pub functions: VPool<TypedFunction, FunctionId>,
    /// (parent function, type args, fnlike type args) -> specialized function.
    /// Rebuilt from `functions` on snapshot restore
    pub function_specializations:
        ahash::HashMap<(FunctionId, TypeSliceId, TypeSliceId), FunctionId>,

    pub variables: VPool<Variable, VariableId>,

    pub types: VPool<Type, TypeId>,
    pub type_hashes: FxHashMap<u64, TypeId>,
    /// Deduped interned slices of TypeIds: equal contents always yield the same
    /// TypeSliceId, so ids serve as exact hash keys and compare in O(1)
    pub type_slices: VPool<TypeIdSlice, TypeSliceId>,
    /// Content index over `type_slices`; rebuilt on snapshot restore
    type_slice_dedup: hashbrown::HashTable<TypeSliceId>,
    pub type_variable_counts: VPool<TypeInfo, TypeId>,
    pub type_instance_info: VPool<Option<GenericInstanceInfo>, TypeId>,
    pub type_defn_info: FxHashMap<TypeId, TypeDefnInfo>,
    pub type_specializations: ahash::HashMap<(TypeId, TypeSliceId), TypeId>,
    pub phys_types: FxHashMap<TypeId, PhysicalTypeResult>,
    /// InferenceHole type ids by hole index, for holes with no static constraint.
    /// `add_type` hash-conses holes to one id per (index, static_type) anyway; this skips
    /// the hash+probe on the common path. PENDING marks not-yet-created indices.
    pub hole_type_cache: Vec<TypeId>,
    pub ast_ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,
    pub builtin_types: BuiltinTypes,
    pub agg_types: VPool<AggregateTypeRecord, AggregateTypeId>,
    /// Lambda types are big, they get extended storage
    pub lambda_types: VPool<LambdaType, LambdaTypeId>,
    pub type_idents: TypeIdents,

    pub globals: VPool<TypedGlobal, TypedGlobalId>,
    pub exprs: TypedExprPool,
    pub calls: VPool<Call, CallId>,
    pub stmts: VPool<TypedStmt, TypedStmtId>,
    pub static_values: StaticValuePool,
    pub type_infos: FxHashMap<TypeId, StaticValueId>,
    pub scopes: Scopes,
    pub messages: RefCell<Vec<K1Message>>,
    pub namespaces: Namespaces,
    pub abilities: VPool<TypedAbility, AbilityId>,
    pub ability_impls: VPool<TypedAbilityImpl, AbilityImplId>,
    /// Key is 'self' type
    pub ability_impl_table: FxHashMap<TypeId, PermList<AbilityImplHandle>>,
    pub ability_impl_table_by_ability: ahash::HashMap<TypeAbilityPair, PermList<AbilityImplHandle>>,
    /// Key is base ability id; the order per base is important; we want earlier
    /// blanket impls to be more specific, and to be tried first
    /// Once a blanket impl succeeds, its added to ability_impl_table
    /// for that type
    pub blanket_impls: FxHashMap<AbilityId, PermList<AbilityImplId>>,
    pub function_name_to_ability_names: FxHashMap<StringId, PermList<StringId>>,
    /// If a namespace is a companion for a generic type, we remember that type's
    /// params here by name so we can re-use them; saves type pool and spec pool bloat
    pub namespace_type_params: FxHashMap<NameInNamespace, TypeId>,
    pub namespace_ast_mappings: FxHashMap<ParsedNamespaceId, NamespaceId>,
    pub function_ast_mappings: FxHashMap<ParsedFunctionId, FunctionId>,
    pub macro_ast_mappings: FxHashMap<parse::ParsedMacroId, FunctionId>,
    pub global_ast_mappings: FxHashMap<ParsedGlobalId, TypedGlobalId>,
    pub globals_in_progress: Vec<TypedGlobalId>,
    pub ability_impl_ast_mappings: FxHashMap<ParsedAbilityImplId, AbilityImplId>,

    pub debug_level_stack: RefCell<Vec<log::LevelFilter>>,
    pub functions_pending_body_specialization: Vec<FunctionId>,
    pub uses_pending_resolution: VecDeque<UsePendingResolution>,
    pub types_pending_definition: VecDeque<TypePendingDefinition>,

    // Status and phases
    module_in_progress: Option<ModuleId>,

    pub ls_entities: RefCell<FxHashMap<FileId, Vec<LsEntity>>>,
    pub completion: Option<CompletionState>,
    /// Per-session LSP compile options; never snapshotted
    pub lsp: crate::compiler::LspCompileOptions,

    /// Interned filename per file, so synthesizing source locations (e.g. for
    /// every assert call) doesn't re-hash the filename string each time
    inference_context_stack: Vec<InferenceContext>,
    inference_context_extras: Vec<InferenceContext>,

    type_defn_context: TypeDefnContext,

    // Buffers that we prefer to re-use to avoid thousands of allocations
    // Clear them after you use them, but leave the memory allocated
    buffers: TypedModuleBuffers,

    pub patterns: TypedPatternPool,

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

    /// Every metaprogram-emitted source, in emission order
    pub emitted_sources: Vec<EmittedSource>,
    pub emitted_parse_cache: FxHashMap<u64, ParsedExprId>,

    // For every static value, once evaluated, we store its runtime representation
    // here; the data lives in vm_static_stack
    pub vm_shared_static_stack: vm::Stack,
    pub vm_global_constant_lookups: FxHashMap<TypedGlobalId, vm::Value>,
    pub vm_static_value_lookups: FxHashMap<StaticValueId, vm::Value>,
    pub vm_process_dlopen_handle: *mut std::ffi::c_void,

    pub vm_dylib_handles: FxHashMap<(ModuleId, StringId), *mut std::ffi::c_void>,
    pub vm_ffi_functions: FxHashMap<FunctionId, vm::VmFfiHandle>,

    pub mem: kmem::Mem<TypedProgram>,
    pub tmp: kmem::Mem<MemTmp>,

    pub ir: ir::ProgramIr,
    pub bc: crate::bc::BcProgram,

    pub timing: Timing,

    pub global_id_k1_arena: Option<TypedGlobalId>,
    pub megarepl: Option<MegareplState>,

    /// Running hash of every compile input consumed so far: build, config, source files
    pub inputs_hash: crate::snap::InputsHash,

    /// diagnostic only, for --chatty and tests
    pub restored_module_count: u32,
}

// SAFETY: TypedProgram's raw pointers point into its own heap allocations
// (kmem arenas, pools) or are process-wide dlopen handles; none of them are
// tied to the thread that created them, so the value may move between
// threads. Sync is deliberately not asserted: concurrent access still needs
// external synchronization (e.g. the server's Mutex).
unsafe impl Send for TypedProgram {}

impl Drop for TypedProgram {
    fn drop(&mut self) {
        unsafe { libc::dlclose(self.vm_process_dlopen_handle) };
        for (id, vm_dylib_handle) in self.vm_dylib_handles.iter() {
            unsafe {
                libc::dlclose(*vm_dylib_handle);
            }
            debug!("Closed dylib handle for module {:?} ident {:?}", id.0, id.1);
        }
    }
}

pub struct Timing {
    pub clock: clock::Clock,
    pub total_infers: usize,
    pub total_infer_nanos: i64,
    pub total_infer_execs: usize,
    pub total_infer_exec_nanos: i64,
    pub total_vm_nanos: i64,
    pub total_vm_instrs: i64,
    pub opcode_counts: [i64; crate::bc::OPCODE_COUNT as usize],
    /// static_value <-> vm memory conversion at unit boundaries (args in,
    /// result out); excluded from total_vm_nanos
    pub total_ferry_nanos: i64,
    pub total_ir_nanos: i64,
    pub total_iropt_nanos: i64,
    pub total_bcgen_nanos: i64,

    // iropt phase breakdown. inline/simplify bracket their phases in
    // optimize_unit; cfg is self-timed inside cfg_compute, so it is *nested
    // within* (not additional to) the other two.
    pub iropt_inline_nanos: i64,
    pub iropt_simplify_nanos: i64,
    pub iropt_cfg_nanos: i64,
    pub iropt_inline_count: i64,
    pub iropt_simplify_passes: i64,
    pub iropt_cfg_computes: i64,
    /// Instructions added to the instrs pool during optimize_unit (inlining
    /// copies + the phis/stores/allocas it synthesizes)
    pub iropt_insts_created: i64,
}

impl Timing {
    pub fn raw(&self) -> u64 {
        self.clock.raw()
    }

    pub fn elapsed_nanos(&self, since: u64) -> u64 {
        self.clock.elapsed_nanos(since)
    }

    pub fn elapsed_ms(&self, since: u64) -> u64 {
        self.clock.elapsed_ms(since)
    }
}

impl TypedProgram {
    pub fn new(
        ast: ParsedProgram,
        config: CompilerConfig,
        lsp: crate::compiler::LspCompileOptions,
    ) -> TypedProgram {
        let completion = lsp
            .completion
            .then(|| CompletionState { marker: ast.idents.intern(COMPLETION_MARKER), site: None });
        let inputs_hash = snapshot::inputs_hash_from_settings(&ast.idents, &config);

        let type_idents = TypeIdents { tag: ast.idents.b.tag, payload: ast.idents.b.payload };
        let mut agg_types = VPool::make("phys_types");
        // Reserve the lower values so they dont conflict with scalars once packed
        agg_types.skip_next_n_slots(PhysicalType::MIN_AGG_ID as usize);

        let root_ident = ast.idents.b.root_module_name;
        let mut scopes = Scopes::make();
        let mut namespaces = Namespaces { namespaces: VPool::make_with_hint("namespaces", 1024) };
        let root_namespace = Namespace {
            name: root_ident,
            scope_id: Scopes::ROOT_SCOPE_ID,
            namespace_type: NamespaceKind::Root,
            companion_type_id: None,
            parent_id: None,
            owner_module: None,
            parsed_id: ParsedId::Namespace(ParsedNamespaceId::ONE),
            lib_name: None,
            reload: false,
        };
        let root_namespace_id = namespaces.add(root_namespace);
        scopes
            .set_scope_owner_id(Scopes::ROOT_SCOPE_ID, ScopeOwnerId::Namespace(root_namespace_id));

        // Add _root ns to the root scope as well so users can use it
        if !scopes.add_namespace(Scopes::ROOT_SCOPE_ID, root_ident, root_namespace_id) {
            panic!("Root namespace was taken, hmmmm");
        }
        let clock = clock::Clock::new();

        let mut vm_static_stack = vm::Stack::make();
        let addr = vm_static_stack.push_t(true as u8);
        let mut vm_global_constant_lookups = FxHashMap::new();
        vm_global_constant_lookups.insert(GLOBAL_ID_K1_IS_STATIC, vm::Value::ptr(addr));
        let process_dlopen_handle =
            unsafe { libc::dlopen(core::ptr::null(), libc::RTLD_LAZY | libc::RTLD_NOLOAD) };
        if process_dlopen_handle.is_null() {
            panic!("Failed to get process dlopen handle");
        }

        let ls_entities = FxHashMap::new();

        let mut inference_context_extras = Vec::with_capacity(4);
        for _ in 0..4 {
            inference_context_extras.push(InferenceContext::make());
        }

        let mut k1 = TypedProgram {
            modules: VPool::make("modules"),
            modules_completed: vec![],
            config,
            program_settings: ProgramSettings { executable: false },
            functions: VPool::make("typed_functions"),
            function_specializations: ahash::HashMap::new(),
            variables: VPool::make("typed_variables"),
            types: VPool::make("types"),
            type_hashes: FxHashMap::new(),
            type_slices: VPool::make("type_slices"),
            type_slice_dedup: hashbrown::HashTable::new(),
            type_variable_counts: VPool::make("type_variable_counts"),
            type_instance_info: VPool::make("instance_info"),
            type_defn_info: FxHashMap::new(),
            type_specializations: ahash::HashMap::new(),
            phys_types: FxHashMap::new(),
            hole_type_cache: Vec::new(),
            ast_ability_mapping: FxHashMap::default(),
            builtin_types: BuiltinTypes::default(),
            agg_types,
            lambda_types: VPool::make("lambdas"),
            type_idents,
            globals: VPool::make("typed_globals"),
            exprs: TypedExprPool::make(),
            calls: VPool::make("typed_calls"),
            stmts: VPool::make("typed_stmts"),
            static_values: StaticValuePool::make(),
            type_infos: FxHashMap::new(),
            scopes,
            messages: RefCell::new(vec![]),
            namespaces,
            abilities: VPool::make("abilities"),
            ability_impls: VPool::make("ability_impls"),
            ability_impl_table: FxHashMap::new(),
            ability_impl_table_by_ability: ahash::HashMap::new(),
            blanket_impls: FxHashMap::new(),
            function_name_to_ability_names: FxHashMap::with_capacity(1024),
            namespace_type_params: FxHashMap::new(),
            namespace_ast_mappings: FxHashMap::with_capacity(512),
            function_ast_mappings: FxHashMap::with_capacity(512),
            macro_ast_mappings: FxHashMap::default(),
            global_ast_mappings: FxHashMap::new(),
            globals_in_progress: vec![],
            ability_impl_ast_mappings: FxHashMap::new(),
            debug_level_stack: RefCell::new(vec![log::max_level()]),
            functions_pending_body_specialization: vec![],
            uses_pending_resolution: VecDeque::new(),
            types_pending_definition: VecDeque::new(),
            ast,
            module_in_progress: None,
            ls_entities: RefCell::new(ls_entities),
            completion,
            lsp,
            inference_context_stack: Vec::new(),
            inference_context_extras,
            type_defn_context: TypeDefnContext::default(),
            buffers: TypedModuleBuffers {
                name_builder: String::new(),
                lexer_tokens: Vec::new(),
                int_parse: String::with_capacity(128),
            },
            patterns: TypedPatternPool::make(),
            vm: Box::new(Some(vm::Vm::make())),
            vm_alts: vec![
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
                vm::Vm::make(),
            ],
            emitted_sources: Vec::new(),
            emitted_parse_cache: FxHashMap::default(),
            vm_shared_static_stack: vm_static_stack,
            vm_global_constant_lookups,
            vm_static_value_lookups: FxHashMap::default(),
            vm_process_dlopen_handle: process_dlopen_handle,
            vm_dylib_handles: FxHashMap::with_capacity(32),
            vm_ffi_functions: FxHashMap::with_capacity(64),

            mem: kmem::Mem::make(),
            tmp: kmem::Mem::make(),

            ir: ir::ProgramIr::make(),
            bc: crate::bc::BcProgram::make(),

            timing: Timing {
                clock,
                total_infers: 0,
                total_infer_nanos: 0,
                total_infer_execs: 0,
                total_infer_exec_nanos: 0,
                total_vm_nanos: 0,
                total_vm_instrs: 0,
                opcode_counts: [0; crate::bc::OPCODE_COUNT as usize],
                total_ferry_nanos: 0,
                total_ir_nanos: 0,
                total_iropt_nanos: 0,
                total_bcgen_nanos: 0,
                iropt_inline_nanos: 0,
                iropt_simplify_nanos: 0,
                iropt_cfg_nanos: 0,
                iropt_inline_count: 0,
                iropt_simplify_passes: 0,
                iropt_cfg_computes: 0,
                iropt_insts_created: 0,
            },
            global_id_k1_arena: None,
            megarepl: None,
            inputs_hash,
            restored_module_count: 0,
        };

        let empty_struct_id = k1.add_type_anon(Type::Struct(StructType::struc(MSlice::empty())));
        k1.builtin_types.empty = empty_struct_id;
        assert_eq!(empty_struct_id, EMPTY_TYPE_ID);

        k1
    }

    pub fn add_module(
        &mut self,
        load_handle: crate::compiler::ModuleLoadHandle,
        primary_module: bool,
    ) -> anyhow::Result<ModuleId> {
        let mut load_stack: SV8<StringId> = smallvec![];
        let mut modules_to_typecheck: SV8<(
            ModuleId,
            Option<crate::compiler::ModuleRemainingSourcesHandle>,
        )> = smallvec![];
        let added_module_id = self.discover_module_and_deps(
            load_handle,
            primary_module,
            &mut load_stack,
            &mut modules_to_typecheck,
        )?;

        if primary_module && self.config.setup_mode.is_setup_only() {
            return Ok(added_module_id);
        }

        // Every root is parsed before any typing, so all headers are hashed
        // in discovery order first
        let mut hash = self.inputs_hash;
        for (module_id, _) in &modules_to_typecheck {
            let module = self.modules.get(*module_id);
            let root = module.source_file_hashes.as_slice(&self.mem)[0];
            let source = self.ast.sources.get(root.file_id);
            hash = hash.add_module_header(
                self.ident_str(module.name),
                self.ast.idents.get_string(source.file_path),
                root.hash,
            );
        }
        for (module_id, remaining) in modules_to_typecheck.into_iter() {
            let files = remaining.map(|r| r.join()).transpose()?;
            let module = self.modules.get(module_id);
            let (module_name, parsed_namespace_id, build_ns_defn) =
                (module.name, module.parsed_namespace_id, module.build_ns_defn);
            let name = self.ident_str(module_name);
            hash = match &files {
                Some(files) => hash.add_module_sources(
                    name,
                    files.iter().map(|f| (f.path.as_str(), f.content_hash)),
                ),
                None => {
                    let file_hashes = module.source_file_hashes.as_slice(&self.mem);
                    hash.add_module_sources(
                        name,
                        file_hashes[1..].iter().map(|sfh| {
                            let s = self.ast.sources.get(sfh.file_id);
                            let path_str = self.ast.idents.get_string(s.file_path);
                            (path_str, sfh.hash)
                        }),
                    )
                }
            };
            let module_hash = hash;
            if let Some(files) = files {
                for file in files {
                    self.parse_module_source_file(
                        module_id,
                        module_name,
                        parsed_namespace_id,
                        file,
                    );
                }
                if !self.ast.errors.is_empty() && !self.lsp.completion {
                    bail!(
                        "Parsing module {} failed with {} errors",
                        self.ident_str(module_name),
                        self.ast.errors.len()
                    );
                }
                self.typecheck_module(module_id, parsed_namespace_id, build_ns_defn)?;
                self.modules_completed.push(module_id);
                // Drain pending IR so a snapshot here contains only whole
                // modules (pending queues empty)
                if self.compile_all_pending_ir(SpanId::NONE).is_err() {
                    bail!("Failed to compile ir");
                };
            }
            self.inputs_hash = module_hash;
            // Sessions compiling overridden content (LSP buffers, completion
            // splices) should not write to the cache
            if self.config.cache
                && self.lsp.source_overrides.is_empty()
                && !self.lsp.completion
                && self.megarepl.is_none()
            {
                if !crate::snap::cache_exists_entry(self.cache_dir(), module_hash) {
                    let cache_dir = self.cache_dir().to_path_buf();
                    let stored = match crate::snap::cache_store_begin(&cache_dir, module_hash) {
                        Ok(mut w) => {
                            self.snap_into(&mut w);
                            crate::snap::cache_store_finish(&cache_dir, module_hash, w)
                        }
                        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
                        Err(e) => Err(e),
                    };
                    if let Err(e) = stored {
                        let warning = self.make_warning(
                            format!("failed to store snapshot cache entry: {e}"),
                            SpanId::NONE,
                        );
                        self.report(warning);
                    }
                }
            }
        }

        let mut spurious_provided_params: Option<(StringId, StringId, ParsedExprId)> = None;
        'outer: for m in self.modules.iter() {
            for entry in self.mem.getn(m.manifest.deps) {
                if let Some(params_expr) = entry.params_struct_literal {
                    let target = self.modules.iter().find(|t| t.name == entry.name).unwrap();
                    if target.params.is_none() {
                        spurious_provided_params = Some((m.name, entry.name, params_expr));
                        break 'outer;
                    }
                }
            }
        }
        if let Some((provider_name, target_name, params_expr)) = spurious_provided_params {
            let span = self.ast.exprs.get_span(params_expr);
            let msg = format!(
                "Module '{}' accepts no parameters (it has no k1/module-params declaration), \
                 but '{}' provides some",
                self.ast.idents.get_string(target_name),
                self.ast.idents.get_string(provider_name),
            );
            return Err(self.module_error(span, msg));
        }

        if primary_module {
            self.validate_exports()?;
        }

        #[cfg(feature = "profile")]
        {
            let mut exprs_by_kind = FxHashMap::new();
            for expr in self.exprs.exprs.iter() {
                let i = exprs_by_kind.entry(expr.kind_name()).or_insert(0);
                *i += 1
            }
            eprintln!("\tExpression kinds:");
            let exprs_by_kind_sorted =
                exprs_by_kind.iter().sorted_by_key(|i| -i.1).collect::<Vec<_>>();
            for (k, v) in exprs_by_kind_sorted.iter() {
                eprintln!("\t\t{}: {}", k, v);
            }
        }

        #[cfg(debug_assertions)]
        if primary_module {
            self.debug_snapshot_roundtrip();
        }

        Ok(added_module_id)
    }

    pub(crate) fn cache_dir(&self) -> &Path {
        Path::new(self.get_string(self.config.cache_dir))
    }

    fn discover_module_and_deps(
        &mut self,
        root_load_handle: crate::compiler::ModuleLoadHandle,
        primary_module: bool,
        load_stack: &mut SV8<StringId>,
        modules_to_typecheck: &mut SV8<(
            ModuleId,
            Option<crate::compiler::ModuleRemainingSourcesHandle>,
        )>,
    ) -> anyhow::Result<ModuleId> {
        debug!("Loading module {}...", root_load_handle.src_path);
        let module_name = root_load_handle.module_name;
        if let Some(m) = self.modules.iter().find(|m| m.name == module_name) {
            // Already discovered this run, or restored from a snapshot; either way its
            // parsed sources, namespaces, and manifest exist. Only the run-scoped work
            // list is rebuilt: queue it (deps first) with readers for incomplete modules.
            fn queue(
                k1: &mut TypedProgram,
                module_id: ModuleId,
                modules_to_typecheck: &mut SV8<(
                    ModuleId,
                    Option<crate::compiler::ModuleRemainingSourcesHandle>,
                )>,
            ) {
                if modules_to_typecheck.iter().any(|(id, _)| *id == module_id) {
                    return;
                }
                let deps = k1.modules.get(module_id).manifest.deps;
                for module_dep in k1.mem.getn(deps) {
                    let dep_name = module_dep.name;
                    let dependent_module = k1
                        .modules
                        .iter()
                        .find(|m| m.name == dep_name)
                        .expect("restored state is missing a discovered module's dep");
                    let dep_id = dependent_module.id;
                    // core and std are discovered by their own add_module calls
                    if dep_id == MODULE_ID_CORE
                        || (!k1.config.no_std && dep_name == k1.ast.idents.b.std)
                    {
                        continue;
                    }
                    queue(k1, dep_id, modules_to_typecheck);
                }
                let remaining = (!k1.modules_completed.contains(&module_id))
                    .then(|| k1.spawn_remaining_sources(module_id));
                modules_to_typecheck.push((module_id, remaining));
            }
            let module_id = m.id;
            queue(self, module_id, modules_to_typecheck);
            return Ok(module_id);
        }

        let home_dir = root_load_handle.module_dir;
        let module_id = self.modules.next_id();
        let module_id = self.modules.add_expected_id(
            Module::pending(
                module_id,
                module_name,
                home_dir,
                root_load_handle.root_source_file_path,
            ),
            module_id,
        );
        let is_core = module_id == MODULE_ID_CORE;
        load_stack.push(module_name);

        let (root_file, remaining_sources) = root_load_handle.await_read_remaining()?;

        let parsed_namespace_id = parse::init_module(module_name, &mut self.ast);
        self.modules.get_mut(module_id).parsed_namespace_id = parsed_namespace_id;
        self.parse_module_source_file(module_id, module_name, parsed_namespace_id, root_file);
        if !self.ast.errors.is_empty() && !self.lsp.completion {
            bail!(
                "Parsing module {} failed with {} errors",
                self.ident_str(module_name),
                self.ast.errors.len()
            );
        }

        self.module_in_progress = Some(module_id);
        self.declare_module_root_namespace(module_id, parsed_namespace_id)?;

        let (manifest, build_ns) = if is_core {
            let manifest = ModuleManifest {
                kind: ModuleKind::Library,
                deps: MSlice::empty(),
                libs: self.mem.pushn(&[LibRef {
                    name: self.ast.idents.intern("k1rt"),
                    link_type: LibRefLinkType::Static,
                }]),
                link_args: MSlice::empty(),
                setup: None,
            };
            (manifest, None)
        } else {
            let build_ns = match self.compile_build_namespace(module_id, parsed_namespace_id) {
                Err(e) => {
                    self.report(e);
                    bail!("Failed to compile ns build of module {}", self.ident_str(module_name))
                }
                Ok(build_ns) => build_ns,
            };
            let manifest_result = match build_ns {
                None => Ok(None),
                Some(build_ns) => self.evaluate_module_manifest(build_ns.scope_id, primary_module),
            };
            match manifest_result {
                Err(e) => {
                    self.report(e);
                    bail!(
                        "Failed to evaluate the manifest of module {}",
                        self.ident_str(module_name)
                    )
                }
                Ok(None) => {
                    let kind =
                        if primary_module { ModuleKind::Executable } else { ModuleKind::Library };
                    (ModuleManifest::defaulted(kind), build_ns)
                }
                Ok(Some(manifest)) => (manifest, build_ns),
            }
        };

        if manifest.kind == ModuleKind::Executable {
            if let Some(m) = self
                .modules
                .iter()
                .find(|m| m.id != module_id && m.manifest.kind == ModuleKind::Executable)
            {
                bail!(
                    "Cannot compile a program with 2 executable modules. {} and {}",
                    self.ident_str(m.name),
                    self.ident_str(module_name)
                );
            }
        }

        if primary_module {
            self.program_settings.executable = manifest.kind == ModuleKind::Executable;
        }

        self.module_in_progress = None;

        let deps = manifest.deps;
        let build_ns_span =
            build_ns.map(|b| self.ast.get_span_for_id(b.parsed_id())).unwrap_or(SpanId::NONE);
        let m = self.modules.get_mut(module_id);
        m.manifest = manifest;
        m.build_ns_defn = build_ns.map(|b| b.parsed_id());
        m.is_dir = remaining_sources.is_dir();

        let setup_decl = self.modules.get(module_id).manifest.setup;
        let mut setup_ran = false;
        if let Some(setup) = setup_decl {
            let force = self.config.setup_mode
                == (crate::compiler::SetupMode::SetupOnly { force: true })
                && primary_module;
            let started = {
                let request = self.setup_request(module_id, setup, force);
                let tmp = self.get_tmp_unsafe();
                let mark = tmp.mark();
                let result = crate::compiler::start_setup(&request, tmp);
                tmp.reset_to(mark);
                result
            };
            let started = match started {
                Ok(started) => started,
                Err(e) => {
                    let msg =
                        format!("Setup failed for module '{}': {e:#}", self.ident_str(module_name));
                    return Err(self.module_error(build_ns_span, msg));
                }
            };
            if let Some(started) = started {
                let Some(build_ns) = build_ns else {
                    self.ice_span(build_ns_span, "setup was declared with no ns build")
                };
                if let Err(e) = self.execute_setup_fn(build_ns.scope_id, home_dir, build_ns_span) {
                    self.report(e);
                    bail!("fn setup failed for module {}", self.ident_str(module_name))
                }
                let finished = {
                    let request = self.setup_request(module_id, setup, force);
                    let tmp = self.get_tmp_unsafe();
                    let mark = tmp.mark();
                    let result = crate::compiler::finish_setup(&request, started, tmp);
                    tmp.reset_to(mark);
                    result
                };
                if let Err(e) = finished {
                    let msg =
                        format!("Setup failed for module '{}': {e:#}", self.ident_str(module_name));
                    return Err(self.module_error(build_ns_span, msg));
                }
                setup_ran = true;
            }
        }

        let remaining = remaining_sources.into_read_sources_handle(
            &self.ast.idents,
            setup_ran,
            &self.lsp.source_overrides,
        );
        let mut dep_handles: SV8<crate::compiler::ModuleLoadHandle> = smallvec![];
        for dep in self.mem.getn(deps) {
            let dep_name = dep.name;
            if self.ast.idents.get_string(dep_name) == self.program_name() {
                let msg = if dep_name == module_name {
                    format!("Module '{}' cannot depend on itself", self.ident_str(module_name))
                } else {
                    format!(
                        "Module '{}' depends on '{}': module name collision with the program itself",
                        self.ident_str(module_name),
                        self.ast.idents.get_string(dep_name)
                    )
                };
                return Err(self.module_error(build_ns_span, msg));
            }
            if load_stack.contains(&dep_name) {
                let mut cycle: Vec<&str> = vec![];
                for n in load_stack.iter().skip_while(|n| **n != dep_name) {
                    cycle.push(self.ast.idents.get_string(*n));
                }
                cycle.push(self.ast.idents.get_string(dep_name));
                let msg = format!("Module dependency cycle: {}", cycle.join(" -> "));
                return Err(self.module_error(build_ns_span, msg));
            }
            if self.modules.iter().any(|m| m.name == dep_name) {
                continue;
            }
            let local_module_deps_path = kpath::join_tmp(
                &mut self.tmp,
                &self.ast.idents,
                self.config.home_dir,
                ("deps", dep_name),
            );
            let k1_home_modules_path = kpath::join_tmp(
                self.get_tmp_unsafe(),
                &self.ast.idents,
                self.config.k1_home,
                ("modules", dep_name),
            );
            let dep_path = if Path::new(local_module_deps_path.as_str()).exists() {
                local_module_deps_path
            } else if Path::new(k1_home_modules_path.as_str()).exists() {
                k1_home_modules_path
            } else {
                let msg = format!(
                    "Module '{}' depends on '{}', which was not found. Searched locally at {local_module_deps_path} and searched installed modules at {k1_home_modules_path}",
                    self.ident_str(module_name),
                    self.ident_str(dep_name),
                );
                return Err(self.module_error(build_ns_span, msg));
            };
            let dep_path_id = self.ast.idents.intern(dep_path);
            match crate::compiler::spawn_module_load(
                &self.ast.idents,
                &mut self.ast.tmp,
                dep_path_id,
                false,
                &self.lsp.source_overrides,
            ) {
                Ok(handle) => dep_handles.push(handle),
                Err(e) => {
                    let msg = format!(
                        "Module '{}' depends on '{}', which failed to load: {}",
                        self.ident_str(module_name),
                        self.ident_str(dep_name),
                        e
                    );
                    return Err(self.module_error(build_ns_span, msg));
                }
            }
        }

        // All dep reads are in flight before we recurse into any of them
        for handle in dep_handles {
            self.discover_module_and_deps(handle, false, load_stack, modules_to_typecheck)?;
        }

        load_stack.pop();
        modules_to_typecheck.push((module_id, Some(remaining)));
        Ok(module_id)
    }

    fn spawn_remaining_sources(
        &self,
        module_id: ModuleId,
    ) -> crate::compiler::ModuleRemainingSourcesHandle {
        let module = self.modules.get(module_id);
        let (home_dir, is_dir, root_source_file_path) =
            (module.home_dir, module.is_dir, module.root_file_path);
        crate::compiler::spawn_sources_read(
            &self.ast.idents,
            home_dir,
            root_source_file_path,
            is_dir,
            &self.lsp.source_overrides,
        )
    }

    fn declare_module_root_namespace(
        &mut self,
        module_id: ModuleId,
        module_root_parsed_namespace: ParsedNamespaceId,
    ) -> anyhow::Result<()> {
        let declared = self.declare_namespace(module_root_parsed_namespace, Scopes::ROOT_SCOPE_ID);
        let typed_namespace_id = match declared {
            Err(e) => {
                self.report(e);
                bail!("{} failed namespace declaration phase", self.program_name())
            }
            Ok(id) => id,
        };
        let scope_id = self.namespaces.get(typed_namespace_id).scope_id;
        let module = self.modules.get_mut(module_id);
        module.namespace_id = typed_namespace_id;
        module.namespace_scope_id = scope_id;

        if module_id != MODULE_ID_CORE {
            // takes 14us last I checked
            self.add_core_uses_to_scope(scope_id, SpanId::NONE)
                .map_err(|e| self.message_to_anyhow(e))?;
        }
        Ok(())
    }

    /// `ns build` is the module's pre-module world: compiled against core and std
    /// only, before its deps load and before its remaining sources exist, since
    /// `fn setup` is what generates them. It declares its own uses; the module's
    /// file-level uses are not resolved yet
    fn compile_build_namespace(
        &mut self,
        module_id: ModuleId,
        module_root_parsed_namespace: ParsedNamespaceId,
    ) -> K1Result<Option<BuildNs>> {
        let build_ident = self.ast.idents.b.build;
        let parsed_ns = self.ast.namespaces.get(module_root_parsed_namespace);
        let mut build_ns_parsed_id = None;
        for defn in parsed_ns.definitions.as_slice(&self.ast.mem) {
            let Some(ns_id) = defn.as_namespace_id() else { continue };
            if self.ast.namespaces.get(ns_id).name == build_ident {
                build_ns_parsed_id = Some(ns_id);
                break;
            }
        }
        let Some(build_ns_parsed_id) = build_ns_parsed_id else { return Ok(None) };
        let span = self.ast.namespaces.get(build_ns_parsed_id).span;

        let module_scope = self.modules.get(module_id).namespace_scope_id;
        let build_ns_id = self.declare_namespace(build_ns_parsed_id, module_scope)?;
        if self.run_all_phases_on_ns(build_ns_parsed_id, module_id, &[]).is_err() {
            // The phases report their own messages
            return self.make_fail("ns build failed to compile", span);
        }
        let scope_id = self.namespaces.get(build_ns_id).scope_id;
        Ok(Some(BuildNs { parsed_namespace_id: build_ns_parsed_id, scope_id }))
    }

    fn setup_request(
        &self,
        module_id: ModuleId,
        setup: SetupDecl,
        force: bool,
    ) -> crate::compiler::SetupRequest<'_> {
        let m = self.modules.get(module_id);
        let root_filename =
            self.ast.sources.get(m.root_file_id(&self.mem)).filename(&self.ast.idents);
        crate::compiler::SetupRequest {
            idents: &self.ast.idents,
            module_dir: m.home_dir,
            module_name: m.name,
            root_filename,
            outputs: self.mem.getn(setup.outputs),
            inputs: self.mem.getn(setup.inputs),
            target: self.config.target,
            force,
        }
    }

    fn execute_setup_fn(
        &mut self,
        build_ns_scope: ScopeId,
        module_dir: StringId,
        decl_span: SpanId,
    ) -> K1Result<()> {
        let Some(setup_fn_id) =
            self.scopes.find_function_local(build_ns_scope, self.ast.idents.b.setup)
        else {
            return self.make_fail(
                "a module that declares setup needs a `fn setup(ctx: k1/setup-ctx)` in its ns build",
                decl_span,
            );
        };
        let setup_ctx_type = self.builtin_types.k1_setup_ctx.unwrap();
        let function = self.get_function(setup_fn_id);
        let span = self.ast.get_span_for_id(function.parsed_id);
        let has_type_params = !function.type_params.is_empty();
        let params = self.mem.getn(self.get_function_type(setup_fn_id).logical_params());
        if has_type_params || params.len() != 1 || params[0].type_id != setup_ctx_type {
            return self
                .make_fail("fn setup must take exactly one parameter of type k1/setup-ctx", span);
        }

        let dir_value = self.static_values.add_string(module_dir);
        let ctx_fields = self.static_values.mem.pushn(&[dir_value]);
        let ctx_value = self
            .static_values
            .add(StaticValue::Struct(StaticStruct { type_id: setup_ctx_type, fields: ctx_fields }));
        self.execute_static_function(setup_fn_id, &[ctx_value], span)?;
        Ok(())
    }

    fn module_error(&mut self, span: SpanId, msg: String) -> anyhow::Error {
        let e = self.make_error(&msg, span);
        self.report(e);
        anyhow::anyhow!(msg)
    }

    fn parse_module_source_file(
        &mut self,
        module_id: ModuleId,
        module_name: StringId,
        parsed_namespace_id: ParsedNamespaceId,
        file: crate::compiler::SourceFile,
    ) -> FileId {
        let path = self.ast.idents.intern(&file.path);
        let source = parse::SourceFile::make(&mut self.ast.mem, path, &file.content);
        let mut token_buffer = std::mem::take(&mut self.buffers.lexer_tokens);
        let (file_id, lex_result) =
            parse::lex_file_into_program(&mut self.ast, source, &mut token_buffer);
        self.modules
            .get_mut(module_id)
            .source_file_hashes
            .push_grow(&mut self.mem, SourceFileHash { file_id, hash: file.content_hash });
        if let Err(e) = lex_result {
            self.ast.report_error(e);
            self.buffers.lexer_tokens = token_buffer;
            return file_id;
        }

        if cfg!(feature = "lsp") {
            let tokens = self.ast.mem.pushn(&token_buffer);
            self.ast.sources.get_mut(file_id).tokens = tokens;
        };

        let mut parser = parse::Parser::make_for_file(
            module_id,
            module_name,
            parsed_namespace_id,
            &mut self.ast,
            &token_buffer,
            file_id,
        );
        parser.parse_file_into_module();
        self.buffers.lexer_tokens = token_buffer;
        file_id
    }

    /// Retrieve the current inference context
    fn ictx(&self) -> &InferenceContext {
        self.inference_context_stack.last().as_ref().unwrap()
    }

    /// Retrieve the current inference context
    fn ictx_mut(&mut self) -> &mut InferenceContext {
        self.inference_context_stack.last_mut().unwrap()
    }

    /// Skip the borrow checker, only when we must, to get access
    /// to the tmp arena without exclusive access to the TypedProgram
    #[allow(clippy::mut_from_ref)]
    #[allow(invalid_reference_casting)]
    pub(crate) fn get_tmp_unsafe(&self) -> &mut kmem::Mem<MemTmp> {
        unsafe {
            let ptr = &self.tmp as *const Mem<MemTmp>;
            let ptr_mut: *mut Mem<MemTmp> = ptr.cast_mut();
            &mut *ptr_mut
        }
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

    fn with_clean_inference<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::take(&mut self.inference_context_stack);
        let result = f(self);
        debug_assert!(
            self.inference_context_stack.is_empty(),
            "inference context leaked from clean domain"
        );
        self.inference_context_stack = saved;
        result
    }

    pub fn push_debug_level(&self) {
        let level = log::LevelFilter::Debug;
        self.debug_level_stack.borrow_mut().push(level);
        log::set_max_level(level);
        debug!("push max_level is now {}", log::max_level())
    }

    pub fn pop_debug_level(&self) {
        let mut stack = self.debug_level_stack.borrow_mut();
        stack.pop();
        log::set_max_level(*stack.last().unwrap());
        debug!("pop max_level is now {}", log::max_level())
    }

    pub fn function_iter(&self) -> impl Iterator<Item = (FunctionId, &TypedFunction)> {
        self.functions.iter_with_ids()
    }

    pub fn get_function_span(&self, function_id: FunctionId) -> SpanId {
        self.ast.get_span_for_id(self.functions.get(function_id).parsed_id)
    }

    pub fn program_name(&self) -> &str {
        self.ast.name_str()
    }

    pub fn get_string(&self, string_id: StringId) -> &str {
        self.ast.idents.get_string(string_id)
    }

    pub fn name_of_type(&self, type_id: TypeId) -> &str {
        match self.get_defn_info(type_id) {
            None => self.types.get(type_id).kind_name(),
            Some(info) => self.ident_str(info.name),
        }
    }

    pub fn ident_str(&self, id: StringId) -> &str {
        self.ast.idents.get_string(id)
    }

    pub fn ident_str_opt(&self, id: Option<StringId>) -> &str {
        match id {
            Some(id) => self.ast.idents.get_string(id),
            None => "<no name>",
        }
    }

    pub fn build_ident_with(
        &mut self,
        mut f: impl FnMut(&mut TypedProgram, &mut String),
    ) -> StringId {
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

    pub fn primary_module(&self) -> &Module {
        if let Some(id) = self.module_in_progress {
            return self.modules.get(id);
        }
        self.modules
            .iter()
            .find(|m| m.manifest.kind == ModuleKind::Executable)
            .unwrap_or_else(|| self.modules.iter().last().unwrap())
    }

    pub fn primary_module_completed(&self) -> bool {
        let id = self.primary_module().id;
        self.modules_completed.contains(&id)
    }

    pub fn validate_exports(&mut self) -> anyhow::Result<()> {
        let mut symbols: FxHashMap<StringId, SpanId> = FxHashMap::default();
        let mut duplicate: Option<(StringId, SpanId)> = None;
        for (function_id, function) in self.function_iter() {
            let Linkage::Exported { fn_name } = function.linkage else {
                continue;
            };
            let symbol = fn_name.unwrap_or(function.name);
            let span = self.get_function_span(function_id);
            if symbols.insert(symbol, span).is_some() {
                duplicate = Some((symbol, span));
                break;
            }
        }
        if duplicate.is_none() {
            for global_id in self.globals.iter_ids() {
                let global = self.globals.get(global_id);
                if !global.is_exported {
                    continue;
                }
                let symbol = self.variables.get(global.variable_id).name;
                if symbols.insert(symbol, global.span).is_some() {
                    duplicate = Some((symbol, global.span));
                    break;
                }
            }
        }
        if let Some((symbol, span)) = duplicate {
            let msg = kerr!(self, span, "Duplicate exported symbol '{}'", self.ident_str(symbol));
            self.report(msg);
            bail!("Duplicate exported symbol");
        }
        Ok(())
    }

    pub fn get_main_function_id(&self) -> Option<FunctionId> {
        if let Some(exec_module) =
            self.modules.iter().find(|m| m.manifest.kind == ModuleKind::Executable)
        {
            self.scopes.find_function_local(exec_module.namespace_scope_id, self.ast.idents.b.main)
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

    fn push_block_expr_id(&mut self, block: &mut BlockBuilder, expr: TypedExprId) {
        let ty = self.exprs.get_type(expr);
        self.push_block_stmt(block, TypedStmt::Expr(expr, ty))
    }

    pub fn get_stmt_type(&self, stmt: TypedStmtId) -> TypeId {
        match self.stmts.get(stmt) {
            TypedStmt::Expr(_, ty) => *ty,
            TypedStmt::Let(_)
            | TypedStmt::Assignment(_)
            | TypedStmt::Require(_)
            | TypedStmt::Defer(_) => self.builtin_types.empty,
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

    fn evaluate_module_manifest(
        &mut self,
        build_ns_scope: ScopeId,
        primary_module: bool,
    ) -> K1Result<Option<ModuleManifest>> {
        let Some(manifest_fn_id) =
            self.scopes.find_function_local(build_ns_scope, self.ast.idents.b.module)
        else {
            return Ok(None);
        };
        let function = self.get_function(manifest_fn_id);
        let fn_span = self.ast.get_span_for_id(function.parsed_id);
        let module_type_id = self.builtin_types.k1_module.unwrap();
        let params = self.get_function_type(manifest_fn_id).logical_params();
        if !function.type_params.is_empty() || !params.is_empty() {
            kbail!(self, fn_span, "fn module takes no parameters");
        }
        if self.get_function_type(manifest_fn_id).return_type != module_type_id {
            kbail!(self, fn_span, "fn module must return k1/module");
        }
        let manifest_result = self.execute_static_function(manifest_fn_id, &[], fn_span)?;

        let StaticValue::Struct(value) = self.static_values.get(manifest_result) else {
            self.ice_span(fn_span, "module manifest value was not a struct");
        };
        let fields: [StaticValueId; 5] =
            self.static_values.mem.getn(value.fields).try_into().unwrap();

        let kind = match self.static_values.get(fields[0]).as_sum().unwrap().payload {
            None => {
                if primary_module {
                    ModuleKind::Executable
                } else {
                    ModuleKind::Library
                }
            }
            Some(kind_value_id) => {
                let (_, int_value) = self.static_values.get(kind_value_id).as_enum().unwrap();
                match int_value.as_u8().unwrap() {
                    0 => ModuleKind::Library,
                    1 => ModuleKind::Executable,
                    k => panic!("Unrecognized module kind index: {k}"),
                }
            }
        };

        let deps_container = *self.static_values.get(fields[1]).as_container().unwrap();
        let mut deps: List<DepEntry, _> = self.mem.new_list(deps_container.elements.len());
        for dep_value_id in self.static_values.get_slice(deps_container.elements) {
            let entry = self.static_values.get(*dep_value_id).as_struct().unwrap();
            let entry_fields = self.static_values.get_slice(entry.fields);
            let name = self.static_values.get(entry_fields[0]).as_string().unwrap();
            let StaticValue::Int(TypedIntValue::U64(params_raw)) =
                self.static_values.get(entry_fields[1])
            else {
                self.ice_span(fn_span, "dep-entry params-expr-id was not a u64");
            };
            let params_struct_literal = match ParsedExprId::from_u32(*params_raw as u32) {
                None => None,
                Some(id) => {
                    let valid = matches!(self.ast.exprs.get_opt(id), Some(ParsedExpr::Struct(_)));
                    if !valid {
                        kbail!(self, fn_span, "Invalid dep params id; must be a struct literal");
                    }
                    Some(id)
                }
            };
            deps.push(DepEntry { name, params_struct_literal });
        }
        let deps = deps.to_slice();

        let libs_container = *self.static_values.get(fields[2]).as_container().unwrap();
        let mut libs: List<LibRef, _> = self.mem.new_list(libs_container.elements.len());
        for lib_ref_value_id in self.static_values.get_slice(libs_container.elements) {
            let lib_ref_struct = self.static_values.get(*lib_ref_value_id).as_struct().unwrap();
            let lib_ref_fields = self.static_values.get_slice(lib_ref_struct.fields);
            let name = self.static_values.get(lib_ref_fields[0]).as_string().unwrap();
            let link_type_u8 =
                self.static_values.get(lib_ref_fields[1]).as_enum().unwrap().1.as_u8().unwrap();
            let link_type = match link_type_u8 {
                0 => LibRefLinkType::Default,
                1 => LibRefLinkType::Static,
                2 => LibRefLinkType::Dynamic,
                _ => panic!("Bad value for link-kind enum"),
            };
            libs.push(LibRef { name, link_type })
        }
        let libs = libs.to_slice();

        let link_args_container = *self.static_values.get(fields[3]).as_container().unwrap();
        let statics = &self.static_values;
        let link_args = self.mem.pushn_iter(
            statics
                .get_slice(link_args_container.elements)
                .iter()
                .map(|link_arg| statics.get(*link_arg).as_string().unwrap()),
        );

        let setup = match self.static_values.get(fields[4]).as_sum().unwrap().payload {
            None => None,
            Some(setup_value_id) => {
                let setup_struct = self.static_values.get(setup_value_id).as_struct().unwrap();
                let setup_fields = self.static_values.get_slice(setup_struct.fields);
                let mut string_list = |field: StaticValueId| {
                    let container = *statics.get(field).as_container().unwrap();
                    self.mem.pushn_iter(
                        statics
                            .get_slice(container.elements)
                            .iter()
                            .map(|id| statics.get(*id).as_string().unwrap()),
                    )
                };
                let outputs = string_list(setup_fields[0]);
                let inputs = string_list(setup_fields[1]);
                Some(SetupDecl { outputs, inputs })
            }
        };

        Ok(Some(ModuleManifest { kind, deps, libs, link_args, setup }))
    }

    /// Resolves a type parameter to its meaning in `scope_id` via the id-keyed
    /// substitution bindings; a miss means the parameter stands for itself
    /// (the normal case inside its own generic context).
    fn get_type_id_resolved(&self, type_id: TypeId, scope_id: ScopeId) -> TypeId {
        match self.types.get(type_id) {
            Type::TypeParameter(_) => match self.scopes.find_type_substitution(scope_id, type_id) {
                None => type_id,
                Some((to, _)) if to == type_id => type_id,
                Some((to, _)) => self.get_type_id_resolved(to, scope_id),
            },
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
    /// - Return expressions
    /// - Last statement of a block
    /// - Assignment rhs
    fn check_expr_type(
        &mut self,
        expected: TypeId,
        expr: TypedExprId,
        scope_id: ScopeId,
        allow_addr_of: bool,
    ) -> CheckExprTypeResult {
        let actual_type_id = self.exprs.get_type(expr);

        let check_result = self.check_types(expected, actual_type_id, scope_id);
        let Err(msg) = check_result else { return CheckExprTypeResult::Ok };

        // Static lifting and erasing
        match (self.types.get(expected), self.get_static_type_of_type(actual_type_id)) {
            // If we failed typechecking, and passed a static, then see
            // whether the type inside this static would pass muster under the 'expected type',
            // If so, erase the static
            (_, Some(actual_static))
                if self.check_types(expected, actual_static.family_type_id, scope_id).is_ok() =>
            {
                let span = self.exprs.get_span(expr);
                let materialized_value = self.materialize_static_value(
                    actual_static.family_type_id,
                    actual_static.value_id,
                    span,
                );
                return CheckExprTypeResult::Coerce(materialized_value, "static_materialize");
            }
            // If we failed typechecking, and we expected a static, and we passed a non-static
            // Try to lift it
            (Type::StaticValue(expected_value_type), None) => {
                if expected_value_type.family_type_id == actual_type_id {
                    if let Ok(static_lifted) = self.attempt_static_lift(expr) {
                        let static_lifted_type = self.exprs.get_type(static_lifted);
                        return match self.check_types(expected, static_lifted_type, scope_id) {
                            Err(msg) => CheckExprTypeResult::Err(k1_format_user!(
                                self,
                                "Static lift resulted in wrong value: {}",
                                msg.as_str()
                            )),
                            Ok(_) => CheckExprTypeResult::Coerce(static_lifted, "static_lift"),
                        };
                    }
                }
            }
            _ => {}
        };

        // If we expect a lambda object and you pass a lambda
        if let Type::LambdaObject(_lam_obj_type) = self.types.get(expected) {
            if let Type::Lambda(lambda_type_id) = self.get_expr_type(expr) {
                let lambda_type_id = *lambda_type_id;
                let lambda_type = self.lambda_types.get(lambda_type_id);
                let lambda_object_type =
                    self.add_lambda_object(lambda_type.function_type, lambda_type.parsed_id);
                match self.check_types(expected, lambda_object_type, scope_id) {
                    Ok(_) => {
                        return match self.lambda_to_lambda_object(expr, lambda_type_id, scope_id) {
                            Ok(lambda_object) => {
                                CheckExprTypeResult::Coerce(lambda_object, "lam->lamobj")
                            }
                            Err(e) => CheckExprTypeResult::Err(
                                self.ast.idents.get_string(e.message).into(),
                            ),
                        };
                    }
                    Err(msg) => {
                        debug!("coerce: detected lam obj case failed: {msg}");
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
                    return CheckExprTypeResult::Coerce(lambda_object, "funref->lamobj");
                }
            }
        }

        if let Type::Reference(_exp_ref) = self.types.get(expected) {
            if let Type::Reference(_actual_ref) = self.types.get(actual_type_id) {
            } else {
                if allow_addr_of {
                    // A reference is expected, and a reference is not provided
                    // If the expr is a place, we can inject an address_of; the place
                    // walk in synth_address_of decides
                    let span = self.exprs.get_span(expr);
                    if let Ok(expr) = self.synth_address_of(expr, span, false) {
                        // self.report_hint(span, "coerce address of");
                        return CheckExprTypeResult::Coerce(expr, "address_of");
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
                                CheckExprTypeResult::Coerce(widened, "widen signed")
                            }
                            (false, false) => {
                                let widened = self.synth_cast(
                                    expr,
                                    expected,
                                    CastType::IntegerCast(IntegerCastDirection::Extend),
                                    None,
                                );
                                CheckExprTypeResult::Coerce(widened, "widen unsigned")
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
                                CheckExprTypeResult::Coerce(to_signed, "widen->unsigned->signed")
                            }
                        }
                    } else {
                        // We never truncate automatically, or change signedness without extension
                        CheckExprTypeResult::Err(msg)
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
                    match self.check_expr_type(expected, dereferenced, scope_id, allow_addr_of) {
                        CheckExprTypeResult::Ok => {
                            return CheckExprTypeResult::Coerce(dereferenced, "deref");
                        }
                        CheckExprTypeResult::Err(_) => {}
                        CheckExprTypeResult::Coerce(typed_expr_id, reason1) => {
                            debug!("coercion rule {reason1} applied under deref");
                            return CheckExprTypeResult::Coerce(typed_expr_id, reason1);
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
        allow_addr_of: bool,
    ) -> K1Result<TypedExprId> {
        debug!(
            "check_and_coerce `{}`, expected: {}",
            self.expr_to_string(expr),
            self.type_id_to_string(expected)
        );
        match self.check_expr_type(expected, expr, scope_id, allow_addr_of) {
            CheckExprTypeResult::Err(msg) => {
                let span = self.exprs.get_span(expr);
                Err(K1Message {
                    message: self.ast.idents.intern(msg),
                    span,
                    level: MessageLevel::Error,
                    error_kind: ErrorKind::TypeError,
                })
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

    // Due to the fact that check_types may get called multiple times for
    // a given expression, for example in many expression positions we'll
    // attempt a coerce after typechecking fails. This means we have a scenario
    // where we're formatting a big string (the typecheck error msg) in the
    // happy path of successful compilation, so we need it to be held to the same
    // standards of performance as the rest of compilation. So we build the string
    // up in our arena, using our own machinery designed for zero-allocation, configurable, rich formatting
    // That's why we return this `MStr` thing here (its essentially a raw string pointer; since it points into TypedProgram technically)
    pub fn check_types(
        &self,
        expected: TypeId,
        actual: TypeId,
        scope_id: ScopeId,
    ) -> Result<(), MStr<MemTmp>> {
        debug!(
            "typecheck: {} <: {}",
            self.type_id_to_string(actual).blue(),
            self.type_id_to_string(expected).blue(),
        );
        if expected == actual {
            return Ok(());
        }

        if let (Some(spec1), Some(spec2)) =
            (self.get_instance_info(expected), self.get_instance_info(actual))
        {
            return if spec1.generic_parent == spec2.generic_parent {
                for (index, (exp_param, act_param)) in self
                    .get_type_slice(spec1.type_args)
                    .iter()
                    .zip(self.get_type_slice(spec2.type_args).iter())
                    .enumerate()
                {
                    debug!(
                        "Comparing params {} and {} inside {}",
                        self.type_id_to_string(*exp_param),
                        self.type_id_to_string(*act_param),
                        self.ident_str(self.get_defn_info(spec1.generic_parent).unwrap().name)
                    );
                    if let Err(msg) = self.check_types(*exp_param, *act_param, scope_id) {
                        let generic = self.types.get(spec1.generic_parent).expect_generic();
                        let param = *self.mem.get_nth(generic.params, index);
                        let msg = k1_format_user!(
                            self,
                            "Expected {} but got {}: Param '{}' is incorrect: {}",
                            expected,
                            actual,
                            self.get_type_parameter(param).name,
                            msg.as_str(),
                        );
                        return Err(msg);
                    }
                }
                Ok(())
            } else {
                Err(k1_format_user!(self, "Expected {}, but got {}", expected, actual))
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

        let expected = self.get_value_type_id_of_type(expected).unwrap_or(expected);
        let actual = self.get_value_type_id_of_type(actual).unwrap_or(actual);

        debug!(
            "typecheck resolved: {} <: {}",
            self.type_id_to_string(actual).blue(),
            self.type_id_to_string(expected).blue(),
        );

        match (self.types.get(expected), self.types.get(actual)) {
            (Type::InferenceHole(_hole), _any) => Ok(()),
            (Type::Struct(s1), Type::Struct(s2)) => {
                // If I expect a certain nominal type
                // I won't accept an anonymous type
                // Nor a nominal type of a different kind
                // But I will accept the same nominal type,
                // But if I expect an anonymous struct, the a named or anonymous one will do
                // if it matches structurally
                let expected_defn_info = self.get_defn_info(expected);
                let actual_defn_info = self.get_defn_info(actual);
                if let Some(expected_defn_info) = expected_defn_info {
                    match actual_defn_info {
                        None => {
                            debug!("expected_defn_info some actual none");
                            return Err(k1_format_user!(
                                self,
                                "Expected named struct {} but got anonymous struct {}",
                                expected,
                                actual
                            ));
                        }
                        Some(actual_defn_info) => {
                            debug!("expected_defn_info some actual some");
                            if expected_defn_info.name == actual_defn_info.name
                                && expected_defn_info.scope == actual_defn_info.scope
                            {
                                // Same nominal type, we're good
                            } else {
                                return Err(k1_format_user!(
                                    self,
                                    "Expected named struct {} but got named struct {}",
                                    expected,
                                    actual
                                ));
                            }
                        }
                    }
                }

                // Proceed to structural typecheck
                if s1.record_kind != s2.record_kind {
                    return Err(k1_format_user!(
                        self,
                        "Expected a {} but got a {}",
                        s1.record_kind.kind_name(),
                        s2.record_kind.kind_name()
                    ));
                }
                if s1.fields.len() != s2.fields.len() {
                    return Err(k1_format_user!(
                        self,
                        "expected struct {} but got struct {}",
                        expected,
                        actual
                    ));
                }
                for (f1, f2) in self.mem.getn(s1.fields).iter().zip(self.mem.getn(s2.fields)) {
                    if f1.name != f2.name {
                        return Err(k1_format_user!(
                            self,
                            "field names differ {}, {}",
                            f1.name,
                            f2.name
                        ));
                    }
                    if let Err(msg) = self.check_types(f1.type_id, f2.type_id, scope_id) {
                        return Err(k1_format_user!(
                            self,
                            "Struct field {} type mismatch: {msg}",
                            f1.name
                        ));
                    };
                }
                Ok(())
            }
            (Type::Reference(exp_ref), Type::Reference(act_ref)) => {
                self.check_types(exp_ref.inner_type, act_ref.inner_type, scope_id)
            }
            (Type::Sum(_exp_sum), Type::Sum(_act_sum)) => {
                // FIXME: We'll probably need a structural treatment for sums eventually
                Err(k1_format_user!(self, "expected sum {} but got sum {}", expected, actual))
            }
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.logical_params().len() != f2.logical_params().len() {
                    return Err(k1_format_user!(
                        self,
                        "Wrong parameter count: expected {} but got {}",
                        f1.logical_params().len(),
                        f2.logical_params().len()
                    ));
                }
                if let Err(msg) = self.check_types(f1.return_type, f2.return_type, scope_id) {
                    Err(k1_format_user!(
                        self,
                        "Wrong return type: expected {} but got {}: {}",
                        expected,
                        actual,
                        msg
                    ))
                } else {
                    for (p1, p2) in self
                        .mem
                        .getn(f1.logical_params())
                        .iter()
                        .zip(self.mem.getn(f2.logical_params()).iter())
                    {
                        if let Err(msg) = self.check_types(p1.type_id, p2.type_id, scope_id) {
                            return Err(k1_format_user!(
                                self,
                                "Incorrect type for parameter '{}': {}",
                                p1.name,
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
            (Type::Lambda(expected_lambda_id), Type::Lambda(actual_lambda_id)) => {
                let expected_lambda = self.lambda_types.get(*expected_lambda_id);
                let actual_lambda = self.lambda_types.get(*actual_lambda_id);
                if expected_lambda.parsed_id == actual_lambda.parsed_id
                    && expected_lambda.function_type == actual_lambda.function_type
                {
                    Ok(())
                } else {
                    Err(
            "Expected a unique lambda, but got a different one. This probably shouldn't happen"
              .into(),
          )
                }
            }
            (Type::LambdaObject(_lambda_object), Type::Lambda(_lambda_type)) => {
                Err(k1_format_user!(
                    self,
                    "expected lambda object but got lambda; need to call toDyn() for now. {} vs {}",
                    expected,
                    actual,
                ))
            }
            (Type::LambdaObject(exp_lambda_object), Type::LambdaObject(act_lambda_object)) => self
                .check_types(
                    exp_lambda_object.function_type,
                    act_lambda_object.function_type,
                    scope_id,
                ),
            (Type::StaticValue(exp_value_type), Type::StaticValue(act_value_type)) => {
                if exp_value_type.family_type_id == act_value_type.family_type_id {
                    match (exp_value_type.value_id, act_value_type.value_id) {
                        (None, None) => Ok(()),    // Both unresolved
                        (None, Some(_)) => Ok(()), // Expected unresolved, actual has a value
                        (Some(exp_value_id), Some(act_value_id)) => {
                            if exp_value_id == act_value_id {
                                Ok(())
                            } else {
                                Err(k1_format_user!(
                                    self,
                                    "Different static values of same type family: {} vs {}",
                                    exp_value_id,
                                    act_value_id
                                ))
                            }
                        }
                        (Some(_), None) => Err(k1_format_user!(
                            self,
                            "Expected a specific value but got a general family: {} vs {}",
                            expected,
                            actual,
                        )),
                    }
                } else {
                    Err(k1_format_user!(
                        self,
                        "Expected value type {} but got value type {}",
                        expected,
                        actual
                    ))
                }
            }
            (Type::FunctionTypeParameter(expected_abstract_function), act) => {
                let expected_function_type = expected_abstract_function.function_type;
                let actual_function_type = self.extract_function_type_from_functionlike(act);
                if let Some(actual_function_type) = actual_function_type {
                    self.check_types(expected_function_type, actual_function_type, scope_id)
                } else {
                    Err(k1_format_user!(
                        self,
                        "Expected some function-like with type: {} but got {}",
                        expected_function_type,
                        actual,
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
                    Err(k1_format_user!(self, "Arrays have different element types: {msg}"))
                } else if let Err(msg) = size_check {
                    Err(k1_format_user!(self, "Arrays have different size types: {msg}"))
                } else {
                    Ok(())
                }
            }
            (Type::Vector(expected_vector), Type::Vector(actual_vector)) => {
                let elem_check = self.check_types(
                    expected_vector.element_type,
                    actual_vector.element_type,
                    scope_id,
                );
                let size_check =
                    self.check_types(expected_vector.size_type, actual_vector.size_type, scope_id);
                if let Err(msg) = elem_check {
                    Err(k1_format_user!(self, "Vectors have different element types: {msg}"))
                } else if let Err(msg) = size_check {
                    Err(k1_format_user!(self, "Vectors have different lane counts: {msg}"))
                } else {
                    Ok(())
                }
            }
            (_expected, Type::Never) => Ok(()),
            (Type::Generic(_), _) | (_, Type::Generic(_)) => {
                let generic =
                    if self.types.get(expected).as_generic().is_some() { expected } else { actual };
                Err(k1_format_user!(
                    self,
                    "Expected {} but got {}: {} is a generic; apply type arguments to name one of its types",
                    expected,
                    actual,
                    generic
                ))
            }
            (_exp, _act) => Err(k1_format_user!(self, "Expected {} but got {}", expected, actual,)),
        }
    }

    /// One-sided, incomplete shape compatibility: pattern-side type parameters are
    /// wildcards. `false` means check_types can never accept any substitution
    /// instance of `pattern` for `candidate`; `true` decides nothing.
    pub fn precheck_types(&self, pattern: TypeId, candidate: TypeId, depth: u32) -> bool {
        if pattern == candidate || depth == 0 {
            return true;
        }
        if let (Some(pat_info), Some(cand_info)) =
            (self.get_instance_info(pattern), self.get_instance_info(candidate))
        {
            if pat_info.generic_parent != cand_info.generic_parent {
                return false;
            }
            for (pat_arg, cand_arg) in self
                .get_type_slice(pat_info.type_args)
                .iter()
                .zip(self.get_type_slice(cand_info.type_args))
            {
                if !self.precheck_types(*pat_arg, *cand_arg, depth - 1) {
                    return false;
                }
            }
            return true;
        }
        match (self.types.get(pattern), self.types.get(candidate)) {
            (
                Type::TypeParameter(_)
                | Type::InferenceHole(_)
                | Type::FunctionTypeParameter(_)
                | Type::StaticValue(_),
                _,
            ) => true,
            (
                _,
                Type::TypeParameter(_)
                | Type::InferenceHole(_)
                | Type::StaticValue(_)
                | Type::Never,
            ) => true,
            (
                Type::Lambda(_)
                | Type::LambdaObject(_)
                | Type::AbilityObject(_)
                | Type::Opaque(_)
                | Type::Generic(_),
                _,
            )
            | (
                _,
                Type::Lambda(_)
                | Type::LambdaObject(_)
                | Type::AbilityObject(_)
                | Type::Opaque(_)
                | Type::Generic(_),
            ) => true,
            (Type::Reference(pat), Type::Reference(cand)) => {
                self.precheck_types(pat.inner_type, cand.inner_type, depth - 1)
            }
            (Type::Array(pat), Type::Array(cand)) => {
                self.precheck_types(pat.element_type, cand.element_type, depth - 1)
                    && self.precheck_types(pat.size_type, cand.size_type, depth - 1)
            }
            (Type::Vector(pat), Type::Vector(cand)) => {
                self.precheck_types(pat.element_type, cand.element_type, depth - 1)
                    && self.precheck_types(pat.size_type, cand.size_type, depth - 1)
            }
            (Type::FunctionPointer(pat), Type::FunctionPointer(cand)) => {
                self.precheck_types(pat.function_type_id, cand.function_type_id, depth - 1)
            }
            (Type::Function(pat), Type::Function(cand)) => {
                pat.logical_params().len() == cand.logical_params().len()
            }
            (Type::Struct(pat), Type::Struct(cand)) => pat.fields.len() == cand.fields.len(),
            (Type::Sum(_), Type::Sum(_)) => true,
            _ => false,
        }
    }

    fn add_function(&mut self, mut function: TypedFunction) -> FunctionId {
        let id = self.functions.next_id();
        if let Some(specialization_info) = &mut function.specialization_info {
            specialization_info.specialized_function_id = id;
            let info = *specialization_info;
            self.function_specializations
                .entry((info.parent_function, info.type_arguments, info.fnlike_type_arguments))
                .or_insert(id);
        }
        let is_concrete = self.is_function_concrete(&function);
        if function.compiler_debug() {
            eprintln!(
                "is_function_concrete={is_concrete} for {}",
                self.function_to_string(&function, false)
            );
        }
        function.flags.set(TypedFunctionFlags::Concrete, is_concrete);
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
        let handle = AbilityImplHandle {
            base_ability_id: ability_impl.base_ability_id,
            specialized_ability_id: ability_impl.ability_id,
            full_impl_id: id,
        };
        self.ability_impl_table
            .entry(ability_impl.self_type_id)
            .or_default()
            .push_grow(&mut self.mem, handle);
        self.ability_impl_table_by_ability
            .entry(TypeAbilityPair {
                self_type_id: ability_impl.self_type_id,
                base_ability_id: ability_impl.base_ability_id,
            })
            .or_default()
            .push_grow(&mut self.mem, handle);
        self.ability_impls.add(ability_impl);
        id
    }

    fn register_builtin_ability_impl_shell(
        &mut self,
        self_type_id: TypeId,
        base_ability_id: AbilityId,
        impl_arguments: TypeIdSlice,
        span: SpanId,
    ) -> AbilityImplHandle {
        let ability = self.abilities.get(base_ability_id);
        let base_scope_id = ability.scope_id;
        let ability_self_type = ability.self_type_id;
        let all_params = ability.parameters;
        let ability_args = ability.kind.arguments(self);
        let scope_id =
            self.scopes.add_child_scope(base_scope_id, ScopeType::AbilityImpl, ScopeOwnerId::None);

        // Add self
        let _ = self.scopes.add_type(scope_id, self.ast.idents.b.self_, self_type_id);
        let _ = self.scopes.add_type_substitution(scope_id, ability_self_type, self_type_id);

        // Add ability-side params
        for (parent_ability_param, ability_arg) in self
            .mem
            .getn(all_params)
            .iter()
            .filter(|p| p.is_ability_side_param())
            .zip(ability_args.iter())
        {
            let _ = self.scopes.add_type(scope_id, parent_ability_param.name, *ability_arg);
            let _ = self.scopes.add_type_substitution(
                scope_id,
                parent_ability_param.type_variable_id,
                *ability_arg,
            );
        }
        // Add impl params
        for (parent_impl_param, impl_arg) in self
            .mem
            .getn(all_params)
            .iter()
            .filter(|p| p.is_impl_param)
            .zip(self.mem.getn(impl_arguments).iter())
        {
            let _ = self.scopes.add_type(scope_id, parent_impl_param.name, *impl_arg);
            let _ = self.scopes.add_type_substitution(
                scope_id,
                parent_impl_param.type_variable_id,
                *impl_arg,
            );
        }

        let impl_id = self.add_ability_impl(TypedAbilityImpl {
            kind: AbilityImplKind::BuiltinDerived,
            blanket_type_params: MSlice::empty(),
            self_type_id,
            base_ability_id,
            ability_id: base_ability_id,
            impl_arguments,
            functions: MSlice::empty(),
            scope_id,
            span,
            compile_errors: MList::empty(),
        });
        AbilityImplHandle {
            base_ability_id,
            specialized_ability_id: base_ability_id,
            full_impl_id: impl_id,
        }
    }

    fn declare_builtin_ability_impl_functions(&mut self, impl_id: AbilityImplId) -> K1Result<()> {
        let imp = self.ability_impls.get(impl_id);
        let base_ability_id = imp.base_ability_id;
        let self_type_id = imp.self_type_id;
        let scope_id = imp.scope_id;
        let functions = self.abilities.get(base_ability_id).functions;

        let mut impl_functions = self.mem.new_list(functions.len());
        for ability_fn_ref in self.mem.getn(functions) {
            let ability_fn = self.functions.get(ability_fn_ref.function_id);
            let spec_fn_id = self
                .declare_function(
                    ability_fn.parsed_id.as_function_id().unwrap(),
                    scope_id,
                    Some(FunctionAbilityContextInfo::ability_impl(
                        base_ability_id,
                        self_type_id,
                        AbilityImplKind::BuiltinDerived,
                        None,
                        false,
                    )),
                    // Why root namespace?! Answer: the namespace is only used for companion type stuff, so
                    // this isn't doing any harm
                    ROOT_NAMESPACE_ID,
                )?
                .expect("an ability impl cannot be conditionally compiled");
            let impl_fn = AbilityImplFunction::FunctionId(spec_fn_id);
            impl_functions.push(impl_fn)
        }
        self.ability_impls.get_mut(impl_id).functions = impl_functions.to_slice();
        Ok(())
    }

    fn implement_ability_for_type_constraint(
        &mut self,
        implementor_self_type_id: TypeId,
        impl_signature: TypedAbilitySignature,
        scope_id: ScopeId,
        span: SpanId,
    ) -> AbilityImplId {
        let ability = self.abilities.get(impl_signature.specialized_ability_id);
        let ability_functions = ability.functions;
        let base_ability_id = ability.base_ability_id;
        let ability_self_type = ability.self_type_id;
        let all_params = self.abilities.get(base_ability_id).parameters;
        let ability_args = ability.kind.arguments(self);
        let mut subst_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        // Add self
        subst_pairs.push(spair! {ability.self_type_id => implementor_self_type_id});
        let _ = self.scopes.add_type_substitution(
            scope_id,
            ability_self_type,
            implementor_self_type_id,
        );

        // Add ability-side params
        for (parent_ability_param, ability_arg) in self
            .mem
            .getn(all_params)
            .iter()
            .filter(|p| p.is_ability_side_param())
            .zip(ability_args.iter())
        {
            subst_pairs.push(spair! {parent_ability_param.type_variable_id => *ability_arg});
            let _ = self.scopes.add_type_substitution(
                scope_id,
                parent_ability_param.type_variable_id,
                *ability_arg,
            );
        }
        // Add impl params
        for (parent_impl_param, impl_arg) in self
            .mem
            .getn(all_params)
            .iter()
            .filter(|p| p.is_impl_param)
            .zip(self.mem.getn(impl_signature.impl_arguments).iter())
        {
            subst_pairs.push(spair! {parent_impl_param.type_variable_id => *impl_arg});
            let _ = self.scopes.add_type_substitution(
                scope_id,
                parent_impl_param.type_variable_id,
                *impl_arg,
            );
        }

        let functions = self.abilities.get(impl_signature.specialized_ability_id).functions;
        let mut impl_functions = self.mem.new_list(functions.len());
        for f in self.mem.getn(ability_functions) {
            let generic_fn = self.get_function(f.function_id);
            let generic_sig = generic_fn.signature();
            let generic_fn_type_id = generic_fn.type_id;
            let specialized_function_type =
                self.substitute_in_type(generic_fn_type_id, &subst_pairs);

            // We have to directly remove 'Self' from the type parameters of the signature
            // since it's the only one of the ability params that gets 'encoded' as a type
            // parameter to the function
            let type_params_minus_self = self.mem.pushn_iter(
                self.mem
                    .getn(generic_sig.type_params)
                    .iter()
                    .filter(|tp| **tp != ability_self_type)
                    .copied(),
            );
            debug_assert_eq!(type_params_minus_self.len() + 1, generic_sig.type_params.len());
            let specialized_signature = FunctionSignature {
                function_type: specialized_function_type,
                type_params: type_params_minus_self,
                ..generic_sig
            };
            debug!(
                "specialized constraint ability function signature {}: {}",
                self.function_signature_to_string(&generic_sig),
                self.function_signature_to_string(&specialized_signature),
            );
            let impl_fn = AbilityImplFunction::Abstract(specialized_signature);
            impl_functions.push(impl_fn)
        }
        self.add_ability_impl(TypedAbilityImpl {
            kind: AbilityImplKind::TypeParamConstraint,
            blanket_type_params: MSlice::empty(),
            self_type_id: implementor_self_type_id,
            base_ability_id,
            ability_id: impl_signature.specialized_ability_id,
            impl_arguments: impl_signature.impl_arguments,
            functions: impl_functions.to_slice(),
            scope_id,
            span,
            compile_errors: MList::empty(),
        })
    }

    fn add_type_parameter(
        &mut self,
        type_parameter: TypeParameter,
        ability_impl_signatures: SV4<TypedAbilitySignature>,
    ) -> TypeId {
        let type_id = self.add_type_anon(Type::TypeParameter(type_parameter));
        for ability_sig in ability_impl_signatures.into_iter() {
            let constrained_impl_scope = self.scopes.add_child_scope(
                type_parameter.scope_id,
                ScopeType::AbilityImpl,
                ScopeOwnerId::None,
            );
            let _ = self.scopes.add_type(constrained_impl_scope, type_parameter.name, type_id);
            self.implement_ability_for_type_constraint(
                type_id,
                ability_sig,
                constrained_impl_scope,
                type_parameter.span,
            );
        }
        type_id
    }

    fn add_function_type_parameter(&mut self, value: FunctionTypeParameter) -> TypeId {
        let type_id = self.add_type_anon(Type::FunctionTypeParameter(value));
        type_id
    }

    pub fn get_constrained_ability_impls_for_type(
        &self,
        type_id: TypeId,
    ) -> SV4<AbilityImplHandle> {
        match self.ability_impl_table.get(&type_id) {
            None => smallvec![],
            Some(v) => {
                let mut handles: SV4<AbilityImplHandle> = smallvec![];
                for handle in v.as_slice(&self.mem) {
                    if self.ability_impls.get(handle.full_impl_id).kind.is_type_param_constraint() {
                        handles.push(*handle);
                    }
                }
                handles
            }
        }
    }

    /// resolution works on the base ability
    pub fn find_or_generate_ability_impl_for_type(
        &mut self,
        self_type_id: TypeId,
        target_base_ability_id: AbilityId,

        // If this is happening as part of inference, then it may be the
        // case that not just any implementation will do; but only one
        // that meets certain constraints. For example, we may need into[string], not just into[<whatever>]
        // For each ability parameter, the type it must conform to,
        // or None for if we didn't solve for it, meaning anything is fine
        parameter_constraints: &[Option<TypeId>],

        allow_self_adjust: bool,

        scope_id: ScopeId,
        span: SpanId,
    ) -> Result<(AbilityImplHandle, SelfAdjust), MStr<MemTmp>> {
        // let mut attempts: SV4<String> = smallvec![];
        if let Some(impl_handle) = self.find_unique_valid_ability_impl(
            self_type_id,
            target_base_ability_id,
            parameter_constraints,
            scope_id,
        )? {
            return Ok((impl_handle, SelfAdjust::None));
        }

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
            for blanket_impl_id in blanket_impls_for_base.as_slice(&self.mem).iter().copied() {
                match self.try_apply_blanket_implementation(
                    blanket_impl_id,
                    self_type_id,
                    target_base_ability_id,
                    parameter_constraints,
                    span,
                ) {
                    None => debug!("Blanket impl didn't work"),
                    Some(impl_handle) => return Ok((impl_handle, SelfAdjust::None)),
                }
            }
        };

        let mut err_msg: Option<MStr<MemTmp>> = None;
        /////////////////// Special type-kind abilities
        if target_base_ability_id == ABILITY_ID_ENUM {
            if let Type::Enum(e) = self.types.get(self_type_id) {
                // ability enum[impl v]
                // Assign 'v' to the integer type of the enum!
                let impl_arguments = self.mem.pushn(&[e.int_type.type_id()]);
                let impl_handle = self.generate_builtin_ability_impl(
                    self_type_id,
                    target_base_ability_id,
                    impl_arguments,
                    span,
                );
                return Ok((impl_handle, SelfAdjust::None));
            }
        }
        if target_base_ability_id == ABILITY_ID_SUM {
            if let Type::Sum(sum) = self.types.get(self_type_id) {
                // ability sum[impl v]
                // Assign 'v' to the integer type of the sum!
                let impl_arguments = self.mem.pushn(&[sum.tag_type.type_id()]);
                let impl_handle = self.generate_builtin_ability_impl(
                    self_type_id,
                    target_base_ability_id,
                    impl_arguments,
                    span,
                );
                // eprintln!("\n----------------- IMPLEMENTED SUM\n");
                // eprintln!("{}", self.ability_impl_to_string(impl_id, true));
                return Ok((impl_handle, SelfAdjust::None));
            }
        }
        if target_base_ability_id == ABILITY_ID_EQUALS {
            match self.types.get(self_type_id) {
                Type::Enum(_) => {
                    let impl_handle = self.generate_builtin_ability_impl(
                        self_type_id,
                        target_base_ability_id,
                        MSlice::empty(),
                        span,
                    );
                    return Ok((impl_handle, SelfAdjust::None));
                }
                Type::Sum(_) | Type::Struct(_) => {
                    match self.generate_builtin_member_wise_impl(
                        self_type_id,
                        target_base_ability_id,
                        scope_id,
                        span,
                    ) {
                        Ok(impl_handle) => return Ok((impl_handle, SelfAdjust::None)),
                        Err(msg) => err_msg = Some(msg),
                    }
                }
                _ => {}
            }
        }

        if target_base_ability_id == ABILITY_ID_PRINT {
            // No derived print for `code`: printing it as text would drop its
            // source spans silently
            let is_code = Some(self_type_id) == self.builtin_types.code;
            if is_code {
                err_msg = Some(k1_format_user!(
                    self,
                    "`code` does not implement print; use .text for the text alone, or write to a code-builder to keep source spans",
                ));
            } else {
                match self.types.get(self_type_id) {
                    Type::Enum(_) => {
                        let impl_handle = self.generate_builtin_ability_impl(
                            self_type_id,
                            target_base_ability_id,
                            MSlice::empty(),
                            span,
                        );
                        return Ok((impl_handle, SelfAdjust::None));
                    }
                    Type::Sum(_) | Type::Struct(_) => {
                        match self.generate_builtin_member_wise_impl(
                            self_type_id,
                            target_base_ability_id,
                            scope_id,
                            span,
                        ) {
                            Ok(impl_handle) => return Ok((impl_handle, SelfAdjust::None)),
                            Err(msg) => err_msg = Some(msg),
                        }
                    }
                    _ => {}
                }
            }
        }

        // The two lanes are exclusive by shape: a reference self can only be
        // dereferenced, a value self can only have its address taken. Both are
        // opt-in, because both are only sound where the caller has a receiver
        // expression to adjust. Satisfying a bare type-param constraint this way
        // is not: `*u64` would answer for `u64`'s equals/add/zero, and then
        // `sum` specializes to `list[*u64]` and cannot produce a `*u64`
        let adjusted_self = if !allow_self_adjust {
            None
        } else {
            match self.types.get(self_type_id).as_reference() {
                Some(reference) => Some((reference.inner_type, SelfAdjust::Deref)),
                None => Some((self.add_reference_type(self_type_id), SelfAdjust::AddrOf)),
            }
        };
        if let Some((adjusted_self_type_id, adjust)) = adjusted_self {
            if let Some(impl_handle) = self.find_unique_valid_ability_impl(
                adjusted_self_type_id,
                target_base_ability_id,
                parameter_constraints,
                scope_id,
            )? {
                return Ok((impl_handle, adjust));
            }
            if let Some(blanket_impls_for_base) = self.blanket_impls.get(&target_base_ability_id) {
                for blanket_impl_id in blanket_impls_for_base.as_slice(&self.mem).iter().copied() {
                    if let Some(impl_handle) = self.try_apply_blanket_implementation(
                        blanket_impl_id,
                        adjusted_self_type_id,
                        target_base_ability_id,
                        parameter_constraints,
                        span,
                    ) {
                        return Ok((impl_handle, adjust));
                    }
                }
            }
        }

        match err_msg {
            None => Err("No matching implementations found".into()),
            Some(msg) => Err(msg),
        }
    }

    fn find_unique_valid_ability_impl(
        &self,
        self_type_id: TypeId,
        target_base_ability_id: AbilityId,
        parameter_constraints: &[Option<TypeId>],
        scope_id: ScopeId,
    ) -> Result<Option<AbilityImplHandle>, MStr<MemTmp>> {
        let Some(impl_handles) = self
            .ability_impl_table_by_ability
            .get(&TypeAbilityPair { self_type_id, base_ability_id: target_base_ability_id })
        else {
            return Ok(None);
        };
        let impl_handles = impl_handles.as_slice(&self.mem);
        debug!(
            "Ability dump for {} {:02} in search of {} {:02}\n{}",
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
                        self.pretty_print_type_slice(
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
            0 => Ok(None),
            1 => Ok(Some(valid_impls[0])),
            _ => {
                // If any of the parameter constraints have holes, then we don't require that we
                // have a unique implementation because the holes will make it such
                // that multiple implementations can match
                let has_holes = parameter_constraints.iter().any(|c| c.is_none());
                if has_holes {
                    Ok(Some(valid_impls[0]))
                } else {
                    let mut impls_formatted = String::new();
                    for (idx, i) in valid_impls.iter().enumerate() {
                        if idx > 0 {
                            impls_formatted.push('\n');
                        }
                        let imp = self.ability_impls.get(i.full_impl_id);
                        write!(
                            impls_formatted,
                            "- IMPL {:02} {:?} {}",
                            i.full_impl_id.0,
                            imp.kind,
                            self.ability_signature_to_string(imp.signature())
                        )
                        .unwrap();
                    }
                    let mut constraints_formatted = String::new();
                    for (idx, maybe_type) in parameter_constraints.iter().enumerate() {
                        if idx > 0 {
                            constraints_formatted.push_str(", ");
                        }
                        match maybe_type {
                            Some(t) => constraints_formatted.push_str(&self.type_id_to_string(*t)),
                            None => constraints_formatted.push('_'),
                        }
                    }
                    eprintln!(
                        "Multiple matching implementations found for constraints {}:\n{}",
                        constraints_formatted, impls_formatted
                    );
                    Err(k1_format_user!(
                        self,
                        "Multiple matching implementations found:\n{}",
                        impls_formatted
                    ))
                }
            }
        }
    }

    fn generate_builtin_ability_impl(
        &mut self,
        self_type_id: TypeId,
        base_ability_id: AbilityId,
        impl_arguments: TypeIdSlice,
        span: SpanId,
    ) -> AbilityImplHandle {
        let handle = self.register_builtin_ability_impl_shell(
            self_type_id,
            base_ability_id,
            impl_arguments,
            span,
        );
        if let Err(e) = self.declare_builtin_ability_impl_functions(handle.full_impl_id) {
            ice_span!(
                self,
                span,
                "Failed while generating builtin ability impl: {}",
                self.ident_str(e.message)
            )
        }
        debug!(
            "\n----------------- IMPLEMENTED BUILTIN {}\n",
            self.ident_str(self.abilities.get(base_ability_id).name)
        );
        debug!("{}", self.ability_impl_to_string(handle.full_impl_id, true));
        handle
    }

    fn generate_builtin_member_wise_impl(
        &mut self,
        self_type_id: TypeId,
        target_base_ability_id: AbilityId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> Result<AbilityImplHandle, MStr<MemTmp>> {
        let first_impl_id = self.ability_impls.next_id();
        let impl_handle = self.register_builtin_ability_impl_shell(
            self_type_id,
            target_base_ability_id,
            MSlice::empty(),
            span,
        );
        let ability_name = self.abilities.get(target_base_ability_id).name;
        let mut err_msg: Option<MStr<MemTmp>> = None;
        match self.types.get(self_type_id) {
            Type::Struct(struct_type) => {
                for field in self.mem.getn(struct_type.fields) {
                    if self
                        .expect_ability_impl(
                            field.type_id,
                            target_base_ability_id,
                            false,
                            scope_id,
                            span,
                        )
                        .is_err()
                    {
                        err_msg = Some(k1_format_user!(
                            self,
                            "field {} type {} does not implement {}",
                            field.name,
                            field.type_id,
                            ability_name,
                        ))
                    }
                }
            }
            Type::Sum(sum_type) => {
                for variant in self.mem.getn(sum_type.variants) {
                    if let Some(payload) = variant.payload {
                        if self
                            .expect_ability_impl(
                                payload,
                                target_base_ability_id,
                                false,
                                scope_id,
                                span,
                            )
                            .is_err()
                        {
                            err_msg = Some(k1_format_user!(
                                self,
                                ":{} variant's data {} does not implement {}",
                                variant.name,
                                payload,
                                ability_name,
                            ))
                        }
                    }
                }
            }
            _ => unreachable!("member-wise derive is only for structs and sums"),
        }
        match err_msg {
            None => {
                if let Err(e) =
                    self.declare_builtin_ability_impl_functions(impl_handle.full_impl_id)
                {
                    ice_span!(
                        self,
                        span,
                        "Failed while generating builtin ability impl: {}",
                        self.ident_str(e.message)
                    )
                }
                Ok(impl_handle)
            }
            Some(msg) => {
                self.unregister_ability_impls_from(first_impl_id);
                Err(msg)
            }
        }
    }

    fn unregister_ability_impls_from(&mut self, first_impl_id: AbilityImplId) {
        let end = self.ability_impls.next_id();
        let mut swept_fns: SV8<FunctionId> = smallvec![];
        let mut impl_id = first_impl_id;
        while impl_id != end {
            let imp = self.ability_impls.get(impl_id);
            let self_type_id = imp.self_type_id;
            let base_ability_id = imp.base_ability_id;
            let functions = imp.functions;
            let handle = AbilityImplHandle {
                base_ability_id,
                specialized_ability_id: imp.ability_id,
                full_impl_id: impl_id,
            };
            if let Some(impls) = self.ability_impl_table.get_mut(&self_type_id) {
                impls.swap_remove_elem(&self.mem, &handle);
            }
            if let Some(impls) = self
                .ability_impl_table_by_ability
                .get_mut(&TypeAbilityPair { self_type_id, base_ability_id })
            {
                impls.swap_remove_elem(&self.mem, &handle);
            }
            for f in self.mem.getn(functions) {
                if let AbilityImplFunction::FunctionId(fid) = f {
                    swept_fns.push(*fid);
                }
            }
            impl_id = impl_id.add_u32(1);
        }
        if !swept_fns.is_empty() {
            self.functions_pending_body_specialization.retain(|f| !swept_fns.contains(f));
        }
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
        let base_params = self.abilities.get(specialized_ability.base_ability_id).parameters;
        let mut ability_params =
            self.mem.getn(base_params).iter().filter(|p| p.is_ability_side_param());
        for (impl_arg, maybe_constraint) in
            specialized_ability.kind.arguments(self).iter().zip(parameter_requirements.iter())
        {
            let param = ability_params.next();
            if let Some(constraint) = maybe_constraint {
                if let Err(msg) = self.check_types(*constraint, *impl_arg, scope_id) {
                    return Err(Cow::Owned(format!(
                        "Implementation has {} = {}, but context requires it to be {}: {msg}",
                        param.map(|p| self.ident_str(p.name)).unwrap_or("?"),
                        self.type_id_to_string(*impl_arg),
                        self.type_id_to_string(*constraint),
                    )));
                }
            }
        }
        Ok(())
    }

    fn impl_arg_named(
        &self,
        base_ability_id: AbilityId,
        impl_arguments: TypeIdSlice,
        name: StringId,
    ) -> Option<TypeId> {
        let params = self.abilities.get(base_ability_id).parameters;
        let mut index = 0;
        for p in self.mem.getn(params) {
            if p.is_impl_param {
                if p.name == name {
                    return Some(*self.mem.get_nth(impl_arguments, index));
                }
                index += 1;
            }
        }
        None
    }

    pub fn find_or_generate_specialized_ability_impl_for_type(
        &mut self,
        self_type_id: TypeId,
        target_specialized_ability_id: AbilityId,
        allow_self_adjust: bool,
        scope_id: ScopeId,
        span: SpanId,
    ) -> Result<(AbilityImplHandle, SelfAdjust), MStr<MemTmp>> {
        let specialized_ability = self.abilities.get(target_specialized_ability_id);
        let base_ability = specialized_ability.base_ability_id;
        let args = specialized_ability.kind.arguments(self);
        let mut parameter_constraints: SV4<Option<TypeId>> = smallvec![];
        for arg in args {
            parameter_constraints.push(Some(*arg));
        }

        self.find_or_generate_ability_impl_for_type(
            self_type_id,
            base_ability,
            &parameter_constraints,
            allow_self_adjust,
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

        if !self.precheck_types(blanket_impl_self_type_id, self_type_id, 4) {
            debug!("Blanket impl self pattern shape cannot match");
            return None;
        }

        let blanket_arguments = blanket_ability.kind.arguments(self);

        debug!(
            "Trying blanket impl {} with blanket arguments {}, impl arguments {}",
            self.ident_str(blanket_ability.name),
            self.pretty_print_types(blanket_arguments, ", "),
            self.pretty_print_type_slice(blanket_impl.impl_arguments, ", "),
        );

        if blanket_arguments.len() != target_ability_args.len() {
            debug!("Wrong arg count {} vs {}", blanket_arguments.len(), target_ability_args.len());
            return None;
        }

        // Reborrows
        let blanket_ability = self.abilities.get(blanket_impl_ability_id);
        let blanket_arguments = blanket_ability.kind.arguments(self);

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
        for (arg_to_blanket, arg_to_target) in blanket_arguments.iter().zip(target_ability_args) {
            match arg_to_target {
                None => {
                    eprintln!("No arg for ability param; probably can't solve it")
                }
                Some(arg_to_target) => args_and_params.push(InferenceInputPair {
                    arg: TypeOrParsedExpr::Type(*arg_to_target),
                    param_type: *arg_to_blanket,
                    allow_mismatch: true,
                }),
            };
        }

        let blanket_impl_type_params_handle =
            self.ability_impls.get(blanket_impl_id).blanket_type_params;
        let root_scope_id = self.scopes.root_scope_id();
        let blanket_impl_type_params = self.mem.getn(blanket_impl_type_params_handle);

        let solutions_result = self.with_clean_inference(|k1| {
            k1.infer_types(
                blanket_impl_type_params,
                blanket_impl_type_params_handle,
                &args_and_params,
                span,
                root_scope_id,
                None,
            )
        });
        let (solutions, _all_solutions) = match solutions_result {
            Err(_) => {
                debug!("Could not solve all blanket impl params");
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
            self.zip_types_to_subst_pairs(blanket_impl_type_params, solutions.as_slice(&self.mem));
        let substituted_self =
            self.substitute_in_type(blanket_impl_self_type_id, &solutions_as_pairs);
        if let Err(msg) = self.check_types(substituted_self, self_type_id, root_scope_id) {
            debug!("blanket impl self type does not accept candidate: {msg}");
            return None;
        }
        for (blanket_arg, required_arg) in blanket_arguments.iter().zip(target_ability_args) {
            if let Some(required_arg) = required_arg {
                let actual_value = self.substitute_in_type(*blanket_arg, &solutions_as_pairs);
                if let Err(msg) = self.check_types(*required_arg, actual_value, root_scope_id) {
                    debug!(
                        "blanket impl, if applied, would result in the wrong type for param {}. {}",
                        self.type_id_to_string(*blanket_arg),
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
            ScopeOwnerId::None,
        );
        let parsed_blanket_impl = self.ast.get_ability_impl(parsed_id);

        for ((typed_param, parsed_param), solution) in self
            .mem
            .getn(blanket_impl_type_params_handle)
            .iter()
            .zip(self.ast.mem.getn(parsed_blanket_impl.generic_impl_params))
            .zip(solutions.as_slice(&self.mem).iter())
        {
            let _ = self.scopes.add_type(constraint_checking_scope, parsed_param.name, *solution);
            let _ = self.scopes.add_type_substitution(
                constraint_checking_scope,
                *typed_param,
                *solution,
            );
            let tp = self.get_type_parameter(*typed_param);
            if let Some(static_constraint) = tp.static_constraint {
                let static_type = self.types.get(static_constraint).as_value_type().unwrap();
                let matched = match self.types.get(*solution) {
                    Type::StaticValue(s) => static_type.family_type_id == s.family_type_id,
                    _non_static => static_type.family_type_id == *solution,
                };
                if !matched {
                    self.report_hint(
                        span,
                        format!(
                            "Blanket impl almost matched but a static constraint failed: {} != {}",
                            self.type_id_to_string(static_type.family_type_id),
                            self.type_id_to_string(*solution)
                        ),
                    );
                    return None;
                }
            }
            for parsed_constraint in self.ast.mem.getn(parsed_param.constraints) {
                let Some(parsed_ability_expr) = parsed_constraint.as_ability() else { continue };
                let constraint_signature = self
                    .eval_ability_expr(parsed_ability_expr, false, constraint_checking_scope)
                    .unwrap();
                if let Err(e) = self.check_ability_constraint(
                    *solution,
                    constraint_signature,
                    parsed_param.name,
                    constraint_checking_scope,
                    span,
                ) {
                    self.report_hint(
                        e.span,
                        format!(
                            "Blanket impl almost matched but a constraint was unsatisfied; {}",
                            self.ident_str(e.message)
                        ),
                    );
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
        solutions: TypeArgs,
    ) -> K1Result<AbilityImplHandle> {
        let blanket_impl = self.ability_impls.get(blanket_impl_id).clone();

        let generic_base_ability_id = blanket_impl.kind.blanket_parent().unwrap();
        debug!(
            "instantiate_blanket_impl: impl {} for {} with {}",
            self.ident_str(self.abilities.get(generic_base_ability_id).name),
            self.type_id_to_string(self_type_id),
            self.pretty_print_types(solutions.as_slice(&self.mem), ", ")
        );

        let new_impl_scope = self.scopes.add_sibling_scope(
            blanket_impl.scope_id,
            ScopeType::AbilityImpl,
            ScopeOwnerId::None,
        );

        let mut pairs: SV4<TypeSubstitutionPair> = smallvec![];
        for (index, param) in self.mem.getn(blanket_impl.blanket_type_params).iter().enumerate() {
            let solution = solutions.as_slice(&self.mem)[index];
            let param_name = self.get_type_parameter(*param).name;
            let _ = self.scopes.add_type(new_impl_scope, param_name, solution);
            let _ = self.scopes.add_type_substitution(new_impl_scope, *param, solution);
            pairs.push(TypeSubstitutionPair { from: *param, to: solution });
        }

        let blanket_ability_args = self.abilities.get(blanket_impl.ability_id).kind.arguments(self);
        let base_ability_params = self.abilities.get(generic_base_ability_id).parameters;
        let mut substituted_ability_args: List<TypeId, MemTmp> =
            self.tmp.new_list(blanket_ability_args.len() as u32);
        for (blanket_arg, base_param) in blanket_ability_args
            .iter()
            .zip(self.mem.getn(base_ability_params).iter().filter(|p| p.is_ability_side_param()))
        {
            // Substitute T, U, V, in for each
            let substituted_type = self.substitute_in_type(*blanket_arg, &pairs);
            substituted_ability_args.push(substituted_type);
            // Blanket param bindings added above shadow ability param names
            let _ = self.scopes.add_type(new_impl_scope, base_param.name, substituted_type);
            let _ = self.scopes.add_type_substitution(
                new_impl_scope,
                base_param.type_variable_id,
                substituted_type,
            );
        }
        let substituted_ability_args_handle =
            self.intern_type_slice(substituted_ability_args.as_slice());
        let concrete_ability_id = self.specialize_ability(
            generic_base_ability_id,
            substituted_ability_args_handle,
            blanket_impl.span,
            blanket_impl.scope_id,
        );

        let mut substituted_impl_arguments: List<TypeId, _> =
            self.mem.new_list(blanket_impl.impl_arguments.len());
        let target_ability_impl_params = self.abilities.get(blanket_impl.ability_id).parameters;
        for (blanket_impl_arg, impl_param) in self
            .mem
            .getn(blanket_impl.impl_arguments)
            .iter()
            .zip(self.mem.getn(target_ability_impl_params).iter().filter(|p| p.is_impl_param))
        {
            // Substitute T, U, V, in for each
            let substituted_type = self.substitute_in_type(*blanket_impl_arg, &pairs);
            substituted_impl_arguments.push(substituted_type);
            let _ = self.scopes.add_type(new_impl_scope, impl_param.name, substituted_type);
            let _ = self.scopes.add_type_substitution(
                new_impl_scope,
                impl_param.type_variable_id,
                substituted_type,
            );
        }

        let _ = self.scopes.add_type(new_impl_scope, self.ast.idents.b.self_, self_type_id);
        let concrete_ability_self = self.abilities.get(concrete_ability_id).self_type_id;
        let _ =
            self.scopes.add_type_substitution(new_impl_scope, concrete_ability_self, self_type_id);

        let substituted_impl_arguments_handle = substituted_impl_arguments.to_slice();
        let mut specialized_functions = self.mem.new_list(blanket_impl.functions.len());
        let kind = AbilityImplKind::DerivedFromBlanket { blanket_impl_id };
        debug!(
            "blanket impl instance scope before function specialization: {}",
            self.scope_id_to_string(new_impl_scope)
        );
        for (index, blanket_impl_function) in
            self.mem.getn(blanket_impl.functions).iter().enumerate()
        {
            // If the functions are abstract, just the type ids
            // If concrete do the declaration thing
            //
            let specialized_function = match *blanket_impl_function {
                AbilityImplFunction::FunctionId(blanket_impl_function_id) => {
                    let blanket_fn = self.get_function(blanket_impl_function_id);
                    let parsed_fn = blanket_fn.parsed_id.as_function_id().unwrap();
                    let decl_fn =
                        *self.mem.get_nth(self.abilities.get(concrete_ability_id).functions, index);
                    let is_default =
                        self.get_function(decl_fn.function_id).parsed_id.as_function_id()
                            == Some(parsed_fn);
                    if is_default
                        && self
                            .check_ability_fn_where_constraints(
                                concrete_ability_id,
                                substituted_impl_arguments_handle,
                                self_type_id,
                                index as u32,
                                new_impl_scope,
                                blanket_impl.span,
                            )
                            .is_err()
                    {
                        AbilityImplFunction::Unavailable
                    } else {
                        let specialized_function_id = self
                            .declare_function(
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
                }
                AbilityImplFunction::Unavailable => AbilityImplFunction::Unavailable,
                AbilityImplFunction::Abstract(_) => {
                    ice_span!(
                        self,
                        blanket_impl.span,
                        "encountered abstract ability impl function in instantiate blanket impl"
                    );
                }
            };
            specialized_functions.push(specialized_function);
        }

        let id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: MSlice::empty(),
            self_type_id,
            ability_id: concrete_ability_id,
            base_ability_id: generic_base_ability_id,
            impl_arguments: substituted_impl_arguments_handle,
            functions: specialized_functions.to_slice(),
            scope_id: new_impl_scope,
            span: blanket_impl.span,
            compile_errors: MList::empty(),
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
    ) -> K1Result<StaticValueId> {
        let parsed_text = self.ast.get_span_content(span);
        let is_float = parsed_text.contains('.');
        if is_float {
            let float_value = self.eval_float_value(span, ctx.expected_type_id)?;
            let value_id = self.static_values.add(StaticValue::Float(float_value));
            Ok(value_id)
        } else {
            let int_value = self.eval_integer_value(span, ctx.expected_type_id)?;
            let value_id = self.static_values.add(StaticValue::Int(int_value));
            Ok(value_id)
        }
    }

    fn eval_float_value(
        &self,
        span: SpanId,
        expected_type_id: Option<TypeId>,
    ) -> K1Result<TypedFloatValue> {
        let parsed_text = self.ast.get_span_content(span);
        let expected_width = match expected_type_id {
            None => NumericWidth::B64,
            Some(F64_TYPE_ID) => NumericWidth::B64,
            Some(F32_TYPE_ID) => NumericWidth::B32,
            Some(_) => {
                // Parse as f64 and let typechecking fail
                NumericWidth::B64
            }
        };
        let value: Result<TypedFloatValue, std::num::ParseFloatError> = match expected_width {
            NumericWidth::B32 => parsed_text.parse::<f32>().map(TypedFloatValue::F32),
            NumericWidth::B64 => parsed_text.parse::<f64>().map(TypedFloatValue::F64),
            _ => unreachable!("unreachable float width"),
        };
        let value =
            value.map_err(|e| kerr!(self, span, "Invalid f{}: {e}", expected_width.bits()))?;
        Ok(value)
    }

    fn eval_integer_value(
        &mut self,
        span: SpanId,
        expected_type_id: Option<TypeId>,
    ) -> K1Result<TypedIntValue> {
        let parsed_text = self.ast.get_span_content(span);

        let is_negative = parsed_text.starts_with('-');
        let num_text = if is_negative { &parsed_text[1..] } else { parsed_text };

        // The number ends at the first byte that can't be part of one; the rest is a suffix
        let mut suffix_start = num_text.len();
        for (idx, b) in num_text.bytes().enumerate() {
            if !(b.is_ascii_hexdigit() || b == b'_' || b == b'x') {
                suffix_start = idx;
                break;
            }
        }
        let suffix_result = match suffix_start == num_text.len() {
            true => None,
            false => {
                let (num, suffix) = num_text.split_at(suffix_start);
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
                        kbail!(
                            self,
                            span,
                            "Invalid integer suffix '{}'; expected u8, u16, u32, u64, uint, usize, i8, i16, i32, i64, int, size",
                            suffix
                        );
                    }
                };
                Some((int_type, num))
            }
        };

        let (suffix_int_type, num_text) = match suffix_result {
            None => (None, num_text),
            Some((int_type, num_text)) => (Some(int_type), num_text),
        };

        let (base, digits) = if let Some(d) = num_text.strip_prefix("0x") {
            (16, d)
        } else if let Some(d) = num_text.strip_prefix("0b") {
            (2, d)
        } else {
            (10, num_text)
        };

        let expected_int_type = match suffix_int_type {
            Some(int_type) => int_type,
            None => match expected_type_id.map(|t| self.types.get(t)) {
                Some(Type::Integer(int_type)) => *int_type,
                Some(_other) => {
                    // Here we're expecting some non-integer type.
                    // The best bet for a good compiler error is to parse as a large signed
                    // value and let typechecking fail
                    IntegerType::I64
                }
                None => IntegerType::I64,
            },
        };
        // from_str_radix takes a leading '-' but not '_' separators or 0x/0b prefixes, so
        // digit-only positive literals (and base-10 negatives, whose '-' is adjacent in the
        // source) parse straight from the source text; the rest get rebuilt in a scratch buffer
        let has_underscore = digits.as_bytes().contains(&b'_');
        let num_to_parse: &str = if !has_underscore && (!is_negative || base == 10) {
            if is_negative { &parsed_text[..1 + digits.len()] } else { digits }
        } else {
            self.buffers.int_parse.clear();
            if is_negative {
                self.buffers.int_parse.push('-');
            }
            for c in digits.chars() {
                if c != '_' {
                    self.buffers.int_parse.push(c);
                }
            }
            &self.buffers.int_parse
        };
        debug!("num_to_parse: {num_to_parse}, base: {base}, type: {expected_int_type}");
        macro_rules! parse_int {
            ($int_type:ident, $rust_int_type:ty, $base: expr) => {{
                let result = <$rust_int_type>::from_str_radix(num_to_parse, $base);
                result.map(|int| TypedIntValue::$int_type(int)).map_err(|e| {
                    self.make_error(
                        format!(
                            "Invalid {} {expected_int_type}: {num_to_parse}. {e}",
                            if base == 16 {
                                "hex"
                            } else if base == 10 {
                                "decimal"
                            } else {
                                "binary"
                            }
                        ),
                        span,
                    )
                })
            }};
        }
        match expected_int_type {
            IntegerType::U8 => parse_int!(U8, u8, base),
            IntegerType::U16 => parse_int!(U16, u16, base),
            IntegerType::U32 => parse_int!(U32, u32, base),
            IntegerType::U64 => parse_int!(U64, u64, base),
            IntegerType::I8 => parse_int!(I8, i8, base),
            IntegerType::I16 => parse_int!(I16, i16, base),
            IntegerType::I32 => parse_int!(I32, i32, base),
            IntegerType::I64 => parse_int!(I64, i64, base),
        }
    }

    fn eval_variable(
        &mut self,
        variable_expr_id: ParsedExprId,
        ctx: EvalExprContext,
        // Currently, only used to determine if this counts as a usage
        is_assignment_lhs: bool,
    ) -> K1Result<(Option<VariableId>, TypedExprId)> {
        let scope_id = ctx.scope_id;
        let ParsedExpr::Variable(variable) = self.ast.exprs.get(variable_expr_id) else { panic!() };
        let name = variable.name;
        let variable_name_span = name.name_span;

        if let Some(cs) = &self.completion
            && name.name == cs.marker
        {
            if cs.site.is_none() && !ctx.is_marker_owned_by_call() {
                let site = if name.path.is_empty() {
                    CompletionSite::Scope { scope_id }
                } else {
                    match self.resolve_qident(scope_id, &name) {
                        Ok(path_scope_id) => CompletionSite::Path { path_scope_id },
                        Err(_) => CompletionSite::Scope { scope_id },
                    }
                };
                self.completion.as_mut().unwrap().site = Some(site);
            }
            // The marker evaluates as a phony of the expected type so the
            // enclosing expression can finish typechecking
            let phony_type = ctx.expected_type_id.unwrap_or(NEVER_TYPE_ID);
            return Ok((None, self.synth_phony(phony_type, variable_name_span)));
        }

        let mut variable_id = self.find_variable_namespaced(scope_id, &name)?;
        if variable_id.is_none() {
            // A global not yet declared: force it, as early references
            // (e.g. #if conditions during declaration phases) precede the
            // declaration pass
            if let Some((parsed_id, defn_scope)) =
                self.find_pending_global_namespaced(scope_id, &name)?
            {
                if self.declare_global(parsed_id, defn_scope)?.is_some() {
                    variable_id = self.find_variable_namespaced(scope_id, &name)?;
                }
            }
        }
        match variable_id {
            None => match self.find_function_namespaced(scope_id, &name)? {
                None => Err(kerr!(
                    self,
                    name.name_span,
                    "No value '{}' is in scope",
                    self.ast.idents.get_string(name.name),
                )),
                Some(fn_id) => {
                    if self.get_function(fn_id).is_macro() {
                        kbail!(
                            self,
                            name.name_span,
                            "Macro '{}' cannot be used as a value",
                            self.ast.idents.get_string(name.name),
                        );
                    }
                    if !self.get_function(fn_id).type_params.is_empty() {
                        kbail!(
                            self,
                            name.name_span,
                            "'{}' is generic; provide its type parameters to reference a specialization: {}[type-a, type-b].&",
                            self.ast.idents.get_string(name.name),
                            self.ast.idents.get_string(name.name),
                        );
                    }
                    if matches!(
                        self.get_function(fn_id).linkage,
                        Linkage::Intrinsic | Linkage::LlvmIntrinsic(_)
                    ) {
                        kbail!(
                            self,
                            name.name_span,
                            "Intrinsic '{}' has no address",
                            self.ast.idents.get_string(name.name),
                        );
                    }
                    Ok((None, self.function_to_reference(fn_id, variable_name_span)))
                }
            },
            Some((variable_id, variable_scope_id)) => {
                self.check_lambda_capture_valid(
                    scope_id,
                    variable_id,
                    variable_scope_id,
                    name.name,
                    variable_name_span,
                )?;
                let v = self.variables.get(variable_id);
                let expr = self.exprs.add(
                    TypedExpr::Variable(VariableExpr { variable_id }),
                    v.type_id,
                    variable_name_span,
                );

                if !is_assignment_lhs {
                    self.register_variable_usage(variable_id, name.name_span);
                }
                Ok((Some(variable_id), expr))
            }
        }
    }

    fn check_lambda_capture_valid(
        &self,
        use_scope: ScopeId,
        variable_id: VariableId,
        variable_scope_id: ScopeId,
        name: StringId,
        span: SpanId,
    ) -> K1Result<()> {
        let Some(lambda_scope_id) = self.scopes.nearest_parent_lambda(use_scope) else {
            return Ok(());
        };
        let variable_is_above_lambda =
            self.scopes.scope_has_ancestor(lambda_scope_id, variable_scope_id);
        let variable_is_global = self.variables.get(variable_id).global_id().is_some();
        if variable_is_above_lambda && !variable_is_global {
            let name = self.ident_str(name);
            kbail!(
                self,
                span,
                "Lambda does not capture '{name}'. Declare it in the capture list: `fn[{name}]` copies its value, `fn[{name}.&]` captures its address",
            );
        }
        Ok(())
    }

    pub fn register_variable_usage(&mut self, variable_id: VariableId, span: SpanId) {
        self.emit_ls_entity(span, LsEntityKind::Variable { variable_id });
        self.variables.get_mut(variable_id).usage_count += 1;
    }

    pub fn get_expr_type(&self, expr_id: TypedExprId) -> &Type {
        self.types.get(self.exprs.get_type(expr_id))
    }

    pub fn get_expr_type_no_follow_static(&self, expr_id: TypedExprId) -> &Type {
        self.types.get(self.exprs.get_type(expr_id))
    }

    fn eval_field_access(
        &mut self,
        expr_id: ParsedExprId,
        field_access: &parse::FieldAccess,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = field_access.span;

        if !field_access.type_args.is_empty() {
            // Treat it like a call; foo.<field_name>[u32]
            let args = self.ast.mem.pushn(&[ParsedCallArg {
                name: None,
                value: field_access.base,
                is_explicit_context: false,
            }]);
            return self.eval_function_call(
                &ParsedCall {
                    name: QIdent::naked(field_access.field_name, span),
                    type_args: field_access.type_args,
                    args,
                    span,
                    is_method: true,
                    id: expr_id,
                },
                None,
                ctx,
                None,
            );
        }

        // Special case: .* dereference operation
        if field_access.field_name == self.ast.idents.b.asterisk {
            return self.eval_dereference(field_access.base, ctx, span);
        }

        // Special case: .& address-of operation
        if field_access.field_name == self.ast.idents.b.amp {
            return self.compile_address_of(field_access.base, ctx, span);
        }

        // Special case: .! unwrap operation
        if field_access.field_name == self.ast.idents.b.bang {
            return self.eval_unwrap_operator(field_access.base, ctx, field_access.span);
        }

        // Special case: .try unwrap operation
        if field_access.field_name == self.ast.idents.b.try_ {
            return self.eval_try_operator(field_access.base, ctx, field_access.span);
        }

        let raw_base_expr = self.eval_expr(field_access.base, ctx.with_no_expected_type())?;
        let raw_base_expr_type = self.exprs.get_type(raw_base_expr);
        let (base_agg_expr, base_type_id) = match self.types.get(raw_base_expr_type) {
            Type::Reference(r) => {
                let inner = r.inner_type;
                (self.synth_dereference(raw_base_expr), inner)
            }
            _other => (raw_base_expr, raw_base_expr_type),
        };

        if let Some(cs) = &mut self.completion
            && cs.site.is_none()
            && field_access.field_name == cs.marker
        {
            cs.site = Some(CompletionSite::Member {
                raw_base_type_id: raw_base_expr_type,
                base_type_id,
                scope_id: ctx.scope_id,
            });
        }

        // Optional fork case: sum.tag
        if field_access.field_name == self.ast.idents.b.tag {
            if let Some(get_tag) = self.handle_sum_get_tag(base_agg_expr, field_access.span)? {
                return Ok(get_tag);
            }
        }

        // Optional fork case: enum.value
        if field_access.field_name == self.ast.idents.b.value {
            if let Some(get_value) = self.handle_enum_get_value(base_agg_expr, field_access.span)? {
                return Ok(get_value);
            }
        }

        //eprintln!("base_type_id is {}", self.type_id_to_string(base_type_id));
        match self.types.get(base_type_id) {
            Type::StaticValue(svt) => {
                let Some(struct_type) = self.types.get(svt.family_type_id).as_struct() else {
                    kbail!(
                        self,
                        span,
                        "Field {} not found on value type {}",
                        field_access.field_name,
                        base_type_id
                    )
                };
                let (field_index, target_field) = struct_type
                    .find_field(&self.mem, field_access.field_name)
                    .ok_or_else(|| {
                        kerr!(
                            self,
                            span,
                            "Field {} not found on struct {}",
                            self.ast.idents.get_string(field_access.field_name),
                            base_type_id
                        )
                    })?;
                self.emit_ls_entity(
                    field_access.span,
                    LsEntityKind::StructField {
                        type_id: svt.family_type_id,
                        field_index: field_index as u32,
                    },
                );
                match svt.value_id {
                    None => {
                        // Abstract case
                        kbail!(self, span, "Can't work with abstract static structs yet")
                    }
                    Some(value_id) => {
                        let field_value_id = match self.static_values.get(value_id) {
                            StaticValue::Struct(static_struct) => {
                                *self.static_values.mem.get_nth(static_struct.fields, field_index)
                            }
                            StaticValue::Zero(_) => {
                                self.static_values.add(StaticValue::Zero(target_field.type_id))
                            }
                            _ => ice_span!(self, span, "Expected struct value type"),
                        };
                        let expr_id = self.add_static_value_expr(field_value_id, span);
                        Ok(expr_id)
                    }
                }
            }
            Type::Struct(struct_type) => {
                let Some((field_index, target_field)) =
                    struct_type.find_field(&self.mem, field_access.field_name)
                else {
                    let mut field_names = String::new();
                    for (idx, f) in self.mem.getn(struct_type.fields).iter().enumerate() {
                        if idx > 0 {
                            field_names.push_str(", ");
                        }
                        field_names.push_str(self.ast.idents.get_string(f.name));
                    }
                    kbail!(
                        self,
                        span,
                        "Field {} not found on struct {}\nFields are: {}",
                        self.ast.idents.get_string(field_access.field_name),
                        base_type_id,
                        field_names
                    );
                };
                self.emit_ls_entity(
                    field_access.span,
                    LsEntityKind::StructField {
                        type_id: base_type_id,
                        field_index: field_index as u32,
                    },
                );
                let result_type = target_field.type_id;
                let packed = self.is_field_access_packed(base_agg_expr, struct_type.record_kind);
                Ok(self.exprs.add(
                    TypedExpr::StructFieldAccess(FieldAccess {
                        base_struct: base_agg_expr,
                        field_index: field_index as u32,
                        packed,
                    }),
                    result_type,
                    span,
                ))
            }
            _ => Err(kerr!(
                self,
                span,
                "Field {} does not exist on type {}",
                self.ast.idents.get_string(field_access.field_name),
                base_type_id
            )),
        }
    }

    fn compile_address_of(
        &mut self,
        base_expr: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        if let ParsedExpr::Call(call) = self.ast.exprs.get(base_expr)
            && !call.type_args.is_empty()
            && call.args.is_empty()
            && !call.is_method
        {
            let call = *call;
            if self.find_variable_namespaced(ctx.scope_id, &call.name)?.is_none()
                && let Some(fn_id) = self.find_function_namespaced(ctx.scope_id, &call.name)?
            {
                return self.compile_specialized_function_reference(fn_id, &call, ctx);
            }
        }
        let expected_type = match ctx.expected_type_id {
            None => None,
            Some(t) => Some(self.get_type_id_dereferenced(t)),
        };
        let input = self.eval_expr(base_expr, ctx.with_expected_type(expected_type))?;
        if let TypedExpr::FunctionPointer(_) = self.exprs.get(input) {
            return Ok(input);
        }
        self.warn_packed_field_address_of(input, span);
        self.synth_address_of(input, span, false)
    }

    fn is_place_in_packed(&self, expr: TypedExprId) -> bool {
        match self.exprs.get(expr) {
            TypedExpr::StructFieldAccess(fa) => fa.packed,
            TypedExpr::ArrayGetElement(array_get) => array_get.packed,
            TypedExpr::SumGetPayload(get_payload) => get_payload.packed,
            _ => false,
        }
    }

    fn is_field_access_packed(&self, base_expr: TypedExprId, record_kind: RecordKind) -> bool {
        record_kind == RecordKind::Packed || self.is_place_in_packed(base_expr)
    }

    fn warn_packed_field_address_of(&mut self, expr: TypedExprId, span: SpanId) {
        if self.is_place_in_packed(expr) {
            self.report(kwarn!(
                self,
                span,
                "Address of a packed field: accesses through the resulting reference assume an alignment packed storage does not guarantee"
            ));
        }
    }

    fn synth_address_of(
        &mut self,
        input: TypedExprId,
        span: SpanId,
        allow_synthetic: bool,
    ) -> K1Result<TypedExprId> {
        let kind = self.check_place_for_address_of(input, allow_synthetic, span)?;
        if let AddressOfKind::StackVariable(variable_id) = kind {
            self.variables.get_mut(variable_id).flags.insert(VariableFlags::AddressTaken);
        }
        let input_type = self.exprs.get_type(input);
        let reference_type = self.add_reference_type(input_type);
        let addr_of_expr = self.exprs.add(
            TypedExpr::AddressOf(AddressOfExpr { target_expr: input, kind }),
            reference_type,
            span,
        );
        Ok(addr_of_expr)
    }

    /// Walks a place (lvalue) chain, verifying `expr` denotes a place whose
    /// address may be taken, and classifying the root.
    /// Valid: a.b.c.y.&   Invalid: a.foo().y.&
    /// `allow_synthetic` permits StackSynthetic variable roots, which user code
    /// cannot name.
    fn check_place_for_address_of(
        &self,
        expr: TypedExprId,
        allow_synthetic: bool,
        span: SpanId,
    ) -> K1Result<AddressOfKind> {
        match self.exprs.get(expr) {
            TypedExpr::Variable(v) => {
                let var = self.variables.get(v.variable_id);
                match var.kind {
                    // Params have no storage in ir (they are SSA values), so no address
                    // exists to take, even through a chain like &param.field
                    VariableKind::FnParam(_) => Err(kerr!(
                        self,
                        span,
                        "Cannot take address of a function parameter; re-declare it or use a * type"
                    )),
                    VariableKind::StackSynthetic(_) => {
                        if allow_synthetic {
                            Ok(AddressOfKind::StackVariable(v.variable_id))
                        } else {
                            Err(kerr!(
                                self,
                                span,
                                "Cannot take address of a compiler-generated variable"
                            ))
                        }
                    }
                    VariableKind::Stack(_) => Ok(AddressOfKind::StackVariable(v.variable_id)),
                    VariableKind::Global(_) => Ok(AddressOfKind::GlobalVariable(v.variable_id)),
                }
            }
            TypedExpr::StructFieldAccess(field_access) => {
                self.check_place_for_address_of(field_access.base_struct, allow_synthetic, span)
            }
            TypedExpr::ArrayGetElement(array_get) => {
                // The index is evaluated as a value; only the base affects placeness
                self.check_place_for_address_of(array_get.base_array, allow_synthetic, span)
            }
            TypedExpr::SumGetPayload(get_payload) => {
                self.check_place_for_address_of(get_payload.sum_expr, allow_synthetic, span)
            }
            // the address is the target reference's value, so no new
            // address is taken and there is nothing to register
            TypedExpr::Deref(_) => Ok(AddressOfKind::ReferenceExpr),
            // A block is a place if its resulting expression is a place
            TypedExpr::Block(block) => {
                let last_stmt = self.mem.getn(block.statements).last().copied();
                if let Some(TypedStmt::Expr(trailing_expr, _)) =
                    last_stmt.map(|s| self.stmts.get(s))
                {
                    self.check_place_for_address_of(*trailing_expr, allow_synthetic, span)
                } else {
                    Err(kerr!(
                        self,
                        span,
                        "Cannot take the address of a block with no trailing expression"
                    ))
                }
            }
            other => Err(kerr!(
                self,
                span,
                "Cannot take the address of a {}; only places (variables, struct fields, array elements, sum payloads, and dereferences) have addresses",
                other.kind_name()
            )),
        }
    }

    fn eval_try_operator(
        &mut self,
        operand: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let scope_id = ctx.scope_id;
        let block_return_type = self.get_return_type_for_scope(scope_id, span)?;
        let (block_try_impl, _) = self.expect_ability_impl(
                    block_return_type,
                    ABILITY_ID_TRY,
                    true,
                    scope_id,
                    span,
                ).map_err(|mut e| {
                        e.message = self.ast.idents.intern(format!(
                            "`.try` can only be used from a function or lambda that returns a type implementing `Try`. {}",
                            self.ident_str(e.message)
                        ));
                        e
                    })?;
        let try_value_original_expr = self.eval_expr(operand, ctx.with_no_expected_type())?;
        let try_value_type = self.exprs.get_type(try_value_original_expr);
        let (value_try_impl, _) =
            self.expect_ability_impl(try_value_type, ABILITY_ID_TRY, true, scope_id, span)?;
        let block_impl_args = self.ability_impls.get(block_try_impl.full_impl_id).impl_arguments;
        let value_impl_args = self.ability_impls.get(value_try_impl.full_impl_id).impl_arguments;
        let block_error_type =
            self.impl_arg_named(ABILITY_ID_TRY, block_impl_args, self.ast.idents.b.e).unwrap();
        let error_type =
            self.impl_arg_named(ABILITY_ID_TRY, value_impl_args, self.ast.idents.b.e).unwrap();
        if let Err(msg) = self.check_types(block_error_type, error_type, scope_id) {
            kbail!(
                self,
                span,
                "This function expects a Try, but with a different Error type than the value: {msg}"
            );
        };
        let value_success_type =
            self.impl_arg_named(ABILITY_ID_TRY, value_impl_args, self.ast.idents.b.t).unwrap();
        let mut result_block = self.new_block_builder(scope_id, ScopeType::LexicalBlock, span, 2);
        let try_value_var = self.synth_variable_defn_simple(
            self.ast.idents.b.try_value,
            try_value_original_expr,
            result_block.scope_id,
        );
        let result_block_ctx = ctx.with_scope(result_block.scope_id).with_no_expected_type();
        let is_ok_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__is_ok.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        let get_ok_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__get_value.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        let get_error_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__get_error.with_span(span),
            &[],
            &[try_value_var.variable_expr],
            result_block_ctx,
            false,
        )?;
        let block_make_error_fn =
            self.ability_impls.get(block_try_impl.full_impl_id).function_at_index(&self.mem, 0);
        let call_id = self.calls.add(Call {
            callee: Callee::from_ability_impl_fn(block_make_error_fn),
            args: self.mem.pushn(&[get_error_call]),
            type_args: TypeArgs::empty(),
            return_type: block_return_type,
            span,
        });
        let make_error_call = self.exprs.add(TypedExpr::Call { call_id }, block_return_type, span);
        let defers = self.gather_defers(result_block.scope_id, span, DeferExtent::FunctionTop);
        let return_error_expr = self.synth_defers_then_exit(
            defers,
            make_error_call,
            result_block_ctx,
            span,
            |k1, value| {
                k1.exprs.add(
                    TypedExpr::Return(TypedReturn { value, returned_variable: None }),
                    NEVER_TYPE_ID,
                    span,
                )
            },
        )?;
        let if_expr = self.synth_if_else(
            value_success_type,
            is_ok_call,
            get_ok_call,
            return_error_expr,
            span,
        );

        self.push_block_stmt_id(&mut result_block, try_value_var.defn_stmt);
        self.push_block_expr_id(&mut result_block, if_expr);

        Ok(self.exprs.add_block(result_block, value_success_type))
    }

    fn eval_unwrap_operator(
        &mut self,
        operand: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let operand_expr = self.eval_expr_inner(operand, ctx.with_no_expected_type())?;
        self.synth_typed_call_typed_args(
            self.ast.idents.f.try__get_value.with_span(span),
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
    ) -> K1Result<TypedExprId> {
        // Example:
        // let x: int = intptr.*
        // The expected_type when we get `intptr.*` is int, so
        // the expected_type when we get `intptr` should be *int
        let inner_expected_type = match ctx.expected_type_id {
            Some(expected) => Some(self.add_reference_type(expected)),
            None => None,
        };
        let base_expr = self.eval_expr(operand, ctx.with_expected_type(inner_expected_type))?;
        let base_expr_type = self.exprs.get_type(base_expr);
        let reference_type = self.types.get(base_expr_type).as_reference().ok_or_else(|| {
            kerr!(self, span, "Cannot dereference non-reference type: {}", base_expr_type)
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

    fn eval_expr(&mut self, expr_id: ParsedExprId, ctx: EvalExprContext) -> K1Result<TypedExprId> {
        let is_debug = self.ast.exprs.is_debug(expr_id);
        if is_debug {
            self.push_debug_level();
        }
        let mut self_ = scopeguard::guard(self, |s| {
            if is_debug {
                s.pop_debug_level()
            }
        });

        let result_expr = self_.eval_expr_inner(expr_id, ctx)?;

        if log::log_enabled!(log::Level::Debug) {
            let expr_span = self_.ast.exprs.get_span(expr_id);
            debug!(
                "DEBUG COMPILE DONE\n\n{}\n  type hint: {}`\n{}`",
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
    ) -> K1Result<TypedExprId> {
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
                        self.eval_struct_expected(expr_id, ctx)
                    } else {
                        self.eval_struct_anonymous(expr_id, ctx)
                    }
                } else {
                    self.eval_struct_anonymous(expr_id, ctx)
                }
            }
            ParsedExpr::If(if_expr) => self.eval_if_expr(&if_expr.clone(), ctx),
            ParsedExpr::While(while_expr) => self.eval_while_loop(&while_expr.clone(), ctx),
            ParsedExpr::Loop(loop_expr) => self.eval_loop_expr(&loop_expr.clone(), ctx),
            ParsedExpr::Return(r) => {
                let r = *r;
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    kbail!(self, r.span, "return cannot be used inside `defer` blocks");
                }
                self.eval_return(r.value, ctx, r.span)
            }
            ParsedExpr::Break(b) => self.eval_break(*b, ctx),
            ParsedExpr::Continue(c) => self.eval_continue(*c, ctx),
            ParsedExpr::BinaryOp(_binary_op) => self.eval_binary_op(expr_id, ctx),
            ParsedExpr::UnaryOp(op) => {
                let op = *op;
                match op.op_kind {
                    ParsedUnaryOpKind::BooleanNegation => {
                        let base = self.eval_expr_with_coercion(
                            op.expr,
                            ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                            true,
                        )?;
                        if self.exprs.get_type(base) == NEVER_TYPE_ID {
                            Ok(base)
                        } else {
                            self.synth_typed_call_typed_args(
                                self.ast.idents.f.neg__negated.with_span(op.span),
                                &[],
                                &[base],
                                ctx.with_no_expected_type(),
                                false,
                            )
                        }
                    }
                    ParsedUnaryOpKind::AddressOf => self.compile_address_of(op.expr, ctx, op.span),
                }
            }
            ParsedExpr::Literal(ParsedLiteral::Char(byte, span)) => {
                let value_id = self.static_values.add(StaticValue::Char(*byte));
                let expr_id = self.add_static_constant_expr(value_id, *span);
                Ok(expr_id)
            }
            ParsedExpr::Literal(ParsedLiteral::Numeric(int)) => {
                let span = int.span;
                let value_id = self.eval_numeric_value(int.text_span, ctx)?;
                let expr_id = self.add_static_constant_expr(value_id, span);
                Ok(expr_id)
            }
            ParsedExpr::Literal(ParsedLiteral::Bool(b, span)) => Ok(self.synth_bool(*b, *span)),
            ParsedExpr::Literal(ParsedLiteral::String(string_id, span)) => {
                let string_id = *string_id;
                let span = *span;
                if self.expected_type_is_code(ctx.expected_type_id) {
                    let value_id = self.make_static_code_value(&[(string_id, span)]);
                    return Ok(self.add_static_constant_expr(value_id, span));
                }
                let static_value_id = self.static_values.add_string(string_id);
                let string_type_id = self.builtin_types.string();
                let should_type_as_static = match ctx.expected_type_id {
                    None => false,
                    Some(t) => match self.get_static_type_of_type(t) {
                        None => false,
                        Some(stat) => stat.family_type_id == string_type_id,
                    },
                };
                let type_to_use = if should_type_as_static {
                    self.add_value_type(string_type_id, Some(static_value_id))
                } else {
                    string_type_id
                };
                let static_expr = self.exprs.add_static(
                    static_value_id,
                    type_to_use,
                    should_type_as_static,
                    span,
                );
                Ok(static_expr)
            }
            ParsedExpr::Variable(_variable) => Ok(self.eval_variable(expr_id, ctx, false)?.1),
            ParsedExpr::FieldAccess(field_access) => {
                let field_access = *field_access;
                self.eval_field_access(expr_id, &field_access, ctx)
            }
            ParsedExpr::Block(block) => {
                let block = *block;
                let scope_type = match block.kind {
                    ParsedBlockKind::FunctionBody => ScopeType::FunctionScope,
                    ParsedBlockKind::LexicalBlock => ScopeType::LexicalBlock,
                    ParsedBlockKind::LoopBody => ScopeType::LoopExprBody,
                };
                let declares = self.ast.mem.getn(block.stmts).iter().any(|stmt_id| {
                    match self.ast.stmts.get(*stmt_id) {
                        ParsedStmt::Use(_)
                        | ParsedStmt::Let(_)
                        | ParsedStmt::Require(_)
                        | ParsedStmt::Defer(_) => true,
                        ParsedStmt::Assign(_) | ParsedStmt::LoneExpression(_) => false,
                    }
                });
                // be thrifty with scopes
                let needs_scope = declares
                    || block.kind == ParsedBlockKind::LoopBody
                    || self.scopes.block_defers.contains_key(&ctx.scope_id);
                let block_ctx = if needs_scope {
                    let block_scope =
                        self.scopes.add_child_scope(ctx.scope_id, scope_type, ScopeOwnerId::None);
                    ctx.with_scope(block_scope)
                } else {
                    ctx
                };
                let needs_terminator = match block.kind {
                    ParsedBlockKind::FunctionBody => true,
                    ParsedBlockKind::LexicalBlock => false,
                    ParsedBlockKind::LoopBody => false,
                };
                let block = self.eval_block(&block, block_ctx, needs_terminator)?;
                Ok(block)
            }
            ParsedExpr::Call(call) => self.eval_function_call(&call.clone(), None, ctx, None),
            ParsedExpr::CallOnExpr(call) => {
                let call = *call;
                let called_expr_span = self.ast.get_expr_span(call.called_expr);
                let called_expr = self.eval_expr(call.called_expr, ctx.with_no_expected_type())?;
                let called_expr_type = self.exprs.get_type(called_expr);
                let Type::FunctionPointer(_) = self.types.get(called_expr_type) else {
                    kbail!(
                        self,
                        called_expr_span,
                        "Not a callable expression; type is '{}' rather than a function pointer",
                        called_expr_type
                    );
                };
                let callee = Callee::DynamicFunction { function_pointer_expr: called_expr };
                let call = ParsedCall {
                    name: QIdent::naked(self.ast.idents.b.invoke, called_expr_span),
                    type_args: MSlice::empty(),
                    args: call.args,
                    span: call.span,
                    is_method: false,
                    id: expr_id,
                };
                self.eval_function_call(&call, None, ctx, Some(callee))
            }
            ParsedExpr::For(for_expr) => self.eval_for_expr(&for_expr.clone(), ctx),
            ParsedExpr::Variant(parsed_variant) => self.eval_variant(*parsed_variant, ctx),
            ParsedExpr::Is(is_expr) => {
                let is_expr = *is_expr;
                // If the 'is' is attached to an if/else, that is handled by if/else
                // This is just the case of the detached 'is' where we want to return a boolean
                // indicating whether or not the pattern matched only
                let true_expression = self.ast.exprs.add(parse::ParsedExpr::Literal(
                    parse::ParsedLiteral::Bool(true, is_expr.span),
                ));
                let true_case = parse::ParsedMatchCase {
                    patterns: MSpillSlice::one(is_expr.pattern),
                    guard_condition_expr: None,
                    expression: true_expression,
                };
                let as_match_expr = parse::ParsedMatch {
                    match_subject: is_expr.target_expression,
                    cases: self.ast.mem.pushn(&[true_case]),
                    span: is_expr.span,
                    is_static: false,
                };
                let match_expr_id = self.ast.exprs.add(parse::ParsedExpr::Match(as_match_expr));
                let check_exhaustive = false;
                // For standalone 'is', we don't allow binding to patterns since they won't work
                let allow_bindings = false;
                // add_fallback: false since we have an explicit false case.
                // Alternatively we could accept a fallback _value_ in eval_match_expr
                let false_expr = self.synth_bool(false, is_expr.span);
                self.eval_match_expr(
                    match_expr_id,
                    ctx,
                    check_exhaustive,
                    allow_bindings,
                    Some(false_expr),
                )
            }
            ParsedExpr::Match(_match_expr) => {
                let check_exhaustive = true;
                let allow_bindings = true;
                self.eval_match_expr(expr_id, ctx, check_exhaustive, allow_bindings, None)
            }
            ParsedExpr::Lambda(_lambda) => self.eval_lambda(expr_id, ctx),
            ParsedExpr::InterpolatedString(is) => {
                if self.expected_type_is_code(ctx.expected_type_id) {
                    let is = *is;
                    self.synth_interpolated_code(is.parts, is.span, ctx, None)
                } else {
                    let res = self.synth_interpolated_string(expr_id, ctx, None)?;
                    Ok(res)
                }
            }
            ParsedExpr::Builtin(span) => {
                // Handled in eval_global_body before dispatching here
                Err(kerr!(
                    self,
                    *span,
                    "builtin can currently only be used as the initializer of a global"
                ))
            }
            ParsedExpr::Static(stat) => {
                let stat = *stat;
                match self.compile_static_or_meta(expr_id, stat, false, ctx)? {
                    StaticExecutionResult::TypedExpr(typed_expr) => Ok(typed_expr),
                    StaticExecutionResult::Definitions(_) => {
                        self.ice_span(stat.span, "Got static definitions from an expression")
                    }
                }
            }
            ParsedExpr::Code(code) => {
                let code_span = code.span;
                let parsed_stmt_span = self.ast.get_stmt_span(code.parsed_stmt);
                let span_content = self
                    .ast
                    .sources
                    .get_span_content(&self.ast.mem, self.ast.spans.get(parsed_stmt_span));
                let string_id = self.ast.idents.intern(span_content);
                let value_id = self.make_static_code_value(&[(string_id, parsed_stmt_span)]);
                Ok(self.add_static_constant_expr(value_id, code_span))
            }
            ParsedExpr::QualifiedAbilityCall(qcall) => {
                let qcall = *qcall;
                let signature = self.eval_ability_expr(qcall.ability_expr, true, ctx.scope_id)?;
                // Locate the precise impl
                let self_type_id = self.eval_type_expr(qcall.self_name, ctx.scope_id)?;
                let (impl_handle, _) = self
                    .find_or_generate_specialized_ability_impl_for_type(
                        self_type_id,
                        signature.specialized_ability_id,
                        true,
                        ctx.scope_id,
                        qcall.span,
                    )
                    .map_err(|msg| kerr!(self, qcall.span, "{}", msg))?;

                // Get the function id from it by name I guess
                let call_ast_expr = *self.ast.exprs.get(qcall.call_expr).expect_call();
                let call_name = call_ast_expr.name.name;
                let Some(tafr) = self
                    .abilities
                    .get(signature.specialized_ability_id)
                    .find_function_by_name(&self.mem, call_name)
                else {
                    kbail!(
                        self,
                        call_ast_expr.name.name_span,
                        "No such function `{}` in ability `{}`",
                        call_name,
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
            ParsedExpr::TypeHint(th) => {
                let th = *th;
                let type_id = self.eval_type_expr(th.ty, ctx.scope_id)?;
                let inner = self.eval_expr(th.inner, ctx.with_expected_type(Some(type_id)))?;
                let allow_addr_of = ctx.is_method_receiver();
                self.check_and_coerce_expr(type_id, inner, ctx.scope_id, allow_addr_of).map_err(
                    |e| kerr!(self, th.span, "Expression did not conform to hint: {}", e.message),
                )
            }
            ParsedExpr::Index(index) => {
                let index = *index;
                let base = self.eval_expr(index.base, ctx.with_no_expected_type())?;
                let base_type = self.get_type_id_dereferenced(self.exprs.get_type(base));
                if self.types.get(base_type).as_array().is_some() {
                    return self.synth_array_element(base, base_type, index.key, ctx, index.span);
                }
                let args = self.ast.mem.pushn(&[
                    ParsedCallArg::unnamed(index.base),
                    ParsedCallArg::unnamed(index.key),
                ]);
                let call_id = self.ast.exprs.add(ParsedExpr::Call(ParsedCall {
                    name: QIdent::naked(self.ast.idents.b.index_ref, index.span),
                    type_args: MSlice::empty(),
                    args,
                    span: index.span,
                    is_method: true,
                    id: ParsedExprId::PENDING,
                }));
                let call = *self.ast.exprs.get(call_id).expect_call();
                let element_ref_expected = match ctx.expected_type_id {
                    Some(expected) => Some(self.add_reference_type(expected)),
                    None => None,
                };
                let element_ref = self.eval_method_call_on_typed_receiver(
                    &call,
                    (index.base, base),
                    ctx.with_expected_type(element_ref_expected),
                )?;
                let element_ref_type = self.exprs.get_type(element_ref);
                if self.types.get(element_ref_type).as_reference().is_none() {
                    kbail!(
                        self,
                        index.span,
                        "index-ref must return a reference to the element; got {}",
                        element_ref_type
                    );
                }
                Ok(self.synth_dereference(element_ref))
            }
        }
    }

    fn eval_expr_with_coercion(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
        fail: bool,
    ) -> K1Result<TypedExprId> {
        let expr = self.eval_expr(expr_id, ctx)?;
        match ctx.expected_type_id {
            None => Ok(expr),
            Some(expected_type) => {
                let allow_addr_of = ctx.is_method_receiver();
                match self.check_and_coerce_expr(expected_type, expr, ctx.scope_id, allow_addr_of) {
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

    fn eval_list_literal(
        &mut self,
        _expr_id: ParsedExprId,
        list_expr: &ParsedListLiteral,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let (expected_element_type, list_kind) = match ctx.expected_type_id.as_ref() {
            Some(&type_id) => {
                if let Some((element_type, container_kind)) =
                    self.get_as_container_instance(type_id)
                {
                    (Some(element_type), container_kind)
                } else {
                    (None, ContainerKind::List)
                }
            }
            None => (None, ContainerKind::List),
        };
        let span = list_expr.span;
        let element_count = list_expr.elements.len();

        if let ContainerKind::Array(array_type_id) = list_kind {
            let size_type = self.types.get(array_type_id).as_array().unwrap().size_type;
            if let Some(array_count) = self.get_concrete_count_of_array(size_type)
                && array_count != element_count as i64
            {
                kbail!(
                    self,
                    span,
                    "Expected {} elements for {} but got {}",
                    array_count,
                    self.type_id_to_string(array_type_id).blue(),
                    element_count
                );
            }
        }

        let mut element_type = None;
        let mut trivial_values = self.tmp.new_list(element_count);
        let mut elements = self.tmp.new_list(element_count);
        for elem in self.ast.mem.getn(list_expr.elements) {
            let current_expected_type = element_type.or(expected_element_type);
            let element_expr = self.eval_expr_with_coercion(
                *elem,
                ctx.with_expected_type(current_expected_type),
                true,
            )?;

            if let Some(static_value) = self.convert_trivial_static_expr(element_expr) {
                trivial_values.push(static_value)
            }

            if element_type.is_none() {
                // Erase static type info since a list of all one static value isn't very useful
                let this_element_type = self.exprs.get_type(element_expr);
                let chased_type = self.get_static_family_id_if_static(this_element_type);
                element_type = Some(chased_type)
            };
            elements.push(element_expr);
        }

        // Note: Typing of empty list literals with no expected type is tricky
        //        We use is_inference here to use UNIT or fail.
        //        If I report UNIT during inference it leads to incorrect failures
        //        Failing during inference is like producing a type hole
        let element_type = match element_type.or(expected_element_type) {
            Some(et) => et,
            None => {
                if ctx.is_inference() {
                    kbail!(self, span, "Not enough information to determine empty list type");
                } else {
                    self.builtin_types.empty
                }
            }
        };

        // Early out for trivial lists
        if trivial_values.len() == element_count as usize {
            let span_type =
                self.instantiate_generic_type(self.builtin_types.span(), &[element_type]);
            let static_span = self.add_static_container_from_ids(
                StaticContainerKind::Span,
                span_type,
                trivial_values.as_slice(),
            );
            let static_span_expr = self.add_static_constant_expr(static_span, span);
            let ctx_hidden = ctx.with_hidden_calls(true);
            return match list_kind {
                ContainerKind::Array(array_type_id) => {
                    let array_size_type =
                        self.types.get(array_type_id).as_array().unwrap().size_type;
                    let span_to_array = self.synth_typed_call_typed_args(
                        self.ast.idents.f.span_to_array.with_span(span),
                        &[element_type, array_size_type],
                        &[static_span_expr],
                        ctx_hidden,
                        false,
                    )?;
                    Ok(span_to_array)
                }
                ContainerKind::Buffer => {
                    let buffer_expr = self.synth_typed_call_typed_args(
                        self.ast.idents.f.buffer_from_span.with_span(span),
                        &[element_type],
                        &[static_span_expr],
                        ctx_hidden,
                        false,
                    )?;
                    Ok(buffer_expr)
                }
                ContainerKind::Span => Ok(static_span_expr),
                ContainerKind::List => {
                    let list_expr = self.synth_typed_call_typed_args(
                        self.ast.idents.f.list_from_span.with_span(span),
                        &[element_type],
                        &[static_span_expr],
                        ctx_hidden,
                        false,
                    )?;
                    Ok(list_expr)
                }
            };
        }

        let mut list_lit_block =
            self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 2 + element_count);
        let list_lit_scope = list_lit_block.scope_id;
        let list_lit_ctx = ctx.with_scope(list_lit_scope).with_no_expected_type();
        let count_expr = self.synth_i64(element_count as i64, span);

        let make_dest_coll = match list_kind {
            ContainerKind::List => self.synth_typed_call_typed_args(
                self.ast.idents.f.list_with_capacity.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            ContainerKind::Buffer | ContainerKind::Span => self.synth_typed_call_typed_args(
                self.ast.idents.f.buffer_allocate.with_span(span),
                &[element_type],
                &[count_expr],
                list_lit_ctx,
                false,
            )?,
            // Unlike the others, the array literal allocates on the stack!
            ContainerKind::Array(array_type_id) => self.synth_typed_call_typed_args(
                self.ast.idents.f.mem_zeroed.with_span(span),
                &[array_type_id],
                &[],
                list_lit_ctx,
                false,
            )?,
        };
        let needs_address_of = matches!(list_kind, ContainerKind::List);
        let dest_coll_variable = self.synth_variable_defn(
            self.ast.idents.b.list_lit,
            make_dest_coll,
            false,
            list_lit_scope,
            None,
        );
        let dest_coll_expr = if needs_address_of {
            self.synth_address_of(dest_coll_variable.variable_expr, SpanId::NONE, true).unwrap()
        } else {
            dest_coll_variable.variable_expr
        };

        list_lit_block.statements.push(dest_coll_variable.defn_stmt);
        for (index, element_value_expr) in elements.iter().enumerate() {
            let index_expr = self.synth_i64(index as i64, span);
            let element_ref = match list_kind {
                ContainerKind::List => {
                    let push_call = self.synth_typed_call_typed_args(
                        self.ast.idents.f.list_push.with_span(span),
                        &[element_type],
                        &[dest_coll_expr, *element_value_expr],
                        list_lit_ctx,
                        false,
                    )?;
                    let type_id = self.exprs.get_type(push_call);
                    let push_stmt = self.stmts.add(TypedStmt::Expr(push_call, type_id));
                    self.push_block_stmt_id(&mut list_lit_block, push_stmt);
                    continue;
                }
                ContainerKind::Buffer | ContainerKind::Span => self.synth_typed_call_typed_args(
                    self.ast.idents.f.buffer_index_unchecked.with_span(span),
                    &[element_type],
                    &[dest_coll_expr, index_expr],
                    list_lit_ctx,
                    false,
                )?,
                ContainerKind::Array(_) => {
                    let element = self.exprs.add(
                        TypedExpr::ArrayGetElement(ArrayGetElement {
                            base_array: dest_coll_expr,
                            index: index_expr,
                            packed: false,
                        }),
                        element_type,
                        span,
                    );
                    self.synth_address_of(element, span, true)?
                }
            };
            let store_stmt = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                destination: element_ref,
                value: *element_value_expr,
                span,
                kind: AssignmentKind::Store,
            }));
            self.push_block_stmt_id(&mut list_lit_block, store_stmt);
        }
        let final_expr = match list_kind {
            ContainerKind::List => dest_coll_variable.variable_expr,
            ContainerKind::Buffer => dest_coll_variable.variable_expr,
            ContainerKind::Span => self.synth_typed_call_typed_args(
                self.ast.idents.f.span_wrapBuffer.with_span(span),
                &[element_type],
                &[dest_coll_variable.variable_expr],
                ctx.with_no_expected_type(),
                false,
            )?,
            ContainerKind::Array(_array_type_id) => dest_coll_variable.variable_expr,
        };
        self.push_block_expr_id(&mut list_lit_block, final_expr);
        let final_expr_type = self.exprs.get_type(final_expr);
        Ok(self.exprs.add_block(list_lit_block, final_expr_type))
    }

    fn eval_struct_anonymous(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let ParsedExpr::Struct(parsed_struct) = *self.ast.exprs.get(expr_id) else {
            self.ice_span(self.ast.get_expr_span(expr_id), "expected struct")
        };
        let mut field_values = self.mem.new_list(parsed_struct.fields.len());
        let mut field_defns = self.mem.new_list(parsed_struct.fields.len());
        for ast_field in self.ast.mem.getn(parsed_struct.fields) {
            let parsed_expr = match ast_field.value {
                StructValueFieldKind::VarShorthand => {
                    self.synth_parsed_variable_expr(ast_field.name, ast_field.span)
                }
                StructValueFieldKind::Expr(parsed_expr) => parsed_expr,
                StructValueFieldKind::Uninit => {
                    kbail!(self, ast_field.span, "uninit is not allowed in anonymous structs");
                }
            };
            let expr = self.eval_expr(parsed_expr, ctx.with_expected_type(None))?;
            let expr_type = self.exprs.get_type(expr);
            if expr_type == NEVER_TYPE_ID {
                kbail!(self, ast_field.span, "never is not allowed in struct literals");
            }
            field_defns.push(StructTypeField {
                name: ast_field.name,
                type_id: expr_type,
                span: ast_field.span,
            });
            field_values.push(StructLiteralField { name: ast_field.name, expr: Some(expr) });
        }

        let struct_type = StructType::struc(field_defns.to_slice());
        let struct_type_id = self.add_type_anon(Type::Struct(struct_type));
        let typed_struct = StructLiteral { fields: field_values.to_slice() };
        Ok(self.exprs.add(TypedExpr::Struct(typed_struct), struct_type_id, parsed_struct.span))
    }

    fn eval_struct_expected(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let ParsedExpr::Struct(parsed_struct) = *self.ast.exprs.get(expr_id) else {
            self.ice_span(self.ast.get_expr_span(expr_id), "expected struct")
        };
        let expected_struct_id = ctx.expected_type_id.unwrap();
        let Type::Struct(expected_struct) = self.types.get(expected_struct_id) else {
            self.ice_span(self.ast.get_expr_span(expr_id), "expected an expected struct type")
        };
        let expected_struct = *expected_struct;

        let is_union = expected_struct.record_kind == RecordKind::Union;
        if is_union && parsed_struct.fields.len() > 1 {
            kbail!(
                self,
                self.ast.get_expr_span(expr_id),
                "{} is a union; a union literal sets at most one field",
                expected_struct_id
            )
        }

        if let Some(cs) = &mut self.completion
            && cs.site.is_none()
            && self.ast.mem.getn(parsed_struct.fields).iter().any(|f| f.name == cs.marker)
        {
            cs.site = Some(CompletionSite::StructField {
                type_id: expected_struct_id,
                parsed_struct_id: expr_id,
            });
        }
        let expected_struct_defn_info = self.get_defn_info(expected_struct_id);
        let field_count = expected_struct.fields.len();

        let mut passed_fields_aligned: SV8<(Option<ParsedExprId>, SpanId)> = smallvec![];

        let struct_span = parsed_struct.span;
        let mut missing_fields: SV4<StringId> = smallvec![];
        for (index, expected_field) in self.mem.getn(expected_struct.fields).iter().enumerate() {
            let Some(passed_field) = self
                .ast
                .mem
                .getn(parsed_struct.fields)
                .iter()
                .find(|f| f.name == expected_field.name)
            else {
                if is_union {
                    passed_fields_aligned.push((None, expected_field.span));
                } else {
                    missing_fields.push(expected_field.name);
                }
                continue;
            };
            let parsed_expr = match passed_field.value {
                StructValueFieldKind::VarShorthand => {
                    Some(self.synth_parsed_variable_expr(passed_field.name, passed_field.span))
                }
                StructValueFieldKind::Expr(parsed_expr) => Some(parsed_expr),
                StructValueFieldKind::Uninit => None,
            };
            self.emit_ls_entity(
                passed_field.span,
                LsEntityKind::StructField {
                    type_id: expected_struct_id,
                    field_index: index as u32,
                },
            );
            passed_fields_aligned.push((parsed_expr, passed_field.span))
        }

        if !missing_fields.is_empty() {
            kbail!(
                self,
                struct_span,
                "struct is missing fields {}",
                missing_fields.iter().map(|fname| self.get_string(*fname)).join(", ")
            );
        }

        if let Some(unknown_field) =
            self.ast.mem.getn(parsed_struct.fields).iter().find(|passed_field| {
                expected_struct.find_field(&self.mem, passed_field.name).is_none()
            })
        {
            kbail!(self, struct_span, "Struct has an unexpected field '{}'", unknown_field.name);
        }

        let mut field_values: List<StructLiteralField, _> = self.mem.new_list(field_count);
        let mut field_types: List<StructTypeField, _> = self.mem.new_list(field_count);
        for ((passed_expr, passed_span), expected_field) in
            passed_fields_aligned.iter().zip(self.mem.getn(expected_struct.fields).iter())
        {
            match passed_expr {
                None => {
                    // Uninitialized field
                    field_values.push(StructLiteralField { name: expected_field.name, expr: None });
                    field_types.push(StructTypeField {
                        name: expected_field.name,
                        type_id: expected_field.type_id,
                        span: expected_field.span,
                    });
                }
                Some(passed_expr) => {
                    let expr = self.eval_expr_with_coercion(
                        *passed_expr,
                        ctx.with_expected_type(Some(expected_field.type_id)),
                        true,
                    )?;
                    let expr_type = self.exprs.get_type(expr);
                    if expr_type == NEVER_TYPE_ID {
                        kbail!(self, *passed_span, "never is not allowed in struct literals");
                    }
                    field_types.push(StructTypeField {
                        name: expected_field.name,
                        type_id: expr_type,
                        span: expected_field.span,
                    });
                    field_values
                        .push(StructLiteralField { name: expected_field.name, expr: Some(expr) });
                }
            };
        }

        let output_instance_info = match self.get_instance_info(expected_struct_id).cloned() {
            None => None,
            Some(mut gi) => {
                if ctx.is_inference() {
                    debug!(
                        "I need to set the right info for {} from expected [{}] and my literal values [{}]",
                        self.type_id_to_string_ext(
                            gi.generic_parent,
                            dump::TypeDisplayMode::Expand
                        ),
                        self.pretty_print_types(self.get_type_slice(gi.type_args), ", "),
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

                    let mut subst_pairs = self.tmp.new_list(generic_fields.len());
                    for (value, generic_field) in
                        field_types.iter().zip(self.mem.getn(generic_fields).iter())
                    {
                        subst_pairs.push(InferenceInputPair {
                            param_type: generic_field.type_id,
                            arg: TypeOrParsedExpr::Type(value.type_id),
                            allow_mismatch: true,
                        });
                    }
                    let generic_params_slice = self.mem.getn(generic_params);
                    let (solutions, _all_solutions) = self
                        .with_clean_inference(|k1| {
                            k1.infer_types(
                                generic_params_slice,
                                generic_params,
                                &subst_pairs,
                                struct_span,
                                ctx.scope_id,
                                None,
                            )
                        })
                        .map_err(|f| self.render_inference_failure(f))?;
                    debug!(
                        "I reverse-engineered these: {}",
                        self.pretty_print_types(solutions.as_slice(&self.mem), ", ")
                    );
                    gi.type_args = self.intern_type_args(solutions);
                    Some(gi)
                } else {
                    Some(gi)
                }
            }
        };
        let output_struct = StructType {
            fields: field_types.to_slice(),
            record_kind: expected_struct.record_kind,
        };
        let output_struct_type_id = self.add_type(
            Type::Struct(output_struct),
            expected_struct_defn_info,
            output_instance_info,
        );

        let typed_struct = StructLiteral { fields: field_values.to_slice() };
        Ok(self.exprs.add(
            TypedExpr::Struct(typed_struct),
            output_struct_type_id,
            parsed_struct.span,
        ))
    }

    fn eval_while_loop(
        &mut self,
        while_expr: &ParsedWhileExpr,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let ParsedExpr::Block(parsed_block) = *self.ast.exprs.get(while_expr.body) else {
            kbail!(self, while_expr.span, "'while' body must be a block");
        };

        let cond_ctx = if self.matching_condition_binds(while_expr.cond) {
            let condition_scope_id = self.scopes.add_child_scope(
                ctx.scope_id,
                ScopeType::LexicalBlock,
                ScopeOwnerId::None,
            );
            ctx.with_scope(condition_scope_id)
        } else {
            ctx
        };

        let condition_or_block = self.eval_matching_condition(while_expr.cond, None, cond_ctx)?;
        let condition = match condition_or_block {
            MatchingConditionResult::MatchingCondition(mc) => mc,
            MatchingConditionResult::NeverBlock(never_block) => return Ok(never_block),
        };

        let body_block_scope_id = self.scopes.add_child_scope(
            cond_ctx.scope_id,
            ScopeType::WhileLoopBody,
            ScopeOwnerId::None,
        );
        self.scopes.add_loop_info(
            body_block_scope_id,
            ScopeLoopInfo { break_type: Some(self.builtin_types.empty), label: while_expr.label },
        );

        let body_block =
            self.eval_block(&parsed_block, ctx.with_scope(body_block_scope_id), false)?;

        // TODO: detect divergent loops: if loop has no breaks or returns, can we type it as never?
        //
        // Loop Info should be able to track this, if we report every
        // break and return
        let loop_type = self.builtin_types.empty;

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
    ) -> K1Result<TypedExprId> {
        let body_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LoopExprBody, ScopeOwnerId::None);
        self.scopes.add_loop_info(
            body_scope,
            ScopeLoopInfo { break_type: ctx.expected_type_id, label: loop_expr.label },
        );

        // Expected type is handled by loop info above, its needed by 'break's but notably we do not
        // want to require the loop's block to return a type other than Unit, so we pass None.
        let expected_expression_type_for_block = None;
        let body_block = self.eval_block(
            &loop_expr.body.clone(),
            ctx.with_scope(body_scope).with_expected_type(expected_expression_type_for_block),
            false,
        )?;

        let loop_info = self.scopes.get_loop_info(body_scope).unwrap();

        let break_type = loop_info.break_type.unwrap_or(self.builtin_types.empty);
        Ok(self.exprs.add(TypedExpr::LoopExpr(LoopExpr { body_block }), break_type, loop_expr.span))
    }

    fn eval_lambda(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        struct ClosureSetup {
            environment_struct: TypedExprId,
            environment_struct_type: TypeId,
            environment_param: FnParamType,
            environment_param_variable_id: VariableId,
            prologue_stmts: SV8<TypedStmtId>,
            capture_bindings: SV4<(VariableId, SpanId)>,
        }

        let lambda = self.ast.exprs.get(expr_id).expect_lambda();
        let lambda_captures = lambda.captures;
        let lambda_arguments = lambda.arguments;
        let lambda_body = lambda.body;
        let span = lambda.span;
        let body_span = self.ast.exprs.get_span(lambda.body);
        let is_closure = !lambda_captures.is_empty();
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
            .map(|ft| *self.types.get(ft).as_function().unwrap());
        let declared_expected_return_type = match lambda.return_type {
            None => None,
            Some(return_type_expr) => Some(self.eval_type_expr(return_type_expr, ctx.scope_id)?),
        };
        let expected_return_type = declared_expected_return_type
            .or(expected_function_type.as_ref().map(|f| f.return_type));

        // Captures resolve at the creation site, in the enclosing scope; each becomes a
        // field of the environment struct. Resolving through eval_variable applies the
        // lambda-boundary check when this lambda is itself nested inside another one.
        let captures = self.ast.mem.getn(lambda_captures);
        let mut capture_field_types: SV4<TypeId> = smallvec![];
        let mut env_field_types = self.mem.new_list(lambda_captures.len());
        let mut env_field_exprs = self.mem.new_list(lambda_captures.len());
        for (index, capture) in captures.iter().enumerate() {
            if captures[..index].iter().any(|prior| prior.name == capture.name) {
                kbail!(self, capture.span, "Duplicate capture '{}'", capture.name);
            }
            if self.ast.mem.getn(lambda_arguments).iter().any(|a| a.binding == capture.name) {
                kbail!(
                    self,
                    capture.span,
                    "Capture '{}' collides with a parameter name",
                    capture.name
                );
            }
            let parsed_var = self.ast.exprs.add(ParsedExpr::Variable(parse::ParsedVariable {
                name: QIdent::naked(capture.name, capture.span),
                span: capture.span,
            }));
            let (variable_id, variable_expr) = self.eval_variable(parsed_var, ctx, false)?;
            let Some(variable_id) = variable_id else {
                kbail!(
                    self,
                    capture.span,
                    "Captures must name variables; '{}' is not one",
                    capture.name
                );
            };
            if self.variables.get(variable_id).global_id().is_some() {
                kbail!(
                    self,
                    capture.span,
                    "'{}' is a global; globals are always in scope and need not be captured",
                    capture.name
                );
            }
            let field_expr = if capture.by_ref {
                self.synth_address_of(variable_expr, capture.span, false)?
            } else {
                variable_expr
            };
            let field_type = self.exprs.get_type(field_expr);
            capture_field_types.push(field_type);
            env_field_types.push(StructTypeField {
                type_id: field_type,
                name: capture.name,
                span: capture.span,
            });
            env_field_exprs.push(StructLiteralField { name: capture.name, expr: Some(field_expr) });
        }
        let env_fields_handle = env_field_types.to_slice();
        let env_field_exprs_handle = env_field_exprs.to_slice();

        let lambda_scope_id =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LambdaScope, ScopeOwnerId::None);
        self.scopes.add_lambda_info(
            lambda_scope_id,
            ScopeLambdaInfo { expected_return_type, returned_variable: None },
        );
        let mut typed_params = self.mem.new_list(lambda_arguments.len() + 1);

        for (index, arg) in self.ast.mem.getn(lambda_arguments).iter().enumerate() {
            let arg_type_id = match arg.ty {
                Some(type_expr) => self.eval_type_expr(type_expr, ctx.scope_id)?,
                None => {
                    let Some(expected_function_type) = expected_function_type.as_ref() else {
                        kbail!(
                            self,
                            arg.span,
                            "Cannot infer lambda parameter type {} without more context",
                            arg.binding
                        );
                    };
                    let Some(expected_ty) =
                        self.mem.get_nth_opt(expected_function_type.logical_params(), index)
                    else {
                        kbail!(
                            self,
                            arg.span,
                            "Cannot infer lambda parameter type {}: expected type has fewer parameters than lambda",
                            arg.binding
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
                is_macro_code: false,
            });
        }

        let mut param_variables = self.mem.new_list(lambda_arguments.len() + 1);

        for (typed_arg, parsed_arg) in typed_params.iter().zip(self.ast.mem.getn(lambda_arguments))
        {
            let name = typed_arg.name;
            let variable_id = self.variables.add(Variable {
                name,
                type_id: typed_arg.type_id,
                owner_scope: lambda_scope_id,
                kind: VariableKind::FnParam(FunctionId::PENDING),
                flags: VariableFlags::empty(),
                usage_count: 0,
                defn_span: parsed_arg.span,
            });
            self.scopes.add_variable(lambda_scope_id, name, variable_id);
            param_variables.push(TypedFunctionParam { variable_id, span: parsed_arg.span })
        }

        // For closures, the environment param, its casted alias, and one `let` per capture
        // are all in place before the body is evaluated, so capture references in the body
        // resolve as ordinary variables.
        let closure_setup: Option<ClosureSetup> = if is_closure {
            let environment_struct_type =
                self.add_type_anon(Type::Struct(StructType::struc(env_fields_handle)));
            let environment_struct = self.exprs.add(
                TypedExpr::Struct(StructLiteral { fields: env_field_exprs_handle }),
                environment_struct_type,
                span,
            );
            let environment_struct_reference_type =
                self.add_reference_type(environment_struct_type);

            // We decay down to POINTER so that the function calls typecheck
            let environment_param = FnParamType {
                name: self.ast.idents.b.lambda_env_var_name,
                type_id: POINTER_TYPE_ID,
                is_context: false,
                is_lambda_env: true,
                is_macro_code: false,
            };
            let environment_param_variable_id = self.variables.add(Variable {
                name: environment_param.name,
                type_id: POINTER_TYPE_ID,
                owner_scope: lambda_scope_id,
                kind: VariableKind::FnParam(FunctionId::PENDING),
                flags: VariableFlags::empty(),
                usage_count: 0,
                defn_span: span,
            });

            let environment_param_access_expr = self.exprs.add(
                TypedExpr::Variable(VariableExpr { variable_id: environment_param_variable_id }),
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
                self.ast.idents.b.lambda_env_var_name,
                cast_env_param,
                false,
                lambda_scope_id,
                None,
            );

            let mut prologue_stmts: SV8<TypedStmtId> = smallvec![];
            prologue_stmts.push(environment_casted_variable.defn_stmt);
            let mut capture_bindings: SV4<(VariableId, SpanId)> = smallvec![];
            for (field_index, capture) in captures.iter().enumerate() {
                let field_type = capture_field_types[field_index];
                let env_variable_expr =
                    self.synth_variable_expr(environment_casted_variable.variable_id, capture.span);
                let env_deref = self.synth_dereference(env_variable_expr);
                let field_access = self.exprs.add(
                    TypedExpr::StructFieldAccess(FieldAccess {
                        base_struct: env_deref,
                        field_index: field_index as u32,
                        packed: false,
                    }),
                    field_type,
                    capture.span,
                );
                // The binding behaves exactly like a user-written `let <name> = env.<name>`
                let defn_stmt = self.stmts.next_id();
                let variable_id = self.variables.add(Variable {
                    name: capture.name,
                    owner_scope: lambda_scope_id,
                    type_id: field_type,
                    kind: VariableKind::Stack(defn_stmt),
                    flags: VariableFlags::empty(),
                    usage_count: 0,
                    defn_span: capture.span,
                });
                self.stmts.add_expected_id(
                    TypedStmt::Let(LetStmt {
                        variable_id,
                        variable_type: field_type,
                        initializer: Some(field_access),
                        span: capture.span,
                    }),
                    defn_stmt,
                );
                self.scopes.add_variable(lambda_scope_id, capture.name, variable_id);
                self.emit_ls_entity(capture.span, LsEntityKind::Variable { variable_id });
                prologue_stmts.push(defn_stmt);
                capture_bindings.push((variable_id, capture.span));
            }
            Some(ClosureSetup {
                environment_struct,
                environment_struct_type,
                environment_param,
                environment_param_variable_id,
                prologue_stmts,
                capture_bindings,
            })
        } else {
            None
        };

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
                kbail!(self, body_span, "Lambda returns incorrect type: {msg}");
            }
        }

        for param in param_variables.iter() {
            self.warn_variable_usage_counts("Lambda parameter", param.variable_id, param.span);
        }

        let return_type = match body_type {
            NEVER_TYPE_ID => expected_return_type.unwrap_or(NEVER_TYPE_ID),
            _ => body_type,
        };

        let enclosing_fn_name =
            self.scopes.nearest_parent_function(ctx.scope_id).map(|id| self.get_function(id).name);

        let name = self.build_ident_with(|k1, s| {
            k1.write_scope_path(s, ctx.scope_id, ".", true);
            s.push('.');
            if let Some(fn_name) = enclosing_fn_name {
                s.push_str(k1.ident_str(fn_name));
            };
            s.push_str("_lam_");
            write!(s, "{}", lambda_scope_id.as_u32()).unwrap();
        });

        // No captures: this is just a regular function
        if !is_closure {
            let function_type = self.add_type_anon(Type::Function(FunctionType {
                physical_params: typed_params.to_slice(),
                return_type,
                is_lambda: false,
            }));

            let body_function_id = self.functions.next_id();
            {
                let scope = self.scopes.get_scope_mut(lambda_scope_id);
                scope.scope_type = ScopeType::FunctionScope;
                scope.clear_nearest_lambda();
                self.scopes
                    .set_scope_owner_id(lambda_scope_id, ScopeOwnerId::Function(body_function_id));
            }

            for v in param_variables.iter() {
                self.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(body_function_id)
            }
            self.add_function(TypedFunction {
                name,
                scope: lambda_scope_id,
                namespace_id: self.scopes.nearest_parent_namespace(lambda_scope_id),
                params: param_variables.to_slice(),
                type_params: MSlice::empty(),
                fnlike_type_params: MSlice::empty(),
                ability_where_constraints: MSlice::empty(),
                body_block: Some(body_expr_id),
                builtin_type: None,
                linkage: Linkage::Standard,
                specialization_info: None,
                parsed_id: expr_id.into(),
                type_id: function_type,
                kind: TypedFunctionKind::Lambda,
                flags: TypedFunctionFlags::AddressTaken,
                dyn_fn_id: None,
                returned_variable: None,
                body_failure: None,
            });

            let function_pointer_type = self.add_function_pointer_type(function_type);
            let expr_id = self.exprs.add(
                TypedExpr::FunctionPointer(FunctionPointerExpr { function_id: body_function_id }),
                function_pointer_type,
                span,
            );
            return Ok(expr_id);
        }

        let ClosureSetup {
            environment_struct,
            environment_struct_type,
            environment_param,
            environment_param_variable_id,
            prologue_stmts,
            capture_bindings,
        } = closure_setup.unwrap();

        for (variable_id, capture_span) in capture_bindings.iter() {
            self.warn_variable_usage_counts("Captured variable", *variable_id, *capture_span);
        }

        typed_params.insert(0, environment_param);
        param_variables.insert(
            0,
            TypedFunctionParam { variable_id: environment_param_variable_id, span: body_span },
        );

        if let TypedExpr::Block(body) = self.exprs.get_mut(body_expr_id) {
            let mut new_stmts =
                self.mem.new_list(body.statements.len() + prologue_stmts.len() as u32);
            new_stmts.extend(&prologue_stmts);
            new_stmts.extend(self.mem.getn(body.statements));

            body.statements = new_stmts.to_slice();
        } else {
            panic!()
        }

        let body_function_id = self.functions.next_id();
        for v in param_variables.iter() {
            self.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(body_function_id)
        }

        let function_type = self.add_type_anon(Type::Function(FunctionType {
            physical_params: typed_params.to_slice(),
            return_type,
            is_lambda: true,
        }));

        let actual_body_function_id = self.add_function(TypedFunction {
            name,
            scope: lambda_scope_id,
            namespace_id: self.scopes.nearest_parent_namespace(lambda_scope_id),
            params: param_variables.to_slice(),
            type_params: MSlice::empty(),
            fnlike_type_params: MSlice::empty(),
            ability_where_constraints: MSlice::empty(),
            body_block: Some(body_expr_id),
            builtin_type: None,
            linkage: Linkage::Standard,
            specialization_info: None,
            parsed_id: expr_id.into(),
            type_id: function_type,
            kind: TypedFunctionKind::Lambda,
            flags: TypedFunctionFlags::empty(),
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        });
        debug_assert_eq!(actual_body_function_id, body_function_id);

        let lambda_type_id = self.add_lambda(
            function_type,
            environment_struct,
            environment_struct_type,
            body_function_id,
            expr_id.into(),
        );
        self.scopes.set_scope_owner_id(
            lambda_scope_id,
            ScopeOwnerId::Lambda(lambda_type_id, body_function_id, lambda_scope_id),
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
            ParsedExpr::Block(b) => *b,
            other_expr => {
                let block = parse::ParsedBlock {
                    span: other_expr.get_span(),
                    kind,
                    stmts: self
                        .ast
                        .mem
                        .pushn(&[self.ast.stmts.add(parse::ParsedStmt::LoneExpression(body))]),
                };
                block
            }
        }
    }

    fn eval_cast(
        &mut self,
        base_expr: ParsedExprId,
        target_type: TypeId,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let base_expr = self.eval_expr(base_expr, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get_type(base_expr);
        if base_expr_type == target_type {
            self.report(kwarn!(self, span, "type is already {}", target_type));
            return Ok(base_expr);
        }

        // float to int / int to float are the remaining non-noop casts handled by this syntax
        let cast_type = match self.types.get(base_expr_type) {
            Type::Integer(from_integer_type) => match self.types.get(target_type) {
                Type::Integer(_) => Err(kerr!(
                    self,
                    span,
                    "Cannot use .as to convert between integer types; use widen(), trunc(), signed(), or unsigned()",
                )),
                Type::Float(_to_float_type) => match from_integer_type {
                    IntegerType::U8 | IntegerType::U16 | IntegerType::U32 | IntegerType::U64 => {
                        Ok(CastType::IntegerUnsignedToFloat)
                    }
                    IntegerType::I8 | IntegerType::I16 | IntegerType::I32 | IntegerType::I64 => {
                        Ok(CastType::IntegerSignedToFloat)
                    }
                },
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast integer '{}' to '{}'",
                    from_integer_type,
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Float(_from_float_type) => match self.types.get(target_type) {
                Type::Integer(to_int_type) => {
                    if to_int_type.is_signed() {
                        Ok(CastType::FloatToSignedInteger)
                    } else {
                        Ok(CastType::FloatToUnsignedInteger)
                    }
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast float to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            _ => {
                let view = self.check_interpret_cast(base_expr_type, target_type, span, 0)?;
                if view.total {
                    Ok(CastType::ReferenceToReference)
                } else {
                    Err(kerr!(
                        self,
                        span,
                        "Viewing {} as {} makes a claim. Use `.narrow` instead",
                        base_expr_type,
                        target_type
                    ))
                }
            }
        }?;
        Ok(self.synth_cast(base_expr, target_type, cast_type, Some(span)))
    }

    fn eval_widen_trunc(
        &mut self,
        base_expr: ParsedExprId,
        target_type: TypeId,
        widen: bool,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let base_expr = self.eval_expr(base_expr, ctx.with_no_expected_type())?;
        let base_type = self.exprs.get_type(base_expr);
        let op = if widen { "widen" } else { "trunc" };
        if base_type == target_type {
            kbail!(
                self,
                span,
                "{} to the same type ({}) does nothing",
                op,
                self.type_id_to_string(target_type)
            );
        }
        enum Operand {
            Int(IntegerType),
            Float(NumericWidth),
            Other,
        }
        fn classify(t: &Type) -> Operand {
            match t {
                Type::Integer(i) => Operand::Int(*i),
                Type::Float(f) => Operand::Float(f.size()),
                _ => Operand::Other,
            }
        }
        let from = classify(self.types.get(base_type));
        let to = classify(self.types.get(target_type));
        let cast_type = match (from, to) {
            (Operand::Int(from), Operand::Int(to)) => match to.width().cmp(&from.width()) {
                Ordering::Greater => {
                    if !widen {
                        kbail!(self, span, "not a truncation",);
                    }
                    if from.is_signed() && !to.is_signed() {
                        kbail!(
                            self,
                            span,
                            "widen from {} to {} would lose the sign; Use .unsigned().widen[{}] or .widen[{}].unsigned()",
                            from,
                            to,
                            to,
                            to.sign_flipped()
                        );
                    }
                    if !from.is_signed() && to.is_signed() {
                        let widened = self.synth_cast(
                            base_expr,
                            target_type,
                            CastType::IntegerCast(IntegerCastDirection::Extend),
                            Some(span),
                        );
                        return Ok(self.synth_cast(
                            widened,
                            target_type,
                            CastType::IntegerCast(IntegerCastDirection::SignChange),
                            Some(span),
                        ));
                    }
                    CastType::IntegerCast(IntegerCastDirection::Extend)
                }
                Ordering::Less => {
                    if widen {
                        kbail!(self, span, "not a widen",);
                    }
                    CastType::IntegerCast(IntegerCastDirection::Truncate)
                }
                Ordering::Equal => {
                    let flip = if to.is_signed() { ".signed()" } else { ".unsigned()" };
                    kbail!(
                        self,
                        span,
                        "{} and {} are the same width; sign reinterpretation: {}",
                        from,
                        to,
                        flip
                    );
                }
            },
            (Operand::Float(from_size), Operand::Float(to_size)) => match to_size.cmp(&from_size) {
                Ordering::Greater => {
                    if !widen {
                        kbail!(self, span, "not a truncation");
                    }
                    CastType::FloatExtend
                }
                Ordering::Less => {
                    if widen {
                        kbail!(self, span, "not a widen");
                    }
                    CastType::FloatTruncate
                }
                Ordering::Equal => unreachable!("equal-width floats are the same type"),
            },
            _ => {
                kbail!(
                    self,
                    span,
                    "{} converts within integer types or within float types; cannot {} {} to {}",
                    op,
                    op,
                    self.type_id_to_string(base_type),
                    self.type_id_to_string(target_type)
                );
            }
        };
        Ok(self.synth_cast(base_expr, target_type, cast_type, Some(span)))
    }

    /// Checks if from_type can validly be viewed as to_type with zero operations applied.
    /// `total` means every bit pattern of the source is valid in the target; otherwise the view
    /// makes a claim about the source bits (only 0 and 1 for int to bool) and is spelled `.narrow`.
    /// A non-Exact extent changes the size, so it is only meaningful behind a reference; depth
    /// counts the references walked through and is used to reject non-exact extents at depth 0
    fn check_interpret_cast(
        &mut self,
        from_type: TypeId,
        to_type: TypeId,
        span: SpanId,
        depth: u32,
    ) -> K1Result<CastView> {
        #[inline(always)]
        fn succeed_if_indirect(
            k1: &TypedProgram,
            view: CastView,
            from_type: TypeId,
            to_type: TypeId,
            span: SpanId,
            depth: u32,
        ) -> K1Result<CastView> {
            if depth > 0 {
                Ok(view)
            } else {
                Err(kerr!(
                    k1,
                    span,
                    "{} as {} is only meaningful behind a reference",
                    from_type,
                    to_type
                ))
            }
        }
        if from_type == to_type {
            return Ok(CastView::TOTAL);
        }
        let view = match (self.types.get(from_type), self.types.get(to_type)) {
            (Type::Integer(i1), Type::Integer(i2)) if i1.width() == i2.width() => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }
            (Type::Integer(i), Type::Float(f)) if i.width() == f.size() => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }
            (Type::Float(f), Type::Integer(i)) if f.width() == i.width() => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }

            (Type::Integer(i), Type::Enum(e)) if *i == e.int_type => {
                succeed_if_indirect(self, CastView::CLAIMS, from_type, to_type, span, depth)
            }
            (Type::Enum(e), Type::Integer(i)) if *i == e.int_type => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }

            (Type::Bool, Type::Integer(i)) if i.width() == NumericWidth::B8 => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }
            (Type::Integer(i), Type::Bool) if i.width() == NumericWidth::B8 => {
                succeed_if_indirect(self, CastView::CLAIMS, from_type, to_type, span, depth)
            }
            (Type::Integer(i), Type::Char) if i.width() == NumericWidth::B8 => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }
            (Type::Char, Type::Integer(i)) if i.width() == NumericWidth::B8 => {
                succeed_if_indirect(self, CastView::TOTAL, from_type, to_type, span, depth)
            }
            (Type::Reference(_), Type::Pointer) => Ok(CastView::TOTAL),
            (Type::Pointer, Type::Reference(_)) => Ok(CastView::CLAIMS),
            (Type::Pointer, Type::FunctionPointer(_)) => Ok(CastView::CLAIMS),
            (Type::Reference(from_r), Type::Reference(to_r)) => {
                let inner =
                    self.check_interpret_cast(from_r.inner_type, to_r.inner_type, span, depth + 1)?;
                Ok(CastView { total: inner.total, extent: ViewExtent::Exact })
            }
            (Type::Struct(_), _) | (_, Type::Struct(_)) => {
                self.check_struct_view(from_type, to_type, span, depth)
            }
            (Type::Opaque(from_o), Type::Opaque(to_o)) => {
                if to_o.size > from_o.size {
                    kbail!(
                        self,
                        span,
                        "cannot view {} as {}: the target is larger ({} vs {} bytes)",
                        from_type,
                        to_type,
                        to_o.size,
                        from_o.size
                    )
                }
                if to_o.align > from_o.align {
                    kbail!(
                        self,
                        span,
                        "cannot view {} as {}: the target has stricter alignment ({} vs {})",
                        from_type,
                        to_type,
                        to_o.align,
                        from_o.align
                    )
                }
                let extent = if to_o.size < from_o.size {
                    ViewExtent::ReadsPrefix
                } else {
                    ViewExtent::Exact
                };
                Ok(CastView { total: false, extent })
            }
            (Type::FunctionPointer(_), Type::Pointer) => Ok(CastView::TOTAL),
            (Type::FunctionPointer(fp_from), Type::FunctionPointer(fp_to)) => {
                let fn_type_from = *self.types.get(fp_from.function_type_id).as_function().unwrap();
                let fn_type_to = *self.types.get(fp_to.function_type_id).as_function().unwrap();
                if fn_type_from.physical_params.len() != fn_type_to.physical_params.len() {
                    kbail!(
                        self,
                        span,
                        "Incompatible function types; wrong param count. {} vs {}",
                        fn_type_from.physical_params.len(),
                        fn_type_to.physical_params.len()
                    )
                };
                // Consider VARIANCE on params / ret type. functions CONSUME params, so
                // contravariance, produce return types, so covariance

                let mut total = true;
                for (param_from, param_to) in
                    self.mem.getn_zip(fn_type_from.physical_params, fn_type_to.physical_params)
                {
                    // depth is 0 since these values are essentially being directly abi-converted
                    // consuming these values: to, from
                    let param_view = self
                        .check_interpret_cast(param_to.type_id, param_from.type_id, span, 0)
                        .map_err(|err| {
                            kerr!(
                                self,
                                span,
                                "incompatible parameter type for cast: {}",
                                err.message
                            )
                        })?;
                    total = total && param_view.total;
                }

                // producing these values: from, to
                match self.check_interpret_cast(
                    fn_type_from.return_type,
                    fn_type_to.return_type,
                    span,
                    0,
                ) {
                    Err(err) => {
                        kbail!(
                            self,
                            span,
                            "incompatible return types for function cast: {}",
                            err.message
                        )
                    }
                    Ok(ret_view) => {
                        total = total && ret_view.total;
                    }
                }
                Ok(CastView { total, extent: ViewExtent::Exact })
            }
            _ => Err(kerr!(self, span, "cannot reinterpret from {} to {}", from_type, to_type)),
        }?;
        if depth == 0 && view.extent != ViewExtent::Exact {
            let how = match view.extent {
                ViewExtent::ReadsPrefix => "reads a prefix of",
                ViewExtent::ReadsBeyond => "extends past",
                ViewExtent::Exact => unreachable!(),
            };
            kbail!(
                self,
                span,
                "cannot view {} as {}: the view {} the source, so it is only meaningful behind a reference",
                from_type,
                to_type,
                how
            )
        }
        Ok(view)
    }

    /// The struct case of check_interpret_cast: pairwise fields when both sides are structs,
    /// else one lazy step along the first-field chain (the types living at offset 0).
    /// Descending into from's first field reads a prefix of from; wrapping from as to's first
    /// field reads beyond it
    fn check_struct_view(
        &mut self,
        from_type: TypeId,
        to_type: TypeId,
        span: SpanId,
        depth: u32,
    ) -> K1Result<CastView> {
        fn struct_fields(
            k1: &TypedProgram,
            t: TypeId,
        ) -> Option<MSlice<StructTypeField, TypedProgram>> {
            match k1.types.get(t) {
                Type::Struct(s) => Some(s.fields),
                _ => None,
            }
        }
        let from_fields = struct_fields(self, from_type);
        let to_fields = struct_fields(self, to_type);

        let pairing_err = match (from_fields, to_fields) {
            (Some(ff), Some(tf)) => {
                match self.check_struct_field_pairing(from_type, to_type, ff, tf, span, depth) {
                    Ok(view) => return Ok(view),
                    Err(e) => Some(e),
                }
            }
            _ => None,
        };

        let mut chain_err = None;
        if let Some(ff) = from_fields {
            let fields = self.mem.getn(ff);
            if let Some(first) = fields.first() {
                let single = fields.len() == 1;
                match self.check_interpret_cast(first.type_id, to_type, span, depth) {
                    Ok(inner) => match inner.extent {
                        ViewExtent::Exact | ViewExtent::ReadsPrefix => {
                            return Ok(CastView {
                                total: inner.total,
                                extent: ViewExtent::ReadsPrefix,
                            });
                        }
                        ViewExtent::ReadsBeyond if single => {
                            return Ok(CastView {
                                total: inner.total,
                                extent: ViewExtent::ReadsBeyond,
                            });
                        }
                        ViewExtent::ReadsBeyond => {}
                    },
                    Err(e) => chain_err = Some(e),
                }
            }
        }

        if let Some(tf) = to_fields {
            let fields = self.mem.getn(tf);
            if let Some(first) = fields.first() {
                let single = fields.len() == 1;
                match self.check_interpret_cast(from_type, first.type_id, span, depth) {
                    Ok(inner) => match inner.extent {
                        ViewExtent::Exact | ViewExtent::ReadsBeyond => {
                            return Ok(CastView {
                                total: inner.total && single,
                                extent: ViewExtent::ReadsBeyond,
                            });
                        }
                        ViewExtent::ReadsPrefix => {}
                    },
                    Err(e) => {
                        if chain_err.is_none() {
                            chain_err = Some(e)
                        }
                    }
                }
            }
        }

        match (pairing_err, chain_err) {
            (Some(e), _) => Err(e),
            (None, Some(e)) => {
                Err(kerr!(self, span, "cannot view {} as {}: {}", from_type, to_type, e.message))
            }
            (None, None) => Err(kerr!(self, span, "cannot view {} as {}", from_type, to_type)),
        }
    }

    fn check_struct_field_pairing(
        &mut self,
        from_type: TypeId,
        to_type: TypeId,
        from_fields: MSlice<StructTypeField, TypedProgram>,
        to_fields: MSlice<StructTypeField, TypedProgram>,
        span: SpanId,
        depth: u32,
    ) -> K1Result<CastView> {
        let from_fields = self.mem.getn(from_fields);
        let to_fields = self.mem.getn(to_fields);
        if to_fields.len() > from_fields.len() {
            kbail!(
                self,
                span,
                "cannot view {} as {}: the target has more fields ({} vs {})",
                from_type,
                to_type,
                to_fields.len(),
                from_fields.len()
            )
        }
        let mut total = true;
        let mut last_extent = ViewExtent::Exact;
        for (index, to_field) in to_fields.iter().enumerate() {
            let from_field = &from_fields[index];
            let inner = match self.check_interpret_cast(
                from_field.type_id,
                to_field.type_id,
                span,
                depth,
            ) {
                Err(e) => kbail!(
                    self,
                    span,
                    "cannot view {} as {}: field {} is incompatible: {}",
                    from_type,
                    to_type,
                    from_field.name,
                    e.message
                ),
                Ok(view) => view,
            };
            match inner.extent {
                ViewExtent::Exact => {}
                ViewExtent::ReadsPrefix => {
                    if index != to_fields.len() - 1 {
                        kbail!(
                            self,
                            span,
                            "cannot view {} as {}: field {} is a prefix view, which shifts the offsets of the fields after it; only the final field can be a prefix",
                            from_type,
                            to_type,
                            from_field.name
                        )
                    }
                    last_extent = ViewExtent::ReadsPrefix;
                }
                ViewExtent::ReadsBeyond => kbail!(
                    self,
                    span,
                    "cannot view {} as {}: field {} would extend past the source field",
                    from_type,
                    to_type,
                    from_field.name
                ),
            }
            total = total && inner.total;
        }
        let extent =
            if to_fields.len() < from_fields.len() || last_extent == ViewExtent::ReadsPrefix {
                ViewExtent::ReadsPrefix
            } else {
                ViewExtent::Exact
            };
        Ok(CastView { total, extent })
    }

    fn eval_cast_narrow(
        &mut self,
        base_expr: ParsedExprId,
        target_type: TypeId,
        span: SpanId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let base_expr = self.eval_expr(base_expr, ctx.with_no_expected_type())?;
        let base_expr_type = self.exprs.get_type(base_expr);

        match self.check_interpret_cast(base_expr_type, target_type, span, 0) {
            Err(e) => Err(kerr!(
                self,
                span,
                "cannot narrow {} to {}: {}",
                base_expr_type,
                target_type,
                e.message,
            )),
            Ok(view) => {
                if view.total {
                    self.report(kwarn!(self, span, "This cast is total; use .as instead"));
                }
                Ok(self.synth_cast(
                    base_expr,
                    target_type,
                    CastType::ReferenceToReference,
                    Some(span),
                ))
            }
        }
    }

    fn eval_for_expr(&mut self, for_expr: &ForExpr, ctx: EvalExprContext) -> K1Result<TypedExprId> {
        // Basically no overlap here in what we need to do.
        if for_expr.is_static {
            return self.eval_static_for_expr(for_expr, ctx);
        };
        let binding_ident = match for_expr.binding {
            None => self.ast.idents.b.it,
            Some(b) => self.ast.exprs.get(b).expect_variable().name.name,
        };
        let binding_span = match for_expr.binding {
            None => for_expr.span,
            Some(binding_expr) => self.ast.exprs.get_span(binding_expr),
        };
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, ctx.with_no_expected_type())?;
        let iterable_type = self.exprs.get_type(iterable_expr);
        let iterable_span = self.exprs.get_span(iterable_expr);
        let body_span = for_expr.body_block.span;

        // Project: Kill all this with the macro system
        let (target_is_iterator, self_adjust) = match self.expect_ability_impl(
            iterable_type,
            ABILITY_ID_ITERABLE,
            true,
            ctx.scope_id,
            iterable_span,
        ) {
            Err(_not_iterable) => {
                match self.expect_ability_impl(
                    iterable_type,
                    ABILITY_ID_ITERATOR,
                    true,
                    ctx.scope_id,
                    iterable_span,
                ) {
                    Err(_not_iterator) => {
                        kbail!(
                            self,
                            iterable_span,
                            "for loop target {} must be Iterable or an Iterator",
                            iterable_type
                        );
                    }
                    Ok((_iterator_impl, adjust)) => (true, adjust),
                }
            }
            Ok((_iterable_impl, adjust)) => (false, adjust),
        };

        // We de-sugar the 'for ... do' expr into a typed while loop, synthesizing
        // a few local variables in order to achieve this.

        let outer_for_expr_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, ScopeOwnerId::None);

        let initial_index_expr = self.synth_i64(-1, for_expr.body_block.span);
        let index_variable = self.synth_variable_defn(
            self.ast.idents.b.it_index,
            initial_index_expr,
            true,
            outer_for_expr_scope,
            Some(iterable_span),
        );
        let mut iterable_defn_stmt: Option<TypedStmtId> = None;
        let coerced_iterable_expr = match self_adjust {
            SelfAdjust::None => iterable_expr,
            SelfAdjust::Deref => self.synth_dereference(iterable_expr),
            SelfAdjust::AddrOf => match self.synth_address_of(iterable_expr, SpanId::NONE, true) {
                Ok(addr) => addr,
                Err(_) => {
                    // The iterable is an rvalue (e.g. a call result); bind it to a
                    // synthetic variable so we have a place to point at
                    let iterable_variable = self.synth_variable_defn(
                        self.ast.idents.b.iterable_var,
                        iterable_expr,
                        false,
                        outer_for_expr_scope,
                        Some(iterable_span),
                    );
                    iterable_defn_stmt = Some(iterable_variable.defn_stmt);
                    self.synth_address_of(iterable_variable.variable_expr, SpanId::NONE, true)?
                }
            },
        };
        let iterator_initializer = if target_is_iterator {
            coerced_iterable_expr
        } else {
            self.synth_typed_call_typed_args(
                self.ast.idents.f.Iterable_iterator.with_span(iterable_span),
                &[],
                &[coerced_iterable_expr],
                ctx.with_scope(outer_for_expr_scope).with_no_expected_type(),
                false,
            )?
        };
        let iterator_variable = self.synth_variable_defn(
            self.ast.idents.b.iter,
            iterator_initializer,
            false,
            outer_for_expr_scope,
            None,
        );
        let iterator_expr =
            self.synth_address_of(iterator_variable.variable_expr, SpanId::NONE, true)?;
        let mut loop_block =
            self.new_block_builder(outer_for_expr_scope, ScopeType::WhileLoopBody, body_span, 3);
        let loop_scope_id = loop_block.scope_id;
        self.scopes.add_loop_info(
            loop_scope_id,
            ScopeLoopInfo { break_type: Some(self.builtin_types.empty), label: for_expr.label },
        );

        let mut consequent_block =
            self.new_block_builder(loop_scope_id, ScopeType::LexicalBlock, iterable_span, 3);

        let loop_scope_ctx = ctx.with_scope(loop_scope_id).with_no_expected_type();
        let iterator_next_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.Iterator_next.with_span(iterable_span),
            &[],
            &[iterator_expr],
            loop_scope_ctx,
            false,
        )?;
        let next_variable = self.synth_variable_defn_simple(
            self.ast.idents.b.next,
            iterator_next_call,
            loop_scope_id,
        );
        // The is-some check below guards this arm, so project the payload
        // directly; try/get-value's crash arm would survive as reachable code
        let next_type = self.exprs.get_type(next_variable.variable_expr);
        let next_sum_type = self.types.get(next_type).expect_sum();
        let some_variant = self.sum_variant_by_index(next_sum_type.variants, 1);
        let some_payload_type = some_variant.payload.unwrap();
        let next_get_payload = self.exprs.add(
            TypedExpr::SumGetPayload(GetSumPayload {
                sum_expr: next_variable.variable_expr,
                variant_index: 1,
                packed: false,
            }),
            some_payload_type,
            iterable_span,
        );
        let binding_variable = self.synth_variable_defn_visible(
            binding_ident,
            next_get_payload,
            consequent_block.scope_id,
            binding_span,
        );
        let body_block = self.eval_block(
            &for_expr.body_block,
            ctx.with_scope(consequent_block.scope_id).with_no_expected_type(),
            false,
        )?;

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

        self.push_block_stmt_id(&mut loop_block, next_variable.defn_stmt); // let next = iter.next();

        consequent_block.statements.push(binding_variable.defn_stmt);
        self.push_block_expr_id(&mut consequent_block, body_block);

        let next_is_some_call = self.synth_sum_is_variant(next_variable.variable_expr, 1, None)?;
        let empty_break = self.synth_empty_value(body_span);
        let break_expr = self.exprs.add(
            TypedExpr::Break(TypedBreak { value: empty_break, loop_scope: loop_scope_id }),
            NEVER_TYPE_ID,
            body_span,
        );
        let consequent_type = self.exprs.get_type(body_block);
        let consequent_block_id = self.exprs.add_block(consequent_block, consequent_type);
        let if_next_loop_else_break_expr = self.synth_if_else(
            self.builtin_types.empty,
            next_is_some_call,
            consequent_block_id,
            break_expr,
            body_span,
        );
        self.push_block_expr_id(&mut loop_block, if_next_loop_else_break_expr);

        let body_block = self.exprs.add_block(loop_block, self.builtin_types.empty);
        let loop_expr = self.exprs.add(
            TypedExpr::LoopExpr(LoopExpr { body_block }),
            self.builtin_types.empty,
            for_expr.span,
        );

        let mut for_expr_initial_statements = self.mem.new_list(4);
        if let Some(iterable_defn_stmt) = iterable_defn_stmt {
            for_expr_initial_statements.push(iterable_defn_stmt);
        }
        for_expr_initial_statements.push(index_variable.defn_stmt);
        for_expr_initial_statements.push(iterator_variable.defn_stmt);
        let loop_stmt_id = self.add_expr_stmt(loop_expr);
        for_expr_initial_statements.push(loop_stmt_id);

        let final_type =
            self.get_stmt_type(*for_expr_initial_statements.as_slice().last().unwrap());
        let final_expr = self.exprs.add_block(
            BlockBuilder {
                scope_id: outer_for_expr_scope,
                statements: for_expr_initial_statements,
                span: for_expr.body_block.span,
            },
            final_type,
        );
        Ok(final_expr)
    }

    // FIXME: Can we remove this in light of new 'macro'? Not quite; inference isn't quite there
    fn eval_static_for_expr(
        &mut self,
        for_expr: &ForExpr,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        if for_expr.label.is_some() {
            kbail!(self, for_expr.span, "#for unrolls at compile time; it has no loop to label");
        }
        let iteree_value_id =
            self.execute_static_expr(for_expr.iterable_expr, ctx.with_no_expected_type(), &[])?;
        let iteree_span = self.ast.get_expr_span(for_expr.iterable_expr);
        let Some(element_count) = self.static_container_len(iteree_value_id) else {
            kbail!(
                self,
                iteree_span,
                "Expected something iterable; got: {}",
                self.static_value_to_string(iteree_value_id)
            );
        };

        let mut block = self.new_block_builder(
            ctx.scope_id,
            ScopeType::LexicalBlock,
            for_expr.span,
            element_count as u32 * 2,
        );
        let binding_name = match for_expr.binding {
            None => self.ast.idents.b.it,
            Some(binding) => self.ast.exprs.get(binding).expect_variable().name.name,
        };
        let binding_span = match for_expr.binding {
            None => for_expr.span,
            Some(binding_expr) => self.ast.exprs.get_span(binding_expr),
        };
        let eval_context = ctx.with_scope(block.scope_id).with_no_expected_type();
        for index in 0..element_count {
            let elem = self.static_container_element(iteree_value_id, index);
            let elem_expr = self.add_static_constant_expr(elem, iteree_span);
            let v = self.synth_variable_defn_visible(
                binding_name,
                elem_expr,
                block.scope_id,
                binding_span,
            );
            let user_expr = self.eval_block(&for_expr.body_block, eval_context, false)?;
            self.push_block_stmt_id(&mut block, v.defn_stmt);
            self.push_block_expr_id(&mut block, user_expr);
        }
        let block_id = self.exprs.add_block(block, self.builtin_types.empty);

        Ok(block_id)
    }

    fn expect_ability_impl(
        &mut self,
        type_id: TypeId,
        base_ability_id: AbilityId,
        allow_self_adjust: bool,
        scope_id: ScopeId,
        span_for_error: SpanId,
    ) -> K1Result<(AbilityImplHandle, SelfAdjust)> {
        self.find_or_generate_ability_impl_for_type(
            type_id,
            base_ability_id,
            &[],
            allow_self_adjust,
            scope_id,
            span_for_error,
        )
        .map_err(|msg| {
            let handles =
                self.ability_impl_table.get(&type_id).map(|l| l.as_slice(&self.mem)).unwrap_or(&[]);
            let mut implemented = String::new();
            for (idx, h) in handles.iter().enumerate() {
                if idx > 0 {
                    implemented.push('\n');
                }
                implemented.push_str(&self.ability_impl_signature_to_string(
                    h.specialized_ability_id,
                    self.ability_impls.get(h.full_impl_id).impl_arguments,
                ));
            }
            kerr!(
                self,
                span_for_error,
                "Missing ability '{}' for '{}': {msg}. It implements the following abilities:\n{}",
                self.abilities.get(base_ability_id).name,
                type_id,
                implemented
            )
        })
    }

    fn eval_static_if_expr(
        &mut self,
        if_expr: &ParsedIfExpr,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        if ctx.is_generic_pass() {
            let filler_expr =
                self.synth_phony(ctx.expected_type_id.unwrap_or(EMPTY_TYPE_ID), if_expr.span);
            return Ok(filler_expr);
        }
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
            if let Some(alt) = alt_expr { alt } else { self.synth_empty_value(if_expr.span) }
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
    ) -> K1Result<TypedExprId> {
        if if_expr.is_static {
            return self.eval_static_if_expr(if_expr, ctx);
        }
        let cond_ctx = if self.matching_condition_binds(if_expr.cond) {
            let condition_scope_id = self.scopes.add_child_scope(
                ctx.scope_id,
                ScopeType::LexicalBlock,
                ScopeOwnerId::None,
            );
            ctx.with_scope(condition_scope_id)
        } else {
            ctx
        };

        let condition_or_block =
            self.eval_matching_condition(if_expr.cond, None, cond_ctx.with_no_expected_type())?;
        let condition = match condition_or_block {
            MatchingConditionResult::MatchingCondition(mc) => mc,
            MatchingConditionResult::NeverBlock(never_block) => return Ok(never_block),
        };

        let consequent = self.eval_expr(if_expr.cons, cond_ctx)?;
        let consequent_original_type = self.exprs.get_type(consequent);

        let cons_original_never = consequent_original_type == NEVER_TYPE_ID;

        // if without else:
        // If there is no alternate, we coerce the consequent to return Unit, so both
        // branches have a matching type, making codegen simpler
        // However, if the consequent is a never type (does not return), we don't need to do this, in
        // fact we can't because then we'd have an expression following a never expression
        let consequent = if if_expr.alt.is_none()
            && !cons_original_never
            && consequent_original_type != self.builtin_types.empty
        {
            self.synth_discard_call(consequent, ctx.with_no_expected_type())?
        } else {
            consequent
        };

        let consequent_type = self.exprs.get_type(consequent);
        let cons_never = consequent_type == NEVER_TYPE_ID;

        let alternate = if let Some(parsed_alt) = if_expr.alt {
            let type_hint = if cons_never {
                ctx.expected_type_id
            } else {
                // We chase down the type because, if its a static, it doesn't really make
                // sense to expect every arm to evaluate to the same static, but rather to
                // the static's inner type
                let consequent_type_chased = self.get_static_family_id_if_static(consequent_type);
                Some(consequent_type_chased)
            };
            self.eval_expr(parsed_alt, ctx.with_expected_type(type_hint))?
        } else {
            self.synth_empty_value(if_expr.span)
        };
        let alternate_type = self.exprs.get_type(alternate);
        let alternate_span = self.exprs.get_span(alternate);

        let alt_never = alternate_type == NEVER_TYPE_ID;
        let no_never = !cons_never && !alt_never;

        let overall_type = if no_never {
            consequent_type
        } else {
            if cons_never { alternate_type } else { consequent_type }
        };

        let alternate = if no_never {
            self.check_and_coerce_expr(consequent_type, alternate, ctx.scope_id, false).map_err(
                |e| {
                    kerr!(
                        self,
                        alternate_span,
                        "else branch type did not match then branch type: {}",
                        e.message,
                    )
                },
            )?
        } else {
            alternate
        };

        let cons_arm = TypedMatchArm { case: None, condition, consequent_expr: consequent };
        let alt_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: alternate,
        };
        Ok(self.exprs.add(
            TypedExpr::Match(TypedMatchExpr {
                subject_defn: None,
                scrutinee: None,
                arms: self.mem.pushn(&[cons_arm, alt_arm]),
            }),
            overall_type,
            if_expr.span,
        ))
    }

    fn eval_matching_condition(
        &mut self,
        condition: ParsedExprId,
        check_exhaustive: Option<(bool, &mut Option<K1Message>)>,
        ctx: EvalExprContext,
    ) -> K1Result<MatchingConditionResult> {
        debug!("matching condition: {}", self.ast.expr_id_to_string(condition));
        let mut all_patterns: SV4<(TypedPatternId, TypedExprId)> = smallvec![];
        let mut allow_bindings: bool = true;
        let mut instrs: List<MatchingConditionInstr, _> = self.mem.new_list(2);
        let condition_span = self.ast.get_expr_span(condition);
        // If there are no boolean conditions, we can check for infallibility
        let mut is_single_pattern_only = true;
        self.handle_matching_condition_rec(
            condition,
            &mut allow_bindings,
            &mut all_patterns,
            &mut instrs,
            &mut is_single_pattern_only,
            ctx,
        )?;

        let mut all_bindings: SmallVec<[VariablePattern; 8]> = smallvec![];
        for (pattern, _) in all_patterns.iter() {
            self.patterns.get_pattern_bindings_rec(*pattern, &mut all_bindings);
        }
        if allow_bindings {
            // Bindings are allowed, fail if there are any duplicates
            all_bindings.sort_by_key(|p| p.name);
            let dupe_binding =
                all_bindings.windows(2).find(|window| window[0].name == window[1].name);
            if let Some(dupe_binding) = dupe_binding {
                kbail!(
                    self,
                    dupe_binding[1].span,
                    "Duplicate binding of name '{}' within same matching if; normally we like shadowing but this is probably never good.",
                    dupe_binding[1].name
                );
            }
        } else {
            // Bindings are disallowed due to the structure of the expressions
            // but there is at least one binding
            if let Some(b) = all_bindings.first() {
                kbail!(
                    self,
                    b.span,
                    "Cannot create bindings unless all patterns are connected by 'and'"
                );
            }
        }

        if let Some((skip_message, exhaustive_out)) = check_exhaustive {
            if is_single_pattern_only {
                let (pattern_id, subject_expr) = all_patterns[0];
                let subject_span = self.exprs.get_span(subject_expr);
                let subject_type = self.exprs.get_type(subject_expr);
                let nonexhaustive_message = self
                    .check_pattern_exhaustiveness(
                        subject_type,
                        &[pattern_id],
                        subject_span,
                        skip_message,
                    )
                    .err();
                *exhaustive_out = nonexhaustive_message;
            } else {
                *exhaustive_out = Some(K1Message {
                    message: self.ast.idents.intern(""),
                    span: condition_span,
                    error_kind: ErrorKind::TypeError,
                    level: MessageLevel::Error,
                })
            }
        }

        let diverges_at = self.matching_condition_diverges(&instrs);
        if let Some(diverge_index) = diverges_at {
            let never_block = self.make_never_condition_block(
                &instrs[0..=diverge_index],
                ctx.scope_id,
                condition_span,
            );
            Ok(MatchingConditionResult::NeverBlock(never_block))
        } else {
            Ok(MatchingConditionResult::MatchingCondition(MatchingCondition {
                instrs: instrs.to_slice_trim(&mut self.mem),
            }))
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
                MatchingConditionInstr::IntEquals { subject, .. } => {
                    if self.exprs.get_type(*subject) == NEVER_TYPE_ID {
                        return Some(index);
                    }
                }
            }
        }
        None
    }

    fn matching_condition_binds(&self, parsed_expr_id: ParsedExprId) -> bool {
        match self.ast.exprs.get(parsed_expr_id) {
            ParsedExpr::Is(_) => true,
            ParsedExpr::BinaryOp(b) if b.op_kind == BinaryOpKind::And => {
                self.matching_condition_binds(b.lhs) || self.matching_condition_binds(b.rhs)
            }
            _ => false,
        }
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
        all_patterns: &mut SV4<(TypedPatternId, TypedExprId)>,
        instrs: &mut List<MatchingConditionInstr, TypedProgram>,
        is_single_pattern_only: &mut bool,
        ctx: EvalExprContext,
    ) -> K1Result<()> {
        debug!("hmirec: {}", self.ast.expr_id_to_string(parsed_expr_id));
        match self.ast.exprs.get(parsed_expr_id) {
            ParsedExpr::Is(is_expr) => {
                let target_expr = is_expr.target_expression;
                let pattern = is_expr.pattern;
                let subject = self.eval_expr(target_expr, ctx)?;
                let subject_type = self.exprs.get_type(subject);
                let subject_var = self.synth_variable_defn_simple(
                    self.ast.idents.b.if_target,
                    subject,
                    ctx.scope_id,
                );
                let pattern = self.compile_pattern_to_type(
                    pattern,
                    subject_type,
                    ctx.scope_id,
                    *allow_bindings,
                )?;
                instrs.push_grow(
                    &mut self.mem,
                    MatchingConditionInstr::Binding { let_stmt: subject_var.defn_stmt },
                );
                self.compile_pattern_into_values(
                    pattern,
                    subject_var.variable_expr,
                    instrs,
                    false,
                    false,
                    ctx,
                )?;
                all_patterns.push((pattern, subject));

                Ok(())
            }
            ParsedExpr::BinaryOp(binary_op) if binary_op.op_kind == BinaryOpKind::And => {
                *is_single_pattern_only = false;
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
                    is_single_pattern_only,
                    ctx,
                )?;
                self.handle_matching_condition_rec(
                    rhs,
                    allow_bindings,
                    all_patterns,
                    instrs,
                    is_single_pattern_only,
                    ctx,
                )?;
                Ok(())
            }
            other => {
                *is_single_pattern_only = false;
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
                    kbail!(self, span, "Expected boolean condition: {msg}");
                };
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(condition));
                Ok(())
            }
        }
    }

    fn eval_standalone_matching_condition(
        &mut self,
        expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let condition_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, ScopeOwnerId::None);
        let condition_ctx = ctx.with_scope(condition_scope).with_no_expected_type();
        let matching_condition = match self.eval_matching_condition(expr_id, None, condition_ctx)? {
            MatchingConditionResult::MatchingCondition(mc) => mc,
            MatchingConditionResult::NeverBlock(never_block) => return Ok(never_block),
        };
        let span = self.ast.exprs.get_span(expr_id);
        let true_arm = TypedMatchArm {
            case: None,
            condition: matching_condition,
            consequent_expr: self.synth_bool(true, span),
        };
        let false_arm = TypedMatchArm {
            case: None,
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: self.synth_bool(false, span),
        };
        let match_expr = TypedExpr::Match(TypedMatchExpr {
            subject_defn: None,
            scrutinee: None,
            arms: self.mem.pushn(&[true_arm, false_arm]),
        });
        Ok(self.exprs.add(match_expr, BOOL_TYPE_ID, span))
    }

    fn eval_binary_op(
        &mut self,
        binary_op_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let ParsedExpr::BinaryOp(binary_op) = *self.ast.exprs.get(binary_op_id) else {
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
            | K::GreaterEqual
            | K::BitAnd
            | K::BitOr
            | K::BitXor
            | K::BitShiftLeft
            | K::BitShiftRight => {
                let fn_ident = match binary_op.op_kind {
                    K::Add => self.ast.idents.f.add__add,
                    K::Subtract => self.ast.idents.f.sub__sub,
                    K::Multiply => self.ast.idents.f.mul__mul,
                    K::Divide => self.ast.idents.f.div__div,
                    K::Rem => self.ast.idents.f.rem__rem,
                    K::Less => self.ast.idents.f.ScalarCmp_lt,
                    K::LessEqual => self.ast.idents.f.ScalarCmp_le,
                    K::Greater => self.ast.idents.f.ScalarCmp_gt,
                    K::GreaterEqual => self.ast.idents.f.ScalarCmp_ge,
                    K::BitAnd => self.ast.idents.f.bitwise_and,
                    K::BitOr => self.ast.idents.f.bitwise_or,
                    K::BitXor => self.ast.idents.f.bitwise_xor,
                    K::BitShiftLeft => self.ast.idents.f.bitwise_shl,
                    K::BitShiftRight => self.ast.idents.f.bitwise_shr,
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
    ) -> K1Result<TypedExprId> {
        // LHS must implement Try and RHS must be its contained type
        let lhs = self.eval_expr(lhs, ctx.with_no_expected_type())?;
        let lhs_type = self.exprs.get_type(lhs);
        let (try_impl, _) = self
            .expect_ability_impl(lhs_type, ABILITY_ID_TRY, true, ctx.scope_id, span)
            .map_err(|e| {
                kerr!(
                    self,
                    span,
                    "'?' operator can only be used on a type that implements `Try`. {}",
                    e.message,
                )
            })?;
        let try_impl = self.ability_impls.get(try_impl.full_impl_id);
        let output_type = *self.mem.get_nth(try_impl.impl_arguments, 0);

        let rhs = self.eval_expr(rhs, ctx.with_expected_type(Some(output_type)))?;
        let rhs_type = self.exprs.get_type(rhs);
        if let Err(msg) = self.check_types(output_type, rhs_type, ctx.scope_id) {
            kbail!(self, span, "RHS value incompatible with `Try` output of LHS: {}", msg);
        }
        let mut coalesce_block =
            self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 2);
        let lhs_variable = self.synth_variable_defn_simple(
            self.ast.idents.b.optelse_lhs,
            lhs,
            coalesce_block.scope_id,
        );
        let coalesce_ctx = ctx.with_scope(coalesce_block.scope_id).with_no_expected_type();
        let lhs_has_value = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__is_ok.with_span(span),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
            false,
        )?;
        let lhs_get_expr = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__get_value.with_span(span),
            &[],
            &[lhs_variable.variable_expr],
            coalesce_ctx,
            false,
        )?;

        let if_else = self.synth_if_else(output_type, lhs_has_value, lhs_get_expr, rhs, span);
        self.push_block_stmt_id(&mut coalesce_block, lhs_variable.defn_stmt);
        self.push_block_expr_id(&mut coalesce_block, if_else);
        Ok(self.exprs.add_block(coalesce_block, output_type))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let ParsedExpr::BinaryOp(binary_op) = *self.ast.exprs.get(binary_op_id) else {
            unreachable!()
        };

        let parsed_equals_call = self.synth_parsed_function_call(
            self.ast.idents.f.equals__equals.with_span(binary_op.span),
            &[],
            &[binary_op.lhs, binary_op.rhs],
            false,
        );
        let call = *self.ast.exprs.get(parsed_equals_call).expect_call();
        let equality_result =
            self.eval_function_call(&call, None, ctx.with_expected_type(Some(BOOL_TYPE_ID)), None)?;
        let final_result = match binary_op.op_kind {
            BinaryOpKind::Equals => equality_result,
            BinaryOpKind::NotEquals => self.synth_typed_call_typed_args(
                self.ast.idents.f.neg__negated.with_span(binary_op.span),
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
    ) -> K1Result<TypedExprId> {
        let new_fn_call = match self.ast.exprs.get(rhs) {
            ParsedExpr::Variable(var) => {
                let args = self.ast.mem.pushn(&[ParsedCallArg::unnamed(lhs)]);
                ParsedCall {
                    name: var.name,
                    type_args: MSlice::empty(),
                    args,
                    span,
                    is_method: false,
                    id: ParsedExprId::PENDING,
                }
            }
            ParsedExpr::Call(fn_call) => {
                let mut args_with_piped = self.ast.mem.new_list(fn_call.args.len() + 1);
                args_with_piped.push(ParsedCallArg::unnamed(lhs));
                args_with_piped.extend(self.ast.mem.getn(fn_call.args));
                let args_with_piped_h = args_with_piped.to_slice();
                ParsedCall {
                    name: fn_call.name,
                    type_args: fn_call.type_args,
                    args: args_with_piped_h,
                    span,
                    is_method: false,
                    id: ParsedExprId::PENDING,
                }
            }
            _ => {
                kbail!(
                    self,
                    self.ast.exprs.get_span(rhs),
                    "rhs of pipe must be function call or function name",
                );
            }
        };
        let new_fn_call_id = self.ast.exprs.add(ParsedExpr::Call(new_fn_call));
        let new_fn_call_clone = *self.ast.exprs.get(new_fn_call_id).expect_call();
        self.eval_function_call(&new_fn_call_clone, None, ctx, None)
    }

    /// Can 'shortcircuit' with Left if the function call to resolve
    /// is actually a builtin
    fn resolve_parsed_call(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        stashed_args: &mut SV8<(ParsedExprId, TypedExprId)>,
    ) -> K1Result<CallResolution> {
        let call_span = fn_call.span;
        if let Some(builtin_result) = self.handle_builtin_function_call_lookalikes(fn_call, ctx)? {
            return Ok(CallResolution::OtherExpr(builtin_result));
        }

        let self_arg_expr: Option<MaybeTypedExpr> = match known_args.as_ref() {
            Some((_, args)) if !args.is_empty() => {
                Some(MaybeTypedExpr::Typed(*args.first().unwrap()))
            }
            _ => match self.ast.mem.find(fn_call.args, |a| !a.is_explicit_context) {
                None => None,
                Some(first) => {
                    match stashed_args.iter().find(|(parsed, _)| *parsed == first.value) {
                        Some((_, typed)) => Some(MaybeTypedExpr::Typed(*typed)),
                        None => Some(MaybeTypedExpr::Parsed(first.value)),
                    }
                }
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
                stashed_args,
            ),
            false => {
                if let Some(function_id) =
                    self.find_function_namespaced(ctx.scope_id, &fn_call.name)?
                {
                    if let Some(function_ability_id) =
                        self.get_function(function_id).kind.ability_id()
                    {
                        let function_ability_index = self
                            .abilities
                            .get(function_ability_id)
                            .find_function_by_name(&self.mem, fn_call.name.name)
                            .unwrap();
                        let ability_impl_function = self.solve_ability_call(
                            function_ability_index,
                            fn_call,
                            None,
                            known_args,
                            ctx,
                            stashed_args,
                        )?;
                        Ok(CallResolution::Call(Callee::from_ability_impl_fn(
                            &ability_impl_function,
                        )))
                    } else {
                        Ok(CallResolution::Call(Callee::make_static(function_id)))
                    }
                } else {
                    // Function lookup failed, now we deal with lower priority 'callable' things
                    // Such as lambda objects or function pointers
                    macro_rules! fn_not_found {
                        () => {
                            Err(kerr!(
                                self,
                                call_span,
                                "Function not found: '{}'",
                                fn_call.name.name
                            ))
                        };
                    }
                    if !fn_call.name.path.is_empty() {
                        return fn_not_found!();
                    }
                    if let Some((variable_id, _scope_id)) =
                        self.scopes.find_variable(ctx.scope_id, fn_call.name.name)
                    {
                        self.register_variable_usage(variable_id, fn_call.name.name_span);
                        let function_variable = self.variables.get(variable_id);
                        debug!(
                            "Variable {} has type {}",
                            self.ident_str(fn_call.name.name),
                            self.type_id_to_string(function_variable.type_id)
                        );

                        match self.types.get(function_variable.type_id) {
                            Type::Lambda(lambda_type_id) => {
                                let lambda_type = self.lambda_types.get(*lambda_type_id);
                                Ok(CallResolution::Call(Callee::StaticLambda {
                                    function_id: lambda_type.function_id,
                                    lambda_value_expr: self.exprs.add(
                                        TypedExpr::Variable(VariableExpr { variable_id }),
                                        function_variable.type_id,
                                        fn_call.span,
                                    ),
                                    lambda_type_id: function_variable.type_id,
                                }))
                            }
                            Type::LambdaObject(_lambda_object) => {
                                Ok(CallResolution::Call(Callee::DynamicLambda(self.exprs.add(
                                    TypedExpr::Variable(VariableExpr { variable_id }),
                                    function_variable.type_id,
                                    fn_call.name.name_span,
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
                                Ok(CallResolution::Call(callee))
                            }
                            Type::FunctionPointer(_function_pointer) => {
                                let function_pointer_expr = self.exprs.add(
                                    TypedExpr::Variable(VariableExpr { variable_id }),
                                    function_variable.type_id,
                                    fn_call.name.name_span,
                                );
                                Ok(CallResolution::Call(Callee::DynamicFunction {
                                    function_pointer_expr,
                                }))
                            }
                            _ => fn_not_found!(),
                        }
                    } else {
                        fn_not_found!()
                    }
                }
            }
        }
    }

    fn get_return_type_for_scope(&self, scope_id: ScopeId, span: SpanId) -> K1Result<TypeId> {
        match self.scopes.enclosing_function_info(scope_id) {
            ScopeEnclosingFunctions { lambda_scope: Some(lambda_scope), .. } => {
                let Some(expected_return_type) =
                    self.scopes.get_lambda_info(lambda_scope).expected_return_type
                else {
                    kbail!(self, span, "We don't know the return type of this lambda");
                };
                Ok(expected_return_type)
            }
            ScopeEnclosingFunctions { function: Some(function_id), .. } => {
                let expected_return_type = self.get_function_type(function_id).return_type;
                Ok(expected_return_type)
            }
            _ => Err(kerr!(self, span, "No parent function")),
        }
    }

    fn get_returned_var_for_scope(&self, scope_id: ScopeId) -> Option<VariableId> {
        match self.scopes.enclosing_function_info(scope_id) {
            ScopeEnclosingFunctions { lambda_scope: Some(lambda_scope), .. } => {
                self.scopes.get_lambda_info(lambda_scope).returned_variable
            }
            ScopeEnclosingFunctions { function: Some(function_id), .. } => {
                let maybe_returned_var = self.functions.get(function_id).returned_variable;
                maybe_returned_var
            }
            _ => None,
        }
    }

    fn find_loop_for_exit(
        &self,
        name: &str,
        label: Option<StringId>,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<(ScopeId, LoopType)> {
        if ctx.flags.contains(EvalExprFlags::Defer) {
            kbail!(self, span, "{name} cannot be used inside `defer` blocks");
        }
        match self.scopes.find_loop(ctx.scope_id, label) {
            Some(found) => Ok(found),
            None => match label {
                None => kbail!(self, span, "{name} outside of loop"),
                Some(label) => {
                    kbail!(self, span, "no enclosing loop labeled @{}", self.ident_str(label))
                }
            },
        }
    }

    fn eval_break(&mut self, brk: ParsedBreak, ctx: EvalExprContext) -> K1Result<TypedExprId> {
        let span = brk.span;
        let (loop_scope_id, loop_type) = self.find_loop_for_exit("break", brk.label, ctx, span)?;
        let loop_info = *self.scopes.get_loop_info(loop_scope_id).unwrap();
        let break_value = match brk.value {
            None => self.synth_empty_value(span),
            Some(value) => match loop_type {
                LoopType::Loop => {
                    self.eval_expr(value, ctx.with_expected_type(loop_info.break_type))?
                }
                LoopType::While => {
                    kbail!(
                        self,
                        span,
                        "break with value is only allowed in `loop` loops, because loop body may not ever be executed"
                    );
                }
            },
        };
        let actual_break_type = self.exprs.get_type(break_value);
        if actual_break_type == NEVER_TYPE_ID {
            kbail!(
                self,
                span,
                "break is dead since returned expression is divergent; consider removing the 'break'"
            );
        }
        match loop_info.break_type {
            Some(expected_break_type) => {
                if let Err(msg) =
                    self.check_types(expected_break_type, actual_break_type, ctx.scope_id)
                {
                    kbail!(self, span, "Break with wrong type: {msg}");
                }
            }
            None => self.scopes.add_loop_info(
                loop_scope_id,
                ScopeLoopInfo { break_type: Some(actual_break_type), ..loop_info },
            ),
        }
        let defers = self.gather_defers(ctx.scope_id, span, DeferExtent::LoopScope(loop_scope_id));
        self.synth_defers_then_exit(defers, break_value, ctx, span, |k1, value| {
            k1.exprs.add(
                TypedExpr::Break(TypedBreak { value, loop_scope: loop_scope_id }),
                NEVER_TYPE_ID,
                span,
            )
        })
    }

    fn eval_continue(
        &mut self,
        cont: ParsedContinue,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = cont.span;
        let (loop_scope_id, _) = self.find_loop_for_exit("continue", cont.label, ctx, span)?;
        let defers = self.gather_defers(ctx.scope_id, span, DeferExtent::LoopScope(loop_scope_id));
        let empty_value = self.synth_empty_value(span);
        self.synth_defers_then_exit(defers, empty_value, ctx, span, |k1, _value| {
            k1.exprs.add(TypedExpr::Continue { loop_scope: loop_scope_id }, NEVER_TYPE_ID, span)
        })
    }

    fn eval_return(
        &mut self,
        parsed_expr: Option<ParsedExprId>,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let expected_return_type = if ctx.is_static() {
            // When _typechecking_, not executing, inside #static blocks
            // The expected return type should just be the expected type of the static block
            ctx.static_expected_return_type
        } else {
            Some(self.get_return_type_for_scope(ctx.scope_id, span)?)
        };
        let return_value = match parsed_expr {
            None => self.synth_empty_value(span),
            Some(parsed_expr) => self.eval_expr_with_coercion(
                parsed_expr,
                ctx.with_expected_type(expected_return_type),
                true,
            )?,
        };
        let returned_variable = self.check_returned_value_expr(return_value, ctx.scope_id)?;

        let return_value_type = self.exprs.get_type(return_value);
        if return_value_type == NEVER_TYPE_ID {
            return Ok(return_value);
        }
        let defers = self.gather_defers(ctx.scope_id, span, DeferExtent::FunctionTop);
        self.synth_defers_then_exit(defers, return_value, ctx, span, |k1, value| {
            k1.exprs.add_return(value, returned_variable, span)
        })
    }

    /// Collects the pending `defer` expressions of every scope exited when control leaves
    /// `from_scope` for the given extent, in execution order: innermost scope first,
    /// last-declared first within a scope.
    fn gather_defers(
        &self,
        from_scope: ScopeId,
        span: SpanId,
        extent: DeferExtent,
    ) -> SV4<ParsedExprId> {
        let mut gathered: SV4<ParsedExprId> = smallvec![];
        let mut scope_id = from_scope;
        loop {
            if let Some(defers) = self.scopes.block_defers.get(&scope_id) {
                gathered.extend(defers.deferred_exprs.iter().rev().copied());
            }
            let scope = self.scopes.get_scope(scope_id);
            let at_boundary = match extent {
                DeferExtent::FunctionTop => scope.scope_type.is_top_of_function(),
                DeferExtent::LoopScope(loop_scope) => scope_id == loop_scope,
            };
            if at_boundary {
                return gathered;
            }
            let overshot_function = matches!(extent, DeferExtent::LoopScope(_))
                && scope.scope_type.is_top_of_function();
            match scope.parent {
                Some(parent) if !overshot_function => scope_id = parent,
                _ => self.ice_span(span, "Defer gathering walked past its boundary scope"),
            }
        }
    }

    /// At a divergent scope exit (return or break) with pending `defers`: computes `value`,
    /// runs the defers, then hands the computed value to `make_exit` so the exit carries it
    /// out past the deferred code. Returns `make_exit(value)` alone when nothing is deferred.
    fn synth_defers_then_exit(
        &mut self,
        defers: SV4<ParsedExprId>,
        value: TypedExprId,
        ctx: EvalExprContext,
        span: SpanId,
        make_exit: impl FnOnce(&mut Self, TypedExprId) -> TypedExprId,
    ) -> K1Result<TypedExprId> {
        if defers.is_empty() {
            return Ok(make_exit(self, value));
        }
        let mut block = self.new_block_builder(
            ctx.scope_id,
            ScopeType::LexicalBlock,
            span,
            defers.len() as u32 + 2,
        );
        let value = if matches!(self.exprs.get(value), TypedExpr::Variable(_)) {
            value
        } else {
            let bound = self.synth_variable_defn_simple(
                self.ast.idents.b.defer_value,
                value,
                block.scope_id,
            );
            self.push_block_stmt_id(&mut block, bound.defn_stmt);
            bound.variable_expr
        };
        for deferred_parsed_expr in defers.into_iter() {
            let deferred_expr = self
                .eval_expr(deferred_parsed_expr, ctx.with_no_expected_type().with_is_defer(true))?;
            let deferred_expr_type_id = self.exprs.get_type(deferred_expr);
            self.push_block_stmt(&mut block, TypedStmt::Expr(deferred_expr, deferred_expr_type_id));
        }
        let exit = make_exit(self, value);
        self.push_block_stmt(&mut block, TypedStmt::Expr(exit, NEVER_TYPE_ID));
        Ok(self.exprs.add_block(block, NEVER_TYPE_ID))
    }

    ////////////////////////////////////////
    // Handling function calls and function-call lookalikes

    fn handle_builtin_function_call_lookalikes(
        &mut self,
        fn_call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> K1Result<Option<TypedExprId>> {
        let call_span = fn_call.span;
        if !fn_call.name.path.is_empty() {
            return Ok(None);
        }
        let n = fn_call.name.name;

        // Method or f(x) syntax
        if n == self.ast.idents.b.with && fn_call.args.len() == 2 {
            let base = MaybeTypedExpr::Parsed(self.ast.mem.get_nth(fn_call.args, 0).value);
            let res = self.compile_patch_struct(base, fn_call, ctx)?;
            return Ok(Some(res));
        }

        // "template".fmt(values): the receiver must be a literal string template,
        // recognized here before evaluation so its parts are still available.
        // Non-literal receivers fall through to ordinary method resolution.
        if fn_call.is_method && n == self.ast.idents.b.fmt && fn_call.args.len() == 2 {
            let receiver =
                self.ast.exprs.skip_type_hint(self.ast.mem.get_nth(fn_call.args, 0).value);
            if matches!(
                self.ast.exprs.get(receiver),
                ParsedExpr::Literal(ParsedLiteral::String(..))
            ) {
                kbail!(
                    self,
                    call_span,
                    "this string has no holes or interpolations to format; write it bare"
                );
            }
            if let ParsedExpr::InterpolatedString(is) = self.ast.exprs.get(receiver) {
                let parts = is.parts;
                let values_arg = self.ast.mem.get_nth(fn_call.args, 1).value;
                let values = self.eval_expr(values_arg, ctx.with_no_expected_type())?;
                let result = if self.expected_type_is_code(ctx.expected_type_id) {
                    self.synth_interpolated_code(parts, call_span, ctx, Some(values))?
                } else {
                    self.synth_interpolated_string(receiver, ctx, Some(values))?
                };
                return Ok(Some(result));
            }
        }

        if !fn_call.is_method {
            if n == self.ast.idents.b.test_compile {
                if fn_call.args.len() != 1 {
                    kbail!(self, call_span, "test-compile takes one argument");
                }
                let arg = self.ast.mem.get_nth(fn_call.args, 0);
                // Because test-compile errors get swallowed, we have to be
                // more restrictive about what you can do in there
                if ctx.is_inference() {
                    kbail!(
                        self,
                        call_span,
                        "Cannot use test-compile when types are being inferred"
                    );
                }
                self.compile_all_pending_ir(call_span)?;
                let result = self.eval_expr(arg.value, ctx.with_no_expected_type());
                let expr = match result {
                    Err(typer_error) => {
                        let string_expr = self.synth_string_literal(typer_error.message, call_span);
                        self.synth_optional_some(string_expr).0
                    }
                    Ok(_expr) => self.synth_optional_none(self.builtin_types.string(), call_span),
                };
                Ok(Some(expr))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn synth_array_element(
        &mut self,
        receiver: TypedExprId,
        array_type_id: TypeId,
        key: ParsedExprId,
        ctx: EvalExprContext,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let array_type = self.types.get(array_type_id).as_array().unwrap();
        let concrete_count = self.get_concrete_count_of_array(array_type.size_type);
        let index_expr = self.eval_expr(key, ctx.with_expected_type(Some(SIZE_TYPE_ID)))?;
        let index_expr = self
            .check_and_coerce_expr(SIZE_TYPE_ID, index_expr, ctx.scope_id, false)
            .map_err(|e| kerr!(self, span, "Array index type error: {}", e.message))?;

        let array_reference_type = self.get_expr_type(receiver).as_reference();
        let array_expr = self.synth_dereference_when(receiver, array_reference_type.is_some());

        if let Ok(static_index_expr) = self.attempt_static_lift(index_expr) {
            let static_index_type = self.exprs.get_type(static_index_expr);
            if let Some(index_size) = self
                .get_value_from_value_type(static_index_type)
                .and_then(|sv| self.static_values.get(sv).as_size())
            {
                if let Some(concrete_size) = concrete_count {
                    if index_size >= concrete_size {
                        kbail!(
                            self,
                            span,
                            "Array index out of bounds: {} >= {}",
                            index_size,
                            concrete_size
                        );
                    }
                }
            }
        }
        let get_element_expr = self.exprs.add(
            TypedExpr::ArrayGetElement(ArrayGetElement {
                base_array: array_expr,
                index: index_expr,
                packed: self.is_place_in_packed(array_expr),
            }),
            array_type.element_type,
            span,
        );
        let array_length_expr = self.synth_typed_call_typed_args(
            QIdent::naked(self.ast.idents.b.len, span),
            &[],
            &[receiver],
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
        let crash_message = self.synth_string_literal(self.ast.idents.b.crash_msg_array_oob, span);
        let crash_oob = self.synth_typed_call_typed_args(
            self.ast.idents.f.core_crash_bounds.with_span(span),
            &[],
            &[array_length_expr, index_expr, crash_message],
            ctx.with_no_expected_type(),
            false,
        )?;
        // We emit `{ if !in_bounds crash; array[index] }` rather than an if/else
        // over the element so that the element access is the block's trailing
        // expr: blocks are place-transparent, which is what makes
        // `arr.[i].&` and `arr.[i] = v` work
        let unit_expr = self.synth_empty_value(span);
        let bounds_check_expr =
            self.synth_if_else(self.builtin_types.empty, is_in_bounds, unit_expr, crash_oob, span);
        let mut statements = self.mem.new_list(2);
        statements.push(self.add_expr_stmt(bounds_check_expr));
        statements.push(self.add_expr_stmt(get_element_expr));
        let block_expr = self.exprs.add_block(
            BlockBuilder { scope_id: ctx.scope_id, statements, span },
            array_type.element_type,
        );
        Ok(block_expr)
    }

    fn handle_array_method_call(
        &mut self,
        receiver: TypedExprId,
        array_type_id: TypeId,
        call: &ParsedCall,
    ) -> K1Result<Option<CallResolution>> {
        let span = call.span;
        let array_type = self.types.get(array_type_id).as_array().unwrap();
        let concrete_count = self.get_concrete_count_of_array(array_type.size_type);
        match call.name.name {
            n if n == self.ast.idents.b.len => {
                let array_length = match concrete_count {
                    None => {
                        // We have some generic Array type like arr: Array[T, N] where N is a type
                        // parameter
                        // And someone called arr.len. The value does not matter as it will never
                        // be seen at runtime, or even compile-time since we don't execute during
                        // the generic pass. So we just provide a validly-typed value of type 'N'.
                        self.synth_phony(array_type.size_type, span)
                    }
                    Some(size) => self.synth_i64(size, span),
                };
                Ok(Some(CallResolution::OtherExpr(array_length)))
            }
            _ => {
                if let Some(method_id) =
                    self.scopes.find_function_local(self.scopes.array_scope_id, call.name.name)
                {
                    Ok(Some(CallResolution::MethodCall {
                        callee: Callee::make_static(method_id),
                        receiver,
                    }))
                } else {
                    Ok(None)
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
        stashed_args: &mut SV8<(ParsedExprId, TypedExprId)>,
    ) -> K1Result<CallResolution> {
        debug_assert!(call.name.path.is_empty());
        let fn_name = call.name.name;
        let call_span = call.span;

        let args = self.ast.mem.getn(call.args);
        let first_arg = args.first().copied();

        // Special cases of this syntax that aren't really method calls
        if let Some(base_arg) = first_arg {
            if fn_name == self.ast.idents.b.to_dyn {
                if let ParsedExpr::Variable(v) = self.ast.exprs.get(base_arg.value) {
                    let function_name = &v.name;
                    let function_id = self.find_function_namespaced(ctx.scope_id, function_name)?;
                    if let Some(function_id) = function_id {
                        let function = self.get_function(function_id);
                        if !function.is_concrete() {
                            kbail!(
                                self,
                                call_span,
                                "Cannot call toDyn with a generic function (compiler todo: accept the type args and specialize it)"
                            );
                        }
                        if function.builtin_type.is_some() {
                            kbail!(
                                self,
                                call_span,
                                "Cannot get a pointer to an intrinsic operation. (If you need one, make a wrapper function)"
                            );
                        }
                        return Ok(CallResolution::OtherExpr(
                            self.function_to_lambda_object(function_id, call_span),
                        ));
                    }
                }
                // Dyn ability erasure: `x.to-dyn[source[t = u8]]()`, or with the
                // target coming from the expected type
                let target_dyn_type: Option<TypeId> = match self
                    .ast
                    .mem
                    .get_nth_opt(call.type_args, 0)
                {
                    Some(type_arg) => match type_arg.type_expr {
                        None => None,
                        Some(type_expr) => {
                            let mut from_ability = None;
                            if let ParsedTypeExpr::TypeApplication(app) =
                                self.ast.type_exprs.get(type_expr)
                            {
                                let app = *app;
                                if self.name_resolves_to_ability(ctx.scope_id, &app.name)? {
                                    let ability_expr = self.ast.mem.push_h(ParsedAbilityExpr {
                                        name: app.name,
                                        arguments: app.args,
                                        span: app.span,
                                    });
                                    let signature =
                                        self.eval_ability_expr(ability_expr, false, ctx.scope_id)?;
                                    from_ability = Some(
                                        self.eval_dyn_ability_object_type(signature, app.span)?,
                                    );
                                }
                            }
                            match from_ability {
                                Some(t) => Some(t),
                                None => {
                                    let evaled = self.eval_type_expr(type_expr, ctx.scope_id)?;
                                    self.types.get(evaled).as_ability_object().map(|_| evaled)
                                }
                            }
                        }
                    },
                    None => ctx
                        .expected_type_id
                        .filter(|et| self.types.get(*et).as_ability_object().is_some()),
                };
                if let Some(target_dyn_type) = target_dyn_type {
                    let base = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                    let base_type = self.exprs.get_type(base);
                    let base_ref = if self.types.get(base_type).as_reference().is_some() {
                        base
                    } else {
                        // Value form: allocate in the ambient mode, like a closure env
                        self.synth_typed_call_typed_args(
                            self.ast.idents.f.mem_new.with_span(call_span),
                            &[base_type],
                            &[base],
                            ctx.with_no_expected_type(),
                            false,
                        )?
                    };
                    let result = self.ability_impl_to_dyn_object(
                        base_ref,
                        target_dyn_type,
                        ctx.scope_id,
                        call_span,
                    )?;
                    return Ok(CallResolution::OtherExpr(result));
                }
            } else if fn_name == self.ast.idents.b.as_
                || fn_name == self.ast.idents.b.widen
                || fn_name == self.ast.idents.b.trunc
                || fn_name == self.ast.idents.b.narrow
            {
                let dest_type = match self.ast.mem.get_nth_opt(call.type_args, 0) {
                    Some(NamedTypeArg { type_expr: Some(type_expr), .. }) => {
                        self.eval_type_expr(*type_expr, ctx.scope_id)?
                    }
                    _ => match ctx.expected_type_id {
                        None => {
                            kbail!(
                                self,
                                call_span,
                                "Cannot use {}() with no expected type",
                                self.ast.idents.get_string(fn_name)
                            );
                        }
                        Some(et) => et,
                    },
                };
                let result = if fn_name == self.ast.idents.b.as_ {
                    self.eval_cast(base_arg.value, dest_type, call_span, ctx)?
                } else if fn_name == self.ast.idents.b.narrow {
                    self.eval_cast_narrow(base_arg.value, dest_type, call_span, ctx)?
                } else {
                    let widen = fn_name == self.ast.idents.b.widen;
                    self.eval_widen_trunc(base_arg.value, dest_type, widen, call_span, ctx)?
                };
                return Ok(CallResolution::OtherExpr(result));
            } else if fn_name == self.ast.idents.b.to_static {
                if call.args.len() != 1 {
                    kbail!(self, call_span, ".toStatic() takes no additional arguments");
                }
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                return match self.attempt_static_lift(base_value) {
                    Err(msg) => {
                        kbail!(self, call_span, "Failed to lift value to static: {}", msg.message);
                    }
                    Ok(static_expr_id) => Ok(CallResolution::OtherExpr(static_expr_id)),
                };
            } else if fn_name == self.ast.idents.b.from_static {
                let base_value = self.eval_expr(base_arg.value, ctx.with_no_expected_type())?;
                let base_type_id = self.exprs.get_type(base_value);
                let Some(static_type) = self.get_static_type_of_type(base_type_id) else {
                    kbail!(
                        self,
                        call_span,
                        "Cannot use .from-static() on non-static type: {}",
                        base_type_id
                    );
                };
                let materialized = self.materialize_static_value(
                    static_type.family_type_id,
                    static_type.value_id,
                    call_span,
                );
                return Ok(CallResolution::OtherExpr(materialized));
            }
        }

        // We compile this expr, but we dont include it with our return value
        // so it has to be re-compiled, as a result every method call base is double-compiled
        // unnecessarily
        let base_expr = match base_expr {
            MaybeTypedExpr::Typed(expr) => expr,
            MaybeTypedExpr::Parsed(parsed_expr_id) => {
                self.eval_expr(parsed_expr_id, ctx.with_no_expected_type())?
            }
        };

        // Handle the special case of the synthesized sum 'as-variant' methods
        if let Some(sum_as_result) = self.handle_sum_as_variant_call(base_expr, call)? {
            return Ok(CallResolution::OtherExpr(sum_as_result));
        }

        let base_expr_type = self.exprs.get_type(base_expr);
        let base_for_method = self.get_base_for_method(base_expr_type);

        if let Type::Array(_array_type) = self.types.get(base_for_method) {
            if let Some(resolution) =
                self.handle_array_method_call(base_expr, base_for_method, call)?
            {
                return Ok(resolution);
            }
        }

        if let Type::Vector(_) = self.types.get(base_for_method) {
            if let Some(method_id) =
                self.scopes.find_function_local(self.scopes.vector_scope_id, call.name.name)
            {
                return Ok(CallResolution::MethodCall {
                    callee: Callee::make_static(method_id),
                    receiver: base_expr,
                });
            }
        }

        // Dynamic dispatch on ability objects: the method must be one of the
        // object's slots; the object itself supplies the self pointer
        if let Type::AbilityObject(ao) = self.types.get(base_for_method) {
            let ao = *ao;
            let ability = self.abilities.get(ao.specialized_ability_id);
            let ability_self_type = ability.self_type_id;
            if let Some(fn_ref) = ability.find_function_by_name(&self.mem, call.name.name) {
                let object_expr = if self.types.get(base_expr_type).as_reference().is_some() {
                    self.synth_dereference(base_expr)
                } else {
                    base_expr
                };
                let repr_fields = self.types.get(ao.struct_representation).expect_struct().fields;
                let repr_fields = self.mem.getn(repr_fields);
                match repr_fields[1..].iter().position(|f| f.name == call.name.name) {
                    Some(position) => {
                        let field_index = (position + 1) as u32;
                        let slot_ptr_type = repr_fields[position + 1].type_id;
                        let slot_function_type = self
                            .types
                            .get(slot_ptr_type)
                            .as_function_pointer()
                            .unwrap()
                            .function_type_id;
                        return Ok(CallResolution::MethodCall {
                            callee: Callee::DynamicAbilityFn {
                                object_expr,
                                field_index,
                                slot_function_type,
                            },
                            receiver: object_expr,
                        });
                    }
                    None => {
                        let signature = TypedAbilitySignature {
                            specialized_ability_id: ao.specialized_ability_id,
                            impl_arguments: ao.impl_arguments,
                        };
                        let subst_pairs = self.dyn_ability_subst_pairs(signature);
                        let reason = self
                            .dyn_slot_fn_type(ability_self_type, &subst_pairs, fn_ref.function_id)
                            .err()
                            .unwrap_or_else(|| "unknown".to_string());
                        kbail!(
                            self,
                            call_span,
                            "'{}' is not dyn-dispatchable: {}",
                            call.name.name,
                            reason
                        );
                    }
                }
            }
        }

        if let Some(companion_ns) =
            self.get_defn_info(base_for_method).and_then(|d| d.companion_namespace)
        {
            let companion_scope_id = self.namespaces.get_scope(companion_ns);
            debug!("companion scope {}", self.scope_id_to_string(companion_scope_id));
            debug!(
                "functions in companion scope for type {}: {:?}",
                self.type_id_to_string(base_for_method),
                self.scopes
                    .iter_scope_functions(companion_scope_id)
                    .map(|(_, f, _)| self.ident_str(self.functions.get(f).name).to_string())
                    .join(", ")
            );
            if let Some(method_id) = self.scopes.find_function_local(companion_scope_id, fn_name) {
                return Ok(CallResolution::MethodCall {
                    callee: Callee::make_static(method_id),
                    receiver: base_expr,
                });
            }
        } else {
            debug!("companion scope not found for call to {}", self.ident_str(fn_name))
        };

        // "w.write(template)" / "w.writeln(template, values?)": when the receiver
        // is a writer and the argument is a string template, this call is the
        // format machinery (code mode when the writer is a *code-builder).
        // Inherent `write` methods were checked above; a non-writer receiver or a
        // string-valued argument falls through to ordinary ability resolution.
        if (fn_name == self.ast.idents.b.write || fn_name == self.ast.idents.b.writeln)
            && known_args.is_none()
            && (call.args.len() == 2 || call.args.len() == 3)
        {
            let newline = fn_name == self.ast.idents.b.writeln;
            let template_arg = self.ast.mem.get_nth(call.args, 1).value;
            let is_template = matches!(
                self.ast.exprs.get(template_arg),
                ParsedExpr::InterpolatedString(_) | ParsedExpr::Literal(ParsedLiteral::String(..))
            );
            let writer_impl = if is_template {
                self.expect_ability_impl(
                    base_expr_type,
                    ABILITY_ID_WRITER,
                    true,
                    ctx.scope_id,
                    call_span,
                )
                .ok()
            } else {
                None
            };
            if let Some((_writer_impl, self_adjust)) = writer_impl {
                let ctx_no_hint = ctx.with_no_expected_type();
                let writer = match self_adjust {
                    SelfAdjust::None => base_expr,
                    SelfAdjust::Deref => self.synth_dereference(base_expr),
                    SelfAdjust::AddrOf => {
                        self.synth_address_of(base_expr, call_span, true).map_err(|e| {
                            kerr!(self,
                                call_span,
                                "The receiver is not immediately a writer, but a reference to it is. So we tried to take its address, which is not allowed because it is not a place: {}",
                                e.message)
                        })?
                    }
                };
                let template_span = self.ast.exprs.get_span(template_arg);
                let newline_part = InterpolatedStringPart::String {
                    string_id: self.ast.idents.b.newline,
                    span: template_span,
                };
                let parts = match self.ast.exprs.get(template_arg) {
                    ParsedExpr::Literal(ParsedLiteral::String(string_id, _)) => {
                        let string_part = InterpolatedStringPart::String {
                            string_id: *string_id,
                            span: template_span,
                        };
                        if newline {
                            self.ast.mem.pushn(&[string_part, newline_part])
                        } else {
                            self.ast.mem.pushn(&[string_part])
                        }
                    }
                    ParsedExpr::InterpolatedString(is) => {
                        if newline {
                            let mut parts =
                                self.ast.mem.new_list_from_slice(is.parts, is.parts.len() + 1);
                            parts.push(newline_part);
                            parts.to_slice()
                        } else {
                            is.parts
                        }
                    }
                    _ => unreachable!(),
                };
                let values = match self.ast.mem.get_nth_opt(call.args, 2) {
                    None => self.synth_empty_value(call_span),
                    Some(values_arg) => self.eval_expr(values_arg.value, ctx_no_hint)?,
                };
                let format_block =
                    self.synth_format_calls(writer, parts, values, call_span, ctx_no_hint)?;
                return Ok(CallResolution::OtherExpr(format_block));
            }
        }

        let Some(ability_names) = self.function_name_to_ability_names.get(&fn_name) else {
            kbail!(
                self,
                call_span,
                "Method '{}' does not exist on type '{}'",
                call.name.name,
                base_expr_type,
            );
        };

        let ability_names = ability_names.as_slice(&self.mem);
        let mut errors: SV4<K1Message> = smallvec![];
        debug!(
            "ability names with function name: {}",
            ability_names.iter().map(|n| self.ident_str(*n)).join(", ")
        );
        let mut ability_ids = self.tmp.new_list(0);
        self.scopes.collect_ability_ids_bound_to_names(
            ctx.scope_id,
            ability_names,
            &mut ability_ids,
            &mut self.tmp,
        );
        for ability_id in ability_ids.as_slice() {
            let Some(ability_function_ref) =
                self.abilities.get(*ability_id).find_function_by_name(&self.mem, fn_name)
            else {
                continue;
            };
            match self.solve_ability_call(
                ability_function_ref,
                call,
                Some(base_expr),
                known_args,
                ctx,
                stashed_args,
            ) {
                Ok(ability_impl_fn) => {
                    return Ok(CallResolution::MethodCall {
                        callee: Callee::from_ability_impl_fn(&ability_impl_fn),
                        receiver: base_expr,
                    });
                }
                Err(e) => {
                    stashed_args.clear();
                    errors.push(e);
                }
            }
        }
        if errors.is_empty() {
            Err(kerr!(
                self,
                call_span,
                "Method '{}' does not exist on type: '{}'",
                call.name.name,
                base_expr_type,
            ))
        } else {
            Err(errors.into_iter().next().unwrap())
        }
    }

    fn compile_specialized_function_reference(
        &mut self,
        generic_function_id: FunctionId,
        call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = call.span;
        let function = self.get_function(generic_function_id);
        let function_name = function.name;
        let is_macro = function.is_macro();
        let type_params = function.type_params;
        let fnlike_type_params = function.fnlike_type_params;
        let signature = function.signature();
        if is_macro {
            kbail!(
                self,
                span,
                "Macro '{}' cannot be used as a value",
                self.ident_str(function_name)
            );
        }
        if matches!(
            self.get_function(generic_function_id).linkage,
            Linkage::Intrinsic | Linkage::LlvmIntrinsic(_)
        ) {
            kbail!(self, span, "Intrinsic '{}' has no address", self.ident_str(function_name));
        }
        if type_params.is_empty() {
            kbail!(self, span, "'{}' takes no type arguments", self.ident_str(function_name));
        }
        if !fnlike_type_params.is_empty() {
            kbail!(
                self,
                span,
                "Cannot reference '{}': fn-like type parameters are only inferable from call arguments",
                self.ident_str(function_name)
            );
        }
        if type_params.len() != call.type_args.len() {
            kbail!(
                self,
                span,
                "Takes {} type arguments; got {}",
                type_params.len(),
                call.type_args.len()
            );
        }
        let (typed_type_args, _subst_pairs) = self.check_type_args_against_params(
            type_params,
            self.ast.mem.getn(call.type_args),
            ctx.scope_id,
            EvalTypeExprContext::EMPTY,
        )?;
        let type_args = TypeArgs::from_slice_in(self.mem.getn(typed_type_args), &mut self.mem);

        let cached_specialization =
            self.find_function_specialization(generic_function_id, type_args, TypeArgs::empty());
        let specialized_function_type = match cached_specialization {
            Some(function_id) => self.get_function(function_id).type_id,
            None => self.substitute_in_function_signature(type_args, TypeArgs::empty(), signature),
        };
        if self.get_type_variable_counts(specialized_function_type).is_abstract()
            || ctx.is_inference()
        {
            let function_pointer_type = self.add_function_pointer_type(specialized_function_type);
            self.emit_ls_entity(
                call.name.name_span,
                LsEntityKind::Function { function_id: generic_function_id, is_defn: false },
            );
            return Ok(self.exprs.add(
                TypedExpr::FunctionPointer(FunctionPointerExpr {
                    function_id: generic_function_id,
                }),
                function_pointer_type,
                span,
            ));
        }
        let specialized_function_id = match cached_specialization {
            Some(function_id) => function_id,
            None => self.specialize_function_declaration(
                type_args,
                TypeArgs::empty(),
                generic_function_id,
            ),
        };
        Ok(self.function_to_reference(specialized_function_id, span))
    }

    pub fn function_to_reference(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let function_pointer_type = self.add_function_pointer_type(function.type_id);
        self.get_function_mut(function_id).flags.insert(TypedFunctionFlags::AddressTaken);
        self.emit_ls_entity(call_span, LsEntityKind::Function { function_id, is_defn: false });
        self.exprs.add(
            TypedExpr::FunctionPointer(FunctionPointerExpr { function_id }),
            function_pointer_type,
            call_span,
        )
    }

    pub fn function_abi(&self, function_id: FunctionId) -> AbiMode {
        let function = self.get_function(function_id);
        match function.linkage {
            Linkage::Standard => {
                if function.address_taken()
                    || function.abi_native()
                    || function.is_reloadable()
                    || self.get_main_function_id() == Some(function_id)
                {
                    AbiMode::Native
                } else {
                    AbiMode::Internal
                }
            }
            Linkage::External { .. }
            | Linkage::Exported { .. }
            | Linkage::Intrinsic
            | Linkage::LlvmIntrinsic(_) => AbiMode::Native,
        }
    }

    pub fn function_to_lambda_object(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let function_defn_span = self.ast.get_span_for_id(function.parsed_id);
        let dyn_function_id = if let Some(dyn_fn_id) = function.dyn_fn_id {
            dyn_fn_id
        } else {
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
                defn_span: function_defn_span,
            });
            let mut new_variables = self.mem.new_list(new_function.params.len() + 1);
            new_variables.push(TypedFunctionParam {
                variable_id: empty_env_variable,
                span: function_defn_span,
            });
            new_variables.extend(self.mem.getn(new_function.params));
            new_function.params = new_variables.to_slice();

            let new_function_type = self.add_lambda_env_to_function_type(new_function.type_id);
            new_function.type_id = new_function_type;
            let old_name = self.ident_str(new_function.name);
            new_function.name = self.ast.idents.intern(format!("{}__dyn", old_name));
            let actual_new_function_id = self.add_function(new_function);
            debug_assert_eq!(actual_new_function_id, new_function_id);
            self.get_function_mut(function_id).dyn_fn_id = Some(new_function_id);
            new_function_id
        };
        let dyn_function = self.get_function(dyn_function_id);
        let lambda_object_type_id =
            self.add_lambda_object(dyn_function.type_id, dyn_function.parsed_id);

        let null_value_id = self.static_values.add(StaticValue::Zero(POINTER_TYPE_ID));
        let null_ptr_expr = self.exprs.add(
            TypedExpr::StaticValue(StaticConstantExpr {
                value_id: null_value_id,
                is_typed_as_static: false,
            }),
            POINTER_TYPE_ID,
            call_span,
        );
        let fn_ptr_field = StructLiteralField {
            name: self.ast.idents.b.fn_ptr,
            expr: Some(self.function_to_reference(dyn_function_id, call_span)),
        };
        let env_ptr_field =
            StructLiteralField { name: self.ast.idents.b.env_ptr, expr: Some(null_ptr_expr) };
        let fields = self.mem.pushn(&[fn_ptr_field, env_ptr_field]);
        let lambda_object_struct_literal = self.exprs.add(
            TypedExpr::Struct(StructLiteral { fields }),
            lambda_object_type_id,
            call_span,
        );
        lambda_object_struct_literal
    }

    pub fn lambda_to_lambda_object(
        &mut self,
        lambda_expr: TypedExprId,
        lambda_type_id: LambdaTypeId,
        scope_id: ScopeId,
    ) -> K1Result<TypedExprId> {
        let span = self.exprs.get_span(lambda_expr);
        let lambda_type = self.lambda_types.get(lambda_type_id);
        let function_id = lambda_type.function_id;
        let function_type = lambda_type.function_type;
        let parsed_id = lambda_type.parsed_id;
        let lambda_object_type_id = self.add_lambda_object(function_type, parsed_id);

        let env_ref = self.synth_typed_call_typed_args(
            self.ast.idents.f.mem_new.with_span(span),
            &[self.exprs.get_type(lambda_expr)],
            &[lambda_expr],
            EvalExprContext::make(scope_id),
            false,
        )?;
        let env_ptr = self.synth_cast(env_ref, POINTER_TYPE_ID, CastType::ReferenceToPointer, None);

        let fn_ptr_field = StructLiteralField {
            name: self.ast.idents.b.fn_ptr,
            expr: Some(self.function_to_reference(function_id, span)),
        };
        let env_ptr_field =
            StructLiteralField { name: self.ast.idents.b.env_ptr, expr: Some(env_ptr) };
        let fields = self.mem.pushn(&[fn_ptr_field, env_ptr_field]);
        Ok(self.exprs.add(TypedExpr::Struct(StructLiteral { fields }), lambda_object_type_id, span))
    }

    fn add_lambda_env_to_function_type(&mut self, function_type_id: TypeId) -> TypeId {
        let function_type = self.types.get(function_type_id).as_function().unwrap();
        let return_type = function_type.return_type;
        let physical_params = function_type.physical_params;
        let empty_env_struct_type = EMPTY_TYPE_ID;
        let empty_env_struct_ref = self.add_reference_type(empty_env_struct_type);
        let mut new_params = self.mem.new_list(physical_params.len() + 1);

        new_params.push(FnParamType {
            name: self.ast.idents.b.lambda_env_var_name,
            type_id: empty_env_struct_ref,
            is_context: false,
            is_lambda_env: true,
            is_macro_code: false,
        });
        new_params.extend(self.mem.getn(physical_params));

        let new_function_type =
            FunctionType { physical_params: new_params.to_slice(), return_type, is_lambda: true };

        let defn_info = self.get_defn_info(function_type_id);
        self.add_type(Type::Function(new_function_type), defn_info, None)
    }

    /// Compiles 'patching' structs using the 'with' construct.
    /// myStructFoo.with({ a: 1, b: false })
    fn compile_patch_struct(
        &mut self,
        base_expr: MaybeTypedExpr,
        call: &ParsedCall,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = call.span;
        let base_struct_expr = match base_expr {
            MaybeTypedExpr::Parsed(parsed) => {
                // We use the expected type
                self.eval_expr(parsed, ctx)?
            }
            MaybeTypedExpr::Typed(typed_expr_id) => typed_expr_id,
        };
        let base_struct_type_id = self.exprs.get_type(base_struct_expr);
        let Type::Struct(base_struct_type) = self.types.get(base_struct_type_id) else {
            kbail!(self, span, "'with' receiver must be a struct");
        };
        let base_struct_fields = base_struct_type.fields;
        let mut block = self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 2);
        let base_struct_name = self.ast.idents.b.base_struct;
        let base_struct_var =
            self.synth_variable_defn_simple(base_struct_name, base_struct_expr, block.scope_id);
        self.push_block_stmt_id(&mut block, base_struct_var.defn_stmt);
        let patch_arg = *self.ast.mem.get_nth(call.args, 1);
        enum ProvidedPatchStruct {
            ParsedFields(AstSlice<StructValueField>),
            TypedExpr(TypedExprId),
        }
        let mut patch_hits = 0;
        let mut patched_count = 0;
        let patch_struct = match self.ast.exprs.get(patch_arg.value) {
            ParsedExpr::Struct(parsed_struct) => {
                ProvidedPatchStruct::ParsedFields(parsed_struct.fields)
            }
            _other => ProvidedPatchStruct::TypedExpr(
                self.eval_expr(patch_arg.value, ctx.with_no_expected_type())?,
            ),
        };

        let mut final_fields = self.mem.new_list(base_struct_fields.len());
        for (base_field_index, base_field) in self.mem.getn(base_struct_fields).iter().enumerate() {
            let name = base_field.name;
            let expr_value_for_field: TypedExprId = match patch_struct {
                ProvidedPatchStruct::ParsedFields(parsed) => {
                    patched_count = parsed.len();
                    let patch_parsed_field =
                        self.ast.mem.getn(parsed).iter().find(|f| f.name == name);
                    match patch_parsed_field {
                        None => self.synth_field_access(
                            base_struct_var.variable_expr,
                            base_field_index,
                            span,
                        ),
                        Some(parsed_field) => {
                            let parsed_expr = match parsed_field.value {
                                StructValueFieldKind::VarShorthand => self
                                    .synth_parsed_variable_expr(
                                        parsed_field.name,
                                        parsed_field.span,
                                    ),
                                StructValueFieldKind::Expr(parsed_expr) => parsed_expr,
                                StructValueFieldKind::Uninit => {
                                    kbail!(self, parsed_field.span, "uninit is not permitted here");
                                }
                            };

                            let typed_expr = self.eval_expr_with_coercion(
                                parsed_expr,
                                ctx.with_expected_type(Some(base_field.type_id)),
                                true,
                            )?;
                            patch_hits += 1;
                            typed_expr
                        }
                    }
                }
                ProvidedPatchStruct::TypedExpr(patch_struct_expr) => {
                    let patch_struct_type_id = self.exprs.get_type(patch_struct_expr);
                    let Type::Struct(patch_struct) = self.types.get(patch_struct_type_id) else {
                        kbail!(self, span, "'with' argument struct must be a struct");
                    };
                    patched_count = patch_struct.fields.len();
                    if let Some((matching_patch_field_index, matching_patch_field)) =
                        self.get_struct_field_by_name(patch_struct_type_id, base_field.name)
                    {
                        if base_field.type_id != matching_patch_field.type_id {
                            kbail!(self, span, "Mismatching types for field {}", base_field.name);
                        }
                        let patch_field_access_expr_id = self.synth_field_access(
                            patch_struct_expr,
                            matching_patch_field_index,
                            span,
                        );
                        patch_hits += 1;
                        patch_field_access_expr_id
                    } else {
                        let base_field_access_expr_id = self.synth_field_access(
                            base_struct_var.variable_expr,
                            base_field_index,
                            span,
                        );
                        base_field_access_expr_id
                    }
                }
            };
            final_fields.push(StructLiteralField {
                name: base_field.name,
                expr: Some(expr_value_for_field),
            })
        }
        if patch_hits < patched_count {
            let base_has_field =
                |name: StringId| self.mem.getn(base_struct_fields).iter().any(|f| f.name == name);
            let mut unmatched: SV4<StringId> = smallvec![];
            match patch_struct {
                ProvidedPatchStruct::ParsedFields(parsed) => {
                    for f in self.ast.mem.getn(parsed) {
                        if !base_has_field(f.name) {
                            unmatched.push(f.name);
                        }
                    }
                }
                ProvidedPatchStruct::TypedExpr(patch_struct_expr) => {
                    let patch_struct_type_id = self.exprs.get_type(patch_struct_expr);
                    let Type::Struct(patch_struct) = self.types.get(patch_struct_type_id) else {
                        unreachable!()
                    };
                    for f in self.mem.getn(patch_struct.fields) {
                        if !base_has_field(f.name) {
                            unmatched.push(f.name);
                        }
                    }
                }
            }
            let names = unmatched.iter().map(|n| self.ident_str(*n)).collect::<Vec<_>>().join(", ");
            kbail!(self, span, "Fields not present in the base struct: {}", names);
        }
        let final_fields_handle = final_fields.to_slice();
        let new_struct = self.exprs.add(
            TypedExpr::Struct(StructLiteral { fields: final_fields_handle }),
            base_struct_type_id,
            span,
        );
        self.push_block_expr_id(&mut block, new_struct);
        let block_id = self.exprs.add_block(block, base_struct_type_id);
        Ok(block_id)
    }

    /// After resolving to a particular root AbilityId + function index using just names,
    /// we have to use the information in the call to 'solve' for Self using the rest
    /// of the information available in the `ParsedCall`, and ultimately come back with either
    /// an error, such as 'couldnt solve', 'not implemented' or: an exact physical function id of the correct AbilityImpl
    fn solve_ability_call(
        &mut self,
        ability_function_ref: TypedAbilityFunctionRef,
        fn_call: &ParsedCall,
        receiver_if_method: Option<TypedExprId>,
        known_args: Option<&(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        stashed_args: &mut SV8<(ParsedExprId, TypedExprId)>,
    ) -> K1Result<AbilityImplFunction> {
        let call_span = fn_call.span;
        let ability_fn_sig = self.get_function(ability_function_ref.function_id).signature();
        let base_ability_id = ability_function_ref.ability_id;
        let ability_fn_type = *self.types.get(ability_fn_sig.function_type).as_function().unwrap();
        let ability_fn_return_type = ability_fn_type.return_type;
        let ability_params = self.abilities.get(base_ability_id).parameters;
        let ability_self_type_id = self.abilities.get(base_ability_id).self_type_id;

        // The function's own type params (following the injected Self) participate in
        // inference too, so the expected return type and explicit type args can reach
        // value arguments; their final solutions come from the impl function call
        let self_ident = self.ast.idents.b.self_;
        let mut fn_own_type_params = self.tmp.new_list(ability_fn_sig.type_params.len());
        for tp in self.mem.getn(ability_fn_sig.type_params) {
            let is_self =
                *tp == ability_self_type_id || self.get_type_parameter(*tp).name == self_ident;
            let is_ability_param =
                self.mem.getn(ability_params).iter().any(|ap| ap.type_variable_id == *tp);
            if !is_self && !is_ability_param {
                fn_own_type_params.push(*tp);
            }
        }

        let mut all_type_params =
            self.tmp.new_list(ability_params.len() + 1 + fn_own_type_params.len() as u32);
        all_type_params.push(ability_self_type_id);
        for p in self.mem.getn(ability_params) {
            all_type_params.push(p.type_variable_id);
        }
        for tp in fn_own_type_params.iter() {
            all_type_params.push(*tp);
        }

        let self_only_type_params_handle = self.mem.pushn(&[ability_self_type_id]);

        let aligned = self.align_call_arguments_with_parameters(
            fn_call,
            &ability_fn_sig,
            receiver_if_method,
            ability_fn_type.logical_params(),
            known_args.map(|ka| ka.1),
            ctx.scope_id,
            true,
            false,
        )?;
        let mut args_and_params = self.tmp.new_list(aligned.len() + 1 + fn_call.type_args.len());
        for (index, type_arg) in self.ast.mem.getn(fn_call.type_args).iter().enumerate() {
            let Some(passed_type_expr) = type_arg.type_expr else { continue };
            let matching_param = match type_arg.name {
                Some(passed_name) => fn_own_type_params
                    .iter()
                    .find(|tp| self.get_type_parameter(**tp).name == passed_name),
                None => fn_own_type_params.get(index),
            };
            // Args that don't line up (e.g. `show/show[i32]` passing Self) are left for
            // the impl function call, which checks them against the impl's type params
            let Some(matching_param) = matching_param else { continue };
            let param_type = *matching_param;
            let passed_type = self.eval_type_expr(passed_type_expr, ctx.scope_id)?;
            args_and_params.push(InferenceInputPair {
                arg: TypeOrParsedExpr::Type(passed_type),
                param_type,
                allow_mismatch: false,
            });
        }
        if let Some(expected_type) = ctx.expected_type_id {
            args_and_params.push(InferenceInputPair {
                arg: TypeOrParsedExpr::Type(expected_type),
                param_type: ability_fn_return_type,
                allow_mismatch: true,
            });
        }
        let first_value_pair_index = args_and_params.len() as u32;
        for (arg, param) in aligned.iter(&self.tmp) {
            let arg = match arg {
                MaybeTypedExpr::Typed(expr) => TypeOrParsedExpr::Type(self.exprs.get_type(*expr)),
                MaybeTypedExpr::Parsed(parsed_expr) => TypeOrParsedExpr::Parsed(*parsed_expr),
            };
            args_and_params.push(InferenceInputPair {
                arg,
                param_type: param.type_id,
                allow_mismatch: false,
            });
        }

        debug!("all ability params: {}", self.pretty_print_types(&all_type_params, ", "));
        debug!("to solve: {}", self.pretty_print_type_slice(self_only_type_params_handle, ", "));

        let mut excluded_args = self.tmp.new_list(ability_fn_sig.fnlike_type_params.len());
        for ftp in self.mem.getn(ability_fn_sig.fnlike_type_params) {
            excluded_args.push(ftp.value_param_index);
        }
        let (self_solution, other_solved) = self
            .infer_types(
                &all_type_params,
                self_only_type_params_handle,
                &args_and_params,
                fn_call.span,
                ctx.scope_id,
                Some(infer::InferArgStash {
                    ctx,
                    first_value_pair_index,
                    excluded_args: &excluded_args,
                    stashed: stashed_args,
                }),
            )
            .map_err(|f| self.render_inference_failure(f))?;

        let mut parameter_constraints: List<Option<TypeId>, MemTmp> =
            self.tmp.new_list(ability_params.len());
        for (index, ab_param) in self.mem.getn(ability_params).iter().enumerate() {
            if ab_param.is_impl_param {
                continue;
            }
            // other_solved is parallel to all_type_params: [Self, ...ability_params, ...]
            let solution = other_solved.as_slice(&self.mem)[1 + index];

            // If we've already solved for one of the params that appear in this ability signature,
            // we need to constrain our ability impl search with those solutions
            parameter_constraints.push(if solution == TypeId::PENDING {
                None
            } else {
                Some(solution)
            });
        }
        let solved_self = self_solution.as_slice(&self.mem)[0];

        let solved_self = self.get_static_family_id_if_static(solved_self);
        let (impl_handle, _) = self
            .find_or_generate_ability_impl_for_type(
                solved_self,
                base_ability_id,
                &parameter_constraints,
                true,
                ctx.scope_id,
                call_span,
            )
            .map_err(|msg| {
                kerr!(
                    self,
                    call_span,
                    "Call to {}/{} with self = {} does not work\n{}",
                    &self.ability_impl_signature_to_string(base_ability_id, MSlice::empty()),
                    fn_call.name.name,
                    solved_self,
                    msg,
                )
            })?;

        let full_impl = *self.ability_impls.get(impl_handle.full_impl_id);
        let impl_function = *full_impl.function_at_index(&self.mem, ability_function_ref.index);
        // FunctionId: availability was decided when the impl was built.
        // Abstract (a type-param constraint pseudo-impl): the fn's where
        // constraints are checked here, at the call, because not all of the
        // param's constraints exist yet when the pseudo-impl is built.
        // Unavailable: decided at impl build; Unavailable carries no payload,
        // so we re-run the check to name the failing constraint
        if let AbilityImplFunction::Unavailable | AbilityImplFunction::Abstract(_) = impl_function {
            if let Err(failure) = self.check_ability_fn_where_constraints(
                full_impl.ability_id,
                full_impl.impl_arguments,
                full_impl.self_type_id,
                ability_function_ref.index,
                ctx.scope_id,
                call_span,
            ) {
                return Err(kerr!(
                    self,
                    call_span,
                    "{}/{} is not available for {}: {}",
                    &self.ability_impl_signature_to_string(base_ability_id, MSlice::empty()),
                    fn_call.name.name,
                    solved_self,
                    failure.message,
                ));
            }
        }
        if let AbilityImplFunction::Unavailable = impl_function {
            return Err(kerr!(
                self,
                call_span,
                "{}/{} is not available for {}: a where constraint on the function is not satisfied",
                &self.ability_impl_signature_to_string(base_ability_id, MSlice::empty()),
                fn_call.name.name,
                solved_self,
            ));
        }
        Ok(impl_function)
    }

    fn handle_enum_get_value(
        &mut self,
        base_expr_id: TypedExprId,
        span: SpanId,
    ) -> K1Result<Option<TypedExprId>> {
        match self.get_expr_type(base_expr_id) {
            Type::Enum(_) => Ok(Some(self.synth_enum_get_value(base_expr_id, span))),
            _ => Ok(None),
        }
    }

    fn handle_sum_get_tag(
        &mut self,
        base_expr_id: TypedExprId,
        span: SpanId,
    ) -> K1Result<Option<TypedExprId>> {
        let sum_type = match self.get_expr_type(base_expr_id) {
            Type::Sum(s) => s,
            _ => return Ok(None),
        };

        let tag_type = sum_type.tag_type;

        Ok(Some(self.exprs.add(
            TypedExpr::SumGetTag(GetSumTag { sum_expr: base_expr_id }),
            tag_type.type_id(),
            span,
        )))
    }

    fn handle_sum_as_variant_call(
        &mut self,
        base_expr: TypedExprId,
        fn_call: &ParsedCall,
    ) -> K1Result<Option<TypedExprId>> {
        let fn_name = self.ident_str(fn_call.name.name);
        let preconditions =
            fn_name.starts_with("as") && fn_call.type_args.is_empty() && fn_call.args.len() == 1;
        if !preconditions {
            return Ok(None);
        }
        let (e, is_reference) = match self.get_expr_type(base_expr) {
            Type::Reference(r) => {
                if let Type::Sum(e) = self.types.get(r.inner_type) {
                    (e, true)
                } else {
                    return Ok(None);
                }
            }
            Type::Sum(e) => (e, false),
            _ => return Ok(None),
        };
        let span = fn_call.span;
        let variants = e.variants;
        let mut s = std::mem::take(&mut self.buffers.name_builder);
        let fn_name = self.ident_str(fn_call.name.name);
        let Some(variant) = self.mem.getn(variants).iter().find(|v| {
            s.push_str("as-");
            let name = self.ident_str(v.name);
            // let first_letter = name.chars().next().unwrap();
            // let rest = name.chars().skip(1);
            // s.push(first_letter.to_ascii_uppercase());
            s.push_str(name);

            let is_match = fn_name == s;
            s.clear();
            is_match
        }) else {
            self.buffers.name_builder = s;
            return Ok(None);
        };
        self.buffers.name_builder = s;
        let Some(payload_type_id) = variant.payload else {
            // Note, we could return a Some(unit) here, but for now I'll just fail
            kbail!(self, span, "Variant '{}' has no data", variant.name);
        };
        let variant_index = variant.index;
        let sum_base_expr =
            if is_reference { self.synth_dereference(base_expr) } else { base_expr };
        let condition = self.synth_sum_is_variant(sum_base_expr, variant_index, Some(span))?;
        let payload_expr = self.exprs.add(
            TypedExpr::SumGetPayload(GetSumPayload {
                sum_expr: sum_base_expr,
                variant_index,
                packed: self.is_place_in_packed(sum_base_expr),
            }),
            payload_type_id,
            span,
        );
        let payload_referenced = if is_reference {
            // Infallible: when is_reference, sum_base_expr is a Deref
            self.synth_address_of(payload_expr, SpanId::NONE, false).unwrap()
        } else {
            payload_expr
        };
        let (consequent, consequent_type_id) = self.synth_optional_some(payload_referenced);

        let out_value_type = self.exprs.get_type(payload_referenced);
        let alternate = self.synth_optional_none(out_value_type, span);
        debug_assert_eq!(consequent_type_id, self.exprs.get_type(alternate));
        Ok(Some(self.synth_if_else(consequent_type_id, condition, consequent, alternate, span)))
    }

    fn eval_variant(
        &mut self,
        parsed_variant: ParsedVariant,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = parsed_variant.span;
        let provided_type = match &parsed_variant.type_name {
            Some(qident) => {
                let Some((type_id, _)) = self.find_type_namespaced(ctx.scope_id, qident)? else {
                    kbail!(self, qident.name_span, "No type {} is in scope", qident);
                };
                self.emit_ls_entity(
                    qident.name_span,
                    LsEntityKind::Type { type_id, applied_type_id: None },
                );
                type_id
            }
            None => match ctx.expected_type_id {
                None => {
                    kbail!(
                        self,
                        span,
                        "Could not infer sum type from context; try supplying the name or providing a type ascription"
                    );
                }
                Some(type_id) => type_id,
            },
        };

        if let Some(cs) = &mut self.completion
            && cs.site.is_none()
            && parsed_variant.variant_name == cs.marker
        {
            cs.site = Some(CompletionSite::Variant { type_id: provided_type });
        }

        match self.types.get(provided_type) {
            Type::Sum(_s) => Ok(()),
            Type::Generic(g) => match self.types.get(g.inner).as_sum() {
                None => {
                    kbail!(
                        self,
                        span,
                        "Expected a sum type; but this is a different generic: {}",
                        provided_type
                    );
                }
                Some(_s) => Ok(()),
            },
            Type::Enum(_e) => Ok(()),
            _ => Err(kerr!(self, parsed_variant.span, "Not a sum or enum type: {}", provided_type)),
        }?;

        let base_sum_or_generic_sum = self.types.get(provided_type);
        let variant_name = parsed_variant.variant_name;
        match base_sum_or_generic_sum {
            Type::Enum(e) => {
                let Some((matching_value_index, _matching_value)) =
                    self.enum_value_by_name(e.member_values, variant_name)
                else {
                    kbail!(self, span, "No member {} in enum {}", variant_name, provided_type);
                };
                let enum_expr = self.exprs.add(
                    TypedExpr::Enum(EnumConstructor { value_index: matching_value_index as u32 }),
                    provided_type,
                    span,
                );
                self.emit_ls_entity(
                    parsed_variant.name_span,
                    LsEntityKind::Variant {
                        type_id: provided_type,
                        variant_index: matching_value_index as u32,
                    },
                );
                Ok(enum_expr)
            }
            Type::Sum(e) => {
                if let Some(_variant) = self.sum_variant_by_name(e.variants, variant_name) {
                    let sum_constructor = self.eval_sum_constructor(
                        provided_type,
                        variant_name,
                        parsed_variant.payload,
                        ctx,
                        parsed_variant.name_span,
                    )?;
                    Ok(sum_constructor)
                } else {
                    Err(kerr!(self, span, "No variant {} on type {}", variant_name, provided_type))
                }
            }
            Type::Generic(g) => {
                let Some(inner_sum) = self.types.get(g.inner).as_sum() else {
                    self.ice_span(span, "checked to be sum")
                };
                let Some(generic_variant) =
                    self.sum_variant_by_name(inner_sum.variants, variant_name)
                else {
                    kbail!(self, span, "No variant {} on type {}", variant_name, g.inner);
                };
                let g_params = g.params;

                let payload_if_needed = match (generic_variant.payload, parsed_variant.payload) {
                    // `empty` payloads can be elided, so we let this through without a payload; it
                    // will be rejected later; all we care about here is collecting information for
                    // inference, and there's no information in an elided payload
                    (Some(_), None) => None,
                    (Some(generic_payload_type_id), Some(payload_parsed_expr)) => {
                        Some((generic_payload_type_id, payload_parsed_expr))
                    }
                    (None, None) => None,
                    (None, Some(_payload_expr)) => {
                        kbail!(
                            self,
                            span,
                            "Variant {} does not take a payload",
                            generic_variant.name
                        );
                    }
                };

                let solved_or_passed_type_params: TypeSliceId = if parsed_variant
                    .type_args
                    .is_empty()
                {
                    match payload_if_needed {
                        None => {
                            match ctx.expected_type_id.map(|t| (t, self.get_instance_info(t))) {
                                Some((expected_type, Some(spec_info))) => {
                                    // We're expecting a specific instance of a generic sum
                                    if spec_info.generic_parent == provided_type {
                                        // Solved params
                                        spec_info.type_args
                                    } else {
                                        kbail!(
                                            self,
                                            span,
                                            "Cannot infer a type for {}; expected mismatching generic type {}",
                                            self.name_of_type(provided_type),
                                            expected_type
                                        );
                                    }
                                }
                                _ => {
                                    kbail!(
                                        self,
                                        span,
                                        "Cannot infer a type for {}",
                                        self.name_of_type(provided_type)
                                    );
                                }
                            }
                        }
                        Some((generic_variant_payload, payload)) => {
                            let mut args_and_params: SV4<InferenceInputPair> = smallvec![];

                            // There are only ever up to 2 'cases' to power inference
                            // - The expected return type together with the type of the sum itself
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
                            let g_params_slice = self.mem.getn(g_params);
                            let (solutions, _all_solutions) = self
                                .infer_types(
                                    g_params_slice,
                                    g_params,
                                    &args_and_params,
                                    span,
                                    ctx.scope_id,
                                    None,
                                )
                                .map_err(|f| self.render_inference_failure(f))?;
                            self.intern_type_args(solutions)
                        }
                    }
                } else {
                    let mut passed_params: List<TypeId, MemTmp> = self.tmp.new_list(g_params.len());
                    for passed_type_arg in self.ast.mem.getn(parsed_variant.type_args) {
                        let Some(passed_type_expr) = passed_type_arg.type_expr else {
                            kbail!(self, span, "Wildcard type _ is not yet supported here");
                        };
                        let type_id = self.eval_type_expr(passed_type_expr, ctx.scope_id)?;
                        passed_params.push(type_id);
                    }
                    self.intern_type_slice(passed_params.as_slice())
                };

                let concrete_type = self.instantiate_generic_type(
                    provided_type,
                    self.get_type_slice(solved_or_passed_type_params),
                );
                let sum_constr = self.eval_sum_constructor(
                    concrete_type,
                    variant_name,
                    parsed_variant.payload,
                    ctx,
                    span,
                )?;
                Ok(sum_constr)
            }
            _ => self.ice_span(span, "should be a Sum, Enum, or Generic"),
        }
    }

    fn align_call_arguments_with_parameters(
        &mut self,
        fn_call: &ParsedCall,
        signature: &FunctionSignature,
        // If a method call and we've already compiled the first argument
        // this is it, we should use it instead of the fn_call's first arg
        method_receiver: Option<TypedExprId>,
        params: MSlice<FnParamType, TypedProgram>,
        known_typed_args: Option<&[TypedExprId]>,
        calling_scope: ScopeId,
        tolerate_missing_context_args: bool,
        skip_leading_receiver_arg: bool,
    ) -> K1Result<ArgsAndParams> {
        let fn_name = fn_call.name.name;
        let span = fn_call.span;
        let args_slice = self.ast.mem.getn(fn_call.args);
        let args_slice = if skip_leading_receiver_arg { &args_slice[1..] } else { args_slice };
        let explicit_context_args = args_slice.iter().any(|a| a.is_explicit_context);
        let named = args_slice.first().is_some_and(|arg| arg.name.is_some());
        let mut final_args: TmpList<MaybeTypedExpr> = self.tmp.new_list(params.len());
        let mut final_params: TmpList<FnParamType> = self.tmp.new_list(params.len());
        let all_params = self.mem.getn(params);
        if !explicit_context_args {
            for context_param in all_params.iter().filter(|p| p.is_context) {
                let resolved = self.resolve_context_arg(
                    context_param,
                    context_param.type_id,
                    calling_scope,
                    fn_call,
                    tolerate_missing_context_args,
                )?;
                if let Some(arg) = resolved {
                    final_args.push(arg);
                    final_params.push(*context_param);
                }
            }
        }

        let is_lambda = all_params.first().is_some_and(|p| p.is_lambda_env);
        let params_slice = if is_lambda { &all_params[1..] } else { all_params };
        let explicit_param_count = params_slice.iter().filter(|p| !p.is_context).count();
        let context_param_count = params_slice.len() - explicit_param_count;
        let total_expected =
            if explicit_context_args { params_slice.len() } else { explicit_param_count };
        let actual_passed_args = args_slice;
        let total_passed = match known_typed_args {
            None => actual_passed_args.len(),
            Some(pre_evaled_params) => pre_evaled_params.len(),
        };

        let expected_literal_params = params_slice
            .iter()
            // If the user opted to pass context params explicitly, then check all params
            // If the user did not, then just check the non-context params, since the compiler is responsible
            // for looking up context params
            .filter(|p| explicit_context_args || !p.is_context);

        if let Some(pre_evaled_params) = known_typed_args {
            for (expr, param) in pre_evaled_params.iter().zip(expected_literal_params) {
                final_args.push(MaybeTypedExpr::Typed(*expr));
                final_params.push(*param)
            }
        } else {
            let first_non_context_param_index =
                if explicit_context_args { context_param_count } else { 0 };
            for (param_index, fn_param) in expected_literal_params.enumerate() {
                // The first parameter that isn't an explicit `context` param is the method self
                // param; if we've already compiled that term, use it here
                if let Some(method_receiver) = method_receiver
                    && param_index == first_non_context_param_index
                {
                    final_args.push(MaybeTypedExpr::Typed(method_receiver));
                    final_params.push(*fn_param);
                    continue;
                }
                let matching_argument = if named {
                    let Some(name_match) =
                        actual_passed_args.iter().find(|arg| arg.name == Some(fn_param.name))
                    else {
                        kbail!(
                            self,
                            fn_call.span,
                            "Missing named argument for parameter {}",
                            fn_param.name
                        );
                    };
                    if let Some(dupe) =
                        final_params.iter().find(|param| param.name == name_match.name.unwrap())
                    {
                        let span = self.ast.exprs.get_span(name_match.value);
                        kbail!(self, span, "Duplicate named argument: {}", dupe.name);
                    };
                    Some(name_match)
                } else {
                    actual_passed_args.get(param_index)
                };
                let Some(param) = matching_argument else {
                    kbail!(
                        self,
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

        // We accounted for every parameter; now we just need to ensure no extras were passed
        if total_passed > total_expected {
            kbail!(
                self,
                span,
                "Too many arguments to {}: expected {}, got {}",
                self.function_signature_to_string(signature),
                total_expected,
                total_passed
            );
        }

        Ok(ArgsAndParams { args: final_args.to_slice(), params: final_params.to_slice() })
    }

    fn resolve_context_arg(
        &mut self,
        context_param: &FnParamType,
        constraint_source_type: TypeId,
        calling_scope: ScopeId,
        fn_call: &ParsedCall,
        tolerate_missing: bool,
    ) -> K1Result<Option<MaybeTypedExpr>> {
        let span = fn_call.span;
        let ability_match = self.find_context_variable_by_ability_constraints(
            calling_scope,
            constraint_source_type,
            span,
        )?;
        let matching_context_variable = match ability_match {
            Some(v) => {
                if constraint_source_type != context_param.type_id
                    && self.variables.get(v).type_id != context_param.type_id
                {
                    let found = self.variables.get(v);
                    kbail!(
                        self,
                        span,
                        "Context variable '{}' has type {}, but this call requires context parameter '{}' of type {}; pass the context argument explicitly",
                        found.name,
                        found.type_id,
                        context_param.name,
                        context_param.type_id
                    );
                }
                Some(v)
            }
            None => self.scopes.find_context_variable_by_type(calling_scope, context_param.type_id),
        };
        if let Some(matching_context_variable) = matching_context_variable {
            let found = self.variables.get(matching_context_variable);
            self.check_lambda_capture_valid(
                calling_scope,
                matching_context_variable,
                found.owner_scope,
                context_param.name,
                span,
            )
            .map_err(|mut e| {
                e.message = self.ast.idents.intern(format!(
                    "This call needs context parameter '{}' from outside the lambda: {}",
                    self.ident_str(context_param.name),
                    self.ident_str(e.message)
                ));
                e
            })?;
            let found = self.variables.get(matching_context_variable);
            let expr = self.exprs.add(
                TypedExpr::Variable(VariableExpr { variable_id: matching_context_variable }),
                found.type_id,
                span,
            );
            self.register_variable_usage(matching_context_variable, fn_call.name.name_span);
            Ok(Some(MaybeTypedExpr::Typed(expr)))
        } else if context_param.type_id == self.builtin_types.source_location.unwrap() {
            let expr = self.synth_source_location(span);
            Ok(Some(MaybeTypedExpr::Typed(expr)))
        } else if !tolerate_missing {
            kbail!(
                self,
                span,
                "Missing context parameter '{}' of type {}",
                context_param.name,
                context_param.type_id
            );
        } else {
            debug!(
                "Tolerating a missing context argument of type {}. Let's try to infer one, one day",
                self.type_id_to_string(context_param.type_id)
            );
            Ok(None)
        }
    }

    fn respecialize_aligned_args(
        &mut self,
        original: &ArgsAndParams,
        generic_params: MSlice<FnParamType, TypedProgram>,
        specialized_params: MSlice<FnParamType, TypedProgram>,
        fn_call: &ParsedCall,
        calling_scope: ScopeId,
        skip_leading_receiver_arg: bool,
    ) -> K1Result<ArgsAndParams> {
        let span = fn_call.span;
        let args_slice = self.ast.mem.getn(fn_call.args);
        let args_slice = if skip_leading_receiver_arg { &args_slice[1..] } else { args_slice };
        let explicit_context_args = args_slice.iter().any(|a| a.is_explicit_context);
        let generic = self.mem.getn(generic_params);
        let specialized = self.mem.getn(specialized_params);
        let orig_args = self.tmp.getn(original.args);
        let orig_params = self.tmp.getn(original.params);
        let mut final_args: TmpList<MaybeTypedExpr> = self.tmp.new_list(specialized.len() as u32);
        let mut final_params: TmpList<FnParamType> = self.tmp.new_list(specialized.len() as u32);

        if !explicit_context_args {
            for (index, context_param) in
                specialized.iter().enumerate().filter(|(_, p)| p.is_context)
            {
                let generic_param = &generic[index];
                debug_assert_eq!(generic_param.name, context_param.name);
                let existing = orig_params.iter().position(|p| p.name == context_param.name);
                let arg = match existing {
                    Some(i) => {
                        if generic_param.type_id != context_param.type_id
                            && let MaybeTypedExpr::Typed(expr) = orig_args[i]
                            && self.exprs.get_type(expr) != context_param.type_id
                        {
                            kbail!(
                                self,
                                span,
                                "Context variable for parameter '{}' has type {}, but this call requires {}; pass the context argument explicitly",
                                context_param.name,
                                self.exprs.get_type(expr),
                                context_param.type_id
                            );
                        }
                        orig_args[i]
                    }
                    None => self
                        .resolve_context_arg(
                            context_param,
                            generic_param.type_id,
                            calling_scope,
                            fn_call,
                            false,
                        )?
                        .unwrap(),
                };
                final_args.push(arg);
                final_params.push(*context_param);
            }
        }

        for (arg, param) in orig_args.iter().zip(orig_params.iter()) {
            if !explicit_context_args && param.is_context {
                continue;
            }
            let specialized_param = specialized
                .iter()
                .find(|p| p.name == param.name)
                .expect("specialization preserves parameter names");
            final_args.push(*arg);
            final_params.push(*specialized_param);
        }

        Ok(ArgsAndParams { args: final_args.to_slice(), params: final_params.to_slice() })
    }

    fn splice_stashed_args(
        &mut self,
        args: TmpSlice<MaybeTypedExpr>,
        stashed_args: &[(ParsedExprId, TypedExprId)],
    ) {
        if stashed_args.is_empty() {
            return;
        }
        for arg in self.tmp.getn_mut(args) {
            if let MaybeTypedExpr::Parsed(p) = arg {
                if let Some((_, typed)) = stashed_args.iter().find(|(parsed, _)| parsed == p) {
                    *arg = MaybeTypedExpr::Typed(*typed);
                }
            }
        }
    }

    pub fn get_callee_function_signature(&self, callee: &Callee) -> FunctionSignature {
        match callee {
            Callee::StaticFunction(function_id) => self.get_function(*function_id).signature(),
            Callee::StaticLambda { function_id, .. } => self.get_function(*function_id).signature(),
            Callee::Abstract { function_sig, .. } => *function_sig,
            Callee::Builtin { function_sig, .. } => *function_sig,
            Callee::DynamicFunction { .. } => {
                let function_type = self.get_callee_function_type(callee);
                FunctionSignature::make_no_generics(None, function_type)
            }
            Callee::DynamicLambda(_) => {
                let function_type = self.get_callee_function_type(callee);
                FunctionSignature::make_no_generics(None, function_type)
            }
            Callee::DynamicAbilityFn { slot_function_type, .. } => {
                FunctionSignature::make_no_generics(None, *slot_function_type)
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
            Callee::Builtin { function_sig, .. } => function_sig.function_type,
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
            Callee::DynamicAbilityFn { slot_function_type, .. } => *slot_function_type,
            Callee::DynamicAbstract { function_sig, .. } => function_sig.function_type,
        }
    }

    pub fn get_callee_builtin(&self, callee: &Callee) -> Option<Builtin> {
        match callee {
            Callee::StaticFunction(function_id) | Callee::StaticLambda { function_id, .. } => {
                self.get_function(*function_id).builtin_type
            }
            Callee::Abstract { .. } => None,
            Callee::Builtin { builtin, .. } => Some(*builtin),
            Callee::DynamicFunction { .. } => None,
            Callee::DynamicLambda(_) => None,
            Callee::DynamicAbilityFn { .. } => None,
            Callee::DynamicAbstract { .. } => None,
        }
    }

    /// A completion cursor among a call's direct args is claimed by the call as a CallArg
    /// site, not by eval_variable; see EvalExprFlags::MarkerOwnedByCall
    fn find_completion_cursor_arg(&self, fn_call: &ParsedCall) -> Option<u32> {
        let cs = self.completion.as_ref()?;
        let position = self.ast.mem.getn(fn_call.args).iter().position(|arg| {
            match self.ast.exprs.get(arg.value) {
                ParsedExpr::Variable(v) => v.name.path.is_empty() && v.name.name == cs.marker,
                _ => false,
            }
        });
        position.map(|i| i as u32)
    }

    fn record_call_arg_site(
        &mut self,
        marker_arg_index: Option<u32>,
        function_id: Option<FunctionId>,
        scope_id: ScopeId,
    ) {
        let Some(arg_index) = marker_arg_index else { return };
        let Some(cs) = &mut self.completion else { return };
        match (function_id, &cs.site) {
            (Some(function_id), None | Some(CompletionSite::CallArg { .. })) => {
                cs.site = Some(CompletionSite::CallArg { function_id, arg_index, scope_id });
            }
            (None, None) => cs.site = Some(CompletionSite::Scope { scope_id }),
            _ => {}
        }
    }

    fn eval_function_call(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        known_callee: Option<Callee>,
    ) -> K1Result<TypedExprId> {
        let tmp_mark = self.tmp.mark();
        let result = self.eval_function_call_inner(fn_call, known_args, ctx, known_callee, &[]);
        self.tmp.reset_to(tmp_mark);
        result
    }

    fn eval_method_call_on_typed_receiver(
        &mut self,
        fn_call: &ParsedCall,
        receiver: (ParsedExprId, TypedExprId),
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let tmp_mark = self.tmp.mark();
        let result = self.eval_function_call_inner(fn_call, None, ctx, None, &[receiver]);
        self.tmp.reset_to(tmp_mark);
        result
    }

    fn eval_function_call_inner(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        known_callee: Option<Callee>,
        pre_typed_args: &[(ParsedExprId, TypedExprId)],
    ) -> K1Result<TypedExprId> {
        let span = fn_call.span;
        debug!("eval_function_call {}", self.qident_to_string(&fn_call.name));
        assert!(
            fn_call.args.is_empty() || known_args.is_none(),
            "cannot pass both typed value args and parsed value args to eval_function_call"
        );
        // Arguments already evaluated during resolution/inference; avoids double-compiles where possible
        let mut stashed_args: SV8<(ParsedExprId, TypedExprId)> = smallvec![];
        stashed_args.extend_from_slice(pre_typed_args);
        let marker_arg_index = self.find_completion_cursor_arg(fn_call);
        let ctx = if marker_arg_index.is_some() { ctx.with_ccursor_owned_by_call() } else { ctx };
        let call_resolution = match known_callee {
            None => {
                match self.resolve_parsed_call(fn_call, known_args.as_ref(), ctx, &mut stashed_args)
                {
                    Ok(resolution) => resolution,
                    Err(e) => {
                        self.record_call_arg_site(marker_arg_index, None, ctx.scope_id);
                        return Err(e);
                    }
                }
            }
            Some(callee) => CallResolution::Call(callee),
        };
        let (callee, method_receiver) = match call_resolution {
            CallResolution::OtherExpr(typed_expr_id) => {
                self.record_call_arg_site(marker_arg_index, None, ctx.scope_id);
                return Ok(typed_expr_id);
            }
            CallResolution::Call(callee) => (callee, None),
            CallResolution::MethodCall { callee, receiver } => (callee, Some(receiver)),
        };
        self.record_call_arg_site(marker_arg_index, callee.maybe_function_id(), ctx.scope_id);
        let skip_leading_receiver_arg = matches!(callee, Callee::DynamicAbilityFn { .. });
        let method_receiver = if skip_leading_receiver_arg { None } else { method_receiver };
        let is_method = method_receiver.is_some();

        if let Some(function_id) = callee.maybe_function_id() {
            if !ctx.is_hidden_calls() {
                self.emit_ls_entity(
                    fn_call.name.name_span,
                    LsEntityKind::Function { function_id, is_defn: false },
                );
            }

            if self.get_function(function_id).is_macro()
                && known_callee.is_none()
                && known_args.is_none()
            {
                if method_receiver.is_some() {
                    kbail!(self, span, "Method-position macros are not yet supported");
                }
                let type_args = self.ast.mem.getn(fn_call.type_args);
                let mut macro_args: SV8<_> = smallvec![];
                for arg in self.ast.mem.getn(fn_call.args) {
                    macro_args.push(*arg)
                }
                return match self.execute_macro_call(
                    type_args,
                    &macro_args,
                    fn_call.span,
                    function_id,
                    false,
                    ctx,
                )? {
                    StaticExecutionResult::TypedExpr(expr) => Ok(expr),
                    StaticExecutionResult::Definitions(_) => self
                        .ice_span(span, "Macro call in expression position produced definitions"),
                };
            }
        }

        // Special form: during manifest eval, `m.dep(name, .{ ... })` captures the params
        // struct literal as a ParsedExprId and retargets the call to k1/module/add-dep-impl
        let callee = 'dep_capture: {
            if !ctx.is_manifest_eval() {
                break 'dep_capture callee;
            }
            let Some(function_id) = callee.maybe_function_id() else {
                break 'dep_capture callee;
            };
            if self.get_function(function_id).name != self.ast.idents.b.dep {
                break 'dep_capture callee;
            }
            let args = self.ast.mem.getn(fn_call.args);
            if args.len() != 3 {
                break 'dep_capture callee;
            }
            let module_ns_scope = {
                let k1_module = self.builtin_types.k1_module.unwrap();
                let ns_id = self.get_companion_namespace(k1_module).unwrap();
                self.namespaces.get(ns_id).scope_id
            };
            if self.scopes.find_function(module_ns_scope, self.ast.idents.b.dep)
                != Some(function_id)
            {
                break 'dep_capture callee;
            }
            let params_arg = args[2].value;
            let params_span = self.ast.exprs.get_span(params_arg);
            let ParsedExpr::Struct(_) = self.ast.exprs.get(params_arg) else {
                kbail!(self, params_span, "dep params must be a struct literal");
            };
            let add_dep_id =
                self.scopes.find_function(module_ns_scope, self.ast.idents.b.add_dep).unwrap();
            let params_value_id =
                self.static_values.add_int(TypedIntValue::U64(params_arg.as_u32() as u64));
            let params_expr =
                self.exprs.add_static(params_value_id, U64_TYPE_ID, false, params_span);
            stashed_args.push((params_arg, params_expr));
            Callee::StaticFunction(add_dep_id)
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let callee_function_type_id = self.get_callee_function_type(&callee);
        let signature = self.get_callee_function_signature(&callee);
        debug!("Callee is: {}", self.function_signature_to_string(&signature));
        let is_generic = signature.has_type_params();

        let original_function_type = self.types.get(callee_function_type_id).as_function().unwrap();
        let params = original_function_type.logical_params();

        let (callee, typechecked_arguments, type_args) = match is_generic {
            false => {
                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    &signature,
                    method_receiver,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                    skip_leading_receiver_arg,
                )?;
                self.splice_stashed_args(args_and_params.args, &stashed_args);
                let mut typechecked_args = self.mem.new_list(args_and_params.len());
                // The receiver is the first non-context param; context params come first
                // in the aligned args
                let receiver_index = if is_method {
                    self.tmp.getn(args_and_params.params).iter().position(|p| !p.is_context)
                } else {
                    None
                };
                for (index, (maybe_typed_expr, param)) in
                    self.tmp.getn_zip(args_and_params.args, args_and_params.params).enumerate()
                {
                    let is_method_receiver = receiver_index == Some(index);
                    let checked_expr = match *maybe_typed_expr {
                        MaybeTypedExpr::Typed(typed) => match self.check_and_coerce_expr(
                            param.type_id,
                            typed,
                            ctx.scope_id,
                            is_method_receiver,
                        ) {
                            Ok(checked_coerced) => checked_coerced,
                            Err(e) => {
                                kbail!(
                                    self,
                                    self.exprs.get_span(typed),
                                    "{}\nOccurred in pre-typed call parameter '{}.{}'",
                                    e.message,
                                    &fn_call.name,
                                    param.name,
                                );
                            }
                        },
                        MaybeTypedExpr::Parsed(parsed) => self
                            .eval_expr_with_coercion(
                                parsed,
                                ctx.with_expected_type(Some(param.type_id))
                                    .with_is_method_receiver(is_method_receiver),
                                true,
                            )
                            .map_err(|err| {
                                kerr!(
                                    self,
                                    err.span,
                                    "{}\nOccurred in call parameter '{}.{}'",
                                    err.message,
                                    &fn_call.name,
                                    param.name,
                                )
                            })?,
                    };
                    typechecked_args.push(checked_expr);
                }
                (callee, typechecked_args.to_slice(), TypeArgs::empty())
            }
            true => {
                let original_args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    &signature,
                    method_receiver,
                    params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    true,
                    skip_leading_receiver_arg,
                )?;
                self.splice_stashed_args(original_args_and_params.args, &stashed_args);

                // We infer the type arguments, or just use them if the user has supplied them
                let type_args = match &known_args {
                    Some((type_args, _va)) if !type_args.is_empty() => {
                        if type_args.len() != signature.type_params.len() as usize {
                            ice_span!(
                                self,
                                span,
                                "Bad known type args, expected {} but got {}",
                                signature.type_params.len(),
                                type_args.len(),
                            )
                        }
                        TypeArgs::from_slice_in(type_args, &mut self.mem)
                    }
                    _ => self.infer_and_constrain_call_type_args(
                        fn_call,
                        signature,
                        ctx,
                        &original_args_and_params,
                        &mut stashed_args,
                    )?,
                };

                let (fnlike_type_args, fnlike_type_arg_values) = self
                    .determine_fnlike_type_args_for_call(
                        signature,
                        type_args,
                        &original_args_and_params,
                        ctx,
                    )?;

                // A repeat call at already-specialized type args reuses that
                // specialization's type rather than re-substituting the signature
                let cached_specialization = match callee {
                    Callee::StaticFunction(function_id) => {
                        self.find_function_specialization(function_id, type_args, fnlike_type_args)
                    }
                    _ => None,
                };
                let specialized_function_type = match cached_specialization {
                    Some(function_id) => self.get_function(function_id).type_id,
                    None => self.substitute_in_function_signature(
                        type_args,
                        fnlike_type_args,
                        signature,
                    ),
                };
                let is_abstract =
                    self.get_type_variable_counts(specialized_function_type).is_abstract();

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
                            let function_id = match cached_specialization {
                                Some(specialized_function_id) => specialized_function_id,
                                None => self.specialize_function_declaration(
                                    type_args,
                                    fnlike_type_args,
                                    function_id,
                                ),
                            };
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
                let specialized_params = specialized_fn_type.logical_params();

                // context parameters the generic alignment tolerated as missing
                // are resolved here for real, now that their types are concrete.
                let args_and_params = self.respecialize_aligned_args(
                    &original_args_and_params,
                    params,
                    specialized_params,
                    fn_call,
                    ctx.scope_id,
                    skip_leading_receiver_arg,
                )?;

                // We've finished inference and all types are known; we now compile all the expressions
                // again to generate code with no holes and fully concrete types.
                let mut typechecked_args = self.mem.new_list(args_and_params.len());

                // We can skip re-evaluating everything if we're just here to learn the return types
                if !ctx.is_inference() {
                    // The receiver is the first non-context param; context params come
                    // first in the aligned args
                    let receiver_index = if is_method {
                        self.tmp.getn(args_and_params.params).iter().position(|p| !p.is_context)
                    } else {
                        None
                    };
                    for (param_index, (maybe_typed_expr, param)) in
                        self.tmp.getn_zip(args_and_params.args, args_and_params.params).enumerate()
                    {
                        let is_method_receiver = receiver_index == Some(param_index);
                        let allow_addr_of = is_method_receiver;
                        // Is this parameter a fnlike type parameter? If so, we already have the
                        // typechecked value expression
                        let matching_ftp_index = self
                            .mem
                            .getn(signature.fnlike_type_params)
                            .iter()
                            .position(|ftp| ftp.value_param_index as usize == param_index);
                        let expr = match matching_ftp_index {
                            Some(ftp_index) => {
                                let value = fnlike_type_arg_values[ftp_index];
                                value
                            }
                            None => match *maybe_typed_expr {
                                MaybeTypedExpr::Typed(typed) => {
                                    let checked_coerced = self.check_and_coerce_expr(
                                        param.type_id,
                                        typed,
                                        ctx.scope_id,
                                        allow_addr_of,
                                    )?;
                                    checked_coerced
                                }
                                MaybeTypedExpr::Parsed(parsed) => self
                                    .eval_expr_with_coercion(
                                        parsed,
                                        ctx.with_expected_type(Some(param.type_id))
                                            .with_is_method_receiver(is_method_receiver),
                                        true,
                                    )
                                    .map_err(|err| {
                                        kerr!(
                                            self,
                                            err.span,
                                            "Error in parameter '{}' in call to '{}''\n{}",
                                            param.name,
                                            &fn_call.name,
                                            err.message
                                        )
                                    })?,
                            },
                        };
                        typechecked_args.push(expr);
                    }
                }

                (final_callee, typechecked_args.to_slice(), type_args)
            }
        };
        self.record_call_arg_site(marker_arg_index, callee.maybe_function_id(), ctx.scope_id);

        // If any arguments definitely crash, we aren't calling the function at all.
        // So let's not generate a `Call`, but rather just the arguments expressions that should be
        // evaluated. This simplifies later compiler stages to not have to carve out special cases
        // for divergent expressions
        let typechecked_arguments_slice = self.mem.getn(typechecked_arguments);
        for (index, arg) in typechecked_arguments_slice.iter().enumerate() {
            if self.exprs.get_type(*arg) == NEVER_TYPE_ID {
                let exprs_so_far = &typechecked_arguments_slice[0..=index];
                return Ok(self.make_never_block(exprs_so_far, ctx.scope_id, span));
            }
        }

        let callee_function_type = self.get_callee_function_type(&callee);
        self.warn_large_arg_copies(typechecked_arguments, callee_function_type);
        let call_return_type =
            self.types.get(callee_function_type).as_function().unwrap().return_type;

        let call = Call {
            callee,
            args: typechecked_arguments,
            type_args,
            return_type: call_return_type,
            span,
        };

        // Builtins that are handled by the typechecking phase are implemented here.
        if let Some(builtin) = self.get_callee_builtin(&call.callee) {
            match builtin {
                Builtin::TyperInline(kind) => {
                    return self.handle_inline_builtin(call, kind, ctx);
                }
                // All other builtins can still be checked for correctness here
                _ => self.check_builtin(&call, builtin, ctx)?,
            }
        }

        let call_id = self.calls.add(call);
        Ok(self.exprs.add(TypedExpr::Call { call_id }, call_return_type, span))
    }

    const LARGE_ARG_COPY_BYTES: u32 = 1024;

    fn warn_large_arg_copies(&mut self, args: PermSlice<TypedExprId>, function_type_id: TypeId) {
        let Type::Function(function_type) = self.types.get(function_type_id) else { return };
        let params = function_type.logical_params();
        if args.len() != params.len() {
            return;
        }
        for i in 0..args.len() as usize {
            let param = *self.mem.get_nth(params, i);
            if param.is_macro_code {
                continue;
            }
            let Some(layout) = self.get_layout(param.type_id) else { continue };
            if layout.size < Self::LARGE_ARG_COPY_BYTES {
                continue;
            }
            let arg = *self.mem.get_nth(args, i);
            if !self.expr_is_place_read(arg) {
                continue;
            }
            let span = self.exprs.get_span(arg);
            self.report(kwarn!(
                self,
                span,
                "Implicit copy of {} bytes: parameter '{}' takes {} by value; consider a reference parameter",
                layout.size,
                param.name,
                param.type_id
            ));
        }
    }

    fn expr_is_place_read(&self, expr: TypedExprId) -> bool {
        match self.exprs.get(expr) {
            TypedExpr::Variable(_)
            | TypedExpr::Deref(_)
            | TypedExpr::StructFieldAccess(_)
            | TypedExpr::ArrayGetElement(_)
            | TypedExpr::SumGetPayload(_) => true,
            TypedExpr::Block(block) => {
                let last_stmt = self.mem.getn(block.statements).last().copied();
                if let Some(TypedStmt::Expr(trailing_expr, _)) =
                    last_stmt.map(|s| self.stmts.get(s))
                {
                    self.expr_is_place_read(*trailing_expr)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    ////////////////////////////////
    // End of handling function calls

    fn attempt_static_lift(&mut self, expr_id: TypedExprId) -> K1Result<TypedExprId> {
        // Take an arbitrary expression and do our very best to turn it into a statically-known
        // value. Easy examples that must work include:
        // - all scalar literals,
        // - string literals,
        // - Perhaps struct and sum literals, if all of their fields also meet the other criteria
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
            e => Err(kerr!(
                self,
                self.exprs.get_span(expr_id),
                "Expression type is unsupported for static lift: {}. For more complex values, use a #static expression instead",
                e.kind_name()
            )),
        };
        if let Ok(result) = result {
            debug_assert_eq!(
                self.types.get(self.exprs.get_type(result)).as_value_type().unwrap().family_type_id,
                self.exprs.get_type(expr_id)
            );
        }
        result
    }

    fn handle_inline_builtin(
        &mut self,
        call: Call,
        intrinsic: BuiltinTyperInline,
        _ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        let span = call.span;
        match intrinsic {
            BuiltinTyperInline::GetStaticValue => {
                let arg0 = *self.mem.get_nth(call.args, 0);
                let static_value_id_arg = self.exprs.get(arg0);
                // For now, require a literal. We could relax this and evaluate it
                // if that's useful
                // This is fun because we take a static value id pointing to an integer and interpret that
                // integer as a static value id!
                let TypedExpr::StaticValue(static_const) = static_value_id_arg else {
                    kbail!(self, span, "Argument must be an integer literal");
                };
                let StaticValue::Int(TypedIntValue::U64(u64_value)) =
                    self.static_values.get(static_const.value_id)
                else {
                    kbail!(self, span, "Argument must be a u64 literal");
                };
                let Some(static_value_id) = StaticValueId::from_u32(*u64_value as u32) else {
                    kbail!(self, span, "Invalid static value id: {}. Cannot be zero", u64_value);
                };
                let Some(_value) = self.static_values.get_opt(static_value_id) else {
                    kbail!(self, span, "No static value with given id: {}", static_value_id);
                };
                let type_id = self.get_static_value_type(static_value_id);
                Ok(self.exprs.add_static(static_value_id, type_id, false, span))
            }
            BuiltinTyperInline::StaticTypeToValue => {
                // intern fn staticTypeToValue[T, ST: value T](): T
                let value_type_arg = call.type_args.as_slice(&self.mem)[1];

                let return_type = call.return_type;
                let Type::StaticValue(value_type) = self.types.get(value_type_arg) else {
                    kbail!(
                        self,
                        span,
                        "Internal Error: 2nd type arg should be a value type: {}",
                        value_type_arg
                    );
                };
                if let Some(static_value_id) = value_type.value_id {
                    Ok(self.add_static_constant_expr(static_value_id, span))
                } else {
                    // Since the static type has no value, we know this is generic code
                    // and we just need to generate a term that typechecks, so a
                    // unit casted to the generic static type. We should probably invent
                    // a way to do this that doesn't require 2 nodes
                    let filler_expr = self.synth_phony(return_type, span);
                    Ok(filler_expr)
                }
            }
            BuiltinTyperInline::CompilerSourceLocation => {
                let source_location = self.synth_source_location(span);
                Ok(source_location)
            }
            BuiltinTyperInline::TypeId => {
                let type_id = call.type_args.as_slice(&self.mem)[0];
                self.register_type_metainfo(type_id);

                let type_id_expr = self.synth_type_id_literal(type_id, span);
                Ok(type_id_expr)
            }
            BuiltinTyperInline::TypeSize
            | BuiltinTyperInline::TypeStride
            | BuiltinTyperInline::TypeAlign => {
                let type_id = call.type_args.as_slice(&self.mem)[0];
                match self.get_physical_type(type_id) {
                    PhysicalTypeResult::No => {
                        if self.get_type_variable_counts(type_id).is_abstract() {
                            Ok(self.synth_phony(SIZE_TYPE_ID, span))
                        } else {
                            kbail!(self, span, "type {} has no size", type_id)
                        }
                    }
                    PhysicalTypeResult::Never => Ok(self.synth_phony(SIZE_TYPE_ID, span)),
                    PhysicalTypeResult::Infinite => Ok(self.synth_phony(SIZE_TYPE_ID, span)),
                    PhysicalTypeResult::Yes(_) => {
                        let layout = self.get_layout(type_id).unwrap();
                        let value_bytes = match intrinsic {
                            BuiltinTyperInline::TypeSize => layout.size as u64,
                            BuiltinTyperInline::TypeStride => layout.stride() as u64,
                            BuiltinTyperInline::TypeAlign => layout.align as u64,
                            _ => unreachable!(),
                        };
                        Ok(self.synth_i64(to_k1_size_u64(value_bytes), span))
                    }
                }
            }
            BuiltinTyperInline::EnumEquals => {
                let arg_a = *self.mem.get_nth(call.args, 0);
                let arg_b = *self.mem.get_nth(call.args, 1);
                let int_value_a = self.synth_enum_get_value(arg_a, span);
                let int_value_b = self.synth_enum_get_value(arg_b, span);
                Ok(self.synth_equals_call_simple(int_value_a, int_value_b, span))
            }
        }
    }

    fn check_builtin(
        &mut self,
        call: &Call,
        intrinsic: Builtin,
        _ctx: EvalExprContext,
    ) -> K1Result<()> {
        match intrinsic {
            Builtin::Ir(BuiltinIr::Bitcast) => {
                let type_from = call.type_args.as_slice(&self.mem)[0];
                let type_to = call.type_args.as_slice(&self.mem)[1];
                let Some(layout_from) = self.get_layout(type_from) else {
                    kbail!(self, call.span, "Cannot bitcast from unsized type: {}", type_from)
                };
                let Some(layout_to) = self.get_layout(type_to) else {
                    kbail!(self, call.span, "Cannot bitcast to unsized type: {}", type_to)
                };
                if layout_from.size == 0 {
                    kbail!(self, call.span, "Cannot bitcast from zero-sized type: {}", type_from)
                }
                if layout_to.size == 0 {
                    kbail!(self, call.span, "Cannot bitcast to zero-sized type: {}", type_to)
                }
                if layout_from.size != layout_to.size {
                    kbail!(
                        self,
                        call.span,
                        "Cannot bitcast between types of different sizes: {} (size {}) -> {} (size {})",
                        type_from,
                        layout_from.size,
                        type_to,
                        layout_to.size
                    )
                }
                Ok(())
            }
            Builtin::Ir(BuiltinIr::Zeroed) => {
                let type_id = call.type_args.as_slice(&self.mem)[0];
                self.warn_if_not_zerosafe(type_id, call.span);
                Ok(())
            }
            Builtin::Ir(
                b @ (BuiltinIr::AtomicLoad
                | BuiltinIr::AtomicStore
                | BuiltinIr::AtomicRmw(_)
                | BuiltinIr::AtomicCmpxchg { .. }
                | BuiltinIr::AtomicFence),
            ) => self.check_atomic_builtin(call, b),
            _ => Ok(()),
        }
    }

    /// Atomic orderings must be compile-time-known
    pub fn atomic_ordering_arg(&self, call: &Call, arg_index: usize) -> K1Result<AtomicOrderingIr> {
        let expr_id = *self.mem.get_nth(call.args, arg_index);
        let span = self.exprs.get_span(expr_id);
        let TypedExpr::Enum(ec) = self.exprs.get(expr_id) else {
            kbail!(self, span, "atomic ordering must be an ordering literal like `:seq-cst`");
        };
        let type_id = self.exprs.get_type(expr_id);
        let Some(enum_type) = self.types.get(type_id).as_enum() else {
            kbail!(self, span, "atomic ordering must be an `atomic/ordering` literal");
        };
        let member = self.mem.getn(enum_type.member_values)[ec.value_index as usize];
        match self.ident_str(member.name) {
            "relaxed" => Ok(AtomicOrderingIr::Relaxed),
            "acquire" => Ok(AtomicOrderingIr::Acquire),
            "release" => Ok(AtomicOrderingIr::Release),
            "acq-rel" => Ok(AtomicOrderingIr::AcqRel),
            "seq-cst" => Ok(AtomicOrderingIr::SeqCst),
            other => Err(kerr!(self, span, "unknown atomic ordering `:{}`", other)),
        }
    }

    fn check_atomic_builtin(&self, call: &Call, builtin: BuiltinIr) -> K1Result<()> {
        match builtin {
            BuiltinIr::AtomicLoad => {
                let ord = self.atomic_ordering_arg(call, 1)?;
                if matches!(ord, AtomicOrderingIr::Release | AtomicOrderingIr::AcqRel) {
                    kbail!(self, call.span, "atomic load cannot use `:{}` ordering", ord.name());
                }
                Ok(())
            }
            BuiltinIr::AtomicStore => {
                let ord = self.atomic_ordering_arg(call, 2)?;
                if matches!(ord, AtomicOrderingIr::Acquire | AtomicOrderingIr::AcqRel) {
                    kbail!(self, call.span, "atomic store cannot use `:{}` ordering", ord.name());
                }
                Ok(())
            }
            BuiltinIr::AtomicRmw(_) => {
                self.atomic_ordering_arg(call, 2)?;
                Ok(())
            }
            BuiltinIr::AtomicCmpxchg { .. } => {
                let success = self.atomic_ordering_arg(call, 3)?;
                let failure = self.atomic_ordering_arg(call, 4)?;
                if matches!(failure, AtomicOrderingIr::Release | AtomicOrderingIr::AcqRel) {
                    kbail!(
                        self,
                        call.span,
                        "atomic cmpxchg failure ordering cannot be `:{}`",
                        failure.name()
                    );
                }
                if failure.to_tag() > success.to_tag() {
                    kbail!(
                        self,
                        call.span,
                        "atomic cmpxchg failure ordering `:{}` cannot be stronger than success ordering `:{}`",
                        failure.name(),
                        success.name()
                    );
                }
                Ok(())
            }
            BuiltinIr::AtomicFence => {
                let ord = self.atomic_ordering_arg(call, 0)?;
                if ord == AtomicOrderingIr::Relaxed {
                    kbail!(
                        self,
                        call.span,
                        "atomic fence requires an ordering stronger than `:relaxed`"
                    );
                }
                Ok(())
            }
            _ => unreachable!("check_atomic_builtin on non-atomic builtin"),
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
        let all_base_params = base_ability.parameters;
        if all_base_params.is_empty() {
            // Special case if the ability has no params at all, e.g., Comparable
            return signature;
        }
        let old_impl_arguments = signature.impl_arguments;
        let old_impl_ability_arguments = specialized_ability.kind.arguments(self);
        debug!(
            "Specializing constraint sig: {} on set {}",
            self.ability_impl_signature_to_string(
                signature.specialized_ability_id,
                old_impl_arguments
            ),
            self.pretty_print_type_substitutions(set, ", ")
        );
        let mut ability_args_new: List<TypeId, MemTmp> = self.tmp.new_list(all_base_params.len());
        let mut impl_args_new: List<TypeId, _> = self.mem.new_list(all_base_params.len());
        for (index, ability_param) in
            self.mem.getn(all_base_params).iter().filter(|p| !p.is_impl_param).enumerate()
        {
            let previous_value = old_impl_ability_arguments[index];
            let substituted = self.substitute_in_type(previous_value, set);
            ability_args_new.push(substituted);
            debug!(
                "> Did ability param {} -> {}",
                self.type_id_to_string_ext(
                    ability_param.type_variable_id,
                    dump::TypeDisplayMode::Expand
                ),
                self.type_id_to_string_ext(substituted, dump::TypeDisplayMode::Expand)
            );
        }
        for (index, impl_param) in
            self.mem.getn(all_base_params).iter().filter(|p| p.is_impl_param).enumerate()
        {
            let previous_value = *self.mem.get_nth(old_impl_arguments, index);
            let substituted = self.substitute_in_type(previous_value, set);
            impl_args_new.push(substituted);
            debug!(
                "> Did impl param {} -> {}",
                self.type_id_to_string_ext(
                    impl_param.type_variable_id,
                    dump::TypeDisplayMode::Expand
                ),
                self.type_id_to_string_ext(substituted, dump::TypeDisplayMode::Expand)
            );
        }
        let ability_args_new_handle = self.intern_type_slice(ability_args_new.as_slice());
        let impl_args_new_handle = impl_args_new.to_slice();
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
            Type::Lambda(lam_id) => Some(self.lambda_types.get(*lam_id).function_type),
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

    pub fn string_type_id(&self) -> TypeId {
        self.builtin_types.string()
    }

    fn substitute_in_function_signature(
        &mut self,
        // Must 'zip' up with each type param
        type_arguments: TypeArgs,
        // Must 'zip' up with each function type param
        fnlike_type_arguments: TypeArgs,
        generic_function_sig: FunctionSignature,
    ) -> TypeId {
        //let generic_function = self.get_function(generic_function_id);
        let generic_function_type_id = generic_function_sig.function_type;
        let mut subst_pairs: SV8<TypeSubstitutionPair> = smallvec![];

        // Here, we're substituting **the entire function type params** for the function types we
        // have. The pairs look like "some T -> T" -> "(int -> int)*"
        for (function_type_param, function_type_arg) in self
            .mem
            .getn(generic_function_sig.fnlike_type_params)
            .iter()
            .zip(fnlike_type_arguments.as_slice(&self.mem))
        {
            subst_pairs.push(TypeSubstitutionPair {
                from: function_type_param.type_id,
                to: *function_type_arg,
            })
        }

        // Here, we're substituting the actual 'normal' type params as well,
        // such as T, U in fn makePair[T, U](t: T, u: U)
        for (gen_param, type_arg) in self
            .mem
            .getn(generic_function_sig.type_params)
            .iter()
            .zip(type_arguments.as_slice(&self.mem))
        {
            subst_pairs.push(TypeSubstitutionPair { from: *gen_param, to: *type_arg });
        }
        let specialized_function_type_id =
            self.substitute_in_type(generic_function_type_id, &subst_pairs);
        debug!(
            "specialized function type: {}",
            self.type_id_to_string(specialized_function_type_id)
        );
        specialized_function_type_id
    }

    fn find_function_specialization(
        &mut self,
        generic_function_id: FunctionId,
        type_arguments: TypeArgs,
        fnlike_type_arguments: TypeArgs,
    ) -> Option<FunctionId> {
        let type_arguments = self.intern_type_args(type_arguments);
        let fnlike_type_arguments = self.intern_type_args(fnlike_type_arguments);
        self.function_specializations
            .get(&(generic_function_id, type_arguments, fnlike_type_arguments))
            .copied()
    }

    fn specialize_function_declaration(
        &mut self,
        // 1 type argument per type parameter
        type_arguments: TypeArgs,
        // 1 type argument per fnlike param
        fnlike_type_arguments: TypeArgs,
        generic_function_id: FunctionId,
    ) -> FunctionId {
        let generic_function = self.get_function(generic_function_id);
        let generic_function_param_variables = generic_function.params;
        let generic_function_scope = generic_function.scope;
        let generic_signature = generic_function.signature();
        let is_typer_function_builtin =
            matches!(generic_function.builtin_type, Some(Builtin::TyperPhysicalFunction(_)));

        debug_assert_eq!(type_arguments.len(), generic_function.type_params.len());

        if let Some(specialized_function_id) = self.find_function_specialization(
            generic_function_id,
            type_arguments,
            fnlike_type_arguments,
        ) {
            return specialized_function_id;
        }
        let specialized_function_type_id = self.substitute_in_function_signature(
            type_arguments,
            fnlike_type_arguments,
            generic_signature,
        );
        let specialized_function_id = self.functions.next_id();
        debug!(
            "specialized function type using {}: {}",
            self.pretty_print_types(type_arguments.as_slice(&self.mem), ", "),
            self.type_id_to_string(specialized_function_type_id)
        );

        let specialized_function_type =
            self.types.get(specialized_function_type_id).as_function().unwrap();

        let spec_fn_scope = self.scopes.add_sibling_scope(
            generic_function_scope,
            ScopeType::FunctionScope,
            ScopeOwnerId::None,
        );

        for (gen_param, type_arg) in self
            .mem
            .getn(generic_signature.type_params)
            .iter()
            .zip(type_arguments.as_slice(&self.mem))
        {
            let param_name = self.get_type_parameter(*gen_param).name;
            let _ = self.scopes.add_type(spec_fn_scope, param_name, *type_arg);
            let _ = self.scopes.add_type_substitution(spec_fn_scope, *gen_param, *type_arg);
        }

        let mut param_variables =
            self.mem.new_list(specialized_function_type.physical_params.len());
        for (specialized_param_type, generic_param) in self
            .mem
            .getn(specialized_function_type.physical_params)
            .iter()
            .zip(self.mem.getn(generic_function_param_variables))
        {
            let name = self.variables.get(generic_param.variable_id).name;
            let mut flags = VariableFlags::empty();
            flags.set(VariableFlags::Context, specialized_param_type.is_context);
            let variable_id = self.variables.add(Variable {
                type_id: specialized_param_type.type_id,
                name,
                owner_scope: spec_fn_scope,
                kind: VariableKind::FnParam(specialized_function_id),
                flags,
                usage_count: 0,
                defn_span: generic_param.span,
            });
            if specialized_param_type.is_context {
                let added = self.scopes.add_context_variable(
                    spec_fn_scope,
                    name,
                    variable_id,
                    specialized_param_type.type_id,
                );
                if !added {
                    // Substitution collapsed two context param types into one; the first
                    // registration wins the type key, but the name must still bind
                    self.scopes.add_variable(spec_fn_scope, name, variable_id);
                }
                // Ability keys derive from the *generic* param's constraints so generic
                // and specialized bodies resolve identically
                let generic_param_type = self.variables.get(generic_param.variable_id).type_id;
                self.register_context_param_ability_keys(
                    spec_fn_scope,
                    variable_id,
                    generic_param_type,
                );
            } else {
                self.scopes.add_variable(spec_fn_scope, name, variable_id);
            }
            param_variables.push(TypedFunctionParam { variable_id, span: generic_param.span })
        }
        let specialization_info = SpecializationInfo {
            parent_function: generic_function_id,
            type_arguments: self.intern_type_args(type_arguments),
            fnlike_type_arguments: self.intern_type_args(fnlike_type_arguments),
            specialized_function_id: FunctionId::PENDING,
            specialized_function_type: specialized_function_type_id,
        };
        let generic_function = self.get_function(generic_function_id);
        let has_body = match generic_function.parsed_id {
            ParsedId::Function(f) => self.ast.get_function(f).body.is_some(),
            ParsedId::Macro(_) => true,
            _ => panic!("Expected function or macro"),
        };
        let flags = generic_function.flags
            & (TypedFunctionFlags::CompilerDebug
                | TypedFunctionFlags::Macro
                | TypedFunctionFlags::ModuleManifest
                | TypedFunctionFlags::AbiNative);
        let specialized_function = TypedFunction {
            name: generic_function.name,
            scope: spec_fn_scope,
            namespace_id: generic_function.namespace_id,
            params: param_variables.to_slice(),
            // Must be empty for correctness; a specialized function has no type parameters!
            type_params: MSlice::empty(),
            // Must be empty for correctness; a specialized function has no function type parameters!
            fnlike_type_params: MSlice::empty(),
            ability_where_constraints: generic_function.ability_where_constraints,
            body_block: None,
            builtin_type: generic_function.builtin_type,
            linkage: generic_function.linkage,
            specialization_info: Some(specialization_info),
            parsed_id: generic_function.parsed_id,
            type_id: specialized_function_type_id,
            kind: generic_function.kind,
            flags,
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        };
        let actual_specialized_function_id = self.add_function(specialized_function);
        debug_assert_eq!(specialized_function_id, actual_specialized_function_id);
        let is_concrete = self.get_function(specialized_function_id).is_concrete();

        self.scopes
            .set_scope_owner_id(spec_fn_scope, ScopeOwnerId::Function(specialized_function_id));

        if (has_body && is_concrete) || is_typer_function_builtin {
            self.functions_pending_body_specialization.push(specialized_function_id);
        }

        specialized_function_id
    }

    fn specialize_function_body(&mut self, function_id: FunctionId) -> K1Result<()> {
        let specialized_function = self.get_function(function_id);
        if specialized_function.body_failure.is_some() || specialized_function.body_block.is_some()
        {
            return Ok(());
        }
        let result = self.specialize_function_body_inner(function_id);
        if let Err(e) = result {
            self.get_function_mut(function_id).body_failure = Some(e);
        }
        result
    }

    fn specialize_function_body_inner(&mut self, function_id: FunctionId) -> K1Result<()> {
        let specialized_function = self.get_function(function_id);
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
        if let Some(err) = parent_function.body_failure {
            kbail!(
                self,
                err.span,
                "Cannot specialize '{}': its definition failed to compile",
                parent_function.name
            );
        }

        // Intrinsics from generic impls (e.g. `impl add for vector[t, n]`) have no
        // body to specialize; calls resolve through the copied builtin_type
        if parent_function.linkage == Linkage::Intrinsic && parent_function.body_block.is_none() {
            return Ok(());
        }

        // Approach: Synthesize the implementation for this builtin
        if let Some(Builtin::TyperPhysicalFunction(
            kind @ (BuiltinTyperFunction::StructPrintTo
            | BuiltinTyperFunction::SumPrintTo
            | BuiltinTyperFunction::EnumPrintTo),
        )) = parent_function.builtin_type
        {
            let body_expr_id = self.generate_intrinsic_function_body(
                function_id,
                specialized_function_scope_id,
                kind,
            )?;
            self.get_function_mut(function_id).body_block = Some(body_expr_id);

            return Ok(());
        };

        // Approach: Just compile the AST again, with bound types
        debug_assert!(specialized_function.body_block.is_none());

        let parsed_body = match parent_function.parsed_id {
            ParsedId::Function(f) => self.ast.get_function(f).body.unwrap(),
            ParsedId::Macro(m) => self.ast.get_macro(m).body,
            _ => panic!("expected function or macro"),
        };
        let ParsedExpr::Block(parsed_body_block) = *self.ast.exprs.get(parsed_body) else {
            kbail!(
                self,
                self.ast.exprs.get_span(parsed_body),
                "[bug] went to specialized function but body expr is not a block"
            );
        };
        let typed_body = self.eval_block(
            &parsed_body_block,
            EvalExprContext::make(specialized_function_scope_id)
                .with_expected_type(Some(specialized_return_type)),
            true,
        )?;

        let body_type = self.exprs.get_type(typed_body);
        if let Err(msg) =
            self.check_types(specialized_return_type, body_type, specialized_function_scope_id)
        {
            kbail!(
                self,
                self.get_span_responsible_for_expr_type(typed_body),
                "[bug] Function body type mismatch: {}\n [occurred in specialization; should not be possible]. signature is: {}",
                msg,
                specialized_function_type
            );
        }

        self.get_function_mut(function_id).body_block = Some(typed_body);

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
        if function.is_generic() {
            return false;
        }
        // If we specialized on something generic, but we don't accept or return it in our
        // signature, we won't catch it by checking the signature!
        // Example: fn typeOnly[T: static u32](): unit
        // If specialized on static[u32, <none>], wouldn't have any generics in its signature
        if let Some(spec_info) = function.specialization_info {
            for t in self.get_type_slice(spec_info.type_arguments) {
                if self.type_variable_counts.get(*t).is_abstract() {
                    return false;
                }
            }
            for t in self.get_type_slice(spec_info.fnlike_type_arguments) {
                if self.type_variable_counts.get(*t).is_abstract() {
                    return false;
                }
            }
        }
        let info = self.get_type_variable_counts(function.type_id);
        let has_no_abstract_types_in_signature = !info.is_abstract();
        has_no_abstract_types_in_signature
    }

    fn eval_stmt(
        &mut self,
        stmt: ParsedStmtId,
        ctx: EvalExprContext,
        coerce_expr: bool,
        stmt_index: usize,
    ) -> K1Result<Option<TypedStmtId>> {
        match self.ast.stmts.get(stmt) {
            ParsedStmt::Use(use_stmt) => {
                let parsed_use = *self.ast.uses.get_use(use_stmt.use_id);
                // These uses should always hit since we only do 1 pass inside function bodies, and
                // at that point all symbols are resolvable
                let useable_symbols =
                    self.find_useable_symbols(ctx.scope_id, &parsed_use.target, true)?;
                if useable_symbols.is_empty() {
                    kbail!(
                        self,
                        parsed_use.target.name_span,
                        "Could not find {}",
                        parsed_use.target.name
                    );
                };
                let provenance =
                    if parsed_use.exposed { Provenance::UseExposed } else { Provenance::Use };
                for useable_symbol in &useable_symbols {
                    self.scopes.add_use_binding(
                        ctx.scope_id,
                        useable_symbol,
                        parsed_use.alias.unwrap_or(parsed_use.target.name),
                        provenance,
                    );
                }
                Ok(None)
            }
            ParsedStmt::Let(parsed_let) => {
                static_assert_size!(parse::ParsedLet, 28);
                let parsed_let = *parsed_let;
                let annotated_type = match parsed_let.type_expr.as_ref() {
                    None => None,
                    Some(&type_expr) => Some(self.eval_type_expr_ext(
                        type_expr,
                        ctx.scope_id,
                        EvalTypeExprContext::VARIABLE_BINDING,
                    )?),
                };
                let maybe_return_type_from_function = if parsed_let.is_returned() {
                    let scope_type = self.scopes.get_scope(ctx.scope_id).scope_type;
                    if stmt_index != 0
                        || (scope_type != ScopeType::FunctionScope
                            && scope_type != ScopeType::LambdaScope)
                    {
                        kbail!(
                            self,
                            parsed_let.span,
                            "let(returned) must be the first statement in a function block; (block type was {})",
                            scope_type.short_name()
                        );
                    }
                    if let Some(_rv) = self.get_returned_var_for_scope(ctx.scope_id) {
                        kbail!(self, parsed_let.span, "There is already a returned let");
                    }
                    let expected_return =
                        self.get_return_type_for_scope(ctx.scope_id, parsed_let.span)?;
                    Some(expected_return)
                } else {
                    None
                };
                match (annotated_type, maybe_return_type_from_function) {
                    (Some(t1), Some(t2)) if t1 != t2 => {
                        let expected_type_span =
                            self.ast.get_type_expr_span(parsed_let.type_expr.unwrap());
                        kbail!(self, expected_type_span, "Must return {}", t2);
                    }
                    _ => {}
                };
                let provided_type = maybe_return_type_from_function.or(annotated_type);

                let expected_rhs_type = provided_type;

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

                if actual_type == Some(NEVER_TYPE_ID) {
                    let never_expr = value_expr.unwrap();
                    let never_stmt = self.stmts.add(TypedStmt::Expr(never_expr, NEVER_TYPE_ID));
                    Ok(Some(never_stmt))
                } else {
                    let variable_type = match actual_type {
                        None => match provided_type {
                            None => {
                                kbail!(self, parsed_let.span, "Uninit let requires a type");
                            }
                            Some(t) => t,
                        },
                        Some(actual_type) => actual_type,
                    };

                    let mut flags = VariableFlags::empty();

                    flags.set(VariableFlags::Context, parsed_let.is_context());
                    flags.set(VariableFlags::Returned, parsed_let.is_returned());
                    let stmt_id = self.stmts.next_id();
                    let variable_id = self.variables.add(Variable {
                        name: parsed_let.name,
                        type_id: variable_type,
                        owner_scope: ctx.scope_id,
                        kind: VariableKind::Stack(stmt_id),
                        flags,
                        usage_count: 0,
                        defn_span: parsed_let.span,
                    });

                    if parsed_let.is_returned() {
                        if let Some(lambda_scope) = self.scopes.nearest_parent_lambda(ctx.scope_id)
                        {
                            let returned_var_ref = &mut self
                                .scopes
                                .lambda_info
                                .get_mut(&lambda_scope)
                                .unwrap()
                                .returned_variable;
                            if let Some(_returned_var_ref) = returned_var_ref {
                                kbail!(
                                    self,
                                    parsed_let.span,
                                    "There is already a returned variable for this lambda"
                                );
                            }
                            *returned_var_ref = Some(variable_id)
                        } else if let Some(function_id) =
                            self.scopes.nearest_parent_function(ctx.scope_id)
                        {
                            let returned_var_ref =
                                &mut self.functions.get_mut(function_id).returned_variable;

                            if let Some(_returned_var_ref) = returned_var_ref {
                                kbail!(
                                    self,
                                    parsed_let.span,
                                    "There is already a returned variable for this function"
                                );
                            }
                            *returned_var_ref = Some(variable_id)
                        }
                    }
                    let val_def_stmt = TypedStmt::Let(LetStmt {
                        variable_type,
                        variable_id,
                        initializer: value_expr,
                        span: parsed_let.span,
                    });
                    if parsed_let.is_context() {
                        let added = self.scopes.add_context_variable(
                            ctx.scope_id,
                            parsed_let.name,
                            variable_id,
                            variable_type,
                        );
                        if !added {
                            kbail!(
                                self,
                                parsed_let.span,
                                "A context variable of type {} already exists in this scope",
                                variable_type
                            );
                        }
                        for ability_expr in self.ast.mem.getn(parsed_let.context_abilities) {
                            self.add_ability_context_variable(
                                *ability_expr,
                                variable_id,
                                variable_type,
                                ctx.scope_id,
                            )?;
                        }
                    } else {
                        self.scopes.add_variable(ctx.scope_id, parsed_let.name, variable_id);
                    }
                    self.emit_ls_entity(parsed_let.span, LsEntityKind::Variable { variable_id });
                    self.stmts.add_expected_id(val_def_stmt, stmt_id);
                    Ok(Some(stmt_id))
                }
            }
            ParsedStmt::Require(require) => {
                static_assert_size!(parse::ParsedRequire, 12);
                let require = *require;
                let has_else = require.else_body.is_some();
                let skip_message = has_else;
                let mut nonexhaustive_msg = None;

                // No need to build an expensive error message that we won't show
                let condition = match self.eval_matching_condition(
                    require.condition_expr,
                    Some((skip_message, &mut nonexhaustive_msg)),
                    ctx.with_no_expected_type(),
                )? {
                    MatchingConditionResult::NeverBlock(never_block) => {
                        let stmt = self.add_expr_stmt(never_block);
                        return Ok(Some(stmt));
                    }
                    MatchingConditionResult::MatchingCondition(condition) => condition,
                };

                let match_was_exhaustive = nonexhaustive_msg.is_none();
                if match_was_exhaustive && has_else {
                    self.report_warn(
                        require.span,
                        "This pattern always matches; remove the 'else' clause",
                    );
                }
                if !match_was_exhaustive && !has_else {
                    kbail!(
                        self,
                        require.span,
                        "This pattern can fail to match; make it infallible or add an 'else' clause: {}",
                        nonexhaustive_msg.unwrap().message
                    );
                }

                let else_scope = self.scopes.add_child_scope(
                    ctx.scope_id,
                    ScopeType::LexicalBlock,
                    ScopeOwnerId::None,
                );

                // Make the binding variables unavailable in the else scope
                for instr in self.mem.getn(condition.instrs) {
                    if let MatchingConditionInstr::Binding { let_stmt } = instr {
                        let stmt = self.stmts.get(*let_stmt).as_let().unwrap();
                        let variable = self.variables.get(stmt.variable_id);

                        if !variable.is_user_hidden() {
                            self.scopes.mask_variable(else_scope, variable.name);
                        }
                    }
                }

                let else_body = if let Some(require_else_body) = require.else_body {
                    let else_body =
                        self.eval_expr(require_else_body, ctx.with_scope(else_scope))?;
                    if self.exprs.get_type(else_body) != NEVER_TYPE_ID {
                        let else_span = self.exprs.get_span(else_body);
                        kbail!(
                            self,
                            else_span,
                            "else branch must diverge; try returning or exiting"
                        );
                    }
                    Some(else_body)
                } else {
                    None
                };

                let id = self.stmts.add(TypedStmt::Require(TypedRequireStmt {
                    condition,
                    else_body,
                    span: require.span,
                }));
                Ok(Some(id))
            }
            ParsedStmt::Assign(assign) => {
                static_assert_size!(parse::AssignStmt, 12);
                let assignment = *assign;
                let lhs_span = self.ast.exprs.get_span(assignment.lhs);
                // A bare variable name rebinds a local (or writes a mutable
                // global); any other lhs must denote a place, and we store to
                // its address. Storing through a reference requires an
                // explicit deref: `r.* = v`.
                let (destination, expected_rhs_type, reassigned_variable_id) =
                    if let ParsedExpr::Variable(_) = self.ast.exprs.get(assignment.lhs) {
                        let (typed_variable_id, lhs) =
                            self.eval_variable(assignment.lhs, ctx, true)?;
                        let Some(variable_id) = typed_variable_id else {
                            kbail!(self, lhs_span, "Must be a regular variable, eg not a function");
                        };
                        let lhs_type = self.exprs.get_type(lhs);
                        match self.variables.get(variable_id).kind {
                            VariableKind::FnParam(_) => {
                                kbail!(
                                    self,
                                    lhs_span,
                                    "Cannot re-assign a function parameter; declare a local, or store through a reference with `param.* = ...`"
                                );
                            }
                            VariableKind::StackSynthetic(_) => {
                                kbail!(
                                    self,
                                    lhs_span,
                                    "Cannot re-assign a synthetic variable or binding; if this is a pattern-bound reference, store through it with `x.* = ...`"
                                );
                            }
                            VariableKind::Stack(_) => (lhs, lhs_type, Some(variable_id)),
                            VariableKind::Global(global_id) => {
                                if self.globals.get(global_id).is_constant {
                                    kbail!(
                                        self,
                                        lhs_span,
                                        "Cannot assign an immutable global; declare it with `let mutable`"
                                    );
                                }
                                let dest = self.synth_address_of(lhs, lhs_span, false)?;
                                (dest, lhs_type, None)
                            }
                        }
                    } else {
                        let lhs = self.eval_expr(assignment.lhs, ctx.with_no_expected_type())?;
                        let lhs_type = self.exprs.get_type(lhs);
                        let dest =
                            self.synth_address_of(lhs, lhs_span, false).map_err(|mut e| {
                                e.message = self.ast.idents.intern(format!(
                                    "Assignment destination must be a place: {}",
                                    self.ident_str(e.message)
                                ));
                                e
                            })?;
                        if let TypedExpr::AddressOf(addr_of) = self.exprs.get(dest) {
                            if let AddressOfKind::GlobalVariable(variable_id) = addr_of.kind {
                                let global_id =
                                    self.variables.get(variable_id).global_id().unwrap();
                                if self.globals.get(global_id).is_constant {
                                    kbail!(
                                        self,
                                        lhs_span,
                                        "Cannot assign an immutable global; declare it with `let mutable`"
                                    );
                                }
                            }
                        }
                        (dest, lhs_type, None)
                    };
                let rhs = self
                    .eval_expr_with_coercion(
                        assignment.rhs,
                        ctx.with_expected_type(Some(expected_rhs_type)),
                        true,
                    )
                    .map_err(|mut e| {
                        e.message = self.ast.idents.intern(format!(
                            "Invalid type for assignment: {}",
                            self.ident_str(e.message)
                        ));
                        e
                    })?;

                if self.exprs.get_type(rhs) == NEVER_TYPE_ID {
                    return Ok(Some(self.add_expr_stmt(rhs)));
                }
                let kind = match reassigned_variable_id {
                    Some(variable_id) => {
                        self.variables
                            .get_mut(variable_id)
                            .flags
                            .set(VariableFlags::Reassigned, true);
                        AssignmentKind::Set
                    }
                    None => AssignmentKind::Store,
                };
                let stmt_id = self.stmts.add(TypedStmt::Assignment(AssignmentStmt {
                    destination,
                    value: rhs,
                    span: assignment.span,
                    kind,
                }));
                Ok(Some(stmt_id))
            }
            ParsedStmt::Defer(defer) => {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    kbail!(self, defer.span, "defer cannot be used inside `defer` blocks");
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

    /// HEY KOLEMAN This block's scope is ALREADY PROVIDED AND SET IN CTX
    fn eval_block(
        &mut self,
        block: &ParsedBlock,
        // HEY KOLEMAN This block's scope is ALREADY PROVIDED AND SET IN CTX
        ctx: EvalExprContext,
        needs_terminator: bool,
    ) -> K1Result<TypedExprId> {
        let block_scope = ctx.scope_id;
        let unit_body;
        let block = if block.stmts.is_empty() {
            let unit_expr = self.ast.exprs.add(ParsedExpr::Struct(parse::ParsedStruct {
                fields: MSlice::empty(),
                span: block.span,
            }));
            let stmt_id = self.ast.stmts.add(ParsedStmt::LoneExpression(unit_expr));
            let stmts = self.ast.mem.pushn(&[stmt_id]);
            unit_body = ParsedBlock { stmts, kind: block.kind, span: block.span };
            &unit_body
        } else {
            block
        };
        let mut stmts = self.mem.new_list(block.stmts.len());
        let mut last_expr_type: TypeId = self.builtin_types.empty;
        let mut last_stmt_is_divergent = false;
        for (index, stmt) in self.ast.mem.getn(block.stmts).iter().enumerate() {
            if last_stmt_is_divergent {
                kbail!(
                    self,
                    self.ast.get_stmt_span(*stmt),
                    "Dead code following divergent statement",
                );
            }
            let is_last = index + 1 == block.stmts.len() as usize;
            let expected_type = if is_last { ctx.expected_type_id } else { None };

            let coerce = expected_type.is_some();
            debug!("eval_stmt {index} with type {}", self.type_id_to_string_opt(expected_type));
            let tmp_mark = self.tmp.mark();
            let stmt_result =
                self.eval_stmt(*stmt, ctx.with_expected_type(expected_type), coerce, index);
            self.tmp.reset_to(tmp_mark);
            let Some(stmt_id) = stmt_result? else {
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

            let stmt_span = self.get_stmt_span(stmt_id);
            last_expr_type = self.get_stmt_type(stmt_id);
            last_stmt_is_divergent = last_expr_type == NEVER_TYPE_ID;

            if !is_last {
                if !last_stmt_is_divergent {
                    let expr_type = match self.stmts.get(stmt_id) {
                        TypedStmt::Expr(_, expr_type) => Some(*expr_type),
                        _ => None,
                    };
                    if let Some(expr_type) = expr_type {
                        let implements_try = self
                            .find_or_generate_ability_impl_for_type(
                                expr_type,
                                ABILITY_ID_TRY,
                                &[],
                                true,
                                block_scope,
                                stmt_span,
                            )
                            .is_ok();
                        if implements_try {
                            self.report(kwarn!(
                                self,
                                stmt_span,
                                "Discarded expression result of type {}; handle it or discard with `let _`",
                                expr_type
                            ));
                        }
                    }
                }
                stmts.push(stmt_id);
                continue;
            }

            // Falling off the end of the block exits its scope, so the block's pending defers
            // run now after the final value is computed, which we ensure by binding a
            // non-trivial final expression and yielding the binding. We typecheck deferred
            // exprs NOW, as if the deferred code were textually pasted here. A divergent final
            // statement instead exits through return/break, which emit defers for every scope
            // they leave themselves.
            let deferred_exprs = if last_stmt_is_divergent {
                smallvec![]
            } else {
                self.scopes
                    .block_defers
                    .get(&block_scope)
                    .map(|d| d.deferred_exprs.clone())
                    .unwrap_or_default()
            };

            let final_value = match self.stmts.get(stmt_id) {
                TypedStmt::Expr(expr, _) if !last_stmt_is_divergent => Some(*expr),
                _ => None,
            };
            let yielded = match final_value {
                None => {
                    stmts.push(stmt_id);
                    None
                }
                Some(final_expr) => {
                    if deferred_exprs.is_empty() {
                        if needs_terminator {
                            Some(final_expr)
                        } else {
                            stmts.push(stmt_id);
                            None
                        }
                    } else if matches!(self.exprs.get(final_expr), TypedExpr::Variable(_)) {
                        Some(final_expr)
                    } else {
                        let bound = self.synth_variable_defn_simple(
                            self.ast.idents.b.defer_value,
                            final_expr,
                            block_scope,
                        );
                        stmts.push_grow(&mut self.mem, bound.defn_stmt);
                        Some(bound.variable_expr)
                    }
                }
            };
            for deferred_parsed_expr in deferred_exprs.iter().rev() {
                let deferred_code = self.eval_expr(
                    *deferred_parsed_expr,
                    ctx.with_no_expected_type().with_is_defer(true),
                )?;
                let defer_type = self.exprs.get_type(deferred_code);
                let defer_stmt_id = self.stmts.add(TypedStmt::Expr(deferred_code, defer_type));
                stmts.push_grow(&mut self.mem, defer_stmt_id);
            }
            // Ensure termination because this is a 'real' control flow block
            // Not just a lexical block the user made. For example, a function body.
            match yielded {
                Some(yielded_expr) if needs_terminator => {
                    let returned_variable =
                        self.check_returned_value_expr(yielded_expr, block_scope)?;
                    let expr_span = self.exprs.get_span(yielded_expr);
                    let return_expr =
                        self.exprs.add_return(yielded_expr, returned_variable, expr_span);
                    let return_stmt = self.stmts.add(TypedStmt::Expr(return_expr, NEVER_TYPE_ID));
                    stmts.push_grow(&mut self.mem, return_stmt);
                }
                Some(yielded_expr) => {
                    let yield_stmt = self.stmts.add(TypedStmt::Expr(yielded_expr, last_expr_type));
                    stmts.push_grow(&mut self.mem, yield_stmt);
                }
                None if needs_terminator && !last_stmt_is_divergent => {
                    let empty = self.synth_empty_value(stmt_span);
                    let return_empty_expr = self.exprs.add_return(empty, None, stmt_span);
                    let return_empty =
                        self.stmts.add(TypedStmt::Expr(return_empty_expr, NEVER_TYPE_ID));
                    stmts.push_grow(&mut self.mem, return_empty);
                }
                None => {}
            }
        }

        for stmt in stmts.iter() {
            if let TypedStmt::Let(let_stmt) = self.stmts.get(*stmt) {
                self.warn_variable_usage_counts(
                    "Local variable",
                    let_stmt.variable_id,
                    let_stmt.span,
                );
            }
        }

        let id = self.exprs.add_block(
            BlockBuilder { scope_id: block_scope, statements: stmts, span: block.span },
            last_expr_type,
        );
        //eprintln!("  finished block with type:\n{}", self.expr_to_string_with_type(id));
        //eprintln!("  last_expr_type was: {}", self.type_id_to_string(last_expr_type));
        Ok(id)
    }

    // Handle 'returned variable' case
    fn check_returned_value_expr(
        &mut self,
        expr: TypedExprId,
        scope_id: ScopeId,
    ) -> K1Result<Option<VariableId>> {
        let expr_span = self.exprs.get_span(expr);
        if let Some(returned_variable_id) = self.get_returned_var_for_scope(scope_id) {
            let err = Err(kerr!(
                self,
                expr_span,
                "Must return declared returned variable {}",
                self.variables.get(returned_variable_id).name,
            ));
            let actual_variable_id = match self.exprs.get(expr) {
                TypedExpr::Variable(v) => v.variable_id,
                _ => {
                    return err;
                }
            };
            if actual_variable_id != returned_variable_id {
                return err;
            }
            Ok(Some(returned_variable_id))
        } else {
            Ok(None)
        }
    }

    fn resolve_intrinsic_function_type(
        &self,
        fn_name: StringId,
        namespace_chain: Dlist<StringId, MemTmp>,
        ability_id: Option<AbilityId>,
        ability_impl_self_type: Option<TypeId>,
    ) -> Result<Builtin, String> {
        let fn_name_str = self.ast.idents.get_string(fn_name);
        let second =
            self.tmp.dlist_nth_data_opt(namespace_chain, 2).map(|node| self.ident_str(*node));
        let third =
            self.tmp.dlist_nth_data_opt(namespace_chain, 3).map(|node| self.ident_str(*node));
        let result = if let Some(ability_id) = ability_id {
            let base_ability_id = self.abilities.get(ability_id).base_ability_id;
            use ArithOpKind as OpKind;
            use ArithOpOp as Op;
            macro_rules! mk_arith {
                ($e: expr) => {
                    Some(Builtin::Ir(BuiltinIr::ArithBinop($e)))
                };
            }
            macro_rules! mk_bitwise {
                ($e: expr) => {
                    Some(Builtin::Ir(BuiltinIr::BitwiseBinop($e)))
                };
            }
            let t = ability_impl_self_type.map(|t| self.types.get(t));
            let is_integer = t.is_some_and(|t| t.as_integer().is_some());
            match (base_ability_id, fn_name_str) {
                (ABILITY_ID_ENUM, "enum-value") => {
                    Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::EnumAbilityGetValue))
                }
                (ABILITY_ID_ENUM, "enum-name") => Some(Builtin::TyperPhysicalFunction(
                    BuiltinTyperFunction::EnumAbilityGetTagName,
                )),
                (ABILITY_ID_SUM, "sum-tag") => {
                    Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::SumAbilityGetTag))
                }
                (ABILITY_ID_SUM, "sum-name") => {
                    Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::SumAbilityGetName))
                }
                (ABILITY_ID_EQUALS, "equals") => match t {
                    Some(Type::Char) | Some(Type::Bool) | Some(Type::Pointer) => {
                        mk_arith!(OpKind::uint(Op::Equals))
                    }
                    Some(Type::Integer(i)) => {
                        let o = if i.is_signed() {
                            OpKind::sint(Op::Equals)
                        } else {
                            OpKind::uint(Op::Equals)
                        };
                        mk_arith!(o)
                    }
                    Some(Type::Float(_)) => mk_arith!(OpKind::float(Op::Equals)),
                    Some(Type::Enum(_)) => {
                        Some(Builtin::TyperInline(BuiltinTyperInline::EnumEquals))
                    }
                    Some(Type::Sum(_)) => {
                        Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::SumEquals))
                    }
                    Some(Type::Struct(_)) => {
                        Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::StructEquals))
                    }
                    _ => None,
                },
                (ABILITY_ID_PRINT, "print-to") => match t {
                    Some(Type::Struct(_)) => {
                        Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::StructPrintTo))
                    }
                    Some(Type::Sum(_)) => {
                        Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::SumPrintTo))
                    }
                    Some(Type::Enum(_)) => {
                        Some(Builtin::TyperPhysicalFunction(BuiltinTyperFunction::EnumPrintTo))
                    }
                    _ => None,
                },
                (ABILITY_ID_BITWISE, "bit-not") if is_integer => {
                    Some(Builtin::Ir(BuiltinIr::BitNot))
                }
                (ABILITY_ID_BITWISE, "bit-and") if is_integer => {
                    mk_bitwise!(BitwiseBinopKind::And)
                }
                (ABILITY_ID_BITWISE, "bit-or") if is_integer => {
                    mk_bitwise!(BitwiseBinopKind::Or)
                }
                (ABILITY_ID_BITWISE, "xor") if is_integer => {
                    mk_bitwise!(BitwiseBinopKind::Xor)
                }
                (ABILITY_ID_BITWISE, "shift-left") if is_integer => {
                    mk_bitwise!(BitwiseBinopKind::ShiftLeft)
                }
                (ABILITY_ID_BITWISE, "shift-right") if is_integer => {
                    let int_type = t.unwrap().expect_integer();
                    if int_type.is_signed() {
                        mk_bitwise!(BitwiseBinopKind::SignedShiftRight)
                    } else {
                        mk_bitwise!(BitwiseBinopKind::UnsignedShiftRight)
                    }
                }
                (ABILITY_ID_BITWISE, bitwise_fn) if matches!(t, Some(Type::Vector(_))) => {
                    let op = match bitwise_fn {
                        "bit-not" => Some(VecOpKind::BitNot),
                        "bit-and" => Some(VecOpKind::BitAnd),
                        "bit-or" => Some(VecOpKind::BitOr),
                        "xor" => Some(VecOpKind::Xor),
                        "shift-left" => Some(VecOpKind::ShiftLeft),
                        "shift-right" => Some(VecOpKind::ShiftRight),
                        _ => None,
                    };
                    op.map(|op| Builtin::Ir(BuiltinIr::VectorOp(op)))
                }
                (ABILITY_ID_ADD, "add") => match t {
                    Some(Type::Integer(i)) => {
                        // Even though signedness is irrelevant here, we still set it properly
                        // just in case it ever is, (for example if we want to make signed wrap UB
                        // instead of wrapping)
                        if i.is_signed() {
                            mk_arith!(ArithOpKind::sint(Op::Add))
                        } else {
                            mk_arith!(ArithOpKind::uint(Op::Add))
                        }
                    }
                    Some(Type::Float(_)) => {
                        mk_arith!(ArithOpKind::float(Op::Add))
                    }
                    Some(Type::Vector(_)) => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Add))),
                    _ => None,
                },
                (ABILITY_ID_SUB, "sub") => match t {
                    Some(Type::Integer(i)) => {
                        if i.is_signed() {
                            mk_arith!(ArithOpKind::sint(Op::Sub))
                        } else {
                            mk_arith!(ArithOpKind::uint(Op::Sub))
                        }
                    }
                    Some(Type::Float(_)) => {
                        mk_arith!(ArithOpKind::float(Op::Sub))
                    }
                    Some(Type::Vector(_)) => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Sub))),
                    _ => None,
                },
                (ABILITY_ID_MUL, "mul") => match t {
                    Some(Type::Integer(i)) => {
                        if i.is_signed() {
                            mk_arith!(ArithOpKind::sint(Op::Mul))
                        } else {
                            mk_arith!(ArithOpKind::uint(Op::Mul))
                        }
                    }
                    Some(Type::Float(_)) => {
                        mk_arith!(ArithOpKind::float(Op::Mul))
                    }
                    Some(Type::Vector(_)) => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Mul))),
                    _ => None,
                },
                (ABILITY_ID_DIV, "div") => match t {
                    Some(Type::Integer(i)) => {
                        if i.is_signed() {
                            mk_arith!(ArithOpKind::sint(Op::Div))
                        } else {
                            mk_arith!(ArithOpKind::uint(Op::Div))
                        }
                    }
                    Some(Type::Float(_)) => {
                        mk_arith!(ArithOpKind::float(Op::Div))
                    }
                    _ => None,
                },
                (ABILITY_ID_NEG, "negated") => match t {
                    Some(Type::Bool) | Some(Type::Integer(_)) | Some(Type::Float(_)) => {
                        Some(Builtin::Ir(BuiltinIr::Negate))
                    }
                    _ => None,
                },
                (ABILITY_ID_REM, "rem") => match t {
                    Some(Type::Integer(i)) => {
                        if i.is_signed() {
                            mk_arith!(ArithOpKind::sint(Op::Rem))
                        } else {
                            mk_arith!(ArithOpKind::uint(Op::Rem))
                        }
                    }
                    Some(Type::Float(_)) => {
                        mk_arith!(ArithOpKind::float(Op::Rem))
                    }
                    _ => None,
                },
                (ABILITY_ID_SCALAR_CMP, _) => {
                    let class = if let Some(Type::Integer(i)) = t {
                        Some(ArithOpClass::from_int_type(*i))
                    } else if let Some(Type::Float(_)) = t {
                        Some(ArithOpClass::Float)
                    } else {
                        None
                    };
                    match class {
                        None => None,
                        Some(class) => match fn_name_str {
                            "lt" => mk_arith!(OpKind { class, op: Op::Lt }),
                            "le" => mk_arith!(OpKind { class, op: Op::Le }),
                            "gt" => mk_arith!(OpKind { class, op: Op::Gt }),
                            "ge" => mk_arith!(OpKind { class, op: Op::Ge }),
                            _ => None,
                        },
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
                Some("mem") => match fn_name_str {
                    "copy" => Some(Builtin::Backend(BackendBuiltin::MemCopy)),
                    "move" => Some(Builtin::Backend(BackendBuiltin::MemMove)),
                    "set" => Some(Builtin::Backend(BackendBuiltin::MemSet)),
                    "equals" => Some(Builtin::Backend(BackendBuiltin::MemEquals)),
                    "zeroed" => Some(Builtin::Ir(BuiltinIr::Zeroed)),
                    "bitcast" => Some(Builtin::Ir(BuiltinIr::Bitcast)),
                    "load-volatile" => Some(Builtin::Ir(BuiltinIr::VolatileLoad)),
                    "store-volatile" => Some(Builtin::Ir(BuiltinIr::VolatileStore)),
                    _ => None,
                },
                Some("types") => match (third, fn_name_str) {
                    (None, "id") => Some(Builtin::TyperInline(BuiltinTyperInline::TypeId)),
                    (None, "size") => Some(Builtin::TyperInline(BuiltinTyperInline::TypeSize)),
                    (None, "stride") => Some(Builtin::TyperInline(BuiltinTyperInline::TypeStride)),
                    (None, "align") => Some(Builtin::TyperInline(BuiltinTyperInline::TypeAlign)),
                    (Some("type-id"), "info") => Some(Builtin::Backend(BackendBuiltin::TypeInfo)),
                    (None, "make-struct") => Some(Builtin::Backend(BackendBuiltin::MakeStruct)),
                    (None, "make-either") => Some(Builtin::Backend(BackendBuiltin::MakeEither)),
                    (None, "make-reference") => {
                        Some(Builtin::Backend(BackendBuiltin::MakeReference))
                    }
                    (None, "make-array") => Some(Builtin::Backend(BackendBuiltin::MakeArray)),
                    (None, "make-fn") => Some(Builtin::Backend(BackendBuiltin::MakeFn)),
                    (None, "make-instance") => Some(Builtin::Backend(BackendBuiltin::MakeInstance)),
                    _ => None,
                },
                Some("string") => None,
                Some("list") => None,
                Some("char") => None,
                Some("ptr") => match fn_name_str {
                    "ref-at" => Some(Builtin::Ir(BuiltinIr::PointerIndex)),
                    _ => None,
                },
                Some("vector") => match fn_name_str {
                    "splat" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Splat))),
                    "load-unchecked" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Load))),
                    "store-unchecked" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::Store))),
                    "get-lane" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::GetLane))),
                    "with-lane" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::WithLane))),
                    "eq-lanes" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::EqLanes))),
                    "to-mask" => Some(Builtin::Ir(BuiltinIr::VectorOp(VecOpKind::ToMask))),
                    _ => None,
                },
                Some("atomic") => match fn_name_str {
                    "load" => Some(Builtin::Ir(BuiltinIr::AtomicLoad)),
                    "store" => Some(Builtin::Ir(BuiltinIr::AtomicStore)),
                    "xchg" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Xchg))),
                    "fetch-add" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Add))),
                    "fetch-sub" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Sub))),
                    "fetch-and" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::And))),
                    "fetch-or" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Or))),
                    "fetch-xor" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Xor))),
                    "fetch-min" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Min))),
                    "fetch-max" => Some(Builtin::Ir(BuiltinIr::AtomicRmw(AtomicRmwOp::Max))),
                    "cmpxchg" => Some(Builtin::Ir(BuiltinIr::AtomicCmpxchg { weak: false })),
                    "cmpxchg-weak" => Some(Builtin::Ir(BuiltinIr::AtomicCmpxchg { weak: true })),
                    "fence" => Some(Builtin::Ir(BuiltinIr::AtomicFence)),
                    _ => None,
                },
                Some("meta") => match fn_name_str {
                    "bake-static-value" => Some(Builtin::Ir(BuiltinIr::BakeStaticValue)),
                    "get-static-value" => {
                        Some(Builtin::TyperInline(BuiltinTyperInline::GetStaticValue))
                    }
                    "static-type-to-value" => {
                        Some(Builtin::TyperInline(BuiltinTyperInline::StaticTypeToValue))
                    }
                    _ => None,
                },
                Some("k1") => match fn_name_str {
                    "exit" => Some(Builtin::Backend(BackendBuiltin::Exit)),
                    "emit-compiler-message" => {
                        Some(Builtin::Backend(BackendBuiltin::CompilerMessage))
                    }
                    "location" => {
                        Some(Builtin::TyperInline(BuiltinTyperInline::CompilerSourceLocation))
                    }
                    // k1/repl
                    "checkbox" => Some(Builtin::Backend(BackendBuiltin::ReplCheckbox)),
                    _ => None,
                },
                Some(_) => None,
            }
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Could not resolve intrinsic function type for function {}/{} (ability: {})",
                self.tmp
                    .dlist_iter(namespace_chain)
                    .map(|i| self.ident_str(*i).to_string())
                    .collect::<Vec<_>>()
                    .join("/"),
                fn_name_str,
                ability_id.is_some()
            )),
        }
    }

    fn eval_sum_constructor(
        &mut self,
        concrete_sum_type: TypeId,
        variant_name: StringId,
        payload: Option<ParsedExprId>,
        ctx: EvalExprContext,
        variant_span: SpanId,
    ) -> K1Result<TypedExprId> {
        let sum = self.types.get(concrete_sum_type).expect_sum();
        let Some(variant) = self.sum_variant_by_name(sum.variants, variant_name) else {
            kbail!(
                self,
                variant_span,
                "No variant '{}' exists in sum '{}'",
                self.ident_str(variant_name).blue(),
                concrete_sum_type
            );
        };
        let variant_index = variant.index;
        self.emit_ls_entity(
            variant_span,
            LsEntityKind::Variant { type_id: concrete_sum_type, variant_index },
        );
        let payload = match variant.payload {
            None => {
                if let Some(_payload_arg) = payload {
                    Err(kerr!(
                        self,
                        variant_span,
                        "Variant '{}' does not have data",
                        self.ident_str(variant_name).blue()
                    ))
                } else {
                    Ok(None)
                }
            }
            Some(payload_type) => {
                if let Some(payload_arg) = payload {
                    let payload_value = self.eval_expr_with_coercion(
                        payload_arg,
                        ctx.with_expected_type(Some(payload_type)),
                        true,
                    )?;
                    Ok(Some(payload_value))
                } else if payload_type == EMPTY_TYPE_ID {
                    Ok(Some(self.synth_empty_value(variant_span)))
                } else {
                    Err(kerr!(
                        self,
                        variant_span,
                        ":{} requires data of type {}",
                        self.ident_str(variant_name).blue(),
                        payload_type
                    ))
                }
            }
        }?;
        let never_payload = payload.is_some_and(|p| self.exprs.get_type(p) == NEVER_TYPE_ID);
        if never_payload {
            // Might as well just codegen the payload expr that wants to exit;
            // now all downstream code doesn't have to worry about the 'crash payload' scenario
            let never_payload_expr = payload.unwrap();
            return Ok(never_payload_expr);
        }

        let sum_constructor = self.exprs.add(
            TypedExpr::SumConstructor(TypedSumConstructor { variant_index, payload }),
            concrete_sum_type,
            variant_span,
        );
        Ok(sum_constructor)
    }

    fn check_ability_fn_where_constraints(
        &mut self,
        specialized_ability_id: AbilityId,
        impl_arguments: TypeIdSlice,
        impl_self_type: TypeId,
        fn_index: u32,
        scope_id: ScopeId,
        span: SpanId,
    ) -> K1Result<()> {
        let decl_fn = *self
            .mem
            .get_nth(self.abilities.get(specialized_ability_id).functions, fn_index as usize);
        let constraints = self.get_function(decl_fn.function_id).ability_where_constraints;
        if constraints.is_empty() {
            return Ok(());
        }

        let ability = *self.abilities.get(specialized_ability_id);
        let mut pairs: SV8<TypeSubstitutionPair> =
            smallvec![spair! {ability.self_type_id => impl_self_type}];
        let ability_args = ability.kind.arguments(self);
        let base_params = self.abilities.get(ability.base_ability_id).parameters;
        for (base_param, ability_arg) in self
            .mem
            .getn(base_params)
            .iter()
            .filter(|p| p.is_ability_side_param())
            .zip(ability_args.iter())
        {
            pairs.push(spair! {base_param.type_variable_id => *ability_arg});
        }
        for (base_param, impl_arg) in self
            .mem
            .getn(base_params)
            .iter()
            .filter(|p| p.is_impl_param)
            .zip(self.mem.getn(impl_arguments).iter())
        {
            pairs.push(spair! {base_param.type_variable_id => *impl_arg});
        }

        for c in self.mem.getn(constraints) {
            let target = self.substitute_in_type(c.target, &pairs);
            let signature =
                self.substitute_in_ability_signature(&pairs, c.signature, scope_id, span);
            self.check_ability_constraint(
                target,
                signature,
                decl_fn.function_name,
                scope_id,
                c.span,
            )?;
        }
        Ok(())
    }

    fn check_ability_constraint(
        &mut self,
        target_type: TypeId,
        signature: TypedAbilitySignature,
        name: StringId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> K1Result<()> {
        debug!(
            "Checking constraint {}: {}",
            self.type_id_to_string(target_type,),
            self.ability_impl_signature_to_string(
                signature.specialized_ability_id,
                signature.impl_arguments
            )
        );
        // We just can't allow ref self here since we don't have a good way to tell the function
        // call that it will work only if we coerce this argument, we only have types at this point.
        if let Ok((impl_handle, _)) = self.find_or_generate_specialized_ability_impl_for_type(
            target_type,
            signature.specialized_ability_id,
            false,
            scope_id,
            span,
        ) {
            let found_impl = self.ability_impls.get(impl_handle.full_impl_id);
            debug_assert!(signature.impl_arguments.len() == found_impl.impl_arguments.len());
            for (index, (constraint_arg, passed_arg)) in self
                .mem
                .getn(signature.impl_arguments)
                .iter()
                .zip(self.mem.getn(found_impl.impl_arguments).iter())
                .enumerate()
            {
                if self.get_type_id_resolved(*constraint_arg, scope_id)
                    != self.get_type_id_resolved(*passed_arg, scope_id)
                {
                    let base_params = self.abilities.get(found_impl.base_ability_id).parameters;
                    let param_name = self
                        .mem
                        .getn(base_params)
                        .iter()
                        .filter(|p| p.is_impl_param)
                        .nth(index)
                        .map(|p| self.ident_str(p.name))
                        .unwrap_or("?");
                    kbail!(
                        self,
                        span,
                        "Provided type {} = {} does implement required ability {}, but the implementation parameter {} is wrong: Expected type was {} but the actual implementation uses {}",
                        name,
                        target_type,
                        self.abilities.get(signature.specialized_ability_id).name,
                        param_name,
                        *constraint_arg,
                        *passed_arg,
                    );
                }
            }
            Ok(())
        } else {
            Err(kerr!(
                self,
                span,
                "Provided type for {} is {} which does not implement required ability {}",
                name,
                target_type,
                self.abilities.get(signature.specialized_ability_id).name
            ))
        }
    }

    fn check_type_constraints(
        &mut self,
        param_name: StringId,
        param_type: TypeId,
        passed_type: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        scope_id: ScopeId,
        span: SpanId,
    ) -> K1Result<()> {
        let tp = *self.get_type_parameter(param_type);
        if let Some(static_constraint) = tp.static_constraint {
            let specialized_constraint =
                self.substitute_in_type(static_constraint, substitution_pairs);
            if let Err(msg) = self.check_types(specialized_constraint, passed_type, scope_id) {
                kbail!(
                    self,
                    span,
                    "Provided type for {} didn't satisfy the static constraint: {msg}",
                    param_name,
                );
            }
        }
        for predicate_constraint_fn_qident in self.mem.getn(tp.predicate_functions) {
            let Some(function_id) =
                self.find_function_namespaced(tp.scope_id, predicate_constraint_fn_qident)?
            else {
                kbail!(self, predicate_constraint_fn_qident.name_span, "Function not found");
            };
            let predicate_result: bool =
                self.execute_type_predicate_function(function_id, passed_type, span)?;
            if !predicate_result {
                kbail!(
                    self,
                    span,
                    "Predicate '{}' failed on type {}",
                    predicate_constraint_fn_qident,
                    passed_type
                )
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
            self.check_ability_constraint(passed_type, signature, param_name, scope_id, span)?;
        }
        Ok(())
    }

    fn execute_type_predicate_function(
        &mut self,
        function_id: FunctionId,
        type_id: TypeId,
        span: SpanId,
    ) -> K1Result<bool> {
        self.require_function_body(function_id, span)?;
        let generic_function = self.functions.get(function_id);
        if generic_function.body_block.is_none() {
            kbail!(self, span, "Predicate function '{}' has no body", generic_function.name);
        }
        if generic_function.type_params.len() != 1 {
            kbail!(
                self,
                span,
                "Must have 1 type parameter to be used as a type predicate function"
            );
        }
        if !generic_function.params.is_empty() {
            kbail!(self, span, "Must have 0 parameters to be used as a type predicate function");
        }
        if !generic_function.fnlike_type_params.is_empty() {
            kbail!(
                self,
                span,
                "Must have no fnlike type params to be used as a type predicate function"
            );
        }
        let function_type = self.types.get(generic_function.type_id).as_function().unwrap();
        if function_type.return_type != BOOL_TYPE_ID {
            kbail!(self, span, "Must return bool to be used as a type predicate function");
        }

        let specialized_function = self.specialize_function_declaration(
            TypeArgs::one(type_id),
            TypeArgs::empty(),
            function_id,
        );
        self.specialize_function_body(specialized_function)?;
        let predicate_result_static_value_id =
            self.execute_static_function(specialized_function, &[], span)?;
        let StaticValue::Bool(predicate_result) =
            *self.static_values.get(predicate_result_static_value_id)
        else {
            ice_span!(self, span, "Expected a bool")
        };
        Ok(predicate_result)
    }

    /// Checks a list of arguments against an ability's signature.
    /// Are all arguments provided? If they have constraints, are they met?
    /// skip_impl_check: Sometimes we only apply the ability-side params
    fn check_ability_arguments(
        &mut self,
        ability_id: AbilityId,
        arguments: &[NameAndType],
        span: SpanId,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> K1Result<(TypeIdSlice, TypeIdSlice)> {
        let ability = self.abilities.get(ability_id);
        let ability_parameters = ability.parameters;

        // Catch unrecognized arguments first
        for arg in arguments {
            let has_matching_param =
                self.mem.getn(ability_parameters).iter().any(|param| param.name == arg.name);
            if !has_matching_param {
                kbail!(self, span, "No parameter named {}", arg.name);
            }
        }

        let mut ability_arguments: List<TypeId, _> = self.mem.new_list(arguments.len() as u32);
        let mut impl_arguments: List<TypeId, _> = self.mem.new_list(arguments.len() as u32);
        let mut subst_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        for param in self
            .mem
            .getn(ability_parameters)
            .iter()
            .filter(|p| !skip_impl_check || p.is_ability_side_param())
        {
            let Some(matching_arg) = arguments.iter().find(|a| a.name == param.name).copied()
            else {
                kbail!(self, span, "Missing argument for ability parameter {}", param.name);
            };
            if param.is_impl_param {
                impl_arguments.push(matching_arg.type_id)
            } else {
                ability_arguments.push(matching_arg.type_id)
            };
            subst_pairs.push(spair! { param.type_variable_id => matching_arg.type_id });
        }

        for (param, pair) in self
            .mem
            .getn(ability_parameters)
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

        let ability_arguments_handle = ability_arguments.to_slice();
        let impl_arguments_handle = impl_arguments.to_slice();
        Ok((ability_arguments_handle, impl_arguments_handle))
    }

    fn specialize_ability(
        &mut self,
        ability_id: AbilityId,
        arguments: TypeSliceId,
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
        let ability_parameters = ability.parameters;
        let ability_namespace_id = ability.namespace_id;
        let specializations = self.abilities.get(generic_ability_id).kind.specializations();
        if self.get_type_slice(arguments).len() > ability_parameters.len() as usize {
            panic!("Passed too many arguments to specialize_ability; probably passed impl args");
        }
        if let Some(cached_specialization) =
            specializations.as_slice(&self.mem).iter().find(|spec| spec.arguments == arguments)
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
            ScopeOwnerId::None,
        );

        for (arg_type, param) in self
            .get_type_slice(arguments)
            .iter()
            .zip(self.mem.getn(ability_parameters).iter().filter(|p| p.is_ability_side_param()))
        {
            let _ = self.scopes.add_type(specialized_ability_scope, param.name, *arg_type);
            let _ = self.scopes.add_type_substitution(
                specialized_ability_scope,
                param.type_variable_id,
                *arg_type,
            );
        }

        // The implementor is responsible for providing the impl_params, so those are the
        // only parameters that the specialized ability should now take
        // ... It also takes 'Self', of course, but we don't treat that as a 'parameter'
        let impl_params_handle = self.mem.pushn_iter(
            self.mem.getn(ability_parameters).iter().filter(|p| p.is_impl_param).copied(),
        );
        for impl_param in self.mem.getn(impl_params_handle) {
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
        let self_ident = self.ast.idents.b.self_;
        let new_self_type_id = self.add_type_parameter(
            TypeParameter {
                name: self_ident,
                static_constraint: None,
                predicate_functions: MSlice::empty(),
                scope_id: specialized_ability_scope,
                span,
            },
            smallvec![],
        );
        let _ = self.scopes.add_type(specialized_ability_scope, self_ident, new_self_type_id);

        let specialized_ability_id = self.abilities.add(TypedAbility {
            name: ability_name,
            base_ability_id: generic_ability_id,
            self_type_id: new_self_type_id,
            parameters: impl_params_handle,
            functions: MSlice::empty(),
            scope_id: specialized_ability_scope,
            ast_id: ability_ast_id,
            namespace_id: ability_namespace_id,
            kind: TypedAbilityKind::Specialized(spec_info),
        });

        let parsed_ability = self.ast.get_ability(ability_ast_id);
        let mut specialized_functions = self.mem.new_list(parsed_ability.functions.len());
        for (index, parsed_fn) in self.ast.mem.getn(parsed_ability.functions).iter().enumerate() {
            let result = self.declare_function(
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

        self.abilities.get_mut(specialized_ability_id).functions = specialized_functions.to_slice();
        {
            let parent_ability = self.abilities.get_mut(generic_ability_id);
            let TypedAbilityKind::Generic { specializations } = &mut parent_ability.kind else {
                panic!("expected generic ability while specializing")
            };
            specializations.push_grow(&mut self.mem, spec_info);
        }

        self.scopes.set_scope_owner_id(
            specialized_ability_scope,
            ScopeOwnerId::Ability(specialized_ability_id),
        );

        specialized_ability_id
    }

    fn check_ability_expr(
        &mut self,
        ability_expr_id: AstHandle<ParsedAbilityExpr>,
        scope_id: ScopeId,
        skip_impl_check: bool,
    ) -> K1Result<(AbilityId, TypeIdSlice, TypeIdSlice)> {
        let ability_expr = self.ast.mem.get(ability_expr_id).clone();
        let ability_id = self.find_ability_or_declare(&ability_expr.name, scope_id)?;

        let mut arguments: SV8<NameAndType> = smallvec![];
        for arg in self.ast.mem.getn(ability_expr.arguments) {
            // TODO: Possible now to pass 'dont cares'. I think we allow them in ability exprs that are constraints but
            //       nowhere else
            let Some(arg_type_expr) = arg.type_expr else {
                kbail!(self, arg.span, "_ is not yet supported as an ability type argument");
            };
            let arg_type = self.eval_type_expr(arg_type_expr, scope_id)?;
            let Some(name) = arg.name else {
                kbail!(self, arg.span, "Ability arguments must all be named, for now");
            };
            arguments.push(NameAndType { name, type_id: arg_type });
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
        ability_expr_id: AstHandle<ParsedAbilityExpr>,
        skip_impl_check: bool,
        scope_id: ScopeId,
    ) -> K1Result<TypedAbilitySignature> {
        let (base_ability_id, ability_arguments, impl_arguments) =
            self.check_ability_expr(ability_expr_id, scope_id, skip_impl_check)?;
        let span = self.ast.mem.get(ability_expr_id).span;
        let ability_arguments = self.intern_type_slice_handle(ability_arguments);
        let new_ability_id =
            self.specialize_ability(base_ability_id, ability_arguments, span, scope_id);
        Ok(TypedAbilitySignature { specialized_ability_id: new_ability_id, impl_arguments })
    }

    /// `let(context(impl <ability>)) x`: check the variable's type implements the ability
    /// and register it under the ability key. The check is strict (no ref-self coercion)
    /// because call sites discharge `[t: <ability>]` constraints strictly; accepting a
    /// by-value binding here would make every consuming call fail, or worse, write into
    /// a copy.
    fn add_ability_context_variable(
        &mut self,
        ability_expr: AstHandle<ParsedAbilityExpr>,
        variable_id: VariableId,
        variable_type: TypeId,
        scope_id: ScopeId,
    ) -> K1Result<()> {
        let signature = self.eval_ability_expr(ability_expr, true, scope_id)?;
        let ability_span = self.ast.mem.get(ability_expr).span;
        let ability_id = signature.specialized_ability_id;
        if let Err(msg) = self.find_or_generate_specialized_ability_impl_for_type(
            variable_type,
            ability_id,
            false,
            scope_id,
            ability_span,
        ) {
            let reference_would = self
                .find_or_generate_specialized_ability_impl_for_type(
                    variable_type,
                    ability_id,
                    true,
                    scope_id,
                    ability_span,
                )
                .is_ok();
            kbail!(
                self,
                ability_span,
                "{} does not implement {}: {}{}",
                variable_type,
                self.abilities.get(ability_id).name,
                msg,
                if reference_would {
                    "\n  A reference to it does; bind one instead: `let context(impl ..) x = value.&`"
                } else {
                    ""
                }
            );
        }
        if !self.scopes.add_context_variable_by_ability(scope_id, ability_id, variable_id) {
            kbail!(
                self,
                ability_span,
                "A context variable for ability {} already exists in this scope",
                self.abilities.get(ability_id).name
            );
        }
        Ok(())
    }

    /// True if every ability-side argument of this (possibly specialized) ability is a
    /// concrete type. Only such abilities can serve as context-variable lookup keys.
    fn ability_arguments_all_concrete(&self, ability_id: AbilityId) -> bool {
        let args = self.abilities.get(ability_id).kind.arguments(self);
        args.iter().all(|arg| !self.type_variable_counts.get(*arg).is_abstract())
    }

    /// Register a function's context param under the ability keys of its type-param
    /// constraints, so the param can satisfy ability-keyed lookups in the body
    /// (transitive context threading). Best-effort: a key collision (two context params
    /// sharing a constraint) poisons the key rather than failing the declaration; only
    /// a lookup that lands on it errors.
    fn register_context_param_ability_keys(
        &mut self,
        scope_id: ScopeId,
        variable_id: VariableId,
        param_type_id: TypeId,
    ) {
        if self.types.get(param_type_id).as_type_parameter().is_none() {
            return;
        }
        for handle in self.get_constrained_ability_impls_for_type(param_type_id) {
            let ability_id = handle.specialized_ability_id;
            if !self.ability_arguments_all_concrete(ability_id) {
                continue;
            }
            if !self.scopes.add_context_variable_by_ability(scope_id, ability_id, variable_id) {
                self.scopes.poison_context_ability_key(scope_id, ability_id);
            }
        }
    }

    /// Implicit context lookup for a param whose (generic) type is a type parameter:
    /// its ability constraints with concrete arguments become lookup keys. Among
    /// distinct candidate variables, the one hit by the most keys wins; a tie is an
    /// ambiguity error, as is landing on a poisoned key.
    fn find_context_variable_by_ability_constraints(
        &self,
        scope_id: ScopeId,
        param_type_id: TypeId,
        span: SpanId,
    ) -> K1Result<Option<VariableId>> {
        if self.types.get(param_type_id).as_type_parameter().is_none() {
            return Ok(None);
        }
        let handles = self.get_constrained_ability_impls_for_type(param_type_id);
        let mut hits: SV4<(AbilityId, VariableId)> = smallvec![];
        for handle in &handles {
            let ability_id = handle.specialized_ability_id;
            if !self.ability_arguments_all_concrete(ability_id) {
                continue;
            }
            match self.scopes.find_context_variable_by_ability(scope_id, ability_id) {
                None => {}
                Some(ContextAbilityEntry::Ambiguous) => {
                    kbail!(
                        self,
                        span,
                        "Multiple context variables in scope provide ability {}; pass the context argument explicitly",
                        self.abilities.get(ability_id).name
                    );
                }
                Some(ContextAbilityEntry::Unique(v)) => hits.push((ability_id, v)),
            }
        }
        let mut candidates: SV4<(VariableId, usize)> = smallvec![];
        for &(_, v) in &hits {
            match candidates.iter_mut().find(|(c, _)| *c == v) {
                Some((_, n)) => *n += 1,
                None => candidates.push((v, 1)),
            }
        }
        if candidates.len() <= 1 {
            return Ok(candidates.first().map(|(v, _)| *v));
        }
        let max_keys_hit = candidates.iter().map(|(_, n)| *n).max().unwrap();
        let mut maximal = candidates.iter().filter(|(_, n)| *n == max_keys_hit);
        let first = maximal.next().unwrap().0;
        if maximal.next().is_none() {
            return Ok(Some(first));
        }
        let names = hits
            .iter()
            .map(|(a, v)| {
                format!(
                    "'{}' (impl {})",
                    self.ident_str(self.variables.get(*v).name),
                    self.ident_str(self.abilities.get(*a).name)
                )
            })
            .unique()
            .join(", ");
        Err(kerr!(
            self,
            span,
            "Ambiguous context: multiple context variables satisfy this parameter's constraints: {}; pass the context argument explicitly",
            names
        ))
    }

    fn dyn_ability_subst_pairs(
        &self,
        signature: TypedAbilitySignature,
    ) -> SV8<TypeSubstitutionPair> {
        let ability = self.abilities.get(signature.specialized_ability_id);
        let mut pairs: SV8<TypeSubstitutionPair> = smallvec![];
        for (param, arg) in self
            .mem
            .getn(ability.parameters)
            .iter()
            .filter(|p| p.is_impl_param)
            .zip(self.mem.getn(signature.impl_arguments))
        {
            pairs.push(spair! { param.type_variable_id => *arg });
        }
        pairs
    }

    /// Computes the slot function type an ability function gets inside a dyn object,
    /// or the reason it is excluded from dynamic dispatch. Two shapes are accepted:
    /// a `self: *mut self` receiver, whose slot replaces self with an opaque state
    /// pointer in lambda-env position; or a function mentioning self nowhere, whose
    /// slot is just the substituted signature (the object acts as a type witness and
    /// no state is passed). Both are physically identical to the impl function, so
    /// its address is directly callable through the slot.
    fn dyn_slot_fn_type(
        &mut self,
        ability_self_type: TypeId,
        subst_pairs: &[TypeSubstitutionPair],
        function_id: FunctionId,
    ) -> Result<TypeId, String> {
        let function = self.get_function(function_id);
        let signature = function.signature();
        let function_type_id = function.type_id;
        let has_own_type_params =
            self.mem.getn(signature.type_params).iter().any(|tp| *tp != ability_self_type);
        if has_own_type_params {
            return Err("it has type parameters; call it on a concrete type instead".to_string());
        }
        let fn_type = self.types.get(function_type_id).as_function().unwrap();
        let physical_params = fn_type.physical_params;
        let return_type = fn_type.return_type;
        let params = self.mem.getn(physical_params);
        if params.iter().any(|p| p.is_context) {
            return Err("it takes context parameters".to_string());
        }
        let self_probe = [spair! { ability_self_type => NEVER_TYPE_ID }];
        let has_receiver = params.first().is_some_and(|p| match self.types.get(p.type_id) {
            Type::Reference(r) => r.inner_type == ability_self_type,
            _ => false,
        });

        let mut slot_params = self.mem.new_list(physical_params.len());
        let value_params = if has_receiver {
            slot_params.push(FnParamType {
                name: self.ast.idents.b.lambda_env_var_name,
                type_id: POINTER_TYPE_ID,
                is_context: false,
                is_lambda_env: true,
                is_macro_code: false,
            });
            &params[1..]
        } else {
            if params.first().is_some_and(|p| p.type_id == ability_self_type) {
                return Err("it must take self by reference: `self: *mut self`".to_string());
            }
            params
        };
        for param in value_params {
            let substituted = self.substitute_in_type(param.type_id, subst_pairs);
            if self.substitute_in_type(substituted, &self_probe) != substituted {
                return Err(format!("parameter '{}' mentions self", self.ident_str(param.name)));
            }
            slot_params.push(FnParamType { type_id: substituted, ..*param });
        }
        let substituted_return = self.substitute_in_type(return_type, subst_pairs);
        if self.substitute_in_type(substituted_return, &self_probe) != substituted_return {
            return Err("its return type mentions self".to_string());
        }
        let slot_fn_type = self.add_type_anon(Type::Function(FunctionType {
            physical_params: slot_params.to_slice(),
            return_type: substituted_return,
            is_lambda: has_receiver,
        }));
        Ok(slot_fn_type)
    }

    fn eval_dyn_ability_object_type(
        &mut self,
        signature: TypedAbilitySignature,
        span: SpanId,
    ) -> K1Result<TypeId> {
        let ability = self.abilities.get(signature.specialized_ability_id);
        let ability_name = ability.name;
        let ability_self_type = ability.self_type_id;
        let ability_functions = ability.functions;

        for arg in ability.kind.arguments(self) {
            if self.get_type_variable_counts(*arg).type_parameter_count > 0 {
                kbail!(
                    self,
                    span,
                    "dyn does not yet support generic ability-side arguments; bind '{}' to a concrete type",
                    *arg
                );
            }
        }

        let subst_pairs = self.dyn_ability_subst_pairs(signature);
        let mut fields = self.mem.new_list(ability_functions.len() + 1);
        fields.push(StructTypeField {
            name: self.ast.idents.b.state,
            type_id: POINTER_TYPE_ID,
            span,
        });
        for fn_ref in self.mem.getn(ability_functions) {
            match self.dyn_slot_fn_type(ability_self_type, &subst_pairs, fn_ref.function_id) {
                Ok(slot_fn_type) => {
                    let slot_ptr_type = self.add_function_pointer_type(slot_fn_type);
                    fields.push(StructTypeField {
                        name: fn_ref.function_name,
                        type_id: slot_ptr_type,
                        span,
                    });
                }
                Err(_reason) => {}
            }
        }
        if fields.len() == 1 {
            kbail!(self, span, "Ability '{}' has no dyn-dispatchable functions", ability_name);
        }
        let fields_handle = fields.to_slice();
        let struct_representation =
            self.add_type_anon(Type::Struct(StructType::struc(fields_handle)));
        Ok(self.add_type_anon(Type::AbilityObject(AbilityObjectType {
            specialized_ability_id: signature.specialized_ability_id,
            impl_arguments: signature.impl_arguments,
            struct_representation,
        })))
    }

    fn ability_impl_to_dyn_object(
        &mut self,
        base_expr: TypedExprId,
        target_dyn_type: TypeId,
        scope_id: ScopeId,
        span: SpanId,
    ) -> K1Result<TypedExprId> {
        let ao = *self.types.get(target_dyn_type).as_ability_object().unwrap();
        let base_type = self.exprs.get_type(base_expr);
        let Some(reference) = self.types.get(base_type).as_reference() else {
            kbail!(
                self,
                span,
                "Only references erase to dyn ability objects; got {}. Use `.to-dyn()` on a value to allocate it first",
                base_type
            );
        };
        let implementor_type = reference.inner_type;

        let Ok((impl_handle, _)) = self.find_or_generate_specialized_ability_impl_for_type(
            implementor_type,
            ao.specialized_ability_id,
            false,
            scope_id,
            span,
        ) else {
            kbail!(
                self,
                span,
                "{} does not implement ability '{}'",
                implementor_type,
                self.abilities.get(ao.specialized_ability_id).name
            );
        };
        let found_impl = self.ability_impls.get(impl_handle.full_impl_id);
        let found_impl_arguments = found_impl.impl_arguments;
        debug_assert!(ao.impl_arguments.len() == found_impl_arguments.len());
        for (dyn_arg, impl_arg) in
            self.mem.getn(ao.impl_arguments).iter().zip(self.mem.getn(found_impl_arguments).iter())
        {
            if self.get_type_id_resolved(*dyn_arg, scope_id)
                != self.get_type_id_resolved(*impl_arg, scope_id)
            {
                kbail!(
                    self,
                    span,
                    "{} implements '{}', but with {}, not {}",
                    implementor_type,
                    self.abilities.get(ao.specialized_ability_id).name,
                    *impl_arg,
                    *dyn_arg,
                );
            }
        }

        let repr_fields = self.types.get(ao.struct_representation).expect_struct().fields;
        let repr_fields = self.mem.getn(repr_fields);

        let state_expr =
            self.synth_cast(base_expr, POINTER_TYPE_ID, CastType::ReferenceToPointer, None);
        let mut literal_fields = self.mem.new_list(repr_fields.len() as u32);
        literal_fields
            .push(StructLiteralField { name: self.ast.idents.b.state, expr: Some(state_expr) });
        for field in &repr_fields[1..] {
            let fn_ref = self
                .abilities
                .get(ao.specialized_ability_id)
                .find_function_by_name(&self.mem, field.name)
                .unwrap();
            let fn_index = fn_ref.index;
            let impl_function = *self
                .ability_impls
                .get(impl_handle.full_impl_id)
                .function_at_index(&self.mem, fn_index);
            match impl_function {
                AbilityImplFunction::FunctionId(impl_fn_id) => {
                    self.get_function_mut(impl_fn_id)
                        .flags
                        .insert(TypedFunctionFlags::AddressTaken);
                    let fn_ptr_expr = self.exprs.add(
                        TypedExpr::FunctionPointer(FunctionPointerExpr { function_id: impl_fn_id }),
                        field.type_id,
                        span,
                    );
                    literal_fields
                        .push(StructLiteralField { name: field.name, expr: Some(fn_ptr_expr) });
                }
                AbilityImplFunction::Unavailable => {
                    kbail!(
                        self,
                        span,
                        "Cannot erase {} to dyn[{}]: function '{}' is unavailable for this type",
                        implementor_type,
                        self.abilities.get(ao.specialized_ability_id).name,
                        field.name,
                    );
                }
                AbilityImplFunction::Abstract(_) => {
                    // Inside an abstract (where-bound generic) body
                    let placeholder = self.synth_phony(target_dyn_type, span);
                    return Ok(placeholder);
                }
            }
        }
        let fields_handle = literal_fields.to_slice();
        Ok(self.exprs.add(
            TypedExpr::Struct(StructLiteral { fields: fields_handle }),
            target_dyn_type,
            span,
        ))
    }

    fn declare_function(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        parent_scope_id: ScopeId,
        ability_info: Option<FunctionAbilityContextInfo>,
        namespace_id: NamespaceId,
    ) -> K1Result<Option<FunctionId>> {
        let namespace = self.namespaces.get(namespace_id);
        let is_reloadable = namespace.reload;
        let companion_type_id = namespace.companion_type_id;
        let ast_fn = *self.ast.get_function(parsed_function_id);
        let name_span = ast_fn.name_span;
        let is_debug = ast_fn.compiler_debug;
        let should_compile =
            self.execute_static_condition(ast_fn.compile_condition, parent_scope_id);
        if !should_compile {
            return Ok(None);
        }
        if is_reloadable && !matches!(ast_fn.linkage, Linkage::Standard) {
            self.fail_if_reload_ns(
                namespace_id,
                ast_fn.signature_span,
                "extern, export, and intrinsic fns",
            )?;
        }
        if is_debug {
            self.push_debug_level();
        }
        let mut self_ = scopeguard::guard(self, |s| {
            if is_debug {
                s.pop_debug_level()
            }
        });

        let mut is_ability_decl = false;
        let mut ability_id = None;
        let mut impl_info = None;
        let mut impl_self_type = None;
        let mut ability_kind_is_specialized = false;
        if let Some(info) = &ability_info {
            ability_id = Some(info.ability_id);
            ability_kind_is_specialized =
                self_.abilities.get(info.ability_id).kind.is_specialized();
            match &info.impl_info {
                None => is_ability_decl = true,
                Some(ii) => {
                    impl_info = Some(ii);
                    impl_self_type = Some(ii.self_type_id);
                }
            }
        }
        let is_ability_impl = impl_info.is_some();

        // In all of these scenarios, we've seen the function before, so we shouldn't do the AST
        // mapping; there's a more appropriate 'original' that already has it
        let skip_ast_mapping = ability_kind_is_specialized
            || impl_info.is_some_and(|ii| {
                ii.is_default
                    || ii.impl_kind.is_derived_from_blanket()
                    || ii.impl_kind.is_type_param_constraint()
                    || ii.impl_kind.is_builtin_derived()
            });

        let resolvable_by_name = !is_ability_impl && !ability_kind_is_specialized;

        let name = match impl_info {
            Some(impl_info) => self_.build_ident_with(|k1, s| {
                write!(
                    s,
                    "impl_{}{}.{}_for_t{}",
                    ability_id.unwrap().as_u32(),
                    k1.ident_str(k1.abilities.get(ability_id.unwrap()).name),
                    k1.ident_str(ast_fn.name),
                    impl_info.self_type_id.as_u32(),
                )
                .unwrap();
            }),
            None => ast_fn.name,
        };

        let fn_scope_id = self_.scopes.add_child_scope(
            parent_scope_id,
            ScopeType::FunctionScope,
            ScopeOwnerId::None,
        );

        let type_params = self_.compile_function_type_params(
            fn_scope_id,
            ast_fn.type_params,
            ast_fn.additional_where_constraints,
            if is_ability_decl { Some(ability_id.unwrap()) } else { None },
            if ability_info.is_none() { companion_type_id } else { None },
            if ability_info.is_none() { Some(namespace_id) } else { None },
        )?;

        let mut ability_where_constraints: SV4<AbilityFnWhereConstraint> = smallvec![];
        for c in self_.ast.mem.getn(ast_fn.additional_where_constraints) {
            let names_own_param =
                self_.ast.mem.getn(ast_fn.type_params).iter().any(|tp| tp.name == c.name);
            if names_own_param || is_ability_impl {
                continue;
            }
            if !is_ability_decl {
                kbail!(&**self_, c.span, "where clause names unknown type parameter: {}", c.name);
            }
            let Some((target, _)) = self_.scopes.find_type(fn_scope_id, c.name) else {
                kbail!(&**self_, c.span, "where clause names unknown type: {}", c.name);
            };
            let ParsedTypeConstraintExpr::Ability(ability_expr) = c.constraint_expr else {
                kbail!(
                    &**self_,
                    c.span,
                    "Only ability constraints are supported in ability function where clauses"
                );
            };
            let signature = self_.eval_ability_expr(ability_expr, false, fn_scope_id)?;
            ability_where_constraints.push(AbilityFnWhereConstraint {
                target,
                signature,
                span: c.span,
            });
        }

        let mut fnlike_type_params: List<FnlikeTypeParam, TypedProgram> = self_.mem.new_list(0);

        // Process parameters
        let param_count = ast_fn.params.len();
        let mut param_types: List<FnParamType, _> = self_.mem.new_list(param_count);
        let mut params = self_.mem.new_list(param_count);
        for (idx, fn_param) in self_.ast.mem.getn(ast_fn.params).iter().enumerate() {
            let type_expr = match fn_param.type_expr {
                ParsedFnParamType::Shorthand => {
                    self_.synth_parsed_type_app(fn_param.name, fn_param.span)
                }
                ParsedFnParamType::Expr(parsed_expr) => parsed_expr,
            };
            let type_id = self_.eval_type_expr_ext(
                type_expr,
                fn_scope_id,
                EvalTypeExprContext {
                    is_direct_function_parameter: true,
                    ..EvalTypeExprContext::EMPTY
                },
            )?;

            // Handle 'existential' type parameters. These are value parameters that
            // introduce a type parameter 'for free' inline.
            // - `some ty` function type parameter, inject the type parameter into the
            match self_.types.get(type_id) {
                Type::FunctionTypeParameter(ftp) => {
                    let name = ftp.name;
                    let span = ftp.span;
                    fnlike_type_params.push_grow(
                        &mut self_.mem,
                        FnlikeTypeParam { name, type_id, value_param_index: idx as u32, span },
                    );
                    // There's actually no way to refer to these types by name,
                    // so we don't need to add a name to the scope
                }
                _ => {}
            }

            // First arg Self shenanigans
            if idx == 0 {
                let name_is_self = fn_param.name == self_.ast.idents.b.self_
                    || fn_param.name == self_.ast.idents.b._self;

                // If the first argument is named self, check if it's a method of the companion type
                let is_ability_fn = ability_id.is_some();
                if name_is_self && !is_ability_fn {
                    if let Some(companion_type_id) = companion_type_id {
                        if self_.get_type_id_dereferenced(type_id) != companion_type_id {
                            match (
                                self_.types.get(companion_type_id),
                                self_.get_instance_info(self_.get_type_id_dereferenced(type_id)),
                            ) {
                                (Type::Generic(_g), Some(spec_info)) => {
                                    let ok = spec_info.generic_parent == companion_type_id;
                                    if !ok {
                                        kbail!(
                                            &**self_,
                                            fn_param.span,
                                            "First parameter named 'self' did not have a companion type",
                                        );
                                    }
                                }
                                _other => {
                                    kbail!(
                                        &**self_,
                                        fn_param.span,
                                        "First parameter named 'self' must be of the companion type, expected {} got {}",
                                        companion_type_id,
                                        type_id,
                                    );
                                }
                            }
                        }
                    } else {
                        kbail!(
                            &**self_,
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
                defn_span: fn_param.span,
            };

            let variable_id = self_.variables.add(variable);
            param_types.push(FnParamType {
                name: fn_param.name,
                type_id,
                is_context,
                is_lambda_env: false,
                is_macro_code: false,
            });
            params.push(TypedFunctionParam { variable_id, span: fn_param.span });
            if is_context {
                let inserted = self_.scopes.add_context_variable(
                    fn_scope_id,
                    fn_param.name,
                    variable_id,
                    type_id,
                );
                if !inserted {
                    kbail!(
                        &**self_,
                        fn_param.span,
                        "Duplicate context parameters for type {}",
                        type_id
                    );
                }
                self_.register_context_param_ability_keys(fn_scope_id, variable_id, type_id);
            } else {
                if !self_.scopes.add_variable(fn_scope_id, fn_param.name, variable_id) {
                    kbail!(&**self_, fn_param.span, "Duplicate parameter name: {}", fn_param.name);
                }
            }
        }

        if !type_params.is_empty() || !fnlike_type_params.is_empty() {
            self_.fail_if_reload_ns(namespace_id, ast_fn.signature_span, "generic fns")?;
        }

        let linkage = match impl_info {
            Some(info) if info.impl_kind == AbilityImplKind::BuiltinDerived => Linkage::Intrinsic,
            _ => match ast_fn.linkage {
                // Fill in the containing ns's lib(..) unless the fn declares its own lib
                Linkage::External { module_id, lib_name: None, fn_name } => Linkage::External {
                    module_id,
                    lib_name: self_.namespaces.get(namespace_id).lib_name,
                    fn_name,
                },
                other => other,
            },
        };
        if ast_fn.is_native && !matches!(linkage, Linkage::Standard) {
            kbail!(
                &**self_,
                ast_fn.signature_span,
                "'native' only on plain functions; intern, extern, and export govern their own ABI"
            );
        }
        if let Linkage::Exported { .. } = linkage {
            if !type_params.is_empty() || !fnlike_type_params.is_empty() {
                kbail!(&**self_, ast_fn.signature_span, "exported functions cannot be generic");
            }
            if ability_info.is_some() {
                kbail!(&**self_, ast_fn.signature_span, "ability functions cannot be exported");
            }
            if ast_fn.body.is_none() {
                kbail!(&**self_, ast_fn.signature_span, "exported functions must have a body");
            }
        }
        let intrinsic_type = match linkage {
            Linkage::Intrinsic => {
                let namespace_chain = self_.name_chain(namespace_id);
                let resolved = self_
                    .resolve_intrinsic_function_type(
                        ast_fn.name,
                        namespace_chain,
                        ability_id,
                        impl_self_type,
                    )
                    .map_err(|msg| {
                        kerr!(&**self_, ast_fn.span, "Error typechecking function: {}", msg,)
                    })?;
                Some(resolved)
            }
            Linkage::LlvmIntrinsic(llvm_name) => Some(Builtin::LlvmIntrinsic(llvm_name)),
            _ => None,
        };
        let return_type = match ast_fn.ret_type {
            None => self_.builtin_types.empty,
            Some(parsed_ret_type) => self_.eval_type_expr(parsed_ret_type, fn_scope_id)?,
        };

        // Typecheck 'main': It must take argc and argv of correct types, or nothing
        // And it must return an i32
        let is_main_fn =
            namespace_id == ROOT_NAMESPACE_ID && ast_fn.name == self_.ast.idents.b.main;
        if is_main_fn {
            match param_types.len() {
                0 => {}
                2 => {
                    let count = param_types[0].type_id == U32_TYPE_ID;
                    let values = param_types[1].type_id == POINTER_TYPE_ID;
                    if !count {
                        kbail!(&**self_, params[0].span, "First parameter must be {}", U32_TYPE_ID);
                    } else if !values {
                        kbail!(
                            &**self_,
                            params[1].span,
                            "Second parameter must be {}",
                            POINTER_TYPE_ID
                        );
                    }
                }
                n => {
                    kbail!(
                        &**self_,
                        ast_fn.signature_span,
                        "main must take exactly 0 or 2 parameters, got {}",
                        n
                    );
                }
            };
            match return_type {
                I32_TYPE_ID => {}
                _other => {
                    kbail!(&**self_, ast_fn.span, "main must return i32");
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
                    | AbilityImplKind::TypeParamConstraint
                    | AbilityImplKind::BuiltinDerived => TypedFunctionKind::AbilityImpl(
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

        let param_types_handle = param_types.to_slice();
        let function_type_id = self_.add_type_anon(Type::Function(FunctionType {
            physical_params: param_types_handle,
            return_type,
            is_lambda: false,
        }));

        let function_type_params_handle = fnlike_type_params.to_slice_trim(&mut self_.mem);
        let function_id = self_.functions.next_id();
        for v in params.iter() {
            self_.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(function_id);
        }
        let param_variables_handle = params.to_slice();
        let where_constraints_handle = self_.mem.pushn(&ability_where_constraints);
        let is_manifest = name == self_.ast.idents.b.module
            && self_.namespaces.get(namespace_id).name == self_.ast.idents.b.build;
        let mut flags = TypedFunctionFlags::empty();
        flags.set(TypedFunctionFlags::CompilerDebug, is_debug);
        flags.set(TypedFunctionFlags::ModuleManifest, is_manifest);
        flags.set(TypedFunctionFlags::Reloadable, is_reloadable);
        flags.set(TypedFunctionFlags::AbiNative, ast_fn.is_native);
        let actual_function_id = self_.add_function(TypedFunction {
            name,
            scope: fn_scope_id,
            namespace_id,
            params: param_variables_handle,
            type_params,
            fnlike_type_params: function_type_params_handle,
            ability_where_constraints: where_constraints_handle,
            body_block: None,
            builtin_type: intrinsic_type,
            linkage,
            specialization_info: None,
            parsed_id: parsed_function_id.into(),
            kind,
            type_id: function_type_id,
            flags,
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        });
        debug_assert_eq!(actual_function_id, function_id);

        if resolvable_by_name {
            if !self_.scopes.add_function(parent_scope_id, ast_fn.name, function_id) {
                let signature_span = ast_fn.signature_span;
                let error =
                    kerr!(&**self_, signature_span, "Function name {} is taken", ast_fn.name);
                self_.report(error);
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

        self_.emit_ls_entity(name_span, LsEntityKind::Function { function_id, is_defn: true });

        Ok(Some(function_id))
    }

    fn find_companion_type_param(
        &self,
        companion_type_id: TypeId,
        name: StringId,
    ) -> Option<TypeId> {
        let Type::Generic(generic) = self.types.get(companion_type_id) else {
            return None;
        };
        let param =
            self.mem.getn(generic.params).iter().find(|p| {
                self.types.get(**p).as_type_parameter().is_some_and(|tp| tp.name == name)
            })?;
        Some(*param)
    }

    fn get_namespace_type_param(
        &mut self,
        ns_id: NamespaceId,
        name: StringId,
        span: SpanId,
    ) -> TypeId {
        if let Some(existing) = self.namespace_type_params.get(&NameInNamespace { ns_id, name }) {
            return *existing;
        }
        let ns_scope_id = self.namespaces.get_scope(ns_id);
        let id = self.add_type_parameter(
            TypeParameter {
                name,
                static_constraint: None,
                predicate_functions: MSlice::empty(),
                scope_id: ns_scope_id,
                span,
            },
            smallvec![],
        );
        self.namespace_type_params.insert(NameInNamespace { ns_id, name }, id);
        id
    }

    fn compile_function_type_params(
        &mut self,
        fn_scope_id: ScopeId,
        ast_type_params: AstSlice<ParsedTypeParam>,
        where_constraints: AstSlice<ParsedTypeConstraint>,
        ability_id: Option<AbilityId>,
        companion_type_id: Option<TypeId>,
        namespace_id: Option<NamespaceId>,
    ) -> K1Result<TypeIdSlice> {
        // Instantiate type arguments.
        let mut type_params: List<TypeId, _> = self.mem.new_list(ast_type_params.len() + 1);

        // Inject the 'Self' type parameter
        if let Some(ability_id) = ability_id {
            let self_type_id = self.abilities.get(ability_id).self_type_id;
            type_params.push(self_type_id)
        }
        for type_parameter in self.ast.mem.getn(ast_type_params) {
            let mut ability_constraint_signatures = SmallVec::new();
            let mut predicate_functions = self.mem.new_list(0);
            let mut static_constraint: Option<TypeId> = None;

            for parsed_constraint in self.ast.mem.getn(type_parameter.constraints).iter().chain(
                self.ast
                    .mem
                    .getn(where_constraints)
                    .iter()
                    .filter(|c| c.name == type_parameter.name)
                    .map(|c| &c.constraint_expr),
            ) {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig =
                            self.eval_ability_expr(*ability_expr, false, fn_scope_id)?;
                        ability_constraint_signatures.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Predicate(qident) => {
                        predicate_functions.push_grow(&mut self.mem, *qident);
                    }
                    ParsedTypeConstraintExpr::Static(static_expr) => {
                        let static_type = self.eval_type_expr(*static_expr, fn_scope_id)?;
                        match &static_constraint {
                            None => static_constraint = Some(static_type),
                            Some(_) => {
                                kbail!(
                                    self,
                                    type_parameter.span,
                                    "Cannot specify more than one static constraint for a parameter"
                                );
                            }
                        }
                    }
                };
            }
            let predicate_functions_handle = predicate_functions.to_slice_trim(&mut self.mem);
            let unconstrained = ability_constraint_signatures.is_empty()
                && predicate_functions_handle.is_empty()
                && static_constraint.is_none();
            let shared_param = if unconstrained {
                companion_type_id
                    .and_then(|ct| self.find_companion_type_param(ct, type_parameter.name))
                    .or_else(|| {
                        namespace_id.map(|ns| {
                            self.get_namespace_type_param(
                                ns,
                                type_parameter.name,
                                type_parameter.span,
                            )
                        })
                    })
            } else {
                None
            };
            let type_variable_id = match shared_param {
                Some(id) => id,
                None => self.add_type_parameter(
                    TypeParameter {
                        name: type_parameter.name,
                        static_constraint,
                        predicate_functions: predicate_functions_handle,
                        scope_id: fn_scope_id,
                        span: type_parameter.span,
                    },
                    ability_constraint_signatures,
                ),
            };
            type_params.push(type_variable_id);
            if !self.scopes.add_type(fn_scope_id, type_parameter.name, type_variable_id) {
                kbail!(
                    self,
                    type_parameter.span,
                    "Duplicate type variable name: {}",
                    type_parameter.name
                );
            }
        }
        Ok(type_params.to_slice())
    }

    fn declare_macro(
        &mut self,
        parsed_macro_id: parse::ParsedMacroId,
        parent_scope_id: ScopeId,
    ) -> K1Result<Option<FunctionId>> {
        let ast_macro = self.ast.get_macro(parsed_macro_id);
        if let Some(owner_ns) = self.scopes.get_scope_owner(parent_scope_id).as_namespace() {
            self.fail_if_reload_ns(owner_ns, ast_macro.span, "macros")?;
        }
        let name = ast_macro.name;
        let name_span = ast_macro.name_span;
        let signature_span = ast_macro.signature_span;
        let params_slice = ast_macro.params;
        let type_params_slice = ast_macro.type_params;
        let compiler_debug = ast_macro.compiler_debug;
        let condition = ast_macro.compile_condition;
        if !self.execute_static_condition(condition, parent_scope_id) {
            return Ok(None);
        }

        let fn_scope_id = self.scopes.add_child_scope(
            parent_scope_id,
            ScopeType::FunctionScope,
            ScopeOwnerId::None,
        );
        let code_type = self.builtin_types.code();

        let type_params = self.compile_function_type_params(
            fn_scope_id,
            type_params_slice,
            MSlice::empty(),
            None,
            None,
            None,
        )?;

        let param_count = params_slice.len();
        let mut param_types: List<FnParamType, _> = self.mem.new_list(param_count);
        let mut params = self.mem.new_list(param_count);
        for fn_param in self.ast.mem.getn(params_slice).iter() {
            let (type_id, is_code) = match fn_param.type_expr {
                ParsedFnParamType::Shorthand => (code_type, true),
                ParsedFnParamType::Expr(type_expr) => {
                    let t = self.eval_type_expr(type_expr, fn_scope_id)?;
                    let is_code = t == code_type;
                    (t, is_code)
                }
            };
            if let Type::FunctionTypeParameter(_ftp) = self.types.get(type_id) {
                kbail!(
                    self,
                    fn_param.span,
                    "'some fn' parameters are not allowed in macro parameters"
                );
            }
            let variable_id = self.variables.add(Variable {
                name: fn_param.name,
                type_id,
                owner_scope: fn_scope_id,
                flags: VariableFlags::empty(),
                usage_count: 0,
                kind: VariableKind::FnParam(FunctionId::PENDING),
                defn_span: fn_param.span,
            });
            param_types.push(FnParamType {
                name: fn_param.name,
                type_id,
                is_context: false,
                is_lambda_env: false,
                is_macro_code: is_code,
            });
            params.push(TypedFunctionParam { variable_id, span: fn_param.span });
            if !self.scopes.add_variable(fn_scope_id, fn_param.name, variable_id) {
                kbail!(self, fn_param.span, "Duplicate parameter name: {}", fn_param.name);
            }
        }

        let param_types_handle = param_types.to_slice();
        let function_type_id = self.add_type_anon(Type::Function(FunctionType {
            physical_params: param_types_handle,
            return_type: code_type,
            is_lambda: false,
        }));

        let function_id = self.functions.next_id();
        for v in params.iter() {
            self.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(function_id);
        }
        let param_variables_handle = params.to_slice();
        let mut flags = TypedFunctionFlags::Macro;
        flags.set(TypedFunctionFlags::CompilerDebug, compiler_debug);
        let actual_function_id = self.add_function(TypedFunction {
            name,
            scope: fn_scope_id,
            namespace_id: self.scopes.nearest_parent_namespace(fn_scope_id),
            params: param_variables_handle,
            type_params,
            fnlike_type_params: MSlice::empty(),
            ability_where_constraints: MSlice::empty(),
            body_block: None,
            builtin_type: None,
            linkage: Linkage::Standard,
            specialization_info: None,
            parsed_id: ParsedId::Macro(parsed_macro_id),
            kind: TypedFunctionKind::Standard,
            type_id: function_type_id,
            flags,
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        });
        debug_assert_eq!(actual_function_id, function_id);

        if !self.scopes.add_function(parent_scope_id, name, function_id) {
            let error = kerr!(
                self,
                signature_span,
                "Name {} is taken (macros and functions may not share names)",
                name
            );
            self.report(error);
        }
        let existed = self.macro_ast_mappings.insert(parsed_macro_id, function_id).is_some();
        debug_assert!(!existed);
        self.scopes.set_scope_owner_id(fn_scope_id, ScopeOwnerId::Function(function_id));
        self.emit_ls_entity(name_span, LsEntityKind::Function { function_id, is_defn: true });

        Ok(Some(function_id))
    }

    pub fn eval_function_body(&mut self, declaration_id: FunctionId) -> K1Result<()> {
        let function = self.get_function(declaration_id);
        if function.body_failure.is_some() || function.body_block.is_some() {
            return Ok(());
        }
        let result = self.eval_function_body_inner(declaration_id);
        if let Err(e) = result {
            self.get_function_mut(declaration_id).body_failure = Some(e);
        }
        result
    }

    fn eval_function_body_inner(&mut self, declaration_id: FunctionId) -> K1Result<()> {
        let function = self.get_function(declaration_id);
        let is_debug = function.compiler_debug();
        if is_debug {
            self.push_debug_level();
        }
        let function = self.get_function(declaration_id);
        let fn_scope_id = function.scope;
        let return_type = self.get_function_type(declaration_id).return_type;
        let is_extern = matches!(function.linkage, Linkage::External { .. });
        let is_concrete = function.is_concrete();
        // Here. Which type of builtin is it? Some, we should synthesize here.
        let (builtin_type_phys_fn, other_intrinsic) = match function.builtin_type {
            Some(Builtin::TyperPhysicalFunction(f)) => (Some(f), false),
            Some(_) => (None, true),
            None => (None, false),
        };
        let is_ability_defn = matches!(function.kind, TypedFunctionKind::AbilityDefn(_));

        let (parsed_function_ret_type, function_signature_span, parsed_body) =
            match function.parsed_id {
                ParsedId::Function(ast_id) => {
                    let parsed_function = self.ast.get_function(ast_id);
                    (parsed_function.ret_type, parsed_function.signature_span, parsed_function.body)
                }
                ParsedId::Macro(macro_id) => {
                    let parsed_macro = self.ast.get_macro(macro_id);
                    (None, parsed_macro.signature_span, Some(parsed_macro.body))
                }
                _ => self.ice("Function body for a non-function parsed id", None),
            };

        let no_body_expected = other_intrinsic || is_extern || is_ability_defn;
        let is_generic = !is_concrete;

        let body_block = match parsed_body.as_ref() {
            None if builtin_type_phys_fn.is_some() => {
                let block_id = self.generate_intrinsic_function_body(
                    declaration_id,
                    fn_scope_id,
                    builtin_type_phys_fn.unwrap(),
                )?;
                Some(block_id)
            }
            None if no_body_expected => None,
            None => {
                kbail!(self, function_signature_span, "function is missing implementation");
            }
            Some(_) if no_body_expected => {
                kbail!(self, function_signature_span, "unexpected function implementation");
            }
            Some(body_ast) => {
                if function.specialization_info.is_some() && !is_concrete {
                    debug!(
                        "Skipping typecheck of body for non-concrete specialization of {}",
                        self.function_id_to_string(declaration_id, true),
                    );
                    return Ok(());
                };
                let body_ast = *body_ast;
                let ParsedExpr::Block(block_itself) = self.ast.exprs.get(body_ast) else {
                    kbail!(self, function_signature_span, "[bug] function bodies must be blocks");
                };
                let block_itself = *block_itself;

                let eval_ctx = EvalExprContext::make(fn_scope_id)
                    .with_expected_type(Some(return_type))
                    .with_manifest_eval(self.get_function(declaration_id).is_module_manifest_fn())
                    // Why do we care to indicate if the function is generic?
                    // Currently, its because we want to avoid running #static and #meta blocks
                    // until the types and static values are provided
                    // There are now other implications, like we only want to do
                    // lsp stuff in the generic pass, and not repeat it in every
                    // specialization pass
                    .with_is_generic_pass(is_generic);

                let block =
                    self.with_clean_inference(|k1| k1.eval_block(&block_itself, eval_ctx, true))?;
                if let Err(msg) =
                    self.check_types(return_type, self.exprs.get_type(block), fn_scope_id)
                {
                    let return_type_span = match parsed_function_ret_type {
                        None => function_signature_span,
                        Some(rt) => self.ast.get_type_expr_span(rt),
                    };
                    kbail!(self, return_type_span, "Return type mismatch: {}", msg);
                } else {
                    Some(block)
                }
            }
        };

        if let Some(body_block) = body_block {
            let f = self.functions.get(declaration_id);

            // Macros may conditionally ignore params, e.g. a disabled debug macro
            if !is_generic && !f.is_macro() {
                for param_variable in self.mem.getn(f.params).iter() {
                    self.warn_variable_usage_counts(
                        "Parameter",
                        param_variable.variable_id,
                        param_variable.span,
                    );
                }
            }

            self.get_function_mut(declaration_id).body_block = Some(body_block);
        }

        if is_debug {
            eprintln!("DEBUG\n{}", self.function_id_to_string(declaration_id, true));
            self.pop_debug_level();
        }
        Ok(())
    }

    fn synth_enum_tag_name_match(
        &mut self,
        enum_param_expr: TypedExprId,
        fn_span: SpanId,
    ) -> TypedExprId {
        let enum_type_id = self.exprs.get_type(enum_param_expr);
        let enum_arg_int_expr = self.synth_enum_get_value(enum_param_expr, fn_span);
        let Type::Enum(enum_type) = self.types.get(enum_type_id) else {
            self.ice_span(fn_span, "not an enum");
        };
        let mut arms: List<TypedMatchArm, _> = self.mem.new_list(enum_type.member_values.len());
        for member in self.mem.getn(enum_type.member_values) {
            let case = self.static_values.add_int(member.int_value);
            let member_name_expr = self.synth_string_literal(member.name, fn_span);
            arms.push(TypedMatchArm {
                case: Some(case),
                condition: MatchingCondition { instrs: MSlice::empty() },
                consequent_expr: member_name_expr,
            });
        }
        let match_expr = TypedExpr::Match(TypedMatchExpr {
            subject_defn: None,
            scrutinee: Some(enum_arg_int_expr),
            arms: arms.to_slice(),
        });
        self.exprs.add(match_expr, self.builtin_types.string(), fn_span)
    }

    fn generate_intrinsic_function_body(
        &mut self,
        function_id: FunctionId,
        fn_scope_id: ScopeId,
        kind: BuiltinTyperFunction,
    ) -> K1Result<TypedExprId> {
        let f = self.functions.get(function_id);
        let return_type = self.get_function_type(function_id).return_type;
        let fn_span = self.ast.get_span_for_id(f.parsed_id);
        let params = f.params;
        let body_expr = match kind {
            BuiltinTyperFunction::EnumAbilityGetValue => {
                let enum_param = *self.mem.get_nth(params, 0);
                let enum_param_expr = self.synth_variable_expr(enum_param.variable_id, fn_span);
                let value_expr = self.synth_enum_get_value(enum_param_expr, fn_span);
                Ok(value_expr)
            }
            BuiltinTyperFunction::EnumAbilityGetTagName => {
                let enum_param = *self.mem.get_nth(params, 0);
                let enum_param_expr = self.synth_variable_expr(enum_param.variable_id, fn_span);
                Ok(self.synth_enum_tag_name_match(enum_param_expr, fn_span))
            }
            BuiltinTyperFunction::SumAbilityGetTag => {
                let sum_param = *self.mem.get_nth(params, 0);
                let sum_param_expr = self.synth_variable_expr(sum_param.variable_id, fn_span);
                let value_expr = self.synth_sum_get_tag(sum_param_expr, fn_span);
                Ok(value_expr)
            }
            BuiltinTyperFunction::SumEquals => {
                let sum_param_a = *self.mem.get_nth(params, 0);
                let sum_param_b = *self.mem.get_nth(params, 1);
                let arg_a = self.synth_variable_expr(sum_param_a.variable_id, fn_span);
                let arg_b = self.synth_variable_expr(sum_param_b.variable_id, fn_span);
                let sum_type_id = self.exprs.get_type(arg_a);
                let param_a_tag_expr = self.synth_sum_get_tag(arg_a, fn_span);
                let param_b_tag_expr = self.synth_sum_get_tag(arg_b, fn_span);

                let Type::Sum(sum_type) = self.types.get(sum_type_id) else {
                    self.ice_span(fn_span, "not a sum");
                };
                let mut arms: List<TypedMatchArm, _> =
                    self.mem.new_list(sum_type.variants.len() + 1);
                for variant in self.mem.getn(sum_type.variants) {
                    let case = self.static_values.add_int(variant.tag_value);
                    let mut conditions = self.mem.new_list(1 + variant.payload.is_some() as u32);
                    conditions.push(MatchingConditionInstr::IntEquals {
                        subject: param_b_tag_expr,
                        value: case,
                    });
                    match variant.payload {
                        None => {}
                        Some(payload_type_id) => {
                            let payload_a = self.exprs.add(
                                TypedExpr::SumGetPayload(GetSumPayload {
                                    sum_expr: arg_a,
                                    variant_index: variant.index,
                                    packed: false,
                                }),
                                payload_type_id,
                                fn_span,
                            );
                            let payload_b = self.exprs.add(
                                TypedExpr::SumGetPayload(GetSumPayload {
                                    sum_expr: arg_b,
                                    variant_index: variant.index,
                                    packed: false,
                                }),
                                payload_type_id,
                                fn_span,
                            );
                            let payload_equals =
                                self.synth_equals_call_simple(payload_a, payload_b, fn_span);
                            conditions.push(MatchingConditionInstr::cond(payload_equals))
                        }
                    }
                    let conditions = conditions.to_slice();
                    arms.push(TypedMatchArm {
                        case: Some(case),
                        condition: MatchingCondition { instrs: conditions },
                        consequent_expr: self.synth_bool(true, fn_span),
                    });
                }
                arms.push(TypedMatchArm {
                    case: None,
                    condition: MatchingCondition { instrs: MSlice::empty() },
                    consequent_expr: self.synth_bool(false, fn_span),
                });
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    subject_defn: None,
                    scrutinee: Some(param_a_tag_expr),
                    arms: arms.to_slice(),
                });
                let match_expr_id = self.exprs.add(match_expr, BOOL_TYPE_ID, fn_span);
                Ok(match_expr_id)
            }
            BuiltinTyperFunction::SumAbilityGetName => {
                let sum_param = *self.mem.get_nth(params, 0);
                let sum_param_expr = self.synth_variable_expr(sum_param.variable_id, fn_span);
                let sum_type_id = self.exprs.get_type(sum_param_expr);
                let sum_arg_int_expr = self.synth_sum_get_tag(sum_param_expr, fn_span);
                let Type::Sum(sum_type) = self.types.get(sum_type_id) else {
                    self.ice_span(fn_span, "not a sum");
                };
                let mut arms: List<TypedMatchArm, _> = self.mem.new_list(sum_type.variants.len());
                for variant in self.mem.getn(sum_type.variants) {
                    let case = self.static_values.add_int(variant.tag_value);
                    let member_name_expr = self.synth_string_literal(variant.name, fn_span);
                    arms.push(TypedMatchArm {
                        case: Some(case),
                        condition: MatchingCondition { instrs: MSlice::empty() },
                        consequent_expr: member_name_expr,
                    });
                }
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    subject_defn: None,
                    scrutinee: Some(sum_arg_int_expr),
                    arms: arms.to_slice(),
                });
                Ok(self.exprs.add(match_expr, self.builtin_types.string(), fn_span))
            }
            BuiltinTyperFunction::StructEquals => {
                let struct_param_a = *self.mem.get_nth(params, 0);
                let struct_param_b = *self.mem.get_nth(params, 1);
                let arg_a = self.synth_variable_expr(struct_param_a.variable_id, fn_span);
                let arg_b = self.synth_variable_expr(struct_param_b.variable_id, fn_span);
                let struct_type_id = self.exprs.get_type(arg_a);
                let struct_type = self.types.get(struct_type_id).as_struct().unwrap();
                let mut conditions = self.mem.new_list(struct_type.fields.len());
                for (index, _field) in self.mem.getn(struct_type.fields).iter().enumerate() {
                    let field_of_a = self.synth_field_access(arg_a, index, fn_span);
                    let field_of_b = self.synth_field_access(arg_b, index, fn_span);
                    let equals_expr =
                        self.synth_equals_call_simple(field_of_a, field_of_b, fn_span);
                    conditions.push(MatchingConditionInstr::cond(equals_expr))
                }
                let equals_arm = TypedMatchArm {
                    case: None,
                    condition: MatchingCondition { instrs: conditions.to_slice() },
                    consequent_expr: self.synth_bool(true, fn_span),
                };
                let false_arm = TypedMatchArm {
                    case: None,
                    condition: MatchingCondition { instrs: MSlice::empty() },
                    consequent_expr: self.synth_bool(false, fn_span),
                };
                let arms = self.mem.pushn(&[equals_arm, false_arm]);
                let match_expr =
                    TypedExpr::Match(TypedMatchExpr { subject_defn: None, scrutinee: None, arms });
                let match_expr_id = self.exprs.add(match_expr, BOOL_TYPE_ID, fn_span);
                // eprintln!("STRUCT EQUALS\n{}", self.expr_to_string(match_expr_id));
                Ok(match_expr_id)
            }
            BuiltinTyperFunction::StructPrintTo => {
                let struct_param = *self.mem.get_nth(params, 0);
                let writer_param = *self.mem.get_nth(params, 1);

                let struct_expr = self.synth_variable_expr(struct_param.variable_id, fn_span);
                let struct_type_id = self.exprs.get_type(struct_expr);
                let struct_type = *self.types.get(struct_type_id).as_struct().unwrap();
                let writer_expr = self.synth_variable_expr(writer_param.variable_id, fn_span);

                let mut block = self.new_block_builder(
                    fn_scope_id,
                    ScopeType::LexicalBlock,
                    fn_span,
                    (struct_type.fields.len() * 4) // 4 per field
                    - 1  // minus last delimiter
                    + 2, // open/close brace
                );
                let ctx = EvalExprContext::make(block.scope_id);

                let obrace_expr = self.synth_string_literal_from_str(".{ ", fn_span);
                let write_obrace = self.synth_printto_call(obrace_expr, writer_expr, ctx)?;
                self.push_block_expr_id(&mut block, write_obrace);

                let comma_expr = self.synth_string_literal_from_str(", ", fn_span);
                let colon_expr = self.synth_string_literal_from_str(" = ", fn_span);
                // FIXME: This won't support indentation until we pluck 'print' out into 'Display' and 'Inspect'
                for (index, field) in self.mem.getn(struct_type.fields).iter().enumerate() {
                    let field_expr = self.synth_field_access(struct_expr, index, fn_span);
                    let name_expr = self.synth_string_literal(field.name, fn_span);
                    let print_name_expr = self.synth_printto_call(name_expr, writer_expr, ctx)?;
                    let print_colon_expr = self.synth_printto_call(colon_expr, writer_expr, ctx)?;

                    let print_value_expr = self.synth_printto_call(field_expr, writer_expr, ctx)?;

                    self.push_block_expr_id(&mut block, print_name_expr);
                    self.push_block_expr_id(&mut block, print_colon_expr);
                    self.push_block_expr_id(&mut block, print_value_expr);

                    if index as u32 != struct_type.fields.len() - 1 {
                        let print_comma_expr =
                            self.synth_printto_call(comma_expr, writer_expr, ctx)?;
                        self.push_block_expr_id(&mut block, print_comma_expr);
                    }
                }

                let cbrace_expr = self.synth_string_literal_from_str(" }", fn_span);
                let write_cbrace = self.synth_printto_call(cbrace_expr, writer_expr, ctx)?;
                self.push_block_expr_id(&mut block, write_cbrace);

                let block_expr_id = self.exprs.add_block(block, EMPTY_TYPE_ID);
                // eprintln!("STRUCT PRINT TO\n{}", self.expr_to_string(block_expr_id));
                Ok(block_expr_id)
            }
            BuiltinTyperFunction::SumPrintTo => {
                let sum_param = *self.mem.get_nth(params, 0);
                let writer_param = *self.mem.get_nth(params, 1);

                let sum_expr = self.synth_variable_expr(sum_param.variable_id, fn_span);
                let writer_expr = self.synth_variable_expr(writer_param.variable_id, fn_span);
                let sum_type_id = self.exprs.get_type(sum_expr);
                let tag_expr = self.synth_sum_get_tag(sum_expr, fn_span);
                let Type::Sum(sum_type) = self.types.get(sum_type_id) else {
                    self.ice_span(fn_span, "not a sum");
                };
                let variants = sum_type.variants;

                let mut arms: List<TypedMatchArm, _> = self.mem.new_list(variants.len());
                for variant in self.mem.getn(variants) {
                    let case = self.static_values.add_int(variant.tag_value);

                    let mut block =
                        self.new_block_builder(fn_scope_id, ScopeType::LexicalBlock, fn_span, 3);
                    let ctx = EvalExprContext::make(block.scope_id);
                    let label = format!(":{}", self.ident_str(variant.name));
                    let label_expr = self.synth_string_literal_from_str(&label, fn_span);
                    let write_label = self.synth_printto_call(label_expr, writer_expr, ctx)?;
                    self.push_block_expr_id(&mut block, write_label);
                    if let Some(payload_type_id) = variant.payload {
                        let space_expr = self.synth_string_literal_from_str(" ", fn_span);
                        let write_space = self.synth_printto_call(space_expr, writer_expr, ctx)?;
                        self.push_block_expr_id(&mut block, write_space);
                        let payload_expr = self.exprs.add(
                            TypedExpr::SumGetPayload(GetSumPayload {
                                sum_expr,
                                variant_index: variant.index,
                                packed: false,
                            }),
                            payload_type_id,
                            fn_span,
                        );
                        let write_payload =
                            self.synth_printto_call(payload_expr, writer_expr, ctx)?;
                        self.push_block_expr_id(&mut block, write_payload);
                    }
                    arms.push(TypedMatchArm {
                        case: Some(case),
                        condition: MatchingCondition { instrs: MSlice::empty() },
                        consequent_expr: self.exprs.add_block(block, EMPTY_TYPE_ID),
                    });
                }
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    subject_defn: None,
                    scrutinee: Some(tag_expr),
                    arms: arms.to_slice(),
                });
                Ok(self.exprs.add(match_expr, EMPTY_TYPE_ID, fn_span))
            }
            BuiltinTyperFunction::EnumPrintTo => {
                let enum_param = *self.mem.get_nth(params, 0);
                let writer_param = *self.mem.get_nth(params, 1);
                let enum_param_expr = self.synth_variable_expr(enum_param.variable_id, fn_span);
                let writer_expr = self.synth_variable_expr(writer_param.variable_id, fn_span);
                let name_expr = self.synth_enum_tag_name_match(enum_param_expr, fn_span);
                let ctx = EvalExprContext::make(fn_scope_id);
                self.synth_printto_call(name_expr, writer_expr, ctx)
            }
        }?;

        let body_expr_type = self.exprs.get_type(body_expr);
        if let Err(msg) = self.check_types(return_type, body_expr_type, fn_scope_id) {
            self.ice_span(fn_span, format!("Builtin wrong return type: {msg}"))
        }
        let mut block = self.new_block_builder(fn_scope_id, ScopeType::FunctionScope, fn_span, 1);
        let ret = self.exprs.add(
            TypedExpr::Return(TypedReturn { value: body_expr, returned_variable: None }),
            NEVER_TYPE_ID,
            fn_span,
        );
        self.push_block_expr_id(&mut block, ret);
        let block_id = self.exprs.add_block(block, NEVER_TYPE_ID);
        Ok(block_id)
    }

    fn warn_variable_usage_counts(
        &mut self,
        kind_name: &str,
        variable_id: VariableId,
        span: SpanId,
    ) {
        let v = self.variables.get(variable_id);
        if v.usage_count == 0 && !v.is_user_hidden() {
            let var_name_str = self.ident_str(v.name);
            if !var_name_str.starts_with("_") {
                self.report(kwarn!(self, span, "{} is never used: {}", kind_name, var_name_str));
            }
        }
    }

    fn compile_ability_definition(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
        scope_id: ScopeId,
    ) -> K1Result<Option<AbilityId>> {
        if let Some(ability_id) = self.find_ability_mapping(parsed_ability_id) {
            return Ok(Some(ability_id));
        }
        let parsed_ability = self.ast.get_ability(parsed_ability_id).clone();
        if !self.execute_static_condition(parsed_ability.compile_condition, scope_id) {
            return Ok(None);
        }
        let parent_namespace_id = self.scopes.get_scope_owner(scope_id).as_namespace().unwrap();
        self.fail_if_reload_ns(parent_namespace_id, parsed_ability.span, "ability definitions")?;

        // If an ability namespace and a regular namespace exist as siblings with the same name,
        // they share a scope, and we put the ability stuff "Self" type, other type params,
        // as well as ability functions, in a child scope. This way everyone can find stuff
        // with a single search, but we also keep the generic stuff tucked away.
        //
        // In this case, the mechanism is just to 'adopt' the existing scope/ns and start putting our stuff inside it
        let (namespace_id, ns_scope_id) = match self
            .namespaces
            .find_child_by_name(parent_namespace_id, parsed_ability.name)
        {
            Some(existing_ns_id) => {
                let existing = self.namespaces.get(existing_ns_id);
                let adoptable = existing.namespace_type == NamespaceKind::User;
                if !adoptable {
                    kbail!(
                        self,
                        parsed_ability.span,
                        "Namespace with name {} already exists",
                        parsed_ability.name
                    );
                }
                let ns_scope_id = existing.scope_id;
                self.namespaces.get_mut(existing_ns_id).namespace_type = NamespaceKind::Ability;
                (existing_ns_id, ns_scope_id)
            }
            None => {
                let ns_scope_id =
                    self.scopes.add_child_scope(scope_id, ScopeType::Namespace, ScopeOwnerId::None);
                let ability_namespace = Namespace {
                    name: parsed_ability.name,
                    scope_id: ns_scope_id,
                    namespace_type: NamespaceKind::Ability,
                    companion_type_id: None,
                    parent_id: Some(parent_namespace_id),
                    owner_module: Some(self.module_in_progress.unwrap()),
                    parsed_id: ParsedId::Ability(parsed_ability_id),
                    lib_name: None,
                    reload: false,
                };
                let namespace_id = self.namespaces.add(ability_namespace);
                if !self.scopes.add_namespace(scope_id, parsed_ability.name, namespace_id) {
                    let occupant =
                        self.scopes.find_namespace_local(scope_id, parsed_ability.name).unwrap();
                    if self.namespaces.get(occupant).parent_id == Some(parent_namespace_id) {
                        kbail!(
                            self,
                            parsed_ability.span,
                            "Namespace with name {} already exists",
                            parsed_ability.name
                        );
                    }
                    self.scopes.replace_namespace(scope_id, parsed_ability.name, namespace_id);
                }
                self.scopes.set_scope_owner_id(ns_scope_id, ScopeOwnerId::Namespace(namespace_id));
                (namespace_id, ns_scope_id)
            }
        };

        let ability_scope_id =
            self.scopes.add_child_scope(ns_scope_id, ScopeType::AbilityDefn, ScopeOwnerId::None);

        let self_ident_id = self.ast.idents.b.self_;
        let mut ability_params: List<TypedAbilityParam, _> =
            self.mem.new_list(parsed_ability.params.len() + 1);
        let self_type_id = self.add_type_parameter(
            TypeParameter {
                name: self_ident_id,
                static_constraint: None,
                predicate_functions: MSlice::empty(),
                scope_id: ability_scope_id,
                span: parsed_ability.span,
            },
            smallvec![],
        );
        let _ = self.scopes.add_type(ability_scope_id, self_ident_id, self_type_id);
        for ability_param in self.ast.mem.getn(parsed_ability.params) {
            let mut ability_constraint_signatures: SV4<TypedAbilitySignature> = smallvec![];
            let mut predicate_functions = self.mem.new_list(0);
            for constraint in self.ast.mem.getn(ability_param.constraints) {
                match constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let signature =
                            self.eval_ability_expr(*ability_expr, false, ability_scope_id)?;
                        ability_constraint_signatures.push(signature);
                    }
                    ParsedTypeConstraintExpr::Static(_) => {}
                    ParsedTypeConstraintExpr::Predicate(qident) => {
                        predicate_functions.push_grow(&mut self.mem, *qident);
                    }
                }
            }
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &self.ast.mem,
                    ability_param.constraints,
                ) {
                    Ok(Some(parsed_constraint)) => {
                        Some(self.eval_type_expr(parsed_constraint, ability_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => kbail!(self, ability_param.span, "{}", msg),
                };

            let predicate_functions_handle = predicate_functions.to_slice_trim(&mut self.mem);
            let param_type_id = self.add_type_parameter(
                TypeParameter {
                    name: ability_param.name,
                    static_constraint: maybe_static_constraint,
                    predicate_functions: predicate_functions_handle,
                    scope_id: ability_scope_id,
                    span: ability_param.span,
                },
                ability_constraint_signatures,
            );

            if !self.scopes.add_type(ability_scope_id, ability_param.name, param_type_id) {
                kbail!(self, ability_param.span, "Duplicate type variable: {}", ability_param.name);
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
            TypedAbilityKind::Generic { specializations: MList::empty() }
        } else {
            TypedAbilityKind::Concrete
        };

        let ability_id = self.abilities.next_id();
        let typed_ability = TypedAbility {
            name: parsed_ability.name,
            base_ability_id: ability_id,
            self_type_id,
            parameters: ability_params.to_slice(),
            functions: MSlice::empty(),
            scope_id: ability_scope_id,
            ast_id: parsed_ability.id,
            namespace_id,
            kind,
        };
        let ability_id = self.abilities.add(typed_ability);
        let added = self.scopes.add_ability(scope_id, parsed_ability.name, ability_id);
        if !added {
            kbail!(
                self,
                parsed_ability.span,
                "Ability with name {} already exists",
                parsed_ability.name
            );
        }
        self.add_ability_mapping(parsed_ability_id, ability_id);
        self.scopes.set_scope_owner_id(ability_scope_id, ScopeOwnerId::Ability(ability_id));

        let mut typed_functions: List<TypedAbilityFunctionRef, _> =
            self.mem.new_list(parsed_ability.functions.len());
        for (index, parsed_function_id) in
            self.ast.mem.getn(parsed_ability.functions).iter().enumerate()
        {
            let Some(function_id) = self.declare_function(
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
            // Also resolvable through the namespace scope, alongside extension members
            if !self.scopes.add_function(ns_scope_id, function_name, function_id) {
                kbail!(
                    self,
                    self.ast.get_function(*parsed_function_id).signature_span,
                    "Ability function name {} is taken by a namespace member",
                    function_name
                );
            }
            let ability_names =
                self.function_name_to_ability_names.entry(function_name).or_default();
            if !ability_names.as_slice(&self.mem).contains(&parsed_ability.name) {
                ability_names.push_grow(&mut self.mem, parsed_ability.name);
            }
            typed_functions.push(TypedAbilityFunctionRef {
                function_name,
                index: index as u32,
                ability_id,
                function_id,
            });
        }
        self.abilities.get_mut(ability_id).functions = typed_functions.to_slice();
        Ok(Some(ability_id))
    }

    fn name_resolves_to_ability(&self, scope_id: ScopeId, name: &QIdent) -> K1Result<bool> {
        if self.find_ability_namespaced(scope_id, name)?.is_some() {
            return Ok(true);
        }
        Ok(name.path.is_empty() && self.scopes.find_pending_ability(scope_id, name.name).is_some())
    }

    fn find_ability_or_declare(
        &mut self,
        ability_name: &QIdent,
        scope_id: ScopeId,
    ) -> K1Result<AbilityId> {
        let found_ability_id = self.find_ability_namespaced(scope_id, ability_name)?;
        found_ability_id.map(Ok).unwrap_or({
            match self.scopes.find_pending_ability(scope_id, ability_name.name) {
                None => Err(kerr!(
                    self,
                    ability_name.name_span,
                    "No ability '{}' is in scope",
                    ability_name.name
                )),
                Some((pending_ability, ability_scope)) => {
                    debug!(
                        "Recursing into pending ability {} from {}",
                        self.ident_str(ability_name.name),
                        self.ast.get_span_content(ability_name.name_span)
                    );
                    match self.compile_ability_definition(pending_ability, ability_scope)? {
                        Some(ability_id) => Ok(ability_id),
                        None => Err(kerr!(
                            self,
                            ability_name.name_span,
                            "No ability '{}' is in scope",
                            ability_name.name
                        )),
                    }
                }
            }
        })
    }

    fn declare_ability_impl(
        &mut self,
        parsed_id: ParsedAbilityImplId,
        scope_id: ScopeId,
    ) -> K1Result<Option<AbilityImplId>> {
        let parsed_ability_impl = self.ast.get_ability_impl(parsed_id).clone();
        let span = parsed_ability_impl.span;
        if !self.execute_static_condition(parsed_ability_impl.compile_condition, scope_id) {
            return Ok(None);
        }
        if let Some(owner_ns) = self.scopes.get_scope_owner(scope_id).as_namespace() {
            self.fail_if_reload_ns(owner_ns, span, "ability impls")?;
        }
        let ability_expr = self.ast.mem.get(parsed_ability_impl.ability_expr).clone();
        let parsed_impl_functions = parsed_ability_impl.functions;

        let impl_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::AbilityImpl, ScopeOwnerId::None);

        let mut blanket_type_params: List<TypeId, _> =
            self.mem.new_list(parsed_ability_impl.generic_impl_params.len());
        for blanket_impl_param in self.ast.mem.getn(parsed_ability_impl.generic_impl_params) {
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &self.ast.mem,
                    blanket_impl_param.constraints,
                ) {
                    Ok(Some(parsed_constraint)) => {
                        Some(self.eval_type_expr(parsed_constraint, impl_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => kbail!(self, blanket_impl_param.span, "{}", msg),
                };
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: blanket_impl_param.name,
                    static_constraint: maybe_static_constraint,
                    // Can't have any of these yet; we check this below for now
                    predicate_functions: MSlice::empty(),
                    scope_id: impl_scope_id,
                    span: blanket_impl_param.span,
                },
                // We create the variable with no ability constraints then add them later, so that its
                // constraints can reference itself
                // Example: impl[T] Add[Rhs = T] where T: Num
                //                            ^ constraint uses T, so Add[Rhs=T] can't exist as
                //                            a term until T exists as a type
                // The constraints need T to exist
                smallvec![],
            );
            if !self.scopes.add_type(impl_scope_id, blanket_impl_param.name, type_variable_id) {
                kbail!(
                    self,
                    blanket_impl_param.span,
                    "Duplicate blanket impl parameter name: {}",
                    blanket_impl_param.name
                );
            }

            if !blanket_impl_param.constraints.is_empty() {
                let param_constraints_scope_id = self.scopes.add_child_scope(
                    impl_scope_id,
                    ScopeType::AbilityImpl,
                    ScopeOwnerId::None,
                );
                for constraint in self.ast.mem.getn(blanket_impl_param.constraints) {
                    match constraint {
                        ParsedTypeConstraintExpr::Ability(ability_expr) => {
                            let constrained_ability_sig =
                                self.eval_ability_expr(*ability_expr, false, impl_scope_id)?;
                            let constraint_span = self.ast.mem.get(*ability_expr).span;
                            self.implement_ability_for_type_constraint(
                                type_variable_id,
                                constrained_ability_sig,
                                param_constraints_scope_id,
                                constraint_span,
                            );
                        }
                        ParsedTypeConstraintExpr::Predicate(qident) => {
                            kbail!(
                                self,
                                qident.name_span,
                                "Blanket implementation parameters cannot have type predicate functions yet"
                            );
                        }
                        ParsedTypeConstraintExpr::Static(_) => {}
                    }
                }
            }
            blanket_type_params.push(type_variable_id);
        }

        let impl_self_type = self.eval_type_expr(parsed_ability_impl.self_type, impl_scope_id)?;
        let ability_sig =
            self.eval_ability_expr(parsed_ability_impl.ability_expr, true, impl_scope_id)?;
        let ability_id = ability_sig.specialized_ability_id;

        // A blanket impl is applied by matching its Self type against a target
        // type, so its blanket-ness must come from the Self type: the only
        // type params that can occur in it are the impl's own. If none does,
        // the impl is really a family of impls for one concrete type
        if !blanket_type_params.is_empty()
            && self.get_type_variable_counts(impl_self_type).type_parameter_count == 0
        {
            kbail!(
                self,
                span,
                "A blanket impl's Self type must mention at least one of its parameters; this impl's Self type {} is concrete",
                self.type_id_to_string(impl_self_type).blue()
            );
        }

        // Uniqueness of implementation:
        // We allow only one implementation per Ability (+ unique params set)
        // Check for existing implementation
        let existing_impls = self.ability_impl_table_by_ability.get(&TypeAbilityPair {
            self_type_id: impl_self_type,
            base_ability_id: self.abilities.get(ability_id).base_ability_id,
        });
        if let Some(existing_impls) = existing_impls
            && existing_impls
                .as_slice(&self.mem)
                .iter()
                .any(|h| h.specialized_ability_id == ability_id)
        {
            kbail!(
                self,
                span,
                "Ability '{}' already implemented for type: {}",
                self.ident_str(self.abilities.get(ability_id).name).blue(),
                self.type_id_to_string(impl_self_type).blue()
            );
        }

        let ability = self.abilities.get(ability_id).clone();
        let ability_name = ability.name;
        let ability_self_type = ability.self_type_id;
        // Bind 'Self' = target_type
        // Discarded because we just made this scope
        let _ = self.scopes.add_type(impl_scope_id, self.ast.idents.b.self_, impl_self_type);
        let _ = self.scopes.add_type_substitution(impl_scope_id, ability_self_type, impl_self_type);

        // We also need to bind any ability parameters that this
        // ability is already specialized on; they aren't in our fresh scope.
        // Name binds are FE convenience only; if the impl declares a generic
        // param with the same name, it shadows, and the id-keyed substitution
        // below still binds the ability param
        let base_params = self.abilities.get(ability.base_ability_id).parameters;
        for (base_param, argument) in self
            .mem
            .getn(base_params)
            .iter()
            .filter(|p| p.is_ability_side_param())
            .zip(ability.kind.arguments(self))
        {
            let _ = self.scopes.add_type(impl_scope_id, base_param.name, *argument);
            let _ = self.scopes.add_type_substitution(
                impl_scope_id,
                base_param.type_variable_id,
                *argument,
            );
        }

        let mut impl_arguments: List<TypeId, _> = self.mem.new_list(ability.parameters.len());
        for impl_param in self.mem.getn(ability.parameters).iter().filter(|p| p.is_impl_param) {
            let Some(&matching_arg) = self
                .ast
                .mem
                .getn(ability_expr.arguments)
                .iter()
                .find(|arg| arg.name == Some(impl_param.name))
            else {
                kbail!(
                    self,
                    ability_expr.span,
                    "Missing implementation-side parameter for Ability {}: {}",
                    ability_name,
                    impl_param.name
                );
            };

            let Some(matching_arg_type_expr) = matching_arg.type_expr else {
                kbail!(self, matching_arg.span, "_ is supported here");
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
            let _ = self.scopes.add_type(impl_scope_id, impl_param.name, arg_type);
            let _ = self.scopes.add_type_substitution(
                impl_scope_id,
                impl_param.type_variable_id,
                arg_type,
            );
            impl_arguments.push(arg_type)
        }

        let impl_arguments_handle = impl_arguments.to_slice();
        let base_ability_id = ability.base_ability_id;
        let kind = if parsed_ability_impl.generic_impl_params.is_empty() {
            AbilityImplKind::Concrete
        } else {
            AbilityImplKind::Blanket { base_ability: base_ability_id, parsed_id }
        };

        // Report extra functions first
        for &parsed_fn in self.ast.mem.getn(parsed_impl_functions) {
            let parsed_fn_name = self.ast.get_function(parsed_fn).name;
            let Some(_ability_function_ref) =
                self.mem.getn(ability.functions).iter().find(|f| f.function_name == parsed_fn_name)
            else {
                kbail!(self, span, "Extra function in ability impl: {}", parsed_fn_name);
            };
        }

        let mut typed_functions = self.mem.new_list(ability.functions.len());
        for ability_function_ref in self.mem.getn(ability.functions) {
            let matching_impl_function =
                self.ast.mem.getn(parsed_impl_functions).iter().find_map(|&fn_id| {
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
                            kbail!(
                                self,
                                span,
                                "Missing implementation for function '{}' in ability '{}'",
                                self.ident_str(ability_function_ref.function_name).blue(),
                                self.ident_str(ability_name).blue()
                            );
                        }
                    }
                }
            };

            if is_default
                && matches!(kind, AbilityImplKind::Concrete)
                && self
                    .check_ability_fn_where_constraints(
                        ability_id,
                        impl_arguments_handle,
                        impl_self_type,
                        ability_function_ref.index,
                        impl_scope_id,
                        span,
                    )
                    .is_err()
            {
                typed_functions.push(AbilityImplFunction::Unavailable);
                continue;
            }

            let impl_function_id = self
                .declare_function(
                    parsed_impl_function_id,
                    impl_scope_id,
                    Some(FunctionAbilityContextInfo::ability_impl(
                        ability_id,
                        impl_self_type,
                        kind,
                        None,
                        is_default,
                    )),
                    // Why root namespace?! Answer: the namespace is only used for companion type stuff, so
                    // this isn't doing any harm
                    ROOT_NAMESPACE_ID,
                )?
                .expect("an ability impl cannot be conditionally compiled");

            let specialized_fun = self.get_function(impl_function_id);
            let specialized_fn_type = specialized_fun.type_id;
            let impl_fn_type_params = specialized_fun.type_params;

            let spec_fn_scope = specialized_fun.scope;

            let generic_function = self.get_function(ability_function_ref.function_id);
            let generic_type = generic_function.type_id;
            let generic_fn_type_params = generic_function.type_params;

            // We check that the signature of the provided impl function matches
            // the signature of the generic function with target_type substituted for Self,
            // and with the ability function's own type params (which follow the injected
            // Self) mapped positionally to the impl function's
            let mut sig_subst: SV4<TypeSubstitutionPair> =
                smallvec![spair! {ability_self_type => impl_self_type}];
            let self_ident = self.ast.idents.b.self_;
            for (gen_tp, impl_tp) in self
                .mem
                .getn(generic_fn_type_params)
                .iter()
                .filter(|tp| self.get_type_parameter(**tp).name != self_ident)
                .zip(self.mem.getn(impl_fn_type_params))
            {
                sig_subst.push(spair! {*gen_tp => *impl_tp});
            }
            let substituted_root_type = self.substitute_in_type(generic_type, &sig_subst);

            let impl_function_span = self.ast.get_function(parsed_impl_function_id).name_span;
            if let Err(msg) =
                self.check_types(substituted_root_type, specialized_fn_type, spec_fn_scope)
            {
                kbail!(
                    self,
                    impl_function_span,
                    "Invalid implementation of {} in ability {}: {msg}",
                    self.ast.idents.get_string(ability_function_ref.function_name),
                    self.ast.idents.get_string(ability_name)
                );
            }

            // Each implementation of an ability function is a reference to it
            self.emit_ls_entity(
                impl_function_span,
                LsEntityKind::Function {
                    function_id: ability_function_ref.function_id,
                    is_defn: false,
                },
            );

            typed_functions.push(AbilityImplFunction::FunctionId(impl_function_id));
        }

        let blanket_type_params_handle = blanket_type_params.to_slice();
        let typed_impl_id = self.add_ability_impl(TypedAbilityImpl {
            kind,
            blanket_type_params: blanket_type_params_handle,
            self_type_id: impl_self_type,
            ability_id,
            base_ability_id,
            impl_arguments: impl_arguments_handle,
            functions: typed_functions.to_slice(),
            scope_id: impl_scope_id,
            span,
            compile_errors: MList::empty(),
        });

        if kind.is_blanket() {
            self.blanket_impls
                .entry(base_ability_id)
                .or_default()
                .push_grow(&mut self.mem, typed_impl_id)
        }

        self.ability_impl_ast_mappings.insert(parsed_id, typed_impl_id);
        Ok(Some(typed_impl_id))
    }

    fn compile_ability_impl_bodies(
        &mut self,
        parsed_ability_impl_id: ParsedAbilityImplId,
        _scope_id: ScopeId,
    ) -> K1Result<()> {
        let Some(&ability_impl_id) = self.ability_impl_ast_mappings.get(&parsed_ability_impl_id)
        else {
            // Missing mapping means, likely, we failed to compile the signature
            // Just do nothing. TODO: flag when defns have failed compilation so we don't
            // mask real bugs
            return Ok(());
        };
        let ability_impl = *self.ability_impls.get(ability_impl_id);

        for (index, impl_fn) in self.mem.getn(ability_impl.functions).iter().enumerate() {
            let impl_fn = match *impl_fn {
                AbilityImplFunction::FunctionId(impl_fn) => impl_fn,
                AbilityImplFunction::Unavailable => continue,
                AbilityImplFunction::Abstract(_) => {
                    self.ice("Expected impl function id, not abstract, in eval_ability_impl", None);
                }
            };
            let decl_fn =
                *self.mem.get_nth(self.abilities.get(ability_impl.ability_id).functions, index);
            let is_default = self.get_function(impl_fn).parsed_id
                == self.get_function(decl_fn.function_id).parsed_id;
            if is_default
                && self
                    .check_ability_fn_where_constraints(
                        ability_impl.ability_id,
                        ability_impl.impl_arguments,
                        ability_impl.self_type_id,
                        index as u32,
                        ability_impl.scope_id,
                        ability_impl.span,
                    )
                    .is_err()
            {
                continue;
            }
            if let Err(e) = self.eval_function_body(impl_fn) {
                self.ability_impls
                    .get_mut(ability_impl_id)
                    .compile_errors
                    .push_grow(&mut self.mem, e);
                self.report(e);
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
        self.tmp.reset(false);
        match def {
            ParsedId::Use(_) => {
                // Uses are all resolved by now
            }
            ParsedId::Namespace(namespace) => {
                self.compile_ns_body(namespace, skip_defns);
            }
            ParsedId::Global(global_id) => {
                if let Err(e) = self.eval_global_body(global_id) {
                    self.report(e)
                };
            }
            ParsedId::Function(parsed_function_id) => {
                if let Some(function_declaration_id) =
                    self.function_ast_mappings.get(&parsed_function_id).copied()
                {
                    if let Err(e) = self.eval_function_body(function_declaration_id) {
                        self.report(e);
                    };
                }
            }
            ParsedId::Macro(parsed_macro_id) => {
                if let Some(function_declaration_id) =
                    self.macro_ast_mappings.get(&parsed_macro_id).copied()
                {
                    if let Err(e) = self.eval_function_body(function_declaration_id) {
                        self.report(e);
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
                    self.report(e);
                };
            }
            ParsedId::StaticDefn(static_expr_id) => {
                let ParsedExpr::Static(s) = self.ast.exprs.get(static_expr_id) else {
                    unreachable!()
                };
                let s = *s;
                let is_metaprogram = s.kind.is_metaprogram();
                // For value programs, we want to run them in the body phase
                // so that they have access to as much code as possible
                if !is_metaprogram {
                    let should_compile =
                        self.execute_static_condition(s.compile_condition, scope_id);

                    if should_compile {
                        let static_ctx = StaticExecContext { expected_return_type: None };
                        let eval_expr_ctx =
                            EvalExprContext::make(scope_id).with_static_ctx(Some(static_ctx));
                        if let Err(e) =
                            self.compile_static_or_meta(static_expr_id, s, true, eval_expr_ctx)
                        {
                            self.report(e);
                        };
                    }
                }
            }
            other_id => {
                panic!("Was asked to eval definition of a non-definition ast node {:?}", other_id)
            }
        }
    }

    #[must_use]
    fn eval_use_definition(
        &mut self,
        scope_id: ScopeId,
        parsed_use_id: ParsedUseId,
        fail_on_traverse_fail: bool,
    ) -> bool {
        let parsed_use = *self.ast.uses.get_use(parsed_use_id);
        let useable_symbols =
            match self.find_useable_symbols(scope_id, &parsed_use.target, fail_on_traverse_fail) {
                Err(e) => {
                    self.report(e);
                    return false;
                }
                Ok(sym) => sym,
            };
        debug!(
            "Usable symbols for {}: {:?}",
            self.qident_to_string(&parsed_use.target),
            &useable_symbols
        );
        if useable_symbols.is_empty() {
            debug!("Inserting unresolved use of {}", self.qident_to_string(&parsed_use.target));
            return false;
        }
        let provenance = if parsed_use.exposed { Provenance::UseExposed } else { Provenance::Use };
        for symbol in &useable_symbols {
            self.scopes.add_use_binding(
                scope_id,
                symbol,
                parsed_use.alias.unwrap_or(parsed_use.target.name),
                provenance,
            );
            debug!("Inserting resolved use of {:?}", symbol);
        }
        true
    }

    fn find_useable_symbols(
        &mut self,
        scope_id: ScopeId,
        name: &QIdent,
        fail_on_traverse_fail: bool,
    ) -> K1Result<SV4<UseableSymbol>> {
        let scope_id_to_search = match self.resolve_qident(scope_id, name) {
            Err(e) => {
                if fail_on_traverse_fail {
                    return Err(e);
                } else {
                    return Ok(smallvec![]);
                }
            }
            Ok(s) => s,
        };
        debug!(
            "Searching scope for useable symbol: {}, Functions:\n{:?}",
            self.scope_name_to_string(scope_id_to_search),
            self.scopes.iter_scope_functions(scope_id_to_search).collect::<Vec<_>>()
        );

        // TODO(MODULES): Validate modules cannot use something from a module they don't depend on
        // even if its in the program
        //
        // But that wouldn't really be a 'use' thing because you can use
        // stuff without 'use'ing it, so actually it needs to happen everywhere
        // we resolve namespaced identifiers :O

        let mut found_symbols: SV4<_> = smallvec![];
        if let Some(function_id) = self.scopes.find_function_exposed(scope_id_to_search, name.name)
        {
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Function(function_id),
            });
        }
        if let Some(type_id) = self.scopes.find_type_exposed(scope_id_to_search, name.name) {
            let companion_namespace = self.get_companion_namespace(type_id);
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type { type_id, companion_namespace },
            })
        } else if let Some(pending_parsed_id) =
            self.scopes.find_pending_type_local(scope_id_to_search, name.name)
        {
            let type_id = self.eval_type_defn(pending_parsed_id, scope_id_to_search)?;
            let companion_namespace = self.get_companion_namespace(type_id);
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type { type_id, companion_namespace },
            })
        }
        if let Some(variable_id) = self
            .scopes
            .find_variable_exposed(scope_id_to_search, name.name)
            .and_then(|vis| vis.variable_id())
        {
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Global(variable_id),
            })
        }
        if let Some(ability_id) = self.scopes.find_ability_exposed(scope_id_to_search, name.name) {
            let namespace_id = self.abilities.get(ability_id).namespace_id;
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Ability(ability_id, namespace_id),
            })
        }
        if let Some(ns_id) = self.scopes.find_namespace_exposed(scope_id_to_search, name.name) {
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Namespace(ns_id),
            })
        }
        Ok(found_symbols)
    }

    /// Registers a global, type, or ability as pending in its namespace's
    /// scope; other kinds are untouched. Runs during the in-order namespace
    /// declaration walk, so a defn's #if can force anything registered above
    /// it. A false condition means no registration: the name never exists,
    /// and an alternate defn of the same name doesn't collide.
    fn register_pending_defn(&mut self, defn: ParsedId, namespace_id: NamespaceId) {
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;
        match defn {
            ParsedId::Global(global_id) => {
                let parsed = self.ast.get_global(global_id);
                let name = parsed.name;
                let condition = parsed.compile_condition;
                if !self.execute_static_condition(condition, namespace_scope_id) {
                    return;
                }
                self.scopes.add_pending_global(namespace_scope_id, name, global_id);
            }
            ParsedId::TypeDefn(type_defn_id) => {
                let parsed_type_defn = self.ast.get_type_defn(type_defn_id).clone();
                if !self.execute_static_condition(
                    parsed_type_defn.compile_condition,
                    namespace_scope_id,
                ) {
                    return;
                }
                let pending_defn =
                    TypePendingDefinition { scope_id: namespace_scope_id, parsed_id: type_defn_id };
                let added = self.scopes.add_pending_type(
                    namespace_scope_id,
                    parsed_type_defn.name,
                    type_defn_id,
                );
                if !added {
                    self.report(kerr!(
                        self,
                        parsed_type_defn.span,
                        "Type {} exists",
                        parsed_type_defn.name
                    ));
                }
                self.types_pending_definition.push_back(pending_defn);
            }
            ParsedId::Ability(parsed_ability_id) => {
                let parsed_ability_defn = self.ast.get_ability(parsed_ability_id);
                let name = parsed_ability_defn.name;
                let span = parsed_ability_defn.span;
                let condition = parsed_ability_defn.compile_condition;
                if !self.execute_static_condition(condition, namespace_scope_id) {
                    return;
                }
                let added = self.scopes.add_pending_ability_defn(
                    namespace_scope_id,
                    name,
                    parsed_ability_id,
                );
                if !added {
                    self.report(kerr!(self, span, "Ability {} exists", name));
                }
            }
            _ => {}
        }
    }

    fn fail_if_reload_ns(&self, ns_id: NamespaceId, span: SpanId, what: &str) -> K1Result<()> {
        if self.namespaces.get(ns_id).reload {
            kbail!(self, span, "{} are not allowed in reloadable namespaces", what);
        }
        Ok(())
    }

    fn declare_namespace_definitions(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) {
        let Some(&namespace_id) = self.namespace_ast_mappings.get(&parsed_namespace_id) else {
            return;
        };
        let namespace = self.namespaces.get(namespace_id);
        let namespace_scope_id = namespace.scope_id;
        let ns_reload = namespace.reload;
        // Before the ns's own defns: a user fn named load/load-async then
        // collides with the synthesized one and fails like any duplicate name
        if ns_reload {
            if let Err(e) = self.get_or_declare_ns_reload_functions(namespace_id) {
                self.report(e);
            }
        }
        let parsed_namespace = self.ast.namespaces.get(parsed_namespace_id);
        for defn in parsed_namespace.definitions.as_slice(&self.ast.mem) {
            if skip_defns.contains(defn) {
                continue;
            }
            // Declarations write into permanent pools/mem; tmp is per-declaration scratch
            let tmp_mark = self.tmp.mark();
            match *defn {
                ParsedId::Use(_use_id) => {}
                ParsedId::Namespace(namespace_id) => {
                    self.declare_namespace_definitions(namespace_id, skip_defns)
                }
                ParsedId::Global(constant_id) => {
                    if let Err(e) = self.declare_global(constant_id, namespace_scope_id) {
                        self.report(e);
                    }
                }
                ParsedId::Function(parsed_function_id) => {
                    if let Err(e) = self.declare_function(
                        parsed_function_id,
                        namespace_scope_id,
                        None,
                        namespace_id,
                    ) {
                        self.report(e);
                    }
                }
                ParsedId::Macro(parsed_macro_id) => {
                    if let Err(e) = self.declare_macro(parsed_macro_id, namespace_scope_id) {
                        self.report(e);
                    }
                }
                ParsedId::TypeDefn(_type_defn_id) => {
                    // Handled by prior phase
                }
                ParsedId::Ability(parsed_ability_id) => {
                    if let Err(e) =
                        self.compile_ability_definition(parsed_ability_id, namespace_scope_id)
                    {
                        self.report(e)
                    };
                }
                ParsedId::AbilityImpl(ability_impl) => {
                    if let Err(e) = self.declare_ability_impl(ability_impl, namespace_scope_id) {
                        self.report(e)
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
            self.tmp.reset_to(tmp_mark);
        }
    }

    fn get_or_declare_ns_reload_functions(&mut self, namespace_id: NamespaceId) -> K1Result<()> {
        let ns = self.namespaces.get(namespace_id);
        let ns_scope = ns.scope_id;
        let load_ident = self.ast.idents.b.load;
        if self.scopes.find_function_local(ns_scope, load_ident).is_some() {
            return Ok(());
        }
        let ns_path_ident =
            self.build_ident_with(|k1, s| k1.write_scope_path(s, ns_scope, "/", true));
        let b = self.ast.idents.b;
        self.declare_ns_reload_fn(namespace_id, load_ident, b.load_ns, ns_path_ident)?;
        self.declare_ns_reload_fn(namespace_id, b.load_async, b.load_ns_async, ns_path_ident)?;
        self.declare_ns_reload_fn(namespace_id, b.loaded_version, b.ns_version, ns_path_ident)?;
        self.declare_ns_reload_fn(namespace_id, b.watch, b.watch_ns, ns_path_ident)
    }

    fn declare_ns_reload_fn(
        &mut self,
        namespace_id: NamespaceId,
        name: StringId,
        callee: StringId,
        ns_path_ident: StringId,
    ) -> K1Result<()> {
        let ns = self.namespaces.get(namespace_id);
        let ns_scope = ns.scope_id;
        let ns_parsed_id = ns.parsed_id.as_namespace_id().unwrap();
        let span = self.ast.namespaces.get(ns_parsed_id).name_span;

        let fn_scope =
            self.scopes.add_child_scope(ns_scope, ScopeType::FunctionScope, ScopeOwnerId::None);
        let function_id = self.functions.next_id();
        self.scopes.set_scope_owner_id(fn_scope, ScopeOwnerId::Function(function_id));

        let ns_path_arg = self.synth_string_literal(ns_path_ident, span);

        let b = self.ast.idents.b;
        let path = self
            .ast
            .mem
            .pushn(&[IdentSpanned::make_anon(b.std), IdentSpanned::make_anon(b.reload)]);
        let load_ns_name = QIdent { path, name: callee, name_span: span };

        let body = self
            .synth_typed_call_typed_args(
                load_ns_name,
                &[],
                &[ns_path_arg],
                EvalExprContext::make(fn_scope),
                false,
            )
            .map_err(|e| kerr!(self, span, "ns(reload) requires std/reload: {}", e.message))?;
        let return_type = self.exprs.get_type(body);
        let body_block = self.synth_return_only_block(fn_scope, body, span);
        let function_type = self.add_type_anon(Type::Function(FunctionType {
            physical_params: MSlice::empty(),
            return_type,
            is_lambda: false,
        }));
        let actual_function_id = self.add_function(TypedFunction {
            name,
            scope: fn_scope,
            namespace_id,
            params: MSlice::empty(),
            type_params: MSlice::empty(),
            fnlike_type_params: MSlice::empty(),
            ability_where_constraints: MSlice::empty(),
            body_block: Some(body_block),
            builtin_type: None,
            linkage: Linkage::Standard,
            specialization_info: None,
            parsed_id: ParsedId::Namespace(ns_parsed_id),
            type_id: function_type,
            kind: TypedFunctionKind::Standard,
            flags: TypedFunctionFlags::empty(),
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        });
        debug_assert_eq!(actual_function_id, function_id);
        let added = self.scopes.add_function(ns_scope, name, function_id);
        debug_assert!(added, "the name was free in the ns scope; we just checked");
        Ok(())
    }

    pub fn reload_hash_for_ns(&self, ns_id: NamespaceId) -> u64 {
        let mut sum = 0u128;
        let mut signature = String::with_capacity(256);
        let mut listing = String::new();
        for (_, function) in self.function_iter() {
            if function.is_reloadable() && function.namespace_id == ns_id {
                let name = self.ident_str(function.name);
                signature.clear();
                self.display_type_id(
                    &mut signature,
                    function.type_id,
                    dump::TypeDisplayMode::Structural,
                )
                .unwrap();
                sum = sum.wrapping_add(
                    crate::snap::InputsHash(0).add(&[name.as_bytes(), signature.as_bytes()]).0,
                );
                if self.config.chatty {
                    writeln!(listing, "  {name}: {signature}").unwrap();
                }
            }
        }
        for global_id in self.globals.iter_ids() {
            let global = self.globals.get(global_id);
            if global.reload_ns == Some(ns_id) {
                let name = self.ident_str(self.variables.get(global.variable_id).name);
                signature.clear();
                self.display_type_id(
                    &mut signature,
                    global.type_id,
                    dump::TypeDisplayMode::Structural,
                )
                .unwrap();
                sum = sum.wrapping_add(
                    crate::snap::InputsHash(0)
                        .add(&[b"let", name.as_bytes(), signature.as_bytes()])
                        .0,
                );
                if self.config.chatty {
                    writeln!(listing, "  let {name}: {signature}").unwrap();
                }
            }
        }
        let hash = crate::snap::InputsHash(sum).add(&[crate::BUILD_ID.as_bytes()]).0 as u64;
        if self.config.chatty {
            let ns_name = self.ident_str(self.namespaces.get(ns_id).name);
            eprint!("reload api of {ns_name} ({hash:016x}):\n{listing}");
        }
        hash
    }

    fn compile_ns_body(&mut self, ast_namespace_id: ParsedNamespaceId, skip_defns: &[ParsedId]) {
        let ast_namespace = self.ast.namespaces.get(ast_namespace_id);
        let ast_definitions = &ast_namespace.definitions;
        let Some(&namespace_id) = self.namespace_ast_mappings.get(&ast_namespace.id) else {
            return;
        };
        let ns_scope_id = self.namespaces.get(namespace_id).scope_id;
        for defn in ast_definitions.as_slice(&self.ast.mem) {
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
    ) -> K1Result<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let name = ast_namespace.name;

        let ns_scope_id =
            self.scopes.add_child_scope(parent_scope_id, ScopeType::Namespace, ScopeOwnerId::None);
        let parent_ns_id = self
            .scopes
            .get_scope_owner(parent_scope_id)
            .as_namespace()
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
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.array;
        if is_array {
            self.scopes.array_scope_id = ns_scope_id;
        }
        let is_vector =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.vector;
        if is_vector {
            self.scopes.vector_scope_id = ns_scope_id;
        }

        let namespace_type = if ast_namespace.is_type_companion {
            NamespaceKind::TypeCompanion
        } else {
            NamespaceKind::User
        };
        let namespace = Namespace {
            name,
            scope_id: ns_scope_id,
            namespace_type,
            companion_type_id: None,
            parent_id: Some(parent_ns_id),
            owner_module: Some(self.module_in_progress.unwrap()),
            parsed_id: ParsedId::Namespace(parsed_namespace_id),
            lib_name: ast_namespace.lib_name,
            reload: ast_namespace.reload,
        };
        let namespace_id = self.namespaces.add(namespace);
        self.scopes.set_scope_owner_id(ns_scope_id, ScopeOwnerId::Namespace(namespace_id));

        if !self.scopes.add_namespace(parent_scope_id, name, namespace_id) {
            let occupant = self.scopes.find_namespace_local(parent_scope_id, name).unwrap();
            if self.namespaces.get(occupant).parent_id == Some(parent_ns_id) {
                kbail!(
                    self,
                    ast_namespace.span,
                    "Namespace name {} is taken",
                    self.ident_str(name).blue()
                );
            }
            self.scopes.replace_namespace(parent_scope_id, name, namespace_id);
        }

        self.namespace_ast_mappings.insert(parsed_namespace_id, namespace_id);
        Ok(namespace_id)
    }

    fn declare_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: ScopeId,
    ) -> K1Result<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let name_span = ast_namespace.name_span;
        let parent_ns = self
            .scopes
            .get_scope_owner(parent_scope)
            .as_namespace()
            .expect("namespace must be defined directly inside another namespace");
        self.fail_if_reload_ns(parent_ns, ast_namespace.span, "nested namespaces")?;

        // Extension is a namespace-tree question, not a name-resolution one: the
        // scope map also holds use-aliases (e.g. the core prelude), which a local
        // declaration shadows via create_namespace instead
        let namespace_id = if let Some(existing) =
            self.namespaces.find_child_by_name(parent_ns, ast_namespace.name)
        {
            // Map this separate namespace AST node to the same semantic namespace
            self.namespace_ast_mappings.insert(parsed_namespace_id, existing);
            debug!("Inserting re-definition node for ns {}", self.ident_str(ast_namespace.name));
            if ast_namespace.reload && !self.namespaces.get(existing).reload {
                let existing_scope = self.namespaces.get(existing).scope_id;
                if self.scopes.iter_scope_namespaces(existing_scope).next().is_some() {
                    kbail!(
                        self,
                        name_span,
                        "nested namespaces are not allowed in reloadable namespaces",
                    );
                }
                self.namespaces.get_mut(existing).reload = true;
            }
            if let Some(lib_name) = ast_namespace.lib_name {
                let existing_ns = self.namespaces.get_mut(existing);
                match existing_ns.lib_name {
                    None => existing_ns.lib_name = Some(lib_name),
                    Some(existing_lib) if existing_lib != lib_name => {
                        kbail!(
                            self,
                            name_span,
                            "Namespace {} already declares lib \"{}\"; conflicting lib \"{}\"",
                            self.ident_str(ast_namespace.name).blue(),
                            self.ident_str(existing_lib),
                            self.ident_str(lib_name)
                        );
                    }
                    Some(_) => {}
                }
            }
            existing
        } else {
            self.create_namespace(parsed_namespace_id, parent_scope)?
        };

        self.emit_ls_entity(name_span, LsEntityKind::Namespace(namespace_id));

        Ok(namespace_id)
    }

    fn discover_uses_in_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
        recurse: bool,
        skip_self: bool,
    ) {
        let Some(namespace_id) = self.namespace_ast_mappings.get(&parsed_namespace_id).copied()
        else {
            // If we haven't even declared namespaces yet
            return;
        };
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let ast_definitions = ast_namespace.definitions.as_slice(&self.ast.mem);
        for defn in ast_definitions {
            if skip_defns.contains(defn) {
                continue;
            }
            match *defn {
                ParsedId::Use(parsed_use_id) => {
                    if !skip_self {
                        debug!(
                            "discovering use {}",
                            self.qident_to_string(&self.ast.uses.get_use(parsed_use_id).target)
                        );
                        self.uses_pending_resolution.push_back(UsePendingResolution {
                            namespace_id,
                            scope_id: namespace_scope_id,
                            use_id: parsed_use_id,
                        })
                    }
                }
                ParsedId::Namespace(ns) => {
                    if recurse {
                        self.discover_uses_in_namespace(ns, skip_defns, true, false);
                    }
                }
                _ => {}
            }
        }
    }

    fn declare_namespaces_in_namespace(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        skip_defns: &[ParsedId],
    ) {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let ast_definitions = ast_namespace.definitions.as_slice(&self.ast.mem);

        let namespace_id = *self.namespace_ast_mappings.get(&parsed_namespace_id).unwrap();
        let namespace_scope_id = self.namespaces.get(namespace_id).scope_id;

        // new_defns will contain all of the namespace's original parsed definitions
        // as well as any new ones generated via metaprogramming
        let mut new_defns = self.ast.mem.new_list(ast_definitions.len() as u32);

        for defn in ast_definitions.iter() {
            new_defns.push_grow(&mut self.ast.mem, *defn);
            if skip_defns.contains(defn) {
                continue;
            }
            match *defn {
                ParsedId::Use(_) => {}
                ParsedId::Global(_) | ParsedId::TypeDefn(_) | ParsedId::Ability(_) => {
                    self.register_pending_defn(*defn, namespace_id);
                }
                ParsedId::Namespace(namespace_id) => {
                    if let Err(e) = self.declare_namespace_recursive(
                        namespace_id,
                        namespace_scope_id,
                        skip_defns,
                    ) {
                        self.report(e)
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
                    let should_compile =
                        self.execute_static_condition(s.compile_condition, namespace_scope_id);

                    if !should_compile {
                        continue;
                    }

                    // Metaprogram top-level evaluation
                    // We simply run the program, getting a string,
                    // parse it as definitions, then we load those definitions
                    // injecting them into the AST, as if they appeared right here.
                    // If it is a namespace, we ensure we handle it right now, as this is the phase that
                    // namespaces should be declared. Everything else will get handled naturally
                    // as we iterate over the namespace's definitions in the future phases
                    let static_ctx = StaticExecContext { expected_return_type: Some(I32_TYPE_ID) };
                    let eval_expr_ctx =
                        EvalExprContext::make(namespace_scope_id).with_static_ctx(Some(static_ctx));
                    let newly_parsed_defns =
                        match self.compile_static_or_meta(static_expr_id, s, true, eval_expr_ctx) {
                            Err(e) => {
                                self.report(e);
                                MSlice::empty()
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
                    for &d in self.ast.mem.getn(newly_parsed_defns) {
                        match d {
                            ParsedId::Namespace(ns) => {
                                if let Err(e) = self.declare_namespace_recursive(
                                    ns,
                                    namespace_scope_id,
                                    skip_defns,
                                ) {
                                    self.report(e)
                                };
                            }
                            ParsedId::Global(_) | ParsedId::TypeDefn(_) | ParsedId::Ability(_) => {
                                self.register_pending_defn(d, namespace_id);
                            }
                            _ => {}
                        }
                    }
                    // We want to insert a given #meta's definitions right after that meta
                    let new_defns_slice = self.ast.mem.getn(newly_parsed_defns);
                    new_defns.extend_grow(&mut self.ast.mem, new_defns_slice);
                }
                _ => {}
            }
        }

        let ast_namespace = self.ast.namespaces.get_mut(parsed_namespace_id);
        ast_namespace.definitions = new_defns.to_mlist();
    }

    fn declare_namespace_recursive(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        parent_scope: ScopeId,
        skip_defns: &[ParsedId],
    ) -> K1Result<Option<NamespaceId>> {
        let condition = self.ast.namespaces.get(parsed_namespace_id).compile_condition;
        if !self.execute_static_condition(condition, parent_scope) {
            return Ok(None);
        }
        let ns_id = self.declare_namespace(parsed_namespace_id, parent_scope)?;
        self.declare_namespaces_in_namespace(parsed_namespace_id, skip_defns);
        Ok(Some(ns_id))
    }

    pub fn typecheck_module(
        &mut self,
        module_id: ModuleId,
        module_root_parsed_namespace: ParsedNamespaceId,
        build_ns_defn: Option<ParsedId>,
    ) -> anyhow::Result<()> {
        self.module_in_progress = Some(module_id);
        let is_core = module_id == MODULE_ID_CORE;
        // The namespace itself was declared at load, so that `ns build` had a parent
        let module_root_namespace_scope_id = self.modules.get(module_id).namespace_scope_id;

        // Meta phase: Find pre namespace, if exists, and fully compile it
        let mut pre_ns_id: Option<ParsedId> = None;
        let parsed_ns = self.ast.namespaces.get(module_root_parsed_namespace);
        if !is_core {
            let mut pre_ns_parsed_id = None;
            for defn in parsed_ns.definitions.as_slice(&self.ast.mem) {
                let Some(ns_id) = defn.as_namespace_id() else { continue };
                if self.ast.namespaces.get(ns_id).name == self.ast.idents.b.pre {
                    pre_ns_parsed_id = Some(ns_id);
                    break;
                }
            }
            if let Some(pre_ns_parsed_id) = pre_ns_parsed_id {
                debug!(">> Phase 0.5 compile pre namespace");
                self.declare_namespace(pre_ns_parsed_id, module_root_namespace_scope_id)
                    .map_err(|e| self.message_to_anyhow(e))?;
                self.run_all_phases_on_ns(pre_ns_parsed_id, module_id, &[])?;
                pre_ns_id = Some(ParsedId::Namespace(pre_ns_parsed_id));
            }
        }

        let skip_defns = match (pre_ns_id, build_ns_defn) {
            (None, None) => &[][..],
            (None, Some(id)) => &[id],
            (Some(id), None) => &[id],
            (Some(id1), Some(id2)) => &[id1, id2],
        };

        self.run_all_phases_on_ns(module_root_parsed_namespace, module_id, skip_defns)?;

        if is_core {
            // Some of these will be redundant, but this lets us use the core prelude from
            // module manifests, and 'pre' modules
            self.add_core_uses_to_scope(self.scopes.root_scope_id(), SpanId::NONE)
                .map_err(|e| self.message_to_anyhow(e))?;
        }

        self.module_in_progress = None;

        Ok(())
    }

    /// Sweeps the pending queue to quiescence, so a use whose target is another
    /// use resolves regardless of declaration order; textual order only decides
    /// the winner when two uses claim the same name.
    fn resolve_pending_uses(&mut self) {
        loop {
            let before = self.uses_pending_resolution.len();
            let mut i = 0;
            while let Some(use_pending) = self.uses_pending_resolution.get(i) {
                if self.eval_use_definition(use_pending.scope_id, use_pending.use_id, false) {
                    self.uses_pending_resolution.remove(i);
                } else {
                    i += 1;
                }
            }
            if self.uses_pending_resolution.len() == before {
                return;
            }
        }
    }

    fn run_all_phases_on_ns(
        &mut self,
        module_root_parsed_namespace: ParsedNamespaceId,
        module_id: ModuleId,
        skip_defns: &[ParsedId],
    ) -> anyhow::Result<()> {
        let is_core = module_id == MODULE_ID_CORE;
        macro_rules! check_for_errors {
            ($msg:expr) => {
                match self.error_count(&[MessageLevel::Error]) {
                    n if n > 0 => {
                        bail!("Module {} failed {} with {} errors", self.program_name(), $msg, n)
                    }
                    _ => {}
                }
            };
        }

        debug!(">> Pass 0 discover and resolve uses");
        self.discover_uses_in_namespace(module_root_parsed_namespace, skip_defns, false, false);
        self.resolve_pending_uses();

        debug!(">> Pass 1 declare namespaces and run global #meta programs");
        self.declare_namespaces_in_namespace(module_root_parsed_namespace, skip_defns);
        check_for_errors!("namespace declaration");

        // Pending Type declaration phase
        debug!(">> Pass 2 declare types");
        self.discover_uses_in_namespace(module_root_parsed_namespace, skip_defns, true, true);

        // If we resolve uses this early in core, we evaluate the builtin types out of the expected order
        if !is_core {
            self.resolve_pending_uses();
        }
        // check_for_errors!("type declaration");
        while let Some(tpd) = self.types_pending_definition.front() {
            debug!(
                "types_pending_definition {}\n{}",
                self.types_pending_definition.len(),
                self.types_pending_definition
                    .iter()
                    .map(|tpd| self.ident_str(self.ast.type_defns.get(tpd.parsed_id).name))
                    .join(", ")
            );
            let tmp_mark = self.tmp.mark();
            let result = self.eval_type_defn(tpd.parsed_id, tpd.scope_id);
            self.tmp.reset_to(tmp_mark);
            if let Err(err) = result {
                self.type_defn_context.reset();
                self.types_pending_definition.pop_front();
                self.report(err);
            }
        }

        check_for_errors!("types");

        debug_assert_eq!(self.types.len(), self.type_variable_counts.len());

        let mut companion_errors: Vec<K1Message> = vec![];
        for ns in self.namespaces.iter() {
            if ns.namespace_type == NamespaceKind::TypeCompanion && ns.companion_type_id.is_none() {
                let span = self.ast.get_span_for_id(ns.parsed_id);
                companion_errors.push(kerr!(
                    self,
                    span,
                    "Unresolved companion namespace; we never found type {}",
                    ns.name
                ));
            }
        }
        for e in companion_errors {
            self.report(e)
        }

        if is_core {
            let fields = self.mem.pushn(&[
                StructTypeField {
                    name: self.ast.idents.b.fn_ptr,
                    type_id: POINTER_TYPE_ID,
                    span: SpanId::NONE,
                },
                StructTypeField {
                    name: self.ast.idents.b.env_ptr,
                    type_id: POINTER_TYPE_ID,
                    span: SpanId::NONE,
                },
            ]);
            let t = self.add_type_anon(Type::Struct(StructType::struc(fields)));
            self.builtin_types.dyn_lambda_obj = Some(t);
            self.assert_builtin_types_correct();
        }

        self.resolve_pending_uses();

        // Everything else declaration phase
        debug!(">> Pass 4 declare rest of definitions (functions, globals)");
        self.declare_namespace_definitions(module_root_parsed_namespace, skip_defns);
        check_for_errors!("general declaration");
        if self.global_id_k1_arena.is_none() {
            panic!("global_id_k1_arena was not set");
        }

        self.resolve_pending_uses();
        let mut unresolved_use_errors: Vec<K1Message> = vec![];
        for pending_use in self.uses_pending_resolution.iter() {
            let parsed_use = self.ast.uses.get_use(pending_use.use_id);
            unresolved_use_errors.push(kerr!(
                self,
                parsed_use.span,
                "Unresolved use of {}",
                parsed_use.target.name
            ));
        }
        for e in unresolved_use_errors {
            self.report(e)
        }

        debug_assert!(self.abilities.get(ABILITY_ID_EQUALS).name == self.ast.idents.b.equals);
        debug_assert!(self.abilities.get(ABILITY_ID_BITWISE).name == self.ast.idents.b.bitwise);
        debug_assert!(
            self.abilities.get(ABILITY_ID_COMPARABLE).name == self.ast.idents.b.comparable
        );

        debug!(">> Pass 5 bodies (functions, globals, abilities)");
        self.compile_ns_body(module_root_parsed_namespace, skip_defns);

        check_for_errors!("typechecking");

        debug!(">> Pass 6 specialize function bodies");
        self.specialize_pending_function_bodies()?;
        check_for_errors!("body specialization");

        Ok(())
    }

    pub fn error_count(&self, kinds: &[MessageLevel]) -> usize {
        self.messages.borrow().iter().filter(|e| kinds.contains(&e.level)).count()
    }

    fn assert_builtin_types_correct(&self) {
        self.builtin_types.assert_complete();
        {
            let buffer_generic = self.types.get(self.builtin_types.buffer()).expect_generic();
            let buffer_struct = self.types.get(buffer_generic.inner).expect_struct();
            // debug_assert_eq!(
            //     self.get_layout(BUFFER_TYPE_ID),
            //     Layout::from_rust_type::<vm::k1_types::K1BufferLike>()
            // );
            debug_assert!(buffer_struct.fields.len() == 2);
            debug_assert!(
                self.mem
                    .getn(buffer_struct.fields)
                    .iter()
                    .map(|f| self.ident_str(f.name))
                    .collect::<SV2<_>>()[..]
                    == ["data", "len"]
            );
        }

        {
            let list_generic = self.types.get(self.builtin_types.list()).expect_generic();
            let info = self.get_defn_info(self.builtin_types.list()).unwrap();
            let list_struct = self.types.get(list_generic.inner).expect_struct();
            debug_assert!(info.name == self.ast.idents.b.list);
            debug_assert!(
                self.mem
                    .getn(list_struct.fields)
                    .iter()
                    .map(|f| self.ident_str(f.name))
                    .collect::<SV2<_>>()[..]
                    == ["buffer", "len"]
            );
        }

        {
            let string_struct = self.types.get(self.builtin_types.string()).expect_struct();
            let info = self.get_defn_info(self.builtin_types.string()).unwrap();
            debug_assert!(info.name == self.ast.idents.b.string);
            debug_assert!(string_struct.fields.len() == 1);
        }

        {
            let optional_generic = self.types.get(self.builtin_types.opt()).expect_generic();
            let info = self.get_defn_info(self.builtin_types.opt()).unwrap();
            let inner = self.types.get(optional_generic.inner);
            debug_assert!(info.name == self.ast.idents.b.opt);
            let variants = self.mem.getn(inner.as_sum().unwrap().variants);
            debug_assert_eq!(variants.len(), 2);
            debug_assert_eq!(variants[OPT_NONE_VARIANT_INDEX].name, self.ast.idents.b.none);
            debug_assert_eq!(variants[OPT_SOME_VARIANT_INDEX].name, self.ast.idents.b.some);
        }
        {
            let ordering_enum = self.types.get(self.builtin_types.ordering.unwrap()).expect_enum();
            let info = self.get_defn_info(self.builtin_types.ordering.unwrap()).unwrap();
            debug_assert!(ordering_enum.member_values.len() == 3);
            debug_assert!(info.name == self.ast.idents.b.ordering);
        }
    }

    fn specialize_pending_function_bodies(&mut self) -> anyhow::Result<()> {
        let mut function_ids: Vec<FunctionId> =
            Vec::with_capacity(self.functions_pending_body_specialization.len());
        while !self.functions_pending_body_specialization.is_empty() {
            function_ids.extend(&self.functions_pending_body_specialization);
            self.functions_pending_body_specialization.clear();
            for function_id in &function_ids {
                if let Err(e) = self.specialize_function_body(*function_id) {
                    self.report(e)
                }
            }
            function_ids.clear()
        }
        Ok(())
    }

    /////////////////////////// Type access apis

    /// Based on the 'size_type' of an array, determine what its concrete count is
    /// Size type can be a type parameter, in which case its 0
    /// But it can also be a known static size (i64), in which case its the value of it
    pub fn get_concrete_count_of_array(&self, size_type: TypeId) -> Option<i64> {
        self.get_type_as_i64(size_type)
    }

    pub fn get_value_from_value_type(&self, type_id: TypeId) -> Option<StaticValueId> {
        match self.types.get(type_id) {
            Type::StaticValue(StaticValueType { value_id: Some(value_id), .. }) => Some(*value_id),
            _ => None,
        }
    }

    /// Works for sum variants too
    pub fn get_struct_layout(&mut self, struct_type_id: TypeId) -> &'static [StructField] {
        self.get_physical_type(struct_type_id);
        self.get_struct_layout_computed(struct_type_id)
    }

    pub fn get_struct_layout_computed(&self, struct_type_id: TypeId) -> &'static [StructField] {
        let struct_pt = self.get_physical_type_computed(struct_type_id).unwrap();
        if struct_pt.is_empty() {
            &[]
        } else {
            self.mem.getn(self.get_agg_struct_layout(struct_pt.expect_agg()))
        }
    }

    pub fn get_static_value_type(&self, id: StaticValueId) -> TypeId {
        match self.static_values.get(id) {
            StaticValue::Empty(type_id) => *type_id,
            StaticValue::Bool(_) => self.builtin_types.bool(),
            StaticValue::Char(_) => self.builtin_types.char(),
            StaticValue::Int(typed_integer_value) => typed_integer_value.get_type(),
            StaticValue::Enum(type_id, _) => *type_id,
            StaticValue::Float(typed_float_value) => typed_float_value.get_type(),
            StaticValue::String(_) => self.builtin_types.string(),
            StaticValue::Zero(type_id) => *type_id,
            StaticValue::Struct(s) => s.type_id,
            StaticValue::Sum(e) => e.sum_type_id,
            StaticValue::LinearContainer(v) => v.type_id,
            StaticValue::RawContainer(r) => r.type_id,
        }
    }

    fn add_core_uses_to_scope(&mut self, scope: ScopeId, span: SpanId) -> K1Result<()> {
        macro_rules! intern_path {
            ($($name: expr),*) => {
                self.ast.mem.pushn(&[$(parse::IdentSpanned { name: $name, span }),*])
            }
        }

        let root_ns = intern_path!(self.ast.idents.b.root_module_name);
        let core_ns = intern_path!(self.ast.idents.b.core);
        let core_mem = intern_path!(self.ast.idents.b.core, self.ast.idents.b.mem);
        let core_types = intern_path!(self.ast.idents.b.core, self.ast.idents.b.types);
        let core_scalarcmp = intern_path!(self.ast.idents.b.core, self.ast.idents.b.scalar_cmp);

        macro_rules! core {
            ($name: expr) => {
                QIdent { path: core_ns, name: get_ident!(self, $name), name_span: span }
            };
        }

        let idents_to_use = [
            QIdent { path: root_ns, name: self.ast.idents.b.core, name_span: span }, // use _root/core;
            QIdent { path: root_ns, name: self.ast.idents.b.std, name_span: span }, // use _root/std;
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
            core!("char"),
            core!("bool"),
            core!("never"),
            core!("empty"),
            core!("ptr"),
            core!("f32"),
            core!("f64"),
            core!("buffer"),
            core!("span"),
            core!("array"),
            core!("list"),
            core!("string"),
            core!("opt"),
            core!("some"),
            core!("none"),
            core!("ordering"),
            core!("result"),
            core!("equals"),
            core!("writer"),
            core!("print"),
            core!("show"),
            core!("bitwise"),
            core!("comparable"),
            core!("try"),
            core!("from-string"),
            core!("iterator"),
            core!("iterable"),
            core!("as-buffer"),
            core!("as-span"),
            core!("index"),
            core!("println"),
            core!("print"),
            core!("eprint"),
            core!("eprintln"),
            core!("identity"),
            core!("assert"),
            core!("assert-equals"),
            core!("assert-msg"),
            core!("crash"),
            core!("meta"),
            core!("mem"),
            core!("allocator"),
            core!("heap"),
            core!("atomic"),
            core!("vector"),
            core!("types"),
            core!("k1"),
            core!("range"),
            core!("rangeable"),
            core!("add"),
            core!("zero"),
            core!("sub"),
            core!("mul"),
            core!("div"),
            core!("rem"),
            core!("neg"),
            core!("sys"),
            core!("files"),
            core!("scalar-cmp"),
            core!("string-builder"),
            core!("code"),
            core!("code-builder"),
            core!("optref"),

            QIdent { path: core_scalarcmp, name: get_ident!(self, "min"), name_span: span },
            QIdent { path: core_scalarcmp, name: get_ident!(self, "max"), name_span: span },

            QIdent { path: core_mem, name: get_ident!(self, "zeroed"), name_span: span },
            QIdent { path: core_types, name: self.ast.idents.b.enum_, name_span: span },
            QIdent { path: core_types, name: get_ident!(self, "sum"), name_span: span },
            QIdent { path: core_types, name: get_ident!(self, "type-id"), name_span: span },
        ];
        for qid in idents_to_use.into_iter() {
            let use_id = self.ast.uses.add_use(parse::ParsedUse {
                target: qid,
                alias: None,
                exposed: false,
                span,
            });
            if !self.eval_use_definition(scope, use_id, true) {
                //We can't quite fail here since we use 'std' from 'core', and it gets
                //resolved later
                //kbail!(self,
                //    qid.span,
                //    "Failed to resolve a core use: {}",
                //    self.qident_to_string(&qid)
                //);
            }
        }
        Ok(())
    }

    fn emit_ls_entity(&self, span: SpanId, kind: LsEntityKind) {
        if cfg!(feature = "lsp") {
            let span = self.ast.spans.get(span);
            let mut ls_entities = self.ls_entities.borrow_mut();
            let file_id = span.file_id;
            let entities_entry = ls_entities.entry(file_id);
            let entities = entities_entry.or_insert_with(|| Vec::with_capacity(128));
            entities.push(LsEntity { kind, span });
        }
    }

    fn positional_param_name(&mut self, index: usize) -> StringId {
        match index {
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
        }
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
