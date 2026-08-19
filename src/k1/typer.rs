// Copyright (c) 2026 knix
// All rights reserved.

pub(crate) mod derive;
pub(crate) mod dump;
pub(crate) mod infer;
pub(crate) mod megarepl;
pub(crate) mod scopes;
pub(crate) mod snapshot;
pub(crate) mod static_value;
pub(crate) mod synth;
pub(crate) mod typed_int_value;
pub(crate) mod types;
pub(crate) mod visit;

use crate::ir::{AtomicOrderingIr, BackendBuiltin, IrUnitId};
use crate::typer::megarepl::MegareplState;
use crate::{bc, clock, compiler, debug, ir, k1_format_user, kbail, kerr, kwarn, vm};
use bitflags::bitflags;
use itertools::Itertools;
pub use static_value::{
    StaticContainer, StaticContainerKind, StaticStruct, StaticSum, StaticValue, StaticValueId,
    StaticValuePool,
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
    ParsedAbilityId, ParsedAbilityImplId, ParsedBlock, ParsedBlockKind, ParsedCall, ParsedCallArg,
    ParsedExpr, ParsedExprId, ParsedFnParamType, ParsedFunctionId, ParsedGlobalId, ParsedId,
    ParsedIfExpr, ParsedListLiteral, ParsedLiteral, ParsedLoopExpr, ParsedNamespaceId,
    ParsedPattern, ParsedPatternId, ParsedProgram, ParsedStaticBlockKind, ParsedStaticExpr,
    ParsedStmt, ParsedStmtId, ParsedTypeConstraint, ParsedTypeConstraintExpr, ParsedTypeDefnId,
    ParsedTypeExpr, ParsedTypeExprId, ParsedTypeParam, ParsedUnaryOpKind, ParsedUseId,
    ParsedVariable, ParsedVariant, ParsedWhileExpr, QIdent, StringId, StructValueField,
    StructValueFieldKind,
};
use crate::vpool::VPool;
use crate::{SV4, SV8, impl_copy_if_small, nz_u32_id, static_assert_size};

#[cfg(test)]
mod layout_test;

nz_u32_id!(FunctionId);
nz_u32_id!(VariableId);

nz_u32_id!(NamespaceId);
pub const ROOT_NAMESPACE_ID: NamespaceId = NamespaceId(NonZeroU32::new(1).unwrap());

/// A/B switch for `for` lowering: library `for-each` macro vs the native typed
/// desugar. Build with `--features native-for` for the native lowering
pub const FOR_VIA_MACRO: bool = !cfg!(feature = "native-for");

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
pub const ABILITY_ID_SCALAR_CMP: AbilityId = AbilityId(NonZeroU32::new(13).unwrap());
pub const ABILITY_ID_COMPARABLE: AbilityId = AbilityId(NonZeroU32::new(14).unwrap());
pub const ABILITY_ID_TRY: AbilityId = AbilityId(NonZeroU32::new(15).unwrap());
pub const ABILITY_ID_ITERATOR: AbilityId = AbilityId(NonZeroU32::new(16).unwrap());
pub const ABILITY_ID_ITERABLE: AbilityId = AbilityId(NonZeroU32::new(17).unwrap());
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
}

#[derive(Clone, Copy)]
pub struct TypeDefnStackEntry {
    pub parsed_id: ParsedTypeDefnId,
    pub reserved_type_id: TypeId,
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
    fn with_manifest_eval(&self) -> EvalExprContext {
        EvalExprContext { flags: self.flags | EvalExprFlags::ManifestEval, ..*self }
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

#[derive(Debug, Clone, Copy)]
enum MaybeTypedExpr {
    Parsed(ParsedExprId),
    Typed(TypedExprId),
}

#[derive(Debug, Clone, Copy)]
enum MacroArg {
    Parsed(ParsedCallArg),
    Typed(TypedExprId),
}

#[derive(Debug, Clone, Copy)]
enum TypeOrParsedExpr {
    Type(TypeId),
    Parsed(ParsedExprId),
}

nz_u32_id!(PatternCtorId);

impl PatternCtorId {
    pub const B_FALSE: PatternCtorId = PatternCtorId::from_u32(1).unwrap();
    pub const B_TRUE: PatternCtorId = PatternCtorId::from_u32(2).unwrap();
    pub const CHAR: PatternCtorId = PatternCtorId::from_u32(3).unwrap();
    pub const STRING: PatternCtorId = PatternCtorId::from_u32(4).unwrap();
    pub const INT: PatternCtorId = PatternCtorId::from_u32(5).unwrap();
    pub const FLOAT: PatternCtorId = PatternCtorId::from_u32(6).unwrap();
    pub const POINTER: PatternCtorId = PatternCtorId::from_u32(7).unwrap();

    pub const TYPE_VARIABLE: PatternCtorId = PatternCtorId::from_u32(8).unwrap();
    pub const FUNCTION_POINTER: PatternCtorId = PatternCtorId::from_u32(9).unwrap();
    pub const LAMBDA_OBJECT: PatternCtorId = PatternCtorId::from_u32(10).unwrap();
    pub const BUFFER: PatternCtorId = PatternCtorId::from_u32(11).unwrap();
    pub const SPAN: PatternCtorId = PatternCtorId::from_u32(12).unwrap();
    pub const OPAQUE: PatternCtorId = PatternCtorId::from_u32(13).unwrap();
}

/// Used for analyzing pattern matching
#[derive(Debug, Clone, Copy)]
pub enum PatternCtor {
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
    LambdaObject,
    ValueType,
    Buffer,
    Span,
    Opaque,
    /// In the future, we should do real array patterns since length is statically known
    Array,
    Reference(PatternCtorId),
    Struct {
        /// Exact-size slice in `patterns.mem`, filled by field index
        fields: MSlice<(StringId, PatternCtorId), TypedPatternPool>,
    },
    Enum {
        variant_name: StringId,
    },
    Sum {
        variant_name: StringId,
        inner: Option<PatternCtorId>,
    },
}

impl PatternCtor {
    pub fn kind_name(&self) -> &'static str {
        match self {
            PatternCtor::BoolFalse => "false",
            PatternCtor::BoolTrue => "true",
            PatternCtor::Char => "char",
            PatternCtor::String => "string",
            PatternCtor::Int => "int",
            PatternCtor::Float => "float",
            PatternCtor::Pointer => "pointer",
            PatternCtor::TypeVariable => "type variable",
            PatternCtor::FunctionPointer => "function pointer",
            PatternCtor::LambdaObject => "lambda object",
            PatternCtor::ValueType => "value type",
            PatternCtor::Buffer => "buffer",
            PatternCtor::Span => "span",
            PatternCtor::Opaque => "opaque",
            PatternCtor::Array => "array",
            PatternCtor::Reference(_) => "reference",
            PatternCtor::Struct { .. } => "struct",
            PatternCtor::Sum { .. } => "sum variant",
            PatternCtor::Enum { .. } => "enum value",
        }
    }

    pub fn struct_fields(&self) -> MSlice<(StringId, PatternCtorId), TypedPatternPool> {
        match self {
            PatternCtor::Struct { fields } => *fields,
            _ => panic!("struct_fields on {}", self.kind_name()),
        }
    }
}

#[derive(Clone, Copy)]
struct PatternCtorTrialEntry {
    ctor: PatternCtorId,
    alive: bool,
}

#[derive(Clone)]
pub struct AbilitySpec9nInfo {
    generic_parent: AbilityId,
    specialized_child: AbilityId,
    arguments: TypeArgs,
}
impl_copy_if_small!(20, AbilitySpec9nInfo);

#[derive(Clone, Copy)]
pub enum TypedAbilityKind {
    Concrete,
    Generic { specializations: PermList<AbilitySpec9nInfo> },
    Specialized(AbilitySpec9nInfo),
}

impl TypedAbilityKind {
    pub fn arguments(&self, mem: &Mem<TypedProgram>) -> &'static [TypeId] {
        match self {
            TypedAbilityKind::Specialized(specialization) => specialization.arguments.as_slice(mem),
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
    pub type_arguments: TypeArgs,
    pub fnlike_type_arguments: TypeArgs,
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
    /// All specializations of me
    pub child_specializations: PermList<SpecializationInfo>,
    /// If I am specialization myself
    pub specialization_info: Option<SpecializationInfo>,
    pub parsed_id: ParsedId,
    pub type_id: TypeId,
    pub compiler_debug: bool,
    pub kind: TypedFunctionKind,
    pub is_concrete: bool,
    pub is_recursive: bool,
    pub is_macro: bool,
    pub is_reloadable: bool,
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

#[derive(Debug, Clone, Copy)]
pub struct TypeSpecialization {
    pub base: TypeId,
    pub args: TypeArgs,
    pub specialized: TypeId,
}
static_assert_size!(TypeSpecialization, 20);

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
    BoolToInt,
    ReferenceToMut,
    ReferenceUnMut,
    ReferenceToReference,
    PointerToReference,
    ReferenceToPointer,
    /// Destination type can only be u64 and i64
    PointerToWord,
    PointerToFunctionPointer,
    WordToPointer,
    FloatExtend,
    FloatTruncate,
    FloatToUnsignedInteger,
    FloatToSignedInteger,
    IntegerUnsignedToFloat,
    IntegerSignedToFloat,
    /// Placeholder for erasing an ability impl to a dyn object inside an
    /// abstract (where-bound generic) body; these are not lowered
    /// since specialization re-evaluates the body and lowers to an allocation of a struct literal
    AbilityImplToDynObject,
}

impl Display for CastType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CastType::IntegerCast(_dir) => write!(f, "intcast"),
            CastType::Integer8ToChar => write!(f, "i8tochar"),
            CastType::IntegerExtendFromChar => write!(f, "iextfromchar"),
            CastType::BoolToInt => write!(f, "bool2int"),
            CastType::ReferenceToMut => write!(f, "reference2mut"),
            CastType::ReferenceUnMut => write!(f, "reference-unmut"),
            CastType::ReferenceToReference => write!(f, "reftoref"),
            CastType::PointerToReference => write!(f, "ptrtoref"),
            CastType::ReferenceToPointer => write!(f, "reftoptr"),
            CastType::PointerToWord => write!(f, "ptrtoword"),
            CastType::WordToPointer => write!(f, "wordtoptr"),
            CastType::PointerToFunctionPointer => write!(f, "ptr-to-fnptr"),
            CastType::FloatExtend => write!(f, "fext"),
            CastType::FloatTruncate => write!(f, "ftrunc"),
            CastType::FloatToUnsignedInteger => write!(f, "ftouint"),
            CastType::FloatToSignedInteger => write!(f, "ftosint"),
            CastType::IntegerUnsignedToFloat => write!(f, "uinttof"),
            CastType::IntegerSignedToFloat => write!(f, "sinttof"),
            CastType::AbilityImplToDynObject => write!(f, "impl2dyn"),
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
}

impl MatchingConditionInstr {
    pub fn cond(value: TypedExprId) -> Self {
        MatchingConditionInstr::Cond { value }
    }
    pub fn binding(let_stmt: TypedStmtId) -> Self {
        MatchingConditionInstr::Binding { let_stmt }
    }
}

impl_copy_if_small!(8, MatchingConditionInstr);

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
    pub initial_let_statements: PermSlice<TypedStmtId>,
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
}

impl GlobalInitialValue {
    pub fn as_value(&self) -> Option<StaticValueId> {
        match self {
            GlobalInitialValue::Value(v) => Some(*v),
            GlobalInitialValue::Pending | GlobalInitialValue::Uninit => None,
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

    pub fn find_child_by_name(&self, parent_id: NamespaceId, name: StringId) -> Option<&Namespace> {
        self.iter()
            .find(|ns| ns.parent_id.is_some_and(|parent| parent == parent_id) && ns.name == name)
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
pub enum BitCastKind {
    ScalarToScalar,
    AggToAgg,
    AggToScalar,
    ScalarToAgg,
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
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinIr {
    BakeStaticValue,
    Zeroed,
    BoolNegate,
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
            BuiltinIr::BoolNegate => "bool_negate",
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
    /// For Pattern matching trials
    trial_ctors: Vec<PatternCtorTrialEntry>,
    field_ctors: Vec<Vec<(StringId, PatternCtorId)>>,
    pattern_ctor_ancestor_stack: Vec<TypeId>,
    int_parse: String,
    visited_types: RefCell<Vec<TypeId>>,
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
    pub manifest: ModuleManifest,
    pub namespace_id: NamespaceId,
    pub namespace_scope_id: ScopeId,
    /// Schema type and merged value of the module's k1/module-params declaration
    pub params: Option<ModuleParams>,
    /// One entry per source file in compile order
    pub source_file_hashes: PermList<SourceFileHash>,
    /// The module's parsed root namespace
    pub parsed_namespace_id: ParsedNamespaceId,
    /// The fn module definition, evaluated at load; typing skips it
    pub manifest_fn_defn: Option<ParsedId>,
    /// Directory module vs single file
    pub is_dir: bool,
}

impl Module {
    // Used to 'reserve' a spot for module so that parser can know its module id
    pub fn pending(id: ModuleId, name: StringId, home_dir: StringId) -> Self {
        Module {
            id,
            name,
            home_dir,
            manifest: ModuleManifest::defaulted(ModuleKind::Library),
            namespace_id: NamespaceId::PENDING,
            namespace_scope_id: ScopeId::PENDING,
            params: None,
            source_file_hashes: MList::empty(),
            parsed_namespace_id: ParsedNamespaceId::PENDING,
            manifest_fn_defn: None,
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
    pub namespace_id: NamespaceId,
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
    pub type_args: TypeArgs,
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
}
impl TypeDefnContext {
    fn reset(&mut self) {
        self.stack.clear();
        self.recursive_mentions.clear();
        self.pending_instances.clear();
        self.completed.clear();
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

    pub variables: VPool<Variable, VariableId>,

    pub types: VPool<Type, TypeId>,
    pub type_hashes: FxHashMap<u64, TypeId>,
    pub type_variable_counts: VPool<TypeInfo, TypeId>,
    pub type_instance_info: VPool<Option<GenericInstanceInfo>, TypeId>,
    pub type_defn_info: FxHashMap<TypeId, TypeDefnInfo>,
    pub type_specializations: hashbrown::HashTable<TypeSpecialization>,
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
    pub type_schemas: FxHashMap<TypeId, StaticValueId>,
    pub type_names: FxHashMap<TypeId, StaticValueId>,
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

    /// Every metaprogram-emitted source, in emission order; carries the byte
    /// range provenance for diagnostics and the queue for the end-of-typecheck
    /// file flush. Its index is the serial in emitted filenames.
    pub emitted_sources: Vec<EmittedSource>,

    // For every static value, once evaluated, we store its runtime representation
    // here; the data lives in vm_static_stack
    pub vm_shared_static_stack: vm::Stack,
    pub vm_global_constant_lookups: FxHashMap<TypedGlobalId, vm::Value>,
    pub vm_static_value_lookups: FxHashMap<StaticValueId, vm::Value>,
    pub vm_process_dlopen_handle: *mut std::ffi::c_void,

    pub vm_dylib_handles: FxHashMap<(ModuleId, StringId), *mut std::ffi::c_void>,
    pub vm_ffi_functions: FxHashMap<FunctionId, vm::VmFfiHandle>,

    /// Perm arena space
    pub mem: kmem::Mem<TypedProgram>,
    /// tmp arena space
    pub tmp: kmem::Mem<MemTmp>,

    pub ir: ir::ProgramIr,
    pub bc: crate::bc::BcProgram,

    pub timing: Timing,

    pub global_id_k1_arena: Option<TypedGlobalId>,
    pub megarepl: Option<MegareplState>,

    /// Hash of every compile input consumed so far: build, config, source files
    pub inputs_hash: crate::snap::InputsHash,
    /// Modules restored from the disk cache this session, for --chatty and tests
    pub restored_module_count: u32,
    /// In-flight snapshot disk writes
    pub pending_cache_writes: Vec<std::thread::JoinHandle<()>>,
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
        let mut pattern_ctors = VPool::make_with_hint("pattern_ctors", 8192);
        pattern_ctors.add_expected_id(PatternCtor::BoolFalse, PatternCtorId::B_FALSE);
        pattern_ctors.add_expected_id(PatternCtor::BoolTrue, PatternCtorId::B_TRUE);
        pattern_ctors.add_expected_id(PatternCtor::Char, PatternCtorId::CHAR);
        pattern_ctors.add_expected_id(PatternCtor::String, PatternCtorId::STRING);
        pattern_ctors.add_expected_id(PatternCtor::Int, PatternCtorId::INT);
        pattern_ctors.add_expected_id(PatternCtor::Float, PatternCtorId::FLOAT);
        pattern_ctors.add_expected_id(PatternCtor::Pointer, PatternCtorId::POINTER);
        pattern_ctors.add_expected_id(PatternCtor::TypeVariable, PatternCtorId::TYPE_VARIABLE);
        pattern_ctors
            .add_expected_id(PatternCtor::FunctionPointer, PatternCtorId::FUNCTION_POINTER);
        pattern_ctors.add_expected_id(PatternCtor::LambdaObject, PatternCtorId::LAMBDA_OBJECT);
        pattern_ctors.add_expected_id(PatternCtor::Buffer, PatternCtorId::BUFFER);
        pattern_ctors.add_expected_id(PatternCtor::Span, PatternCtorId::SPAN);
        pattern_ctors.add_expected_id(PatternCtor::Opaque, PatternCtorId::OPAQUE);

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
            variables: VPool::make("typed_variables"),
            types: VPool::make("types"),
            type_hashes: FxHashMap::new(),
            type_variable_counts: VPool::make("type_variable_counts"),
            type_instance_info: VPool::make("instance_info"),
            type_defn_info: FxHashMap::new(),
            type_specializations: hashbrown::HashTable::new(),
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
            type_schemas: FxHashMap::new(),
            type_names: FxHashMap::new(),
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
                trial_ctors: Vec::new(),
                field_ctors: (0..128).map(|_| Vec::new()).collect::<Vec<_>>(),
                pattern_ctor_ancestor_stack: Vec::with_capacity(64),
                int_parse: String::with_capacity(128),
                visited_types: RefCell::new(Vec::new()),
            },
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
            emitted_sources: Vec::new(),
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
            pending_cache_writes: vec![],
        };

        let empty_struct_id = k1.add_type_anon(Type::Struct(StructType::struc(MSlice::empty())));
        k1.builtin_types.empty = empty_struct_id;
        assert_eq!(empty_struct_id, EMPTY_TYPE_ID);

        k1
    }

    pub fn add_module(
        &mut self,
        root_handle: crate::compiler::ModuleRootHandle,
        primary_module: bool,
    ) -> anyhow::Result<ModuleId> {
        let mut load_stack: Vec<StringId> = vec![];
        let mut modules_to_typecheck: Vec<(
            ModuleId,
            Option<crate::compiler::ModuleRemainingSourcesHandle>,
        )> = vec![];
        let added_module_id = self.discover_module_and_deps(
            root_handle,
            primary_module,
            &mut load_stack,
            &mut modules_to_typecheck,
        )?;

        if primary_module && self.config.setup_mode.is_setup_only() {
            return Ok(added_module_id);
        }

        let mut modules_to_typecheck: Vec<(
            ModuleId,
            Option<Vec<crate::compiler::SourceFile>>,
            crate::snap::InputsHash,
        )> = modules_to_typecheck
            .into_iter()
            .map(|(module_id, remaining)| {
                Ok((
                    module_id,
                    remaining.map(|r| r.join()).transpose()?,
                    crate::snap::InputsHash(0),
                ))
            })
            .collect::<anyhow::Result<_>>()?;

        // Every root is parsed before any typing, so all headers are hashed
        // in discovery order first
        let mut hash = self.inputs_hash;
        for (module_id, _, _) in &modules_to_typecheck {
            let module = self.modules.get(*module_id);
            let root = module.source_file_hashes.as_slice(&self.mem)[0];
            let source = self.ast.sources.get(root.file_id);
            hash = hash.add_module_header(
                self.ident_str(module.name),
                self.ident_str(source.directory),
                self.ident_str(source.filename),
                root.hash,
            );
        }
        for (module_id, files, module_hash) in modules_to_typecheck.iter_mut() {
            let module = self.modules.get(*module_id);
            let name = self.ident_str(module.name);
            hash = match files {
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
                            let path = kpath::join_tmp(
                                self.get_tmp_unsafe(),
                                &self.ast.idents,
                                s.directory,
                                s.filename,
                            );
                            (path, sfh.hash)
                        }),
                    )
                }
            };
            *module_hash = hash;
        }

        for (module_id, files, module_hash) in modules_to_typecheck.into_iter() {
            let module = self.modules.get(module_id);
            let (module_name, home_dir, parsed_namespace_id, manifest_fn_defn) =
                (module.name, module.home_dir, module.parsed_namespace_id, module.manifest_fn_defn);
            let root_file_id = module.root_file_id(&self.mem);
            if let Some(files) = files {
                for file in files {
                    self.parse_module_source_file(
                        module_id,
                        module_name,
                        parsed_namespace_id,
                        home_dir,
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
                if let Err(e) = self.check_manifest_fn_placement(parsed_namespace_id, root_file_id)
                {
                    self.report(e);
                    bail!("Module {} has a misplaced fn module", self.ident_str(module_name));
                }
                self.typecheck_module(module_id, parsed_namespace_id, manifest_fn_defn)?;
                self.modules_completed.push(module_id);
                // Drain pending IR so a snapshot here contains only whole
                // modules (pending queues empty)
                if let Err(msg) = self.compile_all_pending_ir(SpanId::NONE) {
                    self.report(msg);
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
                if !crate::snap::cache_exists(self.cache_dir(), module_hash) {
                    let cache_dir = self.cache_dir().to_path_buf();
                    let bytes = self.snap();
                    self.pending_cache_writes.push(std::thread::spawn(move || {
                        crate::snap::cache_store(&cache_dir, module_hash, &bytes)
                    }));
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
        self.debug_snapshot_roundtrip();

        Ok(added_module_id)
    }

    pub(crate) fn cache_dir(&self) -> &Path {
        Path::new(self.get_string(self.config.cache_dir))
    }

    pub fn join_cache_writes(&mut self) {
        for handle in self.pending_cache_writes.drain(..) {
            let _ = handle.join();
        }
    }

    fn discover_module_and_deps(
        &mut self,
        root_handle: crate::compiler::ModuleRootHandle,
        primary_module: bool,
        load_stack: &mut Vec<StringId>,
        modules_to_typecheck: &mut Vec<(
            ModuleId,
            Option<crate::compiler::ModuleRemainingSourcesHandle>,
        )>,
    ) -> anyhow::Result<ModuleId> {
        debug!("Loading module {}...", root_handle.src_path);
        let module_name = self.ast.idents.intern(kpath::file_stem(&root_handle.src_path));
        if let Some(m) = self.modules.iter().find(|m| m.name == module_name) {
            fn queue(
                k1: &mut TypedProgram,
                module_id: ModuleId,
                modules_to_typecheck: &mut Vec<(
                    ModuleId,
                    Option<crate::compiler::ModuleRemainingSourcesHandle>,
                )>,
            ) {
                if modules_to_typecheck.iter().any(|(id, _)| *id == module_id) {
                    return;
                }
                let deps = k1.modules.get(module_id).manifest.deps;
                for i in 0..deps.len() {
                    let dep_name = k1.mem.getn(deps)[i as usize].name;
                    let dep = k1
                        .modules
                        .iter()
                        .find(|m| m.name == dep_name)
                        .expect("restored state is missing a discovered module's dep");
                    let dep_id = dep.id;
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

        let home_dir = self.ast.idents.intern(&root_handle.module_dir);
        let module_id = self.modules.next_id();
        let module_id = self
            .modules
            .add_expected_id(Module::pending(module_id, module_name, home_dir), module_id);
        let is_core = module_id == MODULE_ID_CORE;
        load_stack.push(module_name);

        let (root_file, remaining_sources) = root_handle.join_root()?;

        let parsed_namespace_id = parse::init_module(module_name, &mut self.ast);
        self.modules.get_mut(module_id).parsed_namespace_id = parsed_namespace_id;
        self.parse_module_source_file(
            module_id,
            module_name,
            parsed_namespace_id,
            home_dir,
            root_file,
        );
        if !self.ast.errors.is_empty() && !self.lsp.completion {
            bail!(
                "Parsing module {} failed with {} errors",
                self.ident_str(module_name),
                self.ast.errors.len()
            );
        }

        let (manifest, manifest_fn_defn) = if is_core {
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
            self.module_in_progress = Some(module_id);
            let manifest_result =
                self.evaluate_module_manifest(parsed_namespace_id, primary_module);
            self.module_in_progress = None;
            match manifest_result {
                Err(e) => {
                    self.report(e);
                    bail!(
                        "Failed to evaluate module manifest. Note: fn module is evaluated \
                         before the module compiles, so module-local definitions are not \
                         visible in it"
                    )
                }
                Ok(None) => {
                    let kind =
                        if primary_module { ModuleKind::Executable } else { ModuleKind::Library };
                    (ModuleManifest::defaulted(kind), None)
                }
                Ok(Some((manifest, fn_defn))) => (manifest, Some(fn_defn)),
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

        let deps = manifest.deps;
        let m = self.modules.get_mut(module_id);
        m.manifest = manifest;
        m.manifest_fn_defn = manifest_fn_defn;
        m.is_dir = remaining_sources.is_dir();

        let manifest_span =
            manifest_fn_defn.map(|d| self.ast.get_span_for_id(d)).unwrap_or(SpanId::NONE);

        let setup_decl = self.modules.get(module_id).manifest.setup;
        if let Some(setup) = setup_decl
            && self.config.setup_mode != crate::compiler::SetupMode::SetupProgram
        {
            if !remaining_sources.is_dir() {
                let msg = "single-file modules cannot declare setup; make it a \
                           directory module (setup.k1 lives in the module directory)";
                return Err(self.module_error(manifest_span, msg.to_string()));
            }
            let request = crate::compiler::SetupRequest {
                idents: &self.ast.idents,
                module_dir: home_dir,
                module_name,
                outputs: self.mem.getn(setup.outputs),
                inputs: self.mem.getn(setup.inputs),
                target: self.config.target,
                k1_home: self.config.k1_home,
                force: self.config.setup_mode
                    == (crate::compiler::SetupMode::SetupOnly { force: true })
                    && primary_module,
                chatty: self.config.chatty,
            };
            if let Err(e) = crate::compiler::run_setup_function(&request) {
                let msg = format!(
                    "Setup failed for module '{}': {e:#} ",
                    self.ast.idents.get_string(module_name),
                );
                return Err(self.module_error(manifest_span, msg));
            }
        }

        let remaining = self.spawn_remaining_sources(module_id);
        let mut dep_handles: Vec<crate::compiler::ModuleRootHandle> = vec![];
        for i in 0..deps.len() {
            let dep_name_id = self.mem.getn(deps)[i as usize].name;
            if self.ast.idents.get_string(dep_name_id) == self.program_name() {
                let msg = format!(
                    "Module '{}' depends on '{}': module name collision with the program itself",
                    self.ident_str(module_name),
                    self.ast.idents.get_string(dep_name_id)
                );
                return Err(self.module_error(manifest_span, msg));
            }
            if load_stack.contains(&dep_name_id) {
                let mut cycle: Vec<&str> = vec![];
                for n in load_stack.iter().skip_while(|n| **n != dep_name_id) {
                    cycle.push(self.ast.idents.get_string(*n));
                }
                cycle.push(self.ast.idents.get_string(dep_name_id));
                let msg = format!("Module dependency cycle: {}", cycle.join(" -> "));
                return Err(self.module_error(manifest_span, msg));
            }
            if self.modules.iter().any(|m| m.name == dep_name_id) {
                continue;
            }
            let dep_str = self.ast.idents.get_string(dep_name_id);
            let module_deps_path = kpath::join_tmp(
                self.get_tmp_unsafe(),
                &self.ast.idents,
                self.config.home_dir,
                ("deps", dep_str),
            );
            let k1_home_modules_path = kpath::join_tmp(
                self.get_tmp_unsafe(),
                &self.ast.idents,
                self.config.k1_home,
                ("modules", dep_str),
            );
            let dep_path = if Path::new(module_deps_path.as_str()).exists() {
                module_deps_path
            } else if Path::new(k1_home_modules_path.as_str()).exists() {
                k1_home_modules_path
            } else {
                let msg = format!(
                    "Module '{}' depends on '{}', which was not found. Probed: {module_deps_path}, {k1_home_modules_path}",
                    self.ident_str(module_name),
                    dep_str,
                );
                return Err(self.module_error(manifest_span, msg));
            };
            match crate::compiler::ModuleRootHandle::spawn(
                &self.ast.idents,
                Path::new(dep_path.as_str()),
                false,
                &self.lsp.source_overrides,
            ) {
                Ok(handle) => dep_handles.push(handle),
                Err(e) => {
                    let msg = format!(
                        "Module '{}' depends on '{}', which failed to load: {}",
                        self.ident_str(module_name),
                        dep_str,
                        e
                    );
                    return Err(self.module_error(manifest_span, msg));
                }
            }
        }

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
        let (home_dir, is_dir) = (module.home_dir, module.is_dir);
        let root_filename = self.ast.sources.get(module.root_file_id(&self.mem)).filename;
        let root_path = crate::compiler::pathbuf_into_string(kpath::join_buf(
            &self.ast.idents,
            home_dir,
            root_filename,
        ));
        crate::compiler::ModuleRemainingSources::new(
            self.ident_str(home_dir).to_string(),
            root_path,
            is_dir,
        )
        .spawn_read(&self.lsp.source_overrides)
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
        directory: StringId,
        file: crate::compiler::SourceFile,
    ) -> FileId {
        let filename = self.ast.idents.intern(kpath::file_name(&file.path));
        let source = parse::SourceFile::make(&mut self.ast.mem, directory, filename, &file.content);
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

    fn check_manifest_fn_placement(
        &self,
        parsed_namespace_id: ParsedNamespaceId,
        root_file_id: FileId,
    ) -> K1Result<()> {
        let module_ident = self.ast.idents.b.module;
        let namespace = self.ast.namespaces.get(parsed_namespace_id);
        for defn in namespace.definitions.as_slice(&self.ast.mem) {
            let Some(fn_id) = defn.as_function_id() else { continue };
            let f = self.ast.get_function(fn_id);
            if f.name == module_ident && self.ast.spans.get(f.span).file_id != root_file_id {
                kbail!(
                    self,
                    f.span,
                    "fn module is the module's manifest and must live in its root file \
                     (module.k1 or <module-name>.k1)"
                );
            }
        }
        Ok(())
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
        &self.ast.name
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

    pub fn run_setup_entry(&mut self, module_dir: &str) -> anyhow::Result<()> {
        let primary = self.primary_module();
        let ns_scope = primary.namespace_scope_id;
        let Some(setup_fn_id) = self.scopes.find_function_local(ns_scope, self.ast.idents.b.setup)
        else {
            bail!("setup.k1 must define a top-level `fn setup(ctx: k1/setup-ctx)`");
        };
        let setup_ctx_type = self.builtin_types.k1_setup_ctx.unwrap();
        let function = self.get_function(setup_fn_id);
        let span = self.ast.get_span_for_id(function.parsed_id);
        let params = self.mem.getn(self.get_function_type(setup_fn_id).logical_params());
        if !function.type_params.is_empty()
            || params.len() != 1
            || params[0].type_id != setup_ctx_type
        {
            let e = self
                .make_error("fn setup must take exactly one parameter of type k1/setup-ctx", span);
            self.report(e);
            bail!("fn setup has the wrong signature");
        }

        let dir_string_id = self.ast.idents.intern(module_dir);
        let dir_value = self.static_values.add_string(dir_string_id);
        let ctx_fields = self.static_values.mem.pushn(&[dir_value]);
        let ctx_value = self
            .static_values
            .add(StaticValue::Struct(StaticStruct { type_id: setup_ctx_type, fields: ctx_fields }));
        let result = self.execute_static_function(setup_fn_id, &[ctx_value], span);
        if let Err(e) = result {
            self.report(e);
            bail!("fn setup failed");
        }
        Ok(())
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

    fn eval_type_defn(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        namespace_scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        let parsed_type_defn = *self.ast.get_type_defn(parsed_type_defn_id);
        let is_generic_defn = !parsed_type_defn.type_params.is_empty();
        let is_alias = parsed_type_defn.flags.is_alias();
        debug!("eval_type_defn {}", self.ident_str(parsed_type_defn.name));

        if parsed_type_defn.name == self.ast.idents.b.some {
            kbail!(self, parsed_type_defn.span, "'some' is not a valid type name");
        }

        let reserved_type_id = if !is_alias {
            let reserved_type_id = self.reserve_type_id();
            self.type_defn_context
                .stack
                .push(TypeDefnStackEntry { parsed_id: parsed_type_defn_id, reserved_type_id });
            Some(reserved_type_id)
        } else {
            None
        };

        let type_eval_context = EvalTypeExprContext {
            is_inside_type_definition_rhs: !parsed_type_defn.flags.is_alias(),
            is_direct_function_parameter: false,
            is_inside_static_type: false,
        };

        // No need to create a scope for non-generic type definitions
        let defn_scope_id = if is_generic_defn {
            let defn_scope_id = self.scopes.add_child_scope(
                namespace_scope_id,
                ScopeType::TypeDefn,
                ScopeOwnerId::None,
            );
            defn_scope_id
        } else {
            namespace_scope_id
        };

        let mut type_params: List<TypeId, _> =
            self.mem.new_list(parsed_type_defn.type_params.len());
        for type_param in self.ast.mem.getn(parsed_type_defn.type_params).iter() {
            let maybe_static_constraint =
                match ParsedTypeConstraintExpr::single_static_constraint_or_fail(
                    &self.ast.mem,
                    type_param.constraints,
                ) {
                    Ok(Some(parsed_static_constraint)) => {
                        Some(self.eval_type_expr(parsed_static_constraint, defn_scope_id)?)
                    }
                    Ok(None) => None,
                    Err(msg) => kbail!(self, type_param.span, "{}", msg),
                };
            let mut ability_constraint_signatures: SV4<TypedAbilitySignature> = smallvec![];
            let mut predicate_functions = self.mem.new_list(0);
            for parsed_constraint in self.ast.mem.getn(type_param.constraints) {
                match parsed_constraint {
                    ParsedTypeConstraintExpr::Ability(ability_expr) => {
                        let ability_sig =
                            self.eval_ability_expr(*ability_expr, false, defn_scope_id)?;
                        ability_constraint_signatures.push(ability_sig);
                    }
                    ParsedTypeConstraintExpr::Predicate(qident) => {
                        predicate_functions.push_grow(&mut self.mem, *qident);
                    }
                    ParsedTypeConstraintExpr::Static(_) => {}
                };
            }
            let predicate_functions_handle = predicate_functions.to_slice();
            let type_variable_id = self.add_type_parameter(
                TypeParameter {
                    name: type_param.name,
                    static_constraint: maybe_static_constraint,
                    predicate_functions: predicate_functions_handle,
                    scope_id: defn_scope_id,
                    span: type_param.span,
                },
                ability_constraint_signatures,
            );
            type_params.push(type_variable_id);
            let added = self.scopes.add_type(defn_scope_id, type_param.name, type_variable_id);
            if !added {
                kbail!(
                    self,
                    type_param.span,
                    "Type variable name '{}' is taken",
                    self.ident_str(type_param.name).blue()
                );
            }
        }
        let type_params_handle = type_params.to_slice();

        // Actually compile the RHS
        let rhs_type_id = match self.ast.type_exprs.get(parsed_type_defn.value_expr) {
            ParsedTypeExpr::Builtin(_) => {
                let defn_type_id = reserved_type_id.unwrap();
                let type_value = match defn_type_id {
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
                    _ => {
                        kbail!(
                            self,
                            parsed_type_defn.span,
                            "Unknown builtin type id: {}",
                            defn_type_id
                        );
                    }
                };
                self.set_type(defn_type_id, type_value, None, None);
                defn_type_id
            }
            _ => {
                let rhs_type_id = self.eval_type_expr_ext(
                    parsed_type_defn.value_expr,
                    defn_scope_id,
                    type_eval_context,
                )?;
                let rhs_is_reserved =
                    self.type_defn_context.stack.iter().any(|e| e.reserved_type_id == rhs_type_id);
                let rhs_is_pending = self
                    .type_defn_context
                    .pending_instances
                    .iter()
                    .any(|p| p.type_id == rhs_type_id);
                let rhs_still_defining = !is_alias && (rhs_is_reserved || rhs_is_pending);
                if rhs_still_defining {
                    kbail!(
                        self,
                        parsed_type_defn.span,
                        "The right-hand side of a type definition cannot be a bare reference to a type that is still being defined; wrap it in a struct or either, or use an alias"
                    );
                }
                rhs_type_id
            }
        };

        // not an alias, so only allow named struct or sum or builtin
        // TODO: revisit this? why can't we have nominal integer types?
        if !is_alias {
            match self.types.get(rhs_type_id) {
                Type::Char
                | Type::Bool
                | Type::Never
                | Type::Pointer
                | Type::Integer(_)
                | Type::Float(_) => {
                    if let ParsedTypeExpr::Builtin(_) =
                        self.ast.type_exprs.get(parsed_type_defn.value_expr)
                    {
                        // fine
                    } else {
                        kbail!(
                            self,
                            parsed_type_defn.span,
                            "Non-alias type definition must be a struct or sum; not a '{}'. Perhaps you intended to create an alias `type(alias) <name> = <type>`",
                            rhs_type_id
                        );
                    }
                }
                // Allowed nominal types
                Type::Struct(_) | Type::Sum(_) | Type::Enum(_) | Type::Opaque(_) => {}
                _other => {
                    kbail!(
                        self,
                        parsed_type_defn.span,
                        "Non-alias type definition must be a struct or either or opaque or builtin; not a '{}'. Perhaps you meant to create an alias `type(alias) <name> = <type>`",
                        rhs_type_id
                    );
                }
            };
        }

        let found_namespace_id =
            self.scopes.find_namespace_local(namespace_scope_id, parsed_type_defn.name);
        let companion_namespace_id = match found_namespace_id {
            None => None,
            Some(ns_id) => {
                let companion_ns = self.namespaces.get(ns_id);
                if companion_ns.namespace_type == NamespaceKind::TypeCompanion {
                    Some(ns_id)
                } else {
                    self.report_hint(parsed_type_defn.span, "matching namespace is not declared as type companion; use an `ns for {}` declaration");
                    None
                }
            }
        };
        let defn_info = if is_alias {
            None
        } else {
            Some(TypeDefnInfo {
                name: parsed_type_defn.name,
                scope: namespace_scope_id,
                companion_namespace: companion_namespace_id,
                ast_id: ParsedId::TypeDefn(parsed_type_defn_id),
                // Over-approximate: anything defined while the cluster has recursive mentions
                // is treated as recursive
                recursive: !self.type_defn_context.recursive_mentions.is_empty(),
            })
        };

        let type_id = if is_generic_defn {
            let gen_type = GenericType { params: type_params_handle, inner: rhs_type_id };
            let generic_id = reserved_type_id.unwrap();
            self.set_type(generic_id, Type::Generic(gen_type), None, defn_info);
            if self.get_specialization(generic_id, self.mem.getn(type_params_handle)).is_none() {
                let inner_content = *self.types.get(rhs_type_id);
                let type_args =
                    TypeArgs::from_slice_in(self.mem.getn(type_params_handle), &mut self.mem);
                let self_instance = self.add_type(
                    inner_content,
                    defn_info,
                    Some(GenericInstanceInfo { generic_parent: generic_id, type_args }),
                );
                self.insert_specialization(generic_id, type_args, self_instance);
            }
            generic_id
        } else {
            if is_alias {
                rhs_type_id
            } else {
                let t = *self.types.get(rhs_type_id);
                let instance_info = self.get_instance_info(rhs_type_id).cloned();
                self.set_type(reserved_type_id.unwrap(), t, instance_info, defn_info);
                reserved_type_id.unwrap()
            }
        };

        if let Some(companion_namespace_id) = companion_namespace_id {
            self.namespaces.get_mut(companion_namespace_id).companion_type_id = Some(type_id);
        }
        let name = parsed_type_defn.name;
        let added = self.scopes.add_type(namespace_scope_id, name, type_id);
        if !added {
            let span = parsed_type_defn.span;
            self.report(kerr!(self, span, "Type {} exists", name));
        }

        // Capture type_ids of compiler-known types
        if namespace_scope_id == self.scopes.core_scope_id {
            if name == self.ast.idents.b.string {
                self.builtin_types.string = Some(type_id);
            } else if name == self.ast.idents.b.bool {
                self.builtin_types.bool = Some(type_id);
            } else if name == self.ast.idents.b.char {
                self.builtin_types.char = Some(type_id);
            } else if name == self.ast.idents.b.buffer {
                self.builtin_types.buffer = Some(type_id);
            } else if name == self.ast.idents.b.span {
                self.builtin_types.span = Some(type_id);
            } else if name == self.ast.idents.b.list {
                self.builtin_types.list = Some(type_id);
            } else if name == self.ast.idents.b.opt {
                self.builtin_types.opt = Some(type_id);
            } else if name == self.ast.idents.b.ordering {
                self.builtin_types.ordering = Some(type_id);
            } else if name == self.ast.idents.b.code {
                self.builtin_types.code = Some(type_id);
            } else if name == self.ast.idents.b.code_chunk {
                self.builtin_types.code_chunk = Some(type_id);
            } else if name == self.ast.idents.b.code_builder {
                self.builtin_types.code_builder = Some(type_id);
            }
        } else if namespace_scope_id == self.scopes.types_scope_id {
            if name == self.ast.idents.b.type_schema {
                self.builtin_types.types_type_schema = Some(type_id);
            } else if name == self.ast.idents.b.int_kind {
                self.builtin_types.types_int_kind = Some(type_id);
            } else if name == self.ast.idents.b.int_value {
                self.builtin_types.types_int_value = Some(type_id)
            } else if name == self.ast.idents.b.float_kind {
                self.builtin_types.types_float_kind = Some(type_id)
            } else if name == self.ast.idents.b.float_value {
                self.builtin_types.types_float_value = Some(type_id)
            } else if name == self.ast.idents.b.type_id {
                self.builtin_types.types_type_id = Some(type_id)
            } else if name == self.ast.idents.b.layout {
                self.builtin_types.types_layout = Some(type_id)
            }
        } else if namespace_scope_id == self.scopes.k1_scope_id {
            if name == self.ast.idents.b.source_location {
                self.builtin_types.source_location = Some(type_id)
            } else if name == self.ast.idents.b.module {
                self.builtin_types.k1_module = Some(type_id)
            } else if name == self.ast.idents.b.setup_ctx {
                self.builtin_types.k1_setup_ctx = Some(type_id)
            }
        }

        if !is_alias {
            let Some(defn_stack_entry) = self.type_defn_context.stack.pop() else {
                self.ice_span(parsed_type_defn.span, "No defn stack entry");
            };
            debug_assert_eq!(defn_stack_entry.reserved_type_id, type_id);
            self.type_defn_context.completed.push((type_id, parsed_type_defn.span));

            if self.type_defn_context.stack.is_empty() {
                self.finish_type_defn_cluster()
            }
        }
        if let Some(idx) = self
            .types_pending_definition
            .iter()
            .position(|tpd| tpd.parsed_id == parsed_type_defn_id)
        {
            // eprintln!("removing pending defn {idx} {}", self.ident_str(name));
            self.types_pending_definition.remove(idx);
        } else {
            self.ice_span(parsed_type_defn.span, "the type we defined was not pending")
        }
        Ok(type_id)
    }

    fn eval_type_expr(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
    ) -> K1Result<TypeId> {
        self.eval_type_expr_ext(type_expr_id, scope_id, EvalTypeExprContext::EMPTY)
    }

    fn eval_type_expr_ext(
        &mut self,
        type_expr_id: ParsedTypeExprId,
        scope_id: ScopeId,
        // The context is mostly about 'where we are' - where this type expression is located -
        // with respect to the source language. Is this a type definition? A function parameter
        // type? Are we immediately inside a function parameter?
        context: EvalTypeExprContext,
    ) -> K1Result<TypeId> {
        let base = match self.ast.type_exprs.get(type_expr_id) {
            ParsedTypeExpr::Builtin(_) => unreachable!(),
            ParsedTypeExpr::Struct(struct_defn) => {
                let struct_defn = *struct_defn;
                let kind = match struct_defn.record_kind {
                    parse::ParsedRecordKind::Struct => RecordKind::Struct,
                    parse::ParsedRecordKind::Union => RecordKind::Union,
                    parse::ParsedRecordKind::Packed => RecordKind::Packed,
                };
                let mut fields: List<StructTypeField, TypedProgram> =
                    self.mem.new_list(struct_defn.fields.len());
                for ast_field in self.ast.mem.getn(struct_defn.fields) {
                    if let Some(existing_field) = fields.iter().find(|f| f.name == ast_field.name) {
                        kbail!(
                            self,
                            struct_defn.span,
                            "Duplicate field name '{}' in {}",
                            existing_field.name,
                            struct_defn.record_kind.kind_name(),
                        );
                    }
                    let ty = self.eval_type_expr_ext(
                        ast_field.type_expr,
                        scope_id,
                        context.descended(),
                    )?;
                    fields.push(StructTypeField {
                        name: ast_field.name,
                        type_id: ty,
                        span: ast_field.name_span,
                    })
                }

                let struct_defn =
                    Type::Struct(StructType { fields: fields.to_slice(), record_kind: kind });
                let type_id = self.add_type_anon(struct_defn);

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
                // Rewrite the sugar and compile the parsed
                let parsed_ty_app = ParsedTypeExpr::TypeApplication(parse::TypeApplication {
                    span: opt.span,
                    name: QIdent::naked(self.ast.idents.b.opt, opt.span),
                    args: self.ast.mem.pushn(&[NamedTypeArg {
                        name: None,
                        type_expr: Some(opt.base),
                        span: opt.span,
                    }]),
                });
                let parsed_ty_app_id = self.ast.type_exprs.add(parsed_ty_app);
                self.eval_type_expr(parsed_ty_app_id, scope_id)
            }
            ParsedTypeExpr::Reference(r) => {
                let inner_ty = self.eval_type_expr_ext(r.base, scope_id, context.descended())?;
                if let Type::Function(_) = self.types.get(inner_ty) {
                    let function_pointer_type = self.add_function_pointer_type(inner_ty);
                    Ok(function_pointer_type)
                } else {
                    let type_id = self.add_reference_type(inner_ty);
                    Ok(type_id)
                }
            }
            ParsedTypeExpr::Array(arr) => {
                let arr = *arr;
                let element_type =
                    self.eval_type_expr_ext(arr.element_type, scope_id, context.descended())?;

                let size_type_id =
                    self.eval_type_expr_ext(arr.size_expr, scope_id, context.descended())?;

                let Some(static_type) = self.get_static_type_of_type(size_type_id) else {
                    kbail!(self, arr.span, "array size must be a static type");
                };
                if static_type.family_type_id != I64_TYPE_ID {
                    kbail!(
                        self,
                        arr.span,
                        "array size must be an int; got {}",
                        static_type.family_type_id
                    );
                }

                let array_type = Type::Array(ArrayType { element_type, size_type: size_type_id });
                let type_id = self.add_type_anon(array_type);
                Ok(type_id)
            }
            ParsedTypeExpr::Sum(sum) => {
                let sum = *sum;
                let variant_count = sum.variants.len();
                if variant_count == 0 {
                    kbail!(self, sum.span, "either must have at least one variant");
                }

                let tag_type = match sum.tag_type {
                    None => {
                        const U8_MAX_VARIANTS: u32 = u8::MAX as u32 + 1;
                        const MAX_VARIANTS: u32 = u16::MAX as u32 + 1;
                        let min_viable = match variant_count {
                            c if c <= U8_MAX_VARIANTS => IntegerType::U8,
                            c if c <= MAX_VARIANTS => IntegerType::U16,
                            _ => {
                                kbail!(
                                    self,
                                    sum.span,
                                    "sum cannot have more than {MAX_VARIANTS} variants"
                                );
                            }
                        };
                        min_viable
                    }
                    Some(tag_type_expr) => {
                        let tag_type = self.eval_type_expr(tag_type_expr, scope_id)?;
                        match self.types.get(tag_type) {
                            Type::Integer(int_type) => *int_type,
                            _ => {
                                kbail!(
                                    self,
                                    self.ast.get_type_expr_span(tag_type_expr),
                                    "Must be an integer type"
                                );
                            }
                        }
                    }
                };

                let has_payloads =
                    self.ast.mem.getn(sum.variants).iter().any(|v| v.payload.is_some());
                if has_payloads {
                    let mut variants: List<TypedSumVariant, _> = self.mem.new_list(variant_count);
                    let mut next_tag = tag_type.zero();
                    for (index, v) in self.ast.mem.getn(sum.variants).iter().enumerate() {
                        let payload_type_id = match &v.payload {
                            None => None,
                            Some(payload_type_expr) => {
                                let type_id = self.eval_type_expr_ext(
                                    *payload_type_expr,
                                    scope_id,
                                    context.descended(),
                                )?;
                                Some(type_id)
                            }
                        };
                        let tag_int = match v.explicit_value {
                            None => next_tag,
                            Some(explicit_value) => {
                                let parsed = self.eval_integer_value(
                                    explicit_value.text_span,
                                    Some(tag_type.type_id()),
                                )?;
                                parsed
                            }
                        };
                        if let Some(existing) = variants.iter().find(|v| v.tag_value == tag_int) {
                            kbail!(
                                self,
                                v.name_span,
                                "Duplicate tag value: {}",
                                existing.tag_value
                            );
                        }
                        let variant = TypedSumVariant {
                            name: v.tag_name,
                            index: index as u32,
                            payload: payload_type_id,
                            tag_value: tag_int,
                            name_span: v.name_span,
                        };

                        next_tag = tag_int.incr();
                        variants.push(variant);
                    }

                    debug!(
                        "variants and tags: {}",
                        variants
                            .iter()
                            .map(|v| format!("  {} {}", self.ident_str(v.name), v.tag_value))
                            .join("\n")
                    );
                    let sum_type = Type::Sum(SumType { variants: variants.to_slice(), tag_type });
                    let sum_type_id = self.add_type_anon(sum_type);
                    Ok(sum_type_id)
                } else {
                    let mut member_values: List<ScalarEnumValue, _> =
                        self.mem.new_list(variant_count);
                    let mut next_tag = tag_type.zero();
                    for v in self.ast.mem.getn(sum.variants).iter() {
                        let tag_value = match v.explicit_value {
                            None => next_tag,
                            Some(explicit_value) => {
                                let parsed = self.eval_integer_value(
                                    explicit_value.text_span,
                                    Some(tag_type.type_id()),
                                )?;
                                parsed
                            }
                        };
                        if let Some(existing) =
                            member_values.iter().find(|v| v.int_value == tag_value)
                        {
                            kbail!(
                                self,
                                v.name_span,
                                "Duplicate enum value: {}",
                                existing.int_value
                            );
                        }
                        let variant = ScalarEnumValue {
                            name: v.tag_name,
                            int_value: tag_value,
                            name_span: v.name_span,
                        };
                        next_tag = tag_value.incr();
                        member_values.push(variant);
                    }

                    debug!(
                        "members and tags: {}",
                        member_values
                            .iter()
                            .map(|v| format!("  {} {}", self.ident_str(v.name), v.int_value))
                            .join("\n")
                    );
                    let enum_type = Type::Enum(ScalarEnumType {
                        member_values: member_values.to_slice(),
                        int_type: tag_type,
                    });
                    let sum_type_id = self.add_type_anon(enum_type);
                    Ok(sum_type_id)
                }
            }
            ParsedTypeExpr::MemberAccess(acc) => {
                let is_dot = matches!(acc.member_kind, parse::TypeMemberAccessKind::Dot);
                let acc = *acc;
                let base_type = self.eval_type_expr_ext(acc.base, scope_id, context.descended())?;
                if let Some(spec_info) = self.get_instance_info(base_type) {
                    let generic = self.types.get(spec_info.generic_parent).expect_generic();
                    if let Some(matching_type_var_pos) = self
                        .mem
                        .getn(generic.params)
                        .iter()
                        .position(|tp| self.get_type_parameter(*tp).name == acc.member_name)
                    {
                        let type_arg_type_id =
                            spec_info.type_args.as_slice(&self.mem)[matching_type_var_pos];
                        return Ok(type_arg_type_id);
                    }
                }
                match self.types.get(base_type) {
                    // You can do dot access on sums to get their variant payloads
                    Type::Sum(sum) => {
                        let Some(matching_variant) =
                            self.sum_variant_by_name(sum.variants, acc.member_name)
                        else {
                            kbail!(
                                self,
                                acc.span,
                                "Variant '{}' does not exist on either '{}'",
                                acc.member_name,
                                base_type
                            );
                        };
                        let Some(payload) = matching_variant.payload else {
                            kbail!(
                                self,
                                acc.span,
                                "Variant '{}' has no payload type",
                                acc.member_name
                            );
                        };
                        if is_dot {
                            kbail!(
                                self,
                                acc.span,
                                "Use :, not ., to access this variant's data type"
                            );
                        }
                        Ok(payload)
                    }
                    // You can do dot access on structs to get their members!
                    Type::Struct(s) => {
                        let Some(field) = s.find_field(&self.mem, acc.member_name) else {
                            kbail!(
                                self,
                                acc.span,
                                "Field {} does not exist on struct {}",
                                acc.member_name,
                                base_type
                            );
                        };
                        if !is_dot {
                            kbail!(
                                self,
                                acc.span,
                                "Use ., not :, to access this struct member's data type"
                            );
                        }
                        Ok(field.1.type_id)
                    }
                    // You can do dot access on References to get out their 'value' types
                    Type::Reference(r) => {
                        if acc.member_name != self.ast.idents.b.value {
                            return make_fail_ast_id(
                                &self.ast,
                                "Invalid member access on Optional type; try '.value'",
                                type_expr_id.into(),
                            );
                        }
                        Ok(r.inner_type)
                    }
                    Type::Function(fun) => {
                        let member_name = self.ast.idents.get_string(acc.member_name);
                        match member_name {
                            "return" => Ok(fun.return_type),
                            _other => {
                                if let Some(param) = self
                                    .mem
                                    .getn(fun.logical_params())
                                    .iter()
                                    .find(|p| p.name == acc.member_name)
                                {
                                    Ok(param.type_id)
                                } else {
                                    return make_fail_ast_id(
                                        &self.ast,
                                        &format!("Function has no parameter named {}", member_name),
                                        type_expr_id.into(),
                                    );
                                }
                            }
                        }
                    }
                    Type::Array(array_type) => {
                        let member_name = self.ast.idents.get_string(acc.member_name);
                        match member_name {
                            "element" => Ok(array_type.element_type),
                            _ => {
                                kbail!(
                                    self,
                                    acc.span,
                                    "Array type has no member named {}; try '.element'",
                                    member_name
                                );
                            }
                        }
                    }
                    _ => {
                        kbail!(
                            self,
                            acc.span,
                            "Type '{}' has no {} member named {}",
                            base_type,
                            if is_dot { "." } else { ":" },
                            self.ast.idents.get_string(acc.member_name)
                        );
                    }
                }
            }
            ParsedTypeExpr::Function(fun_type) => {
                let fun_type = *fun_type;
                let mut params: List<FnParamType, _> = self.mem.new_list(fun_type.params.len());

                for (index, param) in self.ast.mem.getn(fun_type.params).iter().enumerate() {
                    let type_id = self.eval_type_expr(*param, scope_id)?;

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
                        is_macro_code: false,
                    });
                }
                let return_type = self.eval_type_expr(fun_type.return_type, scope_id)?;
                let params_handle = params.to_slice();
                let function_type_id = self.add_type_anon(Type::Function(FunctionType {
                    physical_params: params_handle,
                    is_lambda: false,
                    return_type,
                    abi_mode: AbiMode::Internal,
                }));
                Ok(function_type_id)
            }
            ParsedTypeExpr::TypeOf(tof) => {
                let expr = self.eval_expr(tof.target_expr, EvalExprContext::make(scope_id))?;
                let ty = self.exprs.get_type(expr);
                Ok(ty)
            }
            ParsedTypeExpr::SomeQuant(quant) => {
                if !context.is_direct_function_parameter {
                    kbail!(
                        self,
                        quant.span,
                        "some quantifier is only allowed in function parameters"
                    );
                }
                let span = quant.span;
                let inner = self.eval_type_expr(quant.inner_type_expr, scope_id)?;
                let Type::Function(_function_type) = self.types.get(inner) else {
                    kbail!(self, span, "Expected a function type following 'some'");
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
                    kbail!(
                        self,
                        parsed_static.span,
                        "Static type cannot appear inside static type"
                    );
                }
                if context.is_inside_type_definition_rhs {
                    kbail!(
                        self,
                        parsed_static.span,
                        "Static type cannot appear in type definitions"
                    );
                }
                let parsed_static = *parsed_static;
                let inner_type_id = self.eval_type_expr_ext(
                    parsed_static.family_type_expr,
                    scope_id,
                    EvalTypeExprContext { is_inside_static_type: true, ..context },
                )?;
                if let Type::StaticValue(_) = self.types.get(inner_type_id) {
                    kbail!(
                        self,
                        parsed_static.span,
                        "Value type cannot be nested inside another value type"
                    );
                };
                let value_type = StaticValueType { family_type_id: inner_type_id, value_id: None };
                let static_type_id = self.add_type_anon(Type::StaticValue(value_type));
                Ok(static_type_id)
            }
            ParsedTypeExpr::StaticLiteral(parsed_literal) => {
                let parsed_literal = *parsed_literal;
                let (static_value_id, inner_type_id) =
                    self.literal_to_static_value_and_type(&parsed_literal, scope_id, None)?;
                let static_type_id = self.add_value_type(inner_type_id, Some(static_value_id));
                Ok(static_type_id)
            }
            ParsedTypeExpr::ExecStatic(es) => {
                let es = *es;
                let expr_ctx = EvalExprContext::make(scope_id).with_is_static(true);
                let resulting_value_id = self.execute_static_expr(es.body_expr, expr_ctx, &[])?;
                let resulting_type_id = self.get_static_value_type(resulting_value_id);
                match self.types.get(resulting_type_id) {
                    Type::Struct(_) if resulting_type_id == self.builtin_types.type_id() => {
                        // The user's code returned a type-id struct. The value inside is a type id
                        // and that's our result
                        let struct_value =
                            self.static_values.get(resulting_value_id).as_struct().unwrap();
                        let first_field_id = self.static_values.get_slice(struct_value.fields)[0];
                        let StaticValue::Int(TypedIntValue::U64(type_id_u64)) =
                            self.static_values.get(first_field_id)
                        else {
                            kbail!(self, es.span, "[ice] type-id should contain a u64")
                        };
                        if *type_id_u64 == 0 {
                            kbail!(self, es.span, "[ice] type-id should not be zero")
                        }
                        let type_id = TypeId::from_u32(*type_id_u64 as u32).unwrap();
                        match self.types.get_opt(type_id) {
                            None => kbail!(self, es.span, "Unknown type id: "),
                            Some(_) => Ok(type_id),
                        }
                    }
                    Type::StaticValue(_svt) => {
                        // They returned an actual statically-typed value already
                        Ok(resulting_type_id)
                    }
                    _ => {
                        // For any other type, we take the type of their interned/baked/static
                        // value. For example, the type of 42u8 itself
                        let static_type_id =
                            self.add_type_anon(Type::StaticValue(StaticValueType {
                                family_type_id: resulting_type_id,
                                value_id: Some(resulting_value_id),
                            }));
                        Ok(static_type_id)
                    }
                }
            }
        }?;
        Ok(base)
    }

    fn literal_to_static_value_and_type(
        &mut self,
        parsed_literal: &ParsedLiteral,
        scope_id: ScopeId,
        expected_type_hint: Option<TypeId>,
    ) -> K1Result<(StaticValueId, TypeId)> {
        match parsed_literal {
            ParsedLiteral::Char(byte, _) => {
                Ok((self.static_values.add(StaticValue::Char(*byte)), CHAR_TYPE_ID))
            }
            ParsedLiteral::Bool(b, _) => {
                Ok((self.static_values.add(StaticValue::Bool(*b)), BOOL_TYPE_ID))
            }
            ParsedLiteral::String(s, _) => {
                Ok((self.static_values.add(StaticValue::String(*s)), self.builtin_types.string()))
            }
            ParsedLiteral::Numeric(numeric) => {
                // Parse the numeric literal and determine its type and value
                // Use the expected type hint if provided (e.g., i64 for array sizes)
                let eval_context =
                    EvalExprContext::make(scope_id).with_expected_type(expected_type_hint);
                let num_static_value_id =
                    self.eval_numeric_value(numeric.text_span, eval_context)?;
                Ok((num_static_value_id, self.get_static_value_type(num_static_value_id)))
            }
        }
    }

    /// Temporary home for our type operators until I decide on syntax
    fn detect_and_eval_type_operator(
        &mut self,
        ty_app_id: ParsedTypeExprId,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<Option<TypeId>> {
        let ParsedTypeExpr::TypeApplication(ty_app) = self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };
        if !ty_app.name.path.is_empty() {
            return Ok(None);
        }
        let ty_app = *ty_app;
        match self.ident_str(ty_app.name.name) {
            "dyn" => {
                if ty_app.args.len() != 1 {
                    kbail!(self, ty_app.span, "Expected 1 type parameter for dyn");
                }
                let Some(inner_type_expr_id) = self.ast.mem.get_nth(ty_app.args, 0).type_expr
                else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                // dyn over an ability: the inner expr names an ability, which is not
                // a type, so it must be intercepted before type evaluation
                if let ParsedTypeExpr::TypeApplication(inner_app) =
                    self.ast.type_exprs.get(inner_type_expr_id)
                {
                    let inner_app = *inner_app;
                    if self.name_resolves_to_ability(scope_id, &inner_app.name)? {
                        let ability_expr = self.ast.mem.push_h(ParsedAbilityExpr {
                            name: inner_app.name,
                            arguments: inner_app.args,
                            span: inner_app.span,
                        });
                        let signature = self.eval_ability_expr(ability_expr, false, scope_id)?;
                        let object_type =
                            self.eval_dyn_ability_object_type(signature, inner_app.span)?;
                        return Ok(Some(object_type));
                    }
                }
                let inner =
                    self.eval_type_expr_ext(inner_type_expr_id, scope_id, context.descended())?;
                if self.types.get(inner).as_function().is_none() {
                    kbail!(
                        self,
                        ty_app.span,
                        "Expected function type or ability signature for dyn"
                    );
                }
                let new_function_type = self.add_lambda_env_to_function_type(inner);
                let lambda_object_type =
                    self.add_lambda_object(new_function_type, ty_app_id.into());

                Ok(Some(lambda_object_type))
            }
            "_struct_combine" => {
                if ty_app.args.len() != 2 {
                    kbail!(self, ty_app.span, "Expected 2 type parameters for _struct_combine");
                }
                let args = self.ast.mem.getn(ty_app.args);
                let Some(arg1_expr) = args[0].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

                let struct1 = *self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let struct2 = *self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let record_kind = struct1.record_kind;
                if struct2.record_kind != record_kind {
                    kbail!(
                        self,
                        ty_app.span,
                        "Cannot combine a {} and a {}",
                        struct1.record_kind.kind_name(),
                        struct2.record_kind.kind_name(),
                    );
                }

                let mut combined_fields =
                    self.mem.new_list(struct1.fields.len() + struct2.fields.len());
                combined_fields.extend(self.mem.getn(struct1.fields));
                for field in self.mem.getn(struct2.fields).iter() {
                    let collision = combined_fields.iter().find(|f| f.name == field.name);
                    if let Some(collision) = collision {
                        if collision.type_id != field.type_id {
                            kbail!(
                                self,
                                ty_app.span,
                                "Field '{}' has conflicting types in the two structs",
                                self.ident_str(field.name).blue()
                            );
                        }
                    }
                    combined_fields.push(*field);
                }

                let new_struct =
                    Type::Struct(StructType { fields: combined_fields.to_slice(), record_kind });
                let type_id = self.add_type_anon(new_struct);

                Ok(Some(type_id))
            }
            "_struct_remove" => {
                if ty_app.args.len() != 2 {
                    kbail!(self, ty_app.span, "Expected 2 type parameters for _struct_remove");
                }
                let args = self.ast.mem.getn(ty_app.args);
                let Some(arg1_expr) = args[0].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let Some(arg2_expr) = args[1].type_expr else {
                    kbail!(self, ty_app.span, "Wildcard type `_` not accepted here");
                };
                let arg1 = self.eval_type_expr_ext(arg1_expr, scope_id, context.descended())?;
                let arg2 = self.eval_type_expr_ext(arg2_expr, scope_id, context.descended())?;

                let struct1 = self
                    .types
                    .get(arg1)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let struct2 = self
                    .types
                    .get(arg2)
                    .as_struct()
                    .ok_or_else(|| kerr!(self, ty_app.span, "Expected struct"))?;
                let record_kind = struct1.record_kind;
                if struct2.record_kind != record_kind {
                    kbail!(
                        self,
                        ty_app.span,
                        "Cannot combine a {} and a {}",
                        struct1.record_kind.kind_name(),
                        struct2.record_kind.kind_name(),
                    );
                }
                let struct2_fields = self.mem.getn(struct2.fields);
                let new_fields = self
                    .mem
                    .getn(struct1.fields)
                    .iter()
                    .filter(|f| !struct2_fields.iter().any(|sf| sf.name == f.name))
                    .cloned();
                let new_fields = self.mem.pushn_iter(new_fields);

                let new_struct = Type::Struct(StructType { fields: new_fields, record_kind });
                let type_id = self.add_type_anon(new_struct);
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
    ) -> K1Result<TypeId> {
        let ParsedTypeExpr::TypeApplication(ty_app) = *self.ast.type_exprs.get(ty_app_id) else {
            panic_at_disco!("Expected TypeApplication")
        };

        match self.find_type_namespaced(scope_id, &ty_app.name)? {
            Some((type_id, _)) => match self.types.get(type_id) {
                Type::Generic(g) => {
                    if ty_app.args.len() != g.params.len() {
                        kbail!(
                            self,
                            ty_app.span,
                            "Type {} expects {} type arguments, got {}",
                            &ty_app.name,
                            g.params.len(),
                            ty_app.args.len()
                        );
                    }
                    let g_params = g.params;
                    let (type_arguments_slice, _subst_pairs) = self
                        .check_type_args_against_params(
                            g_params,
                            self.ast.mem.getn(ty_app.args),
                            scope_id,
                            context,
                        )?;
                    let instantiated_type_id =
                        self.instantiate_generic_type(type_id, self.mem.getn(type_arguments_slice));

                    self.emit_ls_entity(
                        ty_app.name.name_span,
                        LsEntityKind::Type { type_id, applied_type_id: Some(instantiated_type_id) },
                    );
                    Ok(instantiated_type_id)
                }
                _other => {
                    if !ty_app.args.is_empty() {
                        kbail!(
                            self,
                            ty_app.span,
                            "Type {} is not generic, but got {} type arguments",
                            &ty_app.name,
                            ty_app.args.len()
                        );
                    }
                    let resolved_type_id = self.get_type_id_resolved(type_id, scope_id);
                    self.emit_ls_entity(
                        ty_app.name.name_span,
                        LsEntityKind::Type { type_id, applied_type_id: None },
                    );
                    Ok(resolved_type_id)
                }
            },
            None => match self.find_pending_type_namespaced(scope_id, &ty_app.name)? {
                None => {
                    if ty_app.args.is_empty() {
                        match self.find_function_namespaced(scope_id, &ty_app.name)? {
                            Some(function_id) => Ok(self.get_function(function_id).type_id),
                            None => match self
                                .resolve_qident_to_constant_type(scope_id, &ty_app.name)?
                            {
                                Some(static_type_id) => Ok(static_type_id),
                                None => Err(kerr!(
                                    self,
                                    ty_app.name.name_span,
                                    "Type '{}' not found",
                                    &ty_app.name,
                                )),
                            },
                        }
                    } else {
                        if let Some(opaque_type_id) = self.handle_opaque_tyapp(&ty_app)? {
                            Ok(opaque_type_id)
                        } else if let Some(vector_type_id) =
                            self.handle_vector_tyapp(&ty_app, scope_id, context)?
                        {
                            Ok(vector_type_id)
                        } else {
                            Err(kerr!(
                                self,
                                ty_app.name.name_span,
                                "Type '{}' not found",
                                &ty_app.name
                            ))
                        }
                    }
                }
                Some((pending_defn, _scope_id)) => {
                    let stack_entry = self
                        .type_defn_context
                        .stack
                        .iter()
                        .find(|e| e.parsed_id == pending_defn.parsed_id)
                        .map(|e| e.reserved_type_id);
                    if let Some(reserved_type_id) = stack_entry {
                        let params = self.ast.get_type_defn(pending_defn.parsed_id).type_params;
                        if ty_app.args.len() != params.len() {
                            kbail!(
                                self,
                                ty_app.span,
                                "Type {} expects {} type arguments, got {}",
                                &ty_app.name,
                                params.len(),
                                ty_app.args.len()
                            );
                        }
                        if ty_app.args.is_empty() {
                            self.type_defn_context.recursive_mentions.push(reserved_type_id);
                            self.emit_ls_entity(
                                ty_app.name.name_span,
                                LsEntityKind::Type {
                                    type_id: reserved_type_id,
                                    applied_type_id: None,
                                },
                            );
                            Ok(reserved_type_id)
                        } else {
                            // A recursive application like the `node[t]` inside node's own
                            // definition: the generic doesn't exist yet, so reserve the
                            // instance's id now; it is filled once the defn cluster completes
                            let mut type_arguments: SV4<TypeId> = smallvec![];
                            for parsed_arg in self.ast.mem.getn(ty_app.args) {
                                let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                                    kbail!(
                                        self,
                                        parsed_arg.span,
                                        "Wildcard _ type not accepted here"
                                    );
                                };
                                let arg_type_id = self.eval_type_expr_ext(
                                    parsed_arg_expr,
                                    scope_id,
                                    context.descended(),
                                )?;
                                if self.recursive_arg_violates_uniformity(arg_type_id) {
                                    kbail!(
                                        self,
                                        parsed_arg.span,
                                        "Polymorphic recursion is not supported: a recursive type argument must be a bare type parameter or contain no type parameters, got {}",
                                        arg_type_id
                                    );
                                }
                                type_arguments.push(arg_type_id);
                            }
                            let instance_type_id = self.get_or_reserve_recursive_instance(
                                reserved_type_id,
                                &type_arguments,
                            );
                            self.emit_ls_entity(
                                ty_app.name.name_span,
                                LsEntityKind::Type {
                                    type_id: reserved_type_id,
                                    applied_type_id: Some(instance_type_id),
                                },
                            );
                            Ok(instance_type_id)
                        }
                    } else {
                        debug!(
                            "Evaluating {} inside {} on demand",
                            self.ident_str(self.ast.get_type_defn(pending_defn.parsed_id).name),
                            self.scope_id_to_string(scope_id)
                        );

                        let _result =
                            self.eval_type_defn(pending_defn.parsed_id, pending_defn.scope_id)?;

                        // Just re-call this function from the top now that the type exists. (hack? idk)
                        self.eval_type_application(ty_app_id, scope_id, context)
                    }
                }
            },
        }
    }

    fn instantiate_generic_type(
        &mut self,
        generic_type: TypeId,
        type_arguments: &[TypeId],
    ) -> TypeId {
        match self.get_specialization(generic_type, type_arguments) {
            Some(existing) => existing,
            None => {
                let args = TypeArgs::from_slice_in(type_arguments, &mut self.mem);
                self.instantiate_generic_type_miss(generic_type, args)
            }
        }
    }

    fn instantiate_generic_type_miss(
        &mut self,
        generic_type: TypeId,
        type_arguments: TypeArgs,
    ) -> TypeId {
        let gen_type = self.types.get(generic_type).expect_generic();
        debug_assert!(gen_type.params.len() == type_arguments.len());
        let defn_info = self.get_defn_info(generic_type).unwrap();
        // Note: This is where we'd check constraints on the pairs:
        // that each passed params meets the constraints of the generic param
        let mut substitution_pairs: SV8<TypeSubstitutionPair> = smallvec![];
        for (type_param, passed_type_arg) in
            self.mem.getn(gen_type.params).iter().zip(type_arguments.as_slice(&self.mem))
        {
            substitution_pairs
                .push(TypeSubstitutionPair { from: *type_param, to: *passed_type_arg });
        }
        let inner = gen_type.inner;

        // For a recursive generic, register the result id in the specialization cache before
        // walking the template, so the recursive mention inside it resolves to this id
        let reserved_result = if defn_info.recursive {
            Some(self.reserve_instance_id(generic_type, type_arguments))
        } else {
            None
        };
        let specialized_type = self.substitute_in_type_ext(
            inner,
            &substitution_pairs,
            Some(generic_type),
            Some(defn_info),
        );
        let result_type = match reserved_result {
            None => {
                self.insert_specialization(generic_type, type_arguments, specialized_type);
                specialized_type
            }
            Some(reserved) => {
                let t = *self.types.get(specialized_type);
                self.set_type(
                    reserved,
                    t,
                    Some(GenericInstanceInfo {
                        generic_parent: generic_type,
                        type_args: type_arguments,
                    }),
                    Some(defn_info),
                );
                self.discard_type_if_last(specialized_type);
                reserved
            }
        };
        if log::log_enabled!(log::Level::Debug) {
            eprintln!(
                "instantiated\n{} with params\n{} got expanded type:\n{}\n\n",
                self.type_id_to_string_ext(inner, dump::TypeDisplayMode::Expand),
                self.pretty_print_types(type_arguments.as_slice(&self.mem), ", "),
                self.type_id_to_string_ext(result_type, dump::TypeDisplayMode::Expand)
            );
        }
        result_type
    }

    /// A recursive application's args must be bare type parameters or contain no type
    /// parameters at all; anything else is polymorphic recursion, whose instantiations
    /// would never terminate
    fn recursive_arg_violates_uniformity(&self, arg: TypeId) -> bool {
        self.type_variable_counts.get(arg).type_parameter_count > 0
            && !matches!(self.types.get(arg), Type::TypeParameter(_))
    }

    fn get_or_reserve_recursive_instance(
        &mut self,
        generic_parent: TypeId,
        type_args: &[TypeId],
    ) -> TypeId {
        if let Some(existing) = self.get_specialization(generic_parent, type_args) {
            return existing;
        }
        let args = TypeArgs::from_slice_in(type_args, &mut self.mem);
        let type_id = self.reserve_instance_id(generic_parent, args);
        self.type_defn_context.pending_instances.push(PendingRecursiveInstance {
            type_id,
            generic_parent,
            type_args: args,
        });
        self.type_defn_context.recursive_mentions.push(type_id);
        type_id
    }

    /// Runs when the type defn stack empties: every defn in the cluster is complete, so fill
    /// the reserved recursive instances by substitution, then check the cluster for infinite
    /// (indirection-free) cycles
    fn finish_type_defn_cluster(&mut self) {
        let pending = std::mem::take(&mut self.type_defn_context.pending_instances);
        for p in &pending {
            let gen_type = self.types.get(p.generic_parent).expect_generic();
            let gen_params = gen_type.params;
            let inner = gen_type.inner;
            let defn_info = self.get_defn_info(p.generic_parent).unwrap();
            let defn_span = self.ast.get_span_for_id(defn_info.ast_id);
            for arg in p.type_args.as_slice(&self.mem) {
                if self.recursive_arg_violates_uniformity(*arg) {
                    self.report(kerr!(self,
                        defn_span,
                        "Polymorphic recursion is not supported: a recursive type argument must be a bare type parameter or contain no type parameters, got {}",
                        *arg));
                }
            }
            let mut substitution_pairs: SV8<TypeSubstitutionPair> = smallvec![];
            for (param, arg) in
                self.mem.getn(gen_params).iter().zip(p.type_args.as_slice(&self.mem))
            {
                substitution_pairs.push(spair! { *param => *arg });
            }
            let specialized = self.substitute_in_type_ext(
                inner,
                &substitution_pairs,
                Some(p.generic_parent),
                Some(defn_info),
            );
            let t = *self.types.get(specialized);
            self.set_type(
                p.type_id,
                t,
                Some(GenericInstanceInfo {
                    generic_parent: p.generic_parent,
                    type_args: p.type_args,
                }),
                Some(defn_info),
            );
        }

        if !self.type_defn_context.recursive_mentions.is_empty() {
            let completed = std::mem::take(&mut self.type_defn_context.completed);
            // Outermost defn first; one cycle report is enough for the whole cluster
            let mut seen: List<TypeId, MemTmp> = self.tmp.new_list(16);
            for (type_id, span) in completed.iter().rev() {
                debug_assert!(seen.is_empty());
                if let Some(cycled_type_id) = self.check_type_finite_rec(*type_id, false, &mut seen)
                {
                    self.report(kerr!(self,
                        *span,
                        "This type has an infinite size due to a cycle; {} was mentioned inside {} with no indirection",
                        cycled_type_id,
                        *type_id,
                    ));
                    break;
                }
            }
        }
        self.type_defn_context.reset()
    }

    fn check_type_args_against_params(
        &mut self,
        params: TypeIdSlice,
        parsed_args: &[NamedTypeArg],
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<(TypeIdSlice, TmpList<TypeSubstitutionPair>)> {
        let mut type_arguments = self.mem.new_list(params.len());
        let mut subst_pairs: List<TypeSubstitutionPair, MemTmp> = self.tmp.new_list(params.len());
        for (param, parsed_arg) in self.mem.getn(params).iter().zip(parsed_args) {
            let Some(parsed_arg_expr) = parsed_arg.type_expr else {
                kbail!(self, parsed_arg.span, "Wildcard _ type not accepted here");
            };
            let arg_type_id =
                self.eval_type_expr_ext(parsed_arg_expr, scope_id, context.descended())?;
            subst_pairs.push(spair! { *param => arg_type_id });
            type_arguments.push(arg_type_id);
        }

        // Repeat the loop, this time checking constraints
        // now that we have all the pairs
        for ((param, parsed_arg), arg_type) in
            self.mem.getn(params).iter().zip(parsed_args).zip(type_arguments.as_slice())
        {
            self.check_type_constraints(
                self.get_type_parameter(*param).name,
                *param,
                *arg_type,
                subst_pairs.as_slice(),
                scope_id,
                parsed_arg.span,
            )?;
        }

        Ok((type_arguments.to_slice(), subst_pairs))
    }

    fn resolve_qident_to_constant_type(
        &mut self,
        scope_id: ScopeId,
        name: &QIdent,
    ) -> K1Result<Option<TypeId>> {
        let global_id = match self.find_variable_namespaced(scope_id, name)? {
            Some((variable_id, _scope)) => match self.variables.get(variable_id).global_id() {
                Some(global_id) => global_id,
                None => return Ok(None),
            },
            None => match self.find_pending_global_namespaced(scope_id, name)? {
                Some((parsed_id, defn_scope)) => {
                    if self.declare_global(parsed_id, defn_scope)?.is_none() {
                        return Ok(None);
                    }
                    *self.global_ast_mappings.get(&parsed_id).unwrap()
                }
                None => return Ok(None),
            },
        };
        if self.globals.get(global_id).initial_value.is_pending() {
            let ast_id = self.globals.get(global_id).ast_id;
            self.eval_global_body(ast_id)?;
        }
        let g = self.globals.get(global_id);
        if !g.is_constant {
            return Ok(None);
        }
        let Some(static_value_id) = g.initial_value.as_value() else {
            return Ok(None);
        };
        let inner_type_id = self.get_static_value_type(static_value_id);
        Ok(Some(self.add_value_type(inner_type_id, Some(static_value_id))))
    }

    fn handle_opaque_tyapp(&mut self, ty_app: &parse::TypeApplication) -> K1Result<Option<TypeId>> {
        if self.ident_str(ty_app.name.name) != "opaque" {
            return Ok(None);
        }
        if !ty_app.name.path.is_empty() {
            kbail!(
                self,
                ty_app.span,
                "Expected 'opaque' with no namespace, got '{}'",
                &ty_app.name
            );
        }
        if ty_app.args.len() != 2 {
            kbail!(
                self,
                ty_app.span,
                "Expected 2 type parameters for opaque, got {}",
                ty_app.args.len()
            );
        }
        let Some(size_expr) = self.ast.mem.get_nth(ty_app.args, 0).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let Some(align_expr) = self.ast.mem.get_nth(ty_app.args, 1).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Numeric(size_lit)) =
            *self.ast.type_exprs.get(size_expr)
        else {
            kbail!(self, ty_app.span, "Expected a static literal for opaque size");
        };
        let ParsedTypeExpr::StaticLiteral(ParsedLiteral::Numeric(align_lit)) =
            *self.ast.type_exprs.get(align_expr)
        else {
            kbail!(self, ty_app.span, "Expected a static literal for opaque alignment");
        };
        let TypedIntValue::U32(size) =
            self.eval_integer_value(size_lit.text_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, size_lit.span, "Expected a u32 value for opaque size");
        };
        let TypedIntValue::U32(align) =
            self.eval_integer_value(align_lit.text_span, Some(IntegerType::U32.type_id()))?
        else {
            kbail!(self, align_lit.span, "Expected a u32 value for opaque alignment");
        };

        if align == 0 || !align.is_power_of_two() || align > 128 {
            kbail!(
                self,
                align_lit.span,
                "Alignment must be a non-zero power of two, not exceeding 128, got {}",
                align
            );
        }

        let opaque_type = self.add_type(Type::Opaque(OpaqueType { size, align }), None, None);
        Ok(Some(opaque_type))
    }

    fn handle_vector_tyapp(
        &mut self,
        ty_app: &parse::TypeApplication,
        scope_id: ScopeId,
        context: EvalTypeExprContext,
    ) -> K1Result<Option<TypeId>> {
        if ty_app.name.name != self.ast.idents.b.vector {
            return Ok(None);
        }
        if !ty_app.name.path.is_empty() {
            kbail!(
                self,
                ty_app.span,
                "Expected 'vector' with no namespace, got '{}'",
                &ty_app.name
            );
        }
        if ty_app.args.len() != 2 {
            kbail!(
                self,
                ty_app.span,
                "Expected 2 type parameters for vector, got {}",
                ty_app.args.len()
            );
        }
        let Some(element_expr) = self.ast.mem.get_nth(ty_app.args, 0).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let Some(size_expr) = self.ast.mem.get_nth(ty_app.args, 1).type_expr else {
            kbail!(self, ty_app.span, "Wildcard _ type not accepted here");
        };
        let element_type = self.eval_type_expr_ext(element_expr, scope_id, context.descended())?;

        let size_type_id = self.eval_type_expr_ext(size_expr, scope_id, context.descended())?;

        let Some(static_type) = self.get_static_type_of_type(size_type_id) else {
            kbail!(self, ty_app.span, "Vector lane count must be a static type");
        };
        if static_type.family_type_id != I64_TYPE_ID {
            kbail!(
                self,
                ty_app.span,
                "Vector lane count must be an int; got {}",
                static_type.family_type_id
            );
        }

        self.validate_vector_parts(element_type, size_type_id, ty_app.span)?;

        let vector_type = Type::Vector(VectorType { element_type, size_type: size_type_id });
        Ok(Some(self.add_type_anon(vector_type)))
    }

    /// Checks whatever is concrete; abstract element/lane-count parts are checked
    /// again at instantiation sites (IR lowering re-validates for intrinsics)
    fn validate_vector_parts(
        &self,
        element_type: TypeId,
        size_type_id: TypeId,
        span: SpanId,
    ) -> K1Result<()> {
        let element_size = match self.types.get(element_type) {
            Type::Integer(it) => Some(it.width().bits() / 8),
            Type::Char => Some(1),
            Type::Float(ft) => Some(ft.size().bits() / 8),
            Type::TypeParameter(_) | Type::InferenceHole(_) | Type::FunctionTypeParameter(_) => {
                None
            }
            _ => {
                kbail!(
                    self,
                    span,
                    "Vector elements must be an int, char, or float type; got {}",
                    element_type
                );
            }
        };
        let Some(count) = self.get_type_as_i64(size_type_id) else {
            return Ok(());
        };
        #[allow(clippy::manual_range_contains)]
        if count < 2 || count > 64 || !(count as u64).is_power_of_two() {
            kbail!(
                self,
                span,
                "Vector lane count must be a power of two between 2 and 64; got {}",
                count
            );
        }
        if let Some(element_size) = element_size {
            let total = element_size as i64 * count;
            if total > 64 {
                kbail!(
                    self,
                    span,
                    "Vector total size must not exceed 64 bytes (512 bits); got {}",
                    total
                );
            }
        }
        Ok(())
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
    ) -> TypeId {
        // The empty substitution is the identity
        if substitution_pairs.is_empty()
            && generic_parent_to_attach.is_none()
            && defn_info_to_attach.is_none()
        {
            return type_id;
        }
        let mut from_kinds = SubstitutionFromKinds::default();
        for pair in substitution_pairs {
            let counts = self.type_variable_counts.get(pair.from);
            match (counts.inference_hole_count > 0, counts.type_parameter_count > 0) {
                (true, false) => from_kinds.holes = true,
                (false, true) => from_kinds.params = true,
                (true, true) => from_kinds.holes_and_params = true,
                (false, false) => from_kinds.other = true,
            }
        }
        let tmp_mark = self.tmp.mark();
        let res = self.substitute_in_type_ext_inner(
            type_id,
            substitution_pairs,
            from_kinds,
            generic_parent_to_attach,
            defn_info_to_attach,
        );
        self.tmp.reset_to(tmp_mark);
        res
    }

    fn substitute_in_type_ext_inner(
        &mut self,
        type_id: TypeId,
        substitution_pairs: &[TypeSubstitutionPair],
        from_kinds: SubstitutionFromKinds,
        generic_parent_to_attach: Option<TypeId>,
        defn_info_to_attach: Option<TypeDefnInfo>,
    ) -> TypeId {
        if generic_parent_to_attach.is_none()
            && defn_info_to_attach.is_none()
            && from_kinds.no_from_occurs_in(self.type_variable_counts.get(type_id))
        {
            return type_id;
        }

        // If this type is already a generic instance of something, just
        // re-specialize it on the right inputs. So find out what the new value
        // of each type param should be and call instantiate_generic_type
        //
        // This happens when specializing a type that contains an Opt[T], for example.
        // This lets us hit our cache as well
        if let Some(spec_info) = self.get_instance_info(type_id) {
            // A,   B,    T
            // int, bool, char
            // Opt[T] -> Opt[char]
            let generic_parent = spec_info.generic_parent;
            let original_args = spec_info.type_args;
            let mut new_type_args = self.tmp.new_list(original_args.len());
            let mut any_change = false;
            for prev_arg in original_args.as_slice(&self.mem) {
                let new_type = self.substitute_in_type_ext_inner(
                    *prev_arg,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_type != *prev_arg {
                    any_change = true;
                }
                new_type_args.push(new_type);
            }
            let parent_pending =
                self.type_defn_context.stack.iter().any(|e| e.reserved_type_id == generic_parent);
            // On no change, or a cache hit, we avoid committing the args to the arena
            return if parent_pending {
                // The parent generic is still being defined (mutual recursion), so its
                // template cannot be substituted yet; defer to a reserved instance id
                self.get_or_reserve_recursive_instance(generic_parent, new_type_args.as_slice())
            } else if any_change {
                self.instantiate_generic_type(generic_parent, new_type_args.as_slice())
            } else {
                self.instantiate_generic_type(generic_parent, original_args.as_slice(&self.mem))
            };
        };

        let matching_subst_pair = substitution_pairs.iter().find(|pair| pair.from == type_id);
        if let Some(matching_pair) = matching_subst_pair {
            return matching_pair.to;
        }

        let res = match self.types.get(type_id) {
            Type::InferenceHole(_) => type_id,
            Type::Char
            | Type::Integer(_)
            | Type::Enum(_)
            | Type::Float(_)
            | Type::Bool
            | Type::Pointer
            | Type::Never => type_id,
            Type::Struct(struc) => {
                let record_kind = struc.record_kind;
                let old_fields = struc.fields;
                let mut any_change = false;
                let original_defn_info = self.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_fields = self.tmp.new_list(old_fields.len());
                for field in self.mem.getn(old_fields) {
                    let new_field_type_id = self.substitute_in_type_ext_inner(
                        field.type_id,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_field_type_id != field.type_id {
                        any_change = true;
                    }
                    let mut new_field = *field;
                    new_field.type_id = new_field_type_id;
                    new_fields.push(new_field);
                }
                if any_change {
                    let generic_instance_info = match generic_parent_to_attach {
                        Some(parent) => {
                            let mut args: List<TypeId, MemTmp> =
                                self.tmp.new_list(substitution_pairs.len() as u32);
                            for pair in substitution_pairs {
                                args.push(pair.to);
                            }
                            Some(GenericInstanceInfo {
                                generic_parent: parent,
                                type_args: TypeArgs::from_slice_in(args.as_slice(), &mut self.mem),
                            })
                        }
                        None => self.get_instance_info(type_id).cloned(),
                    };

                    let new_fields_handle = self.mem.pushn(new_fields.as_slice());
                    let specialized_struct = StructType { fields: new_fields_handle, record_kind };
                    self.add_type(
                        Type::Struct(specialized_struct),
                        defn_info_to_use,
                        generic_instance_info,
                    )
                } else {
                    type_id
                }
            }
            Type::Sum(e) => {
                let original_tag_type = e.tag_type;
                let old_variants = e.variants;
                let mut any_changed = false;
                let original_defn_info = self.get_defn_info(type_id);
                let defn_info_to_use = defn_info_to_attach.or(original_defn_info);
                // Build the candidate in tmp; only commit to the arena if something changed
                let mut new_variants = self.tmp.new_list(old_variants.len());
                for variant in self.mem.getn(old_variants) {
                    let mut new_variant = *variant;
                    if let Some(p) = variant.payload {
                        let new_payload_id = self.substitute_in_type_ext_inner(
                            p,
                            substitution_pairs,
                            from_kinds,
                            None,
                            None,
                        );
                        if new_payload_id != p {
                            any_changed = true;
                            new_variant.payload = Some(new_payload_id)
                        };
                    }
                    new_variants.push(new_variant);
                }
                if any_changed {
                    let generic_instance_info = match generic_parent_to_attach {
                        Some(parent) => {
                            let mut args: List<TypeId, MemTmp> =
                                self.tmp.new_list(substitution_pairs.len() as u32);
                            for pair in substitution_pairs {
                                args.push(pair.to);
                            }
                            Some(GenericInstanceInfo {
                                generic_parent: parent,
                                type_args: TypeArgs::from_slice_in(args.as_slice(), &mut self.mem),
                            })
                        }
                        None => self.get_instance_info(type_id).cloned(),
                    };
                    let new_variants_handle = self.mem.pushn(new_variants.as_slice());
                    let new_sum =
                        SumType { variants: new_variants_handle, tag_type: original_tag_type };
                    let new_sum_id =
                        self.add_type(Type::Sum(new_sum), defn_info_to_use, generic_instance_info);
                    new_sum_id
                } else {
                    type_id
                }
            }
            Type::Opaque(_) => type_id,
            Type::Reference(reference) => {
                let reference = *reference;
                let new_inner = self.substitute_in_type_ext_inner(
                    reference.inner_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_inner != reference.inner_type {
                    self.add_reference_type(new_inner)
                } else {
                    type_id
                }
            }
            Type::TypeParameter(_type_param) => type_id,
            Type::FunctionTypeParameter(ftp) => {
                let function_type_id = ftp.function_type;
                let new_fn_type = self.substitute_in_type_ext_inner(
                    function_type_id,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_fn_type != function_type_id {
                    let type_param = self.types.get(type_id).as_function_type_parameter().unwrap();
                    self.add_type_anon(Type::FunctionTypeParameter(FunctionTypeParameter {
                        name: type_param.name,
                        scope_id: type_param.scope_id,
                        span: type_param.span,
                        function_type: new_fn_type,
                    }))
                } else {
                    type_id
                }
            }
            Type::Generic(_) => {
                unreachable!("substitute_in_type is not expected to be called on a Generic")
            }
            Type::Function(fun_type) => {
                let mut any_new = false;
                let is_lambda = fun_type.is_lambda;
                let old_return_type = fun_type.return_type;
                let old_params = fun_type.physical_params;
                let old_call_conv = fun_type.abi_mode;
                let new_return_type = self.substitute_in_type_ext_inner(
                    old_return_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_return_type != old_return_type {
                    any_new = true
                };
                let mut new_params: List<FnParamType, _> = self.tmp.new_list(old_params.len());
                for param in self.mem.getn(old_params) {
                    let new_param_type = self.substitute_in_type_ext_inner(
                        param.type_id,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_param_type != param.type_id {
                        any_new = true;
                    }
                    let new_param = FnParamType {
                        name: param.name,
                        type_id: new_param_type,
                        is_context: param.is_context,
                        is_lambda_env: param.is_lambda_env,
                        is_macro_code: param.is_macro_code,
                    };
                    new_params.push(new_param);
                }
                if any_new {
                    let new_params_handle = self.mem.pushn(new_params.as_slice());
                    let new_fun_type = FunctionType {
                        physical_params: new_params_handle,
                        return_type: new_return_type,
                        is_lambda,
                        abi_mode: old_call_conv,
                    };
                    let new_function_type_id = self.add_type_anon(Type::Function(new_fun_type));
                    new_function_type_id
                } else {
                    type_id
                }
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let new_fn_type = self.substitute_in_type_ext_inner(
                    fp.function_type_id,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_fn_type != fp.function_type_id {
                    self.add_function_pointer_type(new_fn_type)
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
                let new_fn_type = self.substitute_in_type_ext_inner(
                    fn_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_fn_type != fn_type {
                    self.add_lambda_object(new_fn_type, parsed_id)
                } else {
                    type_id
                }
            }
            Type::AbilityObject(ao) => {
                let ao = *ao;
                let mut any_change = false;
                let mut new_args = self.tmp.new_list(ao.impl_arguments.len());
                for arg in self.mem.getn(ao.impl_arguments) {
                    let new_arg_type = self.substitute_in_type_ext_inner(
                        *arg,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_arg_type != *arg {
                        any_change = true;
                    }
                    new_args.push(new_arg_type);
                }
                if any_change {
                    let impl_arguments = self.mem.pushn(new_args.as_slice());
                    let new_signature = TypedAbilitySignature {
                        specialized_ability_id: ao.specialized_ability_id,
                        impl_arguments,
                    };
                    match self.eval_dyn_ability_object_type(new_signature, SpanId::NONE) {
                        Ok(t) => t,
                        Err(e) => {
                            self.ice("dyn ability substitution produced invalid object", Some(&e))
                        }
                    }
                } else {
                    type_id
                }
            }
            Type::StaticValue(value_type) => {
                if value_type.value_id.is_some() {
                    // Can't substitute inside the type "static[string; "hello"]"
                    // But you can inside type "static[T; _]"
                    type_id
                } else {
                    let family_type = value_type.family_type_id;
                    let new_inner_type = self.substitute_in_type_ext_inner(
                        family_type,
                        substitution_pairs,
                        from_kinds,
                        None,
                        None,
                    );
                    if new_inner_type == family_type {
                        type_id
                    } else {
                        self.add_type_anon(Type::StaticValue(StaticValueType {
                            family_type_id: new_inner_type,
                            value_id: None,
                        }))
                    }
                }
            }
            Type::Array(arr) => {
                let arr = *arr;
                let element_type = arr.element_type;
                let new_element_type = self.substitute_in_type_ext_inner(
                    element_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                let new_size_type = self.substitute_in_type_ext_inner(
                    arr.size_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_element_type == element_type && new_size_type == arr.size_type {
                    type_id // No change needed
                } else {
                    // Create new Array type with substituted element type
                    let new_array_type = Type::Array(ArrayType {
                        element_type: new_element_type,
                        size_type: new_size_type,
                    });
                    self.add_type_anon(new_array_type)
                }
            }
            Type::Vector(vec) => {
                let vec = *vec;
                let new_element_type = self.substitute_in_type_ext_inner(
                    vec.element_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                let new_size_type = self.substitute_in_type_ext_inner(
                    vec.size_type,
                    substitution_pairs,
                    from_kinds,
                    None,
                    None,
                );
                if new_element_type == vec.element_type && new_size_type == vec.size_type {
                    type_id
                } else {
                    self.add_type_anon(Type::Vector(VectorType {
                        element_type: new_element_type,
                        size_type: new_size_type,
                    }))
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

    fn evaluate_module_manifest(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        primary_module: bool,
    ) -> K1Result<Option<(ModuleManifest, ParsedId)>> {
        let module_ident = self.ast.idents.b.module;
        let namespace = self.ast.namespaces.get(parsed_namespace_id);
        let mut manifest_fn_id = None;
        for defn in namespace.definitions.as_slice(&self.ast.mem) {
            let Some(fn_id) = defn.as_function_id() else { continue };
            if self.ast.get_function(fn_id).name == module_ident {
                manifest_fn_id = Some(fn_id);
                break;
            }
        }
        let Some(manifest_fn_id) = manifest_fn_id else {
            return Ok(None);
        };
        let f = self.ast.get_function(manifest_fn_id);
        let (fn_span, fn_body, fn_ret_type) = (f.span, f.body, f.ret_type);
        if !f.type_params.is_empty() || !f.params.is_empty() {
            kbail!(self, fn_span, "fn module takes no parameters");
        }
        let Some(body) = fn_body else {
            kbail!(self, fn_span, "fn module must have a body");
        };
        let module_type_id = self.builtin_types.k1_module.unwrap();
        if let Some(ret_type_expr) = fn_ret_type {
            let ret_type = self.eval_type_expr(ret_type_expr, Scopes::ROOT_SCOPE_ID)?;
            if ret_type != module_type_id {
                kbail!(self, fn_span, "fn module must return k1/module");
            }
        }
        let manifest_result = self.execute_static_expr(
            body,
            EvalExprContext::make(Scopes::ROOT_SCOPE_ID)
                .with_expected_type(Some(module_type_id))
                .with_static_ctx(Some(StaticExecContext {
                    expected_return_type: Some(module_type_id),
                }))
                .with_manifest_eval(),
            &[],
        )?;

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

        Ok(Some((
            ModuleManifest { kind, deps, libs, link_args, setup },
            ParsedId::Function(manifest_fn_id),
        )))
    }

    fn compile_pattern_to_type(
        &mut self,
        pat_expr: ParsedPatternId,
        target_type_id: TypeId,
        scope_id: ScopeId,
        allow_bindings: bool,
    ) -> K1Result<TypedPatternId> {
        let parsed_pattern_expr = self.ast.patterns.get(pat_expr);
        match parsed_pattern_expr {
            ParsedPattern::Wildcard(span) => Ok(self.patterns.add(TypedPattern::Wildcard(*span))),
            ParsedPattern::Literal(literal_expr_id) => {
                match self.ast.exprs.get(*literal_expr_id).expect_literal() {
                    ParsedLiteral::Char(c, span) => match self.types.get(target_type_id) {
                        Type::Char => Ok(self.patterns.add(TypedPattern::LiteralChar(*c, *span))),
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "unrelated pattern type char will never match {}",
                            target_type_id
                        )),
                    },
                    ParsedLiteral::Numeric(num_lit) => {
                        let num_lit = *num_lit;
                        let num_value_id = self.eval_numeric_value(
                            num_lit.text_span,
                            EvalExprContext::make(scope_id)
                                .with_expected_type(Some(target_type_id)),
                        )?;
                        match self.static_values.get(num_value_id) {
                            StaticValue::Int(_) => match self.types.get(target_type_id) {
                                Type::Integer(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralInteger(num_value_id, num_lit.span))),
                                _ => Err(kerr!(
                                    self,
                                    self.ast.get_pattern_span(pat_expr),
                                    "integer literal pattern will never match {}",
                                    target_type_id
                                )),
                            },
                            StaticValue::Float(_) => match self.types.get(target_type_id) {
                                Type::Float(_) => Ok(self
                                    .patterns
                                    .add(TypedPattern::LiteralFloat(num_value_id, num_lit.span))),
                                _ => Err(kerr!(
                                    self,
                                    self.ast.get_pattern_span(pat_expr),
                                    "float literal pattern will never match {}",
                                    target_type_id
                                )),
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
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "bool literal pattern will never match {}",
                            target_type_id
                        )),
                    },
                    ParsedLiteral::String(string_id, span) => {
                        match self.types.get(target_type_id) {
                            Type::StaticValue(svt)
                                if svt.family_type_id == self.builtin_types.string() =>
                            {
                                Ok(())
                            }
                            _ if target_type_id == self.builtin_types.string() => Ok(()),
                            _ => Err(kerr!(
                                self,
                                self.ast.get_pattern_span(pat_expr),
                                "string literal pattern will never match {}",
                                target_type_id
                            )),
                        }?;
                        Ok(self.patterns.add(TypedPattern::LiteralString(*string_id, *span)))
                    }
                }
            }
            ParsedPattern::Variable(ident_id, span) => {
                if *ident_id == self.ast.idents.b.null {
                    match self.types.get(target_type_id) {
                        Type::Reference(reference_type) => Ok(self
                            .patterns
                            .add(TypedPattern::RefNull(reference_type.inner_type, *span))),
                        Type::Pointer => Ok(self.patterns.add(TypedPattern::PointerNull(*span))),
                        _ => Err(kerr!(
                            self,
                            self.ast.get_pattern_span(pat_expr),
                            "'null' is a pattern that applies to reference (*t) types and ptr"
                        )),
                    }
                } else {
                    if !allow_bindings {
                        kbail!(self, *span, "Bindings are not allowed here");
                    }
                    Ok(self.patterns.add(TypedPattern::Variable(VariablePattern {
                        name: *ident_id,
                        type_id: target_type_id,
                        span: *span,
                    })))
                }
            }
            ParsedPattern::Sum(sum_pattern) => {
                let sum_pattern = *sum_pattern;
                let sum_pattern_span = sum_pattern.span;

                if let Some(cs) = &mut self.completion
                    && cs.site.is_none()
                    && sum_pattern.variant_name == cs.marker
                {
                    cs.site = Some(CompletionSite::Variant { type_id: target_type_id });
                }

                match self.types.get(target_type_id) {
                    Type::Sum(sum_type) => {
                        if let Some(name) = sum_pattern.sum_name {
                            match self.scopes.find_type(scope_id, name) {
                                None => {
                                    kbail!(
                                        self,
                                        sum_pattern.span,
                                        "No type named '{}'",
                                        self.ident_str(name).blue()
                                    );
                                }
                                Some((named_type, _)) => {
                                    // Consider generics: 'Opt.Some' applies to all Opt[T]s, so we consider
                                    // the 'base' type
                                    let base_type = match self.get_instance_info(target_type_id) {
                                        Some(info) => info.generic_parent,
                                        None => target_type_id,
                                    };
                                    if base_type != named_type {
                                        kbail!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: sum pattern refers to type '{}' which is not the same as match target '{}'",
                                            self.type_id_to_string_ext(
                                                named_type,
                                                dump::TypeDisplayMode::Expand
                                            )
                                            .blue(),
                                            self.type_id_to_string_ext(
                                                base_type,
                                                dump::TypeDisplayMode::Expand
                                            )
                                            .blue()
                                        );
                                    }
                                }
                            }
                        }
                        let Some(matching_variant) =
                            self.sum_variant_by_name(sum_type.variants, sum_pattern.variant_name)
                        else {
                            kbail!(
                                self,
                                sum_pattern.span,
                                "Impossible pattern: No variant named '{}' in {}",
                                sum_pattern.variant_name,
                                target_type_id,
                            );
                        };

                        let matching_variant_index = matching_variant.index;
                        let matching_variant_name = matching_variant.name;
                        self.emit_ls_entity(
                            sum_pattern.span,
                            LsEntityKind::Variant {
                                type_id: target_type_id,
                                variant_index: matching_variant_index,
                            },
                        );

                        let payload_pattern = match &sum_pattern.payload_pattern {
                            None if matching_variant.payload == Some(EMPTY_TYPE_ID) => {
                                Some(self.patterns.add(TypedPattern::Wildcard(sum_pattern_span)))
                            }
                            None => None,
                            Some(payload_expr) => {
                                let payload_type_id =
                                    matching_variant.payload.ok_or_else(|| {
                                        kerr!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: Variant '{}' has no payload",
                                            matching_variant.name
                                        )
                                    })?;
                                let payload_pattern = self.compile_pattern_to_type(
                                    *payload_expr,
                                    payload_type_id,
                                    scope_id,
                                    allow_bindings,
                                )?;
                                Some(payload_pattern)
                            }
                        };

                        let sum_pattern = TypedSumPattern {
                            sum_type_id: target_type_id,
                            variant_index: matching_variant_index,
                            variant_name: matching_variant_name,
                            payload: payload_pattern,
                            span: sum_pattern_span,
                        };
                        Ok(self.patterns.add(TypedPattern::Sum(sum_pattern)))
                    }
                    Type::Enum(e) => {
                        if let Some(name) = sum_pattern.sum_name {
                            match self.scopes.find_type(scope_id, name) {
                                None => {
                                    kbail!(
                                        self,
                                        sum_pattern.span,
                                        "No type named '{}'",
                                        self.ident_str(name).blue()
                                    );
                                }
                                Some((named_type, _)) => {
                                    // No need to consider generics
                                    if target_type_id != named_type {
                                        kbail!(
                                            self,
                                            sum_pattern.span,
                                            "Impossible pattern: sum pattern refers to type '{}' which is not the same as match target '{}'",
                                            named_type,
                                            target_type_id,
                                        );
                                    }
                                }
                            }
                        }
                        let Some((matching_value_index, matching_value)) =
                            self.enum_value_by_name(e.member_values, sum_pattern.variant_name)
                        else {
                            kbail!(
                                self,
                                sum_pattern.span,
                                "Impossible pattern: No value named '{}' in {}",
                                sum_pattern.variant_name,
                                target_type_id
                            );
                        };
                        let matching_value_name = matching_value.name;
                        self.emit_ls_entity(
                            sum_pattern.span,
                            LsEntityKind::Variant {
                                type_id: target_type_id,
                                variant_index: matching_value_index as u32,
                            },
                        );

                        let enum_pattern = TypedEnumPattern {
                            enum_type_id: target_type_id,
                            member_name: matching_value_name,
                            index: matching_value_index as u32,
                            int_value: matching_value.int_value,
                            span: sum_pattern_span,
                        };
                        Ok(self.patterns.add(TypedPattern::Enum(enum_pattern)))
                    }
                    _ => Err(kerr!(
                        self,
                        sum_pattern.span,
                        "this pattern will never match {}",
                        target_type_id
                    )),
                }
            }
            ParsedPattern::Struct(struct_pattern) => {
                let target_type = self.types.get(target_type_id);
                let struct_pattern = *struct_pattern;
                let expected_struct = *target_type.as_struct().ok_or_else(|| {
                    kerr!(
                        self,
                        struct_pattern.span,
                        "Impossible pattern: Match target '{}' is not a struct",
                        target_type_id
                    )
                })?;
                let mut fields = self.patterns.mem.new_list(struct_pattern.fields.len());
                for (field_name, field_parsed_pattern_id) in
                    self.ast.mem.getn(struct_pattern.fields)
                {
                    let (expected_field_index, expected_field) =
                        expected_struct.find_field(&self.mem, *field_name).ok_or_else(|| {
                            kerr!(
                                self,
                                self.ast.get_pattern_span(*field_parsed_pattern_id),
                                "Impossible pattern: Struct has no field named '{}'",
                                self.ident_str(*field_name).blue()
                            )
                        })?;
                    let field_type_id = expected_field.type_id;
                    let field_pattern = self.compile_pattern_to_type(
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
                    fields: fields.to_slice(),
                    span: struct_pattern.span,
                };
                Ok(self.patterns.add(TypedPattern::Struct(struct_pattern)))
            }
            ParsedPattern::Reference(reference_pattern) => {
                let Type::Reference(r) = self.types.get(target_type_id) else {
                    kbail!(
                        self,
                        reference_pattern.span,
                        "Reference pattern will never match non-reference {}",
                        target_type_id
                    );
                };
                let reference_pattern_span = reference_pattern.span;
                let inner_pattern = self.compile_pattern_to_type(
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
            ParsedPattern::Type(parsed_type_pattern) => {
                let parsed_type_pattern = *parsed_type_pattern;
                let type_id = self.eval_type_expr(parsed_type_pattern.type_expr, scope_id)?;
                let inner_pattern = self.compile_pattern_to_type(
                    parsed_type_pattern.inner,
                    type_id,
                    scope_id,
                    allow_bindings,
                )?;
                let typed_pattern_id = self.patterns.add(TypedPattern::Type(TypePattern {
                    inner_pattern,
                    type_id,
                    span: parsed_type_pattern.span,
                }));
                Ok(typed_pattern_id)
            }
        }
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
    fn check_expr_type<'a>(
        &mut self,
        expected: TypeId,
        expr: TypedExprId,
        scope_id: ScopeId,
        allow_addr_of: bool,
    ) -> CheckExprTypeResult<'a> {
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
                return CheckExprTypeResult::Coerce(
                    materialized_value,
                    "static_materialize".into(),
                );
            }
            // If we failed typechecking, and we expected a static, and we passed a non-static
            // Try to lift it
            (Type::StaticValue(expected_value_type), None) => {
                if expected_value_type.family_type_id == actual_type_id {
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
            if let Type::Lambda(lambda_type_id) = self.get_expr_type(expr) {
                let lambda_type_id = *lambda_type_id;
                let lambda_type = self.lambda_types.get(lambda_type_id);
                let lambda_object_type =
                    self.add_lambda_object(lambda_type.function_type, lambda_type.parsed_id);
                match self.check_types(expected, lambda_object_type, scope_id) {
                    Ok(_) => {
                        return match self.lambda_to_lambda_object(expr, lambda_type_id, scope_id) {
                            Ok(lambda_object) => {
                                CheckExprTypeResult::Coerce(lambda_object, "lam->lamobj".into())
                            }
                            Err(e) => {
                                CheckExprTypeResult::Err(self.ident_str(e.message).to_string())
                            }
                        };
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
                        return CheckExprTypeResult::Coerce(expr, "address_of".into());
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
                        CheckExprTypeResult::Err(msg.to_string())
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

        CheckExprTypeResult::Err(msg.to_string())
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
                    message: self.ast.idents.intern(&msg),
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
                for (index, (exp_param, act_param)) in spec1
                    .type_args
                    .as_slice(&self.mem)
                    .iter()
                    .zip(spec2.type_args.as_slice(&self.mem).iter())
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
            (_exp, _act) => Err(k1_format_user!(self, "Expected {} but got {}", expected, actual,)),
        }
    }

    fn intercept_trivial_static_expr(&mut self, expr_id: TypedExprId) -> Option<StaticValueId> {
        let TypedExpr::Block(b) = self.exprs.get(expr_id) else { return None };

        match self.mem.getn(b.statements) {
            &[s1] => {
                let Some(expr_id) = self.stmts.get(s1).as_expr() else { return None };
                match self.exprs.get(expr_id) {
                    TypedExpr::Return(return_expr) => {
                        match self.exprs.get(return_expr.value) {
                            TypedExpr::StaticValue(s) => Some(s.value_id),
                            TypedExpr::Call { call_id, .. } => {
                                // A call to zeroed() becomes StaticValue::Zero directly; no need to
                                // run silly code
                                let call = self.calls.get(*call_id);
                                let function_id = call.callee.maybe_function_id()?;
                                let function = self.functions.get(function_id);
                                if let Some(Builtin::Ir(BuiltinIr::Zeroed)) = function.builtin_type
                                {
                                    let return_type_id = self.exprs.get_type(return_expr.value);
                                    Some(self.static_values.add(StaticValue::Zero(return_type_id)))
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn compile_all_pending_ir(&mut self, on_behalf_of_span: SpanId) -> K1Result<()> {
        loop {
            // eprintln!(
            //     "compile_all_pending_ir {}",
            //     self.ir.b_units_pending_compile.len()
            // );
            // for p in &self.ir.b_units_pending_compile {
            //     eprintln!("PENDING: {} {}", p.as_u32(), self.function_id_to_string(*p, false));
            // }
            if let Some(function_id) = self.ir.units_pending_compile.keys().next().copied() {
                self.ir.units_pending_compile.remove(&function_id);
                self.eval_function_body(function_id)?;
                if let Err(e) = ir::compile_function(self, function_id) {
                    kbail!(
                        self,
                        on_behalf_of_span,
                        "Failed to compile ir for function execution: {}",
                        e.message
                    );
                };
            } else if let Some(global_id) = self.ir.globals_pending_eval.keys().next().copied() {
                self.ir.globals_pending_eval.remove(&global_id);
                let ast_id = self.globals.get(global_id).ast_id;
                self.eval_global_body(ast_id)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    fn execute_parsed_expr_with_vm(
        &mut self,
        vm: &mut vm::Vm,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<StaticValueId> {
        let expr = self.compile_parsed_expr_for_exec(parsed_expr, ctx, input_parameters)?;
        if let Some(shortcut_value_id) = self.intercept_trivial_static_expr(expr) {
            return Ok(shortcut_value_id);
        }
        let execution_result = bc::exec::execute_compiled_expr(self, vm, expr, true);

        vm.reset(self.global_id_k1_arena);

        let static_value_id = execution_result?;

        Ok(static_value_id)
    }

    /// Typecheck, compile, and optimize a static expr's unit for execution
    fn compile_parsed_expr_for_exec(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<TypedExprId> {
        if let ParsedExpr::Static(_static) = self.ast.exprs.get(parsed_expr) {
            self.report_warn(
                self.ast.exprs.get_span(parsed_expr),
                "This #static is immediately inside a static",
            )
        }

        let parsed_expr_as_block =
            self.ensure_parsed_expr_to_block(parsed_expr, ParsedBlockKind::FunctionBody);
        let expr_span = parsed_expr_as_block.span;
        let static_block_scope =
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::LexicalBlock, ScopeOwnerId::None);
        let mut cur_scope = ctx.scope_id;
        let mut locals_to_mask = self.tmp.new_list(0);

        // Mask everything up to the nearest function scope that is not in input_parameters
        // A better system would be just to run those expressions statically like we do for value macro args
        loop {
            let s = self.scopes.get_scope(cur_scope);
            let parent = s.parent;
            if s.scope_type == ScopeType::FunctionScope || s.scope_type == ScopeType::Namespace {
                break;
            }

            for (name, vis) in &s.variables {
                let Some(variable_id) = vis.variable_id() else {
                    continue;
                };
                if !input_parameters.iter().any(|(input_var_id, _)| *input_var_id == variable_id) {
                    locals_to_mask.push_grow(&mut self.tmp, *name);
                }
            }

            if let Some(parent) = parent { cur_scope = parent } else { break }
        }
        for name in locals_to_mask.as_slice() {
            self.scopes.mask_variable(static_block_scope, *name);
        }

        let static_eval_ctx = ctx.with_scope(static_block_scope);
        let expr = self.eval_block(&parsed_expr_as_block, static_eval_ctx, true)?;
        let expr_metadata = self.ast.exprs.get_metadata(parsed_expr);
        let is_debug = expr_metadata.is_debug;
        if is_debug {
            eprintln!("COMPILED TO BLOCK\n\n{}", self.expr_to_string_with_type(expr));
        }

        ir::compile_top_level_expr(self, expr, input_parameters, is_debug)?;
        self.compile_all_pending_ir(expr_span)?;
        ir::optimize_unit(self, IrUnitId::Expr(expr));
        if is_debug {
            eprintln!(
                "executing optimized unit.\n{}",
                ir::unit_to_string(self, IrUnitId::Expr(expr), false)
            );
        }
        Ok(expr)
    }

    fn execute_static_expr(
        &mut self,
        parsed_expr: ParsedExprId,
        ctx: EvalExprContext,
        input_parameters: &[(VariableId, StaticValueId)],
    ) -> K1Result<StaticValueId> {
        let span = self.ast.exprs.get(parsed_expr).get_span();
        let infer_start = if ctx.is_inference() { Some(self.timing.clock.raw()) } else { None };
        let result = self.do_with_vm(span, |k1, vm| {
            k1.execute_parsed_expr_with_vm(vm, parsed_expr, ctx, input_parameters)
        });
        if let Some(start) = infer_start {
            self.timing.total_infer_execs += 1;
            self.timing.total_infer_exec_nanos += self.timing.clock.elapsed_nanos(start) as i64;
        }
        result
    }

    fn do_with_vm<T>(
        &mut self,
        _span: SpanId,
        mut f: impl FnMut(&mut TypedProgram, &mut vm::Vm) -> T,
    ) -> T {
        let (mut vm, used_alt) = match *std::mem::take(&mut self.vm) {
            None => {
                let maybe_alt = self.vm_alts.pop();
                let alt_vm = match maybe_alt {
                    None => {
                        // self.report_warn(span, "Had to make a new alt VM");
                        let new_vm = vm::Vm::make();
                        new_vm
                    }
                    Some(alt_vm) => alt_vm,
                };
                (alt_vm, true)
            }
            Some(vm) => (vm, false),
        };
        let res = f(self, &mut vm);
        if !used_alt {
            *self.vm = Some(vm);
        } else {
            debug!("Restoring alt VM to pool");
            self.vm_alts.push(vm);
        }

        res
    }

    fn execute_static_function(
        &mut self,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: SpanId,
    ) -> K1Result<StaticValueId> {
        self.do_with_vm(span, |k1, vm| {
            let result =
                Self::static_exec_function_with_vm(k1, vm, function_id, function_parameters, span);
            vm.reset(k1.global_id_k1_arena);
            result
        })
    }

    fn static_exec_prepare_function(
        k1: &mut TypedProgram,
        function_id: FunctionId,
        span: SpanId,
    ) -> K1Result<()> {
        ir::compile_function(k1, function_id)?;
        k1.compile_all_pending_ir(span)?;
        // Macros execute repeatedly; inline_done guards re-optimizing
        let unit_id = IrUnitId::Function(function_id);
        if !ir::get_compiled_unit(&k1.ir, unit_id).unwrap().inline_done {
            ir::optimize_unit(k1, unit_id);
        }
        Ok(())
    }

    /// Compile, optimize, and run; no VM reset, so callers control when the
    /// result's VM memory dies
    fn static_exec_function_with_vm(
        k1: &mut TypedProgram,
        vm: &mut vm::Vm,
        function_id: FunctionId,
        function_parameters: &[StaticValueId],
        span: SpanId,
    ) -> K1Result<StaticValueId> {
        Self::static_exec_prepare_function(k1, function_id, span)?;
        bc::exec::execute_compiled_function(k1, vm, function_id, function_parameters, true)
    }

    fn execute_static_condition(&mut self, cond: Option<ParsedExprId>, scope_id: ScopeId) -> bool {
        if let Some(condition_expr) = cond {
            match self.execute_static_bool(condition_expr, EvalExprContext::make(scope_id)) {
                Err(e) => {
                    self.report(e);
                    false
                }
                Ok(b) => b,
            }
        } else {
            true
        }
    }

    fn execute_static_bool(&mut self, cond: ParsedExprId, ctx: EvalExprContext) -> K1Result<bool> {
        let vm_cond_result = self.execute_static_expr(
            cond,
            ctx.with_expected_type(Some(BOOL_TYPE_ID)).with_static_ctx(Some(StaticExecContext {
                expected_return_type: Some(BOOL_TYPE_ID),
            })),
            &[],
        )?;
        let StaticValue::Bool(condition_bool) = self.static_values.get(vm_cond_result) else {
            let cond_span = self.ast.get_expr_span(cond);
            kbail!(self, cond_span, "Condition is not a boolean");
        };
        Ok(*condition_bool)
    }

    fn declare_global(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        scope_id: ScopeId,
    ) -> K1Result<Option<VariableId>> {
        if let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id) {
            return Ok(Some(self.globals.get(*global_id).variable_id));
        }
        let parsed = *self.ast.get_global(parsed_global_id);
        if !self.execute_static_condition(parsed.compile_condition, scope_id) {
            return Ok(None);
        }
        let owner_ns = match self.scopes.get_scope(scope_id).owner_id {
            ScopeOwnerId::Namespace(ns) => ns,
            _ => kbail!(self, SpanId::NONE, "declare_global's scope_id should be an namespace"),
        };
        if parsed.is_thread_local {
            self.fail_if_reload_ns(owner_ns, parsed.span, "tls globals")?;
        }
        if parsed.is_external || parsed.is_export {
            self.fail_if_reload_ns(owner_ns, parsed.span, "extern and exported globals")?;
        }
        let type_id = self.eval_type_expr(parsed.type_expr, scope_id)?;

        let reload_ns = if self.namespaces.get(owner_ns).reload { Some(owner_ns) } else { None };
        let global_id = self.globals.next_id();
        let variable_id = self.variables.add(Variable {
            name: parsed.name,
            type_id,
            owner_scope: scope_id,
            kind: VariableKind::Global(global_id),
            flags: VariableFlags::empty(),
            usage_count: 0,
            defn_span: parsed.name_span,
        });
        self.globals.add_expected_id(
            TypedGlobal {
                variable_id,
                initial_value: GlobalInitialValue::Pending,
                parsed_expr: parsed.value_expr,
                type_id,
                span: parsed.span,
                is_constant: !parsed.is_mutable,
                is_tls: parsed.is_thread_local,
                is_exported: parsed.is_export,
                is_external: parsed.is_external,
                ast_id: parsed_global_id,
                parent_scope: scope_id,
                reload_ns,
            },
            global_id,
        );

        if scope_id == self.scopes.mem_scope_id && parsed.name == self.ast.idents.b.arena_tmp {
            self.global_id_k1_arena = Some(global_id)
        };
        self.global_ast_mappings.insert(parsed_global_id, global_id);
        self.scopes.add_variable(scope_id, parsed.name, variable_id);

        self.emit_ls_entity(parsed.name_span, LsEntityKind::Variable { variable_id });

        Ok(Some(variable_id))
    }

    pub fn eval_global_body(&mut self, parsed_global_id: ParsedGlobalId) -> K1Result<()> {
        let Some(global_id) = self.global_ast_mappings.get(&parsed_global_id).copied() else {
            // This means we failed to compile the definition; or we have a bug!
            // TODO: Store failures so we can be certain which is true!
            debug!("skipping rest of global body");
            return Ok(());
        };
        // Evaluation is one-shot; the pre-execution drain may get here before the body phase
        if !self.globals.get(global_id).initial_value.is_pending() {
            return Ok(());
        }
        if self.globals_in_progress.contains(&global_id) {
            let global_name = |id: &TypedGlobalId| {
                self.ident_str(self.variables.get(self.globals.get(*id).variable_id).name)
            };
            let mut cycle = String::new();
            for id in self.globals_in_progress.iter() {
                cycle.push_str(global_name(id));
                cycle.push_str(" -> ");
            }
            cycle.push_str(global_name(&global_id));
            kbail!(
                self,
                self.ast.get_global(parsed_global_id).span,
                "Global initializer cycle: {}",
                cycle,
            );
        }
        self.globals_in_progress.push(global_id);
        let result =
            self.with_clean_inference(|k1| k1.eval_global_body_inner(parsed_global_id, global_id));
        let popped = self.globals_in_progress.pop();
        debug_assert_eq!(popped, Some(global_id));
        result
    }

    fn eval_global_body_inner(
        &mut self,
        parsed_global_id: ParsedGlobalId,
        global_id: TypedGlobalId,
    ) -> K1Result<()> {
        let parsed_global = *self.ast.get_global(parsed_global_id);
        let typed_global = self.globals.get(global_id);
        let is_external = typed_global.is_external;
        let parsed_expr = typed_global.parsed_expr;
        let scope_id = typed_global.parent_scope;
        let declared_type = typed_global.type_id;
        let variable_id = typed_global.variable_id;
        let value_expr_id = if is_external {
            match parsed_expr {
                None => {
                    // Evaluated, but there is no compile-time value: storage arrives at
                    // link time. Recording this keeps evaluation one-shot
                    self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Uninit;
                    return Ok(());
                }
                Some(_id) => {
                    kbail!(self, parsed_global.span, "External globals cannot have initializers");
                }
            }
        } else {
            match parsed_expr {
                None => kbail!(self, parsed_global.span, "Global has no initializer"),
                Some(id) => id,
            }
        };

        let global_name = parsed_global.name;
        let global_span = parsed_global.span;

        let expected_type_for_execution = match self.get_static_type_of_type(declared_type) {
            Some(s) => s.family_type_id,
            None => declared_type,
        };

        let static_value_id = if let ParsedExpr::Builtin(span) = self.ast.exprs.get(value_expr_id) {
            let span = *span;
            self.eval_builtin_global(global_name, scope_id, expected_type_for_execution, span)?
        } else if let ParsedExpr::Call(call) = self.ast.exprs.get(value_expr_id)
            && call.name.name == self.ast.idents.b.module_params
            && {
                let path = self.ast.mem.getn(call.name.path);
                path.len() == 1 && path[0].name == self.ast.idents.b.k1
            }
        {
            let (call_span, call_args) = (call.span, call.args);
            self.handle_module_params_decl_call(
                global_id,
                call_span,
                call_args,
                expected_type_for_execution,
                scope_id,
            )?
        } else {
            let ctx = EvalExprContext::make(scope_id)
                .with_expected_type(Some(expected_type_for_execution))
                .with_static_ctx(Some(StaticExecContext {
                    expected_return_type: Some(expected_type_for_execution),
                }));
            self.execute_static_expr(value_expr_id, ctx, &[])?
        };
        let static_value_type_id = self.get_static_value_type(static_value_id);

        match self.get_static_type_of_type(declared_type) {
            None => {
                if let Err(msg) = self.check_types(declared_type, static_value_type_id, scope_id) {
                    kbail!(self, global_span, "Type mismatch for global {}: {}", global_name, msg);
                }
            }
            Some(static_type) => {
                // declared static, with a specific value, must match
                if let Some(expected_value_id) = static_type.value_id {
                    if expected_value_id != static_value_id {
                        kbail!(
                            self,
                            global_span,
                            "Wrong static value: expected {} but got {}",
                            self.static_value_to_string(expected_value_id),
                            self.static_value_to_string(static_value_id)
                        );
                    }
                } else {
                    // declared static, without a value, we must
                    // update the type of the global as well as the type of its tracking variable
                    let static_type_id_with_value =
                        self.add_value_type(static_type.family_type_id, Some(static_value_id));
                    self.globals.get_mut(global_id).type_id = static_type_id_with_value;
                    self.variables.get_mut(variable_id).type_id = static_type_id_with_value
                }
            }
        }

        self.globals.get_mut(global_id).initial_value = GlobalInitialValue::Value(static_value_id);

        Ok(())
    }

    /// Produces the value of a global whose initializer is the `builtin` keyword,
    /// e.g. `let test: bool = builtin`. These are all compiler-known constants
    fn eval_builtin_global(
        &mut self,
        defn_name: StringId,
        scope_id: ScopeId,
        expected_type_id: TypeId,
        span: SpanId,
    ) -> K1Result<StaticValueId> {
        if scope_id != self.get_k1_scope_id() {
            kbail!(self, span, "All the known builtins constants live in the k1 scope");
        }
        let bool_value = match self.ident_str(defn_name) {
            "test" => self.config.is_test_build,
            "no-std" => self.config.no_std,
            "debug" => self.config.debug,
            // The VM overrides this global's value during static execution
            "is-static" => false,
            "os" => {
                let os_tag_value = self.config.target.target_os() as u8;
                let static_enum =
                    StaticValue::Enum(expected_type_id, TypedIntValue::U8(os_tag_value));
                return Ok(self.static_values.add(static_enum));
            }
            "simd-bytes" => {
                let width = self.config.simd_bytes as i64;
                return Ok(self.static_values.add(StaticValue::Int(TypedIntValue::I64(width))));
            }
            s => kbail!(self, span, "Unknown builtin name: {s}"),
        };
        Ok(self.static_values.add(StaticValue::Bool(bool_value)))
    }

    fn handle_module_params_decl_call(
        &mut self,
        global_id: TypedGlobalId,
        call_span: SpanId,
        call_args: AstSlice<ParsedCallArg>,
        schema_type: TypeId,
        scope_id: ScopeId,
    ) -> K1Result<StaticValueId> {
        let global = self.globals.get(global_id);
        let global_span = global.span;
        let global_parent_scope = global.parent_scope;
        let file_id = self.ast.spans.get(global_span).file_id;
        let Some(module_id) =
            self.modules.iter().find(|m| m.root_file_id(&self.mem) == file_id).map(|m| m.id)
        else {
            kbail!(
                self,
                call_span,
                "k1/module-params must be declared in the module's root file \
                 (module.k1 or <module-name>.k1)"
            );
        };
        let module = self.modules.get(module_id);
        let module_name = module.name;
        if global_parent_scope != module.namespace_scope_id {
            kbail!(
                self,
                global_span,
                "k1/module-params must initialize a top-level global of module '{}'",
                self.ident_str(module_name)
            );
        }
        if module.params.is_some() {
            kbail!(
                self,
                global_span,
                "Module '{}' already declared its parameters; k1/module-params may \
                 appear once per module",
                self.ident_str(module_name)
            );
        }
        let Some(schema_struct) = self.types.get(schema_type).as_struct() else {
            kbail!(
                self,
                global_span,
                "The type annotation of a k1/module-params global is the params schema \
                 and must be a struct type"
            );
        };
        let schema_fields = self.mem.getn(schema_struct.fields);

        let args = self.ast.mem.getn(call_args);
        let defaults: Option<SV8<StaticValueId>> = match args {
            [] => None,
            [defaults_arg] => {
                let defaults_expr = defaults_arg.value;
                let defaults_span = self.ast.exprs.get_span(defaults_expr);
                let ctx = EvalExprContext::make(scope_id)
                    .with_expected_type(Some(schema_type))
                    .with_static_ctx(Some(StaticExecContext {
                        expected_return_type: Some(schema_type),
                    }));
                let value_id = self.execute_static_expr(defaults_expr, ctx, &[])?;
                let value_type = self.get_static_value_type(value_id);
                if let Err(msg) = self.check_types(schema_type, value_type, scope_id) {
                    kbail!(self, defaults_span, "Type mismatch in params defaults: {msg}");
                }
                let StaticValue::Struct(s) = self.static_values.get(value_id) else {
                    kbail!(
                        self,
                        defaults_span,
                        "params defaults did not evaluate to a struct value"
                    );
                };
                Some(SmallVec::from_slice(self.static_values.get_slice(s.fields)))
            }
            _ => kbail!(
                self,
                call_span,
                "k1/module-params takes at most one argument: the defaults struct"
            ),
        };

        let mut providers: SV4<(StringId, ParsedExprId)> = smallvec![];
        for m in self.modules.iter() {
            for entry in self.mem.getn(m.manifest.deps) {
                if entry.name == module_name
                    && let Some(params_expr) = entry.params_struct_literal
                {
                    providers.push((m.name, params_expr));
                }
            }
        }

        let mut bound: SV8<Option<(StaticValueId, StringId)>> =
            smallvec![None; schema_fields.len()];
        for (provider_name, params_expr) in providers {
            let ParsedExpr::Struct(s) = self.ast.exprs.get(params_expr) else {
                self.ice_span(call_span, "captured dep params was not a struct literal");
            };
            let literal_fields = self.ast.mem.getn(s.fields);
            for field in literal_fields {
                let Some(field_index) = schema_fields.iter().position(|f| f.name == field.name)
                else {
                    kbail!(
                        self,
                        field.span,
                        "Module '{}' has no parameter '{}'",
                        self.ident_str(module_name),
                        self.ident_str(field.name)
                    );
                };
                let field_type = schema_fields[field_index].type_id;
                let StructValueFieldKind::Expr(field_expr) = field.value else {
                    kbail!(
                        self,
                        field.span,
                        "module params must be provided as explicit field values"
                    );
                };
                let ctx = EvalExprContext::make(Scopes::ROOT_SCOPE_ID)
                    .with_expected_type(Some(field_type))
                    .with_static_ctx(Some(StaticExecContext {
                        expected_return_type: Some(field_type),
                    }));
                let value_id = self.execute_static_expr(field_expr, ctx, &[])?;
                let value_type = self.get_static_value_type(value_id);
                if let Err(msg) = self.check_types(field_type, value_type, Scopes::ROOT_SCOPE_ID) {
                    kbail!(
                        self,
                        field.span,
                        "Type mismatch for parameter '{}' of module '{}': {}",
                        self.ident_str(field.name),
                        self.ident_str(module_name),
                        msg
                    );
                }
                match bound[field_index] {
                    None => bound[field_index] = Some((value_id, provider_name)),
                    Some((existing_id, existing_provider)) => {
                        if existing_id != value_id {
                            kbail!(
                                self,
                                field.span,
                                "Conflicting values for parameter '{}' of module '{}': \
                                 {} (from '{}') vs {} (from '{}')",
                                field.name,
                                module_name,
                                self.static_value_to_string(existing_id),
                                existing_provider,
                                self.static_value_to_string(value_id),
                                provider_name,
                            );
                        }
                    }
                }
            }
        }

        let mut merged: List<StaticValueId, _> =
            self.static_values.mem.new_list(schema_fields.len() as u32);
        for (i, schema_field) in schema_fields.iter().enumerate() {
            let value_id = match bound[i] {
                Some((id, _)) => id,
                None => match &defaults {
                    Some(d) => d[i],
                    None => kbail!(
                        self,
                        call_span,
                        "Missing required parameter '{}' for module '{}'",
                        self.ident_str(schema_field.name),
                        self.ident_str(module_name)
                    ),
                },
            };
            merged.push(value_id);
        }
        let fields_slice = merged.to_slice();
        let merged_id = self
            .static_values
            .add(StaticValue::Struct(StaticStruct { type_id: schema_type, fields: fields_slice }));
        self.modules.get_mut(module_id).params =
            Some(ModuleParams { schema_type, value_id: merged_id });
        Ok(merged_id)
    }

    fn add_function(&mut self, mut function: TypedFunction) -> FunctionId {
        let id = self.functions.next_id();
        if let Some(specialization_info) = &mut function.specialization_info {
            specialization_info.specialized_function_id = id;
            self.functions
                .get_mut(specialization_info.parent_function)
                .child_specializations
                .push_grow(&mut self.mem, *specialization_info);
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
        self.ir.functions.add(None);
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
        let ability_args = ability.kind.arguments(&self.mem);
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
        let ability_args = ability.kind.arguments(&self.mem);
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
            } else if matches!(self.types.get(self_type_id), Type::Sum(_) | Type::Struct(_)) {
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
            specialized_ability.kind.arguments(&self.mem).iter().zip(parameter_requirements.iter())
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
        let args = specialized_ability.kind.arguments(&self.mem);
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

        let blanket_arguments = blanket_ability.kind.arguments(&self.mem);

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
        let blanket_arguments = blanket_ability.kind.arguments(&self.mem);

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
            Err(e) => {
                debug!("Could not solve all blanket impl params: {}", self.ident_str(e.message));
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

        let blanket_ability_args =
            self.abilities.get(blanket_impl.ability_id).kind.arguments(&self.mem);
        let base_ability_params = self.abilities.get(generic_base_ability_id).parameters;
        let mut substituted_ability_args: List<TypeId, _> =
            self.mem.new_list(blanket_ability_args.len() as u32);
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
        let substituted_ability_args_handle = substituted_ability_args.to_slice();
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
                    if self.get_function(fn_id).is_macro {
                        kbail!(
                            self,
                            name.name_span,
                            "Macro '{}' cannot be used as a value",
                            self.ast.idents.get_string(name.name),
                        );
                    }
                    Ok((None, self.function_to_reference(fn_id, variable_name_span)))
                }
            },
            Some((variable_id, variable_scope_id)) => {
                self.check_lambda_capture_boundary(
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

    /// Captures are declared in a lambda's capture list and bind ordinary body-scope
    /// variables, so a use inside a lambda that reaches a local declared outside it
    /// is always an undeclared capture.
    fn check_lambda_capture_boundary(
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
                let (field_index, _target_field) = struct_type
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
                        let StaticValue::Struct(static_struct) = self.static_values.get(value_id)
                        else {
                            ice_span!(self, span, "Expected struct value type")
                        };
                        let field_value_id =
                            self.static_values.mem.get_nth(static_struct.fields, field_index);
                        let expr_id = self.add_static_value_expr(*field_value_id, span);
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
        let expected_type = match ctx.expected_type_id {
            None => None,
            Some(t) => Some(self.get_type_id_dereferenced(t)),
        };
        let input = self.eval_expr(base_expr, ctx.with_expected_type(expected_type))?;
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

    fn eval_expr(
        &mut self,
        expr_id: ParsedExprId,
        mut ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
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
            let allow_addr_of = ctx.is_method_receiver();
            let coerced_expr = self_
                .check_and_coerce_expr(expected_type_id, result_expr, ctx.scope_id, allow_addr_of)
                .map_err(|e| {
                    kerr!(
                        &**self_,
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
            ParsedExpr::BinaryOp(_binary_op) => self.eval_binary_op(expr_id, ctx),
            ParsedExpr::UnaryOp(op) => {
                let op = *op;
                match op.op_kind {
                    ParsedUnaryOpKind::BooleanNegation => {
                        let negated_expr = self.synth_parsed_bool_not(op.expr, op.span);
                        self.eval_expr(negated_expr, ctx)
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
                let true_expression = self.ast.exprs.add(
                    parse::ParsedExpr::Literal(parse::ParsedLiteral::Bool(true, is_expr.span)),
                    false,
                    None,
                );
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
                let match_expr_id =
                    self.ast.exprs.add(parse::ParsedExpr::Match(as_match_expr), false, None);
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

    fn add_static_value_expr(&mut self, value_id: StaticValueId, span: SpanId) -> TypedExprId {
        let inner_type_id = self.get_static_value_type(value_id);
        let static_type_id = self.add_value_type(inner_type_id, Some(value_id));
        self.exprs.add_static(value_id, static_type_id, true, span)
    }

    fn add_static_constant_expr(&mut self, value_id: StaticValueId, span: SpanId) -> TypedExprId {
        let type_id = self.get_static_value_type(value_id);
        self.exprs.add_static(value_id, type_id, false, span)
    }

    /// A constant `code` value: a list of chunks, each text plus its source span
    fn make_static_code_value(&mut self, chunks: &[(StringId, SpanId)]) -> StaticValueId {
        let code_type = self.builtin_types.code();
        let chunk_type = self.builtin_types.code_chunk();
        let chunks_list_type =
            self.instantiate_generic_type(self.builtin_types.list(), &[chunk_type]);
        let mut elements = self.static_values.mem.new_list(chunks.len() as u32);
        for (text, source) in chunks {
            let text_value = self.static_values.add_string(*text);
            let source_value =
                self.static_values.add_int(TypedIntValue::U64(source.as_u32() as u64));
            let chunk_value =
                self.static_values.add_struct_from_slice(chunk_type, &[text_value, source_value]);
            elements.push(chunk_value);
        }
        let elements = elements.to_slice();
        let chunks_value = self.static_values.add(StaticValue::LinearContainer(StaticContainer {
            elements,
            kind: StaticContainerKind::List,
            type_id: chunks_list_type,
        }));
        self.static_values.add_struct_from_slice(code_type, &[chunks_value])
    }

    fn code_from_parsed_expr(&mut self, parsed_expr: ParsedExprId) -> StaticValueId {
        let arg_span = self.ast.exprs.get_span(parsed_expr);
        let content =
            self.ast.sources.get_span_content(&self.ast.mem, self.ast.spans.get(arg_span));
        let string_id = self.ast.idents.intern(content);
        let value_id = self.make_static_code_value(&[(string_id, arg_span)]);
        value_id
    }

    fn materialize_static_value(
        &mut self,
        family_type_id: TypeId,
        value_id: Option<StaticValueId>,
        span: SpanId,
    ) -> TypedExprId {
        match value_id {
            Some(value_id) => self.add_static_constant_expr(value_id, span),
            None => self.synth_phony(family_type_id, span),
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

        let mut list_lit_block =
            self.new_block_builder(ctx.scope_id, ScopeType::LexicalBlock, span, 2 + element_count);
        let list_lit_scope = list_lit_block.scope_id;
        let mut element_type = None;
        let elements: List<TypedExprId, MemTmp> = {
            let mut elements = self.tmp.new_list(element_count);
            for elem in self.ast.mem.getn(list_expr.elements) {
                let current_expected_type = element_type.or(expected_element_type);
                let element_expr =
                    self.eval_expr(*elem, ctx.with_expected_type(current_expected_type))?;
                let element_expr_checked = match current_expected_type {
                    None => element_expr,
                    Some(current_expected_type) => self
                        .check_and_coerce_expr(
                            current_expected_type,
                            element_expr,
                            list_lit_scope,
                            false,
                        )
                        .map_err(|e| {
                            kerr!(self, e.span, "List element had incorrect type: {}", e.message)
                        })?,
                };
                let this_element_type = self.exprs.get_type(element_expr_checked);
                if element_type.is_none() {
                    // Erase static type info since a list of all one static value isn't very useful
                    let chased_type = self.get_static_family_id_if_static(this_element_type);
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
                    kbail!(self, span, "Not enough information to determine empty list type");
                } else {
                    self.builtin_types.empty
                }
            }
        };
        let list_lit_ctx = ctx.with_scope(list_lit_scope).with_no_expected_type();
        let count_expr = self.synth_i64(element_count as i64, span);
        let make_dest_coll = match list_kind {
            ContainerKind::List => self.synth_typed_call_typed_args(
                self.ast.idents.f.List_with_capacity.with_span(span),
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
            // Unlike the others, the array literal should go on the stack!
            ContainerKind::Array(array_type_id) => self.synth_typed_call_typed_args(
                self.ast.idents.f.mem_zeroed.with_span(span),
                &[array_type_id],
                &[],
                list_lit_ctx,
                false,
            )?,
        };
        let needs_address_of = match list_kind {
            ContainerKind::Array(_) => true,
            ContainerKind::Buffer | ContainerKind::Span => false,
            ContainerKind::List => true,
        };
        let dest_coll_variable = self.synth_variable_defn(
            self.ast.idents.b.dest,
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
            let push_call = match list_kind {
                ContainerKind::List => self.synth_typed_call_typed_args(
                    self.ast.idents.f.List_push.with_span(span),
                    &[element_type],
                    &[dest_coll_expr, *element_value_expr],
                    list_lit_ctx,
                    false,
                )?,
                ContainerKind::Buffer | ContainerKind::Span => self.synth_typed_call_typed_args(
                    self.ast.idents.f.buffer_set.with_span(span),
                    &[element_type],
                    &[dest_coll_expr, index_expr, *element_value_expr],
                    list_lit_ctx,
                    false,
                )?,
                ContainerKind::Array(array_type_id) => {
                    // fn set[T, N: static size](array: Array[T, N]*, index: size, value: T): unit
                    let size_type = self.types.get(array_type_id).as_array().unwrap().size_type;
                    self.synth_typed_call_typed_args(
                        self.ast.idents.f.Array_set.with_span(span),
                        &[element_type, size_type],
                        &[dest_coll_expr, index_expr, *element_value_expr],
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

    /// Compiles `#static <expr>` and `#meta <expr>` constructs
    fn compile_static_or_meta(
        &mut self,
        _expr_id: ParsedExprId,
        stat: ParsedStaticExpr,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let span = stat.span;
        let base_expr = stat.base_expr;

        if matches!(stat.kind, ParsedStaticBlockKind::MacroCall) {
            debug_assert!(is_definition);
            let ParsedExpr::Call(call) = self.ast.exprs.get(base_expr) else {
                kbail!(self, span, "Expected a macro call following '$'");
            };
            let call = *call;
            let Some(function_id) = self.find_function_namespaced(ctx.scope_id, &call.name)? else {
                kbail!(
                    self,
                    span,
                    "Unknown macro '{}'; a definition-level macro must live in the module's `pre` namespace or an upstream module",
                    &call.name
                );
            };
            if !self.get_function(function_id).is_macro {
                kbail!(
                    self,
                    span,
                    "'{}' is not a macro; only macros can be invoked with '$'",
                    &call.name
                );
            }
            let mut macro_args: SV8<_> = smallvec![];
            for parsed_arg in self.ast.mem.getn(call.args) {
                macro_args.push(MacroArg::Parsed(*parsed_arg))
            }
            return self.execute_macro_call(
                self.ast.mem.getn(call.type_args),
                &macro_args,
                span,
                function_id,
                true,
                ctx,
            );
        }

        // We don't execute statics during the generic pass, since there's no point
        // 1. we don't know the real types of generics, thus values of things like schemas, etc
        // 2. There's not really a use-case for it, metaprograms always want to generate
        //    real code
        //
        // So we just return the expected type, or a unit
        debug!("eval_static_expr ctx.is_generic_pass={}", ctx.is_generic_pass());
        if ctx.is_generic_pass() {
            let phony_type = ctx.expected_type_id.unwrap_or(EMPTY_TYPE_ID);
            let phony_expr = self.synth_phony(phony_type, span);
            return Ok(StaticExecutionResult::TypedExpr(phony_expr));
        }

        let kind = stat.kind;
        let expected_type_for_execution = match kind {
            ParsedStaticBlockKind::Value => match ctx.expected_type_id {
                None => None,
                Some(expected_type_id) => match self.get_static_type_of_type(expected_type_id) {
                    Some(s) => Some(s.family_type_id),
                    None => Some(expected_type_id),
                },
            },
            ParsedStaticBlockKind::Metaprogram => self.builtin_types.code,
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        };
        let mut static_parameters: SV4<(VariableId, StaticValueId)> = smallvec![];
        for param in self.ast.mem.getn(stat.parameter_names) {
            let variable_expr = self.ast.exprs.add(
                ParsedExpr::Variable(ParsedVariable {
                    name: QIdent::naked(param.name, param.span),
                    span: param.span,
                }),
                false,
                None,
            );
            let (variable_id, variable_expr) = self.eval_variable(variable_expr, ctx, false)?;
            let Some(variable_id) = variable_id else {
                kbail!(self, param.span, "Must be a plain variable");
            };
            let variable_type = self.exprs.get_type(variable_expr);

            self.register_variable_usage(variable_id, param.span);
            debug!(
                "Variable {} will be passed in to static execution",
                self.ident_str(self.variables.get(variable_id).name),
            );
            match self.types.get(variable_type) {
                Type::StaticValue(svt) => {
                    if let Some(value_id) = svt.value_id {
                        static_parameters.push((variable_id, value_id));
                    } else {
                        kbail!(
                            self,
                            param.span,
                            "Value type parameter `{}` is unresolved",
                            param.name
                        );
                    }
                }
                Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                    let static_type =
                        self.types.get(tp.static_constraint.unwrap()).as_value_type().unwrap();
                    let Some(value_id) = static_type.value_id else {
                        kbail!(
                            self,
                            param.span,
                            "Expected a statically-known, 'value' type for argument {}, got a value family",
                            param.name,
                        );
                    };
                    static_parameters.push((variable_id, value_id));
                }
                _ => {
                    kbail!(
                        self,
                        param.span,
                        "Expected a statically-known, 'value' type for argument {}, but got a {}",
                        param.name,
                        variable_type
                    );
                }
            }
        }
        let exec_ctx = ctx.with_expected_type(expected_type_for_execution).with_static_ctx(Some(
            StaticExecContext { expected_return_type: expected_type_for_execution },
        ));
        match kind {
            ParsedStaticBlockKind::Value => {
                let vm_result =
                    self.execute_static_expr(base_expr, exec_ctx, &static_parameters)?;
                let expr = self.add_static_value_expr(vm_result, span);
                Ok(StaticExecutionResult::TypedExpr(expr))
            }
            ParsedStaticBlockKind::Metaprogram => {
                let emitted = self.do_with_vm(span, |k1, vm| {
                    let result = (|| {
                        let expr = k1.compile_parsed_expr_for_exec(
                            base_expr,
                            exec_ctx,
                            &static_parameters,
                        )?;
                        let raw = bc::exec::execute_compiled_expr_raw(k1, vm, expr, true)?;
                        Self::read_emitted_code_raw(k1, &raw, span, is_definition)
                    })();
                    vm.reset(k1.global_id_k1_arena);
                    result
                })?;
                self.compile_emitted_code(emitted, span, ctx, is_definition)
            }
            ParsedStaticBlockKind::MacroCall => unreachable!(),
        }
    }

    /// must run while vm memory is still good
    fn read_emitted_code_raw(
        k1: &mut TypedProgram,
        raw: &bc::exec::RawUnitResult,
        span: SpanId,
        is_definition: bool,
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        use crate::vm::k1_types::{K1Code, K1CodeChunk};
        if !raw.returns_value || Some(raw.result_type_id) != k1.builtin_types.code {
            kbail!(
                k1,
                span,
                "Metaprogram must evaluate to `code`; got {}. Wrap a plain string with code/from-string",
                raw.result_type_id
            );
        }
        #[cfg(debug_assertions)]
        k1.assert_code_layouts();
        let ferry_start = k1.timing.clock.raw();
        let code = unsafe { *(raw.ret_addr as *const K1Code) };
        let chunks = unsafe {
            std::slice::from_raw_parts(
                code.chunks.k1_buffer.data as *const K1CodeChunk,
                code.chunks.len as usize,
            )
        };
        let emitted = k1.build_emitted_source(span, is_definition, chunks);
        k1.timing.total_ferry_nanos += k1.timing.elapsed_nanos(ferry_start) as i64;
        emitted
    }

    fn build_emitted_source(
        &mut self,
        span: SpanId,
        is_definition: bool,
        chunks: &[vm::k1_types::K1CodeChunk],
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        let chunks_len: usize = chunks.iter().map(|c| c.text.len as usize).sum();
        if chunks_len == 0 {
            return Ok(None);
        }
        let (source, line) = self.get_span_location(span);
        let directory = self.ast.idents.get_string(source.directory);
        let filename = self.ast.idents.get_string(source.filename);
        // The source keeps `content` forever, so size it exactly and move it
        // in; 48 covers the header boilerplate and block wrapper
        let mut content = String::with_capacity(chunks_len + directory.len() + filename.len() + 48);
        writeln!(
            &mut content,
            "// generated by #meta block at {}{}{}:{}",
            directory,
            std::path::MAIN_SEPARATOR,
            filename,
            line.line_number(),
        )
        .unwrap();
        if !is_definition {
            content.push_str("{\n");
        }
        let mut table: List<CodeChunkPos, _> = self.mem.new_list(chunks.len() as u32);
        for chunk in chunks {
            let Ok(text) = (unsafe { chunk.text.to_str() }) else {
                kbail!(self, span, "Metaprogram produced a non-utf8 chunk");
            };
            if text.is_empty() {
                continue;
            }
            let start = content.len() as u32;
            content.push_str(text);
            if let Some(source) = SpanId::from_u32(chunk.source as u32) {
                table.push(CodeChunkPos { start, end: start + text.len() as u32, source });
            }
        }
        if !is_definition {
            content.push_str("\n}");
        }
        Ok(Some((content, table.to_slice())))
    }

    #[cfg(debug_assertions)]
    fn assert_code_layouts(&mut self) {
        use crate::vm::k1_types::{K1Code, K1CodeChunk};
        let code_type = self.builtin_types.code.unwrap();
        let chunk_type = self.builtin_types.code_chunk.unwrap();
        assert_eq!(self.get_layout(code_type).unwrap().size as usize, size_of::<K1Code>());
        assert_eq!(self.get_layout(chunk_type).unwrap().size as usize, size_of::<K1CodeChunk>());
        let fields = self.get_struct_layout(chunk_type);
        assert_eq!(fields[0].offset as usize, std::mem::offset_of!(K1CodeChunk, text));
        assert_eq!(fields[1].offset as usize, std::mem::offset_of!(K1CodeChunk, source));
    }

    /// parses emitted source as a fresh file, then compiles it in place of the invocation
    fn compile_emitted_code(
        &mut self,
        emitted: Option<(String, PermSlice<CodeChunkPos>)>,
        span: SpanId,
        ctx: EvalExprContext,
        is_definition: bool,
    ) -> K1Result<StaticExecutionResult> {
        let Some((content, table)) = emitted else {
            return if is_definition {
                Ok(StaticExecutionResult::Definitions(MSlice::empty()))
            } else {
                Ok(StaticExecutionResult::TypedExpr(self.synth_empty_value(span)))
            };
        };
        // Parse the code as its own file (cohesive spans, a source containing
        // the full text), then compile it in place of the invocation
        //
        // TODO: when specializing, include the specialization context in the
        //       filename and print the types at the top of the file; a
        //       'what are we compiling' stack would provide it
        let (source, line) = self.get_span_location(span);
        let line_number = line.line_number();
        let stem = self.ast.idents.get_string(source.filename).strip_suffix(".k1").unwrap();
        let serial = self.emitted_sources.len() + 1;
        let generated_filename =
            self.ast.idents.intern(format!("meta_{stem}_{line_number}_{serial}.k1"));
        let generated_dir = self.config.out_dir_generated;
        debug!("Emitted source:\n---\n{content}\n---");
        let emitted_file = crate::parse::SourceFile::make(
            &mut self.ast.mem,
            generated_dir,
            generated_filename,
            &content,
        );
        let source_for_emission = self.ast.sources.add_file(emitted_file);
        self.emitted_sources.push(EmittedSource {
            file_id: source_for_emission,
            call_span: span,
            entries: table,
            has_diagnostic: false,
        });

        let parse_kind =
            if is_definition { ParseAdHocKind::Definitions } else { ParseAdHocKind::Expr };
        let parsed_metaprogram = self.parse_metaprogram_source(source_for_emission, parse_kind)?;
        match parsed_metaprogram {
            ParseMetaprogramResult::Expr(parsed_expr_id) => {
                let typed_metaprogram = self.eval_expr(parsed_expr_id, ctx)?;
                debug!("Emitted compiled expr:\n{}", self.expr_to_string(typed_metaprogram));
                Ok(StaticExecutionResult::TypedExpr(typed_metaprogram))
            }
            ParseMetaprogramResult::Definitions(defns_slice) => {
                Ok(StaticExecutionResult::Definitions(defns_slice))
            }
        }
    }

    /// Emitted sources accumulate in the sources pool during typechecking; this
    /// writes them out for inspection in one pass, off the expansion path
    pub fn write_emitted_sources(&self) {
        let start = std::time::Instant::now();
        for emitted in &self.emitted_sources {
            if emitted.has_diagnostic {
                let source = self.ast.sources.get(emitted.file_id);
                let path = kpath::join_tmp(
                    self.get_tmp_unsafe(),
                    &self.ast.idents,
                    source.directory,
                    source.filename,
                );
                if let Err(e) =
                    std::fs::write(Path::new(path.as_str()), source.content(&self.ast.mem))
                {
                    eprintln!("Failed to write out generated metaprogram at {path}. {e}");
                }
            }
        }
        if self.config.chatty {
            let elapsed = start.elapsed();
            eprintln!("Wrote {} emitted sources in {:.2?}", self.emitted_sources.len(), elapsed);
        }
    }

    fn execute_macro_call(
        &mut self,
        type_args: &[NamedTypeArg],
        args: &[MacroArg],
        span: SpanId,
        function_id: FunctionId,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let function = self.get_function(function_id);
        let is_generic = !function.is_concrete;
        let function_name = function.name;
        let function_type_params = function.type_params;
        let Some(parsed_macro_id) = function.parsed_id.as_macro_id() else {
            self.ice_span(span, "Macro function without a parsed macro")
        };
        if function.type_params.len() as usize != type_args.len() {
            kbail!(
                self,
                span,
                "Takes {} type arguments; got {}",
                function.type_params.len(),
                type_args.len()
            );
        }

        let type_expr_context = EvalTypeExprContext {
            is_direct_function_parameter: true,
            is_inside_type_definition_rhs: false,
            is_inside_static_type: false,
        };
        let (typed_type_args, _subst_pairs) = self.check_type_args_against_params(
            function_type_params,
            type_args,
            ctx.scope_id,
            type_expr_context,
        )?;

        let parsed_params = self.ast.get_macro(parsed_macro_id).params;
        if args.len() != parsed_params.len() as usize {
            kbail!(
                self,
                span,
                "Macro '{}' takes {} arguments, got {}",
                function_name,
                parsed_params.len(),
                args.len()
            );
        }

        // Ensure the function is compiled.
        self.eval_function_body(function_id)?;

        // Specialize if needed
        let function_to_run = if is_generic {
            let type_args = TypeArgs::from_slice_in(self.mem.getn(typed_type_args), &mut self.mem);
            let spec_fn_id =
                self.specialize_function_declaration(type_args, TypeArgs::empty(), function_id);
            self.specialize_function_body(spec_fn_id)?;
            spec_fn_id
        } else {
            function_id
        };

        let fn_param_types = self
            .types
            .get(self.get_function(function_to_run).type_id)
            .as_function()
            .unwrap()
            .logical_params();

        let mut static_args: SV8<StaticValueId> = smallvec![];
        for (param, arg) in self.mem.getn(fn_param_types).iter().zip(args.iter()) {
            match *arg {
                MacroArg::Parsed(parsed_arg) => {
                    if let Some(passed_name) = parsed_arg.name {
                        if passed_name != param.name {
                            kbail!(
                                self,
                                span,
                                "Macro argument name mismatch: expected {}, got {}",
                                param.name,
                                passed_name,
                            );
                        }
                    }
                    match param.is_macro_code {
                        true => {
                            // when the parameter is of type code,
                            // it gets auto 'quoted' at the callsite
                            // So we just yoink the source text from its span
                            // and actually ignore the previously parsed value.
                            // This means its kinda hard to synthesize one of these arguments
                            //
                            // which is why we're accepting MaybeTypedExpr here
                            let code_value_id = self.code_from_parsed_expr(parsed_arg.value);
                            static_args.push(code_value_id);
                        }
                        false => {
                            let value_id = self.execute_static_expr(
                                parsed_arg.value,
                                ctx.with_expected_type(Some(param.type_id)),
                                &[],
                            )?;
                            let value_type = self.get_static_value_type(value_id);
                            if let Err(msg) =
                                self.check_types(param.type_id, value_type, ctx.scope_id)
                            {
                                kbail!(
                                    self,
                                    span,
                                    "Macro argument '{}' type mismatch: {}",
                                    param.name,
                                    msg,
                                );
                            }
                            static_args.push(value_id);
                        }
                    };
                }
                MacroArg::Typed(typed_arg) => {
                    // We require a static value
                    if !param.is_macro_code {
                        kbail!(
                            self,
                            span, /* call span since we want to point the finger at the bad caller */
                            "bug: we only allow pre-typed macro arguments for code args",
                        );
                    }
                    let TypedExpr::StaticValue(sce) = self.exprs.get(typed_arg) else {
                        kbail!(
                            self,
                            span,
                            "bug: we expect a static value for pre-typed macro arguments",
                        );
                    };
                    static_args.push(sce.value_id)
                }
            }
        }

        self.run_macro_and_compile_output(function_to_run, &static_args, span, is_definition, ctx)
    }

    fn run_macro_and_compile_output(
        &mut self,
        function_id: FunctionId,
        static_args: &[StaticValueId],
        span: SpanId,
        is_definition: bool,
        ctx: EvalExprContext,
    ) -> K1Result<StaticExecutionResult> {
        let emitted = self.do_with_vm(span, |k1, vm| {
            let result =
                Self::macro_emit_with_vm(k1, vm, function_id, static_args, span, is_definition);
            vm.reset(k1.global_id_k1_arena);
            result
        })?;
        self.compile_emitted_code(emitted, span, ctx, is_definition)
    }

    fn macro_emit_with_vm(
        k1: &mut TypedProgram,
        vm: &mut vm::Vm,
        function_id: FunctionId,
        static_args: &[StaticValueId],
        span: SpanId,
        is_definition: bool,
    ) -> K1Result<Option<(String, PermSlice<CodeChunkPos>)>> {
        Self::static_exec_prepare_function(k1, function_id, span)?;
        let raw = bc::exec::execute_compiled_function_raw(k1, vm, function_id, static_args, true)?;
        Self::read_emitted_code_raw(k1, &raw, span, is_definition)
    }

    fn with_parser<R>(
        &mut self,
        file_id: FileId,
        f: impl FnOnce(&mut parse::Parser) -> K1Result<R>,
    ) -> K1Result<R> {
        let mut tokens = std::mem::take(&mut self.buffers.lexer_tokens);
        tokens.clear();

        let module = self.modules.get(self.module_in_progress.unwrap());
        let parsed_namespace_id = module.parsed_namespace_id;
        let code_str = self.ast.sources.get(file_id).content(&self.ast.mem);
        let mut lexer = crate::lex::Lexer::make(code_str, &mut self.ast.spans, file_id);
        if let Err(e) = lexer.run(&mut tokens) {
            let e = ParseError::Lex(e);
            parse::print_error(&self.ast, &e);
            tokens.clear();
            self.buffers.lexer_tokens = tokens;
            kbail!(self, e.span(), "Failed to lex code emitted from here");
        };

        let mut p = crate::parse::Parser::make_for_file(
            module.id,
            module.name,
            parsed_namespace_id,
            &mut self.ast,
            &tokens,
            file_id,
        );

        let r = f(&mut p);
        tokens.clear();
        self.buffers.lexer_tokens = tokens;
        r
    }

    fn parse_metaprogram_source(
        &mut self,
        file_id: FileId,
        kind: ParseAdHocKind,
    ) -> K1Result<ParseMetaprogramResult> {
        self.with_parser(file_id, move |p| {
            let msg_base = "Failed to parse the code you returned: ";
            let error_count_start = p.ast.errors.len();
            match kind {
                ParseAdHocKind::Expr => match p.expect_expression() {
                    Err(e) => Err(make_message(
                        &p.ast.idents,
                        format!("{msg_base}{e}"),
                        e.span(),
                        MessageLevel::Error,
                    )),
                    Ok(parsed_expr) => {
                        if p.ast.errors.len() > error_count_start {
                            let e = p.ast.errors.last().unwrap().clone();
                            Err(make_message(
                                &p.ast.idents,
                                format!("{msg_base}{e}"),
                                e.span(),
                                MessageLevel::Error,
                            ))
                        } else {
                            Ok(ParseMetaprogramResult::Expr(parsed_expr))
                        }
                    }
                },
                ParseAdHocKind::Definitions => {
                    let defns = p.parse_definitions(TokenKind::Eof);
                    if p.ast.errors.len() > error_count_start {
                        let e = p.ast.errors.last().unwrap().clone();
                        Err(make_message(
                            &p.ast.idents,
                            format!("{msg_base}{e}"),
                            e.span(),
                            MessageLevel::Error,
                        ))
                    } else {
                        Ok(ParseMetaprogramResult::Definitions(defns.to_mslice()))
                    }
                }
            }
        })
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
        let original_expected_struct_id = ctx.expected_type_id.unwrap();
        let Type::Struct(original_expected_struct) = self.types.get(original_expected_struct_id)
        else {
            self.ice_span(self.ast.get_expr_span(expr_id), "expected an expected struct type")
        };
        let original_expected_struct = *original_expected_struct;
        if original_expected_struct.record_kind == RecordKind::Union {
            kbail!(
                self,
                self.ast.get_expr_span(expr_id),
                "Cannot use struct literal syntax to construct a union. Expected type {} is a union.",
                original_expected_struct_id
            );
        }
        let expected_struct_id = original_expected_struct_id;
        let expected_struct = *self.types.get(expected_struct_id).expect_struct();
        let expected_struct_defn_info = self.get_defn_info(expected_struct_id);
        let field_count = expected_struct.fields.len();

        let mut passed_fields_aligned: SV8<(
            Option<ParsedExprId>,
            &StructValueField,
            &StructTypeField,
        )> = smallvec![];

        let struct_span = parsed_struct.span;
        for (index, expected_field) in self.mem.getn(expected_struct.fields).iter().enumerate() {
            let Some(passed_field) = self
                .ast
                .mem
                .getn(parsed_struct.fields)
                .iter()
                .find(|f| f.name == expected_field.name)
            else {
                kbail!(
                    self,
                    struct_span,
                    "Struct is missing expected field '{}'",
                    expected_field.name
                );
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
            passed_fields_aligned.push((parsed_expr, passed_field, expected_field))
        }

        if let Some(unknown_field) =
            self.ast.mem.getn(parsed_struct.fields).iter().find(|passed_field| {
                original_expected_struct.find_field(&self.mem, passed_field.name).is_none()
            })
        {
            kbail!(self, struct_span, "Struct has an unexpected field '{}'", unknown_field.name);
        }

        let mut field_values: List<StructLiteralField, _> = self.mem.new_list(field_count);
        let mut field_types: List<StructTypeField, _> = self.mem.new_list(field_count);
        for ((passed_expr, passed_field, _), expected_field) in
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
                        kbail!(self, passed_field.span, "never is not allowed in struct literals");
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
                        self.pretty_print_types(gi.type_args.as_slice(&self.mem), ", "),
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
                    let (solutions, _all_solutions) = self.with_clean_inference(|k1| {
                        k1.infer_types(
                            generic_params_slice,
                            generic_params,
                            &subst_pairs,
                            struct_span,
                            ctx.scope_id,
                            None,
                        )
                    })?;
                    debug!(
                        "I reverse-engineered these: {}",
                        self.pretty_print_types(solutions.as_slice(&self.mem), ", ")
                    );
                    gi.type_args = solutions;
                    Some(gi)
                } else {
                    Some(gi)
                }
            }
        };
        let output_struct = StructType {
            fields: field_types.to_slice(),
            record_kind: original_expected_struct.record_kind,
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
            ScopeLoopInfo { break_type: Some(self.builtin_types.empty) },
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
            let parsed_var = self.ast.exprs.add(
                ParsedExpr::Variable(parse::ParsedVariable {
                    name: QIdent::naked(capture.name, capture.span),
                    span: capture.span,
                }),
                false,
                None,
            );
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
                self.ast.idents.b.env,
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
                abi_mode: AbiMode::Internal,
            }));

            let body_function_id = self.functions.next_id();
            {
                self.scopes.get_scope_mut(lambda_scope_id).scope_type = ScopeType::FunctionScope;
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
                child_specializations: MList::empty(),
                specialization_info: None,
                parsed_id: expr_id.into(),
                type_id: function_type,
                compiler_debug: false,
                kind: TypedFunctionKind::Lambda,
                is_concrete: false,
                is_recursive: false,
                is_macro: false,
                is_reloadable: false,
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
            abi_mode: AbiMode::Internal,
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
            child_specializations: MList::empty(),
            specialization_info: None,
            parsed_id: expr_id.into(),
            type_id: function_type,
            compiler_debug: false,
            kind: TypedFunctionKind::Lambda,
            // Set by add_function
            is_concrete: false,
            is_recursive: false,
            is_macro: false,
            is_reloadable: false,
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

    fn eval_match_expr(
        &mut self,
        match_expr_id: ParsedExprId,
        ctx: EvalExprContext,
        check_exhaustive: bool,
        allow_bindings: bool,
        fallback_expr: Option<TypedExprId>,
    ) -> K1Result<TypedExprId> {
        let parsed_match = *self.ast.exprs.get(match_expr_id).as_match().unwrap();
        if parsed_match.is_static {
            return self.eval_static_match_expr(match_expr_id, ctx);
        };
        if parsed_match.cases.is_empty() {
            return self.make_fail(
                "match with no arms; note `x is {}` is an empty match, `x is .{}` matches the empty struct",
                parsed_match.span,
            );
        }
        let subject_expr =
            self.eval_expr(parsed_match.match_subject, ctx.with_no_expected_type())?;

        let match_subject_variable =
            self.synth_variable_defn_simple(self.ast.idents.b.subject, subject_expr, ctx.scope_id);

        let match_expr_span = parsed_match.span;

        let parsed_cases = parsed_match.cases;
        let parsed_pattern_count: u32 = self
            .ast
            .mem
            .getn(parsed_cases)
            .iter()
            .map(|parsed_case| parsed_case.patterns.len())
            .sum();

        let mut typed_arms: List<TypedMatchArm, _> = self.mem.new_list(parsed_pattern_count + 1); // Add one for fallback arm

        let mut expected_arm_type_id = ctx.expected_type_id;

        let mut all_unguarded_patterns: List<TypedPatternId, MemTmp> =
            self.tmp.new_list(parsed_pattern_count);
        let subject_type = self.exprs.get_type(match_subject_variable.variable_expr);
        let subject_expr_span = self.exprs.get_span(match_subject_variable.variable_expr);

        // Core loop to build up the typed, compiled match arms
        let mut first_error = None;
        for parsed_case in self.ast.mem.getn(parsed_cases) {
            let multi_pattern = parsed_case.patterns.len() > 1;
            let mut expected_bindings: Option<SmallVec<[VariablePattern; 8]>> = None;
            for parsed_pattern_id in parsed_case.patterns.as_slice(&self.ast.mem).iter() {
                let pattern = self.compile_pattern_to_type(
                    *parsed_pattern_id,
                    subject_type,
                    ctx.scope_id,
                    allow_bindings,
                )?;
                let pattern_bindings = self.patterns.get_pattern_bindings(pattern);

                // If a match arm has multiple patterns, they must produce the exact same
                // set of variable bindings: matching name and type
                if multi_pattern {
                    match &expected_bindings {
                        None => {
                            expected_bindings = Some(pattern_bindings.clone());
                        }
                        Some(expected_bindings) => {
                            let this_pattern_bindings = &pattern_bindings;
                            if this_pattern_bindings.is_empty() && !expected_bindings.is_empty() {
                                kbail!(
                                    self,
                                    self.patterns.get(pattern).span_id(),
                                    "Patterns in a multiple pattern arm must have the exact same bindings; but this one has none"
                                );
                            }
                            for (exp_binding, this_binding) in
                                expected_bindings.iter().zip(this_pattern_bindings.iter())
                            {
                                if exp_binding.name != this_binding.name {
                                    kbail!(
                                        self,
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings"
                                    );
                                }
                                if exp_binding.type_id != this_binding.type_id {
                                    kbail!(
                                        self,
                                        this_binding.span,
                                        "Patterns in a multiple pattern arm must have the exact same bindings; but the type differs for {}: {} vs {}",
                                        exp_binding.name,
                                        exp_binding.type_id,
                                        this_binding.type_id
                                    );
                                }
                            }
                        }
                    }
                }

                if parsed_case.guard_condition_expr.is_none() {
                    all_unguarded_patterns.push(pattern);
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
                    let pattern_eval_ctx = if pattern_bindings.is_empty() {
                        ctx.with_no_expected_type()
                    } else {
                        let arm_scope_id = self.scopes.add_child_scope(
                            ctx.scope_id,
                            ScopeType::MatchArm,
                            ScopeOwnerId::None,
                        );
                        ctx.with_scope(arm_scope_id).with_no_expected_type()
                    };
                    let mut instrs = self.mem.new_list(8);
                    self.compile_pattern_into_values(
                        pattern,
                        match_subject_variable.variable_expr,
                        &mut instrs,
                        false,
                        pattern_eval_ctx,
                    )?;

                    if let Some(guard_condition_expr_id) = parsed_case.guard_condition_expr {
                        let guard_condition_expr = self.eval_expr(
                            guard_condition_expr_id,
                            pattern_eval_ctx.with_expected_type(Some(BOOL_TYPE_ID)),
                        )?;
                        instrs.push(MatchingConditionInstr::cond(guard_condition_expr));
                    };

                    // Once we've evaluated the conditions, we can eval the consequent expression inside of it,
                    // since the bindings are now available
                    let consequent_result = self.eval_expr_with_coercion(
                        parsed_case.expression,
                        pattern_eval_ctx.with_expected_type(expected_arm_type_id),
                        true,
                    );
                    let consequent_expr = match consequent_result {
                        Err(err) => {
                            self.report(err);
                            first_error = Some(err);
                            continue;
                        }
                        Ok(expr) => expr,
                    };
                    let consequent_expr_type = self.exprs.get_type(consequent_expr);

                    if expected_arm_type_id.is_none() && consequent_expr_type != NEVER_TYPE_ID {
                        // We chase down the type because, if its a static, it doesn't really make
                        // sense to expect every arm to evaluate to the same static, but rather to
                        // the static's inner type
                        let chased_consequent_id =
                            self.get_static_family_id_if_static(consequent_expr_type);
                        expected_arm_type_id = Some(chased_consequent_id);
                    }

                    let match_arm = TypedMatchArm {
                        condition: MatchingCondition { instrs: instrs.to_slice() },
                        consequent_expr,
                    };
                    // An arm over an uninhabited variant can never match: it is
                    // typechecked but not lowered, so codegen never sees its
                    // impossible bindings
                    if !self.pattern_matches_uninhabited(pattern) {
                        typed_arms.push(match_arm);
                    }
                }
            }
        }

        if let Some(err) = first_error {
            return Err(err);
        }

        // Exhaustiveness Checking
        if check_exhaustive {
            self.check_pattern_exhaustiveness(
                subject_type,
                all_unguarded_patterns.as_slice_mut(),
                subject_expr_span,
                false,
            )?
        }
        let fallback_value = match fallback_expr {
            Some(e) => e,
            None => self.synth_crash_call(
                if check_exhaustive {
                    self.ast.idents.b.crash_msg_no_cases_exhaustive
                } else {
                    self.ast.idents.b.crash_msg_no_cases
                },
                match_expr_span,
                ctx.with_no_expected_type(),
            )?,
        };
        let fallback_arm = TypedMatchArm {
            condition: MatchingCondition { instrs: MSlice::empty() },
            consequent_expr: fallback_value,
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
                arms: typed_arms.to_slice(),
            }),
            match_result_type,
            match_expr_span,
        ))
    }

    fn eval_static_match_expr(
        &mut self,
        match_expr_id: ParsedExprId,
        ctx: EvalExprContext,
    ) -> K1Result<TypedExprId> {
        // Our job is to evaluate the conditions statically. That means either compiling the condition
        // chains into static exprs and running them, or just allowing only trivial patterns

        let ParsedExpr::Match(parsed_match) = self.ast.exprs.get(match_expr_id) else { panic!() };
        let parsed_match = *parsed_match;
        let match_target =
            self.execute_static_expr(parsed_match.match_subject, ctx.with_no_expected_type(), &[])?;
        let subject_span = self.ast.exprs.get_span(parsed_match.match_subject);
        let StaticValue::Enum(target_type_id, enum_value) = *self.static_values.get(match_target)
        else {
            kbail!(self, subject_span, "Only enums are supported in static match for now");
        };
        let enum_members = self.types.get(target_type_id).expect_enum().member_values;
        let Some(target_member) =
            self.mem.getn(enum_members).iter().find(|m| m.int_value == enum_value)
        else {
            self.ice_span(subject_span, "Tag didn't match any variants")
        };
        let target_member_name = target_member.name;

        let mut given_cases = self.tmp.new_list(enum_members.len());
        let mut uncovered_members = self.tmp.new_list(enum_members.len());
        uncovered_members.extend_iter(self.mem.getn(enum_members).iter().map(|m| m.name));
        for case in self.ast.mem.getn(parsed_match.cases) {
            if let Some(guard_expr) = case.guard_condition_expr {
                kbail!(
                    self,
                    self.ast.exprs.get_span(guard_expr),
                    "Guard conditions are not supported in static match for now"
                );
            }
            for pattern_id in case.patterns.as_slice(&self.ast.mem) {
                let compiled_pattern_id =
                    self.compile_pattern_to_type(*pattern_id, target_type_id, ctx.scope_id, false)?;
                let pattern = self.patterns.get(compiled_pattern_id);
                let TypedPattern::Enum(enum_pattern) = pattern else {
                    kbail!(
                        self,
                        self.ast.get_pattern_span(*pattern_id),
                        "Only enum patterns are supported in static match for now"
                    );
                };
                uncovered_members.swap_remove_elem(&enum_pattern.member_name);
                given_cases.push_grow(&mut self.tmp, (*enum_pattern, case.expression));
            }
        }

        if !uncovered_members.is_empty() {
            let mut uncovered_member_names = String::new();
            for (idx, name) in uncovered_members.iter().enumerate() {
                if idx > 0 {
                    uncovered_member_names.push_str(", ");
                }
                uncovered_member_names.push_str(self.ident_str(*name));
            }
            kbail!(
                self,
                parsed_match.span,
                "Non-exhaustive static match: the following variants were not covered: {}",
                uncovered_member_names
            );
        }

        let mut matched = None;
        for (pattern, expr) in given_cases.iter() {
            if pattern.member_name == target_member_name {
                matched = Some(*expr);
            }
        }

        match matched {
            None => Err(kerr!(self, parsed_match.span, "No cases matched")),
            Some(expr) => self.eval_expr(expr, ctx),
        }
    }

    /// A pattern over an uninhabited variant (e.g. `:err e` on result[t, never])
    /// kills no ctors, but is not useless: generic code must write the arm
    fn pattern_matches_uninhabited(&self, pattern_id: TypedPatternId) -> bool {
        match self.patterns.get(pattern_id) {
            TypedPattern::Sum(sp) => {
                let payload_is_never = match self.types.get(sp.sum_type_id) {
                    Type::Sum(sum_type) => self
                        .mem
                        .getn(sum_type.variants)
                        .get(sp.variant_index as usize)
                        .and_then(|v| v.payload)
                        .is_some_and(|p| p == NEVER_TYPE_ID),
                    _ => false,
                };
                payload_is_never || sp.payload.is_some_and(|p| self.pattern_matches_uninhabited(p))
            }
            TypedPattern::Struct(stp) => self
                .patterns
                .mem
                .getn(stp.fields)
                .iter()
                .any(|f| self.pattern_matches_uninhabited(f.pattern)),
            TypedPattern::Reference(refer) => self.pattern_matches_uninhabited(refer.inner_pattern),
            _ => false,
        }
    }

    fn check_pattern_exhaustiveness(
        &mut self,
        subject_type: TypeId,
        all_unguarded_patterns: &mut [TypedPatternId],
        subject_span: SpanId,
        skip_build_message: bool,
    ) -> K1Result<()> {
        let mut trial_constructors = std::mem::take(&mut self.buffers.trial_ctors);
        let mut field_ctors_buf = std::mem::take(&mut self.buffers.field_ctors);
        let mut visited_ancestors = std::mem::take(&mut self.buffers.pattern_ctor_ancestor_stack);
        trial_constructors.clear();
        self.generate_constructors_for_type(
            subject_type,
            &mut trial_constructors,
            &mut field_ctors_buf,
            0,
            &mut visited_ancestors,
            subject_span,
        );
        debug_assert!(visited_ancestors.is_empty());

        let mut kill_counts = self.tmp.new_list::<usize>(all_unguarded_patterns.len() as u32);
        kill_counts.fill_to_cap(0);
        'trial: for trial_entry in trial_constructors.iter_mut() {
            '_pattern: for (pattern_index, pattern) in all_unguarded_patterns.iter().enumerate() {
                if self.pattern_eliminates_ctor(*pattern, trial_entry.ctor) {
                    kill_counts[pattern_index] += 1;
                    // *kill_count += 1;
                    trial_entry.alive = false;
                    continue 'trial;
                }
            }
        }

        let alive_count = trial_constructors.iter().filter(|entry| entry.alive).count();
        self.buffers.trial_ctors = trial_constructors;
        self.buffers.field_ctors = field_ctors_buf;
        self.buffers.pattern_ctor_ancestor_stack = visited_ancestors;
        if alive_count != 0 {
            let msg = if skip_build_message {
                "Unhandled patterns".to_string()
            } else {
                let mut patterns = String::new();
                for entry in self.buffers.trial_ctors.iter() {
                    if !entry.alive {
                        continue;
                    }
                    if !patterns.is_empty() {
                        patterns.push_str("\n- ");
                    }
                    patterns.push_str(&self.pattern_ctor_to_string(entry.ctor));
                }
                format!("{} Unhandled patterns:\n- {}", alive_count, patterns)
            };
            return self.make_fail(&msg, subject_span);
        }

        if let Some((useless_pattern_id, _)) = all_unguarded_patterns
            .iter()
            .zip(kill_counts.as_slice())
            .find(|(_, kill_count)| **kill_count == 0)
        {
            if !self.patterns.pattern_never_useless(*useless_pattern_id)
                && !self.pattern_matches_uninhabited(*useless_pattern_id)
            {
                kbail!(
                    self,
                    self.patterns.get(*useless_pattern_id).span_id(),
                    "This pattern handled no cases: {}",
                    self.pattern_to_string(*useless_pattern_id)
                );
            }
        }
        Ok(())
    }

    fn _eval_static_match_expr(&mut self, _match_expr_id: ParsedExprId, _ctx: EvalExprContext) {
        todo!("support static matches")
    }

    /// Accumulates a list of 'MatchingConditionInstr' while 'compiling' a pattern match.
    /// Basically, every part of a pattern match boils down to either
    /// - A boolean condition to be evaluated
    /// - A new variable binding
    fn compile_pattern_into_values(
        &mut self,
        pattern_id: TypedPatternId,
        target_expr: TypedExprId,
        instrs: &mut List<MatchingConditionInstr, TypedProgram>,
        is_immediately_inside_reference_pattern: bool,
        ctx: EvalExprContext,
    ) -> K1Result<()> {
        let target_expr_type = self.exprs.get_type(target_expr);
        let pat = self.patterns.get(pattern_id);
        match pat {
            TypedPattern::Struct(struct_pattern) => {
                let pattern_fields = struct_pattern.fields;
                let is_referencing = is_immediately_inside_reference_pattern;
                let struct_base = self.synth_dereference_when(target_expr, is_referencing);
                for pattern_field in self.patterns.get_slice(pattern_fields).iter() {
                    let get_struct_field = self.synth_field_access(
                        struct_base,
                        pattern_field.field_index as usize,
                        SpanId::NONE,
                    );
                    let final_field = if is_referencing {
                        // Infallible: when referencing, struct_base is a Deref
                        self.synth_address_of(get_struct_field, SpanId::NONE, false).unwrap()
                    } else {
                        get_struct_field
                    };
                    let var_name = self.build_ident_with(|k1, s| {
                        write!(s, "field_{}", k1.ident_str(pattern_field.name)).unwrap();
                    });
                    let struct_field_variable =
                        self.synth_variable_defn_simple(var_name, final_field, ctx.scope_id);
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
            TypedPattern::Sum(sum_pattern) => {
                let sum_pattern = *sum_pattern;
                let is_referencing = is_immediately_inside_reference_pattern;
                let sum_base = self.synth_dereference_when(target_expr, is_referencing);
                let is_variant_condition = self.synth_sum_is_variant(
                    sum_base,
                    sum_pattern.variant_index,
                    Some(sum_pattern.span),
                )?;
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(is_variant_condition));

                if let Some(payload_pattern) = sum_pattern.payload {
                    let sum_type_id = sum_pattern.sum_type_id;
                    let sum_type = self.types.get(sum_type_id).expect_sum();
                    let variant =
                        self.sum_variant_by_index(sum_type.variants, sum_pattern.variant_index);
                    let variant_name = variant.name;
                    let variant_index = variant.index;
                    let Some(payload_type_id) = variant.payload else {
                        kbail!(
                            self,
                            sum_pattern.span,
                            "Impossible pattern: Variant '{}' does not have data",
                            variant_name
                        );
                    };
                    let get_payload_expr = self.exprs.add(
                        TypedExpr::SumGetPayload(GetSumPayload {
                            sum_expr: sum_base,
                            variant_index,
                            packed: self.is_place_in_packed(sum_base),
                        }),
                        payload_type_id,
                        sum_pattern.span,
                    );
                    let final_payload_expr = if is_referencing {
                        // Infallible: when referencing, sum_base is a Deref
                        self.synth_address_of(get_payload_expr, SpanId::NONE, false).unwrap()
                    } else {
                        get_payload_expr
                    };
                    let payload_variable = self.synth_variable_defn_simple(
                        variant_name,
                        final_payload_expr,
                        ctx.scope_id,
                    );
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
                let binding_variable = self.synth_variable_defn_visible(
                    variable_ident,
                    target_expr,
                    ctx.scope_id,
                    variable_pattern.span,
                );
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
            TypedPattern::RefNull(_inner_type, span) => {
                let span = *span;
                let target_expr_as_ptr = self.synth_cast(
                    target_expr,
                    POINTER_TYPE_ID,
                    CastType::ReferenceToPointer,
                    Some(span),
                );
                let ptr_null_expr =
                    self.add_static_constant_expr(self.static_values.nullptr_id(), span);
                let is_null_expr =
                    self.synth_equals_call_simple(target_expr_as_ptr, ptr_null_expr, span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(is_null_expr));
                Ok(())
            }
            TypedPattern::PointerNull(span) => {
                let span = *span;
                let ptr_null_expr =
                    self.add_static_constant_expr(self.static_values.nullptr_id(), span);
                let is_null_expr = self.synth_equals_call_simple(target_expr, ptr_null_expr, span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(is_null_expr));
                Ok(())
            }
            TypedPattern::Type(pattern) => {
                // We want to push a cond to instrs representing whether the type matches
                let pattern = *pattern;
                let pattern_did_match = target_expr_type == pattern.type_id;
                debug!(
                    "type {} == {}? {pattern_did_match}",
                    self.type_id_to_string(target_expr_type),
                    self.type_id_to_string(pattern.type_id)
                );
                let cond = self.synth_bool(pattern_did_match, pattern.span);
                instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(cond));
                let inner_target_expr = if pattern_did_match {
                    // The variable is already of the correct type, so don't do anything at all
                    target_expr
                } else {
                    // The type pattern failed, and the consequent code will never run, but we need
                    // it to typecheck
                    self.synth_phony(pattern.type_id, pattern.span)
                };
                self.compile_pattern_into_values(
                    pattern.inner_pattern,
                    inner_target_expr,
                    instrs,
                    false,
                    ctx,
                )?;
                Ok(())
            }
            literal_pat => {
                match literal_pat {
                    TypedPattern::LiteralChar(_, _) => {}
                    TypedPattern::LiteralInteger(_, _) => {}
                    TypedPattern::LiteralFloat(_, _) => {}
                    TypedPattern::LiteralBool(_, _) => {}
                    TypedPattern::LiteralString(_, _) => {}
                    TypedPattern::Enum(_) => {}
                    _ => unreachable!("all non-literals should be handled by now"),
                };
                let target_expr = if is_immediately_inside_reference_pattern {
                    // Literal patterns don't do anything special for references; they just need to
                    // function on the de-rereferenced target. Whereas structs, sums, even
                    // reference patterns do different and unique things when matching on
                    // references
                    self.synth_dereference(target_expr)
                } else {
                    target_expr
                };
                match self.patterns.get(pattern_id) {
                    // Not truly a 'literal' but closer to a literal than to an aggregate
                    TypedPattern::Enum(e) => {
                        let span = e.span;
                        let int_value = e.int_value;
                        let pattern_int_value = self.static_values.add(StaticValue::Int(int_value));
                        let pattern_int_value_expr =
                            self.add_static_constant_expr(pattern_int_value, span);
                        let target_int_value = self.synth_enum_get_value(target_expr, span);
                        let equals_call = self.synth_equals_call_simple(
                            target_int_value,
                            pattern_int_value_expr,
                            span,
                        );
                        instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(equals_call));
                        Ok(())
                    }
                    TypedPattern::LiteralChar(byte, span) => {
                        let char_value = self.static_values.add(StaticValue::Char(*byte));
                        let span = *span;
                        let char_expr = self.add_static_constant_expr(char_value, span);
                        let equals_pattern_char =
                            self.synth_equals_call_simple(target_expr, char_expr, span);
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::cond(equals_pattern_char),
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralInteger(int_value, span) => {
                        let span = *span;
                        let pattern_integer_literal =
                            self.add_static_constant_expr(*int_value, span);
                        let equals_pattern_int = self.synth_equals_call_simple(
                            target_expr,
                            pattern_integer_literal,
                            span,
                        );
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
                            self.synth_equals_call_simple(target_expr, pattern_float_literal, span);
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::cond(equals_pattern_float),
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralBool(bool_value, span) => {
                        let span = *span;
                        let bool_expr = self.synth_bool(*bool_value, span);
                        let equals_pattern_bool =
                            self.synth_equals_call_simple(target_expr, bool_expr, span);
                        instrs.push_grow(
                            &mut self.mem,
                            MatchingConditionInstr::cond(equals_pattern_bool),
                        );
                        Ok(())
                    }
                    TypedPattern::LiteralString(string_id, span) => {
                        let span = *span;
                        let string_expr = self.synth_string_literal(*string_id, span);
                        let condition =
                            self.synth_equals_call_simple(target_expr, string_expr, span);
                        instrs.push_grow(&mut self.mem, MatchingConditionInstr::cond(condition));
                        Ok(())
                    }
                    _ => {
                        unreachable!(
                            "should only be literal patterns from here: {}",
                            self.pattern_to_string(pattern_id)
                        )
                    }
                }
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
            self.report(kwarn!(self, span, "Useless cast"));
            return Ok(base_expr);
        }
        enum Outcome {
            Cast(CastType),
            Expr(TypedExprId),
        }
        let cast_type = match self.types.get(base_expr_type) {
            Type::Integer(from_integer_type) => match self.types.get(target_type) {
                Type::Integer(to_integer_type) => {
                    let cast_type = match from_integer_type.width().cmp(&to_integer_type.width()) {
                        Ordering::Less => {
                            // Extend
                            if from_integer_type.is_signed() && !to_integer_type.is_signed() {
                                kbail!(
                                    self,
                                    span,
                                    "Cannot widen from {} to {}; its unclear whether sign or zero extension should occur",
                                    from_integer_type,
                                    to_integer_type
                                );
                            }
                            CastType::IntegerCast(IntegerCastDirection::Extend)
                        }
                        Ordering::Greater => CastType::IntegerCast(IntegerCastDirection::Truncate),
                        // Likely a sign change
                        Ordering::Equal => CastType::IntegerCast(IntegerCastDirection::NoOp),
                    };
                    Ok(Outcome::Cast(cast_type))
                }
                Type::Char => {
                    if from_integer_type.width() == NumericWidth::B8 {
                        Ok(Outcome::Cast(CastType::Integer8ToChar))
                    } else {
                        Err(kerr!(
                            self,
                            span,
                            "Cannot cast integer '{}' to char, must be 8 bits",
                            from_integer_type
                        ))
                    }
                }
                Type::Pointer => match from_integer_type {
                    IntegerType::U64 | IntegerType::I64 => {
                        Ok(Outcome::Cast(CastType::WordToPointer))
                    }
                    _ => Err(kerr!(
                        self,
                        span,
                        "Cannot cast integer '{}' to Pointer (must be word-sized (64-bit))",
                        from_integer_type
                    )),
                },
                Type::Float(_to_float_type) => {
                    // We're just going to allow these casts and make it UB if it doesn't fit, the LLVM
                    // default. If I find a saturating version in LLVM I'll use that instead
                    match from_integer_type {
                        IntegerType::U8
                        | IntegerType::U16
                        | IntegerType::U32
                        | IntegerType::U64 => Ok(Outcome::Cast(CastType::IntegerUnsignedToFloat)),
                        IntegerType::I8
                        | IntegerType::I16
                        | IntegerType::I32
                        | IntegerType::I64 => Ok(Outcome::Cast(CastType::IntegerSignedToFloat)),
                    }
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast integer '{}' to '{}'",
                    from_integer_type,
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Float(from_float_type) => match self.types.get(target_type) {
                Type::Float(to_float_type) => {
                    match from_float_type.size().cmp(&to_float_type.size()) {
                        Ordering::Less => Ok(Outcome::Cast(CastType::FloatExtend)),
                        Ordering::Greater => Ok(Outcome::Cast(CastType::FloatTruncate)),
                        Ordering::Equal => Err(kerr!(self, span, "Useless float cast")),
                    }
                }
                Type::Integer(to_int_type) => match to_int_type {
                    IntegerType::U32 => Ok(Outcome::Cast(CastType::FloatToUnsignedInteger)),
                    IntegerType::U64 => Ok(Outcome::Cast(CastType::FloatToUnsignedInteger)),
                    IntegerType::I32 => Ok(Outcome::Cast(CastType::FloatToSignedInteger)),
                    IntegerType::I64 => Ok(Outcome::Cast(CastType::FloatToSignedInteger)),
                    _ => Err(kerr!(
                        self,
                        span,
                        "Cannot cast float to integer '{}'",
                        self.type_id_to_string(target_type).blue()
                    )),
                },
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast float to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Char => match self.types.get(target_type) {
                Type::Integer(_to_integer_type) => {
                    Ok(Outcome::Cast(CastType::IntegerExtendFromChar))
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast char to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Bool => match self.types.get(target_type) {
                Type::Integer(_to_integer_type) => {
                    // Since the bit pattern of a boolean only
                    // concerns the first bit, it can validly
                    // and safely be represented as all of our
                    // integer types
                    Ok(Outcome::Cast(CastType::BoolToInt))
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast bool to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Reference(_refer) => match self.types.get(target_type) {
                Type::Pointer => Ok(Outcome::Cast(CastType::ReferenceToPointer)),
                Type::AbilityObject(_) => Ok(Outcome::Expr(self.ability_impl_to_dyn_object(
                    base_expr,
                    target_type,
                    ctx.scope_id,
                    span,
                )?)),
                Type::Reference(_) => {
                    self.report_warn(
                        span,
                        format!(
                            "{} to {} (todo: detect safer vs less safe reference casts)",
                            self.type_id_to_string(base_expr_type),
                            self.type_id_to_string(target_type)
                        ),
                    );
                    Ok(Outcome::Cast(CastType::ReferenceToReference))
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast reference to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::FunctionPointer(from_fp) => match self.types.get(target_type) {
                Type::FunctionPointer(to_fp) => {
                    // For good c interop, its really nice to be able to cast function pointers to
                    // slightly different types, for example if we have a typesafe version of a type
                    // with a raw void* baton, our type has a *mut t and the baton has ptr. We want
                    // to allow this
                    let from_type =
                        *self.types.get(from_fp.function_type_id).as_function().unwrap();
                    let to_type = *self.types.get(to_fp.function_type_id).as_function().unwrap();
                    if from_type.physical_params.len() != to_type.physical_params.len() {
                        kbail!(
                            self,
                            span,
                            "Cannot cast between function types: parameter count mismatch: {} vs {}",
                            from_type.physical_params.len(),
                            to_type.physical_params.len()
                        );
                    }
                    if let Err(msg) =
                        self.check_types(from_type.return_type, to_type.return_type, ctx.scope_id)
                    {
                        self.report_warn(
                            span,
                            format!("Cannot guarantee the return types are compatible; {}", msg),
                        )
                    }
                    for (from_param, to_param) in
                        self.mem.getn_zip(from_type.physical_params, to_type.physical_params)
                    {
                        // We can't allow any ABI-affecting casts, so have to be careful. Basically,
                        // nothing that changes the physical type.
                        if let Err(msg) =
                            self.check_types(from_param.type_id, to_param.type_id, ctx.scope_id)
                        {
                            // Pointers and references are abi-identical. It'd be nice to also do
                            // structs with 1 pointer member
                            match (
                                self.types.get(from_param.type_id),
                                self.types.get(to_param.type_id),
                            ) {
                                (Type::Pointer, Type::Reference(_)) => {}
                                (Type::Reference(_), Type::Pointer) => {}
                                _ => self.report_warn(
                                    span,
                                    format!(
                                        "Cannot guarantee these params are compatible; {}",
                                        msg
                                    ),
                                ),
                            }
                        }
                    }
                    Ok(Outcome::Cast(CastType::ReferenceToPointer))
                }
                Type::Pointer => Ok(Outcome::Cast(CastType::ReferenceToPointer)),
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast Function Pointer to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Pointer => match self.types.get(target_type) {
                Type::Reference(_refer) => Ok(Outcome::Cast(CastType::PointerToReference)),
                Type::Integer(IntegerType::U64) => Ok(Outcome::Cast(CastType::PointerToWord)),
                Type::Integer(IntegerType::I64) => Ok(Outcome::Cast(CastType::PointerToWord)),
                Type::FunctionPointer(_) => Ok(Outcome::Cast(CastType::PointerToFunctionPointer)),
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast ptr to '{}'",
                    self.type_id_to_string(target_type).blue()
                )),
            },
            Type::Enum(enum_type) => match self.types.get(target_type) {
                Type::Integer(int_type) if *int_type == enum_type.int_type => {
                    let e = self.synth_enum_get_value(base_expr, span);
                    Ok(Outcome::Expr(e))
                }
                _ => Err(kerr!(
                    self,
                    span,
                    "Cannot cast enum '{}' to '{}'",
                    self.type_id_to_string(base_expr_type).blue(),
                    self.type_id_to_string(target_type).blue()
                )),
            },
            _ => Err(kerr!(
                self,
                span,
                "Cannot cast '{}' to '{}'",
                self.type_id_to_string(base_expr_type).blue(),
                self.type_id_to_string(target_type).blue()
            )),
        }?;
        match cast_type {
            Outcome::Cast(cast_type) => {
                Ok(self.synth_cast(base_expr, target_type, cast_type, Some(span)))
            }
            Outcome::Expr(typed_expr_id) => Ok(typed_expr_id),
        }
    }

    fn eval_for_expr(&mut self, for_expr: &ForExpr, ctx: EvalExprContext) -> K1Result<TypedExprId> {
        // Basically no overlap here in what we need to do.
        if for_expr.is_static {
            return self.eval_static_for_expr(for_expr, ctx);
        };
        if FOR_VIA_MACRO {
            let body_block_expr =
                self.ast.exprs.add(ParsedExpr::Block(for_expr.body_block), false, None);

            let binding = match for_expr.binding {
                None => {
                    let code_value_id =
                        self.make_static_code_value(&[(self.ast.idents.b.it, for_expr.span)]);
                    let code_expr = self.add_static_constant_expr(code_value_id, for_expr.span);
                    MacroArg::Typed(code_expr)
                }
                Some(binding) => MacroArg::Parsed(ParsedCallArg::unnamed(binding)),
            };

            let function_id = self
                .scopes
                .find_function_local(self.scopes.core_scope_id, self.ast.idents.b.for_each)
                .unwrap();
            let StaticExecutionResult::TypedExpr(macro_result) = self.execute_macro_call(
                &[],
                &[
                    binding,
                    MacroArg::Parsed(ParsedCallArg::unnamed(for_expr.iterable_expr)),
                    MacroArg::Parsed(ParsedCallArg::unnamed(body_block_expr)),
                ],
                for_expr.span,
                function_id,
                false,
                ctx,
            )?
            else {
                unreachable!()
            };
            return Ok(macro_result);
        }

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
            self.scopes.add_child_scope(ctx.scope_id, ScopeType::ForExpr, ScopeOwnerId::None);

        let zero_expr = self.synth_i64(0, for_expr.body_block.span);
        let index_variable = self.synth_variable_defn(
            self.ast.idents.b.it_index,
            zero_expr,
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
                    let iterable_name = self.ast.idents.b.iterable;
                    let iterable_variable = self.synth_variable_defn(
                        iterable_name,
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
            self.new_block_builder(outer_for_expr_scope, ScopeType::LexicalBlock, body_span, 3);
        let loop_scope_id = loop_block.scope_id;

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
        let next_getvalue_call = self.synth_typed_call_typed_args(
            self.ast.idents.f.try__get_value.with_span(iterable_span),
            &[],
            &[next_variable.variable_expr],
            ctx.with_scope(consequent_block.scope_id).with_no_expected_type(),
            false,
        )?;
        let binding_variable = self.synth_variable_defn_visible(
            binding_ident,
            next_getvalue_call,
            consequent_block.scope_id,
            binding_span,
        );
        let body_block = self.eval_block(
            &for_expr.body_block,
            ctx.with_scope(consequent_block.scope_id).with_no_expected_type(),
            false,
        )?;

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

        let body_block = self.exprs.add_block(loop_block, self.builtin_types.empty);
        let loop_expr = self.exprs.add(
            TypedExpr::LoopExpr(LoopExpr { body_block }),
            self.builtin_types.empty,
            for_expr.span,
        );

        let mut for_expr_initial_statements = self.mem.new_list(5);
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
        let iteree_value_id =
            self.execute_static_expr(for_expr.iterable_expr, ctx.with_no_expected_type(), &[])?;
        let iteree_value = self.static_values.get(iteree_value_id);
        let iteree_span = self.ast.get_expr_span(for_expr.iterable_expr);
        let Some(iteration_list) = iteree_value.as_container() else {
            kbail!(
                self,
                iteree_span,
                "Expected something iterable; got: {}",
                self.static_value_to_string(iteree_value_id)
            );
        };
        let elements = iteration_list.elements;

        let mut block = self.new_block_builder(
            ctx.scope_id,
            ScopeType::LexicalBlock,
            for_expr.span,
            iteration_list.len() as u32 * 2,
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
        for elem in self.static_values.mem.getn(elements) {
            let elem_expr = self.add_static_constant_expr(*elem, iteree_span);
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
        check_exhaustive: Option<(bool, &mut Option<K1Message>)>,
        ctx: EvalExprContext,
    ) -> K1Result<MatchingConditionResult> {
        debug!("matching condition: {}", self.ast.expr_id_to_string(condition));
        let mut all_patterns: SV4<(TypedPatternId, TypedExprId)> = smallvec![];
        let mut allow_bindings: bool = true;
        let mut instrs: List<MatchingConditionInstr, _> = self.mem.new_list(16);
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
                        &mut [pattern_id],
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
                instrs: instrs.to_slice(),
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
                self.ast.idents.f.bool__negated.with_span(binary_op.span),
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
        let new_fn_call_id = self.ast.exprs.add(ParsedExpr::Call(new_fn_call), false, None);
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
        let calling_scope = ctx.scope_id;
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
            let receiver = self.ast.mem.get_nth(fn_call.args, 0).value;
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

        // Method syntax only
        if !fn_call.is_method {
            if n == self.ast.idents.b.return_ {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    kbail!(self, fn_call.span, "return cannot be used inside `defer` blocks");
                }
                let ret_value = match fn_call.args.len() {
                    0 => Ok(None),
                    1 => {
                        let arg = self.ast.mem.get_nth(fn_call.args, 0);
                        Ok(Some(arg.value))
                    }
                    _ => Err(kerr!(self, fn_call.span, "return(...) must have 0 or 1 arguments")),
                }?;
                let return_expr_id = self.eval_return(ret_value, ctx, call_span)?;
                Ok(Some(return_expr_id))
            } else if n == self.ast.idents.b.break_ {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    kbail!(self, fn_call.span, "break cannot be used inside `defer` blocks");
                }
                if fn_call.args.len() > 1 {
                    kbail!(self, call_span, "break(...) must have 0 or 1 argument");
                }
                // Determine based on loop type if break with value is allowed
                let Some((enclosing_loop_scope_id, loop_type)) =
                    self.scopes.nearest_parent_loop(calling_scope)
                else {
                    kbail!(self, call_span, "break(...) outside of loop");
                };
                let expected_break_type: Option<TypeId> =
                    self.scopes.get_loop_info(enclosing_loop_scope_id).unwrap().break_type;

                let arg = self.ast.mem.get_nth_opt(fn_call.args, 0);
                let break_value = match arg {
                    None => self.synth_empty_value(call_span),
                    Some(fn_call_arg) => {
                        // ALTERNATIVE: Allow break with value from `while` loops but require the type to implement the `Default` trait
                        match loop_type {
                            LoopType::Loop => self.eval_expr(
                                fn_call_arg.value,
                                ctx.with_expected_type(expected_break_type),
                            )?,
                            LoopType::While => {
                                kbail!(
                                    self,
                                    call_span,
                                    "break with value is only allowed in `loop` loops, because loop body may not ever be executed"
                                );
                            }
                        }
                    }
                };
                let actual_break_type = self.exprs.get_type(break_value);
                if actual_break_type == NEVER_TYPE_ID {
                    kbail!(
                        self,
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
                        kbail!(self, call_span, "Break with wrong type: {msg}");
                    }
                } else {
                    self.scopes.add_loop_info(
                        enclosing_loop_scope_id,
                        ScopeLoopInfo { break_type: Some(actual_break_type) },
                    )
                }

                let defers = self.gather_defers(
                    calling_scope,
                    call_span,
                    DeferExtent::LoopScope(enclosing_loop_scope_id),
                );
                let break_expr = self.synth_defers_then_exit(
                    defers,
                    break_value,
                    ctx,
                    call_span,
                    |k1, value| {
                        k1.exprs.add(
                            TypedExpr::Break(TypedBreak {
                                value,
                                loop_scope: enclosing_loop_scope_id,
                            }),
                            NEVER_TYPE_ID,
                            call_span,
                        )
                    },
                )?;
                Ok(Some(break_expr))
            } else if n == self.ast.idents.b.continue_ {
                if ctx.flags.contains(EvalExprFlags::Defer) {
                    kbail!(self, fn_call.span, "continue cannot be used inside `defer` blocks");
                }
                if !fn_call.args.is_empty() {
                    kbail!(self, call_span, "continue takes no arguments");
                }
                let Some((enclosing_loop_scope_id, _)) =
                    self.scopes.nearest_parent_loop(calling_scope)
                else {
                    kbail!(self, call_span, "continue outside of loop");
                };
                let defers = self.gather_defers(
                    calling_scope,
                    call_span,
                    DeferExtent::LoopScope(enclosing_loop_scope_id),
                );
                let empty_value = self.synth_empty_value(call_span);
                let continue_expr = self.synth_defers_then_exit(
                    defers,
                    empty_value,
                    ctx,
                    call_span,
                    |k1, _value| {
                        k1.exprs.add(
                            TypedExpr::Continue { loop_scope: enclosing_loop_scope_id },
                            NEVER_TYPE_ID,
                            call_span,
                        )
                    },
                )?;
                Ok(Some(continue_expr))
            } else if n == self.ast.idents.b.test_compile {
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
                self.compile_all_pending_ir(call_span).map_err(|mut e| {
                    e.message = self.ast.idents.intern(format!(
                        "Failed to compile pending units before test-compile: {}",
                        self.ident_str(e.message)
                    ));
                    e
                })?;
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

    fn handle_array_method_call(
        &mut self,
        receiver: TypedExprId,
        array_type_id: TypeId,
        call: &ParsedCall,
        ctx: EvalExprContext,
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
            n if n == self.ast.idents.b.get => {
                if call.args.len() != 2 {
                    kbail!(self, span, "Array get takes 1 argument, the index");
                }
                let index_arg = self.ast.mem.get_nth(call.args, 1);
                let index_expr =
                    self.eval_expr(index_arg.value, ctx.with_expected_type(Some(SIZE_TYPE_ID)))?;
                let index_expr = self
                    .check_and_coerce_expr(SIZE_TYPE_ID, index_expr, ctx.scope_id, false)
                    .map_err(|e| kerr!(self, span, "Array get index type error: {}", e.message))?;

                let array_reference_type = self.get_expr_type(receiver).as_reference();
                let array_expr =
                    self.synth_dereference_when(receiver, array_reference_type.is_some());

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
                let crash_message =
                    self.synth_string_literal(self.ast.idents.b.crash_msg_array_oob, span);
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
                // `arr.get(i).&` and `arr.get(i) = v` work
                let unit_expr = self.synth_empty_value(span);
                let bounds_check_expr = self.synth_if_else(
                    self.builtin_types.empty,
                    is_in_bounds,
                    unit_expr,
                    crash_oob,
                    span,
                );
                let mut statements = self.mem.new_list(2);
                statements.push(self.add_expr_stmt(bounds_check_expr));
                statements.push(self.add_expr_stmt(get_element_expr));
                let block_expr = self.exprs.add_block(
                    BlockBuilder { scope_id: ctx.scope_id, statements, span },
                    array_type.element_type,
                );
                Ok(Some(CallResolution::OtherExpr(block_expr)))
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
                        if !function.is_concrete {
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
            } else if fn_name == self.ast.idents.b.as_ {
                let dest_type = match self.ast.mem.get_nth_opt(call.type_args, 0) {
                    None => match ctx.expected_type_id {
                        None => {
                            kbail!(self, call_span, "Cannot use as() with no expected type");
                        }
                        Some(et) => et,
                    },
                    Some(type_arg) => match type_arg.type_expr {
                        None => {
                            kbail!(self, call_span, "Cannot use as() with no expected type");
                        }
                        Some(type_expr) => self.eval_type_expr(type_expr, ctx.scope_id)?,
                    },
                };
                let result = self.eval_cast(base_arg.value, dest_type, call_span, ctx)?;
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
                self.handle_array_method_call(base_expr, base_for_method, call, ctx)?
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
                    .map(|(_, f)| self.ident_str(self.functions.get(f).name).to_string())
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

    pub fn function_to_reference(
        &mut self,
        function_id: FunctionId,
        call_span: SpanId,
    ) -> TypedExprId {
        let function = self.get_function(function_id);
        let function_pointer_type = self.add_function_pointer_type(function.type_id);
        self.emit_ls_entity(call_span, LsEntityKind::Function { function_id, is_defn: false });
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
        let call_conv = function_type.abi_mode;
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

        let new_function_type = FunctionType {
            physical_params: new_params.to_slice(),
            return_type,
            is_lambda: true,
            abi_mode: call_conv,
        };

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
            kbail!(
                self,
                span,
                "Some fields in the patch struct did not match any fields in the base struct (todo: name them)",
            );
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
            None,
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
        let (self_solution, other_solved) = self.infer_types(
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
        )?;

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

                let solved_or_passed_type_params: TypeArgs = if parsed_variant.type_args.is_empty()
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
                            let (solutions, _all_solutions) = self.infer_types(
                                g_params_slice,
                                g_params,
                                &args_and_params,
                                span,
                                ctx.scope_id,
                                None,
                            )?;
                            solutions
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
                    TypeArgs::from_slice_in(passed_params.as_slice(), &mut self.mem)
                };

                let concrete_type = self.instantiate_generic_type(
                    provided_type,
                    solved_or_passed_type_params.as_slice(&self.mem),
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
        generic_params: Option<MSlice<FnParamType, TypedProgram>>,
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
                let constraint_source_type = match generic_params {
                    None => context_param.type_id,
                    Some(gp) => self
                        .mem
                        .getn(gp)
                        .iter()
                        .find(|p| p.name == context_param.name)
                        .map(|p| p.type_id)
                        .unwrap_or(context_param.type_id),
                };
                let ability_match = self.find_context_variable_by_ability_constraints(
                    calling_scope,
                    constraint_source_type,
                    span,
                )?;
                let matching_context_variable = match ability_match {
                    Some(v) => {
                        if generic_params.is_some()
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
                    None => self
                        .scopes
                        .find_context_variable_by_type(calling_scope, context_param.type_id),
                };
                if let Some(matching_context_variable) = matching_context_variable {
                    let found = self.variables.get(matching_context_variable);
                    self.check_lambda_capture_boundary(
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
                    final_args.push(MaybeTypedExpr::Typed(self.exprs.add(
                        TypedExpr::Variable(VariableExpr {
                            variable_id: matching_context_variable,
                        }),
                        found.type_id,
                        span,
                    )));
                    self.register_variable_usage(matching_context_variable, fn_call.name.name_span);
                    final_params.push(*context_param);
                } else {
                    let is_source_loc =
                        context_param.type_id == self.builtin_types.source_location.unwrap();
                    if is_source_loc {
                        let expr = self.synth_source_location(span);
                        final_args.push(MaybeTypedExpr::Typed(expr));
                        final_params.push(*context_param);
                    } else if !tolerate_missing_context_args {
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
                        continue;
                    }
                };
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
        let result = self.eval_function_call_inner(fn_call, known_args, ctx, known_callee);
        self.tmp.reset_to(tmp_mark);
        result
    }

    fn eval_function_call_inner(
        &mut self,
        fn_call: &ParsedCall,
        known_args: Option<(&[TypeId], &[TypedExprId])>,
        ctx: EvalExprContext,
        known_callee: Option<Callee>,
    ) -> K1Result<TypedExprId> {
        let span = fn_call.span;
        debug!("eval_function_call {}", self.qident_to_string(&fn_call.name));
        assert!(
            fn_call.args.is_empty() || known_args.is_none(),
            "cannot pass both typed value args and parsed value args to eval_function_call"
        );
        // Arguments already evaluated during resolution/inference; avoids double-compiles where possible
        let mut stashed_args: SV8<(ParsedExprId, TypedExprId)> = smallvec![];
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

            if let Some(enclosing_id) = self.scopes.nearest_parent_function(ctx.scope_id) {
                if enclosing_id == function_id {
                    debug!(
                        "Marking {} as directly recursive (stopgap that does not properly detect cycles but helps for now)",
                        self.function_id_to_string(enclosing_id, false)
                    );
                    self.functions.get_mut(function_id).is_recursive = true;
                }
            }

            if self.get_function(function_id).is_macro
                && known_callee.is_none()
                && known_args.is_none()
            {
                if method_receiver.is_some() {
                    kbail!(self, span, "Method-position macros are not yet supported");
                }
                let type_args = self.ast.mem.getn(fn_call.type_args);
                let mut macro_args: SV8<_> = smallvec![];
                for arg in self.ast.mem.getn(fn_call.args) {
                    macro_args.push(MacroArg::Parsed(*arg))
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
                    None,
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
                    None,
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
                let specialized_params = specialized_fn_type.physical_params;
                let specialized_signature = self.get_callee_function_signature(&callee);
                let args_and_params = self.align_call_arguments_with_parameters(
                    fn_call,
                    &specialized_signature,
                    method_receiver,
                    specialized_params,
                    known_args.map(|(_known_types, known_args)| known_args),
                    ctx.scope_id,
                    false,
                    skip_leading_receiver_arg,
                    Some(params),
                )?;

                self.splice_stashed_args(args_and_params.args, &stashed_args);

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
            let arg = *self.mem.get_nth(args, i);
            if !self.expr_is_place_read(arg) {
                continue;
            }
            let Some(layout) = self.get_layout(param.type_id) else { continue };
            if layout.size < Self::LARGE_ARG_COPY_BYTES {
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

                // We generate a schema for every type for which a typeId is requested
                // This guarantees that we have it available at runtime when typeSchema is
                // called
                // Same for typeName
                self.register_type_metainfo(type_id);

                let type_id_expr = self.synth_type_id_literal(type_id, span);
                Ok(type_id_expr)
            }
            BuiltinTyperInline::TypeSize
            | BuiltinTyperInline::TypeStride
            | BuiltinTyperInline::TypeAlign => {
                let type_id = call.type_args.as_slice(&self.mem)[0];
                match self.get_physical_type(type_id) {
                    PhysicalTypeResult::No => Ok(self.synth_phony(SIZE_TYPE_ID, span)),
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
                    kbail!(self, call.span, "Cannot bitcast to unsized type: {}", type_from)
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
        let old_impl_ability_arguments = specialized_ability.kind.arguments(&self.mem);
        debug!(
            "Specializing constraint sig: {} on set {}",
            self.ability_impl_signature_to_string(
                signature.specialized_ability_id,
                old_impl_arguments
            ),
            self.pretty_print_type_substitutions(set, ", ")
        );
        let mut ability_args_new: List<TypeId, _> = self.mem.new_list(all_base_params.len());
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
        let ability_args_new_handle = ability_args_new.to_slice();
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
        &self,
        generic_function_id: FunctionId,
        type_arguments: TypeArgs,
        fnlike_type_arguments: TypeArgs,
    ) -> Option<FunctionId> {
        let type_arguments = type_arguments.as_slice(&self.mem);
        let fnlike_type_arguments = fnlike_type_arguments.as_slice(&self.mem);
        let generic_function = self.get_function(generic_function_id);
        for spec in generic_function.child_specializations.as_slice(&self.mem) {
            if spec.type_arguments.as_slice(&self.mem) == type_arguments
                && spec.fnlike_type_arguments.as_slice(&self.mem) == fnlike_type_arguments
            {
                return Some(spec.specialized_function_id);
            }
        }
        None
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
            type_arguments,
            fnlike_type_arguments,
            specialized_function_id: FunctionId::PENDING,
            specialized_function_type: specialized_function_type_id,
        };
        let generic_function = self.get_function(generic_function_id);
        let has_body = match generic_function.parsed_id {
            ParsedId::Function(f) => self.ast.get_function(f).body.is_some(),
            ParsedId::Macro(_) => true,
            _ => panic!("Expected function or macro"),
        };
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
            child_specializations: MList::empty(),
            specialization_info: Some(specialization_info),
            parsed_id: generic_function.parsed_id,
            type_id: specialized_function_type_id,
            compiler_debug: generic_function.compiler_debug,
            kind: generic_function.kind,
            is_concrete: false,
            is_recursive: generic_function.is_recursive,
            is_macro: generic_function.is_macro,
            // we reject generics in reloadable places
            is_reloadable: false,
            dyn_fn_id: None,
            returned_variable: None,
            body_failure: None,
        };
        let actual_specialized_function_id = self.add_function(specialized_function);
        debug_assert_eq!(specialized_function_id, actual_specialized_function_id);
        let is_concrete = self.get_function(specialized_function_id).is_concrete;

        self.scopes
            .set_scope_owner_id(spec_fn_scope, ScopeOwnerId::Function(specialized_function_id));

        if (has_body && is_concrete) || is_typer_function_builtin {
            self.functions_pending_body_specialization.push(specialized_function_id);
        }

        specialized_function_id
    }

    fn specialize_function_body(&mut self, function_id: FunctionId) -> K1Result<()> {
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
        let parent_function = self.get_function(parent_function);
        if let Some(err) = parent_function.body_failure.as_ref() {
            return Err(*err);
        }

        // Intrinsics from generic impls (e.g. `impl add for vector[t, n]`) have no
        // body to specialize; calls resolve through the copied builtin_type
        if parent_function.linkage == Linkage::Intrinsic && parent_function.body_block.is_none() {
            return Ok(());
        }

        // Approach: Synthesize the implementation for this builtin
        if let Some(Builtin::TyperPhysicalFunction(
            kind @ (BuiltinTyperFunction::StructPrintTo | BuiltinTyperFunction::SumPrintTo),
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
            for t in spec_info.type_arguments.as_slice(&self.mem) {
                if self.type_variable_counts.get(*t).is_abstract() {
                    return false;
                }
            }
            for t in spec_info.fnlike_type_arguments.as_slice(&self.mem) {
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
                for useable_symbol in &useable_symbols {
                    self.scopes.add_use_binding(
                        ctx.scope_id,
                        useable_symbol,
                        parsed_use.alias.unwrap_or(parsed_use.target.name),
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
            let unit_expr = self.ast.exprs.add(
                ParsedExpr::Struct(parse::ParsedStruct {
                    fields: MSlice::empty(),
                    span: block.span,
                }),
                false,
                None,
            );
            let stmt_id = self.ast.stmts.add(ParsedStmt::LoneExpression(unit_expr));
            let stmts = self.ast.mem.pushn(&[stmt_id]);
            unit_body = ParsedBlock { stmts, kind: block.kind, span: block.span };
            &unit_body
        } else {
            block
        };
        let mut stmts = self.mem.new_list(block.stmts.len() + 1);
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
                Some("sys") => match fn_name_str {
                    "exit" => Some(Builtin::Backend(BackendBuiltin::Exit)),
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
                    (Some("type-id"), "name") => Some(Builtin::Backend(BackendBuiltin::TypeName)),
                    (Some("type-id"), "schema") => {
                        Some(Builtin::Backend(BackendBuiltin::TypeSchema))
                    }
                    (None, "struct-create") => Some(Builtin::Backend(BackendBuiltin::StructCreate)),
                    _ => None,
                },
                Some("bool") => match fn_name_str {
                    "negated" => Some(Builtin::Ir(BuiltinIr::BoolNegate)),
                    _ => None,
                },
                Some("string") => None,
                Some("list") => None,
                Some("char") => None,
                Some("ptr") => match fn_name_str {
                    "ref-at-index" => Some(Builtin::Ir(BuiltinIr::PointerIndex)),
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
        let ability_args = ability.kind.arguments(&self.mem);
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
        let generic_function = self.functions.get(function_id);
        if generic_function.body_block.is_none() {
            self.eval_function_body(function_id)?;
        }
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
        // We take this as an interned slice
        // because we have to store it on the specialization info anyway
        arguments: TypeIdSlice,
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
        if arguments.len() > ability_parameters.len() {
            panic!("Passed too many arguments to specialize_ability; probably passed impl args");
        }
        let arguments_slice = self.mem.getn(arguments);
        if let Some(cached_specialization) = specializations
            .as_slice(&self.mem)
            .iter()
            .find(|spec| spec.arguments.as_slice(&self.mem) == arguments_slice)
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
            .mem
            .getn(arguments)
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
            arguments: TypeArgs::from_slice_in(self.mem.getn(arguments), &mut self.mem),
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
        let args = self.abilities.get(ability_id).kind.arguments(&self.mem);
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
            abi_mode: AbiMode::Internal,
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

        for arg in ability.kind.arguments(&self.mem) {
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
                    // Typing the fn-pointer expr with the slot's type is the erasure:
                    // physically identical; a self receiver arrives as the state pointer
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
                    // Inside an abstract (where-bound generic) body; never lowered.
                    let placeholder = self.synth_cast(
                        base_expr,
                        target_dyn_type,
                        CastType::AbilityImplToDynObject,
                        Some(span),
                    );
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
                        || impl_info.impl_kind.is_builtin_derived()
                })
            });
        let resolvable_by_name = !is_ability_impl && !ability_kind_is_specialized;

        let name = match impl_info.as_ref() {
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
        let call_conv = match linkage {
            Linkage::Standard => AbiMode::Internal,
            Linkage::External { .. } | Linkage::Exported { .. } => AbiMode::Native,
            Linkage::Intrinsic | Linkage::LlvmIntrinsic(_) => AbiMode::Internal,
        };
        let function_type_id = self_.add_type_anon(Type::Function(FunctionType {
            physical_params: param_types_handle,
            return_type,
            is_lambda: false,
            abi_mode: call_conv,
        }));

        let function_type_params_handle = fnlike_type_params.to_slice();
        let function_id = self_.functions.next_id();
        for v in params.iter() {
            self_.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(function_id);
        }
        let param_variables_handle = params.to_slice();
        let where_constraints_handle = self_.mem.pushn(&ability_where_constraints);
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
            child_specializations: MList::empty(),
            specialization_info: None,
            parsed_id: parsed_function_id.into(),
            kind,
            compiler_debug: is_debug,
            type_id: function_type_id,
            is_concrete: false,
            is_recursive: false,
            is_macro: false,
            is_reloadable,
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
            let predicate_functions_handle = predicate_functions.to_slice();
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
            abi_mode: AbiMode::Internal,
        }));

        let function_id = self.functions.next_id();
        for v in params.iter() {
            self.variables.get_mut(v.variable_id).kind = VariableKind::FnParam(function_id);
        }
        let param_variables_handle = params.to_slice();
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
            child_specializations: MList::empty(),
            specialization_info: None,
            parsed_id: ParsedId::Macro(parsed_macro_id),
            kind: TypedFunctionKind::Standard,
            compiler_debug,
            type_id: function_type_id,
            is_concrete: false,
            is_recursive: false,
            is_macro: true,
            is_reloadable: false,
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
        let is_debug = function.compiler_debug;
        if is_debug {
            self.push_debug_level();
        }
        let function = self.get_function(declaration_id);
        let fn_scope_id = function.scope;
        let return_type = self.get_function_type(declaration_id).return_type;
        let is_extern = matches!(function.linkage, Linkage::External { .. });
        let is_concrete = function.is_concrete;
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
            if !is_generic && !f.is_macro {
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
                // FIXME: a 'switch' would be much better, if we had them
                // But we'll synthesize a big if/else inline for now.
                let enum_param = *self.mem.get_nth(params, 0);
                let enum_param_expr = self.synth_variable_expr(enum_param.variable_id, fn_span);
                let enum_type_id = self.exprs.get_type(enum_param_expr);
                let enum_arg_int_expr = self.synth_enum_get_value(enum_param_expr, fn_span);
                let Type::Enum(enum_type) = self.types.get(enum_type_id) else {
                    self.ice_span(fn_span, "not an enum");
                };
                let mut arms: List<TypedMatchArm, _> =
                    self.mem.new_list(enum_type.member_values.len());
                for member in self.mem.getn(enum_type.member_values) {
                    let int_value_expr = self.synth_int(member.int_value, fn_span);
                    let member_name_expr = self.synth_string_literal(member.name, fn_span);
                    let cond =
                        self.synth_equals_call_simple(enum_arg_int_expr, int_value_expr, fn_span);
                    let instrs = self.mem.pushn(&[MatchingConditionInstr::cond(cond)]);
                    arms.push(TypedMatchArm {
                        condition: MatchingCondition { instrs },
                        consequent_expr: member_name_expr,
                    });
                }
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    initial_let_statements: MSlice::empty(),
                    arms: arms.to_slice(),
                });
                Ok(self.exprs.add(match_expr, self.builtin_types.string(), fn_span))
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
                    let variant_tag_expr = self.synth_int(variant.tag_value, fn_span);
                    let a_tag_is_this_variant =
                        self.synth_equals_call_simple(param_a_tag_expr, variant_tag_expr, fn_span);
                    let b_tag_is_this_variant =
                        self.synth_equals_call_simple(param_b_tag_expr, variant_tag_expr, fn_span);
                    let mut conditions = self.mem.new_list(2 + variant.payload.is_some() as u32);
                    conditions.push(MatchingConditionInstr::cond(a_tag_is_this_variant));
                    conditions.push(MatchingConditionInstr::cond(b_tag_is_this_variant));
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
                        condition: MatchingCondition { instrs: conditions },
                        consequent_expr: self.synth_bool(true, fn_span),
                    });
                }
                arms.push(TypedMatchArm {
                    condition: MatchingCondition { instrs: MSlice::empty() },
                    consequent_expr: self.synth_bool(false, fn_span),
                });
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    initial_let_statements: MSlice::empty(),
                    arms: arms.to_slice(),
                });
                let match_expr_id = self.exprs.add(match_expr, BOOL_TYPE_ID, fn_span);
                // eprintln!("SUM EQUALS MATCH\n{}", self.expr_to_string(match_expr_id));
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
                    let int_value_expr = self.synth_int(variant.tag_value, fn_span);
                    let member_name_expr = self.synth_string_literal(variant.name, fn_span);
                    let cond =
                        self.synth_equals_call_simple(sum_arg_int_expr, int_value_expr, fn_span);
                    let instrs = self.mem.pushn(&[MatchingConditionInstr::cond(cond)]);
                    arms.push(TypedMatchArm {
                        condition: MatchingCondition { instrs },
                        consequent_expr: member_name_expr,
                    });
                }
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    initial_let_statements: MSlice::empty(),
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
                    condition: MatchingCondition { instrs: conditions.to_slice() },
                    consequent_expr: self.synth_bool(true, fn_span),
                };
                let false_arm = TypedMatchArm {
                    condition: MatchingCondition { instrs: MSlice::empty() },
                    consequent_expr: self.synth_bool(false, fn_span),
                };
                let arms = self.mem.pushn(&[equals_arm, false_arm]);
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    initial_let_statements: MSlice::empty(),
                    arms,
                });
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
                    let variant_tag_expr = self.synth_int(variant.tag_value, fn_span);
                    let cond = self.synth_equals_call_simple(tag_expr, variant_tag_expr, fn_span);
                    let instrs = self.mem.pushn(&[MatchingConditionInstr::cond(cond)]);

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
                        condition: MatchingCondition { instrs },
                        consequent_expr: self.exprs.add_block(block, EMPTY_TYPE_ID),
                    });
                }
                let match_expr = TypedExpr::Match(TypedMatchExpr {
                    initial_let_statements: MSlice::empty(),
                    arms: arms.to_slice(),
                });
                Ok(self.exprs.add(match_expr, EMPTY_TYPE_ID, fn_span))
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
        if v.usage_count == 0 {
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
            .scopes
            .find_namespace_local(scope_id, parsed_ability.name)
        {
            Some(existing_ns_id) => {
                let existing = self.namespaces.get(existing_ns_id);
                let adoptable = existing.namespace_type == NamespaceKind::User
                    && existing.owner_module == Some(self.module_in_progress.unwrap());
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
                    kbail!(
                        self,
                        parsed_ability.span,
                        "Namespace with name {} already exists",
                        parsed_ability.name
                    );
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

            let predicate_functions_handle = predicate_functions.to_slice();
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
            .zip(ability.kind.arguments(&self.mem))
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
                self.functions.get_mut(impl_fn).body_failure = Some(e);
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
                        self.functions.get_mut(function_declaration_id).body_failure = Some(e);
                        self.report(e);
                    };
                }
            }
            ParsedId::Macro(parsed_macro_id) => {
                if let Some(function_declaration_id) =
                    self.macro_ast_mappings.get(&parsed_macro_id).copied()
                {
                    if let Err(e) = self.eval_function_body(function_declaration_id) {
                        self.functions.get_mut(function_declaration_id).body_failure = Some(e);
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
        // let status_entry = self.use_statuses.get(&parsed_use_id);
        // let is_fulfilled = match status_entry {
        //     Some(use_status) if use_status.is_resolved() => true,
        //     _ => false,
        // };
        // if is_fulfilled {
        //     return true;
        // }
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
        let resolution = if useable_symbols.is_empty() {
            debug!("Inserting unresolved use of {}", self.qident_to_string(&parsed_use.target));
            false
        } else {
            for symbol in &useable_symbols {
                self.scopes.add_use_binding(
                    scope_id,
                    symbol,
                    parsed_use.alias.unwrap_or(parsed_use.target.name),
                );
                debug!("Inserting resolved use of {:?}", symbol);
            }
            true
        };
        // self.use_statuses.insert(parsed_use_id, resolution);
        resolution
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
        if let Some(function_id) = self.scopes.find_function_local(scope_id_to_search, name.name) {
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Function(function_id),
            });
        }
        if let Some(type_id) = self.scopes.find_type_local(scope_id_to_search, name.name) {
            let companion_namespace = self.get_companion_namespace(type_id);
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type { type_id, companion_namespace },
            })
        } else
        // This 'else' is load-bearing since we don't actually remove the pending definitions
        // from the scopes
        if let Some(pending_type) =
            self.scopes.find_pending_type_local(scope_id_to_search, name.name)
        {
            let type_id = self.eval_type_defn(pending_type.parsed_id, pending_type.scope_id)?;
            let companion_namespace = self.get_companion_namespace(type_id);
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Type { type_id, companion_namespace },
            })
        }
        if let Some(variable_id) = self
            .scopes
            .find_variable_local(scope_id_to_search, name.name)
            .and_then(|vis| vis.variable_id())
        {
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Global(variable_id),
            })
        }
        if let Some(ability_id) = self.scopes.find_ability_local(scope_id_to_search, name.name) {
            let namespace_id = self.abilities.get(ability_id).namespace_id;
            found_symbols.push(UseableSymbol {
                source_scope: scope_id_to_search,
                id: UseableSymbolId::Ability(ability_id, namespace_id),
            })
        }
        if let Some(ns_id) = self.scopes.find_namespace_local(scope_id_to_search, name.name) {
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
                if !self
                    .execute_static_condition(parsed_type_defn.compile_condition, namespace_scope_id)
                {
                    return;
                }
                let pending_defn = TypePendingDefinition {
                    namespace_id,
                    scope_id: namespace_scope_id,
                    parsed_id: type_defn_id,
                };
                let added = self.scopes.add_pending_type(
                    namespace_scope_id,
                    parsed_type_defn.name,
                    pending_defn,
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
        let ns_parsed_id = ns.parsed_id;
        let span = self.ast.get_span_for_id(ns_parsed_id);

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
            abi_mode: AbiMode::Internal,
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
            child_specializations: MList::empty(),
            specialization_info: None,
            parsed_id: ns_parsed_id,
            type_id: function_type,
            compiler_debug: false,
            kind: TypedFunctionKind::Standard,
            is_concrete: false,
            is_recursive: false,
            is_macro: false,
            // these load functions are host code: they patch the reloadable lib and do not live in it
            is_reloadable: false,
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
            if function.is_reloadable && function.namespace_id == ns_id {
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
        let is_sys = parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.sys;
        if is_sys {
            self.scopes.sys_scope_id = ns_scope_id
        }
        let is_libc =
            parent_scope_id == self.scopes.core_scope_id && name == self.ast.idents.b.libc;
        if is_libc {
            self.scopes.libc_scope_id = ns_scope_id
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
            kbail!(
                self,
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
    ) -> K1Result<NamespaceId> {
        let ast_namespace = self.ast.namespaces.get(parsed_namespace_id);
        let name_span = ast_namespace.name_span;
        if let Some(parent_ns) = self.scopes.get_scope_owner(parent_scope).as_namespace() {
            self.fail_if_reload_ns(parent_ns, ast_namespace.span, "nested namespaces")?;
        }

        let namespace_id = if let Some(existing) =
            self.scopes.find_namespace_local(parent_scope, ast_namespace.name)
        {
            // Namespace extension
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
                            ParsedId::Global(_)
                            | ParsedId::TypeDefn(_)
                            | ParsedId::Ability(_) => {
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
        manifest_fn_defn: Option<ParsedId>,
    ) -> anyhow::Result<()> {
        self.module_in_progress = Some(module_id);
        let is_core = module_id == MODULE_ID_CORE;

        // Namespace phase
        debug!(">> Phase 0 declare module root namespace");
        let module_root_namespace_declare_result =
            self.declare_namespace(module_root_parsed_namespace, Scopes::ROOT_SCOPE_ID);

        if let Err(e) = module_root_namespace_declare_result {
            self.report(e);
            bail!(
                "{} failed namespace declaration phase with {} errors",
                self.program_name(),
                self.messages.borrow().len()
            )
        }
        let typed_namespace_id = module_root_namespace_declare_result.unwrap();
        let module_root_namespace_scope_id = self.namespaces.get(typed_namespace_id).scope_id;

        let module = self.modules.get_mut(module_id);
        module.namespace_id = typed_namespace_id;
        module.namespace_scope_id = module_root_namespace_scope_id;

        if !is_core {
            // takes 14us last I checked
            self.add_core_uses_to_scope(module_root_namespace_scope_id, SpanId::NONE)
                .map_err(|e| self.message_to_anyhow(e))?;
        }

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

        let skip_defns = match (pre_ns_id, manifest_fn_defn) {
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

    fn resolve_pending_uses(&mut self) {
        let mut i = 0;
        // eprintln!("resolve_pending_uses called with {}", self.uses_pending_resolution.len());
        while let Some(use_pending) = self.uses_pending_resolution.get(i) {
            // Attempting uses here will really only resolve things from pre/
            // or from other modules; stuff from this module will need to be
            // resolved by later passes
            //
            // But this lets us use things like std/meta for metaprograms, which is
            // important
            if self.eval_use_definition(use_pending.scope_id, use_pending.use_id, false) {
                self.uses_pending_resolution.remove(i);
            } else {
                i += 1;
            }
        }
        // eprintln!("resolve_pending_uses finished with {}", self.uses_pending_resolution.len());
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
                let result = self.specialize_function_body(*function_id);
                if let Err(e) = result {
                    self.functions.get_mut(*function_id).body_failure = Some(e);
                    self.report(e)
                }
            }
            function_ids.clear()
        }
        Ok(())
    }

    fn check_type_finite_rec(
        &mut self,
        type_id: TypeId,
        behind_indirection: bool,
        stack: &mut List<TypeId, MemTmp>,
    ) -> Option<TypeId> {
        // Any repeat on the current path is a cycle; only indirection-free ones are
        // infinite. Guarding on every visited id (not just the cluster's recursive
        // mentions) is what terminates the walk on self-referential instances minted
        // by instantiate_generic_type_miss, which are in no mentions list
        if stack.contains(&type_id) {
            return if behind_indirection { None } else { Some(type_id) };
        }
        stack.push_grow(&mut self.tmp, type_id);

        let result = 'walk: {
            match self.types.get(type_id) {
                Type::Struct(struct_type) => {
                    let fields = struct_type.fields;
                    for i in 0..fields.len() as usize {
                        let field_type_id = self.mem.get_nth(fields, i).type_id;
                        if let Some(t) =
                            self.check_type_finite_rec(field_type_id, behind_indirection, stack)
                        {
                            break 'walk Some(t);
                        }
                    }

                    None
                }

                Type::Sum(sum_type) => {
                    let variants = sum_type.variants;
                    for i in 0..variants.len() as usize {
                        if let Some(payload) = self.mem.get_nth(variants, i).payload {
                            if let Some(t) =
                                self.check_type_finite_rec(payload, behind_indirection, stack)
                            {
                                break 'walk Some(t);
                            }
                        }
                    }

                    None
                }

                Type::Reference(reference_type) => {
                    let inner_type = reference_type.inner_type;
                    self.check_type_finite_rec(inner_type, true, stack)
                }

                Type::Generic(generic) => {
                    let inner = generic.inner;
                    self.check_type_finite_rec(inner, behind_indirection, stack)
                }

                Type::Array(array_type) => {
                    let size_type = array_type.size_type;
                    let element_type = array_type.element_type;
                    if self.get_concrete_count_of_array(size_type) != Some(0) {
                        self.check_type_finite_rec(element_type, behind_indirection, stack)
                    } else {
                        None
                    }
                }

                _ => None,
            }
        };

        stack.pop();
        result
    }

    fn generate_constructors_for_type(
        &mut self,
        type_id: TypeId,
        dst: &mut Vec<PatternCtorTrialEntry>,
        field_ctors_buf: &mut Vec<Vec<(StringId, PatternCtorId)>>,
        ctors_base: usize,
        ancestors: &mut Vec<TypeId>,
        span_id: SpanId,
    ) {
        #[inline]
        fn alive(ctor: PatternCtorId) -> PatternCtorTrialEntry {
            PatternCtorTrialEntry { ctor, alive: true }
        }
        fn handle_sum_variant(
            k1: &mut TypedProgram,
            dst: &mut Vec<PatternCtorTrialEntry>,
            field_ctors_buf: &mut Vec<Vec<(StringId, PatternCtorId)>>,
            ctors_base: usize,
            span_id: SpanId,
            ancestors: &mut Vec<TypeId>,
            v: &TypedSumVariant,
        ) {
            match v.payload.as_ref() {
                None => dst.push(alive(
                    k1.pattern_ctors.add(PatternCtor::Sum { variant_name: v.name, inner: None }),
                )),
                Some(payload) => {
                    let prev_len = dst.len();
                    k1.generate_constructors_for_type(
                        *payload,
                        dst,
                        field_ctors_buf,
                        ctors_base,
                        ancestors,
                        span_id,
                    );
                    for payload_pattern_id in dst[prev_len..].iter_mut() {
                        payload_pattern_id.ctor = k1.pattern_ctors.add(PatternCtor::Sum {
                            variant_name: v.name,
                            inner: Some(payload_pattern_id.ctor),
                        })
                    }
                }
            }
        }

        if ancestors.contains(&type_id) {
            // pattern matching: Here is where we would actually place a sentinel
            //        ctor that we could expand as needed by the pattern
            debug!("Stopping pattern ctor recursion at {}", self.type_id_to_string(type_id));

            // Let's just say its a type variable, idk, tired of adding
            // constructors that dont do anything
            dst.push(alive(PatternCtorId::TYPE_VARIABLE));
            return;
        }

        if type_id == self.builtin_types.string() {
            dst.push(alive(PatternCtorId::STRING));
            return;
        }
        ancestors.push(type_id);
        match self.types.get(type_id) {
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
            Type::Opaque(_) => {
                dst.push(alive(PatternCtorId::OPAQUE)) // Opaque is an opaque atom pattern
            }
            Type::Reference(refer) => {
                // Follow the pointer
                let prev_len = dst.len();
                self.generate_constructors_for_type(
                    refer.inner_type,
                    dst,
                    field_ctors_buf,
                    ctors_base,
                    ancestors,
                    span_id,
                );
                for pointee_pattern_id in dst[prev_len..].iter_mut() {
                    pointee_pattern_id.ctor =
                        self.pattern_ctors.add(PatternCtor::Reference(pointee_pattern_id.ctor));
                }
            }
            Type::Array(_array_type) => dst.push(alive(self.pattern_ctors.add(PatternCtor::Array))),
            Type::Sum(sum_type) => {
                for v in self.mem.getn(sum_type.variants) {
                    handle_sum_variant(
                        self,
                        dst,
                        field_ctors_buf,
                        ctors_base,
                        span_id,
                        ancestors,
                        v,
                    )
                }
            }
            Type::Enum(enum_type) => {
                for v in self.mem.getn(enum_type.member_values) {
                    dst.push(alive(
                        self.pattern_ctors.add(PatternCtor::Enum { variant_name: v.name }),
                    ))
                }
            }
            Type::Struct(struc) => {
                debug_assert!(type_id != self.builtin_types.string());

                // This hides a bug with recursive types, but oh well
                match self.get_as_container_instance(type_id) {
                    Some((_, ContainerKind::Buffer)) => {
                        dst.push(alive(PatternCtorId::BUFFER));
                    }
                    Some((_, ContainerKind::Span)) => {
                        dst.push(alive(PatternCtorId::SPAN));
                    }
                    _ => {
                        let field_count = struc.fields.len();
                        if field_count == 0 {
                            dst.push(alive(
                                self.pattern_ctors
                                    .add(PatternCtor::Struct { fields: MSlice::empty() }),
                            ));
                        } else {
                            let mut has_unreachable_field = false;
                            for (index, field) in self.mem.getn(struc.fields).iter().enumerate() {
                                let prev_len = dst.len();
                                self.generate_constructors_for_type(
                                    field.type_id,
                                    dst,
                                    field_ctors_buf,
                                    ctors_base + field_count as usize,
                                    ancestors,
                                    span_id,
                                );
                                let slot = ctors_base + index;
                                while field_ctors_buf.len() <= slot {
                                    field_ctors_buf.push(Vec::new());
                                }
                                field_ctors_buf[slot].clear();
                                if dst.len() == prev_len {
                                    // No constructors
                                    has_unreachable_field = true
                                }
                                for field_ctor in dst[prev_len..].iter() {
                                    field_ctors_buf[slot].push((field.name, field_ctor.ctor));
                                }
                                debug!(
                                    "Pushed {} constructors for field {}; resetting dst to {prev_len}",
                                    field_ctors_buf[slot].len(),
                                    self.ident_str(field.name)
                                );
                                dst.truncate(prev_len);
                            }

                            if !has_unreachable_field {
                                let final_count = field_ctors_buf
                                    [ctors_base..ctors_base + field_count as usize]
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
                                    let fields = self.patterns.mem.pushn_uninit(field_count);
                                    let primed_struct_ctor_id =
                                        self.pattern_ctors.add(PatternCtor::Struct { fields });
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
                                for (field_index, ctors) in field_ctors_buf
                                    [ctors_base..ctors_base + field_count as usize]
                                    .iter()
                                    .enumerate()
                                {
                                    if ctors.len() == 1 {
                                        for result_struct in result_struct_ids.iter_mut() {
                                            let fields = self
                                                .pattern_ctors
                                                .get(result_struct.ctor)
                                                .struct_fields();
                                            self.patterns.mem.getn_mut(fields)[field_index] =
                                                ctors[0];
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
                                        for (row, result_struct) in
                                            result_struct_ids.iter_mut().enumerate()
                                        {
                                            let pattern_index = (row / repeat_count) % ctors.len();
                                            let pattern = ctors[pattern_index];
                                            let fields = self
                                                .pattern_ctors
                                                .get(result_struct.ctor)
                                                .struct_fields();
                                            self.patterns.mem.getn_mut(fields)[field_index] =
                                                pattern;
                                        }
                                        field_index_w_multi_ctor += 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Type::Function(_f) => {
                debug!("function is probably unmatchable");
            }
            Type::LambdaObject(_) => {
                dst.push(alive(self.pattern_ctors.add(PatternCtor::LambdaObject)))
            }
            Type::AbilityObject(_) => {
                dst.push(alive(PatternCtorId::OPAQUE)) // An opaque atom: only bindings match
            }
            Type::StaticValue(_) => dst.push(alive(self.pattern_ctors.add(PatternCtor::ValueType))),
            Type::Never => {}
            _ => self.report_hint(
                span_id,
                format!(
                    "INTERNAL COMPILER ERROR: unhandled type in generate_constructors_for_type {}",
                    self.type_id_to_string(type_id)
                ),
            ),
        };
        let popped = ancestors.pop();
        debug_assert_eq!(popped, Some(type_id));
    }

    fn pattern_eliminates_ctor(&self, pattern: TypedPatternId, ctor: PatternCtorId) -> bool {
        match (self.patterns.get(pattern), self.pattern_ctors.get(ctor)) {
            (TypedPattern::Wildcard(_), _) => true,
            (TypedPattern::Variable(_), _) => true,
            #[allow(clippy::bool_comparison)]
            (TypedPattern::LiteralBool(b, _), PatternCtor::BoolTrue) => *b == true,
            #[allow(clippy::bool_comparison)]
            (TypedPattern::LiteralBool(b, _), PatternCtor::BoolFalse) => *b == false,

            // 'Innumerable' types and their patterns
            (TypedPattern::LiteralChar(_char_pattern, _), PatternCtor::Char) => false,
            (TypedPattern::LiteralInteger(_int_pattern, _), PatternCtor::Int) => false,
            (TypedPattern::LiteralFloat(_float_pattern, _), PatternCtor::Float) => false,
            (TypedPattern::LiteralString(_string_pattern, _), PatternCtor::String) => false,
            (TypedPattern::RefNull(_type_id, _), PatternCtor::Reference(_)) => false,
            (TypedPattern::PointerNull(_), PatternCtor::Pointer) => false,

            (TypedPattern::Sum(sum_pat), PatternCtor::Sum { variant_name, inner }) => {
                if *variant_name == sum_pat.variant_name {
                    match (sum_pat.payload, inner) {
                        (Some(payload), Some(inner)) => {
                            self.pattern_eliminates_ctor(payload, *inner)
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
                for field_pattern in self.patterns.get_slice(struc.fields).iter() {
                    let matching_field_pattern = self
                        .patterns
                        .mem
                        .getn_lt(*fields)
                        .iter()
                        .find(|(name, _ctor_pattern)| *name == field_pattern.name)
                        .map(|(_, ctor_pattern)| ctor_pattern)
                        .unwrap_or_else(|| {
                            ice_span!(
                                self,
                                struc.span,
                                "Field {} not in struct ctor {}; pattern should have failed typecheck by now",
                                self.ident_str(field_pattern.name),
                                self.patterns.mem.getn(*fields).iter().map(|f| self.ident_str(f.0)).join(", ")
                            )
                        });
                    if !self.pattern_eliminates_ctor(field_pattern.pattern, *matching_field_pattern)
                    {
                        matches = false;
                        break;
                    }
                }
                matches
            }
            (TypedPattern::Reference(ref_pattern), PatternCtor::Reference(ref_ctor)) => {
                self.pattern_eliminates_ctor(ref_pattern.inner_pattern, *ref_ctor)
            }
            (TypedPattern::Enum(enum_pattern), PatternCtor::Enum { variant_name }) => {
                enum_pattern.member_name == *variant_name
            }
            (TypedPattern::Type(_type_pattern), _ctor) => false,
            (a, b) => {
                eprintln!(
                    "Unhandled pattern_matches case: pattern {} and ctor {:?}",
                    a.kind_name(),
                    b.kind_name()
                );
                false
            }
        }
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
        let struct_pt = self.get_physical_type(struct_type_id).unwrap();
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
            core!("sys"),
            core!("files"),
            core!("scalar-cmp"),
            core!("string-builder"),
            core!("code"),
            core!("code-builder"),
            core!("optref"),
            QIdent { path: core_mem, name: get_ident!(self, "zeroed"), name_span: span },
            QIdent { path: core_types, name: self.ast.idents.b.enum_, name_span: span },
            QIdent { path: core_types, name: get_ident!(self, "sum"), name_span: span },
            QIdent { path: core_types, name: get_ident!(self, "type-id"), name_span: span },
        ];
        for qid in idents_to_use.into_iter() {
            let use_id = self.ast.uses.add_use(parse::ParsedUse { target: qid, alias: None, span });
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

    fn get_type_schema(&mut self, type_id: TypeId) -> StaticValueId {
        let reserved_id = if let Some(static_value_id) = self.type_schemas.get(&type_id) {
            return *static_value_id;
        } else {
            let reserved_value_id = self.static_values.pool.reserve_id();
            self.type_schemas.insert(type_id, reserved_value_id);
            reserved_value_id
        };

        let type_schema_type_id = self.builtin_types.types_type_schema.unwrap();
        let type_schema = *self.types.get(type_schema_type_id).expect_sum();
        let int_kind_type_id = self.builtin_types.types_int_kind.unwrap();
        let float_kind_type_id = self.builtin_types.types_float_kind.unwrap();
        let get_schema_variant = |self_: &TypedProgram, ident| {
            self_.sum_variant_by_name(type_schema.variants, ident).unwrap()
        };
        let make_variant =
            |self_: &TypedProgram, name: StringId, payload: Option<StaticValueId>| {
                let v = get_schema_variant(self_, name);
                StaticSum { sum_type_id: type_schema_type_id, variant_index: v.index, payload }
            };

        // For now, introspection does not support 'static' types, it just sees through them

        // Temporarily, we could provide a separate boolean-returning function to get a type's
        // static value or something
        let chased_type_id = self.get_static_family_id_if_static(type_id);

        let typ = self.types.get(chased_type_id);
        let schema_static_sum = match typ {
            Type::Char => make_variant(self, self.ast.idents.b.char, None),
            Type::Bool => make_variant(self, self.ast.idents.b.bool, None),
            Type::Pointer => make_variant(self, self.ast.idents.b.ptr, None),
            Type::Integer(integer_type) => {
                let int_kind_enum_value =
                    TypedProgram::make_int_kind(int_kind_type_id, *integer_type);

                let payload_value_id = self.static_values.add(int_kind_enum_value);
                let enum_value = make_variant(self, self.ast.idents.b.int, Some(payload_value_id));
                enum_value
            }
            Type::Float(float_type) => {
                let float_kind_enum_value =
                    TypedProgram::make_float_kind(float_kind_type_id, *float_type);
                let payload_value_id = self.static_values.add(float_kind_enum_value);
                make_variant(self, self.ast.idents.b.float, Some(payload_value_id))
            }
            Type::Enum(enum_type) => {
                let target_enum_members = enum_type.member_values;
                let enum_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.enum_).payload.unwrap();
                let values_span_type_id =
                    self.get_struct_field(enum_schema_payload_type_id, 1).type_id;
                let value_struct_type_id = self.get_as_span_instance(values_span_type_id).unwrap();
                let int_type = enum_type.int_type;
                let int_type_value_id =
                    self.static_values.add(TypedProgram::make_int_kind(int_kind_type_id, int_type));
                let mut member_values = self.static_values.mem.new_list(target_enum_members.len());
                // values: span[{
                //   name: string,
                //   value: int-value,
                // }]
                for member_value in self.mem.getn(target_enum_members) {
                    let name_value_id = self.static_values.add_string(member_value.name);

                    let int_value_type_id = self.builtin_types.types_int_value.unwrap();
                    let int_value_sum = self.types.get(int_value_type_id).expect_sum();
                    let int_value_sum_value = TypedProgram::make_int_value(
                        &mut self.static_values,
                        int_value_type_id,
                        self.mem.getn(int_value_sum.variants),
                        member_value.int_value,
                    );
                    let int_value_sum_value_id =
                        self.static_values.add(StaticValue::Sum(int_value_sum_value));

                    member_values.push(self.static_values.add_struct_from_slice(
                        value_struct_type_id,
                        &[
                            // name: string,
                            name_value_id,
                            // value: int-value,
                            int_value_sum_value_id,
                        ],
                    ))
                }
                let variant_values_slice = member_values.to_slice();
                let variants_span_value_id =
                    self.static_values.add_span(values_span_type_id, variant_values_slice);
                let payload_value_id = self.static_values.add_struct_from_slice(
                    enum_schema_payload_type_id,
                    &[int_type_value_id, variants_span_value_id],
                );
                make_variant(self, self.ast.idents.b.enum_, Some(payload_value_id))
            }
            Type::Struct(_struct_type) if chased_type_id == self.builtin_types.string() => {
                make_variant(self, self.ast.idents.b.string, None)
            }
            Type::Struct(struct_type) => {
                let record_kind = struct_type.record_kind;
                let struct_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.struct_).payload.unwrap();
                // { fields: span[{}] }
                let struct_schema_payload_struct =
                    self.types.get(struct_schema_payload_type_id).expect_struct();
                // { fields: span[{ ... }] }
                let struct_type_fields = struct_type.fields;
                let struct_schema_fields_span_type_id =
                    self.mem.get_nth(struct_schema_payload_struct.fields, 0).type_id;
                let struct_schema_field_item_struct_type_id =
                    self.get_as_span_instance(struct_schema_fields_span_type_id).unwrap();

                // for offsets
                let struct_layout = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => {
                        Some(self.get_struct_layout(type_id))
                    }
                    RecordKind::Union => None,
                };
                // { name: string), typeId: u64, offset: size }
                let mut field_values: List<StaticValueId, StaticValuePool> =
                    self.static_values.mem.new_list(struct_type_fields.len());
                for (index, f) in self.mem.getn(struct_type_fields).iter().enumerate() {
                    let name_string_value_id = self.static_values.add_string(f.name);

                    // We need to ensure that any and all typeIds that we share with the user
                    // are available at runtime, by calling these functions at least once.
                    self.register_type_metainfo(f.type_id);

                    let type_id_value_id = self.add_type_id_value(f.type_id);
                    let offset_u32 = match &struct_layout {
                        None => 0,
                        Some(struct_layout) => struct_layout[index].offset,
                    };
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
                let values_slice = field_values.to_slice();
                let span_value_id =
                    self.static_values.add_span(struct_schema_fields_span_type_id, values_slice);
                let payload = self
                    .static_values
                    .add_struct_from_slice(struct_schema_payload_type_id, &[span_value_id]);
                let variant_name = match record_kind {
                    RecordKind::Struct | RecordKind::Packed => self.ast.idents.b.struct_,
                    RecordKind::Union => self.ast.idents.b.union,
                };
                make_variant(self, variant_name, Some(payload))
            }
            Type::Reference(reference_type) => {
                let reference_type = *reference_type;
                let reference_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.reference).payload.unwrap();
                // { innerTypeId: type-id, mutable: bool }
                let inner_type_id_value_id = self.add_type_id_value(reference_type.inner_type);

                // We need to ensure that any and all typeIds that we share with the user
                // are available at runtime, by calling these functions at least once.
                self.register_type_metainfo(reference_type.inner_type);

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    reference_schema_payload_type_id,
                    &[inner_type_id_value_id],
                );
                make_variant(self, self.ast.idents.b.reference, Some(payload_struct_id))
            }
            Type::Array(array_type) => {
                let array_type = *array_type;
                let concrete_count = self.get_concrete_count_of_array(array_type.size_type);
                let array_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.array).payload.unwrap();
                // { elementTypeId: u64, size: size }
                let element_type_id_value_id = self.add_type_id_value(array_type.element_type);
                self.register_type_metainfo(array_type.element_type);

                let maybe_concrete_size_value_id = match concrete_count {
                    None => None,
                    Some(size) => Some(self.static_values.add_size(size)),
                };
                let option_size = self.synth_optional_type(SIZE_TYPE_ID);
                let size_value_id = synth::synth_static_option(
                    &mut self.static_values,
                    option_size,
                    maybe_concrete_size_value_id,
                );

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    array_schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(self, self.ast.idents.b.array, Some(payload_struct_id))
            }
            Type::Vector(vector_type) => {
                let vector_type = *vector_type;
                let concrete_count = self.get_concrete_count_of_array(vector_type.size_type);
                let vector_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.vector).payload.unwrap();
                let element_type_id_value_id = self.add_type_id_value(vector_type.element_type);
                self.register_type_metainfo(vector_type.element_type);

                let maybe_concrete_size_value_id = match concrete_count {
                    None => None,
                    Some(size) => Some(self.static_values.add_size(size)),
                };
                let option_size = self.synth_optional_type(SIZE_TYPE_ID);
                let size_value_id = synth::synth_static_option(
                    &mut self.static_values,
                    option_size,
                    maybe_concrete_size_value_id,
                );

                let payload_struct_id = self.static_values.add_struct_from_slice(
                    vector_schema_payload_type_id,
                    &[element_type_id_value_id, size_value_id],
                );
                make_variant(self, self.ast.idents.b.vector, Some(payload_struct_id))
            }
            Type::Sum(typed_sum) => {
                let target_sum_variants = typed_sum.variants;
                let either_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.either).payload.unwrap();
                let variants_span_type_id =
                    self.get_struct_field(either_payload_type_id, 2).type_id;
                let variant_struct_type_id =
                    self.get_as_span_instance(variants_span_type_id).unwrap();
                let tag_type = typed_sum.tag_type;
                let tag_type_value_id =
                    self.static_values.add(TypedProgram::make_int_kind(int_kind_type_id, tag_type));
                let sum_agg_id = self.get_physical_type(type_id).unwrap().expect_agg();
                let sum_pt = self.agg_types.get(sum_agg_id).agg_type.expect_sum();
                let payload_offset = sum_pt.payload_offset;
                let payload_offset_value_id =
                    self.static_values.add_size(to_k1_size_usize(payload_offset as usize));
                let mut variant_values = self.static_values.mem.new_list(target_sum_variants.len());
                for variant in self.mem.getn(target_sum_variants) {
                    let name_value_id = self.static_values.add_string(variant.name);

                    let int_value_type_id = self.builtin_types.types_int_value.unwrap();
                    let int_value_enum = self.types.get(int_value_type_id).expect_sum();
                    let tag_value_sum_value = TypedProgram::make_int_value(
                        &mut self.static_values,
                        int_value_type_id,
                        self.mem.getn(int_value_enum.variants),
                        variant.tag_value,
                    );
                    let tag_value_id =
                        self.static_values.add(StaticValue::Sum(tag_value_sum_value));

                    let payload_info_opt_type_id =
                        self.get_struct_field(variant_struct_type_id, 2).type_id;
                    let payload_info_struct_id =
                        self.get_as_opt_instance(payload_info_opt_type_id).unwrap();

                    let payload_info_value_id = match variant.payload {
                        None => synth_static_option(
                            &mut self.static_values,
                            payload_info_opt_type_id,
                            None,
                        ),
                        Some(payload_type_id) => {
                            let type_id_value_id = self.add_type_id_value(payload_type_id);
                            // We need to ensure that any and all typeIds that we share with the user
                            // are available at runtime, by calling these functions at least once.
                            self.register_type_metainfo(payload_type_id);

                            let payload_info_struct_id = self
                                .static_values
                                .add_struct_from_slice(payload_info_struct_id, &[type_id_value_id]);
                            synth_static_option(
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
                            // payload: { typeId: u64 },
                            payload_info_value_id,
                        ],
                    ))
                }
                let variant_values_slice = variant_values.to_slice();
                let variants_span_value_id =
                    self.static_values.add_span(variants_span_type_id, variant_values_slice);
                let payload_value_id = self.static_values.add_struct_from_slice(
                    either_payload_type_id,
                    &[tag_type_value_id, payload_offset_value_id, variants_span_value_id],
                );
                make_variant(self, self.ast.idents.b.either, Some(payload_value_id))
            }
            Type::Opaque(opaque) => {
                // FIXME: Proper opaque type schema
                let s = self.static_values.add(StaticValue::String(
                    self.ast
                        .idents
                        .intern(format!("opaque[size={}, align={}]", opaque.size, opaque.align)),
                ));
                make_variant(self, self.ast.idents.b.other, Some(s))
            }
            Type::Never => make_variant(self, self.ast.idents.b.never, None),
            Type::Function(fn_type) => {
                let fn_type = *fn_type;
                let function_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.function).payload.unwrap();
                //Function({
                //  params: span[{ name: string, typeId: type-id }],
                //  returnTypeId: type-id,
                //}),
                let function_schema_payload_struct =
                    self.types.get(function_schema_payload_type_id).expect_struct();
                let function_params_span_field =
                    self.mem.get_nth(function_schema_payload_struct.fields, 0);
                let function_params_span_type_id = function_params_span_field.type_id;
                let function_param_struct_type_id =
                    self.get_as_span_instance(function_params_span_type_id).unwrap();

                let mut params_value_ids =
                    self.static_values.mem.new_list(fn_type.logical_params().len());
                // Skipping lambda environment parameters;
                // knowing what is a lambda is covered by the type
                // kind the function appears within

                for param in self.mem.getn(fn_type.logical_params()) {
                    self.register_type_metainfo(param.type_id);

                    let param_name_value_id = self.static_values.add_string(param.name);
                    let param_type_id_value_id = self.add_type_id_value(param.type_id);
                    let param_struct_value_id = self.static_values.add_struct_from_slice(
                        function_param_struct_type_id,
                        &[
                            // name: string
                            param_name_value_id,
                            // type-id: type-id
                            param_type_id_value_id,
                        ],
                    );
                    params_value_ids.push(param_struct_value_id)
                }

                let params_value_ids_slice = params_value_ids.to_slice();
                let params_span_value_id = self
                    .static_values
                    .add_span(function_params_span_type_id, params_value_ids_slice);

                self.register_type_metainfo(fn_type.return_type);
                let return_type_id_value_id = self.add_type_id_value(fn_type.return_type);

                let payload = self.static_values.add_struct_from_slice(
                    function_schema_payload_type_id,
                    &[
                        // params
                        params_span_value_id,
                        // returnTypeId
                        return_type_id_value_id,
                    ],
                );
                make_variant(self, self.ast.idents.b.function, Some(payload))
            }
            Type::FunctionPointer(fp) => {
                let fp = *fp;
                let function_pointer_schema_payload_type_id =
                    get_schema_variant(self, self.ast.idents.b.function_pointer).payload.unwrap();

                let function_type_id_value_id = self.add_type_id_value(fp.function_type_id);
                self.register_type_metainfo(fp.function_type_id);

                let payload = self.static_values.add_struct_from_slice(
                    function_pointer_schema_payload_type_id,
                    &[function_type_id_value_id],
                );
                make_variant(self, self.ast.idents.b.function_pointer, Some(payload))
            }
            Type::Lambda(_)
            | Type::LambdaObject(_)
            | Type::AbilityObject(_)
            | Type::TypeParameter(_)
            | Type::Generic(_)
            | Type::FunctionTypeParameter(_)
            | Type::InferenceHole(_)
            | Type::StaticValue(_) => {
                let s = self
                    .static_values
                    .add(StaticValue::String(self.ast.idents.intern(typ.kind_name())));
                make_variant(self, self.ast.idents.b.other, Some(s))
            }
        };

        self.static_values.set(reserved_id, StaticValue::Sum(schema_static_sum));
        reserved_id
    }

    fn get_type_name(&mut self, type_id: TypeId) -> StaticValueId {
        if let Some(existing) = self.type_names.get(&type_id) {
            return *existing;
        }

        let mut s = std::mem::take(&mut self.buffers.name_builder);
        self.display_type_id(&mut s, type_id, dump::TypeDisplayMode::Name).unwrap();
        let string_id = self.ast.idents.intern(&s);
        s.clear();
        self.buffers.name_builder = s;
        let value_id = self.static_values.add_string(string_id);

        self.type_names.insert(type_id, value_id);
        value_id
    }

    pub(crate) fn register_type_metainfo(&mut self, type_id: TypeId) {
        let _ = self.get_type_schema(type_id);
        let _ = self.get_type_name(type_id);
    }

    pub(crate) fn add_type_id_value(&mut self, type_id: TypeId) -> StaticValueId {
        let type_id_type_id = self.builtin_types.type_id();
        self.static_values.add_type_id_value(type_id_type_id, type_id)
    }

    fn make_int_kind(int_kind_type_id: TypeId, integer_type: IntegerType) -> StaticValue {
        StaticValue::Enum(int_kind_type_id, TypedIntValue::U8(integer_type as u8))
    }

    fn make_float_kind(float_kind_type_id: TypeId, float_type: FloatType) -> StaticValue {
        StaticValue::Enum(float_kind_type_id, TypedIntValue::U8(float_type as u8))
    }

    fn make_int_value(
        static_values: &mut StaticValuePool,
        int_value_type_id: TypeId,
        int_value_variants: &[TypedSumVariant],
        integer_value: TypedIntValue,
    ) -> StaticSum {
        let variant = match integer_value {
            TypedIntValue::U8(_) => int_value_variants[0],
            TypedIntValue::U16(_) => int_value_variants[1],
            TypedIntValue::U32(_) => int_value_variants[2],
            TypedIntValue::U64(_) => int_value_variants[3],
            TypedIntValue::I8(_) => int_value_variants[4],
            TypedIntValue::I16(_) => int_value_variants[5],
            TypedIntValue::I32(_) => int_value_variants[6],
            TypedIntValue::I64(_) => int_value_variants[7],
        };
        StaticSum {
            sum_type_id: int_value_type_id,
            variant_index: variant.index,
            payload: Some(static_values.add(StaticValue::Int(integer_value))),
        }
    }

    pub fn get_dylib_handle(
        &mut self,
        function_id: FunctionId,
        lib_name_ident: StringId,
        span: SpanId,
    ) -> K1Result<*mut std::ffi::c_void> {
        let Linkage::External { module_id, .. } = self.functions.get(function_id).linkage else {
            ice_span!(self, span, "get_dylib_handle called for non-external function");
        };
        if let Some(handle) = self.vm_dylib_handles.get(&(module_id, lib_name_ident)) {
            return Ok(*handle);
        }

        let lib_name_str = self.ast.idents.get_string(lib_name_ident);
        let ext = match self.config.target.target_os() {
            crate::compiler::TargetOs::Linux => "so",
            crate::compiler::TargetOs::MacOs => "dylib",
            crate::compiler::TargetOs::Wasm => {
                kbail!(self, span, "Dynamic libraries are not supported on the wasm target");
            }
        };
        debug!("cwd is: {}", std::env::current_dir().unwrap().display());
        debug!("src_path is: {}", self.ast.idents.get_string(self.config.src_path));

        let search_path = kpath::join_tmp(
            self.get_tmp_unsafe(),
            &self.ast.idents,
            self.modules.get(module_id).home_dir,
            (compiler::LIBS_DIR_NAME, format_args!("lib{lib_name_str}.{ext}")),
        );
        if let Some(handle) = self.attempt_dlopen(search_path.as_str()) {
            self.vm_dylib_handles.insert((module_id, lib_name_ident), handle);
            return Ok(handle);
        }

        // System lookup
        let logical_lib_path = lib_name_str;
        if let Some(handle) = self.attempt_dlopen(logical_lib_path) {
            self.vm_dylib_handles.insert((module_id, lib_name_ident), handle);
            return Ok(handle);
        }
        // For the system lookup, we call dlerror() to see everywhere it tried.
        let dlopen_error_message =
            unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) }.to_string_lossy();
        Err(kerr!(
            self,
            span,
            "Failed to dlopen library: '{}'. I tried the project's libs: `{}`, then tried a system lookup: {}",
            lib_name_str,
            search_path,
            dlopen_error_message
        ))
    }

    pub fn attempt_dlopen(&self, name: &str) -> Option<*mut c_void> {
        let c_lib_name = std::ffi::CString::new(name).unwrap();
        let handle = unsafe { libc::dlopen(c_lib_name.as_ptr(), libc::RTLD_LAZY) };
        if handle.is_null() { None } else { Some(handle) }
    }

    pub fn get_span_location(&self, span: SpanId) -> (&parse::SourceFile, parse::Line) {
        let the_span = self.ast.spans.get(span);
        let source = self.ast.sources.get(the_span.file_id);
        let line = source.get_line_for_span_start(&self.ast.mem, the_span).unwrap();
        (source, line)
    }

    pub fn write_scope_path<W: std::fmt::Write + ?Sized>(
        &self,
        w: &mut W,
        scope: ScopeId,
        delimiter: &str,
        skip_root: bool,
    ) {
        let starting_namespace = self.scopes.nearest_parent_namespace(scope);
        let mut chain: SmallVec<[StringId; 8]> = smallvec![];
        let mut ns_id = starting_namespace;
        loop {
            let namespace = self.namespaces.get(ns_id);
            chain.push(namespace.name);
            match namespace.parent_id {
                Some(parent_id) => ns_id = parent_id,
                None => break,
            }
        }
        for (i, name) in chain.iter().rev().enumerate() {
            let ident_str = self.ident_str(*name);
            let is_last = i == chain.len() - 1;

            let is_root = ident_str == "_root";
            if !(is_root && skip_root) {
                write!(w, "{ident_str}").unwrap();
                if !is_last {
                    write!(w, "{delimiter}").unwrap();
                }
            }
        }
    }

    pub fn write_qualified_name(
        &self,
        w: &mut impl std::fmt::Write,
        scope: ScopeId,
        name: &str,
        suffix_id: Option<usize>,
        delimiter: &str,
        skip_root: bool,
    ) {
        self.write_scope_path(w, scope, delimiter, skip_root);
        write!(w, "{delimiter}").unwrap();
        write!(w, "{}", name).unwrap();
        if let Some(suffix_id) = suffix_id {
            write!(w, "_{suffix_id}").unwrap()
        }
    }

    pub fn make_qualified_name(
        &self,
        scope: ScopeId,
        name: StringId,
        suffix_id: Option<usize>,
        delimiter: &str,
        skip_root: bool,
    ) -> String {
        let mut buf = String::with_capacity(64);
        self.write_qualified_name(
            &mut buf,
            scope,
            self.ident_str(name),
            suffix_id,
            delimiter,
            skip_root,
        );
        buf
    }

    // Errors and logging

    pub fn message_to_anyhow(&self, e: K1Message) -> anyhow::Error {
        anyhow::anyhow!("{}: {}", e.level, self.ident_str(e.message))
    }

    pub fn make_error(&self, message: impl AsRef<str>, span: SpanId) -> K1Message {
        make_message(&self.ast.idents, message, span, MessageLevel::Error)
    }

    pub fn make_warning(&self, message: impl AsRef<str>, span: SpanId) -> K1Message {
        make_message(&self.ast.idents, message, span, MessageLevel::Warn)
    }

    pub fn make_fail<A>(&self, message: impl AsRef<str>, span: SpanId) -> K1Result<A> {
        Err(self.make_error(message, span))
    }

    pub fn report(&mut self, e: K1Message) {
        self.report_ext(e, false)
    }
    /// Follows the emitted-file tables back to the span the metaprogram received,
    /// preserving the position within the containing chunk; a span not covered by
    /// any entry stays put
    pub fn remap_to_source_span(&mut self, span_id: SpanId) -> SpanId {
        let mut current = span_id;
        for _ in 0..16 {
            let span = self.ast.spans.get(current);
            let Some(table) = self.emitted_sources.iter().find(|e| e.file_id == span.file_id)
            else {
                return current;
            };
            let entries = self.mem.getn(table.entries);
            let candidate = entries.partition_point(|entry| entry.start <= span.start);
            let Some(&CodeChunkPos { start, end, source }) =
                candidate.checked_sub(1).map(|i| &entries[i])
            else {
                return current;
            };
            if span.start >= end {
                return current;
            }
            let source_span = self.ast.spans.get(source);
            let offset = span.start - start;
            // Emitted text can differ in length from its source span (escapes,
            // dedent); fall back to the whole source span past its end
            current = if offset < source_span.len {
                let len = span.len.min(end - span.start).min(source_span.len - offset);
                self.ast.spans.add(Span {
                    file_id: source_span.file_id,
                    start: source_span.start + offset,
                    len,
                })
            } else {
                source
            };
        }
        current
    }

    pub fn report_ext(&mut self, e: K1Message, no_print: bool) {
        let e = K1Message { span: self.remap_to_source_span(e.span), ..e };
        // Check for duplicates (happens a lot with generic code); remapping makes
        // fresh span ids, so spans compare by contents
        //
        // This will be kind quadraticy if we ever have a lot of diagnostics, which could
        // happen in happy path if we're doing a lot of hints
        let e_span = self.ast.spans.get(e.span);
        let is_duplicate = self.messages.borrow().iter().any(|m| {
            m.message == e.message
                && m.level == e.level
                && m.error_kind == e.error_kind
                && self.ast.spans.get(m.span) == e_span
        });
        if is_duplicate {
            return;
        }

        if let Some(table) = self.emitted_sources.iter_mut().find(|e| e.file_id == e_span.file_id) {
            if e.level == MessageLevel::Error {
                table.has_diagnostic = true
            }
        };

        let skip_print = no_print || {
            if e.level != MessageLevel::Error {
                // Don't print warnings when errors are present
                let has_errors = self.error_count(&[MessageLevel::Error]) > 0;
                has_errors
            } else {
                false
            }
        };

        if !skip_print {
            let use_color = std::io::stderr().is_terminal();
            self.write_error(&mut std::io::stderr(), &e, use_color).unwrap();
        }
        self.messages.borrow_mut().push(e);
    }

    pub fn report_hint(&mut self, span: SpanId, message: impl AsRef<str>) {
        self.report(K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Hint,
            error_kind: ErrorKind::None,
        });
    }

    pub fn report_warn(&mut self, span: SpanId, message: impl AsRef<str>) {
        self.report(K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Warn,
            error_kind: ErrorKind::None,
        });
    }

    pub fn log_hint(&self, span: SpanId, message: impl AsRef<str>) {
        let use_color = std::io::stderr().is_terminal();
        let hint = K1Message {
            message: self.ast.idents.intern(message),
            span,
            level: MessageLevel::Hint,
            error_kind: ErrorKind::None,
        };
        self.write_error(&mut std::io::stderr(), &hint, use_color).unwrap();
    }

    pub fn write_error(
        &self,
        w: &mut impl std::io::Write,
        error: &K1Message,
        use_color: bool,
    ) -> std::io::Result<()> {
        write_error(
            w,
            &self.ast,
            self.ident_str(error.message),
            error.level,
            error.span,
            use_color,
        )?;
        let file_id = self.ast.spans.get(error.span).file_id;
        if let Some(emitted) = self.emitted_sources.iter().find(|e| e.file_id == file_id) {
            writeln!(w, "note: in code compiled in place of this call:")?;
            self.write_location(w, emitted.call_span, use_color);
        }
        Ok(())
    }

    pub fn write_location(&self, w: &mut impl std::io::Write, span: SpanId, use_color: bool) {
        parse::write_source_location(w, &self.ast, span, MessageLevel::Info, 6, None, use_color)
            .unwrap()
    }

    pub fn write_location_error(&self, w: &mut impl std::io::Write, span: SpanId, use_color: bool) {
        parse::write_source_location(w, &self.ast, span, MessageLevel::Error, 6, None, use_color)
            .unwrap()
    }

    #[track_caller]
    pub fn ice_span(&self, span: SpanId, msg: impl AsRef<str>) -> ! {
        let use_color = std::io::stderr().is_terminal();
        self.write_location_error(&mut std::io::stderr(), span, use_color);
        panic!("Internal Compiler Error: {}", msg.as_ref())
    }

    #[track_caller]
    pub fn ice(&self, msg: impl AsRef<str>, error: Option<&K1Message>) -> ! {
        let use_color = std::io::stderr().is_terminal();
        if let Some(error) = error {
            self.write_error(&mut std::io::stderr(), error, use_color).unwrap();
        }
        panic!("Internal Compiler Error at: {}", msg.as_ref())
    }

    #[track_caller]
    pub fn todo_with_span(&self, msg: impl AsRef<str>, span: SpanId) -> ! {
        let use_color = std::io::stderr().is_terminal();
        self.write_location_error(&mut std::io::stderr(), span, use_color);
        panic!("not yet implemented: {}", msg.as_ref())
    }

    // Timing
    //
    pub fn print_timing_info(
        &self,
        module_name: &str,
        full_elapsed_ns: u64,
        out: &mut impl std::io::Write,
    ) -> std::io::Result<()> {
        let infer_ms = self.timing.total_infer_nanos as f64 / 1_000_000.0;
        let vm_ms = self.timing.total_vm_nanos as f64 / 1_000_000.0;
        let lines: usize = self.ast.sources.iter().map(|s| s.1.line_count(&self.ast.mem)).sum();
        // mm lines per ns, aka lines per second
        let lines_per_s = if lines > 0 { lines as f64 * 1e9 / full_elapsed_ns as f64 } else { 0.0 };
        eprintln!(
            "module {} took {}ms ({:.2} line/s, {} lines)",
            module_name,
            full_elapsed_ns / 1_000_000,
            lines_per_s,
            lines
        );
        eprintln!("\t{} expressions", self.exprs.len());
        eprintln!("\t{} statements", self.stmts.len());
        eprintln!("\t{} functions", self.functions.len());
        eprintln!("\t{} types", self.type_count());
        eprintln!("\t{} idents", self.ast.idents.len());
        let iropt_created = self.timing.iropt_insts_created;
        let irgen_created = self.ir.instrs.len() as i64 - iropt_created;
        eprintln!(
            "\t{} instructions ({} irgen + {} iropt, {:.2}x), {}ms ir, {}ms iropt, {:.2}ms bcgen ({} code words)",
            self.ir.instrs.len(),
            irgen_created,
            iropt_created,
            self.ir.instrs.len() as f64 / irgen_created.max(1) as f64,
            self.timing.total_ir_nanos / 1_000_000,
            self.timing.total_iropt_nanos / 1_000_000,
            self.timing.total_bcgen_nanos as f64 / 1_000_000.0,
            self.bc.code.len(),
        );
        eprintln!(
            "\t  iropt: {:.2}ms inline ({} inlines), {:.2}ms simplify ({} passes), {:.2}ms cfg ({} computes; nested in the others)",
            self.timing.iropt_inline_nanos as f64 / 1_000_000.0,
            self.timing.iropt_inline_count,
            self.timing.iropt_simplify_nanos as f64 / 1_000_000.0,
            self.timing.iropt_simplify_passes,
            self.timing.iropt_cfg_nanos as f64 / 1_000_000.0,
            self.timing.iropt_cfg_computes,
        );
        self.tmp.print_usage("\ttmp");
        self.mem.print_usage("\tperm");
        self.mem.print_usage("\tmem types");
        self.ir.mem.print_usage("\tmem ir");
        writeln!(
            out,
            "\t{} infers: {:.2}ms. avg: {:.2}ms. {} static execs during inference: {:.2}ms",
            self.timing.total_infers,
            infer_ms,
            if self.timing.total_infers > 0 {
                infer_ms / self.timing.total_infers as f64
            } else {
                0.0
            },
            self.timing.total_infer_execs,
            self.timing.total_infer_exec_nanos as f64 / 1_000_000.0,
        )?;
        let vm_us = self.timing.total_vm_nanos as f64 / 1_000.0;
        let vm_us_per_instr = vm_us / self.timing.total_vm_instrs as f64;

        writeln!(out, "\t{:.2}ms vm, avg {:.2}us/instr", vm_ms, vm_us_per_instr)?;
        let total_ops: i64 = self.timing.opcode_counts.iter().sum();
        if total_ops > 0 {
            let mut counts: Vec<(crate::bc::Opcode, i64)> =
                Vec::with_capacity(self.timing.opcode_counts.len());
            for (i, n) in self.timing.opcode_counts.iter().enumerate() {
                counts.push((crate::bc::Opcode::from_u8(i as u8), *n));
            }
            counts.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
            let mut parts = Vec::with_capacity(counts.len());
            let mut other = 0i64;
            for (op, n) in counts {
                let pct = n as f64 * 100.0 / total_ops as f64;
                if pct >= 1.0 {
                    parts.push(format!("{} {:.0}% ({})", op.name(), pct, human_count(n)));
                } else {
                    other += n;
                }
            }
            if other > 0 {
                parts.push(format!(
                    "other {:.0}% ({})",
                    other as f64 * 100.0 / total_ops as f64,
                    human_count(other)
                ));
            }
            writeln!(out, "\t  ops: {}", parts.join(", "))?;
        }
        writeln!(
            out,
            "\t{:.2}ms ferry (static_value <-> vm memory)",
            self.timing.total_ferry_nanos as f64 / 1_000_000.0
        )?;
        Ok(())
    }
}

fn human_count(n: i64) -> String {
    if n >= 1_000_000 {
        format!("{:.1}M", n as f64 / 1e6)
    } else if n >= 10_000 {
        format!("{}k", n / 1000)
    } else {
        n.to_string()
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
