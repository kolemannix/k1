use crate::debug;
/// Copyright (c) 2026 knix
/// All rights reserved.
///
/// The goal here is a strongly-typed SSA form
/// instruction-based IR with basic blocks, obviously
/// very much like LLVM, as that is our primary target.
/// But I currently think there's going to be a lot of value
/// in having our own. It'll be easier to write an interpreter for
/// and will help make adding other backends far, far easier
use crate::kmem::{DlNode, Dlist, Handle, List, NodeHandle};
use crate::parse::{self, NumericWidth, StringId};
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::StaticValueId;
use crate::{kbail, kerr, static_assert_size};
use crate::{
    kmem::{self, MSlice},
    lex::SpanId,
    nz_u32_id,
    typer::{types::*, *},
    vpool::VPool,
};
use ahash::{HashMapExt, HashSetExt};
use fxhash::{FxHashMap, FxHashSet};
use std::fmt::Write;

macro_rules! b_ice {
    ($b:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            $b.k1.ice_span($b.cur_span, s)
        }

    }
}

#[derive(Clone, Copy)]
pub struct IrDebugVariableInfo {
    pub name: StringId,
    pub original_type_id: TypeId,
    pub user_hidden: bool,
    pub source_span: SpanId,
}

#[derive(Default, Clone, Copy)]
pub struct IrDebugInfo {
    pub variable_info: Option<IrDebugVariableInfo>,
}

nz_u32_id!(IrCallId);
type IrHandle<T> = Handle<T, ProgramIr>;
pub struct ProgramIr {
    pub mem: kmem::Mem<ProgramIr>,
    pub instrs: VPool<Inst, InstId>,
    pub sources: VPool<SpanId, InstId>,
    pub comments: VPool<IrComment, InstId>,
    pub debug_info: VPool<IrDebugInfo, InstId>,
    /// Compiled ir for actual functions
    pub functions: VPool<Option<IrUnit>, FunctionId>,
    /// Compiled ir for #static exprs and global initializers
    pub exprs: FxHashMap<TypedExprId, IrUnit>,
    pub module_config: IrModuleConfig,
    pub calls: VPool<IrCall, IrCallId>,
    pub cmpxchgs: VPool<AtomicCmpxchgData, AtomicCmpxchgId>,
    pub vec_ops: VPool<VecOpData, VecOpId>,
    pub phys_fn_type_cache: FxHashMap<TypeId, PhysicalFunctionType>,

    // Builder data
    b_variables: FxHashMap<VariableId, BuilderVariable>,
    b_loops: FxHashMap<ScopeId, LoopInfo>,
    pub units_pending_compile: FxHashMap<FunctionId, ()>,
    pub globals_pending_eval: FxHashMap<TypedGlobalId, ()>,

    opt_buf_stack: Vec<iropt::OptVisit>,
    opt_buf_order: Vec<IrUnitId>,
    opt_buf_visited: FxHashSet<IrUnitId>,
    opt_buf_callees: Vec<FunctionId>,
    opt_buf_cfg_compute_work_stack: Vec<BlockId>,
    opt_buf_cfg_compute_visited: FxHashSet<BlockId>,
    opt_buf_inline_self_rewrites: iropt::RewriteMappings,
    opt_buf_inline_inlined_rewrites: iropt::RewriteMappings,
    opt_buf_cfg_simpl_rewrites: iropt::RewriteMappings,
}

static_assert_size!(IrComment, 1);

impl ProgramIr {
    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        use crate::snap::write_map_snap;
        let ProgramIr {
            mem,
            instrs,
            sources,
            comments,
            debug_info,
            functions,
            exprs,
            module_config: IrModuleConfig {},
            calls,
            cmpxchgs,
            vec_ops,
            phys_fn_type_cache: _,
            b_variables: _,
            b_loops: _,
            units_pending_compile,
            globals_pending_eval,
            opt_buf_stack: _,
            opt_buf_order: _,
            opt_buf_visited: _,
            opt_buf_callees: _,
            opt_buf_cfg_compute_work_stack: _,
            opt_buf_cfg_compute_visited: _,
            opt_buf_inline_self_rewrites: _,
            opt_buf_inline_inlined_rewrites: _,
            opt_buf_cfg_simpl_rewrites: _,
        } = self;
        w.write_section("ir");
        mem.snap(w);
        instrs.snap(w);
        sources.snap(w);
        comments.snap(w);
        debug_info.snap(w);
        functions.snap(w);
        write_map_snap(w, exprs);
        calls.snap(w);
        cmpxchgs.snap(w);
        vec_ops.snap(w);
        assert!(units_pending_compile.is_empty());
        assert!(globals_pending_eval.is_empty());
    }

    pub fn restore(&mut self, r: &mut crate::snap::SnapReader) {
        r.section("ir");
        self.mem.restore(r);
        self.instrs.restore(r);
        self.sources.restore(r);
        self.comments.restore(r);
        self.debug_info.restore(r);
        self.functions.restore(r);
        self.exprs = crate::snap::restore_map_snap(r);
        self.calls.restore(r);
        self.cmpxchgs.restore(r);
        self.vec_ops.restore(r);
        self.phys_fn_type_cache.clear();
    }
}

/// Provenance notes attached to every instruction for IR dumps.
#[derive(Clone, Copy)]
pub enum IrComment {
    ArrayGetOffsetPlace,
    BitcastAggToAggCopy,
    BitcastAggToAggPlace,
    BitcastAggToScalar,
    BitcastScalarToAggPlace,
    BitcastScalarToAggStore,
    BoolToInt,
    BreakLoopNoValue,
    BreakLoopWithValue,
    CmpxchgResult,
    DaCapoMaestro,
    DeliverFnPointer,
    DeliverSumPayload,
    DirectVariable,
    DivergentMatch,
    DynAbilityFnPtrOffset,
    DynAbilityStateOffset,
    DynLamEnvPtrOffset,
    DynLamFnPtrOffset,
    EmptyCondition,
    EnterInlinedCode,
    EnterLoop,
    EnterMatch,
    EnterWhileCond,
    EnumInt,
    ExitInlinedCode,
    FieldAccessNoCopy,
    FieldAccessWCopy,
    FoldedVariable,
    FulfillBitcastDestination,
    FulfillCastDestination,
    FulfillLoopBreakDst,
    FulfillMatchDst,
    FulfillVariableUsage,
    GetLaneLoad,
    GetLaneOffset,
    GetSumTagLoadOrCopyToDst,
    GotoWhileCond,
    InfallibleMatchingCondContinue,
    InlineRet,
    InlinedAggRet,
    InlinedScalarReturn,
    LambdaEnvLocation,
    LangDerefFulfillToDst,
    LangDerefNoDst,
    LoopBreakValue,
    MatchArmResultStore,
    MatchPhi,
    MatchingCondBindingFallthroughToCons,
    MatchingCondCond,
    MemsetSize,
    None,
    RefAtIndexOffset,
    ReturnAggregateAtAddress,
    RvoStorage,
    SourceLet,
    SplatResult,
    StaticEnum,
    StaticFloat,
    StaticInt,
    StoreLambdaEnvForCall,
    StoreScalarToDst,
    StoreStaticValueToDst,
    StoreSumLitTag,
    StructAccessPlace,
    StructLitFieldPtr,
    StructLiteral,
    SumLiteralStorage,
    SumPayloadOffset,
    SumPayloadPtr,
    SumTag,
    VecBinopResult,
    VecNotResult,
    VecShiftResult,
    VectorLoad,
    VectorLoadResult,
    VectorStore,
    WithLaneCopy,
    WithLaneOffset,
    WithLaneResult,
    WithLaneStore,
    ZeroedMemset,
    ZeroedNoDst,
}

impl IrComment {
    pub fn str(&self) -> &'static str {
        match self {
            IrComment::ArrayGetOffsetPlace => "array get offset place",
            IrComment::BitcastAggToAggCopy => "bitcast agg to agg copy",
            IrComment::BitcastAggToAggPlace => "bitcast agg to agg place",
            IrComment::BitcastAggToScalar => "bitcast agg to scalar",
            IrComment::BitcastScalarToAggPlace => "bitcast scalar to agg place",
            IrComment::BitcastScalarToAggStore => "bitcast scalar to agg store",
            IrComment::BoolToInt => "bool_to_int",
            IrComment::BreakLoopNoValue => "break loop (no value)",
            IrComment::BreakLoopWithValue => "break loop (with value)",
            IrComment::CmpxchgResult => "cmpxchg result",
            IrComment::DaCapoMaestro => "da capo maestro",
            IrComment::DeliverFnPointer => "deliver fn pointer",
            IrComment::DeliverSumPayload => "deliver sum payload",
            IrComment::DirectVariable => "direct variable",
            IrComment::DivergentMatch => "divergent match",
            IrComment::DynAbilityFnPtrOffset => "dyn ability fn ptr offset",
            IrComment::DynAbilityStateOffset => "dyn ability state offset",
            IrComment::DynLamEnvPtrOffset => "dyn lam env ptr offset",
            IrComment::DynLamFnPtrOffset => "dyn lam fn ptr offset",
            IrComment::EmptyCondition => "empty condition",
            IrComment::EnterInlinedCode => "enter inlined code",
            IrComment::EnterLoop => "enter loop",
            IrComment::EnterMatch => "enter match",
            IrComment::EnterWhileCond => "enter while cond",
            IrComment::EnumInt => "enum int",
            IrComment::ExitInlinedCode => "exit inlined code",
            IrComment::FieldAccessNoCopy => "field access no copy",
            IrComment::FieldAccessWCopy => "field access w copy",
            IrComment::FoldedVariable => "folded variable",
            IrComment::FulfillBitcastDestination => "fulfill bitcast destination",
            IrComment::FulfillCastDestination => "fulfill cast destination",
            IrComment::FulfillLoopBreakDst => "fulfill loop break dst",
            IrComment::FulfillMatchDst => "fulfill match dst",
            IrComment::FulfillVariableUsage => "fulfill variable usage",
            IrComment::GetLaneLoad => "get-lane load",
            IrComment::GetLaneOffset => "get-lane offset",
            IrComment::GetSumTagLoadOrCopyToDst => "get sum tag, load or copy to dst",
            IrComment::GotoWhileCond => "goto while cond",
            IrComment::InfallibleMatchingCondContinue => "infallible matching cond continue",
            IrComment::InlineRet => "inline ret",
            IrComment::InlinedAggRet => "inlined agg ret",
            IrComment::InlinedScalarReturn => "inlined scalar return",
            IrComment::LambdaEnvLocation => "lambda env location",
            IrComment::LangDerefFulfillToDst => "lang deref fulfill to dst",
            IrComment::LangDerefNoDst => "lang deref no dst",
            IrComment::LoopBreakValue => "loop break value",
            IrComment::MatchArmResultStore => "match arm result store",
            IrComment::MatchPhi => "match phi",
            IrComment::MatchingCondBindingFallthroughToCons => {
                "matching cond binding fallthrough to cons"
            }
            IrComment::MatchingCondCond => "matching cond cond",
            IrComment::MemsetSize => "memset size",
            IrComment::None => "",
            IrComment::RefAtIndexOffset => "refAtIndex offset",
            IrComment::ReturnAggregateAtAddress => "return aggregate at address",
            IrComment::RvoStorage => "rvo storage",
            IrComment::SourceLet => "source let",
            IrComment::SplatResult => "splat result",
            IrComment::StaticEnum => "static enum",
            IrComment::StaticFloat => "static float",
            IrComment::StaticInt => "static int",
            IrComment::StoreLambdaEnvForCall => "store lambda env for call",
            IrComment::StoreScalarToDst => "store scalar to dst",
            IrComment::StoreStaticValueToDst => "store static value to dst",
            IrComment::StoreSumLitTag => "store sum lit tag",
            IrComment::StructAccessPlace => "struct access place",
            IrComment::StructLitFieldPtr => "struct lit field ptr",
            IrComment::StructLiteral => "struct literal",
            IrComment::SumLiteralStorage => "sum literal storage",
            IrComment::SumPayloadOffset => "sum payload offset",
            IrComment::SumPayloadPtr => "sum payload ptr",
            IrComment::SumTag => "sum tag",
            IrComment::VecBinopResult => "vec binop result",
            IrComment::VecNotResult => "vec not result",
            IrComment::VecShiftResult => "vec shift result",
            IrComment::VectorLoad => "vector load",
            IrComment::VectorLoadResult => "vector load result",
            IrComment::VectorStore => "vector store",
            IrComment::WithLaneCopy => "with-lane copy",
            IrComment::WithLaneOffset => "with-lane offset",
            IrComment::WithLaneResult => "with-lane result",
            IrComment::WithLaneStore => "with-lane store",
            IrComment::ZeroedMemset => "zeroed memset",
            IrComment::ZeroedNoDst => "zeroed no dst",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrUnitId {
    Function(FunctionId),
    Expr(TypedExprId),
}

pub struct IrModuleConfig {}

impl ProgramIr {
    pub fn make() -> Self {
        ProgramIr {
            mem: kmem::Mem::make(),
            instrs: VPool::make("ir_soa_instrs"),
            sources: VPool::make("ir_soa_sources"),
            comments: VPool::make("ir_soa_comments"),
            debug_info: VPool::make("ir_soa_debug_info"),
            functions: VPool::make("ir_functions"),
            calls: VPool::make("ir_calls"),
            cmpxchgs: VPool::make("ir_cmpxchgs"),
            vec_ops: VPool::make("ir_vec_ops"),
            phys_fn_type_cache: FxHashMap::new(),
            exprs: FxHashMap::new(),
            module_config: IrModuleConfig {},
            b_variables: FxHashMap::new(),
            b_loops: FxHashMap::default(),
            units_pending_compile: FxHashMap::new(),
            globals_pending_eval: FxHashMap::new(),

            opt_buf_stack: vec![],
            opt_buf_order: vec![],
            opt_buf_visited: FxHashSet::new(),
            opt_buf_callees: vec![],
            opt_buf_cfg_compute_work_stack: vec![],
            opt_buf_cfg_compute_visited: FxHashSet::new(),
            opt_buf_inline_self_rewrites: iropt::RewriteMappings::default(),
            opt_buf_inline_inlined_rewrites: iropt::RewriteMappings::default(),
            opt_buf_cfg_simpl_rewrites: iropt::RewriteMappings::default(),
        }
    }

    fn word_sized_int(&self) -> ScalarType {
        ScalarType::U64
    }

    pub fn add_inst(
        &mut self,
        inst: Inst,
        comment: IrComment,
        debug_info: IrDebugInfo,
        span: SpanId,
    ) -> InstId {
        let id = self.instrs.add(inst);
        self.sources.add_expected_id(span, id);
        self.comments.add_expected_id(comment, id);
        self.debug_info.add_expected_id(debug_info, id);
        id
    }
}

pub type IrList<T> = Dlist<T, ProgramIr>;

/// Which source construct produced a block; also its display name in dumps
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BlockSourceKind {
    Entry,
    ExprToplevel,
    RequireContinue,
    RequireElse,
    ArmCond,
    ArmCons,
    MatchFail,
    MatchEnd,
    WhileLoopCondition,
    WhileLoopBody,
    WhileLoopEnd,
    LoopBody,
    LoopEnd,
    MatchingCondContinue,
}

impl BlockSourceKind {
    pub fn str(&self) -> &'static str {
        match self {
            BlockSourceKind::Entry => "entry",
            BlockSourceKind::ExprToplevel => "expr_toplevel",
            BlockSourceKind::RequireContinue => "require_continue",
            BlockSourceKind::RequireElse => "require_else",
            BlockSourceKind::ArmCond => "arm_cond",
            BlockSourceKind::ArmCons => "arm_cons",
            BlockSourceKind::MatchFail => "match_fail",
            BlockSourceKind::MatchEnd => "match_end",
            BlockSourceKind::WhileLoopCondition => "while_loop_condition",
            BlockSourceKind::WhileLoopBody => "while_loop_body",
            BlockSourceKind::WhileLoopEnd => "while_loop_end",
            BlockSourceKind::LoopBody => "loop_body",
            BlockSourceKind::LoopEnd => "loop_end",
            BlockSourceKind::MatchingCondContinue => "matching_cond_continue",
        }
    }
}

#[derive(Clone, Copy)]
pub struct Block {
    pub kind: BlockSourceKind,
    pub instrs: IrList<InstId>,

    pub preds: IrList<BlockId>,
    pub succs: IrList<BlockId>,
}

impl Block {
    pub fn empty(kind: BlockSourceKind) -> Block {
        Block { kind, instrs: IrList::empty(), preds: IrList::empty(), succs: IrList::empty() }
    }
    pub fn identical(&self, other: &Block) -> bool {
        self.kind == other.kind
            && self.instrs.first == other.instrs.first
            && self.instrs.last == other.instrs.last
    }
}

#[derive(Clone, Copy)]
pub struct IrUnit {
    pub result_type_id: TypeId,
    pub unit_id: IrUnitId,
    pub fn_type: PhysicalFunctionType,
    // The number of instructions in this unit
    pub inst_count: u32,
    pub last_alloca_index: Option<u32>,

    pub blocks: Dlist<Block, ProgramIr>,
    pub function_builtin_kind: Option<BackendBuiltin>,
    pub is_debug: bool,

    pub inline_done: bool,
    pub cfg_valid: bool,
}

#[derive(Clone, Copy)]
pub enum DataInst {
    U64(u64),
    I64(i64),
    Float(TypedFloatValue),
}

nz_u32_id!(InstId);
impl InstId {
    fn as_value(&self) -> Value {
        Value::Inst(*self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BackendBuiltin {
    // In LLVM backend, this becomes a runtime switch
    // In the VM, just a lookup. So we leave it as a builtin
    // rather than generate ir for it due to that difference
    TypeSchema,
    TypeName,

    // Platform-provided
    MemCopy,
    MemMove,
    MemSet,
    MemEquals,
    Exit,

    CompilerMessage,
    /// k1/repl/checkbox: a repl cell registering a checkbox widget; the VM
    /// accumulates it as a ReplCommand for the megarepl engine to drain
    ReplCheckbox,
}

impl BackendBuiltin {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BackendBuiltin::TypeSchema => "type_schema",
            BackendBuiltin::TypeName => "type_name",
            BackendBuiltin::MemCopy => "mem_copy",
            BackendBuiltin::MemMove => "mem_move",
            BackendBuiltin::MemSet => "mem_set",
            BackendBuiltin::MemEquals => "mem_equals",
            BackendBuiltin::Exit => "exit",
            BackendBuiltin::CompilerMessage => "compiler_message",
            BackendBuiltin::ReplCheckbox => "repl_checkbox",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtomicOrderingIr {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

impl AtomicOrderingIr {
    pub const fn to_tag(self) -> u8 {
        self as u8
    }

    pub fn from_tag(tag: u8) -> AtomicOrderingIr {
        match tag {
            0 => AtomicOrderingIr::Relaxed,
            1 => AtomicOrderingIr::Acquire,
            2 => AtomicOrderingIr::Release,
            3 => AtomicOrderingIr::AcqRel,
            4 => AtomicOrderingIr::SeqCst,
            _ => unreachable!("bad atomic ordering tag {tag}"),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            AtomicOrderingIr::Relaxed => "relaxed",
            AtomicOrderingIr::Acquire => "acquire",
            AtomicOrderingIr::Release => "release",
            AtomicOrderingIr::AcqRel => "acq-rel",
            AtomicOrderingIr::SeqCst => "seq-cst",
        }
    }
}

/// Min/Max signedness is baked in here, resolved from the element type at lowering
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtomicRmwOpIr {
    Xchg,
    Add,
    Sub,
    And,
    Or,
    Xor,
    MinS,
    MaxS,
    MinU,
    MaxU,
}

impl AtomicRmwOpIr {
    pub const fn to_tag(self) -> u8 {
        self as u8
    }

    pub fn from_tag(tag: u8) -> AtomicRmwOpIr {
        match tag {
            0 => AtomicRmwOpIr::Xchg,
            1 => AtomicRmwOpIr::Add,
            2 => AtomicRmwOpIr::Sub,
            3 => AtomicRmwOpIr::And,
            4 => AtomicRmwOpIr::Or,
            5 => AtomicRmwOpIr::Xor,
            6 => AtomicRmwOpIr::MinS,
            7 => AtomicRmwOpIr::MaxS,
            8 => AtomicRmwOpIr::MinU,
            9 => AtomicRmwOpIr::MaxU,
            _ => unreachable!("bad atomic rmw op tag {tag}"),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            AtomicRmwOpIr::Xchg => "xchg",
            AtomicRmwOpIr::Add => "add",
            AtomicRmwOpIr::Sub => "sub",
            AtomicRmwOpIr::And => "and",
            AtomicRmwOpIr::Or => "or",
            AtomicRmwOpIr::Xor => "xor",
            AtomicRmwOpIr::MinS => "mins",
            AtomicRmwOpIr::MaxS => "maxs",
            AtomicRmwOpIr::MinU => "minu",
            AtomicRmwOpIr::MaxU => "maxu",
        }
    }
}

nz_u32_id!(AtomicCmpxchgId);

/// Writes `{ prev: t, ok: bool }` through `result`; `ok_vm_offset` is the byte
/// offset of `ok` within that aggregate.
#[derive(Clone, Copy)]
pub struct AtomicCmpxchgData {
    pub t: ScalarType,
    pub dst: Value,
    pub expected: Value,
    pub desired: Value,
    pub success: AtomicOrderingIr,
    pub failure: AtomicOrderingIr,
    pub weak: bool,
    pub result: Value,
    pub ok_vm_offset: u32,
}

nz_u32_id!(VecOpId);

/// A lane-wise vector operation. Vectors are memory-backed aggregates in this IR:
/// `dst`/vector operands are addresses. Int/float class and shift signedness
/// are derived from `elem` by the backends.
///
/// Operand shapes: Splat lhs=scalar; unary ops rhs=Empty; Shl/Shr rhs=scalar
/// count; ToMask dst=Empty and the inst itself is the scalar u64 result.
#[derive(Clone, Copy)]
pub struct VecOpData {
    pub op: VecOpIr,
    pub elem: ScalarType,
    pub lanes: u32,
    pub dst: Value,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VecOpIr {
    Splat,
    Add,
    Sub,
    Mul,
    BitNot,
    BitAnd,
    BitOr,
    Xor,
    Shl,
    Shr,
    EqLanes,
    ToMask,
}

impl VecOpIr {
    pub fn name(&self) -> &'static str {
        match self {
            VecOpIr::Splat => "splat",
            VecOpIr::Add => "add",
            VecOpIr::Sub => "sub",
            VecOpIr::Mul => "mul",
            VecOpIr::BitNot => "bit_not",
            VecOpIr::BitAnd => "bit_and",
            VecOpIr::BitOr => "bit_or",
            VecOpIr::Xor => "xor",
            VecOpIr::Shl => "shl",
            VecOpIr::Shr => "shr",
            VecOpIr::EqLanes => "eq_lanes",
            VecOpIr::ToMask => "to_mask",
        }
    }
}

#[derive(Copy, Clone)]
pub struct PhysicalFunctionParam {
    pub original_index: Option<u16>,
    pub pt: PhysicalType,
}

#[derive(Copy, Clone)]
pub struct PhysicalFunctionType {
    pub return_type: PhysicalType,
    pub diverges: bool,
    pub params: MSlice<PhysicalFunctionParam, ProgramIr>,
    pub abi_mode: AbiMode,
}

impl PhysicalFunctionType {
    const fn nil() -> PhysicalFunctionType {
        PhysicalFunctionType {
            return_type: PhysicalType::EMPTY,
            diverges: false,
            params: MSlice::empty(),
            abi_mode: AbiMode::Internal,
        }
    }
}

#[derive(Clone, Copy)]
pub enum IrCallee {
    /// The backend is responsible for implementing this call
    BackendBuiltin(FunctionId, BackendBuiltin),
    /// A normal function call
    Direct(FunctionId),
    /// Standard 'indirect' call, by function pointer
    Indirect(PhysicalFunctionType, Value),
    /// Externally linked call. The VM will attempt to dynamically
    /// invoke this function using libffi, the llvm backend will just
    /// emit a call and expect linkage
    Extern {
        library_name: Option<parse::StringId>,
        function_name: parse::StringId,
        function_id: FunctionId,
    },
    /// A named LLVM intrinsic (`intern("llvm.cttz.i64")`). The llvm backend
    /// declares and calls it verbatim; the VM emulates by name from a host
    /// table, erroring lazily on names it does not know
    LlvmIntrinsic { name: parse::StringId, function_id: FunctionId },
    // (No lambda call; been compiled down to just calls and args by now)
}

impl IrCallee {
    fn known_function_id(&self) -> Option<FunctionId> {
        match self {
            IrCallee::Direct(fid) => Some(*fid),
            IrCallee::Extern { function_id, .. } => Some(*function_id),
            IrCallee::LlvmIntrinsic { function_id, .. } => Some(*function_id),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct PhiCase {
    pub from: BlockId,
    pub value: Value,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Inst(InstId),
    /// `Global` is always a storage location, regardless
    /// of the `k1` global declaration kind (referencing or not!)
    /// This greatly simplifies downstream code
    GlobalAddr {
        storage_pt: PhysicalType,
        id: TypedGlobalId,
    },
    StaticValue {
        t: PhysicalType,
        id: StaticValueId,
    },
    FunctionAddr(FunctionId),
    FnParam {
        t: PhysicalType,
        index: u32,
    },

    // Large 'immediates' just get encoded as their own instruction
    // We have space for u32, so we use it
    Data32 {
        t: ScalarType,
        data: u32,
    },
    Empty,
}

impl Value {
    const fn byte(u8: u8) -> Value {
        Value::Data32 { t: ScalarType::U8, data: u8 as u32 }
    }
    const fn imm32(t: ScalarType, u32: u32) -> Value {
        Value::Data32 { t, data: u32 }
    }
}

#[derive(Clone, Copy)]
pub struct IrCall {
    /// This is the logical return type, no ABI or sret shenanigans
    pub ret_type: PhysicalType,
    pub callee: IrCallee,
    pub args: MSlice<Value, ProgramIr>,
    pub dst: Option<Value>,
}

/// Many of these instructions contain both a high-level description of a type
/// via a `PhysicalType` or `PhysicalTypeId` as well as a low-level size in bytes
/// that presumes a certain offset. This is because this representation serves dual purposes
/// 1. Efficient-ish interpretation at compile-time, where we get to decide offsets
/// 2. Efficient translation into LLVM-ir, which wants to know about aggregate types in order
///    to optimize them away.
///
/// So we simply offer a mixed-level IR. It helps that K1 only supports targets whose alignment and sizing rules conform with what the VM does.
/// This would allow for a universal wire-format for k1 data and we could say that "nothing is platform-specific"* in terms of data layout which would be great
///
/// *Function call conventions vary by the major platforms though so that is still something that will of course be platform-dependent.
#[derive(Clone, Copy)]
pub enum Inst {
    Data(DataInst),

    // Memory manipulation
    Alloca {
        t: PhysicalType,
        vm_layout: Layout,
        returned: bool,
    },
    Store {
        dst: Value,
        value: Value,
        t: ScalarType,
    },
    Load {
        t: ScalarType,
        src: Value,
    },
    AtomicLoad {
        t: ScalarType,
        src: Value,
        ord: AtomicOrderingIr,
    },
    AtomicStore {
        dst: Value,
        value: Value,
        t: ScalarType,
        ord: AtomicOrderingIr,
    },
    AtomicRmw {
        op: AtomicRmwOpIr,
        t: ScalarType,
        dst: Value,
        operand: Value,
        ord: AtomicOrderingIr,
    },
    AtomicCmpxchg {
        id: AtomicCmpxchgId,
    },
    VecOp {
        id: VecOpId,
    },
    Fence {
        ord: AtomicOrderingIr,
    },
    Copy {
        dst: Value,
        src: Value,
        t: PhysicalType,
        vm_size: u32,
    },
    StructOffset {
        struct_t: AggregateTypeId,
        base: Value,
        field_index: u32,
        vm_offset: u32,
    },
    ArrayOffset {
        element_t: PhysicalType,
        base: Value,
        element_index: Value,
    },

    /// The Call instruction stores its destination.
    /// There's all sorts of interesting stuff that later phases can do
    /// with this info; depending on the ABI and if the return value is an
    /// aggregate type, and if its big or small. But coupling it
    /// with the instruction is a completely ABI-agnostic way of providing
    /// the most and best information needed by the 'backend' for generating
    /// ideal code for the return value's placement
    Call {
        call_id: IrCallId,
    },

    // Control Flow
    Jump(BlockId),
    JumpIf {
        cond: Value,
        cons: BlockId,
        alt: BlockId,
    },
    Unreachable,
    // goto considered harmful, but came-from is friend (phi node)
    Phi {
        t: PhysicalType,
        incomings: MSlice<PhiCase, ProgramIr>,
    },
    Ret {
        v: Value,
        agg: bool,
    },

    // Value Operations
    BoolNegate {
        v: Value,
    },
    BitNot {
        v: Value,
    },
    BitCast {
        v: Value,
        to: PhysicalType,
    },
    IntTrunc {
        v: Value,
        to: ScalarType,
    },
    IntExtU {
        v: Value,
        to: ScalarType,
    },
    IntExtS {
        v: Value,
        from: ScalarType,
        to: ScalarType,
    },
    FloatTrunc {
        v: Value,
        to: ScalarType,
    },
    FloatExt {
        v: Value,
        to: ScalarType,
    },
    Float32ToIntUnsigned {
        v: Value,
        to: ScalarType,
    },
    Float64ToIntUnsigned {
        v: Value,
        to: ScalarType,
    },
    Float32ToIntSigned {
        v: Value,
        to: ScalarType,
    },
    Float64ToIntSigned {
        v: Value,
        to: ScalarType,
    },
    IntToFloatUnsigned {
        v: Value,
        from: ScalarType,
        to: ScalarType,
    },
    IntToFloatSigned {
        v: Value,
        from: ScalarType,
        to: ScalarType,
    },
    PtrToWord {
        v: Value,
    },
    WordToPtr {
        v: Value,
    },
    IntAdd {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntSub {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntMul {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntDivUnsigned {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntDivSigned {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntRemUnsigned {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntRemSigned {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    IntCmp {
        lhs: Value,
        rhs: Value,
        pred: IntCmpPred,
        width: u8,
    },
    FloatAdd {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    FloatSub {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    FloatMul {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    FloatDiv {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    FloatRem {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    FloatCmp {
        lhs: Value,
        rhs: Value,
        pred: FloatCmpPred,
        width: u8,
    },
    BitAnd {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    BitOr {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    BitXor {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    BitShiftLeft {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    BitUnsignedShiftRight {
        lhs: Value,
        rhs: Value,
        width: u8,
    },
    BitSignedShiftRight {
        lhs: Value,
        rhs: Value,
        width: u8,
    },

    // Metaprogramming / Magic
    BakeStaticValue {
        type_id: TypeId,
        value: Value,
    },
}

impl Inst {
    fn is_phi(&self) -> bool {
        matches!(self, Inst::Phi { .. })
    }
}

/// Visit every `Value` operand of `inst`, read-only. Calls, phis, and
/// cmpxchgs read their out-of-line payloads from the arenas.
pub fn visit_inst_values(ir: &ProgramIr, inst: &Inst, f: &mut impl FnMut(Value)) {
    match *inst {
        Inst::Data(_)
        | Inst::Alloca { .. }
        | Inst::Fence { .. }
        | Inst::Jump(_)
        | Inst::Unreachable => {}
        Inst::Store { dst, value, .. } | Inst::AtomicStore { dst, value, .. } => {
            f(dst);
            f(value);
        }
        Inst::Load { src, .. } | Inst::AtomicLoad { src, .. } => f(src),
        Inst::AtomicRmw { dst, operand, .. } => {
            f(dst);
            f(operand);
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = *ir.cmpxchgs.get(id);
            f(cas.dst);
            f(cas.expected);
            f(cas.desired);
            f(cas.result);
        }
        Inst::VecOp { id } => {
            let vop = *ir.vec_ops.get(id);
            f(vop.dst);
            f(vop.lhs);
            f(vop.rhs);
        }
        Inst::Copy { dst, src, .. } => {
            f(dst);
            f(src);
        }
        Inst::StructOffset { base, .. } => f(base),
        Inst::ArrayOffset { base, element_index, .. } => {
            f(base);
            f(element_index);
        }
        Inst::Call { call_id } => {
            let call = *ir.calls.get(call_id);
            if let IrCallee::Indirect(_, v) = call.callee {
                f(v);
            }
            if let Some(dst) = call.dst {
                f(dst);
            }
            for arg in ir.mem.getn(call.args) {
                f(*arg);
            }
        }
        Inst::JumpIf { cond, .. } => f(cond),
        Inst::Phi { incomings, .. } => {
            for case in ir.mem.getn(incomings) {
                f(case.value);
            }
        }
        Inst::Ret { v, .. } => f(v),
        Inst::BakeStaticValue { value, .. } => f(value),
        Inst::BoolNegate { v }
        | Inst::BitNot { v }
        | Inst::BitCast { v, .. }
        | Inst::IntTrunc { v, .. }
        | Inst::IntExtU { v, .. }
        | Inst::IntExtS { v, .. }
        | Inst::FloatTrunc { v, .. }
        | Inst::FloatExt { v, .. }
        | Inst::Float32ToIntUnsigned { v, .. }
        | Inst::Float64ToIntUnsigned { v, .. }
        | Inst::Float32ToIntSigned { v, .. }
        | Inst::Float64ToIntSigned { v, .. }
        | Inst::IntToFloatUnsigned { v, .. }
        | Inst::IntToFloatSigned { v, .. }
        | Inst::PtrToWord { v }
        | Inst::WordToPtr { v } => f(v),
        Inst::IntAdd { lhs, rhs, .. }
        | Inst::IntSub { lhs, rhs, .. }
        | Inst::IntMul { lhs, rhs, .. }
        | Inst::IntDivUnsigned { lhs, rhs, .. }
        | Inst::IntDivSigned { lhs, rhs, .. }
        | Inst::IntRemUnsigned { lhs, rhs, .. }
        | Inst::IntRemSigned { lhs, rhs, .. }
        | Inst::IntCmp { lhs, rhs, .. }
        | Inst::FloatAdd { lhs, rhs, .. }
        | Inst::FloatSub { lhs, rhs, .. }
        | Inst::FloatMul { lhs, rhs, .. }
        | Inst::FloatDiv { lhs, rhs, .. }
        | Inst::FloatRem { lhs, rhs, .. }
        | Inst::FloatCmp { lhs, rhs, .. }
        | Inst::BitAnd { lhs, rhs, .. }
        | Inst::BitOr { lhs, rhs, .. }
        | Inst::BitXor { lhs, rhs, .. }
        | Inst::BitShiftLeft { lhs, rhs, .. }
        | Inst::BitUnsignedShiftRight { lhs, rhs, .. }
        | Inst::BitSignedShiftRight { lhs, rhs, .. } => {
            f(lhs);
            f(rhs);
        }
    }
}

static_assert_size!(Inst, 32);

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IntCmpPred {
    Eq,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

impl std::fmt::Display for IntCmpPred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntCmpPred::Eq => "eq",
            IntCmpPred::Slt => "slt",
            IntCmpPred::Sle => "sle",
            IntCmpPred::Sgt => "sgt",
            IntCmpPred::Sge => "sge",
            IntCmpPred::Ult => "ult",
            IntCmpPred::Ule => "ule",
            IntCmpPred::Ugt => "ugt",
            IntCmpPred::Uge => "uge",
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatCmpPred {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

impl std::fmt::Display for FloatCmpPred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FloatCmpPred::Eq => "eq",
            FloatCmpPred::Lt => "lt",
            FloatCmpPred::Le => "le",
            FloatCmpPred::Gt => "gt",
            FloatCmpPred::Ge => "ge",
        };
        write!(f, "{}", s)
    }
}

pub fn get_value_kind(ir: &ProgramIr, value: Value) -> InstKind {
    match value {
        Value::Inst(inst_id) => get_inst_kind(ir, inst_id),
        Value::GlobalAddr { storage_pt: _, id: _ } => InstKind::PTR,
        Value::StaticValue { t, id: _ } => InstKind::Value(t),
        Value::FunctionAddr(_) => InstKind::PTR,
        Value::FnParam { t, .. } => InstKind::Value(t),
        Value::Data32 { t: scalar_type, data: _ } => {
            InstKind::Value(PhysicalType::scalar(scalar_type))
        }
        Value::Empty => InstKind::Value(PhysicalType::EMPTY),
    }
}

pub fn get_inst_kind(ir: &ProgramIr, inst_id: InstId) -> InstKind {
    match *ir.instrs.get(inst_id) {
        Inst::Data(imm) => match imm {
            DataInst::I64(_) => InstKind::scalar(ScalarType::I64),
            DataInst::U64(_) => InstKind::scalar(ScalarType::U64),
            DataInst::Float(TypedFloatValue::F32(_)) => InstKind::scalar(ScalarType::F32),
            DataInst::Float(TypedFloatValue::F64(_)) => InstKind::scalar(ScalarType::F64),
        },
        Inst::Alloca { .. } => InstKind::PTR,
        Inst::Store { .. } => InstKind::Void,
        Inst::Load { t, .. } => InstKind::scalar(t),
        Inst::AtomicLoad { t, .. } => InstKind::scalar(t),
        Inst::AtomicStore { .. } => InstKind::Void,
        Inst::AtomicRmw { t, .. } => InstKind::scalar(t),
        Inst::AtomicCmpxchg { .. } => InstKind::Void,
        Inst::VecOp { id } => match ir.vec_ops.get(id).op {
            VecOpIr::ToMask => InstKind::U64,
            _ => InstKind::Void,
        },
        Inst::Fence { .. } => InstKind::Void,
        Inst::Copy { .. } => InstKind::Void,
        Inst::StructOffset { .. } => InstKind::PTR,
        Inst::ArrayOffset { .. } => InstKind::PTR,
        Inst::Call { call_id: id } => InstKind::Value(ir.calls.get(id).ret_type),
        Inst::Jump(_) => InstKind::Terminator,
        Inst::JumpIf { .. } => InstKind::Terminator,
        Inst::Unreachable => InstKind::Terminator,
        Inst::Phi { t, .. } => InstKind::Value(t),
        Inst::Ret { .. } => InstKind::Terminator,
        Inst::BoolNegate { .. } => InstKind::BOOL,
        Inst::BitNot { v } => get_value_kind(ir, v),
        Inst::BitCast { to, .. } => InstKind::Value(to),
        Inst::IntTrunc { to, .. } => InstKind::scalar(to),
        Inst::IntExtU { to, .. } => InstKind::scalar(to),
        Inst::IntExtS { to, .. } => InstKind::scalar(to),
        Inst::FloatTrunc { to, .. } => InstKind::scalar(to),
        Inst::FloatExt { to, .. } => InstKind::scalar(to),
        Inst::Float32ToIntUnsigned { to, .. } => InstKind::scalar(to),
        Inst::Float32ToIntSigned { to, .. } => InstKind::scalar(to),
        Inst::Float64ToIntUnsigned { to, .. } => InstKind::scalar(to),
        Inst::Float64ToIntSigned { to, .. } => InstKind::scalar(to),
        Inst::IntToFloatUnsigned { to, .. } => InstKind::scalar(to),
        Inst::IntToFloatSigned { to, .. } => InstKind::scalar(to),
        Inst::PtrToWord { .. } => InstKind::scalar(ir.word_sized_int()),
        Inst::WordToPtr { .. } => InstKind::PTR,
        Inst::IntAdd { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntSub { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntMul { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntDivUnsigned { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntDivSigned { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntRemUnsigned { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntRemSigned { lhs, .. } => get_value_kind(ir, lhs),
        Inst::IntCmp { .. } => InstKind::BOOL,
        Inst::FloatAdd { lhs, .. } => get_value_kind(ir, lhs),
        Inst::FloatSub { lhs, .. } => get_value_kind(ir, lhs),
        Inst::FloatMul { lhs, .. } => get_value_kind(ir, lhs),
        Inst::FloatDiv { lhs, .. } => get_value_kind(ir, lhs),
        Inst::FloatRem { lhs, .. } => get_value_kind(ir, lhs),
        Inst::FloatCmp { .. } => InstKind::BOOL,
        Inst::BitAnd { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BitOr { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BitXor { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BitShiftLeft { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BitUnsignedShiftRight { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BitSignedShiftRight { lhs, .. } => get_value_kind(ir, lhs),
        Inst::BakeStaticValue { .. } => InstKind::scalar(ScalarType::U64),
    }
}

#[derive(Clone, Copy)]
pub enum InstKind {
    Value(PhysicalType),
    Void,
    Terminator,
}

impl InstKind {
    pub const EMPTY: InstKind = Self::Value(PhysicalType::EMPTY);
    pub const BOOL: InstKind = Self::scalar(ScalarType::Bool);
    pub const PTR: InstKind = Self::scalar(ScalarType::Pointer);
    pub const U64: InstKind = Self::scalar(ScalarType::U64);

    pub const fn scalar(st: ScalarType) -> InstKind {
        InstKind::Value(PhysicalType::scalar(st))
    }

    fn is_ptr(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_ptr())
    }
    fn is_int(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_int())
    }
    fn is_bool(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_bool())
    }
    fn is_aggregate(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_agg())
    }
    fn is_storage(&self) -> bool {
        self.is_ptr() || self.is_aggregate()
    }
    fn is_value(&self) -> bool {
        matches!(self, InstKind::Value(_))
    }
    fn is_empty(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_empty())
    }
    #[track_caller]
    pub fn expect_value(&self) -> Result<PhysicalType, String> {
        match self {
            InstKind::Value(t) => Ok(*t),
            _ => Err(format!("Expected value, got {}", self.kind_name())),
        }
    }
    fn as_value(&self) -> Option<PhysicalType> {
        match self {
            InstKind::Value(t) => Some(*t),
            _ => None,
        }
    }
    fn is_terminator(&self) -> bool {
        matches!(self, InstKind::Terminator)
    }
    fn is_void(&self) -> bool {
        matches!(self, InstKind::Void)
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            InstKind::Value(_) => "value",
            InstKind::Void => "void",
            InstKind::Terminator => "terminator",
        }
    }
}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> K1Result<()> {
    let start = k1.timing.clock.raw();
    if k1.ir.functions.get(function_id).is_some() {
        return Ok(());
    }

    let mut b = Builder::new(k1);

    //eprintln!("ir::compile_function {}", b.k1.function_id_to_string(function_id, false));
    let f = b.k1.get_function(function_id);
    let function_type = b.k1.types.get(f.type_id).expect_function();
    let return_type_id = function_type.return_type;
    if let Some(err) = f.body_failure {
        return Err(K1Message {
            message: b.k1.ast.idents.intern(format!(
                "Cannot generate ir for function {}, which failed compilation",
                b.k1.ident_str(f.name)
            )),
            span: err.span,
            level: err.level,
            error_kind: ErrorKind::Malformed,
        });
    }
    let intrinsic_type = f.builtin_type;
    let is_debug = f.compiler_debug;
    let fn_span = b.k1.ast.get_span_for_id(f.parsed_id);
    b.cur_span = fn_span;
    b.entry_span = fn_span;

    // Set up parameters
    let fn_params = f.params;
    let phys_fn_type = b.get_physical_fn_type(f.type_id);
    b.fn_type = phys_fn_type;
    let mut non_empty_index = 0;
    for param in b.k1.mem.getn(fn_params).iter() {
        let v = b.k1.variables.get(param.variable_id);
        let t = b.get_physical_type(v.type_id);

        // We do not skip empty types here, even though they do not appear in the physical function
        // type. This is because they do not need to be passed, but we do need to be able to look
        // them up. Consider a function that takes an empty named t: `t: {}`. It should be legal to
        // use `t`.
        //let phys_param =
        //    b.k1.ir
        //        .mem
        //        .getn(fn_phys_type.params)
        //        .iter()
        //        .find(|p| p.original_index as usize == index);
        let value = if t.is_empty() {
            Value::Empty
        } else {
            let value = Value::FnParam { t, index: non_empty_index as u32 };
            non_empty_index += 1;
            value
        };
        let builder_variable =
            BuilderVariable { id: param.variable_id, value, pt: t, indirect: false };
        b.k1.ir.b_variables.insert(builder_variable.id, builder_variable);
    }

    let f = b.k1.get_function(function_id);
    if let Some(body_block) = f.body_block {
        let entry_block = b.push_block(BlockSourceKind::Entry);
        b.goto_block(entry_block);
        compile_block_stmts(&mut b, None, body_block)?;
    } else {
        match f.linkage {
            Linkage::Standard => panic!("ir: function should have a body I think"),
            Linkage::External { .. } => {}
            Linkage::Intrinsic | Linkage::LlvmIntrinsic(_) => {}
        }
    };

    let unit_id = IrUnitId::Function(function_id);
    let maybe_backend_builtin = match intrinsic_type {
        Some(Builtin::Backend(kind)) => Some(kind),
        Some(_) => None,
        None => None,
    };
    finalize_unit(&mut b, return_type_id, unit_id, phys_fn_type, is_debug, maybe_backend_builtin)?;

    if is_debug {
        let s = unit_to_string(b.k1, unit_id, true);
        eprintln!("{s}");
    }

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_ir_nanos += elapsed as i64;
    Ok(())
}

pub fn compile_top_level_expr(
    k1: &mut TypedProgram,
    expr: TypedExprId,
    input_parameters: &[(VariableId, StaticValueId)],
    is_debug: bool,
) -> K1Result<()> {
    let start = k1.timing.clock.raw();

    let mut b = Builder::new(k1);

    for (variable_id, static_value_id) in input_parameters {
        let variable = b.k1.variables.get(*variable_id);
        let pt = b.get_physical_type(variable.type_id);
        b.k1.ir.b_variables.insert(
            *variable_id,
            BuilderVariable {
                id: *variable_id,
                value: Value::StaticValue { t: pt, id: *static_value_id },
                pt,
                indirect: !pt.is_agg(),
            },
        );
    }

    let return_type_id = b.k1.exprs.get_type(expr);
    let (return_type, diverges) = b.get_function_return_type(return_type_id);
    let params = MSlice::empty();
    let phys_fn_type =
        PhysicalFunctionType { return_type, diverges, params, abi_mode: AbiMode::Internal };
    b.fn_type = phys_fn_type;

    debug!("Compiling expr {}", b.k1.expr_to_string(expr));
    let entry_block = b.push_block(BlockSourceKind::ExprToplevel);
    b.goto_block(entry_block);
    let _result = compile_expr(&mut b, None, expr)?;
    let unit_id = IrUnitId::Expr(expr);
    finalize_unit(&mut b, return_type_id, unit_id, phys_fn_type, is_debug, None)?;

    if is_debug {
        let s = unit_to_string(k1, unit_id, true);
        eprintln!("{s}");
    }

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_ir_nanos += elapsed as i64;
    Ok(())
}

fn finalize_unit(
    b: &mut Builder,
    result_type_id: TypeId,
    unit_id: IrUnitId,
    fn_type: PhysicalFunctionType,
    is_debug: bool,
    builtin_kind: Option<BackendBuiltin>,
) -> K1Result<()> {
    let inst_count =
        b.k1.ir
            .mem
            .dlist_iter(b.blocks)
            .map(|block| b.k1.ir.mem.dlist_compute_len(block.instrs) as u32)
            .sum();

    let unit = IrUnit {
        result_type_id,
        unit_id,
        fn_type,
        inst_count,
        last_alloca_index: b.last_alloca_index,
        blocks: b.blocks,
        function_builtin_kind: builtin_kind,
        is_debug,
        inline_done: false,
        cfg_valid: true,
    };
    match unit_id {
        IrUnitId::Function(function_id) => {
            *b.k1.ir.functions.get_mut(function_id) = Some(unit);
        }
        IrUnitId::Expr(expr) => {
            b.k1.ir.exprs.insert(expr, unit);
        }
    }

    iropt::cfg_compute_unit(&mut b.k1.ir, unit_id);
    if cfg!(debug_assertions) {
        validate_unit(b.k1, unit_id)?;
    }

    b.k1.ir.b_variables.clear();
    b.k1.ir.b_loops.clear();
    Ok(())
}

struct BuilderVariable {
    id: VariableId,
    value: Value,
    pt: PhysicalType,
    indirect: bool,
}

#[derive(Clone)]
struct LoopInfo {
    break_value: Option<InstId>,
    end_block: BlockId,
}

pub type BlockId = NodeHandle<Block, ProgramIr>;
pub type InstNode = DlNode<InstId, ProgramIr>;
// Splits block_node at inst into pre and post, leaving `inst` as the last item in pre.
pub type BlockNode = DlNode<Block, ProgramIr>;

pub struct Builder<'k1> {
    // Dependencies
    k1: &'k1 mut TypedProgram,

    blocks: Dlist<Block, ProgramIr>,
    fn_type: PhysicalFunctionType,

    returned_alloca: Option<InstId>,
    last_alloca_index: Option<u32>,
    cur_block: BlockId,
    cur_span: SpanId,
    // entry_span is the span assigned to the hoisted allocas
    entry_span: SpanId,
}

impl<'k1> Builder<'k1> {
    fn new(k1: &'k1 mut TypedProgram) -> Self {
        Self {
            k1,

            blocks: Dlist::empty(),
            fn_type: PhysicalFunctionType::nil(),

            returned_alloca: None,
            last_alloca_index: None,
            cur_block: Handle::nil(),
            cur_span: SpanId::NONE,
            entry_span: SpanId::NONE,
        }
    }

    pub fn optimize_enabled(&self) -> bool {
        self.k1.config.optimize_ir
    }

    fn make_inst(&mut self, inst: Inst, comment: IrComment, debug_info: IrDebugInfo) -> InstId {
        let span = self.cur_span;
        self.k1.ir.add_inst(inst, comment, debug_info, span)
    }

    fn push_alloca(&mut self, pt: PhysicalType, comment: IrComment) -> InstId {
        self.push_alloca_ext(pt, comment, IrDebugInfo::default(), false)
    }

    fn push_alloca_ext(
        &mut self,
        pt: PhysicalType,
        comment: IrComment,
        debug_info: IrDebugInfo,
        returned: bool,
    ) -> InstId {
        let layout = self.k1.get_pt_layout(pt);
        let index = match self.last_alloca_index {
            None => 0,
            Some(i) => i as usize + 1,
        };
        let alloca_span = self.entry_span;
        let inst_id = self.k1.ir.add_inst(
            Inst::Alloca { t: pt, vm_layout: layout, returned },
            comment,
            debug_info,
            alloca_span,
        );
        let mut first_block = self.k1.ir.mem.get_raw_ref(self.blocks.first);
        self.k1.ir.mem.dlist_insert(&mut first_block.data.instrs, index, inst_id);
        self.last_alloca_index = Some(index as u32);
        inst_id
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.k1.ir, inst)
    }

    pub fn get_value_kind(&self, value: Value) -> InstKind {
        get_value_kind(&self.k1.ir, value)
    }

    #[allow(unused)]
    fn insert_inst_before(&mut self, inst_node: NodeHandle<InstId, ProgramIr>, inst_id: InstId) {
        let blocks = self.k1.ir.mem.get_raw_ref(self.cur_block).as_mut();
        self.k1.ir.mem.dlist_insert_before(&mut blocks.data.instrs, inst_node, inst_id);
    }

    #[allow(unused)]
    fn insert_inst_after(&mut self, inst_node: NodeHandle<InstId, ProgramIr>, inst_id: InstId) {
        let blocks = self.k1.ir.mem.get_raw_ref(self.cur_block).as_mut();
        self.k1.ir.mem.dlist_insert_after(&mut blocks.data.instrs, inst_node, inst_id);
    }

    fn push_inst_front(&mut self, inst: Inst, comment: IrComment) -> InstId {
        let id = self.make_inst(inst, comment, IrDebugInfo::default());

        let blocks = self.k1.ir.mem.get_raw_ref(self.cur_block).as_mut();
        self.k1.ir.mem.dlist_push_front(&mut blocks.data.instrs, id);
        id
    }

    fn push_inst(&mut self, inst: Inst, comment: IrComment) -> InstId {
        let id = self.make_inst(inst, comment, IrDebugInfo::default());

        let blocks = self.k1.ir.mem.get_raw_ref(self.cur_block).as_mut();
        self.k1.ir.mem.dlist_push(&mut blocks.data.instrs, id);
        id
    }

    fn push_inst_anon(&mut self, inst: Inst) -> InstId {
        self.push_inst(inst, IrComment::None)
    }

    fn push_struct_offset(
        &mut self,
        struct_agg_id: AggregateTypeId,
        base: Value,
        field_index: u32,
        comment: IrComment,
    ) -> Value {
        if field_index == 0 {
            return base;
        }
        let Some(offset) = self.k1.get_struct_field_offset(struct_agg_id, field_index) else {
            b_ice!(self, "Failed getting offset for field")
        };
        self.push_inst(
            Inst::StructOffset { struct_t: struct_agg_id, base, field_index, vm_offset: offset },
            comment,
        )
        .as_value()
    }

    fn push_jump(&mut self, block_id: BlockId, comment: IrComment) -> InstId {
        self.push_inst(Inst::Jump(block_id), comment)
    }

    fn push_jump_if(
        &mut self,
        cond: Value,
        cons: BlockId,
        alt: BlockId,
        comment: IrComment,
    ) -> InstId {
        if let Value::Data32 { t: ScalarType::Bool, data: b32 } = cond
            && self.optimize_enabled()
        {
            if b32 == 1 {
                // JMPIF true ...
                self.push_jump(cons, comment)
            } else if b32 == 0 {
                // JMPIF false ...
                self.push_jump(alt, comment)
            } else {
                panic!("Unexpected condition value: {b32}")
            }
        } else {
            self.push_inst(Inst::JumpIf { cond, cons, alt }, comment)
        }
    }

    fn push_copy(
        &mut self,
        dst: Value,
        src: Value,
        pt: PhysicalType,
        comment: IrComment,
    ) -> Option<InstId> {
        let layout = self.k1.get_pt_layout(pt);
        if pt.is_empty() {
            None
        } else {
            let copy_inst =
                self.push_inst(Inst::Copy { dst, src, t: pt, vm_size: layout.size }, comment);
            Some(copy_inst)
        }
    }

    fn push_load(&mut self, st: ScalarType, src: Value, comment: IrComment) -> InstId {
        self.push_inst(Inst::Load { t: st, src }, comment)
    }

    fn push_store(&mut self, dst: Value, value: Value, comment: IrComment) -> InstId {
        let t = self.get_value_kind(value).expect_value().unwrap().expect_scalar();
        self.push_inst(Inst::Store { dst, value, t }, comment)
    }

    fn make_int_value(&mut self, int_value: &TypedIntValue, comment: IrComment) -> Value {
        match int_value {
            TypedIntValue::U8(i) => Value::Data32 { t: ScalarType::U8, data: *i as u32 },
            TypedIntValue::U16(i) => Value::Data32 { t: ScalarType::U16, data: *i as u32 },
            TypedIntValue::U32(i) => Value::Data32 { t: ScalarType::U32, data: *i },
            TypedIntValue::U64(i) => {
                if *i <= u32::MAX as u64 {
                    Value::imm32(ScalarType::U64, *i as u32)
                } else {
                    let inst = self.push_inst(Inst::Data(DataInst::U64(*i)), comment);
                    inst.as_value()
                }
            }
            TypedIntValue::I8(i) => Value::Data32 { t: ScalarType::I8, data: *i as u8 as u32 },
            TypedIntValue::I16(i) => Value::Data32 { t: ScalarType::I16, data: *i as u16 as u32 },
            TypedIntValue::I32(i) => Value::imm32(ScalarType::I32, *i as u32),
            TypedIntValue::I64(i) => {
                if *i >= i32::MIN as i64 && *i <= i32::MAX as i64 {
                    Value::imm32(ScalarType::I64, *i as i32 as u32)
                } else {
                    let inst = self.push_inst(Inst::Data(DataInst::I64(*i)), comment);
                    inst.as_value()
                }
            }
        }
    }

    fn push_block(&mut self, kind: BlockSourceKind) -> BlockId {
        let node = self.k1.ir.mem.dlist_push(&mut self.blocks, Block::empty(kind));
        node
    }

    #[track_caller]
    fn goto_block(&mut self, block_id: BlockId) {
        self.cur_block = block_id;
        #[cfg(debug_assertions)]
        {
            self.k1.ir.mem.get(block_id);
        }
    }

    fn get_variable(&self, variable_id: VariableId) -> Option<&BuilderVariable> {
        self.k1.ir.b_variables.get(&variable_id)
    }

    fn get_physical_type_result(&mut self, type_id: TypeId) -> PhysicalTypeResult {
        self.k1.get_physical_type(type_id)
    }

    fn get_physical_type(&mut self, type_id: TypeId) -> PhysicalType {
        match self.get_physical_type_result(type_id) {
            PhysicalTypeResult::Never => {
                b_ice!(self, "ir never type: {}", self.k1.type_id_to_string_ext(type_id, true))
            }
            PhysicalTypeResult::No => {
                b_ice!(
                    self,
                    "ir non-physical type: {}",
                    self.k1.type_id_to_string_ext(type_id, true)
                )
            }
            PhysicalTypeResult::Infinite => {
                b_ice!(self, "ir infinite type: {}", self.k1.type_id_to_string_ext(type_id, true))
            }
            PhysicalTypeResult::Yes(pt) => pt,
        }
    }

    fn type_to_inst_kind(&mut self, type_id: TypeId) -> InstKind {
        if type_id == NEVER_TYPE_ID {
            InstKind::Terminator
        } else {
            let t = self.get_physical_type(type_id);
            InstKind::Value(t)
        }
    }

    fn get_physical_fn_type(&mut self, type_id: TypeId) -> PhysicalFunctionType {
        if let Some(pt) = self.k1.ir.phys_fn_type_cache.get(&type_id) {
            return *pt;
        }
        let function_type = *self.k1.types.get(type_id).expect_function();
        let (return_type, diverges) = self.get_function_return_type(function_type.return_type);

        let mut phys_params = self.k1.ir.mem.new_list(function_type.physical_params.len());
        for (index, param) in self.k1.mem.getn(function_type.physical_params).iter().enumerate() {
            let pt = self.get_physical_type(param.type_id);
            if pt.is_empty() {
                continue;
            }
            if index >= u16::MAX as usize {
                b_ice!(self, "Too many parameters; max is {}", u16::MAX);
            }
            phys_params.push(PhysicalFunctionParam { original_index: Some(index as u16), pt })
        }
        let fn_ty = PhysicalFunctionType {
            params: phys_params.to_slice(),
            diverges,
            return_type,
            abi_mode: function_type.abi_mode,
        };

        self.k1.ir.phys_fn_type_cache.insert(type_id, fn_ty);
        fn_ty
    }

    // Returns: (the function return type, diverges)
    fn get_function_return_type(&mut self, return_type_id: TypeId) -> (PhysicalType, bool) {
        if return_type_id == NEVER_TYPE_ID {
            (PhysicalType::EMPTY, true)
        } else {
            let t = self.get_physical_type(return_type_id);
            (t, false)
        }
    }

    fn _get_instr_block(&self, inst_id: InstId) -> IrHandle<BlockNode> {
        self.k1
            .ir
            .mem
            .dlist_iter_handles(self.blocks)
            .find(|(_h, b)| self.k1.ir.mem.dlist_iter(b.data.instrs).any(|i| *i == inst_id))
            .unwrap()
            .0
    }

    fn _locate_inst(&self, inst_id: InstId) -> (BlockId, Handle<InstNode, ProgramIr>, usize) {
        for (block_handle, block) in self.k1.ir.mem.dlist_iter_handles(self.blocks) {
            for (index, (inst_handle, inst)) in
                self.k1.ir.mem.dlist_iter_handles(block.data.instrs).enumerate()
            {
                if inst.data == inst_id {
                    return (block_handle, inst_handle, index);
                }
            }
        }
        panic!("inst {} not found", inst_id.as_u32())
    }

    fn split_block_at_inst(
        &mut self,
        block_node: BlockId,
        inst_node: IrHandle<InstNode>,
    ) -> Handle<BlockNode, ProgramIr> {
        let mut block_ref = self.k1.ir.mem.get_raw_ref(block_node);
        let kind = block_ref.data.kind;
        let after_insts = self.k1.ir.mem.dlist_split_at_node(&mut block_ref.data.instrs, inst_node);
        let after_block = self.k1.ir.mem.dlist_insert_after(
            &mut self.blocks,
            block_node,
            Block { kind, instrs: after_insts, preds: Dlist::empty(), succs: Dlist::empty() },
        );
        after_block
    }
}

fn store_scalar_if_dst(b: &mut Builder, dst: Option<Value>, value: Value) -> Value {
    match dst {
        None => value,
        Some(dst) => {
            b.push_store(dst, value, IrComment::StoreScalarToDst);
            dst
        }
    }
}

fn store_rich_if_dst(
    b: &mut Builder,
    dst: Option<Value>,
    pt: PhysicalType,
    value: Value,
    comment: IrComment,
) -> Value {
    match dst {
        None => {
            if pt.is_empty() {
                Value::Empty
            } else {
                value
            }
        }
        Some(dst) => {
            store_value(b, pt, dst, value, comment);
            dst
        }
    }
}

fn compile_block_stmts(
    b: &mut Builder,
    dst: Option<Value>,
    body: TypedExprId,
) -> K1Result<Option<Value>> {
    let TypedExpr::Block(body) = b.k1.exprs.get(body) else {
        kbail!(b.k1, b.cur_span, "body is not a block");
    };
    debug!("compiling block {}", b.k1.block_to_string(body));

    let mut last_ret = None;
    let statements = body.statements;
    for (index, &stmt) in b.k1.mem.getn(statements).iter().enumerate() {
        let is_last = index == statements.len() as usize - 1;
        let stmt_dst = if is_last { dst } else { None };
        last_ret = Some(compile_stmt(b, stmt_dst, stmt)?);
    }

    Ok(last_ret)
}

fn compile_stmt(b: &mut Builder, dst: Option<Value>, stmt: TypedStmtId) -> K1Result<Value> {
    debug!("compiling stmt {}", b.k1.stmt_to_string(stmt));
    let prev_span = b.cur_span;
    let stmt_span = b.k1.get_stmt_span(stmt);
    b.cur_span = stmt_span;
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);

    match b.k1.stmts.get(stmt) {
        TypedStmt::Expr(typed_expr_id, _) => {
            let typed_expr_id = *typed_expr_id;
            let v = compile_expr(b, dst, typed_expr_id)?;
            Ok(v)
        }
        TypedStmt::Let(let_stmt) => {
            let let_stmt = *let_stmt;

            let rich_type_id = let_stmt.variable_type;
            let var_pt = b.get_physical_type(let_stmt.variable_type);
            let rich_pt = b.get_physical_type(rich_type_id);

            let typed_var = b.k1.variables.get(let_stmt.variable_id);
            let returned = typed_var.is_returned();
            let debug_info = IrDebugInfo {
                variable_info: Some(IrDebugVariableInfo {
                    name: typed_var.name,
                    original_type_id: let_stmt.variable_type,
                    user_hidden: typed_var.is_user_hidden(),
                    source_span: b.cur_span,
                }),
            };

            if rich_pt.is_empty() {
                //let span = b.cur_span;
                if let Some(init) = let_stmt.initializer {
                    compile_expr(b, None, init)?;
                }
                b.k1.ir.b_variables.insert(
                    let_stmt.variable_id,
                    BuilderVariable {
                        id: let_stmt.variable_id,
                        value: Value::Empty,
                        pt: rich_pt,
                        indirect: false,
                    },
                );
                Ok(Value::Empty)
            } else {
                let variable_alloca =
                    b.push_alloca_ext(rich_pt, IrComment::SourceLet, debug_info, returned);

                if let Some(init) = let_stmt.initializer {
                    compile_expr(b, Some(variable_alloca.as_value()), init)?;
                }
                // Since aggregate values are represented by their address in our IR
                // they are considered 'direct' variables, meaning we don't have to
                // generate a load when they are used.
                //
                // Scalars however are represented as values; and the variable is a place,
                // an address, so we consider it an 'indirect' representation of that scalar value,
                // for example an i32.
                let is_direct = rich_pt.is_agg();
                b.k1.ir.b_variables.insert(
                    let_stmt.variable_id,
                    BuilderVariable {
                        id: let_stmt.variable_id,
                        value: variable_alloca.as_value(),
                        pt: var_pt,
                        indirect: !is_direct,
                    },
                );
                Ok(Value::Empty)
            }
        }
        TypedStmt::Assignment(ass) => {
            let ass = *ass;
            match ass.kind {
                AssignmentKind::Set => {
                    let TypedExpr::Variable(v) = b.k1.exprs.get(ass.destination) else {
                        b.k1.ice_span(ass.span, "Invalid value assignment lhs")
                    };
                    let variable_id = v.variable_id;
                    let CompileVariableResult::Address { addr, constant, .. } =
                        compile_variable_to_address(b, variable_id, true)
                    else {
                        unreachable!()
                    };
                    debug_assert!(!constant);
                    let _rhs_stored = compile_expr(b, Some(addr), ass.value)?;
                    Ok(Value::Empty)
                }
                AssignmentKind::Store => {
                    let lhs = compile_expr(b, None, ass.destination)?;
                    let _rhs_stored = compile_expr(b, Some(lhs), ass.value)?;
                    Ok(Value::Empty)
                }
            }
        }
        TypedStmt::Require(req) => {
            let req = req.clone();
            let require_continue_block = b.push_block(BlockSourceKind::RequireContinue);
            let require_else_block = match req.else_body {
                None => None,
                Some(_) => Some(b.push_block(BlockSourceKind::RequireElse)),
            };

            compile_matching_condition(
                b,
                &req.condition,
                require_continue_block,
                require_else_block,
            )?;

            if let Some(else_body) = req.else_body {
                b.goto_block(require_else_block.unwrap());
                compile_expr(b, None, else_body)?;
            }

            b.goto_block(require_continue_block);

            Ok(Value::Empty)
        }
        TypedStmt::Defer(_) => {
            // These defers are just vestiges of the source; nothing to emit
            Ok(Value::Empty)
        }
    }
}

fn compile_expr(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<Value>,
    expr: TypedExprId,
) -> K1Result<Value> {
    let prev_span = b.cur_span;
    let expr_span = b.k1.exprs.get_span(expr);
    b.cur_span = expr_span;
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);
    let e = b.k1.exprs.get(expr).clone();
    let expr_type = b.k1.exprs.get_type(expr);
    debug!("compiling {} {}", e.kind_name(), b.k1.expr_to_string(expr));
    match e {
        TypedExpr::Struct(struct_literal) => {
            let struct_type_id = expr_type;
            let struct_pt = b.get_physical_type(struct_type_id);
            if struct_pt.is_empty() {
                return Ok(Value::Empty);
            }
            let struct_agg_id = struct_pt.expect_agg();
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.push_alloca(struct_pt, IrComment::StructLiteral).as_value(),
            };
            for (field_index, field) in b.k1.mem.getn(struct_literal.fields).iter().enumerate() {
                match field.expr {
                    None => {
                        // uninitialized field
                    }
                    Some(expr) => {
                        let struct_offset = b.push_struct_offset(
                            struct_agg_id,
                            struct_base,
                            field_index as u32,
                            IrComment::StructLitFieldPtr,
                        );
                        compile_expr(b, Some(struct_offset), expr)?;
                    }
                }
            }
            Ok(struct_base)
        }
        TypedExpr::StructFieldAccess(_) => {
            let (field_ptr, frozen) = compile_expr_place(b, expr)?;
            let result_type = b.get_physical_type(expr_type);
            let needs_copy = !frozen;
            let result = build_field_access(b, dst, field_ptr, result_type, needs_copy);
            Ok(result)
        }
        TypedExpr::ArrayGetElement(_) => {
            let (element_ptr, frozen) = compile_expr_place(b, expr)?;
            let result_type = b.get_physical_type(expr_type);
            let needs_copy = !frozen;
            let result = build_field_access(b, dst, element_ptr, result_type, needs_copy);
            Ok(result)
        }
        TypedExpr::Variable(variable_expr) => {
            let var_result = compile_variable_to_address(b, variable_expr.variable_id, false);
            match var_result {
                CompileVariableResult::FoldedValue { value, pt } => {
                    let stored = store_rich_if_dst(b, dst, pt, value, IrComment::FoldedVariable);
                    Ok(stored)
                }
                CompileVariableResult::Address { addr, pt, indirect, constant } => {
                    if indirect {
                        debug_assert!(!pt.is_agg());
                        let copy_aggregates = !constant;
                        let loaded_or_copied = load_or_copy(
                            b,
                            pt,
                            dst,
                            addr,
                            copy_aggregates,
                            IrComment::FulfillVariableUsage,
                        );
                        Ok(loaded_or_copied)
                    } else {
                        // direct; var_value is already a canonical representation of the value; just have to
                        // fulfill dst
                        let stored = store_rich_if_dst(b, dst, pt, addr, IrComment::DirectVariable);
                        Ok(stored)
                    }
                }
            }
        }
        TypedExpr::AddressOf(address_of) => {
            let (place, _frozen) = compile_expr_place(b, address_of.target_expr)?;

            let stored = store_scalar_if_dst(b, dst, place);
            Ok(stored)
        }
        TypedExpr::Deref(_) => {
            let (src, frozen) = compile_expr_place(b, expr)?;
            let target_pt = b.get_physical_type(expr_type);
            let copy_aggregates = !frozen;
            let loaded = load_or_copy(
                b,
                target_pt,
                dst,
                src,
                copy_aggregates,
                if dst.is_some() {
                    IrComment::LangDerefFulfillToDst
                } else {
                    IrComment::LangDerefNoDst
                },
            );
            Ok(loaded)
        }
        TypedExpr::Block(_) => {
            let last = compile_block_stmts(b, dst, expr)?;
            let block_value = match last {
                None => Value::Empty,
                Some(last) => last,
            };
            Ok(block_value)
        }
        TypedExpr::Call { call_id } => {
            let call = b.k1.calls.get(call_id).clone();

            let function_type_id = b.k1.get_callee_function_type(&call.callee);
            let callee_fn_type = b.get_physical_fn_type(function_type_id);

            let maybe_function_id = call.callee.maybe_function_id();
            let (maybe_builtin, linkage) = match maybe_function_id {
                None => (None, None),
                Some(f_id) => {
                    let f = b.k1.get_function(f_id);
                    (f.builtin_type, Some(f.linkage))
                }
            };

            let callee: IrCallee;
            let mut environment_arg: Option<Value> = None;

            if let Some(Linkage::External { lib_name, fn_name, .. }) = linkage {
                let function_id = maybe_function_id.unwrap();
                let function_name = match fn_name {
                    None => b.k1.get_function(function_id).name,
                    Some(fn_name) => fn_name,
                };
                callee = IrCallee::Extern { library_name: lib_name, function_name, function_id };
            } else if let Some(builtin) = maybe_builtin {
                match builtin {
                    Builtin::Ir(ir_builtin) => {
                        return compile_ir_builtin(b, call, ir_builtin, callee_fn_type, dst);
                    }
                    Builtin::Backend(backend_builtin) => {
                        let function_id = maybe_function_id.unwrap();
                        callee = IrCallee::BackendBuiltin(function_id, backend_builtin)
                    }
                    Builtin::LlvmIntrinsic(name) => {
                        let function_id = maybe_function_id.unwrap();
                        callee = IrCallee::LlvmIntrinsic { name, function_id }
                    }
                    Builtin::TyperPhysicalFunction(_) => {
                        let function_id = maybe_function_id.unwrap();
                        callee = IrCallee::Direct(function_id)
                    }
                    Builtin::TyperInline(_) => unreachable!(),
                }
            } else {
                match &call.callee {
                    Callee::StaticFunction(function_id) => callee = IrCallee::Direct(*function_id),
                    Callee::StaticLambda { function_id, lambda_value_expr, .. } => {
                        // The body takes its env by pointer; spill the by-value env
                        let lambda_env = compile_expr(b, None, *lambda_value_expr)?;
                        let lambda_env_type_id = b.k1.exprs.get_type(*lambda_value_expr);
                        let env_pt = b.get_physical_type(lambda_env_type_id);
                        let env_ptr =
                            b.push_alloca(env_pt, IrComment::LambdaEnvLocation).as_value();
                        store_value(
                            b,
                            env_pt,
                            env_ptr,
                            lambda_env,
                            IrComment::StoreLambdaEnvForCall,
                        );
                        callee = IrCallee::Direct(*function_id);
                        environment_arg = Some(env_ptr);
                    }
                    Callee::Abstract { .. } => {
                        kbail!(b.k1, b.cur_span, "ir abstract callee");
                    }
                    Callee::Builtin { builtin, .. } => {
                        kbail!(b.k1, b.cur_span, "ir builtin callee: {}", builtin.kind_name());
                    }
                    Callee::DynamicLambda(dl) => {
                        let lambda_obj = compile_expr(b, None, *dl)?;
                        let lam_obj_type_id = b.k1.builtin_types.dyn_lambda_obj.unwrap();
                        let lam_obj_pt = b.get_physical_type(lam_obj_type_id).expect_agg();
                        let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
                        let fn_ptr_addr = b.push_struct_offset(
                            lam_obj_pt,
                            lambda_obj,
                            TypedProgram::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
                            IrComment::DynLamFnPtrOffset,
                        );
                        let fn_ptr = load_value(b, ptr_pt, fn_ptr_addr, false, IrComment::None);
                        let env_addr = b.push_struct_offset(
                            lam_obj_pt,
                            lambda_obj,
                            TypedProgram::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
                            IrComment::DynLamEnvPtrOffset,
                        );
                        let env = load_value(b, ptr_pt, env_addr, false, IrComment::None);

                        callee = IrCallee::Indirect(callee_fn_type, fn_ptr);
                        environment_arg = Some(env);
                    }
                    Callee::DynamicAbilityFn { object_expr, field_index, slot_function_type } => {
                        let object = compile_expr(b, None, *object_expr)?;
                        let object_type_id = b.k1.exprs.get_type(*object_expr);
                        let object_pt = b.get_physical_type(object_type_id).expect_agg();
                        let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
                        let fn_ptr_addr = b.push_struct_offset(
                            object_pt,
                            object,
                            *field_index,
                            IrComment::DynAbilityFnPtrOffset,
                        );
                        let fn_ptr = load_value(b, ptr_pt, fn_ptr_addr, false, IrComment::None);
                        callee = IrCallee::Indirect(callee_fn_type, fn_ptr);

                        // Receiver-shape slots take self via the state pointer as
                        // physical arg 0; no-self slots use the object as a type
                        // witness only and pass no state
                        let takes_state =
                            b.k1.types.get(*slot_function_type).as_function().unwrap().is_lambda;
                        if takes_state {
                            let state_addr = b.push_struct_offset(
                                object_pt,
                                object,
                                TypedProgram::ABILITY_OBJECT_STATE_INDEX as u32,
                                IrComment::DynAbilityStateOffset,
                            );
                            let state = load_value(b, ptr_pt, state_addr, false, IrComment::None);
                            environment_arg = Some(state);
                        }
                    }
                    Callee::DynamicFunction { function_pointer_expr } => {
                        let callee_inst = compile_expr(b, None, *function_pointer_expr)?;
                        callee = IrCallee::Indirect(callee_fn_type, callee_inst);
                    }
                    Callee::DynamicAbstract { .. } => {
                        kbail!(b.k1, b.cur_span, "ir abstract call");
                    }
                }
            }

            // Add function to compile queue
            if let Some(function_id) = callee.known_function_id() {
                match b.k1.ir.functions.get(function_id) {
                    None => {
                        b.k1.ir.units_pending_compile.entry(function_id).or_insert(());
                    }
                    Some(_unit) => {}
                }
            }

            let mut args =
                b.k1.ir.mem.new_list(call.args.len() + environment_arg.iter().count() as u32);

            if let Some(environment_arg) = environment_arg {
                args.push(environment_arg)
            }

            for (original_index, arg) in b.k1.mem.getn(call.args).iter().enumerate() {
                // Each arg could be of an `empty` type, and if so it will not be passed to the
                // function.
                // But in this case we still need to compile this expression!
                let value = compile_expr(b, None, *arg)?;

                // For this non-physical argument, find the corresponding physical parameter
                // If there is not one, don't push an argument
                let phys_param =
                    b.k1.ir
                        .mem
                        .getn(callee_fn_type.params)
                        .iter()
                        .find(|p| p.original_index == Some(original_index as u16));

                // But only if its not a ZST do we put it in the call's arguments
                if let Some(_phys_param) = phys_param {
                    args.push(value);
                }
            }
            debug_assert_eq!(callee_fn_type.params.len(), args.len() as u32);
            let args_handle = args.to_slice();
            let call_id = b.k1.ir.calls.add(IrCall {
                ret_type: callee_fn_type.return_type,
                callee,
                args: args_handle,
                dst,
            });
            let call_inst = Inst::Call { call_id };
            let call_inst_id = b.push_inst_anon(call_inst);
            let value_for_call = {
                if callee_fn_type.diverges {
                    let unreachable = b.push_inst_anon(Inst::Unreachable);
                    unreachable.as_value()
                } else {
                    dst.unwrap_or(call_inst_id.as_value())
                }
            };
            Ok(value_for_call)
        }
        TypedExpr::Match(match_expr) => {
            let match_result_type = expr_type;
            let result_inst_kind = b.type_to_inst_kind(match_result_type);
            for stmt in b.k1.mem.getn(match_expr.initial_let_statements) {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.k1.ir.mem.new_list(match_expr.arms.len());
            for _arm in b.k1.mem.getn(match_expr.arms).iter() {
                let arm_block = b.push_block(BlockSourceKind::ArmCond);
                let arm_consequent_block = b.push_block(BlockSourceKind::ArmCons);
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_jump(first_arm_block, IrComment::EnterMatch);

            let fail_block = b.push_block(BlockSourceKind::MatchFail);
            b.goto_block(fail_block);
            b.push_inst_anon(Inst::Unreachable);

            let match_end_block = b.push_block(BlockSourceKind::MatchEnd);

            enum MatchDst {
                Phi(List<PhiCase, ProgramIr>),
                CallerDst(Value),
            }
            let mut result_value: MatchDst = match dst {
                None => MatchDst::Phi(b.k1.ir.mem.new_list(match_expr.arms.len())),
                Some(dst) => MatchDst::CallerDst(dst),
            };
            for ((index, arm), (arm_block, arm_cons_block)) in
                b.k1.mem.getn(match_expr.arms).iter().enumerate().zip(arm_blocks.iter())
            {
                let next_arm = arm_blocks.get(index + 1);
                let next_arm_or_fail: BlockId = match next_arm {
                    None => fail_block,
                    Some((next_arm_block, _)) => *next_arm_block,
                };

                // For each arm, we compile its matching condition which requires 2 inputs:
                // A jump target if the conditions succeed, and a jump target if the conditions
                // fail
                b.goto_block(*arm_block);
                compile_matching_condition(
                    b,
                    &arm.condition,
                    *arm_cons_block,
                    Some(next_arm_or_fail),
                )?;

                b.goto_block(*arm_cons_block);
                let arm_result = compile_expr(b, None, arm.consequent_expr)?;
                let cons_diverges = b.get_value_kind(arm_result).is_terminator();
                debug_assert_eq!(
                    b.k1.exprs.get_type(arm.consequent_expr) == NEVER_TYPE_ID,
                    cons_diverges
                );

                if !cons_diverges {
                    let current_block = b.cur_block;
                    match &mut result_value {
                        MatchDst::Phi(incomings) => {
                            incomings.push(PhiCase { from: current_block, value: arm_result })
                        }
                        MatchDst::CallerDst(dst) => {
                            let pt = result_inst_kind.expect_value().unwrap();
                            store_value(b, pt, *dst, arm_result, IrComment::MatchArmResultStore);
                        }
                    };
                    b.push_jump(match_end_block, IrComment::None);
                }
            }

            b.goto_block(match_end_block);
            match result_inst_kind {
                InstKind::Value(pt) => {
                    if pt.is_empty() {
                        Ok(Value::Empty)
                    } else {
                        match result_value {
                            MatchDst::Phi(incomings) => {
                                let value = if incomings.len() == 1 && b.optimize_enabled() {
                                    incomings[0].value
                                } else {
                                    let incomings_handle = incomings.to_slice();
                                    let phi_inst = b.push_inst(
                                        Inst::Phi { t: pt, incomings: incomings_handle },
                                        IrComment::MatchPhi,
                                    );
                                    phi_inst.as_value()
                                };
                                debug_assert!(dst.is_none());
                                let fulfilled = store_rich_if_dst(
                                    b,
                                    dst,
                                    pt,
                                    value,
                                    IrComment::FulfillMatchDst,
                                );
                                Ok(fulfilled)
                            }
                            MatchDst::CallerDst(dst) => Ok(dst),
                        }
                    }
                }
                InstKind::Void => Err(kerr!(b.k1, b.cur_span, "match result void")),
                InstKind::Terminator => {
                    // match is divergent
                    let inst = b.push_inst(Inst::Unreachable, IrComment::DivergentMatch);
                    Ok(inst.as_value())
                }
            }
        }
        TypedExpr::WhileLoop(w) => {
            let cond_block = b.push_block(BlockSourceKind::WhileLoopCondition);
            let loop_body_block = b.push_block(BlockSourceKind::WhileLoopBody);
            let end_block = b.push_block(BlockSourceKind::WhileLoopEnd);
            let TypedExpr::Block(body_block) = b.k1.exprs.get(w.body) else { unreachable!() };
            let loop_scope_id = body_block.scope_id;
            b.k1.ir.b_loops.insert(loop_scope_id, LoopInfo { break_value: None, end_block });

            b.push_jump(cond_block, IrComment::EnterWhileCond);

            b.goto_block(cond_block);
            compile_matching_condition(b, &w.condition, loop_body_block, Some(end_block))?;

            b.goto_block(loop_body_block);
            let last = compile_block_stmts(b, None, w.body)?;
            if last.is_some_and(|v| !b.get_value_kind(v).is_terminator()) {
                b.push_jump(cond_block, IrComment::GotoWhileCond);
            }

            b.goto_block(end_block);
            Ok(Value::Empty)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            let loop_body_block = b.push_block(BlockSourceKind::LoopBody);
            let loop_end_block = b.push_block(BlockSourceKind::LoopEnd);

            let break_pt_id = b.get_physical_type(expr_type);

            let break_value = if expr_type != b.k1.builtin_types.empty {
                Some(b.push_alloca(break_pt_id, IrComment::LoopBreakValue))
            } else {
                None
            };
            let TypedExpr::Block(body_block) = b.k1.exprs.get(loop_expr.body_block) else {
                unreachable!()
            };
            let body_scope_id = body_block.scope_id;
            b.k1.ir
                .b_loops
                .insert(body_scope_id, LoopInfo { break_value, end_block: loop_end_block });

            // Go to the body
            b.push_jump(loop_body_block, IrComment::EnterLoop);
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if body_value.is_some_and(|v| !b.get_value_kind(v).is_terminator()) {
                b.push_jump(loop_body_block, IrComment::DaCapoMaestro);
            }

            b.goto_block(loop_end_block);
            if let Some(break_alloca) = break_value {
                let stored = load_or_copy(
                    b,
                    break_pt_id,
                    dst,
                    break_alloca.as_value(),
                    false,
                    IrComment::FulfillLoopBreakDst,
                );
                Ok(stored)
            } else {
                Ok(Value::Empty)
            }
        }
        TypedExpr::Break(brk) => {
            let loop_info = b.k1.ir.b_loops.get(&brk.loop_scope).unwrap();
            let end_block = loop_info.end_block;
            if let Some(break_dst) = loop_info.break_value {
                let _stored = compile_expr(b, Some(break_dst.as_value()), brk.value)?;
                let jmp = b.push_jump(end_block, IrComment::BreakLoopWithValue);
                Ok(jmp.as_value())
            } else {
                compile_expr(b, None, brk.value)?;
                let jmp = b.push_jump(end_block, IrComment::BreakLoopNoValue);
                Ok(jmp.as_value())
            }
        }
        TypedExpr::SumConstructor(sum_c) => {
            let sum_pt = b.get_physical_type(expr_type);
            let sum_agg_id = sum_pt.expect_agg();
            let sum_pt_agg = b.k1.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let variants = sum_pt_agg.variants;
            let sum_struct_repr = sum_pt_agg.struct_repr;
            let sum_base = match dst {
                Some(dst) => dst,
                None => b.push_alloca(sum_pt, IrComment::SumLiteralStorage).as_value(),
            };

            let tag_base = sum_base;
            let sum_variant = b.k1.mem.get_nth(variants, sum_c.variant_index as usize);
            let tag_int_value = sum_variant.tag;
            let int_imm = b.make_int_value(&tag_int_value, IrComment::SumTag);
            b.push_store(tag_base, int_imm, IrComment::StoreSumLitTag);

            if let Some(payload_expr) = &sum_c.payload {
                let payload_offset =
                    b.push_struct_offset(sum_struct_repr, sum_base, 1, IrComment::SumPayloadPtr);
                let _payload_value = compile_expr(b, Some(payload_offset), *payload_expr)?;
            }

            Ok(sum_base)
        }
        TypedExpr::SumGetTag(sum_get_tag) => {
            let (sum_base, _frozen) = compile_expr_place(b, sum_get_tag.sum_expr)?;
            let sum_type = b.k1.get_expr_type(sum_get_tag.sum_expr).expect_sum();
            let tag_scalar = PhysicalType::scalar(sum_type.tag_type.get_scalar_type());

            // Load straight from the sum base, dont bother with a struct gep
            Ok(load_or_copy(
                b,
                tag_scalar,
                dst,
                sum_base,
                false,
                IrComment::GetSumTagLoadOrCopyToDst,
            ))
        }
        TypedExpr::SumGetPayload(_sum_get_payload) => {
            let (payload_place, frozen) = compile_expr_place(b, expr)?;
            let result_type = b.get_physical_type(expr_type);
            let make_copy = !frozen;
            let copied = load_or_copy(
                b,
                result_type,
                dst,
                payload_place,
                make_copy,
                IrComment::DeliverSumPayload,
            );
            Ok(copied)
        }
        TypedExpr::Enum(e) => {
            // Just compile to the integer
            let Type::Enum(enum_type) = b.k1.types.get(expr_type) else { unreachable!() };
            let value = b.k1.mem.get_nth(enum_type.member_values, e.value_index as usize);
            let value = b.make_int_value(&value.int_value, IrComment::EnumInt);
            let stored = store_scalar_if_dst(b, dst, value);
            Ok(stored)
        }
        TypedExpr::EnumGetValue(get_value) => {
            let value = compile_expr(b, dst, get_value.enum_expr)?;
            Ok(value)
        }
        TypedExpr::Cast(c) => compile_cast(b, dst, &c, expr),
        TypedExpr::Return(typed_return) => {
            debug_assert!(dst.is_none());
            let return_pt = b.fn_type.return_type;
            let dst = match typed_return.returned_variable {
                None if return_pt.is_agg() => match b.returned_alloca {
                    Some(inst_id) => Some(inst_id.as_value()),
                    None => {
                        let rvo_storage = b.push_alloca_ext(
                            return_pt,
                            IrComment::RvoStorage,
                            IrDebugInfo::default(),
                            true,
                        );
                        b.returned_alloca = Some(rvo_storage);
                        Some(rvo_storage.as_value())
                    }
                },
                _ => None,
            };
            let value = compile_expr(b, dst, typed_return.value)?;
            let is_agg_return = b.fn_type.return_type.is_agg();

            // kills a dependency on an empty value
            let returned_value = if return_pt.is_empty() { Value::Empty } else { value };
            let ret = b.push_inst(
                Inst::Ret { v: returned_value, agg: is_agg_return },
                if is_agg_return { IrComment::ReturnAggregateAtAddress } else { IrComment::None },
            );
            Ok(ret.as_value())
        }
        TypedExpr::Lambda(lam_expr) => {
            let lambda_type_id = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            let l = b.k1.lambda_types.get(lambda_type_id);
            let function_id = l.function_id;
            let env_struct = l.environment_struct;
            b.k1.ir.units_pending_compile.insert(function_id, ());
            compile_expr(b, dst, env_struct)
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = Value::FunctionAddr(fpe.function_id);
            let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
            let stored = store_rich_if_dst(b, dst, ptr_pt, fp, IrComment::DeliverFnPointer);
            b.k1.ir.units_pending_compile.insert(fpe.function_id, ());
            Ok(stored)
        }
        TypedExpr::StaticValue(stat) => {
            let t = b.get_physical_type(expr_type);
            let value = compile_static_value(b, stat.value_id, t);
            let stored = store_rich_if_dst(b, dst, t, value, IrComment::StoreStaticValueToDst);
            Ok(stored)
        }
    }
}

fn compile_expr_place(b: &mut Builder, expr: TypedExprId) -> K1Result<(Value, bool)> {
    match b.k1.exprs.get(expr).clone() {
        TypedExpr::StructFieldAccess(field_access) => {
            let struct_type = b.k1.exprs.get_type(field_access.base_struct);
            let struct_pt_id = b.get_physical_type(struct_type).expect_agg();
            let (base_ptr, frozen) = compile_expr_place(b, field_access.base_struct)?;
            let field_ptr = b.push_struct_offset(
                struct_pt_id,
                base_ptr,
                field_access.field_index,
                IrComment::StructAccessPlace,
            );
            Ok((field_ptr, frozen))
        }
        TypedExpr::ArrayGetElement(array_get) => {
            let (array_base, frozen) = compile_expr_place(b, array_get.base_array)?;
            let array_type = b.k1.exprs.get_type(array_get.base_array);
            let array_agg_id = b.get_physical_type(array_type).expect_agg();
            let (element_pt, _len) = b.k1.agg_types.get(array_agg_id).agg_type.expect_array();
            let index = compile_expr(b, None, array_get.index)?;
            let element_ptr = b.push_inst(
                Inst::ArrayOffset { element_t: element_pt, base: array_base, element_index: index },
                IrComment::ArrayGetOffsetPlace,
            );
            Ok((element_ptr.as_value(), frozen))
        }
        TypedExpr::Variable(variable_expr) => {
            let CompileVariableResult::Address { addr, constant, .. } =
                compile_variable_to_address(b, variable_expr.variable_id, true)
            else {
                panic!("require_address not honored")
            };
            let frozen = constant;
            Ok((addr, frozen))
        }
        TypedExpr::Deref(deref_expr) => {
            let value_of_p = compile_expr(b, None, deref_expr.target)?;
            Ok((value_of_p, false))
        }
        TypedExpr::Block(block) => {
            // Blocks are place-transparent in their trailing expression: compile the
            // leading statements for effect, then the trailing expr as a place
            let statements = block.statements;
            let Some((&last, leading)) = b.k1.mem.getn(statements).split_last() else {
                b_ice!(b, "Empty block is not a place");
            };
            for &stmt in leading {
                compile_stmt(b, None, stmt)?;
            }
            let TypedStmt::Expr(trailing_expr, _) = *b.k1.stmts.get(last) else {
                b_ice!(b, "Block whose last statement is not an expression is not a place");
            };
            compile_expr_place(b, trailing_expr)
        }
        TypedExpr::SumGetPayload(sum_get_payload) => {
            let (sum_base, frozen) = compile_expr_place(b, sum_get_payload.sum_expr)?;
            let sum_type_id = b.k1.exprs.get_type(sum_get_payload.sum_expr);
            let sum_agg_id = b.k1.get_physical_type(sum_type_id).unwrap().expect_agg();
            let sum_pt = b.k1.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let sum_struct_repr = sum_pt.struct_repr;
            let payload_offset =
                b.push_struct_offset(sum_struct_repr, sum_base, 1, IrComment::SumPayloadOffset);
            Ok((payload_offset, frozen))
        }
        TypedExpr::AddressOf(_address_of_expr) => {
            b_ice!(b, "AddressOf is not a place expression; it produces an address, not a place");
        }
        _ => {
            let e = compile_expr(b, None, expr)?;
            debug_assert!(b.get_value_kind(e).is_storage() || b.get_value_kind(e).is_empty());
            Ok((e, true))
        }
    }
}

fn compile_static_value(b: &mut Builder, value_id: StaticValueId, pt: PhysicalType) -> Value {
    // We lower the simple static values
    // but leave the aggregates as globals
    match b.k1.static_values.get(value_id) {
        StaticValue::Empty(_) => Value::Empty,
        StaticValue::Bool(bv) => Value::imm32(ScalarType::Bool, *bv as u32),
        StaticValue::Char(byte) => Value::imm32(ScalarType::Char, *byte as u32),
        StaticValue::Int(int) => {
            let int = *int;
            let int_value = b.make_int_value(&int, IrComment::StaticInt);
            int_value
        }
        StaticValue::Enum(_, int) => {
            let int = *int;
            let int_value = b.make_int_value(&int, IrComment::StaticEnum);
            int_value
        }
        StaticValue::Float(float) => {
            let float = *float;
            //task(ir): Pack small floats
            let imm = b.push_inst(Inst::Data(DataInst::Float(float)), IrComment::StaticFloat);
            imm.as_value()
        }
        StaticValue::String(_)
        | StaticValue::Zero(_)
        | StaticValue::Struct(_)
        | StaticValue::Sum(_)
        | StaticValue::LinearContainer(_) => {
            let value = Value::StaticValue { t: pt, id: value_id };
            value
        }
    }
}

enum CompileVariableResult {
    Address { addr: Value, pt: PhysicalType, indirect: bool, constant: bool },
    FoldedValue { value: Value, pt: PhysicalType },
}
fn compile_variable_to_address(
    b: &mut Builder,
    variable_id: VariableId,
    // Don't fold to the value; the caller wants the address explicitly
    require_address: bool,
) -> CompileVariableResult {
    let variable = b.k1.variables.get(variable_id);
    match variable.global_id() {
        Some(global_id) => {
            let global = b.k1.globals.get(global_id).clone();
            if global.initial_value.is_pending() {
                // We'll need to compile this global's body before we can execute this ir unit
                b.k1.ir.globals_pending_eval.entry(global_id).or_insert(());
            }
            // We typically generate an instruction
            // representing the **address** of the global, because they are always
            // addresses to static memory. For aggregate types, that address _is_
            // the value of the expression referring to the global: we call this 'direct'.
            // But for non-reference types, we must 'load' the value from the address, since
            // the address is just an implementation detail, we call this 'indirect'.
            //
            // That's the unoptimized picture. When optimizations are enabled,
            // we'll try to fold straight to a value.

            let value_type = variable.type_id;
            let is_constant = global.is_constant;
            let value_pt = b.get_physical_type(value_type);

            if let Some(initial_value) = global.initial_value.as_value()
                && global.is_constant
                && value_pt.is_scalar()
                && !require_address
                && b.optimize_enabled()
                && global_id != GLOBAL_ID_K1_IS_STATIC
            {
                let value = compile_static_value(b, initial_value, value_pt);
                let folded_value = match value {
                    Value::Inst(_) => {
                        // A Data inst... that's fine as a value
                        Some(value)
                    }
                    Value::GlobalAddr { .. } => unreachable!(),
                    Value::StaticValue { .. } =>
                    // probably unreachable; since we check for scalar above, and currently
                    // only aggragates compile to Value::StaticValue, but too brittle to assert
                    {
                        None
                    }
                    Value::FunctionAddr(_) => unreachable!(),
                    Value::FnParam { .. } => unreachable!(),
                    Value::Data32 { .. } => Some(value),
                    Value::Empty => Some(value),
                };
                if let Some(value) = folded_value {
                    return CompileVariableResult::FoldedValue { value, pt: value_pt };
                }
            }

            {
                let addr = Value::GlobalAddr { storage_pt: value_pt, id: global_id };
                let is_direct = value_pt.is_agg();
                CompileVariableResult::Address {
                    addr,
                    pt: value_pt,
                    indirect: !is_direct,
                    constant: is_constant,
                }
            }
        }
        None => {
            let Some(var) = b.get_variable(variable_id) else {
                let mut variables = String::new();
                for (idx, bv) in b.k1.ir.b_variables.values().enumerate() {
                    if idx > 0 {
                        variables.push('\n');
                    }
                    write!(variables, "{} {}", bv.id, bv.value).unwrap();
                }
                eprintln!("Variables are: {}", variables);
                b.k1.ice_span(b.cur_span, "Missing variable")
            };
            let var_value = var.value;
            let var_indirect = var.indirect;
            let is_constant = false;
            CompileVariableResult::Address {
                addr: var_value,
                pt: var.pt,
                indirect: var_indirect,
                constant: is_constant,
            }
        }
    }
}

fn build_field_access(
    b: &mut Builder,
    dst: Option<Value>,
    field_ptr: Value,
    result_pt: PhysicalType,
    needs_copy: bool,
) -> Value {
    let make_copy = needs_copy;
    let comment = match make_copy {
        false => IrComment::FieldAccessNoCopy,
        true => IrComment::FieldAccessWCopy,
    };
    let loaded = load_or_copy(b, result_pt, dst, field_ptr, make_copy, comment);
    loaded
}

#[inline]
fn compile_ir_builtin(
    b: &mut Builder,
    call: Call,
    builtin: BuiltinIr,
    callee_fn_type: PhysicalFunctionType,
    dst: Option<Value>,
) -> K1Result<Value> {
    match builtin {
        BuiltinIr::BakeStaticValue => {
            // fn(intern) bakeStaticValue[T](value: T): u64
            let type_id = call.type_args.as_slice(&b.k1.mem)[0];
            let _physical_type = b.get_physical_type(type_id);

            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let value = compile_expr(b, None, arg0)?;
            let bake = b.push_inst_anon(Inst::BakeStaticValue { type_id, value });

            // Produces a type id, which is a scalar
            let stored = store_rich_if_dst(
                b,
                dst,
                callee_fn_type.return_type,
                bake.as_value(),
                IrComment::None,
            );
            Ok(stored)
        }
        BuiltinIr::Zeroed => {
            let type_id = call.type_args.as_slice(&b.k1.mem)[0];
            let pt = b.get_physical_type(type_id);
            match pt.as_enum() {
                PhysicalTypeEnum::Empty => Ok(Value::Empty),
                PhysicalTypeEnum::Agg(agg_id) => {
                    let pt_layout = b.k1.agg_types.get(agg_id).layout;
                    let dst = match dst {
                        None => b.push_alloca(pt, IrComment::ZeroedNoDst).as_value(),
                        Some(dst) => dst,
                    };
                    let zero_u8 = Value::byte(0);
                    // fn(intern) set(dst: ptr, value: u8, count: size): unit
                    let count = b.make_int_value(
                        &TypedIntValue::I64(pt_layout.size as i64),
                        IrComment::MemsetSize,
                    );
                    let memset_args = b.k1.ir.mem.pushn(&[dst, zero_u8, count]);
                    let Some(memset_function_id) =
                        b.k1.scopes.find_function(b.k1.scopes.mem_scope_id, b.k1.ast.idents.b.set)
                    else {
                        b_ice!(b, "Missing memset function");
                    };
                    let memset_call = IrCall {
                        ret_type: PhysicalType::EMPTY,
                        callee: IrCallee::BackendBuiltin(
                            memset_function_id,
                            BackendBuiltin::MemSet,
                        ),
                        args: memset_args,
                        dst: None,
                    };
                    let call_id = b.k1.ir.calls.add(memset_call);
                    b.push_inst(Inst::Call { call_id }, IrComment::ZeroedMemset);
                    Ok(dst)
                }
                PhysicalTypeEnum::Scalar(st) => {
                    let zero_value = zero(st);
                    let stored = store_scalar_if_dst(b, dst, zero_value);
                    Ok(stored)
                }
            }
        }
        BuiltinIr::BoolNegate => {
            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let base = compile_expr(b, None, arg0)?;
            let neg = b.push_inst_anon(Inst::BoolNegate { v: base });
            let stored = store_scalar_if_dst(b, dst, neg.as_value());
            Ok(stored)
        }
        BuiltinIr::BitNot => {
            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let base = compile_expr(b, None, arg0)?;
            let neg = b.push_inst_anon(Inst::BitNot { v: base });
            let stored = store_scalar_if_dst(b, dst, neg.as_value());
            Ok(stored)
        }
        BuiltinIr::Bitcast => {
            let from_type_id = call.type_args.as_slice(&b.k1.mem)[0];
            let to_type_id = call.type_args.as_slice(&b.k1.mem)[1];

            let from_pt = b.get_physical_type(from_type_id);
            let to_pt = b.get_physical_type(to_type_id);

            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let from_value = compile_expr(b, None, arg0)?;
            match (from_pt.as_enum(), to_pt.as_enum()) {
                (PhysicalTypeEnum::Empty, _) | (_, PhysicalTypeEnum::Empty) => {
                    Err(kerr!(b.k1, b.cur_span, "Cannot bitcast to or from empty type"))
                }
                (PhysicalTypeEnum::Scalar(_), PhysicalTypeEnum::Scalar(_)) => {
                    // Note that this also covers Pointer to Pointer
                    let bitcast = b.push_inst_anon(Inst::BitCast { v: from_value, to: to_pt });
                    let stored = store_rich_if_dst(
                        b,
                        dst,
                        to_pt,
                        bitcast.as_value(),
                        IrComment::FulfillBitcastDestination,
                    );
                    Ok(stored)
                }
                (PhysicalTypeEnum::Scalar(_), PhysicalTypeEnum::Agg(_)) => {
                    // We need a place, so its alloca time
                    let locn = match dst {
                        Some(dst) => dst,
                        None => b.push_alloca(to_pt, IrComment::BitcastScalarToAggPlace).as_value(),
                    };

                    // We know a scalar store will work
                    let _stored =
                        b.push_store(locn, from_value, IrComment::BitcastScalarToAggStore);
                    Ok(locn)
                }
                (PhysicalTypeEnum::Agg(_), PhysicalTypeEnum::Scalar(_)) => {
                    // Perform a 'load' of the scalar type _from_ the
                    // aggregate's memory
                    let loaded = load_or_copy(
                        b,
                        to_pt,
                        dst,
                        from_value,
                        false,
                        IrComment::BitcastAggToScalar,
                    );
                    Ok(loaded)
                }
                (PhysicalTypeEnum::Agg(_), PhysicalTypeEnum::Agg(_)) => {
                    // Make a copy to a definitely-aligned destination.
                    let locn = match dst {
                        Some(dst) => dst,
                        None => b.push_alloca(to_pt, IrComment::BitcastAggToAggPlace).as_value(),
                    };
                    let _copied =
                        b.push_copy(locn, from_value, from_pt, IrComment::BitcastAggToAggCopy);
                    Ok(locn)
                }
            }
        }
        BuiltinIr::ArithBinop(op) => compile_arith_binop(b, op, &call, dst),
        BuiltinIr::BitwiseBinop(op) => {
            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let lhs = compile_expr(b, None, arg0)?;
            let arg1 = *b.k1.mem.get_nth(call.args, 1);
            let rhs = compile_expr(b, None, arg1)?;
            let lhs_pt = b.get_value_kind(lhs).expect_value().unwrap();
            let width = b.k1.get_pt_layout(lhs_pt).size_bits() as u8;
            let inst = match op {
                BitwiseBinopKind::And => Inst::BitAnd { lhs, rhs, width },
                BitwiseBinopKind::Or => Inst::BitOr { lhs, rhs, width },
                BitwiseBinopKind::Xor => Inst::BitXor { lhs, rhs, width },
                BitwiseBinopKind::ShiftLeft => Inst::BitShiftLeft { lhs, rhs, width },
                BitwiseBinopKind::UnsignedShiftRight => {
                    Inst::BitUnsignedShiftRight { lhs, rhs, width }
                }
                BitwiseBinopKind::SignedShiftRight => Inst::BitSignedShiftRight { lhs, rhs, width },
            };
            let res = b.push_inst_anon(inst);
            let stored = store_scalar_if_dst(b, dst, res.as_value());
            Ok(stored)
        }
        BuiltinIr::PointerIndex => {
            // fn(intern) refAtIndex[T](self: Pointer, index: uword): T*
            let elem_type_id = call.type_args.as_slice(&b.k1.mem)[0];
            let elem_pt = b.get_physical_type(elem_type_id);
            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let base = compile_expr(b, None, arg0)?;
            let arg1 = *b.k1.mem.get_nth(call.args, 1);
            let element_index = compile_expr(b, None, arg1)?;
            let offset = b.push_inst(
                Inst::ArrayOffset { element_t: elem_pt, base, element_index },
                IrComment::RefAtIndexOffset,
            );
            let stored = store_scalar_if_dst(b, dst, offset.as_value());
            Ok(stored)
        }
        BuiltinIr::AtomicLoad => {
            // fn(intern) load[t](src: *t, ord: ordering): t
            let t = atomic_element_type(b, &call, true)?;
            let ord = b.k1.atomic_ordering_arg(&call, 1)?;
            let src = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let inst = b.push_inst_anon(Inst::AtomicLoad { t, src, ord });
            Ok(store_scalar_if_dst(b, dst, inst.as_value()))
        }
        BuiltinIr::AtomicStore => {
            // fn(intern) store[t](dst: *mut t, value: t, ord: ordering)
            let t = atomic_element_type(b, &call, true)?;
            let ord = b.k1.atomic_ordering_arg(&call, 2)?;
            let store_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            b.push_inst_anon(Inst::AtomicStore { dst: store_dst, value, t, ord });
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        BuiltinIr::AtomicRmw(op) => {
            // fn(intern) <op>[t](dst: *mut t, value: t, ord: ordering): t
            use crate::typer::AtomicRmwOp as Op;
            let allow_pointer = op == Op::Xchg;
            let t = atomic_element_type(b, &call, allow_pointer)?;
            let signed =
                matches!(t, ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64);
            let op = match op {
                Op::Xchg => AtomicRmwOpIr::Xchg,
                Op::Add => AtomicRmwOpIr::Add,
                Op::Sub => AtomicRmwOpIr::Sub,
                Op::And => AtomicRmwOpIr::And,
                Op::Or => AtomicRmwOpIr::Or,
                Op::Xor => AtomicRmwOpIr::Xor,
                Op::Min if signed => AtomicRmwOpIr::MinS,
                Op::Min => AtomicRmwOpIr::MinU,
                Op::Max if signed => AtomicRmwOpIr::MaxS,
                Op::Max => AtomicRmwOpIr::MaxU,
            };
            let ord = b.k1.atomic_ordering_arg(&call, 2)?;
            let rmw_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let operand = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let inst = b.push_inst_anon(Inst::AtomicRmw { op, t, dst: rmw_dst, operand, ord });
            Ok(store_scalar_if_dst(b, dst, inst.as_value()))
        }
        BuiltinIr::AtomicCmpxchg { weak } => {
            // fn(intern) cmpxchg[t](dst: *mut t, expected: t, desired: t,
            //                      success: ordering, failure: ordering): cmpxchg-result[t]
            let t = atomic_element_type(b, &call, true)?;
            let success = b.k1.atomic_ordering_arg(&call, 3)?;
            let failure = b.k1.atomic_ordering_arg(&call, 4)?;
            let cas_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let expected = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let desired = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 2))?;
            let ret_pt = callee_fn_type.return_type;
            let PhysicalTypeEnum::Agg(agg_id) = ret_pt.as_enum() else {
                b_ice!(b, "cmpxchg return type must be an aggregate");
            };
            let Some(ok_vm_offset) = b.k1.get_struct_field_offset(agg_id, 1) else {
                b_ice!(b, "cmpxchg result missing ok field");
            };
            let result = match dst {
                None => b.push_alloca(ret_pt, IrComment::CmpxchgResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.k1.ir.cmpxchgs.add(AtomicCmpxchgData {
                t,
                dst: cas_dst,
                expected,
                desired,
                success,
                failure,
                weak,
                result,
                ok_vm_offset,
            });
            b.push_inst_anon(Inst::AtomicCmpxchg { id });
            Ok(result)
        }
        BuiltinIr::AtomicFence => {
            // fn(intern) fence(ord: ordering)
            let ord = b.k1.atomic_ordering_arg(&call, 0)?;
            b.push_inst_anon(Inst::Fence { ord });
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        BuiltinIr::VectorOp(op) => compile_vector_op(b, op, &call, callee_fn_type, dst),
    }
}

fn compile_vector_op(
    b: &mut Builder,
    op: crate::typer::VecOpKind,
    call: &Call,
    callee_fn_type: PhysicalFunctionType,
    dst: Option<Value>,
) -> K1Result<Value> {
    use crate::typer::VecOpKind;
    match op {
        VecOpKind::Splat => {
            // fn(intern) splat[t, n: static size](value: t): vector[t, n]
            let (elem, lanes) = vector_pt_parts(b, callee_fn_type.return_type)?;
            let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => {
                    b.push_alloca(callee_fn_type.return_type, IrComment::SplatResult).as_value()
                }
                Some(dst) => dst,
            };
            let id = b.k1.ir.vec_ops.add(VecOpData {
                op: VecOpIr::Splat,
                elem,
                lanes,
                dst: locn,
                lhs: value,
                rhs: Value::Empty,
            });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::Add
        | VecOpKind::Sub
        | VecOpKind::Mul
        | VecOpKind::BitAnd
        | VecOpKind::BitOr
        | VecOpKind::Xor
        | VecOpKind::EqLanes => {
            // (lhs: vector[t, n], rhs: vector[t, n]): vector[t, n]
            let op = match op {
                VecOpKind::Add => VecOpIr::Add,
                VecOpKind::Sub => VecOpIr::Sub,
                VecOpKind::Mul => VecOpIr::Mul,
                VecOpKind::BitAnd => VecOpIr::BitAnd,
                VecOpKind::BitOr => VecOpIr::BitOr,
                VecOpKind::Xor => VecOpIr::Xor,
                VecOpKind::EqLanes => VecOpIr::EqLanes,
                _ => unreachable!(),
            };
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt)?;
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let rhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecBinopResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.k1.ir.vec_ops.add(VecOpData { op, elem, lanes, dst: locn, lhs, rhs });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::BitNot => {
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt)?;
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecNotResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.k1.ir.vec_ops.add(VecOpData {
                op: VecOpIr::BitNot,
                elem,
                lanes,
                dst: locn,
                lhs,
                rhs: Value::Empty,
            });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::ShiftLeft | VecOpKind::ShiftRight => {
            // (lhs: vector[t, n], count: u32): vector[t, n]
            let op = if op == VecOpKind::ShiftLeft { VecOpIr::Shl } else { VecOpIr::Shr };
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt)?;
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let count = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecShiftResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.k1.ir.vec_ops.add(VecOpData { op, elem, lanes, dst: locn, lhs, rhs: count });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::ToMask => {
            // (v: vector[t, n]): u64
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let (elem, lanes) = vector_pt_parts(b, vec_pt)?;
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let id = b.k1.ir.vec_ops.add(VecOpData {
                op: VecOpIr::ToMask,
                elem,
                lanes,
                dst: Value::Empty,
                lhs,
                rhs: Value::Empty,
            });
            let inst = b.push_inst_anon(Inst::VecOp { id });
            Ok(store_scalar_if_dst(b, dst, inst.as_value()))
        }
        VecOpKind::Load => {
            // fn(intern) load-unchecked[t, n: static size](src: ptr): vector[t, n]
            let ret_pt = callee_fn_type.return_type;
            let _ = vector_pt_parts(b, ret_pt)?;
            let src = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VectorLoadResult).as_value(),
                Some(dst) => dst,
            };
            b.push_copy(locn, src, ret_pt, IrComment::VectorLoad);
            Ok(locn)
        }
        VecOpKind::Store => {
            // fn(intern) store-unchecked[t, n](self: vector[t, n], dst: ptr)
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let _ = vector_pt_parts(b, vec_pt)?;
            let vec_value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let dst_ptr = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            b.push_copy(dst_ptr, vec_value, vec_pt, IrComment::VectorStore);
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        VecOpKind::GetLane => {
            // fn(intern) get-lane[t, n](self: vector[t, n], index: size): t
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let (elem, _) = vector_pt_parts(b, vec_pt)?;
            let base = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let element_index = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let offset = b.push_inst(
                Inst::ArrayOffset { element_t: PhysicalType::scalar(elem), base, element_index },
                IrComment::GetLaneOffset,
            );
            let loaded = b.push_load(elem, offset.as_value(), IrComment::GetLaneLoad);
            Ok(store_scalar_if_dst(b, dst, loaded.as_value()))
        }
        VecOpKind::WithLane => {
            // fn(intern) with-lane[t, n](self: vector[t, n], index: size, value: t): vector[t, n]
            let ret_pt = callee_fn_type.return_type;
            let (elem, _) = vector_pt_parts(b, ret_pt)?;
            let src_vec = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let element_index = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 2))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::WithLaneResult).as_value(),
                Some(dst) => dst,
            };
            b.push_copy(locn, src_vec, ret_pt, IrComment::WithLaneCopy);
            let offset = b.push_inst(
                Inst::ArrayOffset {
                    element_t: PhysicalType::scalar(elem),
                    base: locn,
                    element_index,
                },
                IrComment::WithLaneOffset,
            );
            b.push_store(offset.as_value(), value, IrComment::WithLaneStore);
            Ok(locn)
        }
    }
}

/// The (element, lane-count) of a concrete vector physical type; errors with a
/// source span for still-abstract or non-vector instantiations
fn vector_pt_parts(b: &mut Builder, pt: PhysicalType) -> K1Result<(ScalarType, u32)> {
    let PhysicalTypeEnum::Agg(agg_id) = pt.as_enum() else {
        kbail!(
            b.k1,
            b.cur_span,
            "vector intrinsic requires a concrete vector type; got {}",
            b.k1.pt_to_string(pt)
        );
    };
    match b.k1.agg_types.get(agg_id).agg_type {
        AggType::Vector { element_pt, len } => Ok((element_pt, len)),
        _ => Err(kerr!(
            b.k1,
            b.cur_span,
            "vector intrinsic requires a vector type; got {}",
            b.k1.pt_to_string(pt)
        )),
    }
}

/// The element type of an atomic intrinsic: type_args[0], which must be an
/// integer-class scalar (pointers allowed for the non-arithmetic ops).
fn atomic_element_type(b: &mut Builder, call: &Call, allow_pointer: bool) -> K1Result<ScalarType> {
    let type_id = call.type_args.as_slice(&b.k1.mem)[0];
    let pt = b.get_physical_type(type_id);
    let scalar = match pt.as_enum() {
        PhysicalTypeEnum::Scalar(st) => Some(st),
        _ => None,
    };
    let supported = match scalar {
        Some(st) if st.is_int() => true,
        Some(ScalarType::Pointer) => allow_pointer,
        _ => false,
    };
    if !supported {
        kbail!(
            b.k1,
            b.cur_span,
            "atomic operations are not supported for type {}; supported are integer-sized scalars{}",
            type_id,
            if allow_pointer { " and pointers" } else { "" }
        );
    }
    Ok(scalar.unwrap())
}

#[inline]
fn compile_cast(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<Value>,
    c: &TypedCast,
    expr_id: TypedExprId,
) -> K1Result<Value> {
    let target_type_id = b.k1.exprs.get_type(expr_id);
    match c.cast_type {
        CastType::ReferenceToReference
        | CastType::ReferenceToMut
        | CastType::ReferenceUnMut
        | CastType::IntegerCast(IntegerCastDirection::NoOp)
        | CastType::IntegerCast(IntegerCastDirection::SignChange)
        | CastType::Integer8ToChar
        | CastType::PointerToReference
        | CastType::PointerToFunctionPointer
        | CastType::ReferenceToPointer => {
            let base_noop = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id);
            let stored =
                store_rich_if_dst(b, dst, to_pt, base_noop, IrComment::FulfillCastDestination);
            Ok(stored)
        }
        CastType::IntegerCast(IntegerCastDirection::Extend)
        | CastType::IntegerCast(IntegerCastDirection::Truncate)
        | CastType::IntegerExtendFromChar => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id);
            let to = to_pt.expect_scalar();
            let inst = match c.cast_type {
                CastType::IntegerCast(IntegerCastDirection::Extend)
                | CastType::IntegerExtendFromChar => {
                    let signed = if matches!(c.cast_type, CastType::IntegerExtendFromChar) {
                        false
                    } else {
                        b.k1.get_expr_type(c.base_expr).as_integer().unwrap().is_signed()
                    };
                    if signed {
                        Inst::IntExtS { from: ScalarType::U8, v: base, to }
                    } else {
                        Inst::IntExtU { v: base, to }
                    }
                }
                CastType::IntegerCast(IntegerCastDirection::Truncate) => {
                    Inst::IntTrunc { v: base, to }
                }
                _ => unreachable!(),
            };
            let inst = b.push_inst_anon(inst);
            let stored = store_scalar_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
        CastType::BoolToInt => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id);
            let to = to_pt.expect_scalar();
            let bitcast = b.push_inst_anon(Inst::BitCast {
                v: base,
                to: PhysicalType::scalar(ScalarType::U8),
            });
            let extend =
                b.push_inst(Inst::IntExtU { v: bitcast.as_value(), to }, IrComment::BoolToInt);
            let stored = store_scalar_if_dst(b, dst, extend.as_value());
            Ok(stored)
        }
        CastType::PointerToWord | CastType::WordToPointer => {
            let base = compile_expr(b, None, c.base_expr)?;
            let inst = match c.cast_type {
                CastType::PointerToWord => Inst::PtrToWord { v: base },
                CastType::WordToPointer => Inst::WordToPtr { v: base },
                _ => unreachable!(),
            };
            let inst = b.push_inst_anon(inst);
            let stored = store_scalar_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
        CastType::FloatExtend
        | CastType::FloatTruncate
        | CastType::FloatToUnsignedInteger
        | CastType::FloatToSignedInteger
        | CastType::IntegerUnsignedToFloat
        | CastType::IntegerSignedToFloat => {
            let base = compile_expr(b, None, c.base_expr)?;
            let from = b.get_value_kind(base).expect_value().unwrap().expect_scalar();
            let to = b.get_physical_type(target_type_id).expect_scalar();
            let inst = match c.cast_type {
                CastType::FloatExtend => Inst::FloatExt { v: base, to },
                CastType::FloatTruncate => Inst::FloatTrunc { v: base, to },
                CastType::FloatToUnsignedInteger => match from {
                    ScalarType::F32 => Inst::Float32ToIntUnsigned { v: base, to },
                    ScalarType::F64 => Inst::Float64ToIntUnsigned { v: base, to },
                    _ => unreachable!(),
                },
                CastType::FloatToSignedInteger => match from {
                    ScalarType::F32 => Inst::Float32ToIntSigned { v: base, to },
                    ScalarType::F64 => Inst::Float64ToIntSigned { v: base, to },
                    _ => unreachable!(),
                },
                CastType::IntegerUnsignedToFloat => Inst::IntToFloatUnsigned { v: base, from, to },
                CastType::IntegerSignedToFloat => Inst::IntToFloatSigned { v: base, from, to },
                _ => unreachable!(),
            };
            let inst = b.push_inst_anon(inst);
            let stored = store_scalar_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
        // Only occurs in abstract (where-bound generic) bodies, which are never
        // lowered; specialization re-evaluates and produces the struct literal
        CastType::AbilityImplToDynObject => b_ice!(b, "ir on abstract dyn-ability erasure"),
    }
}

fn compile_arith_binop(
    b: &mut Builder,
    op: ArithOpKind,
    call: &Call,
    dst: Option<Value>,
) -> K1Result<Value> {
    let arg0 = *b.k1.mem.get_nth(call.args, 0);
    let lhs = compile_expr(b, None, arg0)?;
    let arg1 = *b.k1.mem.get_nth(call.args, 1);
    let rhs = compile_expr(b, None, arg1)?;
    use ArithOpClass as Class;
    use ArithOpOp as Op;
    let lhs_type = b.k1.exprs.get_type(arg0);
    let lhs_pt = b.get_physical_type(lhs_type);
    let lhs_width = b.k1.get_pt_layout(lhs_pt).size_bits() as u8;
    let inst = match (op.op, op.class) {
        (Op::Add, Class::SignedInt | Class::UnsignedInt) => {
            Inst::IntAdd { lhs, rhs, width: lhs_width }
        }
        (Op::Sub, Class::SignedInt | Class::UnsignedInt) => {
            Inst::IntSub { lhs, rhs, width: lhs_width }
        }
        (Op::Mul, Class::SignedInt | Class::UnsignedInt) => {
            Inst::IntMul { lhs, rhs, width: lhs_width }
        }
        (Op::Div, Class::UnsignedInt) => Inst::IntDivUnsigned { lhs, rhs, width: lhs_width },
        (Op::Div, Class::SignedInt) => Inst::IntDivSigned { lhs, rhs, width: lhs_width },
        (Op::Rem, Class::UnsignedInt) => Inst::IntRemUnsigned { lhs, rhs, width: lhs_width },
        (Op::Rem, Class::SignedInt) => Inst::IntRemSigned { lhs, rhs, width: lhs_width },
        (Op::Equals, Class::SignedInt | Class::UnsignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Eq, width: lhs_width }
        }
        (Op::Lt, Class::UnsignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Ult, width: lhs_width }
        }
        (Op::Lt, Class::SignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Slt, width: lhs_width }
        }
        (Op::Le, Class::UnsignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Ule, width: lhs_width }
        }
        (Op::Le, Class::SignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Sle, width: lhs_width }
        }
        (Op::Gt, Class::UnsignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Ugt, width: lhs_width }
        }
        (Op::Gt, Class::SignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Sgt, width: lhs_width }
        }
        (Op::Ge, Class::UnsignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Uge, width: lhs_width }
        }
        (Op::Ge, Class::SignedInt) => {
            Inst::IntCmp { lhs, rhs, pred: IntCmpPred::Sge, width: lhs_width }
        }
        (Op::Add, Class::Float) => Inst::FloatAdd { lhs, rhs, width: lhs_width },
        (Op::Sub, Class::Float) => Inst::FloatSub { lhs, rhs, width: lhs_width },
        (Op::Mul, Class::Float) => Inst::FloatMul { lhs, rhs, width: lhs_width },
        (Op::Div, Class::Float) => Inst::FloatDiv { lhs, rhs, width: lhs_width },
        (Op::Rem, Class::Float) => Inst::FloatRem { lhs, rhs, width: lhs_width },
        (Op::Equals, Class::Float) => {
            Inst::FloatCmp { lhs, rhs, pred: FloatCmpPred::Eq, width: lhs_width }
        }
        (Op::Lt, Class::Float) => {
            Inst::FloatCmp { lhs, rhs, pred: FloatCmpPred::Lt, width: lhs_width }
        }
        (Op::Le, Class::Float) => {
            Inst::FloatCmp { lhs, rhs, pred: FloatCmpPred::Le, width: lhs_width }
        }
        (Op::Gt, Class::Float) => {
            Inst::FloatCmp { lhs, rhs, pred: FloatCmpPred::Gt, width: lhs_width }
        }
        (Op::Ge, Class::Float) => {
            Inst::FloatCmp { lhs, rhs, pred: FloatCmpPred::Ge, width: lhs_width }
        }
    };
    let res = b.push_inst(inst, IrComment::None);
    let stored = store_scalar_if_dst(b, dst, res.as_value());
    Ok(stored)
}

/// Loads a value of a given type from 'src'.
/// 'Load' in this context is an operation internal to this
/// IR; it doesn't have a direct analog in the source language.
/// A Dereference would the closest thing. But we take some liberties here;
/// such as treating this as a no-op for values that are already represented
/// by their location, aka IndirectValues
///
/// INVARIANT: we should never emits a load, store, or copy of Empty
fn load_value(
    b: &mut Builder,
    pt: PhysicalType,
    src: Value,
    make_copy: bool,
    comment: IrComment,
) -> Value {
    match pt.as_enum() {
        PhysicalTypeEnum::Agg(_) => {
            if make_copy {
                let dst = b.push_alloca(pt, comment);
                b.push_copy(dst.as_value(), src, pt, comment);
                dst.as_value()
            } else {
                src
            }
        }
        PhysicalTypeEnum::Scalar(st) => b.push_load(st, src, comment).as_value(),
        PhysicalTypeEnum::Empty => Value::Empty,
    }
}

fn store_value(
    b: &mut Builder,
    pt: PhysicalType,
    dst: Value,
    value: Value,
    comment: IrComment,
) -> Option<InstId> {
    match pt.as_enum() {
        PhysicalTypeEnum::Agg(_) => {
            // Rename to `src` shows that, since we have an aggregate, `value` is a location.
            let src = value;
            let copy_inst = b.push_copy(dst, src, pt, comment);
            debug_assert!(copy_inst.is_some(), "We know its not the Empty type");
            copy_inst
        }
        PhysicalTypeEnum::Scalar(_) => {
            let store_inst = b.push_store(dst, value, comment);
            Some(store_inst)
        }
        PhysicalTypeEnum::Empty => None,
    }
}

fn load_or_copy(
    b: &mut Builder,
    pt: PhysicalType,
    dst: Option<Value>,
    src: Value,
    copy_aggregates: bool,
    comment: IrComment,
) -> Value {
    match dst {
        Some(dst) => {
            let _copy = b.push_copy(dst, src, pt, comment);
            dst
        }
        None => load_value(b, pt, src, copy_aggregates, comment),
    }
}

fn compile_matching_condition(
    b: &mut Builder,
    mc: &MatchingCondition,
    cons_block: BlockId,
    condition_fail_block: Option<BlockId>,
) -> K1Result<()> {
    if mc.instrs.is_empty() {
        // Always true
        b.push_jump(cons_block, IrComment::EmptyCondition);
        return Ok(());
    }
    for (index, inst) in b.k1.mem.getn(mc.instrs).iter().enumerate() {
        let is_last = index == mc.instrs.len() as usize - 1;
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(b, None, *let_stmt)?;
                if is_last {
                    b.push_jump(cons_block, IrComment::MatchingCondBindingFallthroughToCons);
                }
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_value: Value = compile_expr(b, None, *value)?;
                let continue_block = if is_last {
                    cons_block
                } else {
                    b.push_block(BlockSourceKind::MatchingCondContinue)
                };

                // If the matching condition was typechecked as 'infallible', we don't have a fail
                // block, and we just jump to continue.
                match condition_fail_block {
                    None => b.push_jump(continue_block, IrComment::InfallibleMatchingCondContinue),
                    Some(fail_block) => b.push_jump_if(
                        cond_value,
                        continue_block,
                        fail_block,
                        IrComment::MatchingCondCond,
                    ),
                };

                b.goto_block(continue_block);
            }
        }
    }
    Ok(())
}

pub fn zero(t: ScalarType) -> Value {
    // The all-zeroes bit pattern is zero for every scalar, floats included
    Value::Data32 { t, data: 0 }
}

pub fn get_compiled_unit(ir: &ProgramIr, unit: IrUnitId) -> Option<IrUnit> {
    match unit {
        IrUnitId::Function(function_id) => ir.functions.get(function_id).as_ref().copied(),
        IrUnitId::Expr(typed_expr_id) => ir.exprs.get(&typed_expr_id).copied(),
    }
}

pub fn get_compiled_unit_mut(ir: &mut ProgramIr, unit: IrUnitId) -> Option<&mut IrUnit> {
    match unit {
        IrUnitId::Function(function_id) => match ir.functions.get_mut(function_id) {
            Some(func) => Some(func),
            _ => None,
        },
        IrUnitId::Expr(typed_expr_id) => ir.exprs.get_mut(&typed_expr_id),
    }
}

pub fn get_unit_span(k1: &TypedProgram, unit: IrUnitId) -> SpanId {
    match unit {
        IrUnitId::Function(function_id) => k1.get_function_span(function_id),
        IrUnitId::Expr(typed_expr_id) => k1.exprs.get_span(typed_expr_id),
    }
}

////////////////////////////// Validation //////////////////////////////

pub fn validate_unit(k1: &TypedProgram, unit_id: IrUnitId) -> K1Result<()> {
    let mut errors = Vec::new();
    let ir = &k1.ir;
    let span = get_unit_span(k1, unit_id);
    let Some(unit) = get_compiled_unit(&k1.ir, unit_id) else {
        kbail!(k1, span, "Not compiled");
    };
    // eprintln!("validate_unit: {}", unit_name_to_string(k1, unit_id));
    // eprintln!("blocks.first: {}", unit.blocks.first.raw_index());
    // eprintln!("blocks.last: {}", unit.blocks.first.raw_index());
    let mut my_blocks = FxHashSet::new();
    for (block_id, _block) in ir.mem.dlist_iter_handles(unit.blocks) {
        my_blocks.insert(block_id);
    }
    for (block_id, block) in ir.mem.dlist_iter_handles(unit.blocks) {
        for inst_node in ir.mem.dlist_iter_nodes(block.data.instrs) {
            let inst_id = inst_node.data;
            let is_last = inst_node.is_last();
            let inst = ir.instrs.get(inst_id);
            let inst_kind = get_inst_kind(ir, inst_id);
            if !is_last && inst_kind.is_terminator() {
                errors.push(format!("b{}: stray terminator", block_id.raw_index()))
            };
            if is_last && !inst_kind.is_terminator() {
                errors.push(format!("b{}: unterminated", block_id.raw_index()))
            }

            match *inst {
                Inst::Data(_imm) => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, .. } => {
                    let dst_type = get_value_kind(ir, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("store dst v{} is not a ptr", inst_id))
                    }
                }
                Inst::Load { src, .. } => {
                    let src_kind = get_value_kind(ir, src);
                    if !src_kind.is_storage() {
                        errors.push(format!("i{inst_id}: load src is not storage"))
                    }
                }
                Inst::AtomicLoad { src, .. } => {
                    let src_kind = get_value_kind(ir, src);
                    if !src_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic load src is not storage"))
                    }
                }
                Inst::AtomicStore { dst, .. } => {
                    let dst_kind = get_value_kind(ir, dst);
                    if !dst_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic store dst is not storage"))
                    }
                }
                Inst::AtomicRmw { dst, .. } => {
                    let dst_kind = get_value_kind(ir, dst);
                    if !dst_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic rmw dst is not storage"))
                    }
                }
                Inst::AtomicCmpxchg { id } => {
                    let cas = ir.cmpxchgs.get(id);
                    for (v, what) in [(cas.dst, "dst"), (cas.result, "result")] {
                        if !get_value_kind(ir, v).is_storage() {
                            errors.push(format!("i{inst_id}: atomic cmpxchg {what} is not storage"))
                        }
                    }
                }
                Inst::VecOp { id } => {
                    let vop = ir.vec_ops.get(id);
                    if vop.op != VecOpIr::ToMask && !get_value_kind(ir, vop.dst).is_storage() {
                        errors.push(format!("i{inst_id}: vec op dst is not storage"))
                    }
                }
                Inst::Fence { .. } => (),
                Inst::Copy { dst, src, .. } => {
                    let src_type = get_value_kind(ir, src);
                    if !src_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy src is not a ptr"))
                    }
                    let dst_type = get_value_kind(ir, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy dst v{} is not a ptr", inst_id))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = get_value_kind(ir, base);
                    if !base_type.is_storage() {
                        errors.push(format!("i{inst_id}: struct_offset base is not a ptr"))
                    }
                }
                Inst::ArrayOffset { base, element_index, .. } => {
                    let base_type = get_value_kind(ir, base);
                    let index_type = get_value_kind(ir, element_index);
                    if !base_type.is_storage() {
                        errors.push(format!("i{inst_id}: array_offset base is not a ptr"))
                    }

                    if index_type.as_value().and_then(|t| t.as_scalar()).map(|st| st.width())
                        != Some(NumericWidth::B64)
                    {
                        errors.push(format!(
                            "i{inst_id}: array_offset index type is not word-sized int",
                        ))
                    }
                }
                Inst::Call { .. } => (),
                Inst::Jump(block) => {
                    if !my_blocks.contains(&block) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                }
                Inst::JumpIf { cond, cons, alt } => {
                    let cond_type = get_value_kind(ir, cond);
                    if !cond_type.is_value() {
                        errors.push(format!("i{inst_id}: jumpif cond is not a value"))
                    }

                    if !my_blocks.contains(&cons) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                    if !my_blocks.contains(&alt) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                }
                Inst::Unreachable => (),
                Inst::Phi { incomings, .. } => {
                    for incoming in ir.mem.getn(incomings) {
                        let Ok(_value_type) = get_value_kind(ir, incoming.value).expect_value()
                        else {
                            errors.push(format!("i{inst_id}: phi type not a value kind"));
                            continue;
                        };
                        if incoming.from == block_id {
                            errors.push(format!("i{inst_id}: phi incoming block cannot be self"))
                        } else if !my_blocks.contains(&incoming.from) {
                            errors.push(format!("i{inst_id}: phi incoming block does not exist"))
                        }
                    }
                }
                Inst::Ret { v, .. } => {
                    let ret_val_type = get_value_kind(ir, v);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("i{inst_id}: ret value is not a value"))
                    }
                }
                Inst::BoolNegate { v } => {
                    let inst_type = get_value_kind(ir, v);
                    if !inst_type.is_bool() {
                        errors.push(format!("i{inst_id}: bool_negate src is not a bool"))
                    }
                }
                Inst::BitNot { v } => {
                    let inst_type = get_value_kind(ir, v);
                    if !inst_type.is_int() {
                        errors.push(format!("i{inst_id}: bit_not src is not an int"))
                    }
                }
                Inst::BitCast { .. } => (),
                Inst::IntTrunc { to, .. } => {
                    if !to.is_int() {
                        errors.push("i{inst_id}: int trunc to non-int type".to_string())
                    }
                }
                Inst::IntExtU { v, to } | Inst::IntExtS { v, to, .. } => {
                    let inst_type = get_value_kind(ir, v);
                    if !inst_type.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u src is not an int"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u to is not int"))
                    }
                }
                Inst::FloatTrunc { v, to } => {
                    let inst_type = get_value_kind(ir, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F64))
                    {
                        errors.push(format!("i{inst_id}: float_trunc src is not f64"))
                    }
                    if to != ScalarType::F32 {
                        errors.push(format!("i{inst_id}: float_trunc to is not f32"))
                    }
                }
                Inst::FloatExt { v, to } => {
                    let inst_type = get_value_kind(ir, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float_ext src is not f32"))
                    }
                    if to != ScalarType::F64 {
                        errors.push(format!("i{inst_id}: float_ext to is not f64"))
                    }
                }
                Inst::Float32ToIntUnsigned { v, to } | Inst::Float32ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(ir, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float32_to_int src is not f32"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: float32_to_int to is not int"))
                    }
                }
                Inst::Float64ToIntUnsigned { v, to } | Inst::Float64ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(ir, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F64))
                    {
                        errors.push(format!("i{inst_id}: float64_to_int src is not f64"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: float64_to_int to is not int"))
                    }
                }
                Inst::IntToFloatUnsigned { .. } => (),
                Inst::IntToFloatSigned { .. } => (),
                Inst::PtrToWord { v } => {
                    let inst_type = get_value_kind(ir, v);
                    if !inst_type.is_storage() {
                        errors.push(format!("i{inst_id}: ptr_to_word src is not a ptr"))
                    }
                }
                Inst::WordToPtr { v } => {
                    let inst_type = get_value_kind(ir, v);
                    if inst_type.as_value().and_then(|t| t.as_scalar()).is_none() {
                        errors.push(format!("i{inst_id}: word_to_ptr src is not a scalar int",))
                    }
                }
                Inst::IntAdd { .. } => (),
                Inst::IntSub { .. } => (),
                Inst::IntMul { .. } => (),
                Inst::IntDivUnsigned { .. } => (),
                Inst::IntDivSigned { .. } => (),
                Inst::IntRemUnsigned { .. } => (),
                Inst::IntRemSigned { .. } => (),
                Inst::IntCmp { .. } => (),
                Inst::FloatAdd { .. } => (),
                Inst::FloatSub { .. } => (),
                Inst::FloatMul { .. } => (),
                Inst::FloatDiv { .. } => (),
                Inst::FloatRem { .. } => (),
                Inst::FloatCmp { .. } => (),
                Inst::BitAnd { .. } => (),
                Inst::BitOr { .. } => (),
                Inst::BitXor { .. } => (),
                Inst::BitShiftLeft { .. } => (),
                Inst::BitUnsignedShiftRight { .. } => (),
                Inst::BitSignedShiftRight { .. } => (),
                Inst::BakeStaticValue { .. } => (),
            }
        }
    }
    if !errors.is_empty() {
        let error_string = errors.join("\n");
        Err(K1Message {
            span,
            message: k1.ast.idents.intern(format!(
                "IR Unit failed validation\n{}\n{}",
                unit_to_string(k1, unit_id, true),
                error_string
            )),
            level: MessageLevel::Error,
            error_kind: ErrorKind::Internal,
        })
    } else {
        Ok(())
    }
}

mod iropt;
pub use iropt::cfg_compute_unit;
pub use iropt::cfg_simplify;
pub use iropt::optimize_unit;

////////////////////////////// Display //////////////////////////////

pub fn unit_to_string(k1: &TypedProgram, unit: IrUnitId, show_source: bool) -> String {
    let mut s = String::new();
    let unit = get_compiled_unit(&k1.ir, unit).unwrap();
    display_unit(&mut s, k1, &unit, show_source).unwrap();
    s
}

pub fn display_unit_name(
    w: &mut impl Write,
    k1: &TypedProgram,
    unit: IrUnitId,
) -> std::fmt::Result {
    match unit {
        IrUnitId::Function(function_id) => {
            let function = k1.functions.get(function_id);
            k1.write_qualified_name(
                w,
                function.scope,
                k1.ident_str(function.name),
                None,
                "/",
                true,
            );
        }
        IrUnitId::Expr(typed_expr_id) => {
            let expr_span = k1.exprs.get_span(typed_expr_id);
            let (source, line) = k1.get_span_location(expr_span);
            write!(w, "expr {}:{}", &source.filename, line.line_number())?;
        }
    };
    Ok(())
}

pub fn unit_name_to_string(k1: &TypedProgram, unit: IrUnitId) -> String {
    let mut s = String::new();
    display_unit_name(&mut s, k1, unit).unwrap();
    s
}

pub fn display_phys_fn_type(
    w: &mut impl Write,
    k1: &TypedProgram,
    p_fn_ty: &PhysicalFunctionType,
) -> std::fmt::Result {
    w.write_str("fn(")?;
    for (index, param) in k1.ir.mem.getn(p_fn_ty.params).iter().enumerate() {
        write!(w, "p{}: ", index)?;
        k1.display_pt(w, param.pt)?;
        let last = index == p_fn_ty.params.len() as usize - 1;
        if !last {
            w.write_str(", ")?;
        }
    }
    w.write_str("): ")?;
    k1.display_pt(w, p_fn_ty.return_type)?;
    Ok(())
}

pub fn display_unit(
    w: &mut impl Write,
    k1: &TypedProgram,
    unit: &IrUnit,
    show_source: bool,
) -> std::fmt::Result {
    match unit.unit_id {
        IrUnitId::Function(function_id) => {
            k1.write_ident(w, k1.functions.get(function_id).name)?;
            w.write_str(" ")?;
            display_phys_fn_type(w, k1, &unit.fn_type)?;
        }
        IrUnitId::Expr(typed_expr_id) => {
            let expr_span = k1.exprs.get_span(typed_expr_id);
            let (source, line) = k1.get_span_location(expr_span);
            w.write_str("expr ")?;
            display_phys_fn_type(w, k1, &unit.fn_type)?;
            write!(w, "(type id: {})", k1.type_id_to_string(unit.result_type_id))?;
            write!(w, " from {}:{}", k1.ident_str(source.filename), line.line_number())?;
        }
    };
    writeln!(w, " (inst count={}, cfg_valid={})", unit.inst_count, unit.cfg_valid)?;
    display_blocks(w, k1, unit.blocks, unit.cfg_valid, show_source)?;
    Ok(())
}

pub fn display_blocks(
    w: &mut impl Write,
    k1: &TypedProgram,
    blocks: Dlist<Block, ProgramIr>,
    cfg_valid: bool,
    show_source: bool,
) -> std::fmt::Result {
    for (block, _) in k1.ir.mem.dlist_iter_handles(blocks) {
        display_block(w, k1, block, cfg_valid, show_source)?;
    }
    Ok(())
}

pub fn blocks_to_string(
    k1: &TypedProgram,
    blocks: Dlist<Block, ProgramIr>,
    cfg_valid: bool,
    show_source: bool,
) -> String {
    let mut s = String::new();
    display_blocks(&mut s, k1, blocks, cfg_valid, show_source).unwrap();
    s
}

pub fn display_compiled_expr(
    w: &mut impl Write,
    k1: &TypedProgram,
    ir: &ProgramIr,
    expr_id: TypedExprId,
    show_source: bool,
) -> std::fmt::Result {
    let Some(unit) = ir.exprs.get(&expr_id) else { return Ok(()) };
    display_unit(w, k1, unit, show_source)
}

pub fn display_function(
    w: &mut impl Write,
    k1: &TypedProgram,
    ir: &ProgramIr,
    function: FunctionId,
    show_source: bool,
) -> std::fmt::Result {
    let Some(unit) = ir.functions.get(function) else { return Ok(()) };
    display_unit(w, k1, unit, show_source)
}

pub fn inst_to_index(inst_id: InstId, offset: u32) -> u32 {
    inst_id.as_u32() - offset
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    block_id: BlockId,
    cfg_valid: bool,
    show_source: bool,
) -> std::fmt::Result {
    let ir = &k1.ir;
    let block = ir.mem.get(block_id).data;
    write!(w, "b{} {}", block_id.raw_index(), block.kind.str())?;
    if cfg_valid {
        write!(w, "  preds: [")?;
        for (idx, pred) in ir.mem.dlist_iter(block.preds).enumerate() {
            if idx > 0 {
                write!(w, ", ")?;
            }
            write!(w, "b{}", pred.raw_index())?;
        }
        write!(w, "], succs: [")?;
        for (idx, succ) in ir.mem.dlist_iter(block.succs).enumerate() {
            if idx > 0 {
                write!(w, ", ")?;
            }
            write!(w, "b{}", succ.raw_index())?;
        }
        write!(w, "]")?;
    }
    writeln!(w)?;
    for inst_id in ir.mem.dlist_iter(block.instrs) {
        write!(w, " i{:3} = ", *inst_id)?;
        let inst_str = inst_to_string(k1, *inst_id);
        write!(w, "{:60}", inst_str)?;
        let comment = ir.comments.get(*inst_id);
        write!(w, "; {:30}", comment.str())?;

        if show_source {
            let span_id = *ir.sources.get(*inst_id);
            let lines = k1.ast.get_span_content(span_id);
            let the_span = k1.ast.spans.get(span_id);
            let (_, line) = k1.get_span_location(span_id);
            let first_line = lines.lines().next().unwrap_or("");
            let column = the_span.start + 1 - line.start_char;
            write!(w, "| {first_line:30}|{:3}:{}|", line.line_number(), column)?;
        }
        writeln!(w)?;
    }
    writeln!(w, "END")?;
    Ok(())
}

pub fn inst_to_string(k1: &TypedProgram, inst_id: InstId) -> String {
    let mut s = String::new();
    display_inst(&mut s, k1, inst_id).unwrap();
    s
}

pub fn display_inst(w: &mut impl Write, k1: &TypedProgram, inst_id: InstId) -> std::fmt::Result {
    match *k1.ir.instrs.get(inst_id) {
        Inst::Data(imm) => {
            write!(w, "imm ")?;
            display_imm(w, imm)?;
        }
        Inst::Alloca { t, vm_layout, returned } => {
            write!(w, "alloca ")?;
            if returned {
                w.write_str("returned ")?;
            }
            k1.display_pt(w, t)?;
            write!(w, ", align {}", vm_layout.align)?;
        }
        Inst::Store { dst, value, t } => {
            write!(w, "store to {}, ", dst,)?;
            display_scalar_type(w, t)?;
            write!(w, " {}", value)?;
        }
        Inst::Load { t, src } => {
            write!(w, "load ")?;
            display_scalar_type(w, t)?;
            write!(w, " from {}", src)?;
        }
        Inst::AtomicLoad { t, src, ord } => {
            write!(w, "atomic load {} ", ord.name())?;
            display_scalar_type(w, t)?;
            write!(w, " from {}", src)?;
        }
        Inst::AtomicStore { dst, value, t, ord } => {
            write!(w, "atomic store {} to {}, ", ord.name(), dst)?;
            display_scalar_type(w, t)?;
            write!(w, " {}", value)?;
        }
        Inst::AtomicRmw { op, t, dst, operand, ord } => {
            write!(w, "atomic {} {} ", op.name(), ord.name())?;
            display_scalar_type(w, t)?;
            write!(w, " at {}, {}", dst, operand)?;
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = k1.ir.cmpxchgs.get(id);
            write!(
                w,
                "atomic cmpxchg{} {}/{} ",
                if cas.weak { " weak" } else { "" },
                cas.success.name(),
                cas.failure.name()
            )?;
            display_scalar_type(w, cas.t)?;
            write!(
                w,
                " at {}, expected {}, desired {}, into {}",
                cas.dst, cas.expected, cas.desired, cas.result
            )?;
        }
        Inst::VecOp { id } => {
            let vop = k1.ir.vec_ops.get(id);
            write!(w, "vec {} <{} x ", vop.op.name(), vop.lanes)?;
            display_scalar_type(w, vop.elem)?;
            write!(w, "> into {}, {}, {}", vop.dst, vop.lhs, vop.rhs)?;
        }
        Inst::Fence { ord } => {
            write!(w, "fence {}", ord.name())?;
        }
        Inst::Copy { dst, src, t: _, vm_size } => {
            write!(w, "copy {} {}, src {}", vm_size, dst, src)?;
        }
        Inst::StructOffset { struct_t, base, field_index, vm_offset } => {
            write!(w, "struct_offset ")?;
            k1.display_pt(w, PhysicalType::agg(struct_t))?;
            write!(w, ".{}, {} ({})", field_index, base, vm_offset)?;
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            write!(w, "array_offset ")?;
            k1.display_pt(w, element_t)?;
            write!(w, " {}[{}]", base, element_index)?;
        }
        Inst::Call { call_id: id } => {
            let call = k1.ir.calls.get(id);
            write!(w, "call ")?;
            if let Some(dst) = call.dst {
                w.write_str("into ")?;
                display_value(w, &dst)?;
                w.write_str(" ")?;
            }
            k1.display_pt(w, call.ret_type)?;
            match &call.callee {
                IrCallee::BackendBuiltin(_, backend_builtin) => {
                    write!(w, " builtin {}", backend_builtin.kind_name())?;
                }
                IrCallee::Direct(function_id) => {
                    write!(w, " ")?;
                    w.write_str(k1.ident_str(k1.get_function(*function_id).name))?;
                }
                IrCallee::Indirect(_, callee_inst) => {
                    write!(w, " indirect {}", *callee_inst)?;
                }
                IrCallee::LlvmIntrinsic { name, .. } => {
                    write!(w, " llvm {}", k1.ident_str(*name))?;
                }
                IrCallee::Extern { library_name, function_name, .. } => {
                    write!(
                        w,
                        " extern {} {}",
                        k1.ident_str_opt(*library_name),
                        k1.ident_str(*function_name),
                    )?;
                }
            };
            w.write_str("(")?;
            for (index, arg) in k1.ir.mem.getn(call.args).iter().enumerate() {
                write!(w, "{}", *arg)?;
                let last = index == call.args.len() as usize - 1;
                if !last {
                    w.write_str(", ")?;
                }
            }
            w.write_str(")")?;
        }
        Inst::Jump(block_id) => {
            write!(w, "jmp b{} {}", block_id.raw_index(), k1.ir.mem.get(block_id).data.kind.str())?;
        }
        Inst::JumpIf { cond, cons, alt } => {
            write!(
                w,
                "jmpif {}, b{} {}, b{} {}",
                cond,
                cons.raw_index(),
                k1.ir.mem.get(cons).data.kind.str(),
                alt.raw_index(),
                k1.ir.mem.get(alt).data.kind.str()
            )?;
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
        }
        Inst::Phi { t, incomings } => {
            write!(w, "phi ")?;
            k1.display_pt(w, t)?;
            write!(w, " [")?;
            for (i, incoming) in k1.ir.mem.getn(incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(
                    w,
                    "(b{} {}: {})",
                    incoming.from.raw_index(),
                    k1.ir.mem.get(incoming.from).data.kind.str(),
                    incoming.value
                )?;
            }
            write!(w, "]")?;
        }
        Inst::Ret { v, agg } => {
            write!(w, "ret ")?;
            if agg {
                w.write_str("agg ")?;
            }
            display_inst_kind(w, k1, get_value_kind(&k1.ir, v))?;
            write!(w, " {}", v)?;
        }
        Inst::BoolNegate { v } => {
            write!(w, "bool not {}", v)?;
        }
        Inst::BitNot { v } => {
            write!(w, "bitnot {}", v)?;
        }
        Inst::BitCast { v, to } => {
            write!(w, "bitcast ")?;
            k1.display_pt(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntTrunc { v, to } => {
            write!(w, "trunc ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntExtU { v, to } => {
            write!(w, "int extend ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntExtS { v, from, to } => {
            write!(w, "int signed extend {}->{}", from, to)?;
            write!(w, " {}", v)?;
        }
        Inst::FloatTrunc { v, to } => {
            write!(w, "ftrunc ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::FloatExt { v, to } => {
            write!(w, "fext ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::Float32ToIntUnsigned { v, to } => {
            write!(w, "f32toint ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::Float32ToIntSigned { v, to } => {
            write!(w, "f32toint signed ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::Float64ToIntUnsigned { v, to } => {
            write!(w, "f64toint ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::Float64ToIntSigned { v, to } => {
            write!(w, "f64toint signed ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntToFloatUnsigned { v, from: _, to } => {
            write!(w, "inttofloat ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntToFloatSigned { v, from: _, to } => {
            write!(w, "inttofloat signed ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::PtrToWord { v } => {
            write!(w, "ptrtoint {}", v)?;
        }
        Inst::WordToPtr { v } => {
            write!(w, "inttoptr {}", v)?;
        }
        Inst::IntAdd { lhs, rhs, width } => {
            write!(w, "add i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntSub { lhs, rhs, width } => {
            write!(w, "sub i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntMul { lhs, rhs, width } => {
            write!(w, "mul i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntDivUnsigned { lhs, rhs, width } => {
            write!(w, "udiv i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntDivSigned { lhs, rhs, width } => {
            write!(w, "sdiv i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntRemUnsigned { lhs, rhs, width } => {
            write!(w, "urem i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntRemSigned { lhs, rhs, width } => {
            write!(w, "srem i{width} {} {}", lhs, rhs)?;
        }
        Inst::IntCmp { lhs, rhs, pred, width } => {
            write!(w, "icmp i{width} {} {} {}", pred, lhs, rhs)?;
        }
        Inst::FloatAdd { lhs, rhs, width } => {
            write!(w, "fadd f{width} {} {}", lhs, rhs)?;
        }
        Inst::FloatSub { lhs, rhs, width } => {
            write!(w, "fsub f{width} {} {}", lhs, rhs)?;
        }
        Inst::FloatMul { lhs, rhs, width } => {
            write!(w, "fmul f{width} {} {}", lhs, rhs)?;
        }
        Inst::FloatDiv { lhs, rhs, width } => {
            write!(w, "fdiv f{width} {} {}", lhs, rhs)?;
        }
        Inst::FloatRem { lhs, rhs, width } => {
            write!(w, "frem f{width} {} {}", lhs, rhs)?;
        }
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            write!(w, "fcmp f{width} {} {} {}", pred, lhs, rhs)?;
        }
        Inst::BitAnd { lhs, rhs, width } => {
            write!(w, "and i{width} {} {}", lhs, rhs)?;
        }
        Inst::BitOr { lhs, rhs, width } => {
            write!(w, "or i{width} {} {}", lhs, rhs)?;
        }
        Inst::BitXor { lhs, rhs, width } => {
            write!(w, "xor i{width} {} {}", lhs, rhs)?;
        }
        Inst::BitShiftLeft { lhs, rhs, width } => {
            write!(w, "shl i{width} {} {}", lhs, rhs)?;
        }
        Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
            write!(w, "lshr i{width} {} {}", lhs, rhs)?;
        }
        Inst::BitSignedShiftRight { lhs, rhs, width } => {
            write!(w, "ashr i{width} {} {}", lhs, rhs)?;
        }
        Inst::BakeStaticValue { type_id, value } => {
            write!(w, "bake ")?;
            k1.display_type_id(w, type_id, false)?;
            write!(w, " {}", value)?;
        }
    };
    Ok(())
}

pub fn display_inst_kind(
    w: &mut impl std::fmt::Write,
    k1: &TypedProgram,
    kind: InstKind,
) -> std::fmt::Result {
    match kind {
        InstKind::Value(t) => k1.display_pt(w, t),
        InstKind::Void => write!(w, "void"),
        InstKind::Terminator => write!(w, "terminator"),
    }
}

impl From<ScalarType> for &'static str {
    fn from(st: ScalarType) -> &'static str {
        match st {
            ScalarType::U8 => "u8",
            ScalarType::U16 => "u16",
            ScalarType::U32 => "u32",
            ScalarType::U64 => "u64",
            ScalarType::I8 => "i8",
            ScalarType::I16 => "i16",
            ScalarType::I32 => "i32",
            ScalarType::I64 => "i64",
            ScalarType::F32 => "f32",
            ScalarType::F64 => "f64",
            ScalarType::Pointer => "ptr",
            ScalarType::Char => "char",
            ScalarType::Bool => "bool",
        }
    }
}
pub fn display_scalar_type(w: &mut impl Write, scalar: ScalarType) -> std::fmt::Result {
    w.write_str(scalar.into())
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_scalar_type(f, *self)
    }
}

pub fn display_imm(w: &mut impl Write, imm: DataInst) -> std::fmt::Result {
    match imm {
        DataInst::U64(u64) => write!(w, "u64 {}", u64),
        DataInst::I64(i64) => write!(w, "i64 {}", i64),
        DataInst::Float(float) => write!(w, "float {}", float),
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_value(f, self)
    }
}

pub fn display_value(w: &mut impl Write, value: &Value) -> std::fmt::Result {
    match value {
        Value::Inst(inst_id) => write!(w, "i{}", inst_id.as_u32()),
        Value::GlobalAddr { id, .. } => write!(w, "g{}", id.as_u32()),
        Value::StaticValue { id, .. } => write!(w, "static{}", id.as_u32()),
        Value::FunctionAddr(function_id) => write!(w, "f{}", function_id.as_u32()),
        Value::FnParam { index, .. } => write!(w, "p{}", index),
        Value::Data32 { t, data } => write!(w, "data32({}, {})", t, data),
        Value::Empty => write!(w, "{{}}"),
    }
}
