// Copyright (c) 2026 knix
// All rights reserved.
use crate::arith::{
    self, Cmp3Op, FloatCmpPred, FloatOp, FloatUnop, IntCmpPred, IntOp, IntUnop, OverflowOp,
};
use crate::kmem::List;
use crate::parse::{self, StringId};
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::StaticValueId;
use crate::typer::trace::{FrameId, TraceKind};
use crate::unique_stack::UniqueStack;
use crate::vpool::VPool;
use crate::{SV4, debug};
use crate::{kbail, kerr, static_assert_size};
use crate::{
    kmem::{self, MSlice},
    lex::Span,
    nz_u32_id,
    typer::{types::*, *},
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
    pub source_span: Span,
}

nz_u32_id!(IrCallId);
const WORD_SIZED_INT: ScalarType = ScalarType::I64;
pub const EMPTY_PLACE_ADDR: u32 = Layout::ZERO_SIZED.align;
pub struct ProgramIr {
    pub mem: kmem::Mem<ProgramIr>,
    units: VPool<IrUnit, IrUnitIndex>,
    function_units: VPool<Option<IrUnitIndex>, FunctionId>,
    pub exprs: FxHashMap<TypedExprId, IrUnit>,
    pub module_config: IrModuleConfig,
    unit_fn_types: VPool<Option<UnitFnTypeIndex>, TypeId>,
    unit_fn_type_store: VPool<PhysicalFunctionType, UnitFnTypeIndex>,
    unit_bufs: Vec<UnitBuf>,
    compact_inst_map: Vec<Option<InstId>>,
    compact_block_map: Vec<Option<BlockId>>,
    pub insts_emitted: u64,
    pub insts_committed: u64,

    b_variables: FxHashMap<VariableId, BuilderVariable>,
    b_loops: FxHashMap<ScopeId, LoopInfo>,
    pub units_pending_compile: UniqueStack<FunctionId, Option<FrameId>>,
    pub globals_pending_eval: UniqueStack<TypedGlobalId>,

    opt_buf_visit_stack: Vec<iropt::OptVisit>,
    opt_buf_visited: FxHashSet<IrUnitId>,
    opt_buf_callees: Vec<FunctionId>,
    opt_buf_inline: iropt::InlineState,
    opt_buf_promote: Option<Box<iropt::PromoteState>>,
    opt_buf_local_uses: Vec<iropt::LocalUses>,
}

static_assert_size!(IrComment, 1);

impl ProgramIr {
    pub fn snap(&self, w: &mut crate::snap::SnapWriter) {
        use crate::snap::write_map_snap;
        let ProgramIr {
            mem,
            units,
            function_units,
            exprs,
            module_config: IrModuleConfig {},
            unit_fn_types: _,
            unit_fn_type_store: _,
            unit_bufs: _,
            compact_inst_map: _,
            compact_block_map: _,
            insts_emitted,
            insts_committed,
            b_variables: _,
            b_loops: _,
            units_pending_compile,
            globals_pending_eval,
            opt_buf_visit_stack: _,
            opt_buf_visited: _,
            opt_buf_callees: _,
            opt_buf_inline: _,
            opt_buf_promote: _,
            opt_buf_local_uses: _,
        } = self;
        w.write_section("ir");
        w.write_u64(*insts_emitted);
        w.write_u64(*insts_committed);
        mem.snap(w);
        units.snap(w);
        function_units.snap(w);
        write_map_snap(w, exprs);
        assert!(units_pending_compile.is_empty());
        assert!(globals_pending_eval.is_empty());
    }

    pub fn restore(&mut self, r: &mut crate::snap::SnapReader) {
        r.section("ir");
        self.insts_emitted = r.read_u64();
        self.insts_committed = r.read_u64();
        self.mem.restore(r);
        self.units.restore(r);
        self.function_units.restore(r);
        self.exprs = crate::snap::restore_map_snap(r);
    }

    pub fn function_unit(&self, id: FunctionId) -> Option<&IrUnit> {
        let index = self.function_units.lookup(id)?;
        Some(self.units.get(index))
    }

    pub fn function_unit_mut(&mut self, id: FunctionId) -> Option<&mut IrUnit> {
        let index = self.function_units.lookup(id)?;
        Some(self.units.get_mut(index))
    }

    pub fn insert_function_unit(&mut self, id: FunctionId, unit: IrUnit) {
        let index = self.units.add(unit);
        self.function_units.grow_and_set(id, Some(index));
    }
}

#[derive(Clone, Copy)]
pub enum IrComment {
    ArrayGetOffsetPlace,
    AssignmentStore,
    BitcastAggToAggCopy,
    BitcastAggToAggPlace,
    BitcastAggToScalar,
    BitcastScalarToAggPlace,
    BitcastScalarToAggStore,
    BreakLoop,
    CallArgUnalignedCopy,
    ContinueLoop,
    CmpxchgResult,
    DaCapoMaestro,
    DeliverFnPointer,
    DeliverSumPayload,
    DirectVariable,
    DynAbilityFnPtrOffset,
    DynAbilityStateOffset,
    DynLamEnvPtrOffset,
    DynLamFnPtrOffset,
    EnterLoop,
    EnterWhileCond,
    EnumInt,
    ExitInlinedCode,
    FieldAccessNoCopy,
    FieldAccessWCopy,
    FoldedVariable,
    FulfillBitcastDestination,
    FulfillCastDestination,
    FulfillVariableUsage,
    GetLaneLoad,
    GetLaneOffset,
    GetSumTagLoadOrCopyToDst,
    GotoWhileCond,
    InlineRet,
    InlinedAggRet,
    InlinedScalarReturn,
    LambdaEnvLocation,
    LangDerefFulfillToDst,
    LangDerefNoDst,
    LoopBreakValue,
    LoopPhi,
    MatchPhi,
    PromotedPhi,
    PromotedCopy,
    MatchResultSlot,
    MatchSwitch,
    MatchingCondCond,
    None,
    RefAtIndexOffset,
    ReturnAggregateAtAddress,
    RvoStorage,
    SourceLet,
    SplatResult,
    StaticScalar,
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
    Zeroed,
    ZeroedNoDst,
}

impl IrComment {
    pub fn str(&self) -> &'static str {
        match self {
            IrComment::ArrayGetOffsetPlace => "array get offset place",
            IrComment::AssignmentStore => "assignment store",
            IrComment::BitcastAggToAggCopy => "bitcast agg to agg copy",
            IrComment::BitcastAggToAggPlace => "bitcast agg to agg place",
            IrComment::BitcastAggToScalar => "bitcast agg to scalar",
            IrComment::BitcastScalarToAggPlace => "bitcast scalar to agg place",
            IrComment::BitcastScalarToAggStore => "bitcast scalar to agg store",
            IrComment::BreakLoop => "break loop",
            IrComment::CallArgUnalignedCopy => "call arg unaligned copy",
            IrComment::ContinueLoop => "continue loop",
            IrComment::CmpxchgResult => "cmpxchg result",
            IrComment::DaCapoMaestro => "da capo maestro",
            IrComment::DeliverFnPointer => "deliver fn pointer",
            IrComment::DeliverSumPayload => "deliver sum payload",
            IrComment::DirectVariable => "direct variable",
            IrComment::DynAbilityFnPtrOffset => "dyn ability fn ptr offset",
            IrComment::DynAbilityStateOffset => "dyn ability state offset",
            IrComment::DynLamEnvPtrOffset => "dyn lam env ptr offset",
            IrComment::DynLamFnPtrOffset => "dyn lam fn ptr offset",
            IrComment::EnterLoop => "enter loop",
            IrComment::EnterWhileCond => "enter while cond",
            IrComment::EnumInt => "enum int",
            IrComment::ExitInlinedCode => "exit inlined code",
            IrComment::FieldAccessNoCopy => "field access no copy",
            IrComment::FieldAccessWCopy => "field access w copy",
            IrComment::FoldedVariable => "folded variable",
            IrComment::FulfillBitcastDestination => "fulfill bitcast destination",
            IrComment::FulfillCastDestination => "fulfill cast destination",
            IrComment::FulfillVariableUsage => "fulfill variable usage",
            IrComment::GetLaneLoad => "get-lane load",
            IrComment::GetLaneOffset => "get-lane offset",
            IrComment::GetSumTagLoadOrCopyToDst => "get sum tag, load or copy to dst",
            IrComment::GotoWhileCond => "goto while cond",
            IrComment::InlineRet => "inline ret",
            IrComment::InlinedAggRet => "inlined agg ret",
            IrComment::InlinedScalarReturn => "inlined scalar return",
            IrComment::LambdaEnvLocation => "lambda env location",
            IrComment::LangDerefFulfillToDst => "lang deref fulfill to dst",
            IrComment::LangDerefNoDst => "lang deref no dst",
            IrComment::LoopBreakValue => "loop break value",
            IrComment::LoopPhi => "loop phi",
            IrComment::MatchPhi => "match phi",
            IrComment::PromotedPhi => "promoted phi",
            IrComment::PromotedCopy => "promoted copy",
            IrComment::MatchResultSlot => "match result slot",
            IrComment::MatchSwitch => "match switch",
            IrComment::MatchingCondCond => "matching cond cond",
            IrComment::None => "",
            IrComment::RefAtIndexOffset => "refAtIndex offset",
            IrComment::ReturnAggregateAtAddress => "return aggregate at address",
            IrComment::RvoStorage => "rvo storage",
            IrComment::SourceLet => "source let",
            IrComment::SplatResult => "splat result",
            IrComment::StaticScalar => "static scalar",
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
            IrComment::Zeroed => "zeroed",
            IrComment::ZeroedNoDst => "zeroed no dst",
        }
    }
}

nz_u32_id!(IrUnitIndex);
nz_u32_id!(UnitFnTypeIndex);

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
            units: VPool::make("ir_units"),
            function_units: VPool::make("function_units"),
            unit_fn_types: VPool::make("unit_fn_types"),
            unit_fn_type_store: VPool::make("unit_fn_type_store"),
            unit_bufs: Vec::new(),
            compact_inst_map: Vec::new(),
            compact_block_map: Vec::new(),
            insts_emitted: 0,
            insts_committed: 0,
            exprs: FxHashMap::new(),
            module_config: IrModuleConfig {},
            b_variables: FxHashMap::new(),
            b_loops: FxHashMap::default(),
            units_pending_compile: UniqueStack::new(),
            globals_pending_eval: UniqueStack::new(),
            opt_buf_visit_stack: vec![],
            opt_buf_visited: FxHashSet::default(),
            opt_buf_callees: vec![],
            opt_buf_inline: iropt::InlineState::default(),
            opt_buf_promote: None,
            opt_buf_local_uses: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BlockSourceKind {
    Entry,
    ExprToplevel,
    RequireContinue,
    RequireElse,
    ArmCond,
    ArmCons,
    MatchEnd,
    MatchSwitch,
    WhileLoopCondition,
    WhileLoopBody,
    WhileLoopEnd,
    LoopBody,
    LoopEnd,
    MatchingCondContinue,
    InlineExit,
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
            BlockSourceKind::MatchEnd => "match_end",
            BlockSourceKind::MatchSwitch => "match_switch",
            BlockSourceKind::WhileLoopCondition => "while_loop_condition",
            BlockSourceKind::WhileLoopBody => "while_loop_body",
            BlockSourceKind::WhileLoopEnd => "while_loop_end",
            BlockSourceKind::LoopBody => "loop_body",
            BlockSourceKind::LoopEnd => "loop_end",
            BlockSourceKind::MatchingCondContinue => "matching_cond_continue",
            BlockSourceKind::InlineExit => "inline_exit",
        }
    }
}

nz_u32_id!(InstId);
impl InstId {
    fn as_value(&self) -> Value {
        Value::Inst(*self)
    }
}

crate::tag_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum BackendBuiltin {
        TypeInfo,
        ImplOf,
        TestSuite,
        MakeStruct,
        MakeEither,
        MakeReference,
        MakeArray,
        MakeFn,
        MakeInstance,

        MemCopy,
        MemMove,
        MemSet,
        MemEquals,
        Exit,

        CompilerMessage,
        ReplCheckbox,
    }
}

impl BackendBuiltin {
    pub fn kind_name(&self) -> &'static str {
        match self {
            BackendBuiltin::TypeInfo => "type_info",
            BackendBuiltin::ImplOf => "impl_of",
            BackendBuiltin::TestSuite => "test_suite",
            BackendBuiltin::MakeStruct => "make_struct",
            BackendBuiltin::MakeEither => "make_either",
            BackendBuiltin::MakeReference => "make_reference",
            BackendBuiltin::MakeArray => "make_array",
            BackendBuiltin::MakeFn => "make_fn",
            BackendBuiltin::MakeInstance => "make_instance",
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

crate::tag_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AtomicOrderingIr {
        Relaxed,
        Acquire,
        Release,
        AcqRel,
        SeqCst,
    }
}

impl AtomicOrderingIr {
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

crate::tag_enum! {
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
}

impl AtomicRmwOpIr {
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

#[derive(Clone, Copy)]
pub struct AtomicCmpxchgData {
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
    ShrS,
    ShrU,
    EqLanes,
    LtLanesS,
    LtLanesU,
    ShuffleBytes,
    Shuffle { mask: StaticValueId },
    ToMask,
    ReduceOr,
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
            VecOpIr::ShrS => "shr_s",
            VecOpIr::ShrU => "shr_u",
            VecOpIr::EqLanes => "eq_lanes",
            VecOpIr::LtLanesS => "lt_lanes_s",
            VecOpIr::LtLanesU => "lt_lanes_u",
            VecOpIr::ShuffleBytes => "shuffle_bytes",
            VecOpIr::Shuffle { .. } => "shuffle",
            VecOpIr::ToMask => "to_mask",
            VecOpIr::ReduceOr => "reduce_or",
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
}

impl PhysicalFunctionType {
    const fn nil() -> PhysicalFunctionType {
        PhysicalFunctionType {
            return_type: PhysicalType::EMPTY,
            diverges: false,
            params: MSlice::empty(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum IrCallee {
    BackendBuiltin(FunctionId, BackendBuiltin),
    Direct(FunctionId),
    Indirect(PhysicalFunctionType, Value),
    Extern {
        library_name: Option<parse::StringId>,
        function_name: parse::StringId,
        function_id: FunctionId,
    },
    LlvmIntrinsic {
        name: parse::StringId,
        function_id: FunctionId,
    },
}

fn add_call(b: &mut Builder, call: IrCall) -> IrCallId {
    if let Some(function_id) = call.callee.known_function_id()
        && b.k1.ir.function_unit(function_id).is_none()
    {
        let requester = b.k1.trace.top();
        b.k1.ir.units_pending_compile.push(function_id, requester);
    }
    b.u.add_call(call)
}

impl IrCallee {
    fn known_function_id(&self) -> Option<FunctionId> {
        match self {
            IrCallee::Direct(fid) => Some(*fid),
            IrCallee::Extern { function_id, .. } => Some(*function_id),
            IrCallee::LlvmIntrinsic { function_id, .. } => Some(*function_id),
            IrCallee::BackendBuiltin(fid, _) => Some(*fid),
            IrCallee::Indirect(..) => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct PhiCase {
    pub from: BlockId,
    pub value: Value,
}

#[derive(Clone, Copy)]
pub struct SwitchCase {
    pub value: u64,
    pub target: BlockId,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Inst(InstId),
    GlobalAddr { storage_pt: PhysicalType, id: TypedGlobalId },
    StaticValue { t: PhysicalType, id: StaticValueId },
    FunctionAddr(FunctionId),
    FnParam { t: PhysicalType, index: u32 },

    Data32 { t: ScalarType, data: u32 },
    IsStatic,
    Empty,
}

impl Value {
    const fn imm32(t: ScalarType, u32: u32) -> Value {
        Value::Data32 { t, data: u32 }
    }

    pub const fn zero(t: ScalarType) -> Value {
        Value::imm32(t, 0)
    }

    pub fn const_bits(self, u: &UnitView) -> Option<u64> {
        match self {
            Value::Data32 { t, data } => Some(data32_bits(t, data)),
            Value::Inst(id) => match *u.inst(id) {
                Inst::Data { bits, .. } => Some(bits),
                _ => None,
            },
            _ => None,
        }
    }
}

pub fn data32_bits(t: ScalarType, data: u32) -> u64 {
    match t {
        ScalarType::F64 => (f32::from_bits(data) as f64).to_bits(),
        ScalarType::I64 => data as i32 as i64 as u64,
        ScalarType::F32
        | ScalarType::Pointer
        | ScalarType::I8
        | ScalarType::I16
        | ScalarType::I32 => data as u64,
    }
}

#[derive(Clone, Copy)]
pub struct IrCall {
    pub ret_type: PhysicalType,
    pub callee: IrCallee,
    pub args: IrRange<Value>,
    pub dst: Option<Value>,
}

#[derive(Clone, Copy)]
pub enum Inst {
    Data {
        t: ScalarType,
        bits: u64,
    },
    ReloadGlobalAddr {
        storage_pt: PhysicalType,
        id: TypedGlobalId,
    },

    Alloca {
        t: PhysicalType,
        vm_layout: Layout,
        returned: bool,
        debug: Option<IrDebugVariableInfo>,
    },
    Store {
        dst: Value,
        value: Value,
        volatile: bool,
        unaligned: bool,
    },
    Load {
        t: PhysicalType,
        src: Value,
        volatile: bool,
        unaligned: bool,
    },
    AtomicLoad {
        t: ScalarType,
        src: Value,
        ord: AtomicOrderingIr,
    },
    AtomicStore {
        dst: Value,
        value: Value,
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
        volatile: bool,
        unaligned: bool,
    },
    Zero {
        dst: Value,
        t: PhysicalType,
        vm_size: u32,
        unaligned: bool,
    },
    StructOffset {
        struct_t: AggregateTypeId,
        base: Value,
        field_index: u32,
        vm_offset: u32,
        unaligned: bool,
    },
    ArrayOffset {
        element_t: PhysicalType,
        base: Value,
        element_index: Value,
    },

    Call {
        call_id: IrCallId,
    },

    Jump(BlockId),
    JumpIf {
        cond: Value,
        cons: BlockId,
        alt: BlockId,
    },
    Switch {
        value: Value,
        cases: IrRange<SwitchCase>,
        default: BlockId,
    },
    Unreachable,
    Phi {
        t: PhysicalType,
        incomings: IrRange<PhiCase>,
    },
    Ret {
        v: Value,
    },

    Select {
        cond: Value,
        cons: Value,
        alt: Value,
        t: ScalarType,
    },
    BoolNegate {
        v: Value,
    },
    IntUnop {
        op: IntUnop,
        v: Value,
        t: ScalarType,
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
    FloatToIntUnsigned {
        v: Value,
        to: ScalarType,
    },
    FloatToIntSigned {
        v: Value,
        to: ScalarType,
    },
    IntToFloatUnsigned {
        v: Value,
        to: ScalarType,
    },
    IntToFloatSigned {
        v: Value,
        to: ScalarType,
    },
    PtrToWord {
        v: Value,
        to: ScalarType,
    },
    WordToPtr {
        v: Value,
    },
    IntBinop {
        op: IntOp,
        lhs: Value,
        rhs: Value,
        t: ScalarType,
    },
    IntOverflows {
        op: OverflowOp,
        lhs: Value,
        rhs: Value,
    },
    IntCmp {
        lhs: Value,
        rhs: Value,
        pred: IntCmpPred,
    },
    IntCmp3 {
        op: Cmp3Op,
        lhs: Value,
        rhs: Value,
    },
    FloatBinop {
        op: FloatOp,
        lhs: Value,
        rhs: Value,
        t: ScalarType,
    },
    FloatUnop {
        op: FloatUnop,
        v: Value,
        t: ScalarType,
    },
    FloatFma {
        a: Value,
        b: Value,
        c: Value,
        t: ScalarType,
    },
    FloatCmp {
        lhs: Value,
        rhs: Value,
        pred: FloatCmpPred,
    },

    BakeStaticValue {
        type_id: TypeId,
        value: Value,
    },
}

impl Inst {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Inst::Jump(_)
                | Inst::JumpIf { .. }
                | Inst::Switch { .. }
                | Inst::Unreachable
                | Inst::Ret { .. }
        )
    }

    fn is_phi(&self) -> bool {
        matches!(self, Inst::Phi { .. })
    }
}

pub fn visit_inst_values(u: &UnitView, inst: &Inst, f: &mut impl FnMut(Value)) {
    match *inst {
        Inst::Data { .. }
        | Inst::ReloadGlobalAddr { .. }
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
            let cas = *u.cmpxchg(id);
            f(cas.dst);
            f(cas.expected);
            f(cas.desired);
            f(cas.result);
        }
        Inst::VecOp { id } => {
            let vop = *u.vec_op(id);
            f(vop.dst);
            f(vop.lhs);
            f(vop.rhs);
        }
        Inst::Copy { dst, src, .. } => {
            f(dst);
            f(src);
        }
        Inst::Zero { dst, .. } => f(dst),
        Inst::StructOffset { base, .. } => f(base),
        Inst::ArrayOffset { base, element_index, .. } => {
            f(base);
            f(element_index);
        }
        Inst::Call { call_id } => {
            let call = *u.call(call_id);
            if let IrCallee::Indirect(_, v) = call.callee {
                f(v);
            }
            if let Some(dst) = call.dst {
                f(dst);
            }
            for arg in u.args(call.args) {
                f(*arg);
            }
        }
        Inst::JumpIf { cond, .. } => f(cond),
        Inst::Switch { value, .. } => f(value),
        Inst::Phi { incomings, .. } => {
            for case in u.phi_cases(incomings) {
                f(case.value);
            }
        }
        Inst::Ret { v, .. } => f(v),
        Inst::BakeStaticValue { value, .. } => f(value),
        Inst::Select { cond, cons, alt, .. } | Inst::FloatFma { a: cond, b: cons, c: alt, .. } => {
            f(cond);
            f(cons);
            f(alt);
        }
        Inst::BoolNegate { v }
        | Inst::IntUnop { v, .. }
        | Inst::FloatUnop { v, .. }
        | Inst::BitCast { v, .. }
        | Inst::IntTrunc { v, .. }
        | Inst::IntExtU { v, .. }
        | Inst::IntExtS { v, .. }
        | Inst::FloatTrunc { v, .. }
        | Inst::FloatExt { v, .. }
        | Inst::FloatToIntUnsigned { v, .. }
        | Inst::FloatToIntSigned { v, .. }
        | Inst::IntToFloatUnsigned { v, .. }
        | Inst::IntToFloatSigned { v, .. }
        | Inst::PtrToWord { v, .. }
        | Inst::WordToPtr { v } => f(v),
        Inst::IntBinop { lhs, rhs, .. }
        | Inst::IntOverflows { lhs, rhs, .. }
        | Inst::IntCmp { lhs, rhs, .. }
        | Inst::IntCmp3 { lhs, rhs, .. }
        | Inst::FloatBinop { lhs, rhs, .. }
        | Inst::FloatCmp { lhs, rhs, .. } => {
            f(lhs);
            f(rhs);
        }
    }
}

pub fn count_uses(u: &UnitView, out: &mut IdMap<InstId, u32>) {
    out.clear();
    for b in u.block_ids() {
        for id in u.block_insts(b) {
            visit_inst_values(u, u.inst(id), &mut |v| {
                if let Value::Inst(id) = v {
                    out.insert(id, out.get(id).unwrap_or(0) + 1);
                }
            });
        }
    }
}

static_assert_size!(Inst, 40);

pub fn get_value_kind(u: &UnitView, value: Value) -> InstKind {
    match value {
        Value::Inst(inst_id) => get_inst_kind(u, inst_id),
        Value::GlobalAddr { storage_pt: _, id: _ } => InstKind::PTR,
        Value::StaticValue { t, id: _ } => InstKind::Value(t),
        Value::FunctionAddr(_) => InstKind::PTR,
        Value::FnParam { t, .. } => InstKind::Value(t),
        Value::Data32 { t: scalar_type, data: _ } => {
            InstKind::Value(PhysicalType::scalar(scalar_type))
        }
        Value::IsStatic => InstKind::BOOL,
        Value::Empty => InstKind::Value(PhysicalType::EMPTY),
    }
}

pub fn switch_target(cases: &[SwitchCase], default: BlockId, width: u8, bits: u64) -> BlockId {
    let bits = bits & arith::width_mask(width);
    for case in cases {
        if case.value == bits {
            return case.target;
        }
    }
    default
}

pub fn addr_root(u: &UnitView, mut v: Value) -> Value {
    loop {
        let Value::Inst(inst_id) = v else { return v };
        match *u.inst(inst_id) {
            Inst::StructOffset { base, .. } | Inst::ArrayOffset { base, .. } => v = base,
            _ => return v,
        }
    }
}

pub fn is_addr_unaligned(u: &UnitView, mut v: Value) -> bool {
    loop {
        let Value::Inst(inst_id) = v else { return false };
        match *u.inst(inst_id) {
            Inst::StructOffset { unaligned, .. } => return unaligned,
            Inst::ArrayOffset { base, .. } => v = base,
            _ => return false,
        }
    }
}

pub fn get_inst_kind(u: &UnitView, inst_id: InstId) -> InstKind {
    match *u.inst(inst_id) {
        Inst::Data { t, .. } => InstKind::scalar(t),
        Inst::Alloca { .. } | Inst::ReloadGlobalAddr { .. } => InstKind::PTR,
        Inst::Store { .. } => InstKind::Void,
        Inst::Load { t, .. } => InstKind::Value(t),
        Inst::AtomicLoad { t, .. } => InstKind::scalar(t),
        Inst::AtomicStore { .. } => InstKind::Void,
        Inst::AtomicRmw { t, .. } => InstKind::scalar(t),
        Inst::AtomicCmpxchg { .. } => InstKind::Void,
        Inst::VecOp { id } => match u.vec_op(id).op {
            VecOpIr::ToMask => InstKind::I64,
            VecOpIr::ReduceOr => InstKind::scalar(u.vec_op(id).elem),
            _ => InstKind::Void,
        },
        Inst::Fence { .. } => InstKind::Void,
        Inst::Copy { .. } | Inst::Zero { .. } => InstKind::Void,
        Inst::StructOffset { .. } => InstKind::PTR,
        Inst::ArrayOffset { .. } => InstKind::PTR,
        Inst::Call { call_id: id } => InstKind::Value(u.call(id).ret_type),
        Inst::Jump(_) => InstKind::Terminator,
        Inst::JumpIf { .. } => InstKind::Terminator,
        Inst::Switch { .. } => InstKind::Terminator,
        Inst::Unreachable => InstKind::Terminator,
        Inst::Phi { t, .. } => InstKind::Value(t),
        Inst::Ret { .. } => InstKind::Terminator,
        Inst::Select { t, .. } => InstKind::scalar(t),
        Inst::BoolNegate { .. } => InstKind::BOOL,
        Inst::IntUnop { t, .. } => InstKind::scalar(t),
        Inst::BitCast { to, .. } => InstKind::Value(to),
        Inst::IntTrunc { to, .. } => InstKind::scalar(to),
        Inst::IntExtU { to, .. } => InstKind::scalar(to),
        Inst::IntExtS { to, .. } => InstKind::scalar(to),
        Inst::FloatTrunc { to, .. } => InstKind::scalar(to),
        Inst::FloatExt { to, .. } => InstKind::scalar(to),
        Inst::FloatToIntUnsigned { to, .. } => InstKind::scalar(to),
        Inst::FloatToIntSigned { to, .. } => InstKind::scalar(to),
        Inst::IntToFloatUnsigned { to, .. } => InstKind::scalar(to),
        Inst::IntToFloatSigned { to, .. } => InstKind::scalar(to),
        Inst::PtrToWord { to, .. } => InstKind::scalar(to),
        Inst::WordToPtr { .. } => InstKind::PTR,
        Inst::IntBinop { t, .. } => InstKind::scalar(t),
        Inst::IntOverflows { .. } => InstKind::BOOL,
        Inst::IntCmp { .. } => InstKind::BOOL,
        Inst::IntCmp3 { .. } => InstKind::scalar(ScalarType::I8),
        Inst::FloatBinop { t, .. } => InstKind::scalar(t),
        Inst::FloatUnop { t, .. } => InstKind::scalar(t),
        Inst::FloatFma { t, .. } => InstKind::scalar(t),
        Inst::FloatCmp { .. } => InstKind::BOOL,
        Inst::BakeStaticValue { .. } => InstKind::I64,
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
    pub const BOOL: InstKind = Self::scalar(ScalarType::I8);
    pub const PTR: InstKind = Self::scalar(ScalarType::Pointer);
    pub const I64: InstKind = Self::scalar(ScalarType::I64);

    pub const fn scalar(st: ScalarType) -> InstKind {
        InstKind::Value(PhysicalType::scalar(st))
    }

    fn is_ptr(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_ptr())
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
    pub fn expect_scalar(&self) -> ScalarType {
        match self {
            InstKind::Value(t) => t.expect_scalar(),
            _ => panic!("Expected scalar value, got {}", self.kind_name()),
        }
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

pub fn compile_function(
    k1: &mut TypedProgram,
    function_id: FunctionId,
    requester_frame: Option<FrameId>,
) -> K1Result<()> {
    if k1.ir.function_unit(function_id).is_some() {
        return Ok(());
    }
    if k1.trace.stack_contains_key(TraceKind::IrLower, function_id.as_u32()) {
        kbail!(
            k1,
            k1.get_function_span(function_id),
            "Function {} needs its own compiled ir while it is being compiled",
            k1.function_id_to_string(function_id, false)
        );
    }
    let frame =
        k1.trace_push_unit(TraceKind::IrLower, IrUnitId::Function(function_id), requester_frame);
    let span = k1.get_function_span(function_id);
    let result = match k1.require_function_body(function_id, span) {
        Ok(()) => compile_function_body(k1, function_id),
        Err(e) => Err(e),
    };
    k1.trace_pop(frame);
    result
}

fn compile_function_body(k1: &mut TypedProgram, function_id: FunctionId) -> K1Result<()> {
    let mut u = k1.ir.take_unit_buf();
    let result = compile_function_body_into(k1, &mut u, function_id);
    k1.ir.release_unit_buf(u);
    result
}

fn compile_function_body_into(
    k1: &mut TypedProgram,
    u: &mut UnitBuf,
    function_id: FunctionId,
) -> K1Result<()> {
    let mut b = Builder::new(k1, u);

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
    let is_debug = f.compiler_debug();
    let fn_span = b.k1.ast.get_span_for_id(f.parsed_id);
    b.cur_span = fn_span;
    b.entry_span = fn_span;

    let fn_params = f.params;
    let phys_fn_type = b.get_unit_fn_type(f.type_id)?;
    b.fn_type = phys_fn_type;
    let mut non_empty_index = 0;
    for param in b.k1.mem.getn(fn_params).iter() {
        let v = b.k1.variables.get(param.variable_id);
        let t = b.get_physical_type(v.type_id)?;

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
        if f.builtin_type.is_none() && !f.linkage.is_external() {
            b.k1.ice_span(b.k1.get_function_span(function_id), "ir: function has no body")
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
    Ok(())
}

pub fn compile_top_level_expr(
    k1: &mut TypedProgram,
    expr: TypedExprId,
    is_debug: bool,
) -> K1Result<()> {
    let frame = k1.trace_push_unit(TraceKind::IrLower, IrUnitId::Expr(expr), None);
    let result = compile_top_level_expr_body(k1, expr, is_debug);
    k1.trace_pop(frame);
    result
}

fn compile_top_level_expr_body(
    k1: &mut TypedProgram,
    expr: TypedExprId,
    is_debug: bool,
) -> K1Result<()> {
    let mut u = k1.ir.take_unit_buf();
    let result = compile_top_level_expr_into(k1, &mut u, expr, is_debug);
    k1.ir.release_unit_buf(u);
    result
}

fn compile_top_level_expr_into(
    k1: &mut TypedProgram,
    u: &mut UnitBuf,
    expr: TypedExprId,
    is_debug: bool,
) -> K1Result<()> {
    let mut b = Builder::new(k1, u);
    let entry_block = b.push_block(BlockSourceKind::ExprToplevel);
    b.goto_block(entry_block);

    let return_type_id = b.k1.exprs.get_type(expr);
    let (return_type, diverges) = b.get_function_return_type(return_type_id)?;
    let params = MSlice::empty();
    let phys_fn_type = PhysicalFunctionType { return_type, diverges, params };
    b.fn_type = phys_fn_type;

    debug!("Compiling expr {}", b.k1.expr_to_string(expr));
    let _result = compile_expr(&mut b, None, expr)?;
    let unit_id = IrUnitId::Expr(expr);
    finalize_unit(&mut b, return_type_id, unit_id, phys_fn_type, is_debug, None)?;

    if is_debug {
        let s = unit_to_string(b.k1, unit_id, true);
        eprintln!("{s}");
    }
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
    let mut unit = IrUnit::new(result_type_id, unit_id, fn_type, builtin_kind, is_debug);
    b.k1.ir.insts_emitted += b.u.inst_count() as u64;
    iropt::remove_unreachable_blocks(b.k1, b.u);
    if let Some(mut local_uses) = b.local_uses.take() {
        if local_uses.has_allocas() {
            iropt::promote_unit(b.k1, b.u, unit_id, Some(&mut local_uses));
        }
        b.k1.ir.opt_buf_local_uses.push(local_uses);
    }
    iropt::merge_blocks(b.k1, b.u);
    commit_unit(&mut b.k1.ir, b.u, &mut unit);
    b.k1.ir.insts_committed += unit.insts.len() as u64;
    match unit_id {
        IrUnitId::Function(function_id) => {
            b.k1.ir.insert_function_unit(function_id, unit);
        }
        IrUnitId::Expr(expr) => {
            b.k1.ir.exprs.insert(expr, unit);
        }
    }

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
    break_join: LoopBreak,
    end_block: BlockId,
    continue_block: BlockId,
}

#[derive(Clone)]
enum LoopBreak {
    None,
    Slot(Value),
    Phi(SV4<PhiCase>),
}

pub struct Builder<'k1> {
    k1: &'k1 mut TypedProgram,
    u: &'k1 mut UnitBuf,

    fn_type: PhysicalFunctionType,

    returned_alloca: Option<InstId>,
    cur_block: BlockId,
    cur_span: Span,
    entry_span: Span,
    local_uses: Option<iropt::LocalUses>,
}

impl<'k1> Builder<'k1> {
    fn new(k1: &'k1 mut TypedProgram, u: &'k1 mut UnitBuf) -> Self {
        let local_uses = if k1.optimize_ir() {
            let mut local_uses = k1.ir.opt_buf_local_uses.pop().unwrap_or_default();
            local_uses.clear();
            Some(local_uses)
        } else {
            None
        };
        Self {
            k1,
            u,

            fn_type: PhysicalFunctionType::nil(),

            returned_alloca: None,
            cur_block: BlockId::PENDING,
            cur_span: Span::NONE,
            entry_span: Span::NONE,
            local_uses,
        }
    }

    fn make_inst(&mut self, inst: Inst, comment: IrComment) -> InstId {
        self.u.new_inst(inst, self.cur_span, comment)
    }

    fn push_alloca(&mut self, pt: PhysicalType, comment: IrComment) -> InstId {
        self.push_alloca_ext(pt, comment, None, false)
    }

    fn push_alloca_ext(
        &mut self,
        pt: PhysicalType,
        comment: IrComment,
        debug: Option<IrDebugVariableInfo>,
        returned: bool,
    ) -> InstId {
        let layout = self.k1.get_pt_layout(pt);
        let alloca_span = self.entry_span;
        let inst_id = self.u.new_inst(
            Inst::Alloca { t: pt, vm_layout: layout, returned, debug },
            alloca_span,
            comment,
        );
        self.link_alloca(inst_id);
        inst_id
    }

    fn link_alloca(&mut self, inst_id: InstId) {
        let entry = self.u.body.first_block.unwrap();
        match self.u.body.last_alloca {
            None => self.u.push_inst_front(entry, inst_id),
            Some(last_alloca) => self.u.insert_inst_after(last_alloca, inst_id),
        }
        self.u.body.last_alloca = Some(inst_id);
        let inst = *self.u.inst(inst_id);
        self.record_local_uses(&inst, inst_id, entry);
    }

    fn record_local_uses(&mut self, inst: &Inst, id: InstId, block: BlockId) {
        if let Some(local_uses) = &mut self.local_uses {
            local_uses.record_inst(self.k1, self.u, id, inst, block);
        }
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.u.view(), inst)
    }

    pub fn get_value_kind(&self, value: Value) -> InstKind {
        get_value_kind(&self.u.view(), value)
    }

    fn diverges(&self, value: Value) -> bool {
        matches!(value, Value::Inst(id) if self.u.inst(id).is_terminator())
    }

    fn is_addr_unaligned(&self, value: Value) -> bool {
        is_addr_unaligned(&self.u.view(), value)
    }

    fn push_inst_front(&mut self, inst: Inst, comment: IrComment) -> InstId {
        let id = self.make_inst(inst, comment);
        self.u.push_inst_front(self.cur_block, id);
        self.record_local_uses(&inst, id, self.cur_block);
        id
    }

    fn push_inst(&mut self, inst: Inst, comment: IrComment) -> InstId {
        let id = self.make_inst(inst, comment);
        self.u.push_inst(self.cur_block, id);
        self.record_local_uses(&inst, id, self.cur_block);
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
        let agg_type = self.k1.agg_types.get(struct_agg_id).agg_type;
        if matches!(agg_type, AggType::Union { .. }) {
            return base;
        }
        let base_unaligned = self.is_addr_unaligned(base);
        let unaligned = base_unaligned || agg_type.is_packed_struct();
        if field_index == 0 && unaligned == base_unaligned {
            return base;
        }
        let Some(offset) = self.k1.get_struct_field_offset(struct_agg_id, field_index) else {
            b_ice!(self, "Failed getting offset for field")
        };
        self.push_inst(
            Inst::StructOffset {
                struct_t: struct_agg_id,
                base,
                field_index,
                vm_offset: offset,
                unaligned,
            },
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
        if cons == alt {
            return self.push_jump(cons, comment);
        }
        self.push_inst(Inst::JumpIf { cond, cons, alt }, comment)
    }

    fn known_bits(&self, v: Value) -> Option<u64> {
        if self.k1.optimize_ir() { v.const_bits(&self.u.view()) } else { None }
    }

    fn known_bool(&self, v: Value) -> Option<bool> {
        self.known_bits(v).map(|bits| bits != 0)
    }

    fn push_switch(
        &mut self,
        value: Value,
        cases: &[SwitchCase],
        default: BlockId,
        comment: IrComment,
    ) -> InstId {
        if let Some(bits) = self.known_bits(value) {
            let width = self.get_value_kind(value).expect_scalar().width_bits();
            return self.push_jump(switch_target(cases, default, width, bits), comment);
        }
        let cases = self.u.push_switch_cases(cases);
        self.push_inst(Inst::Switch { value, cases, default }, comment)
    }

    fn is_live(&self, block_id: BlockId) -> bool {
        self.u.body.first_block == Some(block_id) || self.u.has_preds(block_id)
    }

    fn push_unreachable_if_dead(&mut self) -> Option<Value> {
        if self.is_live(self.cur_block) {
            None
        } else {
            Some(self.push_inst_anon(Inst::Unreachable).as_value())
        }
    }

    fn push_copy(
        &mut self,
        dst: Value,
        src: Value,
        pt: PhysicalType,
        comment: IrComment,
    ) -> Option<InstId> {
        self.push_copy_ext(dst, src, pt, false, false, comment)
    }

    fn push_copy_ext(
        &mut self,
        dst: Value,
        src: Value,
        pt: PhysicalType,
        forced_unaligned: bool,
        volatile: bool,
        comment: IrComment,
    ) -> Option<InstId> {
        let layout = self.k1.get_pt_layout(pt);
        if pt.is_empty() {
            None
        } else {
            debug_assert!(pt.is_agg(), "scalars move as load and store, not copy");
            let unaligned =
                forced_unaligned || self.is_addr_unaligned(dst) || self.is_addr_unaligned(src);
            let copy_inst = self.push_inst(
                Inst::Copy { dst, src, t: pt, vm_size: layout.size, volatile, unaligned },
                comment,
            );
            Some(copy_inst)
        }
    }

    fn push_load(&mut self, t: PhysicalType, src: Value, comment: IrComment) -> InstId {
        self.push_load_ext(t, src, false, false, comment)
    }

    fn push_load_ext(
        &mut self,
        t: PhysicalType,
        src: Value,
        forced_unaligned: bool,
        volatile: bool,
        comment: IrComment,
    ) -> InstId {
        let unaligned = forced_unaligned || self.is_addr_unaligned(src);
        self.push_inst(Inst::Load { t, src, volatile, unaligned }, comment)
    }

    fn push_store(&mut self, dst: Value, value: Value, comment: IrComment) -> InstId {
        self.push_store_ext(dst, value, false, false, comment)
    }

    fn push_store_ext(
        &mut self,
        dst: Value,
        value: Value,
        forced_unaligned: bool,
        volatile: bool,
        comment: IrComment,
    ) -> InstId {
        let unaligned = forced_unaligned || self.is_addr_unaligned(dst);
        self.push_inst(Inst::Store { dst, value, volatile, unaligned }, comment)
    }

    fn push_const(&mut self, t: ScalarType, bits: u64, comment: IrComment) -> Value {
        let data32 = match t {
            ScalarType::F64 => (f64::from_bits(bits) as f32).to_bits(),
            _ => bits as u32,
        };
        if data32_bits(t, data32) == bits {
            return Value::imm32(t, data32);
        }
        self.push_inst(Inst::Data { t, bits }, comment).as_value()
    }

    fn push_value(&mut self, inst: Inst, comment: IrComment) -> Value {
        if self.k1.optimize_ir() {
            match fold::fold_inst(&self.u.view(), &inst) {
                Some(fold::Simplified::Const(t, bits)) => return self.push_const(t, bits, comment),
                Some(fold::Simplified::Value(v)) => return v,
                None => {}
            }
        }
        self.push_inst(inst, comment).as_value()
    }

    fn push_block(&mut self, kind: BlockSourceKind) -> BlockId {
        self.u.add_block(kind)
    }

    fn push_block_after_current(&mut self, kind: BlockSourceKind) -> BlockId {
        self.u.insert_block_after(self.cur_block, kind)
    }

    fn goto_block(&mut self, block_id: BlockId) {
        self.cur_block = block_id;
    }

    fn get_variable(&self, variable_id: VariableId) -> Option<&BuilderVariable> {
        self.k1.ir.b_variables.get(&variable_id)
    }

    fn get_physical_type_result(&mut self, type_id: TypeId) -> PhysicalTypeResult {
        self.k1.get_physical_type(type_id)
    }

    fn get_physical_type(&mut self, type_id: TypeId) -> K1Result<PhysicalType> {
        match self.get_physical_type_result(type_id) {
            PhysicalTypeResult::No => Err(kerr!(
                self.k1,
                self.cur_span,
                "cannot lower this type to a physical type: {}",
                self.k1.type_id_to_string_ext(type_id, dump::TypeDisplayMode::Expand)
            )),
            PhysicalTypeResult::Infinite => Err(kerr!(
                self.k1,
                self.cur_span,
                "cannot lower this infinite type to a physical type: {}",
                self.k1.type_id_to_string_ext(type_id, dump::TypeDisplayMode::Expand)
            )),
            PhysicalTypeResult::Yes(pt) => Ok(pt),
        }
    }

    fn type_to_inst_kind(&mut self, type_id: TypeId) -> K1Result<InstKind> {
        if type_id == NEVER_TYPE_ID {
            Ok(InstKind::Terminator)
        } else {
            let t = self.get_physical_type(type_id)?;
            Ok(InstKind::Value(t))
        }
    }

    fn get_unit_fn_type(&mut self, type_id: TypeId) -> K1Result<PhysicalFunctionType> {
        if let Some(index) = self.k1.ir.unit_fn_types.lookup(type_id) {
            return Ok(*self.k1.ir.unit_fn_type_store.get(index));
        }
        let function_type = *self.k1.types.get(type_id).expect_function();
        let (return_type, diverges) = self.get_function_return_type(function_type.return_type)?;

        let mut phys_params = self.k1.ir.mem.new_list(function_type.physical_params.len());
        for (index, param) in self.k1.mem.getn(function_type.physical_params).iter().enumerate() {
            let pt = self.get_physical_type(param.type_id)?;
            if pt.is_empty() {
                continue;
            }
            if index >= u16::MAX as usize {
                b_ice!(self, "Too many parameters; max is {}", u16::MAX);
            }
            phys_params.push(PhysicalFunctionParam { original_index: Some(index as u16), pt })
        }
        let fn_ty = PhysicalFunctionType { params: phys_params.to_slice(), diverges, return_type };

        let index = self.k1.ir.unit_fn_type_store.add(fn_ty);
        self.k1.ir.unit_fn_types.grow_and_set(type_id, Some(index));
        Ok(fn_ty)
    }

    fn get_function_return_type(
        &mut self,
        return_type_id: TypeId,
    ) -> K1Result<(PhysicalType, bool)> {
        if return_type_id == NEVER_TYPE_ID {
            Ok((PhysicalType::EMPTY, true))
        } else {
            let t = self.get_physical_type(return_type_id)?;
            Ok((t, false))
        }
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

fn build_value_join(
    b: &mut Builder,
    pt: PhysicalType,
    incomings: &[PhiCase],
    comment: IrComment,
) -> Value {
    debug_assert!(pt.is_scalar());
    if incomings.is_empty() {
        b.push_inst_anon(Inst::Unreachable).as_value()
    } else if incomings.len() == 1 && b.k1.optimize_ir() {
        incomings[0].value
    } else {
        let incomings = b.u.push_phi_cases(incomings);
        b.push_inst(Inst::Phi { t: pt, incomings }, comment).as_value()
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
        if let Some(unreachable) = b.push_unreachable_if_dead() {
            return Ok(Some(unreachable));
        }
        let is_last = index == statements.len() as usize - 1;
        let stmt_dst = if is_last { dst } else { None };
        last_ret = Some(compile_stmt(b, stmt_dst, stmt)?);
    }
    if !last_ret.is_some_and(|v| b.diverges(v))
        && let Some(unreachable) = b.push_unreachable_if_dead()
    {
        return Ok(Some(unreachable));
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

            let pt = b.get_physical_type(let_stmt.variable_type)?;

            let typed_var = b.k1.variables.get(let_stmt.variable_id);
            let returned = typed_var.is_returned();
            let is_ssa_value = pt.is_scalar()
                && !typed_var.is_address_taken()
                && !typed_var.is_reassigned()
                && b.k1.optimize_ir();
            let debug_info = Some(IrDebugVariableInfo {
                name: typed_var.name,
                original_type_id: let_stmt.variable_type,
                user_hidden: typed_var.is_user_hidden(),
                source_span: b.cur_span,
            });

            if pt.is_empty() || is_ssa_value {
                let value = if pt.is_empty() {
                    match let_stmt.initializer {
                        None => Value::Empty,
                        Some(init) => {
                            compile_expr(b, None, init)?;
                            Value::Empty
                        }
                    }
                } else {
                    match let_stmt.initializer {
                        None => Value::Empty,
                        Some(init) => compile_expr(b, None, init)?,
                    }
                };
                b.k1.ir.b_variables.insert(
                    let_stmt.variable_id,
                    BuilderVariable { id: let_stmt.variable_id, value, pt, indirect: false },
                );
            } else {
                let variable_alloca =
                    b.push_alloca_ext(pt, IrComment::SourceLet, debug_info, returned);

                if let Some(init) = let_stmt.initializer {
                    compile_expr(b, Some(variable_alloca.as_value()), init)?;
                }
                let is_direct = pt.is_agg();
                b.k1.ir.b_variables.insert(
                    let_stmt.variable_id,
                    BuilderVariable {
                        id: let_stmt.variable_id,
                        value: variable_alloca.as_value(),
                        pt,
                        indirect: !is_direct,
                    },
                );
            }
            Ok(Value::Empty)
        }
        TypedStmt::Assignment(ass) => {
            let ass = *ass;
            let (addr, frozen) = compile_expr_place(b, ass.destination)?;
            debug_assert!(!frozen);
            store_assignment_value(b, addr, ass.value)?;
            Ok(Value::Empty)
        }
        TypedStmt::Require(req) => {
            let req = req.clone();
            let require_else_block = match req.else_body {
                None => None,
                Some(_) => Some(b.push_block(BlockSourceKind::RequireElse)),
            };

            let continue_block = compile_matching_condition(
                b,
                &req.condition,
                SuccessTarget::Create(BlockSourceKind::RequireContinue),
                require_else_block,
            )?;

            if let Some(else_body) = req.else_body
                && b.is_live(require_else_block.unwrap())
            {
                b.goto_block(require_else_block.unwrap());
                compile_expr(b, None, else_body)?;
            }

            match continue_block {
                Some(continue_block) => b.goto_block(continue_block),
                None => {
                    let dead = b.push_block(BlockSourceKind::RequireContinue);
                    b.goto_block(dead);
                }
            }

            Ok(Value::Empty)
        }
    }
}

fn store_assignment_value(b: &mut Builder, addr: Value, value: TypedExprId) -> K1Result<()> {
    let rhs = compile_expr(b, None, value)?;
    if b.diverges(rhs) {
        return Ok(());
    }
    let pt = b.get_physical_type(b.k1.exprs.get_type(value))?;
    store_value(b, pt, addr, rhs, IrComment::AssignmentStore);
    Ok(())
}

fn compile_expr(b: &mut Builder, dst: Option<Value>, expr: TypedExprId) -> K1Result<Value> {
    let prev_span = b.cur_span;
    let expr_span = b.k1.exprs.get_span(expr);
    b.cur_span = expr_span;
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);
    let e = b.k1.exprs.get(expr).clone();
    let expr_type = b.k1.exprs.get_type(expr);
    let dst = if expr_type == NEVER_TYPE_ID { None } else { dst };
    debug!("compiling {} {}", e.kind_name(), b.k1.expr_to_string(expr));
    match e {
        TypedExpr::Struct(struct_literal) => {
            let struct_type_id = expr_type;
            let struct_pt = b.get_physical_type(struct_type_id)?;
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
                    None => {}
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
            let result_type = b.get_physical_type(expr_type)?;
            let needs_copy = !frozen;
            let result = build_field_access(b, dst, field_ptr, result_type, needs_copy);
            Ok(result)
        }
        TypedExpr::ArrayGetElement(_) => {
            let (element_ptr, frozen) = compile_expr_place(b, expr)?;
            let result_type = b.get_physical_type(expr_type)?;
            let needs_copy = !frozen;
            let result = build_field_access(b, dst, element_ptr, result_type, needs_copy);
            Ok(result)
        }
        TypedExpr::Variable(variable_expr) => {
            let var_result = compile_variable_to_address(b, variable_expr.variable_id, false)?;
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
                        let stored = store_rich_if_dst(b, dst, pt, addr, IrComment::DirectVariable);
                        Ok(stored)
                    }
                }
            }
        }
        TypedExpr::AddressOf(address_of) => {
            let (place, _frozen) = compile_expr_place(b, address_of.target_expr)?;
            let addr = get_place_address(place);
            let stored = store_scalar_if_dst(b, dst, addr);
            Ok(stored)
        }
        TypedExpr::Deref(_) => {
            let (src, frozen) = compile_expr_place(b, expr)?;
            let target_pt = b.get_physical_type(expr_type)?;
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
        TypedExpr::ScalarOp(scalar_op) => compile_scalar_op(b, scalar_op, dst),
        TypedExpr::Call { call_id } => {
            let call = b.k1.calls.get(call_id).clone();

            let function_type_id = b.k1.get_callee_function_type(&call.callee);
            let callee_fn_type = b.get_unit_fn_type(function_type_id)?;

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

            if let Some(Linkage::External { lib_name, link_name, .. }) = linkage {
                let function_id = maybe_function_id.unwrap();
                let function_name = match link_name {
                    None => b.k1.get_function(function_id).name,
                    Some(link_name) => link_name,
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
                        let lambda_env = compile_expr(b, None, *lambda_value_expr)?;
                        let lambda_env_type_id = b.k1.exprs.get_type(*lambda_value_expr);
                        let env_pt = b.get_physical_type(lambda_env_type_id)?;
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
                    Callee::Builtin { function_id, .. } => {
                        kbail!(
                            b.k1,
                            b.cur_span,
                            "ir builtin callee: {}",
                            b.k1.ident_str(b.k1.get_function(*function_id).name)
                        );
                    }
                    Callee::DynamicLambda(dl) => {
                        let lambda_obj = compile_expr(b, None, *dl)?;
                        let lam_obj_type_id = b.k1.exprs.get_type(*dl);
                        let lam_obj_pt = b.get_physical_type(lam_obj_type_id).unwrap().expect_agg();
                        let ptr_pt = PhysicalType::PTR;
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
                    Callee::DynamicAbilityFn { object_expr, slot_index, slot_function_type } => {
                        let object = compile_expr(b, None, *object_expr)?;
                        let object_type_id = b.k1.exprs.get_type(*object_expr);
                        let object_pt = b.get_physical_type(object_type_id).unwrap().expect_agg();
                        let ptr_pt = PhysicalType::PTR;
                        let table_index = TypedProgram::ABILITY_OBJECT_TABLE_INDEX;
                        let table_addr = b.push_struct_offset(
                            object_pt,
                            object,
                            table_index as u32,
                            IrComment::None,
                        );
                        let object_fields = b.k1.get_agg_struct_layout(object_pt);
                        let table_pt =
                            b.k1.mem.get_nth(object_fields, table_index).field_t.expect_agg();
                        let fn_ptr_addr = b.push_struct_offset(
                            table_pt,
                            table_addr,
                            *slot_index,
                            IrComment::DynAbilityFnPtrOffset,
                        );
                        let fn_ptr = load_value(b, ptr_pt, fn_ptr_addr, false, IrComment::None);
                        callee = IrCallee::Indirect(callee_fn_type, fn_ptr);

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

            let mut args =
                b.k1.tmp.new_list(call.args.len() + environment_arg.iter().count() as u32);

            if let Some(environment_arg) = environment_arg {
                args.push(environment_arg)
            }
            let env_param_count = environment_arg.iter().count();

            for (arg_index, arg) in b.k1.mem.getn(call.args).iter().enumerate() {
                let param_index = (arg_index + env_param_count) as u16;
                let phys_param =
                    b.k1.ir
                        .mem
                        .getn(callee_fn_type.params)
                        .iter()
                        .find(|p| p.original_index == Some(param_index))
                        .copied();

                let value = match phys_param {
                    Some(param) if param.pt.is_agg() => {
                        let (place, _frozen) = compile_expr_place(b, *arg)?;
                        let unaligned = b.is_addr_unaligned(place);
                        load_value(b, param.pt, place, unaligned, IrComment::CallArgUnalignedCopy)
                    }
                    _ => compile_expr(b, None, *arg)?,
                };

                if phys_param.is_some() {
                    args.push(value);
                }
            }
            debug_assert_eq!(callee_fn_type.params.len(), args.len() as u32);
            let args_handle = b.u.push_args(args.as_slice());
            let ret_type = callee_fn_type.return_type;
            let sret_dst = if ret_type.is_agg() { dst } else { None };
            let ir_call = IrCall { ret_type, callee, args: args_handle, dst: sret_dst };
            let result = if let IrCallee::Direct(function_id) = callee
                && b.k1.get_function(function_id).is_inline()
            {
                iropt::compile_inline_call(b, function_id, ir_call)?
            } else {
                let call_id = add_call(b, ir_call);
                let call_inst_id = b.push_inst_anon(Inst::Call { call_id });
                if callee_fn_type.diverges {
                    None
                } else {
                    Some(sret_dst.unwrap_or(call_inst_id.as_value()))
                }
            };
            let Some(result) = result else {
                return Ok(b.push_inst_anon(Inst::Unreachable).as_value());
            };
            if ret_type.is_agg() {
                Ok(result)
            } else {
                Ok(store_rich_if_dst(b, dst, ret_type, result, IrComment::StoreScalarToDst))
            }
        }
        TypedExpr::Match(match_expr) => {
            let match_result_type = expr_type;
            let result_inst_kind = b.type_to_inst_kind(match_result_type)?;
            if let Some(stmt) = match_expr.subject_defn {
                compile_stmt(b, None, stmt)?;
            }
            if let InstKind::Value(pt) = result_inst_kind
                && let Some(t) = pt.as_scalar()
                && let Some(selected) = compile_match_as_select(b, &match_expr, t)?
            {
                return Ok(store_scalar_if_dst(b, dst, selected));
            }

            let arms = b.k1.mem.getn(match_expr.arms);
            let arm_count = arms.len();
            debug_assert!(arms.last().is_none_or(|last| {
                last.case.is_none()
                    && b.k1
                        .mem
                        .getn(last.condition.instrs)
                        .iter()
                        .all(|i| matches!(i, MatchingConditionInstr::Binding { .. }))
            }));
            let match_end_block = if result_inst_kind.is_terminator() {
                None
            } else {
                Some(b.push_block(BlockSourceKind::MatchEnd))
            };
            let result_is_empty = matches!(result_inst_kind, InstKind::Value(pt) if pt.is_empty());
            let mut arm_targets: List<Option<BlockId>, MemTmp> =
                b.k1.tmp.new_list(arm_count as u32);
            let mut arm_blocks = b.k1.tmp.new_list(arm_count as u32);
            for (index, arm) in arms.iter().enumerate() {
                let target = match b.k1.get_expr_trivial_exit(arm.consequent_expr) {
                    Some(TrivialExit::Fallthrough) if result_is_empty => match_end_block,
                    Some(TrivialExit::Break(scope)) => {
                        Some(b.k1.ir.b_loops.get(&scope).unwrap().end_block)
                    }
                    Some(TrivialExit::Continue(scope)) => {
                        Some(b.k1.ir.b_loops.get(&scope).unwrap().continue_block)
                    }
                    _ => None,
                };
                arm_targets.push(target);
                let blockless = target.is_some() && !condition_can_branch(b.k1, &arm.condition);
                if index == 0 && arm.case.is_none() {
                    arm_blocks.push(b.cur_block);
                } else if blockless {
                    arm_blocks.push(target.unwrap());
                } else if condition_can_branch(b.k1, &arm.condition) {
                    arm_blocks.push(b.push_block(BlockSourceKind::ArmCond));
                } else {
                    arm_blocks.push(b.push_block(BlockSourceKind::ArmCons));
                }
            }

            let scrutinee = match match_expr.scrutinee {
                None => None,
                Some(scrutinee_expr) => Some(compile_expr(b, None, scrutinee_expr)?),
            };

            let mut entries = b.k1.tmp.new_list(arm_count as u32);
            for (index, arm) in arms.iter().enumerate() {
                let starts_run =
                    arm.case.is_some() && (index == 0 || arms[index - 1].case.is_none());
                let entry = if index == 0 {
                    b.cur_block
                } else if starts_run {
                    b.push_block(BlockSourceKind::MatchSwitch)
                } else {
                    arm_blocks[index]
                };
                entries.push(entry);
            }

            let result_slot = match (dst, result_inst_kind.as_value()) {
                (Some(dst), _) => Some(dst),
                (None, Some(pt)) if pt.is_agg() => {
                    Some(b.push_alloca(pt, IrComment::MatchResultSlot).as_value())
                }
                (None, _) => None,
            };
            let mut incomings = b.k1.tmp.new_list(arm_count as u32);
            let mut fail_targets: List<Option<BlockId>, MemTmp> =
                b.k1.tmp.new_list(arm_count as u32);
            let mut last_arm_result: Option<Value> = None;
            for (index, arm) in arms.iter().enumerate() {
                let starts_run =
                    arm.case.is_some() && (index == 0 || arms[index - 1].case.is_none());
                if arm.case.is_none() {
                    let fail_target =
                        if index + 1 < arm_count { Some(entries[index + 1]) } else { None };
                    fail_targets.push(fail_target);
                } else if starts_run {
                    let mut run_end = index;
                    while arms[run_end].case.is_some() {
                        run_end += 1;
                    }
                    let scrutinee_value = scrutinee.unwrap();
                    let mut cases = b.k1.tmp.new_list((run_end - index) as u32);
                    for k in index..run_end {
                        let case = arms[k].case;
                        let seen = arms[index..k].iter().any(|a| a.case == case);
                        if !seen {
                            let (_, value) =
                                b.k1.static_values
                                    .get(case.unwrap())
                                    .scalar_bits()
                                    .expect("switch case is a scalar");
                            cases.push(SwitchCase { value, target: arm_blocks[k] });
                        }
                        let next_same = arms[k + 1..run_end].iter().position(|a| a.case == case);
                        let fail_target = match next_same {
                            Some(offset) => arm_blocks[k + 1 + offset],
                            None => entries[run_end],
                        };
                        fail_targets.push(Some(fail_target));
                    }
                    if b.is_live(entries[index]) {
                        b.goto_block(entries[index]);
                        b.push_switch(
                            scrutinee_value,
                            cases.as_slice(),
                            entries[run_end],
                            IrComment::MatchSwitch,
                        );
                    }
                }

                let arm_block = arm_blocks[index];
                if let Some(target) = arm_targets[index]
                    && !condition_can_branch(b.k1, &arm.condition)
                {
                    if index == 0 && arm.case.is_none() {
                        b.goto_block(arm_block);
                        b.push_jump(target, IrComment::None);
                    }
                    continue;
                }
                if !b.is_live(arm_block) {
                    continue;
                }
                b.goto_block(arm_block);
                if let Some(target) = arm_targets[index] {
                    compile_matching_condition(
                        b,
                        &arm.condition,
                        SuccessTarget::Existing(target),
                        fail_targets[index],
                    )?;
                    continue;
                }
                let Some(arm_cons_block) = compile_matching_condition(
                    b,
                    &arm.condition,
                    SuccessTarget::Create(BlockSourceKind::ArmCons),
                    fail_targets[index],
                )?
                else {
                    continue;
                };

                b.goto_block(arm_cons_block);
                let arm_result = compile_expr(b, result_slot, arm.consequent_expr)?;
                last_arm_result = Some(arm_result);
                if !b.diverges(arm_result) {
                    if result_slot.is_none() {
                        incomings.push(PhiCase { from: b.cur_block, value: arm_result });
                    }
                    b.push_jump(match_end_block.unwrap(), IrComment::None);
                }
            }

            let Some(match_end_block) = match_end_block else {
                return Ok(match last_arm_result {
                    Some(result) => result,
                    None => {
                        let dead = b.push_block(BlockSourceKind::MatchEnd);
                        b.goto_block(dead);
                        b.push_inst_anon(Inst::Unreachable).as_value()
                    }
                });
            };
            b.goto_block(match_end_block);
            match result_inst_kind {
                InstKind::Value(pt) => match result_slot {
                    Some(slot) => Ok(slot),
                    None if pt.is_empty() => Ok(Value::Empty),
                    None => Ok(build_value_join(b, pt, incomings.as_slice(), IrComment::MatchPhi)),
                },
                InstKind::Void => Err(kerr!(b.k1, b.cur_span, "match result void")),
                InstKind::Terminator => unreachable!(),
            }
        }
        TypedExpr::WhileLoop(w) => {
            let cond_block = b.push_block(BlockSourceKind::WhileLoopCondition);
            let end_block = b.push_block(BlockSourceKind::WhileLoopEnd);
            let TypedExpr::Block(body_block) = b.k1.exprs.get(w.body) else { unreachable!() };
            let loop_scope_id = body_block.scope_id;
            b.k1.ir.b_loops.insert(
                loop_scope_id,
                LoopInfo { break_join: LoopBreak::None, end_block, continue_block: cond_block },
            );

            b.push_jump(cond_block, IrComment::EnterWhileCond);

            b.goto_block(cond_block);
            let body_block = compile_matching_condition(
                b,
                &w.condition,
                SuccessTarget::Create(BlockSourceKind::WhileLoopBody),
                Some(end_block),
            )?;

            if let Some(body_block) = body_block {
                b.goto_block(body_block);
                let last = compile_block_stmts(b, None, w.body)?;
                if last.is_some_and(|v| !b.diverges(v)) {
                    b.push_jump(cond_block, IrComment::GotoWhileCond);
                }
            }

            b.goto_block(end_block);
            Ok(Value::Empty)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            let loop_body_block = b.push_block(BlockSourceKind::LoopBody);
            let loop_end_block = b.push_block(BlockSourceKind::LoopEnd);

            let break_pt = if expr_type == NEVER_TYPE_ID || expr_type == b.k1.builtin_types.empty {
                None
            } else {
                Some(b.get_physical_type(expr_type)?)
            };
            let break_join = match break_pt {
                None => LoopBreak::None,
                Some(pt) if pt.is_agg() => LoopBreak::Slot(match dst {
                    Some(dst) => dst,
                    None => b.push_alloca(pt, IrComment::LoopBreakValue).as_value(),
                }),
                Some(_) => LoopBreak::Phi(smallvec::smallvec![]),
            };
            let TypedExpr::Block(body_block) = b.k1.exprs.get(loop_expr.body_block) else {
                unreachable!()
            };
            let body_scope_id = body_block.scope_id;
            b.k1.ir.b_loops.insert(
                body_scope_id,
                LoopInfo { break_join, end_block: loop_end_block, continue_block: loop_body_block },
            );

            b.push_jump(loop_body_block, IrComment::EnterLoop);
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if body_value.is_some_and(|v| !b.diverges(v)) {
                b.push_jump(loop_body_block, IrComment::DaCapoMaestro);
            }

            b.goto_block(loop_end_block);
            if expr_type == NEVER_TYPE_ID {
                return Ok(b.push_inst_anon(Inst::Unreachable).as_value());
            }
            match b.k1.ir.b_loops.remove(&body_scope_id).unwrap().break_join {
                LoopBreak::None => Ok(Value::Empty),
                LoopBreak::Slot(slot) => Ok(slot),
                LoopBreak::Phi(incomings) => {
                    let value =
                        build_value_join(b, break_pt.unwrap(), &incomings, IrComment::LoopPhi);
                    Ok(store_scalar_if_dst(b, dst, value))
                }
            }
        }
        TypedExpr::Break(brk) => {
            let loop_info = b.k1.ir.b_loops.get(&brk.loop_scope).unwrap();
            let end_block = loop_info.end_block;
            let slot = match loop_info.break_join {
                LoopBreak::Slot(slot) => Some(slot),
                LoopBreak::None | LoopBreak::Phi(_) => None,
            };
            let value = compile_expr(b, slot, brk.value)?;
            if b.diverges(value) {
                return Ok(value);
            }
            let from = b.cur_block;
            if let LoopBreak::Phi(incomings) =
                &mut b.k1.ir.b_loops.get_mut(&brk.loop_scope).unwrap().break_join
            {
                incomings.push(PhiCase { from, value });
            }
            let jmp = b.push_jump(end_block, IrComment::BreakLoop);
            Ok(jmp.as_value())
        }
        TypedExpr::Continue { loop_scope } => {
            let continue_block = b.k1.ir.b_loops.get(&loop_scope).unwrap().continue_block;
            let jmp = b.push_jump(continue_block, IrComment::ContinueLoop);
            Ok(jmp.as_value())
        }
        TypedExpr::SumConstructor(sum_c) => {
            let sum_pt = b.get_physical_type(expr_type)?;
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
            let (tag_t, tag_bits) = sum_variant.tag.scalar_bits();
            let int_imm = b.push_const(tag_t, tag_bits, IrComment::SumTag);
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
            let sum_type_id = b.k1.exprs.get_type(sum_get_tag.sum_expr);
            let sum_pt = b.get_physical_type(sum_type_id)?;
            let tag_pt = b.k1.agg_types.get(sum_pt.expect_agg()).agg_type.expect_sum().tag_pt;

            Ok(load_or_copy(b, tag_pt, dst, sum_base, false, IrComment::GetSumTagLoadOrCopyToDst))
        }
        TypedExpr::SumGetPayload(_sum_get_payload) => {
            let (payload_place, frozen) = compile_expr_place(b, expr)?;
            let result_type = b.get_physical_type(expr_type)?;
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
            let Type::Enum(enum_type) = b.k1.types.get(expr_type) else { unreachable!() };
            let value = b.k1.mem.get_nth(enum_type.member_values, e.value_index as usize);
            let (t, bits) = value.int_value.scalar_bits();
            let value = b.push_const(t, bits, IrComment::EnumInt);
            let stored = store_scalar_if_dst(b, dst, value);
            Ok(stored)
        }
        TypedExpr::EnumGetValue(get_value) => {
            let value = compile_expr(b, dst, get_value.enum_expr)?;
            Ok(value)
        }
        TypedExpr::Cast(c) => compile_cast(b, dst, &c, expr),
        TypedExpr::Return(typed_return) => {
            let return_pt = b.fn_type.return_type;
            let dst = match typed_return.returned_variable {
                None if return_pt.is_agg() => match b.returned_alloca {
                    Some(inst_id) => Some(inst_id.as_value()),
                    None => {
                        let rvo_storage =
                            b.push_alloca_ext(return_pt, IrComment::RvoStorage, None, true);
                        b.returned_alloca = Some(rvo_storage);
                        Some(rvo_storage.as_value())
                    }
                },
                _ => None,
            };
            let value = compile_expr(b, dst, typed_return.value)?;

            let returned_value = if return_pt.is_empty() { Value::Empty } else { value };
            let ret = b.push_inst(
                Inst::Ret { v: returned_value },
                if return_pt.is_agg() {
                    IrComment::ReturnAggregateAtAddress
                } else {
                    IrComment::None
                },
            );
            Ok(ret.as_value())
        }
        TypedExpr::Lambda(lam_expr) => {
            let lambda_type_id = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            let l = b.k1.lambda_types.get(lambda_type_id);
            let function_id = l.function_id;
            let env_struct = l.environment_struct;
            let requester = b.k1.trace.top();
            b.k1.ir.units_pending_compile.push(function_id, requester);
            compile_expr(b, dst, env_struct)
        }
        TypedExpr::FunctionIdReference(_) => {
            let stored =
                store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None);
            Ok(stored)
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = Value::FunctionAddr(fpe.function_id);
            let ptr_pt = PhysicalType::PTR;
            let stored = store_rich_if_dst(b, dst, ptr_pt, fp, IrComment::DeliverFnPointer);
            let requester = b.k1.trace.top();
            b.k1.ir.units_pending_compile.push(fpe.function_id, requester);
            Ok(stored)
        }
        TypedExpr::StaticValue(stat) => {
            if !stat.is_typed_as_static {
                if let StaticValue::Zero(type_id) = *b.k1.static_values.get(stat.value_id) {
                    return compile_zero(b, type_id, dst);
                }
            }
            let t = b.get_physical_type(expr_type)?;
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
            let struct_pt = b.get_physical_type(struct_type)?;
            let (base_ptr, frozen) = compile_expr_place(b, field_access.base_struct)?;
            if struct_pt.is_empty() {
                return Ok((base_ptr, frozen));
            }
            let struct_pt_id = struct_pt.expect_agg();
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
            let array_pt = b.get_physical_type(array_type)?;
            let index = compile_expr(b, None, array_get.index)?;
            if array_pt.is_empty() {
                return Ok((get_place_address(array_base), frozen));
            }
            let (element_pt, _len) =
                b.k1.agg_types.get(array_pt.expect_agg()).agg_type.expect_array();
            let element_ptr = b.push_inst(
                Inst::ArrayOffset { element_t: element_pt, base: array_base, element_index: index },
                IrComment::ArrayGetOffsetPlace,
            );
            Ok((element_ptr.as_value(), frozen))
        }
        TypedExpr::Variable(variable_expr) => {
            let CompileVariableResult::Address { addr, constant, .. } =
                compile_variable_to_address(b, variable_expr.variable_id, true)?
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

fn get_place_address(place: Value) -> Value {
    match place {
        Value::Empty => Value::imm32(ScalarType::Pointer, EMPTY_PLACE_ADDR),
        place => place,
    }
}

fn compile_zero(b: &mut Builder, type_id: TypeId, dst: Option<Value>) -> K1Result<Value> {
    let pt = b.get_physical_type(type_id)?;
    match pt.as_enum() {
        PhysicalTypeEnum::Empty => Ok(Value::Empty),
        PhysicalTypeEnum::Agg(agg_id) => {
            let pt_layout = b.k1.agg_types.get(agg_id).layout;
            let dst = match dst {
                None => b.push_alloca(pt, IrComment::ZeroedNoDst).as_value(),
                Some(dst) => dst,
            };
            let unaligned = b.is_addr_unaligned(dst);
            b.push_inst(
                Inst::Zero { dst, t: pt, vm_size: pt_layout.size, unaligned },
                IrComment::Zeroed,
            );
            Ok(dst)
        }
        PhysicalTypeEnum::Scalar(st) => {
            let stored = store_scalar_if_dst(b, dst, Value::zero(st));
            Ok(stored)
        }
    }
}

fn compile_static_value(b: &mut Builder, value_id: StaticValueId, pt: PhysicalType) -> Value {
    match b.k1.static_values.get(value_id) {
        StaticValue::Empty(_) => Value::Empty,
        scalar @ (StaticValue::Bool(_)
        | StaticValue::Char(_)
        | StaticValue::Int(_)
        | StaticValue::Enum(..)
        | StaticValue::Float(_)) => {
            let (t, bits) = scalar.scalar_bits().unwrap();
            b.push_const(t, bits, IrComment::StaticScalar)
        }
        StaticValue::FunctionPointer(_, function_id) => Value::FunctionAddr(*function_id),
        StaticValue::String(_)
        | StaticValue::Zero(_)
        | StaticValue::Struct(_)
        | StaticValue::Sum(_)
        | StaticValue::LinearContainer(_)
        | StaticValue::RawContainer(_) => {
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
    require_address: bool,
) -> K1Result<CompileVariableResult> {
    let variable = b.k1.variables.get(variable_id);
    match variable.global_id() {
        Some(global_id) => {
            let global = b.k1.globals.get(global_id).clone();
            if global.initial_value.is_pending() {
                b.k1.ir.globals_pending_eval.push(global_id, ());
            }

            let value_type = variable.type_id;
            let is_constant = global.is_constant;
            let value_pt = b.get_physical_type(value_type)?;

            if global_id == GLOBAL_ID_K1_IS_STATIC && !require_address {
                return Ok(CompileVariableResult::FoldedValue {
                    value: Value::IsStatic,
                    pt: value_pt,
                });
            }

            if let Some(initial_value) = global.initial_value.as_value()
                && global.is_constant
                && global.reload_ns.is_none()
                && value_pt.is_scalar()
                && !require_address
                && b.k1.optimize_ir()
            {
                let value = compile_static_value(b, initial_value, value_pt);
                let folded_value = match value {
                    Value::Inst(_) => Some(value),
                    Value::GlobalAddr { .. } => unreachable!(),
                    Value::StaticValue { .. } => None,
                    Value::FunctionAddr(_) => Some(value),
                    Value::FnParam { .. } => unreachable!(),
                    Value::IsStatic => unreachable!(),
                    Value::Data32 { .. } => Some(value),
                    Value::Empty => Some(value),
                };
                if let Some(value) = folded_value {
                    return Ok(CompileVariableResult::FoldedValue { value, pt: value_pt });
                }
            }

            {
                let addr = if global.reload_ns.is_some() {
                    b.push_inst_anon(Inst::ReloadGlobalAddr { storage_pt: value_pt, id: global_id })
                        .as_value()
                } else {
                    Value::GlobalAddr { storage_pt: value_pt, id: global_id }
                };
                let is_direct = value_pt.is_agg();
                Ok(CompileVariableResult::Address {
                    addr,
                    pt: value_pt,
                    indirect: !is_direct,
                    constant: is_constant,
                })
            }
        }
        None => {
            let (value, pt, indirect) = match b.get_variable(variable_id) {
                Some(var) => (var.value, var.pt, var.indirect),
                None => {
                    let type_id = b.k1.variables.get(variable_id).type_id;
                    let Some(value_id) =
                        b.k1.types.get(type_id).as_value_type().and_then(|v| v.value_id)
                    else {
                        let mut variables = String::new();
                        for (idx, bv) in b.k1.ir.b_variables.values().enumerate() {
                            if idx > 0 {
                                variables.push('\n');
                            }
                            write!(variables, "{} ", bv.id).unwrap();
                            display_value(&mut variables, b.k1, &b.u.view(), bv.value).unwrap();
                        }
                        eprintln!("Variables are: {}", variables);
                        b.k1.ice_span(b.cur_span, "Missing variable")
                    };
                    let pt = b.get_physical_type(type_id)?;
                    let value = compile_static_value(b, value_id, pt);
                    (value, pt, !pt.is_agg() && matches!(value, Value::StaticValue { .. }))
                }
            };
            if require_address && pt.is_scalar() && !indirect {
                b.k1.ice_span(b.cur_span, "Address required for a variable bound to a value")
            }
            Ok(CompileVariableResult::Address { addr: value, pt, indirect, constant: false })
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
        BuiltinIr::Unreachable => Ok(b.push_inst_anon(Inst::Unreachable).as_value()),
        BuiltinIr::BakeStaticValue => {
            let type_id = b.k1.get_type_slice(call.type_args)[0];
            let _physical_type = b.get_physical_type(type_id);

            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let value = compile_expr(b, None, arg0)?;
            let bake = b.push_inst_anon(Inst::BakeStaticValue { type_id, value });

            let stored = store_rich_if_dst(
                b,
                dst,
                callee_fn_type.return_type,
                bake.as_value(),
                IrComment::None,
            );
            Ok(stored)
        }
        BuiltinIr::Scalar(_) => b_ice!(b, "scalar op builtins are typed as ScalarOp, not calls"),
        BuiltinIr::IntOverflow(op) => {
            let ret_pt = callee_fn_type.return_type;
            let PhysicalTypeEnum::Agg(agg_id) = ret_pt.as_enum() else {
                b_ice!(b, "overflow result must be an aggregate");
            };
            let t = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt.expect_scalar();
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let rhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let value =
                b.push_value(Inst::IntBinop { op: op.wrapping_op(), lhs, rhs, t }, IrComment::None);
            let overflowed = b.push_value(Inst::IntOverflows { op, lhs, rhs }, IrComment::None);
            let result = match dst {
                None => b.push_alloca(ret_pt, IrComment::None).as_value(),
                Some(dst) => dst,
            };
            let value_addr = b.push_struct_offset(agg_id, result, 0, IrComment::None);
            b.push_store(value_addr, value, IrComment::None);
            let overflowed_addr = b.push_struct_offset(agg_id, result, 1, IrComment::None);
            b.push_store(overflowed_addr, overflowed, IrComment::None);
            Ok(result)
        }
        BuiltinIr::Bitcast => {
            let type_args = b.k1.get_type_slice(call.type_args);
            let (from_type_id, to_type_id) = (type_args[0], type_args[1]);

            let from_pt = b.get_physical_type(from_type_id)?;
            let to_pt = b.get_physical_type(to_type_id)?;

            let arg0 = *b.k1.mem.get_nth(call.args, 0);
            let from_value = compile_expr(b, None, arg0)?;
            match (from_pt.as_enum(), to_pt.as_enum()) {
                (PhysicalTypeEnum::Empty, _) | (_, PhysicalTypeEnum::Empty) => {
                    Err(kerr!(b.k1, b.cur_span, "Cannot bitcast to or from empty type"))
                }
                (PhysicalTypeEnum::Scalar(from_st), PhysicalTypeEnum::Scalar(to_st)) => {
                    let is_ptr = |st: ScalarType| st == ScalarType::Pointer;
                    let value = match (is_ptr(from_st), is_ptr(to_st)) {
                        (true, false) if to_st.is_word_int() => b
                            .push_inst_anon(Inst::PtrToWord { v: from_value, to: to_st })
                            .as_value(),
                        (true, false) => {
                            let word = b
                                .push_inst_anon(Inst::PtrToWord {
                                    v: from_value,
                                    to: WORD_SIZED_INT,
                                })
                                .as_value();
                            b.push_value(Inst::BitCast { v: word, to: to_pt }, IrComment::None)
                        }
                        (false, true) => {
                            let word = if from_st.is_word_int() {
                                from_value
                            } else {
                                let word_pt = PhysicalType::scalar(WORD_SIZED_INT);
                                b.push_value(
                                    Inst::BitCast { v: from_value, to: word_pt },
                                    IrComment::None,
                                )
                            };
                            b.push_inst_anon(Inst::WordToPtr { v: word }).as_value()
                        }
                        (true, true) => from_value,
                        (false, false) => b.push_value(
                            Inst::BitCast { v: from_value, to: to_pt },
                            IrComment::None,
                        ),
                    };
                    let stored = store_rich_if_dst(
                        b,
                        dst,
                        to_pt,
                        value,
                        IrComment::FulfillBitcastDestination,
                    );
                    Ok(stored)
                }
                (PhysicalTypeEnum::Scalar(_), PhysicalTypeEnum::Agg(_)) => {
                    let unaligned =
                        b.k1.get_pt_layout(to_pt).align < b.k1.get_pt_layout(from_pt).align;
                    let locn = match dst {
                        Some(dst) => dst,
                        None => b.push_alloca(to_pt, IrComment::BitcastScalarToAggPlace).as_value(),
                    };

                    let _stored = b.push_store_ext(
                        locn,
                        from_value,
                        unaligned,
                        false,
                        IrComment::BitcastScalarToAggStore,
                    );
                    Ok(locn)
                }
                (PhysicalTypeEnum::Agg(_), PhysicalTypeEnum::Scalar(_)) => {
                    let unaligned =
                        b.k1.get_pt_layout(from_pt).align < b.k1.get_pt_layout(to_pt).align;
                    let loaded = b.push_load_ext(
                        to_pt,
                        from_value,
                        unaligned,
                        false,
                        IrComment::BitcastAggToScalar,
                    );
                    Ok(store_scalar_if_dst(b, dst, loaded.as_value()))
                }
                (PhysicalTypeEnum::Agg(_), PhysicalTypeEnum::Agg(_)) => {
                    let unaligned =
                        b.k1.get_pt_layout(to_pt).align < b.k1.get_pt_layout(from_pt).align;
                    let locn = match dst {
                        Some(dst) => dst,
                        None => b.push_alloca(to_pt, IrComment::BitcastAggToAggPlace).as_value(),
                    };
                    let _copied = b.push_copy_ext(
                        locn,
                        from_value,
                        from_pt,
                        unaligned,
                        false,
                        IrComment::BitcastAggToAggCopy,
                    );
                    Ok(locn)
                }
            }
        }
        BuiltinIr::PointerIndex => {
            let elem_type_id = b.k1.get_type_slice(call.type_args)[0];
            let elem_pt = b.get_physical_type(elem_type_id)?;
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
        BuiltinIr::VolatileLoad => {
            let load_type_id = b.k1.get_type_slice(call.type_args)[0];
            let t = b.get_physical_type(load_type_id)?;
            if t.is_empty() {
                return Ok(Value::Empty);
            }
            let src = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            match t.as_enum() {
                PhysicalTypeEnum::Scalar(_) => {
                    let loaded = b.push_load_ext(t, src, false, true, IrComment::None);
                    Ok(store_scalar_if_dst(b, dst, loaded.as_value()))
                }
                PhysicalTypeEnum::Agg(_) => {
                    let result =
                        dst.unwrap_or_else(|| b.push_alloca(t, IrComment::None).as_value());
                    b.push_copy_ext(result, src, t, false, true, IrComment::None);
                    Ok(result)
                }
                PhysicalTypeEnum::Empty => unreachable!(),
            }
        }
        BuiltinIr::VolatileStore => {
            let store_type_id = b.k1.get_type_slice(call.type_args)[0];
            let t = b.get_physical_type(store_type_id)?;
            if !t.is_empty() {
                let store_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
                let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
                if t.is_agg() {
                    b.push_copy_ext(store_dst, value, t, false, true, IrComment::None);
                } else {
                    b.push_store_ext(store_dst, value, false, true, IrComment::None);
                }
            }
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        BuiltinIr::AtomicLoad => {
            let type_id = b.k1.get_type_slice(call.type_args)[0];
            let t = check_atomic_scalar_type(b, type_id, true)?;
            let ord = b.k1.atomic_ordering_arg(&call, 1)?;
            let src = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let inst = b.push_inst_anon(Inst::AtomicLoad { t, src, ord });
            Ok(store_scalar_if_dst(b, dst, inst.as_value()))
        }
        BuiltinIr::AtomicStore => {
            let type_id = b.k1.get_type_slice(call.type_args)[0];
            check_atomic_scalar_type(b, type_id, true)?;
            let ord = b.k1.atomic_ordering_arg(&call, 2)?;
            let store_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            b.push_inst_anon(Inst::AtomicStore { dst: store_dst, value, ord });
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        BuiltinIr::AtomicRmw(op) => {
            use crate::typer::AtomicRmwOp as Op;
            let allow_pointer = op == Op::Xchg;
            let type_id = b.k1.get_type_slice(call.type_args)[0];
            let t = check_atomic_scalar_type(b, type_id, allow_pointer)?;
            let op = match op {
                Op::Xchg => AtomicRmwOpIr::Xchg,
                Op::Add => AtomicRmwOpIr::Add,
                Op::Sub => AtomicRmwOpIr::Sub,
                Op::And => AtomicRmwOpIr::And,
                Op::Or => AtomicRmwOpIr::Or,
                Op::Xor => AtomicRmwOpIr::Xor,
                Op::Min | Op::Max => {
                    let signed = b.k1.types.get(type_id).expect_int_type().is_signed();
                    match (op, signed) {
                        (Op::Min, false) => AtomicRmwOpIr::MinU,
                        (Op::Min, true) => AtomicRmwOpIr::MinS,
                        (_, false) => AtomicRmwOpIr::MaxU,
                        (_, true) => AtomicRmwOpIr::MaxS,
                    }
                }
            };
            let ord = b.k1.atomic_ordering_arg(&call, 2)?;
            let rmw_dst = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let operand = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let inst = b.push_inst_anon(Inst::AtomicRmw { op, t, dst: rmw_dst, operand, ord });
            Ok(store_scalar_if_dst(b, dst, inst.as_value()))
        }
        BuiltinIr::AtomicCmpxchg { weak } => {
            let type_id = b.k1.get_type_slice(call.type_args)[0];
            check_atomic_scalar_type(b, type_id, true)?;
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
            let id = b.u.add_cmpxchg(AtomicCmpxchgData {
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
            let (elem, lanes) = vector_pt_parts(b, callee_fn_type.return_type);
            let value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => {
                    b.push_alloca(callee_fn_type.return_type, IrComment::SplatResult).as_value()
                }
                Some(dst) => dst,
            };
            let id = b.u.add_vec_op(VecOpData {
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
        | VecOpKind::EqLanes
        | VecOpKind::LtLanes
        | VecOpKind::ShuffleBytes => {
            let op = match op {
                VecOpKind::Add => VecOpIr::Add,
                VecOpKind::Sub => VecOpIr::Sub,
                VecOpKind::Mul => VecOpIr::Mul,
                VecOpKind::BitAnd => VecOpIr::BitAnd,
                VecOpKind::BitOr => VecOpIr::BitOr,
                VecOpKind::Xor => VecOpIr::Xor,
                VecOpKind::EqLanes => VecOpIr::EqLanes,
                VecOpKind::ShuffleBytes => VecOpIr::ShuffleBytes,
                VecOpKind::LtLanes => {
                    let elem_type = get_callee_vector_elem_type(b, call);
                    match b.k1.types.get(elem_type) {
                        Type::Float(_) => VecOpIr::LtLanesU,
                        elem if elem.expect_int_type().is_signed() => VecOpIr::LtLanesS,
                        _ => VecOpIr::LtLanesU,
                    }
                }
                _ => unreachable!(),
            };
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt);
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let rhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecBinopResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.u.add_vec_op(VecOpData { op, elem, lanes, dst: locn, lhs, rhs });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::Shuffle => {
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt);
            let mask_expr = *b.k1.mem.get_nth(call.args, 2);
            let TypedExpr::StaticValue(mask) = b.k1.exprs.get(mask_expr) else {
                b_ice!(b, "vector/shuffle mask is not static after typecheck")
            };
            let op = VecOpIr::Shuffle { mask: mask.value_id };
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let rhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecBinopResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.u.add_vec_op(VecOpData { op, elem, lanes, dst: locn, lhs, rhs });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::BitNot => {
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt);
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecNotResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.u.add_vec_op(VecOpData {
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
            let op = match op {
                VecOpKind::ShiftLeft => VecOpIr::Shl,
                _ => {
                    let elem_type = get_callee_vector_elem_type(b, call);
                    if b.k1.types.get(elem_type).expect_int_type().is_signed() {
                        VecOpIr::ShrS
                    } else {
                        VecOpIr::ShrU
                    }
                }
            };
            let ret_pt = callee_fn_type.return_type;
            let (elem, lanes) = vector_pt_parts(b, ret_pt);
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let count = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VecShiftResult).as_value(),
                Some(dst) => dst,
            };
            let id = b.u.add_vec_op(VecOpData { op, elem, lanes, dst: locn, lhs, rhs: count });
            b.push_inst_anon(Inst::VecOp { id });
            Ok(locn)
        }
        VecOpKind::ToMask | VecOpKind::ReduceOr => {
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let (elem, lanes) = vector_pt_parts(b, vec_pt);
            let lhs = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let id = b.u.add_vec_op(VecOpData {
                op: if op == VecOpKind::ToMask { VecOpIr::ToMask } else { VecOpIr::ReduceOr },
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
            let ret_pt = callee_fn_type.return_type;
            let src = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let locn = match dst {
                None => b.push_alloca(ret_pt, IrComment::VectorLoadResult).as_value(),
                Some(dst) => dst,
            };
            b.push_copy_ext(locn, src, ret_pt, true, false, IrComment::VectorLoad);
            Ok(locn)
        }
        VecOpKind::Store => {
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let vec_value = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let dst_ptr = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            b.push_copy_ext(dst_ptr, vec_value, vec_pt, true, false, IrComment::VectorStore);
            Ok(store_rich_if_dst(b, dst, PhysicalType::EMPTY, Value::Empty, IrComment::None))
        }
        VecOpKind::GetLane => {
            let vec_pt = b.k1.ir.mem.get_nth(callee_fn_type.params, 0).pt;
            let (elem, _) = vector_pt_parts(b, vec_pt);
            let base = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 0))?;
            let element_index = compile_expr(b, None, *b.k1.mem.get_nth(call.args, 1))?;
            let offset = b.push_inst(
                Inst::ArrayOffset { element_t: PhysicalType::scalar(elem), base, element_index },
                IrComment::GetLaneOffset,
            );
            let loaded =
                b.push_load(PhysicalType::scalar(elem), offset.as_value(), IrComment::GetLaneLoad);
            Ok(store_scalar_if_dst(b, dst, loaded.as_value()))
        }
        VecOpKind::WithLane => {
            let ret_pt = callee_fn_type.return_type;
            let (elem, _) = vector_pt_parts(b, ret_pt);
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

fn get_callee_vector_elem_type(b: &mut Builder, call: &Call) -> TypeId {
    let function_type = b.k1.get_callee_function_type(&call.callee);
    let return_type = b.k1.types.get(function_type).as_function().unwrap().return_type;
    let Type::Vector(vector) = b.k1.types.get(return_type) else {
        b.k1.ice_span(b.cur_span, "vector op must return a vector")
    };
    vector.element_type
}

fn vector_pt_parts(b: &Builder, pt: PhysicalType) -> (ScalarType, u32) {
    b.k1.agg_types.get(pt.expect_agg()).agg_type.expect_vector()
}

fn check_atomic_scalar_type(
    b: &mut Builder,
    type_id: TypeId,
    allow_pointer: bool,
) -> K1Result<ScalarType> {
    let pt = b.get_physical_type(type_id)?;
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
    dst: Option<Value>,
    c: &TypedCast,
    expr_id: TypedExprId,
) -> K1Result<Value> {
    let target_type_id = b.k1.exprs.get_type(expr_id);
    match c.cast_type {
        CastType::Discard => {
            compile_expr(b, None, c.base_expr)?;
            Ok(Value::Empty)
        }
        CastType::Reinterpret => {
            let base_noop = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id)?;
            let stored =
                store_rich_if_dst(b, dst, to_pt, base_noop, IrComment::FulfillCastDestination);
            Ok(stored)
        }
        CastType::IntegerCast(IntegerCastDirection::SignExtend)
        | CastType::IntegerCast(IntegerCastDirection::ZeroExtend)
        | CastType::IntegerCast(IntegerCastDirection::Truncate) => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id)?;
            let to = to_pt.expect_scalar();
            let inst = match c.cast_type {
                CastType::IntegerCast(IntegerCastDirection::SignExtend) => {
                    Inst::IntExtS { v: base, to }
                }
                CastType::IntegerCast(IntegerCastDirection::ZeroExtend) => {
                    Inst::IntExtU { v: base, to }
                }
                CastType::IntegerCast(IntegerCastDirection::Truncate) => {
                    Inst::IntTrunc { v: base, to }
                }
                _ => unreachable!(),
            };
            let value = b.push_value(inst, IrComment::None);
            let stored = store_scalar_if_dst(b, dst, value);
            Ok(stored)
        }
        CastType::FloatExtend
        | CastType::FloatTruncate
        | CastType::FloatToUnsignedInteger
        | CastType::FloatToSignedInteger
        | CastType::IntegerUnsignedToFloat
        | CastType::IntegerSignedToFloat => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to = b.get_physical_type(target_type_id)?.expect_scalar();
            let inst = match c.cast_type {
                CastType::FloatExtend => Inst::FloatExt { v: base, to },
                CastType::FloatTruncate => Inst::FloatTrunc { v: base, to },
                CastType::FloatToUnsignedInteger => Inst::FloatToIntUnsigned { v: base, to },
                CastType::FloatToSignedInteger => Inst::FloatToIntSigned { v: base, to },
                CastType::IntegerUnsignedToFloat => Inst::IntToFloatUnsigned { v: base, to },
                CastType::IntegerSignedToFloat => Inst::IntToFloatSigned { v: base, to },
                _ => unreachable!(),
            };
            let inst = b.push_inst_anon(inst);
            let stored = store_scalar_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
    }
}

fn compile_scalar_op(b: &mut Builder, scalar_op: ScalarOp, dst: Option<Value>) -> K1Result<Value> {
    let mut operands: SV4<Value> = smallvec::smallvec![];
    for index in 0..scalar_op.operands.len() as usize {
        let operand = *b.k1.mem.get_nth(scalar_op.operands, index);
        operands.push(compile_expr(b, None, operand)?);
    }
    let lhs_type = b.k1.exprs.get_type(*b.k1.mem.get_nth(scalar_op.operands, 0));
    let t = b.get_physical_type(lhs_type)?.expect_scalar();
    let lhs = operands[0];
    let inst = match scalar_op.kind {
        ScalarOpKind::IntBinop(op) => Inst::IntBinop { op, lhs, rhs: operands[1], t },
        ScalarOpKind::IntUnop(op) => Inst::IntUnop { op, v: lhs, t },
        ScalarOpKind::IntCmp(pred) => Inst::IntCmp { lhs, rhs: operands[1], pred },
        ScalarOpKind::IntCmp3(op) => Inst::IntCmp3 { op, lhs, rhs: operands[1] },
        ScalarOpKind::BoolNegate => Inst::BoolNegate { v: lhs },
        ScalarOpKind::FloatBinop(op) => Inst::FloatBinop { op, lhs, rhs: operands[1], t },
        ScalarOpKind::FloatUnop(op) => Inst::FloatUnop { op, v: lhs, t },
        ScalarOpKind::FloatCmp(pred) => Inst::FloatCmp { lhs, rhs: operands[1], pred },
        ScalarOpKind::FloatFma => Inst::FloatFma { a: lhs, b: operands[1], c: operands[2], t },
    };
    let res = b.push_value(inst, IrComment::None);
    Ok(store_scalar_if_dst(b, dst, res))
}

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
        PhysicalTypeEnum::Scalar(_) => b.push_load(pt, src, comment).as_value(),
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
            let value = load_value(b, pt, src, false, comment);
            store_value(b, pt, dst, value, comment);
            dst
        }
        None => load_value(b, pt, src, copy_aggregates, comment),
    }
}

fn compile_int_equals(
    b: &mut Builder,
    subject: TypedExprId,
    value: StaticValueId,
) -> K1Result<Value> {
    let subject_value = compile_expr(b, None, subject)?;
    let subject_type = b.k1.exprs.get_type(subject);
    let pt = b.get_physical_type(subject_type)?;
    let rhs = compile_static_value(b, value, pt);
    Ok(b.push_value(
        Inst::IntCmp { lhs: subject_value, rhs, pred: IntCmpPred::Eq },
        IrComment::MatchingCondCond,
    ))
}

fn compile_match_as_select(
    b: &mut Builder,
    match_expr: &TypedMatchExpr,
    t: ScalarType,
) -> K1Result<Option<Value>> {
    let &[cons_arm, alt_arm] = b.k1.mem.getn(match_expr.arms) else { return Ok(None) };
    if alt_arm.case.is_some()
        || !alt_arm.condition.instrs.is_empty()
        || !can_speculate(b, cons_arm.consequent_expr)
        || !can_speculate(b, alt_arm.consequent_expr)
    {
        return Ok(None);
    }
    let instrs = b.k1.mem.getn(cons_arm.condition.instrs);
    let is_binding =
        |i: &MatchingConditionInstr| matches!(i, MatchingConditionInstr::Binding { .. });
    let cond = match (cons_arm.case, match_expr.scrutinee, instrs.split_last()) {
        (Some(case), Some(scrutinee), None) => compile_int_equals(b, scrutinee, case)?,
        (None, _, Some((test, bindings)))
            if !is_binding(test) && bindings.iter().all(is_binding) =>
        {
            for binding in bindings {
                let MatchingConditionInstr::Binding { let_stmt } = *binding else { unreachable!() };
                compile_stmt(b, None, let_stmt)?;
            }
            compile_condition_test(b, test)?
        }
        _ => return Ok(None),
    };
    let cons = compile_expr(b, None, cons_arm.consequent_expr)?;
    let alt = compile_expr(b, None, alt_arm.consequent_expr)?;
    Ok(Some(b.push_value(Inst::Select { cond, cons, alt, t }, IrComment::None)))
}

fn compile_condition_test(b: &mut Builder, test: &MatchingConditionInstr) -> K1Result<Value> {
    match *test {
        MatchingConditionInstr::Cond { value } => compile_expr(b, None, value),
        MatchingConditionInstr::IntEquals { subject, value } => {
            compile_int_equals(b, subject, value)
        }
        MatchingConditionInstr::Binding { .. } => unreachable!(),
    }
}

fn can_speculate(b: &Builder, expr: TypedExprId) -> bool {
    match b.k1.exprs.get(expr) {
        TypedExpr::StaticValue(_) | TypedExpr::Enum(_) => true,
        TypedExpr::EnumGetValue(get_value) => can_speculate(b, get_value.enum_expr),
        TypedExpr::Variable(v) => b.get_variable(v.variable_id).is_some(),
        TypedExpr::Block(block) => match b.k1.mem.getn(block.statements) {
            [stmt] => match b.k1.stmts.get(*stmt) {
                TypedStmt::Expr(inner, _) => can_speculate(b, *inner),
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn condition_can_branch(k1: &TypedProgram, mc: &MatchingCondition) -> bool {
    k1.mem
        .getn(mc.instrs)
        .iter()
        .any(|instr| !matches!(instr, MatchingConditionInstr::Binding { .. }))
}

enum SuccessTarget {
    Create(BlockSourceKind),
    Existing(BlockId),
}

fn compile_matching_condition(
    b: &mut Builder,
    mc: &MatchingCondition,
    success: SuccessTarget,
    condition_fail_block: Option<BlockId>,
) -> K1Result<Option<BlockId>> {
    let instrs = b.k1.mem.getn(mc.instrs);
    let last_branch =
        instrs.iter().rposition(|i| !matches!(i, MatchingConditionInstr::Binding { .. }));
    for (index, inst) in instrs.iter().enumerate() {
        if !b.is_live(b.cur_block) {
            return Ok(None);
        }
        if matches!(success, SuccessTarget::Existing(_)) && last_branch.is_none_or(|l| index > l) {
            break;
        }
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(b, None, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { .. } | MatchingConditionInstr::IntEquals { .. } => {
                let cond_value = compile_condition_test(b, inst)?;
                let Some(fail_block) = condition_fail_block else { continue };
                match b.known_bool(cond_value) {
                    Some(true) => {}
                    Some(false) => {
                        b.push_jump(fail_block, IrComment::MatchingCondCond);
                        return Ok(None);
                    }
                    None => {
                        let target = match success {
                            _ if Some(index) != last_branch => {
                                b.push_block_after_current(BlockSourceKind::MatchingCondContinue)
                            }
                            SuccessTarget::Create(kind) => b.push_block_after_current(kind),
                            SuccessTarget::Existing(target) => target,
                        };
                        b.push_jump_if(cond_value, target, fail_block, IrComment::MatchingCondCond);
                        if Some(index) == last_branch
                            && let SuccessTarget::Existing(target) = success
                        {
                            return Ok(Some(target));
                        }
                        b.goto_block(target);
                    }
                }
            }
        }
    }
    match success {
        SuccessTarget::Create(_) => Ok(Some(b.cur_block)),
        SuccessTarget::Existing(target) => {
            b.push_jump(target, IrComment::None);
            Ok(Some(target))
        }
    }
}

pub fn get_compiled_unit(ir: &ProgramIr, unit: IrUnitId) -> Option<&IrUnit> {
    match unit {
        IrUnitId::Function(function_id) => ir.function_unit(function_id),
        IrUnitId::Expr(typed_expr_id) => ir.exprs.get(&typed_expr_id),
    }
}

pub fn get_compiled_unit_mut(ir: &mut ProgramIr, unit: IrUnitId) -> Option<&mut IrUnit> {
    match unit {
        IrUnitId::Function(function_id) => ir.function_unit_mut(function_id),
        IrUnitId::Expr(typed_expr_id) => ir.exprs.get_mut(&typed_expr_id),
    }
}

pub fn get_unit_span(k1: &TypedProgram, unit: IrUnitId) -> Span {
    match unit {
        IrUnitId::Function(function_id) => k1.get_function_span(function_id),
        IrUnitId::Expr(typed_expr_id) => k1.exprs.get_span(typed_expr_id),
    }
}

pub struct ProgramRoots {
    pub main: Option<FunctionId>,
    pub program_exit: Option<FunctionId>,
    pub functions: Vec<FunctionId>,
}

pub fn get_program_roots(k1: &TypedProgram) -> K1Result<ProgramRoots> {
    let is_test = k1.test_suite.is_some();
    let mut exports: Vec<FunctionId> = vec![];
    let mut baked: Vec<FunctionId> = vec![];
    for (function_id, function) in k1.function_iter() {
        if function.linkage.is_exported() && !is_test {
            exports.push(function_id);
        }
        if function.address_baked() {
            baked.push(function_id);
        }
    }
    let main = if is_test {
        let test_main_ident = k1.ast.idents.intern("test-main");
        let Some(test_main_id) = k1.scopes.find_function(k1.scopes.k1_scope_id, test_main_ident)
        else {
            kbail!(k1, Span::NONE, "Missing k1/test-main");
        };
        Some(test_main_id)
    } else {
        let mut any_exported_global = false;
        for global_id in k1.globals.iter_ids() {
            if k1.globals.get(global_id).linkage.is_exported() {
                any_exported_global = true;
            }
        }
        if k1.plan.is_executable() {
            let Some(main_function_id) = k1.get_main_function_id() else {
                kbail!(k1, Span::NONE, "Program {} has no main function", k1.program_name());
            };
            Some(main_function_id)
        } else {
            if exports.is_empty() && !any_exported_global {
                kbail!(
                    k1,
                    Span::NONE,
                    "Library {} exports no functions or globals",
                    k1.program_name()
                );
            }
            None
        }
    };
    let program_exit = if main.is_some() {
        let program_exit_ident = k1.ast.idents.intern("program-exit");
        let Some(program_exit_id) =
            k1.scopes.find_function(k1.scopes.k1_scope_id, program_exit_ident)
        else {
            kbail!(k1, Span::NONE, "Missing k1/program-exit");
        };
        Some(program_exit_id)
    } else {
        None
    };
    let mut functions: Vec<FunctionId> = Vec::with_capacity(exports.len() + baked.len() + 2);
    functions.extend(main);
    functions.extend(program_exit);
    functions.append(&mut exports);
    functions.append(&mut baked);
    Ok(ProgramRoots { main, program_exit, functions })
}

pub fn compile_reachable(k1: &mut TypedProgram, roots: &[FunctionId]) -> K1Result<Vec<FunctionId>> {
    for root in roots {
        let requester = k1.trace.top();
        k1.ir.units_pending_compile.push(*root, requester);
    }
    k1.compile_all_pending_ir(Span::NONE)?;
    for root in roots {
        optimize_unit(k1, IrUnitId::Function(*root))?;
    }
    Ok(collect_reachable_functions(k1, roots))
}

fn collect_inst_function_refs(u: &UnitView, inst: &Inst, refs: &mut Vec<FunctionId>) {
    if let Inst::Call { call_id } = inst {
        match u.call(*call_id).callee {
            IrCallee::Direct(id)
            | IrCallee::Extern { function_id: id, .. }
            | IrCallee::BackendBuiltin(id, _) => refs.push(id),
            IrCallee::LlvmIntrinsic { .. } | IrCallee::Indirect(..) => {}
        }
    }
    visit_inst_values(u, inst, &mut |v| {
        if let Value::FunctionAddr(id) = v {
            refs.push(id)
        }
    });
}

fn collect_reachable_functions(k1: &TypedProgram, roots: &[FunctionId]) -> Vec<FunctionId> {
    let mut reachable: Vec<FunctionId> = Vec::with_capacity(1024);
    let mut seen: FxHashSet<FunctionId> = FxHashSet::with_capacity(1024);
    let mut worklist: Vec<FunctionId> = roots.to_vec();
    let mut seen_blocks: IdMap<BlockId, ()> = IdMap::default();
    let mut block_worklist: Vec<BlockId> = Vec::with_capacity(64);
    while let Some(function_id) = worklist.pop() {
        if !seen.insert(function_id) {
            continue;
        }
        reachable.push(function_id);
        let Some(unit) = k1.ir.function_unit(function_id) else { continue };
        let u = unit.view(&k1.ir.mem);
        seen_blocks.clear();
        block_worklist.clear();
        if let Some(entry) = u.first_block() {
            block_worklist.push(entry);
        }
        while let Some(block_id) = block_worklist.pop() {
            if seen_blocks.contains(block_id) {
                continue;
            }
            seen_blocks.insert(block_id, ());
            for inst_id in u.block_insts(block_id) {
                collect_inst_function_refs(&u, u.inst(inst_id), &mut worklist);
            }
            for succ in u.successors(block_id, Some(false)) {
                block_worklist.push(succ);
            }
        }
    }
    reachable
}

pub fn dump_program_ir(k1: &TypedProgram, reachable: &[FunctionId]) -> std::io::Result<()> {
    let mut dump = String::new();
    for function_id in reachable {
        if let Some(unit) = k1.ir.function_unit(*function_id) {
            display_unit(&mut dump, k1, unit, false).unwrap();
        }
    }
    let mut expr_ids: Vec<TypedExprId> = Vec::with_capacity(k1.ir.exprs.len());
    for expr_id in k1.ir.exprs.keys() {
        expr_ids.push(*expr_id);
    }
    expr_ids.sort_by_key(|id| id.as_u32());
    for expr_id in expr_ids {
        display_unit(&mut dump, k1, &k1.ir.exprs[&expr_id], false).unwrap();
    }
    let out_dir = k1.ast.idents.get_string(k1.config.out_dir);
    std::fs::create_dir_all(out_dir)?;
    std::fs::write(format!("{out_dir}/{}_ir.txt", k1.program_name()), dump)
}

pub fn validate_unit(k1: &TypedProgram, unit_id: IrUnitId) -> K1Result<()> {
    let mut errors = Vec::new();
    let ir = &k1.ir;
    let span = get_unit_span(k1, unit_id);
    let Some(unit) = get_compiled_unit(&k1.ir, unit_id) else {
        kbail!(k1, span, "Not compiled");
    };
    let u = unit.view(&ir.mem);
    let mut my_blocks = vec![false; u.block_count()];
    for block_id in u.block_ids() {
        my_blocks[block_id.as_u32() as usize - 1] = true;
    }
    let my_blocks_contains = |b: &BlockId| my_blocks[b.as_u32() as usize - 1];
    let preds = distinct_preds(&u);
    if let Some(entry) = u.first_block()
        && !preds[entry.as_u32() as usize - 1].is_empty()
    {
        errors.push(format!("b{entry}: entry block has predecessors"))
    }
    let doms = Dominators::compute(&u, &preds);
    let mut block_of = vec![None; u.inst_count()];
    for block_id in u.block_ids() {
        for inst_id in u.block_insts(block_id) {
            block_of[inst_id.as_u32() as usize - 1] = Some(block_id);
        }
    }
    let scalar_of = |v| get_value_kind(&u, v).as_value().and_then(|pt| pt.as_scalar());
    let mut expected_id = 1;
    for block_id in u.block_ids() {
        let block = u.block(block_id);
        let mut seen_non_phi = false;
        for inst_id in u.block_insts(block_id) {
            if inst_id.as_u32() != expected_id {
                errors.push(format!("i{inst_id}: committed ids are not in layout order"))
            }
            expected_id += 1;
            let is_last = Some(inst_id) == block.last;
            let inst = u.inst(inst_id);
            let inst_kind = get_inst_kind(&u, inst_id);
            if !is_last && inst_kind.is_terminator() {
                errors.push(format!("b{}: stray terminator", block_id))
            };
            if is_last && !inst_kind.is_terminator() {
                errors.push(format!("b{}: unterminated", block_id))
            }
            if inst.is_phi() {
                if seen_non_phi {
                    errors.push(format!("i{inst_id}: phi not at the front of b{block_id}"))
                }
            } else {
                seen_non_phi = true;
            }
            if matches!(inst, Inst::Alloca { .. }) && Some(block_id) != u.first_block() {
                errors.push(format!("i{inst_id}: alloca outside the entry block"))
            }

            match *inst {
                Inst::Data { .. } | Inst::ReloadGlobalAddr { .. } => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, value, .. } => {
                    if !get_value_kind(&u, dst).is_storage() {
                        errors.push(format!("i{inst_id}: store dst is not a ptr"))
                    }
                    if scalar_of(value).is_none() {
                        errors.push(format!("i{inst_id}: store value is not a scalar"))
                    }
                }
                Inst::Load { src, .. } => {
                    if !get_value_kind(&u, src).is_storage() {
                        errors.push(format!("i{inst_id}: load src is not storage"))
                    }
                }
                Inst::AtomicLoad { src, .. } => {
                    let src_kind = get_value_kind(&u, src);
                    if !src_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic load src is not storage"))
                    }
                }
                Inst::AtomicStore { dst, .. } => {
                    let dst_kind = get_value_kind(&u, dst);
                    if !dst_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic store dst is not storage"))
                    }
                }
                Inst::AtomicRmw { dst, .. } => {
                    let dst_kind = get_value_kind(&u, dst);
                    if !dst_kind.is_storage() {
                        errors.push(format!("i{inst_id}: atomic rmw dst is not storage"))
                    }
                }
                Inst::AtomicCmpxchg { id } => {
                    let cas = u.cmpxchg(id);
                    for (v, what) in [(cas.dst, "dst"), (cas.result, "result")] {
                        if !get_value_kind(&u, v).is_storage() {
                            errors.push(format!("i{inst_id}: atomic cmpxchg {what} is not storage"))
                        }
                    }
                }
                Inst::VecOp { id } => {
                    let vop = u.vec_op(id);
                    if !matches!(vop.op, VecOpIr::ToMask | VecOpIr::ReduceOr)
                        && !get_value_kind(&u, vop.dst).is_storage()
                    {
                        errors.push(format!("i{inst_id}: vec op dst is not storage"))
                    }
                }
                Inst::Fence { .. } => (),
                Inst::Copy { dst, src, t, .. } => {
                    let src_type = get_value_kind(&u, src);
                    if !src_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy src is not a ptr"))
                    }
                    let dst_type = get_value_kind(&u, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy dst v{} is not a ptr", inst_id))
                    }
                    if !t.is_agg() {
                        errors.push(format!("i{inst_id}: copy of a non-aggregate"))
                    }
                }
                Inst::Zero { dst, t, .. } => {
                    if !get_value_kind(&u, dst).is_storage() {
                        errors.push(format!("i{inst_id}: zero dst is not a ptr"))
                    }
                    if !t.is_agg() {
                        errors.push(format!("i{inst_id}: zero of a non-aggregate"))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = get_value_kind(&u, base);
                    if !base_type.is_storage() {
                        errors.push(format!("i{inst_id}: struct_offset base is not a ptr"))
                    }
                }
                Inst::ArrayOffset { base, element_index, .. } => {
                    if !get_value_kind(&u, base).is_storage() {
                        errors.push(format!("i{inst_id}: array_offset base is not a ptr"))
                    }
                    if !scalar_of(element_index).is_some_and(|st| st.is_word_int()) {
                        errors.push(format!(
                            "i{inst_id}: array_offset index type is not word-sized int",
                        ))
                    }
                }
                Inst::Call { .. } => (),
                Inst::Jump(block) => {
                    if !my_blocks_contains(&block) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                }
                Inst::JumpIf { cond, cons, alt } => {
                    let cond_type = get_value_kind(&u, cond);
                    if !cond_type.is_value() {
                        errors.push(format!("i{inst_id}: jumpif cond is not a value"))
                    }

                    if !my_blocks_contains(&cons) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                    if !my_blocks_contains(&alt) {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                }
                Inst::Switch { value, cases, default } => {
                    let mask = match scalar_of(value) {
                        Some(st) => arith::width_mask(st.width_bits()),
                        None => {
                            errors.push(format!("i{inst_id}: switch value is not a scalar"));
                            u64::MAX
                        }
                    };
                    if !my_blocks_contains(&default) {
                        errors.push(format!("i{inst_id}: switch default to non-existent block"))
                    }
                    let cases = u.switch_cases(cases);
                    for (index, case) in cases.iter().enumerate() {
                        if !my_blocks_contains(&case.target) {
                            errors.push(format!("i{inst_id}: switch case to non-existent block"))
                        }
                        if case.value & mask != case.value {
                            errors.push(format!("i{inst_id}: switch case value exceeds width"))
                        }
                        if cases[..index].iter().any(|c| c.value == case.value) {
                            errors.push(format!("i{inst_id}: duplicate switch case value"))
                        }
                    }
                }
                Inst::Unreachable => (),
                Inst::Phi { t, incomings } => {
                    if t.is_agg() {
                        errors.push(format!("i{inst_id}: phi over an aggregate"))
                    }
                    let block_preds = &preds[block_id.as_u32() as usize - 1];
                    let cases = u.phi_cases(incomings);
                    for incoming in cases {
                        if get_value_kind(&u, incoming.value).expect_value().is_err() {
                            errors.push(format!("i{inst_id}: phi type not a value kind"));
                        }
                        if !my_blocks_contains(&incoming.from) {
                            errors.push(format!("i{inst_id}: phi incoming block does not exist"))
                        } else if !block_preds.contains(&incoming.from) {
                            errors.push(format!(
                                "i{inst_id}: phi incoming from non-predecessor b{}",
                                incoming.from
                            ))
                        }
                    }
                    for pred in block_preds {
                        let mut count = 0;
                        for incoming in cases {
                            if incoming.from == *pred {
                                count += 1;
                            }
                        }
                        if count != 1 {
                            errors.push(format!(
                                "i{inst_id}: phi has {count} incomings for predecessor b{pred}"
                            ))
                        }
                    }
                }
                Inst::Ret { v, .. } => {
                    let ret_val_type = get_value_kind(&u, v);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("i{inst_id}: ret value is not a value"))
                    }
                }
                Inst::Select { cond, cons, alt, t } => {
                    if scalar_of(cond) != Some(ScalarType::I8) {
                        errors.push(format!("i{inst_id}: select cond is not i8"))
                    }
                    if scalar_of(cons) != Some(t) || scalar_of(alt) != Some(t) {
                        errors.push(format!("i{inst_id}: select arm type mismatch"))
                    }
                }
                Inst::BoolNegate { v } => {
                    if scalar_of(v) != Some(ScalarType::I8) {
                        errors.push(format!("i{inst_id}: bool_negate src is not a bool"))
                    }
                }
                Inst::IntUnop { op, v, t } => {
                    if !t.is_int() || scalar_of(v) != Some(t) {
                        errors.push(format!("i{inst_id}: {} src is not {t}", op.name()))
                    }
                }
                Inst::BitCast { .. } => (),
                Inst::IntTrunc { v, to } => {
                    let src = scalar_of(v);
                    if !src.is_some_and(|st| st.is_int() && st.width_bits() > to.width_bits()) {
                        errors.push(format!(
                            "i{inst_id}: int_trunc src is not an int wider than {to}"
                        ))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: int_trunc to is not int"))
                    }
                }
                Inst::IntExtU { v, to } | Inst::IntExtS { v, to } => {
                    let src = scalar_of(v);
                    if !src.is_some_and(|st| st.is_int() && st.width_bits() < to.width_bits()) {
                        errors.push(format!(
                            "i{inst_id}: int_ext src is not an int narrower than {to}"
                        ))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: int_ext to is not int"))
                    }
                }
                Inst::FloatTrunc { v, to } => {
                    if scalar_of(v) != Some(ScalarType::F64) {
                        errors.push(format!("i{inst_id}: float_trunc src is not f64"))
                    }
                    if to != ScalarType::F32 {
                        errors.push(format!("i{inst_id}: float_trunc to is not f32"))
                    }
                }
                Inst::FloatExt { v, to } => {
                    if scalar_of(v) != Some(ScalarType::F32) {
                        errors.push(format!("i{inst_id}: float_ext src is not f32"))
                    }
                    if to != ScalarType::F64 {
                        errors.push(format!("i{inst_id}: float_ext to is not f64"))
                    }
                }
                Inst::FloatToIntUnsigned { v, to } | Inst::FloatToIntSigned { v, to } => {
                    if !scalar_of(v).is_some_and(|st| st.is_float()) {
                        errors.push(format!("i{inst_id}: float_to_int src is not float"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: float_to_int to is not int"))
                    }
                }
                Inst::IntToFloatUnsigned { v, to } | Inst::IntToFloatSigned { v, to } => {
                    if !scalar_of(v).is_some_and(|st| st.is_int()) {
                        errors.push(format!("i{inst_id}: int_to_float src is not int"))
                    }
                    if !to.is_float() {
                        errors.push(format!("i{inst_id}: int_to_float to is not float"))
                    }
                }
                Inst::PtrToWord { v, to } => {
                    let inst_type = get_value_kind(&u, v);
                    if !inst_type.is_storage() {
                        errors.push(format!("i{inst_id}: ptr_to_word src is not a ptr"))
                    }
                    if !to.is_word_int() {
                        errors.push(format!("i{inst_id}: ptr_to_word to is not a word-sized int"))
                    }
                }
                Inst::WordToPtr { v } => {
                    if !scalar_of(v).is_some_and(|st| st.is_word_int()) {
                        errors.push(format!("i{inst_id}: word_to_ptr src is not a word-sized int"))
                    }
                }
                Inst::IntBinop { op, lhs, rhs, t } => {
                    if !t.is_int() {
                        errors.push(format!("i{inst_id}: {} on non-int {t}", op.name()))
                    }
                    if scalar_of(lhs) != Some(t) {
                        errors.push(format!("i{inst_id}: {} lhs is not {t}", op.name()))
                    }
                    if op.takes_count() {
                        if !scalar_of(rhs).is_some_and(|st| st.is_int()) {
                            errors.push(format!("i{inst_id}: {} count is not an int", op.name()))
                        }
                    } else if scalar_of(rhs) != Some(t) {
                        errors.push(format!("i{inst_id}: {} rhs is not {t}", op.name()))
                    }
                }
                Inst::IntOverflows { lhs, rhs, .. }
                | Inst::IntCmp { lhs, rhs, .. }
                | Inst::IntCmp3 { lhs, rhs, .. } => {
                    let l = scalar_of(lhs);
                    if l != scalar_of(rhs) || !l.is_some_and(|st| st.is_int()) {
                        errors.push(format!("i{inst_id}: operands are not one int type"))
                    }
                }
                Inst::FloatBinop { op, lhs, rhs, t } => {
                    if !t.is_float() || scalar_of(lhs) != Some(t) || scalar_of(rhs) != Some(t) {
                        errors.push(format!("i{inst_id}: {} operands are not {t}", op.name()))
                    }
                }
                Inst::FloatUnop { op, v, t } => {
                    if !t.is_float() || scalar_of(v) != Some(t) {
                        errors.push(format!("i{inst_id}: {} src is not {t}", op.name()))
                    }
                }
                Inst::FloatFma { a, b, c, t } => {
                    if !t.is_float()
                        || scalar_of(a) != Some(t)
                        || scalar_of(b) != Some(t)
                        || scalar_of(c) != Some(t)
                    {
                        errors.push(format!("i{inst_id}: fma operands are not {t}"))
                    }
                }
                Inst::FloatCmp { lhs, rhs, .. } => {
                    let l = scalar_of(lhs);
                    if l != scalar_of(rhs) || !l.is_some_and(|st| st.is_float()) {
                        errors.push(format!(
                            "i{inst_id}: float compare operands are not one float type"
                        ))
                    }
                }
                Inst::BakeStaticValue { .. } => (),
            }
        }
    }
    if expected_id as usize - 1 != u.inst_count() {
        errors.push("unit holds insts outside its blocks".to_string())
    }
    for block_id in u.block_ids() {
        if !doms.is_reachable(block_id) {
            continue;
        }
        for inst_id in u.block_insts(block_id) {
            let inst = u.inst(inst_id);
            if let Inst::Phi { incomings, .. } = *inst {
                for incoming in u.phi_cases(incomings) {
                    let Value::Inst(def) = incoming.value else { continue };
                    let Some(def_block) = block_of[def.as_u32() as usize - 1] else { continue };
                    if doms.is_reachable(incoming.from) && !doms.dominates(def_block, incoming.from)
                    {
                        errors.push(format!(
                            "i{inst_id}: phi incoming i{def} does not dominate the end of b{}",
                            incoming.from
                        ))
                    }
                }
                continue;
            }
            visit_inst_values(&u, inst, &mut |v| {
                let Value::Inst(def) = v else { return };
                let Some(def_block) = block_of[def.as_u32() as usize - 1] else { return };
                let dominates = if def_block == block_id {
                    def.as_u32() < inst_id.as_u32()
                } else {
                    doms.dominates(def_block, block_id)
                };
                if !dominates {
                    errors.push(format!("i{inst_id}: use of i{def} which does not dominate it"))
                }
            });
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

fn distinct_preds(u: &UnitView) -> Vec<Vec<BlockId>> {
    let mut preds: Vec<Vec<BlockId>> = vec![Vec::new(); u.block_count()];
    for b in u.block_ids() {
        for succ in u.successors(b, None) {
            let list = &mut preds[succ.as_u32() as usize - 1];
            if !list.contains(&b) {
                list.push(b);
            }
        }
    }
    preds
}

struct Dominators {
    entry: Option<BlockId>,
    rpo: Vec<u32>,
    idom: Vec<Option<BlockId>>,
}

impl Dominators {
    fn compute(u: &UnitView, preds: &[Vec<BlockId>]) -> Dominators {
        let n = u.block_count();
        let mut doms =
            Dominators { entry: u.first_block(), rpo: vec![u32::MAX; n], idom: vec![None; n] };
        let Some(entry) = doms.entry else { return doms };
        let at = |b: BlockId| b.as_u32() as usize - 1;
        let mut postorder: Vec<BlockId> = Vec::new();
        let mut visited = vec![false; n];
        let mut stack: Vec<(BlockId, usize)> = vec![(entry, 0)];
        visited[at(entry)] = true;
        while let Some((b, index)) = stack.last_mut() {
            match u.get_successor(*b, None, *index) {
                Some(succ) => {
                    *index += 1;
                    if !visited[at(succ)] {
                        visited[at(succ)] = true;
                        stack.push((succ, 0));
                    }
                }
                None => {
                    postorder.push(*b);
                    stack.pop();
                }
            }
        }
        for (number, b) in postorder.iter().rev().enumerate() {
            doms.rpo[at(*b)] = number as u32;
        }
        doms.idom[at(entry)] = Some(entry);
        let mut changed = true;
        while changed {
            changed = false;
            for b in postorder.iter().rev().skip(1) {
                let mut new_idom: Option<BlockId> = None;
                for pred in &preds[at(*b)] {
                    if doms.idom[at(*pred)].is_none() {
                        continue;
                    }
                    new_idom = Some(match new_idom {
                        None => *pred,
                        Some(current) => doms.intersect(*pred, current),
                    });
                }
                if doms.idom[at(*b)] != new_idom {
                    doms.idom[at(*b)] = new_idom;
                    changed = true;
                }
            }
        }
        doms
    }

    fn intersect(&self, a: BlockId, b: BlockId) -> BlockId {
        let at = |b: BlockId| b.as_u32() as usize - 1;
        let (mut f1, mut f2) = (a, b);
        while f1 != f2 {
            while self.rpo[at(f1)] > self.rpo[at(f2)] {
                f1 = self.idom[at(f1)].unwrap();
            }
            while self.rpo[at(f2)] > self.rpo[at(f1)] {
                f2 = self.idom[at(f2)].unwrap();
            }
        }
        f1
    }

    fn is_reachable(&self, b: BlockId) -> bool {
        self.rpo[b.as_u32() as usize - 1] != u32::MAX
    }

    fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        if !self.is_reachable(a) || !self.is_reachable(b) {
            return false;
        }
        let mut current = b;
        loop {
            if current == a {
                return true;
            }
            if Some(current) == self.entry {
                return false;
            }
            current = self.idom[current.as_u32() as usize - 1].unwrap();
        }
    }
}

mod fold;
mod iropt;
pub use iropt::optimize_unit;
mod unit;
pub use unit::*;
#[cfg(test)]
mod validate_test;

pub fn unit_to_string(k1: &TypedProgram, unit: IrUnitId, show_source: bool) -> String {
    let mut s = String::new();
    let unit = get_compiled_unit(&k1.ir, unit).unwrap();
    display_unit(&mut s, k1, unit, show_source).unwrap();
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
                k1.get_function_namespace(function_id),
                k1.ident_str(function.name),
                "/",
                true,
            );
        }
        IrUnitId::Expr(typed_expr_id) => {
            let expr_span = k1.exprs.get_span(typed_expr_id);
            let (source, line) = k1.get_span_location(expr_span);
            write!(w, "expr {}:{}", source.filename_str(&k1.ast.idents), line.line_number())?;
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
        if index > 0 {
            w.write_str(", ")?;
        }
        write!(w, "%p{}: ", index)?;
        k1.display_pt(w, param.pt)?;
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
            write!(w, " from {}:{}", source.filename_str(&k1.ast.idents), line.line_number())?;
        }
    };
    writeln!(w, " (inst count={})", unit.inst_count())?;
    display_blocks(w, k1, &unit.view(&k1.ir.mem), show_source)?;
    Ok(())
}

pub fn display_blocks(
    w: &mut impl Write,
    k1: &TypedProgram,
    u: &UnitView,
    show_source: bool,
) -> std::fmt::Result {
    for block in u.block_ids() {
        display_block(w, k1, u, block, show_source)?;
    }
    Ok(())
}

pub fn blocks_to_string(k1: &TypedProgram, u: &UnitView, show_source: bool) -> String {
    let mut s = String::new();
    display_blocks(&mut s, k1, u, show_source).unwrap();
    s
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    u: &UnitView,
    block_id: BlockId,
    show_source: bool,
) -> std::fmt::Result {
    writeln!(w, "b{} ({}):", block_id, u.block(block_id).kind.str())?;
    for inst_id in u.block_insts(block_id) {
        let inst_str = inst_to_string(k1, u, inst_id);
        write!(w, "  {:66}; {:30}", inst_str, u.comment(inst_id).str())?;
        let span = u.span(inst_id);
        if show_source && !span.is_none() {
            let lines = k1.ast.get_span_content(span);
            let (source, line) = k1.get_span_location(span);
            let first_line = lines.lines().next().unwrap_or("");
            let column = source.local_from_global_offset(span.global_start) + 1 - line.start_char;
            write!(w, "| {first_line:30}|{:3}:{}|", line.line_number(), column)?;
        }
        writeln!(w)?;
    }
    Ok(())
}

pub fn inst_to_string(k1: &TypedProgram, u: &UnitView, inst_id: InstId) -> String {
    let mut s = String::new();
    display_inst(&mut s, k1, u, inst_id).unwrap();
    s
}

pub fn display_inst(
    w: &mut impl Write,
    k1: &TypedProgram,
    u: &UnitView,
    inst_id: InstId,
) -> std::fmt::Result {
    match get_inst_kind(u, inst_id) {
        InstKind::Value(t) if !t.is_empty() => {
            write!(w, "%{}: ", inst_id)?;
            k1.display_pt(w, t)?;
            w.write_str(" = ")?;
        }
        _ => write!(w, "%{} = ", inst_id)?,
    }
    let v = |w: &mut dyn Write, value: Value| display_value(w, k1, u, value);
    let unaligned_suffix = |w: &mut dyn Write, unaligned: bool| {
        if unaligned { w.write_str(", unaligned") } else { Ok(()) }
    };
    match *u.inst(inst_id) {
        Inst::Data { t, bits } => {
            write!(w, "imm ")?;
            display_const_value(w, t, bits)?;
        }
        Inst::ReloadGlobalAddr { storage_pt, id } => {
            write!(w, "reload_global_addr @g{} ", id.as_u32())?;
            k1.display_pt(w, storage_pt)?;
        }
        Inst::Alloca { t, vm_layout, returned, .. } => {
            write!(w, "alloca ")?;
            if returned {
                w.write_str("returned ")?;
            }
            k1.display_pt(w, t)?;
            write!(w, ", align {}", vm_layout.align)?;
        }
        Inst::Store { dst, value, volatile, unaligned } => {
            if volatile {
                w.write_str("volatile ")?;
            }
            write!(w, "store ")?;
            v(w, value)?;
            w.write_str(" to ")?;
            v(w, dst)?;
            unaligned_suffix(w, unaligned)?;
        }
        Inst::Load { t: _, src, volatile, unaligned } => {
            if volatile {
                w.write_str("volatile ")?;
            }
            write!(w, "load ")?;
            v(w, src)?;
            unaligned_suffix(w, unaligned)?;
        }
        Inst::AtomicLoad { t: _, src, ord } => {
            write!(w, "atomic load {} ", ord.name())?;
            v(w, src)?;
        }
        Inst::AtomicStore { dst, value, ord } => {
            write!(w, "atomic store {} ", ord.name())?;
            v(w, value)?;
            w.write_str(" to ")?;
            v(w, dst)?;
        }
        Inst::AtomicRmw { op, t: _, dst, operand, ord } => {
            write!(w, "atomic {} {} ", op.name(), ord.name())?;
            v(w, dst)?;
            w.write_str(", ")?;
            v(w, operand)?;
        }
        Inst::AtomicCmpxchg { id } => {
            let cas = u.cmpxchg(id);
            write!(
                w,
                "atomic cmpxchg{} {}/{} ",
                if cas.weak { " weak" } else { "" },
                cas.success.name(),
                cas.failure.name()
            )?;
            w.write_str("at ")?;
            v(w, cas.dst)?;
            w.write_str(", expected ")?;
            v(w, cas.expected)?;
            w.write_str(", desired ")?;
            v(w, cas.desired)?;
            w.write_str(", into ")?;
            v(w, cas.result)?;
        }
        Inst::VecOp { id } => {
            let vop = u.vec_op(id);
            write!(w, "vec {} <{} x ", vop.op.name(), vop.lanes)?;
            display_scalar_type(w, vop.elem)?;
            w.write_str("> ")?;
            v(w, vop.lhs)?;
            w.write_str(", ")?;
            v(w, vop.rhs)?;
            w.write_str(" into ")?;
            v(w, vop.dst)?;
        }
        Inst::Fence { ord } => {
            write!(w, "fence {}", ord.name())?;
        }
        Inst::Copy { dst, src, t, vm_size: _, volatile, unaligned } => {
            if volatile {
                w.write_str("volatile ")?;
            }
            write!(w, "copy ")?;
            k1.display_pt(w, t)?;
            w.write_str(" ")?;
            v(w, src)?;
            w.write_str(" to ")?;
            v(w, dst)?;
            unaligned_suffix(w, unaligned)?;
        }
        Inst::Zero { dst, t, vm_size: _, unaligned } => {
            write!(w, "zero ")?;
            k1.display_pt(w, t)?;
            w.write_str(" ")?;
            v(w, dst)?;
            unaligned_suffix(w, unaligned)?;
        }
        Inst::StructOffset { struct_t, base, field_index, vm_offset, unaligned } => {
            write!(w, "struct_offset ")?;
            k1.display_pt(w, PhysicalType::agg(struct_t))?;
            write!(w, ".{} ", field_index)?;
            v(w, base)?;
            write!(w, " ({})", vm_offset)?;
            unaligned_suffix(w, unaligned)?;
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            write!(w, "array_offset ")?;
            k1.display_pt(w, element_t)?;
            w.write_str(" ")?;
            v(w, base)?;
            w.write_str("[")?;
            v(w, element_index)?;
            w.write_str("]")?;
        }
        Inst::Call { call_id: id } => {
            let call = u.call(id);
            write!(w, "call ")?;
            match &call.callee {
                IrCallee::BackendBuiltin(_, backend_builtin) => {
                    write!(w, "builtin {}", backend_builtin.kind_name())?;
                }
                IrCallee::Direct(function_id) => {
                    w.write_str(k1.ident_str(k1.get_function(*function_id).name))?;
                }
                IrCallee::Indirect(_, callee) => {
                    w.write_str("indirect ")?;
                    v(w, *callee)?;
                }
                IrCallee::LlvmIntrinsic { name, .. } => {
                    write!(w, "llvm {}", k1.ident_str(*name))?;
                }
                IrCallee::Extern { library_name, function_name, .. } => {
                    write!(
                        w,
                        "extern {} {}",
                        k1.ident_str_opt(*library_name),
                        k1.ident_str(*function_name),
                    )?;
                }
            };
            w.write_str("(")?;
            for (index, arg) in u.args(call.args).iter().enumerate() {
                if index > 0 {
                    w.write_str(", ")?;
                }
                v(w, *arg)?;
            }
            w.write_str(")")?;
            if let Some(dst) = call.dst {
                w.write_str(" into ")?;
                v(w, dst)?;
            }
        }
        Inst::Jump(block_id) => {
            write!(w, "jmp b{}", block_id)?;
        }
        Inst::JumpIf { cond, cons, alt } => {
            write!(w, "jmpif ")?;
            v(w, cond)?;
            write!(w, ", b{}, b{}", cons, alt)?;
        }
        Inst::Switch { value, cases, default } => {
            write!(w, "switch ")?;
            v(w, value)?;
            w.write_str(" [")?;
            for (index, case) in u.switch_cases(cases).iter().enumerate() {
                if index > 0 {
                    w.write_str(", ")?;
                }
                write!(w, "{} -> b{}", case.value, case.target)?;
            }
            write!(w, "] default b{}", default)?;
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
        }
        Inst::Phi { t: _, incomings } => {
            write!(w, "phi [")?;
            for (i, incoming) in u.phi_cases(incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "b{}: ", incoming.from)?;
                v(w, incoming.value)?;
            }
            write!(w, "]")?;
        }
        Inst::Ret { v: value } => {
            write!(w, "ret ")?;
            v(w, value)?;
        }
        Inst::Select { cond, cons, alt, .. } => {
            write!(w, "select ")?;
            v(w, cond)?;
            w.write_str(", ")?;
            v(w, cons)?;
            w.write_str(", ")?;
            v(w, alt)?;
        }
        Inst::BoolNegate { v: value } => {
            write!(w, "bool not ")?;
            v(w, value)?;
        }
        Inst::IntUnop { op, v: value, .. } => {
            write!(w, "{} ", op.name())?;
            v(w, value)?;
        }
        Inst::BitCast { v: value, .. } => {
            write!(w, "bitcast ")?;
            v(w, value)?;
        }
        Inst::IntTrunc { v: value, .. } => {
            write!(w, "trunc ")?;
            v(w, value)?;
        }
        Inst::IntExtU { v: value, .. } => {
            write!(w, "zext ")?;
            v(w, value)?;
        }
        Inst::IntExtS { v: value, .. } => {
            write!(w, "sext ")?;
            v(w, value)?;
        }
        Inst::FloatTrunc { v: value, .. } => {
            write!(w, "ftrunc ")?;
            v(w, value)?;
        }
        Inst::FloatExt { v: value, .. } => {
            write!(w, "fext ")?;
            v(w, value)?;
        }
        Inst::FloatToIntUnsigned { v: value, .. } => {
            write!(w, "floattoint ")?;
            v(w, value)?;
        }
        Inst::FloatToIntSigned { v: value, .. } => {
            write!(w, "floattoint signed ")?;
            v(w, value)?;
        }
        Inst::IntToFloatUnsigned { v: value, .. } => {
            write!(w, "inttofloat ")?;
            v(w, value)?;
        }
        Inst::IntToFloatSigned { v: value, .. } => {
            write!(w, "inttofloat signed ")?;
            v(w, value)?;
        }
        Inst::PtrToWord { v: value, .. } => {
            write!(w, "ptrtoint ")?;
            v(w, value)?;
        }
        Inst::WordToPtr { v: value } => {
            write!(w, "inttoptr ")?;
            v(w, value)?;
        }
        Inst::IntBinop { op, lhs, rhs, .. } => binop(w, op.name(), lhs, rhs, &v)?,
        Inst::IntOverflows { op, lhs, rhs, .. } => {
            write!(w, "{} ", op.name())?;
            v(w, lhs)?;
            w.write_str(", ")?;
            v(w, rhs)?;
        }
        Inst::IntCmp { lhs, rhs, pred, .. } => {
            write!(w, "icmp {} ", pred.name())?;
            v(w, lhs)?;
            w.write_str(", ")?;
            v(w, rhs)?;
        }
        Inst::IntCmp3 { op, lhs, rhs, .. } => {
            write!(w, "{} ", op.name())?;
            v(w, lhs)?;
            w.write_str(", ")?;
            v(w, rhs)?;
        }
        Inst::FloatBinop { op, lhs, rhs, .. } => binop(w, op.name(), lhs, rhs, &v)?,
        Inst::FloatUnop { op, v: value, .. } => {
            write!(w, "{} ", op.name())?;
            v(w, value)?;
        }
        Inst::FloatFma { a, b, c, .. } => {
            write!(w, "fma ")?;
            v(w, a)?;
            w.write_str(", ")?;
            v(w, b)?;
            w.write_str(", ")?;
            v(w, c)?;
        }
        Inst::FloatCmp { lhs, rhs, pred, .. } => {
            write!(w, "fcmp {} ", pred.name())?;
            v(w, lhs)?;
            w.write_str(", ")?;
            v(w, rhs)?;
        }
        Inst::BakeStaticValue { type_id, value } => {
            write!(w, "bake ")?;
            k1.display_type_id(w, type_id, dump::TypeDisplayMode::Name)?;
            w.write_str(" ")?;
            v(w, value)?;
        }
    };
    Ok(())
}

fn binop(
    w: &mut impl Write,
    name: &str,
    lhs: Value,
    rhs: Value,
    v: &dyn Fn(&mut dyn Write, Value) -> std::fmt::Result,
) -> std::fmt::Result {
    write!(w, "{name} ")?;
    v(w, lhs)?;
    w.write_str(", ")?;
    v(w, rhs)
}

impl From<ScalarType> for &'static str {
    fn from(st: ScalarType) -> &'static str {
        match st {
            ScalarType::I8 => "i8",
            ScalarType::I16 => "i16",
            ScalarType::I32 => "i32",
            ScalarType::I64 => "i64",
            ScalarType::F32 => "f32",
            ScalarType::F64 => "f64",
            ScalarType::Pointer => "ptr",
        }
    }
}
pub fn display_scalar_type(w: &mut (impl Write + ?Sized), scalar: ScalarType) -> std::fmt::Result {
    w.write_str(scalar.into())
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_scalar_type(f, *self)
    }
}

pub fn display_const_value(w: &mut dyn Write, t: ScalarType, bits: u64) -> std::fmt::Result {
    match t {
        ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64 => {
            write!(w, "{}", crate::arith::sign_extend(t.width_bits(), bits))
        }
        ScalarType::Pointer => {
            if bits == 0 {
                w.write_str("null")
            } else {
                write!(w, "{:#x}", bits)
            }
        }
        ScalarType::F32 => write!(w, "{:?}", f32::from_bits(bits as u32)),
        ScalarType::F64 => write!(w, "{:?}", f64::from_bits(bits)),
    }
}

pub fn display_const(w: &mut dyn Write, t: ScalarType, bits: u64) -> std::fmt::Result {
    display_scalar_type(w, t)?;
    w.write_str(" ")?;
    display_const_value(w, t, bits)
}

pub fn display_value(
    w: &mut dyn Write,
    k1: &TypedProgram,
    u: &UnitView,
    value: Value,
) -> std::fmt::Result {
    match value {
        Value::Inst(inst_id) => match *u.inst(inst_id) {
            Inst::Data { t, bits } => display_const(w, t, bits),
            _ => write!(w, "%{}", inst_id),
        },
        Value::GlobalAddr { id, .. } => write!(w, "@g{}", id.as_u32()),
        Value::StaticValue { id, .. } => write!(w, "${}", id.as_u32()),
        Value::FunctionAddr(function_id) => {
            write!(w, "@{}", k1.ident_str(k1.get_function(function_id).name))
        }
        Value::FnParam { index, .. } => write!(w, "%p{}", index),
        Value::Data32 { t, data } => display_const(w, t, data32_bits(t, data)),
        Value::IsStatic => w.write_str("is-static"),
        Value::Empty => w.write_str("{}"),
    }
}
