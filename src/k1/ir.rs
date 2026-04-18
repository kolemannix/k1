/// Copyright (c) 2025 knix
/// All rights reserved.
///
/// The goal here is a strongly-typed SSA form
/// instruction-based IR with basic blocks, obviously
/// very much like LLVM, as that is our primary target.
/// But I currently think there's going to be a lot of value
/// in having our own. It'll be easier to write an interpreter for
/// and will help make adding other backends far, far easier
use crate::kmem::{DlNode, Dlist, Handle, List, NodeHandle};
use crate::parse::{self, Ident, NumericWidth};
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::StaticValueId;
use crate::{failf, static_assert_size};
use crate::{
    kmem::{self, MSlice, MStr},
    lex::SpanId,
    nz_u32_id,
    typer::{types::*, *},
    vpool::VPool,
};
use ahash::{HashMapExt, HashSetExt};
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;
use log::debug;
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
    pub name: Ident,
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
    pub comments: VPool<IrStr, InstId>,
    pub debug_info: VPool<IrDebugInfo, InstId>,
    /// Compiled ir for actual functions
    pub functions: VPool<Option<IrUnit>, FunctionId>,
    /// Compiled ir for #static exprs and global initializers
    pub exprs: FxHashMap<TypedExprId, IrUnit>,
    pub module_config: IrModuleConfig,
    pub calls: VPool<IrCall, IrCallId>,
    pub phys_fn_type_cache: FxHashMap<TypeId, PhysicalFunctionType>,

    // Builder data
    b_variables: Vec<BuilderVariable>,
    b_loops: FxHashMap<ScopeId, LoopInfo>,
    pub b_units_pending_compile: Vec<FunctionId>,
}
type IrStr = MStr<ProgramIr>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrUnitId {
    Function(FunctionId),
    Expr(TypedExprId),
}

pub struct IrModuleConfig {}

impl ProgramIr {
    pub fn make(instr_count_hint: usize) -> Self {
        let function_count_hint = instr_count_hint / 16;
        ProgramIr {
            mem: kmem::Mem::make(),
            instrs: VPool::make_with_hint("ir_soa_instrs", instr_count_hint),
            sources: VPool::make_with_hint("ir_soa_sources", instr_count_hint),
            comments: VPool::make_with_hint("ir_soa_comments", instr_count_hint),
            debug_info: VPool::make_with_hint("ir_soa_debug_info", instr_count_hint),
            functions: VPool::make_with_hint("ir_functions", function_count_hint),
            calls: VPool::make_with_hint("ir_calls", instr_count_hint / 2),
            phys_fn_type_cache: FxHashMap::new(),
            exprs: FxHashMap::new(),
            module_config: IrModuleConfig {},
            b_variables: Vec::with_capacity(256),
            b_loops: FxHashMap::default(),
            b_units_pending_compile: vec![],
        }
    }

    fn word_sized_int(&self) -> ScalarType {
        ScalarType::U64
    }

    pub fn add_inst(
        &mut self,
        inst: Inst,
        comment: IrStr,
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

#[derive(Clone, Copy)]
pub struct Block {
    pub name: &'static str,
    pub instrs: Dlist<InstId, ProgramIr>,
}

impl Block {
    pub fn identical(&self, other: &Block) -> bool {
        self.name == other.name
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

    pub blocks: Dlist<Block, ProgramIr>,
    pub function_builtin_kind: Option<BackendBuiltin>,
    pub is_debug: bool,

    pub inline_done: bool,
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
    // In LLVM backend, this is becomes a runtime switch
    // In the VM, just a lookup. So we leave it as a builtin
    // rather than generate ir for it due to that difference
    TypeSchema,
    TypeName,

    // Platform-provided
    Allocate,
    AllocateZeroed,
    Reallocate,
    Free,
    MemCopy,
    MemSet,
    MemEquals,
    Exit,

    CompilerMessage,
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
    Builtin(FunctionId, BackendBuiltin),
    Direct(FunctionId),
    Indirect(PhysicalFunctionType, Value),
    Extern {
        library_name: Option<parse::Ident>,
        function_name: parse::Ident,
        function_id: FunctionId,
    },
    // No lambda call; been compiled down to just calls and args by now
}

impl IrCallee {
    fn known_function_id(&self) -> Option<FunctionId> {
        match self {
            IrCallee::Direct(fid) => Some(*fid),
            IrCallee::Extern { function_id, .. } => Some(*function_id),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct CameFromCase {
    pub from: BlockId,
    pub value: Value,
}

#[derive(Clone, Copy)]
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
    // FnRetSlot {
    //     t: PhysicalType,
    // },

    // Large 'immediates' just get encoded as their own instruction
    // We have space for u32, so we use it
    Data32 {
        t: ScalarType,
        data: u32,
    },
    Empty,
}

impl Value {
    const PTR_ZERO: Value = Value::Data32 { t: ScalarType::Pointer, data: 0 };

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
//task(ir): Get inst down to 32 bytes
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
    CameFrom {
        t: PhysicalType,
        incomings: MSlice<CameFromCase, ProgramIr>,
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

pub fn get_value_kind(ir: &ProgramIr, types: &TypePool, value: Value) -> InstKind {
    match value {
        Value::Inst(inst_id) => get_inst_kind(ir, types, inst_id),
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

pub fn get_inst_kind(ir: &ProgramIr, types: &TypePool, inst_id: InstId) -> InstKind {
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
        Inst::Copy { .. } => InstKind::Void,
        Inst::StructOffset { .. } => InstKind::PTR,
        Inst::ArrayOffset { .. } => InstKind::PTR,
        Inst::Call { call_id: id } => InstKind::Value(ir.calls.get(id).ret_type),
        Inst::Jump(_) => InstKind::Terminator,
        Inst::JumpIf { .. } => InstKind::Terminator,
        Inst::Unreachable => InstKind::Terminator,
        Inst::CameFrom { t, .. } => InstKind::Value(t),
        Inst::Ret { .. } => InstKind::Terminator,
        Inst::BoolNegate { .. } => InstKind::BOOL,
        Inst::BitNot { v } => get_value_kind(ir, types, v),
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
        Inst::IntAdd { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntSub { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntMul { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntDivUnsigned { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntDivSigned { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntRemUnsigned { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntRemSigned { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::IntCmp { .. } => InstKind::BOOL,
        Inst::FloatAdd { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::FloatSub { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::FloatMul { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::FloatDiv { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::FloatRem { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::FloatCmp { .. } => InstKind::BOOL,
        Inst::BitAnd { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::BitOr { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::BitXor { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::BitShiftLeft { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::BitUnsignedShiftRight { lhs, .. } => get_value_kind(ir, types, lhs),
        Inst::BitSignedShiftRight { lhs, .. } => get_value_kind(ir, types, lhs),
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
    pub const BOOL: InstKind = Self::scalar(ScalarType::U8);
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
    fn is_u8(&self) -> bool {
        matches!(self, InstKind::Value(pt) if pt.is_u8())
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
    if let Some(err) = f.body_failure.clone() {
        return Err(K1Message {
            message: format!(
                "Cannot generate ir for function {}, which failed compilation",
                b.k1.ident_str(f.name)
            ),
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
            BuilderVariable { id: param.variable_id, value, storage_pt: t, indirect: false };
        b.k1.ir.b_variables.push(builder_variable);
    }

    let f = b.k1.get_function(function_id);
    if let Some(body_block) = f.body_block {
        let entry_block = b.push_block("entry");
        b.goto_block(entry_block);
        compile_block_stmts(&mut b, None, body_block)?;
    } else {
        match f.linkage {
            Linkage::Standard => panic!("ir: function should have a body I think"),
            Linkage::External { .. } => {}
            Linkage::Intrinsic => {}
        }
    };

    let unit_id = IrUnitId::Function(function_id);
    let builtin_kind = match intrinsic_type {
        None => None,
        Some(i) => match builtin_handler(i) {
            BuiltinHandler::Backend(backend_builtin) => Some(backend_builtin),
            _ => None,
        },
    };
    let unit = finalize_unit(&mut b, return_type_id, unit_id, phys_fn_type, is_debug, builtin_kind);

    *b.k1.ir.functions.get_mut(function_id) = Some(unit);

    if is_debug {
        let s = unit_to_string(b.k1, unit_id, true);
        eprintln!("{s}");
    }

    validate_unit(k1, unit_id)?;

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
        b.k1.ir.b_variables.push(BuilderVariable {
            id: *variable_id,
            value: Value::StaticValue { t: pt, id: *static_value_id },
            storage_pt: pt,
            indirect: !pt.is_agg(),
        })
    }

    let return_type_id = b.k1.exprs.get_type(expr);
    let (return_type, diverges) = b.get_function_return_type(return_type_id);
    let params = MSlice::empty();
    let phys_fn_type =
        PhysicalFunctionType { return_type, diverges, params, abi_mode: AbiMode::Internal };
    b.fn_type = phys_fn_type;

    debug!("Compiling expr {}", b.k1.expr_to_string(expr));
    let entry_block = b.push_block("expr_toplevel");
    b.goto_block(entry_block);
    let _result = compile_expr(&mut b, None, expr)?;
    let compiled_expr =
        finalize_unit(&mut b, return_type_id, IrUnitId::Expr(expr), phys_fn_type, is_debug, None);
    b.k1.ir.exprs.insert(expr, compiled_expr);

    let unit_id = IrUnitId::Expr(expr);
    validate_unit(k1, unit_id)?;

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
) -> IrUnit {
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
        blocks: b.blocks,
        function_builtin_kind: builtin_kind,
        is_debug,
        inline_done: false,
    };
    b.k1.ir.b_variables.clear();
    b.k1.ir.b_loops.clear();
    unit
}

struct BuilderVariable {
    id: VariableId,
    value: Value,
    storage_pt: PhysicalType,
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

            last_alloca_index: None,
            cur_block: Handle::nil(),
            cur_span: SpanId::NONE,
            entry_span: SpanId::NONE,
        }
    }

    fn make_inst(&mut self, inst: Inst, comment: IrStr, debug_info: IrDebugInfo) -> InstId {
        let span = self.cur_span;
        self.k1.ir.add_inst(inst, comment, debug_info, span)
    }

    fn push_alloca(&mut self, pt: PhysicalType, comment: impl Into<IrStr>) -> InstId {
        self.push_alloca_ext(pt, comment, IrDebugInfo::default(), false)
    }

    fn push_alloca_ext(
        &mut self,
        pt: PhysicalType,
        comment: impl Into<IrStr>,
        debug_info: IrDebugInfo,
        returned: bool,
    ) -> InstId {
        let layout = self.k1.types.get_pt_layout(pt);
        let index = match self.last_alloca_index {
            None => 0,
            Some(i) => i as usize + 1,
        };
        let alloca_span = self.entry_span;
        let inst_id = self.k1.ir.add_inst(
            Inst::Alloca { t: pt, vm_layout: layout, returned },
            comment.into(),
            debug_info,
            alloca_span,
        );
        let mut first_block = self.k1.ir.mem.get_raw_ref(self.blocks.first);
        self.k1.ir.mem.dlist_insert(&mut first_block.data.instrs, index, inst_id);
        self.last_alloca_index = Some(index as u32);
        inst_id
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.k1.ir, &self.k1.types, inst)
    }

    pub fn get_value_kind(&self, value: Value) -> InstKind {
        get_value_kind(&self.k1.ir, &self.k1.types, value)
    }

    fn push_inst(&mut self, inst: Inst, comment: impl Into<IrStr>) -> InstId {
        let id = self.make_inst(inst, comment.into(), IrDebugInfo::default());

        let blocks = self.k1.ir.mem.get_raw_ref(self.cur_block).as_mut();
        self.k1.ir.mem.dlist_push(&mut blocks.data.instrs, id);
        id
    }

    fn push_inst_anon(&mut self, inst: Inst) -> InstId {
        self.push_inst(inst, "")
    }

    fn push_struct_offset(
        &mut self,
        struct_agg_id: AggregateTypeId,
        base: Value,
        field_index: u32,
        comment: impl Into<IrStr>,
    ) -> Value {
        if field_index == 0 {
            return base;
        }
        let Some(offset) = self.k1.types.get_struct_field_offset(struct_agg_id, field_index) else {
            b_ice!(self, "Failed getting offset for field")
        };
        self.push_inst(
            Inst::StructOffset { struct_t: struct_agg_id, base, field_index, vm_offset: offset },
            comment.into(),
        )
        .as_value()
    }

    fn push_jump(&mut self, block_id: BlockId, comment: impl Into<IrStr>) -> InstId {
        self.push_inst(Inst::Jump(block_id), comment)
    }

    fn push_jump_if(
        &mut self,
        cond: Value,
        cons: BlockId,
        alt: BlockId,
        comment: impl Into<IrStr>,
    ) -> InstId {
        if let Value::Data32 { t: ScalarType::U8, data: b32 } = cond {
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
        comment: impl Into<IrStr>,
    ) -> Option<InstId> {
        let layout = self.k1.types.get_pt_layout(pt);
        if pt.is_empty() {
            None
        } else {
            let copy_inst =
                self.push_inst(Inst::Copy { dst, src, t: pt, vm_size: layout.size }, comment);
            Some(copy_inst)
        }
    }

    fn push_load(&mut self, st: ScalarType, src: Value, comment: impl Into<IrStr>) -> InstId {
        self.push_inst(Inst::Load { t: st, src }, comment)
    }

    fn push_store(&mut self, dst: Value, value: Value, comment: impl Into<IrStr>) -> InstId {
        let t = self.get_value_kind(value).expect_value().unwrap().expect_scalar();
        self.push_inst(Inst::Store { dst, value, t }, comment)
    }

    fn make_int_value(&mut self, int_value: &TypedIntValue, comment: impl Into<IrStr>) -> Value {
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

    fn push_block(&mut self, name: &'static str) -> BlockId {
        let node =
            self.k1.ir.mem.dlist_push(&mut self.blocks, Block { name, instrs: Dlist::empty() });
        node
        // let id = self.block_count;
        // Recycle builder blocks
        // match self.k1.ir.b_blocks.get_mut(self.block_count as usize) {
        //     Some(recycled) => {
        //         self.block_count += 1;
        //         recycled.name = name;
        //     }
        //     None => {
        //         self.block_count += 1;
        //         self.k1.ir.b_blocks.push(Block { name, instrs: Vec::with_capacity(256) });
        //     }
        // }
        // id as BlockId
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
        self.k1.ir.b_variables.iter().find(|bv| bv.id == variable_id)
    }

    fn get_physical_type_result(&mut self, type_id: TypeId) -> PhysicalTypeResult {
        self.k1.get_physical_type(type_id)
    }

    fn get_physical_type(&mut self, type_id: TypeId) -> PhysicalType {
        match self.get_physical_type_result(type_id) {
            PhysicalTypeResult::Never => {
                b_ice!(self, "Type is divergent: {}", self.k1.type_id_to_string_ext(type_id, true))
            }
            PhysicalTypeResult::No => b_ice!(
                self,
                "Not a physical type: {}",
                self.k1.type_id_to_string_ext(type_id, true)
            ),
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
        for (index, param) in
            self.k1.types.mem.getn(function_type.physical_params).iter().enumerate()
        {
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
            params: phys_params.into_handle(&mut self.k1.ir.mem),
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

    fn get_instr_block(&self, inst_id: InstId) -> IrHandle<BlockNode> {
        // nocommit: inst to block is terrible
        self.k1
            .ir
            .mem
            .dlist_iter_with_handles(self.blocks)
            .find(|(_h, b)| self.k1.ir.mem.dlist_iter(b.data.instrs).any(|i| *i == inst_id))
            .unwrap()
            .0
    }

    fn _locate_inst(&self, inst_id: InstId) -> (BlockId, Handle<InstNode, ProgramIr>, usize) {
        for (block_handle, block) in self.k1.ir.mem.dlist_iter_with_handles(self.blocks) {
            for (index, (inst_handle, inst)) in
                self.k1.ir.mem.dlist_iter_with_handles(block.data.instrs).enumerate()
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
        let after_insts = self.k1.ir.mem.dlist_split_at_node(&mut block_ref.data.instrs, inst_node);
        let after_block = self.k1.ir.mem.dlist_insert_after(
            &mut self.blocks,
            block_node,
            Block { name: "rsplit", instrs: after_insts },
        );
        after_block
    }
}

fn store_scalar_if_dst(b: &mut Builder, dst: Option<Value>, value: Value) -> Value {
    match dst {
        None => value,
        Some(dst) => {
            b.push_store(dst, value, "store scalar to dst");
            dst
        }
    }
}

fn store_rich_if_dst(
    b: &mut Builder,
    dst: Option<Value>,
    pt: PhysicalType,
    value: Value,
    comment: impl Into<IrStr>,
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
        return failf!(b.cur_span, "body is not a block");
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

            let rich_type_id = if let_stmt.is_referencing {
                b.k1.types.get(let_stmt.variable_type).as_reference().unwrap().inner_type
            } else {
                let_stmt.variable_type
            };
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
                b.k1.ir.b_variables.push(BuilderVariable {
                    id: let_stmt.variable_id,
                    value: Value::Empty,
                    storage_pt: rich_pt,
                    indirect: false,
                });
                Ok(Value::Empty)
            } else {
                let variable_alloca =
                    b.push_alloca_ext(rich_pt, "source let", debug_info, returned);

                if let Some(init) = let_stmt.initializer {
                    compile_expr(b, Some(variable_alloca.as_value()), init)?;
                }
                // Referencing lets bind to a pointer value, which is exactly what the
                // alloca slot is, so they are 'direct'.
                // Non-referencing lets bind to the dereferenced value, but are backed by an alloca
                // slot, so they are 'indirect' (need loading on rvalue access)
                // non-referencing int -> indirect
                // non-referencing agg -> direct
                let is_direct = if rich_pt.is_agg() { true } else { let_stmt.is_referencing };
                b.k1.ir.b_variables.push(BuilderVariable {
                    id: let_stmt.variable_id,
                    value: variable_alloca.as_value(),
                    storage_pt: var_pt,
                    indirect: !is_direct,
                });
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
                    let builder_variable = b.get_variable(v.variable_id).expect("Missing variable");
                    //if !builder_variable.indirect {
                    //    b.k1.ice_with_span(
                    //        "Expect an indirect variable for value assignment",
                    //        b.k1.exprs.get_span(ass.destination),
                    //    )
                    //};
                    let variable_value = builder_variable.value;
                    let _rhs_stored = compile_expr(b, Some(variable_value), ass.value)?;
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
            let require_continue_block = b.push_block("require_continue");
            let require_else_block = match req.else_body {
                None => None,
                Some(_) => Some(b.push_block("require_else")),
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

pub enum BuiltinHandler {
    IrBakeStaticValue,
    IrZeroed,
    IrBoolNegate,
    IrBitNot,
    IrBitCast,
    IrArithBinop(ArithOpKind),
    IrBitwiseBinop(BitwiseBinopKind),
    IrPointerIndex,
    Typer,
    Backend(BackendBuiltin),
}
pub fn builtin_handler(intrinsic_op: Builtin) -> BuiltinHandler {
    use BuiltinHandler as H;
    match intrinsic_op {
        Builtin::SizeOf => H::Typer,
        Builtin::SizeOfStride => H::Typer,
        Builtin::AlignOf => H::Typer,
        Builtin::CompilerSourceLocation => H::Typer,
        Builtin::GetStaticValue => H::Typer,
        Builtin::StaticTypeToValue => H::Typer,
        Builtin::TypeId => H::Typer,
        Builtin::EnumGetValue => H::Typer,

        Builtin::BakeStaticValue => H::IrBakeStaticValue,
        Builtin::CompilerMessage => H::Backend(BackendBuiltin::CompilerMessage),
        Builtin::Zeroed => H::IrZeroed,

        Builtin::TypeName => H::Backend(BackendBuiltin::TypeName),
        Builtin::TypeSchema => H::Backend(BackendBuiltin::TypeSchema),

        Builtin::BoolNegate => H::IrBoolNegate,
        Builtin::BitNot => H::IrBitNot,
        Builtin::BitCast => H::IrBitCast,
        Builtin::ArithBinop(kind) => H::IrArithBinop(kind),
        Builtin::BitwiseBinop(kind) => H::IrBitwiseBinop(kind),
        Builtin::PointerIndex => H::IrPointerIndex,

        Builtin::Allocate => H::Backend(BackendBuiltin::Allocate),
        Builtin::AllocateZeroed => H::Backend(BackendBuiltin::AllocateZeroed),
        Builtin::Reallocate => H::Backend(BackendBuiltin::Reallocate),
        Builtin::Free => H::Backend(BackendBuiltin::Free),
        Builtin::MemCopy => H::Backend(BackendBuiltin::MemCopy),
        Builtin::MemSet => H::Backend(BackendBuiltin::MemSet),
        Builtin::MemEquals => H::Backend(BackendBuiltin::MemEquals),
        Builtin::Exit => H::Backend(BackendBuiltin::Exit),
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
                None => b.push_alloca(struct_pt, "struct literal").as_value(),
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
                            "struct lit field ptr",
                        );
                        compile_expr(b, Some(struct_offset), expr)?;
                    }
                }
            }
            Ok(struct_base)
        }
        TypedExpr::StructFieldAccess(field_access) => {
            let struct_base = compile_expr(b, None, field_access.base)?;
            let struct_pt_id = b.get_physical_type(field_access.struct_type).expect_agg();
            let field_ptr = b.push_struct_offset(
                struct_pt_id,
                struct_base,
                field_access.field_index,
                "struct field access",
            );
            let result_type = b.get_physical_type(expr_type);
            let result =
                build_field_access(b, field_access.access_kind, dst, field_ptr, result_type);
            Ok(result)
        }
        TypedExpr::ArrayGetElement(array_get) => {
            let array_base = compile_expr(b, None, array_get.base)?;
            let array_agg_id = b.get_physical_type(array_get.array_type).expect_agg();
            let (element_pt, _len) = b.k1.types.agg_types.get(array_agg_id).agg_type.expect_array();
            let index = compile_expr(b, None, array_get.index)?;

            let element_ptr = b.push_inst(
                Inst::ArrayOffset { element_t: element_pt, base: array_base, element_index: index },
                "array get offset",
            );
            let result_type = b.get_physical_type(expr_type);
            let result = build_field_access(
                b,
                array_get.access_kind,
                dst,
                element_ptr.as_value(),
                result_type,
            );
            Ok(result)
        }
        TypedExpr::Variable(variable_expr) => {
            let (var_value, var_pt, is_indirect) =
                compile_variable_to_address(b, variable_expr.variable_id);
            if is_indirect {
                // indirect; var_value is storage that holds the variable's type
                let loaded_or_copied =
                    load_or_copy(b, var_pt, dst, var_value, false, "fulfill variable usage");
                Ok(loaded_or_copied)
            } else {
                // direct; var_value is already a canonical representation of the value; just have to
                // fulfill dst
                let stored = store_rich_if_dst(b, dst, var_pt, var_value, "direct variable");
                Ok(stored)
            }
        }
        TypedExpr::AddressOf(address_of) => {
            let variable_id = address_of.target_variable;
            let (var_addr_value, _is_indirect, _var_storage_pt) =
                compile_variable_to_address(b, variable_id);

            #[cfg(debug_assertions)]
            if !b.get_value_kind(var_addr_value).is_ptr() {
                b.k1.ice_span(b.cur_span, "Address-of yielded non-ptr")
            }

            let stored =
                store_rich_if_dst(b, dst, PhysicalType::PTR, var_addr_value, "fulfill address of");
            Ok(stored)
        }
        TypedExpr::Deref(deref) => {
            let src = compile_expr(b, None, deref.target)?;
            let target_pt = b.get_physical_type(expr_type);
            let loaded = load_or_copy(
                b,
                target_pt,
                dst,
                src,
                true,
                if dst.is_some() { "lang deref fulfill to dst" } else { "lang deref no dst" },
            );
            Ok(loaded)
        }
        TypedExpr::Block(_) => {
            let Some(last) = compile_block_stmts(b, dst, expr)? else {
                return failf!(b.cur_span, "Block has no value");
            };
            Ok(last)
        }
        TypedExpr::Call { call_id } => {
            let call = b.k1.calls.get(call_id).clone();

            let function_type_id = b.k1.get_callee_function_type(&call.callee);
            let callee_fn_type = b.get_physical_fn_type(function_type_id);

            let maybe_function_id = call.callee.maybe_function_id();
            let (intrinsic_op, linkage) = match maybe_function_id {
                None => (None, None),
                Some(f_id) => {
                    let f = b.k1.get_function(f_id);
                    (f.builtin_type, Some(f.linkage))
                }
            };
            let (callee, environment_arg) = match (intrinsic_op, linkage) {
                (Some(intrinsic), _) => {
                    let backend_builtin = match builtin_handler(intrinsic) {
                        BuiltinHandler::IrBakeStaticValue => {
                            return {
                                // intern fn bakeStaticValue[T](value: T): u64
                                let type_id = b.k1.mem.get_nth(call.type_args, 0).type_id;
                                let _physical_type = b.get_physical_type(type_id);

                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let value = compile_expr(b, None, arg0)?;
                                let bake =
                                    b.push_inst_anon(Inst::BakeStaticValue { type_id, value });

                                // Produces a type id, which is a scalar
                                let stored = store_rich_if_dst(
                                    b,
                                    dst,
                                    callee_fn_type.return_type,
                                    bake.as_value(),
                                    "",
                                );
                                Ok(stored)
                            };
                        }
                        BuiltinHandler::IrZeroed => {
                            return {
                                let type_id = b.k1.mem.get_nth(call.type_args, 0).type_id;
                                let pt = b.get_physical_type(type_id);
                                match pt.as_enum() {
                                    PhysicalTypeEnum::Empty => Ok(Value::Empty),
                                    PhysicalTypeEnum::Agg(agg_id) => {
                                        let pt_layout = b.k1.types.agg_types.get(agg_id).layout;
                                        let dst = match dst {
                                            None => b.push_alloca(pt, "zeroed no dst").as_value(),
                                            Some(dst) => dst,
                                        };
                                        let zero_u8 = Value::byte(0);
                                        // intern fn set(dst: ptr, value: u8, count: size): unit
                                        let count = b.make_int_value(
                                            &TypedIntValue::I64(pt_layout.size as i64),
                                            "memset size",
                                        );
                                        let memset_args = b.k1.ir.mem.pushn(&[dst, zero_u8, count]);
                                        let Some(memset_function_id) = b.k1.scopes.find_function(
                                            b.k1.scopes.mem_scope_id,
                                            b.k1.ast.idents.b.set,
                                        ) else {
                                            b_ice!(b, "Missing memset function");
                                        };
                                        let memset_call = IrCall {
                                            ret_type: PhysicalType::EMPTY,
                                            callee: IrCallee::Builtin(
                                                memset_function_id,
                                                BackendBuiltin::MemSet,
                                            ),
                                            args: memset_args,
                                            dst: None,
                                        };
                                        let call_id = b.k1.ir.calls.add(memset_call);
                                        b.push_inst(Inst::Call { call_id }, "zeroed memset");
                                        Ok(dst)
                                    }
                                    PhysicalTypeEnum::Scalar(st) => {
                                        let zero_value = zero(st);
                                        let stored = store_scalar_if_dst(b, dst, zero_value);
                                        Ok(stored)
                                    }
                                }
                            };
                        }
                        BuiltinHandler::IrBoolNegate => {
                            return {
                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let base = compile_expr(b, None, arg0)?;
                                let neg = b.push_inst_anon(Inst::BoolNegate { v: base });
                                let stored = store_scalar_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        BuiltinHandler::IrBitNot => {
                            return {
                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let base = compile_expr(b, None, arg0)?;
                                let neg = b.push_inst_anon(Inst::BitNot { v: base });
                                let stored = store_scalar_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        BuiltinHandler::IrBitCast => {
                            return {
                                let from_type_id = b.k1.mem.get_nth(call.type_args, 0).type_id;
                                let to_type_id = b.k1.mem.get_nth(call.type_args, 1).type_id;

                                let from_pt = b.get_physical_type(from_type_id);
                                let to_pt = b.get_physical_type(to_type_id);

                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let from_value = compile_expr(b, None, arg0)?;
                                match (from_pt.as_enum(), to_pt.as_enum()) {
                                    (PhysicalTypeEnum::Empty, _) | (_, PhysicalTypeEnum::Empty) => {
                                        return failf!(
                                            b.cur_span,
                                            "Cannot bitcast to or from empty type"
                                        );
                                    }
                                    (PhysicalTypeEnum::Scalar(_), PhysicalTypeEnum::Scalar(_)) => {
                                        // Note that this also covers Pointer to Pointer
                                        let bitcast = b.push_inst_anon(Inst::BitCast {
                                            v: from_value,
                                            to: to_pt,
                                        });
                                        let stored = store_rich_if_dst(
                                            b,
                                            dst,
                                            to_pt,
                                            bitcast.as_value(),
                                            "fulfill bitcast destination",
                                        );
                                        Ok(stored)
                                    }
                                    (PhysicalTypeEnum::Scalar(_), PhysicalTypeEnum::Agg(_)) => {
                                        // We need a place, so its alloca time
                                        let locn = match dst {
                                            Some(dst) => dst,
                                            None => b
                                                .push_alloca(to_pt, "bitcast scalar to agg place")
                                                .as_value(),
                                        };

                                        // We know a scalar store will work
                                        let _stored = b.push_store(
                                            locn,
                                            from_value,
                                            "bitcast scalar to agg store",
                                        );
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
                                            "bitcast agg to scalar",
                                        );
                                        Ok(loaded)
                                    }
                                    (PhysicalTypeEnum::Agg(_), PhysicalTypeEnum::Agg(_)) => {
                                        // Make a copy to a definitely-aligned destination.
                                        let locn = match dst {
                                            Some(dst) => dst,
                                            None => b
                                                .push_alloca(to_pt, "bitcast agg to agg place")
                                                .as_value(),
                                        };
                                        let _copied = b.push_copy(
                                            locn,
                                            from_value,
                                            from_pt,
                                            "bitcast agg to agg copy",
                                        );
                                        Ok(locn)
                                    }
                                }
                            };
                        }
                        BuiltinHandler::IrArithBinop(op) => {
                            return compile_arith_binop(b, op, &call, dst);
                        }
                        BuiltinHandler::IrBitwiseBinop(op) => {
                            return {
                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let lhs = compile_expr(b, None, arg0)?;
                                let arg1 = *b.k1.mem.get_nth(call.args, 1);
                                let rhs = compile_expr(b, None, arg1)?;
                                let lhs_pt = b.get_value_kind(lhs).expect_value().unwrap();
                                let width = b.k1.types.get_pt_layout(lhs_pt).size_bits() as u8;
                                let inst = match op {
                                    BitwiseBinopKind::And => Inst::BitAnd { lhs, rhs, width },
                                    BitwiseBinopKind::Or => Inst::BitOr { lhs, rhs, width },
                                    BitwiseBinopKind::Xor => Inst::BitXor { lhs, rhs, width },
                                    BitwiseBinopKind::ShiftLeft => {
                                        Inst::BitShiftLeft { lhs, rhs, width }
                                    }
                                    BitwiseBinopKind::UnsignedShiftRight => {
                                        Inst::BitUnsignedShiftRight { lhs, rhs, width }
                                    }
                                    BitwiseBinopKind::SignedShiftRight => {
                                        Inst::BitSignedShiftRight { lhs, rhs, width }
                                    }
                                };
                                let res = b.push_inst_anon(inst);
                                let stored = store_scalar_if_dst(b, dst, res.as_value());
                                Ok(stored)
                            };
                        }
                        BuiltinHandler::IrPointerIndex => {
                            return {
                                // intern fn refAtIndex[T](self: Pointer, index: uword): T*
                                let elem_type_id = b.k1.mem.get_nth(call.type_args, 0).type_id;
                                let elem_pt = b.get_physical_type(elem_type_id);
                                let arg0 = *b.k1.mem.get_nth(call.args, 0);
                                let base = compile_expr(b, None, arg0)?;
                                let arg1 = *b.k1.mem.get_nth(call.args, 1);
                                let element_index = compile_expr(b, None, arg1)?;
                                let offset = b.push_inst(
                                    Inst::ArrayOffset { element_t: elem_pt, base, element_index },
                                    "refAtIndex offest",
                                );
                                let stored = store_scalar_if_dst(b, dst, offset.as_value());
                                Ok(stored)
                            };
                        }
                        BuiltinHandler::Typer => unreachable!(),
                        BuiltinHandler::Backend(backend_builtin) => backend_builtin,
                    };
                    let Some(function_id) = maybe_function_id else {
                        b_ice!(b, "Missing function id for intrinsic {:?}", intrinsic)
                    };
                    (IrCallee::Builtin(function_id, backend_builtin), None)
                }
                (_, Some(Linkage::External { lib_name, fn_name, .. })) => {
                    let function_id = maybe_function_id.unwrap();
                    let function_name = match fn_name {
                        None => b.k1.get_function(function_id).name,
                        Some(fn_name) => fn_name,
                    };
                    (IrCallee::Extern { library_name: lib_name, function_name, function_id }, None)
                }
                _ => match &call.callee {
                    Callee::StaticFunction(function_id) => (IrCallee::Direct(*function_id), None),
                    Callee::StaticLambda { function_id, lambda_value_expr, .. } => {
                        let lambda_env = compile_expr(b, None, *lambda_value_expr)?;
                        let lambda_env_type_id = b.k1.exprs.get_type(*lambda_value_expr);
                        let env_pt = b.get_physical_type(lambda_env_type_id);
                        //b.k1.write_location(&mut stderr(), b.cur_span);
                        // TODO: Lambda environments really need to go on the arena
                        // Here is where we literally put the env on the stack, even if it contains
                        // no stack-volatile things!
                        let env_ptr = b.push_alloca(env_pt, "lambda env location").as_value();
                        store_value(b, env_pt, env_ptr, lambda_env, "store lambda env for call");
                        (IrCallee::Direct(*function_id), Some(env_ptr))
                    }
                    Callee::Abstract { .. } => return failf!(b.cur_span, "ir abstract callee"),
                    Callee::Builtin { .. } => return failf!(b.cur_span, "ir builtin callee"),
                    Callee::DynamicLambda(dl) => {
                        let lambda_obj = compile_expr(b, None, *dl)?;
                        let lam_obj_type_id = b.k1.types.builtins.dyn_lambda_obj.unwrap();
                        let lam_obj_pt = b.get_physical_type(lam_obj_type_id).expect_agg();
                        let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
                        let fn_ptr_addr = b.push_struct_offset(
                            lam_obj_pt,
                            lambda_obj,
                            TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
                            "dyn lam fn ptr offset",
                        );
                        let fn_ptr = load_value(b, ptr_pt, fn_ptr_addr, false, "");
                        let env_addr = b.push_struct_offset(
                            lam_obj_pt,
                            lambda_obj,
                            TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
                            "dyn lam env ptr offset",
                        );
                        let env = load_value(b, ptr_pt, env_addr, false, "");

                        (IrCallee::Indirect(callee_fn_type, fn_ptr), Some(env))
                    }
                    Callee::DynamicFunction { function_pointer_expr } => {
                        let callee_inst = compile_expr(b, None, *function_pointer_expr)?;
                        (IrCallee::Indirect(callee_fn_type, callee_inst), None)
                    }
                    Callee::DynamicAbstract { .. } => {
                        return failf!(b.cur_span, "ir abstract call");
                    }
                },
            };

            // Add function to compile queue, and (maybe) do some inlining one day!
            if let Some(function_id) = callee.known_function_id() {
                match b.k1.ir.functions.get(function_id) {
                    None => {
                        if !b.k1.ir.b_units_pending_compile.contains(&function_id) {
                            b.k1.ir.b_units_pending_compile.push(function_id);
                        }
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
            let args_handle = b.k1.ir.mem.list_to_handle(args);
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
            for stmt in b.k1.mem.getn(match_expr.initial_let_statements) {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.k1.ir.mem.new_list(match_expr.arms.len());
            for _arm in b.k1.mem.getn(match_expr.arms).iter() {
                let arm_block = b.push_block("arm_cond");
                let arm_consequent_block = b.push_block("arm_cons");
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_jump(first_arm_block, "enter match");

            let fail_block = b.push_block("match_fail");
            b.goto_block(fail_block);
            b.push_inst_anon(Inst::Unreachable);

            let match_end_block = b.push_block("match_end");
            b.goto_block(match_end_block);
            let result_inst_kind = b.type_to_inst_kind(match_result_type);
            let (result_came_from, is_empty) = match result_inst_kind {
                InstKind::Value(pt) => {
                    if pt.is_empty() {
                        (None, true)
                    } else {
                        let came_from_inst = b.push_inst(
                            Inst::CameFrom { t: pt, incomings: MSlice::empty() },
                            "match phi",
                        );
                        (Some(came_from_inst), false)
                    }
                }
                InstKind::Void => {
                    return failf!(b.cur_span, "come from void");
                }
                InstKind::Terminator => (None, false),
            };

            let mut incomings: List<CameFromCase, _> = b.k1.ir.mem.new_list(match_expr.arms.len());
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
                let result = compile_expr(b, None, arm.consequent_expr)?;
                let cons_diverges = b.get_value_kind(result).is_terminator();
                debug_assert_eq!(
                    b.k1.exprs.get_type(arm.consequent_expr) == NEVER_TYPE_ID,
                    cons_diverges
                );

                if !cons_diverges {
                    let current_block = b.cur_block;
                    incomings.push(CameFromCase { from: current_block, value: result });
                    b.push_jump(match_end_block, "");
                }
            }
            b.goto_block(match_end_block);
            match result_came_from {
                None => {
                    if is_empty {
                        Ok(Value::Empty)
                    } else {
                        // match is divergent
                        let inst = b.push_inst(Inst::Unreachable, "divergent match");
                        Ok(inst.as_value())
                    }
                }
                Some(came_from) => {
                    let real_incomings = b.k1.ir.mem.list_to_handle(incomings);
                    let Inst::CameFrom { incomings: i, .. } = b.k1.ir.instrs.get_mut(came_from)
                    else {
                        unreachable!()
                    };
                    *i = real_incomings;
                    let pt_id = b.get_physical_type(match_result_type);
                    Ok(store_rich_if_dst(
                        b,
                        dst,
                        pt_id,
                        came_from.as_value(),
                        "deliver match result value",
                    ))
                }
            }
        }
        TypedExpr::WhileLoop(w) => {
            let cond_block = b.push_block("while_loop_condition");
            let loop_body_block = b.push_block("while_loop_body");
            let end_block = b.push_block("while_loop_end");
            let TypedExpr::Block(body_block) = b.k1.exprs.get(w.body) else { unreachable!() };
            let loop_scope_id = body_block.scope_id;
            b.k1.ir.b_loops.insert(loop_scope_id, LoopInfo { break_value: None, end_block });

            b.push_jump(cond_block, "enter while cond");

            b.goto_block(cond_block);
            compile_matching_condition(b, &w.condition, loop_body_block, Some(end_block))?;

            b.goto_block(loop_body_block);
            let last = compile_block_stmts(b, None, w.body)?;
            if last.is_some_and(|v| !b.get_value_kind(v).is_terminator()) {
                b.push_jump(cond_block, "goto while cond");
            }

            b.goto_block(end_block);
            Ok(Value::Empty)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            let loop_body_block = b.push_block("loop_body");
            let loop_end_block = b.push_block("loop_end");

            let break_pt_id = b.get_physical_type(expr_type);

            let break_value = if expr_type != b.k1.types.builtins.empty {
                Some(b.push_alloca(break_pt_id, "loop break value"))
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
            b.push_jump(loop_body_block, "enter loop");
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if body_value.is_some_and(|v| !b.get_value_kind(v).is_terminator()) {
                b.push_jump(loop_body_block, "da capo maestro");
            }

            b.goto_block(loop_end_block);
            if let Some(break_alloca) = break_value {
                let stored = load_or_copy(
                    b,
                    break_pt_id,
                    dst,
                    break_alloca.as_value(),
                    false,
                    "fulfill loop break dst",
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
                let jmp = b.push_jump(end_block, "break loop (with value)");
                Ok(jmp.as_value())
            } else {
                compile_expr(b, None, brk.value)?;
                let jmp = b.push_jump(end_block, "break loop (no value)");
                Ok(jmp.as_value())
            }
        }
        TypedExpr::SumConstructor(sum_c) => {
            let sum_pt = b.get_physical_type(expr_type);
            let sum_agg_id = sum_pt.expect_agg();
            let sum_pt_agg = b.k1.types.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let variants = sum_pt_agg.variants;
            let sum_struct_repr = sum_pt_agg.struct_repr;
            let sum_base = match dst {
                Some(dst) => dst,
                None => b.push_alloca(sum_pt, "sum literal storage").as_value(),
            };

            let tag_base = sum_base;
            let sum_variant = b.k1.types.mem.get_nth(variants, sum_c.variant_index as usize);
            let tag_int_value = sum_variant.tag;
            let int_imm = b.make_int_value(&tag_int_value, "sum tag");
            b.push_store(tag_base, int_imm, "store sum lit tag");

            if let Some(payload_expr) = &sum_c.payload {
                let payload_offset =
                    b.push_struct_offset(sum_struct_repr, sum_base, 1, "sum payload ptr");
                let _payload_value = compile_expr(b, Some(payload_offset), *payload_expr)?;
            }

            Ok(sum_base)
        }
        TypedExpr::SumGetTag(sum_get_tag) => {
            let sum_base = compile_expr(b, None, sum_get_tag.sum_expr_or_reference)?;
            let sum_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.exprs.get_type(sum_get_tag.sum_expr_or_reference))
                    .expect_sum();
            let tag_scalar = PhysicalType::scalar(sum_type.tag_type.get_scalar_type());

            // Load straight from the sum base, dont bother with a struct gep
            Ok(load_or_copy(
                b,
                tag_scalar,
                dst,
                sum_base,
                false,
                "get sum tag, load or copy to dst",
            ))
        }
        TypedExpr::SumGetPayload(sum_get_payload) => {
            let variant_index = sum_get_payload.variant_index;
            let sum_base_value = compile_expr(b, None, sum_get_payload.sum_expr)?;

            let base_expr_type_id = b.k1.exprs.get_type(sum_get_payload.sum_expr);
            let base_t = b.k1.types.get(base_expr_type_id);
            let sum_type_id = match sum_get_payload.access_kind {
                FieldAccessKind::ValueToValue => base_expr_type_id,
                FieldAccessKind::Dereference | FieldAccessKind::ReferenceThrough => {
                    base_t.expect_reference().inner_type
                }
            };
            let sum_agg_id = b.k1.get_physical_type(sum_type_id).unwrap().expect_agg();
            let sum_pt = b.k1.types.agg_types.get(sum_agg_id).agg_type.expect_sum();
            let variants = sum_pt.variants;
            let sum_struct_repr = sum_pt.struct_repr;
            let payload_offset =
                b.push_struct_offset(sum_struct_repr, sum_base_value, 1, "sum payload offset");
            if sum_get_payload.access_kind == FieldAccessKind::ReferenceThrough {
                // We're generating a pointer to the payload. The variant itself is a reference
                // and the value we produce here is just a pointer to the payload
                let stored = store_scalar_if_dst(b, dst, payload_offset);
                Ok(stored)
            } else {
                // We're loading the payload. The variant itself may or may not be a reference.
                // If it's a reference, we need to do a copying load to avoid incorrect aliasing
                // If it's not, we don't need to make a copy since the source is just a value
                // (albeit represented as an address)
                let make_copy = match sum_get_payload.access_kind {
                    FieldAccessKind::ValueToValue => false,
                    FieldAccessKind::Dereference => true,
                    FieldAccessKind::ReferenceThrough => unreachable!(),
                };
                let sum_variant = b.k1.types.mem.get_nth(variants, variant_index as usize);
                let payload_pt = sum_variant.payload.unwrap();
                let copied = load_or_copy(
                    b,
                    payload_pt,
                    dst,
                    payload_offset,
                    make_copy,
                    "deliver sum payload",
                );
                Ok(copied)
            }
        }
        TypedExpr::Enum(e) => {
            // Just compile to the integer
            let Type::Enum(enum_type) = b.k1.types.get(expr_type) else { unreachable!() };
            let value = b.k1.types.mem.get_nth(enum_type.member_values, e.value_index as usize);
            let value = b.make_int_value(&value.int_value, "enum int");
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
                None if return_pt.is_agg() => {
                    let rvo_storage =
                        b.push_alloca_ext(return_pt, "rvo storage", IrDebugInfo::default(), true);
                    Some(rvo_storage.as_value())
                }
                _ => None,
            };
            let value = compile_expr(b, dst, typed_return.value)?;
            let is_agg_return = b.fn_type.return_type.is_agg();
            let ret = b.push_inst(
                Inst::Ret { v: value, agg: is_agg_return },
                if is_agg_return { "return aggregate at address" } else { "" },
            );
            Ok(ret.as_value())
        }
        TypedExpr::Lambda(lam_expr) => {
            let lambda_type_id = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            let l = b.k1.types.lambda_types.get(lambda_type_id);
            let function_id = l.function_id;
            let env_struct = l.environment_struct;
            b.k1.ir.b_units_pending_compile.push(function_id);
            compile_expr(b, dst, env_struct)
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = Value::FunctionAddr(fpe.function_id);
            let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
            let stored = store_rich_if_dst(b, dst, ptr_pt, fp, "deliver fn pointer");
            b.k1.ir.b_units_pending_compile.push(fpe.function_id);
            Ok(stored)
        }
        TypedExpr::PendingCapture(_) => b_ice!(b, "ir on PendingCapture"),
        TypedExpr::StaticValue(stat) => {
            let t = b.get_physical_type(expr_type);
            // We lower the simple scalar static values
            // but leave the aggregates as globals
            match b.k1.static_values.get(stat.value_id) {
                StaticValue::Empty => Ok(Value::Empty),
                StaticValue::Bool(bv) => {
                    let imm = Value::byte(*bv as u8);
                    let store = store_scalar_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Char(byte) => {
                    let imm = Value::byte(*byte);
                    let store = store_scalar_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Int(int) => {
                    let int = *int;
                    let imm = b.make_int_value(&int, "static int");
                    let store = store_scalar_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Enum(_, int) => {
                    let int = *int;
                    let imm = b.make_int_value(&int, "static enum");
                    let store = store_scalar_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Float(float) => {
                    let float = *float;
                    //task(ir): Pack small floats
                    let imm = b.push_inst(Inst::Data(DataInst::Float(float)), "static float");
                    let store = store_scalar_if_dst(b, dst, imm.as_value());
                    Ok(store)
                }
                StaticValue::String(_)
                | StaticValue::Zero(_)
                | StaticValue::Struct(_)
                | StaticValue::Sum(_)
                | StaticValue::LinearContainer(_) => {
                    let value = Value::StaticValue { t, id: stat.value_id };
                    let stored = store_rich_if_dst(b, dst, t, value, "store static value to dst");
                    Ok(stored)
                }
            }
        }
    }
}

fn compile_variable_to_address(
    b: &mut Builder,
    variable_id: VariableId,
) -> (Value, PhysicalType, bool) {
    let variable = b.k1.variables.get(variable_id);
    match variable.global_id() {
        Some(global_id) => {
            // Globals are pretty complex. We always generate an instruction
            // representing the **address** of the global, because they are always
            // addresses to static memory. For aggregate types, that address _is_
            // the value of the expression referring to the global, but for
            // non-reference types, we must 'load' the value from the address, since
            // the address is just an implementation detail
            let value_type = variable.type_id;
            let value_pt = b.get_physical_type(value_type);
            let address = Value::GlobalAddr { storage_pt: value_pt, id: global_id };
            let is_direct = value_pt.is_agg();
            (address, value_pt, !is_direct)
        }
        None => {
            let Some(var) = b.get_variable(variable_id) else {
                eprintln!(
                    "Variables are: {}",
                    b.k1.ir
                        .b_variables
                        .iter()
                        .map(|bv| format!("{} {}", bv.id, bv.value))
                        .join("\n")
                );
                b.k1.ice_span(b.cur_span, "Missing variable")
            };
            let var_value = var.value;
            let var_indirect = var.indirect;
            (var_value, var.storage_pt, var_indirect)
        }
    }
}

fn build_field_access(
    b: &mut Builder,
    access_kind: FieldAccessKind,
    dst: Option<Value>,
    field_ptr: Value,
    result_pt: PhysicalType,
) -> Value {
    if access_kind == FieldAccessKind::ReferenceThrough {
        let stored = store_scalar_if_dst(b, dst, field_ptr);
        stored
    } else {
        // We're loading a field. The variant itself may or may not be a reference.
        // If it's a reference, we need to do a copying load to avoid incorrect aliasing
        // If it's not, we don't need to make a copy since the source is just a value
        // (albeit represented as an address)
        let make_copy = match access_kind {
            FieldAccessKind::ValueToValue => false,
            FieldAccessKind::Dereference => true,
            FieldAccessKind::ReferenceThrough => unreachable!(),
        };
        let comment = match make_copy {
            false => "field access no copy",
            true => "field access w copy",
        };
        let loaded = load_or_copy(b, result_pt, dst, field_ptr, make_copy, comment);
        loaded
    }
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
        | CastType::ReferenceToPointer => {
            let base_noop = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(target_type_id);
            let casted = b.push_inst(Inst::BitCast { v: base_noop, to: to_pt }, "cast noop");
            let stored =
                store_rich_if_dst(b, dst, to_pt, casted.as_value(), "fulfill cast destination");
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
            let extend = b.push_inst(Inst::IntExtU { v: bitcast.as_value(), to }, "bool_to_int");
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
        CastType::LambdaToLambdaObject => {
            let lambda_type_id = b.k1.get_expr_type(c.base_expr).as_lambda().unwrap();
            let lambda_type = b.k1.types.lambda_types.get(lambda_type_id);
            let lambda_function_id = lambda_type.function_id;

            let lambda_env_type = b.get_physical_type(lambda_type.env_type);
            // Representing the environment of lambda objects as a pointer
            // is problematic since the lambda object will only be 'good' as long as that pointer
            // is 'good', and currently we use stack space for it. But it could be that the
            // environment itself is very stable, for example 2 integers and a pointer to the heap.
            // But, all lambda objects must be the same size. It seems maybe we should
            // heap-allocate the environments, but in k1 that would mean using the current
            // allocator, or requiring one.
            // So I think I need to finalize a bit more what the 'current allocator' means, then
            // can update this to put it there. Likely an arena push
            let obj_struct_type = b.get_physical_type(b.k1.types.builtins.dyn_lambda_obj.unwrap());

            // Now we need a pointer to the environment
            // dyn lambda durability: Change to arena.push_struct call. This feels like
            //           code that might be able to live in typer.rs
            let lambda_env_ptr = b.push_alloca(lambda_env_type, "lambda env storage").as_value();

            // Produces just the lambda's environment as a value. We don't need the function
            // pointer because we know it from the type still
            // We store the environment directly into our stack pointer
            let _lambda_env_inst = compile_expr(b, Some(lambda_env_ptr), c.base_expr)?;

            let fn_ptr = Value::FunctionAddr(lambda_function_id);

            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.push_alloca(obj_struct_type, "lambda object storage").as_value(),
            };
            // Store fn ptr, then env ptr into the object
            let lam_obj_fn_ptr_addr = b.push_struct_offset(
                obj_struct_type.expect_agg(),
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
                "",
            );
            b.push_store(lam_obj_fn_ptr_addr, fn_ptr, "");

            let lam_obj_env_ptr_addr = b.push_struct_offset(
                obj_struct_type.expect_agg(),
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
                "",
            );
            b.push_store(lam_obj_env_ptr_addr, lambda_env_ptr, "");

            Ok(lam_obj_ptr)
        }
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
    let lhs_width = b.k1.types.get_pt_layout(lhs_pt).size_bits() as u8;
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
    let res = b.push_inst(inst, "");
    let stored = store_scalar_if_dst(b, dst, res.as_value());
    Ok(stored)
}

/// Loads a value of a given type from 'src'.
/// 'Load' in this context is an operation internal to this
/// IR; it doesn't have a direct analog in the source language.
/// A Dereference would the closest thing. But we take some liberties here;
/// such as treating this as a no-op for values that are already represented
/// by their location, aka IndirectValues
fn load_value(
    b: &mut Builder,
    pt: PhysicalType,
    src: Value,
    make_copy: bool,
    comment: impl Into<IrStr>,
) -> Value {
    match pt.as_enum() {
        PhysicalTypeEnum::Agg(_) => {
            if make_copy {
                let comment = comment.into();
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
    comment: impl Into<IrStr>,
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
    comment: impl Into<IrStr>,
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
        b.push_jump(cons_block, "empty condition");
        return Ok(());
    }
    for inst in b.k1.mem.getn(mc.instrs).iter() {
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(b, None, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_value: Value = compile_expr(b, None, *value)?;
                let continue_block = b.push_block("matching_cond_continue");

                // If the matching condition was typechecked as 'infallible', we don't have a fail
                // block, and we just jump to continue.
                match condition_fail_block {
                    None => b.push_jump(continue_block, "infallible matching cond continue"),
                    Some(fail_block) => {
                        b.push_jump_if(cond_value, continue_block, fail_block, "matching cond cond")
                    }
                };

                b.goto_block(continue_block);
            }
        }
    }
    b.push_jump(cons_block, "matching cond fallthrough cons");
    Ok(())
}

pub fn zero(t: ScalarType) -> Value {
    match t {
        ScalarType::U8 => Value::Data32 { t: ScalarType::U8, data: 0 },
        ScalarType::U16 => Value::Data32 { t: ScalarType::U16, data: 0 },
        ScalarType::U32 => Value::Data32 { t: ScalarType::U32, data: 0 },
        ScalarType::U64 => Value::Data32 { t: ScalarType::U64, data: 0 },
        ScalarType::I8 => Value::Data32 { t: ScalarType::I8, data: 0 },
        ScalarType::I16 => Value::Data32 { t: ScalarType::I16, data: 0 },
        ScalarType::I32 => Value::Data32 { t: ScalarType::I32, data: 0 },
        ScalarType::I64 => Value::Data32 { t: ScalarType::I64, data: 0 },
        ScalarType::F32 => Value::Data32 { t: ScalarType::F32, data: (0.0f32).to_bits() },
        ScalarType::F64 => {
            // Bit pattern is all zeroes anyway, but we still construct it via Rust's float
            // machinery
            Value::Data32 { t: ScalarType::F64, data: (0.0f64).to_bits() as u32 }
        }
        ScalarType::Pointer => Value::PTR_ZERO,
    }
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
        return failf!(span, "Not compiled");
    };
    for (block_index, block) in ir.mem.dlist_iter(unit.blocks).enumerate() {
        for inst_node in ir.mem.dlist_iter_nodes(block.instrs) {
            let inst_id = inst_node.data;
            let is_last = inst_node.is_last();
            let inst = ir.instrs.get(inst_id);
            let inst_kind = get_inst_kind(ir, &k1.types, inst_id);
            if !is_last && inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: stray terminator"))
            };
            if is_last && !inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: unterminated"))
            }

            match *inst {
                Inst::Data(_imm) => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, .. } => {
                    let dst_type = get_value_kind(ir, &k1.types, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("store dst v{} is not a ptr", inst_id))
                    }
                }
                Inst::Load { src, .. } => {
                    let src_kind = get_value_kind(ir, &k1.types, src);
                    if !src_kind.is_storage() {
                        errors.push(format!("i{inst_id}: load src is not storage"))
                    }
                }
                Inst::Copy { dst, src, .. } => {
                    let src_type = get_value_kind(ir, &k1.types, src);
                    if !src_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy src is not a ptr"))
                    }
                    let dst_type = get_value_kind(ir, &k1.types, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy dst v{} is not a ptr", inst_id))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = get_value_kind(ir, &k1.types, base);
                    if !base_type.is_storage() {
                        errors.push(format!("i{inst_id}: struct_offset base is not a ptr"))
                    }
                }
                Inst::ArrayOffset { base, element_index, .. } => {
                    let base_type = get_value_kind(ir, &k1.types, base);
                    let index_type = get_value_kind(ir, &k1.types, element_index);
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
                    if !ir.mem.dlist_iter(unit.blocks).any(|b| b.identical(&ir.mem.get(block).data))
                    {
                        errors.push(format!("i{inst_id}: jump to non-existent block"))
                    }
                }
                Inst::JumpIf { cond, .. } => {
                    let cond_type = get_value_kind(ir, &k1.types, cond);
                    if !cond_type.is_value() {
                        errors.push(format!("i{inst_id}: jumpif cond is not a value"))
                    }
                }
                Inst::Unreachable => (),
                Inst::CameFrom { .. } => (),
                Inst::Ret { v, .. } => {
                    let ret_val_type = get_value_kind(ir, &k1.types, v);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("i{inst_id}: ret value is not a value"))
                    }
                }
                Inst::BoolNegate { v } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !inst_type.is_u8() {
                        errors.push(format!("i{inst_id}: bool_negate src is not a bool"))
                    }
                }
                Inst::BitNot { v } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
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
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !inst_type.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u src is not an int"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u to is not int"))
                    }
                }
                Inst::FloatTrunc { v, to } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F64))
                    {
                        errors.push(format!("i{inst_id}: float_trunc src is not f64"))
                    }
                    if to != ScalarType::F32 {
                        errors.push(format!("i{inst_id}: float_trunc to is not f32"))
                    }
                }
                Inst::FloatExt { v, to } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float_ext src is not f32"))
                    }
                    if to != ScalarType::F64 {
                        errors.push(format!("i{inst_id}: float_ext to is not f64"))
                    }
                }
                Inst::Float32ToIntUnsigned { v, to } | Inst::Float32ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float32_to_int src is not f32"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: float32_to_int to is not int"))
                    }
                }
                Inst::Float64ToIntUnsigned { v, to } | Inst::Float64ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
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
                    let inst_type = get_value_kind(ir, &k1.types, v);
                    if !inst_type.is_storage() {
                        errors.push(format!("i{inst_id}: ptr_to_word src is not a ptr"))
                    }
                }
                Inst::WordToPtr { v } => {
                    let inst_type = get_value_kind(ir, &k1.types, v);
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
        let error_string = errors.into_iter().join("\n");
        Err(K1Message {
            span,
            message: format!(
                "IR Unit failed validation\n{}\n{}",
                unit_to_string(k1, unit_id, true),
                error_string
            ),
            level: MessageLevel::Error,
            error_kind: ErrorKind::Internal,
        })
    } else {
        Ok(())
    }
}

mod iropt;
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
            k1.write_qualified_name(w, function.scope, k1.ident_str(function.name), "/", true);
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
        k1.types.display_pt(w, param.pt)?;
        let last = index == p_fn_ty.params.len() as usize - 1;
        if !last {
            w.write_str(", ")?;
        }
    }
    w.write_str("): ")?;
    k1.types.display_pt(w, p_fn_ty.return_type)?;
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
            write!(w, " from {}:{}", &source.filename, line.line_number())?;
        }
    };
    writeln!(w, " (inst count={})", unit.inst_count)?;
    display_blocks(w, k1, unit.blocks, show_source)?;
    Ok(())
}

pub fn display_blocks(
    w: &mut impl Write,
    k1: &TypedProgram,
    blocks: Dlist<Block, ProgramIr>,
    show_source: bool,
) -> std::fmt::Result {
    for (index, (block, _)) in k1.ir.mem.dlist_iter_with_handles(blocks).enumerate() {
        display_block(w, k1, block, index, show_source)?;
    }
    Ok(())
}

pub fn blocks_to_string(
    k1: &TypedProgram,
    blocks: Dlist<Block, ProgramIr>,
    show_source: bool,
) -> String {
    let mut s = String::new();
    display_blocks(&mut s, k1, blocks, show_source).unwrap();
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
    index: usize,
    show_source: bool,
) -> std::fmt::Result {
    let ir = &k1.ir;
    let block = ir.mem.get(block_id).data;
    write!(w, "b{} {}", block_id.raw_index(), block.name)?;
    writeln!(w)?;
    for inst_id in ir.mem.dlist_iter(block.instrs) {
        if show_source {
            let span_id = *ir.sources.get(*inst_id);
            let lines = k1.ast.get_span_content(span_id);
            let the_span = k1.ast.spans.get(span_id);
            let (_, line) = k1.get_span_location(span_id);
            let first_line = lines.lines().next().unwrap_or("");
            let column = the_span.start + 1 - line.start_char;
            write!(w, "|  {first_line:80}| L{:3}:{} |", line.line_number(), column)?;
        }

        write!(w, " i{:3} = ", *inst_id)?;
        display_inst(w, k1, ir, *inst_id)?;
        let comment = ir.comments.get(*inst_id);
        if !comment.is_empty() {
            write!(w, " ; {}", comment)?;
        }
        writeln!(w)?;
    }
    writeln!(w, "END")?;
    Ok(())
}

pub fn inst_to_string(k1: &TypedProgram, inst_id: InstId) -> String {
    let mut s = String::new();
    display_inst(&mut s, k1, &k1.ir, inst_id).unwrap();
    s
}

pub fn display_inst(
    w: &mut impl Write,
    k1: &TypedProgram,
    ir: &ProgramIr,
    inst_id: InstId,
) -> std::fmt::Result {
    match *ir.instrs.get(inst_id) {
        Inst::Data(imm) => {
            write!(w, "imm ")?;
            display_imm(w, imm)?;
        }
        Inst::Alloca { t, vm_layout, returned } => {
            write!(w, "alloca ")?;
            if returned {
                w.write_str("returned ")?;
            }
            k1.types.display_pt(w, t)?;
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
        Inst::Copy { dst, src, t: _, vm_size } => {
            write!(w, "copy {} {}, src {}", vm_size, dst, src)?;
        }
        Inst::StructOffset { struct_t, base, field_index, vm_offset } => {
            write!(w, "struct_offset ")?;
            k1.types.display_pt(w, PhysicalType::agg(struct_t))?;
            write!(w, ".{}, {} ({})", field_index, base, vm_offset)?;
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            write!(w, "array_offset ")?;
            k1.types.display_pt(w, element_t)?;
            write!(w, " {}[{}]", base, element_index)?;
        }
        Inst::Call { call_id: id } => {
            let call = ir.calls.get(id);
            write!(w, "call ")?;
            if let Some(dst) = call.dst {
                w.write_str("into ")?;
                display_value(w, &dst)?;
                w.write_str(" ")?;
            }
            k1.types.display_pt(w, call.ret_type)?;
            match &call.callee {
                IrCallee::Builtin(_, intrinsic_operation) => {
                    write!(w, " builtin {:?}", intrinsic_operation)?;
                }
                IrCallee::Direct(function_id) => {
                    write!(w, " ")?;
                    w.write_str(k1.ident_str(k1.get_function(*function_id).name))?;
                }
                IrCallee::Indirect(_, callee_inst) => {
                    write!(w, " indirect {}", *callee_inst)?;
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
            for (index, arg) in ir.mem.getn(call.args).iter().enumerate() {
                write!(w, "{}", *arg)?;
                let last = index == call.args.len() as usize - 1;
                if !last {
                    w.write_str(", ")?;
                }
            }
            w.write_str(")")?;
        }
        Inst::Jump(block_id) => {
            write!(w, "jmp b{} {}", block_id.raw_index(), k1.ir.mem.get(block_id).data.name)?;
        }
        Inst::JumpIf { cond, cons, alt } => {
            write!(
                w,
                "jmpif {}, b{}, b{}",
                cond,
                k1.ir.mem.get(cons).data.name,
                k1.ir.mem.get(alt).data.name
            )?;
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
        }
        Inst::CameFrom { t, incomings } => {
            write!(w, "comefrom ")?;
            k1.types.display_pt(w, t)?;
            write!(w, " [")?;
            for (i, incoming) in ir.mem.getn(incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "(b{}: {})", k1.ir.mem.get(incoming.from).data.name, incoming.value)?;
            }
            write!(w, "]")?;
        }
        Inst::Ret { v, agg } => {
            write!(w, "ret ")?;
            if agg {
                w.write_str("agg ")?;
            }
            display_inst_kind(w, &k1.types, get_value_kind(ir, &k1.types, v))?;
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
            k1.types.display_pt(w, to)?;
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
    types: &TypePool,
    kind: InstKind,
) -> std::fmt::Result {
    match kind {
        InstKind::Value(t) => types.display_pt(w, t),
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
