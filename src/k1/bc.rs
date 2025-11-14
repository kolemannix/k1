/// Copyright (c) 2025 knix
/// All rights reserved.
///
/// The goal here is a strongly-typed SSA form
/// instruction-based IR with basic blocks, obviously
/// very much like LLVM, as that is our primary target.
/// But I currently think there's going to be a lot of value
/// in having our own. It'll be easier to write an interpreter for
/// and will help make adding other backends far, far easier
use crate::compiler::WordSize;
use crate::kmem::MList;
use crate::parse::NumericWidth;
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::StaticValueId;
use crate::{failf, mformat};
use crate::{
    kmem::{self, MSlice, MStr},
    lex::SpanId,
    nz_u32_id,
    pool::VPool,
    typer::{types::*, *},
};
use ahash::HashMapExt;
use fxhash::FxHashMap;
use itertools::Itertools;
use log::debug;
use std::fmt::Write;

macro_rules! b_ice {
    ($b:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            $b.k1.ice_with_span(s, $b.cur_span)
        }

    }
}

nz_u32_id!(BcCallId);
pub struct ProgramBytecode {
    pub mem: kmem::Mem<ProgramBytecode>,
    pub instrs: VPool<Inst, InstId>,
    pub sources: VPool<SpanId, InstId>,
    pub comments: VPool<BcStr, InstId>,
    /// Compiled bytecode for actual functions
    pub functions: VPool<Option<CompiledUnit>, FunctionId>,
    /// Compiled bytecode for #static exprs and global initializers
    pub exprs: FxHashMap<TypedExprId, CompiledUnit>,
    pub module_config: BcModuleConfig,
    pub calls: VPool<BcCall, BcCallId>,

    // Builder data
    b_blocks: Vec<Block>,
    b_variables: Vec<BuilderVariable>,
    b_loops: FxHashMap<ScopeId, LoopInfo>,
    pub b_units_pending_compile: Vec<FunctionId>,
}
type BcStr = MStr<ProgramBytecode>;

#[derive(Clone, Copy)]
pub enum CompilableUnitId {
    Function(FunctionId),
    Expr(TypedExprId),
}

pub struct BcModuleConfig {
    pub word_size: WordSize,
}

impl ProgramBytecode {
    pub fn make(instr_count_hint: usize, word_size: WordSize) -> Self {
        let function_count_hint = instr_count_hint / 32;
        ProgramBytecode {
            mem: kmem::Mem::make(),
            instrs: VPool::make_with_hint("bytecode_soa_instrs", instr_count_hint),
            sources: VPool::make_with_hint("bytecode_soa_sources", instr_count_hint),
            comments: VPool::make_with_hint("bytecode_soa_comments", instr_count_hint),
            functions: VPool::make_with_hint("bytecode_functions", function_count_hint),
            calls: VPool::make_with_hint("bytecode_calls", instr_count_hint / 2),
            exprs: FxHashMap::new(),
            module_config: BcModuleConfig { word_size },
            b_blocks: vec![],
            b_variables: vec![],
            b_loops: FxHashMap::default(),
            b_units_pending_compile: vec![],
        }
    }

    fn word_sized_int(&self) -> ScalarType {
        ScalarType::U64
    }
}

#[derive(Clone)]
pub struct Block {
    pub name: BcStr,
    pub instrs: Vec<InstId>,
}

#[derive(Clone, Copy)]
pub struct CompiledBlock {
    pub name: BcStr,
    pub instrs: MSlice<InstId, ProgramBytecode>,
}

#[derive(Clone, Copy)]
pub struct CompiledUnit {
    pub unit_id: CompilableUnitId,
    /// If a compiled expression, the returned type.
    pub expr_ret_kind: Option<PhysicalType>,
    // The offset of the first instruction id
    // used by this compiled unit.
    // Subtract this to get sane indices for dense storage
    pub inst_offset: u32,
    // The number of instructions in this unit; used to reserve N 'registers' in our register buffer
    pub inst_count: u32,

    pub blocks: MSlice<CompiledBlock, ProgramBytecode>,
    pub fn_params: MSlice<PhysicalType, ProgramBytecode>,
    pub is_debug: bool,
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
pub type BlockId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BcBuiltin {
    // In LLVM backend, this is becomes a runtime switch
    // In the VM, just a lookup. So we leave it as a builtin
    // rather than generate bytecode for it due to that difference
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

    // Implemented by the compile-time interpreter
    CompilerMessage,
}

// Lambdas have been compiled down to just calls and args by now
#[derive(Clone, Copy)]
pub enum BcCallee {
    Builtin(BcBuiltin),
    Direct(FunctionId),
    Indirect(Value),
}

#[derive(Clone, Copy)]
pub struct CameFromCase {
    pub from: BlockId,
    pub value: Value,
}

#[derive(Clone, Copy)]
pub enum Value {
    Inst(InstId),
    Global { t: PhysicalType, id: TypedGlobalId },
    StaticValue { t: PhysicalType, id: StaticValueId },
    FunctionAddr(FunctionId),
    FnParam { t: PhysicalType, index: u32 },

    // Large 'immediates' just get encoded as their own instruction
    // We have space for u32, so we use it
    Imm32 { t: ScalarType, data: u32 },
    PtrZero,
}

impl Value {
    const UNIT: Value = Value::byte(0);
    #[allow(unused)]
    const FALSE: Value = Value::byte(0);
    #[allow(unused)]
    const TRUE: Value = Value::byte(1);
    const fn byte(u8: u8) -> Value {
        Value::Imm32 { t: ScalarType::U8, data: u8 as u32 }
    }
    const fn imm32(t: ScalarType, u32: u32) -> Value {
        Value::Imm32 { t, data: u32 }
    }
}

#[derive(Clone, Copy)]
pub struct BcCall {
    pub dst: Option<Value>,
    pub ret_inst_kind: InstKind,
    pub callee: BcCallee,
    pub args: MSlice<Value, ProgramBytecode>,
}

/// Many of these instructions contain both a high-level description of a type
/// via a `PhysicalType` or `PhysicalTypeId` as well as a low-level size in bytes
/// that presumes a certain offset. This is because this representation serves dual purposes
/// 1. Efficient interpretation at compile-time, where we get to decide offsets
/// 2. Efficient translation into LLVM-ir, which wants to know about aggregate types in order
///    to optimize them away.
///
/// So we simply offer a mixed-level IR. I may strengthen the assertion in the future by saying
/// that K1 only supports targets whose alignment and sizing rules conform with what the VM does.
/// This would allow for a universal wire-format for k1 data and we could say that "nothing is
/// platform-specific"* in terms of data layout which would be great
///
/// *Function call conventions vary by the major platforms though so that is still something that will of course be platform-dependent.
//task(bc): Get inst to 32 bytes at the most
pub enum Inst {
    Data(DataInst),

    // Memory manipulation
    Alloca {
        t: PhysicalType,
        vm_layout: Layout,
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
        struct_t: PhysicalTypeId,
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
        id: BcCallId,
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
        incomings: MSlice<CameFromCase, ProgramBytecode>,
    },
    Ret(Value),

    // Value Operations
    BoolNegate {
        v: Value,
    },
    BitNot {
        v: Value,
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

pub fn get_value_kind(bc: &ProgramBytecode, types: &TypePool, value: &Value) -> InstKind {
    match value {
        Value::Inst(inst_id) => get_inst_kind(bc, types, *inst_id),
        Value::Global { t: _, id: _ } => InstKind::PTR,
        Value::StaticValue { t, id: _ } => InstKind::Value(*t),
        Value::FunctionAddr(_) => InstKind::PTR,
        Value::FnParam { t, .. } => InstKind::Value(*t),
        Value::Imm32 { t: scalar_type, data: _ } => {
            InstKind::Value(PhysicalType::Scalar(*scalar_type))
        }
        Value::PtrZero => InstKind::PTR,
    }
}

pub fn get_inst_kind(bc: &ProgramBytecode, types: &TypePool, inst_id: InstId) -> InstKind {
    match bc.instrs.get(inst_id) {
        Inst::Data(imm) => match imm {
            DataInst::I64(_) => InstKind::scalar(ScalarType::I64),
            DataInst::U64(_) => InstKind::scalar(ScalarType::U64),
            DataInst::Float(TypedFloatValue::F32(_)) => InstKind::scalar(ScalarType::F32),
            DataInst::Float(TypedFloatValue::F64(_)) => InstKind::scalar(ScalarType::F64),
        },
        Inst::Alloca { .. } => InstKind::PTR,
        Inst::Store { .. } => InstKind::Void,
        Inst::Load { t, .. } => InstKind::scalar(*t),
        Inst::Copy { .. } => InstKind::Void,
        Inst::StructOffset { .. } => InstKind::PTR,
        Inst::ArrayOffset { .. } => InstKind::PTR,
        Inst::Call { id } => bc.calls.get(*id).ret_inst_kind,
        Inst::Jump(_) => InstKind::Terminator,
        Inst::JumpIf { .. } => InstKind::Terminator,
        Inst::Unreachable => InstKind::Terminator,
        Inst::CameFrom { t, .. } => InstKind::Value(*t),
        Inst::Ret(_) => InstKind::Terminator,
        Inst::BoolNegate { .. } => InstKind::BOOL,
        Inst::BitNot { v } => get_value_kind(bc, types, v),
        Inst::IntTrunc { to, .. } => InstKind::scalar(*to),
        Inst::IntExtU { to, .. } => InstKind::scalar(*to),
        Inst::IntExtS { to, .. } => InstKind::scalar(*to),
        Inst::FloatTrunc { to, .. } => InstKind::scalar(*to),
        Inst::FloatExt { to, .. } => InstKind::scalar(*to),
        Inst::Float32ToIntUnsigned { to, .. } => InstKind::scalar(*to),
        Inst::Float32ToIntSigned { to, .. } => InstKind::scalar(*to),
        Inst::Float64ToIntUnsigned { to, .. } => InstKind::scalar(*to),
        Inst::Float64ToIntSigned { to, .. } => InstKind::scalar(*to),
        Inst::IntToFloatUnsigned { to, .. } => InstKind::scalar(*to),
        Inst::IntToFloatSigned { to, .. } => InstKind::scalar(*to),
        Inst::PtrToWord { .. } => InstKind::scalar(bc.word_sized_int()),
        Inst::WordToPtr { .. } => InstKind::PTR,
        Inst::IntAdd { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntSub { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntMul { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntDivUnsigned { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntDivSigned { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntRemUnsigned { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntRemSigned { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::IntCmp { .. } => InstKind::BOOL,
        Inst::FloatAdd { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::FloatSub { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::FloatMul { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::FloatDiv { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::FloatRem { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::FloatCmp { .. } => InstKind::BOOL,
        Inst::BitAnd { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::BitOr { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::BitXor { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::BitShiftLeft { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::BitUnsignedShiftRight { lhs, .. } => get_value_kind(bc, types, lhs),
        Inst::BitSignedShiftRight { lhs, .. } => get_value_kind(bc, types, lhs),
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
    pub const UNIT: InstKind = Self::scalar(ScalarType::U8);
    pub const BOOL: InstKind = Self::scalar(ScalarType::U8);
    pub const PTR: InstKind = Self::scalar(ScalarType::Pointer);
    pub const U64: InstKind = Self::scalar(ScalarType::U64);

    pub const fn scalar(st: ScalarType) -> InstKind {
        InstKind::Value(PhysicalType::Scalar(st))
    }

    fn is_ptr(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Scalar(ScalarType::Pointer)))
    }
    fn is_int(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Scalar(st)) if st.is_int())
    }
    fn is_byte(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Scalar(ScalarType::U8)))
    }
    fn is_aggregate(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Agg(_)))
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
            _ => Err(format!("Expected value, got {}", self.kind_str())),
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

    pub fn kind_str(&self) -> &'static str {
        match self {
            InstKind::Value(PhysicalType::Scalar(ScalarType::U8)) => "u8",
            InstKind::Value(PhysicalType::Scalar(ScalarType::U16)) => "u16",
            InstKind::Value(PhysicalType::Scalar(ScalarType::U32)) => "u32",
            InstKind::Value(PhysicalType::Scalar(ScalarType::U64)) => "u64",
            InstKind::Value(PhysicalType::Scalar(ScalarType::I8)) => "i8",
            InstKind::Value(PhysicalType::Scalar(ScalarType::I16)) => "i16",
            InstKind::Value(PhysicalType::Scalar(ScalarType::I32)) => "i32",
            InstKind::Value(PhysicalType::Scalar(ScalarType::I64)) => "i64",
            InstKind::Value(PhysicalType::Scalar(ScalarType::F32)) => "f32",
            InstKind::Value(PhysicalType::Scalar(ScalarType::F64)) => "f64",
            InstKind::Value(PhysicalType::Scalar(ScalarType::Pointer)) => "ptr",
            InstKind::Value(PhysicalType::Agg(_)) => "agg",
            InstKind::Void => "void",
            InstKind::Terminator => "terminator",
        }
    }
}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> TyperResult<()> {
    let start = k1.timing.clock.raw();

    let mut b = Builder::new(k1);

    debug!("Compiling function {}", b.k1.function_id_to_string(function_id, false));
    let name = b.k1.bytecode.mem.push_str("entry");
    b.push_block(name);
    let f = b.k1.get_function(function_id);
    let is_debug = f.compiler_debug;
    let fn_span = b.k1.ast.get_span_for_id(f.parsed_id);
    b.cur_span = fn_span;

    let Some(body_block) = &f.body_block else {
        return failf!(fn_span, "Function has no body to compile");
    };
    let body_block = *body_block;

    // Set up parameters
    let param_variables = f.param_variables;
    let mut params = b.k1.bytecode.mem.new_list(param_variables.len());
    for (index, param_variable_id) in b.k1.mem.getn(param_variables).iter().enumerate() {
        let v = b.k1.variables.get(*param_variable_id);
        let t = b.get_physical_type(v.type_id);
        b.k1.bytecode.b_variables.push(BuilderVariable {
            id: *param_variable_id,
            value: Value::FnParam { t, index: index as u32 },
            indirect: false,
        });
        params.push(t);
    }
    let params_handle = b.k1.bytecode.mem.vec_to_mslice(&params);

    compile_block_stmts(&mut b, None, body_block)?;

    let unit_id = CompilableUnitId::Function(function_id);
    let unit = finalize_unit(&mut b, unit_id, params_handle, None, is_debug);

    *b.k1.bytecode.functions.get_mut(function_id) = Some(unit);

    if is_debug {
        let s = compiled_unit_to_string(b.k1, unit_id, true);
        eprintln!("{s}");
    }

    validate_unit(k1, unit_id)?;

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_bytecode_nanos += elapsed;
    Ok(())
}

pub fn compile_top_level_expr(
    k1: &mut TypedProgram,
    expr: TypedExprId,
    input_parameters: &[(VariableId, StaticValueId)],
    is_debug: bool,
) -> TyperResult<()> {
    let start = k1.timing.clock.raw();

    let mut b = Builder::new(k1);

    for (variable_id, static_value_id) in input_parameters {
        let variable = b.k1.variables.get(*variable_id);
        let pt = b.get_physical_type(variable.type_id);
        b.k1.bytecode.b_variables.push(BuilderVariable {
            id: *variable_id,
            value: Value::StaticValue { t: pt, id: *static_value_id },
            indirect: false,
        })
    }

    debug!("Compiling expr {}", b.k1.expr_to_string(expr));
    let name = b.k1.bytecode.mem.push_str("expr_");
    b.push_block(name);

    let _result = compile_expr(&mut b, None, expr)?;
    let return_pt = match b.type_to_inst_kind(b.k1.exprs.get_type(expr)) {
        InstKind::Value(pt) => Some(pt),
        InstKind::Void => None,
        InstKind::Terminator => None,
    };
    let compiled_expr =
        finalize_unit(&mut b, CompilableUnitId::Expr(expr), MSlice::empty(), return_pt, is_debug);

    b.k1.bytecode.exprs.insert(expr, compiled_expr);

    validate_unit(k1, CompilableUnitId::Expr(expr))?;

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_bytecode_nanos += elapsed;
    Ok(())
}

fn finalize_unit(
    b: &mut Builder,
    unit_id: CompilableUnitId,
    fn_params: MSlice<PhysicalType, ProgramBytecode>,
    expr_ret_kind: Option<PhysicalType>,
    is_debug: bool,
) -> CompiledUnit {
    let compiled_blocks = b.bake_blocks();
    let inst_count = b.k1.bytecode.instrs.len() as u32 - b.inst_offset + 1;
    let unit = CompiledUnit {
        unit_id,
        expr_ret_kind,
        inst_offset: b.inst_offset,
        inst_count,
        blocks: compiled_blocks,
        fn_params,
        is_debug,
    };
    b.reset_compilation_unit();
    unit
}

struct BuilderVariable {
    id: VariableId,
    value: Value,
    indirect: bool,
}

#[derive(Clone)]
struct LoopInfo {
    break_value: Option<InstId>,
    end_block: BlockId,
}
pub struct Builder<'k1> {
    // Dependencies
    k1: &'k1 mut TypedProgram,

    inst_offset: u32,
    block_count: u32,
    last_alloca_index: Option<u32>,
    cur_block: BlockId,
    cur_span: SpanId,
}

impl<'k1> Builder<'k1> {
    fn new(k1: &'k1 mut TypedProgram) -> Self {
        let inst_offset = k1.bytecode.instrs.len() as u32 + 1;
        Self {
            inst_offset,
            k1,
            block_count: 0,
            last_alloca_index: None,
            cur_block: 0,
            cur_span: SpanId::NONE,
        }
    }

    fn reset_compilation_unit(&mut self) {
        for b in &mut self.k1.bytecode.b_blocks {
            b.instrs.clear();
        }
        self.block_count = 0;
        self.k1.bytecode.b_variables.clear();
        self.k1.bytecode.b_loops.clear();
    }

    fn bake_blocks(&mut self) -> MSlice<CompiledBlock, ProgramBytecode> {
        let mut blocks = self.k1.bytecode.mem.new_list(self.k1.bytecode.b_blocks.len() as u32);
        for b in Builder::builder_blocks_iter(self.block_count, &self.k1.bytecode.b_blocks) {
            let instrs = self.k1.bytecode.mem.pushn(&b.instrs);
            let b = CompiledBlock { name: b.name, instrs };
            blocks.push(b)
        }
        self.k1.bytecode.mem.vec_to_mslice(&blocks)
    }

    fn builder_blocks_iter(block_count: u32, b_blocks: &[Block]) -> impl Iterator<Item = &Block> {
        b_blocks[0..block_count as usize].iter()
    }

    fn make_str(&mut self, s: impl AsRef<str>) -> BcStr {
        self.k1.bytecode.mem.push_str(s.as_ref())
    }

    fn make_inst(&mut self, inst: Inst, comment: BcStr) -> InstId {
        let id = self.k1.bytecode.instrs.add(inst);
        let ids = self.k1.bytecode.sources.add(self.cur_span);
        let idc = self.k1.bytecode.comments.add(comment);
        debug_assert!(id == ids && id == idc);
        id
    }

    fn push_inst_to(&mut self, block: BlockId, inst: Inst, comment: BcStr) -> InstId {
        let id = self.make_inst(inst, comment);

        self.k1.bytecode.b_blocks[block as usize].instrs.push(id);
        id
    }

    fn push_alloca(&mut self, pt: PhysicalType, comment: &str) -> InstId {
        let layout = self.k1.types.get_pt_layout(&pt);
        let index = match self.last_alloca_index {
            None => 0,
            Some(i) => i as usize + 1,
        };
        let comment = self.make_str(comment);
        let inst_id = self.make_inst(Inst::Alloca { t: pt, vm_layout: layout }, comment);
        self.k1.bytecode.b_blocks[0].instrs.insert(index, inst_id);
        inst_id
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.k1.bytecode, &self.k1.types, inst)
    }

    pub fn get_value_kind(&self, value: &Value) -> InstKind {
        get_value_kind(&self.k1.bytecode, &self.k1.types, value)
    }

    fn push_inst(&mut self, inst: Inst, comment: &str) -> InstId {
        let comment = self.make_str(comment);
        self.push_inst_to(self.cur_block, inst, comment)
    }

    fn push_inst_anon(&mut self, inst: Inst) -> InstId {
        self.push_inst(inst, "")
    }

    fn push_struct_offset(
        &mut self,
        agg_id: PhysicalTypeId,
        base: Value,
        field_index: u32,
        comment: &str,
    ) -> Value {
        if field_index == 0 {
            return base;
        }
        let Some(offset) = self.k1.types.get_struct_field_offset(agg_id, field_index) else {
            b_ice!(self, "Failed getting offset for field")
        };
        self.push_inst(
            Inst::StructOffset { struct_t: agg_id, base, field_index, vm_offset: offset },
            comment,
        )
        .as_value()
    }

    fn push_jump(&mut self, block_id: BlockId, comment: &str) -> InstId {
        self.push_inst(Inst::Jump(block_id), comment)
    }

    fn push_jump_if(&mut self, cond: Value, cons: BlockId, alt: BlockId, comment: &str) -> InstId {
        if let Value::Imm32 { t: ScalarType::U8, data: b32 } = cond {
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

    fn push_copy(&mut self, dst: Value, src: Value, pt: PhysicalType, comment: &str) -> InstId {
        let layout = self.k1.types.get_pt_layout(&pt);
        self.push_inst(Inst::Copy { dst, src, t: pt, vm_size: layout.size }, comment)
    }

    fn push_load(&mut self, st: ScalarType, src: Value, comment: &str) -> InstId {
        self.push_inst(Inst::Load { t: st, src }, comment)
    }

    fn push_store(&mut self, dst: Value, value: Value, comment: &str) -> InstId {
        let t = self.get_value_kind(&value).expect_value().unwrap().expect_scalar();
        self.push_inst(Inst::Store { dst, value, t }, comment)
    }

    fn make_int_value(&mut self, int_value: &TypedIntValue, comment: &str) -> Value {
        match int_value {
            TypedIntValue::U8(i) => Value::Imm32 { t: ScalarType::U8, data: *i as u32 },
            TypedIntValue::U16(i) => Value::Imm32 { t: ScalarType::U16, data: *i as u32 },
            TypedIntValue::U32(i) | TypedIntValue::UWord32(i) => {
                Value::Imm32 { t: ScalarType::U32, data: *i }
            }
            TypedIntValue::U64(i) | TypedIntValue::UWord64(i) => {
                if *i <= u32::MAX as u64 {
                    Value::imm32(int_value.get_integer_type().get_scalar_type(), *i as u32)
                } else {
                    let inst = self.push_inst(Inst::Data(DataInst::U64(*i)), comment);
                    inst.as_value()
                }
            }
            TypedIntValue::I8(i) => Value::byte(*i as u8),
            TypedIntValue::I16(i) => Value::Imm32 { t: ScalarType::I16, data: *i as u32 },
            TypedIntValue::I32(i) | TypedIntValue::IWord32(i) => {
                Value::imm32(int_value.get_integer_type().get_scalar_type(), *i as u32)
            }
            TypedIntValue::I64(i) | TypedIntValue::IWord64(i) => {
                if *i >= i32::MIN as i64 && *i <= i32::MAX as i64 {
                    Value::imm32(int_value.get_integer_type().get_scalar_type(), *i as i32 as u32)
                } else {
                    let inst = self.push_inst(Inst::Data(DataInst::I64(*i)), comment);
                    inst.as_value()
                }
            }
        }
    }

    fn push_block(&mut self, name: BcStr) -> BlockId {
        let id = self.block_count;
        // Recycle builder blocks
        match self.k1.bytecode.b_blocks.get_mut(self.block_count as usize) {
            Some(recycled) => {
                self.block_count += 1;
                recycled.name = name;
            }
            None => {
                self.block_count += 1;
                self.k1.bytecode.b_blocks.push(Block { name, instrs: vec![] });
            }
        }

        id as BlockId
    }

    #[track_caller]
    fn goto_block(&mut self, block_id: BlockId) {
        match self.k1.bytecode.b_blocks.get(block_id as usize) {
            None => panic!("goto_block on non-existent block: {}", block_id),
            Some(_) => self.cur_block = block_id,
        }
    }

    fn get_variable(&self, variable_id: VariableId) -> Option<&BuilderVariable> {
        self.k1.bytecode.b_variables.iter().find(|bv| bv.id == variable_id)
    }

    fn get_physical_type(&self, type_id: TypeId) -> PhysicalType {
        self.k1.types.get_physical_type(type_id).unwrap_or_else(|| {
            b_ice!(
                self,
                "Not a physical type: {}",
                self.k1.type_id_to_string_ext(self.k1.types.get_chased_id(type_id), true)
            )
        })
    }

    fn type_to_inst_kind(&self, type_id: TypeId) -> InstKind {
        if type_id == NEVER_TYPE_ID {
            InstKind::Terminator
        } else {
            let t = self.get_physical_type(type_id);
            InstKind::Value(t)
        }
    }
}

fn store_simple_if_dst(b: &mut Builder, dst: Option<Value>, value: Value) -> Value {
    match dst {
        None => value,
        Some(dst) => b.push_store(dst, value, "store to dst").as_value(),
    }
}

fn store_rich_if_dst(
    b: &mut Builder,
    dst: Option<Value>,
    pt: PhysicalType,
    value: Value,
    comment: &str,
) -> Value {
    match dst {
        None => value,
        Some(dst) => store_value(b, pt, dst, value, comment).as_value(),
    }
}

fn compile_block_stmts(
    b: &mut Builder,
    dst: Option<Value>,
    body: TypedExprId,
) -> TyperResult<Option<Value>> {
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

fn compile_stmt(b: &mut Builder, dst: Option<Value>, stmt: TypedStmtId) -> TyperResult<Value> {
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
            // Handle the case where the rhs crashes first, ugh
            if let_stmt.variable_type == NEVER_TYPE_ID {
                // If there's an initializer, run it so we crash
                let init = let_stmt.initializer.unwrap();
                let v = compile_expr(b, None, init)?;
                return Ok(v);
            }

            let var_pt_id = b.get_physical_type(let_stmt.variable_type);

            //task(bc): If variable is never re-assigned, and does not require memory
            //          we could avoid the alloca and use an immediate. Its unclear to me
            //          if this is a good idea
            let variable_alloca = b.push_alloca(var_pt_id, "variable alloca");

            // value_ptr means a pointer matching the type of the rhs
            // For a referencing let, the original alloca a ptr
            // So we need one for the inner type as well
            let value_ptr = if let_stmt.is_referencing {
                let Type::Reference(reference_type) = b.k1.types.get(let_stmt.variable_type) else {
                    panic!("Expected reference for referencing let");
                };
                let reference_inner_type_pt_id = b.get_physical_type(reference_type.inner_type);
                let value_alloca =
                    b.push_alloca(reference_inner_type_pt_id, "referencing inner alloca");
                b.push_store(
                    variable_alloca.as_value(),
                    value_alloca.as_value(),
                    "referencing initializer store",
                );
                value_alloca
            } else {
                variable_alloca
            };

            // If there's an initializer, store it in value_ptr
            if let Some(init) = let_stmt.initializer {
                compile_expr(b, Some(value_ptr.as_value()), init)?;
            }
            b.k1.bytecode.b_variables.push(BuilderVariable {
                id: let_stmt.variable_id,
                value: variable_alloca.as_value(),
                indirect: true,
            });
            Ok(Value::UNIT)
        }
        TypedStmt::Assignment(ass) => {
            let ass = *ass;
            match ass.kind {
                AssignmentKind::Set => {
                    let TypedExpr::Variable(v) = b.k1.exprs.get(ass.destination) else {
                        b.k1.ice_with_span("Invalid value assignment lhs", ass.span)
                    };
                    let builder_variable = b.get_variable(v.variable_id).expect("Missing variable");
                    if !builder_variable.indirect {
                        b.k1.ice_with_span(
                            "Expect an indirect variable for value assignment",
                            b.k1.exprs.get_span(ass.destination),
                        )
                    };
                    let variable_value = builder_variable.value;
                    let _rhs_stored = compile_expr(b, Some(variable_value), ass.value)?;
                    Ok(Value::UNIT)
                }
                AssignmentKind::Store => {
                    let lhs = compile_expr(b, None, ass.destination)?;
                    let _rhs_stored = compile_expr(b, Some(lhs), ass.value)?;
                    Ok(Value::UNIT)
                }
            }
        }
        TypedStmt::Require(req) => {
            //task(bc): matching condition clone
            let req = req.clone();
            let continue_name = mformat!(b.k1.bytecode.mem, "req_cont_{}", stmt.as_u32());
            let require_continue_block = b.push_block(continue_name);
            let else_name = mformat!(b.k1.bytecode.mem, "req_else_{}", stmt.as_u32());
            let require_else_block = b.push_block(else_name);

            compile_matching_condition(
                b,
                &req.condition,
                require_continue_block,
                require_else_block,
            )?;

            b.goto_block(require_else_block);
            compile_expr(b, None, req.else_body)?;

            b.goto_block(require_continue_block);

            Ok(Value::UNIT)
        }
        TypedStmt::Defer(_) => {
            // These defers are just vestiges of the source; nothing to emit
            Ok(Value::UNIT)
        }
    }
}

fn compile_expr(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<Value>,
    expr: TypedExprId,
) -> TyperResult<Value> {
    let prev_span = b.cur_span;
    let expr_span = b.k1.exprs.get_span(expr);
    b.cur_span = expr_span;
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);
    let e = b.k1.exprs.get(expr).clone();
    let expr_type = b.k1.exprs.get_type(expr);
    debug!("compiling {} {}", e.kind_str(), b.k1.expr_to_string(expr));
    match e {
        TypedExpr::Struct(struct_literal) => {
            let struct_type_id = expr_type;
            let struct_pt = b.get_physical_type(struct_type_id);
            let struct_agg_id = struct_pt.expect_agg();
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.push_alloca(struct_pt, "struct literal").as_value(),
            };
            for (field_index, field) in b.k1.mem.getn(struct_literal.fields).iter().enumerate() {
                debug_assert!(b.k1.types.get(struct_type_id).as_struct().is_some());
                let struct_offset = b.push_struct_offset(
                    struct_agg_id,
                    struct_base,
                    field_index as u32,
                    "struct lit field ptr",
                );
                compile_expr(b, Some(struct_offset), field.expr)?;
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
            let (element_pt, _len) =
                b.k1.types.phys_types.get(array_agg_id).agg_type.expect_array();
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
            let variable = b.k1.variables.get(variable_expr.variable_id);
            match variable.global_id {
                Some(global_id) => {
                    // Globals are pretty complex. We always generate an instruction
                    // representing the **address** of the global, because they are always
                    // addresses to static memory. For reference types, that address _is_
                    // the value of the expression referring to the global, but for
                    // non-reference types, we must 'load' the value from the address, since
                    // the address is just an implementation detail. For aggregate types,
                    // this load is a no-op, since we represent them as their locations anyway

                    // It matters whether this is a reference in K1, for reasons outlined above.
                    // This info gets erased when compiling types (this would become Pointer which
                    // is indistinguishable from a global of type Pointer)
                    let is_reference = b.k1.types.get(variable.type_id).as_reference().is_some();

                    // By 'value_type' I mean the shape of the actual memory, so
                    // the inner type if its a reference, and just the type if its not
                    let value_type = match b.k1.types.get(variable.type_id).as_reference() {
                        None => variable.type_id,
                        Some(r) => r.inner_type,
                    };
                    let value_pt = b.get_physical_type(value_type);
                    let address = Value::Global { t: value_pt, id: global_id };
                    match value_pt {
                        PhysicalType::Scalar(_) => {
                            // We have a scalar type, but do we have a pointer to it or just
                            // a value?
                            if is_reference {
                                // If reference, the address of the global is what we're after
                                let stored = store_simple_if_dst(b, dst, address);
                                Ok(stored)
                            } else {
                                // The value of the global is what we're after
                                let stored = load_or_copy(
                                    b,
                                    value_pt,
                                    dst,
                                    address,
                                    false,
                                    "load of scalar global variable",
                                );
                                Ok(stored)
                            }
                        }
                        PhysicalType::Agg(_) => {
                            if is_reference {
                                let stored = store_simple_if_dst(b, dst, address);
                                Ok(stored)
                            } else {
                                // The source code is talking about an aggregate _value_
                                // So if we have a dst, then it is of the aggregate's layout, not a Ptr-size!
                                // So we have to copy
                                let stored = store_rich_if_dst(
                                    b,
                                    dst,
                                    value_pt,
                                    address,
                                    "load of non-referene global agg",
                                );
                                Ok(stored)
                            }
                        }
                    }
                }
                None => {
                    let Some(var) = b.get_variable(variable_expr.variable_id) else {
                        eprintln!(
                            "Variables are: {}",
                            b.k1.bytecode
                                .b_variables
                                .iter()
                                .map(|bv| format!("{} {}", bv.id, bv.value))
                                .join("\n")
                        );
                        b.k1.ice_with_span("Missing variable", expr_span)
                    };
                    let var_type_pt_id = b.get_physical_type(expr_type);
                    let var_value = var.value;
                    if var.indirect {
                        let loaded = load_or_copy(
                            b,
                            var_type_pt_id,
                            dst,
                            var_value,
                            false,
                            "load indirect variable",
                        );
                        Ok(loaded)
                    } else {
                        let stored =
                            store_rich_if_dst(b, dst, var_type_pt_id, var_value, "direct variable");
                        Ok(stored)
                    }
                }
            }
        }
        TypedExpr::Deref(deref) => {
            let src = compile_expr(b, None, deref.target)?;
            let target_pt = b.get_physical_type(expr_type);
            let loaded = load_or_copy(b, target_pt, dst, src, true, "lang deref");
            Ok(loaded)
        }
        TypedExpr::Block(_) => {
            let Some(last) = compile_block_stmts(b, dst, expr)? else {
                return failf!(b.cur_span, "Block has no value");
            };
            Ok(last)
        }
        TypedExpr::Call { call_id, return_type, .. } => {
            // task(bc): deal with clone() of Call and TypedExpr
            let call = b.k1.calls.get(call_id).clone();
            let mut args = b.k1.bytecode.mem.new_list(call.args.len() as u32 + 1);
            let builtin = if let Some(function_id) = call.callee.maybe_function_id() {
                if let Some(intrinsic) = b.k1.get_function(function_id).intrinsic_type {
                    match intrinsic {
                        IntrinsicOperation::SizeOf => unreachable!(),
                        IntrinsicOperation::SizeOfStride => unreachable!(),
                        IntrinsicOperation::AlignOf => unreachable!(),
                        IntrinsicOperation::CompilerSourceLocation => unreachable!(),
                        IntrinsicOperation::GetStaticValue => unreachable!(),
                        IntrinsicOperation::StaticTypeToValue => unreachable!(),
                        IntrinsicOperation::TypeId => unreachable!(),

                        IntrinsicOperation::BakeStaticValue => {
                            return {
                                // intern fn bakeStaticValue[T](value: T): u64
                                let type_id = b.k1.named_types.get_nth(call.type_args, 0).type_id;
                                let _physical_type = b.get_physical_type(type_id);

                                let value = compile_expr(b, None, call.args[0])?;
                                let bake =
                                    b.push_inst_anon(Inst::BakeStaticValue { type_id, value });
                                let stored = store_simple_if_dst(b, dst, bake.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::CompilerMessage => Some(BcBuiltin::CompilerMessage),
                        IntrinsicOperation::Zeroed => {
                            return {
                                let type_id = b.k1.named_types.get_nth(call.type_args, 0).type_id;
                                match b.get_physical_type(type_id) {
                                    pt @ PhysicalType::Agg(agg_id) => {
                                        let pt_layout = b.k1.types.phys_types.get(agg_id).layout;
                                        let dst = match dst {
                                            None => b.push_alloca(pt, "zeroed no dst").as_value(),
                                            Some(dst) => dst,
                                        };
                                        let zero_u8 = Value::byte(0);
                                        // intern fn set(dst: Pointer, value: u8, count: uword): unit
                                        let count = Value::Imm32 {
                                            t: ScalarType::U8,
                                            data: pt_layout.size,
                                        };
                                        let memset_args =
                                            b.k1.bytecode.mem.pushn(&[dst, zero_u8, count]);
                                        let memset_call = BcCall {
                                            dst: None,
                                            ret_inst_kind: InstKind::UNIT,
                                            callee: BcCallee::Builtin(BcBuiltin::MemSet),
                                            args: memset_args,
                                        };
                                        let call_id = b.k1.bytecode.calls.add(memset_call);
                                        b.push_inst(Inst::Call { id: call_id }, "zeroed memset");
                                        Ok(dst)
                                    }
                                    PhysicalType::Scalar(st) => {
                                        let zero_value = zero(st);
                                        let stored = store_simple_if_dst(b, dst, zero_value);
                                        Ok(stored)
                                    }
                                }
                            };
                        }

                        IntrinsicOperation::TypeName => Some(BcBuiltin::TypeName),
                        IntrinsicOperation::TypeSchema => Some(BcBuiltin::TypeSchema),

                        IntrinsicOperation::BoolNegate => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst_anon(Inst::BoolNegate { v: base });
                                let stored = store_simple_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::BitNot => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst_anon(Inst::BitNot { v: base });
                                let stored = store_simple_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::ArithBinop(op) => {
                            return compile_arith_binop(b, op, &call, dst);
                        }
                        IntrinsicOperation::BitwiseBinop(op) => {
                            return {
                                let lhs = compile_expr(b, None, call.args[0])?;
                                let rhs = compile_expr(b, None, call.args[1])?;
                                let lhs_pt = b.get_value_kind(&lhs).expect_value().unwrap();
                                let width = b.k1.types.get_pt_layout(&lhs_pt).size_bits() as u8;
                                let inst = match op {
                                    IntrinsicBitwiseBinopKind::And => {
                                        Inst::BitAnd { lhs, rhs, width }
                                    }
                                    IntrinsicBitwiseBinopKind::Or => {
                                        Inst::BitOr { lhs, rhs, width }
                                    }
                                    IntrinsicBitwiseBinopKind::Xor => {
                                        Inst::BitXor { lhs, rhs, width }
                                    }
                                    IntrinsicBitwiseBinopKind::ShiftLeft => {
                                        Inst::BitShiftLeft { lhs, rhs, width }
                                    }
                                    IntrinsicBitwiseBinopKind::UnsignedShiftRight => {
                                        Inst::BitUnsignedShiftRight { lhs, rhs, width }
                                    }
                                    IntrinsicBitwiseBinopKind::SignedShiftRight => {
                                        Inst::BitSignedShiftRight { lhs, rhs, width }
                                    }
                                };
                                let res = b.push_inst_anon(inst);
                                let stored = store_simple_if_dst(b, dst, res.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::PointerIndex => {
                            return {
                                // intern fn refAtIndex[T](self: Pointer, index: uword): T*
                                let elem_type_id =
                                    b.k1.named_types.get_nth(call.type_args, 0).type_id;
                                let elem_pt = b.get_physical_type(elem_type_id);
                                let base = compile_expr(b, None, call.args[0])?;
                                let element_index = compile_expr(b, None, call.args[1])?;
                                let offset = b.push_inst(
                                    Inst::ArrayOffset { element_t: elem_pt, base, element_index },
                                    "refAtIndex offest",
                                );
                                let stored = store_simple_if_dst(b, dst, offset.as_value());
                                Ok(stored)
                            };
                        }

                        IntrinsicOperation::Allocate => Some(BcBuiltin::Allocate),
                        IntrinsicOperation::AllocateZeroed => Some(BcBuiltin::AllocateZeroed),
                        IntrinsicOperation::Reallocate => Some(BcBuiltin::Reallocate),
                        IntrinsicOperation::Free => Some(BcBuiltin::Free),
                        IntrinsicOperation::MemCopy => Some(BcBuiltin::MemCopy),
                        IntrinsicOperation::MemSet => Some(BcBuiltin::MemSet),
                        IntrinsicOperation::MemEquals => Some(BcBuiltin::MemEquals),
                        IntrinsicOperation::Exit => Some(BcBuiltin::Exit),
                    }
                } else {
                    None
                }
            } else {
                None
            };
            let callee = if let Some(builtin) = builtin {
                BcCallee::Builtin(builtin)
            } else {
                match &call.callee {
                    Callee::StaticFunction(function_id) => BcCallee::Direct(*function_id),
                    Callee::StaticLambda { function_id, lambda_value_expr, .. } => {
                        let lambda_env = compile_expr(b, None, *lambda_value_expr)?;
                        args.push(lambda_env);
                        BcCallee::Direct(*function_id)
                    }
                    Callee::Abstract { .. } => return failf!(b.cur_span, "bc abstract call"),
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

                        args.push(env);
                        BcCallee::Indirect(fn_ptr)
                    }
                    Callee::DynamicFunction { function_pointer_expr } => {
                        let callee_inst = compile_expr(b, None, *function_pointer_expr)?;
                        BcCallee::Indirect(callee_inst)
                    }
                    Callee::DynamicAbstract { .. } => {
                        return failf!(b.cur_span, "bc abstract call");
                    }
                }
            };
            if let BcCallee::Direct(function_id) = &callee {
                match b.k1.bytecode.functions.get(*function_id) {
                    None => {
                        if !b.k1.bytecode.b_units_pending_compile.contains(function_id) {
                            b.k1.bytecode.b_units_pending_compile.push(*function_id);
                        }
                    }
                    Some(unit) => {
                        // TODO: Function inlining!
                        let mut total = 0;
                        for block in b.k1.bytecode.mem.getn(unit.blocks) {
                            total += block.instrs.len()
                        }
                        if total < 50 {
                            //eprintln!("Would inline call")
                        }
                    }
                }
            }
            let return_inst_kind: InstKind = b.type_to_inst_kind(return_type);
            for arg in &call.args {
                args.push(compile_expr(b, None, *arg)?);
            }
            let call_dst = match dst {
                None => match return_inst_kind {
                    InstKind::Value(PhysicalType::Scalar(_)) => None,
                    InstKind::Value(pt @ PhysicalType::Agg(_)) => {
                        Some(b.push_alloca(pt, "call return agg storage").as_value())
                    }
                    InstKind::Void => None,
                    InstKind::Terminator => None,
                },
                Some(dst) => Some(dst),
            };
            let args_handle = b.k1.bytecode.mem.vec_to_mslice(&args);
            let call_id = b.k1.bytecode.calls.add(BcCall {
                dst: call_dst,
                ret_inst_kind: return_inst_kind,
                callee,
                args: args_handle,
            });
            let call_inst = Inst::Call { id: call_id };
            let call_inst_id = b.push_inst_anon(call_inst);
            match call_dst {
                Some(dst) => Ok(dst),
                None => Ok(call_inst_id.as_value()),
            }
        }
        TypedExpr::Match(match_expr) => {
            let match_result_type = expr_type;
            for stmt in b.k1.mem.getn(match_expr.initial_let_statements) {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.k1.bytecode.mem.new_list(match_expr.arms.len());
            for (arm_index, _arm) in b.k1.mem.getn(match_expr.arms).iter().enumerate() {
                let name = mformat!(b.k1.bytecode.mem, "arm_{}_cond__{}", arm_index, expr.as_u32());
                let name_cons =
                    mformat!(b.k1.bytecode.mem, "arm_{}_cons__{}", arm_index, expr.as_u32());
                let arm_block = b.push_block(name);
                let arm_consequent_block = b.push_block(name_cons);
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_jump(first_arm_block, "enter match");

            let fail_name = mformat!(b.k1.bytecode.mem, "match_fail__{}", expr.as_u32());
            let fail_block = b.push_block(fail_name);
            b.goto_block(fail_block);
            b.push_inst_anon(Inst::Unreachable);

            let end_name = mformat!(b.k1.bytecode.mem, "match_end__{}", expr.as_u32());
            let match_end_block = b.push_block(end_name);
            b.goto_block(match_end_block);
            let result_inst_kind = b.type_to_inst_kind(match_result_type);
            let result_came_from = match result_inst_kind {
                InstKind::Value(_) => {
                    let pt = b.get_physical_type(match_result_type);
                    Some(b.push_inst(
                        Inst::CameFrom { t: pt, incomings: MSlice::empty() },
                        "match phi",
                    ))
                }
                InstKind::Void => {
                    eprintln!("expr {}", b.k1.expr_to_string(expr));
                    return failf!(b.cur_span, "come from void");
                }
                InstKind::Terminator => None,
            };

            let mut incomings: MList<CameFromCase, _> =
                b.k1.bytecode.mem.new_list(match_expr.arms.len());
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
                compile_matching_condition(b, &arm.condition, *arm_cons_block, next_arm_or_fail)?;

                b.goto_block(*arm_cons_block);
                let result = compile_expr(b, None, arm.consequent_expr)?;
                if !b.get_value_kind(&result).is_terminator() {
                    let current_block = b.cur_block;
                    incomings.push(CameFromCase { from: current_block, value: result });
                    b.push_jump(match_end_block, "");
                }
            }
            b.goto_block(match_end_block);
            match result_came_from {
                None => {
                    // match is divergent; we never get here
                    let inst = b.push_inst(Inst::Unreachable, "divergent match");
                    Ok(inst.as_value())
                }
                Some(came_from) => {
                    let real_incomings = b.k1.bytecode.mem.vec_to_mslice(&incomings);
                    let Inst::CameFrom { incomings: i, .. } =
                        b.k1.bytecode.instrs.get_mut(came_from)
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
            let cond_name = mformat!(b.k1.bytecode.mem, "while_cond__{}", expr.as_u32());
            let cond_block = b.push_block(cond_name);
            let body_name = mformat!(b.k1.bytecode.mem, "while_body__{}", expr.as_u32());
            let loop_body_block = b.push_block(body_name);
            let end_name = mformat!(b.k1.bytecode.mem, "while_end__{}", expr.as_u32());
            let end_block = b.push_block(end_name);
            let TypedExpr::Block(body_block) = b.k1.exprs.get(w.body) else { unreachable!() };
            let loop_scope_id = body_block.scope_id;
            b.k1.bytecode.b_loops.insert(loop_scope_id, LoopInfo { break_value: None, end_block });

            b.push_jump(cond_block, "enter while cond");

            b.goto_block(cond_block);
            compile_matching_condition(b, &w.condition, loop_body_block, end_block)?;

            b.goto_block(loop_body_block);
            let last = compile_block_stmts(b, None, w.body)?;
            if last.is_some_and(|v| !b.get_value_kind(&v).is_terminator()) {
                b.push_jump(cond_block, "goto while cond");
            }

            b.goto_block(end_block);
            Ok(Value::UNIT)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            // let start_block = self.builder.get_insert_block().unwrap();
            // let current_fn = start_block.get_parent().unwrap();
            let body_name = mformat!(b.k1.bytecode.mem, "loop_body__{}", expr.as_u32());
            let loop_body_block = b.push_block(body_name);
            let end_name = mformat!(b.k1.bytecode.mem, "loop_end__{}", expr.as_u32());
            let loop_end_block = b.push_block(end_name);

            let break_pt_id = b.get_physical_type(expr_type);

            let break_value = if expr_type != UNIT_TYPE_ID {
                Some(b.push_alloca(break_pt_id, "loop break value"))
            } else {
                None
            };
            let TypedExpr::Block(body_block) = b.k1.exprs.get(loop_expr.body_block) else {
                unreachable!()
            };
            let body_scope_id = body_block.scope_id;
            b.k1.bytecode
                .b_loops
                .insert(body_scope_id, LoopInfo { break_value, end_block: loop_end_block });

            // Go to the body
            b.push_jump(loop_body_block, "enter loop");
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if body_value.is_some_and(|v| !b.get_value_kind(&v).is_terminator()) {
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
                Ok(Value::UNIT)
            }
        }
        TypedExpr::Break(brk) => {
            let loop_info = b.k1.bytecode.b_loops.get(&brk.loop_scope).unwrap();
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
        TypedExpr::EnumConstructor(enumc) => {
            let variant_type_id = expr_type;
            let variant_pt = b.get_physical_type(variant_type_id);
            let enum_base = match dst {
                Some(dst) => dst,
                None => b.push_alloca(variant_pt, "enum literal storage").as_value(),
            };

            let tag_base = enum_base;
            let enum_variant = b.k1.types.get(variant_type_id).expect_enum_variant();
            let tag_int_value = enum_variant.tag_value;
            let int_imm = b.make_int_value(&tag_int_value, "enum tag");
            b.push_store(tag_base, int_imm, "store enum lit tag");

            if let Some(payload_expr) = &enumc.payload {
                let payload_offset =
                    b.push_struct_offset(variant_pt.expect_agg(), enum_base, 1, "enum payload ptr");
                let _payload_value = compile_expr(b, Some(payload_offset), *payload_expr)?;
            }

            Ok(enum_base)
        }
        TypedExpr::EnumGetTag(e_get_tag) => {
            let enum_base = compile_expr(b, None, e_get_tag.enum_expr_or_reference)?;
            let enum_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.exprs.get_type(e_get_tag.enum_expr_or_reference))
                    .expect_enum();
            let tag_type = enum_type.tag_type;
            let tag_scalar = b.get_physical_type(tag_type);

            // Load straight from the enum base, dont bother with a struct gep
            // task(bc): Copy if dst
            Ok(load_or_copy(
                b,
                tag_scalar,
                dst,
                enum_base,
                false,
                "get enum tag, load or copy to dst",
            ))
        }
        TypedExpr::EnumGetPayload(e_get_payload) => {
            let enum_variant_base = compile_expr(b, None, e_get_payload.enum_variant_expr)?;
            if b.get_value_kind(&enum_variant_base).is_terminator() {
                return Ok(enum_variant_base);
            }

            let variant_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.exprs.get_type(e_get_payload.enum_variant_expr))
                    .expect_enum_variant();
            let variant_pt = b.get_physical_type(variant_type.my_type_id).expect_agg();
            let variant_payload = variant_type.payload;
            let payload_offset =
                b.push_struct_offset(variant_pt, enum_variant_base, 1, "enum payload offset");
            if e_get_payload.access_kind == FieldAccessKind::ReferenceThrough {
                let base_is_reference =
                    b.k1.get_expr_type(e_get_payload.enum_variant_expr).as_reference().is_some();

                debug_assert!(base_is_reference);
                // We're generating a pointer to the payload. The variant itself is a reference
                // and the value we produce here is just a pointer to the payload
                let stored = store_simple_if_dst(b, dst, payload_offset);
                Ok(stored)
            } else {
                // We're loading the payload. The variant itself may or may not be a reference.
                // If it's a reference, we need to do a copying load to avoid incorrect aliasing
                // If it's not, we don't need to make a copy since the source is just a value
                // (albeit represented as an address)
                let make_copy = match e_get_payload.access_kind {
                    FieldAccessKind::ValueToValue => false,
                    FieldAccessKind::Dereference => true,
                    FieldAccessKind::ReferenceThrough => unreachable!(),
                };
                let payload_type_id = variant_payload.unwrap();
                let payload_pt_id = b.get_physical_type(payload_type_id);
                let copied = load_or_copy(
                    b,
                    payload_pt_id,
                    dst,
                    payload_offset,
                    make_copy,
                    "deliver enum payload",
                );
                Ok(copied)
            }
        }
        TypedExpr::Cast(c) => compile_cast(b, dst, &c, expr),
        TypedExpr::Return(typed_return) => {
            let inst = compile_expr(b, None, typed_return.value)?;
            let ret = b.push_inst(Inst::Ret(inst), "");
            Ok(ret.as_value())
        }
        TypedExpr::Lambda(lam_expr) => {
            let l = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            let function_id = l.function_id;
            let env_struct = l.environment_struct;
            b.k1.bytecode.b_units_pending_compile.push(function_id);
            compile_expr(b, dst, env_struct)
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = Value::FunctionAddr(fpe.function_id);
            let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
            let stored = store_rich_if_dst(b, dst, ptr_pt, fp, "deliver fn pointer");
            b.k1.bytecode.b_units_pending_compile.push(fpe.function_id);
            Ok(stored)
        }
        TypedExpr::FunctionToLambdaObject(fn_to_lam_obj) => {
            let obj_struct_type = b.get_physical_type(fn_to_lam_obj.lambda_object_type_id);
            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.push_alloca(obj_struct_type, "lam obj storage").as_value(),
            };
            let fn_ptr = Value::FunctionAddr(fn_to_lam_obj.function_id);
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
            b.push_store(lam_obj_env_ptr_addr, Value::PtrZero, "");
            b.k1.bytecode.b_units_pending_compile.push(fn_to_lam_obj.function_id);
            Ok(lam_obj_ptr)
        }
        TypedExpr::PendingCapture(_) => b.k1.ice_with_span("bc on PendingCapture", b.cur_span),
        TypedExpr::StaticValue(stat) => {
            let t = b.get_physical_type(expr_type);
            // We lower the simple scalar static values
            // but leave the aggregates as globals
            match b.k1.static_values.get(stat.value_id) {
                StaticValue::Unit => {
                    let store = store_simple_if_dst(b, dst, Value::byte(UNIT_BYTE_VALUE));
                    Ok(store)
                }
                StaticValue::Bool(bv) => {
                    let imm = Value::byte(*bv as u8);
                    let store = store_simple_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Char(byte) => {
                    let imm = Value::byte(*byte);
                    let store = store_simple_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Int(int) => {
                    let int = *int;
                    let imm = b.make_int_value(&int, "static int");
                    let store = store_simple_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Float(float) => {
                    let float = *float;
                    //task(bc): Pack small floats
                    let imm = b.push_inst(Inst::Data(DataInst::Float(float)), "static float");
                    let store = store_simple_if_dst(b, dst, imm.as_value());
                    Ok(store)
                }
                //task(bc) non-trival static lowerings! Zero can probably be our zero impl?
                StaticValue::String(_)
                | StaticValue::Zero(_)
                | StaticValue::Struct(_)
                | StaticValue::Enum(_)
                | StaticValue::LinearContainer(_) => {
                    let value = Value::StaticValue { t, id: stat.value_id };
                    let stored = store_rich_if_dst(b, dst, t, value, "store static value to dst");
                    Ok(stored)
                }
            }
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
        let stored = store_simple_if_dst(b, dst, field_ptr);
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
) -> TyperResult<Value> {
    let target_type_id = b.k1.exprs.get_type(expr_id);
    match c.cast_type {
        CastType::EnumToVariant
        | CastType::VariantToEnum
        | CastType::ReferenceToReference
        | CastType::ReferenceToMut
        | CastType::ReferenceUnMut
        | CastType::IntegerCast(IntegerCastDirection::NoOp)
        | CastType::Integer8ToChar
        | CastType::StaticErase
        | CastType::PointerToReference
        | CastType::ReferenceToPointer => {
            let base_noop = compile_expr(b, dst, c.base_expr)?;
            Ok(base_noop)
        }
        CastType::Transmute => {
            todo!()
        }
        CastType::IntegerCast(IntegerCastDirection::SignChange) => {
            // Treating this as a noop for now but it does mean that
            // we have a slightly incorrectly typed instr here,
            // but it should not lead to any incorrect instructions
            let base_noop = compile_expr(b, dst, c.base_expr)?;
            Ok(base_noop)
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
                        Inst::IntExtS { v: base, to }
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
            let stored = store_simple_if_dst(b, dst, inst.as_value());
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
            let stored = store_simple_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
        CastType::FloatExtend
        | CastType::FloatTruncate
        | CastType::FloatToUnsignedInteger
        | CastType::FloatToSignedInteger
        | CastType::IntegerUnsignedToFloat
        | CastType::IntegerSignedToFloat => {
            let base = compile_expr(b, None, c.base_expr)?;
            let from = b.get_value_kind(&base).expect_value().unwrap().expect_scalar();
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
            let stored = store_simple_if_dst(b, dst, inst.as_value());
            Ok(stored)
        }
        CastType::LambdaToLambdaObject => {
            let lambda_type = b.k1.get_expr_type(c.base_expr).as_lambda().unwrap();
            let lambda_function_id = lambda_type.function_id;

            let lambda_env_type = b.get_physical_type(lambda_type.env_type);
            // It seems that representing the environment of lambda objects as a pointer
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
    op: IntrinsicArithOpKind,
    call: &Call,
    dst: Option<Value>,
) -> TyperResult<Value> {
    let lhs = compile_expr(b, None, call.args[0])?;
    let rhs = compile_expr(b, None, call.args[1])?;
    use IntrinsicArithOpClass as Class;
    use IntrinsicArithOpOp as Op;
    let lhs_type = b.k1.exprs.get_type(call.args[0]);
    let lhs_width = b.k1.types.get_layout(lhs_type).size_bits() as u8;
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
    let stored = store_simple_if_dst(b, dst, res.as_value());
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
    comment: &str,
) -> Value {
    match pt {
        PhysicalType::Agg(_) => {
            if make_copy {
                let dst = b.push_alloca(pt, comment);
                b.push_copy(dst.as_value(), src, pt, comment);
                dst.as_value()
            } else {
                src
            }
        }
        PhysicalType::Scalar(st) => b.push_load(st, src, comment).as_value(),
    }
}

fn store_value(
    b: &mut Builder,
    pt: PhysicalType,
    dst: Value,
    value: Value,
    comment: &str,
) -> InstId {
    match pt {
        PhysicalType::Agg(_) => {
            // Rename to `src` shows that, since we have an aggregate, `value` is a location.
            let src = value;
            b.push_copy(dst, src, pt, comment)
        }
        PhysicalType::Scalar(_) => b.push_store(dst, value, comment),
    }
}

fn load_or_copy(
    b: &mut Builder,
    pt: PhysicalType,
    dst: Option<Value>,
    src: Value,
    copy_aggregates: bool,
    comment: &str,
) -> Value {
    match dst {
        Some(dst) => b.push_copy(dst, src, pt, comment).as_value(),
        None => load_value(b, pt, src, copy_aggregates, comment),
    }
}

fn compile_matching_condition(
    b: &mut Builder,
    mc: &MatchingCondition,
    cons_block: BlockId,
    condition_fail_block: BlockId,
) -> TyperResult<()> {
    if mc.instrs.is_empty() {
        // Always true
        b.push_jump(cons_block, "empty condition");
        return Ok(());
    }
    for (index, inst) in b.k1.mem.getn(mc.instrs).iter().enumerate() {
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(b, None, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_value: Value = compile_expr(b, None, *value)?;
                if b.k1.exprs.get_type(*value) == NEVER_TYPE_ID {
                    return Ok(());
                }
                let continue_name = mformat!(b.k1.bytecode.mem, "mc_cont__{}", index + 1);
                let continue_block = b.push_block(continue_name);
                b.push_jump_if(
                    cond_value,
                    continue_block,
                    condition_fail_block,
                    "matching cond cond",
                );
                b.goto_block(continue_block);
            }
        }
    }
    b.push_jump(cons_block, "matching cond fallthrough cons");
    Ok(())
}

pub fn zero(t: ScalarType) -> Value {
    match t {
        ScalarType::U8 => Value::Imm32 { t: ScalarType::U8, data: 0 },
        ScalarType::U16 => Value::Imm32 { t: ScalarType::U16, data: 0 },
        ScalarType::U32 => Value::Imm32 { t: ScalarType::U32, data: 0 },
        ScalarType::U64 => Value::Imm32 { t: ScalarType::U64, data: 0 },
        ScalarType::I8 => Value::Imm32 { t: ScalarType::I8, data: 0 },
        ScalarType::I16 => Value::Imm32 { t: ScalarType::I16, data: 0 },
        ScalarType::I32 => Value::Imm32 { t: ScalarType::I32, data: 0 },
        ScalarType::I64 => Value::Imm32 { t: ScalarType::I64, data: 0 },
        ScalarType::F32 => Value::Imm32 { t: ScalarType::F32, data: (0.0f32).to_bits() },
        ScalarType::F64 => {
            // Bit pattern is all zeroes anyway
            Value::Imm32 { t: ScalarType::F64, data: (0.0f64).to_bits() as u32 }
        }
        ScalarType::Pointer => Value::PtrZero,
    }
}

pub fn get_compiled_unit(bc: &ProgramBytecode, unit: CompilableUnitId) -> Option<CompiledUnit> {
    match unit {
        CompilableUnitId::Function(function_id) => bc.functions.get(function_id).as_ref().copied(),
        CompilableUnitId::Expr(typed_expr_id) => bc.exprs.get(&typed_expr_id).copied(),
    }
}

pub fn get_unit_span(k1: &TypedProgram, unit: CompilableUnitId) -> SpanId {
    match unit {
        CompilableUnitId::Function(function_id) => k1.get_function_span(function_id),
        CompilableUnitId::Expr(typed_expr_id) => k1.exprs.get_span(typed_expr_id),
    }
}

////////////////////////////// Validation //////////////////////////////

pub fn validate_unit(k1: &TypedProgram, unit_id: CompilableUnitId) -> TyperResult<()> {
    let mut errors = Vec::new();
    let bc = &k1.bytecode;
    let span = get_unit_span(k1, unit_id);
    let Some(unit) = get_compiled_unit(&k1.bytecode, unit_id) else {
        return failf!(span, "Not compiled");
    };
    for (block_index, block) in bc.mem.getn(unit.blocks).iter().enumerate() {
        for (index, inst_id) in bc.mem.getn(block.instrs).iter().enumerate() {
            let is_last = index as u32 == block.instrs.len() - 1;
            let inst = bc.instrs.get(*inst_id);
            let inst_kind = get_inst_kind(bc, &k1.types, *inst_id);
            if !is_last && inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: stray terminator"))
            };
            if is_last && !inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: unterminated"))
            }

            match inst {
                Inst::Data(_imm) => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, .. } => {
                    let dst_type = get_value_kind(bc, &k1.types, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("store dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::Load { src, .. } => {
                    let src_kind = get_value_kind(bc, &k1.types, src);
                    if !src_kind.is_storage() {
                        errors.push(format!("i{inst_id}: load src is not storage"))
                    }
                }
                Inst::Copy { dst, src, .. } => {
                    let src_type = get_value_kind(bc, &k1.types, src);
                    if !src_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy src is not a ptr"))
                    }
                    let dst_type = get_value_kind(bc, &k1.types, dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("i{inst_id}: copy dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = get_value_kind(bc, &k1.types, base);
                    if !base_type.is_storage() {
                        errors.push(format!("i{inst_id}: struct_offset base is not a ptr"))
                    }
                }
                Inst::ArrayOffset { base, element_index, .. } => {
                    let base_type = get_value_kind(bc, &k1.types, base);
                    let index_type = get_value_kind(bc, &k1.types, element_index);
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
                    if *block >= unit.blocks.len() {
                        errors.push(format!("i{inst_id}: jump to non-existent block b{}", *block))
                    }
                }
                Inst::JumpIf { cond, .. } => {
                    let cond_type = get_value_kind(bc, &k1.types, cond);
                    if !cond_type.is_value() {
                        errors.push(format!("i{inst_id}: jumpif cond is not a value"))
                    }
                }
                Inst::Unreachable => (),
                Inst::CameFrom { .. } => (),
                Inst::Ret(v) => {
                    let ret_val_type = get_value_kind(bc, &k1.types, v);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("i{inst_id}: ret value is not a value"))
                    }
                }
                Inst::BoolNegate { v } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !inst_type.is_byte() {
                        errors.push(format!("i{inst_id}: bool_negate src is not a bool"))
                    }
                }
                Inst::BitNot { v } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !inst_type.is_int() {
                        errors.push(format!("i{inst_id}: bit_not src is not an int"))
                    }
                }
                Inst::IntTrunc { to, .. } => {
                    if !to.is_int() {
                        errors.push("i{inst_id}: int trunc to non-int type".to_string())
                    }
                }
                Inst::IntExtU { v, to } | Inst::IntExtS { v, to } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !inst_type.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u src is not an int"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: int_ext_u to is not int"))
                    }
                }
                Inst::FloatTrunc { v, to } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F64))
                    {
                        errors.push(format!("i{inst_id}: float_trunc src is not f64"))
                    }
                    if *to != ScalarType::F32 {
                        errors.push(format!("i{inst_id}: float_trunc to is not f32"))
                    }
                }
                Inst::FloatExt { v, to } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float_ext src is not f32"))
                    }
                    if *to != ScalarType::F64 {
                        errors.push(format!("i{inst_id}: float_ext to is not f64"))
                    }
                }
                Inst::Float32ToIntUnsigned { v, to } | Inst::Float32ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !(inst_type.as_value().and_then(|t| t.as_scalar()) == Some(ScalarType::F32))
                    {
                        errors.push(format!("i{inst_id}: float32_to_int src is not f32"))
                    }
                    if !to.is_int() {
                        errors.push(format!("i{inst_id}: float32_to_int to is not int"))
                    }
                }
                Inst::Float64ToIntUnsigned { v, to } | Inst::Float64ToIntSigned { v, to } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
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
                    let inst_type = get_value_kind(bc, &k1.types, v);
                    if !inst_type.is_storage() {
                        errors.push(format!("i{inst_id}: ptr_to_word src is not a ptr"))
                    }
                }
                Inst::WordToPtr { v } => {
                    let inst_type = get_value_kind(bc, &k1.types, v);
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
        Err(TyperError {
            span,
            message: format!(
                "Bytecode Unit failed validation\n{}\n{}",
                compiled_unit_to_string(k1, unit_id, true),
                error_string
            ),
            level: ErrorLevel::Error,
        })
    } else {
        Ok(())
    }
}

////////////////////////////// Display //////////////////////////////

pub fn compiled_unit_to_string(
    k1: &TypedProgram,
    unit: CompilableUnitId,
    show_source: bool,
) -> String {
    let mut s = String::new();
    let unit = get_compiled_unit(&k1.bytecode, unit).unwrap();
    display_unit(&mut s, k1, &unit, show_source).unwrap();
    s
}

pub fn display_unit_name(
    w: &mut impl Write,
    k1: &TypedProgram,
    unit: CompilableUnitId,
) -> std::fmt::Result {
    match unit {
        CompilableUnitId::Function(function_id) => {
            let function = k1.functions.get(function_id);
            k1.write_qualified_name(w, function.scope, k1.ident_str(function.name), "/", true);
        }
        CompilableUnitId::Expr(typed_expr_id) => {
            let expr_span = k1.exprs.get_span(typed_expr_id);
            let (source, line) = k1.get_span_location(expr_span);
            write!(w, "expr {}:{}", &source.filename, line.line_number())?;
        }
    };
    Ok(())
}

pub fn display_unit(
    w: &mut impl Write,
    k1: &TypedProgram,
    unit: &CompiledUnit,
    show_source: bool,
) -> std::fmt::Result {
    match unit.unit_id {
        CompilableUnitId::Function(function_id) => {
            k1.write_ident(w, k1.functions.get(function_id).name)?;
        }
        CompilableUnitId::Expr(typed_expr_id) => {
            let expr_span = k1.exprs.get_span(typed_expr_id);
            let (source, line) = k1.get_span_location(expr_span);
            write!(w, "expr {}:{}", &source.filename, line.line_number())?;
        }
    };
    writeln!(w, " (offset={}, inst count={})", unit.inst_offset, unit.inst_count)?;
    for (index, _block) in k1.bytecode.mem.getn(unit.blocks).iter().enumerate() {
        let id = index as BlockId;
        display_block(w, k1, &k1.bytecode, unit, id, show_source)?;
    }
    Ok(())
}

pub fn display_compiled_expr(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    expr_id: TypedExprId,
    show_source: bool,
) -> std::fmt::Result {
    let Some(unit) = bc.exprs.get(&expr_id) else { return Ok(()) };
    display_unit(w, k1, unit, show_source)
}

pub fn display_function(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: FunctionId,
    show_source: bool,
) -> std::fmt::Result {
    let Some(unit) = bc.functions.get(function) else { return Ok(()) };
    display_unit(w, k1, unit, show_source)
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    unit: &CompiledUnit,
    block_id: BlockId,
    show_source: bool,
) -> std::fmt::Result {
    let block = bc.mem.get_nth(unit.blocks, block_id as usize);
    write!(w, "b{} ", block_id)?;
    if !block.name.is_empty() {
        w.write_str(bc.mem.get_str(block.name))?;
    }
    writeln!(w)?;
    for inst_id in bc.mem.getn(block.instrs).iter() {
        if show_source {
            let span_id = bc.sources.get(*inst_id);
            let lines = k1.ast.get_span_content(*span_id);
            let first_line = lines.lines().next().unwrap_or("");
            write!(w, "|  {first_line:50}|")?;
        }

        write!(w, " i{:3} = ", *inst_id)?;
        display_inst(w, k1, bc, *inst_id)?;
        let comment = bc.comments.get(*inst_id);
        if !comment.is_empty() {
            write!(w, " ; {}", bc.mem.get_str(*comment))?;
        }
        writeln!(w)?;
    }
    writeln!(w, "END")?;
    Ok(())
}

pub fn inst_to_string(k1: &TypedProgram, inst_id: InstId) -> String {
    let mut s = String::new();
    display_inst(&mut s, k1, &k1.bytecode, inst_id).unwrap();
    s
}

pub fn display_inst(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    inst_id: InstId,
) -> std::fmt::Result {
    match bc.instrs.get(inst_id) {
        Inst::Data(imm) => {
            write!(w, "imm ")?;
            display_imm(w, imm)?;
        }
        Inst::Alloca { t, vm_layout } => {
            write!(w, "alloca ")?;
            display_pt(w, &k1.types, t)?;
            write!(w, ", align {}", vm_layout.align)?;
        }
        Inst::Store { dst, value, t } => {
            write!(w, "store to {}, ", *dst,)?;
            display_scalar_type(w, t)?;
            write!(w, " {}", *value)?;
        }
        Inst::Load { t, src } => {
            write!(w, "load ")?;
            display_scalar_type(w, t)?;
            write!(w, " from {}", *src)?;
        }
        Inst::Copy { dst, src, t: _, vm_size } => {
            write!(w, "copy {} {}, src {}", *vm_size, *dst, *src)?;
        }
        Inst::StructOffset { struct_t, base, field_index, vm_offset } => {
            write!(w, "struct_offset ")?;
            display_pt(w, &k1.types, &PhysicalType::Agg(*struct_t))?;
            write!(w, ".{}, {} ({})", *field_index, *base, *vm_offset)?;
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            write!(w, "array_offset ")?;
            display_pt(w, &k1.types, element_t)?;
            write!(w, " {}[{}]", *base, *element_index)?;
        }
        Inst::Call { id } => {
            let call = bc.calls.get(*id);
            write!(w, "call ")?;
            display_inst_kind(w, &k1.types, &call.ret_inst_kind)?;
            if let Some(dst) = &call.dst {
                write!(w, " into {}", *dst)?;
            }
            match &call.callee {
                BcCallee::Builtin(intrinsic_operation) => {
                    write!(w, " builtin {:?}", intrinsic_operation)?;
                }
                BcCallee::Direct(function_id) => {
                    write!(w, " ")?;
                    w.write_str(k1.ident_str(k1.get_function(*function_id).name))?;
                }
                BcCallee::Indirect(callee_inst) => {
                    write!(w, " indirect {}", *callee_inst)?;
                }
            };
            w.write_str("(")?;
            for (index, arg) in bc.mem.getn(call.args).iter().enumerate() {
                write!(w, "{}", *arg)?;
                let last = index == call.args.len() as usize - 1;
                if !last {
                    w.write_str(", ")?;
                }
            }
            w.write_str(")")?;
        }
        Inst::Jump(block_id) => {
            write!(w, "jmp b{}", *block_id)?;
        }
        Inst::JumpIf { cond, cons, alt } => {
            write!(w, "jmpif {}, b{}, b{}", *cond, *cons, *alt)?;
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
        }
        Inst::CameFrom { t, incomings } => {
            write!(w, "comefrom ")?;
            display_pt(w, &k1.types, t)?;
            write!(w, " [")?;
            for (i, incoming) in bc.mem.getn(*incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "(b{}: {})", incoming.from, incoming.value)?;
            }
            write!(w, "]")?;
        }
        Inst::Ret(value) => {
            write!(w, "ret {}", *value)?;
        }
        Inst::BoolNegate { v } => {
            write!(w, "bool not {}", v)?;
        }
        Inst::BitNot { v } => {
            write!(w, "bitnot {}", v)?;
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
        Inst::IntExtS { v, to } => {
            write!(w, "int extend signed ")?;
            display_scalar_type(w, to)?;
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
            write!(w, "add i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntSub { lhs, rhs, width } => {
            write!(w, "sub i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntMul { lhs, rhs, width } => {
            write!(w, "mul i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntDivUnsigned { lhs, rhs, width } => {
            write!(w, "udiv i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntDivSigned { lhs, rhs, width } => {
            write!(w, "sdiv i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntRemUnsigned { lhs, rhs, width } => {
            write!(w, "urem i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntRemSigned { lhs, rhs, width } => {
            write!(w, "srem i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::IntCmp { lhs, rhs, pred, width } => {
            write!(w, "icmp i{width} {} {} {}", pred, *lhs, *rhs)?;
        }
        Inst::FloatAdd { lhs, rhs, width } => {
            write!(w, "fadd f{width} {} {}", *lhs, *rhs)?;
        }
        Inst::FloatSub { lhs, rhs, width } => {
            write!(w, "fsub f{width} {} {}", *lhs, *rhs)?;
        }
        Inst::FloatMul { lhs, rhs, width } => {
            write!(w, "fmul f{width} {} {}", *lhs, *rhs)?;
        }
        Inst::FloatDiv { lhs, rhs, width } => {
            write!(w, "fdiv f{width} {} {}", *lhs, *rhs)?;
        }
        Inst::FloatRem { lhs, rhs, width } => {
            write!(w, "frem f{width} {} {}", *lhs, *rhs)?;
        }
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            write!(w, "fcmp f{width} {} {} {}", pred, *lhs, *rhs)?;
        }
        Inst::BitAnd { lhs, rhs, width } => {
            write!(w, "and i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BitOr { lhs, rhs, width } => {
            write!(w, "or i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BitXor { lhs, rhs, width } => {
            write!(w, "xor i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BitShiftLeft { lhs, rhs, width } => {
            write!(w, "shl i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
            write!(w, "lshr i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BitSignedShiftRight { lhs, rhs, width } => {
            write!(w, "ashr i{width} {} {}", *lhs, *rhs)?;
        }
        Inst::BakeStaticValue { type_id, value } => {
            write!(w, "bake ")?;
            k1.display_type_id(w, *type_id, false)?;
            write!(w, " {}", *value)?;
        }
    };
    Ok(())
}

pub fn display_inst_kind(
    w: &mut impl std::fmt::Write,
    types: &TypePool,
    kind: &InstKind,
) -> std::fmt::Result {
    match kind {
        InstKind::Value(t) => display_pt(w, types, t),
        InstKind::Void => write!(w, "void"),
        InstKind::Terminator => write!(w, "terminator"),
    }
}

pub fn display_scalar_type(w: &mut impl Write, scalar: &ScalarType) -> std::fmt::Result {
    match scalar {
        ScalarType::U8 => write!(w, "u8"),
        ScalarType::U16 => write!(w, "u16"),
        ScalarType::U32 => write!(w, "u32"),
        ScalarType::U64 => write!(w, "u64"),
        ScalarType::I8 => write!(w, "i8"),
        ScalarType::I16 => write!(w, "i16"),
        ScalarType::I32 => write!(w, "i32"),
        ScalarType::I64 => write!(w, "i64"),
        ScalarType::F32 => write!(w, "f32"),
        ScalarType::F64 => write!(w, "f64"),
        ScalarType::Pointer => write!(w, "ptr"),
    }
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_scalar_type(f, self)
    }
}

pub fn display_imm(w: &mut impl Write, imm: &DataInst) -> std::fmt::Result {
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
        Value::Global { id, .. } => write!(w, "g{}", id.as_u32()),
        Value::StaticValue { id, .. } => write!(w, "static{}", id.as_u32()),
        Value::FunctionAddr(function_id) => write!(w, "f{}", function_id.as_u32()),
        Value::FnParam { index, .. } => write!(w, "p{}", index),
        Value::Imm32 { t: _, data } => write!(w, "data32({})", data),
        Value::PtrZero => write!(w, "ptr0"),
    }
}
