use crate::compiler::WordSize;
// Copyright (c) 2025 knix
// All rights reserved.
//
// The goal here is a strongly-typed SSA form
// instruction-based IR with basic blocks, obviously
// very much like LLVM, as that is our primary target.
// But I currently think there's going to be a lot of value
// in having our own. It'll be easier to write an interpreter for
// and will help make adding other backends far, far easier
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::StaticValueId;
use crate::{
    SV8,
    kmem::{self, MSlice, MStr},
    lex::SpanId,
    nz_u32_id,
    pool::VPool,
    typer::{types::*, *},
};
use crate::{failf, mformat};
use ahash::HashMapExt;
use fxhash::FxHashMap;
use itertools::Itertools;
use log::debug;
use smallvec::smallvec;
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
    pub comments: VPool<MStr<ProgramBytecode>, InstId>,
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
    pub b_units_pending_compile: Vec<CompilableUnit>,
}

#[derive(Clone, Copy)]
pub enum CompilableUnit {
    Function(FunctionId),
    Expr(TypedExprId),
}

pub struct BcModuleConfig {
    pub word_size: WordSize,
}

impl ProgramBytecode {
    pub fn make(instr_count_hint: usize, word_size: WordSize) -> Self {
        let function_count_hint = instr_count_hint / 128;
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
        match self.module_config.word_size {
            WordSize::W32 => ScalarType::I32,
            WordSize::W64 => ScalarType::I64,
        }
    }
}

#[derive(Clone)]
pub struct Block {
    pub name: MStr<ProgramBytecode>,
    pub instrs: Vec<InstId>,
}

#[derive(Clone, Copy)]
pub struct CompiledBlock {
    pub name: MStr<ProgramBytecode>,
    pub instrs: MSlice<InstId, ProgramBytecode>,
}

#[derive(Clone, Copy)]
pub struct CompiledUnit {
    pub unit: CompilableUnit,
    // The offset of the first instruction id
    // used by this compiled unit.
    // Subtract this to get sane indices for dense storage
    pub inst_offset: u32,
    pub blocks: MSlice<CompiledBlock, ProgramBytecode>,
}

#[derive(Clone, Copy)]
pub enum Imm {
    I64(u64),
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
    BakeStaticValue,
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
    const FALSE: Value = Value::byte(0);
    const TRUE: Value = Value::byte(1);
    const fn byte(u8: u8) -> Value {
        Value::Imm32 { t: ScalarType::I8, data: u8 as u32 }
    }
    const fn imm32(u32: u32) -> Value {
        Value::Imm32 { t: ScalarType::I32, data: u32 }
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
    // Data
    Imm(Imm),

    // Memory manipulation
    Alloca {
        t: PhysicalType,
        vm_layout: Layout,
    },
    Store {
        dst: Value,
        value: Value,
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
    /// task(bc): Optimize size of Call variant
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

    // Operations
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
    },
    WordToPtr {
        v: Value,
    },
    //task(bc): Break these out into their own instructions
    ArithBin {
        op: IntrinsicArithOpKind,
        lhs: Value,
        rhs: Value,
    },
    BitwiseBin {
        op: IntrinsicBitwiseBinopKind,
        lhs: Value,
        rhs: Value,
    },
}

pub fn get_value_kind(bc: &ProgramBytecode, types: &TypePool, value: &Value) -> InstKind {
    match value {
        Value::Inst(inst_id) => get_inst_kind(bc, types, *inst_id),
        Value::Global { t, id: _ } => InstKind::Value(*t),
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
        Inst::Imm(imm) => match imm {
            Imm::I64(_) => InstKind::U64,
            Imm::Float(TypedFloatValue::F32(_)) => InstKind::scalar(ScalarType::F32),
            Imm::Float(TypedFloatValue::F64(_)) => InstKind::scalar(ScalarType::F64),
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
        Inst::BoolNegate { .. } => InstKind::I8,
        Inst::BitNot { v } => get_value_kind(bc, types, v),
        Inst::IntTrunc { to, .. } => InstKind::scalar(*to),
        Inst::IntExtU { to, .. } => InstKind::scalar(*to),
        Inst::IntExtS { to, .. } => InstKind::scalar(*to),
        Inst::FloatTrunc { to, .. } => InstKind::scalar(*to),
        Inst::FloatExt { to, .. } => InstKind::scalar(*to),
        Inst::FloatToIntSigned { to, .. } => InstKind::scalar(*to),
        Inst::FloatToIntUnsigned { to, .. } => InstKind::scalar(*to),
        Inst::IntToFloatUnsigned { to, .. } => InstKind::scalar(*to),
        Inst::IntToFloatSigned { to, .. } => InstKind::scalar(*to),
        Inst::PtrToWord { .. } => InstKind::scalar(bc.word_sized_int()),
        Inst::WordToPtr { .. } => InstKind::PTR,
        Inst::ArithBin { op, lhs, .. } => match op.op {
            IntrinsicArithOpOp::Equals => InstKind::I8,
            IntrinsicArithOpOp::Add => get_value_kind(bc, types, lhs),
            IntrinsicArithOpOp::Sub => get_value_kind(bc, types, lhs),
            IntrinsicArithOpOp::Mul => get_value_kind(bc, types, lhs),
            IntrinsicArithOpOp::Div => get_value_kind(bc, types, lhs),
            IntrinsicArithOpOp::Rem => get_value_kind(bc, types, lhs),
            IntrinsicArithOpOp::Lt => InstKind::I8,
            IntrinsicArithOpOp::Le => InstKind::I8,
            IntrinsicArithOpOp::Gt => InstKind::I8,
            IntrinsicArithOpOp::Ge => InstKind::I8,
        },
        Inst::BitwiseBin { op, lhs, .. } => match op {
            IntrinsicBitwiseBinopKind::And => get_value_kind(bc, types, lhs),
            IntrinsicBitwiseBinopKind::Or => get_value_kind(bc, types, lhs),
            IntrinsicBitwiseBinopKind::Xor => get_value_kind(bc, types, lhs),
            IntrinsicBitwiseBinopKind::ShiftLeft => get_value_kind(bc, types, lhs),
            IntrinsicBitwiseBinopKind::SignedShiftRight => get_value_kind(bc, types, lhs),
            IntrinsicBitwiseBinopKind::UnsignedShiftRight => get_value_kind(bc, types, lhs),
        },
    }
}

#[derive(Clone, Copy)]
pub enum InstKind {
    Value(PhysicalType),
    Void,
    Terminator,
}

impl InstKind {
    pub const PTR: InstKind = Self::scalar(ScalarType::Pointer);
    pub const I8: InstKind = Self::scalar(ScalarType::I8);
    pub const U16: InstKind = Self::scalar(ScalarType::I16);
    pub const U32: InstKind = Self::scalar(ScalarType::I32);
    pub const U64: InstKind = Self::scalar(ScalarType::I64);

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
        matches!(self, InstKind::Value(PhysicalType::Scalar(ScalarType::I8)))
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

impl Inst {}

pub fn compile_function(
    k1: &mut TypedProgram,
    function_id: FunctionId,
    compile_deps: bool,
) -> TyperResult<()> {
    let start = k1.timing.clock.raw();

    let inst_offset = k1.bytecode.instrs.len() as u32;
    let mut builder =
        Builder { inst_offset, k1, block_count: 0, cur_block: 0, cur_span: SpanId::NONE };

    compile_unit_rec(&mut builder, CompilableUnit::Function(function_id), compile_deps)?;

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_bytecode_nanos += elapsed;
    Ok(())
}

pub fn compile_top_level_expr(
    k1: &mut TypedProgram,
    expr: TypedExprId,
    input_parameters: &[(VariableId, StaticValueId)],
    compile_deps: bool,
) -> TyperResult<()> {
    let start = k1.timing.clock.raw();

    let inst_offset = k1.bytecode.instrs.len() as u32;
    let mut b = Builder { inst_offset, k1, block_count: 0, cur_block: 0, cur_span: SpanId::NONE };
    for (variable_id, static_value_id) in input_parameters {
        let variable = b.k1.variables.get(*variable_id);
        let pt = b.get_physical_type(variable.type_id);
        b.k1.bytecode.b_variables.push(BuilderVariable {
            id: *variable_id,
            value: Value::StaticValue { t: pt, id: *static_value_id },
            indirect: false,
        })
    }
    compile_unit_rec(&mut b, CompilableUnit::Expr(expr), compile_deps)?;

    let elapsed = k1.timing.elapsed_nanos(start);
    k1.timing.total_bytecode_nanos += elapsed;
    Ok(())
}

fn compile_unit_rec(b: &mut Builder, unit: CompilableUnit, compile_deps: bool) -> TyperResult<()> {
    let mut unit = Some(unit);
    loop {
        if unit.is_none() {
            if compile_deps {
                unit = b.k1.bytecode.b_units_pending_compile.pop();
            }
        }

        match unit {
            None => return Ok(()),
            Some(u) => {
                compile_unit(b, u)?;
                unit = None;
            }
        }
    }
}

fn compile_unit(b: &mut Builder, unit: CompilableUnit) -> TyperResult<()> {
    match unit {
        CompilableUnit::Function(function_id) => {
            eprintln!("Compiling function {}", b.k1.function_id_to_string(function_id, false));
            let name = b.k1.bytecode.mem.push_str("entry");
            b.push_block(name);
            let f = b.k1.get_function(function_id);
            let fn_span = b.k1.ast.get_span_for_id(f.parsed_id);
            b.cur_span = fn_span;

            // Set up parameters
            let param_variables = f.param_variables.clone();
            for (index, param_variable_id) in param_variables.iter().enumerate() {
                let v = b.k1.variables.get(*param_variable_id);
                let t = b.get_physical_type(v.type_id);
                b.k1.bytecode.b_variables.push(BuilderVariable {
                    id: *param_variable_id,
                    value: Value::FnParam { t, index: index as u32 },
                    indirect: false,
                });
            }

            let compiled_blocks = b.bake_blocks();
            let function = CompiledUnit {
                unit: CompilableUnit::Function(function_id),
                inst_offset: b.inst_offset,
                blocks: compiled_blocks,
            };
            b.reset_compilation_unit();

            *b.k1.bytecode.functions.get_mut(function_id) = Some(function);
            Ok(())
        }
        CompilableUnit::Expr(typed_expr_id) => {
            eprintln!("Compiling expr {}", b.k1.expr_to_string(typed_expr_id));
            let name = b.k1.bytecode.mem.push_str("expr_");
            b.push_block(name);

            let result = compile_expr(b, None, typed_expr_id)?;
            b.push_inst(Inst::Ret(result));

            let compiled_blocks = b.bake_blocks();
            let compiled_expr = CompiledUnit {
                unit: CompilableUnit::Expr(typed_expr_id),
                inst_offset: b.inst_offset,
                blocks: compiled_blocks,
            };
            b.reset_compilation_unit();
            b.k1.bytecode.exprs.insert(typed_expr_id, compiled_expr);
            Ok(())
        }
    }
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
    cur_block: BlockId,
    cur_span: SpanId,
}

impl<'k1> Builder<'k1> {
    fn reset_compilation_unit(&mut self) {
        for b in &mut self.k1.bytecode.b_blocks {
            b.instrs.clear();
        }
        self.block_count = 0;
        self.k1.bytecode.b_variables.clear();
        self.k1.bytecode.b_loops.clear();
    }

    fn bake_blocks(&mut self) -> MSlice<CompiledBlock, ProgramBytecode> {
        let mut blocks = self.k1.bytecode.mem.new_vec(self.k1.bytecode.b_blocks.len() as u32);
        for b in Builder::builder_blocks_iter(self.block_count, &self.k1.bytecode.b_blocks) {
            let instrs = self.k1.bytecode.mem.push_slice(&b.instrs);
            let b = CompiledBlock { name: b.name, instrs };
            blocks.push(b)
        }
        self.k1.bytecode.mem.vec_to_mslice(&blocks)
    }

    fn builder_blocks_iter(block_count: u32, b_blocks: &[Block]) -> impl Iterator<Item = &Block> {
        b_blocks[0..block_count as usize].iter()
    }

    fn push_inst_to(&mut self, block: BlockId, inst: Inst) -> InstId {
        let id = self.k1.bytecode.instrs.add(inst);
        let ids = self.k1.bytecode.sources.add(self.cur_span);
        let idc = self.k1.bytecode.comments.add(MStr::empty());
        debug_assert!(id == ids && id == idc);

        self.k1.bytecode.b_blocks[block as usize].instrs.push(id);
        id
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.k1.bytecode, &self.k1.types, inst)
    }

    pub fn get_value_kind(&self, value: &Value) -> InstKind {
        get_value_kind(&self.k1.bytecode, &self.k1.types, value)
    }

    fn alloca_type(&mut self, pt: PhysicalType) -> InstId {
        let layout = self.k1.types.get_pt_layout(&pt);
        self.push_inst_to(self.cur_block, Inst::Alloca { t: pt, vm_layout: layout })
    }

    fn push_inst(&mut self, inst: Inst) -> InstId {
        self.push_inst_to(self.cur_block, inst)
    }

    fn push_struct_offset(
        &mut self,
        agg_id: PhysicalTypeId,
        base: Value,
        field_index: u32,
    ) -> InstId {
        let Some(offset) = self.k1.types.get_struct_field_offset(agg_id, field_index) else {
            b_ice!(self, "Failed getting offset for field")
        };
        self.push_inst(Inst::StructOffset {
            struct_t: agg_id,
            base,
            field_index,
            vm_offset: offset,
        })
    }

    fn push_jump(&mut self, block_id: BlockId) -> InstId {
        self.push_inst(Inst::Jump(block_id))
    }

    fn push_copy(&mut self, dst: Value, src: Value, pt: PhysicalType) -> InstId {
        let layout = self.k1.types.get_pt_layout(&pt);
        self.push_inst(Inst::Copy { dst, src, t: pt, vm_size: layout.size })
    }

    fn push_load(&mut self, st: ScalarType, src: Value) -> InstId {
        self.push_inst(Inst::Load { t: st, src })
    }

    fn push_store(&mut self, dst: Value, value: Value) -> InstId {
        self.push_inst(Inst::Store { dst, value })
    }

    fn push_int_value(&mut self, int_value: &TypedIntValue) -> Value {
        match int_value {
            TypedIntValue::U8(i) => Value::Imm32 { t: ScalarType::I8, data: *i as u32 },
            TypedIntValue::U16(i) => Value::Imm32 { t: ScalarType::I16, data: *i as u32 },
            TypedIntValue::U32(i) | TypedIntValue::UWord32(i) => {
                Value::Imm32 { t: ScalarType::I32, data: *i }
            }
            TypedIntValue::U64(i) | TypedIntValue::UWord64(i) => {
                if *i <= u32::MAX as u64 {
                    Value::imm32(*i as u32)
                } else {
                    let inst = self.push_inst(Inst::Imm(Imm::I64(*i)));
                    inst.as_value()
                }
            }
            TypedIntValue::I8(i) => Value::byte(*i as u8),
            TypedIntValue::I16(i) => Value::Imm32 { t: ScalarType::I16, data: *i as u32 },
            TypedIntValue::I32(i) | TypedIntValue::IWord32(i) => Value::imm32(*i as u32),
            TypedIntValue::I64(i) | TypedIntValue::IWord64(i) => {
                if *i >= i32::MIN as i64 && *i <= i32::MAX as i64 {
                    Value::imm32(*i as u32)
                } else {
                    let inst = self.push_inst(Inst::Imm(Imm::I64(*i as u64)));
                    inst.as_value()
                }
            }
        }
    }

    fn push_block(&mut self, name: MStr<ProgramBytecode>) -> BlockId {
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
            b_ice!(self, "Not a physical type: {}", self.k1.type_id_to_string(type_id))
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
        Some(dst) => b.push_store(dst, value).as_value(),
    }
}

fn store_rich_if_dst(b: &mut Builder, dst: Option<Value>, pt: PhysicalType, value: Value) -> Value {
    match dst {
        None => value,
        Some(dst) => store_value(b, pt, dst, value).as_value(),
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

    let mut last_ret = None;
    let statements = body.statements.clone();
    for (index, &stmt) in statements.iter().enumerate() {
        let is_last = index == statements.len() - 1;
        let stmt_dst = if is_last { dst } else { None };
        last_ret = Some(compile_stmt(b, stmt_dst, stmt)?);
    }

    Ok(last_ret)
}

fn compile_stmt(b: &mut Builder, dst: Option<Value>, stmt: TypedStmtId) -> TyperResult<Value> {
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
            let variable_alloca = b.alloca_type(var_pt_id);

            // value_ptr means a pointer matching the type of the rhs
            // For a referencing let, the original alloca a ptr
            // So we need one for the inner type as well
            let value_ptr = if let_stmt.is_referencing {
                let Type::Reference(reference_type) = b.k1.types.get(let_stmt.variable_type) else {
                    panic!("Expected reference for referencing let");
                };
                let reference_inner_type_pt_id = b.get_physical_type(reference_type.inner_type);
                let value_alloca = b.alloca_type(reference_inner_type_pt_id);
                b.push_store(variable_alloca.as_value(), value_alloca.as_value());
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
                            v.span,
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
            if req.condition.diverges {
                b.push_inst(Inst::Unreachable);
            }

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
    b.cur_span = b.k1.exprs.get(expr).get_span();
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);
    let e = b.k1.exprs.get(expr).clone();
    match e {
        TypedExpr::Struct(struct_literal) => {
            let struct_pt = b.get_physical_type(struct_literal.type_id);
            let struct_agg_id = struct_pt.expect_agg();
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.alloca_type(struct_pt).as_value(),
            };
            for (field_index, field) in struct_literal.fields.iter().enumerate() {
                debug_assert!(b.k1.types.get(struct_literal.type_id).as_struct().is_some());
                let Some(offset) =
                    b.k1.types.get_struct_field_offset(struct_agg_id, field_index as u32)
                else {
                    b_ice!(b, "Failed getting offset for field")
                };
                let struct_offset = b.push_inst(Inst::StructOffset {
                    struct_t: struct_agg_id,
                    base: struct_base,
                    field_index: field_index as u32,
                    vm_offset: offset,
                });
                compile_expr(b, Some(struct_offset.as_value()), field.expr)?;
            }
            Ok(struct_base)
        }
        TypedExpr::StructFieldAccess(field_access) => {
            let struct_base = compile_expr(b, None, field_access.base)?;
            let struct_pt_id = b.get_physical_type(field_access.struct_type).expect_agg();
            let Some(offset) =
                b.k1.types.get_struct_field_offset(struct_pt_id, field_access.field_index)
            else {
                b_ice!(b, "Failed getting offset for field")
            };
            let field_ptr = b.push_inst(Inst::StructOffset {
                struct_t: struct_pt_id,
                base: struct_base,
                field_index: field_access.field_index,
                vm_offset: offset,
            });
            let result_type = b.get_physical_type(field_access.result_type);
            let result = build_field_access(
                b,
                field_access.access_kind,
                dst,
                field_ptr.as_value(),
                result_type,
            );
            Ok(result)
        }
        TypedExpr::ArrayGetElement(array_get) => {
            let array_base = compile_expr(b, None, array_get.base)?;
            let element_pt = b.get_physical_type(array_get.array_type);
            let index = compile_expr(b, None, array_get.index)?;

            let element_ptr = b.push_inst(Inst::ArrayOffset {
                element_t: element_pt,
                base: array_base,
                element_index: index,
            });
            let result_type = b.get_physical_type(array_get.result_type);
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
                                let stored = load_or_copy(b, value_pt, dst, address, false);
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
                                let stored = store_rich_if_dst(b, dst, value_pt, address);
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
                        b.k1.ice_with_span("Missing variable", variable_expr.span)
                    };
                    let var_type_pt_id = b.get_physical_type(variable_expr.type_id);
                    let var_value = var.value;
                    if var.indirect {
                        let loaded = load_or_copy(b, var_type_pt_id, dst, var_value, false);
                        Ok(loaded)
                    } else {
                        let stored = store_rich_if_dst(b, dst, var_type_pt_id, var_value);
                        Ok(stored)
                    }
                }
            }
        }
        TypedExpr::Deref(deref) => {
            let src = compile_expr(b, None, deref.target)?;
            let target_pt = b.get_physical_type(deref.type_id);
            let loaded = match dst {
                Some(dst) => b.push_copy(dst, src, target_pt).as_value(),
                None => load_value(b, target_pt, src, true),
            };
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
            let mut args = b.k1.bytecode.mem.new_vec(call.args.len() as u32 + 1);
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

                        IntrinsicOperation::BakeStaticValue => Some(BcBuiltin::BakeStaticValue),
                        IntrinsicOperation::CompilerMessage => Some(BcBuiltin::CompilerMessage),
                        IntrinsicOperation::Zeroed => {
                            return {
                                let type_id = b.k1.named_types.get_nth(call.type_args, 0).type_id;
                                match b.get_physical_type(type_id) {
                                    pt @ PhysicalType::Agg(agg_id) => {
                                        let pt_layout = b.k1.types.phys_types.get(agg_id).layout;
                                        let dst = match dst {
                                            None => b.alloca_type(pt).as_value(),
                                            Some(dst) => dst,
                                        };
                                        let zero_u8 = Value::byte(0);
                                        // intern fn set(dst: Pointer, value: u8, count: uword): unit
                                        let count = Value::Imm32 {
                                            t: ScalarType::I8,
                                            data: pt_layout.size,
                                        };
                                        let memset_args =
                                            b.k1.bytecode.mem.push_slice(&[dst, zero_u8, count]);
                                        let memset_call = BcCall {
                                            dst: None,
                                            ret_inst_kind: InstKind::Void,
                                            callee: BcCallee::Builtin(BcBuiltin::MemSet),
                                            args: memset_args,
                                        };
                                        let call_id = b.k1.bytecode.calls.add(memset_call);
                                        b.push_inst(Inst::Call { id: call_id });
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

                        IntrinsicOperation::TypeName | IntrinsicOperation::TypeSchema => {
                            unreachable!("handled as normal call")
                        }

                        IntrinsicOperation::BoolNegate => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst(Inst::BoolNegate { v: base });
                                let stored = store_simple_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::BitNot => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst(Inst::BitNot { v: base });
                                let stored = store_simple_if_dst(b, dst, neg.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::ArithBinop(op) => {
                            return {
                                let lhs = compile_expr(b, None, call.args[0])?;
                                let rhs = compile_expr(b, None, call.args[1])?;
                                let res = b.push_inst(Inst::ArithBin { op, lhs, rhs });
                                let stored = store_simple_if_dst(b, dst, res.as_value());
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::BitwiseBinop(op) => {
                            return {
                                let lhs = compile_expr(b, None, call.args[0])?;
                                let rhs = compile_expr(b, None, call.args[1])?;
                                let res = b.push_inst(Inst::BitwiseBin { op, lhs, rhs });
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
                                let offset = b.push_inst(Inst::ArrayOffset {
                                    element_t: elem_pt,
                                    base,
                                    element_index,
                                });
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
                        );
                        let fn_ptr = load_value(b, ptr_pt, fn_ptr_addr.as_value(), false);
                        let env_addr = b.push_struct_offset(
                            lam_obj_pt,
                            lambda_obj,
                            TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
                        );
                        let env = load_value(b, ptr_pt, env_addr.as_value(), false);

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
                        b.k1.bytecode
                            .b_units_pending_compile
                            .push(CompilableUnit::Function(*function_id))
                    }
                    _ => {}
                }
            }
            let return_type_id = return_type;
            let return_inst_kind: InstKind = b.type_to_inst_kind(return_type_id);
            for arg in &call.args {
                args.push(compile_expr(b, None, *arg)?);
            }
            let call_dst = match dst {
                None => match return_inst_kind {
                    InstKind::Value(_pt) => {
                        let pt_id = b.get_physical_type(return_type_id);
                        Some(b.alloca_type(pt_id).as_value())
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
            let call_inst_id = b.push_inst(call_inst);
            Ok(call_inst_id.as_value())
        }
        TypedExpr::Match(match_expr) => {
            for stmt in &match_expr.initial_let_statements {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.k1.bytecode.mem.new_vec(match_expr.arms.len() as u32);
            for (arm_index, _arm) in match_expr.arms.iter().enumerate() {
                let name = mformat!(b.k1.bytecode.mem, "arm_{}_cond__{}", arm_index, expr.as_u32());
                let name_cons =
                    mformat!(b.k1.bytecode.mem, "arm_{}_cons__{}", arm_index, expr.as_u32());
                let arm_block = b.push_block(name);
                let arm_consequent_block = b.push_block(name_cons);
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_jump(first_arm_block);

            let fail_name = mformat!(b.k1.bytecode.mem, "match_fail__{}", expr.as_u32());
            let fail_block = b.push_block(fail_name);
            b.goto_block(fail_block);
            b.push_inst(Inst::Unreachable);

            let end_name = mformat!(b.k1.bytecode.mem, "match_end__{}", expr.as_u32());
            let match_end_block = b.push_block(end_name);
            b.goto_block(match_end_block);
            let result_inst_kind = b.type_to_inst_kind(match_expr.result_type);
            let result_came_from = match result_inst_kind {
                InstKind::Value(_) => {
                    let pt = b.get_physical_type(match_expr.result_type);
                    Some(b.push_inst(Inst::CameFrom { t: pt, incomings: MSlice::empty() }))
                }
                InstKind::Void => {
                    eprintln!("expr {}", b.k1.expr_to_string(expr));
                    return failf!(b.cur_span, "come from void");
                }
                InstKind::Terminator => None,
            };

            let mut incomings: SV8<CameFromCase> = smallvec![];
            for ((index, arm), (arm_block, arm_cons_block)) in
                match_expr.arms.iter().enumerate().zip(arm_blocks.iter())
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
                    b.push_jump(match_end_block);
                }
            }
            b.goto_block(match_end_block);
            match result_came_from {
                None => {
                    // match is divergent; we never get here
                    let inst = b.push_inst(Inst::Unreachable);
                    Ok(inst.as_value())
                }
                Some(came_from) => {
                    let real_incomings = b.k1.bytecode.mem.push_slice(&incomings);
                    let Inst::CameFrom { incomings: i, .. } =
                        b.k1.bytecode.instrs.get_mut(came_from)
                    else {
                        unreachable!()
                    };
                    *i = real_incomings;
                    let pt_id = b.get_physical_type(match_expr.result_type);
                    Ok(store_rich_if_dst(b, dst, pt_id, came_from.as_value()))
                }
            }
        }
        TypedExpr::LogicalAnd(and) => {
            let rhs_check_name = mformat!(b.k1.bytecode.mem, "and_rhs__{}", expr.as_u32());
            let rhs_check = b.push_block(rhs_check_name);
            let short_circuit_name = mformat!(b.k1.bytecode.mem, "and_short__{}", expr.as_u32());
            let short_circuit = b.push_block(short_circuit_name);
            let final_block_name = mformat!(b.k1.bytecode.mem, "and_end__{}", expr.as_u32());
            let final_block = b.push_block(final_block_name);

            let mut incomings = b.k1.bytecode.mem.new_vec(2);

            let start_block = b.cur_block;
            let lhs = compile_expr(b, None, and.lhs)?;
            b.push_inst(Inst::JumpIf { cond: lhs, cons: rhs_check, alt: short_circuit });
            incomings.push(CameFromCase { from: start_block, value: Value::FALSE });
            b.push_jump(final_block);

            b.goto_block(rhs_check);
            let rhs = compile_expr(b, None, and.rhs)?;
            let rhs_incoming = b.cur_block;
            incomings.push(CameFromCase { from: rhs_incoming, value: rhs });
            b.push_jump(final_block);

            b.goto_block(final_block);
            let incomings_handle = b.k1.bytecode.mem.vec_to_mslice(&incomings);
            let result_came_from = b.push_inst(Inst::CameFrom {
                t: PhysicalType::Scalar(ScalarType::I8),
                incomings: incomings_handle,
            });
            Ok(result_came_from.as_value())
        }
        TypedExpr::LogicalOr(or) => {
            // Short-circuiting control-flow Or
            let rhs_check_name = mformat!(b.k1.bytecode.mem, "or_rhs__{}", expr.as_u32());
            let rhs_check = b.push_block(rhs_check_name);
            let short_circuit_name = mformat!(b.k1.bytecode.mem, "or_short__{}", expr.as_u32());
            let short_circuit = b.push_block(short_circuit_name);
            let final_block_name = mformat!(b.k1.bytecode.mem, "or_end__{}", expr.as_u32());
            let final_block = b.push_block(final_block_name);

            let mut incomings = b.k1.bytecode.mem.new_vec(2);

            let start_block = b.cur_block;
            let lhs = compile_expr(b, None, or.lhs)?;
            b.push_inst(Inst::JumpIf { cond: lhs, cons: short_circuit, alt: rhs_check });

            incomings.push(CameFromCase { from: start_block, value: Value::TRUE });
            b.push_jump(final_block);

            b.goto_block(rhs_check);
            let rhs = compile_expr(b, None, or.rhs)?;
            let rhs_incoming = b.cur_block;
            incomings.push(CameFromCase { from: rhs_incoming, value: rhs });
            b.push_jump(final_block);

            b.goto_block(final_block);
            let incomings_handle = b.k1.bytecode.mem.vec_to_mslice(&incomings);
            let result_came_from = b
                .push_inst(Inst::CameFrom {
                    t: PhysicalType::Scalar(ScalarType::I8),
                    incomings: incomings_handle,
                })
                .as_value();
            Ok(result_came_from)
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

            b.push_jump(cond_block);

            b.goto_block(cond_block);
            compile_matching_condition(b, &w.condition, loop_body_block, end_block)?;

            b.goto_block(loop_body_block);
            let last = compile_block_stmts(b, None, w.body)?;
            if last.is_some_and(|v| !b.get_value_kind(&v).is_terminator()) {
                b.push_jump(cond_block);
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

            let break_pt_id = b.get_physical_type(loop_expr.break_type);

            let break_value = if loop_expr.break_type != UNIT_TYPE_ID {
                Some(b.alloca_type(break_pt_id))
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
            b.push_jump(loop_body_block);
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if body_value.is_some_and(|v| !b.get_value_kind(&v).is_terminator()) {
                b.push_jump(loop_body_block);
            }

            b.goto_block(loop_end_block);
            if let Some(break_alloca) = break_value {
                let loaded = load_value(b, break_pt_id, break_alloca.as_value(), false);
                Ok(loaded)
            } else {
                Ok(Value::UNIT)
            }
        }
        TypedExpr::Break(brk) => {
            let loop_info = b.k1.bytecode.b_loops.get(&brk.loop_scope).unwrap();
            let end_block = loop_info.end_block;
            if let Some(break_dst) = loop_info.break_value {
                let _stored = compile_expr(b, Some(break_dst.as_value()), brk.value)?;
                let jmp = b.push_jump(end_block);
                Ok(jmp.as_value())
            } else {
                compile_expr(b, None, brk.value)?;
                let jmp = b.push_jump(end_block);
                Ok(jmp.as_value())
            }
        }
        TypedExpr::EnumConstructor(enumc) => {
            let enumc_inst_kind = b.type_to_inst_kind(enumc.variant_type_id);
            if enumc_inst_kind.is_terminator() {
                let payload = enumc.payload.unwrap();
                let crash = compile_expr(b, None, payload)?;
                return Ok(crash);
            }

            let variant_pt = b.get_physical_type(enumc.variant_type_id);
            let enum_base = match dst {
                Some(dst) => dst,
                None => b.alloca_type(variant_pt).as_value(),
            };

            let tag_base = enum_base;
            let enum_variant = b.k1.types.get(enumc.variant_type_id).expect_enum_variant();
            let tag_int_value = enum_variant.tag_value;
            let int_imm = b.push_int_value(&tag_int_value);
            b.push_store(tag_base, int_imm);

            if let Some(payload_expr) = &enumc.payload {
                let payload_offset = b.push_struct_offset(variant_pt.expect_agg(), enum_base, 1);
                let _payload_value =
                    compile_expr(b, Some(payload_offset.as_value()), *payload_expr)?;
            }

            Ok(enum_base)
        }
        TypedExpr::EnumGetTag(e_get_tag) => {
            let enum_base = compile_expr(b, None, e_get_tag.enum_expr_or_reference)?;
            let enum_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.get_expr_type_id(e_get_tag.enum_expr_or_reference))
                    .expect_enum();
            let tag_type = enum_type.tag_type;
            let tag_scalar = b.get_physical_type(tag_type).expect_scalar();

            // Load straight from the enum base, dont bother with a struct gep
            // task(bc): Copy if dst
            let tag = b.push_load(tag_scalar, enum_base).as_value();
            let stored = store_simple_if_dst(b, dst, tag);
            Ok(stored)
        }
        TypedExpr::EnumGetPayload(e_get_payload) => {
            let enum_variant_base = compile_expr(b, None, e_get_payload.enum_variant_expr)?;
            if b.get_value_kind(&enum_variant_base).is_terminator() {
                return Ok(enum_variant_base);
            }

            let variant_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.get_expr_type_id(e_get_payload.enum_variant_expr))
                    .expect_enum_variant();
            let variant_pt = b.get_physical_type(variant_type.my_type_id).expect_agg();
            let variant_payload = variant_type.payload;
            let payload_offset = b.push_struct_offset(variant_pt, enum_variant_base, 1);
            if e_get_payload.access_kind == FieldAccessKind::ReferenceThrough {
                let base_is_reference =
                    b.k1.get_expr_type(e_get_payload.enum_variant_expr).as_reference().is_some();

                debug_assert!(base_is_reference);
                // We're generating a pointer to the payload. The variant itself is a reference
                // and the value we produce here is just a pointer to the payload
                let stored = store_simple_if_dst(b, dst, payload_offset.as_value());
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
                let copied =
                    load_or_copy(b, payload_pt_id, dst, payload_offset.as_value(), make_copy);
                Ok(copied)
            }
        }
        TypedExpr::Cast(c) => compile_cast(b, dst, &c, expr),
        TypedExpr::Return(typed_return) => {
            //task(bc): Return value optimization
            let inst = compile_expr(b, None, typed_return.value)?;
            let ret = b.push_inst(Inst::Ret(inst));
            Ok(ret.as_value())
        }
        TypedExpr::Lambda(lam_expr) => {
            let l = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            let env_struct = l.environment_struct;
            match dst {
                Some(dst) => compile_expr(b, Some(dst), env_struct),
                None => {
                    let env = compile_expr(b, None, env_struct)?;
                    Ok(env)
                }
            }
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = Value::FunctionAddr(fpe.function_id);
            let ptr_pt = b.get_physical_type(POINTER_TYPE_ID);
            let stored = store_rich_if_dst(b, dst, ptr_pt, fp);
            Ok(stored)
        }
        TypedExpr::FunctionToLambdaObject(fn_to_lam_obj) => {
            let obj_struct_type = b.get_physical_type(fn_to_lam_obj.lambda_object_type_id);
            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.alloca_type(obj_struct_type).as_value(),
            };
            let fn_ptr = Value::FunctionAddr(fn_to_lam_obj.function_id);
            let lam_obj_fn_ptr_addr = b
                .push_struct_offset(
                    obj_struct_type.expect_agg(),
                    lam_obj_ptr,
                    TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
                )
                .as_value();
            b.push_store(lam_obj_fn_ptr_addr, fn_ptr);
            let lam_obj_env_ptr_addr = b.push_struct_offset(
                obj_struct_type.expect_agg(),
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_env_ptr_addr.as_value(), Value::PtrZero);
            Ok(lam_obj_ptr)
        }
        TypedExpr::PendingCapture(_) => b.k1.ice_with_span("bc on PendingCapture", b.cur_span),
        TypedExpr::StaticValue(stat) => {
            let t = b.get_physical_type(stat.type_id);
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
                    let imm = b.push_int_value(&int);
                    let store = store_simple_if_dst(b, dst, imm);
                    Ok(store)
                }
                StaticValue::Float(float) => {
                    let float = *float;
                    //task(bc): Pack small floats
                    let imm = b.push_inst(Inst::Imm(Imm::Float(float)));
                    let store = store_simple_if_dst(b, dst, imm.as_value());
                    Ok(store)
                }
                //task(bc) non-trival static lowerings! Zero can probably be our zero impl?
                StaticValue::String(_) => Ok(Value::StaticValue { t, id: stat.value_id }),
                StaticValue::Zero(_) => Ok(Value::StaticValue { t, id: stat.value_id }),
                StaticValue::Struct(_) => Ok(Value::StaticValue { t, id: stat.value_id }),
                StaticValue::Enum(_) => Ok(Value::StaticValue { t, id: stat.value_id }),
                StaticValue::LinearContainer(_) => Ok(Value::StaticValue { t, id: stat.value_id }),
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
        let loaded = load_or_copy(b, result_pt, dst, field_ptr, make_copy);
        loaded
    }
}

#[inline]
fn compile_cast(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<Value>,
    c: &TypedCast,
    _expr_id: TypedExprId,
) -> TyperResult<Value> {
    match c.cast_type {
        CastType::EnumToVariant
        | CastType::VariantToEnum
        | CastType::ReferenceToReference
        | CastType::ReferenceToMut
        | CastType::ReferenceUnMut
        | CastType::IntegerCast(IntegerCastDirection::NoOp)
        | CastType::Integer8ToChar
        | CastType::StaticErase
        | CastType::Transmute
        | CastType::PointerToReference
        | CastType::ReferenceToPointer => {
            let base_noop = compile_expr(b, dst, c.base_expr)?;
            Ok(base_noop)
        }
        CastType::IntegerCast(IntegerCastDirection::Extend)
        | CastType::IntegerCast(IntegerCastDirection::Truncate)
        | CastType::IntegerExtendFromChar => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to_pt = b.get_physical_type(c.target_type_id);
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
            let inst = b.push_inst(inst);
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
            let inst = b.push_inst(inst);
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
            let to = b.get_physical_type(c.target_type_id).expect_scalar();
            let inst = match c.cast_type {
                CastType::FloatExtend => Inst::FloatExt { v: base, to },
                CastType::FloatTruncate => Inst::FloatTrunc { v: base, to },
                CastType::FloatToUnsignedInteger => Inst::FloatToIntUnsigned { v: base, to },
                CastType::FloatToSignedInteger => Inst::FloatToIntSigned { v: base, to },
                CastType::IntegerUnsignedToFloat => Inst::IntToFloatUnsigned { v: base, to },
                CastType::IntegerSignedToFloat => Inst::IntToFloatSigned { v: base, to },
                _ => unreachable!(),
            };
            let inst = b.push_inst(inst);
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
            let obj_struct_type = b.get_physical_type(b.k1.types.builtins.dyn_lambda_obj.unwrap());

            // Now we need a pointer to the environment
            // nocommit(3): dyn lambda durability: Change to arena.push_struct call. This feels like
            //           code that might be able to live in typer.k1
            let lambda_env_ptr = b.alloca_type(lambda_env_type).as_value();

            // Produces just the lambda's environment as a value. We don't need the function
            // pointer because we know it from the type still
            // We store the environment directly into our stack pointer
            let _lambda_env_inst = compile_expr(b, Some(lambda_env_ptr), c.base_expr)?;

            let fn_ptr = Value::FunctionAddr(lambda_function_id);

            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.alloca_type(obj_struct_type).as_value(),
            };
            // Store fn ptr, then env ptr into the object
            let lam_obj_fn_ptr_addr = b.push_struct_offset(
                obj_struct_type.expect_agg(),
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_fn_ptr_addr.as_value(), fn_ptr);

            let lam_obj_env_ptr_addr = b.push_struct_offset(
                obj_struct_type.expect_agg(),
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_env_ptr_addr.as_value(), lambda_env_ptr);

            Ok(lam_obj_ptr)
        }
    }
}

/// Loads a value of a given type from 'src'.
/// 'Load' in this context is an operation internal to this
/// IR; it doesn't have a direct analog in the source language.
/// A Dereference would the closest thing. But we take some liberties here;
/// such as treating this as a no-op for values that are already represented
/// by their location, aka IndirectValues
fn load_value(b: &mut Builder, pt: PhysicalType, src: Value, make_copy: bool) -> Value {
    match pt {
        PhysicalType::Agg(_) => {
            if make_copy {
                let dst = b.alloca_type(pt);
                let pt_layout = b.k1.types.get_pt_layout(&pt);
                b.push_inst(Inst::Copy { dst: dst.as_value(), src, t: pt, vm_size: pt_layout.size })
                    .as_value()
            } else {
                src
            }
        }
        PhysicalType::Scalar(st) => b.push_load(st, src).as_value(),
    }
}

fn store_value(b: &mut Builder, pt: PhysicalType, dst: Value, value: Value) -> InstId {
    match pt {
        PhysicalType::Agg(_) => {
            // Rename to `src` shows that, since we have an aggregate, `value` is a location.
            let src = value;
            b.push_copy(dst, src, pt)
        }
        PhysicalType::Scalar(_) => b.push_store(dst, value),
    }
}

fn load_or_copy(
    b: &mut Builder,
    pt: PhysicalType,
    dst: Option<Value>,
    src: Value,
    copy_aggregates: bool,
) -> Value {
    match dst {
        Some(dst) => b.push_copy(dst, src, pt).as_value(),
        None => load_value(b, pt, src, copy_aggregates),
    }
}

fn compile_matching_condition(
    b: &mut Builder,
    mc: &MatchingCondition,
    cons_block: BlockId,
    condition_fail_block: BlockId,
) -> TyperResult<()> {
    b.cur_span = mc.span;
    if mc.instrs.is_empty() {
        // Always true
        b.push_jump(cons_block);
        return Ok(());
    }
    for (index, inst) in mc.instrs.iter().enumerate() {
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(b, None, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_value: Value = compile_expr(b, None, *value)?;
                if b.k1.get_expr_type_id(*value) == NEVER_TYPE_ID {
                    return Ok(());
                }
                let is_last = index == mc.instrs.len() - 1;
                let continue_block = if is_last { cons_block } else { b.push_block(MStr::empty()) };
                b.push_inst(Inst::JumpIf {
                    cond: cond_value,
                    cons: continue_block,
                    alt: condition_fail_block,
                });
                b.goto_block(continue_block);
            }
        }
    }
    Ok(())
}

pub fn zero(t: ScalarType) -> Value {
    match t {
        ScalarType::I8 => Value::byte(0),
        ScalarType::I16 => Value::Imm32 { t: ScalarType::I16, data: 0 },
        ScalarType::I32 => Value::Imm32 { t: ScalarType::I32, data: 0 },
        ScalarType::I64 => Value::Imm32 { t: ScalarType::I64, data: 0 },
        ScalarType::F32 => Value::Imm32 { t: ScalarType::I64, data: (0.0f32).to_bits() },
        ScalarType::F64 => {
            // Bit pattern is all zeroes anyway
            Value::Imm32 { t: ScalarType::I64, data: (0.0f64).to_bits() as u32 }
        }
        ScalarType::Pointer => Value::PtrZero,
    }
}

////////////////////////////// Validation //////////////////////////////

pub fn validate_function(k1: &TypedProgram, function: FunctionId, errors: &mut Vec<String>) {
    let bc = &k1.bytecode;
    let f = bc.functions.get(function).as_ref().unwrap();
    for (block_index, block) in bc.mem.get_slice(f.blocks).iter().enumerate() {
        for (index, inst_id) in bc.mem.get_slice(block.instrs).iter().enumerate() {
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
                Inst::Imm(_imm) => (),
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

                    if index_type.as_value().and_then(|t| t.as_scalar())
                        != Some(bc.word_sized_int())
                    {
                        errors.push(format!(
                            "i{inst_id}: array_offset index type is not word-sized int",
                        ))
                    }
                }
                Inst::Call { .. } => (),
                Inst::Jump(block) => {
                    if *block >= f.blocks.len() {
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
                Inst::IntExtU { .. } => (),
                Inst::IntExtS { .. } => (),
                Inst::FloatTrunc { .. } => (),
                Inst::FloatExt { .. } => (),
                Inst::FloatToIntSigned { .. } => (),
                Inst::FloatToIntUnsigned { .. } => (),

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
                    if inst_type.as_value().and_then(|t| t.as_scalar()) != Some(bc.word_sized_int())
                    {
                        errors.push(format!("i{inst_id}: word_to_ptr src is not a word-sized int",))
                    }
                }
                Inst::ArithBin { .. } => {
                    //task(bc): Validate arith bins
                }
                Inst::BitwiseBin { .. } => {
                    //task(bc): Validate bitwise bins
                }
            }
        }
    }
}

////////////////////////////// Display //////////////////////////////

pub fn display_function(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: FunctionId,
    show_source: bool,
) -> std::fmt::Result {
    let Some(function) = bc.functions.get(function) else { return Ok(()) };
    for (index, _block) in bc.mem.get_slice(function.blocks).iter().enumerate() {
        let id = index as BlockId;
        display_block(w, k1, bc, function, id, show_source)?;
    }
    Ok(())
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: &CompiledUnit,
    block_id: BlockId,
    show_source: bool,
) -> std::fmt::Result {
    let block = bc.mem.get_nth(function.blocks, block_id as usize);
    write!(w, "b{} ", block_id)?;
    if !block.name.is_empty() {
        w.write_str(bc.mem.get_str(block.name))?;
    }
    writeln!(w)?;
    for inst_id in bc.mem.get_slice(block.instrs).iter() {
        write!(w, "  i{} = ", *inst_id)?;
        display_inst(w, k1, bc, *inst_id, show_source)?;
        writeln!(w)?;
    }
    writeln!(w, "END")?;
    Ok(())
}

pub fn display_inst(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    inst_id: InstId,
    show_source: bool,
) -> std::fmt::Result {
    match bc.instrs.get(inst_id) {
        Inst::Imm(imm) => {
            write!(w, "imm ")?;
            display_imm(w, imm)?;
        }
        Inst::Alloca { t, vm_layout } => {
            write!(w, "alloca ")?;
            display_pt(w, &k1.types, t)?;
            write!(w, ", align {}", vm_layout.align)?;
        }
        Inst::Store { dst, value } => {
            let inst_kind = get_value_kind(bc, &k1.types, value);
            write!(w, "store to {}, ", *dst,)?;
            display_inst_kind(w, &k1.types, &inst_kind)?;
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
            for (index, arg) in bc.mem.get_slice(call.args).iter().enumerate() {
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
            for (i, incoming) in bc.mem.get_slice(*incomings).iter().enumerate() {
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
        Inst::FloatToIntUnsigned { v, to } => {
            write!(w, "ftoint ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::FloatToIntSigned { v, to } => {
            write!(w, "ftoint signed ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntToFloatUnsigned { v, to } => {
            write!(w, "inttofloat ")?;
            display_scalar_type(w, to)?;
            write!(w, " {}", v)?;
        }
        Inst::IntToFloatSigned { v, to } => {
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
        Inst::ArithBin { op, lhs, rhs } => {
            write!(w, "{:?} {} {}", op.op, *lhs, *rhs)?;
        }
        Inst::BitwiseBin { op, lhs, rhs } => {
            write!(w, "{:?} {} {}", op, *lhs, *rhs)?;
        }
    };
    if show_source {
        let span_id = bc.sources.get(inst_id);
        let lines = k1.ast.get_span_content(*span_id);
        let first_line = lines.lines().next().unwrap_or("");
        write!(w, " ;\t\t\t {}", first_line)?;
    }
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

fn display_pt(
    w: &mut impl std::fmt::Write,
    types: &TypePool,
    t: &PhysicalType,
) -> std::fmt::Result {
    match t {
        PhysicalType::Scalar(st) => write!(w, "{}", st),
        PhysicalType::Agg(agg) => match &types.phys_types.get(*agg).agg_type {
            // Important specialization since wrappers are common
            AggType::Struct1(t1) => {
                w.write_str("{ ")?;
                display_pt(w, types, t1)?;
                w.write_str(" }")?;
                Ok(())
            }
            AggType::EnumVariant(evl) => {
                write!(w, "{{ tag({})", evl.tag)?;
                if let Some(payload) = &evl.payload {
                    write!(w, ", ")?;
                    display_pt(w, types, payload)?;
                };
                w.write_str(" }")?;
                Ok(())
            }
            AggType::Struct { fields } => {
                w.write_str("{ ")?;
                for (index, field) in types.mem.get_slice(*fields).iter().enumerate() {
                    display_pt(w, types, &field.field_t)?;
                    let last = index == fields.len() as usize - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(" }")?;
                Ok(())
            }
            AggType::Array { len, element_t: t } => {
                w.write_str("[")?;
                display_pt(w, types, t)?;
                write!(w, " x {}]", *len)?;
                Ok(())
            }
            AggType::Opaque { layout } => {
                write!(w, "opaque {}, align {}", layout.size, layout.align)
            }
        },
    }
}

pub fn display_scalar_type(w: &mut impl Write, scalar: &ScalarType) -> std::fmt::Result {
    match scalar {
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

pub fn display_imm(w: &mut impl Write, imm: &Imm) -> std::fmt::Result {
    match imm {
        Imm::I64(int) => write!(w, "i64 {}", int),
        Imm::Float(float) => write!(w, "float {}", float),
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_value(f, self)
    }
}

fn display_value(w: &mut impl Write, value: &Value) -> std::fmt::Result {
    match value {
        Value::Inst(inst_id) => write!(w, "i{}", inst_id.as_u32()),
        Value::Global { id, .. } => write!(w, "g{}", id.as_u32()),
        Value::StaticValue { id, .. } => write!(w, "static{}", id.as_u32()),
        Value::FunctionAddr(function_id) => write!(w, "f{}", function_id.as_u32()),
        Value::FnParam { index, .. } => write!(w, "p{}", index),
        Value::Imm32 { t, data } => write!(w, "{} {}", t, data),
        Value::PtrZero => write!(w, "ptr0"),
    }
}

fn generate_typename_or_schema_function() {
    // nocommit pull out once working
    // let type_id_arg = f.param_variables[0];
    // let is_type_name = f.intrinsic_type == Some(IntrinsicOperation::TypeName);
    // let return_pt = if is_type_name {
    //     // typeName returns string
    //     b.get_physical_type(STRING_TYPE_ID)
    // } else {
    //     // typeSchema returns TypeSchema
    //     b.get_physical_type(b.k1.types.builtins.types_type_schema.unwrap())
    // };
    // let entry_block = b.cur_block;
    //
    // // nocommit(4) fix this MStr thing when you just want a static string
    // let else_name = b.k1.bytecode.mem.push_str("miss");
    // let else_block = b.push_block(else_name);
    // b.goto_block(else_block);
    //
    // // TODO: We should print what happened and call exit properly
    // b.push_inst(Inst::Unreachable);
    //
    // let finish_name = b.k1.bytecode.mem.push_str("finish");
    // let finish_block = b.push_block(finish_name);
    //
    // // Value and block index
    // let mut cases: Vec<(Value, u32)> = Vec::with_capacity(b.k1.type_schemas.len());
    // if is_type_name {
    //     for (type_id, string_value_id) in
    //         b.k1.type_names
    //             .iter()
    //             .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
    //     {
    //         if b.k1.types.get_contained_type_variable_counts(*type_id).is_abstract()
    //         {
    //             // Skips abstract types; obviously; but should this be an assertion
    //             // instead? Does this happen?
    //             panic!("typename of abstract type happened");
    //             // continue;
    //         }
    //         let arm_block_name =
    //             b.k1.bytecode.mem.push_str(&format!("arm_type_{}", type_id.as_u32()));
    //         let arm_block = b.push_block(arm_block_name);
    //         b.goto_block(arm_block);
    //         let type_id_inst =
    //             b.push_inst(Inst::Imm(Imm::I64(type_id.as_u32() as u64)));
    //         let type_id_value = Value::Inst(type_id_inst);
    //
    //         let string_type = b.get_physical_type(STRING_TYPE_ID);
    //         let global_value =
    //             Value::StaticValue { t: string_type, id: *string_value_id };
    //         store_value();
    //         self.store_k1_value(&return_llvm_type, sret_ptr, value);
    //         self.builder.build_unconditional_branch(finish_block).unwrap();
    //         cases.push((type_id_int_value, arm_block));
    //     }
    // } else {
    //     for (type_id, schema_value_id) in self
    //         .k1
    //         .type_schemas
    //         .iter()
    //         .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
    //     {
    //         if self
    //             .k1
    //             .types
    //             .get_contained_type_variable_counts(*type_id)
    //             .is_abstract()
    //         {
    //             // No point re-ifying types that don't exist at runtime
    //             // like type parameters
    //             continue;
    //         }
    //         let my_block =
    //             self.append_basic_block(&format!("arm_type_{}", type_id.as_u32()));
    //         self.builder.position_at_end(my_block);
    //         let type_id_int_value =
    //             self.ctx.i64_type().const_int(type_id.as_u32() as u64, false);
    //
    //         let value = self.codegen_static_value_as_code(*schema_value_id)?;
    //         self.store_k1_value(&return_llvm_type, sret_ptr, value);
    //         self.builder.build_unconditional_branch(finish_block).unwrap();
    //         cases.push((type_id_int_value, my_block));
    //     }
    // }
    // self.builder.position_at_end(entry_block);
    // let _switch =
    //     self.builder.build_switch(type_id_arg, else_block, &cases).unwrap();
    //
    // self.builder.position_at_end(finish_block);
    // self.builder.build_return(None).unwrap()
}
