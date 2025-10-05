use crate::compiler::WordSize;
use crate::kmem::MHandle;
use crate::parse::{NumericWidth, StringId};
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
use crate::{
    SV8,
    kmem::{self, MSlice, MStr},
    lex::SpanId,
    nz_u32_id,
    pool::VPool,
    typer::{types::*, *},
};
use crate::{ice_span, mformat};
use ahash::HashMapExt;
use fxhash::FxHashMap;
use smallvec::smallvec;
use std::{borrow::Cow, fmt::Write};

pub struct ProgramBytecode {
    pub mem: kmem::Mem<ProgramBytecode>,
    pub instrs: VPool<Inst, InstId>,
    pub sources: VPool<SpanId, InstId>,
    pub comments: VPool<MStr<ProgramBytecode>, InstId>,
    pub functions: VPool<Option<Function>, FunctionId>,
    pub module_config: BcModuleConfig,
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
            module_config: BcModuleConfig { word_size },
        }
    }

    fn word_sized_int(&self) -> BcType {
        match self.module_config.word_size {
            WordSize::W32 => BcType::Scalar(ScalarType::I(NumericWidth::B32)),
            WordSize::W64 => BcType::Scalar(ScalarType::I(NumericWidth::B64)),
        }
    }
}

pub struct Block {
    pub name: MStr<ProgramBytecode>,
    pub instrs: Vec<InstId>,
}

pub struct Function {
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn get_block(&self, id: BlockId) -> &Block {
        &self.blocks[id as usize]
    }
}

type BcResult<T> = Result<T, Cow<'static, str>>;

#[derive(Clone, Copy)]
pub enum Imm {
    Unit,
    Bool(bool),
    I8(u8),
    I16(u16),
    I32(u32),
    I64(u64),
    Float(TypedFloatValue),
    PtrZero,
}

fn imm_from_int_value(int_value: &TypedIntValue) -> Imm {
    match int_value {
        TypedIntValue::U8(i) => Imm::I8(*i),
        TypedIntValue::U16(i) => Imm::I16(*i),
        TypedIntValue::U32(i) => Imm::I32(*i),
        TypedIntValue::U64(i) => Imm::I64(*i),
        TypedIntValue::UWord32(i) => Imm::I32(*i),
        TypedIntValue::UWord64(i) => Imm::I64(*i),
        TypedIntValue::I8(i) => Imm::I8(*i as u8),
        TypedIntValue::I16(i) => Imm::I16(*i as u16),
        TypedIntValue::I32(i) => Imm::I32(*i as u32),
        TypedIntValue::I64(i) => Imm::I64(*i as u64),
        TypedIntValue::IWord32(i) => Imm::I32(*i as u32),
        TypedIntValue::IWord64(i) => Imm::I64(*i as u64),
    }
}

nz_u32_id!(InstId);
pub type BlockId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BcBuiltin {
    Zeroed,

    BoolNegate,
    BitNot,
    ArithBinop(IntrinsicArithOpKind),
    BitwiseBinop(IntrinsicBitwiseBinopKind),
    LogicalAnd,
    LogicalOr,
    PointerIndex,

    // nocommit(2) functions with runtime switches
    //             Codegen this in BC IR
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
}

// Lambdas have been compiled down to just calls and args by now
#[derive(Clone, Copy)]
pub enum BcCallee {
    Builtin(BcBuiltin),
    Direct(FunctionId),
    Indirect(InstId),
}

#[derive(Clone, Copy)]
pub struct ComeFromCase {
    from: BlockId,
    value: InstId,
}

//task(bc): Rename to lofi
pub enum Inst {
    // Data
    Imm(Imm),
    FunctionAddr(FunctionId),
    StaticValue { t: BcType, id: StaticValueId },
    String { id: StringId },
    // Global { t: BcType, id: TypedGlobalId },

    // Memory manipulation
    Alloca { t: BcType },
    Store { dst: InstId, value: InstId },
    Load { t: BcType, src: InstId },
    Copy { dst: InstId, src: InstId, size: u32 },
    StructOffset { struct_t: BcType, base: InstId, field_index: u32 },
    ArrayOffset { element_t: BcType, base: InstId, element_index: InstId },

    Call(InstKind, BcCallee, MSlice<InstId, ProgramBytecode>),

    // Control Flow
    Jump(BlockId),
    JumpIf { cond: InstId, cons: BlockId, alt: BlockId },
    Unreachable,
    // goto considered harmful, but come-from is friend (phi node)
    ComeFrom { t: BcType, incomings: MSlice<ComeFromCase, ProgramBytecode> },
    Ret(InstId),

    // Operations
    BoolNegate { inst: InstId },
    BitNot { inst: InstId },
    IntTrunc { inst: InstId, to: BcType },
    IntExtU { inst: InstId, to: BcType },
    IntExtS { inst: InstId, to: BcType },
    FloatTrunc { inst: InstId, to: BcType },
    FloatExt { inst: InstId, to: BcType },
    FloatToIntUnsigned { inst: InstId, to: BcType },
    FloatToIntSigned { inst: InstId, to: BcType },
    IntToFloatUnsigned { inst: InstId, to: BcType },
    IntToFloatSigned { inst: InstId, to: BcType },
    PtrToWord { inst: InstId },
    WordToPtr { inst: InstId },
    //task(bc): Break these out into their own instructions
    ArithBin { op: IntrinsicArithOpKind, lhs: InstId, rhs: InstId },
    BitwiseBin { op: IntrinsicBitwiseBinopKind, lhs: InstId, rhs: InstId },

    // Platform ops
    Memset { t: BcType, dst: InstId, value: InstId },
}

fn get_inst_kind(bc: &ProgramBytecode, inst_id: InstId) -> InstKind {
    match bc.instrs.get(inst_id) {
        Inst::Imm(imm) => match imm {
            Imm::Unit => InstKind::U8,
            Imm::Bool(_) => InstKind::U8,
            Imm::I8(_) => InstKind::U8,
            Imm::I16(_) => InstKind::U16,
            Imm::I32(_) => InstKind::U32,
            Imm::I64(_) => InstKind::U64,
            Imm::Float(TypedFloatValue::F32(_)) => InstKind::Value(BcType::Scalar(ScalarType::F32)),
            Imm::Float(TypedFloatValue::F64(_)) => InstKind::Value(BcType::Scalar(ScalarType::F64)),
            Imm::PtrZero => InstKind::PTR,
        },
        Inst::FunctionAddr(_) => InstKind::PTR,
        Inst::StaticValue { t, .. } => InstKind::Value(*t),
        Inst::String { .. } => InstKind::PTR,
        Inst::Alloca { .. } => InstKind::PTR,
        Inst::Store { .. } => InstKind::Void,
        Inst::Load { t, .. } => InstKind::Value(*t),
        Inst::Copy { .. } => InstKind::Void,
        Inst::StructOffset { .. } => InstKind::PTR,
        Inst::ArrayOffset { .. } => InstKind::PTR,
        Inst::Call(t, ..) => *t,
        Inst::Jump(_) => InstKind::Terminator,
        Inst::JumpIf { .. } => InstKind::Terminator,
        Inst::Unreachable => InstKind::Terminator,
        Inst::ComeFrom { t, .. } => InstKind::Value(*t),
        Inst::Ret(_) => InstKind::Terminator,
        Inst::BoolNegate { .. } => InstKind::U8,
        Inst::BitNot { inst } => get_inst_kind(bc, *inst),
        Inst::IntTrunc { to, .. } => InstKind::Value(*to),
        Inst::IntExtU { to, .. } => InstKind::Value(*to),
        Inst::IntExtS { to, .. } => InstKind::Value(*to),
        Inst::FloatTrunc { to, .. } => InstKind::Value(*to),
        Inst::FloatExt { to, .. } => InstKind::Value(*to),
        Inst::FloatToIntSigned { to, .. } => InstKind::Value(*to),
        Inst::FloatToIntUnsigned { to, .. } => InstKind::Value(*to),
        Inst::IntToFloatUnsigned { to, .. } => InstKind::Value(*to),
        Inst::IntToFloatSigned { to, .. } => InstKind::Value(*to),
        Inst::PtrToWord { .. } => InstKind::Value(bc.word_sized_int()),
        Inst::WordToPtr { .. } => InstKind::PTR,
        Inst::ArithBin { .. } => todo!(),
        Inst::BitwiseBin { .. } => todo!(),
        Inst::Memset { .. } => InstKind::Void,
    }
}

#[derive(Clone, Copy)]
pub enum BcAgg {
    Struct { fields: MSlice<BcType, ProgramBytecode> },
    Opaque { size: u32, align: u32 },
    Array { element_t: MHandle<BcType, ProgramBytecode>, len: u32 },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    I(NumericWidth),
    F32,
    F64,
    Pointer,
}

impl ScalarType {
    fn zero(&self) -> Imm {
        match self {
            ScalarType::I(w) => match w {
                NumericWidth::B8 => Imm::I8(0),
                NumericWidth::B16 => Imm::I16(0),
                NumericWidth::B32 => Imm::I32(0),
                NumericWidth::B64 => Imm::I64(0),
            },
            ScalarType::F32 => Imm::Float(TypedFloatValue::F32(0.0)),
            ScalarType::F64 => Imm::Float(TypedFloatValue::F64(0.0)),
            ScalarType::Pointer => Imm::PtrZero,
        }
    }
}

#[derive(Clone, Copy)]
pub enum BcType {
    Scalar(ScalarType),
    Agg(BcAgg),
}

impl BcType {
    pub fn is_agg(&self) -> bool {
        matches!(self, BcType::Agg(_))
    }
    pub fn is_scalar(&self) -> bool {
        matches!(self, BcType::Scalar(_))
    }
    #[track_caller]
    pub fn expect_scalar(&self) -> ScalarType {
        match self {
            BcType::Scalar(s) => *s,
            _ => panic!("Expected scalar"),
        }
    }
    pub fn as_scalar(&self) -> Option<ScalarType> {
        match self {
            BcType::Scalar(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub enum InstKind {
    Value(BcType),
    Void,
    Terminator,
}

impl InstKind {
    pub const PTR: InstKind = InstKind::Value(BcType::Scalar(ScalarType::Pointer));
    pub const U8: InstKind = InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B8)));
    pub const U16: InstKind = InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B16)));
    pub const U32: InstKind = InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B32)));
    pub const U64: InstKind = InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B64)));
    fn is_ptr(&self) -> bool {
        matches!(self, InstKind::Value(BcType::Scalar(ScalarType::Pointer)))
    }
    fn is_int(&self) -> bool {
        matches!(self, InstKind::Value(BcType::Scalar(ScalarType::I(_))))
    }
    fn is_byte(&self) -> bool {
        matches!(self, InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B8))))
    }
    fn is_aggregate(&self) -> bool {
        matches!(self, InstKind::Value(BcType::Agg(_)))
    }
    fn is_storage(&self) -> bool {
        self.is_ptr() || self.is_aggregate()
    }
    fn is_value(&self) -> bool {
        matches!(self, InstKind::Value(_))
    }
    #[track_caller]
    fn expect_value(&self) -> BcType {
        match self {
            InstKind::Value(t) => *t,
            _ => panic!("Expected value, got {}", self.kind_str()),
        }
    }
    fn as_value(&self) -> Option<BcType> {
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
            InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B8))) => "i8",
            InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B16))) => "i16",
            InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B32))) => "i32",
            InstKind::Value(BcType::Scalar(ScalarType::I(NumericWidth::B64))) => "i64",
            InstKind::Value(BcType::Scalar(ScalarType::F32)) => "f32",
            InstKind::Value(BcType::Scalar(ScalarType::F64)) => "f64",
            InstKind::Value(BcType::Scalar(ScalarType::Pointer)) => "ptr",
            InstKind::Value(BcType::Agg(_)) => "agg",
            InstKind::Void => "void",
            InstKind::Terminator => "term",
        }
    }
}

fn compile_type(b: &mut Builder, type_id: TypeId) -> Result<InstKind, String> {
    fn ok_value(value: BcType) -> Result<InstKind, String> {
        Ok(InstKind::Value(value))
    }
    fn ok_scalar(scalar: ScalarType) -> Result<InstKind, String> {
        Ok(InstKind::Value(BcType::Scalar(scalar)))
    }
    match b.k1.types.get(type_id) {
        //task(bc): Eventually, Unit should correspond to Void, not a byte. But only once we can
        //successfully lower it to a zero-sized type everywhere; for example a struct member of
        //type unit
        Type::Unit | Type::Char | Type::Bool => Ok(InstKind::U8),

        // Drops signedness since its now encoded in ops
        Type::Integer(i) => ok_scalar(ScalarType::I(i.width())),

        Type::Float(FloatType::F32) => ok_scalar(ScalarType::F32),
        Type::Float(FloatType::F64) => ok_scalar(ScalarType::F64),

        Type::Pointer | Type::Reference(_) | Type::FunctionPointer(_) => {
            ok_scalar(ScalarType::Pointer)
        }
        Type::Array(array) => {
            let InstKind::Value(t) = compile_type(b, array.element_type)? else {
                return Err("Array element is not a value type".into());
            };
            let Some(len) = array.concrete_count else {
                return Err("Array has no concrete length".into());
            };
            let t = b.bc.mem.push_h(t);
            ok_value(BcType::Agg(BcAgg::Array { element_t: t, len: len as u32 }))
        }
        Type::Struct(s) => {
            let mut fields = b.bc.mem.new_vec(s.fields.len());
            for field in b.k1.types.mem.get_slice(s.fields) {
                let field_type = compile_type(b, field.type_id)?;
                let InstKind::Value(field_type) = field_type else {
                    return Err("Struct field is not a value type".into());
                };
                fields.push(field_type);
            }
            ok_value(BcType::Agg(BcAgg::Struct { fields: b.bc.mem.vec_to_mslice(&fields) }))
        }
        Type::Enum(_) => {
            // compile Enums to an opaque array, for now.
            // Unless we want to have 'union' in this IR, which could be _neato_
            let layout = b.k1.types.get_layout(type_id);
            ok_value(BcType::Agg(BcAgg::Opaque { size: layout.size, align: layout.align }))
        }
        Type::EnumVariant(ev) => {
            // Compile as the 2-field struct: tag and payload
            let tag = b.compile_type(ev.tag_value.get_type()).expect_value();
            let payload = if let Some(payload) = ev.payload {
                let payload = compile_type(b, payload)?;
                let InstKind::Value(payload_type) = payload else {
                    return Err("Enum payload is not a value type".into());
                };
                Some(payload_type)
            } else {
                None
            };
            let fields = if let Some(payload) = payload { &[tag, payload][..] } else { &[tag] };
            ok_value(BcType::Agg(BcAgg::Struct { fields: b.bc.mem.push_slice(fields) }))
        }
        Type::Lambda(lam) => compile_type(b, lam.env_type),
        Type::LambdaObject(lam_obj) => compile_type(b, lam_obj.struct_representation),
        Type::Never => Ok(InstKind::Terminator),

        Type::Function(_)
        | Type::Static(_)
        | Type::Generic(_)
        | Type::TypeParameter(_)
        | Type::FunctionTypeParameter(_)
        | Type::InferenceHole(_)
        | Type::Unresolved(_)
        | Type::RecursiveReference(_) => Err("Not a physical type".into()),
    }
}

impl Inst {}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> BcResult<()> {
    let mut bc = k1.bytecode.borrow_mut();

    let f = k1.get_function(function_id);

    let Some(body) = f.body_block else { return Err("no body".into()) };

    //task(bc): Re-use Builder's allocations
    let fn_span = k1.ast.get_span_for_id(f.parsed_id);
    let mut builder = Builder {
        bc: &mut bc,
        k1,
        blocks: vec![],
        variables: vec![],
        loops: FxHashMap::new(),
        cur_block: 0,
        cur_span: fn_span,
    };
    builder.blocks.push(Block { name: builder.bc.mem.push_str("entry"), instrs: vec![] });

    compile_block_stmts(&mut builder, None, body)?;

    *bc.functions.get_mut(function_id) = Some(Function { blocks: builder.blocks });

    Ok(())
}

struct BuilderVariable {
    id: VariableId,
    inst: InstId,
    indirect: bool,
}

#[derive(Clone)]
struct LoopInfo {
    break_value: Option<InstId>,
    end_block: BlockId,
}
struct Builder<'bc, 'k1> {
    // Dependencies
    bc: &'bc mut ProgramBytecode,
    k1: &'k1 TypedProgram,

    // Core data
    blocks: Vec<Block>,

    // Bookkeeping
    variables: Vec<BuilderVariable>,
    loops: FxHashMap<ScopeId, LoopInfo>,
    // lambdas: FxHashMap<TypeId, FunctionId>,
    cur_block: BlockId,
    cur_span: SpanId,
}

impl<'bc, 'k1> Builder<'bc, 'k1> {
    fn push_inst_to(&mut self, block: BlockId, inst: Inst) -> InstId {
        let id = self.bc.instrs.add(inst);
        let ids = self.bc.sources.add(self.cur_span);
        let idc = self.bc.comments.add(MStr::empty());
        debug_assert!(id == ids && id == idc);

        self.blocks[block as usize].instrs.push(id);
        id
    }

    pub fn get_inst_kind(&self, inst: InstId) -> InstKind {
        get_inst_kind(&self.bc, inst)
    }

    pub fn inst_is_terminator(&self, inst: InstId) -> bool {
        get_inst_kind(&self.bc, inst).is_terminator()
    }

    fn compile_type(&mut self, type_id: TypeId) -> InstKind {
        match compile_type(self, type_id) {
            Err(msg) => ice_span!(&self.k1, self.cur_span, "{msg}"),
            Ok(k) => k,
        }
    }

    fn alloca_type(&mut self, type_id: TypeId) -> InstId {
        let t = self.compile_type(type_id).expect_value();
        self.push_inst_to(self.cur_block, Inst::Alloca { t })
    }

    fn push_inst(&mut self, inst: Inst) -> InstId {
        self.push_inst_to(self.cur_block, inst)
    }

    fn push_struct_offset(&mut self, struct_t: BcType, base: InstId, field_index: u32) -> InstId {
        self.push_inst(Inst::StructOffset { struct_t, base, field_index })
    }

    fn push_unit(&mut self) -> InstId {
        self.push_inst(Inst::Imm(Imm::Unit))
    }

    fn push_jump(&mut self, block_id: BlockId) -> InstId {
        self.push_inst(Inst::Jump(block_id))
    }

    fn push_copy(&mut self, dst: InstId, src: InstId, type_id: TypeId) -> InstId {
        let layout = self.k1.types.get_layout(type_id);
        self.push_inst(Inst::Copy { dst, src, size: layout.size })
    }

    fn push_load(&mut self, type_id: TypeId, src: InstId) -> InstId {
        let t = self.compile_type(type_id).expect_value();
        self.push_inst(Inst::Load { t, src })
    }

    fn push_store(&mut self, dst: InstId, value: InstId) -> InstId {
        self.push_inst(Inst::Store { dst, value })
    }

    fn push_block(&mut self, name: MStr<ProgramBytecode>) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block { name, instrs: vec![] });
        id as BlockId
    }

    fn get_block(&self, block_id: BlockId) -> &Block {
        &self.blocks[block_id as usize]
    }

    fn get_inst_mut(&mut self, inst_id: InstId) -> &mut Inst {
        self.bc.instrs.get_mut(inst_id)
    }

    fn ptr_sized_int(&self) -> BcType {
        self.bc.word_sized_int()
    }

    #[track_caller]
    fn goto_block(&mut self, block_id: BlockId) {
        match self.blocks.get(block_id as usize) {
            None => panic!("goto_block on non-existent block: {}", block_id),
            Some(_) => self.cur_block = block_id,
        }
    }

    fn get_variable(&self, variable_id: VariableId) -> Option<&BuilderVariable> {
        self.variables.iter().find(|bv| bv.id == variable_id)
    }
}

fn store_simple_if_dst(b: &mut Builder, dst: Option<InstId>, value: InstId) -> InstId {
    match dst {
        None => value,
        Some(dst) => b.push_store(dst, value),
    }
}

fn store_rich_if_dst(b: &mut Builder, dst: Option<InstId>, t: TypeId, value: InstId) -> InstId {
    match dst {
        None => value,
        Some(dst) => store_value(b, t, dst, value),
    }
}

fn compile_block_stmts(
    b: &mut Builder,
    dst: Option<InstId>,
    body: TypedExprId,
) -> BcResult<InstId> {
    let TypedExpr::Block(body) = b.k1.exprs.get(body) else {
        return Err("body is not a block".into());
    };

    for (index, &stmt) in body.statements.iter().enumerate() {
        let is_last = index == body.statements.len() - 1;
        let stmt_dst = if is_last { dst } else { None };
        compile_stmt(b, stmt_dst, stmt)?;
    }

    if let Some(last_inst) = b.get_block(b.cur_block).instrs.last() {
        Ok(*last_inst)
    } else {
        b.k1.ice("block has no statements", None)
    }
}

fn compile_stmt(b: &mut Builder, dst: Option<InstId>, stmt: TypedStmtId) -> BcResult<()> {
    let prev_span = b.cur_span;
    let stmt_span = b.k1.get_stmt_span(stmt);
    b.cur_span = stmt_span;
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);

    match b.k1.stmts.get(stmt) {
        TypedStmt::Expr(typed_expr_id, _) => {
            compile_expr(b, dst, *typed_expr_id)?;
            Ok(())
        }
        TypedStmt::Let(let_stmt) => {
            let variable_alloca = b.alloca_type(let_stmt.variable_type);

            // value_ptr means a pointer matching the type of the rhs
            // For a referencing let, the original alloca a ptr
            // So we need one for the inner type as well
            let value_ptr = if let_stmt.is_referencing {
                let Type::Reference(reference_type) = b.k1.types.get(let_stmt.variable_type) else {
                    panic!("Expected reference for referencing let");
                };
                let reference_inner_type = reference_type.inner_type;
                let value_alloca = b.alloca_type(reference_inner_type);
                b.push_store(variable_alloca, value_alloca);
                value_alloca
            } else {
                variable_alloca
            };

            // If there's an initializer, store it in value_ptr
            if let Some(init) = let_stmt.initializer {
                compile_expr(b, Some(value_ptr), init)?;
            }
            b.variables.push(BuilderVariable {
                id: let_stmt.variable_id,
                inst: variable_alloca,
                indirect: true,
            });
            Ok(())
        }
        TypedStmt::Assignment(ass) => {
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
                    let variable_inst = builder_variable.inst;
                    let _rhs_stored = compile_expr(b, Some(variable_inst), ass.value)?;
                    Ok(())
                }
                AssignmentKind::Store => {
                    let lhs = compile_expr(b, None, ass.destination)?;
                    let _rhs_stored = compile_expr(b, Some(lhs), ass.value)?;
                    // let value_type = b.k1.get_expr_type_id(ass.value);
                    // b.push_store(value_type, lhs, rhs);
                    Ok(())
                }
            }
        }
        TypedStmt::Require(req) => {
            let continue_name = mformat!(b.bc.mem, "req_cont_{}", stmt.as_u32());
            let require_continue_block = b.push_block(continue_name);
            let else_name = mformat!(b.bc.mem, "req_else_{}", stmt.as_u32());
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

            Ok(())
        }
        TypedStmt::Defer(_) => {
            // These defers are just vestiges of the source; nothing to emit
            Ok(())
        }
    }
}

fn compile_expr(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<InstId>,
    expr: TypedExprId,
) -> BcResult<InstId> {
    let prev_span = b.cur_span;
    b.cur_span = b.k1.exprs.get(expr).get_span();
    let b = &mut scopeguard::guard(b, |b| b.cur_span = prev_span);
    let e = b.k1.exprs.get(expr);
    match e {
        TypedExpr::Unit(_) => {
            let imm = b.push_inst(Inst::Imm(Imm::Unit));
            let store = store_simple_if_dst(b, dst, imm);
            Ok(store)
        }
        TypedExpr::Char(byte, _) => {
            let imm = b.push_inst(Inst::Imm(Imm::I8(*byte)));
            let store = store_simple_if_dst(b, dst, imm);
            Ok(store)
        }
        TypedExpr::Bool(bv, _) => {
            let imm = b.push_inst(Inst::Imm(Imm::Bool(*bv)));
            let store = store_simple_if_dst(b, dst, imm);
            Ok(store)
        }
        TypedExpr::Integer(int) => {
            let imm = imm_from_int_value(&int.value);
            let inst_id = b.push_inst(Inst::Imm(imm));
            let store = store_simple_if_dst(b, dst, inst_id);
            Ok(store)
        }
        TypedExpr::Float(float) => {
            let imm = b.push_inst(Inst::Imm(Imm::Float(float.value)));
            let store = store_simple_if_dst(b, dst, imm);
            Ok(store)
        }
        TypedExpr::String(string_id, _) => {
            let inst = b.push_inst(Inst::String { id: *string_id });
            Ok(inst)
        }
        TypedExpr::Struct(struct_literal) => {
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.alloca_type(struct_literal.type_id),
            };
            for (field_index, field) in struct_literal.fields.iter().enumerate() {
                debug_assert!(b.k1.types.get(struct_literal.type_id).as_struct().is_some());
                let struct_type = compile_type(b, struct_literal.type_id)?.expect_value();
                let struct_offset = b.push_inst(Inst::StructOffset {
                    struct_t: struct_type,
                    base: struct_base,
                    field_index: field_index as u32,
                });
                compile_expr(b, Some(struct_offset), field.expr)?;
            }
            Ok(struct_base)
        }
        TypedExpr::StructFieldAccess(field_access) => {
            let struct_base = compile_expr(b, None, field_access.base)?;
            let struct_type = compile_type(b, field_access.struct_type)?.expect_value();
            let field_ptr = b.push_inst(Inst::StructOffset {
                struct_t: struct_type,
                base: struct_base,
                field_index: field_access.field_index,
            });
            let result = build_field_access(
                b,
                field_access.access_kind,
                dst,
                field_ptr,
                field_access.result_type,
            );
            Ok(result)
        }
        TypedExpr::ArrayGetElement(array_get) => {
            let array_base = compile_expr(b, None, array_get.base)?;
            let InstKind::Value(BcType::Agg(BcAgg::Array { element_t, .. })) =
                b.compile_type(array_get.array_type)
            else {
                return Err("ArrayGetElement on non-array".into());
            };
            let index = compile_expr(b, None, array_get.index)?;

            let element_t = *b.bc.mem.get(element_t);
            let element_ptr = b.push_inst(Inst::ArrayOffset {
                element_t,
                base: array_base,
                element_index: index,
            });
            // Possibly DRY up 3 sites 'kinded possibly-loading access'
            let result = build_field_access(
                b,
                array_get.access_kind,
                dst,
                element_ptr,
                array_get.result_type,
            );
            Ok(result)
        }
        TypedExpr::Variable(variable_expr) => {
            let Some(var) = b.get_variable(variable_expr.variable_id) else {
                b.k1.ice_with_span("no variable", variable_expr.span)
            };
            let var_inst = var.inst;
            let var_value = if var.indirect {
                // task(bc): Eventual Copy here; since its a Load then a Store if there's a dst
                let var_value = load_value(b, variable_expr.type_id, var_inst, false);
                var_value
            } else {
                var.inst
            };
            let store = store_rich_if_dst(b, dst, variable_expr.type_id, var_value);
            Ok(store)
        }
        TypedExpr::Deref(deref) => {
            let src = compile_expr(b, None, deref.target)?;
            let loaded = match dst {
                Some(dst) => b.push_copy(dst, src, deref.type_id),
                None => load_value(b, deref.type_id, src, true),
            };
            Ok(loaded)
        }
        TypedExpr::Block(_) => {
            let last = compile_block_stmts(b, dst, expr)?;
            Ok(last)
        }
        TypedExpr::Call { call_id, return_type, .. } => {
            let call = b.k1.calls.get(*call_id);
            let mut args = b.bc.mem.new_vec(call.args.len() as u32 + 1);
            let builtin = if let Some(function_id) = call.callee.maybe_function_id() {
                if let Some(intrinsic) = b.k1.get_function(function_id).intrinsic_type {
                    match intrinsic {
                        IntrinsicOperation::SizeOf => unreachable!(),
                        IntrinsicOperation::SizeOfStride => unreachable!(),
                        IntrinsicOperation::AlignOf => unreachable!(),
                        IntrinsicOperation::CompilerSourceLocation => unreachable!(),
                        IntrinsicOperation::CompilerMessage => unreachable!(),
                        IntrinsicOperation::BakeStaticValue => unreachable!(),
                        IntrinsicOperation::GetStaticValue => unreachable!(),
                        IntrinsicOperation::StaticTypeToValue => unreachable!(),
                        IntrinsicOperation::TypeId => unreachable!(),

                        IntrinsicOperation::Zeroed => {
                            // do this one, memset for aggregates
                            // zero imm for basics
                            return {
                                let type_id = b.k1.named_types.get_nth(call.type_args, 0).type_id;
                                let bc_type = b.compile_type(type_id).expect_value();
                                let zero = if bc_type.is_agg() {
                                    let dst = match dst {
                                        None => b.alloca_type(type_id),
                                        Some(dst) => dst,
                                    };
                                    let zero_u8 = b.push_inst(Inst::Imm(Imm::I8(0)));
                                    b.push_inst(Inst::Memset { dst, t: bc_type, value: zero_u8 });
                                    dst
                                } else {
                                    let imm = match bc_type {
                                        BcType::Scalar(s) => s.zero(),
                                        BcType::Agg(_) => unreachable!(),
                                    };
                                    let inst = b.push_inst(Inst::Imm(imm));
                                    inst
                                };
                                Ok(zero)
                            };
                        }

                        // do these
                        IntrinsicOperation::TypeName => Some(BcBuiltin::TypeName),
                        IntrinsicOperation::TypeSchema => Some(BcBuiltin::TypeSchema),

                        IntrinsicOperation::BoolNegate => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst(Inst::BoolNegate { inst: base });
                                let stored = store_simple_if_dst(b, dst, neg);
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::BitNot => {
                            return {
                                let base = compile_expr(b, None, call.args[0])?;
                                let neg = b.push_inst(Inst::BitNot { inst: base });
                                let stored = store_simple_if_dst(b, dst, neg);
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::ArithBinop(op) => {
                            return {
                                let lhs = compile_expr(b, None, call.args[0])?;
                                let rhs = compile_expr(b, None, call.args[1])?;
                                let res = b.push_inst(Inst::ArithBin { op, lhs, rhs });
                                let stored = store_simple_if_dst(b, dst, res);
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::BitwiseBinop(op) => {
                            return {
                                let lhs = compile_expr(b, None, call.args[0])?;
                                let rhs = compile_expr(b, None, call.args[1])?;
                                let res = b.push_inst(Inst::BitwiseBin { op, lhs, rhs });
                                let stored = store_simple_if_dst(b, dst, res);
                                Ok(stored)
                            };
                        }
                        IntrinsicOperation::LogicalAnd => todo!(),
                        IntrinsicOperation::LogicalOr => todo!(),
                        IntrinsicOperation::PointerIndex => todo!(),

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
                    Callee::Abstract { .. } => return Err("bc abstract call".into()),
                    Callee::DynamicLambda(dl) => {
                        let lambda_obj = compile_expr(b, None, *dl)?;
                        let lam_obj_type_id = b.k1.types.builtins.dyn_lambda_obj.unwrap();
                        let lam_obj_type = compile_type(b, lam_obj_type_id)?.expect_value();
                        let fn_ptr_addr = b.push_inst(Inst::StructOffset {
                            struct_t: lam_obj_type,
                            base: lambda_obj,
                            field_index: TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
                        });
                        let fn_ptr = load_value(b, POINTER_TYPE_ID, fn_ptr_addr, false);
                        let env_addr = b.push_inst(Inst::StructOffset {
                            struct_t: lam_obj_type,
                            base: lambda_obj,
                            field_index: TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
                        });
                        let env = load_value(b, POINTER_TYPE_ID, env_addr, false);

                        args.push(env);
                        BcCallee::Indirect(fn_ptr)
                    }
                    Callee::DynamicFunction { function_pointer_expr } => {
                        let callee_inst = compile_expr(b, None, *function_pointer_expr)?;
                        BcCallee::Indirect(callee_inst)
                    }
                    Callee::DynamicAbstract { .. } => {
                        return Err("bc abstract call".into());
                    }
                }
            };
            let return_type_id = *return_type;
            let return_type = b.compile_type(return_type_id);
            let call_inst = {
                for arg in &call.args {
                    args.push(compile_expr(b, None, *arg)?);
                }
                Inst::Call(return_type, callee, b.bc.mem.vec_to_mslice(&args))
            };
            let call_inst_id = b.push_inst(call_inst);
            //task(bc): Storing call results directly into DST!
            let stored = store_rich_if_dst(b, dst, return_type_id, call_inst_id);
            Ok(stored)
        }
        TypedExpr::Match(match_expr) => {
            for stmt in &match_expr.initial_let_statements {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.bc.mem.new_vec(match_expr.arms.len() as u32);
            for (arm_index, _arm) in match_expr.arms.iter().enumerate() {
                let name = mformat!(b.bc.mem, "arm_{}_cond__{}", arm_index, expr.as_u32());
                let name_cons = mformat!(b.bc.mem, "arm_{}_cons__{}", arm_index, expr.as_u32());
                let arm_block = b.push_block(name);
                let arm_consequent_block = b.push_block(name_cons);
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_jump(first_arm_block);

            let fail_name = mformat!(b.bc.mem, "match_fail__{}", expr.as_u32());
            let fail_block = b.push_block(fail_name);
            b.goto_block(fail_block);
            b.push_inst(Inst::Unreachable);

            let end_name = mformat!(b.bc.mem, "match_end__{}", expr.as_u32());
            let match_end_block = b.push_block(end_name);
            b.goto_block(match_end_block);
            let result_type = b.compile_type(match_expr.result_type);
            let result_come_from = match result_type {
                InstKind::Value(physical_type) => Some(
                    b.push_inst(Inst::ComeFrom { t: physical_type, incomings: MSlice::empty() }),
                ),
                InstKind::Void => return Err("come from void".into()),
                InstKind::Terminator => None,
            };

            let mut incomings: SV8<ComeFromCase> = smallvec![];
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
                if !b.get_inst_kind(result).is_terminator() {
                    let current_block = b.cur_block;
                    incomings.push(ComeFromCase { from: current_block, value: result });
                    b.push_jump(match_end_block);
                }
            }
            b.goto_block(match_end_block);
            match result_come_from {
                None => {
                    // match is divergent; we never get here
                    let inst = b.push_inst(Inst::Unreachable);
                    Ok(inst)
                }
                Some(come_from) => {
                    let real_incomings = b.bc.mem.push_slice(&incomings);
                    let Inst::ComeFrom { incomings: i, .. } = b.bc.instrs.get_mut(come_from) else {
                        unreachable!()
                    };
                    *i = real_incomings;
                    Ok(store_rich_if_dst(b, dst, match_expr.result_type, come_from))
                }
            }
        }
        TypedExpr::WhileLoop(w) => {
            let cond_name = mformat!(b.bc.mem, "while_cond__{}", expr.as_u32());
            let cond_block = b.push_block(cond_name);
            let body_name = mformat!(b.bc.mem, "while_body__{}", expr.as_u32());
            let loop_body_block = b.push_block(body_name);
            let end_name = mformat!(b.bc.mem, "while_end__{}", expr.as_u32());
            let end_block = b.push_block(end_name);
            let TypedExpr::Block(body_block) = b.k1.exprs.get(w.body) else { unreachable!() };
            let loop_scope_id = body_block.scope_id;
            b.loops.insert(loop_scope_id, LoopInfo { break_value: None, end_block });

            b.push_jump(cond_block);

            b.goto_block(cond_block);
            compile_matching_condition(b, &w.condition, loop_body_block, end_block)?;

            b.goto_block(loop_body_block);
            let last = compile_block_stmts(b, None, w.body)?;
            if !b.get_inst_kind(last).is_terminator() {
                b.push_jump(cond_block);
            }

            b.goto_block(end_block);
            let unit = b.push_inst(Inst::Imm(Imm::Unit));
            Ok(unit)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            // let start_block = self.builder.get_insert_block().unwrap();
            // let current_fn = start_block.get_parent().unwrap();
            let body_name = mformat!(b.bc.mem, "loop_body__{}", expr.as_u32());
            let loop_body_block = b.push_block(body_name);
            let end_name = mformat!(b.bc.mem, "loop_end__{}", expr.as_u32());
            let loop_end_block = b.push_block(end_name);

            let break_value = if loop_expr.break_type != UNIT_TYPE_ID {
                Some(b.alloca_type(loop_expr.break_type))
            } else {
                None
            };
            let TypedExpr::Block(body_block) = b.k1.exprs.get(loop_expr.body_block) else {
                unreachable!()
            };
            b.loops
                .insert(body_block.scope_id, LoopInfo { break_value, end_block: loop_end_block });

            // Go to the body
            b.push_jump(loop_body_block);
            b.goto_block(loop_body_block);
            let body_value = compile_block_stmts(b, None, loop_expr.body_block)?;
            if !b.get_inst_kind(body_value).is_terminator() {
                b.push_jump(loop_body_block);
            }

            b.goto_block(loop_end_block);
            if let Some(break_alloca) = break_value {
                let loaded = load_value(b, loop_expr.break_type, break_alloca, false);
                Ok(loaded)
            } else {
                Ok(b.push_unit())
            }
        }
        TypedExpr::Break(brk) => {
            let loop_info = b.loops.get(&brk.loop_scope).unwrap();
            let end_block = loop_info.end_block;
            if let Some(break_dst) = loop_info.break_value {
                let _stored = compile_expr(b, Some(break_dst), brk.value)?;
                let jmp = b.push_jump(end_block);
                Ok(jmp)
            } else {
                compile_expr(b, None, brk.value)?;
                let jmp = b.push_jump(end_block);
                Ok(jmp)
            }
        }
        TypedExpr::EnumConstructor(enumc) => {
            let enum_base = match dst {
                Some(dst) => dst,
                None => b.alloca_type(enumc.variant_type_id),
            };

            let tag_base = enum_base;
            let enum_variant = b.k1.types.get(enumc.variant_type_id).expect_enum_variant();
            let tag_int_value = enum_variant.tag_value;
            let int_imm = b.push_inst(Inst::Imm(imm_from_int_value(&tag_int_value)));
            b.push_store(tag_base, int_imm);

            if let Some(payload_expr) = &enumc.payload {
                let variant_struct_type = compile_type(b, enum_variant.my_type_id)?.expect_value();
                let payload_offset = b.push_inst(Inst::StructOffset {
                    struct_t: variant_struct_type,
                    base: enum_base,
                    field_index: 1,
                });
                let _payload_value = compile_expr(b, Some(payload_offset), *payload_expr)?;
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
            // Load straight from the enum base, dont bother with a struct gep
            let tag = b.push_load(tag_type, enum_base);
            let stored = store_simple_if_dst(b, dst, tag);
            Ok(stored)
        }
        TypedExpr::EnumGetPayload(e_get_payload) => {
            let enum_variant_base = compile_expr(b, None, e_get_payload.enum_variant_expr)?;
            let variant_type =
                b.k1.types
                    .get_type_dereferenced(b.k1.get_expr_type_id(e_get_payload.enum_variant_expr))
                    .expect_enum_variant();
            let variant_struct_type = compile_type(b, variant_type.my_type_id)?.expect_value();
            let payload_offset = b.push_struct_offset(variant_struct_type, enum_variant_base, 1);
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
                let payload_type_id = variant_type.payload.unwrap();
                let copied = load_or_copy(b, payload_type_id, dst, payload_offset, make_copy);
                Ok(copied)
            }
        }
        TypedExpr::Cast(_) => compile_cast(b, dst, expr),
        TypedExpr::Return(typed_return) => {
            //task(bc): Track an optional 'ret ptr' for the function; compile straight into it
            let inst = compile_expr(b, None, typed_return.value)?;
            let ret = b.push_inst(Inst::Ret(inst));
            Ok(ret)
        }
        TypedExpr::Lambda(lam_expr) => {
            let l = b.k1.types.get(lam_expr.lambda_type).as_lambda().unwrap();
            match dst {
                Some(dst) => compile_expr(b, Some(dst), l.environment_struct),
                None => {
                    let env = compile_expr(b, None, l.environment_struct)?;
                    Ok(env)
                }
            }
        }
        TypedExpr::FunctionPointer(fpe) => {
            let fp = b.push_inst(Inst::FunctionAddr(fpe.function_id));
            let stored = store_rich_if_dst(b, dst, fpe.function_pointer_type, fp);
            Ok(stored)
        }
        TypedExpr::FunctionToLambdaObject(fn_to_lam_obj) => {
            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.alloca_type(fn_to_lam_obj.lambda_object_type_id),
            };
            let fn_ptr = b.push_inst(Inst::FunctionAddr(fn_to_lam_obj.function_id));
            let obj_struct_type =
                b.compile_type(fn_to_lam_obj.lambda_object_type_id).expect_value();
            let lam_obj_fn_ptr_addr = b.push_struct_offset(
                obj_struct_type,
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_fn_ptr_addr, fn_ptr);
            let lam_obj_env_ptr_addr = b.push_struct_offset(
                obj_struct_type,
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_ENV_PTR_INDEX as u32,
            );
            //task(bc): Cleaner ptr-sized-int OR a notion of null that isn't zero
            let zero = b.push_inst(Inst::Imm(Imm::I64(0)));
            b.push_store(lam_obj_env_ptr_addr, zero);
            Ok(lam_obj_ptr)
        }
        TypedExpr::PendingCapture(_) => b.k1.ice_with_span("bc on PendingCapture", b.cur_span),
        TypedExpr::StaticValue(id, type_id, _) => {
            let t = b.compile_type(*type_id).expect_value();
            Ok(b.push_inst(Inst::StaticValue { t, id: *id }))
        }
    }
}

fn build_field_access(
    b: &mut Builder,
    access_kind: FieldAccessKind,
    dst: Option<InstId>,
    field_ptr: InstId,
    result_type_id: TypeId,
) -> InstId {
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
        let loaded = load_or_copy(b, result_type_id, dst, field_ptr, make_copy);
        loaded
    }
}

#[inline]
fn compile_cast(
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<InstId>,
    expr: TypedExprId,
) -> BcResult<InstId> {
    let TypedExpr::Cast(c) = b.k1.exprs.get(expr) else { unreachable!() };
    eprintln!("bc cast {}", c.cast_type);
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
            let to = b.compile_type(c.target_type_id).expect_value();
            let inst = match c.cast_type {
                CastType::IntegerCast(IntegerCastDirection::Extend)
                | CastType::IntegerExtendFromChar => {
                    let signed = if matches!(c.cast_type, CastType::IntegerExtendFromChar) {
                        false
                    } else {
                        b.k1.get_expr_type(c.base_expr).as_integer().unwrap().is_signed()
                    };
                    if signed {
                        Inst::IntExtS { inst: base, to }
                    } else {
                        Inst::IntExtU { inst: base, to }
                    }
                }
                CastType::IntegerCast(IntegerCastDirection::Truncate) => {
                    Inst::IntTrunc { inst: base, to }
                }
                _ => unreachable!(),
            };
            let inst = b.push_inst(inst);
            let stored = store_simple_if_dst(b, dst, inst);
            Ok(stored)
        }
        CastType::PointerToWord | CastType::WordToPointer => {
            let base = compile_expr(b, None, c.base_expr)?;
            let inst = match c.cast_type {
                CastType::PointerToWord => Inst::PtrToWord { inst: base },
                CastType::WordToPointer => Inst::WordToPtr { inst: base },
                _ => unreachable!(),
            };
            let inst = b.push_inst(inst);
            let stored = store_simple_if_dst(b, dst, inst);
            Ok(stored)
        }
        CastType::FloatExtend
        | CastType::FloatTruncate
        | CastType::FloatToUnsignedInteger
        | CastType::FloatToSignedInteger
        | CastType::IntegerUnsignedToFloat
        | CastType::IntegerSignedToFloat => {
            let base = compile_expr(b, None, c.base_expr)?;
            let to = b.compile_type(c.target_type_id).expect_value();
            let inst = match c.cast_type {
                CastType::FloatExtend => Inst::FloatExt { inst: base, to },
                CastType::FloatTruncate => Inst::FloatTrunc { inst: base, to },
                CastType::FloatToUnsignedInteger => Inst::FloatToIntUnsigned { inst: base, to },
                CastType::FloatToSignedInteger => Inst::FloatToIntSigned { inst: base, to },
                CastType::IntegerUnsignedToFloat => Inst::IntToFloatUnsigned { inst: base, to },
                CastType::IntegerSignedToFloat => Inst::IntToFloatSigned { inst: base, to },
                _ => unreachable!(),
            };
            let inst = b.push_inst(inst);
            let stored = store_simple_if_dst(b, dst, inst);
            Ok(stored)
        }
        CastType::LambdaToLambdaObject => {
            // Produces just the lambda's environment as a value. We don't need the function
            // pointer because we know it from the type still

            let lambda_env_inst = compile_expr(b, None, c.base_expr)?;
            let lambda_type = b.k1.get_expr_type(c.base_expr).as_lambda().unwrap();

            // Now we need a pointer to the environment
            let lambda_env_ptr = b.alloca_type(lambda_type.env_type);
            //task(bc): nocommit(2) This conversion is kinda stinky because we could instead
            //just say that lambdas are POINTERS to their env structs, rather than their
            //env structs themselves. Its no less efficient to generate, because we currently
            //do a struct literal, which requires an alloca anyway, but we'd simply treat
            //it as a reference now while doing the same amount of actual work to build it
            store_value(b, lambda_type.env_type, lambda_env_ptr, lambda_env_inst);

            let lambda_function_id = lambda_type.function_id;
            let fn_ptr = b.push_inst(Inst::FunctionAddr(lambda_function_id));

            let lam_obj_ptr = match dst {
                Some(dst) => dst,
                None => b.alloca_type(b.k1.types.builtins.dyn_lambda_obj.unwrap()),
            };
            // Store fn ptr, then env ptr into the object
            let obj_struct_type =
                b.compile_type(b.k1.types.builtins.dyn_lambda_obj.unwrap()).expect_value();
            let lam_obj_fn_ptr_addr = b.push_struct_offset(
                obj_struct_type,
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_fn_ptr_addr, fn_ptr);
            let lam_obj_env_ptr_addr = b.push_struct_offset(
                obj_struct_type,
                lam_obj_ptr,
                TypePool::LAMBDA_OBJECT_FN_PTR_INDEX as u32,
            );
            b.push_store(lam_obj_env_ptr_addr, lambda_env_ptr);

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
fn load_value(b: &mut Builder, type_id: TypeId, src: InstId, make_copy: bool) -> InstId {
    let kind = b.compile_type(type_id);
    match kind.is_aggregate() {
        true => {
            if make_copy {
                let layout = b.k1.types.get_layout(type_id);
                let dst = b.alloca_type(type_id);
                b.push_inst(Inst::Copy { dst, src, size: layout.size })
            } else {
                src
            }
        }
        false => b.push_load(type_id, src),
    }
}

fn store_value(b: &mut Builder, type_id: TypeId, dst: InstId, src: InstId) -> InstId {
    let kind = b.compile_type(type_id);
    match kind.is_aggregate() {
        true => {
            let layout = b.k1.types.get_layout(type_id);
            b.push_inst(Inst::Copy { dst, src, size: layout.size })
        }
        false => b.push_store(dst, src),
    }
}

fn load_or_copy(
    b: &mut Builder,
    type_id: TypeId,
    dst: Option<InstId>,
    src: InstId,
    copy_aggregates: bool,
) -> InstId {
    match dst {
        Some(dst) => b.push_copy(dst, src, type_id),
        None => load_value(b, type_id, src, copy_aggregates),
    }
}

fn compile_matching_condition(
    b: &mut Builder,
    mc: &MatchingCondition,
    cons_block: BlockId,
    condition_fail_block: BlockId,
) -> BcResult<()> {
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
                let cond_value: InstId = compile_expr(b, None, *value)?;
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

////////////////////////////// Validation //////////////////////////////

pub fn validate_function(k1: &TypedProgram, function: FunctionId, errors: &mut Vec<String>) {
    let bc = k1.bytecode.borrow();
    let f = bc.functions.get(function).as_ref().unwrap();
    for (block_index, block) in f.blocks.iter().enumerate() {
        for (index, inst_id) in block.instrs.iter().enumerate() {
            let is_last = index == block.instrs.len() - 1;
            let inst = bc.instrs.get(*inst_id);
            let inst_kind = get_inst_kind(&bc, *inst_id);
            if !is_last && inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: stray terminator"))
            };
            if is_last && !inst_kind.is_terminator() {
                errors.push(format!("b{block_index}: unterminated"))
            }

            match inst {
                Inst::Imm(_imm) => (),
                Inst::FunctionAddr(_) => (),
                Inst::StaticValue { .. } => (),
                Inst::String { .. } => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, .. } => {
                    let dst_type = get_inst_kind(&bc, *dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("store dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::Load { t, src } => {
                    let src_kind = get_inst_kind(&bc, *src);
                    if !src_kind.is_storage() {
                        errors.push(format!("load src v{} is not storage", *src))
                    }
                    if t.is_agg() {
                        errors
                            .push(format!("cannot load an aggregate to an SSA value: {}", *inst_id))
                    }
                }
                Inst::Copy { dst, src, .. } => {
                    let src_type = get_inst_kind(&bc, *src);
                    if !src_type.is_storage() {
                        errors.push(format!("copy src v{} is not a ptr", *src))
                    }
                    let dst_type = get_inst_kind(&bc, *dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("copy dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = get_inst_kind(&bc, *base);
                    if !base_type.is_storage() {
                        errors.push(format!("struct_offset base v{} is not a ptr", *base))
                    }
                }
                Inst::ArrayOffset { base, element_index, .. } => {
                    let base_type = get_inst_kind(&bc, *base);
                    let index_type = get_inst_kind(&bc, *element_index);
                    if !base_type.is_storage() {
                        errors.push(format!("array_offset base v{} is not a ptr", *base))
                    }

                    if index_type.as_value().and_then(|t| t.as_scalar())
                        != bc.word_sized_int().as_scalar()
                    {
                        errors.push(format!(
                            "array_offset index type is not word-sized int: v{}",
                            *element_index
                        ))
                    }
                }
                Inst::Call(_, _, _) => (),
                Inst::Jump(block) => {
                    if f.blocks.get(*block as usize).is_none() {
                        errors.push(format!("jump to non-existent block b{}", *block))
                    }
                }
                Inst::JumpIf { cond, .. } => {
                    let cond_type = get_inst_kind(&bc, *cond);
                    if !cond_type.is_value() {
                        errors.push(format!("jumpif cond v{} is not a value", *inst_id))
                    }
                }
                Inst::Unreachable => (),
                Inst::ComeFrom { .. } => (),
                Inst::Ret(inst_id) => {
                    let ret_val_type = get_inst_kind(&bc, *inst_id);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("ret value v{} is not a value", *inst_id))
                    }
                }
                Inst::BoolNegate { inst } => {
                    let inst_type = get_inst_kind(&bc, *inst);
                    if !inst_type.is_byte() {
                        errors.push(format!("bool_negate src v{} is not a bool", *inst))
                    }
                }
                Inst::BitNot { inst } => {
                    let inst_type = get_inst_kind(&bc, *inst);
                    if !inst_type.is_int() {
                        errors.push(format!("bit_not src v{} is not an int", *inst))
                    }
                }
                Inst::IntTrunc { to, .. } => {
                    if !matches!(to, BcType::Scalar(ScalarType::I(_))) {
                        errors.push(format!("int trunc to non-int type"))
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
                Inst::PtrToWord { inst } => {
                    let inst_type = get_inst_kind(&bc, *inst);
                    if !inst_type.is_storage() {
                        errors.push(format!("ptr_to_word src v{} is not a ptr", *inst))
                    }
                }
                Inst::WordToPtr { inst } => {
                    let inst_type = get_inst_kind(&bc, *inst);
                    if inst_type.as_value().and_then(|t| t.as_scalar())
                        != bc.word_sized_int().as_scalar()
                    {
                        errors.push(format!("word_to_ptr src v{} is not a word-sized int", *inst))
                    }
                }
                Inst::ArithBin { .. } => {
                    //task(bc): Validate arith bins
                }
                Inst::BitwiseBin { .. } => {
                    //task(bc): Validate bitwise bins
                }
                Inst::Memset { dst, t, value } => {
                    let dst_type = get_inst_kind(&bc, *dst);
                    if !dst_type.is_storage() {
                        errors.push(format!("memset dst v{} is not a ptr", *dst))
                    }
                    if !t.is_agg() {
                        errors.push(format!("memset type is not an aggregate"))
                    }
                    let value_type = get_inst_kind(&bc, *value);
                    if !value_type.is_byte() {
                        errors.push(format!("memset value v{} is not a byte", *value))
                    }
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
    for (index, _block) in function.blocks.iter().enumerate() {
        let id = index as BlockId;
        display_block(w, k1, bc, function, id, show_source)?;
    }
    Ok(())
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: &Function,
    block_id: BlockId,
    show_source: bool,
) -> std::fmt::Result {
    let block = function.get_block(block_id);
    write!(w, "b{} ", block_id)?;
    if !block.name.is_empty() {
        w.write_str(bc.mem.get_str(block.name))?;
    }
    writeln!(w)?;
    for inst_id in block.instrs.iter() {
        write!(w, "  v{} = ", *inst_id)?;
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
        Inst::FunctionAddr(fn_id) => {
            write!(w, "function {}", fn_id.as_u32())?;
        }
        Inst::StaticValue { id, .. } => {
            write!(w, "static ")?;
            k1.display_static_value(w, *id)?;
        }
        Inst::String { id } => {
            write!(w, "string {}", *id)?;
        }
        Inst::Alloca { t } => {
            write!(w, "alloca ")?;
            display_bc_type(w, bc, t)?;
        }
        Inst::Store { dst, value } => {
            let inst_kind = get_inst_kind(&bc, *value);
            write!(w, "store to v{}, ", *dst,)?;
            display_inst_kind(w, bc, &inst_kind)?;
            write!(w, " v{}", *value)?;
        }
        Inst::Load { t, src } => {
            write!(w, "load ")?;
            display_bc_type(w, bc, t)?;
            write!(w, " from v{}", *src)?;
        }
        Inst::Copy { dst, src, size } => {
            write!(w, "copy {} v{}, src v{}", *size, *dst, *src)?;
        }
        Inst::StructOffset { struct_t, base, field_index } => {
            write!(w, "struct_offset ")?;
            display_bc_type(w, bc, struct_t)?;
            write!(w, ".{}, v{}", *field_index, *base)?;
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            write!(w, "array_offset ")?;
            display_bc_type(w, bc, element_t)?;
            write!(w, " v{}[v{}]", *base, *element_index)?;
        }
        Inst::Call(t, callee, args) => {
            write!(w, "call ")?;
            display_inst_kind(w, bc, t)?;
            match callee {
                BcCallee::Builtin(intrinsic_operation) => {
                    write!(w, " builtin {:?}", intrinsic_operation)?;
                }
                BcCallee::Direct(function_id) => {
                    write!(w, " ")?;
                    w.write_str(k1.ident_str(k1.get_function(*function_id).name))?;
                }
                BcCallee::Indirect(callee_inst) => {
                    write!(w, " indirect v{}", *callee_inst)?;
                }
            };
            w.write_str("(")?;
            for (index, arg) in bc.mem.get_slice(*args).iter().enumerate() {
                write!(w, "v{}", *arg)?;
                let last = index == args.len() as usize - 1;
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
            write!(w, "jmpif v{}, b{}, b{}", *cond, *cons, *alt)?;
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
        }
        Inst::ComeFrom { t, incomings } => {
            write!(w, "comefrom ")?;
            display_bc_type(w, bc, t)?;
            write!(w, " [")?;
            for (i, incoming) in bc.mem.get_slice(*incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "(b{}: v{})", incoming.from, incoming.value)?;
            }
            write!(w, "]")?;
        }
        Inst::Ret(value) => {
            write!(w, "ret v{}", *value)?;
        }
        Inst::BoolNegate { inst } => {
            write!(w, "bool not v{}", *inst)?;
        }
        Inst::BitNot { inst } => {
            write!(w, "bitnot v{}", *inst)?;
        }
        Inst::IntTrunc { inst, to } => {
            write!(w, "trunc ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::IntExtU { inst, to } => {
            write!(w, "int extend ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::IntExtS { inst, to } => {
            write!(w, "int extend signed ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::FloatTrunc { inst, to } => {
            write!(w, "ftrunc ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::FloatExt { inst, to } => {
            write!(w, "fext ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::FloatToIntUnsigned { inst, to } => {
            write!(w, "ftoint ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::FloatToIntSigned { inst, to } => {
            write!(w, "ftoint signed ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::IntToFloatUnsigned { inst, to } => {
            write!(w, "inttofloat ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::IntToFloatSigned { inst, to } => {
            write!(w, "inttofloat signed ")?;
            display_bc_type(w, bc, to)?;
            write!(w, " v{}", *inst)?;
        }
        Inst::PtrToWord { inst } => {
            write!(w, "ptrtoint v{}", *inst)?;
        }
        Inst::WordToPtr { inst } => {
            write!(w, "inttoptr v{}", *inst)?;
        }
        Inst::ArithBin { .. } => todo!(),
        Inst::BitwiseBin { .. } => todo!(),
        Inst::Memset { t, dst, value } => {
            write!(w, "memset ")?;
            display_bc_type(w, bc, t)?;
            write!(w, ", v{}, v{}", *dst, *value)?;
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

fn display_inst_kind(
    w: &mut impl std::fmt::Write,
    bc: &ProgramBytecode,
    kind: &InstKind,
) -> std::fmt::Result {
    match kind {
        InstKind::Value(t) => display_bc_type(w, bc, t),
        InstKind::Void => write!(w, "void"),
        InstKind::Terminator => write!(w, "term"),
    }
}

fn display_bc_type(
    w: &mut impl std::fmt::Write,
    bc: &ProgramBytecode,
    t: &BcType,
) -> std::fmt::Result {
    match t {
        BcType::Scalar(ScalarType::I(width)) => write!(w, "i{}", width.bits()),
        BcType::Scalar(ScalarType::F32) => write!(w, "f32"),
        BcType::Scalar(ScalarType::F64) => write!(w, "f64"),
        BcType::Scalar(ScalarType::Pointer) => write!(w, "ptr"),
        BcType::Agg(agg) => match agg {
            BcAgg::Struct { fields } => {
                w.write_str("{ ")?;
                for (index, field_type) in bc.mem.get_slice(*fields).iter().enumerate() {
                    display_bc_type(w, bc, field_type)?;
                    let last = index == fields.len() as usize - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(" }")?;
                Ok(())
            }
            BcAgg::Array { len, element_t: t } => {
                w.write_str("[")?;
                display_bc_type(w, bc, bc.mem.get(*t))?;
                write!(w, " x {}]", *len)?;
                Ok(())
            }
            BcAgg::Opaque { size, align } => write!(w, "opaque {}, align {}", *size, *align),
        },
    }
}

pub fn display_imm(w: &mut impl Write, imm: &Imm) -> std::fmt::Result {
    match imm {
        Imm::Unit => write!(w, "unit"),
        Imm::Bool(b) => write!(w, "bool {}", b),
        Imm::I8(int) => write!(w, "i8 {}", int),
        Imm::I16(int) => write!(w, "i16 {}", int),
        Imm::I32(int) => write!(w, "i32 {}", int),
        Imm::I64(int) => write!(w, "i64 {}", int),
        Imm::Float(float) => write!(w, "float {}", float),
        Imm::PtrZero => write!(w, "ptr zero"),
    }
}
