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
    parse::StringId,
    pool::VPool,
    typer::{types::*, *},
};
use crate::{ice_span, mformat};
use ahash::HashMapExt;
use fxhash::FxHashMap;
use smallvec::smallvec;
use std::{borrow::Cow, fmt::Write};

pub struct ProgramBytecode {
    pub mem: kmem::Mem,
    pub instrs: VPool<Inst, InstId>,
    pub sources: VPool<SpanId, InstId>,
    pub comments: VPool<MStr, InstId>,
    pub functions: VPool<Option<Function>, FunctionId>,
}

impl ProgramBytecode {
    pub fn make(instr_count_hint: usize) -> Self {
        let function_count_hint = instr_count_hint / 128;
        ProgramBytecode {
            mem: kmem::Mem::make(),
            instrs: VPool::make_with_hint("bytecode_soa_instrs", instr_count_hint),
            sources: VPool::make_with_hint("bytecode_soa_sources", instr_count_hint),
            comments: VPool::make_with_hint("bytecode_soa_comments", instr_count_hint),
            functions: VPool::make_with_hint("bytecode_functions", function_count_hint),
        }
    }
}

pub struct Block {
    pub name: MStr,
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
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
}

nz_u32_id!(InstId);
pub type BlockId = u32;

// Lambdas have been compiled down to just calls and args by now
#[derive(Clone, Copy)]
pub enum BcCallee {
    Builtin(IntrinsicOperation),
    Direct(FunctionId),
    Indirect(InstId),
}

#[derive(Clone, Copy)]
pub struct ComeFromCase {
    from: BlockId,
    value: InstId,
}

//task(bc): Rename to lofi
//task(bc): Superinstructions
pub enum Inst {
    Imm(Imm),

    // Memory and value manipulation
    Alloca { t: PhysicalType },
    Store { dst: InstId, value: InstId },
    Load { t: PhysicalType, src: InstId },
    Copy { dst: InstId, src: InstId, size: u32 },
    StructOffset { struct_t: PhysicalType, base: InstId, field_index: u32 },

    Call(InstKind, BcCallee, MSlice<InstId>),

    // Control Flow
    Jump(BlockId),
    JumpIf { cond: InstId, cons: BlockId, alt: BlockId },
    Unreachable,
    // goto considered harmful, but come-from is friend (phi node)
    ComeFrom { t: PhysicalType, incomings: MSlice<ComeFromCase> },
    Ret(InstId),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PhysicalType {
    I(IntegerType),
    F32,
    F64,
    Pointer,
    Aggregate(Layout),
}

impl std::fmt::Display for PhysicalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysicalType::I(i) => write!(f, "{i}"),
            PhysicalType::F32 => write!(f, "f32"),
            PhysicalType::F64 => write!(f, "f64"),
            PhysicalType::Pointer => write!(f, "ptr"),
            PhysicalType::Aggregate(layout) => write!(f, "agg({})", layout.size),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InstKind {
    Value(PhysicalType),
    Void,
    Terminator,
}

impl std::fmt::Display for InstKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstKind::Value(t) => write!(f, "{}", t),
            InstKind::Void => write!(f, "void"),
            InstKind::Terminator => write!(f, "term"),
        }
    }
}

impl InstKind {
    pub const PTR: InstKind = InstKind::Value(PhysicalType::Pointer);
    pub const U8: InstKind = InstKind::Value(PhysicalType::I(IntegerType::U8));
    fn is_ptr(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Pointer))
    }
    fn is_aggregate(&self) -> bool {
        matches!(self, InstKind::Value(PhysicalType::Aggregate(_)))
    }
    fn is_storage(&self) -> bool {
        self.is_ptr() || self.is_aggregate()
    }
    fn is_value(&self) -> bool {
        matches!(self, InstKind::Value(_))
    }
    #[track_caller]
    fn expect_value(&self) -> PhysicalType {
        match self {
            InstKind::Value(t) => *t,
            _ => panic!("Expected value, got {}", self.kind_str()),
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
            InstKind::Value(PhysicalType::I(_)) => "int",
            InstKind::Value(PhysicalType::F32) => "f32",
            InstKind::Value(PhysicalType::F64) => "f64",
            InstKind::Value(PhysicalType::Pointer) => "ptr",
            InstKind::Value(PhysicalType::Aggregate(_)) => "agg",
            InstKind::Void => "void",
            InstKind::Terminator => "term",
        }
    }
}

fn compile_type(types: &TypePool, type_id: TypeId) -> Result<InstKind, String> {
    match types.get(type_id) {
        //task(bc): Eventually, Unit should correspond to Void, not a byte. But only once we can
        //successfully lower it to a zero-sized type everywhere; for example a struct member of
        //type unit
        Type::Unit => Ok(InstKind::Value(PhysicalType::I(IntegerType::U8))),
        Type::Char => Ok(InstKind::Value(PhysicalType::I(IntegerType::U8))),
        Type::Bool => Ok(InstKind::Value(PhysicalType::I(IntegerType::U8))),
        Type::Integer(i) => Ok(InstKind::Value(PhysicalType::I(*i))),
        Type::Float(FloatType::F32) => Ok(InstKind::Value(PhysicalType::F32)),
        Type::Float(FloatType::F64) => Ok(InstKind::Value(PhysicalType::F64)),
        Type::Pointer => Ok(InstKind::Value(PhysicalType::Pointer)),
        Type::Reference(_) => Ok(InstKind::Value(PhysicalType::Pointer)),
        Type::FunctionPointer(_) => Ok(InstKind::Value(PhysicalType::Pointer)),
        Type::Array(_) | Type::Struct(_) | Type::Enum(_) | Type::EnumVariant(_) => {
            let layout = types.get_layout(type_id);
            Ok(InstKind::Value(PhysicalType::Aggregate(layout)))
        }
        Type::Lambda(lam) => {
            // Should be the struct type of the lambda's environment
            let layout = types.get_layout(lam.env_type);
            Ok(InstKind::Value(PhysicalType::Aggregate(layout)))
        }
        Type::LambdaObject(lam_obj) => {
            // Should be the lambda's object's struct type
            // - env ptr
            // - function ptr
            let layout = types.get_layout(lam_obj.struct_representation);
            Ok(InstKind::Value(PhysicalType::Aggregate(layout)))
        }
        Type::Never => Ok(InstKind::Terminator),

        Type::Function(_)
        | Type::Static(_)
        | Type::Generic(_)
        | Type::TypeParameter(_)
        | Type::FunctionTypeParameter(_)
        | Type::InferenceHole(_)
        | Type::Unresolved(_)
        | Type::RecursiveReference(_) => Err(format!("Not a physical type")),
    }
}

impl Inst {
    pub fn get_kind(&self, types: &TypePool) -> InstKind {
        match self {
            Inst::Imm(imm) => match imm {
                Imm::Unit => InstKind::U8,
                Imm::Bool(_) => InstKind::U8,
                Imm::Char(_) => InstKind::U8,
                Imm::Int(i) => InstKind::Value(PhysicalType::I(i.get_integer_type())),
                Imm::Float(f) => InstKind::Value(PhysicalType::F32),
                Imm::String(_) => {
                    InstKind::Value(PhysicalType::Aggregate(types.get_layout(STRING_TYPE_ID)))
                }
            },
            Inst::Alloca { .. } => InstKind::PTR,
            Inst::Store { .. } => InstKind::Void,
            Inst::Load { t, .. } => InstKind::Value(*t),
            Inst::Copy { .. } => InstKind::Void,
            Inst::StructOffset { .. } => InstKind::PTR,
            Inst::Call(t, ..) => *t,
            Inst::Jump(_) => InstKind::Terminator,
            Inst::JumpIf { .. } => InstKind::Terminator,
            Inst::Unreachable => InstKind::Terminator,
            Inst::ComeFrom { t, .. } => InstKind::Value(*t),
            Inst::Ret(_) => InstKind::Terminator,
        }
    }

    pub fn is_terminator(&self, types: &TypePool) -> bool {
        self.get_kind(types).is_terminator()
    }
}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> BcResult<()> {
    let mut bc = k1.bytecode.borrow_mut();

    let f = k1.get_function(function_id);

    let Some(body) = f.body_block else { return Err("no body".into()) };

    //task(bc): Re-use Builder's allocations
    let fn_span = k1.ast.get_span_for_id(f.parsed_id);
    let mut builder = Builder {
        bc: &mut bc,
        k1,
        variables: vec![],
        blocks: vec![],
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
    bc: &'bc mut ProgramBytecode,
    k1: &'k1 TypedProgram,
    variables: Vec<BuilderVariable>,
    blocks: Vec<Block>,
    loops: FxHashMap<ScopeId, LoopInfo>,
    cur_block: BlockId,
    cur_span: SpanId,
}

impl<'bc, 'k1> Builder<'bc, 'k1> {
    fn push_inst_to(&mut self, block: BlockId, inst: Inst) -> InstId {
        let id = self.bc.instrs.add(inst);
        let ids = self.bc.sources.add(self.cur_span);
        let idc = self.bc.comments.add(MStr::E);
        debug_assert!(id == ids && id == idc);

        self.blocks[block as usize].instrs.push(id);
        id
    }

    fn get_inst_kind(&self, inst_id: InstId) -> InstKind {
        self.bc.instrs.get(inst_id).get_kind(&self.k1.types)
    }

    fn compile_type(&self, type_id: TypeId) -> InstKind {
        match compile_type(&self.k1.types, type_id) {
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

    fn push_block(&mut self, name: MStr) -> BlockId {
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

fn store_if_dst(b: &mut Builder, dst: Option<InstId>, t: TypeId, src: InstId) -> InstId {
    match dst {
        None => src,
        Some(dst) => store_value(b, t, dst, src),
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
    b.cur_span = b.k1.get_stmt_span(stmt);
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
                    let _rhs_stored = compile_expr(b, Some(builder_variable.inst), ass.value)?;
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
        TypedStmt::Require(_typed_require_stmt) => {
            todo!("bc require")
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
    b.cur_span = b.k1.exprs.get(expr).get_span();
    let e = b.k1.exprs.get(expr);
    match e {
        TypedExpr::Unit(_) => {
            let imm = b.push_inst(Inst::Imm(Imm::Unit));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Char(byte, _) => {
            let imm = b.push_inst(Inst::Imm(Imm::Char(*byte)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Bool(bv, _) => {
            let imm = b.push_inst(Inst::Imm(Imm::Bool(*bv)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Integer(int) => {
            let imm = b.push_inst(Inst::Imm(Imm::Int(int.value)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Float(float) => {
            let imm = b.push_inst(Inst::Imm(Imm::Float(float.value)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::String(string_id, _) => {
            //task(bc): Careful now; string is a struct
            let imm = b.push_inst(Inst::Imm(Imm::String(*string_id)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Struct(struct_literal) => {
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.alloca_type(struct_literal.type_id),
            };
            for (field_index, field) in struct_literal.fields.iter().enumerate() {
                debug_assert!(b.k1.types.get(struct_literal.type_id).as_struct().is_some());
                let struct_type = compile_type(&b.k1.types, struct_literal.type_id)?.expect_value();
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
            let struct_type = compile_type(&b.k1.types, field_access.struct_type)?.expect_value();
            let field_ptr = b.push_inst(Inst::StructOffset {
                struct_t: struct_type,
                base: struct_base,
                field_index: field_access.field_index,
            });
            if field_access.is_referencing {
                Ok(field_ptr)
            } else {
                // nocommit Revisit this. In the oldbackend i wrote:
                // We copy the field whether or not the base struct is a reference, because it
                // could be inside a reference, we can't assume this isn't mutable memory just
                // because our immediate base struct isn't a reference

                // But I'm not sure if that's true. Once you have a non-reference
                // struct value, I think its safe to assume it can't change
                // In the scenario above, you'd have to have de-referenced that struct
                // out of the container struct pointer, which would have made a copy
                // So you already have a local copy that's just a value
                let make_copy = false;
                let loaded = load_value(b, field_access.result_type, field_ptr, make_copy);
                Ok(loaded)
            }
        }
        TypedExpr::ArrayGetElement(get) => {
            todo!("array get")
        }
        TypedExpr::Variable(variable_expr) => {
            let Some(var) = b.get_variable(variable_expr.variable_id) else {
                b.k1.ice_with_span("no variable", variable_expr.span)
            };
            let var_value = if var.indirect {
                // task(bc): Eventual Copy here; since its a Load then a Store if there's a dst
                let var_value = load_value(b, variable_expr.type_id, var.inst, false);
                var_value
            } else {
                var.inst
            };
            let store = store_if_dst(b, dst, variable_expr.type_id, var_value);
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
            let callee = match &call.callee {
                Callee::StaticFunction(function_id) => BcCallee::Direct(*function_id),
                Callee::StaticLambda { .. } => {
                    todo!("bc lambda call")
                }
                Callee::Abstract { .. } => return Err("bc abstract call".into()),
                Callee::DynamicLambda(_) => todo!("bc lambda ind call"),
                Callee::DynamicFunction { function_pointer_expr } => {
                    let callee_inst = compile_expr(b, None, *function_pointer_expr)?;
                    BcCallee::Indirect(callee_inst)
                }
                Callee::DynamicAbstract { .. } => {
                    return Err("bc abstract call".into());
                }
            };
            let return_type = b.compile_type(*return_type);
            let call_inst = {
                let mut args = b.bc.mem.new_vec(call.args.len());
                for arg in &call.args {
                    args.push(compile_expr(b, None, *arg)?);
                }
                Inst::Call(return_type, callee, b.bc.mem.vec_to_mslice(&args))
            };
            let call_inst_id = b.push_inst(call_inst);
            Ok(call_inst_id)
        }
        TypedExpr::Match(match_expr) => {
            for stmt in &match_expr.initial_let_statements {
                compile_stmt(b, None, *stmt)?;
            }

            let mut arm_blocks = b.bc.mem.new_vec(match_expr.arms.len());
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
                    let Inst::ComeFrom { incomings: i, .. } = b.bc.instrs.get_mut(come_from) else {
                        unreachable!()
                    };
                    *i = b.bc.mem.push_slice(&incomings);
                    Ok(store_if_dst(b, dst, match_expr.result_type, come_from))
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
            let int_imm = b.push_inst(Inst::Imm(Imm::Int(tag_int_value)));
            b.push_store(tag_base, int_imm);

            if let Some(payload) = &enumc.payload {
                let variant_struct = enum_variant.my_type_id;
                todo!("bc enum payload")
                //let payload_offset = b.push_inst(Inst::StructOffset {
                //    struct_t: variant_struct,
                //    base: enum_base,
                //    field_index: 1,
                //});
                //let value = compile_expr(b, payload_offset, expr)?;
            }

            Ok(enum_base)
        }
        TypedExpr::EnumIsVariant(_) => todo!(),
        TypedExpr::EnumGetTag(_) => todo!(),
        TypedExpr::EnumGetPayload(_) => todo!(),
        TypedExpr::Cast(_) => todo!(),
        TypedExpr::Return(typed_return) => {
            //task(bc): Track an optional 'ret ptr' for the function; compile straight into it
            let inst = compile_expr(b, None, typed_return.value)?;
            let ret = b.push_inst(Inst::Ret(inst));
            Ok(ret)
        }
        TypedExpr::Lambda(_) => todo!(),
        TypedExpr::FunctionPointer(_) => todo!(),
        TypedExpr::FunctionToLambdaObject(_) => todo!(),
        TypedExpr::PendingCapture(_) => todo!(),
        TypedExpr::StaticValue(_, _, _) => todo!(),
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
                let continue_block = if is_last { cons_block } else { b.push_block(MStr::E) };
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
            if !is_last && inst.is_terminator(&k1.types) {
                errors.push(format!("b{block_index}: stray terminator"))
            };
            if is_last && !inst.is_terminator(&k1.types) {
                errors.push(format!("b{block_index}: unterminated"))
            }

            match inst {
                Inst::Imm(_imm) => (),
                Inst::Alloca { .. } => (),
                Inst::Store { dst, .. } => {
                    let dst_type = bc.instrs.get(*dst).get_kind(&k1.types);
                    if !dst_type.is_storage() {
                        errors.push(format!("store dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::Load { t, src } => {
                    let src_type = bc.instrs.get(*src).get_kind(&k1.types);
                    if !src_type.is_storage() {
                        errors.push(format!("load src v{} is not a ptr", *inst_id))
                    }
                }
                Inst::Copy { dst, src, .. } => {
                    let src_type = bc.instrs.get(*src).get_kind(&k1.types);
                    if !src_type.is_storage() {
                        errors.push(format!("copy src v{} is not a ptr", *src))
                    }
                    let dst_type = bc.instrs.get(*dst).get_kind(&k1.types);
                    if !dst_type.is_storage() {
                        errors.push(format!("copy dst v{} is not a ptr", *inst_id))
                    }
                }
                Inst::StructOffset { base, .. } => {
                    let base_type = bc.instrs.get(*base).get_kind(&k1.types);
                    if !base_type.is_storage() {
                        errors.push(format!("struct_offset base v{} is not a ptr", *base))
                    }
                }
                Inst::Call(_, _, _) => (),
                Inst::Jump(block) => {
                    if f.blocks.get(*block as usize).is_none() {
                        errors.push(format!("jump to non-existent block b{}", *block))
                    }
                }
                Inst::JumpIf { cond, .. } => {
                    let cond_type = bc.instrs.get(*cond).get_kind(&k1.types);
                    if !cond_type.is_value() {
                        errors.push(format!("jumpif cond v{} is not a value", *inst_id))
                    }
                }
                Inst::Unreachable => (),
                Inst::ComeFrom { t, incomings } => (),
                Inst::Ret(inst_id) => {
                    let ret_val_type = bc.instrs.get(*inst_id).get_kind(&k1.types);
                    if ret_val_type.is_terminator() || ret_val_type.is_void() {
                        errors.push(format!("ret value v{} is not a value", *inst_id))
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
        Inst::Alloca { t } => {
            write!(w, "alloca {}", *t)?;
        }
        Inst::Store { dst, value } => {
            let inst_kind = bc.instrs.get(*value).get_kind(&k1.types);
            write!(w, "store to v{}, {} v{}", *dst, inst_kind, *value)?;
        }
        Inst::Load { t, src } => {
            write!(w, "load {}", *t)?;
            write!(w, " from v{}", *src)?;
        }
        Inst::Copy { dst, src, size } => {
            write!(w, "copy {} v{}, src v{}", *size, *dst, *src)?;
        }
        Inst::StructOffset { struct_t, base, field_index } => {
            write!(w, "struct_offset {}", *struct_t)?;
            write!(w, ".{}, v{}", *field_index, *base)?;
        }
        Inst::Call(t, callee, args) => {
            write!(w, "call {} ", *t)?;
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
            write!(w, "comefrom {} [", *t)?;
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
    };
    if show_source {
        let span_id = bc.sources.get(inst_id);
        let lines = k1.ast.get_span_content(*span_id);
        let first_line = lines.lines().next().unwrap_or("");
        write!(w, " ;\t\t\t {}", first_line)?;
    }
    Ok(())
}

pub fn display_imm(w: &mut impl Write, imm: &Imm) -> std::fmt::Result {
    match imm {
        Imm::Unit => write!(w, "unit"),
        Imm::Bool(b) => write!(w, "bool {}", b),
        Imm::Char(c) => write!(w, "char '{}'", *c as char),
        Imm::Int(int) => write!(w, "int {}", int),
        Imm::Float(float) => write!(w, "float {}", float),
        Imm::String(string_id) => write!(w, "string {}", string_id),
    }
}
