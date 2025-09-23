// Copyright (c) 2025 knix
// All rights reserved.
//
// The goal here is a strongly-typed SSA form
// instruction-based IR with basic blocks, obviously
// very much like LLVM, as that is our primary target.
// But I currently think there's going to be a lot of value
// in having our own. It'll be easier to write an interpreter for
// and will help make adding other backends far, far easier
use crate::{
    SV8,
    kmem::{self, MSlice},
    parse::StringId,
    pool::VPool,
    typer::{
        types::{NEVER_TYPE_ID, Type, TypeId},
        *,
    },
};
use smallvec::smallvec;
use std::{borrow::Cow, fmt::Write};

pub struct ProgramBytecode {
    pub mem: kmem::Mem,
    pub functions: VPool<Option<Function>, FunctionId>,
}

pub struct Block {
    //task(bc): Good memory layout for blocks and instructions
    pub instrs: Vec<InstId>,
}

pub struct Function {
    pub instrs: Vec<Inst>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn get_block(&self, id: BlockId) -> &Block {
        &self.blocks[id as usize]
    }
    pub fn get_inst(&self, id: InstId) -> &Inst {
        &self.instrs[id as usize]
    }
}

type BcResult<T> = Result<T, Cow<'static, str>>;

#[derive(Clone, Copy)]
pub enum Imm {
    Unit,
    Bool(bool),
    Char(u8),
    Integer(TypedIntValue),
    Float(TypedFloatValue),
    String(StringId),
}

pub type InstId = u32;
pub type BlockId = u32;

#[derive(Clone, Copy)]
pub struct InstSlice {
    start: u32,
    len: u32,
}

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
    //task(bc): InstId or Value? Or, do we allow immediates here?
    value: InstId,
}

//task(bc): Think about source location on bc::Inst
pub enum Inst {
    Imm(Imm),

    // Memory and value manipulation
    Alloca { t: TypeId },
    Store { t: TypeId, dst: InstId, src: InstId },
    Load { t: TypeId, src: InstId },
    StructOffset { struct_t: TypeId, base: InstId, field_index: u32 },

    Call(TypeId, BcCallee, MSlice<InstId>),

    // Control Flow
    Jump(BlockId),
    JumpIf { cond: InstId, cons: BlockId, alt: BlockId },
    Unreachable,
    // goto considered harmful, but come-from is friend (phi node)
    ComeFrom { incomings: MSlice<ComeFromCase> },
    Ret(InstId),
}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> BcResult<()> {
    let Some(mut bc) = std::mem::take(&mut k1.bytecode) else {
        return Err("bytecode object is missing".into());
    };

    let f = k1.get_function(function_id);

    let Some(body) = f.body_block else { return Err("no body".into()) };

    //task(bc): Re-use Builder's buffers
    let mut builder = Builder { instrs: vec![], variables: vec![], blocks: vec![], cur_block: 0 };
    builder.blocks.push(Block { instrs: vec![] });

    compile_block_stmts(k1, &mut bc, &mut builder, body)?;

    *bc.functions.get_mut(function_id) =
        Some(Function { instrs: builder.instrs, blocks: builder.blocks });

    k1.bytecode = Some(bc);

    Ok(())
}

struct BuilderVariable {
    id: VariableId,
    inst: InstId,
    indirect: bool,
}

struct Builder {
    instrs: Vec<Inst>,
    variables: Vec<BuilderVariable>,
    blocks: Vec<Block>,
    cur_block: BlockId,
}

impl Builder {
    fn push_inst_to(&mut self, block: BlockId, inst: Inst) -> InstId {
        let id = self.instrs.len();
        self.instrs.push(inst);
        self.blocks[block as usize].instrs.push(id as InstId);
        id as InstId
    }

    fn push_inst(&mut self, inst: Inst) -> InstId {
        self.push_inst_to(self.cur_block, inst)
    }

    fn push_store(&mut self, ty: TypeId, dst: InstId, src: InstId) -> InstId {
        self.push_inst(Inst::Store { t: ty, dst, src })
    }

    fn push_block(&mut self) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block { instrs: vec![] });
        id as BlockId
    }

    fn get_inst_mut(&mut self, inst_id: InstId) -> &mut Inst {
        &mut self.instrs[inst_id as usize]
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
        Some(dst) => b.push_store(t, dst, src),
    }
}

fn compile_block_stmts(
    k1: &TypedProgram,
    bc: &mut ProgramBytecode,
    b: &mut Builder,
    body: TypedExprId,
) -> BcResult<()> {
    let TypedExpr::Block(body) = k1.exprs.get(body) else {
        return Err("body is not a block".into());
    };

    for &stmt in &body.statements {
        compile_stmt(k1, bc, b, stmt)?;
    }

    Ok(())
}

fn compile_stmt(
    k1: &TypedProgram,
    bc: &mut ProgramBytecode,
    b: &mut Builder,
    stmt: TypedStmtId,
) -> BcResult<()> {
    match k1.stmts.get(stmt) {
        TypedStmt::Expr(typed_expr_id, _) => {
            compile_expr(k1, bc, b, None, *typed_expr_id)?;
            Ok(())
        }
        TypedStmt::Let(let_stmt) => {
            let variable_alloca = b.push_inst(Inst::Alloca { t: let_stmt.variable_type });

            // value_ptr means a pointer matching the type of the rhs
            // For a referencing let, the original alloca a ptr
            // So we need one for the inner type as well
            let value_ptr = if let_stmt.is_referencing {
                let Type::Reference(reference_type) = k1.types.get(let_stmt.variable_type) else {
                    panic!("Expected reference for referencing let");
                };
                let reference_inner_type = reference_type.inner_type;
                let value_alloca = b.push_inst(Inst::Alloca { t: reference_inner_type });
                b.push_store(let_stmt.variable_type, variable_alloca, value_alloca);
                value_alloca
            } else {
                variable_alloca
            };

            // If there's an initializer, store it in value_ptr
            if let Some(init) = let_stmt.initializer {
                compile_expr(k1, bc, b, Some(value_ptr), init)?;
            }
            b.variables.push(BuilderVariable {
                id: let_stmt.variable_id,
                inst: variable_alloca,
                indirect: true,
            });
            Ok(())
        }
        TypedStmt::Assignment(_assignment_stmt) => todo!(),
        TypedStmt::Require(_typed_require_stmt) => todo!(),
        TypedStmt::Defer(_typed_defer_stmt) => todo!(),
    }
}

fn compile_expr(
    k1: &TypedProgram,
    bc: &mut ProgramBytecode,
    b: &mut Builder,
    // Where to put the result; aka value placement or destination-aware codegen
    dst: Option<InstId>,
    expr: TypedExprId,
) -> BcResult<InstId> {
    let e = k1.exprs.get(expr);
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
            let imm = b.push_inst(Inst::Imm(Imm::Integer(int.value)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Float(float) => {
            let imm = b.push_inst(Inst::Imm(Imm::Float(float.value)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::String(string_id, _) => {
            let imm = b.push_inst(Inst::Imm(Imm::String(*string_id)));
            let store = store_if_dst(b, dst, e.get_type(), imm);
            Ok(store)
        }
        TypedExpr::Struct(struct_literal) => {
            let struct_base = match dst {
                Some(dst) => dst,
                None => b.push_inst(Inst::Alloca { t: struct_literal.type_id }),
            };
            for (field_index, field) in struct_literal.fields.iter().enumerate() {
                debug_assert!(k1.types.get(struct_literal.type_id).as_struct().is_some());
                let struct_offset = b.push_inst(Inst::StructOffset {
                    struct_t: struct_literal.type_id,
                    base: struct_base,
                    field_index: field_index as u32,
                });
                compile_expr(k1, bc, b, Some(struct_offset), field.expr)?;
            }
            match dst {
                None => Ok(b.push_inst(Inst::Load { t: struct_literal.type_id, src: struct_base })),
                Some(dst) => Ok(dst),
            }
        }
        TypedExpr::StructFieldAccess(_field_access) => todo!(),
        TypedExpr::ArrayGetElement(_array_get_element) => todo!(),
        TypedExpr::Variable(variable_expr) => {
            let Some(var) = b.get_variable(variable_expr.variable_id) else {
                k1.ice_with_span("no variable", variable_expr.span)
            };
            let var_value = if var.indirect {
                // Eventual Copy here; since its a Load then a Store if there's a dst
                let var_value = b.push_inst(Inst::Load { t: variable_expr.type_id, src: var.inst });
                var_value
            } else {
                var.inst
            };
            let store = store_if_dst(b, dst, variable_expr.type_id, var_value);
            Ok(store)
        }
        TypedExpr::UnaryOp(_unary_op) => todo!(),
        TypedExpr::Block(_typed_block) => todo!("block"),
        TypedExpr::Call { call_id, return_type, .. } => {
            let call = k1.calls.get(*call_id);
            let callee = match &call.callee {
                Callee::StaticFunction(function_id) => BcCallee::Direct(*function_id),
                Callee::StaticLambda { .. } => {
                    todo!("bc lambda call")
                }
                Callee::Abstract { .. } => return Err("bc abstract call".into()),
                Callee::DynamicLambda(_) => todo!("bc lambda ind call"),
                Callee::DynamicFunction { function_pointer_expr } => {
                    let callee_inst = compile_expr(k1, bc, b, None, *function_pointer_expr)?;
                    BcCallee::Indirect(callee_inst)
                }
                Callee::DynamicAbstract { .. } => {
                    return Err("bc abstract call".into());
                }
            };
            let call_inst = {
                let mut args = bc.mem.new_vec(call.args.len());
                for arg in &call.args {
                    args.push(compile_expr(k1, bc, b, None, *arg)?);
                }
                Inst::Call(*return_type, callee, bc.mem.vec_to_mslice(&args))
            };
            let call_inst_id = b.push_inst(call_inst);
            Ok(call_inst_id)
        }
        TypedExpr::Match(match_expr) => {
            for stmt in &match_expr.initial_let_statements {
                compile_stmt(k1, bc, b, *stmt)?;
            }

            let mut arm_blocks = bc.mem.new_vec(match_expr.arms.len());
            for _arm in match_expr.arms.iter() {
                let arm_block = b.push_block();
                let arm_consequent_block = b.push_block();
                arm_blocks.push((arm_block, arm_consequent_block));
            }

            let first_arm_block = arm_blocks[0].0;
            b.push_inst(Inst::Jump(first_arm_block));

            let fail_block = b.push_block();
            b.goto_block(fail_block);
            b.push_inst(Inst::Unreachable);

            let match_end_block = b.push_block();
            b.goto_block(match_end_block);
            let result_come_from = if match_expr.result_type == NEVER_TYPE_ID {
                None
            } else {
                Some(b.push_inst(Inst::ComeFrom { incomings: MSlice::empty() }))
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
                compile_matching_condition(
                    k1,
                    bc,
                    b,
                    &arm.condition,
                    *arm_cons_block,
                    next_arm_or_fail,
                )?;

                b.goto_block(*arm_cons_block);
                let result = compile_expr(k1, bc, b, None, arm.consequent_expr)?;
                if k1.get_expr_type_id(arm.consequent_expr) != NEVER_TYPE_ID {
                    let current_block = b.cur_block;
                    incomings.push(ComeFromCase { from: current_block, value: result });
                    b.push_inst(Inst::Jump(match_end_block));
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
                    let Inst::ComeFrom { incomings: i } = b.get_inst_mut(come_from) else {
                        unreachable!()
                    };
                    *i = bc.mem.push_slice(&incomings);
                    Ok(store_if_dst(b, dst, match_expr.result_type, come_from))
                }
            }
        }
        TypedExpr::WhileLoop(_) => todo!(),
        TypedExpr::LoopExpr(_) => todo!(),
        TypedExpr::EnumConstructor(_) => todo!(),
        TypedExpr::EnumIsVariant(_) => todo!(),
        TypedExpr::EnumGetTag(_) => todo!(),
        TypedExpr::EnumGetPayload(_) => todo!(),
        TypedExpr::Cast(_) => todo!(),
        TypedExpr::Return(typed_return) => {
            let inst = compile_expr(k1, bc, b, None, typed_return.value)?;
            let ret = b.push_inst(Inst::Ret(inst));
            Ok(ret)
        }
        TypedExpr::Break(_) => todo!(),
        TypedExpr::Lambda(_) => todo!(),
        TypedExpr::FunctionPointer(_) => todo!(),
        TypedExpr::FunctionToLambdaObject(_) => todo!(),
        TypedExpr::PendingCapture(_) => todo!(),
        TypedExpr::StaticValue(_, _, _) => todo!(),
    }
}

fn compile_matching_condition(
    k1: &TypedProgram,
    bc: &mut ProgramBytecode,
    b: &mut Builder,
    mc: &MatchingCondition,
    cons_block: BlockId,
    condition_fail_block: BlockId,
) -> BcResult<()> {
    if mc.instrs.is_empty() {
        // Always true
        b.push_inst(Inst::Jump(cons_block));
        return Ok(());
    }
    for (index, inst) in mc.instrs.iter().enumerate() {
        match inst {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                compile_stmt(k1, bc, b, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_value: InstId = compile_expr(k1, bc, b, None, *value)?;
                if k1.get_expr_type_id(*value) == NEVER_TYPE_ID {
                    return Ok(());
                }
                let is_last = index == mc.instrs.len() - 1;
                let continue_block = if is_last { cons_block } else { b.push_block() };
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

////////////////////////////// Display //////////////////////////////

pub fn display_function(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: FunctionId,
) -> std::fmt::Result {
    let Some(function) = bc.functions.get(function) else { return Ok(()) };
    for (index, _block) in function.blocks.iter().enumerate() {
        let id = index as BlockId;
        display_block(w, k1, bc, function, id)?;
    }
    Ok(())
}

pub fn display_block(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: &Function,
    block_id: BlockId,
) -> std::fmt::Result {
    let block = function.get_block(block_id);
    writeln!(w, "B{}", block_id)?;
    for (idx, inst_id) in block.instrs.iter().enumerate() {
        write!(w, "  v{} = ", *inst_id)?;
        display_inst(w, k1, bc, function, *inst_id)?;
        let last = idx == block.instrs.len() - 1;
        writeln!(w)?;
    }
    writeln!(w, "END")?;
    Ok(())
}

pub fn display_inst(
    w: &mut impl Write,
    k1: &TypedProgram,
    bc: &ProgramBytecode,
    function: &Function,
    inst_id: InstId,
) -> std::fmt::Result {
    match function.get_inst(inst_id) {
        Inst::Imm(imm) => {
            write!(w, "imm ")?;
            display_imm(w, imm)?;
            Ok(())
        }
        Inst::Alloca { t } => {
            write!(w, "alloca ",)?;
            k1.display_type_id(w, *t, false)?;
            Ok(())
        }
        Inst::Store { t, dst, src } => {
            write!(w, "store to v{}, ", *dst)?;
            k1.display_type_id(w, *t, false)?;
            write!(w, " v{}", *src)?;
            Ok(())
        }
        Inst::Load { t, src } => {
            write!(w, "load from ")?;
            k1.display_type_id(w, *t, false)?;
            write!(w, " v{}", *src)?;
            Ok(())
        }
        Inst::StructOffset { struct_t, base, field_index } => {
            write!(w, "struct_offset ")?;
            k1.display_type_id(w, *struct_t, false)?;
            write!(w, ".{}, v{}", *field_index, *base)?;
            Ok(())
        }
        Inst::Call(t, callee, args) => {
            write!(w, "call ")?;
            k1.display_type_id(w, *t, false)?;
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
            Ok(())
        }

        Inst::Jump(block_id) => {
            write!(w, "jmp b{}", *block_id)?;
            Ok(())
        }
        Inst::JumpIf { cond, cons, alt } => {
            write!(w, "jmpif v{}, b{}, b{}", *cond, *cons, *alt)?;
            Ok(())
        }
        Inst::Unreachable => {
            write!(w, "unreachable")?;
            Ok(())
        }
        Inst::ComeFrom { incomings } => {
            write!(w, "comefrom [")?;
            for (i, incoming) in bc.mem.get_slice(*incomings).iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }
                write!(w, "(b{}: v{})", incoming.from, incoming.value)?;
            }
            write!(w, "]")?;
            Ok(())
        }
        Inst::Ret(value) => {
            write!(w, "ret v{}", *value)?;
            Ok(())
        }
    }
}

pub fn display_imm(w: &mut impl Write, imm: &Imm) -> std::fmt::Result {
    match imm {
        Imm::Unit => write!(w, "unit"),
        Imm::Bool(b) => write!(w, "bool {}", b),
        Imm::Char(c) => write!(w, "char '{}'", *c as char),
        Imm::Integer(int) => write!(w, "int {}", int),
        Imm::Float(float) => write!(w, "float {}", float),
        Imm::String(string_id) => write!(w, "string {}", string_id),
    }
}
