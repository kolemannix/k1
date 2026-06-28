use ahash::HashMapExt;
use fxhash::FxHashMap;

use crate::ir::{
    self, BackendBuiltin, BlockId, FloatCmpPred, Inst, InstId, InstKind, IntCmpPred, IrCallee,
    IrUnit, IrUnitId, Value as IrValue,
};
use crate::lex::SpanId;
use crate::parse;
use crate::typer::types::{Layout, PhysicalType, ScalarType, TypeId};
use crate::typer::{FunctionId, K1Result, StaticValueId, TypedExprId, TypedGlobalId, TypedProgram};
use crate::{errf, kmem::MSlice};

#[derive(Default)]
pub struct ProgramBc {
    pub functions: FxHashMap<FunctionId, BcUnit>,
    pub exprs: FxHashMap<TypedExprId, BcUnit>,
}

impl ProgramBc {
    pub fn make() -> Self {
        Self { functions: FxHashMap::new(), exprs: FxHashMap::new() }
    }
}

#[derive(Clone)]
pub struct BcUnit {
    pub result_type_id: TypeId,
    pub unit_id: IrUnitId,
    pub return_pt: PhysicalType,
    pub diverges: bool,
    pub param_count: u32,
    pub local_count: u32,
    pub instrs: Vec<BcInst>,
    pub spans: Vec<SpanId>,
}

#[derive(Clone)]
pub struct BcInst {
    pub block: u32,
    pub kind: BcInstKind,
}

#[derive(Clone)]
pub enum BcInstKind {
    Data {
        dst: u32,
        imm: ir::DataInst,
    },
    Alloca {
        dst: u32,
        vm_layout: Layout,
    },
    Store {
        dst: BcValue,
        value: BcValue,
        t: ScalarType,
    },
    Load {
        dst: u32,
        t: ScalarType,
        src: BcValue,
    },
    Copy {
        dst: BcValue,
        src: BcValue,
        vm_size: u32,
    },
    StructOffset {
        dst: u32,
        base: BcValue,
        vm_offset: u32,
    },
    ArrayOffset {
        dst: u32,
        element_t: PhysicalType,
        base: BcValue,
        element_index: BcValue,
    },
    Call {
        dst: Option<u32>,
        ret_type: PhysicalType,
        callee: BcCallee,
        args: Vec<BcValue>,
        ret_dst: Option<BcValue>,
    },
    Jump {
        offset: i32,
    },
    JumpIf {
        cond: BcValue,
        cons_offset: i32,
        alt_offset: i32,
    },
    Unreachable,
    Phi {
        dst: u32,
        incomings: Vec<(u32, BcValue)>,
    },
    Ret {
        v: BcValue,
    },
    BoolNegate {
        dst: u32,
        v: BcValue,
    },
    BitNot {
        dst: u32,
        v: BcValue,
    },
    BitCast {
        dst: u32,
        v: BcValue,
    },
    IntTrunc {
        dst: u32,
        v: BcValue,
        to: ScalarType,
    },
    IntExtU {
        dst: u32,
        v: BcValue,
    },
    IntExtS {
        dst: u32,
        v: BcValue,
        from: ScalarType,
        to: ScalarType,
    },
    FloatTrunc {
        dst: u32,
        v: BcValue,
    },
    FloatExt {
        dst: u32,
        v: BcValue,
    },
    Float32ToIntUnsigned {
        dst: u32,
        v: BcValue,
        to: ScalarType,
    },
    Float64ToIntUnsigned {
        dst: u32,
        v: BcValue,
        to: ScalarType,
    },
    Float32ToIntSigned {
        dst: u32,
        v: BcValue,
        to: ScalarType,
    },
    Float64ToIntSigned {
        dst: u32,
        v: BcValue,
        to: ScalarType,
    },
    IntToFloatUnsigned {
        dst: u32,
        v: BcValue,
        from: ScalarType,
        to: ScalarType,
    },
    IntToFloatSigned {
        dst: u32,
        v: BcValue,
        from: ScalarType,
        to: ScalarType,
    },
    PtrToWord {
        dst: u32,
        v: BcValue,
    },
    WordToPtr {
        dst: u32,
        v: BcValue,
    },
    IntAdd {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntSub {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntMul {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntDivUnsigned {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntDivSigned {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntRemUnsigned {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntRemSigned {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    IntCmp {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        pred: IntCmpPred,
        width: u8,
    },
    FloatAdd {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    FloatSub {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    FloatMul {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    FloatDiv {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    FloatRem {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    FloatCmp {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        pred: FloatCmpPred,
        width: u8,
    },
    BitAnd {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BitOr {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BitXor {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BitShiftLeft {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BitUnsignedShiftRight {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BitSignedShiftRight {
        dst: u32,
        lhs: BcValue,
        rhs: BcValue,
        width: u8,
    },
    BakeStaticValue {
        dst: u32,
        type_id: TypeId,
        value: BcValue,
    },
}

#[derive(Clone)]
pub enum BcCallee {
    BackendBuiltin(BackendBuiltin),
    Direct(FunctionId),
    Indirect(BcValue),
    Extern {
        library_name: Option<parse::StringId>,
        function_name: parse::StringId,
        function_id: FunctionId,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BcValue {
    Local(u32),
    GlobalAddr { storage_pt: PhysicalType, id: TypedGlobalId },
    StaticValue { t: PhysicalType, id: StaticValueId },
    FunctionAddr(FunctionId),
    FnParam { t: PhysicalType, index: u32 },
    Data32 { t: ScalarType, data: u32 },
    Empty,
}

enum PendingPatch {
    Jump { pc: usize, target: BlockId },
    JumpIf { pc: usize, cons: BlockId, alt: BlockId },
}

pub fn get_or_compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> K1Result<BcUnit> {
    if let Some(unit) = k1.bc.functions.get(&function_id) {
        return Ok(unit.clone());
    }
    compile_function(k1, function_id)?;
    Ok(k1.bc.functions.get(&function_id).unwrap().clone())
}

pub fn get_or_compile_expr(k1: &mut TypedProgram, expr_id: TypedExprId) -> K1Result<BcUnit> {
    if let Some(unit) = k1.bc.exprs.get(&expr_id) {
        return Ok(unit.clone());
    }
    compile_expr(k1, expr_id)?;
    Ok(k1.bc.exprs.get(&expr_id).unwrap().clone())
}

pub fn compile_function(k1: &mut TypedProgram, function_id: FunctionId) -> K1Result<()> {
    let Some(unit) = k1.ir.functions.get(function_id) else {
        return Err(errf!(
            k1.get_function_span(function_id),
            "Cannot compile bytecode for uncompiled function"
        ));
    };
    let bc = lower_unit(k1, *unit)?;
    k1.bc.functions.insert(function_id, bc);
    Ok(())
}

pub fn compile_expr(k1: &mut TypedProgram, expr_id: TypedExprId) -> K1Result<()> {
    let Some(unit) = k1.ir.exprs.get(&expr_id).copied() else {
        return Err(errf!(
            k1.exprs.get_span(expr_id),
            "Cannot compile bytecode for uncompiled expr"
        ));
    };
    let bc = lower_unit(k1, unit)?;
    k1.bc.exprs.insert(expr_id, bc);
    Ok(())
}

fn lower_unit(k1: &TypedProgram, unit: IrUnit) -> K1Result<BcUnit> {
    let mut block_starts: FxHashMap<BlockId, usize> = FxHashMap::new();
    let mut block_indices: FxHashMap<BlockId, u32> = FxHashMap::new();
    let mut inst_slots: FxHashMap<InstId, u32> = FxHashMap::new();
    let mut next_slot = 0u32;
    let mut next_block_index = 0u32;

    for (block_id, block) in k1.ir.mem.dlist_iter_handles(unit.blocks) {
        block_indices.insert(block_id, next_block_index);
        next_block_index += 1;
        for inst_node in k1.ir.mem.dlist_iter_nodes(block.data.instrs) {
            let inst_id = inst_node.data;
            if let InstKind::Value(_) = ir::get_inst_kind(&k1.ir, &k1.types, inst_id) {
                inst_slots.insert(inst_id, next_slot);
                next_slot += 1;
            }
        }
    }

    let mut instrs = Vec::with_capacity(unit.inst_count as usize);
    let mut spans = Vec::with_capacity(unit.inst_count as usize);
    let mut patches = Vec::new();

    for (block_id, block) in k1.ir.mem.dlist_iter_handles(unit.blocks) {
        block_starts.insert(block_id, instrs.len());
        let block_index = block_indices[&block_id];
        for inst_node in k1.ir.mem.dlist_iter_nodes(block.data.instrs) {
            let inst_id = inst_node.data;
            let inst = *k1.ir.instrs.get(inst_id);
            let span = *k1.ir.sources.get(inst_id);
            let pc = instrs.len();
            let kind =
                lower_inst(k1, inst, inst_id, pc, &inst_slots, &block_indices, &mut patches)?;
            instrs.push(BcInst { block: block_index, kind });
            spans.push(span);
        }
    }

    for patch in patches {
        match patch {
            PendingPatch::Jump { pc, target } => {
                let offset = jump_offset(pc, block_starts[&target]);
                match &mut instrs[pc].kind {
                    BcInstKind::Jump { offset: o } => *o = offset,
                    _ => unreachable!(),
                }
            }
            PendingPatch::JumpIf { pc, cons, alt } => {
                let cons_offset = jump_offset(pc, block_starts[&cons]);
                let alt_offset = jump_offset(pc, block_starts[&alt]);
                match &mut instrs[pc].kind {
                    BcInstKind::JumpIf { cons_offset: c, alt_offset: a, .. } => {
                        *c = cons_offset;
                        *a = alt_offset;
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    Ok(BcUnit {
        result_type_id: unit.result_type_id,
        unit_id: unit.unit_id,
        return_pt: unit.fn_type.return_type,
        diverges: unit.fn_type.diverges,
        param_count: unit.fn_type.params.len(),
        local_count: next_slot,
        instrs,
        spans,
    })
}

fn jump_offset(from_pc: usize, to_pc: usize) -> i32 {
    let from = from_pc as isize;
    let to = to_pc as isize;
    (to - from) as i32
}

fn dst_slot(inst_id: InstId, inst_slots: &FxHashMap<InstId, u32>) -> u32 {
    inst_slots[&inst_id]
}

fn value(v: IrValue, inst_slots: &FxHashMap<InstId, u32>) -> BcValue {
    match v {
        IrValue::Inst(inst_id) => BcValue::Local(inst_slots[&inst_id]),
        IrValue::GlobalAddr { storage_pt, id } => BcValue::GlobalAddr { storage_pt, id },
        IrValue::StaticValue { t, id } => BcValue::StaticValue { t, id },
        IrValue::FunctionAddr(function_id) => BcValue::FunctionAddr(function_id),
        IrValue::FnParam { t, index } => BcValue::FnParam { t, index },
        IrValue::Data32 { t, data } => BcValue::Data32 { t, data },
        IrValue::Empty => BcValue::Empty,
    }
}

fn values(
    k1: &TypedProgram,
    args: MSlice<IrValue, ir::ProgramIr>,
    inst_slots: &FxHashMap<InstId, u32>,
) -> Vec<BcValue> {
    k1.ir.mem.getn(args).iter().map(|v| value(*v, inst_slots)).collect()
}

fn lower_inst(
    k1: &TypedProgram,
    inst: Inst,
    inst_id: InstId,
    pc: usize,
    inst_slots: &FxHashMap<InstId, u32>,
    block_indices: &FxHashMap<BlockId, u32>,
    patches: &mut Vec<PendingPatch>,
) -> K1Result<BcInstKind> {
    let v = |value_: IrValue| value(value_, inst_slots);
    let dst = || dst_slot(inst_id, inst_slots);
    let kind = match inst {
        Inst::Data(imm) => BcInstKind::Data { dst: dst(), imm },
        Inst::Alloca { vm_layout, .. } => BcInstKind::Alloca { dst: dst(), vm_layout },
        Inst::Store { dst, value, t } => BcInstKind::Store { dst: v(dst), value: v(value), t },
        Inst::Load { t, src } => BcInstKind::Load { dst: dst(), t, src: v(src) },
        Inst::Copy { dst, src, vm_size, .. } => {
            BcInstKind::Copy { dst: v(dst), src: v(src), vm_size }
        }
        Inst::StructOffset { base, vm_offset, .. } => {
            BcInstKind::StructOffset { dst: dst(), base: v(base), vm_offset }
        }
        Inst::ArrayOffset { element_t, base, element_index } => BcInstKind::ArrayOffset {
            dst: dst(),
            element_t,
            base: v(base),
            element_index: v(element_index),
        },
        Inst::Call { call_id } => {
            let call = k1.ir.calls.get(call_id);
            let callee = match call.callee {
                IrCallee::BackendBuiltin(_, builtin) => BcCallee::BackendBuiltin(builtin),
                IrCallee::Direct(function_id) => BcCallee::Direct(function_id),
                IrCallee::Indirect(_, callee_value) => BcCallee::Indirect(v(callee_value)),
                IrCallee::Extern { library_name, function_name, function_id } => {
                    BcCallee::Extern { library_name, function_name, function_id }
                }
            };
            let dst = inst_slots.get(&inst_id).copied();
            BcInstKind::Call {
                dst,
                ret_type: call.ret_type,
                callee,
                args: values(k1, call.args, inst_slots),
                ret_dst: call.dst.map(v),
            }
        }
        Inst::Jump(target) => {
            patches.push(PendingPatch::Jump { pc, target });
            BcInstKind::Jump { offset: 0 }
        }
        Inst::JumpIf { cond, cons, alt } => {
            patches.push(PendingPatch::JumpIf { pc, cons, alt });
            BcInstKind::JumpIf { cond: v(cond), cons_offset: 0, alt_offset: 0 }
        }
        Inst::Unreachable => BcInstKind::Unreachable,
        Inst::Phi { incomings, .. } => {
            let incomings = k1
                .ir
                .mem
                .getn(incomings)
                .iter()
                .map(|case| (block_indices[&case.from], v(case.value)))
                .collect();
            BcInstKind::Phi { dst: dst(), incomings }
        }
        Inst::Ret { v: ret, .. } => BcInstKind::Ret { v: v(ret) },
        Inst::BoolNegate { v: val } => BcInstKind::BoolNegate { dst: dst(), v: v(val) },
        Inst::BitNot { v: val } => BcInstKind::BitNot { dst: dst(), v: v(val) },
        Inst::BitCast { v: val, .. } => BcInstKind::BitCast { dst: dst(), v: v(val) },
        Inst::IntTrunc { v: val, to } => BcInstKind::IntTrunc { dst: dst(), v: v(val), to },
        Inst::IntExtU { v: val, .. } => BcInstKind::IntExtU { dst: dst(), v: v(val) },
        Inst::IntExtS { v: val, from, to } => {
            BcInstKind::IntExtS { dst: dst(), v: v(val), from, to }
        }
        Inst::FloatTrunc { v: val, .. } => BcInstKind::FloatTrunc { dst: dst(), v: v(val) },
        Inst::FloatExt { v: val, .. } => BcInstKind::FloatExt { dst: dst(), v: v(val) },
        Inst::Float32ToIntUnsigned { v: val, to } => {
            BcInstKind::Float32ToIntUnsigned { dst: dst(), v: v(val), to }
        }
        Inst::Float64ToIntUnsigned { v: val, to } => {
            BcInstKind::Float64ToIntUnsigned { dst: dst(), v: v(val), to }
        }
        Inst::Float32ToIntSigned { v: val, to } => {
            BcInstKind::Float32ToIntSigned { dst: dst(), v: v(val), to }
        }
        Inst::Float64ToIntSigned { v: val, to } => {
            BcInstKind::Float64ToIntSigned { dst: dst(), v: v(val), to }
        }
        Inst::IntToFloatUnsigned { v: val, from, to } => {
            BcInstKind::IntToFloatUnsigned { dst: dst(), v: v(val), from, to }
        }
        Inst::IntToFloatSigned { v: val, from, to } => {
            BcInstKind::IntToFloatSigned { dst: dst(), v: v(val), from, to }
        }
        Inst::PtrToWord { v: val } => BcInstKind::PtrToWord { dst: dst(), v: v(val) },
        Inst::WordToPtr { v: val } => BcInstKind::WordToPtr { dst: dst(), v: v(val) },
        Inst::IntAdd { lhs, rhs, width } => {
            BcInstKind::IntAdd { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntSub { lhs, rhs, width } => {
            BcInstKind::IntSub { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntMul { lhs, rhs, width } => {
            BcInstKind::IntMul { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntDivUnsigned { lhs, rhs, width } => {
            BcInstKind::IntDivUnsigned { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntDivSigned { lhs, rhs, width } => {
            BcInstKind::IntDivSigned { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntRemUnsigned { lhs, rhs, width } => {
            BcInstKind::IntRemUnsigned { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntRemSigned { lhs, rhs, width } => {
            BcInstKind::IntRemSigned { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::IntCmp { lhs, rhs, pred, width } => {
            BcInstKind::IntCmp { dst: dst(), lhs: v(lhs), rhs: v(rhs), pred, width }
        }
        Inst::FloatAdd { lhs, rhs, width } => {
            BcInstKind::FloatAdd { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::FloatSub { lhs, rhs, width } => {
            BcInstKind::FloatSub { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::FloatMul { lhs, rhs, width } => {
            BcInstKind::FloatMul { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::FloatDiv { lhs, rhs, width } => {
            BcInstKind::FloatDiv { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::FloatRem { lhs, rhs, width } => {
            BcInstKind::FloatRem { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::FloatCmp { lhs, rhs, pred, width } => {
            BcInstKind::FloatCmp { dst: dst(), lhs: v(lhs), rhs: v(rhs), pred, width }
        }
        Inst::BitAnd { lhs, rhs, width } => {
            BcInstKind::BitAnd { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BitOr { lhs, rhs, width } => {
            BcInstKind::BitOr { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BitXor { lhs, rhs, width } => {
            BcInstKind::BitXor { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BitShiftLeft { lhs, rhs, width } => {
            BcInstKind::BitShiftLeft { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BitUnsignedShiftRight { lhs, rhs, width } => {
            BcInstKind::BitUnsignedShiftRight { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BitSignedShiftRight { lhs, rhs, width } => {
            BcInstKind::BitSignedShiftRight { dst: dst(), lhs: v(lhs), rhs: v(rhs), width }
        }
        Inst::BakeStaticValue { type_id, value } => {
            BcInstKind::BakeStaticValue { dst: dst(), type_id, value: v(value) }
        }
    };

    Ok(kind)
}
