use ahash::HashMapExt;
use fxhash::FxHashMap;

use crate::ir::{
    self, BackendBuiltin, BlockId, FloatCmpPred, Inst, InstId, InstKind, IntCmpPred, IrCallee,
    IrUnit, IrUnitId, PhysicalFunctionType, Value as IrValue,
};
use crate::lex::SpanId;
use crate::parse;
use crate::typer::types::{Layout, PhysicalType, ScalarType, TypeId};
use crate::typer::{FunctionId, K1Result, StaticValueId, TypedExprId, TypedGlobalId, TypedProgram};
use crate::{errf, kmem::MSlice, nz_u32_id};

nz_u32_id!(BcFunctionId);

#[derive(Clone, Default)]
pub struct ProgramBc {
    pub functions: Vec<BcFunction>,
    pub function_indexes: FxHashMap<FunctionId, BcFunctionId>,
    pub expr_indexes: FxHashMap<TypedExprId, BcFunctionId>,
    pub function_ref_table: Vec<Option<BcFunctionId>>,
}

impl ProgramBc {
    pub fn make() -> Self {
        Self {
            functions: Vec::new(),
            function_indexes: FxHashMap::new(),
            expr_indexes: FxHashMap::new(),
            function_ref_table: Vec::new(),
        }
    }

    pub fn get_function(&self, id: BcFunctionId) -> &BcFunction {
        &self.functions[bc_index(id)]
    }

    pub fn function_for_ref_bits(&self, function_id_bits: u64) -> Option<BcFunctionId> {
        self.function_ref_table.get(function_id_bits as usize).copied().flatten()
    }
}

fn bc_index(id: BcFunctionId) -> usize {
    id.as_u32() as usize - 1
}

#[derive(Clone)]
pub struct BcFunction {
    pub unit_id: IrUnitId,
    pub result_type_id: TypeId,
    pub return_pt: PhysicalType,
    pub return_layout: Layout,
    pub diverges: bool,
    pub frame_size: u32,
    pub frame_align: u32,
    pub param_offsets: Vec<FrameOffset>,
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
    Data { dst: FrameOffset, imm: ir::DataInst },
    Store { dst: BcValue, value: BcValue, t: ScalarType },
    Load { dst: FrameOffset, t: ScalarType, src: BcValue },
    Copy { dst: BcValue, src: BcValue, size: u32 },
    StructOffset { dst: FrameOffset, base: BcValue, offset: u32 },
    ArrayOffset { dst: FrameOffset, element_stride: u32, base: BcValue, element_index: BcValue },
    CallDirect {
        result: Option<FrameOffset>,
        callee: BcFunctionId,
        args: Vec<BcValue>,
        ret: BcReturn,
        next_pc: usize,
    },
    CallIndirect {
        result: Option<FrameOffset>,
        callee: BcValue,
        args: Vec<BcValue>,
        ret: BcReturn,
        next_pc: usize,
    },
    CallExtern {
        result: Option<FrameOffset>,
        function_id: FunctionId,
        library_name: Option<parse::StringId>,
        function_name: parse::StringId,
        fn_type: PhysicalFunctionType,
        param_pts: Vec<PhysicalType>,
        args: Vec<BcValue>,
        ret: BcReturn,
        next_pc: usize,
    },
    CallBuiltin {
        result: Option<FrameOffset>,
        builtin: BackendBuiltin,
        args: Vec<BcValue>,
        ret: BcReturn,
        next_pc: usize,
    },
    Jump { offset: i32 },
    JumpIf { cond: BcValue, cons_offset: i32, alt_offset: i32 },
    Unreachable,
    Phi { dst: BcValue, size: u32, incomings: Vec<(u32, BcValue)> },
    Ret { v: BcValue },
    BoolNegate { dst: FrameOffset, v: BcValue },
    BitNot { dst: FrameOffset, v: BcValue },
    BitCast { dst: FrameOffset, v: BcValue },
    IntTrunc { dst: FrameOffset, v: BcValue, to: ScalarType },
    IntExtU { dst: FrameOffset, v: BcValue },
    IntExtS { dst: FrameOffset, v: BcValue, from: ScalarType, to: ScalarType },
    FloatTrunc { dst: FrameOffset, v: BcValue },
    FloatExt { dst: FrameOffset, v: BcValue },
    Float32ToIntUnsigned { dst: FrameOffset, v: BcValue, to: ScalarType },
    Float64ToIntUnsigned { dst: FrameOffset, v: BcValue, to: ScalarType },
    Float32ToIntSigned { dst: FrameOffset, v: BcValue, to: ScalarType },
    Float64ToIntSigned { dst: FrameOffset, v: BcValue, to: ScalarType },
    IntToFloatUnsigned { dst: FrameOffset, v: BcValue, from: ScalarType, to: ScalarType },
    IntToFloatSigned { dst: FrameOffset, v: BcValue, from: ScalarType, to: ScalarType },
    PtrToWord { dst: FrameOffset, v: BcValue },
    WordToPtr { dst: FrameOffset, v: BcValue },
    IntAdd { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntSub { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntMul { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntDivUnsigned { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntDivSigned { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntRemUnsigned { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntRemSigned { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    IntCmp { dst: FrameOffset, lhs: BcValue, rhs: BcValue, pred: IntCmpPred, width: u8 },
    FloatAdd { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    FloatSub { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    FloatMul { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    FloatDiv { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    FloatRem { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    FloatCmp { dst: FrameOffset, lhs: BcValue, rhs: BcValue, pred: FloatCmpPred, width: u8 },
    BitAnd { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BitOr { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BitXor { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BitShiftLeft { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BitUnsignedShiftRight { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BitSignedShiftRight { dst: FrameOffset, lhs: BcValue, rhs: BcValue, width: u8 },
    BakeStaticValue { dst: FrameOffset, type_id: TypeId, value: BcValue },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FrameOffset(pub u32);

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BcValue {
    Slot(FrameOffset),
    Address(FrameOffset),
    GlobalAddr { storage_pt: PhysicalType, layout: Layout, id: TypedGlobalId },
    StaticValue { t: PhysicalType, id: StaticValueId },
    FunctionAddr(FunctionId),
    Data32 { t: ScalarType, data: u32 },
    Empty,
}

#[derive(Clone, Copy)]
pub struct BcReturn {
    pub pt: PhysicalType,
    pub layout: Layout,
    pub dst: Option<BcValue>,
    pub mode: BcReturnMode,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BcReturnMode {
    None,
    Scalar,
    InMemory,
}

enum PendingPatch {
    Jump { pc: usize, target: BlockId },
    JumpIf { pc: usize, cons: BlockId, alt: BlockId },
}

pub fn compile_expr_program(
    k1: &TypedProgram,
    expr_id: TypedExprId,
    reachable_functions: &FxHashMap<FunctionId, ()>,
) -> K1Result<ProgramBc> {
    let Some(expr_unit) = k1.ir.exprs.get(&expr_id).copied() else {
        return Err(errf!(
            k1.exprs.get_span(expr_id),
            "Cannot compile bytecode for uncompiled expr"
        ));
    };
    compile_program(k1, Some((expr_id, expr_unit)), None, reachable_functions)
}

pub fn compile_function_program(
    k1: &TypedProgram,
    function_id: FunctionId,
    reachable_functions: &FxHashMap<FunctionId, ()>,
) -> K1Result<ProgramBc> {
    let Some(function_unit) = *k1.ir.functions.get(function_id) else {
        return Err(errf!(
            k1.get_function_span(function_id),
            "Cannot compile bytecode for uncompiled function"
        ));
    };
    compile_program(k1, None, Some((function_id, function_unit)), reachable_functions)
}

pub fn close_ir_for_expr_execution(
    k1: &mut TypedProgram,
    expr_id: TypedExprId,
) -> K1Result<FxHashMap<FunctionId, ()>> {
    let Some(expr_unit) = k1.ir.exprs.get(&expr_id).copied() else {
        return Err(errf!(
            k1.exprs.get_span(expr_id),
            "Cannot close IR for uncompiled expr"
        ));
    };
    close_ir_for_execution(k1, vec![expr_unit], k1.exprs.get_span(expr_id))
}

pub fn close_ir_for_function_execution(
    k1: &mut TypedProgram,
    function_id: FunctionId,
) -> K1Result<FxHashMap<FunctionId, ()>> {
    let Some(function_unit) = *k1.ir.functions.get(function_id) else {
        return Err(errf!(
            k1.get_function_span(function_id),
            "Cannot close IR for uncompiled function"
        ));
    };
    let mut reachable = close_ir_for_execution(k1, vec![function_unit], k1.get_function_span(function_id))?;
    reachable.insert(function_id, ());
    Ok(reachable)
}

fn close_ir_for_execution(
    k1: &mut TypedProgram,
    mut units_to_scan: Vec<IrUnit>,
    span: SpanId,
) -> K1Result<FxHashMap<FunctionId, ()>> {
    k1.compile_all_pending_ir(span)?;
    let mut seen_functions: FxHashMap<FunctionId, ()> = FxHashMap::new();

    while let Some(unit) = units_to_scan.pop() {
        for (function_id, ref_span) in collect_unit_function_refs(k1, unit) {
            if seen_functions.contains_key(&function_id) {
                continue;
            }
            seen_functions.insert(function_id, ());

            if k1.ir.functions.get(function_id).is_none() {
                if !k1.ir.units_pending_compile.contains(&function_id) {
                    k1.ir.units_pending_compile.push(function_id);
                }
                k1.compile_all_pending_ir(if ref_span == SpanId::NONE { span } else { ref_span })?;
            }

            let Some(function_unit) = *k1.ir.functions.get(function_id) else {
                return Err(errf!(
                    if ref_span == SpanId::NONE { span } else { ref_span },
                    "Function was not compiled for bytecode execution: {}",
                    k1.function_id_to_string(function_id, false)
                ));
            };
            units_to_scan.push(function_unit);
        }
    }

    Ok(seen_functions)
}

fn collect_unit_function_refs(k1: &TypedProgram, unit: IrUnit) -> Vec<(FunctionId, SpanId)> {
    let mut refs = Vec::new();
    for (_, block) in k1.ir.mem.dlist_iter_handles(unit.blocks) {
        for inst_node in k1.ir.mem.dlist_iter_nodes(block.data.instrs) {
            let inst_id = inst_node.data;
            let span = *k1.ir.sources.get(inst_id);
            let inst = *k1.ir.instrs.get(inst_id);
            collect_inst_function_refs(k1, inst, span, &mut refs);
        }
    }
    refs.sort_by_key(|(function_id, _)| function_id.as_u32());
    refs.dedup_by_key(|(function_id, _)| *function_id);
    refs
}

fn collect_value_function_ref(v: IrValue, span: SpanId, refs: &mut Vec<(FunctionId, SpanId)>) {
    if let IrValue::FunctionAddr(function_id) = v {
        refs.push((function_id, span));
    }
}

fn collect_inst_function_refs(
    k1: &TypedProgram,
    inst: Inst,
    span: SpanId,
    refs: &mut Vec<(FunctionId, SpanId)>,
) {
    match inst {
        Inst::Data(_) | Inst::Alloca { .. } | Inst::Jump(_) | Inst::Unreachable => {}
        Inst::Store { dst, value, .. } => {
            collect_value_function_ref(dst, span, refs);
            collect_value_function_ref(value, span, refs);
        }
        Inst::Load { src, .. } => collect_value_function_ref(src, span, refs),
        Inst::Copy { dst, src, .. } => {
            collect_value_function_ref(dst, span, refs);
            collect_value_function_ref(src, span, refs);
        }
        Inst::StructOffset { base, .. } => collect_value_function_ref(base, span, refs),
        Inst::ArrayOffset { base, element_index, .. } => {
            collect_value_function_ref(base, span, refs);
            collect_value_function_ref(element_index, span, refs);
        }
        Inst::Call { call_id } => {
            let call = k1.ir.calls.get(call_id);
            match call.callee {
                IrCallee::Direct(function_id) => refs.push((function_id, span)),
                IrCallee::Indirect(_, callee) => collect_value_function_ref(callee, span, refs),
                IrCallee::BackendBuiltin(_, _) | IrCallee::Extern { .. } => {}
            }
            for arg in k1.ir.mem.getn(call.args) {
                collect_value_function_ref(*arg, span, refs);
            }
            if let Some(dst) = call.dst {
                collect_value_function_ref(dst, span, refs);
            }
        }
        Inst::JumpIf { cond, .. } => collect_value_function_ref(cond, span, refs),
        Inst::Phi { incomings, .. } => {
            for incoming in k1.ir.mem.getn(incomings) {
                collect_value_function_ref(incoming.value, span, refs);
            }
        }
        Inst::Ret { v, .. }
        | Inst::BoolNegate { v }
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
        | Inst::WordToPtr { v }
        | Inst::BakeStaticValue { value: v, .. } => collect_value_function_ref(v, span, refs),
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
            collect_value_function_ref(lhs, span, refs);
            collect_value_function_ref(rhs, span, refs);
        }
    }
}

fn compile_program(
    k1: &TypedProgram,
    expr_entry: Option<(TypedExprId, IrUnit)>,
    function_entry: Option<(FunctionId, IrUnit)>,
    reachable_functions: &FxHashMap<FunctionId, ()>,
) -> K1Result<ProgramBc> {
    let mut program = ProgramBc::make();
    let mut functions = Vec::new();
    for (function_id, unit) in k1.ir.functions.iter_with_ids() {
        if !reachable_functions.contains_key(&function_id) {
            continue;
        }
        if let Some(unit) = *unit {
            let bc_id = BcFunctionId::from_u32(program.functions.len() as u32 + 1).unwrap();
            program.function_indexes.insert(function_id, bc_id);
            functions.push((function_id, unit));
            program.functions.push(empty_function(IrUnitId::Function(function_id)));
        }
    }

    if let Some((function_id, unit)) = function_entry
        && !program.function_indexes.contains_key(&function_id)
    {
        let bc_id = BcFunctionId::from_u32(program.functions.len() as u32 + 1).unwrap();
        program.function_indexes.insert(function_id, bc_id);
        functions.push((function_id, unit));
        program.functions.push(empty_function(IrUnitId::Function(function_id)));
    }

    if let Some((expr_id, _)) = expr_entry {
        let bc_id = BcFunctionId::from_u32(program.functions.len() as u32 + 1).unwrap();
        program.expr_indexes.insert(expr_id, bc_id);
        program.functions.push(empty_function(IrUnitId::Expr(expr_id)));
    }

    let max_function_id =
        program.function_indexes.keys().map(|id| id.as_u32() as usize).max().unwrap_or(0);
    program.function_ref_table.resize(max_function_id + 1, None);
    for (function_id, bc_id) in &program.function_indexes {
        program.function_ref_table[function_id.as_u32() as usize] = Some(*bc_id);
    }

    for (function_id, unit) in functions {
        let bc_id = program.function_indexes[&function_id];
        program.functions[bc_index(bc_id)] = lower_unit(k1, &program, unit)?;
    }
    if let Some((expr_id, unit)) = expr_entry {
        let bc_id = program.expr_indexes[&expr_id];
        program.functions[bc_index(bc_id)] = lower_unit(k1, &program, unit)?;
    }

    Ok(program)
}

fn empty_function(unit_id: IrUnitId) -> BcFunction {
    BcFunction {
        unit_id,
        result_type_id: TypeId::PENDING,
        return_pt: PhysicalType::EMPTY,
        return_layout: Layout::ZERO_SIZED,
        diverges: false,
        frame_size: 0,
        frame_align: 1,
        param_offsets: vec![],
        instrs: vec![],
        spans: vec![],
    }
}

fn lower_unit(k1: &TypedProgram, program: &ProgramBc, unit: IrUnit) -> K1Result<BcFunction> {
    let mut block_starts: FxHashMap<BlockId, usize> = FxHashMap::new();
    let mut block_indices: FxHashMap<BlockId, u32> = FxHashMap::new();
    let mut inst_values: FxHashMap<InstId, BcValue> = FxHashMap::new();
    let mut frame = FrameLayout::new();
    let mut param_offsets = Vec::with_capacity(unit.fn_type.params.len() as usize);

    for _ in k1.ir.mem.getn(unit.fn_type.params) {
        param_offsets.push(frame.alloc_value_slot());
    }

    for (block_id, block) in k1.ir.mem.dlist_iter_handles(unit.blocks) {
        block_indices.insert(block_id, block_indices.len() as u32);
        for inst_node in k1.ir.mem.dlist_iter_nodes(block.data.instrs) {
            let inst_id = inst_node.data;
            match *k1.ir.instrs.get(inst_id) {
                Inst::Alloca { vm_layout, .. } => {
                    let offset = frame.alloc(vm_layout);
                    inst_values.insert(inst_id, BcValue::Address(offset));
                }
                Inst::Call { call_id } if k1.ir.calls.get(call_id).dst.is_some() => {
                    inst_values.insert(inst_id, BcValue::Slot(frame.alloc_value_slot()));
                }
                _ => {
                    if let InstKind::Value(pt) = ir::get_inst_kind(&k1.ir, &k1.types, inst_id) {
                        if pt.is_empty() {
                            inst_values.insert(inst_id, BcValue::Empty);
                        } else if pt.is_agg() {
                            let offset = frame.alloc(k1.types.get_pt_layout(pt));
                            inst_values.insert(inst_id, BcValue::Address(offset));
                        } else {
                            inst_values.insert(inst_id, BcValue::Slot(frame.alloc_value_slot()));
                        }
                    }
                }
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
            if matches!(inst, Inst::Alloca { .. }) {
                continue;
            }
            let span = *k1.ir.sources.get(inst_id);
            let pc = instrs.len();
            let kind = lower_inst(
                k1,
                program,
                inst,
                inst_id,
                pc,
                &inst_values,
                &param_offsets,
                &block_indices,
                &mut patches,
            )?;
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

    let return_layout = k1.types.get_pt_layout(unit.fn_type.return_type);
    Ok(BcFunction {
        unit_id: unit.unit_id,
        result_type_id: unit.result_type_id,
        return_pt: unit.fn_type.return_type,
        return_layout,
        diverges: unit.fn_type.diverges,
        frame_size: frame.finish_size(),
        frame_align: frame.align,
        param_offsets,
        instrs,
        spans,
    })
}

fn jump_offset(from_pc: usize, to_pc: usize) -> i32 {
    let from = from_pc as isize;
    let to = to_pc as isize;
    (to - from) as i32
}

fn lower_inst(
    k1: &TypedProgram,
    program: &ProgramBc,
    inst: Inst,
    inst_id: InstId,
    pc: usize,
    inst_values: &FxHashMap<InstId, BcValue>,
    param_offsets: &[FrameOffset],
    block_indices: &FxHashMap<BlockId, u32>,
    patches: &mut Vec<PendingPatch>,
) -> K1Result<BcInstKind> {
    let v = |value_: IrValue| value(k1, value_, inst_values, param_offsets);
    let dst = || frame_slot(inst_values[&inst_id]);
    let kind = match inst {
        Inst::Alloca { .. } => unreachable!(),
        Inst::Data(imm) => BcInstKind::Data { dst: dst(), imm },
        Inst::Store { dst, value, t } => BcInstKind::Store { dst: v(dst), value: v(value), t },
        Inst::Load { t, src } => BcInstKind::Load { dst: dst(), t, src: v(src) },
        Inst::Copy { dst, src, vm_size, .. } => {
            BcInstKind::Copy { dst: v(dst), src: v(src), size: vm_size }
        }
        Inst::StructOffset { base, vm_offset, .. } => {
            BcInstKind::StructOffset { dst: dst(), base: v(base), offset: vm_offset }
        }
        Inst::ArrayOffset { element_t, base, element_index } => {
            let element_stride = k1.types.get_pt_layout(element_t).stride();
            BcInstKind::ArrayOffset {
                dst: dst(),
                element_stride,
                base: v(base),
                element_index: v(element_index),
            }
        }
        Inst::Call { call_id } => {
            let call = k1.ir.calls.get(call_id);
            let args = values(k1, call.args, inst_values, param_offsets);
            let result = inst_values.get(&inst_id).and_then(|v| v.as_slot());
            let ret_dst = call.dst.map(|dst| v(dst)).or_else(|| match return_mode(call.ret_type) {
                BcReturnMode::None => None,
                _ => inst_values.get(&inst_id).copied(),
            });
            let ret = BcReturn {
                pt: call.ret_type,
                layout: k1.types.get_pt_layout(call.ret_type),
                dst: ret_dst,
                mode: return_mode(call.ret_type),
            };
            let next_pc = pc + 1;
            match call.callee {
                IrCallee::BackendBuiltin(_, builtin) => {
                    BcInstKind::CallBuiltin { result, builtin, args, ret, next_pc }
                }
                IrCallee::Direct(function_id) => {
                    let Some(callee) = program.function_indexes.get(&function_id).copied() else {
                        return Err(errf!(
                            *k1.ir.sources.get(inst_id),
                            "Direct callee was not present in closed bytecode program: {}",
                            k1.function_id_to_string(function_id, false)
                        ));
                    };
                    BcInstKind::CallDirect { result, callee, args, ret, next_pc }
                }
                IrCallee::Indirect(_, callee_value) => {
                    BcInstKind::CallIndirect {
                        result,
                        callee: v(callee_value),
                        args,
                        ret,
                        next_pc,
                    }
                }
                IrCallee::Extern { library_name, function_name, function_id } => {
                    let fn_type = k1.ir.functions.get(function_id).unwrap().fn_type;
                    let param_pts =
                        k1.ir.mem.getn(fn_type.params).iter().map(|p| p.pt).collect();
                    BcInstKind::CallExtern {
                        result,
                        function_id,
                        library_name,
                        function_name,
                        fn_type,
                        param_pts,
                        args,
                        ret,
                        next_pc,
                    }
                }
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
            let dst = inst_values[&inst_id];
            let size = match ir::get_inst_kind(&k1.ir, &k1.types, inst_id) {
                InstKind::Value(pt) if pt.is_agg() => k1.types.get_pt_layout(pt).size,
                _ => 0,
            };
            BcInstKind::Phi { dst, size, incomings }
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

fn return_mode(pt: PhysicalType) -> BcReturnMode {
    if pt.is_empty() {
        BcReturnMode::None
    } else if pt.is_agg() {
        BcReturnMode::InMemory
    } else {
        BcReturnMode::Scalar
    }
}

fn values(
    k1: &TypedProgram,
    args: MSlice<IrValue, ir::ProgramIr>,
    inst_values: &FxHashMap<InstId, BcValue>,
    param_offsets: &[FrameOffset],
) -> Vec<BcValue> {
    k1.ir.mem.getn(args).iter().map(|v| value(k1, *v, inst_values, param_offsets)).collect()
}

fn value(
    k1: &TypedProgram,
    v: IrValue,
    inst_values: &FxHashMap<InstId, BcValue>,
    param_offsets: &[FrameOffset],
) -> BcValue {
    match v {
        IrValue::Inst(inst_id) => inst_values[&inst_id],
        IrValue::GlobalAddr { storage_pt, id } => {
            BcValue::GlobalAddr { storage_pt, layout: k1.types.get_pt_layout(storage_pt), id }
        }
        IrValue::StaticValue { t, id } => BcValue::StaticValue { t, id },
        IrValue::FunctionAddr(function_id) => BcValue::FunctionAddr(function_id),
        IrValue::FnParam { index, .. } => BcValue::Slot(param_offsets[index as usize]),
        IrValue::Data32 { t, data } => BcValue::Data32 { t, data },
        IrValue::Empty => BcValue::Empty,
    }
}

fn frame_slot(value: BcValue) -> FrameOffset {
    value.as_slot().unwrap_or_else(|| panic!("Expected frame slot"))
}

impl BcValue {
    pub fn as_slot(self) -> Option<FrameOffset> {
        match self {
            BcValue::Slot(offset) => Some(offset),
            _ => None,
        }
    }
}

struct FrameLayout {
    size: u32,
    align: u32,
}

impl FrameLayout {
    fn new() -> Self {
        Self { size: 0, align: 1 }
    }

    fn alloc_value_slot(&mut self) -> FrameOffset {
        self.alloc(Layout { size: 8, align: 8 })
    }

    fn alloc(&mut self, layout: Layout) -> FrameOffset {
        let offset = if layout.size == 0 {
            self.size
        } else {
            self.size.next_multiple_of(layout.align)
        };
        self.size = offset + layout.size;
        self.align = self.align.max(layout.align);
        FrameOffset(offset)
    }

    fn finish_size(&self) -> u32 {
        self.size.next_multiple_of(self.align)
    }
}
