// Copyright (c) 2026 knix
// All rights reserved.

pub mod disasm;
pub mod exec;
pub mod lower;

use fxhash::{FxHashMap, FxHashSet};

use crate::ir::IrUnitId;
use crate::lex::SpanId;
use crate::typer::types::PhysicalType;
use crate::typer::{FunctionId, TypedExprId};
use crate::vpool::VVec;

/// Marks a bc value as an index into the constant pool
pub const VALUE_MASK_CONST_ID: u32 = 0x8000_0000;

/// Marks a bc value as a frame offset
pub const VALUE_MASK_FRAME_OFFSET: u32 = 0x4000_0000;

/// Frame header size in words: [caller_fp][return_pc][sret_addr]
pub const FRAME_HEADER_WORDS: u32 = 3;

/// Frame bases and sizes are aligned to this, allowing allocas up to the
/// widest vector's natural alignment (512-bit)
pub const FRAME_ALIGN: u32 = 64;

pub const PENDING_PC: u32 = u32::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    // Control
    Halt = 0,
    Enter,
    Jump,
    JumpIf,
    /// Fused compare-and-branch: A = width in bits, B = IntCmpPred tag
    JumpIfIntCmp,
    Unreachable,
    Ret,
    RetAgg,
    Call,
    CallIndirect,
    CallExtern,
    CallBuiltin,
    CallLlvm,
    RetGet,
    RetStore,
    // Memory / data
    Mov,
    LoadGlobal,
    Load,
    Store,
    Copy,
    PtrAddImm,
    PtrIndex,
    // Int ALU (A = width in bits)
    IntAdd,
    IntSub,
    IntMul,
    IntDivU,
    IntDivS,
    IntRemU,
    IntRemS,
    IntCmp, // B = IntCmpPred tag
    // Float ALU (A = width in bits)
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatRem,
    FloatCmp, // B = FloatCmpPred tag
    // Bit ALU (A = width in bits)
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    ShrU,
    ShrS,
    // Unary (A = width in bits for FloatNeg)
    BoolNegate,
    BitNot,
    FloatNeg,
    Cast, // A = CastKind, B = (from_bits << 8) | to_bits
    // Metaprogramming
    BakeStaticValue,
    // Atomics (A = width in bits; B packs ordering / op, see operand_count comments)
    AtomicLoad,
    AtomicStore,
    AtomicRmw,
    AtomicCmpxchg,
    Fence,
}

pub const OPCODE_COUNT: u8 = Opcode::Fence as u8 + 1;

impl Opcode {
    pub fn from_u8(v: u8) -> Opcode {
        debug_assert!(v < OPCODE_COUNT, "bad opcode {v}");
        unsafe { core::mem::transmute::<u8, Opcode>(v) }
    }

    /// Total operand words for an instruction with header B field `b`
    pub const fn operand_count_with(self, b: u16) -> usize {
        match self {
            Opcode::Call | Opcode::CallIndirect => self.operand_count() + b as usize,
            _ => self.operand_count(),
        }
    }

    pub const fn operand_count(self) -> usize {
        // TODO(perf): I think we could bake the operand count into the opcode bits.
        match self {
            Opcode::Halt => 0,
            Opcode::Enter => 1,        // [frame_bytes]
            Opcode::Jump => 1,         // [pc]
            Opcode::JumpIf => 3,       // [cond src][cons pc][alt pc]
            Opcode::JumpIfIntCmp => 4, // [lhs src][rhs src][cons pc][alt pc]
            Opcode::Unreachable => 0,
            Opcode::Ret => 1,    // [src]
            Opcode::RetAgg => 2, // [src][size]
            // Call forms carry B = nargs additional arg-src operands beyond
            // the base count here (see operand_count_with)
            Opcode::Call => 3, // [target pc][fp_delta bytes][sret src] + args
            Opcode::CallIndirect => 3, // [fn src][fp_delta][sret src] + args
            Opcode::CallExtern => 6, // [function_id][lib_name(0=none)][fn_name][ret_pt][fp_delta][nargs]
            Opcode::CallBuiltin => 3, // A = builtin tag; [ret_pt][fp_delta][nargs]
            Opcode::CallLlvm => 4,   // [fn_name + 1][ret_pt][fp_delta][nargs]
            Opcode::RetGet => 1,     // [dst]
            Opcode::RetStore => 1,   // A = width in bits; [addr src]
            Opcode::Mov => 2,        // [dst][src]
            Opcode::LoadGlobal => 3, // [dst][global_id][storage_pt]
            Opcode::Load => 2,       // A = width in bits; [dst][addr src]
            Opcode::Store => 2,      // A = width in bits; [addr src][val src]
            Opcode::Copy => 3,       // [dst src][src src][size]
            Opcode::PtrAddImm => 3,  // [dst][base src][byte offset]
            Opcode::PtrIndex => 4,   // [dst][base src][index src][stride]
            Opcode::IntAdd
            | Opcode::IntSub
            | Opcode::IntMul
            | Opcode::IntDivU
            | Opcode::IntDivS
            | Opcode::IntRemU
            | Opcode::IntRemS
            | Opcode::IntCmp
            | Opcode::FloatAdd
            | Opcode::FloatSub
            | Opcode::FloatMul
            | Opcode::FloatDiv
            | Opcode::FloatRem
            | Opcode::FloatCmp
            | Opcode::BitAnd
            | Opcode::BitOr
            | Opcode::BitXor
            | Opcode::Shl
            | Opcode::ShrU
            | Opcode::ShrS => 3, // [dst][lhs][rhs]
            Opcode::BoolNegate | Opcode::BitNot | Opcode::FloatNeg => 2, // [dst][src]
            Opcode::Cast => 2,       // A = CastKind, B = from_bits << 8 | to_bits; [dst][src]
            Opcode::BakeStaticValue => 3, // [dst][type_id][src]
            Opcode::AtomicLoad => 2, // A = width in bits, B = ordering; [dst][addr src]
            Opcode::AtomicStore => 2, // A = width in bits, B = ordering; [addr src][val src]
            Opcode::AtomicRmw => 3, // A = width in bits, B = op << 8 | ordering; [dst][addr src][operand src]
            // A = width in bits, B = success | failure << 4 | weak << 8;
            // [result src][addr src][expected src][desired src][ok byte offset]
            Opcode::AtomicCmpxchg => 5,
            Opcode::Fence => 0, // B = ordering
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Opcode::Halt => "halt",
            Opcode::Enter => "enter",
            Opcode::Jump => "jump",
            Opcode::JumpIf => "jump_if",
            Opcode::JumpIfIntCmp => "jump_if_icmp",
            Opcode::Unreachable => "unreachable",
            Opcode::Ret => "ret",
            Opcode::RetAgg => "ret_agg",
            Opcode::Call => "call",
            Opcode::CallIndirect => "call_indirect",
            Opcode::CallExtern => "call_extern",
            Opcode::CallBuiltin => "call_builtin",
            Opcode::CallLlvm => "call_llvm",
            Opcode::RetGet => "ret_get",
            Opcode::RetStore => "ret_store",
            Opcode::Mov => "mov",
            Opcode::LoadGlobal => "load_global",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Copy => "copy",
            Opcode::PtrAddImm => "ptr_add_imm",
            Opcode::PtrIndex => "ptr_index",
            Opcode::IntAdd => "iadd",
            Opcode::IntSub => "isub",
            Opcode::IntMul => "imul",
            Opcode::IntDivU => "idiv_u",
            Opcode::IntDivS => "idiv_s",
            Opcode::IntRemU => "irem_u",
            Opcode::IntRemS => "irem_s",
            Opcode::IntCmp => "icmp",
            Opcode::FloatAdd => "fadd",
            Opcode::FloatSub => "fsub",
            Opcode::FloatMul => "fmul",
            Opcode::FloatDiv => "fdiv",
            Opcode::FloatRem => "frem",
            Opcode::FloatCmp => "fcmp",
            Opcode::BitAnd => "and",
            Opcode::BitOr => "or",
            Opcode::BitXor => "xor",
            Opcode::Shl => "shl",
            Opcode::ShrU => "shr_u",
            Opcode::ShrS => "shr_s",
            Opcode::BoolNegate => "bool_neg",
            Opcode::BitNot => "bit_not",
            Opcode::FloatNeg => "fneg",
            Opcode::Cast => "cast",
            Opcode::BakeStaticValue => "bake_static",
            Opcode::AtomicLoad => "atomic_load",
            Opcode::AtomicStore => "atomic_store",
            Opcode::AtomicRmw => "atomic_rmw",
            Opcode::AtomicCmpxchg => "atomic_cmpxchg",
            Opcode::Fence => "fence",
        }
    }
}

/// Cast kinds for the `Cast` opcode. The `B` field of the header packs
/// `(from_width_bits << 8) | to_width_bits`; not all kinds use both.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CastKind {
    IntTrunc = 0,
    IntExtS,
    FloatTrunc,
    FloatExt,
    F32ToUInt,
    F32ToSInt,
    F64ToUInt,
    F64ToSInt,
    UIntToF32,
    UIntToF64,
    SIntToF32,
    SIntToF64,
}

impl CastKind {
    pub fn from_u8(v: u8) -> CastKind {
        debug_assert!(v <= CastKind::SIntToF64 as u8, "bad cast kind {v}");
        unsafe { core::mem::transmute::<u8, CastKind>(v) }
    }
}

pub fn builtin_tag(b: crate::ir::BackendBuiltin) -> u8 {
    b as u8
}

pub fn builtin_from_tag(t: u8) -> crate::ir::BackendBuiltin {
    use crate::ir::BackendBuiltin as B;
    match t {
        x if x == B::TypeSchema as u8 => B::TypeSchema,
        x if x == B::TypeName as u8 => B::TypeName,
        x if x == B::MakeStruct as u8 => B::MakeStruct,
        x if x == B::MakeEither as u8 => B::MakeEither,
        x if x == B::MakeReference as u8 => B::MakeReference,
        x if x == B::MakeArray as u8 => B::MakeArray,
        x if x == B::MakeFn as u8 => B::MakeFn,
        x if x == B::MemCopy as u8 => B::MemCopy,
        x if x == B::MemMove as u8 => B::MemMove,
        x if x == B::MemSet as u8 => B::MemSet,
        x if x == B::MemEquals as u8 => B::MemEquals,
        x if x == B::Exit as u8 => B::Exit,
        x if x == B::CompilerMessage as u8 => B::CompilerMessage,
        x if x == B::ReplCheckbox as u8 => B::ReplCheckbox,
        _ => unreachable!("bad builtin tag"),
    }
}

#[inline(always)]
pub const fn header(op: Opcode, a: u8, b: u16) -> u32 {
    op as u32 | ((a as u32) << 8) | ((b as u32) << 16)
}

#[inline(always)]
pub const fn header_op(h: u32) -> u8 {
    h as u8
}

#[inline(always)]
pub const fn header_a(h: u32) -> u8 {
    (h >> 8) as u8
}

#[inline(always)]
pub const fn header_b(h: u32) -> u16 {
    (h >> 16) as u16
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnitKind {
    /// Has a lowered body in the code stream
    Body,
    /// Externally linked; callable only through `CallExtern`
    Extern,
    /// Backend builtin; callable only through `CallBuiltin`
    Builtin,
}

#[derive(Debug, Clone, Copy)]
pub struct UnitInfo {
    pub kind: UnitKind,
    pub code_start: u32,
    pub frame_bytes: u32,
    pub param_count: u32,
    pub ret_pt: PhysicalType,
    pub diverges: bool,
}

pub struct BcProgram {
    /// The shared instruction stream for every lowered unit. `code[0]` is a
    /// lone `Halt`: popping the top frame sets `pc = 0` and lands on it.
    pub code: VVec<u32>,
    /// Program-wide constant pool; src operands with `VALUE_MASK_CONST_ID` index it.
    pub consts: VVec<u64>,
    const_dedup: FxHashMap<u64, u32>,

    pub functions: FxHashMap<FunctionId, UnitInfo>,
    pub exprs: FxHashMap<TypedExprId, UnitInfo>,

    /// Absolute code indices holding `PENDING_PC` call targets, waiting for
    /// the keyed function to finish lowering (recursion cycles).
    pub(crate) pending_call_fixups: FxHashMap<FunctionId, Vec<u32>>,
    pub(crate) in_progress: FxHashSet<FunctionId>,

    /// Sorted by start pc: (start, end, unit). For stack traces / diagnostics.
    pub unit_ranges: VVec<(u32, u32, IrUnitId)>,
    /// Sorted by pc: the span for all code from this pc until the next entry.
    /// Only consulted on slow paths and errors.
    pub spans: VVec<(u32, SpanId)>,

    /// Reusable lowering state; a stack because callee lowering recurses.
    /// Popped/pushed by `lower::lower_unit`, `reset()` between uses — so the
    /// per-unit collections are allocated once and reused for the whole
    /// program.
    pub(crate) lower_ctx_pool: Vec<lower::LowerCtx>,
}

impl BcProgram {
    pub fn make() -> BcProgram {
        let mut code = VVec::make("bc_code");
        code.push(header(Opcode::Halt, 0, 0));
        BcProgram {
            code,
            consts: VVec::make("bc_consts"),
            const_dedup: FxHashMap::default(),
            functions: FxHashMap::default(),
            exprs: FxHashMap::default(),
            pending_call_fixups: FxHashMap::default(),
            in_progress: FxHashSet::default(),
            unit_ranges: VVec::make("bc_unit_ranges"),
            spans: VVec::make("bc_spans"),
            lower_ctx_pool: Vec::new(),
        }
    }

    pub fn intern_const(&mut self, value: u64) -> u32 {
        if let Some(idx) = self.const_dedup.get(&value) {
            return *idx | VALUE_MASK_CONST_ID;
        }
        let idx = self.consts.len() as u32;
        debug_assert!(idx & VALUE_MASK_CONST_ID == 0, "constant pool overflow");
        self.consts.push(value);
        self.const_dedup.insert(value, idx);
        idx | VALUE_MASK_CONST_ID
    }

    pub fn get_unit_info(&self, unit_id: IrUnitId) -> Option<UnitInfo> {
        match unit_id {
            IrUnitId::Function(fid) => self.functions.get(&fid).copied(),
            IrUnitId::Expr(eid) => self.exprs.get(&eid).copied(),
        }
    }

    /// The unit whose code range contains `pc`, if any.
    pub fn unit_for_pc(&self, pc: u32) -> Option<IrUnitId> {
        let idx = self.unit_ranges.partition_point(|(start, _, _)| *start <= pc);
        if idx == 0 {
            return None;
        }
        let (start, end, unit) = self.unit_ranges[idx - 1];
        if pc >= start && pc < end { Some(unit) } else { None }
    }

    pub fn span_for_pc(&self, pc: u32) -> SpanId {
        let idx = self.spans.partition_point(|(span_pc, _)| *span_pc <= pc);
        if idx == 0 {
            return SpanId::NONE;
        }
        self.spans[idx - 1].1
    }
}

#[cfg(test)]
mod bc_test {
    use super::*;
    use crate::typer::FunctionId;

    #[test]
    fn header_roundtrip() {
        let h = header(Opcode::IntCmp, 32, 7);
        assert_eq!(header_op(h), Opcode::IntCmp as u8);
        assert_eq!(header_a(h), 32);
        assert_eq!(header_b(h), 7);
        let h2 = header(Opcode::BakeStaticValue, 255, u16::MAX);
        assert_eq!(header_op(h2), Opcode::BakeStaticValue as u8);
        assert_eq!(header_a(h2), 255);
        assert_eq!(header_b(h2), u16::MAX);
    }

    #[test]
    fn opcode_tags_roundtrip() {
        for i in 0..OPCODE_COUNT {
            let op = Opcode::from_u8(i);
            assert_eq!(op as u8, i);
            // Every opcode has a name and a fixed operand count
            assert!(!op.name().is_empty());
            assert!(op.operand_count() <= 6);
        }
    }

    #[test]
    fn pred_tags_roundtrip() {
        use crate::ir::{FloatCmpPred, IntCmpPred};
        for p in [
            IntCmpPred::Eq,
            IntCmpPred::Slt,
            IntCmpPred::Sle,
            IntCmpPred::Sgt,
            IntCmpPred::Sge,
            IntCmpPred::Ult,
            IntCmpPred::Ule,
            IntCmpPred::Ugt,
            IntCmpPred::Uge,
        ] {
            assert!(IntCmpPred::from_u8(p as u8) == p);
        }
        for p in [
            FloatCmpPred::Eq,
            FloatCmpPred::Lt,
            FloatCmpPred::Le,
            FloatCmpPred::Gt,
            FloatCmpPred::Ge,
        ] {
            assert!(FloatCmpPred::from_u8(p as u8) == p);
        }
    }

    #[test]
    fn const_pool_dedup() {
        let mut bc = BcProgram::make();
        let a = bc.intern_const(42);
        let b = bc.intern_const(42);
        let c = bc.intern_const(43);
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert!(a & VALUE_MASK_CONST_ID != 0);
        assert_eq!(bc.consts[(a & !VALUE_MASK_CONST_ID) as usize], 42);
        assert_eq!(bc.consts.len(), 2);
    }

    #[test]
    fn unit_and_span_lookup() {
        let mut bc = BcProgram::make();
        let f1 = IrUnitId::Function(FunctionId::ONE);
        let f2 = IrUnitId::Function(FunctionId::ONE.add_u32(1));
        bc.unit_ranges.push((1, 10, f1));
        bc.unit_ranges.push((10, 30, f2));
        assert!(bc.unit_for_pc(0).is_none());
        assert!(bc.unit_for_pc(1) == Some(f1));
        assert!(bc.unit_for_pc(9) == Some(f1));
        assert!(bc.unit_for_pc(10) == Some(f2));
        assert!(bc.unit_for_pc(29) == Some(f2));
        assert!(bc.unit_for_pc(30).is_none());
    }
}
