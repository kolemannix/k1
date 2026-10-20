// Copyright (c) 2026 knix
// All rights reserved.

//! Disassembler for the bc stream — used for `#debug` unit dumps, divergence
//! logs, and tests.

use std::fmt::Write;

use crate::ir;
use crate::typer::TypedProgram;

use super::{
    Opcode, VALUE_MASK_CONST_ID, VALUE_MASK_FRAME_OFFSET, builtin_from_tag, header_a, header_b,
    header_op,
};

fn write_src(w: &mut String, k1: &TypedProgram, src: u32) {
    if src & VALUE_MASK_CONST_ID != 0 {
        let idx = (src & !VALUE_MASK_CONST_ID) as usize;
        match k1.bc.consts.get(idx) {
            Some(v) => write!(w, "c[{}]={:#x}", idx, v).unwrap(),
            None => write!(w, "c[{}]=<oob>", idx).unwrap(),
        }
    } else if src & VALUE_MASK_FRAME_OFFSET != 0 {
        write!(w, "fp+{}", src & !VALUE_MASK_FRAME_OFFSET).unwrap();
    } else {
        write!(w, "f[{}]", src).unwrap();
    }
}

fn write_dst(w: &mut String, dst: u32) {
    write!(w, "f[{}]", dst).unwrap();
}

/// [sret src][arg srcs...] tail shared by call and call_indirect
fn write_call_tail(w: &mut String, k1: &TypedProgram, ops: &[u32]) {
    write!(w, " sret=").unwrap();
    write_src(w, k1, ops[2]);
    write!(w, " args(").unwrap();
    for (i, arg) in ops[3..].iter().enumerate() {
        if i > 0 {
            write!(w, ", ").unwrap();
        }
        write_src(w, k1, *arg);
    }
    write!(w, ")").unwrap();
}

pub fn disasm_range(k1: &TypedProgram, start: u32, end: u32) -> String {
    let mut s = String::new();
    let mut pc = start as usize;
    let end = (end as usize).min(k1.bc.code.len());
    while pc < end {
        let next = disasm_one(k1, &mut s, pc);
        writeln!(&mut s).unwrap();
        if next <= pc {
            break; // defensive: malformed stream
        }
        pc = next;
    }
    s
}

/// Renders one instruction at `pc`, returning the pc of the next instruction.
pub fn disasm_one(k1: &TypedProgram, w: &mut String, pc: usize) -> usize {
    let code = &k1.bc.code;
    let h = code[pc];
    let op = Opcode::from_u8(header_op(h));
    let a = header_a(h);
    let b = header_b(h);
    let n = op.operand_count_with(b);
    let ops: &[u32] = &code[pc + 1..pc + 1 + n];

    write!(w, "{:6} ", pc).unwrap();
    match op {
        Opcode::Halt | Opcode::Unreachable => {
            write!(w, "{}", op.name()).unwrap();
        }
        Opcode::Enter => {
            write!(w, "enter frame_bytes={}", ops[0]).unwrap();
        }
        Opcode::Jump => {
            write!(w, "jump @{}", ops[0]).unwrap();
        }
        Opcode::JumpIfEq
        | Opcode::JumpIfSlt
        | Opcode::JumpIfSle
        | Opcode::JumpIfSgt
        | Opcode::JumpIfSge
        | Opcode::JumpIfUlt
        | Opcode::JumpIfUle
        | Opcode::JumpIfUgt
        | Opcode::JumpIfUge
        | Opcode::JumpIfNe => {
            write!(w, "{}.{} ", op.name(), a).unwrap();
            write_src(w, k1, ops[0]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, " @{} @{}", ops[2], ops[3]).unwrap();
        }
        Opcode::JumpIf => {
            write!(w, "jump_if ").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " @{} @{}", ops[1], ops[2]).unwrap();
        }
        Opcode::Switch => {
            write!(w, "switch.{} ", a).unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " default @{}", ops[1]).unwrap();
            for i in 0..b as usize {
                let base = 2 + i * 3;
                let value = ops[base] as u64 | ((ops[base + 1] as u64) << 32);
                write!(w, " {}@{}", value, ops[base + 2]).unwrap();
            }
        }
        Opcode::Ret => {
            write!(w, "ret ").unwrap();
            write_src(w, k1, ops[0]);
        }
        Opcode::RetAgg => {
            write!(w, "ret_agg ").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " size={}", ops[1]).unwrap();
        }
        Opcode::Call => {
            write!(w, "call @{} fp+={}", ops[0], ops[1]).unwrap();
            write_call_tail(w, k1, ops);
        }
        Opcode::CallIndirect => {
            write!(w, "call_indirect ").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " fp+={}", ops[1]).unwrap();
            write_call_tail(w, k1, ops);
        }
        Opcode::CallExtern => {
            write!(
                w,
                "call_extern fn#{} ret_pt={} fp+={} nargs={}",
                ops[0], ops[3], ops[4], ops[5]
            )
            .unwrap();
        }
        Opcode::CallBuiltin => {
            write!(
                w,
                "call_builtin {} ret_pt={} fp+={} nargs={}",
                builtin_from_tag(a).kind_name(),
                ops[0],
                ops[1],
                ops[2]
            )
            .unwrap();
        }
        Opcode::CallLlvm => {
            write!(
                w,
                "call_llvm {} ret_pt={} fp+={} nargs={}",
                k1.ident_str(crate::parse::StringId::from_u32(ops[0]).unwrap()),
                ops[1],
                ops[2],
                ops[3]
            )
            .unwrap();
        }
        Opcode::RetGet => {
            write!(w, "ret_get ").unwrap();
            write_dst(w, ops[0]);
        }
        Opcode::RetStore => {
            write!(w, "ret_store.{} ", a).unwrap();
            write_src(w, k1, ops[0]);
        }
        Opcode::Mov => {
            write!(w, "mov ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::LoadGlobal => {
            write!(w, "load_global ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- global#{}", ops[1]).unwrap();
        }
        Opcode::Load8 | Opcode::Load16 | Opcode::Load32 | Opcode::Load64 => {
            write!(w, "{} ", op.name()).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- [").unwrap();
            write_src(w, k1, ops[1]);
            if b != 0 {
                write!(w, "+{}", b).unwrap();
            }
            write!(w, "]").unwrap();
        }
        Opcode::Store8 | Opcode::Store16 | Opcode::Store32 | Opcode::Store64 => {
            write!(w, "{} [", op.name()).unwrap();
            write_src(w, k1, ops[0]);
            if b != 0 {
                write!(w, "+{}", b).unwrap();
            }
            write!(w, "] <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::LoadFp8 | Opcode::LoadFp16 | Opcode::LoadFp32 | Opcode::LoadFp64 => {
            write!(w, "{} ", op.name()).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- [fp+{}]", ops[1]).unwrap();
        }
        Opcode::StoreFp8 | Opcode::StoreFp16 | Opcode::StoreFp32 | Opcode::StoreFp64 => {
            write!(w, "{} [fp+{}] <- ", op.name(), ops[0]).unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::AtomicLoad => {
            write!(w, "atomic_load.{} {} ", a, ir::AtomicOrderingIr::from_tag(b as u8).name())
                .unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- [").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, "]").unwrap();
        }
        Opcode::AtomicStore => {
            write!(w, "atomic_store.{} {} [", a, ir::AtomicOrderingIr::from_tag(b as u8).name())
                .unwrap();
            write_src(w, k1, ops[0]);
            write!(w, "] <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::AtomicRmw => {
            write!(
                w,
                "atomic_{}.{} {} ",
                ir::AtomicRmwOpIr::from_tag((b >> 8) as u8).name(),
                a,
                ir::AtomicOrderingIr::from_tag((b & 0xff) as u8).name()
            )
            .unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- [").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, "], ").unwrap();
            write_src(w, k1, ops[2]);
        }
        Opcode::AtomicCmpxchg => {
            write!(
                w,
                "atomic_cmpxchg{}.{} {}/{} result [",
                if b >> 8 & 1 == 1 { "_weak" } else { "" },
                a,
                ir::AtomicOrderingIr::from_tag((b & 0xf) as u8).name(),
                ir::AtomicOrderingIr::from_tag((b >> 4 & 0xf) as u8).name()
            )
            .unwrap();
            write_src(w, k1, ops[0]);
            write!(w, "] <- [").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, "], expected ").unwrap();
            write_src(w, k1, ops[2]);
            write!(w, ", desired ").unwrap();
            write_src(w, k1, ops[3]);
            write!(w, ", ok_offset={}", ops[4]).unwrap();
        }
        Opcode::Fence => {
            write!(w, "fence {}", ir::AtomicOrderingIr::from_tag(b as u8).name()).unwrap();
        }
        Opcode::Zero => {
            write!(w, "zero [").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, "] size={}", ops[1]).unwrap();
        }
        Opcode::Copy => {
            write!(w, "copy [").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, "] <- [").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, "] size={}", ops[2]).unwrap();
        }
        Opcode::PtrAddImm => {
            write!(w, "ptr_add ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, " + {}", ops[2]).unwrap();
        }
        Opcode::PtrIndex => {
            write!(w, "ptr_index ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, " + ").unwrap();
            write_src(w, k1, ops[2]);
            write!(w, "*{}", ops[3]).unwrap();
        }
        Opcode::IntAddOverflowsU
        | Opcode::IntAddOverflowsS
        | Opcode::IntSubOverflowsU
        | Opcode::IntSubOverflowsS
        | Opcode::IntMulOverflowsU
        | Opcode::IntMulOverflowsS
        | Opcode::IntEq
        | Opcode::IntSlt
        | Opcode::IntSle
        | Opcode::IntSgt
        | Opcode::IntSge
        | Opcode::IntUlt
        | Opcode::IntUle
        | Opcode::IntUgt
        | Opcode::IntUge
        | Opcode::IntNe
        | Opcode::IntUcmp
        | Opcode::IntScmp
        | Opcode::FloatAdd
        | Opcode::FloatSub
        | Opcode::FloatMul
        | Opcode::FloatDiv
        | Opcode::FloatRem
        | Opcode::FloatMin
        | Opcode::FloatMax
        | Opcode::FloatEq
        | Opcode::FloatLt
        | Opcode::FloatLe
        | Opcode::FloatGt
        | Opcode::FloatGe
        | Opcode::FloatNe
        | Opcode::IntAdd
        | Opcode::IntSub
        | Opcode::IntMul
        | Opcode::IntDivU
        | Opcode::IntDivS
        | Opcode::IntRemU
        | Opcode::IntRemS
        | Opcode::IntMulHighU
        | Opcode::IntMulHighS
        | Opcode::IntRotl
        | Opcode::IntRotr
        | Opcode::BitAnd
        | Opcode::BitOr
        | Opcode::BitXor
        | Opcode::Shl
        | Opcode::ShrU
        | Opcode::ShrS => {
            write!(w, "{}.{} ", op.name(), a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[2]);
        }
        Opcode::FloatFma => {
            write!(w, "fma.{} ", a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[2]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[3]);
        }
        Opcode::BoolNegate => {
            write!(w, "{} ", op.name()).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::BitNot
        | Opcode::IntCtz
        | Opcode::IntClz
        | Opcode::IntPopcount
        | Opcode::IntBswap
        | Opcode::FloatNeg
        | Opcode::FloatSqrt
        | Opcode::FloatFloor
        | Opcode::FloatCeil
        | Opcode::FloatRoundToZero
        | Opcode::FloatRoundEven
        | Opcode::IntTrunc
        | Opcode::FloatTrunc
        | Opcode::FloatExt
        | Opcode::F32ToUInt
        | Opcode::F32ToSInt
        | Opcode::F64ToUInt
        | Opcode::F64ToSInt
        | Opcode::UIntToF32
        | Opcode::UIntToF64
        | Opcode::SIntToF32
        | Opcode::SIntToF64 => {
            write!(w, "{}.{} ", op.name(), a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::IntExtS => {
            write!(w, "{}.{}.{} ", op.name(), a, b).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::Select => {
            write!(w, "select ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, " ? ").unwrap();
            write_src(w, k1, ops[2]);
            write!(w, " : ").unwrap();
            write_src(w, k1, ops[3]);
        }
        Opcode::BakeStaticValue => {
            write!(w, "bake_static ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- type#{} ", ops[1]).unwrap();
            write_src(w, k1, ops[2]);
        }
    }
    pc + 1 + n
}
