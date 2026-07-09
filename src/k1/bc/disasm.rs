// Copyright (c) 2026 knix
// All rights reserved.

//! Disassembler for the bc stream — used for `#debug` unit dumps, divergence
//! logs, and tests.

use std::fmt::Write;

use crate::typer::TypedProgram;

use super::{
    CastKind, Opcode, SRC_CONST_BIT, builtin_from_tag, float_pred_from_tag, header_a, header_b,
    header_op, int_pred_from_tag,
};

fn write_src(w: &mut String, k1: &TypedProgram, src: u32) {
    if src & SRC_CONST_BIT != 0 {
        let idx = (src & !SRC_CONST_BIT) as usize;
        match k1.bc.consts.get(idx) {
            Some(v) => write!(w, "c[{}]={:#x}", idx, v).unwrap(),
            None => write!(w, "c[{}]=<oob>", idx).unwrap(),
        }
    } else {
        write!(w, "f[{}]", src).unwrap();
    }
}

fn write_dst(w: &mut String, dst: u32) {
    write!(w, "f[{}]", dst).unwrap();
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
    let n = op.operand_count();
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
        Opcode::JumpIf => {
            write!(w, "jump_if ").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " @{} @{}", ops[1], ops[2]).unwrap();
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
        }
        Opcode::CallIndirect => {
            write!(w, "call_indirect ").unwrap();
            write_src(w, k1, ops[0]);
            write!(w, " fp+={}", ops[1]).unwrap();
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
        Opcode::Lea => {
            write!(w, "lea ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- fp+{}", ops[1]).unwrap();
        }
        Opcode::LoadGlobal => {
            write!(w, "load_global ").unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- global#{}", ops[1]).unwrap();
        }
        Opcode::Load => {
            write!(w, "load.{} ", a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- [").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, "]").unwrap();
        }
        Opcode::Store => {
            write!(w, "store.{} [", a).unwrap();
            write_src(w, k1, ops[0]);
            write!(w, "] <- ").unwrap();
            write_src(w, k1, ops[1]);
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
        Opcode::IntCmp => {
            write!(w, "icmp.{}.{} ", int_pred_from_tag(b), a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[2]);
        }
        Opcode::FloatCmp => {
            write!(w, "fcmp.{}.{} ", float_pred_from_tag(b), a).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
            write!(w, ", ").unwrap();
            write_src(w, k1, ops[2]);
        }
        Opcode::IntAdd
        | Opcode::IntSub
        | Opcode::IntMul
        | Opcode::IntDivU
        | Opcode::IntDivS
        | Opcode::IntRemU
        | Opcode::IntRemS
        | Opcode::FloatAdd
        | Opcode::FloatSub
        | Opcode::FloatMul
        | Opcode::FloatDiv
        | Opcode::FloatRem
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
        Opcode::BoolNegate | Opcode::BitNot => {
            write!(w, "{} ", op.name()).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
        }
        Opcode::Cast => {
            let kind = CastKind::from_u8(a);
            write!(w, "cast.{:?} ", kind).unwrap();
            write_dst(w, ops[0]);
            write!(w, " <- ").unwrap();
            write_src(w, k1, ops[1]);
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
