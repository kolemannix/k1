use std::{
    io::stderr,
    num::NonZeroU32,
    sync::atomic::{AtomicU64, Ordering},
};

use ahash::HashMapExt;
use colored::Colorize;
use ecow::EcoVec;
use fxhash::FxHashMap;
use itertools::Itertools;
use k1_types::{CompilerMessageLevel, K1Buffer, K1SourceLocation};
use log::debug;
use smallvec::{SmallVec, smallvec};

use crate::{
    compiler::WordSize,
    failf, int_binop,
    lex::SpanId,
    nz_u32_id,
    parse::{Ident, StringId},
    pool::SliceHandle,
    typer::{
        self, BinaryOpKind, CastType, CodeEmission, FunctionId, IntrinsicOperation, Layout,
        MatchingCondition, MatchingConditionInstr, NameAndTypeId, StaticBuffer, StaticEnum,
        StaticStruct, StaticValue, StaticValueId, TypedExpr, TypedExprId, TypedFloatValue,
        TypedGlobalId, TypedIntValue, TypedMatchExpr, TypedProgram, TypedStmtId, TyperResult,
        VariableExpr, VariableId, make_fail_span,
        types::{
            BOOL_TYPE_ID, CHAR_TYPE_ID, F32_TYPE_ID, F64_TYPE_ID, FloatType, I32_TYPE_ID,
            I64_TYPE_ID, IWORD_TYPE_ID, IntegerType, NEVER_TYPE_ID, POINTER_TYPE_ID,
            STRING_TYPE_ID, Type, TypeId, TypedEnumVariant, Types, U32_TYPE_ID, U64_TYPE_ID,
            UNIT_TYPE_ID, UWORD_TYPE_ID,
        },
    },
};

#[cfg(test)]
mod vm_test;

mod binop;

/// Bit-for-bit mappings of K1 types
pub mod k1_types {

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1SourceLocation {
        pub filename: K1Buffer,
        pub line: u64,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1Buffer {
        pub len: usize,
        pub data: *const u8,
    }

    #[repr(u8)]
    pub enum CompilerMessageLevel {
        Info = 0,
        Warn = 1,
        Error = 2,
    }

    impl K1Buffer {
        ///# Safety
        /// None of this is safe
        pub unsafe fn to_slice<'a, T>(self) -> &'a [T] {
            unsafe { std::slice::from_raw_parts(self.data as *const T, self.len) }
        }

        ///# Safety
        /// Really make sure its a char buffer
        pub unsafe fn to_str<'a>(self) -> &'a str {
            unsafe {
                let slice = self.to_slice();
                std::str::from_utf8(slice).unwrap()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackSelection {
    StaticSpace,
    CallStackCurrent,
}

pub struct Vm {
    /// Code may update globals; so we store this VM run's 'copy' of that global's
    /// value here; these are (currently) reset on each invocation of #static code
    ///
    /// One example from k1 is code that sets the global, thread-local, default allocator
    ///
    /// If we need to preserve them for the entire compilation, we may need to re-use
    /// the same VM, currently we create one per static execution
    globals: FxHashMap<TypedGlobalId, Value>,
    static_stack: Stack,
    stack: Stack,
    eval_depth: AtomicU64,
    eval_span: SpanId,

    pub emits: Vec<CodeEmission>,
    pub allow_emits: bool,
}

impl Vm {
    pub fn reset(&mut self) {
        // Note that we don't de-allocate anything
        self.static_stack.reset();
        self.stack.reset();
        self.globals.clear();

        self.eval_depth.store(0, Ordering::Relaxed);
        self.eval_span = SpanId::NONE;
        self.emits.clear();
    }

    pub fn make(stack_size_bytes: usize, static_size_bytes: usize) -> Self {
        let stack = Stack::make(stack_size_bytes);
        let static_stack = Stack::make(static_size_bytes);
        Self {
            globals: FxHashMap::with_capacity(8192),
            static_stack,
            stack,
            eval_depth: AtomicU64::new(0),
            eval_span: SpanId::NONE,
            emits: Vec::with_capacity(128),
            allow_emits: false,
        }
    }

    pub fn dump(&mut self, m: &TypedProgram) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for index in 0..self.stack.frames.len() {
            let frame_string = self.dump_frame(m, index);
            write!(w, "FRAME {index:02} {}", frame_string).unwrap();
        }
        s
    }

    pub fn get_destination_stack(&mut self, dst_stack: StackSelection) -> &mut Stack {
        match dst_stack {
            StackSelection::StaticSpace => &mut self.static_stack,
            StackSelection::CallStackCurrent => &mut self.stack,
        }
    }

    pub fn insert_local(&mut self, frame_index: u32, variable_id: VariableId, value: Value) {
        let frame_locals = self.stack.locals.entry(frame_index).or_default();
        frame_locals.insert(variable_id, value);
    }

    pub fn insert_current_local(&mut self, variable_id: VariableId, value: Value) {
        self.insert_local(self.stack.current_frame(), variable_id, value)
    }

    pub fn get_current_local(&mut self, variable_id: VariableId) -> Option<Value> {
        self.get_local(self.stack.current_frame(), variable_id)
    }

    pub fn get_local(&mut self, frame_index: u32, variable_id: VariableId) -> Option<Value> {
        let frame_locals = self.stack.locals.entry(frame_index).or_default();
        frame_locals.get(&variable_id).copied()
    }

    pub fn dump_current_frame(&mut self, m: &TypedProgram) -> String {
        self.dump_frame(m, self.stack.frames.len() - 1)
    }

    pub fn dump_frame(&mut self, m: &TypedProgram, frame_index: usize) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        let frame = self.stack.frames[frame_index];
        writeln!(w, "Frame:  {}", m.ident_str_opt(frame.debug_name)).unwrap();
        writeln!(w, "Base :  {:?}", frame.base_ptr).unwrap();
        writeln!(w, "Locals").unwrap();
        let locals = self
            .stack
            .locals
            .entry(frame_index as u32)
            .or_default()
            .iter()
            .map(|p| (*p.0, *p.1))
            .collect_vec();
        let _debug_frame = self.stack.push_new_frame(None, None);
        for (k, v) in locals.into_iter() {
            let var = m.variables.get(k);
            let v_name = var.name;
            let hidden = var.user_hidden;
            if hidden {
                continue;
            }
            write!(w, "  {}: {} = ", m.ident_str(v_name), m.type_id_to_string(v.get_type()))
                .unwrap();
            render_debug_value(w, self, m, v);
            writeln!(w).unwrap()
        }
        self.stack.pop_frame();

        //write!(w, "\nDATA\n").unwrap();
        //let frame = &self.call_stack[frame_index];
        //frame.to_bytes().chunks(8).for_each(|bytes| {
        //    for b in bytes {
        //        write!(w, "{:02x} ", b).unwrap();
        //    }
        //    writeln!(w).unwrap();
        //});
        s
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VmExit {
    #[allow(unused)]
    span: SpanId,
    code: i32,
}

#[derive(Debug, Clone, Copy)]
pub enum VmResult {
    Value(Value),
    Break(Value),
    Return(Value),
    Exit(VmExit),
}

impl VmResult {
    pub const UNIT: VmResult = VmResult::Value(Value::Unit);

    pub fn expect_value(&self) -> Value {
        match self {
            VmResult::Value(v) => *v,
            VmResult::Break(_) => unreachable!("expect_value on break"),
            VmResult::Return(_) => unreachable!("expect_value on return"),
            VmResult::Exit(_) => unreachable!("expect_value on exit"),
        }
    }

    fn is_terminating(&self) -> bool {
        match self {
            VmResult::Value(_) => false,
            VmResult::Break(_) => true,
            VmResult::Return(_) => true,
            VmResult::Exit(_) => true,
        }
    }
}

impl From<Value> for VmResult {
    fn from(v: Value) -> Self {
        VmResult::Value(v)
    }
}

impl From<&Value> for VmResult {
    fn from(v: &Value) -> Self {
        VmResult::Value(*v)
    }
}

crate::static_assert_size!(Value, 24);
#[derive(Debug, Clone, Copy)]
pub enum Value {
    // 'Immediate' values
    Unit,
    Bool(bool),
    Char(u8),
    Int(TypedIntValue),
    Float(TypedFloatValue),
    Pointer(usize),
    // Despite holding a Rust pointer,
    // Reference is immediate since copying, loading or storing one only
    // entails a load or store of its pointer
    Reference { type_id: TypeId, ptr: *const u8 },
    // Aggregate; 'By address' values
    Agg { type_id: TypeId, ptr: *const u8 },
}

impl Value {
    pub const UNIT: Value = Value::Unit;
    pub const TRUE: Value = Value::Bool(true);
    pub const FALSE: Value = Value::Bool(false);

    pub fn is_immediate(&self) -> bool {
        match self {
            Value::Unit
            | Value::Bool(_)
            | Value::Char(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::Pointer(_) => true,
            Value::Reference { .. } => true,
            Value::Agg { .. } => false,
        }
    }

    pub fn as_agg(&self) -> Option<*const u8> {
        match self {
            Value::Agg { ptr, .. } => Some(*ptr),
            _ => None,
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            Value::Unit => UNIT_TYPE_ID,
            Value::Bool(_) => BOOL_TYPE_ID,
            Value::Char(_) => CHAR_TYPE_ID,
            Value::Int(typed_integer_value) => typed_integer_value.get_type(),
            Value::Float(typed_float_value) => typed_float_value.get_type(),
            Value::Pointer(_) => POINTER_TYPE_ID,
            Value::Reference { type_id, .. } => *type_id,
            Value::Agg { type_id, .. } => *type_id,
        }
    }

    pub fn set_type_id(&mut self, new_type_id: TypeId) {
        match self {
            Value::Unit => panic!("Cannot set type_id on a scalar"),
            Value::Bool(_) => panic!("Cannot set type_id on a scalar"),
            Value::Char(_) => panic!("Cannot set type_id on a scalar"),
            Value::Int(_) => panic!("Cannot set type_id on a scalar"),
            Value::Float(_) => panic!("Cannot set type_id on a scalar"),
            Value::Pointer(_) => panic!("Cannot set type_id on a scalar"),
            Value::Reference { type_id, .. } => *type_id = new_type_id,
            Value::Agg { type_id, .. } => *type_id = new_type_id,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::Char(_) => "char",
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::Pointer { .. } => "pointer",
            Value::Reference { .. } => "reference",
            Value::Agg { .. } => "struct",
        }
    }

    pub fn as_int(&self) -> Option<TypedIntValue> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn expect_int(&self) -> TypedIntValue {
        match self {
            Value::Int(i) => *i,
            _ => unreachable!("expect_integer on value {:?}", self),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn expect_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => unreachable!("expect_bool on value {:?}", self),
        }
    }

    pub fn as_ptr(&self) -> Option<usize> {
        match self {
            Value::Pointer(p) => Some(*p),
            _ => None,
        }
    }

    pub fn expect_ptr(&self) -> usize {
        match self {
            Value::Pointer(p) => *p,
            _ => unreachable!("expect_ptr on value {:?}", self),
        }
    }

    pub fn expect_ref(&self) -> *const u8 {
        match self {
            Value::Reference { ptr, .. } => *ptr,
            _ => unreachable!("expect_ref on value {:?}", self),
        }
    }

    pub fn expect_agg(&self) -> *const u8 {
        match self {
            Value::Agg { ptr, .. } => *ptr,
            _ => unreachable!("expect_agg on value {:?}", self),
        }
    }

    pub fn expect_float(&self) -> TypedFloatValue {
        match self {
            Value::Float(fv) => *fv,
            _ => unreachable!("expect_float on value {:?}", self),
        }
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Value::Int(TypedIntValue::U64(v))
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

const GLOBAL_ID_STATIC: TypedGlobalId = TypedGlobalId::from_nzu32(NonZeroU32::new(1).unwrap());

pub fn execute_single_expr_with_vm(
    m: &mut TypedProgram,
    expr: TypedExprId,
    vm: &mut Vm,
    input_parameters: &[(VariableId, StaticValueId)],
) -> TyperResult<Value> {
    // Tell the code we're about to execute that this is static
    // Useful for conditional compilation to branch and do what makes sense
    // in an interpreted context
    if cfg!(debug_assertions) {
        // The very first global defines IS_STATIC, so we won't find it
        if !m.globals.is_empty() {
            let k1_static_global = m.globals.get(GLOBAL_ID_STATIC);
            debug_assert_eq!(
                m.ident_str(m.variables.get(k1_static_global.variable_id).name),
                "IS_STATIC"
            );
        }
    }
    if !m.globals.is_empty() {
        vm.globals.insert(GLOBAL_ID_STATIC, Value::Bool(true));
    }

    let span = m.exprs.get(expr).get_span();
    vm.stack.push_new_frame(None, Some(span));

    // Bind a local variable in the VM for each input_parameter
    for (variable_id, static_value_id) in input_parameters {
        let vm_value =
            static_value_to_vm_value(vm, StackSelection::CallStackCurrent, m, *static_value_id)?;
        vm.insert_current_local(*variable_id, vm_value);
    }

    // eprintln!("{}", vm.dump_current_frame(m));

    let res = match execute_expr(vm, m, expr) {
        Err(e) => {
            m.write_error(&mut stderr(), &e).unwrap();
            failf!(span, "Static execution failed")
        }
        Ok(VmResult::Value(value)) => Ok(value),
        Ok(VmResult::Exit(exit)) => {
            failf!(span, "Static execution exited with code: {}", exit.code)
        }
        Ok(VmResult::Return(value)) => Ok(value),
        Ok(VmResult::Break(_)) => unreachable!("Break result from top-level expression"),
    };
    if cfg!(debug_assertions) {
        if let Ok(r) = res {
            debug!(
                "VM Invocation result for {}: {}",
                m.expr_to_string(expr),
                debug_value_to_string(vm, m, r),
            );
        }
    }
    res
}

macro_rules! return_exit {
    ($result:expr) => {{
        match $result {
            VmResult::Value(v) => v,
            VmResult::Break(_) => return Ok($result),
            VmResult::Return(_) => return Ok($result),
            VmResult::Exit(_) => return Ok($result),
        }
    }};
}

macro_rules! execute_expr_return_exit {
    ($vm:expr, $m:expr, $expr:expr) => {{
        let result = execute_expr($vm, $m, $expr)?;
        match result {
            VmResult::Value(v) => Ok(v),
            VmResult::Break(_) => return Ok(result),
            VmResult::Return(_) => return Ok(result),
            VmResult::Exit(_) => return Ok(result),
        }
    }};
}

fn execute_expr(vm: &mut Vm, m: &mut TypedProgram, expr: TypedExprId) -> TyperResult<VmResult> {
    vm.eval_depth.fetch_add(1, Ordering::Relaxed);
    let prev = vm.eval_span;
    vm.eval_span = m.exprs.get(expr).get_span();
    let mut vm = scopeguard::guard(vm, |vm| {
        vm.eval_depth.fetch_sub(1, Ordering::Relaxed);
        vm.eval_span = prev;
    });
    let vm = &mut vm;

    //print!("{}", " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize));
    //let mut s = String::new();
    //std::io::stdin().read_line(&mut s).unwrap();

    let result: TyperResult<VmResult> = match m.exprs.get(expr) {
        TypedExpr::Unit(_) => Ok(Value::Unit.into()),
        TypedExpr::Char(byte, _) => Ok(Value::Char(*byte).into()),
        TypedExpr::Bool(b, _) => Ok(Value::Bool(*b).into()),
        TypedExpr::Integer(typed_integer_expr) => Ok(Value::Int(typed_integer_expr.value).into()),
        TypedExpr::Float(typed_float_expr) => Ok(Value::Float(typed_float_expr.value).into()),
        TypedExpr::String(s, _) => {
            let string_value = string_id_to_value(vm, StackSelection::CallStackCurrent, m, *s);
            Ok(string_value.into())
        }
        TypedExpr::Struct(s) => {
            let mut values: SmallVec<[Value; 8]> = SmallVec::with_capacity(s.fields.len());
            let fields = s.fields.clone();
            let s_type_id = s.type_id;
            for field in &fields {
                let value = execute_expr_return_exit!(vm, m, field.expr)?;
                values.push(value);
            }

            let struct_value = vm.stack.push_struct_values(&m.types, s_type_id, &values);
            Ok(struct_value.into())
        }
        TypedExpr::StructFieldAccess(field_access) => {
            let field_access = *field_access;
            let struct_value = execute_expr_return_exit!(vm, m, field_access.base)?;
            let struct_ptr = match struct_value {
                Value::Agg { ptr: struct_ptr, .. } => struct_ptr,
                Value::Reference { ptr, .. } => ptr,
                _ => unreachable!("malformed field access: not a struct or struct*"),
            };
            if field_access.is_referencing {
                let field_ptr = gep_struct_field(
                    &m.types,
                    field_access.struct_type,
                    struct_ptr,
                    field_access.field_index as usize,
                );
                Ok(Value::Reference { type_id: field_access.result_type, ptr: field_ptr }.into())
            } else {
                let field_value = load_struct_field(
                    vm,
                    m,
                    field_access.struct_type,
                    struct_ptr,
                    field_access.field_index as usize,
                    true,
                )?;
                Ok(field_value.into())
            }
        }
        TypedExpr::Variable(variable_expr) => execute_variable_expr(vm, m, *variable_expr),
        TypedExpr::UnaryOp(unary_op) => {
            let span = unary_op.span;
            let target_type = unary_op.type_id;
            match unary_op.kind {
                typer::UnaryOpKind::Dereference => {
                    let ref_expr = unary_op.expr;
                    let reference_value = execute_expr_return_exit!(vm, m, ref_expr)?;
                    let Value::Reference { ptr, .. } = reference_value else {
                        m.ice_with_span(
                            format!("malformed dereference: {:?}", reference_value),
                            span,
                        )
                    };
                    let v = load_value(vm, m, target_type, ptr, true)?;
                    Ok(v.into())
                }
            }
        }
        TypedExpr::BinaryOp(bin_op) => {
            let bin_op = *bin_op;
            use BinaryOpKind as K;
            match bin_op.kind {
                K::Equals | K::NotEquals => {
                    let lhs = execute_expr_return_exit!(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_return_exit!(vm, m, bin_op.rhs)?;
                    let not_equals = bin_op.kind == K::NotEquals;
                    let bool_value_pre = match (lhs, rhs) {
                        (Value::Unit, Value::Unit) => Ok(true),
                        (Value::Bool(b1), Value::Bool(b2)) => Ok(b1 == b2),
                        (Value::Char(c1), Value::Char(c2)) => Ok(c1 == c2),
                        (Value::Int(i1), Value::Int(i2)) => Ok(i1 == i2),
                        (Value::Float(f1), Value::Float(f2)) => Ok(f1 == f2),
                        (lhs, rhs) => {
                            failf!(
                                bin_op.span,
                                "static equality over {} and {} is unimplemented",
                                lhs.kind_name(),
                                rhs.kind_name()
                            )
                        }
                    }?;
                    let bool_value = if not_equals { !bool_value_pre } else { bool_value_pre };
                    Ok(Value::Bool(bool_value).into())
                }
                K::Add | K::Subtract | K::Multiply | K::Divide | K::Rem => {
                    let lhs = execute_expr_return_exit!(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_return_exit!(vm, m, bin_op.rhs)?;
                    Ok(binop::execute_arith_op(lhs, rhs, bin_op.kind).into())
                }
                K::Less | K::LessEqual | K::Greater | K::GreaterEqual => {
                    let lhs = execute_expr_return_exit!(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_return_exit!(vm, m, bin_op.rhs)?;
                    Ok(binop::execute_cmp_op(lhs, rhs, bin_op.kind).into())
                }
                K::And => {
                    // The language semantics guarantee short-circuiting of And
                    let lhs = execute_expr_return_exit!(vm, m, bin_op.lhs)?.expect_bool();

                    if lhs {
                        let rhs = execute_expr_return_exit!(vm, m, bin_op.rhs)?.expect_bool();
                        Ok(Value::Bool(rhs).into())
                    } else {
                        Ok(Value::Bool(false).into())
                    }
                }
                K::Or => {
                    // The language semantics guarantee short-circuiting of Or
                    let lhs = execute_expr_return_exit!(vm, m, bin_op.lhs)?.expect_bool();
                    if lhs {
                        Ok(Value::Bool(true).into())
                    } else {
                        let rhs = execute_expr_return_exit!(vm, m, bin_op.rhs)?.expect_bool();
                        Ok(Value::Bool(rhs).into())
                    }
                }
                _ => {
                    failf!(bin_op.span, "Unsupported static binary op: {}", m.expr_to_string(expr))
                }
            }
        }
        TypedExpr::Block(_) => execute_block(vm, m, expr),
        TypedExpr::Call(_) => execute_call(vm, m, expr),
        TypedExpr::Match(match_expr) => {
            let match_expr = match_expr.clone();
            //eprintln!("execute_match: {}", m.expr_to_string(expr));
            execute_match(vm, m, &match_expr)
        }
        TypedExpr::WhileLoop(while_expr) => {
            let while_expr = while_expr.clone();
            let result = loop {
                let cond_result = execute_matching_condition(vm, m, &while_expr.condition_block)?;
                let cond = return_exit!(cond_result).expect_bool();
                if !cond {
                    break VmResult::UNIT;
                }

                let body_result = execute_expr(vm, m, while_expr.body)?;
                match body_result {
                    VmResult::Value(_) => continue,
                    VmResult::Break(_) => break VmResult::UNIT,
                    VmResult::Return(_) | VmResult::Exit(_) => break body_result,
                }
            };
            Ok(result)
        }
        TypedExpr::LoopExpr(loop_expr) => {
            let body_block = loop_expr.body_block;
            let result = loop {
                let block_result = execute_block(vm, m, body_block)?;
                //eprintln!("loop iter {} {:?}", m.expr_to_string(body_block).blue(), block_result);
                match block_result {
                    VmResult::Value(_) => continue,
                    VmResult::Break(value) => break VmResult::Value(value),
                    VmResult::Return(_) | VmResult::Exit(_) => break block_result,
                }
            };
            Ok(result)
        }
        TypedExpr::Break(b) => match execute_expr(vm, m, b.value)? {
            VmResult::Value(value) => Ok(VmResult::Break(value)),
            VmResult::Break(_) => unreachable!("cant break inside a break"),
            VmResult::Return(_) => unreachable!("cant return inside a break"),
            res @ VmResult::Exit(_) => Ok(res),
        },
        TypedExpr::EnumConstructor(e) => {
            let e = *e;
            let payload_value = match e.payload {
                None => None,
                Some(payload_expr) => Some(execute_expr_return_exit!(vm, m, payload_expr)?),
            };
            let enum_ptr = vm.stack.push_enum(&m.types, e.variant_type_id, payload_value);
            Ok(Value::Agg { type_id: e.variant_type_id, ptr: enum_ptr }.into())
        }
        // EnumIsVariant could actually go away in favor of just an == on the getTag + a type-level
        // get tag expr on rhs
        TypedExpr::EnumIsVariant(is_variant) => {
            let is_variant = *is_variant;
            let Value::Agg { ptr, .. } = execute_expr_return_exit!(vm, m, is_variant.enum_expr)?
            else {
                m.ice_with_span("malformed enum get_tag", is_variant.span)
            };
            let enum_type = m.get_expr_type(is_variant.enum_expr).expect_enum();
            let Value::Int(tag_value) = load_value(vm, m, enum_type.tag_type, ptr, true)? else {
                unreachable!()
            };
            let variant = &enum_type.variants[is_variant.variant_index as usize];
            Ok(Value::Bool(tag_value == variant.tag_value).into())
        }
        TypedExpr::EnumGetTag(get_tag) => {
            let get_tag = *get_tag;
            let Value::Agg { ptr, .. } = execute_expr_return_exit!(vm, m, get_tag.enum_expr)?
            else {
                m.ice_with_span("malformed enum get_tag", get_tag.span)
            };
            // Do not load the entire enum to the stack, simply
            // interpret the base ptr as a ptr to the tag type
            let enum_type = m.get_expr_type(get_tag.enum_expr).expect_enum();
            let tag_value = load_value(vm, m, enum_type.tag_type, ptr, true)?;
            Ok(tag_value.into())
        }
        TypedExpr::EnumGetPayload(get_variant_payload) => {
            let get_payload = *get_variant_payload;
            let base_value = execute_expr_return_exit!(vm, m, get_payload.enum_variant_expr)?;
            let enum_ptr = match base_value {
                Value::Agg { ptr: variant_ptr, .. } => variant_ptr,
                Value::Reference { ptr, .. } => ptr,
                _ => unreachable!("malformed get variant payload: not a agg or agg*"),
            };
            let variant_type = match m.get_expr_type(get_payload.enum_variant_expr) {
                Type::EnumVariant(ev) => ev,
                Type::Reference(r) => m.types.get(r.inner_type).expect_enum_variant(),
                _ => unreachable!("malformed get variant payload"),
            };
            let payload_ptr = gep_enum_payload(&m.types, variant_type, enum_ptr);
            if get_payload.is_referencing {
                let payload_reference =
                    Value::Reference { type_id: get_payload.result_type_id, ptr: payload_ptr };
                Ok(payload_reference.into())
            } else {
                let payload_value =
                    load_value(vm, m, variant_type.payload.unwrap(), payload_ptr, true)?;
                Ok(payload_value.into())
            }
        }
        TypedExpr::Cast(typed_cast) => {
            let cast = typed_cast.clone();
            let span = cast.span;
            let base_value = execute_expr_return_exit!(vm, m, cast.base_expr)?;
            match cast.cast_type {
                CastType::IntegerCast(_direction) => {
                    let int_to_cast = base_value.expect_int();
                    let value = integer_cast(&m.types, int_to_cast, cast.target_type_id);
                    Ok(Value::Int(value).into())
                }
                CastType::IntegerExtendFromChar => {
                    let Value::Char(c) = base_value else { unreachable!() };
                    let int_to_cast = TypedIntValue::U8(c);
                    let value = integer_cast(&m.types, int_to_cast, cast.target_type_id);
                    Ok(Value::Int(value).into())
                }
                CastType::Integer8ToChar => {
                    let byte = match base_value.expect_int() {
                        TypedIntValue::U8(b) => b,
                        TypedIntValue::I8(b) => b as u8,
                        _ => unreachable!(),
                    };
                    Ok(Value::Char(byte).into())
                }
                CastType::IntegerToFloat => {
                    let int_to_cast = base_value.expect_int();
                    let float_value = match cast.target_type_id {
                        F32_TYPE_ID => {
                            if int_to_cast.is_signed() {
                                TypedFloatValue::F32(int_to_cast.to_i32() as f32)
                            } else {
                                TypedFloatValue::F32(int_to_cast.to_u32() as f32)
                            }
                        }
                        F64_TYPE_ID => {
                            if int_to_cast.is_signed() {
                                TypedFloatValue::F64(int_to_cast.to_i64() as f64)
                            } else {
                                TypedFloatValue::F64(int_to_cast.to_u64() as f64)
                            }
                        }
                        _ => unreachable!(),
                    };
                    Ok(Value::Float(float_value).into())
                }
                CastType::IntegerToPointer => {
                    let u = base_value.expect_int().expect_uword();
                    Ok(Value::Pointer(u as usize).into())
                }
                CastType::EnumToVariant | CastType::VariantToEnum => {
                    let new_value = match base_value {
                        Value::Agg { ptr, .. } => Value::Agg { ptr, type_id: cast.target_type_id },
                        _ => m.ice_with_span(
                            format!("Malformed EnumToVariant cast: {}", m.expr_to_string(expr)),
                            vm.eval_span,
                        ),
                    };
                    Ok(VmResult::Value(new_value))
                }
                CastType::PointerToReference => {
                    let Value::Pointer(value) = base_value else {
                        m.ice_with_span("malformed pointer-to-reference cast", span)
                    };
                    Ok(Value::Reference { type_id: cast.target_type_id, ptr: value as *const u8 }
                        .into())
                }
                CastType::ReferenceToPointer => {
                    let Value::Reference { ptr, .. } = base_value else {
                        m.ice_with_span("malformed reference-to-pointer cast", span)
                    };
                    Ok(Value::Pointer(ptr.addr()).into())
                }
                CastType::ReferenceToReference => {
                    let Value::Reference { ptr, .. } = base_value else {
                        m.ice_with_span("malformed reference-to-reference cast", span)
                    };
                    Ok(Value::Reference { type_id: cast.target_type_id, ptr }.into())
                }
                CastType::PointerToWord => {
                    let ptr_usize = base_value.expect_ptr();
                    let int_value = match cast.target_type_id {
                        UWORD_TYPE_ID => TypedIntValue::UWord64(ptr_usize as u64),
                        IWORD_TYPE_ID => TypedIntValue::IWord64(ptr_usize as i64),
                        _ => m.ice_with_span("malformed PointerToWord", span),
                    };
                    Ok(Value::Int(int_value).into())
                }
                CastType::FloatExtend => {
                    let float_value = base_value.expect_float();
                    let extended = match (float_value, cast.target_type_id) {
                        (TypedFloatValue::F32(f32), F64_TYPE_ID) => {
                            TypedFloatValue::F64(f32 as f64)
                        }
                        _ => m.ice_with_span("malformed float extend", vm.eval_span),
                    };
                    Ok(Value::Float(extended).into())
                }
                CastType::FloatTruncate => {
                    let float_value = base_value.expect_float();
                    let truncated = match (float_value, cast.target_type_id) {
                        (TypedFloatValue::F64(f64), F32_TYPE_ID) => {
                            TypedFloatValue::F32(f64 as f32)
                        }
                        _ => m.ice_with_span("malformed float truncate", vm.eval_span),
                    };
                    Ok(Value::Float(truncated).into())
                }
                CastType::FloatToInteger => {
                    let float_value = base_value.expect_float();
                    let int_value = match (float_value, cast.target_type_id) {
                        (TypedFloatValue::F32(f32), U32_TYPE_ID) => TypedIntValue::U32(f32 as u32),
                        (TypedFloatValue::F32(f32), I32_TYPE_ID) => TypedIntValue::I32(f32 as i32),
                        (TypedFloatValue::F64(f64), U64_TYPE_ID) => TypedIntValue::U64(f64 as u64),
                        (TypedFloatValue::F64(f64), I64_TYPE_ID) => TypedIntValue::I64(f64 as i64),
                        _ => m.ice_with_span("malformed float to integer cast", vm.eval_span),
                    };
                    Ok(Value::Int(int_value).into())
                }
                CastType::LambdaToLambdaObject => {
                    // Lambda = the struct
                    // So we need a pointer to the struct, ideally on _our own_ stack, then
                    // a pointer to the function, which is an encoded function id
                    let lambda_object_type = cast.target_type_id;
                    let lambda_type = m.get_expr_type(cast.base_expr).as_lambda().unwrap();
                    let obj_type = m.types.get(lambda_object_type).as_lambda_object().unwrap();
                    if cfg!(debug_assertions) {
                        let struct_layout = m.types.get_layout(obj_type.struct_representation);
                        debug_assert_eq!(struct_layout.size, 16);
                    }

                    let lambda_obj_struct_type =
                        m.types.get(obj_type.struct_representation).expect_struct();
                    let function_reference_type = lambda_obj_struct_type.fields
                        [crate::typer::types::Types::LAMBDA_OBJECT_FN_PTR_INDEX]
                        .type_id;
                    let environment_ref_type = lambda_obj_struct_type.fields
                        [crate::typer::types::Types::LAMBDA_OBJECT_ENV_PTR_INDEX]
                        .type_id;
                    let environment_struct = base_value.expect_agg();

                    // We just 'cast' the environment struct's base ptr to a reference value
                    // since structs are represented as pointers.
                    let env_reference =
                        Value::Reference { type_id: environment_ref_type, ptr: environment_struct };
                    let function_ref_value = function_id_to_ref_value(
                        lambda_type.body_function_id,
                        function_reference_type,
                    );
                    let mut lambda_object = vm.stack.push_struct_values(
                        &m.types,
                        obj_type.struct_representation,
                        &[function_ref_value, env_reference],
                    );
                    lambda_object.set_type_id(lambda_object_type);
                    Ok(lambda_object.into())
                }
                CastType::ToNever => Ok(VmResult::Value(base_value)),
                CastType::StaticErase => Ok(VmResult::Value(base_value)),
            }
        }
        TypedExpr::Return(typed_return) => {
            let span = typed_return.span;
            match execute_expr(vm, m, typed_return.value)? {
                VmResult::Value(value) => Ok(VmResult::Return(value)),
                // Return inside return happens when the user
                // has an early return in a terminating block
                // We wrap the entire terminating block in a Return
                // so that its, well, terminated.
                VmResult::Return(value) => Ok(VmResult::Return(value)),
                VmResult::Break(_) => failf!(span, "Break inside return"),
                VmResult::Exit(code) => Ok(VmResult::Exit(code)),
            }
        }
        TypedExpr::Lambda(l) => {
            // Lambdas are physically represented as simply their environment structs
            // Since we know the function id from the type still
            let lambda_type_id = l.lambda_type;
            let lambda_type = m.types.get(lambda_type_id).as_lambda().unwrap();
            let mut env_struct = execute_expr_return_exit!(vm, m, lambda_type.environment_struct)?;
            env_struct.set_type_id(lambda_type_id);
            Ok(VmResult::Value(env_struct))
        }
        TypedExpr::FunctionReference(fun_ref) => {
            let function_ref_value =
                function_id_to_ref_value(fun_ref.function_id, fun_ref.function_reference_type);
            Ok(VmResult::Value(function_ref_value))
        }
        TypedExpr::FunctionToLambdaObject(f_to_lam) => {
            let obj_type = m.types.get(f_to_lam.lambda_object_type_id).as_lambda_object().unwrap();
            if cfg!(debug_assertions) {
                let struct_layout = m.types.get_layout(obj_type.struct_representation);
                debug_assert_eq!(struct_layout.size, 16);
            }

            let lambda_obj_struct_type =
                m.types.get(obj_type.struct_representation).expect_struct();
            let function_reference_type = lambda_obj_struct_type.fields
                [crate::typer::types::Types::LAMBDA_OBJECT_FN_PTR_INDEX]
                .type_id;

            // This is a pointer to a zero-sized environment struct since this is a function
            // not a lambda
            let env_ptr = Value::Reference { type_id: POINTER_TYPE_ID, ptr: vm.stack.cursor };
            let function_ref_value =
                function_id_to_ref_value(f_to_lam.function_id, function_reference_type);
            let mut lambda_object = vm.stack.push_struct_values(
                &m.types,
                obj_type.struct_representation,
                &[function_ref_value, env_ptr],
            );
            lambda_object.set_type_id(f_to_lam.lambda_object_type_id);
            Ok(lambda_object.into())
        }
        TypedExpr::PendingCapture(_) => m.ice_with_span("pending capture in vm", vm.eval_span),
        TypedExpr::StaticValue(value_id, _, _) => {
            let vm_value =
                static_value_to_vm_value(vm, StackSelection::CallStackCurrent, m, *value_id)?;
            Ok(vm_value.into())
        }
    };
    if cfg!(debug_assertions) {
        if let Ok(VmResult::Value(v)) = result {
            let expected_type = m.exprs.get(expr).get_type();
            let is_metaprogram_special_case =
                expected_type == NEVER_TYPE_ID && v.get_type() == UNIT_TYPE_ID;
            let is_static_expected =
                matches!(m.types.get_no_follow_static(expected_type), Type::Static(_));
            if !is_metaprogram_special_case && !is_static_expected {
                if let Err(msg) = m.check_types(
                    m.exprs.get(expr).get_type(),
                    v.get_type(),
                    // We use root scope because we don't expected to need to resolve any type
                    // variables in VM code; it should all be concrete, and that's all the
                    // scope is used for in check_types
                    m.scopes.get_root_scope_id(),
                ) {
                    eprintln!("{}", vm.dump_current_frame(m));
                    return failf!(
                        vm.eval_span,
                        "vm eval type mismatch after executing '{}'\n{}",
                        m.expr_to_string_with_type(expr),
                        msg,
                    );
                }
            }
        }
    }
    //debug!("{}-> {:?}", " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize), result);
    result
}

fn execute_variable_expr(
    vm: &mut Vm,
    m: &mut TypedProgram,
    variable_expr: VariableExpr,
) -> TyperResult<VmResult> {
    let v_id = variable_expr.variable_id;
    let variable = m.variables.get(v_id);
    if let Some(global_id) = variable.global_id {
        // Handle global
        // Case 1: It's in the VM because someone mutated it
        //         OR we already evaluated it during this execution
        // Case 2: Grab it from the module
        return match vm.globals.get(&global_id) {
            Some(global_value) => Ok(global_value.into()),
            None => {
                let global = m.globals.get(global_id);
                let type_id = global.ty;
                let is_referencing = global.is_referencing;
                let global_value = match global.initial_value {
                    None => {
                        m.eval_global_body(global.ast_id, Some(vm))?;
                        m.globals.get(global_id).initial_value.unwrap()
                    }
                    Some(value_id) => value_id,
                };
                let vm_value =
                    static_value_to_vm_value(vm, StackSelection::StaticSpace, m, global_value)?;
                let final_value = if is_referencing {
                    let ptr = vm.static_stack.push_ptr_uninit();
                    store_value(&m.types, ptr, vm_value);
                    Value::Reference { type_id, ptr }
                } else {
                    vm_value
                };

                vm.globals.insert(global_id, final_value);
                Ok(final_value.into())
            }
        };
    }

    let Some(v) = vm.get_current_local(v_id) else {
        return failf!(
            variable_expr.span,
            "Variable missing in vm: {}",
            m.ident_str(m.variables.get(v_id).name)
        );
    };
    Ok(v.into())
}

fn function_id_to_ref_value(function_id: FunctionId, function_reference_type_id: TypeId) -> Value {
    let function_id_u32 = function_id.as_u32();
    let function_id_as_ptr = function_id_u32 as usize as *const u8;
    debug!(
        "Encoding function id {function_id_u32} into the pointer address of a Reference value {:?}",
        function_id_as_ptr
    );
    let value = Value::Reference { type_id: function_reference_type_id, ptr: function_id_as_ptr };
    value
}

pub fn static_value_to_vm_value(
    vm: &mut Vm,
    dst_stack: StackSelection,
    m: &TypedProgram,
    static_value_id: StaticValueId,
) -> TyperResult<Value> {
    match m.static_values.get(static_value_id) {
        StaticValue::Unit => Ok(Value::Unit),
        StaticValue::Boolean(bv) => Ok(Value::Bool(*bv)),
        StaticValue::Char(cb) => Ok(Value::Char(*cb)),
        StaticValue::Int(iv) => Ok(Value::Int(*iv)),
        StaticValue::Float(fv) => Ok(Value::Float(*fv)),
        StaticValue::String(string_id) => {
            let value = string_id_to_value(vm, dst_stack, m, *string_id);
            Ok(value)
        }
        StaticValue::NullPointer => Ok(Value::Pointer(0)),
        StaticValue::Struct(static_struct) => {
            let mut values: SmallVec<[Value; 8]> = smallvec![];
            for f in static_struct.fields.iter() {
                let value = static_value_to_vm_value(vm, dst_stack, m, *f)?;
                values.push(value);
            }
            let struct_value = vm.get_destination_stack(dst_stack).push_struct_values(
                &m.types,
                static_struct.type_id,
                &values,
            );
            Ok(struct_value)
        }
        StaticValue::Enum(e) => {
            let payload_value = match e.payload {
                None => None,
                Some(static_value_id) => {
                    let value = static_value_to_vm_value(vm, dst_stack, m, static_value_id)?;
                    Some(value)
                }
            };
            let enum_ptr = vm.get_destination_stack(dst_stack).push_enum(
                &m.types,
                e.variant_type_id,
                payload_value,
            );
            let type_id = if e.typed_as_enum {
                m.types.get(e.variant_type_id).expect_enum_variant().enum_type_id
            } else {
                e.variant_type_id
            };
            Ok(Value::Agg { type_id, ptr: enum_ptr })
        }
        StaticValue::Buffer(buf) => {
            let elements = buf.elements.clone();
            let element_type = m.types.get(buf.type_id).as_buffer_instance().unwrap().type_args[0];

            let layout = m.types.get_layout(element_type);
            let buffer_allocation_layout = layout.array_me(buf.len());

            debug!(
                "Pushing {} bytes for Buffer {}",
                buffer_allocation_layout.size,
                m.static_value_to_string(static_value_id)
            );
            let base_mem = vm.static_stack.push_layout_uninit(buffer_allocation_layout);

            for (index, elem_value_id) in elements.iter().enumerate() {
                let elem_value = static_value_to_vm_value(vm, dst_stack, m, *elem_value_id)?;
                let elem_dst_ptr = unsafe { base_mem.byte_add(index * layout.size as usize) };
                store_value(&m.types, elem_dst_ptr, elem_value);
            }
            let buffer = K1Buffer { len: elements.len(), data: base_mem.cast_const() };
            let buffer_struct_ptr = vm.get_destination_stack(dst_stack).push_t(buffer);

            Ok(Value::Agg { type_id: buf.type_id, ptr: buffer_struct_ptr })
        }
    }
}

#[inline]
pub fn execute_block(
    vm: &mut Vm,
    m: &mut TypedProgram,
    block_expr: TypedExprId,
) -> TyperResult<VmResult> {
    let mut last_stmt_result = VmResult::UNIT;
    let TypedExpr::Block(typed_block) = m.exprs.get(block_expr) else { unreachable!() };
    let stmts = typed_block.statements.clone();
    for stmt in stmts.iter() {
        last_stmt_result = execute_stmt(vm, m, *stmt)?;
        //eprintln!("LAST STMT RESULT IS {}", debug_vm_result_to_string(vm, m, last_stmt_result));
        if last_stmt_result.is_terminating() {
            return Ok(last_stmt_result);
        }
    }
    Ok(last_stmt_result)
}

pub fn execute_stmt(
    vm: &mut Vm,
    m: &mut TypedProgram,
    stmt_id: TypedStmtId,
) -> TyperResult<VmResult> {
    match m.stmts.get(stmt_id) {
        typer::TypedStmt::Expr(typed_expr_id, _) => {
            let result = execute_expr(vm, m, *typed_expr_id)?;
            Ok(result)
        }
        typer::TypedStmt::Let(let_stmt) => {
            let let_stmt = *let_stmt;
            let v = execute_expr_return_exit!(vm, m, let_stmt.initializer)?;
            let to_store = if let_stmt.is_referencing {
                let reference_type = let_stmt.variable_type;
                let value_type = v.get_type();
                debug_assert_eq!(
                    m.types.get(reference_type).expect_reference().inner_type,
                    value_type
                );
                let value_ptr = match v.as_agg() {
                    None => vm.stack.push_value(&m.types, v),
                    Some(ptr) => ptr,
                };
                Value::Reference { type_id: reference_type, ptr: value_ptr }
            } else {
                v
            };
            vm.insert_current_local(let_stmt.variable_id, to_store);
            Ok(VmResult::UNIT)
        }
        typer::TypedStmt::Assignment(assgn) => {
            let assgn = *assgn;
            let v = execute_expr_return_exit!(vm, m, assgn.value)?;

            match assgn.kind {
                typer::AssignmentKind::Value => {
                    let TypedExpr::Variable(destination_var) = m.exprs.get(assgn.destination)
                    else {
                        m.ice("Value assignment lhs was not a variable", None)
                    };
                    vm.insert_current_local(destination_var.variable_id, v);
                    Ok(VmResult::UNIT)
                }
                typer::AssignmentKind::Reference => {
                    let lhs_value = execute_expr_return_exit!(vm, m, assgn.destination)?;
                    let Value::Reference { ptr, .. } = lhs_value else {
                        m.ice(
                            format!(
                                "Reference assignment lhs must be a Reference value: {:?}",
                                lhs_value
                            ),
                            None,
                        )
                    };
                    store_value(&m.types, ptr.cast_mut(), v);
                    Ok(VmResult::UNIT)
                }
            }
        }
        typer::TypedStmt::Require(require) => {
            // TODO(vm perf): We are cloning MatchingConditions all over the VM
            let require = require.clone();
            let span = require.span;
            let else_body = require.else_body;
            let condition =
                return_exit!(execute_matching_condition(vm, m, &require.condition)?).expect_bool();
            if !condition {
                let _else_result = execute_expr_return_exit!(vm, m, else_body)?;
                m.ice_with_span("Else block should have exited or returned", span)
            }

            Ok(VmResult::UNIT)
        }
    }
}

fn execute_call(vm: &mut Vm, m: &mut TypedProgram, call_id: TypedExprId) -> TyperResult<VmResult> {
    debug!("Executing call: {}", m.expr_to_string(call_id));
    let call = m.exprs.get(call_id).expect_call();
    let call_args = call.args.clone();
    let span = call.span;
    let (function_id, maybe_lambda_env_ptr) = match call.callee {
        typer::Callee::StaticFunction(function_id) => (function_id, None),
        typer::Callee::StaticLambda {
            function_id, lambda_value_expr: environment_value, ..
        } => {
            // This is the lambda's physical form, so its the environment struct
            let lambda_value = execute_expr(vm, m, environment_value)?.expect_value();
            // We need a pointer to the environment for dispatch, so just use the aggregate's ptr
            let lambda_env_ptr = Value::Pointer(lambda_value.expect_agg().addr());
            (function_id, Some(lambda_env_ptr))
        }
        typer::Callee::Abstract { .. } => m.ice_with_span("Abstract call attempt in VM", span),
        typer::Callee::DynamicLambda(lambda_object_expr) => {
            let lambda_object_struct_ptr =
                execute_expr_return_exit!(vm, m, lambda_object_expr)?.expect_agg();
            let struct_type = m
                .get_expr_type(lambda_object_expr)
                .as_lambda_object()
                .unwrap()
                .struct_representation;
            let function_ptr = load_struct_field(
                vm,
                m,
                struct_type,
                lambda_object_struct_ptr,
                Types::LAMBDA_OBJECT_FN_PTR_INDEX,
                true,
            )?
            .expect_ref();
            let env_ref_value = load_struct_field(
                vm,
                m,
                struct_type,
                lambda_object_struct_ptr,
                Types::LAMBDA_OBJECT_ENV_PTR_INDEX,
                true,
            )?;
            let function_id = NonZeroU32::new(function_ptr.addr() as u32).unwrap();
            let function_id = FunctionId::from_nzu32(function_id);
            (function_id, Some(env_ref_value))
        }
        typer::Callee::DynamicFunction { function_reference_expr } => {
            let callee_ref =
                execute_expr_return_exit!(vm, m, function_reference_expr)?.expect_ref();
            // We stuff function ids into the pointers for dynamic dispatch in the VM
            let function_id = NonZeroU32::new(callee_ref.addr() as u32).unwrap();
            let function_id = FunctionId::from_nzu32(function_id);
            (function_id, None)
        }
        typer::Callee::DynamicAbstract { .. } => {
            m.ice_with_span("Abstract call attempt in VM", span)
        }
    };

    let function = m.get_function(function_id);

    if let Some(intrinsic_type) = function.intrinsic_type {
        let call = m.exprs.get(call_id).expect_call();
        return execute_intrinsic(
            vm,
            m,
            call.type_args,
            &call_args,
            call.return_type,
            intrinsic_type,
        );
    };

    if function.body_block.is_none() {
        let prev_level = log::max_level();
        log::set_max_level(log::LevelFilter::Info);
        eprintln!(
            "vm has to trigger compilation of body: {}",
            m.function_id_to_string(function_id, true)
        );
        m.eval_function_body(function_id)?;
        log::set_max_level(prev_level);
    }

    let function = m.get_function(function_id);
    let Some(body_block_expr) = function.body_block else {
        return failf!(span, "Cannot execute function {}: no body", m.ident_str(function.name));
    };

    if vm.stack.frames.len() == 128 {
        let frame_names = make_stack_trace(m, &vm.stack);
        eprintln!("{}", frame_names);
        return failf!(span, "VM call stack overflow");
    }

    let return_type_id = m.types.get(function.type_id).expect_function().return_type;
    let aggregate_return_layout = if m.types.is_aggregate_repr(return_type_id) {
        Some(m.types.get_layout(return_type_id))
    } else {
        None
    };
    let agg_ret_ptr = if let Some(return_type_layout) = aggregate_return_layout {
        let ptr = vm.stack.push_layout_uninit(return_type_layout);
        Some(ptr)
    } else {
        None
    };

    let name = function.name;

    // Arguments!
    let param_variables = function.param_variables.clone();
    let mut param_values: SmallVec<[(VariableId, Value); 8]> = smallvec![];
    let is_lambda = maybe_lambda_env_ptr.is_some();
    let arg_offset = if is_lambda { 1 } else { 0 };
    for (index, variable_id) in param_variables.iter().enumerate() {
        if index == 0 && is_lambda {
            param_values.push((*variable_id, maybe_lambda_env_ptr.unwrap()));
        } else {
            let arg_offset = index - arg_offset;
            let arg = call_args[arg_offset];
            // Execute this in _caller_ frame so the data outlives the callee
            let value = execute_expr_return_exit!(vm, m, arg)?;
            debug!(
                "argument {}: {}",
                m.ident_str(m.variables.get(*variable_id).name),
                debug_value_to_string(vm, m, value)
            );

            param_values.push((*variable_id, value));
        }
    }

    // But bind the variables in the _callee_ frame
    vm.stack.push_new_frame(Some(name), Some(span));
    for (variable_id, value) in param_values.into_iter() {
        vm.insert_current_local(variable_id, value);
    }

    // Push the callee frame for body execution
    //eprintln!("******************** NEW FRAME {} **********************", m.name_of(function.name));
    //eprintln!("{}", vm.dump_current_frame(m));
    //eprintln!("{}", vm.dump_current_frame(m));

    let result_value = match execute_expr(vm, m, body_block_expr)? {
        VmResult::Value(value) => value,
        VmResult::Return(value) => value,
        exit @ VmResult::Exit(_) => {
            vm.stack.pop_frame();
            return Ok(exit);
        }
        VmResult::Break(_) => unreachable!("got break from function"),
    };

    // If immediate, just 'Copy' the rust value; otherwise memcpy the data to the old stack's
    // 'sret' reserved space
    vm.stack.pop_frame();
    let updated_value = match result_value.as_agg() {
        None => result_value,
        Some(result_ptr) => {
            let dst_ptr = agg_ret_ptr.unwrap();
            // TODO(vm perf): When we evaluate a 'Return' expr, we could store results directly
            //           into here, as long as we can look up this ptr somehow, and avoid
            //           the full copy
            let layout = aggregate_return_layout.unwrap();
            unsafe {
                copy_aggregate(dst_ptr, result_ptr, layout.size as usize);
            }
            let updated_value =
                Value::Agg { type_id: result_value.get_type(), ptr: dst_ptr.cast_const() };
            updated_value
        }
    };
    Ok(updated_value.into())
}

fn execute_intrinsic(
    vm: &mut Vm,
    m: &mut TypedProgram,
    type_args: SliceHandle<NameAndTypeId>,
    args: &[TypedExprId],
    return_type: TypeId,
    intrinsic_type: IntrinsicOperation,
) -> TyperResult<VmResult> {
    match intrinsic_type {
        IntrinsicOperation::SizeOf
        | IntrinsicOperation::SizeOfStride
        | IntrinsicOperation::AlignOf
        | IntrinsicOperation::GetStaticValue => {
            unreachable!("Handled by typer phase")
        }
        IntrinsicOperation::Zeroed => {
            let type_id = m.named_types.get_nth(type_args, 0).type_id;
            let value = match m.types.get(type_id) {
                Type::Static(_) => {
                    return failf!(
                        vm.eval_span,
                        "zeroed() for statically known values currently unsupported",
                    );
                }
                Type::Unit => Value::Unit,
                Type::Char => Value::Char(0),
                Type::Bool => Value::Bool(false),
                Type::Pointer => Value::Pointer(0),
                Type::Integer(integer_type) => {
                    let zero_value = integer_type.zero();
                    Value::Int(zero_value)
                }
                Type::Float(float_type) => Value::Float(float_type.zero()),
                Type::Reference(_) => Value::Reference { type_id, ptr: core::ptr::null() },
                Type::Struct(_)
                | Type::Enum(_)
                | Type::EnumVariant(_)
                | Type::Lambda(_)
                | Type::LambdaObject(_) => {
                    debug_assert!(m.types.is_aggregate_repr(type_id));
                    let layout = m.types.get_layout(type_id);
                    let data = vm.stack.push_layout_uninit(layout);
                    Value::Agg { type_id, ptr: data }
                }
                Type::Never
                | Type::Function(_)
                | Type::Generic(_)
                | Type::TypeParameter(_)
                | Type::FunctionTypeParameter(_)
                | Type::InferenceHole(_)
                | Type::Unresolved(_)
                | Type::RecursiveReference(_) => m.ice_with_span(
                    format!(
                        "not a value type; zeroed() for type {} is undefined",
                        m.types.get(type_id).kind_name()
                    ),
                    vm.eval_span,
                ),
            };
            Ok(VmResult::Value(value))
        }
        IntrinsicOperation::TypeId => {
            unreachable!("Handled by typer phase")
        }
        IntrinsicOperation::TypeName => {
            let TypedIntValue::U64(type_id_value) =
                execute_expr_return_exit!(vm, m, args[0])?.expect_int()
            else {
                m.ice_with_span("Malformed TypeName call", vm.eval_span)
            };
            let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_value as u32).unwrap());
            let string_id = *m.type_names.get(&type_id).unwrap();
            let value = string_id_to_value(vm, StackSelection::CallStackCurrent, m, string_id);
            Ok(VmResult::Value(value))
        }
        IntrinsicOperation::TypeSchema => {
            let TypedIntValue::U64(type_id_value) =
                execute_expr_return_exit!(vm, m, args[0])?.expect_int()
            else {
                m.ice_with_span("Malformed TypeSchema call", vm.eval_span)
            };
            let type_id = TypeId::from_nzu32(NonZeroU32::new(type_id_value as u32).unwrap());
            let Some(schema_static_value_id) = m.type_schemas.get(&type_id) else {
                m.ice_with_span(
                    format!("Missing type schema: {}", m.type_id_to_string(type_id)),
                    vm.eval_span,
                )
            };
            let value = static_value_to_vm_value(
                vm,
                StackSelection::CallStackCurrent,
                m,
                *schema_static_value_id,
            )?;
            Ok(VmResult::Value(value))
        }
        IntrinsicOperation::CompilerSourceLocation => {
            unreachable!("Operation CompilerSourceLocation is handled in typer")
        }
        IntrinsicOperation::BoolNegate => {
            let b = execute_expr_return_exit!(vm, m, args[0])?.expect_bool();
            Ok(Value::Bool(!b).into())
        }
        IntrinsicOperation::BitNot => {
            let int = execute_expr_return_exit!(vm, m, args[0])?.expect_int();
            Ok(Value::Int(int.bit_not()).into())
        }
        IntrinsicOperation::BitwiseBinop(kind) => {
            let inta = execute_expr_return_exit!(vm, m, args[0])?.expect_int();
            let intb = execute_expr_return_exit!(vm, m, args[1])?.expect_int();
            use std::ops::{Shl, Shr};
            let int_value = match kind {
                typer::IntrinsicBitwiseBinopKind::And => inta.bit_and(&intb),
                typer::IntrinsicBitwiseBinopKind::Or => inta.bit_or(&intb),
                typer::IntrinsicBitwiseBinopKind::Xor => inta.bit_xor(&intb),
                typer::IntrinsicBitwiseBinopKind::ShiftLeft => int_binop!(inta, &intb, shl),
                typer::IntrinsicBitwiseBinopKind::ShiftRight => int_binop!(inta, &intb, shr),
            };
            Ok(Value::Int(int_value).into())
        }
        IntrinsicOperation::PointerIndex => {
            // intern fn refAtIndex[T](self: Pointer, index: uword): T*
            let typ = m.named_types.get_nth(type_args, 0).type_id;
            let ptr = execute_expr_return_exit!(vm, m, args[0])?.expect_ptr();
            let index = execute_expr_return_exit!(vm, m, args[1])?.expect_int().expect_uword();
            let result = ptr + offset_at_index(&m.types, typ, index as usize);
            Ok(Value::Reference { type_id: return_type, ptr: result as *const u8 }.into())
        }
        IntrinsicOperation::Allocate | IntrinsicOperation::AllocateZeroed => {
            let zero = intrinsic_type == IntrinsicOperation::AllocateZeroed;
            let size_expr = args[0];
            let align_expr = args[1];
            let size = execute_expr_return_exit!(vm, m, size_expr)?.expect_int().expect_uword();
            let align = execute_expr_return_exit!(vm, m, align_expr)?.expect_int().expect_uword();
            let Ok(layout) = std::alloc::Layout::from_size_align(size, align) else {
                vm_crash(m, vm, format!("Rust didn't like this layout: size={size}, align={align}"))
            };
            let ptr = allocate(layout, zero);

            Ok(Value::Pointer(ptr.addr()).into())
        }
        IntrinsicOperation::Reallocate => {
            let old_ptr_expr = args[0];
            let old_size_expr = args[1];
            let old_align_expr = args[2];
            let new_size_expr = args[3];
            let old_ptr = execute_expr_return_exit!(vm, m, old_ptr_expr)?.expect_ptr();
            let old_size =
                execute_expr_return_exit!(vm, m, old_size_expr)?.expect_int().expect_uword();
            let align =
                execute_expr_return_exit!(vm, m, old_align_expr)?.expect_int().expect_uword();
            let new_size =
                execute_expr_return_exit!(vm, m, new_size_expr)?.expect_int().expect_uword();
            let layout = std::alloc::Layout::from_size_align(old_size, align).unwrap();
            let ptr = unsafe { std::alloc::realloc(old_ptr as *mut u8, layout, new_size) };
            Ok(Value::Pointer(ptr.addr()).into())
        }
        IntrinsicOperation::Free => {
            let ptr_expr = args[0];
            let size_expr = args[1];
            let align_expr = args[2];
            let ptr = execute_expr_return_exit!(vm, m, ptr_expr)?.expect_ptr();
            let size = execute_expr_return_exit!(vm, m, size_expr)?.expect_int().expect_u64();
            let align = execute_expr_return_exit!(vm, m, align_expr)?.expect_int().expect_u64();

            let layout =
                std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();
            unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };
            Ok(VmResult::UNIT)
        }
        IntrinsicOperation::MemCopy => {
            let [dst, src, count] = args[0..3] else { unreachable!() };
            let dst = execute_expr_return_exit!(vm, m, dst)?.expect_ptr();
            let src = execute_expr_return_exit!(vm, m, src)?.expect_ptr();
            let count = execute_expr_return_exit!(vm, m, count)?.expect_int().expect_uword();
            unsafe {
                std::ptr::copy_nonoverlapping(src as *const u8, dst as *mut u8, count as usize)
            };
            Ok(VmResult::UNIT)
        }
        IntrinsicOperation::MemSet => todo!(),
        IntrinsicOperation::MemEquals => {
            //intern fn compare(p1: Pointer, p2: Pointer, size: uword): i32
            let [p1, p2, size] = args[0..3] else { unreachable!() };
            let p1_usize = execute_expr_return_exit!(vm, m, p1)?.expect_ptr();
            let p2_usize = execute_expr_return_exit!(vm, m, p2)?.expect_ptr();
            let size = execute_expr_return_exit!(vm, m, size)?.expect_int().expect_uword();
            let p1 = p1_usize as *const u8;
            let p2 = p2_usize as *const u8;
            let p1 = unsafe { slice_from_raw_parts_checked(vm, m, p1, size as usize) };
            let p2 = unsafe { slice_from_raw_parts_checked(vm, m, p2, size as usize) };

            // Slice equality on [u8] results in memcmp
            let eq = p1 == p2;

            Ok(Value::Bool(eq).into())
        }
        IntrinsicOperation::Exit => {
            let TypedIntValue::I32(code) = execute_expr_return_exit!(vm, m, args[0])?.expect_int()
            else {
                unreachable!("malformed exit (code type)")
            };
            Ok(VmResult::Exit(VmExit { span: vm.eval_span, code }))
        }
        IntrinsicOperation::CompilerMessage => {
            let location_arg = execute_expr_return_exit!(vm, m, args[0])?;
            let level_arg = execute_expr_return_exit!(vm, m, args[1])?.expect_agg();
            let message_arg = execute_expr_return_exit!(vm, m, args[2])?;
            let location = unsafe { (location_arg.expect_agg() as *const K1SourceLocation).read() };
            let level = unsafe { (level_arg as *const CompilerMessageLevel).read() };
            let color = match level {
                CompilerMessageLevel::Info => colored::Color::White,
                CompilerMessageLevel::Warn => colored::Color::Yellow,
                CompilerMessageLevel::Error => colored::Color::Red,
            };
            let level_str = match level {
                CompilerMessageLevel::Info => "info",
                CompilerMessageLevel::Warn => "warn",
                CompilerMessageLevel::Error => "error",
            };
            let message = value_to_string_id(m, message_arg);
            let filename = unsafe { location.filename.to_str() };
            eprintln!(
                "[{}:{} {}] {}",
                filename,
                location.line,
                level_str,
                m.get_string(message).color(color)
            );
            Ok(VmResult::UNIT)
        }
        IntrinsicOperation::EmitString => {
            // intern fn emit(code: string): unit
            if !vm.allow_emits {
                return failf!(
                    vm.eval_span,
                    "emit() called in a context that does not allow emits"
                );
            }
            let string_value = execute_expr_return_exit!(vm, m, args[0])?;
            let string_id = value_to_string_id(m, string_value);
            vm.emits.push(CodeEmission::String(string_id));
            Ok(VmResult::UNIT)
        }
        IntrinsicOperation::BakeStaticValue => {
            // intern fn bakeStaticValue[T](value: T): u64
            let value_value = execute_expr_return_exit!(vm, m, args[0])?;
            let value_id = vm_value_to_static_value(m, vm, value_value, vm.eval_span)?;
            Ok(VmResult::Value(Value::Int(TypedIntValue::U64(value_id.as_u32() as u64))))
        }
    }
}

fn allocate(layout: std::alloc::Layout, zero: bool) -> *mut u8 {
    let ptr = if zero {
        unsafe { std::alloc::alloc_zeroed(layout) }
    } else {
        unsafe { std::alloc::alloc(layout) }
    };
    ptr
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_value(types: &Types, dst: *mut u8, value: Value) -> usize {
    //eprintln!("store value to = {:?}", dst);
    unsafe {
        match value {
            Value::Unit => {
                dst.write(typer::UNIT_BYTE_VALUE);
                size_of::<u8>()
            }
            Value::Bool(b) => {
                dst.write(b as u8);
                size_of::<u8>()
            }
            Value::Char(b) => {
                dst.write(b);
                size_of::<u8>()
            }
            Value::Int(value) => match value {
                TypedIntValue::U8(v) => {
                    dst.write(v);
                    size_of::<u8>()
                }
                TypedIntValue::I8(v) => {
                    (dst as *mut i8).write(v);
                    size_of::<i8>()
                }
                TypedIntValue::U16(v) => {
                    (dst as *mut u16).write(v);
                    size_of::<u16>()
                }
                TypedIntValue::I16(v) => {
                    (dst as *mut i16).write(v);
                    size_of::<i16>()
                }
                TypedIntValue::U32(v) | TypedIntValue::UWord32(v) => {
                    (dst as *mut u32).write(v);
                    size_of::<u32>()
                }
                TypedIntValue::I32(v) | TypedIntValue::IWord32(v) => {
                    (dst as *mut i32).write(v);
                    size_of::<i32>()
                }
                TypedIntValue::U64(v) | TypedIntValue::UWord64(v) => {
                    (dst as *mut u64).write(v);
                    size_of::<u64>()
                }
                TypedIntValue::I64(v) | TypedIntValue::IWord64(v) => {
                    (dst as *mut i64).write(v);
                    size_of::<i64>()
                }
            },
            Value::Float(value) => match value {
                TypedFloatValue::F32(f32) => {
                    let v = f32.to_bits();
                    (dst as *mut u32).write(v);
                    size_of::<u32>()
                }
                TypedFloatValue::F64(f64) => {
                    let v = f64.to_bits();
                    (dst as *mut u64).write(v);
                    size_of::<u64>()
                }
            },
            Value::Pointer(value) => {
                (dst as *mut usize).write(value);
                size_of::<usize>()
            }
            Value::Reference { ptr, .. } => {
                (dst as *mut usize).write(ptr.addr());
                size_of::<usize>()
            }
            Value::Agg { type_id, ptr } => {
                let struct_layout = types.get_layout(type_id);
                let size_bytes = struct_layout.size as usize;
                // Equivalent of memcpy
                debug!("copy_from {:?} -> {:?} {size_bytes}", dst, ptr);
                copy_aggregate(dst, ptr, size_bytes);
                size_bytes
            }
        }
    }
}

#[inline]
unsafe fn copy_aggregate(dst: *mut u8, src: *const u8, size: usize) {
    unsafe { dst.copy_from_nonoverlapping(src, size) }
}

pub fn load_value_copying_aggs(
    vm: &mut Vm,
    m: &TypedProgram,
    type_id: TypeId,
    ptr: *const u8,
) -> TyperResult<Value> {
    load_value(vm, m, type_id, ptr, true)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_value(
    vm: &mut Vm,
    m: &TypedProgram,
    type_id: TypeId,
    ptr: *const u8,
    copy_aggregates: bool,
) -> TyperResult<Value> {
    let span = vm.eval_span;
    if ptr.is_null() {
        return failf!(span, "Attempt to dereference null pointer");
    }
    if ptr.addr() < 256 {
        return failf!(span, "Attempt to dereference likely value pointer {}", ptr.addr());
    }
    let layout = m.types.get_layout(type_id);
    if layout.size == 0 {
        return failf!(
            span,
            "For now, cannot load a zero-sized type: {}",
            m.type_id_to_string(type_id)
        );
    };
    debug!("load of '{}' from {:?} {:?}", m.type_id_to_string(type_id), ptr, layout);
    match m.types.get(type_id) {
        Type::Static(stat) => load_value(vm, m, stat.inner_type_id, ptr, copy_aggregates),
        Type::Unit => Ok(Value::UNIT),
        Type::Char => {
            let byte = unsafe { ptr.read() };
            Ok(Value::Char(byte))
        }
        Type::Integer(integer_type) => {
            let int_value = unsafe {
                match integer_type {
                    IntegerType::U8 => TypedIntValue::U8(ptr.read()),
                    IntegerType::U16 => TypedIntValue::U16((ptr as *const u16).read()),
                    IntegerType::U32 => TypedIntValue::U32((ptr as *const u32).read()),
                    IntegerType::U64 => TypedIntValue::U64((ptr as *const u64).read()),
                    IntegerType::I8 => TypedIntValue::I8((ptr as *const i8).read()),
                    IntegerType::I16 => TypedIntValue::I16((ptr as *const i16).read()),
                    IntegerType::I32 => TypedIntValue::I32((ptr as *const i32).read()),
                    IntegerType::I64 => TypedIntValue::I64((ptr as *const i64).read()),
                    IntegerType::UWord(WordSize::W32) => {
                        unreachable!("We should have no W32 values in the vm")
                    }
                    IntegerType::UWord(WordSize::W64) => {
                        TypedIntValue::UWord64((ptr as *const u64).read())
                    }
                    IntegerType::IWord(WordSize::W32) => {
                        unreachable!("We should have no W32 values in the vm")
                    }
                    IntegerType::IWord(WordSize::W64) => {
                        TypedIntValue::IWord64((ptr as *const i64).read())
                    }
                }
            };
            Ok(Value::Int(int_value))
        }
        Type::Float(float_type) => {
            let float_value = unsafe {
                match float_type {
                    FloatType::F32 => TypedFloatValue::F32((ptr as *const f32).read()),
                    FloatType::F64 => TypedFloatValue::F64((ptr as *const f64).read()),
                }
            };
            Ok(Value::Float(float_value))
        }
        Type::Bool => {
            debug_assert_eq!(layout.size, 1);
            let byte = unsafe { *ptr };
            Ok(Value::Bool(byte != 0))
        }
        Type::Pointer => {
            debug_assert_eq!(layout.size as usize, size_of::<usize>());
            let value = unsafe { *(ptr as *const usize) };
            Ok(Value::Pointer(value))
        }
        Type::Reference(_reference_type) => {
            debug_assert_eq!(layout.size as usize, size_of::<usize>());

            // We're loading a reference, which means we are loading a usize
            let value: usize = unsafe { (ptr as *const usize).read() };
            let loaded_ptr = value as *const u8;
            Ok(Value::Reference { type_id, ptr: loaded_ptr })
        }
        Type::Struct(_)
        | Type::Enum(_)
        | Type::EnumVariant(_)
        | Type::Lambda(_)
        | Type::LambdaObject(_) => {
            debug_assert!(m.types.is_aggregate_repr(type_id));
            if copy_aggregates {
                let layout = m.types.get_layout(type_id);
                let frame_ptr = vm.stack.push_raw_copy_layout(layout, ptr);
                Ok(Value::Agg { type_id, ptr: frame_ptr })
            } else {
                Ok(Value::Agg { type_id, ptr })
            }
        }
        //
        Type::Never
        | Type::Function(_)
        | Type::Generic(_)
        | Type::TypeParameter(_)
        | Type::FunctionTypeParameter(_)
        | Type::InferenceHole(_)
        | Type::Unresolved(_)
        | Type::RecursiveReference(_) => unreachable!("Not a value type"),
    }
}

fn aligned_to(ptr: *const u8, align_bytes: usize) -> *const u8 {
    let bytes_needed = ptr.align_offset(align_bytes);
    unsafe { ptr.byte_add(bytes_needed) }
}

fn aligned_to_mut(ptr: *mut u8, align_bytes: usize) -> *mut u8 {
    let bytes_needed = ptr.align_offset(align_bytes);
    unsafe { ptr.byte_add(bytes_needed) }
}

pub fn build_enum_at_location(
    dst: *mut u8,
    types: &Types,
    variant_type_id: TypeId,
    payload_value: Option<Value>,
) -> (*const u8, Layout) {
    let variant_type = types.get(variant_type_id).expect_enum_variant();
    let enum_layout = types.get_layout(variant_type_id);
    // TODO(vm): Represent no-payload enums as
    // an int-based Value variant, not aggregates
    // if enum_type.is_no_payload() {
    //     // Simple integer value
    //     debug!("TODO: optimize for no payload enums")
    // }

    let start_dst = aligned_to_mut(dst, enum_layout.align as usize);

    let _tag_written = store_value(types, start_dst, Value::Int(variant_type.tag_value));
    if let Some(payload_value) = payload_value {
        let payload_offset_bytes = types.enum_variant_payload_offset_bytes(variant_type);
        let payload_base_aligned = unsafe { start_dst.byte_add(payload_offset_bytes) };
        store_value(types, payload_base_aligned, payload_value);
    }
    (start_dst, enum_layout)
}

fn build_struct_unaligned(dst: *mut u8, types: &Types, struct_type_id: TypeId, members: &[Value]) {
    let struct_layout = &types.get_struct_layout(struct_type_id);

    for (value, field_offset) in members.iter().zip(struct_layout.field_offsets.iter()) {
        // Go to offset
        let field_dst = unsafe { dst.byte_add(*field_offset as usize) };
        store_value(types, field_dst, *value);
    }
}

pub fn offset_at_index(types: &Types, type_id: TypeId, index: usize) -> usize {
    let size_bytes = types.get_layout(type_id).size as usize;
    index * size_bytes
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn gep_struct_field(
    types: &Types,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
) -> *const u8 {
    let struct_type = types.get_struct_layout(struct_type);
    let field_offset_bytes = struct_type.field_offsets[field_index];

    let field_ptr = unsafe { struct_ptr.byte_add(field_offset_bytes as usize) };
    field_ptr
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_struct_field(
    vm: &mut Vm,
    m: &TypedProgram,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
    copy: bool,
) -> TyperResult<Value> {
    let field_ptr = gep_struct_field(&m.types, struct_type, struct_ptr, field_index);
    let field = &m.types.get(struct_type).expect_struct().fields[field_index];
    let value = load_value(vm, m, field.type_id, field_ptr, copy)?;
    // load_struct_field x (offset=0) of type GenericPoint[i32] is Int(I32(4)). Full struct: [4, 0, 0, 0, 0, 0, 0, 0]
    // load_struct_field y (offset=0) of type GenericPoint[i32] is Int(I32(4)). Full struct: [4, 0, 0, 0, 0, 0, 0, 0]

    debug!(
        "load_struct_field {} (offset={}) of type {} is {:?}. Full struct: {:?}",
        m.ident_str(field.name),
        field_ptr.addr() - struct_ptr.addr(),
        m.type_id_to_string(struct_type),
        value,
        unsafe {
            std::slice::from_raw_parts(struct_ptr, m.types.get_layout(struct_type).size as usize)
        }
    );
    Ok(value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn gep_enum_payload(
    types: &Types,
    variant_type: &TypedEnumVariant,
    enum_ptr: *const u8,
) -> *const u8 {
    let payload_offset_bytes = types.enum_variant_payload_offset_bytes(variant_type);

    let payload_ptr = unsafe { enum_ptr.byte_add(payload_offset_bytes) };
    payload_ptr
}

nz_u32_id!(FrameIndex);

pub struct Stack {
    allocation: Box<[u8]>,
    frames: Vec<StackFrame>,
    locals: FxHashMap<u32, FxHashMap<VariableId, Value>>,
    cursor: *const u8,
}

#[derive(Debug, Clone, Copy)]
pub struct StackFrame {
    index: u32,
    base_ptr: *const u8,
    debug_name: Option<Ident>,
    call_span: Option<SpanId>,
}

impl StackFrame {
    pub fn make(
        index: u32,
        base_ptr: *const u8,
        debug_name: Option<Ident>,
        call_span: Option<SpanId>,
    ) -> StackFrame {
        StackFrame { index, base_ptr, debug_name, call_span }
    }
}

impl Stack {
    pub fn make(size: usize) -> Stack {
        eprintln!("make stack {size}");
        let allocation: Box<[u8]> = vec![0; size].into();
        let base_ptr = allocation.as_ptr();
        let frames_cap = if size == 0 { 0 } else { 512 };
        Self {
            allocation,
            frames: Vec::with_capacity(frames_cap),
            locals: FxHashMap::with_capacity(frames_cap),
            cursor: base_ptr,
        }
    }

    pub fn reset(&mut self) {
        self.cursor = self.base_ptr();
        unsafe {
            core::ptr::write_bytes(self.allocation.as_mut_ptr(), 0, self.allocation.len());
        }
        self.frames.clear();
    }

    fn push_new_frame(&mut self, name: Option<Ident>, call_span: Option<SpanId>) -> StackFrame {
        let index = self.frames.len() as u32;
        let base_ptr = self.cursor;
        let frame = StackFrame::make(index, base_ptr, name, call_span);
        self.push_frame(frame)
    }

    fn push_frame(&mut self, frame: StackFrame) -> StackFrame {
        self.frames.push(frame);
        *self.frames.last().unwrap()
    }

    fn pop_frame(&mut self) -> StackFrame {
        let f = self.frames.pop().unwrap();
        self.locals.entry(f.index).or_default().clear();
        self.cursor = f.base_ptr;
        f
    }

    fn current_frame(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    #[inline]
    pub fn base_ptr(&self) -> *const u8 {
        self.allocation.as_ptr()
    }

    #[inline]
    pub fn base_addr(&self) -> usize {
        self.base_ptr().addr()
    }

    #[inline]
    pub fn end_ptr(&self) -> *const u8 {
        self.allocation.as_ptr_range().end
    }

    #[inline]
    pub fn current_offset_bytes(&self) -> usize {
        self.cursor.addr() - self.base_addr()
    }

    #[inline]
    fn cursor_mut(&self) -> *mut u8 {
        self.cursor as *mut u8
    }

    #[inline]
    fn check_bounds(&self, ptr: *const u8) {
        if self.allocation.is_empty() {
            panic!("check_bounds on a stack with no allocation")
        }
        let ptr_addr = ptr.addr();
        let base_addr = self.base_addr();
        let end_addr = self.end_ptr().addr();
        let oob = ptr_addr < base_addr || ptr_addr >= end_addr;
        if oob {
            panic!(
                "Out of bounds access to stack frame: {} < {} < {}",
                base_addr, ptr_addr, end_addr
            );
        }
    }

    pub fn push_slice(&mut self, slice: &[u8]) -> *const u8 {
        let slice_stack_ptr = self.cursor_mut();
        let slice_len = slice.len();
        if cfg!(debug_assertions) {
            self.check_bounds(unsafe { slice_stack_ptr.byte_add(slice_len) })
        }

        unsafe {
            // Copy bytes using memcpy:
            let dst_slice = std::slice::from_raw_parts_mut(slice_stack_ptr, slice_len);
            // eprintln!("copy_from_slice {:?} {:?}", slice.as_ptr(), slice_stack_ptr);
            dst_slice.copy_from_slice(slice);
        }
        // Would be nice to re-use the 'end' of the slice ptr
        self.cursor = unsafe { self.cursor.byte_add(slice_len) };
        slice_stack_ptr.cast_const()
    }

    fn align_to_bytes(&mut self, align_bytes: usize) {
        self.cursor = aligned_to(self.cursor, align_bytes)
    }

    pub fn push_struct_values(
        &mut self,
        types: &Types,
        struct_type_id: TypeId,
        members: &[Value],
    ) -> Value {
        let struct_layout = types.get_layout(struct_type_id);
        let struct_base = self.push_layout_uninit(struct_layout);
        build_struct_unaligned(struct_base, types, struct_type_id, members);
        self.advance_cursor(struct_layout.size as usize);
        Value::Agg { type_id: struct_type_id, ptr: struct_base }
    }

    pub fn push_enum(
        &mut self,
        types: &Types,
        variant_type_id: TypeId,
        payload_value: Option<Value>,
    ) -> *const u8 {
        let dst = self.cursor_mut();
        let (enum_base, layout) =
            build_enum_at_location(dst, types, variant_type_id, payload_value);
        self.advance_cursor(layout.size as usize);
        enum_base
    }

    pub fn push_value(&mut self, types: &Types, value: Value) -> *const u8 {
        let layout = types.get_layout(value.get_type());
        self.align_to_bytes(layout.align as usize);
        self.push_value_no_align(types, value)
    }

    // We may not want to align, in case we're pushing this value as part of a packed struct.
    // Or if our caller already aligned us
    pub fn push_value_no_align(&mut self, types: &Types, value: Value) -> *const u8 {
        let dst: *mut u8 = self.cursor_mut();

        let written: usize = store_value(types, dst, value);
        self.advance_cursor(written);
        dst.cast_const()
    }

    pub fn advance_cursor(&mut self, count: usize) {
        self.cursor = unsafe { self.cursor.byte_add(count) };
    }

    pub fn push_t<T: Copy>(&mut self, value: T) -> *const u8 {
        let c = self.push_layout_uninit(Layout::from_rust_type::<T>());
        unsafe {
            (c as *mut T).write(value);
        }
        c
    }

    pub fn push_layout_uninit(&mut self, layout: Layout) -> *mut u8 {
        if cfg!(debug_assertions) {
            self.check_bounds(unsafe { self.cursor.byte_add(layout.size as usize) })
        }
        self.align_to_bytes(layout.align as usize);
        let c = self.cursor_mut();
        self.advance_cursor(layout.size as usize);
        c
    }

    #[inline]
    pub fn push_usize(&mut self, value: usize) -> *const u8 {
        self.push_t(value)
    }

    #[inline]
    pub fn push_ptr_uninit(&mut self) -> *mut u8 {
        self.push_usize(0).cast_mut()
    }

    /// Push a raw copy of `size_bytes` from `src_ptr` into the frame buffer.
    /// The copy is aligned to `align_bytes`.
    #[inline]
    fn push_raw_copy(
        &mut self,
        size_bytes: usize,
        align_bytes: usize,
        src_ptr: *const u8,
    ) -> *const u8 {
        self.align_to_bytes(align_bytes);
        unsafe {
            debug!("push_raw_copy {:?} {}", src_ptr, size_bytes);
            let src_slice = std::slice::from_raw_parts(src_ptr, size_bytes);
            self.push_slice(src_slice)
        }
    }

    fn push_raw_copy_layout(&mut self, layout: Layout, src_ptr: *const u8) -> *const u8 {
        self.push_raw_copy(layout.size as usize, layout.align as usize, src_ptr)
    }

    pub fn to_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.base_ptr(), self.current_offset_bytes()) }
    }
}

fn execute_match(
    vm: &mut Vm,
    m: &mut TypedProgram,
    match_expr: &TypedMatchExpr,
) -> TyperResult<VmResult> {
    for stmt in &match_expr.initial_let_statements {
        execute_stmt(vm, m, *stmt)?;
    }
    for (arm_index, arm) in match_expr.arms.iter().enumerate() {
        let condition_result = return_exit!(execute_matching_condition(vm, m, &arm.condition)?);
        let condition_bool = condition_result.expect_bool();
        debug!("arm {arm_index} cond is {condition_bool}");
        if condition_bool {
            let res = execute_expr(vm, m, arm.consequent_expr)?;
            debug!("match res is {:?}", res);
            return Ok(res);
        } else {
            continue;
        }
    }
    failf!(match_expr.span, "Malformed match: No match arms executed")
}

fn execute_matching_condition(
    vm: &mut Vm,
    m: &mut TypedProgram,
    cond: &MatchingCondition,
) -> TyperResult<VmResult> {
    for instr in cond.instrs.iter() {
        match instr {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                execute_stmt(vm, m, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let cond_result = execute_expr_return_exit!(vm, m, *value)?;
                let cond_bool = cond_result.expect_bool();
                if !cond_bool {
                    return Ok(Value::FALSE.into());
                }
            }
        }
    }
    Ok(Value::TRUE.into())
}

/// Please don't hang on to this reference for very long
pub fn value_to_rust_str(value: Value) -> &'static str {
    let ptr = value.expect_agg();
    let k1_string = unsafe { (ptr as *const k1_types::K1Buffer).read() };
    unsafe { k1_string.to_str() }
}

pub fn value_to_string_id(m: &mut TypedProgram, value: Value) -> StringId {
    let rust_str = value_to_rust_str(value);
    m.ast.strings.intern(rust_str)
}

pub fn value_to_ident(m: &mut TypedProgram, value: Value) -> Ident {
    let rust_str = value_to_rust_str(value);
    m.ast.idents.intern(rust_str)
}

pub fn string_id_to_value(
    vm: &mut Vm,
    dst_stack: StackSelection,
    m: &TypedProgram,
    string_id: StringId,
) -> Value {
    let char_buffer_type_id = m.types.get(STRING_TYPE_ID).expect_struct().fields[0].type_id;
    let string_layout = m.types.get_layout(STRING_TYPE_ID);
    debug_assert_eq!(string_layout, m.types.get_layout(char_buffer_type_id));

    let s = m.get_string(string_id);
    let k1_string = k1_types::K1Buffer { len: s.len(), data: s.as_ptr() };
    debug_assert_eq!(size_of_val(&k1_string), string_layout.size as usize);

    let string_stack_addr = vm.get_destination_stack(dst_stack).push_t(k1_string);
    Value::Agg { type_id: STRING_TYPE_ID, ptr: string_stack_addr }
}

/// VM values contain a lot of pointers to the VM's stack and heap
/// (which is currently just the host's heap)
/// We need to convert these into 'constants' so that we can embed
/// them in a binary, for example LLVM. This function does that
/// by recursively 'loading' all the values out of the VM value.
///
/// Obviously not all types are supported; only things you can reasonably
/// embed in a binary; raw pointers for example are out.
///
/// For complex types (not a char array) like a big slice of structs, I think we may just have to use
/// some sort of 'embed binary data' feature of the backend
pub fn vm_value_to_static_value(
    m: &mut TypedProgram,
    vm: &mut Vm,
    vm_value: Value,
    span: SpanId,
) -> TyperResult<StaticValueId> {
    debug!("vm_to_static: {:?}", vm_value);
    let v = match vm_value {
        Value::Unit => StaticValue::Unit,
        Value::Bool(b) => StaticValue::Boolean(b),
        Value::Char(c) => StaticValue::Char(c),
        Value::Int(typed_integer_value) => StaticValue::Int(typed_integer_value),
        Value::Float(typed_float_value) => StaticValue::Float(typed_float_value),
        Value::Pointer(value) => {
            if value == 0 {
                StaticValue::NullPointer
            } else {
                return failf!(
                    span,
                    "Raw pointer ({:0x}) cannot be converted to a static value; use a Reference or a plain value type instead.",
                    value
                );
            }
        }
        Value::Reference { .. } => {
            // Now this, I can do. Load the value and bake it into the binary
            // This is just a de-reference
            // Needs to become a global.
            // Rely on the VM's code to load it, then make a K1 'global' to hold it?

            //let _loaded_value = vm::load_value(vm, m, *type_id, *ptr, true, span).unwrap();
            todo!("Introduce CompileTimeValue::Reference");
        }
        Value::Agg { type_id, ptr } => {
            if type_id == STRING_TYPE_ID {
                let box_str = value_to_string_id(m, vm_value);
                StaticValue::String(box_str)
            } else if m.types.get(type_id).as_buffer_instance().is_some() {
                let (buffer, element_type) = value_as_buffer(m, vm_value);
                let mut elements = EcoVec::with_capacity(buffer.len);
                for index in 0..buffer.len {
                    let elem_vm = get_buffer_element(vm, m, buffer, element_type, index)
                        .unwrap_or_else(|e| {
                            m.ice_with_span(
                                format!("Failed to load buffer element {index}: {}", e),
                                span,
                            )
                        });
                    let elem_static = vm_value_to_static_value(m, vm, elem_vm, span)?;
                    elements.push(elem_static);
                }
                StaticValue::Buffer(StaticBuffer { elements, type_id })
            } else {
                let typ = m.types.get(type_id);
                match typ {
                    Type::Struct(struct_type) => {
                        let mut field_value_ids = EcoVec::with_capacity(struct_type.fields.len());
                        let struct_fields = struct_type.fields.clone();
                        for (index, _) in struct_fields.iter().enumerate() {
                            let field_value =
                                load_struct_field(vm, m, type_id, ptr, index, false).unwrap();
                            let field_static_value_id =
                                vm_value_to_static_value(m, vm, field_value, span)?;
                            field_value_ids.push(field_static_value_id)
                        }
                        StaticValue::Struct(StaticStruct { type_id, fields: field_value_ids })
                    }
                    Type::Enum(enum_type) => {
                        let tag =
                            load_value(vm, m, enum_type.tag_type, ptr, false).unwrap().expect_int();
                        let variant =
                            enum_type.variants.iter().find(|v| v.tag_value == tag).unwrap();
                        let variant_type_id = variant.my_type_id;
                        let variant_index = variant.index;

                        let payload = match variant.payload {
                            None => None,
                            Some(payload_type) => {
                                let payload_ptr = gep_enum_payload(&m.types, variant, ptr);
                                let payload_value =
                                    load_value(vm, m, payload_type, payload_ptr, false).unwrap();
                                let static_value_id =
                                    vm_value_to_static_value(m, vm, payload_value, span)?;
                                Some(static_value_id)
                            }
                        };
                        StaticValue::Enum(StaticEnum {
                            variant_type_id,
                            variant_index,
                            typed_as_enum: true,
                            payload,
                        })
                    }
                    Type::EnumVariant(_enum_variant) => {
                        todo!("enum variant vm -> static")
                    }
                    _ => unreachable!("aggregate should be struct or enum"),
                }
            }
        }
    };
    let id = m.static_values.add(v);
    Ok(id)
}

pub fn value_as_buffer(m: &TypedProgram, buffer_value: Value) -> (k1_types::K1Buffer, TypeId) {
    let element_type =
        m.types.get(buffer_value.get_type()).as_buffer_instance().unwrap().type_args[0];
    let ptr = buffer_value.expect_agg();
    let buffer_ptr = ptr as *const k1_types::K1Buffer;
    let buffer = unsafe { buffer_ptr.read() };
    (buffer, element_type)
}

pub fn get_buffer_element(
    vm: &mut Vm,
    m: &TypedProgram,
    buffer: K1Buffer,
    elem_type: TypeId,
    index: usize,
) -> TyperResult<Value> {
    let data_ptr = buffer.data;

    let elem_offset = offset_at_index(&m.types, elem_type, index);
    let elem_ptr = unsafe { data_ptr.byte_add(elem_offset) };
    load_value(vm, m, elem_type, elem_ptr, true)
}

#[allow(unused)]
fn render_debug_vm_result(
    w: &mut impl std::fmt::Write,
    vm: &mut Vm,
    m: &TypedProgram,
    result: VmResult,
) {
    match result {
        VmResult::Value(value) => {
            w.write_str("VALUE ").unwrap();
            render_debug_value(w, vm, m, value)
        }
        VmResult::Break(value) => {
            w.write_str("BREAK ").unwrap();
            render_debug_value(w, vm, m, value)
        }
        VmResult::Return(value) => {
            w.write_str("RETURN ").unwrap();
            render_debug_value(w, vm, m, value)
        }
        VmResult::Exit(vm_exit) => write!(w, "EXIT {}", vm_exit.code).unwrap(),
    }
}

#[allow(unused)]
fn debug_vm_result_to_string(vm: &mut Vm, m: &TypedProgram, result: VmResult) -> String {
    let mut s = String::new();
    render_debug_vm_result(&mut s, vm, m, result);
    s
}

fn render_debug_value(w: &mut impl std::fmt::Write, vm: &mut Vm, m: &TypedProgram, value: Value) {
    //eprintln!("render debug of {:?} and {}", value, m.type_id_to_string(value.get_type()));
    match value {
        Value::Unit => w.write_str("()").unwrap(),
        Value::Bool(b) => {
            if b {
                w.write_str("true").unwrap()
            } else {
                w.write_str("false").unwrap()
            }
        }
        Value::Char(c) => write!(w, "{}", c as char).unwrap(),
        Value::Int(int) => write!(w, "{}", int).unwrap(),
        Value::Float(float) => write!(w, "{}", float).unwrap(),
        Value::Pointer(p) => write!(w, "{}", p).unwrap(),
        Value::Reference { type_id, ptr } => {
            let type_to_load = m.types.get_type_id_dereferenced(type_id);
            match m.types.get(type_to_load) {
                Type::Function(_) => write!(w, "<FN PTR: {}>", ptr.addr()).unwrap(),
                _ => match load_value(vm, m, type_to_load, ptr, true) {
                    Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                    Ok(loaded) => render_debug_value(w, vm, m, loaded),
                },
            };
        }
        Value::Agg { type_id, ptr } => {
            match m.types.get(type_id) {
                st @ Type::Struct(struct_type) => {
                    if let Some(buffer_type) = st.as_buffer_instance() {
                        let buffer_ptr = ptr as *const k1_types::K1Buffer;
                        let buffer = unsafe { buffer_ptr.read() };
                        let len = buffer.len;
                        let data_ptr = buffer.data;
                        write!(w, "<buffer len={len} ").unwrap();

                        let preview_count = std::cmp::min(len, 10);
                        w.write_str("[").unwrap();
                        let elem_type = buffer_type.type_args[0];
                        for i in 0..preview_count {
                            let elem_offset = offset_at_index(&m.types, elem_type, i);
                            let elem_ptr = unsafe { data_ptr.byte_add(elem_offset) };
                            match load_value(vm, m, elem_type, elem_ptr, true) {
                                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                                Ok(loaded) => render_debug_value(w, vm, m, loaded),
                            };
                            if i < preview_count - 1 {
                                w.write_str(", ").unwrap();
                            }
                        }
                        w.write_str("]>").unwrap();
                    } else {
                        w.write_str("{ ").unwrap();
                        for (field_index, f) in struct_type.fields.iter().enumerate() {
                            write!(w, "{}: ", m.ident_str(f.name)).unwrap();
                            match load_struct_field(vm, m, type_id, ptr, field_index, true) {
                                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                                Ok(loaded) => render_debug_value(w, vm, m, loaded),
                            };
                            if field_index != struct_type.fields.len() - 1 {
                                write!(w, ", ").unwrap();
                            }
                        }
                        write!(w, " }}").unwrap();
                    }
                }
                Type::Enum(enum_type) => match load_value(vm, m, enum_type.tag_type, ptr, true) {
                    Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                    Ok(tag) => {
                        let tag = tag.expect_int();
                        match enum_type.variants.iter().find(|v| v.tag_value == tag) {
                            None => write!(w, "<ERROR Bad Enum Tag>").unwrap(),
                            Some(variant) => write!(w, "{}", m.ident_str(variant.name)).unwrap(),
                        }
                    }
                },
                _ => write!(w, "<cannot render {}", m.type_id_to_string(type_id)).unwrap(),
            };
        }
    };
}

fn debug_value_to_string(vm: &mut Vm, m: &TypedProgram, value: Value) -> String {
    let mut w = String::new();
    render_debug_value(&mut w, vm, m, value);
    w
}

fn integer_cast(
    types: &Types,
    int_to_cast: TypedIntValue,
    target_type_id: TypeId,
) -> TypedIntValue {
    match types.get(target_type_id).expect_integer() {
        IntegerType::U8 => int_to_cast.to_u8().into(),
        IntegerType::U16 => int_to_cast.to_u16().into(),
        IntegerType::U32 => int_to_cast.to_u32().into(),
        IntegerType::U64 => int_to_cast.to_u64().into(),
        IntegerType::I8 => (int_to_cast.to_u8() as i8).into(),
        IntegerType::I16 => (int_to_cast.to_u16() as i16).into(),
        IntegerType::I32 => (int_to_cast.to_u32() as i32).into(),
        IntegerType::I64 => (int_to_cast.to_u64() as i64).into(),
        IntegerType::UWord(WordSize::W32) => unreachable!(),
        IntegerType::UWord(WordSize::W64) => TypedIntValue::UWord64(int_to_cast.to_u64()),
        IntegerType::IWord(WordSize::W32) => unreachable!(),
        IntegerType::IWord(WordSize::W64) => TypedIntValue::IWord64(int_to_cast.to_u64() as i64),
    }
}

unsafe fn slice_from_raw_parts_checked<'a, T>(
    vm: &Vm,
    m: &TypedProgram,
    data: *const T,
    len: usize,
) -> &'a [T] {
    let null_ok = data.is_null() && len == 0;
    if cfg!(debug_assertions) {
        if !data.is_aligned() {
            m.ice_with_span(
                format!("slice_from_raw_parts: ptr is unaligned: {:?}", data),
                vm.eval_span,
            );
        }
        let is_null = data.is_null();
        let is_zst = std::mem::size_of::<T>() == 0;
        if !null_ok {
            if !is_zst && is_null {
                let frame_names = make_stack_trace(m, &vm.stack);
                eprintln!("STACK TRACE\n{}", frame_names);
                m.ice_with_span(
                    format!(
                        "slice_from_raw_parts: data={:?} len={len} size={}",
                        data,
                        std::mem::size_of::<T>()
                    ),
                    vm.eval_span,
                );
            }
        }
    }
    let data = if null_ok { core::ptr::dangling() } else { data };
    unsafe { std::slice::from_raw_parts(data, len) }
}

fn make_stack_trace(m: &TypedProgram, stack: &Stack) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    stack.frames.iter().for_each(|f| {
        write!(&mut s, "{}", m.ident_str_opt(f.debug_name)).unwrap();
        match f.call_span {
            None => {}
            Some(span) => {
                let (source, line) = m.get_span_location(span);
                write!(&mut s, " {}:{}", source.filename, line.line_number()).unwrap()
            }
        };
        writeln!(&mut s).unwrap();
    });
    s
}

#[track_caller]
fn vm_crash(m: &TypedProgram, vm: &Vm, msg: impl AsRef<str>) -> ! {
    eprintln!("{}", make_stack_trace(m, &vm.stack));
    m.ice_with_span(msg, vm.eval_span)
}

//mod c_mem {
//    use std::os::raw::{c_size_t, c_void};
//    extern "C" {
//        fn malloc(size: c_size_t) -> *mut c_void;
//        fn realloc(ptr: *mut c_void, size: c_size_t) -> *mut c_void;
//        fn free(ptr: *mut c_void);
//    }
//}
