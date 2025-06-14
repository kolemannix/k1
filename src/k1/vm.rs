use std::{
    num::NonZeroU32,
    sync::atomic::{AtomicU64, Ordering},
};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use itertools::Itertools;
use log::debug;
use smallvec::{smallvec, SmallVec};

use crate::{
    compiler::WordSize,
    errf, failf,
    lex::SpanId,
    nz_u32_id,
    parse::{Identifier, NumericWidth, StringId},
    typer::{
        self, make_error, make_fail_span,
        types::{
            IntegerType, StructTypeField, Type, TypeId, TypedEnumVariant, Types, BOOL_TYPE_ID,
            CHAR_TYPE_ID, POINTER_TYPE_ID, STRING_TYPE_ID, UNIT_TYPE_ID,
        },
        BinaryOpKind, CastType, IntrinsicFunction, Layout, MatchingCondition,
        MatchingConditionInstr, SimpleNamedType, StaticValue, TypedExpr, TypedExprId,
        TypedFloatValue, TypedGlobalId, TypedIntValue, TypedMatchExpr, TypedProgram, TypedStmtId,
        TyperResult, VariableId,
    },
};

#[cfg(test)]
mod vm_test;

mod binop;

/// Bit-for-bit mappings of K1 types
pub mod k1_types {

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct K1Buffer {
        pub len: usize,
        pub data: *const u8,
    }

    impl K1Buffer {
        pub unsafe fn to_slice<'a, T>(self) -> &'a [T] {
            unsafe { std::slice::from_raw_parts(self.data as *const T, self.len) }
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
}

impl Vm {
    pub fn zero() -> Vm {
        Vm {
            globals: FxHashMap::new(),
            static_stack: Stack::make(0),
            stack: Stack::make(0),
            eval_depth: AtomicU64::new(0),
            eval_span: SpanId::NONE,
        }
    }
    pub fn reset(&mut self) {
        // Note that we don't de-allocate anything
        self.static_stack.reset();
        self.stack.reset();
        self.globals.clear();

        self.eval_depth.store(0, Ordering::Relaxed);
        self.eval_span = SpanId::NONE;
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
        }
    }

    pub fn dump(&mut self, m: &TypedProgram) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for index in 0..self.stack.frames.len() {
            let frame_string = self.dump_frame(m, index);
            write!(w, "{}", frame_string).unwrap();
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

    pub fn get_current_local(&self, variable_id: VariableId) -> Option<Value> {
        self.get_local(self.stack.current_frame(), variable_id)
    }

    pub fn get_local(&self, frame_index: u32, variable_id: VariableId) -> Option<Value> {
        let frame_locals = self.stack.locals.get(&frame_index).unwrap();
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
        writeln!(w, "Frame:  {}", m.name_of_opt(frame.debug_name)).unwrap();
        writeln!(w, "Base :  {:?})", frame.base_ptr).unwrap();
        writeln!(w, "Locals").unwrap();
        let locals = self
            .stack
            .locals
            .get(&(frame_index as u32))
            .unwrap()
            .iter()
            .map(|p| (*p.0, *p.1))
            .collect_vec();
        let _debug_frame = self.stack.push_new_frame(None);
        for (k, v) in locals.into_iter() {
            let var = m.variables.get(k);
            let v_name = var.name;
            let hidden = var.user_hidden;
            if hidden {
                continue;
            }
            write!(w, "  {}: {} = ", m.name_of(v_name), m.type_id_to_string(v.get_type())).unwrap();
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
pub enum VmResult {
    Value(Value),
    Break(Value),
    Return(Value),
    Exit(i32),
}

impl VmResult {
    pub const UNIT: VmResult = VmResult::Value(Value::Unit);

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

    pub fn kind_str(&self) -> &'static str {
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

    fn expect_agg(&self) -> *const u8 {
        match self {
            Value::Agg { ptr, .. } => *ptr,
            _ => unreachable!("expect_agg on value {:?}", self),
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

pub fn execute_single_expr(m: &mut TypedProgram, expr: TypedExprId) -> TyperResult<Value> {
    // DO NOT RETURN EARLY FROM THIS FUNCTION
    // without replacing the vm
    let mut vm = std::mem::take(&mut m.vm).unwrap();

    // Tell the code we're about to execute that this is static
    // Useful for conditional compilation to branch and do what makes sense
    // in an interpreted context
    if cfg!(debug_assertions) {
        // The very first global defines IS_STATIC, so we won't find it
        if !m.globals.is_empty() {
            let k1_static_global = m.globals.get(GLOBAL_ID_STATIC);
            debug_assert_eq!(
                m.name_of(m.variables.get(k1_static_global.variable_id).name),
                "IS_STATIC"
            );
        }
    }
    if !m.globals.is_empty() {
        vm.globals.insert(GLOBAL_ID_STATIC, Value::Bool(true));
    }

    vm.stack.push_new_frame(None);
    let span = m.exprs.get(expr).get_span();
    let v = match execute_expr(&mut vm, m, expr) {
        Err(e) => Err(e),
        Ok(VmResult::Value(value)) => Ok(value),
        Ok(VmResult::Exit(code)) => failf!(span, "Static execution exited with code: {code}"),
        Ok(VmResult::Return(_value)) => {
            failf!(
                span,
                "Return result from top-level expression. This might become the norm: {}",
                m.expr_to_string(expr)
            )
        }
        Ok(VmResult::Break(_)) => unreachable!("Break result from top-level expression"),
    };
    *m.vm = Some(vm);
    v
}

macro_rules! return_exit {
    ($result:expr) => {{
        match $result {
            VmResult::Value(v) => v,
            VmResult::Break(_) => panic!("Expected Value but got Break"),
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
            VmResult::Break(_) => {
                Err(errf!($m.exprs.get($expr).get_span(), "Expected Value but got Break"))
            }
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

    //eprint!("{}", " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize));
    // let mut s = String::new();
    // std::io::stdin().read_line(&mut s).unwrap();

    let result: TyperResult<VmResult> = match m.exprs.get(expr) {
        TypedExpr::Unit(_) => Ok(Value::Unit.into()),
        TypedExpr::Char(byte, _) => Ok(Value::Char(*byte).into()),
        TypedExpr::Bool(b, _) => Ok(Value::Bool(*b).into()),
        TypedExpr::Integer(typed_integer_expr) => Ok(Value::Int(typed_integer_expr.value).into()),
        TypedExpr::Float(typed_float_expr) => Ok(Value::Float(typed_float_expr.value).into()),
        TypedExpr::String(s, _) => {
            let string_value = k1_str_to_value(vm, StackSelection::CallStackCurrent, m, *s);
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

            let struct_base = vm.stack.push_struct_values(&m.types, s_type_id, &values);
            Ok(Value::Agg { type_id: s_type_id, ptr: struct_base }.into())
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
                let (field_ptr, _) = gep_struct_field(
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
                    false,
                )?;
                Ok(field_value.into())
            }
        }
        TypedExpr::Variable(variable_expr) => {
            let v_id = variable_expr.variable_id;
            let variable = m.variables.get(v_id);
            if let Some(global_id) = variable.global_id {
                // Handle global
                // Case 1: It's in the VM because someone mutated it
                //         OR we already evaluated it during this execution
                // Case 2: Grab it from the module
                match vm.globals.get(&global_id) {
                    Some(global_value) => Ok(global_value.into()),
                    None => {
                        let global = m.globals.get(global_id);
                        let Some(global_value) = global.initial_value else {
                            return failf!(
                                variable_expr.span,
                                "Global {} not initialized",
                                m.name_of(variable.name)
                            );
                        };
                        let vm_value = static_value_to_vm_value(
                            vm,
                            StackSelection::StaticSpace,
                            m,
                            m.static_values.get(global_value),
                        )?;
                        let final_value = if global.is_referencing {
                            let ptr = vm.static_stack.push_ptr_uninit();
                            store_value(&m.types, ptr, vm_value);
                            Value::Reference { type_id: global.ty, ptr }
                        } else {
                            vm_value
                        };

                        vm.globals.insert(global_id, final_value);
                        Ok(final_value.into())
                    }
                }
            } else {
                let Some(v) = vm.get_current_local(v_id) else {
                    eprintln!("{}", vm.dump(m));
                    return failf!(
                        variable_expr.span,
                        "Variable missing in vm: {}",
                        m.name_of(m.variables.get(v_id).name)
                    );
                };
                Ok(v.into())
            }
        }
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
                        (Value::Bool(b1), Value::Bool(b2)) => {
                            eprintln!("{b1} == {b2}");
                            Ok(b1 == b2)
                        }
                        (Value::Char(c1), Value::Char(c2)) => Ok(c1 == c2),
                        (Value::Int(i1), Value::Int(i2)) => Ok(i1 == i2),
                        (Value::Float(f1), Value::Float(f2)) => Ok(f1 == f2),
                        (lhs, rhs) => {
                            failf!(
                                bin_op.span,
                                "static equality over {} and {} is unimplemented",
                                lhs.kind_str(),
                                rhs.kind_str()
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
            let enum_ptr =
                vm.stack.push_enum(&m.types, e.type_id, e.variant_index as usize, payload_value);
            Ok(Value::Agg { type_id: e.type_id, ptr: enum_ptr }.into())
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
        TypedExpr::EnumGetPayload(get_payload) => {
            let get_payload = *get_payload;
            let Value::Agg { ptr, .. } = execute_expr_return_exit!(vm, m, get_payload.enum_expr)?
            else {
                m.ice_with_span("malformed enum get_payload", get_payload.span)
            };
            let enum_type = m.get_expr_type(get_payload.enum_expr).expect_enum();
            let variant = enum_type.variant_by_index(get_payload.variant_index);
            let payload_ptr = gep_enum_payload(&m.types, variant, ptr);
            let payload_value = load_value(vm, m, variant.payload.unwrap(), payload_ptr, false)?;
            Ok(payload_value.into())
        }
        TypedExpr::Cast(typed_cast) => {
            let typed_cast = typed_cast.clone();
            let span = typed_cast.span;
            let base_value = execute_expr_return_exit!(vm, m, typed_cast.base_expr)?;
            match typed_cast.cast_type {
                CastType::IntegerExtend => {
                    let _ = base_value.expect_int();
                    todo!()
                    //match i {
                    //    TypedIntegerValue::U8(_) => todo!(),
                    //    TypedIntegerValue::U16(_) => todo!(),
                    //    TypedIntegerValue::U32(_) => todo!(),
                    //    TypedIntegerValue::U64(_) => todo!(),
                    //    TypedIntegerValue::I8(_) => todo!(),
                    //    TypedIntegerValue::I16(_) => todo!(),
                    //    TypedIntegerValue::I32(_) => todo!(),
                    //    TypedIntegerValue::I64(_) => todo!(),
                    //}
                }
                CastType::IntegerTruncate => {
                    let int_to_trunc = base_value.expect_int();
                    let value: TypedIntValue = match m.types.get(typed_cast.target_type_id) {
                        Type::Integer(IntegerType::U8) => int_to_trunc.to_u8().into(),
                        Type::Integer(IntegerType::U16) => int_to_trunc.to_u16().into(),
                        Type::Integer(IntegerType::U32) => int_to_trunc.to_u32().into(),
                        Type::Integer(IntegerType::U64) => int_to_trunc.to_u64().into(),
                        Type::Integer(IntegerType::I8) => (int_to_trunc.to_u8() as i8).into(),
                        Type::Integer(IntegerType::I16) => (int_to_trunc.to_u16() as i16).into(),
                        Type::Integer(IntegerType::I32) => (int_to_trunc.to_u32() as i32).into(),
                        Type::Integer(IntegerType::I64) => (int_to_trunc.to_u64() as i64).into(),
                        _ => unreachable!(),
                    };
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
                CastType::IntegerExtendFromChar => todo!(),
                CastType::IntegerToFloat => todo!(),
                CastType::IntegerToPointer => {
                    let u = base_value.expect_int().expect_uword();
                    Ok(Value::Pointer(u as usize).into())
                }
                CastType::KnownNoOp => Ok(base_value.into()),
                CastType::PointerToReference => {
                    let Value::Pointer(value) = base_value else {
                        m.ice_with_span("malformed pointer-to-reference cast", span)
                    };
                    Ok(Value::Reference {
                        type_id: typed_cast.target_type_id,
                        ptr: value as *const u8,
                    }
                    .into())
                }
                CastType::ReferenceToPointer => {
                    let Value::Reference { ptr, .. } = base_value else {
                        m.ice_with_span("malformed reference-to-pointer cast", span)
                    };
                    Ok(Value::Pointer(ptr.addr()).into())
                }
                CastType::PointerToInteger => todo!(),
                CastType::FloatExtend => todo!(),
                CastType::FloatTruncate => todo!(),
                CastType::FloatToInteger => todo!(),
                CastType::LambdaToLambdaObject => todo!(),
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
        TypedExpr::Lambda(_) => todo!(),
        TypedExpr::FunctionReference(_) => todo!(),
        TypedExpr::FunctionToLambdaObject(_) => todo!(),
        TypedExpr::PendingCapture(_) => todo!(),
        TypedExpr::StaticValue(value_id, _, _) => {
            let static_value = m.static_values.get(*value_id);
            let vm_value =
                static_value_to_vm_value(vm, StackSelection::CallStackCurrent, m, static_value)?;
            Ok(vm_value.into())
        }
    };
    //debug!("{}-> {:?}", " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize), result);
    result
}

pub fn static_value_to_vm_value(
    vm: &mut Vm,
    dst_stack: StackSelection,
    m: &TypedProgram,
    static_value: &StaticValue,
) -> TyperResult<Value> {
    match static_value {
        StaticValue::Unit(_) => Ok(Value::Unit),
        StaticValue::Boolean(bv, _) => Ok(Value::Bool(*bv)),
        StaticValue::Char(cb, _) => Ok(Value::Char(*cb)),
        StaticValue::Integer(iv, _) => Ok(Value::Int(*iv)),
        StaticValue::Float(fv, _) => Ok(Value::Float(*fv)),
        StaticValue::String(string_id, _) => {
            let value = k1_str_to_value(vm, dst_stack, m, *string_id);
            Ok(value)
        }
        StaticValue::NullPointer(_) => Ok(Value::Pointer(0)),
        StaticValue::Struct(static_struct) => {
            let mut values: SmallVec<[Value; 8]> = smallvec![];
            for f in static_struct.fields.iter() {
                let static_value = m.static_values.get(*f);
                let value = static_value_to_vm_value(vm, dst_stack, m, static_value)?;
                values.push(value);
            }
            let struct_ptr = vm.get_destination_stack(dst_stack).push_struct_values(
                &m.types,
                static_struct.type_id,
                &values,
            );
            Ok(Value::Agg { type_id: static_struct.type_id, ptr: struct_ptr })
        }
        StaticValue::Enum(e) => {
            let payload_value = match e.payload {
                None => None,
                Some(static_value_id) => {
                    let static_value = m.static_values.get(static_value_id);
                    let value = static_value_to_vm_value(vm, dst_stack, m, static_value)?;
                    Some(value)
                }
            };
            let enum_ptr = vm.get_destination_stack(dst_stack).push_enum(
                &m.types,
                e.type_id,
                e.variant_index as usize,
                payload_value,
            );
            Ok(Value::Agg { type_id: e.type_id, ptr: enum_ptr })
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
            let let_stmt = let_stmt.clone();
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
                debug_assert_eq!(let_stmt.variable_type, v.get_type());
                v
            };
            vm.insert_current_local(let_stmt.variable_id, to_store);
            Ok(VmResult::Value(Value::UNIT))
        }
        typer::TypedStmt::Assignment(assgn) => {
            let assgn = assgn.clone();
            let v = execute_expr_return_exit!(vm, m, assgn.value)?;

            match assgn.kind {
                typer::AssignmentKind::Value => {
                    let TypedExpr::Variable(destination_var) = m.exprs.get(assgn.destination)
                    else {
                        m.ice("Value assignment lhs was not a variable", None)
                    };
                    vm.insert_current_local(destination_var.variable_id, v);
                    Ok(VmResult::Value(Value::UNIT))
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
                    Ok(VmResult::Value(Value::UNIT))
                }
            }
        }
        typer::TypedStmt::Require(_) => {
            todo!("static require not yet implemented")
        }
    }
}

fn execute_call(vm: &mut Vm, m: &mut TypedProgram, call_id: TypedExprId) -> TyperResult<VmResult> {
    let call = m.exprs.get(call_id).expect_call();
    let call_args = call.args.clone();
    let span = call.span;
    let function_id = match call.callee {
        typer::Callee::StaticFunction(function_id) => function_id,
        typer::Callee::StaticLambda { function_id, .. } => {
            function_id
        }
        typer::Callee::StaticAbstract {..} => {
            m.ice_with_span("Abstract call attempt in VM", span)
        }
        typer::Callee::DynamicLambda(_) => {
            return failf!(span, "Function pointers are not supported in static; one day I could JIT the functions and get pointers; oh my")
        }
        typer::Callee::DynamicFunction(_) => {
            return failf!(span, "Function pointers are not supported in static")
        }
        typer::Callee::DynamicAbstract {..} => {
            m.ice_with_span("Abstract call attempt in VM", span)
        }
    };

    let function = m.get_function(function_id);

    if let Some(intrinsic_type) = function.intrinsic_type {
        let type_args = call.type_args.clone();
        return execute_intrinsic(vm, m, &type_args, &call_args, call.return_type, intrinsic_type);
    };

    if function.body_block.is_none() {
        // let m_ptr = m as *const TypedModule;
        // let mut_m = m_ptr.cast_mut();
        // unsafe { (*mut_m).eval_function_body(function_id)? }
        let prev_level = log::max_level();
        log::set_max_level(log::LevelFilter::Info);
        m.eval_function_body(function_id)?;
        log::set_max_level(prev_level);
    }

    let function = m.get_function(function_id);
    let Some(body_block_expr) = function.body_block else {
        return failf!(span, "Cannot execute function {}: no body", m.name_of(function.name));
    };

    if vm.stack.frames.len() == 128 {
        eprintln!("{}", vm.stack.frames.iter().map(|f| m.name_of_opt(f.debug_name)).join("\n"));
        return failf!(span, "VM call stack overflow");
    }

    let return_type_id = m.types.get(function.type_id).expect_function().return_type;
    let aggregate_return_layout = if m.types.is_aggregate_repr(return_type_id) {
        Some(m.types.get_layout(return_type_id).unwrap())
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
    for (variable_id, arg) in param_variables.iter().zip(call_args.iter()) {
        // Execute this in _caller_ frame so the data outlives the callee
        let value = execute_expr_return_exit!(vm, m, *arg)?;

        param_values.push((*variable_id, value));
    }

    // But bind the variables in the _callee_ frame
    vm.stack.push_new_frame(Some(name));
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
                copy_aggregate(dst_ptr, result_ptr, layout.size_bytes());
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
    type_args: &[SimpleNamedType],
    args: &[TypedExprId],
    return_type: TypeId,
    intrinsic_type: IntrinsicFunction,
) -> TyperResult<VmResult> {
    match intrinsic_type {
        IntrinsicFunction::SizeOf => {
            let type_id = type_args[0].type_id;
            let layout = m.types.get_layout(type_id);
            let size_bytes = match layout {
                Some(l) => l.size_bytes(),
                None => 0,
            };
            Ok(Value::Int(TypedIntValue::UWord64(size_bytes as u64)).into())
        }
        IntrinsicFunction::SizeOfStride => {
            let type_id = type_args[0].type_id;
            let layout = m.types.get_layout(type_id);
            let stride_bytes = match layout {
                Some(l) => l.stride_bytes(),
                None => 0,
            };
            Ok(Value::Int(TypedIntValue::UWord64(stride_bytes as u64)).into())
        }
        IntrinsicFunction::AlignOf => {
            let type_id = type_args[0].type_id;
            let layout = m.types.get_layout(type_id);
            let align_bytes = match layout {
                Some(l) => l.align_bytes(),
                None => 0,
            };
            Ok(Value::Int(TypedIntValue::UWord64(align_bytes as u64)).into())
        }
        IntrinsicFunction::TypeId => {
            let type_id = type_args[0].type_id;
            Ok(Value::from(type_id.to_u64()).into())
        }
        IntrinsicFunction::BoolNegate => {
            let b = execute_expr_return_exit!(vm, m, args[0])?.expect_bool();
            Ok(Value::Bool(!b).into())
        }
        IntrinsicFunction::BitNot => todo!("BitNot"),
        IntrinsicFunction::BitAnd => todo!(),
        IntrinsicFunction::BitOr => todo!(),
        IntrinsicFunction::BitXor => todo!(),
        IntrinsicFunction::BitShiftLeft => todo!(),
        IntrinsicFunction::BitShiftRight => todo!(),
        IntrinsicFunction::PointerIndex => {
            // intern fn refAtIndex[T](self: Pointer, index: uword): T*
            let typ = type_args[0].type_id;
            let ptr = execute_expr_return_exit!(vm, m, args[0])?.expect_ptr();
            let index = execute_expr_return_exit!(vm, m, args[1])?.expect_int().expect_uword();
            let result = ptr + offset_at_index(&m.types, typ, index as usize);
            Ok(Value::Reference { type_id: return_type, ptr: result as *const u8 }.into())
        }
        IntrinsicFunction::CompilerSourceLocation => {
            unreachable!("CompilerSourceLocation is handled in typer")
        }
        IntrinsicFunction::Allocate | IntrinsicFunction::AllocateZeroed => {
            let zero = intrinsic_type == IntrinsicFunction::AllocateZeroed;
            let size_expr = args[0];
            let align_expr = args[1];
            let size = execute_expr_return_exit!(vm, m, size_expr)?.expect_int().expect_uword();
            let align = execute_expr_return_exit!(vm, m, align_expr)?.expect_int().expect_uword();
            let layout = std::alloc::Layout::from_size_align(size, align).unwrap();
            let ptr = if zero {
                unsafe { std::alloc::alloc_zeroed(layout) }
            } else {
                unsafe { std::alloc::alloc(layout) }
            };
            Ok(Value::Pointer(ptr.addr()).into())
        }
        IntrinsicFunction::Reallocate => {
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
        IntrinsicFunction::Free => {
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
        IntrinsicFunction::MemCopy => {
            let [dst, src, count] = args[0..3] else { unreachable!() };
            let dst = execute_expr_return_exit!(vm, m, dst)?.expect_ptr();
            let src = execute_expr_return_exit!(vm, m, src)?.expect_ptr();
            let count = execute_expr_return_exit!(vm, m, count)?.expect_int().expect_u64();
            unsafe {
                std::ptr::copy_nonoverlapping(src as *const u8, dst as *mut u8, count as usize)
            };
            Ok(VmResult::UNIT)
        }
        IntrinsicFunction::MemSet => todo!(),
        IntrinsicFunction::MemEquals => {
            //intern fn compare(p1: Pointer, p2: Pointer, size: uword): i32
            let [p1, p2, size] = args[0..3] else { unreachable!() };
            let p1 = execute_expr_return_exit!(vm, m, p1)?.expect_ptr();
            let p2 = execute_expr_return_exit!(vm, m, p2)?.expect_ptr();
            let size = execute_expr_return_exit!(vm, m, size)?.expect_int().expect_uword();
            let p1 = p1 as *const u8;
            let p2 = p2 as *const u8;
            let p1 = unsafe { std::slice::from_raw_parts(p1, size as usize) };
            let p2 = unsafe { std::slice::from_raw_parts(p2, size as usize) };
            // Run rust's memcmp equivalent
            let eq = p1 == p2;
            Ok(Value::Bool(eq).into())
        }
        IntrinsicFunction::Exit => {
            let TypedIntValue::I32(code) = execute_expr_return_exit!(vm, m, args[0])?.expect_int()
            else {
                unreachable!("malformed exit (code type)")
            };
            Ok(VmResult::Exit(code))
        }
        IntrinsicFunction::EmitCompilerMessage => {
            let message_arg = execute_expr_return_exit!(vm, m, args[1])?;
            eprintln!("msg arg is {:?}", message_arg);
            let message = value_to_rust_str(m, message_arg);
            let (source, line) = m.get_span_location(vm.eval_span);
            eprintln!("[MSG {}:{}] {}", source.filename, line.line_number(), m.get_string(message));
            Ok(VmResult::UNIT)
        }
    }
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
                let struct_layout = types.get_layout(type_id).unwrap();
                let size_bytes = struct_layout.size_bytes();
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
    dst.copy_from_nonoverlapping(src, size)
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
    let Some(layout) = m.types.get_layout(type_id) else {
        m.ice_with_span("Cannot dereference a type with no known layout", span)
    };
    debug!("load of '{}' from {:?} {:?}", m.type_id_to_string(type_id), ptr, layout);
    match m.types.get(type_id) {
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
                match float_type.size {
                    NumericWidth::B32 => TypedFloatValue::F32((ptr as *const f32).read()),
                    NumericWidth::B64 => TypedFloatValue::F64((ptr as *const f64).read()),
                    _ => unreachable!(),
                }
            };
            Ok(Value::Float(float_value))
        }
        Type::Bool => {
            debug_assert_eq!(layout.size_bits, 8);
            let byte = unsafe { *ptr };
            Ok(Value::Bool(byte != 0))
        }
        Type::Pointer => {
            debug_assert_eq!(layout.size_bytes(), size_of::<usize>());
            let value = unsafe { *(ptr as *const usize) };
            Ok(Value::Pointer(value))
        }
        Type::Reference(_reference_type) => {
            debug_assert_eq!(layout.size_bytes(), size_of::<usize>());

            // We're loading a reference, which means we are loading a usize
            let value: usize = unsafe { (ptr as *const usize).read() };
            let loaded_ptr = value as *const u8;
            Ok(Value::Reference { type_id, ptr: loaded_ptr })
        }
        Type::Struct(_) | Type::Enum(_) | Type::EnumVariant(_) => {
            if copy_aggregates {
                let layout = m.types.get_layout(type_id).unwrap();
                let frame_ptr = vm.stack.push_raw_copy_layout(layout, ptr);
                Ok(Value::Agg { type_id, ptr: frame_ptr })
            } else {
                Ok(Value::Agg { type_id, ptr })
            }
        }
        Type::Never => unreachable!("Not a value type"),
        Type::Function(_) => unreachable!("Not a value type"),
        Type::Lambda(_) => todo!("just a struct load of the env"),
        Type::LambdaObject(_) => todo!("struct of the lambda obj"),
        Type::Generic(_) => unreachable!("Not a value type"),
        Type::TypeParameter(_) => unreachable!("Not a value type"),
        Type::FunctionTypeParameter(_) => unreachable!("Not a value type"),
        Type::InferenceHole(_) => unreachable!("Not a value type"),
        Type::RecursiveReference(_) => unreachable!("Not a value type"),
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

fn build_enum(
    dst: *mut u8,
    types: &Types,
    enum_type_id: TypeId,
    variant_index: usize,
    payload_value: Option<Value>,
) -> (*const u8, Layout) {
    let enum_type = types.get(enum_type_id).expect_enum();
    let enum_layout = types.get_layout(enum_type_id).unwrap();
    // TODO(vm): Represent no-payload enums as
    // an int-based Value variant, not aggregates
    if enum_type.is_no_payload() {
        // Simple integer value
        eprintln!("TODO: optimize for no payload enums")
    }
    let variant = &enum_type.variants[variant_index];

    let start_dst = aligned_to_mut(dst, enum_layout.align_bytes());

    let _tag_written = store_value(types, start_dst, Value::Int(variant.tag_value));
    if let Some(payload_value) = payload_value {
        let payload_offset_bytes = types.enum_variant_payload_offset(variant);
        let payload_base_aligned = unsafe { start_dst.byte_add(payload_offset_bytes) };
        store_value(types, payload_base_aligned, payload_value);
    }
    (start_dst, enum_layout)
}

fn build_struct_unaligned(dst: *mut u8, types: &Types, struct_type_id: TypeId, members: &[Value]) {
    let struct_fields = &types.get(struct_type_id).expect_struct().fields;

    for (value, field_type) in members.iter().zip(struct_fields.iter()) {
        // Go to offset
        let field_dst = unsafe { dst.byte_add(field_type.offset_bits as usize / 8) };
        store_value(types, field_dst, *value);
    }
}

pub fn offset_at_index(types: &Types, type_id: TypeId, index: usize) -> usize {
    let size_bytes = types.get_layout(type_id).map(|l| l.size_bytes()).unwrap_or(0);
    index * size_bytes
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn gep_struct_field(
    types: &Types,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
) -> (*const u8, &StructTypeField) {
    let struct_type = types.get(struct_type).expect_struct();
    let field = &struct_type.fields[field_index];
    let offset_bytes = field.offset_bits as usize / 8;

    let field_ptr = unsafe { struct_ptr.byte_add(offset_bytes) };
    (field_ptr, field)
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
    let (field_ptr, field) = gep_struct_field(&m.types, struct_type, struct_ptr, field_index);
    let value = load_value(vm, m, field.type_id, field_ptr, copy)?;
    debug!(
        "load_struct_field {} of type {} is {:?}. Full struct: {:?}",
        m.name_of(field.name),
        m.type_id_to_string(struct_type),
        value,
        unsafe {
            std::slice::from_raw_parts(
                struct_ptr,
                m.types.get_layout(struct_type).unwrap().size_bytes(),
            )
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
    let payload_offset_bytes = types.enum_variant_payload_offset(variant_type);

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
    debug_name: Option<Identifier>,
}

impl StackFrame {
    pub fn make(index: u32, base_ptr: *const u8, debug_name: Option<Identifier>) -> StackFrame {
        StackFrame { index, base_ptr, debug_name }
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
        self.allocation.fill(0);
        self.frames.clear();
    }

    fn push_new_frame(&mut self, name: Option<Identifier>) -> StackFrame {
        let index = self.frames.len() as u32;
        let base_ptr = self.cursor;
        let frame = StackFrame::make(index, base_ptr, name);
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
            let dst_slice = std::slice::from_raw_parts_mut(slice_stack_ptr, slice_len);
            // Copy bytes using memcpy:
            debug!("copy_from_slice {:?} {:?}", slice.as_ptr(), slice_stack_ptr);
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
    ) -> *const u8 {
        let struct_layout = types.get_layout(struct_type_id).unwrap();
        let struct_base = self.push_layout_uninit(struct_layout);
        build_struct_unaligned(struct_base, types, struct_type_id, members);
        self.advance_cursor(struct_layout.size_bytes());
        struct_base
    }

    pub fn push_enum(
        &mut self,
        types: &Types,
        enum_type_id: TypeId,
        variant_index: usize,
        payload_value: Option<Value>,
    ) -> *const u8 {
        let dst = self.cursor_mut();
        let (enum_base, layout) =
            build_enum(dst, types, enum_type_id, variant_index, payload_value);
        self.advance_cursor(layout.size_bytes());
        enum_base
    }

    pub fn push_value(&mut self, types: &Types, value: Value) -> *const u8 {
        let layout = types.get_layout(value.get_type()).unwrap();
        self.align_to_bytes(layout.align_bytes());
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
            self.check_bounds(unsafe { self.cursor.byte_add(layout.size_bytes()) })
        }
        self.align_to_bytes(layout.align_bytes());
        let c = self.cursor_mut();
        self.advance_cursor(layout.size_bytes());
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
        self.push_raw_copy(layout.size_bytes(), layout.align_bytes(), src_ptr)
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

pub fn value_to_rust_str(m: &mut TypedProgram, value: Value) -> StringId {
    let ptr = value.expect_agg();
    let k1_string = unsafe { (ptr as *const k1_types::K1Buffer).read() };
    eprintln!("Loading to rust str: {} and {:?}", k1_string.len, k1_string.data);
    let string_slice = unsafe {
        let slice = k1_string.to_slice::<u8>();
        std::str::from_utf8(slice).unwrap()
    };
    m.ast.strings.intern(string_slice)
}

pub fn k1_str_to_value(
    vm: &mut Vm,
    dst_stack: StackSelection,
    m: &TypedProgram,
    string_id: StringId,
) -> Value {
    let char_buffer_type_id = m.types.get(STRING_TYPE_ID).expect_struct().fields[0].type_id;
    let string_layout = m.types.get_layout(STRING_TYPE_ID).unwrap();
    debug_assert_eq!(string_layout, m.types.get_layout(char_buffer_type_id).unwrap());

    let s = m.get_string(string_id);
    let k1_string = k1_types::K1Buffer { len: s.len(), data: s.as_ptr() };
    debug_assert_eq!(size_of_val(&k1_string), string_layout.size_bytes());

    let string_stack_addr = vm.get_destination_stack(dst_stack).push_t(k1_string);
    Value::Agg { type_id: STRING_TYPE_ID, ptr: string_stack_addr }
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
            //eprintln!(
            //    "render debug of reference and inner type {}",
            //    m.type_id_to_string(type_to_load)
            //);
            //write!(w, "{}", ptr.addr()).unwrap();
            match load_value(vm, m, type_to_load, ptr, true) {
                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                Ok(loaded) => render_debug_value(w, vm, m, loaded),
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
                        for (i, f) in struct_type.fields.iter().enumerate() {
                            write!(w, "{}: ", m.name_of(f.name)).unwrap();
                            match load_struct_field(vm, m, type_id, ptr, f.index as usize, true) {
                                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                                Ok(loaded) => render_debug_value(w, vm, m, loaded),
                            };
                            if i != struct_type.fields.len() - 1 {
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
                            Some(variant) => write!(w, "{}", m.name_of(variant.name)).unwrap(),
                        }
                    }
                },
                _ => write!(w, "?").unwrap(),
            };
        }
    };
}

//mod c_mem {
//    use std::os::raw::{c_size_t, c_void};
//    extern "C" {
//        fn malloc(size: c_size_t) -> *mut c_void;
//        fn realloc(ptr: *mut c_void, size: c_size_t) -> *mut c_void;
//        fn free(ptr: *mut c_void);
//    }
//}
