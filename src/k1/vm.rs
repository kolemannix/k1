use std::{
    collections::VecDeque,
    num::NonZeroU32,
    sync::atomic::{AtomicU64, Ordering},
};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use log::debug;
use smallvec::{smallvec, SmallVec};

use crate::{
    errf, failf,
    lex::SpanId,
    nz_u32_id,
    parse::NumericWidth,
    typer::{
        self, make_error, make_fail_span,
        types::{
            IntegerType, StructTypeField, Type, TypeId, TypedEnumVariant, Types, BOOL_TYPE_ID,
            CHAR_TYPE_ID, POINTER_TYPE_ID, STRING_TYPE_ID, UNIT_TYPE_ID,
        },
        BinaryOpKind, Call, CastType, IntrinsicFunction, Layout, MatchingCondition,
        MatchingConditionInstr, StaticValue, TypedExpr, TypedExprId, TypedFloatValue,
        TypedGlobalId, TypedIntValue, TypedMatchExpr, TypedModule, TypedStmtId, TyperResult,
        VariableId,
    },
};

#[cfg(test)]
mod vm_test;

mod binop;

pub struct Vm {
    /// Code may update globals; so we store this VM run's 'copy' of that global's
    /// value here; these are (currently) reset on each invocation of #static code
    ///
    /// One example from k1 is code that sets the global, thread-local, default allocator
    ///
    /// If we need to preserve them for the entire compilation, we may need to re-use
    /// the same VM, currently we create one per static execution
    globals: FxHashMap<TypedGlobalId, Value>,
    static_space: StackFrame,
    call_stack: VecDeque<StackFrame>,
    eval_depth: AtomicU64,
    eval_span: SpanId,
}

impl Vm {
    pub fn make() -> Self {
        // TODO(vm): Re-use this memory, we really want zero-allocation VM invocations for very
        //           simple expressions
        Self {
            globals: FxHashMap::new(),
            // TODO(vm): Re-use this memory, we really want zero-allocation
            // VM invocations for simple expressions
            static_space: StackFrame::make("static".to_string()),
            call_stack: VecDeque::with_capacity(8),
            eval_depth: AtomicU64::new(0),
            eval_span: SpanId::NONE,
        }
    }

    fn push_new_frame(&mut self, name: String) -> &mut StackFrame {
        let frame = StackFrame::make(name);
        self.push_frame(frame)
    }

    fn push_frame(&mut self, frame: StackFrame) -> &mut StackFrame {
        debug!("Pushing frame {} [depth={}]", frame.debug_name, self.call_stack.len() + 1);
        self.call_stack.push_back(frame);
        self.call_stack.back_mut().unwrap()
    }

    fn pop_frame(&mut self) -> StackFrame {
        let f = self.call_stack.pop_back().unwrap();
        debug!("Popped frame {} [depth={}]", f.debug_name, self.call_stack.len());
        f
    }

    fn current_frame(&self) -> &StackFrame {
        self.call_stack.back().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        let f = self.call_stack.back_mut().unwrap();
        f
    }

    pub fn dump(&mut self, m: &TypedModule) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for index in 0..self.call_stack.len() {
            let frame_string = self.dump_frame(m, index);
            write!(w, "{}", frame_string).unwrap();
        }
        s
    }

    pub fn dump_current_frame(&mut self, m: &TypedModule) -> String {
        self.dump_frame(m, self.call_stack.len() - 1)
    }

    pub fn dump_frame(&mut self, m: &TypedModule, frame_index: usize) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        let frame = &self.call_stack[frame_index];
        writeln!(w, "Frame:  {}", frame.debug_name).unwrap();
        writeln!(
            w,
            "Base :  {:?} - {:?} ({})",
            frame.buffer.as_ptr(),
            frame.buffer.as_ptr_range().end,
            frame.current_offset_bytes()
        )
        .unwrap();
        writeln!(w, "Locals").unwrap();
        let locals = frame.locals.clone();
        let _debug_frame = self.push_new_frame("DEBUG_DUMP".to_string());
        for (k, v) in &locals {
            let var = m.variables.get(*k);
            let v_name = var.name;
            let hidden = var.user_hidden;
            if hidden {
                continue;
            }
            write!(w, "  {}: {} = ", m.name_of(v_name), m.type_id_to_string(v.get_type())).unwrap();
            eprintln!("RENDERING LOCAL {}", m.name_of(v_name));
            render_debug_value(w, self, m, *v);
            writeln!(w).unwrap()
        }
        self.pop_frame();

        write!(w, "\nDATA\n").unwrap();
        let frame = &self.call_stack[frame_index];
        frame.to_bytes().chunks(8).for_each(|bytes| {
            for b in bytes {
                write!(w, "{:02x} ", b).unwrap();
            }
            writeln!(w).unwrap();
        });
        s
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VmResult {
    Value(Value),
    Break(Value),
    Exit(i32),
}

impl VmResult {
    pub const UNIT: VmResult = VmResult::Value(Value::Unit);
    fn expect_value(&self, span: SpanId) -> TyperResult<Value> {
        match self {
            VmResult::Value(v) => Ok(*v),
            VmResult::Break(_) => Err(errf!(span, "Expected Value but got Break")),
            VmResult::Exit(code) => Err(errf!(span, "Static execution exited with code: {code}")),
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

    pub fn ptr_if_not_immediate(&self) -> Option<*const u8> {
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

pub fn execute_single_expr(m: &TypedModule, expr: TypedExprId) -> TyperResult<(Vm, Value)> {
    let mut vm = Vm::make();

    // Tell the code we're about to execute that this is static
    // Useful for conditional compilation to branch and do what makes sense
    // in an interpreted context
    if cfg!(debug_assertions) {
        let k1_static_global = m.globals.get(GLOBAL_ID_STATIC);
        debug_assert_eq!(
            m.name_of(m.variables.get(k1_static_global.variable_id).name),
            "IS_STATIC"
        );
    }
    vm.globals.insert(GLOBAL_ID_STATIC, Value::Bool(true));

    vm.push_new_frame(format!("single_expr_{}", expr));
    let span = m.exprs.get(expr).get_span();
    let v = execute_expr(&mut vm, m, expr)?.expect_value(span)?;
    Ok((vm, v))
}

#[inline]
fn execute_expr_ev(vm: &mut Vm, m: &TypedModule, expr: TypedExprId) -> TyperResult<Value> {
    let span = m.exprs.get(expr).get_span();
    execute_expr(vm, m, expr)?.expect_value(span)
}

fn execute_expr(vm: &mut Vm, m: &TypedModule, expr: TypedExprId) -> TyperResult<VmResult> {
    log::set_max_level(log::LevelFilter::Debug);
    vm.eval_depth.fetch_add(1, Ordering::Relaxed);
    let prev = vm.eval_span;
    vm.eval_span = m.exprs.get(expr).get_span();
    let mut vm = scopeguard::guard(vm, |vm| {
        log::set_max_level(log::LevelFilter::Info);
        vm.eval_depth.fetch_sub(1, Ordering::Relaxed);
        vm.eval_span = prev;
    });
    let vm = &mut vm;

    debug!(
        "{}{}",
        " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize),
        m.expr_to_string_with_type(expr)
    );
    let result: TyperResult<VmResult> = match m.exprs.get(expr) {
        TypedExpr::Unit(_) => Ok(Value::Unit.into()),
        TypedExpr::Char(byte, _) => Ok(Value::Char(*byte).into()),
        TypedExpr::Bool(b, _) => Ok(Value::Bool(*b).into()),
        TypedExpr::Integer(typed_integer_expr) => Ok(Value::Int(typed_integer_expr.value).into()),
        TypedExpr::Float(typed_float_expr) => Ok(Value::Float(typed_float_expr.value).into()),
        TypedExpr::String(s, _) => {
            // This needs to result in a string struct
            let string_struct = m.types.get(STRING_TYPE_ID).expect_struct();
            let char_buffer_type_id = string_struct.fields[0].type_id;

            let len: Value = Value::Int(TypedIntValue::U64(s.len() as u64));

            // Just point right at the TypedExpr's string; this is OK
            // since we should never mutate TypedExprs
            let base_ptr: *const u8 = s.as_ptr();
            let char_data: Value = Value::Reference { type_id: CHAR_TYPE_ID, ptr: base_ptr };

            let frame = vm.current_frame_mut();
            let char_buffer_addr =
                frame.push_struct_values(&m.types, char_buffer_type_id, &[len, char_data]);
            // String is just a wrapper around char_buffer
            let string_addr = char_buffer_addr;
            debug_assert_eq!(
                m.types.get_layout(STRING_TYPE_ID),
                m.types.get_layout(char_buffer_type_id)
            );

            let string_struct = Value::Agg { type_id: STRING_TYPE_ID, ptr: string_addr };
            Ok(string_struct.into())
        }
        TypedExpr::Struct(s) => {
            let mut values: SmallVec<[Value; 8]> = SmallVec::with_capacity(s.fields.len());
            let fields = s.fields.clone();
            let s_type_id = s.type_id;
            for field in &fields {
                let value = execute_expr_ev(vm, m, field.expr)?;
                values.push(value);
            }

            let frame = vm.current_frame_mut();
            let struct_base = frame.push_struct_values(&m.types, s_type_id, &values);
            Ok(Value::Agg { type_id: s_type_id, ptr: struct_base }.into())
        }
        TypedExpr::StructFieldAccess(field_access) => {
            let struct_value = execute_expr_ev(vm, m, field_access.base)?;
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
                        let vm_value = static_value_to_vm_value(
                            &mut vm.static_space,
                            m,
                            m.static_values.get(global.initial_value),
                        )?;
                        let final_value = if global.is_referencing {
                            let ptr = vm.static_space.push_ptr_uninit();
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
                let frame = vm.current_frame();
                let Some(v) = frame.locals.get(&v_id) else {
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
                    let reference_value = execute_expr_ev(vm, m, ref_expr)?;
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
            let span = bin_op.span;
            use BinaryOpKind as K;
            match bin_op.kind {
                K::Equals => {
                    let bin_op = bin_op.clone();
                    let lhs = execute_expr_ev(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_ev(vm, m, bin_op.rhs)?;
                    let value = match (lhs, rhs) {
                        (Value::Unit, Value::Unit) => Ok(Value::TRUE),
                        (Value::Bool(b1), Value::Bool(b2)) => {
                            eprintln!("{b1} == {b2}");
                            Ok(Value::Bool(b1 == b2))
                        }
                        (Value::Char(c1), Value::Char(c2)) => Ok(Value::Bool(c1 == c2)),
                        (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 == i2)),
                        (Value::Float(f1), Value::Float(f2)) => Ok(Value::Bool(f1 == f2)),
                        (lhs, rhs) => {
                            failf!(
                                span,
                                "static equality over {} and {} is unimplemented",
                                lhs.kind_str(),
                                rhs.kind_str()
                            )
                        }
                    }?;
                    Ok(value.into())
                }
                K::NotEquals => {
                    unreachable!("Do we even product NotEquals exprs? Or desugar")
                }
                K::Add | K::Subtract | K::Multiply | K::Divide | K::Rem => {
                    let lhs = execute_expr_ev(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_ev(vm, m, bin_op.rhs)?;
                    Ok(binop::execute_arith_op(lhs, rhs, bin_op.kind).into())
                }
                K::Less | K::LessEqual | K::Greater | K::GreaterEqual => {
                    let lhs = execute_expr_ev(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr_ev(vm, m, bin_op.rhs)?;
                    Ok(binop::execute_cmp_op(lhs, rhs, bin_op.kind).into())
                }
                K::And => {
                    // The language semantics guarantee short-circuiting of And
                    let lhs = execute_expr_ev(vm, m, bin_op.lhs)?.expect_bool();

                    if lhs {
                        let rhs = execute_expr_ev(vm, m, bin_op.rhs)?.expect_bool();
                        Ok(Value::Bool(rhs).into())
                    } else {
                        Ok(Value::Bool(false).into())
                    }
                }
                K::Or => {
                    // The language semantics guarantee short-circuiting of Or
                    let lhs = execute_expr_ev(vm, m, bin_op.lhs)?.expect_bool();
                    if lhs {
                        Ok(Value::Bool(true).into())
                    } else {
                        let rhs = execute_expr_ev(vm, m, bin_op.rhs)?.expect_bool();
                        Ok(Value::Bool(rhs).into())
                    }
                }
                _ => {
                    failf!(bin_op.span, "Unsupported static binary op: {}", m.expr_to_string(expr))
                }
            }
        }
        TypedExpr::Block(_) => execute_block(vm, m, expr),
        TypedExpr::Call(call) => execute_call(vm, m, call),
        TypedExpr::Match(match_expr) => execute_match(vm, m, match_expr),
        TypedExpr::WhileLoop(_) => todo!(),
        TypedExpr::LoopExpr(loop_expr) => {
            let mut s = String::new();
            let result = loop {
                eprintln!("loop iter {}", vm.dump_current_frame(m));
                let l = std::io::stdin().read_line(&mut s).unwrap();
                match execute_block(vm, m, loop_expr.body_block)? {
                    VmResult::Value(value) => continue,
                    res @ VmResult::Break(value) => break res,
                    res @ VmResult::Exit(_) => break res,
                }
            };
            Ok(result)
        }
        TypedExpr::EnumConstructor(e) => {
            let payload_value = match e.payload {
                None => None,
                Some(payload_expr) => Some(execute_expr_ev(vm, m, payload_expr)?),
            };
            let frame = vm.current_frame_mut();
            let enum_ptr =
                frame.push_enum(&m.types, e.type_id, e.variant_index as usize, payload_value);
            Ok(Value::Agg { type_id: e.type_id, ptr: enum_ptr }.into())
        }
        // EnumIsVariant could actually go away in favor of just an == on the getTag + a type-level
        // get tag expr on rhs
        TypedExpr::EnumIsVariant(is_variant) => {
            let Value::Agg { ptr, .. } = execute_expr_ev(vm, m, is_variant.enum_expr)? else {
                m.ice_with_span("malformed enum get_tag", is_variant.span)
            };
            // Do not load the entire enum to the stack, simply
            // interpret the base ptr as a ptr to the tag type
            let enum_type = m.get_expr_type(is_variant.enum_expr).expect_enum();
            let Value::Int(tag_value) = load_value(vm, m, enum_type.tag_type, ptr, true)? else {
                unreachable!()
            };
            let variant = &enum_type.variants[is_variant.variant_index as usize];
            Ok(Value::Bool(tag_value == variant.tag_value).into())
        }
        TypedExpr::EnumGetTag(get_tag) => {
            let Value::Agg { ptr, .. } = execute_expr_ev(vm, m, get_tag.enum_expr)? else {
                m.ice_with_span("malformed enum get_tag", get_tag.span)
            };
            // Do not load the entire enum to the stack, simply
            // interpret the base ptr as a ptr to the tag type
            let enum_type = m.get_expr_type(get_tag.enum_expr).expect_enum();
            let tag_value = load_value(vm, m, enum_type.tag_type, ptr, true)?;
            Ok(tag_value.into())
        }
        TypedExpr::EnumGetPayload(get_payload) => {
            let Value::Agg { ptr, .. } = execute_expr_ev(vm, m, get_payload.enum_expr)? else {
                m.ice_with_span("malformed enum get_payload", get_payload.span)
            };
            // Do not load the entire enum to the stack, simply
            // interpret the base ptr as a ptr to the tag type
            let enum_type = m.get_expr_type(get_payload.enum_expr).expect_enum();
            let variant = enum_type.variant_by_index(get_payload.variant_index);
            let payload_ptr = gep_enum_payload(&m.types, variant, ptr);
            let payload_value = load_value(vm, m, variant.payload.unwrap(), payload_ptr, false)?;
            Ok(payload_value.into())
        }
        TypedExpr::Cast(typed_cast) => {
            let typed_cast = typed_cast.clone();
            let span = typed_cast.span;
            let base_value = execute_expr_ev(vm, m, typed_cast.base_expr)?;
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
                CastType::IntegerTruncate => todo!(),
                CastType::Integer8ToChar => todo!(),
                CastType::IntegerExtendFromChar => todo!(),
                CastType::IntegerToFloat => todo!(),
                CastType::IntegerToPointer => {
                    // TODO: If host platform is 32-bit, then we should require a u32 here
                    let u = base_value.expect_int().expect_u64();
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
            let value = execute_expr(vm, m, typed_return.value)?;
            // Unlike in the LLVM backend, we return the value of the return
            // expr here; we have the luxury of not worrying about 'instructions'
            // vs 'values'; execute_block here in the VM just always uses the value
            // of the last expr
            Ok(value)
        }
        TypedExpr::Break(b) => {
            let unit = execute_expr(vm, b.value)?;
        }
        TypedExpr::Lambda(_) => todo!(),
        TypedExpr::FunctionReference(_) => todo!(),
        TypedExpr::FunctionToLambdaObject(_) => todo!(),
        TypedExpr::PendingCapture(_) => todo!(),
        TypedExpr::StaticValue(_, _, _) => {
            unreachable!("Already evaluated? Or do I need to support this if we reference a global in a static fn?")
        }
    };
    //debug!("{}-> {:?}", " ".repeat(vm.eval_depth.load(Ordering::Relaxed) as usize), result);
    result
}

pub fn static_value_to_vm_value(
    dst_frame: &mut StackFrame,
    m: &TypedModule,
    static_value: &StaticValue,
) -> TyperResult<Value> {
    match static_value {
        StaticValue::Unit(_) => Ok(Value::Unit),
        StaticValue::Boolean(bv, _) => Ok(Value::Bool(*bv)),
        StaticValue::Char(cb, _) => Ok(Value::Char(*cb)),
        StaticValue::Integer(iv, _) => Ok(Value::Int(*iv)),
        StaticValue::Float(fv, _) => Ok(Value::Float(*fv)),
        StaticValue::String(_box_str, _) => {
            failf!(static_value.get_span(), "static str to vm value")
        }
        StaticValue::Pointer(value, _) => Ok(Value::Pointer(*value as usize)),
        StaticValue::Struct(static_struct) => {
            let mut values: SmallVec<[Value; 8]> = smallvec![];
            for f in static_struct.fields.iter() {
                let static_value = m.static_values.get(*f);
                let value = static_value_to_vm_value(dst_frame, m, static_value)?;
                values.push(value);
            }
            let struct_ptr = dst_frame.push_struct_values(&m.types, static_struct.type_id, &values);
            Ok(Value::Agg { type_id: static_struct.type_id, ptr: struct_ptr })
        }
        StaticValue::Enum(e) => {
            let payload_value = match e.payload {
                None => None,
                Some(static_value_id) => {
                    let static_value = m.static_values.get(static_value_id);
                    let value = static_value_to_vm_value(dst_frame, m, static_value)?;
                    Some(value)
                }
            };
            let enum_ptr =
                dst_frame.push_enum(&m.types, e.type_id, e.variant_index as usize, payload_value);
            Ok(Value::Agg { type_id: e.type_id, ptr: enum_ptr })
        }
    }
}

pub fn execute_block(
    vm: &mut Vm,
    m: &TypedModule,
    block_expr: TypedExprId,
) -> TyperResult<VmResult> {
    let mut last_stmt_result = VmResult::UNIT;
    let TypedExpr::Block(typed_block) = m.exprs.get(block_expr) else { unreachable!() };
    // TODO(vm): clone of block statements vec
    let stmts = typed_block.statements.clone();
    for stmt in stmts.iter() {
        last_stmt_result = execute_stmt(vm, m, *stmt)?;
    }
    Ok(last_stmt_result)
}

pub fn execute_stmt(vm: &mut Vm, m: &TypedModule, stmt_id: TypedStmtId) -> TyperResult<VmResult> {
    // TODO: Handle divergence / never type. We should exit the VM gracefully and then just
    // report a normal compilation error from `typer`!
    //
    // Do we do a 'never' _Value_? It's a type, not a value. But LLVM does it as a value
    match m.stmts.get(stmt_id) {
        typer::TypedStmt::Expr(typed_expr_id, _) => {
            let result = execute_expr(vm, m, *typed_expr_id)?;
            Ok(result)
        }
        typer::TypedStmt::Let(let_stmt) => {
            let let_stmt = let_stmt.clone();
            let v = execute_expr_ev(vm, m, let_stmt.initializer)?;
            let to_store = if let_stmt.is_referencing {
                let reference_type = let_stmt.variable_type;
                let value_type = v.get_type();
                debug_assert_eq!(
                    m.types.get(reference_type).expect_reference().inner_type,
                    value_type
                );
                let stack_ptr = vm.current_frame_mut().push_ptr_uninit();
                store_value(&m.types, stack_ptr, v);
                Value::Reference { type_id: reference_type, ptr: stack_ptr }
            } else {
                debug_assert_eq!(let_stmt.variable_type, v.get_type());
                v
            };
            vm.current_frame_mut().locals.insert(let_stmt.variable_id, to_store);
            Ok(VmResult::Value(Value::UNIT))
        }
        typer::TypedStmt::Assignment(assgn) => {
            let assgn = assgn.clone();
            let v = execute_expr_ev(vm, m, assgn.value)?;

            match assgn.kind {
                typer::AssignmentKind::Value => {
                    let TypedExpr::Variable(destination_var) = m.exprs.get(assgn.destination)
                    else {
                        m.ice("Value assignment lhs was not a variable", None)
                    };
                    vm.current_frame_mut().locals.insert(destination_var.variable_id, v);
                    Ok(VmResult::Value(Value::UNIT))
                }
                typer::AssignmentKind::Reference => {
                    let lhs_value = execute_expr_ev(vm, m, assgn.destination)?;
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

fn execute_call(vm: &mut Vm, m: &TypedModule, call: &Call) -> TyperResult<VmResult> {
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
        return match intrinsic_type {
            IntrinsicFunction::SizeOf => {
                let type_id = call.type_args[0].type_id;
                let layout = m.types.get_layout(type_id);
                let size_bytes = match layout {
                    Some(l) => l.size_bytes(),
                    None => 0,
                };
                // TODO: Should be a usize; but K1 doesn't have one
                Ok(Value::from(size_bytes as u64).into())
            }
            IntrinsicFunction::SizeOfStride => {
                let type_id = call.type_args[0].type_id;
                let layout = m.types.get_layout(type_id);
                let stride_bytes = match layout {
                    Some(l) => l.stride_bytes(),
                    None => 0,
                };
                // TODO: Should be a usize; but K1 doesn't have one
                Ok(Value::from(stride_bytes as u64).into())
            }
            IntrinsicFunction::AlignOf => {
                let type_id = call.type_args[0].type_id;
                let layout = m.types.get_layout(type_id);
                let align_bytes = match layout {
                    Some(l) => l.align_bytes(),
                    None => 0,
                };
                // TODO: Should be a usize; but K1 doesn't have one
                Ok(Value::from(align_bytes as u64).into())
            }
            IntrinsicFunction::TypeId => {
                let type_id = call.type_args[0].type_id;
                Ok(Value::from(type_id.to_u64()).into())
            }
            IntrinsicFunction::BoolNegate => {
                let b = execute_expr_ev(vm, m, call.args[0])?.expect_bool();
                return Ok(Value::Bool(!b).into());
            }
            IntrinsicFunction::BitNot => todo!("BitNot"),
            IntrinsicFunction::BitAnd => todo!(),
            IntrinsicFunction::BitOr => todo!(),
            IntrinsicFunction::BitXor => todo!(),
            IntrinsicFunction::BitShiftLeft => todo!(),
            IntrinsicFunction::BitShiftRight => todo!(),
            IntrinsicFunction::PointerIndex => {
                // intern fn refAtIndex[T](self: Pointer, index: u64): T*
                let typ = call.type_args[0].type_id;
                let layout = m.types.get_layout(typ).unwrap();
                let ptr = execute_expr_ev(vm, m, call.args[0])?.expect_ptr();
                let index = execute_expr_ev(vm, m, call.args[1])?.expect_int().expect_u64();
                let result = ptr + (index as usize * layout.size_bytes());
                eprintln!("GEP: {} {} {} {}", ptr, index, layout.size_bytes(), result);
                Ok(Value::Reference { type_id: call.return_type, ptr: result as *const u8 }.into())
            }
            IntrinsicFunction::CompilerSourceLocation => todo!("CompilerSourceLocation"),
        };
    };

    if let Some(result) = vm_intercept_call(vm, m, call)? {
        return Ok(result);
    };
    let Some(body_block_expr) = function.body_block else {
        return failf!(span, "Cannot execute function {}: no body", m.name_of(function.name));
    };

    let mut callee_frame = StackFrame::make(m.name_of(function.name).to_string());

    // Arguments!
    for (param, arg) in function.param_variables.iter().zip(call.args.iter()) {
        // Execute this in _caller_ frame so the data outlives the callee
        let value = execute_expr_ev(vm, m, *arg)?;

        // But bind the variables in the _callee_ frame
        callee_frame.locals.insert(*param, value);
    }

    // Push the callee frame for body execution
    //eprintln!("******************** NEW FRAME {} **********************", m.name_of(function.name));
    //eprintln!("{}", vm.dump_current_frame(m));
    vm.push_frame(callee_frame);
    //eprintln!("{}", vm.dump_current_frame(m));

    let result_value = execute_expr_ev(vm, m, body_block_expr)?;
    // result_value lives in the stack frame we're about to nuke
    // It just to be copied onto the current stack frame...
    // TODO: Handle 'never'

    // If immediate, just 'Copy' the rust value; otherwise memcpy the data to the new stack
    let callee_frame = vm.pop_frame();
    let updated_value = match result_value.ptr_if_not_immediate() {
        None => result_value,
        Some(_ptr) => {
            let caller_frame = vm.current_frame_mut();
            let stored_result = caller_frame.push_value(&m.types, result_value);
            let updated_value = match result_value {
                Value::Agg { type_id, .. } => Value::Agg { type_id, ptr: stored_result },
                _ => unreachable!(),
            };
            updated_value
        }
    };
    drop(callee_frame);
    Ok(updated_value.into())
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn store_value(types: &Types, dst: *mut u8, value: Value) -> usize {
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
                TypedIntValue::U32(v) => {
                    (dst as *mut u32).write(v);
                    size_of::<u32>()
                }
                TypedIntValue::I32(v) => {
                    (dst as *mut i32).write(v);
                    size_of::<i32>()
                }
                TypedIntValue::U64(v) => {
                    (dst as *mut u64).write(v);
                    size_of::<u64>()
                }
                TypedIntValue::I64(v) => {
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
                let count = struct_layout.size_bytes();
                // Equivalent of memcpy
                dst.copy_from_nonoverlapping(ptr.cast(), count);
                count
            }
        }
    }
}

pub fn load_value_to_stack(
    vm: &mut Vm,
    m: &TypedModule,
    type_id: TypeId,
    ptr: *const u8,
) -> TyperResult<Value> {
    load_value(vm, m, type_id, ptr, true)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_value(
    vm: &mut Vm,
    m: &TypedModule,
    type_id: TypeId,
    ptr: *const u8,
    copy: bool,
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
    eprintln!("load of '{}' from {:?} {:?}", m.type_id_to_string(type_id), ptr, layout);
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
        Type::Reference(reference_type) => {
            debug_assert_eq!(layout.size_bytes(), size_of::<usize>());

            // We're loading a reference, which means we are loading a usize
            let value: usize = unsafe { (ptr as *const usize).read() };
            let loaded_ptr = value as *const u8;
            Ok(Value::Reference { type_id, ptr: loaded_ptr })
        }
        Type::Struct(_struct_type) => {
            if copy {
                let layout = m.types.get_layout(type_id).unwrap();
                let frame = vm.current_frame_mut();
                let frame_ptr = frame.push_raw_copy_layout(layout, ptr);
                Ok(Value::Agg { type_id, ptr: frame_ptr })
            } else {
                Ok(Value::Agg { type_id, ptr })
            }
        }
        Type::Enum(_) => {
            let layout = m.types.get_layout(type_id).unwrap();
            let frame = vm.current_frame_mut();
            let frame_ptr = frame.push_raw_copy_layout(layout, ptr);
            Ok(Value::Agg { type_id, ptr: frame_ptr })
        }
        Type::EnumVariant(ev) => {
            let layout = m.types.get_layout(ev.enum_type_id).unwrap();
            let frame = vm.current_frame_mut();
            let frame_ptr = frame.push_raw_copy_layout(layout, ptr);
            Ok(Value::Agg { type_id, ptr: frame_ptr })
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

fn build_struct(
    dst: *mut u8,
    types: &Types,
    struct_type_id: TypeId,
    members: &[Value],
) -> (*const u8, Layout) {
    let struct_fields = &types.get(struct_type_id).expect_struct().fields;
    let struct_layout = types.get_layout(struct_type_id).unwrap();

    let start_dst = aligned_to_mut(dst, struct_layout.align_bytes());

    for (value, field_type) in members.iter().zip(struct_fields.iter()) {
        // Go to offset
        let field_dst = unsafe { start_dst.byte_add(field_type.offset_bits as usize / 8) };
        store_value(types, field_dst, *value);
    }
    (start_dst.cast_const(), struct_layout)
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
    m: &TypedModule,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
    copy: bool,
) -> TyperResult<Value> {
    let (field_ptr, field) = gep_struct_field(&m.types, struct_type, struct_ptr, field_index);
    let value = load_value(vm, m, field.type_id, field_ptr, copy)?;
    eprintln!(
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

pub struct StackFrame {
    buffer: Box<[u8; 8192]>,
    pub cursor: *const u8,
    pub locals: FxHashMap<VariableId, Value>,
    debug_name: String,
}

impl Drop for StackFrame {
    fn drop(&mut self) {
        eprintln!("DROP StackFrame {} Base {:?}", self.debug_name, self.buffer.as_ptr())
    }
}

impl StackFrame {
    pub fn make(name: String) -> Self {
        let buffer = Box::new([0; 8192]);
        let cursor = buffer.as_ptr();
        Self { buffer, cursor, locals: FxHashMap::new(), debug_name: name }
    }

    pub fn base_ptr(&self) -> *const u8 {
        self.buffer.as_ptr()
    }

    pub fn current_offset_bytes(&self) -> usize {
        self.cursor.addr() - self.base_ptr().addr()
    }

    fn cursor_mut(&self) -> *mut u8 {
        self.cursor as *mut u8
    }

    pub fn push_slice(&mut self, slice: &[u8]) -> *const u8 {
        let slice_stack_ptr = self.cursor_mut();
        let slice_len = slice.len();

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
        let (struct_base, struct_layout) =
            build_struct(self.cursor_mut(), types, struct_type_id, members);
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
    pub fn push_value_no_align(&mut self, types: &Types, value: Value) -> *const u8 {
        let dst: *mut u8 = self.cursor_mut();

        let written: usize = store_value(types, dst, value);
        self.advance_cursor(written);
        dst.cast_const()
    }

    pub fn advance_cursor(&mut self, count: usize) {
        self.cursor = unsafe { self.cursor.byte_add(count) };
    }

    pub fn push_n<T: Copy>(&mut self, value: T) -> *const u8 {
        let c = self.cursor;
        unsafe {
            (self.cursor_mut() as *mut T).write(value);
            self.advance_cursor(size_of::<T>());
            c
        }
    }

    pub fn push_usize(&mut self, value: usize) -> *const u8 {
        self.push_n(value)
    }

    pub fn push_ptr_uninit(&mut self) -> *mut u8 {
        self.push_usize(0).cast_mut()
    }

    /// Push a raw copy of `size_bytes` from `src_ptr` into the frame buffer.
    /// The copy is aligned to `align_bytes`.
    fn push_raw_copy(
        &mut self,
        size_bytes: usize,
        align_bytes: usize,
        src_ptr: *const u8,
    ) -> *const u8 {
        self.align_to_bytes(align_bytes);
        unsafe {
            debug!("from raw parts: {:?} {}", src_ptr, size_bytes);
            let src_slice = std::slice::from_raw_parts(src_ptr, size_bytes);
            self.push_slice(src_slice)
        }
    }

    fn push_raw_copy_layout(&mut self, layout: Layout, src_ptr: *const u8) -> *const u8 {
        self.push_raw_copy(layout.size_bytes(), layout.align_bytes(), src_ptr)
    }

    pub fn to_bytes(&self) -> &[u8] {
        &self.buffer[0..self.current_offset_bytes()]
    }
}

fn execute_match(
    vm: &mut Vm,
    m: &TypedModule,
    match_expr: &TypedMatchExpr,
) -> TyperResult<VmResult> {
    for stmt in &match_expr.initial_let_statements {
        execute_stmt(vm, m, *stmt)?;
    }
    for arm in &match_expr.arms {
        let Value::Bool(b) = execute_matching_condition(vm, m, &arm.condition)? else {
            unreachable!()
        };
        if b {
            return execute_expr(vm, m, arm.consequent_expr);
        } else {
            continue;
        }
    }
    failf!(match_expr.span, "Malformed match: No match arms executed")
}

fn execute_matching_condition(
    vm: &mut Vm,
    m: &TypedModule,
    cond: &MatchingCondition,
) -> TyperResult<Value> {
    for instr in cond.instrs.iter() {
        match instr {
            MatchingConditionInstr::Binding { let_stmt, .. } => {
                execute_stmt(vm, m, *let_stmt)?;
            }
            MatchingConditionInstr::Cond { value } => {
                let Value::Bool(cond_bool) = execute_expr_ev(vm, m, *value)? else {
                    unreachable!()
                };
                if !cond_bool {
                    return Ok(Value::Bool(false));
                }
            }
        }
    }
    Ok(Value::Bool(true))
}

fn vm_intercept_call(vm: &mut Vm, m: &TypedModule, call: &Call) -> TyperResult<Option<VmResult>> {
    let Some(function_id) = call.callee.maybe_function_id() else { return Ok(None) };
    let function = m.get_function(function_id);
    let ns = m.scopes.nearest_parent_namespace(function.scope);
    let namespace_chain = m.namespaces.name_chain(ns);

    let fn_name_str = m.name_of(function.name);
    let second = namespace_chain.get(1).map(|id| m.name_of(*id));
    // nocommit: Convert all the allocation functions and memcpy and memmove to
    //           k1 intrinsics
    // nocommit: Convert sys/exit to an intrinsic too
    match second {
        // root
        None => Ok(None),
        Some("libc") => match fn_name_str {
            "memcpy" => {
                let [dst, src, count] = call.args[0..3] else { unreachable!() };
                let dst = execute_expr_ev(vm, m, dst)?.expect_ptr();
                let src = execute_expr_ev(vm, m, src)?.expect_ptr();
                let count = execute_expr_ev(vm, m, count)?.expect_int().expect_u64();
                unsafe {
                    std::ptr::copy_nonoverlapping(src as *const u8, dst as *mut u8, count as usize)
                };
                Ok(Some(VmResult::UNIT))
            }
            _ => Ok(None),
        },
        Some("mem") => match fn_name_str {
            "systemZeroAlloc" | "systemMalloc" => {
                let zero = fn_name_str == "systemZeroAlloc";
                let size_expr = call.args[0];
                let align_expr = call.args[1];
                let size = execute_expr_ev(vm, m, size_expr)?.expect_int().expect_u64();
                let align = execute_expr_ev(vm, m, align_expr)?.expect_int().expect_u64();
                let layout =
                    std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();
                let ptr = if zero {
                    unsafe { std::alloc::alloc_zeroed(layout) }
                } else {
                    unsafe { std::alloc::alloc(layout) }
                };
                Ok(Some(Value::Pointer(ptr.addr()).into()))
            }
            "systemRealloc" => {
                let old_ptr_expr = call.args[0];
                let old_size_expr = call.args[1];
                let old_align_expr = call.args[2];
                let new_size_expr = call.args[3];
                let old_ptr = execute_expr_ev(vm, m, old_ptr_expr)?.expect_ptr();
                let old_size = execute_expr_ev(vm, m, old_size_expr)?.expect_int().expect_u64();
                let align = execute_expr_ev(vm, m, old_align_expr)?.expect_int().expect_u64();
                let new_size = execute_expr_ev(vm, m, new_size_expr)?.expect_int().expect_u64();
                let layout =
                    std::alloc::Layout::from_size_align(old_size as usize, align as usize).unwrap();
                let ptr =
                    unsafe { std::alloc::realloc(old_ptr as *mut u8, layout, new_size as usize) };
                Ok(Some(Value::Pointer(ptr.addr()).into()))
            }
            "systemFree" => {
                let ptr_expr = call.args[0];
                let size_expr = call.args[1];
                let align_expr = call.args[2];
                let ptr = execute_expr_ev(vm, m, ptr_expr)?.expect_ptr();
                let size = execute_expr_ev(vm, m, size_expr)?.expect_int().expect_u64();
                let align = execute_expr_ev(vm, m, align_expr)?.expect_int().expect_u64();

                let layout =
                    std::alloc::Layout::from_size_align(size as usize, align as usize).unwrap();
                unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };
                Ok(Some(VmResult::UNIT))
            }
            _ => Ok(None),
        },
        Some("sys") => match fn_name_str {
            "exit" => {
                let code_expr = call.args[0];
                let TypedIntValue::I32(code) = execute_expr_ev(vm, m, code_expr)?.expect_int()
                else {
                    unreachable!("wrong exit code type")
                };
                Ok(Some(VmResult::Exit(code)))
            }
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn render_debug_value(w: &mut impl std::fmt::Write, vm: &mut Vm, m: &TypedModule, value: Value) {
    eprintln!("render debug of {:?} and {}", value, m.type_id_to_string(value.get_type()));
    match value {
        Value::Unit => write!(w, "()").unwrap(),
        Value::Bool(b) => {
            if b {
                write!(w, "true").unwrap()
            } else {
                write!(w, "false").unwrap()
            }
        }
        Value::Char(c) => write!(w, "{}", c).unwrap(),
        Value::Int(int) => write!(w, "{}", int).unwrap(),
        Value::Float(float) => write!(w, "{}", float).unwrap(),
        Value::Pointer(p) => write!(w, "{}", p).unwrap(),
        Value::Reference { type_id, ptr } => {
            let type_to_load = m.types.get_type_id_dereferenced(type_id);
            eprintln!(
                "render debug of reference and inner type {}",
                m.type_id_to_string(type_to_load)
            );
            //write!(w, "{}", ptr.addr()).unwrap();
            match load_value(vm, m, type_to_load, ptr, true) {
                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                Ok(loaded) => render_debug_value(w, vm, m, loaded),
            };
        }
        Value::Agg { type_id, ptr } => {
            match m.types.get(type_id) {
                st @ Type::Struct(struct_type) => {
                    if let Some(buffer) = st.as_buffer_instance() {
                        write!(w, "<buffer>").unwrap()
                    } else {
                        write!(w, "{{ ").unwrap();
                        for f in &struct_type.fields {
                            write!(w, "{}: ", m.name_of(f.name)).unwrap();
                            match load_struct_field(vm, m, type_id, ptr, f.index as usize, true) {
                                Err(e) => write!(w, "<ERROR {}>", e.message).unwrap(),
                                Ok(loaded) => render_debug_value(w, vm, m, loaded),
                            };
                            write!(w, ", ").unwrap();
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
