use std::collections::VecDeque;

use ahash::HashMapExt;
use fxhash::FxHashMap;
use log::debug;

use crate::{
    errf, failf,
    lex::SpanId,
    nz_u32_id,
    parse::{Identifier, NumericWidth},
    pool,
    typer::{
        self, make_error, make_fail_span,
        types::{
            IntegerType, StructType, Type, TypeId, Types, BOOL_TYPE_ID, CHAR_TYPE_ID,
            POINTER_TYPE_ID, STRING_TYPE_ID, UNIT_TYPE_ID,
        },
        BinaryOpKind, CastType, FunctionId, TypedExpr, TypedExprId, TypedFloatValue,
        TypedIntegerValue, TypedModule, TypedStmtId, TyperResult, VariableId,
    },
};

#[cfg(test)]
mod vm_test;

pub struct Vm {
    call_stack: VecDeque<StackFrame>,
}

impl Vm {
    fn current_frame(&self) -> &StackFrame {
        self.call_stack.back().unwrap()
    }
    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.call_stack.back_mut().unwrap()
    }
}

nz_u32_id!(FrameIndex);

struct StackFrame {
    pub buffer: Vec<u8>,
    debug_name: String,
}

impl Drop for StackFrame {
    fn drop(&mut self) {
        eprintln!("DROP StackFrame {}", self.debug_name)
    }
}

impl StackFrame {
    pub fn make(name: String) -> Self {
        // TODO(vm): Log resizes
        Self { buffer: Vec::with_capacity(8192), debug_name: name }
    }
    pub fn push_struct_values(&mut self, types: &Types, type_id: TypeId, members: &[Value]) {
        let mut last_field_end = 0;
        let struct_type = types.get(type_id).expect_struct();
        for (value, field_type) in members.iter().zip(struct_type.fields.iter()) {
            let padding = field_type.offset_bits - last_field_end;
            eprintln!("Preceeding padding: {padding}");
            let field_size = types.get_layout(field_type.type_id);
            self.push_padding_bits(padding);
            self.push_value(value);
            last_field_end = field_type.offset_bits
        }
        let struct_layout = types.get_layout(type_id).unwrap();
        let end_padding = struct_layout.stride_bits;
    }
    pub fn push_value(&mut self, value: &Value) {}
    pub fn push_padding_bits(&mut self, padding_bits: u32) {
        let padding_bytes = padding_bits / 8;
        // TODO(vm): Use unsafe to make this a simple edit of .length
        self.buffer.extend_from_slice(&vec![0u8; padding_bytes as usize]);
    }
}

impl Vm {
    pub fn make() -> Self {
        // TODO(vm): Re-use this memory, we really want zero-allocation VM invocations for very
        //           simple expressions
        Self { call_stack: VecDeque::with_capacity(8) }
    }
}

#[derive(Debug, Clone)]
enum ReferenceValue {
    // Memory is owned by K1 code
    Raw(*const ()),
    // Memory is owned by the Vm; current case is just for stack-pointers.
    LetRef(Box<Value>),
}

impl Drop for ReferenceValue {
    fn drop(&mut self) {
        eprintln!("DROP ReferenceValue {:?}", self)
    }
}

crate::static_assert_size!(Value, 24);
#[derive(Debug, Clone)]
pub enum Value {
    // 'Immediate' values
    Unit,
    Bool(bool),
    Char(u8),
    Integer(TypedIntegerValue),
    Float(TypedFloatValue),
    Pointer { value: usize },
    // 'By address' values
    Reference { type_id: TypeId, ptr: *const () },
    Struct { type_id: TypeId, ptr: *const () },
    Enum { type_id: TypeId, ptr: *const () },
}

impl Value {
    pub const UNIT: Value = Value::Unit;
    pub const TRUE: Value = Value::Bool(true);
    pub const FALSE: Value = Value::Bool(false);

    pub fn get_type(&self) -> TypeId {
        match self {
            Value::Unit => UNIT_TYPE_ID,
            Value::Bool(_) => BOOL_TYPE_ID,
            Value::Char(_) => CHAR_TYPE_ID,
            Value::Integer(typed_integer_value) => typed_integer_value.get_type(),
            Value::Float(typed_float_value) => typed_float_value.get_type(),
            Value::Pointer { value } => POINTER_TYPE_ID,
            Value::Reference { type_id, .. } => *type_id,
            Value::Struct { type_id, .. } => *type_id,
            Value::Enum { type_id, .. } => *type_id,
        }
    }

    pub fn kind_str(&self) -> &'static str {
        match self {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::Char(_) => "char",
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::Pointer { .. } => "pointer",
            Value::Reference { .. } => "reference",
            Value::Struct { .. } => "struct",
            Value::Enum { .. } => "enum",
        }
    }
}

pub fn execute_single_expr(m: &mut TypedModule, expr: TypedExprId) -> TyperResult<Value> {
    let mut vm = Vm::make();
    let initial_frame = StackFrame { buffer: Vec::new(), debug_name: format!("expr_{}", expr) };
    vm.call_stack.push_back(initial_frame);
    let v = execute_expr(&mut vm, m, expr)?;
    Ok(v)
}

fn execute_expr(vm: &mut Vm, m: &mut TypedModule, expr: TypedExprId) -> TyperResult<Value> {
    m.push_debug_level();
    let mut m = scopeguard::guard(m, |m| m.pop_debug_level());
    debug!("VM execute: {}", m.expr_to_string_with_type(expr));
    match m.exprs.get(expr) {
        TypedExpr::Unit(_) => Ok(Value::Unit),
        TypedExpr::Char(byte, _) => Ok(Value::Char(*byte)),
        TypedExpr::Bool(b, _) => Ok(Value::Bool(*b)),
        TypedExpr::Integer(typed_integer_expr) => Ok(Value::Integer(typed_integer_expr.value)),
        TypedExpr::Float(typed_float_expr) => Ok(Value::Float(typed_float_expr.value)),
        TypedExpr::String(s, _) => {
            // This needs to result in a string struct
            let string_struct = m.types.get(STRING_TYPE_ID).expect_struct();
            let char_buffer_type_id = string_struct.fields[0].type_id;

            let len: Value = Value::Integer(TypedIntegerValue::U64(s.len() as u64));

            // Just point right at the TypedExpr's string; this is OK
            // since we should never mutate TypedExprs
            let base_ptr: *const u8 = s.as_ptr();

            let data: Value =
                Value::Reference { type_id: CHAR_TYPE_ID, ptr: base_ptr as *const () };
            let frame = vm.current_frame_mut();
            let char_buffer_addr = frame.push_struct(&[len, char_buffer]);
            let addr = frame
                .push_struct(&[Value::Struct { type_id: STRING_TYPE_ID, ptr: char_buffer_addr }]);
            frame.push_reference();
            let char_buffer =
                Value::Struct { type_id: char_buffer_type_id, fields: vec![len, data] };
            let string_struct =
                Value::Struct { type_id: STRING_TYPE_ID, fields: vec![char_buffer] };
            Ok(string_struct)
        }
        TypedExpr::Struct(s) => {
            let mut values = Vec::with_capacity(s.fields.len());
            let fields = s.fields.clone();
            let s_type_id = s.type_id;
            for field in &fields {
                let value = execute_expr(vm, &mut m, field.expr)?;
                values.push(value);
            }
            Ok(Value::Struct { type_id: s_type_id, fields: values })
        }
        TypedExpr::StructFieldAccess(field_access) => todo!(),
        TypedExpr::Variable(variable_expr) => {
            let v_id = variable_expr.variable_id;
            let frame = vm.call_stack.back().unwrap();
            let Some(v) = frame.locals.get(&v_id) else {
                return failf!(
                    variable_expr.span,
                    "Variable missing in vm: {}",
                    m.name_of(m.variables.get(v_id).name)
                );
            };
            // nocommit: How do we deal with this cloning of values from our Hashmap
            // Only bad clone here is the struct fields Vec
            // We can convert Struct to be a ptr too, then Value is basically just a tiny handle?
            Ok(v.clone())
        }
        TypedExpr::UnaryOp(unary_op) => {
            let span = unary_op.span;
            let target_type = unary_op.type_id;
            match unary_op.kind {
                typer::UnaryOpKind::Dereference => {
                    let ref_expr = unary_op.expr;
                    let Value::Reference { ptr, .. } = execute_expr(vm, &mut m, ref_expr)? else {
                        m.ice_with_span("malformed dereference", span)
                    };
                    load_value(&m, target_type, ptr, span)
                }
            }
        }
        TypedExpr::BinaryOp(bin_op) => match bin_op.kind {
            BinaryOpKind::Equals => {
                let bin_op = bin_op.clone();
                let lhs = execute_expr(vm, &mut m, bin_op.lhs)?;
                let rhs = execute_expr(vm, &mut m, bin_op.rhs)?;
                match (lhs, rhs) {
                    (Value::Unit, Value::Unit) => Ok(Value::TRUE),
                    (Value::Bool(b1), Value::Bool(b2)) => {
                        eprintln!("{b1} == {b2}");
                        Ok(Value::Bool(b1 == b2))
                    }
                    (Value::Char(c1), Value::Char(c2)) => Ok(Value::Bool(c1 == c2)),
                    (Value::Integer(i1), Value::Integer(i2)) => Ok(Value::Bool(i1 == i2)),
                    (Value::Float(f1), Value::Float(f2)) => Ok(Value::Bool(f1 == f2)),
                    (lhs, rhs) => {
                        failf!(
                            bin_op.span,
                            "static equality over {} and {} is unimplemented",
                            lhs.kind_str(),
                            rhs.kind_str()
                        )
                    }
                }
            }
            BinaryOpKind::NotEquals => {
                unreachable!("Do we even product NotEquals exprs? Or desugar")
            }
            _ => {
                failf!(bin_op.span, "Unsupported static binary op: {}", m.expr_to_string(expr))
            }
        },
        TypedExpr::Block(typed_block) => {
            let mut last_value = Value::UNIT;
            let stmts = typed_block.statements.clone();
            for stmt in stmts.iter() {
                last_value = execute_stmt(vm, &mut m, *stmt).unwrap();
            }
            Ok(last_value)
        }
        TypedExpr::Call(call) => todo!(),
        TypedExpr::Match(typed_match_expr) => todo!(),
        TypedExpr::WhileLoop(while_loop) => todo!(),
        TypedExpr::LoopExpr(loop_expr) => todo!(),
        TypedExpr::EnumConstructor(typed_enum_constructor) => todo!(),
        TypedExpr::EnumIsVariant(typed_enum_is_variant_expr) => todo!(),
        TypedExpr::EnumGetPayload(get_enum_payload) => todo!(),
        TypedExpr::Cast(typed_cast) => {
            let typed_cast = typed_cast.clone();
            let span = typed_cast.span;
            let base_value = execute_expr(vm, &mut m, typed_cast.base_expr)?;
            match typed_cast.cast_type {
                CastType::IntegerExtend => {
                    let span = typed_cast.span;
                    let Value::Integer(iv) = base_value else {
                        m.ice_with_span("malformed integer cast", span)
                    };
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
                    let span = typed_cast.span;
                    // TODO: If host platform is 32-bit, then we should require a u32 here
                    let Value::Integer(TypedIntegerValue::U64(u)) = base_value else {
                        m.ice_with_span("malformed integer cast", span)
                    };
                    Ok(Value::Pointer { value: u as usize })
                }
                CastType::KnownNoOp => Ok(base_value),
                CastType::PointerToReference => {
                    let Value::Pointer { value } = base_value else {
                        m.ice_with_span("malformed pointer-to-reference cast", span)
                    };
                    Ok(Value::Reference {
                        type_id: typed_cast.target_type_id,
                        ptr: value as *const (),
                    })
                }
                CastType::ReferenceToPointer => {
                    let Value::Reference { ptr, .. } = base_value else {
                        m.ice_with_span("malformed reference-to-pointer cast", span)
                    };
                    Ok(Value::Pointer { value: ptr as usize })
                }
                CastType::PointerToInteger => todo!(),
                CastType::FloatExtend => todo!(),
                CastType::FloatTruncate => todo!(),
                CastType::FloatToInteger => todo!(),
                CastType::LambdaToLambdaObject => todo!(),
            }
        }
        TypedExpr::Return(typed_return) => todo!(),
        TypedExpr::Break(typed_break) => todo!(),
        TypedExpr::Lambda(lambda_expr) => todo!(),
        TypedExpr::FunctionReference(function_reference_expr) => todo!(),
        TypedExpr::FunctionToLambdaObject(function_to_lambda_object_expr) => todo!(),
        TypedExpr::PendingCapture(pending_capture_expr) => todo!(),
        TypedExpr::StaticValue(_, _, _) => {
            unreachable!("Already evaluated? Or do I need to support this if we reference a global in a static fn?")
        }
    }
}

pub fn execute_function(
    vm: &mut Vm,
    m: &mut TypedModule,
    function_id: FunctionId,
) -> TyperResult<()> {
    let function = m.get_function(function_id);
    let function_span = m.ast.get_span_for_id(function.parsed_id);
    let Some(body_id) = function.body_block else {
        return failf!(function_span, "Cannot execute function: no body");
    };
    let TypedExpr::Block(body) = m.exprs.get(body_id) else {
        return failf!(function_span, "Cannot execute function: body is not a block");
    };
    // TODO(clone): Cloning just because of the borrow
    //              Maybe EcoVec since its cheaper to copy is the way
    let stmts = body.statements.clone();
    for stmt_id in &stmts {
        execute_stmt(vm, m, *stmt_id)?;
    }

    todo!()
}

pub fn execute_stmt(vm: &mut Vm, m: &mut TypedModule, stmt_id: TypedStmtId) -> TyperResult<Value> {
    // TODO: Handle divergence / never type. We should exit the VM gracefully and then just
    // report a normal compilation error from `typer`!
    //
    // Do we do a 'never' _Value_? It's a type, not a value. But LLVM does it as a value
    match m.stmts.get(stmt_id) {
        typer::TypedStmt::Expr(typed_expr_id, type_id) => {
            let v = execute_expr(vm, m, *typed_expr_id)?;
            Ok(v)
        }
        typer::TypedStmt::Let(let_stmt) => {
            let let_stmt = let_stmt.clone();
            // TODO: Maybe referencing let needs special treatment, maybe not.
            let v = execute_expr(vm, m, let_stmt.initializer)?;
            let to_store = if let_stmt.is_referencing {
                // We don't have a 'stack', but we could allocate values on
                // the heap in a 'stack' member of `Vm` for now. A bump arena
                // that gets reset when the function returns would be the most correct
                // If we had a call stack, we could do that
                todo!()
            } else {
                v
            };
            vm.current_frame_mut().locals.insert(let_stmt.variable_id, to_store);
            Ok(Value::UNIT)
        }
        typer::TypedStmt::Assignment(assgn) => {
            let assgn = assgn.clone();
            let v = execute_expr(vm, m, assgn.value)?;

            match assgn.kind {
                typer::AssignmentKind::Value => {
                    let TypedExpr::Variable(destination_var) = m.exprs.get(assgn.destination)
                    else {
                        m.ice("Value assignment lhs was not a variable", None)
                    };
                    vm.current_frame_mut().locals.insert(destination_var.variable_id, v);
                    Ok(Value::UNIT)
                }
                typer::AssignmentKind::Reference => {
                    let lhs_value = execute_expr(vm, m, assgn.destination)?;
                    let Value::Reference { type_id, ptr } = lhs_value else {
                        m.ice(
                            format!(
                                "Reference assignment lhs must be a Reference value: {:?}",
                                lhs_value
                            ),
                            None,
                        )
                    };
                    store_value(type_id, ptr, v)?;
                    Ok(Value::UNIT)
                }
            }
        }
        typer::TypedStmt::Require(typed_require_stmt) => {
            todo!("static require not yet implemented")
        }
    }
}

pub fn store_value(type_id: TypeId, ptr: *const (), value: Value) -> TyperResult<()> {
    todo!()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_value(
    m: &TypedModule,
    type_id: TypeId,
    ptr: *const (),
    span: SpanId,
) -> TyperResult<Value> {
    let Some(layout) = m.type_layout(type_id) else {
        m.ice_with_span("Cannot dereference a type with no known layout", span)
    };
    eprintln!("Layout for {}: {:?}", m.type_id_to_string(type_id), layout);
    match m.types.get(type_id) {
        Type::Unit(_) => Ok(Value::UNIT),
        Type::Char(_) => {
            let byte = unsafe { *(ptr as *const u8) };
            Ok(Value::Char(byte))
        }
        Type::Integer(integer_type) => {
            let int_value = unsafe {
                match integer_type {
                    IntegerType::U8 => TypedIntegerValue::U8(*(ptr as *const u8)),
                    IntegerType::U16 => TypedIntegerValue::U16(*(ptr as *const u16)),
                    IntegerType::U32 => TypedIntegerValue::U32(*(ptr as *const u32)),
                    IntegerType::U64 => TypedIntegerValue::U64(*(ptr as *const u64)),
                    IntegerType::I8 => TypedIntegerValue::I8(*(ptr as *const i8)),
                    IntegerType::I16 => TypedIntegerValue::I16(*(ptr as *const i16)),
                    IntegerType::I32 => TypedIntegerValue::I32(*(ptr as *const i32)),
                    IntegerType::I64 => TypedIntegerValue::I64(*(ptr as *const i64)),
                }
            };
            Ok(Value::Integer(int_value))
        }
        Type::Float(float_type) => {
            let float_value = unsafe {
                match float_type.size {
                    NumericWidth::B32 => TypedFloatValue::F32(*(ptr as *const f32)),
                    NumericWidth::B64 => TypedFloatValue::F64(*(ptr as *const f64)),
                    _ => unreachable!(),
                }
            };
            Ok(Value::Float(float_value))
        }
        Type::Bool(_) => {
            debug_assert_eq!(layout.size_bits, 8);
            let byte = unsafe { *(ptr as *const u8) };
            Ok(Value::Bool(byte != 0))
        }
        Type::Pointer(_) => {
            debug_assert_eq!(layout.size_bits as usize, size_of::<usize>());
            let value = unsafe { *(ptr as *const usize) };
            Ok(Value::Pointer { value })
        }
        Type::Reference(reference_type) => {
            debug_assert_eq!(layout.size_bits as usize, size_of::<usize>());
            let value: *const () = unsafe { *(ptr as *const *const ()) };
            Ok(Value::Reference { type_id: reference_type.inner_type, ptr: value })
        }
        Type::Struct(struct_type) => {
            let mut field_values = Vec::with_capacity(struct_type.fields.len());
            for field in struct_type.fields.iter() {
                let offset_bytes = field.offset_bits / 8;

                let field_ptr = unsafe { ptr.byte_add(offset_bytes as usize) };

                let value = load_value(m, field.type_id, field_ptr, span)?;
                field_values.push(value)
            }
            Ok(Value::Struct { type_id, fields: field_values })
        }
        Type::Enum(typed_enum) => todo!(),
        Type::EnumVariant(typed_enum_variant) => todo!(),
        Type::Never(type_defn_info) => unreachable!("Not a value type"),
        Type::Function(function_type) => unreachable!("Not a value type"),
        Type::Lambda(lambda_type) => todo!("just a struct load of the env"),
        Type::LambdaObject(lambda_object_type) => todo!("struct of the lambda obj"),
        Type::Generic(generic_type) => unreachable!("Not a value type"),
        Type::TypeParameter(type_parameter) => unreachable!("Not a value type"),
        Type::FunctionTypeParameter(function_type_parameter) => unreachable!("Not a value type"),
        Type::InferenceHole(inference_hole_type) => unreachable!("Not a value type"),
        Type::RecursiveReference(recursive_reference) => unreachable!("Not a value type"),
    }
}

pub fn load_struct_field(struct_type: TypeId, ptr: *const (), field_index: usize) -> &Value {
    todo!()
}
