use std::{
    collections::VecDeque,
    ptr::{slice_from_raw_parts, slice_from_raw_parts_mut},
};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use log::debug;
use smallvec::SmallVec;

use crate::{
    errf, failf,
    lex::SpanId,
    nz_u32_id,
    parse::{Identifier, NumericWidth},
    pool,
    typer::{
        self, make_error, make_fail_span,
        types::{
            IntegerType, StructType, StructTypeField, Type, TypeId, Types, BOOL_TYPE_ID,
            CHAR_TYPE_ID, POINTER_TYPE_ID, STRING_TYPE_ID, UNIT_TYPE_ID,
        },
        BinaryOpKind, Call, CastType, FunctionId, Layout, TypedExpr, TypedExprId, TypedFloatValue,
        TypedIntegerValue, TypedModule, TypedStmtId, TyperResult, VariableId,
    },
};

#[cfg(test)]
mod vm_test;

pub struct Vm {
    call_stack: VecDeque<StackFrame>,
}

impl Vm {
    pub fn make() -> Self {
        // TODO(vm): Re-use this memory, we really want zero-allocation VM invocations for very
        //           simple expressions
        Self { call_stack: VecDeque::with_capacity(8) }
    }

    fn push_frame(&mut self, name: String) -> &mut StackFrame {
        let frame = StackFrame::make(name);
        self.call_stack.push_back(frame);
        self.call_stack.back_mut().unwrap()
    }

    fn pop_frame(&mut self) -> StackFrame {
        self.call_stack.pop_back().unwrap()
    }

    fn current_frame(&self) -> &StackFrame {
        self.call_stack.back().unwrap()
    }
    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.call_stack.back_mut().unwrap()
    }

    pub fn dump(&self, m: &TypedModule) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        let w = &mut s;
        for frame in &self.call_stack {
            writeln!(w, "Frame {}", frame.debug_name).unwrap();
            writeln!(w, "Locals").unwrap();
            for (k, v) in &frame.locals {
                writeln!(w, "  {}: {:?}", m.name_of(m.variables.get(*k).name), v).unwrap();
            }

            write!(w, "\nDATA ({} bytes)\n", frame.current_offset_bytes()).unwrap();
            frame.to_bytes().chunks(8).for_each(|bytes| {
                for b in bytes {
                    write!(w, "{:02x} ", b).unwrap();
                }
                writeln!(w).unwrap();
            });
        }
        s
    }
}

crate::static_assert_size!(Value, 24);
#[derive(Debug, Clone, Copy)]
pub enum Value {
    // 'Immediate' values
    Unit,
    Bool(bool),
    Char(u8),
    Integer(TypedIntegerValue),
    Float(TypedFloatValue),
    Pointer(usize),
    // Reference is immediate since copying, loading or storing one only
    // entails a load or store of its pointer
    Reference { type_id: TypeId, ptr: *const [u8] },
    // 'By address' values
    Struct { type_id: TypeId, ptr: *const [u8] },
    Enum { type_id: TypeId, ptr: *const [u8] },
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
            | Value::Integer(_)
            | Value::Float(_)
            | Value::Pointer(_) => true,
            Value::Reference { .. } => true,
            Value::Struct { .. } => false,
            Value::Enum { .. } => false,
        }
    }

    pub fn ptr_if_not_immediate(&self) -> Option<*const [u8]> {
        match self {
            Value::Struct { ptr, .. } => Some(*ptr),
            Value::Enum { ptr, .. } => Some(*ptr),
            _ => None,
        }
    }

    pub fn get_type(&self) -> TypeId {
        match self {
            Value::Unit => UNIT_TYPE_ID,
            Value::Bool(_) => BOOL_TYPE_ID,
            Value::Char(_) => CHAR_TYPE_ID,
            Value::Integer(typed_integer_value) => typed_integer_value.get_type(),
            Value::Float(typed_float_value) => typed_float_value.get_type(),
            Value::Pointer(_) => POINTER_TYPE_ID,
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

pub fn execute_single_expr(m: &mut TypedModule, expr: TypedExprId) -> TyperResult<(Vm, Value)> {
    let mut vm = Vm::make();
    vm.push_frame(format!("single_expr_{}", expr));
    let v = execute_expr(&mut vm, m, expr)?;
    Ok((vm, v))
}

fn execute_expr(vm: &mut Vm, m: &TypedModule, expr: TypedExprId) -> TyperResult<Value> {
    log::set_max_level(log::LevelFilter::Debug);
    scopeguard::defer! {
        log::set_max_level(log::LevelFilter::Info)
    };

    debug!("VM execute: {}", m.expr_to_string_with_type(expr));
    let result = match m.exprs.get(expr) {
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
            let base_ptr: *const [u8] = slice_from_raw_parts(s.as_ptr(), s.len());
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

            let string_struct = Value::Struct { type_id: STRING_TYPE_ID, ptr: string_addr };
            Ok(string_struct)
        }
        TypedExpr::Struct(s) => {
            let mut values: SmallVec<[Value; 8]> = SmallVec::with_capacity(s.fields.len());
            let fields = s.fields.clone();
            let s_type_id = s.type_id;
            for field in &fields {
                let value = execute_expr(vm, m, field.expr)?;
                values.push(value);
            }

            let frame = vm.current_frame_mut();
            let struct_base = frame.push_struct_values(&m.types, s_type_id, &values);
            Ok(Value::Struct { type_id: s_type_id, ptr: struct_base })
        }
        TypedExpr::StructFieldAccess(field_access) => {
            todo!()
        }
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
                    let Value::Reference { ptr, .. } = execute_expr(vm, m, ref_expr)? else {
                        m.ice_with_span("malformed dereference", span)
                    };
                    load_value(vm, &m, target_type, ptr, span)
                }
            }
        }
        TypedExpr::BinaryOp(bin_op) => match bin_op.kind {
            BinaryOpKind::Equals => {
                let bin_op = bin_op.clone();
                let lhs = execute_expr(vm, m, bin_op.lhs)?;
                let rhs = execute_expr(vm, m, bin_op.rhs)?;
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
        TypedExpr::Block(typed_block) => execute_block(vm, m, expr),
        TypedExpr::Call(call) => execute_fn_call(vm, m, call),
        TypedExpr::Match(match_expr) => {
            todo!("vm match")
        }
        TypedExpr::WhileLoop(while_loop) => todo!(),
        TypedExpr::LoopExpr(loop_expr) => todo!(),
        TypedExpr::EnumConstructor(typed_enum_constructor) => todo!(),
        TypedExpr::EnumIsVariant(typed_enum_is_variant_expr) => todo!(),
        TypedExpr::EnumGetPayload(get_enum_payload) => todo!(),
        TypedExpr::Cast(typed_cast) => {
            let typed_cast = typed_cast.clone();
            let span = typed_cast.span;
            let base_value = execute_expr(vm, m, typed_cast.base_expr)?;
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
                    Ok(Value::Pointer(u as usize))
                }
                CastType::KnownNoOp => Ok(base_value),
                CastType::PointerToReference => {
                    let Value::Pointer(value) = base_value else {
                        m.ice_with_span("malformed pointer-to-reference cast", span)
                    };
                    let pointee_layout = m
                        .types
                        .get_layout(m.types.get_type_id_dereferenced(typed_cast.target_type_id))
                        .unwrap();
                    Ok(Value::Reference {
                        type_id: typed_cast.target_type_id,
                        ptr: slice_from_raw_parts(value as *const u8, pointee_layout.size_bytes()),
                    })
                }
                CastType::ReferenceToPointer => {
                    let Value::Reference { ptr, .. } = base_value else {
                        m.ice_with_span("malformed reference-to-pointer cast", span)
                    };
                    Ok(Value::Pointer(ptr.addr()))
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
        TypedExpr::Break(typed_break) => todo!(),
        TypedExpr::Lambda(lambda_expr) => todo!(),
        TypedExpr::FunctionReference(function_reference_expr) => todo!(),
        TypedExpr::FunctionToLambdaObject(function_to_lambda_object_expr) => todo!(),
        TypedExpr::PendingCapture(pending_capture_expr) => todo!(),
        TypedExpr::StaticValue(_, _, _) => {
            unreachable!("Already evaluated? Or do I need to support this if we reference a global in a static fn?")
        }
    };
    debug!("  -> {:?}", result);
    result
}

pub fn execute_block(vm: &mut Vm, m: &TypedModule, block_expr: TypedExprId) -> TyperResult<Value> {
    let mut last_value = Value::UNIT;
    let TypedExpr::Block(typed_block) = m.exprs.get(block_expr) else { unreachable!() };
    // TODO(vm): clone of block statements vec
    let stmts = typed_block.statements.clone();
    for stmt in stmts.iter() {
        last_value = execute_stmt(vm, m, *stmt)?;
    }
    Ok(last_value)
}

pub fn execute_stmt(vm: &mut Vm, m: &TypedModule, stmt_id: TypedStmtId) -> TyperResult<Value> {
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
                let reference_type = let_stmt.variable_type;
                let value_type = v.get_type();
                debug_assert_eq!(
                    m.types.get(reference_type).expect_reference().inner_type,
                    value_type
                );
                let stack_ptr = vm.current_frame_mut().push_ptr_uninit();
                store_value(&m.types, value_type, stack_ptr, v);
                Value::Reference { type_id: reference_type, ptr: stack_ptr }
            } else {
                debug_assert_eq!(let_stmt.variable_type, v.get_type());
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
                    store_value(&m.types, type_id, ptr.cast_mut(), v);
                    Ok(Value::UNIT)
                }
            }
        }
        typer::TypedStmt::Require(typed_require_stmt) => {
            todo!("static require not yet implemented")
        }
    }
}

fn execute_fn_call(vm: &mut Vm, m: &TypedModule, call: &Call) -> TyperResult<Value> {
    let span = call.span;
    let function_id = match call.callee {
        typer::Callee::StaticFunction(function_id) => function_id,
        typer::Callee::StaticLambda { function_id, environment_ptr, lambda_type_id } => {
            function_id
        }
        typer::Callee::StaticAbstract { generic_function_id, function_type } => {
            m.ice_with_span("Abstract call attempt in VM", span)
        }
        typer::Callee::DynamicLambda(typed_expr_id) => {
            return failf!(span, "Function pointers are not supported in static; one day I could JIT the functions and get pointers; oh my")
        }
        typer::Callee::DynamicFunction(typed_expr_id) => {
            return failf!(span, "Function pointers are not supported in static")
        }
        typer::Callee::DynamicAbstract { variable_id, function_type } => {
            m.ice_with_span("Abstract call attempt in VM", span)
        }
    };
    let function = m.get_function(function_id);
    vm.push_frame(m.name_of(function.name).to_string());
    let Some(body_block_expr) = function.body_block else {
        eprintln!("{}", m.function_id_to_string(function_id, true));
        return failf!(span, "Cannot execute function: no body");
    };
    let result_value = execute_expr(vm, m, body_block_expr)?;
    // result_value lives in the stack frame we're about to nuke
    // It just to be copied onto the current stack frame...
    let callee_frame = vm.pop_frame();
    let caller_frame = vm.current_frame_mut();
    // TODO: Handle 'never'

    // If immediate, just 'Copy' the rust value; otherwise memcpy the data to the new stack
    match result_value.ptr_if_not_immediate() {
        None => Ok(result_value),
        Some(ptr) => {
            let stored_result = caller_frame.push_value(&m.types, result_value);
            let updated_value = match result_value {
                Value::Struct { type_id, ptr } => Value::Struct { type_id, ptr: stored_result },
                Value::Enum { type_id, ptr } => Value::Enum { type_id, ptr: stored_result },
                _ => unreachable!(),
            };
            Ok(updated_value)
        }
    }
}

pub fn store_value(types: &Types, type_id: TypeId, dst: *mut [u8], value: Value) -> usize {
    // nocommit: I think this is really all StackFrame is doing in push_value...
    // Steps to DRY: 1. Call this from push_value with dst = cursor
    //               2  Make this return bytes written
    //               3. Bump cursor by bytes_written
    unsafe {
        match value {
            Value::Unit => {
                let dst = dst as *mut u8;
                dst.write(typer::UNIT_BYTE_VALUE);
                size_of::<u8>()
            }
            Value::Bool(b) => {
                let dst = dst as *mut u8;
                dst.write(b as u8);
                size_of::<u8>()
            }
            Value::Char(b) => {
                let dst = dst as *mut u8;
                dst.write(b as u8);
                size_of::<u8>()
            }
            Value::Integer(value) => match value {
                TypedIntegerValue::U8(v) => {
                    (dst as *mut u8).write(v);
                    size_of::<u8>()
                }
                TypedIntegerValue::I8(v) => {
                    (dst as *mut i8).write(v);
                    size_of::<i8>()
                }
                TypedIntegerValue::U16(v) => {
                    (dst as *mut u16).write(v);
                    size_of::<u16>()
                }
                TypedIntegerValue::I16(v) => {
                    (dst as *mut i16).write(v);
                    size_of::<i16>()
                }
                TypedIntegerValue::U32(v) => {
                    (dst as *mut u32).write(v);
                    size_of::<u32>()
                }
                TypedIntegerValue::I32(v) => {
                    (dst as *mut i32).write(v);
                    size_of::<i32>()
                }
                TypedIntegerValue::U64(v) => {
                    (dst as *mut u64).write(v);
                    size_of::<u64>()
                }
                TypedIntegerValue::I64(v) => {
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
                debug_assert_eq!(dst.len(), size_of_val(&value));
                (dst as *mut usize).write(value);
                size_of::<usize>()
            }
            Value::Reference { ptr, .. } => {
                (dst as *mut usize).write(ptr.addr());
                size_of::<usize>()
            }
            Value::Struct { type_id, ptr } => {
                let struct_layout = types.get_layout(type_id).unwrap();
                debug_assert_eq!(dst.len(), struct_layout.size_bytes());
                debug_assert_eq!(dst.len(), ptr.len());
                let count = dst.len();
                // Equivalent of memmove; safer than memcpy
                (dst as *mut u8).copy_from(ptr.cast(), count);
                count
            }
            Value::Enum { .. } => todo!(),
        }
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_value(
    vm: &mut Vm,
    m: &TypedModule,
    type_id: TypeId,
    ptr: *const [u8],
    span: SpanId,
) -> TyperResult<Value> {
    let Some(layout) = m.types.get_layout(type_id) else {
        m.ice_with_span("Cannot dereference a type with no known layout", span)
    };
    eprintln!("Layout for {}: {:?}", m.type_id_to_string(type_id), layout);
    match m.types.get(type_id) {
        Type::Unit => Ok(Value::UNIT),
        Type::Char => {
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
        Type::Bool => {
            debug_assert_eq!(layout.size_bits, 8);
            let byte = unsafe { *(ptr as *const u8) };
            Ok(Value::Bool(byte != 0))
        }
        Type::Pointer => {
            debug_assert_eq!(layout.size_bytes(), size_of::<usize>());
            debug_assert_eq!(ptr.len(), size_of::<usize>());
            let value = unsafe { *(ptr as *const usize) };
            Ok(Value::Pointer(value))
        }
        Type::Reference(reference_type) => {
            debug_assert_eq!(layout.size_bytes(), size_of::<usize>());
            debug_assert_eq!(ptr.len(), size_of::<usize>());
            // We're loading a reference, which means we are loading a usize
            // that points to something with layout `layout`
            let len = layout.size_bytes();
            let value: *const () = unsafe { *(ptr as *const *const ()) };
            // We materialize a len value using the type system
            let loaded_ptr = slice_from_raw_parts(value as *const u8, len);
            Ok(Value::Reference { type_id: reference_type.inner_type, ptr: loaded_ptr })
        }
        Type::Struct(_struct_type) => {
            let layout = m.types.get_layout(type_id).unwrap();
            let frame = vm.current_frame_mut();
            let frame_ptr = frame.push_raw_copy_layout(layout, ptr);
            Ok(Value::Struct { type_id, ptr: frame_ptr })
        }
        Type::Enum(typed_enum) => todo!(),
        Type::EnumVariant(typed_enum_variant) => todo!(),
        Type::Never => unreachable!("Not a value type"),
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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_struct_field(
    vm: &mut Vm,
    m: &TypedModule,
    struct_type: TypeId,
    struct_ptr: *const [u8],
    field_index: usize,
    span: SpanId,
) -> TyperResult<Value> {
    let struct_type = m.types.get(struct_type).expect_struct();
    let field = &struct_type.fields[field_index];
    let field_layout = m.types.get_layout(field.type_id).unwrap();
    let field_ptr = unsafe {
        slice_from_raw_parts(
            (struct_ptr as *const u8).byte_add(field.offset_bits as usize / 8),
            field_layout.size_bytes(),
        )
    };

    let value = load_value(vm, m, field.type_id, field_ptr, span)?;
    Ok(value)
}

nz_u32_id!(FrameIndex);

struct StackFrame {
    buffer: Box<[u8; 8192]>,
    pub cursor: *const u8,
    pub locals: FxHashMap<VariableId, Value>,
    debug_name: String,
}

impl Drop for StackFrame {
    fn drop(&mut self) {
        eprintln!("DROP StackFrame {}", self.debug_name)
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

    pub fn push_slice(&mut self, slice: &[u8]) -> *const [u8] {
        let slice_stack_ptr = std::ptr::slice_from_raw_parts_mut(self.cursor_mut(), slice.len());
        let slice_len = slice.len();
        unsafe {
            let dst_slice = std::slice::from_raw_parts_mut(slice_stack_ptr as *mut u8, slice_len);
            // Copy bytes using memcpy:
            dst_slice.copy_from_slice(slice);
        }
        // Would be nice to re-use the 'end' of the slice ptr
        self.cursor = unsafe { self.cursor.byte_add(slice_len) };
        slice_stack_ptr.cast_const()
    }

    fn align_to_bytes(&mut self, align_bytes: usize) {
        let current_ptr_bytes = self.cursor;

        let bytes_needed = current_ptr_bytes.align_offset(align_bytes);
        unsafe {
            self.cursor = self.cursor.byte_add(bytes_needed);
        }
    }

    /// Push `padding_bytes` of zero bytes into the frame buffer
    pub fn push_padding_bytes(&mut self, padding_bytes: usize) {
        eprintln!("Pushing padding: {}bytes", padding_bytes);
        if padding_bytes == 0 {
            return;
        };
        self.push_slice(&vec![0u8; padding_bytes]);
    }

    /// This should be the only place that we 'form' a struct.
    /// nocommit: Alternatively to the current approach, we could just compute the base of each
    /// field and store it; there's no point in pushing padding!
    ///
    /// It might be good to parameterize this over the buffer type
    /// in case we ever need to build a struct in the heap or the Interp memory for export
    pub fn push_struct_values(
        &mut self,
        types: &Types,
        struct_type_id: TypeId,
        members: &[Value],
    ) -> *const [u8] {
        let struct_fields = &types.get(struct_type_id).expect_struct().fields;
        let struct_layout = types.get_layout(struct_type_id).unwrap();

        let mut last_field_end: usize = 0;
        self.align_to_bytes(struct_layout.align_bytes());

        let start_offset_bytes = self.cursor as usize;
        let base_ptr = slice_from_raw_parts(self.cursor, struct_layout.size_bytes());
        for (value, field_type) in members.iter().zip(struct_fields.iter()) {
            let padding = (field_type.offset_bits as usize / 8) - last_field_end;
            eprintln!("Preceeding padding: {padding}");
            self.push_padding_bytes(padding);
            self.push_value_no_align(types, *value);
            last_field_end = (self.cursor as usize) - start_offset_bytes
        }
        base_ptr
    }

    pub fn push_value(&mut self, types: &Types, value: Value) -> *const [u8] {
        let layout = types.get_layout(value.get_type()).unwrap();
        self.align_to_bytes(layout.align_bytes());
        self.push_value_no_align(types, value)
    }

    // We may not want to align, in case we're pushing this value as part of a packed struct.
    pub fn push_value_no_align(&mut self, types: &Types, value: Value) -> *const [u8] {
        let dst = self.cursor_mut();

        // This fat pointer construction is redundant and silly
        let layout = types.get_layout(value.get_type()).unwrap();
        let dst: *mut [u8] = slice_from_raw_parts_mut(dst, layout.size_bytes());

        let written = store_value(types, value.get_type(), dst, value);
        debug_assert_eq!(written, layout.size_bytes());
        self.advance_cursor(written);
        dst.cast_const()
    }

    pub fn advance_cursor(&mut self, count: usize) {
        self.cursor = unsafe { self.cursor.byte_add(count) };
    }

    pub fn push_byte(&mut self, u: u8) -> *const [u8] {
        let r = self.cursor as *mut u8;
        unsafe {
            *r = u;
            self.cursor = self.cursor.byte_add(1);
        }
        slice_from_raw_parts(r.cast_const(), 1)
    }

    pub fn push_ptr_uninit(&mut self) -> *mut [u8] {
        self.push_usize(0).cast_mut()
    }

    pub fn push_usize(&mut self, value: usize) -> *const [u8] {
        match size_of::<usize>() {
            4 => self.push_integer(TypedIntegerValue::U32(value as u32)),
            8 => self.push_integer(TypedIntegerValue::U64(value as u64)),
            x => unreachable!("usize is {x}"),
        }
    }

    pub fn push_integer(&mut self, value: TypedIntegerValue) -> *const [u8] {
        match value {
            TypedIntegerValue::U8(v) => self.push_byte(v),
            TypedIntegerValue::I8(v) => self.push_byte(v as u8),
            TypedIntegerValue::U16(v) => self.push_slice(&v.to_le_bytes()),
            TypedIntegerValue::I16(v) => self.push_slice(&v.to_le_bytes()),
            TypedIntegerValue::U32(v) => self.push_slice(&v.to_le_bytes()),
            TypedIntegerValue::I32(v) => self.push_slice(&v.to_le_bytes()),
            TypedIntegerValue::U64(v) => self.push_slice(&v.to_le_bytes()),
            TypedIntegerValue::I64(v) => self.push_slice(&v.to_le_bytes()),
        }
    }

    /// Push a raw copy of `size_bytes` from `src_ptr` into the frame buffer.
    /// The copy is aligned to `align_bytes`.
    fn push_raw_copy(
        &mut self,
        size_bytes: usize,
        align_bytes: usize,
        src_ptr: *const [u8],
    ) -> *const [u8] {
        debug_assert_eq!(src_ptr.len(), size_bytes);
        self.align_to_bytes(align_bytes);
        let src = src_ptr as *const u8;

        unsafe {
            let src_slice = std::slice::from_raw_parts(src, size_bytes);
            self.push_slice(src_slice)
        }
    }

    fn push_raw_copy_layout(&mut self, layout: Layout, src_ptr: *const [u8]) -> *const [u8] {
        self.push_raw_copy(layout.size_bytes(), layout.align_bytes(), src_ptr)
    }

    pub fn to_bytes(&self) -> &[u8] {
        &self.buffer[0..self.current_offset_bytes()]
    }
}
