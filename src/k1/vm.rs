use std::{collections::VecDeque, num::NonZeroU32};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use log::{debug, info};
use smallvec::{smallvec, SmallVec};

use crate::{
    failf,
    lex::SpanId,
    nz_u32_id,
    parse::NumericWidth,
    typer::{
        self, make_fail_span,
        types::{
            IntegerType, StructTypeField, Type, TypeId, Types, BOOL_TYPE_ID, CHAR_TYPE_ID,
            POINTER_TYPE_ID, STRING_TYPE_ID, UNIT_TYPE_ID,
        },
        BinaryOpKind, Call, CastType, IntrinsicFunction, Layout, MatchingCondition,
        MatchingConditionInstr, StaticValue, TypedExpr, TypedExprId, TypedFloatValue,
        TypedGlobalId, TypedIntegerValue, TypedMatchExpr, TypedModule, TypedStmtId, TyperResult,
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
    call_stack: VecDeque<StackFrame>,
}

impl Vm {
    pub fn make() -> Self {
        // TODO(vm): Re-use this memory, we really want zero-allocation VM invocations for very
        //           simple expressions
        Self { globals: FxHashMap::new(), call_stack: VecDeque::with_capacity(8) }
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
    Reference { type_id: TypeId, ptr: *const u8 },
    // 'By address' values
    Struct { type_id: TypeId, ptr: *const u8 },
    Enum { type_id: TypeId, ptr: *const u8 },
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

    pub fn ptr_if_not_immediate(&self) -> Option<*const u8> {
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

const GLOBAL_ID_STATIC: TypedGlobalId = TypedGlobalId::from_nzu32(NonZeroU32::new(1).unwrap());

pub fn execute_single_expr(m: &mut TypedModule, expr: TypedExprId) -> TyperResult<(Vm, Value)> {
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
            let struct_value = execute_expr(vm, m, field_access.base)?;
            let Some(struct_ptr) = struct_value.ptr_if_not_immediate() else {
                m.ice_with_span("struct field access on immediate value", field_access.span)
            };
            if field_access.is_referencing {
                let (field_ptr, _) = gep_struct_field(
                    m,
                    field_access.struct_type,
                    struct_ptr,
                    field_access.field_index as usize,
                );
                Ok(Value::Reference { type_id: field_access.result_type, ptr: field_ptr })
            } else {
                let field_value = load_struct_field(
                    vm,
                    m,
                    field_access.struct_type,
                    struct_ptr,
                    field_access.field_index as usize,
                    field_access.span,
                )?;
                Ok(field_value)
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
                    Some(global_value) => Ok(*global_value),
                    None => {
                        let initial = m.globals.get(global_id).initial_value;
                        let vm_value =
                            static_value_to_vm_value(vm, m, m.static_values.get(initial))?;

                        vm.globals.insert(global_id, vm_value);
                        Ok(vm_value)
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
                Ok(*v)
            }
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
                    load_value(vm, m, target_type, ptr, span)
                }
            }
        }
        TypedExpr::BinaryOp(bin_op) => {
            let span = bin_op.span;
            use BinaryOpKind as K;
            match bin_op.kind {
                K::Equals => {
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
                                span,
                                "static equality over {} and {} is unimplemented",
                                lhs.kind_str(),
                                rhs.kind_str()
                            )
                        }
                    }
                }
                K::NotEquals => {
                    unreachable!("Do we even product NotEquals exprs? Or desugar")
                }

                K::Add | K::Subtract | K::Multiply | K::Divide | K::Rem => {
                    let lhs = execute_expr(vm, m, bin_op.lhs)?;
                    let rhs = execute_expr(vm, m, bin_op.rhs)?;
                    binop::execute_arith_op(lhs, rhs, bin_op.kind, span)
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
        TypedExpr::LoopExpr(_) => todo!(),
        TypedExpr::EnumConstructor(_) => todo!(),
        TypedExpr::EnumIsVariant(_) => todo!(),
        TypedExpr::EnumGetPayload(_) => todo!(),
        TypedExpr::EnumGetTag(_) => todo!(),
        TypedExpr::Cast(typed_cast) => {
            let typed_cast = typed_cast.clone();
            let span = typed_cast.span;
            let base_value = execute_expr(vm, m, typed_cast.base_expr)?;
            match typed_cast.cast_type {
                CastType::IntegerExtend => {
                    let span = typed_cast.span;
                    let Value::Integer(_) = base_value else {
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
                    Ok(Value::Reference {
                        type_id: typed_cast.target_type_id,
                        ptr: value as *const u8,
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
        TypedExpr::Break(_) => todo!(),
        TypedExpr::Lambda(_) => todo!(),
        TypedExpr::FunctionReference(_) => todo!(),
        TypedExpr::FunctionToLambdaObject(_) => todo!(),
        TypedExpr::PendingCapture(_) => todo!(),
        TypedExpr::StaticValue(_, _, _) => {
            unreachable!("Already evaluated? Or do I need to support this if we reference a global in a static fn?")
        }
    };
    debug!("  -> {:?}", result);
    result
}

pub fn static_value_to_vm_value(
    vm: &mut Vm,
    m: &TypedModule,
    static_value: &StaticValue,
) -> TyperResult<Value> {
    match static_value {
        StaticValue::Unit(_) => Ok(Value::Unit),
        StaticValue::Boolean(bv, _) => Ok(Value::Bool(*bv)),
        StaticValue::Char(cb, _) => Ok(Value::Char(*cb)),
        StaticValue::Integer(iv, _) => Ok(Value::Integer(*iv)),
        StaticValue::Float(fv, _) => Ok(Value::Float(*fv)),
        StaticValue::String(_box_str, _) => {
            failf!(static_value.get_span(), "static str to vm value")
        }
        StaticValue::Pointer(value, _) => Ok(Value::Pointer(*value as usize)),
        StaticValue::Struct(static_struct) => {
            let mut values: SmallVec<[Value; 8]> = smallvec![];
            for f in static_struct.fields.iter() {
                let static_value = m.static_values.get(*f);
                let value = static_value_to_vm_value(vm, m, static_value)?;
                values.push(value);
            }
            let struct_ptr =
                vm.current_frame_mut().push_struct_values(&m.types, static_struct.type_id, &values);
            Ok(Value::Struct { type_id: static_struct.type_id, ptr: struct_ptr })
        }
        StaticValue::Enum(_compile_time_enum) => {
            failf!(static_value.get_span(), "static enum to vm value")
        }
    }
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
        typer::TypedStmt::Expr(typed_expr_id, _) => {
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
                store_value(&m.types, stack_ptr, v);
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
                    Ok(Value::UNIT)
                }
            }
        }
        typer::TypedStmt::Require(_) => {
            todo!("static require not yet implemented")
        }
    }
}

fn execute_call(vm: &mut Vm, m: &TypedModule, call: &Call) -> TyperResult<Value> {
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
                Ok(Value::Integer(TypedIntegerValue::U64(size_bytes as u64)))
            }
            IntrinsicFunction::SizeOfStride => {
                let type_id = call.type_args[0].type_id;
                let layout = m.types.get_layout(type_id);
                let stride_bytes = match layout {
                    Some(l) => l.stride_bytes(),
                    None => 0,
                };
                // TODO: Should be a usize; but K1 doesn't have one
                Ok(Value::Integer(TypedIntegerValue::U64(stride_bytes as u64)))
            }
            IntrinsicFunction::AlignOf => {
                let type_id = call.type_args[0].type_id;
                let layout = m.types.get_layout(type_id);
                let align_bytes = match layout {
                    Some(l) => l.align_bytes(),
                    None => 0,
                };
                // TODO: Should be a usize; but K1 doesn't have one
                Ok(Value::Integer(TypedIntegerValue::U64(align_bytes as u64)))
            }
            IntrinsicFunction::TypeId => {
                let type_id = call.type_args[0].type_id;
                Ok(Value::Integer(TypedIntegerValue::U64(type_id.to_u64())))
            }
            IntrinsicFunction::BoolNegate => {
                let Value::Bool(b) = execute_expr(vm, m, call.args[0])? else { unreachable!() };
                return Ok(Value::Bool(!b));
            }
            IntrinsicFunction::BitNot => todo!(),
            IntrinsicFunction::BitAnd => todo!(),
            IntrinsicFunction::BitOr => todo!(),
            IntrinsicFunction::BitXor => todo!(),
            IntrinsicFunction::BitShiftLeft => todo!(),
            IntrinsicFunction::BitShiftRight => todo!(),
            IntrinsicFunction::PointerIndex => todo!(),
            IntrinsicFunction::CompilerSourceLocation => todo!(),
        };
    };

    let Some(body_block_expr) = function.body_block else {
        eprintln!("{}", m.function_id_to_string(function_id, true));
        if let Some(result) = handle_special_call(vm, m, call)? {
            return Ok(result);
        } else {
            return failf!(span, "Cannot execute function {}: no body", m.name_of(function.name));
        }
    };

    let mut callee_frame = StackFrame::make(m.name_of(function.name).to_string());

    // Arguments!
    for (param, arg) in function.param_variables.iter().zip(call.args.iter()) {
        // Execute this in _caller_ frame so the data outlives the callee
        let value = execute_expr(vm, m, *arg)?;

        // But bind the variables in the _callee_ frame
        callee_frame.locals.insert(*param, value);
    }

    // Push the callee frame for body execution
    vm.push_frame(callee_frame);

    let result_value = execute_expr(vm, m, body_block_expr)?;
    // result_value lives in the stack frame we're about to nuke
    // It just to be copied onto the current stack frame...
    // TODO: Handle 'never'

    // If immediate, just 'Copy' the rust value; otherwise memcpy the data to the new stack
    let updated_value = match result_value.ptr_if_not_immediate() {
        None => result_value,
        Some(_ptr) => {
            let caller_frame = vm.current_frame_mut();
            let stored_result = caller_frame.push_value(&m.types, result_value);
            let updated_value = match result_value {
                Value::Struct { type_id, .. } => Value::Struct { type_id, ptr: stored_result },
                Value::Enum { type_id, .. } => Value::Enum { type_id, ptr: stored_result },
                _ => unreachable!(),
            };
            updated_value
        }
    };
    vm.pop_frame();
    Ok(updated_value)
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
            Value::Integer(value) => match value {
                TypedIntegerValue::U8(v) => {
                    dst.write(v);
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
                (dst as *mut usize).write(value);
                size_of::<usize>()
            }
            Value::Reference { ptr, .. } => {
                (dst as *mut usize).write(ptr.addr());
                size_of::<usize>()
            }
            Value::Struct { type_id, ptr } => {
                let struct_layout = types.get_layout(type_id).unwrap();
                let count = struct_layout.size_bytes();
                // Equivalent of memmove; safer than memcpy
                dst.copy_from(ptr.cast(), count);
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
    ptr: *const u8,
    span: SpanId,
) -> TyperResult<Value> {
    let Some(layout) = m.types.get_layout(type_id) else {
        m.ice_with_span("Cannot dereference a type with no known layout", span)
    };
    eprintln!("Layout for {}: {:?}", m.type_id_to_string(type_id), layout);
    match m.types.get(type_id) {
        Type::Unit => Ok(Value::UNIT),
        Type::Char => {
            let byte = unsafe { ptr.read() };
            Ok(Value::Char(byte))
        }
        Type::Integer(integer_type) => {
            let int_value = unsafe {
                match integer_type {
                    IntegerType::U8 => TypedIntegerValue::U8(ptr.read()),
                    IntegerType::U16 => TypedIntegerValue::U16((ptr as *const u16).read()),
                    IntegerType::U32 => TypedIntegerValue::U32((ptr as *const u32).read()),
                    IntegerType::U64 => TypedIntegerValue::U64((ptr as *const u64).read()),
                    IntegerType::I8 => TypedIntegerValue::I8((ptr as *const i8).read()),
                    IntegerType::I16 => TypedIntegerValue::I16((ptr as *const i16).read()),
                    IntegerType::I32 => TypedIntegerValue::I32((ptr as *const i32).read()),
                    IntegerType::I64 => TypedIntegerValue::I64((ptr as *const i64).read()),
                }
            };
            Ok(Value::Integer(int_value))
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
            Ok(Value::Reference { type_id: reference_type.inner_type, ptr: loaded_ptr })
        }
        Type::Struct(_struct_type) => {
            let layout = m.types.get_layout(type_id).unwrap();
            let frame = vm.current_frame_mut();
            let frame_ptr = frame.push_raw_copy_layout(layout, ptr);
            Ok(Value::Struct { type_id, ptr: frame_ptr })
        }
        Type::Enum(_) => todo!(),
        Type::EnumVariant(_) => todo!(),
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
    m: &TypedModule,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
) -> (*const u8, &StructTypeField) {
    let struct_type = m.types.get(struct_type).expect_struct();
    let field = &struct_type.fields[field_index];

    let field_ptr = unsafe { struct_ptr.byte_add(field.offset_bits as usize / 8) };
    (field_ptr, field)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn load_struct_field(
    vm: &mut Vm,
    m: &TypedModule,
    struct_type: TypeId,
    struct_ptr: *const u8,
    field_index: usize,
    span: SpanId,
) -> TyperResult<Value> {
    let (field_ptr, field) = gep_struct_field(m, struct_type, struct_ptr, field_index);
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

    pub fn push_slice(&mut self, slice: &[u8]) -> *const u8 {
        let slice_stack_ptr = self.cursor_mut();
        let slice_len = slice.len();

        unsafe {
            let dst_slice = std::slice::from_raw_parts_mut(slice_stack_ptr, slice_len);
            // Copy bytes using memcpy:
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

fn execute_match(vm: &mut Vm, m: &TypedModule, match_expr: &TypedMatchExpr) -> TyperResult<Value> {
    for stmt in &match_expr.initial_let_statements {
        execute_stmt(vm, m, *stmt)?;
    }
    for arm in &match_expr.arms {
        let Value::Bool(b) = execute_matching_condition(vm, m, &arm.condition)? else {
            unreachable!()
        };
        if b {
            info!("vm: arm true");
            let r = execute_expr(vm, m, arm.consequent_expr)?;
            return Ok(r);
        } else {
            info!("vm: next arm");
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
                let Value::Bool(cond_bool) = execute_expr(vm, m, *value)? else { unreachable!() };
                if !cond_bool {
                    return Ok(Value::Bool(false));
                }
            }
        }
    }
    Ok(Value::Bool(true))
}

fn handle_special_call(vm: &mut Vm, m: &TypedModule, call: &Call) -> TyperResult<Option<Value>> {
    Ok(None)
}
