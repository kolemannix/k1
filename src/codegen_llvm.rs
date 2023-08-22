use anyhow::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;

use inkwell::module::Linkage;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType, StructType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
    InstructionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
use log::trace;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use crate::ir::*;
use crate::parse::IdentifierId;

struct BuiltinTypes<'ctx> {
    i64: IntType<'ctx>,
    unit: IntType<'ctx>,
    unit_value: IntValue<'ctx>,
    boolean: IntType<'ctx>,
    true_value: IntValue<'ctx>,
    false_value: IntValue<'ctx>,
    char: IntType<'ctx>,
    c_str: PointerType<'ctx>,
    string: StructType<'ctx>,
}

struct LibcFunctions<'ctx> {
    printf: FunctionValue<'ctx>,
}

pub struct Codegen<'ctx> {
    ctx: &'ctx Context,
    pub module: Rc<IrModule>,
    llvm_module: inkwell::module::Module<'ctx>,
    llvm_machine: Option<TargetMachine>,
    builder: Builder<'ctx>,
    llvm_functions: HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_types: HashMap<TypeId, BasicTypeEnum<'ctx>>,
    /// The type of stored pointers here should be one level higher than the type of the
    /// value pointed to. This is so that it can be used reliably, without matching or
    /// checking, as a Pointer to the actual type
    variables: HashMap<VariableId, Pointer<'ctx>>,
    globals: HashMap<VariableId, GlobalValue<'ctx>>,
    libc_functions: LibcFunctions<'ctx>,
    builtin_globals: HashMap<String, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    default_address_space: AddressSpace,
    heap_address_space: AddressSpace,
}

#[derive(Copy, Clone)]
struct Pointer<'ctx> {
    pointee_ty: BasicTypeEnum<'ctx>,
    pointer: PointerValue<'ctx>,
}

impl<'ctx> Pointer<'ctx> {
    fn loaded_value(&self, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
        builder.build_load(self.pointee_ty, self.pointer, "loaded_value")
    }
}

impl<'ctx> From<Pointer<'ctx>> for GeneratedValue<'ctx> {
    fn from(ptr: Pointer<'ctx>) -> Self {
        GeneratedValue::Pointer(ptr)
    }
}

impl<'ctx> From<&Pointer<'ctx>> for GeneratedValue<'ctx> {
    fn from(ptr: &Pointer<'ctx>) -> Self {
        GeneratedValue::Pointer(*ptr)
    }
}

/// When we codegen an expression, sometimes the result is a pointer value, which does
/// not contain type information, so we add the pointee type in that case
#[derive(Copy, Clone)]
enum GeneratedValue<'ctx> {
    Value(BasicValueEnum<'ctx>),
    Pointer(Pointer<'ctx>),
}

impl<'ctx> From<BasicValueEnum<'ctx>> for GeneratedValue<'ctx> {
    fn from(value: BasicValueEnum<'ctx>) -> Self {
        GeneratedValue::Value(value)
    }
}

impl<'ctx> GeneratedValue<'ctx> {
    /// This conversion loses type info but is necessary often for passing to LLVM
    /// calls, such as for function calls, we need to convert from GeneratedValue BasicMetaDataValueEnum
    fn as_basic_value_enum(&self) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Value(value) => *value,
            GeneratedValue::Pointer(pointer) => pointer.pointer.as_basic_value_enum(),
        }
    }

    fn loaded_value(&self, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Pointer(pointer) => pointer.loaded_value(builder),
            GeneratedValue::Value(value) => *value,
        }
    }

    fn loaded_value_debug(&self, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Pointer(pointer) => pointer.loaded_value(builder),
            GeneratedValue::Value(value) => *value,
        }
    }

    fn expect_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Value(value) => *value,
            GeneratedValue::Pointer(_pointer) => panic!("Expected Value but got Pointer"),
        }
    }
    fn expect_pointer(&self) -> Pointer<'ctx> {
        match self {
            GeneratedValue::Value(_value) => panic!("Expected Pointer but got Value"),
            GeneratedValue::Pointer(pointer) => *pointer,
        }
    }
    fn value_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            GeneratedValue::Value(value) => value.get_type(),
            GeneratedValue::Pointer(pointer) => pointer.pointee_ty,
        }
    }
}

pub fn init_context() -> Context {
    Context::create()
}

type CodegenResult<T> = Result<T>;

fn i8_array_from_str<'ctx>(ctx: &'ctx Context, value: &str) -> ArrayValue<'ctx> {
    let chars =
        value.bytes().map(|b| ctx.i8_type().const_int(b as u64, false)).collect::<Vec<IntValue>>();
    ctx.i8_type().const_array(&chars)
}

impl<'ctx> Codegen<'ctx> {
    pub fn create(ctx: &'ctx Context, module: Rc<IrModule>) -> Codegen<'ctx> {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let llvm_module = ctx.create_module(&module.ast.name);
        let pointers = HashMap::new();
        let format_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(3), None, "formatString");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%i\n"));
            global
        };
        let globals = HashMap::new();
        let mut builtin_globals: HashMap<String, GlobalValue<'ctx>> = HashMap::new();
        builtin_globals.insert("formatString".to_string(), format_str);

        let builtin_types = BuiltinTypes {
            i64: ctx.i64_type(),
            unit: ctx.bool_type(),
            unit_value: ctx.bool_type().const_int(0, false),
            boolean: ctx.bool_type(),
            true_value: ctx.bool_type().const_int(1, true),
            false_value: ctx.bool_type().const_int(0, true),
            char: char_type,
            c_str: char_type.ptr_type(AddressSpace::default()),
            string: ctx.struct_type(
                &[
                    BasicTypeEnum::IntType(ctx.i64_type()),
                    BasicTypeEnum::PointerType(char_type.ptr_type(AddressSpace::default())),
                ],
                false,
            ),
        };

        let printf_type = ctx.i32_type().fn_type(&[builtin_types.c_str.into()], true);
        let printf = llvm_module.add_function("printf", printf_type, Some(Linkage::External));
        Codegen {
            ctx,
            module,
            llvm_module,
            llvm_machine: None,
            builder,
            variables: pointers,
            globals,
            llvm_functions: HashMap::new(),
            llvm_types: HashMap::new(),
            libc_functions: LibcFunctions { printf },
            builtin_globals,
            builtin_types,
            default_address_space: AddressSpace::default(),
            heap_address_space: AddressSpace::from(1),
        }
    }

    fn get_ident_name(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        self.module.ast.get_ident_name(id)
    }

    fn build_print_int_call(&mut self, call: &FunctionCall) -> BasicValueEnum<'ctx> {
        // Assume the arg is an int since that's what the intrinsic typechecks for
        let first_arg = self.codegen_expr(&call.args[0]).loaded_value(&self.builder);

        let format_str = self.builtin_globals.get("formatString").unwrap();
        let format_str_ptr = self.builder.build_bitcast(
            format_str.as_pointer_value(),
            self.builtin_types.c_str,
            "fmt_str",
        );
        let call = self
            .builder
            .build_call(
                self.libc_functions.printf,
                &[format_str_ptr.into(), first_arg.into()],
                "printf",
            )
            .try_as_basic_value()
            .left()
            .unwrap();
        call.set_name("println_res");
        call
    }
    fn codegen_type(&mut self, type_ref: TypeRef) -> BasicTypeEnum<'ctx> {
        trace!("codegen for type {type_ref:?}");
        match type_ref {
            TypeRef::Int => self.builtin_types.i64.as_basic_type_enum(),
            TypeRef::Bool => self.builtin_types.boolean.as_basic_type_enum(),
            TypeRef::String => self.builtin_types.string.as_basic_type_enum(),
            TypeRef::Unit => self.builtin_types.unit.as_basic_type_enum(),
            TypeRef::TypeId(type_id) => {
                match self.llvm_types.get(&type_id) {
                    None => {
                        // Generate and store the type in here
                        let module = self.module.clone();
                        let ty = module.get_type(type_id);
                        match ty {
                            Type::Record(record) => {
                                trace!("generating llvm pointer type for record type {type_id}");
                                let field_types: Vec<_> =
                                    record.fields.iter().map(|f| self.codegen_type(f.ty)).collect();
                                let struct_type = self.ctx.struct_type(&field_types, false);
                                self.llvm_types.insert(type_id, struct_type.as_basic_type_enum());
                                struct_type.as_basic_type_enum()
                            }
                            Type::OpaqueAlias(_alias) => {
                                todo!("opaque alias")
                            }
                            Type::Array(_array) => {
                                todo!("codegen for Array")
                            }
                        }
                    }
                    Some(basic_ty) => *basic_ty,
                }
            }
        }
    }

    fn codegen_val(&mut self, val: &ValDef) -> GeneratedValue<'ctx> {
        let value = self.codegen_expr(&val.initializer);
        let pointee_ty = value.value_type();
        let value_ptr = self.builder.build_alloca(
            pointee_ty.ptr_type(self.default_address_space),
            &val.variable_id.to_string(),
        );
        trace!("codegen_val {}: pointee_ty: {pointee_ty:?}", val.variable_id.to_string());
        // Rather than LOADing to the stack, we want to make a pointer to our value,
        // whether our value is a pointer or not. So we're always storing a pointer
        // in self.variables that, when loaded, gives the actual type of the variable
        self.builder.build_store(value_ptr, value.as_basic_value_enum());
        // self.builder.build_store(value_ptr, value.loaded_value_debug(&self.builder));
        let pointer = Pointer { pointer: value_ptr, pointee_ty };
        self.variables.insert(val.variable_id, pointer);
        pointer.into()
    }

    fn codegen_literal(&mut self, literal: &IrLiteral) -> GeneratedValue<'ctx> {
        match literal {
            IrLiteral::Unit(_) => self.builtin_types.unit_value.as_basic_value_enum().into(),
            IrLiteral::Bool(b, _) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum().into(),
                false => self.builtin_types.false_value.as_basic_value_enum().into(),
            },
            IrLiteral::Int(int_value, _) => {
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                let value = self.builtin_types.i64.const_int(*int_value as u64, false);
                value.as_basic_value_enum().into()
            }
            IrLiteral::Str(_, _) => todo!("codegen String"),
            IrLiteral::Record(record) => {
                let ty = self.codegen_type(TypeRef::TypeId(record.type_id));
                // let struct_ty = ty.get_element_type().into_struct_type();
                let struct_ptr = self.builder.build_alloca(ty, "record");
                for (idx, field) in record.fields.iter().enumerate() {
                    let value = self.codegen_expr(&field.expr);
                    let field_ptr =
                        self.builder.build_struct_gep(ty, struct_ptr, idx as u32, "").unwrap();
                    self.builder.build_store(field_ptr, value.loaded_value(&self.builder));
                }
                GeneratedValue::Pointer(Pointer { pointee_ty: ty, pointer: struct_ptr })
            }
            IrLiteral::Array(array) => {
                // First, we allocate memory somewhere not on the stack
                let Type::Array(array_ir_type) = self.module.get_type(array.type_id) else {
                    panic!("expected array type for array");
                };
                let element_type = self.codegen_type(array_ir_type.element_type);
                let array_len = self.builtin_types.i64.const_int(array.elements.len() as u64, true);
                let array_ptr = self
                    .builder
                    .build_array_malloc(element_type, array_len, "array_literal")
                    .unwrap();
                // Store each element
                for (index, element_expr) in array.elements.iter().enumerate() {
                    let value = self.codegen_expr(element_expr);
                    let index_value = self.ctx.i64_type().const_int(index as u64, true);
                    log::trace!("storing element {} of array literal: {:?}", index, element_expr);
                    let ptr_at_index = unsafe {
                        self.builder.build_gep(element_type, array_ptr, &[index_value], "elem")
                    };
                    self.builder.build_store(ptr_at_index, value.loaded_value(&self.builder));
                }

                let array_type = element_type
                    .array_type(array.elements.len() as u32)
                    .as_basic_type_enum()
                    .ptr_type(self.default_address_space);
                GeneratedValue::Pointer(Pointer {
                    pointee_ty: array_type.as_basic_type_enum(),
                    pointer: array_ptr,
                })
            }
        }
    }
    fn codegen_if_else(&mut self, ir_if: &IrIf) -> GeneratedValue<'ctx> {
        let typ = self.codegen_type(ir_if.ir_type);
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let consequent_block = self.ctx.append_basic_block(current_fn, "if_cons");
        let alternate_block = self.ctx.append_basic_block(current_fn, "if_alt");
        let merge_block = self.ctx.append_basic_block(current_fn, "if_merge");

        // Entry block
        let condition = self.codegen_expr(&ir_if.condition);
        let condition_value = condition.loaded_value(&self.builder).into_int_value();
        self.builder.build_conditional_branch(condition_value, consequent_block, alternate_block);

        // Consequent Block
        // If any of these blocks have an early return, they'll return None, and we'll panic for
        // now
        self.builder.position_at_end(consequent_block);
        let consequent_expr = self
            .codegen_block(&ir_if.consequent)
            .expect("Expected IF branch block to return something")
            .loaded_value(&self.builder);
        let consequent_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block);

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_expr = self
            .codegen_block(&ir_if.alternate)
            .expect("Expected IF branch block to return something")
            .loaded_value(&self.builder);
        // TODO: Cleaner to have codegen_block RETURN its 'terminus' block rather than
        //       rely on the builder state
        let alternate_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block);

        // Merge block
        self.builder.position_at_end(merge_block);
        let phi_value = self.builder.build_phi(typ, "if_phi");
        phi_value.add_incoming(&[
            (&consequent_expr, consequent_final_block),
            (&alternate_expr, alternate_final_block),
        ]);
        phi_value.as_basic_value().into()
    }
    fn codegen_expr(&mut self, expr: &IrExpr) -> GeneratedValue<'ctx> {
        match expr {
            IrExpr::Literal(literal) => self.codegen_literal(literal),
            IrExpr::Variable(ir_var) => {
                log::trace!("codegen variable expr {ir_var:?}");
                if let Some(pointer) = self.variables.get(&ir_var.variable_id) {
                    log::trace!("codegen variable got type {:?}", pointer.pointee_ty);
                    let loaded = pointer.loaded_value(&self.builder);
                    loaded.into()
                } else if let Some(global) = self.globals.get(&ir_var.variable_id) {
                    let value = global.get_initializer().unwrap();
                    value.into()
                } else {
                    panic!("No pointer or global found for variable {:?}", ir_var)
                }
            }
            IrExpr::FieldAccess(field_access) => {
                let target_value = self.codegen_expr(&field_access.base).as_basic_value_enum();
                let target_ptr = self.build_store(target_value, "record_access_ptr");
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        target_value.get_type(),
                        target_ptr.as_basic_value_enum().into_pointer_value(),
                        field_access.target_field.index as u32,
                        "field_access_target_ptr",
                    )
                    .unwrap();
                let target_ty = self.codegen_type(field_access.target_field.ty);
                let loaded = self.builder.build_load(target_ty, field_ptr, "field_access_target");
                loaded.into()
            }
            IrExpr::If(ir_if) => self.codegen_if_else(ir_if),
            IrExpr::BinaryOp(bin_op) => match bin_op.ir_type {
                TypeRef::Int => {
                    if bin_op.kind.is_integer_op() {
                        let lhs_value = self
                            .codegen_expr(&bin_op.lhs)
                            .loaded_value(&self.builder)
                            .into_int_value();
                        let rhs_value = self
                            .codegen_expr(&bin_op.rhs)
                            .loaded_value(&self.builder)
                            .into_int_value();
                        let op_res = match bin_op.kind {
                            BinaryOpKind::Add => {
                                self.builder.build_int_add(lhs_value, rhs_value, "add")
                            }
                            BinaryOpKind::Multiply => {
                                self.builder.build_int_mul(lhs_value, rhs_value, "mul")
                            }
                            BinaryOpKind::And => {
                                self.builder.build_and(lhs_value, rhs_value, "and")
                            }
                            BinaryOpKind::Or => self.builder.build_or(lhs_value, rhs_value, "or"),
                            BinaryOpKind::Equals => self.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs_value,
                                rhs_value,
                                "eq",
                            ),
                        };
                        op_res.as_basic_value_enum().into()
                    } else {
                        panic!("Unsupported binary operation {:?} on Int", bin_op.kind)
                    }
                }
                TypeRef::Bool => match bin_op.kind {
                    BinaryOpKind::And | BinaryOpKind::Or | BinaryOpKind::Equals => {
                        let lhs_int = self
                            .codegen_expr(&bin_op.lhs)
                            .loaded_value(&self.builder)
                            .into_int_value();
                        let rhs_int = self
                            .codegen_expr(&bin_op.rhs)
                            .loaded_value(&self.builder)
                            .into_int_value();
                        let op = match bin_op.kind {
                            BinaryOpKind::And => {
                                self.builder.build_and(lhs_int, rhs_int, "bool_and")
                            }
                            BinaryOpKind::Or => self.builder.build_or(lhs_int, rhs_int, "bool_or"),
                            BinaryOpKind::Equals => self.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs_int,
                                rhs_int,
                                "bool_eq",
                            ),
                            _ => panic!(),
                        };
                        op.as_basic_value_enum().into()
                    }
                    other => panic!("Unsupported binary operation {other:?} on Bool"),
                },
                TypeRef::String => panic!("No string binary ops yet"),
                TypeRef::Unit => panic!("No unit binary ops"),
                TypeRef::TypeId(_) => todo!("codegen for binary ops on user-defined types"),
            },
            IrExpr::Block(_block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and assign the return value to an alloca
                todo!("codegen lexical block")
            }
            IrExpr::FunctionCall(call) => self.codegen_function_call(call),
        }
    }
    fn codegen_function_call(&mut self, call: &FunctionCall) -> GeneratedValue<'ctx> {
        let ir_module = self.module.clone();
        let callee = ir_module.get_function(call.callee_function_id);
        if let Some(intrinsic_type) = callee.intrinsic_type {
            return self.codegen_intrinsic(intrinsic_type, call);
        }

        let function_value =
            *self.llvm_functions.get(&call.callee_function_id).unwrap_or_else(|| {
                panic!("LLVM function not found: {}", &*self.get_ident_name(callee.name))
            });
        let args: Vec<BasicMetadataValueEnum<'ctx>> = call
            .args
            .iter()
            .map(|arg_expr| {
                let basic_value = self.codegen_expr(arg_expr);
                basic_value.loaded_value(&self.builder).into()
            })
            .collect();
        let callsite_value = self.builder.build_call(function_value, &args, "");
        // Call returns Right for void, and Left for values
        let result_value =
            callsite_value.try_as_basic_value().left().expect("function returned void");
        result_value.into()
    }

    fn codegen_intrinsic(
        &mut self,
        intrinsic_type: IntrinsicFunctionType,
        call: &FunctionCall,
    ) -> GeneratedValue<'ctx> {
        match intrinsic_type {
            IntrinsicFunctionType::PrintInt => {
                self.build_print_int_call(call);
                self.builtin_types.unit_value.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::ArrayIndex => {
                let pointee_ty = self.codegen_type(call.ret_type);
                let array_expr = &call.args[0];
                let index_expr = &call.args[1];
                let array_value = self.codegen_expr(array_expr);
                let index_value = self.codegen_expr(index_expr);
                // Arrays are always malloc'd in this language, so we expect a pointer
                // This doesnt have to be the case, but I want dynamic arrays to be the default
                let array_value_as_ptr = array_value.as_basic_value_enum().into_pointer_value();
                let index_int_value = index_value.loaded_value(&self.builder).into_int_value();
                unsafe {
                    let gep_ptr = self.builder.build_gep(
                        pointee_ty,
                        array_value_as_ptr,
                        &[index_int_value],
                        "array_index",
                    );
                    Pointer { pointee_ty, pointer: gep_ptr }.into()
                }
            }
        }
    }
    // This needs to return either a basic value or an instruction value (in the case of early return)
    // Actually, early return is a big rabbit hole. We need to typecheck it in ir gen, and probably
    // store it on the block
    //
    // For now, I'm going to return an Option. If the block has an early return, we just return
    // None. We'll fix it when implementing early returns
    // Maybe we rename ReturnStmt to Early Return to separate it from tail returns, which have
    // pretty different semantics and implications for codegen, I am realizing
    fn codegen_block(&mut self, block: &IrBlock) -> Option<GeneratedValue<'ctx>> {
        let mut last: Option<GeneratedValue<'ctx>> = None;
        for stmt in &block.statements {
            match stmt {
                IrStmt::Expr(expr) => last = Some(self.codegen_expr(expr)),
                IrStmt::ValDef(val_def) => {
                    let value = self.codegen_val(val_def).expect_pointer();
                    last = Some(value.into())
                }
                IrStmt::ReturnStmt(return_stmt) => {
                    let value = self.codegen_expr(&return_stmt.expr);
                    self.builder.build_return(Some(&value.loaded_value(&self.builder)));
                    return None;
                }
                IrStmt::Assignment(assignment) => {
                    let variable_id = &assignment.destination_variable;
                    let destination_ptr =
                        *self.variables.get(variable_id).expect("Missing variable");
                    let initializer = self.codegen_expr(&assignment.value);
                    let _store = self.store(&destination_ptr, initializer);
                    last = Some(self.builtin_types.unit_value.as_basic_value_enum().into())
                }
            }
        }
        last
    }
    fn build_store(&mut self, value: BasicValueEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let ptr = self.builder.build_alloca(value.get_type(), name);
        self.builder.build_store(ptr, value);
        ptr
    }
    /// Stores `value` into `pointer`, loading the value pointed to by `value` if necessary
    fn store(
        &mut self,
        pointer: &Pointer<'ctx>,
        value: GeneratedValue<'ctx>,
    ) -> InstructionValue<'ctx> {
        self.builder.build_store(pointer.pointer, value.loaded_value(&self.builder))
    }
    pub fn codegen_module(&mut self) {
        for constant in &self.module.constants {
            match constant.expr {
                IrExpr::Literal(IrLiteral::Int(i64, _)) => {
                    let llvm_ty = self.builtin_types.i64;
                    let llvm_val =
                        self.llvm_module.add_global(llvm_ty, Some(AddressSpace::default()), "");
                    llvm_val.set_constant(true);
                    llvm_val.set_initializer(&llvm_ty.const_int(i64 as u64, false));
                    self.globals.insert(constant.variable_id, llvm_val);
                }
                _ => todo!("constant must be int"),
            }
        }
        // TODO: It is gross to assume the ID is the index here, just because its implemented
        //       that way. We should expose a way to iterate that provides the ID from the IR module
        //       This will matter once the ID is more than just the index (it will at least need
        //       module ID)
        for (function_id, function) in self.module.clone().functions.iter().enumerate() {
            if function.intrinsic_type.is_some() {
                // FIXME: Is this correct to not even codegen a function?
                //        It means we are 'inlining' all intrinsics unconditionally
                //        Probably should make an llvm function and implement it ourselves,
                //        and call it regularly
                continue;
            }
            let param_types: Vec<BasicMetadataTypeEnum> = function
                .params
                .iter()
                .map(|fn_arg| self.codegen_type(fn_arg.ir_type).into())
                .collect();
            let ret_type: BasicMetadataTypeEnum<'ctx> = self.codegen_type(function.ret_type).into();
            let fn_ty = match ret_type {
                BasicMetadataTypeEnum::IntType(i) => i.fn_type(&param_types, false),
                BasicMetadataTypeEnum::PointerType(p) => p.fn_type(&param_types, false),
                _ => panic!("Unexpected function llvm type"),
            };
            let fn_val = {
                let name = self.module.ast.get_ident_name(function.name);
                self.llvm_module.add_function(&name, fn_ty, None)
            };
            self.llvm_functions.insert(function_id as u32, fn_val);
            let entry_block = self.ctx.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry_block);
            for (i, param) in fn_val.get_param_iter().enumerate() {
                let ir_param = &function.params[i];
                let ty = self.codegen_type(ir_param.ir_type);
                let param_name = self.module.ast.get_ident_name(ir_param.name);
                param.set_name(&param_name);
                let pointer = self.builder.build_alloca(ty, &param_name);
                self.builder.build_store(pointer, param);
                self.variables.insert(ir_param.variable_id, Pointer { pointer, pointee_ty: ty });
            }
            self.codegen_block(
                function.block.as_ref().expect("functions must have blocks by codegen"),
            );
        }
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    pub fn optimize(&mut self) -> CodegenResult<()> {
        Target::initialize_aarch64(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        self.llvm_module.verify().map_err(|err| {
            eprintln!("{}", self.llvm_module.to_string());
            anyhow::anyhow!("Module '{}' failed validation: {}", self.name(), err.to_string_lossy())
        })?;
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        self.llvm_module.set_data_layout(&machine.get_target_data().get_data_layout());
        self.llvm_module.set_triple(&triple);
        let pass_manager_b = PassManagerBuilder::create();
        pass_manager_b.set_optimization_level(OptimizationLevel::Aggressive);
        // pass_manager_b.populate_function_pass_manager(&function_pass_manager);
        // pass_manager_b.populate_module_pass_manager(&module_pass_manager);
        // The PassManager::create function works for Module and Function. If module,
        // the expected input is (). If function, the expected input is a Module
        // add_verifier_pass
        // let function_pass_manager: PassManager<FunctionValue<'ctx>> =
        //     PassManager::create(&self.llvm_module);
        // function_pass_manager.add_verifier_pass();

        let module_pass_manager: PassManager<inkwell::module::Module<'ctx>> =
            PassManager::create(());
        module_pass_manager.add_promote_memory_to_register_pass();
        module_pass_manager.add_function_attrs_pass();
        module_pass_manager.add_instruction_combining_pass();
        // module_pass_manager.add_function_inlining_pass();
        module_pass_manager.add_verifier_pass();

        module_pass_manager.run_on(&self.llvm_module);

        machine.add_analysis_passes(&module_pass_manager);

        self.llvm_machine = Some(machine);

        Ok(())
    }

    pub fn emit_object_file(&self) -> Result<()> {
        let filename = format!("{}.o", self.name());
        println!("Outputting object file to {filename}");
        let machine =
            self.llvm_machine.as_ref().expect("Cannot emit object file before optimizing");
        let path = Path::new("./artifacts").join(Path::new(&filename));
        machine
            .write_to_file(&self.llvm_module, inkwell::targets::FileType::Object, &path)
            .unwrap();
        Ok(())
    }

    pub fn output_llvm_ir_text(&self) -> String {
        self.llvm_module.print_to_string().to_string()
    }

    pub fn interpret_module(&self) -> Result<u64> {
        let engine = self.llvm_module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let return_value =
            unsafe { engine.run_function(self.llvm_module.get_last_function().unwrap(), &[]) };
        let res: u64 = return_value.as_int(true);
        Ok(res)
    }
}
