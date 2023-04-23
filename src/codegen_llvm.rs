use anyhow::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType, StringRadix, StructType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
    IntValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use crate::{ir::*, trace};

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

struct BuiltinFunctions<'ctx> {
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
    /// TODO: Pointers should be scoped and stuff not global
    ///
    /// The type of stored pointers here should be one level higher than the type of the
    /// value pointed to. This is to that it can be used reliably, without matching or
    /// checking, as a Pointer to the actual type
    ///
    /// I guess we could also just store Values too, doesn't have to always be pointers.
    /// But we're only going to use these for things that can be re-assigned, or for structs
    /// which need to be treated as pointers. It might be cool to abstract this behind
    /// some enum that can be either a Value or a PointerValue, and knows what its target type is,
    /// and can insert a load as needed. PointerOrValue.as_pointer() or .as_value() or something.
    pointers: HashMap<VariableId, Pointer<'ctx>>,
    globals: HashMap<VariableId, GlobalValue<'ctx>>,
    builtin_functions: BuiltinFunctions<'ctx>,
    builtin_globals: HashMap<String, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    default_address_space: AddressSpace,
}

#[derive(Copy, Clone)]
struct Pointer<'ctx> {
    pointee_ty: BasicTypeEnum<'ctx>,
    pointer: PointerValue<'ctx>,
}

/// When we codegen an expression, sometimes the result is a pointer value, which does
/// not contain type information, so we return the pointee type in that case
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

    fn expect_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Value(value) => *value,
            GeneratedValue::Pointer(pointer) => panic!("Expected Value but got Pointer"),
        }
    }
    fn expect_pointer(&self) -> Pointer<'ctx> {
        match self {
            GeneratedValue::Value(value) => panic!("Expected Pointer but got Value"),
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
        let llvm_module = ctx.create_module(&module.ast.name.0);
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
            pointers,
            globals,
            llvm_functions: HashMap::new(),
            llvm_types: HashMap::new(),
            builtin_functions: BuiltinFunctions { printf },
            builtin_globals,
            builtin_types,
            default_address_space: AddressSpace::default(),
        }
    }

    fn to_ptr_type(&self, ty: BasicTypeEnum<'ctx>) -> PointerType<'ctx> {
        ty.ptr_type(self.default_address_space)
    }

    fn build_printf_call(&mut self, call: &FunctionCall) -> BasicValueEnum<'ctx> {
        // Assume the arg is an int since that's what the intrinsic typechecks for
        let first_arg = self.codegen_expr(&call.args[0]).expect_value().into_int_value();

        let format_str = self.builtin_globals.get("formatString").unwrap();
        let format_str_ptr = self.builder.build_bitcast(
            format_str.as_pointer_value(),
            self.builtin_types.c_str,
            "fmt_str",
        );
        let call = self
            .builder
            .build_call(
                self.builtin_functions.printf,
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
        trace!("typechecking {type_ref:?}");
        match type_ref {
            TypeRef::Int => self.builtin_types.i64.as_basic_type_enum(),
            TypeRef::Bool => self.builtin_types.boolean.as_basic_type_enum(),
            TypeRef::String => self.builtin_types.string.as_basic_type_enum(),
            TypeRef::Unit => self.builtin_types.unit.as_basic_type_enum(),
            TypeRef::TypeId(type_id) => {
                match self.llvm_types.get(&type_id) {
                    None => {
                        // Generate and store the type in here
                        let module = &self.module.clone();
                        let ty = module.get_type(type_id);
                        match ty {
                            Type::Record(record) => {
                                trace!("generating llvm pointer type for record type {type_id}");
                                let field_types: Vec<_> =
                                    record.fields.iter().map(|f| self.codegen_type(f.ty)).collect();
                                let struct_type = self
                                    .ctx
                                    .struct_type(&field_types, false)
                                    .ptr_type(self.default_address_space);
                                self.llvm_types.insert(type_id, struct_type.as_basic_type_enum());
                                struct_type.as_basic_type_enum()
                            }
                            Type::OpaqueAlias(alias) => {
                                todo!("opaque alias")
                            }
                        }
                    }
                    Some(basic_ty) => *basic_ty,
                }
            }
        }
    }
    fn eval_basic_metadata_ty(&mut self, ir_type: TypeRef) -> BasicMetadataTypeEnum<'ctx> {
        self.codegen_type(ir_type).as_basic_type_enum().into()
    }
    fn codegen_val(&mut self, val: &ValDef) -> GeneratedValue<'ctx> {
        let value = self.codegen_expr(&val.initializer);
        value
    }
    fn codegen_literal(&mut self, literal: &IrLiteral) -> GeneratedValue<'ctx> {
        match literal {
            IrLiteral::Record(record) => {
                let ty = self.codegen_type(TypeRef::TypeId(record.type_id));
                // ty.into_pointer_type().get_element_type()
                let struct_ptr = self.builder.build_alloca(ty, "record");
                for (idx, field) in record.fields.iter().enumerate() {
                    let value = self.codegen_expr(&field.expr);
                    let field_ptr = self
                        .builder
                        .build_struct_gep(ty, struct_ptr, idx as u32, &field.name)
                        .unwrap();
                    self.builder.build_store(field_ptr, value);
                }
                GeneratedValue::Pointer(Pointer { pointee_ty: ty, pointer: struct_ptr })
            }
            IrLiteral::Unit(_) => self.builtin_types.unit_value.as_basic_value_enum().into(),
            IrLiteral::Int(int_value, _) => {
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                let value = self.builtin_types.i64.const_int(*int_value as u64, false);
                value.as_basic_value_enum().into()
            }
            IrLiteral::Bool(b, _) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum().into(),
                false => self.builtin_types.false_value.as_basic_value_enum().into(),
            },
            IrLiteral::Str(_, _) => todo!("codegen String"),
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
        let condition_value = condition.expect_value().into_int_value();
        self.builder.build_conditional_branch(condition_value, consequent_block, alternate_block);

        // Consequent Block
        // If any of these blocks have an early return, they'll return None, and we'll panic for
        // now
        self.builder.position_at_end(consequent_block);
        let consequent_expr = self
            .codegen_block(&ir_if.consequent)
            .expect("Expected IF branch block to return something");
        self.builder.build_unconditional_branch(merge_block);

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_expr = self
            .codegen_block(&ir_if.alternate)
            .expect("Expected IF branch block to return something");
        self.builder.build_unconditional_branch(merge_block);

        // Merge block
        self.builder.position_at_end(merge_block);
        let phi_value = self.builder.build_phi(typ, "if_phi");
        phi_value.add_incoming(&[
            (&consequent_expr, consequent_block),
            (&alternate_expr, alternate_block),
        ]);
        phi_value.as_basic_value().into()
    }
    fn codegen_expr(&mut self, expr: &IrExpr) -> GeneratedValue<'ctx> {
        match expr {
            IrExpr::Literal(literal) => self.codegen_literal(literal),
            IrExpr::Variable(ir_var) => {
                if let Some(ptr) = self.pointers.get(&ir_var.variable_id) {
                    let ty = self.codegen_type(ir_var.ir_type);
                    // TODO: We may not need to load the pointer now
                    // since we have our new enum that carries more type info
                    let loaded =
                        self.builder.build_load(ptr.pointee_ty, ptr.pointer, "loaded_variable");
                    loaded.into()
                } else if let Some(global) = self.globals.get(&ir_var.variable_id) {
                    let value = global.get_initializer().unwrap();
                    value.into()
                } else {
                    panic!("No pointer or global found for variable {:?}", ir_var)
                }
            }
            IrExpr::FieldAccess(field_access) => {
                let target_ptr = self.codegen_expr(&field_access.base).expect_pointer();
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        target_ptr.pointee_ty,
                        target_ptr.pointer,
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
                        let lhs_value =
                            self.codegen_expr(&bin_op.lhs).expect_value().into_int_value();
                        let rhs_value =
                            self.codegen_expr(&bin_op.rhs).expect_value().into_int_value();
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
                        };
                        op_res.as_basic_value_enum().into()
                    } else {
                        panic!("Unsupported binary operation {:?} on Int", bin_op.kind)
                    }
                }
                TypeRef::Bool => match bin_op.kind {
                    BinaryOpKind::And | BinaryOpKind::Or => {
                        let lhs_int =
                            self.codegen_expr(&bin_op.lhs).expect_value().into_int_value();
                        let rhs_int =
                            self.codegen_expr(&bin_op.rhs).expect_value().into_int_value();
                        let op = match bin_op.kind {
                            BinaryOpKind::And => {
                                self.builder.build_and(lhs_int, rhs_int, "bool_and")
                            }
                            BinaryOpKind::Or => self.builder.build_or(lhs_int, rhs_int, "bool_or"),
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
            IrExpr::Block(block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and assign the return value to an alloca
                todo!("codegen block")
            }
            IrExpr::FunctionCall(call) => {
                let ir_module = self.module.clone();
                let callee = ir_module.get_function(call.callee_function_id);
                if !&callee.is_intrinsic {
                    let function_value: FunctionValue = self
                        .llvm_module
                        .get_function(&callee.name)
                        .unwrap_or_else(|| panic!("LLVM function not found: {}", callee.name));
                    let args: Vec<BasicMetadataValueEnum<'ctx>> = call
                        .args
                        .iter()
                        .map(|arg_expr| {
                            let basic_value = self.codegen_expr(arg_expr);
                            BasicMetadataValueEnum::from(basic_value.as_basic_value_enum())
                        })
                        .collect();
                    let callsite_value =
                        self.builder.build_call(function_value, &args, &callee.name);
                    // Call return Right for void, and Left for values
                    let result_value =
                        callsite_value.try_as_basic_value().left().expect("function returned void");
                    result_value.into()
                } else if callee.name == "println" {
                    // TODO: This is our first 'intrinsic'
                    // Support intrinsics properly in the IR or prelude source or something
                    self.build_printf_call(call);
                    let ptr = self.builder.build_alloca(self.builtin_types.unit, "println_res_ptr");
                    self.builder.build_store(ptr, self.builtin_types.unit.const_int(0, false));
                    self.builtin_types.unit_value.as_basic_value_enum().into()
                } else {
                    todo!("Unexpected intrinic: {}", callee.name);
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
    fn codegen_block(&mut self, block: &IrBlock) -> Option<BasicValueEnum<'ctx>> {
        let mut last: Option<BasicValueEnum<'ctx>> = None;
        for stmt in &block.statements {
            match stmt {
                IrStmt::Expr(expr) => last = Some(self.codegen_expr(expr).as_basic_value_enum()),
                IrStmt::ValDef(val_def) => {
                    let typ = self.codegen_type(val_def.ir_type);
                    let ptr =
                        self.builder.build_alloca(typ, &format!("val_{}", val_def.variable_id));
                    let value = self.codegen_val(val_def);
                    self.builder.build_store(ptr, value);
                    self.pointers.insert(val_def.variable_id, ptr);
                    last = Some(ptr.as_basic_value_enum())
                }
                IrStmt::ReturnStmt(return_stmt) => {
                    let value = self.codegen_expr(&return_stmt.expr);
                    self.builder.build_return(Some(&value));
                    return None;
                }
                IrStmt::Assignment(assignment) => {
                    let variable_id = &assignment.destination_variable;
                    let destination_ptr =
                        *self.pointers.get(variable_id).expect("Missing variable");
                    let initializer = self.codegen_expr(&assignment.value);
                    let store = self.builder.build_store(destination_ptr, initializer);
                    let unit = self.builtin_types.unit.const_zero();
                    last = Some(unit.as_basic_value_enum())
                }
            }
        }
        last
    }
    pub fn codegen_module(&mut self) {
        for constant in &self.module.constants {
            let variable = self.module.get_variable(constant.variable_id);
            match constant.expr {
                IrExpr::Literal(IrLiteral::Int(i64, _)) => {
                    let llvm_ty = self.builtin_types.i64;
                    let llvm_val = self.llvm_module.add_global(
                        llvm_ty,
                        Some(AddressSpace::default()),
                        &variable.name,
                    );
                    llvm_val.set_constant(true);
                    llvm_val.set_initializer(&llvm_ty.const_int(i64 as u64, false));
                    self.globals.insert(constant.variable_id, llvm_val);
                }
                _ => todo!("constant must be int"),
            }
        }
        for function in &self.module.clone().functions {
            if function.is_intrinsic {
                continue;
            }
            let param_types: Vec<BasicMetadataTypeEnum> = function
                .params
                .iter()
                .map(|fn_arg| self.eval_basic_metadata_ty(fn_arg.ir_type))
                .collect();
            let ret_type = self.eval_basic_metadata_ty(function.ret_type);
            let fn_ty = match ret_type {
                BasicMetadataTypeEnum::IntType(i) => i.fn_type(&param_types, false),
                BasicMetadataTypeEnum::PointerType(p) => p.fn_type(&param_types, false),
                _ => panic!("Unexpected function llvm type"),
            };
            let fn_val = self.llvm_module.add_function(&function.name, fn_ty, None);
            let entry_block = self.ctx.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry_block);
            for (i, param) in fn_val.get_param_iter().enumerate() {
                let ir_param = &function.params[i];
                param.set_name(&ir_param.name);
                let ty = self.codegen_type(ir_param.ir_type);
                let ptr = self.builder.build_alloca(ty, &ir_param.name);
                self.builder.build_store(ptr, param);
                self.pointers.insert(ir_param.variable_id, ptr);
            }
            self.codegen_block(&function.block);
        }
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    pub fn optimize(&mut self) -> CodegenResult<()> {
        Target::initialize_aarch64(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        // let triple = TargetTriple::create("arm64-apple-darwin");
        let target = Target::from_triple(&triple).unwrap();
        self.llvm_module.verify().map_err(|err| {
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
        module_pass_manager.add_verifier_pass();
        module_pass_manager.add_promote_memory_to_register_pass();
        module_pass_manager.add_function_attrs_pass();
        // module_pass_manager.add_instruction_combining_pass();
        module_pass_manager.add_function_inlining_pass();

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
