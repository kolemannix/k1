use anyhow::Result;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LLVMModule};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType, StringRadix, StructType,
};
use inkwell::values::{
    AnyValueEnum, ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue,
    FunctionValue, GlobalValue, InstructionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::path::Path;

use crate::ir::*;

struct BuiltinTypes<'ctx> {
    i64: IntType<'ctx>,
    unit: IntType<'ctx>,
    boolean: IntType<'ctx>,
    char: IntType<'ctx>,
    c_str: PointerType<'ctx>,
    string: StructType<'ctx>,
}

struct BuiltinFunctions<'ctx> {
    printf: FunctionValue<'ctx>,
}

pub struct Codegen<'ast, 'ctx> {
    ctx: &'ctx Context,
    module: &'ctx IrModule<'ast>,
    llvm_module: inkwell::module::Module<'ctx>,
    builder: Builder<'ctx>,
    llvm_functions: HashMap<FunctionId, FunctionValue<'ctx>>,
    // TODO: pointers should be by scope or something
    pointers: HashMap<VariableId, PointerValue<'ctx>>,
    globals: HashMap<VariableId, GlobalValue<'ctx>>,
    builtin_functions: BuiltinFunctions<'ctx>,
    builtin_globals: HashMap<String, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
}

pub fn init_context() -> Context {
    Context::create()
}

type CodegenResult<T> = anyhow::Result<T>;

fn i8_array_from_str<'ctx>(ctx: &'ctx Context, value: &str) -> ArrayValue<'ctx> {
    let chars =
        value.bytes().map(|b| ctx.i8_type().const_int(b as u64, false)).collect::<Vec<IntValue>>();
    ctx.i8_type().const_array(&chars)
}

impl<'ast, 'ctx> Codegen<'ast, 'ctx> {
    pub fn create(ctx: &'ctx Context, module: &'ctx IrModule<'ast>) -> Codegen<'ast, 'ctx> {
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
            boolean: ctx.bool_type(),
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
            builder,
            pointers,
            globals,
            llvm_functions: HashMap::new(),
            builtin_functions: BuiltinFunctions { printf },
            builtin_globals,
            builtin_types,
        }
    }

    fn loaded_int_value_from_enum(&mut self, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match value {
            BasicValueEnum::IntValue(int_value) => int_value,
            BasicValueEnum::PointerValue(int_ptr) => {
                let loaded = self.builder.build_load(int_ptr, "binop_lhs");
                loaded.into_int_value()
            }
            other => {
                panic!("Could not coerce {other:?} value to Int")
            }
        }
    }

    fn build_printf_call(&mut self, call: &FunctionCall) -> BasicValueEnum<'ctx> {
        // Assume the arg is an int since that's what the intrinsic typechecks for
        let first_arg = self.codegen_expr(&call.args[0]);
        // Coerce the arg to an int ptr
        let first_arg_loaded = self.loaded_int_value_from_enum(first_arg);
        dbg!(first_arg_loaded);

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
                &[format_str_ptr.into(), first_arg_loaded.into()],
                "printf",
            )
            .try_as_basic_value()
            .left()
            .unwrap();
        call.set_name("println_res");
        call
    }
    fn eval_type(&self, expr: IrType) -> BasicTypeEnum<'ctx> {
        match expr {
            IrType::Int => self.builtin_types.i64.as_basic_type_enum(),
            IrType::Bool => self.builtin_types.boolean.as_basic_type_enum(),
            IrType::String => self.builtin_types.string.as_basic_type_enum(),
            IrType::Unit => self.builtin_types.unit.as_basic_type_enum(),
        }
    }
    fn eval_basic_metadata_ty(&self, ir_type: IrType) -> BasicMetadataTypeEnum<'ctx> {
        self.eval_type(ir_type).as_basic_type_enum().into()
    }
    fn codegen_val(&mut self, val: &ValDef) -> inkwell::values::BasicValueEnum<'ctx> {
        let value = self.codegen_expr(&val.initializer);
        value
    }
    // We alloca everything with no guilt because even clang does that apparently
    fn codegen_expr(&mut self, expr: &IrExpr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            IrExpr::Str(_) => {
                todo!("Strings!")
            }
            IrExpr::Int(int_value) => {
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                let value = self.builtin_types.i64.const_int(*int_value as u64, false);
                value.as_basic_value_enum()
            }
            IrExpr::Bool(b) => {
                let value = self.builtin_types.boolean.const_int(*b as u64, true);
                value.as_basic_value_enum()
            }
            IrExpr::Variable(ir_var) => {
                if let Some(ptr) = self.pointers.get(&ir_var.variable_id) {
                    ptr.as_basic_value_enum()
                } else if let Some(global) = self.globals.get(&ir_var.variable_id) {
                    let value = global.get_initializer().unwrap();
                    value
                } else {
                    panic!("No pointer or global found for variable {:?}", ir_var)
                }
            }
            IrExpr::BinaryOp(bin_op) => match bin_op.kind {
                BinaryOpKind::Add => {
                    let lhs_value = self.codegen_expr(&bin_op.lhs);
                    let rhs_value = self.codegen_expr(&bin_op.rhs);
                    let lhs_int = self.loaded_int_value_from_enum(lhs_value);
                    let rhs_int = self.loaded_int_value_from_enum(rhs_value);
                    let add_res = self.builder.build_int_add(lhs_int, rhs_int, "add");
                    add_res.as_basic_value_enum()
                }
                BinaryOpKind::Multiply => {
                    let lhs_value = self.codegen_expr(&bin_op.lhs);
                    let rhs_value = self.codegen_expr(&bin_op.rhs);
                    let lhs_int = self.loaded_int_value_from_enum(lhs_value);
                    let rhs_int = self.loaded_int_value_from_enum(rhs_value);
                    let mul_res = self.builder.build_int_mul(lhs_int, rhs_int, "mul");
                    mul_res.as_basic_value_enum()
                }
            },
            IrExpr::Block(block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and assign the return value to an alloca
                todo!("codegen block")
            }
            IrExpr::FunctionCall(call) => {
                let callee = self.module.get_function(call.callee_function_id);
                if !&callee.is_intrinsic {
                    let function_value: FunctionValue = self
                        .llvm_module
                        .get_function(&callee.name)
                        .expect(&format!("LLVM function not found: {}", callee.name));
                    let args: Vec<BasicMetadataValueEnum<'ctx>> = call
                        .args
                        .iter()
                        .map(|arg_expr| {
                            let basic_value = self.codegen_expr(arg_expr);
                            BasicMetadataValueEnum::from(basic_value)
                        })
                        .collect();
                    let callsite_value =
                        self.builder.build_call(function_value, &args, &callee.name);
                    // Call return Right for void, and Left for values
                    let result_value =
                        callsite_value.try_as_basic_value().left().expect("function returned void");
                    result_value
                } else if callee.name == "println" {
                    // TODO: This is our first 'intrinsic'
                    // Support intrinsics properly in the IR
                    self.build_printf_call(call);
                    println!("println call returned {:?}", call);
                    let ptr = self.builder.build_alloca(self.builtin_types.unit, "println_res_ptr");
                    self.builder.build_store(ptr, self.builtin_types.unit.const_int(0, false));
                    self.builtin_types.unit.const_int(0, false).as_basic_value_enum()
                } else {
                    todo!("Unexpected instrinic: {}", callee.name);
                }
            }
        }
    }
    fn codegen_block(&mut self, block: &IrBlock) {
        for stmt in &block.statements {
            match stmt {
                IrStmt::Expr(expr) => {
                    self.codegen_expr(expr);
                }
                IrStmt::ValDef(val_def) => {
                    let typ = self.eval_type(val_def.ir_type);
                    let ptr =
                        self.builder.build_alloca(typ, &format!("val_{}", val_def.variable_id));
                    let value = self.codegen_val(val_def);
                    self.builder.build_store(ptr, value);
                    self.pointers.insert(val_def.variable_id, ptr);
                }
                IrStmt::ReturnStmt(return_stmt) => {
                    let value = self.codegen_expr(&return_stmt.expr);
                    self.builder.build_return(Some(&value));
                }
                IrStmt::Assignment(_) => todo!("assignment codegen"),
            }
        }
    }
    pub fn codegen_module(&mut self) {
        for constant in &self.module.constants {
            let variable = self.module.get_variable(constant.variable_id);
            match constant.expr {
                IrExpr::Int(i64) => {
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
                _ => panic!("constant must be int"),
            }
        }
        for function in &self.module.functions {
            println!("codegen for Function {}", function.name);
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
                let ptr =
                    self.builder.build_alloca(self.eval_type(ir_param.ir_type), &ir_param.name);
                self.builder.build_store(ptr, param);
                self.pointers.insert(ir_param.variable_id, ptr);
            }
            self.codegen_block(&function.block);
        }
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    pub fn optimize(&self) -> Result<(), Box<dyn std::error::Error>> {
        Target::initialize_aarch64(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        // let triple = TargetTriple::create("arm64-apple-darwin");
        println!("Using triple: {}", triple);
        let target = Target::from_triple(&triple)?;
        self.llvm_module.verify()?;
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
        module_pass_manager.add_instruction_combining_pass();
        module_pass_manager.add_function_inlining_pass();

        module_pass_manager.run_on(&self.llvm_module);

        machine.add_analysis_passes(&module_pass_manager);
        let filename = format!("{}.o", self.name());
        println!("Outputting object file to {filename}");
        self.llvm_module.set_data_layout(&machine.get_target_data().get_data_layout());
        self.llvm_module.set_triple(&triple);
        machine.write_to_file(
            &self.llvm_module,
            inkwell::targets::FileType::Object,
            Path::new(&filename),
        )?;
        Ok(())
    }

    pub fn output_llvm_ir_text(&self) -> String {
        self.llvm_module.print_to_string().to_string()
    }
}
