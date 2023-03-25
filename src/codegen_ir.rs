use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LLVMModule};
use inkwell::targets::{InitializationConfig, Target, TargetTriple};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType,
    StringRadix, StructType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, InstructionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::fmt::format;

use crate::ir::*;

struct BuiltinTypes<'ctx> {
    i64: IntType<'ctx>,
    unit: IntType<'ctx>,
    boolean: IntType<'ctx>,
    char: IntType<'ctx>,
    string: StructType<'ctx>,
}

pub struct CodeGen<'ast, 'ctx> {
    module: &'ctx IrModule<'ast>,
    context: &'ctx Context,
    builder: Builder<'ctx>,
    // TODO: should be by scope or something
    pointers: HashMap<VariableId, PointerValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
}

pub fn init_context() -> Context {
    Context::create()
}

impl<'ast, 'ctx> CodeGen<'ast, 'ctx> {
    pub fn create(ctx: &'ctx Context, module: &'ctx IrModule<'ast>) -> CodeGen<'ast, 'ctx> {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let pointers = HashMap::new();
        CodeGen {
            module,
            context: ctx,
            builder,
            pointers,
            builtin_types: BuiltinTypes {
                i64: ctx.i64_type(),
                unit: ctx.bool_type(),
                boolean: ctx.bool_type(),
                char: char_type,
                string: ctx.struct_type(
                    &[
                        BasicTypeEnum::IntType(ctx.i64_type()),
                        BasicTypeEnum::PointerType(char_type.ptr_type(AddressSpace::Generic)),
                    ],
                    false,
                ),
            },
        }
    }
    fn eval_type(&self, expr: IrType) -> BasicTypeEnum {
        match expr {
            IrType::Int => self.builtin_types.i64.as_basic_type_enum(),
            IrType::String => self.builtin_types.string.as_basic_type_enum(),
            IrType::Unit => self.builtin_types.unit.as_basic_type_enum(),
        }
    }
    fn eval_basic_metadata_ty(&self, expr: IrType) -> BasicMetadataTypeEnum {
        match expr {
            IrType::Int => BasicMetadataTypeEnum::IntType(self.context.i64_type()),
            IrType::String => BasicMetadataTypeEnum::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic),
            ),
            // TODO: Representing Unit as just the boolean false for now, maybe some const ptr is
            // better, idk
            IrType::Unit => BasicMetadataTypeEnum::IntType(self.context.bool_type()),
        }
    }
    fn i8_array_from_str(&self, value: &str) -> ArrayValue {
        let chars = value
            .bytes()
            .map(|b| self.context.i8_type().const_int(b as u64, false))
            .collect::<Vec<IntValue>>();
        self.context.i8_type().const_array(&chars)
    }
    fn codegen_val(&self, val: &ValDef) -> inkwell::values::PointerValue {
        // We alloca everything with no guilt because even clang does that apparently
        let typ = self.eval_type(val.ir_type);
        let ptr = self.builder.build_alloca(typ, &val.variable_id.to_string());
        let value = self.codegen_expr(val.initializer);
        self.builder.build_store(ptr, value);
        ptr
    }
    fn codegen_expr(&self, expr: &IrExpr) -> inkwell::values::PointerValue {
        match expr {
            IrExpr::Int(int_value) => {
                let ptr = self.builder.build_alloca(self.builtin_types.i64, "");
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                self.builder
                    .build_store(ptr, self.builtin_types.i64.const_int(int_value as u64, false));
                ptr
            }
            IrExpr::Variable(ir_var) => self
                .pointers
                .get(&ir_var.variable_id)
                .expect(&format!("Pointer for variable {} not found", ir_var.variable_id))
                .clone(),
            IrExpr::BinaryOp(bin_op) => {
                let lhs_ptr = self.codegen_expr(&bin_op.lhs);
                let rhs_ptr = self.codegen_expr(&bin_op.rhs);
                match bin_op.kind {
                    BinaryOpKind::Add => {
                        let lhs_loaded =
                            self.builder.build_load(lhs_ptr, "load_lhs").into_int_value();
                        let rhs_loaded =
                            self.builder.build_load(rhs_ptr, "load_rhs").into_int_value();
                        let add_res = self.builder.build_int_add(lhs_loaded, rhs_loaded, "add");
                        let add_res_ptr = self.builder.build_alloca(
                            self.eval_type(bin_op.ir_type).as_basic_type_enum(),
                            "add_res_ptr",
                        );
                        add_res_ptr
                    }
                }
            }
            IrExpr::FunctionCall(call) => {
                // TODO: This is our first 'intrinsic'
                // Support intrinsics properly in the IR
                if &call.name.0 == "println" {
                    let c_str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                    let printf_type = self.context.i32_type().fn_type(&[c_str_type.into()], true);
                    let printf_fn =
                        module.add_function("printf", printf_type, Some(Linkage::External));
                    let first_arg = if let Expression::Literal(Literal::Numeric(num)) =
                        &fn_call.args[0].value
                    {
                        num.as_ref()
                    } else {
                        ""
                    };
                    let arg1_type = self.context.i8_type().array_type(first_arg.len() as u32);
                    let arg1_chars = first_arg
                        .bytes()
                        .map(|b| self.context.i8_type().const_int(b as u64, false))
                        .collect::<Vec<IntValue>>();
                    let arg1_ptr = self.builder.build_alloca(arg1_type, "arg1");
                    let arg1_value = self.context.i8_type().const_array(&arg1_chars);
                    self.builder.build_store(arg1_ptr, arg1_value);
                    let zero = self.context.i32_type().const_zero();
                    let value = self.builder.build_bitcast(arg1_ptr, c_str_type, "value");
                    let format_str_ptr = self.builder.build_bitcast(
                        format_str.as_pointer_value(),
                        c_str_type,
                        "fmt_str",
                    );
                    let call = self
                        .builder
                        .build_call(printf_fn, &[format_str_ptr.into(), value.into()], "printf")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    call.set_name("println_res_HELLOWORLD");
                }
            }
            _ => todo!("codegen expr"),
        }
    }
    fn codegen_block(&mut self, block: &IrBlock) -> BasicBlock {
        let entry_block = self.context.append_basic_block(fn_val, "entry");
        // TODO: Factor out codegen block
        self.builder.position_at_end(entry_block);
        let mut ptrs: Vec<inkwell::values::PointerValue> = Vec::new();
        for stmt in &block.statements {
            match stmt {
                IrStmt::Expr(expr) => {
                    self.codegen_expr(expr);
                }
                IrStmt::ValDef(val_def) => {
                    ptrs.push(self.codegen_val(val_def));
                }
                IrStmt::ReturnStmt(return_stmt) => {
                    self.builder.build_return(None);
                }
                IrStmt::Assignment(_) => todo!(),
            }
        }
    }
    pub fn codegen_module<'a>(&mut self, ast: &IrModule<'a>) -> String {
        let module = self.context.create_module(&ast.name.0);
        let format_str = {
            let global =
                module.add_global(self.context.i8_type().array_type(2), None, "formatString");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&self.i8_array_from_str("%s"));
            global
        };
        for constant in &ast.constants {
            let llvm_ty = self.context.i32_type();
            let llvm_val = module.add_global(llvm_ty, Some(AddressSpace::Const), &ast.name.0);
            llvm_val.set_constant(true);
            llvm_val.set_initializer(
                &llvm_ty.const_int_from_string("fixme", StringRadix::Decimal).unwrap(),
            );
        }
        for function in &ast.functions {
            let param_types: Vec<BasicMetadataTypeEnum> = function
                .params
                .iter()
                .map(|fn_arg| self.eval_basic_metadata_ty(&fn_arg.typ))
                .collect();
            let ret_type = self.eval_basic_metadata_ty(function.ret_type);
            let fn_ty = match ret_type {
                BasicMetadataTypeEnum::IntType(i) => i.fn_type(&param_types, false),
                BasicMetadataTypeEnum::PointerType(p) => p.fn_type(&param_types, false),
                _ => panic!("Unexpected function llvm type"),
            };
            let fn_val = module.add_function(&function.name, fn_ty, None);
            fn_val
                .get_param_iter()
                .enumerate()
                .for_each(|(i, arg)| arg.set_name(&function.params[i].name));
            let block = function.block;
            let entry_block = self.codegen_block(&function.block);
        }
        module.print_to_string().to_string()
    }
}
