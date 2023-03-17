use crate::ir::{IRGen, IRType, IrExpr, IrItem, IrNode};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LLVMModule};
use inkwell::targets::{InitializationConfig, Target, TargetTriple};
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, StringRadix};
use inkwell::values::{ArrayValue, BasicMetadataValueEnum, BasicValue, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::fmt::format;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
}

pub fn init_context() -> Context {
    Context::create()
}

impl<'ctx> CodeGen<'ctx> {
    pub fn create(ctx: &'ctx Context) -> CodeGen {
        let builder = ctx.create_builder();
        CodeGen { context: ctx, builder }
    }
    fn get_basic_metadata_ty(&self, typ: &IRType) -> BasicMetadataTypeEnum {
        match typ {
            IRType::Unit => BasicMetadataTypeEnum::MetadataType(),
            IRType::Int => BasicMetadataTypeEnum::IntType(self.context.i32_type()),
            IRType::String => unimplemented!("Codegen for String"),
            IRType::Unset => unreachable!("Unset should not reach codegen"),
        }
    }
    fn int_array_from_str(&self, value: &str) -> ArrayValue {
        let chars = value.bytes().map(|b| self.context.i8_type().const_int(b as u64, false)).collect::<Vec<IntValue>>();
        self.context.i8_type().const_array(&chars)
    }
    fn allocate_val(&self, val: &ValDef) -> inkwell::values::PointerValue {
        match val.typ {
            None => {
                panic!("val def needs a type by codegen");
            }
            Some(TypeExpression::Primitive(TypePrimitive::Int)) => {
                let ptr = self.builder.build_alloca(self.context.i32_type(), &val.name.0);
                if let Expression::Literal(Literal::Numeric(s)) = &val.value {
                    if let Some(v) = self.context.i32_type().const_int_from_string(&s, StringRadix::Decimal) {
                        self.builder.build_store(ptr, v);
                    }
                }
                ptr
            }
        }
    }
    pub fn codegen_module(&mut self, ir: &IRGen) -> String {
        let module = self.context.create_module(&ir.ast.name.0);
        let format_str = {
            let global = module.add_global(self.context.i8_type().array_type(2), None, "formatString");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&self.int_array_from_str("%s"));
            global
        };
        let mut nodes = &ir.nodes.iter();
        while let Some(node) = nodes.next() {
            match node {
                IrNode::Expr(IrExpr::Int(value)) => {
                    let llvm_ty = self.context.i32_type();
                    let llvm_val = module.add_global(llvm_ty, Some(AddressSpace::Const), "const_int");
                    llvm_val.set_constant(true);
                    llvm_val.set_initializer(&llvm_ty.const_int(*value, false));
                }
                IrNode::Item(IrItem::Func { name, ret_type, body, params_len }) => {
                    let mut params: Vec<BasicMetadataTypeEnum> = vec![];
                    for i in 0..*params_len {
                        if let IrNode::Item(IrItem::FuncParam { name, ir_type }) = nodes.next().unwrap() {
                            params.push(self.get_basic_metadata_ty(ir_type))
                        }
                    }
                    fn_def.args.iter().map(|fn_arg| self.get_basic_metadata_ty(&fn_arg.typ)).collect();
                    let fn_ty = match fn_def.ret_type {
                        // TODO: Inferring 'void' should be part of lowering
                        None => self.context.void_type().fn_type(&params, false),
                        Some(TypeExpression::Primitive(TypePrimitive::Int)) => {
                            self.context.i32_type().fn_type(&params, false)
                        }
                    };
                    let fn_val = module.add_function(&fn_def.name.0, fn_ty, None);
                    fn_val.get_param_iter().enumerate().for_each(|(i, arg)| arg.set_name(&fn_def.args[i].name.0));
                    let entry_block = self.context.append_basic_block(fn_val, "entry");
                    if let Some(block) = &fn_def.block {
                        self.builder.position_at_end(entry_block);
                        let mut ptrs: Vec<inkwell::values::PointerValue> = Vec::new();
                        for stmt in &block.stmts {
                            if let BlockStmt::ValDef(val_def) = stmt {
                                ptrs.push(self.allocate_val(val_def));
                            }
                            if let BlockStmt::ReturnStmt(Expression::Variable(ident)) = stmt {
                                // let ptr = self.builder.build
                                let r = fn_val.get_first_param().unwrap();
                                let ptr = ptrs.iter().find(|p| p.get_name().to_str().unwrap() == ident.0).unwrap();
                                let loaded = self.builder.build_load(*ptr, "ret");
                                self.builder.build_return(Some(&loaded));
                            }
                            if let BlockStmt::LoneExpression(Expression::FnCall(fn_call)) = stmt {
                                if &fn_call.name.0 == "println" {
                                    let c_str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                                    let printf_type = self.context.i32_type().fn_type(&[c_str_type.into()], true);
                                    let printf_fn = module.add_function("printf", printf_type, Some(Linkage::External));
                                    let first_arg =
                                        if let Expression::Literal(Literal::Numeric(num)) = &fn_call.args[0].value {
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
                                    // let value = unsafe { self.builder.build_in_bounds_gep(arg1_ptr, &[zero], "idk") };
                                    let value = self.builder.build_bitcast(arg1_ptr, c_str_type, "value");
                                    let format_str_ptr = self.builder.build_bitcast(
                                        format_str.as_pointer_value(),
                                        c_str_type,
                                        "fmt_str",
                                    );
                                    self.builder.build_call(
                                        printf_fn,
                                        &[format_str_ptr.into(), value.into()],
                                        "printf",
                                    );
                                }
                            }
                            // self.builder.build_call()
                        }
                    } else {
                        self.builder.build_return(None);
                    }
                }
            }
        }
        module.print_to_string().to_string()
    }
}
