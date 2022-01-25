use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LLVMModule;
use inkwell::targets::{InitializationConfig, Target, TargetTriple};
use inkwell::types::{IntType, StringRadix};
use inkwell::values::{BasicValue, IntValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::TypeExpression::Primitive;
use crate::ast::{ConstVal, Definition, Expression, Literal, Module, TypeExpression, TypePrimitive};

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
        CodeGen { context: ctx, builder: builder }
    }
    fn get_ty(&self, expr: &TypeExpression) -> IntType<'ctx> {
        match expr {
            Primitive(TypePrimitive::Int) => {
                let i32_typ = self.context.i32_type();
                i32_typ
            }
        }
    }
    pub fn codegen_module(&mut self, ast: &Module) -> String {
        let module = self.context.create_module(&ast.name.0);
        for definition in &ast.defs {
            match definition {
                Definition::Const(ConstVal { name, typ, value }) => {
                    if let Expression::Literal(Literal::Numeric(const_ident)) = value {
                        let llvm_ty = self.get_ty(typ);
                        let llvm_val = module.add_global(llvm_ty, Some(AddressSpace::Const), &name.0);
                        llvm_val.set_constant(true);
                        llvm_val.set_initializer(
                            &llvm_ty.const_int_from_string(&const_ident.0, StringRadix::Decimal).unwrap(),
                        );
                    } else {
                        panic!("Const must be a literal expression")
                    }
                }
                Definition::FnDef(fn_def) => {}
            }
        }
        module.print_to_string().to_string()
    }
}
