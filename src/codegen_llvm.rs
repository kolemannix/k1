use anyhow::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;

use inkwell::module::Linkage as LlvmLinkage;
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
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use crate::parse::IdentifierId;
use crate::typer::{self, *};

struct BuiltinTypes<'ctx> {
    ctx: &'ctx Context,
    none: IntType<'ctx>,
    i64: IntType<'ctx>,
    unit: IntType<'ctx>,
    unit_value: IntValue<'ctx>,
    boolean: IntType<'ctx>,
    true_value: IntValue<'ctx>,
    false_value: IntValue<'ctx>,
    char: IntType<'ctx>,
    c_str: PointerType<'ctx>,
    any_ptr: PointerType<'ctx>,
    any_value: BasicTypeEnum<'ctx>,
    string_struct: StructType<'ctx>,
}

impl<'ctx> BuiltinTypes<'ctx> {
    fn array_struct(&self, elem_type: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        // NOTE: All the 'length' types could probably be i32 but our language int type is only i64
        //       so we couldn't enforce that at the lang level right now
        let length_type = self.ctx.i64_type().as_basic_type_enum();
        let data_type = elem_type.ptr_type(AddressSpace::default()).as_basic_type_enum();
        self.ctx.struct_type(&[length_type, data_type], false)
    }
    fn array_length_loaded(
        &self,
        builder: &Builder<'ctx>,
        array_ptr: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        // The element type doesn't matter we just want to gep to the length field
        let length_ptr = builder
            .build_struct_gep(self.array_struct(self.any_value), array_ptr, 0, "array_length_ptr")
            .unwrap();
        let length = builder.build_load(self.i64, length_ptr, "length_value");
        length.into_int_value()
    }
    fn array_data_ptr(
        &self,
        builder: &Builder<'ctx>,
        array_pointer: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let data_ptr_ptr = builder
            .build_struct_gep(
                self.array_struct(self.any_value),
                array_pointer,
                1,
                "array_data_ptr_ptr",
            )
            .unwrap();
        let data_ptr = builder.build_load(self.any_ptr, data_ptr_ptr, "array_data_ptr");
        data_ptr.into_pointer_value()
    }

    fn string_length_loaded(
        &self,
        builder: &Builder<'ctx>,
        string_pointer: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        let length_ptr = builder
            .build_struct_gep(self.string_struct, string_pointer, 0, "string_length_ptr")
            .unwrap();
        let length = builder.build_load(self.i64, length_ptr, "length_value");
        length.into_int_value()
    }
    fn string_data_ptr(
        &self,
        builder: &Builder<'ctx>,
        string_pointer: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let data_ptr_ptr = builder
            .build_struct_gep(self.string_struct, string_pointer, 1, "string_data_ptr_ptr")
            .unwrap();
        let data_ptr = builder.build_load(self.c_str, data_ptr_ptr, "string_data_ptr");
        data_ptr.into_pointer_value()
    }
}

struct LibcFunctions<'ctx> {
    printf: FunctionValue<'ctx>,
    exit: FunctionValue<'ctx>,
}

pub struct Codegen<'ctx> {
    ctx: &'ctx Context,
    pub module: Rc<TypedModule>,
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
}

#[derive(Copy, Clone, Debug)]
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

/// When we codegen an expression, sometimes we produce a pointer that may
/// be needed by the caller (for example, for assignment). In these
/// cases we return Pointer, indicating that you've got a pointer
/// to whatever the IR type indicates you should have.
/// Example: { a: 1} is a record, which we model as ptr, but since we
/// are returning the actual ptr to the record, not a pointer to it, we
/// use Value.
/// Example: [1,2,3][0]. We return a pointer to the array at that location
/// in case this is for assignment. The types expect an int, but we gave a Pointer,
/// so we return it in Pointer to indicate that
#[derive(Copy, Clone, Debug)]
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

    fn expect_value(&self) -> BasicValueEnum<'ctx> {
        match self {
            GeneratedValue::Value(value) => *value,
            GeneratedValue::Pointer(pointer) => {
                panic!("Expected Value but got Pointer on {:?}", pointer)
            }
        }
    }
    fn expect_pointer(&self) -> Pointer<'ctx> {
        match self {
            GeneratedValue::Value(value) => panic!("Expected Pointer but got Value on {}", value),
            GeneratedValue::Pointer(pointer) => *pointer,
        }
    }
}

type CodegenResult<T> = Result<T>;

fn i8_array_from_str<'ctx>(ctx: &'ctx Context, value: &str) -> ArrayValue<'ctx> {
    let chars =
        value.bytes().map(|b| ctx.i8_type().const_int(b as u64, false)).collect::<Vec<IntValue>>();
    ctx.i8_type().const_array(&chars)
}

impl<'ctx> Codegen<'ctx> {
    pub fn create(ctx: &'ctx Context, module: Rc<TypedModule>) -> Codegen<'ctx> {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let llvm_module = ctx.create_module(&module.ast.name);
        // Example of linking an LLVM module
        // let stdlib_module = ctx
        //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("nxlib/llvm")).unwrap())
        //     .unwrap();
        // llvm_module.link_in_module(stdlib_module).unwrap();
        let pointers = HashMap::new();
        let format_int_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(4), None, "formatInt");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%i\n\0"));
            global
        };
        let format_str_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(6), None, "formatString");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%.*s\n\0"));
            global
        };
        let globals = HashMap::new();
        let mut builtin_globals: HashMap<String, GlobalValue<'ctx>> = HashMap::new();
        builtin_globals.insert("formatInt".to_string(), format_int_str);
        builtin_globals.insert("formatString".to_string(), format_str_str);
        let string_struct = ctx.struct_type(
            &[
                ctx.i64_type().as_basic_type_enum(),
                char_type.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            ],
            false,
        );

        let builtin_types = BuiltinTypes {
            ctx,
            none: ctx.i64_type(),
            i64: ctx.i64_type(),
            unit: ctx.bool_type(),
            unit_value: ctx.bool_type().const_int(0, false),
            boolean: ctx.bool_type(),
            true_value: ctx.bool_type().const_int(1, false),
            false_value: ctx.bool_type().const_int(0, false),
            char: char_type,
            c_str: char_type.ptr_type(AddressSpace::default()),
            any_ptr: ctx.i64_type().ptr_type(AddressSpace::default()),
            any_value: ctx.i64_type().as_basic_type_enum(),
            string_struct,
        };

        let printf_type = ctx.i32_type().fn_type(&[builtin_types.c_str.into()], true);
        let printf = llvm_module.add_function("printf", printf_type, Some(LlvmLinkage::External));
        let exit = llvm_module.add_function(
            "exit",
            ctx.void_type().fn_type(&[builtin_types.i64.into()], false),
            Some(LlvmLinkage::External),
        );
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
            libc_functions: LibcFunctions { printf, exit },
            builtin_globals,
            builtin_types,
            default_address_space: AddressSpace::default(),
        }
    }

    fn get_ident_name(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        self.module.ast.get_ident_str(id)
    }

    fn build_print_int_call(&mut self, int_value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        // TODO: Builtin globals could be a struct not a hashmap because its all static currently
        let format_str_global = self.builtin_globals.get("formatInt").unwrap();
        let call = self
            .builder
            .build_call(
                self.libc_functions.printf,
                &[format_str_global.as_pointer_value().into(), int_value.into()],
                "printf",
            )
            .try_as_basic_value()
            .left()
            .unwrap();
        call.set_name("print_int_res");
        call
    }

    fn build_print_string_call(
        &mut self,
        string_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        trace!("codegen print_string");
        let length = self
            .builtin_types
            .string_length_loaded(&self.builder, string_value.into_pointer_value());
        let data =
            self.builtin_types.string_data_ptr(&self.builder, string_value.into_pointer_value());

        let format_str = self.builtin_globals.get("formatString").unwrap();
        let call = self
            .builder
            .build_call(
                self.libc_functions.printf,
                &[format_str.as_pointer_value().into(), length.into(), data.into()],
                "printf",
            )
            .try_as_basic_value()
            .left()
            .unwrap();
        call.set_name("print_str_res");
        call
    }
    // FIXME: Only needs mut self because of the self.llvm_types cache, which
    //        has questionable value at this point in time
    fn build_record_type(&mut self, record: &RecordDefn) -> StructType<'ctx> {
        let field_types: Vec<_> =
            record.fields.iter().map(|f| self.get_llvm_type(f.type_id)).collect();
        let struct_type = self.ctx.struct_type(&field_types, false);
        // self.llvm_types.insert(type_id, struct_ptr_type.as_basic_type_enum());
        struct_type
    }
    fn build_optional_type(&mut self, optional_type: &OptionalType) -> StructType<'ctx> {
        // Optional representation summary:
        // Pointer-like types (array, record, string) are represented the same way, and
        // we use the null pointer to represent None
        // Scalar types (int, char, bool) are represented as a struct with a boolean
        // flag indicating whether the value is None, and the value itself
        match optional_type.inner_type {
            UNIT_TYPE_ID | CHAR_TYPE_ID | BOOL_TYPE_ID | INT_TYPE_ID => {
                let inner = self.get_llvm_type(optional_type.inner_type);
                // Do I need to think about alignment and packing?
                let llvm_struct = self
                    .ctx
                    .struct_type(&[self.ctx.bool_type().as_basic_type_enum(), inner], false);
                llvm_struct
            }
            type_id => match self.module.get_type(type_id) {
                Type::String => self.builtin_types.string_struct,
                other => {
                    unimplemented!("codegen for optional type {:?}", other);
                }
            },
        }
    }

    fn get_llvm_type(&mut self, type_id: TypeId) -> BasicTypeEnum<'ctx> {
        // Now that I am not relying on inspect the BasicTypeEnum returned by this function,
        // but rather just using the type info from the TAST, I think I can go back to using this
        // method instead of build_record_type. I can just immediately cast to a StructType if
        // I need one for GEP generation
        trace!("codegen for type {}", self.module.type_id_to_string(type_id));
        match type_id {
            UNIT_TYPE_ID => self.builtin_types.unit.as_basic_type_enum(),
            CHAR_TYPE_ID => self.builtin_types.char.as_basic_type_enum(),
            INT_TYPE_ID => self.builtin_types.i64.as_basic_type_enum(),
            BOOL_TYPE_ID => self.builtin_types.boolean.as_basic_type_enum(),
            STRING_TYPE_ID => {
                // Any ptr type will work; need to re-think this whole function
                self.builtin_types.char.ptr_type(self.default_address_space).as_basic_type_enum()
            }
            type_id => {
                match self.llvm_types.get(&type_id) {
                    None => {
                        // Generate and store the type in here
                        let module = self.module.clone();
                        let ty = module.get_type(type_id);
                        match ty {
                            Type::Optional(optional) => {
                                self.build_optional_type(optional).as_basic_type_enum()
                            }
                            Type::Record(record) => {
                                trace!("generating llvm pointer type for record type {type_id}");
                                let field_types: Vec<_> = record
                                    .fields
                                    .iter()
                                    .map(|f| self.get_llvm_type(f.type_id))
                                    .collect();
                                let struct_type = self.ctx.struct_type(&field_types, false);
                                let struct_ptr_type = struct_type
                                    .as_basic_type_enum()
                                    .ptr_type(self.default_address_space);
                                self.llvm_types
                                    .insert(type_id, struct_ptr_type.as_basic_type_enum());
                                struct_ptr_type.as_basic_type_enum()
                            }
                            Type::OpaqueAlias(_alias) => {
                                todo!("opaque alias")
                            }
                            Type::TypeVariable(v) => {
                                panic!("codegen was asked to codegen a type variable {:?}", v)
                            }
                            Type::Array(_array) => {
                                // Does not matter what pointer type we use
                                // let element_type = self.codegen_type_ref(array.element_type);
                                // element_type
                                //     .ptr_type(self.default_address_space)
                                //     .as_basic_type_enum()
                                self.builtin_types
                                    .i64
                                    .ptr_type(self.default_address_space)
                                    .as_basic_type_enum()
                            }
                            other => {
                                panic!(
                                    "get_llvm_type for type dropped through unexpectedly: {:?}",
                                    other
                                )
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
        let pointee_ty = self.get_llvm_type(val.ty);
        let variable = self.module.get_variable(val.variable_id);
        let value_ptr = self.builder.build_alloca(
            pointee_ty.ptr_type(self.default_address_space),
            &self.get_ident_name(variable.name),
        );
        trace!("codegen_val {}: pointee_ty: {pointee_ty:?}", &*self.get_ident_name(variable.name));
        // We're always storing a pointer
        // in self.variables that, when loaded, gives the actual type of the variable
        self.builder.build_store(value_ptr, value.as_basic_value_enum());
        let pointer = Pointer { pointer: value_ptr, pointee_ty };
        self.variables.insert(val.variable_id, pointer);
        pointer.into()
    }

    fn codegen_if_else(&mut self, ir_if: &TypedIf) -> GeneratedValue<'ctx> {
        let typ = self.get_llvm_type(ir_if.ty);
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let consequent_block = self.ctx.append_basic_block(current_fn, "if_cons");
        let alternate_block = self.ctx.append_basic_block(current_fn, "if_alt");
        let merge_block = self.ctx.append_basic_block(current_fn, "if_merge");

        // Entry block
        let condition = self.codegen_expr(&ir_if.condition);
        let condition_value = condition.into_int_value();
        self.builder.build_conditional_branch(condition_value, consequent_block, alternate_block);

        // Consequent Block
        // If any of these blocks have an early return, they'll return None, and we'll panic for
        // now
        self.builder.position_at_end(consequent_block);
        let (consequent_final_block, consequent_expr) =
            match self.codegen_block_statements(&ir_if.consequent) {
                None => {
                    // The consequent block does not return an expression, instead, it returns early
                    // Thus, a PHI makes no sense, and the type of this entire if 'expression' is
                    // Unit, and there's no need for a PHI.
                    (None, None)
                }
                Some(value) => {
                    let consequent_final_block = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(merge_block);
                    (Some(consequent_final_block), Some(value))
                }
            };

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let (alternate_final_block, alternate_expr) =
            match self.codegen_block_statements(&ir_if.alternate) {
                None => (None, None),
                Some(value) => {
                    let alternate_final_block = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(merge_block);
                    (Some(alternate_final_block), Some(value))
                }
            };

        // FIXME: What we are doing here is inferring whether this is an if expression or
        //       an if statement that can return early.
        //       If both sides result in an expression, we do the PHI thing
        //       If either side returns early, we can't, and we return unit
        //
        //       This should all be done and decided earlier during typechecking to make codegen
        //       less branchy and more mechanical.

        // Merge block
        self.builder.position_at_end(merge_block);
        match (consequent_expr, alternate_expr) {
            (Some(consequent_expr), Some(alternate_expr)) => {
                let phi_value = self.builder.build_phi(typ, "if_phi");
                phi_value.add_incoming(&[
                    (&consequent_expr, consequent_final_block.unwrap()),
                    (&alternate_expr, alternate_final_block.unwrap()),
                ]);
                phi_value.as_basic_value().into()
            }
            _ => self.builtin_types.unit_value.as_basic_value_enum().into(),
        }
    }
    fn codegen_expr(&mut self, expr: &TypedExpr) -> BasicValueEnum<'ctx> {
        let value = self.codegen_expr_inner(expr);
        value.loaded_value(&self.builder)
    }
    fn codegen_optional_value(
        &mut self,
        type_id: TypeId,
        optional_some: Option<&OptionalSome>,
    ) -> BasicValueEnum<'ctx> {
        let optional_type_llvm = self.get_llvm_type(type_id);
        let optional_type = self.module.get_type(type_id).expect_optional_type();
        let is_none = optional_some.is_none();
        match optional_type.inner_type {
            UNIT_TYPE_ID | CHAR_TYPE_ID | INT_TYPE_ID | BOOL_TYPE_ID => {
                let struct_type = optional_type_llvm.into_struct_type();
                let discriminator_type = struct_type.get_field_type_at_index(0).unwrap();
                let discriminator_value: BasicValueEnum<'ctx> = if is_none {
                    discriminator_type.const_zero().into()
                } else {
                    discriminator_type.into_int_type().const_int(1, false).into()
                };
                let value_value: BasicValueEnum<'ctx> = if let Some(opt_some) = optional_some {
                    self.codegen_expr(&opt_some.inner_expr)
                } else {
                    struct_type.get_field_type_at_index(1).unwrap().const_zero().into()
                };
                let none_struct = self
                    .builder
                    .build_insert_value(
                        struct_type.get_undef(),
                        discriminator_value,
                        0,
                        &format!("opt_discrim_{}", type_id),
                    )
                    .unwrap();
                let struct_value = self
                    .builder
                    .build_insert_value(
                        none_struct,
                        value_value,
                        1,
                        &format!("opt_value_{}", type_id),
                    )
                    .unwrap();
                let struct_ptr =
                    self.builder.build_alloca(struct_type, &format!("opt_{}", type_id));
                self.builder.build_store(struct_ptr, struct_value);
                struct_ptr.as_basic_value_enum().into()
            }
            STRING_TYPE_ID => {
                if let Some(optional_some) = optional_some {
                    self.codegen_expr(&optional_some.inner_expr)
                } else {
                    let ptr =
                        self.builder.build_alloca(self.builtin_types.string_struct, "none_string");
                    self.builder.build_store(ptr, ptr.get_type().const_null());
                    ptr.as_basic_value_enum().into()
                }
            }
            _ => {
                unimplemented!("codegen for none type {:?}", type_id)
            }
        }
    }
    /// Returns the original reference, even if its one layer of indirection
    /// higher than the expression should give. For example, a pointer to
    /// an array element or record member, rather than the element or member itself
    /// This allows for the caller to decide if they want to just load the value, or use the pointer
    /// for things like assignment
    fn codegen_expr_inner(&mut self, expr: &TypedExpr) -> GeneratedValue<'ctx> {
        log::trace!("codegen expr\n{}", self.module.expr_to_string(expr));
        match expr {
            TypedExpr::None(type_id, _) => self.codegen_optional_value(*type_id, None).into(),
            TypedExpr::OptionalSome(opt_some) => {
                self.codegen_optional_value(opt_some.type_id, Some(opt_some)).into()
            }
            TypedExpr::Unit(_) => self.builtin_types.unit_value.as_basic_value_enum().into(),
            TypedExpr::Char(byte, _) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum().into()
            }
            TypedExpr::Bool(b, _) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum().into(),
                false => self.builtin_types.false_value.as_basic_value_enum().into(),
            },
            TypedExpr::Int(int_value, _) => {
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                let value = self.builtin_types.i64.const_int(*int_value as u64, false);
                value.as_basic_value_enum().into()
            }
            TypedExpr::Str(string_value, _) => {
                // We will make them records (structs) with an array pointer and a length for now
                let global_str = self.llvm_module.add_global(
                    self.builtin_types.char.array_type(string_value.len() as u32),
                    None,
                    "str_data",
                );
                global_str.set_initializer(&i8_array_from_str(self.ctx, string_value));
                global_str.set_constant(true);
                let global_value =
                    self.llvm_module.add_global(self.builtin_types.string_struct, None, "str");
                global_value.set_constant(true);
                let value = self.builtin_types.string_struct.const_named_struct(&[
                    self.builtin_types.i64.const_int(string_value.len() as u64, true).into(),
                    global_str.as_pointer_value().into(),
                ]);
                global_value.set_initializer(&value);
                global_value.as_basic_value_enum().into()
            }
            TypedExpr::Record(record) => {
                let module = self.module.clone();
                let record_type = module.get_type(record.type_id).expect_record_type();
                let record_llvm_type = self.build_record_type(record_type);
                let struct_ptr = self.builder.build_alloca(record_llvm_type, "record");
                for (idx, field) in record.fields.iter().enumerate() {
                    let value = self.codegen_expr(&field.expr);
                    let field_ptr = self
                        .builder
                        .build_struct_gep(
                            record_llvm_type,
                            struct_ptr,
                            idx as u32,
                            &format!("record_init_{}", idx),
                        )
                        .unwrap();
                    // let value_to_store = self.load_if_value_type(value, field.expr.get_type());
                    self.builder.build_store(field_ptr, value);
                }
                GeneratedValue::Value(struct_ptr.as_basic_value_enum())
            }
            TypedExpr::Array(array) => {
                let Type::Array(array_type) = self.module.get_type(array.type_id) else {
                    panic!("expected array type for array");
                };
                let element_type = self.get_llvm_type(array_type.element_type);
                let array_len = self.builtin_types.i64.const_int(array.elements.len() as u64, true);

                let array_ptr = self
                    .make_array(array_len, element_type)
                    .as_basic_value_enum()
                    .into_pointer_value();
                let array_data_ptr = self.builtin_types.array_data_ptr(&self.builder, array_ptr);
                // Store each element
                for (index, element_expr) in array.elements.iter().enumerate() {
                    let value = self.codegen_expr(element_expr);
                    let index_value = self.ctx.i64_type().const_int(index as u64, true);
                    log::trace!("storing element {} of array literal: {:?}", index, element_expr);
                    let ptr_at_index = unsafe {
                        self.builder.build_gep(element_type, array_data_ptr, &[index_value], "elem")
                    };
                    self.builder.build_store(ptr_at_index, value);
                }
                array_ptr.as_basic_value_enum().into()
            }
            TypedExpr::Variable(ir_var) => {
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
            TypedExpr::FieldAccess(field_access) => {
                let result = self.codegen_expr(&field_access.base);
                let type_id = field_access.base.get_type();
                let module = self.module.clone();
                let ty = module.get_type(type_id);
                if let Type::Record(record) = ty {
                    let record_type = self.build_record_type(record);
                    let record_pointer = result.into_pointer_value();
                    let (index, _) = record
                        .find_field(field_access.target_field)
                        .expect("RecordDefn missing field in codegen!");

                    let field_ptr = self
                        .builder
                        .build_struct_gep(
                            record_type,
                            record_pointer,
                            index as u32,
                            "field_access_target_ptr",
                        )
                        .unwrap();
                    let target_ty = self.get_llvm_type(field_access.ty);
                    Pointer { pointee_ty: target_ty, pointer: field_ptr }.into()
                } else {
                    panic!("Invalid field access: {:?}", field_access)
                }
            }
            TypedExpr::If(if_expr) => self.codegen_if_else(if_expr),
            TypedExpr::BinaryOp(bin_op) => match bin_op.ty {
                INT_TYPE_ID => {
                    let lhs_value = self.codegen_expr(&bin_op.lhs).into_int_value();
                    let rhs_value = self.codegen_expr(&bin_op.rhs).into_int_value();
                    let op_res = match bin_op.kind {
                        BinaryOpKind::Add => {
                            self.builder.build_int_add(lhs_value, rhs_value, "add")
                        }
                        BinaryOpKind::Subtract => {
                            self.builder.build_int_sub(lhs_value, rhs_value, "sub")
                        }
                        BinaryOpKind::Multiply => {
                            self.builder.build_int_mul(lhs_value, rhs_value, "mul")
                        }
                        BinaryOpKind::Divide => {
                            self.builder.build_int_signed_div(lhs_value, rhs_value, "sdiv")
                        }
                        BinaryOpKind::And => self.builder.build_and(lhs_value, rhs_value, "and"),
                        BinaryOpKind::Or => self.builder.build_or(lhs_value, rhs_value, "or"),
                        BinaryOpKind::Equals => self.builder.build_int_compare(
                            IntPredicate::EQ,
                            lhs_value,
                            rhs_value,
                            "eq",
                        ),
                        _ => {
                            panic!("Unsupported bin op kind returning int: {}", bin_op.kind)
                        }
                    };
                    op_res.as_basic_value_enum().into()
                }
                BOOL_TYPE_ID => match bin_op.kind {
                    BinaryOpKind::And | BinaryOpKind::Or => {
                        let lhs_int = self.codegen_expr(&bin_op.lhs).into_int_value();
                        let rhs_int = self.codegen_expr(&bin_op.rhs).into_int_value();
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
                    BinaryOpKind::Equals => {
                        // I actually have no idea how I want to handle equality at this point
                        // Obviously we want some sort of value equality on user-defined types
                        // But here for builtin types maybe we just keep assuming everything
                        // is represented as an int value
                        let lhs_int = self.codegen_expr(&bin_op.lhs).into_int_value();
                        let rhs_int = self.codegen_expr(&bin_op.rhs).into_int_value();
                        self.builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                lhs_int,
                                rhs_int,
                                &format!("{}", bin_op.kind),
                            )
                            .as_basic_value_enum()
                            .into()
                    }

                    BinaryOpKind::Less
                    | BinaryOpKind::LessEqual
                    | BinaryOpKind::Greater
                    | BinaryOpKind::GreaterEqual => {
                        let lhs_int = self.codegen_expr(&bin_op.lhs).into_int_value();
                        let rhs_int = self.codegen_expr(&bin_op.rhs).into_int_value();
                        let pred = match bin_op.kind {
                            BinaryOpKind::Equals => IntPredicate::EQ,
                            BinaryOpKind::Less => IntPredicate::SLT,
                            BinaryOpKind::LessEqual => IntPredicate::SLE,
                            BinaryOpKind::Greater => IntPredicate::SGT,
                            BinaryOpKind::GreaterEqual => IntPredicate::SGE,
                            _ => unreachable!("unexpected binop kind"),
                        };
                        self.builder
                            .build_int_compare(pred, lhs_int, rhs_int, &format!("{}", bin_op.kind))
                            .as_basic_value_enum()
                            .into()
                    }
                    other => panic!("Unsupported binary operation {other:?} returning Bool"),
                },
                STRING_TYPE_ID => panic!("No string-returning binary ops yet"),
                UNIT_TYPE_ID => panic!("No unit-returning binary ops"),
                CHAR_TYPE_ID => panic!("No char-returning binary ops"),
                other => todo!("codegen for binary ops on user-defined types: {other}"),
            },
            TypedExpr::UnaryOp(unary_op) => {
                let value = self.codegen_expr(&unary_op.expr);
                let op_res = match unary_op.kind {
                    UnaryOpKind::BooleanNegation => {
                        let value = value.into_int_value();
                        self.builder.build_not(value, "bool_not")
                    }
                };
                op_res.as_basic_value_enum().into()
            }
            TypedExpr::Block(_block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and assign the return value to an alloca
                todo!("codegen lexical block")
            }
            TypedExpr::FunctionCall(call) => self.codegen_function_call(call),
            TypedExpr::ArrayIndex(index_op) => self.codegen_array_index_operation(index_op),
            TypedExpr::StringIndex(index_op) => self.codegen_string_index_operation(index_op),
        }
    }
    fn codegen_function_call(&mut self, call: &Call) -> GeneratedValue<'ctx> {
        let ir_module = self.module.clone();
        let callee = ir_module.get_function(call.callee_function_id);

        let function_value = self.codegen_function(call.callee_function_id, callee);

        let args: Vec<BasicMetadataValueEnum<'ctx>> = call
            .args
            .iter()
            .map(|arg_expr| {
                let basic_value = self.codegen_expr(arg_expr);
                basic_value.into()
                // let expected_type = arg_expr.get_type();
                // self.load_if_value_type(basic_value, expected_type).into()
            })
            .collect();
        let callsite_value = self.builder.build_call(function_value, &args, "call_ret");
        // Call returns Right for void, and Left for values
        let result_value =
            callsite_value.try_as_basic_value().left().expect("function returned void");
        result_value.into()
    }

    fn codegen_string_index_operation(&mut self, operation: &IndexOp) -> GeneratedValue<'ctx> {
        let string_value = self.codegen_expr(&operation.base_expr);
        let index_value = self.codegen_expr(&operation.index_expr);

        // strings are pointers to structs
        let string_value_as_ptr = string_value.as_basic_value_enum().into_pointer_value();
        let data_ptr = self.builtin_types.string_data_ptr(&self.builder, string_value_as_ptr);

        let index_int_value = index_value.into_int_value();
        unsafe {
            let gep_ptr = self.builder.build_gep(
                self.builtin_types.char,
                data_ptr,
                &[index_int_value],
                "string_index_ptr",
            );
            Pointer { pointee_ty: self.builtin_types.char.as_basic_type_enum(), pointer: gep_ptr }
                .into()
        }
    }

    fn codegen_array_index_operation(&mut self, operation: &IndexOp) -> GeneratedValue<'ctx> {
        let pointee_ty = self.get_llvm_type(operation.result_type);
        let array_value = self.codegen_expr(&operation.base_expr);
        let index_value = self.codegen_expr(&operation.index_expr);

        let array_value_as_ptr = array_value.into_pointer_value();
        let index_int_value = index_value.into_int_value();
        // Likely we need one more level of dereferencing
        let array_data = self.builtin_types.array_data_ptr(&self.builder, array_value_as_ptr);
        unsafe {
            let gep_ptr = self.builder.build_gep(
                pointee_ty,
                array_data,
                &[index_int_value],
                "array_index_ptr",
            );
            Pointer { pointee_ty, pointer: gep_ptr }.into()
        }
    }

    fn make_string(&mut self, array_len: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        let string_ptr = self
            .builder
            .build_array_malloc(self.builtin_types.char, array_len, "string_data")
            .unwrap();
        let string_type = self.builtin_types.string_struct;
        let pointer_to_struct = self.builder.build_alloca(string_type, "string");
        // insert_value returns a value and takes a value, doesn't modify memory at pointers
        // We can start building a struct by giving it an undefined struct first
        let string_struct = self
            .builder
            .build_insert_value(string_type.get_undef(), array_len, 0, "string_len")
            .unwrap();
        let string_struct =
            self.builder.build_insert_value(string_struct, string_ptr, 1, "string_data").unwrap();
        self.builder.build_store(pointer_to_struct, string_struct);
        pointer_to_struct.as_basic_value_enum()
    }

    fn make_array(
        &mut self,
        array_len: IntValue<'ctx>,
        element_type: BasicTypeEnum<'ctx>,
    ) -> GeneratedValue<'ctx> {
        // First, we allocate memory somewhere not on the stack
        let array_ptr =
            self.builder.build_array_malloc(element_type, array_len, "array_literal").unwrap();
        let array_type = self.builtin_types.array_struct(element_type);
        let pointer_to_struct = self.builder.build_alloca(array_type, "array_lit");
        // insert_value returns a value and takes a value, doesn't modify memory at pointers
        // We can start building a struct by giving it an undefined struct first
        let array_struct = self
            .builder
            .build_insert_value(array_type.get_undef(), array_len, 0, "array_len")
            .unwrap();
        let array_struct =
            self.builder.build_insert_value(array_struct, array_ptr, 1, "array_data").unwrap();
        self.builder.build_store(pointer_to_struct, array_struct);
        pointer_to_struct.as_basic_value_enum().into()
    }

    fn get_loaded_variable(&mut self, variable_id: VariableId) -> BasicValueEnum<'ctx> {
        self.variables.get(&variable_id).unwrap().loaded_value(&self.builder)
    }

    fn codegen_intrinsic(
        &mut self,
        intrinsic_type: IntrinsicFunctionType,
        function: &Function,
    ) -> GeneratedValue<'ctx> {
        match intrinsic_type {
            IntrinsicFunctionType::Exit => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.builder.build_call(self.libc_functions.exit, &[first_arg.into()], "exit");
                self.builtin_types.unit_value.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::PrintInt => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.build_print_int_call(first_arg);
                self.builtin_types.unit_value.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::PrintString => {
                let string_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.build_print_string_call(string_arg);
                self.builtin_types.unit_value.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::StringLength => {
                let string_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let length = self.builtin_types.string_length_loaded(&self.builder, string_arg);
                length.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::ArrayLength => {
                let array_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let length = self.builtin_types.array_length_loaded(&self.builder, array_arg);
                length.as_basic_value_enum().into()
            }
            IntrinsicFunctionType::ArrayNew => {
                let array_type_id = function.ret_type;
                let array_type = self.module.get_type(array_type_id);
                let element_type = self.get_llvm_type(array_type.expect_array_type().element_type);
                let len = self.get_loaded_variable(function.params[0].variable_id).into_int_value();
                self.make_array(len, element_type)
            }
            IntrinsicFunctionType::StringNew => {
                let array =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let array_len = self.builtin_types.array_length_loaded(&self.builder, array);
                let string = self.make_string(array_len).into_pointer_value();
                let string_data = self.builtin_types.string_data_ptr(&self.builder, string);
                let array_data = self.builtin_types.array_data_ptr(&self.builder, array);
                let _copied =
                    self.builder.build_memcpy(string_data, 1, array_data, 1, array_len).unwrap();
                string.as_basic_value_enum().into()
            }
        }
    }
    // This needs to return either a basic value or an instruction value (in the case of early return)
    // Actually, early return is a big rabbit hole. We need to typecheck it earlier, and probably
    // store it on the block
    //
    // For now, I'm going to return an Option. If the block has an early return, we just return
    // None. We'll fix it when implementing early returns
    // Maybe we rename ReturnStmt to Early Return to separate it from tail returns, which have
    // pretty different semantics and implications for codegen, I am realizing
    fn codegen_block_statements(&mut self, block: &TypedBlock) -> Option<BasicValueEnum<'ctx>> {
        let mut last: Option<BasicValueEnum<'ctx>> = None;
        for stmt in &block.statements {
            match stmt {
                TypedStmt::Expr(expr) => last = Some(self.codegen_expr(expr)),
                TypedStmt::ValDef(val_def) => {
                    let _value = self.codegen_val(val_def).expect_pointer();
                    last = Some(self.builtin_types.unit_value.as_basic_value_enum())
                }
                TypedStmt::Assignment(assignment) => match assignment.destination.deref() {
                    TypedExpr::Variable(v) => {
                        let destination_ptr =
                            *self.variables.get(&v.variable_id).expect("Missing variable");
                        let initializer = self.codegen_expr(&assignment.value);
                        self.builder.build_store(destination_ptr.pointer, initializer);

                        //self.builder.build_store(des, value.loaded_value(&self.builder))
                        last = Some(self.builtin_types.unit_value.as_basic_value_enum())
                    }
                    TypedExpr::FieldAccess(_field_access) => {
                        // We use codegen_expr_inner to get the pointer to the accessed field
                        let field_ptr = self.codegen_expr_inner(&assignment.destination);
                        let ptr = field_ptr.expect_pointer();
                        let rhs = self.codegen_expr(&assignment.value);
                        self.builder.build_store(ptr.pointer, rhs);
                        last = Some(self.builtin_types.unit_value.as_basic_value_enum())
                    }
                    TypedExpr::ArrayIndex(_array_index) => {
                        // We use codegen_expr_inner to get the llvm 'pointer' to the indexed element
                        let elem_ptr = self.codegen_expr_inner(&assignment.destination);
                        let ptr = elem_ptr.expect_pointer();
                        let rhs = self.codegen_expr(&assignment.value);
                        self.builder.build_store(ptr.pointer, rhs);
                        last = Some(self.builtin_types.unit_value.as_basic_value_enum())
                    }
                    _ => {
                        panic!("Invalid assignment lhs")
                    }
                },
                TypedStmt::WhileLoop(while_stmt) => {
                    let start_block = self.builder.get_insert_block().unwrap();
                    let current_fn = start_block.get_parent().unwrap();
                    let loop_entry_block = self.ctx.append_basic_block(current_fn, "while_cond");
                    let loop_body_block = self.ctx.append_basic_block(current_fn, "while_body");
                    let loop_end_block = self.ctx.append_basic_block(current_fn, "while_end");

                    // Go to the loop
                    self.builder.build_unconditional_branch(loop_entry_block);

                    self.builder.position_at_end(loop_entry_block);
                    let cond = self.codegen_expr(&while_stmt.cond).into_int_value();

                    self.builder.build_conditional_branch(cond, loop_body_block, loop_end_block);

                    self.builder.position_at_end(loop_body_block);
                    self.codegen_block_statements(&while_stmt.block);
                    self.builder.build_unconditional_branch(loop_entry_block);

                    self.builder.position_at_end(loop_end_block);
                }
            }
        }
        last
    }

    /// Stores `value` into `pointer`, loading the value pointed to by `value` if necessary
    fn store_loaded(
        &mut self,
        pointer: &Pointer<'ctx>,
        value: GeneratedValue<'ctx>,
    ) -> InstructionValue<'ctx> {
        self.builder.build_store(pointer.pointer, value.loaded_value(&self.builder))
    }

    fn codegen_function(
        &mut self,
        function_id: FunctionId,
        function: &Function,
    ) -> FunctionValue<'ctx> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return *function;
        }
        if function.is_generic() {
            panic!("Cannot codegen generic function: {}", &*self.get_ident_name(function.name));
        }

        let maybe_starting_block = self.builder.get_insert_block();
        let param_types: Vec<BasicMetadataTypeEnum> = function
            .params
            .iter()
            .map(|fn_arg| self.get_llvm_type(fn_arg.type_id).into())
            .collect();
        let ret_type: BasicMetadataTypeEnum<'ctx> = self.get_llvm_type(function.ret_type).into();
        let fn_ty = match ret_type {
            BasicMetadataTypeEnum::IntType(i) => i.fn_type(&param_types, false),
            BasicMetadataTypeEnum::PointerType(p) => p.fn_type(&param_types, false),
            _ => panic!("Unexpected function llvm type"),
        };
        let llvm_linkage = match function.linkage {
            typer::Linkage::Standard => None,
            typer::Linkage::External => Some(LlvmLinkage::External),
            typer::Linkage::Intrinsic => None,
        };
        let fn_val = {
            let name = self.module.ast.get_ident_str(function.name);
            self.llvm_module.add_function(&name, fn_ty, llvm_linkage)
        };

        self.llvm_functions.insert(function_id, fn_val);

        if function.linkage == Linkage::External {
            return fn_val;
        }

        let entry_block = self.ctx.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry_block);
        for (i, param) in fn_val.get_param_iter().enumerate() {
            let ir_param = &function.params[i];
            let ty = self.get_llvm_type(ir_param.type_id);
            let param_name = self.module.ast.get_ident_str(ir_param.name);
            param.set_name(&param_name);
            let pointer = self.builder.build_alloca(ty, &param_name);
            self.builder.build_store(pointer, param);
            self.variables.insert(ir_param.variable_id, Pointer { pointer, pointee_ty: ty });
        }
        match function.intrinsic_type {
            Some(intrinsic_type) => {
                let value = self.codegen_intrinsic(intrinsic_type, function).as_basic_value_enum();
                self.builder.build_return(Some(&value));
            }
            None => {
                // The plan is to separate the notion of "expression blocks" from "control flow
                // blocks" to make this all more reasonable
                // Rust rejects "unreachable expression"
                let value = self.codegen_block_statements(
                    function.block.as_ref().expect("functions must have blocks by codegen"),
                );
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    let return_value =
                        value.unwrap_or(self.builtin_types.unit_value.as_basic_value_enum());
                    self.builder.build_return(Some(&return_value));
                }
            }
        };
        if let Some(start_block) = maybe_starting_block {
            self.builder.position_at_end(start_block);
        }
        fn_val
    }

    pub fn codegen_module(&mut self) {
        for constant in &self.module.constants {
            match constant.expr {
                TypedExpr::Int(i64, _) => {
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
        for (id, function) in self.module.clone().function_iter() {
            if !function.is_generic() {
                self.codegen_function(id, function);
            }
        }
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    pub fn optimize(&mut self, optimize: bool) -> CodegenResult<()> {
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
        if optimize {
            module_pass_manager.add_promote_memory_to_register_pass();
            module_pass_manager.add_function_attrs_pass();
            module_pass_manager.add_instruction_combining_pass();
        }
        // module_pass_manager.add_function_inlining_pass();
        module_pass_manager.add_verifier_pass();

        module_pass_manager.run_on(&self.llvm_module);

        machine.add_analysis_passes(&module_pass_manager);

        self.llvm_machine = Some(machine);

        Ok(())
    }

    pub fn emit_object_file(&self, rel_destination_dir: &str) -> Result<()> {
        let filename = format!("{}.o", self.name());
        let machine =
            self.llvm_machine.as_ref().expect("Cannot emit object file before optimizing");
        let path = Path::new(rel_destination_dir).join(Path::new(&filename));
        log::info!("Outputting object file to {}", path.to_str().unwrap());
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
