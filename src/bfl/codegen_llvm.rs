use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::path::Path;

use anyhow::bail;
use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DILocation, DIScope, DISubprogram, DIType, DWARFEmissionKind,
    DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType,
    StructType, VoidType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
    InstructionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
use log::{debug, info, trace};

use crate::lex::SpanId;
use crate::parse::{FileId, IdentifierId};
use crate::typer::scopes::ScopeId;
use crate::typer::{Linkage as TyperLinkage, *};

const STRING_LENGTH_FIELD_INDEX: u32 = 0;
const STRING_DATA_FIELD_INDEX: u32 = 1;

const ARRAY_LENGTH_FIELD_INDEX: u32 = 0;
const ARRAY_CAPACITY_FIELD_INDEX: u32 = 1;
const ARRAY_DATA_FIELD_INDEX: u32 = 2;

const WORD_SIZE_BITS: u64 = 64;

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
    pub span: SpanId,
}

type CodegenResult<T> = Result<T, CodegenError>;

impl Display for CodegenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("error in span {:?}: {}", self.span, self.message))
    }
}

impl Error for CodegenError {}

#[derive(Debug, Copy, Clone)]
struct BranchSetup<'ctx> {
    then_block: BasicBlock<'ctx>,
    else_block: BasicBlock<'ctx>,
}

#[derive(Debug, Clone, Copy)]
enum LlvmValue<'ctx> {
    BasicValue(BasicValueEnum<'ctx>),
    Never(InstructionValue<'ctx>),
}
impl<'ctx> LlvmValue<'ctx> {
    fn expect_value(self) -> BasicValueEnum<'ctx> {
        match self {
            LlvmValue::BasicValue(bv) => bv,
            LlvmValue::Never(_) => panic!("Expected BasicValue on never value"),
        }
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for LlvmValue<'ctx> {
    fn from(value: BasicValueEnum<'ctx>) -> Self {
        LlvmValue::BasicValue(value)
    }
}

#[derive(Debug, Copy, Clone)]
struct LlvmPointerType<'ctx> {
    #[allow(unused)]
    type_id: TypeId,
    pointer_type: BasicTypeEnum<'ctx>,
    pointee_type: BasicTypeEnum<'ctx>,
    di_type: DIType<'ctx>,
}

#[derive(Debug, Clone)]
struct LlvmVoidType<'ctx> {
    di_type: DIType<'ctx>,
    void_type: VoidType<'ctx>,
}

#[derive(Debug, Copy, Clone)]
struct LlvmValueType<'ctx> {
    #[allow(unused)]
    type_id: TypeId,
    basic_type: BasicTypeEnum<'ctx>,
    di_type: DIType<'ctx>,
}

#[derive(Debug, Clone, Copy)]
struct EnumVariantStructType<'ctx> {
    struct_type: StructType<'ctx>,
    size_bits: u32,
    di_type: DIType<'ctx>,
}

#[derive(Debug, Clone)]
struct LlvmEnumType<'ctx> {
    type_id: TypeId,
    base_struct_type: StructType<'ctx>,
    variant_structs: Vec<EnumVariantStructType<'ctx>>,
    di_type: DIType<'ctx>,
}

#[derive(Debug, Clone)]
struct LlvmStructType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    #[allow(unused)]
    fields: Vec<LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
}

#[derive(Debug, Clone)]
enum LlvmType<'ctx> {
    Value(LlvmValueType<'ctx>),
    EnumType(LlvmEnumType<'ctx>),
    StructType(LlvmStructType<'ctx>),
    Pointer(LlvmPointerType<'ctx>),
    Void(LlvmVoidType<'ctx>),
}

impl<'ctx> From<LlvmVoidType<'ctx>> for LlvmType<'ctx> {
    fn from(void: LlvmVoidType<'ctx>) -> Self {
        LlvmType::Void(void)
    }
}

impl<'ctx> From<LlvmValueType<'ctx>> for LlvmType<'ctx> {
    fn from(value: LlvmValueType<'ctx>) -> Self {
        LlvmType::Value(value)
    }
}

impl<'ctx> From<LlvmEnumType<'ctx>> for LlvmType<'ctx> {
    fn from(value: LlvmEnumType<'ctx>) -> Self {
        LlvmType::EnumType(value)
    }
}

impl<'ctx> From<LlvmStructType<'ctx>> for LlvmType<'ctx> {
    fn from(value: LlvmStructType<'ctx>) -> Self {
        LlvmType::StructType(value)
    }
}

impl<'ctx> From<LlvmPointerType<'ctx>> for LlvmType<'ctx> {
    fn from(value: LlvmPointerType<'ctx>) -> Self {
        LlvmType::Pointer(value)
    }
}

impl<'ctx> LlvmType<'ctx> {
    pub fn expect_pointer(&self) -> LlvmPointerType<'ctx> {
        match self {
            LlvmType::Value(value) => panic!("expected pointer on value: {:?}", value),
            LlvmType::Pointer(pointer) => *pointer,
            LlvmType::EnumType(_) => panic!("expected pointer on enum"),
            LlvmType::StructType(_) => panic!("expected pointer on struct"),
            LlvmType::Void(_) => panic!("expected pointer on void"),
        }
    }

    pub fn expect_enum(self) -> LlvmEnumType<'ctx> {
        match self {
            LlvmType::Value(value) => panic!("expected enum on value: {:?}", value),
            LlvmType::Pointer(_pointer) => panic!("expected enum on pointer"),
            LlvmType::EnumType(e) => e,
            LlvmType::StructType(_s) => panic!("expected enum on struct"),
            LlvmType::Void(_) => panic!("expected enum on void"),
        }
    }

    #[allow(unused)]
    fn type_id(&self) -> TypeId {
        match self {
            LlvmType::Value(value) => value.type_id,
            LlvmType::Pointer(pointer) => pointer.type_id,
            LlvmType::EnumType(e) => e.type_id,
            LlvmType::StructType(s) => s.type_id,
            LlvmType::Void(_) => NEVER_TYPE_ID,
        }
    }

    fn value_basic_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            LlvmType::Value(value) => value.basic_type,
            LlvmType::Pointer(pointer) => pointer.pointer_type,
            LlvmType::EnumType(e) => e.base_struct_type.as_basic_type_enum(),
            LlvmType::StructType(s) => s.struct_type.as_basic_type_enum(),
            LlvmType::Void(_) => panic!("No value type on Void / never"),
        }
    }

    #[allow(unused)]
    fn value_any_type(&self) -> AnyTypeEnum<'ctx> {
        match self {
            LlvmType::Void(v) => v.void_type.as_any_type_enum(),
            _ => self.value_basic_type().as_any_type_enum(),
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            LlvmType::Value(value) => value.di_type,
            LlvmType::Pointer(pointer) => pointer.di_type,
            LlvmType::EnumType(e) => e.di_type,
            LlvmType::StructType(s) => s.di_type,
            LlvmType::Void(v) => v.di_type,
        }
    }

    fn is_void(&self) -> bool {
        match self {
            LlvmType::Void(_) => true,
            _ => false,
        }
    }
}

struct BuiltinTypes<'ctx> {
    ctx: &'ctx Context,
    i64: IntType<'ctx>,
    unit: IntType<'ctx>,
    unit_value: IntValue<'ctx>,
    boolean: IntType<'ctx>,
    true_value: IntValue<'ctx>,
    false_value: IntValue<'ctx>,
    i1: IntType<'ctx>,
    char: IntType<'ctx>,
    c_str: PointerType<'ctx>,
    string_struct: StructType<'ctx>,
    tag_type: IntType<'ctx>,
}

impl<'ctx> BuiltinTypes<'ctx> {
    fn array_length(&self, builder: &Builder<'ctx>, array: StructValue<'ctx>) -> IntValue<'ctx> {
        let length =
            builder.build_extract_value(array, ARRAY_LENGTH_FIELD_INDEX, "array_length").unwrap();
        length.into_int_value()
    }

    fn build_array_capacity(
        &self,
        builder: &Builder<'ctx>,
        array: StructValue<'ctx>,
    ) -> IntValue<'ctx> {
        let cap =
            builder.build_extract_value(array, ARRAY_CAPACITY_FIELD_INDEX, "array_cap").unwrap();
        cap.into_int_value()
    }
    fn array_data(&self, builder: &Builder<'ctx>, array: StructValue<'ctx>) -> PointerValue<'ctx> {
        builder
            .build_extract_value(array, ARRAY_DATA_FIELD_INDEX, "array_data")
            .unwrap()
            .into_pointer_value()
    }

    fn string_length(
        &self,
        builder: &Builder<'ctx>,
        string_struct: StructValue<'ctx>,
    ) -> IntValue<'ctx> {
        builder
            .build_extract_value(string_struct, STRING_LENGTH_FIELD_INDEX, "string_length")
            .unwrap()
            .into_int_value()
    }

    fn string_data(
        &self,
        builder: &Builder<'ctx>,
        string_struct: StructValue<'ctx>,
    ) -> PointerValue<'ctx> {
        builder
            .build_extract_value(string_struct, STRING_DATA_FIELD_INDEX, "string_data")
            .unwrap()
            .into_pointer_value()
    }

    fn padding_type(&self, size_bits: u32) -> inkwell::types::BasicTypeEnum<'ctx> {
        let mut padding_members: Vec<BasicTypeEnum<'ctx>> = vec![];
        let mut remaining_bits = size_bits;
        while remaining_bits >= 64 {
            padding_members.push(self.ctx.i64_type().into());
            remaining_bits -= 64;
        }
        while remaining_bits >= 32 {
            padding_members.push(self.ctx.i32_type().into());
            remaining_bits -= 32;
        }
        while remaining_bits >= 8 {
            padding_members.push(self.ctx.i8_type().into());
            remaining_bits -= 8;
        }
        self.ctx.struct_type(&padding_members, true).as_basic_type_enum()
        // debug_assert!(size_bits % 8 == 0);
        // let byte_count = size_bits / 8;
        // self.ctx.custom_width_int_type(8).array_type(byte_count).as_basic_type_enum()
        //self.ctx.custom_width_int_type(size_bits).as_basic_type_enum()
    }
}

struct LibcFunctions<'ctx> {
    printf: FunctionValue<'ctx>,
    exit: FunctionValue<'ctx>,
    memcmp: FunctionValue<'ctx>,
}

pub struct Codegen<'ctx, 'module> {
    ctx: &'ctx Context,
    pub module: &'module TypedModule,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: Option<TargetMachine>,
    builder: Builder<'ctx>,
    llvm_functions: HashMap<FunctionId, FunctionValue<'ctx>>,
    llvm_types: RefCell<HashMap<TypeId, LlvmType<'ctx>>>,
    /// The type of stored pointers here should be one level higher than the type of the
    /// value pointed to. This is so that it can be used reliably, without matching or
    /// checking, as a Pointer to the actual type
    variables: HashMap<VariableId, Pointer<'ctx>>,
    globals: HashMap<VariableId, GlobalValue<'ctx>>,
    libc_functions: LibcFunctions<'ctx>,
    builtin_globals: HashMap<String, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    debug: DebugContext<'ctx>,
    tag_type_mappings: HashMap<IdentifierId, GlobalValue<'ctx>>,
}

struct DebugContext<'ctx> {
    files: HashMap<FileId, DIFile<'ctx>>,
    debug_builder: DebugInfoBuilder<'ctx>,
    #[allow(unused)]
    compile_unit: DICompileUnit<'ctx>,
    debug_stack: Vec<DebugStackEntry<'ctx>>,
    #[allow(unused)]
    scopes: HashMap<ScopeId, DIScope<'ctx>>,
    strip_debug: bool,
}

impl<'ctx> DebugContext<'ctx> {
    fn create_pointer_type(&self, name: &str, pointee: DIType<'ctx>) -> DIType<'ctx> {
        self.debug_builder
            .create_pointer_type(
                name,
                pointee,
                WORD_SIZE_BITS,
                WORD_SIZE_BITS as u32,
                AddressSpace::default(),
            )
            .as_type()
    }

    fn push_scope(&mut self, scope: DIScope<'ctx>, file: DIFile<'ctx>) {
        self.debug_stack.push(DebugStackEntry { scope, file });
    }
    fn pop_scope(&mut self) {
        self.debug_stack.pop();
    }
    fn current_scope(&self) -> DIScope<'ctx> {
        self.debug_stack.last().unwrap().scope
    }
    fn current_file(&self) -> DIFile<'ctx> {
        self.debug_stack.last().unwrap().file
    }
}

#[derive(Copy, Clone, Debug)]
struct Pointer<'ctx> {
    #[allow(unused)]
    pointee_type_id: TypeId,
    pointee_llvm_type: BasicTypeEnum<'ctx>,
    pointer: PointerValue<'ctx>,
}

impl<'ctx> Pointer<'ctx> {
    fn loaded_value(&self, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
        builder.build_load(self.pointee_llvm_type, self.pointer, "loaded_value")
    }
}

struct DebugStackEntry<'ctx> {
    scope: DIScope<'ctx>,
    file: DIFile<'ctx>,
}

fn i8_array_from_str<'ctx>(ctx: &'ctx Context, value: &str) -> ArrayValue<'ctx> {
    let bytes = value.as_bytes();
    ctx.const_string(bytes, false)
}

impl<'ctx, 'module> Codegen<'ctx, 'module> {
    fn init_debug(
        ctx: &'ctx Context,
        llvm_module: &LlvmModule<'ctx>,
        module: &TypedModule,
        optimize: bool,
        debug: bool,
    ) -> DebugContext<'ctx> {
        // We may need to create a DIBuilder per-file.
        // For now let's use main file
        let source = module.ast.sources.get_main();
        let (debug_builder, compile_unit) = llvm_module.create_debug_info_builder(
            false,
            DWARFSourceLanguage::C,
            &source.filename,
            &source.directory,
            "bfl_compiler",
            optimize,
            "",
            0,
            "",
            DWARFEmissionKind::Full,
            0,
            false,
            false,
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
            "MacOSX.sdk",
        );
        let md0 = ctx.metadata_node(&[
            ctx.i32_type().const_int(2, false).into(),
            ctx.metadata_string("SDK Version").into(),
            ctx.i32_type()
                .const_array(&[
                    ctx.i32_type().const_int(14, false),
                    ctx.i32_type().const_int(0, false),
                ])
                .into(),
        ]);
        let md1 = ctx.metadata_node(&[
            ctx.i32_type().const_int(2, false).into(),
            ctx.metadata_string("Dwarf Version").into(),
            ctx.i32_type().const_int(4, false).into(),
        ]);
        let md2 = ctx.metadata_node(&[
            ctx.i32_type().const_int(2, false).into(),
            ctx.metadata_string("Debug Info Version").into(),
            ctx.i32_type().const_int(3, false).into(),
        ]);
        // let md3 = ctx.metadata_node(&[
        //     ctx.i32_type().const_int(1, false).into(),
        //     ctx.metadata_string("PIC Level").into(),
        //     ctx.i32_type().const_int(2, false).into(),
        // ]);
        let md4 = ctx.metadata_node(&[
            ctx.i32_type().const_int(1, false).into(),
            ctx.metadata_string("PIE Level").into(),
            ctx.i32_type().const_int(2, false).into(),
        ]);
        llvm_module.add_global_metadata("llvm.module.flags", &md0).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md1).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md2).unwrap();
        // llvm_module.add_global_metadata("llvm.module.flags", &md3).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md4).unwrap();

        let di_files: HashMap<FileId, DIFile> = module
            .ast
            .sources
            .iter()
            .map(|(file_id, source)| {
                (file_id, debug_builder.create_file(&source.filename, &source.directory))
            })
            .collect();
        let mut debug = DebugContext {
            files: di_files,
            debug_builder,
            compile_unit,
            debug_stack: Vec::new(),
            scopes: HashMap::new(),
            strip_debug: !debug,
        };
        // May need to restore this
        debug.push_scope(compile_unit.as_debug_info_scope(), compile_unit.get_file());
        debug
    }

    pub fn create(
        ctx: &'ctx Context,
        module: &'module TypedModule,
        debug: bool,
        optimize: bool,
    ) -> Codegen<'ctx, 'module> {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let llvm_module = ctx.create_module(&module.ast.name);
        llvm_module.set_source_file_name(&module.ast.sources.get_main().filename);
        // Example of linking an LLVM module
        // let stdlib_module = ctx
        //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("nxlib/llvm")).unwrap())
        //     .unwrap();
        // llvm_module.link_in_module(stdlib_module).unwrap();

        let debug_context = Codegen::init_debug(ctx, &llvm_module, &module, optimize, debug);

        let pointers = HashMap::new();
        let format_int_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(3), None, "formatInt");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%i\0"));
            global
        };
        let format_str_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(5), None, "formatString");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%.*s\0"));
            global
        };
        let globals = HashMap::new();
        let mut builtin_globals: HashMap<String, GlobalValue<'ctx>> = HashMap::new();
        builtin_globals.insert("formatInt".to_string(), format_int_str);
        builtin_globals.insert("formatString".to_string(), format_str_str);
        let string_struct = Codegen::make_named_struct(
            ctx,
            "string",
            &[
                ctx.i64_type().as_basic_type_enum(),
                char_type.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            ],
        );

        let builtin_types = BuiltinTypes {
            ctx,
            i64: ctx.i64_type(),
            unit: ctx.i8_type(),
            unit_value: ctx.i8_type().const_int(0, false),
            // If we switch bools to i8, we need to cast to i1 for branches
            // If we keep i1, we need to do more alignment / padding work
            boolean: ctx.i8_type(),
            true_value: ctx.i8_type().const_int(1, false),
            false_value: ctx.i8_type().const_int(0, false),
            i1: ctx.bool_type(),
            char: char_type,
            c_str: char_type.ptr_type(AddressSpace::default()),
            string_struct,
            tag_type: ctx.i64_type(),
        };

        let printf_type = ctx.i32_type().fn_type(&[builtin_types.c_str.into()], true);
        let printf = llvm_module.add_function("printf", printf_type, Some(LlvmLinkage::External));
        let exit = llvm_module.add_function(
            "exit",
            ctx.void_type().fn_type(&[builtin_types.i64.into()], false),
            Some(LlvmLinkage::External),
        );
        let byte_ptr = ctx.i8_type().ptr_type(AddressSpace::default());
        let memcmp = llvm_module.add_function(
            "memcmp",
            ctx.i32_type()
                .fn_type(&[byte_ptr.into(), byte_ptr.into(), ctx.i64_type().into()], false),
            Some(LlvmLinkage::External),
        );
        let mut tag_type_value: u64 = 0;
        let mut tag_type_mappings: HashMap<IdentifierId, GlobalValue<'ctx>> = HashMap::new();
        for (_type_id, typ) in module.types.iter() {
            if let Type::TagInstance(tag) = typ {
                let value = builtin_types.tag_type.const_int(tag_type_value, false);
                let name = module.ast.identifiers.get_name(tag.ident);
                let global = llvm_module.add_global(
                    builtin_types.tag_type,
                    None,
                    &format!("Tag.{}", &*name),
                );
                global.set_constant(true);
                global.set_initializer(&value);
                tag_type_mappings.insert(tag.ident, global);
                tag_type_value += 1;
            }
        }
        Codegen {
            ctx,
            module,
            llvm_module,
            llvm_machine: None,
            builder,
            variables: pointers,
            globals,
            llvm_functions: HashMap::new(),
            llvm_types: RefCell::new(HashMap::new()),
            libc_functions: LibcFunctions { printf, exit, memcmp },
            builtin_globals,
            builtin_types,
            debug: debug_context,
            tag_type_mappings,
        }
    }

    fn set_debug_location(&self, span: SpanId) -> DILocation<'ctx> {
        let span = self.module.ast.spans.get(span);
        let line = self.module.ast.sources.get_line_for_span(span).expect("No line for span");
        let column = span.start - line.start_char;
        let locn = self.debug.debug_builder.create_debug_location(
            self.ctx,
            line.line_index + 1,
            column,
            self.debug.current_scope(),
            None,
        );
        self.builder.set_current_debug_location(locn);
        locn
    }

    fn get_ident_name(&self, id: IdentifierId) -> &str {
        self.module.ast.identifiers.get_name(id)
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

    fn build_print_string_call(&self, string_value: StructValue<'ctx>) -> BasicValueEnum<'ctx> {
        trace!("codegen print_string");
        let length = self.builtin_types.string_length(&self.builder, string_value);
        let data = self.builtin_types.string_data(&self.builder, string_value);

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

    fn build_optional_type(
        &self,
        type_id: TypeId,
        optional_type: &OptionalType,
    ) -> CodegenResult<LlvmStructType<'ctx>> {
        // Optional types are represented as a struct with a boolean
        // discriminant indicating whether the value is None, and the value itself
        let boolean_type = self.codegen_type(BOOL_TYPE_ID)?;
        let inner_type = self.codegen_type(optional_type.inner_type)?;
        let struct_type = self
            .ctx
            .struct_type(&[boolean_type.value_basic_type(), inner_type.value_basic_type()], false);
        let di_type = self.make_debug_struct_type(
            &format!("optional_{}", type_id.to_string()),
            SpanId::NONE,
            &[("tag", boolean_type.debug_type()), ("payload", inner_type.debug_type())],
        );
        Ok(LlvmStructType { type_id, struct_type, fields: vec![boolean_type, inner_type], di_type })
    }

    fn create_array_type(&self, elem_type: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        // NOTE: All the 'length' types could probably be i32 but our language int type is only i64
        //       so we couldn't enforce that at the lang level right now
        let length_type = self.ctx.i64_type().as_basic_type_enum();
        let capacity_type = self.ctx.i64_type().as_basic_type_enum();
        let data_type = elem_type.ptr_type(AddressSpace::default()).as_basic_type_enum();
        self.ctx.struct_type(&[length_type, capacity_type, data_type], false)
    }

    fn get_size_of_type_in_bits(&self, type_id: TypeId) -> CodegenResult<u32> {
        Ok(self.get_debug_type(type_id)?.get_size_in_bits() as u32)
    }

    fn get_line_number(&self, span: SpanId) -> u32 {
        let span = self.module.ast.spans.get(span);
        let line = self.module.ast.sources.get_line_for_span(span).expect("No line for span");
        line.line_index + 1
    }

    fn make_debug_struct_type(
        &self,
        name: &str,
        span: SpanId,
        field_types: &[(impl AsRef<str>, DIType<'ctx>)],
    ) -> DIType<'ctx> {
        let mut offset = 0;
        let mut final_size = 0;
        let line_number = self.get_line_number(span);
        let fields = &field_types
            .iter()
            .map(|(field_name, member_type)| {
                let t = self.debug.debug_builder.create_member_type(
                    self.debug.current_scope(),
                    field_name.as_ref(),
                    self.debug.current_file(),
                    line_number,
                    member_type.get_size_in_bits(),
                    WORD_SIZE_BITS as u32,
                    offset,
                    0,
                    *member_type,
                );
                // FIXME: for debug offset, what about padding
                offset += member_type.get_size_in_bits();
                final_size = offset + member_type.get_size_in_bits();
                t.as_type()
            })
            .collect::<Vec<_>>();
        self.debug
            .debug_builder
            .create_struct_type(
                self.debug.current_scope(),
                name,
                self.debug.current_file(),
                line_number,
                final_size,
                WORD_SIZE_BITS as u32,
                0,
                None,
                fields,
                0,
                None,
                name,
            )
            .as_type()
    }

    fn get_debug_type(&self, type_id: TypeId) -> CodegenResult<DIType<'ctx>> {
        Ok(self.codegen_type(type_id)?.debug_type())
    }

    fn codegen_type(&self, type_id: TypeId) -> CodegenResult<LlvmType<'ctx>> {
        trace!("codegen for type {}", self.module.type_id_to_string(type_id));
        let _dw_ate_address = 0x01;
        let dw_ate_boolean = 0x02;
        let _dw_ate_complex_float = 0x03;
        let _dw_ate_float = 0x04;
        let dw_ate_signed = 0x05;
        let dw_ate_char = 0x06;
        let dw_ate_unsigned = 0x07;
        let _dw_ate_unsigned_char = 0x08;
        let result = self.llvm_types.borrow().get(&type_id).cloned();
        if let Some(result) = result {
            return Ok(result);
        };
        let codegened_type = match type_id {
            UNIT_TYPE_ID => Ok(LlvmValueType {
                type_id: UNIT_TYPE_ID,
                basic_type: self.builtin_types.unit.as_basic_type_enum(),
                di_type: self
                    .debug
                    .debug_builder
                    .create_basic_type(
                        "unit",
                        self.builtin_types.unit.get_bit_width() as u64,
                        dw_ate_boolean,
                        0,
                    )
                    .unwrap()
                    .as_type(),
            }
            .into()),
            CHAR_TYPE_ID => Ok(LlvmValueType {
                type_id: CHAR_TYPE_ID,
                basic_type: self.builtin_types.char.as_basic_type_enum(),
                di_type: self
                    .debug
                    .debug_builder
                    .create_basic_type(
                        "char",
                        self.builtin_types.char.get_bit_width() as u64,
                        dw_ate_char,
                        0,
                    )
                    .unwrap()
                    .as_type(),
            }
            .into()),
            INT_TYPE_ID => Ok(LlvmValueType {
                type_id: INT_TYPE_ID,
                basic_type: self.builtin_types.i64.as_basic_type_enum(),
                di_type: self
                    .debug
                    .debug_builder
                    .create_basic_type(
                        "int",
                        self.builtin_types.i64.get_bit_width() as u64,
                        dw_ate_signed,
                        0,
                    )
                    .unwrap()
                    .as_type(),
            }
            .into()),
            BOOL_TYPE_ID => Ok(LlvmValueType {
                type_id: BOOL_TYPE_ID,
                basic_type: self.builtin_types.boolean.as_basic_type_enum(),
                di_type: self
                    .debug
                    .debug_builder
                    .create_basic_type(
                        "bool",
                        self.builtin_types.boolean.get_bit_width() as u64,
                        dw_ate_signed,
                        0,
                    )
                    .unwrap()
                    .as_type(),
            }
            .into()),
            STRING_TYPE_ID => {
                let data_ptr_type = self
                    .debug
                    .create_pointer_type("string_data", self.get_debug_type(CHAR_TYPE_ID)?);
                let di_type = self.make_debug_struct_type(
                    "string",
                    SpanId::NONE,
                    &[("length", self.get_debug_type(INT_TYPE_ID)?), ("data", data_ptr_type)],
                );
                Ok(LlvmValueType {
                    type_id: STRING_TYPE_ID,
                    basic_type: self.builtin_types.string_struct.as_basic_type_enum(),
                    di_type,
                }
                .into())
            }
            type_id => {
                // Generate and store the type in here
                let ty = self.module.types.get(type_id);
                match ty {
                    Type::Optional(optional) => {
                        Ok(self.build_optional_type(type_id, &optional)?.into())
                    }
                    Type::Struct(struc) => {
                        trace!("generating llvm type for struct type {type_id}");
                        let mut field_types = Vec::with_capacity(struc.fields.len());
                        let mut field_di_types = Vec::with_capacity(struc.fields.len());
                        let name = struc
                            .type_defn_info
                            .as_ref()
                            .map(|info| self.get_ident_name(info.name).to_string())
                            .unwrap_or("<anon_struct>".to_string());
                        for field in &struc.fields {
                            let field_type = self.codegen_type(field.type_id)?;
                            field_types.push(field_type.value_basic_type());
                            field_di_types.push((
                                self.module.ast.identifiers.get_name(field.name),
                                field_type.debug_type(),
                            ));
                        }
                        let struct_type = self.ctx.struct_type(&field_types, false);
                        let di_type = self.make_debug_struct_type(
                            &name,
                            self.module.ast.get_span_for_id(struc.ast_node),
                            &field_di_types,
                        );
                        Ok(LlvmValueType {
                            type_id,
                            basic_type: struct_type.as_basic_type_enum(),
                            di_type,
                        }
                        .into())
                    }
                    t @ Type::TypeVariable(v) => {
                        println!("{}", self.module);
                        let span = self.module.ast.get_span_for_maybe_id(t.ast_node());
                        Err(CodegenError {
                            message: format!(
                                "codegen was asked to codegen a type variable {:?}",
                                v
                            ),
                            span,
                        })
                    }
                    Type::Array(array) => {
                        let element_type = self.codegen_type(array.element_type)?;
                        let struct_type = self.create_array_type(element_type.value_basic_type());
                        let name = format!("array_{}", array.element_type);
                        // This badly duplicates the shape of 'array'
                        // This is why it would be amazing to have the tools in the source
                        // language to define types like this; basically we just need Pointer<T>
                        let struct_di_type = self.make_debug_struct_type(
                            &name,
                            SpanId::NONE,
                            &[
                                ("length", self.codegen_type(INT_TYPE_ID)?.debug_type()),
                                ("capacity", self.codegen_type(INT_TYPE_ID)?.debug_type()),
                                (
                                    "data",
                                    self.debug.create_pointer_type(
                                        "array_data",
                                        element_type.debug_type(),
                                    ),
                                ),
                            ],
                        );
                        Ok(LlvmPointerType {
                            type_id,
                            pointer_type: struct_type
                                .ptr_type(AddressSpace::default())
                                .as_basic_type_enum(),
                            pointee_type: struct_type.as_basic_type_enum(),
                            di_type: self.debug.create_pointer_type(&name, struct_di_type),
                        }
                        .into())
                    }
                    Type::Reference(reference) => {
                        // Could also use any_ptr here
                        let inner_type = self.codegen_type(reference.inner_type)?;
                        let inner_debug_type = inner_type.debug_type();
                        Ok(LlvmPointerType {
                            type_id,
                            pointer_type: inner_type
                                .value_basic_type()
                                .ptr_type(AddressSpace::default())
                                .as_basic_type_enum(),
                            pointee_type: inner_type.value_basic_type(),
                            di_type: self.debug.create_pointer_type(
                                &format!("reference_{}", type_id),
                                inner_debug_type,
                            ),
                        }
                        .into())
                    }
                    Type::TagInstance(tag_instance) => Ok(LlvmType::Value(LlvmValueType {
                        type_id,
                        basic_type: self.builtin_types.tag_type.as_basic_type_enum(),
                        di_type: self
                            .debug
                            .debug_builder
                            .create_basic_type(
                                &format!(
                                    "tag_{}",
                                    self.module.ast.identifiers.get_name(tag_instance.ident)
                                ),
                                self.builtin_types.tag_type.get_bit_width() as u64,
                                dw_ate_unsigned,
                                0,
                            )
                            .unwrap()
                            .as_type(),
                    })),
                    Type::Enum(enum_type) => {
                        let mut payload_section_size: u32 = 0;
                        let mut variant_structs = Vec::with_capacity(enum_type.variants.len());
                        let discriminant_field = self.builtin_types.tag_type;
                        let discriminant_field_debug = self
                            .debug
                            .debug_builder
                            .create_basic_type("tag", WORD_SIZE_BITS, dw_ate_unsigned, 0)
                            .unwrap()
                            .as_type();
                        for variant in enum_type.variants.iter() {
                            let variant_size: u32;
                            let (variant_struct, variant_struct_debug) =
                                if let Some(payload_type_id) = variant.payload {
                                    let variant_payload_type =
                                        self.codegen_type(payload_type_id)?;
                                    let struc = self.ctx.struct_type(
                                        &[
                                            discriminant_field.as_basic_type_enum(),
                                            variant_payload_type.value_basic_type(),
                                        ],
                                        false,
                                    );
                                    let debug_struct = {
                                        self.make_debug_struct_type(
                                            &format!(
                                                "variant_{}",
                                                self.module
                                                    .ast
                                                    .identifiers
                                                    .get_name(variant.tag_name)
                                            ),
                                            SpanId::NONE,
                                            &[
                                                ("tag", discriminant_field_debug),
                                                ("payload", variant_payload_type.debug_type()),
                                            ],
                                        )
                                    };
                                    variant_size =
                                        self.get_size_of_type_in_bits(payload_type_id)?;
                                    if variant_size > payload_section_size {
                                        payload_section_size = variant_size;
                                    }
                                    (struc, debug_struct)
                                } else {
                                    variant_size = 0;
                                    let s = self.ctx.struct_type(
                                        &[discriminant_field.as_basic_type_enum()],
                                        false,
                                    );
                                    let debug_struct = {
                                        self.make_debug_struct_type(
                                            &format!(
                                                "variant_{}",
                                                self.module
                                                    .ast
                                                    .identifiers
                                                    .get_name(variant.tag_name)
                                            ),
                                            SpanId::NONE,
                                            &[("tag", discriminant_field_debug)],
                                        )
                                    };
                                    (s, debug_struct)
                                };
                            variant_structs.push(EnumVariantStructType {
                                struct_type: variant_struct,
                                size_bits: variant_size,
                                di_type: variant_struct_debug,
                            });
                        }

                        // Now that we know the full size, go back and pad each variant struct
                        for (index, variant_struct) in variant_structs.iter_mut().enumerate() {
                            let size_diff_bits = payload_section_size - variant_struct.size_bits;
                            let mut fields = variant_struct.struct_type.get_field_types();
                            if size_diff_bits != 0 {
                                let padding =
                                    self.builtin_types.padding_type(size_diff_bits as u32);
                                fields.push(padding.as_basic_type_enum());
                                debug!(
                                    "Padding variant {} with {}",
                                    variant_struct.struct_type, padding
                                );
                                let variant = &enum_type.variants[index];
                                let struct_name = &format!(
                                    "{}_{}",
                                    &*self.get_ident_name(variant.tag_name),
                                    variant
                                        .payload
                                        .map(|p| p.to_string())
                                        .unwrap_or("".to_string()),
                                );
                                let variant_struct_type =
                                    Codegen::make_named_struct(&self.ctx, struct_name, &fields);
                                variant_struct.struct_type = variant_struct_type;
                            } else {
                                debug!("Not padding variant {}", variant_struct.struct_type);
                            }
                        }

                        let discriminant_field = self.builtin_types.tag_type;
                        let union_padding = self.builtin_types.padding_type(payload_section_size);

                        let base_type = self.ctx.struct_type(
                            &[
                                discriminant_field.as_basic_type_enum(),
                                union_padding.as_basic_type_enum(),
                            ],
                            false,
                        );

                        let full_size = discriminant_field.get_bit_width() + payload_section_size;
                        let member_types: Vec<_> = variant_structs
                            .iter()
                            .map(|variant| {
                                let name = Codegen::get_di_type_name(variant.di_type);
                                self.debug
                                    .debug_builder
                                    .create_member_type(
                                        self.debug.current_scope(),
                                        &name,
                                        self.debug.current_file(),
                                        0,
                                        variant.size_bits as u64,
                                        64,
                                        0,
                                        0,
                                        variant.di_type,
                                    )
                                    .as_type()
                            })
                            .collect();
                        let name = enum_type
                            .type_defn_info
                            .as_ref()
                            .map(|info| self.module.ast.identifiers.get_name(info.name).to_string())
                            .unwrap_or(type_id.to_string());
                        let name = &format!("{name}_{type_id}");
                        let debug_union_type = self
                            .debug
                            .debug_builder
                            .create_union_type(
                                self.debug.current_scope(),
                                &name,
                                self.debug.current_file(),
                                0,
                                full_size as u64,
                                64,
                                0,
                                &member_types,
                                0,
                                &name,
                            )
                            .as_type();

                        Ok(LlvmEnumType {
                            type_id,
                            base_struct_type: base_type,
                            variant_structs,
                            di_type: debug_union_type,
                        }
                        .into())
                    }
                    Type::EnumVariant(ev) => {
                        let parent_enum = self.codegen_type(ev.enum_type_id)?.expect_enum();
                        // Let's try this for now; we may have to represent this differently
                        // as a LlvmStructType
                        // let variant = parent_enum.variant_structs[ev.index];
                        Ok(parent_enum.into())
                    }
                    Type::Never => {
                        // From DWARF standard:
                        // "Debugging information entries for C void functions
                        // should not have an attribute for the return"
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_basic_type("never", 0, dw_ate_char, 0)
                            .unwrap()
                            .as_type();
                        Ok(LlvmVoidType { void_type: self.ctx.void_type(), di_type }.into())
                    }
                    other => Err(CodegenError {
                        message: format!(
                            "codegen_type for type dropped through unexpectedly: {:?}",
                            other
                        ),
                        span: self.module.ast.get_span_for_maybe_id(other.ast_node()),
                    }),
                }
            }
        }?;
        self.llvm_types.borrow_mut().insert(type_id, codegened_type.clone());
        Ok(codegened_type)
    }

    fn get_di_type_name(di_type: DIType<'ctx>) -> String {
        use llvm_sys::debuginfo::LLVMDITypeGetName;
        unsafe {
            let mut length = 0;
            let buf = LLVMDITypeGetName(di_type.as_mut_ptr(), &mut length);
            let slice: &[u8] = std::slice::from_raw_parts(buf as *const u8, length);
            String::from_utf8_lossy(slice).to_string()
        }
    }

    fn make_named_struct(
        ctx: &'ctx Context,
        name: &str,
        fields: &[BasicTypeEnum<'ctx>],
    ) -> StructType<'ctx> {
        let struc = ctx.opaque_struct_type(name);
        struc.set_body(fields, false);
        struc
    }

    fn codegen_val(&mut self, val: &ValDef) -> CodegenResult<PointerValue<'ctx>> {
        let value = self.codegen_expr_basic_value(&val.initializer)?;
        let variable_type = self.codegen_type(val.ty)?;
        let variable = self.module.variables.get_variable(val.variable_id);
        let variable_ptr = self
            .builder
            .build_alloca(variable_type.value_basic_type(), &self.get_ident_name(variable.name));
        trace!(
            "codegen_val {}: pointee_ty: {variable_type:?}",
            &*self.get_ident_name(variable.name)
        );
        // We're always storing a pointer
        // in self.variables that, when loaded, gives the actual type of the variable
        let store_instr = self.builder.build_store(variable_ptr, value);
        self.debug.debug_builder.insert_declare_before_instruction(
            variable_ptr,
            Some(self.debug.debug_builder.create_auto_variable(
                self.debug.current_scope(),
                &self.get_ident_name(variable.name),
                self.debug.current_file(),
                self.get_line_number(val.span),
                variable_type.debug_type(),
                true,
                0,
                WORD_SIZE_BITS as u32,
            )),
            None,
            self.builder.get_current_debug_location().unwrap(),
            store_instr,
        );
        let pointer = Pointer {
            pointer: variable_ptr,
            pointee_type_id: val.ty,
            pointee_llvm_type: variable_type.value_basic_type(),
        };
        self.variables.insert(val.variable_id, pointer);
        Ok(variable_ptr)
    }

    fn codegen_if_else(&mut self, ir_if: &TypedIf) -> CodegenResult<LlvmValue<'ctx>> {
        let typ = self.codegen_type(ir_if.ty)?;
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let consequent_block = self.ctx.append_basic_block(current_fn, "if_cons");
        let alternate_block = self.ctx.append_basic_block(current_fn, "if_alt");
        let merge_block = self.ctx.append_basic_block(current_fn, "if_merge");

        // Entry block
        let condition = self.codegen_expr_basic_value(&ir_if.condition)?;
        let condition_value = self.bool_to_i1(condition.into_int_value(), "cond_i1");
        self.builder.build_conditional_branch(condition_value, consequent_block, alternate_block);

        // TODO: Support when either or both arms are of type 'never'
        // or, equivalently for typing the 'if', if they
        // return from the whole function, rather than yield an expression value
        //Figure out what phi does if one of its incomings is unreachable

        // Consequent Block
        self.builder.position_at_end(consequent_block);
        let consequent_incoming = match self.codegen_block_statements(&ir_if.consequent)? {
            LlvmValue::Never(_) => None,
            LlvmValue::BasicValue(value) => {
                let consequent_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block);
                Some((consequent_final_block, value))
            }
        };

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_incoming = match self.codegen_block_statements(&ir_if.alternate)? {
            LlvmValue::Never(_) => None,
            LlvmValue::BasicValue(value) => {
                let alternate_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block);
                Some((alternate_final_block, value))
            }
        };

        // Merge block
        self.builder.position_at_end(merge_block);
        let phi_value = self.builder.build_phi(typ.value_basic_type(), "if_phi");
        if let Some((cons_inc_block, cons_value)) = consequent_incoming {
            phi_value.add_incoming(&[(&cons_value, cons_inc_block)]);
        };
        if let Some((alt_inc_block, alt_value)) = alternate_incoming {
            phi_value.add_incoming(&[(&alt_value, alt_inc_block)]);
        };
        debug!(
            "{:?} PHI COUNT {}",
            self.builder.get_insert_block().unwrap().get_parent().unwrap().get_name(),
            phi_value.count_incoming()
        );
        if phi_value.count_incoming() == 0 {
            // Both branches are divergent
            let unreachable = self.builder.build_unreachable();
            Ok(LlvmValue::Never(unreachable))
        } else {
            Ok(LlvmValue::BasicValue(phi_value.as_basic_value()))
        }
    }

    /// We expect this expr to produce a value, rather than exiting the program or otherwise crashing
    /// This is the most common case
    fn codegen_expr_basic_value(
        &mut self,
        expr: &TypedExpr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        Ok(self.codegen_expr(expr)?.expect_value())
    }

    fn codegen_optional_value(
        &mut self,
        type_id: TypeId,
        optional_some: Option<&OptionalSome>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let is_none = optional_some.is_none();
        let optional_llvm_type = self.codegen_type(type_id)?;
        let struct_type = optional_llvm_type.value_basic_type().into_struct_type();
        let discriminator_value =
            if is_none { self.builtin_types.false_value } else { self.builtin_types.true_value };
        let value_value: BasicValueEnum<'ctx> = if let Some(opt_some) = optional_some {
            self.codegen_expr_basic_value(&opt_some.inner_expr)?
        } else {
            struct_type.get_field_type_at_index(1).unwrap().const_zero()
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
            .build_insert_value(none_struct, value_value, 1, &format!("opt_value_{}", type_id))
            .unwrap();
        Ok(struct_value.as_basic_value_enum())
    }
    fn codegen_optional_has_value(&mut self, optional_value: StructValue<'ctx>) -> IntValue<'ctx> {
        let discriminator_value = self
            .builder
            .build_extract_value(optional_value, 0, "discrim_value")
            .unwrap()
            .into_int_value();
        let is_some = self.builder.build_int_compare(
            IntPredicate::NE,
            discriminator_value,
            self.builtin_types.false_value,
            "is_some_cmp",
        );
        let is_some_bool = self.i1_to_bool(is_some, "is_some");
        is_some_bool
    }
    fn codegen_optional_get(&mut self, optional_value: StructValue<'ctx>) -> BasicValueEnum<'ctx> {
        // This is UNSAFE we don't check the discriminator. The actual unwrap method
        // or user-facing unwrap should check it and exit if it is None later
        self.builder
            .build_extract_value(optional_value, 1, "opt_value")
            .expect("optional should always have a 2nd field")
    }

    fn codegen_expr_lvalue(&mut self, expr: &TypedExpr) -> CodegenResult<PointerValue<'ctx>> {
        trace!("codegen expr lvalue\n{}", self.module.expr_to_string(expr));
        // We need only match on expression types that are allowed to appear as lvalues. That means:
        // - Variables
        // - Array indices
        // - Struct members
        match expr {
            TypedExpr::Variable(ir_var) => {
                if let Some(pointer) = self.variables.get(&ir_var.variable_id) {
                    trace!(
                        "codegen variable (lvalue) got pointee type {:?}",
                        pointer.pointee_llvm_type
                    );
                    Ok(pointer.pointer)
                } else {
                    Err(CodegenError {
                        message: format!(
                            "No pointer found for variable (lvalue) {}",
                            self.module.expr_to_string(expr)
                        ),
                        span: expr.get_span(),
                    })
                }
            }
            TypedExpr::StructFieldAccess(field_access) => {
                // Expect pointer since since is an lvalue, which means field_access.base is actually a reference
                // to a struct.
                // But do not use the lvalue version of codegen_expr() since that gives us a pointer to the pointer!
                debug_assert!(self
                    .module
                    .types
                    .get(field_access.base.get_type())
                    .as_reference()
                    .is_some());

                let struct_type =
                    self.codegen_type(field_access.base.get_type())?.expect_pointer().pointee_type;

                let struct_pointer =
                    self.codegen_expr(&field_access.base)?.expect_value().into_pointer_value();
                let field_ptr = self
                    .builder
                    .build_struct_gep(
                        struct_type,
                        struct_pointer,
                        field_access.target_field_index,
                        &format!(
                            "struct.{}",
                            &*self.module.ast.identifiers.get_name(field_access.target_field)
                        ),
                    )
                    .unwrap();
                Ok(field_ptr)
            }
            TypedExpr::ArrayIndex(index_op) => {
                let elem_ptr = self.codegen_array_index_operation(index_op)?;
                Ok(elem_ptr)
            }
            TypedExpr::StringIndex(index_op) => {
                let elem_ptr = self.codegen_string_index_operation(index_op)?;
                Ok(elem_ptr)
            }
            _ => Err(CodegenError {
                message: format!("Unexpected lvalue: {}", self.module.expr_to_string(expr)),
                span: expr.get_span(),
            }),
        }
    }

    fn codegen_expr(&mut self, expr: &TypedExpr) -> CodegenResult<LlvmValue<'ctx>> {
        self.set_debug_location(expr.get_span());
        trace!("codegen expr\n{}", self.module.expr_to_string(expr));
        match expr {
            TypedExpr::Variable(ir_var) => {
                if let Some(pointer) = self.variables.get(&ir_var.variable_id) {
                    trace!("codegen variable got pointee type {:?}", pointer.pointee_llvm_type);
                    let loaded = pointer.loaded_value(&self.builder);
                    trace!("codegen variable (rvalue) got loaded value {:?}", loaded);
                    Ok(loaded.into())
                } else if let Some(global) = self.globals.get(&ir_var.variable_id) {
                    let value = global.get_initializer().unwrap();
                    Ok(value.into())
                } else {
                    Err(CodegenError {
                        message: format!(
                            "No pointer or global found for variable {}",
                            self.module.expr_to_string(expr)
                        ),
                        span: expr.get_span(),
                    })
                }
            }
            TypedExpr::None(type_id, _) => {
                let optional_value = self.codegen_optional_value(*type_id, None)?;
                Ok(optional_value.into())
            }
            TypedExpr::OptionalSome(opt_some) => {
                let optional_value =
                    self.codegen_optional_value(opt_some.type_id, Some(opt_some))?;
                Ok(optional_value.into())
            }
            TypedExpr::OptionalHasValue(optional_expr) => {
                let optional_value = self.codegen_expr_basic_value(optional_expr)?;
                let optional_has_value =
                    self.codegen_optional_has_value(optional_value.into_struct_value());
                Ok(optional_has_value.as_basic_value_enum().into())
            }
            TypedExpr::OptionalGet(opt_get) => {
                if opt_get.checked {
                    let optional_value = self.codegen_expr_basic_value(&opt_get.inner_expr)?;
                    let has_value =
                        self.codegen_optional_has_value(optional_value.into_struct_value());
                    let has_value_i1 = self.bool_to_i1(has_value, "unwrap_check");
                    let branch =
                        self.build_conditional_branch(has_value_i1, "unwrap_ok", "unwrap_crash");

                    // label: unwrap_crash
                    self.builder.position_at_end(branch.else_block);
                    self.build_bfl_crash("get on empty optional", opt_get.span);

                    // label: unwrap_ok
                    self.builder.position_at_end(branch.then_block);
                    let payload = self.codegen_optional_get(optional_value.into_struct_value());

                    Ok(payload.into())
                } else {
                    let optional_value = self.codegen_expr_basic_value(&opt_get.inner_expr)?;
                    let payload = self.codegen_optional_get(optional_value.into_struct_value());
                    Ok(payload.into())
                }
            }
            TypedExpr::Unit(_) => Ok(self.builtin_types.unit_value.as_basic_value_enum().into()),
            TypedExpr::Char(byte, _) => Ok(self
                .builtin_types
                .char
                .const_int(*byte as u64, false)
                .as_basic_value_enum()
                .into()),
            TypedExpr::Bool(b, _) => match b {
                true => Ok(self.builtin_types.true_value.as_basic_value_enum().into()),
                false => Ok(self.builtin_types.false_value.as_basic_value_enum().into()),
            },
            TypedExpr::Int(int_value, _) => {
                // LLVM only has unsigned values, the instructions are what provide the semantics
                // of signed vs unsigned
                let value = self.builtin_types.i64.const_int(*int_value as u64, false);
                Ok(value.as_basic_value_enum().into())
            }
            TypedExpr::Str(string_value, _) => {
                // We will make them structs (structs) with an array pointer and a length for now
                let global_str_data = self.llvm_module.add_global(
                    self.builtin_types.char.array_type(string_value.len() as u32),
                    None,
                    "string_lit_data",
                );
                global_str_data.set_initializer(&i8_array_from_str(self.ctx, string_value));
                global_str_data.set_constant(true);
                let global_value = self.llvm_module.add_global(
                    self.builtin_types.string_struct,
                    None,
                    "string_lit",
                );
                global_value.set_constant(true);
                let value = self.builtin_types.string_struct.const_named_struct(&[
                    self.builtin_types.i64.const_int(string_value.len() as u64, true).into(),
                    global_str_data.as_pointer_value().into(),
                ]);
                global_value.set_initializer(&value);
                let loaded = self.builder.build_load(
                    self.builtin_types.string_struct,
                    global_value.as_pointer_value(),
                    "str_struct",
                );
                Ok(loaded.into())
            }
            TypedExpr::Struct(struc) => {
                let struct_llvm_type =
                    self.codegen_type(struc.type_id)?.value_basic_type().into_struct_type();
                let mut struct_value = struct_llvm_type.get_undef();
                for (idx, field) in struc.fields.iter().enumerate() {
                    let value = self.codegen_expr_basic_value(&field.expr)?;
                    struct_value = self
                        .builder
                        .build_insert_value(
                            struct_value,
                            value,
                            idx as u32,
                            &format!("struct_init_{}", idx),
                        )
                        .unwrap()
                        .into_struct_value();
                }
                Ok(struct_value.as_basic_value_enum().into())
            }
            TypedExpr::StructFieldAccess(field_access) => {
                // Codegen the field's loaded value
                let struc = self.codegen_expr_basic_value(&field_access.base)?;
                let struct_struct = struc.into_struct_value();
                let field_value = self
                    .builder
                    .build_extract_value(
                        struct_struct,
                        field_access.target_field_index,
                        &format!(
                            "struc.{}",
                            &*self.module.ast.identifiers.get_name(field_access.target_field)
                        ),
                    )
                    .unwrap();
                Ok(field_value.into())
            }
            TypedExpr::Array(array) => {
                let Type::Array(array_type) = self.module.types.get(array.type_id) else {
                    panic!("expected array type for array");
                };
                let element_type = self.codegen_type(array_type.element_type)?.value_basic_type();
                let array_len = self.builtin_types.i64.const_int(array.elements.len() as u64, true);

                let array_capacity = array_len;
                let array_value = self.make_array(array_len, array_capacity, element_type, false);
                let array_data = self.builtin_types.array_data(&self.builder, array_value);
                // Store each element
                for (index, element_expr) in array.elements.iter().enumerate() {
                    let value = self.codegen_expr_basic_value(element_expr)?;
                    let index_value = self.ctx.i64_type().const_int(index as u64, true);
                    trace!("storing element {} of array literal: {:?}", index, element_expr);
                    let ptr_at_index = unsafe {
                        self.builder.build_gep(element_type, array_data, &[index_value], "elem")
                    };
                    self.builder.build_store(ptr_at_index, value);
                }
                let array_ptr = self.builder.build_alloca(array_value.get_type(), "array_ptr");
                self.builder.build_store(array_ptr, array_value);
                Ok(array_ptr.as_basic_value_enum().into())
            }
            TypedExpr::If(if_expr) => self.codegen_if_else(if_expr),
            TypedExpr::BinaryOp(bin_op) => self.codegen_binop(bin_op),
            TypedExpr::UnaryOp(unary_op) => {
                let value = self.codegen_expr_basic_value(&unary_op.expr)?;
                match unary_op.kind {
                    UnaryOpKind::Dereference => {
                        let value_ptr = value.into_pointer_value();
                        let pointee_ty = self.codegen_type(unary_op.type_id)?;
                        let value = self.builder.build_load(
                            pointee_ty.value_basic_type(),
                            value_ptr,
                            "deref",
                        );
                        Ok(value.into())
                    }
                    UnaryOpKind::Reference => {
                        let value_location = self.builder.build_alloca(value.get_type(), "ref");
                        self.builder.build_store(value_location, value);
                        Ok(value_location.as_basic_value_enum().into())
                    }
                    UnaryOpKind::BooleanNegation => {
                        let truncated = self.bool_to_i1(value.into_int_value(), "bool_trunc4neg");
                        let negated = self.builder.build_not(truncated, "i1_negated");
                        let promoted = self.i1_to_bool(negated, "negated");
                        Ok(promoted.as_basic_value_enum().into())
                    }
                }
            }
            TypedExpr::Block(block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and yield the result value
                let block_value = self.codegen_block_statements(block)?;
                Ok(block_value)
            }
            TypedExpr::FunctionCall(call) => self.codegen_function_call(call),
            TypedExpr::ArrayIndex(index_op) => {
                let elem_ptr = self.codegen_array_index_operation(index_op)?;
                let elem_type = self.codegen_type(index_op.result_type)?;
                let elem_value = self.builder.build_load(
                    elem_type.value_basic_type(),
                    elem_ptr,
                    "array_index_rvalue",
                );
                Ok(elem_value.into())
            }
            TypedExpr::StringIndex(index_op) => {
                let elem_ptr = self.codegen_string_index_operation(index_op)?;
                let elem_type = self.codegen_type(index_op.result_type)?;
                let elem_value = self.builder.build_load(
                    elem_type.value_basic_type(),
                    elem_ptr,
                    "string_index_rvalue",
                );
                Ok(elem_value.into())
            }
            TypedExpr::Tag(tag_expr) => {
                let int_value = self.get_value_for_tag_type(tag_expr.name);
                Ok(int_value.as_basic_value_enum().into())
            }
            TypedExpr::EnumConstructor(enum_constr) => {
                let llvm_type = self.codegen_type(enum_constr.type_id)?;
                let enum_type = llvm_type.expect_enum();
                let variant_tag_name = enum_constr.variant_name;

                let llvm_variant =
                    enum_type.variant_structs[enum_constr.variant_index as usize].struct_type;

                let enum_ptr = self.builder.build_alloca(llvm_variant, "enum_constr");

                // Store the tag_value in the first slot
                let tag_value = self.get_value_for_tag_type(variant_tag_name);
                let tag_pointer = self
                    .builder
                    .build_struct_gep(
                        llvm_variant,
                        enum_ptr,
                        0,
                        &format!("enum_tag_{}", &*self.get_ident_name(variant_tag_name)),
                    )
                    .unwrap();
                self.builder.build_store(tag_pointer, tag_value);

                if let Some(payload) = &enum_constr.payload {
                    let value = self.codegen_expr_basic_value(payload)?;
                    let payload_pointer = self
                        .builder
                        .build_struct_gep(
                            llvm_variant,
                            enum_ptr,
                            1,
                            &format!("enum_payload_{}", &*self.get_ident_name(variant_tag_name)),
                        )
                        .unwrap();
                    self.builder.build_store(payload_pointer, value);
                }

                let loaded_value =
                    self.builder.build_load(enum_type.base_struct_type, enum_ptr, "enum_value");
                Ok(loaded_value.into())
            }
            TypedExpr::EnumIsVariant(enum_is_variant) => {
                let enum_value = self
                    .codegen_expr_basic_value(&enum_is_variant.target_expr)?
                    .into_struct_value();
                let is_variant_bool =
                    self.codegen_enum_is_variant(enum_value, enum_is_variant.variant_name);
                Ok(is_variant_bool.as_basic_value_enum().into())
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                let enum_type =
                    self.codegen_type(enum_get_payload.target_expr.get_type())?.expect_enum();
                let enum_value = self
                    .codegen_expr_basic_value(&enum_get_payload.target_expr)?
                    .into_struct_value();
                let variant_type =
                    &enum_type.variant_structs[enum_get_payload.variant_index as usize];
                let value_payload = self.get_enum_payload(variant_type.struct_type, enum_value);
                Ok(value_payload.into())
            }
            TypedExpr::EnumCast(enum_cast) => {
                let enum_value =
                    self.codegen_expr_basic_value(&enum_cast.base)?.into_struct_value();
                let is_variant_bool =
                    self.codegen_enum_is_variant(enum_value, enum_cast.variant_name);

                let cond = self.bool_to_i1(is_variant_bool, "enum_cast_check");
                let branch = self.build_conditional_branch(cond, "cast_post", "cast_fail");

                // cast_fail
                self.builder.position_at_end(branch.else_block);
                // TODO: We can call @llvm.expect.i1 in order
                // to help branch prediction and indicate that failure is 'cold'
                self.build_bfl_crash("bad enum cast", enum_cast.span);

                // cast_post
                self.builder.position_at_end(branch.then_block);
                // Ultimately, this cast is currently a no-op
                // So we don't need to emit any instructions in cast_post

                Ok(enum_value.as_basic_value_enum().into())
            }
        }
    }

    fn codegen_binop(&mut self, bin_op: &BinaryOp) -> CodegenResult<LlvmValue<'ctx>> {
        match bin_op.ty {
            INT_TYPE_ID => {
                let lhs_value = self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                let rhs_value = self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
                let op_res = match bin_op.kind {
                    BinaryOpKind::Add => self.builder.build_int_add(lhs_value, rhs_value, "add"),
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
                    BinaryOpKind::Equals => {
                        self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "eq")
                    }
                    _ => {
                        panic!("Unsupported bin op kind returning int: {}", bin_op.kind)
                    }
                };
                Ok(op_res.as_basic_value_enum().into())
            }
            BOOL_TYPE_ID => match bin_op.kind {
                BinaryOpKind::And | BinaryOpKind::Or => {
                    let lhs = self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                    let op = match bin_op.kind {
                        BinaryOpKind::And => {
                            let lhs_i1 = self.bool_to_i1(lhs, "lhs_for_cmp");
                            let short_circuit_branch =
                                self.build_conditional_branch(lhs_i1, "rhs_check", "short_circuit");
                            let phi_destination = self.append_basic_block("and_result");

                            // label: rhs_check; lhs was true
                            self.builder.position_at_end(short_circuit_branch.then_block);
                            let rhs = self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();

                            // Don't forget to grab the actual incoming block in case bin_op.rhs did branching!
                            let rhs_incoming = self.builder.get_insert_block().unwrap();
                            self.builder.build_unconditional_branch(phi_destination);
                            // return rhs

                            // label: short_circuit; lhs was false
                            self.builder.position_at_end(short_circuit_branch.else_block);
                            self.builder.build_unconditional_branch(phi_destination);
                            // return lhs aka false

                            // label: and_result
                            self.builder.position_at_end(phi_destination);
                            let result =
                                self.builder.build_phi(self.builtin_types.boolean, "bool_and");
                            result.add_incoming(&[
                                (&rhs, rhs_incoming),
                                (&self.builtin_types.false_value, short_circuit_branch.else_block),
                            ]);
                            result.as_basic_value().into_int_value()
                        }
                        BinaryOpKind::Or => {
                            let rhs_int =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
                            self.builder.build_or(lhs, rhs_int, "bool_or")
                        }
                        BinaryOpKind::Equals => {
                            let rhs_int =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
                            self.builder.build_int_compare(
                                IntPredicate::EQ,
                                lhs,
                                rhs_int,
                                "bool_eq",
                            )
                        }
                        _ => panic!("Unhandled binop combo"),
                    };
                    Ok(op.as_basic_value_enum().into())
                }
                BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                    let lhs_int = self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                    let rhs_int = self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
                    let cmp_result = self.builder.build_int_compare(
                        if bin_op.kind == BinaryOpKind::Equals {
                            IntPredicate::EQ
                        } else {
                            IntPredicate::NE
                        },
                        lhs_int,
                        rhs_int,
                        &format!("{}_i1", bin_op.kind),
                    );
                    Ok(self
                        .i1_to_bool(cmp_result, &format!("{}_res", bin_op.kind))
                        .as_basic_value_enum()
                        .into())
                }

                BinaryOpKind::Less
                | BinaryOpKind::LessEqual
                | BinaryOpKind::Greater
                | BinaryOpKind::GreaterEqual => {
                    let lhs_int = self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                    let rhs_int = self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
                    let pred = match bin_op.kind {
                        BinaryOpKind::Less => IntPredicate::SLT,
                        BinaryOpKind::LessEqual => IntPredicate::SLE,
                        BinaryOpKind::Greater => IntPredicate::SGT,
                        BinaryOpKind::GreaterEqual => IntPredicate::SGE,
                        _ => unreachable!("unexpected binop kind"),
                    };
                    let i1_compare = self.builder.build_int_compare(
                        pred,
                        lhs_int,
                        rhs_int,
                        &format!("{}_i1", bin_op.kind),
                    );
                    Ok(self
                        .i1_to_bool(i1_compare, &format!("{}_res", bin_op.kind))
                        .as_basic_value_enum()
                        .into())
                }
                other => panic!("Unsupported binary operation {other:?} returning Bool"),
            },
            STRING_TYPE_ID => panic!("No string-returning binary ops yet"),
            UNIT_TYPE_ID => panic!("No unit-returning binary ops"),
            CHAR_TYPE_ID => panic!("No char-returning binary ops"),
            other => todo!("codegen for binary ops on user-defined types: {other}"),
        }
    }

    fn build_bfl_crash(&self, msg: &str, span_id: SpanId) -> InstructionValue {
        let msg_string = self.const_string_ptr(&msg, "crash_msg");
        let span = self.module.ast.spans.get(span_id);
        let line = self.module.ast.sources.get_line_for_span(span).unwrap();
        let filename = self
            .const_string_ptr(&self.module.ast.sources.source_by_span(span).filename, "filename");
        self.builder.build_call(
            self.llvm_module.get_function("_bfl_crash").unwrap(),
            &[
                msg_string.into(),
                filename.into(),
                self.builtin_types.i64.const_int(line.line_index as u64 + 1, false).into(),
            ],
            "crash",
        );
        self.builder.build_unreachable()
    }

    fn build_conditional_branch(
        &mut self,
        cond: IntValue<'ctx>,
        then_name: &str,
        else_name: &str,
    ) -> BranchSetup<'ctx> {
        let then_block = self.append_basic_block(then_name);
        let else_block = self.append_basic_block(else_name);
        self.builder.build_conditional_branch(cond, then_block, else_block);
        BranchSetup { then_block, else_block }
    }

    fn append_basic_block(&mut self, name: &str) -> BasicBlock<'ctx> {
        let origin_block = self.builder.get_insert_block().unwrap();
        let current_fn = origin_block.get_parent().unwrap();
        let block = self.ctx.append_basic_block(current_fn, name);
        block
    }

    fn codegen_enum_is_variant(
        &mut self,
        enum_value: StructValue<'ctx>,
        variant_name: IdentifierId,
    ) -> IntValue<'ctx> {
        let tag_value = self.get_value_for_tag_type(variant_name);
        let enum_tag_value = self.get_enum_tag(enum_value);
        let is_equal = self.builder.build_int_compare(
            IntPredicate::EQ,
            enum_tag_value,
            tag_value,
            "is_variant_cmp",
        );
        let is_equal_bool = self.i1_to_bool(is_equal, "is_variant");
        is_equal_bool
    }

    fn bool_to_i1(&self, bool: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_truncate(bool, self.builtin_types.i1, name)
    }

    fn i1_to_bool(&self, i1: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_cast(i1, self.builtin_types.boolean, name)
    }

    fn get_enum_tag(&self, enum_value: StructValue<'ctx>) -> IntValue<'ctx> {
        self.builder.build_extract_value(enum_value, 0, "get_tag").unwrap().into_int_value()
    }

    fn get_enum_payload(
        &self,
        variant_type: StructType<'ctx>,
        enum_value: StructValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ptr = self.builder.build_alloca(variant_type, "enum_ptr_for_payload");
        self.builder.build_store(ptr, enum_value);
        // let casted_ptr = self
        //     .builder
        //     .build_bitcast(ptr, variant_type.ptr_type(AddressSpace::default()), "variant_cast")
        //     .into_pointer_value();
        let payload_ptr =
            self.builder.build_struct_gep(variant_type, ptr, 1, "get_payload_ptr").unwrap();
        let payload_value = self.builder.build_load(
            variant_type.get_field_type_at_index(1).unwrap(),
            payload_ptr,
            "get_payload",
        );
        payload_value
    }

    fn codegen_function_call(&mut self, call: &Call) -> CodegenResult<LlvmValue<'ctx>> {
        let callee = self.module.get_function(call.callee_function_id);

        let function_value = self.codegen_function(call.callee_function_id, callee)?;

        let args: CodegenResult<Vec<BasicMetadataValueEnum<'ctx>>> = call
            .args
            .iter()
            .enumerate()
            .map(|(idx, arg_expr)| {
                let expected_type = callee.params[idx].type_id;
                let basic_value = self.codegen_expr_basic_value(arg_expr);
                trace!(
                    "codegen function call arg: {}: {} (expected: {}) \n {:?}",
                    self.module.expr_to_string(arg_expr),
                    self.module.type_id_to_string(arg_expr.get_type()),
                    self.module.type_id_to_string(expected_type),
                    basic_value
                );
                basic_value.map(|bv| bv.into())
            })
            .collect();
        let callsite_value = self.builder.build_call(function_value, &args?, "call_ret");
        match callsite_value.try_as_basic_value() {
            either::Either::Left(value) => Ok(LlvmValue::BasicValue(value)),
            either::Either::Right(_instr) => {
                if call.ret_type == NEVER_TYPE_ID {
                    let unreachable = self.builder.build_unreachable();
                    Ok(LlvmValue::Never(unreachable))
                } else {
                    panic!("Function returned LLVM void but wasn't typed as never")
                }
            }
        }
    }

    fn codegen_string_index_operation(
        &mut self,
        operation: &IndexOp,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let string_value = self.codegen_expr_basic_value(&operation.base_expr)?;
        let index_value = self.codegen_expr_basic_value(&operation.index_expr)?;

        let string_value = string_value.as_basic_value_enum().into_struct_value();
        let data_ptr = self.builtin_types.string_data(&self.builder, string_value);

        let index_int_value = index_value.into_int_value();
        let character_pointer = unsafe {
            self.builder.build_gep(
                self.builtin_types.char,
                data_ptr,
                &[index_int_value],
                "string_index_ptr",
            )
        };
        Ok(character_pointer)
    }

    fn codegen_array_index_operation(
        &mut self,
        operation: &IndexOp,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointee_ty = self.codegen_type(operation.result_type)?;
        // TOOD: Once codegen_expr returns type info, we can ditch this extra work to get and generate types separately
        let array_type = self.create_array_type(pointee_ty.value_basic_type());
        let array_value = self.codegen_expr_basic_value(&operation.base_expr)?.into_pointer_value();
        let index_value = self.codegen_expr_basic_value(&operation.index_expr)?;

        let array_struct =
            self.builder.build_load(array_type, array_value, "array_value").into_struct_value();
        let index_int_value = index_value.into_int_value();
        // Likely we need one more level of dereferencing
        let array_data = self.builtin_types.array_data(&self.builder, array_struct);
        let gep_ptr = unsafe {
            self.builder.build_gep(
                pointee_ty.value_basic_type(),
                array_data,
                &[index_int_value],
                "array_index_ptr",
            )
        };
        // We want to return a pointer to the element, not the element itself
        // This is usually so that we can assign to the element
        Ok(gep_ptr)
    }

    fn make_string(&self, array_len: IntValue<'ctx>) -> StructValue<'ctx> {
        let data_ptr = self
            .builder
            .build_array_malloc(self.builtin_types.char, array_len, "string_data")
            .unwrap();
        let string_type = self.builtin_types.string_struct;
        let string_struct = self
            .builder
            .build_insert_value(
                string_type.get_undef(),
                array_len,
                STRING_LENGTH_FIELD_INDEX,
                "string_len",
            )
            .unwrap();
        let string_struct = self
            .builder
            .build_insert_value(string_struct, data_ptr, STRING_DATA_FIELD_INDEX, "string_data")
            .unwrap();
        // 63 = ascii '?' character to detect uninitialized memory
        self.builder
            .build_memset(data_ptr, 1, self.builtin_types.char.const_int(63, false), array_len)
            .unwrap();
        string_struct.as_basic_value_enum().into_struct_value()
    }

    #[allow(unused)]
    fn const_string_ptr(&self, string: &str, name: &str) -> PointerValue<'ctx> {
        let char_data = self.ctx.const_string(string.as_bytes(), false);
        let length_value = self.builtin_types.i64.const_int(string.len() as u64, false);
        let char_data_global =
            self.llvm_module.add_global(char_data.get_type(), None, &format!("{name}_data"));
        char_data_global.set_initializer(&char_data);
        let string_struct = self.builtin_types.string_struct.const_named_struct(&[
            length_value.as_basic_value_enum(),
            char_data_global.as_pointer_value().as_basic_value_enum(),
        ]);
        let g = self.llvm_module.add_global(self.builtin_types.string_struct, None, name);
        g.set_initializer(&string_struct);
        g.as_pointer_value()
    }

    #[allow(unused)]
    fn const_string_loaded(&self, string: &str, name: &str) -> StructValue<'ctx> {
        let ptr = self.const_string_ptr(string, name);
        self.builder
            .build_load(self.builtin_types.string_struct, ptr, &format!("{name}_loaded"))
            .into_struct_value()
    }

    fn make_array(
        &mut self,
        array_len: IntValue<'ctx>,
        capacity: IntValue<'ctx>,
        element_type: BasicTypeEnum<'ctx>,
        zero_initialize: bool,
    ) -> StructValue<'ctx> {
        // First, we allocate memory somewhere not on the stack
        let data_ptr =
            self.builder.build_array_malloc(element_type, capacity, "array_data").unwrap();
        if zero_initialize {
            let size_bytes = self.builder.build_int_mul(
                capacity,
                element_type.size_of().unwrap(),
                "memset_bytes",
            );
            self.builder
                .build_memset(data_ptr, 1, self.builtin_types.char.const_zero(), size_bytes)
                .unwrap();
        }
        let array_type = self.create_array_type(element_type);
        // insert_value returns a value and takes a value, doesn't modify memory at pointers
        // We can start building a struct by giving it an undefined struct first
        let array_struct = self
            .builder
            .build_insert_value(array_type.get_undef(), array_len, 0, "array_len")
            .unwrap();
        let array_struct =
            self.builder.build_insert_value(array_struct, capacity, 1, "array_cap").unwrap();
        let array_struct =
            self.builder.build_insert_value(array_struct, data_ptr, 2, "array_data").unwrap();
        array_struct.into_struct_value()
    }

    fn get_loaded_variable(&mut self, variable_id: VariableId) -> BasicValueEnum<'ctx> {
        self.variables.get(&variable_id).unwrap().loaded_value(&self.builder)
    }

    fn codegen_intrinsic(
        &mut self,
        intrinsic_type: IntrinsicFunctionType,
        function: &TypedFunction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match intrinsic_type {
            IntrinsicFunctionType::Exit => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.builder.build_call(self.libc_functions.exit, &[first_arg.into()], "exit");
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunctionType::PrintInt => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.build_print_int_call(first_arg);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunctionType::PrintString => {
                let string_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_struct_value();
                self.build_print_string_call(string_arg);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunctionType::StringLength => {
                let string_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_struct_value();
                let length = self.builtin_types.string_length(&self.builder, string_arg);
                Ok(length.as_basic_value_enum())
            }
            IntrinsicFunctionType::StringFromCharArray => {
                let array =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let array_type =
                    self.create_array_type(self.builtin_types.char.as_basic_type_enum());
                let array_struct =
                    self.builder.build_load(array_type, array, "array_struct").into_struct_value();
                let array_len = self.builtin_types.array_length(&self.builder, array_struct);
                let string = self.make_string(array_len);
                let string_data = self.builtin_types.string_data(&self.builder, string);
                let array_data = self.builtin_types.array_data(&self.builder, array_struct);
                let _copied =
                    self.builder.build_memcpy(string_data, 1, array_data, 1, array_len).unwrap();
                Ok(string.as_basic_value_enum())
            }
            IntrinsicFunctionType::StringEquals => {
                let string1 =
                    self.get_loaded_variable(function.params[0].variable_id).into_struct_value();
                let string2 =
                    self.get_loaded_variable(function.params[1].variable_id).into_struct_value();
                // self.build_print_string_call(self.const_string("string_eq_fn\n"));
                let string1len = self.builtin_types.string_length(&self.builder, string1);
                let string2len = self.builtin_types.string_length(&self.builder, string2);
                let len_eq = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    string1len,
                    string2len,
                    "len_eq",
                );
                let result = self.builder.build_alloca(self.builtin_types.boolean, "result");
                self.builder.build_store(result, self.builtin_types.false_value);

                // If lengths are equal, go to mem compare
                let compare_finish_branch =
                    self.build_conditional_branch(len_eq, "compare", "finish");
                let compare_block = compare_finish_branch.then_block;
                let finish_block = compare_finish_branch.else_block;

                self.builder.position_at_end(compare_block);
                let len_is_zero = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    string1len,
                    self.builtin_types.i64.const_zero(),
                    "len_is_zero",
                );
                let compare_cases_branch =
                    self.build_conditional_branch(len_is_zero, "empty_strings_case", "memcmp_case");
                let empty_strings_block = compare_cases_branch.then_block;
                let memcmp_block = compare_cases_branch.else_block;

                self.builder.position_at_end(empty_strings_block);
                // self.build_print_string_call(self.const_string("empty strings case\n"));
                self.builder.build_store(result, self.builtin_types.true_value);
                self.builder.build_unconditional_branch(finish_block);

                self.builder.position_at_end(memcmp_block);
                // self.build_print_string_call(self.const_string("memcmp case\n"));
                let string1data = self.builtin_types.string_data(&self.builder, string1);
                let string2data = self.builtin_types.string_data(&self.builder, string2);
                let memcmp_result_i32 = self
                    .builder
                    .build_call(
                        self.libc_functions.memcmp,
                        &[string1data.into(), string2data.into(), string1len.into()],
                        "data_eq",
                    )
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();
                let is_zero = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    memcmp_result_i32,
                    self.ctx.i32_type().const_zero(),
                    "is_zero",
                );
                self.builder.build_store(result, is_zero);
                self.builder.build_unconditional_branch(finish_block);

                self.builder.position_at_end(finish_block);
                // self.build_print_string_call(self.const_string("finish_block"));
                Ok(self
                    .builder
                    .build_load(self.builtin_types.boolean, result, "string_eq_result_value")
                    .as_basic_value_enum())
            }
            IntrinsicFunctionType::ArrayLength => {
                let array_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let array_type: LlvmPointerType =
                    self.codegen_type(function.params[0].type_id)?.expect_pointer();
                let array_struct = self
                    .builder
                    .build_load(
                        array_type.pointee_type.into_struct_type(),
                        array_arg,
                        "array_struct",
                    )
                    .into_struct_value();
                let length = self.builtin_types.array_length(&self.builder, array_struct);
                Ok(length.as_basic_value_enum())
            }
            IntrinsicFunctionType::ArrayNew => {
                let array_type_id = function.ret_type;
                let array_type = self.module.types.get(array_type_id);
                let element_type = self.codegen_type(array_type.expect_array().element_type)?;
                let len = self.get_loaded_variable(function.params[0].variable_id).into_int_value();

                // TODO: Use ctlz intrinsic to count leading zeroes and get next highest
                //       power of 2 for capacity
                // let ctlz_intrinsic = inkwell::intrinsics::Intrinsic::find("llvm.ctlz").unwrap();
                // let ctlz_function = ctlz_intrinsic.get_declaration(&self.llvm_module, &[self.ctx.i64_type().as_basic_type_enum()]).unwrap();
                // let capacity = self.builder.build_call(ctlz_function, &[], "capacity");
                let capacity = len;
                let array_struct =
                    self.make_array(len, capacity, element_type.value_basic_type(), true);
                let array_ptr =
                    self.builder.build_malloc(array_struct.get_type(), "array_ptr").unwrap();
                self.builder.build_store(array_ptr, array_struct);
                Ok(array_ptr.as_basic_value_enum())
            }
            IntrinsicFunctionType::ArrayGrow => {
                // We need to resize the array and copy the elements into the new memory
                let self_param = &function.params[0];
                let array_ptr =
                    self.get_loaded_variable(self_param.variable_id).into_pointer_value();
                let array_type = self
                    .codegen_type(self_param.type_id)?
                    .expect_pointer()
                    .pointee_type
                    .into_struct_type();
                let array =
                    self.builder.build_load(array_type, array_ptr, "array").into_struct_value();
                let cap = self.builtin_types.build_array_capacity(&self.builder, array);
                let cap_is_zero = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    cap,
                    self.builtin_types.i64.const_zero(),
                    "cap_is_zero",
                );
                let cap_times_two = self.builder.build_int_mul(
                    cap,
                    self.builtin_types.i64.const_int(2, true),
                    "new_cap",
                );
                let new_cap = self
                    .builder
                    .build_select(
                        cap_is_zero,
                        self.builtin_types.i64.const_int(1, true),
                        cap_times_two,
                        "new_cap",
                    )
                    .into_int_value();
                // self.build_print_string_call(self.const_string("growing array\n"));
                let old_data = self.builtin_types.array_data(&self.builder, array);
                let element_type_id =
                    self.module.types.get(self_param.type_id).expect_array().element_type;
                let new_data_type = self.codegen_type(element_type_id)?;
                let new_data = self
                    .builder
                    .build_array_malloc(new_data_type.value_basic_type(), new_cap, "new_data")
                    .unwrap();
                let memcpy_bytes = self.builder.build_int_mul(
                    cap,
                    new_data_type.value_basic_type().size_of().unwrap(),
                    "memcpy_bytes",
                );
                let _copied =
                    self.builder.build_memcpy(new_data, 1, old_data, 1, memcpy_bytes).unwrap();
                let array = self
                    .builder
                    .build_insert_value(array, new_data, ARRAY_DATA_FIELD_INDEX, "new_array_data")
                    .unwrap();
                let array = self
                    .builder
                    .build_insert_value(array, new_cap, ARRAY_CAPACITY_FIELD_INDEX, "new_array_cap")
                    .unwrap();
                self.builder.build_store(array_ptr, array);
                self.builder.build_free(old_data);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunctionType::ArrayCapacity => {
                let array_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let array_struct_type = self
                    .codegen_type(function.params[0].type_id)?
                    .expect_pointer()
                    .pointee_type
                    .into_struct_type();
                let array_value = self
                    .builder
                    .build_load(array_struct_type, array_arg, "array_value")
                    .into_struct_value();
                let capacity_value = self
                    .builtin_types
                    .build_array_capacity(&self.builder, array_value)
                    .as_basic_value_enum();
                Ok(capacity_value)
            }
            IntrinsicFunctionType::ArraySetLength => {
                let array_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_pointer_value();
                let array_struct_type = self
                    .codegen_type(function.params[0].type_id)?
                    .expect_pointer()
                    .pointee_type
                    .into_struct_type();
                let array_value = self
                    .builder
                    .build_load(array_struct_type, array_arg, "array_value")
                    .into_struct_value();
                let new_len =
                    self.get_loaded_variable(function.params[1].variable_id).into_int_value();
                let updated_array = self
                    .builder
                    .build_insert_value(
                        array_value,
                        new_len,
                        ARRAY_LENGTH_FIELD_INDEX,
                        "set_length",
                    )
                    .unwrap();
                self.builder.build_store(array_arg, updated_array);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
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
    fn codegen_block_statements(&mut self, block: &TypedBlock) -> CodegenResult<LlvmValue<'ctx>> {
        let unit_value = self.builtin_types.unit_value.as_basic_value_enum().into();
        let mut last: LlvmValue<'ctx> = unit_value;
        self.set_debug_location(block.span);
        for stmt in &block.statements {
            match stmt {
                TypedStmt::Expr(expr) => last = self.codegen_expr(expr)?,
                TypedStmt::ValDef(val_def) => {
                    let _value = self.codegen_val(val_def);
                    last = unit_value;
                }
                TypedStmt::Assignment(assignment) => {
                    match assignment.destination.deref() {
                        // ASSIGNMENT! We're in lvalue land. We need to get the pointer to the
                        // destination, and be sure to call the correct variant of codegen_expr
                        TypedExpr::Variable(v) => {
                            let destination_ptr =
                                *self.variables.get(&v.variable_id).expect("Missing variable");
                            let initializer = self.codegen_expr_basic_value(&assignment.value)?;
                            self.builder.build_store(destination_ptr.pointer, initializer);

                            last = unit_value;
                        }
                        TypedExpr::StructFieldAccess(_field_access) => {
                            // We use codegen_expr_lvalue to get the pointer to the accessed field
                            let field_ptr = self.codegen_expr_lvalue(&assignment.destination)?;
                            let rhs = self.codegen_expr_basic_value(&assignment.value)?;
                            self.builder.build_store(field_ptr, rhs);
                            last = unit_value
                        }
                        TypedExpr::ArrayIndex(_array_index) => {
                            // We use codegen_expr_lvalue to get the pointer to the accessed element
                            let elem_ptr = self.codegen_expr_lvalue(&assignment.destination)?;
                            let rhs = self.codegen_expr_basic_value(&assignment.value)?;
                            self.builder.build_store(elem_ptr, rhs);
                            last = unit_value
                        }
                        _ => {
                            panic!("Invalid assignment lhs")
                        }
                    }
                }
                TypedStmt::WhileLoop(while_stmt) => {
                    let start_block = self.builder.get_insert_block().unwrap();
                    let current_fn = start_block.get_parent().unwrap();
                    let loop_entry_block = self.ctx.append_basic_block(current_fn, "while_cond");
                    let loop_body_block = self.ctx.append_basic_block(current_fn, "while_body");
                    let loop_end_block = self.ctx.append_basic_block(current_fn, "while_end");

                    // Go to the loop
                    self.builder.build_unconditional_branch(loop_entry_block);

                    self.builder.position_at_end(loop_entry_block);
                    let cond = self.codegen_expr_basic_value(&while_stmt.cond)?.into_int_value();
                    let cond_i1 = self.bool_to_i1(cond, "while_cond");

                    self.builder.build_conditional_branch(cond_i1, loop_body_block, loop_end_block);

                    self.builder.position_at_end(loop_body_block);
                    self.codegen_block_statements(&while_stmt.block)?;
                    self.builder.build_unconditional_branch(loop_entry_block);

                    self.builder.position_at_end(loop_end_block);
                }
            }
        }
        Ok(last)
    }

    fn push_function_debug_info(
        &mut self,
        function: &TypedFunction,
        return_type: &LlvmType<'ctx>,
        param_types: &[LlvmType<'ctx>],
    ) -> CodegenResult<DISubprogram<'ctx>> {
        let return_type = return_type.debug_type();
        let dbg_param_types =
            &param_types.iter().map(|param_type| param_type.debug_type()).collect::<Vec<_>>();
        let span = self.module.ast.spans.get(function.span);
        let function_line_number =
            self.module.ast.sources.get_line_for_span(span).expect("line for span").line_index + 1;
        let function_scope_start_line_number = function_line_number;
        let function_file = self.debug.files.get(&span.file_id).unwrap();
        let dbg_fn_type = self.debug.debug_builder.create_subroutine_type(
            *function_file,
            Some(return_type),
            dbg_param_types,
            0,
        );
        let di_subprogram = self.debug.debug_builder.create_function(
            self.debug.current_scope(),
            &self.module.ast.identifiers.get_name(function.name),
            None,
            *function_file,
            function_line_number,
            dbg_fn_type,
            false,
            true,
            function_scope_start_line_number,
            0,
            false,
        );

        self.debug.push_scope(di_subprogram.as_debug_info_scope(), *function_file);
        Ok(di_subprogram)
    }

    fn codegen_function(
        &mut self,
        function_id: FunctionId,
        function: &TypedFunction,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        trace!("codegen function\n{}", self.module.function_id_to_string(function_id, true));

        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(*function);
        }
        let function_line_number = self
            .module
            .ast
            .sources
            .get_line_for_span(self.module.ast.spans.get(function.span))
            .expect("line for span")
            .line_index
            + 1;
        if function.is_generic() {
            panic!("Cannot codegen generic function: {}", &*self.get_ident_name(function.name));
        }

        let maybe_starting_block = self.builder.get_insert_block();
        let param_types: CodegenResult<Vec<LlvmType<'ctx>>> =
            function.params.iter().map(|fn_arg| self.codegen_type(fn_arg.type_id)).collect();
        let param_types = param_types?;
        let param_metadata_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            param_types.iter().map(|llvm_type| llvm_type.value_basic_type().into()).collect();
        let ret_type = self.codegen_type(function.ret_type)?;

        let fn_ty = match &ret_type {
            LlvmType::Void(v) => v.void_type.fn_type(&param_metadata_types, false),
            _ => ret_type.value_basic_type().fn_type(&param_metadata_types, false),
        };
        let llvm_linkage = match function.linkage {
            TyperLinkage::Standard => None,
            TyperLinkage::External => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        let fn_val = {
            let name = self.module.ast.identifiers.get_name(function.name);
            self.llvm_module.add_function(&name, fn_ty, llvm_linkage)
        };

        self.llvm_functions.insert(function_id, fn_val);

        if function.linkage == Linkage::External {
            return Ok(fn_val);
        }

        let di_subprogram = self.push_function_debug_info(function, &ret_type, &param_types)?;

        let entry_block = self.ctx.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry_block);
        for (i, param) in fn_val.get_param_iter().enumerate() {
            let typed_param = &function.params[i];
            let ty = self.codegen_type(typed_param.type_id)?;
            let param_name = self.module.ast.identifiers.get_name(typed_param.name);
            trace!(
                "Got LLVM type for variable {}: {} (from {})",
                &*param_name,
                ty.value_basic_type(),
                self.module.type_id_to_string(typed_param.type_id)
            );
            param.set_name(&param_name);
            self.set_debug_location(typed_param.span);
            let pointer = self.builder.build_alloca(ty.value_basic_type(), &param_name);
            let arg_debug_type = self.get_debug_type(typed_param.type_id)?;
            let di_local_variable = self.debug.debug_builder.create_parameter_variable(
                self.debug.current_scope(),
                &param_name,
                typed_param.position,
                self.debug.current_file(),
                function_line_number,
                arg_debug_type,
                true,
                0,
            );
            let store = self.builder.build_store(pointer, param);
            self.debug.debug_builder.insert_declare_before_instruction(
                pointer,
                Some(di_local_variable),
                None,
                self.set_debug_location(typed_param.span),
                store,
            );
            self.variables.insert(
                typed_param.variable_id,
                Pointer {
                    pointer,
                    pointee_type_id: typed_param.type_id,
                    pointee_llvm_type: ty.value_basic_type(),
                },
            );
        }
        match function.intrinsic_type {
            Some(intrinsic_type) => {
                trace!("codegen intrinsic {:?} fn {:?}", intrinsic_type, function);
                let value = self.codegen_intrinsic(intrinsic_type, function)?.as_basic_value_enum();
                self.builder.build_return(Some(&value));
            }
            None => {
                // The plan is to separate the notion of "expression blocks" from "control flow
                // blocks" to make this all more reasonable
                // Rust rejects "unreachable expression"
                let function_block = function.block.as_ref().unwrap_or_else(|| {
                    panic!("Function has no block {}", &*self.get_ident_name(function.name))
                });
                let value = self.codegen_block_statements(function_block)?;
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    if ret_type.is_void() {
                        self.builder.build_return(None);
                    } else {
                        match value {
                            LlvmValue::BasicValue(basic_value) => {
                                self.builder.build_return(Some(&basic_value))
                            }
                            LlvmValue::Never(_instr) => self.builder.build_unreachable(),
                            // LlvmValue::Void(_instr) => self.builder.build_return(None),
                        };
                    }
                }
            }
        };
        if let Some(start_block) = maybe_starting_block {
            self.builder.position_at_end(start_block);
        }
        #[allow(clippy::single_match)]
        match function.intrinsic_type {
            // Workaround: ArrayNew returns a pointer to an alloca, so it needs to be inlined
            Some(IntrinsicFunctionType::ArrayNew) => {
                // Always inline
                fn_val.add_attribute(
                    AttributeLoc::Function,
                    self.ctx.create_string_attribute("alwaysinline", "true"),
                );
            }
            _ => {}
        }
        self.debug.pop_scope();
        fn_val.set_subprogram(di_subprogram);
        Ok(fn_val)
    }

    pub fn codegen_module(&mut self) -> CodegenResult<()> {
        let start = std::time::Instant::now();
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
        for (id, function) in self.module.function_iter() {
            if function.should_codegen() {
                self.codegen_function(id, function)?;
            }
        }
        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    pub fn optimize(&mut self, optimize: bool) -> anyhow::Result<()> {
        let start = std::time::Instant::now();
        Target::initialize_aarch64(&InitializationConfig::default());
        // let triple = TargetMachine::get_default_triple();
        let triple = TargetTriple::create("arm64-apple-macosx14.4.0");
        let target = Target::from_triple(&triple).unwrap();
        if !self.debug.strip_debug {
            self.debug.debug_builder.finalize();
        } else {
            self.llvm_module.strip_debug_info();
        }
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

        let module_pass_manager: PassManager<LlvmModule<'ctx>> = PassManager::create(());
        if optimize {
            module_pass_manager.add_cfg_simplification_pass();
            module_pass_manager.add_promote_memory_to_register_pass();
            module_pass_manager.add_instruction_combining_pass();
        }

        // Workaround: We have to inline always because ArrayNew returns a pointer to an alloca
        module_pass_manager.add_function_inlining_pass();

        module_pass_manager.add_function_attrs_pass();
        module_pass_manager.add_verifier_pass();

        module_pass_manager.run_on(&self.llvm_module);

        machine.add_analysis_passes(&module_pass_manager);

        self.llvm_machine = Some(machine);

        info!("codegen phase 'optimize' took {}ms", start.elapsed().as_millis());

        Ok(())
    }

    #[allow(unused)]
    pub fn emit_object_file(&self, rel_destination_dir: &str) -> anyhow::Result<()> {
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

    pub fn interpret_module(&self) -> anyhow::Result<u64> {
        let engine = self.llvm_module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let bfllib_module = self
            .ctx
            .create_module_from_ir(
                MemoryBuffer::create_from_file(Path::new("bfllib/bfllib.ll")).unwrap(),
            )
            .unwrap();
        self.llvm_module.link_in_module(bfllib_module).unwrap();
        let Some(main_fn_id) = self.module.get_main_function_id() else {
            bail!("No main function")
        };
        let llvm_function = self.llvm_functions.get(&main_fn_id).unwrap();
        eprintln!("Interpreting {}", self.module.name());
        let return_value = unsafe { engine.run_function(*llvm_function, &[]) };
        let res: u64 = return_value.as_int(true);
        Ok(res)
    }

    fn get_value_for_tag_type(&self, identifier_id: IdentifierId) -> IntValue<'ctx> {
        let global = self.tag_type_mappings.get(&identifier_id).expect("tag type mapping value");
        let ptr = global.as_pointer_value();
        self.builder.build_load(self.builtin_types.tag_type, ptr, "tag_value").into_int_value()
    }
}
