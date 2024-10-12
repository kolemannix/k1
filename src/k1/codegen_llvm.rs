use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::path::Path;

use anyhow::bail;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DILocation, DIScope, DISubprogram, DIType, DWARFEmissionKind,
    DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType, PointerType,
    StructType, VoidType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
    InstructionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel};
use log::{debug, info, trace};

use crate::lex::SpanId;
use crate::parse::{FileId, Identifier, NumericWidth};
use crate::typer::scopes::ScopeId;
use crate::typer::types::*;
use crate::typer::{Linkage as TyperLinkage, *};

const STRING_LENGTH_FIELD_INDEX: u32 = 0;
const STRING_DATA_FIELD_INDEX: u32 = 1;

const WORD_SIZE_BITS: u64 = 64;

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
    pub span: SpanId,
}

macro_rules! err {
    ($span:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            Err(CodegenError {
                message: s,
                span: $span,
            })
        }
    };
}

type CodegenResult<T> = Result<T, CodegenError>;

impl Display for CodegenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("error in span {:?}: {}", self.span, self.message))
    }
}

impl Error for CodegenError {}

fn size_info(td: &TargetData, typ: &dyn AnyType) -> SizeInfo {
    SizeInfo {
        size_bits: td.get_bit_size(typ) as u32,
        align_bits: td.get_preferred_alignment(typ) * 8,
    }
}

#[derive(Debug, Copy, Clone)]
struct BranchSetup<'ctx> {
    then_block: BasicBlock<'ctx>,
    else_block: BasicBlock<'ctx>,
}

struct StructDebugMember<'ctx, 'name> {
    name: &'name str,
    di_type: DIType<'ctx>,
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

#[derive(Debug, Clone, Copy)]
pub struct SizeInfo {
    size_bits: u32,
    align_bits: u32,
}
impl SizeInfo {
    pub const POINTER: SizeInfo =
        SizeInfo { size_bits: WORD_SIZE_BITS as u32, align_bits: WORD_SIZE_BITS as u32 };
    pub const ZERO: SizeInfo = SizeInfo { size_bits: 0, align_bits: 0 };

    /// https://learn.microsoft.com/en-us/cpp/c-language/alignment-c?view=msvc-170
    /// struct and union types have an alignment equal to the largest alignment of any member.
    /// Padding bytes are added within a struct to ensure individual member alignment requirements are met.
    ///
    /// THIS FUNCTION DOESNT INSERT PADDING SO THE SIZE OF STRUCTS WILL NOT
    /// MATCH LLVMS.
    #[deprecated(
        since = "0.1.0",
        note = "This function does not insert padding, so the size of structs will not match LLVM's. Resurrect this if we do our own backend"
    )]
    pub fn compute_size_of_aggregate(sizes: &[SizeInfo]) -> SizeInfo {
        let mut max_align = 0;
        let mut total_size = 0;
        for size in sizes {
            total_size += size.size_bits;
            if size.align_bits > max_align {
                max_align = size.align_bits
            }
        }
        SizeInfo { size_bits: total_size, align_bits: max_align }
    }

    pub fn scalar_value(size_bits: u32) -> SizeInfo {
        SizeInfo { size_bits, align_bits: size_bits }
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
    size: SizeInfo,
}

#[derive(Debug, Clone, Copy)]
struct EnumVariantStructType<'ctx> {
    name: Identifier,
    struct_type: StructType<'ctx>,
    tag_value: IntValue<'ctx>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

#[derive(Debug, Clone)]
struct LlvmEnumType<'ctx> {
    type_id: TypeId,
    #[allow(unused)]
    tag_type: IntType<'ctx>,
    base_struct_type: StructType<'ctx>,
    variant_structs: Vec<EnumVariantStructType<'ctx>>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

#[derive(Debug, Clone)]
struct LlvmStructType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    #[allow(unused)]
    fields: Vec<LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
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
    #[allow(unused)]
    pub fn size_info(&self) -> SizeInfo {
        match self {
            LlvmType::Value(v) => v.size,
            LlvmType::EnumType(e) => e.size,
            LlvmType::StructType(s) => s.size,
            LlvmType::Pointer(_p) => SizeInfo::POINTER,
            LlvmType::Void(_) => SizeInfo::ZERO,
        }
    }

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
    int: IntType<'ctx>,
    unit: IntType<'ctx>,
    unit_value: IntValue<'ctx>,
    boolean: IntType<'ctx>,
    true_value: IntValue<'ctx>,
    false_value: IntValue<'ctx>,
    i1: IntType<'ctx>,
    char: IntType<'ctx>,
    c_str: PointerType<'ctx>,
    ptr: PointerType<'ctx>,
    ptr_sized_int: IntType<'ctx>,
}

impl<'ctx> BuiltinTypes<'ctx> {
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
        debug_assert!(size_bits % 8 == 0);
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
    llvm_machine: TargetMachine,
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
            "k1_compiler",
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
        let mut llvm_module = ctx.create_module(&module.ast.name);
        llvm_module.set_source_file_name(&module.ast.sources.get_main().filename);
        // Example of linking an LLVM module
        // let stdlib_module = ctx
        //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("nxlib/llvm")).unwrap())
        //     .unwrap();
        // llvm_module.link_in_module(stdlib_module).unwrap();

        let debug_context = Codegen::init_debug(ctx, &llvm_module, module, optimize, debug);

        let machine = Codegen::set_up_machine(&mut llvm_module);
        let target_data = machine.get_target_data();

        let pointers = HashMap::new();
        let format_int_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(4), None, "formatInt");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%li\0"));
            global
        };
        let format_uint_str = {
            let global = llvm_module.add_global(ctx.i8_type().array_type(5), None, "formatUInt");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            global.set_initializer(&i8_array_from_str(ctx, "%llu\0"));
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
        builtin_globals.insert("formatUInt".to_string(), format_uint_str);
        builtin_globals.insert("formatString".to_string(), format_str_str);
        let int = ctx.i64_type();

        let builtin_types = BuiltinTypes {
            ctx,
            int,
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
            // It doesn't matter what type the pointer points to; its irrelevant in LLVM
            // since pointers do not actually have types
            ptr: ctx.i64_type().ptr_type(AddressSpace::default()),
            ptr_sized_int: ctx.ptr_sized_int_type(&target_data, None),
        };

        let printf_type = ctx.i32_type().fn_type(&[builtin_types.c_str.into()], true);
        let printf = llvm_module.add_function("printf", printf_type, Some(LlvmLinkage::External));
        let exit = llvm_module.add_function(
            "exit",
            ctx.void_type().fn_type(&[builtin_types.int.into()], false),
            Some(LlvmLinkage::External),
        );
        let byte_ptr = ctx.i8_type().ptr_type(AddressSpace::default());
        let memcmp = llvm_module.add_function(
            "memcmp",
            ctx.i32_type()
                .fn_type(&[byte_ptr.into(), byte_ptr.into(), ctx.i64_type().into()], false),
            Some(LlvmLinkage::External),
        );
        Codegen {
            ctx,
            module,
            llvm_module,
            llvm_machine: machine,
            builder,
            variables: pointers,
            globals,
            llvm_functions: HashMap::new(),
            llvm_types: RefCell::new(HashMap::new()),
            libc_functions: LibcFunctions { printf, exit, memcmp },
            builtin_globals,
            builtin_types,
            debug: debug_context,
        }
    }

    fn size_info(&self, typ: &dyn AnyType) -> SizeInfo {
        let td = self.llvm_machine.get_target_data();
        size_info(&td, typ)
    }

    fn offset_of_struct_member(&self, typ: &StructType, field_index: u32) -> u64 {
        let td = self.llvm_machine.get_target_data();
        let offset_bytes = td.offset_of_element(typ, field_index).unwrap();
        offset_bytes * 8
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

    fn get_ident_name(&self, id: Identifier) -> &str {
        self.module.ast.identifiers.get_name(id)
    }

    fn build_print_int_call(
        &mut self,
        int_value: BasicValueEnum<'ctx>,
        signed: bool,
    ) -> BasicValueEnum<'ctx> {
        // Note: These 'builtin globals' should be a struct not a hashmap because its all static currently
        let format_str_global = if signed {
            self.builtin_globals.get("formatInt").unwrap()
        } else {
            self.builtin_globals.get("formatUInt").unwrap()
        };
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

    fn get_line_number(&self, span: SpanId) -> u32 {
        let span = self.module.ast.spans.get(span);
        let line = self.module.ast.sources.get_line_for_span(span).expect("No line for span");
        line.line_index + 1
    }

    fn make_debug_struct_type<'names>(
        &self,
        name: &str,
        struct_type: &StructType,
        span: SpanId,
        field_types: &[StructDebugMember<'ctx, 'names>],
    ) -> DIType<'ctx> {
        let line_number = self.get_line_number(span);
        let size = self.size_info(struct_type);
        let fields = &field_types
            .iter()
            .enumerate()
            .map(|(member_index, debug_member)| {
                let offset = self.offset_of_struct_member(struct_type, member_index as u32);
                let t = self.debug.debug_builder.create_member_type(
                    self.debug.current_scope(),
                    debug_member.name,
                    self.debug.current_file(),
                    line_number,
                    debug_member.di_type.get_size_in_bits(),
                    debug_member.di_type.get_align_in_bits(),
                    offset,
                    0,
                    debug_member.di_type,
                );
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
                size.size_bits as u64,
                size.align_bits,
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
        let result = self.llvm_types.borrow().get(&type_id).cloned();
        if let Some(result) = result {
            return Ok(result);
        };
        self.codegen_type_inner(type_id, 0)
    }

    fn codegen_type_inner(&self, type_id: TypeId, depth: usize) -> CodegenResult<LlvmType<'ctx>> {
        let _dw_ate_address = 0x01;
        let dw_ate_boolean = 0x02;
        let _dw_ate_complex_float = 0x03;
        let dw_ate_float = 0x04;
        let dw_ate_signed = 0x05;
        let dw_ate_char = 0x06;
        let dw_ate_unsigned = 0x07;
        let _dw_ate_unsigned_char = 0x08;
        let make_value_integer_type =
            |name: &str,
             type_id: TypeId,
             int_type: IntType<'ctx>,
             encoding: llvm_sys::debuginfo::LLVMDWARFTypeEncoding| {
                let size = self.size_info(&int_type);
                LlvmType::Value(LlvmValueType {
                    type_id,
                    basic_type: int_type.as_basic_type_enum(),
                    size,
                    di_type: self
                        .debug
                        .debug_builder
                        .create_basic_type(name, size.size_bits as u64, encoding, 0)
                        .unwrap()
                        .as_type(),
                })
            };
        debug!("codegen for type {}", self.module.type_id_to_string(type_id));
        let mut no_cache = false;
        let span = self.module.get_span_for_type_id(type_id).unwrap_or(SpanId::NONE);
        let codegened_type = match self.module.types.get_no_follow(type_id) {
            Type::Unit(_) => Ok(make_value_integer_type(
                "unit",
                UNIT_TYPE_ID,
                self.builtin_types.unit,
                dw_ate_boolean,
            )),
            Type::Char(_) => Ok(make_value_integer_type(
                "char",
                CHAR_TYPE_ID,
                self.builtin_types.char,
                dw_ate_char,
            )),
            Type::Integer(IntegerType::U8) => {
                Ok(make_value_integer_type("u8", U8_TYPE_ID, self.ctx.i8_type(), dw_ate_unsigned))
            }
            Type::Integer(IntegerType::U16) => Ok(make_value_integer_type(
                "u16",
                U16_TYPE_ID,
                self.ctx.i16_type(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::U32) => Ok(make_value_integer_type(
                "u32",
                U32_TYPE_ID,
                self.ctx.i32_type(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::U64) => Ok(make_value_integer_type(
                "u64",
                U64_TYPE_ID,
                self.ctx.i64_type(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::I8) => {
                Ok(make_value_integer_type("i8", I8_TYPE_ID, self.ctx.i8_type(), dw_ate_signed))
            }
            Type::Integer(IntegerType::I16) => {
                Ok(make_value_integer_type("i16", I16_TYPE_ID, self.ctx.i16_type(), dw_ate_signed))
            }
            Type::Integer(IntegerType::I32) => {
                Ok(make_value_integer_type("i32", I32_TYPE_ID, self.ctx.i32_type(), dw_ate_signed))
            }
            Type::Integer(IntegerType::I64) => {
                Ok(make_value_integer_type("i64", I64_TYPE_ID, self.ctx.i64_type(), dw_ate_signed))
            }
            ft @ Type::Float(float_type) => {
                let llvm_type = match float_type.size {
                    NumericWidth::B8 => unreachable!("f8 is not a thing"),
                    NumericWidth::B16 => self.ctx.f16_type(),
                    NumericWidth::B32 => self.ctx.f32_type(),
                    NumericWidth::B64 => self.ctx.f64_type(),
                };
                let size = SizeInfo::scalar_value(float_type.size.bit_width());
                let float_name = self.module.type_to_string(ft, false);
                Ok(LlvmValueType {
                    type_id,
                    basic_type: llvm_type.as_basic_type_enum(),
                    size,
                    di_type: self
                        .debug
                        .debug_builder
                        .create_basic_type(&float_name, size.size_bits as u64, dw_ate_float, 0)
                        .unwrap()
                        .as_type(),
                }
                .into())
            }
            Type::Bool(_) => Ok(LlvmValueType {
                type_id: BOOL_TYPE_ID,
                basic_type: self.builtin_types.boolean.as_basic_type_enum(),
                size: self.size_info(&self.builtin_types.boolean),
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
            Type::Pointer(_) => {
                // We represent this as a LlvmValueType, not a LlvmPointerType, since its
                // our 'raw' pointer that is really a 'number' wrapper and has no target type
                // attached
                let placeholder_pointee = self.codegen_type(U64_TYPE_ID)?.debug_type();
                Ok(LlvmValueType {
                    type_id: POINTER_TYPE_ID,
                    basic_type: self.builtin_types.ptr.as_basic_type_enum(),
                    size: self.size_info(&self.builtin_types.ptr),
                    di_type: self
                        .debug
                        .debug_builder
                        .create_pointer_type(
                            "Pointer",
                            placeholder_pointee,
                            self.builtin_types.ptr_sized_int.get_bit_width() as u64,
                            WORD_SIZE_BITS as u32,
                            AddressSpace::default(),
                        )
                        .as_type(),
                }
                .into())
            }
            Type::Struct(struc) => {
                trace!("generating llvm type for struct type {type_id}");
                let field_count = struc.fields.len();
                let mut field_types = Vec::with_capacity(field_count);
                let mut field_basic_types = Vec::with_capacity(field_count);
                let name = struc
                    .type_defn_info
                    .as_ref()
                    .map(|info| self.get_ident_name(info.name).to_string())
                    .unwrap_or("<anon_struct>".to_string());
                for field in &struc.fields {
                    let field_llvm_type = self.codegen_type_inner(field.type_id, depth + 1)?;
                    field_basic_types.push(field_llvm_type.value_basic_type());
                    field_types.push(field_llvm_type);
                }

                let llvm_struct_type = self.ctx.struct_type(&field_basic_types, false);

                let mut field_di_types: Vec<StructDebugMember> = Vec::with_capacity(field_count);
                for (index, field_llvm_type) in field_types.iter().enumerate() {
                    let ident = struc.fields[index].name;
                    field_di_types.push(StructDebugMember {
                        name: self.module.ast.identifiers.get_name(ident),
                        di_type: field_llvm_type.debug_type(),
                    });
                }
                let size = self.size_info(&llvm_struct_type);
                let di_type =
                    self.make_debug_struct_type(&name, &llvm_struct_type, span, &field_di_types);
                Ok(LlvmValueType {
                    type_id,
                    basic_type: llvm_struct_type.as_basic_type_enum(),
                    size,
                    di_type,
                }
                .into())
            }
            Type::TypeVariable(v) => {
                err!(span, "codegen was asked to codegen a type variable {:?}", v)
            }
            Type::Reference(reference) => {
                // Could also use any_ptr here
                let inner_type = self.codegen_type_inner(reference.inner_type, depth + 1)?;
                let inner_debug_type = inner_type.debug_type();
                Ok(LlvmPointerType {
                    type_id,
                    pointer_type: inner_type
                        .value_basic_type()
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    pointee_type: inner_type.value_basic_type(),
                    di_type: self
                        .debug
                        .create_pointer_type(&format!("reference_{}", type_id), inner_debug_type),
                }
                .into())
            }
            Type::Enum(enum_type) => {
                let mut variant_structs = Vec::with_capacity(enum_type.variants.len());
                if enum_type.variants.len() >= u8::MAX as usize {
                    panic!("too many enum variants for now");
                }
                let tag_int_type =
                    self.codegen_type(U8_TYPE_ID)?.value_basic_type().into_int_type();
                let discriminant_field_debug = self
                    .debug
                    .debug_builder
                    .create_basic_type(
                        "tag",
                        tag_int_type.get_bit_width() as u64,
                        dw_ate_unsigned,
                        0,
                    )
                    .unwrap()
                    .as_type();
                for variant in enum_type.variants.iter() {
                    let (variant_struct, variant_struct_debug) = if let Some(payload_type_id) =
                        variant.payload
                    {
                        let variant_payload_type =
                            self.codegen_type_inner(payload_type_id, depth + 1)?;
                        let struc = self.ctx.struct_type(
                            &[
                                tag_int_type.as_basic_type_enum(),
                                variant_payload_type.value_basic_type(),
                            ],
                            false,
                        );
                        let debug_struct = self.make_debug_struct_type(
                            &format!(
                                "variant_{}",
                                self.module.ast.identifiers.get_name(variant.name)
                            ),
                            &struc,
                            SpanId::NONE,
                            &[
                                StructDebugMember {
                                    name: "tag",
                                    di_type: discriminant_field_debug,
                                },
                                StructDebugMember {
                                    name: "payload",
                                    di_type: variant_payload_type.debug_type(),
                                },
                            ],
                        );
                        (struc, debug_struct)
                    } else {
                        let s = self.ctx.struct_type(&[tag_int_type.as_basic_type_enum()], false);
                        let debug_struct = {
                            self.make_debug_struct_type(
                                &format!(
                                    "variant_{}",
                                    self.module.ast.identifiers.get_name(variant.name)
                                ),
                                &s,
                                SpanId::NONE,
                                &[StructDebugMember {
                                    name: "tag",
                                    di_type: discriminant_field_debug,
                                }],
                            )
                        };
                        (s, debug_struct)
                    };
                    variant_structs.push(EnumVariantStructType {
                        name: variant.name,
                        struct_type: variant_struct,
                        tag_value: tag_int_type.const_int(variant.index as u64, false),
                        di_type: variant_struct_debug,
                        size: self.size_info(&variant_struct),
                    });
                }

                let largest_variant_size =
                    variant_structs.iter().map(|v| v.size.size_bits).max().unwrap();

                // Now that we know the full size, go back and pad each variant struct
                for (index, variant_struct) in variant_structs.iter_mut().enumerate() {
                    let size_diff_bits = largest_variant_size - variant_struct.size.size_bits;
                    let mut fields = variant_struct.struct_type.get_field_types();
                    if size_diff_bits != 0 {
                        let padding = self.builtin_types.padding_type(size_diff_bits as u32);
                        fields.push(padding.as_basic_type_enum());
                        debug!("Padding variant {} with {}", variant_struct.struct_type, padding);
                        let variant = &enum_type.variants[index];
                        let struct_name = &format!(
                            "{}_{}",
                            &*self.get_ident_name(variant.name),
                            variant.payload.map(|p| p.to_string()).unwrap_or("".to_string()),
                        );
                        let variant_struct_type =
                            Codegen::make_named_struct(self.ctx, struct_name, &fields);

                        // We call size_info on a non-opaque copy to get the size
                        let variant_struct_size = self.size_info(&variant_struct_type);
                        variant_struct.struct_type = variant_struct_type;
                        variant_struct.size = variant_struct_size;
                    } else {
                        debug!("Not padding variant {}", variant_struct.struct_type);
                    }
                }

                let union_padding = self
                    .builtin_types
                    .padding_type(largest_variant_size - tag_int_type.get_bit_width());

                let base_type = self.ctx.struct_type(
                    &[tag_int_type.as_basic_type_enum(), union_padding.as_basic_type_enum()],
                    false,
                );

                let mut enum_size = self.size_info(&base_type);

                // Alignment is the largest alignment of any variant
                enum_size.align_bits =
                    variant_structs.iter().map(|v| v.size.align_bits).max().unwrap();

                let same_size_check = variant_structs
                    .iter()
                    .map(|s| s.size.size_bits)
                    .all(|size| size == enum_size.size_bits);
                if !same_size_check {
                    panic!(
                        "Enum codegen sizes were wrong. Base: {:?}, Variants: {:?}",
                        &base_type,
                        variant_structs.iter().map(|v| v.struct_type).collect::<Vec<_>>()
                    );
                }

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
                                variant.size.size_bits as u64,
                                variant.size.align_bits,
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
                        enum_size.size_bits as u64,
                        enum_size.align_bits,
                        0,
                        &member_types,
                        0,
                        &name,
                    )
                    .as_type();

                Ok(LlvmEnumType {
                    type_id,
                    tag_type: tag_int_type,
                    base_struct_type: base_type,
                    variant_structs,
                    di_type: debug_union_type,
                    size: enum_size,
                }
                .into())
            }
            Type::EnumVariant(ev) => {
                let parent_enum = self.codegen_type(ev.enum_type_id)?.expect_enum();
                Ok(parent_enum.into())
            }
            Type::Never(_) => {
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
            Type::OpaqueAlias(opaque) => self.codegen_type_inner(opaque.aliasee, depth + 1),
            Type::RecursiveReference(rr) => {
                if depth == 0 {
                    // If this is not a recursive call, we already built this type
                    // and should 'follow the redirect' so that calling code
                    // gets the actual type
                    self.codegen_type(rr.root_type_id)
                } else {
                    // If this is a recursive call (depth > 0), we are in the process of
                    // building the type, and should return a placeholder for the 'recursive
                    // reference' type
                    // For recursive references, we use an empty struct as the representation,
                    // and use LLVMs opaque struct type to represent it
                    let defn_info = self
                        .module
                        .types
                        .get_defn_info(rr.root_type_id)
                        .expect("recursive type must have defn info");

                    // FIXME: This needs to codegen the name in a standardized way so it matches
                    let name = self.get_ident_name(defn_info.name);
                    let s = self.ctx.opaque_struct_type(name);

                    no_cache = true;
                    Ok(LlvmType::StructType(LlvmStructType {
                        type_id,
                        struct_type: s,
                        fields: Vec::new(),
                        di_type: self.make_debug_struct_type(name, &s, SpanId::NONE, &[]),
                        size: SizeInfo::ZERO,
                    }))
                }
            }
            Type::Generic(_) => {
                panic!("Cannot codegen a Generic; something went wrong in typecheck")
            }
            Type::Function(_) => todo!("Cannot yet codegen a physical Function type"),
        }?;
        if !no_cache {
            self.llvm_types.borrow_mut().insert(type_id, codegened_type.clone());
        }
        // eprintln!(
        //     "Size of '{}': {:?}",
        //     self.module.type_id_to_string(type_id),
        //     codegened_type.size_info()
        // );
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

        // Consequent Block
        self.builder.position_at_end(consequent_block);
        let consequent_block_value = self.codegen_expr(&ir_if.consequent)?;
        let consequent_incoming = match consequent_block_value {
            LlvmValue::Never(_) => None,
            LlvmValue::BasicValue(value) => {
                let consequent_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block);
                Some((consequent_final_block, value))
            }
        };

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_incoming = match self.codegen_expr(&ir_if.alternate)? {
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

    fn codegen_expr_lvalue(&mut self, expr: &TypedExpr) -> CodegenResult<PointerValue<'ctx>> {
        trace!("codegen expr lvalue\n{}", self.module.expr_to_string(expr));
        // We need only match on expression types that are allowed to appear as lvalues. That means:
        // - Variables
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
            TypedExpr::Integer(integer) => {
                let value = self.codegen_integer_value(integer)?;
                Ok(value.into())
            }
            TypedExpr::Float(float) => {
                let value = self.codegen_float_value(float)?;
                Ok(value.into())
            }
            TypedExpr::Str(string_value, _) => {
                // Get a hold of the type for 'string' (its just a struct that we expect to exist!)
                let string_type = self.codegen_type(STRING_TYPE_ID).unwrap();
                let string_struct = string_type.value_basic_type().into_struct_type();

                let global_str_data = self.llvm_module.add_global(
                    self.builtin_types.char.array_type(string_value.len() as u32),
                    None,
                    "string_lit_data",
                );
                global_str_data.set_initializer(&i8_array_from_str(self.ctx, string_value));
                global_str_data.set_constant(true);
                let global_value = self.llvm_module.add_global(string_struct, None, "string_lit");
                global_value.set_constant(true);
                let value = string_struct.const_named_struct(&[
                    self.builtin_types.int.const_int(string_value.len() as u64, true).into(),
                    global_str_data.as_pointer_value().into(),
                ]);
                global_value.set_initializer(&value);
                let loaded = self.builder.build_load(
                    string_struct,
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
                    UnaryOpKind::ReferenceToInt => {
                        let as_int = self.builder.build_ptr_to_int(
                            value.into_pointer_value(),
                            self.builtin_types.int,
                            "ptr_as_int",
                        );
                        Ok(as_int.as_basic_value_enum().into())
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
            TypedExpr::EnumConstructor(enum_constr) => {
                let llvm_type = self.codegen_type(enum_constr.type_id)?;
                let enum_type = llvm_type.expect_enum();
                let variant_tag_name = enum_constr.variant_name;

                let enum_variant = enum_type.variant_structs[enum_constr.variant_index as usize];

                let enum_ptr = self.builder.build_alloca(enum_variant.struct_type, "enum_constr");

                // Store the tag_value in the first slot
                let tag_pointer = self
                    .builder
                    .build_struct_gep(
                        enum_variant.struct_type,
                        enum_ptr,
                        0,
                        &format!("enum_tag_{}", &*self.get_ident_name(variant_tag_name)),
                    )
                    .unwrap();
                self.builder.build_store(tag_pointer, enum_variant.tag_value);

                if let Some(payload) = &enum_constr.payload {
                    let value = self.codegen_expr_basic_value(payload)?;
                    let payload_pointer = self
                        .builder
                        .build_struct_gep(
                            enum_variant.struct_type,
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
                let enum_llvm =
                    self.codegen_type(enum_is_variant.target_expr.get_type())?.expect_enum();
                let variant = enum_llvm
                    .variant_structs
                    .iter()
                    .find(|v| v.name == enum_is_variant.variant_name)
                    .unwrap();
                let is_variant_bool = self.codegen_enum_is_variant(enum_value, variant.tag_value);
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
            TypedExpr::Cast(cast) => {
                match cast.cast_type {
                    CastType::KnownNoOp | CastType::Integer8ToChar => {
                        let value = self.codegen_expr_basic_value(&cast.base_expr)?;
                        Ok(value.into())
                    }
                    CastType::IntegerExtend | CastType::IntegerExtendFromChar => {
                        let value = self.codegen_expr_basic_value(&cast.base_expr)?;
                        let int_value = value.into_int_value();
                        let llvm_type = self.codegen_type(cast.target_type_id)?;
                        let integer_type =
                            self.module.types.get(cast.target_type_id).expect_integer();
                        let value: IntValue<'ctx> = if integer_type.is_signed() {
                            self.builder.build_int_s_extend(
                                int_value,
                                llvm_type.value_basic_type().into_int_type(),
                                "extend_cast",
                            )
                        } else {
                            self.builder.build_int_z_extend(
                                int_value,
                                llvm_type.value_basic_type().into_int_type(),
                                "extend_cast",
                            )
                        };
                        Ok(value.as_basic_value_enum().into())
                    }
                    CastType::IntegerTruncate => {
                        let value = self.codegen_expr_basic_value(&cast.base_expr)?;
                        let int_value = value.into_int_value();
                        let int_type = self.codegen_type(cast.target_type_id)?;
                        let truncated_value = self.builder.build_int_truncate(
                            int_value,
                            int_type.value_basic_type().into_int_type(),
                            "trunc_cast",
                        );
                        Ok(truncated_value.as_basic_value_enum().into())
                    }
                    CastType::FloatExtend => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_float_value();
                        let float_dst_type = self
                            .codegen_type(cast.target_type_id)?
                            .value_basic_type()
                            .into_float_type();
                        let extended_value =
                            self.builder.build_float_ext(from_value, float_dst_type, "fext");
                        Ok(extended_value.as_basic_value_enum().into())
                    }
                    CastType::FloatTruncate => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_float_value();
                        let float_dst_type = self
                            .codegen_type(cast.target_type_id)?
                            .value_basic_type()
                            .into_float_type();
                        let extended_value =
                            self.builder.build_float_trunc(from_value, float_dst_type, "ftrunc");
                        Ok(extended_value.as_basic_value_enum().into())
                    }
                    CastType::FloatToInteger => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_float_value();
                        let int_dst_type = self.codegen_type(cast.target_type_id)?;
                        let int_dst_type_llvm = int_dst_type.value_basic_type().into_int_type();
                        let int_dest_k1_type =
                            self.module.types.get(cast.target_type_id).expect_integer();
                        let casted_int_value = if int_dest_k1_type.is_signed() {
                            self.builder.build_float_to_signed_int(
                                from_value,
                                int_dst_type_llvm,
                                "",
                            )
                        } else {
                            self.builder.build_float_to_unsigned_int(
                                from_value,
                                int_dst_type_llvm,
                                "",
                            )
                        };
                        Ok(casted_int_value.as_basic_value_enum().into())
                    }
                    CastType::IntegerToFloat => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_int_value();
                        let from_int_k1_type =
                            self.module.types.get(cast.base_expr.get_type()).expect_integer();
                        let float_dst_type = self.codegen_type(cast.target_type_id)?;
                        let float_dst_type_llvm =
                            float_dst_type.value_basic_type().into_float_type();
                        let casted_float_value = if from_int_k1_type.is_signed() {
                            self.builder.build_signed_int_to_float(
                                from_value,
                                float_dst_type_llvm,
                                "",
                            )
                        } else {
                            self.builder.build_unsigned_int_to_float(
                                from_value,
                                float_dst_type_llvm,
                                "",
                            )
                        };
                        Ok(casted_float_value.as_basic_value_enum().into())
                    }
                    CastType::PointerToReference => {
                        // I think this is a complete no-op in the LLVM ir
                        let pointer =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_pointer_value();
                        Ok(pointer.as_basic_value_enum().into())
                    }
                    CastType::ReferenceToPointer => {
                        // I think this is a complete no-op in the LLVM ir
                        let reference =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_pointer_value();
                        Ok(reference.as_basic_value_enum().into())
                    }
                    CastType::PointerToInteger => {
                        let ptr =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_pointer_value();
                        let as_int = self.builder.build_ptr_to_int(
                            ptr,
                            self.builtin_types.ptr_sized_int,
                            "ptrtoint_cast",
                        );
                        Ok(as_int.as_basic_value_enum().into())
                    }
                    CastType::IntegerToPointer => {
                        let int = self.codegen_expr_basic_value(&cast.base_expr)?.into_int_value();
                        let as_ptr = self.builder.build_int_to_ptr(
                            int,
                            self.builtin_types.ptr,
                            "inttoptr_cast",
                        );
                        Ok(as_ptr.as_basic_value_enum().into())
                    }
                }
            }
            TypedExpr::Return(ret) => {
                let return_value = self.codegen_expr_basic_value(&ret.value)?;
                let ret_inst = self.builder.build_return(Some(&return_value));
                Ok(LlvmValue::Never(ret_inst))
            }
        }
    }

    fn codegen_binop(&mut self, bin_op: &BinaryOp) -> CodegenResult<LlvmValue<'ctx>> {
        // This would be simpler if we first matched on lhs than on result type,
        // because we have to branch on int vs float now for each result type
        match self.module.types.get(bin_op.ty) {
            Type::Integer(integer_type) => {
                let signed = integer_type.is_signed();
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
                        if signed {
                            self.builder.build_int_signed_div(lhs_value, rhs_value, "sdiv")
                        } else {
                            self.builder.build_int_unsigned_div(lhs_value, rhs_value, "udiv")
                        }
                    }
                    BinaryOpKind::And => unreachable!("bitwise 'and' is handled by an ability"),
                    BinaryOpKind::Or => unreachable!("bitwise 'or' is handled by an ability"),
                    BinaryOpKind::Rem => {
                        if signed {
                            self.builder.build_int_signed_rem(lhs_value, rhs_value, "srem")
                        } else {
                            self.builder.build_int_unsigned_rem(lhs_value, rhs_value, "urem")
                        }
                    }
                    _ => {
                        panic!("Unsupported bin op kind returning int: {}", bin_op.kind)
                    }
                };
                Ok(op_res.as_basic_value_enum().into())
            }
            Type::Float(_float_type) => {
                let lhs_value = self.codegen_expr_basic_value(&bin_op.lhs)?.into_float_value();
                let rhs_value = self.codegen_expr_basic_value(&bin_op.rhs)?.into_float_value();
                let op_res = match bin_op.kind {
                    BinaryOpKind::Add => self
                        .builder
                        .build_float_add(lhs_value, rhs_value, "fadd")
                        .as_basic_value_enum(),
                    BinaryOpKind::Subtract => self
                        .builder
                        .build_float_sub(lhs_value, rhs_value, "fsub")
                        .as_basic_value_enum(),
                    BinaryOpKind::Multiply => self
                        .builder
                        .build_float_mul(lhs_value, rhs_value, "fmul")
                        .as_basic_value_enum(),
                    BinaryOpKind::Divide => self
                        .builder
                        .build_float_div(lhs_value, rhs_value, "fdiv")
                        .as_basic_value_enum(),
                    BinaryOpKind::And => unreachable!("bitwise 'and' is unsupported on float"),
                    BinaryOpKind::Or => unreachable!("bitwise 'or' is unsupported on float"),
                    BinaryOpKind::Rem => self
                        .builder
                        .build_float_rem(lhs_value, rhs_value, "frem")
                        .as_basic_value_enum(),
                    _ => {
                        panic!("Unsupported bin op kind returning float: {}", bin_op.kind)
                    }
                };
                Ok(op_res.as_basic_value_enum().into())
            }
            Type::Bool(_) => match bin_op.kind {
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
                    match self.module.types.get(bin_op.lhs.get_type()) {
                        Type::Float(_) => {
                            let lhs_float =
                                self.codegen_expr_basic_value(&bin_op.lhs)?.into_float_value();
                            let rhs_float =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_float_value();
                            let cmp_result = self.builder.build_float_compare(
                                if bin_op.kind == BinaryOpKind::Equals {
                                    FloatPredicate::OEQ
                                } else {
                                    FloatPredicate::ONE
                                },
                                lhs_float,
                                rhs_float,
                                &format!("f{}_i1", bin_op.kind),
                            );
                            Ok(self
                                .i1_to_bool(cmp_result, &format!("{}_res", bin_op.kind))
                                .as_basic_value_enum()
                                .into())
                        }
                        t if t.is_scalar_int_value() => {
                            let lhs_int =
                                self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                            let rhs_int =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
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
                        _ => unreachable!("unreachable Equals/NotEquals call on type"),
                    }
                }
                BinaryOpKind::Less
                | BinaryOpKind::LessEqual
                | BinaryOpKind::Greater
                | BinaryOpKind::GreaterEqual => {
                    match self.module.types.get(bin_op.lhs.get_type()) {
                        Type::Integer(_) => {
                            let lhs_int =
                                self.codegen_expr_basic_value(&bin_op.lhs)?.into_int_value();
                            let rhs_int =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_int_value();
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
                        Type::Float(_) => {
                            let lhs_float =
                                self.codegen_expr_basic_value(&bin_op.lhs)?.into_float_value();
                            let rhs_float =
                                self.codegen_expr_basic_value(&bin_op.rhs)?.into_float_value();
                            let pred = match bin_op.kind {
                                BinaryOpKind::Less => FloatPredicate::OLT,
                                BinaryOpKind::LessEqual => FloatPredicate::OLE,
                                BinaryOpKind::Greater => FloatPredicate::OGT,
                                BinaryOpKind::GreaterEqual => FloatPredicate::OGE,
                                _ => unreachable!("unexpected binop kind"),
                            };
                            let i1_compare = self.builder.build_float_compare(
                                pred,
                                lhs_float,
                                rhs_float,
                                &format!("f{}", bin_op.kind),
                            );
                            Ok(self.i1_to_bool(i1_compare, "").as_basic_value_enum().into())
                        }
                        _ => unreachable!("unexpected comparison operand; not float or int"),
                    }
                }
                other => panic!("Unsupported binary operation {other:?} returning Bool"),
            },
            Type::Unit(_) => panic!("No unit-returning binary ops"),
            Type::Char(_) => panic!("No char-returning binary ops"),
            _other => unreachable!("codegen for binary ops on other types"),
        }
    }

    fn _build_k1_crash(&self, msg: &str, span_id: SpanId) -> InstructionValue {
        let msg_string = self.const_string_ptr(msg, "crash_msg");
        let span = self.module.ast.spans.get(span_id);
        let line = self.module.ast.sources.get_line_for_span(span).unwrap();
        let filename = self
            .const_string_ptr(&self.module.ast.sources.source_by_span(span).filename, "filename");
        self.builder.build_call(
            self.llvm_module.get_function("_k1_crash").unwrap(),
            &[
                msg_string.into(),
                filename.into(),
                self.builtin_types.int.const_int(line.line_index as u64 + 1, false).into(),
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
        variant_tag_value: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let enum_tag_value = self.get_enum_tag(enum_value);
        let is_equal = self.builder.build_int_compare(
            IntPredicate::EQ,
            enum_tag_value,
            variant_tag_value,
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
        // We have to use a pointer to the enum value to extract the payload
        // in the type of the variant; its like a reinterpret cast

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

        if let Some(intrinsic_type) = callee.intrinsic_type {
            if intrinsic_type.is_inlined() {
                return self.codegen_intrinsic_inline(callee.intrinsic_type.unwrap(), call);
            }
        }

        let function_value = self.codegen_function(call.callee_function_id, callee)?;

        let args: CodegenResult<Vec<BasicMetadataValueEnum<'ctx>>> = call
            .args
            .iter()
            .enumerate()
            .map(|(idx, arg_expr)| {
                let expected_type = callee.params[idx].type_id;
                let basic_value = self.codegen_expr_basic_value(arg_expr);
                trace!(
                    "codegen function call arg: {}: {} (expected: {})",
                    self.module.expr_to_string(arg_expr),
                    self.module.type_id_to_string(arg_expr.get_type()),
                    self.module.type_id_to_string(expected_type)
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
        string_base: &TypedExpr,
        string_index: &TypedExpr,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let string_value = self.codegen_expr_basic_value(string_base)?;
        let index_value = self.codegen_expr_basic_value(string_index)?;

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

    #[allow(unused)]
    fn const_string_ptr(&self, string: &str, name: &str) -> PointerValue<'ctx> {
        let char_data = self.ctx.const_string(string.as_bytes(), false);
        let length_value = self.builtin_types.int.const_int(string.len() as u64, false);
        let char_data_global =
            self.llvm_module.add_global(char_data.get_type(), None, &format!("{name}_data"));
        char_data_global.set_initializer(&char_data);
        let string_type = self.codegen_type(STRING_TYPE_ID).unwrap();
        let string_struct_type = string_type.value_basic_type().into_struct_type();
        let string_struct = string_struct_type.const_named_struct(&[
            length_value.as_basic_value_enum(),
            char_data_global.as_pointer_value().as_basic_value_enum(),
        ]);
        let g = self.llvm_module.add_global(string_struct_type, None, name);
        g.set_initializer(&string_struct);
        g.as_pointer_value()
    }

    #[allow(unused)]
    fn const_string_loaded(&self, string: &str, name: &str) -> StructValue<'ctx> {
        let string_type = self.codegen_type(STRING_TYPE_ID).unwrap();
        let string_struct_type = string_type.value_basic_type().into_struct_type();
        let ptr = self.const_string_ptr(string, name);
        self.builder
            .build_load(string_struct_type, ptr, &format!("{name}_loaded"))
            .into_struct_value()
    }

    fn get_loaded_variable(&mut self, variable_id: VariableId) -> BasicValueEnum<'ctx> {
        self.variables.get(&variable_id).unwrap().loaded_value(&self.builder)
    }

    fn codegen_intrinsic_inline(
        &mut self,
        intrinsic_type: IntrinsicFunction,
        call: &Call,
    ) -> CodegenResult<LlvmValue<'ctx>> {
        match intrinsic_type {
            IntrinsicFunction::SizeOf => {
                let type_param = &call.type_args[0];
                let llvm_type = self.codegen_type(type_param.type_id)?;
                let size = self.size_info(&llvm_type.value_any_type());
                let size_bytes = size.size_bits / 8;
                let size_value = self.builtin_types.int.const_int(size_bytes as u64, false);
                Ok(size_value.as_basic_value_enum().into())
            }
            IntrinsicFunction::AlignOf => {
                let type_param = &call.type_args[0];
                let llvm_type = self.codegen_type(type_param.type_id)?;
                let size = self.size_info(&llvm_type.value_any_type());
                let align_bytes = size.align_bits / 8;
                let align_value = self.builtin_types.int.const_int(align_bytes as u64, false);
                Ok(align_value.as_basic_value_enum().into())
            }
            IntrinsicFunction::BitNot => {
                let input_value = self.codegen_expr_basic_value(&call.args[0])?.into_int_value();
                let not_value = self.builder.build_not(input_value, "not");
                Ok(not_value.as_basic_value_enum().into())
            }
            IntrinsicFunction::BitAnd
            | IntrinsicFunction::BitXor
            | IntrinsicFunction::BitOr
            | IntrinsicFunction::BitShiftLeft
            | IntrinsicFunction::BitShiftRight => {
                // We only support signed 64 bit integers for now
                let is_operand_signed = true;
                let sign_extend = is_operand_signed;
                let lhs = self.codegen_expr_basic_value(&call.args[0])?.into_int_value();
                let rhs = self.codegen_expr_basic_value(&call.args[1])?.into_int_value();
                let result = match intrinsic_type {
                    IntrinsicFunction::BitAnd => self.builder.build_and(lhs, rhs, "and"),
                    IntrinsicFunction::BitXor => self.builder.build_xor(lhs, rhs, "xor"),
                    IntrinsicFunction::BitOr => self.builder.build_or(lhs, rhs, "or"),
                    IntrinsicFunction::BitShiftLeft => {
                        self.builder.build_left_shift(lhs, rhs, "shl")
                    }
                    IntrinsicFunction::BitShiftRight => {
                        self.builder.build_right_shift(lhs, rhs, sign_extend, "shr")
                    }
                    _ => unreachable!(),
                };
                Ok(result.as_basic_value_enum().into())
            }
            IntrinsicFunction::TypeId => {
                let type_param = &call.type_args[0];
                let type_id_value = self.codegen_integer_value(&TypedIntegerExpr {
                    value: TypedIntegerValue::U64(type_param.type_id.to_u64()),
                    span: call.span,
                })?;
                Ok(type_id_value.into())
            }
            IntrinsicFunction::StringGet => {
                let string_elem_ptr =
                    self.codegen_string_index_operation(&call.args[0], &call.args[1])?;
                let string_elem_value =
                    self.builder.build_load(self.builtin_types.char, string_elem_ptr, "string_get");
                Ok(string_elem_value.into())
            }
            IntrinsicFunction::StringSet => {
                todo!()
            }
            IntrinsicFunction::PointerIndex => {
                //  Reference:
                //  intern fn refAtIndex[T](self: Pointer, index: u64): T*
                let pointee_ty_arg = call.type_args[0];
                let elem_type = self.codegen_type(pointee_ty_arg.type_id)?;
                let ptr = self.codegen_expr_basic_value(&call.args[0])?.into_pointer_value();
                let index = self.codegen_expr_basic_value(&call.args[1])?.into_int_value();
                let result_pointer = unsafe {
                    self.builder.build_in_bounds_gep(
                        elem_type.value_basic_type(),
                        ptr,
                        &[index],
                        "refAtIndex",
                    )
                };
                Ok(result_pointer.as_basic_value_enum().into())
            }
            IntrinsicFunction::ReferenceSet => {
                //  intern fn referenceSet[T](t: T*, value: T): unit
                let reference_value =
                    self.codegen_expr_basic_value(&call.args[0])?.into_pointer_value();
                let actual_value = self.codegen_expr_basic_value(&call.args[1])?;
                self.builder.build_store(reference_value, actual_value);
                Ok(self.builtin_types.unit_value.as_basic_value_enum().into())
            }
            _ => {
                panic!("Unexpected intrinsic type for inline gen {:?}", intrinsic_type)
            }
        }
    }

    fn codegen_intrinsic_function_body(
        &mut self,
        intrinsic_type: IntrinsicFunction,
        function: &TypedFunction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match intrinsic_type {
            IntrinsicFunction::Exit => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.builder.build_call(self.libc_functions.exit, &[first_arg.into()], "exit");
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunction::PrintInt => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.build_print_int_call(first_arg, true);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunction::PrintUInt => {
                let first_arg = self.get_loaded_variable(function.params[0].variable_id);
                self.build_print_int_call(first_arg, false);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunction::PrintString => {
                let string_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_struct_value();
                self.build_print_string_call(string_arg);
                Ok(self.builtin_types.unit_value.as_basic_value_enum())
            }
            IntrinsicFunction::StringLength => {
                let string_arg =
                    self.get_loaded_variable(function.params[0].variable_id).into_struct_value();
                let length = self.builtin_types.string_length(&self.builder, string_arg);
                Ok(length.as_basic_value_enum())
            }
            IntrinsicFunction::StringEquals => {
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
                    self.builtin_types.int.const_zero(),
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
            _ => {
                panic!("Unexpected intrinsic type for actual llvm function {:?}", intrinsic_type)
            }
        }
    }

    fn codegen_block_statements(&mut self, block: &TypedBlock) -> CodegenResult<LlvmValue<'ctx>> {
        let unit_value = self.builtin_types.unit_value.as_basic_value_enum().into();
        let mut last: LlvmValue<'ctx> = unit_value;
        self.set_debug_location(block.span);
        for stmt in &block.statements {
            match stmt {
                TypedStmt::Expr(expr) => last = self.codegen_expr(expr)?,
                TypedStmt::ValDef(val_def) => {
                    let _value = self.codegen_val(val_def)?;
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
        debug!("codegen function\n{}", self.module.function_id_to_string(function_id, true));

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
            TyperLinkage::External(_name) => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        let llvm_name = match function.linkage {
            TyperLinkage::External(Some(name)) => self.module.get_ident_str(name),
            _ => &self.module.make_qualified_name(function.scope, function.name, "__", true),
        };
        let fn_val = self.llvm_module.add_function(llvm_name, fn_ty, llvm_linkage);

        self.llvm_functions.insert(function_id, fn_val);

        if matches!(function.linkage, Linkage::External(_)) {
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
                let value = self
                    .codegen_intrinsic_function_body(intrinsic_type, function)?
                    .as_basic_value_enum();
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
                        };
                    }
                }
            }
        };
        if let Some(start_block) = maybe_starting_block {
            self.builder.position_at_end(start_block);
        }
        self.debug.pop_scope();
        fn_val.set_subprogram(di_subprogram);
        Ok(fn_val)
    }

    fn codegen_integer_value(
        &self,
        integer: &TypedIntegerExpr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(integer.get_type())?;
        let llvm_int_ty = llvm_ty.value_basic_type().into_int_type();
        let Type::Integer(int_type) = self.module.types.get(llvm_ty.type_id()) else { panic!() };
        let llvm_value = if int_type.is_signed() {
            llvm_int_ty.const_int(integer.value.as_u64(), true)
        } else {
            llvm_int_ty.const_int(integer.value.as_u64(), false)
        };
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_float_value(&self, float: &TypedFloatExpr) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(float.get_type())?;
        let llvm_float_ty = llvm_ty.value_basic_type().into_float_type();
        let llvm_value = llvm_float_ty.const_float(float.value.as_f64());
        Ok(llvm_value.as_basic_value_enum())
    }

    pub fn codegen_module(&mut self) -> CodegenResult<()> {
        let start = std::time::Instant::now();
        for constant in &self.module.constants {
            match &constant.expr {
                TypedExpr::Integer(integer) => {
                    let llvm_value = self.codegen_integer_value(integer)?;
                    let variable = self.module.variables.get_variable(constant.variable_id);
                    let llvm_global = self.llvm_module.add_global(
                        llvm_value.get_type(),
                        Some(AddressSpace::default()),
                        self.module.get_ident_str(variable.name),
                    );
                    llvm_global.set_constant(true);
                    llvm_global.set_initializer(&llvm_value);
                    self.globals.insert(constant.variable_id, llvm_global);
                }
                _ => unimplemented!("constants must be integers"),
            }
        }
        for (id, function) in self.module.function_iter() {
            if self.module.should_codegen_function(function) {
                self.codegen_function(id, function)?;
            }
        }
        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    fn set_up_machine(module: &mut LlvmModule) -> TargetMachine {
        // Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let triple = TargetMachine::get_default_triple();
        // let triple = TargetTriple::create("arm64-apple-macosx14.4.0");
        let target = Target::from_triple(&triple).unwrap();
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

        module.set_data_layout(&machine.get_target_data().get_data_layout());
        module.set_triple(&triple);

        machine
    }

    pub fn optimize(&mut self, optimize: bool) -> anyhow::Result<()> {
        let start = std::time::Instant::now();

        if !self.debug.strip_debug {
            self.debug.debug_builder.finalize();
        } else {
            self.llvm_module.strip_debug_info();
        }
        self.llvm_module.verify().map_err(|err| {
            eprintln!("{}", self.llvm_module.to_string());
            anyhow::anyhow!("Module '{}' failed validation: {}", self.name(), err.to_string_lossy())
        })?;

        let module_pass_manager: PassManager<LlvmModule<'ctx>> = PassManager::create(());
        if optimize {
            module_pass_manager.add_cfg_simplification_pass();
            module_pass_manager.add_scalar_repl_aggregates_pass_ssa();
            module_pass_manager.add_promote_memory_to_register_pass();
            module_pass_manager.add_new_gvn_pass();
            module_pass_manager.add_instruction_combining_pass();
            module_pass_manager.add_sccp_pass();
            module_pass_manager.add_aggressive_dce_pass();
        }

        // Workaround: We have to inline always because ArrayNew returns a pointer to an alloca
        module_pass_manager.add_function_inlining_pass();

        module_pass_manager.add_function_attrs_pass();
        module_pass_manager.add_verifier_pass();

        self.llvm_machine.add_analysis_passes(&module_pass_manager);

        module_pass_manager.run_on(&self.llvm_module);

        info!("codegen phase 'optimize' took {}ms", start.elapsed().as_millis());

        Ok(())
    }

    #[allow(unused)]
    pub fn emit_object_file(&self, rel_destination_dir: &str) -> anyhow::Result<()> {
        let filename = format!("{}.o", self.name());
        let machine = &self.llvm_machine;
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
        let base_lib_module = self
            .ctx
            .create_module_from_ir(
                MemoryBuffer::create_from_file(Path::new("k1lib/k1lib.ll")).unwrap(),
            )
            .unwrap();
        self.llvm_module.link_in_module(base_lib_module).unwrap();
        let Some(main_fn_id) = self.module.get_main_function_id() else {
            bail!("No main function")
        };
        let llvm_function = self.llvm_functions.get(&main_fn_id).unwrap();
        eprintln!("Interpreting {}", self.module.name());
        let return_value = unsafe { engine.run_function(*llvm_function, &[]) };
        let res: u64 = return_value.as_int(true);
        Ok(res)
    }
}
