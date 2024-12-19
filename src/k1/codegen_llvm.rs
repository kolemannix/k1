use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::path::Path;

use anyhow::bail;
use either::Either;
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
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine, TargetTriple};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType, VoidType,
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
        stride_bits: td.get_abi_size(typ) as u32 * 8,
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
    Pointer(PointerValue<'ctx>),
    Never(InstructionValue<'ctx>),
}
impl<'ctx> LlvmValue<'ctx> {
    fn as_basic_value(self) -> Either<InstructionValue<'ctx>, BasicValueEnum<'ctx>> {
        match self {
            LlvmValue::BasicValue(bv) => Either::Right(bv),
            LlvmValue::Pointer(p) => Either::Right(p.as_basic_value_enum()),
            LlvmValue::Never(instr) => Either::Left(instr),
        }
    }
    fn expect_basic_value(self) -> BasicValueEnum<'ctx> {
        self.as_basic_value().expect_right("Expected BasicValue on never value")
    }

    #[allow(unused)]
    fn expect_never(&self) -> InstructionValue<'ctx> {
        self.as_basic_value().expect_left("Expected Never on a real value")
    }
}

impl<'ctx> From<PointerValue<'ctx>> for LlvmValue<'ctx> {
    fn from(pointer: PointerValue<'ctx>) -> Self {
        LlvmValue::Pointer(pointer)
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
    /// Stride represents the space between the start of 2 successive elements, it bumps to the
    /// next aligned slot. This must be used by builtin Buffer functionality in order to be correct
    stride_bits: u32,
}
impl SizeInfo {
    pub const POINTER: SizeInfo = SizeInfo {
        size_bits: WORD_SIZE_BITS as u32,
        align_bits: WORD_SIZE_BITS as u32,
        stride_bits: WORD_SIZE_BITS as u32,
    };
    pub const ZERO: SizeInfo = SizeInfo { size_bits: 0, align_bits: 0, stride_bits: 0 };

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
        SizeInfo { size_bits: total_size, align_bits: max_align, stride_bits: total_size }
    }

    pub fn scalar_value(size_bits: u32) -> SizeInfo {
        SizeInfo { size_bits, align_bits: size_bits, stride_bits: size_bits }
    }
}

#[derive(Debug, Copy, Clone)]
struct LlvmPointerType<'ctx> {
    #[allow(unused)]
    type_id: TypeId,
    pointer_type: PointerType<'ctx>,
    #[allow(unused)]
    pointee_type: AnyTypeEnum<'ctx>,
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
struct LlvmClosureType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

#[derive(Debug, Clone)]
enum LlvmType<'ctx> {
    Value(LlvmValueType<'ctx>),
    EnumType(LlvmEnumType<'ctx>),
    StructType(LlvmStructType<'ctx>),
    Closure(LlvmClosureType<'ctx>),
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

impl<'ctx> From<LlvmClosureType<'ctx>> for LlvmType<'ctx> {
    fn from(value: LlvmClosureType<'ctx>) -> Self {
        LlvmType::Closure(value)
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
            LlvmType::Closure(c) => c.size,
        }
    }

    #[allow(unused)]
    pub fn expect_pointer(&self) -> LlvmPointerType<'ctx> {
        match self {
            LlvmType::Pointer(pointer) => *pointer,
            _ => panic!("expected pointer on {self:?}"),
        }
    }

    pub fn expect_enum(self) -> LlvmEnumType<'ctx> {
        match self {
            LlvmType::EnumType(e) => e,
            _ => panic!("expected enum on {self:?}"),
        }
    }

    fn expect_struct(self) -> LlvmStructType<'ctx> {
        match self {
            LlvmType::StructType(s) => s,
            _ => panic!("expected struct on {self:?}"),
        }
    }

    fn fn_type(
        &self,
        args: &[BasicMetadataTypeEnum<'ctx>],
        varargs: bool,
    ) -> LlvmFunctionType<'ctx> {
        match self {
            LlvmType::Void(v) => v.void_type.fn_type(args, varargs),
            other => other.physical_value_type().fn_type(args, varargs),
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
            LlvmType::Closure(c) => c.type_id,
        }
    }

    fn physical_value_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            LlvmType::Value(value) => value.basic_type,
            LlvmType::Pointer(pointer) => pointer.pointer_type.as_basic_type_enum(),
            LlvmType::EnumType(e) => e.base_struct_type.as_basic_type_enum(),
            LlvmType::StructType(s) => s.struct_type.as_basic_type_enum(),
            LlvmType::Void(_) => panic!("No value type on Void / never"),
            LlvmType::Closure(c) => c.struct_type.as_basic_type_enum(),
        }
    }

    #[allow(unused)]
    fn value_any_type(&self) -> AnyTypeEnum<'ctx> {
        match self {
            LlvmType::Void(v) => v.void_type.as_any_type_enum(),
            _ => self.physical_value_type().as_any_type_enum(),
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            LlvmType::Value(value) => value.di_type,
            LlvmType::Pointer(pointer) => pointer.di_type,
            LlvmType::EnumType(e) => e.di_type,
            LlvmType::StructType(s) => s.di_type,
            LlvmType::Void(v) => v.di_type,
            LlvmType::Closure(c) => c.di_type,
        }
    }

    #[allow(unused)]
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
    ptr: PointerType<'ctx>,
    ptr_sized_int: IntType<'ctx>,
}

impl<'ctx> BuiltinTypes<'ctx> {
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

#[derive(Clone, Copy)]
pub struct LoopInfo<'ctx> {
    pub break_value_ptr: Option<PointerValue<'ctx>>,
    pub end_block: BasicBlock<'ctx>,
}

pub struct CodegenedFunction<'ctx> {
    pub function_value: FunctionValue<'ctx>,
    pub instruction_count: usize,
}

pub struct Codegen<'ctx, 'module> {
    ctx: &'ctx Context,
    pub module: &'module TypedModule,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: TargetMachine,
    builder: Builder<'ctx>,
    pub llvm_functions: HashMap<FunctionId, CodegenedFunction<'ctx>>,
    llvm_types: RefCell<HashMap<TypeId, LlvmType<'ctx>>>,
    /// The type of stored pointers here should be one level higher than the type of the
    /// value pointed to. This is so that it can be used reliably, without matching or
    /// checking, as a Pointer to the actual type
    variables: HashMap<VariableId, Pointer<'ctx>>,
    globals: HashMap<VariableId, GlobalValue<'ctx>>,
    closure_environments: HashMap<TypeId, PointerValue<'ctx>>,
    loops: HashMap<ScopeId, LoopInfo<'ctx>>,
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

#[derive(Debug)]
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
        //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("k1lib/llvm")).unwrap())
        //     .unwrap();
        // llvm_module.link_in_module(stdlib_module).unwrap();

        let debug_context = Codegen::init_debug(ctx, &llvm_module, module, optimize, debug);

        let machine = Codegen::set_up_machine(&mut llvm_module);
        let target_data = machine.get_target_data();

        let pointers = HashMap::new();
        let globals = HashMap::new();
        let int = ctx.i64_type();

        let closure_environments = HashMap::new();

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
            // It doesn't matter what type the pointer points to; its irrelevant in LLVM
            // since pointers do not actually have types
            ptr: ctx.i64_type().ptr_type(AddressSpace::default()),
            ptr_sized_int: ctx.ptr_sized_int_type(&target_data, None),
        };

        Codegen {
            ctx,
            module,
            llvm_module,
            llvm_machine: machine,
            builder,
            variables: pointers,
            globals,
            closure_environments,
            loops: HashMap::new(),
            llvm_functions: HashMap::new(),
            llvm_types: RefCell::new(HashMap::new()),
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
        let line = self.module.ast.sources.get_line_for_span_start(span).expect("No line for span");
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

    fn get_line_number(&self, span: SpanId) -> u32 {
        let span = self.module.ast.spans.get(span);
        let line = self.module.ast.sources.get_line_for_span_start(span).expect("No line for span");
        line.line_index + 1
    }

    fn write_type_name(
        &self,
        w: &mut impl std::io::Write,
        type_id: TypeId,
        defn_info: Option<&TypeDefnInfo>,
    ) {
        match defn_info {
            None => write!(w, "{}", type_id).unwrap(),
            Some(info) => self.module.write_qualified_name(w, info.scope, info.name, "/", true),
        }
    }

    fn codegen_type_name(&self, type_id: TypeId, defn_info: Option<&TypeDefnInfo>) -> String {
        let mut s = Vec::with_capacity(64);
        self.write_type_name(&mut s, type_id, defn_info);
        String::from_utf8(s).unwrap()
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
                // We don't know what it points to, so we just say 'bytes' for the best debugger
                // experience
                let placeholder_pointee = self.codegen_type(I8_TYPE_ID)?.debug_type();
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
                            self.builtin_types.ptr_sized_int.get_bit_width(),
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
                let name = self.codegen_type_name(type_id, struc.type_defn_info.as_ref());
                for field in &struc.fields {
                    let field_llvm_type = self.codegen_type_inner(field.type_id, depth + 1)?;
                    field_basic_types.push(field_llvm_type.physical_value_type());
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
                Ok(LlvmStructType {
                    type_id,
                    struct_type: llvm_struct_type,
                    fields: field_types,
                    size,
                    di_type,
                }
                .into())
            }
            Type::TypeVariable(v) => {
                err!(span, "codegen was asked to codegen a type variable {:?}", v)
            }
            Type::Reference(reference) => {
                let inner_type = self.codegen_type_inner(reference.inner_type, depth + 1)?;
                let inner_debug_type = inner_type.debug_type();
                Ok(LlvmPointerType {
                    type_id,
                    pointer_type: inner_type
                        .physical_value_type()
                        .ptr_type(AddressSpace::default()),
                    pointee_type: inner_type.physical_value_type().as_any_type_enum(),
                    di_type: self
                        .debug
                        .create_pointer_type(&format!("reference_{}", type_id), inner_debug_type),
                }
                .into())
            }
            Type::Enum(enum_type) => {
                let enum_name = enum_type
                    .type_defn_info
                    .as_ref()
                    .map(|info| self.module.make_qualified_name(info.scope, info.name, ".", true))
                    .unwrap_or(type_id.to_string());
                let mut variant_structs = Vec::with_capacity(enum_type.variants.len());
                if enum_type.variants.len() >= u8::MAX as usize {
                    panic!("too many enum variants for now");
                }
                let tag_int_type =
                    self.codegen_type(U8_TYPE_ID)?.physical_value_type().into_int_type();
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
                    let variant_name = format!("{enum_name}.{}", self.get_ident_name(variant.name));
                    let (variant_struct, variant_struct_debug) = if let Some(payload_type_id) =
                        variant.payload
                    {
                        let variant_payload_type =
                            self.codegen_type_inner(payload_type_id, depth + 1)?;
                        let struc = self.ctx.struct_type(
                            &[
                                tag_int_type.as_basic_type_enum(),
                                variant_payload_type.physical_value_type(),
                            ],
                            false,
                        );
                        let debug_struct = self.make_debug_struct_type(
                            &variant_name,
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
                                &variant_name,
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
                        let padding = self.builtin_types.padding_type(size_diff_bits);
                        fields.push(padding.as_basic_type_enum());
                        debug!("Padding variant {} with {}", variant_struct.struct_type, padding);
                        let variant = &enum_type.variants[index];
                        let struct_name = &format!(
                            "{}_{}",
                            self.get_ident_name(variant.name),
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
                let debug_union_type = self
                    .debug
                    .debug_builder
                    .create_union_type(
                        self.debug.current_scope(),
                        &enum_name,
                        self.debug.current_file(),
                        0,
                        enum_size.size_bits as u64,
                        enum_size.align_bits,
                        0,
                        &member_types,
                        0,
                        &enum_name,
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
                        .get_type_defn_info(rr.root_type_id)
                        .expect("recursive type must have defn info");

                    let name = self.codegen_type_name(type_id, Some(defn_info));
                    let s = self.ctx.opaque_struct_type(&name);

                    no_cache = true;
                    Ok(LlvmType::StructType(LlvmStructType {
                        type_id,
                        struct_type: s,
                        fields: Vec::new(),
                        di_type: self.make_debug_struct_type(&name, &s, SpanId::NONE, &[]),
                        size: SizeInfo::ZERO,
                    }))
                }
            }
            Type::Function(_function_type) => {
                let pointee = self.make_llvm_function_type(type_id)?;
                let placeholder_pointee = self.codegen_type(I8_TYPE_ID)?.debug_type();

                Ok(LlvmType::Pointer(LlvmPointerType {
                    type_id,
                    pointer_type: pointee.ptr_type(AddressSpace::default()),
                    pointee_type: pointee.as_any_type_enum(),
                    di_type: self
                        .debug
                        .debug_builder
                        .create_pointer_type(
                            "Pointer",
                            placeholder_pointee,
                            self.builtin_types.ptr_sized_int.get_bit_width() as u64,
                            self.builtin_types.ptr_sized_int.get_bit_width(),
                            AddressSpace::default(),
                        )
                        .as_type(),
                }))
            }
            Type::Closure(closure_type) => {
                let closure_object_type = self
                    .module
                    .types
                    .get(closure_type.closure_object_type)
                    .as_closure_object()
                    .unwrap();
                let struct_type =
                    self.codegen_type(closure_object_type.struct_representation)?.expect_struct();

                Ok(LlvmType::Closure(LlvmClosureType {
                    type_id,
                    struct_type: struct_type.struct_type,
                    di_type: struct_type.di_type,
                    size: struct_type.size,
                }))
            }
            Type::ClosureObject(closure_object_type) => {
                let struct_type =
                    self.codegen_type(closure_object_type.struct_representation)?.expect_struct();
                Ok(LlvmType::Closure(LlvmClosureType {
                    type_id,
                    struct_type: struct_type.struct_type,
                    di_type: struct_type.di_type,
                    size: struct_type.size,
                }))
            }
            Type::Generic(_) => {
                panic!("Cannot codegen a Generic; something went wrong in typecheck")
            }
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

    fn make_llvm_function_type(
        &self,
        function_type_id: TypeId,
    ) -> CodegenResult<LlvmFunctionType<'ctx>> {
        let function_type = self.module.types.get(function_type_id).as_function().unwrap();
        let return_type = self.codegen_type(function_type.return_type)?;
        let params: CodegenResult<Vec<BasicMetadataTypeEnum<'ctx>>> = function_type
            .params
            .iter()
            .map(|p| {
                self.codegen_type(p.type_id)
                    .map(|t| BasicMetadataTypeEnum::from(t.physical_value_type()))
            })
            .collect();
        let params: Vec<_> = params?;
        let fn_type = return_type.fn_type(&params, false);
        Ok(fn_type)
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

    fn codegen_let(&mut self, let_stmt: &LetStmt) -> CodegenResult<LlvmValue<'ctx>> {
        let value = self.codegen_expr(&let_stmt.initializer)?;

        if let LlvmValue::Never(instr) = value {
            return Ok(LlvmValue::Never(instr));
        }

        // If this is a let*, then we put the rhs behind another alloca so that we end up
        // with a pointer to the value
        let value = if let_stmt.is_referencing {
            let basic_value = value.expect_basic_value();
            let value_ptr = self.builder.build_alloca(basic_value.get_type(), "");
            self.builder.build_store(value_ptr, basic_value);
            value_ptr.as_basic_value_enum()
        } else {
            value.expect_basic_value()
        };

        let variable_type = self.codegen_type(let_stmt.variable_type)?;
        let variable = self.module.variables.get_variable(let_stmt.variable_id);
        let variable_ptr =
            self.builder.build_alloca(value.get_type(), self.get_ident_name(variable.name));

        trace!(
            "codegen_let referencing={} {}: pointee_ty: {variable_type:?}",
            let_stmt.is_referencing,
            self.get_ident_name(variable.name)
        );

        // We're always storing a pointer in self.variables that, when loaded, gives the actual type of the variable
        let store_instr = self.builder.build_store(variable_ptr, value);

        if !self.module.name_of(variable.name).starts_with("__") {
            self.debug.debug_builder.insert_declare_before_instruction(
                variable_ptr,
                Some(self.debug.debug_builder.create_auto_variable(
                    self.debug.current_scope(),
                    self.get_ident_name(variable.name),
                    self.debug.current_file(),
                    self.get_line_number(let_stmt.span),
                    variable_type.debug_type(),
                    true,
                    0,
                    WORD_SIZE_BITS as u32,
                )),
                None,
                self.builder.get_current_debug_location().unwrap(),
                store_instr,
            );
        }
        let pointer = Pointer {
            pointer: variable_ptr,
            pointee_type_id: let_stmt.variable_type,
            pointee_llvm_type: variable_type.physical_value_type(),
        };
        self.variables.insert(let_stmt.variable_id, pointer);
        Ok(variable_ptr.into())
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
        let consequent_incoming = match consequent_block_value.as_basic_value() {
            Either::Left(_) => None,
            Either::Right(value) => {
                let consequent_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block);
                Some((consequent_final_block, value))
            }
        };

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_block_value = self.codegen_expr(&ir_if.alternate)?;
        let alternate_incoming = match alternate_block_value.as_basic_value() {
            Either::Left(_) => None,
            Either::Right(value) => {
                let alternate_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block);
                Some((alternate_final_block, value))
            }
        };

        // Merge block
        self.builder.position_at_end(merge_block);

        if ir_if.ty == NEVER_TYPE_ID {
            // Both branches are divergent
            let unreachable = self.builder.build_unreachable();
            Ok(LlvmValue::Never(unreachable))
        } else {
            let phi_value = self.builder.build_phi(typ.physical_value_type(), "if_phi");
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
                panic!("No incoming blocks even though type is not never")
            } else {
                Ok(LlvmValue::BasicValue(phi_value.as_basic_value()))
            }
        }
    }

    /// We expect this expr to produce a value, rather than exiting the program or otherwise crashing
    /// This is the most common case
    fn codegen_expr_basic_value(
        &mut self,
        expr: &TypedExpr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        Ok(self.codegen_expr(expr)?.expect_basic_value())
    }

    fn codegen_expr(&mut self, expr: &TypedExpr) -> CodegenResult<LlvmValue<'ctx>> {
        self.set_debug_location(expr.get_span());
        trace!("codegen expr\n{}", self.module.expr_to_string(expr));
        match expr {
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
                let string_wrapper_struct = string_type.physical_value_type().into_struct_type();
                let char_buffer_struct =
                    string_wrapper_struct.get_field_type_at_index(0).unwrap().into_struct_type();

                // Ensure the string layout is what we expect
                // deftype string = { buffer: Buffer[char] }
                debug_assert!(
                    char_buffer_struct
                        .get_field_type_at_index(0)
                        .unwrap()
                        .into_int_type()
                        .get_bit_width()
                        == 64
                );
                debug_assert!(char_buffer_struct
                    .get_field_type_at_index(1)
                    .unwrap()
                    .is_pointer_type());
                debug_assert!(char_buffer_struct.count_fields() == 2);

                let global_str_data = self.llvm_module.add_global(
                    self.builtin_types.char.array_type(string_value.len() as u32),
                    None,
                    "str_data",
                );
                global_str_data.set_initializer(&i8_array_from_str(self.ctx, string_value));
                global_str_data.set_constant(true);
                let global_str_value = string_wrapper_struct
                    .const_named_struct(&[char_buffer_struct
                        .const_named_struct(&[
                            self.builtin_types
                                .int
                                .const_int(string_value.len() as u64, true)
                                .into(),
                            global_str_data.as_pointer_value().into(),
                        ])
                        .as_basic_value_enum()])
                    .as_basic_value_enum();
                let global_value = self.llvm_module.add_global(string_wrapper_struct, None, "str");
                global_value.set_constant(true);
                global_value.set_initializer(&global_str_value);
                let loaded = self.builder.build_load(
                    string_wrapper_struct,
                    global_value.as_pointer_value(),
                    "",
                );
                Ok(loaded.into())
            }
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
            TypedExpr::Struct(struc) => {
                let struct_llvm_type =
                    self.codegen_type(struc.type_id)?.physical_value_type().into_struct_type();
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
                let name = &format!(
                    "struc.{}",
                    self.module.ast.identifiers.get_name(field_access.target_field)
                );
                if field_access.is_referencing {
                    // Codegen the field's memory location
                    let struc = self.codegen_expr_basic_value(&field_access.base)?;
                    let struct_pointer = struc.into_pointer_value();
                    let struct_llvm_type = self.codegen_type(field_access.struct_type)?;
                    let struct_physical_type = struct_llvm_type.expect_struct().struct_type;
                    let field_pointer = self
                        .builder
                        .build_struct_gep(
                            struct_physical_type,
                            struct_pointer,
                            field_access.target_field_index,
                            name,
                        )
                        .unwrap();
                    Ok(field_pointer.into())
                } else {
                    // Codegen the field's loaded value
                    let struc = self.codegen_expr_basic_value(&field_access.base)?;
                    let struct_struct = struc.into_struct_value();
                    let field_value = self
                        .builder
                        .build_extract_value(struct_struct, field_access.target_field_index, name)
                        .unwrap();
                    Ok(field_value.into())
                }
            }
            TypedExpr::If(if_expr) => self.codegen_if_else(if_expr),
            TypedExpr::WhileLoop(while_expr) => self.codegen_while_expr(while_expr),
            TypedExpr::LoopExpr(loop_expr) => self.codegen_loop_expr(loop_expr),
            TypedExpr::BinaryOp(bin_op) => self.codegen_binop(bin_op),
            TypedExpr::UnaryOp(unary_op) => {
                let value = self.codegen_expr_basic_value(&unary_op.expr)?;
                match unary_op.kind {
                    UnaryOpKind::Dereference => {
                        let value_ptr = value.into_pointer_value();
                        let pointee_ty = self.codegen_type(unary_op.type_id)?;
                        let value = self.builder.build_load(
                            pointee_ty.physical_value_type(),
                            value_ptr,
                            "deref",
                        );
                        Ok(value.into())
                    }
                }
            }
            TypedExpr::Block(block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and yield the result value
                let block_value = self.codegen_block(block)?;
                Ok(block_value)
            }
            TypedExpr::Call(call) => self.codegen_function_call(call),
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
                        &format!("enum_tag_{}", self.get_ident_name(variant_tag_name)),
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
                            &format!("enum_payload_{}", self.get_ident_name(variant_tag_name)),
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
                let enum_type = self
                    .module
                    .types
                    .get_type_id_dereferenced(enum_get_payload.target_expr.get_type());
                let enum_type = self.codegen_type(enum_type)?.expect_enum();
                let enum_value = self.codegen_expr_basic_value(&enum_get_payload.target_expr)?;
                let variant_type =
                    &enum_type.variant_structs[enum_get_payload.variant_index as usize];

                if enum_get_payload.is_referencing {
                    let enum_value = enum_value.into_pointer_value();
                    let payload_pointer =
                        self.get_enum_payload_reference(variant_type.struct_type, enum_value);
                    Ok(payload_pointer.into())
                } else {
                    let enum_value = enum_value.into_struct_value();
                    let value_payload = self.get_enum_payload(variant_type.struct_type, enum_value);
                    Ok(value_payload.into())
                }
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
                                llvm_type.physical_value_type().into_int_type(),
                                "extend_cast",
                            )
                        } else {
                            self.builder.build_int_z_extend(
                                int_value,
                                llvm_type.physical_value_type().into_int_type(),
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
                            int_type.physical_value_type().into_int_type(),
                            "trunc_cast",
                        );
                        Ok(truncated_value.as_basic_value_enum().into())
                    }
                    CastType::FloatExtend => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_float_value();
                        let float_dst_type = self
                            .codegen_type(cast.target_type_id)?
                            .physical_value_type()
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
                            .physical_value_type()
                            .into_float_type();
                        let extended_value =
                            self.builder.build_float_trunc(from_value, float_dst_type, "ftrunc");
                        Ok(extended_value.as_basic_value_enum().into())
                    }
                    CastType::FloatToInteger => {
                        let from_value =
                            self.codegen_expr_basic_value(&cast.base_expr)?.into_float_value();
                        let int_dst_type = self.codegen_type(cast.target_type_id)?;
                        let int_dst_type_llvm = int_dst_type.physical_value_type().into_int_type();
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
                            float_dst_type.physical_value_type().into_float_type();
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
            TypedExpr::Break(brk) => {
                let loop_info = *self.loops.get(&brk.loop_scope).unwrap();
                let break_value = self.codegen_expr_basic_value(&brk.value)?;
                if let Some(break_value_ptr) = loop_info.break_value_ptr {
                    self.builder.build_store(break_value_ptr, break_value);
                }
                let branch_inst = self.builder.build_unconditional_branch(loop_info.end_block);
                Ok(LlvmValue::Never(branch_inst))
            }
            TypedExpr::Closure(closure_expr) => {
                let closure_type =
                    self.module.types.get(closure_expr.closure_type).as_closure().unwrap();
                let llvm_fn = self.codegen_function_or_get(closure_type.body_function_id)?;
                let environment =
                    self.codegen_expr_basic_value(&closure_type.environment_struct)?;
                let environment_ptr = self.builder.build_alloca(environment.get_type(), "env");
                self.closure_environments.insert(closure_expr.closure_type, environment_ptr);
                self.builder.build_store(environment_ptr, environment);

                // The struct type for a closure's physical representation
                // What I could do is distinguish between static and dynamic here
                // I would introduce a new LlvmType representing closure values
                // that would be an enum, w/ statically known vs dynamic.
                // For static you'd have a FunctionValue and environment
                // For dynamic you'd have both the fn ptr and the env ptr
                //
                // As long as we don't actually make the call dynamically, it shouldn't matter
                // too much though

                let closure_object_type = self
                    .module
                    .types
                    .get(closure_type.closure_object_type)
                    .as_closure_object()
                    .unwrap();
                let closure_struct_type =
                    self.codegen_type(closure_object_type.struct_representation)?.expect_struct();

                let ptr = self.builder.build_alloca(closure_struct_type.struct_type, "closure");
                let fn_ptr = self
                    .builder
                    .build_struct_gep(closure_struct_type.struct_type, ptr, 0, "closure_fn")
                    .unwrap();
                let env_ptr = self
                    .builder
                    .build_struct_gep(closure_struct_type.struct_type, ptr, 1, "closure_env")
                    .unwrap();

                self.builder.build_store(fn_ptr, llvm_fn.as_global_value().as_pointer_value());
                self.builder.build_store(env_ptr, environment_ptr);

                let closure_struct =
                    self.builder.build_load(closure_struct_type.struct_type, ptr, "closure");

                Ok(LlvmValue::BasicValue(closure_struct))
            }
            TypedExpr::FunctionName(fn_name_expr) => {
                let function_value = self.codegen_function_or_get(fn_name_expr.function_id)?;
                let function_ptr =
                    function_value.as_global_value().as_pointer_value().as_basic_value_enum();
                Ok(function_ptr.into())
            }
            e @ TypedExpr::PendingCapture(_) => {
                panic!("Unsupported expression: {e:?}")
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

    fn get_enum_payload_reference(
        &self,
        variant_type: StructType<'ctx>,
        enum_pointer: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let payload_ptr = self
            .builder
            .build_struct_gep(variant_type, enum_pointer, 1, "get_payload_ptr")
            .unwrap();
        payload_ptr
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

        // Cannot cast aggregate types :'(
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
        let typed_function = call.callee.maybe_function_id().map(|f| self.module.get_function(f));
        if let Some(intrinsic_type) = typed_function.and_then(|f| f.intrinsic_type) {
            if intrinsic_type.is_inlined() {
                return self.codegen_intrinsic_inline(intrinsic_type, call);
            }
        }

        let mut args: VecDeque<BasicMetadataValueEnum<'ctx>> =
            VecDeque::with_capacity(call.args.len() + 1);
        for arg_expr in call.args.iter() {
            let basic_value = self.codegen_expr_basic_value(arg_expr)?;
            trace!(
                "codegen function call arg: {}: {}",
                self.module.expr_to_string(arg_expr),
                self.module.type_id_to_string(arg_expr.get_type()),
            );
            args.push_back(basic_value.into())
        }
        let function_type = self.module.get_callee_function_type(&call.callee);
        let llvm_function_type = self.make_llvm_function_type(function_type)?;
        let callsite_value = match &call.callee {
            Callee::StaticFunction(function_id) => {
                let function_value = self.codegen_function_or_get(*function_id)?;

                self.set_debug_location(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "")
            }
            Callee::StaticClosure { function_id, closure_type_id } => {
                let closure_env_ptr =
                    self.closure_environments.get(closure_type_id).unwrap().as_basic_value_enum();
                args.push_front(closure_env_ptr.into());

                let function_value = self.codegen_function_or_get(*function_id)?;

                self.set_debug_location(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "")
            }
            Callee::DynamicFunction(function_reference_expr) => {
                let function_ptr =
                    self.codegen_expr_basic_value(function_reference_expr)?.into_pointer_value();

                self.set_debug_location(call.span);
                self.builder.build_indirect_call(
                    llvm_function_type,
                    function_ptr,
                    args.make_contiguous(),
                    "",
                )
            }
            Callee::DynamicClosure(callee_struct_expr) => {
                let callee_struct =
                    self.codegen_expr_basic_value(callee_struct_expr)?.into_struct_value();
                let fn_ptr = self
                    .builder
                    .build_extract_value(callee_struct, 0, "fn_ptr")
                    .unwrap()
                    .into_pointer_value();
                let closure_env_ptr =
                    self.builder.build_extract_value(callee_struct, 1, "env_ptr").unwrap();
                args.push_front(closure_env_ptr.into());

                // self.set_debug_location(call.span);
                self.builder.build_indirect_call(
                    llvm_function_type,
                    fn_ptr,
                    args.make_contiguous(),
                    "",
                )
            }
        };
        match callsite_value.try_as_basic_value() {
            either::Either::Left(value) => Ok(LlvmValue::BasicValue(value)),
            either::Either::Right(_instr) => {
                if call.return_type == NEVER_TYPE_ID {
                    let unreachable = self.builder.build_unreachable();
                    Ok(LlvmValue::Never(unreachable))
                } else {
                    panic!("Function returned LLVM void but wasn't typed as never")
                }
            }
        }
    }

    #[allow(unused)]
    fn const_string_ptr(&self, string: &str, name: &str) -> PointerValue<'ctx> {
        let char_data = self.ctx.const_string(string.as_bytes(), false);
        let length_value = self.builtin_types.int.const_int(string.len() as u64, false);
        let char_data_global =
            self.llvm_module.add_global(char_data.get_type(), None, &format!("{name}_data"));
        char_data_global.set_initializer(&char_data);
        let string_type = self.codegen_type(STRING_TYPE_ID).unwrap();
        let string_struct_type = string_type.physical_value_type().into_struct_type();
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
        let string_struct_type = string_type.physical_value_type().into_struct_type();
        let ptr = self.const_string_ptr(string, name);
        self.builder
            .build_load(string_struct_type, ptr, &format!("{name}_loaded"))
            .into_struct_value()
    }

    fn codegen_intrinsic_inline(
        &mut self,
        intrinsic_type: IntrinsicFunction,
        call: &Call,
    ) -> CodegenResult<LlvmValue<'ctx>> {
        match intrinsic_type {
            IntrinsicFunction::SizeOf
            | IntrinsicFunction::SizeOfStride
            | IntrinsicFunction::AlignOf => {
                let type_param = &call.type_args[0];
                let llvm_type = self.codegen_type(type_param.type_id)?;
                let size = self.size_info(&llvm_type.value_any_type());
                let num_bits = match intrinsic_type {
                    IntrinsicFunction::SizeOf => size.size_bits,
                    IntrinsicFunction::SizeOfStride => size.stride_bits,
                    IntrinsicFunction::AlignOf => size.align_bits,
                    _ => unreachable!(),
                };
                let num_bytes = num_bits / 8;
                let size_value = self.builtin_types.int.const_int(num_bytes as u64, false);
                Ok(size_value.as_basic_value_enum().into())
            }
            IntrinsicFunction::BoolNegate => {
                let input_value = self.codegen_expr_basic_value(&call.args[0])?.into_int_value();
                let truncated = self.bool_to_i1(input_value, "");
                let negated = self.builder.build_not(truncated, "");
                let promoted = self.i1_to_bool(negated, "");
                Ok(promoted.as_basic_value_enum().into())
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
            IntrinsicFunction::PointerIndex => {
                //  Reference:
                //  intern fn refAtIndex[T](self: Pointer, index: u64): T*
                let pointee_ty_arg = call.type_args[0];
                let elem_type = self.codegen_type(pointee_ty_arg.type_id)?;
                let ptr = self.codegen_expr_basic_value(&call.args[0])?.into_pointer_value();
                let index = self.codegen_expr_basic_value(&call.args[1])?.into_int_value();
                let result_pointer = unsafe {
                    self.builder.build_in_bounds_gep(
                        elem_type.physical_value_type(),
                        ptr,
                        &[index],
                        "refAtIndex",
                    )
                };
                Ok(result_pointer.as_basic_value_enum().into())
            }
        }
    }

    fn codegen_intrinsic_function_body(
        &mut self,
        intrinsic_type: IntrinsicFunction,
        _function: &TypedFunction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        panic!("Unexpected non-inline intrinsic {:?}", intrinsic_type)
    }

    fn codegen_block(&mut self, block: &TypedBlock) -> CodegenResult<LlvmValue<'ctx>> {
        let unit_value = self.builtin_types.unit_value.as_basic_value_enum().into();
        let mut last: LlvmValue<'ctx> = unit_value;
        self.set_debug_location(block.span);
        for stmt in &block.statements {
            if let LlvmValue::Never(_never_instr) = last {
                eprintln!("Aborting block after generating {}", _never_instr);
                return Ok(last);
            }
            match stmt {
                TypedStmt::Expr(expr) => last = self.codegen_expr(expr)?,
                TypedStmt::Let(val_def) => {
                    let value = self.codegen_let(val_def)?;
                    last = if val_def.variable_type == NEVER_TYPE_ID { value } else { unit_value };
                }
                TypedStmt::Assignment(assignment) => {
                    let rhs = self.codegen_expr_basic_value(&assignment.value)?;
                    let lhs_pointer = match assignment.kind {
                        AssignmentKind::Value => {
                            match assignment.destination.deref() {
                                // ASSIGNMENT! We're in lvalue land. We need to get the pointer to the
                                // destination, and be sure to call the correct variant of codegen_expr
                                TypedExpr::Variable(v) => {
                                    let destination_ptr = *self
                                        .variables
                                        .get(&v.variable_id)
                                        .expect("Missing variable");
                                    destination_ptr.pointer
                                }
                                e => {
                                    panic!(
                                        "Invalid assignment lhs: {}",
                                        self.module.expr_to_string(e)
                                    )
                                }
                            }
                        }
                        AssignmentKind::Reference => self
                            .codegen_expr_basic_value(&assignment.destination)?
                            .into_pointer_value(),
                    };

                    self.builder.build_store(lhs_pointer, rhs);
                    last = unit_value;
                }
            }
        }
        Ok(last)
    }

    fn codegen_while_expr(&mut self, while_loop: &WhileLoop) -> CodegenResult<LlvmValue<'ctx>> {
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let loop_entry_block = self.ctx.append_basic_block(current_fn, "while_cond");
        let loop_body_block = self.ctx.append_basic_block(current_fn, "while_body");
        let loop_end_block = self.ctx.append_basic_block(current_fn, "while_end");

        self.loops.insert(
            while_loop.body.scope_id,
            LoopInfo { break_value_ptr: None, end_block: loop_end_block },
        );

        // Go to the loop
        self.builder.build_unconditional_branch(loop_entry_block);

        self.builder.position_at_end(loop_entry_block);
        let cond = self.codegen_expr_basic_value(&while_loop.cond)?.into_int_value();
        let cond_i1 = self.bool_to_i1(cond, "while_cond");

        self.builder.build_conditional_branch(cond_i1, loop_body_block, loop_end_block);

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(&while_loop.body)?;
        match body_value.as_basic_value() {
            Either::Left(_instr) => {}
            Either::Right(_bv) => {
                self.builder.build_unconditional_branch(loop_entry_block);
            }
        }

        self.builder.position_at_end(loop_end_block);
        Ok(self.builtin_types.unit_value.as_basic_value_enum().into())
    }

    fn codegen_loop_expr(&mut self, loop_expr: &LoopExpr) -> CodegenResult<LlvmValue<'ctx>> {
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        // let loop_entry_block = self.ctx.append_basic_block(current_fn, "loop_entry");
        let loop_body_block = self.ctx.append_basic_block(current_fn, "loop_body");
        let loop_end_block = self.ctx.append_basic_block(current_fn, "loop_end");

        let break_type = self.codegen_type(loop_expr.break_type)?;
        // Optimization: skip if unit type
        let break_value_ptr = self.builder.build_alloca(break_type.physical_value_type(), "break");
        self.loops.insert(
            loop_expr.body.scope_id,
            LoopInfo { break_value_ptr: Some(break_value_ptr), end_block: loop_end_block },
        );

        // Go to the body
        self.builder.build_unconditional_branch(loop_body_block);

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(&loop_expr.body)?;
        match body_value.as_basic_value() {
            Either::Left(_instr) => {}
            Either::Right(_bv) => {
                self.builder.build_unconditional_branch(loop_body_block);
            }
        }

        self.builder.position_at_end(loop_end_block);
        let break_value = self.builder.build_load(
            break_type.physical_value_type(),
            break_value_ptr,
            "loop_value",
        );
        Ok(break_value.into())
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
        let span_id = self.module.ast.get_span_for_id(function.parsed_id);
        let function_file_id = self.module.ast.spans.get(span_id).file_id;
        let (function_line, _) =
            self.module.ast.get_lines_for_span_id(span_id).expect("line for span");
        let function_line_number = function_line.line_number();
        let function_scope_start_line_number = function_line_number;
        let function_file = self.debug.files.get(&function_file_id).unwrap();
        let dbg_fn_type = self.debug.debug_builder.create_subroutine_type(
            *function_file,
            Some(return_type),
            dbg_param_types,
            0,
        );
        let di_subprogram = self.debug.debug_builder.create_function(
            self.debug.current_scope(),
            self.module.ast.identifiers.get_name(function.name),
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

    fn codegen_function_or_get(
        &mut self,
        function_id: FunctionId,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(function.function_value);
        }
        debug!("\ncodegen function\n{}", self.module.function_id_to_string(function_id, true));

        let function = self.module.get_function(function_id);
        let function_type = self.module.types.get(function.type_id).as_function().unwrap();

        let function_line_number = self
            .module
            .ast
            .get_lines_for_span_id(self.module.ast.get_span_for_id(function.parsed_id))
            .expect("line for span")
            .1
            .line_number();

        let maybe_starting_block = self.builder.get_insert_block();

        let mut param_types: Vec<LlvmType<'ctx>> = Vec::with_capacity(function_type.params.len());
        let mut param_metadata_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            Vec::with_capacity(function_type.params.len());

        for param in function_type.params.iter() {
            let res = self.codegen_type(param.type_id)?;
            param_metadata_types.push(BasicMetadataTypeEnum::from(res.physical_value_type()));
            param_types.push(res);
        }
        let return_type = self.codegen_type(function_type.return_type)?;

        let llvm_name = match function.linkage {
            TyperLinkage::External(Some(name)) => self.module.name_of(name),
            _ => &self.module.make_qualified_name(function.scope, function.name, ".", true),
        };
        let fn_ty = return_type.fn_type(&param_metadata_types, false);
        let llvm_linkage = match function.linkage {
            TyperLinkage::Standard => None,
            TyperLinkage::External(_name) => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        if self.llvm_module.get_function(llvm_name).is_some() {
            return err!(
                self.module.ast.get_span_for_id(function.parsed_id),
                "Dupe function name: {}",
                llvm_name
            );
        }
        let function_value = self.llvm_module.add_function(llvm_name, fn_ty, llvm_linkage);

        self.llvm_functions
            .insert(function_id, CodegenedFunction { function_value, instruction_count: 0 });

        if matches!(function.linkage, Linkage::External(_)) {
            return Ok(function_value);
        }

        let di_subprogram = self.push_function_debug_info(function, &return_type, &param_types)?;

        let entry_block = self.ctx.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);
        for (i, param) in function_value.get_param_iter().enumerate() {
            let typed_param = &function_type.params[i];
            let ty = self.codegen_type(typed_param.type_id)?;
            let param_name = self.module.ast.identifiers.get_name(typed_param.name);
            trace!(
                "Got LLVM type for variable {}: {} (from {})",
                param_name,
                ty.physical_value_type(),
                self.module.type_id_to_string(typed_param.type_id)
            );
            param.set_name(param_name);
            self.set_debug_location(typed_param.span);
            let pointer = self.builder.build_alloca(ty.physical_value_type(), param_name);
            let arg_debug_type = self.get_debug_type(typed_param.type_id)?;
            let di_local_variable = self.debug.debug_builder.create_parameter_variable(
                self.debug.current_scope(),
                param_name,
                i as u32,
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
            let variable_id = function.param_variables[i];
            self.variables.insert(
                variable_id,
                Pointer {
                    pointer,
                    pointee_type_id: typed_param.type_id,
                    pointee_llvm_type: ty.physical_value_type(),
                },
            );
        }
        let _terminator_instr = match function.intrinsic_type {
            Some(intrinsic_type) => {
                trace!("codegen intrinsic {:?} fn {:?}", intrinsic_type, function);
                let value = self
                    .codegen_intrinsic_function_body(intrinsic_type, function)?
                    .as_basic_value_enum();
                LlvmValue::Never(self.builder.build_return(Some(&value)))
            }
            None => {
                let function_block = function.body_block.as_ref().unwrap_or_else(|| {
                    panic!("Function has no block {}", self.get_ident_name(function.name))
                });
                self.codegen_block(function_block)?
            }
        };
        if let Some(start_block) = maybe_starting_block {
            self.builder.position_at_end(start_block);
        }
        self.debug.pop_scope();
        function_value.set_subprogram(di_subprogram);

        {
            let mut count = 0;
            let mut cur_blk: Option<BasicBlock<'ctx>> = function_value.get_first_basic_block();
            loop {
                let Some(blk) = cur_blk else { break };
                let mut cur_inst = blk.get_first_instruction();
                while let Some(inst) = cur_inst {
                    count += 1;
                    cur_inst = inst.get_next_instruction();
                }
                cur_blk = blk.get_next_basic_block();
            }
            self.llvm_functions.get_mut(&function_id).unwrap().instruction_count = count;
        }
        Ok(function_value)
    }

    fn codegen_integer_value(
        &self,
        integer: &TypedIntegerExpr,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(integer.get_type())?;
        let llvm_int_ty = llvm_ty.physical_value_type().into_int_type();
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
        let llvm_float_ty = llvm_ty.physical_value_type().into_float_type();
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
                        self.module.name_of(variable.name),
                    );
                    llvm_global.set_constant(true);
                    llvm_global.set_initializer(&llvm_value);
                    self.globals.insert(constant.variable_id, llvm_global);
                }
                _ => unimplemented!("constants must be integers"),
            }
        }
        // TODO: Codegen only the exported functions as well as the necessary ones
        // for (id, function) in self.module.function_iter() {
        //     if function.is_concrete {
        //         self.codegen_function_or_get(id)?;
        //     }
        // }
        self.codegen_function_or_get(self.module.get_main_function_id().unwrap())?;
        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    fn set_up_machine(module: &mut LlvmModule) -> TargetMachine {
        // Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        // let triple = TargetMachine::get_default_triple();
        let triple = TargetTriple::create("arm64-apple-macosx14.4.0");
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
            module_pass_manager.add_scalar_repl_aggregates_pass_ssa();
            module_pass_manager.add_promote_memory_to_register_pass();
            module_pass_manager.add_new_gvn_pass();
            module_pass_manager.add_aggressive_inst_combiner_pass();
            module_pass_manager.add_memcpy_optimize_pass();
            module_pass_manager.add_scoped_no_alias_aa_pass();
            module_pass_manager.add_type_based_alias_analysis_pass();
            module_pass_manager.add_sccp_pass();
            module_pass_manager.add_aggressive_dce_pass();
            module_pass_manager.add_ind_var_simplify_pass();
            module_pass_manager.add_loop_idiom_pass();
            module_pass_manager.add_loop_unroll_pass();
            module_pass_manager.add_function_inlining_pass();
            module_pass_manager.add_cfg_simplification_pass();
        }

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
        let return_value = unsafe { engine.run_function(llvm_function.function_value, &[]) };
        let res: u64 = return_value.as_int(true);
        Ok(res)
    }
}
