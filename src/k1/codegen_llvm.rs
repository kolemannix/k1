use std::cell::RefCell;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::Path;

use ahash::HashMapExt;
use anyhow::bail;
use either::Either;
use fxhash::FxHashMap;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFile, DILocation, DIScope, DISubprogram, DIType, DWARFEmissionKind,
    DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType, VoidType,
};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
    InstructionValue, IntValue, PointerValue,
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

macro_rules! failf {
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

macro_rules! errf {
    ($span:expr, $($format_args:expr),*) => {
        {
            let s: String = format!($($format_args),*);
            CodegenError {
                message: s,
                span: $span,
            }
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
    let sz = SizeInfo {
        stride_bits: td.get_abi_size(typ) as u32 * 8,
        size_bits: td.get_bit_size(typ) as u32,
        pref_align_bits: td.get_preferred_alignment(typ) * 8,
        abi_align_bits: td.get_abi_alignment(typ) * 8,
    };
    // if sz.pref_align_bits != sz.abi_align_bits {
    //     // For some reason, llvm defaults the preferred alignment of all structs to 64 bits
    //     // "a:0:64" in the default datalayout string
    //     let is_single_member_struct = if typ.as_any_type_enum().is_struct_type() {
    //         typ.as_any_type_enum().into_struct_type().count_fields() == 1
    //     } else {
    //         false
    //     };
    //     if !is_single_member_struct {
    //         info!(
    //         "Type has different preferred and abi alignments, and is not a single-member struct. Thought you'd like to know.\n{}. pref={}, abi={}",
    //         typ.print_to_string(),
    //         sz.pref_align_bits,
    //         sz.abi_align_bits
    //     )
    //     }
    // };
    sz
}

trait BuilderResultExt {
    type V;
    fn to_err(self, span: SpanId) -> CodegenResult<Self::V>;
}

impl<T> BuilderResultExt for Result<T, BuilderError> {
    type V = T;
    fn to_err(self, span: SpanId) -> CodegenResult<Self::V> {
        self.map_err(|e| errf!(span, "LLVM Error: {}", e))
    }
}

// TODO: cache these by type id?
pub struct K1LlvmFunctionType<'ctx> {
    llvm_function_type: LlvmFunctionType<'ctx>,
    param_types: Vec<K1LlvmType<'ctx>>,
    return_type: K1LlvmType<'ctx>,
    is_sret: bool,
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
    Void(InstructionValue<'ctx>),
}
impl<'ctx> LlvmValue<'ctx> {
    fn as_basic_value(self) -> Either<InstructionValue<'ctx>, BasicValueEnum<'ctx>> {
        match self {
            LlvmValue::BasicValue(bv) => Either::Right(bv),
            LlvmValue::Void(instr) => Either::Left(instr),
        }
    }
    fn expect_basic_value(self) -> BasicValueEnum<'ctx> {
        self.as_basic_value().expect_right("Expected BasicValue on never value")
    }

    #[allow(unused)]
    fn as_never(&self) -> Option<InstructionValue<'ctx>> {
        match self {
            LlvmValue::Void(instr) => Some(*instr),
            _ => None,
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
    /// Stride represents the space between the start of 2 successive elements, it bumps to the
    /// next aligned slot. This must be used by builtin Buffer functionality in order to be correct
    stride_bits: u32,
    pref_align_bits: u32,
    abi_align_bits: u32,
}
impl SizeInfo {
    pub const POINTER: SizeInfo = SizeInfo {
        size_bits: WORD_SIZE_BITS as u32,
        stride_bits: WORD_SIZE_BITS as u32,
        pref_align_bits: WORD_SIZE_BITS as u32,
        abi_align_bits: WORD_SIZE_BITS as u32,
    };
    pub const ZERO: SizeInfo =
        SizeInfo { size_bits: 0, stride_bits: 0, pref_align_bits: 0, abi_align_bits: 0 };

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
            if size.pref_align_bits > max_align {
                max_align = size.pref_align_bits
            }
        }
        SizeInfo {
            size_bits: total_size,
            stride_bits: total_size,
            pref_align_bits: max_align,
            abi_align_bits: max_align,
        }
    }

    pub fn scalar_value(size_bits: u32) -> SizeInfo {
        SizeInfo {
            size_bits,
            stride_bits: size_bits,
            pref_align_bits: size_bits,
            abi_align_bits: size_bits,
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct LlvmReferenceType<'ctx> {
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

#[derive(Debug, Clone)]
struct EnumVariantType<'ctx> {
    name: Identifier,
    envelope_type: StructType<'ctx>,
    variant_struct_type: StructType<'ctx>,
    payload_type: Option<K1LlvmType<'ctx>>,
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
    variants: Vec<EnumVariantType<'ctx>>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

#[derive(Debug, Clone)]
struct LlvmStructType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    fields: Vec<K1LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

// Can this just be 'struct'? :()
#[derive(Debug, Clone)]
struct LlvmLambdaObjectType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    di_type: DIType<'ctx>,
    size: SizeInfo,
}

#[derive(Debug, Clone)]
enum K1LlvmType<'ctx> {
    Value(LlvmValueType<'ctx>),
    EnumType(LlvmEnumType<'ctx>),
    StructType(LlvmStructType<'ctx>),
    LambdaObject(LlvmLambdaObjectType<'ctx>),
    Reference(LlvmReferenceType<'ctx>),
    Void(LlvmVoidType<'ctx>),
}

impl<'ctx> From<LlvmVoidType<'ctx>> for K1LlvmType<'ctx> {
    fn from(void: LlvmVoidType<'ctx>) -> Self {
        K1LlvmType::Void(void)
    }
}

impl<'ctx> From<LlvmValueType<'ctx>> for K1LlvmType<'ctx> {
    fn from(value: LlvmValueType<'ctx>) -> Self {
        K1LlvmType::Value(value)
    }
}

impl<'ctx> From<LlvmEnumType<'ctx>> for K1LlvmType<'ctx> {
    fn from(value: LlvmEnumType<'ctx>) -> Self {
        K1LlvmType::EnumType(value)
    }
}

impl<'ctx> From<LlvmStructType<'ctx>> for K1LlvmType<'ctx> {
    fn from(value: LlvmStructType<'ctx>) -> Self {
        K1LlvmType::StructType(value)
    }
}

impl<'ctx> From<LlvmReferenceType<'ctx>> for K1LlvmType<'ctx> {
    fn from(value: LlvmReferenceType<'ctx>) -> Self {
        K1LlvmType::Reference(value)
    }
}

impl<'ctx> From<LlvmLambdaObjectType<'ctx>> for K1LlvmType<'ctx> {
    fn from(value: LlvmLambdaObjectType<'ctx>) -> Self {
        K1LlvmType::LambdaObject(value)
    }
}

impl<'ctx> K1LlvmType<'ctx> {
    #[allow(unused)]
    pub fn size_info(&self) -> SizeInfo {
        match self {
            K1LlvmType::Value(v) => v.size,
            K1LlvmType::EnumType(e) => e.size,
            K1LlvmType::StructType(s) => s.size,
            K1LlvmType::Reference(_p) => SizeInfo::POINTER,
            K1LlvmType::Void(_) => SizeInfo::ZERO,
            K1LlvmType::LambdaObject(c) => c.size,
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            K1LlvmType::Value(_) => false,
            K1LlvmType::EnumType(_) => true,
            K1LlvmType::StructType(_) => true,
            K1LlvmType::Reference(_) => false,
            K1LlvmType::Void(_) => false,
            K1LlvmType::LambdaObject(_) => true,
        }
    }

    #[allow(unused)]
    pub fn expect_pointer(&self) -> LlvmReferenceType<'ctx> {
        match self {
            K1LlvmType::Reference(pointer) => *pointer,
            _ => panic!("expected pointer on {self:?}"),
        }
    }

    pub fn expect_enum(self) -> LlvmEnumType<'ctx> {
        match self {
            K1LlvmType::EnumType(e) => e,
            _ => panic!("expected enum on {self:?}"),
        }
    }

    fn expect_struct(self) -> LlvmStructType<'ctx> {
        match self {
            K1LlvmType::StructType(s) => s,
            _ => panic!("expected struct on {self:?}"),
        }
    }

    fn expect_lambda_object(self) -> LlvmLambdaObjectType<'ctx> {
        match self {
            K1LlvmType::LambdaObject(lam_obj) => lam_obj,
            _ => panic!("expected struct on {self:?}"),
        }
    }

    fn fn_type(
        &self,
        args: &[BasicMetadataTypeEnum<'ctx>],
        varargs: bool,
    ) -> LlvmFunctionType<'ctx> {
        match self {
            K1LlvmType::Void(v) => v.void_type.fn_type(args, varargs),
            other => other.canonical_repr_type().fn_type(args, varargs),
        }
    }

    #[allow(unused)]
    fn type_id(&self) -> TypeId {
        match self {
            K1LlvmType::Value(value) => value.type_id,
            K1LlvmType::Reference(pointer) => pointer.type_id,
            K1LlvmType::EnumType(e) => e.type_id,
            K1LlvmType::StructType(s) => s.type_id,
            K1LlvmType::Void(_) => NEVER_TYPE_ID,
            K1LlvmType::LambdaObject(c) => c.type_id,
        }
    }

    fn canonical_repr_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.basic_type,
            K1LlvmType::Reference(pointer) => pointer.pointer_type.as_basic_type_enum(),
            K1LlvmType::EnumType(e) => e
                .base_struct_type
                .get_context()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            K1LlvmType::StructType(s) => {
                s.struct_type.get_context().ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
            K1LlvmType::Void(_) => panic!("No canonical repr type on Void / never"),
            K1LlvmType::LambdaObject(c) => {
                c.struct_type.get_context().ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
        }
    }

    fn rich_value_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.basic_type,
            K1LlvmType::Reference(pointer) => pointer.pointer_type.as_basic_type_enum(),
            K1LlvmType::EnumType(e) => e.base_struct_type.as_basic_type_enum(),
            K1LlvmType::StructType(s) => s.struct_type.as_basic_type_enum(),
            K1LlvmType::Void(_) => panic!("No rich value type on Void / never"),
            K1LlvmType::LambdaObject(c) => c.struct_type.as_basic_type_enum(),
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.di_type,
            K1LlvmType::Reference(pointer) => pointer.di_type,
            K1LlvmType::EnumType(e) => e.di_type,
            K1LlvmType::StructType(s) => s.di_type,
            K1LlvmType::Void(v) => v.di_type,
            K1LlvmType::LambdaObject(c) => c.di_type,
        }
    }

    #[allow(unused)]
    fn is_void(&self) -> bool {
        match self {
            K1LlvmType::Void(_) => true,
            _ => false,
        }
    }
}

struct BuiltinTypes<'ctx> {
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
    dynamic_lambda_object: StructType<'ctx>,
}

#[derive(Clone, Copy)]
pub struct LoopInfo<'ctx> {
    pub break_value_ptr: Option<PointerValue<'ctx>>,
    pub end_block: BasicBlock<'ctx>,
}

pub struct CodegenedFunction<'ctx> {
    pub function_type: K1LlvmFunctionType<'ctx>,
    pub function_value: FunctionValue<'ctx>,
    pub sret_pointer: Option<PointerValue<'ctx>>,
    pub instruction_count: usize,
}

pub struct Codegen<'ctx, 'module> {
    ctx: &'ctx Context,
    pub module: &'module TypedModule,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: TargetMachine,
    builder: Builder<'ctx>,
    pub llvm_functions: FxHashMap<FunctionId, CodegenedFunction<'ctx>>,
    pub llvm_function_to_k1: FxHashMap<FunctionValue<'ctx>, FunctionId>,
    llvm_types: RefCell<FxHashMap<TypeId, K1LlvmType<'ctx>>>,
    variables: FxHashMap<VariableId, VariablePointer<'ctx>>,
    globals: FxHashMap<VariableId, GlobalValue<'ctx>>,
    lambda_functions: FxHashMap<TypeId, FunctionValue<'ctx>>,
    loops: FxHashMap<ScopeId, LoopInfo<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    debug: DebugContext<'ctx>,
}

struct DebugContext<'ctx> {
    files: FxHashMap<FileId, DIFile<'ctx>>,
    debug_builder: DebugInfoBuilder<'ctx>,
    #[allow(unused)]
    compile_unit: DICompileUnit<'ctx>,
    debug_stack: Vec<DebugStackEntry<'ctx>>,
    #[allow(unused)]
    scopes: FxHashMap<ScopeId, DIScope<'ctx>>,
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
struct VariablePointer<'ctx> {
    #[allow(unused)]
    pointee_type_id: TypeId,
    pointee_llvm_type: BasicTypeEnum<'ctx>,
    pointer_value: PointerValue<'ctx>,
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

        let di_files: FxHashMap<FileId, DIFile> = module
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
            scopes: FxHashMap::new(),
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
    ) -> Self {
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

        let ptr = ctx.ptr_type(AddressSpace::default());
        let builtin_types = BuiltinTypes {
            int: ctx.i64_type(),
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
            ptr,
            ptr_sized_int: ctx.ptr_sized_int_type(&target_data, None),
            dynamic_lambda_object: ctx
                .struct_type(&[ptr.as_basic_type_enum(), ptr.as_basic_type_enum()], false),
        };

        Codegen {
            ctx,
            module,
            llvm_module,
            llvm_machine: machine,
            builder,
            variables: FxHashMap::new(),
            globals: FxHashMap::new(),
            lambda_functions: FxHashMap::new(),
            loops: FxHashMap::new(),
            llvm_functions: FxHashMap::new(),
            llvm_function_to_k1: FxHashMap::new(),
            llvm_types: RefCell::new(FxHashMap::new()),
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

    fn set_debug_location_from_span(&self, span: SpanId) -> DILocation<'ctx> {
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

    fn set_debug_location(&self, locn: DILocation<'ctx>) {
        self.builder.set_current_debug_location(locn)
    }

    fn get_debug_location(&self) -> DILocation<'ctx> {
        self.builder.get_current_debug_location().unwrap()
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
        // FIXME: We need to revisit the entire story around names in codegen
        let name = self.module.type_id_to_string(type_id);
        match defn_info {
            None => write!(w, "{}", name).unwrap(),
            Some(info) => self.module.write_qualified_name(w, info.scope, &name, "/", true),
        };
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
                size.pref_align_bits,
                0,
                None,
                fields,
                0,
                None,
                name,
            )
            .as_type()
    }

    fn make_pointer_type(&self, pointee_di_type: DIType<'ctx>) -> LlvmValueType<'ctx> {
        LlvmValueType {
            type_id: POINTER_TYPE_ID,
            basic_type: self.builtin_types.ptr.as_basic_type_enum(),
            size: self.size_info(&self.builtin_types.ptr),
            di_type: self
                .debug
                .debug_builder
                .create_pointer_type(
                    "Pointer",
                    pointee_di_type,
                    self.builtin_types.ptr_sized_int.get_bit_width() as u64,
                    self.builtin_types.ptr_sized_int.get_bit_width(),
                    AddressSpace::default(),
                )
                .as_type(),
        }
    }

    fn get_debug_type(&self, type_id: TypeId) -> CodegenResult<DIType<'ctx>> {
        Ok(self.codegen_type(type_id)?.debug_type())
    }

    #[allow(unused)]
    fn print_layout_info(&self) {
        eprintln!(
            "DATALAYOUT STR {}",
            self.llvm_machine.get_target_data().get_data_layout().as_str().to_string_lossy()
        );
        eprintln!("i8: {:?}", self.size_info(&self.ctx.i8_type()));
        eprintln!("i16: {:?}", self.size_info(&self.ctx.i16_type()));
        eprintln!("i32: {:?}", self.size_info(&self.ctx.i32_type()));
        eprintln!("i64: {:?}", self.size_info(&self.ctx.i64_type()));
        eprintln!("ptr: {:?}", self.size_info(&self.builtin_types.ptr));
        eprintln!(
            "{{i8}}: {:?}",
            self.size_info(
                &self.ctx.struct_type(&[self.ctx.i8_type().as_basic_type_enum()], false)
            )
        );
        eprintln!(
            "{{i16}}: {:?}",
            self.size_info(
                &self.ctx.struct_type(&[self.ctx.i16_type().as_basic_type_enum()], false)
            )
        );
        eprintln!(
            "{{i32}}: {:?}",
            self.size_info(
                &self.ctx.struct_type(&[self.ctx.i32_type().as_basic_type_enum()], false)
            )
        );
        eprintln!(
            "{{ i8, i32 }}: {:?}",
            self.size_info(&self.ctx.struct_type(
                &[
                    self.ctx.i8_type().as_basic_type_enum(),
                    self.ctx.i32_type().as_basic_type_enum()
                ],
                false
            ))
        );
        eprintln!(
            "{{i64}}: {:?}",
            self.size_info(
                &self.ctx.struct_type(&[self.ctx.i64_type().as_basic_type_enum()], false)
            )
        );
        eprintln!(
            "{{ptr}}: {:?}",
            self.size_info(
                &self.ctx.struct_type(&[self.builtin_types.ptr.as_basic_type_enum()], false)
            )
        );
    }

    fn codegen_type(&self, type_id: TypeId) -> CodegenResult<K1LlvmType<'ctx>> {
        self.codegen_type_inner(type_id, 0)
    }

    fn codegen_type_inner(&self, type_id: TypeId, depth: usize) -> CodegenResult<K1LlvmType<'ctx>> {
        let result = self.llvm_types.borrow().get(&type_id).cloned();
        if let Some(result) = result {
            return Ok(result);
        };
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
                K1LlvmType::Value(LlvmValueType {
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
                        dw_ate_boolean,
                        0,
                    )
                    .unwrap()
                    .as_type(),
            }
            .into()),
            Type::Pointer(_) => {
                // We don't know what it points to, so we just say 'bytes' for the best debugger
                // experience
                let placeholder_pointee = self.codegen_type(U8_TYPE_ID)?.debug_type();
                Ok(self.make_pointer_type(placeholder_pointee).into())
            }
            ts @ Type::Struct(struc) => {
                let buffer_instance = ts.as_buffer_instance();
                trace!("generating llvm type for struct type {type_id}");
                let field_count = struc.fields.len();
                let mut field_types = Vec::with_capacity(field_count);
                let mut field_basic_types = Vec::with_capacity(field_count);
                let mut field_di_types: Vec<StructDebugMember> = Vec::with_capacity(field_count);
                let name = self.codegen_type_name(type_id, struc.type_defn_info.as_ref());
                for field in &struc.fields {
                    let field_llvm_type = self.codegen_type_inner(field.type_id, depth + 1)?;
                    let debug_type = if buffer_instance.is_some()
                        && field.name
                            == self.module.ast.identifiers.get(BUFFER_DATA_FIELD_NAME).unwrap()
                        && field.type_id == POINTER_TYPE_ID
                    {
                        let buffer_instance = buffer_instance.unwrap();
                        let buffer_type_argument = buffer_instance.param_values[0];
                        let element_type =
                            self.codegen_type_inner(buffer_type_argument, depth + 1)?;
                        let array_ptr_di_type = self.make_pointer_type(element_type.debug_type());
                        array_ptr_di_type.di_type
                    } else {
                        field_llvm_type.debug_type()
                    };
                    field_di_types.push(StructDebugMember {
                        name: self.module.ast.identifiers.get_name(field.name),
                        di_type: debug_type,
                    });
                    field_basic_types.push(field_llvm_type.rich_value_type());
                    field_types.push(field_llvm_type);
                }

                let llvm_struct_type = self.ctx.struct_type(&field_basic_types, false);

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
            Type::TypeParameter(tp) => {
                failf!(span, "codegen was asked to codegen a type parameter {:?}", tp)
            }
            Type::FunctionTypeParameter(ftp) => {
                failf!(span, "codegen was asked to codegen a function type parameter {:?}", ftp)
            }
            Type::InferenceHole(h) => {
                failf!(span, "codegen was asked to codegen a type inference hole {:?}", h)
            }
            Type::Reference(reference) => {
                let inner_type = self.codegen_type_inner(reference.inner_type, depth + 1)?;
                let inner_debug_type = inner_type.debug_type();
                Ok(LlvmReferenceType {
                    type_id,
                    pointer_type: self.builtin_types.ptr,
                    pointee_type: inner_type.rich_value_type().as_any_type_enum(),
                    di_type: self
                        .debug
                        .create_pointer_type(&format!("reference_{}", type_id), inner_debug_type),
                }
                .into())
            }
            Type::Enum(enum_type) => {
                // self.print_layout_info();

                let enum_name = enum_type
                    .type_defn_info
                    .as_ref()
                    .map(|info| self.codegen_type_name(type_id, Some(info)))
                    .unwrap_or(type_id.to_string());
                let mut variant_structs = Vec::with_capacity(enum_type.variants.len());
                if enum_type.variants.len() >= u8::MAX as usize {
                    panic!("too many enum variants for now");
                }

                let mut payload_types = Vec::with_capacity(enum_type.variants.len());
                for variant in enum_type.variants.iter() {
                    if let Some(payload_type_id) = variant.payload {
                        let variant_payload_type =
                            self.codegen_type_inner(payload_type_id, depth + 1)?;
                        payload_types.push(Some(variant_payload_type))
                    } else {
                        payload_types.push(None)
                    }
                }
                let payload_max_alignment = payload_types
                    .iter()
                    .filter_map(|x| x.as_ref().map(|x| x.size_info().abi_align_bits))
                    .max()
                    .unwrap_or(0);
                let word_size_int_type_id =
                    if WORD_SIZE_BITS == 64 { U64_TYPE_ID } else { U32_TYPE_ID };
                let tag_int_type_id = match enum_type.explicit_tag_type {
                    Some(explicit_tag_type) => explicit_tag_type,
                    None => {
                        match payload_max_alignment {
                            0 => U8_TYPE_ID, // No payloads case
                            8 => U8_TYPE_ID,
                            16 => U16_TYPE_ID,
                            32 => U32_TYPE_ID,
                            // If the payload(s) require to be aligned on a 64-bit or larger
                            // boundary, there's no point in using a tag type smaller than the word
                            // size
                            64 => word_size_int_type_id,
                            _ => word_size_int_type_id,
                        }
                    }
                };
                debug!(
                    "Maximum payload alignment is {payload_max_alignment}, selected tag type {}",
                    self.module.type_id_to_string(tag_int_type_id)
                );
                let tag_int_type =
                    self.codegen_type(tag_int_type_id)?.rich_value_type().into_int_type();
                let tag_field_debug = self
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

                for (variant, maybe_payload_type) in
                    enum_type.variants.iter().zip(payload_types.into_iter())
                {
                    let variant_name = format!("{enum_name}.{}", self.get_ident_name(variant.name));
                    let (variant_struct, variant_struct_debug, payload_type) =
                        if let Some(_payload_type_id) = variant.payload {
                            let variant_payload_type = maybe_payload_type.unwrap();
                            let fields = &[
                                tag_int_type.as_basic_type_enum(),
                                variant_payload_type.rich_value_type(),
                            ];
                            let variant_struct_type =
                                Codegen::make_named_struct(self.ctx, &variant_name, fields);
                            let debug_struct = self.make_debug_struct_type(
                                &variant_name,
                                &variant_struct_type,
                                SpanId::NONE,
                                &[
                                    StructDebugMember { name: "tag", di_type: tag_field_debug },
                                    StructDebugMember {
                                        name: "payload",
                                        di_type: variant_payload_type.debug_type(),
                                    },
                                ],
                            );
                            (variant_struct_type, debug_struct, Some(variant_payload_type))
                        } else {
                            let variant_struct_type = Codegen::make_named_struct(
                                self.ctx,
                                &variant_name,
                                &[tag_int_type.as_basic_type_enum()],
                            );
                            let debug_struct = {
                                self.make_debug_struct_type(
                                    &variant_name,
                                    &variant_struct_type,
                                    SpanId::NONE,
                                    &[StructDebugMember { name: "tag", di_type: tag_field_debug }],
                                )
                            };
                            (variant_struct_type, debug_struct, None)
                        };
                    debug!(
                        "Variant {} {}, size is {:?}",
                        variant_name,
                        variant_struct.print_to_string(),
                        self.size_info(&variant_struct)
                    );
                    variant_structs.push(EnumVariantType {
                        name: variant.name,
                        envelope_type: self.ctx.struct_type(&[], false),
                        variant_struct_type: variant_struct,
                        payload_type,
                        tag_value: tag_int_type.const_int(variant.index as u64, false),
                        di_type: variant_struct_debug,
                        size: self.size_info(&variant_struct),
                    });
                }

                // Enum sizing and layout rules:
                // - Alignment of the enum is the max(alignment) of the variants
                // - Size of the enum is the size of the largest variant, not necessarily the same
                //   variant, plus alignment end padding
                // - In order to get to a type that has this alignment and size, we copy clang and
                //   'devise' a struct that will do it for us. This struct is simply 2 fields:
                //   - First, the strictestly-aligned variant. This sets the alignment of our
                //     struct
                //   - Second, padding bytes such that we're at least as large as the largest
                //     variant, and aligned to our own alignment

                let largest_variant =
                    variant_structs.iter().max_by_key(|v| v.size.stride_bits).unwrap();
                let strictest_aligned_variant =
                    variant_structs.iter().max_by_key(|v| v.size.abi_align_bits).unwrap();
                let enum_alignment = strictest_aligned_variant.size.abi_align_bits;
                let largest_variant_size = largest_variant.size.size_bits;

                // largest variant size rounded up to multiple of strictest_aligned_variant
                let enum_size_bits = largest_variant_size.div_ceil(enum_alignment) * enum_alignment;
                let physical_type_padding_bits =
                    enum_size_bits - strictest_aligned_variant.size.size_bits;
                debug_assert!(physical_type_padding_bits % 8 == 0);
                debug!(
                    "type {} largest variant size={largest_variant_size}, align={enum_alignment}. Physical size: {}, end padding: {}",
                    self.module.type_id_to_string(type_id), enum_size_bits, physical_type_padding_bits
                );

                let physical_type_fields: &[BasicTypeEnum<'ctx>] =
                    if physical_type_padding_bits == 0 {
                        &[strictest_aligned_variant.variant_struct_type.as_basic_type_enum()]
                    } else {
                        &[
                            strictest_aligned_variant.variant_struct_type.as_basic_type_enum(),
                            self.ctx
                                .i8_type()
                                .array_type(physical_type_padding_bits / 8)
                                .as_basic_type_enum(),
                        ]
                    };
                let physical_type =
                    Codegen::make_named_struct(self.ctx, &enum_name, physical_type_fields);
                let physical_type_size_info = self.size_info(&physical_type);
                debug!(
                    "Physical type: {} size info: {:?}",
                    physical_type.print_to_string(),
                    physical_type_size_info
                );
                debug_assert!(physical_type_size_info.size_bits == enum_size_bits);
                debug_assert!(physical_type_size_info.stride_bits == enum_size_bits);
                debug_assert!(physical_type_size_info.abi_align_bits == enum_alignment);

                // Now that we have the physical envelope representation, we add it to each variant
                for variant_struct in variant_structs.iter_mut() {
                    variant_struct.envelope_type = physical_type;
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
                                variant.size.pref_align_bits,
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
                        enum_size_bits as u64,
                        enum_alignment,
                        0,
                        &member_types,
                        0,
                        &enum_name,
                    )
                    .as_type();

                Ok(LlvmEnumType {
                    type_id,
                    tag_type: tag_int_type,
                    base_struct_type: physical_type,
                    variants: variant_structs,
                    di_type: debug_union_type,
                    size: physical_type_size_info,
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
                    self.codegen_type(rr.root_type_id.unwrap())
                } else {
                    // If this is a recursive call (depth > 0), we are in the process of
                    // building the type, and should return a placeholder for the 'recursive
                    // reference' type
                    // For recursive references, we use an empty struct as the representation,
                    // and use LLVMs opaque struct type to represent it
                    let defn_info = self
                        .module
                        .types
                        .get_type_defn_info(rr.root_type_id.unwrap())
                        .expect("recursive type must have defn info");

                    let name = self.codegen_type_name(type_id, Some(defn_info));
                    let s = self.ctx.opaque_struct_type(&name);

                    no_cache = true;
                    Ok(K1LlvmType::StructType(LlvmStructType {
                        type_id,
                        struct_type: s,
                        fields: Vec::new(),
                        di_type: self.make_debug_struct_type(&name, &s, SpanId::NONE, &[]),
                        size: SizeInfo::ZERO,
                    }))
                }
            }
            Type::Function(_function_type) => {
                let function_type = self.make_llvm_function_type(type_id)?;
                let pointee_type = function_type.llvm_function_type;
                let placeholder_pointee = self.codegen_type(I8_TYPE_ID)?.debug_type();
                Ok(K1LlvmType::Reference(LlvmReferenceType {
                    type_id,
                    pointer_type: self.builtin_types.ptr,
                    pointee_type: pointee_type.as_any_type_enum(),
                    di_type: self
                        .debug
                        .debug_builder
                        .create_pointer_type(
                            "function_ptr",
                            placeholder_pointee,
                            self.builtin_types.ptr_sized_int.get_bit_width() as u64,
                            self.builtin_types.ptr_sized_int.get_bit_width(),
                            AddressSpace::default(),
                        )
                        .as_type(),
                }))
            }
            Type::Lambda(lambda_type) => {
                let struct_type = self.codegen_type(lambda_type.env_type)?.expect_struct();
                Ok(K1LlvmType::StructType(struct_type))
            }
            Type::LambdaObject(lambda_object_type) => {
                let struct_type =
                    self.codegen_type(lambda_object_type.struct_representation)?.expect_struct();
                Ok(K1LlvmType::LambdaObject(LlvmLambdaObjectType {
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
    ) -> CodegenResult<K1LlvmFunctionType<'ctx>> {
        let function_type = self.module.types.get(function_type_id).as_function().unwrap();
        let return_type = self.codegen_type(function_type.return_type)?;
        let is_sret = return_type.is_aggregate();
        let mut param_types: Vec<K1LlvmType<'ctx>> =
            Vec::with_capacity(function_type.physical_params.len() + 1);
        for p in function_type.physical_params.iter() {
            let param_type = self.codegen_type(p.type_id)?;
            param_types.push(param_type)
        }

        let actual_params = if is_sret {
            let mut p: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::with_capacity(param_types.len() + 1);
            p.push(return_type.canonical_repr_type().into());
            p.extend(
                param_types.iter().map(|p| BasicMetadataTypeEnum::from(p.canonical_repr_type())),
            );
            p
        } else {
            param_types.iter().map(|p| p.canonical_repr_type().into()).collect()
        };

        let fn_type = if is_sret {
            self.ctx.void_type().fn_type(&actual_params, false)
        } else {
            return_type.fn_type(&actual_params, false)
        };

        Ok(K1LlvmFunctionType { llvm_function_type: fn_type, param_types, return_type, is_sret })
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
        let value = self.codegen_expr(let_stmt.initializer)?;

        if let LlvmValue::Void(instr) = value {
            return Ok(LlvmValue::Void(instr));
        }
        let value = value.expect_basic_value();

        let variable_type = self.codegen_type(let_stmt.variable_type)?;
        let variable = self.module.variables.get(let_stmt.variable_id);
        let variable_ptr = self
            .builder
            .build_alloca(variable_type.rich_value_type(), self.get_ident_name(variable.name))
            .to_err(let_stmt.span)?;
        let store_instr = if let_stmt.is_referencing {
            // If this is a let*, then we put the rhs behind another alloca so that we end up
            // with a pointer to the value
            let Type::Reference(reference_type) = self.module.types.get(variable.type_id) else {
                panic!("Expected reference for referencing let");
            };
            let reference_inner_llvm_type = self.codegen_type(reference_type.inner_type)?;
            if reference_inner_llvm_type.is_aggregate() {
                debug_assert!(value.is_pointer_value());
                self.builder.build_store(variable_ptr, value).unwrap()
            } else {
                let value_ptr = self
                    .builder
                    .build_alloca(reference_inner_llvm_type.rich_value_type(), "")
                    .unwrap();
                self.builder.build_store(value_ptr, value).unwrap();
                self.builder.build_store(variable_ptr, value_ptr).unwrap()
            }
        } else {
            self.store_k1_value(&variable_type, variable_ptr, value)
        };

        trace!(
            "codegen_let referencing={} {}: pointee_ty: {variable_type:?}",
            let_stmt.is_referencing,
            self.get_ident_name(variable.name)
        );

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
        let pointer = VariablePointer {
            pointer_value: variable_ptr,
            pointee_type_id: let_stmt.variable_type,
            pointee_llvm_type: variable_type.canonical_repr_type(),
        };
        self.variables.insert(let_stmt.variable_id, pointer);
        Ok(self.builtin_types.unit_value.as_basic_value_enum().into())
    }

    fn codegen_match(&mut self, match_expr: &TypedMatchExpr) -> CodegenResult<LlvmValue<'ctx>> {
        for stmt in &match_expr.initial_let_statements {
            if let instr @ LlvmValue::Void(_) = self.codegen_statement(*stmt)? {
                return Ok(instr);
            }
        }

        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let mut arm_blocks = vec![];
        for (index, _arm) in match_expr.arms.iter().enumerate() {
            let arm_block = self.ctx.append_basic_block(current_fn, &format!("arm_{index}"));
            let arm_consequent_block =
                self.ctx.append_basic_block(current_fn, &format!("arm_cons_{index}"));
            arm_blocks.push((arm_block, arm_consequent_block))
        }

        self.builder.build_unconditional_branch(arm_blocks[0].0).unwrap();

        let fail_block = self.ctx.append_basic_block(current_fn, "match_fail");
        self.builder.position_at_end(fail_block);
        self.builder.build_unreachable().unwrap();

        let match_end_block = self.ctx.append_basic_block(current_fn, "match_end");
        self.builder.position_at_end(match_end_block);
        let result_k1_type = self.codegen_type(match_expr.result_type)?;

        // If the whole match exits, there's no phi value
        let result_value = if result_k1_type.type_id() == NEVER_TYPE_ID {
            None
        } else {
            let phi_value = self
                .builder
                .build_phi(result_k1_type.canonical_repr_type(), "match_result")
                .unwrap();
            Some(phi_value)
        };

        'arm: for ((index, arm), (arm_block, arm_cons_block)) in
            match_expr.arms.iter().enumerate().zip(arm_blocks.iter())
        {
            self.builder.position_at_end(*arm_block);
            // I'm gonna do the bindings first, though the pattern_condition doesn't currently
            // depend on them, we'd like it to to fully de-duplicate the matching code
            for setup_stmt in &arm.setup_statements {
                if let LlvmValue::Void(_instr) = self.codegen_statement(*setup_stmt)? {
                    // If one of the scrutinees is a bot type, this block is terminated so just
                    // continue to the next arm
                    continue 'arm;
                }
            }
            let next_arm = arm_blocks.get(index + 1);
            let next_arm_or_fail = next_arm.map(|p| p.0).unwrap_or(fail_block);

            let guard_block = if let Some(_guard_condition) = arm.guard_condition {
                let guard_block =
                    self.ctx.insert_basic_block_after(*arm_block, &format!("arm_guard_{index}"));
                Some(guard_block)
            } else {
                None
            };

            self.builder.position_at_end(*arm_block);
            let pattern_condition = self.codegen_expr_basic_value(arm.pattern_condition)?;
            let pattern_condition_i1 =
                self.bool_to_i1(pattern_condition.into_int_value(), "arm_pattern_i1");
            self.builder
                .build_conditional_branch(
                    pattern_condition_i1,
                    guard_block.unwrap_or(*arm_cons_block),
                    next_arm_or_fail,
                )
                .unwrap();

            // Binding values must be visible to the guard block
            self.builder.position_at_end(guard_block.unwrap_or(*arm_cons_block));
            for binding_stmt in &arm.pattern_bindings {
                self.codegen_statement(*binding_stmt)?;
            }

            if let Some(guard_block) = guard_block {
                // Guard must be a boolean
                self.builder.position_at_end(guard_block);
                let guard_value = self.codegen_expr_basic_value(arm.guard_condition.unwrap())?;
                let condition_value = self.bool_to_i1(guard_value.into_int_value(), "arm_guard_i1");
                self.builder
                    .build_conditional_branch(condition_value, *arm_cons_block, next_arm_or_fail)
                    .unwrap();
            }

            self.builder.position_at_end(*arm_cons_block);
            let LlvmValue::BasicValue(consequent) = self.codegen_expr(arm.consequent_expr)? else {
                // If the consequent crashes or returns early, this block is terminated so just
                // continue to the next case
                continue;
            };

            // Running the consequent could have landed us in a different basic block, so the
            // incoming has to be from where we were when we built the 'consequent' value
            let consequent_end_block = self.builder.get_insert_block().unwrap();
            if let Some(result_value) = result_value {
                result_value.add_incoming(&[(&consequent, consequent_end_block)]);
            }
            self.builder.build_unconditional_branch(match_end_block).unwrap();
        }

        self.builder.position_at_end(match_end_block);

        if let Some(result_value) = result_value {
            Ok(result_value.as_basic_value().into())
        } else {
            Ok(LlvmValue::Void(self.builder.build_unreachable().unwrap()))
        }
    }

    fn codegen_if_else(&mut self, ir_if: &TypedIf) -> CodegenResult<LlvmValue<'ctx>> {
        let typ = self.codegen_type(ir_if.ty)?;
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let consequent_block = self.ctx.append_basic_block(current_fn, "if_cons");
        let alternate_block = self.ctx.append_basic_block(current_fn, "if_alt");
        let merge_block = self.ctx.append_basic_block(current_fn, "if_merge");

        // Entry block
        let condition = self.codegen_expr_basic_value(ir_if.condition)?;
        let condition_value = self.bool_to_i1(condition.into_int_value(), "cond_i1");
        self.builder
            .build_conditional_branch(condition_value, consequent_block, alternate_block)
            .unwrap();

        // Consequent Block
        self.builder.position_at_end(consequent_block);
        let consequent_block_value = self.codegen_expr(ir_if.consequent)?;
        let consequent_incoming = match consequent_block_value.as_basic_value() {
            Either::Left(_) => None,
            Either::Right(value) => {
                let consequent_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block).unwrap();
                Some((consequent_final_block, value))
            }
        };

        // Alternate Block
        self.builder.position_at_end(alternate_block);
        let alternate_block_value = self.codegen_expr(ir_if.alternate)?;
        let alternate_incoming = match alternate_block_value.as_basic_value() {
            Either::Left(_) => None,
            Either::Right(value) => {
                let alternate_final_block = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(merge_block).unwrap();
                Some((alternate_final_block, value))
            }
        };

        // Merge block
        self.builder.position_at_end(merge_block);

        if ir_if.ty == NEVER_TYPE_ID {
            // Both branches are divergent
            let unreachable = self.builder.build_unreachable().unwrap();
            Ok(LlvmValue::Void(unreachable))
        } else {
            let phi_value = self.builder.build_phi(typ.canonical_repr_type(), "if_phi").unwrap();
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
        expr: TypedExprId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        Ok(self.codegen_expr(expr)?.expect_basic_value())
    }

    /// This function is responsible for 'loading' a value of some arbitrary type and giving
    /// it in its usable, canonical form to the caller.
    /// Example 1: struct field access
    /// the value after a struct field access: a.x
    /// - If the K1 type of x is a T*, we need to load the struct gep so we're returning the
    ///   pointer, not a pointer into the struct that points to a pointer
    /// - If the K1 type of x is a {}, we can just return the result of the struct gep
    /// - if the K1 type of x is anything else, we need to load the struct gep into a local since
    ///   its a scalar value
    ///
    /// Does this mean that T** will never load? No, because that's the job of 'deref', not of
    /// 'load'. This 'load' isn't really a thing in the source language. Going from T** to T*
    /// changes the type in the source lang, so corresponds to a real deref instruction. Deref
    /// should not skip pointers... otherwise what are we doing
    fn load_k1_value(
        &self,
        llvm_type: &K1LlvmType<'ctx>,
        source: PointerValue<'ctx>,
        name: &str,
        _make_copy: bool,
    ) -> BasicValueEnum<'ctx> {
        if llvm_type.is_aggregate() {
            // No-op; we want to interact with these types as pointers
            debug!(
                "smart loading noop on type {}",
                self.module.type_id_to_string(llvm_type.type_id())
            );
            //if make_copy {
            //    self._alloca_copy_entire_value(
            //        source,
            //        llvm_type.rich_value_type(),
            //        &format!("{name}_copy"),
            //    )
            //    .as_basic_value_enum()
            //} else {
            //    source.as_basic_value_enum()
            //}
            //
            source.as_basic_value_enum()
        } else {
            // Scalars must be truly loaded
            self.builder.build_load(llvm_type.rich_value_type(), source, name).unwrap()
        }
    }

    /// Handles the storage of aggregates by doing a (hopefully) correct memcpy
    fn store_k1_value(
        &self,
        llvm_type: &K1LlvmType<'ctx>,
        dest: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> InstructionValue<'ctx> {
        if llvm_type.is_aggregate() {
            self.memcpy_entire_value(dest, value.into_pointer_value(), llvm_type.rich_value_type())
        } else {
            self.builder.build_store(dest, value).unwrap()
        }
    }

    fn _alloca_copy_entire_value(
        &self,
        src: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let dst = self.builder.build_alloca(ty, name).unwrap();
        self.memcpy_entire_value(dst, src, ty);
        dst
    }

    fn memcpy_entire_value(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) -> InstructionValue<'ctx> {
        let size = self.ctx.ptr_sized_int_type(&self.llvm_machine.get_target_data(), None);
        let size_info = self.size_info(&ty);
        let bytes = size.const_int((size_info.stride_bits / 8) as u64, false);
        let align_bytes = size_info.abi_align_bits / 8;
        self.builder
            .build_memcpy(dst, align_bytes, src, align_bytes, bytes)
            .unwrap()
            .as_instruction_value()
            .unwrap()
    }

    fn codegen_compile_time_value(
        &self,
        compile_time_value: &CompileTimeValue,
    ) -> BasicValueEnum<'ctx> {
        let result = match compile_time_value {
            CompileTimeValue::Unit => self.builtin_types.unit_value.as_basic_value_enum(),
            CompileTimeValue::Boolean(b) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum(),
                false => self.builtin_types.false_value.as_basic_value_enum(),
            },
            CompileTimeValue::Char(byte) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum()
            }
            CompileTimeValue::Integer(int_value) => self.codegen_integer_value(*int_value).unwrap(),
            CompileTimeValue::Float(float_value) => self.codegen_float_value(*float_value).unwrap(),
            CompileTimeValue::String(boxed_str) => self.codegen_string_literal(boxed_str).unwrap(),
            CompileTimeValue::Pointer(ptr) => {
                if *ptr == 0 {
                    self.ctx.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum()
                } else {
                    panic!("comptime Pointer (raw address) that was not zero; I have no idea what to do with that. Maybe we enhance that type such that 'NULL' is not zero but a part of the type")
                }
            }
        };
        result
    }

    fn codegen_string_literal(&self, string_value: &str) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Get a hold of the type for 'string' (its just a struct that we expect to exist!)
        let string_type = self.codegen_type(STRING_TYPE_ID)?;
        let string_wrapper_struct = string_type.rich_value_type().into_struct_type();
        let char_buffer_struct =
            string_wrapper_struct.get_field_type_at_index(0).unwrap().into_struct_type();

        // Ensure the string layout is what we expect
        // deftype string = { buffer: Buffer[char] }
        debug_assert!(
            char_buffer_struct.get_field_type_at_index(0).unwrap().into_int_type().get_bit_width()
                == 64
        );
        debug_assert!(char_buffer_struct.get_field_type_at_index(1).unwrap().is_pointer_type());
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
                    self.builtin_types.int.const_int(string_value.len() as u64, true).into(),
                    global_str_data.as_pointer_value().into(),
                ])
                .as_basic_value_enum()])
            .as_basic_value_enum();
        let global_value = self.llvm_module.add_global(string_wrapper_struct, None, "str");
        global_value.set_constant(true);
        global_value.set_initializer(&global_str_value);
        Ok(global_value.as_pointer_value().as_basic_value_enum())
    }

    fn codegen_expr(&mut self, expr_id: TypedExprId) -> CodegenResult<LlvmValue<'ctx>> {
        let expr = self.module.exprs.get(expr_id);
        let span = expr.get_span();
        self.set_debug_location_from_span(span);
        debug!("codegen expr\n{}", self.module.expr_to_string_with_type(expr_id));
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
                Ok(self.codegen_integer_value(integer.value).unwrap().into())
            }
            TypedExpr::Float(float) => Ok(self.codegen_float_value(float.value).unwrap().into()),
            TypedExpr::String(string_value, _) => {
                Ok(self.codegen_string_literal(string_value)?.into())
            }
            TypedExpr::Variable(ir_var) => {
                if let Some(pointer) = self.variables.get(&ir_var.variable_id) {
                    let llvm_type = self.codegen_type(ir_var.type_id)?;
                    debug!(
                        "codegen variable {} got pointee type {:?}",
                        self.module.type_id_to_string(ir_var.type_id),
                        pointer.pointee_llvm_type
                    );
                    let var_name = self.module.variables.get(ir_var.variable_id).name;
                    let result_value = self.load_k1_value(
                        &llvm_type,
                        pointer.pointer_value,
                        &format!("{}_loaded", self.module.name_of(var_name)),
                        false,
                    );
                    Ok(result_value.into())
                } else if let Some(global) = self.globals.get(&ir_var.variable_id) {
                    let llvm_type = self.codegen_type(ir_var.type_id)?;
                    eprintln!(
                        "Loading a global of type {}",
                        self.module.type_id_to_string(ir_var.type_id)
                    );
                    if let K1LlvmType::Reference(_r) = llvm_type {
                        Ok(global.as_pointer_value().as_basic_value_enum().into())
                    } else {
                        // Unlike with our own variables (though we should probably change this!)
                        // the global is always a pointer whether the type
                        // is int* or int. So we have to do something different
                        // (besides just a smart load) based on that.
                        // If its an int*, we don't want to load at all.
                        //
                        // We could do this for our referencing `let`s too.
                        // Avoid the extra alloca and treat it different on interpretation.
                        // This would mean I think changing what load_k1_value does for references.
                        // It would no-op, whereas today it loads
                        let value =
                            self.load_k1_value(&llvm_type, global.as_pointer_value(), "", false);
                        Ok(value.into())
                    }
                } else {
                    Err(CodegenError {
                        message: format!(
                            "No pointer or global found for variable {}",
                            self.module.expr_to_string(expr_id)
                        ),
                        span,
                    })
                }
            }
            TypedExpr::Struct(struc) => {
                let struct_k1_llvm_type = self.codegen_type(struc.type_id)?.expect_struct();
                let struct_llvm_type = struct_k1_llvm_type.struct_type;
                let struct_ptr =
                    self.builder.build_alloca(struct_llvm_type, "struct_literal").unwrap();
                for (idx, field) in struc.fields.iter().enumerate() {
                    let value = self.codegen_expr_basic_value(field.expr)?;
                    let field_addr = self
                        .builder
                        .build_struct_gep(
                            struct_llvm_type,
                            struct_ptr,
                            idx as u32,
                            &format!("{}_store_addr", self.module.name_of(field.name)),
                        )
                        .unwrap();
                    let field_type = &struct_k1_llvm_type.fields[idx];
                    self.store_k1_value(field_type, field_addr, value);
                }
                Ok(struct_ptr.as_basic_value_enum().into())
            }
            TypedExpr::StructFieldAccess(field_access) => {
                let name = &format!(
                    "struc.{}",
                    self.module.ast.identifiers.get_name(field_access.target_field)
                );
                let field_index = field_access.target_field_index;
                let struct_llvm_type = self.codegen_type(field_access.struct_type)?.expect_struct();
                let struct_physical_type = struct_llvm_type.struct_type;
                // Codegen the field's memory location
                let struc = self.codegen_expr_basic_value(field_access.base)?;
                let struct_pointer = struc.into_pointer_value();
                let field_pointer = self
                    .builder
                    .build_struct_gep(struct_physical_type, struct_pointer, field_index, name)
                    .unwrap();
                // Don't even attempt to load the value for referencing access
                if field_access.is_referencing {
                    Ok(field_pointer.as_basic_value_enum().into())
                } else {
                    let field_type = &struct_llvm_type.fields[field_index as usize];
                    let field_value = self.load_k1_value(field_type, field_pointer, name, false);
                    Ok(field_value.into())
                }
            }
            TypedExpr::If(if_expr) => self.codegen_if_else(if_expr),
            TypedExpr::Match(match_expr) => self.codegen_match(match_expr),
            TypedExpr::WhileLoop(while_expr) => self.codegen_while_expr(while_expr),
            TypedExpr::LoopExpr(loop_expr) => self.codegen_loop_expr(loop_expr),
            TypedExpr::BinaryOp(bin_op) => self.codegen_binop(bin_op),
            TypedExpr::UnaryOp(unary_op) => {
                let value = self.codegen_expr_basic_value(unary_op.expr)?;
                match unary_op.kind {
                    UnaryOpKind::Dereference => {
                        let value_ptr = value.into_pointer_value();
                        let pointee_ty = self.codegen_type(unary_op.type_id)?;
                        debug!(
                            "Dereference: type {} w/ llvm value {} as llvm type {}",
                            self.module.expr_to_string_with_type(unary_op.expr),
                            value_ptr,
                            pointee_ty.canonical_repr_type().print_to_string()
                        );
                        let value = if pointee_ty.is_aggregate() {
                            value_ptr.as_basic_value_enum()
                        } else {
                            self.builder
                                .build_load(pointee_ty.canonical_repr_type(), value_ptr, "deref")
                                .unwrap()
                        };
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

                let enum_variant = &enum_type.variants[enum_constr.variant_index as usize];

                let enum_ptr = self
                    .builder
                    .build_alloca(enum_variant.envelope_type, "enum_constr")
                    .to_err(enum_constr.span)?;

                // Store the tag_value in the first slot
                let tag_pointer = self
                    .builder
                    .build_struct_gep(
                        enum_variant.variant_struct_type,
                        enum_ptr,
                        0,
                        &format!("enum_tag_{}", self.get_ident_name(variant_tag_name)),
                    )
                    .unwrap();
                self.builder.build_store(tag_pointer, enum_variant.tag_value).unwrap();

                if let Some(payload) = &enum_constr.payload {
                    let value = self.codegen_expr_basic_value(*payload)?;
                    let payload_pointer = self
                        .builder
                        .build_struct_gep(
                            enum_variant.variant_struct_type,
                            enum_ptr,
                            1,
                            &format!("enum_payload_{}", self.get_ident_name(variant_tag_name)),
                        )
                        .unwrap();
                    let payload_type = enum_variant.payload_type.as_ref().unwrap();
                    self.store_k1_value(payload_type, payload_pointer, value);
                }

                Ok(enum_ptr.as_basic_value_enum().into())
            }
            TypedExpr::EnumIsVariant(enum_is_variant) => {
                let enum_value = self
                    .codegen_expr_basic_value(enum_is_variant.target_expr)?
                    .into_pointer_value();
                let target_expr_type_id =
                    self.module.exprs.get(enum_is_variant.target_expr).get_type();
                let enum_llvm = self.codegen_type(target_expr_type_id)?.expect_enum();
                let variant = &enum_llvm.variants[enum_is_variant.variant_index as usize];
                let is_variant_bool = self.codegen_enum_is_variant(
                    enum_value,
                    variant.tag_value,
                    self.module.name_of(variant.name),
                );
                Ok(is_variant_bool.as_basic_value_enum().into())
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                let target_expr_type_id =
                    self.module.exprs.get(enum_get_payload.target_expr).get_type();
                let enum_type = self.module.types.get_type_id_dereferenced(target_expr_type_id);
                let enum_type = self.codegen_type(enum_type)?.expect_enum();
                let enum_value = self.codegen_expr_basic_value(enum_get_payload.target_expr)?;
                let variant_type = &enum_type.variants[enum_get_payload.variant_index as usize];

                if enum_get_payload.is_referencing {
                    let enum_value = enum_value.into_pointer_value();
                    let payload_pointer = self
                        .get_enum_payload_reference(variant_type.variant_struct_type, enum_value);
                    Ok(payload_pointer.as_basic_value_enum().into())
                } else {
                    let enum_value = enum_value.into_pointer_value();
                    let payload_pointer = self
                        .get_enum_payload_reference(variant_type.variant_struct_type, enum_value);
                    let payload_copied = self.load_k1_value(
                        variant_type.payload_type.as_ref().unwrap(),
                        payload_pointer,
                        "payload_by_value",
                        false,
                    );
                    Ok(payload_copied.into())
                }
            }
            TypedExpr::Cast(cast) => self.codegen_cast(cast),
            TypedExpr::Return(ret) => {
                let return_value = self.codegen_expr_basic_value(ret.value)?;
                let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let function_id = self.llvm_function_to_k1.get(&function).unwrap();
                let codegened_function = self.llvm_functions.get(function_id).unwrap();
                match codegened_function.sret_pointer {
                    None => {
                        // Normal return
                        let ret_inst = self.builder.build_return(Some(&return_value)).unwrap();
                        Ok(LlvmValue::Void(ret_inst))
                    }
                    Some(sret_ptr) => {
                        self.store_k1_value(
                            &codegened_function.function_type.return_type,
                            sret_ptr,
                            return_value,
                        );
                        let ret = self.builder.build_return(None).unwrap();
                        Ok(LlvmValue::Void(ret))
                    }
                }
            }
            TypedExpr::Break(break_) => {
                let loop_info = *self.loops.get(&break_.loop_scope).unwrap();
                let break_value = self.codegen_expr_basic_value(break_.value)?;
                if let Some(break_value_ptr) = loop_info.break_value_ptr {
                    self.builder.build_store(break_value_ptr, break_value).unwrap();
                }
                let branch_inst =
                    self.builder.build_unconditional_branch(loop_info.end_block).unwrap();
                Ok(LlvmValue::Void(branch_inst))
            }
            TypedExpr::Lambda(lambda_expr) => {
                eprintln!("codegen lambda {:?}", lambda_expr);
                let lambda_type =
                    self.module.types.get(lambda_expr.lambda_type).as_lambda().unwrap();
                let llvm_fn = self.codegen_function_or_get(lambda_type.body_function_id)?;
                let environment_struct_value = self
                    .codegen_expr_basic_value(lambda_type.environment_struct)?
                    .into_pointer_value();

                self.lambda_functions.insert(lambda_expr.lambda_type, llvm_fn);

                Ok(environment_struct_value.as_basic_value_enum().into())
            }
            TypedExpr::FunctionReference(function_reference_expr) => {
                let function_value =
                    self.codegen_function_or_get(function_reference_expr.function_id)?;
                let function_ptr =
                    function_value.as_global_value().as_pointer_value().as_basic_value_enum();
                self.set_debug_location_from_span(function_reference_expr.span);
                Ok(function_ptr.as_basic_value_enum().into())
            }
            TypedExpr::FunctionToLambdaObject(fn_to_lam_obj) => {
                let function_value = self.codegen_function_or_get(fn_to_lam_obj.function_id)?;
                self.set_debug_location_from_span(fn_to_lam_obj.span);
                let function_ptr =
                    function_value.as_global_value().as_pointer_value().as_basic_value_enum();
                let lam_obj_struct_type =
                    self.codegen_type(fn_to_lam_obj.lambda_object_type_id)?.expect_lambda_object();

                // rich_value_type() equivalent
                let lambda_object_ptr =
                    self.builder.build_alloca(lam_obj_struct_type.struct_type, "fn2obj").unwrap();

                let obj_function_ptr_ptr = self
                    .builder
                    .build_struct_gep(
                        lam_obj_struct_type.struct_type,
                        lambda_object_ptr,
                        0,
                        "fn_ptr",
                    )
                    .unwrap();
                self.builder.build_store(obj_function_ptr_ptr, function_ptr).unwrap();

                let obj_env_ptr_ptr = self
                    .builder
                    .build_struct_gep(
                        lam_obj_struct_type.struct_type,
                        lambda_object_ptr,
                        1,
                        "nop_env",
                    )
                    .unwrap();
                self.builder
                    .build_store(obj_env_ptr_ptr, self.builtin_types.ptr.const_null())
                    .unwrap();

                // This is a STRUCT, in K1 terms, not a struct reference, it's just that the physical
                // representation type for aggregates _is_ ptr in our codegen
                Ok(LlvmValue::BasicValue(lambda_object_ptr.as_basic_value_enum()))
            }
            e @ TypedExpr::PendingCapture(_) => {
                panic!("Unsupported expression: {e:?}")
            }
        }
    }

    fn codegen_cast(&mut self, cast: &TypedCast) -> CodegenResult<LlvmValue<'ctx>> {
        match cast.cast_type {
            CastType::KnownNoOp | CastType::Integer8ToChar => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                Ok(value.into())
            }
            CastType::IntegerExtend | CastType::IntegerExtendFromChar => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                let int_value = value.into_int_value();
                let llvm_type = self.codegen_type(cast.target_type_id)?;
                let integer_type = self.module.types.get(cast.target_type_id).expect_integer();
                let value: IntValue<'ctx> = if integer_type.is_signed() {
                    self.builder
                        .build_int_s_extend(
                            int_value,
                            llvm_type.rich_value_type().into_int_type(),
                            "extend_cast",
                        )
                        .unwrap()
                } else {
                    self.builder
                        .build_int_z_extend(
                            int_value,
                            llvm_type.rich_value_type().into_int_type(),
                            "extend_cast",
                        )
                        .unwrap()
                };
                Ok(value.as_basic_value_enum().into())
            }
            CastType::IntegerTruncate => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                let int_value = value.into_int_value();
                let int_type = self.codegen_type(cast.target_type_id)?;
                let truncated_value = self
                    .builder
                    .build_int_truncate(
                        int_value,
                        int_type.rich_value_type().into_int_type(),
                        "trunc_cast",
                    )
                    .unwrap();
                Ok(truncated_value.as_basic_value_enum().into())
            }
            CastType::FloatExtend => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let float_dst_type =
                    self.codegen_type(cast.target_type_id)?.rich_value_type().into_float_type();
                let extended_value =
                    self.builder.build_float_ext(from_value, float_dst_type, "fext").unwrap();
                Ok(extended_value.as_basic_value_enum().into())
            }
            CastType::FloatTruncate => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let float_dst_type =
                    self.codegen_type(cast.target_type_id)?.rich_value_type().into_float_type();
                let extended_value =
                    self.builder.build_float_trunc(from_value, float_dst_type, "ftrunc").unwrap();
                Ok(extended_value.as_basic_value_enum().into())
            }
            CastType::FloatToInteger => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let int_dst_type = self.codegen_type(cast.target_type_id)?;
                let int_dst_type_llvm = int_dst_type.rich_value_type().into_int_type();
                let int_dest_k1_type = self.module.types.get(cast.target_type_id).expect_integer();
                let casted_int_value = if int_dest_k1_type.is_signed() {
                    self.builder
                        .build_float_to_signed_int(from_value, int_dst_type_llvm, "")
                        .unwrap()
                } else {
                    self.builder
                        .build_float_to_unsigned_int(from_value, int_dst_type_llvm, "")
                        .unwrap()
                };
                Ok(casted_int_value.as_basic_value_enum().into())
            }
            CastType::IntegerToFloat => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_int_value();
                let base_expr_type = self.module.exprs.get(cast.base_expr).get_type();
                let from_int_k1_type = self.module.types.get(base_expr_type).expect_integer();
                let float_dst_type = self.codegen_type(cast.target_type_id)?;
                let float_dst_type_llvm = float_dst_type.rich_value_type().into_float_type();
                let casted_float_value = if from_int_k1_type.is_signed() {
                    self.builder
                        .build_signed_int_to_float(from_value, float_dst_type_llvm, "")
                        .unwrap()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(from_value, float_dst_type_llvm, "")
                        .unwrap()
                };
                Ok(casted_float_value.as_basic_value_enum().into())
            }
            CastType::PointerToReference => {
                // This is a complete no-op in the LLVM ir
                // since llvm 'ptr' is untyped, and all we're doing
                // here is adding a type to our pointer
                let pointer = self.codegen_expr_basic_value(cast.base_expr)?.into_pointer_value();
                Ok(pointer.as_basic_value_enum().into())
            }
            CastType::ReferenceToPointer => {
                // This is a complete no-op in the LLVM ir
                // since llvm 'ptr' is untyped, and all we're doing
                // here is removing the type from our pointer
                let reference = self.codegen_expr_basic_value(cast.base_expr)?.into_pointer_value();
                Ok(reference.as_basic_value_enum().into())
            }
            CastType::PointerToInteger => {
                let ptr = self.codegen_expr_basic_value(cast.base_expr)?.into_pointer_value();
                let as_int = self
                    .builder
                    .build_ptr_to_int(ptr, self.builtin_types.ptr_sized_int, "ptrtoint_cast")
                    .unwrap();
                Ok(as_int.as_basic_value_enum().into())
            }
            CastType::IntegerToPointer => {
                let int = self.codegen_expr_basic_value(cast.base_expr)?.into_int_value();
                let as_ptr = self
                    .builder
                    .build_int_to_ptr(int, self.builtin_types.ptr, "inttoptr_cast")
                    .unwrap();
                Ok(as_ptr.as_basic_value_enum().into())
            }
            CastType::LambdaToLambdaObject => {
                // Produces just the lambda's environment as a value. We don't need the function
                // pointer because we know it from the type still
                let lambda_env_value = self.codegen_expr_basic_value(cast.base_expr)?;
                let env_pointer =
                    self.builder.build_alloca(lambda_env_value.get_type(), "env_ptr").unwrap();
                self.builder.build_store(env_pointer, lambda_env_value).unwrap();

                let fn_value = self
                    .lambda_functions
                    .get(&self.module.get_expr_type_id(cast.base_expr))
                    .unwrap();
                let fn_ptr = fn_value.as_global_value().as_pointer_value();

                let lam_obj = self.builtin_types.dynamic_lambda_object.get_undef();
                let lam_obj = self.builder.build_insert_value(lam_obj, fn_ptr, 0, "").unwrap();
                let lam_obj =
                    self.builder.build_insert_value(lam_obj, lambda_env_value, 1, "").unwrap();

                // Aggregates have to be pointers because that's just how we represent them
                let lam_obj_ptr = self
                    .builder
                    .build_alloca(self.builtin_types.dynamic_lambda_object, "lam_obj_ptr")
                    .unwrap();
                self.builder.build_store(lam_obj_ptr, lam_obj).unwrap();

                Ok(lam_obj_ptr.as_basic_value_enum().into())
            }
        }
    }

    fn codegen_binop(&mut self, bin_op: &BinaryOp) -> CodegenResult<LlvmValue<'ctx>> {
        // This would be simpler if we first matched on lhs than on result type,
        // because we have to branch on int vs float now for each result type
        match self.module.types.get(bin_op.ty) {
            Type::Integer(integer_type) => {
                let signed = integer_type.is_signed();
                let lhs_value = self.codegen_expr_basic_value(bin_op.lhs)?.into_int_value();
                let rhs_value = self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();
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
                let op_int_value = op_res.to_err(bin_op.span)?;
                Ok(op_int_value.as_basic_value_enum().into())
            }
            Type::Float(_float_type) => {
                let lhs_value = self.codegen_expr_basic_value(bin_op.lhs)?.into_float_value();
                let rhs_value = self.codegen_expr_basic_value(bin_op.rhs)?.into_float_value();
                let op_res = match bin_op.kind {
                    BinaryOpKind::Add => self
                        .builder
                        .build_float_add(lhs_value, rhs_value, "fadd")
                        .unwrap()
                        .as_basic_value_enum(),
                    BinaryOpKind::Subtract => self
                        .builder
                        .build_float_sub(lhs_value, rhs_value, "fsub")
                        .unwrap()
                        .as_basic_value_enum(),
                    BinaryOpKind::Multiply => self
                        .builder
                        .build_float_mul(lhs_value, rhs_value, "fmul")
                        .unwrap()
                        .as_basic_value_enum(),
                    BinaryOpKind::Divide => self
                        .builder
                        .build_float_div(lhs_value, rhs_value, "fdiv")
                        .unwrap()
                        .as_basic_value_enum(),
                    BinaryOpKind::And => unreachable!("bitwise 'and' is unsupported on float"),
                    BinaryOpKind::Or => unreachable!("bitwise 'or' is unsupported on float"),
                    BinaryOpKind::Rem => self
                        .builder
                        .build_float_rem(lhs_value, rhs_value, "frem")
                        .unwrap()
                        .as_basic_value_enum(),
                    _ => {
                        panic!("Unsupported bin op kind returning float: {}", bin_op.kind)
                    }
                };
                Ok(op_res.as_basic_value_enum().into())
            }
            Type::Bool(_) => match bin_op.kind {
                BinaryOpKind::And | BinaryOpKind::Or => {
                    let lhs = self.codegen_expr_basic_value(bin_op.lhs)?.into_int_value();
                    let op = match bin_op.kind {
                        BinaryOpKind::And => {
                            let lhs_i1 = self.bool_to_i1(lhs, "lhs_for_cmp");
                            let short_circuit_branch =
                                self.build_conditional_branch(lhs_i1, "rhs_check", "short_circuit");
                            let phi_destination = self.append_basic_block("and_result");

                            // label: rhs_check; lhs was true
                            self.builder.position_at_end(short_circuit_branch.then_block);
                            let rhs = self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();

                            // Don't forget to grab the actual incoming block in case bin_op.rhs did branching!
                            let rhs_incoming = self.builder.get_insert_block().unwrap();
                            self.builder.build_unconditional_branch(phi_destination).unwrap();
                            // return rhs

                            // label: short_circuit; lhs was false
                            self.builder.position_at_end(short_circuit_branch.else_block);
                            self.builder.build_unconditional_branch(phi_destination).unwrap();
                            // return lhs aka false

                            // label: and_result
                            self.builder.position_at_end(phi_destination);
                            let result = self
                                .builder
                                .build_phi(self.builtin_types.boolean, "bool_and")
                                .unwrap();
                            result.add_incoming(&[
                                (&rhs, rhs_incoming),
                                (&self.builtin_types.false_value, short_circuit_branch.else_block),
                            ]);
                            result.as_basic_value().into_int_value()
                        }
                        BinaryOpKind::Or => {
                            let rhs_int =
                                self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();
                            self.builder.build_or(lhs, rhs_int, "bool_or").unwrap()
                        }
                        BinaryOpKind::Equals => {
                            let rhs_int =
                                self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();
                            self.builder
                                .build_int_compare(IntPredicate::EQ, lhs, rhs_int, "bool_eq")
                                .unwrap()
                        }
                        _ => panic!("Unhandled binop combo"),
                    };
                    Ok(op.as_basic_value_enum().into())
                }
                BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                    match self.module.get_expr_type(bin_op.lhs) {
                        Type::Float(_) => {
                            let lhs_float =
                                self.codegen_expr_basic_value(bin_op.lhs)?.into_float_value();
                            let rhs_float =
                                self.codegen_expr_basic_value(bin_op.rhs)?.into_float_value();
                            let cmp_result = self
                                .builder
                                .build_float_compare(
                                    if bin_op.kind == BinaryOpKind::Equals {
                                        FloatPredicate::OEQ
                                    } else {
                                        FloatPredicate::ONE
                                    },
                                    lhs_float,
                                    rhs_float,
                                    &format!("f{}_i1", bin_op.kind),
                                )
                                .unwrap();
                            Ok(self
                                .i1_to_bool(cmp_result, &format!("{}_res", bin_op.kind))
                                .as_basic_value_enum()
                                .into())
                        }
                        t if t.is_scalar_int_value() => {
                            let lhs_int =
                                self.codegen_expr_basic_value(bin_op.lhs)?.into_int_value();
                            let rhs_int =
                                self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();
                            let cmp_result = self
                                .builder
                                .build_int_compare(
                                    if bin_op.kind == BinaryOpKind::Equals {
                                        IntPredicate::EQ
                                    } else {
                                        IntPredicate::NE
                                    },
                                    lhs_int,
                                    rhs_int,
                                    &format!("{}_i1", bin_op.kind),
                                )
                                .unwrap();
                            Ok(self
                                .i1_to_bool(cmp_result, &format!("{}_res", bin_op.kind))
                                .as_basic_value_enum()
                                .into())
                        }
                        t => unreachable!(
                            "unreachable Equals/NotEquals call on type: {}",
                            self.module.type_to_string(t, false)
                        ),
                    }
                }
                BinaryOpKind::Less
                | BinaryOpKind::LessEqual
                | BinaryOpKind::Greater
                | BinaryOpKind::GreaterEqual => match self.module.get_expr_type(bin_op.lhs) {
                    Type::Integer(_) => {
                        let lhs_int = self.codegen_expr_basic_value(bin_op.lhs)?.into_int_value();
                        let rhs_int = self.codegen_expr_basic_value(bin_op.rhs)?.into_int_value();
                        let pred = match bin_op.kind {
                            BinaryOpKind::Less => IntPredicate::SLT,
                            BinaryOpKind::LessEqual => IntPredicate::SLE,
                            BinaryOpKind::Greater => IntPredicate::SGT,
                            BinaryOpKind::GreaterEqual => IntPredicate::SGE,
                            _ => unreachable!("unexpected binop kind"),
                        };
                        let i1_compare = self
                            .builder
                            .build_int_compare(
                                pred,
                                lhs_int,
                                rhs_int,
                                &format!("{}_i1", bin_op.kind),
                            )
                            .unwrap();
                        Ok(self
                            .i1_to_bool(i1_compare, &format!("{}_res", bin_op.kind))
                            .as_basic_value_enum()
                            .into())
                    }
                    Type::Float(_) => {
                        let lhs_float =
                            self.codegen_expr_basic_value(bin_op.lhs)?.into_float_value();
                        let rhs_float =
                            self.codegen_expr_basic_value(bin_op.rhs)?.into_float_value();
                        let pred = match bin_op.kind {
                            BinaryOpKind::Less => FloatPredicate::OLT,
                            BinaryOpKind::LessEqual => FloatPredicate::OLE,
                            BinaryOpKind::Greater => FloatPredicate::OGT,
                            BinaryOpKind::GreaterEqual => FloatPredicate::OGE,
                            _ => unreachable!("unexpected binop kind"),
                        };
                        let i1_compare = self
                            .builder
                            .build_float_compare(
                                pred,
                                lhs_float,
                                rhs_float,
                                &format!("f{}", bin_op.kind),
                            )
                            .unwrap();
                        Ok(self.i1_to_bool(i1_compare, "").as_basic_value_enum().into())
                    }
                    _ => unreachable!("unexpected comparison operand; not float or int"),
                },
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
        self.builder.build_conditional_branch(cond, then_block, else_block).unwrap();
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
        enum_value: PointerValue<'ctx>,
        variant_tag_value: IntValue<'ctx>,
        variant_name: &str,
    ) -> IntValue<'ctx> {
        let enum_tag_value = self.get_enum_tag(variant_tag_value.get_type(), enum_value);
        let is_equal = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                enum_tag_value,
                variant_tag_value,
                "is_variant_cmp",
            )
            .unwrap();
        let is_equal_bool = self.i1_to_bool(is_equal, &format!("is_variant_{}", variant_name));
        is_equal_bool
    }

    fn bool_to_i1(&self, bool: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_truncate(bool, self.builtin_types.i1, name).unwrap()
    }

    fn i1_to_bool(&self, i1: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_cast(i1, self.builtin_types.boolean, name).unwrap()
    }

    fn get_enum_tag(
        &self,
        tag_type: IntType<'ctx>,
        enum_value: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        // Just interpret the enum memory just as the int tag that is always at the start, no need to
        // treat it as a struct
        self.builder.build_load(tag_type, enum_value, "tag").unwrap().into_int_value()
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
            let arg_value = self.codegen_expr(*arg_expr)?;
            let LlvmValue::BasicValue(basic_value) = arg_value else { return Ok(arg_value) };
            trace!(
                "codegen function call arg: {}",
                self.module.expr_to_string_with_type(*arg_expr),
            );
            args.push_back(basic_value.into())
        }
        let function_type = self.module.get_callee_function_type(&call.callee);
        let llvm_function_type = self.make_llvm_function_type(function_type)?;
        let sret_alloca = if llvm_function_type.is_sret {
            let sret_alloca = self
                .builder
                .build_alloca(llvm_function_type.return_type.rich_value_type(), "call_sret")
                .unwrap();
            args.push_front(sret_alloca.into());
            Some(sret_alloca)
        } else {
            None
        };
        let env_arg_index = if llvm_function_type.is_sret { 1 } else { 0 };
        let callsite_value = match &call.callee {
            Callee::StaticFunction(function_id) => {
                let function_value = self.codegen_function_or_get(*function_id)?;

                self.set_debug_location_from_span(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "").unwrap()
            }
            Callee::StaticLambda { function_id, environment_ptr, .. } => {
                let lambda_env_variable = self.variables.get(environment_ptr).unwrap();
                // We know this is always a pointer to the environment, and since we aren't loading
                // the environment, we just use the ptr as-is
                let lambda_env_ptr = lambda_env_variable.pointer_value;
                // self.lambda_environments.get(lambda_type_id).unwrap().as_basic_value_enum();
                args.insert(env_arg_index, lambda_env_ptr.into());

                let function_value = self.codegen_function_or_get(*function_id)?;

                self.set_debug_location_from_span(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "").unwrap()
            }
            Callee::DynamicFunction(function_reference_expr) => {
                let function_ptr =
                    self.codegen_expr_basic_value(*function_reference_expr)?.into_pointer_value();

                self.set_debug_location_from_span(call.span);

                let call_site_value = self
                    .builder
                    .build_indirect_call(
                        llvm_function_type.llvm_function_type,
                        function_ptr,
                        args.make_contiguous(),
                        "",
                    )
                    .unwrap();
                call_site_value
            }
            Callee::DynamicLambda(callee_struct_expr) => {
                let lambda_object_type = self.builtin_types.dynamic_lambda_object;
                let callee_struct =
                    self.codegen_expr_basic_value(*callee_struct_expr)?.into_pointer_value();

                self.set_debug_location_from_span(call.span);
                let fn_ptr_gep = self
                    .builder
                    .build_struct_gep(lambda_object_type, callee_struct, 0, "fn_ptr_gep")
                    .unwrap();
                let fn_ptr = self
                    .builder
                    .build_load(
                        lambda_object_type.get_field_type_at_index(0).unwrap(),
                        fn_ptr_gep,
                        "fn_ptr",
                    )
                    .unwrap()
                    .into_pointer_value();
                let env_ptr_gep = self
                    .builder
                    .build_struct_gep(lambda_object_type, callee_struct, 1, "env_ptr_gep")
                    .unwrap();
                let env_ptr = self
                    .builder
                    .build_load(
                        lambda_object_type.get_field_type_at_index(1).unwrap(),
                        env_ptr_gep,
                        "env_ptr",
                    )
                    .unwrap();
                args.insert(env_arg_index, env_ptr.into());

                eprintln!(
                    "The k1 fn type on the lambda object {}",
                    self.module.type_id_to_string(function_type)
                );
                eprintln!("Calling indirect with type {}", llvm_function_type.llvm_function_type);
                self.builder
                    .build_indirect_call(
                        llvm_function_type.llvm_function_type,
                        fn_ptr,
                        args.make_contiguous(),
                        "",
                    )
                    .unwrap()
            }
            Callee::Abstract { .. } => {
                return failf!(
                    call.span,
                    "Internal Compiler Error: cannot codegen an Abstract callee"
                )
            }
        };

        if llvm_function_type.is_sret {
            let sret_attribute = self.ctx.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                llvm_function_type.return_type.rich_value_type().as_any_type_enum(),
            );
            callsite_value.add_attribute(AttributeLoc::Param(0), sret_attribute)
        };
        match callsite_value.try_as_basic_value() {
            either::Either::Left(value) => Ok(LlvmValue::BasicValue(value)),
            either::Either::Right(_instr) => {
                if llvm_function_type.is_sret {
                    let sret_pointer = sret_alloca.unwrap();
                    // We should technically 'load' this, but the load would be a no-op since
                    // its an aggregate.
                    Ok(sret_pointer.as_basic_value_enum().into())
                } else if call.return_type == NEVER_TYPE_ID {
                    let unreachable = self.builder.build_unreachable().unwrap();
                    Ok(LlvmValue::Void(unreachable))
                } else {
                    panic!("Function returned LLVM void but wasn't typed as never and was not sret")
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
        let string_struct_type = string_type.rich_value_type().into_struct_type();
        let string_struct = string_struct_type.const_named_struct(&[
            length_value.as_basic_value_enum(),
            char_data_global.as_pointer_value().as_basic_value_enum(),
        ]);
        let g = self.llvm_module.add_global(string_struct_type, None, name);
        g.set_initializer(&string_struct);
        g.as_pointer_value()
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
                let size = self.size_info(&llvm_type.rich_value_type().as_any_type_enum());
                let num_bits = match intrinsic_type {
                    IntrinsicFunction::SizeOf => size.size_bits,
                    IntrinsicFunction::SizeOfStride => size.stride_bits,
                    IntrinsicFunction::AlignOf => size.pref_align_bits,
                    _ => unreachable!(),
                };
                let num_bytes = num_bits / 8;
                let size_value = self.builtin_types.int.const_int(num_bytes as u64, false);
                Ok(size_value.as_basic_value_enum().into())
            }
            IntrinsicFunction::BoolNegate => {
                let input_value = self.codegen_expr_basic_value(call.args[0])?.into_int_value();
                let truncated = self.bool_to_i1(input_value, "");
                let negated = self.builder.build_not(truncated, "").unwrap();
                let promoted = self.i1_to_bool(negated, "");
                Ok(promoted.as_basic_value_enum().into())
            }
            IntrinsicFunction::BitNot => {
                let input_value = self.codegen_expr_basic_value(call.args[0])?.into_int_value();
                let not_value = self.builder.build_not(input_value, "not").unwrap();
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
                let lhs = self.codegen_expr_basic_value(call.args[0])?.into_int_value();
                let rhs = self.codegen_expr_basic_value(call.args[1])?.into_int_value();
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
                let result = result.to_err(call.span)?;
                Ok(result.as_basic_value_enum().into())
            }
            IntrinsicFunction::TypeId => {
                let type_param = &call.type_args[0];
                let type_id_value = self
                    .codegen_integer_value(TypedIntegerValue::U64(type_param.type_id.to_u64()))?;
                Ok(type_id_value.into())
            }
            IntrinsicFunction::PointerIndex => {
                //  Reference:
                //  intern fn refAtIndex[T](self: Pointer, index: u64): T*
                let pointee_ty_arg = call.type_args[0];
                let elem_type = self.codegen_type(pointee_ty_arg.type_id)?;
                let ptr = self.codegen_expr_basic_value(call.args[0])?.into_pointer_value();
                let index = self.codegen_expr_basic_value(call.args[1])?.into_int_value();
                let result_pointer = unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            elem_type.rich_value_type(),
                            ptr,
                            &[index],
                            "refAtIndex",
                        )
                        .unwrap()
                };
                Ok(result_pointer.as_basic_value_enum().into())
            }
            IntrinsicFunction::CompilerSourceLocation => {
                unreachable!("CompilerSourceLocation is handled in typechecking phase")
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
        self.set_debug_location_from_span(block.span);
        for stmt in &block.statements {
            if let LlvmValue::Void(never_instr) = last {
                eprintln!("Aborting block after generating {}", never_instr);
                break;
            }
            last = self.codegen_statement(*stmt)?;
        }
        Ok(last)
    }

    fn codegen_statement(&mut self, statement: TypedStmtId) -> CodegenResult<LlvmValue<'ctx>> {
        match self.module.stmts.get(statement) {
            TypedStmt::Expr(expr, _) => self.codegen_expr(*expr),
            TypedStmt::Let(let_stmt) => self.codegen_let(let_stmt),
            TypedStmt::Assignment(assignment) => {
                let rhs = self.codegen_expr(assignment.value)?;
                let LlvmValue::BasicValue(rhs) = rhs else {
                    return Ok(rhs);
                };
                let lhs_pointer = match assignment.kind {
                    AssignmentKind::Value => {
                        match self.module.exprs.get(assignment.destination) {
                            // ASSIGNMENT! We're in lvalue land. We need to get the pointer to the
                            // destination, and be sure to call the correct variant of codegen_expr
                            TypedExpr::Variable(v) => {
                                let destination_ptr =
                                    *self.variables.get(&v.variable_id).expect("Missing variable");
                                destination_ptr.pointer_value
                            }
                            _ => {
                                panic!(
                                    "Invalid assignment lhs: {}",
                                    self.module.expr_to_string(assignment.destination)
                                )
                            }
                        }
                    }
                    AssignmentKind::Reference => {
                        self.codegen_expr_basic_value(assignment.destination)?.into_pointer_value()
                    }
                };

                let value_type =
                    self.codegen_type(self.module.exprs.get(assignment.value).get_type())?;
                self.store_k1_value(&value_type, lhs_pointer, rhs);
                Ok(self.builtin_types.unit_value.as_basic_value_enum().into())
            }
        }
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
        self.builder.build_unconditional_branch(loop_entry_block).unwrap();

        self.builder.position_at_end(loop_entry_block);
        let cond = self.codegen_expr_basic_value(while_loop.cond)?.into_int_value();
        let cond_i1 = self.bool_to_i1(cond, "while_cond");

        self.builder.build_conditional_branch(cond_i1, loop_body_block, loop_end_block).unwrap();

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(&while_loop.body)?;
        match body_value.as_basic_value() {
            Either::Left(_instr) => {}
            Either::Right(_bv) => {
                self.builder.build_unconditional_branch(loop_entry_block).unwrap();
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
        let break_value_ptr =
            self.builder.build_alloca(break_type.rich_value_type(), "break").unwrap();
        self.loops.insert(
            loop_expr.body.scope_id,
            LoopInfo { break_value_ptr: Some(break_value_ptr), end_block: loop_end_block },
        );

        // Go to the body
        self.builder.build_unconditional_branch(loop_body_block).unwrap();

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(&loop_expr.body)?;
        match body_value.as_basic_value() {
            Either::Left(_instr) => {}
            Either::Right(_bv) => {
                self.builder.build_unconditional_branch(loop_body_block).unwrap();
            }
        }

        self.builder.position_at_end(loop_end_block);
        let break_value = self.load_k1_value(&break_type, break_value_ptr, "loop_value", false);
        Ok(break_value.into())
    }

    fn make_function_debug_info(
        &mut self,
        function_name: &str,
        function_span: SpanId,
        return_type: DIType<'ctx>,
        param_debug_types: &[DIType<'ctx>],
    ) -> CodegenResult<(DISubprogram<'ctx>, DIFile<'ctx>)> {
        let span_id = function_span;
        let function_file_id = self.module.ast.spans.get(span_id).file_id;
        let (function_line, _) =
            self.module.ast.get_lines_for_span_id(span_id).expect("line for span");
        let function_line_number = function_line.line_number();
        let function_scope_start_line_number = function_line_number;
        let function_file = self.debug.files.get(&function_file_id).unwrap();
        let dbg_fn_type = self.debug.debug_builder.create_subroutine_type(
            *function_file,
            Some(return_type),
            param_debug_types,
            0,
        );
        let di_subprogram = self.debug.debug_builder.create_function(
            self.debug.current_scope(),
            function_name,
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
        Ok((di_subprogram, *function_file))
    }

    fn codegen_function_or_get(
        &mut self,
        function_id: FunctionId,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(function.function_value);
        }
        debug!("\ncodegen function\n{}", self.module.function_id_to_string(function_id, true));
        let previous_debug_location = self.get_debug_location();

        let function = self.module.get_function(function_id);
        let function_type_id = function.type_id;
        let function_type = self.module.types.get(function.type_id).as_function().unwrap();

        let function_line_number = self
            .module
            .ast
            .get_lines_for_span_id(self.module.ast.get_span_for_id(function.parsed_id))
            .expect("line for span")
            .1
            .line_number();

        let maybe_starting_block = self.builder.get_insert_block();

        let mut param_types: Vec<K1LlvmType<'ctx>> =
            Vec::with_capacity(function_type.physical_params.len());
        let mut param_metadata_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            Vec::with_capacity(function_type.physical_params.len());

        let llvm_function_type = self.make_llvm_function_type(function_type_id)?;
        let is_sret = llvm_function_type.is_sret;

        let sret_attribute = if is_sret {
            let struct_type = llvm_function_type.return_type.rich_value_type();
            let sret_attribute = self.ctx.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                struct_type.as_any_type_enum(),
            );
            Some(sret_attribute)
        } else {
            None
        };

        for param in function_type.physical_params.iter() {
            let res = self.codegen_type(param.type_id)?;
            param_metadata_types.push(BasicMetadataTypeEnum::from(res.canonical_repr_type()));
            param_types.push(res);
        }

        let llvm_name = match function.linkage {
            TyperLinkage::External(Some(name)) => self.module.name_of(name),
            _ => &self.module.make_qualified_name(function.scope, function.name, ".", true),
        };
        let llvm_linkage = match function.linkage {
            TyperLinkage::Standard => None,
            TyperLinkage::External(_name) => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        if self.llvm_module.get_function(llvm_name).is_some() {
            return failf!(
                self.module.ast.get_span_for_id(function.parsed_id),
                "Dupe function name: {}",
                llvm_name
            );
        }
        let function_value = self.llvm_module.add_function(
            llvm_name,
            llvm_function_type.llvm_function_type,
            llvm_linkage,
        );
        if let Some(attribute) = sret_attribute {
            function_value.add_attribute(AttributeLoc::Param(0), attribute);
        }

        let function_span = self.module.ast.get_span_for_id(function.parsed_id);
        let (di_subprogram, di_file) = self.make_function_debug_info(
            self.module.name_of(function.name),
            function_span,
            llvm_function_type.return_type.debug_type(),
            &llvm_function_type.param_types.iter().map(|t| t.debug_type()).collect::<Vec<_>>(),
        )?;

        self.llvm_functions.insert(
            function_id,
            CodegenedFunction {
                function_type: llvm_function_type,
                function_value,
                sret_pointer: None,
                instruction_count: 0,
            },
        );
        self.llvm_function_to_k1.insert(function_value, function_id);

        if matches!(function.linkage, Linkage::External(_)) {
            return Ok(function_value);
        }

        self.debug.push_scope(di_subprogram.as_debug_info_scope(), di_file);

        let entry_block = self.ctx.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);
        if is_sret {
            self.llvm_functions.get_mut(&function_id).unwrap().sret_pointer =
                Some(function_value.get_first_param().unwrap().into_pointer_value())
        }
        for (i, param) in function_value.get_param_iter().enumerate() {
            let sret_offset = if is_sret { 1 } else { 0 };
            let is_sret_param = i == 0 && is_sret;
            if is_sret_param {
                param.set_name("sret_ptr");
                continue;
            }
            let typed_param = if is_sret_param {
                &FnParamType {
                    name: self.module.ast.identifiers.get("ret").unwrap(),
                    type_id: function_type.return_type,
                    is_context: false,
                    is_lambda_env: false,
                    span: self.module.ast.get_span_for_id(function.parsed_id),
                }
            } else {
                &function_type.physical_params[i - sret_offset]
            };
            let ty = self.codegen_type(typed_param.type_id)?;
            let param_name = self.module.ast.identifiers.get_name(typed_param.name);
            trace!(
                "Got LLVM type for variable {}: {} (from {})",
                param_name,
                ty.rich_value_type(),
                self.module.type_id_to_string(typed_param.type_id)
            );
            param.set_name(&format!("{param_name}_arg"));
            self.set_debug_location_from_span(typed_param.span);
            let pointer = self.builder.build_alloca(ty.rich_value_type(), param_name).unwrap();
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
            let store_instr = self.store_k1_value(&ty, pointer, param);
            self.debug.debug_builder.insert_declare_before_instruction(
                pointer,
                Some(di_local_variable),
                None,
                self.set_debug_location_from_span(typed_param.span),
                store_instr,
            );
            let variable_id = function.param_variables[i - sret_offset];
            self.variables.insert(
                variable_id,
                VariablePointer {
                    pointer_value: pointer,
                    pointee_type_id: typed_param.type_id,
                    pointee_llvm_type: ty.canonical_repr_type(),
                },
            );
        }
        let _terminator_instr = match function.intrinsic_type {
            Some(intrinsic_type) => {
                trace!("codegen intrinsic {:?} fn {:?}", intrinsic_type, function);
                let value = self
                    .codegen_intrinsic_function_body(intrinsic_type, function)?
                    .as_basic_value_enum();
                let ret = self.builder.build_return(Some(&value)).unwrap();
                LlvmValue::Void(ret)
            }
            None => {
                let function_block = function.body_block.unwrap_or_else(|| {
                    panic!("Function has no block {}", self.get_ident_name(function.name))
                });
                let TypedExpr::Block(function_block) = self.module.exprs.get(function_block) else {
                    panic!("Expected block")
                };
                self.codegen_block(function_block)?
            }
        };
        if let Some(start_block) = maybe_starting_block {
            self.builder.position_at_end(start_block);
        }
        self.debug.pop_scope();
        function_value.set_subprogram(di_subprogram);

        self.set_debug_location(previous_debug_location);

        Ok(function_value)
    }

    fn count_function_instructions(function_value: FunctionValue<'ctx>) -> usize {
        let mut count = 0;
        // eprintln!("counting function {:?}", function_value.get_name().to_str());
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
        count
    }

    fn codegen_integer_value(
        &self,
        integer: TypedIntegerValue,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(integer.get_type())?;
        let llvm_int_ty = llvm_ty.rich_value_type().into_int_type();
        let Type::Integer(int_type) = self.module.types.get(llvm_ty.type_id()) else { panic!() };
        let llvm_value = if int_type.is_signed() {
            llvm_int_ty.const_int(integer.as_u64(), true)
        } else {
            llvm_int_ty.const_int(integer.as_u64(), false)
        };
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_float_value(&self, float: TypedFloatValue) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(float.get_type())?;
        let llvm_float_ty = llvm_ty.rich_value_type().into_float_type();
        let llvm_value = llvm_float_ty.const_float(float.as_f64());
        Ok(llvm_value.as_basic_value_enum())
    }

    pub fn codegen_module(&mut self) -> CodegenResult<()> {
        let start = std::time::Instant::now();
        for global in self.module.globals.iter() {
            let initialized_basic_value = self.codegen_compile_time_value(&global.initial_value);
            let variable = self.module.variables.get(global.variable_id);
            let name =
                self.module.make_qualified_name(variable.owner_scope, variable.name, "__", false);
            let llvm_global = self.llvm_module.add_global(
                initialized_basic_value.get_type(),
                Some(AddressSpace::default()),
                &name,
            );
            if global.is_comptime {
                llvm_global.set_constant(true);
            } else {
                llvm_global.set_constant(false);
            }
            llvm_global.set_initializer(&initialized_basic_value);
            self.globals.insert(global.variable_id, llvm_global);
        }
        // TODO: Codegen the exported functions as well as the called ones
        // for (id, function) in self.module.function_iter() {
        //     if function.linkage.is_exported() {
        //         self.codegen_function_or_get(id)?;
        //     }
        // }
        let Some(main_function_id) = self.module.get_main_function_id() else {
            return failf!(SpanId::NONE, "Module {} has no main function", self.module.name());
        };
        self.codegen_function_or_get(main_function_id)?;
        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    pub fn name(&self) -> &str {
        self.module.name()
    }

    fn set_up_machine(module: &mut LlvmModule) -> TargetMachine {
        // Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        // I use this explicit triple to avoid an annoying warning log on mac.
        // let triple_str = &format!("arm64-apple-macosx{}", MAC_SDK_VERSION);
        // let triple = TargetTriple::create(triple_str);

        let triple = TargetMachine::get_default_triple();
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

        let data_layout = &machine.get_target_data().get_data_layout();
        info!(
            "Initializing to 'native' target using triple {}. Layout: {}",
            target.get_name().to_string_lossy(),
            data_layout.as_str().to_string_lossy()
        );
        module.set_data_layout(data_layout);
        module.set_triple(&triple);

        machine
    }

    pub fn optimize_verify(&mut self, optimize: bool) -> anyhow::Result<()> {
        let start = std::time::Instant::now();

        if !self.debug.strip_debug {
            self.debug.debug_builder.finalize();
        } else {
            self.llvm_module.strip_debug_info();
        }
        self.llvm_module.verify().map_err(|err| {
            let llvm_text = self.output_llvm_ir_text();
            let mut f = std::fs::File::create(format!("{}_fail.ll", self.name()))
                .expect("Failed to create .ll file");
            std::io::Write::write_all(&mut f, llvm_text.as_bytes()).unwrap();
            anyhow::anyhow!("Module '{}' failed validation: {}", self.name(), err.to_string_lossy())
        })?;

        if optimize {
            self.llvm_module
                .run_passes("default<O3>", &self.llvm_machine, PassBuilderOptions::create())
                .unwrap();
        } else {
            self.llvm_module
                .run_passes("function(mem2reg)", &self.llvm_machine, PassBuilderOptions::create())
                .unwrap();
        }

        self.llvm_module.verify().unwrap();

        // self.llvm_machine.add_analysis_passes(&module_pass_manager);
        // module_pass_manager.run_on(&self.llvm_module);

        info!("codegen phase 'optimize' took {}ms", start.elapsed().as_millis());
        for (_, function) in self.llvm_functions.iter_mut() {
            let new_count = Codegen::count_function_instructions(function.function_value);
            function.instruction_count = new_count;
        }

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
