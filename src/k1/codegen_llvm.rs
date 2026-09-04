// Copyright (c) 2026 knix
// All rights reserved.

use std::num::NonZeroU32;
use std::path::Path;

use ahash::{HashMapExt, HashSetExt};
use fxhash::{FxHashMap, FxHashSet};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::{AsContextRef, Context};
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIExpression, DIFile, DILocalVariable, DILocation, DIScope,
    DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{
    AnyType, ArrayType, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType,
    VectorType as LlvmVectorType,
};
use inkwell::values::{
    ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue,
    FunctionValue, GlobalValue, InstructionValue, IntValue, PointerValue, StructValue, ValueKind,
};
use inkwell::{
    AddressSpace, AtomicOrdering, FloatPredicate, IntPredicate, OptimizationLevel, ThreadLocalMode,
};
use itertools::Itertools;
use llvm_sys::debuginfo::LLVMDIBuilderInsertDbgValueRecordAtEnd;
use llvm_sys::debuginfo::LLVMDIBuilderInsertDeclareRecordAtEnd;

use log::{debug, trace};

use crate::compiler::{self};
use crate::ir::{
    BackendBuiltin, BlockId, Inst, InstId, IrCallee, IrUnitId, PhysicalFunctionType, ProgramIr,
    Value,
};
use crate::kmem::{Handle, List, MSlice};
use crate::lex::SpanId;
use crate::parse::{FileId, StringId};
use crate::typer::types::{
    AbiMode, AggType, AggregateTypeId, Layout, PhysicalType, PhysicalTypeEnum, PhysicalTypeResult,
    ScalarType, Type, TypeDefnInfo, TypeId,
};
use crate::typer::{
    FunctionId, K1Result, Linkage as TyperLinkage, NamespaceId, StaticContainerKind,
    StaticRawContainer, StaticValue, StaticValueId, TypedFloatValue, TypedGlobalId, TypedIntValue,
    TypedProgram,
};
use crate::{SV8, ir, kbail, kmem};

#[allow(unused)]
fn llvm_size_info(td: &TargetData, typ: &dyn AnyType) -> Layout {
    Layout { size: td.get_abi_size(typ) as u32, align: td.get_abi_alignment(typ) }
}

/// llvm::CallingConv::Fast
const LLVM_CALL_CONV_FAST: u32 = 8;

#[derive(Clone, Copy)]
pub struct CgFunctionType<'ctx> {
    llvm_function_type: LlvmFunctionType<'ctx>,
    // Does not include sret, or abi mappings
    param_k1_types: MSlice<CgType<'ctx>, CgPerm>,

    // Does not include sret
    param_abi_mappings: MSlice<AbiParamMapping, CgPerm>,
    return_logical_cg_type: CgType<'ctx>,
    return_abi_mapping: AbiParamMapping,
    is_sret: bool,
}

#[derive(Copy, Clone, Debug)]
enum AbiParamMapping {
    VoidReturnEmpty,
    ScalarInRegister,
    /// How everyone does 1-8 byte structs
    StructInInteger {
        abi_width: u32,
        active_width: u32,
    },
    /// Same registers as an i64 but keeps pointer provenance
    StructAsPointer,
    /// A single SYS-V SSE eightbyte, which travels packed in one XMM register.
    /// clang types it `float`, `<2 x float>`, or `double`.
    StructInSse {
        element: ScalarType,
        count: u32,
    },
    /// How clang does X86 9-16 byte structs
    StructByEightbytePair {
        class1: RegisterClass,
        class2: RegisterClass,
        active_bits2: u32,
    },
    StructByHfa {
        element: ScalarType,
        count: u32,
    },
    /// How clang does ARM64 9-16 byte structs
    StructByIntPairArray,
    BigStructByPtrToCopy {
        byval_attr: bool,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RegisterClass {
    Initial,
    Int,
    Float,
    Ptr,
}

impl RegisterClass {
    fn combine(&self, other: RegisterClass) -> RegisterClass {
        match (self, other) {
            (RegisterClass::Initial, _) => other,
            // A pointer eightbyte is a GPR just like Int, so this never changes
            // which register is used; preferring Ptr for union overlaps keeps
            // the pointer type (and thus provenance/capabilities) in the IR
            (RegisterClass::Ptr, _) | (_, RegisterClass::Ptr) => RegisterClass::Ptr,
            (RegisterClass::Int, RegisterClass::Int) => RegisterClass::Int,
            (RegisterClass::Float, RegisterClass::Float) => RegisterClass::Float,
            // Anything can go in a general purpose register like an int, but
            // only more specific things (floats, vectors later) can go in
            // those special registers, so mixtures result in int
            _mix => RegisterClass::Int,
        }
    }
}

#[derive(Copy, Clone)]
struct LlvmScalarType<'ctx> {
    #[allow(unused)]
    pt: PhysicalType,
    basic_type: BasicTypeEnum<'ctx>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Clone, Copy)]
struct CgStructType<'ctx> {
    pt: PhysicalType,
    struct_type: StructType<'ctx>,
    fields: MSlice<CgType<'ctx>, CgPerm>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgArrayType<'ctx> {
    pt: PhysicalType,
    #[allow(unused)]
    count: u32,
    array_type: ArrayType<'ctx>,
    #[allow(unused)]
    element_type: Handle<CgType<'ctx>, CgPerm>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgVectorType<'ctx> {
    pt: PhysicalType,
    #[allow(unused)]
    count: u32,
    vector_type: LlvmVectorType<'ctx>,
    #[allow(unused)]
    element_type: Handle<CgType<'ctx>, CgPerm>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgUnionType<'ctx> {
    pt: PhysicalType,
    aligned_opaque_repr: StructType<'ctx>,
    #[allow(unused)]
    members: MSlice<CgType<'ctx>, CgPerm>,
    layout: Layout,
    di_type: DIType<'ctx>,
}

#[derive(Copy, Clone)]
enum CgType<'ctx> {
    Scalar(LlvmScalarType<'ctx>),
    StructType(CgStructType<'ctx>),
    ArrayType(CgArrayType<'ctx>),
    Vector(CgVectorType<'ctx>),
    Union(CgUnionType<'ctx>),
}

impl<'ctx> From<LlvmScalarType<'ctx>> for CgType<'ctx> {
    fn from(value: LlvmScalarType<'ctx>) -> Self {
        CgType::Scalar(value)
    }
}

impl<'ctx> From<CgStructType<'ctx>> for CgType<'ctx> {
    fn from(value: CgStructType<'ctx>) -> Self {
        CgType::StructType(value)
    }
}

impl<'ctx> CgType<'ctx> {
    pub fn pt(&self) -> PhysicalType {
        match self {
            CgType::Scalar(s) => s.pt,
            CgType::StructType(s) => s.pt,
            CgType::ArrayType(a) => a.pt,
            CgType::Vector(v) => v.pt,
            CgType::Union(u) => u.pt,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            CgType::Scalar(_) => "Scalar",
            CgType::StructType(_) => "StructType",
            CgType::ArrayType(_) => "ArrayType",
            CgType::Vector(_) => "Vector",
            CgType::Union(_) => "Union",
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            CgType::Scalar(_) => false,
            CgType::StructType(_) => true,
            CgType::ArrayType(_) => true,
            CgType::Vector(_) => true,
            CgType::Union(_) => true,
        }
    }

    #[track_caller]
    fn expect_struct(self) -> CgStructType<'ctx> {
        match self {
            CgType::StructType(s) => s,
            _ => panic!("expected struct on {}", self.kind_name()),
        }
    }

    #[track_caller]
    #[allow(unused)]
    fn expect_array(self) -> CgArrayType<'ctx> {
        match self {
            CgType::ArrayType(array) => array,
            _ => panic!("expected array on {}", self.kind_name()),
        }
    }

    fn rich_repr_layout(&self) -> Layout {
        match self {
            CgType::Scalar(value) => value.layout,
            CgType::StructType(s) => s.layout,
            CgType::ArrayType(a) => a.layout,
            CgType::Vector(v) => v.layout,
            CgType::Union(u) => u.layout,
        }
    }

    fn rich_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            CgType::Scalar(value) => value.basic_type,
            CgType::StructType(s) => s.struct_type.as_basic_type_enum(),
            CgType::ArrayType(a) => a.array_type.as_basic_type_enum(),
            CgType::Vector(v) => v.vector_type.as_basic_type_enum(),
            CgType::Union(u) => u.aligned_opaque_repr.as_basic_type_enum(),
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            CgType::Scalar(value) => value.di_type,
            CgType::StructType(s) => s.di_type,
            CgType::ArrayType(a) => a.di_type,
            CgType::Vector(v) => v.di_type,
            CgType::Union(u) => u.di_type,
        }
    }

    #[allow(unused)]
    fn as_scalar(self) -> Option<LlvmScalarType<'ctx>> {
        match self {
            CgType::Scalar(scalar) => Some(scalar),
            _ => None,
        }
    }
}

struct BuiltinTypes<'ctx> {
    scalars: [LlvmScalarType<'ctx>; 13],
    boolean: IntType<'ctx>,
    true_value: IntValue<'ctx>,
    false_value: IntValue<'ctx>,
    i1: IntType<'ctx>,
    char: IntType<'ctx>,
    ptr: PointerType<'ctx>,
    ptr_sized_int: IntType<'ctx>,
    empty_struct: StructType<'ctx>,
}

impl<'ctx> BuiltinTypes<'ctx> {
    fn empty_struct_value(&self) -> BasicValueEnum<'ctx> {
        self.empty_struct.get_undef().as_basic_value_enum()
    }
}

pub struct CgFunction<'ctx> {
    pub function_type: CgFunctionType<'ctx>,
    pub function_value: FunctionValue<'ctx>,
    pub blocks: FxHashMap<BlockId, BasicBlock<'ctx>>,
    /// These are canonical, not ABI-mapped, and also logical, as in,
    /// sret is excluded, so the first item is the first param the function actually takes
    pub param_values: Vec<BasicValueEnum<'ctx>>,
    pub last_alloca_instr: Option<InstructionValue<'ctx>>,
    pub returned_sret_variable: Option<InstId>,
    pub debug_info: DISubprogram<'ctx>,
    pub debug_file: DIFile<'ctx>,
}

pub struct CgPerm;
pub struct CodegenTmp;

#[derive(Debug)]
pub struct CgError {
    pub message: String,
    pub span: SpanId,
}
pub type CgResult<A> = Result<A, CgError>;

macro_rules! cgerr {
    ($span:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        CgError { message: format!($fmt $(, $arg)*), span: $span }
    };
}
macro_rules! cgbail {
    ($span:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        return Err(cgerr!($span, $fmt $(, $arg)*))
    };
}

pub struct UnitPlan {
    pub index: usize,
    pub count: usize,
    pub functions: Vec<FunctionId>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CgKind {
    Host,
    ReloadDylib(NamespaceId),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Pipeline {
    None,
    Dev,
    O3,
    ThinLtoPreLink,
}

pub enum UnitOutput {
    Object(Pipeline),
    Bitcode(Pipeline),
}

pub enum UnitArtifact {
    Object(String),
    Bitcode { bytes: Box<[u8]>, exported: Vec<String>, referenced: Vec<String> },
}

pub struct CodegenRoots {
    pub main: Option<FunctionId>,
    pub program_exit: Option<FunctionId>,
    pub exports: Vec<FunctionId>,
    pub reachable: Vec<FunctionId>,
}

struct SharedProgram<'a>(&'a TypedProgram);
unsafe impl Sync for SharedProgram<'_> {}

pub struct Cg<'ctx, 'k1> {
    ctx: &'ctx Context,
    pub k1: &'k1 TypedProgram,
    kind: CgKind,
    plan: UnitPlan,
    owned: FxHashSet<FunctionId>,
    has_reloadable_fns: bool,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: TargetMachine,
    builder: Builder<'ctx>,
    pub llvm_functions: FxHashMap<FunctionId, CgFunction<'ctx>>,
    functions_pending_body_compilation: Vec<FunctionId>,
    llvm_types: FxHashMap<AggregateTypeId, CgType<'ctx>>,
    globals: FxHashMap<TypedGlobalId, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    strings: FxHashMap<StringId, GlobalValue<'ctx>>,
    static_values_basics: FxHashMap<StaticValueId, BasicValueEnum<'ctx>>,
    static_values_globals: FxHashMap<StaticValueId, GlobalValue<'ctx>>,
    debug: DebugContext<'ctx>,
    tmp: kmem::Mem<CodegenTmp>,
    mem: kmem::Mem<CgPerm>,

    current_insert_function: FunctionId,
    last_debug_location: std::cell::Cell<Option<(SpanId, DILocation<'ctx>)>>,

    buffers: CgBuffers,
}

struct CgBuffers {
    cfg_seen: FxHashSet<BlockId>,
    cfg_blocks_rpo: Vec<BlockId>,
}

struct DebugContext<'ctx> {
    files: FxHashMap<FileId, DIFile<'ctx>>,
    debug_builder: DebugInfoBuilder<'ctx>,
    #[allow(unused)]
    compile_unit: DICompileUnit<'ctx>,
    debug_stack: Vec<DebugStackEntry<'ctx>>,
    line_tables_only: bool,
}

impl<'ctx> DebugContext<'ctx> {
    // Missing inkwell function
    pub fn insert_dbg_value_at_end(
        &self,
        value: BasicValueEnum<'ctx>,
        var_info: DILocalVariable<'ctx>,
        expr: Option<DIExpression<'ctx>>,
        debug_loc: DILocation<'ctx>,
        block: BasicBlock<'ctx>,
    ) {
        unsafe {
            LLVMDIBuilderInsertDbgValueRecordAtEnd(
                self.debug_builder.as_mut_ptr(),
                value.as_value_ref(),
                var_info.as_mut_ptr(),
                expr.unwrap_or_else(|| self.debug_builder.create_expression(vec![])).as_mut_ptr(),
                debug_loc.as_mut_ptr(),
                block.as_mut_ptr(),
            )
        };
    }

    // Bugged inkwell function: https://github.com/TheDan64/inkwell/issues/613
    pub fn insert_declare_at_end(
        &self,
        storage: PointerValue<'ctx>,
        var_info: Option<DILocalVariable<'ctx>>,
        expr: Option<DIExpression<'ctx>>,
        debug_loc: DILocation<'ctx>,
        block: BasicBlock<'ctx>,
    ) {
        let _dbg_record = unsafe {
            LLVMDIBuilderInsertDeclareRecordAtEnd(
                self.debug_builder.as_mut_ptr(),
                storage.as_value_ref(),
                var_info.map(|v| v.as_mut_ptr()).unwrap_or(std::ptr::null_mut()),
                expr.unwrap_or_else(|| self.debug_builder.create_expression(vec![])).as_mut_ptr(),
                debug_loc.as_mut_ptr(),
                block.as_mut_ptr(),
            )
        };
    }

    fn push_scope(&mut self, span: SpanId, scope: DIScope<'ctx>, file: DIFile<'ctx>) {
        self.debug_stack.push(DebugStackEntry { span, scope, file });
    }
    fn pop_scope(&mut self) {
        self.debug_stack.pop();
    }
    fn current_entry(&self) -> &DebugStackEntry<'ctx> {
        self.debug_stack.last().unwrap()
    }
    fn current_span(&self) -> SpanId {
        self.current_entry().span
    }
    fn current_scope(&self) -> DIScope<'ctx> {
        self.current_entry().scope
    }
    fn current_file(&self) -> DIFile<'ctx> {
        self.current_entry().file
    }
}

#[derive(Debug)]
struct DebugStackEntry<'ctx> {
    span: SpanId,
    scope: DIScope<'ctx>,
    file: DIFile<'ctx>,
}

fn i8_array_from_str<'ctx>(ctx: &'ctx Context, value: &str) -> ArrayValue<'ctx> {
    let bytes = value.as_bytes();
    ctx.const_string(bytes, false)
}

pub fn run_passes(module: &LlvmModule, machine: &TargetMachine, pipeline: Pipeline) {
    // PassBuilderOptions leaves SLP vectorization off by default; clang
    // turns it on at O2+
    let options = PassBuilderOptions::create();
    options.set_loop_slp_vectorization(true);
    let text = match pipeline {
        Pipeline::None => return,
        Pipeline::O3 => "default<O3>",
        Pipeline::ThinLtoPreLink => "thinlto-pre-link<O3>",
        // Default builds, not optimized but not debug
        Pipeline::Dev => {
            "function(mem2reg,instcombine<no-verify-fixpoint;max-iterations=1>,simplifycfg),globaldce,mergefunc"
        }
    };
    module.run_passes(text, machine, options).unwrap();
}

pub fn emit_object(module: &LlvmModule, machine: &TargetMachine, path: &str) -> CgResult<()> {
    if let Err(e) =
        machine.write_to_file(module, inkwell::targets::FileType::Object, Path::new(path))
    {
        cgbail!(SpanId::NONE, "Error writing object file to path {path}: {}", e.to_string_lossy());
    }
    Ok(())
}

/// Textual IR shaped for Fil-C's LLVM fork, which stock LLVM cannot
/// express through its API: the input datalayout must mark address space 0
/// non-integral (`ni:0`, rejected by upstream DataLayout parsing), and the
/// fork reads the post-instrumentation layout from its own
/// `datalayout_after_filc` module field. Patch both into the header.
pub fn llvm_ir_text_filc(module: &LlvmModule) -> String {
    let text = module.print_to_string().to_string();
    let dl_prefix = "target datalayout = \"";
    let dl_start = text.find(dl_prefix).expect("module has no datalayout line");
    let layout_start = dl_start + dl_prefix.len();
    let layout_end = layout_start + text[layout_start..].find('"').unwrap();
    let layout = &text[layout_start..layout_end];
    let line_end = layout_end + 1;
    format!(
        "{}target datalayout = \"e-m:e-ni:0-{}\"\ntarget datalayout_after_filc = \"{}\"{}",
        &text[..dl_start],
        layout.strip_prefix("e-m:e-").expect("expected x86_64 linux datalayout"),
        layout,
        &text[line_end..]
    )
}

fn buffer_bytes(buffer: &inkwell::memory_buffer::MemoryBuffer) -> Box<[u8]> {
    unsafe {
        let start = llvm_sys::core::LLVMGetBufferStart(buffer.as_mut_ptr()) as *const u8;
        let size = llvm_sys::core::LLVMGetBufferSize(buffer.as_mut_ptr());
        std::slice::from_raw_parts(start, size).into()
    }
}

#[repr(C)]
struct K1ThinLtoUnit {
    data: *const u8,
    len: usize,
}

struct ThinLtoSink<'a> {
    paths: &'a [String],
    error: Option<String>,
}

extern "C" fn thinlto_write_object(
    ctx: *mut std::ffi::c_void,
    index: usize,
    data: *const u8,
    len: usize,
) {
    let sink = unsafe { &mut *(ctx as *mut ThinLtoSink) };
    let bytes = unsafe { std::slice::from_raw_parts(data, len) };
    let path = &sink.paths[index];
    if let Err(e) = std::fs::write(path, bytes) {
        sink.error = Some(format!("Error writing object file to path {path}: {e}"));
    }
}

unsafe extern "C" {
    fn k1_thinlto_bitcode(
        module: llvm_sys::prelude::LLVMModuleRef,
    ) -> llvm_sys::prelude::LLVMMemoryBufferRef;
    fn k1_thinlto_codegen(
        units: *const K1ThinLtoUnit,
        unit_count: usize,
        cpu: *const std::ffi::c_char,
        features: *const std::ffi::c_char,
        pic: i32,
        preserved: *const *const std::ffi::c_char,
        preserved_count: usize,
        cross_referenced: *const *const std::ffi::c_char,
        cross_referenced_count: usize,
        cache_dir: *const std::ffi::c_char,
        emit: extern "C" fn(*mut std::ffi::c_void, usize, *const u8, usize),
        ctx: *mut std::ffi::c_void,
    ) -> i32;
}

fn write_failure_file(module: &LlvmModule, name: &str) {
    let llvm_text = module.print_to_string().to_string();
    let mut f =
        std::fs::File::create(format!("{name}_fail.ll")).expect("Failed to create .ll file");
    std::io::Write::write_all(&mut f, llvm_text.as_bytes()).unwrap();
}

impl<'ctx, 'module> Cg<'ctx, 'module> {
    fn init_debug(
        ctx: &'ctx Context,
        llvm_module: &LlvmModule<'ctx>,
        module: &TypedProgram,
        optimize: bool,
        debug: bool,
    ) -> DebugContext<'ctx> {
        // We may need to create a DIBuilder per-file.
        // For now let's use main file
        let source = module.ast.sources.get_main();
        let is_macos = module.config.target.platform() == compiler::Platform::PosixMacos;
        let sysroot = if is_macos { compiler::MAC_SDK_SYSROOT } else { "" };
        let sdk = if is_macos { "MacOSX.sdk" } else { "" };
        let (debug_builder, compile_unit) = llvm_module.create_debug_info_builder(
            false,
            DWARFSourceLanguage::C,
            source.filename_str(&module.ast.idents),
            source.directory_str(&module.ast.idents),
            "k1_compiler",
            optimize,
            "",
            0,
            "",
            if debug { DWARFEmissionKind::Full } else { DWARFEmissionKind::LineTablesOnly },
            0,
            false,
            false,
            sysroot,
            sdk,
        );
        let md0 = ctx.metadata_node(&[
            ctx.i32_type().const_int(2, false).into(),
            ctx.metadata_string("SDK Version").into(),
            ctx.i32_type()
                .const_array(&[
                    ctx.i32_type().const_int(15, false),
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
        let md3 = ctx.metadata_node(&[
            ctx.i32_type().const_int(1, false).into(),
            ctx.metadata_string("PIC Level").into(),
            ctx.i32_type().const_int(2, false).into(),
        ]);
        // revisit this metadata (I did it when scrambling to get debug info to show up)
        // I know that at least the dwarf version is required by lldb
        llvm_module.add_global_metadata("llvm.module.flags", &md0).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md1).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md2).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md3).unwrap();

        // Only an executable is position-independent *and* known not to be
        // loaded elsewhere
        if module.program_settings.executable {
            let md4 = ctx.metadata_node(&[
                ctx.i32_type().const_int(1, false).into(),
                ctx.metadata_string("PIE Level").into(),
                ctx.i32_type().const_int(2, false).into(),
            ]);
            llvm_module.add_global_metadata("llvm.module.flags", &md4).unwrap();
        }

        let mut di_files: FxHashMap<FileId, DIFile> = FxHashMap::default();
        for (file_id, source) in module.ast.sources.iter() {
            let filename = source.filename_str(&module.ast.idents);
            let directory = source.directory_str(&module.ast.idents);
            di_files.insert(file_id, debug_builder.create_file(filename, directory));
        }
        let mut debug = DebugContext {
            files: di_files,
            debug_builder,
            compile_unit,
            debug_stack: Vec::new(),
            line_tables_only: !debug,
        };
        debug.push_scope(SpanId::NONE, compile_unit.as_debug_info_scope(), compile_unit.get_file());
        debug
    }

    pub fn create(
        ctx: &'ctx Context,
        k1: &'module TypedProgram,
        debug: bool,
        optimize: bool,
        kind: CgKind,
        plan: UnitPlan,
    ) -> Self {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let llvm_module = ctx.create_module(k1.program_name());
        llvm_module.set_source_file_name(k1.ast.sources.get_main().filename_str(&k1.ast.idents));

        let debug_context = Cg::init_debug(ctx, &llvm_module, k1, optimize, debug);

        Cg::initialize_targets();
        let machine = Cg::make_target_machine(optimize, k1.config.target);
        let target_data = machine.get_target_data();
        llvm_module.set_data_layout(&target_data.get_data_layout());
        llvm_module.set_triple(&machine.get_triple());

        if !k1.config.emit_llvm {
            unsafe {
                llvm_sys::core::LLVMContextSetDiscardValueNames(ctx.as_ctx_ref(), 1);
            }
        }

        let ptr = ctx.ptr_type(AddressSpace::default());
        let scalars = std::array::from_fn(|i| {
            Cg::make_scalar_type(ctx, &debug_context, ptr, ScalarType::from_tag(i as u32 + 1))
        });
        let builtin_types = BuiltinTypes {
            scalars,
            boolean: ctx.i8_type(),
            true_value: ctx.i8_type().const_int(1, false),
            false_value: ctx.i8_type().const_int(0, false),
            i1: ctx.bool_type(),
            char: char_type,
            ptr,
            ptr_sized_int: ctx.ptr_sized_int_type(&target_data, None),
            empty_struct: ctx.struct_type(&[], false),
        };

        let has_reloadable_fns = k1.namespaces.iter().any(|ns| ns.reload);
        let mut owned = FxHashSet::with_capacity(plan.functions.len());
        for f in &plan.functions {
            owned.insert(*f);
        }

        Cg {
            ctx,
            k1,
            kind,
            plan,
            owned,
            has_reloadable_fns,
            llvm_module,
            llvm_machine: machine,
            builder,
            globals: FxHashMap::new(),
            //lambda_functions: FxHashMap::new(),
            llvm_functions: FxHashMap::new(),
            functions_pending_body_compilation: Vec::new(),
            llvm_types: FxHashMap::new(),
            builtin_types,
            strings: FxHashMap::new(),
            static_values_basics: FxHashMap::new(),
            static_values_globals: FxHashMap::new(),
            debug: debug_context,
            tmp: kmem::Mem::make(),
            mem: kmem::Mem::make(),

            current_insert_function: FunctionId::PENDING,
            last_debug_location: std::cell::Cell::new(None),

            buffers: CgBuffers {
                cfg_seen: FxHashSet::new(),
                cfg_blocks_rpo: Vec::with_capacity(16),
            },
        }
    }

    fn multi_unit(&self) -> bool {
        self.plan.count > 1
    }

    fn is_first_unit(&self) -> bool {
        self.plan.index == 0
    }

    pub fn codegen_program(&mut self, roots: &CodegenRoots) -> CgResult<()> {
        let first_unit = self.is_first_unit();
        let plan_functions = self.plan.functions.clone();

        if first_unit {
            match self.kind {
                CgKind::Host => {
                    let global_ids: Vec<TypedGlobalId> = self.k1.globals.iter_ids().collect();
                    for global_id in &global_ids {
                        let g = self.k1.globals.get(*global_id);
                        if g.reload_ns.is_some() {
                            // We don't own this; the dylib does
                            continue;
                        }
                        let shared = !g.is_exported && !self.has_reloadable_fns && g.is_constant;
                        if g.is_exported
                            || self.has_reloadable_fns
                            || (self.multi_unit() && !shared)
                        {
                            self.codegen_global(*global_id)?;
                        }
                    }
                    if self.has_reloadable_fns {
                        let mut reloadable: Vec<FunctionId> = vec![];
                        for (function_id, function) in self.k1.function_iter() {
                            if function.is_reloadable() {
                                reloadable.push(function_id);
                            }
                        }
                        for function_id in reloadable {
                            self.reload_fn_addr_global(function_id);
                        }
                        for global_id in &global_ids {
                            if self.k1.globals.get(*global_id).reload_ns.is_some() {
                                self.codegen_reload_global_addr_slot(*global_id);
                            }
                        }
                    }
                }
                CgKind::ReloadDylib(ns_id) => {
                    // The load gate: the loader compares this stamp against the
                    // running host's descriptor hash and refuses a drifted api
                    let api_hash = self.k1.reload_hash_for_ns(ns_id);
                    let mut ns_path = String::with_capacity(64);
                    self.k1.write_scope_path(
                        &mut ns_path,
                        self.k1.namespaces.get(ns_id).scope_id,
                        "/",
                        true,
                    );
                    let hash_global = self.llvm_module.add_global(
                        self.ctx.i64_type(),
                        None,
                        &format!("__k1_reload_hash_{ns_path}"),
                    );
                    hash_global.set_initializer(&self.ctx.i64_type().const_int(api_hash, false));
                    hash_global.set_constant(true);
                    hash_global.set_alignment(8);
                    hash_global.set_linkage(LlvmLinkage::External);

                    // The ns's globals are roots too: every one must be defined
                    // and exported here, referenced or not, or the loader's
                    // all-or-nothing dlsym pass refuses the artifact
                    let mut ns_globals: Vec<TypedGlobalId> = vec![];
                    for global_id in self.k1.globals.iter_ids() {
                        if self.k1.globals.get(global_id).reload_ns == Some(ns_id) {
                            ns_globals.push(global_id);
                        }
                    }
                    for global_id in ns_globals {
                        self.codegen_global(global_id)?;
                    }
                }
            }
        }

        for function_id in &plan_functions {
            self.declare_llvm_function(*function_id)?;
        }
        let main_function = match roots.main {
            Some(main_function_id) if first_unit => {
                let function_value = self.declare_llvm_function(main_function_id)?;
                Some((main_function_id, function_value))
            }
            _ => None,
        };
        let program_exit_value = match (main_function, roots.program_exit) {
            (Some(_), Some(id)) => Some(self.declare_llvm_function(id)?),
            _ => None,
        };

        let mut inst_mappings = FxHashMap::with_capacity(512);
        while let Some(fn_id) = self.functions_pending_body_compilation.pop() {
            self.codegen_function_body(&mut inst_mappings, fn_id)?;
        }

        if let Some((_, function_value)) = main_function {
            self.builder.unset_current_debug_location();
            let is_wasi = self.k1.config.target.platform() == compiler::Platform::Wasi;
            let (entrypoint_name, entrypoint_fn_type) = if is_wasi {
                // WASI rejects any entry signature other than void _start()
                if !function_value.get_type().get_param_types().is_empty() {
                    cgbail!(SpanId::NONE, "main with parameters is not supported on wasm");
                }
                ("_start", self.ctx.void_type().fn_type(&[], false))
            } else {
                (
                    "main",
                    self.ctx
                        .i32_type()
                        .fn_type(&function_value.get_type().get_param_types(), false),
                )
            };
            let entrypoint =
                self.llvm_module.add_function(entrypoint_name, entrypoint_fn_type, None);
            let entry_block = self.ctx.append_basic_block(entrypoint, "entry");
            self.builder.position_at_end(entry_block);
            let entrypoint_params = entrypoint.get_params();
            let mut params: Vec<BasicMetadataValueEnum<'ctx>> =
                Vec::with_capacity(entrypoint_params.len());
            for p in &entrypoint_params {
                params.push((*p).into());
            }
            let main_call = self.builder.build_call(function_value, &params, "").unwrap();
            main_call.set_call_convention(function_value.get_call_conventions());
            let res = main_call.try_as_basic_value().basic();
            let exit_code: BasicValueEnum<'ctx> = match res {
                None => self.ctx.i32_type().const_zero().as_basic_value_enum(),
                Some(v) => v,
            };
            let exit_fv = program_exit_value.unwrap();
            let exit_call = self.builder.build_call(exit_fv, &[exit_code.into()], "").unwrap();
            exit_call.set_call_convention(exit_fv.get_call_conventions());
            self.builder.build_unreachable().unwrap();
        }

        if first_unit && self.kind == CgKind::Host && self.has_reloadable_fns {
            self.emit_reload_descriptors();
        }

        Ok(())
    }

    fn inst_function_refs(k1: &TypedProgram, inst: &Inst, refs: &mut Vec<FunctionId>) {
        if let Inst::Call { call_id } = inst {
            match k1.ir.calls.get(*call_id).callee {
                IrCallee::Direct(id)
                | IrCallee::Extern { function_id: id, .. }
                | IrCallee::BackendBuiltin(id, _) => refs.push(id),
                IrCallee::LlvmIntrinsic { .. } | IrCallee::Indirect(..) => {}
            }
        }
        ir::visit_inst_values(&k1.ir, inst, &mut |v| {
            if let Value::FunctionAddr(id) = v {
                refs.push(id)
            }
        });
    }

    fn live_successors(ir: &ProgramIr, block_id: BlockId, out: &mut Vec<BlockId>) {
        let block = &ir.mem.get(block_id).data;
        let mut last: Option<InstId> = None;
        for inst_id in ir.mem.dlist_iter(block.instrs) {
            last = Some(*inst_id);
        }
        let Some(last) = last else { return };
        match ir.instrs.get(last) {
            Inst::Jump(target) => out.push(*target),
            Inst::JumpIf { cond, cons, alt } => {
                if *cond != Value::IsStatic {
                    out.push(*cons);
                }
                out.push(*alt);
            }
            Inst::Switch { cases, default, .. } => {
                for case in ir.mem.getn(*cases) {
                    out.push(case.target);
                }
                out.push(*default);
            }
            _ => {}
        }
    }

    fn walk_functions(k1: &TypedProgram, roots: &[FunctionId]) -> Vec<FunctionId> {
        let mut reachable: Vec<FunctionId> = Vec::with_capacity(1024);
        let mut seen: FxHashSet<FunctionId> = FxHashSet::with_capacity(1024);
        let mut worklist: Vec<FunctionId> = roots.to_vec();
        let mut seen_blocks: FxHashSet<BlockId> = FxHashSet::with_capacity(64);
        let mut block_worklist: Vec<BlockId> = Vec::with_capacity(64);
        while let Some(function_id) = worklist.pop() {
            if !seen.insert(function_id) {
                continue;
            }
            reachable.push(function_id);
            let Some(unit) = k1.ir.functions.get(&function_id) else { continue };
            seen_blocks.clear();
            block_worklist.clear();
            if !unit.blocks.first.is_nil() {
                block_worklist.push(unit.blocks.first);
            }
            while let Some(block_id) = block_worklist.pop() {
                if !seen_blocks.insert(block_id) {
                    continue;
                }
                let block = &k1.ir.mem.get(block_id).data;
                for inst_id in k1.ir.mem.dlist_iter(block.instrs) {
                    Cg::inst_function_refs(k1, k1.ir.instrs.get(*inst_id), &mut worklist);
                }
                Cg::live_successors(&k1.ir, block_id, &mut block_worklist);
            }
        }
        reachable
    }

    pub fn prepare_ir(k1: &mut TypedProgram, roots: &[FunctionId]) -> K1Result<Vec<FunctionId>> {
        let mut roots = roots.to_vec();
        if k1.namespaces.iter().any(|ns| ns.reload) {
            let crash_ident = k1.ast.idents.intern("crash-unloaded-ns");
            let Some(crash_id) = k1.scopes.find_function_local(k1.scopes.k1_scope_id, crash_ident)
            else {
                kbail!(k1, SpanId::NONE, "core is missing fn k1/crash-unloaded-ns");
            };
            roots.push(crash_id);
        }
        let roots = &roots;
        for root in roots {
            ir::compile_function(k1, *root)?;
        }
        k1.compile_all_pending_ir(SpanId::NONE)?;
        if k1.config.optimize {
            for root in roots {
                ir::optimize_unit(k1, IrUnitId::Function(*root));
            }
        }
        let reachable = Cg::walk_functions(k1, roots);
        k1.compute_all_physical_types();
        Ok(reachable)
    }

    pub fn prepare_dylib(k1: &mut TypedProgram, ns_id: NamespaceId) -> K1Result<CodegenRoots> {
        let mut roots: Vec<FunctionId> = vec![];
        for (function_id, function) in k1.function_iter() {
            if function.is_reloadable() && function.namespace_id == ns_id {
                roots.push(function_id);
            }
        }
        let reachable = Cg::prepare_ir(k1, &roots)?;
        Ok(CodegenRoots { main: None, program_exit: None, exports: vec![], reachable })
    }

    pub fn prepare_host(k1: &mut TypedProgram) -> K1Result<CodegenRoots> {
        let mut exports: Vec<FunctionId> = vec![];
        let mut any_exported_global = false;
        for global_id in k1.globals.iter_ids() {
            if k1.globals.get(global_id).is_exported {
                any_exported_global = true;
            }
        }
        for (function_id, function) in k1.function_iter() {
            if function.linkage.is_exported() {
                exports.push(function_id);
            }
        }
        let main = if k1.program_settings.executable {
            let Some(main_function_id) = k1.get_main_function_id() else {
                kbail!(k1, SpanId::NONE, "Program {} has no main function", k1.program_name());
            };
            Some(main_function_id)
        } else {
            if exports.is_empty() && !any_exported_global {
                kbail!(
                    k1,
                    SpanId::NONE,
                    "Library {} exports no functions or globals",
                    k1.program_name()
                );
            }
            None
        };
        let program_exit = if main.is_some() {
            let program_exit_ident = k1.ast.idents.intern("program-exit");
            let Some(program_exit_id) =
                k1.scopes.find_function(k1.scopes.k1_scope_id, program_exit_ident)
            else {
                kbail!(k1, SpanId::NONE, "Missing k1/program-exit");
            };
            Some(program_exit_id)
        } else {
            None
        };
        let mut roots: Vec<FunctionId> = Vec::with_capacity(exports.len() + 2);
        roots.extend(main);
        roots.extend(program_exit);
        roots.extend_from_slice(&exports);
        let reachable = Cg::prepare_ir(k1, &roots)?;
        Ok(CodegenRoots { main, program_exit, exports, reachable })
    }

    pub fn plan_units(
        k1: &TypedProgram,
        reachable: &[FunctionId],
        max_units: usize,
    ) -> Vec<UnitPlan> {
        const MIN_UNIT_INSTRUCTIONS: u32 = 8 * 1024;
        let mut sized: Vec<(u32, FunctionId)> = Vec::with_capacity(reachable.len());
        let mut total: u32 = 0;
        for function_id in reachable {
            let size = match k1.ir.functions.get(function_id) {
                Some(unit) => unit.inst_count + 1,
                None => 0,
            };
            total += size;
            sized.push((size, *function_id));
        }
        let count = ((total / MIN_UNIT_INSTRUCTIONS) as usize).clamp(1, max_units.max(1));
        let mut plans: Vec<UnitPlan> = Vec::with_capacity(count);
        let mut loads: Vec<u32> = Vec::with_capacity(count);
        for index in 0..count {
            plans.push(UnitPlan {
                index,
                count,
                functions: Vec::with_capacity(sized.len() / count + 1),
            });
            loads.push(0);
        }
        sized.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.as_u32().cmp(&b.1.as_u32())));
        for (size, function_id) in sized {
            let mut lightest = 0;
            for (i, load) in loads.iter().enumerate() {
                if *load < loads[lightest] {
                    lightest = i;
                }
            }
            loads[lightest] += size;
            plans[lightest].functions.push(function_id);
        }
        plans
    }

    pub fn collect_module_exported_symbols(&self) -> Vec<String> {
        let mut names = vec![];
        for g in self.llvm_module.get_globals() {
            if !g.is_declaration()
                && g.get_linkage() == LlvmLinkage::External
                && g.get_visibility() == inkwell::GlobalVisibility::Default
            {
                names.push(g.get_name().to_string_lossy().into_owned());
            }
        }
        for f in self.llvm_module.get_functions() {
            let g = f.as_global_value();
            if !g.is_declaration()
                && f.get_linkage() == LlvmLinkage::External
                && g.get_visibility() == inkwell::GlobalVisibility::Default
            {
                names.push(f.get_name().to_string_lossy().into_owned());
            }
        }
        names
    }

    pub fn collect_all_module_symbols(&self) -> Vec<String> {
        let mut names = vec![];
        for g in self.llvm_module.get_globals() {
            if g.is_declaration() {
                names.push(g.get_name().to_string_lossy().into_owned());
            }
        }
        for f in self.llvm_module.get_functions() {
            if f.as_global_value().is_declaration() && f.get_intrinsic_id() == 0 {
                names.push(f.get_name().to_string_lossy().into_owned());
            }
        }
        names
    }

    pub fn codegen_units(
        k1: &TypedProgram,
        roots: &CodegenRoots,
        plans: Vec<UnitPlan>,
        kind: CgKind,
        debug: bool,
        output: UnitOutput,
        object_path: impl Fn(usize) -> String + Sync,
    ) -> CgResult<Vec<UnitArtifact>> {
        Cg::initialize_targets();
        let shared = SharedProgram(k1);
        let shared = &shared;
        let output = &output;
        let object_path = &object_path;
        let optimize = k1.config.optimize;
        let chatty = k1.config.chatty;
        let unit_count = plans.len();
        let cores = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1);
        let worker_count = unit_count.min(cores).max(1);
        let queue = std::sync::Mutex::new(plans);
        let queue = &queue;
        let mut artifacts: Vec<Option<UnitArtifact>> = Vec::with_capacity(unit_count);
        for _ in 0..unit_count {
            artifacts.push(None);
        }
        let artifacts = std::sync::Mutex::new(artifacts);
        let artifacts = &artifacts;
        let mut results = Vec::with_capacity(worker_count);
        std::thread::scope(|scope| {
            let mut workers = Vec::with_capacity(worker_count);
            for worker_index in 0..worker_count {
                let worker = std::thread::Builder::new()
                    .stack_size(crate::STACK_SIZE)
                    .name(format!("{}.cg{}", k1.program_name(), worker_index))
                    .spawn_scoped(scope, move || -> CgResult<()> {
                        loop {
                            let Some(plan) = queue.lock().unwrap().pop() else { return Ok(()) };
                            let start = std::time::Instant::now();
                            let index = plan.index;
                            let ctx = Context::create();
                            let mut cg = Cg::create(&ctx, shared.0, debug, optimize, kind, plan);
                            cg.codegen_program(roots)?;
                            let generated = start.elapsed();
                            cg.finalize_debug_info();
                            cg.verify()?;
                            let artifact = match output {
                                UnitOutput::Object(pipeline) => {
                                    cg.run_passes(*pipeline);
                                    let path = object_path(index);
                                    cg.emit_object_file(&path)?;
                                    UnitArtifact::Object(path)
                                }
                                UnitOutput::Bitcode(pipeline) => {
                                    cg.run_passes(*pipeline);
                                    let bytes = if *pipeline == Pipeline::ThinLtoPreLink {
                                        cg.thinlto_bitcode()
                                    } else {
                                        buffer_bytes(&cg.llvm_module.write_bitcode_to_memory())
                                    };
                                    UnitArtifact::Bitcode {
                                        bytes,
                                        exported: cg.collect_module_exported_symbols(),
                                        referenced: cg.collect_all_module_symbols(),
                                    }
                                }
                            };
                            if chatty {
                                eprintln!(
                                    "unit {index}: {} fns; codegen {}ms, passes + output {}ms",
                                    cg.owned.len(),
                                    generated.as_millis(),
                                    (start.elapsed() - generated).as_millis()
                                );
                            }
                            artifacts.lock().unwrap()[index] = Some(artifact);
                        }
                    })
                    .unwrap();
                workers.push(worker);
            }
            for worker in workers {
                results.push(match worker.join() {
                    Ok(result) => result,
                    Err(_) => Err(cgerr!(SpanId::NONE, "a codegen unit panicked")),
                });
            }
        });
        for result in results {
            result?;
        }
        let mut done = Vec::with_capacity(unit_count);
        let artifacts = std::mem::take(&mut *artifacts.lock().unwrap());
        for artifact in artifacts {
            done.push(artifact.expect("every unit produces an artifact"));
        }
        Ok(done)
    }

    fn thinlto_bitcode(&self) -> Box<[u8]> {
        let buffer = unsafe {
            inkwell::memory_buffer::MemoryBuffer::new(k1_thinlto_bitcode(
                self.llvm_module.as_mut_ptr(),
            ))
        };
        buffer_bytes(&buffer)
    }

    pub fn merge_units<'c>(
        ctx: &'c Context,
        name: &str,
        artifacts: &[UnitArtifact],
    ) -> CgResult<LlvmModule<'c>> {
        let mut merged: Option<LlvmModule<'c>> = None;
        for (i, artifact) in artifacts.iter().enumerate() {
            let UnitArtifact::Bitcode { bytes, .. } = artifact else {
                panic!("merge_units on an object artifact")
            };
            let buffer_name = std::ffi::CString::new(format!("{name}.{i}")).unwrap();
            let buffer = unsafe {
                inkwell::memory_buffer::MemoryBuffer::new(
                    llvm_sys::core::LLVMCreateMemoryBufferWithMemoryRangeCopy(
                        bytes.as_ptr() as *const std::ffi::c_char,
                        bytes.len(),
                        buffer_name.as_ptr(),
                    ),
                )
            };
            let module = match LlvmModule::parse_bitcode_from_buffer(&buffer, ctx) {
                Ok(module) => module,
                Err(e) => cgbail!(SpanId::NONE, "Unit {i} bitcode failed to parse: {}", e),
            };
            match &merged {
                None => merged = Some(module),
                Some(base) => {
                    if let Err(e) = base.link_in_module(module) {
                        cgbail!(SpanId::NONE, "Merging unit {i}: {}", e);
                    }
                }
            }
        }
        let merged = merged.expect("at least one unit");
        merged.set_name(name);
        for g in merged.get_globals() {
            if !g.is_declaration() && g.get_visibility() == inkwell::GlobalVisibility::Hidden {
                g.set_linkage(LlvmLinkage::Internal);
                g.set_visibility(inkwell::GlobalVisibility::Default);
            }
        }
        for f in merged.get_functions() {
            let g = f.as_global_value();
            if !g.is_declaration() && g.get_visibility() == inkwell::GlobalVisibility::Hidden {
                f.set_linkage(LlvmLinkage::Internal);
                g.set_visibility(inkwell::GlobalVisibility::Default);
            }
        }
        Ok(merged)
    }

    pub fn thinlto_codegen(
        k1: &TypedProgram,
        artifacts: &[UnitArtifact],
        object_paths: &[String],
    ) -> CgResult<()> {
        let start = std::time::Instant::now();
        let mut units: Vec<K1ThinLtoUnit> = Vec::with_capacity(artifacts.len());
        let mut preserved: Vec<std::ffi::CString> = vec![];
        let mut cross_referenced: Vec<std::ffi::CString> = vec![];
        let symbol_prefix =
            if k1.config.target.platform() == compiler::Platform::PosixMacos { "_" } else { "" };
        for artifact in artifacts {
            let UnitArtifact::Bitcode { bytes, exported, referenced } = artifact else {
                panic!("thinlto_codegen on an object artifact")
            };
            units.push(K1ThinLtoUnit { data: bytes.as_ptr(), len: bytes.len() });
            for name in exported {
                preserved.push(std::ffi::CString::new(format!("{symbol_prefix}{name}")).unwrap());
            }
            for name in referenced {
                cross_referenced
                    .push(std::ffi::CString::new(format!("{symbol_prefix}{name}")).unwrap());
            }
        }
        let mut preserved_ptrs: Vec<*const std::ffi::c_char> = Vec::with_capacity(preserved.len());
        for p in &preserved {
            preserved_ptrs.push(p.as_ptr());
        }
        let mut cross_ptrs: Vec<*const std::ffi::c_char> =
            Vec::with_capacity(cross_referenced.len());
        for p in &cross_referenced {
            cross_ptrs.push(p.as_ptr());
        }
        let (cpu, features) = Cg::target_cpu_features(k1.config.target);
        let cpu = std::ffi::CString::new(cpu).unwrap();
        let features = std::ffi::CString::new(features).unwrap();
        let pic = k1.config.target.arch() != compiler::Arch::Wasm;
        let cache_dir = if k1.config.cache {
            let dir = format!("{}/thinlto", k1.ast.idents.get_string(k1.config.cache_dir));
            if let Err(e) = std::fs::create_dir_all(&dir) {
                cgbail!(SpanId::NONE, "Failed to create ThinLTO cache dir {dir}: {e}");
            }
            dir
        } else {
            String::new()
        };
        let cache_dir = std::ffi::CString::new(cache_dir).unwrap();
        let mut sink = ThinLtoSink { paths: object_paths, error: None };
        let code = unsafe {
            k1_thinlto_codegen(
                units.as_ptr(),
                units.len(),
                cpu.as_ptr(),
                features.as_ptr(),
                pic as i32,
                preserved_ptrs.as_ptr(),
                preserved_ptrs.len(),
                cross_ptrs.as_ptr(),
                cross_ptrs.len(),
                cache_dir.as_ptr(),
                thinlto_write_object,
                (&mut sink) as *mut ThinLtoSink as *mut std::ffi::c_void,
            )
        };
        if let Some(e) = sink.error {
            cgbail!(SpanId::NONE, "{e}");
        }
        if code != 0 {
            cgbail!(SpanId::NONE, "ThinLTO failed with code {code}");
        }
        if k1.config.chatty {
            eprintln!("thinlto of {} units took {}ms", units.len(), start.elapsed().as_millis());
        }
        Ok(())
    }

    /// Emits a constant per reloadable namespace which holds information about what symbols it
    /// contains; the reloader k1 code iterates this descriptor. Also holds signature hash for
    /// mismatch detection
    fn emit_reload_descriptors(&mut self) {
        enum Slot {
            Fn(FunctionId),
            Global(TypedGlobalId),
        }
        let mut reload_nss: Vec<NamespaceId> = vec![];
        for ns_id in self.k1.namespaces.namespaces.iter_ids() {
            if self.k1.namespaces.get(ns_id).reload {
                reload_nss.push(ns_id);
            }
        }

        let program_name = self.k1.program_name().to_string();
        let dylib_ext = self.k1.config.target.platform().dylib_ext();
        let ptr_type = self.builtin_types.ptr;
        let entry_type = self.ctx.struct_type(&[ptr_type.into(), ptr_type.into()], false);
        for ns_id in reload_nss {
            let mut entries: Vec<(String, Slot)> = vec![];
            for (function_id, function) in self.k1.function_iter() {
                if function.is_reloadable() && function.namespace_id == ns_id {
                    entries.push((
                        self.make_reloadable_function_symbol(function_id),
                        Slot::Fn(function_id),
                    ));
                }
            }
            for global_id in self.k1.globals.iter_ids() {
                if self.k1.globals.get(global_id).reload_ns == Some(ns_id) {
                    entries.push((
                        self.make_reloadable_global_symbol(global_id),
                        Slot::Global(global_id),
                    ));
                }
            }
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            let api_hash = self.k1.reload_hash_for_ns(ns_id);
            let ns = self.k1.namespaces.get(ns_id);
            let ns_name = self.k1.ident_str(ns.name).to_string();
            let mut ns_path = String::with_capacity(64);
            self.k1.write_scope_path(&mut ns_path, ns.scope_id, "/", true);

            let file_name_ptr =
                self.cstring_constant(&format!("{program_name}.{ns_name}.{dylib_ext}"));
            let mut entry_values = Vec::with_capacity(entries.len());
            for (symbol, slot) in &entries {
                let symbol_ptr = self.cstring_constant(symbol);
                let slot_global = match slot {
                    Slot::Fn(id) => self.reload_fn_addr_global(*id),
                    Slot::Global(id) => self.codegen_reload_global_addr_slot(*id),
                };

                // an array of { symbol: cstr, addr slot the loader patches: ptr }.
                entry_values.push(entry_type.const_named_struct(&[
                    symbol_ptr.into(),
                    slot_global.as_pointer_value().into(),
                ]));
            }
            let entries_array = entry_type.const_array(&entry_values);
            let entries_global =
                self.llvm_module.add_global(entries_array.get_type(), None, "reload_entries");
            entries_global.set_initializer(&entries_array);
            entries_global.set_constant(true);
            entries_global.set_linkage(LlvmLinkage::Private);

            // The loader bumps this after each successful swap; loaded-version reads it
            let version_global =
                self.llvm_module.add_global(self.ctx.i64_type(), None, "reload_version");
            version_global.set_initializer(&self.ctx.i64_type().const_zero());
            version_global.set_alignment(8);
            version_global.set_linkage(LlvmLinkage::Private);

            // { dylib file name: cstr, api hash: u64, version: *mut u64, entry count: u64, entries: ptr }
            let descriptor = self.ctx.const_struct(
                &[
                    file_name_ptr.into(),
                    self.ctx.i64_type().const_int(api_hash, false).into(),
                    version_global.as_pointer_value().into(),
                    self.ctx.i64_type().const_int(entries.len() as u64, false).into(),
                    entries_global.as_pointer_value().into(),
                ],
                false,
            );
            let descriptor_global = self.llvm_module.add_global(
                descriptor.get_type(),
                None,
                &format!("__k1_reload_ns_{ns_path}"),
            );
            descriptor_global.set_initializer(&descriptor);
            descriptor_global.set_constant(true);
            descriptor_global.set_linkage(LlvmLinkage::External);
        }
    }

    fn make_reloadable_function_symbol(&self, function_id: FunctionId) -> String {
        let function = self.k1.get_function(function_id);
        Cg::mangle(self.k1.make_qualified_name(function.scope, function.name, None, "/", true))
    }

    fn is_reloadable_function(&self, function_id: FunctionId) -> bool {
        let function = self.k1.get_function(function_id);
        if !function.is_reloadable() {
            return false;
        }
        match self.kind {
            CgKind::Host => true,
            CgKind::ReloadDylib(ns_id) => function.namespace_id != ns_id,
        }
    }

    /// A reloadable fn's current address: the `ptr` global the loader patches.
    /// The host defines it (null until the ns is loaded); dylibs declare it
    /// and bind to the host's copy at dlopen.
    fn reload_fn_addr_global(&mut self, function_id: FunctionId) -> GlobalValue<'ctx> {
        let name =
            format!("__k1_reload_fn_addr_{}", self.make_reloadable_function_symbol(function_id));
        if let Some(existing) = self.llvm_module.get_global(&name) {
            return existing;
        }
        let global = self.llvm_module.add_global(self.builtin_types.ptr, None, &name);
        global.set_alignment(8);
        global.set_linkage(LlvmLinkage::External);
        if self.kind == CgKind::Host && self.is_first_unit() {
            global.set_initializer(&self.builtin_types.ptr.const_null());
        }
        global
    }

    /// Same naming rules as reloadable_function_symbol: fns and globals share
    /// a scope's namespace, so plain qualified names cannot collide
    fn make_reloadable_global_symbol(&self, global_id: TypedGlobalId) -> String {
        let variable = self.k1.variables.get(self.k1.globals.get(global_id).variable_id);
        Cg::mangle(self.k1.make_qualified_name(
            variable.owner_scope,
            variable.name,
            None,
            "/",
            true,
        ))
    }

    /// A reloadable global's current storage: the `ptr` slot the loader
    /// patches to the newest dylib's copy. Host defines it (null until
    /// loaded); dylibs bind to the host's copy at dlopen.
    fn codegen_reload_global_addr_slot(&mut self, global_id: TypedGlobalId) -> GlobalValue<'ctx> {
        let name =
            format!("__k1_reload_global_addr_{}", self.make_reloadable_global_symbol(global_id));
        if let Some(existing) = self.llvm_module.get_global(&name) {
            return existing;
        }
        let global = self.llvm_module.add_global(self.builtin_types.ptr, None, &name);
        global.set_alignment(8);
        global.set_linkage(LlvmLinkage::External);
        if self.kind == CgKind::Host && self.is_first_unit() {
            global.set_initializer(&self.builtin_types.ptr.const_null());
        }
        global
    }

    fn crash_unloaded_fn_value(&mut self) -> CgResult<FunctionValue<'ctx>> {
        let Some(not_loaded_fn_id) =
            self.k1.ast.idents.lookup("crash-unloaded-ns").and_then(|ident| {
                self.k1.scopes.find_function_local(self.k1.scopes.k1_scope_id, ident)
            })
        else {
            cgbail!(SpanId::NONE, "core is missing fn k1/crash-unloaded-ns");
        };
        self.declare_llvm_function(not_loaded_fn_id)
    }

    /// `ptr __k1_reload_global_load(slot, ns-name, global-name)`: acquire-load
    /// the slot, crash-unloaded-ns on null, else return the storage address.
    /// One private copy per module; every cross-unit global access calls it,
    /// so each access observes the newest swap.
    fn reload_global_load_helper(&mut self) -> CgResult<FunctionValue<'ctx>> {
        if let Some(f) = self.llvm_module.get_function("__k1_reload_global_load") {
            return Ok(f);
        }
        let crash_fn = self.crash_unloaded_fn_value()?;
        let ptr_type = self.builtin_types.ptr;
        let fn_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
        let f = self.llvm_module.add_function(
            "__k1_reload_global_load",
            fn_type,
            Some(LlvmLinkage::Private),
        );
        let saved_block = self.builder.get_insert_block();
        let saved_loc = self.builder.get_current_debug_location();
        self.builder.unset_current_debug_location();

        let entry_block = self.ctx.append_basic_block(f, "entry");
        self.builder.position_at_end(entry_block);
        let slot = f.get_nth_param(0).unwrap().into_pointer_value();
        let loaded = self.builder.build_load(ptr_type, slot, "addr").unwrap();
        let load_instr = loaded.as_instruction_value().unwrap();
        load_instr.set_atomic_ordering(AtomicOrdering::Acquire).unwrap();
        load_instr.set_alignment(8).unwrap();
        let addr = loaded.into_pointer_value();

        let unloaded_block = self.ctx.append_basic_block(f, "unloaded");
        let ok_block = self.ctx.append_basic_block(f, "ok");
        let is_null = self.builder.build_is_null(addr, "").unwrap();
        self.builder.build_conditional_branch(is_null, unloaded_block, ok_block).unwrap();

        self.builder.position_at_end(unloaded_block);
        let ns_name_param = f.get_nth_param(1).unwrap();
        let global_name_param = f.get_nth_param(2).unwrap();
        self.builder
            .build_call(crash_fn, &[ns_name_param.into(), global_name_param.into()], "")
            .unwrap()
            .set_call_convention(crash_fn.get_call_conventions());
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(ok_block);
        self.builder.build_return(Some(&addr)).unwrap();

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        if let Some(loc) = saved_loc {
            self.builder.set_current_debug_location(loc);
        }
        Ok(f)
    }

    fn codegen_reload_global_addr(
        &mut self,
        global_id: TypedGlobalId,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        let slot = self.codegen_reload_global_addr_slot(global_id);
        let helper = self.reload_global_load_helper()?;
        let global = self.k1.globals.get(global_id);
        let global_name =
            self.k1.ident_str(self.k1.variables.get(global.variable_id).name).to_string();
        let ns_name =
            self.k1.ident_str(self.k1.namespaces.get(global.reload_ns.unwrap()).name).to_string();
        let ns_cstr = self.cstring_constant(&ns_name);
        let name_cstr = self.cstring_constant(&global_name);
        let call = self
            .builder
            .build_call(
                helper,
                &[slot.as_pointer_value().into(), ns_cstr.into(), name_cstr.into()],
                "reload_global",
            )
            .unwrap();
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => Ok(v),
            ValueKind::Instruction(_) => unreachable!("__k1_reload_global_load returns ptr"),
        }
    }

    fn cstring_constant(&mut self, text: &str) -> PointerValue<'ctx> {
        let value = self.ctx.const_string(text.as_bytes(), true);
        let global = self.llvm_module.add_global(value.get_type(), None, "cstr");
        global.set_initializer(&value);
        global.set_constant(true);
        global.set_unnamed_addr(true);
        global.set_linkage(LlvmLinkage::Private);
        global.as_pointer_value()
    }

    /// Acquire-load the fn's current address and tail-call it; on null call
    /// k1/crash-unloaded-ns, which crashes with the ns/fn names
    fn codegen_reload_stub(&mut self, function_id: FunctionId) -> CgResult<()> {
        let cg_fn = self.llvm_functions.get(&function_id).unwrap();
        let function_value = cg_fn.function_value;
        let llvm_fn_type = cg_fn.function_type.llvm_function_type;
        let is_sret = cg_fn.function_type.is_sret;
        let return_cg_type = cg_fn.function_type.return_logical_cg_type;
        let param_abi_mappings = cg_fn.function_type.param_abi_mappings;
        let param_k1_types = cg_fn.function_type.param_k1_types;
        let di_subprogram = cg_fn.debug_info;
        let di_file = cg_fn.debug_file;

        let typed_fn = self.k1.get_function(function_id);
        let fn_name = self.k1.ident_str(typed_fn.name).to_string();
        let ns_name =
            self.k1.ident_str(self.k1.namespaces.get(typed_fn.namespace_id).name).to_string();
        let function_span = self.k1.ast.get_span_for_id(typed_fn.parsed_id);

        let fn_addr_global = self.reload_fn_addr_global(function_id);
        self.debug.push_scope(function_span, di_subprogram.as_debug_info_scope(), di_file);
        self.set_debug_location_from_span(function_span);
        let entry_block = self.ctx.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);
        let loaded = self.builder.build_load(
            self.builtin_types.ptr,
            fn_addr_global.as_pointer_value(),
            "impl_ptr",
        );
        let loaded = loaded.unwrap();
        let load_instr = loaded.as_instruction_value().unwrap();
        load_instr.set_atomic_ordering(AtomicOrdering::Acquire).unwrap();
        load_instr.set_alignment(8).unwrap();
        let impl_ptr = loaded.into_pointer_value();

        let unloaded_block = self.ctx.append_basic_block(function_value, "unloaded");
        let call_block = self.ctx.append_basic_block(function_value, "call_impl");
        let is_null = self.builder.build_is_null(impl_ptr, "").unwrap();
        self.builder.build_conditional_branch(is_null, unloaded_block, call_block).unwrap();

        self.builder.position_at_end(unloaded_block);
        let not_loaded_fv = self.crash_unloaded_fn_value()?;
        let ns_cstr = self.cstring_constant(&ns_name);
        let fn_cstr = self.cstring_constant(&fn_name);
        self.builder
            .build_call(not_loaded_fv, &[ns_cstr.into(), fn_cstr.into()], "")
            .unwrap()
            .set_call_convention(not_loaded_fv.get_call_conventions());
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(call_block);
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> =
            Vec::with_capacity(function_value.count_params() as usize);
        for param in function_value.get_param_iter() {
            args.push(param.into());
        }
        let call = self.builder.build_indirect_call(llvm_fn_type, impl_ptr, &args, "").unwrap();
        call.set_tail_call(true);
        call.add_attribute(AttributeLoc::Function, self.make_enum_attribute("nounwind", 0));
        if is_sret {
            for attr in self.make_sret_attributes(&return_cg_type) {
                call.add_attribute(AttributeLoc::Param(0), attr);
            }
        }
        let param_offset = if is_sret { 1 } else { 0 };
        for i in 0..param_abi_mappings.len() as usize {
            let abi_mapping = *self.mem.get_nth_lt(param_abi_mappings, i);
            if matches!(abi_mapping, AbiParamMapping::BigStructByPtrToCopy { byval_attr: true }) {
                let k1_type = *self.mem.get_nth_lt(param_k1_types, i);
                for attr in self.make_byval_attributes(&k1_type) {
                    call.add_attribute(AttributeLoc::Param((i + param_offset) as u32), attr);
                }
            }
        }
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => self.builder.build_return(Some(&v)).unwrap(),
            ValueKind::Instruction(_) => self.builder.build_return(None).unwrap(),
        };
        self.debug.pop_scope();
        Ok(())
    }

    fn codegen_global(&mut self, global_id: TypedGlobalId) -> CgResult<GlobalValue<'ctx>> {
        if let Some(g) = self.globals.get(&global_id) {
            return Ok(*g);
        }
        let global = self.k1.globals.get(global_id).clone();

        let name = self.k1.global_link_symbol(&global);

        if let Some(reload_ns) = global.reload_ns {
            if self.kind != CgKind::ReloadDylib(reload_ns) {
                cgbail!(
                    global.span,
                    "ICE: reload-ns global reached codegen_global outside its dylib; \
                     accesses must go through the addr slot"
                );
            }
            let initial_static_value_id = global.initial_value.as_value().unwrap();
            let initializer_basic_value =
                self.codegen_static_value_as_const(initial_static_value_id, 0)?;
            let layout = self.k1.get_layout_computed(global.type_id).unwrap();
            let symbol = self.make_reloadable_global_symbol(global_id);
            let llvm_global = self.make_global_from_value(
                initializer_basic_value,
                layout.align,
                &symbol,
                global.is_constant,
                LlvmLinkage::External,
                false,
            );
            self.globals.insert(global_id, llvm_global);
            return Ok(llvm_global);
        }

        let is_dylib = matches!(self.kind, CgKind::ReloadDylib(_));
        let is_private = !global.is_exported && !self.has_reloadable_fns;
        let shared = is_private && global.is_constant;
        let defined_elsewhere = !shared && self.multi_unit() && !self.is_first_unit();

        // If we're a reloadable dylib, all globals get treated like externals usually do
        // we link to them and expect to find them in the host
        let llvm_global = if global.is_external || is_dylib || defined_elsewhere {
            let PhysicalTypeResult::Yes(global_pt) =
                self.k1.get_physical_type_computed(global.type_id)
            else {
                cgbail!(global.span, "ICE: Not physical; reject this in typer");
            };
            let basic_type = self.codegen_type(global_pt);
            let g = self.make_external_global(basic_type.rich_type(), &name, global.is_constant);
            if global.is_tls && self.target_supports_tls() {
                g.set_thread_local(true);
                let mode = if defined_elsewhere && self.k1.program_settings.executable {
                    ThreadLocalMode::LocalExecTLSModel
                } else {
                    ThreadLocalMode::GeneralDynamicTLSModel
                };
                g.set_thread_local_mode(Some(mode));
            }
            if is_private && !global.is_external {
                g.set_visibility(inkwell::GlobalVisibility::Hidden);
            }
            g
        } else {
            let initial_static_value_id = global.initial_value.as_value().unwrap();
            let initializer_basic_value =
                self.codegen_static_value_as_const(initial_static_value_id, 0)?;

            // With reloadable nses in play, every global is visible to dlopened dylibs
            let llvm_linkage = if shared {
                LlvmLinkage::LinkOnceODR
            } else if !is_private || self.multi_unit() {
                LlvmLinkage::External
            } else {
                LlvmLinkage::Private
            };
            let layout = self.k1.get_layout_computed(global.type_id).unwrap();
            let g = self.make_global_from_value(
                initializer_basic_value,
                layout.align,
                &name,
                global.is_constant,
                llvm_linkage,
                global.is_tls,
            );
            if is_private && llvm_linkage != LlvmLinkage::Private {
                g.set_visibility(inkwell::GlobalVisibility::Hidden);
            }
            g
        };

        self.globals.insert(global_id, llvm_global);
        Ok(llvm_global)
    }

    #[allow(unused)]
    fn layout_per_llvm(&self, typ: &dyn AnyType) -> Layout {
        let td = self.llvm_machine.get_target_data();
        llvm_size_info(&td, typ)
    }

    fn padding_type(&self, size_bytes: u32) -> ArrayType<'ctx> {
        self.ctx.i8_type().array_type(size_bytes)
    }

    fn get_debug_location_from_span(&self, span: SpanId) -> DILocation<'ctx> {
        let span = self.k1.ast.spans.get(span);
        let line = self
            .k1
            .ast
            .sources
            .get_line_for_span_start(&self.k1.ast.mem, span)
            .expect("No line for span");
        let column = span.start + 1 - line.start_char;
        let locn = self.debug.debug_builder.create_debug_location(
            self.ctx,
            line.line_index + 1,
            column,
            self.debug.current_scope(),
            None,
        );
        locn
    }
    fn set_debug_location_from_span(&self, span: SpanId) {
        if span.is_none() {
            return;
        }
        if let Some((last_span, _)) = self.last_debug_location.get()
            && last_span == span
        {
            return;
        }
        let locn = self.get_debug_location_from_span(span);
        self.last_debug_location.set(Some((span, locn)));
        self.builder.set_current_debug_location(locn);
    }

    #[allow(unused)]
    fn set_debug_location(&self, locn: DILocation<'ctx>) {
        self.builder.set_current_debug_location(locn)
    }

    fn get_debug_location(&self) -> DILocation<'ctx> {
        self.builder.get_current_debug_location().unwrap()
    }

    fn get_line_number(&self, span: SpanId) -> u32 {
        let span = self.k1.ast.spans.get(span);
        let line = self
            .k1
            .ast
            .sources
            .get_line_for_span_start(&self.k1.ast.mem, span)
            .expect("No line for span");
        line.line_index + 1
    }

    // FIXME: slow dumb bad mangle function
    fn mangle(name: String) -> String {
        name.replace("[", "_of_")
            .replace("]", "_")
            .replace(",", "_")
            .replace("*", "ref")
            .replace(" ", "_")
    }

    fn write_type_name(
        &self,
        w: &mut impl std::fmt::Write,
        type_id: TypeId,
        defn_info: Option<TypeDefnInfo>,
    ) {
        // FIXME: Using this as typename is bad; consider a really big struct.
        let name = self.k1.type_id_to_string(type_id);
        let name = Cg::mangle(name);

        match defn_info {
            None => write!(w, "{}", name).unwrap(),
            Some(info) => self.k1.write_qualified_name(w, info.scope, &name, None, ".", true),
        };
    }

    fn codegen_type_name(&self, type_id: TypeId) -> String {
        let defn_info = self.k1.get_defn_info(type_id);
        let mut s = String::with_capacity(64);
        self.write_type_name(&mut s, type_id, defn_info);
        s
    }

    const DW_ATE_ADDRESS: u32 = 0x01;
    const DW_ATE_BOOLEAN: u32 = 0x02;
    const _DW_ATE_COMPLEX_FLOAT: u32 = 0x03;
    const DW_ATE_FLOAT: u32 = 0x04;
    const DW_ATE_SIGNED: u32 = 0x05;
    const _DW_ATE_CHAR: u32 = 0x06;
    const DW_ATE_UNSIGNED: u32 = 0x07;
    const DW_ATE_UNSIGNED_CHAR: u32 = 0x08;

    fn make_scalar_type(
        ctx: &'ctx Context,
        debug: &DebugContext<'ctx>,
        ptr: PointerType<'ctx>,
        st: ScalarType,
    ) -> LlvmScalarType<'ctx> {
        let layout = st.get_layout();
        let (name, encoding): (&'static str, u32) = match st {
            ScalarType::U8 => ("u8", Self::DW_ATE_UNSIGNED),
            ScalarType::U16 => ("u16", Self::DW_ATE_UNSIGNED),
            ScalarType::U32 => ("u32", Self::DW_ATE_UNSIGNED),
            ScalarType::U64 => ("u64", Self::DW_ATE_UNSIGNED),
            ScalarType::I8 => ("i8", Self::DW_ATE_SIGNED),
            ScalarType::I16 => ("i16", Self::DW_ATE_SIGNED),
            ScalarType::I32 => ("i32", Self::DW_ATE_SIGNED),
            ScalarType::I64 => ("i64", Self::DW_ATE_SIGNED),
            ScalarType::F32 => ("f32", Self::DW_ATE_FLOAT),
            ScalarType::F64 => ("f64", Self::DW_ATE_FLOAT),
            ScalarType::Pointer => ("ptr", Self::DW_ATE_ADDRESS),
            ScalarType::Char => ("char", Self::DW_ATE_UNSIGNED_CHAR),
            ScalarType::Bool => ("bool", Self::DW_ATE_BOOLEAN),
        };
        let basic_type: BasicTypeEnum<'ctx> = match st {
            ScalarType::U8 | ScalarType::Char | ScalarType::Bool | ScalarType::I8 => {
                ctx.i8_type().into()
            }
            ScalarType::U16 | ScalarType::I16 => ctx.i16_type().into(),
            ScalarType::U32 | ScalarType::I32 => ctx.i32_type().into(),
            ScalarType::U64 | ScalarType::I64 => ctx.i64_type().into(),
            ScalarType::F32 => ctx.f32_type().into(),
            ScalarType::F64 => ctx.f64_type().into(),
            ScalarType::Pointer => ptr.into(),
        };
        let di_type = debug
            .debug_builder
            .create_basic_type(name, layout.size_bits() as u64, encoding, 0)
            .unwrap()
            .as_type();
        LlvmScalarType { pt: PhysicalType::scalar(st), basic_type, layout, di_type }
    }

    fn codegen_type(&mut self, pt: PhysicalType) -> CgType<'ctx> {
        //eprintln!("codegen_type {}", self.k1.pt_to_string(pt));
        match pt.as_enum() {
            PhysicalTypeEnum::Scalar(st) => {
                self.builtin_types.scalars[st.to_tag() as usize - 1].into()
            }
            PhysicalTypeEnum::Agg(agg_id) => {
                if let Some(k1) = self.llvm_types.get(&agg_id) {
                    return *k1;
                }
                let agg = self.k1.agg_types.get(agg_id);
                let agg_layout = agg.layout;
                let type_name = self.codegen_type_name(agg.origin_type_id);
                let cg_type = match agg.agg_type {
                    AggType::Struct { fields, packed } => {
                        let mut cg_field_types = self.mem.new_list(fields.len());
                        let mut field_rich_types = self.tmp.new_list(fields.len());
                        let mut field_di_types = self.tmp.new_list(fields.len());

                        let span = self.debug.current_span();
                        let line_number = self.get_line_number(span);
                        for phys_field in self.k1.mem.getn(fields) {
                            let cg_type = self.codegen_type(phys_field.field_t);
                            let field_name = self.k1.ident_str(phys_field.name);
                            cg_field_types.push(cg_type);
                            field_rich_types.push(cg_type.rich_type());
                            let debug_member = self
                                .debug
                                .debug_builder
                                .create_member_type(
                                    self.debug.current_scope(),
                                    field_name,
                                    self.debug.current_file(),
                                    line_number,
                                    cg_type.rich_repr_layout().size_bits() as u64,
                                    cg_type.rich_repr_layout().align_bits(),
                                    phys_field.offset as u64,
                                    0,
                                    cg_type.debug_type(),
                                )
                                .as_type();
                            field_di_types.push(debug_member)
                        }
                        let struct_type = self.ctx.struct_type(&field_rich_types, packed);
                        if cfg!(debug_assertions) {
                            let td = self.llvm_machine.get_target_data();
                            for (i, phys_field) in self.k1.mem.getn(fields).iter().enumerate() {
                                let llvm_offset =
                                    td.offset_of_element(&struct_type, i as u32).unwrap();
                                assert_eq!(
                                    llvm_offset, phys_field.offset as u64,
                                    "K1/LLVM field offset mismatch in {}, field {}",
                                    type_name, i
                                );
                            }
                            let llvm_layout = self.layout_per_llvm(&struct_type);
                            assert_eq!(
                                llvm_layout.size,
                                agg_layout.stride(),
                                "K1/LLVM struct size mismatch in {}",
                                type_name
                            );
                        }
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_struct_type(
                                self.debug.current_scope(),
                                &type_name,
                                self.debug.current_file(),
                                line_number,
                                agg_layout.size as u64,
                                agg_layout.align,
                                0,
                                None,
                                &field_di_types,
                                0,
                                None,
                                &type_name,
                            )
                            .as_type();
                        CgType::StructType(CgStructType {
                            pt,
                            struct_type,
                            fields: cg_field_types.to_slice(),
                            di_type,
                            layout: agg_layout,
                        })
                    }
                    AggType::Array { element_pt, len } => {
                        let element_type = self.codegen_type(element_pt);
                        let array_type = element_type.rich_type().array_type(len);
                        let array_layout = self.k1.get_pt_layout(pt);
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_array_type(
                                element_type.debug_type(),
                                array_layout.size_bits() as u64,
                                array_layout.align_bits(),
                                &[],
                            )
                            .as_type();
                        CgType::ArrayType(CgArrayType {
                            pt,
                            count: len,
                            array_type,
                            element_type: self.mem.push_h(element_type),
                            di_type,
                            layout: array_layout,
                        })
                    }
                    AggType::Vector { element_pt, len } => {
                        let element_type = self.codegen_type(PhysicalType::scalar(element_pt));
                        let vector_type = match element_type.rich_type() {
                            BasicTypeEnum::IntType(it) => it.vec_type(len),
                            BasicTypeEnum::FloatType(ft) => ft.vec_type(len),
                            other => panic!("Non-scalar vector element type: {other}"),
                        };
                        let vector_layout = self.k1.get_pt_layout(pt);
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_array_type(
                                element_type.debug_type(),
                                vector_layout.size_bits() as u64,
                                vector_layout.align_bits(),
                                &[],
                            )
                            .as_type();
                        CgType::Vector(CgVectorType {
                            pt,
                            count: len,
                            vector_type,
                            element_type: self.mem.push_h(element_type),
                            di_type,
                            layout: vector_layout,
                        })
                    }
                    AggType::Union { members } => {
                        let mut cg_members = self.mem.new_list(members.len());
                        let mut di_members = self.tmp.new_list(members.len());
                        let mut basic_type_members = self.tmp.new_list(members.len());
                        for m in self.k1.mem.getn(members) {
                            let cg_member = self.codegen_type(m.ty);
                            basic_type_members.push(cg_member.rich_type());
                            cg_members.push(cg_member);
                            di_members.push(cg_member.debug_type());
                        }
                        let span = self.debug.current_span();
                        let line_number = self.get_line_number(span);
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_union_type(
                                self.debug.current_scope(),
                                &type_name,
                                self.debug.current_file(),
                                line_number,
                                agg_layout.size_bits() as u64,
                                agg_layout.align_bits(),
                                0,
                                &di_members,
                                0,
                                &type_name,
                            )
                            .as_type();
                        let aligned_opaque_repr = self.codegen_opaque_repr(agg_layout);
                        CgType::Union(CgUnionType {
                            pt,
                            aligned_opaque_repr,
                            members: cg_members.to_slice(),
                            layout: agg_layout,
                            di_type,
                        })
                    }
                    AggType::Sum(e) => {
                        let struct_repr_cg_type =
                            self.codegen_type(PhysicalType::agg(e.struct_repr));

                        struct_repr_cg_type
                    }
                    AggType::Opaque { size, align } => {
                        let layout = Layout { size, align };
                        let aligned_opaque_repr = self.codegen_opaque_repr(layout);

                        let span = self.debug.current_span();
                        let line_number = self.get_line_number(span);
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_union_type(
                                self.debug.current_scope(),
                                &type_name,
                                self.debug.current_file(),
                                line_number,
                                agg_layout.size_bits() as u64,
                                agg_layout.align_bits(),
                                0,
                                &[],
                                0,
                                &type_name,
                            )
                            .as_type();

                        // For now, we'll call this a 'Union', its just our own type anyway,
                        // arguably it shouldn't even be a Sum since they share so much
                        CgType::Union(CgUnionType {
                            pt,
                            aligned_opaque_repr,
                            members: MSlice::empty(),
                            layout,
                            di_type,
                        })
                    }
                };
                self.llvm_types.insert(agg_id, cg_type);
                cg_type
            }
            PhysicalTypeEnum::Empty => {
                let struct_type = self.ctx.struct_type(&[], false);
                let di_type = self
                    .debug
                    .debug_builder
                    .create_struct_type(
                        self.debug.current_scope(),
                        "empty",
                        self.debug.current_file(),
                        1,
                        0,
                        1,
                        0,
                        None,
                        &[],
                        0,
                        None,
                        "empty",
                    )
                    .as_type();
                CgType::StructType(CgStructType {
                    pt,
                    struct_type,
                    fields: MSlice::empty(),
                    di_type,
                    layout: Layout::ZERO_SIZED,
                })
            }
        }
    }

    fn codegen_opaque_repr(&self, expected_layout: Layout) -> StructType<'ctx> {
        // For union types, we generate a 2-field struct to trick LLVM.

        // Field 1 is a synthetic integer wide enough to force the alignment of the
        // struct, and Field 2 is an array of bytes, ensuring NO padding at all,
        // large enough to get the whole thing to be exactly `size`, with no end padding.
        // This is mostly what clang does for unions, modulo maybe some cleverness for simple cases
        // and maybe Class detection (for possible float reg passing?)

        if expected_layout.align > expected_layout.size && expected_layout.size != 0 {
            panic!(
                "Cannot create overaligned union with align {} > size {}",
                expected_layout.align, expected_layout.size
            );
        }
        let aligner_type = self
            .ctx
            .custom_width_int_type(NonZeroU32::new(expected_layout.align_bits()).unwrap())
            .unwrap();
        let padding_bytes = expected_layout.size.saturating_sub(aligner_type.get_bit_width() / 8);

        let padding = self.padding_type(padding_bytes);
        let aligned_struct_repr = if padding_bytes == 0 {
            self.ctx.struct_type(&[aligner_type.as_basic_type_enum()], false)
        } else {
            self.ctx.struct_type(
                &[aligner_type.as_basic_type_enum(), padding.as_basic_type_enum()],
                false,
            )
        };

        let llvm_layout = self.layout_per_llvm(&aligned_struct_repr);
        if expected_layout.strided() != llvm_layout {
            eprintln!("UNION LAYOUT MISMATCH: us {} vs llvm {}", expected_layout, llvm_layout);
        }
        aligned_struct_repr
    }

    fn scalar_basic_type(&self, st: ScalarType) -> BasicTypeEnum<'ctx> {
        self.builtin_types.scalars[st.to_tag() as usize - 1].basic_type
    }

    fn pt_canon_type(&self, pt: PhysicalType) -> BasicTypeEnum<'ctx> {
        match pt.as_enum() {
            PhysicalTypeEnum::Empty => self.builtin_types.empty_struct.as_basic_type_enum(),
            PhysicalTypeEnum::Scalar(st) => self.scalar_basic_type(st),
            PhysicalTypeEnum::Agg(_) => self.builtin_types.ptr.as_basic_type_enum(),
        }
    }

    fn make_cg_function_type(
        &mut self,
        phys_fn_type: &PhysicalFunctionType,
    ) -> CgResult<CgFunctionType<'ctx>> {
        let param_types = phys_fn_type.params;
        let return_type = phys_fn_type.return_type;
        let _diverges = phys_fn_type.diverges;
        let return_logical_cg_type = self.codegen_type(phys_fn_type.return_type);
        let return_type_abi_mapping = self.get_abi_mapping_for_type(return_type, true);

        // If a function returns a big (typically > 2 words) struct, its actually
        // 'returned' in the first parameter, which is a pointer
        let is_sret = match return_type_abi_mapping {
            AbiParamMapping::VoidReturnEmpty => false,
            AbiParamMapping::ScalarInRegister => false,
            AbiParamMapping::StructInInteger { .. } => false,
            AbiParamMapping::StructAsPointer => false,
            AbiParamMapping::StructInSse { .. } => false,
            AbiParamMapping::StructByEightbytePair { .. } => false,
            AbiParamMapping::StructByHfa { .. } => false,
            AbiParamMapping::StructByIntPairArray => false,
            AbiParamMapping::BigStructByPtrToCopy { .. } => true,
        };

        let physical_return_mapped_type = if is_sret {
            None
        } else {
            self.mapped_abi_type_return(phys_fn_type.return_type, return_type_abi_mapping)
        };

        let param_count = param_types.len();

        // The logical parameters closest to K1 model
        let mut param_llvm_types: List<CgType<'ctx>, _> = self.mem.new_list(param_count);
        // Foreach k1 param above, describe how to map it to LLVM params
        let mut param_abi_mappings: List<AbiParamMapping, _> = self.mem.new_list(param_count);

        // The physical LLVM params; the ones the function will have.
        // For now this is 1:1 in count with the logical params, as I choose to pass the int pairs
        // in a struct, but it need not be; that is, 1 k1 param could result in n llvm params,
        // where n could even be 0 for a ZST or uninhabited type
        let mut function_final_params: List<BasicMetadataTypeEnum<'ctx>, _> =
            self.mem.new_list(param_count + is_sret as u32);

        if is_sret {
            function_final_params.push(self.builtin_types.ptr.into())
        }

        for param in self.k1.ir.mem.getn(param_types) {
            let param_cg_type = self.codegen_type(param.pt);
            let abi_mapping = self.get_abi_mapping_for_type(param.pt, false);
            param_abi_mappings.push(abi_mapping);
            param_llvm_types.push(param_cg_type);
            let mapped_type = self.mapped_abi_type_param(param.pt, abi_mapping);
            function_final_params.push(mapped_type.into());

            //eprintln!(
            //    "abi mapping for {} is {:?}. Mapped type: {:?}",
            //    param_cg_type.rich_type(),
            //    abi_mapping,
            //    mapped_type
            //);
        }

        let fn_type = match physical_return_mapped_type {
            None => self.ctx.void_type().fn_type(&function_final_params, false),
            Some(rt) => rt.fn_type(&function_final_params, false),
        };

        Ok(CgFunctionType {
            llvm_function_type: fn_type,
            param_k1_types: param_llvm_types.to_slice(),
            param_abi_mappings: param_abi_mappings.to_slice(),
            return_logical_cg_type,
            return_abi_mapping: return_type_abi_mapping,
            is_sret,
        })
    }

    fn get_float_type(&self, scalar_type: ScalarType) -> FloatType<'ctx> {
        match scalar_type {
            ScalarType::F32 => self.ctx.f32_type(),
            ScalarType::F64 => self.ctx.f64_type(),
            other => panic!("Expected a float scalar type, got {other:?}"),
        }
    }

    fn mapped_abi_type_return(
        &mut self,
        pt: PhysicalType,
        abi_mapping: AbiParamMapping,
    ) -> Option<BasicTypeEnum<'ctx>> {
        if pt.is_empty() { None } else { Some(self.mapped_abi_type_param(pt, abi_mapping)) }
    }
    fn mapped_abi_type_param(
        &mut self,
        pt: PhysicalType,
        abi_mapping: AbiParamMapping,
    ) -> BasicTypeEnum<'ctx> {
        match abi_mapping {
            AbiParamMapping::VoidReturnEmpty => {
                unreachable!("VoidReturn does not map to a BasicType")
            }
            AbiParamMapping::ScalarInRegister => self.pt_canon_type(pt),
            AbiParamMapping::StructInInteger { abi_width, .. } => {
                let int_type = self
                    .ctx
                    .custom_width_int_type(NonZeroU32::new(abi_width).unwrap())
                    .unwrap()
                    .as_basic_type_enum();
                int_type
            }
            AbiParamMapping::StructAsPointer => self.builtin_types.ptr.as_basic_type_enum(),
            AbiParamMapping::StructInSse { element, count } => {
                let element_type = self.get_float_type(element);
                if count == 1 {
                    element_type.as_basic_type_enum()
                } else {
                    element_type.vec_type(count).as_basic_type_enum()
                }
            }
            AbiParamMapping::StructByEightbytePair { class1, class2, active_bits2 } => {
                // We know field 1 is a full 8 bits
                let f1 = match class1 {
                    RegisterClass::Initial => panic!("Got Initial EightbyteClass"),
                    RegisterClass::Int => self.ctx.i64_type().as_basic_type_enum(),
                    RegisterClass::Float => self.ctx.f64_type().as_basic_type_enum(),
                    RegisterClass::Ptr => self.builtin_types.ptr.as_basic_type_enum(),
                };
                let f2 = match (class2, active_bits2) {
                    (RegisterClass::Initial, _) => panic!("Got Initial EightbyteClass"),
                    (RegisterClass::Ptr, _) => self.builtin_types.ptr.as_basic_type_enum(),
                    (RegisterClass::Int, bits) => {
                        if bits <= 8 {
                            self.ctx.i8_type().as_basic_type_enum()
                        } else if bits <= 16 {
                            self.ctx.i16_type().as_basic_type_enum()
                        } else if bits <= 32 {
                            self.ctx.i32_type().as_basic_type_enum()
                        } else {
                            self.ctx.i64_type().as_basic_type_enum()
                        }
                    }
                    (RegisterClass::Float, bits) => {
                        if bits <= 32 {
                            self.ctx.f32_type().as_basic_type_enum()
                        } else {
                            self.ctx.f64_type().as_basic_type_enum()
                        }
                    }
                };
                let struct_type = self.ctx.struct_type(&[f1, f2], false).as_basic_type_enum();
                struct_type
            }
            AbiParamMapping::StructByHfa { element, count } => {
                let element_type = self.get_float_type(element).as_basic_type_enum();
                let mut fields = self.tmp.new_list(count);
                for _ in 0..count {
                    fields.push(element_type);
                }
                self.ctx.struct_type(&fields, false).as_basic_type_enum()
            }
            AbiParamMapping::StructByIntPairArray => {
                let array_type = self.ctx.i64_type().array_type(2).as_basic_type_enum();
                array_type
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } => {
                let ptr_type = self.builtin_types.ptr.as_basic_type_enum();
                ptr_type
            }
        }
    }

    /// Takes a value either passed to or returned from a function and converts it back
    /// to how the compiler expects that type to be represented
    fn canonicalize_abi_param_value(
        &mut self,
        mapping: AbiParamMapping,
        cg_ty: &CgType<'ctx>,
        abi_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        debug!("canonicalizing {} to {} via {:?}", abi_value, cg_ty.rich_type(), mapping);
        match mapping {
            AbiParamMapping::VoidReturnEmpty => {
                unreachable!("we should not have a BasicValue for VoidReturnEmpty")
            }
            AbiParamMapping::ScalarInRegister => abi_value,
            AbiParamMapping::StructInInteger { abi_width: _, active_width } => {
                // abi_width is the type of the integer value in abi_value
                // active_width is the size of the struct we are unpacking it into
                let truncated = self
                    .builder
                    .build_int_truncate(
                        abi_value.into_int_value(),
                        self.ctx
                            .custom_width_int_type(NonZeroU32::new(active_width).unwrap())
                            .unwrap(),
                        "",
                    )
                    .unwrap();
                let ptr = self.build_k1_alloca(cg_ty, "struct_in_integer_storage");
                self.store_at_k1_align(ptr, truncated, cg_ty);
                ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructAsPointer => {
                let ptr = self.build_k1_alloca(cg_ty, "struct_as_ptr_storage");
                self.store_at_k1_align(ptr, abi_value, cg_ty);
                ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructInSse { .. } => {
                let ptr = self.build_k1_alloca(cg_ty, "struct_in_sse_storage");
                self.store_at_k1_align(ptr, abi_value, cg_ty);
                ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructByEightbytePair { .. }
            | AbiParamMapping::StructByIntPairArray => {
                // The ABI value's fields can extend past the struct's own size (a
                // 12-byte struct arrives as [2 x i64], an 11-byte one as {i64, i32}),
                // so spill into a slot sized for the ABI type, not the struct; the
                // struct occupies its leading bytes
                let abi_ty = abi_value.get_type();
                let dst_ptr = self.build_alloca(abi_ty, "abi_pair_storage");
                let abi_align = self.llvm_machine.get_target_data().get_abi_alignment(&abi_ty);
                let align = abi_align.max(cg_ty.rich_repr_layout().align);
                dst_ptr.as_instruction().unwrap().set_alignment(align).unwrap();
                if self.k1.config.filc && self.pt_has_pointer_in_union(cg_ty.pt()) {
                    self.build_zhas_union_marker(dst_ptr);
                }
                self.builder.build_store(dst_ptr, abi_value).unwrap();
                dst_ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructByHfa { .. } => {
                let dst_ptr = self.build_k1_alloca(cg_ty, "struct_by_hfa_storage");
                debug_assert!(abi_value.get_type().is_struct_type());
                self.store_at_k1_align(dst_ptr, abi_value, cg_ty);
                dst_ptr.as_basic_value_enum()
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } => {
                // Our canonical representation of all aggregates is an llvm ptr
                // And this abi route represents them as a ptr, so nothing to do
                abi_value
            }
        }
    }

    fn marshal_abi_return_value(
        &mut self,
        mapping: AbiParamMapping,
        cg_ty: &CgType<'ctx>,
        k1_value: BasicValueEnum<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match mapping {
            AbiParamMapping::VoidReturnEmpty | AbiParamMapping::BigStructByPtrToCopy { .. } => None,
            _ => {
                let value = self.marshal_abi_param_value(mapping, cg_ty, k1_value, true);
                Some(value)
            }
        }
    }

    /// Takes a canonical k1 value to pass to or return from a function and converts it
    /// to the ABI format
    fn marshal_abi_param_value(
        &mut self,
        mapping: AbiParamMapping,
        cg_ty: &CgType<'ctx>,
        k1_value: BasicValueEnum<'ctx>,
        is_return: bool,
    ) -> BasicValueEnum<'ctx> {
        let pt = cg_ty.pt();
        debug!("marshalling k1 {}: {} with {:?}", k1_value, self.k1.pt_to_string(pt), mapping);
        match mapping {
            AbiParamMapping::VoidReturnEmpty => panic!("VoidReturnEmpty should be handled"),
            AbiParamMapping::ScalarInRegister => k1_value,
            AbiParamMapping::StructInInteger { active_width, .. } => {
                let abi_type = self.mapped_abi_type_param(pt, mapping);
                let dst_int_type = abi_type.into_int_type();
                // The ABI int can be a non-power-of-two width (i24 for a
                // 3-byte struct, i40/i48/i56 for 5-7); bit_width/8 is then
                // not a legal alignment. Use the type's ABI alignment, which
                // is also exactly what the alloca below gets.
                let td = self.llvm_machine.get_target_data();
                let dst_int_align = td.get_abi_alignment(&dst_int_type);
                let dst_int_size = td.get_abi_size(&dst_int_type);
                let integer_ptr = self.build_alloca(dst_int_type, "abi_struct_int");
                self.emit_lifetime_marker(true, integer_ptr, dst_int_size);
                self.builder.build_store(integer_ptr, dst_int_type.const_zero()).unwrap();

                // %1 = alloca %struct.Small2, align 1
                // %2 = alloca i64, align 8
                // call void @llvm.memset.p0.i64(ptr align 1 %1, i8 0, i64 6, i1 false)
                // ..
                // call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 1 %1, i64 6, i1 false)
                // %3 = load i64, ptr %2, align 8
                // call void @takes_small(i64 %3)

                let src_layout = self.k1.get_pt_layout(pt);
                self.builder
                    .build_memcpy(
                        integer_ptr,
                        dst_int_align,
                        k1_value.into_pointer_value(),
                        src_layout.align,
                        self.builtin_types.ptr_sized_int.const_int(active_width as u64 / 8, false),
                    )
                    .unwrap();
                let integer_value = self.builder.build_load(abi_type, integer_ptr, "").unwrap();
                self.emit_lifetime_marker(false, integer_ptr, dst_int_size);
                integer_value
            }
            AbiParamMapping::StructAsPointer => {
                self.load_at_k1_align(self.builtin_types.ptr, k1_value.into_pointer_value(), cg_ty)
            }
            AbiParamMapping::StructInSse { .. } => {
                let abi_type = self.mapped_abi_type_param(pt, mapping);
                self.load_at_k1_align(abi_type, k1_value.into_pointer_value(), cg_ty)
            }
            AbiParamMapping::StructByEightbytePair { .. }
            | AbiParamMapping::StructByIntPairArray => {
                // The ABI type's fields can extend past the struct's own size (a
                // 12-byte struct becomes [2 x i64], an 11-byte one {i64, i32});
                // loading it straight off the struct would read out of bounds, so
                // undersized or under-aligned structs stage through a zeroed
                // ABI-sized slot (clang's pattern, as in StructInInteger above)
                let abi_type = self.mapped_abi_type_param(pt, mapping);
                let src_layout = self.k1.get_pt_layout(pt);
                let td = self.llvm_machine.get_target_data();
                let abi_store_size = td.get_store_size(&abi_type);
                let abi_align = td.get_abi_alignment(&abi_type);
                let src_ptr = k1_value.into_pointer_value();
                if abi_store_size <= src_layout.size as u64 && src_layout.align >= abi_align {
                    self.builder.build_load(abi_type, src_ptr, "").unwrap()
                } else {
                    let tmp = self.build_alloca(abi_type, "abi_pair_tmp");
                    let abi_alloc_size = td.get_abi_size(&abi_type);
                    self.emit_lifetime_marker(true, tmp, abi_alloc_size);
                    self.builder
                        .build_memset(
                            tmp,
                            abi_align,
                            self.ctx.i8_type().const_zero(),
                            self.builtin_types.ptr_sized_int.const_int(abi_alloc_size, false),
                        )
                        .unwrap();
                    self.builder
                        .build_memcpy(
                            tmp,
                            abi_align,
                            src_ptr,
                            src_layout.align,
                            self.builtin_types
                                .ptr_sized_int
                                .const_int(src_layout.size as u64, false),
                        )
                        .unwrap();
                    let abi_value = self.builder.build_load(abi_type, tmp, "").unwrap();
                    self.emit_lifetime_marker(false, tmp, abi_alloc_size);
                    abi_value
                }
            }
            AbiParamMapping::StructByHfa { .. } => {
                let abi_type = self.mapped_abi_type_param(pt, mapping);
                self.load_at_k1_align(abi_type, k1_value.into_pointer_value(), cg_ty)
            }
            AbiParamMapping::BigStructByPtrToCopy { byval_attr } => {
                // Our canonical representation of all aggregates is an llvm ptr
                // And this abi route represents them as a ptr, already.
                //
                // But, if this is truly the 'value' getting passed to the C code, then
                // this would allow mutation incorrectly, and we do have to make a copy
                // So, if this is not a return, but a param marshal, we'd make a copy
                if is_return {
                    // For returns its moot, the ptr is already a caller-owned slot
                    k1_value
                } else {
                    if byval_attr {
                        // We don't need to do an extra copy because byval lowering will do it
                        k1_value
                    } else {
                        // Just a pointer parameter; we take responsibility for making the copy.
                        // The callee may only read it during the call, so the caller
                        // (codegen_function_call) ends its lifetime after the callsite
                        let callers_copy = self.build_k1_alloca(cg_ty, "abi_caller_copy");
                        self.emit_lifetime_marker(
                            true,
                            callers_copy,
                            cg_ty.rich_repr_layout().size as u64,
                        );
                        self.memcpy_k1_value(callers_copy, k1_value.into_pointer_value(), cg_ty);
                        callers_copy.as_basic_value_enum()
                    }
                }
            }
        }
    }

    /// Loads `llvm_type` from `ptr` at the k1 type's alignment, which can be
    /// lower than `llvm_type`'s natural alignment (ABI marshalling, packed structs)
    fn load_at_k1_align(
        &self,
        llvm_type: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        cg_ty: &CgType<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let load = self.builder.build_load(llvm_type, ptr, "").unwrap();
        load.as_instruction_value().unwrap().set_alignment(cg_ty.rich_repr_layout().align).unwrap();
        load
    }

    fn store_at_k1_align(
        &self,
        ptr: PointerValue<'ctx>,
        value: impl BasicValue<'ctx>,
        cg_ty: &CgType<'ctx>,
    ) -> InstructionValue<'ctx> {
        let store = self.builder.build_store(ptr, value).unwrap();
        store.set_alignment(cg_ty.rich_repr_layout().align).unwrap();
        store
    }

    /// Handles the storage of aggregates by doing a (hopefully) correct memcpy
    fn store_k1_value(
        &self,
        cg_type: &CgType<'ctx>,
        dst: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> InstructionValue<'ctx> {
        if cg_type.is_aggregate() {
            self.memcpy_k1_value(dst, value.into_pointer_value(), cg_type)
        } else {
            self.store_at_k1_align(dst, value, cg_type)
        }
    }

    fn build_float_to_int_saturating(
        &self,
        input: FloatValue<'ctx>,
        from: ScalarType,
        to: ScalarType,
        signed: bool,
    ) -> IntValue<'ctx> {
        let to_int_type = self.scalar_basic_type(to).into_int_type();
        let op = if signed { "fptosi" } else { "fptoui" };
        let name = format!("llvm.{}.sat.i{}.f{}", op, to.width().bits(), from.width().bits());
        let function = match self.llvm_module.get_function(&name) {
            Some(f) => f,
            None => {
                let fn_type = to_int_type.fn_type(&[self.scalar_basic_type(from).into()], false);
                self.llvm_module.add_function(&name, fn_type, None)
            }
        };
        self.builder
            .build_call(function, &[input.into()], "")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value()
    }

    fn emit_lifetime_marker(&self, start: bool, ptr: PointerValue<'ctx>, size_bytes: u64) {
        let name = if start { "llvm.lifetime.start.p0" } else { "llvm.lifetime.end.p0" };
        let function = match self.llvm_module.get_function(name) {
            Some(f) => f,
            None => {
                let fn_type = self
                    .ctx
                    .void_type()
                    .fn_type(&[self.ctx.i64_type().into(), self.builtin_types.ptr.into()], false);
                self.llvm_module.add_function(name, fn_type, None)
            }
        };
        let size = self.ctx.i64_type().const_int(size_bytes, false);
        self.builder.build_call(function, &[size.into(), ptr.into()], "").unwrap();
    }

    fn memcpy_layout(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        layout: Layout,
    ) -> InstructionValue<'ctx> {
        let bytes = self.builtin_types.ptr_sized_int.const_int(layout.size as u64, false);
        let align_bytes = layout.align;
        self.builder
            .build_memcpy(dst, align_bytes, src, align_bytes, bytes)
            .unwrap()
            .as_instruction_value()
            .unwrap()
    }

    fn memcpy_k1_value(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        ty: &CgType<'ctx>,
    ) -> InstructionValue<'ctx> {
        let layout = ty.rich_repr_layout();
        self.memcpy_layout(dst, src, layout)
    }

    fn get_current_function(&self) -> &CgFunction<'ctx> {
        let codegened_function = self.llvm_functions.get(&self.current_insert_function).unwrap();
        codegened_function
    }

    fn get_current_function_mut(&mut self) -> &mut CgFunction<'ctx> {
        let codegened_function =
            self.llvm_functions.get_mut(&self.current_insert_function).unwrap();
        codegened_function
    }

    fn get_llvm_block(&self, block_id: BlockId) -> CgResult<BasicBlock<'ctx>> {
        // We skip our 'prelude' block which exists only in the llvm ir
        match self.get_current_function().blocks.get(&block_id) {
            Some(bb) => Ok(*bb),
            None => Err(cgerr!(
                self.debug.current_span(),
                "Failed to get block: b{}",
                block_id.raw_index()
            )),
        }
    }

    fn get_insert_function_mut(&mut self) -> &mut CgFunction<'ctx> {
        let codegened_function =
            self.llvm_functions.get_mut(&self.current_insert_function).unwrap();
        codegened_function
    }

    fn build_struct_gep(
        &mut self,
        ptr: PointerValue<'ctx>,
        struct_type: StructType<'ctx>,
        idx: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder.build_struct_gep(struct_type, ptr, idx, name).unwrap()
    }

    fn append_basic_block(&mut self, name: &str) -> BasicBlock<'ctx> {
        let origin_block = self.builder.get_insert_block().unwrap();
        let current_fn = origin_block.get_parent().unwrap();
        let block = self.ctx.append_basic_block(current_fn, name);
        block
    }

    fn llvm_atomic_ordering(ord: ir::AtomicOrderingIr) -> AtomicOrdering {
        match ord {
            ir::AtomicOrderingIr::Relaxed => AtomicOrdering::Monotonic,
            ir::AtomicOrderingIr::Acquire => AtomicOrdering::Acquire,
            ir::AtomicOrderingIr::Release => AtomicOrdering::Release,
            ir::AtomicOrderingIr::AcqRel => AtomicOrdering::AcquireRelease,
            ir::AtomicOrderingIr::SeqCst => AtomicOrdering::SequentiallyConsistent,
        }
    }

    fn bool_to_i1(&self, bool: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_truncate(bool, self.builtin_types.i1, name).unwrap()
    }

    fn i1_to_bool(&self, i1: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_cast_sign_flag(i1, self.builtin_types.boolean, false, name).unwrap()
    }

    fn build_k1_alloca(&mut self, ty: &CgType<'ctx>, name: &str) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(ty.rich_type(), name);
        ptr.as_instruction().unwrap().set_alignment(ty.rich_repr_layout().align).unwrap();
        if self.k1.config.filc && self.pt_has_pointer_in_union(ty.pt()) {
            self.build_zhas_union_marker(ptr);
        }
        ptr
    }

    /// Fil-C's pizlonator tracks pointer capabilities per alloca based on the
    /// pointer operations it can see; a union payload holding pointers is only
    /// visible through untyped copies, so it must be marked with the fork's
    /// `zhas_union` intrinsic (as Fil-C's clang does for union-typed locals)
    /// or capabilities are dropped in transit.
    fn build_zhas_union_marker(&mut self, alloca: PointerValue<'ctx>) {
        let zhas_union = self.llvm_module.get_function("zhas_union").unwrap_or_else(|| {
            let fn_type = self.ctx.void_type().fn_type(&[self.builtin_types.ptr.into()], false);
            self.llvm_module.add_function("zhas_union", fn_type, Some(LlvmLinkage::External))
        });
        self.builder.build_call(zhas_union, &[alloca.into()], "").unwrap();
    }

    fn pt_has_pointer_in_union(&self, pt: PhysicalType) -> bool {
        fn pt_contains_pointer(c: &Cg, pt: PhysicalType) -> bool {
            match pt.as_enum() {
                PhysicalTypeEnum::Empty => false,
                PhysicalTypeEnum::Scalar(st) => matches!(st, ScalarType::Pointer),
                PhysicalTypeEnum::Agg(agg_id) => match c.k1.agg_types.get(agg_id).agg_type {
                    AggType::Struct { fields, .. } => {
                        c.k1.mem.getn(fields).iter().any(|f| pt_contains_pointer(c, f.field_t))
                    }
                    AggType::Array { element_pt, .. } => pt_contains_pointer(c, element_pt),
                    AggType::Vector { .. } => false,
                    AggType::Union { members } => {
                        c.k1.mem.getn(members).iter().any(|m| pt_contains_pointer(c, m.ty))
                    }
                    AggType::Sum(e) => pt_contains_pointer(c, PhysicalType::agg(e.struct_repr)),
                    AggType::Opaque { .. } => false,
                },
            }
        }
        fn walk(c: &Cg, pt: PhysicalType) -> bool {
            match pt.as_enum() {
                PhysicalTypeEnum::Empty | PhysicalTypeEnum::Scalar(_) => false,
                PhysicalTypeEnum::Agg(agg_id) => match c.k1.agg_types.get(agg_id).agg_type {
                    AggType::Struct { fields, .. } => {
                        c.k1.mem.getn(fields).iter().any(|f| walk(c, f.field_t))
                    }
                    AggType::Array { element_pt, .. } => walk(c, element_pt),
                    AggType::Vector { .. } => false,
                    AggType::Union { members } => {
                        c.k1.mem.getn(members).iter().any(|m| pt_contains_pointer(c, m.ty))
                    }
                    AggType::Sum(e) => walk(c, PhysicalType::agg(e.struct_repr)),
                    AggType::Opaque { .. } => false,
                },
            }
        }
        walk(self, pt)
    }

    /// Inserts an alloca in the entry block of the function
    fn build_alloca<T: BasicType<'ctx>>(&mut self, ty: T, name: &str) -> PointerValue<'ctx> {
        let original_block = self.builder.get_insert_block().unwrap();
        let f = self.get_current_function();
        let function_entry_block = f.function_value.get_first_basic_block().unwrap();

        let debug_locn = self.builder.get_current_debug_location();
        match f.last_alloca_instr {
            None => match function_entry_block.get_first_instruction() {
                Some(instr) => {
                    self.builder.position_at(function_entry_block, &instr);
                }
                None => {
                    self.builder.position_at_end(function_entry_block);
                }
            },
            Some(last_alloca) => {
                self.builder.position_at(function_entry_block, &last_alloca);
            }
        };

        let alloca = self.builder.build_alloca(ty, name).unwrap();
        alloca.as_instruction().unwrap().set_debug_location(None);
        self.get_insert_function_mut().last_alloca_instr = Some(alloca.as_instruction().unwrap());

        self.builder.position_at_end(original_block);
        self.restore_debug_location(debug_locn);
        alloca
    }

    fn restore_debug_location(&self, locn: Option<DILocation<'ctx>>) {
        match locn {
            Some(locn) => self.builder.set_current_debug_location(locn),
            None => self.builder.unset_current_debug_location(),
        }
    }

    fn codegen_function_call(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        call_id: ir::IrCallId,
        span: SpanId,
    ) -> CgResult<Option<BasicValueEnum<'ctx>>> {
        let call = self.k1.ir.calls.get(call_id);
        let callee = call.callee;
        let call_args = call.args;
        let call_dst = call.dst;

        enum CallKind<'ctx> {
            Direct(FunctionId),
            Indirect(PointerValue<'ctx>),
        }
        let (llvm_callee, cg_fn_type) = match callee {
            IrCallee::BackendBuiltin(function_id, _)
            | IrCallee::Direct(function_id)
            | IrCallee::Extern { function_id, .. } => {
                self.declare_llvm_function(function_id)?;
                let fn_type = self.llvm_functions.get(&function_id).unwrap().function_type;
                (CallKind::Direct(function_id), fn_type)
            }
            IrCallee::LlvmIntrinsic { name, function_id } => {
                return self.codegen_llvm_intrinsic_call(
                    inst_mappings,
                    name,
                    function_id,
                    call_args,
                    call_dst,
                    span,
                );
            }
            IrCallee::Indirect(fn_type, value) => {
                let callee_value = self.resolve_value(inst_mappings, value)?.into_pointer_value();
                let cg_fn_type = self.make_cg_function_type(&fn_type)?;
                (CallKind::Indirect(callee_value), cg_fn_type)
            }
        };

        let mut args: List<BasicMetadataValueEnum<'ctx>, _> =
            self.mem.new_list(cg_fn_type.llvm_function_type.count_param_types());

        let sret_dst = if cg_fn_type.is_sret {
            match call_dst {
                Some(dst) => {
                    let dst_value = self.resolve_value(inst_mappings, dst)?;
                    Some(dst_value.into_pointer_value())
                }
                None => {
                    let alloca =
                        self.build_k1_alloca(&cg_fn_type.return_logical_cg_type, "sret_storage\0");
                    Some(alloca)
                }
            }
        } else {
            None
        };

        if let Some(sret_dst) = sret_dst {
            args.push(sret_dst.into())
        }

        let mut byval_args = self.tmp.new_list(0);
        let mut caller_copies = self.tmp.new_list(0);
        for (index, arg_ir_value) in self.k1.ir.mem.getn(call_args).iter().enumerate() {
            let arg_value = self.resolve_value(inst_mappings, *arg_ir_value)?;

            let param_k1_ty = *self.mem.get_nth_lt(cg_fn_type.param_k1_types, index);
            let abi_mapping = *self.mem.get_nth_lt(cg_fn_type.param_abi_mappings, index);
            let value_marshalled =
                self.marshal_abi_param_value(abi_mapping, &param_k1_ty, arg_value, false);
            trace!("codegen function call arg type: {}", value_marshalled);
            match abi_mapping {
                AbiParamMapping::BigStructByPtrToCopy { byval_attr: true } => {
                    byval_args.push_grow(&mut self.tmp, (args.len(), param_k1_ty));
                }
                // marshal made a fresh caller-owned copy; the callee may only
                // read it during the call, so it dies at the callsite
                AbiParamMapping::BigStructByPtrToCopy { byval_attr: false } => {
                    caller_copies.push_grow(
                        &mut self.tmp,
                        (
                            value_marshalled.into_pointer_value(),
                            param_k1_ty.rich_repr_layout().size as u64,
                        ),
                    );
                }
                _ => {}
            };
            args.push(value_marshalled.into())
        }

        let callsite_value = match llvm_callee {
            CallKind::Direct(function_id) => {
                let function_value = self.declare_llvm_function(function_id)?;
                self.set_debug_location_from_span(span);

                let call = self.builder.build_call(function_value, &args, "").unwrap();
                call.set_call_convention(function_value.get_call_conventions());
                call
            }
            CallKind::Indirect(fn_ptr) => {
                self.set_debug_location_from_span(span);
                let call_site_value = self
                    .builder
                    .build_indirect_call(cg_fn_type.llvm_function_type, fn_ptr, &args, "")
                    .unwrap();
                call_site_value
                    .add_attribute(AttributeLoc::Function, self.make_enum_attribute("nounwind", 0));
                call_site_value
            }
        };
        for (byval_param_index, param_k1_ty) in byval_args.as_slice() {
            for attr in self.make_byval_attributes(param_k1_ty) {
                callsite_value.add_attribute(AttributeLoc::Param(*byval_param_index as u32), attr);
            }
        }

        if cg_fn_type.is_sret {
            for attr in self.make_sret_attributes(&cg_fn_type.return_logical_cg_type) {
                callsite_value.add_attribute(AttributeLoc::Param(0), attr);
            }
        };
        for (copy_ptr, size) in caller_copies.as_slice() {
            self.emit_lifetime_marker(false, *copy_ptr, *size);
        }
        match callsite_value.try_as_basic_value() {
            ValueKind::Basic(returned_value) => {
                let canonical_value = self.canonicalize_abi_param_value(
                    cg_fn_type.return_abi_mapping,
                    &cg_fn_type.return_logical_cg_type,
                    returned_value,
                );
                match call_dst {
                    None => Ok(Some(canonical_value)),
                    Some(dst) => {
                        let dst_ptr = self.resolve_value(inst_mappings, dst)?;
                        self.store_k1_value(
                            &cg_fn_type.return_logical_cg_type,
                            dst_ptr.into_pointer_value(),
                            canonical_value,
                        );
                        Ok(Some(dst_ptr))
                    }
                }
            }
            ValueKind::Instruction(_instr) => {
                if cg_fn_type.is_sret {
                    Ok(Some(sret_dst.unwrap().as_basic_value_enum()))
                } else {
                    Ok(None)
                }
            }
        }
    }

    /// Named-intrinsic calls (`intern("llvm.*")`) speak LLVM's type language:
    /// bool <-> i1, vectors by value as <n x t>, other scalars as-is. The
    /// declared K1 signature is trusted; the LLVM verifier is the backstop.
    /// Constant bool args fold to constant i1, satisfying immarg parameters
    fn codegen_llvm_intrinsic_call(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        name: StringId,
        function_id: FunctionId,
        call_args: MSlice<ir::Value, ProgramIr>,
        call_dst: Option<ir::Value>,
        span: SpanId,
    ) -> CgResult<Option<BasicValueEnum<'ctx>>> {
        let fn_type_id = self.k1.get_function(function_id).type_id;
        let fn_type = *self.k1.types.get(fn_type_id).expect_function();

        let intrinsic_param_type =
            |c: &mut Self, type_id: TypeId| -> CgResult<BasicTypeEnum<'ctx>> {
                match c.k1.types.get(type_id) {
                    Type::Bool => Ok(c.builtin_types.i1.as_basic_type_enum()),
                    Type::Vector(vt) => {
                        let vt = *vt;
                        let pt = c.k1.get_physical_type_computed(type_id).unwrap().expect_agg();
                        let (elem, lanes) = c.k1.agg_types.get(pt).agg_type.expect_vector();
                        let _ = vt;
                        Ok(c.vec_llvm_type(elem, lanes).as_basic_type_enum())
                    }
                    Type::Char | Type::Integer(_) | Type::Float(_) | Type::Pointer => {
                        let pt = c.k1.get_physical_type_computed(type_id).unwrap();
                        Ok(c.codegen_type(pt).rich_type())
                    }
                    _ => Err(cgerr!(
                        span,
                        "llvm intrinsic signatures support bool, scalar, and vector types; got {}",
                        type_id
                    )),
                }
            };

        let params = self.k1.mem.getn(fn_type.physical_params);
        let mut param_types: SV8<BasicMetadataTypeEnum<'ctx>> = smallvec::smallvec![];
        for p in params.iter() {
            param_types.push(intrinsic_param_type(self, p.type_id)?.into());
        }
        let return_type_id = fn_type.return_type;
        let returns_empty = match self.k1.get_physical_type_computed(return_type_id) {
            PhysicalTypeResult::Yes(pt) => pt.is_empty(),
            _ => false,
        };
        let llvm_fn_type = if returns_empty {
            self.ctx.void_type().fn_type(&param_types, false)
        } else {
            match self.k1.types.get(return_type_id) {
                Type::Vector(_) => {
                    cgbail!(span, "llvm intrinsics with vector returns are not yet supported");
                }
                _ => intrinsic_param_type(self, return_type_id)?.fn_type(&param_types, false),
            }
        };

        let name_str = self.k1.ident_str(name).to_string();
        let function_value = match self.llvm_module.get_function(&name_str) {
            Some(f) => f,
            None => self.llvm_module.add_function(&name_str, llvm_fn_type, None),
        };

        let mut args: SV8<BasicMetadataValueEnum<'ctx>> = smallvec::smallvec![];
        for (index, arg_ir_value) in self.k1.ir.mem.getn(call_args).iter().enumerate() {
            let arg_value = self.resolve_value(inst_mappings, *arg_ir_value)?;
            let param_type_id = params[index].type_id;
            let marshalled: BasicValueEnum<'ctx> = match self.k1.types.get(param_type_id) {
                Type::Bool => self
                    .builder
                    .build_int_truncate(arg_value.into_int_value(), self.builtin_types.i1, "")
                    .unwrap()
                    .as_basic_value_enum(),
                Type::Vector(_) => {
                    // Vector args arrive as addresses; pass by value
                    let pt =
                        self.k1.get_physical_type_computed(param_type_id).unwrap().expect_agg();
                    let (elem, lanes) = self.k1.agg_types.get(pt).agg_type.expect_vector();
                    let vec_type = self.vec_llvm_type(elem, lanes);
                    let load = self
                        .builder
                        .build_load(vec_type, arg_value.into_pointer_value(), "")
                        .unwrap();
                    let align = elem.get_layout().stride() * lanes;
                    load.as_instruction_value().unwrap().set_alignment(align).unwrap();
                    load
                }
                _ => arg_value,
            };
            args.push(marshalled.into());
        }

        self.set_debug_location_from_span(span);
        let callsite = self.builder.build_call(function_value, &args, "").unwrap();
        match callsite.try_as_basic_value() {
            ValueKind::Basic(returned) => {
                let canonical = match self.k1.types.get(return_type_id) {
                    Type::Bool => {
                        self.i1_to_bool(returned.into_int_value(), "").as_basic_value_enum()
                    }
                    _ => returned,
                };
                match call_dst {
                    None => Ok(Some(canonical)),
                    Some(dst) => {
                        let dst_ptr = self.resolve_value(inst_mappings, dst)?;
                        self.builder.build_store(dst_ptr.into_pointer_value(), canonical).unwrap();
                        Ok(Some(dst_ptr))
                    }
                }
            }
            ValueKind::Instruction(_) => Ok(None),
        }
    }

    fn load_function_argument(
        &mut self,
        function_id: FunctionId,
        index: usize,
    ) -> BasicMetadataValueEnum<'ctx> {
        let cg_fn = self.llvm_functions.get(&function_id).unwrap();
        let basic_value = cg_fn.param_values[index];
        basic_value.into()
    }

    fn codegen_builtin_function_body(
        &mut self,
        builtin_type: BackendBuiltin,
        function_id: FunctionId,
    ) -> CgResult<InstructionValue<'ctx>> {
        let function = self.k1.get_function(function_id);
        let function_span =
            self.k1.ast.get_function(function.parsed_id.as_function_id().unwrap()).signature_span;
        self.set_debug_location_from_span(function_span);

        let instr = match builtin_type {
            BackendBuiltin::MemCopy | BackendBuiltin::MemMove => {
                // fn(intern) copy(
                //   dst: Pointer,
                //   src: Pointer,
                //   count: uword
                // ): empty
                let dst_ptr_arg = self.load_function_argument(function_id, 0).into_pointer_value();
                let src_ptr_arg = self.load_function_argument(function_id, 1).into_pointer_value();
                let size_arg = self.load_function_argument(function_id, 2).into_int_value();
                let dst_align_bytes = 1;
                let src_align_bytes = 1;
                if builtin_type == BackendBuiltin::MemMove {
                    self.builder
                        .build_memmove(
                            dst_ptr_arg,
                            dst_align_bytes,
                            src_ptr_arg,
                            src_align_bytes,
                            size_arg,
                        )
                        .unwrap();
                } else {
                    let _not_actually_a_ret_ptr = self
                        .builder
                        .build_memcpy(
                            dst_ptr_arg,
                            dst_align_bytes,
                            src_ptr_arg,
                            src_align_bytes,
                            size_arg,
                        )
                        .unwrap();
                }
                self.builder.build_return(None).unwrap()
            }
            BackendBuiltin::MemSet => {
                // fn(intern) set(dst: ptr, value: u8, count: int): unit
                let dst_arg = self.load_function_argument(function_id, 0);
                let value_arg = self.load_function_argument(function_id, 1);
                let count_arg = self.load_function_argument(function_id, 2);
                let _not_actually_a_ret_ptr = self
                    .builder
                    .build_memset(
                        dst_arg.into_pointer_value(),
                        1,
                        value_arg.into_int_value(),
                        count_arg.into_int_value(),
                    )
                    .unwrap();
                self.builder.build_return(None).unwrap()
            }
            BackendBuiltin::MemEquals => {
                // fn(intern) equals(p1: Pointer, p2: Pointer, size: uword): bool
                let p1_arg = self.load_function_argument(function_id, 0);
                let p2_arg = self.load_function_argument(function_id, 1);
                let size_arg = self.load_function_argument(function_id, 2);

                let memcmp_fv = match self.llvm_module.get_function("memcmp") {
                    Some(f) => f,
                    None => {
                        let fn_type = self.ctx.i32_type().fn_type(
                            &[
                                self.builtin_types.ptr.into(),
                                self.builtin_types.ptr.into(),
                                self.builtin_types.ptr_sized_int.into(),
                            ],
                            false,
                        );
                        self.llvm_module.add_function(
                            "memcmp",
                            fn_type,
                            Some(LlvmLinkage::External),
                        )
                    }
                };
                let call =
                    self.builder.build_call(memcmp_fv, &[p1_arg, p2_arg, size_arg], "").unwrap();
                let result =
                    call.try_as_basic_value().expect_basic("memcmp return").into_int_value();
                let is_zero = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, result, result.get_type().const_zero(), "")
                    .unwrap();
                let bool_equal = self.i1_to_bool(is_zero, "");
                self.builder.build_return(Some(&bool_equal)).unwrap()
            }
            BackendBuiltin::Exit => {
                // fn(intern) exit(code: i32): never
                self.builder.build_unreachable().unwrap()
            }

            BackendBuiltin::TypeInfo => {
                let cg_fn = self.llvm_functions.get(&function_id).unwrap();
                debug_assert!(cg_fn.function_type.is_sret);
                let out_storage =
                    cg_fn.function_value.get_param_iter().nth(0).unwrap().into_pointer_value();
                let type_id_ptr = self.load_function_argument(function_id, 0).into_pointer_value();
                let type_id_arg = self
                    .builder
                    .build_load(self.ctx.i64_type(), type_id_ptr, "")
                    .unwrap()
                    .into_int_value();
                let cg_fn = self.llvm_functions.get(&function_id).unwrap();
                let type_info_cg_type = cg_fn.function_type.return_logical_cg_type;
                let entry_block = self.builder.get_insert_block().unwrap();

                let else_block = self.append_basic_block("miss");
                self.builder.position_at_end(else_block);
                // TODO: Proper crash
                self.builder.build_unreachable().unwrap();

                let finish_block = self.append_basic_block("finish");

                let mut cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> =
                    Vec::with_capacity(self.k1.type_infos.len());

                for (type_id, info_value_id) in self
                    .k1
                    .type_infos
                    .iter()
                    .map(|(x, y)| (*x, *y))
                    .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
                {
                    if self.k1.get_type_variable_counts(type_id).is_abstract() {
                        // No point re-ifying types that don't exist at runtime
                        // like type parameters
                        continue;
                    }
                    let my_block =
                        self.append_basic_block(&format!("arm_type_{}", type_id.as_u32()));
                    self.builder.position_at_end(my_block);
                    let type_id_int_value =
                        self.ctx.i64_type().const_int(type_id.as_u32() as u64, false);

                    let value = self.codegen_static_value_canonical(info_value_id)?;

                    self.store_k1_value(&type_info_cg_type, out_storage, value);
                    self.builder.build_unconditional_branch(finish_block).unwrap();
                    cases.push((type_id_int_value, my_block));
                }
                self.builder.position_at_end(entry_block);
                let _switch = self.builder.build_switch(type_id_arg, else_block, &cases).unwrap();

                self.builder.position_at_end(finish_block);
                self.builder.build_return(None).unwrap()
            }
            BackendBuiltin::MakeStruct
            | BackendBuiltin::MakeEither
            | BackendBuiltin::MakeReference
            | BackendBuiltin::MakeArray
            | BackendBuiltin::MakeFn
            | BackendBuiltin::MakeInstance => self.builder.build_unreachable().unwrap(),
            BackendBuiltin::CompilerMessage => self.builder.build_return(None).unwrap(),
            BackendBuiltin::ReplCheckbox => self.builder.build_return(None).unwrap(),
        };
        Ok(instr)
    }

    fn codegen_block(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        block_id: BlockId,
    ) -> CgResult<BasicBlock<'ctx>> {
        let block = self.k1.ir.mem.get(block_id);
        let llvm_block = self.get_llvm_block(block_id)?;
        self.builder.position_at_end(llvm_block);
        for inst in self.k1.ir.mem.dlist_iter(block.data.instrs) {
            self.codegen_inst(inst_mappings, *inst)?;
        }
        Ok(llvm_block)
    }

    fn resolve_value(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        value: ir::Value,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        //eprintln!("codegen_value {}", value);
        match value {
            ir::Value::Inst(inst_id) => match inst_mappings.get(&inst_id) {
                Some(v) => Ok(*v),
                None => Err(cgerr!(
                    self.debug.current_span(),
                    "codegen llvm has no value for this instruction: i{} {}",
                    inst_id.as_u32(),
                    ir::inst_to_string(self.k1, inst_id)
                )),
            },
            ir::Value::GlobalAddr { id, .. } => {
                let reload_ns = self.k1.globals.get(id).reload_ns;
                if let Some(reload_ns) = reload_ns
                    && self.kind != CgKind::ReloadDylib(reload_ns)
                {
                    // Not our storage: the current version's copy, via the patched slot
                    return self.codegen_reload_global_addr(id);
                }
                let global_value = self.codegen_global(id)?;
                Ok(global_value.as_pointer_value().as_basic_value_enum())
            }
            ir::Value::StaticValue { id, .. } => self.codegen_static_value_canonical(id),
            ir::Value::FunctionAddr(function_id) => {
                debug_assert!(
                    self.k1.function_abi(function_id) == AbiMode::Native,
                    "address materialized for internal-abi fn {}",
                    self.k1.function_id_to_string(function_id, false)
                );
                let function_value = self.declare_llvm_function(function_id)?;
                Ok(function_value.as_global_value().as_pointer_value().into())
            }
            ir::Value::FnParam { index, .. } => {
                let function = self.get_current_function();
                let v = function.param_values[index as usize];
                Ok(v)
            }
            ir::Value::Data32 { t, data } => {
                let v: BasicValueEnum<'ctx> = match t {
                    ScalarType::U8 | ScalarType::Char | ScalarType::Bool => {
                        self.ctx.i8_type().const_int(data as u64, false).into()
                    }
                    ScalarType::U16 => self.ctx.i16_type().const_int(data as u64, false).into(),
                    ScalarType::U32 => self.ctx.i32_type().const_int(data as u64, false).into(),
                    ScalarType::U64 => self.ctx.i64_type().const_int(data as u64, false).into(),
                    ScalarType::I8 => {
                        self.ctx.i8_type().const_int(data as i32 as i64 as u64, true).into()
                    }
                    ScalarType::I16 => {
                        self.ctx.i16_type().const_int(data as i32 as i64 as u64, true).into()
                    }
                    ScalarType::I32 => {
                        self.ctx.i32_type().const_int(data as i32 as i64 as u64, true).into()
                    }
                    ScalarType::I64 => {
                        self.ctx.i64_type().const_int(data as i32 as i64 as u64, true).into()
                    }
                    ScalarType::F32 => {
                        self.ctx.f32_type().const_float(f32::from_bits(data) as f64).into()
                    }
                    ScalarType::F64 => {
                        self.ctx.f64_type().const_float(f32::from_bits(data) as f64).into()
                    }
                    ScalarType::Pointer => {
                        if data == 0 {
                            self.builtin_types.ptr.const_zero().into()
                        } else {
                            panic!("Got non-zero Pointer in codegen_llvm: {data}")
                        }
                    }
                };
                Ok(v)
            }
            ir::Value::IsStatic => Ok(self.builtin_types.false_value.into()),
            ir::Value::Empty => Ok(self.builtin_types.empty_struct_value()),
        }
    }

    fn vec_llvm_type(&mut self, elem: ScalarType, lanes: u32) -> LlvmVectorType<'ctx> {
        match self.codegen_type(PhysicalType::scalar(elem)).rich_type() {
            BasicTypeEnum::IntType(it) => it.vec_type(lanes),
            BasicTypeEnum::FloatType(ft) => ft.vec_type(lanes),
            other => panic!("Non-scalar vector element type: {other}"),
        }
    }

    /// Vector operands/results live in memory; loads and stores here use the
    /// vector's natural alignment, which every vector-typed place has
    fn vec_load(
        &mut self,
        vop: &ir::VecOpData,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        addr: ir::Value,
    ) -> CgResult<inkwell::values::VectorValue<'ctx>> {
        let ptr = self.resolve_value(inst_mappings, addr)?.into_pointer_value();
        let vec_type = self.vec_llvm_type(vop.elem, vop.lanes);
        let load = self.builder.build_load(vec_type, ptr, "").unwrap();
        load.as_instruction_value().unwrap().set_alignment(self.vec_align(vop)).unwrap();
        Ok(load.into_vector_value())
    }

    fn vec_store(
        &mut self,
        vop: &ir::VecOpData,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        value: inkwell::values::VectorValue<'ctx>,
    ) -> CgResult<()> {
        let dst_ptr = self.resolve_value(inst_mappings, vop.dst)?.into_pointer_value();
        let store = self.builder.build_store(dst_ptr, value).unwrap();
        store.set_alignment(self.vec_align(vop)).unwrap();
        Ok(())
    }

    fn vec_align(&self, vop: &ir::VecOpData) -> u32 {
        vop.elem.get_layout().stride() * vop.lanes
    }

    /// Returns the scalar result for value-producing ops (to-mask), None otherwise
    fn codegen_vec_op(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        vop: ir::VecOpData,
    ) -> CgResult<Option<BasicValueEnum<'ctx>>> {
        use ir::VecOpIr;
        let is_float = matches!(vop.elem, ScalarType::F32 | ScalarType::F64);
        match vop.op {
            VecOpIr::Splat => {
                let scalar = self.resolve_value(inst_mappings, vop.lhs)?;
                let vec_type = self.vec_llvm_type(vop.elem, vop.lanes);
                let i32t = self.ctx.i32_type();
                let undef = vec_type.get_undef();
                let v0 = self
                    .builder
                    .build_insert_element(undef, scalar, i32t.const_zero(), "")
                    .unwrap();
                let mask_zeroes: Vec<IntValue<'ctx>> = vec![i32t.const_zero(); vop.lanes as usize];
                let mask = LlvmVectorType::const_vector(&mask_zeroes);
                let splat = self.builder.build_shuffle_vector(v0, undef, mask, "").unwrap();
                self.vec_store(&vop, inst_mappings, splat)?;
                Ok(None)
            }
            VecOpIr::Add | VecOpIr::Sub | VecOpIr::Mul => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let r = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let result = if is_float {
                    let (l, r) = (l.as_basic_value_enum(), r.as_basic_value_enum());
                    let (l, r) = (l.into_vector_value(), r.into_vector_value());
                    match vop.op {
                        VecOpIr::Add => self.builder.build_float_add(l, r, "").unwrap(),
                        VecOpIr::Sub => self.builder.build_float_sub(l, r, "").unwrap(),
                        VecOpIr::Mul => self.builder.build_float_mul(l, r, "").unwrap(),
                        _ => unreachable!(),
                    }
                } else {
                    match vop.op {
                        VecOpIr::Add => self.builder.build_int_add(l, r, "").unwrap(),
                        VecOpIr::Sub => self.builder.build_int_sub(l, r, "").unwrap(),
                        VecOpIr::Mul => self.builder.build_int_mul(l, r, "").unwrap(),
                        _ => unreachable!(),
                    }
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::BitAnd | VecOpIr::BitOr | VecOpIr::Xor => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let r = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let result = match vop.op {
                    VecOpIr::BitAnd => self.builder.build_and(l, r, "").unwrap(),
                    VecOpIr::BitOr => self.builder.build_or(l, r, "").unwrap(),
                    VecOpIr::Xor => self.builder.build_xor(l, r, "").unwrap(),
                    _ => unreachable!(),
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::BitNot => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let result = self.builder.build_not(l, "").unwrap();
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::Shl | VecOpIr::Shr => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let count = self.resolve_value(inst_mappings, vop.rhs)?.into_int_value();
                let elem_int_type = l.get_type().get_element_type().into_int_type();
                let count = self.builder.build_int_cast(count, elem_int_type, "").unwrap();
                // Splat the uniform count across lanes
                let i32t = self.ctx.i32_type();
                let undef = l.get_type().get_undef();
                let c0 =
                    self.builder.build_insert_element(undef, count, i32t.const_zero(), "").unwrap();
                let mask_zeroes: Vec<IntValue<'ctx>> = vec![i32t.const_zero(); vop.lanes as usize];
                let counts = self
                    .builder
                    .build_shuffle_vector(c0, undef, LlvmVectorType::const_vector(&mask_zeroes), "")
                    .unwrap();
                let is_signed = matches!(
                    vop.elem,
                    ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64
                );
                let result = match vop.op {
                    VecOpIr::Shl => self.builder.build_left_shift(l, counts, "").unwrap(),
                    _ => self.builder.build_right_shift(l, counts, is_signed, "").unwrap(),
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::EqLanes => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let r = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let cmp_i1 = if is_float {
                    self.builder.build_float_compare(FloatPredicate::OEQ, l, r, "").unwrap()
                } else {
                    self.builder.build_int_compare(IntPredicate::EQ, l, r, "").unwrap()
                };
                let elem_bits = vop.elem.get_layout().size_bits();
                let int_lane_vec = self
                    .ctx
                    .custom_width_int_type(std::num::NonZeroU32::new(elem_bits).unwrap())
                    .unwrap()
                    .vec_type(vop.lanes);
                let mask_lanes = self.builder.build_int_s_extend(cmp_i1, int_lane_vec, "").unwrap();
                let result = if is_float {
                    let float_vec = self.vec_llvm_type(vop.elem, vop.lanes);
                    self.builder
                        .build_bit_cast(mask_lanes, float_vec, "")
                        .unwrap()
                        .into_vector_value()
                } else {
                    mask_lanes
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::ToMask => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let elem_bits = vop.elem.get_layout().size_bits();
                let int_lane_vec = self
                    .ctx
                    .custom_width_int_type(std::num::NonZeroU32::new(elem_bits).unwrap())
                    .unwrap()
                    .vec_type(vop.lanes);
                let as_ints = if is_float {
                    self.builder.build_bit_cast(l, int_lane_vec, "").unwrap().into_vector_value()
                } else {
                    l
                };
                let zero = int_lane_vec.const_zero();
                let msbs =
                    self.builder.build_int_compare(IntPredicate::SLT, as_ints, zero, "").unwrap();
                let mask_int_type = self
                    .ctx
                    .custom_width_int_type(std::num::NonZeroU32::new(vop.lanes).unwrap())
                    .unwrap();
                let mask_small =
                    self.builder.build_bit_cast(msbs, mask_int_type, "").unwrap().into_int_value();
                let mask =
                    self.builder.build_int_z_extend(mask_small, self.ctx.i64_type(), "").unwrap();
                Ok(Some(mask.as_basic_value_enum()))
            }
        }
    }

    fn claimed_align(&self, t: PhysicalType, unaligned: bool) -> u32 {
        if unaligned { 1 } else { self.k1.get_pt_layout(t).align }
    }

    fn codegen_inst(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        inst_id: InstId,
    ) -> CgResult<()> {
        let ir = &self.k1.ir;
        let span = *ir.sources.get(inst_id);
        self.set_debug_location_from_span(span);
        // eprintln!("codegen_inst i{} {}", inst_id.as_u32(), ir::inst_to_string(self.k1, inst_id));
        let inst = *ir.instrs.get(inst_id);
        match inst {
            Inst::Data(data_inst) => {
                let value: BasicValueEnum<'ctx> = match data_inst {
                    ir::DataInst::U64(u) => self.ctx.i64_type().const_int(u, false).into(),
                    ir::DataInst::I64(i) => self.ctx.i64_type().const_int(i as u64, true).into(),
                    ir::DataInst::Float(f) => match f {
                        TypedFloatValue::F32(f32) => {
                            self.ctx.f32_type().const_float(f32 as f64).into()
                        }
                        TypedFloatValue::F64(f64) => self.ctx.f64_type().const_float(f64).into(),
                    },
                };
                inst_mappings.insert(inst_id, value.as_basic_value_enum());
                Ok(())
            }
            Inst::Alloca { t, returned, .. } => {
                // task(debug info): Eventually we could supplement with the type_id from the
                // VariableDebugInfo here in order to differentiate between byte/char/bool
                let cg_type = self.codegen_type(t);

                let alloca_ptr = if returned {
                    let cg_function = self.get_current_function_mut();
                    debug_assert_eq!(
                        cg_type.pt(),
                        cg_function.function_type.return_logical_cg_type.pt()
                    );
                    if cg_function.function_type.is_sret {
                        let sret_param_value =
                            cg_function.function_value.get_param_iter().nth(0).unwrap();
                        cg_function.returned_sret_variable = Some(inst_id);
                        sret_param_value.into_pointer_value()
                    } else {
                        self.build_k1_alloca(&cg_type, "")
                    }
                } else {
                    // build_k1_alloca hoists the alloca to the top of the function, and sets an explicit align
                    self.build_k1_alloca(&cg_type, "")
                };
                inst_mappings.insert(inst_id, alloca_ptr.as_basic_value_enum());

                let ir_debug_info = self.k1.ir.debug_info.get(inst_id);
                if let Some(var_info) = ir_debug_info.variable_info
                    && !var_info.user_hidden
                    && !self.debug.line_tables_only
                {
                    let name_str = self.k1.ident_str(var_info.name);
                    let debug_locn = self.get_debug_location_from_span(var_info.source_span);
                    let local_variable = self.debug.debug_builder.create_auto_variable(
                        self.debug.current_scope(),
                        name_str,
                        self.debug.current_file(),
                        self.get_line_number(var_info.source_span),
                        cg_type.debug_type(),
                        true,
                        0,
                        cg_type.rich_repr_layout().align,
                    );
                    self.debug.insert_declare_at_end(
                        alloca_ptr,
                        Some(local_variable),
                        None,
                        debug_locn,
                        self.builder.get_insert_block().unwrap(),
                    );
                }

                Ok(())
            }
            Inst::Store { t, dst, value, volatile, unaligned } => {
                let align = self.claimed_align(t, unaligned);
                let dst_ptr = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                let value = match t.as_enum() {
                    PhysicalTypeEnum::Scalar(_) => self.resolve_value(inst_mappings, value)?,
                    PhysicalTypeEnum::Agg(_) => {
                        let cg_ty = self.codegen_type(t);
                        let src_ptr =
                            self.resolve_value(inst_mappings, value)?.into_pointer_value();
                        let load = self.builder.build_load(cg_ty.rich_type(), src_ptr, "").unwrap();
                        load.as_instruction_value().unwrap().set_alignment(align).unwrap();
                        load
                    }
                    PhysicalTypeEnum::Empty => unreachable!(),
                };
                let store = self.builder.build_store(dst_ptr, value).unwrap();
                store.set_alignment(align).unwrap();
                if volatile {
                    unsafe { llvm_sys::core::LLVMSetVolatile(store.as_value_ref(), 1) };
                }
                Ok(())
            }
            Inst::Load { t, src, dst, volatile, unaligned } => {
                let cg_ty = self.codegen_type(t);
                let src_ptr = self.resolve_value(inst_mappings, src)?.into_pointer_value();
                let load = self.builder.build_load(cg_ty.rich_type(), src_ptr, "").unwrap();
                let load_inst = load.as_instruction_value().unwrap();
                load_inst.set_alignment(self.claimed_align(t, unaligned)).unwrap();
                if volatile {
                    unsafe { llvm_sys::core::LLVMSetVolatile(load_inst.as_value_ref(), 1) };
                }
                if dst == ir::Value::Empty {
                    inst_mappings.insert(inst_id, load);
                } else {
                    let dst_align = self.claimed_align(t, unaligned);
                    let dst = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                    self.builder.build_store(dst, load).unwrap().set_alignment(dst_align).unwrap();
                }
                Ok(())
            }
            Inst::AtomicLoad { t, src, ord } => {
                let cg_ty = self.codegen_type(PhysicalType::scalar(t));
                let src_ptr = self.resolve_value(inst_mappings, src)?.into_pointer_value();
                let load = self.builder.build_load(cg_ty.rich_type(), src_ptr, "").unwrap();
                let instr = load.as_instruction_value().unwrap();
                instr.set_alignment(t.get_layout().align).unwrap();
                instr.set_atomic_ordering(Self::llvm_atomic_ordering(ord)).unwrap();
                inst_mappings.insert(inst_id, load);
                Ok(())
            }
            Inst::AtomicStore { dst, value, t, ord } => {
                let dst_pointer = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                let value = self.resolve_value(inst_mappings, value)?;
                let store = self.builder.build_store(dst_pointer, value).unwrap();
                store.set_alignment(t.get_layout().align).unwrap();
                store.set_atomic_ordering(Self::llvm_atomic_ordering(ord)).unwrap();
                Ok(())
            }
            Inst::AtomicRmw { op, t, dst, operand, ord } => {
                use inkwell::AtomicRMWBinOp as RmwOp;
                let dst_pointer = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                let operand = self.resolve_value(inst_mappings, operand)?;
                // atomicrmw only takes integer operands; a pointer-typed element
                // round-trips through the word-sized integer
                let is_ptr = t == ScalarType::Pointer;
                let int_operand = if is_ptr {
                    self.builder
                        .build_ptr_to_int(
                            operand.into_pointer_value(),
                            self.builtin_types.ptr_sized_int,
                            "",
                        )
                        .unwrap()
                } else {
                    operand.into_int_value()
                };
                let rmw_op = match op {
                    ir::AtomicRmwOpIr::Xchg => RmwOp::Xchg,
                    ir::AtomicRmwOpIr::Add => RmwOp::Add,
                    ir::AtomicRmwOpIr::Sub => RmwOp::Sub,
                    ir::AtomicRmwOpIr::And => RmwOp::And,
                    ir::AtomicRmwOpIr::Or => RmwOp::Or,
                    ir::AtomicRmwOpIr::Xor => RmwOp::Xor,
                    ir::AtomicRmwOpIr::MinS => RmwOp::Min,
                    ir::AtomicRmwOpIr::MaxS => RmwOp::Max,
                    ir::AtomicRmwOpIr::MinU => RmwOp::UMin,
                    ir::AtomicRmwOpIr::MaxU => RmwOp::UMax,
                };
                let prev = self
                    .builder
                    .build_atomicrmw(
                        rmw_op,
                        dst_pointer,
                        int_operand,
                        Self::llvm_atomic_ordering(ord),
                    )
                    .unwrap();
                let result: BasicValueEnum<'ctx> = if is_ptr {
                    self.builder.build_int_to_ptr(prev, self.builtin_types.ptr, "").unwrap().into()
                } else {
                    prev.into()
                };
                inst_mappings.insert(inst_id, result);
                Ok(())
            }
            Inst::AtomicCmpxchg { id } => {
                let cas = *self.k1.ir.cmpxchgs.get(id);
                let dst_pointer = self.resolve_value(inst_mappings, cas.dst)?.into_pointer_value();
                let expected = self.resolve_value(inst_mappings, cas.expected)?;
                let desired = self.resolve_value(inst_mappings, cas.desired)?;
                let result_ptr =
                    self.resolve_value(inst_mappings, cas.result)?.into_pointer_value();
                let pair = self
                    .builder
                    .build_cmpxchg(
                        dst_pointer,
                        expected,
                        desired,
                        Self::llvm_atomic_ordering(cas.success),
                        Self::llvm_atomic_ordering(cas.failure),
                    )
                    .unwrap();
                if cas.weak {
                    unsafe { llvm_sys::core::LLVMSetWeak(pair.as_value_ref(), 1) };
                }
                let prev = self.builder.build_extract_value(pair, 0, "").unwrap();
                let ok_i1 = self.builder.build_extract_value(pair, 1, "").unwrap().into_int_value();
                let ok_bool = self.i1_to_bool(ok_i1, "");
                self.builder.build_store(result_ptr, prev).unwrap();
                let ok_offset = self.ctx.i32_type().const_int(cas.ok_vm_offset as u64, false);
                let ok_ptr = unsafe {
                    self.builder
                        .build_in_bounds_gep(self.ctx.i8_type(), result_ptr, &[ok_offset], "")
                        .unwrap()
                };
                self.builder.build_store(ok_ptr, ok_bool).unwrap();
                Ok(())
            }
            Inst::VecOp { id } => {
                let vop = *self.k1.ir.vec_ops.get(id);
                if let Some(result) = self.codegen_vec_op(inst_mappings, vop)? {
                    inst_mappings.insert(inst_id, result);
                }
                Ok(())
            }
            Inst::Fence { ord } => {
                self.builder.build_fence(Self::llvm_atomic_ordering(ord), false, "").unwrap();
                Ok(())
            }
            Inst::Copy { dst, src, t, unaligned, .. } => {
                let dst_value = self.resolve_value(inst_mappings, dst)?;
                let src_value = self.resolve_value(inst_mappings, src)?;
                let layout = self.k1.get_pt_layout(t);
                let align = self.claimed_align(t, unaligned);
                let bytes = self.builtin_types.ptr_sized_int.const_int(layout.size as u64, false);
                self.builder
                    .build_memcpy(
                        dst_value.into_pointer_value(),
                        align,
                        src_value.into_pointer_value(),
                        align,
                        bytes,
                    )
                    .unwrap();
                Ok(())
            }
            Inst::StructOffset { struct_t, base, field_index, .. } => {
                let base_ptr = self.resolve_value(inst_mappings, base)?.into_pointer_value();

                let cg_struct_type = self.codegen_type(PhysicalType::agg(struct_t)).expect_struct();
                let gep =
                    self.build_struct_gep(base_ptr, cg_struct_type.struct_type, field_index, "");
                inst_mappings.insert(inst_id, gep.into());
                Ok(())
            }
            Inst::ArrayOffset { element_t, base, element_index } => {
                let cg_elem_ty = self.codegen_type(element_t);
                let base_ptr = self.resolve_value(inst_mappings, base)?.into_pointer_value();
                let index_int = self.resolve_value(inst_mappings, element_index)?.into_int_value();
                let gep = unsafe {
                    self.builder
                        .build_in_bounds_gep(cg_elem_ty.rich_type(), base_ptr, &[index_int], "")
                        .unwrap()
                };
                inst_mappings.insert(inst_id, gep.into());
                Ok(())
            }
            Inst::Call { call_id: id } => {
                if let Some(return_value) = self.codegen_function_call(inst_mappings, id, span)? {
                    inst_mappings.insert(inst_id, return_value);
                } else {
                    inst_mappings.insert(inst_id, self.builtin_types.empty_struct_value());
                }
                Ok(())
            }
            Inst::Jump(block_id) => {
                let dst_block = self.get_llvm_block(block_id)?;
                let _jump = self.builder.build_unconditional_branch(dst_block).unwrap();
                Ok(())
            }
            Inst::JumpIf { cond, cons, alt } => {
                if cond == Value::IsStatic {
                    let else_block = self.get_llvm_block(alt)?;
                    self.builder.build_unconditional_branch(else_block).unwrap();
                    return Ok(());
                }
                let cond_value = self.resolve_value(inst_mappings, cond)?;
                let cond_value_i1 = self.bool_to_i1(cond_value.into_int_value(), "");
                let then_block = self.get_llvm_block(cons)?;
                let else_block = self.get_llvm_block(alt)?;
                let _branch = self
                    .builder
                    .build_conditional_branch(cond_value_i1, then_block, else_block)
                    .unwrap();
                Ok(())
            }
            Inst::Switch { value, cases, default, .. } => {
                let value = self.resolve_value(inst_mappings, value)?.into_int_value();
                let int_type = value.get_type();
                let default_block = self.get_llvm_block(default)?;
                let mut llvm_cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> =
                    Vec::with_capacity(cases.len() as usize);
                for case in self.k1.ir.mem.getn(cases) {
                    let case_value = int_type.const_int(case.value, false);
                    llvm_cases.push((case_value, self.get_llvm_block(case.target)?));
                }
                self.builder.build_switch(value, default_block, &llvm_cases).unwrap();
                Ok(())
            }
            Inst::Unreachable => {
                self.builder.build_unreachable().unwrap();
                Ok(())
            }
            Inst::Phi { t, incomings } => {
                let phi_ty = self.pt_canon_type(t);
                let phi = self.builder.build_phi(phi_ty, "").unwrap();
                let phi_block = self.builder.get_insert_block().unwrap();
                let debug_locn = self.builder.get_current_debug_location();

                for incoming in self.k1.ir.mem.getn(incomings) {
                    let Some(block) =
                        self.get_current_function().blocks.get(&incoming.from).copied()
                    else {
                        continue;
                    };
                    // Resolve in the edge's source block: anything it emits (e.g. a
                    // reload-global addr call) must dominate the edge, and the phi
                    // must stay at block top
                    match block.get_terminator() {
                        Some(terminator) => self.builder.position_before(&terminator),
                        None => self.builder.position_at_end(block),
                    }
                    let value = self.resolve_value(inst_mappings, incoming.value)?;
                    self.builder.position_at_end(phi_block);
                    self.restore_debug_location(debug_locn);
                    phi.add_incoming(&[(&value, block)])
                }

                inst_mappings.insert(inst_id, phi.as_basic_value());
                Ok(())
            }
            Inst::Ret { v, .. } => {
                let ret_value = self.resolve_value(inst_mappings, v)?;
                let current_fn = self.get_current_function();
                let current_fn_ty = current_fn.function_type;

                if current_fn_ty.is_sret {
                    // We will have already stored the result in the sret slot, since its just an
                    // alias of the variable
                    if current_fn.returned_sret_variable.is_some() {
                        self.builder.build_return(None).unwrap();
                    } else {
                        let sret_value = current_fn
                            .function_value
                            .get_param_iter()
                            .next()
                            .unwrap()
                            .into_pointer_value();
                        self.store_k1_value(
                            &current_fn_ty.return_logical_cg_type,
                            sret_value,
                            ret_value,
                        );
                        self.builder.build_return(None).unwrap();
                    }
                } else {
                    let ret_value_marshalled = self.marshal_abi_return_value(
                        current_fn_ty.return_abi_mapping,
                        &current_fn_ty.return_logical_cg_type,
                        ret_value,
                    );
                    match ret_value_marshalled {
                        None => self.builder.build_return(None).unwrap(),
                        Some(v) => self.builder.build_return(Some(&v)).unwrap(),
                    };
                }
                Ok(())
            }
            Inst::BoolNegate { v } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let i1_input = self.bool_to_i1(input, "");
                let not_i1 = self.builder.build_not(i1_input, "").unwrap();
                let not_bool = self.i1_to_bool(not_i1, "");
                inst_mappings.insert(inst_id, not_bool.as_basic_value_enum());
                Ok(())
            }
            Inst::BitNot { v } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let not_input = self.builder.build_not(input, "").unwrap();
                inst_mappings.insert(inst_id, not_input.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatNeg { v, .. } => {
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let neg_input = self.builder.build_float_neg(input, "").unwrap();
                inst_mappings.insert(inst_id, neg_input.as_basic_value_enum());
                Ok(())
            }
            Inst::BitCast { v, to } => {
                let input = self.resolve_value(inst_mappings, v)?;

                // From agg to scalar -> handled by IF
                // From scalar to agg -> handled by IF
                match to.as_enum() {
                    PhysicalTypeEnum::Empty => panic!("BitCast on ZST"),
                    // From agg to agg -> canon type is ptr, nothing to do.
                    PhysicalTypeEnum::Agg(_) => {
                        inst_mappings.insert(inst_id, input);
                        Ok(())
                    }
                    // From scalar to scalar -> do the llvm bitcast
                    PhysicalTypeEnum::Scalar(st) => {
                        let llvm_ty = self.scalar_basic_type(st);
                        let bitcast = self.builder.build_bit_cast(input, llvm_ty, "").unwrap();
                        inst_mappings.insert(inst_id, bitcast);
                        Ok(())
                    }
                }
            }
            Inst::IntTrunc { v, to } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let trunc = self.builder.build_int_truncate(input, to_int_type, "").unwrap();
                inst_mappings.insert(inst_id, trunc.as_basic_value_enum());
                Ok(())
            }
            Inst::IntExtU { v, to } | Inst::IntExtS { v, to, .. } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let signed = matches!(inst, ir::Inst::IntExtS { .. });
                let ext =
                    self.builder.build_int_cast_sign_flag(input, to_int_type, signed, "").unwrap();
                inst_mappings.insert(inst_id, ext.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatTrunc { v, to } => {
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let trunc = self.builder.build_float_trunc(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, trunc.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatExt { v, to } => {
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let ext = self.builder.build_float_ext(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, ext.as_basic_value_enum());
                Ok(())
            }
            Inst::Float32ToIntUnsigned { v, to } | Inst::Float64ToIntUnsigned { v, to } => {
                let from = if matches!(inst, Inst::Float32ToIntUnsigned { .. }) {
                    ScalarType::F32
                } else {
                    ScalarType::F64
                };
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let int = self.build_float_to_int_saturating(input, from, to, false);
                inst_mappings.insert(inst_id, int.as_basic_value_enum());
                Ok(())
            }
            Inst::Float32ToIntSigned { v, to } | Inst::Float64ToIntSigned { v, to } => {
                let from = if matches!(inst, Inst::Float32ToIntSigned { .. }) {
                    ScalarType::F32
                } else {
                    ScalarType::F64
                };
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let int = self.build_float_to_int_saturating(input, from, to, true);
                inst_mappings.insert(inst_id, int.as_basic_value_enum());
                Ok(())
            }
            Inst::IntToFloatUnsigned { v, from: _, to } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let float =
                    self.builder.build_unsigned_int_to_float(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, float.as_basic_value_enum());
                Ok(())
            }
            Inst::IntToFloatSigned { v, from: _, to } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let float =
                    self.builder.build_signed_int_to_float(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, float.as_basic_value_enum());
                Ok(())
            }
            Inst::PtrToWord { v } => {
                let input = self.resolve_value(inst_mappings, v)?.into_pointer_value();
                let word_int = self
                    .builder
                    .build_ptr_to_int(input, self.builtin_types.ptr_sized_int, "")
                    .unwrap();
                inst_mappings.insert(inst_id, word_int.as_basic_value_enum());
                Ok(())
            }
            Inst::WordToPtr { v } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let ptr_value =
                    self.builder.build_int_to_ptr(input, self.builtin_types.ptr, "").unwrap();
                inst_mappings.insert(inst_id, ptr_value.as_basic_value_enum());
                Ok(())
            }
            Inst::IntAdd { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                // Should overflow trap? idk self.builder.build_int_nsw_add
                let sum = self.builder.build_int_add(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, sum.as_basic_value_enum());
                Ok(())
            }
            Inst::IntSub { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let diff = self.builder.build_int_sub(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, diff.as_basic_value_enum());
                Ok(())
            }
            Inst::IntMul { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let prod = self.builder.build_int_mul(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, prod.as_basic_value_enum());
                Ok(())
            }
            Inst::IntDivUnsigned { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let div = self.builder.build_int_unsigned_div(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, div.as_basic_value_enum());
                Ok(())
            }
            Inst::IntDivSigned { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let div = self.builder.build_int_signed_div(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, div.as_basic_value_enum());
                Ok(())
            }
            Inst::IntRemUnsigned { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let rem = self.builder.build_int_unsigned_rem(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, rem.as_basic_value_enum());
                Ok(())
            }
            Inst::IntRemSigned { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let rem = self.builder.build_int_signed_rem(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, rem.as_basic_value_enum());
                Ok(())
            }
            Inst::IntCmp { lhs, rhs, pred, .. } => {
                let llvm_pred = match pred {
                    ir::IntCmpPred::Eq => IntPredicate::EQ,
                    ir::IntCmpPred::Slt => IntPredicate::SLT,
                    ir::IntCmpPred::Sle => IntPredicate::SLE,
                    ir::IntCmpPred::Sgt => IntPredicate::SGT,
                    ir::IntCmpPred::Sge => IntPredicate::SGE,
                    ir::IntCmpPred::Ult => IntPredicate::ULT,
                    ir::IntCmpPred::Ule => IntPredicate::ULE,
                    ir::IntCmpPred::Ugt => IntPredicate::UGT,
                    ir::IntCmpPred::Uge => IntPredicate::UGE,
                };
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let cmp =
                    self.builder.build_int_compare(llvm_pred, lhs_value, rhs_value, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatAdd { lhs, rhs, .. }
            | Inst::FloatSub { lhs, rhs, .. }
            | Inst::FloatMul { lhs, rhs, .. }
            | Inst::FloatDiv { lhs, rhs, .. }
            | Inst::FloatRem { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_float_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_float_value();
                let result = match inst {
                    Inst::FloatAdd { .. } => {
                        self.builder.build_float_add(lhs_value, rhs_value, "").unwrap()
                    }
                    Inst::FloatSub { .. } => {
                        self.builder.build_float_sub(lhs_value, rhs_value, "").unwrap()
                    }
                    Inst::FloatMul { .. } => {
                        self.builder.build_float_mul(lhs_value, rhs_value, "").unwrap()
                    }
                    Inst::FloatDiv { .. } => {
                        self.builder.build_float_div(lhs_value, rhs_value, "").unwrap()
                    }
                    Inst::FloatRem { .. } => {
                        self.builder.build_float_rem(lhs_value, rhs_value, "").unwrap()
                    }
                    _ => unreachable!(),
                };
                inst_mappings.insert(inst_id, result.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatCmp { lhs, rhs, pred, .. } => {
                let llvm_pred = match pred {
                    ir::FloatCmpPred::Eq => FloatPredicate::OEQ,
                    ir::FloatCmpPred::Lt => FloatPredicate::OLT,
                    ir::FloatCmpPred::Le => FloatPredicate::OLE,
                    ir::FloatCmpPred::Gt => FloatPredicate::OGT,
                    ir::FloatCmpPred::Ge => FloatPredicate::OGE,
                };
                let lhs = self.resolve_value(inst_mappings, lhs)?.into_float_value();
                let rhs = self.resolve_value(inst_mappings, rhs)?.into_float_value();
                let cmp = self.builder.build_float_compare(llvm_pred, lhs, rhs, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
                Ok(())
            }
            Inst::BitAnd { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let and = self.builder.build_and(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, and.as_basic_value_enum());
                Ok(())
            }
            Inst::BitOr { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let or = self.builder.build_or(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, or.as_basic_value_enum());
                Ok(())
            }
            Inst::BitXor { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let xor = self.builder.build_xor(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, xor.as_basic_value_enum());
                Ok(())
            }
            Inst::BitShiftLeft { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let rhs_casted =
                    self.cast_int_to_match(rhs_value, lhs_value, "shift_magnitude_adjust");

                let shl = self.builder.build_left_shift(lhs_value, rhs_casted, "").unwrap();
                inst_mappings.insert(inst_id, shl.as_basic_value_enum());
                Ok(())
            }
            Inst::BitUnsignedShiftRight { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let rhs_casted =
                    self.cast_int_to_match(rhs_value, lhs_value, "shift_magnitude_adjust");
                let lshr =
                    self.builder.build_right_shift(lhs_value, rhs_casted, false, "").unwrap();
                inst_mappings.insert(inst_id, lshr.as_basic_value_enum());
                Ok(())
            }
            Inst::BitSignedShiftRight { lhs, rhs, .. } => {
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let rhs_casted =
                    self.cast_int_to_match(rhs_value, lhs_value, "shift_magnitude_adjust");
                let ashr = self.builder.build_right_shift(lhs_value, rhs_casted, true, "").unwrap();
                inst_mappings.insert(inst_id, ashr.as_basic_value_enum());
                Ok(())
            }
            Inst::BakeStaticValue { .. } => Err(cgerr!(
                self.debug.current_span(),
                "BakeStaticValue is only available to compile-time code"
            )),
        }
    }

    fn cast_int_to_match(
        &self,
        subject: IntValue<'ctx>,
        desired: IntValue<'ctx>,
        name: &str,
    ) -> IntValue<'ctx> {
        if subject.get_type().get_bit_width() != desired.get_type().get_bit_width() {
            self.builder.build_int_cast_sign_flag(subject, desired.get_type(), false, name).unwrap()
        } else {
            subject
        }
    }

    fn make_function_debug_info(
        &mut self,
        function_name: &str,
        function_span: SpanId,
        return_type: DIType<'ctx>,
        param_debug_types: &[DIType<'ctx>],
        is_definition: bool,
    ) -> CgResult<(DISubprogram<'ctx>, DIFile<'ctx>)> {
        let span_id = function_span;
        let function_file_id = self.k1.ast.spans.get(span_id).file_id;
        let (function_line, _) = self.k1.ast.get_lines_for_span_id(span_id).expect("line for span");
        let function_line_number = function_line.line_number();
        let function_scope_start_line_number = function_line_number;
        let function_file = self.debug.files.get(&function_file_id).unwrap();
        let dbg_fn_type = self.debug.debug_builder.create_subroutine_type(
            *function_file,
            Some(return_type),
            param_debug_types,
            0,
        );
        let parent_scope = function_file.as_debug_info_scope();
        let di_subprogram = self.debug.debug_builder.create_function(
            parent_scope,
            function_name,
            None,
            *function_file,
            function_line_number,
            dbg_fn_type,
            false,
            is_definition,
            function_scope_start_line_number,
            0,
            false,
        );
        Ok((di_subprogram, *function_file))
    }

    fn declare_llvm_function(&mut self, function_id: FunctionId) -> CgResult<FunctionValue<'ctx>> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(function.function_value);
        }
        debug!("declare_llvm_function\n{}", self.k1.function_id_to_string(function_id, false));

        let typed_function = self.k1.get_function(function_id);
        let typed_function_linkage = typed_function.linkage;
        let typed_function_params = typed_function.params;
        let function_span = self.k1.ast.get_span_for_id(typed_function.parsed_id);

        let is_dylib_root = match self.kind {
            CgKind::Host => false,
            CgKind::ReloadDylib(ns_id) => {
                typed_function.is_reloadable() && typed_function.namespace_id == ns_id
            }
        };
        let has_body = is_dylib_root || !typed_function_linkage.is_external();
        let is_definition = has_body && self.owned.contains(&function_id);
        let is_private = !is_dylib_root
            && matches!(
                typed_function_linkage,
                TyperLinkage::Standard | TyperLinkage::Intrinsic | TyperLinkage::LlvmIntrinsic(_)
            );
        let llvm_linkage = if is_private && is_definition && !self.multi_unit() {
            LlvmLinkage::Internal
        } else {
            LlvmLinkage::External
        };
        let llvm_name = if is_dylib_root {
            self.make_reloadable_function_symbol(function_id)
        } else {
            match typed_function.linkage {
                TyperLinkage::External { fn_name: Some(link_name), .. }
                | TyperLinkage::Exported { fn_name: Some(link_name) } => {
                    self.k1.ident_str(link_name).to_string()
                }
                TyperLinkage::Exported { fn_name: None } => {
                    self.k1.ident_str(typed_function.name).to_string()
                }
                _ => Cg::mangle(self.k1.function_symbol_name(function_id)),
            }
        };

        let existing_declaration = match self.llvm_module.get_function(&llvm_name) {
            None => None,
            Some(existing) => {
                if !is_definition {
                    Some(existing)
                } else {
                    cgbail!(
                        self.k1.ast.get_span_for_id(typed_function.parsed_id),
                        "Dupe function name: {}",
                        llvm_name
                    );
                }
            }
        };

        let Some(ir_fn) = self.k1.ir.functions.get(&function_id).copied() else {
            cgbail!(
                function_span,
                "Internal Compiler Error: missing ir for function {}, referenced from {}",
                self.k1.function_id_to_string(function_id, false),
                if self.current_insert_function == FunctionId::PENDING {
                    "codegen itself".to_string()
                } else {
                    self.k1.function_id_to_string(self.current_insert_function, false)
                }
            );
        };

        let abi_mode = self.k1.function_abi(function_id);
        let llvm_function_type = self.make_cg_function_type(&ir_fn.fn_type)?;
        debug!(
            "-> res (is_sret={}) {}",
            llvm_function_type.is_sret, llvm_function_type.llvm_function_type
        );

        let mut di_types: SV8<_> = smallvec::smallvec![];
        for t in self.mem.getn_lt(llvm_function_type.param_k1_types).iter() {
            di_types.push(t.debug_type());
        }
        let (di_subprogram, di_file) = self.make_function_debug_info(
            &llvm_name,
            function_span,
            llvm_function_type.return_logical_cg_type.debug_type(),
            &di_types,
            is_definition,
        )?;
        let is_sret = llvm_function_type.is_sret;

        if let Some(existing) = existing_declaration {
            if existing.get_type() != llvm_function_type.llvm_function_type {
                cgbail!(
                    function_span,
                    "Duplicate extern declarations of '{}' disagree on signature",
                    llvm_name
                );
            }
            self.llvm_functions.insert(
                function_id,
                CgFunction {
                    param_values: Vec::with_capacity(
                        llvm_function_type.param_k1_types.len() as usize
                    ),
                    function_type: llvm_function_type,
                    function_value: existing,
                    blocks: FxHashMap::new(),
                    last_alloca_instr: None,
                    returned_sret_variable: None,
                    debug_info: di_subprogram,
                    debug_file: di_file,
                },
            );
            return Ok(existing);
        }

        let function_value = self.llvm_module.add_function(
            &llvm_name,
            llvm_function_type.llvm_function_type,
            Some(llvm_linkage),
        );
        if abi_mode == AbiMode::Internal {
            function_value.set_call_conventions(LLVM_CALL_CONV_FAST);
        }
        if is_private && self.multi_unit() {
            function_value.as_global_value().set_visibility(inkwell::GlobalVisibility::Hidden);
        }
        // K1 has no unwinding: no invoke, no landingpad, anywhere. Extern C
        // decls get it too; LLVM cannot infer it across declaration boundaries
        function_value
            .add_attribute(AttributeLoc::Function, self.make_enum_attribute("nounwind", 0));
        // Keep frame pointers so backtrace() can walk K1 frames
        function_value.add_attribute(
            AttributeLoc::Function,
            self.ctx.create_string_attribute("frame-pointer", "non-leaf"),
        );
        if ir_fn.fn_type.diverges {
            function_value
                .add_attribute(AttributeLoc::Function, self.make_enum_attribute("noreturn", 0));
        }

        if self.k1.config.target.arch() == compiler::Arch::Wasm {
            if let TyperLinkage::External { lib_name: Some(lib_name), .. } = typed_function_linkage
            {
                function_value.add_attribute(
                    AttributeLoc::Function,
                    self.ctx
                        .create_string_attribute("wasm-import-module", self.k1.ident_str(lib_name)),
                );
                function_value.add_attribute(
                    AttributeLoc::Function,
                    self.ctx.create_string_attribute("wasm-import-name", &llvm_name),
                );
            }
            if typed_function_linkage.is_exported() {
                function_value.add_attribute(
                    AttributeLoc::Function,
                    self.ctx.create_string_attribute("wasm-export-name", &llvm_name),
                );
            }
        }
        if is_sret {
            for attr in self.make_sret_attributes(&llvm_function_type.return_logical_cg_type) {
                function_value.add_attribute(AttributeLoc::Param(0), attr);
            }
        }

        // We have to make another pass, now that we've actually made an llvm function value,
        // to set some parameter attributes: names and byval, currently
        for (i, param) in function_value.get_param_iter().enumerate() {
            if is_sret && i == 0 {
                param.set_name("sret")
            } else {
                let offset = if is_sret { 1 } else { 0 };
                let typed_param = self.k1.mem.get_nth(typed_function_params, i - offset);
                let v = self.k1.variables.get(typed_param.variable_id);
                param.set_name(self.k1.ident_str(v.name));

                // Without the byval attribute, X86 (System V) big struct calls don't work
                let abi_mapping =
                    self.mem.get_nth_lt(llvm_function_type.param_abi_mappings, i - offset);
                if matches!(abi_mapping, AbiParamMapping::BigStructByPtrToCopy { byval_attr: true })
                {
                    let k1_type =
                        *self.mem.get_nth_lt(llvm_function_type.param_k1_types, i - offset);
                    for attr in self.make_byval_attributes(&k1_type) {
                        function_value.add_attribute(AttributeLoc::Param(i as u32), attr);
                    }
                }
            }
        }

        if is_definition {
            self.functions_pending_body_compilation.push(function_id);
        }

        function_value.set_subprogram(di_subprogram);

        self.llvm_functions.insert(
            function_id,
            CgFunction {
                param_values: Vec::with_capacity(llvm_function_type.param_k1_types.len() as usize),
                function_type: llvm_function_type,
                function_value,
                blocks: FxHashMap::new(),
                last_alloca_instr: None,
                returned_sret_variable: None,
                debug_info: di_subprogram,
                debug_file: di_file,
            },
        );
        Ok(function_value)
    }

    fn get_abi_mapping_for_type(&self, pt: PhysicalType, is_return: bool) -> AbiParamMapping {
        enum CallConv {
            AMD64,
            ARM64,
        }
        let callconv = match self.k1.config.target.arch() {
            compiler::Arch::Intel => CallConv::AMD64,
            compiler::Arch::Arm => CallConv::ARM64,
            compiler::Arch::Wasm => CallConv::ARM64,
        };

        // https://yorickpeterse.com/articles/the-mess-that-is-handling-structure-arguments-and-returns-in-llvm/
        match pt.as_enum() {
            PhysicalTypeEnum::Empty => {
                if is_return {
                    AbiParamMapping::VoidReturnEmpty
                } else {
                    panic!("Cannot have empty parameter type")
                }
            }
            PhysicalTypeEnum::Scalar(_) => AbiParamMapping::ScalarInRegister,
            PhysicalTypeEnum::Agg(agg_id) => {
                let agg_record = self.k1.agg_types.get(agg_id);
                match agg_record.agg_type {
                    AggType::Sum(_)
                    | AggType::Union { .. }
                    | AggType::Struct { .. }
                    | AggType::Array { .. }
                    | AggType::Vector { .. }
                    | AggType::Opaque { .. } => {
                        let size_bytes = agg_record.layout.size;
                        if size_bytes == 8 {
                            let ([class1, _], _) = self.collect_aggregate_eightbytes(pt);
                            if class1 == RegisterClass::Ptr {
                                return AbiParamMapping::StructAsPointer;
                            }
                        }
                        match callconv {
                            CallConv::ARM64 => {
                                if let Some((element, count)) = self.detect_float_aggregate(pt) {
                                    AbiParamMapping::StructByHfa { element, count }
                                } else if size_bytes <= 8 {
                                    // Returns use an ABI type of the exact size of the struct
                                    // Params use an ABI type of 64, but need to know the 'active' bits

                                    let struct_bits = size_bytes * 8;
                                    let abi_bits = if is_return { struct_bits } else { 64 };
                                    let active_bits = struct_bits;
                                    AbiParamMapping::StructInInteger {
                                        abi_width: abi_bits,
                                        active_width: active_bits,
                                    }
                                } else if size_bytes <= 16 {
                                    // If the size of the structure is between 9 and 16 bytes, pass the structure as
                                    // [an array of] two integers of 8 bytes each
                                    AbiParamMapping::StructByIntPairArray
                                } else {
                                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: false }
                                }
                            }
                            CallConv::AMD64 => {
                                // SysV AMD64 3.2.3: unaligned fields force class MEMORY
                                if agg_record.has_misaligned_fields {
                                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: true }
                                } else if size_bytes <= 8 {
                                    let ([class1, _], _) = self.collect_aggregate_eightbytes(pt);
                                    if class1 == RegisterClass::Float {
                                        // One SSE eightbyte: it rides in an XMM register, so it
                                        // must be typed as floats, not as an integer
                                        let (element, count) = self
                                            .detect_float_aggregate(pt)
                                            .unwrap_or((ScalarType::F64, 1));
                                        AbiParamMapping::StructInSse { element, count }
                                    } else {
                                        // Otherwise it rides in a GPR as an integer of its size
                                        let width_bits = size_bytes * 8;
                                        AbiParamMapping::StructInInteger {
                                            abi_width: width_bits,
                                            active_width: width_bits,
                                        }
                                    }
                                } else if size_bytes <= 16 {
                                    // "If the size is between 8 and 16 bytes, the logic is a little more difficult."
                                    // Pass by classified eightbytes
                                    let ([eb1, eb2], eb2_bits) =
                                        self.collect_aggregate_eightbytes(pt);
                                    if eb1 == RegisterClass::Initial
                                        || eb2 == RegisterClass::Initial
                                    {
                                        panic!(
                                            "Failed to collect 2 eightbytes for 9-16 byte struct {}. Likely a bug.",
                                            self.k1.pt_to_string(pt)
                                        )
                                    }
                                    AbiParamMapping::StructByEightbytePair {
                                        class1: eb1,
                                        class2: eb2,
                                        active_bits2: eb2_bits,
                                    }
                                } else {
                                    // Without the byval attribute, SYS-V does not work
                                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: true }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn detect_float_aggregate(&self, pt: PhysicalType) -> Option<(ScalarType, u32)> {
        fn add_member(element: &mut Option<ScalarType>, count: &mut u32, st: ScalarType) -> bool {
            if !matches!(st, ScalarType::F32 | ScalarType::F64) {
                return false;
            }

            match element {
                Some(existing) if *existing != st => false,
                _ => {
                    *element = Some(st);
                    *count += 1;
                    *count <= 4
                }
            }
        }

        fn visit<'ctx, 'k1>(
            c: &Cg<'ctx, 'k1>,
            element: &mut Option<ScalarType>,
            count: &mut u32,
            t: PhysicalType,
        ) -> bool {
            match t.as_enum() {
                PhysicalTypeEnum::Empty => true,
                PhysicalTypeEnum::Scalar(st) => add_member(element, count, st),
                PhysicalTypeEnum::Agg(agg_id) => {
                    let agg_record = c.k1.agg_types.get(agg_id);
                    match agg_record.agg_type {
                        AggType::Struct { fields, .. } => {
                            c.k1.mem
                                .getn(fields)
                                .iter()
                                .all(|f| visit(c, element, count, f.field_t))
                        }
                        AggType::Array { element_pt, len } => {
                            (0..len).all(|_| visit(c, element, count, element_pt))
                        }
                        AggType::Vector { .. }
                        | AggType::Union { .. }
                        | AggType::Sum(_)
                        | AggType::Opaque { .. } => false,
                    }
                }
            }
        }

        let mut element = None;
        let mut count = 0;
        if visit(self, &mut element, &mut count, pt) && count > 0 {
            Some((element.unwrap(), count))
        } else {
            None
        }
    }

    // What a horrible amount of code for such a small transformation!
    fn collect_aggregate_eightbytes(&self, pt: PhysicalType) -> ([RegisterClass; 2], u32) {
        // This whole thing could be generalized to collect N eightbytes, rather than 2, which would let me use it
        // for the HFA detection
        fn mark_bits(
            classes: &mut [RegisterClass; 2],
            active_bits2: &mut u32,
            offset_bits: u32,
            size_bits: u32,
            class: RegisterClass,
        ) {
            if size_bits == 0 {
                return;
            }

            let end_bits = offset_bits + size_bits;
            debug_assert!(end_bits <= 128);

            for eightbyte in (offset_bits / 64)..=((end_bits - 1) / 64) {
                let i = eightbyte as usize;
                classes[i] = classes[i].combine(class);
                if i == 1 {
                    *active_bits2 = (*active_bits2).max(end_bits.min(128) - 64);
                }
            }
        }

        fn scalar_register_class(st: ScalarType) -> RegisterClass {
            match st {
                ScalarType::U8
                | ScalarType::U16
                | ScalarType::U32
                | ScalarType::U64
                | ScalarType::I8
                | ScalarType::I16
                | ScalarType::I32
                | ScalarType::I64
                | ScalarType::Char
                | ScalarType::Bool => RegisterClass::Int,
                ScalarType::F32 | ScalarType::F64 => RegisterClass::Float,
                ScalarType::Pointer => RegisterClass::Ptr,
            }
        }

        fn handle_type_rec<'ctx, 'k1>(
            c: &Cg<'ctx, 'k1>,
            classes: &mut [RegisterClass; 2],
            active_bits2: &mut u32,
            offset_bits: u32,
            t: PhysicalType,
        ) {
            match t.as_enum() {
                PhysicalTypeEnum::Empty => {}
                PhysicalTypeEnum::Scalar(st) => {
                    let class = scalar_register_class(st);
                    mark_bits(
                        classes,
                        active_bits2,
                        offset_bits,
                        st.get_layout().size_bits(),
                        class,
                    )
                }
                PhysicalTypeEnum::Agg(agg_id) => {
                    let agg_record = c.k1.agg_types.get(agg_id);
                    match agg_record.agg_type {
                        AggType::Struct { fields, .. } => {
                            for f in c.k1.mem.getn(fields) {
                                handle_type_rec(
                                    c,
                                    classes,
                                    active_bits2,
                                    offset_bits + f.offset * 8,
                                    f.field_t,
                                )
                            }
                        }
                        AggType::Array { element_pt: element_t, len } => {
                            let element_layout = c.k1.get_pt_layout(element_t);
                            for i in 0..len {
                                let element_offset = element_layout.offset_at_index(i as usize);
                                handle_type_rec(
                                    c,
                                    classes,
                                    active_bits2,
                                    offset_bits + element_offset as u32 * 8,
                                    element_t,
                                )
                            }
                        }
                        AggType::Vector { element_pt, len } => {
                            let element_layout = element_pt.get_layout();
                            for i in 0..len {
                                let element_offset = element_layout.offset_at_index(i as usize);
                                handle_type_rec(
                                    c,
                                    classes,
                                    active_bits2,
                                    offset_bits + element_offset as u32 * 8,
                                    PhysicalType::scalar(element_pt),
                                )
                            }
                        }
                        AggType::Union { members } => {
                            // Classify every member at the same offset; combine()
                            // prefers Ptr so pointer-bearing unions travel as ptr
                            for m in c.k1.mem.getn(members) {
                                handle_type_rec(c, classes, active_bits2, offset_bits, m.ty)
                            }
                            // Members may not cover the union's full (max) size
                            mark_bits(
                                classes,
                                active_bits2,
                                offset_bits,
                                agg_record.layout.size_bits(),
                                RegisterClass::Int,
                            )
                        }
                        AggType::Sum(e) => {
                            // Just handle the sum's struct representation
                            handle_type_rec(
                                c,
                                classes,
                                active_bits2,
                                offset_bits,
                                PhysicalType::agg(e.struct_repr),
                            )
                        }
                        AggType::Opaque { size, .. } => mark_bits(
                            classes,
                            active_bits2,
                            offset_bits,
                            size * 8,
                            RegisterClass::Int,
                        ),
                    }
                }
            }
        }
        let mut classes = [RegisterClass::Initial, RegisterClass::Initial];
        let mut active_bits2 = 0;

        handle_type_rec(self, &mut classes, &mut active_bits2, 0, pt);

        (classes, active_bits2)
    }

    fn codegen_function_body(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        function_id: FunctionId,
    ) -> CgResult<()> {
        self.current_insert_function = function_id;
        if self.is_reloadable_function(function_id) {
            return self.codegen_reload_stub(function_id);
        }
        let typed_function = self.k1.get_function(function_id);
        let is_debug = typed_function.compiler_debug();

        let function_span = self.k1.ast.get_span_for_id(typed_function.parsed_id);
        let function_line_number = self
            .k1
            .ast
            .get_lines_for_span_id(function_span)
            .expect("line for function span")
            .0
            .line_number();

        let typed_function_params = typed_function.params;
        let codegened_function = self.llvm_functions.get(&function_id).unwrap();
        let cg_function_type = &codegened_function.function_type;
        let is_sret = cg_function_type.is_sret;
        let param_k1_types = cg_function_type.param_k1_types;
        let param_abi_mappings = cg_function_type.param_abi_mappings;
        let function_value = codegened_function.function_value;

        self.debug.push_scope(
            function_span,
            codegened_function.debug_info.as_debug_info_scope(),
            codegened_function.debug_file,
        );
        self.last_debug_location.set(None);
        self.set_debug_location_from_span(function_span);

        let prelude_block = self.ctx.append_basic_block(function_value, "prelude");
        self.builder.position_at_end(prelude_block);
        for (i, param) in function_value.get_param_iter().enumerate() {
            let is_sret_param = i == 0 && is_sret;
            if is_sret_param {
                continue;
            }

            let logical_param_index = i - if is_sret { 1 } else { 0 };
            let param_k1_type = *self.mem.get_nth_lt(param_k1_types, logical_param_index);
            let param_abi_mapping = *self.mem.get_nth_lt(param_abi_mappings, logical_param_index);

            // let typed_param_record =
            //     self.k1.mem.get_nth(typed_function_params, logical_param_index);

            let mapped_value =
                self.canonicalize_abi_param_value(param_abi_mapping, &param_k1_type, param);

            self.set_debug_location_from_span(function_span);
            let ps = &mut self.llvm_functions.get_mut(&function_id).unwrap().param_values;
            //eprintln!("pushing param value {}: {}", ps.len(), mapped_value);
            ps.push(mapped_value);
            if !self.debug.line_tables_only {
                let typed_param = self.k1.mem.get_nth(typed_function_params, logical_param_index);
                let name = self.k1.ident_str(self.k1.variables.get(typed_param.variable_id).name);
                let di_local_variable = self.debug.debug_builder.create_parameter_variable(
                    self.debug.current_scope(),
                    name,
                    logical_param_index as u32,
                    self.debug.current_file(),
                    function_line_number,
                    param_k1_type.debug_type(),
                    true,
                    0,
                );
                self.debug.insert_dbg_value_at_end(
                    mapped_value,
                    di_local_variable,
                    None,
                    self.get_debug_location(),
                    prelude_block,
                );
            }
        }

        self.set_debug_location_from_span(function_span);
        self.codegen_unit_body(inst_mappings, function_id)?;
        if is_debug {
            // function_value.view_function_cfg_only();
            debug!("LLVM final function");
            function_value.print_to_stderr();
        }
        self.debug.pop_scope();
        self.last_debug_location.set(None);

        Ok(())
    }

    fn codegen_unit_body(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        function_id: FunctionId,
    ) -> CgResult<()> {
        let Some(ir_unit) = self.k1.ir.functions.get(&function_id).copied() else {
            cgbail!(
                self.k1.get_function_span(function_id),
                "Internal Compiler Error: missing ir for function {}",
                self.k1.function_id_to_string(function_id, false)
            );
        };
        match ir_unit.function_builtin_kind {
            Some(builtin_kind) => {
                let _terminator_instr =
                    self.codegen_builtin_function_body(builtin_kind, function_id)?;
                return Ok(());
            }
            None => {}
        };

        let ir_unit = self.k1.ir.functions.get(&function_id).copied().unwrap();
        let blocks = ir_unit.blocks;

        let mut seen = std::mem::take(&mut self.buffers.cfg_seen);
        let mut blocks_rpo = std::mem::take(&mut self.buffers.cfg_blocks_rpo);
        self.compute_cfg_order(blocks.first, &mut blocks_rpo, &mut seen);

        let mut block_mapping = FxHashMap::new();
        let llvm_function = self.get_current_function().function_value;
        for block in &blocks_rpo {
            let kind = self.k1.ir.mem.get(*block).data.kind;
            let b = self.ctx.append_basic_block(llvm_function, kind.str());
            block_mapping.insert(*block, b);
        }
        self.get_current_function_mut().blocks = block_mapping;

        {
            // Jump from prelude to entry block
            let debug_locn = self.builder.get_current_debug_location().unwrap();
            self.builder.unset_current_debug_location();

            let entry = self.get_llvm_block(blocks.first)?;
            self.builder.build_unconditional_branch(entry).unwrap();

            self.builder.set_current_debug_location(debug_locn);
        }

        inst_mappings.clear();
        for block in &blocks_rpo {
            self.codegen_block(inst_mappings, *block)?;
        }
        {
            blocks_rpo.clear();
            seen.clear();
            self.buffers.cfg_seen = seen;
            self.buffers.cfg_blocks_rpo = blocks_rpo;
        }

        Ok(())
    }

    fn compute_cfg_order(
        &mut self,
        entry: BlockId,
        result: &mut Vec<BlockId>,
        seen: &mut FxHashSet<BlockId>,
    ) {
        fn dfs(
            ir: &ProgramIr,
            b: BlockId,
            seen: &mut FxHashSet<BlockId>,
            result: &mut Vec<BlockId>,
        ) {
            if !seen.insert(b) {
                return;
            }

            let mut successors = Vec::with_capacity(4);
            Cg::live_successors(ir, b, &mut successors);
            for succ in successors {
                dfs(ir, succ, seen, result);
            }

            result.push(b); // postorder: after successors
        }

        dfs(&self.k1.ir, entry, seen, result);

        result.reverse(); // reverse → RPO
    }

    fn codegen_int_value(&mut self, integer: TypedIntValue) -> BasicValueEnum<'ctx> {
        let cg_ty = self.codegen_type(integer.get_integer_type().get_pt());
        let llvm_int_ty = cg_ty.rich_type().into_int_type();
        let llvm_value = if integer.get_integer_type().is_signed() {
            llvm_int_ty.const_int(integer.to_u64_bits(), true)
        } else {
            llvm_int_ty.const_int(integer.to_u64_bits(), false)
        };
        llvm_value.as_basic_value_enum()
    }

    fn codegen_float_value(&mut self, float: TypedFloatValue) -> CgResult<BasicValueEnum<'ctx>> {
        let cg_ty = self.codegen_type(PhysicalType::scalar(float.get_scalar_type()));
        let llvm_float_ty = cg_ty.rich_type().into_float_type();
        let llvm_value = llvm_float_ty.const_float(float.as_f64());
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_static_value_as_const(
        &mut self,
        static_value_id: StaticValueId,
        depth: usize,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        if let Some(basic) = self.static_values_basics.get(&static_value_id) {
            return Ok(*basic);
        }
        debug!("codegen_static_value_as_const {}", self.k1.static_value_to_string(static_value_id));

        let result = match self.k1.static_values.get(static_value_id) {
            StaticValue::Empty(_type_id) => {
                // NOTE: We may need a few different representations of empty here
                // to keep the type system happy: empty array, empty struct, at least
                self.builtin_types.empty_struct_value()
            }
            StaticValue::Bool(b) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum(),
                false => self.builtin_types.false_value.as_basic_value_enum(),
            },
            StaticValue::Char(byte) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum()
            }
            StaticValue::Int(int_value) => self.codegen_int_value(*int_value),
            StaticValue::Enum(_, int_value) => self.codegen_int_value(*int_value),
            StaticValue::Float(float_value) => self.codegen_float_value(*float_value).unwrap(),
            StaticValue::String(string_id) => {
                let string_global = self.codegen_string_id_to_global(*string_id).unwrap();
                string_global.get_initializer().unwrap()
            }
            StaticValue::Zero(type_id) => {
                let storage_type_id = match self.k1.types.get(*type_id) {
                    Type::Reference(r) => r.inner_type,
                    _ => *type_id,
                };
                let pt = self.k1.get_physical_type_computed(storage_type_id).unwrap();
                let cg_type = self.codegen_type(pt);
                let zero = cg_type.rich_type().const_zero();
                zero.as_basic_value_enum()
            }
            StaticValue::Struct(s) => {
                debug_assert!(!s.fields.is_empty());
                // Always a packed struct, accounting for every byte.
                let s_type_id = s.type_id;
                let s_fields = s.fields;
                let layout = self.k1.get_struct_layout_computed(s.type_id);
                let mut last_offset = 0;
                let mut packed_values = self.tmp.new_list(8);
                for (field, field_layout) in
                    self.k1.static_values.mem.getn(s_fields).iter().zip(layout)
                {
                    let padding = field_layout.offset - last_offset;
                    if padding > 0 {
                        // There is padding here, we have to insert it
                        let padding_value =
                            self.padding_type(padding).get_undef().as_basic_value_enum();
                        packed_values.push_grow(&mut self.tmp, padding_value);
                    }
                    let value = self.codegen_static_value_as_const(*field, depth + 1)?;
                    packed_values.push_grow(&mut self.tmp, value);
                    let field_size = self.k1.get_pt_layout(field_layout.field_t);
                    debug_assert_eq!(self.layout_per_llvm(&value.get_type()).size, field_size.size);
                    last_offset = field_layout.offset + field_size.size;
                }
                let struct_value = self.ctx.const_struct(&packed_values, true);

                debug_assert_eq!(
                    self.layout_per_llvm(&struct_value.get_type()).size,
                    self.k1.get_layout_computed(s_type_id).unwrap().size,
                    "Checking Size of: {}",
                    struct_value
                );
                struct_value.as_basic_value_enum()
            }
            StaticValue::Sum(sum) => {
                let sum = *sum;
                let mut packed_values = self.tmp.new_list(4);

                let sum_agg_id =
                    self.k1.get_physical_type_computed(sum.sum_type_id).unwrap().expect_agg();
                let sum_agg_record = self.k1.agg_types.get(sum_agg_id);
                let sum_pt = sum_agg_record.agg_type.expect_sum();
                let variant = self.k1.mem.get_nth(sum_pt.variants, sum.variant_index as usize);
                let variant_tag = variant.tag;
                let envelope_layout = sum_agg_record.layout;
                let variant_payload_pt = variant.payload;
                let payload_offset = sum_pt.payload_offset;

                let tag_llvm_value = self.codegen_int_value(variant_tag);
                let tag_layout = variant_tag.get_scalar_type().get_layout();
                packed_values.push(tag_llvm_value);
                match sum.payload {
                    None => {
                        let padding_to_end = envelope_layout.size - tag_layout.size;
                        if padding_to_end > 0 {
                            packed_values.push(
                                self.padding_type(padding_to_end).get_undef().as_basic_value_enum(),
                            );
                        }
                    }
                    Some(payload) => {
                        let tag_end = tag_layout.size;
                        let payload_padding = payload_offset - tag_end;
                        if payload_padding > 0 {
                            packed_values.push(
                                self.padding_type(payload_padding)
                                    .get_undef()
                                    .as_basic_value_enum(),
                            );
                        }

                        let payload_value =
                            self.codegen_static_value_as_const(payload, depth + 1)?;
                        packed_values.push(payload_value);

                        let payload_pt = variant_payload_pt.unwrap();
                        let payload_size = self.k1.get_pt_layout(payload_pt).size;
                        let written_so_far = payload_offset + payload_size;
                        let padding_to_end = envelope_layout.size - written_so_far;
                        if padding_to_end > 0 {
                            packed_values.push(
                                self.padding_type(padding_to_end).get_undef().as_basic_value_enum(),
                            );
                        }
                    }
                }

                let struct_value = self.ctx.const_struct(&packed_values, true);
                debug_assert_eq!(
                    self.layout_per_llvm(&struct_value.get_type()).size,
                    envelope_layout.size
                );
                struct_value.as_basic_value_enum()
            }
            StaticValue::LinearContainer(cont) => {
                let cont = *cont;
                let element_type = self.k1.get_linear_container_element(cont.type_id).unwrap();
                let span_elements = self.k1.static_values.mem.getn(cont.elements);
                let array_value =
                    self.codegen_static_elements_array(element_type, span_elements, depth)?;
                let element_align = self.k1.get_layout_computed(element_type).unwrap().align;
                self.codegen_static_container(
                    static_value_id,
                    cont.type_id,
                    cont.kind,
                    cont.len(),
                    element_align,
                    array_value.as_basic_value_enum(),
                )
            }
            StaticValue::RawContainer(raw) => {
                let raw = *raw;
                let array_value = self.codegen_raw_bytes_array(raw);
                self.codegen_static_container(
                    static_value_id,
                    raw.type_id,
                    raw.kind,
                    raw.len(),
                    raw.scalar.get_layout().align,
                    array_value.as_basic_value_enum(),
                )
            }
        };
        if depth == 0 {
            self.static_values_basics.insert(static_value_id, result);
        }
        Ok(result)
    }

    fn codegen_raw_bytes_array(&mut self, raw: StaticRawContainer) -> ArrayValue<'ctx> {
        let element_type = self.codegen_type(PhysicalType::scalar(raw.scalar)).rich_type();
        let bytes = self.k1.static_values.mem.getn(raw.bytes);
        if bytes.is_empty() {
            return element_type.array_type(0).const_zero();
        }
        let value = unsafe {
            llvm_sys::core::LLVMConstDataArray(
                element_type.as_type_ref(),
                bytes.as_ptr() as *const std::ffi::c_char,
                bytes.len(),
            )
        };
        unsafe { ArrayValue::new(value) }
    }

    fn codegen_static_container(
        &mut self,
        static_value_id: StaticValueId,
        container_type_id: TypeId,
        kind: StaticContainerKind,
        len: usize,
        element_align: u32,
        array_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            StaticContainerKind::Array | StaticContainerKind::Vector => array_value,
            StaticContainerKind::Span | StaticContainerKind::Buffer | StaticContainerKind::List => {
                let data_global = self.make_global_from_value(
                    array_value,
                    element_align,
                    &format!("k1.static.{}.elems", static_value_id.as_u32()),
                    true,
                    LlvmLinkage::LinkOnceODR,
                    false,
                );
                let data = data_global.as_pointer_value();
                let len = len as u64;
                let final_struct = match kind {
                    StaticContainerKind::Buffer => {
                        let buffer_pt =
                            self.k1.get_physical_type_computed(container_type_id).unwrap();
                        let buffer_cg_type = self.codegen_type(buffer_pt).expect_struct();
                        self.make_buffer_struct(buffer_cg_type.struct_type, len, data)
                    }
                    StaticContainerKind::Span => {
                        self.make_span_struct(container_type_id, len, data)
                    }
                    StaticContainerKind::List => {
                        let buffer_struct = self.make_span_struct(container_type_id, len, data);
                        self.make_list_struct(container_type_id, buffer_struct, len)
                    }
                    StaticContainerKind::Array | StaticContainerKind::Vector => unreachable!(),
                };
                final_struct.as_basic_value_enum()
            }
        }
    }

    fn codegen_static_elements_array(
        &mut self,
        element_type: TypeId,
        elements: &[StaticValueId],
        depth: usize,
    ) -> CgResult<StructValue<'ctx>> {
        let element_layout = self.k1.get_layout_computed(element_type).unwrap();
        // Stride padding is a container concern, not part of Layout.size, so each
        // element may be followed by an explicit padding entry
        let end_padding = element_layout.stride() - element_layout.size;
        let values_per_element: u32 = if end_padding > 0 { 2 } else { 1 };
        let mut packed_values = self.tmp.new_list(elements.len() as u32 * values_per_element);

        for elem in elements.iter() {
            let elem_basic_value = self.codegen_static_value_as_const(*elem, depth + 1)?;
            packed_values.push(elem_basic_value);

            if end_padding > 0 {
                let padding_value =
                    self.padding_type(end_padding).get_undef().as_basic_value_enum();
                packed_values.push(padding_value);
            }
        }
        let array_packed_struct = self.ctx.const_struct(&packed_values, true);
        Ok(array_packed_struct)
    }

    fn make_global_for_static_value(
        &mut self,
        static_value_id: StaticValueId,
    ) -> CgResult<GlobalValue<'ctx>> {
        if let Some(global) = self.static_values_globals.get(&static_value_id) {
            return Ok(*global);
        };
        let direct_value = self.codegen_static_value_as_const(static_value_id, 0)?;
        let type_id = self.k1.get_static_value_type(static_value_id);
        let layout = self.k1.get_layout_computed(type_id).unwrap();
        let global = self.make_global_from_value(
            direct_value,
            layout.align,
            &format!("k1.static.{}", static_value_id.as_u32()),
            true,
            LlvmLinkage::LinkOnceODR,
            false,
        );
        self.static_values_globals.insert(static_value_id, global);
        Ok(global)
    }

    fn target_supports_tls(&self) -> bool {
        self.k1.config.target.arch() != compiler::Arch::Wasm
            && self.k1.config.target.platform() != compiler::Platform::Bare
    }

    fn make_external_global(
        &mut self,
        typ: BasicTypeEnum<'ctx>,
        name: &str,
        constant: bool,
    ) -> GlobalValue<'ctx> {
        let global = self.llvm_module.add_global(typ, None, name);
        global.set_constant(constant);
        global.set_linkage(LlvmLinkage::External);
        global
    }

    fn make_global_from_value(
        &mut self,
        value: BasicValueEnum<'ctx>,
        align: u32,
        name: &str,
        constant: bool,
        linkage: LlvmLinkage,
        is_tls: bool,
    ) -> GlobalValue<'ctx> {
        let global = self.llvm_module.add_global(value.get_type(), None, name);
        global.set_alignment(align);
        global.set_unnamed_addr(true);
        global.set_initializer(&value);
        global.set_constant(constant);
        global.set_linkage(linkage);
        if linkage == LlvmLinkage::LinkOnceODR {
            global.set_visibility(inkwell::GlobalVisibility::Hidden);
        }

        if is_tls && self.target_supports_tls() {
            global.set_thread_local(true);
            let mode = if self.k1.program_settings.executable {
                ThreadLocalMode::LocalExecTLSModel
            } else {
                ThreadLocalMode::GeneralDynamicTLSModel
            };
            global.set_thread_local_mode(Some(mode));
        }
        global
    }

    fn codegen_static_value_canonical(
        &mut self,
        static_value_id: StaticValueId,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        debug!(
            "codegen_static_value_canonical {}",
            self.k1.static_value_to_string(static_value_id)
        );
        let v = self.k1.static_values.get(static_value_id);
        let result = match v {
            StaticValue::Empty(_)
            | StaticValue::Bool(_)
            | StaticValue::Char(_)
            | StaticValue::Int(_)
            | StaticValue::Enum(_, _)
            | StaticValue::Float(_) => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::String(string_id) => {
                let string_global = self.codegen_string_id_to_global(*string_id).unwrap();
                string_global.as_basic_value_enum()
            }
            StaticValue::Zero(type_id) => {
                let pt = self.k1.get_physical_type_computed(*type_id).unwrap();
                if pt.is_agg() {
                    let global = self.make_global_for_static_value(static_value_id)?;
                    global.as_pointer_value().as_basic_value_enum()
                } else {
                    self.codegen_type(pt).rich_type().const_zero()
                }
            }
            StaticValue::Struct(_)
            | StaticValue::Sum(_)
            | StaticValue::LinearContainer(_)
            | StaticValue::RawContainer(_) => {
                let global = self.make_global_for_static_value(static_value_id)?;
                global.as_pointer_value().as_basic_value_enum()
            }
        };
        Ok(result)
    }

    fn make_string_llvm_global(&mut self, string_id: StringId) -> CgResult<GlobalValue<'ctx>> {
        let string_name = &format!("k1.string.{}.bytes", string_id.as_u32());
        let rust_str = self.k1.get_string(string_id);
        let str_len = rust_str.len();
        let global_str_data = self.llvm_module.add_global(
            self.builtin_types.char.array_type(str_len as u32),
            None,
            string_name,
        );
        let str_data_array = i8_array_from_str(self.ctx, rust_str);
        global_str_data.set_linkage(LlvmLinkage::LinkOnceODR);
        global_str_data.set_visibility(inkwell::GlobalVisibility::Hidden);
        global_str_data.set_initializer(&str_data_array);
        global_str_data.set_unnamed_addr(true);
        global_str_data.set_constant(true);

        // Ensure the string layout is what we expect
        // type string = { private span: span[char] }
        let string_type_id = self.k1.string_type_id();
        let string_pt = self.k1.get_physical_type_computed(string_type_id).unwrap();
        let string_type = self.codegen_type(string_pt).expect_struct();
        let string_wrapper_struct_type = string_type.struct_type;

        let char_span_struct = self.mem.get_nth_lt(string_type.fields, 0).expect_struct();
        let char_buffer_cg_type = self.mem.get_nth_lt(char_span_struct.fields, 0).expect_struct();
        debug_assert!(
            char_buffer_cg_type.struct_type.get_field_type_at_index(0).unwrap().is_pointer_type()
        );
        debug_assert!(
            char_buffer_cg_type
                .struct_type
                .get_field_type_at_index(1)
                .unwrap()
                .into_int_type()
                .get_bit_width()
                == 64
        );
        debug_assert!(char_buffer_cg_type.struct_type.count_fields() == 2);

        let char_buffer_struct_value = self.make_buffer_struct(
            char_buffer_cg_type.struct_type,
            str_len as u64,
            global_str_data.as_pointer_value(),
        );
        let char_span_struct_value = char_span_struct
            .struct_type
            .const_named_struct(&[char_buffer_struct_value.as_basic_value_enum()]);
        let string_wrapper_struct = string_wrapper_struct_type
            .const_named_struct(&[char_span_struct_value.as_basic_value_enum()]);

        let global_str_struct = self.llvm_module.add_global(
            string_wrapper_struct_type,
            None,
            &format!("k1.string.{}", string_id.as_u32()),
        );
        global_str_struct.set_initializer(&string_wrapper_struct);
        global_str_struct.set_constant(true);
        global_str_struct.set_unnamed_addr(true);
        global_str_struct.set_linkage(LlvmLinkage::LinkOnceODR);
        global_str_struct.set_visibility(inkwell::GlobalVisibility::Hidden);

        Ok(global_str_struct)
    }

    fn codegen_string_id_to_global(&mut self, string_id: StringId) -> CgResult<GlobalValue<'ctx>> {
        if let Some(cached_string) = self.strings.get(&string_id) {
            Ok(*cached_string)
        } else {
            let ptr = self.make_string_llvm_global(string_id)?;
            self.strings.insert(string_id, ptr);
            Ok(ptr)
        }
    }

    fn make_buffer_struct(
        &mut self,
        struct_type: StructType<'ctx>,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> StructValue<'ctx> {
        let buffer_struct_value = struct_type.const_named_struct(&[
            data.as_basic_value_enum(),
            self.ctx.i64_type().const_int(len, false).as_basic_value_enum(),
        ]);
        buffer_struct_value
    }

    fn make_span_struct(
        &mut self,
        span_type_id: TypeId,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> StructValue<'ctx> {
        let buffer_type_id = self
            .k1
            .mem
            .get_nth_lt(self.k1.types.get(span_type_id).expect_struct().fields, 0)
            .type_id;
        let buffer_pt = self.k1.get_physical_type_computed(buffer_type_id).unwrap();
        let buffer_cg_type = self.codegen_type(buffer_pt).expect_struct();
        self.make_buffer_struct(buffer_cg_type.struct_type, len, data)
    }

    fn make_list_struct(
        &mut self,
        list_type_id: TypeId,
        buffer_struct: StructValue<'ctx>,
        len: u64,
    ) -> StructValue<'ctx> {
        let list_pt = self.k1.get_physical_type_computed(list_type_id).unwrap();
        let list_cg_type = self.codegen_type(list_pt).expect_struct();
        list_cg_type.struct_type.const_named_struct(&[
            // data
            buffer_struct.as_basic_value_enum(),
            // capacity
            self.ctx.i64_type().const_int(len, false).as_basic_value_enum(),
        ])
    }

    pub fn name(&self) -> &str {
        self.k1.program_name()
    }

    fn initialize_targets() {
        Target::initialize_x86(&InitializationConfig::default());
        Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_webassembly(&InitializationConfig::default());
    }

    fn target_cpu_features(k1_target: compiler::Target) -> (String, String) {
        let is_native = compiler::detect_host_target() == Some(k1_target);
        if is_native {
            (
                TargetMachine::get_host_cpu_name().to_string(),
                TargetMachine::get_host_cpu_features().to_string(),
            )
        } else {
            match k1_target.arch() {
                // SSE2 is the x86-64 baseline
                compiler::Arch::Intel => ("x86-64".to_string(), "".to_string()),
                compiler::Arch::Arm => ("generic".to_string(), "+neon".to_string()),
                compiler::Arch::Wasm => (
                    "generic".to_string(),
                    // bulk-memory lets llvm.memcpy/memmove/memset lower to
                    // memory.copy/memory.fill instead of libc calls
                    "+simd128,+bulk-memory,+sign-ext,+mutable-globals,+nontrapping-fptoint"
                        .to_string(),
                ),
            }
        }
    }

    pub fn make_target_machine(optimize: bool, k1_target: compiler::Target) -> TargetMachine {
        // Bare targets ride the ELF triples: their object is consumed by a
        // kernel or embedder toolchain, never by mac userland
        let triple = match k1_target {
            compiler::Target::Wasm64Wasi => {
                inkwell::targets::TargetTriple::create("wasm64-unknown-wasi")
            }
            compiler::Target::Wasm64Bare => {
                inkwell::targets::TargetTriple::create("wasm64-unknown-unknown")
            }
            compiler::Target::Intel64Linux | compiler::Target::Intel64Bare => {
                inkwell::targets::TargetTriple::create("x86_64-unknown-linux-gnu")
            }
            compiler::Target::Arm64Bare => {
                inkwell::targets::TargetTriple::create("aarch64-unknown-linux-gnu")
            }
            compiler::Target::Arm64Macos => inkwell::targets::TargetTriple::create(&format!(
                "arm64-apple-macosx{}",
                compiler::MAC_SDK_VERSION
            )),
        };

        let target = Target::from_triple(&triple).unwrap();
        let (cpu, features) = Cg::target_cpu_features(k1_target);
        let opt_level =
            if !optimize { OptimizationLevel::None } else { OptimizationLevel::Aggressive };
        // PIC wasm is for -shared modules; executables must be non-PIC
        let reloc_mode = if k1_target.arch() == compiler::Arch::Wasm {
            inkwell::targets::RelocMode::Static
        } else {
            inkwell::targets::RelocMode::PIC
        };
        target
            .create_target_machine(
                &triple,
                &cpu,
                &features,
                opt_level,
                reloc_mode,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap()
    }

    pub fn finalize_debug_info(&self) {
        self.debug.debug_builder.finalize();
    }

    pub fn verify(&self) -> CgResult<()> {
        if let Err(err) = self.llvm_module.verify() {
            self.write_failure_file();
            cgbail!(
                SpanId::NONE,
                "Module '{}' failed validation: {}",
                self.unit_name(),
                err.to_string_lossy()
            );
        }
        Ok(())
    }

    fn unit_name(&self) -> String {
        let mut name = self.name().to_string();
        if let CgKind::ReloadDylib(ns_id) = self.kind {
            name.push('.');
            name.push_str(self.k1.ident_str(self.k1.namespaces.get(ns_id).name));
        }
        if self.multi_unit() {
            name.push_str(&format!(".{}", self.plan.index));
        }
        name
    }

    fn write_failure_file(&self) {
        write_failure_file(&self.llvm_module, &self.unit_name());
    }

    pub fn run_passes(&self, pipeline: Pipeline) {
        run_passes(&self.llvm_module, &self.llvm_machine, pipeline);
    }

    pub fn emit_object_file(&self, path: &str) -> CgResult<()> {
        emit_object(&self.llvm_module, &self.llvm_machine, path)
    }

    pub fn emit_llvm_ir_text(&self) -> String {
        self.llvm_module.print_to_string().to_string()
    }

    pub fn emit_bitcode_to_path(&self, path: impl AsRef<Path>) -> bool {
        self.llvm_module.write_bitcode_to_path(path)
    }

    fn make_enum_attribute(&self, name: &str, value: u64) -> Attribute {
        self.ctx.create_enum_attribute(Attribute::get_named_enum_kind_id(name), value)
    }

    fn make_sret_attributes(&self, return_type: &CgType<'ctx>) -> [Attribute; 4] {
        let layout = return_type.rich_repr_layout();
        [
            self.ctx.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                return_type.rich_type().as_any_type_enum(),
            ),
            self.make_enum_attribute("align", layout.align as u64),
            self.make_enum_attribute("noalias", 0),
            self.make_enum_attribute("dereferenceable", layout.size as u64),
        ]
    }

    fn make_byval_attributes(&self, param_type: &CgType<'ctx>) -> [Attribute; 2] {
        [
            self.ctx.create_type_attribute(
                Attribute::get_named_enum_kind_id("byval"),
                param_type.rich_type().as_any_type_enum(),
            ),
            self.make_enum_attribute("align", param_type.rich_repr_layout().align as u64),
        ]
    }
}
