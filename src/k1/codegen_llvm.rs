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
    AsDIScope, DICompileUnit, DIEnumerator, DIExpression, DIFile, DILocalVariable, DILocation,
    DIScope, DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{
    AnyType, ArrayType, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType,
    VectorType as LlvmVectorType,
};
use inkwell::values::{
    AggregateValue, ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum,
    CallSiteValue, FloatValue, FunctionValue, GlobalValue, InstructionValue, IntValue,
    MetadataValue, PhiValue, PointerValue, StructValue, ValueKind,
};
use inkwell::{
    AddressSpace, AtomicOrdering, FloatPredicate, IntPredicate, OptimizationLevel, ThreadLocalMode,
};
use llvm_sys::debuginfo::LLVMDIBuilderInsertDbgValueRecordAtEnd;
use llvm_sys::debuginfo::LLVMDIBuilderInsertDeclareRecordAtEnd;

use log::{debug, trace};

use crate::arith::{FloatOp, FloatUnop, IntOp, OverflowOp};
use crate::compiler::{self};
use crate::ir::{
    BackendBuiltin, BlockId, IdMap, Inst, InstId, IrCallee, PhysicalFunctionType, Value,
};
use crate::kmem::{List, MSlice};
use crate::lex::Span;
use crate::parse::{FileId, NumericWidth, StringId};
use crate::perfect_hash::{self, PerfectHash};
use crate::typer::types::{
    AbiMode, AggType, AggregateTypeId, Layout, NamedScalarId, PhysicalType, PhysicalTypeEnum,
    PhysicalTypeResult, RegisterClass, ScalarType, StructField, Type, TypeDefnInfo, TypeId,
};
use crate::typer::{
    FunctionId, K1Result, Linkage as TyperLinkage, NamespaceId, StaticContainerKind,
    StaticRawContainer, StaticValue, StaticValueId, TypedFloatValue, TypedGlobalId, TypedIntValue,
    TypedProgram,
};
use crate::{SV4, SV8, ir, kbail, kmem};

fn llvm_size_info(td: &TargetData, typ: &dyn AnyType) -> Layout {
    Layout { size: td.get_abi_size(typ) as u32, align: td.get_abi_alignment(typ) }
}

fn llvm_float_constant<'ctx>(ctx: &'ctx Context, value: TypedFloatValue) -> FloatValue<'ctx> {
    let (bits, float_type) = match value {
        TypedFloatValue::F32(v) => {
            (ctx.i32_type().const_int(v.to_bits() as u64, false), ctx.f32_type())
        }
        TypedFloatValue::F64(v) => (ctx.i64_type().const_int(v.to_bits(), false), ctx.f64_type()),
    };
    unsafe {
        FloatValue::new(llvm_sys::core::LLVMConstBitCast(
            bits.as_value_ref(),
            float_type.as_type_ref(),
        ))
    }
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
    abi: AbiMode,
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
    InVectorRegisters {
        element: ScalarType,
        lanes: u32,
        count: u32,
    },
    /// How clang does X86 9-16 byte structs
    StructByEightbytePair {
        class1: RegisterClass,
        class2: RegisterClass,
        active_bits2: u32,
    },
    /// How clang does ARM64 9-16 byte structs
    StructByIntPairArray,
    BigStructByPtrToCopy {
        byval_attr: bool,
    },
    BigStructByPtr,
}

#[derive(Copy, Clone)]
struct LlvmScalarType<'ctx> {
    pt: PhysicalType,
    basic_type: BasicTypeEnum<'ctx>,
    di_type: Option<DIType<'ctx>>,
    layout: Layout,
    load_metadata: [Option<(u32, MetadataValue<'ctx>)>; 2],
    internal_abi_attributes: [Option<Attribute>; 2],
    native_abi_attribute: Option<Attribute>,
}

#[derive(Clone, Copy)]
struct CgStructType<'ctx> {
    pt: PhysicalType,
    struct_type: StructType<'ctx>,
    fields: MSlice<CgType<'ctx>, CgPerm>,
    di_type: Option<DIType<'ctx>>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgArrayType<'ctx> {
    pt: PhysicalType,
    array_type: ArrayType<'ctx>,
    di_type: Option<DIType<'ctx>>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgVectorType<'ctx> {
    pt: PhysicalType,
    vector_type: LlvmVectorType<'ctx>,
    di_type: Option<DIType<'ctx>>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgUnionType<'ctx> {
    pt: PhysicalType,
    aligned_repr: BasicTypeEnum<'ctx>,
    layout: Layout,
    di_type: Option<DIType<'ctx>>,
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
            CgType::Union(u) => u.aligned_repr.as_basic_type_enum(),
        }
    }

    fn debug_type(&self) -> Option<DIType<'ctx>> {
        match self {
            CgType::Scalar(value) => value.di_type,
            CgType::StructType(s) => s.di_type,
            CgType::ArrayType(a) => a.di_type,
            CgType::Vector(v) => v.di_type,
            CgType::Union(u) => u.di_type,
        }
    }
}

struct BuiltinTypes<'ctx> {
    scalars: [LlvmScalarType<'ctx>; ScalarType::ALL.len()],
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
    fn scalar(&self, st: ScalarType) -> LlvmScalarType<'ctx> {
        self.scalars[st.index()]
    }

    fn empty_struct_value(&self) -> BasicValueEnum<'ctx> {
        self.empty_struct.get_undef().as_basic_value_enum()
    }
}

pub struct CgFunction<'ctx> {
    pub function_type: CgFunctionType<'ctx>,
    pub function_value: FunctionValue<'ctx>,
    /// These are canonical, not ABI-mapped, and also logical, as in,
    /// sret is excluded, so the first item is the first param the function actually takes
    pub param_values: Vec<BasicValueEnum<'ctx>>,
    pub last_alloca_instr: Option<InstructionValue<'ctx>>,
    pub returned_sret_variable: Option<InstId>,
    pub return_block: Option<(BasicBlock<'ctx>, SV4<PhiValue<'ctx>>)>,
    pub debug_info: DISubprogram<'ctx>,
    pub debug_file: DIFile<'ctx>,
}

pub struct CgPerm;
pub struct CgTmp;

#[derive(Debug)]
pub struct CgError {
    pub message: String,
    pub span: Span,
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

/// One codegen unit's clock ticks, recorded on its worker thread
pub struct UnitTiming {
    pub index: usize,
    pub fn_count: usize,
    pub clock_start: u64,
    pub clock_generated: u64,
    pub clock_passed: u64,
    pub clock_end: u64,
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
    llvm_named_scalars: FxHashMap<NamedScalarId, LlvmScalarType<'ctx>>,
    globals: FxHashMap<TypedGlobalId, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    strings: FxHashMap<StringId, GlobalValue<'ctx>>,
    static_values_basics: FxHashMap<StaticValueId, BasicValueEnum<'ctx>>,
    static_values_globals: FxHashMap<StaticValueId, GlobalValue<'ctx>>,
    debug: DebugContext<'ctx>,
    tmp: kmem::Mem<CgTmp>,
    mem: kmem::Mem<CgPerm>,

    current_insert_function: FunctionId,
    cur_unit: ir::UnitView<'static>,
    cur_blocks: IdMap<BlockId, BasicBlock<'ctx>>,
    last_debug_location: std::cell::Cell<Option<(Span, DILocation<'ctx>)>>,

    buffers: CgBuffers<'ctx>,
}

struct CgBuffers<'ctx> {
    cfg_seen: IdMap<BlockId, ()>,
    cfg_blocks_rpo: Vec<BlockId>,
    pending_phis: Vec<(PhiValue<'ctx>, BlockId, InstId, Option<DILocation<'ctx>>)>,
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

    fn push_scope(&mut self, span: Span, scope: DIScope<'ctx>, file: DIFile<'ctx>) {
        self.debug_stack.push(DebugStackEntry { span, scope, file });
    }
    fn pop_scope(&mut self) {
        self.debug_stack.pop();
    }
    fn current_entry(&self) -> &DebugStackEntry<'ctx> {
        self.debug_stack.last().unwrap()
    }
    fn current_span(&self) -> Span {
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
    span: Span,
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
            "always-inline,function(sroa,instcombine<no-verify-fixpoint;max-iterations=1>,simplifycfg),mergefunc"
        }
    };
    module.run_passes(text, machine, options).unwrap();
}

pub fn emit_object(module: &LlvmModule, machine: &TargetMachine, path: &str) -> CgResult<()> {
    if let Err(e) =
        machine.write_to_file(module, inkwell::targets::FileType::Object, Path::new(path))
    {
        cgbail!(Span::NONE, "Error writing object file to path {path}: {}", e.to_string_lossy());
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
    ) -> DebugContext<'ctx> {
        let build = module.plan.config;
        // We may need to create a DIBuilder per-file.
        // For now let's use main file
        let source = module.ast.sources.get_main();
        let is_macos = module.plan.config.target.platform() == compiler::Platform::PosixMacos;
        let sysroot = if is_macos { compiler::MAC_SDK_SYSROOT } else { "" };
        let sdk = if is_macos { "MacOSX.sdk" } else { "" };
        let (debug_builder, compile_unit) = llvm_module.create_debug_info_builder(
            false,
            DWARFSourceLanguage::C,
            source.filename_str(&module.ast.idents),
            source.directory_str(&module.ast.idents),
            "k1_compiler",
            build.optimize,
            "",
            0,
            "",
            if build.debug { DWARFEmissionKind::Full } else { DWARFEmissionKind::LineTablesOnly },
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
        if module.plan.is_executable() {
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
            line_tables_only: !build.debug,
        };
        debug.push_scope(Span::NONE, compile_unit.as_debug_info_scope(), compile_unit.get_file());
        debug
    }

    pub fn create(
        ctx: &'ctx Context,
        k1: &'module TypedProgram,
        kind: CgKind,
        plan: UnitPlan,
    ) -> Self {
        let builder = ctx.create_builder();
        let char_type = ctx.i8_type();
        let llvm_module = ctx.create_module(k1.program_name());
        llvm_module.set_source_file_name(k1.ast.sources.get_main().filename_str(&k1.ast.idents));

        let debug_context = Cg::init_debug(ctx, &llvm_module, k1);

        Cg::initialize_targets();
        let machine = Cg::make_target_machine(k1);
        let target_data = machine.get_target_data();
        llvm_module.set_data_layout(&target_data.get_data_layout());
        llvm_module.set_triple(&machine.get_triple());

        if !k1.config.tools.emit_llvm {
            unsafe {
                llvm_sys::core::LLVMContextSetDiscardValueNames(ctx.as_ctx_ref(), 1);
            }
        }

        let ptr = ctx.ptr_type(AddressSpace::default());
        let scalars = std::array::from_fn(|i| {
            Cg::make_scalar_type(ctx, &debug_context, ptr, ScalarType::ALL[i])
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
            llvm_named_scalars: FxHashMap::new(),
            builtin_types,
            strings: FxHashMap::new(),
            static_values_basics: FxHashMap::new(),
            static_values_globals: FxHashMap::new(),
            debug: debug_context,
            tmp: kmem::Mem::make(),
            mem: kmem::Mem::make(),

            current_insert_function: FunctionId::PENDING,
            cur_unit: ir::UnitView::EMPTY,
            cur_blocks: IdMap::default(),
            last_debug_location: std::cell::Cell::new(None),

            buffers: CgBuffers {
                cfg_seen: IdMap::default(),
                cfg_blocks_rpo: Vec::with_capacity(16),
                pending_phis: Vec::new(),
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
                        let shared =
                            !g.linkage.is_exported() && !self.has_reloadable_fns && g.is_constant;
                        if g.linkage.is_exported()
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

        let mut inst_mappings = IdMap::default();
        while let Some(fn_id) = self.functions_pending_body_compilation.pop() {
            self.codegen_function_body(&mut inst_mappings, fn_id)?;
        }

        if let Some((main_function_id, function_value)) = main_function {
            self.builder.unset_current_debug_location();
            let is_wasi = self.k1.plan.config.target.platform() == compiler::Platform::Wasi;
            let (entrypoint_name, entrypoint_fn_type) = if is_wasi {
                // WASI rejects any entry signature other than void _start()
                if !function_value.get_type().get_param_types().is_empty() {
                    cgbail!(Span::NONE, "main with parameters is not supported on wasm");
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
            let main_call = self.build_call(function_value, &params);
            let res = main_call.try_as_basic_value().basic();
            let i32_type = self.ctx.i32_type();
            let exit_code = match res {
                None => i32_type.const_zero(),
                Some(v) => {
                    let ret_type = self.k1.get_function_type(main_function_id).return_type;
                    let signed = self.k1.types.get(ret_type).expect_integer().is_signed();
                    self.builder
                        .build_int_cast_sign_flag(v.into_int_value(), i32_type, signed, "")
                        .unwrap()
                }
            };
            let exit_fv = program_exit_value.unwrap();
            self.build_call(exit_fv, &[exit_code.into()]);
            self.builder.build_unreachable().unwrap();
        }

        if first_unit && self.kind == CgKind::Host && self.has_reloadable_fns {
            self.emit_reload_descriptors();
        }

        Ok(())
    }

    fn prepare_roots(
        k1: &mut TypedProgram,
        roots: &mut Vec<FunctionId>,
    ) -> K1Result<Vec<FunctionId>> {
        let mut compiler_required_k1_fns = vec!["crash-div"];
        if k1.namespaces.iter().any(|ns| ns.reload) {
            compiler_required_k1_fns.push("crash-unloaded-ns");
        }
        if !k1.type_infos.is_empty() {
            compiler_required_k1_fns.push("crash-missing-type-info");
        }
        for name in compiler_required_k1_fns {
            let Some(id) = Cg::find_k1_ns_fn(k1, name) else {
                kbail!(k1, Span::NONE, "core is missing fn k1/{name}");
            };
            roots.push(id);
        }
        let reachable = ir::compile_reachable(k1, roots)?;
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
        let reachable = Cg::prepare_roots(k1, &mut roots)?;
        Ok(CodegenRoots { main: None, program_exit: None, reachable })
    }

    pub fn prepare_host(k1: &mut TypedProgram) -> K1Result<CodegenRoots> {
        let mut roots = ir::get_program_roots(k1)?;
        let reachable = Cg::prepare_roots(k1, &mut roots.functions)?;
        Ok(CodegenRoots { main: roots.main, program_exit: roots.program_exit, reachable })
    }

    pub fn plan_units(
        k1: &TypedProgram,
        reachable: &[FunctionId],
        max_units: usize,
    ) -> Vec<UnitPlan> {
        const MIN_UNIT_INSTRUCTIONS: u64 = 2 * 1024;
        let size_of = |function_id: &FunctionId| -> u64 {
            match k1.ir.function_unit(*function_id) {
                Some(unit) => unit.inst_count() as u64 + 1,
                None => 0,
            }
        };
        let mut total: u64 = 0;
        for function_id in reachable {
            total += size_of(function_id);
        }
        let count = ((total / MIN_UNIT_INSTRUCTIONS) as usize).clamp(1, max_units.max(1));
        let count_u64 = count as u64;
        let mut by_id: Vec<FunctionId> = reachable.to_vec();
        by_id.sort_by_key(|f| f.as_u32());
        let mut chunks: Vec<Vec<FunctionId>> = Vec::with_capacity(count);
        let mut functions: Vec<FunctionId> = Vec::with_capacity(reachable.len() / count + 1);
        let mut cumulative: u64 = 0;
        for function_id in &by_id {
            functions.push(*function_id);
            cumulative += size_of(function_id);
            let boundary = (chunks.len() as u64 + 1) * total;
            if chunks.len() + 1 < count && cumulative * count_u64 >= boundary {
                chunks.push(std::mem::take(&mut functions));
            }
        }
        chunks.push(functions);
        let count = chunks.len();
        let mut plans: Vec<UnitPlan> = Vec::with_capacity(count);
        for (index, functions) in chunks.into_iter().enumerate() {
            plans.push(UnitPlan { index, count, functions });
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
        mut plans: Vec<UnitPlan>,
        kind: CgKind,
        output: UnitOutput,
        object_path: impl Fn(usize) -> String + Sync,
    ) -> CgResult<(Vec<UnitArtifact>, Vec<UnitTiming>)> {
        Cg::initialize_targets();
        let shared = SharedProgram(k1);
        let shared = &shared;
        let output = &output;
        let object_path = &object_path;
        let unit_count = plans.len();
        let timings: std::sync::Mutex<Vec<UnitTiming>> =
            std::sync::Mutex::new(Vec::with_capacity(unit_count));
        let timings = &timings;
        let worker_count = unit_count.min(crate::get_thread_limit()).max(1);
        plans.reverse();
        let queue = std::sync::Mutex::new(plans);
        let queue = &queue;
        let mut artifacts: Vec<Option<UnitArtifact>> = Vec::with_capacity(unit_count);
        for _ in 0..unit_count {
            artifacts.push(None);
        }
        let artifacts = std::sync::Mutex::new(artifacts);
        let artifacts = &artifacts;
        let work = move || -> CgResult<()> {
            loop {
                let Some(plan) = queue.lock().unwrap().pop() else { return Ok(()) };
                let clock = shared.0.trace.clock;
                let clock_start = clock.raw();
                let index = plan.index;
                let ctx = Context::create();
                let mut cg = Cg::create(&ctx, shared.0, kind, plan);
                cg.codegen_program(roots)?;
                let clock_generated = clock.raw();
                cg.finalize_debug_info();
                if cfg!(debug_assertions) {
                    cg.verify()?;
                }
                let clock_passed;
                let artifact = match output {
                    UnitOutput::Object(pipeline) => {
                        cg.run_passes(*pipeline);
                        clock_passed = clock.raw();
                        let path = object_path(index);
                        cg.emit_object_file(&path)?;
                        UnitArtifact::Object(path)
                    }
                    UnitOutput::Bitcode(pipeline) => {
                        cg.run_passes(*pipeline);
                        clock_passed = clock.raw();
                        let bytes = if *pipeline == Pipeline::ThinLtoPreLink {
                            cg.thinlto_bitcode()
                        } else {
                            cg.llvm_module.write_bitcode_to_memory().as_slice().into()
                        };
                        UnitArtifact::Bitcode {
                            bytes,
                            exported: cg.collect_module_exported_symbols(),
                            referenced: cg.collect_all_module_symbols(),
                        }
                    }
                };
                timings.lock().unwrap().push(UnitTiming {
                    index,
                    fn_count: cg.owned.len(),
                    clock_start,
                    clock_generated,
                    clock_passed,
                    clock_end: clock.raw(),
                });
                artifacts.lock().unwrap()[index] = Some(artifact);
            }
        };
        let mut results = Vec::with_capacity(worker_count);
        if worker_count == 1 {
            results.push(work());
        } else {
            std::thread::scope(|scope| {
                let mut workers = Vec::with_capacity(worker_count);
                for worker_index in 0..worker_count {
                    let worker = std::thread::Builder::new()
                        .stack_size(crate::STACK_SIZE)
                        .name(format!("{}.cg{}", k1.program_name(), worker_index))
                        .spawn_scoped(scope, work)
                        .unwrap();
                    workers.push(worker);
                }
                for worker in workers {
                    results.push(match worker.join() {
                        Ok(result) => result,
                        Err(_) => Err(cgerr!(Span::NONE, "a codegen unit panicked")),
                    });
                }
            });
        }
        for result in results {
            result?;
        }
        let mut done = Vec::with_capacity(unit_count);
        let artifacts = std::mem::take(&mut *artifacts.lock().unwrap());
        for artifact in artifacts {
            done.push(artifact.expect("every unit produces an artifact"));
        }
        let timings = std::mem::take(&mut *timings.lock().unwrap());
        Ok((done, timings))
    }

    fn thinlto_bitcode(&self) -> Box<[u8]> {
        let buffer = unsafe {
            inkwell::memory_buffer::MemoryBuffer::new(k1_thinlto_bitcode(
                self.llvm_module.as_mut_ptr(),
            ))
        };
        buffer.as_slice().into()
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
            let buffer = inkwell::memory_buffer::MemoryBuffer::create_from_memory_range_copy(
                bytes,
                &format!("{name}.{i}"),
            );
            let module = match LlvmModule::parse_bitcode_from_buffer(&buffer, ctx) {
                Ok(module) => module,
                Err(e) => cgbail!(Span::NONE, "Unit {i} bitcode failed to parse: {}", e),
            };
            match &merged {
                None => merged = Some(module),
                Some(base) => {
                    if let Err(e) = base.link_in_module(module) {
                        cgbail!(Span::NONE, "Merging unit {i}: {}", e);
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
        let mut units: Vec<K1ThinLtoUnit> = Vec::with_capacity(artifacts.len());
        let mut preserved: Vec<std::ffi::CString> = vec![];
        let mut cross_referenced: Vec<std::ffi::CString> = vec![];
        let symbol_prefix = if k1.plan.config.target.platform() == compiler::Platform::PosixMacos {
            "_"
        } else {
            ""
        };
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
        let (cpu, features) = Cg::llvm_cpu_features(k1);
        let cpu = std::ffi::CString::new(cpu).unwrap();
        let features = std::ffi::CString::new(features).unwrap();
        let pic = k1.plan.config.target.arch() != compiler::Arch::Wasm;
        let cache_dir = if k1.config.tools.cache {
            let dir = format!("{}/thinlto", k1.ast.idents.get_string(k1.config.cache_dir));
            if let Err(e) = std::fs::create_dir_all(&dir) {
                cgbail!(Span::NONE, "Failed to create ThinLTO cache dir {dir}: {e}");
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
            cgbail!(Span::NONE, "{e}");
        }
        if code != 0 {
            cgbail!(Span::NONE, "ThinLTO failed with code {code}");
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
        let dylib_ext = self.k1.plan.config.target.platform().dylib_ext();
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

    fn find_k1_ns_fn(k1: &TypedProgram, name: &str) -> Option<FunctionId> {
        let ident = k1.ast.idents.lookup(name)?;
        k1.scopes.find_function_local(k1.scopes.k1_scope_id, ident)
    }

    fn k1_ns_fn_value(&mut self, name: &str) -> CgResult<FunctionValue<'ctx>> {
        let Some(function_id) = Cg::find_k1_ns_fn(self.k1, name) else {
            cgbail!(Span::NONE, "core is missing fn k1/{name}");
        };
        self.declare_llvm_function(function_id)
    }

    fn build_call(
        &self,
        callee: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CallSiteValue<'ctx> {
        let call = self.builder.build_call(callee, args, "").unwrap();
        call.set_call_convention(callee.get_call_conventions());
        call
    }

    fn int_div_helper(
        &mut self,
        int_type: IntType<'ctx>,
        signed: bool,
        rem: bool,
    ) -> CgResult<FunctionValue<'ctx>> {
        let name = format!(
            "__k1_{}{}_i{}",
            if signed { "s" } else { "u" },
            if rem { "rem" } else { "div" },
            int_type.get_bit_width()
        );
        if let Some(f) = self.llvm_module.get_function(&name) {
            return Ok(f);
        }
        let crash_fn = self.k1_ns_fn_value("crash-div")?;
        let fn_type = int_type.fn_type(&[int_type.into(), int_type.into()], false);
        let f = self.llvm_module.add_function(&name, fn_type, Some(LlvmLinkage::Private));
        f.add_attribute(AttributeLoc::Function, self.make_enum_attribute("alwaysinline", 0));
        f.add_attribute(AttributeLoc::Function, self.make_enum_attribute("nounwind", 0));
        let saved_block = self.builder.get_insert_block();
        let saved_loc = self.builder.get_current_debug_location();
        self.builder.unset_current_debug_location();

        let entry_block = self.ctx.append_basic_block(f, "entry");
        self.builder.position_at_end(entry_block);
        let lhs = f.get_nth_param(0).unwrap().into_int_value();
        let rhs = f.get_nth_param(1).unwrap().into_int_value();
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, rhs, int_type.const_zero(), "div_zero")
            .unwrap();
        let bad = if signed {
            let min = int_type.const_int(1u64 << (int_type.get_bit_width() - 1), false);
            let is_neg_one = self
                .builder
                .build_int_compare(IntPredicate::EQ, rhs, int_type.const_all_ones(), "")
                .unwrap();
            let is_min = self.builder.build_int_compare(IntPredicate::EQ, lhs, min, "").unwrap();
            let overflow = self.builder.build_and(is_neg_one, is_min, "div_overflow").unwrap();
            self.builder.build_or(is_zero, overflow, "div_bad").unwrap()
        } else {
            is_zero
        };
        let crash_block = self.ctx.append_basic_block(f, "crash");
        let ok_block = self.ctx.append_basic_block(f, "ok");
        self.builder.build_conditional_branch(bad, crash_block, ok_block).unwrap();

        self.builder.position_at_end(crash_block);
        let is_zero_bool = self.i1_to_bool(is_zero, "");
        self.build_call(crash_fn, &[is_zero_bool.into()]);
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(ok_block);
        let result = match (signed, rem) {
            (false, false) => self.builder.build_int_unsigned_div(lhs, rhs, "").unwrap(),
            (true, false) => self.builder.build_int_signed_div(lhs, rhs, "").unwrap(),
            (false, true) => self.builder.build_int_unsigned_rem(lhs, rhs, "").unwrap(),
            (true, true) => self.builder.build_int_signed_rem(lhs, rhs, "").unwrap(),
        };
        self.builder.build_return(Some(&result)).unwrap();

        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        if let Some(loc) = saved_loc {
            self.builder.set_current_debug_location(loc);
        }
        Ok(f)
    }

    fn build_checked_int_div(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        signed: bool,
        rem: bool,
    ) -> CgResult<IntValue<'ctx>> {
        let helper = self.int_div_helper(lhs.get_type(), signed, rem)?;
        let call = self.build_call(helper, &[lhs.into(), rhs.into()]);
        Ok(call.try_as_basic_value().basic().unwrap().into_int_value())
    }

    fn build_shift_count(&self, count: IntValue<'ctx>, subject: IntValue<'ctx>) -> IntValue<'ctx> {
        let count = self.cast_int_to_match(count, subject, "shift_magnitude_adjust");
        let width = subject.get_type().get_bit_width() as u64;
        let mask = subject.get_type().const_int(width - 1, false);
        self.builder.build_and(count, mask, "shift_count").unwrap()
    }

    /// `ptr __k1_reload_global_load(slot, ns-name, global-name)`: acquire-load
    /// the slot, crash-unloaded-ns on null, else return the storage address.
    /// One private copy per module; every cross-unit global access calls it,
    /// so each access observes the newest swap.
    fn reload_global_load_helper(&mut self) -> CgResult<FunctionValue<'ctx>> {
        if let Some(f) = self.llvm_module.get_function("__k1_reload_global_load") {
            return Ok(f);
        }
        let crash_fn = self.k1_ns_fn_value("crash-unloaded-ns")?;
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
        self.build_call(crash_fn, &[ns_name_param.into(), global_name_param.into()]);
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
        let call = self.build_call(
            helper,
            &[slot.as_pointer_value().into(), ns_cstr.into(), name_cstr.into()],
        );
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
        let not_loaded_fv = self.k1_ns_fn_value("crash-unloaded-ns")?;
        let ns_cstr = self.cstring_constant(&ns_name);
        let fn_cstr = self.cstring_constant(&fn_name);
        self.build_call(not_loaded_fv, &[ns_cstr.into(), fn_cstr.into()]);
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

    fn codegen_global_addr(&mut self, global_id: TypedGlobalId) -> CgResult<BasicValueEnum<'ctx>> {
        let addr = self.codegen_global(global_id)?.as_pointer_value();
        if !(self.k1.globals.get(global_id).is_tls && self.target_supports_tls()) {
            return Ok(addr.as_basic_value_enum());
        }
        let threadlocal_address = Intrinsic::find("llvm.threadlocal.address")
            .unwrap()
            .get_declaration(&self.llvm_module, &[self.builtin_types.ptr.as_basic_type_enum()])
            .unwrap();
        let call = self.builder.build_call(threadlocal_address, &[addr.into()], "").unwrap();
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => Ok(v),
            ValueKind::Instruction(_) => unreachable!("llvm.threadlocal.address returns ptr"),
        }
    }

    fn codegen_global(&mut self, global_id: TypedGlobalId) -> CgResult<GlobalValue<'ctx>> {
        if let Some(g) = self.globals.get(&global_id) {
            return Ok(*g);
        }
        let global = self.k1.globals.get(global_id).clone();

        if let Some(reload_ns) = global.reload_ns {
            if self.kind != CgKind::ReloadDylib(reload_ns) {
                cgbail!(
                    global.span,
                    "ICE: reload-ns global reached codegen_global outside its dylib; \
                     accesses must go through the addr slot"
                );
            }
        }
        let owned_here = global.reload_ns.is_some();
        let name = if owned_here {
            self.make_reloadable_global_symbol(global_id)
        } else {
            self.k1.global_link_symbol(&global)
        };

        let is_dylib = matches!(self.kind, CgKind::ReloadDylib(_));
        let is_private = !global.linkage.is_exported() && !self.has_reloadable_fns;
        let shared = is_private && global.is_constant;
        let defined_elsewhere = !shared && self.multi_unit() && !self.is_first_unit();

        // A reloadable dylib defines only the globals of its own ns; every other
        // global is a declaration bound to the host's copy at dlopen
        let llvm_global =
            if global.linkage.is_external() || (is_dylib && !owned_here) || defined_elsewhere {
                let PhysicalTypeResult::Yes(global_pt) =
                    self.k1.get_physical_type_computed(global.type_id)
                else {
                    cgbail!(global.span, "ICE: Not physical; reject this in typer");
                };
                let basic_type = self.codegen_type(global_pt);
                let g =
                    self.make_external_global(basic_type.rich_type(), &name, global.is_constant);
                if global.is_tls && self.target_supports_tls() {
                    g.set_thread_local(true);
                    let mode = if defined_elsewhere && self.k1.plan.is_executable() {
                        ThreadLocalMode::LocalExecTLSModel
                    } else {
                        ThreadLocalMode::GeneralDynamicTLSModel
                    };
                    g.set_thread_local_mode(Some(mode));
                }
                if is_private && !global.linkage.is_external() {
                    g.set_visibility(inkwell::GlobalVisibility::Hidden);
                }
                g
            } else {
                let initial_static_value_id = global.initial_value.as_value().unwrap();
                let initializer_basic_value = self.codegen_static_value_as_const(
                    initial_static_value_id,
                    0,
                    !global.is_constant,
                )?;

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

    fn get_debug_location_from_span(&self, span: Span) -> DILocation<'ctx> {
        let source = self.k1.ast.sources.source_by_span(span);
        let line =
            source.get_line_for_span_start(&self.k1.ast.mem, span).expect("No line for span");
        let column = source.local_from_global_offset(span.global_start) + 1 - line.start_char;
        let locn = self.debug.debug_builder.create_debug_location(
            self.ctx,
            line.line_index + 1,
            column,
            self.debug.current_scope(),
            None,
        );
        locn
    }
    fn set_debug_location_from_span(&self, span: Span) {
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

    fn get_line_number(&self, span: Span) -> u32 {
        if span.is_none() {
            return 0;
        }
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
    const DW_ATE_FLOAT: u32 = 0x04;
    const DW_ATE_SIGNED: u32 = 0x05;
    const DW_ATE_UNSIGNED: u32 = 0x07;
    const DW_ATE_UNSIGNED_CHAR: u32 = 0x08;

    fn make_scalar_type(
        ctx: &'ctx Context,
        debug: &DebugContext<'ctx>,
        ptr: PointerType<'ctx>,
        st: ScalarType,
    ) -> LlvmScalarType<'ctx> {
        let layout = st.get_layout();
        let encoding = match st {
            ScalarType::I8 | ScalarType::I16 | ScalarType::I32 | ScalarType::I64 => {
                Self::DW_ATE_SIGNED
            }
            ScalarType::F32 | ScalarType::F64 => Self::DW_ATE_FLOAT,
            ScalarType::Pointer => Self::DW_ATE_ADDRESS,
        };
        let name: &'static str = st.into();
        let basic_type: BasicTypeEnum<'ctx> = match st {
            ScalarType::I8 => ctx.i8_type().into(),
            ScalarType::I16 => ctx.i16_type().into(),
            ScalarType::I32 => ctx.i32_type().into(),
            ScalarType::I64 => ctx.i64_type().into(),
            ScalarType::F32 => ctx.f32_type().into(),
            ScalarType::F64 => ctx.f64_type().into(),
            ScalarType::Pointer => ptr.into(),
        };
        let di_type = if debug.line_tables_only {
            None
        } else {
            Some(
                debug
                    .debug_builder
                    .create_basic_type(name, layout.size_bits(), encoding, 0)
                    .unwrap()
                    .as_type(),
            )
        };
        LlvmScalarType {
            pt: PhysicalType::scalar(st),
            basic_type,
            layout,
            di_type,
            load_metadata: [None, None],
            internal_abi_attributes: [None, None],
            native_abi_attribute: None,
        }
    }

    fn codegen_type(&mut self, pt: PhysicalType) -> CgType<'ctx> {
        //eprintln!("codegen_type {}", self.k1.pt_to_string(pt));
        match pt.as_enum() {
            PhysicalTypeEnum::Scalar(st) => match pt.named_scalar() {
                None => self.builtin_types.scalar(st).into(),
                Some(id) => self.codegen_named_scalar(st, id).into(),
            },
            PhysicalTypeEnum::Agg(agg_id) => {
                if let Some(k1) = self.llvm_types.get(&agg_id) {
                    return *k1;
                }
                let agg = self.k1.agg_types.get(agg_id);
                let agg_layout = agg.layout;
                let full_debug = !self.debug.line_tables_only;
                let cg_type = match agg.agg_type {
                    AggType::Struct { fields, packed } => {
                        let mut cg_field_types = self.mem.new_list(fields.len());
                        let mut field_rich_types = self.tmp.new_list(fields.len());
                        for phys_field in self.k1.mem.getn(fields) {
                            let cg_type = self.codegen_type(phys_field.field_t);
                            cg_field_types.push(cg_type);
                            field_rich_types.push(cg_type.rich_type());
                        }
                        let cg_field_types = cg_field_types.to_slice();
                        let struct_type = self.ctx.struct_type(&field_rich_types, packed);
                        if cfg!(debug_assertions) {
                            let td = self.llvm_machine.get_target_data();
                            for (i, phys_field) in self.k1.mem.getn(fields).iter().enumerate() {
                                let llvm_offset =
                                    td.offset_of_element(&struct_type, i as u32).unwrap();
                                assert_eq!(
                                    llvm_offset,
                                    phys_field.offset as u64,
                                    "K1/LLVM field offset mismatch in {}, field {}",
                                    self.codegen_type_name(agg.origin_type_id),
                                    i
                                );
                            }
                            let llvm_layout = self.layout_per_llvm(&struct_type);
                            assert_eq!(
                                llvm_layout.size,
                                agg_layout.stride(),
                                "K1/LLVM struct size mismatch in {}",
                                self.codegen_type_name(agg.origin_type_id)
                            );
                        }
                        let di_type = if full_debug {
                            Some(self.make_struct_debug_type(
                                agg.origin_type_id,
                                agg_layout,
                                fields,
                                cg_field_types,
                            ))
                        } else {
                            None
                        };
                        CgType::StructType(CgStructType {
                            pt,
                            struct_type,
                            fields: cg_field_types,
                            di_type,
                            layout: agg_layout,
                        })
                    }
                    AggType::Array { element_pt, len } => {
                        let element_type = self.codegen_type(element_pt);
                        let array_type = element_type.rich_type().array_type(len);
                        let array_layout = self.k1.get_pt_layout(pt);
                        let di_type = element_type.debug_type().map(|element_di_type| {
                            self.debug
                                .debug_builder
                                .create_array_type(
                                    element_di_type,
                                    array_layout.size_bits(),
                                    array_layout.align_bits(),
                                    &[],
                                )
                                .as_type()
                        });
                        CgType::ArrayType(CgArrayType {
                            pt,
                            array_type,
                            di_type,
                            layout: array_layout,
                        })
                    }
                    AggType::Vector { element_pt, len } => {
                        let element_type = self.codegen_type(PhysicalType::scalar(element_pt));
                        let vector_type = self.vec_llvm_type(element_pt, len);
                        let vector_layout = self.k1.get_pt_layout(pt);
                        let di_type = element_type.debug_type().map(|element_di_type| {
                            self.debug
                                .debug_builder
                                .create_array_type(
                                    element_di_type,
                                    vector_layout.size_bits(),
                                    vector_layout.align_bits(),
                                    &[],
                                )
                                .as_type()
                        });
                        CgType::Vector(CgVectorType {
                            pt,
                            vector_type,
                            di_type,
                            layout: vector_layout,
                        })
                    }
                    AggType::Union { members } => {
                        let di_type = if full_debug {
                            let mut di_members = self.tmp.new_list(members.len());
                            for m in self.k1.mem.getn(members) {
                                di_members.push(self.codegen_type(m.ty).debug_type().unwrap());
                            }
                            Some(self.make_union_debug_type(
                                agg.origin_type_id,
                                agg_layout,
                                di_members.as_slice(),
                            ))
                        } else {
                            None
                        };
                        let aligned_repr =
                            self.codegen_opaque_repr(agg_layout).as_basic_type_enum();
                        CgType::Union(CgUnionType { pt, aligned_repr, layout: agg_layout, di_type })
                    }
                    AggType::Sum(e) => {
                        let struct_repr_cg_type =
                            self.codegen_type(PhysicalType::agg(e.struct_repr));

                        struct_repr_cg_type
                    }
                    AggType::Opaque { size, align } => {
                        let layout = Layout { size, align };
                        let aligned_opaque_repr = self.codegen_opaque_repr(layout);
                        let di_type = if full_debug {
                            Some(self.make_union_debug_type(agg.origin_type_id, agg_layout, &[]))
                        } else {
                            None
                        };

                        // For now, we'll call this a 'Union', its just our own type anyway,
                        // arguably it shouldn't even be a Sum since they share so much
                        CgType::Union(CgUnionType {
                            pt,
                            aligned_repr: aligned_opaque_repr.as_basic_type_enum(),
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
                let di_type = if self.debug.line_tables_only {
                    None
                } else {
                    Some(
                        self.debug
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
                            .as_type(),
                    )
                };
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

    fn make_struct_debug_type(
        &mut self,
        type_id: TypeId,
        layout: Layout,
        fields: MSlice<StructField, TypedProgram>,
        cg_fields: MSlice<CgType<'ctx>, CgPerm>,
    ) -> DIType<'ctx> {
        let type_name = self.codegen_type_name(type_id);
        let line_number = self.get_line_number(self.debug.current_span());
        let mut field_di_types = self.tmp.new_list(fields.len());
        for (phys_field, cg_type) in
            self.k1.mem.getn(fields).iter().zip(self.mem.getn_lt(cg_fields))
        {
            let debug_member = self
                .debug
                .debug_builder
                .create_member_type(
                    self.debug.current_scope(),
                    self.k1.ident_str(phys_field.name),
                    self.debug.current_file(),
                    line_number,
                    cg_type.rich_repr_layout().size_bits(),
                    cg_type.rich_repr_layout().align_bits(),
                    phys_field.offset as u64,
                    0,
                    cg_type.debug_type().unwrap(),
                )
                .as_type();
            field_di_types.push(debug_member)
        }
        self.debug
            .debug_builder
            .create_struct_type(
                self.debug.current_scope(),
                &type_name,
                self.debug.current_file(),
                line_number,
                layout.size as u64,
                layout.align,
                0,
                None,
                field_di_types.as_slice(),
                0,
                None,
                &type_name,
            )
            .as_type()
    }

    fn make_union_debug_type(
        &self,
        type_id: TypeId,
        layout: Layout,
        di_members: &[DIType<'ctx>],
    ) -> DIType<'ctx> {
        let type_name = self.codegen_type_name(type_id);
        let line_number = self.get_line_number(self.debug.current_span());
        self.debug
            .debug_builder
            .create_union_type(
                self.debug.current_scope(),
                &type_name,
                self.debug.current_file(),
                line_number,
                layout.size_bits(),
                layout.align_bits(),
                0,
                di_members,
                0,
                &type_name,
            )
            .as_type()
    }

    fn codegen_opaque_repr(&self, expected_layout: Layout) -> StructType<'ctx> {
        // For union types (and actual opaques), we generate a 2-field struct to trick LLVM.

        // Field 1 is a synthetic integer wide enough to force the alignment of the
        // struct, and Field 2 is an array of bytes, ensuring NO padding at all,
        // large enough to get the whole thing to be exactly `size`, with no end padding.

        if expected_layout.align > expected_layout.size && expected_layout.size != 0 {
            panic!(
                "Cannot create overaligned union with align {} > size {}",
                expected_layout.align, expected_layout.size
            );
        }
        let aligner_type = self.custom_width_int_type(expected_layout.align_bits());
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

    fn codegen_named_scalar(&mut self, st: ScalarType, id: NamedScalarId) -> LlvmScalarType<'ctx> {
        if let Some(named) = self.llvm_named_scalars.get(&id) {
            return *named;
        }
        let origin = *self.k1.named_scalars.get(id);
        let (load_metadata, internal_abi_attributes) = if let Type::Reference(r) =
            self.k1.types.get(origin)
        {
            match self.k1.get_layout_nonmut(r.inner_type) {
                Some(inner) if inner.size > 0 => (
                    [
                        Some((
                            self.ctx.get_kind_id("dereferenceable"),
                            self.make_i64_metadata(inner.size as u64),
                        )),
                        Some((
                            self.ctx.get_kind_id("align"),
                            self.make_i64_metadata(inner.align as u64),
                        )),
                    ],
                    self.make_pointee_attributes(inner).map(Some),
                ),
                _ => (
                    [Some((self.ctx.get_kind_id("nonnull"), self.ctx.metadata_node(&[]))), None],
                    [Some(self.make_enum_attribute("nonnull", 0)), None],
                ),
            }
        } else {
            let runs = self.get_type_range_runs(origin, st.width());
            let range = if runs.is_empty() {
                None
            } else {
                Some((self.ctx.get_kind_id("range"), self.make_range_metadata(st.width(), &runs)))
            };
            let range_attribute = match runs.as_slice() {
                [(lo, hi)] => Some(self.make_range_attribute(st.width(), *lo, *hi)),
                _ => None,
            };
            ([range, None], [range_attribute, None])
        };
        let native_abi_attribute = match st.width() {
            NumericWidth::B8 | NumericWidth::B16 if self.k1.is_named_scalar_signed(id) => {
                Some(self.make_enum_attribute("signext", 0))
            }
            NumericWidth::B8 | NumericWidth::B16 => Some(self.make_enum_attribute("zeroext", 0)),
            NumericWidth::B32 | NumericWidth::B64 => None,
        };
        let di_type = if self.debug.line_tables_only {
            None
        } else {
            Some(self.make_named_scalar_debug_type(st, id))
        };
        let named = LlvmScalarType {
            pt: PhysicalType::named(st, id),
            di_type,
            load_metadata,
            internal_abi_attributes,
            native_abi_attribute,
            ..self.builtin_types.scalar(st)
        };
        self.llvm_named_scalars.insert(id, named);
        named
    }

    fn make_named_scalar_debug_type(&mut self, st: ScalarType, id: NamedScalarId) -> DIType<'ctx> {
        let origin = *self.k1.named_scalars.get(id);
        let signed = self.k1.is_named_scalar_signed(id);
        let int_encoding = if signed { Self::DW_ATE_SIGNED } else { Self::DW_ATE_UNSIGNED };
        let layout = st.get_layout();
        let make_basic = |cg: &Self, name: &str, encoding: u32| {
            cg.debug.debug_builder.create_basic_type(name, layout.size_bits(), encoding, 0).unwrap()
        };
        let mut enumerators: SV8<DIEnumerator<'ctx>> = smallvec::smallvec![];
        let (name, int_type) = match *self.k1.types.get(origin) {
            Type::Bool => return make_basic(self, "bool", Self::DW_ATE_BOOLEAN).as_type(),
            Type::Char => return make_basic(self, "char", Self::DW_ATE_UNSIGNED_CHAR).as_type(),
            Type::Integer(int_type) => {
                return make_basic(self, &int_type.to_string(), int_encoding).as_type();
            }
            Type::Reference(_) => return self.builtin_types.scalar(st).di_type.unwrap(),
            Type::Enum(e) => {
                for member in self.k1.mem.getn(e.member_values) {
                    enumerators.push(self.debug.debug_builder.create_enumerator(
                        self.k1.ident_str(member.name),
                        member.int_value.to_u64_bits() as i64,
                        !signed,
                    ));
                }
                (self.codegen_type_name(origin), e.int_type)
            }
            Type::Sum(sum) => {
                for variant in self.k1.mem.getn(sum.variants) {
                    enumerators.push(self.debug.debug_builder.create_enumerator(
                        self.k1.ident_str(variant.name),
                        variant.tag_value.to_u64_bits() as i64,
                        !signed,
                    ));
                }
                (format!("{}.tag", self.codegen_type_name(origin)), sum.tag_type)
            }
            _ => self.k1.ice_span(Span::NONE, "named scalar origin is not a scalar type"),
        };
        let inner = make_basic(self, &int_type.to_string(), int_encoding);
        let line_number = self.get_line_number(self.debug.current_span());
        self.debug
            .debug_builder
            .create_enumeration_type(
                self.debug.current_scope(),
                &name,
                self.debug.current_file(),
                line_number,
                layout.size_bits(),
                layout.align_bits(),
                &enumerators,
                inner.as_type(),
            )
            .as_type()
    }

    fn get_type_range_runs(&self, type_id: TypeId, width: NumericWidth) -> SV4<(u64, u64)> {
        let mut values: SV8<u64> = smallvec::smallvec![];
        match self.k1.types.get(type_id) {
            Type::Bool => {
                values.push(0);
                values.push(1);
            }
            Type::Enum(e) if !e.partial => {
                for member in self.k1.mem.getn(e.member_values) {
                    values.push(member.int_value.to_u64_bits());
                }
            }
            Type::Sum(_) => {
                let agg_id = self.k1.get_physical_type_computed(type_id).unwrap().expect_agg();
                let sum = self.k1.agg_types.get(agg_id).agg_type.expect_sum();
                for variant in self.k1.mem.getn(sum.variants) {
                    values.push(variant.tag.to_u64_bits());
                }
            }
            _ => return smallvec::smallvec![],
        }
        collect_ranges_from_values(width.bits() as u32, &mut values)
    }

    fn make_range_metadata(&self, width: NumericWidth, runs: &[(u64, u64)]) -> MetadataValue<'ctx> {
        let int_type = self.custom_width_int_type(width.bits() as u32);
        let mut bounds: SV8<BasicMetadataValueEnum<'ctx>> = smallvec::smallvec![];
        for (lo, hi) in runs {
            bounds.push(int_type.const_int(*lo, false).into());
            bounds.push(int_type.const_int(*hi, false).into());
        }
        self.ctx.metadata_node(&bounds)
    }

    fn scalar_abi_attributes(
        abi: AbiMode,
        mapping: AbiParamMapping,
        cg_type: &CgType<'ctx>,
    ) -> [Option<Attribute>; 2] {
        let (AbiParamMapping::ScalarInRegister, CgType::Scalar(scalar)) = (mapping, cg_type) else {
            return [None, None];
        };
        match abi {
            AbiMode::Internal => scalar.internal_abi_attributes,
            AbiMode::Native => [scalar.native_abi_attribute, None],
        }
    }

    fn make_range_attribute(&self, width: NumericWidth, lo: u64, hi: u64) -> Attribute {
        let lower = [lo];
        let upper = [hi];
        unsafe {
            Attribute::new(llvm_sys::core::LLVMCreateConstantRangeAttribute(
                self.ctx.raw(),
                Attribute::get_named_enum_kind_id("range"),
                width.bits() as u32,
                lower.as_ptr(),
                upper.as_ptr(),
            ))
        }
    }

    fn custom_width_int_type(&self, width_bits: u32) -> IntType<'ctx> {
        self.ctx.custom_width_int_type(NonZeroU32::new(width_bits).unwrap()).unwrap()
    }

    fn scalar_basic_type(&self, st: ScalarType) -> BasicTypeEnum<'ctx> {
        self.builtin_types.scalar(st).basic_type
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
        abi: AbiMode,
    ) -> CgResult<CgFunctionType<'ctx>> {
        let param_types = phys_fn_type.params;
        let return_type = phys_fn_type.return_type;
        let _diverges = phys_fn_type.diverges;
        let return_logical_cg_type = self.codegen_type(phys_fn_type.return_type);
        let return_type_abi_mapping = self.get_abi_mapping_for_type(return_type, true, abi);

        // If a function returns a big (typically > 2 words) struct, its actually
        // 'returned' in the first parameter, which is a pointer
        let is_sret = match return_type_abi_mapping {
            AbiParamMapping::VoidReturnEmpty => false,
            AbiParamMapping::ScalarInRegister => false,
            AbiParamMapping::StructInInteger { .. } => false,
            AbiParamMapping::StructAsPointer => false,
            AbiParamMapping::InVectorRegisters { .. } => false,
            AbiParamMapping::StructByEightbytePair { .. } => false,
            AbiParamMapping::StructByIntPairArray => false,
            AbiParamMapping::BigStructByPtrToCopy { .. } => true,
            AbiParamMapping::BigStructByPtr => false,
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
            let abi_mapping = self.get_abi_mapping_for_type(param.pt, false, abi);
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
            abi,
        })
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
                let int_type = self.custom_width_int_type(abi_width).as_basic_type_enum();
                int_type
            }
            AbiParamMapping::StructAsPointer => self.builtin_types.ptr.as_basic_type_enum(),
            AbiParamMapping::InVectorRegisters { element, lanes, count } => {
                let base_type = if lanes == 1 {
                    self.scalar_basic_type(element)
                } else {
                    self.vec_llvm_type(element, lanes).as_basic_type_enum()
                };
                if count == 1 {
                    base_type
                } else {
                    base_type.array_type(count).as_basic_type_enum()
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
            AbiParamMapping::StructByIntPairArray => {
                let array_type = self.ctx.i64_type().array_type(2).as_basic_type_enum();
                array_type
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } | AbiParamMapping::BigStructByPtr => {
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
                        self.custom_width_int_type(active_width),
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
            AbiParamMapping::InVectorRegisters { .. } => {
                let ptr = self.build_k1_alloca(cg_ty, "in_vector_registers_storage");
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
                if self.k1.plan.config.filc && self.pt_has_pointer_in_union(cg_ty.pt()) {
                    self.build_zhas_union_marker(dst_ptr);
                }
                self.builder.build_store(dst_ptr, abi_value).unwrap();
                dst_ptr.as_basic_value_enum()
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } | AbiParamMapping::BigStructByPtr => {
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

    fn branch_to_return_block(&mut self, abi_value: BasicValueEnum<'ctx>) {
        let from_block = self.builder.get_insert_block().unwrap();
        let mut parts: SV4<BasicValueEnum<'ctx>> = smallvec::smallvec![];
        match abi_value {
            BasicValueEnum::StructValue(s) => {
                for i in 0..s.get_type().count_fields() {
                    parts.push(self.builder.build_extract_value(s, i, "").unwrap());
                }
            }
            BasicValueEnum::ArrayValue(a) => {
                for i in 0..a.get_type().len() {
                    parts.push(self.builder.build_extract_value(a, i, "").unwrap());
                }
            }
            other => parts.push(other),
        }
        if self.get_current_function().return_block.is_none() {
            let block =
                self.ctx.append_basic_block(self.get_current_function().function_value, "ret");
            self.builder.position_at_end(block);
            let mut phis: SV4<PhiValue<'ctx>> = smallvec::smallvec![];
            for part in &parts {
                debug_assert!(!part.is_struct_value() && !part.is_array_value());
                phis.push(self.builder.build_phi(part.get_type(), "").unwrap());
            }
            self.builder.position_at_end(from_block);
            self.get_current_function_mut().return_block = Some((block, phis));
        }
        let (block, phis) = self.get_current_function().return_block.as_ref().unwrap();
        for (phi, part) in phis.iter().zip(parts.iter()) {
            phi.add_incoming(&[(part, from_block)]);
        }
        self.builder.build_unconditional_branch(*block).unwrap();
    }

    fn codegen_return_block(&mut self, function_span: Span) {
        let Some((block, phis)) = self.get_current_function_mut().return_block.take() else {
            return;
        };
        self.builder.position_at_end(block);
        self.set_debug_location_from_span(function_span);
        let abi_type = self.get_current_function().function_value.get_type().get_return_type();
        let mut agg = match abi_type {
            Some(BasicTypeEnum::StructType(st)) => st.get_poison().as_aggregate_value_enum(),
            Some(BasicTypeEnum::ArrayType(at)) => at.get_poison().as_aggregate_value_enum(),
            _ => {
                self.builder.build_return(Some(&phis[0].as_basic_value())).unwrap();
                return;
            }
        };
        for (i, phi) in phis.iter().enumerate() {
            agg = self.builder.build_insert_value(agg, phi.as_basic_value(), i as u32, "").unwrap();
        }
        self.builder.build_return(Some(&agg)).unwrap();
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
                let integer_ptr = self.build_alloca(dst_int_type, "abi_struct_int");
                self.emit_lifetime_marker(true, integer_ptr);
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
                self.emit_lifetime_marker(false, integer_ptr);
                integer_value
            }
            AbiParamMapping::StructAsPointer => {
                self.load_at_k1_align(self.builtin_types.ptr, k1_value.into_pointer_value(), cg_ty)
            }
            AbiParamMapping::InVectorRegisters { .. } => {
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
                    self.emit_lifetime_marker(true, tmp);
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
                    self.emit_lifetime_marker(false, tmp);
                    abi_value
                }
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
                        self.emit_lifetime_marker(true, callers_copy);
                        self.memcpy_k1_value(callers_copy, k1_value.into_pointer_value(), cg_ty);
                        callers_copy.as_basic_value_enum()
                    }
                }
            }
            AbiParamMapping::BigStructByPtr => k1_value,
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
        let name = format!("llvm.{}.sat.i{}.f{}", op, to.width_bits(), from.width_bits());
        let function = match self.llvm_module.get_function(&name) {
            Some(f) => f,
            None => {
                let fn_type = to_int_type.fn_type(&[self.scalar_basic_type(from).into()], false);
                self.llvm_module.add_function(&name, fn_type, None)
            }
        };
        self.build_call(function, &[input.into()])
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value()
    }

    fn build_float_intrinsic(&self, name: &str, args: &[FloatValue<'ctx>]) -> FloatValue<'ctx> {
        let function = Intrinsic::find(name)
            .unwrap()
            .get_declaration(&self.llvm_module, &[args[0].get_type().as_basic_type_enum()])
            .unwrap();
        let mut call_args: SV4<BasicMetadataValueEnum<'ctx>> = SV4::new();
        for arg in args {
            call_args.push((*arg).into());
        }
        self.build_call(function, &call_args)
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_float_value()
    }

    fn emit_lifetime_marker(&self, start: bool, alloca: PointerValue<'ctx>) {
        let name = if start { "llvm.lifetime.start" } else { "llvm.lifetime.end" };
        let function = Intrinsic::find(name)
            .unwrap()
            .get_declaration(&self.llvm_module, &[self.builtin_types.ptr.as_basic_type_enum()])
            .unwrap();
        self.build_call(function, &[alloca.into()]);
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
        match self.cur_blocks.get(block_id) {
            Some(bb) => Ok(bb),
            None => Err(cgerr!(self.debug.current_span(), "Failed to get block: b{}", block_id)),
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
        let i1 = self.builder.build_int_truncate(bool, self.builtin_types.i1, name).unwrap();
        if let Some(trunc) = i1.as_instruction() {
            trunc.set_no_unsigned_wrap_flag(true).unwrap();
        }
        i1
    }

    fn i1_to_bool(&self, i1: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_cast_sign_flag(i1, self.builtin_types.boolean, false, name).unwrap()
    }

    fn build_k1_alloca(&mut self, ty: &CgType<'ctx>, name: &str) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(ty.rich_type(), name);
        ptr.as_instruction().unwrap().set_alignment(ty.rich_repr_layout().align).unwrap();
        if self.k1.plan.config.filc && self.pt_has_pointer_in_union(ty.pt()) {
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
        self.build_call(zhas_union, &[alloca.into()]);
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        call_id: ir::IrCallId,
        span: Span,
    ) -> CgResult<Option<BasicValueEnum<'ctx>>> {
        let call = self.cur_unit.call(call_id);
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
                let cg_fn_type = self.make_cg_function_type(&fn_type, AbiMode::Native)?;
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
        for (index, arg_ir_value) in self.cur_unit.args(call_args).iter().enumerate() {
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
                    caller_copies.push_grow(&mut self.tmp, value_marshalled.into_pointer_value());
                }
                _ => {}
            };
            args.push(value_marshalled.into())
        }

        let callsite_value = match llvm_callee {
            CallKind::Direct(function_id) => {
                let function_value = self.declare_llvm_function(function_id)?;
                self.set_debug_location_from_span(span);

                self.build_call(function_value, &args)
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
        let param_offset = cg_fn_type.is_sret as u32;
        for index in 0..cg_fn_type.param_k1_types.len() {
            let k1_type = self.mem.get_nth_lt(cg_fn_type.param_k1_types, index as usize);
            let mapping = *self.mem.get_nth_lt(cg_fn_type.param_abi_mappings, index as usize);
            for attr in
                Self::scalar_abi_attributes(cg_fn_type.abi, mapping, k1_type).into_iter().flatten()
            {
                callsite_value.add_attribute(AttributeLoc::Param(index + param_offset), attr);
            }
        }
        for attr in Self::scalar_abi_attributes(
            cg_fn_type.abi,
            cg_fn_type.return_abi_mapping,
            &cg_fn_type.return_logical_cg_type,
        )
        .into_iter()
        .flatten()
        {
            callsite_value.add_attribute(AttributeLoc::Return, attr);
        }
        for copy_ptr in caller_copies.as_slice() {
            self.emit_lifetime_marker(false, *copy_ptr);
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

    /// Named-intrinsic calls (`builtin("llvm.*")`) speak LLVM's type language:
    /// bool <-> i1, vectors by value as <n x t>, other scalars as-is. The
    /// declared K1 signature is trusted; the LLVM verifier is the backstop.
    /// Constant bool args fold to constant i1, satisfying immarg parameters
    fn codegen_llvm_intrinsic_call(
        &mut self,
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        name: StringId,
        function_id: FunctionId,
        call_args: ir::IrRange<ir::Value>,
        call_dst: Option<ir::Value>,
        span: Span,
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
        for (index, arg_ir_value) in self.cur_unit.args(call_args).iter().enumerate() {
            let arg_value = self.resolve_value(inst_mappings, *arg_ir_value)?;
            let param_type_id = params[index].type_id;
            let marshalled: BasicValueEnum<'ctx> = match self.k1.types.get(param_type_id) {
                Type::Bool => self.bool_to_i1(arg_value.into_int_value(), "").as_basic_value_enum(),
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
        let callsite = self.build_call(function_value, &args);
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
                // fn(builtin) copy(
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
                // fn(builtin) set(dst: ptr, value: u8, count: int): unit
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
                // fn(builtin) equals(p1: Pointer, p2: Pointer, size: uword): bool
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
                let call = self.build_call(memcmp_fv, &[p1_arg, p2_arg, size_arg]);
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
                // fn(builtin) exit(code: i32): never
                self.builder.build_unreachable().unwrap()
            }

            BackendBuiltin::TestSuite => {
                let Some(suite) = self.k1.test_suite else {
                    cgbail!(Span::NONE, "k1/test/suite exists only in test builds");
                };
                let cg_fn = self.llvm_functions.get(&function_id).unwrap();
                debug_assert!(cg_fn.function_type.is_sret);
                let out_storage =
                    cg_fn.function_value.get_param_iter().nth(0).unwrap().into_pointer_value();
                let suite_cg_type = cg_fn.function_type.return_logical_cg_type;
                let suite_value = self.codegen_static_value_canonical(suite)?;
                self.store_k1_value(&suite_cg_type, out_storage, suite_value);
                self.builder.build_return(None).unwrap()
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
                let i32_type = self.ctx.i32_type();
                let i64_type = self.ctx.i64_type();
                let i16_type = self.ctx.i16_type();
                let ptr_type = self.builtin_types.ptr;

                let mut entries: Vec<(TypeId, StaticValueId)> =
                    Vec::with_capacity(self.k1.type_infos.len());
                for (type_id, info_value_id) in self.k1.type_infos.iter() {
                    // No point re-ifying types that don't exist at runtime like type parameters
                    if !self.k1.get_type_variable_counts(*type_id).is_abstract() {
                        entries.push((*type_id, *info_value_id));
                    }
                }
                entries.sort_unstable_by_key(|(type_id, _)| type_id.as_u32());

                let mut type_keys: Vec<u64> = Vec::with_capacity(entries.len());
                for (type_id, _) in &entries {
                    type_keys.push(type_id.as_u32() as u64);
                }
                let hash = PerfectHash::build(&type_keys);

                // Seed, key and info tables
                let mut keys: Vec<IntValue<'ctx>> = Vec::with_capacity(hash.slot_keys.len());
                let mut infos: Vec<PointerValue<'ctx>> = Vec::with_capacity(hash.slot_keys.len());
                for (key, entry) in hash.slot_keys.iter().zip(hash.slot_entries.iter()) {
                    keys.push(i32_type.const_int(*key, false));
                    match entry {
                        Some(index) => {
                            let info = self.codegen_static_value_canonical(entries[*index].1)?;
                            infos.push(info.into_pointer_value());
                        }
                        None => infos.push(ptr_type.const_null()),
                    }
                }
                let mut seeds: Vec<IntValue<'ctx>> = Vec::with_capacity(hash.bucket_seeds.len());
                for seed in &hash.bucket_seeds {
                    seeds.push(i16_type.const_int(*seed as u64, false));
                }
                let table_global = |name: &str, array: inkwell::values::ArrayValue<'ctx>| {
                    let global = self.llvm_module.add_global(array.get_type(), None, name);
                    global.set_initializer(&array);
                    global.set_constant(true);
                    global.set_unnamed_addr(true);
                    global.set_linkage(LlvmLinkage::Private);
                    global.as_pointer_value()
                };
                let seeds_global = table_global("type_info_seeds", i16_type.const_array(&seeds));
                let keys_global = table_global("type_info_keys", i32_type.const_array(&keys));
                let infos_global = table_global("type_info_values", ptr_type.const_array(&infos));

                let c1 = i64_type.const_int(perfect_hash::C1, false);
                let c2 = i64_type.const_int(perfect_hash::C2, false);
                let hashed = self.builder.build_int_mul(type_id_arg, c1, "").unwrap();
                let bucket = self
                    .builder
                    .build_right_shift(
                        hashed,
                        i64_type.const_int(64 - hash.bucket_bits as u64, false),
                        false,
                        "bucket",
                    )
                    .unwrap();
                let seed_ptr = unsafe {
                    self.builder.build_in_bounds_gep(i16_type, seeds_global, &[bucket], "").unwrap()
                };
                let seed =
                    self.builder.build_load(i16_type, seed_ptr, "").unwrap().into_int_value();
                let seed = self.builder.build_int_z_extend(seed, i64_type, "").unwrap();
                let mix = self.builder.build_int_mul(seed, c1, "").unwrap();
                let mixed = self.builder.build_xor(type_id_arg, mix, "").unwrap();
                let rehashed = self.builder.build_int_mul(mixed, c2, "").unwrap();
                let slot = self
                    .builder
                    .build_right_shift(
                        rehashed,
                        i64_type.const_int(64 - hash.slot_bits as u64, false),
                        false,
                        "slot",
                    )
                    .unwrap();
                let key_ptr = unsafe {
                    self.builder.build_in_bounds_gep(i32_type, keys_global, &[slot], "").unwrap()
                };
                let key = self.builder.build_load(i32_type, key_ptr, "").unwrap().into_int_value();
                let key = self.builder.build_int_z_extend(key, i64_type, "").unwrap();
                let hit =
                    self.builder.build_int_compare(IntPredicate::EQ, key, type_id_arg, "").unwrap();
                let found_block = self.append_basic_block("found");
                let miss_block = self.append_basic_block("miss");
                self.builder.build_conditional_branch(hit, found_block, miss_block).unwrap();

                self.builder.position_at_end(miss_block);
                let crash_fn = self.k1_ns_fn_value("crash-missing-type-info")?;
                self.build_call(crash_fn, &[type_id_arg.into()]);
                self.builder.build_unreachable().unwrap();

                self.builder.position_at_end(found_block);
                let info_slot = unsafe {
                    self.builder.build_in_bounds_gep(ptr_type, infos_global, &[slot], "").unwrap()
                };
                let info = self.builder.build_load(ptr_type, info_slot, "").unwrap();
                self.store_k1_value(&type_info_cg_type, out_storage, info);
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        block_id: BlockId,
    ) -> CgResult<BasicBlock<'ctx>> {
        let llvm_block = self.get_llvm_block(block_id)?;
        self.builder.position_at_end(llvm_block);
        for inst_id in self.cur_unit.block_insts(block_id) {
            self.codegen_inst(inst_mappings, block_id, inst_id)?;
        }
        Ok(llvm_block)
    }

    fn codegen_phi_incomings(
        &mut self,
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        phi: PhiValue<'ctx>,
        block_id: BlockId,
        inst_id: InstId,
        debug_locn: Option<DILocation<'ctx>>,
    ) -> CgResult<()> {
        let Inst::Phi { incomings, .. } = *self.cur_unit.inst(inst_id) else { unreachable!() };
        for incoming in self.cur_unit.phi_cases(incomings) {
            let Some(block) = self.cur_blocks.get(incoming.from) else {
                continue;
            };
            let incoming_is_real_succ =
                self.cur_unit.successors(incoming.from, Some(false)).any(|succ| succ == block_id);
            if !incoming_is_real_succ {
                continue;
            }
            match block.get_terminator() {
                Some(terminator) => self.builder.position_before(&terminator),
                None => self.builder.position_at_end(block),
            }
            self.restore_debug_location(debug_locn);
            let value = self.resolve_value(inst_mappings, incoming.value)?;
            phi.add_incoming(&[(&value, block)])
        }
        Ok(())
    }

    fn resolve_value(
        &mut self,
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        value: ir::Value,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        //eprintln!("codegen_value {}", value);
        match value {
            ir::Value::Inst(inst_id) => match inst_mappings.get(inst_id) {
                Some(v) => Ok(v),
                None => Err(cgerr!(
                    self.debug.current_span(),
                    "codegen llvm has no value for this instruction: i{} {}",
                    inst_id.as_u32(),
                    ir::inst_to_string(self.k1, &self.cur_unit, inst_id)
                )),
            },
            ir::Value::GlobalAddr { id, .. } => {
                debug_assert!(self.k1.globals.get(id).reload_ns.is_none());
                self.codegen_global_addr(id)
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
                    ScalarType::F32 => {
                        llvm_float_constant(self.ctx, TypedFloatValue::F32(f32::from_bits(data)))
                            .into()
                    }
                    ScalarType::F64 => llvm_float_constant(
                        self.ctx,
                        TypedFloatValue::F64(f32::from_bits(data) as f64),
                    )
                    .into(),
                    ScalarType::Pointer => {
                        if data == 0 {
                            self.builtin_types.ptr.const_zero().into()
                        } else {
                            panic!("Got non-zero Pointer in codegen_llvm: {data}")
                        }
                    }
                    _ => self
                        .scalar_basic_type(t)
                        .into_int_type()
                        .const_int(ir::data32_bits(t, data), false)
                        .into(),
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
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
            VecOpIr::Shl | VecOpIr::ShrS | VecOpIr::ShrU => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let count = self.resolve_value(inst_mappings, vop.rhs)?.into_int_value();
                let elem_int_type = l.get_type().get_element_type().into_int_type();
                let count = self.builder.build_int_cast(count, elem_int_type, "").unwrap();
                let count_mask =
                    elem_int_type.const_int(elem_int_type.get_bit_width() as u64 - 1, false);
                let count = self.builder.build_and(count, count_mask, "shift_count").unwrap();
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
                let result = match vop.op {
                    VecOpIr::Shl => self.builder.build_left_shift(l, counts, "").unwrap(),
                    VecOpIr::ShrS => self.builder.build_right_shift(l, counts, true, "").unwrap(),
                    _ => self.builder.build_right_shift(l, counts, false, "").unwrap(),
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::Shuffle { mask } => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let r = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let i32t = self.ctx.i32_type();
                let mut lanes: Vec<IntValue<'ctx>> = Vec::with_capacity(vop.lanes as usize);
                for lane in 0..vop.lanes as usize {
                    let index = self.k1.get_static_container_element(mask, lane).as_size().unwrap();
                    lanes.push(i32t.const_int(index as u64, false));
                }
                let result = self
                    .builder
                    .build_shuffle_vector(l, r, LlvmVectorType::const_vector(&lanes), "")
                    .unwrap();
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::ShuffleBytes => {
                let table = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let idx = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let v16i8 = self.vec_llvm_type(ScalarType::I8, 16);
                let arch = self.k1.plan.config.target.arch();
                let (name, idx, overloads): (&str, _, &[BasicTypeEnum<'ctx>]) = match arch {
                    compiler::Arch::Intel => ("llvm.x86.ssse3.pshuf.b.128", idx, &[]),
                    compiler::Arch::Arm | compiler::Arch::Wasm => {
                        let keep = self.ctx.i8_type().const_int(0x8F, false);
                        let keep_lanes = LlvmVectorType::const_vector(&[keep; 16]);
                        let masked = self.builder.build_and(idx, keep_lanes, "").unwrap();
                        if arch == compiler::Arch::Arm {
                            ("llvm.aarch64.neon.tbl1", masked, &[v16i8.as_basic_type_enum()])
                        } else {
                            ("llvm.wasm.swizzle", masked, &[])
                        }
                    }
                };
                let f = Intrinsic::find(name)
                    .unwrap()
                    .get_declaration(&self.llvm_module, overloads)
                    .unwrap();
                let call = self.builder.build_call(f, &[table.into(), idx.into()], "").unwrap();
                let result = match call.try_as_basic_value() {
                    ValueKind::Basic(v) => v.into_vector_value(),
                    ValueKind::Instruction(_) => unreachable!("byte shuffle returns a vector"),
                };
                self.vec_store(&vop, inst_mappings, result)?;
                Ok(None)
            }
            VecOpIr::EqLanes | VecOpIr::LtLanesS | VecOpIr::LtLanesU => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let r = self.vec_load(&vop, inst_mappings, vop.rhs)?;
                let cmp_i1 = match (vop.op, is_float) {
                    (VecOpIr::EqLanes, true) => {
                        self.builder.build_float_compare(FloatPredicate::OEQ, l, r, "").unwrap()
                    }
                    (VecOpIr::EqLanes, false) => {
                        self.builder.build_int_compare(IntPredicate::EQ, l, r, "").unwrap()
                    }
                    (_, true) => {
                        self.builder.build_float_compare(FloatPredicate::OLT, l, r, "").unwrap()
                    }
                    (VecOpIr::LtLanesS, false) => {
                        self.builder.build_int_compare(IntPredicate::SLT, l, r, "").unwrap()
                    }
                    _ => self.builder.build_int_compare(IntPredicate::ULT, l, r, "").unwrap(),
                };
                let elem_bits = vop.elem.get_layout().size_bits() as u32;
                let int_lane_vec = self.custom_width_int_type(elem_bits).vec_type(vop.lanes);
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
            VecOpIr::ReduceOr => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let f = Intrinsic::find("llvm.vector.reduce.or")
                    .unwrap()
                    .get_declaration(&self.llvm_module, &[l.get_type().as_basic_type_enum()])
                    .unwrap();
                let call = self.builder.build_call(f, &[l.into()], "").unwrap();
                let result = match call.try_as_basic_value() {
                    ValueKind::Basic(v) => v,
                    ValueKind::Instruction(_) => unreachable!("reduce-or returns a scalar"),
                };
                Ok(Some(result))
            }
            VecOpIr::ToMask => {
                let l = self.vec_load(&vop, inst_mappings, vop.lhs)?;
                let elem_bits = vop.elem.get_layout().size_bits() as u32;
                let int_lane_vec = self.custom_width_int_type(elem_bits).vec_type(vop.lanes);
                let as_ints = if is_float {
                    self.builder.build_bit_cast(l, int_lane_vec, "").unwrap().into_vector_value()
                } else {
                    l
                };
                let zero = int_lane_vec.const_zero();
                let msbs =
                    self.builder.build_int_compare(IntPredicate::SLT, as_ints, zero, "").unwrap();
                let mask_int_type = self.custom_width_int_type(vop.lanes);
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        block_id: BlockId,
        inst_id: InstId,
    ) -> CgResult<()> {
        let span = self.cur_unit.span(inst_id);
        self.set_debug_location_from_span(span);
        let inst = *self.cur_unit.inst(inst_id);
        match inst {
            Inst::Data(data_inst) => {
                let value: BasicValueEnum<'ctx> = match data_inst {
                    ir::DataInst::I64(bits) => self.ctx.i64_type().const_int(bits, false).into(),
                    ir::DataInst::F64(f) => {
                        llvm_float_constant(self.ctx, TypedFloatValue::F64(f)).into()
                    }
                };
                inst_mappings.insert(inst_id, value.as_basic_value_enum());
                Ok(())
            }
            Inst::ReloadGlobalAddr { id, .. } => {
                let reload_ns = self.k1.globals.get(id).reload_ns;
                let value = if let Some(reload_ns) = reload_ns
                    && self.kind != CgKind::ReloadDylib(reload_ns)
                {
                    self.codegen_reload_global_addr(id)?
                } else {
                    self.codegen_global_addr(id)?
                };
                inst_mappings.insert(inst_id, value);
                Ok(())
            }
            Inst::Alloca { t, returned, debug, .. } => {
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

                if let Some(var_info) = debug
                    && !var_info.user_hidden
                    && !var_info.source_span.is_none()
                    && !self.debug.line_tables_only
                {
                    let name_str = self.k1.ident_str(var_info.name);
                    let debug_locn = self.get_debug_location_from_span(var_info.source_span);
                    let local_variable = self.debug.debug_builder.create_auto_variable(
                        self.debug.current_scope(),
                        name_str,
                        self.debug.current_file(),
                        self.get_line_number(var_info.source_span),
                        cg_type.debug_type().unwrap(),
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
                let align = self.claimed_align(PhysicalType::scalar(t), unaligned);
                let dst_ptr = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                let value = self.resolve_value(inst_mappings, value)?;
                let store = self.builder.build_store(dst_ptr, value).unwrap();
                store.set_alignment(align).unwrap();
                if volatile {
                    store.set_volatile(true).unwrap();
                }
                Ok(())
            }
            Inst::Load { t, src, volatile, unaligned } => {
                let cg_ty = self.codegen_type(t);
                let src_ptr = self.resolve_value(inst_mappings, src)?.into_pointer_value();
                let load = self.builder.build_load(cg_ty.rich_type(), src_ptr, "").unwrap();
                let load_inst = load.as_instruction_value().unwrap();
                load_inst.set_alignment(self.claimed_align(t, unaligned)).unwrap();
                if volatile {
                    load_inst.set_volatile(true).unwrap();
                }
                if let CgType::Scalar(scalar) = cg_ty {
                    for (kind_id, metadata) in scalar.load_metadata.into_iter().flatten() {
                        load_inst.set_metadata(metadata, kind_id).unwrap();
                    }
                }
                inst_mappings.insert(inst_id, load);
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
                let cas = *self.cur_unit.cmpxchg(id);
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
                let vop = *self.cur_unit.vec_op(id);
                if let Some(result) = self.codegen_vec_op(inst_mappings, vop)? {
                    inst_mappings.insert(inst_id, result);
                }
                Ok(())
            }
            Inst::Fence { ord } => {
                self.builder.build_fence(Self::llvm_atomic_ordering(ord), false, "").unwrap();
                Ok(())
            }
            Inst::Copy { dst, src, t, volatile, unaligned, .. } => {
                let dst_value = self.resolve_value(inst_mappings, dst)?;
                let src_value = self.resolve_value(inst_mappings, src)?;
                let layout = self.k1.get_pt_layout(t);
                let align = self.claimed_align(t, unaligned);
                let bytes = self.builtin_types.ptr_sized_int.const_int(layout.size as u64, false);
                let ptr = self.builtin_types.ptr.as_basic_type_enum();
                let memcpy = Intrinsic::find("llvm.memcpy")
                    .unwrap()
                    .get_declaration(
                        &self.llvm_module,
                        &[ptr, ptr, self.builtin_types.ptr_sized_int.as_basic_type_enum()],
                    )
                    .unwrap();
                let is_volatile = self.builtin_types.i1.const_int(volatile as u64, false);
                let call = self
                    .builder
                    .build_call(
                        memcpy,
                        &[dst_value.into(), src_value.into(), bytes.into(), is_volatile.into()],
                        "",
                    )
                    .unwrap();
                let align_attr = self.make_enum_attribute("align", align as u64);
                call.add_attribute(AttributeLoc::Param(0), align_attr);
                call.add_attribute(AttributeLoc::Param(1), align_attr);
                Ok(())
            }
            Inst::Zero { dst, t, unaligned, .. } => {
                let dst_value = self.resolve_value(inst_mappings, dst)?.into_pointer_value();
                let layout = self.k1.get_pt_layout(t);
                let align = self.claimed_align(t, unaligned);
                let bytes = self.builtin_types.ptr_sized_int.const_int(layout.size as u64, false);
                self.builder
                    .build_memset(dst_value, align, self.ctx.i8_type().const_zero(), bytes)
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
                for case in self.cur_unit.switch_cases(cases) {
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
            Inst::Phi { t, .. } => {
                let phi_ty = self.pt_canon_type(t);
                let phi = self.builder.build_phi(phi_ty, "").unwrap();
                let debug_locn = self.builder.get_current_debug_location();
                self.buffers.pending_phis.push((phi, block_id, inst_id, debug_locn));
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
                        None => {
                            self.builder.build_return(None).unwrap();
                        }
                        Some(v) => self.branch_to_return_block(v),
                    };
                }
                Ok(())
            }
            Inst::Select { cond, cons, alt, .. } => {
                let cond = self.resolve_value(inst_mappings, cond)?.into_int_value();
                let cond_i1 = self.bool_to_i1(cond, "");
                let cons = self.resolve_value(inst_mappings, cons)?;
                let alt = self.resolve_value(inst_mappings, alt)?;
                let selected = self.builder.build_select(cond_i1, cons, alt, "").unwrap();
                inst_mappings.insert(inst_id, selected);
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
            Inst::BitNot { v, .. } => {
                let input = self.resolve_value(inst_mappings, v)?.into_int_value();
                let not_input = self.builder.build_not(input, "").unwrap();
                inst_mappings.insert(inst_id, not_input.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatUnop { op, v, .. } => {
                let input = self.resolve_value(inst_mappings, v)?.into_float_value();
                let result = match op {
                    FloatUnop::Neg => self.builder.build_float_neg(input, "").unwrap(),
                    FloatUnop::Sqrt => self.build_float_intrinsic("llvm.sqrt", &[input]),
                    FloatUnop::Floor => self.build_float_intrinsic("llvm.floor", &[input]),
                    FloatUnop::Ceil => self.build_float_intrinsic("llvm.ceil", &[input]),
                    FloatUnop::RoundToZero => self.build_float_intrinsic("llvm.trunc", &[input]),
                    FloatUnop::RoundEven => self.build_float_intrinsic("llvm.roundeven", &[input]),
                };
                inst_mappings.insert(inst_id, result.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatFma { a, b, c, .. } => {
                let a = self.resolve_value(inst_mappings, a)?.into_float_value();
                let b = self.resolve_value(inst_mappings, b)?.into_float_value();
                let c = self.resolve_value(inst_mappings, c)?.into_float_value();
                let result = self.build_float_intrinsic("llvm.fma", &[a, b, c]);
                inst_mappings.insert(inst_id, result.as_basic_value_enum());
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
            Inst::PtrToWord { v, .. } => {
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
            Inst::IntBinop { op, lhs, rhs, .. } => {
                let l = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let r = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let result = match op {
                    IntOp::Add => self.builder.build_int_add(l, r, "").unwrap(),
                    IntOp::Sub => self.builder.build_int_sub(l, r, "").unwrap(),
                    IntOp::Mul => self.builder.build_int_mul(l, r, "").unwrap(),
                    IntOp::DivU => self.build_checked_int_div(l, r, false, false)?,
                    IntOp::DivS => self.build_checked_int_div(l, r, true, false)?,
                    IntOp::RemU => self.build_checked_int_div(l, r, false, true)?,
                    IntOp::RemS => self.build_checked_int_div(l, r, true, true)?,
                    IntOp::And => self.builder.build_and(l, r, "").unwrap(),
                    IntOp::Or => self.builder.build_or(l, r, "").unwrap(),
                    IntOp::Xor => self.builder.build_xor(l, r, "").unwrap(),
                    IntOp::Shl => {
                        let count = self.build_shift_count(r, l);
                        self.builder.build_left_shift(l, count, "").unwrap()
                    }
                    IntOp::ShrU | IntOp::ShrS => {
                        let count = self.build_shift_count(r, l);
                        self.builder.build_right_shift(l, count, op == IntOp::ShrS, "").unwrap()
                    }
                    IntOp::MulHighU | IntOp::MulHighS => {
                        let signed = op == IntOp::MulHighS;
                        let narrow = l.get_type();
                        let width = narrow.get_bit_width();
                        let wide = self.custom_width_int_type(width * 2);
                        let l = self.builder.build_int_cast_sign_flag(l, wide, signed, "").unwrap();
                        let r = self.builder.build_int_cast_sign_flag(r, wide, signed, "").unwrap();
                        let product = self.builder.build_int_mul(l, r, "").unwrap();
                        let shift = wide.const_int(width as u64, false);
                        let high =
                            self.builder.build_right_shift(product, shift, false, "").unwrap();
                        self.builder.build_int_truncate(high, narrow, "").unwrap()
                    }
                };
                inst_mappings.insert(inst_id, result.as_basic_value_enum());
                Ok(())
            }
            Inst::IntOverflows { op, signed, lhs, rhs, .. } => {
                let l = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let r = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let name = match (op, signed) {
                    (OverflowOp::Add, false) => "llvm.uadd.with.overflow",
                    (OverflowOp::Add, true) => "llvm.sadd.with.overflow",
                    (OverflowOp::Sub, false) => "llvm.usub.with.overflow",
                    (OverflowOp::Sub, true) => "llvm.ssub.with.overflow",
                    (OverflowOp::Mul, false) => "llvm.umul.with.overflow",
                    (OverflowOp::Mul, true) => "llvm.smul.with.overflow",
                };
                let function = Intrinsic::find(name)
                    .unwrap()
                    .get_declaration(&self.llvm_module, &[l.get_type().as_basic_type_enum()])
                    .unwrap();
                let pair = self
                    .build_call(function, &[l.into(), r.into()])
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_struct_value();
                let flag = self.builder.build_extract_value(pair, 1, "").unwrap().into_int_value();
                let bool_value = self.i1_to_bool(flag, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
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
                    ir::IntCmpPred::Ne => IntPredicate::NE,
                };
                let lhs_value = self.resolve_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.resolve_value(inst_mappings, rhs)?.into_int_value();
                let cmp =
                    self.builder.build_int_compare(llvm_pred, lhs_value, rhs_value, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatBinop { op, lhs, rhs, .. } => {
                let l = self.resolve_value(inst_mappings, lhs)?.into_float_value();
                let r = self.resolve_value(inst_mappings, rhs)?.into_float_value();
                let result = match op {
                    FloatOp::Add => self.builder.build_float_add(l, r, "").unwrap(),
                    FloatOp::Sub => self.builder.build_float_sub(l, r, "").unwrap(),
                    FloatOp::Mul => self.builder.build_float_mul(l, r, "").unwrap(),
                    FloatOp::Div => self.builder.build_float_div(l, r, "").unwrap(),
                    FloatOp::Rem => self.builder.build_float_rem(l, r, "").unwrap(),
                    FloatOp::Min => self.build_float_intrinsic("llvm.minimum", &[l, r]),
                    FloatOp::Max => self.build_float_intrinsic("llvm.maximum", &[l, r]),
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
                    ir::FloatCmpPred::Ne => FloatPredicate::UNE,
                };
                let lhs = self.resolve_value(inst_mappings, lhs)?.into_float_value();
                let rhs = self.resolve_value(inst_mappings, rhs)?.into_float_value();
                let cmp = self.builder.build_float_compare(llvm_pred, lhs, rhs, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
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
        function_span: Span,
        return_type: Option<DIType<'ctx>>,
        param_debug_types: &[DIType<'ctx>],
        is_definition: bool,
    ) -> CgResult<(DISubprogram<'ctx>, DIFile<'ctx>)> {
        let (function_file_id, function_line_number) = if function_span.is_none() {
            (0, 0)
        } else {
            let source = self.k1.ast.sources.source_by_span(function_span);
            let line = source
                .get_line_for_span_start(&self.k1.ast.mem, function_span)
                .expect("line for span");
            (source.file_id, line.line_number())
        };
        let function_scope_start_line_number = function_line_number;
        let function_file = self.debug.files.get(&function_file_id).unwrap();
        let dbg_fn_type = self.debug.debug_builder.create_subroutine_type(
            *function_file,
            return_type,
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
        let is_private = !is_dylib_root && typed_function_linkage == TyperLinkage::Standard;
        let llvm_linkage = if is_private && is_definition && !self.multi_unit() {
            LlvmLinkage::Internal
        } else {
            LlvmLinkage::External
        };
        let llvm_name = if is_dylib_root {
            self.make_reloadable_function_symbol(function_id)
        } else {
            match typed_function.linkage {
                TyperLinkage::External { link_name: Some(link_name), .. }
                | TyperLinkage::Exported { link_name: Some(link_name) } => {
                    self.k1.ident_str(link_name).to_string()
                }
                TyperLinkage::Exported { link_name: None } => {
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

        let Some(ir_fn) = self.k1.ir.function_unit(function_id).copied() else {
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
        let llvm_function_type = self.make_cg_function_type(&ir_fn.fn_type, abi_mode)?;
        debug!(
            "-> res (is_sret={}) {}",
            llvm_function_type.is_sret, llvm_function_type.llvm_function_type
        );

        let mut di_types: SV8<_> = smallvec::smallvec![];
        for t in self.mem.getn_lt(llvm_function_type.param_k1_types).iter() {
            if let Some(di_type) = t.debug_type() {
                di_types.push(di_type);
            }
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
                    last_alloca_instr: None,
                    returned_sret_variable: None,
                    return_block: None,
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
        if ir_fn.is_cold(self.k1) {
            function_value
                .add_attribute(AttributeLoc::Function, self.make_enum_attribute("cold", 0));
        }
        if typed_function.is_noinline() {
            function_value
                .add_attribute(AttributeLoc::Function, self.make_enum_attribute("noinline", 0));
        }

        if self.k1.plan.config.target.arch() == compiler::Arch::Wasm {
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
                let k1_type = *self.mem.get_nth_lt(llvm_function_type.param_k1_types, i - offset);
                match abi_mapping {
                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: true } => {
                        for attr in self.make_byval_attributes(&k1_type) {
                            function_value.add_attribute(AttributeLoc::Param(i as u32), attr);
                        }
                    }
                    AbiParamMapping::BigStructByPtr => {
                        for attr in self.make_borrowed_ptr_attributes(&k1_type) {
                            function_value.add_attribute(AttributeLoc::Param(i as u32), attr);
                        }
                    }
                    _ => {}
                }
                for attr in Self::scalar_abi_attributes(abi_mode, *abi_mapping, &k1_type)
                    .into_iter()
                    .flatten()
                {
                    function_value.add_attribute(AttributeLoc::Param(i as u32), attr);
                }
            }
        }
        for attr in Self::scalar_abi_attributes(
            abi_mode,
            llvm_function_type.return_abi_mapping,
            &llvm_function_type.return_logical_cg_type,
        )
        .into_iter()
        .flatten()
        {
            function_value.add_attribute(AttributeLoc::Return, attr);
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
                last_alloca_instr: None,
                returned_sret_variable: None,
                return_block: None,
                debug_info: di_subprogram,
                debug_file: di_file,
            },
        );
        Ok(function_value)
    }

    fn get_abi_mapping_for_type(
        &self,
        pt: PhysicalType,
        is_return: bool,
        abi: AbiMode,
    ) -> AbiParamMapping {
        let native = self.get_native_abi_mapping_for_type(pt, is_return);
        match native {
            AbiParamMapping::BigStructByPtrToCopy { .. }
                if abi == AbiMode::Internal && !is_return =>
            {
                AbiParamMapping::BigStructByPtr
            }
            other => other,
        }
    }

    fn get_native_abi_mapping_for_type(
        &self,
        pt: PhysicalType,
        is_return: bool,
    ) -> AbiParamMapping {
        let arch = self.k1.plan.config.target.arch();
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
                let size_bytes = agg_record.layout.size;
                if size_bytes == 8 {
                    let ([class1, _], _) = self.k1.get_pt_eightbyte_classes(pt);
                    if class1 == RegisterClass::Ptr {
                        return AbiParamMapping::StructAsPointer;
                    }
                }
                if let Some(ha) = self.k1.get_pt_vector_registers(pt, is_return, arch) {
                    return AbiParamMapping::InVectorRegisters {
                        element: ha.element,
                        lanes: ha.lanes,
                        count: ha.count,
                    };
                }
                match arch {
                    compiler::Arch::Arm | compiler::Arch::Wasm => {
                        if size_bytes <= 8 {
                            // Returns use an ABI type of the exact size of the struct
                            // Params use an ABI type of 64, but need to know the 'active' bits
                            let struct_bits = size_bytes * 8;
                            let abi_bits = if is_return { struct_bits } else { 64 };
                            AbiParamMapping::StructInInteger {
                                abi_width: abi_bits,
                                active_width: struct_bits,
                            }
                        } else if size_bytes <= 16 {
                            // If the size of the structure is between 9 and 16 bytes, pass the structure as
                            // [an array of] two integers of 8 bytes each
                            AbiParamMapping::StructByIntPairArray
                        } else {
                            AbiParamMapping::BigStructByPtrToCopy { byval_attr: false }
                        }
                    }
                    compiler::Arch::Intel => {
                        // SysV AMD64 3.2.3: unaligned fields force class MEMORY
                        if agg_record.has_misaligned_fields {
                            AbiParamMapping::BigStructByPtrToCopy { byval_attr: true }
                        } else if size_bytes <= 8 {
                            let ([class1, _], _) = self.k1.get_pt_eightbyte_classes(pt);
                            if class1 == RegisterClass::Float {
                                // One SSE eightbyte: it rides in an XMM register, so it
                                // must be typed as floats, not as an integer
                                let (element, lanes) =
                                    match self.k1.get_pt_homogeneous_aggregate(pt) {
                                        Some(ha) if ha.lanes == 1 => (ha.element, ha.count),
                                        _ => (ScalarType::F64, 1),
                                    };
                                AbiParamMapping::InVectorRegisters { element, lanes, count: 1 }
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
                            let ([eb1, eb2], eb2_bits) = self.k1.get_pt_eightbyte_classes(pt);
                            if eb1 == RegisterClass::Initial || eb2 == RegisterClass::Initial {
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

    fn codegen_function_body(
        &mut self,
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
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
            .get_lines_for_span(function_span)
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
                    param_k1_type.debug_type().unwrap(),
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
        inst_mappings: &mut IdMap<InstId, BasicValueEnum<'ctx>>,
        function_id: FunctionId,
    ) -> CgResult<()> {
        let Some(ir_unit) = self.k1.ir.function_unit(function_id).copied() else {
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

        let u = ir_unit.view(&self.k1.ir.mem);
        self.cur_unit = u;
        let entry_block = u.first_block().unwrap();

        let mut seen = std::mem::take(&mut self.buffers.cfg_seen);
        let mut blocks_rpo = std::mem::take(&mut self.buffers.cfg_blocks_rpo);
        self.compute_cfg_order(entry_block, &mut blocks_rpo, &mut seen);

        self.cur_blocks.clear();
        let llvm_function = self.get_current_function().function_value;
        for block in &blocks_rpo {
            let kind = u.block(*block).kind;
            let b = self.ctx.append_basic_block(llvm_function, kind.str());
            self.cur_blocks.insert(*block, b);
        }

        {
            // Jump from prelude to entry block
            let debug_locn = self.builder.get_current_debug_location().unwrap();
            self.builder.unset_current_debug_location();

            let entry = self.get_llvm_block(entry_block)?;
            self.builder.build_unconditional_branch(entry).unwrap();

            self.builder.set_current_debug_location(debug_locn);
        }

        inst_mappings.clear();
        self.buffers.pending_phis.clear();
        for block in &blocks_rpo {
            self.codegen_block(inst_mappings, *block)?;
        }
        let pending_phis = std::mem::take(&mut self.buffers.pending_phis);
        for (phi, block_id, inst_id, debug_locn) in &pending_phis {
            self.codegen_phi_incomings(inst_mappings, *phi, *block_id, *inst_id, *debug_locn)?;
        }
        self.buffers.pending_phis = pending_phis;
        self.codegen_return_block(self.k1.get_function_span(function_id));
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
        seen: &mut IdMap<BlockId, ()>,
    ) {
        fn dfs(
            u: &ir::UnitView,
            b: BlockId,
            seen: &mut IdMap<BlockId, ()>,
            result: &mut Vec<BlockId>,
        ) {
            if seen.contains(b) {
                return;
            }
            seen.insert(b, ());
            for succ in u.successors(b, Some(false)) {
                dfs(u, succ, seen, result);
            }
            result.push(b);
        }

        dfs(&self.cur_unit, entry, seen, result);

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

    fn codegen_static_value_as_const(
        &mut self,
        static_value_id: StaticValueId,
        depth: usize,
        writable: bool,
    ) -> CgResult<BasicValueEnum<'ctx>> {
        if !writable && let Some(basic) = self.static_values_basics.get(&static_value_id) {
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
            StaticValue::FunctionPointer(_, function_id) => {
                let function_value = self.declare_llvm_function(*function_id)?;
                function_value.as_global_value().as_pointer_value().into()
            }
            StaticValue::Float(float_value) => llvm_float_constant(self.ctx, *float_value).into(),
            StaticValue::String(string_id) => {
                let string_global = self.codegen_string_id_to_global(*string_id).unwrap();
                string_global.get_initializer().unwrap()
            }
            StaticValue::Zero(type_id) => {
                let pt = self.k1.get_physical_type_computed(*type_id).unwrap();
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
                    let value = self.codegen_static_value_as_const(*field, depth + 1, writable)?;
                    packed_values.push_grow(&mut self.tmp, value);
                    let field_size = self.k1.get_pt_layout(field_layout.field_t);
                    debug_assert_eq!(self.layout_per_llvm(&value.get_type()).size, field_size.size);
                    last_offset = field_layout.offset + field_size.size;
                }
                let struct_size = self.k1.get_layout_computed(s_type_id).unwrap().size;
                if struct_size > last_offset {
                    let padding_value = self
                        .padding_type(struct_size - last_offset)
                        .get_undef()
                        .as_basic_value_enum();
                    packed_values.push_grow(&mut self.tmp, padding_value);
                }
                let struct_value = self.ctx.const_struct(&packed_values, true);

                debug_assert_eq!(
                    self.layout_per_llvm(&struct_value.get_type()).size,
                    struct_size,
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
                            self.codegen_static_value_as_const(payload, depth + 1, writable)?;
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
                let elements_writable = writable && cont.kind != StaticContainerKind::Span;
                let array_value = self.codegen_static_elements_array(
                    element_type,
                    span_elements,
                    depth,
                    elements_writable,
                )?;
                let element_align = self.k1.get_layout_computed(element_type).unwrap().align;
                self.codegen_static_container(
                    static_value_id,
                    cont.type_id,
                    cont.kind,
                    cont.len(),
                    element_align,
                    array_value.as_basic_value_enum(),
                    writable,
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
                    writable,
                )
            }
        };
        if depth == 0 && !writable {
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
        writable: bool,
    ) -> BasicValueEnum<'ctx> {
        match kind {
            StaticContainerKind::Array | StaticContainerKind::Vector => array_value,
            StaticContainerKind::Span | StaticContainerKind::Buffer | StaticContainerKind::List => {
                let data_global = if writable && kind != StaticContainerKind::Span {
                    self.make_global_from_value(
                        array_value,
                        element_align,
                        &format!("k1.static.{}.elems.mut", static_value_id.as_u32()),
                        false,
                        LlvmLinkage::Private,
                        false,
                    )
                } else {
                    self.make_global_from_value(
                        array_value,
                        element_align,
                        &format!("k1.static.{}.elems", static_value_id.as_u32()),
                        true,
                        LlvmLinkage::LinkOnceODR,
                        false,
                    )
                };
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
        writable: bool,
    ) -> CgResult<StructValue<'ctx>> {
        let element_layout = self.k1.get_layout_computed(element_type).unwrap();
        // Stride padding is a container concern, not part of Layout.size, so each
        // element may be followed by an explicit padding entry
        let end_padding = element_layout.stride() - element_layout.size;
        let values_per_element: u32 = if end_padding > 0 { 2 } else { 1 };
        let mut packed_values = self.tmp.new_list(elements.len() as u32 * values_per_element);

        for elem in elements.iter() {
            let elem_basic_value =
                self.codegen_static_value_as_const(*elem, depth + 1, writable)?;
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
        let direct_value = self.codegen_static_value_as_const(static_value_id, 0, false)?;
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
        self.k1.plan.config.target.arch() != compiler::Arch::Wasm
            && self.k1.plan.config.target.platform() != compiler::Platform::Bare
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
            let mode = if self.k1.plan.is_executable() {
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
            | StaticValue::FunctionPointer(_, _)
            | StaticValue::Float(_) => {
                self.codegen_static_value_as_const(static_value_id, 0, false)?
            }
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
        let rust_str = self.k1.get_string(string_id);
        let content_hash = xxhash_rust::xxh3::xxh3_128(rust_str.as_bytes());
        let string_name = &format!("k1.string.{content_hash:032x}.bytes");
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
            &format!("k1.string.{content_hash:032x}"),
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

    fn llvm_cpu_features(k1: &TypedProgram) -> (String, String) {
        let build = k1.plan.config;
        match k1.plan.get(build.cpu) {
            "native" => (
                TargetMachine::get_host_cpu_name().to_string(),
                TargetMachine::get_host_cpu_features().to_string(),
            ),
            cpu => (cpu.to_string(), k1.plan.get(build.features).to_string()),
        }
    }

    pub fn make_target_machine(k1: &TypedProgram) -> TargetMachine {
        let build = k1.plan.config;
        let k1_target = build.target;
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
        let (cpu, features) = Cg::llvm_cpu_features(k1);
        let opt_level =
            if !build.optimize { OptimizationLevel::None } else { OptimizationLevel::Aggressive };
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
                Span::NONE,
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

    fn make_borrowed_ptr_attributes(&self, param_type: &CgType<'ctx>) -> [Attribute; 3] {
        let [dereferenceable, align] = self.make_pointee_attributes(param_type.rich_repr_layout());
        [self.make_enum_attribute("readonly", 0), dereferenceable, align]
    }

    fn make_pointee_attributes(&self, pointee: Layout) -> [Attribute; 2] {
        [
            self.make_enum_attribute("dereferenceable", pointee.size as u64),
            self.make_enum_attribute("align", pointee.align as u64),
        ]
    }

    fn make_i64_metadata(&self, value: u64) -> MetadataValue<'ctx> {
        self.ctx.metadata_node(&[self.ctx.i64_type().const_int(value, false).into()])
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

fn collect_ranges_from_values(width: u32, values: &mut [u64]) -> SV4<(u64, u64)> {
    let modulus = 1u128 << width;
    for v in values.iter_mut() {
        *v = (*v as u128 % modulus) as u64;
    }
    values.sort_unstable();
    let mut runs: SV4<(u128, u128)> = smallvec::smallvec![];
    for v in values.iter() {
        let v = *v as u128;
        match runs.last_mut() {
            Some((_, hi)) if *hi >= v => *hi = (*hi).max(v + 1),
            _ => runs.push((v, v + 1)),
        }
    }
    if runs.len() == 1 && runs[0] == (0, modulus) {
        return smallvec::smallvec![];
    }
    if runs.len() > 1 && runs[0].0 == 0 && runs[runs.len() - 1].1 == modulus {
        let first = runs.remove(0);
        let last = runs.last_mut().unwrap();
        last.1 = first.1;
    }
    let sign_bit = 1u128 << (width - 1);
    let mut signed_order: SV4<(u64, u64)> = smallvec::smallvec![];
    for (lo, hi) in runs {
        signed_order.push((lo as u64, (hi % modulus) as u64));
    }
    signed_order.sort_by_key(|(lo, _)| (*lo as u128) ^ sign_bit);
    signed_order
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collect_ranges_from_values_merge_and_order() {
        assert_eq!(collect_ranges_from_values(8, &mut [1, 0]).as_slice(), &[(0, 2)]);
        assert_eq!(collect_ranges_from_values(8, &mut [10, 11, 12]).as_slice(), &[(10, 13)]);
        assert_eq!(collect_ranges_from_values(8, &mut [3, 2, 1, 0]).as_slice(), &[(0, 4)]);
        assert_eq!(
            collect_ranges_from_values(8, &mut [0b11, 0b01, 0b00]).as_slice(),
            &[(0, 2), (3, 4)]
        );
        assert_eq!(collect_ranges_from_values(8, &mut [255, 0]).as_slice(), &[(255, 1)]);
        assert_eq!(collect_ranges_from_values(8, &mut [127, 128]).as_slice(), &[(127, 129)]);
        assert_eq!(collect_ranges_from_values(8, &mut [200, 5]).as_slice(), &[(200, 201), (5, 6)]);
        assert_eq!(
            collect_ranges_from_values(8, &mut [0xFFFF_FFFF_FFFF_FFFF]).as_slice(),
            &[(255, 0)]
        );
        let mut all: Vec<u64> = Vec::new();
        for v in 0..256 {
            all.push(v);
        }
        assert!(collect_ranges_from_values(8, &mut all).is_empty());
        assert_eq!(collect_ranges_from_values(64, &mut [u64::MAX, 0]).as_slice(), &[(u64::MAX, 1)]);
        assert_eq!(
            collect_ranges_from_values(32, &mut [256, 768, 769]).as_slice(),
            &[(256, 257), (768, 770)]
        );
    }

    fn assert_float_bits(ctx: &Context, value: TypedFloatValue, expected: u64) {
        let int_type = match value {
            TypedFloatValue::F32(_) => ctx.i32_type(),
            TypedFloatValue::F64(_) => ctx.i64_type(),
        };
        let constant = llvm_float_constant(ctx, value);
        let bits = unsafe {
            IntValue::new(llvm_sys::core::LLVMConstBitCast(
                constant.as_value_ref(),
                int_type.as_type_ref(),
            ))
        };
        assert_eq!(bits.get_zero_extended_constant(), Some(expected), "{expected:#018x}");
    }

    #[test]
    fn llvm_f32_constants_preserve_bits() {
        let ctx = Context::create();
        for bits in [
            0,
            0x8000_0000,
            1,
            0x8000_0001,
            0x7f7f_ffff,
            0x7f80_0000,
            0xff80_0000,
            0x7fc0_1234,
            0xffc0_1234,
            0x7f80_1234,
            0xff80_1234,
        ] {
            assert_float_bits(&ctx, TypedFloatValue::F32(f32::from_bits(bits)), bits as u64);
        }
    }

    #[test]
    fn llvm_f64_constants_preserve_bits() {
        let ctx = Context::create();
        for bits in [
            0,
            0x8000_0000_0000_0000,
            1,
            0x8000_0000_0000_0001,
            0x7fef_ffff_ffff_ffff,
            0x7ff0_0000_0000_0000,
            0xfff0_0000_0000_0000,
            0x7ff8_0000_0000_1234,
            0xfff8_0000_0000_1234,
            0x7ff0_0000_0000_1234,
            0xfff0_0000_0000_1234,
        ] {
            assert_float_bits(&ctx, TypedFloatValue::F64(f64::from_bits(bits)), bits);
        }
    }
}
