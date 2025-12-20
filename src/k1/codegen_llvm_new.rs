// Copyright (c) 2025 knix
// All rights reserved.

use std::path::Path;

use ahash::HashMapExt;
use anyhow::bail;
use either::Either;
use fxhash::FxHashMap;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIExpression, DIFile, DILocalVariable, DILocation, DIScope,
    DISubprogram, DIType, DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder,
};
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine, TargetTriple};
use inkwell::types::{
    AnyType, AnyTypeEnum, ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType, VoidType,
};
use inkwell::values::{
    ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
    GlobalValue, InstructionValue, IntValue, PointerValue, StructValue, ValueKind,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel, ThreadLocalMode};
use itertools::Itertools;
use llvm_sys::debuginfo::LLVMDIBuilderInsertDbgValueRecordAtEnd;
use log::{debug, info, trace};

use crate::bc::{
    BackendBuiltin, BcCallee, CompiledBlock, Inst, InstId, InstKind, PhysicalFunctionType,
    ProgramBytecode,
};
use crate::compiler::{self, MAC_SDK_VERSION};
use crate::kmem::{MHandle, MList, MSlice};
use crate::lex::SpanId;
use crate::parse::{FileId, Ident, StringId};
use crate::typer::scopes::ScopeId;
use crate::typer::types::{
    AbiMode, AggType, PhysicalType, PhysicalTypeId, STRING_TYPE_ID, ScalarType, TypeDefnInfo,
    TypeId,
};
use crate::typer::{
    FunctionId, Layout, Linkage as TyperLinkage, StaticContainerKind, StaticValue, StaticValueId,
    TypedFloatValue, TypedGlobalId, TypedIntValue, TypedProgram, TyperResult,
};
use crate::{SV8, bc, failf, kmem};

#[allow(unused)]
fn llvm_size_info(td: &TargetData, typ: &dyn AnyType) -> Layout {
    Layout { size: td.get_abi_size(typ) as u32, align: td.get_abi_alignment(typ) }
}

#[derive(Clone)]
pub struct CgFunctionType<'ctx> {
    llvm_function_type: LlvmFunctionType<'ctx>,
    // Does not include sret, or abi mappings
    param_k1_types: MSlice<CgType<'ctx>, CgPerm>,

    // Does not include sret
    param_abi_mappings: MSlice<AbiParamMapping, CgPerm>,
    // Should probably wrap this in a handle due to size
    return_cg_type: CgType<'ctx>,
    #[allow(unused)]
    return_abi_mapping: AbiParamMapping,
    is_sret: bool,
}

#[derive(Copy, Clone, Debug)]
enum AbiParamMapping {
    ScalarInRegister,
    /// How everyone does 1-8 byte structs
    StructInInteger {
        width: u32,
    },
    /// How clang does X86 9-16 byte structs
    StructByEightbytePair {
        class1: EightbyteClass,
        class2: EightbyteClass,
        active_bits2: u32,
    },
    /// How clang does ARM64 9-16 byte structs
    StructByIntPairArray,
    BigStructByPtrToCopy {
        byval_attr: bool,
    },
    StructByPtrNoCopy,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum EightbyteClass {
    Initial,
    Int,
    Float,
}

impl EightbyteClass {
    fn combine(&self, other: EightbyteClass) -> EightbyteClass {
        match (self, other) {
            (EightbyteClass::Initial, _) => other,
            (EightbyteClass::Int, EightbyteClass::Int) => EightbyteClass::Int,
            (EightbyteClass::Float, EightbyteClass::Float) => EightbyteClass::Float,
            // Anything can go in a general purpose register like an int, but
            // only more specific things (floats, vectors later) can go in
            // those special registers, so mixtures result in int
            _mix => EightbyteClass::Int,
        }
    }
}

//#[derive(Debug, Clone, Copy)]
//enum LlvmValue<'ctx> {
//    BasicValue(BasicValueEnum<'ctx>),
//    Void(InstructionValue<'ctx>),
//}
//impl<'ctx> LlvmValue<'ctx> {
//    fn as_basic_value(self) -> Either<InstructionValue<'ctx>, BasicValueEnum<'ctx>> {
//        match self {
//            LlvmValue::BasicValue(bv) => Either::Right(bv),
//            LlvmValue::Void(instr) => Either::Left(instr),
//        }
//    }
//    fn expect_basic_value(self) -> BasicValueEnum<'ctx> {
//        self.as_basic_value().expect_right("Expected BasicValue on never value")
//    }
//
//    #[allow(unused)]
//    fn as_never(&self) -> Option<InstructionValue<'ctx>> {
//        match self {
//            LlvmValue::Void(instr) => Some(*instr),
//            _ => None,
//        }
//    }
//}
//
//impl<'ctx> From<BasicValueEnum<'ctx>> for LlvmValue<'ctx> {
//    fn from(value: BasicValueEnum<'ctx>) -> Self {
//        LlvmValue::BasicValue(value)
//    }
//}

#[derive(Copy, Clone)]
struct LlvmReferenceType<'ctx> {
    type_id: TypeId,
    ptr_basic_type: PointerType<'ctx>,
    pointer_type: MHandle<CgType<'ctx>, CgPerm>,
    #[allow(unused)]
    pointee_type: MHandle<CgType<'ctx>, CgPerm>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct LlvmVoidType<'ctx> {
    di_type: DIType<'ctx>,
    void_type: VoidType<'ctx>,
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
    count: u32,
    array_type: ArrayType<'ctx>,
    element_type: MHandle<CgType<'ctx>, CgPerm>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Copy, Clone)]
struct CgOpaqueType<'ctx> {
    pt: PhysicalType,
    aligned_struct_repr: StructType<'ctx>,
    layout: Layout,
    di_type: DIType<'ctx>,
}

#[derive(Copy, Clone)]
enum CgType<'ctx> {
    Scalar(LlvmScalarType<'ctx>),
    StructType(CgStructType<'ctx>),
    ArrayType(CgArrayType<'ctx>),
    Reference(LlvmReferenceType<'ctx>),
    Void(LlvmVoidType<'ctx>),
    Opaque(CgOpaqueType<'ctx>),
}

impl<'ctx> From<LlvmVoidType<'ctx>> for CgType<'ctx> {
    fn from(void: LlvmVoidType<'ctx>) -> Self {
        CgType::Void(void)
    }
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

impl<'ctx> From<LlvmReferenceType<'ctx>> for CgType<'ctx> {
    fn from(value: LlvmReferenceType<'ctx>) -> Self {
        CgType::Reference(value)
    }
}

impl<'ctx> CgType<'ctx> {
    pub fn pt(&self) -> PhysicalType {
        match self {
            CgType::Scalar(s) => s.pt,
            CgType::StructType(s) => s.pt,
            CgType::ArrayType(a) => a.pt,
            CgType::Reference(_) => PhysicalType::Scalar(ScalarType::Pointer),
            CgType::Void(_) => panic!("no pt on void"),
            CgType::Opaque(o) => o.pt,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            CgType::Scalar(_) => "Scalar",
            CgType::Reference(_) => "Reference",
            CgType::Void(_) => "Void",
            CgType::StructType(_) => "StructType",
            CgType::ArrayType(_) => "ArrayType",
            CgType::Opaque(_) => "Opaque",
        }
    }

    pub fn is_aggregate(&self) -> bool {
        match self {
            CgType::Scalar(_) => false,
            CgType::Reference(_) => false,
            CgType::Void(_) => false,
            CgType::StructType(_) => true,
            CgType::ArrayType(_) => true,
            CgType::Opaque(_) => true,
        }
    }

    // Unions are impossible to encode directly in LLVM's type system.
    // We represent them, usually, as byte arrays, but these byte arrays have the wrong alignment.
    // We can correct for this in direct loads, stores, and copies, but when the union appears
    // inside another data structure, like a Struct or Array, we cannot tell LLVM its true
    // alignment, this causes GEPs to essentially ignore padding, because byte arrays have align
    // = 1.
    //
    // To fix this, any time a struct or array or even another Enum contains an Enum (union),
    // we calculate the GEP offsets ourselves in bytes, using gep i8, 0, n. As far as I can tell,
    // this is what rustc does.
    //
    // Note: I ended up doing something more sneaky where I synthesize a type with the proper
    // size and align, and no unused bits
    // #[allow(unused)]
    // pub fn requires_custom_alignment(&self) -> bool {
    //     match self {
    //         CgType::Scalar(_) => false,
    //         CgType::Reference(_) => false,
    //         CgType::Void(_) => false,
    //         CgType::StructType(_) => false,
    //         CgType::ArrayType(a) => {
    //             // If the array's elements don't have the 'right' alignment in LLVM,
    //             // then neither will the array!
    //             a.element_type.requires_custom_alignment()
    //         }
    //         CgType::LambdaObject(_) => false,
    //     }
    // }

    #[track_caller]
    #[allow(unused)]
    pub fn expect_reference(&self) -> &LlvmReferenceType<'ctx> {
        match self {
            CgType::Reference(reference) => reference,
            _ => panic!("expected pointer on {}", self.kind_name()),
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
    fn expect_array(self) -> CgArrayType<'ctx> {
        match self {
            CgType::ArrayType(array) => array,
            _ => panic!("expected array on {}", self.kind_name()),
        }
    }

    fn rich_repr_layout(&self) -> Layout {
        match self {
            CgType::Scalar(value) => value.layout,
            CgType::Reference(pointer) => pointer.layout,
            CgType::StructType(s) => s.layout,
            CgType::ArrayType(a) => a.layout,
            CgType::Void(_) => panic!("No rich value type on Void / never"),
            CgType::Opaque(o) => o.layout,
        }
    }

    fn rich_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            CgType::Scalar(value) => value.basic_type,
            CgType::Reference(r) => r.ptr_basic_type.as_basic_type_enum(),
            CgType::StructType(s) => s.struct_type.as_basic_type_enum(),
            CgType::ArrayType(a) => a.array_type.as_basic_type_enum(),
            CgType::Opaque(o) => o.aligned_struct_repr.as_basic_type_enum(),
            CgType::Void(_) => panic!("No rich value type on Void / never"),
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            CgType::Scalar(value) => value.di_type,
            CgType::Reference(pointer) => pointer.di_type,
            CgType::StructType(s) => s.di_type,
            CgType::ArrayType(a) => a.di_type,
            CgType::Opaque(o) => o.di_type,
            CgType::Void(v) => v.di_type,
        }
    }

    fn is_void(&self) -> bool {
        match self {
            CgType::Void(_) => true,
            _ => false,
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

impl<'ctx> BuiltinTypes<'ctx> {
    pub fn ptr_basic(&self) -> BasicTypeEnum<'ctx> {
        self.ptr.as_basic_type_enum()
    }
    pub fn unit_basic(&self) -> BasicValueEnum<'ctx> {
        self.unit_value.as_basic_value_enum()
    }
}

pub struct CgFunction<'ctx> {
    pub function_type: CgFunctionType<'ctx>,
    pub function_value: FunctionValue<'ctx>,
    /// These are canonical, not ABI-mapped, and also logical, as in,
    /// sret is excluded, so the first item is the first param the function actually takes
    pub param_values: Vec<BasicValueEnum<'ctx>>,
    pub sret_pointer: Option<PointerValue<'ctx>>,
    pub last_alloca_instr: Option<InstructionValue<'ctx>>,
    pub instruction_count: usize,
    pub debug_info: DISubprogram<'ctx>,
    pub debug_file: DIFile<'ctx>,
}

pub struct CgPerm;
pub struct CodegenTmp;

pub struct Cg<'ctx, 'k1> {
    ctx: &'ctx Context,
    pub k1: &'k1 mut TypedProgram,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: TargetMachine,
    builder: Builder<'ctx>,
    pub llvm_functions: FxHashMap<FunctionId, CgFunction<'ctx>>,
    pub llvm_function_to_k1: FxHashMap<FunctionValue<'ctx>, FunctionId>,
    // nocommit fix this double-buffer thingy
    functions_pending_body_compilation: Vec<FunctionId>,
    llvm_types: FxHashMap<PhysicalTypeId, CgType<'ctx>>,
    globals: FxHashMap<TypedGlobalId, GlobalValue<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    strings: FxHashMap<StringId, GlobalValue<'ctx>>,
    static_values_basics: FxHashMap<StaticValueId, BasicValueEnum<'ctx>>,
    static_values_globals: FxHashMap<StaticValueId, GlobalValue<'ctx>>,
    debug: DebugContext<'ctx>,
    tmp: kmem::Mem<CodegenTmp>,
    mem: kmem::Mem<CgPerm>,

    current_insert_function: FunctionId,
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

    fn create_pointer_type(&self, name: &str, pointee: DIType<'ctx>) -> DIType<'ctx> {
        self.debug_builder
            .create_pointer_type(
                name,
                pointee,
                crate::WORD_SIZE_BITS as u64,
                crate::WORD_SIZE_BITS as u32,
                AddressSpace::default(),
            )
            .as_type()
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
        let sysroot = match module.ast.config.target.target_os() {
            compiler::TargetOs::Linux => "",
            compiler::TargetOs::MacOs => compiler::MAC_SDK_SYSROOT,
            compiler::TargetOs::Wasm => "",
        };
        let sdk = match module.ast.config.target.target_os() {
            compiler::TargetOs::Linux => "",
            compiler::TargetOs::MacOs => "MacOSX.sdk",
            compiler::TargetOs::Wasm => "",
        };
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
        let md4 = ctx.metadata_node(&[
            ctx.i32_type().const_int(1, false).into(),
            ctx.metadata_string("PIE Level").into(),
            ctx.i32_type().const_int(2, false).into(),
        ]);
        // revisit this metadata (I did it when scrambling to get debug info to show up)
        // I know that at least the dwarf version is required by lldb
        llvm_module.add_global_metadata("llvm.module.flags", &md0).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md1).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md2).unwrap();
        llvm_module.add_global_metadata("llvm.module.flags", &md3).unwrap();
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
        debug.push_scope(SpanId::NONE, compile_unit.as_debug_info_scope(), compile_unit.get_file());
        debug
    }

    pub fn create(
        ctx: &'ctx Context,
        module: &'module mut TypedProgram,
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

        let debug_context = Cg::init_debug(ctx, &llvm_module, module, optimize, debug);

        let machine = Cg::set_up_machine(&mut llvm_module);
        let target_data = machine.get_target_data();

        let ptr = ctx.ptr_type(AddressSpace::default());
        let builtin_types = BuiltinTypes {
            unit: ctx.i8_type(),
            unit_value: ctx.i8_type().const_int(crate::typer::UNIT_BYTE_VALUE as u64, false),
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

        Cg {
            ctx,
            k1: module,
            llvm_module,
            llvm_machine: machine,
            builder,
            globals: FxHashMap::new(),
            //lambda_functions: FxHashMap::new(),
            llvm_functions: FxHashMap::new(),
            llvm_function_to_k1: FxHashMap::new(),
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
        }
    }

    #[allow(unused)]
    fn llvm_size_info(&self, typ: &dyn AnyType) -> Layout {
        let td = self.llvm_machine.get_target_data();
        llvm_size_info(&td, typ)
    }

    fn get_layout(&self, type_id: TypeId) -> Layout {
        self.k1.types.get_layout(type_id)
    }

    fn padding_type(&self, size_bytes: u32) -> ArrayType<'ctx> {
        self.ctx.i8_type().array_type(size_bytes)
    }

    fn set_debug_location_from_span(&self, span: SpanId) -> DILocation<'ctx> {
        let span = self.k1.ast.spans.get(span);
        let line = self.k1.ast.sources.get_line_for_span_start(span).expect("No line for span");
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

    #[allow(unused)]
    fn set_debug_location(&self, locn: DILocation<'ctx>) {
        self.builder.set_current_debug_location(locn)
    }

    fn get_debug_location(&self) -> DILocation<'ctx> {
        self.builder.get_current_debug_location().unwrap()
    }

    fn get_ident_name(&self, id: Ident) -> &str {
        self.k1.ast.idents.get_name(id)
    }

    fn get_line_number(&self, span: SpanId) -> u32 {
        let span = self.k1.ast.spans.get(span);
        let line = self.k1.ast.sources.get_line_for_span_start(span).expect("No line for span");
        line.line_index + 1
    }

    fn write_type_name(
        &self,
        w: &mut impl std::fmt::Write,
        type_id: TypeId,
        defn_info: Option<TypeDefnInfo>,
    ) {
        // FIXME: Using this typename is bad; it needs to be purpose-built and mangled
        let name = self.k1.type_id_to_string(type_id);
        match defn_info {
            None => write!(w, "{}", name).unwrap(),
            Some(info) => self.k1.write_qualified_name(w, info.scope, &name, "/", true),
        };
    }

    fn codegen_type_name(&self, type_id: TypeId) -> String {
        let defn_info = self.k1.types.get_defn_info(type_id);
        let mut s = String::with_capacity(64);
        self.write_type_name(&mut s, type_id, defn_info);
        s
    }

    const DW_ATE_ADDRESS: u32 = 0x01;
    const DW_ATE_BOOLEAN: u32 = 0x02;
    const _DW_ATE_COMPLEX_FLOAT: u32 = 0x03;
    const DW_ATE_FLOAT: u32 = 0x04;
    const DW_ATE_SIGNED: u32 = 0x05;
    const DW_ATE_CHAR: u32 = 0x06;
    const DW_ATE_UNSIGNED: u32 = 0x07;
    const _DW_ATE_UNSIGNED_CHAR: u32 = 0x08;

    fn codegen_inst_kind(&mut self, inst_kind: InstKind) -> CgType<'ctx> {
        match inst_kind {
            InstKind::Void | InstKind::Terminator => CgType::Void(LlvmVoidType {
                di_type: self
                    .debug
                    .debug_builder
                    .create_basic_type("void", 0, Self::DW_ATE_CHAR, 0)
                    .unwrap()
                    .as_type(),
                void_type: self.ctx.void_type(),
            }),
            InstKind::Value(pt) => self.codegen_type(pt),
        }
    }

    // FIXME(newcodgen): CgType is too big to return by value
    fn codegen_type(&mut self, pt: PhysicalType) -> CgType<'ctx> {
        match pt {
            PhysicalType::Scalar(st) => {
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
                };
                let basic_type = self.scalar_basic_type(st);
                let di_type = self
                    .debug
                    .debug_builder
                    .create_basic_type(name, layout.size_bits() as u64, encoding, 0)
                    .unwrap()
                    .as_type();
                CgType::Scalar(LlvmScalarType { pt, basic_type, layout, di_type })
            }
            PhysicalType::Agg(agg_id) => {
                if let Some(k1) = self.llvm_types.get(&agg_id) {
                    return *k1;
                }
                let agg = self.k1.types.phys_types.get(agg_id);
                let agg_layout = agg.layout;
                let type_name = self.codegen_type_name(agg.origin_type_id);
                let cg_type = match agg.agg_type {
                    AggType::EnumVariant(e) => panic!("Who is asking"),
                    AggType::Struct { fields } => {
                        let mut cg_field_types = self.mem.new_list(fields.len());
                        let mut field_rich_types = self.tmp.new_list(fields.len());
                        let mut field_di_types = self.tmp.new_list(fields.len());
                        let k1_struct_type = self.k1.types.get(agg.origin_type_id).expect_struct();
                        let span = self.debug.current_span();
                        let line_number = self.get_line_number(span);
                        for (phys_field, k1_field) in self
                            .k1
                            .types
                            .mem
                            .getn(fields)
                            .iter()
                            .zip(self.k1.types.mem.getn(k1_struct_type.fields))
                        {
                            let cg_type = self.codegen_type(phys_field.field_t);
                            let field_name = self.k1.ident_str(k1_field.name);
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
                        let struct_type = self.ctx.struct_type(&field_rich_types, false);
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
                            fields: cg_field_types.into_handle(&mut self.mem),
                            di_type,
                            layout: agg_layout,
                        })
                    }
                    AggType::Array { element_pt, len } => {
                        let element_type = self.codegen_type(element_pt);
                        let array_type = element_type.rich_type().array_type(len);
                        let layout = self.k1.types.get_pt_layout(element_pt);
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_array_type(
                                element_type.debug_type(),
                                layout.size_bits() as u64,
                                layout.align_bits(),
                                &[],
                            )
                            .as_type();
                        CgType::ArrayType(CgArrayType {
                            pt,
                            count: len,
                            array_type,
                            element_type: self.mem.push_h(element_type),
                            di_type,
                            layout,
                        })
                    }
                    AggType::Opaque { layout } => {
                        // For opaque types, which we currently only use to represent our tagged
                        // unions (`either` in the source), we generate a 2-field struct to trick
                        // LLVM.
                        // Field 1 is a synthetic integer wide enough to force the alignment of the
                        // struct, and Field 2 is an array of bytes, ensuring NO padding at all,
                        // large enough to get the whole thing to be exactly `size`. This is
                        // mostly what clang does for unions, and probably what I'll also do for
                        // unions once I have them
                        let aligner_type = self.ctx.custom_width_int_type(layout.align_bits());
                        let padding_bytes = layout.size - (aligner_type.get_bit_width() / 8);
                        let padding = self.padding_type(padding_bytes);
                        let aligned_struct_repr = self.ctx.struct_type(
                            &[aligner_type.as_basic_type_enum(), padding.as_basic_type_enum()],
                            false,
                        );
                        // nocommit: Once we introduce 'Union' as a physical aggregate instead of
                        // using Opaque here, we'll be able to generate a good debug type for our
                        // tagged unions. But for now, we'll generate a garbage one
                        let di_type = self
                            .debug
                            .debug_builder
                            .create_basic_type("opaque", layout.size_bits() as u64, 0, 0)
                            .unwrap()
                            .as_type();
                        CgType::Opaque(CgOpaqueType { pt, aligned_struct_repr, layout, di_type })
                    }
                };
                self.llvm_types.insert(agg_id, cg_type);
                cg_type
            }
        }
    }

    fn scalar_basic_type(&self, st: ScalarType) -> BasicTypeEnum<'ctx> {
        match st {
            ScalarType::U8 => self.ctx.i8_type().into(),
            ScalarType::U16 => self.ctx.i16_type().into(),
            ScalarType::U32 => self.ctx.i32_type().into(),
            ScalarType::U64 => self.ctx.i64_type().into(),
            ScalarType::I8 => self.ctx.i8_type().into(),
            ScalarType::I16 => self.ctx.i16_type().into(),
            ScalarType::I32 => self.ctx.i32_type().into(),
            ScalarType::I64 => self.ctx.i64_type().into(),
            ScalarType::F32 => self.ctx.f32_type().into(),
            ScalarType::F64 => self.ctx.f64_type().into(),
            ScalarType::Pointer => self.builtin_types.ptr.into(),
        }
    }

    fn pt_canon_type(&self, pt: PhysicalType) -> BasicTypeEnum<'ctx> {
        match pt {
            PhysicalType::Scalar(st) => self.scalar_basic_type(st),
            PhysicalType::Agg(_) => self.builtin_types.ptr.as_basic_type_enum(),
        }
    }

    fn pt_rich_type(&mut self, pt: PhysicalType) -> BasicTypeEnum<'ctx> {
        match pt {
            PhysicalType::Scalar(st) => self.scalar_basic_type(st),
            PhysicalType::Agg(_) => {
                let k1_llvm_ty = self.codegen_type(pt);
                k1_llvm_ty.rich_type()
            }
        }
    }

    fn pt_debug_type(&mut self, pt: PhysicalType) -> DIType<'ctx> {
        let k1_llvm_ty = self.codegen_type(pt);
        k1_llvm_ty.debug_type()
    }

    fn canonical_repr_type(&self, t: &CgType<'ctx>) -> BasicTypeEnum<'ctx> {
        match t {
            CgType::Scalar(value) => value.basic_type,
            CgType::Reference(_) => self.builtin_types.ptr_basic(),
            CgType::StructType(_) => self.builtin_types.ptr_basic(),
            CgType::ArrayType(_) => self.builtin_types.ptr_basic(),
            CgType::Void(_) => panic!("No canonical repr type on Void"),
            CgType::Opaque(_) => self.builtin_types.ptr_basic(),
        }
    }

    // nocommit does this need to exist by the end
    fn rich_repr_type(&self, t: &CgType<'ctx>) -> BasicTypeEnum<'ctx> {
        match t {
            CgType::Scalar(value) => value.basic_type,
            CgType::Reference(_) => self.builtin_types.ptr_basic(),
            CgType::StructType(s) => s.struct_type.as_basic_type_enum(),
            CgType::ArrayType(a) => a.array_type.as_basic_type_enum(),
            CgType::Opaque(opaque) => opaque.aligned_struct_repr.as_basic_type_enum(),
            CgType::Void(_) => panic!("No rich value type on Void / never"),
        }
    }

    fn make_cg_function_type(
        &mut self,
        phys_fn_type: &PhysicalFunctionType,
    ) -> TyperResult<CgFunctionType<'ctx>> {
        let return_type = phys_fn_type.return_type;
        let abi_mode = phys_fn_type.abi_mode;
        let param_types = phys_fn_type.params;
        let return_cg_type = self.codegen_inst_kind(return_type);
        let return_type_abi_mapping = match return_type {
            InstKind::Void | InstKind::Terminator => AbiParamMapping::ScalarInRegister,
            InstKind::Value(return_type) => {
                self.get_abi_mapping_for_type(abi_mode, return_type, true)
            }
        };
        // eprintln!("make fn type {}", self.k1.type_id_to_string(function_type_id));
        let return_mapped_type = match return_type {
            InstKind::Void | InstKind::Terminator => None,
            InstKind::Value(return_type) => {
                Some(self.mapped_abi_type(return_type, return_type_abi_mapping))
            }
        };
        let is_sret = match return_type_abi_mapping {
            AbiParamMapping::ScalarInRegister => false,
            AbiParamMapping::StructInInteger { .. } => false,
            AbiParamMapping::StructByEightbytePair { .. } => false,
            AbiParamMapping::StructByIntPairArray => false,
            AbiParamMapping::BigStructByPtrToCopy { .. } => true,
            AbiParamMapping::StructByPtrNoCopy => true,
        };

        let param_count = param_types.len() as u32 + if is_sret { 1 } else { 0 };

        // The logical parameters closest to K1 model
        let mut param_llvm_types: MList<CgType<'ctx>, _> = self.mem.new_list(param_count);
        // Foreach k1 param above, describe how to map it to LLVM params
        let mut param_abi_mappings: MList<AbiParamMapping, _> = self.mem.new_list(param_count);

        // The physical LLVM params; the ones the function will have.
        // For now this is 1:1 in count with the logical params, as I choose to pass the int pairs
        // in a struct, but it need not be; that is, 1 k1 param could result in n llvm params,
        // where n could even be 0 for a ZST or uninhabited type
        let mut function_final_params: MList<BasicMetadataTypeEnum<'ctx>, _> =
            self.mem.new_list(param_count);

        // If a function returns a big (typically > 2 words) struct, its actually
        // 'returned' in the first parameter, which is a pointer
        if is_sret {
            let Some(return_mapped_type) = return_mapped_type else {
                return failf!(self.debug.current_span(), "sret but no return_mapped_type");
            };
            debug_assert!(return_mapped_type.is_pointer_type());
            function_final_params.push(return_mapped_type.into());
        }

        for p in self.k1.bytecode.mem.getn(param_types) {
            let param_llvm_type = self.codegen_type(*p);
            let abi_mapping = self.get_abi_mapping_for_type(abi_mode, *p, false);
            // eprintln!("abi mapping for {} is {:?}", self.rich_repr_type(&param_type), abi_mapping);
            param_abi_mappings.push(abi_mapping);
            param_llvm_types.push(param_llvm_type);
            let mapped_type = self.mapped_abi_type(*p, abi_mapping);
            // eprintln!(
            //     "abi mapping for {} is {:?}. Mapped type: {}",
            //     self.rich_repr_type(&param_type),
            //     abi_mapping,
            //     mapped_type
            // );
            function_final_params.push(mapped_type.into());
        }

        let fn_type = match return_mapped_type {
            None => self.ctx.void_type().fn_type(&function_final_params, false),
            Some(t) => t.fn_type(&function_final_params, false),
        };

        Ok(CgFunctionType {
            llvm_function_type: fn_type,
            param_k1_types: self.mem.list_to_handle(param_llvm_types),
            param_abi_mappings: self.mem.list_to_handle(param_abi_mappings),
            return_cg_type,
            return_abi_mapping: return_type_abi_mapping,
            is_sret,
        })
    }

    fn mapped_abi_type(
        &self,
        pt: PhysicalType,
        abi_mapping: AbiParamMapping,
    ) -> BasicTypeEnum<'ctx> {
        match abi_mapping {
            AbiParamMapping::ScalarInRegister => self.pt_canon_type(pt),
            AbiParamMapping::StructInInteger { width } => {
                self.ctx.custom_width_int_type(width).as_basic_type_enum()
            }
            AbiParamMapping::StructByEightbytePair { class1, class2, active_bits2 } => {
                // We know field 1 is a full 8 bits
                let f1 = match class1 {
                    EightbyteClass::Initial => panic!("Got Initial EightbyteClass"),
                    EightbyteClass::Int => self.ctx.i64_type().as_basic_type_enum(),
                    EightbyteClass::Float => self.ctx.f64_type().as_basic_type_enum(),
                };
                let f2 = match (class2, active_bits2) {
                    (EightbyteClass::Initial, _) => panic!("Got Initial EightbyteClass"),
                    (EightbyteClass::Int, bits) => {
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
                    (EightbyteClass::Float, bits) => {
                        if bits <= 32 {
                            self.ctx.f32_type().as_basic_type_enum()
                        } else {
                            self.ctx.f64_type().as_basic_type_enum()
                        }
                    }
                };
                self.ctx.struct_type(&[f1, f2], false).as_basic_type_enum()
            }
            AbiParamMapping::StructByIntPairArray => {
                self.ctx.i64_type().array_type(2).as_basic_type_enum()
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } => {
                self.builtin_types.ptr.as_basic_type_enum()
            }
            AbiParamMapping::StructByPtrNoCopy => self.builtin_types.ptr.as_basic_type_enum(),
        }
    }

    /// Takes a value either passed to or returned from a function and converts it back
    /// to how the compiler expects that type to be represented
    fn canonicalize_abi_param_value(
        &mut self,
        mapping: AbiParamMapping,
        k1_ty: &CgType<'ctx>,
        abi_value: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        debug!("canonicalizing {} to {} via {:?}", abi_value, self.rich_repr_type(k1_ty), mapping);
        match mapping {
            AbiParamMapping::ScalarInRegister => abi_value,
            AbiParamMapping::StructInInteger { width } => {
                let truncated = self
                    .builder
                    .build_int_truncate(
                        abi_value.into_int_value(),
                        self.ctx.custom_width_int_type(width),
                        "",
                    )
                    .unwrap();
                let ptr = self.build_k1_alloca(k1_ty, "struct_in_integer_storage");
                self.builder.build_store(ptr, truncated).unwrap();
                ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructByEightbytePair { .. } => {
                let dst_ptr = self.build_k1_alloca(k1_ty, "struct_by_ebpair_storage");
                debug_assert!(abi_value.get_type().is_struct_type());
                // Yes its a struct store but its guaranteed to be only 2 members, so lets try it out.
                // clang for x86 actually has 2 scalar BasicValues at this point (2 params vs 1 struct),
                // but this should work too
                self.builder.build_store(dst_ptr, abi_value).unwrap();
                dst_ptr.as_basic_value_enum()
            }
            AbiParamMapping::StructByIntPairArray => {
                let dst_ptr = self.build_k1_alloca(k1_ty, "struct_by_intpairarray_storage");
                debug_assert!(abi_value.get_type().is_array_type());
                // Clang performs this exact array store ([2 x i64])
                self.builder.build_store(dst_ptr, abi_value).unwrap();
                dst_ptr.as_basic_value_enum()
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } => {
                // Our canonical representation of all aggregates is an llvm ptr
                // And this abi route represents them as a ptr, so nothing to do
                abi_value
            }
            AbiParamMapping::StructByPtrNoCopy => abi_value,
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
        debug!(
            "marshalling k1 {}: {} with {:?}",
            k1_value,
            self.k1.types.pt_to_string(pt),
            mapping
        );
        match mapping {
            AbiParamMapping::ScalarInRegister => k1_value,
            AbiParamMapping::StructInInteger { width } => {
                let abi_type = self.mapped_abi_type(pt, mapping);
                let integer_ptr = self.build_alloca(abi_type, "abi_struct_int");

                // %1 = alloca %struct.Small2, align 1
                // %2 = alloca i64, align 8
                // call void @llvm.memset.p0.i64(ptr align 1 %1, i8 0, i64 6, i1 false)
                // ..
                // call void @llvm.memcpy.p0.p0.i64(ptr align 8 %2, ptr align 1 %1, i64 6, i1 false)
                // %3 = load i64, ptr %2, align 8
                // call void @takes_small(i64 %3)

                let src_layout = self.k1.types.get_pt_layout(pt);
                self.builder
                    .build_memcpy(
                        integer_ptr,
                        width,
                        k1_value.into_pointer_value(),
                        src_layout.align,
                        self.builtin_types.ptr_sized_int.const_int(src_layout.size as u64, false),
                    )
                    .unwrap();
                let integer_value = self.builder.build_load(abi_type, integer_ptr, "").unwrap();
                integer_value
            }
            AbiParamMapping::StructByEightbytePair { .. } => {
                //define dso_local void @call_eb_pair_mixed() #0 {
                //  %1 = alloca %struct.Classes, align 4
                //  call void @llvm.memset.p0.i64(ptr align 4 %1, i8 0, i64 16, i1 false)
                //  ..
                //  %2 = getelementptr inbounds { i64, i64 }, ptr %1, i32 0, i32 0
                //  %3 = load i64, ptr %2, align 4
                //  %4 = getelementptr inbounds { i64, i64 }, ptr %1, i32 0, i32 1
                //  %5 = load i64, ptr %4, align 4
                //  call void @eb_pair_mixed(i64 %3, i64 %5)
                //  ret void
                //}
                // This is different than what clang does above, for x86, but I really don't
                // want to map 1 param to 2 params, and have good reason to believe this struct
                // load is going to work, since the array load works for ARM, and the struct load
                // works for returns
                //
                // Here's clang returning a mixed class eightbyte pair using an aggregate load
                // %3 = load { i64, float }, ptr %2, align 8
                // ret { i64, float } %3
                let abi_type = self.mapped_abi_type(pt, mapping);

                // This load should also accomplish the necessary copy to make this semantically
                // by-value op
                let loaded_aggregate =
                    self.builder.build_load(abi_type, k1_value.into_pointer_value(), "").unwrap();
                loaded_aggregate
            }
            AbiParamMapping::StructByIntPairArray => {
                // define void @call_eb_pair_mixed() #0 {
                //   %1 = alloca %struct.Classes, align 4
                //   call void @llvm.memset.p0.i64(ptr align 4 %1, i8 0, i64 16, i1 false)
                //   ..
                //   %2 = load [2 x i64], ptr %1, align 4
                //   call void @eb_pair_mixed([2 x i64] %2)
                //   ret void
                // }
                let abi_type = self.mapped_abi_type(pt, mapping);

                // This load should also accomplish the necessary copy to make this semantically
                // by-value op
                let loaded_aggregate =
                    self.builder.build_load(abi_type, k1_value.into_pointer_value(), "").unwrap();
                loaded_aggregate
            }
            AbiParamMapping::BigStructByPtrToCopy { .. } => {
                // Our canonical representation of all aggregates is an llvm ptr
                // And this abi route represents them as a ptr, already.
                //
                // But, this is truly the 'value' getting passed to the C code, then
                // this would allow mutation incorrectly, and we do have to make a copy
                // So, if this is not a return, but a param marshal, we'd make a copy
                if is_return {
                    // For returns its moot, the ptr is already a caller-owned slot
                    k1_value
                } else {
                    let callers_copy = self.alloca_copy_entire_value(
                        k1_value.into_pointer_value(),
                        cg_ty,
                        "abi_callers_copy",
                    );
                    callers_copy.as_basic_value_enum()
                }
            }
            AbiParamMapping::StructByPtrNoCopy => {
                // Our canonical representation of all aggregates is an llvm ptr
                // And this abi route represents them as a ptr, so nothing to do
                //
                // This is possible (avoiding the copy) because k1 does not allow mutation of a struct
                // value, it has to be a language-level pointer
                k1_value
            }
        }
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
        &mut self,
        cg_type: &CgType<'ctx>,
        source: PointerValue<'ctx>,
        name: &str,
        make_copy: bool,
    ) -> BasicValueEnum<'ctx> {
        if cg_type.is_aggregate() {
            if make_copy {
                self.alloca_copy_entire_value(source, cg_type, &format!("{name}_copy"))
                    .as_basic_value_enum()
            } else {
                // No-op; we want to interact with these types as pointers
                debug!("smart loading noop on type {}", self.k1.types.pt_to_string(cg_type.pt()));
                source.as_basic_value_enum()
            }
        } else {
            // Scalars must be truly loaded
            self.builder.build_load(self.rich_repr_type(cg_type), source, name).unwrap()
        }
    }

    /// Handles the storage of aggregates by doing a (hopefully) correct memcpy
    fn store_k1_value(
        &self,
        cg_type: &CgType<'ctx>,
        dest: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> InstructionValue<'ctx> {
        if cg_type.is_aggregate() {
            self.memcpy_k1_value(dest, value.into_pointer_value(), cg_type)
        } else {
            let store = self.builder.build_store(dest, value).unwrap();
            store.set_alignment(cg_type.rich_repr_layout().align).unwrap();
            store
        }
    }

    fn alloca_copy_entire_value(
        &mut self,
        src: PointerValue<'ctx>,
        ty: &CgType<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let dst = self.build_k1_alloca(ty, name);
        self.memcpy_k1_value(dst, src, ty);
        dst
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

    fn get_insert_function(&self) -> &CgFunction<'ctx> {
        let codegened_function = self.llvm_functions.get(&self.current_insert_function).unwrap();
        codegened_function
    }

    fn get_nth_block(&self, index: usize) -> TyperResult<BasicBlock<'ctx>> {
        match self.get_insert_function().function_value.get_basic_block_iter().nth(index) {
            Some(bb) => Ok(bb),
            None => failf!(self.debug.current_span(), "Failed to get nth block: {index}"),
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
        struct_type: &CgStructType<'ctx>,
        idx: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        // if struct_type.manual_field_geps {
        //     let struct_agg_id =
        //         self.k1.types.get_physical_type(struct_type.type_id).unwrap().expect_agg();
        //     let offset = self.k1.types.get_struct_field_offset(struct_agg_id, idx).unwrap();
        //     unsafe {
        //         self.builder
        //             .build_in_bounds_gep(
        //                 self.ctx.i8_type(),
        //                 ptr,
        //                 &[self.builtin_types.ptr_sized_int.const_int(offset as u64, false)],
        //                 name,
        //             )
        //             .unwrap()
        //     }
        // } else {
        self.builder.build_struct_gep(struct_type.struct_type, ptr, idx, name).unwrap()
        //}
    }

    fn append_basic_block(&mut self, name: &str) -> BasicBlock<'ctx> {
        let origin_block = self.builder.get_insert_block().unwrap();
        let current_fn = origin_block.get_parent().unwrap();
        let block = self.ctx.append_basic_block(current_fn, name);
        block
    }

    fn bool_to_i1(&self, bool: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_truncate(bool, self.builtin_types.i1, name).unwrap()
    }

    fn i1_to_bool(&self, i1: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        self.builder.build_int_cast_sign_flag(i1, self.builtin_types.boolean, false, name).unwrap()
    }

    fn build_k1_alloca(&mut self, ty: &CgType<'ctx>, name: &str) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(self.rich_repr_type(ty), name);
        ptr.as_instruction().unwrap().set_alignment(ty.rich_repr_layout().align).unwrap();
        ptr
    }

    /// Inserts an alloca in the entry block of the function
    fn build_alloca<T: BasicType<'ctx>>(&mut self, ty: T, name: &str) -> PointerValue<'ctx> {
        let original_block = self.builder.get_insert_block().unwrap();
        let f = self.get_insert_function();
        let function_entry_block = f.function_value.get_first_basic_block().unwrap();

        // Position the builder
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
        self.get_insert_function_mut().last_alloca_instr = Some(alloca.as_instruction().unwrap());

        // Restore the builder's position
        self.builder.position_at_end(original_block);
        alloca
    }

    fn codegen_function_call(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        call_id: bc::BcCallId,
    ) -> TyperResult<Option<BasicValueEnum<'ctx>>> {
        let call = self.k1.bytecode.calls.get(call_id);
        let callee = call.callee;
        let call_args = call.args;

        enum CallKind<'ctx> {
            Direct(FunctionId),
            Indirect(PointerValue<'ctx>),
        }
        let (llvm_callee, cg_fn_type) = match callee {
            BcCallee::Builtin(function_id, _)
            | BcCallee::Direct(function_id)
            | BcCallee::Extern(_, _, function_id) => {
                self.declare_llvm_function(function_id)?;
                let fn_type = self.llvm_functions.get(&function_id).unwrap().function_type.clone();
                (CallKind::Direct(function_id), fn_type)
            }
            BcCallee::Indirect(fn_type, value) => {
                let callee_value = self.codegen_value(inst_mappings, value)?.into_pointer_value();
                let cg_fn_type = self.make_cg_function_type(&fn_type)?;
                (CallKind::Indirect(callee_value), cg_fn_type)
            }
        };

        let mut args: MList<BasicMetadataValueEnum<'ctx>, _> =
            self.mem.new_list(cg_fn_type.llvm_function_type.count_param_types());

        let sret_alloca = if cg_fn_type.is_sret {
            let rich_type = cg_fn_type.return_cg_type.rich_type();
            let sret_alloca = self.build_alloca(rich_type, "call_sret");
            args.push(sret_alloca.into());
            Some(sret_alloca)
        } else {
            None
        };
        for (index, arg_bc_value) in self.k1.bytecode.mem.getn(call_args).iter().enumerate() {
            let arg_value = self.codegen_value(inst_mappings, *arg_bc_value)?;

            let param_k1_ty = *self.mem.get_nth_lt(cg_fn_type.param_k1_types, index);
            let abi_mapping = *self.mem.get_nth_lt(cg_fn_type.param_abi_mappings, index);
            let value_marshalled =
                self.marshal_abi_param_value(abi_mapping, &param_k1_ty, arg_value, false);
            trace!("codegen function call arg type: {}", value_marshalled);
            args.push(value_marshalled.into())
        }

        let callsite_value = match llvm_callee {
            CallKind::Direct(function_id) => {
                let function_value = self.declare_llvm_function(function_id)?;
                //self.set_debug_location_from_span(call.span);
                self.builder.build_call(function_value, &args, "").unwrap()
            }
            CallKind::Indirect(fn_ptr) => {
                //self.set_debug_location_from_span(call.span);
                let call_site_value = self
                    .builder
                    .build_indirect_call(cg_fn_type.llvm_function_type, fn_ptr, &args, "")
                    .unwrap();
                call_site_value
            }
        };

        if cg_fn_type.is_sret {
            let sret_attribute = self.make_sret_attribute(
                self.rich_repr_type(&cg_fn_type.return_cg_type).as_any_type_enum(),
            );
            callsite_value.add_attribute(AttributeLoc::Param(0), sret_attribute);
        };
        match callsite_value.try_as_basic_value() {
            ValueKind::Basic(returned_value) => {
                let canonical_value = self.canonicalize_abi_param_value(
                    cg_fn_type.return_abi_mapping,
                    &cg_fn_type.return_cg_type,
                    returned_value,
                );
                Ok(Some(canonical_value))
            }
            ValueKind::Instruction(_instr) => {
                if cg_fn_type.is_sret {
                    let sret_pointer = sret_alloca.unwrap();
                    Ok(sret_pointer.as_basic_value_enum().into())
                } else if cg_fn_type.return_cg_type.is_void() {
                    let _unreachable = self.builder.build_unreachable().unwrap();
                    Ok(None)
                } else {
                    panic!("Function returned LLVM void but wasn't typed as never and was not sret")
                }
            }
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

    fn find_libc_function_id(&self, ident: Ident) -> Option<FunctionId> {
        self.k1
            .functions
            .iter_with_ids()
            .find(|(_, f)| f.scope == self.k1.scopes.mem_scope_id && f.name == ident)
            .map(|(id, _)| id)
    }

    // nocommit: Can I write these in C, or k1, or llvm, and load them?
    fn codegen_builtin_function_body(
        &mut self,
        builtin_type: BackendBuiltin,
        function_id: FunctionId,
    ) -> TyperResult<InstructionValue<'ctx>> {
        let function = self.k1.get_function(function_id);
        let function_span =
            self.k1.ast.get_function(function.parsed_id.as_function_id().unwrap()).signature_span;
        self.set_debug_location_from_span(function_span);

        let instr = match builtin_type {
            BackendBuiltin::Allocate => {
                // intern fn alloc(size: uword, align: uword): Pointer
                let size_arg = self.load_function_argument(function_id, 0);

                let malloc_ident = self.k1.ast.idents.intern("malloc");
                let Some(malloc_fn_id) = self.find_libc_function_id(malloc_ident) else {
                    return failf!(
                        function_span,
                        "Failed to find libc malloc function for Allocate builtin"
                    );
                };
                let malloc_fv = self.declare_llvm_function(malloc_fn_id)?;
                let call = self.builder.build_call(malloc_fv, &[size_arg], "").unwrap();
                let result = call.try_as_basic_value().basic().unwrap();
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::AllocateZeroed => {
                // intern fn allocZeroed(size: uword, align: uword): Pointer
                let size_arg = self.load_function_argument(function_id, 0);
                let count_one =
                    self.builtin_types.ptr_sized_int.const_int(1, false).as_basic_value_enum();

                let calloc_ident = self.k1.ast.idents.intern("calloc");
                let calloc_fn_id = self.find_libc_function_id(calloc_ident).unwrap();
                let calloc_fv = self.declare_llvm_function(calloc_fn_id)?;
                // libc/calloc(count = 1, size = size);
                let call =
                    self.builder.build_call(calloc_fv, &[count_one.into(), size_arg], "").unwrap();
                let result = call.try_as_basic_value().expect_basic("calloc return");
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::Reallocate => {
                // intern fn realloc(ptr: Pointer, oldSize: uword, align: uword, newSize: uword): Pointer
                let old_ptr_arg = self.load_function_argument(function_id, 0);
                let new_size_arg = self.load_function_argument(function_id, 3);

                let realloc_ident = self.k1.ast.idents.intern("realloc");
                let realloc_fn_id = self.find_libc_function_id(realloc_ident).unwrap();
                let realloc_fv = self.declare_llvm_function(realloc_fn_id)?;
                let call =
                    self.builder.build_call(realloc_fv, &[old_ptr_arg, new_size_arg], "").unwrap();
                let result = call.try_as_basic_value().expect_basic("realloc return");
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::Free => {
                let old_ptr_arg = self.load_function_argument(function_id, 0);

                let free_ident = self.k1.ast.idents.intern("free");
                let free_fn_id = self.find_libc_function_id(free_ident).unwrap();
                let free_fv = self.declare_llvm_function(free_fn_id)?;
                let call = self.builder.build_call(free_fv, &[old_ptr_arg], "").unwrap();
                let result = call.try_as_basic_value().expect_basic("free return");
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::MemCopy => {
                // intern fn copy(
                //   dst: Pointer,
                //   src: Pointer,
                //   count: uword
                // ): unit
                let dst_ptr_arg = self.load_function_argument(function_id, 0).into_pointer_value();
                let src_ptr_arg = self.load_function_argument(function_id, 1).into_pointer_value();
                let size_arg = self.load_function_argument(function_id, 2).into_int_value();
                let dst_align_bytes = 1;
                let src_align_bytes = 1;
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
                let result = self.builtin_types.unit_basic();
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::MemSet => {
                // intern fn set(dst: Pointer, value: u8, count: uword): unit
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
                let result = self.builtin_types.unit_basic();
                self.builder.build_return(Some(&result)).unwrap()
            }
            BackendBuiltin::MemEquals => {
                // intern fn equals(p1: Pointer, p2: Pointer, size: uword): bool
                let p1_arg = self.load_function_argument(function_id, 0);
                let p2_arg = self.load_function_argument(function_id, 1);
                let size_arg = self.load_function_argument(function_id, 2);

                let memcmp_ident = self.k1.ast.idents.intern("memcmp");
                let memcmp_fn_id = self.find_libc_function_id(memcmp_ident).unwrap();
                let memcmp_fv = self.declare_llvm_function(memcmp_fn_id)?;
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
                // intern fn exit(code: i32): never
                let code_arg = self.load_function_argument(function_id, 0);

                let exit_ident = self.k1.ast.idents.intern("exit");
                let exit_fn_id = self.find_libc_function_id(exit_ident).unwrap();
                let exit_fv = self.declare_llvm_function(exit_fn_id)?;
                let call = self.builder.build_call(exit_fv, &[code_arg], "").unwrap();
                let _result = call.try_as_basic_value();
                self.builder.build_unreachable().unwrap()
            }

            BackendBuiltin::TypeSchema | BackendBuiltin::TypeName => {
                // intern fn typeSchema(id: u64): TypeSchema
                let type_id_arg = self.load_function_argument(function_id, 0).into_int_value();
                let is_type_name = builtin_type == BackendBuiltin::TypeName;
                let cg_fn = self.llvm_functions.get(&function_id).unwrap();
                let return_llvm_type = cg_fn.function_type.return_cg_type;
                let entry_block = self.builder.get_insert_block().unwrap();

                // typeSchema and typeName return a struct, so we have to do sret shenanigans
                let sret_ptr = self.llvm_functions.get(&function_id).unwrap().sret_pointer.unwrap();

                let else_block = self.append_basic_block("miss");
                self.builder.position_at_end(else_block);
                // TODO: Proper crash
                self.builder.build_unreachable().unwrap();

                let finish_block = self.append_basic_block("finish");

                let mut cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> =
                    Vec::with_capacity(self.k1.type_schemas.len());
                if is_type_name {
                    for (type_id, static_string_id) in self
                        .k1
                        .type_names
                        .iter()
                        .map(|(x, y)| (*x, *y))
                        .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
                    {
                        if self.k1.types.get_contained_type_variable_counts(type_id).is_abstract() {
                            // No point re-ifying types that don't exist at runtime
                            // like type parameters
                            continue;
                        }
                        let my_block =
                            self.append_basic_block(&format!("arm_type_{}", type_id.as_u32()));
                        self.builder.position_at_end(my_block);
                        let type_id_int_value =
                            self.ctx.i64_type().const_int(type_id.as_u32() as u64, false);

                        let value = {
                            let StaticValue::String(string_id) =
                                self.k1.static_values.get(static_string_id)
                            else {
                                panic!("typename should be a string")
                            };
                            let global_value = self.codegen_string_id_to_global(
                                *string_id,
                                Some(&format!("typename_{}\0", type_id.as_u32())),
                            )?;
                            global_value.as_pointer_value().as_basic_value_enum()
                        };
                        self.store_k1_value(&return_llvm_type, sret_ptr, value);
                        self.builder.build_unconditional_branch(finish_block).unwrap();
                        cases.push((type_id_int_value, my_block));
                    }
                } else {
                    for (type_id, schema_value_id) in self
                        .k1
                        .type_schemas
                        .iter()
                        .map(|(x, y)| (*x, *y))
                        .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
                    {
                        if self.k1.types.get_contained_type_variable_counts(type_id).is_abstract() {
                            // No point re-ifying types that don't exist at runtime
                            // like type parameters
                            continue;
                        }
                        let my_block =
                            self.append_basic_block(&format!("arm_type_{}", type_id.as_u32()));
                        self.builder.position_at_end(my_block);
                        let type_id_int_value =
                            self.ctx.i64_type().const_int(type_id.as_u32() as u64, false);

                        let value = self.codegen_static_value_canonical(schema_value_id)?;
                        self.store_k1_value(&return_llvm_type, sret_ptr, value);
                        self.builder.build_unconditional_branch(finish_block).unwrap();
                        cases.push((type_id_int_value, my_block));
                    }
                }
                self.builder.position_at_end(entry_block);
                let _switch = self.builder.build_switch(type_id_arg, else_block, &cases).unwrap();

                self.builder.position_at_end(finish_block);
                self.builder.build_return(None).unwrap()
            }
            BackendBuiltin::CompilerMessage => {
                return failf!(
                    function_span,
                    "Internal Compiler Error: cannot codegen CompilerMessage builtin"
                );
            }
        };
        Ok(instr)
    }

    fn codegen_block(
        &mut self,
        mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        block_id: u32,
        block: &bc::CompiledBlock,
    ) -> TyperResult<BasicBlock<'ctx>> {
        let current_function = self.get_insert_function().function_value;
        let llvm_block = current_function.get_basic_block_iter().nth(block_id as usize).unwrap();
        self.builder.position_at_end(llvm_block);
        for inst in self.k1.bytecode.mem.getn(block.instrs) {
            self.codegen_inst(mappings, *inst)?;
        }
        Ok(llvm_block)
    }

    fn codegen_value(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        value: bc::Value,
    ) -> TyperResult<BasicValueEnum<'ctx>> {
        match value {
            bc::Value::Inst(inst_id) => match inst_mappings.get(&inst_id) {
                Some(v) => Ok(*v),
                None => {
                    return failf!(
                        self.debug.current_span(),
                        "Whiffed inst id lookup: {}",
                        inst_id.as_u32()
                    );
                }
            },
            bc::Value::Global { t, id } => {
                let Some(global_value) = self.globals.get(&id).copied() else {
                    return failf!(
                        self.debug.current_span(),
                        "Whiffed global id lookup: {}",
                        id.as_u32()
                    );
                };
                let k1_global = self.k1.globals.get(id);
                if k1_global.is_referencing {
                    Ok(global_value.as_pointer_value().into())
                } else {
                    let cg_type = self.codegen_type(t);
                    let loaded =
                        self.load_k1_value(&cg_type, global_value.as_pointer_value(), "", false);
                    Ok(loaded)
                }
            }
            bc::Value::StaticValue { id, .. } => self.codegen_static_value_canonical(id),
            bc::Value::FunctionAddr(function_id) => {
                let Some(cg_fun) = self.llvm_functions.get(&function_id) else {
                    return failf!(
                        self.debug.current_span(),
                        "Whiffed function id lookup: {}",
                        function_id.as_u32()
                    );
                };
                Ok(cg_fun.function_value.as_global_value().as_pointer_value().into())
            }
            bc::Value::FnParam { index, .. } => {
                let function = self.get_insert_function();
                let v = function.param_values[index as usize];
                Ok(v)
            }
            bc::Value::Imm32 { t, data } => {
                let v: BasicValueEnum<'ctx> = match t {
                    ScalarType::U8 => self.ctx.i8_type().const_int(data as u64, false).into(),
                    ScalarType::U16 => self.ctx.i16_type().const_int(data as u64, false).into(),
                    ScalarType::U32 => self.ctx.i32_type().const_int(data as u64, false).into(),
                    ScalarType::U64 => self.ctx.i64_type().const_int(data as u64, false).into(),
                    ScalarType::I8 => self.ctx.i8_type().const_int(data as u64, true).into(),
                    ScalarType::I16 => self.ctx.i16_type().const_int(data as u64, true).into(),
                    ScalarType::I32 => self.ctx.i32_type().const_int(data as u64, true).into(),
                    ScalarType::I64 => self.ctx.i64_type().const_int(data as u64, true).into(),
                    ScalarType::F32 => {
                        self.ctx.f32_type().const_float(f32::from_bits(data) as f64).into()
                    }
                    ScalarType::F64 => {
                        self.ctx.f64_type().const_float(f32::from_bits(data) as f64).into()
                    }
                    ScalarType::Pointer => unreachable!(),
                };
                Ok(v)
            }
            bc::Value::PtrZero => Ok(self.builtin_types.ptr.const_null().into()),
        }
    }

    fn codegen_inst(
        &mut self,
        inst_mappings: &mut FxHashMap<InstId, BasicValueEnum<'ctx>>,
        inst_id: InstId,
    ) -> TyperResult<()> {
        let bc = &self.k1.bytecode;
        let span = *bc.sources.get(inst_id);
        self.set_debug_location_from_span(span);
        let inst = *bc.instrs.get(inst_id);
        match inst {
            Inst::Data(data_inst) => {
                let value: BasicValueEnum<'ctx> = match data_inst {
                    bc::DataInst::U64(u) => self.ctx.i64_type().const_int(u, false).into(),
                    bc::DataInst::I64(i) => self.ctx.i64_type().const_int(i as u64, true).into(),
                    bc::DataInst::Float(f) => match f {
                        TypedFloatValue::F32(f32) => {
                            self.ctx.f32_type().const_float(f32 as f64).into()
                        }
                        TypedFloatValue::F64(f64) => self.ctx.f64_type().const_float(f64).into(),
                    },
                };
                inst_mappings.insert(inst_id, value.as_basic_value_enum());
                Ok(())
            }
            Inst::Alloca { t, .. } => {
                let cg_type = self.codegen_type(t);
                // Our helper here hoists the alloca to the top of the function, and sets an explicit align
                let instr = self.build_k1_alloca(&cg_type, "");
                inst_mappings.insert(inst_id, instr.as_basic_value_enum());

                // nocommit: Declare variable if we should
                // let local_variable = self.debug.debug_builder.create_auto_variable(
                //         self.debug.current_scope(),
                //         &name,
                //         self.debug.current_file(),
                //         self.get_line_number(let_stmt.span),
                //         variable_type.debug_type(),
                //         true,
                //         0,
                //         variable_type.rich_repr_layout().align,
                //     );

                Ok(())
            }
            Inst::Store { dst, value, t } => {
                let dst_pointer = self.codegen_value(inst_mappings, dst)?.into_pointer_value();
                let value = self.codegen_value(inst_mappings, value)?;
                let _store = self.builder.build_store(dst_pointer, value).unwrap();
                Ok(())
            }
            Inst::Load { t, src } => {
                let cg_ty = self.codegen_type(PhysicalType::Scalar(t));
                let src_ptr = self.codegen_value(inst_mappings, src)?.into_pointer_value();
                let load = self.builder.build_load(cg_ty.rich_type(), src_ptr, "").unwrap();
                inst_mappings.insert(inst_id, load);
                Ok(())
            }
            Inst::Copy { dst, src, t, .. } => {
                let dst_value = self.codegen_value(inst_mappings, dst)?;
                let src_value = self.codegen_value(inst_mappings, src)?;
                let layout = self.k1.types.get_pt_layout(t);
                let _memcpy = self.memcpy_layout(
                    dst_value.into_pointer_value(),
                    src_value.into_pointer_value(),
                    layout,
                );
                Ok(())
            }
            Inst::StructOffset { struct_t, base, field_index, .. } => {
                let cg_struct = self.codegen_type(PhysicalType::Agg(struct_t)).expect_struct();
                let base_ptr = self.codegen_value(inst_mappings, base)?.into_pointer_value();
                let gep = self.build_struct_gep(base_ptr, &cg_struct, field_index, "");
                inst_mappings.insert(inst_id, gep.into());
                Ok(())
            }
            Inst::ArrayOffset { element_t, base, element_index } => {
                let cg_elem_ty = self.codegen_type(element_t);
                let base_ptr = self.codegen_value(inst_mappings, base)?.into_pointer_value();
                let index_int = self.codegen_value(inst_mappings, element_index)?.into_int_value();
                let gep = unsafe {
                    self.builder
                        .build_gep(cg_elem_ty.rich_type(), base_ptr, &[index_int], "")
                        .unwrap()
                };
                inst_mappings.insert(inst_id, gep.into());
                Ok(())
            }
            Inst::Call { id } => {
                if let Some(return_value) = self.codegen_function_call(inst_mappings, id)? {
                    inst_mappings.insert(inst_id, return_value);
                }
                Ok(())
            },
            Inst::Jump(block_id) => {
                let dst_block = self.get_nth_block(block_id as usize)?;
                let _jump = self.builder.build_unconditional_branch(dst_block).unwrap();
                Ok(())
            }
            Inst::JumpIf { cond, cons, alt } => {
                let cond_value = self.codegen_value(inst_mappings, cond)?;
                let cond_value_i1 = self.bool_to_i1(cond_value.into_int_value(), "");
                let then_block = self.get_nth_block(cons as usize)?;
                let else_block = self.get_nth_block(alt as usize)?;
                let _branch = self
                    .builder
                    .build_conditional_branch(cond_value_i1, then_block, else_block)
                    .unwrap();
                Ok(())
            }
            Inst::Unreachable => {
                self.builder.build_unreachable().unwrap();
                Ok(())
            }
            Inst::CameFrom { t, incomings } => {
                let phi_ty = self.codegen_type(t);
                let phi = self.builder.build_phi(phi_ty.rich_type(), "").unwrap();
                for incoming in self.k1.bytecode.mem.getn(incomings) {
                    let value = self.codegen_value(inst_mappings, incoming.value)?;
                    let block = self.get_nth_block(incoming.from as usize)?;
                    phi.add_incoming(&[(&value, block)])
                }
                inst_mappings.insert(inst_id, phi.as_basic_value());
                Ok(())
            }
            Inst::Ret(value) => {
                let ret_value = self.codegen_value(inst_mappings, value)?;
                let current_fn = self.get_insert_function();
                match current_fn.sret_pointer {
                    None => {
                        let _return = self.builder.build_return(Some(&ret_value)).unwrap();
                        Ok(())
                    }
                    Some(sret_ptr) => {
                        let ret_cg_type = &current_fn.function_type.return_cg_type;
                        let _store = self.store_k1_value(ret_cg_type, sret_ptr, ret_value);
                        let _return = self.builder.build_return(None).unwrap();
                        Ok(())
                    }
                }
            }
            Inst::BoolNegate { v } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let i1_input = self.bool_to_i1(input, "");
                let not_i1 = self.builder.build_not(i1_input, "").unwrap();
                let not_bool = self.i1_to_bool(not_i1, "");
                inst_mappings.insert(inst_id, not_bool.as_basic_value_enum());
                Ok(())
            }
            Inst::BitNot { v } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let not_input = self.builder.build_not(input, "").unwrap();
                inst_mappings.insert(inst_id, not_input.as_basic_value_enum());
                Ok(())
            }
            Inst::BitCast { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?;

                // From agg to scalar -> handled by BC
                // From scalar to agg -> handled by BC
                match to {
                    // From agg to agg -> canon type is ptr, nothing to do.
                    PhysicalType::Agg(_) => {
                        inst_mappings.insert(inst_id, input);
                        Ok(())
                    }
                    // From scalar to scalar -> do the llvm bitcast
                    PhysicalType::Scalar(st) => {
                        let llvm_ty = self.scalar_basic_type(st);
                        let bitcast = self.builder.build_bit_cast(input, llvm_ty, "").unwrap();
                        inst_mappings.insert(inst_id, bitcast);
                        Ok(())
                    }
                }
            }
            Inst::IntTrunc { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let trunc = self.builder.build_int_truncate(input, to_int_type, "").unwrap();
                inst_mappings.insert(inst_id, trunc.as_basic_value_enum());
                Ok(())
            }
            Inst::IntExtU { v, to } | Inst::IntExtS { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let signed = matches!(inst, bc::Inst::IntExtS { .. });
                let ext =
                    self.builder.build_int_cast_sign_flag(input, to_int_type, signed, "").unwrap();
                inst_mappings.insert(inst_id, ext.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatTrunc { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_float_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let trunc = self.builder.build_float_trunc(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, trunc.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatExt { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_float_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let ext = self.builder.build_float_ext(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, ext.as_basic_value_enum());
                Ok(())
            }
            Inst::Float32ToIntUnsigned { v, to } | Inst::Float64ToIntUnsigned { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_float_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let int = self.builder.build_float_to_unsigned_int(input, to_int_type, "").unwrap();
                inst_mappings.insert(inst_id, int.as_basic_value_enum());
                Ok(())
            }
            Inst::Float32ToIntSigned { v, to } | Inst::Float64ToIntSigned { v, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_float_value();
                let to_int_type = self.scalar_basic_type(to).into_int_type();
                let int = self.builder.build_float_to_signed_int(input, to_int_type, "").unwrap();
                inst_mappings.insert(inst_id, int.as_basic_value_enum());
                Ok(())
            }
            Inst::IntToFloatUnsigned { v, from, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let float =
                    self.builder.build_unsigned_int_to_float(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, float.as_basic_value_enum());
                Ok(())
            }
            Inst::IntToFloatSigned { v, from, to } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let to_float_type = self.scalar_basic_type(to).into_float_type();
                let float =
                    self.builder.build_signed_int_to_float(input, to_float_type, "").unwrap();
                inst_mappings.insert(inst_id, float.as_basic_value_enum());
                Ok(())
            }
            Inst::PtrToWord { v } => {
                let input = self.codegen_value(inst_mappings, v)?.into_pointer_value();
                let word_int = self
                    .builder
                    .build_ptr_to_int(input, self.builtin_types.ptr_sized_int, "")
                    .unwrap();
                inst_mappings.insert(inst_id, word_int.as_basic_value_enum());
                Ok(())
            }
            Inst::WordToPtr { v } => {
                let input = self.codegen_value(inst_mappings, v)?.into_int_value();
                let ptr_value =
                    self.builder.build_int_to_ptr(input, self.builtin_types.ptr, "").unwrap();
                inst_mappings.insert(inst_id, ptr_value.as_basic_value_enum());
                Ok(())
            }
            Inst::IntAdd { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                // nocommit: decide if overflow traps or not, signed and unsigned
                //self.builder.build_int_nsw_add
                let sum = self.builder.build_int_add(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, sum.as_basic_value_enum());
                Ok(())
            }
            Inst::IntSub { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let diff = self.builder.build_int_sub(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, diff.as_basic_value_enum());
                Ok(())
            }
            Inst::IntMul { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let prod = self.builder.build_int_mul(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, prod.as_basic_value_enum());
                Ok(())
            }
            Inst::IntDivUnsigned { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let div = self.builder.build_int_unsigned_div(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, div.as_basic_value_enum());
                Ok(())
            }
            Inst::IntDivSigned { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let div = self.builder.build_int_signed_div(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, div.as_basic_value_enum());
                Ok(())
            }
            Inst::IntRemUnsigned { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let rem = self.builder.build_int_unsigned_rem(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, rem.as_basic_value_enum());
                Ok(())
            }
            Inst::IntRemSigned { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let rem = self.builder.build_int_signed_rem(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, rem.as_basic_value_enum());
                Ok(())
            }
            Inst::IntCmp { lhs, rhs, pred, width } => {
                let llvm_pred = match pred {
                    bc::IntCmpPred::Eq => IntPredicate::EQ,
                    bc::IntCmpPred::Slt => IntPredicate::SLT,
                    bc::IntCmpPred::Sle => IntPredicate::SLE,
                    bc::IntCmpPred::Sgt => IntPredicate::SGT,
                    bc::IntCmpPred::Sge => IntPredicate::SGE,
                    bc::IntCmpPred::Ult => IntPredicate::ULT,
                    bc::IntCmpPred::Ule => IntPredicate::ULE,
                    bc::IntCmpPred::Ugt => IntPredicate::UGT,
                    bc::IntCmpPred::Uge => IntPredicate::UGE,
                };
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let cmp =
                    self.builder.build_int_compare(llvm_pred, lhs_value, rhs_value, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
                Ok(())
            }
            Inst::FloatAdd { lhs, rhs, width }
            | Inst::FloatSub { lhs, rhs, width }
            | Inst::FloatMul { lhs, rhs, width }
            | Inst::FloatDiv { lhs, rhs, width }
            | Inst::FloatRem { lhs, rhs, width } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_float_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_float_value();
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
            Inst::FloatCmp { lhs, rhs, pred, width } => {
                let llvm_pred = match pred {
                    bc::FloatCmpPred::Eq => FloatPredicate::OEQ,
                    bc::FloatCmpPred::Lt => FloatPredicate::OLT,
                    bc::FloatCmpPred::Le => FloatPredicate::OLE,
                    bc::FloatCmpPred::Gt => FloatPredicate::OGT,
                    bc::FloatCmpPred::Ge => FloatPredicate::OGE,
                };
                let lhs = self.codegen_value(inst_mappings, lhs)?.into_float_value();
                let rhs = self.codegen_value(inst_mappings, rhs)?.into_float_value();
                let cmp = self.builder.build_float_compare(llvm_pred, lhs, rhs, "").unwrap();
                let bool_value = self.i1_to_bool(cmp, "");
                inst_mappings.insert(inst_id, bool_value.as_basic_value_enum());
                Ok(())
            }
            Inst::BitAnd { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let and = self.builder.build_and(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, and.as_basic_value_enum());
                Ok(())
            }
            Inst::BitOr { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let or = self.builder.build_or(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, or.as_basic_value_enum());
                Ok(())
            }
            Inst::BitXor { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let xor = self.builder.build_xor(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, xor.as_basic_value_enum());
                Ok(())
            }
            Inst::BitShiftLeft { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let shl = self.builder.build_left_shift(lhs_value, rhs_value, "").unwrap();
                inst_mappings.insert(inst_id, shl.as_basic_value_enum());
                Ok(())
            }
            Inst::BitUnsignedShiftRight { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let lshr = self.builder.build_right_shift(lhs_value, rhs_value, false, "").unwrap();
                inst_mappings.insert(inst_id, lshr.as_basic_value_enum());
                Ok(())
            }
            Inst::BitSignedShiftRight { lhs, rhs, .. } => {
                let lhs_value = self.codegen_value(inst_mappings, lhs)?.into_int_value();
                let rhs_value = self.codegen_value(inst_mappings, rhs)?.into_int_value();
                let ashr = self.builder.build_right_shift(lhs_value, rhs_value, true, "").unwrap();
                inst_mappings.insert(inst_id, ashr.as_basic_value_enum());
                Ok(())
            }
            Inst::BakeStaticValue { .. } => {
                return failf!(
                    self.debug.current_span(),
                    "BakeStaticValue is only available to compile-time code"
                );
            }
        }
    }

    fn make_function_debug_info(
        &mut self,
        function_name: &str,
        function_span: SpanId,
        return_type: DIType<'ctx>,
        param_debug_types: &[DIType<'ctx>],
    ) -> TyperResult<(DISubprogram<'ctx>, DIFile<'ctx>)> {
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

    fn declare_llvm_function(
        &mut self,
        function_id: FunctionId,
    ) -> TyperResult<FunctionValue<'ctx>> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(function.function_value);
        }
        debug!("codegen function signature\n{}", self.k1.function_id_to_string(function_id, false));

        let typed_function = self.k1.get_function(function_id);
        let typed_function_linkage = typed_function.linkage;
        let typed_function_params = typed_function.params;
        let function_span = self.k1.ast.get_span_for_id(typed_function.parsed_id);

        let llvm_linkage = match typed_function.linkage {
            TyperLinkage::Standard => None,
            TyperLinkage::External { .. } => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        let llvm_name = match typed_function.linkage {
            TyperLinkage::External { fn_name: Some(link_name), .. } => {
                self.k1.ident_str(link_name).to_string()
            }
            _ => self.k1.make_qualified_name(typed_function.scope, typed_function.name, ".", true),
        };

        if self.llvm_module.get_function(&llvm_name).is_some() {
            if let Some(LlvmLinkage::External) = llvm_linkage {
                eprintln!("Allowing duplicate external name declaration: {}", llvm_name)
            } else {
                return failf!(
                    self.k1.ast.get_span_for_id(typed_function.parsed_id),
                    "Dupe function name: {}",
                    llvm_name
                );
            }
        }

        let bytecode_fn = self.k1.bytecode.functions.get(function_id).unwrap();

        let llvm_function_type = self.make_cg_function_type(&bytecode_fn.fn_type)?;

        let di_types: SV8<_> = self
            .mem
            .getn_lt(llvm_function_type.param_k1_types)
            .iter()
            .map(|t| t.debug_type())
            .collect();
        let (di_subprogram, di_file) = self.make_function_debug_info(
            &llvm_name,
            function_span,
            llvm_function_type.return_cg_type.debug_type(),
            &di_types,
        )?;
        let is_sret = llvm_function_type.is_sret;

        let sret_attribute = if is_sret {
            let struct_type = self.rich_repr_type(&llvm_function_type.return_cg_type);
            let sret_attribute = self.make_sret_attribute(struct_type.as_any_type_enum());
            let align_attribute = self.make_align_attribute(
                llvm_function_type.return_cg_type.rich_repr_layout().align as u64,
            );
            Some((sret_attribute, align_attribute))
        } else {
            None
        };

        // TODO: Figure out how to mark all standard functions dso_local
        let function_value = self.llvm_module.add_function(
            &llvm_name,
            llvm_function_type.llvm_function_type,
            llvm_linkage,
        );
        let sret_pointer = if is_sret {
            Some(function_value.get_first_param().unwrap().into_pointer_value())
        } else {
            None
        };
        if let Some((sret_attribute, align_attribute)) = sret_attribute {
            function_value.add_attribute(AttributeLoc::Param(0), sret_attribute);
            function_value.add_attribute(AttributeLoc::Param(0), align_attribute);
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
                let abi_mapping =
                    self.mem.get_nth_lt(llvm_function_type.param_abi_mappings, i - offset);
                if let AbiParamMapping::BigStructByPtrToCopy { byval_attr } = abi_mapping {
                    // FIXME: We likely need to be setting param alignments explicitly sometimes
                    //function_value.set_param_alignment(param_index, alignment);
                    if *byval_attr {
                        let k1_type =
                            self.mem.get_nth_lt(llvm_function_type.param_k1_types, i - offset);
                        let byval_attribute = self
                            .make_byval_attribute(self.rich_repr_type(k1_type).as_any_type_enum());
                        function_value
                            .add_attribute(AttributeLoc::Param(i as u32), byval_attribute);
                    }
                }
            }
        }

        let compile_body = !typed_function_linkage.is_external();
        if compile_body {
            self.functions_pending_body_compilation.push(function_id);
        }

        function_value.set_subprogram(di_subprogram);

        self.llvm_functions.insert(
            function_id,
            CgFunction {
                function_type: llvm_function_type,
                function_value,
                param_values: Vec::with_capacity(4),
                sret_pointer,
                last_alloca_instr: None,
                instruction_count: 0,
                debug_info: di_subprogram,
                debug_file: di_file,
            },
        );
        self.llvm_function_to_k1.insert(function_value, function_id);

        Ok(function_value)
    }

    fn get_abi_mapping_for_type(
        &self,
        abi_mode: AbiMode,
        pt: PhysicalType,
        is_return: bool,
    ) -> AbiParamMapping {
        enum CallConv {
            InternalK1,
            AMD64,
            ARM64,
        }
        let callconv = match abi_mode {
            AbiMode::Internal => CallConv::InternalK1,
            AbiMode::Native => match self.k1.ast.config.target {
                compiler::Target::LinuxIntel64 => CallConv::AMD64,
                compiler::Target::MacOsArm64 => CallConv::ARM64,
                compiler::Target::Wasm64 => CallConv::ARM64,
            },
        };

        // https://yorickpeterse.com/articles/the-mess-that-is-handling-structure-arguments-and-returns-in-llvm/
        match pt {
            PhysicalType::Scalar(_) => AbiParamMapping::ScalarInRegister,
            PhysicalType::Agg(agg_id) => {
                let agg_record = self.k1.types.phys_types.get(agg_id);
                match agg_record.agg_type {
                    AggType::EnumVariant(_) | AggType::Struct { .. } | AggType::Array { .. } => {
                        let size_bytes = agg_record.layout.size;
                        match callconv {
                            CallConv::InternalK1 => {
                                if size_bytes <= 8 {
                                    AbiParamMapping::ScalarInRegister
                                } else if size_bytes <= 16 {
                                    AbiParamMapping::StructByPtrNoCopy
                                } else {
                                    AbiParamMapping::StructByPtrNoCopy
                                }
                            }
                            CallConv::ARM64 => {
                                // nocommit: Check for HFA on ARM64
                                if size_bytes <= 8 {
                                    // Returns use the exact width; otherwise just i64
                                    let width_bits = if is_return { size_bytes * 8 } else { 64 };
                                    AbiParamMapping::StructInInteger { width: width_bits }
                                } else if size_bytes <= 16 {
                                    // If the size of the structure is between 9 and 16 bytes, pass the structure as
                                    // [an array of] two integers of 8 bytes each
                                    AbiParamMapping::StructByIntPairArray
                                } else {
                                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: false }
                                }
                            }
                            CallConv::AMD64 => {
                                if size_bytes <= 8 {
                                    // If the size of the structure is less than 8 bytes, pass the structure as an integer of its size in bits
                                    let width_bits = size_bytes * 8;
                                    AbiParamMapping::StructInInteger { width: width_bits }
                                } else if size_bytes <= 16 {
                                    // "If the size is between 8 and 16 bytes, the logic is a little more difficult."
                                    // Pass by classified eightbytes
                                    let (eb1, eb2, eb2_bits) =
                                        self.collect_aggregate_eightbytes(pt);
                                    AbiParamMapping::StructByEightbytePair {
                                        class1: eb1,
                                        class2: eb2,
                                        active_bits2: eb2_bits,
                                    }
                                } else {
                                    AbiParamMapping::BigStructByPtrToCopy { byval_attr: true }
                                }
                            }
                        }
                    }
                    AggType::Opaque { .. } => {
                        // enum abi mapping eventually; for
                        // now we just say they are always passed by pointer
                        match callconv {
                            CallConv::InternalK1 => AbiParamMapping::StructByPtrNoCopy,
                            CallConv::AMD64 | CallConv::ARM64 => {
                                AbiParamMapping::BigStructByPtrToCopy { byval_attr: !is_return }
                            }
                        }
                    }
                }
            }
        }
    }

    // What a horrible amount of code for such a small transformation!
    fn collect_aggregate_eightbytes(
        &self,
        pt: PhysicalType,
    ) -> (EightbyteClass, EightbyteClass, u32) {
        // This whole thing could be generalized to collect N eightbytes, rather than 2, which would let me use it
        // for the HFA detection
        fn append_type(
            class1: &mut Option<EightbyteClass>,
            class2: &mut Option<EightbyteClass>,
            cur_bits: &mut u32,
            class: EightbyteClass,
            size_bits: u32,
        ) {
            match (class1, class2) {
                (c1 @ None, None) => *c1 = Some(class),
                (Some(c1), c2 @ None) => {
                    let scalar_size_signed: i32 = size_bits as i32;
                    let cur_bits_signed: i32 = *cur_bits as i32;

                    let new_bits = scalar_size_signed + cur_bits_signed;
                    let bleed_bits = new_bits - 8;

                    let new_c1_class = c1.combine(class);
                    *c1 = new_c1_class;
                    if bleed_bits > 0 {
                        // `bleed`: This scalar actually spans the first 8 and last 8 bytes;
                        // so contributes to the class of both
                        *c2 = Some(class);
                        *cur_bits = bleed_bits as u32;
                    } else if bleed_bits == 0 {
                        // We completed the first eightbyte exactly; initialize the 2nd and reset
                        // cur_bits
                        *c2 = Some(EightbyteClass::Initial);
                        *cur_bits = 0;
                    } else {
                        *cur_bits = new_bits as u32;
                    }
                }
                (Some(_), Some(c2)) => *c2 = c2.combine(class),
                (None, Some(_)) => unreachable!(),
            }
        }
        fn handle_type_rec<'ctx, 'k1>(
            c: &Cg<'ctx, 'k1>,
            class1: &mut Option<EightbyteClass>,
            class2: &mut Option<EightbyteClass>,
            cur_bits: &mut u32,
            t: PhysicalType,
        ) {
            match t {
                PhysicalType::Scalar(st) => {
                    let class = match st {
                        ScalarType::U8 => EightbyteClass::Int,
                        ScalarType::U16 => EightbyteClass::Int,
                        ScalarType::U32 => EightbyteClass::Int,
                        ScalarType::U64 => EightbyteClass::Int,
                        ScalarType::I8 => EightbyteClass::Int,
                        ScalarType::I16 => EightbyteClass::Int,
                        ScalarType::I32 => EightbyteClass::Int,
                        ScalarType::I64 => EightbyteClass::Int,
                        ScalarType::F32 => EightbyteClass::Float,
                        ScalarType::F64 => EightbyteClass::Float,
                        ScalarType::Pointer => EightbyteClass::Int,
                    };
                    append_type(class1, class2, cur_bits, class, st.get_layout().size_bits())
                }
                PhysicalType::Agg(agg_id) => {
                    let agg_record = c.k1.types.phys_types.get(agg_id);
                    match agg_record.agg_type {
                        AggType::EnumVariant(evl) => {
                            handle_type_rec(
                                c,
                                class1,
                                class2,
                                cur_bits,
                                PhysicalType::Scalar(evl.tag.get_scalar_type()),
                            );
                            if let Some(payload) = evl.payload {
                                handle_type_rec(c, class1, class2, cur_bits, payload)
                            }
                        }
                        AggType::Struct { fields } => {
                            for f in c.k1.types.mem.getn(fields) {
                                handle_type_rec(c, class1, class2, cur_bits, f.field_t)
                            }
                        }
                        AggType::Array { element_pt: element_t, len } => {
                            for _ in 0..len {
                                handle_type_rec(c, class1, class2, cur_bits, element_t)
                            }
                        }
                        AggType::Opaque { layout } => append_type(
                            class1,
                            class2,
                            cur_bits,
                            EightbyteClass::Int,
                            layout.size_bits(),
                        ),
                    }
                }
            }
        }
        let mut class1 = Some(EightbyteClass::Initial);
        let mut class2 = None;
        let mut cur_bits = 0;

        handle_type_rec(self, &mut class1, &mut class2, &mut cur_bits, pt);

        match (class1, class2) {
            (Some(c1), Some(c2)) => (c1, c2, cur_bits),
            _ => panic!(
                "Failed to collect 2 eightbytes for 9-16 byte struct {}. Likely a bug.",
                self.k1.types.pt_to_string(pt)
            ),
        }
    }

    fn codegen_function_body(&mut self, function_id: FunctionId) -> TyperResult<()> {
        debug!("codegen function body\n{}", self.k1.function_id_to_string(function_id, false));
        self.current_insert_function = function_id;
        let typed_function = self.k1.get_function(function_id);
        let typed_function_params = typed_function.params;

        let function_span = self.k1.ast.get_span_for_id(typed_function.parsed_id);
        let function_line_number = self
            .k1
            .ast
            .get_lines_for_span_id(function_span)
            .expect("line for function span")
            .0
            .line_number();

        let codegened_function = self.llvm_functions.get(&function_id).unwrap();
        let cg_function_type = &codegened_function.function_type;
        let param_k1_types = cg_function_type.param_k1_types;
        let param_abi_mappings = cg_function_type.param_abi_mappings;
        let is_sret = cg_function_type.is_sret;
        let function_value = codegened_function.function_value;

        self.debug.push_scope(
            function_span,
            codegened_function.debug_info.as_debug_info_scope(),
            codegened_function.debug_file,
        );

        let entry_block = self.ctx.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);
        for (i, param) in function_value.get_param_iter().enumerate() {
            let is_sret_param = i == 0 && is_sret;
            if is_sret_param {
                continue;
            }

            let logical_param_index = i - if is_sret { 1 } else { 0 };
            let param_k1_type = *self.mem.get_nth_lt(param_k1_types, logical_param_index);
            let param_abi_mapping = *self.mem.get_nth_lt(param_abi_mappings, logical_param_index);

            let typed_param_record =
                self.k1.mem.get_nth(typed_function_params, logical_param_index);
            self.set_debug_location_from_span(typed_param_record.span);

            let name = param.get_name().to_str().unwrap();
            let mapped_value =
                self.canonicalize_abi_param_value(param_abi_mapping, &param_k1_type, param);

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

            self.llvm_functions.get_mut(&function_id).unwrap().param_values[logical_param_index] =
                mapped_value;
            self.debug.insert_dbg_value_at_end(
                mapped_value,
                di_local_variable,
                None,
                self.get_debug_location(),
                entry_block,
            );
        }

        let bc_unit = self.k1.bytecode.functions.get(function_id).unwrap();
        // If we got this far and the function has an intrinsic type, then its
        // the kind that needs to be a physical LLVM function
        match bc_unit.function_builtin_kind {
            Some(builtin_kind) => {
                let _terminator_instr =
                    self.codegen_builtin_function_body(builtin_kind, function_id)?;
            }
            None => {
                self.codegen_unit_body(bc_unit.blocks);
            }
        };
        self.debug.pop_scope();

        Ok(())
    }

    fn codegen_unit_body(
        &mut self,
        blocks: MSlice<CompiledBlock, ProgramBytecode>,
    ) -> TyperResult<()> {
        let fv = self.get_insert_function().function_value;
        for block in self.k1.bytecode.mem.getn_lt(blocks) {
            self.ctx.append_basic_block(fv, block.name.as_str());
        }

        // nocommit: Re-use hashmap allocation
        let mut mappings = FxHashMap::new();
        for (index, block) in self.k1.bytecode.mem.getn(blocks).iter().enumerate() {
            self.codegen_block(&mut mappings, index as u32, block)?;
        }
        Ok(())
    }

    fn _count_function_instructions(function_value: FunctionValue<'ctx>) -> usize {
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

    fn codegen_float_value(&mut self, float: TypedFloatValue) -> TyperResult<BasicValueEnum<'ctx>> {
        let cg_ty = self.codegen_type(PhysicalType::Scalar(float.get_scalar_type()));
        let llvm_float_ty = cg_ty.rich_type().into_float_type();
        let llvm_value = llvm_float_ty.const_float(float.as_f64());
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_global(&mut self, global_id: TypedGlobalId) -> TyperResult<()> {
        let global = self.k1.globals.get(global_id).clone();
        let initial_static_value_id = global.initial_value.unwrap();
        let initializer_basic_value =
        self.codegen_static_value_as_const(initial_static_value_id, 0)?;

        let variable = self.k1.variables.get(global.variable_id);

        let maybe_reference_type = self.k1.types.get(global.ty).as_reference();

        let layout = self.k1.types.get_layout(global.ty);
        let llvm_linkage = match global.is_exported {
            false => LlvmLinkage::Private,
            true => LlvmLinkage::External,
        };
        // For now let's just use the name, eventually we'll ask the user for a link name
        let name = if global.is_exported {
            self.k1.ident_str(variable.name).to_string()
        } else {
            self.k1.make_qualified_name(variable.owner_scope, variable.name, "__", false)
        };
        let llvm_global = self.make_global_from_value(
            initializer_basic_value,
            layout.align,
            &name,
            global.is_constant,
            llvm_linkage,
        );
        if self.k1.program_settings.multithreaded {
            if global.is_tls {
                llvm_global.set_thread_local(true);
                let mode = if self.k1.program_settings.executable {
                    ThreadLocalMode::LocalExecTLSModel
                } else {
                    // We don't yet support dynamic library as a target
                    // So even libraries can use InitialExec
                    ThreadLocalMode::InitialExecTLSModel
                };
                llvm_global.set_thread_local_mode(Some(mode));
            }
        }
        self.globals.insert(global_id, llvm_global);
        Ok(())
    }

    pub fn codegen_program(&mut self) -> TyperResult<()> {
        let start = std::time::Instant::now();

        let global_ids: Vec<TypedGlobalId> = self.k1.globals.iter_ids().collect();

        // Guarantee code generation of exported globals
        for global_id in &global_ids {
            let g = self.k1.globals.get(*global_id);
            if g.is_exported {
                self.codegen_global(*global_id)?;
            }
        }

        // TODO: Codegen the exported functions as well as the called ones
        // for (id, function) in self.module.function_iter() {
        //     if function.linkage.is_exported() {
        //         self.codegen_function_signature(id)?;
        //     }
        // }

        let Some(main_function_id) = self.k1.get_main_function_id() else {
            return failf!(SpanId::NONE, "Program {} has no main function", self.k1.program_name());
        };
        let function_value = self.declare_llvm_function(main_function_id)?;

        let mut pending_buffer: Vec<FunctionId> =
            Vec::with_capacity(self.functions_pending_body_compilation.len());
        while !self.functions_pending_body_compilation.is_empty() {
            pending_buffer.extend(&self.functions_pending_body_compilation);
            self.functions_pending_body_compilation.clear();

            for id in &pending_buffer {
                self.codegen_function_body(*id)?;
            }

            pending_buffer.clear();
        }

        self.builder.unset_current_debug_location();
        let entrypoint = self.llvm_module.add_function("main", function_value.get_type(), None);
        let entry_block = self.ctx.append_basic_block(entrypoint, "entry");
        self.builder.position_at_end(entry_block);
        let params: Vec<BasicMetadataValueEnum<'ctx>> =
            entrypoint.get_params().iter().map(|p| (*p).into()).collect();
        let res = self
            .builder
            .build_call(function_value, &params, "")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap();
        self.builder.build_return(Some(&res)).unwrap();

        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    fn codegen_static_value_as_const(
        &mut self,
        static_value_id: StaticValueId,
        depth: usize,
    ) -> TyperResult<BasicValueEnum<'ctx>> {
        if let Some(basic) = self.static_values_basics.get(&static_value_id) {
            return Ok(*basic);
        }
        debug!("codegen_static_value_as_const {}", self.k1.static_value_to_string(static_value_id));

        let result = match self.k1.static_values.get(static_value_id) {
            StaticValue::Unit => self.builtin_types.unit_basic(),
            StaticValue::Bool(b) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum(),
                false => self.builtin_types.false_value.as_basic_value_enum(),
            },
            StaticValue::Char(byte) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum()
            }
            StaticValue::Int(int_value) => self.codegen_int_value(*int_value),
            StaticValue::Float(float_value) => self.codegen_float_value(*float_value).unwrap(),
            StaticValue::String(string_id) => {
                let string_global = self
                    .codegen_string_id_to_global(
                        *string_id,
                        Some(&format!("static_{}\0", static_value_id.as_u32())),
                    )
                    .unwrap();
                string_global.get_initializer().unwrap()
            }
            StaticValue::Zero(type_id) => {
                let pt = self.k1.types.get_physical_type(*type_id).unwrap();
                let cg_type = self.codegen_type(pt);
                let zero = cg_type.rich_type().const_zero();
                zero.as_basic_value_enum()
            }
            StaticValue::Struct(s) => {
                // Always a packed struct, accounting for every byte.
                let s_type_id = s.type_id;
                let layout = self.k1.types.get_struct_layout(s.type_id);
                let mut last_offset = 0;
                let mut packed_values = self.tmp.new_list(8);
                for (field, field_layout) in
                    self.k1.static_values.mem.getn(s.fields).iter().zip(layout)
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
                    let field_size = self.k1.types.get_pt_layout(field_layout.field_t);
                    debug_assert_eq!(self.llvm_size_info(&value.get_type()).size, field_size.size);
                    last_offset = field_layout.offset + field_size.size;
                }
                let struct_value = self.ctx.const_struct(&packed_values, true);

                debug_assert_eq!(
                    self.llvm_size_info(&struct_value.get_type()).size,
                    self.k1.types.get_layout(s_type_id).size,
                    "Checking Size of: {}",
                    struct_value
                );
                struct_value.as_basic_value_enum()
            }
            StaticValue::Enum(e) => {
                let e = *e;
                let mut packed_values = self.tmp.new_list(4);

                let variant_pt_id =
                    self.k1.types.get_physical_type(e.variant_type_id).unwrap().expect_agg();
                let variant_agg =
                    self.k1.types.phys_types.get(variant_pt_id).agg_type.expect_enum_variant();
                let variant_tag = variant_agg.tag;
                let envelope_layout = variant_agg.envelope;
                let variant_payload = variant_agg.payload;
                let variant_offset = variant_agg.payload_offset;

                let tag_llvm_value = self.codegen_int_value(variant_tag);
                let tag_layout = variant_tag.get_scalar_type().get_layout();
                packed_values.push(tag_llvm_value);
                match e.payload {
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
                        let payload_offset = variant_offset.unwrap();
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

                        let payload_pt = variant_payload.unwrap();
                        let payload_size = self.k1.types.get_pt_layout(payload_pt).size;
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
                    self.llvm_size_info(&struct_value.get_type()).size,
                    envelope_layout.size
                );
                struct_value.as_basic_value_enum()
            }
            StaticValue::LinearContainer(view) => {
                let view = *view;
                let (element_type, _) =
                    self.k1.types.get_as_container_instance(view.type_id).unwrap();
                let view_elements = self.k1.static_values.mem.getn(view.elements);
                let array_value =
                    self.codegen_static_elements_array(element_type, view_elements, depth)?;

                match view.kind {
                    StaticContainerKind::View => {
                        let element_type_layout = self.k1.types.get_layout(element_type);
                        let data_global = self.make_global_from_value(
                            array_value.as_basic_value_enum(),
                            element_type_layout.align,
                            &format!("static_elems_{}\0", static_value_id.as_u32()),
                            true,
                            LlvmLinkage::Private,
                        );
                        data_global.set_constant(true);
                        data_global.set_unnamed_addr(true);
                        data_global.set_initializer(&array_value);
                        let view_struct = self
                            .make_view_struct(
                                view.type_id,
                                view.len() as u64,
                                data_global.as_pointer_value(),
                            )
                            .unwrap();
                        view_struct.as_basic_value_enum()
                    }
                    StaticContainerKind::Array => array_value.as_basic_value_enum(),
                }
            }
        };
        if depth == 0 {
            self.static_values_basics.insert(static_value_id, result);
        }
        Ok(result)
    }

    fn codegen_static_elements_array(
        &mut self,
        element_type: TypeId,
        elements: &[StaticValueId],
        depth: usize,
    ) -> TyperResult<StructValue<'ctx>> {
        let mut packed_values = self.tmp.new_list(elements.len() as u32);

        // let element_backend_type = self.codegen_type(element_type)?;
        let element_layout = self.k1.types.get_layout(element_type);

        for elem in elements.iter() {
            let elem_basic_value = self.codegen_static_value_as_const(*elem, depth + 1)?;
            packed_values.push(elem_basic_value);

            // IF STRIDE != SIZE, MAKE SURE TO ADD IT! Because in K1 that's a container concern
            // and not part of the Layout.size
            let end_padding = element_layout.stride() - element_layout.size;
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
    ) -> TyperResult<GlobalValue<'ctx>> {
        if let Some(global) = self.static_values_globals.get(&static_value_id) {
            return Ok(*global);
        };
        let struct_value = self.codegen_static_value_as_const(static_value_id, 0)?;
        let type_id = self.k1.static_values.get(static_value_id).get_type();
        let layout = self.k1.types.get_layout(type_id);
        let global = self.make_global_from_value(
            struct_value,
            layout.align,
            &format!("static_{}\0", static_value_id.as_u32()),
            true,
            LlvmLinkage::Private,
        );
        self.static_values_globals.insert(static_value_id, global);
        Ok(global)
    }

    fn make_global_from_value(
        &mut self,
        value: BasicValueEnum<'ctx>,
        align: u32,
        name: &str,
        constant: bool,
        linkage: LlvmLinkage,
    ) -> GlobalValue<'ctx> {
        let global = self.llvm_module.add_global(value.get_type(), None, name);
        global.set_alignment(align);
        global.set_unnamed_addr(true);
        global.set_initializer(&value);
        global.set_constant(constant);
        global.set_linkage(linkage);
        global
    }

    fn codegen_static_value_canonical(
        &mut self,
        static_value_id: StaticValueId,
    ) -> TyperResult<BasicValueEnum<'ctx>> {
        debug!(
            "codegen_static_value_canonical {}",
            self.k1.static_value_to_string(static_value_id)
        );
        let v = self.k1.static_values.get(static_value_id);
        let result = match v {
            StaticValue::Unit => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::Bool(_) => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::Char(_) => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::Int(_) => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::Float(_) => self.codegen_static_value_as_const(static_value_id, 0)?,
            StaticValue::String(string_id) => {
                let string_global = self
                    .codegen_string_id_to_global(
                        *string_id,
                        Some(&format!("static_{}\0", static_value_id.as_u32())),
                    )
                    .unwrap();
                string_global.as_basic_value_enum()
            }
            StaticValue::Zero(type_id) => {
                let pt = self.k1.types.get_physical_type(*type_id).unwrap();
                let cg_type = self.codegen_type(pt);
                let zero_rich = cg_type.rich_type().const_zero();
                match pt.is_agg() {
                    true => {
                        todo!(
                            "generate a zero region global; could we have a single zero region as big as needed? For arrays, detect 'zero' pattern and append"
                        )
                    }
                    false => zero_rich,
                }
                //zero.as_basic_value_enum()
            }
            StaticValue::Struct(_) => {
                let global = self.make_global_for_static_value(static_value_id)?;
                global.as_pointer_value().as_basic_value_enum()
            }
            StaticValue::Enum(_) => {
                let global = self.make_global_for_static_value(static_value_id)?;
                global.as_pointer_value().as_basic_value_enum()
            }
            StaticValue::LinearContainer(_) => {
                let global = self.make_global_for_static_value(static_value_id)?;
                global.as_pointer_value().as_basic_value_enum()
            }
        };
        Ok(result)
    }

    fn make_string_llvm_global(
        &mut self,
        string_id: StringId,
        name: Option<&str>,
    ) -> TyperResult<GlobalValue<'ctx>> {
        let string_name = match name {
            None => "string_data",
            Some(n) => &format!("string_data_{n}\0"),
        };
        let rust_str = self.k1.get_string(string_id);
        let str_len = rust_str.len();
        let global_str_data = self.llvm_module.add_global(
            self.builtin_types.char.array_type(str_len as u32),
            None,
            string_name,
        );
        let str_data_array = i8_array_from_str(self.ctx, rust_str);
        global_str_data.set_linkage(LlvmLinkage::Private);
        global_str_data.set_initializer(&str_data_array);
        global_str_data.set_unnamed_addr(true);
        global_str_data.set_constant(true);

        // Ensure the string layout is what we expect
        // deftype string = { private view: View[char] }
        let string_pt = self.k1.types.get_physical_type(STRING_TYPE_ID).unwrap();
        let string_type = self.codegen_type(string_pt).expect_struct();
        let string_wrapper_struct_type = string_type.struct_type;

        let char_view_struct = self.mem.get_nth_lt(string_type.fields, 0).expect_struct();
        let char_buffer_cg_type = self.mem.get_nth_lt(char_view_struct.fields, 0).expect_struct();
        debug_assert!(
            char_buffer_cg_type
                .struct_type
                .get_field_type_at_index(0)
                .unwrap()
                .into_int_type()
                .get_bit_width()
                == 64
        );
        debug_assert!(
            char_buffer_cg_type.struct_type.get_field_type_at_index(1).unwrap().is_pointer_type()
        );
        debug_assert!(char_buffer_cg_type.struct_type.count_fields() == 2);

        let char_buffer_struct_value = self.make_buffer_struct(
            char_buffer_cg_type.struct_type,
            str_len as u64,
            global_str_data.as_pointer_value(),
        )?;
        let char_view_struct_value = char_view_struct
            .struct_type
            .const_named_struct(&[char_buffer_struct_value.as_basic_value_enum()]);
        let string_wrapper_struct = string_wrapper_struct_type
            .const_named_struct(&[char_view_struct_value.as_basic_value_enum()]);

        let global_str_struct =
            self.llvm_module.add_global(string_wrapper_struct_type, None, name.unwrap_or(""));
        global_str_struct.set_initializer(&string_wrapper_struct);
        global_str_struct.set_constant(true);
        global_str_struct.set_unnamed_addr(true);

        Ok(global_str_struct)
    }

    fn codegen_string_id_to_global(
        &mut self,
        string_id: StringId,
        name: Option<&str>,
    ) -> TyperResult<GlobalValue<'ctx>> {
        if let Some(cached_string) = self.strings.get(&string_id) {
            Ok(*cached_string)
        } else {
            let ptr = self.make_string_llvm_global(string_id, name)?;
            self.strings.insert(string_id, ptr);
            Ok(ptr)
        }
    }

    fn make_buffer_struct(
        &mut self,
        struct_type: StructType<'ctx>,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> TyperResult<StructValue<'ctx>> {
        let buffer_struct_value = struct_type.const_named_struct(&[
            self.builtin_types.ptr_sized_int.const_int(len, false).as_basic_value_enum(),
            data.as_basic_value_enum(),
        ]);
        Ok(buffer_struct_value)
    }

    fn make_view_struct(
        &mut self,
        view_type_id: TypeId,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> TyperResult<StructValue<'ctx>> {
        let buffer_type_id = self
            .k1
            .types
            .mem
            .get_nth_lt(self.k1.types.get(view_type_id).expect_struct().fields, 0)
            .type_id;
        // nocommit Physically, every buffer and every view is the same type so all of this is dumb
        // we don't even get resilience because we still hardcode knowledge of the 2 fields and
        // their order anyway
        let buffer_pt = self.k1.types.get_physical_type(buffer_type_id).unwrap();
        let buffer_cg_type = self.codegen_type(buffer_pt).expect_struct();
        self.make_buffer_struct(buffer_cg_type.struct_type, len, data)
    }

    pub fn name(&self) -> &str {
        self.k1.program_name()
    }

    fn set_up_machine(module: &mut LlvmModule) -> TargetMachine {
        // Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        // I use this explicit triple to avoid an annoying warning log on mac.
        let triple_str = &format!("arm64-apple-macosx{}", MAC_SDK_VERSION);
        let triple = TargetTriple::create(triple_str);
        // let triple = TargetMachine::get_default_triple();

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
            // self.llvm_module
            //     .run_passes("function(mem2reg)", &self.llvm_machine, PassBuilderOptions::create())
            //     .unwrap();
        }

        self.llvm_module.verify().unwrap();

        // self.llvm_machine.add_analysis_passes(&module_pass_manager);
        // module_pass_manager.run_on(&self.llvm_module);

        info!("codegen phase 'optimize' took {}ms", start.elapsed().as_millis());
        //for (_, function) in self.llvm_functions.iter_mut() {
        //    let new_count = Codegen::count_function_instructions(function.function_value);
        //    function.instruction_count = new_count;
        //}

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

    pub fn write_bitcode_to_path(&self, path: impl AsRef<Path>) -> bool {
        self.llvm_module.write_bitcode_to_path(path)
    }

    pub fn interpret_module(&self) -> anyhow::Result<u64> {
        let engine = self.llvm_module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        // let base_lib_module = self
        //     .ctx
        //     .create_module_from_ir(
        //         MemoryBuffer::create_from_file(Path::new("k1lib/k1lib.ll")).unwrap(),
        //     )
        //     .unwrap();
        // self.llvm_module.link_in_module(base_lib_module).unwrap();
        let Some(main_fn_id) = self.k1.get_main_function_id() else { bail!("No main function") };
        let llvm_function = self.llvm_functions.get(&main_fn_id).unwrap();
        eprintln!("Interpreting {}", self.k1.program_name());
        let return_value = unsafe { engine.run_function(llvm_function.function_value, &[]) };
        let res: u64 = return_value.as_int(true);
        Ok(res)
    }

    fn make_sret_attribute(&self, typ: AnyTypeEnum<'ctx>) -> Attribute {
        self.ctx.create_type_attribute(Attribute::get_named_enum_kind_id("sret"), typ)
    }
    fn make_align_attribute(&self, align: u64) -> Attribute {
        self.ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("align"), align)
    }
    fn make_byval_attribute(&self, typ: AnyTypeEnum<'ctx>) -> Attribute {
        self.ctx.create_type_attribute(Attribute::get_named_enum_kind_id("byval"), typ)
    }
}
