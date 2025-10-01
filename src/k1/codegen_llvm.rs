// Copyright (c) 2025 knix
// All rights reserved.

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
    AsDIScope, DICompileUnit, DIExpression, DIFile, DILocalVariable, DILocation, DIScope,
    DISubprogram, DISubroutineType, DIType, DWARFEmissionKind, DWARFSourceLanguage,
    DebugInfoBuilder,
};
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage as LlvmLinkage, Module as LlvmModule};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine};
use inkwell::types::{
    AnyType, AnyTypeEnum, ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum,
    FunctionType as LlvmFunctionType, IntType, PointerType, StructType, VoidType,
};
use inkwell::values::{
    ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
    GlobalValue, InstructionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel, ThreadLocalMode};
use itertools::Itertools;
use llvm_sys::debuginfo::LLVMDIBuilderInsertDbgValueAtEnd;
use log::{debug, info, trace};
use smallvec::smallvec;

use crate::SV8;
use crate::compiler::WordSize;
use crate::lex::SpanId;
use crate::parse::{FileId, Ident, StringId};
use crate::typer::scopes::ScopeId;
use crate::typer::static_value::{StaticContainerKind, StaticValuePool};
use crate::typer::types::{
    BOOL_TYPE_ID, CHAR_TYPE_ID, FloatType, FnParamType, I8_TYPE_ID, I16_TYPE_ID, I32_TYPE_ID,
    I64_TYPE_ID, IntegerType, NEVER_TYPE_ID, POINTER_TYPE_ID, STRING_TYPE_ID, Type, TypeDefnInfo,
    TypeId, U8_TYPE_ID, U16_TYPE_ID, U32_TYPE_ID, U64_TYPE_ID, UNIT_TYPE_ID, UWORD_TYPE_ID,
};
use crate::typer::{
    AssignmentKind, Call, CallId, Callee, CastType, FunctionId, IntegerCastDirection,
    IntrinsicArithOpClass, IntrinsicArithOpOp, IntrinsicBitwiseBinopKind, IntrinsicOperation,
    Layout, LetStmt, Linkage as TyperLinkage, LoopExpr, MatchingCondition, MatchingConditionInstr,
    StaticValue, StaticValueId, TypedBlock, TypedCast, TypedExpr, TypedExprId, TypedFloatValue,
    TypedFunction, TypedGlobalId, TypedIntValue, TypedMatchExpr, TypedProgram, TypedStmt,
    TypedStmtId, VariableId, WhileLoop,
};

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
    pub span: SpanId,
}

macro_rules! return_void {
    ($e:expr) => {
        match $e {
            LlvmValue::BasicValue(v) => v,
            other => return Ok(other),
        }
    };
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

#[allow(unused)]
fn llvm_size_info(td: &TargetData, typ: &dyn AnyType) -> Layout {
    Layout { size: td.get_bit_size(typ) as u32 / 8, align: td.get_abi_alignment(typ) }
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

// TODO(perf): cache llvm function types by type id
pub struct K1LlvmFunctionType<'ctx> {
    #[allow(unused)]
    type_id: TypeId,
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

#[derive(Debug, Clone)]
struct LlvmReferenceType<'ctx> {
    type_id: TypeId,
    pointer_type: PointerType<'ctx>,
    pointee_type: Box<K1LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
    layout: Layout,
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
    layout: Layout,
}

#[derive(Debug, Clone)]
struct EnumVariantType<'ctx> {
    name: Ident,
    envelope_type: ArrayType<'ctx>,
    variant_struct_type: StructType<'ctx>,
    payload_type: Option<K1LlvmType<'ctx>>,
    tag_value: IntValue<'ctx>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Debug, Clone)]
struct LlvmEnumType<'ctx> {
    type_id: TypeId,
    #[allow(unused)]
    tag_type: IntType<'ctx>,
    base_array_type: ArrayType<'ctx>,
    variants: Vec<EnumVariantType<'ctx>>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Debug, Clone)]
struct LlvmStructType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    fields: Vec<K1LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

#[derive(Debug, Clone)]
struct LlvmArrayType<'ctx> {
    type_id: TypeId,
    #[allow(unused)]
    count: u64,
    array_type: ArrayType<'ctx>,
    element_type: Box<K1LlvmType<'ctx>>,
    di_type: DIType<'ctx>,
    layout: Layout,
}

// Can this just be 'struct'? :()
#[derive(Debug, Clone)]
struct LlvmLambdaObjectType<'ctx> {
    type_id: TypeId,
    struct_type: StructType<'ctx>,
    di_type: DIType<'ctx>,
    size: Layout,
}

#[derive(Debug, Clone)]
enum K1LlvmType<'ctx> {
    Value(LlvmValueType<'ctx>),
    EnumType(LlvmEnumType<'ctx>),
    StructType(LlvmStructType<'ctx>),
    ArrayType(LlvmArrayType<'ctx>),
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
    // pub fn size_info(&self) -> Layout {
    //     match self {
    //         K1LlvmType::Value(v) => v.layout,
    //         K1LlvmType::Reference(r) => r.layout,
    //         K1LlvmType::EnumType(e) => e.layout,
    //         K1LlvmType::StructType(s) => s.layout,
    //         K1LlvmType::ArrayType(a) => a.layout,
    //         K1LlvmType::Void(_) => Layout::ZERO,
    //         K1LlvmType::LambdaObject(c) => c.size,
    //     }
    // }

    pub fn is_aggregate(&self) -> bool {
        match self {
            K1LlvmType::Value(_) => false,
            K1LlvmType::Reference(_) => false,
            K1LlvmType::Void(_) => false,
            K1LlvmType::EnumType(_) => true,
            K1LlvmType::StructType(_) => true,
            K1LlvmType::ArrayType(_) => true,
            K1LlvmType::LambdaObject(_) => true,
        }
    }

    #[track_caller]
    pub fn expect_reference(&self) -> &LlvmReferenceType<'ctx> {
        match self {
            K1LlvmType::Reference(reference) => reference,
            _ => panic!("expected pointer on {self:?}"),
        }
    }

    #[track_caller]
    pub fn expect_enum(self) -> LlvmEnumType<'ctx> {
        match self {
            K1LlvmType::EnumType(e) => e,
            _ => panic!("expected enum on {self:?}"),
        }
    }

    #[track_caller]
    fn expect_struct(self) -> LlvmStructType<'ctx> {
        match self {
            K1LlvmType::StructType(s) => s,
            _ => panic!("expected struct on {self:?}"),
        }
    }

    #[track_caller]
    fn expect_lambda_object(self) -> LlvmLambdaObjectType<'ctx> {
        match self {
            K1LlvmType::LambdaObject(lam_obj) => lam_obj,
            _ => panic!("expected struct on {self:?}"),
        }
    }

    #[track_caller]
    fn expect_array(self) -> LlvmArrayType<'ctx> {
        match self {
            K1LlvmType::ArrayType(array) => array,
            _ => panic!("expected array on {self:?}"),
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
            K1LlvmType::ArrayType(a) => a.type_id,
            K1LlvmType::Void(_) => NEVER_TYPE_ID,
            K1LlvmType::LambdaObject(c) => c.type_id,
        }
    }

    fn canonical_repr_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.basic_type,
            K1LlvmType::Reference(pointer) => pointer.pointer_type.as_basic_type_enum(),
            K1LlvmType::EnumType(e) => e
                .base_array_type
                .get_context()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
            K1LlvmType::StructType(s) => {
                s.struct_type.get_context().ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
            K1LlvmType::ArrayType(a) => {
                a.array_type.get_context().ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
            K1LlvmType::LambdaObject(c) => {
                c.struct_type.get_context().ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
            K1LlvmType::Void(_) => panic!("No canonical repr type on Void / never"),
        }
    }

    fn rich_repr_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.basic_type,
            K1LlvmType::Reference(pointer) => pointer.pointer_type.as_basic_type_enum(),
            K1LlvmType::EnumType(e) => e.base_array_type.as_basic_type_enum(),
            K1LlvmType::StructType(s) => s.struct_type.as_basic_type_enum(),
            K1LlvmType::ArrayType(a) => a.array_type.as_basic_type_enum(),
            K1LlvmType::Void(_) => panic!("No rich value type on Void / never"),
            K1LlvmType::LambdaObject(c) => c.struct_type.as_basic_type_enum(),
        }
    }

    fn rich_repr_layout(&self) -> Layout {
        match self {
            K1LlvmType::Value(value) => value.layout,
            K1LlvmType::Reference(pointer) => pointer.layout,
            K1LlvmType::EnumType(e) => e.layout,
            K1LlvmType::StructType(s) => s.layout,
            K1LlvmType::ArrayType(a) => a.layout,
            K1LlvmType::Void(_) => panic!("No rich value type on Void / never"),
            K1LlvmType::LambdaObject(c) => c.size,
        }
    }

    fn debug_type(&self) -> DIType<'ctx> {
        match self {
            K1LlvmType::Value(value) => value.di_type,
            K1LlvmType::Reference(pointer) => pointer.di_type,
            K1LlvmType::EnumType(e) => e.di_type,
            K1LlvmType::StructType(s) => s.di_type,
            K1LlvmType::ArrayType(a) => a.di_type,
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
    pub fn unit_basic(&self) -> BasicValueEnum<'ctx> {
        self.unit_value.as_basic_value_enum()
    }
    pub fn uword(&self) -> IntType<'ctx> {
        self.ptr_sized_int
    }
}

#[derive(Clone, Copy)]
pub struct LoopInfo<'ctx> {
    pub break_value_ptr: Option<PointerValue<'ctx>>,
    pub break_type: Option<TypeId>,
    pub end_block: BasicBlock<'ctx>,
}

pub struct CodegenedFunction<'ctx> {
    pub function_type: K1LlvmFunctionType<'ctx>,
    pub function_value: FunctionValue<'ctx>,
    pub sret_pointer: Option<PointerValue<'ctx>>,
    pub last_alloca_instr: Option<InstructionValue<'ctx>>,
    pub instruction_count: usize,
}

pub struct Codegen<'ctx, 'k1> {
    ctx: &'ctx Context,
    pub k1: &'k1 TypedProgram,
    llvm_module: LlvmModule<'ctx>,
    llvm_machine: TargetMachine,
    builder: Builder<'ctx>,
    pub llvm_functions: FxHashMap<FunctionId, CodegenedFunction<'ctx>>,
    pub llvm_function_to_k1: FxHashMap<FunctionValue<'ctx>, FunctionId>,
    functions_pending_body_compilation: Vec<FunctionId>,
    llvm_types: RefCell<FxHashMap<TypeId, K1LlvmType<'ctx>>>,
    variable_to_value: FxHashMap<VariableId, VariableValue<'ctx>>,
    lambda_functions: FxHashMap<TypeId, FunctionValue<'ctx>>,
    loops: FxHashMap<ScopeId, LoopInfo<'ctx>>,
    builtin_types: BuiltinTypes<'ctx>,
    strings: FxHashMap<StringId, GlobalValue<'ctx>>,
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
    word_size: WordSize,
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
    ) -> InstructionValue<'ctx> {
        let value_ref = unsafe {
            LLVMDIBuilderInsertDbgValueAtEnd(
                self.debug_builder.as_mut_ptr(),
                value.as_value_ref(),
                var_info.as_mut_ptr(),
                expr.unwrap_or_else(|| self.debug_builder.create_expression(vec![])).as_mut_ptr(),
                debug_loc.as_mut_ptr(),
                block.as_mut_ptr(),
            )
        };

        unsafe { InstructionValue::new(value_ref) }
    }

    fn create_pointer_type(&self, name: &str, pointee: DIType<'ctx>) -> DIType<'ctx> {
        self.debug_builder
            .create_pointer_type(
                name,
                pointee,
                self.word_size.bits() as u64,
                self.word_size.bits(),
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
    fn current_scope(&self) -> DIScope<'ctx> {
        self.current_entry().scope
    }
    fn current_file(&self) -> DIFile<'ctx> {
        self.current_entry().file
    }
}

#[derive(Copy, Clone, Debug)]
enum VariableValue<'ctx> {
    Indirect {
        pointer_value: PointerValue<'ctx>,
    },
    Direct {
        value: BasicValueEnum<'ctx>,
    },
    /// Call this function using sret to get the value of this variable
    /// Used for complex static values that cannot be built using LLVM's
    /// constant and global mechanisms
    ByFunctionCall {
        function: FunctionValue<'ctx>,
    },
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

impl<'ctx, 'module> Codegen<'ctx, 'module> {
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
            word_size: module.ast.config.target.word_size(),
        };
        debug.push_scope(SpanId::NONE, compile_unit.as_debug_info_scope(), compile_unit.get_file());
        debug
    }

    pub fn create(
        ctx: &'ctx Context,
        module: &'module TypedProgram,
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

        Codegen {
            ctx,
            k1: module,
            llvm_module,
            llvm_machine: machine,
            builder,
            variable_to_value: FxHashMap::new(),
            lambda_functions: FxHashMap::new(),
            loops: FxHashMap::new(),
            llvm_functions: FxHashMap::new(),
            llvm_function_to_k1: FxHashMap::new(),
            functions_pending_body_compilation: Vec::new(),
            llvm_types: RefCell::new(FxHashMap::new()),
            builtin_types,
            strings: FxHashMap::new(),
            debug: debug_context,
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

    fn offset_of_struct_member(&self, typ: &StructType, field_index: u32) -> u64 {
        let td = self.llvm_machine.get_target_data();
        let offset_bytes = td.offset_of_element(typ, field_index).unwrap();
        offset_bytes * 8
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
        w: &mut impl std::io::Write,
        type_id: TypeId,
        defn_info: Option<TypeDefnInfo>,
    ) {
        // FIXME: We need to revisit the entire story around names in codegen
        let name = self.k1.type_id_to_string(type_id);
        match defn_info {
            None => write!(w, "{}", name).unwrap(),
            Some(info) => self.k1.write_qualified_name(w, info.scope, &name, "/", true),
        };
    }

    fn codegen_type_name(&self, type_id: TypeId, defn_info: Option<TypeDefnInfo>) -> String {
        let mut s = Vec::with_capacity(64);
        self.write_type_name(&mut s, type_id, defn_info);
        String::from_utf8(s).unwrap()
    }

    fn make_debug_struct_type<'names>(
        &self,
        name: &str,
        type_id: TypeId,
        struct_type: &StructType,
        span: SpanId,
        field_types: &[StructDebugMember<'ctx, 'names>],
    ) -> DIType<'ctx> {
        let line_number = self.get_line_number(span);
        let layout = self.get_layout(type_id);
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
                layout.size as u64,
                layout.align,
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
            layout: self.get_layout(POINTER_TYPE_ID),
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

    fn codegen_type(&self, type_id: TypeId) -> CodegenResult<K1LlvmType<'ctx>> {
        self.codegen_type_inner(type_id, 0)
    }

    fn codegen_type_inner(&self, type_id: TypeId, depth: usize) -> CodegenResult<K1LlvmType<'ctx>> {
        let result = self.llvm_types.borrow().get(&type_id).cloned();
        if let Some(result) = result {
            return Ok(result);
        };
        let dw_ate_address = 0x01;
        let dw_ate_boolean = 0x02;
        let _dw_ate_complex_float = 0x03;
        let dw_ate_float = 0x04;
        let dw_ate_signed = 0x05;
        let dw_ate_char = 0x06;
        let dw_ate_unsigned = 0x07;
        let _dw_ate_unsigned_char = 0x08;
        let make_value_basic_type =
            |name: &str,
             type_id: TypeId,
             basic_type: BasicTypeEnum<'ctx>,
             encoding: llvm_sys::debuginfo::LLVMDWARFTypeEncoding| {
                let layout = self.get_layout(type_id);
                K1LlvmType::Value(LlvmValueType {
                    type_id,
                    basic_type,
                    layout,
                    di_type: self
                        .debug
                        .debug_builder
                        .create_basic_type(name, layout.size_bits() as u64, encoding, 0)
                        .unwrap()
                        .as_type(),
                })
            };
        debug!("codegen for type {} depth {depth}", self.k1.type_id_to_string(type_id));
        let mut no_cache = false;
        // Might be better to switch to the debug context span, rather than the type's span
        let span = self.k1.get_span_for_type_id(type_id).unwrap_or(SpanId::NONE);
        let codegened_type = match self.k1.types.get_no_follow(type_id) {
            Type::Unit => Ok(make_value_basic_type(
                "unit",
                UNIT_TYPE_ID,
                self.builtin_types.unit.as_basic_type_enum(),
                dw_ate_boolean,
            )),
            Type::Char => Ok(make_value_basic_type(
                "char",
                CHAR_TYPE_ID,
                self.builtin_types.char.as_basic_type_enum(),
                dw_ate_char,
            )),
            Type::Integer(IntegerType::U8) => Ok(make_value_basic_type(
                "u8",
                U8_TYPE_ID,
                self.ctx.i8_type().as_basic_type_enum(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::U16) => Ok(make_value_basic_type(
                "u16",
                U16_TYPE_ID,
                self.ctx.i16_type().as_basic_type_enum(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::U32) => Ok(make_value_basic_type(
                "u32",
                U32_TYPE_ID,
                self.ctx.i32_type().as_basic_type_enum(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::U64) => Ok(make_value_basic_type(
                "u64",
                U64_TYPE_ID,
                self.ctx.i64_type().as_basic_type_enum(),
                dw_ate_unsigned,
            )),
            Type::Integer(IntegerType::UWord(w)) => {
                let llvm_type = self.builtin_types.ptr_sized_int;
                assert_eq!(llvm_type.get_bit_width(), w.width().bits());
                Ok(make_value_basic_type(
                    "uword",
                    UWORD_TYPE_ID,
                    llvm_type.as_basic_type_enum(),
                    dw_ate_unsigned,
                ))
            }
            Type::Integer(IntegerType::I8) => Ok(make_value_basic_type(
                "i8",
                I8_TYPE_ID,
                self.ctx.i8_type().as_basic_type_enum(),
                dw_ate_signed,
            )),
            Type::Integer(IntegerType::I16) => Ok(make_value_basic_type(
                "i16",
                I16_TYPE_ID,
                self.ctx.i16_type().as_basic_type_enum(),
                dw_ate_signed,
            )),
            Type::Integer(IntegerType::I32) => Ok(make_value_basic_type(
                "i32",
                I32_TYPE_ID,
                self.ctx.i32_type().as_basic_type_enum(),
                dw_ate_signed,
            )),
            Type::Integer(IntegerType::I64) => Ok(make_value_basic_type(
                "i64",
                I64_TYPE_ID,
                self.ctx.i64_type().as_basic_type_enum(),
                dw_ate_signed,
            )),
            Type::Integer(IntegerType::IWord(w)) => {
                let llvm_type = self.builtin_types.ptr_sized_int;
                assert_eq!(llvm_type.get_bit_width(), w.width().bits());
                Ok(make_value_basic_type(
                    "iword",
                    UWORD_TYPE_ID,
                    llvm_type.as_basic_type_enum(),
                    dw_ate_signed,
                ))
            }
            Type::Float(float_type) => {
                let llvm_type = match float_type {
                    FloatType::F32 => self.ctx.f32_type(),
                    FloatType::F64 => self.ctx.f64_type(),
                };
                let name = match float_type {
                    FloatType::F32 => "f32",
                    FloatType::F64 => "f64",
                };
                Ok(make_value_basic_type(
                    name,
                    type_id,
                    llvm_type.as_basic_type_enum(),
                    dw_ate_float,
                ))
            }
            Type::Bool => Ok(make_value_basic_type(
                "bool",
                BOOL_TYPE_ID,
                self.builtin_types.boolean.as_basic_type_enum(),
                dw_ate_boolean,
            )),
            Type::Pointer => {
                let llvm_type = self.builtin_types.ptr.as_basic_type_enum();
                Ok(make_value_basic_type("Pointer", POINTER_TYPE_ID, llvm_type, dw_ate_address))
            }
            Type::Struct(struc) => {
                let buffer_element_type = self.k1.types.get_as_buffer_instance(type_id);
                trace!("generating llvm type for struct type {type_id}");
                let field_count = struc.fields.len();
                let mut field_types = Vec::with_capacity(field_count as usize);
                let mut field_basic_types = Vec::with_capacity(field_count as usize);
                let mut field_di_types: Vec<StructDebugMember> =
                    Vec::with_capacity(field_count as usize);
                let name = self.codegen_type_name(type_id, self.k1.types.get_defn_info(type_id));
                for field in self.k1.types.mem.get_slice(struc.fields) {
                    let field_llvm_type = self.codegen_type_inner(field.type_id, depth + 1)?;
                    let debug_type = if buffer_element_type.is_some()
                        && field.name == self.k1.ast.idents.b.data
                    {
                        let element_type =
                            self.codegen_type_inner(buffer_element_type.unwrap(), depth + 1)?;
                        let array_ptr_di_type = self.make_pointer_type(element_type.debug_type());
                        array_ptr_di_type.di_type
                    } else {
                        field_llvm_type.debug_type()
                    };
                    field_di_types.push(StructDebugMember {
                        name: self.k1.ast.idents.get_name(field.name),
                        di_type: debug_type,
                    });
                    field_basic_types.push(field_llvm_type.rich_repr_type());
                    field_types.push(field_llvm_type);
                }

                let llvm_struct_type = self.ctx.struct_type(&field_basic_types, false);

                let layout = self.get_layout(type_id);
                let di_type = self.make_debug_struct_type(
                    &name,
                    type_id,
                    &llvm_struct_type,
                    span,
                    &field_di_types,
                );
                Ok(LlvmStructType {
                    type_id,
                    struct_type: llvm_struct_type,
                    fields: field_types,
                    layout,
                    di_type,
                }
                .into())
            }
            Type::TypeParameter(_tp) => {
                failf!(
                    self.debug.current_entry().span,
                    "codegen was asked to codegen a type parameter {}",
                    self.k1.dump_type_id_to_string(type_id)
                )
            }
            Type::FunctionTypeParameter(ftp) => {
                failf!(span, "codegen was asked to codegen a function type parameter {:?}", ftp)
            }
            Type::InferenceHole(h) => {
                failf!(span, "codegen was asked to codegen a type inference hole {:?}", h)
            }
            Type::FunctionPointer(fp) => {
                let placeholder_pointee = self.codegen_type(I8_TYPE_ID)?;
                let llvm_function_type = self.make_llvm_function_type(fp.function_type_id)?;
                let subroutine_type = self.debug.debug_builder.create_subroutine_type(
                    self.debug.current_file(),
                    Some(llvm_function_type.return_type.debug_type()),
                    &llvm_function_type
                        .param_types
                        .iter()
                        .map(|t| t.debug_type())
                        .collect::<Vec<_>>(),
                    0,
                );

                Ok(LlvmReferenceType {
                    type_id,
                    pointer_type: self.builtin_types.ptr,
                    di_type: self.debug.create_pointer_type(
                        &format!("fn_ptr_{}", type_id),
                        // Safety: DISubroutineType and DIType are both just LLVMMetadataRefs
                        unsafe {
                            core::mem::transmute::<DISubroutineType<'ctx>, DIType<'ctx>>(
                                subroutine_type,
                            )
                        },
                    ),
                    pointee_type: Box::new(placeholder_pointee),
                    layout: Layout::from_scalar_bits(self.word_size().bits()),
                }
                .into())
            }
            Type::Reference(reference) => {
                let inner_type = self.codegen_type_inner(reference.inner_type, depth + 1)?;
                let inner_debug_type = inner_type.debug_type();
                Ok(LlvmReferenceType {
                    type_id,
                    pointer_type: self.builtin_types.ptr,
                    pointee_type: Box::new(inner_type),
                    di_type: self
                        .debug
                        .create_pointer_type(&format!("reference_{}", type_id), inner_debug_type),
                    layout: Layout::from_scalar_bits(self.word_size().bits()),
                }
                .into())
            }
            Type::Array(array_type) => {
                let element_type = self.codegen_type_inner(array_type.element_type, depth + 1)?;
                let element_basic_type = element_type.rich_repr_type();
                let Some(size) = array_type.concrete_count else {
                    self.k1.ice("codegen_type_inner: expected concrete count for array type", None);
                };
                let llvm_array_type = element_basic_type.array_type(size as u32);

                let layout = self.get_layout(type_id);

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

                Ok(K1LlvmType::ArrayType(LlvmArrayType {
                    type_id,
                    count: size,
                    array_type: llvm_array_type,
                    element_type: Box::new(element_type),
                    di_type,
                    layout,
                }))
            }
            Type::Enum(enum_type) => {
                let enum_name = self
                    .k1
                    .types
                    .get_defn_info(type_id)
                    .map(|info| self.codegen_type_name(type_id, Some(info)))
                    .unwrap_or(type_id.to_string());
                let enum_layout = self.k1.types.get_layout(type_id);
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
                let tag_int_type =
                    self.codegen_type(enum_type.tag_type)?.rich_repr_type().into_int_type();
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
                                variant_payload_type.rich_repr_type(),
                            ];
                            let variant_struct_type =
                                Codegen::make_named_struct(self.ctx, &variant_name, fields);
                            let debug_struct = self.make_debug_struct_type(
                                &variant_name,
                                variant.my_type_id,
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
                                    variant.my_type_id,
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
                        self.get_layout(variant.my_type_id)
                    );
                    variant_structs.push(EnumVariantType {
                        name: variant.name,
                        envelope_type: self.ctx.i8_type().array_type(0),
                        variant_struct_type: variant_struct,
                        payload_type,
                        tag_value: tag_int_type
                            .const_int(variant.tag_value.to_u64_unconditional(), false),
                        di_type: variant_struct_debug,
                        layout: self.get_layout(variant.my_type_id),
                    });
                }

                // Enum sizing and layout rules:
                // - Alignment of the enum is the max(alignment) of the variants
                // - Size of the enum is the size of the largest variant, not necessarily the same
                //   variant, plus alignment end padding
                // - *********** NEW: We no longer rely on fabricating an 'envelope' struct for the
                //   union. Instead we represent the enum itself as an i8 array, and only the
                //   variants as structs, and use LLVM's `align` attribute where needed to specify
                //   the alignment of the enum.
                // - *********** OLD: In order to get to an actual LLVM type that has this alignment and size, we copy clang,
                //   and 'devise' a struct that will do it for us. This struct is simply 2 fields:
                //   - First, the strictestly-aligned variant. This sets the alignment of our
                //     struct
                //   - Second, padding bytes such that we're at least as large as the largest
                //     variant, and aligned to our own alignment

                let physical_type = self.ctx.i8_type().array_type(enum_layout.size);
                let enum_size_bits = enum_layout.size_bits();

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
                                variant.layout.size as u64,
                                variant.layout.align,
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
                        enum_layout.align_bits(),
                        0,
                        &member_types,
                        0,
                        &enum_name,
                    )
                    .as_type();

                Ok(LlvmEnumType {
                    type_id,
                    tag_type: tag_int_type,
                    base_array_type: physical_type,
                    variants: variant_structs,
                    di_type: debug_union_type,
                    layout: enum_layout,
                }
                .into())
            }
            Type::EnumVariant(ev) => {
                let parent_enum = self.codegen_type(ev.enum_type_id)?.expect_enum();
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
                        .k1
                        .types
                        .get_defn_info(rr.root_type_id)
                        .expect("recursive type must have defn info");

                    let name = self.codegen_type_name(type_id, Some(defn_info));
                    let s = self.ctx.opaque_struct_type(&name);

                    no_cache = true;
                    Ok(K1LlvmType::StructType(LlvmStructType {
                        type_id,
                        struct_type: s,
                        fields: Vec::new(),
                        di_type: self.make_debug_struct_type(
                            &name,
                            rr.root_type_id,
                            &s,
                            SpanId::NONE,
                            &[],
                        ),
                        layout: self.get_layout(rr.root_type_id),
                    }))
                }
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
                    size: struct_type.layout,
                }))
            }
            Type::Static(stat) => self.codegen_type(stat.inner_type_id),
            Type::Function(_function_type) => {
                panic!("Cannot codegen a naked Function type")
            }
            Type::Unresolved(_) => {
                panic!("Cannot codegen type for Unresolved; something went wrong in typecheck")
            }
            Type::Generic(_) => {
                panic!("Cannot codegen type for Generic; something went wrong in typecheck")
            }
        }?;
        if !no_cache {
            self.llvm_types.borrow_mut().insert(type_id, codegened_type.clone());
        }
        // let is_never = matches!(self.k1.types.get_no_follow(type_id), Type::Never);
        // if !is_never {
        //     let llvm_layout = self.llvm_size_info(&codegened_type.rich_repr_type());
        //     let k1_layout = self.k1.types.layouts.get(type_id);
        //     if llvm_layout.size != k1_layout.stride() {
        //         self.k1.ice(
        //             format!(
        //                 "DIFFERENT SIZES for {} {} llvm={} k1={}",
        //                 self.k1.types.get(type_id).kind_name(),
        //                 self.k1.type_id_to_string(type_id),
        //                 llvm_layout.size,
        //                 k1_layout.stride()
        //             ),
        //             None,
        //         )
        //     }
        // }
        Ok(codegened_type)
    }

    fn make_llvm_function_type(
        &self,
        function_type_id: TypeId,
    ) -> CodegenResult<K1LlvmFunctionType<'ctx>> {
        let function_type = self.k1.types.get(function_type_id).as_function().unwrap();
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

        Ok(K1LlvmFunctionType {
            type_id: function_type_id,
            llvm_function_type: fn_type,
            param_types,
            return_type,
            is_sret,
        })
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
        let variable_type = self.codegen_type(let_stmt.variable_type)?;
        let variable = self.k1.variables.get(let_stmt.variable_id);
        let name = self.get_ident_name(variable.name).to_string();

        let local_variable = if !variable_type.is_void() {
            Some(self.debug.debug_builder.create_auto_variable(
                self.debug.current_scope(),
                &name,
                self.debug.current_file(),
                self.get_line_number(let_stmt.span),
                variable_type.debug_type(),
                true,
                0,
                variable_type.rich_repr_layout().align,
            ))
        } else {
            None
        };

        // Some 'lets' don't need an extra alloca; if they are not re-assigned
        // and they are not 'referencing' then the value representation is fine
        let value = match let_stmt.initializer {
            None => None,
            Some(initializer) => {
                let value = self.codegen_expr(initializer)?;
                if let LlvmValue::Void(instr) = value {
                    return Ok(LlvmValue::Void(instr));
                }
                Some(value.expect_basic_value())
            }
        };

        let variable_value = {
            let variable_ptr = self.build_k1_alloca(&variable_type, &name);
            self.debug.debug_builder.insert_declare_at_end(
                variable_ptr,
                local_variable,
                None,
                self.builder.get_current_debug_location().unwrap(),
                self.builder.get_insert_block().unwrap(),
            );

            if let_stmt.is_referencing {
                // If this is a let*, then we put the rhs behind another alloca so that we end up
                // with a pointer to the value
                let Type::Reference(reference_type) = self.k1.types.get(variable_type.type_id())
                else {
                    panic!("Expected reference for referencing let");
                };
                let reference_inner_llvm_type = self.codegen_type(reference_type.inner_type)?;
                if reference_inner_llvm_type.is_aggregate() {
                    if let Some(value) = value {
                        debug_assert!(value.is_pointer_value());
                        self.builder.build_store(variable_ptr, value).unwrap();
                    }
                } else {
                    // We need 2 allocas here because we need to store
                    // an address in an alloca, and the address needs to be
                    // a stack address.
                    let value_ptr = self.build_k1_alloca(&reference_inner_llvm_type, "");
                    if let Some(value) = value {
                        self.builder.build_store(value_ptr, value).unwrap();
                    }
                    self.builder.build_store(variable_ptr, value_ptr).unwrap();
                }
            } else {
                if let Some(value) = value {
                    self.store_k1_value(&variable_type, variable_ptr, value);
                }
            };

            VariableValue::Indirect { pointer_value: variable_ptr }
        };

        trace!(
            "codegen_let referencing={} {}: pointee_ty: {variable_type:?}",
            let_stmt.is_referencing, name
        );

        // Disable to hide compiler-internal variables!
        // It depends if we are debugging the user program or debugging the compiler

        // if !self.module.name_of(variable.name).starts_with("__") {
        // }

        self.variable_to_value.insert(let_stmt.variable_id, variable_value);
        Ok(self.builtin_types.unit_basic().into())
    }

    fn codegen_match(&mut self, match_expr: &TypedMatchExpr) -> CodegenResult<LlvmValue<'ctx>> {
        for stmt in &match_expr.initial_let_statements {
            if let instr @ LlvmValue::Void(_) = self.codegen_statement(*stmt)? {
                return Ok(instr);
            }
        }

        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let mut arm_blocks: SV8<_> = smallvec![];
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
            let next_arm = arm_blocks.get(index + 1);
            let next_arm_or_fail = next_arm.map(|p| p.0).unwrap_or(fail_block);

            self.builder.position_at_end(*arm_block);
            self.codegen_matching_condition(&arm.condition, *arm_cons_block, next_arm_or_fail)?;

            self.builder.position_at_end(*arm_cons_block);
            let LlvmValue::BasicValue(consequent) = self.codegen_expr(arm.consequent_expr)? else {
                // If the consequent crashes or returns early, this block is terminated so just
                // continue to the next arm
                continue 'arm;
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
        &mut self,
        llvm_type: &K1LlvmType<'ctx>,
        source: PointerValue<'ctx>,
        name: &str,
        make_copy: bool,
    ) -> BasicValueEnum<'ctx> {
        if llvm_type.is_aggregate() {
            if make_copy {
                self.alloca_copy_entire_value(source, llvm_type, &format!("{name}_copy"))
                    .as_basic_value_enum()
            } else {
                // No-op; we want to interact with these types as pointers
                debug!(
                    "smart loading noop on type {}",
                    self.k1.type_id_to_string(llvm_type.type_id())
                );
                source.as_basic_value_enum()
            }
        } else {
            // Scalars must be truly loaded
            self.builder.build_load(llvm_type.rich_repr_type(), source, name).unwrap()
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
            self.memcpy_entire_value(dest, value.into_pointer_value(), llvm_type)
        } else {
            let store = self.builder.build_store(dest, value).unwrap();
            store.set_alignment(llvm_type.rich_repr_layout().align).unwrap();
            store
        }
    }

    fn alloca_copy_entire_value(
        &mut self,
        src: PointerValue<'ctx>,
        ty: &K1LlvmType<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let dst = self.build_k1_alloca(ty, name);
        self.memcpy_entire_value(dst, src, ty);
        dst
    }

    fn memcpy_entire_value(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        ty: &K1LlvmType<'ctx>,
    ) -> InstructionValue<'ctx> {
        let size = self.builtin_types.ptr_sized_int;
        let layout = ty.rich_repr_layout();
        let bytes = size.const_int(layout.size as u64, false);
        let align_bytes = layout.align;
        self.builder
            .build_memcpy(dst, align_bytes, src, align_bytes, bytes)
            .unwrap()
            .as_instruction_value()
            .unwrap()
    }

    fn load_variable_value(
        &mut self,
        k1_llvm_type: &K1LlvmType<'ctx>,
        variable_value: VariableValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match variable_value {
            VariableValue::Indirect { pointer_value } => {
                self.load_k1_value(k1_llvm_type, pointer_value, "", false)
            }
            VariableValue::Direct { value } => value,
            VariableValue::ByFunctionCall { function } => {
                let ptr = self.build_k1_alloca(k1_llvm_type, "load_variable_sret");
                let callsite_value = self.builder.build_call(function, &[ptr.into()], "").unwrap();
                // Call-site sret
                let sret_attribute =
                    self.make_sret_attribute(k1_llvm_type.rich_repr_type().as_any_type_enum());
                eprintln!("I generated a call to static maker function: {}", callsite_value);
                callsite_value.add_attribute(AttributeLoc::Param(0), sret_attribute);
                ptr.as_basic_value_enum()
            }
        }
    }

    fn codegen_static_value_as_const(
        &mut self,
        static_value_id: StaticValueId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        debug!("codegen_static_value_as_const {}", self.k1.static_value_to_string(static_value_id));

        if !is_llvm_const_representable(&self.k1.static_values, static_value_id) {
            self.k1.ice(
                "Tried to codegen a static value outside of a function context, which is not allowed",
                None
            )
        }

        let result = match self.k1.static_values.get(static_value_id) {
            StaticValue::Unit => self.builtin_types.unit_basic(),
            StaticValue::Bool(b) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum(),
                false => self.builtin_types.false_value.as_basic_value_enum(),
            },
            StaticValue::Char(byte) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum()
            }
            StaticValue::Int(int_value) => self.codegen_integer_value(*int_value).unwrap(),
            StaticValue::Float(float_value) => self.codegen_float_value(*float_value).unwrap(),
            StaticValue::String(string_id) => {
                let string_struct = self.codegen_string_id(*string_id).unwrap();
                string_struct.get_initializer().unwrap()
            }
            StaticValue::Zero(type_id) => {
                let llvm_type = self.codegen_type(*type_id)?;
                let zero = llvm_type.rich_repr_type().const_zero();
                zero.as_basic_value_enum()
            }
            StaticValue::Struct(s) => {
                let mut type_fields = Vec::with_capacity(s.fields.len() as usize);
                let mut fields_basic_values = Vec::with_capacity(s.fields.len() as usize);
                // We actually have to specialize the LLVM struct type here because it won't allow
                // an Opt[i64] type with a Some[i64] value
                // This is also why we can't represent non-empty Buffers as llvm-const values
                // We have no way to change the types, or to reference other buffers
                for field in self.k1.static_values.get_slice(s.fields).iter() {
                    let value = self.codegen_static_value_as_const(*field)?;
                    type_fields.push(value.get_type());
                    fields_basic_values.push(value);
                }
                let specialized_type = self.ctx.struct_type(&type_fields, false);
                let struct_value = specialized_type.const_named_struct(&fields_basic_values);
                struct_value.as_basic_value_enum()
            }
            StaticValue::Enum(e) => {
                let llvm_type = self.codegen_type(e.variant_type_id)?.expect_enum();

                let variant = &llvm_type.variants[e.variant_index as usize];
                let physical_struct = variant.variant_struct_type;
                let enum_value = match e.payload {
                    None => physical_struct
                        .const_named_struct(&[variant.tag_value.as_basic_value_enum()]),
                    Some(payload_comptime_value_id) => {
                        let payload_value =
                            self.codegen_static_value_as_const(payload_comptime_value_id)?;
                        // We have to construct a unique type for this value because
                        // if we use the variant struct, the payload will come back with
                        // a more specific type than it, since everything is represented as its
                        // most specific, concrete types in this static world (specifically enum values)
                        let unique_struct_type = self.ctx.struct_type(
                            &[llvm_type.tag_type.as_basic_type_enum(), payload_value.get_type()],
                            false,
                        );

                        // Sanity check since we'll perform a load on this as _our_ type
                        debug_assert_eq!(
                            self.llvm_size_info(&unique_struct_type),
                            self.llvm_size_info(&variant.variant_struct_type)
                        );
                        unique_struct_type.const_named_struct(&[
                            variant.tag_value.as_basic_value_enum(),
                            payload_value,
                        ])
                    }
                };
                enum_value.as_basic_value_enum()
            }
            StaticValue::LinearContainer(view) => {
                let (element_type, _) =
                    self.k1.types.get_as_container_instance(view.type_id).unwrap();
                let array_value = self.codegen_static_elements_array(
                    element_type,
                    self.k1.static_values.get_slice(view.elements),
                )?;

                match view.kind {
                    StaticContainerKind::View => {
                        let data_global = self.llvm_module.add_global(
                            array_value.get_type(),
                            None,
                            &format!("elems_{}", static_value_id),
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
        Ok(result)
    }

    fn codegen_static_elements_array(
        &mut self,
        element_type: TypeId,
        elements: &[StaticValueId],
    ) -> CodegenResult<ArrayValue<'ctx>> {
        let element_backend_type = self.codegen_type(element_type)?;
        let element_basic_type = element_backend_type.rich_repr_type();
        let mut values: SV8<BasicValueEnum<'ctx>> = smallvec![];
        for elem in elements.iter() {
            let elem_basic_value = self.codegen_static_value_as_const(*elem)?;
            values.push(elem_basic_value);
        }
        let array_value = match element_basic_type {
            BasicTypeEnum::ArrayType(array_type) => array_type
                .const_array(&values.into_iter().map(|v| v.into_array_value()).collect_vec()),
            BasicTypeEnum::FloatType(float_type) => float_type
                .const_array(&values.into_iter().map(|v| v.into_float_value()).collect_vec()),
            BasicTypeEnum::IntType(int_type) => {
                int_type.const_array(&values.into_iter().map(|v| v.into_int_value()).collect_vec())
            }
            BasicTypeEnum::PointerType(pointer_type) => pointer_type
                .const_array(&values.into_iter().map(|v| v.into_pointer_value()).collect_vec()),
            BasicTypeEnum::StructType(struct_type) => struct_type
                .const_array(&values.into_iter().map(|v| v.into_struct_value()).collect_vec()),
            BasicTypeEnum::VectorType(_) => unreachable!(),
            BasicTypeEnum::ScalableVectorType(_) => unreachable!(),
        };
        Ok(array_value)
    }

    fn codegen_static_value_as_code(
        &mut self,
        static_value_id: StaticValueId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        debug!("codegen_static_value_as_code {}", self.k1.static_value_to_string(static_value_id));

        // Invariant: If the value is_statically_representable, we cannot emit any instructions
        //            since this value could be used in a global context
        //            However, if it is not, we will be inside a function context, and
        //            can emit instructions
        let insert_block = self.builder.get_insert_block();
        if insert_block.is_none()
            && !is_llvm_const_representable(&self.k1.static_values, static_value_id)
        {
            self.k1.ice(
                "Tried to codegen a static value outside of a function context, which is not allowed",
                None
            )
        }

        let v = self.k1.static_values.get(static_value_id);
        let result = match v {
            StaticValue::Unit => self.builtin_types.unit_basic(),
            StaticValue::Bool(b) => match b {
                true => self.builtin_types.true_value.as_basic_value_enum(),
                false => self.builtin_types.false_value.as_basic_value_enum(),
            },
            StaticValue::Char(byte) => {
                self.builtin_types.char.const_int(*byte as u64, false).as_basic_value_enum()
            }
            StaticValue::Int(int_value) => self.codegen_integer_value(*int_value).unwrap(),
            StaticValue::Float(float_value) => self.codegen_float_value(*float_value).unwrap(),
            StaticValue::String(string_id) => {
                let string_ptr = self.codegen_string_id(*string_id).unwrap();
                string_ptr.as_basic_value_enum()
            }
            StaticValue::Zero(type_id) => {
                let llvm_type = self.codegen_type(*type_id)?;
                let zero = llvm_type.rich_repr_type().const_zero();
                zero.as_basic_value_enum()
            }
            StaticValue::Struct(s) => {
                let llvm_type = self.codegen_type(s.type_id)?;

                // We cannot use build_k1_alloca since we aren't inside a k1 function
                // So we have to ensure we align properly ourselves
                // I'm not worried about pushing these allocas up to the top since we
                // only have 1 basic block anyway, we'll never branch in static values
                let dest_ptr =
                    self.builder.build_alloca(llvm_type.rich_repr_type(), "static_struc").unwrap();
                dest_ptr
                    .as_instruction()
                    .unwrap()
                    .set_alignment(llvm_type.rich_repr_layout().align)
                    .unwrap();

                let llvm_type = llvm_type.expect_struct();
                for (field_index, field) in
                    self.k1.static_values.get_slice(s.fields).iter().enumerate()
                {
                    let value = self.codegen_static_value_as_code(*field)?;
                    let field_llvm_type = &llvm_type.fields[field_index];
                    let dest_offset = self
                        .builder
                        .build_struct_gep(llvm_type.struct_type, dest_ptr, field_index as u32, "")
                        .unwrap();
                    self.store_k1_value(field_llvm_type, dest_offset, value);
                }
                dest_ptr.into()
            }
            StaticValue::Enum(e) => {
                let llvm_type = self.codegen_type(e.variant_type_id)?;
                let enum_ptr =
                    self.builder.build_alloca(llvm_type.rich_repr_type(), "static_struc").unwrap();
                enum_ptr
                    .as_instruction()
                    .unwrap()
                    .set_alignment(llvm_type.rich_repr_layout().align)
                    .unwrap();

                let llvm_type = llvm_type.expect_enum();
                let enum_variant = &llvm_type.variants[e.variant_index as usize];
                // Store the tag_value in the first slot
                let tag_pointer = self
                    .builder
                    .build_struct_gep(enum_variant.variant_struct_type, enum_ptr, 0, "")
                    .unwrap();
                self.builder.build_store(tag_pointer, enum_variant.tag_value).unwrap();

                if let Some(payload_id) = &e.payload {
                    let value = self.codegen_static_value_as_code(*payload_id)?;
                    let payload_pointer = self
                        .builder
                        .build_struct_gep(enum_variant.variant_struct_type, enum_ptr, 1, "")
                        .unwrap();
                    let payload_type = enum_variant.payload_type.as_ref().unwrap();
                    self.store_k1_value(payload_type, payload_pointer, value);
                }

                enum_ptr.into()
            }
            StaticValue::LinearContainer(view) => {
                let element_type = self.k1.types.get_as_view_instance(view.type_id).unwrap();
                let element_backend_type = self.codegen_type(element_type)?;
                let element_basic_type = element_backend_type.rich_repr_type();

                let array_type = element_basic_type.array_type(view.len() as u32);
                let data_global = self.llvm_module.add_global(array_type, None, "");
                data_global.set_constant(false);
                data_global.set_unnamed_addr(true);
                data_global.set_initializer(&array_type.const_zero());

                let mut values: SV8<BasicValueEnum<'ctx>> = smallvec![];
                for (index, elem) in
                    self.k1.static_values.get_slice(view.elements).iter().enumerate()
                {
                    let elem_basic_value = self.codegen_static_value_as_code(*elem)?;
                    let array_offset_ptr = unsafe {
                        self.builder
                            .build_in_bounds_gep(
                                element_basic_type,
                                data_global.as_pointer_value(),
                                &[self.ctx.i32_type().const_int(index as u64, false)],
                                "",
                            )
                            .unwrap()
                    };
                    self.store_k1_value(&element_backend_type, array_offset_ptr, elem_basic_value);
                    values.push(elem_basic_value);
                }

                match view.kind {
                    StaticContainerKind::Array => {
                        data_global.as_pointer_value().as_basic_value_enum()
                    }
                    StaticContainerKind::View => {
                        let view_struct = self
                            .make_view_struct(
                                view.type_id,
                                view.len() as u64,
                                data_global.as_pointer_value(),
                            )
                            .unwrap();

                        let view_ptr = self
                            .builder
                            .build_alloca(view_struct.get_type(), "static_view")
                            .unwrap();

                        // Ok to store an aggregate since it's known to have just the 2 fields
                        self.builder.build_store(view_ptr, view_struct).unwrap();
                        view_ptr.as_basic_value_enum()
                    }
                }
            }
        };
        Ok(result)
    }

    fn make_string_llvm_global(
        &self,
        rust_str: &str,
        name: Option<&str>,
    ) -> CodegenResult<GlobalValue<'ctx>> {
        let global_str_data = self.llvm_module.add_global(
            self.builtin_types.char.array_type(rust_str.len() as u32),
            None,
            "str_data",
        );
        let str_data_array = i8_array_from_str(self.ctx, rust_str);
        global_str_data.set_initializer(&str_data_array);
        global_str_data.set_unnamed_addr(true);
        global_str_data.set_constant(true);

        // Ensure the string layout is what we expect
        // deftype string = { private view: View[char] }
        let string_type = self.codegen_type(STRING_TYPE_ID)?.expect_struct();
        let string_wrapper_struct_type = string_type.struct_type;

        let char_view_struct = (string_type.fields[0].clone()).expect_struct();
        let char_buffer_struct = (char_view_struct.fields[0].clone()).expect_struct();
        let char_buffer_type_id = char_buffer_struct.type_id;
        debug_assert!(
            char_buffer_struct
                .struct_type
                .get_field_type_at_index(0)
                .unwrap()
                .into_int_type()
                .get_bit_width()
                == 64
        );
        debug_assert!(
            char_buffer_struct.struct_type.get_field_type_at_index(1).unwrap().is_pointer_type()
        );
        debug_assert!(char_buffer_struct.struct_type.count_fields() == 2);

        let char_buffer_struct_value = self.make_buffer_struct(
            char_buffer_type_id,
            rust_str.len() as u64,
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

    fn codegen_string_id(&mut self, string_id: StringId) -> CodegenResult<GlobalValue<'ctx>> {
        if let Some(cached_string) = self.strings.get(&string_id) {
            Ok(*cached_string)
        } else {
            let string_value = self.k1.get_string(string_id);
            let ptr = self.make_string_llvm_global(string_value, None)?;

            self.strings.insert(string_id, ptr);
            Ok(ptr)
        }
    }

    fn make_buffer_struct(
        &self,
        buffer_type_id: TypeId,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> CodegenResult<StructValue<'ctx>> {
        let buffer_type = self.codegen_type(buffer_type_id)?.expect_struct();
        let buffer_struct_type = buffer_type.struct_type;
        let buffer_struct_value = buffer_struct_type.const_named_struct(&[
            self.builtin_types.uword().const_int(len, false).as_basic_value_enum(),
            data.as_basic_value_enum(),
        ]);
        Ok(buffer_struct_value)
    }

    fn make_view_struct(
        &self,
        view_type_id: TypeId,
        len: u64,
        data: PointerValue<'ctx>,
    ) -> CodegenResult<StructValue<'ctx>> {
        let view_type = self.codegen_type(view_type_id)?.expect_struct();
        let buffer_type_id = view_type.fields[0].type_id();
        self.make_buffer_struct(buffer_type_id, len, data)
    }

    fn get_insert_function(&self) -> &CodegenedFunction<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let function_id = self.llvm_function_to_k1.get(&function).unwrap();
        let codegened_function = self.llvm_functions.get(function_id).unwrap();
        codegened_function
    }

    fn get_insert_function_mut(&mut self) -> &mut CodegenedFunction<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let function_id = self.llvm_function_to_k1.get(&function).unwrap();
        let codegened_function = self.llvm_functions.get_mut(function_id).unwrap();
        codegened_function
    }

    fn codegen_expr(&mut self, expr_id: TypedExprId) -> CodegenResult<LlvmValue<'ctx>> {
        let expr = self.k1.exprs.get(expr_id);
        let span = expr.get_span();

        // TODO: push debug log level for debugged exprs in codegen as well
        // #[cfg(debug_assertions)]
        // {
        //     let directives = self.module.ast.exprs.get_directives(expr_id);
        //     let debug_directive = directives
        //         .iter()
        //         .find(|p| matches!(p, parse::ParsedDirective::CompilerDebug { .. }));
        //     let is_debug = debug_directive.is_some();
        //     if is_debug {
        //         self.push_debug_level();
        //     }
        // }
        // let mut self_ = scopeguard::guard(self, |s| {
        //     if is_debug {
        //         s.pop_debug_level()
        //     }
        // });

        self.set_debug_location_from_span(span);
        debug!("codegen expr\n{}", self.k1.expr_to_string_with_type(expr_id));
        match expr {
            TypedExpr::Unit(_) => Ok(self.builtin_types.unit_basic().into()),
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
                let string_pointer = self.codegen_string_id(*string_value)?;
                Ok(string_pointer.as_basic_value_enum().into())
            }
            TypedExpr::Variable(ir_var) => {
                if let Some(variable_value) = self.variable_to_value.get(&ir_var.variable_id) {
                    let llvm_type = self.codegen_type(ir_var.type_id)?;
                    debug!(
                        "codegen variable {} got pointee type {:?}",
                        self.k1.type_id_to_string(ir_var.type_id),
                        &llvm_type
                    );
                    Ok(self.load_variable_value(&llvm_type, *variable_value).into())
                } else {
                    Err(CodegenError {
                        message: format!(
                            "No pointer or global found for variable {} id={}",
                            self.k1.expr_to_string(expr_id),
                            ir_var.variable_id
                        ),
                        span,
                    })
                }
            }
            TypedExpr::Struct(struc) => {
                debug!("codegen struct {}", self.k1.expr_to_string_with_type(expr_id));
                let k1_type = self.codegen_type(struc.type_id)?;
                let struct_ptr = self.build_k1_alloca(&k1_type, "struct_literal");
                let struct_k1_llvm_type = k1_type.expect_struct();
                let struct_llvm_struct = struct_k1_llvm_type.struct_type;
                for (idx, field) in struc.fields.iter().enumerate() {
                    let value = self.codegen_expr_basic_value(field.expr)?;
                    let field_addr = self
                        .builder
                        .build_struct_gep(
                            struct_llvm_struct,
                            struct_ptr,
                            idx as u32,
                            &format!("{}_store_addr", self.k1.ident_str(field.name)),
                        )
                        .unwrap();
                    let field_type = &struct_k1_llvm_type.fields[idx];
                    self.store_k1_value(field_type, field_addr, value);
                }
                Ok(struct_ptr.as_basic_value_enum().into())
            }
            TypedExpr::StructFieldAccess(field_access) => {
                let name = if cfg!(debug_assertions) {
                    &format!("struc.{}", self.k1.ast.idents.get_name(field_access.target_field))
                } else {
                    ""
                };
                let field_index = field_access.field_index;
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
                    // We copy the field whether or not the base struct is a reference, because it
                    // could be inside a reference, we can't assume this isn't mutable memory just
                    // because our immediate base struct isn't a reference
                    let field_value = self.load_k1_value(field_type, field_pointer, name, true);
                    Ok(field_value.into())
                }
            }
            TypedExpr::ArrayGetElement(array_get) => {
                // `base` can be a pointer to an array or an array value proper, just like
                // struct access
                let base_value = self.codegen_expr(array_get.base)?;
                let LlvmValue::BasicValue(base_value) = base_value else {
                    // It exits
                    return Ok(base_value);
                };
                let index_value = self.codegen_expr(array_get.index)?;
                let LlvmValue::BasicValue(index_value) = index_value else {
                    // It exits
                    return Ok(index_value);
                };
                let index_value = index_value.into_int_value();

                let array_type = self.codegen_type(array_get.array_type)?.expect_array();

                // This will be an Llvm PointerValue whether its a reference in K1 or not!
                let array_ptr = base_value.into_pointer_value();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(
                            array_type.array_type,
                            array_ptr,
                            &[self.builtin_types.ptr_sized_int.const_zero(), index_value],
                            "",
                        )
                        .unwrap()
                };

                if array_get.is_referencing {
                    Ok(elem_ptr.as_basic_value_enum().into())
                } else {
                    let element_type = &array_type.element_type;
                    // We copy the element whether or not the base array is a reference, because it
                    // could be inside a reference, we can't assume this isn't mutable memory just
                    // because our immediate base array isn't a reference
                    let element_value = self.load_k1_value(element_type, elem_ptr, "", true);
                    Ok(element_value.into())
                }
            }
            TypedExpr::Match(match_expr) => self.codegen_match(match_expr),
            TypedExpr::WhileLoop(while_expr) => self.codegen_while_expr(while_expr),
            TypedExpr::LoopExpr(loop_expr) => self.codegen_loop_expr(loop_expr),
            TypedExpr::Deref(deref) => {
                let value = self.codegen_expr_basic_value(deref.target)?;
                let value_ptr = value.into_pointer_value();
                let pointee_ty = self.codegen_type(deref.type_id)?;
                debug!(
                    "Dereference: type {} w/ llvm value {} as llvm type {}",
                    self.k1.expr_to_string_with_type(deref.target),
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
            TypedExpr::Block(block) => {
                // This is just a lexical scoping block, not a control-flow block, so doesn't need
                // to correspond to an LLVM basic block
                // We just need to codegen each statement and yield the result value
                let block_value = self.codegen_block(block)?;
                Ok(block_value)
            }
            TypedExpr::Call { call_id, .. } => self.codegen_function_call(*call_id),
            TypedExpr::EnumConstructor(enum_constr) => {
                let llvm_type = self.codegen_type(enum_constr.variant_type_id)?;
                let enum_ptr = self.build_k1_alloca(&llvm_type, "enum_constr");

                let enum_type = llvm_type.expect_enum();

                let enum_variant = &enum_type.variants[enum_constr.variant_index as usize];
                let variant_tag_name = enum_variant.name;

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
            TypedExpr::EnumGetTag(enum_get_tag) => {
                let enum_value = self
                    .codegen_expr_basic_value(enum_get_tag.enum_expr_or_reference)?
                    .into_pointer_value();
                let enum_type_id = self.k1.types.get_type_id_dereferenced(
                    self.k1.exprs.get(enum_get_tag.enum_expr_or_reference).get_type(),
                );
                let enum_llvm_type = self.codegen_type(enum_type_id)?.expect_enum();
                let enum_tag_value = self.get_enum_tag(enum_llvm_type.tag_type, enum_value);
                Ok(enum_tag_value.as_basic_value_enum().into())
            }
            TypedExpr::EnumGetPayload(enum_get_payload) => {
                let target_expr_type_id =
                    self.k1.exprs.get(enum_get_payload.enum_variant_expr).get_type();
                let enum_type = self.k1.types.get_type_id_dereferenced(target_expr_type_id);
                let enum_type = self.codegen_type(enum_type)?.expect_enum();
                let enum_value =
                    self.codegen_expr_basic_value(enum_get_payload.enum_variant_expr)?;
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
                let return_result = self.codegen_expr(ret.value)?;
                let LlvmValue::BasicValue(return_value) = return_result else {
                    return Ok(return_result);
                };
                let codegened_function = self.get_insert_function();
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
                    let break_type = self.codegen_type(loop_info.break_type.unwrap())?;
                    self.store_k1_value(&break_type, break_value_ptr, break_value);
                }
                let branch_inst =
                    self.builder.build_unconditional_branch(loop_info.end_block).unwrap();
                Ok(LlvmValue::Void(branch_inst))
            }
            TypedExpr::Lambda(lambda_expr) => {
                debug!("codegen lambda {:?}", lambda_expr);
                let lambda_type = self.k1.types.get(lambda_expr.lambda_type).as_lambda().unwrap();
                let llvm_fn = self.codegen_function_signature(lambda_type.body_function_id)?;
                let environment_struct_value = self
                    .codegen_expr_basic_value(lambda_type.environment_struct)?
                    .into_pointer_value();

                self.lambda_functions.insert(lambda_expr.lambda_type, llvm_fn);

                Ok(environment_struct_value.as_basic_value_enum().into())
            }
            TypedExpr::FunctionPointer(function_pointer_expr) => {
                let function_value =
                    self.codegen_function_signature(function_pointer_expr.function_id)?;
                let function_ptr =
                    function_value.as_global_value().as_pointer_value().as_basic_value_enum();
                self.set_debug_location_from_span(function_pointer_expr.span);
                Ok(function_ptr.as_basic_value_enum().into())
            }
            TypedExpr::FunctionToLambdaObject(fn_to_lam_obj) => {
                let function_value = self.codegen_function_signature(fn_to_lam_obj.function_id)?;
                self.set_debug_location_from_span(fn_to_lam_obj.span);
                let function_ptr =
                    function_value.as_global_value().as_pointer_value().as_basic_value_enum();
                let lam_obj_struct_type = self.codegen_type(fn_to_lam_obj.lambda_object_type_id)?;
                // lam_obj_struct_type.struct_type is equivalent to rich_value_type()
                let lambda_object_ptr = self.build_k1_alloca(&lam_obj_struct_type, "fn2obj");
                let lam_obj_type = lam_obj_struct_type.expect_lambda_object();

                let obj_function_ptr_ptr = self
                    .builder
                    .build_struct_gep(lam_obj_type.struct_type, lambda_object_ptr, 0, "fn_ptr")
                    .unwrap();
                self.builder.build_store(obj_function_ptr_ptr, function_ptr).unwrap();

                let obj_env_ptr_ptr = self
                    .builder
                    .build_struct_gep(lam_obj_type.struct_type, lambda_object_ptr, 1, "nop_env")
                    .unwrap();
                self.builder
                    .build_store(obj_env_ptr_ptr, self.builtin_types.ptr.const_null())
                    .unwrap();

                // This is a STRUCT, in K1 terms, not a struct reference, it's just that the physical
                // representation type for aggregates _is_ ptr in our codegen
                Ok(LlvmValue::BasicValue(lambda_object_ptr.as_basic_value_enum()))
            }
            TypedExpr::StaticValue(value_id, ..) => {
                let static_value = self.codegen_static_value_as_code(*value_id)?;
                Ok(static_value.into())
            }
            TypedExpr::PendingCapture(_) => {
                panic!("Unsupported expression: {}", self.k1.expr_to_string(expr_id))
            }
        }
    }

    fn codegen_cast(&mut self, cast: &TypedCast) -> CodegenResult<LlvmValue<'ctx>> {
        match cast.cast_type {
            CastType::EnumToVariant
            | CastType::VariantToEnum
            | CastType::ReferenceToReference
            | CastType::ReferenceToMut
            | CastType::ReferenceUnMut
            | CastType::IntegerCast(IntegerCastDirection::NoOp)
            | CastType::Integer8ToChar
            | CastType::StaticErase => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                Ok(value.into())
            }
            CastType::IntegerCast(IntegerCastDirection::Extend)
            | CastType::IntegerExtendFromChar => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                let int_value = value.into_int_value();
                let llvm_type = self.codegen_type(cast.target_type_id)?;
                let integer_type = self.k1.types.get(cast.target_type_id).expect_integer();
                let value: IntValue<'ctx> = if integer_type.is_signed() {
                    self.builder
                        .build_int_s_extend(
                            int_value,
                            llvm_type.rich_repr_type().into_int_type(),
                            "extend_cast_sext",
                        )
                        .unwrap()
                } else {
                    self.builder
                        .build_int_z_extend(
                            int_value,
                            llvm_type.rich_repr_type().into_int_type(),
                            "extend_cast_zext",
                        )
                        .unwrap()
                };
                Ok(value.as_basic_value_enum().into())
            }
            CastType::IntegerCast(IntegerCastDirection::Truncate) => {
                let value = self.codegen_expr_basic_value(cast.base_expr)?;
                let int_value = value.into_int_value();
                let int_type = self.codegen_type(cast.target_type_id)?;
                let truncated_value = self
                    .builder
                    .build_int_truncate(
                        int_value,
                        int_type.rich_repr_type().into_int_type(),
                        "trunc_cast",
                    )
                    .unwrap();
                Ok(truncated_value.as_basic_value_enum().into())
            }
            CastType::FloatExtend => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let float_dst_type =
                    self.codegen_type(cast.target_type_id)?.rich_repr_type().into_float_type();
                let extended_value =
                    self.builder.build_float_ext(from_value, float_dst_type, "fext").unwrap();
                Ok(extended_value.as_basic_value_enum().into())
            }
            CastType::FloatTruncate => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let float_dst_type =
                    self.codegen_type(cast.target_type_id)?.rich_repr_type().into_float_type();
                let extended_value =
                    self.builder.build_float_trunc(from_value, float_dst_type, "ftrunc").unwrap();
                Ok(extended_value.as_basic_value_enum().into())
            }
            CastType::FloatToInteger => {
                let from_value = self.codegen_expr_basic_value(cast.base_expr)?.into_float_value();
                let int_dst_type = self.codegen_type(cast.target_type_id)?;
                let int_dst_type_llvm = int_dst_type.rich_repr_type().into_int_type();
                let int_dest_k1_type = self.k1.types.get(cast.target_type_id).expect_integer();
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
                let base_expr_type = self.k1.exprs.get(cast.base_expr).get_type();
                let from_int_k1_type = self.k1.types.get(base_expr_type).expect_integer();
                let float_dst_type = self.codegen_type(cast.target_type_id)?;
                let float_dst_type_llvm = float_dst_type.rich_repr_type().into_float_type();
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
            CastType::PointerToWord => {
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
                let env_pointer = self.build_alloca(lambda_env_value.get_type(), "env_ptr");
                self.builder.build_store(env_pointer, lambda_env_value).unwrap();

                let fn_value =
                    self.lambda_functions.get(&self.k1.get_expr_type_id(cast.base_expr)).unwrap();
                let fn_ptr = fn_value.as_global_value().as_pointer_value();

                let lam_obj = self.builtin_types.dynamic_lambda_object.get_undef();
                let lam_obj = self.builder.build_insert_value(lam_obj, fn_ptr, 0, "").unwrap();
                let lam_obj =
                    self.builder.build_insert_value(lam_obj, lambda_env_value, 1, "").unwrap();

                let lam_obj_ptr =
                    self.build_alloca(self.builtin_types.dynamic_lambda_object, "lam_obj_ptr");
                self.builder.build_store(lam_obj_ptr, lam_obj).unwrap();

                Ok(lam_obj_ptr.as_basic_value_enum().into())
            }
            CastType::Transmute => {
                self.k1.ice_with_span("Cast Transmute unsupported by codegen", cast.span)
            }
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

    fn build_k1_alloca(&mut self, ty: &K1LlvmType<'ctx>, name: &str) -> PointerValue<'ctx> {
        let ptr = self.build_alloca(ty.rich_repr_type(), name);
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

    fn codegen_function_call(&mut self, call_id: CallId) -> CodegenResult<LlvmValue<'ctx>> {
        let call = self.k1.calls.get(call_id);
        let typed_function = call.callee.maybe_function_id().map(|f| self.k1.get_function(f));
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
            trace!("codegen function call arg: {}", self.k1.expr_to_string_with_type(*arg_expr),);
            args.push_back(basic_value.into())
        }
        let function_type = self.k1.get_callee_function_type(&call.callee);
        let llvm_function_type = self.make_llvm_function_type(function_type)?;
        let sret_alloca = if llvm_function_type.is_sret {
            let sret_alloca = self.build_k1_alloca(&llvm_function_type.return_type, "call_sret");
            args.push_front(sret_alloca.into());
            Some(sret_alloca)
        } else {
            None
        };
        let env_arg_index = if llvm_function_type.is_sret { 1 } else { 0 };
        let callsite_value = match &call.callee {
            Callee::Abstract { .. } => {
                self.k1.ice_with_span("Cannot codegen a call to an abstract function", call.span)
            }
            Callee::StaticFunction(function_id) => {
                let function_value = self.codegen_function_signature(*function_id)?;

                self.set_debug_location_from_span(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "").unwrap()
            }
            Callee::StaticLambda { function_id, lambda_value_expr, .. } => {
                let lambda_env_struct = self.codegen_expr(*lambda_value_expr)?;

                // I think we need to turn this into a pointer to the env
                // before calling
                args.insert(env_arg_index, lambda_env_struct.expect_basic_value().into());

                let function_value = self.codegen_function_signature(*function_id)?;

                self.set_debug_location_from_span(call.span);
                self.builder.build_call(function_value, args.make_contiguous(), "").unwrap()
            }
            Callee::DynamicFunction { function_pointer_expr } => {
                let function_ptr =
                    self.codegen_expr_basic_value(*function_pointer_expr)?.into_pointer_value();

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

                debug!(
                    "The k1 fn type on the lambda object {}",
                    self.k1.type_id_to_string(function_type)
                );
                debug!("Calling indirect with type {}", llvm_function_type.llvm_function_type);
                self.builder
                    .build_indirect_call(
                        llvm_function_type.llvm_function_type,
                        fn_ptr,
                        args.make_contiguous(),
                        "",
                    )
                    .unwrap()
            }
            Callee::DynamicAbstract { .. } => {
                return failf!(
                    call.span,
                    "Internal Compiler Error: cannot codegen an Abstract callee"
                );
            }
        };

        if llvm_function_type.is_sret {
            let sret_attribute = self.make_sret_attribute(
                llvm_function_type.return_type.rich_repr_type().as_any_type_enum(),
            );
            callsite_value.add_attribute(AttributeLoc::Param(0), sret_attribute);
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
        let length_value = self.builtin_types.uword().const_int(string.len() as u64, false);
        let char_data_global =
            self.llvm_module.add_global(char_data.get_type(), None, &format!("{name}_data"));
        char_data_global.set_initializer(&char_data);
        let string_type = self.codegen_type(STRING_TYPE_ID).unwrap();
        let string_struct_type = string_type.rich_repr_type().into_struct_type();
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
        intrinsic_type: IntrinsicOperation,
        call: &Call,
    ) -> CodegenResult<LlvmValue<'ctx>> {
        match intrinsic_type {
            IntrinsicOperation::SizeOf
            | IntrinsicOperation::SizeOfStride
            | IntrinsicOperation::AlignOf => {
                unreachable!("Handled by typer phase {:?}", intrinsic_type)
            }
            IntrinsicOperation::Zeroed => {
                let type_param = self.k1.named_types.get_nth(call.type_args, 0);
                let k1_type = self.codegen_type(type_param.type_id)?;
                if k1_type.is_aggregate() {
                    let ptr = self.build_k1_alloca(&k1_type, "zeroed");
                    self.builder
                        .build_memset(
                            ptr,
                            k1_type.rich_repr_layout().align,
                            self.ctx.i8_type().const_zero(),
                            self.ctx
                                .i32_type()
                                .const_int(k1_type.rich_repr_layout().size as u64, false),
                        )
                        .unwrap();
                    Ok(ptr.as_basic_value_enum().into())
                } else {
                    let zero_value = k1_type.rich_repr_type().const_zero();
                    Ok(zero_value.into())
                }
            }
            IntrinsicOperation::BoolNegate => {
                let input_value = return_void!(self.codegen_expr(call.args[0])?).into_int_value();
                let truncated = self.bool_to_i1(input_value, "");
                let negated = self.builder.build_not(truncated, "").unwrap();
                let promoted = self.i1_to_bool(negated, "");
                Ok(promoted.as_basic_value_enum().into())
            }
            IntrinsicOperation::BitNot => {
                let input_value = return_void!(self.codegen_expr(call.args[0])?);
                let input_value_int = input_value.into_int_value();
                let not_value = self.builder.build_not(input_value_int, "not").unwrap();
                Ok(not_value.as_basic_value_enum().into())
            }
            IntrinsicOperation::ArithBinop(op_kind) => {
                let lhs = return_void!(self.codegen_expr(call.args[0])?);
                let rhs = return_void!(self.codegen_expr(call.args[1])?);
                use IntrinsicArithOpOp as Op;
                match op_kind.class {
                    IntrinsicArithOpClass::SignedInt | IntrinsicArithOpClass::UnsignedInt => {
                        let is_signed = op_kind.class.is_signed_int();
                        let lhs_int = lhs.into_int_value();
                        let rhs_int = rhs.into_int_value();
                        match op_kind.op {
                            Op::Equals => {
                                let cmp_result = self
                                    .builder
                                    .build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "")
                                    .unwrap();
                                Ok(self.i1_to_bool(cmp_result, "").as_basic_value_enum().into())
                            }
                            Op::Add => {
                                let add_result =
                                    self.builder.build_int_add(lhs_int, rhs_int, "").unwrap();
                                Ok(add_result.as_basic_value_enum().into())
                            }
                            Op::Sub => {
                                let sub_result =
                                    self.builder.build_int_sub(lhs_int, rhs_int, "").unwrap();
                                Ok(sub_result.as_basic_value_enum().into())
                            }
                            Op::Mul => {
                                let mul_result =
                                    self.builder.build_int_mul(lhs_int, rhs_int, "").unwrap();
                                Ok(mul_result.as_basic_value_enum().into())
                            }
                            Op::Div => {
                                let div_result = if is_signed {
                                    self.builder.build_int_signed_div(lhs_int, rhs_int, "").unwrap()
                                } else {
                                    self.builder
                                        .build_int_unsigned_div(lhs_int, rhs_int, "")
                                        .unwrap()
                                };
                                Ok(div_result.as_basic_value_enum().into())
                            }
                            Op::Rem => {
                                let rem_result = if is_signed {
                                    self.builder.build_int_signed_rem(lhs_int, rhs_int, "").unwrap()
                                } else {
                                    self.builder
                                        .build_int_unsigned_rem(lhs_int, rhs_int, "")
                                        .unwrap()
                                };
                                Ok(rem_result.as_basic_value_enum().into())
                            }
                            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                                let pred = match (is_signed, op_kind.op) {
                                    (true, Op::Lt) => IntPredicate::SLT,
                                    (true, Op::Le) => IntPredicate::SLE,
                                    (true, Op::Gt) => IntPredicate::SGT,
                                    (true, Op::Ge) => IntPredicate::SGE,
                                    (false, Op::Lt) => IntPredicate::ULT,
                                    (false, Op::Le) => IntPredicate::ULE,
                                    (false, Op::Gt) => IntPredicate::UGT,
                                    (false, Op::Ge) => IntPredicate::UGE,
                                    _ => unreachable!("unexpected binop kind"),
                                };
                                let i1_compare = self
                                    .builder
                                    .build_int_compare(pred, lhs_int, rhs_int, "")
                                    .unwrap();
                                Ok(self.i1_to_bool(i1_compare, "").as_basic_value_enum().into())
                            }
                        }
                    }
                    IntrinsicArithOpClass::Float => {
                        let lhs_f = lhs.into_float_value();
                        let rhs_f = rhs.into_float_value();
                        match op_kind.op {
                            Op::Equals => {
                                let cmp_result = self
                                    .builder
                                    .build_float_compare(FloatPredicate::OEQ, lhs_f, rhs_f, "")
                                    .unwrap();
                                Ok(self.i1_to_bool(cmp_result, "").as_basic_value_enum().into())
                            }
                            Op::Add => {
                                let add_result =
                                    self.builder.build_float_add(lhs_f, rhs_f, "").unwrap();
                                Ok(add_result.as_basic_value_enum().into())
                            }
                            Op::Sub => {
                                let result =
                                    self.builder.build_float_sub(lhs_f, rhs_f, "").unwrap();
                                Ok(result.as_basic_value_enum().into())
                            }
                            Op::Mul => {
                                let result =
                                    self.builder.build_float_mul(lhs_f, rhs_f, "").unwrap();
                                Ok(result.as_basic_value_enum().into())
                            }
                            Op::Div => {
                                let result =
                                    self.builder.build_float_div(lhs_f, rhs_f, "").unwrap();
                                Ok(result.as_basic_value_enum().into())
                            }
                            Op::Rem => {
                                let result =
                                    self.builder.build_float_rem(lhs_f, rhs_f, "").unwrap();
                                Ok(result.as_basic_value_enum().into())
                            }
                            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                                let pred = match op_kind.op {
                                    Op::Lt => FloatPredicate::OLT,
                                    Op::Le => FloatPredicate::OLE,
                                    Op::Gt => FloatPredicate::OGT,
                                    Op::Ge => FloatPredicate::OGE,
                                    _ => unreachable!("unexpected binop kind"),
                                };
                                let i1_compare = self
                                    .builder
                                    .build_float_compare(pred, lhs_f, rhs_f, "")
                                    .unwrap();
                                Ok(self.i1_to_bool(i1_compare, "").as_basic_value_enum().into())
                            }
                        }
                    }
                }
            }
            IntrinsicOperation::BitwiseBinop(op_kind) => {
                let lhs = return_void!(self.codegen_expr(call.args[0])?).into_int_value();
                let rhs = return_void!(self.codegen_expr(call.args[1])?).into_int_value();
                let result = match op_kind {
                    IntrinsicBitwiseBinopKind::And => self.builder.build_and(lhs, rhs, ""),
                    IntrinsicBitwiseBinopKind::Xor => self.builder.build_xor(lhs, rhs, ""),
                    IntrinsicBitwiseBinopKind::Or => self.builder.build_or(lhs, rhs, ""),
                    IntrinsicBitwiseBinopKind::ShiftLeft => {
                        self.builder.build_left_shift(lhs, rhs, "")
                    }
                    IntrinsicBitwiseBinopKind::SignedShiftRight => {
                        self.builder.build_right_shift(lhs, rhs, true, "")
                    }
                    IntrinsicBitwiseBinopKind::UnsignedShiftRight => {
                        self.builder.build_right_shift(lhs, rhs, false, "")
                    }
                };
                let result = result.to_err(call.span)?;
                Ok(result.as_basic_value_enum().into())
            }
            IntrinsicOperation::LogicalAnd => {
                let lhs = return_void!(self.codegen_expr(call.args[0])?).into_int_value();

                let lhs_i1 = self.bool_to_i1(lhs, "");
                let short_circuit_branch =
                    self.build_conditional_branch(lhs_i1, "rhs_check", "short_circuit");
                let phi_destination = self.append_basic_block("and_result");

                // label: rhs_check; lhs was true
                self.builder.position_at_end(short_circuit_branch.then_block);
                let rhs = return_void!(self.codegen_expr(call.args[1])?).into_int_value();

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
                let result =
                    self.builder.build_phi(self.builtin_types.boolean, "bool_and").unwrap();
                result.add_incoming(&[
                    (&rhs, rhs_incoming),
                    (&self.builtin_types.false_value, short_circuit_branch.else_block),
                ]);
                Ok(result.as_basic_value().into())
            }
            IntrinsicOperation::LogicalOr => {
                let lhs = return_void!(self.codegen_expr(call.args[0])?).into_int_value();

                let lhs_i1 = self.bool_to_i1(lhs, "");
                let start_block = self.builder.get_insert_block().unwrap();
                let short_circuit_branch =
                    self.build_conditional_branch(lhs_i1, "or_result", "rhs_check");
                let block_result = short_circuit_branch.then_block;
                let block_rhs_check = short_circuit_branch.else_block;

                self.builder.position_at_end(block_rhs_check);
                let rhs = return_void!(self.codegen_expr(call.args[1])?).into_int_value();
                self.builder.build_unconditional_branch(block_result).unwrap();

                self.builder.position_at_end(block_result);
                let phi = self.builder.build_phi(self.builtin_types.boolean, "").unwrap();
                phi.add_incoming(&[
                    (&self.builtin_types.true_value, start_block),
                    (&rhs.as_basic_value_enum(), block_rhs_check),
                ]);

                Ok(phi.as_basic_value().into())
            }
            IntrinsicOperation::TypeId => {
                unreachable!("TypeId is handled in typer phase")
            }
            IntrinsicOperation::PointerIndex => {
                //  Reference:
                //  intern fn refAtIndex[T](self: Pointer, index: uword): T*
                let pointee_ty_arg = self.k1.named_types.get_nth(call.type_args, 0);
                let elem_type = self.codegen_type(pointee_ty_arg.type_id)?;
                let ptr = return_void!(self.codegen_expr(call.args[0])?).into_pointer_value();
                let index = return_void!(self.codegen_expr(call.args[1])?).into_int_value();
                let result_pointer = unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            elem_type.rich_repr_type(),
                            ptr,
                            &[index],
                            "refAtIndex",
                        )
                        .unwrap()
                };
                Ok(result_pointer.as_basic_value_enum().into())
            }
            IntrinsicOperation::CompilerMessage => Ok(self.builtin_types.unit_basic().into()),
            IntrinsicOperation::CompilerSourceLocation => {
                unreachable!("CompilerSourceLocation is handled in typechecking phase")
            }
            _ => panic!("Unexpected inline intrinsic {:?}", intrinsic_type),
        }
    }

    fn load_function_argument(
        &mut self,
        function: &TypedFunction,
        index: usize,
    ) -> CodegenResult<BasicMetadataValueEnum<'ctx>> {
        let variable_id = function.param_variables[index];
        let fn_type = self.k1.types.get(function.type_id).expect_function();
        let param_type = &fn_type.physical_params[index];
        let variable_value = self.variable_to_value.get(&variable_id).unwrap();
        let basic_value =
            self.load_variable_value(&self.codegen_type(param_type.type_id)?, *variable_value);
        Ok(basic_value.into())
    }

    fn codegen_intrinsic_function_body(
        &mut self,
        intrinsic_type: IntrinsicOperation,
        function_id: FunctionId,
        function: &TypedFunction,
    ) -> CodegenResult<InstructionValue<'ctx>> {
        let function_span =
            self.k1.ast.get_function(function.parsed_id.as_function_id().unwrap()).signature_span;
        self.set_debug_location_from_span(function_span);
        let instr = match intrinsic_type {
            IntrinsicOperation::Allocate => {
                // intern fn alloc(size: uword, align: uword): Pointer
                let size_arg = self.load_function_argument(function, 0)?;

                let f = self.llvm_module.get_function("malloc").unwrap();
                let call = self.builder.build_call(f, &[size_arg], "").unwrap();
                let result = call.try_as_basic_value().unwrap_left();
                self.builder.build_return(Some(&result)).unwrap()
            }
            IntrinsicOperation::AllocateZeroed => {
                // intern fn allocZeroed(size: uword, align: uword): Pointer
                let size_arg = self.load_function_argument(function, 0)?;
                let count_one =
                    self.builtin_types.ptr_sized_int.const_int(1, false).as_basic_value_enum();

                let f = self.llvm_module.get_function("calloc").unwrap();
                // libc/calloc(count = 1, size = size);
                let call = self.builder.build_call(f, &[count_one.into(), size_arg], "").unwrap();
                let result = call.try_as_basic_value().unwrap_left();
                self.builder.build_return(Some(&result)).unwrap()
            }
            IntrinsicOperation::Reallocate => {
                // intern fn realloc(ptr: Pointer, oldSize: uword, align: uword, newSize: uword): Pointer
                let old_ptr_arg = self.load_function_argument(function, 0)?;
                let new_size_arg = self.load_function_argument(function, 3)?;

                let f = self.llvm_module.get_function("realloc").unwrap();
                let call = self.builder.build_call(f, &[old_ptr_arg, new_size_arg], "").unwrap();
                let result = call.try_as_basic_value().unwrap_left();
                self.builder.build_return(Some(&result)).unwrap()
            }
            IntrinsicOperation::Free => {
                let old_ptr_arg = self.load_function_argument(function, 0)?;

                let f = self.llvm_module.get_function("free").unwrap();
                let call = self.builder.build_call(f, &[old_ptr_arg], "").unwrap();
                let result = call.try_as_basic_value().unwrap_left();
                self.builder.build_return(Some(&result)).unwrap()
            }
            IntrinsicOperation::MemCopy => {
                // intern fn copy(
                //   dst: Pointer,
                //   src: Pointer,
                //   count: uword
                // ): unit
                let dst_ptr_arg = self.load_function_argument(function, 0)?.into_pointer_value();
                let src_ptr_arg = self.load_function_argument(function, 1)?.into_pointer_value();
                let size_arg = self.load_function_argument(function, 2)?.into_int_value();
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
            IntrinsicOperation::MemSet => {
                // intern fn set(
                //   dst: Pointer,
                //   value: u8,
                //   count: uword
                // ): unit
                todo!("vm memset")
            }
            IntrinsicOperation::MemEquals => {
                // intern fn equals(p1: Pointer, p2: Pointer, size: uword): bool
                let p1_arg = self.load_function_argument(function, 0)?;
                let p2_arg = self.load_function_argument(function, 1)?;
                let size_arg = self.load_function_argument(function, 2)?;

                let f = self.llvm_module.get_function("memcmp").unwrap();
                let call = self.builder.build_call(f, &[p1_arg, p2_arg, size_arg], "").unwrap();
                let result = call.try_as_basic_value().unwrap_left().into_int_value();
                let is_zero = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, result, result.get_type().const_zero(), "")
                    .unwrap();
                let bool_equal = self.i1_to_bool(is_zero, "");
                self.builder.build_return(Some(&bool_equal)).unwrap()
            }
            IntrinsicOperation::Exit => {
                // intern fn exit(code: i32): never
                let code_arg = self.load_function_argument(function, 0)?;

                let f = self.llvm_module.get_function("exit").unwrap();
                let call = self.builder.build_call(f, &[code_arg], "").unwrap();
                let _result = call.try_as_basic_value().unwrap_right();
                self.builder.build_unreachable().unwrap()
            }

            IntrinsicOperation::TypeSchema | IntrinsicOperation::TypeName => {
                // intern fn typeSchema(id: u64): TypeSchema
                let type_id_arg = self.load_function_argument(function, 0)?.into_int_value();
                let is_type_name = intrinsic_type == IntrinsicOperation::TypeName;
                let return_llvm_type = if is_type_name {
                    // typeName returns string
                    self.codegen_type(STRING_TYPE_ID)?
                } else {
                    // typeSchema returns TypeSchema
                    self.codegen_type(self.k1.types.builtins.types_type_schema.unwrap())?
                };
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
                // TODO: sort the schemas so codegen more predictably
                if is_type_name {
                    for (type_id, string_id) in self
                        .k1
                        .type_names
                        .iter()
                        .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
                    {
                        if self.k1.types.get_contained_type_variable_counts(*type_id).is_abstract()
                        {
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
                            let global_value = self.codegen_string_id(*string_id)?;
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
                        .sorted_unstable_by_key(|(type_id, _)| type_id.as_u32())
                    {
                        if self.k1.types.get_contained_type_variable_counts(*type_id).is_abstract()
                        {
                            // No point re-ifying types that don't exist at runtime
                            // like type parameters
                            continue;
                        }
                        let my_block =
                            self.append_basic_block(&format!("arm_type_{}", type_id.as_u32()));
                        self.builder.position_at_end(my_block);
                        let type_id_int_value =
                            self.ctx.i64_type().const_int(type_id.as_u32() as u64, false);

                        let value = self.codegen_static_value_as_code(*schema_value_id)?;
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
            _ => unreachable!("Unexpected non-inline intrinsic function"),
        };
        Ok(instr)
    }

    fn codegen_block(&mut self, block: &TypedBlock) -> CodegenResult<LlvmValue<'ctx>> {
        let unit_value = self.builtin_types.unit_basic().into();
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
        match self.k1.stmts.get(statement) {
            TypedStmt::Expr(expr, _) => self.codegen_expr(*expr),
            TypedStmt::Let(let_stmt) => self.codegen_let(let_stmt),
            TypedStmt::Assignment(assignment) => {
                let rhs = return_void!(self.codegen_expr(assignment.value)?);
                let lhs_pointer = match assignment.kind {
                    AssignmentKind::Set => {
                        let TypedExpr::Variable(v) = self.k1.exprs.get(assignment.destination)
                        else {
                            self.k1.ice_with_span("Invalid value assignment lhs", assignment.span)
                        };
                        let VariableValue::Indirect { pointer_value, .. } =
                            *self.variable_to_value.get(&v.variable_id).expect("Missing variable")
                        else {
                            self.k1.ice_with_span(
                                "Expect an indirect variable for value assignment",
                                v.span,
                            )
                        };
                        pointer_value
                    }
                    AssignmentKind::Store => {
                        let dest = return_void!(self.codegen_expr(assignment.destination)?);
                        dest.into_pointer_value()
                    }
                };

                let value_type =
                    self.codegen_type(self.k1.exprs.get(assignment.value).get_type())?;
                self.store_k1_value(&value_type, lhs_pointer, rhs);
                Ok(self.builtin_types.unit_basic().into())
            }
            TypedStmt::Require(require_stmt) => {
                let start_block = self.builder.get_insert_block().unwrap();
                let require_continue_block = self.append_basic_block("require_continue");
                let require_else_block = self.append_basic_block("require_else");

                self.codegen_matching_condition(
                    &require_stmt.condition,
                    require_continue_block,
                    require_else_block,
                )?;

                self.builder.position_at_end(require_else_block);
                self.codegen_expr(require_stmt.else_body)?;

                if require_stmt.condition.diverges {
                    require_continue_block.remove_from_function().unwrap();
                    self.builder.position_at_end(start_block);
                } else {
                    self.builder.position_at_end(require_continue_block);
                }

                Ok(self.builtin_types.unit_basic().into())
            }
            TypedStmt::Defer(_defer) => Ok(self.builtin_types.unit_basic().into()),
        }
    }

    fn codegen_matching_condition(
        &mut self,
        condition: &MatchingCondition,
        cons_block: BasicBlock<'ctx>,
        else_block: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        for stmt in &condition.instrs {
            match stmt {
                MatchingConditionInstr::Binding { let_stmt, .. } => {
                    self.codegen_statement(*let_stmt)?;
                }
                MatchingConditionInstr::Cond { value } => {
                    let condition = self.codegen_expr(*value)?;
                    let LlvmValue::BasicValue(bool_value) = condition else {
                        return Ok(());
                    };
                    let i1 = self.bool_to_i1(bool_value.into_int_value(), "");

                    let continue_block = self.append_basic_block("");
                    self.builder.build_conditional_branch(i1, continue_block, else_block).unwrap();

                    self.builder.position_at_end(continue_block);
                }
            }
        }

        // If no conditions fail, we can go to the consequent of this matching condition
        self.builder.build_unconditional_branch(cons_block).unwrap();
        Ok(())
    }

    fn codegen_while_expr(&mut self, while_loop: &WhileLoop) -> CodegenResult<LlvmValue<'ctx>> {
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let loop_entry_block = self.ctx.append_basic_block(current_fn, "while_cond");
        let loop_body_block = self.ctx.append_basic_block(current_fn, "while_body");
        let loop_end_block = self.ctx.append_basic_block(current_fn, "while_end");

        let TypedExpr::Block(body_block) = self.k1.exprs.get(while_loop.body) else {
            unreachable!()
        };
        self.loops.insert(
            body_block.scope_id,
            LoopInfo { break_value_ptr: None, break_type: None, end_block: loop_end_block },
        );

        self.builder.build_unconditional_branch(loop_entry_block).unwrap();

        self.builder.position_at_end(loop_entry_block);
        self.codegen_matching_condition(&while_loop.condition, loop_body_block, loop_end_block)?;

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(body_block)?;
        match body_value.as_basic_value() {
            Either::Left(_instr) => {}
            Either::Right(_bv) => {
                self.builder.build_unconditional_branch(loop_entry_block).unwrap();
            }
        }

        self.builder.position_at_end(loop_end_block);
        Ok(self.builtin_types.unit_basic().into())
    }

    fn codegen_loop_expr(&mut self, loop_expr: &LoopExpr) -> CodegenResult<LlvmValue<'ctx>> {
        let start_block = self.builder.get_insert_block().unwrap();
        let current_fn = start_block.get_parent().unwrap();
        let loop_body_block = self.ctx.append_basic_block(current_fn, "loop_body");
        let loop_end_block = self.ctx.append_basic_block(current_fn, "loop_end");

        let break_type = self.codegen_type(loop_expr.break_type)?;
        // TODO llvm ir Optimization: skip alloca if break is unit type
        let break_value_ptr = self.build_k1_alloca(&break_type, "break");
        let TypedExpr::Block(body_block) = self.k1.exprs.get(loop_expr.body_block) else {
            unreachable!()
        };
        self.loops.insert(
            body_block.scope_id,
            LoopInfo {
                break_value_ptr: Some(break_value_ptr),
                break_type: Some(loop_expr.break_type),
                end_block: loop_end_block,
            },
        );

        // Go to the body
        self.builder.build_unconditional_branch(loop_body_block).unwrap();

        self.builder.position_at_end(loop_body_block);
        let body_value = self.codegen_block(body_block)?;
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

    fn codegen_function_signature(
        &mut self,
        function_id: FunctionId,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(function) = self.llvm_functions.get(&function_id) {
            return Ok(function.function_value);
        }
        debug!("codegen function signature\n{}", self.k1.function_id_to_string(function_id, false));

        let typed_function = self.k1.get_function(function_id);
        let function_type_id = typed_function.type_id;
        let function_type = self.k1.types.get(typed_function.type_id).as_function().unwrap();

        let mut param_types: Vec<K1LlvmType<'ctx>> =
            Vec::with_capacity(function_type.physical_params.len());
        let mut param_metadata_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            Vec::with_capacity(function_type.physical_params.len());

        let llvm_linkage = match typed_function.linkage {
            TyperLinkage::Standard => None,
            TyperLinkage::External { .. } => Some(LlvmLinkage::External),
            TyperLinkage::Intrinsic => None,
        };
        let llvm_name = match typed_function.linkage {
            TyperLinkage::External { link_name: Some(link_name), .. } => {
                self.k1.ident_str(link_name)
            }
            _ => &self.k1.make_qualified_name(typed_function.scope, typed_function.name, ".", true),
        };

        if self.llvm_module.get_function(llvm_name).is_some() {
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

        let llvm_function_type = self.make_llvm_function_type(function_type_id)?;
        let is_sret = llvm_function_type.is_sret;

        let sret_attribute = if is_sret {
            let struct_type = llvm_function_type.return_type.rich_repr_type();
            let sret_attribute = self.make_sret_attribute(struct_type.as_any_type_enum());
            let align_attribute = self.make_align_attribute(
                llvm_function_type.return_type.rich_repr_layout().align as u64,
            );
            Some((sret_attribute, align_attribute))
        } else {
            None
        };

        for param in function_type.physical_params.iter() {
            let param_type = self.codegen_type(param.type_id)?;

            param_metadata_types
                .push(BasicMetadataTypeEnum::from(param_type.canonical_repr_type()));
            param_types.push(param_type);
        }

        // TODO: Mark all Standard functions dso_local
        let function_value = self.llvm_module.add_function(
            llvm_name,
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

        let compile_body = !typed_function.linkage.is_external();
        if compile_body {
            self.functions_pending_body_compilation.push(function_id);
        }
        self.llvm_functions.insert(
            function_id,
            CodegenedFunction {
                function_type: llvm_function_type,
                function_value,
                sret_pointer,
                last_alloca_instr: None,
                instruction_count: 0,
            },
        );
        self.llvm_function_to_k1.insert(function_value, function_id);

        Ok(function_value)
    }

    fn codegen_function_body(&mut self, function_id: FunctionId) -> CodegenResult<()> {
        debug!("codegen function body\n{}", self.k1.function_id_to_string(function_id, true));
        let typed_function = self.k1.get_function(function_id);
        let function_type = self.k1.types.get(typed_function.type_id).as_function().unwrap();

        let function_span = self.k1.ast.get_span_for_id(typed_function.parsed_id);
        let function_line_number = self
            .k1
            .ast
            .get_lines_for_span_id(function_span)
            .expect("line for function span")
            .0
            .line_number();

        let codegened_function = self.llvm_functions.get(&function_id).unwrap();
        let llvm_function_type = &codegened_function.function_type;
        let is_sret = llvm_function_type.is_sret;
        let function_value = codegened_function.function_value;

        let (di_subprogram, di_file) = self.make_function_debug_info(
            function_value.get_name().to_str().unwrap(),
            function_span,
            llvm_function_type.return_type.debug_type(),
            &llvm_function_type.param_types.iter().map(|t| t.debug_type()).collect::<Vec<_>>(),
        )?;

        self.debug.push_scope(function_span, di_subprogram.as_debug_info_scope(), di_file);

        let entry_block = self.ctx.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);
        for (i, param) in function_value.get_param_iter().enumerate() {
            let sret_offset = if is_sret { 1 } else { 0 };
            let is_sret_param = i == 0 && is_sret;
            if is_sret_param {
                param.set_name("sret_ptr");
                continue;
            }
            let variable_id = typed_function.param_variables[i - sret_offset];
            let typed_param = if is_sret_param {
                &FnParamType {
                    name: self.k1.ast.idents.get("ret").unwrap(),
                    type_id: function_type.return_type,
                    is_context: false,
                    is_lambda_env: false,
                    span: self.k1.ast.get_span_for_id(typed_function.parsed_id),
                }
            } else {
                &function_type.physical_params[i - sret_offset]
            };
            let param_type = self.codegen_type(typed_param.type_id)?;
            let param_name = self.k1.ident_str(typed_param.name);
            trace!(
                "Got LLVM type for variable {}: {} (from {})",
                param_name,
                param_type.rich_repr_type(),
                self.k1.type_id_to_string(typed_param.type_id)
            );
            param.set_name(param_name);

            self.set_debug_location_from_span(typed_param.span);
            let di_local_variable = self.debug.debug_builder.create_parameter_variable(
                self.debug.current_scope(),
                param_name,
                i as u32,
                self.debug.current_file(),
                function_line_number,
                param_type.debug_type(),
                true,
                0,
            );
            self.debug.insert_dbg_value_at_end(
                param,
                di_local_variable,
                None,
                self.get_debug_location(),
                entry_block,
            );
            debug!(
                "Inserting variable {i} for function {} id={} {} id={}",
                self.k1.ident_str(typed_function.name),
                function_id,
                param_name,
                variable_id
            );
            self.variable_to_value.insert(variable_id, VariableValue::Direct { value: param });
        }
        match typed_function.intrinsic_type {
            Some(intrinsic_type) => {
                trace!("codegen intrinsic {:?} fn {:?}", intrinsic_type, typed_function);
                let _terminator_instr = self.codegen_intrinsic_function_body(
                    intrinsic_type,
                    function_id,
                    typed_function,
                )?;
            }
            None => {
                let function_block = typed_function.body_block.unwrap_or_else(|| {
                    panic!("Function has no block {}", self.get_ident_name(typed_function.name))
                });
                let TypedExpr::Block(function_block) = self.k1.exprs.get(function_block) else {
                    panic!("Expected block")
                };
                self.codegen_block(function_block)?;
            }
        };
        self.debug.pop_scope();
        function_value.set_subprogram(di_subprogram);

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

    fn codegen_integer_value(&self, integer: TypedIntValue) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(integer.get_type())?;
        let llvm_int_ty = llvm_ty.rich_repr_type().into_int_type();
        let Type::Integer(int_type) = self.k1.types.get(llvm_ty.type_id()) else { panic!() };
        let llvm_value = if int_type.is_signed() {
            llvm_int_ty.const_int(integer.to_u64_unconditional(), true)
        } else {
            llvm_int_ty.const_int(integer.to_u64_unconditional(), false)
        };
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_float_value(&self, float: TypedFloatValue) -> CodegenResult<BasicValueEnum<'ctx>> {
        let llvm_ty = self.codegen_type(float.get_type())?;
        let llvm_float_ty = llvm_ty.rich_repr_type().into_float_type();
        let llvm_value = llvm_float_ty.const_float(float.as_f64());
        Ok(llvm_value.as_basic_value_enum())
    }

    fn codegen_global(&mut self, global_id: TypedGlobalId) -> CodegenResult<()> {
        let global = self.k1.globals.get(global_id);
        let initial_static_value_id = global.initial_value.unwrap();
        let variable = self.k1.variables.get(global.variable_id);
        let name = self.k1.make_qualified_name(variable.owner_scope, variable.name, "__", false);
        let maybe_reference_type = self.k1.types.get(global.ty).as_reference();
        let is_reference_type = maybe_reference_type.is_some();

        if is_llvm_const_representable(&self.k1.static_values, initial_static_value_id) {
            // eprintln!("{name} is const representable, is_reference_type={is_reference_type}");
            let initialized_basic_value =
                self.codegen_static_value_as_const(initial_static_value_id)?;
            let llvm_global = self.llvm_module.add_global(
                initialized_basic_value.get_type(),
                Some(AddressSpace::default()),
                &name,
            );
            llvm_global.set_constant(global.is_constant);
            llvm_global.set_initializer(&initialized_basic_value);
            llvm_global.set_unnamed_addr(true);
            if self.k1.program_settings.multithreaded {
                if global.is_tls {
                    llvm_global.set_thread_local(true);
                    let mode = if self.k1.program_settings.executable {
                        ThreadLocalMode::InitialExecTLSModel
                    } else {
                        // We don't yet support dynamic library as a target
                        // So all libraries can use InitialExec
                        ThreadLocalMode::InitialExecTLSModel
                    };
                    llvm_global.set_thread_local_mode(Some(mode));
                }
            }
            let variable_value = if is_reference_type {
                // Direct; global is a ptr, which is the correct type
                // This will not be 'loaded' by load_variable_value
                VariableValue::Direct { value: llvm_global.as_basic_value_enum() }
            } else {
                // Indirect; global is a ptr to the value of correct type
                // This will be 'loaded' by load_variable_value
                VariableValue::Indirect { pointer_value: llvm_global.as_pointer_value() }
            };
            self.variable_to_value.insert(global.variable_id, variable_value);
        } else {
            if !global.is_constant {
                return failf!(
                    global.span,
                    "Value is too complex to use as initializer for a mutable global, because I'll never run any code at startup behind your back: {}",
                    self.k1.static_value_to_string(initial_static_value_id)
                );
            }
            // For complex static values, we create a function that returns the value
            // And we call it wherever the static is used, probably with inlinealways
            let global_type = self.codegen_type(global.ty)?;
            let value_k1_llvm_type = match maybe_reference_type {
                None => &global_type,
                Some(_) => &global_type.expect_reference().pointee_type,
            };
            let fn_type = self.ctx.void_type().fn_type(&[self.builtin_types.ptr.into()], false);
            let function = self.llvm_module.add_function(&name, fn_type, None);
            let sret_param = function.get_first_param().unwrap();
            sret_param.set_name("sret_param");
            let sret_ptr = sret_param.into_pointer_value();

            let sret_attribute =
                self.make_sret_attribute(value_k1_llvm_type.rich_repr_type().as_any_type_enum());
            let align_attribute =
                self.make_align_attribute(value_k1_llvm_type.rich_repr_layout().align as u64);
            // Function-side sret
            function.add_attribute(AttributeLoc::Param(0), sret_attribute);
            function.add_attribute(AttributeLoc::Param(0), align_attribute);

            let block = self.ctx.append_basic_block(function, "");
            self.builder.position_at_end(block);

            // This recursively builds up the value
            let final_value = self.codegen_static_value_as_code(initial_static_value_id)?;

            self.store_k1_value(value_k1_llvm_type, sret_ptr, final_value);
            self.builder.build_return(None).unwrap();

            self.variable_to_value
                .insert(global.variable_id, VariableValue::ByFunctionCall { function });
            debug!("I built a static value maker function:\n{}", function);
        }
        Ok(())
    }

    pub fn codegen_program(&mut self) -> CodegenResult<()> {
        let start = std::time::Instant::now();
        let global_ids: Vec<TypedGlobalId> = self.k1.globals.iter_ids().collect();
        for global_id in &global_ids {
            self.codegen_global(*global_id)?;
        }

        // TODO: Codegen the exported functions as well as the called ones
        // for (id, function) in self.module.function_iter() {
        //     if function.linkage.is_exported() {
        //         self.codegen_function_signature(id)?;
        //     }
        // }

        // Hack to guarantee presence of required extern declarations
        for (id, function) in self.k1.function_iter() {
            if let TyperLinkage::External { link_name: Some(link_name) } = function.linkage {
                match self.k1.ident_str(link_name) {
                    "malloc" | "calloc" | "realloc" | "free" | "memcmp" | "exit" => {
                        self.codegen_function_signature(id)?;
                    }
                    _ => {}
                };
            }
        }

        let Some(main_function_id) = self.k1.get_main_function_id() else {
            return failf!(SpanId::NONE, "Program {} has no main function", self.k1.program_name());
        };
        let function_value = self.codegen_function_signature(main_function_id)?;

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
            .unwrap_left();
        self.builder.build_return(Some(&res)).unwrap();

        info!("codegen phase 'ir' took {}ms", start.elapsed().as_millis());
        Ok(())
    }

    pub fn name(&self) -> &str {
        self.k1.program_name()
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

        // if optimize {
        //     self.llvm_module
        //         .run_passes("default<O3>", &self.llvm_machine, PassBuilderOptions::create())
        //         .unwrap();
        // } else {
        //     self.llvm_module
        //         .run_passes("function(mem2reg)", &self.llvm_machine, PassBuilderOptions::create())
        //         .unwrap();
        // }

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

    pub fn word_size(&self) -> WordSize {
        self.k1.ast.config.target.word_size()
    }

    fn make_sret_attribute(&self, typ: AnyTypeEnum<'ctx>) -> Attribute {
        self.ctx.create_type_attribute(Attribute::get_named_enum_kind_id("sret"), typ)
    }
    fn make_align_attribute(&self, align: u64) -> Attribute {
        self.ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("align"), align)
    }
}

/// If true, we codegen an LLVM global or a const struct for this value
/// If false, we represent it as a call to a function that builds up
/// the struct or enum or buffer using 'runtime' code, albeit entirely
/// compile-time known, so it gets massively optimized
fn is_llvm_const_representable(static_values: &StaticValuePool, id: StaticValueId) -> bool {
    match static_values.get(id) {
        StaticValue::Unit => true,
        StaticValue::Bool(_) => true,
        StaticValue::Char(_) => true,
        StaticValue::Int(_) => true,
        StaticValue::Float(_) => true,
        StaticValue::String(_) => true,
        StaticValue::Zero(_) => true,
        StaticValue::Struct(s) => static_values
            .get_slice(s.fields)
            .iter()
            .all(|field_id| is_llvm_const_representable(static_values, *field_id)),
        StaticValue::Enum(e) => match e.payload {
            None => true,
            Some(payload_id) => is_llvm_const_representable(static_values, payload_id),
        },
        StaticValue::LinearContainer(cont) => static_values
            .get_slice(cont.elements)
            .iter()
            .all(|elem_id| is_llvm_const_representable(static_values, *elem_id)),
    }
}
