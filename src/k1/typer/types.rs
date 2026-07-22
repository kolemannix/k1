// Copyright (c) 2026 knix
// All rights reserved.

use std::collections::hash_map::Entry;
use std::fmt::{Display, Formatter};
use std::hash::Hasher;

use fxhash::FxHashMap;

use crate::typer::scopes::*;

use crate::parse::{IdentPool, ParsedId, StringId};

use crate::{SV4, impl_copy_if_small, nz_u32_id, typer::*};

nz_u32_id!(TypeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
}

impl Layout {
    pub fn from_rust_type<T>() -> Layout {
        let size = std::mem::size_of::<T>() as u32;
        let align = std::mem::align_of::<T>() as u32;
        Layout { size, align }
    }

    pub fn strided(&self) -> Layout {
        Layout { size: self.stride(), align: self.align }
    }

    pub fn from_scalar_bytes(bytes: u32) -> Layout {
        Layout { size: bytes, align: bytes }
    }

    pub fn from_scalar_bits(bits: u32) -> Layout {
        Layout { size: bits / 8, align: bits / 8 }
    }
    pub const ZERO_SIZED: Layout = Layout { size: 0, align: 1 };

    pub fn stride(&self) -> u32 {
        self.size.next_multiple_of(self.align)
    }

    pub fn offset_at_index(&self, index: usize) -> usize {
        self.stride() as usize * index
    }

    // Returns: the start, or offset, of the new field
    pub fn append_to_aggregate(&mut self, layout: Layout) -> u32 {
        debug_assert_ne!(layout.align, 0);
        let offset = self.size;
        let new_field_start = offset.next_multiple_of(layout.align);
        if cfg!(debug_assertions) {
            let padding = new_field_start - offset;
            if padding != 0 {
                debug!("Aggregate padding: {padding}");
            }
        };
        let new_end_unaligned = new_field_start + layout.size;
        let new_align = std::cmp::max(self.align, layout.align);
        self.size = new_end_unaligned;
        self.align = new_align;
        new_field_start
    }

    pub fn size_bits(&self) -> u32 {
        self.size * 8
    }

    pub fn align_bits(&self) -> u32 {
        self.align * 8
    }

    pub fn array_me(&self, len: usize) -> Layout {
        let element_size_padded = self.stride();
        let array_size = element_size_padded * (len as u32);
        Layout { size: array_size, align: if array_size == 0 { 1 } else { self.align } }
    }
}

impl Display for Layout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Layout {{ size {}, align {} }}", self.size, self.align)
    }
}

#[derive(Clone)]
pub struct StructTypeField {
    pub name: StringId,
    pub type_id: TypeId,
    pub span: SpanId,
}
impl_copy_if_small!(12, StructTypeField);

#[derive(Clone)]
pub struct GenericInstanceInfo {
    pub generic_parent: TypeId,
    pub type_args: TypeIdSlice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LoopType {
    Loop,
    While,
}

nz_u32_id!(TypeDefnId);

#[derive(Clone, PartialEq, Eq)]
// TODO: For speed, these can just be stored by ast_id. store ast_id instead of defn_info on every type,
// then look it up
// they're not small
pub struct TypeDefnInfo {
    pub name: StringId,
    pub scope: ScopeId,
    pub companion_namespace: Option<NamespaceId>,
    pub ast_id: ParsedId,
    pub recursive: bool,
}
impl_copy_if_small!(24, TypeDefnInfo);

impl std::hash::Hash for TypeDefnInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.scope.hash(state);
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum RecordKind {
    Struct,
    Union,
}

impl RecordKind {
    pub fn kind_name(&self) -> &'static str {
        match self {
            RecordKind::Struct => "struct",
            RecordKind::Union => "union",
        }
    }
}

#[derive(Clone, Copy)]
pub struct StructType {
    pub fields: MSlice<StructTypeField, TypePool>,
    pub record_kind: RecordKind,
}

impl StructType {
    pub fn struc(fields: MSlice<StructTypeField, TypePool>) -> StructType {
        StructType { fields, record_kind: RecordKind::Struct }
    }

    pub fn find_field(
        &self,
        m: &kmem::Mem<TypePool>,
        field_name: StringId,
    ) -> Option<(usize, &StructTypeField)> {
        m.getn(self.fields).iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const EMPTY_TYPE_ID: TypeId = TypeId(NonZeroU32::new(1).unwrap());
pub const U8_TYPE_ID: TypeId = TypeId(NonZeroU32::new(2).unwrap());
pub const U16_TYPE_ID: TypeId = TypeId(NonZeroU32::new(3).unwrap());
pub const U32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(4).unwrap());
pub const U64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(5).unwrap());
pub const I8_TYPE_ID: TypeId = TypeId(NonZeroU32::new(6).unwrap());
pub const I16_TYPE_ID: TypeId = TypeId(NonZeroU32::new(7).unwrap());
pub const I32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(8).unwrap());
pub const I64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(9).unwrap());
pub const SIZE_TYPE_ID: TypeId = I64_TYPE_ID;

pub const CHAR_TYPE_ID: TypeId = TypeId(NonZeroU32::new(10).unwrap());
pub const BOOL_TYPE_ID: TypeId = TypeId(NonZeroU32::new(11).unwrap());
pub const NEVER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(12).unwrap());
pub const POINTER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(13).unwrap());
pub const F32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(14).unwrap());
pub const F64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(15).unwrap());

#[derive(Clone)]
pub struct ListType {
    pub element_type: TypeId,
}

#[derive(Clone)]
pub struct TypeParameter {
    pub name: StringId,
    pub static_constraint: Option<TypeId>,
    pub predicate_functions: MSlice<QIdent, TypedProgram>,
    pub scope_id: ScopeId,
    pub span: SpanId,
}
impl_copy_if_small!(24, TypeParameter);

#[derive(Clone)]
pub struct FunctionTypeParameter {
    pub name: StringId,
    pub scope_id: ScopeId,
    pub span: SpanId,
    pub function_type: TypeId,
}

#[derive(Clone)]
pub struct InferenceHoleType {
    pub index: u32,
    pub static_type: Option<TypeId>,
}

#[derive(Clone, Copy)]
pub struct ReferenceType {
    pub inner_type: TypeId,
}

#[derive(Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
    pub size_type: TypeId,
}
impl_copy_if_small!(8, ArrayType);

#[derive(Clone, Copy)]
pub struct TypedSumVariant {
    pub name: StringId,
    pub index: u32,
    pub payload: Option<TypeId>,
    pub tag_value: TypedIntValue,
    pub name_span: SpanId,
}

#[derive(Clone, Copy)]
pub struct SumType {
    pub variants: MSlice<TypedSumVariant, TypePool>,
    pub tag_type: IntegerType,
}

#[derive(Copy, Clone)]
pub struct ScalarEnumValue {
    pub name: StringId,
    pub int_value: TypedIntValue,
    pub name_span: SpanId,
}

#[derive(Copy, Clone)]
pub struct ScalarEnumType {
    pub member_values: MSlice<ScalarEnumValue, TypePool>,
    pub int_type: IntegerType,
}

impl SumType {}

#[derive(Clone)]
pub struct GenericType {
    pub params: MSlice<NameAndType, TypedProgram>,
    pub inner: TypeId,
}

impl GenericType {}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IntegerType {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    I8 = 4,
    I16 = 5,
    I32 = 6,
    I64 = 7,
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
        }
    }
}

impl IntegerType {
    pub fn type_id(&self) -> TypeId {
        match self {
            Self::U8 => U8_TYPE_ID,
            Self::U16 => U16_TYPE_ID,
            Self::U32 => U32_TYPE_ID,
            Self::U64 => U64_TYPE_ID,
            Self::I8 => I8_TYPE_ID,
            Self::I16 => I16_TYPE_ID,
            Self::I32 => I32_TYPE_ID,
            Self::I64 => I64_TYPE_ID,
        }
    }

    pub fn width(&self) -> NumericWidth {
        match self {
            Self::U8 | Self::I8 => NumericWidth::B8,
            Self::U16 | Self::I16 => NumericWidth::B16,
            Self::U32 | Self::I32 => NumericWidth::B32,
            Self::U64 | Self::I64 => NumericWidth::B64,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => false,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 => true,
        }
    }

    pub fn zero(&self) -> TypedIntValue {
        match self {
            IntegerType::U8 => TypedIntValue::U8(0),
            IntegerType::U16 => TypedIntValue::U16(0),
            IntegerType::U32 => TypedIntValue::U32(0),
            IntegerType::U64 => TypedIntValue::U64(0),
            IntegerType::I8 => TypedIntValue::I8(0),
            IntegerType::I16 => TypedIntValue::I16(0),
            IntegerType::I32 => TypedIntValue::I32(0),
            IntegerType::I64 => TypedIntValue::I64(0),
        }
    }

    pub fn get_scalar_type(&self) -> ScalarType {
        match self {
            IntegerType::U8 => ScalarType::U8,
            IntegerType::U16 => ScalarType::U16,
            IntegerType::U32 => ScalarType::U32,
            IntegerType::U64 => ScalarType::U64,
            IntegerType::I8 => ScalarType::I8,
            IntegerType::I16 => ScalarType::I16,
            IntegerType::I32 => ScalarType::I32,
            IntegerType::I64 => ScalarType::I64,
        }
    }

    pub fn get_pt(&self) -> PhysicalType {
        PhysicalType::scalar(self.get_scalar_type())
    }
}

#[derive(Clone, Copy)]
pub enum FloatType {
    F32 = 0,
    F64 = 1,
}
impl FloatType {
    pub fn zero(&self) -> TypedFloatValue {
        match self {
            FloatType::F32 => TypedFloatValue::F32(0.0),
            FloatType::F64 => TypedFloatValue::F64(0.0),
        }
    }

    pub fn size(&self) -> NumericWidth {
        match self {
            FloatType::F32 => NumericWidth::B32,
            FloatType::F64 => NumericWidth::B64,
        }
    }
}

// pub struct Spanned<T> {
//     pub v: T,
//     pub span: SpanId,
// }

#[derive(Clone, Copy)]
pub struct FnParamType {
    pub type_id: TypeId,
    pub name: StringId,
    pub is_context: bool,
    pub is_lambda_env: bool,
    pub is_macro_code: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AbiMode {
    Internal,
    Native,
}

#[derive(Clone)]
pub struct FunctionType {
    pub physical_params: MSlice<FnParamType, TypePool>,
    pub return_type: TypeId,
    pub is_lambda: bool,
    pub abi_mode: AbiMode,
}
impl_copy_if_small!(16, FunctionType);

impl FunctionType {
    pub fn logical_params(&self) -> MSlice<FnParamType, TypePool> {
        if self.is_lambda { self.physical_params.skip(1) } else { self.physical_params }
    }
}

nz_u32_id!(LambdaTypeId);

#[derive(Clone)]
pub struct LambdaType {
    pub function_type: TypeId,
    pub env_type: TypeId,
    pub parsed_id: ParsedId,
    pub function_id: FunctionId,
    // This kinda crosses the streams; its a value expression in a type, but
    // that's because a lambda's environment is basically values baked into a function
    // Its almost like a comptime-known value, aka the type 5
    // This expression must be run from the spot the lambda is defined
    pub environment_struct: TypedExprId,
}

#[derive(Clone)]
pub struct LambdaObjectType {
    pub function_type: TypeId,
    pub parsed_id: ParsedId,
    pub struct_representation: TypeId,
}

#[derive(Clone, Copy)]
pub struct StaticValueType {
    pub family_type_id: TypeId,
    pub value_id: Option<StaticValueId>,
}

#[derive(Clone, Copy)]
pub struct FunctionPointerType {
    pub function_type_id: TypeId,
}

#[derive(Clone, Copy)]
pub struct OpaqueType {
    pub size: u32,
    pub align: u32,
}

impl OpaqueType {
    pub fn layout(&self) -> Layout {
        Layout { size: self.size, align: self.align }
    }
}

static_assert_size!(Type, 28);
#[derive(Clone)]
pub enum Type {
    Char,
    Bool,
    /// Our Pointer is a raw untyped pointer; we mostly have this type for expressing intent
    /// and because it allows us to treat it as a ptr in LLVM which
    /// allows for pointer-reasoning based optimizations
    Pointer,
    Integer(IntegerType),
    Float(FloatType),
    Enum(ScalarEnumType),
    Reference(ReferenceType),
    Array(ArrayType),
    Struct(StructType),
    Sum(SumType),
    Opaque(OpaqueType),

    /// An uninhabited type; used to indicate divergent control flow
    Never,
    Function(FunctionType),
    /// Function pointers deserve to be represented differently than just a Reference to a function type
    /// Otherwise, function pointers become a special case of references almost
    /// everywhere, since they can't be de-referenced and don't point to a physical k1 type
    FunctionPointer(FunctionPointerType),
    Lambda(LambdaTypeId),
    LambdaObject(LambdaObjectType),

    StaticValue(StaticValueType),

    // Not-so-physical types
    Generic(GenericType),
    #[allow(clippy::enum_variant_names)]
    TypeParameter(TypeParameter),
    FunctionTypeParameter(FunctionTypeParameter),
    InferenceHole(InferenceHoleType),
}

impl TypePool {
    fn type_eq(
        &self,
        t1: &Type,
        t2: &Type,
        defn1: Option<&TypeDefnInfo>,
        defn2: Option<&TypeDefnInfo>,
    ) -> bool {
        match (t1, t2) {
            (Type::Char, Type::Char) => true,
            (Type::Integer(int1), Type::Integer(int2)) => int1 == int2,
            (Type::Enum(se1), Type::Enum(se2)) => {
                if defn1 != defn2 {
                    return false;
                }
                if se1.member_values.len() != se2.member_values.len() {
                    return false;
                }
                for (v1, v2) in
                    self.mem.getn(se1.member_values).iter().zip(self.mem.getn(se2.member_values))
                {
                    let mismatch = v1.name != v2.name || v1.int_value != v2.int_value;
                    if mismatch {
                        return false;
                    }
                }
                true
            }
            (Type::Float(f1), Type::Float(f2)) => f1.size() == f2.size(),
            (Type::Bool, Type::Bool) => true,
            (Type::Pointer, Type::Pointer) => true,
            (Type::Struct(s1), Type::Struct(s2)) => {
                if defn1 != defn2 {
                    return false;
                }
                if s1.fields.len() != s2.fields.len() {
                    return false;
                }
                for (f1, f2) in self.mem.getn(s1.fields).iter().zip(self.mem.getn(s2.fields)) {
                    let mismatch = f1.name != f2.name || f1.type_id != f2.type_id;
                    if mismatch {
                        return false;
                    }
                }
                true
            }
            (Type::Reference(r1), Type::Reference(r2)) => r1.inner_type == r2.inner_type,
            (Type::Array(a1), Type::Array(a2)) => {
                a1.element_type == a2.element_type && a1.size_type == a2.size_type
            }
            (Type::TypeParameter(t1), Type::TypeParameter(t2)) => {
                t1.name == t2.name && t1.scope_id == t2.scope_id
            }
            (Type::FunctionTypeParameter(ftp1), Type::FunctionTypeParameter(ftp2)) => {
                ftp1.name == ftp2.name
                    && ftp1.scope_id == ftp2.scope_id
                    && ftp1.function_type == ftp2.function_type
            }
            (Type::InferenceHole(h1), Type::InferenceHole(h2)) => {
                h1.index == h2.index && h1.static_type == h2.static_type
            }
            (Type::Sum(e1), Type::Sum(e2)) => {
                if defn1 != defn2 {
                    return false;
                }
                if e1.variants.len() != e2.variants.len() {
                    return false;
                }
                for (v1, v2) in self.mem.getn(e1.variants).iter().zip(self.mem.getn(e2.variants)) {
                    let mismatch = v1.name != v2.name || v1.payload != v2.payload;
                    if mismatch {
                        return false;
                    }
                }
                true
            }
            (Type::Opaque(o1), Type::Opaque(o2)) => {
                if defn1 != defn2 {
                    return false;
                }

                o1.size == o2.size && o1.align == o2.align
            }
            (Type::Never, Type::Never) => true,
            // We never really want to de-dupe this type as its inherently unique
            (Type::Generic(_g1), Type::Generic(_g2)) => false,
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.return_type == f2.return_type
                    && f1.physical_params.len() == f2.physical_params.len()
                {
                    self.mem
                        .getn(f1.physical_params)
                        .iter()
                        .zip(self.mem.getn(f2.physical_params))
                        .all(|(p1, p2)| {
                            p1.type_id == p2.type_id && p1.is_macro_code == p2.is_macro_code
                        })
                } else {
                    false
                }
            }
            (Type::FunctionPointer(fp1), Type::FunctionPointer(fp2)) => {
                fp1.function_type_id == fp2.function_type_id
            }
            (Type::Lambda(lt1_id), Type::Lambda(lt2_id)) => {
                if *lt1_id == *lt2_id {
                    true
                } else {
                    let lt1 = self.lambda_types.get(*lt1_id);
                    let lt2 = self.lambda_types.get(*lt2_id);
                    // The function type is key here so that we _dont_ equate 'inference artifact' lambdas
                    // with real ones: '0 -> '1 vs int -> bool
                    lt1.function_id == lt2.function_id
                        && lt1.function_type == lt2.function_type
                        && lt1.parsed_id == lt2.parsed_id
                }
            }
            (Type::LambdaObject(_co1), Type::LambdaObject(_co2)) => false,
            (Type::StaticValue(vt1), Type::StaticValue(vt2)) => vt1.value_id == vt2.value_id,
            (t1, t2) => {
                if t1.kind_name() == t2.kind_name() {
                    panic!("Missing handling for kind in type_eq: {}", t1.kind_name())
                };
                false
            }
        }
    }

    fn hash_type(&self, typ: &Type, defn: Option<TypeDefnInfo>) -> u64 {
        use std::hash::Hash;
        use std::hash::Hasher;
        use std::mem::discriminant;

        let mut hasher = fxhash::FxHasher::default();
        let state = &mut hasher;

        discriminant(typ).hash(state);
        match typ {
            Type::Char => {}
            Type::Integer(int) => discriminant(int).hash(state),
            Type::Enum(_se) => {
                defn.hash(state);
            }
            Type::Bool => {}
            Type::Struct(s) => {
                defn.hash(state);
                s.record_kind.hash(state);
                s.fields.len().hash(state);
                for f in self.mem.getn(s.fields) {
                    f.name.hash(state);
                    f.type_id.hash(state);
                }
            }
            Type::Reference(r) => {
                r.inner_type.hash(state);
            }
            Type::TypeParameter(t_param) => {
                t_param.name.hash(state);
                t_param.scope_id.hash(state);
            }
            Type::FunctionTypeParameter(ftp) => {
                ftp.name.hash(state);
                ftp.scope_id.hash(state);
                ftp.function_type.hash(state);
            }
            Type::InferenceHole(hole) => {
                hole.index.hash(state);
                hole.static_type.hash(state);
            }
            Type::Sum(e) => {
                defn.hash(state);
                e.variants.len().hash(state);
                for v in self.mem.getn(e.variants) {
                    v.name.hash(state);
                    v.payload.hash(state);
                }
            }
            Type::Opaque(opaque) => {
                defn.hash(state);
                opaque.size.hash(state);
                opaque.align.hash(state);
            }
            // Inherently unique as well
            Type::Generic(_generic) => {
                defn.hash(state);
            }
            Type::Function(fun) => {
                fun.return_type.hash(state);
                for param in self.mem.getn(fun.physical_params) {
                    param.type_id.hash(state);
                    param.name.hash(state);
                    param.is_context.hash(state);
                    param.is_lambda_env.hash(state);
                    param.is_macro_code.hash(state);
                }
            }
            Type::FunctionPointer(fp) => fp.function_type_id.hash(state),
            Type::Lambda(lt_id) => {
                let lt = self.lambda_types.get(*lt_id);
                lt.parsed_id.hash(state);
                lt.function_type.hash(state);
                lt.function_id.hash(state);
            }
            Type::Float(ft) => {
                ft.size().bits().hash(state);
            }
            Type::Pointer => {}
            Type::Never => {}
            Type::LambdaObject(co) => {
                co.function_type.hash(state);
                co.struct_representation.hash(state);
            }
            Type::StaticValue(svt) => {
                svt.family_type_id.hash(state);
                svt.value_id.hash(state)
            }
            Type::Array(arr) => {
                arr.element_type.hash(state);
                arr.size_type.hash(state);
            }
        }
        state.finish()
    }
}

impl Type {
    pub fn kind_name(&self) -> &'static str {
        match self {
            Type::Char => "char",
            Type::Integer(_) => "integer",
            Type::Float(_) => "float",
            Type::Bool => "bool",
            Type::Pointer => "pointer",
            Type::Struct(_) => "struct",
            Type::Reference(_) => "reference",
            Type::TypeParameter(_) => "param",
            Type::FunctionTypeParameter(_) => "ftp",
            Type::InferenceHole(_) => "hole",
            Type::Sum(_) => "sum",
            Type::Opaque(_) => "opaque",
            Type::Enum(_) => "enum",
            Type::Never => "never",
            Type::Generic(_) => "generic",
            Type::Function(_) => "function",
            Type::FunctionPointer(_) => "function_ptr",
            Type::Lambda(_) => "lambda",
            Type::LambdaObject(_) => "lambdaobj",
            Type::StaticValue(_) => "value",
            Type::Array(_) => "array",
        }
    }

    pub fn as_reference(&self) -> Option<ReferenceType> {
        match self {
            Type::Reference(r) => Some(*r),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_reference(&self) -> ReferenceType {
        match self {
            Type::Reference(r) => *r,
            _ => panic!("expect_reference called on: {}", self.kind_name()),
        }
    }

    pub fn as_function_pointer(&self) -> Option<FunctionPointerType> {
        match self {
            Type::FunctionPointer(fp) => Some(*fp),
            _ => None,
        }
    }

    pub fn as_value_type(&self) -> Option<&StaticValueType> {
        match self {
            Type::StaticValue(v) => Some(v),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_sum_mut(&mut self) -> &mut SumType {
        match self {
            Type::Sum(s) => s,
            _ => panic!("expected sum type"),
        }
    }

    pub fn as_sum(&self) -> Option<&SumType> {
        match self {
            Type::Sum(sum) => Some(sum),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_sum(&self) -> &SumType {
        match self {
            Type::Sum(sum) => sum,
            _ => panic!("expected sum on {}", self.kind_name()),
        }
    }

    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            Type::Struct(struc) => Some(struc),
            _ => None,
        }
    }

    #[track_caller]
    pub fn as_enum(&self) -> Option<&ScalarEnumType> {
        match self {
            Type::Enum(e) => Some(e),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_enum(&self) -> &ScalarEnumType {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum on {}", self.kind_name()),
        }
    }

    #[track_caller]
    pub fn expect_struct(&self) -> &StructType {
        match self {
            Type::Struct(struc) => struc,
            _ => panic!("expect_struct called on: {}", self.kind_name()),
        }
    }

    #[track_caller]
    pub fn expect_generic(&self) -> &GenericType {
        match self {
            Type::Generic(g) => g,
            _ => panic!("expect_generic called on: {}", self.kind_name()),
        }
    }

    pub fn as_integer(&self) -> Option<&IntegerType> {
        match self {
            Type::Integer(int) => Some(int),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<&FloatType> {
        match self {
            Type::Float(float) => Some(float),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_integer(&self) -> IntegerType {
        match self {
            Type::Integer(int) => *int,
            _ => panic!("expect_integer called on: {}", self.kind_name()),
        }
    }

    pub fn as_tvar(&self) -> Option<&TypeParameter> {
        match self {
            Type::TypeParameter(tvar) => Some(tvar),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&FunctionType> {
        match self {
            Type::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> Option<LambdaTypeId> {
        match self {
            Type::Lambda(l) => Some(*l),
            _ => None,
        }
    }
    pub fn as_lambda_object(&self) -> Option<&LambdaObjectType> {
        match self {
            Type::LambdaObject(co) => Some(co),
            _ => None,
        }
    }

    pub fn as_type_parameter(&self) -> Option<&TypeParameter> {
        match self {
            Type::TypeParameter(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_function_type_parameter(&self) -> Option<&FunctionTypeParameter> {
        match self {
            Type::FunctionTypeParameter(ftp) => Some(ftp),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_function(&self) -> &FunctionType {
        match self {
            Type::Function(f) => f,
            _ => panic!("expect_function called on: {}", self.kind_name()),
        }
    }

    pub(crate) fn as_array(&self) -> Option<ArrayType> {
        match self {
            Type::Array(arr) => Some(*arr),
            _ => None,
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct TypeVariableInfo {
    pub inference_variable_count: u32,
    pub type_parameter_count: u32,
    pub unresolved_static_count: u32,
}

impl TypeVariableInfo {
    const EMPTY: TypeVariableInfo = TypeVariableInfo {
        inference_variable_count: 0,
        type_parameter_count: 0,
        unresolved_static_count: 0,
    };

    pub fn is_abstract(&self) -> bool {
        self.inference_variable_count > 0
            || self.type_parameter_count > 0
            || self.unresolved_static_count > 0
    }

    fn add(self, other: &Self) -> Self {
        Self {
            inference_variable_count: self.inference_variable_count
                + other.inference_variable_count,
            type_parameter_count: self.type_parameter_count + other.type_parameter_count,
            unresolved_static_count: self.unresolved_static_count + other.unresolved_static_count,
        }
    }
}

#[derive(Default)]
pub struct BuiltinTypes {
    pub empty: TypeId,
    pub buffer: Option<TypeId>,
    pub span: Option<TypeId>,
    pub list: Option<TypeId>,
    pub string: Option<TypeId>,
    pub opt: Option<TypeId>,
    pub code: Option<TypeId>,
    pub code_span: Option<TypeId>,
    pub code_builder: Option<TypeId>,
    pub dyn_lambda_obj: Option<TypeId>,
    pub source_location: Option<TypeId>,
    pub ordering: Option<TypeId>,
    pub types_layout: Option<TypeId>,
    pub types_type_schema: Option<TypeId>,
    pub types_int_kind: Option<TypeId>,
    pub types_int_value: Option<TypeId>,
    pub types_float_kind: Option<TypeId>,
    pub types_float_value: Option<TypeId>,
}

impl BuiltinTypes {
    pub fn assert_complete(&self) {
        assert!(self.empty != TypeId::PENDING);
        debug_assert!(self.buffer.is_some());
        debug_assert!(self.span.is_some());
        debug_assert!(self.list.is_some());
        debug_assert!(self.string.is_some());
        debug_assert!(self.opt.is_some());
        debug_assert!(self.code.is_some());
        debug_assert!(self.code_span.is_some());
        debug_assert!(self.code_builder.is_some());
        debug_assert!(self.dyn_lambda_obj.is_some());
        debug_assert!(self.types_layout.is_some());
        debug_assert!(self.types_type_schema.is_some());
        debug_assert!(self.types_int_kind.is_some());
        debug_assert!(self.types_int_value.is_some());
        debug_assert!(self.types_float_kind.is_some());
        debug_assert!(self.types_float_value.is_some());
    }
    pub fn string(&self) -> TypeId {
        self.string.expect("string builtin missing")
    }
    pub fn buffer(&self) -> TypeId {
        self.buffer.expect("buffer builtin missing")
    }
    pub fn list(&self) -> TypeId {
        self.list.expect("list builtin missing")
    }
    pub fn span(&self) -> TypeId {
        self.span.expect("span builtin missing")
    }
    pub fn opt(&self) -> TypeId {
        self.opt.expect("opt builtin missing")
    }
    pub fn code(&self) -> TypeId {
        self.code.expect("code builtin missing")
    }
    pub fn code_span(&self) -> TypeId {
        self.code_span.expect("code-span builtin missing")
    }
    pub fn code_builder(&self) -> TypeId {
        self.code_builder.expect("code-builder builtin missing")
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarType {
    U8 = 1,
    U16 = 2,
    U32 = 3,
    U64 = 4,
    I8 = 5,
    I16 = 6,
    I32 = 7,
    I64 = 8,
    F32 = 9,
    F64 = 10,
    Pointer = 11,
}

impl ScalarType {
    pub const fn to_tag(self) -> u8 {
        self as u8
    }

    pub const fn from_tag(tag: u32) -> Self {
        match tag {
            1 => Self::U8,
            2 => Self::U16,
            3 => Self::U32,
            4 => Self::U64,
            5 => Self::I8,
            6 => Self::I16,
            7 => Self::I32,
            8 => Self::I64,
            9 => Self::F32,
            10 => Self::F64,
            11 => Self::Pointer,
            _ => panic!("Not a scalartype tag"),
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            ScalarType::I8
                | ScalarType::I16
                | ScalarType::I32
                | ScalarType::I64
                | ScalarType::U8
                | ScalarType::U16
                | ScalarType::U32
                | ScalarType::U64
        )
    }

    pub fn get_layout(&self) -> Layout {
        match self {
            ScalarType::U8 | ScalarType::I8 => Layout::from_scalar_bits(8),
            ScalarType::U16 | ScalarType::I16 => Layout::from_scalar_bits(16),
            ScalarType::U32 | ScalarType::I32 => Layout::from_scalar_bits(32),
            ScalarType::U64 | ScalarType::I64 => Layout::from_scalar_bits(64),
            ScalarType::F32 => Layout::from_scalar_bits(32),
            ScalarType::F64 => Layout::from_scalar_bits(64),
            ScalarType::Pointer => Layout::from_scalar_bits(64),
        }
    }

    pub fn width(&self) -> NumericWidth {
        match self {
            ScalarType::U8 | ScalarType::I8 => NumericWidth::B8,
            ScalarType::U16 | ScalarType::I16 => NumericWidth::B16,
            ScalarType::U32 | ScalarType::I32 => NumericWidth::B32,
            ScalarType::U64 | ScalarType::I64 => NumericWidth::B64,
            ScalarType::F32 => NumericWidth::B32,
            ScalarType::F64 => NumericWidth::B64,
            ScalarType::Pointer => NumericWidth::B64,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PhysicalTypeResult {
    No,
    Never,
    Infinite,
    Yes(PhysicalType),
}

impl PhysicalTypeResult {
    pub fn unwrap(self) -> PhysicalType {
        match self {
            PhysicalTypeResult::No => panic!("Called unwrap on PhysicalTypeResult::No"),
            PhysicalTypeResult::Never => panic!("Called unwrap on PhysicalTypeResult::Never"),
            PhysicalTypeResult::Infinite => panic!("Called unwrap on PhysicalTypeResult::Infinite"),
            PhysicalTypeResult::Yes(pt) => pt,
        }
    }
}

#[repr(transparent)]
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct PhysicalType(u32);

impl std::fmt::Debug for PhysicalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.as_enum() {
            PhysicalTypeEnum::Scalar(scalar_type) => write!(f, "{}", scalar_type),
            PhysicalTypeEnum::Agg(agg_id) => write!(f, "{}", agg_id),
            PhysicalTypeEnum::Empty => f.write_str("{}"),
        }
    }
}

impl PhysicalType {
    pub const EMPTY: PhysicalType = PhysicalType(0);
    pub const U8: PhysicalType = PhysicalType(ScalarType::U8 as u32);
    pub const PTR: PhysicalType = PhysicalType(ScalarType::Pointer as u32);

    const MIN_AGG_ID: u32 = 16;

    /// Raw representation, for encoding into bytecode operands
    pub const fn to_u32(self) -> u32 {
        self.0
    }

    /// Inverse of `to_u32`
    pub const fn from_u32(v: u32) -> PhysicalType {
        PhysicalType(v)
    }

    pub const fn as_scalar(self) -> Option<ScalarType> {
        match self.0 {
            1..=11 => Some(ScalarType::from_tag(self.0)),
            _ => None,
        }
    }

    pub const fn expect_scalar(&self) -> ScalarType {
        self.as_scalar().unwrap()
    }

    pub const fn expect_agg(&self) -> AggregateTypeId {
        debug_assert!(self.is_agg());
        AggregateTypeId::from_u32(self.0).unwrap()
    }

    pub const fn is_ptr(self) -> bool {
        self.0 as u8 == ScalarType::Pointer as u8
    }

    pub const fn is_u8(self) -> bool {
        self.0 as u8 == ScalarType::U8 as u8
    }

    pub const fn is_agg(self) -> bool {
        self.0 >= Self::MIN_AGG_ID
    }

    pub const fn is_scalar(self) -> bool {
        self.0 >= ScalarType::U8 as u32 && self.0 <= ScalarType::Pointer as u32
    }

    pub const fn is_int(self) -> bool {
        self.0 >= ScalarType::U8 as u32 && self.0 <= ScalarType::I64 as u32
    }

    pub fn is_empty(self) -> bool {
        self == Self::EMPTY
    }

    pub const fn pack(pt: PhysicalTypeEnum) -> PhysicalType {
        match pt {
            PhysicalTypeEnum::Scalar(st) => Self::scalar(st),
            PhysicalTypeEnum::Agg(agg_id) => Self::agg(agg_id),
            PhysicalTypeEnum::Empty => Self::EMPTY,
        }
    }
    pub fn as_enum(self) -> PhysicalTypeEnum {
        match self.0 {
            0 => PhysicalTypeEnum::Empty,
            t @ 1..=11 => PhysicalTypeEnum::Scalar(ScalarType::from_tag(t)),
            agg_id => PhysicalTypeEnum::Agg(AggregateTypeId::from_u32(agg_id).unwrap()),
        }
    }

    pub const fn scalar(st: ScalarType) -> PhysicalType {
        PhysicalType(st.to_tag() as u32)
    }

    pub const fn agg(agg_id: AggregateTypeId) -> PhysicalType {
        debug_assert!(agg_id.as_u32() >= Self::MIN_AGG_ID);
        PhysicalType(agg_id.as_u32())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysicalTypeEnum {
    Scalar(ScalarType),
    Agg(AggregateTypeId),
    /// All zero-sized type ids compile to the Empty variant, so zero-sized behavior is
    /// handled consistently everywhere
    Empty,
}

impl PhysicalTypeEnum {
    pub const fn pack(self) -> PhysicalType {
        PhysicalType::pack(self)
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            PhysicalTypeEnum::Scalar(_) => "scalar",
            PhysicalTypeEnum::Agg(_) => "agg",
            PhysicalTypeEnum::Empty => "empty",
        }
    }

    pub fn is_agg(&self) -> bool {
        matches!(self, PhysicalTypeEnum::Agg(_))
    }

    #[track_caller]
    pub fn expect_agg(&self) -> AggregateTypeId {
        match self {
            PhysicalTypeEnum::Agg(id) => *id,
            _ => panic!("Expected agg on {}", self.kind_name()),
        }
    }

    #[track_caller]
    pub fn expect_scalar(&self) -> ScalarType {
        match self {
            PhysicalTypeEnum::Scalar(s) => *s,
            _ => panic!("Expected scalar"),
        }
    }
    pub fn as_scalar(&self) -> Option<ScalarType> {
        match self {
            PhysicalTypeEnum::Scalar(s) => Some(*s),
            _ => None,
        }
    }

    pub fn is_scalar(&self) -> bool {
        matches!(self, PhysicalTypeEnum::Scalar(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, PhysicalTypeEnum::Empty)
    }
}

#[derive(Clone, Copy)]
pub struct StructField {
    pub offset: u32,
    pub field_t: PhysicalType,
    pub name: StringId,
}

#[derive(Clone, Copy)]
pub struct SumPt {
    pub tag_type: ScalarType,
    pub struct_repr: AggregateTypeId,
    pub variants: MSlice<SumVariantPt, TypePool>,
    pub payload_offset: u32,
}

#[derive(Clone, Copy)]
pub struct SumVariantPt {
    pub tag: TypedIntValue,
    pub payload: Option<PhysicalType>,
}

#[derive(Clone, Copy)]
pub struct UnionMember {
    pub name: StringId,
    pub ty: PhysicalType,
}

#[derive(Clone, Copy)]
pub enum AggType {
    Struct { fields: MSlice<StructField, TypePool> },
    Array { element_pt: PhysicalType, len: u32 },
    Union { members: MSlice<UnionMember, TypePool> },
    Sum(SumPt),
    Opaque { size: u32, align: u32 },
}

impl AggType {
    #[track_caller]
    pub fn expect_array(&self) -> (PhysicalType, u32) {
        match self {
            AggType::Array { element_pt: element_t, len } => (*element_t, *len),
            _ => panic!("Expected array agg type"),
        }
    }

    #[track_caller]
    pub fn expect_sum(&self) -> &SumPt {
        match self {
            AggType::Sum(sum) => sum,
            _ => panic!("Expected sum agg type"),
        }
    }

    #[track_caller]
    pub fn expect_struct(&self) -> MSlice<StructField, TypePool> {
        match self {
            AggType::Struct { fields } => *fields,
            _ => panic!("Expected struct agg type"),
        }
    }
}

nz_u32_id!(AggregateTypeId);
pub struct AggregateTypeRecord {
    pub agg_type: AggType,
    pub origin_type_id: TypeId,
    pub layout: Layout,
}

pub struct TypePoolIdents {
    tag: StringId,
    payload: StringId,
}

pub struct TypePool {
    pub types: VPool<Type, TypeId>,
    /// We use this to efficiently check if we already have seen a type,
    /// and retrieve its ID if so. We used to iterate the pool but it
    /// got slow
    pub hashes: FxHashMap<u64, TypeId>,

    pub type_variable_counts: VPool<TypeVariableInfo, TypeId>,
    pub instance_info: VPool<Option<GenericInstanceInfo>, TypeId>,

    pub defn_info: FxHashMap<TypeId, TypeDefnInfo>,
    pub specializations: FxHashMap<TypeId, Vec<(TypeIdSlice, TypeId)>>,
    pub phys_types: FxHashMap<TypeId, PhysicalTypeResult>,

    /// InferenceHole type ids by hole index, for holes with no static constraint.
    /// `add` hash-conses holes to one id per (index, static_type) anyway; this skips
    /// the hash+probe on the common path. PENDING marks not-yet-created indices.
    hole_type_cache: Vec<TypeId>,

    /// Lookup mappings for parsed -> typed ids
    pub ast_ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,

    pub builtins: BuiltinTypes,

    pub agg_types: VPool<AggregateTypeRecord, AggregateTypeId>,

    pub mem: kmem::Mem<TypePool>,

    /// Lambda types are big, they get extended storage
    pub lambda_types: VPool<LambdaType, LambdaTypeId>,

    pub idents: TypePoolIdents,
}

impl TypePool {
    pub fn empty(tag_ident: StringId, payload_ident: StringId) -> TypePool {
        let mut agg_types = VPool::make("phys_types");
        // Reserve the lower values so they dont conflict with scalars once packed
        agg_types.skip_next_n_slots(PhysicalType::MIN_AGG_ID as usize);

        TypePool {
            types: VPool::make("types"),
            hashes: FxHashMap::new(),

            type_variable_counts: VPool::make("type_variable_counts"),
            instance_info: VPool::make("instance_info"),

            defn_info: FxHashMap::new(),
            specializations: FxHashMap::new(),
            phys_types: FxHashMap::new(),

            hole_type_cache: Vec::new(),

            ast_ability_mapping: FxHashMap::default(),

            builtins: BuiltinTypes::default(),

            agg_types,

            mem: kmem::Mem::make(),

            lambda_types: VPool::make("lambdas"),

            idents: TypePoolIdents { tag: tag_ident, payload: payload_ident },
        }
    }

    #[cfg(test)]
    pub fn with_builtin_types() -> TypePool {
        let mut this = TypePool::empty(StringId::forged(), StringId::forged());
        this.add_anon(Type::Integer(IntegerType::U8));
        this.add_anon(Type::Integer(IntegerType::U16));
        this.add_anon(Type::Integer(IntegerType::U32));
        this.add_anon(Type::Integer(IntegerType::U64));
        this.add_anon(Type::Integer(IntegerType::I8));
        this.add_anon(Type::Integer(IntegerType::I16));
        this.add_anon(Type::Integer(IntegerType::I32));
        this.add_anon(Type::Integer(IntegerType::I64));

        this.add_anon(Type::Struct(StructType {
            fields: MSlice::empty(),
            record_kind: RecordKind::Struct,
        }));
        this.add_anon(Type::Char);
        this.add_anon(Type::Bool);
        this.add_anon(Type::Never);
        this.add_anon(Type::Pointer);
        this.add_anon(Type::Float(FloatType::F32));
        this.add_anon(Type::Float(FloatType::F64));

        this
    }

    pub fn add(
        &mut self,
        typ: Type,
        defn_info: Option<TypeDefnInfo>,
        instance_info: Option<GenericInstanceInfo>,
    ) -> TypeId {
        let hash = self.hash_type(&typ, defn_info);
        if let Entry::Occupied(entry) = self.hashes.entry(hash) {
            let existing_id = *entry.get();
            let existing = self.types.get(existing_id);
            let existing_defn_info = self.defn_info.get(&existing_id);
            if self.type_eq(&typ, existing, defn_info.as_ref(), existing_defn_info) {
                return existing_id;
            }
        }

        let type_id = self.types.add(typ);
        self.hashes.insert(hash, type_id);

        // 2 AoS fields to handle
        // pub type_variable_counts
        // pub instance_info

        let variable_counts = self.count_type_variables(type_id);
        self.type_variable_counts.add_expected_id(variable_counts, type_id);

        self.instance_info.add_expected_id(instance_info, type_id);

        if let Some(defn_info) = defn_info {
            self.defn_info.insert(type_id, defn_info);
        }

        debug_assert_eq!(self.type_variable_counts.len(), self.types.len());
        debug_assert_eq!(self.instance_info.len(), self.types.len());

        type_id
    }

    pub fn set_type(
        &mut self,
        id: TypeId,
        type_value: Type,
        instance_info: Option<GenericInstanceInfo>,
        defn_info: Option<TypeDefnInfo>,
    ) {
        let hash = self.hash_type(&type_value, defn_info);
        *self.get_mut(id) = type_value;
        self.hashes.insert(hash, id);

        let variable_counts = self.count_type_variables(id);
        *self.type_variable_counts.get_mut(id) = variable_counts;

        if let Some(defn_info) = defn_info {
            self.defn_info.insert(id, defn_info);
        }
        *self.instance_info.get_mut(id) = instance_info;
    }

    pub fn next_type_id(&self) -> TypeId {
        self.types.next_id()
    }

    pub fn reserve_id(&mut self) -> TypeId {
        let id = self.types.reserve_id();
        let id2 = self.instance_info.reserve_id();
        let id3 = self.type_variable_counts.reserve_id();
        debug_assert_eq!(id, id2);
        debug_assert_eq!(id, id3);
        id
    }

    pub fn add_reference_type(&mut self, inner_type: TypeId) -> TypeId {
        self.add_anon(Type::Reference(ReferenceType { inner_type }))
    }

    pub fn add_function_pointer_type(&mut self, function_type_id: TypeId) -> TypeId {
        self.add_anon(Type::FunctionPointer(FunctionPointerType { function_type_id }))
    }

    fn make_union_type(
        &mut self,
        origin_type_id: TypeId,
        members: MSlice<UnionMember, TypePool>,
    ) -> PhysicalType {
        let mut size = 0;
        let mut align = 1;
        for m in self.mem.getn(members) {
            let layout = self.get_pt_layout(m.ty);
            if layout.size > size {
                size = layout.size
            }
            if layout.align > align {
                align = layout.align
            }
        }
        let union_layout = Layout { size, align };
        if size == 0 {
            PhysicalType::EMPTY
        } else {
            let agg_id = self.agg_types.add(AggregateTypeRecord {
                agg_type: AggType::Union { members },
                origin_type_id,
                layout: union_layout,
            });
            PhysicalType::agg(agg_id)
        }
    }

    pub fn add_value_type(
        &mut self,
        family_type_id: TypeId,
        value_id: Option<StaticValueId>,
    ) -> TypeId {
        self.add_anon(Type::StaticValue(StaticValueType { family_type_id, value_id }))
    }

    pub fn add_anon(&mut self, typ: Type) -> TypeId {
        self.add(typ, None, None)
    }

    pub fn get_inference_hole(&mut self, index: u32, static_type: Option<TypeId>) -> TypeId {
        if static_type.is_none() {
            if let Some(&cached) = self.hole_type_cache.get(index as usize) {
                if cached != TypeId::PENDING {
                    return cached;
                }
            }
        }
        let type_id = self.add_anon(Type::InferenceHole(InferenceHoleType { index, static_type }));
        if static_type.is_none() {
            let index = index as usize;
            if self.hole_type_cache.len() <= index {
                self.hole_type_cache.resize(index + 1, TypeId::PENDING);
            }
            self.hole_type_cache[index] = type_id;
        }
        type_id
    }

    /// The two types of types that we need to treat as 'static' types are Static types themselves
    /// and type parameters with a constraint to a specific static, which is basically the same
    /// thing since it can have no other constraints
    pub fn get_value_type_id_of_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.get(type_id) {
            Type::StaticValue(_vt) => Some(type_id),
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                let t = tp.static_constraint.unwrap();
                debug_assert!(self.get(t).as_value_type().is_some());
                Some(t)
            }
            Type::InferenceHole(ih) if ih.static_type.is_some() => {
                let t = ih.static_type.unwrap();
                debug_assert!(self.get(t).as_value_type().is_some());
                Some(t)
            }
            _ => None,
        }
    }

    pub fn get_static_type_of_type(&self, type_id: TypeId) -> Option<StaticValueType> {
        match self.get_value_type_id_of_type(type_id) {
            None => None,
            Some(type_id) => Some(*self.get(type_id).as_value_type().unwrap()),
        }
    }

    pub fn is_static(&self, type_id: TypeId) -> bool {
        self.get_value_type_id_of_type(type_id).is_some()
    }

    #[inline]
    pub fn get(&self, type_id: TypeId) -> &Type {
        self.types.get(type_id)
    }

    pub fn get_struct_field(&self, type_id: TypeId, field_index: usize) -> &StructTypeField {
        self.mem.get_nth(self.get(type_id).expect_struct().fields, field_index)
    }

    pub fn get_struct_field_by_name(
        &self,
        type_id: TypeId,
        name: StringId,
    ) -> Option<(usize, &StructTypeField)> {
        self.get(type_id).expect_struct().find_field(&self.mem, name)
    }

    pub fn get_static_family_id_if_static(&self, type_id: TypeId) -> TypeId {
        match self.get(type_id) {
            Type::StaticValue(svt) => svt.family_type_id,
            _ => type_id,
        }
    }

    pub fn get_base_for_method(&self, type_id: TypeId) -> TypeId {
        // Follow references
        match self.get(type_id) {
            Type::Reference(r) => r.inner_type,
            _ => type_id,
        }
    }

    #[track_caller]
    pub fn get_type_parameter(&self, type_id: TypeId) -> &TypeParameter {
        if let Type::TypeParameter(tv) = self.get(type_id) {
            tv
        } else {
            panic!("Expected type variable on type {}", type_id)
        }
    }

    pub fn get_type_variable_mut(&mut self, type_id: TypeId) -> &mut TypeParameter {
        if let Type::TypeParameter(tv) = self.get_mut(type_id) {
            tv
        } else {
            panic!("Expected type variable on type {}", type_id)
        }
    }

    pub fn get_as_sum(&self, type_id: TypeId) -> Option<&SumType> {
        match self.get(type_id) {
            Type::Sum(e) => Some(e),
            _ => None,
        }
    }

    pub fn get_instance_info(&self, type_id: TypeId) -> Option<&GenericInstanceInfo> {
        self.instance_info.get(type_id).as_ref()
    }

    pub fn get_defn_info(&self, type_id: TypeId) -> Option<TypeDefnInfo> {
        self.defn_info.get(&type_id).copied()
    }

    pub fn get_companion_namespace(&self, type_id: TypeId) -> Option<NamespaceId> {
        self.get_defn_info(type_id).and_then(|info| info.companion_namespace)
    }

    pub fn add_lambda(
        &mut self,
        function_type_id: TypeId,
        environment_struct: TypedExprId,
        environment_type: TypeId,
        body_function_id: FunctionId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let lambda_type_id = self.lambda_types.add(LambdaType {
            function_type: function_type_id,
            env_type: environment_type,
            parsed_id,
            function_id: body_function_id,
            environment_struct,
        });
        let lambda_type_id = self.add_anon(Type::Lambda(lambda_type_id));
        lambda_type_id
    }

    pub const LAMBDA_OBJECT_FN_PTR_INDEX: usize = 0;
    pub const LAMBDA_OBJECT_ENV_PTR_INDEX: usize = 1;

    pub fn add_lambda_object(
        &mut self,
        identifiers: &IdentPool,
        function_type_id: TypeId,
        parsed_id: ParsedId,
    ) -> TypeId {
        // Unlike types.builtins.dyn_lam_obj, this contains the actual function type
        // for typechecking calls
        // The other one is just concerned with mapping to the right physical type.
        // Which is just { ptr, ptr }
        let fn_ptr_type = self.add_function_pointer_type(function_type_id);
        let fields = self.mem.pushn(&[
            StructTypeField {
                name: identifiers.b.fn_ptr,
                type_id: fn_ptr_type,
                span: SpanId::NONE,
            },
            StructTypeField {
                name: identifiers.b.env_ptr,
                type_id: POINTER_TYPE_ID,
                span: SpanId::NONE,
            },
        ]);
        let struct_representation = self.add_anon(Type::Struct(StructType::struc(fields)));
        self.add_anon(Type::LambdaObject(LambdaObjectType {
            function_type: function_type_id,
            parsed_id,
            struct_representation,
        }))
    }

    pub fn get_mut(&mut self, type_id: TypeId) -> &mut Type {
        self.types.get_mut(type_id)
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = TypeId> {
        (0..self.types.len()).map(|i| TypeId(NonZeroU32::new(i as u32 + 1).unwrap()))
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types
            .iter()
            .enumerate()
            .map(|(i, t)| (TypeId(NonZeroU32::new(i as u32 + 1).unwrap()), t))
    }

    pub fn get_type_id_dereferenced(&self, type_id: TypeId) -> TypeId {
        match self.get(type_id) {
            Type::Reference(r) => r.inner_type,
            _ => type_id,
        }
    }

    pub fn get_type_dereferenced(&self, type_id: TypeId) -> &Type {
        match self.get(type_id) {
            Type::Reference(r) => self.get(r.inner_type),
            _ => self.get(type_id),
        }
    }

    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    pub fn add_ability_mapping(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
        ability_id: AbilityId,
    ) -> bool {
        self.ast_ability_mapping.insert(parsed_ability_id, ability_id).is_none()
    }

    pub fn find_ability_mapping(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
    ) -> Option<AbilityId> {
        self.ast_ability_mapping.get(&parsed_ability_id).copied()
    }

    pub fn get_type_variable_counts(&self, type_id: TypeId) -> TypeVariableInfo {
        *self.type_variable_counts.get(type_id)
    }

    pub fn count_type_variables(&self, type_id: TypeId) -> TypeVariableInfo {
        const EMPTY: TypeVariableInfo = TypeVariableInfo::EMPTY;
        debug!("count_type_variables of {} {}", type_id, self.get(type_id).kind_name());

        match self.get(type_id) {
            Type::TypeParameter(_tp) => TypeVariableInfo {
                type_parameter_count: 1,
                inference_variable_count: 0,
                unresolved_static_count: 0,
            },
            Type::FunctionTypeParameter(ftp) => {
                let base_info = TypeVariableInfo {
                    type_parameter_count: 1,
                    inference_variable_count: 0,
                    unresolved_static_count: 0,
                };
                let fn_info = self.type_variable_counts.get(ftp.function_type);
                base_info.add(fn_info)
            }
            Type::InferenceHole(_hole) => TypeVariableInfo {
                type_parameter_count: 0,
                inference_variable_count: 1,
                unresolved_static_count: 0,
            },
            Type::Char => EMPTY,
            Type::Integer(_) => EMPTY,
            Type::Float(_) => EMPTY,
            Type::Bool => EMPTY,
            Type::Pointer => EMPTY,
            Type::Struct(struc) => {
                let mut result = EMPTY;
                for field in self.mem.getn(struc.fields).iter() {
                    result = result.add(self.type_variable_counts.get(field.type_id))
                }
                result
            }
            Type::Reference(refer) => *self.type_variable_counts.get(refer.inner_type),
            Type::Sum(e) => {
                let mut result = EMPTY;
                for v in self.mem.getn(e.variants) {
                    if let Some(payload) = v.payload {
                        result = result.add(self.type_variable_counts.get(payload));
                    }
                }
                result
            }
            Type::Opaque(_) => EMPTY,
            Type::Enum(_) => EMPTY,
            Type::Never => EMPTY,
            // The real answer here would be, all the type variables on the RHS that aren't one of
            // the params. In other words, all FREE type variables
            Type::Generic(_gen) => EMPTY,
            Type::Function(fun) => {
                let mut result = EMPTY;
                for param in self.mem.getn(fun.physical_params).iter() {
                    result = result.add(self.type_variable_counts.get(param.type_id))
                }
                result = result.add(self.type_variable_counts.get(fun.return_type));
                result
            }
            Type::FunctionPointer(fp) => *self.type_variable_counts.get(fp.function_type_id),
            Type::Lambda(lambda_id) => {
                let lambda = self.lambda_types.get(*lambda_id);
                self.type_variable_counts
                    .get(lambda.function_type)
                    .add(self.type_variable_counts.get(lambda.env_type))
            }
            // But a lambda object is generic if its function is generic
            Type::LambdaObject(co) => *self.type_variable_counts.get(co.function_type),
            Type::StaticValue(svt) => {
                let this = if svt.value_id.is_none() {
                    TypeVariableInfo {
                        inference_variable_count: 0,
                        type_parameter_count: 0,
                        unresolved_static_count: 1,
                    }
                } else {
                    EMPTY
                };
                let inner = self.type_variable_counts.get(svt.family_type_id);
                this.add(inner)
            }
            Type::Array(arr) => {
                // Arrays contain 2 types, the element type and the size type,
                // which is usually a `static uword`, but can be a type parameter
                self.type_variable_counts
                    .get(arr.element_type)
                    .add(self.type_variable_counts.get(arr.size_type))
            }
        }
    }

    // PHYSICAL TYPE STUFF /////////////////////////////////////////////////////

    pub fn get_pt_layout(&self, pt: PhysicalType) -> Layout {
        match pt.as_enum() {
            PhysicalTypeEnum::Empty => Layout::ZERO_SIZED,
            PhysicalTypeEnum::Scalar(s) => s.get_layout(),
            PhysicalTypeEnum::Agg(agg_id) => self.agg_types.get(agg_id).layout,
        }
    }

    pub fn get_value_of_static_type<'a>(
        &self,
        static_values: &'a StaticValuePool,
        type_id: TypeId,
    ) -> Option<&'a StaticValue> {
        match self.get(type_id) {
            Type::StaticValue(StaticValueType { value_id: Some(value_id), .. }) => {
                Some(static_values.get(*value_id))
            }
            _ => None,
        }
    }

    pub fn get_type_as_i64(
        &self,
        static_values: &StaticValuePool,
        size_type: TypeId,
    ) -> Option<i64> {
        match self.get_value_of_static_type(static_values, size_type) {
            Some(StaticValue::Int(TypedIntValue::I64(i))) => Some(*i),
            _ => None,
        }
    }

    pub fn compute_physical_type(
        &mut self,
        static_values: &StaticValuePool,
        type_id: TypeId,
    ) -> PhysicalTypeResult {
        match self.get(type_id) {
            Type::Char | Type::Bool => PhysicalTypeResult::Yes(PhysicalType::U8),

            Type::Integer(i) => {
                let st = i.get_scalar_type();
                PhysicalTypeResult::Yes(PhysicalType::scalar(st))
            }
            Type::Enum(se) => {
                let st = se.int_type.get_scalar_type();
                PhysicalTypeResult::Yes(PhysicalType::scalar(st))
            }
            Type::Float(FloatType::F32) => {
                PhysicalTypeResult::Yes(PhysicalType::scalar(ScalarType::F32))
            }
            Type::Float(FloatType::F64) => {
                PhysicalTypeResult::Yes(PhysicalType::scalar(ScalarType::F64))
            }
            Type::Pointer | Type::Reference(_) | Type::FunctionPointer(_) => {
                PhysicalTypeResult::Yes(PhysicalType::scalar(ScalarType::Pointer))
            }
            Type::Array(array) => {
                let count = self.get_type_as_i64(static_values, array.size_type);
                match count {
                    None => PhysicalTypeResult::No,
                    Some(0) => PhysicalTypeResult::Yes(PhysicalType::EMPTY),
                    Some(len) => match self.get_physical_type(static_values, array.element_type) {
                        PhysicalTypeResult::No => PhysicalTypeResult::No,
                        PhysicalTypeResult::Never => PhysicalTypeResult::Never,
                        PhysicalTypeResult::Infinite => PhysicalTypeResult::No,
                        PhysicalTypeResult::Yes(element_pt) => {
                            let elem_layout = self.get_pt_layout(element_pt);
                            let record = AggregateTypeRecord {
                                agg_type: AggType::Array { element_pt, len: len as u32 },
                                origin_type_id: type_id,
                                layout: elem_layout.array_me(len as usize),
                            };
                            let id = self.agg_types.add(record);
                            PhysicalTypeResult::Yes(PhysicalType::agg(id))
                        }
                    },
                }
            }
            Type::Struct(s) => match s.record_kind {
                RecordKind::Struct => {
                    let s_fields = s.fields;
                    let mut fields = self.mem.new_list(s.fields.len());
                    let mut layout = Layout::ZERO_SIZED;
                    for field in self.mem.getn(s_fields) {
                        match self.get_physical_type(static_values, field.type_id) {
                            PhysicalTypeResult::No => return PhysicalTypeResult::No,
                            PhysicalTypeResult::Never => return PhysicalTypeResult::Never,
                            PhysicalTypeResult::Infinite => return PhysicalTypeResult::Infinite,
                            PhysicalTypeResult::Yes(field_pt) => {
                                let field_layout = self.get_pt_layout(field_pt);
                                let offset = layout.append_to_aggregate(field_layout);
                                fields.push(StructField {
                                    field_t: field_pt,
                                    offset,
                                    name: field.name,
                                });
                            }
                        }
                    }
                    if layout.size == 0 {
                        PhysicalTypeResult::Yes(PhysicalType::EMPTY)
                    } else {
                        let fields_handle = self.mem.list_to_handle(fields);
                        let agg_id = self.agg_types.add(AggregateTypeRecord {
                            agg_type: AggType::Struct { fields: fields_handle },
                            origin_type_id: type_id,
                            layout,
                        });
                        PhysicalTypeResult::Yes(PhysicalType::agg(agg_id))
                    }
                }
                RecordKind::Union => {
                    let u_fields = s.fields;
                    let mut members = self.mem.new_list(u_fields.len());
                    for field in self.mem.getn(u_fields) {
                        match self.get_physical_type(static_values, field.type_id) {
                            PhysicalTypeResult::No => return PhysicalTypeResult::No,
                            PhysicalTypeResult::Never => return PhysicalTypeResult::Never,
                            PhysicalTypeResult::Infinite => return PhysicalTypeResult::Infinite,
                            PhysicalTypeResult::Yes(field_pt) => {
                                members.push(UnionMember { name: field.name, ty: field_pt });
                            }
                        }
                    }

                    let members_handle = self.mem.list_to_handle(members);
                    let union = self.make_union_type(type_id, members_handle);
                    PhysicalTypeResult::Yes(union)
                }
            },
            Type::Sum(e) => {
                let variant_count = e.variants.len();

                let tag_scalar = e.tag_type.get_scalar_type();
                let tag_layout = tag_scalar.get_layout();

                let mut physical_variants = self.mem.new_list(variant_count);
                let mut union_members = self.mem.new_list(variant_count);

                let e = self.get(type_id).expect_sum();

                for v in self.mem.getn(e.variants) {
                    if let Some(payload) = &v.payload {
                        match self.get_physical_type(static_values, *payload) {
                            PhysicalTypeResult::No => return PhysicalTypeResult::No,
                            PhysicalTypeResult::Never => {
                                // We simply skip this variant!
                                debug!("I am skipping this sum variant")
                            }
                            PhysicalTypeResult::Infinite => {
                                // We can never figure out a size large enough to hold all variants,
                                // since one variant is infinitely sized
                                return PhysicalTypeResult::No;
                            }
                            PhysicalTypeResult::Yes(payload_pt) => {
                                union_members.push(UnionMember { name: v.name, ty: payload_pt });

                                physical_variants.push(SumVariantPt {
                                    tag: v.tag_value,
                                    payload: Some(payload_pt),
                                });
                            }
                        }
                    } else {
                        physical_variants.push(SumVariantPt { tag: v.tag_value, payload: None });
                    }
                }

                let members_handle = self.mem.list_to_handle(union_members);
                let union_id = self.make_union_type(type_id, members_handle);
                let union_layout = self.get_pt_layout(union_id);

                let mut struct_layout = tag_layout;
                let tag_field = StructField {
                    offset: 0,
                    field_t: PhysicalType::scalar(tag_scalar),
                    name: self.idents.tag,
                };
                let union_offset = struct_layout.append_to_aggregate(union_layout);
                let payload_field = StructField {
                    offset: union_offset,
                    field_t: union_id,
                    name: self.idents.payload,
                };
                let fields = self.mem.pushn(&[tag_field, payload_field]);
                let struct_repr = self.agg_types.add(AggregateTypeRecord {
                    agg_type: AggType::Struct { fields },
                    origin_type_id: type_id,
                    layout: struct_layout,
                });
                let agg_sum = AggType::Sum(SumPt {
                    tag_type: tag_scalar,
                    struct_repr,
                    variants: self.mem.list_to_handle(physical_variants),
                    payload_offset: union_offset,
                });
                let sum_agg_id = self.agg_types.add(AggregateTypeRecord {
                    agg_type: agg_sum,
                    origin_type_id: type_id,
                    layout: struct_layout,
                });
                PhysicalTypeResult::Yes(PhysicalType::agg(sum_agg_id))
            }
            Type::Opaque(opaque) => {
                let opaque_agg_id = self.agg_types.add(AggregateTypeRecord {
                    agg_type: AggType::Opaque { size: opaque.size, align: opaque.align },
                    origin_type_id: type_id,
                    layout: opaque.layout(),
                });
                PhysicalTypeResult::Yes(PhysicalType::agg(opaque_agg_id))
            }
            Type::Lambda(lam_id) => {
                let lam = self.lambda_types.get(*lam_id);
                self.add_physical_duplicate(static_values, type_id, lam.env_type)
            }
            Type::LambdaObject(lam_obj) => {
                self.add_physical_duplicate(static_values, type_id, lam_obj.struct_representation)
            }
            Type::StaticValue(_vt) => PhysicalTypeResult::Yes(PhysicalType::EMPTY),
            Type::Never => PhysicalTypeResult::Never,
            Type::Function(_)
            | Type::Generic(_)
            | Type::TypeParameter(_)
            | Type::FunctionTypeParameter(_)
            | Type::InferenceHole(_) => PhysicalTypeResult::No,
        }
    }

    pub fn add_physical_duplicate(
        &mut self,
        static_values: &StaticValuePool,
        origin_type_id: TypeId,
        other: TypeId,
    ) -> PhysicalTypeResult {
        match self.get_physical_type(static_values, other) {
            PhysicalTypeResult::No => PhysicalTypeResult::No,
            PhysicalTypeResult::Never => PhysicalTypeResult::Never,
            PhysicalTypeResult::Infinite => PhysicalTypeResult::Infinite,
            orig @ PhysicalTypeResult::Yes(other_pt) => match other_pt.as_enum() {
                PhysicalTypeEnum::Empty => orig,
                PhysicalTypeEnum::Scalar(_) => orig,
                PhysicalTypeEnum::Agg(agg_id) => {
                    let r = self.agg_types.get(agg_id);
                    let r_new = AggregateTypeRecord {
                        agg_type: r.agg_type,
                        origin_type_id,
                        layout: r.layout,
                    };
                    let new_id = self.agg_types.add(r_new);
                    PhysicalTypeResult::Yes(PhysicalType::agg(new_id))
                }
            },
        }
    }

    // Works for enum variants too
    pub fn get_struct_field_offset(
        &self,
        agg_id: AggregateTypeId,
        field_index: u32,
    ) -> Option<u32> {
        match self.agg_types.get(agg_id).agg_type {
            AggType::Sum(e) => self.get_struct_field_offset(e.struct_repr, field_index),
            AggType::Struct { fields } => {
                if field_index < fields.len() {
                    let field_type = self.mem.get_nth(fields, field_index as usize);
                    Some(field_type.offset)
                } else {
                    None
                }
            }
            AggType::Array { .. } => None,
            AggType::Union { .. } => None,
            AggType::Opaque { .. } => None,
        }
    }

    pub fn get_agg_struct_layout(&self, struct_agg_id: AggregateTypeId) -> SV4<StructField> {
        match self.agg_types.get(struct_agg_id).agg_type {
            AggType::Sum(e) => self.get_agg_struct_layout(e.struct_repr),
            AggType::Struct { fields } => self.mem.getn_sv4(fields),
            AggType::Array { .. } => panic!("Array is not a struct"),
            AggType::Union { .. } => panic!("union has no struct-like layout"),
            AggType::Opaque { .. } => panic!("opaque has no struct-like layout"),
        }
    }

    pub fn get_sums_struct_layout(
        &self,
        sum_agg_id: AggregateTypeId,
    ) -> (StructField, Option<StructField>) {
        let struct_id = self.agg_types.get(sum_agg_id).agg_type.expect_sum().struct_repr;
        let fields = self.agg_types.get(struct_id).agg_type.expect_struct();
        let tag = *self.mem.get_nth(fields, 0);
        let payload = self.mem.get_nth_opt(fields, 1).copied();
        (tag, payload)
    }

    pub fn enum_value_by_name(
        &self,
        values: MSlice<ScalarEnumValue, TypePool>,
        name: StringId,
    ) -> Option<(usize, &'static ScalarEnumValue)> {
        self.mem.getn(values).iter().find_position(|v| v.name == name)
    }
    pub fn sum_variant_by_name(
        &self,
        variants: MSlice<TypedSumVariant, TypePool>,
        name: StringId,
    ) -> Option<&'static TypedSumVariant> {
        self.mem.getn(variants).iter().find(|v| v.name == name)
    }
    pub fn sum_variant_by_index(
        &self,
        variants: MSlice<TypedSumVariant, TypePool>,
        index: u32,
    ) -> &TypedSumVariant {
        self.mem.get_nth(variants, index as usize)
    }

    pub fn get_physical_type(
        &mut self,
        static_values: &StaticValuePool,
        type_id: TypeId,
    ) -> PhysicalTypeResult {
        match self.phys_types.get(&type_id) {
            Some(result) => *result,
            None => {
                let pt_result = self.compute_physical_type(static_values, type_id);
                self.phys_types.insert(type_id, pt_result);
                pt_result
            }
        }
    }

    pub fn get_layout_nonmut(&self, type_id: TypeId) -> Option<Layout> {
        match self.phys_types.get(&type_id) {
            Some(maybe_pt) => match maybe_pt {
                PhysicalTypeResult::No => None,
                PhysicalTypeResult::Never => None,
                PhysicalTypeResult::Infinite => None,
                PhysicalTypeResult::Yes(pt) => Some(self.get_pt_layout(*pt)),
            },
            None => None,
        }
    }

    pub fn get_layout(
        &mut self,
        static_values: &StaticValuePool,
        type_id: TypeId,
    ) -> Option<Layout> {
        match self.get_physical_type(static_values, type_id) {
            PhysicalTypeResult::No => None,
            PhysicalTypeResult::Never => None,
            PhysicalTypeResult::Infinite => None,
            PhysicalTypeResult::Yes(pt) => Some(self.get_pt_layout(pt)),
        }
    }

    pub fn get_as_list_instance(&self, type_id: TypeId) -> Option<ListType> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == self.builtins.list() {
                Some(ListType { element_type: *self.mem.get_nth(spec_info.type_args, 0) })
            } else {
                None
            }
        })
    }

    pub fn get_as_buffer_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == self.builtins.buffer() {
                Some(*self.mem.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_span_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == self.builtins.span() {
                Some(*self.mem.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_container_instance(&self, type_id: TypeId) -> Option<(TypeId, ContainerKind)> {
        if let Some(info) = self.get_instance_info(type_id) {
            if info.generic_parent == self.builtins.list() {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::List))
            } else if info.generic_parent == self.builtins.buffer() {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::Buffer))
            } else if info.generic_parent == self.builtins.span() {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::Span))
            } else {
                None
            }
        } else if let Type::Array(array_type) = self.types.get(type_id) {
            Some((array_type.element_type, ContainerKind::Array(type_id)))
        } else {
            None
        }
    }

    pub fn get_as_opt_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == self.builtins.opt() {
                Some(*self.mem.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_specialization(&self, base: TypeId, args: TypeIdSlice) -> Option<TypeId> {
        if let Some(specializations) = self.specializations.get(&base) {
            for candidate in specializations {
                if self.mem.slices_equal_copy(candidate.0, args) {
                    return Some(candidate.1);
                }
            }
        }
        None
    }

    /// Cache lookup against args that don't (yet) live in `self.mem`; lets callers
    /// avoid committing an args slice to the permanent arena until a cache miss
    pub fn get_specialization_slice(&self, base: TypeId, args: &[TypeId]) -> Option<TypeId> {
        if let Some(specializations) = self.specializations.get(&base) {
            for candidate in specializations {
                if self.mem.getn(candidate.0) == args {
                    return Some(candidate.1);
                }
            }
        }
        None
    }

    pub fn insert_specialization(&mut self, base: TypeId, args: TypeIdSlice, specialized: TypeId) {
        match self.specializations.entry(base) {
            Entry::Occupied(mut o) => o.get_mut().push((args, specialized)),
            Entry::Vacant(v) => {
                v.insert(vec![(args, specialized)]);
            }
        }
    }

    pub fn pt_to_string(&self, t: PhysicalType) -> String {
        let mut s = String::new();
        self.display_pt(&mut s, t).unwrap();
        s
    }

    pub fn display_ptp(&self, w: &mut impl std::fmt::Write, ptp: PhysicalType) -> std::fmt::Result {
        self.display_pt(w, ptp)
    }

    pub fn display_pt(&self, w: &mut impl std::fmt::Write, t: PhysicalType) -> std::fmt::Result {
        match t.as_enum() {
            PhysicalTypeEnum::Empty => w.write_str("{}"),
            PhysicalTypeEnum::Scalar(st) => write!(w, "{}", st),
            PhysicalTypeEnum::Agg(agg) => match self.agg_types.get(agg).agg_type {
                AggType::Sum(e) => {
                    w.write_str("sum ")?;
                    self.display_pt(w, PhysicalType::agg(e.struct_repr))?;
                    Ok(())
                }
                AggType::Struct { fields } => {
                    w.write_str("{ ")?;
                    for (index, field) in self.mem.getn(fields).iter().enumerate() {
                        self.display_pt(w, field.field_t)?;
                        let last = index == fields.len() as usize - 1;
                        if !last {
                            w.write_str(", ")?;
                        }
                    }
                    w.write_str(" }")?;
                    Ok(())
                }
                AggType::Array { len, element_pt: t } => {
                    w.write_str("[")?;
                    self.display_pt(w, t)?;
                    write!(w, " x {}]", len)?;
                    Ok(())
                }
                AggType::Union { members } => {
                    write!(w, "union {{ ")?;
                    for (index, m) in self.mem.getn(members).iter().enumerate() {
                        self.display_pt(w, m.ty)?;
                        let is_last = index == members.len() as usize - 1;
                        if !is_last {
                            write!(w, ", ")?;
                        }
                    }
                    write!(w, " }}")?;
                    Ok(())
                }
                AggType::Opaque { size, align } => {
                    write!(w, "opaque[{}, {}]", size, align)
                }
            },
        }
    }
}

// static mut MAX_VISITED: usize = 0;

// Talks about the 4 kinds of contiguous 'collection' / 'container' types that
// the compiler knows about
pub enum ContainerKind {
    Array(TypeId),
    Buffer,
    Span,
    List,
}
