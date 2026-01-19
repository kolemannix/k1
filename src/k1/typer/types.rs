// Copyright (c) 2025 knix
// All rights reserved.

use std::collections::hash_map::Entry;
use std::fmt::{Display, Formatter};
use std::hash::Hasher;

use fxhash::FxHashMap;

use crate::typer::scopes::*;

use crate::parse::{Ident, IdentPool, ParsedId, ParsedTypeDefnId};

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
    pub name: Ident,
    pub type_id: TypeId,
}
impl_copy_if_small!(8, StructTypeField);

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
pub struct TypeDefnInfo {
    pub name: Ident,
    pub scope: ScopeId,
    pub companion_namespace: Option<NamespaceId>,
    pub ast_id: ParsedId,
}
impl_copy_if_small!(20, TypeDefnInfo);

impl std::hash::Hash for TypeDefnInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.scope.hash(state);
    }
}

#[derive(Clone)]
pub struct StructType {
    pub fields: MSlice<StructTypeField, TypePool>,
}

impl StructType {
    pub fn find_field(
        &self,
        m: &kmem::Mem<TypePool>,
        field_name: Ident,
    ) -> Option<(usize, &StructTypeField)> {
        m.getn(self.fields).iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const EMPTY_ID: TypeId = TypeId(NonZeroU32::new(1).unwrap());
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

pub const BUFFER_DATA_FIELD_NAME: &str = "data";
pub const BUFFER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(16).unwrap());

pub const VIEW_TYPE_ID: TypeId = TypeId(NonZeroU32::new(17).unwrap());

pub const LIST_TYPE_ID: TypeId = TypeId(NonZeroU32::new(18).unwrap());
pub const STRING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(19).unwrap());
pub const OPTIONAL_TYPE_ID: TypeId = TypeId(NonZeroU32::new(20).unwrap());
pub const COMPILER_SOURCE_LOC_TYPE_ID: TypeId = TypeId(NonZeroU32::new(21).unwrap());
pub const ORDERING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(22).unwrap());

pub const _RESULT_TYPE_ID: TypeId = TypeId(NonZeroU32::new(23).unwrap());
//pub const TYPE_SCHEMA_TYPE_ID: TypeId = TypeId(NonZeroU32::new(39).unwrap());

#[derive(Clone)]
pub struct ListType {
    pub element_type: TypeId,
}

#[derive(Clone)]
pub struct TypeParameter {
    pub name: Ident,
    pub static_constraint: Option<TypeId>,
    pub scope_id: ScopeId,
    pub span: SpanId,
}
impl_copy_if_small!(16, TypeParameter);

#[derive(Clone)]
pub struct FunctionTypeParameter {
    pub name: Ident,
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
    pub mutable: bool,
}
impl ReferenceType {
    pub fn is_read_only(&self) -> bool {
        !self.mutable
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }
}

#[derive(Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
    pub size_type: TypeId,
    pub concrete_count: Option<u64>,
}
impl_copy_if_small!(24, ArrayType);

#[derive(Clone, Copy)]
pub struct TypedEnumVariant {
    pub name: Ident,
    pub index: u32,
    pub payload: Option<TypeId>,
    pub tag_value: TypedIntValue,
}

#[derive(Clone)]
pub struct EnumType {
    pub variants: MSlice<TypedEnumVariant, TypePool>,
    pub ast_node: ParsedId,
    pub tag_type: TypeId,
}

impl EnumType {}

#[derive(Clone)]
pub struct GenericType {
    pub params: SliceHandle<NameAndTypeId>,
    pub inner: TypeId,
}

impl GenericType {}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IntegerType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
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
        PhysicalType::Scalar(self.get_scalar_type())
    }
}

#[derive(Clone, Copy)]
pub enum FloatType {
    F32,
    F64,
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
    // FIXME: Determine if param names are truly 'part' of the function type.
    // For now, keeping them to fix some bugs
    pub name: Ident,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_lambda_env: bool,
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

#[derive(Clone)]
pub struct RecursiveReference {
    pub root_type_id: TypeId,
    //pub type_args: SV4<TypeId>,
}

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
pub struct StaticType {
    pub family_type_id: TypeId,
    pub value_id: Option<StaticValueId>,
}

#[derive(Clone, Copy)]
pub struct FunctionPointerType {
    pub function_type_id: TypeId,
}

static_assert_size!(Type, 32);
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
    Reference(ReferenceType),
    Array(ArrayType),
    Struct(StructType),
    Enum(EnumType),

    /// An uninhabited type; used to indicate divergent control flow
    Never,
    Function(FunctionType),
    /// Function pointers deserve to be represented differently than just a Reference to a function type
    /// Otherwise, function pointers become a special case of references almost
    /// everywhere, since they can't be de-referenced and don't point to a physical k1 type
    FunctionPointer(FunctionPointerType),
    Lambda(LambdaType),
    LambdaObject(LambdaObjectType),

    Static(StaticType),

    // Not-so-physical types
    Generic(GenericType),
    #[allow(clippy::enum_variant_names)]
    TypeParameter(TypeParameter),
    FunctionTypeParameter(FunctionTypeParameter),
    InferenceHole(InferenceHoleType),
    Unresolved(ParsedTypeDefnId),
    /// A recursive reference to the type in which it appears
    /// Also used for Co-recursive references, e.g.:
    /// deftype Red = { b: Black* }
    /// deftype Black = { r: Red* }
    RecursiveReference(RecursiveReference),
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
            (Type::Reference(r1), Type::Reference(r2)) => {
                r1.inner_type == r2.inner_type && r1.mutable == r2.mutable
            }
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
            (Type::Enum(e1), Type::Enum(e2)) => {
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
                        .all(|(p1, p2)| p1.type_id == p2.type_id)
                } else {
                    false
                }
            }
            (Type::FunctionPointer(fp1), Type::FunctionPointer(fp2)) => {
                fp1.function_type_id == fp2.function_type_id
            }
            (Type::Lambda(c1), Type::Lambda(c2)) => {
                // The function type is key here so that we _dont_ equate 'inference artifact' lambdas
                // with real ones: '0 -> '1 vs int -> bool
                c1.function_id == c2.function_id
                    && c1.function_type == c2.function_type
                    && c1.parsed_id == c2.parsed_id
            }
            (Type::LambdaObject(_co1), Type::LambdaObject(_co2)) => false,
            (Type::Static(stat1), Type::Static(stat2)) => stat1.value_id == stat2.value_id,
            (Type::RecursiveReference(rr1), Type::RecursiveReference(rr2)) => {
                rr1.root_type_id == rr2.root_type_id
            }
            (Type::Unresolved(ur1), Type::Unresolved(ur2)) => ur1 == ur2,
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
            Type::Bool => {}
            Type::Struct(s) => {
                defn.hash(state);
                s.fields.len().hash(state);
                for f in self.mem.getn(s.fields) {
                    f.name.hash(state);
                    f.type_id.hash(state);
                }
            }
            Type::Reference(r) => {
                r.inner_type.hash(state);
                r.mutable.hash(state);
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
            Type::Enum(e) => {
                defn.hash(state);
                e.variants.len().hash(state);
                for v in self.mem.getn(e.variants) {
                    v.name.hash(state);
                    v.payload.hash(state);
                }
            }
            // Inherently unique as well
            Type::Generic(generic) => {
                generic.inner.hash(state);
                generic.params.index().hash(state);
                generic.params.len().hash(state);
            }
            Type::Function(fun) => {
                fun.return_type.hash(state);
                for param in self.mem.getn(fun.physical_params) {
                    param.name.hash(state);
                    param.is_context.hash(state);
                    param.is_lambda_env.hash(state);
                    param.type_id.hash(state);
                }
            }
            Type::FunctionPointer(fp) => fp.function_type_id.hash(state),
            Type::Lambda(c) => {
                c.parsed_id.hash(state);
                c.function_type.hash(state);
                c.function_id.hash(state);
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
            Type::Static(stat) => {
                stat.family_type_id.hash(state);
                stat.value_id.hash(state)
            }
            Type::Unresolved(id) => {
                id.hash(state);
            }
            Type::RecursiveReference(rr) => {
                rr.root_type_id.hash(state);
            }
            Type::Array(arr) => {
                arr.element_type.hash(state);
                arr.size_type.hash(state);
                arr.concrete_count.hash(state);
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
            Type::Enum(_) => "enum",
            Type::Never => "never",
            Type::Generic(_) => "generic",
            Type::Function(_) => "function",
            Type::FunctionPointer(_) => "function_ptr",
            Type::Lambda(_) => "lambda",
            Type::LambdaObject(_) => "lambdaobj",
            Type::Static(_) => "static",
            Type::Unresolved(_) => "unresolved",
            Type::RecursiveReference(_) => "recurse",
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

    pub fn as_static(&self) -> Option<&StaticType> {
        match self {
            Type::Static(s) => Some(s),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_enum_mut(&mut self) -> &mut EnumType {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum type"),
        }
    }

    pub fn as_enum(&self) -> Option<&EnumType> {
        match self {
            Type::Enum(e) => Some(e),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_enum(&self) -> &EnumType {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum on {}", self.kind_name()),
        }
    }

    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            Type::Struct(struc) => Some(struc),
            _ => None,
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

    #[track_caller]
    pub fn expect_recursive_reference(&mut self) -> &mut RecursiveReference {
        match self {
            Type::RecursiveReference(r) => r,
            _ => panic!("expected recursive reference"),
        }
    }

    pub fn as_function(&self) -> Option<&FunctionType> {
        match self {
            Type::Function(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> Option<&LambdaType> {
        match self {
            Type::Lambda(c) => Some(c),
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

    pub fn as_unresolved(&self) -> Option<ParsedTypeDefnId> {
        match self {
            Type::Unresolved(id) => Some(*id),
            _ => None,
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

    fn add(self, other: Self) -> Self {
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
    pub string: Option<TypeId>,
    pub buffer: Option<TypeId>,
    pub dyn_lambda_obj: Option<TypeId>,
    pub types_layout: Option<TypeId>,
    pub types_type_schema: Option<TypeId>,
    pub types_int_kind: Option<TypeId>,
    pub types_int_value: Option<TypeId>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Pointer,
}

impl ScalarType {
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

#[derive(Clone, Copy)]
pub enum PhysicalType {
    Scalar(ScalarType),
    Agg(AggregateTypeId),
    /// All zero-sized type ids should compile to the Empty variant, so zero-sized behavior is
    /// handled consistently everywhere
    Empty,
}

impl PhysicalType {
    pub fn is_agg(&self) -> bool {
        matches!(self, PhysicalType::Agg(_))
    }

    #[track_caller]
    pub fn expect_agg(&self) -> AggregateTypeId {
        match self {
            PhysicalType::Agg(id) => *id,
            _ => panic!("Expected agg"),
        }
    }

    #[track_caller]
    pub fn expect_scalar(&self) -> ScalarType {
        match self {
            PhysicalType::Scalar(s) => *s,
            _ => panic!("Expected scalar"),
        }
    }
    pub fn as_scalar(&self) -> Option<ScalarType> {
        match self {
            PhysicalType::Scalar(s) => Some(*s),
            _ => None,
        }
    }

    pub fn is_scalar(&self) -> bool {
        matches!(self, PhysicalType::Scalar(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, PhysicalType::Empty)
    }
}

#[derive(Clone, Copy)]
pub struct StructField {
    pub offset: u32,
    pub field_t: PhysicalType,
    pub name: Ident,
}

#[derive(Clone, Copy)]
pub struct EnumPt {
    pub tag_type: ScalarType,
    pub struct_repr: AggregateTypeId,
    pub variants: MSlice<EnumVariantPt, TypePool>,
    pub payload_offset: Option<u32>,
}

#[derive(Clone, Copy)]
pub struct EnumVariantPt {
    pub tag: TypedIntValue,
    pub payload: Option<PhysicalType>,
}

#[derive(Clone, Copy)]
pub struct UnionMember {
    pub name: Ident,
    pub ty: PhysicalType,
}

#[derive(Clone, Copy)]
pub enum AggType {
    Struct { fields: MSlice<StructField, TypePool> },
    Array { element_pt: PhysicalType, len: u32 },
    Union { members: MSlice<UnionMember, TypePool> },
    Enum(EnumPt),
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
    pub fn expect_enum(&self) -> &EnumPt {
        match self {
            AggType::Enum(e) => e,
            _ => panic!("Expected enum agg type"),
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
    tag: Ident,
    payload: Ident,
}

pub struct TypePool {
    pub types: VPool<Type, TypeId>,
    /// We use this to efficiently check if we already have seen a type,
    /// and retrieve its ID if so. We used to iterate the pool but it
    /// got slow
    pub hashes: FxHashMap<u64, TypeId>,

    /// AoS-style info associated with each type id
    pub type_phys_type_lookup: VPool<Option<PhysicalType>, TypeId>,
    pub type_variable_counts: VPool<TypeVariableInfo, TypeId>,
    pub instance_info: VPool<Option<GenericInstanceInfo>, TypeId>,

    pub defn_info: FxHashMap<TypeId, TypeDefnInfo>,
    pub specializations: FxHashMap<TypeId, Vec<(TypeIdSlice, TypeId)>>,

    /// Lookup mappings for parsed -> typed ids
    pub ast_type_defn_mapping: FxHashMap<ParsedTypeDefnId, TypeId>,
    pub ast_ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,

    pub builtins: BuiltinTypes,

    pub agg_types: VPool<AggregateTypeRecord, AggregateTypeId>,

    pub mem: kmem::Mem<TypePool>,

    pub idents: TypePoolIdents,
}

impl TypePool {
    pub fn empty(tag_ident: Ident, payload_ident: Ident) -> TypePool {
        const EXPECTED_TYPE_COUNT: usize = 65536;
        TypePool {
            types: VPool::make_with_hint("types", EXPECTED_TYPE_COUNT),
            hashes: FxHashMap::with_capacity(EXPECTED_TYPE_COUNT),

            type_phys_type_lookup: VPool::make_with_hint(
                "type_phys_type_lookup",
                EXPECTED_TYPE_COUNT,
            ),
            type_variable_counts: VPool::make_with_hint(
                "type_variable_counts",
                EXPECTED_TYPE_COUNT,
            ),
            instance_info: VPool::make_with_hint("instance_info", EXPECTED_TYPE_COUNT),

            defn_info: FxHashMap::default(),
            specializations: FxHashMap::default(),

            ast_type_defn_mapping: FxHashMap::default(),
            ast_ability_mapping: FxHashMap::default(),

            builtins: BuiltinTypes::default(),

            agg_types: VPool::make_with_hint("phys_types", EXPECTED_TYPE_COUNT / 2),

            mem: kmem::Mem::make(),

            idents: TypePoolIdents { tag: tag_ident, payload: payload_ident },
        }
    }

    #[cfg(test)]
    pub fn with_builtin_types() -> TypePool {
        let mut this = TypePool::empty(Ident::forged(), Ident::forged());
        this.add_anon(Type::Integer(IntegerType::U8));
        this.add_anon(Type::Integer(IntegerType::U16));
        this.add_anon(Type::Integer(IntegerType::U32));
        this.add_anon(Type::Integer(IntegerType::U64));
        this.add_anon(Type::Integer(IntegerType::I8));
        this.add_anon(Type::Integer(IntegerType::I16));
        this.add_anon(Type::Integer(IntegerType::I32));
        this.add_anon(Type::Integer(IntegerType::I64));

        this.add_anon(Type::Struct(StructType { fields: MSlice::empty() }));
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

        // 3 AoS fields to handle
        // pub type_phys_type_lookup
        // pub type_variable_counts
        // pub instance_info

        let pt_id = self.compile_physical_type(type_id);
        self.type_phys_type_lookup.add(pt_id);

        let variable_counts = self.count_type_variables(type_id);
        self.type_variable_counts.add(variable_counts);

        self.instance_info.add(instance_info);

        if let Some(defn_info) = defn_info {
            self.defn_info.insert(type_id, defn_info);
        }

        debug_assert_eq!(self.type_variable_counts.len(), self.types.len());
        debug_assert_eq!(self.instance_info.len(), self.types.len());
        debug_assert_eq!(self.instance_info.len(), self.type_phys_type_lookup.len());

        type_id
    }

    pub fn add_unresolved_type_defn(
        &mut self,
        parsed_id: ParsedTypeDefnId,
        info: TypeDefnInfo,
    ) -> TypeId {
        let type_id = self.add(Type::Unresolved(parsed_id), Some(info), None);
        self.ast_type_defn_mapping.insert(parsed_id, type_id);
        type_id
    }

    pub fn resolve_unresolved(
        &mut self,
        unresolved_type_id: TypeId,
        type_value: Type,
        instance_info: Option<GenericInstanceInfo>,
    ) {
        let defn_info = self.defn_info.get(&unresolved_type_id).copied();
        let hash = self.hash_type(&type_value, defn_info);
        let typ = self.get_mut(unresolved_type_id);
        if typ.as_unresolved().is_none() {
            panic!("Tried to resolve a type that was not unresolved: {}", typ.kind_name());
        }
        *typ = type_value;
        self.hashes.insert(hash, unresolved_type_id);
        // Adding a type is a mess, since we have all these places to update
        // and we have to do it differently if we're resolving vs adding new.
        // ...
        // Update: HARD AGREE 4 months later!
        // Checklist is:
        // - manage the hash
        // - Update the 3 SoA fields: variable counts, phys_type_mapping, and instance_info
        // - Manage both the resolve vs insert paths
        // - Handle enums since they are self-referential
        //
        // Update: Mostly fixed this by making enum variants not their own types

        let variable_counts = self.count_type_variables(unresolved_type_id);
        *self.type_variable_counts.get_mut(unresolved_type_id) = variable_counts;
        *self.instance_info.get_mut(unresolved_type_id) = instance_info;

        let pt_id = self.compile_physical_type(unresolved_type_id);
        *self.type_phys_type_lookup.get_mut(unresolved_type_id) = pt_id;
    }

    pub fn next_type_id(&self) -> TypeId {
        // Safety: If you add one to a u32 it'll never be zero.
        // Community Notes: Not true, it can wrap!
        unsafe { TypeId(NonZeroU32::new_unchecked(self.types.len() as u32 + 1)) }
    }

    pub fn add_reference_type(&mut self, inner_type: TypeId, mutable: bool) -> TypeId {
        self.add_anon(Type::Reference(ReferenceType { inner_type, mutable }))
    }

    pub fn add_function_pointer_type(&mut self, function_type_id: TypeId) -> TypeId {
        self.add_anon(Type::FunctionPointer(FunctionPointerType { function_type_id }))
    }

    fn make_union_type(
        &mut self,
        origin_type_id: TypeId,
        members: MSlice<UnionMember, TypePool>,
    ) -> AggregateTypeId {
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
        self.agg_types.add(AggregateTypeRecord {
            agg_type: AggType::Union { members },
            origin_type_id,
            layout: union_layout,
        })
    }

    pub fn add_static_type(
        &mut self,
        family_type_id: TypeId,
        value_id: Option<StaticValueId>,
    ) -> TypeId {
        self.add_anon(Type::Static(StaticType { family_type_id, value_id }))
    }

    pub fn add_anon(&mut self, typ: Type) -> TypeId {
        self.add(typ, None, None)
    }

    #[inline]
    pub fn get_no_follow(&self, type_id: TypeId) -> &Type {
        self.types.get(type_id)
    }

    #[inline]
    pub fn get_no_follow_static(&self, type_id: TypeId) -> &Type {
        match self.get_no_follow(type_id) {
            Type::RecursiveReference(rr) => self.get_no_follow_static(rr.root_type_id),
            t => t,
        }
    }

    /// The two types of types that we need to treat as 'static' types are Static types themselves
    /// and type parameters with a constraint to a specific static, which is basically the same
    /// thing since it can have no other constraints
    pub fn get_static_type_id_of_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.get_no_follow_static(type_id) {
            Type::Static(_st) => Some(type_id),
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                let t = tp.static_constraint.unwrap();
                debug_assert!(self.get_no_follow(t).as_static().is_some());
                Some(t)
            }
            Type::InferenceHole(ih) if ih.static_type.is_some() => {
                let t = ih.static_type.unwrap();
                debug_assert!(self.get_no_follow(t).as_static().is_some());
                Some(t)
            }
            _ => None,
        }
    }

    pub fn get_static_type_of_type(&self, type_id: TypeId) -> Option<StaticType> {
        match self.get_static_type_id_of_type(type_id) {
            None => None,
            Some(type_id) => Some(*self.get_no_follow_static(type_id).as_static().unwrap()),
        }
    }

    pub fn is_static(&self, type_id: TypeId) -> bool {
        self.get_static_type_id_of_type(type_id).is_some()
    }

    #[inline]
    /// Its important to understand that this basic 'get type' follows
    /// redirects for both recursives and statics. This is because 99% of
    /// code wants to treat them as their contained type
    /// And the other 1% is the code that explicitly is checking for Static or RecursiveReference,
    /// and will be forced to call get_no_follow in order to achieve its goal anyway
    pub fn get(&self, type_id: TypeId) -> &Type {
        match self.get_no_follow(type_id) {
            Type::RecursiveReference(rr) => self.get(rr.root_type_id),
            Type::Static(stat) => self.get(stat.family_type_id),
            t => t,
        }
    }

    pub fn get_struct_field(&self, type_id: TypeId, field_index: usize) -> &StructTypeField {
        self.mem.get_nth(self.get(type_id).expect_struct().fields, field_index)
    }

    pub fn get_struct_field_by_name(
        &self,
        type_id: TypeId,
        name: Ident,
    ) -> Option<(usize, &StructTypeField)> {
        self.get(type_id).expect_struct().find_field(&self.mem, name)
    }

    pub fn get_chased_id(&self, type_id: TypeId) -> TypeId {
        match self.get_no_follow(type_id) {
            Type::RecursiveReference(rr) => rr.root_type_id,
            Type::Static(stat) => stat.family_type_id,
            _ => type_id,
        }
    }

    pub fn get_base_for_method(&self, type_id: TypeId) -> TypeId {
        // Follow references
        let static_chased = match self.get_no_follow(type_id) {
            Type::Reference(r) => r.inner_type,
            _ => type_id,
        };

        // Then follow statics
        match self.get_no_follow(static_chased) {
            Type::RecursiveReference(rr) => rr.root_type_id,
            Type::Static(stat) => stat.family_type_id,
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                self.get_static_type_of_type(tp.static_constraint.unwrap()).unwrap().family_type_id
            }
            _ => static_chased,
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

    pub fn get_as_enum(&self, type_id: TypeId) -> Option<&EnumType> {
        match self.get(type_id) {
            Type::Enum(e) => Some(e),
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
        let lambda_type_id = self.add_anon(Type::Lambda(LambdaType {
            function_type: function_type_id,
            env_type: environment_type,
            parsed_id,
            function_id: body_function_id,
            environment_struct,
        }));
        lambda_type_id
    }

    pub fn add_empty_struct(&mut self) -> TypeId {
        self.add_anon(Type::Struct(StructType { fields: MSlice::empty() }))
    }

    pub const LAMBDA_OBJECT_FN_PTR_INDEX: usize = 0;
    pub const LAMBDA_OBJECT_ENV_PTR_INDEX: usize = 1;

    pub fn add_lambda_object(
        &mut self,
        identifiers: &IdentPool,
        function_type_id: TypeId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let fn_ptr_type = self.add_function_pointer_type(function_type_id);
        let fields = self.mem.pushn(&[
            StructTypeField { name: identifiers.b.fn_ptr, type_id: fn_ptr_type },
            StructTypeField { name: identifiers.b.env_ptr, type_id: POINTER_TYPE_ID },
        ]);
        let struct_representation = self.add_anon(Type::Struct(StructType { fields }));
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

    pub fn find_type_defn_mapping(&mut self, type_defn_id: ParsedTypeDefnId) -> Option<TypeId> {
        self.ast_type_defn_mapping.get(&type_defn_id).copied()
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

    pub fn get_contained_type_variable_counts(&self, type_id: TypeId) -> TypeVariableInfo {
        *self.type_variable_counts.get(type_id)
    }

    /// Recursively checks if given type contains any type variables
    /// Note: We could cache whether or not a type is generic on insertion into the type pool
    ///       But types are not immutable so this could be a dangerous idea!
    pub fn count_type_variables(&self, type_id: TypeId) -> TypeVariableInfo {
        const EMPTY: TypeVariableInfo = TypeVariableInfo::EMPTY;
        debug!("count_type_variables of {} {}", type_id, self.get_no_follow(type_id).kind_name());
        match self.get_no_follow(type_id) {
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
                let fn_info = self.count_type_variables(ftp.function_type);
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
                    result = result.add(self.count_type_variables(field.type_id))
                }
                result
            }
            Type::Reference(refer) => self.count_type_variables(refer.inner_type),
            Type::Enum(e) => {
                let mut result = EMPTY;
                for v in self.mem.getn(e.variants) {
                    if let Some(payload) = v.payload {
                        result = result.add(self.count_type_variables(payload));
                    }
                }
                result
            }
            Type::Never => EMPTY,
            // The real answer here would be, all the type variables on the RHS that aren't one of
            // the params. In other words, all FREE type variables
            Type::Generic(_gen) => EMPTY,
            Type::Function(fun) => {
                let mut result = EMPTY;
                for param in self.mem.getn(fun.physical_params).iter() {
                    result = result.add(self.count_type_variables(param.type_id))
                }
                result = result.add(self.count_type_variables(fun.return_type));
                result
            }
            Type::FunctionPointer(fp) => self.count_type_variables(fp.function_type_id),
            Type::Lambda(lambda) => self
                .count_type_variables(lambda.function_type)
                .add(self.count_type_variables(lambda.env_type)),
            // But a lambda object is generic if its function is generic
            Type::LambdaObject(co) => self.count_type_variables(co.function_type),
            Type::Static(stat) => {
                let this = if stat.value_id.is_none() {
                    TypeVariableInfo {
                        inference_variable_count: 0,
                        type_parameter_count: 0,
                        unresolved_static_count: 1,
                    }
                } else {
                    EMPTY
                };
                let inner = self.count_type_variables(stat.family_type_id);
                this.add(inner)
            }
            Type::Unresolved(_) => EMPTY,
            Type::RecursiveReference(rr) => {
                if let Type::Generic(generic) = self.get(rr.root_type_id) {
                    TypeVariableInfo {
                        inference_variable_count: 0,
                        type_parameter_count: generic.params.len() as u32,
                        unresolved_static_count: 0,
                    }
                } else {
                    EMPTY
                }
            }
            Type::Array(arr) => {
                // Arrays contain 2 types, the element type and the size type,
                // which is usually a `static uword`, but can be a type parameter
                self.count_type_variables(arr.element_type)
                    .add(self.count_type_variables(arr.size_type))
            }
        }
    }

    // PHYSICAL TYPE STUFF /////////////////////////////////////////////////////

    pub fn get_pt_layout(&self, pt: PhysicalType) -> Layout {
        match pt {
            PhysicalType::Empty => Layout::ZERO_SIZED,
            PhysicalType::Scalar(s) => s.get_layout(),
            PhysicalType::Agg(agg_id) => self.agg_types.get(agg_id).layout,
        }
    }

    pub fn compile_physical_type(&mut self, type_id: TypeId) -> Option<PhysicalType> {
        match self.get_no_follow_static(type_id) {
            Type::Char | Type::Bool => Some(PhysicalType::Scalar(ScalarType::U8)),

            Type::Integer(i) => {
                let st = i.get_scalar_type();
                Some(PhysicalType::Scalar(st))
            }

            Type::Float(FloatType::F32) => Some(PhysicalType::Scalar(ScalarType::F32)),
            Type::Float(FloatType::F64) => Some(PhysicalType::Scalar(ScalarType::F64)),
            Type::Pointer | Type::Reference(_) | Type::FunctionPointer(_) => {
                Some(PhysicalType::Scalar(ScalarType::Pointer))
            }
            Type::Array(array) => match self.get_physical_type(array.element_type) {
                None => None,
                Some(element_t) => match array.concrete_count {
                    None => None,
                    Some(len) => {
                        if len == 0 {
                            Some(PhysicalType::Empty)
                        } else {
                            let elem_layout = self.get_pt_layout(element_t);
                            let record = AggregateTypeRecord {
                                agg_type: AggType::Array { element_pt: element_t, len: len as u32 },
                                origin_type_id: type_id,
                                layout: elem_layout.array_me(len as usize),
                            };
                            let id = self.agg_types.add(record);
                            Some(PhysicalType::Agg(id))
                        }
                    }
                },
            },
            Type::Struct(s) => {
                let s_fields = s.fields;
                let mut fields = self.mem.new_list(s.fields.len());
                let mut layout = Layout::ZERO_SIZED;
                let mut not_physical = false;
                for field in self.mem.getn(s_fields) {
                    if not_physical {
                        continue;
                    }
                    match self.get_physical_type(field.type_id) {
                        None => {
                            not_physical = true;
                        }
                        Some(field_pt) => {
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
                if not_physical {
                    None
                } else {
                    if layout.size == 0 {
                        Some(PhysicalType::Empty)
                    } else {
                        let fields_handle = self.mem.list_to_handle(fields);
                        let agg_id = self.agg_types.add(AggregateTypeRecord {
                            agg_type: AggType::Struct { fields: fields_handle },
                            origin_type_id: type_id,
                            layout,
                        });
                        Some(PhysicalType::Agg(agg_id))
                    }
                }
            }
            Type::Enum(e) => {
                let variant_count = e.variants.len();

                let tag_scalar = self.get_physical_type(e.tag_type).unwrap().expect_scalar();
                let tag_layout = tag_scalar.get_layout();

                let mut physical_variants = self.mem.new_list(variant_count);
                let mut union_members = self.mem.new_list(variant_count);
                let mut is_physical = true;

                let e = self.get(type_id).expect_enum();

                for v in self.mem.getn(e.variants) {
                    if let Some(payload) = &v.payload {
                        match self.get_physical_type(*payload) {
                            None => {
                                is_physical = false;
                            }
                            Some(payload_pt) => {
                                union_members.push(UnionMember { name: v.name, ty: payload_pt });

                                physical_variants.push(EnumVariantPt {
                                    tag: v.tag_value,
                                    payload: Some(payload_pt),
                                });
                            }
                        }
                    } else {
                        physical_variants.push(EnumVariantPt { tag: v.tag_value, payload: None });
                    }
                }

                if is_physical {
                    let members_handle = self.mem.list_to_handle(union_members);
                    let union_id = self.make_union_type(type_id, members_handle);
                    let union_layout = self.get_pt_layout(PhysicalType::Agg(union_id));

                    let mut struct_layout = tag_layout;
                    let tag_field = StructField {
                        offset: 0,
                        field_t: PhysicalType::Scalar(tag_scalar),
                        name: self.idents.tag,
                    };
                    let (struct_fields, union_offset) = if members_handle.is_empty() {
                        (self.mem.pushn(&[tag_field]), None)
                    } else {
                        let union_offset = struct_layout.append_to_aggregate(union_layout);
                        let payload_field = StructField {
                            offset: union_offset,
                            field_t: PhysicalType::Agg(union_id),
                            name: self.idents.payload,
                        };
                        let fields = self.mem.pushn(&[tag_field, payload_field]);
                        (fields, Some(union_offset))
                    };
                    let struct_repr = self.agg_types.add(AggregateTypeRecord {
                        agg_type: AggType::Struct { fields: struct_fields },
                        origin_type_id: type_id,
                        layout: struct_layout,
                    });
                    let agg_enum = AggType::Enum(EnumPt {
                        tag_type: tag_scalar,
                        struct_repr,
                        variants: self.mem.list_to_handle(physical_variants),
                        payload_offset: union_offset,
                    });
                    let enum_agg_id = self.agg_types.add(AggregateTypeRecord {
                        agg_type: agg_enum,
                        origin_type_id: type_id,
                        layout: struct_layout,
                    });
                    Some(PhysicalType::Agg(enum_agg_id))
                } else {
                    None
                }
            }
            Type::Lambda(lam) => self.add_physical_duplicate(type_id, lam.env_type),
            Type::LambdaObject(lam_obj) => {
                self.add_physical_duplicate(type_id, lam_obj.struct_representation)
            }
            Type::Static(stat) => {
                // Re-use the inner physical type
                self.add_physical_duplicate(type_id, stat.family_type_id)
            }
            Type::Never => None,
            Type::Function(_)
            | Type::Generic(_)
            | Type::TypeParameter(_)
            | Type::FunctionTypeParameter(_)
            | Type::InferenceHole(_)
            | Type::Unresolved(_)
            | Type::RecursiveReference(_) => None,
        }
    }

    pub fn add_physical_duplicate(
        &mut self,
        origin_type_id: TypeId,
        other: TypeId,
    ) -> Option<PhysicalType> {
        match self.get_physical_type(other) {
            None => None,
            Some(other_pt) => match other_pt {
                pt @ PhysicalType::Empty => Some(pt),
                pt @ PhysicalType::Scalar(_) => Some(pt),
                PhysicalType::Agg(agg_id) => {
                    let r = self.agg_types.get(agg_id);
                    let r_new = AggregateTypeRecord {
                        agg_type: r.agg_type,
                        origin_type_id,
                        layout: r.layout,
                    };
                    let new_id = self.agg_types.add(r_new);
                    Some(PhysicalType::Agg(new_id))
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
            AggType::Enum(e) => self.get_struct_field_offset(e.struct_repr, field_index),
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
        }
    }

    pub fn get_physical_type(&self, type_id: TypeId) -> Option<PhysicalType> {
        let chased = self.get_chased_id(type_id);
        match self.type_phys_type_lookup.get(chased) {
            None => None,
            Some(pt) => Some(*pt),
        }
    }

    /// Works for enum variants too
    // Note: feels clumsy to return an SV4 here but nothing better comes to mind.
    // I wanna return a slice but with what backing memory?
    pub fn get_struct_layout(&self, struct_type_id: TypeId) -> SV4<StructField> {
        let struct_pt = self.get_physical_type(struct_type_id).unwrap();
        if struct_pt.is_empty() {
            smallvec![]
        } else {
            self.get_agg_struct_layout(struct_pt.expect_agg())
        }
    }

    pub fn get_agg_struct_layout(&self, struct_agg_id: AggregateTypeId) -> SV4<StructField> {
        match self.agg_types.get(struct_agg_id).agg_type {
            AggType::Enum(e) => self.get_agg_struct_layout(e.struct_repr),
            AggType::Struct { fields } => self.mem.getn_sv4(fields),
            AggType::Array { .. } => panic!("Array is not a struct"),
            AggType::Union { .. } => panic!("not a struct"),
        }
    }

    pub fn get_agg_for_type(&self, type_id: TypeId) -> &AggregateTypeRecord {
        let agg_id = self.get_physical_type(type_id).unwrap().expect_agg();
        self.agg_types.get(agg_id)
    }

    pub fn get_enum_struct_layout(
        &self,
        enum_agg_id: AggregateTypeId,
    ) -> (StructField, Option<StructField>) {
        let struct_id = self.agg_types.get(enum_agg_id).agg_type.expect_enum().struct_repr;
        let fields = self.agg_types.get(struct_id).agg_type.expect_struct();
        let tag = *self.mem.get_nth(fields, 0);
        let payload = self.mem.get_nth_opt(fields, 1).copied();
        (tag, payload)
    }

    pub fn enum_variant_by_name(
        &self,
        variants: MSlice<TypedEnumVariant, TypePool>,
        name: Ident,
    ) -> Option<&'static TypedEnumVariant> {
        self.mem.getn(variants).iter().find(|v| v.name == name)
    }
    pub fn enum_variant_by_index(
        &self,
        variants: MSlice<TypedEnumVariant, TypePool>,
        index: u32,
    ) -> &TypedEnumVariant {
        self.mem.getn(variants).iter().find(|v| v.index == index).unwrap()
    }

    pub fn get_layout(&self, type_id: TypeId) -> Layout {
        let chased = self.get_chased_id(type_id);
        match self.type_phys_type_lookup.get(chased) {
            None => Layout::ZERO_SIZED,
            Some(pt) => self.get_pt_layout(*pt),
        }
    }

    pub fn get_ast_node(&self, type_id: TypeId) -> Option<ParsedId> {
        match self.get(type_id) {
            Type::Enum(e) => Some(e.ast_node),
            Type::Lambda(clos) => Some(clos.parsed_id),
            Type::LambdaObject(clos_obj) => Some(clos_obj.parsed_id),
            _ => self.get_defn_info(type_id).map(|info| info.ast_id),
        }
    }

    pub fn is_aggregate(&self, type_id: TypeId) -> bool {
        match self.get(type_id) {
            Type::Struct(_) => true,
            Type::Enum(_) => true,
            Type::Lambda(_) => true,
            Type::LambdaObject(_) => true,
            Type::Static(stat) => self.is_aggregate(stat.family_type_id),
            Type::Array(_) => true,
            Type::Char => false,
            Type::Integer(_) => false,
            Type::Float(_) => false,
            Type::Bool => false,
            Type::Pointer => false,
            Type::Reference(_) => false,
            Type::Never => false,
            Type::Function(_) => false,
            Type::FunctionPointer(_) => false,
            Type::Generic(_) => false,
            Type::TypeParameter(_) => false,
            Type::FunctionTypeParameter(_) => false,
            Type::InferenceHole(_) => false,
            Type::Unresolved(_) => false,
            Type::RecursiveReference(_) => false,
        }
    }

    pub fn get_as_list_instance(&self, type_id: TypeId) -> Option<ListType> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == LIST_TYPE_ID {
                Some(ListType { element_type: *self.mem.get_nth(spec_info.type_args, 0) })
            } else {
                None
            }
        })
    }

    pub fn get_as_buffer_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == BUFFER_TYPE_ID {
                Some(*self.mem.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_view_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == VIEW_TYPE_ID {
                Some(*self.mem.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_container_instance(&self, type_id: TypeId) -> Option<(TypeId, ContainerKind)> {
        if let Some(info) = self.get_instance_info(type_id) {
            if info.generic_parent == LIST_TYPE_ID {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::List))
            } else if info.generic_parent == BUFFER_TYPE_ID {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::Buffer))
            } else if info.generic_parent == VIEW_TYPE_ID {
                Some((*self.mem.get_nth(info.type_args, 0), ContainerKind::View))
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
            if spec_info.generic_parent == OPTIONAL_TYPE_ID {
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

    pub fn display_pt(&self, w: &mut impl std::fmt::Write, t: PhysicalType) -> std::fmt::Result {
        match t {
            PhysicalType::Empty => w.write_str("{}"),
            PhysicalType::Scalar(st) => write!(w, "{}", st),
            PhysicalType::Agg(agg) => match self.agg_types.get(agg).agg_type {
                AggType::Enum(e) => {
                    w.write_str("enum ")?;
                    self.display_pt(w, PhysicalType::Agg(e.struct_repr))?;
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
            },
        }
    }
}

// Talks about the 4 kinds of contiguous 'collection' / 'container' types that
// the compiler knows about
pub enum ContainerKind {
    Array(TypeId),
    Buffer,
    View,
    List,
}
