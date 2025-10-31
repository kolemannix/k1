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

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Ident,
    pub type_id: TypeId,
}
impl_copy_if_small!(8, StructTypeField);

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub layout: Layout,
    pub field_offsets: SV8<u32>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
        m.get_slice(self.fields).iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const U8_TYPE_ID: TypeId = TypeId(NonZeroU32::new(1).unwrap());
pub const U16_TYPE_ID: TypeId = TypeId(NonZeroU32::new(2).unwrap());
pub const U32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(3).unwrap());
pub const U64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(4).unwrap());
pub const UWORD_TYPE_ID: TypeId = TypeId(NonZeroU32::new(5).unwrap());
pub const I8_TYPE_ID: TypeId = TypeId(NonZeroU32::new(6).unwrap());
pub const I16_TYPE_ID: TypeId = TypeId(NonZeroU32::new(7).unwrap());
pub const I32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(8).unwrap());
pub const I64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(9).unwrap());
pub const IWORD_TYPE_ID: TypeId = TypeId(NonZeroU32::new(10).unwrap());

pub const UNIT_TYPE_ID: TypeId = TypeId(NonZeroU32::new(11).unwrap());
pub const CHAR_TYPE_ID: TypeId = TypeId(NonZeroU32::new(12).unwrap());
pub const BOOL_TYPE_ID: TypeId = TypeId(NonZeroU32::new(13).unwrap());
pub const NEVER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(14).unwrap());
pub const POINTER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(15).unwrap());
pub const F32_TYPE_ID: TypeId = TypeId(NonZeroU32::new(16).unwrap());
pub const F64_TYPE_ID: TypeId = TypeId(NonZeroU32::new(17).unwrap());

pub const BUFFER_DATA_FIELD_NAME: &str = "data";
pub const BUFFER_TYPE_ID: TypeId = TypeId(NonZeroU32::new(18).unwrap());

pub const VIEW_TYPE_ID: TypeId = TypeId(NonZeroU32::new(19).unwrap());

pub const LIST_TYPE_ID: TypeId = TypeId(NonZeroU32::new(20).unwrap());
pub const STRING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(21).unwrap());
pub const OPTIONAL_TYPE_ID: TypeId = TypeId(NonZeroU32::new(22).unwrap());
pub const COMPILER_SOURCE_LOC_TYPE_ID: TypeId = TypeId(NonZeroU32::new(23).unwrap());
pub const ORDERING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(24).unwrap());
//pub const TYPE_SCHEMA_TYPE_ID: TypeId = TypeId(NonZeroU32::new(39).unwrap());

#[derive(Debug, Clone)]
pub struct ListType {
    pub element_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub name: Ident,
    pub static_constraint: Option<TypeId>,
    pub scope_id: ScopeId,
    pub span: SpanId,
}
impl_copy_if_small!(16, TypeParameter);

#[derive(Debug, Clone)]
pub struct FunctionTypeParameter {
    pub name: Ident,
    pub scope_id: ScopeId,
    pub span: SpanId,
    pub function_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct InferenceHoleType {
    pub index: u32,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
    pub size_type: TypeId,
    pub concrete_count: Option<u64>,
}
impl_copy_if_small!(24, ArrayType);

#[derive(Debug, Clone)]
pub struct TypedEnumVariant {
    pub enum_type_id: TypeId,
    pub my_type_id: TypeId,
    pub name: Ident,
    pub index: u32,
    pub payload: Option<TypeId>,
    pub tag_value: TypedIntValue,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub variants: EcoVec<TypedEnumVariant>,
    pub ast_node: ParsedId,
    pub tag_type: TypeId,
}

impl TypedEnum {
    pub fn is_no_payload(&self) -> bool {
        !self.has_payloads()
    }
    pub fn has_payloads(&self) -> bool {
        self.variants.iter().any(|v| v.payload.is_some())
    }
    pub fn variant_by_name(&self, name: Ident) -> Option<&TypedEnumVariant> {
        self.variants.iter().find(|v| v.name == name)
    }
    pub fn variant_by_index(&self, index: u32) -> &TypedEnumVariant {
        self.variants.iter().find(|v| v.index == index).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub params: SliceHandle<NameAndTypeId>,
    pub inner: TypeId,
}

impl GenericType {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerType {
    U8,
    U16,
    U32,
    U64,
    UWord(WordSize),
    I8,
    I16,
    I32,
    I64,
    IWord(WordSize),
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::UWord(_) => write!(f, "uword"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::IWord(_) => write!(f, "iword"),
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
            Self::UWord(_) => UWORD_TYPE_ID,
            Self::I8 => I8_TYPE_ID,
            Self::I16 => I16_TYPE_ID,
            Self::I32 => I32_TYPE_ID,
            Self::I64 => I64_TYPE_ID,
            Self::IWord(_) => IWORD_TYPE_ID,
        }
    }

    pub fn width(&self) -> NumericWidth {
        match self {
            Self::U8 | Self::I8 => NumericWidth::B8,
            Self::U16 | Self::I16 => NumericWidth::B16,
            Self::U32 | Self::I32 => NumericWidth::B32,
            Self::U64 | Self::I64 => NumericWidth::B64,
            Self::UWord(word_size) | Self::IWord(word_size) => word_size.width(),
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::UWord(_) => false,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::IWord(_) => true,
        }
    }

    pub fn zero(&self) -> TypedIntValue {
        match self {
            IntegerType::U8 => TypedIntValue::U8(0),
            IntegerType::U16 => TypedIntValue::U16(0),
            IntegerType::U32 => TypedIntValue::U32(0),
            IntegerType::U64 => TypedIntValue::U64(0),
            IntegerType::UWord(WordSize::W32) => TypedIntValue::UWord32(0),
            IntegerType::UWord(WordSize::W64) => TypedIntValue::UWord64(0),
            IntegerType::I8 => TypedIntValue::I8(0),
            IntegerType::I16 => TypedIntValue::I16(0),
            IntegerType::I32 => TypedIntValue::I32(0),
            IntegerType::I64 => TypedIntValue::I64(0),
            IntegerType::IWord(WordSize::W32) => TypedIntValue::IWord32(0),
            IntegerType::IWord(WordSize::W64) => TypedIntValue::IWord64(0),
        }
    }

    pub fn get_scalar_type(&self) -> ScalarType {
        match self {
            IntegerType::U8 => ScalarType::U8,
            IntegerType::U16 => ScalarType::U16,
            IntegerType::U32 => ScalarType::U32,
            IntegerType::U64 => ScalarType::U64,
            IntegerType::UWord(WordSize::W32) => ScalarType::U32,
            IntegerType::UWord(WordSize::W64) => ScalarType::U64,
            IntegerType::I8 => ScalarType::I8,
            IntegerType::I16 => ScalarType::I16,
            IntegerType::I32 => ScalarType::I32,
            IntegerType::I64 => ScalarType::I64,
            IntegerType::IWord(WordSize::W32) => ScalarType::I32,
            IntegerType::IWord(WordSize::W64) => ScalarType::I64,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    pub span: SpanId,
}

#[derive(Clone)]
pub struct FunctionType {
    pub physical_params: MSlice<FnParamType, TypePool>,
    pub return_type: TypeId,
    pub is_lambda: bool,
}

impl FunctionType {
    pub fn logical_params(&self) -> MSlice<FnParamType, TypePool> {
        if self.is_lambda { self.physical_params.skip(1) } else { self.physical_params }
    }
}

#[derive(Debug, Clone)]
pub struct RecursiveReference {
    pub root_type_id: TypeId,
    pub type_args: SV4<TypeId>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LambdaObjectType {
    pub function_type: TypeId,
    pub parsed_id: ParsedId,
    pub struct_representation: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub struct StaticType {
    pub inner_type_id: TypeId,
    pub value_id: Option<StaticValueId>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionPointerType {
    pub function_type_id: TypeId,
}

// To shrink this, we'd
// [x] move TypeDefnInfo off,
// [ ] convert Vecs to EcoVecs, or slice handles when we can
static_assert_size!(Type, 48);
#[derive(Clone)]
pub enum Type {
    Unit,
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
    Enum(TypedEnum),

    /// Enum variants are proper types of their own, for lots
    /// of reasons that make programming nice. Unlike in Rust :()
    EnumVariant(TypedEnumVariant),

    /// The 'bottom', uninhabited type; used to indicate exits of the program
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
            (Type::Unit, Type::Unit) => true,
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
                for (f1, f2) in
                    self.mem.get_slice(s1.fields).iter().zip(self.mem.get_slice(s2.fields))
                {
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
            (Type::InferenceHole(h1), Type::InferenceHole(h2)) => h1.index == h2.index,
            (Type::Enum(e1), Type::Enum(e2)) => {
                if defn1 != defn2 {
                    return false;
                }
                if e1.variants.len() != e2.variants.len() {
                    return false;
                }
                for (index, v1) in e1.variants.iter().enumerate() {
                    let v2 = &e2.variants[index];
                    let mismatch = v1.name != v2.name || v1.payload != v2.payload;
                    if mismatch {
                        return false;
                    }
                }
                true
            }
            // We never really want to de-dupe this type as its inherently unique
            (Type::EnumVariant(_ev1), Type::EnumVariant(_ev2)) => false,
            (Type::Never, Type::Never) => true,
            // We never really want to de-dupe this type as its inherently unique
            (Type::Generic(_g1), Type::Generic(_g2)) => false,
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.return_type == f2.return_type
                    && f1.physical_params.len() == f2.physical_params.len()
                {
                    self.mem
                        .get_slice(f1.physical_params)
                        .iter()
                        .zip(self.mem.get_slice(f2.physical_params))
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
                c1.function_type == c2.function_type && c1.parsed_id == c2.parsed_id
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
            Type::Unit => {}
            Type::Char => {}
            Type::Integer(int) => discriminant(int).hash(state),
            Type::Bool => {}
            Type::Struct(s) => {
                defn.hash(state);
                s.fields.len().hash(state);
                for f in self.mem.get_slice(s.fields) {
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
            }
            Type::Enum(e) => {
                defn.hash(state);
                e.variants.len().hash(state);
                for v in e.variants.iter() {
                    v.name.hash(state);
                    v.payload.hash(state);
                }
            }
            // We never really want to de-dupe this type as its inherently unique
            Type::EnumVariant(variant) => {
                variant.enum_type_id.hash(state);
                variant.name.hash(state);
                variant.payload.hash(state);
            }
            // Inherently unique as well
            Type::Generic(generic) => {
                generic.inner.hash(state);
                generic.params.index().hash(state);
                generic.params.len().hash(state);
            }
            Type::Function(fun) => {
                fun.return_type.hash(state);
                for param in self.mem.get_slice(fun.physical_params) {
                    param.name.hash(state);
                    param.is_context.hash(state);
                    param.is_lambda_env.hash(state);
                    param.type_id.hash(state);
                }
            }
            Type::FunctionPointer(fp) => fp.function_type_id.hash(state),
            Type::Lambda(c) => {
                c.parsed_id.hash(state);
                c.function_type.hash(state)
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
                stat.inner_type_id.hash(state);
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
            Type::Unit => "scalar",
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
            Type::EnumVariant(_) => "variant",
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

    // Note: This is kind of a codegen concern that doesn't belong in this layer,
    //       but it has some implications for typechecking, and I'm not super worried
    //       about platform independence in the middle-end right now
    // Should Pointer be here?
    pub fn is_scalar_int_value(&self) -> bool {
        matches!(self, Type::Unit | Type::Char | Type::Integer(_) | Type::Bool)
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
    pub fn expect_enum_mut(&mut self) -> &mut TypedEnum {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum type"),
        }
    }

    #[track_caller]
    pub fn expect_enum_variant(&self) -> &TypedEnumVariant {
        match self {
            Type::EnumVariant(v) => v,
            _ => panic!("expected enum variant on {}", self.kind_name()),
        }
    }

    #[track_caller]
    pub fn expect_enum_variant_mut(&mut self) -> &mut TypedEnumVariant {
        match self {
            Type::EnumVariant(v) => v,
            _ => panic!("expected enum variant type"),
        }
    }

    pub fn as_enum(&self) -> Option<&TypedEnum> {
        match self {
            Type::Enum(e) => Some(e),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_enum(&self) -> &TypedEnum {
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

    pub fn as_enum_variant(&self) -> Option<&TypedEnumVariant> {
        match self {
            Type::EnumVariant(ev) => Some(ev),
            _ => None,
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

#[derive(Debug, Default, Clone, Copy)]
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
    Agg(PhysicalTypeId),
}

impl PhysicalType {
    pub fn is_agg(&self) -> bool {
        matches!(self, PhysicalType::Agg(_))
    }

    #[track_caller]
    pub fn expect_agg(&self) -> PhysicalTypeId {
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
}

#[derive(Clone, Copy)]
pub struct StructField {
    pub offset: u32,
    pub field_t: PhysicalType,
}

#[derive(Clone, Copy)]
pub struct EnumVariantLayout {
    pub tag: ScalarType,
    pub payload: Option<PhysicalType>,
    pub payload_offset: Option<u32>,
    pub envelope: Layout,
}

#[derive(Clone, Copy)]
pub enum AggType {
    // Important specialization since wrappers are common
    Struct1(PhysicalType),
    EnumVariant(EnumVariantLayout),
    Struct { fields: MSlice<StructField, TypePool> },
    Array { element_t: PhysicalType, len: u32 },
    Opaque { layout: Layout },
}

impl AggType {
    #[track_caller]
    pub fn expect_array(&self) -> (PhysicalType, u32) {
        match self {
            AggType::Array { element_t, len } => (*element_t, *len),
            _ => panic!("Expected array agg type"),
        }
    }

    #[track_caller]
    pub fn expect_enum_variant(&self) -> &EnumVariantLayout {
        match self {
            AggType::EnumVariant(ev) => ev,
            _ => panic!("Expected enum variant agg type"),
        }
    }
}

nz_u32_id!(PhysicalTypeId);
pub struct PhysicalTypeRecord {
    pub agg_type: AggType,
    pub origin_type_id: TypeId,
    pub layout: Layout,
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
    pub specializations: FxHashMap<(TypeId, TypeIdSlice), TypeId>,

    /// Lookup mappings for parsed -> typed ids
    pub ast_type_defn_mapping: FxHashMap<ParsedTypeDefnId, TypeId>,
    pub ast_ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,

    pub builtins: BuiltinTypes,

    // nocommit: deprecated; merge with new `mem`
    pub type_slices: VPool<TypeId, TypeSliceId>,

    pub phys_types: VPool<PhysicalTypeRecord, PhysicalTypeId>,

    pub mem: kmem::Mem<TypePool>,
}

impl TypePool {
    pub fn empty() -> TypePool {
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

            type_slices: VPool::make_with_hint("type_slices", EXPECTED_TYPE_COUNT),

            phys_types: VPool::make_with_hint("phys_types", EXPECTED_TYPE_COUNT / 2),

            mem: kmem::Mem::make(),
        }
    }

    #[cfg(test)]
    pub fn with_builtin_types() -> TypePool {
        let mut this = TypePool::empty();
        this.add_anon(Type::Integer(IntegerType::U8));
        this.add_anon(Type::Integer(IntegerType::U16));
        this.add_anon(Type::Integer(IntegerType::U32));
        this.add_anon(Type::Integer(IntegerType::U64));
        this.add_anon(Type::Integer(IntegerType::I8));
        this.add_anon(Type::Integer(IntegerType::I16));
        this.add_anon(Type::Integer(IntegerType::I32));
        this.add_anon(Type::Integer(IntegerType::I64));

        this.add_anon(Type::Unit);
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

        // if type_id is a static, we fail to invoke the enum path
        // Maybe we check that explicitly here; not sure what else to do
        let t = match typ {
            Type::Enum(e) => {
                // Enums and variants are self-referential
                // so we do extra work to ensure all the self-references are correct
                let enum_type_id = self.add_or_resolve_enum(e, None, defn_info, instance_info);
                enum_type_id
            }
            Type::EnumVariant(_ev) => {
                panic!("EnumVariant cannot be directly interned; intern the Enum instead")
            }
            _ => {
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

                type_id
            }
        };
        debug_assert_eq!(self.type_variable_counts.len(), self.types.len());
        debug_assert_eq!(self.instance_info.len(), self.types.len());
        debug_assert_eq!(self.instance_info.len(), self.type_phys_type_lookup.len());

        t
    }

    fn add_or_resolve_enum(
        &mut self,
        mut e: TypedEnum,
        type_id_to_use: Option<TypeId>,
        defn_info: Option<TypeDefnInfo>,
        instance_info: Option<GenericInstanceInfo>,
    ) -> TypeId {
        // Enums and variants are self-referential
        // so we handle them specially
        let next_type_id = self.next_type_id();
        let first_variant_id = next_type_id.0;
        let variant_count = e.variants.len();
        let enum_type_id = match type_id_to_use {
            None => TypeId(first_variant_id.saturating_add(variant_count as u32)),
            Some(type_id) => type_id,
        };

        let mut physical_types = self.mem.new_vec(e.variants.len() as u32);
        let mut is_physical = true;

        // Enum sizing and layout rules:
        // - Alignment of the enum is the max(alignment) of the variants
        // - Size of the enum is the size of the largest variant, not necessarily the same
        //   variant, plus alignment end padding
        //
        //  ... Basically, this is just union layout rules, I now understand 2 years later
        let tag_scalar = self.get_physical_type(e.tag_type).unwrap().expect_scalar();
        let tag_layout = tag_scalar.get_layout();
        let mut max_variant_align = tag_layout.align;
        let mut max_variant_size = tag_layout.size;
        for v in e.variants.make_mut().iter_mut() {
            let variant_id = TypeId(first_variant_id.saturating_add(v.index));
            v.my_type_id = variant_id;
            v.enum_type_id = enum_type_id;
            let variant = Type::EnumVariant(v.clone());
            let actual_id = self.types.add(variant);
            debug_assert_eq!(variant_id, actual_id);

            let variant_variable_counts = self.count_type_variables(variant_id);
            self.type_variable_counts.add(variant_variable_counts);

            self.instance_info.add(instance_info.clone());

            if let Some(defn_info) = defn_info {
                self.defn_info.insert(v.my_type_id, defn_info);
            }

            // PhysicalLayout
            if let Some(payload) = &v.payload {
                match self.get_physical_type(*payload) {
                    None => {
                        is_physical = false;
                    }
                    Some(payload_pt) => {
                        let mut struct_layout = tag_layout;
                        let payload_layout = self.get_pt_layout(&payload_pt);
                        let payload_offset = struct_layout.append_to_aggregate(payload_layout);

                        if struct_layout.align > max_variant_align {
                            max_variant_align = struct_layout.align
                        };
                        if struct_layout.size > max_variant_size {
                            max_variant_size = struct_layout.size
                        };

                        physical_types.push(EnumVariantLayout {
                            tag: tag_scalar,
                            payload: Some(payload_pt),
                            payload_offset: Some(payload_offset),
                            // To be filled in once all variants are known
                            envelope: Layout::ZERO,
                        });
                    }
                }
            } else {
                physical_types.push(EnumVariantLayout {
                    tag: tag_scalar,
                    payload: None,
                    payload_offset: None,
                    // To be filled in once all variants are known
                    envelope: Layout::ZERO,
                });
            }
        }

        let envelope = Layout { size: max_variant_size, align: max_variant_align };
        if is_physical {
            debug_assert_eq!(physical_types.len(), variant_count);
            for (variant_idx, pt) in physical_types.iter_mut().enumerate() {
                pt.envelope = envelope;
                let variant_id_for_idx =
                    TypeId(first_variant_id.saturating_add(variant_idx as u32));
                let pt_id = self.phys_types.add(PhysicalTypeRecord {
                    agg_type: AggType::EnumVariant(*pt),
                    origin_type_id: variant_id_for_idx,
                    layout: envelope,
                });
                self.type_phys_type_lookup.add(Some(PhysicalType::Agg(pt_id)));
            }
        } else {
            for _ in 0..variant_count {
                self.type_phys_type_lookup.add(None);
            }
        }

        let enum_pt = if is_physical {
            let agg_id = self.phys_types.add(PhysicalTypeRecord {
                agg_type: AggType::Opaque { layout: envelope },
                origin_type_id: enum_type_id,
                layout: envelope,
            });
            Some(PhysicalType::Agg(agg_id))
        } else {
            None
        };

        let enum_type = Type::Enum(e);
        let enum_hash = self.hash_type(&enum_type, defn_info);
        self.hashes.insert(enum_hash, enum_type_id);
        match type_id_to_use {
            None => {
                // Inserting a new type
                let type_id = self.types.add(enum_type);

                let variable_counts = self.count_type_variables(enum_type_id);
                self.type_variable_counts.add(variable_counts);

                self.instance_info.add(instance_info);
                self.type_phys_type_lookup.add(enum_pt);

                if let Some(defn_info) = defn_info {
                    self.defn_info.insert(type_id, defn_info);
                }
            }
            Some(_unresolved_type_id) => {
                // We're updating unresolved_type_id to point to the enum.

                // Remove stale hash
                // No real need to do this since no one will ever collide with the old 'Unresolved'
                // type as they are hashed by their unique AST ID, basically making them unique
                // let old = self.get(enum_type_id);
                // let old_hash = self.hash(old);
                // self.hashes.remove(&old_hash);

                *self.get_mut(enum_type_id) = enum_type;

                let variable_counts = self.count_type_variables(enum_type_id);
                *self.type_variable_counts.get_mut(enum_type_id) = variable_counts;
                *self.instance_info.get_mut(enum_type_id) = instance_info;
                *self.type_phys_type_lookup.get_mut(enum_type_id) = enum_pt;
            }
        };

        debug_assert_eq!(self.types.len(), self.type_variable_counts.len());
        debug_assert_eq!(self.types.len(), self.instance_info.len());
        debug_assert_eq!(self.types.len(), self.type_phys_type_lookup.len());

        enum_type_id
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
        match type_value {
            Type::Enum(e) => {
                self.add_or_resolve_enum(e, Some(unresolved_type_id), defn_info, instance_info);
            }
            _ => {
                let hash = self.hash_type(&type_value, defn_info);
                let typ = self.get_mut(unresolved_type_id);
                if typ.as_unresolved().is_none() {
                    panic!("Tried to resolve a type that was not unresolved: {}", typ.kind_name());
                }
                *typ = type_value;
                self.hashes.insert(hash, unresolved_type_id);
                // FIXME: Adding a type is a mess, since we have all these places to update
                // and we have to do it differently if we're resolving vs adding new.
                // ...
                // HARD AGREE 4 months later!
                // Checklist is:
                // - manage the hash
                // - Update the 3 SoA fields: variable counts, phys_type_mapping, and instance_info
                // - Manage both the resolve vs insert paths
                // - Handle enums since they are self-referential

                let variable_counts = self.count_type_variables(unresolved_type_id);
                *self.type_variable_counts.get_mut(unresolved_type_id) = variable_counts;
                *self.instance_info.get_mut(unresolved_type_id) = instance_info;

                let pt_id = self.compile_physical_type(unresolved_type_id);
                *self.type_phys_type_lookup.get_mut(unresolved_type_id) = pt_id;
            }
        };
    }

    pub fn next_type_id(&self) -> TypeId {
        // Safety: If you add one to a u32 it'll never be zero
        unsafe { TypeId(NonZeroU32::new_unchecked(self.types.len() as u32 + 1)) }
    }

    pub fn add_reference_type(&mut self, inner_type: TypeId, mutable: bool) -> TypeId {
        self.add_anon(Type::Reference(ReferenceType { inner_type, mutable }))
    }

    pub fn add_function_pointer_type(&mut self, function_type_id: TypeId) -> TypeId {
        self.add_anon(Type::FunctionPointer(FunctionPointerType { function_type_id }))
    }

    pub fn add_static_type(
        &mut self,
        inner_type_id: TypeId,
        value_id: Option<StaticValueId>,
    ) -> TypeId {
        self.add_anon(Type::Static(StaticType { inner_type_id, value_id }))
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
            _ => None,
        }
    }

    pub fn get_static_type_of_type(&self, type_id: TypeId) -> Option<StaticType> {
        match self.get_static_type_id_of_type(type_id) {
            None => None,
            Some(type_id) => Some(*self.get_no_follow_static(type_id).as_static().unwrap()),
        }
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
            Type::Static(stat) => self.get(stat.inner_type_id),
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
            Type::Static(stat) => stat.inner_type_id,
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
            Type::Static(stat) => stat.inner_type_id,
            Type::TypeParameter(tp) if tp.static_constraint.is_some() => {
                self.get_static_type_of_type(tp.static_constraint.unwrap()).unwrap().inner_type_id
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

    pub fn get_as_enum(&self, type_id: TypeId) -> Option<(&TypedEnum, Option<&TypedEnumVariant>)> {
        match self.get(type_id) {
            Type::Enum(e) => Some((e, None)),
            Type::EnumVariant(ev) => Some((self.get(ev.enum_type_id).expect_enum(), Some(ev))),
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
        let fields = self.mem.push_slice(&[
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
            Type::Unit => EMPTY,
            Type::Char => EMPTY,
            Type::Integer(_) => EMPTY,
            Type::Float(_) => EMPTY,
            Type::Bool => EMPTY,
            Type::Pointer => EMPTY,
            Type::Struct(struc) => {
                let mut result = EMPTY;
                for field in self.mem.get_slice(struc.fields).iter() {
                    result = result.add(self.count_type_variables(field.type_id))
                }
                result
            }
            Type::Reference(refer) => self.count_type_variables(refer.inner_type),
            Type::Enum(e) => {
                let mut result = EMPTY;
                for v in e.variants.iter() {
                    if let Some(payload) = v.payload {
                        result = result.add(self.count_type_variables(payload));
                    }
                }
                result
            }
            Type::EnumVariant(ev) => {
                if let Some(payload) = ev.payload {
                    self.count_type_variables(payload)
                } else {
                    EMPTY
                }
            }
            Type::Never => EMPTY,
            // The real answer here would be, all the type variables on the RHS that aren't one of
            // the params. In other words, all FREE type variables
            Type::Generic(_gen) => EMPTY,
            Type::Function(fun) => {
                let mut result = EMPTY;
                for param in self.mem.get_slice(fun.physical_params).iter() {
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
                let inner = self.count_type_variables(stat.inner_type_id);
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

    pub fn get_pt_layout(&self, pt: &PhysicalType) -> Layout {
        match pt {
            PhysicalType::Scalar(s) => s.get_layout(),
            PhysicalType::Agg(agg_id) => self.phys_types.get(*agg_id).layout,
        }
    }

    pub fn compile_physical_type(&mut self, type_id: TypeId) -> Option<PhysicalType> {
        match self.get_no_follow_static(type_id) {
            //task(bc): Eventually, Unit should correspond to Void, not a byte. But only once we can
            //successfully lower it to a zero-sized type everywhere; for example a struct member of
            //type unit
            Type::Unit | Type::Char | Type::Bool => Some(PhysicalType::Scalar(ScalarType::U8)),

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
                        let elem_layout = self.get_pt_layout(&element_t);
                        let record = PhysicalTypeRecord {
                            agg_type: AggType::Array { element_t, len: len as u32 },
                            origin_type_id: type_id,
                            layout: elem_layout.array_me(len as usize),
                        };
                        let id = self.phys_types.add(record);
                        Some(PhysicalType::Agg(id))
                    }
                },
            },
            Type::Struct(s) => {
                if s.fields.len() == 1 {
                    // 1-field struct special case, bit less pointer chasing
                    let f = self.mem.get_nth(s.fields, 0);
                    match self.get_physical_type(f.type_id) {
                        None => None,
                        Some(pt) => {
                            let layout = self.get_pt_layout(&pt);
                            let agg_id = self.phys_types.add(PhysicalTypeRecord {
                                agg_type: AggType::Struct1(pt),
                                origin_type_id: type_id,
                                layout,
                            });
                            Some(PhysicalType::Agg(agg_id))
                        }
                    }
                } else {
                    let s_fields = s.fields;
                    let mut fields = self.mem.new_vec(s.fields.len());
                    let mut layout = Layout::ZERO;
                    let mut not_physical = false;
                    for field in self.mem.get_slice(s_fields) {
                        if not_physical {
                            continue;
                        }
                        match self.get_physical_type(field.type_id) {
                            None => {
                                not_physical = true;
                            }
                            Some(field_pt) => {
                                let field_layout = self.get_pt_layout(&field_pt);
                                let offset = layout.append_to_aggregate(field_layout);
                                fields.push(StructField { field_t: field_pt, offset });
                            }
                        }
                    }
                    if not_physical {
                        None
                    } else {
                        let fields_handle = self.mem.vec_to_mslice(&fields);
                        let agg_id = self.phys_types.add(PhysicalTypeRecord {
                            agg_type: AggType::Struct { fields: fields_handle },
                            origin_type_id: type_id,
                            layout,
                        });
                        Some(PhysicalType::Agg(agg_id))
                    }
                }
            }
            Type::Enum(_typed_enum) => {
                unreachable!("enum physical types sold separately")
            }
            Type::EnumVariant(_ev) => {
                unreachable!("enum physical types sold separately")
            }
            Type::Lambda(lam) => self.add_physical_duplicate(type_id, lam.env_type),
            Type::LambdaObject(lam_obj) => {
                self.add_physical_duplicate(type_id, lam_obj.struct_representation)
            }
            Type::Static(stat) => {
                // Re-use the inner physical type
                self.add_physical_duplicate(type_id, stat.inner_type_id)
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
                pt @ PhysicalType::Scalar(_) => Some(pt),
                PhysicalType::Agg(agg_id) => {
                    let r = self.phys_types.get(agg_id);
                    let r_new = PhysicalTypeRecord {
                        agg_type: r.agg_type,
                        origin_type_id,
                        layout: r.layout,
                    };
                    let new_id = self.phys_types.add(r_new);
                    Some(PhysicalType::Agg(new_id))
                }
            },
        }
    }

    // Works for enum variants too
    pub fn get_struct_field_offset(&self, agg_id: PhysicalTypeId, field_index: u32) -> Option<u32> {
        match self.phys_types.get(agg_id).agg_type {
            AggType::Struct1(_) => {
                if field_index == 0 {
                    Some(0)
                } else {
                    None
                }
            }
            AggType::Struct { fields } => {
                if field_index < fields.len() {
                    let field_type = self.mem.get_nth(fields, field_index as usize);
                    Some(field_type.offset)
                } else {
                    None
                }
            }
            AggType::EnumVariant(ev) => match field_index {
                0 => Some(0),
                1 => ev.payload_offset,
                _ => None,
            },
            AggType::Array { .. } => None,
            AggType::Opaque { .. } => None,
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
    pub fn get_struct_layout(&self, struct_type_id: TypeId) -> SV4<StructField> {
        let struct_agg_id = self.get_physical_type(struct_type_id).unwrap().expect_agg();
        match self.phys_types.get(struct_agg_id).agg_type {
            AggType::Struct1(t) => smallvec![StructField { offset: 0, field_t: t }],
            AggType::Struct { fields } => self.mem.get_slice_sv4(fields),
            AggType::EnumVariant(evl) => {
                let tag_field = StructField { offset: 0, field_t: PhysicalType::Scalar(evl.tag) };
                match evl.payload {
                    None => {
                        smallvec![tag_field]
                    }
                    Some(pt) => smallvec![
                        tag_field,
                        StructField { offset: evl.payload_offset.unwrap(), field_t: pt },
                    ],
                }
            }
            _ => panic!("not a struct"),
        }
    }

    pub fn enum_variant_payload_fields(
        &self,
        idents: &IdentPool,
        ev: &TypedEnumVariant,
    ) -> (StructTypeField, Option<StructTypeField>) {
        let tag_field = StructTypeField { name: idents.b.tag, type_id: ev.tag_value.get_type() };

        if let Some(payload) = ev.payload {
            let payload_field = StructTypeField { name: idents.b.payload, type_id: payload };
            (tag_field, Some(payload_field))
        } else {
            (tag_field, None)
        }
    }

    pub fn enum_variant_payload_offset_bytes(&self, ev: &TypedEnumVariant) -> usize {
        let mut layout = Layout::ZERO;
        layout.append_to_aggregate(self.get_layout(ev.tag_value.get_type()));
        let payload_start = layout.append_to_aggregate(self.get_layout(ev.payload.unwrap()));
        payload_start as usize
    }

    pub fn get_layout(&self, type_id: TypeId) -> Layout {
        let chased = self.get_chased_id(type_id);
        match self.type_phys_type_lookup.get(chased) {
            None => Layout::ZERO,
            Some(pt) => self.get_pt_layout(pt),
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

    pub fn is_aggregate_repr(&self, type_id: TypeId) -> bool {
        match self.get(type_id) {
            Type::Struct(_) => true,
            Type::Enum(_) => true,
            Type::EnumVariant(_) => true,
            Type::Lambda(_) => true,
            Type::LambdaObject(_) => true,
            Type::Static(stat) => self.is_aggregate_repr(stat.inner_type_id),
            Type::Array(_) => true,
            Type::Unit => false,
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
                Some(ListType { element_type: *self.type_slices.get_nth(spec_info.type_args, 0) })
            } else {
                None
            }
        })
    }

    pub fn get_as_buffer_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == BUFFER_TYPE_ID {
                Some(*self.type_slices.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_view_instance(&self, type_id: TypeId) -> Option<TypeId> {
        self.instance_info.get(type_id).as_ref().and_then(|spec_info| {
            if spec_info.generic_parent == VIEW_TYPE_ID {
                Some(*self.type_slices.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_as_container_instance(&self, type_id: TypeId) -> Option<(TypeId, ContainerKind)> {
        if let Some(info) = self.get_instance_info(type_id) {
            if info.generic_parent == LIST_TYPE_ID {
                Some((*self.type_slices.get_nth(info.type_args, 0), ContainerKind::List))
            } else if info.generic_parent == BUFFER_TYPE_ID {
                Some((*self.type_slices.get_nth(info.type_args, 0), ContainerKind::Buffer))
            } else if info.generic_parent == VIEW_TYPE_ID {
                Some((*self.type_slices.get_nth(info.type_args, 0), ContainerKind::View))
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
                Some(*self.type_slices.get_nth(spec_info.type_args, 0))
            } else {
                None
            }
        })
    }

    pub fn get_specialization(
        &self,
        base: TypeId,
        args: SliceHandle<TypeSliceId>,
    ) -> Option<TypeId> {
        self.specializations.get(&(base, args)).copied()
    }

    pub fn insert_specialization(
        &mut self,
        base: TypeId,
        args: SliceHandle<TypeSliceId>,
        specialized: TypeId,
    ) {
        self.specializations.insert((base, args), specialized);
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

pub fn display_pt(
    w: &mut impl std::fmt::Write,
    types: &TypePool,
    t: &PhysicalType,
) -> std::fmt::Result {
    match t {
        PhysicalType::Scalar(st) => write!(w, "{}", st),
        PhysicalType::Agg(agg) => match &types.phys_types.get(*agg).agg_type {
            // Important specialization since wrappers are common
            AggType::Struct1(t1) => {
                w.write_str("{ ")?;
                display_pt(w, types, t1)?;
                w.write_str(" }")?;
                Ok(())
            }
            AggType::EnumVariant(evl) => {
                write!(w, "{{ tag({})", evl.tag)?;
                if let Some(payload) = &evl.payload {
                    write!(w, ", ")?;
                    display_pt(w, types, payload)?;
                };
                w.write_str(" }")?;
                Ok(())
            }
            AggType::Struct { fields } => {
                w.write_str("{ ")?;
                for (index, field) in types.mem.get_slice(*fields).iter().enumerate() {
                    display_pt(w, types, &field.field_t)?;
                    let last = index == fields.len() as usize - 1;
                    if !last {
                        w.write_str(", ")?;
                    }
                }
                w.write_str(" }")?;
                Ok(())
            }
            AggType::Array { len, element_t: t } => {
                w.write_str("[")?;
                display_pt(w, types, t)?;
                write!(w, " x {}]", *len)?;
                Ok(())
            }
            AggType::Opaque { layout } => {
                write!(w, "opaque {}, align {}", layout.size, layout.align)
            }
        },
    }
}

pub fn pt_to_string(types: &TypePool, t: &PhysicalType) -> String {
    let mut s = String::new();
    display_pt(&mut s, types, t).unwrap();
    s
}
