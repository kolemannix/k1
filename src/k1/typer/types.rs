use std::fmt::{Display, Formatter};
use std::hash::Hasher;

use fxhash::FxHashMap;

use crate::typer::scopes::*;

use crate::parse::{Ident, ParsedId, ParsedTypeDefnId};

use crate::{SV4, impl_copy_if_small, nz_u32_id, typer::*};

nz_u32_id!(TypeId);

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Ident,
    pub type_id: TypeId,
    pub private: bool,
}
impl_copy_if_small!(12, StructTypeField);

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub layout: Layout,
    pub field_offsets: SV8<u32>,
}

#[derive(Debug, Clone)]
pub struct GenericInstanceInfo {
    pub generic_parent: TypeId,
    pub type_args: SV4<TypeId>,
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

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: EcoVec<StructTypeField>,
    pub generic_instance_info: Option<GenericInstanceInfo>,
    pub defn_id: Option<TypeDefnId>,
}

impl StructType {
    pub fn is_named(&self) -> bool {
        self.defn_id.is_some()
    }
    pub fn is_anon(&self) -> bool {
        self.defn_id.is_none()
    }
    pub fn find_field(&self, field_name: Ident) -> Option<(usize, &StructTypeField)> {
        self.fields.iter().enumerate().find(|(_, field)| field.name == field_name)
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

pub const LIST_TYPE_ID: TypeId = TypeId(NonZeroU32::new(19).unwrap());
pub const STRING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(20).unwrap());
pub const OPTIONAL_TYPE_ID: TypeId = TypeId(NonZeroU32::new(21).unwrap());
pub const COMPILER_SOURCE_LOC_TYPE_ID: TypeId = TypeId(NonZeroU32::new(22).unwrap());
pub const ORDERING_TYPE_ID: TypeId = TypeId(NonZeroU32::new(23).unwrap());
//pub const TYPE_SCHEMA_TYPE_ID: TypeId = TypeId(NonZeroU32::new(39).unwrap());

#[derive(Debug, Clone)]
pub struct ListType {
    pub element_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub name: Ident,
    pub scope_id: ScopeId,
    pub span: SpanId,
}
impl_copy_if_small!(12, TypeParameter);

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

#[derive(Debug, Clone)]
pub struct OptionalType {
    pub inner_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct ReferenceType {
    pub inner_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
    pub size: u64,
}

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
    pub defn_id: Option<TypeDefnId>,
    /// Populated for specialized copies of generic enums, contains provenance info
    pub generic_instance_info: Option<GenericInstanceInfo>,
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
pub struct OpaqueTypeAlias {
    pub ast_id: ParsedTypeDefnId,
    pub aliasee: TypeId,
    pub type_defn_info: TypeDefnInfo,
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub params: SliceHandle<NameAndTypeId>,
    pub inner: TypeId,
    // TODO(perf): get specializations HashMap off of GenericType
    pub specializations: FxHashMap<SV4<TypeId>, TypeId>,
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

#[derive(Debug, Clone, Copy)]
pub struct FnParamType {
    // FIXME: Determine if param names are truly 'part' of the function type.
    // For now, keeping them to fix some bugs
    pub name: Ident,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_lambda_env: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub physical_params: EcoVec<FnParamType>,
    pub return_type: TypeId,
}

impl FunctionType {
    pub fn logical_params(&self) -> &[FnParamType] {
        if self.physical_params.is_empty() {
            &[]
        } else {
            if self.physical_params[0].is_lambda_env {
                &self.physical_params[1..]
            } else {
                &self.physical_params[..]
            }
        }
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
    pub body_function_id: FunctionId,
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

#[derive(Debug, Clone)]
pub struct StaticType {
    pub inner_type_id: TypeId,
    pub value_id: Option<StaticValueId>,
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub function_id: FunctionId,
    pub function_type: TypeId,
}

// To shrink this, we'd
// [x] move TypeDefnInfo off,
// [ ] convert Vecs to EcoVecs, or slice handles when we can
static_assert_size!(Type, 72);
#[derive(Debug, Clone)]
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

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (&self, &other) {
            (Type::Unit, Type::Unit) => true,
            (Type::Char, Type::Char) => true,
            (Type::Integer(int1), Type::Integer(int2)) => int1 == int2,
            (Type::Float(f1), Type::Float(f2)) => f1.size() == f2.size(),
            (Type::Bool, Type::Bool) => true,
            (Type::Pointer, Type::Pointer) => true,
            (Type::Struct(s1), Type::Struct(s2)) => {
                if s1.defn_id != s2.defn_id {
                    return false;
                }
                if s1.fields.len() != s2.fields.len() {
                    return false;
                }
                for (index, f1) in s1.fields.iter().enumerate() {
                    let f2 = &s2.fields[index];
                    let mismatch = f1.name != f2.name || f1.type_id != f2.type_id;
                    if mismatch {
                        return false;
                    }
                }
                true
            }
            (Type::Reference(r1), Type::Reference(r2)) => r1.inner_type == r2.inner_type,
            (Type::Array(a1), Type::Array(a2)) => {
                a1.element_type == a2.element_type && a1.size == a2.size
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
                if e1.defn_id != e2.defn_id {
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
                    f1.physical_params
                        .iter()
                        .zip(f2.physical_params.iter())
                        .all(|(p1, p2)| p1.type_id == p2.type_id)
                } else {
                    false
                }
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
}

impl Eq for Type {}

impl std::hash::Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Unit => "unit".hash(state),
            Type::Char => "char".hash(state),
            Type::Integer(int1) => match int1 {
                IntegerType::U8 => "u8".hash(state),
                IntegerType::U16 => "u16".hash(state),
                IntegerType::U32 => "u32".hash(state),
                IntegerType::U64 => "u64".hash(state),
                IntegerType::UWord(_) => "uword".hash(state),
                IntegerType::I8 => "i8".hash(state),
                IntegerType::I16 => "i16".hash(state),
                IntegerType::I32 => "i32".hash(state),
                IntegerType::I64 => "i64".hash(state),
                IntegerType::IWord(_) => "iword".hash(state),
            },
            Type::Bool => "bool".hash(state),
            Type::Struct(s) => {
                "struct".hash(state);
                s.defn_id.hash(state);
                s.fields.len().hash(state);
                for f in &s.fields {
                    f.name.hash(state);
                    f.type_id.hash(state);
                }
            }
            Type::Reference(r) => {
                "ref".hash(state);
                r.inner_type.hash(state);
            }
            Type::TypeParameter(t_param) => {
                "tvar".hash(state);
                t_param.name.hash(state);
                t_param.scope_id.hash(state);
            }
            Type::FunctionTypeParameter(ftp) => {
                "ftp".hash(state);
                ftp.name.hash(state);
                ftp.scope_id.hash(state);
                ftp.function_type.hash(state);
            }
            Type::InferenceHole(hole) => {
                "hole".hash(state);
                hole.index.hash(state);
            }
            Type::Enum(e) => {
                "enum".hash(state);
                e.defn_id.hash(state);
                e.variants.len().hash(state);
                for v in e.variants.iter() {
                    v.name.hash(state);
                    v.payload.hash(state);
                }
            }
            // We never really want to de-dupe this type as its inherently unique
            Type::EnumVariant(variant) => {
                "variant".hash(state);
                variant.enum_type_id.hash(state);
                variant.name.hash(state);
                variant.payload.hash(state);
            }
            // Inherently unique as well
            Type::Generic(generic) => {
                "gen".hash(state);
                generic.inner.hash(state);
                generic.params.index().hash(state);
                generic.params.len().hash(state);
            }
            Type::Function(fun) => {
                "fun".hash(state);
                fun.return_type.hash(state);
                for param in &fun.physical_params {
                    param.name.hash(state);
                    param.is_context.hash(state);
                    param.is_lambda_env.hash(state);
                    param.type_id.hash(state);
                }
            }
            Type::Lambda(c) => {
                "lambda".hash(state);
                c.parsed_id.hash(state);
                c.function_type.hash(state)
            }
            Type::Float(ft) => {
                "float".hash(state);
                ft.size().bits().hash(state);
            }
            Type::Pointer => {
                "ptr".hash(state);
            }
            Type::Never => "never".hash(state),
            Type::LambdaObject(co) => {
                "lambda_object".hash(state);
                co.function_type.hash(state);
                co.struct_representation.hash(state);
            }
            Type::Static(stat) => {
                "stat".hash(state);
                stat.inner_type_id.hash(state);
                stat.value_id.hash(state)
            }
            Type::Unresolved(id) => {
                "unresolved".hash(state);
                id.hash(state);
            }
            Type::RecursiveReference(rr) => {
                "recurse".hash(state);
                rr.root_type_id.hash(state);
            }
            Type::Array(arr) => {
                "array".hash(state);
                arr.element_type.hash(state);
                arr.size.hash(state);
            }
        }
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

    pub fn as_list_instance(&self) -> Option<ListType> {
        if let Type::Struct(s) = self {
            s.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == LIST_TYPE_ID {
                    Some(ListType { element_type: spec_info.type_args[0] })
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub fn as_buffer_instance(&self) -> Option<&GenericInstanceInfo> {
        if let Type::Struct(s) = self {
            s.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == BUFFER_TYPE_ID { Some(spec_info) } else { None }
            })
        } else {
            None
        }
    }

    pub fn as_opt_instance(&self) -> Option<OptionalType> {
        if let Type::Enum(e) = self {
            e.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == OPTIONAL_TYPE_ID {
                    Some(OptionalType { inner_type: spec_info.type_args[0] })
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub fn as_reference(&self) -> Option<&ReferenceType> {
        match self {
            Type::Reference(r) => Some(r),
            _ => None,
        }
    }

    #[track_caller]
    pub fn expect_reference(&self) -> &ReferenceType {
        match self {
            Type::Reference(r) => r,
            _ => panic!("expect_reference called on: {:?}", self),
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
            _ => panic!("expected enum on {:?}", self),
        }
    }

    #[track_caller]
    pub fn expect_optional(&self) -> OptionalType {
        self.as_opt_instance().unwrap()
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
            _ => panic!("expect_struct called on: {:?}", self),
        }
    }

    #[track_caller]
    pub fn expect_generic(&self) -> &GenericType {
        match self {
            Type::Generic(g) => g,
            _ => panic!("expect_generic called on: {:?}", self),
        }
    }

    #[track_caller]
    pub fn expect_integer(&self) -> &IntegerType {
        match self {
            Type::Integer(int) => int,
            _ => panic!("expect_integer called on: {:?}", self),
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
            _ => panic!("expect_function called on: {:?}", self),
        }
    }

    pub fn as_unresolved(&self) -> Option<ParsedTypeDefnId> {
        match self {
            Type::Unresolved(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TypeVariableInfo {
    pub inference_variable_count: u32,
    pub type_parameter_count: u32,
}

impl TypeVariableInfo {
    const EMPTY: TypeVariableInfo =
        TypeVariableInfo { inference_variable_count: 0, type_parameter_count: 0 };

    pub fn is_abstract(&self) -> bool {
        self.inference_variable_count > 0 || self.type_parameter_count > 0
    }

    fn add(self, other: Self) -> Self {
        Self {
            inference_variable_count: self.inference_variable_count
                + other.inference_variable_count,
            type_parameter_count: self.type_parameter_count + other.type_parameter_count,
        }
    }
}

#[derive(Default)]
pub struct BuiltinTypes {
    pub string: Option<TypeId>,
    pub buffer: Option<TypeId>,
    pub types_layout: Option<TypeId>,
    pub types_type_schema: Option<TypeId>,
    pub types_int_kind: Option<TypeId>,
    pub types_int_value: Option<TypeId>,
}

pub struct TypesConfig {
    pub ptr_size_bits: u32,
}

pub struct Types {
    pub types: Vec<Type>,
    /// We use this to efficiently check if we already have seen a type,
    /// and retrieve its ID if so. We used to iterate the pool but it
    /// got slow
    pub existing_types_mapping: FxHashMap<Type, TypeId>,

    /// AoS-style info associated with each type id
    pub layouts: Pool<Layout, TypeId>,
    pub type_variable_counts: Pool<TypeVariableInfo, TypeId>,

    /// Definition info for named types
    pub types_to_defns: FxHashMap<TypeId, TypeDefnId>,

    /// De-duped pool of type definitions
    pub existing_defns: FxHashMap<TypeDefnInfo, TypeDefnId>,
    pub defns: Pool<TypeDefnInfo, TypeDefnId>,

    /// Lookup mappings for parsed -> typed ids
    pub ast_type_defn_mapping: FxHashMap<ParsedTypeDefnId, TypeId>,
    pub ast_ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,

    pub builtins: BuiltinTypes,

    pub config: TypesConfig,
}

impl Types {
    pub fn empty() -> Types {
        Types {
            types: Vec::new(),
            layouts: Pool::new("layouts"),
            types_to_defns: FxHashMap::default(),
            existing_defns: FxHashMap::default(),
            defns: Pool::new("type_defns"),
            type_variable_counts: Pool::new("type_variable_counts"),
            existing_types_mapping: FxHashMap::default(),
            ast_type_defn_mapping: FxHashMap::default(),
            ast_ability_mapping: FxHashMap::default(),
            builtins: BuiltinTypes::default(),
            config: TypesConfig { ptr_size_bits: 64 },
        }
    }

    #[cfg(test)]
    pub fn with_builtin_types() -> Types {
        let mut this = Types::empty();
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

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        if let Some(&existing_type_id) = self.existing_types_mapping.get(&typ) {
            return existing_type_id;
        }

        let t = match typ {
            Type::Enum(e) => {
                // Enums and variants are self-referential
                // so we handle them specially
                let enum_type_id = self.add_or_resolve_enum(e, None);
                enum_type_id
            }
            Type::EnumVariant(_ev) => {
                panic!("EnumVariant cannot be directly interned; intern the Enum instead")
            }
            _ => {
                let type_id = self.next_type_id();
                self.types.push(typ.clone());
                self.existing_types_mapping.insert(typ, type_id);

                let variable_counts = self.count_type_variables(type_id);
                self.type_variable_counts.add(variable_counts);

                let layout = self.compute_type_layout(type_id);
                self.layouts.add(layout);

                type_id
            }
        };
        debug_assert_eq!(self.layouts.len(), self.types.len());
        debug_assert_eq!(self.type_variable_counts.len(), self.types.len());

        t
    }

    fn add_or_resolve_enum(&mut self, mut e: TypedEnum, type_id_to_use: Option<TypeId>) -> TypeId {
        // Enums and variants are self-referential
        // so we handle them specially
        let next_type_id = self.next_type_id();
        let variant_count = e.variants.len();
        let enum_type_id = match type_id_to_use {
            None => TypeId(next_type_id.0.saturating_add(variant_count as u32)),
            Some(type_id) => type_id,
        };

        let defn_id = e.defn_id;

        for v in e.variants.make_mut().iter_mut() {
            let variant_id = TypeId(next_type_id.0.saturating_add(v.index));
            v.my_type_id = variant_id;
            v.enum_type_id = enum_type_id;
            let variant = Type::EnumVariant(v.clone());
            self.types.push(variant.clone());
            self.existing_types_mapping.insert(variant, variant_id);
            let variant_variable_counts = self.count_type_variables(variant_id);
            self.type_variable_counts.add(variant_variable_counts);

            if let Some(defn_id) = defn_id {
                self.types_to_defns.insert(v.my_type_id, defn_id);
            }
        }

        self.existing_types_mapping.insert(Type::Enum(e.clone()), enum_type_id);
        match type_id_to_use {
            None => {
                // Inserting a new type
                self.types.push(Type::Enum(e));

                let variable_counts = self.count_type_variables(enum_type_id);
                self.type_variable_counts.add(variable_counts);
            }
            Some(_unresolved_type_id) => {
                // We're updating unresolved_type_id to point to the enum.
                *self.get_mut(enum_type_id) = Type::Enum(e);

                let variable_counts = self.count_type_variables(enum_type_id);
                *self.type_variable_counts.get_mut(enum_type_id) = variable_counts;
            }
        };

        // Set the layout for the enum, then insert for each variant
        let layout = self.compute_type_layout(enum_type_id);
        match type_id_to_use {
            None => {
                self.layouts.add(layout);
            }
            Some(_) => *self.layouts.get_mut(enum_type_id) = layout,
        };
        for _ in 0..variant_count {
            self.layouts.add(layout);
        }

        debug_assert_eq!(self.types.len(), self.layouts.len());
        debug_assert_eq!(self.types.len(), self.type_variable_counts.len());

        enum_type_id
    }

    pub fn add_unresolved_type_defn(
        &mut self,
        parsed_id: ParsedTypeDefnId,
        info: TypeDefnInfo,
    ) -> TypeId {
        let defn_id = self.add_defn(info);
        let type_id = self.add_type(Type::Unresolved(parsed_id));
        self.ast_type_defn_mapping.insert(parsed_id, type_id);
        self.types_to_defns.insert(type_id, defn_id);
        type_id
    }

    pub fn resolve_unresolved(&mut self, unresolved_type_id: TypeId, type_value: Type) {
        let typ = self.get_mut(unresolved_type_id);
        if typ.as_unresolved().is_none() {
            panic!("Tried to resolve a type that was not unresolved: {:?}", typ);
        }
        match type_value {
            Type::Enum(e) => {
                self.add_or_resolve_enum(e, Some(unresolved_type_id));
            }
            _ => {
                *typ = type_value.clone();
                // FIXME: Adding a type is a mess, since we have all these places to update
                // and we have to do it differently if we're resolving vs adding new.
                self.existing_types_mapping.insert(type_value, unresolved_type_id);

                let variable_counts = self.count_type_variables(unresolved_type_id);
                *self.type_variable_counts.get_mut(unresolved_type_id) = variable_counts;

                let layout = self.compute_type_layout(unresolved_type_id);
                *self.layouts.get_mut(unresolved_type_id) = layout;
            }
        };
    }

    pub fn next_type_id(&self) -> TypeId {
        // Safety: If you add one to a u32 it'll never be zero
        unsafe { TypeId(NonZeroU32::new_unchecked(self.types.len() as u32 + 1)) }
    }

    pub fn add_reference_type(&mut self, inner_type: TypeId) -> TypeId {
        self.add_anon(Type::Reference(ReferenceType { inner_type }))
    }

    pub fn add_static_type(
        &mut self,
        inner_type_id: TypeId,
        value_id: Option<StaticValueId>,
    ) -> TypeId {
        self.add_anon(Type::Static(StaticType { inner_type_id, value_id }))
    }

    pub fn add(&mut self, typ: Type, defn_id: Option<TypeDefnId>) -> TypeId {
        let type_id = self.add_type(typ);
        if let Some(defn_id) = defn_id {
            self.types_to_defns.insert(type_id, defn_id);
        };
        type_id
    }

    pub fn add_anon(&mut self, typ: Type) -> TypeId {
        self.add_type(typ)
    }

    pub fn add_defn(&mut self, info: TypeDefnInfo) -> TypeDefnId {
        if let Some(existing) = self.existing_defns.get(&info) {
            return *existing;
        }
        self.defns.add(info)
    }

    #[inline]
    pub fn get_no_follow(&self, type_id: TypeId) -> &Type {
        &self.types[type_id.0.get() as usize - 1]
    }

    #[inline]
    pub fn get_no_follow_static(&self, type_id: TypeId) -> &Type {
        match self.get_no_follow(type_id) {
            Type::RecursiveReference(rr) => self.get_no_follow_static(rr.root_type_id),
            t => t,
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

    pub fn get_type_variable(&self, type_id: TypeId) -> &TypeParameter {
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

    pub fn get_generic_instance_info(&self, type_id: TypeId) -> Option<&GenericInstanceInfo> {
        match self.get_no_follow(type_id) {
            Type::Enum(e) => e.generic_instance_info.as_ref(),
            Type::EnumVariant(ev) => self.get_generic_instance_info(ev.enum_type_id),
            Type::Struct(s) => s.generic_instance_info.as_ref(),
            Type::Static(_stat) => None,
            Type::Unit => None,
            Type::Char => None,
            Type::Integer(_) => None,
            Type::Float(_) => None,
            Type::Bool => None,
            Type::Pointer => None,
            Type::Reference(_) => None,
            Type::TypeParameter(_) => None,
            Type::FunctionTypeParameter(_) => None,
            Type::InferenceHole(_) => None,
            Type::Never => None,
            Type::Generic(_gen) => None,
            Type::Function(_) => None,
            Type::Lambda(_) => None,
            Type::LambdaObject(_) => None,
            Type::Unresolved(_) => None,
            Type::RecursiveReference(_) => None,
            Type::Array(_) => None,
        }
    }

    pub fn get_defn_id(&self, type_id: TypeId) -> Option<TypeDefnId> {
        self.types_to_defns.get(&type_id).copied()
    }

    pub fn get_defn_info(&self, type_id: TypeId) -> Option<TypeDefnInfo> {
        match self.types_to_defns.get(&type_id) {
            Some(info_id) => Some(*self.defns.get(*info_id)),
            None => None,
        }
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
            body_function_id,
            environment_struct,
        }));
        lambda_type_id
    }

    pub fn add_empty_struct(&mut self) -> TypeId {
        self.add_anon(Type::Struct(StructType {
            fields: eco_vec![],
            defn_id: None,
            generic_instance_info: None,
        }))
    }

    pub const LAMBDA_OBJECT_FN_PTR_INDEX: usize = 0;
    pub const LAMBDA_OBJECT_ENV_PTR_INDEX: usize = 1;

    pub fn add_lambda_object(
        &mut self,
        identifiers: &Identifiers,
        function_type_id: TypeId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let fn_ptr_type = self.add_reference_type(function_type_id);
        let fields = eco_vec![
            StructTypeField {
                name: identifiers.builtins.fn_ptr,
                type_id: fn_ptr_type,
                private: false,
            },
            StructTypeField {
                name: identifiers.builtins.env_ptr,
                type_id: POINTER_TYPE_ID,
                private: false,
            },
        ];
        let struct_representation = self.add_anon(Type::Struct(StructType {
            fields,
            defn_id: None,
            generic_instance_info: None,
        }));
        self.add_anon(Type::LambdaObject(LambdaObjectType {
            function_type: function_type_id,
            parsed_id,
            struct_representation,
        }))
    }

    pub fn get_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.types[type_id.0.get() as usize - 1]
    }

    pub fn swap(&mut self, type1: TypeId, type2: TypeId) {
        self.types.swap(type1.0.get() as usize - 1, type2.0.get() as usize - 1);
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
            Type::TypeParameter(_tp) => {
                TypeVariableInfo { type_parameter_count: 1, inference_variable_count: 0 }
            }
            Type::FunctionTypeParameter(ftp) => {
                let base_info =
                    TypeVariableInfo { type_parameter_count: 1, inference_variable_count: 0 };
                let fn_info = self.count_type_variables(ftp.function_type);
                base_info.add(fn_info)
            }
            Type::InferenceHole(_hole) => {
                TypeVariableInfo { type_parameter_count: 0, inference_variable_count: 1 }
            }
            Type::Unit => EMPTY,
            Type::Char => EMPTY,
            Type::Integer(_) => EMPTY,
            Type::Float(_) => EMPTY,
            Type::Bool => EMPTY,
            Type::Pointer => EMPTY,
            Type::Struct(struc) => {
                let mut result = EMPTY;
                for field in struc.fields.iter() {
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
                for param in fun.physical_params.iter() {
                    result = result.add(self.count_type_variables(param.type_id))
                }
                result = result.add(self.count_type_variables(fun.return_type));
                result
            }
            Type::Lambda(lambda) => self
                .count_type_variables(lambda.function_type)
                .add(self.count_type_variables(lambda.env_type)),
            // But a lambda object is generic if its function is generic
            Type::LambdaObject(co) => self.count_type_variables(co.function_type),
            Type::Static(stat) => self.count_type_variables(stat.inner_type_id),
            Type::Unresolved(_) => EMPTY,
            Type::RecursiveReference(rr) => {
                if let Type::Generic(generic) = self.get(rr.root_type_id) {
                    TypeVariableInfo {
                        inference_variable_count: 0,
                        type_parameter_count: generic.params.len() as u32,
                    }
                } else {
                    EMPTY
                }
            }
            Type::Array(arr) => {
                // Arrays contain type variables if their element type does
                self.count_type_variables(arr.element_type)
            }
        }
    }

    pub fn word_size_bits(&self) -> u32 {
        self.config.ptr_size_bits
    }

    pub fn get_struct_layout(&self, struct_type_id: TypeId) -> StructLayout {
        let struct_type = self.get(struct_type_id).expect_struct();
        let mut layout = Layout::ZERO;
        let mut field_offsets: SV8<u32> = smallvec![];
        for field in &struct_type.fields {
            let field_layout = self.get_layout(field.type_id);
            let field_offset = layout.append_to_aggregate(field_layout);
            field_offsets.push(field_offset)
        }
        StructLayout { layout, field_offsets }
    }

    pub fn compute_type_layout(&self, type_id: TypeId) -> Layout {
        const Z: Layout = Layout::ZERO;
        match self.get_no_follow(type_id) {
            Type::Unit => Layout::from_scalar_bytes(1),
            Type::Char => Layout::from_scalar_bytes(1),
            Type::Integer(integer_type) => Layout::from_scalar_bits(integer_type.width().bits()),
            Type::Float(float_type) => Layout::from_scalar_bits(float_type.size().bits()),
            Type::Bool => Layout::from_scalar_bytes(1),
            Type::Pointer => Layout::from_scalar_bits(self.word_size_bits()),
            Type::Struct(struct_type) => {
                let mut layout = Layout::ZERO;
                for field in &struct_type.fields {
                    let field_layout = self.compute_type_layout(field.type_id);
                    layout.append_to_aggregate(field_layout);
                }
                layout
            }
            Type::Reference(_reference_type) => Layout::from_scalar_bits(self.word_size_bits()),
            Type::Enum(typed_enum) => {
                let payload_layouts: Vec<Option<Layout>> = typed_enum
                    .variants
                    .iter()
                    .map(|v| v.payload.map(|p| self.compute_type_layout(p)))
                    .collect();

                // Enum sizing and layout rules:
                // - Alignment of the enum is the max(alignment) of the variants
                // - Size of the enum is the size of the largest variant, not necessarily the same
                //   variant, plus alignment end padding
                let tag_layout = self.compute_type_layout(typed_enum.tag_type);
                let mut max_variant_align = 0;
                let mut max_variant_size = 0;
                for (_variant, payload_layout) in typed_enum.variants.iter().zip(payload_layouts) {
                    let struct_repr = {
                        let mut l = Layout::ZERO;
                        l.append_to_aggregate(tag_layout);
                        if let Some(payload_layout) = payload_layout {
                            l.append_to_aggregate(payload_layout);
                        }
                        l
                    };
                    if struct_repr.align > max_variant_align {
                        max_variant_align = struct_repr.align
                    };
                    if struct_repr.size > max_variant_size {
                        max_variant_size = struct_repr.size
                    };
                }
                let enum_size = Layout { size: max_variant_size, align: max_variant_align };
                enum_size
            }
            Type::EnumVariant(variant) => self.compute_type_layout(variant.enum_type_id),
            Type::Never => Z,
            Type::Function(_) => Z,
            Type::Lambda(lambda_type) => self.compute_type_layout(lambda_type.env_type),
            Type::LambdaObject(lambda_object_type) => {
                self.compute_type_layout(lambda_object_type.struct_representation)
            }
            // Note: Eventually we will treat statics as ZSTs as an optimization;
            //       the value contains no information
            //       I am preparing for this by _NOT_ doing the lazy thing
            //       and allowing static and their underlying types to be interchanged
            //       But rather inserting explicit casts and conversions so that,
            //       once the physical type is not the same, we are already correct
            Type::Static(stat) => self.compute_type_layout(stat.inner_type_id),
            Type::Generic(_) => Z,
            Type::TypeParameter(_) => Z,
            Type::FunctionTypeParameter(_) => Z,
            Type::InferenceHole(_) => Z,
            Type::Unresolved(_) => Z,
            Type::RecursiveReference(_) => Z,
            Type::Array(arr) => {
                let element_layout = self.compute_type_layout(arr.element_type);
                Layout::array_me(&element_layout, arr.size as usize)
            }
        }
    }

    pub fn enum_variant_payload_offset_bytes(&self, ev: &TypedEnumVariant) -> usize {
        let mut layout = Layout::ZERO;
        layout.append_to_aggregate(self.get_layout(ev.tag_value.get_type()));
        let payload_start = layout.append_to_aggregate(self.get_layout(ev.payload.unwrap()));
        payload_start as usize
    }

    pub fn get_layout(&self, type_id: TypeId) -> Layout {
        *self.layouts.get(type_id)
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
            Type::Unit => false,
            Type::Char => false,
            Type::Integer(_) => false,
            Type::Float(_) => false,
            Type::Bool => false,
            Type::Pointer => false,
            Type::Reference(_) => false,
            Type::Never => false,
            Type::Function(_) => false,
            Type::Generic(_) => false,
            Type::TypeParameter(_) => false,
            Type::FunctionTypeParameter(_) => false,
            Type::InferenceHole(_) => false,
            Type::Unresolved(_) => false,
            Type::RecursiveReference(_) => false,
            Type::Array(_) => true,
        }
    }
}
