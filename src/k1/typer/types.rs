use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hasher;

use fxhash::FxHashMap;

use crate::typer::scopes::*;

use crate::parse::Identifier;
use crate::parse::{ParsedId, ParsedTypeDefnId};

use crate::typer::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub const PENDING: TypeId = TypeId(u32::MAX);

    pub fn to_u64(&self) -> u64 {
        self.0 as u64
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Identifier,
    pub type_id: TypeId,
    pub index: u32,
    pub private: bool,
}

#[derive(Debug, Clone)]
pub struct GenericInstanceInfo {
    pub generic_parent: TypeId,
    pub param_values: Vec<TypeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LoopType {
    Loop,
    While,
}

#[derive(Debug, Clone)]
pub struct TypeDefnInfo {
    pub name: Identifier,
    pub scope: ScopeId,
    // If there's a corresponding namespace for this type defn, this is it
    pub companion_namespace: Option<NamespaceId>,
    pub ast_id: ParsedId,
}

impl std::hash::Hash for TypeDefnInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.scope.hash(state);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClosureStructTypes {
    pub fn_type_id: TypeId,
    pub env_type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Vec<StructTypeField>,
    /// Populated for non-anonymous (named) structs
    pub type_defn_info: Option<TypeDefnInfo>,
    pub generic_instance_info: Option<GenericInstanceInfo>,
    pub ast_node: ParsedId,
}

impl StructType {
    pub fn is_named(&self) -> bool {
        self.type_defn_info.is_some()
    }

    pub fn is_anonymous(&self) -> bool {
        self.type_defn_info.is_none()
    }

    pub fn find_field(&self, field_name: Identifier) -> Option<(usize, &StructTypeField)> {
        self.fields.iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const U8_TYPE_ID: TypeId = TypeId(0);
pub const U16_TYPE_ID: TypeId = TypeId(1);
pub const U32_TYPE_ID: TypeId = TypeId(2);
pub const U64_TYPE_ID: TypeId = TypeId(3);
pub const I8_TYPE_ID: TypeId = TypeId(4);
pub const I16_TYPE_ID: TypeId = TypeId(5);
pub const I32_TYPE_ID: TypeId = TypeId(6);
pub const I64_TYPE_ID: TypeId = TypeId(7);

pub const UNIT_TYPE_ID: TypeId = TypeId(8);
pub const CHAR_TYPE_ID: TypeId = TypeId(9);
pub const BOOL_TYPE_ID: TypeId = TypeId(10);
pub const NEVER_TYPE_ID: TypeId = TypeId(11);
pub const POINTER_TYPE_ID: TypeId = TypeId(12);
pub const F32_TYPE_ID: TypeId = TypeId(13);
pub const F64_TYPE_ID: TypeId = TypeId(14);

pub const BUFFER_DATA_FIELD_NAME: &str = "data";
pub const BUFFER_TYPE_ID: TypeId = TypeId(18);

pub const LIST_TYPE_ID: TypeId = TypeId(23);
pub const STRING_TYPE_ID: TypeId = TypeId(26);
pub const OPTIONAL_TYPE_ID: TypeId = TypeId(31);
pub const COMPILER_SOURCE_LOC_TYPE_ID: TypeId = TypeId(32);
pub const ORDERING_TYPE_ID: TypeId = TypeId(36);

#[derive(Debug, Clone)]
pub struct ListType {
    pub element_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub name: Identifier,
    pub scope_id: ScopeId,
    pub span: SpanId,
    pub is_inference_variable: bool,
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
pub struct TypedEnumVariant {
    pub enum_type_id: TypeId,
    pub my_type_id: TypeId,
    pub name: Identifier,
    pub index: u32,
    pub payload: Option<TypeId>,
    // Always matches the info of this variant's enum
    pub type_defn_info: Option<TypeDefnInfo>,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub variants: Vec<TypedEnumVariant>,
    /// Populated for non-anonymous (named) enums
    pub type_defn_info: Option<TypeDefnInfo>,
    /// Populated for specialized copies of generic enums, contains provenance info
    pub generic_instance_info: Option<GenericInstanceInfo>,
    pub ast_node: ParsedId,
    pub explicit_tag_type: Option<TypeId>,
}

impl TypedEnum {
    pub fn variant_by_name(&self, name: Identifier) -> Option<&TypedEnumVariant> {
        self.variants.iter().find(|v| v.name == name)
    }
    pub fn variant_by_index(&self, index: u32) -> Option<&TypedEnumVariant> {
        self.variants.iter().find(|v| v.index == index)
    }
}

#[derive(Debug, Clone)]
pub struct OpaqueTypeAlias {
    pub ast_id: ParsedTypeDefnId,
    pub aliasee: TypeId,
    pub type_defn_info: TypeDefnInfo,
}

#[derive(Debug, Clone)]
pub struct GenericTypeParam {
    pub name: Identifier,
    pub type_id: TypeId,
    pub span: SpanId,
}

impl NamedType for GenericTypeParam {
    fn name(&self) -> Identifier {
        self.name
    }
    fn type_id(&self) -> TypeId {
        self.type_id
    }
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub params: Vec<GenericTypeParam>,
    pub inner: TypeId,
    pub type_defn_info: TypeDefnInfo,
    pub specializations: HashMap<Vec<TypeId>, TypeId>,
}

impl GenericType {
    pub fn name(&self) -> Identifier {
        self.type_defn_info.name
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct FloatType {
    pub size: NumericWidth,
    pub defn_info: TypeDefnInfo,
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerType::U8 => write!(f, "u8"),
            IntegerType::U16 => write!(f, "u16"),
            IntegerType::U32 => write!(f, "u32"),
            IntegerType::U64 => write!(f, "u64"),
            IntegerType::I8 => write!(f, "i8"),
            IntegerType::I16 => write!(f, "i16"),
            IntegerType::I32 => write!(f, "i32"),
            IntegerType::I64 => write!(f, "i64"),
        }
    }
}

impl IntegerType {
    pub fn type_id(&self) -> TypeId {
        match self {
            IntegerType::U8 => U8_TYPE_ID,
            IntegerType::U16 => U16_TYPE_ID,
            IntegerType::U32 => U32_TYPE_ID,
            IntegerType::U64 => U64_TYPE_ID,
            IntegerType::I8 => I8_TYPE_ID,
            IntegerType::I16 => I16_TYPE_ID,
            IntegerType::I32 => I32_TYPE_ID,
            IntegerType::I64 => I64_TYPE_ID,
        }
    }

    pub fn width(&self) -> NumericWidth {
        match self {
            IntegerType::U8 | IntegerType::I8 => NumericWidth::B8,
            IntegerType::U16 | IntegerType::I16 => NumericWidth::B16,
            IntegerType::U32 | IntegerType::I32 => NumericWidth::B32,
            IntegerType::U64 | IntegerType::I64 => NumericWidth::B64,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            IntegerType::U8 | IntegerType::U16 | IntegerType::U32 | IntegerType::U64 => false,
            IntegerType::I8 | IntegerType::I16 | IntegerType::I32 | IntegerType::I64 => true,
        }
    }
}

// pub struct Spanned<T> {
//     pub v: T,
//     pub span: SpanId,
// }

#[derive(Debug, Clone)]
pub struct FnParamType {
    // FIXME: Determine if param names are truly 'part' of the function type.
    // For now, keeping them to fix some bugs
    pub name: Identifier,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_closure_env: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<FnParamType>,
    pub return_type: TypeId,
    pub defn_info: Option<TypeDefnInfo>,
}

#[derive(Debug, Clone)]
pub struct RecursiveReference {
    pub parsed_id: ParsedTypeDefnId,
    pub root_type_id: TypeId,
}

impl RecursiveReference {
    pub fn is_pending(&self) -> bool {
        self.root_type_id == TypeId::PENDING
    }
}

#[derive(Debug, Clone)]
pub struct LambdaType {
    pub function_type: TypeId,
    pub env_type: TypeId,
    pub parsed_id: ParsedId,
    pub body_function_id: FunctionId,
    // This kinda crosses the streams; its a value expression in a type, but
    // that's because a closure's environment is basically values baked into a function
    // Its almost like a comptime-known value, aka the type 5
    pub environment_struct: TypedExpr,
    pub closure_object_type: TypeId,
}

/// I'd love for this to just become 'Dynamic' type
#[derive(Debug, Clone)]
pub struct LambdaObjectType {
    pub function_type: TypeId,
    pub parsed_id: ParsedId,
    pub struct_representation: TypeId,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit(TypeDefnInfo),
    Char(TypeDefnInfo),
    Integer(IntegerType),
    Float(FloatType),
    Bool(TypeDefnInfo),
    /// Our Pointer is a raw untyped pointer; we mostly have this type for expressing intent
    /// and because it allows us to treat it as a ptr in LLVM which
    /// allows for pointer-reasoning based optimizations
    Pointer(TypeDefnInfo),
    Struct(StructType),
    Reference(ReferenceType),
    Enum(TypedEnum),
    /// Enum variants are proper types of their own, for lots
    /// of reasons that make programming nice. Unlike in Rust :()
    EnumVariant(TypedEnumVariant),
    /// The 'bottom', uninhabited type; used to indicate exits of the program
    Never(TypeDefnInfo),
    Function(FunctionType),
    Closure(LambdaType),
    ClosureObject(LambdaObjectType),
    // Less physical types
    Generic(GenericType),
    #[allow(clippy::enum_variant_names)]
    TypeVariable(TypeVariable),
    RecursiveReference(RecursiveReference),
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unit(_), Type::Unit(_)) => true,
            (Type::Char(_), Type::Char(_)) => true,
            (Type::Integer(int1), Type::Integer(int2)) => int1 == int2,
            (Type::Float(f1), Type::Float(f2)) => f1.size == f2.size,
            (Type::Bool(_), Type::Bool(_)) => true,
            (Type::Pointer(_), Type::Pointer(_)) => true,
            (Type::Struct(s1), Type::Struct(s2)) => {
                if s1.is_named() || s2.is_named() {
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
            (Type::TypeVariable(t1), Type::TypeVariable(t2)) => {
                t1.name == t2.name && t1.scope_id == t2.scope_id
            }
            (Type::Enum(e1), Type::Enum(e2)) => {
                if e1.type_defn_info.is_some() || e2.type_defn_info.is_some() {
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
            (Type::Never(_), Type::Never(_)) => true,
            // We never really want to de-dupe this type as its inherently unique
            (Type::Generic(_g1), Type::Generic(_g2)) => false,
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.return_type == f2.return_type && f1.params.len() == f2.params.len() {
                    return f1
                        .params
                        .iter()
                        .zip(f2.params.iter())
                        .all(|(p1, p2)| p1.type_id == p2.type_id);
                };
                false
            }
            (Type::Closure(_c1), Type::Closure(_c2)) => false,
            (Type::ClosureObject(_co1), Type::ClosureObject(_co2)) => false,
            (Type::RecursiveReference(rr1), Type::RecursiveReference(rr2)) => {
                rr1.root_type_id == rr2.root_type_id
            }
            _ => false,
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Unit(_) => "unit".hash(state),
            Type::Char(_) => "char".hash(state),
            Type::Integer(int1) => match int1 {
                IntegerType::U8 => "u8".hash(state),
                IntegerType::U16 => "u16".hash(state),
                IntegerType::U32 => "u32".hash(state),
                IntegerType::U64 => "u64".hash(state),
                IntegerType::I8 => "i8".hash(state),
                IntegerType::I16 => "i16".hash(state),
                IntegerType::I32 => "i32".hash(state),
                IntegerType::I64 => "i64".hash(state),
            },
            Type::Bool(_) => "bool".hash(state),
            Type::Struct(s) => {
                "struct".hash(state);
                if let Some(type_defn_info) = &s.type_defn_info {
                    type_defn_info.hash(state);
                } else {
                    s.fields.len().hash(state);
                    for f in &s.fields {
                        f.name.hash(state);
                        f.type_id.hash(state);
                    }
                }
            }
            Type::Reference(r) => {
                "ref".hash(state);
                r.inner_type.hash(state);
            }
            Type::TypeVariable(tvar) => {
                "tvar".hash(state);
                tvar.name.hash(state);
                tvar.scope_id.hash(state);
            }
            Type::Enum(e) => {
                "enum".hash(state);
                if let Some(type_defn_info) = &e.type_defn_info {
                    type_defn_info.hash(state);
                } else {
                    e.variants.len().hash(state);
                    for v in e.variants.iter() {
                        v.name.hash(state);
                        v.payload.hash(state);
                    }
                }
            }
            // We never really want to de-dupe this type as its inherently unique
            Type::EnumVariant(variant) => {
                "variant".hash(state);
                variant.enum_type_id.hash(state);
                variant.name.hash(state);
                variant.payload.hash(state);
            }
            Type::Generic(gen) => {
                "gen".hash(state);
                gen.type_defn_info.hash(state);
            }
            Type::Function(fun) => {
                "fun".hash(state);
                fun.return_type.hash(state);
                for param in &fun.params {
                    param.name.hash(state);
                    param.is_context.hash(state);
                    param.is_closure_env.hash(state);
                    param.type_id.hash(state);
                }
            }
            Type::Closure(c) => {
                "closure".hash(state);
                c.parsed_id.hash(state);
                c.function_type.hash(state)
            }
            Type::Float(ft) => {
                "float".hash(state);
                ft.size.bit_width().hash(state);
            }
            Type::Pointer(_) => {
                "ptr".hash(state);
            }
            Type::Never(_) => "never".hash(state),
            Type::ClosureObject(co) => {
                "closure_object".hash(state);
                co.function_type.hash(state);
                co.struct_representation.hash(state);
            }
            Type::RecursiveReference(rr) => {
                "recurse".hash(state);
                rr.root_type_id.hash(state);
            }
        }
    }
}

impl Type {
    pub fn kind_name(&self) -> &'static str {
        match self {
            Type::Unit(_) => "scalar",
            Type::Char(_) => "scalar",
            Type::Integer(_) => "scalar",
            Type::Float(_) => "scalar",
            Type::Bool(_) => "scalar",
            Type::Pointer(_) => "pointer",
            Type::Struct(_) => "struct",
            Type::Reference(_) => "reference",
            Type::TypeVariable(_) => "tvar",
            Type::Enum(_) => "enum",
            Type::EnumVariant(_) => "variant",
            Type::Never(_) => "never",
            Type::Generic(_) => "generic",
            Type::Function(_) => "function",
            Type::Closure(_) => "closure",
            Type::ClosureObject(_) => "closure obj",
            Type::RecursiveReference(_) => "recurse",
        }
    }

    // Note: This is kind of a codegen concern that doesn't belong in this layer,
    //       but it has some implications for typechecking, and I'm not super worried
    //       about platform independence in the middle-end right now
    pub fn is_scalar_int_value(&self) -> bool {
        matches!(self, Type::Unit(_) | Type::Char(_) | Type::Integer(_) | Type::Bool(_))
    }

    pub fn ast_node(&self) -> Option<ParsedId> {
        match self {
            Type::Unit(defn_info)
            | Type::Char(defn_info)
            | Type::Bool(defn_info)
            | Type::Pointer(defn_info)
            | Type::Never(defn_info) => Some(defn_info.ast_id),
            Type::Float(ft) => Some(ft.defn_info.ast_id),
            Type::Integer(_) => None,
            Type::Struct(t) => Some(t.ast_node),
            Type::Reference(_t) => None,
            Type::TypeVariable(_t) => None,
            Type::Enum(e) => Some(e.ast_node),
            Type::EnumVariant(_ev) => None,
            Type::Generic(gen) => Some(gen.type_defn_info.ast_id),
            Type::Function(_fun) => None,
            Type::Closure(clos) => Some(clos.parsed_id),
            Type::ClosureObject(clos_obj) => Some(clos_obj.parsed_id),
            Type::RecursiveReference(r) => Some(ParsedId::TypeDefn(r.parsed_id)),
        }
    }

    pub fn defn_info(&self) -> Option<&TypeDefnInfo> {
        match self {
            Type::Unit(defn_info)
            | Type::Char(defn_info)
            | Type::Bool(defn_info)
            | Type::Pointer(defn_info)
            | Type::Never(defn_info) => Some(defn_info),
            Type::Float(ft) => Some(&ft.defn_info),
            Type::Integer(_) => None,
            Type::Struct(t) => t.type_defn_info.as_ref(),
            Type::Reference(_t) => None,
            Type::TypeVariable(_t) => None,
            Type::Enum(e) => e.type_defn_info.as_ref(),
            Type::EnumVariant(ev) => ev.type_defn_info.as_ref(),
            Type::Generic(gen) => Some(&gen.type_defn_info),
            Type::Function(_fun) => None,
            Type::Closure(_clos) => None,
            Type::ClosureObject(_clos_obj) => None,
            Type::RecursiveReference(_r) => None,
        }
    }

    pub fn as_list_instance(&self) -> Option<ListType> {
        if let Type::Struct(s) = self {
            s.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == LIST_TYPE_ID {
                    Some(ListType { element_type: spec_info.param_values[0] })
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
                if spec_info.generic_parent == BUFFER_TYPE_ID {
                    Some(spec_info)
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub fn as_optional(&self) -> Option<OptionalType> {
        if let Type::Enum(e) = self {
            e.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == OPTIONAL_TYPE_ID {
                    Some(OptionalType { inner_type: spec_info.param_values[0] })
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
    pub fn expect_reference(&self) -> &ReferenceType {
        match self {
            Type::Reference(r) => r,
            _ => panic!("expect_reference called on: {:?}", self),
        }
    }

    pub fn expect_enum_mut(&mut self) -> &mut TypedEnum {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum type"),
        }
    }

    pub fn expect_enum_variant(&self) -> &TypedEnumVariant {
        match self {
            Type::EnumVariant(v) => v,
            _ => panic!("expected enum variant type"),
        }
    }

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

    pub fn expect_enum(&self) -> &TypedEnum {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum on {:?}", self),
        }
    }

    pub fn expect_optional(&self) -> OptionalType {
        self.as_optional().unwrap()
    }

    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            Type::Struct(struc) => Some(struc),
            _ => None,
        }
    }

    pub fn expect_struct(&self) -> &StructType {
        match self {
            Type::Struct(struc) => struc,
            _ => panic!("expect_struct called on: {:?}", self),
        }
    }

    pub fn expect_generic(&self) -> &GenericType {
        match self {
            Type::Generic(g) => g,
            _ => panic!("expect_generic called on: {:?}", self),
        }
    }

    pub fn expect_integer(&self) -> &IntegerType {
        match self {
            Type::Integer(int) => int,
            _ => panic!("expect_integer called on: {:?}", self),
        }
    }

    pub fn as_tvar(&self) -> Option<&TypeVariable> {
        match self {
            Type::TypeVariable(tvar) => Some(tvar),
            _ => None,
        }
    }

    pub fn as_recursive_reference(&mut self) -> &mut RecursiveReference {
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

    pub fn as_closure(&self) -> Option<&LambdaType> {
        match self {
            Type::Closure(c) => Some(c),
            _ => None,
        }
    }
    pub fn as_closure_object(&self) -> Option<&LambdaObjectType> {
        match self {
            Type::ClosureObject(co) => Some(co),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TypeVariableInfo {
    pub inference_variable_count: u32,
    pub type_variable_count: u32,
}

impl TypeVariableInfo {
    const EMPTY: TypeVariableInfo =
        TypeVariableInfo { inference_variable_count: 0, type_variable_count: 0 };

    fn add(self, other: Self) -> Self {
        Self {
            inference_variable_count: self.inference_variable_count
                + other.inference_variable_count,
            type_variable_count: self.type_variable_count + other.type_variable_count,
        }
    }
}

#[derive(Debug)]
pub struct Types {
    pub types: Vec<Type>,
    /// We use this to efficiently check if we already have seen a type,
    /// and retrieve its ID if so. We used to iterate the pool but it
    /// got slow
    pub existing_types_mapping: FxHashMap<Type, TypeId>,
    pub type_defn_mapping: FxHashMap<ParsedTypeDefnId, TypeId>,
    pub type_variable_counts: FxHashMap<TypeId, TypeVariableInfo>,
    pub ability_mapping: FxHashMap<ParsedAbilityId, AbilityId>,
    pub placeholder_mapping: FxHashMap<ParsedTypeDefnId, TypeId>,
}

impl Types {
    pub fn add_type_ext(&mut self, typ: Type, dedupe: bool) -> TypeId {
        if dedupe {
            if let Some(&existing_type_id) = self.existing_types_mapping.get(&typ) {
                return existing_type_id;
            }
        }

        let type_id = match typ {
            Type::Enum(mut e) => {
                // Enums and variants are self-referential
                // so we handle them specially
                let next_type_id = self.next_type_id();
                let enum_type_id = TypeId(next_type_id.0 + e.variants.len() as u32);

                for v in e.variants.iter_mut() {
                    let variant_id = TypeId(next_type_id.0 + v.index);
                    v.my_type_id = variant_id;
                    v.enum_type_id = enum_type_id;
                    let variant = Type::EnumVariant(v.clone());
                    self.types.push(variant.clone());
                    self.existing_types_mapping.insert(variant, variant_id);
                    let variant_variable_counts = self.count_type_variables(variant_id);
                    self.type_variable_counts.insert(variant_id, variant_variable_counts);
                }

                let e = Type::Enum(e);
                self.types.push(e.clone());
                self.existing_types_mapping.insert(e, enum_type_id);
                let variable_counts = self.count_type_variables(enum_type_id);
                self.type_variable_counts.insert(enum_type_id, variable_counts);
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
                self.type_variable_counts.insert(type_id, variable_counts);
                type_id
            }
        };

        type_id
    }

    pub fn next_type_id(&self) -> TypeId {
        TypeId(self.types.len() as u32)
    }

    pub fn add_reference_type(&mut self, inner_type: TypeId) -> TypeId {
        self.add_type(Type::Reference(ReferenceType { inner_type }))
    }

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        self.add_type_ext(typ, true)
    }

    #[inline]
    pub fn get_no_follow(&self, type_id: TypeId) -> &Type {
        &self.types[type_id.0 as usize]
    }

    #[inline]
    pub fn get(&self, type_id: TypeId) -> &Type {
        match self.get_no_follow(type_id) {
            Type::RecursiveReference(rr) => self.get(rr.root_type_id),
            t => t,
        }
    }

    pub fn get_type_variable(&self, type_id: TypeId) -> &TypeVariable {
        if let Type::TypeVariable(tv) = self.get(type_id) {
            tv
        } else {
            panic!("Expected type variable on type {}", type_id)
        }
    }

    pub fn get_type_variable_mut(&mut self, type_id: TypeId) -> &mut TypeVariable {
        if let Type::TypeVariable(tv) = self.get_mut(type_id) {
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
            Type::Unit(_) => None,
            Type::Char(_) => None,
            Type::Integer(_) => None,
            Type::Float(_) => None,
            Type::Bool(_) => None,
            Type::Pointer(_) => None,
            Type::Reference(_) => None,
            Type::TypeVariable(_) => None,
            Type::Never(_) => None,
            Type::Generic(_gen) => None,
            Type::Function(_) => None,
            Type::Closure(_) => None,
            Type::ClosureObject(_) => None,
            Type::RecursiveReference(_) => None,
        }
    }

    pub fn get_type_defn_info(&self, type_id: TypeId) -> Option<&TypeDefnInfo> {
        self.get(type_id).defn_info()
    }

    pub fn add_closure(
        &mut self,
        identifiers: &Identifiers,
        function_type_id: TypeId,
        environment_struct: TypedExpr,
        body_function_id: FunctionId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let closure_type_id = self.add_type(Type::Closure(LambdaType {
            function_type: function_type_id,
            env_type: environment_struct.get_type(),
            parsed_id,
            body_function_id,
            environment_struct,
            closure_object_type: TypeId::PENDING,
        }));
        let closure_object_type = self.add_closure_object(identifiers, function_type_id, parsed_id);
        if let Type::Closure(c) = self.get_mut(closure_type_id) {
            c.closure_object_type = closure_object_type;
        }
        closure_type_id
    }

    pub fn add_closure_object(
        &mut self,
        identifiers: &Identifiers,
        function_type_id: TypeId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let fn_ptr_type = self.add_reference_type(function_type_id);
        let env_type = self.add_type(Type::Struct(StructType {
            fields: vec![],
            type_defn_info: None,
            generic_instance_info: None,
            ast_node: parsed_id,
        }));
        let env_ptr_type = self.add_reference_type(env_type);
        let fields = vec![
            StructTypeField {
                name: identifiers.get("fn_ptr").unwrap(),
                type_id: fn_ptr_type,
                index: 0,
                private: false,
            },
            StructTypeField {
                name: identifiers.get("env_ptr").unwrap(),
                type_id: env_ptr_type,
                index: 1,
                private: false,
            },
        ];
        let struct_representation = self.add_type(Type::Struct(StructType {
            fields,
            type_defn_info: None,
            generic_instance_info: None,
            ast_node: parsed_id,
        }));
        self.add_type(Type::ClosureObject(LambdaObjectType {
            function_type: function_type_id,
            parsed_id,
            struct_representation,
        }))
    }

    pub fn get_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.types[type_id.0 as usize]
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types.iter().enumerate().map(|(i, t)| (TypeId(i as u32), t))
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

    pub fn add_type_defn_mapping(
        &mut self,
        type_defn_id: ParsedTypeDefnId,
        type_id: TypeId,
    ) -> bool {
        self.type_defn_mapping.insert(type_defn_id, type_id).is_none()
    }

    pub fn find_type_defn_mapping(&mut self, type_defn_id: ParsedTypeDefnId) -> Option<TypeId> {
        self.type_defn_mapping.get(&type_defn_id).copied()
    }

    pub fn add_ability_mapping(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
        ability_id: AbilityId,
    ) -> bool {
        self.ability_mapping.insert(parsed_ability_id, ability_id).is_none()
    }

    pub fn find_ability_mapping(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
    ) -> Option<AbilityId> {
        self.ability_mapping.get(&parsed_ability_id).copied()
    }

    pub fn get_type_variable_info(&self, type_id: TypeId) -> TypeVariableInfo {
        *self.type_variable_counts.get(&type_id).unwrap()
    }

    /// Recursively checks if given type contains any type variables
    /// Note: We could cache whether or not a type is generic on insertion into the type pool
    ///       But types are not immutable so this could be a dangerous idea!
    pub fn count_type_variables(&self, type_id: TypeId) -> TypeVariableInfo {
        const EMPTY: TypeVariableInfo = TypeVariableInfo::EMPTY;
        // nocommit: I don't think we can trust this; since it assumes no type variables
        //           occur in the rhs type independently, which may not be true. Well, where
        //           would they come from, maybe it is fine.
        //if let Some(spec_info) = self.get_generic_instance_info(type_id) {
        //    return spec_info
        //        .param_values
        //        .iter()
        //        .fold(EMPTY, |t1, type_id| t1.add(self.count_type_variables(*type_id)));
        //}
        match self.get_no_follow(type_id) {
            Type::TypeVariable(tv) => TypeVariableInfo {
                type_variable_count: 1,
                inference_variable_count: if tv.is_inference_variable { 1 } else { 0 },
            },
            Type::Unit(_) => EMPTY,
            Type::Char(_) => EMPTY,
            Type::Integer(_) => EMPTY,
            Type::Float(_) => EMPTY,
            Type::Bool(_) => EMPTY,
            Type::Pointer(_) => EMPTY,
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
            Type::Never(_) => EMPTY,
            // The real answer here would be, all the type variables on the RHS that aren't one of
            // the params. In other words, all FREE type variables
            Type::Generic(_gen) => EMPTY,
            Type::Function(fun) => {
                let mut result = EMPTY;
                for param in fun.params.iter() {
                    result = result.add(self.count_type_variables(param.type_id))
                }
                result = result.add(self.count_type_variables(fun.return_type));
                result
            }
            Type::Closure(closure) => self
                .count_type_variables(closure.function_type)
                .add(self.count_type_variables(closure.env_type)),
            // But a closure object is generic if its function is generic
            Type::ClosureObject(co) => self.count_type_variables(co.function_type),
            Type::RecursiveReference(_rr) => EMPTY,
        }
    }
}
