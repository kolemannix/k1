use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::typer::scopes::*;

use crate::parse::IdentifierId;
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
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: IdentifierId,
    pub type_id: TypeId,
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct TypeDefnInfo {
    pub name: IdentifierId,
    pub scope: ScopeId,
    // If there's a corresponding namespace for this type defn, this is it
    pub companion_namespace: Option<NamespaceId>,
    pub generic_parent: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Vec<StructTypeField>,
    /// Populated for non-anonymous (named) structs
    pub type_defn_info: Option<TypeDefnInfo>,
    pub ast_node: ParsedId,
}

impl StructType {
    pub fn is_named(&self) -> bool {
        self.type_defn_info.is_some()
    }

    pub fn is_anonymous(&self) -> bool {
        self.type_defn_info.is_none()
    }

    pub fn find_field(&self, field_name: IdentifierId) -> Option<(usize, &StructTypeField)> {
        self.fields.iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const UNIT_TYPE_ID: TypeId = TypeId(0);
pub const CHAR_TYPE_ID: TypeId = TypeId(1);
pub const BOOL_TYPE_ID: TypeId = TypeId(2);
pub const STRING_TYPE_ID: TypeId = TypeId(3);
pub const NEVER_TYPE_ID: TypeId = TypeId(4);
pub const U8_TYPE_ID: TypeId = TypeId(5);
pub const U16_TYPE_ID: TypeId = TypeId(6);
pub const U32_TYPE_ID: TypeId = TypeId(7);
pub const U64_TYPE_ID: TypeId = TypeId(8);
pub const I8_TYPE_ID: TypeId = TypeId(9);
pub const I16_TYPE_ID: TypeId = TypeId(10);
pub const I32_TYPE_ID: TypeId = TypeId(11);
pub const I64_TYPE_ID: TypeId = TypeId(12);
pub const RAW_POINTER_TYPE_ID: TypeId = TypeId(13);

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub name: IdentifierId,
    pub scope_id: ScopeId,
    /// This is where trait bounds would go
    pub _constraints: Option<Vec<()>>,
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
pub struct TagInstanceType {
    pub ident: IdentifierId,
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariant {
    pub enum_type_id: TypeId,
    pub my_type_id: TypeId,
    pub tag_name: IdentifierId,
    pub index: u32,
    pub payload: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub variants: Vec<TypedEnumVariant>,
    /// Populated for non-anonymous (named) enums
    pub type_defn_info: Option<TypeDefnInfo>,
    pub ast_node: ParsedId,
}

impl TypedEnum {
    pub fn variant_by_name(&self, name: IdentifierId) -> Option<&TypedEnumVariant> {
        self.variants.iter().find(|v| v.tag_name == name)
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
    pub name: IdentifierId,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub params: Vec<GenericTypeParam>,
    pub inner: TypeId,
    pub ast_id: ParsedTypeDefnId,
    pub type_defn_info: TypeDefnInfo,
    pub specializations: HashMap<Vec<TypeId>, TypeId>,
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

    pub fn bit_width(&self) -> u32 {
        match self {
            IntegerType::U8 | IntegerType::I8 => 8,
            IntegerType::U16 | IntegerType::I16 => 16,
            IntegerType::U32 | IntegerType::I32 => 32,
            IntegerType::U64 | IntegerType::I64 => 64,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            IntegerType::U8 | IntegerType::U16 | IntegerType::U32 | IntegerType::U64 => false,
            IntegerType::I8 | IntegerType::I16 | IntegerType::I32 | IntegerType::I64 => true,
        }
    }
}

pub struct Spanned<T> {
    pub v: T,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FnArgType {
    pub type_id: TypeId,
    pub name: IdentifierId,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<FnArgType>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Char,
    Integer(IntegerType),
    Bool,
    String,
    Struct(StructType),
    Array(ArrayType),
    Optional(OptionalType),
    Reference(ReferenceType),
    #[allow(clippy::enum_variant_names)]
    TypeVariable(TypeVariable),
    TagInstance(TagInstanceType),
    Enum(TypedEnum),
    // Enum variants are proper types of their own, for lots
    // of reasons that make programming nice. Unlike in Rust :()
    EnumVariant(TypedEnumVariant),
    Never,
    OpaqueAlias(OpaqueTypeAlias),
    Generic(GenericType),
    Function(FunctionType),
}

impl Type {
    pub fn ast_node(&self) -> Option<ParsedId> {
        match self {
            Type::Unit | Type::Char | Type::Integer(_) | Type::Bool | Type::String => None,
            Type::Struct(t) => Some(t.ast_node),
            Type::Array(_a) => None,
            Type::Optional(_t) => None,
            Type::Reference(_t) => None,
            Type::TypeVariable(_t) => None,
            Type::TagInstance(_t) => None,
            Type::Enum(e) => Some(e.ast_node),
            Type::EnumVariant(_ev) => None,
            Type::Never => None,
            Type::OpaqueAlias(opaque) => Some(opaque.ast_id.into()),
            Type::Generic(gen) => Some(gen.ast_id.into()),
            Type::Function(_fun) => None,
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
    pub fn as_optional(&self) -> Option<&OptionalType> {
        match self {
            Type::Optional(opt) => Some(opt),
            _ => None,
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
            _ => panic!("expected enum"),
        }
    }

    pub fn expect_optional(&self) -> &OptionalType {
        match self {
            Type::Optional(opt) => opt,
            _ => panic!("expect_optional called on: {:?}", self),
        }
    }

    pub fn expect_array(&self) -> &ArrayType {
        match self {
            Type::Array(array) => array,
            _ => panic!("expect_array called on: {:?}", self),
        }
    }

    pub fn as_array(&self) -> Option<&ArrayType> {
        match self {
            Type::Array(array) => Some(array),
            _ => None,
        }
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

    fn as_tag(&self) -> Option<&TagInstanceType> {
        match self {
            Type::TagInstance(tag) => Some(tag),
            _ => None,
        }
    }

    pub fn expect_generic(&self) -> &GenericType {
        match self {
            Type::Generic(g) => g,
            _ => panic!("expect_generic called on: {:?}", self),
        }
    }

    pub fn defn_info(&self) -> Option<&TypeDefnInfo> {
        match self {
            Type::Unit => None,
            Type::Char => None,
            Type::Integer(_) => None,
            Type::Bool => None,
            Type::String => None,
            Type::Struct(s) => s.type_defn_info.as_ref(),
            Type::Array(_) => None,
            Type::Optional(_) => None,
            Type::Reference(_) => None,
            Type::TypeVariable(_) => None,
            Type::TagInstance(_) => None,
            Type::Enum(e) => e.type_defn_info.as_ref(),
            Type::EnumVariant(_) => None,
            Type::Never => None,
            Type::OpaqueAlias(opaque) => Some(&opaque.type_defn_info),
            Type::Generic(gen) => Some(&gen.type_defn_info),
            Type::Function(_) => None,
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
}

#[derive(Default, Debug)]
pub struct Types {
    pub types: Vec<Type>,
    pub type_defn_mapping: HashMap<ParsedTypeDefnId, TypeId>,
}

impl Types {
    pub fn add_type_ext(&mut self, typ: Type, dedupe: bool) -> TypeId {
        if dedupe {
            for (existing_type_id, existing_type) in self.iter() {
                if TypedModule::type_eq(existing_type, &typ) {
                    return existing_type_id;
                }
            }
        }

        match typ {
            Type::Enum(mut e) => {
                // Enums and variants are self-referential
                // so we handle them specially
                let next_type_id = self.next_type_id();
                let enum_type_id = TypeId(next_type_id.0 + e.variants.len() as u32);

                for v in e.variants.iter_mut() {
                    let variant_id = TypeId(next_type_id.0 + v.index);
                    v.my_type_id = variant_id;
                    v.enum_type_id = enum_type_id;
                    self.types.push(Type::EnumVariant(v.clone()));
                }

                self.types.push(Type::Enum(e));
                enum_type_id
            }
            Type::EnumVariant(_ev) => {
                panic!("EnumVariant cannot be directly interned; intern the Enum instead")
            }
            _ => {
                let type_id = self.next_type_id();
                self.types.push(typ);
                type_id
            }
        }
    }

    pub fn next_type_id(&self) -> TypeId {
        TypeId(self.types.len() as u32)
    }

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        self.add_type_ext(typ, true)
    }

    pub fn get(&self, type_id: TypeId) -> &Type {
        &self.types[type_id.0 as usize]
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

    /// Recursively checks if given type contains any type variables
    /// TODO: Cache whether or not a type is generic on insertion into the type pool
    pub fn does_type_reference_type_variables(&self, type_id: TypeId) -> bool {
        match self.get(type_id) {
            Type::Unit => false,
            Type::Char => false,
            Type::Integer(_) => false,
            Type::Bool => false,
            Type::String => false,
            Type::Array(arr) => self.does_type_reference_type_variables(arr.element_type),
            // We don't _yet_ support generics in structs
            Type::Struct(struc) => {
                for field in struc.fields.iter() {
                    if self.does_type_reference_type_variables(field.type_id) {
                        return true;
                    }
                }
                return false;
            }
            Type::Optional(opt) => self.does_type_reference_type_variables(opt.inner_type),
            Type::Reference(refer) => self.does_type_reference_type_variables(refer.inner_type),
            Type::TypeVariable(_) => true,
            Type::TagInstance(_) => false,
            // We don't _yet_ support generics in enums
            Type::Enum(_) => false,
            Type::EnumVariant(_) => false,
            Type::Never => false,
            Type::OpaqueAlias(opaque) => self.does_type_reference_type_variables(opaque.aliasee),
            Type::Generic(_gen) => true,
            Type::Function(fun) => {
                for param in fun.params.iter() {
                    if self.does_type_reference_type_variables(param.type_id) {
                        return true;
                    }
                }
                if self.does_type_reference_type_variables(fun.return_type) {
                    return true;
                }
                return false;
            }
        }
    }

    pub fn item_type_of_iterable(&self, type_id: TypeId) -> Option<TypeId> {
        match self.get(type_id) {
            Type::Unit => None,
            Type::Char => None,
            Type::Integer(_) => None,
            Type::Bool => None,
            Type::String => Some(CHAR_TYPE_ID),
            Type::Array(arr) => Some(arr.element_type),
            Type::Struct(_struct) => None,
            Type::Optional(_opt) => None,
            Type::Reference(_refer) => None,
            Type::TypeVariable(_) => None,
            Type::TagInstance(_) => None,
            Type::Enum(_) => None,
            Type::EnumVariant(_) => None,
            Type::Never => None,
            Type::OpaqueAlias(_opaque) => None,
            Type::Generic(_gen) => None,
            Type::Function(_fun) => None,
        }
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

    // FIXME: Slow
    pub fn get_type_for_tag(&mut self, tag_ident: IdentifierId) -> TypeId {
        for (type_id, typ) in self.iter() {
            if let Type::TagInstance(tag) = typ {
                if tag.ident == tag_ident {
                    return type_id;
                }
            }
        }
        let tag_type = Type::TagInstance(TagInstanceType { ident: tag_ident });
        let tag_type_id = self.add_type(tag_type);
        tag_type_id
    }
}
