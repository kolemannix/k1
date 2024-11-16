use std::collections::HashMap;
use std::fmt::{Display, Formatter};

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
        f.write_str(&self.0.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct StructTypeField {
    pub name: Identifier,
    pub type_id: TypeId,
    pub index: u32,
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

pub const ARRAY_TYPE_ID: TypeId = TypeId(17);
pub const STRING_TYPE_ID: TypeId = TypeId(18);
pub const OPTIONAL_TYPE_ID: TypeId = TypeId(23);

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub name: Identifier,
    pub scope_id: ScopeId,
    pub ability_impls: Vec<AbilityId>,
    pub span: SpanId,
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
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub variants: Vec<TypedEnumVariant>,
    /// Populated for non-anonymous (named) enums
    pub type_defn_info: Option<TypeDefnInfo>,
    /// Populated for specialized copies of generic enums, contains provenance info
    pub generic_instance_info: Option<GenericInstanceInfo>,
    pub ast_node: ParsedId,
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
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub params: Vec<GenericTypeParam>,
    pub inner: TypeId,
    pub ast_id: ParsedTypeDefnId,
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
pub struct FnArgType {
    pub name: Identifier,
    pub type_id: TypeId,
    pub is_context: bool,
    pub is_closure_env: bool,
    pub span: SpanId,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<FnArgType>,
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
pub struct ClosureType {
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

#[derive(Debug, Clone)]
pub struct ClosureObjectType {
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
    #[allow(clippy::enum_variant_names)]
    TypeVariable(TypeVariable),
    Enum(TypedEnum),
    /// Enum variants are proper types of their own, for lots
    /// of reasons that make programming nice. Unlike in Rust :()
    EnumVariant(TypedEnumVariant),
    Never(TypeDefnInfo),
    OpaqueAlias(OpaqueTypeAlias),
    Generic(GenericType),
    Function(FunctionType),
    Closure(ClosureType),
    ClosureObject(ClosureObjectType),
    RecursiveReference(RecursiveReference),
}

impl Type {
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
            Type::OpaqueAlias(opaque) => Some(opaque.ast_id.into()),
            Type::Generic(gen) => Some(gen.ast_id.into()),
            Type::Function(_fun) => None,
            Type::Closure(clos) => Some(clos.parsed_id),
            Type::ClosureObject(clos_obj) => Some(clos_obj.parsed_id),
            Type::RecursiveReference(r) => Some(ParsedId::TypeDefn(r.parsed_id)),
        }
    }

    pub fn as_array_instance(&self) -> Option<ArrayType> {
        if let Type::Struct(s) = self {
            s.generic_instance_info.as_ref().and_then(|spec_info| {
                if spec_info.generic_parent == ARRAY_TYPE_ID {
                    Some(ArrayType { element_type: spec_info.param_values[0] })
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
            _ => panic!("expected enum"),
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

    pub fn as_closure(&self) -> Option<&ClosureType> {
        match self {
            Type::Closure(c) => Some(c),
            _ => None,
        }
    }
    pub fn as_closure_object(&self) -> Option<&ClosureObjectType> {
        match self {
            Type::ClosureObject(co) => Some(co),
            _ => None,
        }
    }
}

#[derive(Default, Debug)]
pub struct Types {
    pub types: Vec<Type>,
    pub type_defn_mapping: HashMap<ParsedTypeDefnId, TypeId>,
    pub placeholder_mapping: HashMap<ParsedTypeDefnId, TypeId>,
}

impl Types {
    pub fn add_type_ext(&mut self, typ: Type, dedupe: bool) -> TypeId {
        if dedupe {
            for (existing_type_id, existing_type) in self.iter() {
                if self.type_eq(existing_type, &typ) {
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

    pub fn get_as_enum(&self, type_id: TypeId) -> Option<(&TypedEnum, Option<&TypedEnumVariant>)> {
        match self.get(type_id) {
            Type::Enum(e) => Some((e, None)),
            Type::EnumVariant(ev) => Some((self.get(ev.enum_type_id).expect_enum(), Some(ev))),
            _ => None,
        }
    }

    pub fn get_generic_instance_info(&self, type_id: TypeId) -> Option<&GenericInstanceInfo> {
        match self.get(type_id) {
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
            Type::OpaqueAlias(_opaque) => None,
            Type::Generic(_gen) => None,
            Type::Function(_) => None,
            Type::Closure(_) => None,
            Type::ClosureObject(_) => None,
            Type::RecursiveReference(_) => None,
        }
    }

    pub fn get_defn_info(&self, type_id: TypeId) -> Option<&TypeDefnInfo> {
        match self.get(type_id) {
            Type::Enum(e) => e.type_defn_info.as_ref(),
            Type::EnumVariant(ev) => self.get_defn_info(ev.enum_type_id),
            Type::Struct(s) => s.type_defn_info.as_ref(),
            Type::Unit(info) => Some(info),
            Type::Char(info) => Some(info),
            Type::Integer(_) => None,
            Type::Float(f) => Some(&f.defn_info),
            Type::Bool(info) => Some(info),
            Type::Pointer(info) => Some(info),
            Type::Reference(_) => None,
            Type::TypeVariable(_) => None,
            Type::Never(info) => Some(info),
            Type::OpaqueAlias(opaque) => Some(&opaque.type_defn_info),
            Type::Generic(gen) => Some(&gen.type_defn_info),
            Type::Function(f) => f.defn_info.as_ref(),
            Type::Closure(_c) => None,
            Type::ClosureObject(_) => None,
            Type::RecursiveReference(_) => None,
        }
    }

    pub fn add_closure(
        &mut self,
        identifiers: &Identifiers,
        function_type_id: TypeId,
        environment_struct: TypedExpr,
        body_function_id: FunctionId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let env_type = environment_struct.get_type();
        let closure_type_id = self.add_type(Type::Closure(ClosureType {
            function_type: function_type_id,
            env_type: environment_struct.get_type(),
            parsed_id,
            body_function_id,
            environment_struct,
            closure_object_type: TypeId::PENDING,
        }));
        let closure_object_type =
            self.add_closure_object(identifiers, function_type_id, env_type, parsed_id);
        if let Type::Closure(c) = self.get_mut(closure_type_id) {
            c.closure_object_type = closure_object_type;
        }
        closure_type_id
    }

    pub fn add_closure_object(
        &mut self,
        identifiers: &Identifiers,
        function_type_id: TypeId,
        env_type: TypeId,
        parsed_id: ParsedId,
    ) -> TypeId {
        let fn_ptr_type = self.add_reference_type(function_type_id);
        let env_ptr_type = self.add_reference_type(env_type);
        let fields = vec![
            StructTypeField {
                name: identifiers.get("fn_ptr").unwrap(),
                type_id: fn_ptr_type,
                index: 0,
            },
            StructTypeField {
                name: identifiers.get("env_ptr").unwrap(),
                type_id: env_ptr_type,
                index: 1,
            },
        ];
        let struct_representation = self.add_type(Type::Struct(StructType {
            fields,
            type_defn_info: None,
            generic_instance_info: None,
            ast_node: parsed_id,
        }));
        self.add_type(Type::ClosureObject(ClosureObjectType {
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

    /// Recursively checks if given type contains any type variables
    /// Note: We could cache whether or not a type is generic on insertion into the type pool
    ///       But types are not immutable so this could be a dangerous idea!
    pub fn does_type_reference_type_variables(&self, type_id: TypeId) -> bool {
        if let Some(spec_info) = self.get_generic_instance_info(type_id) {
            return spec_info
                .param_values
                .iter()
                .any(|t| self.does_type_reference_type_variables(*t));
        }
        match self.get_no_follow(type_id) {
            Type::TypeVariable(_) => true,
            Type::Unit(_) => false,
            Type::Char(_) => false,
            Type::Integer(_) => false,
            Type::Float(_) => false,
            Type::Bool(_) => false,
            Type::Pointer(_) => false,
            Type::Struct(struc) => {
                for field in struc.fields.iter() {
                    if self.does_type_reference_type_variables(field.type_id) {
                        return true;
                    }
                }
                return false;
            }
            Type::Reference(refer) => self.does_type_reference_type_variables(refer.inner_type),
            Type::Enum(e) => {
                for v in e.variants.iter() {
                    if let Some(payload) = v.payload {
                        if self.does_type_reference_type_variables(payload) {
                            return true;
                        }
                    }
                }
                return false;
            }
            Type::EnumVariant(ev) => {
                if let Some(payload) = ev.payload {
                    if self.does_type_reference_type_variables(payload) {
                        return true;
                    }
                }
                return false;
            }
            Type::Never(_) => false,
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
            Type::Closure(closure) => {
                self.does_type_reference_type_variables(closure.function_type)
                    || self.does_type_reference_type_variables(closure.env_type)
            }
            // But a closure object is generic if its function is generic
            Type::ClosureObject(co) => self.does_type_reference_type_variables(co.function_type),
            Type::RecursiveReference(_rr) => false,
        }
    }

    pub fn item_type_of_iterable(
        &self,
        identifiers: &Identifiers,
        scopes: &Scopes,
        type_id: TypeId,
    ) -> Option<TypeId> {
        match self.get(type_id) {
            Type::Unit(_) => None,
            Type::Char(_) => None,
            Type::Integer(_) => None,
            Type::Float(_) => None,
            Type::Bool(_) => None,
            Type::Pointer(_) => None,
            // Check for Array and string since they are currently structs
            t @ Type::Struct(struc) => {
                if let Some(array_type) = t.as_array_instance() {
                    Some(array_type.element_type)
                } else if let Some(defn_info) = struc.type_defn_info.as_ref() {
                    if defn_info.name == identifiers.get("string").unwrap()
                        && defn_info.scope == scopes.get_root_scope_id()
                    {
                        Some(CHAR_TYPE_ID)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Type::Reference(_refer) => None,
            Type::TypeVariable(_) => None,
            Type::Enum(_) => None,
            Type::EnumVariant(_) => None,
            Type::Never(_) => None,
            Type::OpaqueAlias(_opaque) => None,
            Type::Generic(_gen) => None,
            Type::Function(_fun) => None,
            Type::Closure(_closure) => None,
            Type::ClosureObject(_co) => None,
            Type::RecursiveReference(_) => None,
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

    // Ended up storing redirects as RecursiveReference, but keeping this around
    // Ah I also got this confused with substitute_in_type which is actually used for generic instantiation
    // This one looks like it mutates the type tree instead of making a new type
    pub fn replace_type(
        &mut self,
        placeholder_id: TypeId,
        replace_in: TypeId,
        replace_with: TypeId,
    ) {
        match self.get_mut(replace_in) {
            Type::Unit(_) => (),
            Type::Char(_) => (),
            Type::Integer(_) => (),
            Type::Float(_) => (),
            Type::Bool(_) => (),
            Type::Pointer(_) => (),
            Type::Struct(s) => {
                for f in s.fields.clone().iter_mut() {
                    if f.type_id == placeholder_id {
                        f.type_id = replace_with;
                    } else {
                        self.replace_type(placeholder_id, f.type_id, replace_with);
                    }
                }
            }
            Type::Reference(refer) => {
                let inner_type = refer.inner_type;
                self.replace_type(placeholder_id, inner_type, replace_with)
            }
            Type::TypeVariable(_) => (),
            Type::Enum(e) => {
                for v in e.variants.clone().iter_mut() {
                    if v.my_type_id == placeholder_id {
                        v.my_type_id = replace_with;
                    } else {
                        self.replace_type(placeholder_id, v.my_type_id, replace_with)
                    }
                }
            }
            Type::EnumVariant(ev) => {
                if let Some(payload) = ev.payload {
                    if payload == placeholder_id {
                        ev.payload = Some(replace_with);
                    } else {
                        self.replace_type(placeholder_id, payload, replace_with)
                    }
                }
            }
            Type::Never(_) => (),
            Type::OpaqueAlias(_) => todo!(),
            Type::Generic(_) => unreachable!(),
            Type::Function(_f) => todo!(),
            Type::Closure(_c) => todo!(),
            Type::ClosureObject(_co) => todo!(),
            Type::RecursiveReference(_rr) => (),
        }
    }

    fn type_id_eq(&self, type1: TypeId, type2: TypeId) -> bool {
        self.type_eq(&self.get(type1), &self.get(type2))
    }

    fn type_eq(&self, type1: &Type, type2: &Type) -> bool {
        match (type1, type2) {
            (Type::Unit(_), Type::Unit(_)) => true,
            (Type::Char(_), Type::Char(_)) => true,
            (Type::Integer(int1), Type::Integer(int2)) => int1 == int2,
            (Type::Bool(_), Type::Bool(_)) => true,
            (Type::Struct(r1), Type::Struct(r2)) => {
                if r1.is_named() || r2.is_named() {
                    return false;
                }
                if r1.fields.len() != r2.fields.len() {
                    return false;
                }
                for (index, f1) in r1.fields.iter().enumerate() {
                    let f2 = &r2.fields[index];
                    let mismatch = f1.name != f2.name || f1.type_id != f2.type_id;
                    if mismatch {
                        return false;
                    }
                }
                return true;
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
            // We never really want to de-dupe this type as its inherently unique
            (Type::OpaqueAlias(_opa1), Type::OpaqueAlias(_opa2)) => false,
            // We never really want to de-dupe this type as its inherently unique
            (Type::Generic(_g1), Type::Generic(_g2)) => false,
            (Type::Function(f1), Type::Function(f2)) => {
                if self.type_id_eq(f1.return_type, f2.return_type)
                    && f1.params.len() == f2.params.len()
                {
                    return f1
                        .params
                        .iter()
                        .zip(f2.params.iter())
                        .all(|(p1, p2)| self.type_id_eq(p1.type_id, p2.type_id));
                };
                return false;
            }
            (Type::Closure(_c1), Type::Closure(_c2)) => false,
            _ => false,
        }
    }
}
