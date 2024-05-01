#![allow(clippy::match_like_matches_macro)]
pub mod dump;
pub mod scopes;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Write};
use std::ops::Deref;
use std::rc::Rc;

use anyhow::bail;
use colored::Colorize;
use either::Either;
use log::{debug, trace, Level};

use scopes::*;

use crate::lex::{Span, TokenKind};
use crate::parse::{
    self, ExpressionId, FnCallArg, ForExpr, ForExprType, IfExpr, IndexOperation, ParsedAbilityId,
    ParsedAbilityImplId, ParsedConstantId, ParsedDefinitionId, ParsedFunctionId, ParsedNamespaceId,
    ParsedTypeDefnId, ParsedTypeExpression,
};
use crate::parse::{
    AstDefinitionId, Block, BlockStmt, FnCall, IdentifierId, Literal, ParsedExpression,
    ParsedFunction, ParsedModule,
};

pub type FunctionId = u32;
pub type VariableId = u32;
pub type TypeId = u32;
pub type NamespaceId = u32;
pub type AbilityId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Standard,
    External,
    Intrinsic,
}

#[derive(Debug, Clone)]
pub struct RecordTypeField {
    pub name: IdentifierId,
    pub type_id: TypeId,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct RecordType {
    pub fields: Vec<RecordTypeField>,
    pub name_if_named: Option<IdentifierId>,
    pub span: Span,
}

impl RecordType {
    pub fn find_field(&self, field_name: IdentifierId) -> Option<(usize, &RecordTypeField)> {
        self.fields.iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

pub const UNIT_TYPE_ID: TypeId = 0;
pub const CHAR_TYPE_ID: TypeId = 1;
pub const INT_TYPE_ID: TypeId = 2;
pub const BOOL_TYPE_ID: TypeId = 3;
pub const STRING_TYPE_ID: TypeId = 4;

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeId,
    // todo: Lose the span!
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    identifier_id: IdentifierId,
    /// This is where trait bounds would go
    _constraints: Option<Vec<()>>,
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
pub struct TagInstance {
    pub ident: IdentifierId,
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariant {
    pub tag_name: IdentifierId,
    pub index: u32,
    pub payload: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub variants: Vec<TypedEnumVariant>,
    pub name_if_named: Option<IdentifierId>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Char,
    Int,
    Bool,
    String,
    Record(RecordType),
    Array(ArrayType),
    Optional(OptionalType),
    Reference(ReferenceType),
    #[allow(clippy::enum_variant_names)]
    TypeVariable(TypeVariable),
    TagInstance(TagInstance),
    Enum(TypedEnum),
}

impl Type {
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

    pub fn expect_enum(&self) -> &TypedEnum {
        match self {
            Type::Enum(e) => e,
            _ => panic!("expected enum type"),
        }
    }

    pub fn as_enum(&self) -> Option<&TypedEnum> {
        match self {
            Type::Enum(e) => Some(e),
            _ => None,
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

    pub fn as_record(&self) -> Option<&RecordType> {
        match self {
            Type::Record(record) => Some(record),
            _ => None,
        }
    }

    pub fn expect_record(&self) -> &RecordType {
        match self {
            Type::Record(record) => record,
            _ => panic!("expect_record called on: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedAbilityFunctionRef {
    pub function_name: IdentifierId,
    pub function_id: FunctionId,
}

#[derive(Debug, Clone)]
pub struct TypedAbility {
    pub name: IdentifierId,
    pub functions: Vec<TypedAbilityFunctionRef>,
    pub scope_id: ScopeId,
    pub ast_id: ParsedAbilityId,
}

impl TypedAbility {
    pub(crate) fn find_function_by_name(
        &self,
        name: IdentifierId,
    ) -> Option<(usize, &TypedAbilityFunctionRef)> {
        self.functions.iter().enumerate().find(|(_, f)| f.function_name == name)
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    // If this block is just an expression, the type of the expression
    pub expr_type: TypeId,
    pub scope_id: ScopeId,
    pub statements: Vec<TypedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnArgDefn {
    pub name: IdentifierId,
    pub variable_id: VariableId,
    pub position: u32,
    pub type_id: TypeId,
    pub span: Span,
}

pub struct SpecializationParams {
    pub fn_scope_id: ScopeId,
    pub new_name: IdentifierId,
    pub known_intrinsic: Option<IntrinsicFunctionType>,
    pub generic_parent_function: FunctionId,
    pub is_ability_impl: bool,
}

#[derive(Debug, Clone)]
pub struct SpecializationRecord {
    pub specialized_type_params: Vec<TypeId>,
    pub specialized_function_id: FunctionId,
    pub specialized_params: Vec<FnArgDefn>,
}

#[derive(Debug, Clone, Copy)]
pub enum TypedFunctionMetadata {
    Standard(ParsedFunctionId),
    Specialization { generic_parsed_function_id: ParsedFunctionId, generic_defn: FunctionId },
    AbilityDefn(ParsedFunctionId),
    AbilityImpl { generic_defn: FunctionId, impl_parsed_function_id: ParsedFunctionId },
}
impl TypedFunctionMetadata {
    /// If this function has a corresponding parsed_function_id, return it.
    /// In the case of a specialized generic function, it will not.
    fn parsed_function_id(&self) -> ParsedFunctionId {
        match self {
            TypedFunctionMetadata::Standard(pfi) => *pfi,
            TypedFunctionMetadata::Specialization { generic_parsed_function_id, .. } => {
                *generic_parsed_function_id
            }
            TypedFunctionMetadata::AbilityDefn(pfi) => *pfi,
            TypedFunctionMetadata::AbilityImpl {
                impl_parsed_function_id: parsed_function_id,
                ..
            } => *parsed_function_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: IdentifierId,
    pub scope: ScopeId,
    pub ret_type: TypeId,
    pub params: Vec<FnArgDefn>,
    pub type_params: Option<Vec<TypeParam>>,
    pub block: Option<TypedBlock>,
    pub intrinsic_type: Option<IntrinsicFunctionType>,
    pub linkage: Linkage,
    pub specializations: Vec<SpecializationRecord>,
    pub metadata: TypedFunctionMetadata,
    pub span: Span,
}

impl TypedFunction {
    pub fn should_codegen(&self) -> bool {
        match self.metadata {
            TypedFunctionMetadata::Standard(_) => !self.is_generic(),
            TypedFunctionMetadata::Specialization { .. } => true,
            TypedFunctionMetadata::AbilityDefn(_) => false,
            TypedFunctionMetadata::AbilityImpl { .. } => true,
        }
    }
    pub fn is_generic(&self) -> bool {
        match &self.type_params {
            None => false,
            Some(vec) => !vec.is_empty(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub ident: IdentifierId,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub variable_id: VariableId,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Equals,
    NotEquals,
}

impl Display for BinaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOpKind::Add => f.write_char('+'),
            BinaryOpKind::Subtract => f.write_char('-'),
            BinaryOpKind::Multiply => f.write_char('*'),
            BinaryOpKind::Divide => f.write_char('/'),
            BinaryOpKind::Less => f.write_char('<'),
            BinaryOpKind::Greater => f.write_char('>'),
            BinaryOpKind::LessEqual => f.write_str("<="),
            BinaryOpKind::GreaterEqual => f.write_str(">="),
            BinaryOpKind::And => f.write_str("and"),
            BinaryOpKind::Or => f.write_str("or"),
            BinaryOpKind::Equals => f.write_str("=="),
            BinaryOpKind::NotEquals => f.write_str("!="),
        }
    }
}

impl BinaryOpKind {
    pub fn precedence(&self) -> usize {
        use BinaryOpKind as B;
        match self {
            B::Multiply | B::Divide => 100,
            B::Add | B::Subtract => 90,
            B::Less | B::LessEqual | B::Greater | B::GreaterEqual | B::Equals | B::NotEquals => 80,
            B::And => 70,
            B::Or => 66,
        }
    }

    pub fn from_tokenkind(kind: TokenKind) -> Option<BinaryOpKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOpKind::Add),
            TokenKind::Minus => Some(BinaryOpKind::Subtract),
            TokenKind::Asterisk => Some(BinaryOpKind::Multiply),
            TokenKind::Slash => Some(BinaryOpKind::Divide),
            TokenKind::OpenAngle => Some(BinaryOpKind::Less),
            TokenKind::CloseAngle => Some(BinaryOpKind::Greater),
            TokenKind::KeywordAnd => Some(BinaryOpKind::And),
            TokenKind::KeywordOr => Some(BinaryOpKind::Or),
            TokenKind::EqualsEquals => Some(BinaryOpKind::Equals),
            TokenKind::BangEquals => Some(BinaryOpKind::NotEquals),
            _ => None,
        }
    }

    pub fn is_integer_op(&self) -> bool {
        use BinaryOpKind as B;
        match self {
            B::Add | B::Subtract => true,
            B::Multiply | B::Divide => true,
            B::Less | B::Greater | B::LessEqual | B::GreaterEqual => true,
            B::Or | B::And => true,
            B::Equals | B::NotEquals => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub ty: TypeId,
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnaryOpKind {
    BooleanNegation,
    Reference,
    Dereference,
}

impl Display for UnaryOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOpKind::BooleanNegation => f.write_char('!'),
            UnaryOpKind::Reference => f.write_char('&'),
            UnaryOpKind::Dereference => f.write_char('*'),
        }
    }
}

impl UnaryOpKind {
    pub fn from_tokenkind(kind: TokenKind) -> Option<UnaryOpKind> {
        match kind {
            TokenKind::Bang => Some(UnaryOpKind::BooleanNegation),
            TokenKind::Ampersand => Some(UnaryOpKind::Reference),
            TokenKind::Asterisk => Some(UnaryOpKind::Dereference),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub type_id: TypeId,
    pub expr: Box<TypedExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee_function_id: FunctionId,
    pub args: Vec<TypedExpr>,
    pub ret_type: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: IdentifierId,
    pub expr: TypedExpr,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub fields: Vec<RecordField>,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<TypedExpr>,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: TypedExpr,
    pub consequent: TypedBlock,
    pub alternate: TypedBlock,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: Box<TypedExpr>,
    pub target_field: IdentifierId,
    pub target_field_index: u32,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IndexOp {
    pub base_expr: Box<TypedExpr>,
    pub index_expr: Box<TypedExpr>,
    pub result_type: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct OptionalSome {
    pub inner_expr: Box<TypedExpr>,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct OptionalGet {
    pub inner_expr: Box<TypedExpr>,
    pub result_type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedForExpr {
    pub iterable_expr: Box<TypedExpr>,
    pub binding: IdentifierId,
    pub body_block: Box<TypedBlock>,
    pub result_type_id: TypeId,
    pub for_expr_type: ForExprType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedTagExpr {
    pub name: IdentifierId,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedEnumConstructor {
    pub type_id: TypeId,
    pub variant_name: IdentifierId,
    pub variant_index: u32,
    pub payload: Option<Box<TypedExpr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Unit(Span),
    Char(u8, Span),
    Bool(bool, Span),
    Int(i64, Span),
    Str(String, Span),
    None(TypeId, Span),
    Record(Record),
    Array(ArrayLiteral),
    Variable(VariableExpr),
    RecordFieldAccess(FieldAccess),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Block(TypedBlock),
    FunctionCall(Call),
    If(Box<TypedIf>),
    ArrayIndex(IndexOp),
    StringIndex(IndexOp),
    // TODO: Can some of these just be unary ops
    OptionalSome(OptionalSome),
    OptionalHasValue(Box<TypedExpr>),
    OptionalGet(OptionalGet),
    Tag(TypedTagExpr),
    EnumConstructor(TypedEnumConstructor),
}

impl TypedExpr {
    pub fn unit_literal(span: Span) -> TypedExpr {
        TypedExpr::Unit(span)
    }

    #[inline]
    pub fn get_type(&self) -> TypeId {
        match self {
            TypedExpr::None(type_id, _) => *type_id,
            TypedExpr::Unit(_) => UNIT_TYPE_ID,
            TypedExpr::Char(_, _) => CHAR_TYPE_ID,
            TypedExpr::Str(_, _) => STRING_TYPE_ID,
            TypedExpr::Int(_, _) => INT_TYPE_ID,
            TypedExpr::Bool(_, _) => BOOL_TYPE_ID,
            TypedExpr::Record(record) => record.type_id,
            TypedExpr::Array(arr) => arr.type_id,
            TypedExpr::Variable(var) => var.type_id,
            TypedExpr::RecordFieldAccess(field_access) => field_access.ty,
            TypedExpr::BinaryOp(binary_op) => binary_op.ty,
            TypedExpr::UnaryOp(unary_op) => unary_op.type_id,
            TypedExpr::Block(b) => b.expr_type,
            TypedExpr::FunctionCall(call) => call.ret_type,
            TypedExpr::If(ir_if) => ir_if.ty,
            TypedExpr::ArrayIndex(op) => op.result_type,
            TypedExpr::StringIndex(op) => op.result_type,
            TypedExpr::OptionalSome(opt) => opt.type_id,
            TypedExpr::OptionalHasValue(_opt) => BOOL_TYPE_ID,
            TypedExpr::OptionalGet(opt_get) => opt_get.result_type_id,
            TypedExpr::Tag(tag_expr) => tag_expr.type_id,
            TypedExpr::EnumConstructor(enum_cons) => enum_cons.type_id,
        }
    }
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypedExpr::Unit(span) => *span,
            TypedExpr::Char(_, span) => *span,
            TypedExpr::Bool(_, span) => *span,
            TypedExpr::Int(_, span) => *span,
            TypedExpr::Str(_, span) => *span,
            TypedExpr::None(_, span) => *span,
            TypedExpr::Record(record) => record.span,
            TypedExpr::Array(array) => array.span,
            TypedExpr::Variable(var) => var.span,
            TypedExpr::RecordFieldAccess(field_access) => field_access.span,
            TypedExpr::BinaryOp(binary_op) => binary_op.span,
            TypedExpr::UnaryOp(unary_op) => unary_op.span,
            TypedExpr::Block(b) => b.span,
            TypedExpr::FunctionCall(call) => call.span,
            TypedExpr::If(ir_if) => ir_if.span,
            TypedExpr::ArrayIndex(op) => op.span,
            TypedExpr::StringIndex(op) => op.span,
            TypedExpr::OptionalSome(opt) => opt.inner_expr.get_span(),
            TypedExpr::OptionalHasValue(opt) => opt.get_span(),
            TypedExpr::OptionalGet(get) => get.span,
            TypedExpr::Tag(tag_expr) => tag_expr.span,
            TypedExpr::EnumConstructor(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValDef {
    pub variable_id: VariableId,
    pub ty: TypeId,
    pub initializer: TypedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub expr: TypedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub destination: Box<TypedExpr>,
    pub value: Box<TypedExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedWhileLoop {
    pub cond: TypedExpr,
    pub block: TypedBlock,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Expr(Box<TypedExpr>),
    ValDef(Box<ValDef>),
    Assignment(Box<Assignment>),
    WhileLoop(Box<TypedWhileLoop>),
}

impl TypedStmt {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypedStmt::Expr(e) => e.get_span(),
            TypedStmt::ValDef(v) => v.span,
            TypedStmt::Assignment(ass) => ass.span,
            TypedStmt::WhileLoop(w) => w.span,
        }
    }
}

#[derive(Debug)]
pub struct TyperError {
    message: String,
    span: Span,
}

impl TyperError {
    fn make(message: impl AsRef<str>, span: Span) -> TyperError {
        TyperError { message: message.as_ref().to_owned(), span }
    }
}

impl Display for TyperError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("error on line {}: {}", self.span.line, self.message))
    }
}

impl Error for TyperError {}

pub type TyperResult<A> = Result<A, TyperError>;

#[derive(Debug)]
pub struct Variable {
    pub name: IdentifierId,
    pub type_id: TypeId,
    pub is_mutable: bool,
    pub owner_scope: Option<ScopeId>,
}

#[derive(Debug)]
pub struct Constant {
    pub variable_id: VariableId,
    pub expr: TypedExpr,
    pub ty: TypeId,
    pub span: Span,
}

pub struct Namespace {
    pub name: IdentifierId,
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicFunctionType {
    Exit,
    PrintInt,
    PrintString,
    StringLength,
    ArrayLength,
    ArrayNew,
    ArrayGrow,
    ArraySetLength,
    ArrayCapacity,
    StringFromCharArray,
    StringEquals,
}

impl IntrinsicFunctionType {
    pub fn from_function_name(value: &str) -> Option<Self> {
        match value {
            "printInt" => Some(IntrinsicFunctionType::PrintInt),
            "print" => Some(IntrinsicFunctionType::PrintString),
            "exit" => Some(IntrinsicFunctionType::Exit),
            _ => None,
        }
    }
}

fn make_error<T: AsRef<str>>(message: T, span: Span) -> TyperError {
    TyperError::make(message.as_ref(), span)
}

fn make_fail<A, T: AsRef<str>>(message: T, span: Span) -> TyperResult<A> {
    Err(make_error(message, span))
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TypedDefinitionId {
    Function(FunctionId),
    Type(TypeId),
    Namespace(NamespaceId),
    Ability(AbilityId),
}

impl TypedDefinitionId {
    fn as_function_id(&self) -> FunctionId {
        match self {
            TypedDefinitionId::Function(f) => *f,
            _ => panic!("Expected function id"),
        }
    }

    fn as_namespace_id(&self) -> NamespaceId {
        match self {
            TypedDefinitionId::Namespace(n) => *n,
            _ => panic!("Expected namespace id"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AbilityImplementation {
    pub type_id: TypeId,
    pub ability_id: AbilityId,
    /// In order they are defined in the ability; currently we only ever have one
    pub functions: Vec<FunctionId>,
}

pub struct TypedModule {
    pub ast: Rc<ParsedModule>,
    functions: Vec<TypedFunction>,
    pub variables: Vec<Variable>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub scopes: Scopes,
    pub errors: Vec<TyperError>,
    pub namespaces: Vec<Namespace>,
    pub abilities: Vec<TypedAbility>,
    pub implementations: Vec<AbilityImplementation>,
    pub function_ast_mappings: HashMap<ParsedFunctionId, FunctionId>,
    pub namespace_ast_mappings: HashMap<ParsedNamespaceId, NamespaceId>,
}

impl TypedModule {
    pub fn new(parsed_module: Rc<ParsedModule>) -> TypedModule {
        let root_ident = parsed_module.ident_id("_root");
        let types = vec![Type::Unit, Type::Char, Type::Int, Type::Bool, Type::String];
        let scopes = Scopes::make(root_ident);
        let namespaces = vec![Namespace { name: root_ident, scope_id: scopes.get_root_scope_id() }];
        TypedModule {
            ast: parsed_module,
            functions: Vec::new(),
            variables: Vec::new(),
            types,
            constants: Vec::new(),
            scopes,
            errors: Vec::new(),
            namespaces,
            abilities: Vec::new(),
            implementations: Vec::new(),
            function_ast_mappings: HashMap::new(),
            namespace_ast_mappings: HashMap::new(),
        }
    }

    pub fn function_iter(&self) -> impl Iterator<Item = (FunctionId, &TypedFunction)> {
        self.functions.iter().enumerate().map(|(idx, f)| (idx as FunctionId, f))
    }

    fn internal_compiler_error(&self, message: impl AsRef<str>, span: Span) -> ! {
        self.print_error(message, span);
        panic!()
    }

    fn print_error(&self, message: impl AsRef<str>, span: Span) {
        let line_no = span.line + 1;
        let source = self.ast.sources.source_by_span(span);
        eprintln!(
            "{} at {}:{}\n  -> {}",
            "error".red(),
            &source.filename,
            line_no,
            message.as_ref()
        );
        let (_line_start, line_text) = self.ast.sources.get_line_for_span(span);
        eprintln!("{}", line_text.red());
        eprintln!(" -> {}", self.ast.sources.get_span_content(span).red());
    }

    pub fn name(&self) -> &str {
        &self.ast.name
    }

    fn get_ident_str(&self, id: IdentifierId) -> impl Deref<Target = str> + '_ {
        self.ast.get_ident_str(id)
    }

    fn get_namespace(&self, namespace_id: NamespaceId) -> &Namespace {
        &self.namespaces[namespace_id as usize]
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(typ);
        id as u32
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id as usize]
    }

    pub fn get_type_dereferenced(&self, type_id: TypeId) -> &Type {
        match self.get_type(type_id) {
            Type::Reference(r) => self.get_type(r.inner_type),
            _ => self.get_type(type_id),
        }
    }

    pub fn get_type_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.types[type_id as usize]
    }

    // pub fn is_reference_type(&self, ty: TypeId) -> bool {
    //     match ty {
    //         TypeId::Unit => false,
    //         TypeId::Char => false,
    //         TypeId::Int => false,
    //         TypeId::Bool => false,
    //         TypeId::String => false,
    //         TypeId::TypeId(type_id) => {
    //             let ty = self.get_type(type_id);
    //             match ty {
    //                 Type::Record(_) => true,
    //                 Type::Array(_) => true,
    //                 Type::TypeVariable(_) => true,
    //                 Type::Optional(opt) => true,
    //             }
    //         }
    //     }
    // }

    /// Recursively checks if given type contains any type variables
    fn is_type_generic(&self, type_id: TypeId) -> bool {
        match self.get_type(type_id) {
            Type::Unit => false,
            Type::Char => false,
            Type::Int => false,
            Type::Bool => false,
            Type::String => false,
            Type::Array(arr) => self.is_type_generic(arr.element_type),
            // We don't _yet_ support generics in records
            Type::Record(_record) => false,
            Type::Optional(opt) => self.is_type_generic(opt.inner_type),
            Type::Reference(refer) => self.is_type_generic(refer.inner_type),
            Type::TypeVariable(_) => true,
            Type::TagInstance(_) => false,
            // We don't _yet_ support generics in records
            Type::Enum(_) => false,
        }
    }

    fn item_type_of_iterable(&self, type_id: TypeId) -> Option<TypeId> {
        match self.get_type(type_id) {
            Type::Unit => None,
            Type::Char => None,
            Type::Int => None,
            Type::Bool => None,
            Type::String => Some(CHAR_TYPE_ID),
            Type::Array(arr) => Some(arr.element_type),
            Type::Record(_record) => None,
            Type::Optional(_opt) => None,
            Type::Reference(_refer) => None,
            Type::TypeVariable(_) => None,
            Type::TagInstance(_) => None,
            Type::Enum(_) => None,
        }
    }

    fn add_ability(&mut self, ability: TypedAbility) -> AbilityId {
        let ability_id = self.abilities.len();
        self.abilities.push(ability);
        ability_id as u32
    }

    fn get_ability(&self, ability_id: AbilityId) -> &TypedAbility {
        &self.abilities[ability_id as usize]
    }

    fn eval_type_defn(
        &mut self,
        parsed_type_defn_id: ParsedTypeDefnId,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        let ast = self.ast.clone();
        let parsed_type_defn = ast.get_type_defn(parsed_type_defn_id);
        let type_id = self.eval_type_expr(&parsed_type_defn.value_expr, scope_id)?;
        match self.get_type_mut(type_id) {
            Type::Record(record_defn) => {
                // Add the name to this record defn so it can have associated
                // methods and constants
                // FIXME: This name needs to be fully qualified!
                record_defn.name_if_named = Some(parsed_type_defn.name);
                Ok(type_id)
            }
            Type::Enum(enum_defn) => {
                // Add the name to this enum defn so it can have associated
                // methods and constants
                // FIXME: This name needs to be fully qualified!
                enum_defn.name_if_named = Some(parsed_type_defn.name);
                Ok(type_id)
            }
            _ => make_fail(
                "Invalid rhs for named type definition",
                parsed_type_defn.value_expr.get_span(),
            ),
        }?;
        self.scopes.add_type(scope_id, parsed_type_defn.name, type_id);
        Ok(type_id)
    }

    fn eval_type_expr(
        &mut self,
        expr: &ParsedTypeExpression,
        scope_id: ScopeId,
    ) -> TyperResult<TypeId> {
        let base = match expr {
            ParsedTypeExpression::Unit(_) => Ok(UNIT_TYPE_ID),
            ParsedTypeExpression::Char(_) => Ok(CHAR_TYPE_ID),
            ParsedTypeExpression::Int(_) => Ok(INT_TYPE_ID),
            ParsedTypeExpression::Bool(_) => Ok(BOOL_TYPE_ID),
            ParsedTypeExpression::String(_) => Ok(STRING_TYPE_ID),
            ParsedTypeExpression::Record(record_defn) => {
                let mut fields: Vec<RecordTypeField> = Vec::new();
                for (index, ast_field) in record_defn.fields.iter().enumerate() {
                    let ty = self.eval_type_expr(&ast_field.ty, scope_id)?;
                    fields.push(RecordTypeField { name: ast_field.name, type_id: ty, index })
                }
                let record_defn =
                    RecordType { fields, name_if_named: None, span: record_defn.span };
                let type_id = self.add_type(Type::Record(record_defn));
                Ok(type_id)
            }
            ParsedTypeExpression::Name(ident, span) => {
                let ty_ref = self.scopes.find_type(scope_id, *ident);

                ty_ref.ok_or_else(|| {
                    make_error(
                        format!(
                            "could not find type for identifier {}",
                            &*self.ast.get_ident_str(*ident)
                        ),
                        *span,
                    )
                })
            }
            ParsedTypeExpression::TagName(ident, _span) => {
                // Make a type for this tag, if there isn't one
                let tag_type_id = self.get_type_for_tag(*ident);
                Ok(tag_type_id)
            }
            ParsedTypeExpression::TypeApplication(ty_app) => {
                if self.ast.ident_id("Array") == ty_app.base {
                    if ty_app.params.len() == 1 {
                        let element_ty = self.eval_type_expr(&ty_app.params[0], scope_id)?;
                        let array_ty = ArrayType { span: ty_app.span, element_type: element_ty };
                        let type_id = self.add_type(Type::Array(array_ty));
                        Ok(type_id)
                    } else {
                        self.internal_compiler_error(
                            "Expected 1 type parameter for Array",
                            ty_app.span,
                        )
                    }
                } else {
                    todo!("not supported: generic non builtin types")
                }
            }
            ParsedTypeExpression::Optional(opt) => {
                let inner_ty = self.eval_type_expr(&opt.base, scope_id)?;
                let optional_type = Type::Optional(OptionalType { inner_type: inner_ty });
                let type_id = self.add_type(optional_type);
                Ok(type_id)
            }
            ParsedTypeExpression::Reference(r) => {
                let inner_ty = self.eval_type_expr(&r.base, scope_id)?;
                let reference_type = Type::Reference(ReferenceType { inner_type: inner_ty });
                let type_id = self.add_type(reference_type);
                Ok(type_id)
            }
            ParsedTypeExpression::Enum(e) => {
                let mut variants = Vec::with_capacity(e.variants.len());
                for (index, v) in e.variants.iter().enumerate() {
                    // Ensure there's a type for this tag as a workaround for codegen
                    // since codegen looks at `types` to enumerate all the tags in the program
                    self.get_type_for_tag(v.tag_name);
                    let payload_type_id = match &v.payload_expression {
                        None => None,
                        Some(payload_type_expr) => {
                            let type_id = self.eval_type_expr(payload_type_expr, scope_id)?;
                            Some(type_id)
                        }
                    };
                    let variant = TypedEnumVariant {
                        tag_name: v.tag_name,
                        index: index as u32,
                        payload: payload_type_id,
                    };
                    variants.push(variant);
                }
                let enum_type = Type::Enum(TypedEnum { variants, name_if_named: None });
                let type_id = self.add_type(enum_type);
                Ok(type_id)
            }
        }?;
        Ok(base)
    }

    // FIXME: Slow
    fn get_type_for_tag(&mut self, tag_ident: IdentifierId) -> TypeId {
        for (idx, typ) in self.types.iter().enumerate() {
            if let Type::TagInstance(tag) = typ {
                if tag.ident == tag_ident {
                    return idx as TypeId;
                }
            }
        }
        let tag_type = Type::TagInstance(TagInstance { ident: tag_ident });
        let tag_type_id = self.add_type(tag_type);
        tag_type_id
    }

    fn eval_const_type_expr(&mut self, expr: &ParsedTypeExpression) -> TyperResult<TypeId> {
        let ty = self.eval_type_expr(expr, self.scopes.get_root_scope_id())?;
        match ty {
            UNIT_TYPE_ID => Ok(ty),
            CHAR_TYPE_ID => Ok(ty),
            INT_TYPE_ID => Ok(ty),
            BOOL_TYPE_ID => Ok(ty),
            STRING_TYPE_ID => Ok(ty),
            _ => make_fail("Only scalar types allowed in constants", expr.get_span()),
        }
    }

    fn typecheck_record(
        &self,
        expected: &RecordType,
        actual: &RecordType,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        if expected.fields.len() != actual.fields.len() {
            return Err(format!(
                "expected record with {} fields, got {}",
                expected.fields.len(),
                actual.fields.len()
            ));
        }
        for expected_field in &expected.fields {
            trace!("typechecking record field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name)
            else {
                return Err(format!("expected record to have field {}", expected_field.name));
            };
            self.typecheck_types(expected_field.type_id, matching_field.type_id, scope_id)?;
        }
        Ok(())
    }

    /// This implements 'duck-typing' for records, which is really cool
    /// but I do not want to do this by default since the codegen involves
    /// either v-tables or monomorphization of functions that accept records
    /// Maybe a <: syntax to opt-in to dynamic stuff like this, read as "conforms to"
    /// input <: {quack: () -> ()} means that it has at least a quack function
    /// fn takes_quacker = (input <: {quack: () -> ()}) -> ()
    ///
    /// "Conforms To" would mean that it has at least the same fields as the expected type, and
    /// it has them at least as strongly. If an optional is expected, actual can optional or required
    /// If a required is expected, actual must be required, etc. Basically TypeScripts structural typing
    #[allow(unused)]
    fn typecheck_record_duck(
        &self,
        expected: &RecordType,
        actual: &RecordType,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        for expected_field in &expected.fields {
            trace!("typechecking record field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name)
            else {
                return Err(format!("expected field {}", expected_field.name));
            };
            self.typecheck_types(matching_field.type_id, expected_field.type_id, scope_id)?;
        }
        Ok(())
    }

    fn typecheck_types(
        &self,
        expected: TypeId,
        actual: TypeId,
        scope_id: ScopeId,
    ) -> Result<(), String> {
        trace!(
            "typecheck expect {} actual {}",
            self.type_id_to_string(expected),
            self.type_id_to_string(actual)
        );
        if expected == actual {
            return Ok(());
        }
        match (self.get_type(expected), self.get_type(actual)) {
            (Type::Optional(o1), Type::Optional(o2)) => {
                self.typecheck_types(o1.inner_type, o2.inner_type, scope_id)
            }
            (Type::Record(r1), Type::Record(r2)) => self.typecheck_record(r1, r2, scope_id),
            (Type::Array(a1), Type::Array(a2)) => {
                self.typecheck_types(a1.element_type, a2.element_type, scope_id)
            }
            (Type::TypeVariable(t1), Type::TypeVariable(t2)) => {
                if t1.identifier_id == t2.identifier_id {
                    Ok(())
                } else {
                    Err(format!(
                        "expected type variable {} but got {}",
                        &*self.get_ident_str(t1.identifier_id),
                        &*self.get_ident_str(t2.identifier_id)
                    ))
                }
            }
            (Type::Reference(o1), Type::Reference(o2)) => {
                self.typecheck_types(o1.inner_type, o2.inner_type, scope_id)
            }
            (Type::Enum(_e), Type::TagInstance(_t)) => {
                eprintln!(
                    "typechecking enum vs a tag instance {} {}",
                    self.type_id_to_string(expected),
                    self.type_id_to_string(actual)
                );
                Err("unimplemented".to_string())
            }
            (exp, act) => {
                // Resolve type variables
                if let Type::TypeVariable(expected_type_var) = exp {
                    if let Some(expected_resolved) =
                        self.scopes.find_type(scope_id, expected_type_var.identifier_id)
                    {
                        // We will recursively just resolve to the same type variable without this check
                        // this check requires us to make progress. Doesn't prevent cycles though I guess
                        if expected_resolved != expected {
                            println!(
                                "Expected '{}' resolved to {}",
                                &*self.ast.get_ident_str(expected_type_var.identifier_id),
                                self.type_id_to_string(expected_resolved)
                            );
                            return self.typecheck_types(expected_resolved, actual, scope_id);
                        }
                    }
                }
                if let Type::TypeVariable(actual_type_var) = act {
                    if let Some(actual_resolved) =
                        self.scopes.find_type(scope_id, actual_type_var.identifier_id)
                    {
                        // We will recursively just resolve to the same type variable without this check
                        // this check requires us to make progress. Doesn't prevent cycles though I guess
                        if actual_resolved != actual {
                            return self.typecheck_types(expected, actual_resolved, scope_id);
                        }
                        return self.typecheck_types(expected, actual_resolved, scope_id);
                    }
                }
                Err(format!(
                    "Expected {} but got {}",
                    self.type_to_string(exp),
                    self.type_to_string(act)
                ))
            }
        }
    }

    fn eval_const(&mut self, parsed_constant_id: ParsedConstantId) -> TyperResult<VariableId> {
        let ast = self.ast.clone();
        let parsed_constant = ast.get_constant(parsed_constant_id);
        let scope_id = 0;
        let type_id = self.eval_const_type_expr(&parsed_constant.ty)?;
        let expr = match &*self.ast.get_expression(parsed_constant.value_expr) {
            ParsedExpression::Literal(Literal::Numeric(n, span)) => {
                let num = self.parse_numeric(n).map_err(|msg| make_error(msg, *span))?;
                TypedExpr::Int(num, parsed_constant.span)
            }
            ParsedExpression::Literal(Literal::Bool(b, span)) => TypedExpr::Bool(*b, *span),
            ParsedExpression::Literal(Literal::Char(c, span)) => TypedExpr::Char(*c, *span),
            _other => {
                return make_fail(
                    "Only literals are currently supported as constants",
                    parsed_constant.span,
                );
            }
        };
        let variable_id = self.add_variable(Variable {
            name: parsed_constant.name,
            type_id,
            is_mutable: false,
            owner_scope: None,
        });
        self.constants.push(Constant {
            variable_id,
            expr,
            ty: type_id,
            span: parsed_constant.span,
        });
        self.scopes.add_variable(scope_id, parsed_constant.name, variable_id);
        Ok(variable_id)
    }

    fn get_stmt_expression_type(&self, stmt: &TypedStmt) -> TypeId {
        match stmt {
            TypedStmt::Expr(expr) => expr.get_type(),
            TypedStmt::ValDef(_) => UNIT_TYPE_ID,
            TypedStmt::Assignment(_) => UNIT_TYPE_ID,
            TypedStmt::WhileLoop(_) => UNIT_TYPE_ID,
        }
    }

    fn add_variable(&mut self, variable: Variable) -> VariableId {
        let id = self.variables.len();
        self.variables.push(variable);
        id as u32
    }

    pub fn get_variable(&self, id: VariableId) -> &Variable {
        &self.variables[id as usize]
    }

    fn add_function(&mut self, function: TypedFunction) -> FunctionId {
        let id = self.functions.len();
        self.functions.push(function);
        id as u32
    }

    fn add_namespace(&mut self, namespace: Namespace) -> NamespaceId {
        let id = self.namespaces.len();
        self.namespaces.push(namespace);
        id as u32
    }

    pub fn get_function(&self, function_id: FunctionId) -> &TypedFunction {
        &self.functions[function_id as usize]
    }

    pub fn get_function_mut(&mut self, function_id: FunctionId) -> &mut TypedFunction {
        &mut self.functions[function_id as usize]
    }

    fn parse_numeric(&self, s: &str) -> Result<i64, String> {
        // Eventually we need to find out what type of number literal this is.
        // For now we only support i64
        let num: i64 = s.parse().map_err(|_e| "Failed to parse signed numeric literal")?;
        Ok(num)
    }

    // If the expr is already a block, do nothing
    // If it is not, make a new block with just this expression inside.
    // Used main for if/else
    fn transform_expr_to_block(&mut self, expr: TypedExpr, block_scope: ScopeId) -> TypedBlock {
        match expr {
            TypedExpr::Block(b) => b,
            expr => {
                let ret_type = expr.get_type();
                let span = expr.get_span();
                let statement = TypedStmt::Expr(Box::new(expr));
                let statements = vec![statement];

                TypedBlock { expr_type: ret_type, scope_id: block_scope, statements, span }
            }
        }
    }

    fn coerce_block_to_unit_block(&mut self, block: &mut TypedBlock) {
        let span = block.statements.last().map(|s| s.get_span()).unwrap_or(block.span);
        let unit_literal = TypedExpr::unit_literal(span);
        block.statements.push(TypedStmt::Expr(Box::new(unit_literal)));
        block.expr_type = UNIT_TYPE_ID;
    }

    fn traverse_namespace_chain(
        &self,
        scope_id: ScopeId,
        namespaces: &[IdentifierId],
        span: Span,
    ) -> TyperResult<ScopeId> {
        trace!(
            "traverse_namespace_chain: {:?}",
            namespaces.iter().map(|id| self.get_ident_str(*id).to_string()).collect::<Vec<_>>()
        );
        let ns_iter = namespaces.iter();
        let mut cur_scope = scope_id;
        for ns in ns_iter {
            let namespace_id = self.scopes.find_namespace(cur_scope, *ns).ok_or(make_error(
                format!(
                    "Namespace not found: {} in scope: {:?}",
                    &*self.get_ident_str(*ns),
                    self.scopes.get_scope(scope_id)
                ),
                span,
            ))?;
            let namespace = self.get_namespace(namespace_id);
            cur_scope = namespace.scope_id;
        }
        Ok(cur_scope)
    }

    fn eval_index_operation(
        &mut self,
        index_op: &IndexOperation,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExpr> {
        let index_expr = self.eval_expr(index_op.index_expr, scope_id, Some(INT_TYPE_ID))?;
        if index_expr.get_type() != INT_TYPE_ID {
            return make_fail("index type must be int", index_op.span);
        }

        let base_expr = self.eval_expr(index_op.target, scope_id, None)?;
        let target_type = base_expr.get_type();
        match target_type {
            STRING_TYPE_ID => Ok(TypedExpr::StringIndex(IndexOp {
                base_expr: Box::new(base_expr),
                index_expr: Box::new(index_expr),
                result_type: CHAR_TYPE_ID,
                span: index_op.span,
            })),
            target_type_id => {
                let target_type = self.get_type(target_type_id);
                match target_type {
                    Type::Array(array_type) => Ok(TypedExpr::ArrayIndex(IndexOp {
                        base_expr: Box::new(base_expr),
                        index_expr: Box::new(index_expr),
                        result_type: array_type.element_type,
                        span: index_op.span,
                    })),
                    _ => make_fail("index base must be an array", index_op.span),
                }
            }
        }
    }

    fn eval_variable(
        &mut self,
        variable: &parse::Variable,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExpr> {
        let variable_id = self.scopes.find_variable(scope_id, variable.name).ok_or(make_error(
            format!("{} is not defined", &*self.get_ident_str(variable.name)),
            variable.span,
        ))?;
        let v = self.get_variable(variable_id);
        if is_assignment_lhs && !v.is_mutable {
            return make_fail(
                format!("Cannot assign to immutable variable {}", &*self.get_ident_str(v.name)),
                variable.span,
            );
        }
        let expr = TypedExpr::Variable(VariableExpr {
            type_id: v.type_id,
            variable_id,
            span: variable.span,
        });
        trace!(
            "variable {} had type {}",
            &*self.get_ident_str(variable.name),
            self.type_id_to_string(v.type_id)
        );
        Ok(expr)
    }

    fn eval_field_access(
        &mut self,
        field_access: &parse::FieldAccess,
        scope_id: ScopeId,
        is_assignment_lhs: bool,
    ) -> TyperResult<TypedExpr> {
        let mut base_expr = self.eval_expr(field_access.base, scope_id, None)?;
        let type_id = base_expr.get_type();
        let maybe_record_type = match self.get_type(type_id) {
            Type::Reference(reference_type) => {
                // Auto de-reference records for  field access
                let maybe_record = self.get_type(reference_type.inner_type).as_record();
                if !is_assignment_lhs && maybe_record.is_some() {
                    // Dereference the base expression
                    base_expr = TypedExpr::UnaryOp(UnaryOp {
                        kind: UnaryOpKind::Dereference,
                        type_id: reference_type.inner_type,
                        span: base_expr.get_span(),
                        expr: Box::new(base_expr),
                    });
                }
                maybe_record
            }
            Type::Record(record_type) => Some(record_type),
            _ => None,
        };
        let Some(record_type) = maybe_record_type else {
            return make_fail(
                format!(
                    "Cannot access field {} on non-record type: {}",
                    &*self.get_ident_str(field_access.target),
                    self.type_id_to_string(base_expr.get_type())
                ),
                field_access.span,
            );
        };
        let (field_index, target_field) =
            record_type.find_field(field_access.target).ok_or(make_error(
                format!(
                    "Field {} not found on record type",
                    &*self.get_ident_str(field_access.target)
                ),
                field_access.span,
            ))?;
        Ok(TypedExpr::RecordFieldAccess(FieldAccess {
            base: Box::new(base_expr),
            target_field: field_access.target,
            target_field_index: field_index as u32,
            ty: target_field.type_id,
            span: field_access.span,
        }))
    }

    fn eval_assigment_lhs_expr(
        &mut self,
        expr: ExpressionId,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        match &*self.ast.clone().get_expression(expr) {
            ParsedExpression::IndexOperation(index_op) => {
                self.eval_index_operation(index_op, scope_id)
            }
            ParsedExpression::Variable(variable) => self.eval_variable(variable, scope_id, true),
            ParsedExpression::FieldAccess(field_access) => {
                self.eval_field_access(field_access, scope_id, true)
            }
            other => make_fail(format!("Invalid assignment lhs: {:?}", other), other.get_span()),
        }
    }

    /// Used for
    /// - Auto Some()-boxing,
    /// - auto-referencing,
    /// - de-referencing,
    /// - enum variant construction
    /// This is a good place to do this because we just typechecked an expression, and
    /// we know the expected type.
    fn coerce_expression_to_expected_type(
        &mut self,
        expected_type_id: TypeId,
        expression: TypedExpr,
        scope_id: ScopeId,
    ) -> TypedExpr {
        // For some reason, we skip coercion for 'None'. Need to run that down
        if let TypedExpr::None(_type_id, _span) = expression {
            return expression;
        }
        match self.get_type(expected_type_id) {
            Type::Enum(e) => match &expression {
                TypedExpr::Tag(tag_expr) => {
                    let matching_variant = e.variants.iter().find(|v| v.tag_name == tag_expr.name);
                    if let Some(matching_variant) = matching_variant {
                        if let Some(p) = matching_variant.payload {
                            eprintln!(
                                "We do not have a matching variant: {}",
                                self.type_id_to_string(p)
                            );
                            expression
                        } else {
                            eprintln!("We have a matching variant: {:?}", matching_variant);
                            let span = tag_expr.span;
                            TypedExpr::EnumConstructor(TypedEnumConstructor {
                                type_id: expected_type_id,
                                variant_name: matching_variant.tag_name,
                                variant_index: matching_variant.index,
                                payload: None,
                                span,
                            })
                        }
                    } else {
                        // We 'fail' by just giving up the coercion and let typechecking fail
                        // better
                        expression
                    }
                }
                _ => expression,
            },
            Type::Optional(optional_type) => {
                match self.typecheck_types(
                    optional_type.inner_type,
                    expression.get_type(),
                    scope_id,
                ) {
                    Ok(_) => TypedExpr::OptionalSome(OptionalSome {
                        inner_expr: Box::new(expression),
                        type_id: expected_type_id,
                    }),
                    Err(_) => expression,
                }
            }
            Type::Reference(reference_type) => {
                match self.typecheck_types(
                    reference_type.inner_type,
                    expression.get_type(),
                    scope_id,
                ) {
                    Ok(_) => {
                        let base_span = expression.get_span();
                        TypedExpr::UnaryOp(UnaryOp {
                            kind: UnaryOpKind::Reference,
                            type_id: expected_type_id,
                            expr: Box::new(expression),
                            span: base_span,
                        })
                    }
                    Err(_) => expression,
                }
            }
            _ => expression,
        }
    }

    fn eval_expr(
        &mut self,
        expr: ExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let ast = self.ast.clone();
        let expected_type = match ast.get_expression_type_hint(expr) {
            Some(t) => {
                let type_id = self.eval_type_expr(t.deref(), scope_id)?;
                Some(type_id)
            }
            None => expected_type,
        };
        let base_result = self.eval_expr_inner(expr, scope_id, expected_type)?;

        if let Some(expected_type_id) = expected_type {
            let coerced =
                self.coerce_expression_to_expected_type(expected_type_id, base_result, scope_id);
            Ok(coerced)
        } else {
            Ok(base_result)
        }
    }

    /// Passing `expected_type` is an optimization that can save us work.
    /// It does not guarantee that the returned expr always conforms
    /// to the given `expected_type`
    /// Although, maybe we re-think that because it would save
    /// a lot of code if we did a final check here before returning!
    fn eval_expr_inner(
        &mut self,
        expr: ExpressionId,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        trace!(
            "eval_expr: {}: {:?}",
            &self.ast.expression_to_string(expr),
            expected_type.map(|t| self.type_id_to_string(t))
        );
        let ast = self.ast.clone();
        let expr = ast.get_expression(expr).clone();
        let result = match &(expr) {
            ParsedExpression::Array(array_expr) => {
                let mut element_type: Option<TypeId> = match expected_type {
                    Some(type_id) => match self.get_type(type_id) {
                        Type::Array(arr) => Ok(Some(arr.element_type)),
                        _ => Ok(None),
                    },
                    None => Ok(None),
                }?;
                let elements: Vec<TypedExpr> = {
                    let mut elements = Vec::new();
                    for elem in &array_expr.elements {
                        let ir_expr = self.eval_expr(*elem, scope_id, element_type)?;
                        if element_type.is_none() {
                            element_type = Some(ir_expr.get_type())
                        };
                        elements.push(ir_expr);
                    }
                    elements
                };

                let element_type = element_type.expect("By now this should be populated");
                // Technically we should not insert a new type here if we already have a type_id
                // representing an Array with this element type. But maybe we just make
                // the type internment do an equality check instead, so the 'consumer' code
                // throughout the compiler doesn't have to worry about creating or not creating
                // duplicate types; this is what Andrew Kelley just implemented with Zig's
                // intern pool that does full equality checking
                // https://github.com/ziglang/zig/pull/15569
                let type_id = match expected_type {
                    Some(t) => t,
                    None => {
                        let array_type = ArrayType { element_type, span: array_expr.span };
                        self.add_type(Type::Array(array_type))
                    }
                };
                Ok(TypedExpr::Array(ArrayLiteral { elements, type_id, span: array_expr.span }))
            }
            ParsedExpression::IndexOperation(index_op) => {
                self.eval_index_operation(index_op, scope_id)
            }
            ParsedExpression::Record(ast_record) => {
                // FIXME: Let's factor out Structs and Records into separate things
                //        records can be created on the fly and are just hashmap literals
                //        Structs are structs
                let mut field_values = Vec::new();
                let mut field_defns = Vec::new();
                let expected_record = if let Some(expected_type) = expected_type {
                    match self.get_type(expected_type) {
                        Type::Record(record) => Some((expected_type, record.clone())),
                        Type::Optional(opt) => match self.get_type(opt.inner_type) {
                            Type::Record(record) => Some((opt.inner_type, record.clone())),
                            other_ty => {
                                return make_fail(
                                    format!(
                                        "Got record literal but expected {}",
                                        self.type_to_string(other_ty)
                                    ),
                                    ast_record.span,
                                );
                            }
                        },
                        Type::Reference(refer) => match self.get_type(refer.inner_type) {
                            Type::Record(record) => Some((refer.inner_type, record.clone())),
                            other_ty => {
                                return make_fail(
                                    format!(
                                        "Got record literal but expected {}",
                                        self.type_to_string(other_ty)
                                    ),
                                    ast_record.span,
                                );
                            }
                        },
                        other_ty => {
                            return make_fail(
                                format!(
                                    "Got record literal but expected {}",
                                    self.type_to_string(other_ty)
                                ),
                                ast_record.span,
                            );
                        }
                    }
                } else {
                    None
                };
                for (index, ast_field) in ast_record.fields.iter().enumerate() {
                    let expected_field = expected_record
                        .as_ref()
                        .and_then(|(_, rec)| rec.find_field(ast_field.name));
                    let expected_type_id = expected_field.map(|(_, f)| f.type_id);
                    let expr = self.eval_expr(ast_field.expr, scope_id, expected_type_id)?;
                    field_defns.push(RecordTypeField {
                        name: ast_field.name,
                        type_id: expr.get_type(),
                        index,
                    });
                    field_values.push(RecordField { name: ast_field.name, expr });
                }
                // We can use 'expected type' here to just go ahead and typecheck or fail
                // rather than make a duplicate type
                let record_type_id = match expected_record {
                    None => {
                        let record_type = RecordType {
                            fields: field_defns,
                            name_if_named: None,
                            span: ast_record.span,
                        };
                        let anon_record_type_id = self.add_type(Type::Record(record_type));
                        Ok(anon_record_type_id)
                    }
                    Some((expected_type_id, expected_record)) => {
                        match self.typecheck_record(
                            &expected_record,
                            &RecordType {
                                fields: field_defns,
                                name_if_named: None,
                                span: ast_record.span,
                            },
                            scope_id,
                        ) {
                            Ok(_) => Ok(expected_type_id),
                            Err(s) => {
                                make_fail(format!("Invalid record type: {}", s), ast_record.span)
                            }
                        }
                    }
                }?;
                let typed_record =
                    Record { fields: field_values, span: ast_record.span, type_id: record_type_id };
                let expr = TypedExpr::Record(typed_record);
                trace!("generated record: {}", self.expr_to_string(&expr));
                Ok(expr)
            }
            ParsedExpression::If(if_expr) => self.eval_if_expr(if_expr, scope_id, expected_type),
            ParsedExpression::BinaryOp(binary_op) => {
                // Infer expected type to be type of operand1
                match binary_op.op_kind {
                    BinaryOpKind::Equals | BinaryOpKind::NotEquals => {
                        return self.eval_equality_expr(binary_op, scope_id, expected_type);
                    }
                    _ => {}
                };
                let lhs = self.eval_expr(binary_op.lhs, scope_id, None)?;
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs.get_type()))?;

                // FIXME: Typechecker We are not really typechecking binary operations at all.
                //        This is not enough; we need to check that the lhs is actually valid
                //        for this operation first
                if self.typecheck_types(lhs.get_type(), rhs.get_type(), scope_id).is_err() {
                    return make_fail("operand types did not match", binary_op.span);
                }

                let kind = binary_op.op_kind;
                let result_type = match kind {
                    BinaryOpKind::Add => lhs.get_type(),
                    BinaryOpKind::Subtract => lhs.get_type(),
                    BinaryOpKind::Multiply => lhs.get_type(),
                    BinaryOpKind::Divide => lhs.get_type(),
                    BinaryOpKind::Less => BOOL_TYPE_ID,
                    BinaryOpKind::LessEqual => BOOL_TYPE_ID,
                    BinaryOpKind::Greater => BOOL_TYPE_ID,
                    BinaryOpKind::GreaterEqual => BOOL_TYPE_ID,
                    BinaryOpKind::And => lhs.get_type(),
                    BinaryOpKind::Or => lhs.get_type(),
                    BinaryOpKind::Equals => BOOL_TYPE_ID,
                    BinaryOpKind::NotEquals => BOOL_TYPE_ID,
                };
                let expr = TypedExpr::BinaryOp(BinaryOp {
                    kind,
                    ty: result_type,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                });
                Ok(expr)
            }
            ParsedExpression::UnaryOp(op) => {
                let base_expr = self.eval_expr(op.expr, scope_id, None)?;
                match op.op_kind {
                    UnaryOpKind::Dereference => {
                        let reference_type = self
                            .get_type(base_expr.get_type())
                            .as_reference()
                            .ok_or(make_error(
                                format!(
                                    "Cannot dereference non-reference type: {}",
                                    self.type_id_to_string(base_expr.get_type())
                                ),
                                op.span,
                            ))?;
                        eprintln!(
                            "DEREFERENCING: {}: {}",
                            self.expr_to_string(&base_expr),
                            self.type_id_to_string(base_expr.get_type())
                        );
                        Ok(TypedExpr::UnaryOp(UnaryOp {
                            kind: UnaryOpKind::Dereference,
                            type_id: reference_type.inner_type,
                            expr: Box::new(base_expr),
                            span: op.span,
                        }))
                    }
                    UnaryOpKind::Reference => {
                        let type_id = self.add_type(Type::Reference(ReferenceType {
                            inner_type: base_expr.get_type(),
                        }));
                        Ok(TypedExpr::UnaryOp(UnaryOp {
                            kind: UnaryOpKind::Reference,
                            type_id,
                            expr: Box::new(base_expr),
                            span: op.span,
                        }))
                    }
                    UnaryOpKind::BooleanNegation => {
                        self.typecheck_types(BOOL_TYPE_ID, base_expr.get_type(), scope_id)
                            .map_err(|s| make_error(s, op.span))?;
                        Ok(TypedExpr::UnaryOp(UnaryOp {
                            kind: UnaryOpKind::BooleanNegation,
                            type_id: BOOL_TYPE_ID,
                            expr: Box::new(base_expr),
                            span: op.span,
                        }))
                    }
                }
            }
            ParsedExpression::Literal(Literal::Unit(span)) => Ok(TypedExpr::Unit(*span)),
            ParsedExpression::Literal(Literal::None(span)) => {
                // If we are expecting an Option, I need to reach inside it to get the inner type
                let expected_type = expected_type.ok_or(make_error(
                    "Cannot infer type of None literal without type hint",
                    *span,
                ))?;
                let expected_type =
                    self.get_type(expected_type).as_optional().ok_or(make_error(
                        format!(
                            "Expected optional type for None literal but got {:?}",
                            expected_type
                        ),
                        *span,
                    ))?;
                let inner_type = expected_type.inner_type;
                let none_type = Type::Optional(OptionalType { inner_type });
                // FIXME: We'll re-create the type for optional int, bool, etc over and over. Instead of add_type it should be
                //        self.get_or_add_type()
                let type_id = self.add_type(none_type);
                Ok(TypedExpr::None(type_id, *span))
            }
            ParsedExpression::Literal(Literal::Char(byte, span)) => {
                Ok(TypedExpr::Char(*byte, *span))
            }
            ParsedExpression::Literal(Literal::Numeric(s, span)) => {
                let num = self.parse_numeric(s).map_err(|msg| make_error(msg, *span))?;
                Ok(TypedExpr::Int(num, *span))
            }
            ParsedExpression::Literal(Literal::Bool(b, span)) => {
                let expr = TypedExpr::Bool(*b, *span);
                Ok(expr)
            }
            ParsedExpression::Literal(Literal::String(s, span)) => {
                // So sad, we could just point to the source. BUT if we ever do escaping and things
                // then the source string is not the same as the string the user meant; perhaps here
                // is the place, or maybe in the parser, that we would actually do some work, which
                // would justify storing it separately. But then, post-transform, we should intern
                // these
                let expr = TypedExpr::Str(s.clone(), *span);
                Ok(expr)
            }
            ParsedExpression::Variable(variable) => self.eval_variable(variable, scope_id, false),
            ParsedExpression::FieldAccess(field_access) => {
                self.eval_field_access(field_access, scope_id, false)
            }
            ParsedExpression::Block(block) => {
                let block = self.eval_block(block, scope_id, expected_type)?;
                Ok(TypedExpr::Block(block))
            }
            ParsedExpression::MethodCall(m_call) => {
                let base_expr = self.eval_expr(m_call.base, scope_id, None)?;
                let call =
                    self.eval_function_call(&m_call.call, Some(base_expr), None, scope_id)?;
                Ok(call)
            }
            ParsedExpression::FnCall(fn_call) => {
                let call = self.eval_function_call(fn_call, None, None, scope_id)?;
                Ok(call)
            }
            ParsedExpression::OptionalGet(optional_get) => {
                let base = self.eval_expr_inner(optional_get.base, scope_id, expected_type)?;
                let Type::Optional(optional_type) = self.get_type(base.get_type()) else {
                    return make_fail(
                        format!(
                            "Cannot get value with ! from non-optional type: {}",
                            self.type_id_to_string(base.get_type())
                        ),
                        optional_get.span,
                    );
                };
                Ok(TypedExpr::OptionalGet(OptionalGet {
                    inner_expr: Box::new(base),
                    result_type_id: optional_type.inner_type,
                    span: optional_get.span,
                }))
            }
            ParsedExpression::For(for_expr) => {
                // We have to make a copy since eval_for_expr synthesizes expressions which borrows
                // from ast.expressions
                self.eval_for_expr(for_expr, scope_id, expected_type)
            }
            ParsedExpression::Tag(tag_expr) => {
                let type_id = self.get_type_for_tag(tag_expr.tag);
                let typed_expr = TypedExpr::Tag(TypedTagExpr {
                    name: tag_expr.tag,
                    type_id,
                    span: tag_expr.span,
                });
                Ok(typed_expr)
            }
            ParsedExpression::EnumConstructor(e) => {
                let expected_type = expected_type
                    .ok_or(make_error("Could not infer enum type from context", e.span))?;
                let enum_type = self
                    .get_type(expected_type)
                    .as_enum()
                    .ok_or(make_error("Expected an enum type", e.span))?;
                let typed_variant =
                    enum_type.variants.iter().find(|variant| variant.tag_name == e.tag).ok_or(
                        make_error(
                            format!(
                                "No variant {} exists in enum {}",
                                &*self.get_ident_str(e.tag),
                                self.type_id_to_string(expected_type)
                            ),
                            e.span,
                        ),
                    )?;
                let variant_payload = typed_variant.payload.ok_or(make_error(
                    format!("Variant {} does not take a payload", &*self.ast.get_ident_str(e.tag)),
                    e.span,
                ))?;
                let variant_index = typed_variant.index;
                let variant_name = typed_variant.tag_name;
                // drop(typed_variant);
                let payload_expr = self.eval_expr(e.payload, scope_id, Some(variant_payload))?;
                Ok(TypedExpr::EnumConstructor(TypedEnumConstructor {
                    type_id: expected_type,
                    payload: Some(Box::new(payload_expr)),
                    variant_index,
                    variant_name,
                    span: e.span,
                }))
            }
        };
        result
    }

    /// no_mangle: Skip mangling if we want the variable to be accessible from user code
    fn synth_variable_decl(
        &mut self,
        mut variable: Variable,
        span: Span,
        initializer: TypedExpr,
        no_mangle: bool,
    ) -> (VariableId, TypedStmt, TypedExpr) {
        let type_id = variable.type_id;
        let scope_id = variable.owner_scope.unwrap_or(self.scopes.get_root_scope_id());
        let new_ident = if no_mangle {
            variable.name
        } else {
            let new_ident_name =
                { format!("__{}_{}", &*self.ast.get_ident_str(variable.name), scope_id) };
            self.ast.ident_id(&new_ident_name)
        };
        variable.name = new_ident;
        let variable_id = self.add_variable(variable);
        let expr = VariableExpr { type_id, variable_id, span };
        let val_def =
            TypedStmt::ValDef(Box::new(ValDef { variable_id, ty: type_id, initializer, span }));
        self.scopes.add_variable(scope_id, new_ident, variable_id);
        (variable_id, val_def, TypedExpr::Variable(expr))
    }

    fn synth_variable_parsed_expr(&self, variable_id: VariableId, span: Span) -> ExpressionId {
        let ast = self.ast.clone();
        ast.add_expression(ParsedExpression::Variable(parse::Variable {
            name: self.get_variable(variable_id).name,
            namespaces: Vec::new(),
            span,
        }))
    }

    fn synth_function_call(
        &mut self,
        namespaces: Vec<IdentifierId>,
        name: IdentifierId,
        known_type_args: Option<Vec<TypeId>>,
        value_arg_exprs: Vec<ExpressionId>,
        span: Span,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExpr> {
        self.eval_function_call(
            &FnCall {
                name,
                type_args: None,
                args: value_arg_exprs
                    .into_iter()
                    .map(|expr| FnCallArg { name: None, value: expr })
                    .collect(),
                namespaces,
                span,
            },
            None,
            known_type_args,
            scope_id,
        )
    }

    fn eval_for_expr(
        &mut self,
        for_expr: &ForExpr,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let binding_ident = for_expr.binding.unwrap_or(self.ast.ident_id("it"));
        let iterable_expr = self.eval_expr(for_expr.iterable_expr, scope_id, None)?;
        let iteree_type = iterable_expr.get_type();
        let is_string_iteree = iteree_type == STRING_TYPE_ID;
        let iterable_span = iterable_expr.get_span();
        let body_span = for_expr.body_block.span;

        let Some(item_type) = self.item_type_of_iterable(iteree_type) else {
            return make_fail(
                format!(
                    "Type {} is not iterable",
                    self.type_id_to_string(iterable_expr.get_type())
                ),
                iterable_span,
            );
        };

        let is_do_block = for_expr.expr_type == ForExprType::Do;

        // We de-sugar the 'for ... do' expr into a typed while loop, synthesizing
        // a few local variables in order to achieve this.
        // I think these are broken scoping-wise and will collide with user code currently!

        let for_expr_scope = self.scopes.add_child_scope(scope_id, ScopeType::ForExpr, None);

        let (index_variable, index_defn_stmt, index_variable_expr) = self.synth_variable_decl(
            Variable {
                name: self.ast.ident_id("it_index"),
                type_id: INT_TYPE_ID,
                is_mutable: true,
                owner_scope: Some(for_expr_scope),
            },
            body_span,
            TypedExpr::Int(0, for_expr.body_block.span),
            true,
        );
        let (iteree_variable_id, iteree_defn_stmt, iteree_variable_expr) = self
            .synth_variable_decl(
                Variable {
                    name: self.ast.ident_id("iteree"),
                    type_id: iteree_type,
                    is_mutable: false,
                    owner_scope: Some(for_expr_scope),
                },
                iterable_span,
                iterable_expr,
                false,
            );
        let iteree_length_call = if is_string_iteree {
            self.synth_function_call(
                vec![self.ast.ident_id("string")],
                self.ast.ident_id("length"),
                None,
                vec![self.synth_variable_parsed_expr(iteree_variable_id, iterable_span)],
                iterable_span,
                for_expr_scope,
            )?
        } else {
            self.synth_function_call(
                vec![self.ast.ident_id("Array")],
                self.ast.ident_id("length"),
                Some(vec![item_type]),
                vec![self.synth_variable_parsed_expr(iteree_variable_id, iterable_span)],
                iterable_span,
                for_expr_scope,
            )?
        };
        let (iteree_length_variable_id, iteree_length_defn_stmt, iteree_length_variable_expr) =
            self.synth_variable_decl(
                Variable {
                    name: self.ast.ident_id("iteree_length"),
                    type_id: INT_TYPE_ID,
                    is_mutable: false,
                    owner_scope: Some(for_expr_scope),
                },
                iterable_span,
                iteree_length_call,
                false,
            );

        let while_scope_id =
            self.scopes.add_child_scope(for_expr_scope, ScopeType::WhileBody, None);
        let binding_variable_id = self.add_variable(Variable {
            name: binding_ident,
            type_id: item_type,
            is_mutable: false,
            owner_scope: Some(while_scope_id),
        });
        self.scopes.add_variable(while_scope_id, binding_ident, binding_variable_id);
        let iteration_element_val_def = TypedStmt::ValDef(Box::new(ValDef {
            variable_id: binding_variable_id,
            ty: item_type,
            initializer: if is_string_iteree {
                TypedExpr::StringIndex(IndexOp {
                    base_expr: Box::new(iteree_variable_expr.clone()),
                    index_expr: Box::new(TypedExpr::Variable(VariableExpr {
                        type_id: INT_TYPE_ID,
                        variable_id: index_variable,
                        span: body_span,
                    })),
                    result_type: item_type,
                    span: body_span,
                })
            } else {
                TypedExpr::ArrayIndex(IndexOp {
                    base_expr: Box::new(iteree_variable_expr.clone()),
                    index_expr: Box::new(TypedExpr::Variable(VariableExpr {
                        type_id: INT_TYPE_ID,
                        variable_id: index_variable,
                        span: body_span,
                    })),
                    result_type: item_type,
                    span: body_span,
                })
            },
            span: body_span,
        }));

        // TODO: we can hint to the block based on the expected type of the entire for expr
        let body_scope_id =
            self.scopes.add_child_scope(while_scope_id, ScopeType::LexicalBlock, None);
        let body_block = self.eval_block(&for_expr.body_block, body_scope_id, None)?;
        let body_block_result_type = body_block.expr_type;

        let resulting_type = if is_do_block {
            UNIT_TYPE_ID
        } else {
            match self.get_type(iteree_type) {
                Type::Optional(_opt) => {
                    let new_optional =
                        Type::Optional(OptionalType { inner_type: body_block.expr_type });
                    self.add_type(new_optional)
                }
                Type::Array(_arr) => {
                    let new_array = Type::Array(ArrayType {
                        element_type: body_block.expr_type,
                        span: for_expr.span,
                    });
                    self.add_type(new_array)
                }
                Type::String => {
                    let new_array = Type::Array(ArrayType {
                        element_type: body_block.expr_type,
                        span: for_expr.span,
                    });
                    self.add_type(new_array)
                }
                other => todo!("Unsupported iteree type: {}", self.type_to_string(other)),
            }
        };
        let yield_decls = if !is_do_block {
            let yield_variable = Variable {
                name: self.ast.ident_id("yielded_coll"),
                type_id: resulting_type,
                is_mutable: false,
                owner_scope: Some(for_expr_scope),
            };

            let yield_initializer = match self.get_type(resulting_type) {
                Type::Array(_array_type) => self.synth_function_call(
                    vec![self.ast.ident_id("Array")],
                    self.ast.ident_id("new"),
                    Some(vec![body_block_result_type]),
                    vec![self.synth_variable_parsed_expr(iteree_length_variable_id, iterable_span)],
                    body_span,
                    for_expr_scope,
                )?,
                _ => {
                    return make_fail("unsupported resulting_type in for_expr yield", for_expr.span)
                }
            };
            Some(self.synth_variable_decl(yield_variable, body_span, yield_initializer, false))
        } else {
            None
        };
        let mut while_block = TypedBlock {
            expr_type: UNIT_TYPE_ID,
            scope_id: while_scope_id,
            statements: Vec::new(),
            span: body_span,
        };
        // Prepend the val def to the body block
        while_block.statements.push(iteration_element_val_def);
        let (_user_block_val_id, user_block_val_def, user_block_val_expr) = self
            .synth_variable_decl(
                Variable {
                    name: self.ast.ident_id("block_expr_val"),
                    type_id: body_block.expr_type,
                    is_mutable: false,
                    owner_scope: Some(while_scope_id),
                },
                body_span,
                TypedExpr::Block(body_block),
                false,
            );
        while_block.statements.push(user_block_val_def);

        // Assign element to yielded array
        if let Some((_yield_coll_variable_id, _yield_def, yielded_coll_expr)) = &yield_decls {
            match self.get_type(resulting_type) {
                Type::Array(_) => {
                    // yielded_coll[index] = block_expr_val;
                    let element_assign = TypedStmt::Assignment(Box::new(Assignment {
                        destination: Box::new(TypedExpr::ArrayIndex(IndexOp {
                            base_expr: Box::new(yielded_coll_expr.clone()),
                            index_expr: Box::new(index_variable_expr.clone()),
                            result_type: body_block_result_type,
                            span: body_span,
                        })),
                        value: Box::new(user_block_val_expr),
                        span: body_span,
                    }));
                    while_block.statements.push(element_assign);
                }
                _ => {}
            }
        }

        // Append the index increment to the body block
        let index_increment_statement = TypedStmt::Assignment(Box::new(Assignment {
            destination: Box::new(index_variable_expr.clone()),
            value: Box::new(TypedExpr::BinaryOp(BinaryOp {
                kind: BinaryOpKind::Add,
                ty: INT_TYPE_ID,
                lhs: Box::new(TypedExpr::Variable(VariableExpr {
                    type_id: INT_TYPE_ID,
                    variable_id: index_variable,
                    span: iterable_span,
                })),
                rhs: Box::new(TypedExpr::Int(1, iterable_span)),
                span: iterable_span,
            })),
            span: iterable_span,
        }));
        while_block.statements.push(index_increment_statement);

        let while_stmt = TypedStmt::WhileLoop(Box::new(TypedWhileLoop {
            cond: TypedExpr::BinaryOp(BinaryOp {
                kind: BinaryOpKind::Less,
                ty: BOOL_TYPE_ID,
                lhs: Box::new(index_variable_expr.clone()),
                rhs: Box::new(iteree_length_variable_expr),
                span: iterable_span,
            }),
            block: while_block,
            span: for_expr.span,
        }));

        let mut for_expr_initial_statements = Vec::with_capacity(4);
        for_expr_initial_statements.push(index_defn_stmt);
        for_expr_initial_statements.push(iteree_defn_stmt);
        for_expr_initial_statements.push(iteree_length_defn_stmt);
        if let Some((_yield_id, yield_def, _yield_expr)) = &yield_decls {
            for_expr_initial_statements.push(yield_def.clone());
        }
        let mut for_expr_block = TypedBlock {
            expr_type: resulting_type,
            scope_id: for_expr_scope,
            statements: for_expr_initial_statements,
            span: for_expr.body_block.span,
        };

        for_expr_block.statements.push(while_stmt);
        if let Some((yielded_variable_id, _, _)) = &yield_decls {
            let yield_expr = TypedExpr::Variable(VariableExpr {
                type_id: resulting_type,
                variable_id: *yielded_variable_id,
                span: for_expr.body_block.span,
            });
            for_expr_block.statements.push(TypedStmt::Expr(Box::new(yield_expr)));
        }

        let final_expr = TypedExpr::Block(for_expr_block);
        // eprintln!("{}", self.expr_to_string(&final_expr));
        Ok(final_expr)
    }

    fn expect_ability_implementation(
        &self,
        ability_id: AbilityId,
        type_id: TypeId,
        span_for_error: Span,
    ) -> TyperResult<&AbilityImplementation> {
        self.implementations
            .iter()
            .find(|imple| imple.type_id == type_id && imple.ability_id == ability_id)
            .ok_or(make_error("Missing ability implementation", span_for_error))
    }

    fn eval_equality_expr(
        &mut self,
        binary_op: &parse::BinaryOp,
        scope_id: ScopeId,
        _expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        assert!(
            binary_op.op_kind == BinaryOpKind::Equals
                || binary_op.op_kind == BinaryOpKind::NotEquals
        );
        let lhs = self.eval_expr(binary_op.lhs, scope_id, None)?;
        let equals_expr = match self.get_type(lhs.get_type()) {
            Type::Unit | Type::Char | Type::Int | Type::Bool => {
                // All these scalar types treated as IntValue s in codegen
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs.get_type()))?;
                if rhs.get_type() == lhs.get_type() {
                    Ok(TypedExpr::BinaryOp(BinaryOp {
                        kind: BinaryOpKind::Equals,
                        ty: BOOL_TYPE_ID,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span: binary_op.span,
                    }))
                } else {
                    make_fail("Bad rhs type", binary_op.span)
                }
            }
            Type::String => {
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs.get_type()))?;
                if rhs.get_type() != STRING_TYPE_ID {
                    make_fail("Expected string on rhs", binary_op.span)
                } else {
                    // Look in the root scope only
                    let equals_ability_id = self
                        .scopes
                        .get_root_scope()
                        .find_ability(self.ast.ident_id("Equals"))
                        .unwrap();

                    let implementation = self.expect_ability_implementation(
                        equals_ability_id,
                        STRING_TYPE_ID,
                        binary_op.span,
                    )?;
                    let ability = self.get_ability(equals_ability_id);
                    let equals_index =
                        ability.find_function_by_name(self.ast.ident_id("equals")).unwrap().0;
                    let equals_implementation_function_id = implementation.functions[equals_index];
                    let call_expr = TypedExpr::FunctionCall(Call {
                        callee_function_id: equals_implementation_function_id,
                        args: vec![lhs, rhs],
                        ret_type: BOOL_TYPE_ID,
                        span: binary_op.span,
                    });
                    Ok(call_expr)
                }
            }
            Type::Record(_record_defn) => {
                todo!("record equality")
            }
            Type::Array(array) => {
                // We need a cleaner way to generate _generic_ calls in typer
                // known_type_args is a good start, but I think we need to be able to
                // pass in pre-evaluated value args as well and pick up from there with specialization
                let array_equality_call = self.synth_function_call(
                    vec![self.ast.ident_id("Array")],
                    self.ast.ident_id("equals"),
                    Some(vec![array.element_type]),
                    vec![binary_op.lhs, binary_op.rhs],
                    binary_op.span,
                    scope_id,
                )?;
                Ok(array_equality_call)
            }
            Type::Optional(_opt) => {
                todo!("optional equality")
            }
            Type::Reference(_refer) => {
                todo!("reference equality")
            }
            Type::TypeVariable(_type_var) => {
                let rhs = self.eval_expr(binary_op.rhs, scope_id, Some(lhs.get_type()))?;
                if let Err(msg) = self.typecheck_types(lhs.get_type(), rhs.get_type(), scope_id) {
                    return make_fail(
                        format!("Type mismatch on equality of 2 generic variables: {}", msg),
                        binary_op.span,
                    );
                }
                Ok(TypedExpr::BinaryOp(BinaryOp {
                    kind: BinaryOpKind::Equals,
                    ty: BOOL_TYPE_ID,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                }))
            }
            Type::TagInstance(tag) => {
                let tag_ident = tag.ident;
                let rhs = self.eval_expr(binary_op.rhs, scope_id, None)?;
                let Type::TagInstance(rhs_tag) = self.get_type(rhs.get_type()) else {
                    return make_fail("Expected string on rhs", binary_op.span);
                };
                if rhs_tag.ident != tag_ident {
                    return make_fail("Cannot compare different tags for equality", binary_op.span);
                }
                Ok(TypedExpr::BinaryOp(BinaryOp {
                    kind: BinaryOpKind::Equals,
                    ty: BOOL_TYPE_ID,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                }))
            }
            Type::Enum(_e) => {
                todo!("equality enum lhs")
            }
        }?;
        if binary_op.op_kind == BinaryOpKind::Equals {
            Ok(equals_expr)
        } else {
            Ok(TypedExpr::UnaryOp(UnaryOp {
                kind: UnaryOpKind::BooleanNegation,
                type_id: BOOL_TYPE_ID,
                expr: Box::new(equals_expr),
                span: binary_op.span,
            }))
        }
    }

    fn eval_if_expr(
        &mut self,
        if_expr: &IfExpr,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedExpr> {
        let mut condition = self.eval_expr(if_expr.cond, scope_id, None)?;
        let consequent_scope_id = self.scopes.add_child_scope(scope_id, ScopeType::IfBody, None);
        let mut consequent = if if_expr.optional_ident.is_some() {
            let condition_optional_type = match self.get_type(condition.get_type()) {
                Type::Optional(opt) => opt,
                _other => {
                    return make_fail(
                        "Condition type for if with binding must be an optional",
                        condition.get_span(),
                    );
                }
            };
            let inner_type = condition_optional_type.inner_type;
            let (binding, binding_span) = if_expr.optional_ident.expect("We already checked this");
            // Make a variable with the identifier binding from the expr
            // That is the non-optional type of the condition's type
            let narrowed_variable = Variable {
                name: binding,
                type_id: inner_type,
                is_mutable: false,
                // This should be the scope of the consequent expr
                owner_scope: Some(scope_id),
            };
            let narrowed_variable_id = self.add_variable(narrowed_variable);
            let consequent_scope = self.scopes.get_scope_mut(consequent_scope_id);
            consequent_scope.add_variable(binding, narrowed_variable_id);
            let original_condition = condition.clone();
            condition = TypedExpr::OptionalHasValue(Box::new(condition));
            let consequent_expr =
                self.eval_expr(if_expr.cons, consequent_scope_id, expected_type)?;
            let mut consequent = self.transform_expr_to_block(consequent_expr, consequent_scope_id);
            consequent.statements.insert(
                0,
                TypedStmt::ValDef(Box::new(ValDef {
                    variable_id: narrowed_variable_id,
                    ty: inner_type,
                    initializer: TypedExpr::OptionalGet(OptionalGet {
                        inner_expr: Box::new(original_condition),
                        result_type_id: inner_type,
                        span: binding_span,
                    }),
                    span: binding_span,
                })),
            );
            consequent
        } else {
            // If there is no binding, the condition must be a boolean
            if let Err(msg) = self.typecheck_types(BOOL_TYPE_ID, condition.get_type(), scope_id) {
                return make_fail(
                    format!("Invalid if condition type: {}. If you intended to use a binding optional if, you must supply a binding using |<ident>|", msg),
                    condition.get_span(),
                );
            }
            let consequent_expr =
                self.eval_expr(if_expr.cons, consequent_scope_id, expected_type)?;
            self.transform_expr_to_block(consequent_expr, consequent_scope_id)
        };
        let consequent_type = consequent.expr_type;
        // De-sugar if without else:
        // If there is no alternate, we coerce the consequent to return Unit, so both
        // branches have a matching type, making codegen simpler
        if if_expr.alt.is_none() {
            self.coerce_block_to_unit_block(&mut consequent);
        };
        let alternate_scope = self.scopes.add_child_scope(scope_id, ScopeType::ElseBody, None);
        let alternate = if let Some(alt) = if_expr.alt {
            let expr = self.eval_expr(alt, alternate_scope, Some(consequent_type))?;
            self.transform_expr_to_block(expr, alternate_scope)
        } else {
            TypedBlock {
                expr_type: UNIT_TYPE_ID,
                scope_id: alternate_scope,
                statements: vec![TypedStmt::Expr(Box::new(TypedExpr::unit_literal(if_expr.span)))],
                span: if_expr.span,
            }
        };
        if let Err(msg) = self.typecheck_types(consequent.expr_type, alternate.expr_type, scope_id)
        {
            return make_fail(
                format!("else branch type did not match then branch type: {}", msg),
                alternate.span,
            );
        }
        let overall_type = consequent.expr_type;
        Ok(TypedExpr::If(Box::new(TypedIf {
            condition,
            consequent,
            alternate,
            ty: overall_type,
            span: if_expr.span,
        })))
    }

    fn get_namespace_scope_for_ident(
        &self,
        scope_id: ScopeId,
        identifier_id: IdentifierId,
    ) -> &Scope {
        let namespace_id = self.scopes.find_namespace(scope_id, identifier_id).unwrap();
        let namespace = self.get_namespace(namespace_id);
        let scope = self.scopes.get_scope(namespace.scope_id);
        scope
    }

    /// Can 'shortcircuit' with Left if the function call to resolve
    /// is actually a builtin
    fn resolve_function_call(
        &self,
        fn_call: &FnCall,
        this_expr: Option<&TypedExpr>,
        calling_scope: ScopeId,
    ) -> TyperResult<Either<TypedExpr, FunctionId>> {
        let function_id = match this_expr {
            Some(base_expr) => {
                // Resolve a method call
                let type_id = base_expr.get_type();
                let function_id = match self.get_type_dereferenced(type_id) {
                    Type::String => {
                        let string_ident_id = self.ast.ident_id("string");
                        let string_scope =
                            self.get_namespace_scope_for_ident(calling_scope, string_ident_id);
                        string_scope.find_function(fn_call.name)
                    }
                    Type::Char => {
                        let char_ident_id = self.ast.ident_id("char");
                        let char_scope =
                            self.get_namespace_scope_for_ident(calling_scope, char_ident_id);
                        char_scope.find_function(fn_call.name)
                    }
                    Type::Array(_array_type) => {
                        let array_ident_id = self.ast.ident_id("Array");
                        let array_scope =
                            self.get_namespace_scope_for_ident(calling_scope, array_ident_id);
                        array_scope.find_function(fn_call.name)
                    }
                    Type::Optional(_optional_type) => {
                        if fn_call.name == self.ast.ident_id("hasValue")
                            && fn_call.args.is_empty()
                            && fn_call.type_args.is_none()
                        {
                            return Ok(Either::Left(TypedExpr::OptionalHasValue(Box::new(
                                base_expr.clone(),
                            ))));
                        } else {
                            None
                        }
                    }
                    Type::Record(record) => {
                        // Need to distinguish between instances of 'named'
                        // records and anonymous ones
                        let Some(record_type_name) = record.name_if_named else {
                            return make_fail(
                                "Anonymous records currently have no methods",
                                record.span,
                            );
                        };
                        let record_scope =
                            self.get_namespace_scope_for_ident(calling_scope, record_type_name);
                        record_scope.find_function(fn_call.name)
                    }
                    _ => None,
                };
                match function_id {
                    Some(function_id) => function_id,
                    None => {
                        return make_fail(
                            format!(
                                "Method {} does not exist on type {:?}",
                                &*self.get_ident_str(fn_call.name),
                                self.type_id_to_string(type_id),
                            ),
                            fn_call.span,
                        );
                    }
                }
            }
            None => {
                // Resolve a non-method call
                let is_namespaced_call = !fn_call.namespaces.is_empty();
                let scope_to_search = self.traverse_namespace_chain(
                    calling_scope,
                    &fn_call.namespaces,
                    fn_call.span,
                )?;
                // Qualified lookups shouldn't recurse up the scopes; they should search only the references namespace
                let recursive = !is_namespaced_call;
                let function_id = self
                    .scopes
                    .find_function(scope_to_search, fn_call.name, recursive)
                    .ok_or(make_error(
                        format!(
                            "Function not found: {} in scope: {:?}",
                            &*self.get_ident_str(fn_call.name),
                            self.scopes.get_scope(calling_scope)
                        ),
                        fn_call.span,
                    ))?;
                function_id
            }
        };
        return Ok(Either::Right(function_id));
    }

    // skip_typecheck: I actually just want this to handle the 'self' order and zipping thing because I sometimes
    // don't want these checked just yet because I'm using them for inference
    fn typecheck_call_arguments(
        &mut self,
        fn_call: &FnCall,
        this_expr: Option<TypedExpr>,
        params: &Vec<FnArgDefn>,
        calling_scope: ScopeId,
        skip_typecheck: bool,
    ) -> TyperResult<Vec<TypedExpr>> {
        let mut final_args: Vec<TypedExpr> = Vec::new();
        // We have to deal with `self` arguments outside of the loop because
        // we can't 'move' out of this_expr more than once
        let mut skip_first = false;
        if let Some(first) = params.get(0) {
            let is_self = first.name == self.ast.ident_id("self");
            if is_self {
                if let Some(this) = this_expr {
                    if !skip_typecheck {
                        if let Err(e) =
                            self.typecheck_types(first.type_id, this.get_type(), calling_scope)
                        {
                            return make_fail(
                                format!(
                                    "Invalid parameter type for 'self' to function {}: {}",
                                    &*self.ast.get_ident_str(fn_call.name),
                                    e
                                ),
                                fn_call.span,
                            );
                        }
                    }
                    final_args.push(this);
                    skip_first = true;
                }
            }
        }
        let start: u32 = if skip_first { 1 } else { 0 };
        for fn_param in &params[start as usize..] {
            let matching_param_by_name =
                fn_call.args.iter().find(|arg| arg.name == Some(fn_param.name));
            // If we skipped 'self', we need to subtract 1 from the offset we index into fn_call.args with
            let matching_idx = fn_param.position - start;
            let matching_param = matching_param_by_name.or(fn_call.args.get(matching_idx as usize));
            if let Some(param) = matching_param {
                let expected_type_for_param =
                    if skip_typecheck { None } else { Some(fn_param.type_id) };
                let expr = self.eval_expr(param.value, calling_scope, expected_type_for_param)?;
                if !skip_typecheck {
                    if let Err(e) =
                        self.typecheck_types(fn_param.type_id, expr.get_type(), calling_scope)
                    {
                        return make_fail(
                            format!(
                                "Invalid parameter type passed to function {}: {}",
                                &*self.ast.get_ident_str(fn_call.name),
                                e
                            ),
                            expr.get_span(),
                        );
                    }
                }
                final_args.push(expr);
            } else {
                return make_fail(
                    format!(
                        "Missing argument to function {}: {}",
                        &*self.ast.get_ident_str(fn_call.name),
                        &*self.get_ident_str(fn_param.name)
                    ),
                    fn_call.span,
                );
            }
        }
        Ok(final_args)
    }

    fn eval_function_call(
        &mut self,
        fn_call: &FnCall,
        this_expr: Option<TypedExpr>,
        known_type_args: Option<Vec<TypeId>>,
        scope_id: ScopeId,
    ) -> TyperResult<TypedExpr> {
        // Special case for Some() because it is parsed as a function call
        // but should result in a special expression
        if fn_call.name == self.ast.ident_id("Some") {
            if fn_call.args.len() != 1 {
                return make_fail("Some() must have exactly one argument", fn_call.span);
            }
            let arg = self.eval_expr_inner(fn_call.args[0].value, scope_id, None)?;
            let type_id = arg.get_type();
            let optional_type = Type::Optional(OptionalType { inner_type: type_id });
            let type_id = self.add_type(optional_type);
            return Ok(TypedExpr::OptionalSome(OptionalSome {
                inner_expr: Box::new(arg),
                type_id,
            }));
        }

        let function_id = match self.resolve_function_call(fn_call, this_expr.as_ref(), scope_id)? {
            Either::Left(expr) => return Ok(expr),
            Either::Right(function_id) => function_id,
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let original_function = self.get_function(function_id);
        let original_params = original_function.params.clone();

        let (function_to_call, typechecked_arguments) = if let Some(type_params) =
            &original_function.type_params
        {
            let intrinsic_type = original_function.intrinsic_type;

            // We infer the type arguments, or evaluate them if the user has supplied them
            let type_args = match known_type_args {
                Some(ta) => {
                    // Need the ident
                    ta.into_iter()
                        .enumerate()
                        .map(|(idx, type_id)| TypeParam { ident: type_params[idx].ident, type_id })
                        .collect()
                }
                None => {
                    self.infer_call_type_args(fn_call, function_id, this_expr.as_ref(), scope_id)?
                }
            };

            // We skip specialization if any of the type arguments are type variables: `any_type_vars`
            // because we could just be evaluating a generic function that calls another generic function,
            // in which case we don't want to generate a 'specialized' function where we haven't
            // actually specialized everything
            let mut fully_concrete = true;
            for TypeParam { type_id, .. } in type_args.iter() {
                if self.is_type_generic(*type_id) {
                    fully_concrete = false
                }
            }
            if !fully_concrete {
                (
                    function_id,
                    self.typecheck_call_arguments(
                        fn_call,
                        this_expr,
                        &original_params,
                        scope_id,
                        false,
                    )?,
                )
            } else {
                self.get_specialized_function_for_call(
                    fn_call,
                    type_args,
                    function_id,
                    intrinsic_type,
                    this_expr,
                    scope_id,
                )?
            }
        } else {
            (
                function_id,
                self.typecheck_call_arguments(
                    fn_call,
                    this_expr,
                    &original_params,
                    scope_id,
                    false,
                )?,
            )
        };

        let function_ret_type = self.get_function(function_to_call).ret_type;
        let call = Call {
            callee_function_id: function_to_call,
            args: typechecked_arguments,
            ret_type: function_ret_type,
            span: fn_call.span,
        };
        Ok(TypedExpr::FunctionCall(call))
    }

    fn infer_call_type_args(
        &mut self,
        fn_call: &FnCall,
        generic_function_id: FunctionId,
        this_expr: Option<&TypedExpr>,
        calling_scope: ScopeId,
    ) -> TyperResult<Vec<TypeParam>> {
        let generic_function = self.get_function(generic_function_id);
        let generic_type_params = generic_function
            .type_params
            .as_ref()
            .cloned()
            .expect("expected function to be generic");
        let generic_name = generic_function.name;
        let generic_params = generic_function.params.clone();
        let type_params = match &fn_call.type_args {
            Some(type_args) => {
                if type_args.len() != generic_type_params.len() {
                    return make_fail(
                        format!(
                            "Expected {} type arguments but got {}",
                            generic_type_params.len(),
                            type_args.len()
                        ),
                        fn_call.span,
                    );
                }
                let mut checked_params = Vec::new();
                for (idx, type_arg) in type_args.iter().enumerate() {
                    let param = &generic_type_params[idx];
                    // TODO: This is where we'd apply constraints
                    let type_id = self.eval_type_expr(&type_arg.type_expr, calling_scope)?;
                    checked_params.push(TypeParam { ident: param.ident, type_id });
                }
                checked_params
            }
            None => {
                let exprs = self.typecheck_call_arguments(
                    fn_call,
                    this_expr.cloned(),
                    &generic_params,
                    calling_scope,
                    true,
                )?;

                let mut solved_params: Vec<TypeParam> = Vec::new();

                for (idx, expr) in exprs.iter().enumerate() {
                    let param = &generic_params[idx];

                    // This 'match' is where we would ideally implement some actual
                    // recursive type unification algorithm rather than hardcode 2 cases
                    let maybe_solved_param = match self.get_type(param.type_id) {
                        Type::TypeVariable(tv) => {
                            // If the type param is used in the type of the argument, we can infer
                            // the type param from the type of the argument
                            Some(TypeParam { ident: tv.identifier_id, type_id: expr.get_type() })
                        }
                        Type::Array(array_type) => {
                            // If the type param is used in the element type of an array, we can infer
                            // the type param from the type of the array
                            if let Type::TypeVariable(tv) = self.get_type(array_type.element_type) {
                                if let Type::Array(at) = self.get_type(expr.get_type()) {
                                    Some(TypeParam {
                                        ident: tv.identifier_id,
                                        type_id: at.element_type,
                                    })
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    if let Some(solved_param) = maybe_solved_param {
                        let existing_solution =
                            solved_params.iter().find(|p| p.ident == solved_param.ident);
                        if let Some(existing_solution) = existing_solution {
                            if let Err(msg) = self.typecheck_types(
                                existing_solution.type_id,
                                solved_param.type_id,
                                calling_scope,
                            ) {
                                return make_fail(
                                    format!(
                                        "Conflicting type parameters for type param {} in call to {}: {}",
                                        &*self.get_ident_str(solved_param.ident),
                                        &*self.get_ident_str(generic_name),
                                        msg
                                    ),
                                    fn_call.span,
                                );
                            }
                        }
                        solved_params.push(solved_param);
                    }
                }
                if solved_params.len() < generic_type_params.len() {
                    return make_fail(
                        format!(
                            "Could not infer all type parameters for function call to {}",
                            &*self.get_ident_str(generic_name)
                        ),
                        fn_call.span,
                    );
                } else {
                    debug!("Solved params: {:?}", solved_params);
                    solved_params
                }
            }
        };
        Ok(type_params)
    }

    fn get_specialized_function_for_call(
        &mut self,
        fn_call: &FnCall,
        inferred_or_passed_type_args: Vec<TypeParam>,
        generic_function_id: FunctionId,
        intrinsic_type: Option<IntrinsicFunctionType>,
        this_expr: Option<TypedExpr>,
        calling_scope: ScopeId,
    ) -> TyperResult<(FunctionId, Vec<TypedExpr>)> {
        let spec_fn_scope_id = self.scopes.add_scope_to_root(ScopeType::FunctionScope, None);
        let generic_function = self.get_function(generic_function_id);
        let generic_function_metadata = generic_function.metadata;
        let specializations = generic_function.specializations.clone();
        let name = String::from(&*self.get_ident_str(generic_function.name));
        // drop(generic_function);
        let mut new_name = name.clone();
        // Add type_args to scope and typecheck them against the actual params
        for type_param in inferred_or_passed_type_args.iter() {
            if let Type::TypeVariable(tv) = self.get_type(type_param.type_id) {
                return make_fail(
                    format!(
                        "Cannot specialize function with type variable: {}",
                        &*self.get_ident_str(tv.identifier_id)
                    ),
                    fn_call.span,
                );
            };
            debug!(
                "Adding type param {}: {} to scope for specialized function {}",
                &*self.get_ident_str(type_param.ident),
                self.type_id_to_string(type_param.type_id),
                name
            );
            self.scopes
                .get_scope_mut(spec_fn_scope_id)
                .add_type(type_param.ident, type_param.type_id);
        }
        let type_ids = inferred_or_passed_type_args
            .iter()
            .map(|type_param| type_param.type_id)
            .collect::<Vec<_>>();
        new_name.push_str("_");
        new_name.push_str(
            &type_ids.iter().map(|type_id| type_id.to_string()).collect::<Vec<_>>().join("_"),
        );

        self.scopes.get_scope_mut(spec_fn_scope_id).name = Some(self.ast.ident_id(&new_name));

        for (i, existing_specialization) in specializations.iter().enumerate() {
            // For now, naive comparison that all type ids are identical
            // There may be some scenarios where they are _equivalent_ but not identical
            // But I'm not sure
            let types_stringified = existing_specialization
                .specialized_type_params
                .iter()
                .map(|type_id| self.type_id_to_string(*type_id))
                .collect::<Vec<_>>()
                .join("_");
            debug!("existing specialization for {} {}: {}", name, i, types_stringified);
            if existing_specialization.specialized_type_params == type_ids {
                debug!(
                    "Found existing specialization for function {} with types: {}",
                    name, types_stringified
                );
                let exprs = self.typecheck_call_arguments(
                    fn_call,
                    this_expr,
                    &existing_specialization.specialized_params,
                    calling_scope,
                    false,
                )?;
                return Ok((existing_specialization.specialized_function_id, exprs));
            }
        }

        // TODO: new_name should really be calculated by specialize_function
        let generic_function_ast_id = generic_function_metadata.parsed_function_id();
        let specialized_function_id = self.specialize_function(
            generic_function_ast_id,
            SpecializationParams {
                fn_scope_id: spec_fn_scope_id,
                new_name: self.ast.ident_id(&new_name),
                known_intrinsic: intrinsic_type,
                generic_parent_function: generic_function_id,
                is_ability_impl: false,
            },
        )?;
        let specialized_params = self.get_function(specialized_function_id).params.clone();
        self.get_function_mut(generic_function_id).specializations.push(SpecializationRecord {
            specialized_function_id,
            specialized_type_params: type_ids,
            specialized_params: specialized_params.clone(),
        });

        let typechecked_exprs = self.typecheck_call_arguments(
            fn_call,
            this_expr,
            &specialized_params,
            calling_scope,
            false,
        )?;
        Ok((specialized_function_id, typechecked_exprs))
    }

    fn eval_block_stmt(
        &mut self,
        stmt: &BlockStmt,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedStmt> {
        match stmt {
            BlockStmt::ValDef(val_def) => {
                let provided_type = match val_def.type_id.as_ref() {
                    None => None,
                    Some(type_expr) => Some(self.eval_type_expr(type_expr, scope_id)?),
                };
                let value_expr = self.eval_expr(val_def.value, scope_id, provided_type)?;
                let actual_type = value_expr.get_type();
                let variable_type = if let Some(expected_type) = provided_type {
                    if let Err(msg) = self.typecheck_types(expected_type, actual_type, scope_id) {
                        return make_fail(
                            format!("Local variable type mismatch: {}", msg),
                            val_def.span,
                        );
                    }
                    expected_type
                } else {
                    actual_type
                };

                let variable_id = self.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name,
                    type_id: variable_type,
                    owner_scope: Some(scope_id),
                });
                let val_def_stmt = TypedStmt::ValDef(Box::new(ValDef {
                    ty: variable_type,
                    variable_id,
                    initializer: value_expr,
                    span: val_def.span,
                }));
                self.scopes.add_variable(scope_id, val_def.name, variable_id);
                Ok(val_def_stmt)
            }
            BlockStmt::Assignment(assignment) => {
                // let lhs = self.eval_expr(&assignment.lhs, scope_id, None)?;
                let lhs = self.eval_assigment_lhs_expr(assignment.lhs, scope_id, None)?;
                let rhs = self.eval_expr(assignment.rhs, scope_id, Some(lhs.get_type()))?;
                if let Err(msg) = self.typecheck_types(lhs.get_type(), rhs.get_type(), scope_id) {
                    return make_fail(
                        format!("Invalid types for assignment: {}", msg),
                        assignment.span,
                    );
                }
                let expr = TypedStmt::Assignment(Box::new(Assignment {
                    destination: Box::new(lhs),
                    value: Box::new(rhs),
                    span: assignment.span,
                }));
                Ok(expr)
            }
            BlockStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(*expression, scope_id, expected_type)?;
                Ok(TypedStmt::Expr(Box::new(expr)))
            }
            BlockStmt::While(while_stmt) => {
                let cond = self.eval_expr(while_stmt.cond, scope_id, Some(BOOL_TYPE_ID))?;
                if let Err(e) = self.typecheck_types(BOOL_TYPE_ID, cond.get_type(), scope_id) {
                    return make_fail(
                        format!("Invalid while condition type: {}", e),
                        cond.get_span(),
                    );
                }
                let block = self.eval_block(&while_stmt.block, scope_id, None)?;
                Ok(TypedStmt::WhileLoop(Box::new(TypedWhileLoop {
                    cond,
                    block,
                    span: while_stmt.span,
                })))
            }
        }
    }
    fn eval_block(
        &mut self,
        block: &Block,
        scope_id: ScopeId,
        expected_type: Option<TypeId>,
    ) -> TyperResult<TypedBlock> {
        let mut statements = Vec::with_capacity(block.stmts.len());
        let mut expr_type_of_block: TypeId = UNIT_TYPE_ID;
        for (index, stmt) in block.stmts.iter().enumerate() {
            let is_last = index == block.stmts.len() - 1;
            let expected_type = if is_last { expected_type } else { None };
            let stmt = self.eval_block_stmt(stmt, scope_id, expected_type)?;
            if is_last {
                expr_type_of_block = self.get_stmt_expression_type(&stmt);
            }
            statements.push(stmt);
        }

        let typed_block =
            TypedBlock { expr_type: expr_type_of_block, scope_id, statements, span: block.span };
        Ok(typed_block)
    }

    fn _get_scope_for_namespace(&self, namespace_ident: IdentifierId) -> ScopeId {
        self.namespaces.iter().find(|ns| ns.name == namespace_ident).unwrap().scope_id
    }

    fn resolve_intrinsic_function_type(
        &self,
        fn_def: &ParsedFunction,
        scope_id: ScopeId,
    ) -> Result<IntrinsicFunctionType, String> {
        trace!("resolve_intrinsic_function_type for {}", &*self.get_ident_str(fn_def.name));
        let result = if let Some(current_namespace) =
            self.namespaces.iter().find(|ns| ns.scope_id == scope_id)
        {
            if current_namespace.name == self.ast.ident_id("string") {
                if fn_def.name == self.ast.ident_id("length") {
                    Some(IntrinsicFunctionType::StringLength)
                } else if fn_def.name == self.ast.ident_id("fromChars") {
                    Some(IntrinsicFunctionType::StringFromCharArray)
                } else if fn_def.name == self.ast.ident_id("equals") {
                    Some(IntrinsicFunctionType::StringEquals)
                } else {
                    None
                }
            } else if current_namespace.name == self.ast.ident_id("Array") {
                if fn_def.name == self.ast.ident_id("length") {
                    Some(IntrinsicFunctionType::ArrayLength)
                } else if fn_def.name == self.ast.ident_id("capacity") {
                    Some(IntrinsicFunctionType::ArrayCapacity)
                } else if fn_def.name == self.ast.ident_id("grow") {
                    Some(IntrinsicFunctionType::ArrayGrow)
                } else if fn_def.name == self.ast.ident_id("new") {
                    Some(IntrinsicFunctionType::ArrayNew)
                } else if fn_def.name == self.ast.ident_id("set_length") {
                    Some(IntrinsicFunctionType::ArraySetLength)
                } else {
                    None
                }
            } else if current_namespace.name == self.ast.ident_id("char") {
                // Future Char intrinsics
                None
            } else if current_namespace.name == self.ast.ident_id("_root") {
                let function_name = &*self.get_ident_str(fn_def.name);
                IntrinsicFunctionType::from_function_name(function_name)
            } else {
                None
            }
        } else if let Some(ability) = self.abilities.iter().find(|ab| ab.scope_id == scope_id) {
            if ability.name == self.ast.ident_id("Equals") {
                if fn_def.args.first().unwrap().ty.is_string() {
                    Some(IntrinsicFunctionType::StringEquals)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            panic!(
                "Functions must be defined within a namespace or ability scope: {:?}",
                &*self.get_ident_str(fn_def.name)
            )
        };
        match result {
            Some(result) => Ok(result),
            None => {
                return Err(format!(
                    "Could not resolve intrinsic function type for function {}",
                    &*self.get_ident_str(fn_def.name),
                ))
            }
        }
    }

    fn eval_function_predecl(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        // Note: messy: parent_scope_id is only used if not specializing
        parent_scope_id: ScopeId,
        specialization_params: Option<SpecializationParams>,
        is_ability_decl: bool,
    ) -> TyperResult<FunctionId> {
        let specialize = specialization_params.is_some();
        let ast = self.ast.clone();
        let parsed_function = ast.get_function(parsed_function_id);
        let name = specialization_params
            .as_ref()
            .map(|params| params.new_name)
            .unwrap_or(parsed_function.name);
        let fn_scope_id = match specialization_params.as_ref().map(|params| params.fn_scope_id) {
            None => {
                self.scopes.add_child_scope(parent_scope_id, ScopeType::FunctionScope, Some(name))
            }
            Some(fn_scope_id) => fn_scope_id,
        };
        // Some madness to get the actual enclosing scope
        let parent_scope_id =
            self.scopes.get_scope(fn_scope_id).parent.unwrap_or(self.scopes.get_root_scope_id());

        // Instantiate type arguments
        let is_generic = !specialize
            && parsed_function.type_args.as_ref().map(|args| !args.is_empty()).unwrap_or(false);
        trace!(
            "eval_function {} is_generic: {} in scope: {}",
            &*self.get_ident_str(parsed_function.name),
            is_generic,
            parent_scope_id
        );
        let mut type_params: Option<Vec<TypeParam>> = None;
        if is_generic {
            let mut the_type_params = Vec::new();
            for type_parameter in parsed_function.type_args.as_ref().unwrap().iter() {
                let type_variable =
                    TypeVariable { identifier_id: type_parameter.ident, _constraints: None };
                let type_variable_id = self.add_type(Type::TypeVariable(type_variable));
                let fn_scope = self.scopes.get_scope_mut(fn_scope_id);
                let type_param =
                    TypeParam { ident: type_parameter.ident, type_id: type_variable_id };
                the_type_params.push(type_param);
                fn_scope.add_type(type_parameter.ident, type_variable_id)
            }
            type_params = Some(the_type_params);
            trace!(
                "Added type arguments to function {} scope {:?}",
                &*self.get_ident_str(parsed_function.name),
                self.scopes.get_scope(fn_scope_id)
            );
        }

        // Typecheck arguments
        let mut params = Vec::new();
        for (idx, fn_arg) in parsed_function.args.iter().enumerate() {
            let type_id = self.eval_type_expr(&fn_arg.ty, fn_scope_id)?;
            if specialize {
                trace!(
                    "Specializing argument: {} got {}",
                    &*self.get_ident_str(fn_arg.name),
                    self.type_id_to_string(type_id)
                );
            }
            let variable = Variable {
                name: fn_arg.name,
                type_id,
                is_mutable: false,
                owner_scope: Some(fn_scope_id),
            };
            let variable_id = self.add_variable(variable);
            params.push(FnArgDefn {
                name: fn_arg.name,
                variable_id,
                position: idx as u32,
                type_id,
                span: fn_arg.span,
            });
            self.scopes.add_variable(fn_scope_id, fn_arg.name, variable_id);
        }

        let intrinsic_type = if let Some(known_intrinsic) =
            specialization_params.as_ref().and_then(|params| params.known_intrinsic)
        {
            Some(known_intrinsic)
        } else if parsed_function.linkage == Linkage::Intrinsic {
            let resolved = self
                .resolve_intrinsic_function_type(parsed_function, parent_scope_id)
                .map_err(|msg| {
                make_error(format!("Error typechecking function: {}", msg), parsed_function.span)
            })?;
            Some(resolved)
        } else {
            None
        };
        let given_ret_type = match &parsed_function.ret_type {
            None => UNIT_TYPE_ID,
            Some(type_expr) => self.eval_type_expr(type_expr, fn_scope_id)?,
        };
        let metadata = if is_ability_decl {
            TypedFunctionMetadata::AbilityDefn(parsed_function_id)
        } else if let Some(spec_params) = specialization_params {
            if spec_params.is_ability_impl {
                TypedFunctionMetadata::AbilityImpl {
                    generic_defn: spec_params.generic_parent_function,
                    impl_parsed_function_id: parsed_function_id,
                }
            } else {
                TypedFunctionMetadata::Specialization {
                    generic_parsed_function_id: parsed_function_id,
                    generic_defn: parsed_function_id,
                }
            }
        } else {
            TypedFunctionMetadata::Standard(parsed_function_id)
        };
        let function = TypedFunction {
            name,
            scope: fn_scope_id,
            ret_type: given_ret_type,
            params,
            type_params,
            block: None,
            intrinsic_type,
            linkage: parsed_function.linkage,
            specializations: Vec::new(),
            metadata,
            span: parsed_function.span,
        };
        let function_id = self.add_function(function);

        // We do not want to resolve specialized functions by name!
        // So don't add them to any scope.
        // They all have the same name but different types!!!
        if !specialize {
            self.scopes.add_function(parent_scope_id, parsed_function.name, function_id);

            trace!(
                "Inserting AST definition mapping for id {} {}, specialize={specialize}",
                parsed_function.id,
                &*self.get_ident_str(parsed_function.name)
            );
        }

        // Only if this isn't a specialization, add the ast mapping
        if let TypedFunctionMetadata::Specialization { .. } = metadata {
        } else {
            self.function_ast_mappings.insert(metadata.parsed_function_id(), function_id);
        }

        Ok(function_id)
    }

    fn eval_function_body(&mut self, declaration_id: FunctionId) -> TyperResult<()> {
        let function = self.get_function(declaration_id);
        let fn_scope_id = function.scope;
        let given_ret_type = function.ret_type;
        let is_extern = function.linkage == Linkage::External;
        let ast_id = function.metadata.parsed_function_id();
        let is_intrinsic = function.intrinsic_type.is_some();

        let ast = self.ast.clone();
        let ast_fn_def = ast.get_function(ast_id);

        let body_block = match &ast_fn_def.block {
            Some(block_ast) => {
                let block = self.eval_block(block_ast, fn_scope_id, Some(given_ret_type))?;
                if let Err(msg) = self.typecheck_types(given_ret_type, block.expr_type, fn_scope_id)
                {
                    return make_fail(
                        format!(
                            "Function {} return type mismatch: {}",
                            &*self.get_ident_str(ast_fn_def.name),
                            msg
                        ),
                        ast_fn_def.span,
                    );
                } else {
                    Some(block)
                }
            }
            None if is_intrinsic || is_extern => None,
            None => return make_fail("function is missing implementation", ast_fn_def.span),
        };
        // Add the body now
        if let Some(body_block) = body_block {
            self.get_function_mut(declaration_id).block = Some(body_block);
        }
        Ok(())
    }

    fn specialize_function(
        &mut self,
        parsed_function_id: ParsedFunctionId,
        specialization_params: SpecializationParams,
    ) -> TyperResult<FunctionId> {
        let specialized_function_id = self.eval_function_predecl(
            parsed_function_id,
            // This scope is unused when specializing, so we just pass the root
            self.scopes.get_root_scope_id(),
            Some(specialization_params),
            false,
        )?;
        self.eval_function_body(specialized_function_id)?;
        Ok(specialized_function_id)
    }

    fn eval_namespace_decl(
        &mut self,
        parsed_namespace_id: ParsedNamespaceId,
        scope_id: ScopeId,
    ) -> TyperResult<NamespaceId> {
        let ast = self.ast.clone();
        let ast_namespace = ast.get_namespace(parsed_namespace_id);
        let ns_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::Namespace, Some(ast_namespace.name));
        let namespace = Namespace { name: ast_namespace.name, scope_id: ns_scope_id };
        let namespace_id = self.add_namespace(namespace);

        // We add the new namespace's scope as a child of the current scope
        let scope = self.scopes.get_scope_mut(scope_id);
        scope.add_namespace(ast_namespace.name, namespace_id);

        self.namespace_ast_mappings.insert(ast_namespace.id, namespace_id);

        for defn in &ast_namespace.definitions {
            self.eval_definition_declaration_phase(*defn, ns_scope_id)?;
        }
        Ok(namespace_id)
    }

    fn eval_namespace(&mut self, ast_namespace_id: ParsedNamespaceId) -> TyperResult<NamespaceId> {
        let ast = self.ast.clone();
        let ast_namespace = ast.get_namespace(ast_namespace_id);
        let namespace_id = *self.namespace_ast_mappings.get(&ast_namespace.id).unwrap();
        let ns_scope_id = self.get_namespace(namespace_id).scope_id;
        for defn in &ast_namespace.definitions {
            self.eval_definition(*defn, ns_scope_id)?;
        }
        Ok(namespace_id)
    }

    fn eval_definition_declaration_phase(
        &mut self,
        defn_id: ParsedDefinitionId,
        scope_id: ScopeId,
    ) -> TyperResult<()> {
        match defn_id {
            ParsedDefinitionId::Namespace(namespace_id) => {
                self.eval_namespace_decl(namespace_id, scope_id)?;
                Ok(())
            }
            ParsedDefinitionId::Constant(constant_id) => {
                let _variable_id: VariableId = self.eval_const(constant_id)?;
                Ok(())
            }
            ParsedDefinitionId::Function(parsed_function_id) => {
                self.eval_function_predecl(parsed_function_id, scope_id, None, false)?;
                Ok(())
            }
            ParsedDefinitionId::TypeDefn(type_defn_id) => {
                // We can just do type defs in the declaration phase, so we skip them in the 2nd pass
                self.eval_type_defn(type_defn_id, scope_id)?;
                Ok(())
            }
            ParsedDefinitionId::Ability(parsed_ability_id) => {
                // Typechecking an ability definition means declaring the functions, registering the ability,
                // and the fact that it contains those functions. Also making sure they are valid? Do they have to take
                // at least one Self arg?
                self.eval_ability_defn(parsed_ability_id, scope_id)?;
                Ok(())
            }
            ParsedDefinitionId::AbilityImpl(_ability_impl) => {
                // Nothing to do in this phase for impls
                Ok(())
            }
        }
    }

    fn eval_ability_defn(
        &mut self,
        parsed_ability_id: ParsedAbilityId,
        scope_id: ScopeId,
    ) -> TyperResult<()> {
        let ast = self.ast.clone();
        let parsed_ability = ast.get_ability(parsed_ability_id);
        let ability_scope_id =
            self.scopes.add_child_scope(scope_id, ScopeType::Namespace, Some(parsed_ability.name));
        // Open up the scope for the ability, and add type variable "Self" into scope
        let self_ident_id = self.ast.ident_id("Self");
        let self_type_id = self.add_type(Type::TypeVariable(TypeVariable {
            identifier_id: self_ident_id,
            _constraints: None,
        }));
        self.scopes.get_scope_mut(ability_scope_id).add_type(self_ident_id, self_type_id);
        let mut typed_functions: Vec<TypedAbilityFunctionRef> =
            Vec::with_capacity(parsed_ability.functions.len());
        for parsed_function_id in parsed_ability.functions.iter() {
            let function_id =
                self.eval_function_predecl(*parsed_function_id, ability_scope_id, None, true)?;
            let function_name = self.ast.get_function(*parsed_function_id).name;
            typed_functions.push(TypedAbilityFunctionRef { function_name, function_id });
        }
        let typed_ability = TypedAbility {
            name: parsed_ability.name,
            functions: typed_functions,
            scope_id: ability_scope_id,
            ast_id: parsed_ability.id,
        };
        let ability_id = self.add_ability(typed_ability);
        self.scopes
            .get_scope_mut(self.scopes.get_root_scope_id())
            .add_ability(parsed_ability.name, ability_id);
        Ok(())
    }

    fn eval_ability_impl(
        &mut self,
        parsed_ability_impl_id: ParsedAbilityImplId,
        scope_id: ScopeId,
    ) -> TyperResult<()> {
        let ast = self.ast.clone();
        let parsed_ability_implementation = ast.get_ability_impl(parsed_ability_impl_id);
        let ability_name = parsed_ability_implementation.ability_name;
        let Some(ability_id) = self.scopes.get_root_scope().find_ability(ability_name) else {
            return make_fail(
                format!("Ability does not exist: {}", &*self.get_ident_str(ability_name)),
                parsed_ability_implementation.span,
            );
        };
        let target_type =
            self.eval_type_expr(&parsed_ability_implementation.target_type, scope_id)?;

        // Scoping / orphan / coherence: For now, let's globally allow only one implementation per (Ability, Target Type) pair
        // Check for existing implementation
        for existing_impl in &self.implementations {
            if existing_impl.ability_id == ability_id && existing_impl.type_id == target_type {
                return make_fail(
                    format!(
                        "Ability '{}' already implemented for type: {}",
                        &*self.get_ident_str(ability_name).blue(),
                        self.type_id_to_string(target_type).blue()
                    ),
                    parsed_ability_implementation.span,
                );
            }
        }

        let ability = self.get_ability(ability_id);
        let ability_scope_id = ability.scope_id;
        let mut typed_functions = Vec::new();
        // Note(clone): TypedAbilityFunctionRef is super cheap to clone
        for ability_function_ref in &ability.functions.clone() {
            let Some(impl_parsed_fn) =
                parsed_ability_implementation.functions.iter().find_map(|&fn_id| {
                    let the_fn = ast.get_function(fn_id);
                    if the_fn.name == ability_function_ref.function_name {
                        Some(the_fn)
                    } else {
                        None
                    }
                })
            else {
                return make_fail(
                    format!(
                        "Missing implementation for function '{}' in ability '{}'",
                        &*self.get_ident_str(ability_function_ref.function_name).blue(),
                        &*self.get_ident_str(ability_name).blue()
                    ),
                    parsed_ability_implementation.span,
                );
            };
            // Make a scope for the impl function
            let new_name =
                format!("{}_impl_{}", &*self.ast.get_ident_str(impl_parsed_fn.name), target_type);
            let new_name_ident = self.ast.ident_id(&new_name);
            let spec_fn_scope_id = self.scopes.add_child_scope(
                ability_scope_id,
                ScopeType::FunctionScope,
                Some(new_name_ident),
            );
            // Bind 'Self' = target_type
            self.scopes
                .get_scope_mut(spec_fn_scope_id)
                .add_type(self.ast.ident_id("Self"), target_type);

            let function_impl = self.specialize_function(
                impl_parsed_fn.id,
                SpecializationParams {
                    fn_scope_id: spec_fn_scope_id,
                    new_name: impl_parsed_fn.name,
                    known_intrinsic: None,
                    generic_parent_function: ability_function_ref.function_id,
                    is_ability_impl: true,
                },
            )?;

            let specialized = self.get_function(function_impl);
            let generic = self.get_function(ability_function_ref.function_id);
            if specialized.params.len() != generic.params.len() {
                return make_fail(
                    format!(
                        "Invalid implementation of {} in ability {}: wrong number of parameters",
                        &*self.ast.get_ident_str(ability_function_ref.function_name),
                        &*self.ast.get_ident_str(ability_name)
                    ),
                    impl_parsed_fn.span,
                );
            }
            for (index, specialized_param) in specialized.params.iter().enumerate() {
                let generic_param = &generic.params[index];
                if let Err(msg) = self.typecheck_types(
                    generic_param.type_id,
                    specialized_param.type_id,
                    spec_fn_scope_id,
                ) {
                    return make_fail(
                        format!(
                            "Invalid implementation of {} in ability {} for parameter {}: {}",
                            &*self.ast.get_ident_str(ability_function_ref.function_name),
                            &*self.ast.get_ident_str(ability_name),
                            &*self.ast.get_ident_str(generic_param.name),
                            msg
                        ),
                        impl_parsed_fn.span,
                    );
                }
            }
            if let Err(msg) =
                self.typecheck_types(generic.ret_type, specialized.ret_type, spec_fn_scope_id)
            {
                return make_fail(
                    format!(
                        "Invalid implementation of '{}' in ability '{}': Wrong return type: {}",
                        &*self.ast.get_ident_str(ability_function_ref.function_name),
                        &*self.ast.get_ident_str(ability_name),
                        msg
                    ),
                    impl_parsed_fn.span,
                );
            }

            typed_functions.push(function_impl);
        }

        let ability_implementation =
            AbilityImplementation { type_id: target_type, ability_id, functions: typed_functions };
        self.implementations.push(ability_implementation);
        Ok(())
    }

    fn eval_definition(&mut self, def: ParsedDefinitionId, scope_id: ScopeId) -> TyperResult<()> {
        match def {
            ParsedDefinitionId::Namespace(namespace) => {
                self.eval_namespace(namespace)?;
                Ok(())
            }
            ParsedDefinitionId::Constant(_const_val) => {
                // Nothing to do in this phase for a const
                Ok(())
            }
            ParsedDefinitionId::Function(parsed_function_id) => {
                let function_declaration_id = self
                    .function_ast_mappings
                    .get(&parsed_function_id)
                    .expect("function predecl lookup failed");
                self.eval_function_body(*function_declaration_id)?;
                Ok(())
            }
            ParsedDefinitionId::TypeDefn(_type_defn) => {
                // Nothing to do in this phase for a type definition
                Ok(())
            }
            ParsedDefinitionId::Ability(_ability) => {
                // Nothing to do in this phase for an ability
                Ok(())
            }
            ParsedDefinitionId::AbilityImpl(ability_impl) => {
                self.eval_ability_impl(ability_impl, scope_id)?;
                Ok(())
            }
        }
    }
    pub fn run(&mut self) -> anyhow::Result<()> {
        let mut errors: Vec<TyperError> = Vec::new();

        let scope_id = self.scopes.get_root_scope_id();

        for &parsed_definition_id in self.ast.clone().get_root_namespace().definitions.iter() {
            let result = self.eval_definition_declaration_phase(parsed_definition_id, scope_id);
            if let Err(e) = result {
                self.print_error(&e.message, e.span);
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            bail!("{} failed declaration phase with {} errors", self.name(), errors.len())
        }

        for &parsed_definition_id in self.ast.clone().get_root_namespace().definitions.iter() {
            let result = self.eval_definition(parsed_definition_id, scope_id);
            if let Err(e) = result {
                self.print_error(&e.message, e.span);
                errors.push(e);
            }
        }
        if !errors.is_empty() {
            if log::log_enabled!(Level::Debug) {
                debug!("{}", self);
            }
            bail!("{} failed typechecking with {} errors", self.name(), errors.len())
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_module, ParseResult, Source};
    use crate::typer::*;

    fn setup(src: &str, test_name: &str) -> ParseResult<ParsedModule> {
        parse_module(Rc::new(Source::make(
            0,
            "unit_test".to_string(),
            test_name.to_string(),
            src.to_string(),
        )))
    }

    #[test]
    fn const_definition_1() -> anyhow::Result<()> {
        let src = r"val x: int = 420;";
        let module = setup(src, "const_definition_1.nx")?;
        let mut ir = TypedModule::new(Rc::new(module));
        ir.run()?;
        let i1 = &ir.constants[0];
        if let TypedExpr::Int(i, span) = i1.expr {
            assert_eq!(i, 420);
            assert_eq!(span.end, 16);
            assert_eq!(span.start, 0);
            Ok(())
        } else {
            panic!("{i1:?} was not an int")
        }
    }

    #[test]
    fn fn_definition_1() -> anyhow::Result<()> {
        let src = r#"
        fn foo(): int {
          1
        }
        fn basic(x: int, y: int): int {
          val x: int = 0; mut y: int = 1;
          y = { 1; 2; 3 };
          y = 42 + 42;
          foo()
        }"#;
        let module = setup(src, "basic_fn.nx")?;
        let mut ir = TypedModule::new(Rc::new(module));
        ir.run()?;
        println!("{:?}", ir.functions);
        Ok(())
    }
}
