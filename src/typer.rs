#![allow(clippy::match_like_matches_macro)]

use crate::lex::Span;
use crate::parse::{self, ParsedNamespace};
use crate::parse::{
    AstId, AstModule, Block, BlockStmt, Definition, Expression, FnCall, FnDef, IdentifierId,
    Literal,
};
use anyhow::{bail, Result};
use colored::Colorize;
use log::{error, trace};
use parse_display::Display;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type ScopeId = u32;
pub type FunctionId = u32;
pub type VariableId = u32;
pub type TypeId = u32;
pub type NamespaceId = u32;

#[derive(Debug, Clone)]
pub struct RecordDefnField {
    pub name: IdentifierId,
    pub ty: TypeRef,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct RecordDefn {
    pub fields: Vec<RecordDefnField>,
    pub name_if_named: Option<IdentifierId>,
    pub span: Span,
}

impl RecordDefn {
    pub fn find_field(&self, field_name: IdentifierId) -> Option<(usize, &RecordDefnField)> {
        self.fields.iter().enumerate().find(|(_, field)| field.name == field_name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Unit,
    Char,
    Int,
    Bool,
    String,
    TypeId(TypeId),
}

impl TypeRef {
    pub fn as_type_id(&self) -> Option<TypeId> {
        match self {
            TypeRef::TypeId(type_id) => Some(*type_id),
            _ => None,
        }
    }
    pub fn expect_type_id(&self) -> TypeId {
        match self {
            TypeRef::TypeId(type_id) => *type_id,
            _ => panic!("Expected TypeId on {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeExpression {
    pub ty: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    identifier_id: IdentifierId,
    /// This is where trait bounds would go
    constraints: Option<Vec<()>>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Record(RecordDefn),
    Array(ArrayType),
    OpaqueAlias(TypeRef),
    TypeVariable(TypeVariable),
}

impl Type {
    pub fn expect_array_type(&self) -> &ArrayType {
        match self {
            Type::Array(array) => array,
            _ => panic!("expect_array called on: {:?}", self),
        }
    }
    pub fn expect_record_type(&self) -> &RecordDefn {
        match self {
            Type::Record(record) => record,
            _ => panic!("expect_record called on: {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    // If this block is just an expression, the type of the expression
    pub expr_type: TypeRef,
    pub scope_id: ScopeId,
    pub statements: Vec<TypedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnArgDefn {
    pub name: IdentifierId,
    pub variable_id: VariableId,
    pub position: usize,
    pub ty: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: IdentifierId,
    pub scope: ScopeId,
    pub ret_type: TypeRef,
    pub params: Vec<FnArgDefn>,
    pub type_params: Option<Vec<TypeParam>>,
    pub block: Option<TypedBlock>,
    pub intrinsic_type: Option<IntrinsicFunctionType>,
    pub specializations: Vec<FunctionId>,
    pub ast_id: AstId,
}

impl Function {
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
    pub ir_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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
}

impl BinaryOpKind {
    pub fn is_integer_op(&self) -> bool {
        use BinaryOpKind as B;
        match self {
            B::Add | B::Subtract => true,
            B::Multiply | B::Divide => true,
            B::Less | B::Greater | B::LessEqual | B::GreaterEqual => true,
            B::Or | B::And => true,
            B::Equals => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub ir_type: TypeRef,
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee_function_id: FunctionId,
    pub args: Vec<TypedExpr>,
    pub ret_type: TypeRef,
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
pub enum TypedLiteral {
    Unit(Span),
    Char(u8, Span),
    Bool(bool, Span),
    Int(i64, Span),
    Str(String, Span),
    Record(Record),
    Array(ArrayLiteral),
}

impl TypedLiteral {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypedLiteral::Unit(span) => *span,
            TypedLiteral::Char(_, span) => *span,
            TypedLiteral::Str(_, span) => *span,
            TypedLiteral::Int(_, span) => *span,
            TypedLiteral::Bool(_, span) => *span,
            TypedLiteral::Record(record) => record.span,
            TypedLiteral::Array(arr) => arr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: TypedExpr,
    pub consequent: TypedBlock,
    pub alternate: TypedBlock,
    pub ir_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: Box<TypedExpr>,
    pub target_field: IdentifierId,
    pub ir_type: TypeRef,
    pub span: Span,
}
#[derive(Debug, Clone)]
pub struct IndexOp {
    pub base_expr: Box<TypedExpr>,
    pub index_expr: Box<TypedExpr>,
    pub result_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Literal(TypedLiteral),
    Variable(VariableExpr),
    FieldAccess(FieldAccess),
    BinaryOp(BinaryOp),
    Block(TypedBlock),
    FunctionCall(Call),
    If(Box<TypedIf>),
    ArrayIndex(IndexOp),
    StringIndex(IndexOp),
}

impl TypedExpr {
    pub fn unit_literal(span: Span) -> TypedExpr {
        TypedExpr::Literal(TypedLiteral::Unit(span))
    }

    #[inline]
    pub fn get_type(&self) -> TypeRef {
        match self {
            TypedExpr::Literal(TypedLiteral::Unit(_)) => TypeRef::Unit,
            TypedExpr::Literal(TypedLiteral::Char(_, _)) => TypeRef::Char,
            TypedExpr::Literal(TypedLiteral::Str(_, _)) => TypeRef::String,
            TypedExpr::Literal(TypedLiteral::Int(_, _)) => TypeRef::Int,
            TypedExpr::Literal(TypedLiteral::Bool(_, _)) => TypeRef::Bool,
            TypedExpr::Literal(TypedLiteral::Record(record)) => TypeRef::TypeId(record.type_id),
            TypedExpr::Literal(TypedLiteral::Array(arr)) => TypeRef::TypeId(arr.type_id),
            TypedExpr::Variable(var) => var.ir_type,
            TypedExpr::FieldAccess(field_access) => field_access.ir_type,
            TypedExpr::BinaryOp(binary_op) => binary_op.ir_type,
            TypedExpr::Block(b) => b.expr_type,
            TypedExpr::FunctionCall(call) => call.ret_type,
            TypedExpr::If(ir_if) => ir_if.ir_type,
            TypedExpr::ArrayIndex(op) => op.result_type,
            TypedExpr::StringIndex(op) => op.result_type,
        }
    }
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypedExpr::Literal(lit) => lit.get_span(),
            TypedExpr::Variable(var) => var.span,
            TypedExpr::FieldAccess(field_access) => field_access.span,
            TypedExpr::BinaryOp(binary_op) => binary_op.span,
            TypedExpr::Block(b) => b.span,
            TypedExpr::FunctionCall(call) => call.span,
            TypedExpr::If(ir_if) => ir_if.span,
            TypedExpr::ArrayIndex(op) => op.span,
            TypedExpr::StringIndex(op) => op.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValDef {
    pub variable_id: VariableId,
    pub ir_type: TypeRef,
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

// TODO: When do we 'clone' a whole TypedStmt?
#[derive(Debug, Clone)]
pub enum TypedStmt {
    Expr(Box<TypedExpr>),
    ValDef(Box<ValDef>),
    ReturnStmt(Box<ReturnStmt>),
    Assignment(Box<Assignment>),
    WhileLoop(Box<TypedWhileLoop>),
}

impl TypedStmt {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            TypedStmt::Expr(e) => e.get_span(),
            TypedStmt::ValDef(v) => v.span,
            TypedStmt::ReturnStmt(ret) => ret.span,
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

pub type TypedGenResult<A> = Result<A, TyperError>;

#[derive(Debug)]
pub struct Variable {
    pub name: IdentifierId,
    pub ir_type: TypeRef,
    pub is_mutable: bool,
    pub owner_scope: Option<ScopeId>,
}

#[derive(Debug)]
pub struct Constant {
    pub variable_id: VariableId,
    pub expr: TypedExpr,
    pub ir_type: TypeRef,
    pub span: Span,
}

pub struct Namespace {
    pub name: IdentifierId,
    pub scope_id: ScopeId,
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    fn make() -> Self {
        let scopes = vec![Scope::default()];
        Scopes { scopes }
    }
    fn get_root_scope_id(&self) -> ScopeId {
        0 as ScopeId
    }
    fn add_scope_to_root(&mut self) -> ScopeId {
        self.add_scope(0)
    }
    fn add_scope(&mut self, parent_scope_id: ScopeId) -> ScopeId {
        let scope = Scope { parent: Some(parent_scope_id), ..Scope::default() };
        let id = self.scopes.len() as ScopeId;
        self.scopes.push(scope);
        let parent_scope = self.get_scope_mut(parent_scope_id);
        parent_scope.children.push(id);
        id
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id as usize]
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id as usize]
    }

    fn find_namespace(&self, scope: ScopeId, ident: IdentifierId) -> Option<NamespaceId> {
        let scope = self.get_scope(scope);
        if let ns @ Some(_r) = scope.find_namespace(ident) {
            return ns;
        }
        match scope.parent {
            Some(parent) => self.find_namespace(parent, ident),
            None => None,
        }
    }

    fn find_variable(&self, scope: ScopeId, ident: IdentifierId) -> Option<VariableId> {
        let scope = self.get_scope(scope);
        if let v @ Some(_r) = scope.find_variable(ident) {
            return v;
        }
        match scope.parent {
            Some(parent) => self.find_variable(parent, ident),
            None => None,
        }
    }

    fn add_variable(&mut self, scope_id: ScopeId, ident: IdentifierId, variable_id: VariableId) {
        let scope = self.get_scope_mut(scope_id);
        scope.add_variable(ident, variable_id);
    }

    fn find_function(&self, scope: ScopeId, ident: IdentifierId) -> Option<FunctionId> {
        let scope = self.get_scope(scope);
        if let f @ Some(_r) = scope.find_function(ident) {
            return f;
        }
        match scope.parent {
            Some(parent) => self.find_function(parent, ident),
            None => None,
        }
    }

    fn add_function(
        &mut self,
        scope_id: ScopeId,
        identifier: IdentifierId,
        function_id: FunctionId,
    ) {
        self.get_scope_mut(scope_id).add_function(identifier, function_id)
    }

    fn add_type(&mut self, scope_id: ScopeId, ident: IdentifierId, ty: TypeRef) {
        self.get_scope_mut(scope_id).add_type(ident, ty)
    }

    fn find_type(&self, scope_id: ScopeId, ident: IdentifierId) -> Option<TypeRef> {
        let scope = self.get_scope(scope_id);
        if let v @ Some(_r) = scope.find_type(ident) {
            return v;
        }
        match scope.parent {
            Some(parent) => self.find_type(parent, ident),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicFunctionType {
    Exit,
    PrintInt,
    PrintString,
    StringLength,
    ArrayLength,
    ArrayNew,
    CharToString,
    StringNew,
}

impl IntrinsicFunctionType {
    pub fn from_function_name(value: &str) -> Option<Self> {
        match value {
            "printInt" => Some(IntrinsicFunctionType::PrintInt),
            "print" => Some(IntrinsicFunctionType::PrintString),
            "exit" => Some(IntrinsicFunctionType::Exit),
            "array_new" => Some(IntrinsicFunctionType::ArrayNew),
            "string_new" => Some(IntrinsicFunctionType::StringNew),
            _ => None,
        }
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    variables: HashMap<IdentifierId, VariableId>,
    functions: HashMap<IdentifierId, FunctionId>,
    namespaces: HashMap<IdentifierId, NamespaceId>,
    types: HashMap<IdentifierId, TypeRef>,
    parent: Option<ScopeId>,
    children: Vec<ScopeId>,
}
impl Scope {
    fn find_variable(&self, ident: IdentifierId) -> Option<VariableId> {
        self.variables.get(&ident).copied()
    }
    fn add_variable(&mut self, ident: IdentifierId, value: VariableId) {
        self.variables.insert(ident, value);
    }

    fn add_type(&mut self, ident: IdentifierId, ty: TypeRef) {
        self.types.insert(ident, ty);
    }

    fn find_type(&self, ident: IdentifierId) -> Option<TypeRef> {
        self.types.get(&ident).copied()
    }

    fn add_function(&mut self, ident: IdentifierId, function_id: FunctionId) {
        self.functions.insert(ident, function_id);
    }

    fn find_function(&self, ident: IdentifierId) -> Option<FunctionId> {
        self.functions.get(&ident).copied()
    }

    fn add_namespace(&mut self, ident: IdentifierId, namespace_id: NamespaceId) {
        self.namespaces.insert(ident, namespace_id);
    }

    fn find_namespace(&self, ident: IdentifierId) -> Option<NamespaceId> {
        self.namespaces.get(&ident).copied()
    }
}

fn make_err<T: AsRef<str>>(s: T, span: Span) -> TyperError {
    TyperError::make(s.as_ref(), span)
}

fn make_fail<A, T: AsRef<str>>(s: T, span: Span) -> TypedGenResult<A> {
    Err(make_err(s, span))
}

pub struct TypedModule {
    pub ast: Rc<AstModule>,
    functions: Vec<Function>,
    pub variables: Vec<Variable>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub scopes: Scopes,
    pub errors: Vec<TyperError>,
    pub namespaces: Vec<Namespace>,
}

impl TypedModule {
    pub fn new(parsed_module: Rc<AstModule>) -> TypedModule {
        let scopes = Scopes::make();
        let root_ident = parsed_module.ident_id("_root");
        TypedModule {
            ast: parsed_module,
            functions: Vec::new(),
            variables: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            scopes: Scopes::make(),
            errors: Vec::new(),
            namespaces: vec![Namespace { name: root_ident, scope_id: scopes.get_root_scope_id() }],
        }
    }

    pub fn function_iter(&self) -> impl Iterator<Item = (FunctionId, &Function)> {
        self.functions.iter().enumerate().map(|(idx, f)| (idx as FunctionId, f))
    }

    fn internal_compiler_error(&self, message: impl AsRef<str>, span: Span) -> ! {
        self.print_error(message, span);
        panic!()
    }

    fn print_error(&self, message: impl AsRef<str>, span: Span) {
        let adjusted_line = span.line as i32 - crate::prelude::PRELUDE_LINES as i32 + 1;
        let line_no =
            if adjusted_line < 0 { "PRELUDE".to_string() } else { adjusted_line.to_string() };
        eprintln!("{} at {}:{}\n  -> {}", "error".red(), self.name(), line_no, message.as_ref());
        eprintln!("{}", self.ast.source.get_line_by_index(span.line).red());
        eprintln!(" -> {}", self.ast.source.get_span_content(span).red());
    }

    pub fn name(&self) -> &str {
        &self.ast.name
    }

    fn get_ident_str(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        self.ast.get_ident_str(id)
    }

    fn report_error(&mut self, span: Span, message: String) {
        self.errors.push(TyperError { span, message })
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(typ);
        id as u32
    }

    // Should namespaces live in scopes instead of the module? Maybe scopes just have ident -> namespace_id
    fn get_namespace(&self, namespace_id: NamespaceId) -> Option<&Namespace> {
        self.namespaces.get(namespace_id as usize)
    }

    pub fn get_type_if_id(&self, type_ref: TypeRef) -> Option<&Type> {
        if let TypeRef::TypeId(type_id) = type_ref {
            Some(self.get_type(type_id))
        } else {
            None
        }
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id as usize]
    }

    pub fn get_type_mut(&mut self, type_id: TypeId) -> &mut Type {
        &mut self.types[type_id as usize]
    }

    pub fn is_reference_type(&self, ty: TypeRef) -> bool {
        match ty {
            TypeRef::Unit => false,
            TypeRef::Char => false,
            TypeRef::Int => false,
            TypeRef::Bool => false,
            TypeRef::String => false,
            TypeRef::TypeId(type_id) => {
                let ty = self.get_type(type_id);
                match ty {
                    Type::Record(_) => true,
                    Type::Array(_) => true,
                    Type::OpaqueAlias(t) => self.is_reference_type(*t),
                    Type::TypeVariable(_) => true,
                }
            }
        }
    }

    /// Recursively checks if given type contains any type variables
    fn is_generic(&self, ty: TypeRef) -> bool {
        match ty {
            TypeRef::TypeId(type_id) => match self.get_type(type_id) {
                Type::TypeVariable(_) => true,
                Type::Record(record) => record.fields.iter().any(|f| self.is_generic(f.ty)),
                Type::Array(arr) => self.is_generic(arr.element_type),
                Type::OpaqueAlias(t) => self.is_generic(*t),
            },
            _ => false,
        }
    }

    fn eval_type_defn(
        &mut self,
        defn: &parse::TypeDefn,
        scope_id: ScopeId,
    ) -> TypedGenResult<TypeId> {
        let TypeRef::TypeId(type_id) = self.eval_type_expr(&defn.value_expr, scope_id)? else {
            return make_fail("Expected non-scalar rhs of type definition", defn.value_expr.get_span());
        };
        match self.get_type_mut(type_id) {
            Type::Record(record_defn) => {
                // Add the name to this record defn so it can have associated
                // methods and constants
                record_defn.name_if_named = Some(defn.name);
                Ok(type_id)
            }
            _ => make_fail("Invalid rhs for named type definition", defn.value_expr.get_span()),
        }?;
        self.scopes.add_type(scope_id, defn.name, TypeRef::TypeId(type_id));
        Ok(type_id)
    }

    fn eval_type_expr(
        &mut self,
        expr: &parse::TypeExpression,
        scope_id: ScopeId,
    ) -> TypedGenResult<TypeRef> {
        match expr {
            parse::TypeExpression::Unit(_) => Ok(TypeRef::Unit),
            parse::TypeExpression::Char(_) => Ok(TypeRef::Char),
            parse::TypeExpression::Int(_) => Ok(TypeRef::Int),
            parse::TypeExpression::Bool(_) => Ok(TypeRef::Bool),
            parse::TypeExpression::String(_) => Ok(TypeRef::String),
            parse::TypeExpression::Record(record_defn) => {
                let mut fields: Vec<RecordDefnField> = Vec::new();
                for (index, ast_field) in record_defn.fields.iter().enumerate() {
                    let ty = self.eval_type_expr(&ast_field.ty, scope_id)?;
                    fields.push(RecordDefnField { name: ast_field.name, ty, index })
                }
                let record_defn =
                    RecordDefn { fields, name_if_named: None, span: record_defn.span };
                let type_id = self.add_type(Type::Record(record_defn));
                Ok(TypeRef::TypeId(type_id))
            }
            parse::TypeExpression::Name(ident, span) => {
                let ty_ref = self.scopes.find_type(scope_id, *ident);

                ty_ref.ok_or_else(|| {
                    error!("Scope {} Types: {:?}", scope_id, self.scopes.get_scope(scope_id).types);
                    error!(
                        "Scope {} Vars: {:?}",
                        scope_id,
                        self.scopes.get_scope(scope_id).variables
                    );
                    make_err(
                        format!(
                            "could not find type for identifier {}",
                            &*self.ast.get_ident_str(*ident)
                        ),
                        *span,
                    )
                })
            }
            parse::TypeExpression::TypeApplication(ty_app) => {
                let base_name = self.ast.get_ident_str(ty_app.base);
                if &*base_name == "Array" {
                    drop(base_name);
                    if ty_app.params.len() == 1 {
                        let element_ty = self.eval_type_expr(&ty_app.params[0], scope_id)?;
                        let array_ty = ArrayType { span: ty_app.span, element_type: element_ty };
                        let type_id = self.add_type(Type::Array(array_ty));
                        Ok(TypeRef::TypeId(type_id))
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
        }
    }

    /// This should just call eval_type_expr then reject types that arent' allowed in const
    fn eval_const_type_expr(
        &self,
        expr: &parse::TypeExpression,
        _scope_id: ScopeId,
    ) -> TypedGenResult<TypeRef> {
        match expr {
            parse::TypeExpression::Unit(_) => Ok(TypeRef::Unit),
            parse::TypeExpression::Char(_) => Ok(TypeRef::Char),
            parse::TypeExpression::Int(_) => Ok(TypeRef::Int),
            parse::TypeExpression::Bool(_) => Ok(TypeRef::Bool),
            parse::TypeExpression::String(_) => Ok(TypeRef::String),
            parse::TypeExpression::Record(_) => {
                self.internal_compiler_error("No const records yet", expr.get_span())
            }
            parse::TypeExpression::Name(_, _) => {
                self.internal_compiler_error("No references allowed in constants", expr.get_span())
            }
            parse::TypeExpression::TypeApplication(_) => self.internal_compiler_error(
                "No type parameters allowed in constants",
                expr.get_span(),
            ),
        }
    }

    fn typecheck_record(&self, expected: &RecordDefn, actual: &RecordDefn) -> Result<(), String> {
        if expected.fields.len() != actual.fields.len() {
            return Err(format!(
                "expected record with {} fields, got {}",
                expected.fields.len(),
                actual.fields.len()
            ));
        }
        for expected_field in &expected.fields {
            trace!("typechecking record field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name) else {
                return Err(format!("expected record to have field {}", expected_field.name))
            };
            self.typecheck_types(matching_field.ty, expected_field.ty)?;
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
        expected: &RecordDefn,
        actual: &RecordDefn,
    ) -> Result<(), String> {
        for expected_field in &expected.fields {
            trace!("typechecking record field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name) else {
                return Err(format!("expected field {}", expected_field.name));
            };
            self.typecheck_types(matching_field.ty, expected_field.ty)?;
        }
        Ok(())
    }

    fn typecheck_types(&self, expected: TypeRef, actual: TypeRef) -> Result<(), String> {
        trace!("typechecking {:?} vs {:?}", expected, actual);
        if expected == actual {
            Ok(())
        } else {
            match (expected, actual) {
                (TypeRef::TypeId(id1), TypeRef::TypeId(id2)) => {
                    let ty1 = self.get_type(id1);
                    let ty2 = self.get_type(id2);
                    match (ty1, ty2) {
                        (Type::Record(r1), Type::Record(r2)) => self.typecheck_record(r1, r2),
                        (Type::Array(a1), Type::Array(a2)) => {
                            self.typecheck_types(a1.element_type, a2.element_type)
                        }
                        _ => Err(format!("Expected {ty1:?} but got {ty2:?}")),
                    }
                }
                (TypeRef::TypeId(id1), got) => {
                    Err(format!("Expected {:?} but got {:?}", self.get_type(id1), got))
                }
                (exp, TypeRef::TypeId(id2)) => {
                    Err(format!("Expected {:?} but got {:?}", exp, self.get_type(id2)))
                }
                (exp, got) => Err(format!("Expected {:?} but got {:?}", exp, got)),
            }
        }
    }

    fn eval_const(&mut self, const_expr: &parse::ConstVal) -> TypedGenResult<VariableId> {
        let scope_id = 0;
        let ir_type = self.eval_const_type_expr(&const_expr.ty, scope_id)?;
        let num = match &const_expr.value_expr {
            Expression::Literal(Literal::Numeric(n, span)) => {
                self.parse_numeric(n).map_err(|msg| make_err(msg, *span))?
            }
            _other => {
                return make_fail(
                    "Only literals are currently supported as constants",
                    const_expr.span,
                )
            }
        };
        let expr = TypedExpr::Literal(TypedLiteral::Int(num, const_expr.span));
        let variable_id = self.add_variable(Variable {
            name: const_expr.name,
            ir_type,
            is_mutable: false,
            owner_scope: None,
        });
        self.constants.push(Constant { variable_id, expr, ir_type, span: const_expr.span });
        self.scopes.add_variable(scope_id, const_expr.name, variable_id);
        Ok(variable_id)
    }

    fn get_stmt_expression_type(&self, stmt: &TypedStmt) -> TypeRef {
        match stmt {
            TypedStmt::Expr(expr) => expr.get_type(),
            TypedStmt::ValDef(_) => TypeRef::Unit,
            TypedStmt::Assignment(_) => TypeRef::Unit,
            TypedStmt::WhileLoop(_) => TypeRef::Unit,
            TypedStmt::ReturnStmt(_) => TypeRef::Unit,
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

    fn add_function(&mut self, function: Function) -> FunctionId {
        let id = self.functions.len();
        self.functions.push(function);
        id as u32
    }

    fn add_namespace(&mut self, namespace: Namespace) -> NamespaceId {
        let id = self.namespaces.len();
        self.namespaces.push(namespace);
        id as u32
    }

    pub fn get_function(&self, function_id: FunctionId) -> &Function {
        &self.functions[function_id as usize]
    }

    pub fn get_function_mut(&mut self, function_id: FunctionId) -> &mut Function {
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
    fn transform_expr_to_block(&mut self, expr: TypedExpr, scope_id: ScopeId) -> TypedBlock {
        match expr {
            TypedExpr::Block(b) => b,
            expr => {
                let block_scope = self.scopes.add_scope(scope_id);
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
        block.expr_type = TypeRef::Unit;
    }

    fn traverse_namespace_chain(
        &self,
        scope_id: ScopeId,
        namespaces: &[IdentifierId],
        span: Span,
    ) -> TypedGenResult<ScopeId> {
        log::trace!(
            "traverse_namespace_chain: {:?}",
            namespaces.iter().map(|id| self.get_ident_str(*id).to_string()).collect::<Vec<_>>()
        );
        let ns_iter = namespaces.iter();
        let mut cur_scope = scope_id;
        for ns in ns_iter {
            let namespace_id = self.scopes.find_namespace(cur_scope, *ns).ok_or(make_err(
                format!(
                    "Namespace not found: {} in scope: {:?}",
                    &*self.get_ident_str(*ns),
                    self.scopes.get_scope(scope_id)
                ),
                span,
            ))?;
            let namespace = self.get_namespace(namespace_id).unwrap();
            cur_scope = namespace.scope_id;
        }
        Ok(cur_scope)
    }

    /// Passing `expected_type` is an optimization that can save us work.
    /// It does not guarantee that the returned expr always conforms
    /// to the given `expected_type`
    /// Although, maybe we re-think that because it would save
    /// a lot of code if we did a final check here before returning!
    fn eval_expr(
        &mut self,
        expr: &Expression,
        scope_id: ScopeId,
        expected_type: Option<TypeRef>,
    ) -> TypedGenResult<TypedExpr> {
        match expr {
            Expression::Array(array_expr) => {
                let mut element_type: Option<TypeRef> = match expected_type.as_ref() {
                    Some(TypeRef::TypeId(type_id)) => match self.get_type(*type_id) {
                        Type::Array(arr) => Ok(Some(arr.element_type)),
                        t => make_fail(format!("Expected {:?} but got Array", t), array_expr.span),
                    },
                    Some(other) => {
                        make_fail(format!("Expected {:?} but got Array", other), array_expr.span)
                    }
                    None => Ok(None),
                }?;
                let elements: Vec<TypedExpr> = {
                    let mut elements = Vec::new();
                    for elem in &array_expr.elements {
                        let ir_expr = self.eval_expr(elem, scope_id, element_type)?;
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
                    Some(t) => t.expect_type_id(),
                    None => {
                        let array_type = ArrayType { element_type, span: array_expr.span };
                        let type_id = self.add_type(Type::Array(array_type));
                        type_id
                    }
                };
                Ok(TypedExpr::Literal(TypedLiteral::Array(ArrayLiteral {
                    elements,
                    type_id,
                    span: array_expr.span,
                })))
            }
            Expression::IndexOperation(index_op) => {
                let index_expr =
                    self.eval_expr(&index_op.index_expr, scope_id, Some(TypeRef::Int))?;
                if index_expr.get_type() != TypeRef::Int {
                    return make_fail("index type must be int", index_op.span);
                }

                let base_expr = self.eval_expr(&index_op.target, scope_id, None)?;
                let target_type = base_expr.get_type();
                match target_type {
                    TypeRef::String => Ok(TypedExpr::StringIndex(IndexOp {
                        base_expr: Box::new(base_expr),
                        index_expr: Box::new(index_expr),
                        result_type: TypeRef::Char,
                        span: index_op.span,
                    })),
                    TypeRef::TypeId(target_type_id) => {
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
                    _ => make_fail("invalid index base type", index_op.span),
                }
            }
            Expression::Record(ast_record) => {
                // TODO: Technically we could check expected_type here and save this evaluation if it
                // is not a record
                let mut field_values = Vec::new();
                let mut field_defns = Vec::new();
                for (index, ast_field) in ast_record.fields.iter().enumerate() {
                    let expr = self.eval_expr(&ast_field.expr, scope_id, None)?;
                    field_defns.push(RecordDefnField {
                        name: ast_field.name,
                        ty: expr.get_type(),
                        index,
                    });
                    field_values.push(RecordField { name: ast_field.name, expr });
                }
                // We can use 'expected type' here to just go ahead and typecheck or fail
                // rather than make a duplicate type
                let record_type_id = match expected_type.as_ref() {
                    None => {
                        let record_type = RecordDefn {
                            fields: field_defns,
                            name_if_named: None,
                            span: ast_record.span,
                        };
                        let anon_record_type_id = self.add_type(Type::Record(record_type));
                        Ok(anon_record_type_id)
                    }
                    Some(TypeRef::TypeId(record_type_id)) => match self.get_type(*record_type_id) {
                        // If there is an expected type, it had better be a record
                        // If it is, we return its existing id
                        // If it is not, that's a typechecker error
                        Type::Record(_) => Ok(*record_type_id),
                        t => make_fail(
                            format!("Expected type {:?} but got record literal", t),
                            ast_record.span,
                        ),
                    },
                    Some(other_type) => make_fail(
                        format!("Expected type {:?} but got record literal", other_type),
                        ast_record.span,
                    ),
                }?;
                let ir_record =
                    Record { fields: field_values, span: ast_record.span, type_id: record_type_id };
                Ok(TypedExpr::Literal(TypedLiteral::Record(ir_record)))
            }
            Expression::If(if_expr) => {
                // Ensure boolean condition (or optional which isn't built yet)
                let condition = self.eval_expr(&if_expr.cond, scope_id, Some(TypeRef::Bool))?;
                if let Err(e) = self.typecheck_types(TypeRef::Bool, condition.get_type()) {
                    return make_fail(
                        format!("Invalid if condition type: {}", e),
                        if_expr.cond.get_span(),
                    );
                }
                let consequent_expr = self.eval_expr(&if_expr.cons, scope_id, None)?;
                let consequent_type = consequent_expr.get_type();
                // De-sugar if without else:
                // If there is no alternate, we coerce the consequent to return Unit, so both
                // branches have a matching type, making codegen simple and branchless
                let consequent = if if_expr.alt.is_none() {
                    let mut block = self.transform_expr_to_block(consequent_expr, scope_id);
                    self.coerce_block_to_unit_block(&mut block);
                    block
                } else {
                    self.transform_expr_to_block(consequent_expr, scope_id)
                };
                let alternate = if let Some(alt) = &if_expr.alt {
                    let expr = self.eval_expr(alt, scope_id, Some(consequent_type))?;
                    self.transform_expr_to_block(expr, scope_id)
                } else {
                    TypedBlock {
                        expr_type: TypeRef::Unit,
                        scope_id,
                        statements: vec![TypedStmt::Expr(Box::new(TypedExpr::unit_literal(
                            if_expr.span,
                        )))],
                        span: if_expr.span,
                    }
                };
                if self.typecheck_types(consequent.expr_type, alternate.expr_type).is_err() {
                    return make_fail(
                        format!(
                            "else branch type {:?} did not match then branch type {:?}",
                            consequent.expr_type, alternate.expr_type
                        ),
                        alternate.span,
                    );
                }
                let overall_type = consequent.expr_type;
                Ok(TypedExpr::If(Box::new(TypedIf {
                    condition,
                    consequent,
                    alternate,
                    ir_type: overall_type,
                    span: if_expr.span,
                })))
            }
            Expression::BinaryOp(binary_op) => {
                // Infer expected type to be type of operand1
                let lhs = self.eval_expr(&binary_op.lhs, scope_id, None)?;
                let rhs = self.eval_expr(&binary_op.rhs, scope_id, Some(lhs.get_type()))?;

                // FIXME: Typechecker We are not really typechecking binary operations at all.
                //        This is not enough; we need to check that the lhs is actually valid
                //        for this operation first
                if self.typecheck_types(lhs.get_type(), rhs.get_type()).is_err() {
                    return make_fail("operand types did not match", binary_op.span);
                }

                let kind = match binary_op.op_kind {
                    parse::BinaryOpKind::Add => BinaryOpKind::Add,
                    parse::BinaryOpKind::Subtract => BinaryOpKind::Subtract,
                    parse::BinaryOpKind::Multiply => BinaryOpKind::Multiply,
                    parse::BinaryOpKind::Divide => BinaryOpKind::Divide,
                    parse::BinaryOpKind::Less => BinaryOpKind::Less,
                    parse::BinaryOpKind::LessEqual => BinaryOpKind::LessEqual,
                    parse::BinaryOpKind::Greater => BinaryOpKind::Greater,
                    parse::BinaryOpKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                    parse::BinaryOpKind::And => BinaryOpKind::And,
                    parse::BinaryOpKind::Or => BinaryOpKind::Or,
                    parse::BinaryOpKind::Equals => BinaryOpKind::Equals,
                };
                let result_type = match kind {
                    BinaryOpKind::Add => lhs.get_type(),
                    BinaryOpKind::Subtract => lhs.get_type(),
                    BinaryOpKind::Multiply => lhs.get_type(),
                    BinaryOpKind::Divide => lhs.get_type(),
                    BinaryOpKind::Less => TypeRef::Bool,
                    BinaryOpKind::LessEqual => TypeRef::Bool,
                    BinaryOpKind::Greater => TypeRef::Bool,
                    BinaryOpKind::GreaterEqual => TypeRef::Bool,
                    BinaryOpKind::And => lhs.get_type(),
                    BinaryOpKind::Or => lhs.get_type(),
                    BinaryOpKind::Equals => TypeRef::Bool,
                };
                let expr = TypedExpr::BinaryOp(BinaryOp {
                    kind,
                    ir_type: result_type,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                });
                Ok(expr)
            }
            Expression::Literal(Literal::Unit(span)) => {
                Ok(TypedExpr::Literal(TypedLiteral::Unit(*span)))
            }
            Expression::Literal(Literal::Char(byte, span)) => {
                Ok(TypedExpr::Literal(TypedLiteral::Char(*byte, *span)))
            }
            Expression::Literal(Literal::Numeric(s, span)) => {
                let num = self.parse_numeric(s).map_err(|msg| make_err(msg, *span))?;
                Ok(TypedExpr::Literal(TypedLiteral::Int(num, *span)))
            }
            Expression::Literal(Literal::Bool(b, span)) => {
                let expr = TypedExpr::Literal(TypedLiteral::Bool(*b, *span));
                Ok(expr)
            }
            Expression::Literal(Literal::String(s, span)) => {
                // So sad, we could just point to the source. BUT if we ever do escaping and things
                // then the source string is not the same as the string the user meant; perhaps here
                // is the place, or maybe in the parser, that we would actually do some work, which
                // would justify storing it separately. But then, post-transform, we should intern
                // these
                let expr = TypedExpr::Literal(TypedLiteral::Str(s.clone(), *span));
                Ok(expr)
            }
            Expression::Variable(variable) => {
                let var_index =
                    self.scopes.find_variable(scope_id, variable.ident).ok_or(make_err(
                        format!("{} is not defined", &*self.get_ident_str(variable.ident)),
                        variable.span,
                    ))?;
                let v = self.get_variable(var_index);
                let expr = TypedExpr::Variable(VariableExpr {
                    ir_type: v.ir_type,
                    variable_id: var_index,
                    span: variable.span,
                });
                Ok(expr)
            }
            Expression::FieldAccess(field_access) => {
                let base_expr = self.eval_expr(&field_access.base, scope_id, None)?;
                let type_ref = base_expr.get_type();
                let ret_type = match type_ref {
                    TypeRef::TypeId(type_id) => {
                        let ty = self.get_type(type_id);
                        match ty {
                            Type::Record(record_type) => {
                                let (_idx, target_field) =
                                    record_type.find_field(field_access.target).ok_or(make_err(
                                        format!(
                                            "Field {} not found on record type",
                                            &*self.get_ident_str(field_access.target)
                                        ),
                                        field_access.span,
                                    ))?;
                                Ok(target_field.ty)
                            }
                            _ => make_fail(
                                format!(
                                    "Cannot access field {} on non-record type: {:?}",
                                    &*self.get_ident_str(field_access.target),
                                    ty
                                ),
                                field_access.span,
                            ),
                        }
                    }
                    _ => make_fail(
                        format!(
                            "Cannot access field {} on non-record type: {:?}",
                            &*self.get_ident_str(field_access.target),
                            type_ref
                        ),
                        field_access.span,
                    ),
                }?;
                Ok(TypedExpr::FieldAccess(FieldAccess {
                    base: Box::new(base_expr),
                    target_field: field_access.target,
                    ir_type: ret_type,
                    span: field_access.span,
                }))
            }
            Expression::Block(block) => {
                let block = self.eval_block(block, scope_id)?;
                Ok(TypedExpr::Block(block))
            }
            Expression::MethodCall(m_call) => {
                let base_expr = self.eval_expr(&m_call.base, scope_id, None)?;
                let call = self.eval_function_call(&m_call.call, Some(base_expr), scope_id)?;
                Ok(TypedExpr::FunctionCall(call))
            }
            Expression::FnCall(fn_call) => {
                let call = self.eval_function_call(fn_call, None, scope_id)?;
                Ok(TypedExpr::FunctionCall(call))
            }
        }
    }
    fn eval_function_call(
        &mut self,
        fn_call: &FnCall,
        this_expr: Option<TypedExpr>,
        scope_id: ScopeId,
    ) -> TypedGenResult<Call> {
        // This block is all about method or resolution
        // We are trying to find out if this method or function
        // exists, and returning its id if so
        let function_id = match this_expr.as_ref() {
            Some(base_expr) => {
                // Resolve a method call
                let type_ref = base_expr.get_type();
                let function_id = match type_ref {
                    TypeRef::String => {
                        // TODO: Abstract out a way to go from identifier to scope
                        //       (name -> ident id -> namespace id -> namespace -> scope id -> scope
                        let string_ident_id = self.ast.ident_id("string");
                        let string_namespace_id =
                            self.scopes.find_namespace(scope_id, string_ident_id).unwrap();
                        let string_namespace = self.get_namespace(string_namespace_id).unwrap();
                        let string_scope = self.scopes.get_scope(string_namespace.scope_id);
                        string_scope.find_function(fn_call.name)
                    }
                    TypeRef::Char => {
                        let char_ident_id = self.ast.ident_id("char");
                        let char_namespace_id =
                            self.scopes.find_namespace(scope_id, char_ident_id).unwrap();
                        let char_namespace = self.get_namespace(char_namespace_id).unwrap();
                        let char_scope = self.scopes.get_scope(char_namespace.scope_id);
                        char_scope.find_function(fn_call.name)
                    }
                    TypeRef::TypeId(type_id) => {
                        let ty = self.get_type(type_id);
                        match ty {
                            Type::Array(_array_type) => {
                                let array_ident_id = self.ast.ident_id("Array");
                                let array_namespace_id =
                                    self.scopes.find_namespace(scope_id, array_ident_id).unwrap();
                                let array_namespace =
                                    self.get_namespace(array_namespace_id).unwrap();
                                let array_scope = self.scopes.get_scope(array_namespace.scope_id);
                                array_scope.find_function(fn_call.name)
                            }
                            Type::Record(record) => {
                                // Need to distinguish between instances of 'named'
                                // records and anonymous ones
                                let Some(record_type_name) = record.name_if_named else {
                                  return make_fail("Anonymous records currently have no methods", record.span);  
                                };
                                let record_namespace_id =
                                    self.scopes.find_namespace(scope_id, record_type_name).unwrap();
                                let record_namespace =
                                    self.get_namespace(record_namespace_id).unwrap();
                                let record_scope = self.scopes.get_scope(record_namespace.scope_id);
                                record_scope.find_function(fn_call.name)
                            }
                            _ => None,
                        }
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
                                type_ref,
                            ),
                            fn_call.span,
                        )
                    }
                }
            }
            None => {
                // Resolve a non-method call
                let scope_to_search =
                    self.traverse_namespace_chain(scope_id, &fn_call.namespaces, fn_call.span)?;
                let function_id =
                    self.scopes.find_function(scope_to_search, fn_call.name).ok_or(make_err(
                        format!(
                            "Function not found: {} in scope: {:?}",
                            &*self.get_ident_str(fn_call.name),
                            self.scopes.get_scope(scope_id)
                        ),
                        fn_call.span,
                    ))?;
                function_id
            }
        };

        // Now that we have resolved to a function id, we need to specialize it if generic
        let original_function = self.get_function(function_id);
        let function_to_call = if original_function.is_generic() {
            let intrinsic_type = original_function.intrinsic_type;
            self.get_specialized_function_for_call(fn_call, function_id, intrinsic_type)?
        } else {
            function_id
        };
        let mut final_args: Vec<TypedExpr> = Vec::new();
        let params_cloned = self.get_function(function_to_call).params.clone();
        // We have to deal with this outside of the loop because
        // we can't 'move' out of this_expr more than once
        let mut skip_first = false;
        if let Some(first) = params_cloned.get(0) {
            let is_self = first.name == self.ast.ident_id("self");
            if is_self {
                if let Some(this) = this_expr {
                    final_args.push(this);
                    skip_first = true;
                }
            }
        }
        let start = if skip_first { 1 } else { 0 };
        for fn_param in &params_cloned[start..] {
            let matching_param_by_name =
                fn_call.args.iter().find(|arg| arg.name == Some(fn_param.name));
            // If we skipped 'self', we need to subtract 1 from the offset we index into fn_call.args with
            let matching_idx = fn_param.position - start;
            let matching_param = matching_param_by_name.or(fn_call.args.get(matching_idx));
            if let Some(param) = matching_param {
                let expr = self.eval_expr(&param.value, scope_id, Some(fn_param.ty))?;
                if let Err(e) = self.typecheck_types(fn_param.ty, expr.get_type()) {
                    return make_fail(
                        format!("Invalid parameter type: {}", e),
                        param.value.get_span(),
                    );
                }
                final_args.push(expr);
            } else {
                return make_fail(
                    format!(
                        "Could not find match for parameter {}",
                        &*self.get_ident_str(fn_param.name)
                    ),
                    fn_call.span,
                );
            }
        }
        let function_ret_type = self.get_function(function_to_call).ret_type;
        let call = Call {
            callee_function_id: function_to_call,
            args: final_args,
            ret_type: function_ret_type,
            span: fn_call.span,
        };
        Ok(call)
    }

    fn get_specialized_function_for_call(
        &mut self,
        fn_call: &FnCall,
        old_function_id: FunctionId,
        intrinsic_type: Option<IntrinsicFunctionType>,
    ) -> TypedGenResult<FunctionId> {
        // TODO: Implement full generic type inference. This could get slow!
        //       Cases like [T](t: T) are easier but [T](x: ComplexType[A, B, T]) and solving for
        //       T in that case is hard. Requires recursive search.
        //       I wonder if we could infer in simple cases and refuse to infer
        //       in complex cases that would be slow.
        //       Inference algorithm:
        //       1. Find arguments that include a type param
        //       2. Find the actual value passed for each, find where the type variable appears within
        //          that type expression, and assign it to the concrete type

        // FIXME: Can we avoid this clone of the whole function
        let generic_function = self.get_function(old_function_id).clone();
        trace!(
            "Specializing function call: {}, {} ,astid {}",
            &*self.get_ident_str(fn_call.name),
            &*self.get_ident_str(generic_function.name),
            generic_function.ast_id
        );
        let type_params =
            generic_function.type_params.as_ref().expect("expected function to be generic");
        let type_args =
            fn_call.type_args.as_ref().ok_or(make_err("fn call mising type args", fn_call.span))?;
        let mut new_name = self.get_ident_str(fn_call.name).to_string();

        // The specialized function lives in the root of the module because
        // we never look it up by name; we look up the generic version then use a cached
        // specialized function anyway

        // The only real difference is the scope: it has substitutions for the type variables
        let spec_fn_scope_id = self.scopes.add_scope_to_root();
        for (i, type_param) in type_params.iter().enumerate() {
            let type_arg = &type_args[i];
            let type_ref = self.eval_type_expr(&type_arg.value, spec_fn_scope_id)?;
            self.scopes.get_scope_mut(spec_fn_scope_id).add_type(type_param.ident, type_ref);
        }
        new_name.push_str("_spec_");
        let specialization_count = generic_function.specializations.len();
        new_name.push_str(&specialization_count.to_string());

        let ast = self.ast.clone();
        let Definition::FnDef(ast_def) = ast.get_defn(generic_function.ast_id) else {
            self.internal_compiler_error("failed to get AST node for function specialization", fn_call.span)
        };
        let specialized_function_id = self.eval_function(
            ast_def,
            self.scopes.get_root_scope_id(),
            Some(spec_fn_scope_id),
            true,
            intrinsic_type,
        )?;
        Ok(specialized_function_id)
    }
    fn eval_block_stmt(
        &mut self,
        stmt: &BlockStmt,
        scope_id: ScopeId,
    ) -> TypedGenResult<TypedStmt> {
        match stmt {
            BlockStmt::ValDef(val_def) => {
                let provided_ir_type = match val_def.ty.as_ref() {
                    None => None,
                    Some(type_expr) => Some(self.eval_type_expr(&type_expr, scope_id)?),
                };
                let value_expr = self.eval_expr(&val_def.value, scope_id, provided_ir_type)?;
                let actual_type = value_expr.get_type();
                if let Some(expected_type) = provided_ir_type {
                    if let Err(msg) = self.typecheck_types(expected_type, actual_type) {
                        return make_fail(
                            format!("Local variable type mismatch: {}", msg),
                            val_def.span,
                        );
                    }
                }

                let variable_id = self.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name,
                    ir_type: actual_type,
                    owner_scope: Some(scope_id),
                });
                let val_def_stmt = TypedStmt::ValDef(Box::new(ValDef {
                    ir_type: actual_type,
                    variable_id,
                    initializer: value_expr,
                    span: val_def.span,
                }));
                self.scopes.add_variable(scope_id, val_def.name, variable_id);
                Ok(val_def_stmt)
            }
            BlockStmt::Assignment(assignment) => {
                let lhs = self.eval_expr(&assignment.lhs, scope_id, None)?;
                match &lhs {
                    TypedExpr::Variable(v) => {
                        let var = self.get_variable(v.variable_id);
                        if !var.is_mutable {
                            return make_fail(
                                "Cannot assign to immutable variable",
                                assignment.span,
                            );
                        }
                    }
                    TypedExpr::FieldAccess(_) => {
                        trace!("assignment to record member");
                    }
                    TypedExpr::ArrayIndex(_) => {
                        trace!("assignment to array index");
                    }
                    _ => {
                        return make_fail(
                            format!("Invalid assignment lhs: {:?}", lhs),
                            lhs.get_span(),
                        )
                    }
                };
                let rhs = self.eval_expr(&assignment.rhs, scope_id, Some(lhs.get_type()))?;
                if let Err(msg) = self.typecheck_types(lhs.get_type(), rhs.get_type()) {
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
                let expr = self.eval_expr(expression, scope_id, None)?;
                Ok(TypedStmt::Expr(Box::new(expr)))
            }
            BlockStmt::While(while_stmt) => {
                let cond = self.eval_expr(&while_stmt.cond, scope_id, Some(TypeRef::Bool))?;
                if let Err(e) = self.typecheck_types(TypeRef::Bool, cond.get_type()) {
                    return make_fail(
                        format!("Invalid while condition type: {}", e),
                        cond.get_span(),
                    );
                }
                let block = self.eval_block(&while_stmt.block, scope_id)?;
                Ok(TypedStmt::WhileLoop(Box::new(TypedWhileLoop {
                    cond,
                    block,
                    span: while_stmt.span,
                })))
            }
        }
    }
    fn eval_block(&mut self, block: &Block, scope_id: ScopeId) -> TypedGenResult<TypedBlock> {
        let mut statements = Vec::new();
        for stmt in &block.stmts {
            let stmt = self.eval_block_stmt(stmt, scope_id)?;
            statements.push(stmt);
        }

        let expr_type = if let Some(stmt) = statements.last() {
            self.get_stmt_expression_type(stmt)
        } else {
            TypeRef::Unit
        };
        // This whole expr_type vs return_type thing is just a really bad beginner version of the concept of "control flow" in
        // a typechecker!!!
        //
        // let return_type = statements.iter().find(|stmt| stmt.get_return_type());
        let ir_block = TypedBlock { expr_type, scope_id: 0, statements, span: block.span };
        Ok(ir_block)
    }

    fn get_scope_for_namespace(&self, namespace_ident: IdentifierId) -> ScopeId {
        self.namespaces.iter().find(|ns| ns.name == namespace_ident).unwrap().scope_id
    }

    fn resolve_intrinsic_function_type(
        &self,
        fn_def: &FnDef,
        scope_id: ScopeId,
    ) -> IntrinsicFunctionType {
        trace!("resolve_intrinsic_function_type for {}", &*self.get_ident_str(fn_def.name));
        let Some(current_namespace) = self.namespaces.iter().find(|ns| ns.scope_id == scope_id) else {
            println!("{:?}", fn_def);
            panic!("Functions must be defined within a namespace scope: {:?}", &*self.get_ident_str(fn_def.name))
        };
        let result = if current_namespace.name == self.ast.ident_id("string") {
            if fn_def.name == self.ast.ident_id("length") {
                Some(IntrinsicFunctionType::StringLength)
            } else if fn_def.name == self.ast.ident_id("new") {
                Some(IntrinsicFunctionType::StringNew)
            } else {
                None
            }
        } else if current_namespace.name == self.ast.ident_id("Array") {
            if fn_def.name == self.ast.ident_id("length") {
                Some(IntrinsicFunctionType::ArrayLength)
            } else if fn_def.name == self.ast.ident_id("new") {
                Some(IntrinsicFunctionType::ArrayNew)
            } else {
                None
            }
        } else if current_namespace.name == self.ast.ident_id("char") {
            if fn_def.name == self.ast.ident_id("to_string") {
                Some(IntrinsicFunctionType::CharToString)
            } else {
                None
            }
        } else if current_namespace.name == self.ast.ident_id("_root") {
            let function_name = &*self.get_ident_str(fn_def.name);
            IntrinsicFunctionType::from_function_name(function_name)
        } else {
            None
        };
        match result {
            Some(result) => result,
            None => panic!(
                "Could not resolve intrinsic function type for function: {} in namespace: {}",
                &*self.get_ident_str(fn_def.name),
                &*self.get_ident_str(current_namespace.name)
            ),
        }
    }

    fn eval_function(
        &mut self,
        fn_def: &FnDef,
        parent_scope_id: ScopeId,
        fn_scope_id: Option<ScopeId>,
        specialize: bool,
        // Used only during specialization; we already know the intrinsic type
        // from the generic version so we just pass it in
        known_intrinsic: Option<IntrinsicFunctionType>,
    ) -> TypedGenResult<FunctionId> {
        let mut params = Vec::new();
        let fn_scope_id = match fn_scope_id {
            None => self.scopes.add_scope(parent_scope_id),
            Some(fn_scope_id) => fn_scope_id,
        };

        // Instantiate type arguments
        let is_generic =
            !specialize && fn_def.type_args.as_ref().map(|args| !args.is_empty()).unwrap_or(false);
        trace!(
            "eval_function {} is_generic: {}, specialize: {} in scope: {}",
            &*self.get_ident_str(fn_def.name),
            is_generic,
            specialize,
            parent_scope_id
        );
        let mut type_params: Option<Vec<TypeParam>> = None;
        if is_generic {
            let mut the_type_params = Vec::new();
            for type_parameter in fn_def.type_args.as_ref().unwrap().iter() {
                let type_variable =
                    TypeVariable { identifier_id: type_parameter.ident, constraints: None };
                let type_variable_id = self.add_type(Type::TypeVariable(type_variable));
                let fn_scope = self.scopes.get_scope_mut(fn_scope_id);
                let type_param =
                    TypeParam { ident: type_parameter.ident, type_id: type_variable_id };
                the_type_params.push(type_param);
                fn_scope.add_type(type_parameter.ident, TypeRef::TypeId(type_variable_id))
            }
            type_params = Some(the_type_params);
            trace!(
                "Added type arguments to function {} scope {:?}",
                &*self.get_ident_str(fn_def.name),
                self.scopes.get_scope(fn_scope_id)
            );
        }

        // Typecheck arguments
        for (idx, fn_arg) in fn_def.args.iter().enumerate() {
            let ir_type = self.eval_type_expr(&fn_arg.ty, fn_scope_id)?;
            if specialize {
                trace!("Specializing: {:?} got {:?}", &*self.get_ident_str(fn_arg.name), ir_type);
            }
            let variable = Variable {
                name: fn_arg.name,
                ir_type,
                is_mutable: false,
                owner_scope: Some(fn_scope_id),
            };
            let variable_id = self.add_variable(variable);
            params.push(FnArgDefn { name: fn_arg.name, variable_id, position: idx, ty: ir_type });
            self.scopes.add_variable(fn_scope_id, fn_arg.name, variable_id);
        }

        let intrinsic_type = if specialize && known_intrinsic.is_some() {
            known_intrinsic
        } else if fn_def.is_intrinsic {
            Some(self.resolve_intrinsic_function_type(fn_def, parent_scope_id))
        } else {
            None
        };
        let given_ret_type = match &fn_def.ret_type {
            None => TypeRef::Unit,
            Some(type_expr) => self.eval_type_expr(type_expr, fn_scope_id)?,
        };
        let function = Function {
            name: fn_def.name,
            scope: fn_scope_id,
            ret_type: given_ret_type,
            params,
            type_params,
            block: None,
            intrinsic_type,
            specializations: Vec::new(),
            ast_id: fn_def.ast_id,
        };
        let function_id = self.add_function(function);
        // We do not want to resolve specialized functions by name!
        // So don't add them to any scope.
        // They all have the same name but different types!!!
        if !specialize {
            self.scopes.add_function(parent_scope_id, fn_def.name, function_id);
        }
        let is_intrinsic = intrinsic_type.is_some();
        let body_block = match &fn_def.block {
            Some(block_ast) => {
                let block = self.eval_block(block_ast, fn_scope_id)?;
                if let Err(msg) = self.typecheck_types(given_ret_type, block.expr_type) {
                    return make_fail(
                        format!(
                            "Function {} return type mismatch: {}",
                            &*self.get_ident_str(fn_def.name),
                            msg
                        ),
                        fn_def.span,
                    );
                } else {
                    Some(block)
                }
            }
            None if is_intrinsic => None,
            None => return make_fail("function is missing implementation", fn_def.span),
        };
        // Add the body now
        let function = self.get_function_mut(function_id);
        function.block = body_block;
        Ok(function_id)
    }
    fn eval_namespace(
        &mut self,
        ast_namespace: &ParsedNamespace,
        scope_id: ScopeId,
    ) -> TypedGenResult<NamespaceId> {
        // We add the new namespace's scope as a child of the current scope
        let ns_scope_id = self.scopes.add_scope(scope_id);
        let namespace = Namespace { name: ast_namespace.name, scope_id: ns_scope_id };
        let namespace_id = self.add_namespace(namespace);
        // We add the new namespace to the current scope
        let scope = self.scopes.get_scope_mut(scope_id);
        scope.add_namespace(ast_namespace.name, namespace_id);
        for fn_def in &ast_namespace.definitions {
            if let Definition::FnDef(fn_def) = fn_def {
                self.eval_function(fn_def, ns_scope_id, None, false, None)?;
            } else {
                panic!("Unsupported definition type inside namespace: {:?}", fn_def)
            }
        }
        Ok(namespace_id)
    }
    fn eval_definition(&mut self, def: &Definition, scope_id: ScopeId) -> TypedGenResult<()> {
        match def {
            Definition::Namespace(namespace) => {
                self.eval_namespace(namespace, scope_id)?;
                Ok(())
            }
            Definition::Const(const_val) => {
                let _variable_id: VariableId = self.eval_const(const_val)?;
                Ok(())
            }
            Definition::FnDef(fn_def) => {
                self.eval_function(fn_def, scope_id, None, false, None)?;
                Ok(())
            }
            Definition::TypeDef(type_defn) => {
                self.eval_type_defn(type_defn, scope_id)?;
                let _typ = self.eval_type_expr(&type_defn.value_expr, scope_id)?;
                Ok(())
            }
        }
    }
    pub fn run(&mut self) -> Result<()> {
        let mut errors: Vec<TyperError> = Vec::new();
        // TODO: 'Declare' everything first, will allow modules
        //        to declare their API without full typechecking
        //        will also allow recursion without hacks

        let scope_id = self.scopes.get_root_scope_id();
        for defn in self.ast.clone().defns_iter() {
            let result = self.eval_definition(defn, scope_id);
            if let Err(e) = result {
                self.print_error(&e.message, e.span);
                errors.push(e);
            }
        }
        if !errors.is_empty() {
            bail!("Typechecking failed")
        }
        Ok(())
    }
}

impl Display for TypedModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Module ")?;
        f.write_str(&self.ast.name)?;
        f.write_str("\n")?;
        for (id, variable) in self.variables.iter().enumerate() {
            f.write_fmt(format_args!("id={id:02} {variable:#?}\n"))?;
        }
        f.write_str(": \n")?;
        for (id, func) in self.functions.iter().enumerate() {
            f.write_fmt(format_args!("id={id:02} {func:#?}\n"))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::parse::parse_text;
    use crate::typer::*;

    #[test]
    fn const_definition_1() -> anyhow::Result<()> {
        let src = r"val x: int = 420;";
        let module = parse_text(src, "basic_fn.nx", false)?;
        let mut ir = TypedModule::new(Rc::new(module));
        ir.run()?;
        let i1 = &ir.constants[0];
        if let TypedExpr::Literal(TypedLiteral::Int(i, span)) = i1.expr {
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
        let module = parse_text(src, "basic_fn.nx", false)?;
        let mut ir = TypedModule::new(Rc::new(module));
        ir.run()?;
        println!("{:?}", ir.functions);
        Ok(())
    }
}
