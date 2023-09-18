#![allow(clippy::match_like_matches_macro)]

use crate::ir::Type::OpaqueAlias;
use crate::lex::Span;
use crate::parse;
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

#[derive(Debug, Clone)]
pub struct RecordDefnField {
    pub name: IdentifierId,
    pub ty: TypeRef,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct RecordDefn {
    pub fields: Vec<RecordDefnField>,
    pub span: Span,
}

impl RecordDefn {
    pub fn find_field(&self, field_name: IdentifierId) -> Option<RecordDefnField> {
        self.fields.iter().find(|field| field.name == field_name).cloned()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Unit,
    Int,
    Bool,
    String,
    TypeId(TypeId),
}

impl TypeRef {
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

#[derive(Debug, Clone)]
pub struct IrBlock {
    pub ret_type: TypeRef,
    pub scope_id: ScopeId,
    pub statements: Vec<IrStmt>,
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
    pub block: Option<IrBlock>,
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
    Multiply,
    And,
    Or,
    Equals,
}

impl BinaryOpKind {
    pub fn is_integer_op(&self) -> bool {
        match self {
            BinaryOpKind::Add => true,
            BinaryOpKind::Multiply => true,
            BinaryOpKind::Or | BinaryOpKind::And => true,
            BinaryOpKind::Equals => true,
        }
    }
    pub fn is_boolean_op(&self) -> bool {
        match self {
            BinaryOpKind::Equals => true,
            BinaryOpKind::Or | BinaryOpKind::And => true,
            BinaryOpKind::Add => false,
            BinaryOpKind::Multiply => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub ir_type: TypeRef,
    pub lhs: Box<IrExpr>,
    pub rhs: Box<IrExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub callee_function_id: FunctionId,
    pub args: Vec<IrExpr>,
    // FIXME: ret_type doesn't belong on here; just look up the function to get it
    pub ret_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: IdentifierId,
    pub expr: IrExpr,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub fields: Vec<RecordField>,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<IrExpr>,
    pub type_id: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrLiteral {
    Unit(Span),
    Bool(bool, Span),
    Int(i64, Span),
    Str(String, Span),
    Record(Record),
    Array(ArrayLiteral),
}

impl IrLiteral {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            IrLiteral::Unit(span) => *span,
            IrLiteral::Str(_, span) => *span,
            IrLiteral::Int(_, span) => *span,
            IrLiteral::Bool(_, span) => *span,
            IrLiteral::Record(record) => record.span,
            IrLiteral::Array(arr) => arr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrIf {
    pub condition: IrExpr,
    pub consequent: IrBlock,
    pub alternate: IrBlock,
    pub ir_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub base: Box<IrExpr>,
    pub target_field: RecordDefnField,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrExpr {
    Literal(IrLiteral),
    Variable(VariableExpr),
    FieldAccess(FieldAccess),
    BinaryOp(BinaryOp),
    Block(IrBlock),
    FunctionCall(FunctionCall),
    If(Box<IrIf>),
}

impl IrExpr {
    pub fn unit_literal(span: Span) -> IrExpr {
        IrExpr::Literal(IrLiteral::Unit(span))
    }

    #[inline]
    pub fn get_type(&self) -> TypeRef {
        match self {
            IrExpr::Literal(IrLiteral::Unit(_)) => TypeRef::Unit,
            IrExpr::Literal(IrLiteral::Str(_, _)) => TypeRef::String,
            IrExpr::Literal(IrLiteral::Int(_, _)) => TypeRef::Int,
            IrExpr::Literal(IrLiteral::Bool(_, _)) => TypeRef::Bool,
            IrExpr::Literal(IrLiteral::Record(record)) => TypeRef::TypeId(record.type_id),
            IrExpr::Literal(IrLiteral::Array(arr)) => TypeRef::TypeId(arr.type_id),
            IrExpr::Variable(var) => var.ir_type,
            IrExpr::FieldAccess(field_access) => field_access.target_field.ty,
            IrExpr::BinaryOp(binary_op) => binary_op.ir_type,
            IrExpr::Block(b) => b.ret_type,
            IrExpr::FunctionCall(call) => call.ret_type,
            IrExpr::If(ir_if) => ir_if.ir_type,
        }
    }
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            IrExpr::Literal(lit) => lit.get_span(),
            IrExpr::Variable(var) => var.span,
            IrExpr::FieldAccess(field_access) => field_access.span,
            IrExpr::BinaryOp(binary_op) => binary_op.span,
            IrExpr::Block(b) => b.span,
            IrExpr::FunctionCall(call) => call.span,
            IrExpr::If(ir_if) => ir_if.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValDef {
    pub variable_id: VariableId,
    pub ir_type: TypeRef,
    pub initializer: IrExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub expr: IrExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub destination: Box<IrExpr>,
    pub value: Box<IrExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrStmt {
    Expr(Box<IrExpr>),
    ValDef(Box<ValDef>),
    ReturnStmt(Box<ReturnStmt>),
    Assignment(Box<Assignment>),
}

impl IrStmt {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            IrStmt::Expr(e) => e.get_span(),
            IrStmt::ValDef(v) => v.span,
            IrStmt::ReturnStmt(ret) => ret.span,
            IrStmt::Assignment(ass) => ass.span,
        }
    }
}

#[derive(Debug)]
pub struct IrGenError {
    message: String,
    span: Span,
}

impl IrGenError {
    fn make(message: impl AsRef<str>, span: Span) -> IrGenError {
        IrGenError { message: message.as_ref().to_owned(), span }
    }
}

impl Display for IrGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("error on line {}: {}", self.span.line, self.message))
    }
}

impl Error for IrGenError {}

pub type IrGenResult<A> = Result<A, IrGenError>;

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
    pub expr: IrExpr,
    pub ir_type: TypeRef,
    pub span: Span,
}

pub struct IrCompilerError {
    pub span: Span,
    pub message: String,
}

pub struct IrModule {
    pub ast: Rc<AstModule>,
    pub functions: Vec<Function>,
    pub variables: Vec<Variable>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub scopes: Scopes,
    pub errors: Vec<IrCompilerError>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    intrinsic_functions: HashMap<IntrinsicFunctionType, FunctionId>,
}

impl Scopes {
    fn make() -> Self {
        let scopes = vec![Scope::default()];
        Scopes { scopes, intrinsic_functions: HashMap::new() }
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

    fn find_function(&self, scope_id: ScopeId, identifier: IdentifierId) -> Option<FunctionId> {
        self.get_scope(scope_id).find_function(identifier)
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
    PrintInt,
    ArrayIndex,
}

impl IntrinsicFunctionType {
    pub fn from_function_name(value: &str) -> Option<Self> {
        match value {
            "printInt" => Some(IntrinsicFunctionType::PrintInt),
            "_arrayIndex" => Some(IntrinsicFunctionType::ArrayIndex),
            _ => None,
        }
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    variables: HashMap<IdentifierId, VariableId>,
    functions: HashMap<IdentifierId, FunctionId>,
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
}

fn make_err<T: AsRef<str>>(s: T, span: Span) -> IrGenError {
    IrGenError::make(s.as_ref(), span)
}

fn make_fail<A, T: AsRef<str>>(s: T, span: Span) -> IrGenResult<A> {
    Err(make_err(s, span))
}

impl IrModule {
    pub fn new(parsed_module: Rc<AstModule>) -> IrModule {
        IrModule {
            ast: parsed_module,
            functions: Vec::new(),
            variables: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            scopes: Scopes::make(),
            errors: Vec::new(),
        }
    }

    fn internal_compiler_error(&self, message: impl AsRef<str>, span: Span) -> ! {
        self.print_error(message, span);
        panic!()
    }

    fn print_error(&self, message: impl AsRef<str>, span: Span) {
        eprintln!(
            "{} at {}:{}\n  -> {}",
            "error".red(),
            self.name(),
            (span.line as usize - crate::prelude::PRELUDE_LINES) + 1,
            message.as_ref()
        );
        eprintln!("{}", self.ast.source.get_line_by_index(span.line).red());
        eprintln!(" -> {}", self.ast.source.get_span_content(span).red());
    }

    pub fn name(&self) -> &str {
        &self.ast.name
    }

    fn get_ident_name(&self, id: IdentifierId) -> impl std::ops::Deref<Target = str> + '_ {
        self.ast.get_ident_name(id)
    }

    fn report_error(&mut self, span: Span, message: String) {
        self.errors.push(IrCompilerError { span, message })
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(typ);
        id as u32
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id as usize]
    }

    pub fn is_reference_type(&self, ty: TypeRef) -> bool {
        match ty {
            TypeRef::Unit => false,
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

    fn eval_type_expr(
        &mut self,
        expr: &parse::TypeExpression,
        scope_id: ScopeId,
    ) -> IrGenResult<TypeRef> {
        match expr {
            parse::TypeExpression::Unit(_) => Ok(TypeRef::Unit),
            parse::TypeExpression::Int(_) => Ok(TypeRef::Int),
            parse::TypeExpression::Bool(_) => Ok(TypeRef::Bool),
            parse::TypeExpression::Record(record_defn) => {
                let mut fields: Vec<RecordDefnField> = Vec::new();
                for (index, ast_field) in record_defn.fields.iter().enumerate() {
                    let ty = self.eval_type_expr(&ast_field.ty, scope_id)?;
                    fields.push(RecordDefnField { name: ast_field.name, ty, index })
                }
                let record_defn = RecordDefn { fields, span: record_defn.span };
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
                            &*self.ast.get_ident_name(*ident)
                        ),
                        *span,
                    )
                })
            }
            parse::TypeExpression::TypeApplication(ty_app) => {
                let base_name = self.ast.get_ident_name(ty_app.base);
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

    /// Eventually this will be more restrictive than its sibling eval_type_expr
    fn eval_const_type_expr(
        &self,
        expr: &parse::TypeExpression,
        _scope_id: ScopeId,
    ) -> IrGenResult<TypeRef> {
        match expr {
            parse::TypeExpression::Unit(_) => Ok(TypeRef::Unit),
            parse::TypeExpression::Int(_) => Ok(TypeRef::Int),
            parse::TypeExpression::Bool(_) => Ok(TypeRef::Bool),
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

    fn eval_const(&mut self, const_expr: &parse::ConstVal) -> IrGenResult<VariableId> {
        let scope_id = 0;
        let parse::ConstVal { name, ty: typ, value_expr: value, span } = const_expr;
        let ir_type = self.eval_const_type_expr(typ, scope_id)?;
        let num = match value {
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
        let expr = IrExpr::Literal(IrLiteral::Int(num, *span));
        let variable_id = self.add_variable(Variable {
            name: *name,
            ir_type,
            is_mutable: false,
            owner_scope: None,
        });
        self.constants.push(Constant { variable_id, expr, ir_type, span: *span });
        self.scopes.add_variable(scope_id, *name, variable_id);
        Ok(variable_id)
    }

    fn get_stmt_return_type(&self, stmt: &IrStmt) -> Option<TypeRef> {
        match stmt {
            IrStmt::Expr(expr) => Some(expr.get_type()),
            IrStmt::ValDef(_) => Some(TypeRef::Unit),
            // FIXME: This is not quite right; a return statement
            // is different; should probably be None or 'never'
            IrStmt::ReturnStmt(ret) => Some(ret.expr.get_type()),
            IrStmt::Assignment(_) => Some(TypeRef::Unit),
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

    fn transform_expr_to_block(&mut self, expr: IrExpr, scope_id: ScopeId) -> IrBlock {
        match expr {
            IrExpr::Block(b) => b,
            expr => {
                let block_scope = self.scopes.add_scope(scope_id);
                let ret_type = expr.get_type();
                let span = expr.get_span();
                let statement = IrStmt::Expr(Box::new(expr));
                let statements = vec![statement];

                IrBlock { ret_type, scope_id: block_scope, statements, span }
            }
        }
    }

    fn coerce_block_to_unit_block(&mut self, block: &mut IrBlock) {
        let span = block.statements.last().map(|s| s.get_span()).unwrap_or(block.span);
        let unit_literal = IrExpr::unit_literal(span);
        block.statements.push(IrStmt::Expr(Box::new(unit_literal)));
        block.ret_type = TypeRef::Unit;
    }

    // Maybe, pass in expected type for smarter stuff
    fn eval_expr(&mut self, expr: &Expression, scope_id: ScopeId) -> IrGenResult<IrExpr> {
        match expr {
            Expression::Array(array_expr) => {
                // Yet another place where passing in the expected type
                // would be effective
                // What is the type of an array literal?
                // If we have some subtyping, the 'widest' common element type
                // Maybe just the type of the first element and enforce homogeneous
                // IF empty, Never?
                let mut element_type: TypeRef = TypeRef::Unit;
                let elements: Vec<IrExpr> = {
                    let mut elements = Vec::new();
                    for elem in &array_expr.elements {
                        let ir_expr = self.eval_expr(elem, scope_id)?;
                        if element_type != TypeRef::Unit {
                            self.typecheck_types(element_type, ir_expr.get_type())
                                .map_err(|msg| make_err(msg, elem.get_span()))?
                        }
                        element_type = ir_expr.get_type();
                        elements.push(ir_expr);
                    }
                    elements
                };
                let array_type = ArrayType { element_type, span: array_expr.span };
                let type_id = self.add_type(Type::Array(array_type));
                Ok(IrExpr::Literal(IrLiteral::Array(ArrayLiteral {
                    elements,
                    type_id,
                    span: array_expr.span,
                })))
            }
            Expression::IndexOperation(index_op) => {
                // De-sugar to _arrayIndex(target, index)
                // TODO: Indexing only works for builtin Array for now

                let index_expr = self.eval_expr(&index_op.index_expr, scope_id)?;
                if index_expr.get_type() != TypeRef::Int {
                    return make_fail("index type must be int", index_op.span);
                }

                let target = self.eval_expr(&index_op.target, scope_id)?;
                let TypeRef::TypeId(target_type_id) =  target.get_type() else {
                    return make_fail("index base must be an array", index_op.span)
                };
                let target_type = self.get_type(target_type_id);
                let Type::Array(array_type) = target_type else {
                    return make_fail("index base must be an array", index_op.span);
                };
                // Special-case: call prelude function "_arrayIndex"
                // Just need to look up the ID by intrinsic type
                let array_index_fn = self
                    .scopes
                    .intrinsic_functions
                    .get(&IntrinsicFunctionType::ArrayIndex)
                    .unwrap();

                // _arrayIndex(array, 42)
                Ok(IrExpr::FunctionCall(FunctionCall {
                    callee_function_id: *array_index_fn,
                    args: vec![target, index_expr],
                    ret_type: array_type.element_type,
                    span: index_op.span,
                }))
            }
            Expression::Record(ast_record) => {
                let mut fields = Vec::new();
                let mut field_types = Vec::new();
                for (index, ast_field) in ast_record.fields.iter().enumerate() {
                    let expr = self.eval_expr(&ast_field.expr, scope_id)?;
                    field_types.push(RecordDefnField {
                        name: ast_field.name,
                        ty: expr.get_type(),
                        index,
                    });
                    fields.push(RecordField { name: ast_field.name, expr });
                }
                let record_type = RecordDefn { fields: field_types, span: ast_record.span };
                let record_ty = self.add_type(Type::Record(record_type));
                let ir_record = Record { fields, span: ast_record.span, type_id: record_ty };
                Ok(IrExpr::Literal(IrLiteral::Record(ir_record)))
            }
            Expression::If(if_expr) => {
                // Ensure boolean condition (or optional which isn't built yet)
                let condition = self.eval_expr(&if_expr.cond, scope_id)?;
                if self.typecheck_types(TypeRef::Bool, condition.get_type()).is_err() {
                    return make_fail(
                        format!(
                            "If condition must be of type bool; but got {:?}",
                            condition.get_type()
                        ),
                        if_expr.cond.get_span(),
                    );
                }
                let consequent_expr = self.eval_expr(&if_expr.cons, scope_id)?;
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
                    let expr = self.eval_expr(alt, scope_id)?;
                    self.transform_expr_to_block(expr, scope_id)
                } else {
                    IrBlock {
                        ret_type: TypeRef::Unit,
                        scope_id,
                        statements: vec![IrStmt::Expr(Box::new(IrExpr::unit_literal(
                            if_expr.span,
                        )))],
                        span: if_expr.span,
                    }
                };
                if self.typecheck_types(consequent.ret_type, alternate.ret_type).is_err() {
                    return make_fail(
                        format!(
                            "else branch type {:?} did not match then branch type {:?}",
                            consequent.ret_type, alternate.ret_type
                        ),
                        alternate.span,
                    );
                }
                let overall_type = consequent.ret_type;
                Ok(IrExpr::If(Box::new(IrIf {
                    condition,
                    consequent,
                    alternate,
                    ir_type: overall_type,
                    span: if_expr.span,
                })))
            }
            Expression::BinaryOp(binary_op) => {
                // Infer expected type to be type of operand1
                let lhs = self.eval_expr(&binary_op.lhs, scope_id)?;
                let rhs = self.eval_expr(&binary_op.rhs, scope_id)?;

                if self.typecheck_types(lhs.get_type(), rhs.get_type()).is_err() {
                    return make_fail("operand types did not match", binary_op.span);
                }

                let kind = match binary_op.op_kind {
                    parse::BinaryOpKind::Add => BinaryOpKind::Add,
                    parse::BinaryOpKind::Multiply => BinaryOpKind::Multiply,
                    parse::BinaryOpKind::And => BinaryOpKind::And,
                    parse::BinaryOpKind::Or => BinaryOpKind::Or,
                    parse::BinaryOpKind::Equals => BinaryOpKind::Equals,
                };
                let result_type = match kind {
                    BinaryOpKind::Equals => TypeRef::Bool,
                    _ => lhs.get_type(),
                };
                let expr = IrExpr::BinaryOp(BinaryOp {
                    kind,
                    ir_type: result_type,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                });
                Ok(expr)
            }
            Expression::Literal(Literal::Unit(span)) => Ok(IrExpr::Literal(IrLiteral::Unit(*span))),
            Expression::Literal(Literal::Numeric(s, span)) => {
                let num = self.parse_numeric(s).map_err(|msg| make_err(msg, *span))?;
                Ok(IrExpr::Literal(IrLiteral::Int(num, *span)))
            }
            Expression::Literal(Literal::Bool(b, span)) => {
                let expr = IrExpr::Literal(IrLiteral::Bool(*b, *span));
                Ok(expr)
            }
            Expression::Literal(Literal::String(s, span)) => {
                let expr = IrExpr::Literal(IrLiteral::Str(s.clone(), *span));
                Ok(expr)
            }
            Expression::Variable(variable) => {
                let var_index =
                    self.scopes.find_variable(scope_id, variable.ident).ok_or(make_err(
                        format!("{} is not defined", &*self.get_ident_name(variable.ident)),
                        variable.span,
                    ))?;
                let v = self.get_variable(var_index);
                let expr = IrExpr::Variable(VariableExpr {
                    ir_type: v.ir_type,
                    variable_id: var_index,
                    span: variable.span,
                });
                Ok(expr)
            }
            Expression::FieldAccess(field_access) => {
                let base_expr = self.eval_expr(&field_access.base, scope_id)?;
                // TODO Cleanup This is a recurring 'double-check' pattern where we know we need a type id
                //      but also need it to resolve to a particular type
                let TypeRef::TypeId(type_id) = base_expr.get_type() else {
                    return make_fail(format!("Cannot access field {} on non-record type", field_access.target), field_access.span);
                };
                let Type::Record(record_type) = self.get_type(type_id) else {
                    return make_fail(format!("Cannot access field {} on non-record type", field_access.target), field_access.span);
                };
                let target_field = record_type.find_field(field_access.target).ok_or(make_err(
                    format!("Field {} not found on record type", field_access.target),
                    field_access.span,
                ))?;
                Ok(IrExpr::FieldAccess(FieldAccess {
                    base: Box::new(base_expr),
                    target_field,
                    span: field_access.span,
                }))
            }
            Expression::Block(block) => {
                let block = self.eval_block(block, scope_id)?;
                Ok(IrExpr::Block(block))
            }
            Expression::FnCall(fn_call) => {
                let function_id =
                    self.scopes.find_function(scope_id, fn_call.name).ok_or(make_err(
                        format!("Function not found: {}", &*self.get_ident_name(fn_call.name)),
                        fn_call.span,
                    ))?;

                let function_to_call = if self.get_function(function_id).is_generic() {
                    self.get_specialized_function_for_call(fn_call, function_id)?
                } else {
                    function_id
                };
                let mut call_parameters: Vec<(FnArgDefn, &parse::FnCallArg)> = Vec::new();
                for fn_param in &self.get_function(function_to_call).params {
                    let matching_param_by_name =
                        fn_call.args.iter().find(|arg| arg.name == Some(fn_param.name));
                    let matching_param =
                        matching_param_by_name.or(fn_call.args.get(fn_param.position));
                    if let Some(param) = matching_param {
                        call_parameters.push((fn_param.clone(), param));
                    } else {
                        return make_fail(
                            format!("Could not find match for parameter {}", fn_param.name),
                            fn_call.span,
                        );
                    }
                }
                // forget: function
                let mut fn_args: Vec<IrExpr> = Vec::new();
                for (arg_defn, arg_value) in call_parameters {
                    let param_expr = self.eval_expr(&arg_value.value, scope_id)?;
                    if let Err(e) = self.typecheck_types(arg_defn.ty, param_expr.get_type()) {
                        return make_fail(
                            format!("Invalid parameter type: {}", e),
                            arg_value.value.get_span(),
                        );
                    }
                    fn_args.push(param_expr);
                }
                let function_ret_type = self.get_function(function_to_call).ret_type;
                let call = IrExpr::FunctionCall(FunctionCall {
                    callee_function_id: function_to_call,
                    args: fn_args,
                    ret_type: function_ret_type,
                    span: fn_call.span,
                });
                Ok(call)
            }
        }
    }
    fn get_specialized_function_for_call(
        &mut self,
        fn_call: &FnCall,
        old_function_id: FunctionId,
    ) -> IrGenResult<FunctionId> {
        // TODO: Implement full generic type inference. This could get slow!
        //       Cases like [T](t: T) are easier but [T](x: ComplexType[A, B, T]) and solving for
        //       T in that case is hard. Requires recursive search.
        //       I wonder if we could infer in simple cases and refuse to infer
        //       in complex cases that would be slow.
        //       Inference algorithm:
        //       1. Find arguments that include a type param
        //       2. Find the actual value passed for each, find where the type variable appears within
        //          that type expression, and assign it to the concrete type
        trace!("Specializing function: {}", &*self.get_ident_name(fn_call.name));
        let generic_function = self.get_function(old_function_id).clone();
        let type_params =
            generic_function.type_params.as_ref().expect("expected function to be generic");
        let type_args =
            fn_call.type_args.as_ref().ok_or(make_err("fn call mising type args", fn_call.span))?;
        let mut new_name = self.get_ident_name(fn_call.name).to_string();

        // The specialized function lives in the root of the module
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
        let specialized_function_id =
            self.eval_function(ast_def, generic_function.ast_id, spec_fn_scope_id, true)?;
        Ok(specialized_function_id)
    }
    fn eval_block_stmt(&mut self, stmt: &BlockStmt, scope_id: ScopeId) -> IrGenResult<IrStmt> {
        match stmt {
            BlockStmt::ReturnStmt(return_stmt) => {
                let expr = self.eval_expr(&return_stmt.expr, scope_id)?;
                let ret_inst =
                    IrStmt::ReturnStmt(Box::new(ReturnStmt { expr, span: return_stmt.span }));
                Ok(ret_inst)
            }
            BlockStmt::ValDef(val_def) => {
                let value_expr = self.eval_expr(&val_def.value, scope_id)?;
                let provided_type = val_def
                    .ty
                    .as_ref()
                    .ok_or(make_err("Missing type annotation for val", val_def.span))?;
                let ir_type = self.eval_type_expr(provided_type, scope_id)?;
                let variable_id = self.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name,
                    ir_type,
                    owner_scope: Some(scope_id),
                });
                if let Err(msg) = self.typecheck_types(ir_type, value_expr.get_type()) {
                    return make_fail(
                        format!("Local variable type mismatch {}", msg),
                        val_def.span,
                    );
                }
                let val_def_expr = IrStmt::ValDef(Box::new(ValDef {
                    ir_type,
                    variable_id,
                    initializer: value_expr,
                    span: val_def.span,
                }));
                self.scopes.add_variable(scope_id, val_def.name, variable_id);
                Ok(val_def_expr)
            }
            BlockStmt::Assignment(assignment) => {
                // So, we want to be able to assign to an array elem, x[1] = 2,
                // but we currently desugar the expression x[1] to a function call that returns an int, not a pointer
                // we don't even have 'pointers' as language constructs so we need to do something different
                // for the lhs based on whether its part of an assignment or not
                //
                // The question is: how do we represent "pointer to array elem" or "pointer to struct member"
                // As the LHS in the IR so that codegen knows what to do with it
                // I either need to have 'Pointer' types in the typed ast or
                // just special-case it
                let lhs = self.eval_expr(&assignment.lhs, scope_id)?;
                match &lhs {
                    IrExpr::Variable(v) => {
                        let var = self.get_variable(v.variable_id);
                        if !var.is_mutable {
                            return make_fail(
                                "Cannot assign to immutable variable",
                                assignment.span,
                            );
                        }
                    }
                    IrExpr::FunctionCall(c) => {
                        let f = self.get_function(c.callee_function_id);
                        if f.intrinsic_type == Some(IntrinsicFunctionType::ArrayIndex) {
                            // TODO Check mutability of the underlying array? How?
                        } else {
                            return make_fail("Invalid assignment lhs", lhs.get_span());
                        }
                    }
                    IrExpr::FieldAccess(field_access) => {
                        trace!("assignment to record member");
                    }
                    _ => {
                        return make_fail(
                            format!("Invalid assignment lhs: {:?}", lhs),
                            lhs.get_span(),
                        )
                    }
                };
                let rhs = self.eval_expr(&assignment.rhs, scope_id)?;
                if let Err(msg) = self.typecheck_types(lhs.get_type(), rhs.get_type()) {
                    return make_fail(
                        format!("Invalid types for assignment: {}", msg),
                        assignment.span,
                    );
                }
                let expr = IrStmt::Assignment(Box::new(Assignment {
                    destination: Box::new(lhs),
                    value: Box::new(rhs),
                    span: assignment.span,
                }));
                Ok(expr)
            }
            BlockStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(expression, scope_id)?;
                Ok(IrStmt::Expr(Box::new(expr)))
            }
        }
    }
    fn eval_block(&mut self, block: &Block, scope_id: ScopeId) -> IrGenResult<IrBlock> {
        let mut statements = Vec::new();
        for stmt in &block.stmts {
            let stmt = self.eval_block_stmt(stmt, scope_id)?;
            statements.push(stmt);
        }
        // TODO: The return type of a block is actually an interesting problem, need to think about
        // branching, early returns, etc!
        // For now return type is inferred to be the return type of the last instruction
        // note: This came up again when implementing if/else (return is special!)
        let ret_type = if let Some(stmt) = statements.last() {
            self.get_stmt_return_type(stmt).expect("last block statement should have a return type")
        } else {
            TypeRef::Unit
        };
        let ir_block = IrBlock { ret_type, scope_id: 0, statements, span: block.span };
        Ok(ir_block)
    }
    fn eval_function(
        &mut self,
        fn_def: &FnDef,
        fn_ast_id: AstId,
        scope_id: ScopeId,
        specialize: bool,
    ) -> IrGenResult<FunctionId> {
        let mut params = Vec::new();
        let fn_scope_id = self.scopes.add_scope(scope_id);

        // Instantiate type arguments
        let is_generic =
            !specialize && fn_def.type_args.as_ref().map(|args| !args.is_empty()).unwrap_or(false);
        trace!(
            "function {} is_generic: {}, specialize: {}",
            &*self.get_ident_name(fn_def.name),
            is_generic,
            specialize
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
                &*self.get_ident_name(fn_def.name),
                self.scopes.get_scope(fn_scope_id)
            );
        }

        // Typecheck arguments
        for (idx, fn_arg) in fn_def.args.iter().enumerate() {
            let ir_type = self.eval_type_expr(&fn_arg.ty, fn_scope_id)?;
            if specialize {
                trace!("Specializing: {:?} got {:?}", &fn_arg, ir_type);
            }
            let variable = Variable {
                name: fn_arg.name,
                ir_type,
                is_mutable: false,
                owner_scope: Some(fn_scope_id),
            };
            let variable_id = self.add_variable(variable);
            params.push(FnArgDefn { name: fn_arg.name, variable_id, position: idx, ty: ir_type });
            self.scopes.add_variable(scope_id, fn_arg.name, variable_id);
        }

        let intrinsic_type = if fn_def.is_intrinsic {
            let name = &*self.ast.get_ident_name(fn_def.name);
            IntrinsicFunctionType::from_function_name(name)
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
            ast_id: fn_ast_id,
        };
        let function_id = self.add_function(function);
        self.scopes.add_function(scope_id, fn_def.name, function_id);
        if let Some(intrinsic_type) = intrinsic_type {
            self.scopes.intrinsic_functions.insert(intrinsic_type, function_id);
        }
        let is_intrinsic = intrinsic_type.is_some();
        let body_block = match &fn_def.block {
            Some(block_ast) => {
                let block = self.eval_block(block_ast, scope_id)?;
                if let Err(msg) = self.typecheck_types(given_ret_type, block.ret_type) {
                    return make_fail(
                        format!(
                            "Function {} return type mismatch: {}",
                            &*self.get_ident_name(fn_def.name),
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
    fn eval_definition(&mut self, ast_id: AstId, def: &Definition) -> IrGenResult<()> {
        let scope_id = self.scopes.get_root_scope_id();
        match def {
            Definition::Const(const_val) => {
                let _variable_id: VariableId = self.eval_const(const_val)?;
                Ok(())
            }
            Definition::FnDef(fn_def) => {
                self.eval_function(fn_def, ast_id, scope_id, false)?;
                Ok(())
            }
            Definition::Type(type_defn) => {
                let typ = self.eval_type_expr(&type_defn.value_expr, scope_id)?;
                self.scopes.add_type(scope_id, type_defn.name, typ);
                Ok(())
            }
        }
    }
    pub fn run(&mut self) -> Result<()> {
        let mut errors: Vec<IrGenError> = Vec::new();
        // TODO: 'Declare' everything first, will allow modules
        //        to declare their API without full typechecking
        //        will also allow recursion without hacks

        for (id, defn) in self.ast.clone().defns_iter() {
            let result = self.eval_definition(id, defn);
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

impl Display for IrModule {
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
    use crate::ir::*;
    use crate::parse::parse_text;

    #[test]
    fn const_definition_1() -> anyhow::Result<()> {
        let src = r"val x: int = 420;";
        let module = parse_text(src, "basic_fn.nx", false)?;
        let mut ir = IrModule::new(Rc::new(module));
        ir.run()?;
        let i1 = &ir.constants[0];
        if let IrExpr::Literal(IrLiteral::Int(i, span)) = i1.expr {
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
          return 1;
        }
        fn basic(x: int, y: int): int {
          val x: int = 0; mut y: int = 1;
          y = { 1; 2; 3 };
          y = 42 + 42;
          return foo();
        }"#;
        let module = parse_text(src, "basic_fn.nx", false)?;
        let mut ir = IrModule::new(Rc::new(module));
        ir.run()?;
        println!("{:?}", ir.functions);
        Ok(())
    }
}
