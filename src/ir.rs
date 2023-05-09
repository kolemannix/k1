use crate::lex::Span;
use crate::parse;
use crate::parse::{
    Block, BlockStmt, Definition, Expression, FnDef, IdentifierId, Literal, Module,
};
use crate::trace;
use anyhow::{anyhow, bail, Result};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRef {
    Unit,
    Int,
    Bool,
    String,
    TypeId(TypeId),
}

#[derive(Debug, Clone)]
pub struct TypeExpression {
    pub ty: TypeRef,
    pub span: Span,
}

pub enum Type {
    Record(RecordDefn),
    OpaqueAlias(TypeRef),
}

#[derive(Debug, Clone)]
pub struct IrBlock {
    pub ret_type: TypeRef,
    pub scope_id: ScopeId,
    pub statements: Vec<IrStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: IdentifierId,
    pub variable_id: VariableId,
    pub position: usize,
    pub ir_type: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: IdentifierId,
    pub ret_type: TypeRef,
    pub params: Vec<FuncParam>,
    pub block: IrBlock,
    pub intrinsic_type: Option<IntrinsicFunctionType>,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub variable_id: VariableId,
    pub ir_type: TypeRef,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Multiply,
    And,
    Or,
}

impl BinaryOpKind {
    pub fn is_integer_op(&self) -> bool {
        match self {
            BinaryOpKind::Add => true,
            BinaryOpKind::Multiply => true,
            BinaryOpKind::Or | BinaryOpKind::And => true,
            _ => false,
        }
    }
    pub fn is_boolean_op(&self) -> bool {
        match self {
            BinaryOpKind::Or | BinaryOpKind::And => true,
            _ => false,
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
    pub span: Span,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub enum IrLiteral {
    Unit(Span),
    Str(String, Span),
    Int(i64, Span),
    Bool(bool, Span),
    Record(Record),
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
    pub destination_variable: VariableId,
    pub value: IrExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrStmt {
    // TODO: Should we just Box the Exprs since they are big?
    Expr(IrExpr),
    ValDef(ValDef),
    ReturnStmt(ReturnStmt),
    Assignment(Assignment),
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
struct IRGenError {
    msg: String,
}

impl Display for IRGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("IRGenError: ")?;
        f.write_str(&self.msg)
    }
}

impl Error for IRGenError {}

impl From<&str> for IRGenError {
    fn from(s: &str) -> Self {
        IRGenError { msg: s.to_string() }
    }
}

pub type IrGenResult<A> = anyhow::Result<A>;
// expressions dont have ids
// statements dont have ids
// scopes have ids
// types have ids
//

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

pub struct IrModule {
    pub ast: Rc<Module>,
    src: String,
    pub functions: Vec<Function>,
    pub variables: Vec<Variable>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub scopes: Scopes,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    intrinsic_functions: HashMap<FunctionId, IntrinsicFunctionType>,
}

impl Scopes {
    fn make() -> Self {
        let scopes = vec![Scope::default()];
        Scopes { scopes, intrinsic_functions: HashMap::new() }
    }
    fn add_scope(&mut self, parent_scope_id: ScopeId) -> ScopeId {
        let mut scope = Scope::default();
        scope.parent = Some(parent_scope_id);
        let id = self.scopes.len();
        self.scopes.push(scope);
        id as ScopeId
    }

    fn get_root(&self) -> &Scope {
        self.get_scope(0)
    }
    fn get_root_mut(&mut self) -> &mut Scope {
        self.get_scope_mut(0)
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id as usize]
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id as usize]
    }

    fn find_variable(&self, scope: ScopeId, ident: IdentifierId) -> Option<VariableId> {
        let scope = self.get_scope(scope);
        if let v @ Some(r) = scope.find_variable(ident) {
            return v;
        }
        match scope.parent {
            Some(parent) => self.find_variable(parent, ident),
            None => None,
        }
    }

    fn find_variable_local(&self, scope_id: ScopeId, ident: IdentifierId) -> Option<VariableId> {
        self.scopes[scope_id as usize].find_variable(ident)
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
        self.get_scope(scope_id).find_type(ident)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicFunctionType {
    PrintInt,
}

#[derive(Default)]
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

fn simple_err<T: AsRef<str>>(s: T) -> anyhow::Error {
    IRGenError::from(s.as_ref()).into()
}

fn simple_fail<A, T: AsRef<str>>(s: T) -> IrGenResult<A> {
    Err(Box::new(IRGenError::from(s.as_ref())).into())
}

impl IrModule {
    pub fn new(parsed_module: Rc<Module>) -> IrModule {
        let mut module = IrModule {
            ast: parsed_module,
            src: String::new(),
            functions: Vec::new(),
            variables: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            scopes: Scopes::make(),
        };
        // TODO: Would be much better to write a prelude file
        //       in the source lang with some "extern" function definitions
        let println_arg = Variable {
            name: module.ast.ident_id("printInt_value"),
            ir_type: TypeRef::Int,
            is_mutable: false,
            owner_scope: Some(0),
        };
        let println_arg_id = module.add_variable(println_arg);
        let intrinsic_functions = vec![Function {
            name: module.ast.ident_id("printInt"),
            ret_type: TypeRef::Unit,
            params: vec![FuncParam {
                name: module.ast.ident_id("value"),
                variable_id: println_arg_id,
                position: 0,
                ir_type: TypeRef::Int,
            }],
            block: IrBlock {
                ret_type: TypeRef::Unit,
                scope_id: 0,
                statements: Vec::with_capacity(0),
                span: Span::NONE,
            },
            intrinsic_type: Some(IntrinsicFunctionType::PrintInt),
        }];
        for function in intrinsic_functions {
            let name = function.name;
            let function_id = module.add_function(function);
            module.scopes.get_root_mut().add_function(name, function_id);
        }
        module
    }

    pub fn name(&self) -> &str {
        &self.ast.name
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(typ);
        id as u32
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id as usize]
    }

    fn eval_type_expr(
        &mut self,
        expr: &parse::TypeExpression,
        scope_id: ScopeId,
    ) -> IrGenResult<TypeRef> {
        match expr {
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
                ty_ref.ok_or(anyhow!("could not find type for identifier {:?}", ident))
            }
        }
    }

    /// Eventually this will be more restrictive than its sibling eval_type_expr
    fn eval_const_type_expr(
        &self,
        expr: &parse::TypeExpression,
        scope_id: ScopeId,
    ) -> IrGenResult<TypeRef> {
        match expr {
            parse::TypeExpression::Int(_) => Ok(TypeRef::Int),
            parse::TypeExpression::Bool(_) => Ok(TypeRef::Bool),
            parse::TypeExpression::Record(_) => simple_fail("No const records yet"),
            parse::TypeExpression::Name(_, _) => simple_fail("No references allowed in constants"),
        }
    }

    fn typecheck_record(&self, expected: &RecordDefn, actual: &RecordDefn) -> IrGenResult<()> {
        for expected_field in &expected.fields {
            trace!("typechecking record field {:?}", expected_field);
            let Some(matching_field) = actual.fields.iter().find(|f| f.name == expected_field.name) else {
                bail!("expected field {}", expected_field.name)
            };
            self.typecheck_types(matching_field.ty, expected_field.ty)?;
        }
        Ok(())
    }

    fn typecheck_types(&self, expected: TypeRef, actual: TypeRef) -> IrGenResult<()> {
        // Eventually, types can be compatible without being equal
        // While we won't have full-blown inheritance, there will be some shallow subtyping
        // and interfaces
        if expected == actual {
            Ok(())
        } else {
            match (expected, actual) {
                (TypeRef::TypeId(id1), TypeRef::TypeId(id2)) => {
                    let ty1 = self.get_type(id1);
                    let ty2 = self.get_type(id2);
                    match (ty1, ty2) {
                        (Type::Record(r1), Type::Record(r2)) => self.typecheck_record(r1, r2),
                        _ => todo!("typecheck_types: other types"),
                    }
                }
                _ => bail!("Unrelated types failed typecheck"),
            }
        }
    }

    fn eval_const(&mut self, const_expr: &parse::ConstVal) -> IrGenResult<VariableId> {
        let scope_id = 0;
        match const_expr {
            parse::ConstVal { name, ty: typ, value_expr: value, span } => {
                let ir_type = self.eval_const_type_expr(&typ, scope_id)?;
                let num = match value {
                    Expression::Literal(Literal::Numeric(n, _span)) => self.parse_numeric(n)?,
                    other => {
                        return simple_fail("Only literals are currently supported as constants")
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
        }
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

    pub fn get_variable(&self, index: u32) -> &Variable {
        &self.variables[index as usize]
    }

    fn add_function(&mut self, function: Function) -> FunctionId {
        let id = self.functions.len();
        self.functions.push(function);
        id as u32
    }

    pub fn get_function(&self, function_id: FunctionId) -> &Function {
        &self.functions[function_id as usize]
    }

    fn parse_numeric(&self, s: &str) -> IrGenResult<i64> {
        // Eventually we need to find out what type of number literal this is.
        // For now we only support i64
        let num: i64 =
            s.parse().map_err(|e| simple_err("Failed to parse signed numeric literal"))?;
        Ok(num)
    }

    fn transform_expr_to_block(&mut self, expr: IrExpr) -> IrBlock {
        match expr {
            IrExpr::Block(b) => b,
            expr => {
                // TODO: Call create scope, do the right stuff
                let block_scope_id = 0;
                let ret_type = expr.get_type();
                let span = expr.get_span();
                let statement = IrStmt::Expr(expr);
                let statements = vec![statement];

                IrBlock { ret_type, scope_id: block_scope_id, statements, span }
            }
        }
    }

    fn coerce_block_to_unit_block(&mut self, block: &mut IrBlock) -> () {
        let span = block.statements.last().map(|s| s.get_span()).unwrap_or(block.span);
        let unit_literal = IrExpr::unit_literal(span);
        block.statements.push(IrStmt::Expr(unit_literal));
        block.ret_type = TypeRef::Unit;
    }

    // Maybe, pass in expected type for smarter stuff
    fn eval_expr(&mut self, expr: &Expression, scope_id: ScopeId) -> IrGenResult<IrExpr> {
        match expr {
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
                    anyhow::bail!(
                        "If condition must be of type Boolean; but got {:?}",
                        condition.get_type()
                    );
                }
                let consequent_expr = self.eval_expr(&if_expr.cons, scope_id)?;
                // De-sugar if without else:
                // If there is no alternate, we coerce the consequent to return Unit, so both
                // branches have a matching type, making codegen simple and branchless
                let consequent = if if_expr.alt.is_none() {
                    let mut block = self.transform_expr_to_block(consequent_expr);
                    self.coerce_block_to_unit_block(&mut block);
                    block
                } else {
                    self.transform_expr_to_block(consequent_expr)
                };
                let alternate = if let Some(alt) = &if_expr.alt {
                    let expr = self.eval_expr(alt, scope_id)?;
                    self.transform_expr_to_block(expr)
                } else {
                    IrBlock {
                        ret_type: TypeRef::Unit,
                        scope_id,
                        statements: vec![IrStmt::Expr(IrExpr::unit_literal(if_expr.span))],
                        span: if_expr.span,
                    }
                };
                if self.typecheck_types(consequent.ret_type, alternate.ret_type).is_err() {
                    anyhow::bail!(
                        "else branch type {:?} did not match then branch type {:?}",
                        consequent.ret_type,
                        alternate.ret_type
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
                let lhs = self.eval_expr(&binary_op.operand1, scope_id)?;
                let rhs = self.eval_expr(&binary_op.operand2, scope_id)?;

                if self.typecheck_types(lhs.get_type(), rhs.get_type()).is_err() {
                    return simple_fail("operand types did not match");
                }

                let kind = match binary_op.operation {
                    parse::BinaryOpKind::Add => BinaryOpKind::Add,
                    parse::BinaryOpKind::Multiply => BinaryOpKind::Multiply,
                    parse::BinaryOpKind::And => BinaryOpKind::And,
                    parse::BinaryOpKind::Or => BinaryOpKind::Or,
                };
                let expr = IrExpr::BinaryOp(BinaryOp {
                    kind,
                    ir_type: lhs.get_type(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: binary_op.span,
                });
                Ok(expr)
            }
            Expression::Literal(Literal::Numeric(s, span)) => {
                let num = self.parse_numeric(s)?;
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
                let var_index = self
                    .scopes
                    .find_variable(scope_id, variable.ident)
                    .ok_or(simple_err(format!("Identifier not found: {}", variable.ident)))?;
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
                let TypeRef::TypeId(type_id) = base_expr.get_type() else {
                    bail!("Cannot access field {} on non-record type", field_access.target);
                };
                let Type::Record(record_type) = self.get_type(type_id) else {
                    bail!("Cannot access field {} on non-record type", field_access.target);
                };
                let target_field = record_type.find_field(field_access.target).ok_or(
                    simple_err(format!("Field {} not found on record type", field_access.target)),
                )?;
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
                let function_id = self
                    .scopes
                    .find_function(scope_id, fn_call.name)
                    .ok_or(simple_err(format!("Function not found: {}", fn_call.name)))?;
                // TODO: cloning a function
                // Can I 'split the borrow' here by extracting this into a function
                // that doesn't take all of self mutably
                // Not sure if RC or clone is lighter-weight here... it does clone a vec
                let function = self.functions[function_id as usize].clone();
                let mut fn_args: Vec<IrExpr> = Vec::new();
                for fn_param in &function.params {
                    let matching_param_by_name =
                        fn_call.args.iter().find(|arg| arg.name == Some(fn_param.name));
                    let matching_param =
                        matching_param_by_name.or(fn_call.args.get(fn_param.position));
                    if let Some(param) = matching_param {
                        let param_expr = self.eval_expr(&param.value, scope_id)?;
                        fn_args.push(param_expr);
                    } else {
                        return simple_fail("Could not find match for parameter {fn_param.name}");
                    }
                }
                let call = IrExpr::FunctionCall(FunctionCall {
                    callee_function_id: function_id,
                    args: fn_args,
                    ret_type: function.ret_type,
                    span: fn_call.span,
                });
                Ok(call)
            }
        }
    }
    fn eval_block_stmt(&mut self, stmt: &BlockStmt, scope_id: ScopeId) -> IrGenResult<IrStmt> {
        match stmt {
            BlockStmt::ReturnStmt(return_stmt) => {
                let expr = self.eval_expr(&return_stmt.expr, scope_id)?;
                let ret_inst = IrStmt::ReturnStmt(ReturnStmt { expr, span: return_stmt.span });
                Ok(ret_inst)
            }
            BlockStmt::ValDef(val_def) => {
                let value_expr = self.eval_expr(&val_def.value, scope_id)?;
                let provided_type =
                    val_def.ty.as_ref().expect("Type inference not supported on vals yet!");
                let ir_type = self.eval_type_expr(provided_type, scope_id)?;
                let variable_id = self.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name,
                    ir_type,
                    owner_scope: Some(scope_id),
                });
                if let Err(e) = self.typecheck_types(ir_type, value_expr.get_type()) {
                    bail!("local type mismatch: {e}",);
                }
                let val_def_expr = IrStmt::ValDef(ValDef {
                    ir_type,
                    variable_id,
                    initializer: value_expr,
                    span: val_def.span,
                });
                self.scopes.add_variable(scope_id, val_def.name, variable_id);
                Ok(val_def_expr)
            }
            BlockStmt::Assignment(assignment) => {
                let expr = self.eval_expr(&assignment.expr, scope_id)?;
                let dest =
                    self.scopes.find_variable(scope_id, assignment.ident).ok_or(simple_err(
                        format!("Variable {} not found in scope {}", assignment.ident, scope_id),
                    ))?;
                let var = self.get_variable(dest);
                if !var.is_mutable {
                    anyhow::bail!("Cannot assign to immutable variable {}", assignment.ident)
                }
                if self.typecheck_types(var.ir_type, expr.get_type()).is_err() {
                    return simple_fail("Typecheck of assignment failed");
                }
                let expr = IrStmt::Assignment(Assignment {
                    value: expr,
                    destination_variable: dest,
                    span: assignment.span,
                });
                Ok(expr)
            }
            BlockStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(expression, scope_id)?;
                Ok(IrStmt::Expr(expr))
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
    fn eval_function(&mut self, fn_def: &FnDef, scope_id: ScopeId) -> IrGenResult<FunctionId> {
        let mut params = Vec::new();
        let fn_scope = self.scopes.add_scope(scope_id);
        for (idx, fn_arg) in fn_def.args.iter().enumerate() {
            let ir_type = self.eval_type_expr(&fn_arg.ty, scope_id)?;
            let variable = Variable {
                name: fn_arg.name,
                ir_type,
                is_mutable: false,
                owner_scope: Some(scope_id),
            };
            let variable_id = self.add_variable(variable);
            params.push(FuncParam { name: fn_arg.name, variable_id, position: idx, ir_type });
            self.scopes.add_variable(scope_id, fn_arg.name, variable_id);
        }
        let body_block = fn_def
            .block
            .as_ref()
            .ok_or(IRGenError::from("Top-level function definitions must have a body"))?;
        // I need to push the block exprs into `exprs`, and push the actual block too to get its
        // index, so I can put its index in the Expr::Func
        let body_block = self.eval_block(body_block, scope_id)?;
        // If a return type was given in the AST, we need to typecheck it
        let ret_type: TypeRef = match &fn_def.ret_type {
            None => body_block.ret_type,
            Some(given_ret_type) => {
                let given_ir_type = self.eval_type_expr(given_ret_type, scope_id)?;
                if self.typecheck_types(given_ir_type, body_block.ret_type).is_err() {
                    return simple_fail(format!(
                        "Function {} ret type mismatch: {:?} {:?}",
                        fn_def.name, given_ir_type, body_block.ret_type
                    ));
                }
                given_ir_type
            }
        };
        let function = Function {
            name: fn_def.name,
            ret_type,
            params,
            block: body_block,
            intrinsic_type: None,
        };
        let function_id = self.add_function(function);
        self.scopes.add_function(scope_id, fn_def.name, function_id);
        Ok(function_id)
    }
    fn eval_definition(&mut self, def: &Definition) -> IrGenResult<()> {
        let scope_id = 0;
        match def {
            Definition::Const(const_val) => {
                let _variable_id: VariableId = self.eval_const(const_val)?;
                Ok(())
            }
            Definition::FnDef(fn_def) => {
                self.eval_function(fn_def, scope_id)?;
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
        for def in &self.ast.clone().defs {
            self.eval_definition(def)?;
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
    fn const_definition_1() -> Result<(), Box<dyn Error>> {
        let src = r"val x: Int = 420;";
        let module = parse_text(src, "basic_fn.nx")?;
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
    fn fn_definition_1() -> IrGenResult<()> {
        let src = r#"
        fn foo(): Int {
          return 1;
        }
        fn basic(x: Int, y: Int): Int {
          val x: Int = 0; mut y: Int = 1;
          y = { 1; 2; 3 };
          y = 42 + 42;
          return foo();
        }"#;
        let module = parse_text(src, "basic_fn.nx")?;
        let mut ir = IrModule::new(Rc::new(module));
        ir.run()?;
        println!("{:?}", ir.functions);
        Ok(())
    }
}
