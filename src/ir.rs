use crate::ast::{
    self, Block, BlockStmt, Definition, Expression, FnDef, Literal, Module, TypeExpression,
    TypePrimitive,
};
use crate::lex::Span;
use anyhow::Result;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type ScopeId = u32;
pub type FunctionId = u32;
pub type VariableId = u32;
pub type Index = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IrType {
    Unit,
    Int,
    Bool,
    String,
    // TypeExpr(Index)
}

#[derive(Debug, Clone)]
pub struct IrBlock {
    pub ret_type: IrType,
    pub scope_id: ScopeId,
    pub statements: Vec<IrStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: String,
    pub variable_id: VariableId,
    pub position: usize,
    pub ir_type: IrType,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub ret_type: IrType,
    pub params: Vec<FuncParam>,
    pub block: IrBlock,
    pub is_intrinsic: bool,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub variable_id: Index,
    pub ir_type: IrType,
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
    pub ir_type: IrType,
    pub lhs: Box<IrExpr>,
    pub rhs: Box<IrExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub callee_function_id: FunctionId,
    pub args: Vec<IrExpr>,
    pub ret_type: IrType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrLiteral {
    Unit(Span),
    Str(String, Span),
    Int(i64, Span),
    Bool(bool, Span),
}

impl IrLiteral {
    #[inline]
    pub fn get_span(&self) -> Span {
        match self {
            IrLiteral::Unit(span) => *span,
            IrLiteral::Str(_, span) => *span,
            IrLiteral::Int(_, span) => *span,
            IrLiteral::Bool(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrIf {
    pub condition: IrExpr,
    pub consequent: IrBlock,
    pub alternate: IrBlock,
    pub ir_type: IrType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IrExpr {
    Literal(IrLiteral),
    Variable(VariableExpr),
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
    pub fn get_type(&self) -> IrType {
        match self {
            IrExpr::Literal(IrLiteral::Unit(_)) => IrType::Unit,
            IrExpr::Literal(IrLiteral::Str(_, _)) => IrType::String,
            IrExpr::Literal(IrLiteral::Int(_, _)) => IrType::Int,
            IrExpr::Literal(IrLiteral::Bool(_, _)) => IrType::Bool,
            IrExpr::Variable(var) => var.ir_type,
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
    pub ir_type: IrType,
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
    pub dest: VariableId,
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
    pub name: String,
    pub ir_type: IrType,
    pub is_mutable: bool,
    pub owner_scope: Option<ScopeId>,
}

#[derive(Debug)]
pub struct Constant {
    pub variable_id: Index,
    pub expr: IrExpr,
    pub ir_type: IrType,
    pub span: Span,
}

pub struct IrModule {
    pub ast: Rc<Module>,
    src: String,
    pub functions: Vec<Function>,
    pub variables: Vec<Variable>,
    pub constants: Vec<Constant>,
    pub scopes: Vec<Scope>,
}

#[derive(Default)]
pub struct Scope {
    variables: HashMap<String, VariableId>,
    functions: HashMap<String, FunctionId>,
    parent: Option<Box<Scope>>,
}
impl Scope {
    fn find_recursive(&self, name: impl AsRef<str>) -> Option<Index> {
        match self.find_variable(&name) {
            Some(r) => Some(r),
            None => self.parent.as_ref().and_then(|p| p.find_recursive(name)),
        }
    }
    fn find_variable(&self, name: impl AsRef<str>) -> Option<Index> {
        self.variables.get(name.as_ref()).copied()
    }
    fn add_variable(&mut self, name: String, value: Index) {
        self.variables.insert(name, value);
    }

    fn add_function(&mut self, name: String, function_id: Index) {
        self.functions.insert(name, function_id);
    }

    fn find_function(&self, name: impl AsRef<str>) -> Option<Index> {
        self.functions.get(name.as_ref()).copied()
    }
}

fn simple_err<T: AsRef<str>>(s: T) -> anyhow::Error {
    IRGenError::from(s.as_ref()).into()
}

fn simple_fail<A, T: AsRef<str>>(s: T) -> IrGenResult<A> {
    Err(Box::new(IRGenError::from(s.as_ref())).into())
}

impl IrModule {
    pub fn new(module: Rc<Module>) -> IrModule {
        let mut module = IrModule {
            ast: module,
            src: String::new(),
            functions: Vec::new(),
            variables: Vec::new(),
            constants: Vec::new(),
            scopes: vec![Scope::default()],
        };
        // TODO: Would be much better to write a prelude file
        // in the source lang with some "extern" function definitions
        let println_arg = Variable {
            name: "println_value".to_string(),
            ir_type: IrType::Int,
            is_mutable: false,
            owner_scope: Some(0),
        };
        let println_arg_id = module.add_variable(println_arg);
        let intrinsic_functions = vec![Function {
            name: "println".to_string(),
            ret_type: IrType::Unit,
            params: vec![FuncParam {
                name: "value".to_string(),
                variable_id: println_arg_id,
                position: 0,
                ir_type: IrType::Int,
            }],
            block: IrBlock {
                ret_type: IrType::Unit,
                scope_id: 0,
                statements: Vec::with_capacity(0),
                span: Span::NONE,
            },
            is_intrinsic: true,
        }];
        for function in intrinsic_functions {
            let name = function.name.clone();
            let function_id = module.add_function(function);
            module.scopes[0].add_function(name, function_id);
        }
        module
    }

    pub fn name(&self) -> &str {
        &self.ast.name.0
    }

    fn add_variable_to_scope(&mut self, scope_id: ScopeId, name: String, variable_id: VariableId) {
        self.scopes[scope_id as usize].add_variable(name, variable_id);
    }

    fn find_variable_in_scope(&self, scope_id: u32, name: impl AsRef<str>) -> Option<VariableId> {
        self.scopes[scope_id as usize].find_variable(name)
    }

    fn eval_type_expr(&self, expr: &TypeExpression, scope_id: ScopeId) -> IrGenResult<IrType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IrType::Int),
            TypeExpression::Primitive(TypePrimitive::Bool) => Ok(IrType::Bool),
        }
    }

    /// Eventually this will be more restrictive than its sibling eval_type_expr
    fn eval_const_type_expr(
        &self,
        expr: &TypeExpression,
        scope_id: ScopeId,
    ) -> IrGenResult<IrType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IrType::Int),
            TypeExpression::Primitive(TypePrimitive::Bool) => Ok(IrType::Bool),
        }
    }

    fn is_valid_type(&self, source: IrType, destination: IrType) -> bool {
        // Eventually, types can be compatible without being equal
        // While we won't have full-blown inheritance, there will be some shallow subtyping
        // and interfaces
        source == destination
    }

    fn eval_const(&mut self, const_expr: &ast::ConstVal) -> IrGenResult<VariableId> {
        let scope_id = 0;
        match const_expr {
            ast::ConstVal { name, typ, value_expr: value, span } => {
                let ir_type = self.eval_const_type_expr(&typ, scope_id)?;
                let num = match value {
                    Expression::Literal(Literal::Numeric(n, _span)) => self.parse_numeric(n)?,
                    other => {
                        return simple_fail("Only literals are currently supported as constants")
                    }
                };
                let expr = IrExpr::Literal(IrLiteral::Int(num, *span));
                let variable_id = self.add_variable(Variable {
                    name: name.0.clone(),
                    ir_type,
                    is_mutable: false,
                    owner_scope: None,
                });
                self.constants.push(Constant { variable_id, expr, ir_type, span: *span });
                self.add_variable_to_scope(scope_id, name.0.clone(), variable_id);
                Ok(variable_id)
            }
        }
    }

    fn get_stmt_return_type(&self, stmt: &IrStmt) -> Option<IrType> {
        match stmt {
            IrStmt::Expr(expr) => Some(expr.get_type()),
            IrStmt::ReturnStmt(ret) => Some(ret.expr.get_type()),
            _ => None,
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
        block.ret_type = IrType::Unit;
    }

    fn eval_expr(&mut self, expr: &Expression, scope_id: ScopeId) -> IrGenResult<IrExpr> {
        match expr {
            Expression::If(if_expr) => {
                // Ensure boolean condition (or optional which isn't built yet)
                let condition = self.eval_expr(&if_expr.cond, scope_id)?;
                if !self.is_valid_type(condition.get_type(), IrType::Bool) {
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
                        ret_type: IrType::Unit,
                        scope_id,
                        statements: vec![IrStmt::Expr(IrExpr::unit_literal(if_expr.span))],
                        span: if_expr.span,
                    }
                };
                if !self.is_valid_type(consequent.ret_type, alternate.ret_type) {
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

                if !self.is_valid_type(lhs.get_type(), rhs.get_type()) {
                    return simple_fail("operand types did not match");
                }

                let kind = match binary_op.operation {
                    ast::BinaryOpKind::Add => BinaryOpKind::Add,
                    ast::BinaryOpKind::Multiply => BinaryOpKind::Multiply,
                    ast::BinaryOpKind::And => BinaryOpKind::And,
                    ast::BinaryOpKind::Or => BinaryOpKind::Or,
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
                    .find_variable_in_scope(scope_id, &variable.ident)
                    .ok_or(simple_err(format!("Identifier not found: {}", variable.ident.0)))?;
                let v = self.get_variable(var_index);
                let expr = IrExpr::Variable(VariableExpr {
                    ir_type: v.ir_type,
                    variable_id: var_index,
                    span: variable.span,
                });
                Ok(expr)
            }
            Expression::Block(block) => {
                let block = self.eval_block(block, scope_id)?;
                Ok(IrExpr::Block(block))
            }
            Expression::FnCall(fn_call) => {
                let function_id = self.scopes[scope_id as usize]
                    .find_function(&fn_call.name)
                    .ok_or(simple_err(format!("Function not found: {}", fn_call.name.as_ref())))?;
                // TODO: cloning a function
                // Not sure if RC or clone is lighter-weight here... it does clone a vec
                let function = self.functions[function_id as usize].clone();
                let mut fn_args: Vec<IrExpr> = Vec::new();
                for fn_param in &function.params {
                    let matching_param_by_name =
                        fn_call.args.iter().find(|arg| arg.name.as_ref() == Some(&fn_param.name));
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
                    val_def.typ.as_ref().expect("Type inference not supported on vals yet!");
                let ir_type = self.eval_type_expr(provided_type, scope_id)?;
                let variable_id = self.add_variable(Variable {
                    is_mutable: val_def.is_mutable,
                    name: val_def.name.0.clone(),
                    ir_type,
                    owner_scope: Some(scope_id),
                });
                let val_def_expr = IrStmt::ValDef(ValDef {
                    ir_type,
                    variable_id,
                    initializer: value_expr,
                    span: val_def.span,
                });
                self.add_variable_to_scope(scope_id, val_def.name.0.clone(), variable_id);
                Ok(val_def_expr)
            }
            BlockStmt::Assignment(assignment) => {
                let expr = self.eval_expr(&assignment.expr, scope_id)?;
                let dest =
                    self.find_variable_in_scope(scope_id, &assignment.ident).ok_or(simple_err(
                        format!("Variable {} not found in scope {}", assignment.ident.0, scope_id),
                    ))?;
                let var = self.get_variable(dest);
                if !self.is_valid_type(var.ir_type, expr.get_type()) {
                    return simple_fail("Typecheck of assignment failed");
                }
                let expr =
                    IrStmt::Assignment(Assignment { value: expr, dest, span: assignment.span });
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
            IrType::Unit
        };
        let ir_block = IrBlock { ret_type, scope_id: 0, statements, span: block.span };
        Ok(ir_block)
    }
    fn eval_function(&mut self, fn_def: &FnDef, scope_id: ScopeId) -> IrGenResult<Index> {
        let mut params = Vec::new();
        for (idx, fn_arg) in fn_def.args.iter().enumerate() {
            let ir_type = self.eval_type_expr(&fn_arg.typ, scope_id)?;
            let variable = Variable {
                name: fn_arg.name.0.clone(),
                ir_type,
                is_mutable: false,
                owner_scope: Some(scope_id),
            };
            let variable_id = self.add_variable(variable);
            params.push(FuncParam {
                name: fn_arg.name.0.clone(),
                variable_id,
                position: idx,
                ir_type,
            });
            self.add_variable_to_scope(scope_id, fn_arg.name.0.clone(), variable_id);
        }
        let body_block = fn_def
            .block
            .as_ref()
            .ok_or(IRGenError::from("Top-level function definitions must have a body"))?;
        // I need to push the block exprs into `exprs`, and push the actual block too to get its
        // index, so I can put its index in the Expr::Func
        let body_block = self.eval_block(body_block, scope_id)?;
        // If a return type was given in the AST, we need to typecheck it
        let ret_type: IrType = match &fn_def.ret_type {
            None => body_block.ret_type,
            Some(given_ret_type) => {
                let given_ir_type = self.eval_type_expr(given_ret_type, scope_id)?;
                if !self.is_valid_type(given_ir_type, body_block.ret_type) {
                    return simple_fail(format!(
                        "Function {} ret type mismatch: {:?} {:?}",
                        &fn_def.name.0, given_ir_type, body_block.ret_type
                    ));
                }
                given_ir_type
            }
        };
        let function = Function {
            name: fn_def.name.0.clone(),
            ret_type,
            params,
            block: body_block,
            is_intrinsic: false,
        };
        let function_id = self.add_function(function);
        self.scopes[scope_id as usize].add_function(fn_def.name.0.clone(), function_id);
        Ok(function_id)
    }
    fn eval_definition(&mut self, def: &Definition) -> IrGenResult<()> {
        match def {
            Definition::Const(const_val) => {
                let variable_id: VariableId = self.eval_const(const_val)?;
                Ok(())
            }
            Definition::FnDef(fn_def) => {
                let scope_index = 0;
                self.eval_function(fn_def, scope_index)?;
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
        f.write_str(&self.ast.name.0)?;
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
