use crate::ast::{
    self, Block, BlockStmt, Definition, Expression, FnDef, Literal, Module, TypeExpression, TypePrimitive,
};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

type ScopeId = u32;
type Index = u32;

/// TODO: Add Ref, an optimization that allows the compiler
/// to know about certain values, like 0, 1, 2, false, true
/// and many others in order to enable a lot of fun compile time
/// execution and stuff.
///
/// The idea is that instead of always being a reference to an
/// instantiated instruction,
/// a Ref can also be an actual compiler-internal Value, like Zero or false.
// #[derive(Debug, Clone, Copy)]
// #[allow(dead_code)]
// pub enum Ref {
//     Zero,
//     One,
//     Instr(Index),
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IrType {
    Unit,
    Int,
    String,
    // TypeExpr(Index)
}

#[derive(Debug, Clone)]
pub struct IRBlock {
    ret_type: IrType,
    scope_id: ScopeId,
    pub statements: Vec<IrStmt>,
}

#[derive(Debug, Clone)]
struct FuncParam {
    name: String,
    ir_type: IrType,
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    ret_type: IrType,
    params: Vec<FuncParam>,
    block: IRBlock,
}

// LLVM
// Global identifiers (functions, global variables) begin with the '@' character.
// Local identifiers (register names, types) begin with the '%' character
// functions have a scope id, and an owner scope id (optional?)
#[derive(Debug, Clone)]
pub enum IrExpr {
    Str(String),
    Int(u64),
    Variable { variable_id: Index, ir_type: IrType },
    Add(IrType, Box<IrExpr>, Box<IrExpr>),
    Block(IRBlock),
    Call { callee: Index, args: Index, ret_type: IrType },
}

#[derive(Debug, Clone)]
pub enum IrStmt {
    // TODO: Should we just Box the Exprs since they are big?
    Expr(IrExpr),
    ValDef { variable_id: Index, ir_type: IrType, initial_value: IrExpr },
    Ret { expr: IrExpr },
    Assign { dest: Index, value: IrExpr },
}

impl IrExpr {
    fn get_type(&self) -> IrType {
        match self {
            IrExpr::Str(_) => IrType::String,
            IrExpr::Int(_) => IrType::Int,
            IrExpr::Variable { ir_type, .. } => *ir_type,
            IrExpr::Add(typ, _, _) => *typ,
            IrExpr::Block(b) => b.ret_type,
            IrExpr::Call { ret_type, .. } => *ret_type,
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

pub type IrGenResult<A> = Result<A, Box<dyn Error>>;
// expressions dont have ids
// statements dont have ids
// scopes have ids
// types have ids
//

#[derive(Debug)]
pub struct Variable {
    name: String,
    ir_type: IrType,
    is_mutable: bool,
}

#[derive(Debug)]
pub struct Constant {
    variable_id: Index,
    expr: IrExpr,
    ir_type: IrType,
}

pub struct IRModule<'a> {
    pub ast: &'a Module,
    src: String,
    pub functions: Vec<Function>,
    pub variables: Vec<Variable>,
    pub constants: Vec<Constant>,
    pub scopes: Vec<Scope>,
}

#[derive(Default)]
pub struct Scope {
    variables: HashMap<String, Index>,
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
}

fn simple_err<T: AsRef<str>>(s: T) -> Box<dyn Error> {
    Box::new(IRGenError::from(s.as_ref()))
}

fn simple_fail<A, T: AsRef<str>>(s: T) -> IrGenResult<A> {
    Err(Box::new(IRGenError::from(s.as_ref())))
}

impl<'a> IRModule<'a> {
    pub fn new(module: &Module) -> IRModule {
        IRModule {
            ast: module,
            src: String::new(),
            functions: Vec::new(),
            variables: Vec::new(),
            constants: Vec::new(),
            scopes: vec![Scope::default()],
        }
    }

    fn eval_type_expr(&self, expr: &TypeExpression, scope_id: usize) -> IrGenResult<IrType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IrType::Int),
        }
    }

    /// Eventually this will be more restrictive than its sibling eval_type_expr
    fn eval_const_type_expr(&self, expr: &TypeExpression, scope_id: usize) -> IrGenResult<IrType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IrType::Int),
        }
    }

    fn eval_const(&mut self, const_expr: &ast::ConstVal) -> IrGenResult<Index> {
        let scope_id = 0;
        match const_expr {
            ast::ConstVal { name, typ, value } => {
                let ir_type = self.eval_const_type_expr(&typ, scope_id)?;
                let expr = match value {
                    Expression::Literal(Literal::Numeric(n)) => self.parse_numeric(n)?,
                    other => return simple_fail("Only literals are currently supported as constants"),
                };
                // TODO: Store expr somewhere?
                let variable_id = self.add_variable(Variable { name: name.0.clone(), ir_type, is_mutable: false });
                self.constants.push(Constant { variable_id, expr, ir_type });
                self.scopes[scope_id].add_variable(name.0.clone(), variable_id);
                Ok(variable_id)
            }
        }
    }

    fn get_stmt_return_type(&self, stmt: &IrStmt) -> Option<IrType> {
        match stmt {
            IrStmt::Expr(expr) => Some(expr.get_type()),
            IrStmt::Ret { expr } => Some(expr.get_type()),
            _ => None,
        }
    }

    fn add_variable(&mut self, variable: Variable) -> Index {
        let id = self.variables.len();
        self.variables.push(variable);
        id as u32
    }

    fn get_variable(&self, index: u32) -> &Variable {
        &self.variables[index as usize]
    }

    fn add_function(&mut self, function: Function) -> Index {
        let id = self.functions.len();
        self.functions.push(function);
        id as u32
    }

    fn parse_numeric(&self, s: &str) -> IrGenResult<IrExpr> {
        // Eventually we need to find out what type of number literal this is.
        // For now we only support u64
        let num: u64 = s.parse().map_err(|e| simple_err("Failed to parse numeric literal"))?;
        Ok(IrExpr::Int(num))
    }

    fn eval_expr(&mut self, expr: &Expression, scope: usize) -> IrGenResult<IrExpr> {
        match expr {
            Expression::InfixOp(infix_op) => {
                // Infer expected type to be type of operand1
                let lhs = self.eval_expr(&infix_op.operand1, scope)?;
                let rhs = self.eval_expr(&infix_op.operand2, scope)?;

                if lhs.get_type() != rhs.get_type() {
                    return simple_fail("operand types did not match");
                }

                let expr = match infix_op.operation {
                    ast::InfixOpKind::Add => IrExpr::Add(lhs.get_type(), Box::new(lhs), Box::new(rhs)),
                    ast::InfixOpKind::Mult => return simple_fail("Mult is unimplemented"),
                };
                Ok(expr)
            }
            Expression::Literal(Literal::Numeric(s)) => {
                let expr = self.parse_numeric(&s)?;
                Ok(expr)
            }
            Expression::Literal(Literal::String(s)) => {
                let expr = IrExpr::Str(s.clone());
                Ok(expr)
            }
            Expression::Variable(ident) => {
                let var_index = self.scopes[scope]
                    .find_variable(&ident.0)
                    .ok_or(simple_err(format!("Identifier not found: {}", ident.0)))?;
                let v = self.get_variable(var_index);
                let expr = IrExpr::Variable { ir_type: v.ir_type, variable_id: var_index };
                Ok(expr)
            }
            Expression::Block(_) => unimplemented!("eval_expr Block"),
            Expression::FnCall(_) => unimplemented!("eval_expr FnCall"),
        }
    }
    fn eval_block_stmt(&mut self, stmt: &BlockStmt, scope_id: usize) -> IrGenResult<IrStmt> {
        match stmt {
            BlockStmt::ReturnStmt(expr) => {
                let expr = self.eval_expr(expr, scope_id)?;
                let ret_inst = IrStmt::Ret { expr };
                Ok(ret_inst)
            }
            BlockStmt::ValDef(val_def) => {
                let value_expr = self.eval_expr(&val_def.value, scope_id)?;
                let provided_type = val_def.typ.as_ref().expect("Type inference not supported on vals yet!");
                let ir_type = self.eval_type_expr(provided_type, scope_id)?;
                let variable_id =
                    self.add_variable(Variable { is_mutable: false, name: val_def.name.0.clone(), ir_type });
                let val_def_expr = IrStmt::ValDef { ir_type, variable_id, initial_value: value_expr };
                self.scopes[scope_id].add_variable(val_def.name.0.clone(), variable_id);
                Ok(val_def_expr)
            }
            BlockStmt::MutDef(_) => simple_fail("Mutable variables are unimplemented"),
            BlockStmt::If(_) => simple_fail("IF expressions are unimplemented"),
            BlockStmt::Assignment(assignment) => {
                let expr = self.eval_expr(&assignment.expr, scope_id)?;
                let dest = self.scopes[scope_id]
                    .find_recursive(&assignment.ident)
                    .ok_or(simple_err(format!("Identifier not found: {}", &assignment.ident.0)))?;
                let var = self.get_variable(dest);
                if var.ir_type != expr.get_type() {
                    return simple_fail("Typecheck of assignment failed");
                }
                let expr = IrStmt::Assign { value: expr, dest };
                Ok(expr)
            }
            BlockStmt::LoneExpression(expression) => {
                let expr = self.eval_expr(expression, scope_id)?;
                Ok(IrStmt::Expr(expr))
            }
        }
    }
    fn eval_block(&mut self, block: &Block, scope: usize) -> IrGenResult<IRBlock> {
        let mut statements = Vec::new();
        for stmt in &block.stmts {
            let stmt = self.eval_block_stmt(stmt, scope)?;
            statements.push(stmt);
        }
        // TODO: The return type of a block is actually an interesting problem, need to think about
        // branching, early returns, etc!
        // For now return type is inferred to be the return type of the last instruction
        let ret_type = if let Some(stmt) = statements.last() {
            self.get_stmt_return_type(stmt).expect("last block statement should have a return type")
        } else {
            IrType::Unit
        };
        let ir_block = IRBlock { ret_type, scope_id: 0, statements };
        Ok(ir_block)
    }
    fn eval_function(&mut self, fn_def: &FnDef, scope_id: usize) -> IrGenResult<Index> {
        let mut params = Vec::new();
        for fn_arg in &fn_def.args {
            let ir_type = self.eval_type_expr(&fn_arg.typ, scope_id)?;
            let variable = Variable { name: fn_arg.name.0.clone(), ir_type, is_mutable: false };
            let variable_id = self.add_variable(variable);
            params.push(FuncParam { name: fn_arg.name.0.clone(), ir_type });
            self.scopes[scope_id].add_variable(fn_arg.name.0.clone(), variable_id);
        }
        let body_block =
            fn_def.block.as_ref().ok_or(IRGenError::from("Top-level function definitions must have a body"))?;
        // I need to push the block exprs into `exprs`, and push the actual block too to get its
        // index, so I can put its index in the Expr::Func
        let body_block = self.eval_block(body_block, scope_id)?;
        // If a return type was given in the AST, we need to typecheck it
        let ret_type: IrType = match &fn_def.ret_type {
            None => body_block.ret_type,
            Some(given_ret_type) => {
                let given_ir_type = self.eval_type_expr(given_ret_type, scope_id)?;
                if given_ir_type != body_block.ret_type {
                    return simple_fail(format!(
                        "Function {} ret type mismatch: {:?} {:?}",
                        &fn_def.name.0, given_ir_type, body_block.ret_type
                    ));
                }
                given_ir_type
            }
        };
        let function = Function { name: fn_def.name.0.clone(), ret_type, params, block: body_block };
        let function_id = self.add_function(function);
        Ok(function_id)
    }
    fn eval_definition(&mut self, def: &Definition) -> IrGenResult<()> {
        match def {
            Definition::Const(const_val) => {
                let variable_id: Index = self.eval_const(const_val)?;
                Ok(())
            }
            Definition::FnDef(fn_def) => {
                let scope_index = 0;
                self.eval_function(fn_def, scope_index)?;
                Ok(())
            }
        }
    }
    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        for def in &self.ast.defs {
            self.eval_definition(def)?;
        }
        Ok(())
    }
}

impl<'a> Display for IRModule<'a> {
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

    // #[test]
    // fn const_definition_1() -> Result<(), Box<dyn Error>> {
    //     let src = r#"
    //     val x: Int = 420;"#;
    //     let module = parse_text(src, "basic_fn.nx")?;
    //     let mut ir = IRModule::new(&module);
    //     ir.run()?;
    //     let i1 = &ir.nodes[0];
    //     if let IrNode::Expr(IrExpr::Int(i)) = *i1 {
    //         assert_eq!(i, 420);
    //         Ok(())
    //     } else {
    //         panic!("{i1:?} was not an int")
    //     }
    // }

    #[test]
    fn fn_definition_1() -> IrGenResult<()> {
        let src = r#"
        fn basic(x: Int, y: Int): Int {
          println(42, 42, 42);
          val x: Int = 0;
          mut y: Int = 1;
          y = { 1; 2; 3 };
          y = add(42, 42);
          return add(x, y);
        }"#;
        let module = parse_text(src, "basic_fn.nx")?;
        let mut ir = IRModule::new(&module);
        ir.run()?;
        println!("{:?}", ir.functions);
        Ok(())
    }
}
