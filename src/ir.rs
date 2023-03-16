use crate::ast::{
    self, Block, BlockStmt, Definition, Expression, FnDef, Literal, Module, TypeExpression, TypePrimitive,
};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

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
pub enum IRType {
    Unset,
    Unit,
    Int,
    String,
    // TypeExpr(Index)
}

#[derive(Debug, Clone)]
pub struct IRBlock {
    instrs_len: usize,
    ret_type: IRType,
}

#[derive(Debug, Clone)]
struct FuncParam {
    name: String,
    typ: IRType,
}

#[derive(Debug, Clone)]
pub enum IrNode {
    Expr(Instr),
    Item(IrItem),
}

#[derive(Debug, Clone)]
pub enum IrItem {
    Func { name: String, params_len: usize, body: Index, ret_type: IRType },
    FuncParam { name: String, ir_type: IRType },
}

#[derive(Debug, Clone)]
pub enum Instr {
    Str(String),
    Int(u64),
    Variable { ir_type: IRType, index: Index },
    ValDef { name: String, ir_type: IRType },
    Assign { dest: Index, ir_type: IRType, value: Index },
    Add(IRType, Index, Index),
    Block(IRBlock),
    Call { callee: Index, args: Index, ret_type: IRType },
    Ret { index: Index, ret_type: IRType },
}

impl Instr {
    fn get_type(&self) -> IRType {
        match self {
            Instr::Str(_) => IRType::String,
            Instr::Int(_) => IRType::Int,
            Instr::Variable { ir_type, .. } => *ir_type,
            Instr::ValDef { .. } => IRType::Unit,
            Instr::Assign { .. } => IRType::Unit,
            Instr::Add(typ, _, _) => *typ,
            Instr::Block(b) => b.ret_type,
            Instr::Call { ret_type, .. } => *ret_type,
            Instr::Ret { ret_type, .. } => *ret_type,
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

pub struct IRGen<'a> {
    ast: &'a Module,
    src: String,
    nodes: Vec<IrNode>,
    top_scope: Scope,
}

struct Scope {
    // TODO: Store indices into interned strings array instead
    members: Vec<(String, Index)>,
    parent: Option<Box<Scope>>,
}
impl Scope {
    fn new() -> Self {
        Scope { members: vec![], parent: None }
    }
    fn find(&self, name: impl AsRef<str>) -> Option<Index> {
        match self.find_local(&name) {
            Some(r) => Some(r),
            None => self.parent.as_ref().and_then(|p| p.find(name)),
        }
    }
    fn find_local(&self, name: impl AsRef<str>) -> Option<Index> {
        self.members.iter().find_map(|(n, r)| if n == name.as_ref() { Some(r.clone()) } else { None })
    }
    fn add(&mut self, name: String, value: Index) {
        self.members.push((name, value));
    }
}

fn simple_err<T: AsRef<str>>(s: T) -> Box<dyn Error> {
    Box::new(IRGenError::from(s.as_ref()))
}

fn simple_fail<A, T: AsRef<str>>(s: T) -> IrGenResult<A> {
    Err(Box::new(IRGenError::from(s.as_ref())))
}

impl<'a> IRGen<'a> {
    pub fn new(module: &Module) -> IRGen {
        return IRGen {
            ast: module,
            src: String::new(),
            nodes: Vec::new(),
            top_scope: Scope { members: Vec::new(), parent: None },
        };
    }

    fn get_instr_type(&self, instr: &Instr) -> IRType {
        instr.get_type()
    }

    fn get_node(&self, idx: Index) -> &IrNode {
        &self.nodes[idx as usize]
    }

    fn get_instr(&self, idx: Index) -> IrGenResult<&Instr> {
        match &self.nodes[idx as usize] {
            IrNode::Expr(expr) => Ok(expr),
            _ => simple_fail(format!("get_instr failed {idx}")),
        }
    }
    fn get_item_mut(&mut self, idx: Index) -> IrGenResult<&mut IrItem> {
        match &mut self.nodes[idx as usize] {
            IrNode::Item(item) => Ok(item),
            _ => simple_fail(format!("get_item_mut failed {idx}")),
        }
    }
    fn eval_type_expr(&self, expr: &TypeExpression, _scope: usize) -> IrGenResult<IRType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IRType::Int),
        }
    }
    fn push_expr_node(&mut self, instr: Instr) -> Index {
        self.nodes.push(IrNode::Expr(instr));
        let index = self.nodes.len() - 1;
        index as u32
    }
    fn push_node(&mut self, node: IrNode) -> Index {
        self.nodes.push(node);
        let index = self.nodes.len() - 1;
        index as u32
    }
    fn eval_const_expr(&mut self, expr: &Expression) -> IrGenResult<Index> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::String(s) => {
                    let instr = Instr::Str(s.clone());
                    let index = self.push_expr_node(instr);
                    Ok(index)
                }
                Literal::Numeric(s) => match u64::from_str(s) {
                    Err(e) => Err(Box::new(IRGenError { msg: e.to_string() })),
                    Ok(u) => {
                        let instr = Instr::Int(u);
                        let index = self.push_expr_node(instr);
                        Ok(index)
                    }
                },
            },
            _ => simple_fail("Only Literals are supported in const expressions"),
        }
    }
    fn eval_expr(&mut self, expr: &Expression, scope: usize) -> IrGenResult<(Instr, Index)> {
        match expr {
            Expression::InfixOp(infix_op) => {
                // Infer expected type to be type of operand1
                let (lhs_instr, lhs_idx) = self.eval_expr(&infix_op.operand1, scope)?;
                let (rhs_instr, rhs_idx) = self.eval_expr(&infix_op.operand2, scope)?;

                let typ = self.get_instr_type(&lhs_instr);
                let typ2 = self.get_instr_type(&rhs_instr);
                if typ != typ2 {
                    return simple_fail("operand types did not match");
                }

                let instr = match infix_op.operation {
                    ast::InfixOpKind::Add => Instr::Add(typ, lhs_idx, rhs_idx),
                    ast::InfixOpKind::Mult => return simple_fail("Mult is unimplemented"),
                };
                let result_idx = self.push_expr_node(instr.clone());
                Ok((instr, result_idx))
            }
            Expression::Literal(Literal::Numeric(s)) => {
                let value_u64: u64 = s.parse()?;
                let instr = Instr::Int(value_u64);
                let index = self.push_expr_node(instr.clone());
                Ok((instr, index))
            }
            Expression::Literal(Literal::String(s)) => {
                let instr = Instr::Str(s.clone());
                let index = self.push_expr_node(instr.clone());
                Ok((instr, index))
            }
            Expression::Variable(ident) => {
                let node_index = self
                    .top_scope
                    .find_local(&ident.0)
                    .ok_or(simple_err(format!("Identifier not found: {}", ident.0)))?;
                let instr = match self.get_node(node_index) {
                    IrNode::Item(IrItem::FuncParam { ir_type, .. }) => {
                        Instr::Variable { ir_type: *ir_type, index: node_index }
                    }
                    IrNode::Expr(Instr::ValDef { ir_type, .. }) => {
                        Instr::Variable { ir_type: *ir_type, index: node_index }
                    }
                    other => panic!("Variable pointed to unsupprted ir node: {:?}", other),
                };
                let index = self.push_expr_node(instr.clone());
                Ok((instr, index))
            }
            Expression::Block(_) => unimplemented!("eval_expr Block"),
            Expression::FnCall(_) => unimplemented!("eval_expr FnCall"),
        }
    }
    fn eval_block_stmt(&mut self, stmt: &BlockStmt, scope: usize) -> IrGenResult<Index> {
        match stmt {
            BlockStmt::ReturnStmt(expr) => {
                let (ret_val, idx) = self.eval_expr(expr, scope)?;
                let ret_inst = Instr::Ret { index: idx, ret_type: ret_val.get_type() };
                let ret_idx = self.push_expr_node(ret_inst);
                Ok(ret_idx)
            }
            BlockStmt::ValDef(val_def) => {
                let (value_instr, idx) = self.eval_expr(&val_def.value, scope)?;
                let provided_type = val_def.typ.as_ref().expect("Type inference not supported on vals yet!");
                let ir_type = self.eval_type_expr(provided_type, scope)?;
                let val_def_instr = Instr::ValDef { ir_type, name: val_def.name.0.clone() };
                let idx = self.push_expr_node(val_def_instr);
                self.top_scope.add(val_def.name.0.clone(), idx);
                Ok(idx)
            }
            BlockStmt::MutDef(_) => simple_fail("Mutable variables are unimplemented"),
            BlockStmt::If(_) => simple_fail("IF expressions are unimplemented"),
            BlockStmt::Assignment(assignment) => {
                let (value_instr, idx) = self.eval_expr(&assignment.expr, scope)?;
                let dest = self
                    .top_scope
                    .find(&assignment.ident.0)
                    .ok_or(simple_err(format!("Identifier not found: {}", &assignment.ident.0)))?;
                let instr = Instr::Assign { value: idx, ir_type: self.get_instr_type(&value_instr), dest };
                let idx = self.push_expr_node(instr);
                Ok(idx)
            }
            BlockStmt::LoneExpression(expression) => {
                let (value_instr, idx) = self.eval_expr(expression, scope)?;
                Ok(idx)
            }
        }
    }
    fn eval_block(&mut self, block: &Block, scope: usize) -> IrGenResult<Index> {
        let start_idx = self.nodes.len();
        for stmt in &block.stmts {
            self.eval_block_stmt(stmt, scope)?;
        }
        // TODO: length is prob wrong here; need a better way to know how many
        // nodes are in the block
        let last_index = self.nodes.len();
        let instr_count = last_index - start_idx;
        // For now return type is inferred to be the return type of the last instruction
        let ret_type =
            if let Some(IrNode::Expr(inst)) = self.nodes.last() { self.get_instr_type(inst) } else { IRType::Unit };

        let index = self.push_expr_node(Instr::Block(IRBlock { ret_type, instrs_len: instr_count }));
        Ok(index)
    }
    fn eval_function_definition(&mut self, fn_def: &FnDef, scope: usize) -> IrGenResult<Index> {
        for fn_arg in &fn_def.args {
            let ir_type = self.eval_type_expr(&fn_arg.typ, scope)?;
            let index = self.push_node(IrNode::Item(IrItem::FuncParam { name: fn_arg.name.0.clone(), ir_type }));
            self.top_scope.add(fn_arg.name.0.clone(), index);
        }
        let fn_item = IrItem::Func {
            name: fn_def.name.0.clone(),
            ret_type: IRType::Unset,
            params_len: fn_def.args.len(),
            body: 0,
        };
        let fn_index = self.push_node(IrNode::Item(fn_item));
        let body_block =
            fn_def.block.as_ref().ok_or(IRGenError::from("Top-level function definitions must have a body"))?;
        // I need to push the block instrs into `instrs`, and push the actual block too to get its
        // index, so I can put its index in the Instr::Func
        let body_index = self.eval_block(body_block, scope)?;
        let body_instr = self.get_instr(body_index)?;
        // let param_block_instr = self.eval_param_block(&fn_def.args, scope)?;
        // If a return type was given in the AST, we need to typecheck it
        let fn_ret_type: IRType = match &fn_def.ret_type {
            None => self.get_instr_type(body_instr),
            Some(given_ret_type) => {
                let given_ir_type = self.eval_type_expr(given_ret_type, scope)?;
                if given_ir_type != self.get_instr_type(body_instr) {
                    return simple_fail(format!(
                        "Function {} ret type mismatch: {:?} {:?}",
                        &fn_def.name.0,
                        given_ir_type,
                        self.get_instr_type(body_instr)
                    ));
                }
                given_ir_type
            }
        };
        let fun_item = self.get_item_mut(fn_index)?;
        if let IrItem::Func { body, ret_type, .. } = fun_item {
            *body = body_index;
            *ret_type = fn_ret_type;
        };
        Ok(fn_index)
    }
    fn eval_definition(&mut self, def: &Definition) -> IrGenResult<Index> {
        match def {
            Definition::Const(const_val) => {
                let typ = self.eval_type_expr(&const_val.typ, 0)?;
                let value: Index = self.eval_const_expr(&const_val.value)?;
                let instr = self.get_instr(value)?;
                let typecheck = instr.get_type() == typ;
                if !typecheck {
                    return simple_fail("failed typecheck of const, i have no source location");
                }
                self.top_scope.add(const_val.name.0.clone(), value);
                Ok(value)
            }
            Definition::FnDef(fn_def) => {
                let scope_index = 0;
                self.eval_function_definition(fn_def, scope_index)
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

impl<'a> Display for IRGen<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Module ")?;
        f.write_str(&self.ast.name.0)?;
        f.write_str(": \n")?;
        for (idx, instr) in self.nodes.iter().enumerate() {
            f.write_fmt(format_args!(":{idx:02} {instr:?}\n"))?;
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
        let src = r#"
        val x: Int = 420;"#;
        let module = parse_text(src, "basic_fn.nx")?;
        let mut ir = IRGen::new(&module);
        ir.run()?;
        let i1 = &ir.nodes[0];
        if let IrNode::Expr(Instr::Int(i)) = *i1 {
            assert_eq!(i, 420);
            Ok(())
        } else {
            panic!("{i1:?} was not an int")
        }
    }

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
        let mut ir = IRGen::new(&module);
        ir.run()?;
        println!("{:?}", ir.nodes);
        Ok(())
    }
}
