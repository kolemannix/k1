use crate::ast::{Block, Definition, Expression, FnDef, Literal, Module, TypeExpression, TypePrimitive};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

type Index = u32;

/// A value, or a reference to an instruction
#[derive(Debug, Clone, Copy)]
pub enum Ref {
    Zero,
    One,
    Instr(Index),
}


#[derive(Debug, Clone, Copy)]
pub enum IRType {
    Unit,
    Int,
    String,
    // TypeInstr(Index)
}

#[derive(Debug, Clone)]
struct IRBlock {
    instrs_len: u32,
    ret_type: IRType,
}

#[derive(Debug, Clone)]
pub enum Instr {
    // A string literal, stored directly in the IR. One day in the strings array
    Str(String),
    // An integer literal
    Int(u64),
    Add(IRType, Ref, Ref),
    Block(IRBlock),
    Func { name: String, param_block: Index, body: Index, ret_type: IRType },
    Alloc { typ: IRType },
    Store { dest: Index, value: Ref },
    Call { callee: Index, args: Index },
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
    instrs: Vec<Instr>,
    top_scope: Scope,
}

struct Scope {
    // TODO: Store indices into interned strings array instead
    members: Vec<(String, Ref)>,
    parent: Option<Box<Scope>>,
}
impl Scope {
    fn find(&self, name: impl AsRef<str>) -> Option<Ref> {
        match self.find_local(&name) {
            Some(r) => Some(r),
            None => self.parent.as_ref().and_then(|p| p.find(name)),
        }
    }
    fn find_local(&self, name: impl AsRef<str>) -> Option<Ref> {
        self.members.iter().find_map(|(n, r)| if n == name.as_ref() { Some(r.clone()) } else { None })
    }
    fn add(&mut self, name: String, value: Ref) -> IrGenResult<()> {
        self.members.push((name, value));
        Ok(())
    }
}

fn simple_error(s: &str) -> Box<IRGenError> {
    Box::new(IRGenError::from(s))
}

impl<'a> IRGen<'a> {
    fn new(module: &Module) -> IRGen {
        return IRGen {
            ast: module,
            src: String::new(),
            instrs: Vec::new(),
            top_scope: Scope { members: Vec::new(), parent: None },
        };
    }
    fn get_instr(&self, idx: Index) -> &Instr {
        &self.instrs[idx as usize]
    }
    fn eval_type_expr(&self, expr: &TypeExpression, _scope: &Scope) -> IrGenResult<IRType> {
        match expr {
            TypeExpression::Primitive(TypePrimitive::Int) => Ok(IRType::Int),
        }
    }
    fn get_ref_type(&self, r: Ref) -> IRType {
        match r {
            Ref::Zero => IRType::Int,
            Ref::One => IRType::Int,
            Ref::Instr(idx) => {
                let instr = self.get_instr(idx);
                self.get_instr_type(instr)
            },
        }
    }
    fn get_instr_type(&self, instr: &Instr) -> IRType {
        match instr {
            Instr::Str(_) => IRType::String,
            Instr::Int(_) => IRType::Int,
            Instr::Add(typ, r1, r2) => typ.clone(),
            Instr::Block(b) => b.ret_type.clone(),
            Instr::Func { ret_type, .. } => ret_type.clone(),
            Instr::Alloc { .. } => IRType::Unit,
            Instr::Store { .. } => IRType::Unit,
            Instr::Call { callee, .. } => {
                let callee = self.get_instr(*callee);
                if let Instr::Func { ret_type , .. } = *callee {
                    ret_type
                } else {
                    IRType::Unit
                }
            },

        }
    }
    fn check_type(&self, typ: &IRType, value: Ref) -> bool {
        match typ {
            IRType::Unit => match value {

            },
            IRType::Int => match value {
                Ref::Zero => true,
                Ref::One => true,
                Ref::Instr(idx) => {
                    let instr = &self.instrs[idx as usize];
                    match instr {
                        Instr::Int(_) => true,
                        Instr::Func { ret_type, .. } => self.check_type(ret_type, value),
                        _ => false,
                    }
                }
            },
            IRType::String => match value {
                Ref::Instr(idx) => {
                    let instr = &self.instrs[idx as usize];
                    match instr {
                        Instr::Str(_) => true,
                        Instr::Func { ret_type, .. } => self.check_type(ret_type, value),
                        _ => false,
                    }
                }
                _ => false,
            },
        }
    }
    fn push_instr(&mut self, instr: Instr) -> Index {
        self.instrs.push(instr);
        let index = self.instrs.len() - 1;
        index as u32
    }
    fn eval_const_expr(&mut self, expr: &Expression) -> IrGenResult<Ref> {
        return match expr {
            Expression::Literal(lit) => match lit {
                Literal::String(s) => {
                    let instr = Instr::Str(s.clone());
                    let index = self.push_instr(instr);
                    Ok(Ref::Instr(index))
                }
                Literal::Numeric(s) => match u64::from_str(&s) {
                    Err(e) => Err(Box::new(IRGenError { msg: e.to_string() })),
                    Ok(u) => {
                        let instr = Instr::Int(u);
                        let index = self.push_instr(instr);
                        Ok(Ref::Instr(index))
                    }
                },
            },
            _ => Err(simple_error("Only Literals are supported in const expressions")),
        };
    }
    fn eval_block(&mut self, block: &Block, scope: &Scope) -> IrGenResult<IRBlock> {
        let mut instrs: Vec<Instr> = Vec::new();
        for stmt in &block.stmts {
            let instr = self.eval_block_stmt(stmt, scope)?;
            instrs.push(instr);
        }
        // For now return type is inferred to be the return type of the last instruction
        let ret_type = if let Some(inst) = instrs.last() {
            self.get_type(inst)
        } else { IRType::Unit }
        Ok(IRBlock { ret_type: })
    }
    fn eval_function_definition(&mut self, fn_def: FnDef, scope: &Scope) -> IrGenResult<Ref> {
        let body_block = fn_def.block.ok_or(IRGenError::from("Top-level function definitions must have a body"))?;
        let body_instr = self.eval_block(&body_block);
        let fn_instr = Instr::Func {
            name: fn_def.name.0.clone(),
            ret_type: fn_ret_type,
            param_block: param_block_index,
            body: body_index,
        };
        Err(simple_error("unimplemented FnDef"))
    }
    fn eval_definition(&mut self, def: &Definition) -> IrGenResult<Ref> {
        match def {
            Definition::Const(const_val) => {
                let typ = self.eval_type_expr(&const_val.typ, &self.top_scope)?;
                let value: Ref = self.eval_const_expr(&const_val.value)?;
                let typecheck = self.check_type(&typ, value);
                if !typecheck {
                    return Err(simple_error("failed typecheck of const, i have no source location"));
                }
                self.top_scope.add(const_val.name.0.clone(), value.clone())?;
                Ok(value.clone())
            }
            Definition::FnDef(fn_def) => self.eval_function_definition(fn_def, &self.top_scope),
        }
    }
    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        for def in &self.ast.defs {
            self.eval_definition(def)?;
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
        let i1 = &ir.instrs[0];
        if let Instr::Int(i) = *i1 {
            assert_eq!(i, 420);
            Ok(())
        } else {
            panic!("{:?} was not an int", i1)
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
        println!("{:?}", ir.instrs);
        Ok(())
    }
}
