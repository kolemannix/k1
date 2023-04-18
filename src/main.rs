use std::env;
use std::rc::Rc;

mod ast;
//mod codegen;
mod codegen_ir;
mod ir;
mod lex;
mod output;
mod parse;
#[cfg(test)]
mod test_suite;

use anyhow::Result;
use inkwell::context::Context;
use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;

use crate::codegen_ir::Codegen;

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

pub fn compile_single_file_program<'ctx>(
    ctx: &'ctx Context,
    filename: &str,
    source: &String,
) -> Result<Codegen<'ctx>> {
    let ast = parse::parse_text(&source, filename).unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}': {:?}", filename, e);
        panic!("parse error");
    });
    let ast = Rc::new(ast);
    let mut irgen = ir::IrModule::new(ast);
    irgen.run()?;
    let irgen = Rc::new(irgen);
    // println!("{irgen}");
    let mut codegen: Codegen<'ctx> = Codegen::create(ctx, irgen);
    codegen.codegen_module();
    codegen.optimize()?;
    Ok(codegen)
}

fn main() -> Result<()> {
    static_assert_size!(ast::Definition, 168);
    println!("Size of ast::Definition: {}", std::mem::size_of::<ast::Definition>());
    println!("Size of ast::BlockStmt: {}", std::mem::size_of::<ast::BlockStmt>());
    println!("Size of ast::Expression: {}", std::mem::size_of::<ast::Expression>());
    println!("Size of ir::IrExpr: {}", std::mem::size_of::<ir::IrExpr>());
    println!("Size of ir::IrStmt: {}", std::mem::size_of::<ir::IrStmt>());

    let args: Vec<String> = env::args().collect();
    println!("NexLang Compiler v0.1.0");
    let src_path = &args[1];
    println!("src_dir: {}", src_path);

    let ctx = Context::create();
    let filename = Path::new(&src_path).file_name().unwrap().to_str().unwrap();
    let src = fs::read_to_string(src_path).expect("could not read source directory");
    let codegen = compile_single_file_program(&ctx, filename, &src)?;

    codegen.emit_object_file()?;
    let result = codegen.interpret_module()?;
    println!("Interp result: {}", result);
    let mut f = File::create(format!("./artifacts/{}.ll", filename))?;
    let llvm_text = codegen.output_llvm_ir_text();
    f.write_all(llvm_text.as_bytes()).unwrap();
    // println!("{}", llvm_text);
    Ok(())
}
