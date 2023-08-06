use std::env;
use std::rc::Rc;

mod codegen_llvm;
mod ir;
mod lex;
mod parse;
mod prelude;
#[cfg(test)]
mod test_suite;

use anyhow::Result;
use inkwell::context::Context;

use std::fs;
use std::fs::File;
use std::io::{Write};
use std::path::Path;

use crate::codegen_llvm::Codegen;

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
    let use_prelude = true;
    let ast = parse::parse_text(source, filename, use_prelude).unwrap_or_else(|e| {
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
    env_logger::init();

    static_assert_size!(parse::Definition, 16);
    static_assert_size!(parse::BlockStmt, 144);
    static_assert_size!(parse::Expression, 56);
    static_assert_size!(ir::IrExpr, 64);
    static_assert_size!(ir::IrStmt, 16);
    println!("Size of ast::Definition: {}", std::mem::size_of::<parse::Definition>());
    println!("Size of ast::FnDef: {}", std::mem::size_of::<parse::FnDef>());
    println!("Size of ast::TypeExpression: {}", std::mem::size_of::<parse::TypeExpression>());

    let args: Vec<String> = env::args().collect();
    println!("NexLang Compiler v0.1.0");
    let src_path = &args[1];
    println!("src_dir: {}", src_path);

    let ctx = Context::create();
    let filename = Path::new(&src_path).file_name().unwrap().to_str().unwrap();
    let src = fs::read_to_string(src_path).expect("could not read source directory");
    let codegen = compile_single_file_program(&ctx, filename, &src)?;

    let llvm_text = codegen.output_llvm_ir_text();
    let mut f = File::create(format!("./artifacts/{}.ll", filename))?;
    f.write_all(llvm_text.as_bytes()).unwrap();

    codegen.emit_object_file()?;
    let result = codegen.interpret_module()?;
    println!("Interp result: {}", result);
    // println!("{}", llvm_text);
    Ok(())
}
