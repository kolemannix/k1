use std::env;

mod ast;
//mod codegen;
mod codegen_ir;
mod ir;
mod lex;
mod output;
mod parse;

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

fn main() -> Result<(), Box<dyn Error>> {
    static_assert_size!(ast::Definition, 96);
    println!("Size of ast::Definition: {}", std::mem::size_of::<ast::Definition>());
    println!("Size of ast::BlockStmt: {}", std::mem::size_of::<ast::BlockStmt>());
    println!("Size of ast::Expression: {}", std::mem::size_of::<ast::Expression>());
    println!("Size of ir::IrExpr: {}", std::mem::size_of::<ir::IrExpr>());
    println!("Size of ir::IrStmt: {}", std::mem::size_of::<ir::IrStmt>());

    let args: Vec<String> = env::args().collect();
    println!("NexLang Compiler v0.1.0");
    let src_path = &args[1];
    println!("src_dir: {}", src_path);
    let src = fs::read_to_string(src_path).expect("could not read source directory");
    let path = Path::new(&src_path);
    let filename = path.file_name().unwrap().to_str().unwrap();
    let ast = parse::parse_text(&src, filename).unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}': {:?}", src_path, e);
        panic!("parse error");
    });
    let ctx = codegen_ir::init_context();
    let mut irgen = ir::IrModule::new(&ast);
    irgen.run()?;
    println!("{irgen}");
    let mut codegen = Codegen::create(&ctx, &irgen);
    codegen.codegen_module();
    codegen.optimize()?;
    let mut f = File::create(format!("{}_llvm_out.ll", filename))?;
    let llvm_text = codegen.output_llvm_ir_text();
    f.write_all(llvm_text.as_bytes()).unwrap();
    println!("{}", llvm_text);
    Ok(())
}
