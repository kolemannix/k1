use std::env;

mod ast;
mod codegen;
mod ir;
mod lex;
mod output;
mod parse;

use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read, Write};

use crate::codegen::CodeGen;

/// Type size assertion. The first argument is a type and the second argument is its expected size.
/// Cool trick from rustc.
#[macro_export]
macro_rules! static_assert_size {
    ($ty:ty, $size:expr) => {
        const _: [(); $size] = [(); ::std::mem::size_of::<$ty>()];
    };
}

fn main() -> Result<(), Box<dyn Error>> {
    static_assert_size!(ast::Definition, 88);
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
    let ast = parse::parse_text(&src, src_path).unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}': {:?}", src_path, e);
        panic!("parse error");
    });
    let ctx = codegen::init_context();
    let mut codegen = CodeGen::create(&ctx);
    let mut irgen = ir::IRModule::new(&ast);
    if let Err(e) = irgen.run() {
        eprintln!("{}", e);
        println!("{irgen}")
    } else {
        println!("{irgen}");
    }
    let result = codegen.codegen_module(&ast);
    let mut f = File::create("llvm_out.ll").expect("make llvm_ir");
    f.write_all(result.as_bytes()).unwrap();
    println!("{}", result);
    Ok(())
    // for path in paths {
    //     let path = path.expect("Error reading dir entry");
    //     let file = File::open(path.path()).unwrap();
    //     let mut buf_read = BufReader::new(file);
    //     let mut content = String::new();
    //     buf_read.read_to_string(&mut content).expect("read failed");
    //     let parsed = parse::parse_text(&content);
    //     let ctx = codegen::init_context();
    //     match parsed {
    //         Err(parse_error) => {
    //
    //         }
    //         Ok(module) => {
    //             let mut codegen = CodeGen::create(&ctx);
    //             let result = codegen.codegen_module(&module);
    //             println!("{}", result);
    //         }
    //     }
    // }
}
