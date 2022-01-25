use std::env;

mod ast;
mod codegen;
mod lex;
mod output;
mod parse;
use std::fs;
use std::fs::File;
use std::io::{BufReader, Read};

use crate::codegen::CodeGen;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("NexLang Compiler v0.1.0");
    let src_path = &args[1];
    println!("src_dir: {}", src_path);
    let src = fs::read_to_string(src_path).expect("could not read source directory");
    let parsed = parse::parse_text(&src, src_path).unwrap_or_else(|e| {
        eprintln!("Encountered ParseError on file '{}': {:?}", src_path, e);
        panic!("parse error");
    });
    let ctx = codegen::init_context();
    let mut codegen = CodeGen::create(&ctx);
    let result = codegen.codegen_module(&parsed);
    println!("{}", result);
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
