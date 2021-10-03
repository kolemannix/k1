use std::env;
use ast::*;

mod parse;
mod ast;
mod lex;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("NixLang Compiler v0.1.0");
    let src_dir = &args[1];
    println!("src_dir: {}", src_dir);
}