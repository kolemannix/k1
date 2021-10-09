use std::env;
use ast::*;

mod parse;
mod ast;
mod lex;
mod log;

fn main() {
    let args: Vec<String> = env::args().collect();
    log::normal("NexLang Compiler v0.1.0");
    let src_dir = &args[1];
    log::normal(&format!("src_dir: {}", src_dir));
}