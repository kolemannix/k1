use std::env;

mod ast;
mod lex;
mod parse;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("NexLang Compiler v0.1.0");
    let src_dir = &args[1];
    println!("src_dir: {}", src_dir);
}
