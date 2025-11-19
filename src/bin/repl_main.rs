// Copyright (c) 2025 knix
// All rights reserved.

use std::path::PathBuf;
use std::process::ExitCode;

use k1::compiler::CompilerConfig;
use k1::lex::TokenKind;
use k1::parse::{ParsedProgram, Source};
use log::info;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> anyhow::Result<ExitCode> {
    let l = Box::leak(Box::new(
        env_logger::Builder::new()
            .format_timestamp(None)
            .filter_level(log::LevelFilter::Debug)
            .build(),
    ));
    log::set_logger(l).unwrap();
    log::set_max_level(log::LevelFilter::Info);
    info!("k1 repl v0.1.0");

    let lib_dir_pathbuf =
        std::env::var("K1_LIB_DIR").map(PathBuf::from).unwrap_or(PathBuf::from("k1lib"));
    let mut ast = ParsedProgram::make(
        "repl".to_string(),
        CompilerConfig {
            is_test_build: false,
            no_std: false,
            target: k1::compiler::detect_host_target()
                .unwrap_or(k1::compiler::Target::LinuxIntel64),
            debug: true,
            out_dir: ".k1-out/repl".into(),
            k1_lib_dir: lib_dir_pathbuf,
        },
    );
    let name = "repl.k1";
    let file_id = 0;
    let cwd = std::env::current_dir().unwrap();

    let mut line = String::with_capacity(1024);
    loop {
        let _len = std::io::stdin().read_line(&mut line).unwrap();
        let mut tokens = vec![];
        let lex_result = k1::parse::lex_text(
            &mut ast,
            Source::make(
                file_id,
                cwd.to_str().unwrap().to_string(),
                name.to_string(),
                line.clone(),
            ),
            &mut tokens,
        );
        let tokens = match lex_result {
            Ok(_) => {
                for t in &tokens {
                    eprintln!("{:20} {:?}", t.kind, t)
                }
                tokens
            }
            Err(e) => {
                eprintln!("Lexer failure: {}", e.message());
                return Ok(ExitCode::SUCCESS);
            }
        };
        let module_name = ast.idents.intern("REPL");
        let parsed_ns_id = k1::parse::init_module(module_name, &mut ast);
        let mut parser =
            k1::parse::Parser::make_for_file(module_name, parsed_ns_id, &mut ast, &tokens, file_id);
        match parser.parse_statement() {
            Ok(None) => match parser.parse_definition(TokenKind::Eof) {
                Ok(Some(_defn)) => eprintln!("thanks for your definition"),
                Ok(None) => eprintln!("neither a statement nor a definition but not an error hmm"),
                Err(e) => {
                    k1::parse::print_error(&ast, &e);
                }
            },
            Ok(Some(stmt_id)) => {
                eprintln!("> {}", ast.stmt_id_to_string(stmt_id));
            }
            Err(e) => {
                k1::parse::print_error(&ast, &e);
            }
        }
    }
}
