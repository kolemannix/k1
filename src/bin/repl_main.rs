use std::process::ExitCode;

use k1::compiler::CompilerConfig;
use k1::parse::{ParsedModule, Source};
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

    let mut parsed_module = ParsedModule::make(
        "parse_test".to_string(),
        CompilerConfig {
            is_test_build: false,
            no_std: false,
            target: k1::compiler::detect_host_target()
                .unwrap_or(k1::compiler::Target::LinuxIntel64),
        },
    );
    let name = "repl.k1";
    let file_id = 0;
    let cwd = std::env::current_dir().unwrap();

    let mut line = String::with_capacity(1024);
    loop {
        let len = std::io::stdin().read_line(&mut line).unwrap();
        let lex_result = k1::parse::lex_text(
            &mut parsed_module,
            Source::make(
                file_id,
                cwd.to_str().unwrap().to_string(),
                name.to_string(),
                line.clone(),
            ),
        );
        let tokens = match lex_result {
            Ok(tokens) => {
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
        let mut parser = k1::parse::Parser::make(&tokens, file_id, &mut parsed_module);
        match parser.parse_statement() {
            Ok(None) => match parser.parse_definition() {
                Ok(Some(defn)) => eprintln!("thanks for your definition"),
                Ok(None) => eprintln!("neither a statement nor a definition but not an error hmm"),
                Err(e) => {
                    k1::parse::print_error(&parsed_module, &e);
                }
            },
            Ok(Some(stmt_id)) => {
                eprintln!("> {}", parsed_module.stmt_id_to_string(stmt_id));
            }
            Err(e) => {
                k1::parse::print_error(&parsed_module, &e);
            }
        }
    }
}
