// Copyright (c) 2026 knix
// All rights reserved.
use std::path::PathBuf;
use std::process::ExitCode;

use k1::compiler;
use k1::compiler::{Command, CompileProgramError, CompileRequest, Target, ToolFlags};
use log::info;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() -> anyhow::Result<ExitCode> {
    let l = Box::leak(Box::new(
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
            .format_timestamp(None)
            .build(),
    ));
    let max_level = l.filter();
    log::set_logger(l).unwrap();
    log::set_max_level(max_level);
    let (request, program_args) = match parse_cli() {
        Ok(cli) => cli,
        Err(e) => {
            eprintln!("k1: {e}\nRun `k1 --help` for usage");
            return Ok(ExitCode::FAILURE);
        }
    };
    let command = request.build.command;
    let mut program = match compiler::compile_program(request) {
        Ok(program) => program,
        Err(CompileProgramError::TyperFailure(program)) => {
            compiler::report_trace(&program);
            return Ok(ExitCode::FAILURE);
        }
        Err(CompileProgramError::Build(message)) => {
            eprintln!("{message}");
            return Ok(ExitCode::FAILURE);
        }
    };
    if !command.codegens() || command == Command::Server {
        compiler::report_trace(&program);
    }
    if !command.codegens() {
        // Setup runs as a module-load gate, so a successful compile means setup ran.
        // In release builds, just exit fast
        if cfg!(debug_assertions) {
            return Ok(ExitCode::SUCCESS);
        } else {
            std::process::exit(0)
        }
    };
    if command == Command::Server {
        use std::sync::{Arc, Mutex};
        k1::server::serve(Arc::new(Mutex::new(Some(Box::new(program)))));
        return Ok(ExitCode::SUCCESS);
    }
    if matches!(command, Command::Run | Command::Test) && !program.plan.is_executable() {
        eprintln!(
            "{} is a library module; run/test require an executable module",
            program.program_name()
        );
        return Ok(ExitCode::FAILURE);
    }
    let llvm_ctx = inkwell::context::Context::create();
    let codegen_result = compiler::codegen_module(&llvm_ctx, &mut program);
    compiler::report_trace(&program);
    let success = match codegen_result {
        Ok(()) => match command {
            Command::Check => unreachable!(),
            Command::Build => true,
            Command::Run => {
                info!("run executable: {}", program.program_name());
                let exit_code = compiler::run_compiled_program(
                    &program.ast.idents,
                    program.plan.config.target,
                    program.config.out_dir,
                    program.config.home_dir,
                    program.program_name(),
                    false,
                    &program_args,
                );
                exit_code == Some(0)
            }
            Command::Test => {
                info!("test executable: {}", program.program_name());
                let exit_code = compiler::run_compiled_program(
                    &program.ast.idents,
                    program.plan.config.target,
                    program.config.out_dir,
                    program.config.home_dir,
                    program.program_name(),
                    true,
                    &[],
                );
                exit_code == Some(0)
            }
            Command::Server => unreachable!("server runs before codegen"),
            Command::Setup { .. } => unreachable!("setup exits after compile"),
            Command::Clean => {
                // Clear the out dir
                let out_dir_path =
                    PathBuf::from(program.ast.idents.get_string(program.config.out_dir));
                if std::fs::exists(&out_dir_path).unwrap() {
                    std::fs::remove_dir_all(&out_dir_path)?;
                }
                true
            }
        },
        Err(err) => {
            eprintln!("Codegen error: {err}");
            false
        }
    };
    if !cfg!(debug_assertions) {
        std::process::exit(if success { 0 } else { 1 })
    }
    Ok(if success { ExitCode::SUCCESS } else { ExitCode::FAILURE })
}

fn usage() -> String {
    let mut targets = String::new();
    for target in Target::ALL {
        if !targets.is_empty() {
            targets.push('|');
        }
        targets.push_str(target.to_str());
    }
    format!(
        "k1 {version}

Usage: k1 <command> [path] [options] [-- program args]

  path is a file or a module dir (default: .)

Commands:
  check, c    Typecheck
  build, b    Build the executable or library
  run, r      Build and run; later arguments go to the program (after -- if they
              start with -)
  test, t     Build and run the test executable
  server      Serve the megarepl for the program
  setup       Run the module's setup if its stamp is stale
  clean       Delete the module's .k1-out

Build options (fn build sees these as req.default and req.options):
  --target <{targets}>
  --optimize
  --debug              Emit debug info
  --no-std             Load core only
  --sanitize           Link AddressSanitizer and UndefinedBehaviorSanitizer
  --filc               Compile and link through Fil-C (intel64-linux, needs K1_FILC)
  -D <name[=value]>    Option for the primary module's fn build

Tool options:
  --no-cache           Skip the disk cache
  --emit-llvm          Write <out>/<program>.ll
  --dump-module        Write the typed program as text
  --dump-ir            Write the IR codegen consumes
  --dump-idents        Write the identifier pool with stats
  --dump-trace         Write the compile trace as folded stacks
  --chatty             Print timing summaries
  --no-optimize-ir     Skip k1's own IR optimizations
  --profile            pprof flamegraph (profile feature builds)
  --force              setup: rerun even if fresh
  -h, --help
  -V, --version
",
        version = env!("CARGO_PKG_VERSION"),
    )
}

fn parse_command(name: &str) -> Result<Command, lexopt::Error> {
    Ok(match name {
        "check" | "c" => Command::Check,
        "build" | "b" => Command::Build,
        "run" | "r" => Command::Run,
        "test" | "t" => Command::Test,
        "server" => Command::Server,
        "setup" => Command::Setup { force: false },
        "clean" => Command::Clean,
        _ => return Err(format!("unknown command {name}").into()),
    })
}

fn parse_cli() -> Result<(CompileRequest, Vec<String>), lexopt::Error> {
    use lexopt::prelude::*;
    let mut parser = lexopt::Parser::from_env();
    let mut command = None;
    let mut path = None;
    let mut program_args = Vec::new();
    let mut target = None;
    let mut force = false;
    let (mut optimize, mut debug, mut no_std, mut sanitize, mut filc) =
        (false, false, false, false, false);
    let mut options = Vec::new();
    let mut tools = ToolFlags::default();
    while let Some(arg) = parser.next()? {
        match arg {
            Long("target") => {
                let name = parser.value()?.string()?;
                let Some(parsed) = Target::parse(&name) else {
                    return Err(format!("unknown target {name}").into());
                };
                target = Some(parsed);
            }
            Long("optimize") => optimize = true,
            Long("debug") => debug = true,
            Long("no-std") => no_std = true,
            Long("sanitize") => sanitize = true,
            Long("filc") => filc = true,
            Short('D') => options.push(parser.value()?.string()?),
            Long("no-cache") => tools.cache = false,
            Long("emit-llvm") => tools.emit_llvm = true,
            Long("dump-module") => tools.dump_module = true,
            Long("dump-ir") => tools.dump_ir = true,
            Long("dump-idents") => tools.dump_idents = true,
            Long("dump-trace") => tools.dump_trace = true,
            Long("chatty") => tools.chatty = true,
            Long("no-optimize-ir") => tools.optimize_ir = false,
            Long("profile") => tools.profile = true,
            Long("force") => force = true,
            Short('h') | Long("help") => {
                print!("{}", usage());
                std::process::exit(0);
            }
            Short('V') | Long("version") => {
                println!("k1 {}", env!("CARGO_PKG_VERSION"));
                std::process::exit(0);
            }
            Value(value) if command.is_none() => command = Some(parse_command(&value.string()?)?),
            Value(value) if path.is_none() => path = Some(PathBuf::from(value)),
            Value(value) if command == Some(Command::Run) => program_args.push(value.string()?),
            _ => return Err(arg.unexpected()),
        }
    }
    let command = match command {
        None => return Err("missing command".into()),
        Some(Command::Setup { .. }) => Command::Setup { force },
        Some(_) if force => return Err("--force only applies to setup".into()),
        Some(command) => command,
    };
    let path = path.unwrap_or_else(|| PathBuf::from("."));
    let mut request = CompileRequest::new(path, command, target)?;
    let build = &mut request.build.default;
    build.optimize = optimize;
    build.debug = debug;
    build.no_std = no_std;
    build.sanitize = sanitize;
    build.filc = filc;
    request.build.options = options;
    request.tools = tools;
    Ok((request, program_args))
}
