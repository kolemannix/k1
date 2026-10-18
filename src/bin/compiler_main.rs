// Copyright (c) 2026 knix
// All rights reserved.
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use k1::compiler;
use k1::compiler::{Command, CompileProgramError, CompileRequest, Target, ToolFlags};
use k1::parse;
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
        Ok(Cli::Compile(request, program_args)) => (request, program_args),
        Ok(Cli::Parse(path)) => return Ok(parse_file(&path)),
        Err(e) => {
            eprintln!("k1: {e}\nRun `k1 --help` for usage");
            return Ok(ExitCode::FAILURE);
        }
    };
    let command = request.build.command;
    let static_tests = request.tests.static_tests;
    let mut program = match compiler::compile_program(request) {
        Ok(program) => program,
        Err(CompileProgramError::TyperFailure(program)) => {
            compiler::report_trace(&program);
            eprintln!("{}", program.failure_summary());
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
    let static_ok = !static_tests || compiler::report_static_tests(&program);
    if !command.codegens() {
        // Setup runs as a module-load gate, so a successful compile means setup ran.
        // In release builds, just exit fast
        let exit_code = if static_ok { 0 } else { 1 };
        if cfg!(debug_assertions) {
            return Ok(ExitCode::from(exit_code));
        } else {
            std::process::exit(exit_code.into())
        }
    };
    if command == Command::Server {
        use std::sync::{Arc, Mutex};
        k1::server::serve(Arc::new(Mutex::new(Some(Box::new(program)))));
        return Ok(ExitCode::SUCCESS);
    }
    if command == Command::Run && !program.plan.is_executable() {
        eprintln!(
            "{} is a library module; run requires an executable module",
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
                static_ok && exit_code == Some(0)
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

fn parse_file(path: &Path) -> ExitCode {
    let content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("k1: cannot read {}: {e}", path.display());
            return ExitCode::FAILURE;
        }
    };
    let (ast, lexed) = parse::parse_standalone(path.to_string_lossy().into_owned(), &content);
    print!("{}", parse::highlight::highlight_text(&ast, &lexed));
    for error in &ast.errors {
        parse::print_error(&ast, error);
    }
    if ast.errors.is_empty() { ExitCode::SUCCESS } else { ExitCode::FAILURE }
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

Usage: k1 <command> [path] [options] [-- program args | test filters]

  path is a file or a module dir (default: .)

Commands:
  check, c    Typecheck
  build, b    Build the executable or library
  run, r      Build and run; later arguments go to the program (after -- if they
              start with -)
  test, t     Run the module's fn(test)s in the VM, then in the test executable;
              later arguments filter by name substring (after -- if they start
              with -)
  server      Serve the megarepl for the program
  setup       Run the module's setup if its stamp is stale
  clean       Delete the module's .k1-out
  parse       Print one file's tokens as `<start> <len> <class>` lines for highlighting

Build options (fn build sees these as req.default and req.options):
  --target <{targets}>
  --optimize
  --debug              Emit debug info
  --sanitize           Link AddressSanitizer and UndefinedBehaviorSanitizer
  --filc               Compile and link through Fil-C (intel64-linux, needs K1_FILC)
  -D <name[=value]>    Option for the primary module's fn build

Test options:
  --skip-static        Skip running the tests in the VM
  --skip-runtime       Skip the test executable: check plus the VM run

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

enum Cli {
    Compile(CompileRequest, Vec<String>),
    Parse(PathBuf),
}

fn parse_cli() -> Result<Cli, lexopt::Error> {
    use lexopt::prelude::*;
    let mut parser = lexopt::Parser::from_env();
    let mut command = None;
    let (mut skip_static, mut skip_runtime) = (false, false);
    let mut parse_only = false;
    let mut path = None;
    let mut trailing = Vec::new();
    let mut target = None;
    let mut force = false;
    let (mut optimize, mut debug, mut sanitize, mut filc) = (false, false, false, false);
    let mut options = Vec::new();
    let mut tools = ToolFlags::default();
    loop {
        if let Some(mut raw) = parser.try_raw_args()
            && raw.peek().is_some_and(|arg| arg == "--")
        {
            raw.next();
            for arg in raw {
                trailing.push(arg.into_string().map_err(|_| "argument is not UTF-8")?);
            }
            break;
        }
        let Some(arg) = parser.next()? else { break };
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
            Long("skip-static") => skip_static = true,
            Long("skip-runtime") => skip_runtime = true,
            Short('h') | Long("help") => {
                print!("{}", usage());
                std::process::exit(0);
            }
            Short('V') | Long("version") => {
                println!("k1 {}", env!("CARGO_PKG_VERSION"));
                std::process::exit(0);
            }
            Value(value) if command.is_none() && !parse_only => {
                let name = value.string()?;
                if name == "parse" {
                    parse_only = true
                } else {
                    command = Some(parse_command(&name)?)
                }
            }
            Value(value) if path.is_none() => path = Some(PathBuf::from(value)),
            Value(value) => trailing.push(value.string()?),
            _ => return Err(arg.unexpected()),
        }
    }
    if parse_only {
        return match path {
            Some(path) => Ok(Cli::Parse(path)),
            None => Err("parse needs a .k1 file".into()),
        };
    }
    let command = match command {
        None => return Err("missing command".into()),
        Some(Command::Setup { .. }) => Command::Setup { force },
        Some(_) if force => return Err("--force only applies to setup".into()),
        Some(command) => command,
    };
    let (command, static_tests, filter, program_args) = match (command, skip_static, skip_runtime) {
        (Command::Test, true, true) => {
            return Err("--skip-static and --skip-runtime together skip every test".into());
        }
        (Command::Test, false, false) => (Command::Test, true, trailing, Vec::new()),
        (Command::Test, false, true) => (Command::Check, true, trailing, Vec::new()),
        (Command::Test, true, false) => (Command::Test, false, trailing, Vec::new()),
        (_, true, _) | (_, _, true) => {
            return Err("--skip-static and --skip-runtime only apply to test".into());
        }
        (Command::Run, ..) => (Command::Run, false, Vec::new(), trailing),
        _ if !trailing.is_empty() => {
            return Err(format!("unexpected argument {}", trailing[0]).into());
        }
        (command, ..) => (command, false, Vec::new(), Vec::new()),
    };
    let path = path.unwrap_or_else(|| PathBuf::from("."));
    let mut request = CompileRequest::new(path, command, target)?;
    request.tests.static_tests = static_tests;
    request.tests.filter = filter;
    let build = &mut request.build.default;
    build.optimize = optimize;
    build.debug = debug;
    build.sanitize = sanitize;
    build.filc = filc;
    request.build.options = options;
    request.tools = tools;
    Ok(Cli::Compile(request, program_args))
}
