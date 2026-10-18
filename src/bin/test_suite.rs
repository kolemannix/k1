// Copyright (c) 2026 knix
// All rights reserved.

use std::{
    path::Path,
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

use anyhow::{Result, bail};
use colored::Colorize;
use inkwell::context::Context;
use k1::{
    compiler::{self, Command, CompileProgramError, CompileRequest},
    typer::{ErrorKind, K1Message, MessageLevel},
};

const USAGE: &str = "Usage: k1_test [filter] [--serial] [--tests-dir <dir>]

  filter        Only run single-file cases whose name contains it
  --serial      Run cases one at a time, in name order
  --tests-dir   Test source root (default: test_src)
";

struct TestSuiteArgs {
    serial: bool,
    filter: Option<String>,
    tests_dir: String,
}

fn parse_args() -> Result<TestSuiteArgs, lexopt::Error> {
    use lexopt::prelude::*;
    let mut parser = lexopt::Parser::from_env();
    let mut args = TestSuiteArgs { serial: false, filter: None, tests_dir: "test_src".to_string() };
    while let Some(arg) = parser.next()? {
        match arg {
            Long("serial") => args.serial = true,
            Long("tests-dir") => args.tests_dir = parser.value()?.string()?,
            Short('h') | Long("help") => {
                print!("{USAGE}");
                std::process::exit(0);
            }
            Value(filter) if args.filter.is_none() => args.filter = Some(filter.string()?),
            _ => return Err(arg.unexpected()),
        }
    }
    Ok(args)
}

fn get_expected_error(test_file: &Path) -> Option<String> {
    let mut path = test_file.canonicalize().unwrap();
    if path.is_dir() {
        path = path.join(k1::plan::BUILD_FILE_NAME);
    }
    let src = std::fs::read_to_string(path).expect("could not read source file for test {}");
    let last_line = src.lines().rev().find(|l| !l.is_empty())?;
    last_line.strip_prefix("//errmsg: ").map(|message| message.to_string())
}

fn compile(path: &Path, k1_home: &str, command: Command) -> Result<CompileResult> {
    let mut request = match CompileRequest::new(path.to_owned(), command, None) {
        Ok(request) => request,
        Err(message) => bail!("{}: {message}", path.display()),
    };
    request.tools.emit_llvm = true;
    request.tools.cache = false;
    request.k1_home = Some(k1_home.to_string());
    Ok(compiler::compile_program(request))
}

type CompileResult = std::result::Result<k1::typer::TypedProgram, CompileProgramError>;

fn run_output(
    typed_program: &k1::typer::TypedProgram,
    exe_name: &str,
) -> Result<std::process::Output> {
    let idents = &typed_program.ast.idents;
    let exe = k1::kpath::join_pathbuf(idents, typed_program.config.out_dir, exe_name);
    Ok(std::process::Command::new(exe)
        .current_dir(idents.get_string(typed_program.config.home_dir))
        .output()?)
}

fn test_file<P: AsRef<Path>>(ctx: &Context, k1_home: &str, path: P) -> Result<()> {
    let filename = path.as_ref().file_name().unwrap().to_str().unwrap();
    let expected_error = get_expected_error(path.as_ref());
    let mut typed_program = match compile(path.as_ref(), k1_home, Command::Build)? {
        Err(CompileProgramError::Build(message)) => bail!("{filename}: {message}"),
        Err(CompileProgramError::TyperFailure(module)) => {
            let messages = module.messages.borrow();
            if let Some(parse_error) = module.ast.errors.first() {
                if let Some(message) = &expected_error
                    && parse_error.message().contains(message.as_str())
                {
                    return Ok(());
                }
                module
                    .write_error(
                        &mut std::io::stderr(),
                        &K1Message {
                            message: module.ast.idents.intern(parse_error.message()),
                            span: parse_error.span(),
                            error_kind: ErrorKind::ParseError,
                            level: MessageLevel::Error,
                        },
                        false,
                    )
                    .unwrap();
                bail!("{filename}: Failed parsing: {}", parse_error)
            }
            let Some(err) = messages.iter().find(|e| e.level == MessageLevel::Error) else {
                bail!("{filename}: Failed but had no errors")
            };
            let Some(message) = expected_error else {
                let mut buf = Vec::new();
                module.write_error(&mut buf, err, true).unwrap();
                bail!("{filename}: compile error: {}", String::from_utf8_lossy(&buf))
            };
            if !module.ident_str(err.message).contains(&message) {
                bail!(
                    "{filename}: Failed with unexpected message: {}",
                    module.ident_str(err.message)
                )
            }
            return Ok(());
        }
        Ok(typed_program) => typed_program,
    };
    let name = typed_program.program_name().to_string();
    if let Some(message) = expected_error {
        if compiler::codegen_module(ctx, &mut typed_program).is_ok() {
            bail!("{name} Expected failed compilation but actually succeeded")
        }
        let messages = typed_program.messages.borrow();
        let Some(err) = messages.iter().find(|e| e.level == MessageLevel::Error) else {
            bail!("{name}: Failed after typechecking but had no errors")
        };
        if !typed_program.ident_str(err.message).contains(message.as_str()) {
            bail!(
                "{name}: Failed with unexpected message: {}",
                typed_program.ident_str(err.message)
            )
        }
        return Ok(());
    }
    if typed_program.get_main_function_id().is_some() {
        compiler::codegen_module(ctx, &mut typed_program)?;
        let output = run_output(&typed_program, &name)?;
        if !output.status.success() {
            bail!("{name} {}:\n{}", output.status, String::from_utf8_lossy(&output.stderr));
        }
        return Ok(());
    }
    let mut test_program = match compile(path.as_ref(), k1_home, Command::Test)? {
        Ok(test_program) => test_program,
        Err(CompileProgramError::Build(message)) => bail!("{filename}: {message}"),
        Err(CompileProgramError::TyperFailure(module)) => bail!("{}", module.failure_summary()),
    };
    let mut static_failures = String::new();
    for result in &test_program.test_results {
        if let Some(failure) = result.failure {
            let name = test_program.ident_str(result.name);
            static_failures
                .push_str(&format!("FAIL {name}\n{}\n", test_program.ident_str(failure)));
        }
    }
    if !static_failures.is_empty() {
        bail!("{name} static tests:\n{static_failures}");
    }
    compiler::codegen_module(ctx, &mut test_program)?;
    let output = run_output(&test_program, &format!("{name}_test"))?;
    if !output.status.success() {
        bail!("{name} tests {}:\n{}", output.status, String::from_utf8_lossy(&output.stdout));
    }
    Ok(())
}

pub fn main() -> Result<()> {
    let test_suite_args = match parse_args() {
        Ok(args) => args,
        Err(e) => bail!("{e}\n{USAGE}"),
    };
    let test_dir = test_suite_args.tests_dir;
    let k1_home = std::env::current_dir()?.to_str().unwrap().to_string();
    let mut all_tests = Vec::new();
    for dir_entry in std::fs::read_dir(test_dir)? {
        let dir_entry = dir_entry?;
        let metadata = dir_entry.metadata()?;
        let path = dir_entry.path().canonicalize().unwrap();
        eprintln!("{path:?}");

        if path.file_name().unwrap().to_str().unwrap().starts_with(".") {
            continue;
        }

        if metadata.is_file() {
            let extension = path.extension().unwrap();
            if extension == "k1" {
                match test_suite_args.filter.as_ref() {
                    None => all_tests.push(path.to_path_buf()),
                    Some(f) => {
                        let name_stem = path.file_stem().unwrap().to_string_lossy();
                        if name_stem.contains(f) {
                            all_tests.push(path.to_path_buf())
                        }
                    }
                }
            }
        } else if metadata.is_dir() {
            all_tests.push(path.to_path_buf())
        }
    }

    let parallel = !test_suite_args.serial;

    if !parallel {
        all_tests.sort_by(|p1, p2| {
            let p1 = p1.file_name().unwrap().to_str().unwrap();
            let p2 = p2.file_name().unwrap().to_str().unwrap();
            p1.cmp(p2)
        });
    }

    let total: usize = all_tests.len();
    let success = AtomicUsize::new(0);
    let failures = Mutex::new(Vec::with_capacity(total));

    // Note: This doesn't bound the number of spawned threads
    if parallel {
        std::thread::scope(|scope| {
            for test in all_tests.iter() {
                let filename = test.as_path().file_name().unwrap().to_str().unwrap();
                std::thread::Builder::new()
                    .stack_size(k1::STACK_SIZE)
                    .name(filename.to_string())
                    .spawn_scoped(scope, || {
                        let ctx = Context::create();
                        let filename = filename.to_string();
                        eprintln!("{filename:040}...");
                        let result = test_file(&ctx, &k1_home, test.as_path());
                        match result {
                            Ok(_) => {
                                eprintln!("{filename:040} {}", "PASS".green());
                                success.fetch_add(1, Ordering::Relaxed);
                            }
                            Err(e) => {
                                let mut failures = failures.lock().unwrap();
                                failures.push((
                                    test.as_path()
                                        .file_name()
                                        .unwrap()
                                        .to_str()
                                        .unwrap()
                                        .to_string(),
                                    e,
                                ));
                            }
                        }
                    })
                    .unwrap();
            }
        });
    } else {
        for test in all_tests.iter() {
            let ctx = Context::create();
            let filename = test.as_path().file_name().unwrap().to_str().unwrap();
            eprintln!("{filename:040}...");
            let result = test_file(&ctx, &k1_home, test.as_path());
            match result {
                Ok(_) => {
                    eprintln!("{filename:040} {}", "PASS".green());
                    success.fetch_add(1, Ordering::Relaxed);
                }
                Err(e) => {
                    let mut failures = failures.lock().unwrap();
                    failures.push((
                        test.as_path().file_name().unwrap().to_str().unwrap().to_string(),
                        e,
                    ));
                }
            }
        }
    }
    let success = success.into_inner();
    if success != total {
        let failures = failures.lock().unwrap();
        eprintln!("\n-----------------------------------\nFailed tests:\n");
        for (filename, result) in failures.iter() {
            eprintln!("{filename:040} {}: {}", "FAIL".red(), result);
        }
        bail!("{} tests failed", total - success);
    } else {
        eprintln!("Ran {} tests, {} succeeded", total, success);
    }
    Ok(())
}
