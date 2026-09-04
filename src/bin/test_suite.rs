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
use clap::Parser;
use colored::Colorize;
use inkwell::context::Context;
use k1::{
    compiler::{self, Command, CompileProgramError},
    typer::{ErrorKind, K1Message, MessageLevel},
};
use std::os::unix::prelude::ExitStatusExt;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct TestSuiteClapArgs {
    /// Run in parallel if true
    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub parallel: bool,

    /// Filters test cases by name substring
    pub filter: Option<String>,

    /// The root directory of the test sources to run
    #[arg(long)]
    pub tests_dir: Option<String>,
}

#[derive(Debug)]
enum TestExpectation {
    ExitCode {
        code: i32,
        message: Option<String>,
    },
    CompileErrorMessage {
        message: String,
    },
    AbortErrorMessage {
        message: String,
    },
    #[allow(unused)]
    CompileErrorLine {
        line: u32,
    },
}
impl TestExpectation {
    fn exit_code(&self) -> Option<i32> {
        match self {
            Self::ExitCode { code, .. } => Some(*code),
            _ => None,
        }
    }
    fn expected_message(&self) -> Option<&str> {
        match self {
            Self::ExitCode { message, .. } => message.as_ref().map(|s| s.as_str()),
            _ => None,
        }
    }
}

// linux symbolizes in-process via libk1rt's vendored libbacktrace; macos shells out to atos
fn backtrace_symbolizer_available() -> bool {
    !cfg!(target_os = "macos") || Path::new("/usr/bin/atos").exists()
}

fn get_test_expectation(test_file: &Path) -> TestExpectation {
    let mut path = test_file.canonicalize().unwrap();
    if path.is_dir() {
        match k1::compiler::detect_module_root_file(&path.to_string_lossy()) {
            Some(root) => path = root,
            None => return TestExpectation::ExitCode { code: 0, message: None },
        }
    }
    let src = std::fs::read_to_string(path).expect("could not read source file for test {}");

    let last_line = src.lines().rev().find(|l| !l.is_empty()).expect("last line");
    // We want expected output but we can't intercept or read what goes to stdout, so we just make
    // it expected return value for now
    let error_message_prefix = "//errmsg: ";
    let exit_code_prefix = "//exitcode: ";
    let abort_msg_prefix = "//abortmsg: ";
    if let Some(expected_error) = last_line.strip_prefix(error_message_prefix) {
        TestExpectation::CompileErrorMessage { message: expected_error.to_string() }
    } else if let Some(s) = last_line.strip_prefix(exit_code_prefix) {
        let end = s.find(char::is_whitespace).unwrap_or(s.len());
        let exit_code_str = &s[..end];
        let as_i32: i32 = exit_code_str.parse().unwrap();
        let message: String = s[end..].trim_start().to_string();
        TestExpectation::ExitCode {
            code: as_i32,
            message: if message.is_empty() { None } else { Some(message) },
        }
    } else if let Some(expected_abort_msg) = last_line.strip_prefix(abort_msg_prefix) {
        TestExpectation::AbortErrorMessage { message: expected_abort_msg.to_string() }
    } else {
        TestExpectation::ExitCode { code: 0, message: None }
    }
}

fn test_file<P: AsRef<Path>>(ctx: &Context, path: P) -> Result<()> {
    let filename = path.as_ref().file_name().unwrap().to_str().unwrap();
    let args = k1::compiler::Args {
        optimize: false,
        debug: false,
        sanitize: false,
        filc: false,
        no_std: false,
        emit_llvm: true,
        dump_module: false,
        dump_idents: false,
        profile: false,
        target: None,
        chatty: false,
        optimize_ir: true,
        cache: false,
        k1_home_override: None,
        command: Command::Build { file: Some(path.as_ref().to_owned()) },
    };
    let compile_result = compiler::compile_program(&args);
    let expectation = get_test_expectation(path.as_ref());
    match compile_result {
        Err(CompileProgramError::TyperFailure(module)) => {
            let messages = module.messages.borrow();
            if let Some(parse_error) = module.ast.errors.first() {
                if let TestExpectation::CompileErrorMessage { message } = &expectation {
                    if parse_error.message().contains(message.as_str()) {
                        return Ok(());
                    }
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
            match expectation {
                TestExpectation::CompileErrorMessage { message } => {
                    // Check for message!
                    if !module.ident_str(err.message).contains(&message) {
                        bail!(
                            "{filename}: Failed with unexpected message: {}",
                            module.ident_str(err.message)
                        )
                    }
                }
                TestExpectation::CompileErrorLine { .. } => {
                    unimplemented!("error line test")
                }
                TestExpectation::AbortErrorMessage { .. } => {
                    let mut buf = Vec::new();
                    module.write_error(&mut buf, err, true).unwrap();
                    let s = String::from_utf8_lossy(&buf);
                    bail!("{filename}\n\tExpected: abort\n\tgot    : compile error '{}'", s)
                }
                TestExpectation::ExitCode { code: expected_code, .. } => {
                    let mut buf = Vec::new();
                    module.write_error(&mut buf, err, true).unwrap();
                    let s = String::from_utf8_lossy(&buf);
                    bail!(
                        "{filename}\n\tExpected: exit code {}\n\tgot     : compile error: {}",
                        expected_code,
                        s
                    )
                }
            }
        }
        Ok(mut typed_program) => {
            let name = typed_program.program_name().to_string();
            let expect_exit = matches!(
                expectation,
                TestExpectation::ExitCode { .. } | TestExpectation::AbortErrorMessage { .. }
            );
            if expect_exit {
                compiler::codegen_module(&args, ctx, &mut typed_program)?;
                let mut run_cmd = std::process::Command::new(k1::kpath::join_pathbuf(
                    &typed_program.ast.idents,
                    typed_program.config.out_dir,
                    name.as_str(),
                ));
                let run_result = run_cmd.output();
                match run_result {
                    Err(e) => {
                        bail!(".output() failed with {e:?}")
                    }
                    Ok(output) => {
                        let run_status = output.status;
                        let stderr_str = String::from_utf8_lossy(&output.stderr);
                        let stderr_lines = stderr_str.lines();
                        let last_stderr_line = stderr_lines.last();
                        if let Some(signal) = run_status.signal() {
                            if signal != 6 {
                                bail!("{name} terminated by signal: {signal}");
                            }
                            let TestExpectation::AbortErrorMessage {
                                message: expected_abort_message,
                            } = &expectation
                            else {
                                bail!("{name} terminated by SIGABRT");
                            };
                            match last_stderr_line {
                                None => bail!(
                                    "{name} Expected abortmsg {expected_abort_message} but got abort with no output",
                                ),
                                Some(abort_msg) => {
                                    if !abort_msg.contains(expected_abort_message) {
                                        bail!(
                                            "{name} abort message '{abort_msg}' did not match expected message: {expected_abort_message}"
                                        )
                                    }
                                    if backtrace_symbolizer_available() {
                                        let stdout_str = String::from_utf8_lossy(&output.stdout);
                                        let frame_ref = format!("{name}.k1:");
                                        if !stdout_str.contains(frame_ref.as_str()) {
                                            bail!(
                                                "{name} abort backtrace has no symbolized frame '{frame_ref}'; stdout:\n{stdout_str}"
                                            )
                                        }
                                    }
                                    return Ok(());
                                }
                            }
                        }
                        if run_status.code() != expectation.exit_code() {
                            bail!(
                                "{name} failed wrong exit code: exp {:?}, actual status: {:?}",
                                expectation.exit_code(),
                                run_status,
                            );
                        }
                        if let Some(expected_exit_message) = expectation.expected_message() {
                            match last_stderr_line {
                                None => bail!(
                                    "{name} Expected exit message {expected_exit_message} but got exit with no output",
                                ),
                                Some(exit_msg) => {
                                    if !exit_msg.contains(expected_exit_message) {
                                        bail!(
                                            "{name} exit message '{exit_msg}' did not match expected message: {expected_exit_message}"
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                let TestExpectation::CompileErrorMessage { message } = &expectation else {
                    unimplemented!("error line test")
                };
                if compiler::codegen_module(&args, ctx, &mut typed_program).is_ok() {
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
            }
        }
    };
    Ok(())
}

pub fn main() -> Result<()> {
    let test_suite_args = TestSuiteClapArgs::parse();
    eprintln!("{:#?}", test_suite_args);
    let test_dir = test_suite_args.tests_dir.unwrap_or("test_src".to_string());
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

    let parallel = test_suite_args.parallel;

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
                        let result = test_file(&ctx, test.as_path());
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
            let result = test_file(&ctx, test.as_path());
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
