use std::{
    path::Path,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    },
};

use anyhow::{bail, Result};
use clap::Parser;
use colored::Colorize;
use inkwell::context::Context;
use k1::compiler::{self, Command};
use std::os::unix::prelude::ExitStatusExt;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct TestSuiteClapArgs {
    /// If true, uses an LLVM Intrerpreter. Otherwise, builds and runs real executables
    #[arg(long, default_value_t = false, action = clap::ArgAction::Set)]
    pub interpret: bool,

    /// Run in parallel if true
    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub parallel: bool,

    /// Filters test cases by name substring
    pub filter: Option<String>,
}

#[derive(Debug)]
enum TestExpectation {
    ExitCode(i32),
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
            Self::ExitCode(c) => Some(*c),
            _ => None,
        }
    }
    fn abort_message(&self) -> Option<&str> {
        match self {
            Self::AbortErrorMessage { message } => Some(message),
            _ => None,
        }
    }
}

fn get_test_expectation(test_file: &Path) -> TestExpectation {
    let path = test_file.canonicalize().unwrap();
    let src = std::fs::read_to_string(path).expect("could not read source file for test {}");

    let last_line = src.lines().rev().find(|l| !l.is_empty()).expect("last line");
    // We want expected output but we can't intercept or read what goes to stdout, so we just make
    // it expected return value for now
    let error_message_prefix = "//errmsg: ";
    let exit_code_prefix = "//exitcode: ";
    let abort_msg_prefix = "//abortmsg: ";
    if last_line.starts_with(error_message_prefix) {
        let expected_error: String = last_line.chars().skip(error_message_prefix.len()).collect();
        TestExpectation::CompileErrorMessage { message: expected_error }
    } else if last_line.starts_with(exit_code_prefix) {
        let s: String = last_line.chars().skip(exit_code_prefix.len()).collect();
        let as_i32: i32 = s.parse().unwrap();
        TestExpectation::ExitCode(as_i32)
    } else if last_line.starts_with(abort_msg_prefix) {
        let expected_error: String = last_line.chars().skip(abort_msg_prefix.len()).collect();
        TestExpectation::AbortErrorMessage { message: expected_error }
    } else {
        TestExpectation::ExitCode(0)
    }
}

fn test_file<P: AsRef<Path>>(ctx: &Context, path: P, interpret: bool) -> Result<()> {
    let out_dir = ".k1-out/test_suite";
    let filename = path.as_ref().file_name().unwrap().to_str().unwrap();
    let args = k1::compiler::Args {
        no_llvm_opt: true,
        debug: true,
        no_std: false,
        write_llvm: true,
        dump_module: false,
        gui: false,
        command: Command::Build { file: path.as_ref().to_owned() },
    };
    let compile_result = compiler::compile_module(&args);
    let expectation = get_test_expectation(path.as_ref());
    match compile_result {
        Err(err) => match err.module.as_ref() {
            Some(module) => {
                let err = &module.errors[0];
                match expectation {
                    TestExpectation::CompileErrorMessage { message } => {
                        // Check for message!
                        if !err.to_string().contains(&message) {
                            bail!("{filename}: Failed with unexpected message: {}", err.to_string())
                        }
                    }
                    TestExpectation::CompileErrorLine { .. } => {
                        unimplemented!("error line test")
                    }
                    TestExpectation::AbortErrorMessage { .. } => {
                        let mut buf = Vec::new();
                        module.write_error(&mut buf, err).unwrap();
                        let s = String::from_utf8_lossy(&buf);
                        bail!("{filename}\n\tExpected: abort\n\tgot    : compile error '{}'", s)
                    }
                    TestExpectation::ExitCode(expected_code) => {
                        let mut buf = Vec::new();
                        module.write_error(&mut buf, err).unwrap();
                        let s = String::from_utf8_lossy(&buf);
                        bail!(
                            "{filename}\n\tExpected: exit code {}\n\tgot     : compile error: {}",
                            expected_code,
                            s
                        )
                    }
                }
            }
            None => {
                bail!("{} Failed during parsing, probably", filename)
            }
        },
        Ok(typed_module) => {
            let name = typed_module.name();
            let expect_exit = matches!(
                expectation,
                TestExpectation::ExitCode(_) | TestExpectation::AbortErrorMessage { .. }
            );
            if expect_exit {
                let output_executable = !interpret;
                let codegen = compiler::codegen_module(
                    &args,
                    ctx,
                    &typed_module,
                    out_dir,
                    output_executable,
                )?;

                if interpret {
                    match codegen.interpret_module() {
                        Err(e) => bail!("{name} interpret failed: {e}"),
                        Ok(res) => {
                            let result_code = res as i32;
                            let expected_code = expectation.exit_code();
                            if Some(result_code) != expected_code {
                                bail!(
                                    "{name} failed wrong exit code: exp {expected_code:?}, actual {result_code}",
                                );
                            }
                        }
                    }
                } else {
                    let mut run_cmd =
                        std::process::Command::new(format!("{}/{}.out", out_dir, name));
                    let run_result = run_cmd.output();
                    match run_result {
                        Err(e) => {
                            bail!(".output() failed with {e:?}")
                        }
                        Ok(output) => {
                            let run_status = output.status;
                            if let Some(signal) = run_status.signal() {
                                if signal == 5 {
                                    bail!("{name} terminated by trap signal: {signal}");
                                } else if signal == 6 {
                                    if let Some(expected_abort_message) =
                                        expectation.abort_message()
                                    {
                                        let stderr_str = String::from_utf8_lossy(&output.stderr);
                                        let stderr_lines = stderr_str.lines();
                                        match stderr_lines.last() {
                                            None =>
                                            bail!(
                                                "{name} Expected abortmsg {expected_abort_message} but got abort with no output",
                                            ),
                                            Some(abort_msg) => {
                                                if !abort_msg.contains(expected_abort_message) {
                                                    bail!(
                                                        "{name} abort message '{abort_msg}' did not match expected message: {expected_abort_message}"
                                                    )
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    bail!("{name} terminated by signal: {signal}");
                                }
                            };
                            if run_status.code() != expectation.exit_code() {
                                bail!(
                                    "{name} failed wrong exit code: exp {:?}, actual {:?}",
                                    expectation.exit_code(),
                                    run_status.code(),
                                );
                            }
                        }
                    }
                }
            } else {
                bail!("{name} Expected failed compilation but actually succeeded")
            }
        }
    };
    Ok(())
}

pub fn main() -> Result<()> {
    let test_suite_args = TestSuiteClapArgs::parse();
    eprintln!("{:#?}", test_suite_args);
    let test_dir = "test_src";
    let mut all_tests = Vec::new();
    for dir_entry in std::fs::read_dir(test_dir)? {
        let dir_entry = dir_entry?;
        let metadata = dir_entry.metadata()?;
        let path = dir_entry.path();
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

    // TODO: grouping by n to bound threads created
    if parallel {
        std::thread::scope(|scope| {
            for test in all_tests.iter() {
                let filename = test.as_path().file_name().unwrap().to_str().unwrap();
                std::thread::Builder::new()
                    .name(filename.to_string())
                    .spawn_scoped(scope, || {
                        let ctx = Context::create();
                        let filename = filename.to_string();
                        eprintln!("{filename:040}...");
                        let result = test_file(&ctx, test.as_path(), test_suite_args.interpret);
                        if result.is_ok() {
                            eprintln!("{filename:040} {}", "PASS".green());
                            success.fetch_add(1, Ordering::Relaxed);
                        } else {
                            let mut failures = failures.lock().unwrap();
                            failures.push((
                                test.as_path().file_name().unwrap().to_str().unwrap().to_string(),
                                result.unwrap_err(),
                            ));
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
            let result = test_file(&ctx, test.as_path(), test_suite_args.interpret);
            if result.is_ok() {
                eprintln!("{filename:040} {}", "PASS".green());
                success.fetch_add(1, Ordering::Relaxed);
            } else {
                let mut failures = failures.lock().unwrap();
                failures.push((
                    test.as_path().file_name().unwrap().to_str().unwrap().to_string(),
                    result.unwrap_err(),
                ));
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
