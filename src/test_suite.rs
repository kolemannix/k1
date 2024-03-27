use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, Result};
use inkwell::context::Context;
use std::os::unix::prelude::ExitStatusExt;

fn test_file<P: AsRef<Path>>(ctx: &Context, path: P) -> Result<()> {
    let path = path.as_ref().canonicalize().unwrap();
    let filename = path.file_name().unwrap().to_str().unwrap();
    let src_dir = path.parent().unwrap().to_str().unwrap();
    let src = std::fs::read_to_string(&path).expect("could not read source file for test");
    println!("{}", filename);
    let out_dir = "bfl-out/test_suite";
    crate::compile_directory_program(
        ctx,
        src_dir,
        false,
        out_dir,
        false,
        true,
        Some(|p: &PathBuf| *p == path),
    )
    .map_err(|err| anyhow!("\tTEST CASE FAILED COMPILE: {}. Reason: {}", filename, err))?;
    let last_line = src.lines().last().unwrap();
    // We want expected output but we can't intercept or read what goes to stdout, so we just make
    // it expected return value for now
    let expected_result = if last_line.starts_with("// ") {
        let s: String = last_line.chars().skip(3).collect();
        let as_u64: Option<u64> = s.parse().ok();
        as_u64
    } else {
        None
    };
    let mut run_cmd = std::process::Command::new(format!("{}/{}.out", out_dir, filename));
    if let Some(exp) = expected_result {
        println!("\tExpected: {exp}");
    }
    let run_status = run_cmd.status().unwrap();
    if !run_status.success() {
        bail!(
            "TEST CASE FAILED EXECUTION: {}, exit code: {:?}, signal: {:?}",
            filename,
            run_status.code(),
            run_status.signal()
        );
    };
    Ok(())
}

#[cfg(test)]
#[test]
pub fn run_all() -> Result<()> {
    let ctx = Context::create();
    let test_dir = "test_src";
    let mut total = 0;
    let mut success = 0;
    let mut failures: Vec<String> = Vec::new();
    for dir_entry in std::fs::read_dir(test_dir)? {
        let dir_entry = dir_entry?;
        let metadata = dir_entry.metadata()?;
        if metadata.is_file() {
            let result = test_file(&ctx, dir_entry.path());
            if result.is_ok() {
                success += 1;
            } else {
                failures.push(dir_entry.path().file_name().unwrap().to_str().unwrap().to_string());
                eprintln!("Test failed: {}", result.unwrap_err());
            }
            total += 1;
        }
    }
    if success != total {
        eprintln!("Failed tests: {:?}", failures);
        bail!("{} tests failed", total - success);
    } else {
        eprintln!("Ran {} tests, {} succeeded", total, success);
    }
    Ok(())
}
