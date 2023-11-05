use std::path::Path;

use anyhow::{anyhow, bail, Result};
use inkwell::context::Context;

fn test_file<'ctx, P: AsRef<Path>>(ctx: &'ctx Context, path: P) -> Result<()> {
    let path = path.as_ref();
    let filename = path.file_name().unwrap().to_str().unwrap();
    let src = std::fs::read_to_string(path)?;
    println!("********** {:?} **********", filename);
    let out_dir = "nx-out/test_suite";
    crate::compile_single_file_program(ctx, filename, &src, false, out_dir)
        .map_err(|err| anyhow!("TEST CASE FAILED COMPILE: {}. Reason: {}", filename, err))?;
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
    let run_output = run_cmd.output().unwrap();
    let output = String::from_utf8(run_output.stdout).unwrap();
    match expected_result {
        None => {
            println!("{filename}:\n{}", output);
        }
        Some(exp) => {
            println!("Expected: {exp}");
            println!("Output: {}", output);
        }
    }
    if !run_output.status.success() {
        bail!(
            "TEST CASE FAILED EXECUTION: {}, exit code: {}",
            filename,
            run_output.status.code().unwrap()
        );
    };
    Ok(())
}

#[cfg(test)]
#[test]
pub fn run_all() -> Result<()> {
    let ctx = Context::create();
    let test_dir = "resources/test_src";
    for dir_entry in std::fs::read_dir(test_dir)? {
        let dir_entry = dir_entry?;
        let metadata = dir_entry.metadata()?;
        if metadata.is_file() {
            test_file(&ctx, dir_entry.path())?;
        }
    }
    Ok(())
}
