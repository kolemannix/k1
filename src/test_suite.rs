use std::path::Path;

use anyhow::Result;
use inkwell::context::Context;

fn test_file<'ctx, P: AsRef<Path>>(ctx: &'ctx Context, path: P) -> Result<()> {
    let path = path.as_ref();
    let filename = path.file_name().unwrap().to_str().unwrap();
    let src = std::fs::read_to_string(path)?;
    println!("********** {:?} **********", filename);
    let codegen = crate::compile_single_file_program(ctx, filename, &src).map_err(|err| {
        eprintln!("TEST CASE FAILED COMPILE: {:?}", path);
        eprintln!("Reason:           {}", err);
        err
    })?;
    let result = codegen.interpret_module().map_err(|err| {
        eprintln!("TEST CASE FAILED EXECUTION: {:?}", path);
        eprintln!("Reason: {}", err);
        err
    })?;
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
    match expected_result {
        None => println!("result: {result}"),
        Some(exp) => {
            println!("result: {result} expected: {exp}");
            // assert_eq!(result, exp)
        }
    }
    Ok(())
}

#[cfg(test)]
#[test]
pub fn array_int() -> Result<()> {
    let ctx = Context::create();
    test_file(&ctx, "resources/test_src/array_int.nx")?;
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
