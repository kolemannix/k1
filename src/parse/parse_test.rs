use crate::parse::*;
use crate::ast::*;
use std::ops::Deref;

#[test]
fn hello_world() -> Result<(), String> {
  let parse_result = parse_file("resources/nixsample/hello_world.nx")?;
  println!("{:?}", parse_result);
  assert_eq!(parse_result.name.0, "module1".to_string());
  Ok(())
}

#[test]
fn basic_fn() -> Result<(), String> {
  let Module { name, defs } = parse_file("resources/nixsample/basic_fn.nx")?;
  assert_eq!(name.0, "module1".to_string());
  if let Some(Definition::FnDef(fndef)) = defs.first() {
    assert_eq!(fndef.name.0, "basic")
  } else {
    panic!("fail")
  }
  Ok(())
}