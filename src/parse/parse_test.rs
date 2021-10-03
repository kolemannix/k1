use crate::parse::*;
use crate::ast::*;

#[test]
fn test1() -> Result<(), String> {
  let parse_result = parse_file("resources/nixsample/hello_world.nx")?;
  println!("{:?}", parse_result);
  assert_eq!(parse_result.name.0, "module1".to_string());
  Ok(())
}