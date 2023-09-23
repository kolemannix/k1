pub const PRELUDE_SOURCE: &str = r#"fn printInt(value: int): unit
fn exit(code: int): unit
fn assert(value: bool): unit {
  return if value { () } else { exit(1) }
}
type Array = {}
fn _arrayIndex<T>(array: Array<T>, index: int): T 
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 8;
