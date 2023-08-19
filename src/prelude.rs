pub const PRELUDE_SOURCE: &str = r#"
fn printInt(value: int): unit {
  return ();
}
type Array = {}
fn _arrayIndex(array: Array[int], index: int): int {
  return 0;
}
// -------- END PRELUDE --------
"#;
