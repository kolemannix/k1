pub const PRELUDE_SOURCE: &str = r#"
fn printInt(value: int): unit {
  return ();
}
type Array = {}
fn _arrayIndex(array: Array[int], index: int): int {
  return 0;
}
// fn _arrayIndexT[T](array: Array[T], index: int): T {
//   return 0;
// }
// -------- END PRELUDE --------
"#;
