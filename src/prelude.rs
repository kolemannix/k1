pub const PRELUDE_SOURCE: &str = r#"
fn printInt(value: int): unit
type Array = {}
// fn _arrayIndex(array: Array<int>, index: int): int {
//   return 0;
// }
fn _arrayIndex<T>(array: Array<T>, index: int): T 
// -------- END PRELUDE --------
"#;
