pub const PRELUDE_SOURCE: &str = r#"fn printInt(value: int): unit
type Array = {}
fn _arrayIndex<T>(array: Array<T>, index: int): T 
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 4;
