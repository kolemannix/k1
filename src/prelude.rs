pub const PRELUDE_SOURCE: &str = r#"fn printInt(value: int): unit
fn print(value: string): unit
fn exit(code: int): unit
fn assert(value: bool): unit {
  return if value { () } else { 
    print("ASSERT FAILED");
    exit(1)
  };
}
type Array = {}
namespace string {
  fn length(self: string): int
}
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 14;
