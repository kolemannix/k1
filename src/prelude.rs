pub const PRELUDE_SOURCE: &str = r#"intern fn printInt(value: int): unit
intern fn print(value: string): unit
intern fn exit(code: int): unit
fn assert(value: bool): unit {
  return if value { () } else { 
    print("ASSERT FAILED");
    exit(1)
  };
}
type Array = {}
namespace string {
  intern fn length(self: string): int
}
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 14;
