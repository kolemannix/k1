pub const PRELUDE_SOURCE: &str = r#"intern fn printInt(value: int): unit
intern fn print(value: string): unit
intern fn exit(code: int): unit
fn assert(value: bool): unit {
  return if value { () } else { 
    print("ASSERT FAILED");
    exit(1)
  };
}
namespace char {
  intern fn to_string(self: char): string
}
type Array = {}
intern fn array_new<T>(len: int): Array<T>
namespace Array {
  intern fn length(self: Array): int
}
intern fn string_new(bytes: Array<char>): string
namespace string {
  intern fn length(self: string): int
  fn concat(self: string, other: string): string {
    val new_length = self.length() + other.length();
    val copied = array_new<char>(new_length);
    mut i = 0;
    while (i < self.length()) {
      copied[i] = self[i];
      i = i + 1;
    };
    i = 0;
    while (i < other.length()) {
      copied[i + self.length()] = other[i];
      i = i + 1;
    };
    return string_new(copied);
  }
}
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 37;
