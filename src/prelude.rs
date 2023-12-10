pub const PRELUDE_SOURCE: &str = r#"
extern fn _nx_charToString(c: char): string
extern fn _nx_charToInt(c: char): int
extern fn _nx_intToChar(i: int): char
extern fn _nx_readFileToString(path: string): string
intern fn printInt(value: int): unit
intern fn print(value: string): unit
intern fn exit(code: int): unit

namespace Files {
  fn readToString(path: string): string {
    _nx_readFileToString(path)
  }
}

fn printBool(value: bool): unit {
  if value { print("true") } else { print("false") }
}
fn assert(value: bool): unit {
  if value { () } else { 
    print("ASSERT FAILED");
    exit(1)
  };
}
namespace char {
  fn fromInt(value: int): char {
    _nx_intToChar(value)
  }
  fn toInt(self: char): int {
    _nx_charToInt(self)
  }
  fn toString(self: char): string {
    _nx_charToString(self)
  }
}
fn println(value: string): unit {
  print(value);
  print(char::fromInt(10).toString());
}
type Array = {}

namespace Array {
  intern fn new<T>(len: int): Array<T>
  intern fn length(self: Array): int
  intern fn capacity(self: Array): int
  intern fn grow<T>(self: Array<T>): unit
  intern fn set_length<T>(self: Array<T>, new_length: int): unit
  fn push<T>(self: Array<T>, elem: T): unit {
    val start_length = self.length();
    if start_length == self.capacity() {
      self.grow<T>();
    };
    self[start_length] = elem;
    self.set_length<T>(start_length + 1);
  }
}
namespace string {
  intern fn fromChars(bytes: Array<char>): string
  intern fn length(self: string): int
  fn index_of(self: string, c: char): int {
    mut i = 0;
    mut ret = -1;
    while (ret == -1 and i < self.length()) {
      if (self[i] == c) {
        ret = i;
      };
      i = i + 1;
    };
    ret
  }
  fn concat(self: string, other: string): string {
    val new_length = self.length() + other.length();
    val copied = Array::new<char>(new_length);
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
    fromChars(copied)
  }
}
// -------- END PRELUDE --------"#;
pub const PRELUDE_LINES: usize = 87;
