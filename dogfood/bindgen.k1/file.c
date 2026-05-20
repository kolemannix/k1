// file.cpp
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct i2 {
  int x; int y;
} i2;

struct foo {
  char _char;
  unsigned char _unsigned_char;
  // char16_t _char16_t;
  size_t _sizet;
  int _int;
  int* _int_p;
  bool _bool;
  float _float;
  long long _longlong;
  long _long;
  i2 _i2;
  struct i2 _struct_i2;
};

// 'consts','moduleconsts', 'newtype' and 'newtype_global'.
enum Color {
  Color_Red,
  Color_Green
};

// typedef struct opaq {
//   char b[100];
//   int x;
// } opaq;

int main(void) {
  return 0;
}
