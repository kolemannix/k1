// file.cpp
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct i2 {
  int x; int y;
} i2;

union Union1_u {
  int x;
  float f;
  long l;
};
typedef union Union1_u Union1_t;

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
  struct { int x; int y; } _struct_anon;
  Union1_t _union_1;
  union Union1_u _union_union1;
  union {
    int x; float y; char c[4];
  } _union_anon;
  float _muh_floats[];
};

void myFunction(char incompleteArray[]) {
  return;
}

enum Color {
  Color_Red = 577,
  Color_Green = -10,
  Color_Blue = 42
};

enum Size { 
  Size_Small,
  Size_Medium,
  Size_Large
};

struct opaq {
  char b[100];
  int x;
};

int main(void) {
  return 0;
}
