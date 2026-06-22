// file.cpp
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct cinput_i2 {
  int x; int y;
} cinput_i2;

union cinput_Union1_u {
  int x;
  float f;
  long l;
};
typedef union cinput_Union1_u cinput_Union1_t;

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
  cinput_i2 _i2;
  struct cinput_i2 _struct_i2;
  struct { int x; int y; } _struct_anon;
  cinput_Union1_t _union_1;
  union cinput_Union1_u _union_union1;
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
  // Color_Blue should get excluded due to explicit exclusion
  Color_Blue = 42
};

enum Size { 
  Size_Small,
  Size_Medium,
  Size_Large
};

// should get excluded due to dunder
struct my__internal {};

// should get mangled; loop_c
struct cinput_loop {};

struct cinput_custom_name {};

struct opaq {
  char b[100];
  int x;
};

// should have correct link name
int cinput_main(void) {
  return 0;
}
