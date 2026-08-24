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

int cinput_sum(int lhs, unsigned int rhs) {
  return lhs + rhs;
}

int cinput_unnamed_param(int, unsigned int named, float loop);

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

#define CINPUT_ANSWER 42
#define CINPUT_HEX 0xFF
#define CINPUT_SHIFTED (1u << 4)
#define CINPUT_COMBO (CINPUT_ANSWER + CINPUT_SHIFTED * 2)
#define CINPUT_NEG (-7)
#define CINPUT_OCTAL 017
#define CINPUT_BIGMASK 0xFFFFFFFFFFFFFFFFULL
#define CINPUT_ALIAS CINPUT_ANSWER
#define CINPUT_CHAR 'A'
#define CINPUT_FLOAT 2.5
#define CINPUT_FLOATF 1.5f
#define CINPUT_STR "hello\tworld"
#define CINPUT_STR2 "ab" "cd"
#define CINPUT_INVERTED (~CINPUT_HEX)
#define CINPUT_NEGREF (-CINPUT_ANSWER)
#define CINPUT_STRALIAS CINPUT_STR
#define CINPUT_FLOATALIAS CINPUT_FLOAT
// the rest should all be skipped
#define CINPUT_GUARD
#define CINPUT_FNLIKE(x) ((x) + 1)
#define CINPUT_ATTR __attribute__((deprecated))
#define CINPUT_CAST ((unsigned long)5)
#define CINPUT_DANGLING (CINPUT_NOT_EMITTED + 1)
#define CINPUT_STRMATH (CINPUT_STR + 1)
