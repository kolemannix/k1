#include <stdint.h>

extern int printf(const char *, ...);

typedef struct { char x; } VerySmall;
// define i8 @very_small(i64 %0, i64 %1) #0
VerySmall very_small(VerySmall a, VerySmall b) {
  VerySmall r = {0};
  r.x += a.x;
  r.x += b.x;
  printf("very small, a.x=%d, b.x=%d, r.x=%d\n", a.x, b.x, r.x);
  return r;
}

typedef struct { char r; char g; char b; char a; } Small;
Small small(Small a, Small b) {
  Small r = {0};
  r.r += a.r;
  r.g += a.g;
  r.b += a.b;
  r.a += a.a;
  r.r += b.r;
  r.g += b.g;
  r.b += b.b;
  r.a += b.a;
  return r;
}

typedef struct { uint64_t a; uint64_t b; } Medium;
Medium medium(Medium a, Medium b) {
  Medium r = {0};
  r.a += a.a;
  r.b += a.b;
  r.a += b.a;
  r.b += b.b;
  return r;
}

typedef struct { float f1; float f2; short i3[4]; } MixedClassMedium;

typedef struct { Medium med1; Medium med2; } Large;
Large large(Large a, Large b) {
  Large r = {0};
  r.med1.a += a.med1.a;
  r.med1.b += a.med1.b;
  r.med2.a += a.med2.a;
  r.med2.b += a.med2.b;
  r.med1.a += b.med1.a;
  r.med1.b += b.med1.b;
  r.med2.a += b.med2.a;
  r.med2.b += b.med2.b;
  return r;
}

typedef struct { float f1; float f2; } HFASmall;
void hfa_small(HFASmall hfa1) {
  return;
}
typedef struct { double f1; double f2; double f3; double f4; } HFABig;
void hfa_big(HFABig hfa1) {
  return;
}

enum CXCursorKind {
  UnexposedDecl = 1,
  StructDecl = 2,
};
typedef struct {
  enum CXCursorKind kind;
  int xdata;
  const void *data[3];
} CXCursor;

CXCursor rtCursor(CXCursor in) {
  CXCursor out;
  out.kind = in.kind;
  out.xdata = in.xdata;
  out.data[0] = in.data[0];
  out.data[1] = in.data[1];
  out.data[2] = in.data[2];
  printf("rtCursor, kind=%d, xdata=%d, data[0]=%p, data[1]=%p, data[2]=%p\n", in.kind, in.xdata, in.data[0], in.data[1], in.data[2]);
  return out;
}


typedef struct {
  const void *data;
  unsigned private_flags;
} CXString;

CXString getFileName(void *SFile) {
  CXString s;
  s.data = (void*)3;
  s.private_flags = 21;
  return s;
}


typedef struct {
  const void *ptr_data[2];
  unsigned int_data;
} CXSourceLocation;

CXSourceLocation getCursorLocation(CXCursor c) {
  CXSourceLocation locn;
  locn.ptr_data[0] = (void*)1;
  locn.ptr_data[1] = (void*)2;
  locn.int_data = 42;
  return locn;
}
