#include <stdint.h>
#include <stdio.h>

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




