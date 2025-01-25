#include <stdio.h>

struct P3 { long x; long y; long z; };

long one(struct P3 p) {
  return p.x;
}

struct P3 structRet() {
  struct P3 p;
  p.x = 1;
  p.y = 2;
  p.z = 3;
  return p;
}

int main() {
  struct P3 p;
  p.x = 3;
  p.y = 4;
  p.z = 0;
  struct P3 p3 = structRet();
  return (int)p3.z;
}
