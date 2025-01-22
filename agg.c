#include <stdio.h>

struct P3 { long x; long y; long z; };

long one(struct P3 p) {
  return p.x;
}

int main() {
  struct P3 p;
  p.x = 3;
  p.y = 4;
  p.z = 0;
  return (int)one(p);
}
