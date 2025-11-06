#include <string.h>
#include <sys/mman.h>
typedef struct {
  long *x;
  long y;
  long z;
} Point;

Point make_point() {
  Point p;
  long x = 42;
  p.x = &x;
  p.y = 13;
  return p;
}

int main() {
  off_t asdf;
  void *addr = mmap(NULL, 1024, );
  return 0;
}
