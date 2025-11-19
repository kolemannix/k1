#include <dlfcn.h>
#include <ffi/ffi.h>
#include <stdio.h>
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
  // void *addr = mmap(NULL, 1024, 0);
  printf("MAP_PRIVATE = %#x\n", MAP_PRIVATE);
  printf("MAP_ANON    = %#x\n", MAP_ANON);
  // printf("MAP_STACK   = %#x\n", MAP_STACK);
  POSIX_MADV_SEQUENTIAL;
  MADV_SEQUENTIAL;

  void *h = dlopen("k1lib/libk1rt.dylib", RTLD_NOW | RTLD_LOCAL);
  if (!h)
    fprintf(stderr, "dlerror: %s\n", dlerror());

  printf("h: %p\n", h);
  return 0;
}
