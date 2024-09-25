#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  char* s = malloc(1024);
  int64_t x = 45;
  int len = snprintf(NULL, 0, "%lld", x) + 1;
  printf("len: %d\n", len);
  snprintf(s, len, "%lld", x);

  printf("Buffer is: %s", s);
  return 0;
}
