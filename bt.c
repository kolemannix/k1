#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

void print_backtrace(int frame_count) {
  void *frames[frame_count];
  backtrace(frames, frame_count);
  char **symbols = backtrace_symbols(frames, frame_count);
  char *i = symbols[0];
  for (int i = 0; i < frame_count; i++) {
    printf("%s\n", symbols[i]);
  }
  return;
}

void recurse(int depth) {
  if (depth == 100) {

    exit(1);
  }
  recurse(depth + 1);
}

int main(void) {
  recurse(0);
  return 0;
}
