#include <stdio.h>

int main() {
  char c = 'A';
  if (fputc(c, stdout) == EOF) {
    perror("fputc failed");
    return 1;
  }
  return 0;
}
