#include <stdint.h>
#include <stdio.h>

typedef struct {
  int32_t a;
  int32_t b;
} klib_pair;

extern int32_t klib_add(int32_t a, int32_t b);
extern int32_t klib_scale(int32_t x);
extern klib_pair klib_pair_swap(klib_pair p);
extern int64_t klib_sum_to(int64_t n);
extern int32_t klib_answer;

int main(void) {
  printf("add=%d\n", klib_add(2, 3));
  printf("scale=%d\n", klib_scale(21));
  klib_pair p = klib_pair_swap((klib_pair){.a = 1, .b = 2});
  printf("swap=%d,%d\n", p.a, p.b);
  printf("sum=%lld\n", (long long)klib_sum_to(10));
  printf("answer=%d\n", klib_answer);
  return 0;
}
