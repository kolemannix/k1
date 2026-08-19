/* Freestanding consumer: the "bootstrap in C, logic in k1" shape. The
 * bootstrap owns _start, syscalls, and printing; the k1 library owns logic. */

extern long k1_add(long a, long b);
extern long k1_sum(const long *data, unsigned long count);

static long sys_write(int fd, const void *buf, unsigned long n) {
  long ret;
  __asm__ volatile("syscall"
                   : "=a"(ret)
                   : "a"(1), "D"((long)fd), "S"(buf), "d"(n)
                   : "rcx", "r11", "memory");
  return ret;
}

static void sys_exit(int code) {
  __asm__ volatile("syscall" : : "a"(231), "D"((long)code));
  __builtin_unreachable();
}

int main(void) {
  long data[4] = {10, 20, 30, 40};
  long value = k1_sum(data, 4) + k1_add(2, 1); /* 103 */

  char buf[32];
  int i = 32;
  buf[--i] = '\n';
  if (value == 0)
    buf[--i] = '0';
  while (value > 0 && i > 0) {
    buf[--i] = (char)('0' + value % 10);
    value /= 10;
  }
  sys_write(1, buf + i, (unsigned long)(32 - i));
  return 0;
}

void _k1_start(void) { sys_exit(main()); }

__asm__(".global _start\n"
        "_start:\n"
        "  xorl %ebp, %ebp\n"
        "  andq $-16, %rsp\n"
        "  callq _k1_start\n"
        "  hlt\n");
