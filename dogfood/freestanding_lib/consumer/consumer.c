/* Freestanding consumer: the "bootstrap in C, logic in k1" shape. The
 * bootstrap owns _start, syscalls, and printing; the k1 library owns logic.
 * The k1_platform_* definitions below ARE the bare platform: the library's
 * object demands exactly the contract symbols its code reaches, and the
 * consumer decides what they mean. */

extern long k1_add(long a, long b);
extern long k1_sum(const long *data, unsigned long count);
extern long k1_describe_sum(const long *data, unsigned long count);

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

/* Anonymous overcommitted pages are already a lazy reservation and arrive
 * zeroed on first touch, so commit is a no-op here. A kernel consumer would
 * route these to its own paging instead. */
static void *sys_mmap(unsigned long len) {
  long ret;
  register long r10 __asm__("r10") = 0x22; /* MAP_PRIVATE|MAP_ANONYMOUS */
  register long r8 __asm__("r8") = -1;
  register long r9 __asm__("r9") = 0;
  __asm__ volatile("syscall"
                   : "=a"(ret)
                   : "a"(9), "D"(0L), "S"(len), "d"(3L /* RW */), "r"(r10),
                     "r"(r8), "r"(r9)
                   : "rcx", "r11", "memory");
  return (void *)ret;
}

void *k1_platform_mem_reserve(unsigned long min_size) {
  void *p = sys_mmap(min_size);
  if ((long)p < 0)
    sys_exit(90);
  return p;
}
void k1_platform_mem_commit(void *base, unsigned long len) {
  (void)base;
  (void)len;
}
void k1_platform_mem_release(void *base, unsigned long len) {
  long ret;
  __asm__ volatile("syscall"
                   : "=a"(ret)
                   : "a"(11), "D"(base), "S"(len)
                   : "rcx", "r11", "memory");
  (void)ret;
}
long k1_platform_io_write(int fd, const void *buf, unsigned long n) {
  return sys_write(fd, buf, n);
}
/* tty means line-buffered: k1's println flushes at the newline */
int k1_platform_io_is_tty(int fd) {
  (void)fd;
  return 1;
}
void k1_platform_process_exit(int code) { sys_exit(code); }

int main(void) {
  long data[4] = {10, 20, 30, 40};
  long value = k1_describe_sum(data, 4) + k1_add(2, 1); /* prints, then 103 */

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
