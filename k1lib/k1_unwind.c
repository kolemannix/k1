#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include <stdio.h>

void _k1_show_backtrace(void) {
  unw_cursor_t cursor;
  unw_context_t uc;
  unw_word_t ip, sp, off;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);
  char symbol[256] = {"<unknown>"};
  char *name = symbol;

  while (unw_step(&cursor) > 0) {
    if ( !unw_get_proc_name(&cursor, symbol, sizeof(symbol), &off) ) {
      name = symbol;
    }
    unw_get_reg(&cursor, UNW_REG_IP, &ip);
    unw_get_reg(&cursor, UNW_REG_SP, &sp);
    printf("%-30s [ ip = %lx, sp = %lx ]\n", name, (long) ip, (long) sp);
  }
}
