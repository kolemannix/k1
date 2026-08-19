/*
 * Copyright (c) 2025 knix
 * All rights reserved.
 */

#include <errno.h>

int _k1_errno(void) { return errno; }

#ifdef __FILC__
#include <stdfil.h>
void _k1_print_backtrace(int max_count) {
  (void)max_count;
  zdump_stack();
}
#else
#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <unistd.h>

static int _k1_symbolize_atos(void **frames, int count) {
  char cmd[4096];
  int off = snprintf(cmd, sizeof(cmd), "atos -i -p %d", (int)getpid());
  for (int i = 0; i < count; i++) {
    int n = snprintf(cmd + off, sizeof(cmd) - (size_t)off, " 0x%llx",
                     (unsigned long long)((uintptr_t)frames[i] - 1));
    if (n < 0 || off + n + 16 >= (int)sizeof(cmd)) break;
    off += n;
  }
  snprintf(cmd + off, sizeof(cmd) - (size_t)off, " 2>/dev/null");
  fflush(stdout);
  FILE *pipe = popen(cmd, "r");
  if (pipe == NULL) return -1;
  char line[1024];
  int lines = 0;
  while (fgets(line, sizeof(line), pipe) != NULL) {
    if (line[0] == '\n') continue;
    fputs(line, stdout);
    lines += 1;
  }
  pclose(pipe);
  return lines > 0 ? 0 : -1;
}
#else
#include "vendor/libbacktrace/backtrace.h"

static struct backtrace_state *_k1_bt_state;

struct _k1_bt_ctx {
  int remaining;
  int printed;
};

static void _k1_bt_error(void *data, const char *msg, int errnum) {
  (void)data;
  (void)msg;
  (void)errnum;
}

static void _k1_bt_syminfo(void *data, uintptr_t pc, const char *symname, uintptr_t symval,
                           uintptr_t symsize) {
  (void)data;
  (void)symval;
  (void)symsize;
  if (symname != NULL) {
    printf("%s\n", symname);
  } else {
    printf("0x%lx\n", (unsigned long)pc);
  }
}

static int _k1_bt_frame(void *data, uintptr_t pc, const char *filename, int lineno,
                        const char *function) {
  struct _k1_bt_ctx *ctx = data;
  if (pc == (uintptr_t)-1) return 0;
  if (ctx->remaining <= 0) return 1;
  ctx->remaining -= 1;
  ctx->printed += 1;
  if (function != NULL && filename != NULL) {
    printf("%s at %s:%d\n", function, filename, lineno);
  } else if (function != NULL) {
    printf("%s\n", function);
  } else {
    backtrace_syminfo(_k1_bt_state, pc, _k1_bt_syminfo, _k1_bt_error, NULL);
  }
  return 0;
}
#endif

static void _k1_print_raw(void **frames, int count) {
  char **symbols = backtrace_symbols(frames, count);
  if (symbols == NULL) return;
  for (int i = 0; i < count; i++) {
    printf("%s\n", symbols[i]);
  }
  free(symbols);
}

void _k1_print_backtrace(int max_count) {
#ifdef __APPLE__
  void *frames[max_count];
  int count = backtrace(frames, max_count);
  if (_k1_symbolize_atos(frames, count) == 0) return;
  _k1_print_raw(frames, count);
#else
  if (_k1_bt_state == NULL) {
    _k1_bt_state = backtrace_create_state(NULL, 1, _k1_bt_error, NULL);
  }
  if (_k1_bt_state != NULL) {
    struct _k1_bt_ctx ctx = {max_count, 0};
    backtrace_full(_k1_bt_state, 0, _k1_bt_frame, _k1_bt_error, &ctx);
    if (ctx.printed > 0) return;
  }
  void *frames[max_count];
  int count = backtrace(frames, max_count);
  _k1_print_raw(frames, count);
#endif
}
#endif
