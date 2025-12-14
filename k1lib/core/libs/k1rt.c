/*
 * Copyright (c) 2025 knix
 * All rights reserved.
 */

#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"
#include <stdint.h>

int _k1_snprintf_f64(char *buf, size_t size, double arg, int32_t places) {
  return stbsp_snprintf(buf, size, "%.*f", places, arg);
}
int _k1_snprintf_f32(char *buf, size_t size, float arg, int32_t places) {
  return stbsp_snprintf(buf, size, "%.*f", places, arg);
}

#include <execinfo.h> // for backtrace
#include <stdio.h> // for printf
void _k1_print_backtrace(int max_count) {
  void *frames[max_count];
  int count = backtrace(frames, max_count);
  char **symbols = backtrace_symbols(frames, count);
  char *i = symbols[0];
  for (int i = 0; i < count; i++) {
    printf("%s\n", symbols[i]);
  }
  return;
}
