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

#include "k1rt_backtrace.c"
