/*
 * Copyright (c) 2025 knix
 * All rights reserved.
 */

#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"
#include <stddef.h>
#include <stdint.h>

int _k1_snprintf_f64(char *buf, size_t size, double arg, int32_t places) {
  if (places == -1)
    return stbsp_snprintf(buf, size, "%f", arg);
  else
    return stbsp_snprintf(buf, size, "%.*f", places, arg);
}
int _k1_snprintf_f32(char *buf, size_t size, float arg, int32_t places) {
  if (places == -1)
    return stbsp_snprintf(buf, size, "%f", arg);
  else
    return stbsp_snprintf(buf, size, "%.*f", places, arg);
}

#define FFC_IMPL
#include "ffc.h"
