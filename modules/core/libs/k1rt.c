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

/* wasm links no libc; ffreestanding keeps clang from converting these loops back into themselves! */
#if defined(__wasm__)
void *memcpy(void *dst, const void *src, size_t n) {
  unsigned char *d = dst;
  const unsigned char *s = src;
  for (size_t i = 0; i < n; i++)
    d[i] = s[i];
  return dst;
}
void *memmove(void *dst, const void *src, size_t n) {
  unsigned char *d = dst;
  const unsigned char *s = src;
  if (d < s)
    for (size_t i = 0; i < n; i++)
      d[i] = s[i];
  else
    for (size_t i = n; i > 0; i--)
      d[i - 1] = s[i - 1];
  return dst;
}
void *memset(void *dst, int value, size_t n) {
  unsigned char *d = dst;
  for (size_t i = 0; i < n; i++)
    d[i] = (unsigned char)value;
  return dst;
}
int memcmp(const void *p1, const void *p2, size_t n) {
  const unsigned char *a = p1;
  const unsigned char *b = p2;
  for (size_t i = 0; i < n; i++) {
    if (a[i] != b[i])
      return a[i] < b[i] ? -1 : 1;
  }
  return 0;
}

/* compiler-rt libcall for i128 multiply (ffc's u128 products); built from
 * 64x64->64 multiplies only so it cannot lower back into itself */
static unsigned __int128 _k1_mul_u64_full(uint64_t a, uint64_t b) {
  uint64_t a_lo = a & 0xffffffffu, a_hi = a >> 32;
  uint64_t b_lo = b & 0xffffffffu, b_hi = b >> 32;
  uint64_t t = a_lo * b_lo;
  uint64_t w0 = t & 0xffffffffu;
  uint64_t k = t >> 32;
  t = a_hi * b_lo + k;
  uint64_t w1 = t & 0xffffffffu;
  uint64_t w2 = t >> 32;
  t = a_lo * b_hi + w1;
  uint64_t hi = a_hi * b_hi + w2 + (t >> 32);
  uint64_t lo = (t << 32) | w0;
  return ((unsigned __int128)hi << 64) | lo;
}
__int128 __multi3(__int128 a, __int128 b) {
  unsigned __int128 ua = (unsigned __int128)a;
  unsigned __int128 ub = (unsigned __int128)b;
  uint64_t a_lo = (uint64_t)ua, a_hi = (uint64_t)(ua >> 64);
  uint64_t b_lo = (uint64_t)ub, b_hi = (uint64_t)(ub >> 64);
  unsigned __int128 r = _k1_mul_u64_full(a_lo, b_lo);
  uint64_t r_hi = (uint64_t)(r >> 64) + a_lo * b_hi + a_hi * b_lo;
  return (__int128)(((unsigned __int128)r_hi << 64) | (uint64_t)r);
}
#endif // end wasm
