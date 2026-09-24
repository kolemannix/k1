/*
 * Copyright (c) 2025 knix
 * All rights reserved.
 */

#define NDEBUG
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define FFC_IMPL
#include "ffc.h"

#define RYU_OPTIMIZE_SIZE
#ifdef __wasm__
#define RYU_ONLY_64_BIT_OPS
#endif
#include "ryu/d2s.c"
#include "ryu/f2s.c"

static int put_str(char *out, const char *s) {
  int n = 0;
  while ((out[n] = s[n]))
    n++;
  return n;
}

static int put_uint(char *out, uint64_t v) {
  int n = 0;
  for (uint64_t t = v; t != 0; t /= 10)
    n++;
  for (int i = n; i-- > 0; v /= 10)
    out[i] = (char)('0' + v % 10);
  return n;
}

static int put_digits(char *out, const char *digits, int n, int point) {
  int o = 0;
  if (point <= 0) {
    out[o++] = '0';
    out[o++] = '.';
    for (int i = point; i < 0; i++)
      out[o++] = '0';
    for (int i = 0; i < n; i++)
      out[o++] = digits[i];
    return o;
  }
  for (int i = 0; i < point; i++)
    out[o++] = i < n ? digits[i] : '0';
  out[o++] = '.';
  if (point >= n)
    out[o++] = '0';
  for (int i = point; i < n; i++)
    out[o++] = digits[i];
  return o;
}

static int put_shortest(char *out, bool neg, uint64_t mantissa, int32_t exp10) {
  char digits[20];
  int n = put_uint(digits, mantissa);
  int o = 0;
  if (neg)
    out[o++] = '-';
  int32_t x = exp10 + n - 1;
  if (x >= -4 && x < 16)
    return o + put_digits(out + o, digits, n, x + 1);
  o += put_digits(out + o, digits, n, 1);
  out[o++] = 'e';
  if (x < 0) {
    out[o++] = '-';
    x = -x;
  }
  return o + put_uint(out + o, (uint64_t)x);
}

static int shortest_f64(char *out, double d) {
  uint64_t bits = double_to_bits(d);
  bool neg = bits >> 63;
  uint64_t m = bits & ((1ull << 52) - 1);
  uint32_t e = (bits >> 52) & 0x7FF;
  if (e == 0x7FF)
    return put_str(out, m ? "nan" : neg ? "-inf" : "inf");
  if (e == 0 && m == 0)
    return put_str(out, neg ? "-0.0" : "0.0");
  floating_decimal_64 v;
  if (!d2d_small_int(m, e, &v))
    v = d2d(m, e);
  return put_shortest(out, neg, v.mantissa, v.exponent);
}

static int shortest_f32(char *out, float f) {
  uint32_t bits = float_to_bits(f);
  bool neg = bits >> 31;
  uint32_t m = bits & ((1u << 23) - 1);
  uint32_t e = (bits >> 23) & 0xFF;
  if (e == 0xFF)
    return put_str(out, m ? "nan" : neg ? "-inf" : "inf");
  if (e == 0 && m == 0)
    return put_str(out, neg ? "-0.0" : "0.0");
  floating_decimal_32 v = f2d(m, e);
  return put_shortest(out, neg, v.mantissa, v.exponent);
}

int _k1_snprintf_f64(char *buf, size_t size, double arg, int32_t places) {
  if (places != -1)
    return (int)ffc_format_double_fixed(buf, size, arg, places);
  return size < 32 ? -1 : shortest_f64(buf, arg);
}
int _k1_snprintf_f32(char *buf, size_t size, float arg, int32_t places) {
  if (places != -1)
    return (int)ffc_format_double_fixed(buf, size, arg, places);
  return size < 32 ? -1 : shortest_f32(buf, arg);
}

/* wasm and no-crt link no libc; ffreestanding keeps clang from converting
 * these loops back into themselves! */
#if defined(__wasm__) || defined(K1_NOCRT)
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
/* llvm lowers equality-only memcmp calls to bcmp on linux */
int bcmp(const void *p1, const void *p2, size_t n) { return memcmp(p1, p2, n); }

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

#include "musl/fmod.c"
#endif // end wasm
