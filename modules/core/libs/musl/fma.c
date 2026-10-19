/* musl libc src/math/fma.c, src/math/scalbn.c and src/math/fmaf.c; MIT, see
 * COPYRIGHT. fmaf is FreeBSD /usr/src/lib/msun/src/s_fmaf.c:
 *
 * Copyright (c) 2005-2011 David Schultz <das@FreeBSD.ORG>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <float.h>

static double scalbn(double x, int n) {
  union {
    double f;
    uint64_t i;
  } u;
  double y = x;

  if (n > 1023) {
    y *= 0x1p1023;
    n -= 1023;
    if (n > 1023) {
      y *= 0x1p1023;
      n -= 1023;
      if (n > 1023)
        n = 1023;
    }
  } else if (n < -1022) {
    y *= 0x1p-1022 * 0x1p53;
    n += 1022 - 53;
    if (n < -1022) {
      y *= 0x1p-1022 * 0x1p53;
      n += 1022 - 53;
      if (n < -1022)
        n = -1022;
    }
  }
  u.i = (uint64_t)(0x3ff + n) << 52;
  x = y * u.f;
  return x;
}

#define ASUINT64(x) ((union {double f; uint64_t i;}){x}).i
#define ZEROINFNAN (0x7ff - 0x3ff - 52 - 1)

struct num {
  uint64_t m;
  int e;
  int sign;
};

static struct num normalize(double x) {
  uint64_t ix = ASUINT64(x);
  int e = ix >> 52;
  int sign = e & 0x800;
  e &= 0x7ff;
  if (!e) {
    ix = ASUINT64(x * 0x1p63);
    e = ix >> 52 & 0x7ff;
    e = e ? e - 63 : 0x800;
  }
  ix &= (1ull << 52) - 1;
  ix |= 1ull << 52;
  ix <<= 1;
  e -= 0x3ff + 52 + 1;
  return (struct num){ix, e, sign};
}

static void mul(uint64_t *hi, uint64_t *lo, uint64_t x, uint64_t y) {
  uint64_t t1, t2, t3;
  uint64_t xlo = (uint32_t)x, xhi = x >> 32;
  uint64_t ylo = (uint32_t)y, yhi = y >> 32;

  t1 = xlo * ylo;
  t2 = xlo * yhi + xhi * ylo;
  t3 = xhi * yhi;
  *lo = t1 + (t2 << 32);
  *hi = t3 + (t2 >> 32) + (t1 > *lo);
}

double fma(double x, double y, double z) {
  struct num nx, ny, nz;
  nx = normalize(x);
  ny = normalize(y);
  nz = normalize(z);

  if (nx.e >= ZEROINFNAN || ny.e >= ZEROINFNAN)
    return x * y + z;
  if (nz.e >= ZEROINFNAN) {
    if (nz.e > ZEROINFNAN)
      return x * y;
    return z;
  }

  uint64_t rhi, rlo, zhi, zlo;
  mul(&rhi, &rlo, nx.m, ny.m);

  int e = nx.e + ny.e;
  int d = nz.e - e;
  if (d > 0) {
    if (d < 64) {
      zlo = nz.m << d;
      zhi = nz.m >> (64 - d);
    } else {
      zlo = 0;
      zhi = nz.m;
      e = nz.e - 64;
      d -= 64;
      if (d == 0) {
      } else if (d < 64) {
        rlo = rhi << (64 - d) | rlo >> d | !!(rlo << (64 - d));
        rhi = rhi >> d;
      } else {
        rlo = 1;
        rhi = 0;
      }
    }
  } else {
    zhi = 0;
    d = -d;
    if (d == 0) {
      zlo = nz.m;
    } else if (d < 64) {
      zlo = nz.m >> d | !!(nz.m << (64 - d));
    } else {
      zlo = 1;
    }
  }

  int sign = nx.sign ^ ny.sign;
  int samesign = !(sign ^ nz.sign);
  int nonzero = 1;
  if (samesign) {
    rlo += zlo;
    rhi += zhi + (rlo < zlo);
  } else {
    uint64_t t = rlo;
    rlo -= zlo;
    rhi = rhi - zhi - (t < rlo);
    if (rhi >> 63) {
      rlo = -rlo;
      rhi = -rhi - !!rlo;
      sign = !sign;
    }
    nonzero = !!rhi;
  }

  if (nonzero) {
    e += 64;
    d = __builtin_clzll(rhi) - 1;
    rhi = rhi << d | rlo >> (64 - d) | !!(rlo << d);
  } else if (rlo) {
    d = __builtin_clzll(rlo) - 1;
    if (d < 0)
      rhi = rlo >> 1 | (rlo & 1);
    else
      rhi = rlo << d;
  } else {
    return x * y + z;
  }
  e -= d;

  int64_t i = rhi;
  if (sign)
    i = -i;
  double r = i;

  if (e < -1022 - 62) {
    if (e == -1022 - 63) {
      double c = 0x1p63;
      if (sign)
        c = -c;
      if (r == c) {
        float fltmin = 0x0.ffffff8p-63 * FLT_MIN * r;
        return DBL_MIN / FLT_MIN * fltmin;
      }
      if (rhi << 53) {
        i = rhi >> 1 | (rhi & 1) | 1ull << 62;
        if (sign)
          i = -i;
        r = i;
        r = 2 * r - c;
        {
          double tiny = DBL_MIN / FLT_MIN * r;
          r += (double)(tiny * tiny) * (r - r);
        }
      }
    } else {
      d = 10;
      i = (rhi >> d | !!(rhi << (64 - d))) << d;
      if (sign)
        i = -i;
      r = i;
    }
  }
  return scalbn(r, e);
}

float fmaf(float x, float y, float z) {
  double xy, result;
  union {
    double f;
    uint64_t i;
  } u;
  int e;

  xy = (double)x * y;
  result = xy + z;
  u.f = result;
  e = u.i >> 52 & 0x7ff;
  if ((u.i & 0x1fffffff) != 0x10000000 || e == 0x7ff ||
      (result - xy == z && result - z == xy)) {
    z = result;
    return z;
  }

  double err;
  int neg = u.i >> 63;
  if (neg == (z > xy))
    err = xy - result + z;
  else
    err = z - result + xy;
  if (neg == (err < 0))
    u.i++;
  else
    u.i--;
  z = u.f;
  return z;
}
