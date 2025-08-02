#include <stdio.h>

// TODO(rt) switch to stb's snprintf
int _k1_snprintf_f32(char *buf, size_t size, const char *fmt, float arg) {
  return snprintf(buf, size, "%f", arg);
}
// TODO(rt) switch to stb's snprintf
int _k1_snprintf_f64(char *buf, size_t size, const char *fmt, double arg) {
  return snprintf(buf, size, "%f", arg);
}
