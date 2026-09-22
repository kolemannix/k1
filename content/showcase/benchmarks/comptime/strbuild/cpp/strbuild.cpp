#include <cstdint>
#include <cstdio>
#include <string>

constexpr int N = 10000;
#define COMPTIME 1

#if COMPTIME
#define CT constexpr
#else
#define CT
#endif

struct summary { size_t len; uint32_t sum; };

CT void append_decimal(std::string& s, int v) {
    char digits[12];
    int n = 0;
    do { digits[n++] = char('0' + v % 10); v /= 10; } while (v > 0);
    while (n > 0) s += digits[--n];
}

CT uint32_t fnv1a(const std::string& s) {
    uint32_t h = 2166136261;
    for (char c : s) h = (h ^ uint8_t(c)) * 16777619;
    return h;
}

CT summary build() {
    std::string s;
    for (int i = 0; i < N; i++) {
        if (i > 0) s += ',';
        append_decimal(s, i);
    }
    return {s.size(), fnv1a(s)};
}

CT summary result = build();

int main() { printf("%zu %u\n", result.len, result.sum); }
