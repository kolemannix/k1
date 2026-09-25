#include <chrono>
#include <cstdint>
#include <cstdio>
#include <string>
std::string buf[1024];
__attribute__((noinline)) void from_int(uint64_t n) {
  for (uint64_t i = 0; i < n; i++) buf[i & 1023] = std::to_string(i);
}
int main() {
  uint64_t n = 100000000;
  double best = 1e30;
  for (int r = 0; r < 5; r++) {
    auto t0 = std::chrono::steady_clock::now();
    from_int(n);
    double s = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
    if (s < best) best = s;
  }
  printf("C++ std::to_string: %.1f million strings per second (last=%s)\n", n / best / 1e6, buf[(n - 1) & 1023].c_str());
}
