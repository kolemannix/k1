#include <array>
#include <cstdio>

constexpr long N = 10000;
#define COMPTIME 1

#if COMPTIME
#define CT constexpr
#else
#define CT
#endif

struct summary { long count; long last; };

CT summary sieve() {
    std::array<bool, N> composite{};
    std::array<long, N> primes{};
    long count = 0;
    for (long i = 2; i < N; i++) {
        if (!composite[i]) {
            primes[count++] = i;
            for (long j = i * i; j < N; j += i) composite[j] = true;
        }
    }
    return {count, primes[count - 1]};
}

CT summary result = sieve();

int main() { printf("%ld %ld\n", result.count, result.last); }
