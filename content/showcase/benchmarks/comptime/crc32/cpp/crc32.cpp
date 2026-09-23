#include <array>
#include <cstdint>
#include <cstdio>

constexpr size_t B = 65536;
#define COMPTIME 1

#if COMPTIME
#define CT constexpr
#else
#define CT
#endif

CT std::array<uint32_t, 256> crc_table() {
    std::array<uint32_t, 256> table{};
    for (uint32_t i = 0; i < 256; i++) {
        uint32_t c = i;
        for (int k = 0; k < 8; k++) c = (c & 1) ? 0xEDB88320 ^ (c >> 1) : c >> 1;
        table[i] = c;
    }
    return table;
}

CT std::array<uint8_t, B> random_bytes() {
    std::array<uint8_t, B> bytes{};
    uint32_t x = 2463534242;
    for (auto& b : bytes) {
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        b = uint8_t(x);
    }
    return bytes;
}

CT uint32_t crc32(const std::array<uint32_t, 256>& table, const std::array<uint8_t, B>& bytes) {
    uint32_t c = 0xFFFFFFFF;
    for (uint8_t b : bytes) c = table[(c ^ b) & 0xFF] ^ (c >> 8);
    return c ^ 0xFFFFFFFF;
}

CT uint32_t checksum() { return crc32(crc_table(), random_bytes()); }

CT uint32_t sum = checksum();

int main() { printf("%u\n", sum); }
