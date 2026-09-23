MASK = (1 << 64) - 1


def xorshift(x):
    x ^= (x << 13) & MASK
    x ^= x >> 7
    x ^= (x << 17) & MASK
    return x


def main():
    n = 5_000_000
    m = {}
    key = 0x9E3779B97F4A7C15
    for i in range(n):
        key = xorshift(key)
        m[key] = i
    hit = 0x9E3779B97F4A7C15
    miss = 0x2545F4914F6CDD1D
    found = 0
    total = 0
    for i in range(n):
        hit = xorshift(hit)
        miss = xorshift(miss)
        v = m.get(miss if i & 1 else hit)
        if v is not None:
            found += 1
            total += v
    print(f"inserted: {len(m)} found: {found} sum: {total}")


main()
