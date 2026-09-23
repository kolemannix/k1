#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint64_t *keys, *vals;
    size_t cap, count;
    unsigned shift;
} table;

static uint64_t xorshift(uint64_t *s) {
    uint64_t x = *s;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    return *s = x;
}

static size_t slot(const table *t, uint64_t key) {
    return (size_t)((key * 0x9E3779B97F4A7C15ull) >> t->shift);
}

static void table_init(table *t, unsigned log2cap) {
    t->cap = (size_t)1 << log2cap;
    t->shift = 64 - log2cap;
    t->count = 0;
    t->keys = calloc(t->cap, sizeof *t->keys);
    t->vals = malloc(t->cap * sizeof *t->vals);
}

static void table_insert(table *t, uint64_t key, uint64_t val);

static void table_grow(table *t) {
    table old = *t;
    table_init(t, 65 - old.shift);
    for (size_t i = 0; i < old.cap; i++)
        if (old.keys[i]) table_insert(t, old.keys[i], old.vals[i]);
    free(old.keys);
    free(old.vals);
}

static void table_insert(table *t, uint64_t key, uint64_t val) {
    if ((t->count + 1) * 2 > t->cap) table_grow(t);
    size_t mask = t->cap - 1, i = slot(t, key);
    while (t->keys[i] != 0 && t->keys[i] != key) i = (i + 1) & mask;
    if (t->keys[i] == 0) {
        t->keys[i] = key;
        t->count++;
    }
    t->vals[i] = val;
}

static const uint64_t *table_get(const table *t, uint64_t key) {
    size_t mask = t->cap - 1, i = slot(t, key);
    while (t->keys[i] != 0) {
        if (t->keys[i] == key) return &t->vals[i];
        i = (i + 1) & mask;
    }
    return NULL;
}

int main(void) {
    uint64_t n = 5000000;
    table m;
    table_init(&m, 4);
    uint64_t keys = 0x9E3779B97F4A7C15ull;
    for (uint64_t i = 0; i < n; i++) table_insert(&m, xorshift(&keys), i);
    uint64_t hits = 0x9E3779B97F4A7C15ull, misses = 0x2545F4914F6CDD1Dull;
    uint64_t found = 0, sum = 0;
    for (uint64_t i = 0; i < n; i++) {
        uint64_t hit_key = xorshift(&hits);
        uint64_t miss_key = xorshift(&misses);
        const uint64_t *v = table_get(&m, i % 2 == 0 ? hit_key : miss_key);
        if (v) {
            found++;
            sum += *v;
        }
    }
    printf("inserted: %zu found: %llu sum: %llu\n", m.count, (unsigned long long)found, (unsigned long long)sum);
    return 0;
}
