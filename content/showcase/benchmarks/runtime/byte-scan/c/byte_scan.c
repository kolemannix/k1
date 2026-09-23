#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE ((size_t)256 * 1024 * 1024)
#define REPEAT 8
#define NEWLINE_STRIDE 4093
#define DELIM_STRIDE 1000003

static uint64_t xorshift(uint64_t *s) {
    uint64_t x = *s;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    return *s = x;
}

static void fill(unsigned char *buf, size_t n) {
    uint64_t *words = (uint64_t *)buf;
    uint64_t s = 0x2545F4914F6CDD1Dull;
    for (size_t i = 0; i < n / 8; i++)
        words[i] = (xorshift(&s) & 0x1f1f1f1f1f1f1f1full) | 0x4040404040404040ull;
    for (size_t i = NEWLINE_STRIDE; i < n; i += NEWLINE_STRIDE) buf[i] = '\n';
    static const unsigned char marks[3] = {',', ':', '"'};
    size_t which = 0;
    for (size_t d = DELIM_STRIDE; d < n; d += DELIM_STRIDE) {
        buf[d] = marks[which];
        which = (which + 1) % 3;
    }
}

static long count_newlines(const unsigned char *buf, size_t n) {
    long count = 0;
    const unsigned char *p = buf, *end = buf + n;
    while ((p = memchr(p, '\n', end - p)) != NULL) {
        count++;
        p++;
    }
    return count;
}

static const char *first_of(const char *p, const char *end) {
    const char *a = memchr(p, ',', end - p), *b = memchr(p, ':', end - p), *c = memchr(p, '"', end - p);
    const char *m = a;
    if (b != NULL && (m == NULL || b < m)) m = b;
    if (c != NULL && (m == NULL || c < m)) m = c;
    return m;
}

static void scan_delims(const char *buf, long *found, long *index_sum, long *which_sum) {
    const char *p = buf, *end = buf + SIZE;
    while ((p = first_of(p, end)) != NULL) {
        *found += 1;
        *index_sum += p - buf;
        *which_sum += *p == ',' ? 0 : *p == ':' ? 1 : 2;
        p++;
    }
}

int main(void) {
    unsigned char *buf = malloc(SIZE);
    fill(buf, SIZE);
    long absent = 0, newlines = 0, delims = 0, index_sum = 0, which_sum = 0;
    for (int r = 0; r < REPEAT; r++) {
        if (memchr(buf, 0, SIZE) != NULL) absent++;
        newlines += count_newlines(buf, SIZE);
        scan_delims((const char *)buf, &delims, &index_sum, &which_sum);
    }
    printf("absent: %ld newlines: %ld delims: %ld index-sum: %ld which-sum: %ld\n", absent, newlines, delims, index_sum, which_sum);
    free(buf);
    return 0;
}
