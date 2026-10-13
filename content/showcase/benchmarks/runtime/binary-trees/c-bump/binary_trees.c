#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node { struct node *left, *right; } node;

typedef struct arena { char *base, *cursor, *end; } arena;

static arena arena_new(size_t capacity) {
    char *base = malloc(capacity);
    return (arena){ base, base, base + capacity };
}

static void arena_reset(arena *a) {
    memset(a->base, 0, (size_t)(a->cursor - a->base));
    a->cursor = a->base;
}

static node *make(arena *a, int depth) {
    if (a->cursor + sizeof(node) > a->end) abort();
    node *n = (node *)a->cursor;
    a->cursor += sizeof(node);
    if (depth > 0) {
        n->left = make(a, depth - 1);
        n->right = make(a, depth - 1);
    }
    return n;
}

static long check(const node *n) {
    return n->left == NULL ? 1 : 1 + check(n->left) + check(n->right);
}

int main(void) {
    int max_depth = 20;
    int stretch = max_depth + 1;
    size_t nodes = (size_t)1 << (stretch + 1);
    arena ambient = arena_new(nodes * sizeof(node));
    arena scratch = arena_new(nodes * sizeof(node));
    node *t = make(&scratch, stretch);
    printf("stretch tree of depth %d\t check: %ld\n", stretch, check(t));
    arena_reset(&scratch);
    node *long_lived = make(&ambient, max_depth);
    for (int depth = 4; depth <= max_depth; depth += 2) {
        long iterations = 1L << (max_depth - depth + 4);
        long total = 0;
        for (long i = 0; i < iterations; i++) {
            node *tree = make(&scratch, depth);
            total += check(tree);
            arena_reset(&scratch);
        }
        printf("%ld\t trees of depth %d\t check: %ld\n", iterations, depth, total);
    }
    printf("long lived tree of depth %d\t check: %ld\n", max_depth, check(long_lived));
    return 0;
}
