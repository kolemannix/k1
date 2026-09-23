#include <stdio.h>
#include <stdlib.h>

typedef struct node { struct node *left, *right; } node;

static node *make(int depth) {
    node *n = malloc(sizeof *n);
    if (depth == 0) {
        n->left = n->right = NULL;
    } else {
        n->left = make(depth - 1);
        n->right = make(depth - 1);
    }
    return n;
}

static long check(const node *n) {
    return n->left == NULL ? 1 : 1 + check(n->left) + check(n->right);
}

static void destroy(node *n) {
    if (n->left != NULL) {
        destroy(n->left);
        destroy(n->right);
    }
    free(n);
}

int main(void) {
    int max_depth = 20;
    int stretch = max_depth + 1;
    node *t = make(stretch);
    printf("stretch tree of depth %d\t check: %ld\n", stretch, check(t));
    destroy(t);
    node *long_lived = make(max_depth);
    for (int depth = 4; depth <= max_depth; depth += 2) {
        long iterations = 1L << (max_depth - depth + 4);
        long total = 0;
        for (long i = 0; i < iterations; i++) {
            node *tree = make(depth);
            total += check(tree);
            destroy(tree);
        }
        printf("%ld\t trees of depth %d\t check: %ld\n", iterations, depth, total);
    }
    printf("long lived tree of depth %d\t check: %ld\n", max_depth, check(long_lived));
    destroy(long_lived);
    return 0;
}
