def make(depth):
    if depth == 0:
        return (None, None)
    depth -= 1
    return (make(depth), make(depth))


def check(node):
    left, right = node
    if left is None:
        return 1
    return 1 + check(left) + check(right)


def main():
    max_depth = 20
    stretch = max_depth + 1
    print(f"stretch tree of depth {stretch}\t check: {check(make(stretch))}")
    long_lived = make(max_depth)
    for depth in range(4, max_depth + 1, 2):
        iterations = 1 << (max_depth - depth + 4)
        total = 0
        for _ in range(iterations):
            total += check(make(depth))
        print(f"{iterations}\t trees of depth {depth}\t check: {total}")
    print(f"long lived tree of depth {max_depth}\t check: {check(long_lived)}")


main()
