public final class Main {
    static final class Node {
        Node left;
        Node right;
    }

    static Node make(int depth) {
        Node n = new Node();
        if (depth != 0) {
            n.left = make(depth - 1);
            n.right = make(depth - 1);
        }
        return n;
    }

    static long check(Node n) {
        return n.left == null ? 1 : 1 + check(n.left) + check(n.right);
    }

    static String run() {
        StringBuilder out = new StringBuilder();
        int maxDepth = 20;
        int stretch = maxDepth + 1;
        out.append(String.format("stretch tree of depth %d\t check: %d\n", stretch, check(make(stretch))));
        Node longLived = make(maxDepth);
        for (int depth = 4; depth <= maxDepth; depth += 2) {
            long iterations = 1L << (maxDepth - depth + 4);
            long total = 0;
            for (long i = 0; i < iterations; i++) {
                total += check(make(depth));
            }
            out.append(String.format("%d\t trees of depth %d\t check: %d\n", iterations, depth, total));
        }
        out.append(String.format("long lived tree of depth %d\t check: %d\n", maxDepth, check(longLived)));
        return out.toString();
    }

    public static void main(String[] args) {
        boolean steady = args.length > 0 && args[0].equals("steady");
        int reps = steady ? 3 : 1;
        String output = null;
        long ms = 0;
        for (int i = 0; i < reps; i++) {
            long start = System.nanoTime();
            output = run();
            ms = (System.nanoTime() - start) / 1_000_000;
        }
        System.out.print(output);
        System.out.flush();
        if (steady) {
            System.err.println("steady-state ms: " + ms);
        }
    }
}
