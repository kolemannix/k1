import java.util.HashMap;

public final class Main {
    static final class Rng {
        long state;

        Rng(long seed) {
            state = seed;
        }

        long next() {
            long x = state;
            x ^= x << 13;
            x ^= x >>> 7;
            x ^= x << 17;
            state = x;
            return x;
        }
    }

    static String run() {
        long n = 5_000_000;
        HashMap<Long, Long> m = new HashMap<>();
        Rng keys = new Rng(0x9E3779B97F4A7C15L);
        for (long i = 0; i < n; i++) {
            m.put(keys.next(), i);
        }
        Rng hits = new Rng(0x9E3779B97F4A7C15L);
        Rng misses = new Rng(0x2545F4914F6CDD1DL);
        long found = 0;
        long sum = 0;
        for (long i = 0; i < n; i++) {
            long hitKey = hits.next();
            long missKey = misses.next();
            Long v = m.get(i % 2 == 0 ? hitKey : missKey);
            if (v != null) {
                found++;
                sum += v;
            }
        }
        return String.format("inserted: %d found: %d sum: %d\n", m.size(), found, sum);
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
