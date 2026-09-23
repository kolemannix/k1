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

    static final class Table {
        long[] keys;
        long[] vals;
        int count;
        int shift;

        Table(int log2cap) {
            keys = new long[1 << log2cap];
            vals = new long[1 << log2cap];
            shift = 64 - log2cap;
            count = 0;
        }

        int slot(long key) {
            return (int) ((key * 0x9E3779B97F4A7C15L) >>> shift);
        }

        void grow() {
            long[] oldKeys = keys;
            long[] oldVals = vals;
            keys = new long[oldKeys.length * 2];
            vals = new long[oldVals.length * 2];
            shift -= 1;
            count = 0;
            for (int i = 0; i < oldKeys.length; i++) {
                if (oldKeys[i] != 0) {
                    insert(oldKeys[i], oldVals[i]);
                }
            }
        }

        void insert(long key, long val) {
            if ((count + 1) * 2 > keys.length) {
                grow();
            }
            int mask = keys.length - 1;
            int i = slot(key);
            while (keys[i] != 0 && keys[i] != key) {
                i = (i + 1) & mask;
            }
            if (keys[i] == 0) {
                keys[i] = key;
                count++;
            }
            vals[i] = val;
        }

        int find(long key) {
            int mask = keys.length - 1;
            int i = slot(key);
            while (keys[i] != 0) {
                if (keys[i] == key) {
                    return i;
                }
                i = (i + 1) & mask;
            }
            return -1;
        }
    }

    static String run() {
        long n = 5_000_000;
        Table m = new Table(4);
        Rng keys = new Rng(0x9E3779B97F4A7C15L);
        for (long i = 0; i < n; i++) {
            m.insert(keys.next(), i);
        }
        Rng hits = new Rng(0x9E3779B97F4A7C15L);
        Rng misses = new Rng(0x2545F4914F6CDD1DL);
        long found = 0;
        long sum = 0;
        for (long i = 0; i < n; i++) {
            long hitKey = hits.next();
            long missKey = misses.next();
            int at = m.find(i % 2 == 0 ? hitKey : missKey);
            if (at >= 0) {
                found++;
                sum += m.vals[at];
            }
        }
        return String.format("inserted: %d found: %d sum: %d\n", m.count, found, sum);
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
