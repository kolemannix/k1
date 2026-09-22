import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;

public final class Main {
    static final int SIZE = 256 * 1024 * 1024;
    static final int REPEAT = 8;
    static final int NEWLINE_STRIDE = 4093;
    static final int DELIM_STRIDE = 1_000_003;

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

    static void fill(byte[] buf) {
        LongBuffer words = ByteBuffer.wrap(buf).order(ByteOrder.LITTLE_ENDIAN).asLongBuffer();
        Rng rng = new Rng(0x2545F4914F6CDD1DL);
        for (int i = 0; i < words.capacity(); i++) {
            words.put(i, (rng.next() & 0x1f1f1f1f1f1f1f1fL) | 0x4040404040404040L);
        }
        for (int i = NEWLINE_STRIDE; i < buf.length; i += NEWLINE_STRIDE) {
            buf[i] = '\n';
        }
        byte[] marks = {(byte) ',', (byte) ':', (byte) '"'};
        int which = 0;
        for (int d = DELIM_STRIDE; d < buf.length; d += DELIM_STRIDE) {
            buf[d] = marks[which];
            which = (which + 1) % 3;
        }
    }

    static int indexOf(byte[] data, int from, byte target) {
        for (int i = from; i < data.length; i++) {
            if (data[i] == target) {
                return i;
            }
        }
        return -1;
    }

    static int indexOfAny(byte[] data, int from) {
        for (int i = from; i < data.length; i++) {
            byte b = data[i];
            if (b == ',' || b == ':' || b == '"') {
                return i;
            }
        }
        return -1;
    }

    static long countNewlines(byte[] data) {
        long count = 0;
        int from = 0;
        while (true) {
            int p = indexOf(data, from, (byte) '\n');
            if (p < 0) {
                return count;
            }
            count++;
            from = p + 1;
        }
    }

    static long[] scanDelims(byte[] data) {
        long found = 0;
        long indexSum = 0;
        long whichSum = 0;
        int from = 0;
        while (true) {
            int p = indexOfAny(data, from);
            if (p < 0) {
                return new long[] {found, indexSum, whichSum};
            }
            found++;
            indexSum += p;
            whichSum += data[p] == ',' ? 0 : data[p] == ':' ? 1 : 2;
            from = p + 1;
        }
    }

    static String run() {
        byte[] buf = new byte[SIZE];
        fill(buf);
        long absent = 0;
        long newlines = 0;
        long delims = 0;
        long indexSum = 0;
        long whichSum = 0;
        for (int r = 0; r < REPEAT; r++) {
            if (indexOf(buf, 0, (byte) 0) >= 0) {
                absent++;
            }
            newlines += countNewlines(buf);
            long[] s = scanDelims(buf);
            delims += s[0];
            indexSum += s[1];
            whichSum += s[2];
        }
        return String.format(
                "absent: %d newlines: %d delims: %d index-sum: %d which-sum: %d\n",
                absent, newlines, delims, indexSum, whichSum);
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
