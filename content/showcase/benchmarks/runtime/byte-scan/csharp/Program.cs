using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

sealed class Rng
{
    ulong state;

    public Rng(ulong seed)
    {
        state = seed;
    }

    public ulong Next()
    {
        ulong x = state;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        state = x;
        return x;
    }
}

static class Program
{
    const int Size = 256 * 1024 * 1024;
    const int Repeat = 8;
    const int NewlineStride = 4093;
    const int DelimStride = 1_000_003;

    static void Fill(byte[] buf)
    {
        Span<ulong> words = MemoryMarshal.Cast<byte, ulong>(buf.AsSpan());
        Rng rng = new Rng(0x2545F4914F6CDD1D);
        for (int i = 0; i < words.Length; i++)
        {
            words[i] = (rng.Next() & 0x1f1f1f1f1f1f1f1f) | 0x4040404040404040;
        }
        for (int i = NewlineStride; i < buf.Length; i += NewlineStride)
        {
            buf[i] = (byte)'\n';
        }
        byte[] marks = { (byte)',', (byte)':', (byte)'"' };
        int which = 0;
        for (int d = DelimStride; d < buf.Length; d += DelimStride)
        {
            buf[d] = marks[which];
            which = (which + 1) % 3;
        }
    }

    static long CountNewlines(byte[] data)
    {
        long count = 0;
        int from = 0;
        while (true)
        {
            int p = data.AsSpan(from).IndexOf((byte)'\n');
            if (p < 0)
            {
                return count;
            }
            count++;
            from += p + 1;
        }
    }

    static (long, long, long) ScanDelims(byte[] data)
    {
        long found = 0;
        long indexSum = 0;
        long whichSum = 0;
        int from = 0;
        while (true)
        {
            int p = data.AsSpan(from).IndexOfAny((byte)',', (byte)':', (byte)'"');
            if (p < 0)
            {
                return (found, indexSum, whichSum);
            }
            int index = from + p;
            found++;
            indexSum += index;
            whichSum += data[index] == (byte)',' ? 0 : data[index] == (byte)':' ? 1 : 2;
            from = index + 1;
        }
    }

    static string Run()
    {
        byte[] buf = new byte[Size];
        Fill(buf);
        long absent = 0;
        long newlines = 0;
        long delims = 0;
        long indexSum = 0;
        long whichSum = 0;
        for (int r = 0; r < Repeat; r++)
        {
            if (buf.AsSpan().IndexOf((byte)0) >= 0)
            {
                absent++;
            }
            newlines += CountNewlines(buf);
            (long f, long i, long w) = ScanDelims(buf);
            delims += f;
            indexSum += i;
            whichSum += w;
        }
        return $"absent: {absent} newlines: {newlines} delims: {delims} index-sum: {indexSum} which-sum: {whichSum}\n";
    }

    static void Main(string[] args)
    {
        bool steady = args.Length > 0 && args[0] == "steady";
        int reps = steady ? 3 : 1;
        string output = null;
        long ms = 0;
        for (int i = 0; i < reps; i++)
        {
            Stopwatch sw = Stopwatch.StartNew();
            output = Run();
            ms = sw.ElapsedMilliseconds;
        }
        Console.Out.Write(output);
        Console.Out.Flush();
        if (steady)
        {
            Console.Error.WriteLine("steady-state ms: " + ms);
        }
    }
}
