using System;
using System.Collections.Generic;
using System.Diagnostics;

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
    static string Run()
    {
        const ulong n = 5_000_000;
        Dictionary<ulong, ulong> m = new Dictionary<ulong, ulong>();
        Rng keys = new Rng(0x9E3779B97F4A7C15);
        for (ulong i = 0; i < n; i++)
        {
            m[keys.Next()] = i;
        }
        Rng hits = new Rng(0x9E3779B97F4A7C15);
        Rng misses = new Rng(0x2545F4914F6CDD1D);
        ulong found = 0;
        ulong sum = 0;
        for (ulong i = 0; i < n; i++)
        {
            ulong hitKey = hits.Next();
            ulong missKey = misses.Next();
            ulong key = i % 2 == 0 ? hitKey : missKey;
            if (m.TryGetValue(key, out ulong v))
            {
                found++;
                sum += v;
            }
        }
        return $"inserted: {m.Count} found: {found} sum: {sum}\n";
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
