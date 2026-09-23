using System;
using System.Diagnostics;
using System.Text;

sealed class Node
{
    public Node Left;
    public Node Right;
}

static class Program
{
    static Node Make(int depth)
    {
        Node n = new Node();
        if (depth != 0)
        {
            n.Left = Make(depth - 1);
            n.Right = Make(depth - 1);
        }
        return n;
    }

    static long Check(Node n)
    {
        return n.Left == null ? 1 : 1 + Check(n.Left) + Check(n.Right);
    }

    static string Run()
    {
        StringBuilder outp = new StringBuilder();
        int maxDepth = 20;
        int stretch = maxDepth + 1;
        outp.Append($"stretch tree of depth {stretch}\t check: {Check(Make(stretch))}\n");
        Node longLived = Make(maxDepth);
        for (int depth = 4; depth <= maxDepth; depth += 2)
        {
            long iterations = 1L << (maxDepth - depth + 4);
            long total = 0;
            for (long i = 0; i < iterations; i++)
            {
                total += Check(Make(depth));
            }
            outp.Append($"{iterations}\t trees of depth {depth}\t check: {total}\n");
        }
        outp.Append($"long lived tree of depth {maxDepth}\t check: {Check(longLived)}\n");
        return outp.ToString();
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
