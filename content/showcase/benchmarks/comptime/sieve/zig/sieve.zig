const std = @import("std");

const N = 10_000;
const comptime_work = true;

const Summary = struct { count: usize, last: usize };

fn sieve(comptime n: usize, primes: *[n]usize) usize {
    var words: [(n + 63) / 64]u64 = @splat(0);
    const w = &words;
    var count: usize = 0;
    var i: usize = 2;
    while (i < n) : (i += 1) {
        if ((w[i >> 6] >> @truncate(i & 63)) & 1 == 0) {
            primes[count] = i;
            count += 1;
            var j = i * i;
            while (j < n) : (j += i) w[j >> 6] |= @as(u64, 1) << @truncate(j & 63);
        }
    }
    return count;
}

fn summarize() Summary {
    @setEvalBranchQuota(1_000_000_000);
    var primes: [N]usize = undefined;
    const count = sieve(N, &primes);
    return .{ .count = count, .last = primes[count - 1] };
}

pub fn main(init: std.process.Init) !void {
    var buf: [64]u8 = undefined;
    var w = std.Io.File.stdout().writerStreaming(init.io, &buf);
    const r = if (comptime_work) comptime summarize() else summarize();
    try w.interface.print("{d} {d}\n", .{ r.count, r.last });
    try w.interface.flush();
}
