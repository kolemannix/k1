const std = @import("std");

const N = 10_000;
const comptime_work = true;

const Summary = struct { count: usize, last: usize };

fn sieve(comptime n: usize) struct { primes: [n]usize, count: usize } {
    var composite = [_]bool{false} ** n;
    var primes: [n]usize = undefined;
    var count: usize = 0;
    var i: usize = 2;
    while (i < n) : (i += 1) {
        if (!composite[i]) {
            primes[count] = i;
            count += 1;
            var j = i * i;
            while (j < n) : (j += i) composite[j] = true;
        }
    }
    return .{ .primes = primes, .count = count };
}

fn summarize() Summary {
    @setEvalBranchQuota(1_000_000_000);
    const s = sieve(N);
    return .{ .count = s.count, .last = s.primes[s.count - 1] };
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const r = if (comptime_work) comptime summarize() else summarize();
    try stdout.print("{d} {d}\n", .{ r.count, r.last });
}
