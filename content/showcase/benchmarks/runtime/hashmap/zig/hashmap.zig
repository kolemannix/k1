const std = @import("std");

const Rng = struct {
    state: u64,
    fn next(self: *Rng) u64 {
        var x = self.state;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.state = x;
        return x;
    }
};

pub fn main() !void {
    const n: u64 = 5_000_000;
    var m = std.AutoHashMap(u64, u64).init(std.heap.page_allocator);
    defer m.deinit();
    var keys = Rng{ .state = 0x9E3779B97F4A7C15 };
    var i: u64 = 0;
    while (i < n) : (i += 1) try m.put(keys.next(), i);
    var hits = Rng{ .state = 0x9E3779B97F4A7C15 };
    var misses = Rng{ .state = 0x2545F4914F6CDD1D };
    var found: u64 = 0;
    var sum: u64 = 0;
    i = 0;
    while (i < n) : (i += 1) {
        const hit_key = hits.next();
        const miss_key = misses.next();
        const key = if (i % 2 == 0) hit_key else miss_key;
        if (m.get(key)) |v| {
            found += 1;
            sum += v;
        }
    }
    try std.io.getStdOut().writer().print("inserted: {d} found: {d} sum: {d}\n", .{ m.count(), found, sum });
}
