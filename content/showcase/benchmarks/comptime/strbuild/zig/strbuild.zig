const std = @import("std");

const N = 10_000;
const comptime_work = true;

const Summary = struct { len: usize, sum: u32 };

fn fnv1a(bytes: []const u8) u32 {
    var h: u32 = 2166136261;
    for (bytes) |b| h = (h ^ b) *% 16777619;
    return h;
}

fn build() Summary {
    @setEvalBranchQuota(1_000_000_000);
    var buf: [N * 8]u8 = undefined;
    var len: usize = 0;
    for (0..N) |i| {
        if (i > 0) {
            buf[len] = ',';
            len += 1;
        }
        len += std.fmt.formatIntBuf(buf[len..], i, 10, .lower, .{});
    }
    return .{ .len = len, .sum = fnv1a(buf[0..len]) };
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const r = if (comptime_work) comptime build() else build();
    try stdout.print("{d} {d}\n", .{ r.len, r.sum });
}
