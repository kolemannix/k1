const std = @import("std");

const N = 10_000;
const comptime_work = true;

const Summary = struct { len: usize, sum: u32 };

fn fnv1a(bytes: anytype, len: usize) u32 {
    var h: u32 = 2166136261;
    var i: usize = 0;
    while (i < len) : (i += 1) h = (h ^ bytes[i]) *% 16777619;
    return h;
}

fn build() Summary {
    @setEvalBranchQuota(1_000_000_000);
    var buf: [N * 8]u8 = @splat(0);
    const p = &buf;
    var len: usize = 0;
    for (0..N) |i| {
        if (i > 0) {
            p[len] = ',';
            len += 1;
        }
        var v = i;
        var n: usize = 1;
        while (v >= 10) : (v /= 10) n += 1;
        var k = len + n;
        v = i;
        while (k > len) {
            k -= 1;
            p[k] = '0' + @as(u8, @intCast(v % 10));
            v /= 10;
        }
        len += n;
    }
    return .{ .len = len, .sum = fnv1a(buf, len) };
}

pub fn main(init: std.process.Init) !void {
    var out_buf: [256]u8 = undefined;
    var out = std.Io.File.stdout().writer(init.io, &out_buf);
    const stdout = &out.interface;
    const r = if (comptime_work) comptime build() else build();
    try stdout.print("{d} {d}\n", .{ r.len, r.sum });
    try stdout.flush();
}
