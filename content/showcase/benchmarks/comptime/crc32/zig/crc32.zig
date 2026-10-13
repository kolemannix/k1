const std = @import("std");

const B = 65536;
const comptime_work = true;

fn crcTable() [256]u32 {
    var table: [256]u32 = undefined;
    for (&table, 0..) |*entry, i| {
        var c: u32 = @intCast(i);
        for (0..8) |_| c = if ((c & 1) == 1) 0xEDB88320 ^ (c >> 1) else c >> 1;
        entry.* = c;
    }
    return table;
}

fn randomBytes(comptime n: usize) [n]u8 {
    var bytes: [n]u8 = @splat(0);
    var x: u32 = 2463534242;
    for (&bytes) |*b| {
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        b.* = @truncate(x);
    }
    return bytes;
}

fn crc32(table: [256]u32, bytes: anytype) u32 {
    var c: u32 = 0xFFFFFFFF;
    var i: usize = 0;
    while (i < bytes.len) : (i += 1) c = table[(c ^ bytes[i]) & 0xFF] ^ (c >> 8);
    return c ^ 0xFFFFFFFF;
}

fn checksum() u32 {
    @setEvalBranchQuota(1_000_000_000);
    const bytes = randomBytes(B);
    return crc32(crcTable(), bytes);
}

pub fn main(init: std.process.Init) !void {
    var out_buf: [256]u8 = undefined;
    var out = std.Io.File.stdout().writer(init.io, &out_buf);
    const stdout = &out.interface;
    const sum = if (comptime_work) comptime checksum() else checksum();
    try stdout.print("{d}\n", .{sum});
    try stdout.flush();
}
