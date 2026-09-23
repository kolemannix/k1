const std = @import("std");

const size: usize = 256 * 1024 * 1024;
const repeat: usize = 8;
const newline_stride: usize = 4093;
const delim_stride: usize = 1_000_003;

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

fn fill(words: []u64, buf: []u8) void {
    var rng = Rng{ .state = 0x2545F4914F6CDD1D };
    for (words) |*w| w.* = (rng.next() & 0x1f1f1f1f1f1f1f1f) | 0x4040404040404040;
    var i: usize = newline_stride;
    while (i < buf.len) : (i += newline_stride) buf[i] = '\n';
    const marks = [_]u8{ ',', ':', '"' };
    var which: usize = 0;
    var d: usize = delim_stride;
    while (d < buf.len) : (d += delim_stride) {
        buf[d] = marks[which];
        which = (which + 1) % 3;
    }
}

fn countNewlines(data: []const u8) i64 {
    var count: i64 = 0;
    var from: usize = 0;
    while (std.mem.indexOfScalarPos(u8, data, from, '\n')) |p| {
        count += 1;
        from = p + 1;
    }
    return count;
}

const Scan = struct { found: i64, index_sum: i64, which_sum: i64 };

fn scanDelims(data: []const u8) Scan {
    var r = Scan{ .found = 0, .index_sum = 0, .which_sum = 0 };
    var from: usize = 0;
    while (std.mem.indexOfAnyPos(u8, data, from, ",:\"")) |p| {
        r.found += 1;
        r.index_sum += @intCast(p);
        r.which_sum += switch (data[p]) {
            ',' => 0,
            ':' => 1,
            else => 2,
        };
        from = p + 1;
    }
    return r;
}

pub fn main() !void {
    const words = try std.heap.page_allocator.alloc(u64, size / 8);
    defer std.heap.page_allocator.free(words);
    const buf = std.mem.sliceAsBytes(words);
    fill(words, buf);
    var absent: i64 = 0;
    var newlines: i64 = 0;
    var delims: i64 = 0;
    var index_sum: i64 = 0;
    var which_sum: i64 = 0;
    var r: usize = 0;
    while (r < repeat) : (r += 1) {
        if (std.mem.indexOfScalar(u8, buf, 0) != null) absent += 1;
        newlines += countNewlines(buf);
        const s = scanDelims(buf);
        delims += s.found;
        index_sum += s.index_sum;
        which_sum += s.which_sum;
    }
    try std.io.getStdOut().writer().print("absent: {d} newlines: {d} delims: {d} index-sum: {d} which-sum: {d}\n", .{ absent, newlines, delims, index_sum, which_sum });
}
