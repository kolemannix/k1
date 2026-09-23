const std = @import("std");

const Node = struct { left: ?*Node, right: ?*Node };

fn make(alloc: std.mem.Allocator, depth: u32) !*Node {
    const n = try alloc.create(Node);
    if (depth == 0) {
        n.* = .{ .left = null, .right = null };
    } else {
        n.* = .{ .left = try make(alloc, depth - 1), .right = try make(alloc, depth - 1) };
    }
    return n;
}

fn check(n: *const Node) i64 {
    if (n.left) |l| return 1 + check(l) + check(n.right.?);
    return 1;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const max_depth: u32 = 20;
    const stretch = max_depth + 1;
    var short_lived = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer short_lived.deinit();
    var long_lived_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer long_lived_arena.deinit();

    try stdout.print("stretch tree of depth {d}\t check: {d}\n", .{ stretch, check(try make(short_lived.allocator(), stretch)) });
    _ = short_lived.reset(.retain_capacity);
    const long_lived = try make(long_lived_arena.allocator(), max_depth);
    var depth: u32 = 4;
    while (depth <= max_depth) : (depth += 2) {
        const iterations = @as(i64, 1) << @intCast(max_depth - depth + 4);
        var total: i64 = 0;
        var i: i64 = 0;
        while (i < iterations) : (i += 1) {
            total += check(try make(short_lived.allocator(), depth));
            _ = short_lived.reset(.retain_capacity);
        }
        try stdout.print("{d}\t trees of depth {d}\t check: {d}\n", .{ iterations, depth, total });
    }
    try stdout.print("long lived tree of depth {d}\t check: {d}\n", .{ max_depth, check(long_lived) });
}
