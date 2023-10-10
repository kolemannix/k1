const NexString = packed struct {
    len: u64,
    data: [*]u8,
};
fn NexArray(T: type) type {
    return struct {
        len: u64,
        data: [*]T,
    };
}

export const HELLO_WORLD = "Hello, World";

export fn zigadd(a: i64, b: i64) i64 {
    const s = NexString{ .len = 4, .data = [4]u8{ 'a', 's', 'd', 'f' } };
    return a + b;
}

// const std = @import("std");
// const testing = std.testing;
// test "basic add functionality" {
//     try testing.expect(zigadd(3, 7) == 10);
// }
