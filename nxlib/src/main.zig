const std = @import("std");
const allocator = std.heap.c_allocator;

const NxString = packed struct {
    len: u64,
    data: [*]u8,
};
fn NxArray(T: type) type {
    return struct {
        len: u64,
        data: [*]T,
    };
}

export const HELLO_WORLD = "Hello, World";
// const s = NxString{ .len = 4, .data = [4]u8{ 'a', 's', 'd', 'f' } };

export fn _nx_charToString(c: u8) *NxString {
    const new_str: *NxString = allocator.create(NxString) catch unreachable;
    const data = allocator.alloc(u8, 1) catch unreachable;
    data[0] = c;
    new_str.* = .{ .len = 1, .data = data.ptr };
    return new_str;
}

// const std = @import("std");
// const testing = std.testing;
// test "basic add functionality" {
//     try testing.expect(zigadd(3, 7) == 10);
// }
