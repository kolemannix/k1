const std = @import("std");
const allocator = std.heap.c_allocator;

const NxString = packed struct {
    len: u64,
    data: [*]u8,

    fn as_slice(self: *const NxString) []u8 {
        return self.data[0..self.len];
    }
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

export fn _nx_readFileToString(s: *NxString) *const NxString {
    //std.fs.cwd()
    const file = std.fs.openFileAbsolute(s.as_slice(), .{ .mode = .read_only }) catch unreachable;
    defer file.close();
    const buffer = file.readToEndAlloc(allocator, 1024) catch unreachable;
    const length = buffer.len;
    // std.debug.print("reading {s} to {} buf ", .{ s.as_slice(), length });
    const new_str: *NxString = allocator.create(NxString) catch unreachable;
    new_str.* = .{ .len = length, .data = buffer.ptr };
    // std.debug.print("our string {any} \n'{s}'", .{ new_str, new_str.as_slice() });
    return new_str;
}
