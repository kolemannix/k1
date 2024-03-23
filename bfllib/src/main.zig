const std = @import("std");
const allocator = std.heap.c_allocator;

const BflString = packed struct {
    len: u64,
    data: [*]u8,

    fn as_slice(self: *const BflString) []u8 {
        return self.data[0..self.len];
    }
};

export fn _bfl_charToString(c: u8) BflString {
    const data = allocator.alloc(u8, 1) catch unreachable;
    data[0] = c;
    const new_str: BflString = .{ .len = 1, .data = data.ptr };
    return new_str;
}

export fn _bfl_charToInt(c: u8) i64 {
    return @intCast(c);
}

export fn _bfl_intToChar(i: i64) u8 {
    return @intCast(i);
}

export fn _bfl_readFileToString(s: BflString) BflString {
    //std.fs.cwd()
    const file = std.fs.openFileAbsolute(s.as_slice(), .{ .mode = .read_only }) catch unreachable;
    defer file.close();
    const buffer = file.readToEndAlloc(allocator, 1024 * 1024 * 10) catch unreachable;
    const length = buffer.len;
    // std.debug.print("reading {s} to {} buf ", .{ s.as_slice(), length });
    const new_str: BflString = .{ .len = length, .data = buffer.ptr };
    // std.debug.print("our string {any} \n'{s}'", .{ new_str, new_str.as_slice() });
    return new_str;
}
