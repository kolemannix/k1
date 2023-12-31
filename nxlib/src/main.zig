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
        const Self = @This();
        len: u64,
        cap: u64,
        data: [*]T,

        fn push(self: *Self, value: T) void {
            if (self.len + 1 > self.cap) {
              self.grow();
            }
            self.data[self.len] = value;
            self.len += 1;
        }

        fn grow(self: *Self) void {
          const new_cap = self.cap * 2;
          const data = allocator.alloc(T, new_cap);
          @memcpy(data, self.data);
          self.cap = new_cap;
          self.data = data;
        }
    };
}

export const HELLO_WORLD = "Hello, World";
// const s = NxString{ .len = 4, .data = [4]u8{ 'a', 's', 'd', 'f' } };

export fn _nx_charToString(c: u8) NxString {
    const data = allocator.alloc(u8, 1) catch unreachable;
    data[0] = c;
    const new_str: NxString = .{ .len = 1, .data = data.ptr };
    return new_str;
}

export fn _nx_charToInt(c: u8) i64 {
    return @intCast(c);
}

export fn _nx_intToChar(i: i64) u8 {
    return @intCast(i);
}

export fn _nx_readFileToString(s: NxString) NxString {
    //std.fs.cwd()
    const file = std.fs.openFileAbsolute(s.as_slice(), .{ .mode = .read_only }) catch unreachable;
    defer file.close();
    const buffer = file.readToEndAlloc(allocator, 1024 * 1024 * 10) catch unreachable;
    const length = buffer.len;
    // std.debug.print("reading {s} to {} buf ", .{ s.as_slice(), length });
    const new_str: NxString = .{ .len = length, .data = buffer.ptr };
    // std.debug.print("our string {any} \n'{s}'", .{ new_str, new_str.as_slice() });
    return new_str;
}
