const std = @import("std");

fn writeJson(w: anytype, value: anytype) !void {
    switch (@typeInfo(@TypeOf(value))) {
        .@"struct" => |s| {
            try w.writeByte('{');
            inline for (s.fields, 0..) |f, i| {
                if (i > 0) try w.writeByte(',');
                try w.print("\"{s}\":", .{f.name});
                try writeJson(w, @field(value, f.name));
            }
            try w.writeByte('}');
        },
        .bool => try w.writeAll(if (value) "true" else "false"),
        .int => try w.print("{d}", .{value}),
        .pointer => try w.print("\"{s}\"", .{value}),
        else => @compileError("unsupported type " ++ @typeName(@TypeOf(value))),
    }
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var counting = std.io.countingWriter(std.io.null_writer);
    try serializeAll(counting.writer());
    try stdout.print("{d}\n", .{counting.bytes_written});
}
