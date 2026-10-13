const std = @import("std");

fn writeJson(w: *std.Io.Writer, value: anytype) !void {
    switch (@typeInfo(@TypeOf(value))) {
        .@"struct" => |s| {
            try w.writeByte('{');
            inline for (s.field_names, 0..) |name, i| {
                if (i > 0) try w.writeByte(',');
                try w.print("\"{s}\":", .{name});
                try writeJson(w, @field(value, name));
            }
            try w.writeByte('}');
        },
        .bool => try w.writeAll(if (value) "true" else "false"),
        .int => try w.print("{d}", .{value}),
        .pointer => try w.print("\"{s}\"", .{value}),
        else => @compileError("unsupported type " ++ @typeName(@TypeOf(value))),
    }
}

pub fn main(init: std.process.Init) !void {
    var out_buf: [256]u8 = undefined;
    var out = std.Io.File.stdout().writer(init.io, &out_buf);
    const stdout = &out.interface;
    var count_buf: [256]u8 = undefined;
    var counting: std.Io.Writer.Discarding = .init(&count_buf);
    try serializeAll(&counting.writer);
    try stdout.print("{d}\n", .{counting.fullCount()});
    try stdout.flush();
}
