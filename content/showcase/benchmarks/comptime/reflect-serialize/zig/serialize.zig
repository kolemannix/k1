const std = @import("std");

fn fnv1a(h: u32, bytes: []const u8) u32 {
    var x = h;
    for (bytes) |b| x = (x ^ b) *% 16777619;
    return x;
}

fn writeJson(w: *std.Io.Writer, value: anytype) !void {
    switch (@typeInfo(@TypeOf(value))) {
        .@"struct" => |s| {
            try w.writeByte('{');
            const fingerprint = comptime blk: {
                var h: u32 = 2166136261;
                for (s.field_names, s.field_types) |name, T| {
                    h = fnv1a(h, name);
                    h = fnv1a(h, switch (@typeInfo(T)) {
                        .bool => ":bool,",
                        .int => ":int,",
                        else => ":str,",
                    });
                }
                break :blk h;
            };
            try w.print("\"schema\":{d},", .{fingerprint});
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
