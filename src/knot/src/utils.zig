const std = @import("std");

pub fn concat(alloc: std.mem.Allocator, s1: []const u8, s2: []const u8) ![]u8 {
    var buf = try alloc.alloc(u8, s1.len + s2.len);
    @memcpy(buf[0..s1.len], s1);
    @memcpy(buf[s1.len..], s2);
    return buf;
}
