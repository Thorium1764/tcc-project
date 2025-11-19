const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void
{
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);
    std.debug.print("contents of {s}:\n", .{argv[1]});

    const file = try std.fs.cwd().openFile(argv[1], .{});
    defer file.close();

    const src = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

    const tokens = try lexer.tokenize(src);

    for (tokens) |token| {
        try printStruct(lexer.Token, token);
    }
}

pub fn printStruct(comptime T: type, value: T) !void { //what the fuck is this
    const info = @typeInfo(T);
    if (info != .@"struct") @compileError("T must be a struct");
    const fields = info.@"struct".fields;
    inline for (fields) |field| {
        const name = field.name;
        const val = @field(value, name);
        std.debug.print("{s} = {}\n", .{name, val});
    }
}
