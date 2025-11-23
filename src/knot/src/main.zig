const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void
{
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    const file = try std.fs.cwd().openFile(argv[1], .{});
    defer file.close();

    var src = lexer.src{.src = try file.readToEndAlloc(allocator, std.math.maxInt(usize))};

    const toks = try src.tokenize(allocator);

    var tokens = parser.Tokens{.tokens = toks.items};

    const ast = try tokens.parse(allocator);
    _ = ast;

    for (toks.items) |token| {
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
        std.debug.print("{s} = {any}\n", .{name, val});
    }
}
