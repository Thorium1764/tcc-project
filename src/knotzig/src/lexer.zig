const std = @import("std");

pub const TokenType = enum {
    IntLit,
    CharLit,
    StringLit,
    FloatLit,
    Plus,
    Minus,
    Star,
    FSlash,
    BSlash,
    Modulo,
    Ampersand,
};

pub const Value = union(enum){
    Int: i32,
    Float: f64,
    Char: u8,
    String: []const u8,
};

pub const Token = struct {
    toktype: TokenType,
    line: u32,
    value: Value,
};

pub fn tokenize(src: []const u8) ![]const Token {
    const tokens: [1]Token = .{Token{
        .toktype = TokenType.IntLit,
        .line = 1,
        .value = Value{ .String = src }
    }};
    return tokens[0..];
}
