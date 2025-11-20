const std = @import("std");

pub const TokenType = enum {
    IntLit,
    CharLit,
    FloatLit,
    StringLit,
    TypeLit,
    Plus,
    Star,
    Minus,
    FSlash,
    While,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Assign,
    If,
    Else,
    Ident,
};

pub const Value = union(enum){
    Int: i32,
    Float: f64,
    Char: u8,
    String: []const u8,
    Type: TypeV,
};

pub const Token = struct {
    toktype: TokenType,
    line: u32,
    value: Value,
};

pub const TypeV = enum {
    Int,
    Char,
    String,
    Float,
};

pub const src = struct {
    src: []const u8,
    index: i32,


    pub fn tokenize(self: src) ![]const Token {
        



        const tokens: [1]Token = .{Token{
            .toktype = TokenType.IntLit,
            .line = 1,
            .value = Value{ .String = src }
        }};
        return tokens[0..];
    }

    fn peek(self: src, offset: i32) u8
    {
        if (self.index + offset >= self.src.len)
            return 0;
        return self.src[self.index + offset];
    }

    fn consume(self: src) u8
    {
        self.index += 1;
        return self.src[self.index-1];
    }
};

pub fn tokenize(src: []const u8) ![]const Token {
    



    const tokens: [1]Token = .{Token{
        .toktype = TokenType.IntLit,
        .line = 1,
        .value = Value{ .String = src }
    }};
    return tokens[0..];
}

fn peek(offset: int = 0)
