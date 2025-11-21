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
    Print,
    ToString
};

pub const Value = union(enum){
    Int: i32,
    Float: f64,
    Char: u8,
    String: []const u8,
    Type: TypeV,
    None: void
};

pub const Token = struct {
    toktype: TokenType,
    line: u32,
    value: ?Value,
};

pub const TypeV = enum {
    Int,
    Char,
    String,
    Float,
};

pub const src = struct {
    src: []const u8,
    index: u32,


    pub fn tokenize(self: *src, alloc: std.mem.Allocator) !std.ArrayList(Token) {
        var line: u32 = 1;
        var tokens = std.ArrayList(Token).empty;
        defer tokens.deinit(alloc);
        while (self.peek(0) != 0) {
            switch (self.peek(0)) {
                '+' => {try tokens.append(alloc, Token{ .toktype = TokenType.Plus, .line = line, .value = null}); self.consume();},
                '-' => {try tokens.append(alloc, Token{ .toktype = TokenType.Minus, .line = line, .value = null}); self.consume();},
                '*' => {try tokens.append(alloc, Token{ .toktype = TokenType.Star, .line = line, .value = null}); self.consume();},
                '/' => {try tokens.append(alloc, Token{ .toktype = TokenType.FSlash, .line = line, .value = null}); self.consume();},
                '(' => {try tokens.append(alloc, Token{ .toktype = TokenType.OpenParen, .line = line, .value = null}); self.consume();},
                ')' => {try tokens.append(alloc, Token{ .toktype = TokenType.CloseParen, .line = line, .value = null}); self.consume();},
                '{' => {try tokens.append(alloc, Token{ .toktype = TokenType.OpenCurly, .line = line, .value = null}); self.consume();},
                '}' => {try tokens.append(alloc, Token{ .toktype = TokenType.CloseCurly, .line = line, .value = null}); self.consume();},
                '\n' => {line += 1; self.consume();},
                else => {std.debug.print("Invalid Token detected at line .{}: .{c}", .{line, self.peek(0)});}
            }
        }
        return tokens;
    }

    fn peek(self: *src, offset: u32) u8
    {
        if (self.index + offset >= self.src.len)
            return 0;
        return self.src[self.index + offset];
    }

    fn consume(self: *src) void
    {
        self.index += 1;
    }
};

