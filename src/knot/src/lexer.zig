const std = @import("std");
const utils = @import("utils.zig");

pub const TokenType = enum {
    IntLit,
    CharLit,
    //FloatLit,
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
    Cast,
    EmptyToken,
};

pub const Value = union(enum){
    Int: i32,
    Float: f64,
    Char: u8,
    String: []const u8,
    Type: TypeV,
};

pub const Token = struct {
    toktype: TokenType = TokenType.EmptyToken,
    line: u32 = 0,
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
    index: u32 = 0,


    pub fn tokenize(self: *src, alloc: std.mem.Allocator) !std.ArrayList(Token) {
        var line: u32 = 1;
        var tokens = std.ArrayList(Token).empty;
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
                '=' => {try tokens.append(alloc, Token{ .toktype = TokenType.Assign, .line = line, .value = null}); self.consume();},
                'A'...'Z', 'a'...'z' => try tokens.append(alloc, try self.checkKeyword(alloc, line)),
                '"' => try tokens.append(alloc, try self.checkString(alloc, line)),
                '\'' => try tokens.append(alloc, try self.checkChar(alloc, line)),
                '0'...'9' => try tokens.append(alloc, try self.checkNum(alloc, line)),
                '\n' => {line += 1; self.consume();},
                ' ' => self.consume(),
                else => std.debug.panic("Invalid Token detected at line .{d}: .{c}\n", .{line, self.peek(0)}),
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

    fn cons(self: *src) u8
    {
        self.index += 1;
        return self.src[self.index - 1];
    }

    fn checkKeyword(self: *src, alloc: std.mem.Allocator, line: u32) !Token
    {
        var buf = std.ArrayList(u8).empty;
        while(std.ascii.isAlphanumeric(self.peek(0))){
            try buf.append(alloc, self.cons());
        }
        if(std.mem.eql(u8, buf.items, "if")) return Token{ .toktype = TokenType.If, .line = line, .value = null};
        if(std.mem.eql(u8, buf.items, "else")) return Token{ .toktype = TokenType.Else, .line = line, .value = null};
        if(std.mem.eql(u8, buf.items, "while")) return Token{ .toktype = TokenType.While, .line = line, .value = null};
        if(std.mem.eql(u8, buf.items, "print")) return Token{ .toktype = TokenType.Print, .line = line, .value = null};
        if(std.mem.eql(u8, buf.items, "cast")) return Token{ .toktype = TokenType.Cast, .line = line, .value = null};
        if(std.mem.eql(u8, buf.items, "int")) return Token{ .toktype = TokenType.TypeLit, .line = line, .value = Value{.Type = TypeV.Int}};
        if(std.mem.eql(u8, buf.items, "string")) return Token{ .toktype = TokenType.TypeLit, .line = line, .value = Value{.Type = TypeV.String}};
        if(std.mem.eql(u8, buf.items, "char")) return Token{ .toktype = TokenType.TypeLit, .line = line, .value = Value{.Type = TypeV.Char}};
        if(std.mem.eql(u8, buf.items, "float")) return Token{ .toktype = TokenType.TypeLit, .line = line, .value = Value{.Type = TypeV.Float}};
        return Token{.toktype = TokenType.Ident, .line = line, .value = Value{.String = buf.items}};
    }

    fn checkString(self: *src, alloc: std.mem.Allocator, line: u32) !Token
    {
        self.index += 1;
        var buf = std.ArrayList(u8).empty;
        while(self.peek(0) != '"'){
            try buf.append(alloc, self.cons());
        }
        self.consume();
        return Token{.toktype = TokenType.StringLit, .line = line, .value = Value{.String = buf.items}};
    }

    fn checkChar(self: *src, alloc: std.mem.Allocator, line: u32) !Token
    {
        self.index += 1;
        const c = self.peek(0);
        if (c == '\\'){
            return Token{.toktype = TokenType.CharLit, .line = line, .value = Value{.String = try utils.concat(alloc, "\\", &[_]u8{c})}};

        }
        return Token{.toktype = TokenType.CharLit, .line = line, .value = Value{.Char = c}};
    }

    fn checkNum(self: *src, alloc: std.mem.Allocator, line: u32) !Token
    {
        var buf = std.ArrayList(u8).empty;
        while(std.ascii.isDigit(self.peek(0))){
            try buf.append(alloc, self.cons());
        }
        const int = std.fmt.parseInt(i32, buf.items, 10) catch |err| {
            std.debug.print("Failed to parse: {}\n", .{err});
            return err;
        };
        return Token{.toktype = TokenType.IntLit, .line = line, .value = Value{.Int = int}};
    }
};

