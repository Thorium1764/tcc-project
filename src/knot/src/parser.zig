const std = @import("std");
const lexer = @import("lexer.zig");

pub const BinOp = enum {
    Add,
    Sub,
    Mult,
    Div,
};

pub const NodeTerm = union(enum) {
    TermLit: lexer.Token,
    TermFnVal: struct {func: lexer.Token, param: [*]NodeExpr},
    TermParen: *NodeExpr,
    TermIdent: lexer.Token,
};

pub const NodeExpr = union(enum) {
    ExprBin: struct {binOp: BinOp, expr1: *NodeExpr, expr2: *NodeExpr},
    ExprTerm: *NodeTerm,
};

pub const NodeStmt = union(enum) {
    StmtAssign: struct { ident: lexer.Token, expr: *NodeExpr},
    StmtFnCall: struct { func: lexer.Token, param: [*]NodeExpr},
    StmtIf: struct {cond: *NodeExpr, then: *NodeStmt, els: ?*NodeStmt},
    StmtNoOp,
};

pub const AST = struct {
    stmts: std.ArrayList(NodeStmt),

    pub fn init() AST{
        return .{.stmts = std.ArrayList(NodeStmt).empty};
    }
};

pub const Tokens = struct {
    tokens: []const lexer.Token,
    index: u32 = 0,

    pub fn parse(self: *Tokens, alloc: std.mem.Allocator) !AST {
        var ast = AST.init();
        while(self.index < self.tokens.len){
            switch(self.peek(0).toktype){
                lexer.TokenType.If => try ast.stmts.append(alloc, self.parseIf()),
                lexer.TokenType.Ident => try ast.stmts.append(alloc, self.parseIdent()),
                lexer.TokenType.TypeLit => try ast.stmts.append(alloc, self.parseDeclare()),
                else => std.debug.panic("invalid token detected at line: {}\n", .{self.peek(0).line})
            }
        }



    }

    pub fn peek(self: Tokens, offset: u32) lexer.Token {
        if (self.index + offset >= self.tokens.len)
            return lexer.Token{.value = null};
        return self.tokens[self.index + offset];
    }

    pub fn parseIf(self: *Tokens, alloc: std.mem.Allocator) !NodeStmt
    {
        
    }
};


