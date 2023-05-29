const std = @import("std");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const Lexer = lex.Lexer;
const cell = @import("cell.zig");
const Cell = cell.Cell;
const sexp = @import("sexpr.zig");
const str = @import("string.zig");
const sym = @import("symbol.zig");
const eval = @import("eval.zig");
const prim = @import("primitive.zig");
const vec = @import("vector.zig");
const spc = @import("special.zig");
const print = std.debug.print;

const Sexpr = sexp.Sexpr;
const TaggedPtr = sexp.TaggedPtr;
const UntaggedPtr = sexp.UntaggedPtr;
const TaggedInt = sexp.TaggedInt;
const UntaggedInt = sexp.UntaggedInt;
const PtrTag = sexp.PtrTag;
const SpecialTag = sexp.SpecialTag;
const SymbolId = sym.SymbolId;
const nil = sexp.nil;
const sxFalse = sexp.sxFalse;
const sxTrue  = sexp.sxTrue;
const sxEnd = sexp.sxEnd;
const sxVoid = sexp.sxVoid;
const TagShift = sexp.TagShift;
const TagMask = sexp.TagMask;
const SpecialTagShift = sexp.SpecialTagShift;
const SpecialTagMask = sexp.SpecialTagMask;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeInteger = sexp.makeInteger;
const makeFloat = sexp.makeFloat;
const makeVector = sexp.makeVector;
const ReadError = lex.ReadError;
const quoteExpr = eval.quoteExpr;

const ParsingError = error{
    ExpectedRightParenthesis,
    ExpectedRightBracket,
    ExpectedRightBrace,
    VectorIsTooLong,
};

const SexprError = ParsingError || eval.EvalError || lex.ReadError;

const MAXVECSIZE = vec.MAXVECSIZE;

/// Parse one S-expression
/// Return tagged pointer to its internal form
/// Entry: token -> 1st token in Sexpr
/// Exit:  token -> last token in Sexpr
pub fn parseSexpr(lexer: *Lexer) !Sexpr {
    switch (lexer.token) {
        .end => return sxEnd,
        .integer => {
            return try makeInteger(lexer.ivalue);
        },
        .float => {
            return makeFloat(lexer.fvalue);
        },
        .symbol => {
            return makeTaggedPtr(lexer.xvalue, .symbol);
        },
        .string => {
            return makeTaggedPtr(lexer.xvalue, .string);
        },
        .lparens => {
            const rparens = lexer.rparens;  // Matching right parenthesis
            try lexer.nextToken(); // Skip (
            const list = try parseList(lexer);
            if (lexer.token != .rparens)
                return ParsingError.ExpectedRightParenthesis;
            if (lexer.schar != rparens) {
                return switch (rparens) {
                    ']' => ParsingError.ExpectedRightBracket,
                    '}' => ParsingError.ExpectedRightBrace,
                    else => ParsingError.ExpectedRightParenthesis,
                };
            }
            return list;
        },
        .quote, .qquote, .unquote, .unquote_spl => {
            const name = switch (lexer.token) {
                .quote => eval.kwQuote,
                .qquote => eval.kwQuasiquote,
                .unquote => eval.kwUnquote,
                .unquote_spl => eval.kwUnquote_spl,
                else => unreachable,
            };
            // Transform '<exp> into (quote <exp>), similarly for ` , ,@
            try lexer.nextToken(); // Skip ' or ` or , or ,@
            const expr = try parseSexpr(lexer);
            return try quoteExpr(name, expr);
        },
        .hash_f => {
            return sxFalse;
        },
        .hash_t => {
            return sxTrue;
        },
        .hash_vec => {
            const rparens = lexer.rparens;  // Matching right parenthesis
            try lexer.nextToken(); // Skip #(
            const vexp = try parseVector(lexer);
            if (lexer.token != .rparens)
                return ParsingError.ExpectedRightParenthesis;
            if (lexer.schar != rparens) {
                return switch (rparens) {
                    ']' => ParsingError.ExpectedRightBracket,
                    '}' => ParsingError.ExpectedRightBrace,
                    else => ParsingError.ExpectedRightParenthesis,
                };
            }
            return vexp;
        },
        . hash_char => {
            return makeTaggedPtr(lexer.xvalue, .char);
        },
        else => return nil,
    }
    unreachable;
}

// Entry: token -> 1st token after (
// Exit:  token -> )
fn parseList(lexer: *Lexer) SexprError!Sexpr {
    if (lexer.token == .rparens or lexer.token == .end)
        return nil;
    const car = try parseSexpr(lexer);
    var cdr: Sexpr = undefined;
    try lexer.nextToken();
    if (lexer.token == .dot) { // (A . B)
        try lexer.nextToken();
        cdr = try parseSexpr(lexer);
        try lexer.nextToken();
    } else { // (A B...)
        cdr = try parseList(lexer);
    }
    const ptr = try Cell.alloc();
    cell.cellArray[ptr].dot.car = car;
    cell.cellArray[ptr].dot.cdr = cdr;
    return makeTaggedPtr(ptr, .pair);
}

// Entry: token -> 1st token after #(
// Exit:  token -> )
fn parseVector(lexer: *Lexer) SexprError!Sexpr {
    var tvec: [MAXVECSIZE]Sexpr = undefined;
    var siz: u32 = 0;

    while (lexer.token != .rparens) {
        if (lexer.token == .end)
            return ParsingError.ExpectedRightParenthesis;
        if (siz == MAXVECSIZE)
            return ParsingError.VectorIsTooLong;
        const exp = try parseSexpr(lexer);
        tvec[siz] = exp;
        siz += 1;
        try lexer.nextToken();
    }

    return makeVector(tvec[0..siz]);
}

pub fn printSexpr(sexpr: Sexpr, quoted: bool) !void {
    if (sexpr == nil) {
        if (quoted)
            print("'", .{});
        print("()", .{});
        return;
    }
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    const exp = sexpr >> TagShift;
    switch (tag) {
        .small_int => {
            const val: i64 = @as(i64, @bitCast(TaggedInt, sexpr)) >> TagShift;
            print("{d}", .{val});
        },
        .integer => {
            const val = cell.cellArray[exp].int;
            print("{d}", .{val});
        },
        .float => {
            const val = cell.cellArray[exp].flt;
            print("{d}", .{val});
        },
        .boolean => {
            print("#{s}", .{ if (exp == 0) "f" else "t" });
        },
        .char => {
            const code = @truncate(u8, exp);
            if (chr.nameFromCode(code)) |name| {
                print("#\\{s}", .{name});
            } else {
                print("#\\{c}", .{code});
            }
        },
        .pair => {
            const ptr = cell.cellArray[exp].dot;
            if (quoted)
                print("'", .{});
            print("(", .{});
            try printSexpr(ptr.car, false);
            try printList(ptr.cdr);
            print(")", .{});
        },
        .primitive => {
            print("#<primitive:{s}>", .{prim.getName(exp)});
        },
        .procedure => {
            print("#<procedure>", .{});
        },
        .special => {
            if (sexpr == sxVoid) {
                // Don't print void
            } else if (@intToEnum(SpecialTag, exp & SpecialTagMask) == .form) {
                print("#<special-form:{s}>", .{spc.getName(exp >> SpecialTagShift)});
            } else {
                print("#<special:unknown:{x}>", .{exp});
            }
        },
        .string => {
            print("\"{s}\"", .{str.get(exp)});
        },
        .symbol => {
            if (quoted)
                print("'", .{});
            print("{s}", .{sym.getName(exp)});
        },
        .vector => {
            const siz = vec.vecArray[exp];
            const v = vec.vecArray[exp+1 .. exp+1+siz];
            var i: u32 = 0;

            if (quoted)
                print("'", .{});
            print("#(", .{});
            while (i < siz) {
                try printSexpr(v[i], false);
                i += 1;
                if (i < siz)
                    print(" ", .{});
            }
            print(")", .{});
        },
    }
}

pub fn printList(sexpr: Sexpr) std.os.WriteError!void {
    if (sexpr == nil)
        return;
    print("{s}", .{" "});
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    if (tag == .pair) { // (A B...)
        const ptr = cell.cellArray[sexpr >> TagShift].dot;
        try printSexpr(ptr.car, false);
        try printList(ptr.cdr);
    } else { // (A . B)
        print("{s}", .{". "});
        try printSexpr(sexpr, false);
    }
}

const expect = @import("std").testing.expect;
test "parsing an s-expr" {
    //try expect(sexpr() == 0);
}
