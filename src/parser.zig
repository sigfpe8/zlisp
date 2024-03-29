const std = @import("std");
const lex = @import("lexer.zig");
const Lexer = lex.Lexer;
const cell = @import("cell.zig");
const Cell = cell.Cell;
const erz = @import("error.zig");
const out = @import("inpout.zig");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const vec = @import("vector.zig");

const Sexpr = sexp.Sexpr;
const TaggedPtr = sexp.TaggedPtr;
const UntaggedPtr = sexp.UntaggedPtr;
const PtrTag = sexp.PtrTag;
const nil = sexp.nil;
const sxFalse = sexp.sxFalse;
const sxTrue  = sexp.sxTrue;
const sxEof = sexp.sxEof;
const makeNumber = lex.makeNumber;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeInteger = sexp.makeInteger;
const makeRational = sexp.makeRational;
const makeFloat = sexp.makeFloat;
const makeVector = sexp.makeVector;
const ReadError = lex.ReadError;
const quoteExpr = eval.quoteExpr;

const ParsingError = erz.ParsingError;
const SchemeError = erz.SchemeError;

/// Parse all expressions in current input file until eof
pub fn parseFile(lexer: *Lexer) void {
    while (!lexer.eof) {
        parseLine(lexer) catch |err| {
            lexer.logError(err);
        };
        lexer.inexpr = false;
        lexer.cpos = lexer.line.len; // Force new line
    }
}

/// Parse current input line.
/// If the current expression continues on the following
/// lines, read as many lines as necessary to complete it.
pub fn parseLine(lexer: *Lexer) !void {
    while (true) {
        lexer.nextToken() catch |err| {
            lexer.logError(err);
            return;
        };
        lexer.inexpr = true;
        var sexpr = parseSexpr(lexer) catch |err| {
            lexer.logError(err);
            return;
        };
        lexer.inexpr = false;
        if (lexer.eof or sexpr == sxEof)
            break;

        sexpr = eval.globalEnv.evalPop(sexpr) catch |err| {
            eval.logError(err);
            return;
        };

        if (lexer.isterm) {
            out.printSexpr(sexpr, true);
            out.print("\n", .{});
        }
    }
    if (lexer.isterm)
        out.print("\n", .{});
}

/// Parse one S-expression
/// Return tagged pointer to its internal form
/// Entry: token -> 1st token in Sexpr
/// Exit:  token -> last token in Sexpr
pub fn parseSexpr(lexer: *Lexer) !Sexpr {
    switch (lexer.token) {
        .end => return sxEof,
        .number => {
            return try makeNumber(lexer.number);
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
fn parseList(lexer: *Lexer) anyerror!Sexpr {
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
fn parseVector(lexer: *Lexer) anyerror!Sexpr {
    var len: u32 = 0;
    const base = eval.stackGetSP();
    defer eval.stackSetSP(base);

    while (lexer.token != .rparens) {
        if (lexer.token == .end)
            return ParsingError.ExpectedRightParenthesis;
        const exp = try parseSexpr(lexer);
        try eval.stackPush(exp);
        len += 1;
        try lexer.nextToken();
    }

    const tvec = eval.stackGetSlice(base, len);
    return makeVector(tvec[0..len]);
}
