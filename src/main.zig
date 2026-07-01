const std = @import("std");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const par = @import("parser.zig");
const cell = @import("cell.zig");
const iop = @import("inpout.zig");
const pri = @import("primitive.zig");
const vec = @import("vector.zig");
const proc = @import("procedure.zig");
const eval = @import("eval.zig");
const sexp = @import("sexpr.zig");
const str = @import("string.zig");
const spc = @import("special.zig");
const sym = @import("symbol.zig");

const Cell = cell.Cell;
const Lexer = lex.Lexer;
const Proc = proc.Proc;

const ver_major = 0;
const ver_minor = 1;
const ver_patch = 0;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    lex.init(io, allocator);
    sym.init(allocator);
    defer sym.deinit();

    try iop.init(io, allocator);
    defer iop.deinit();

    iop.print("ZLisp [{}.{}.{}]\n", .{ ver_major, ver_minor, ver_patch });

    try Cell.init(8192);
    defer Cell.deinit();
    try vec.init(8192);
    defer vec.deinit();
    try Proc.init(1024);
    defer Proc.deinit();
    try str.init(allocator);
    defer str.deinit();

    try eval.init(allocator);
    defer eval.deinit();
    try chr.init(allocator);
    defer chr.deinit();
    try spc.init();
    try pri.init();

    // Load files passed as arguments
    for (args[1..]) |arg| {
        iop.loadFile(arg);
    }

    // Start REPL
    const lexer = iop.getStdinLexer();
    par.parseFile(lexer);
}

