const std = @import("std");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const parser = @import("parser.zig");
const cell = @import("cell.zig");
const out = @import("inpout.zig");
const prim = @import("primitive.zig");
const vec = @import("vector.zig");
const proc = @import("procedure.zig");
const eval = @import("eval.zig");
const sexp = @import("sexpr.zig");
const str = @import("string.zig");
const spc = @import("special.zig");

const Cell = cell.Cell;
const Lexer = lex.Lexer;
const Proc = proc.Proc;
const sxEnd = sexp.sxEnd;

const ver_major = 0;
const ver_minor = 1;
const ver_patch = 0;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    try out.init();
    defer out.deinit();

    out.print("ZLisp [{}.{}.{}]\n", .{ ver_major, ver_minor, ver_patch });

    try Cell.init(8192);
    defer Cell.deinit();
    try vec.init(8192);
    defer vec.deinit();
    try Proc.init(1024);
    defer Proc.deinit();
    try str.init();
    defer str.deinit();

    try eval.internKeywords();
    try chr.init();
    try spc.init();
    try prim.init();

    // Read files passed as arguments
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const lexer = Lexer.create(args[i]) catch |err| {
            out.print("Could not open file \"{s}\"; error: {any}.\n", .{ args[i], err });
            continue;
        };

        parser.parseFile(lexer);
        lexer.destroy();
    }

    // Start REPL
    const lexer = try Lexer.create(null);
    defer lexer.destroy();

    parser.parseFile(lexer);
}

