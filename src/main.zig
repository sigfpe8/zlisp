const std = @import("std");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const Lexer = lex.Lexer;
const parser = @import("parser.zig");
const cell = @import("cell.zig");
const Cell = cell.Cell;
const out = @import("inpout.zig");
const sym = @import("symbol.zig");
const prim = @import("primitive.zig");
const vec = @import("vector.zig");
const proc = @import("procedure.zig");
const eval = @import("eval.zig");
const sexp = @import("sexpr.zig");
const str = @import("string.zig");
const spc = @import("special.zig");
const Proc = proc.Proc;
const PtrTag = sexp.PtrTag;
const TagMask = sexp.TagMask;
const sxEnd = sexp.sxEnd;

const ReadError = @import("error.zig").ReadError;

const ver_major = 0;
const ver_minor = 1;
const ver_patch = 0;

const stdout = std.io.getStdOut().writer();
const stdin_file = std.io.getStdIn();
var buf_reader = std.io.bufferedReader(stdin_file.reader());
const stdin = buf_reader.reader();

var filein: std.fs.File.Reader = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    try stdout.print("ZLisp [{}.{}.{}]\n", .{ ver_major, ver_minor, ver_patch });

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

    var buf: [2048]u8 = undefined;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const path = args[i];
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            stdout.print("Could not open file \"{s}\", error: {any}.\n", .{ path, err }) catch {};
            continue;
        };
        defer file.close();

        filein = std.fs.File.reader(file);

        var lexer = Lexer{ .buffer = &buf, .line = buf[0..0], .nextLine = nextFileLine, .silent = true, };
        while (!lexer.eof) {
            repl(&lexer) catch |err| {
                try stdout.print("main file:  {!}\n", .{err});
            };
            lexer.inexpr = false;
            lexer.cpos = lexer.line.len; // Force new line
        }
    }

    var lexer = Lexer{ .buffer = &buf, .line = buf[0..0], .nextLine = nextStdinLine, };

    while (!lexer.eof) {
        repl(&lexer) catch |err| {
            try stdout.print("main stdin:  {!}\n", .{err});
        };
        lexer.inexpr = false;
        lexer.cpos = lexer.line.len; // Force new line
    }
}

fn repl(lexer: *Lexer) !void {
    try lexer.nextTokenChar();
    while (true) {
        lexer.nextToken() catch |err| {
            lexer.logError(err);
            return;
        };
        lexer.inexpr = true;
        var sexpr = parser.parseSexpr(lexer) catch |err| {
            lexer.logError(err);
            return;
        };
        lexer.inexpr = false;
        if (lexer.eof or sexpr == sxEnd)
            break;

        sexpr = eval.globalEnv.eval(sexpr) catch |err| {
            eval.logError(err);
            return;
        };

        if (!lexer.silent) {
            out.printSexpr(sexpr, true);
            try stdout.print("\n", .{});
        }
    }
    if (!lexer.silent)
        try stdout.print("\n", .{});
}

fn nextFileLine(lexer: *Lexer) ReadError!?[]const u8 {
    const buffer = lexer.buffer;
    const line = nextLine(filein, buffer);
    return line;
}

fn nextStdinLine(lexer: *Lexer) ReadError!?[]const u8 {
    if (lexer.inexpr) {
        std.debug.print("    ", .{});
    } else
        std.debug.print("> ", .{});
    const buffer = lexer.buffer;
    const line = nextLine(stdin, buffer);
    return line;
}

fn nextLine(reader: anytype, buffer: []u8) ReadError!?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(buffer,'\n',))
               orelse return null;
    // Trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

const expect = @import("std").testing.expect;
test "sizeof" {
    try stdout.print("\nsizeOf([]u8)={d}\n", .{@sizeOf([]u8)});
    try stdout.print("sizeOf([*]u8)={d}\n", .{@sizeOf([*]u8)});
    try stdout.print("sizeOf(?i64)={d}\n", .{@sizeOf(?i64)});
    try stdout.print("sizeOf(?*i64)={d}\n", .{@sizeOf(?*i64)});
    var optional_var: ?i32 = null;
    try expect(optional_var == null);
    optional_var = 123;
    try expect(optional_var == @as(i32,123));
}
