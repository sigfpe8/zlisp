const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const parser = @import("parser.zig");
const cell = @import("cell.zig");
const Cell = cell.Cell;
const env = @import("eval.zig");
const sym = @import("symbol.zig");
const prim = @import("primitive.zig");
const vec = @import("vector.zig");
const proc = @import("procedure.zig");
const Proc = proc.Proc;

const ver_major = 0;
const ver_minor = 1;
const ver_patch = 0;

const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    const stdin_file = std.io.getStdIn();
    var buf_reader = std.io.bufferedReader(stdin_file.reader());
    const stdin = buf_reader.reader();

    try stdout.print("ZLisp [{}.{}.{}]\n", .{ ver_major, ver_minor, ver_patch });

    try Cell.init(8192);
    defer Cell.deinit();
    try vec.init(8192);
    defer vec.deinit();
    try Proc.init(1024);
    defer Proc.deinit();

    try env.internKeywords();
    try prim.init();

    const prompt = "> ";
    var buf: [2048]u8 = undefined;

    while (true) {
        // sym.symbolListAll();
        try stdout.print("{s}", .{prompt});
        var line = (try nextLine(stdin, &buf)) orelse break;
        parser.parseLine(line) catch |err| {
            try stdout.print("  {!}\n", .{err});
        };
    }
}

fn nextLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

test "sizeof" {
    try stdout.print("\nsizeOf([]u8)={d}\n", .{@sizeOf([]u8)});
    try stdout.print("sizeOf([*]u8)={d}\n", .{@sizeOf([*]u8)});
}
