const std = @import("std");
const mem = std.mem;
const print = std.debug.print;

// Some ideas adapted from:
//  https://zig.news/andrewrk/how-to-use-hash-map-contexts-to-save-memory-when-doing-a-string-table-3l33

pub const SymbolId = u32;
const Allocator = std.mem.Allocator;

var sym_table: std.StringHashMap(SymbolId) = undefined;

const SYMTABLESIZE = 2048;
var sym_bytes: [SYMTABLESIZE]u8 = undefined;
var sym_next_id: SymbolId = 0;

const SymbolError = error{
    SymTableArrayFull,
};

// The id of a symbol is its offset in the sym_bytes[] array.
//
//  sym_bytes[]:
//    +---------+---------+-----+
//    | symbol1 | symbol2 | ... |
//    +---------+---------+-----+
//     ^         ^
//     id1       id2
//
// Each symbol is stored as a u8 length followed by its name.
// Therefore the maximum length of a symbol is 255 bytes.
//
//    +---+-------------+
//    |len|  name       |
//    +---+-------------+
//         \___ len ___/
//     \____ symbol ___/
//
// A symbol has the same id in all scopes and is never removed from
// the table. Note that a symbol may be bound to different values in
// different environments, but its id is always the same.

pub fn init(allocator: Allocator) void {
    sym_table = std.StringHashMap(SymbolId).init(allocator);
}

pub fn deinit() void {
    // No need to free the keys because they are in the static sym_bytes[] array
    sym_table.deinit();
}

pub fn intern(symbol: []const u8) !SymbolId {
    const len: u8 = @truncate(symbol.len);
    // print("intern('{s}'): ", .{symbol});

    // If the symbol exists in sym_table[], return its value
    if (sym_table.get(symbol)) |id| {
        // print("found: {d}\n", .{id});
        return id;
    }

    // Get a new id (offset to where it will go in sym_bytes[])
    const id: SymbolId = sym_next_id;

    // print("new id: {d}\n", .{id});

    // Check for room in bytes array
    if (id + len + 1 > SYMTABLESIZE)
        return SymbolError.SymTableArrayFull;

    // Copy symbol to sym_bytes[]
    sym_bytes[id] = len;
    @memcpy(sym_bytes[id + 1 .. id + 1 + len], symbol);
    sym_next_id += len + 1;

    // Add it to sym_table[]
    try sym_table.put(sym_bytes[id + 1 .. id + 1 + len], id);
    return id;
}

pub fn isSymbol(symbol: []const u8) bool {
    return sym_table.contains(symbol);
}

pub fn getName(id: SymbolId) []u8 {
    const len = sym_bytes[id];
    return sym_bytes[id + 1 .. id + len + 1];
}

pub fn symbolListAll() void {
    print("Symbol table: count={d}, capacity={d}\n", .{ sym_table.count(), sym_table.capacity() });

    var it = sym_table.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const value = entry.value_ptr.*;
        std.debug.print("{s}: {d}\n", .{ key, value });
    }
}

test "Testing symbols" {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    // defer {
    //     const check = gpa.deinit();
    //     if (check == .leak) {
    //         std.debug.print("Memory leak detected!\n", .{});
    //     }
    // }

    const expect = std.testing.expect;

    init(allocator);
    defer deinit();

    print("\nTypeOf(sym_table)={s}\n", .{@typeName(@TypeOf(sym_table))});

    const h = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    //    init();
    const sym1 = try intern("hello");
    const sym2 = try intern("world");
    const sym3 = try intern(&h);
    const sym4 = try intern("scheme");

    try expect(sym1 == 0);
    try expect(sym2 == 6);
    try expect(sym3 == 0);
    try expect(sym4 == 12);

    try expect(isSymbol("hello") == true);
    try expect(isSymbol("world") == true);
    try expect(isSymbol("scheme") == true);
    try expect(isSymbol("lisp") == false);
    try expect(isSymbol("+") == false);

    print("sym hello  => {s}\n", .{getName(sym1)});
    print("sym world  => {s}\n", .{getName(sym2)});
    print("sym scheme => {s}\n", .{getName(sym4)});
}
