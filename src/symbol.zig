const std = @import("std");
const mem = std.mem;
const print = std.debug.print;

// Some ideas adapted from:
//  https://zig.news/andrewrk/how-to-use-hash-map-contexts-to-save-memory-when-doing-a-string-table-3l33

pub const SymbolId = u32;

var ggpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = ggpa.allocator();
//const allocator = std.testing.allocator;
var sym_table = std.StringHashMap(SymbolId).init(allocator);
// var sym_bytes = std.ArrayList(u8).init(allocator);
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

pub fn intern(symbol: []const u8) !SymbolId {
    const len: u8 = @truncate(u8, symbol.len);
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
    mem.copy(u8, sym_bytes[id + 1 .. id + 1 + len], symbol);
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
    const expect = @import("std").testing.expect;
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

// ---------------------------------------------------------------

// const SymTable = struct {
//     string_bytes: std.ArrayListUnmanaged(u8),
//     /// Key is string_bytes index.
//     string_table: std.HashMapUnmanaged(u32, void, IndexContext, std.hash_map.default_max_load_percentage),
// };

// const IndexContext = struct {
//     string_bytes: *std.ArrayListUnmanaged(u8),

//     pub fn eql(self: IndexContext, a: u32, b: u32) bool {
//         _ = self;
//         return a == b;
//     }

//     pub fn hash(self: IndexContext, x: u32) u64 {
//         const x_slice = mem.span(@ptrCast([*:0]const u8, self.string_bytes.items.ptr) + x);
//         return std.hash_map.hashString(x_slice);
//     }
// };

// const SliceAdapter = struct {
//     string_bytes: *std.ArrayListUnmanaged(u8),

//     pub fn eql(self: SliceAdapter, a_slice: []const u8, b: u32) bool {
//         const b_slice = mem.span(@ptrCast([*:0]const u8, self.string_bytes.items.ptr) + b);
//         return mem.eql(u8, a_slice, b_slice);
//     }

//     pub fn hash(self: SliceAdapter, adapted_key: []const u8) u64 {
//         _ = self;
//         return std.hash_map.hashString(adapted_key);
//     }
// };

// test "hash contexts" {
//     const gpa = std.testing.allocator;

//     var symbolTable: SymTable = .{
//         .string_bytes = .{},
//         .string_table = .{},
//     };
//     defer symbolTable.string_bytes.deinit(gpa);
//     defer symbolTable.string_table.deinit(gpa);

//     const index_context: IndexContext = .{ .string_bytes = &symbolTable.string_bytes };

//     const hello_index = @intCast(u32, symbolTable.string_bytes.items.len);
//     try symbolTable.string_bytes.appendSlice(gpa, "hello\x00");
//     try symbolTable.string_table.putContext(gpa, hello_index, {}, index_context);

//     const world_index = @intCast(u32, symbolTable.string_bytes.items.len);
//     try symbolTable.string_bytes.appendSlice(gpa, "world\x00");
//     try symbolTable.string_table.putContext(gpa, world_index, {}, index_context);

//     // now we want to check if a string exists based on a string literal
//     const slice_context: SliceAdapter = .{ .string_bytes = &symbolTable.string_bytes };
//     const found_entry = symbolTable.string_table.getEntryAdapted(@as([]const u8, "world"), slice_context).?;
//     try std.testing.expectEqual(found_entry.key_ptr.*, world_index);
// }
