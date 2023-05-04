const std = @import("std");
const mem = std.mem;
const print = std.debug.print;

pub const StringId = u32;
const StringOff = u32;

pub const String = struct {
    off: StringOff,
    len: u32,    
};

var ggpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = ggpa.allocator();

pub var stringsTable = std.ArrayList(String).init(allocator);
var bytesTable   = std.ArrayList(u8).init(allocator);
var freeStrings: StringId = 0;

pub fn init() !void {
    // Id 0 is reserved for the null (empty) string
    try stringsTable.append(.{ .off = 0, .len = 0, });
}

pub fn deinit() void {
    stringsTable.deinit();
    bytesTable.deinit();
}

pub fn add(str: []const u8) !StringId {
    // Next string offset is at the end of the current array
    const off: StringOff = @truncate(StringOff, bytesTable.items.len);
    try bytesTable.appendSlice(str);
    var sid: StringId = 0;

    if (freeStrings == 0) {
        // Next id is at the end of the current array
        sid = @truncate(StringId, stringsTable.items.len);
    } else {
        // Reuse id from the free list
        sid = freeStrings;
        freeStrings = stringsTable.items[sid].off;
    }

    try stringsTable.append(.{ .off = off, .len = @truncate(u32,str.len), });
    return sid;
}

pub fn free(sid: StringId) void {
    // assert(sid < stringsTable.len)
    stringsTable.items[sid].off = freeStrings;
    freeStrings = sid;
}


pub fn get(sid: StringId) []u8 {
    // assert(sid < stringsTable.len)
    const str = stringsTable.items[sid];
    const off = str.off;
    const len = str.len;
    return bytesTable.items[off..off+len];
}

test "basic strings" {
    const expect = std.testing.expect;
    try init();
    defer deinit();

    const s1 = try add("hello");
    const s2 = try add("Hello, World!");
    const s3 = try add("The quick brown fox jumps over the lazy dog.");
    try expect(s1 == 1);
    try expect(s2 == 2);
    try expect(s3 == 3);
    print("s0 = '{s}'\n", .{ get(0)});
    print("s1 = '{s}'\n", .{ get(s1)});
    print("s2 = '{s}'\n", .{ get(s2)});
    print("s3 = '{s}'\n", .{ get(s3)});
    free(s2);
    try expect(freeStrings == 2);
    const s4 = try add("Scheme");
    try expect(s4 == 2);
    free(s1);
    try expect(freeStrings == 1);
    free(s3);
    try expect(freeStrings == 3);
    free(s4);
    try expect(freeStrings == 2);
}