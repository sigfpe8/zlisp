const std = @import("std");

// Character names

const CharName = struct {
    name: []const u8,   // Character name
    code: u32,          // U+code
    prefer: bool,       // True if this is the preferred name
};

const charNameTable = [_]CharName{
    .{ .name = "alarm",     .code = 0x0007, .prefer = true  },  //  = #\bell
    .{ .name = "backspace", .code = 0x0008, .prefer = true  },  //  backspace
    .{ .name = "bell",      .code = 0x0007, .prefer = false },  //  = #\alarm
    .{ .name = "delete",    .code = 0x007F, .prefer = true  },  //  = #\rubout
    .{ .name = "escape",    .code = 0x001B, .prefer = true  },  //  escape
    .{ .name = "formfeed",  .code = 0x000C, .prefer = true  },  //  = #\page
    .{ .name = "linefeed",  .code = 0x000A, .prefer = true  },  //  = #\newline
    .{ .name = "newline",   .code = 0x000A, .prefer = false },  //  = #\linefeed
    .{ .name = "nul",       .code = 0x0000, .prefer = false },  //  = #\null
    .{ .name = "null",      .code = 0x0000, .prefer = true  },  //  = #\nul
    .{ .name = "page",      .code = 0x000C, .prefer = false },  //  = #\formfeed
    .{ .name = "return",    .code = 0x000D, .prefer = true  },  //  carriage return
    .{ .name = "rubout",    .code = 0x007F, .prefer = false },  //  = #\delete
    .{ .name = "space",     .code = 0x0020, .prefer = true  },  //  space
    .{ .name = "tab",       .code = 0x0009, .prefer = true  },  //  tab
    .{ .name = "vtab",      .code = 0x000B, .prefer = true  },  //  vertical tab
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

// Maps a character name to its index in charNameTable[]
var charNameHashMap = std.StringHashMap(u32).init(allocator);

// Maps a character code to its index in charNameTable[]
var charCodeHashMap = std.AutoHashMap(u32, u32).init(allocator);

pub fn init() !void {
    for (charNameTable) |cns, i| {
        try charNameHashMap.put(cns.name, @truncate(u32,i));
        if (cns.prefer)
            try charCodeHashMap.put(cns.code, @truncate(u32,i));
    }
}

pub fn codeFromName(name: []const u8) ?u32 {
    if (charNameHashMap.get(name)) |i|
        return charNameTable[i].code;
    return null;
}

pub fn nameFromCode(code: u32) ?[]const u8 {
    if (charCodeHashMap.get(code)) |i|
        return charNameTable[i].name;
    return null;
}

test "Testing character names" {
    const expect = @import("std").testing.expect;
    const equal  = std.mem.eql;

    try init();
    try expect(codeFromName("alarm").? == 7);
    try expect(codeFromName("backspace").? == 8);
    try expect(codeFromName("bell").? == 7);
    try expect(codeFromName("delete").? == 127);
    try expect(codeFromName("escape").? == 27);
    try expect(codeFromName("formfeed").? == 12);
    try expect(codeFromName("linefeed").? == 10);
    try expect(codeFromName("newline").? == 10);
    try expect(codeFromName("nul").? == 0);
    try expect(codeFromName("null").? == 0);
    try expect(codeFromName("page").? == 12);
    try expect(codeFromName("return").? == 13);
    try expect(codeFromName("space").? == 32);
    try expect(codeFromName("tab").? == 9);
    try expect(codeFromName("vtab").? == 11);
    try expect(codeFromName("SpaceX") == null);

    try expect(equal(u8, nameFromCode(0).?, "null"));
    try expect(equal(u8, nameFromCode(7).?, "alarm"));
    try expect(equal(u8, nameFromCode(8).?, "backspace"));
    try expect(equal(u8, nameFromCode(9).?, "tab"));
    try expect(equal(u8, nameFromCode(10).?, "linefeed"));
    try expect(equal(u8, nameFromCode(11).?, "vtab"));
    try expect(equal(u8, nameFromCode(12).?, "formfeed"));
    try expect(equal(u8, nameFromCode(13).?, "return"));
    try expect(equal(u8, nameFromCode(27).?, "escape"));
    try expect(equal(u8, nameFromCode(32).?, "space"));
    try expect(equal(u8, nameFromCode(127).?, "delete"));
    try (expect(nameFromCode(255) == null));
}
