const std = @import("std");
const cel = @import("cell.zig");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const nbr = @import("number.zig");
const spc = @import("special.zig");
const str = @import("string.zig");
const sxp = @import("sexpr.zig");
const sym = @import("symbol.zig");
const vec = @import("vector.zig");

const Lexer = lex.Lexer;

const Sexpr = sxp.Sexpr;
const nil = sxp.nil;
const PtrTag = sxp.PtrTag;
const SpecialTag = sxp.SpecialTag;
const SpecialTagMask = sxp.SpecialTagMask;
const SpecialTagShift = sxp.SpecialTagShift;
const sxFalse = sxp.sxFalse;
const sxTrue = sxp.sxTrue;
const sxVoid = sxp.sxVoid;
const SymbolId = sym.SymbolId;
const TaggedInt = sxp.TaggedInt;
const TagMask = sxp.TagMask;
const TagShift = sxp.TagShift;
const UntaggedInt = sxp.UntaggedInt;
const makePort = sxp.makePort;

const EvalError = @import("error.zig").EvalError;
const getName = @import("primitive.zig").getName;

const getAsInt = nbr.getAsInt;
const getSign = nbr.getSign;

const MAXVECSIZE = vec.MAXVECSIZE;

var ggpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = ggpa.allocator();

pub const PortId = u32;

pub const Reader = std.fs.File.Reader;
pub const Writer = std.fs.File.Writer;

const InputPort = struct {
    lexer: *Lexer,
};

const OutputPort = struct {
    name: []const u8,
    writer: Writer,
};

const NewPort = union(enum) {
    inp: InputPort,
    out: OutputPort,
};

pub const Port = union(enum) {
    reader: Reader,
    writer: Writer,
};

// This crashes the compiler :(
// var portsTable = std.ArrayList(Port).initCapacity(allocator, 20);
var portsTable = std.ArrayList(Port).init(allocator);

var stdin = std.io.getStdIn().reader();   // Reader{}
var stdout = std.io.getStdOut().writer(); // Writer{}
var stderr = std.io.getStdOut().writer(); // Writer{}

var stdinPort:  Sexpr = makePort(0);
var stdoutPort: Sexpr = makePort(1);
var stderrPort: Sexpr = makePort(2);

pub fn init() !void {
    try portsTable.append(.{ .reader = stdin  }); // [0]
    try portsTable.append(.{ .writer = stdout }); // [1]
    try portsTable.append(.{ .writer = stderr }); // [2]
}

pub fn deinit() void {
    portsTable.deinit();
}

pub fn newReader(reader: Reader) !PortId {
    const pid = @truncate(PortId, portsTable.items.len);
    try portsTable.append(.{ .reader = reader });
    return pid;
}

pub fn newWriter(writer: Writer) !PortId {
    const pid = @truncate(PortId, portsTable.items.len);
    try portsTable.append(.{ .writer = writer });
    return pid;
}

pub fn print(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch |err| {
        std.debug.print("Error {any} writing to stdout:\n", .{err});
        std.debug.print(format, args);
    };
}

fn isInputPort(arg: Sexpr) bool {
    const exp = arg >> TagShift;
    const tag = @intToEnum(PtrTag, arg & TagMask);
    if (tag != .port)
        return false;
    switch (portsTable.items[exp]) {
        .reader => return true,
        .writer => return false,
    }
}

fn isOutputPort(arg: Sexpr) bool {
    const exp = arg >> TagShift;
    const tag = @intToEnum(PtrTag, arg & TagMask);
    if (tag != .port)
        return false;
    switch (portsTable.items[exp]) {
        .reader => return false,
        .writer => return true,
    }
}

pub fn pCurrentInputPort(args: []Sexpr) EvalError!Sexpr {
    _ = args.len;
    return stdinPort;
}

pub fn pCurrentOutputPort(args: []Sexpr) EvalError!Sexpr {
    _ = args.len;
    return stdoutPort;
}

pub fn pInputPortPred(args: []Sexpr) EvalError!Sexpr {
    // (input-port? <exp>)
    return if (isInputPort(args[0])) sxTrue else sxFalse;
}

pub fn pOutputPortPred(args: []Sexpr) EvalError!Sexpr {
    // (output-port? <exp>)
    return if (isOutputPort(args[0])) sxTrue else sxFalse;
}

pub fn pOpenOutputFile(args: []Sexpr) EvalError!Sexpr {
    // (open-output-file <exp>)
    const arg = args[0];
    const exp = arg >> TagShift;
    const tag = @intToEnum(PtrTag, arg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;

    const path = str.get(exp);
    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
        std.debug.print("Could not open file \"{s}\" for output, error: {any}.\n", .{ path, err });
        return EvalError.OpenOutputFileFailed;
    };

    const id = try newWriter(file.writer());
    return makePort(id);
}

pub fn pCloseOutputPort(args: []Sexpr) EvalError!Sexpr {
    // (close-output-port <port>)
    const arg = args[0];
    const pid = arg >> TagShift;
    if (!isOutputPort(arg))
        return EvalError.ExpectedOutputPort;
    const writer = portsTable.items[pid].writer;
    writer.context.close();
    return sxVoid;
}

pub fn pCloseInputPort(args: []Sexpr) EvalError!Sexpr {
    // (close-input-port <port>)
    const arg = args[0];
    const pid = arg >> TagShift;
    if (!isInputPort(arg))
        return EvalError.ExpectedInputPort;
    const reader = portsTable.items[pid].reader;
    reader.context.close();
    return sxVoid;
}

pub fn pDisplay(args: []Sexpr) EvalError!Sexpr {
    // (display <exp>)
    const save_stdout = stdout;
    defer stdout = save_stdout;
    const save_stdoutPort = stdoutPort;
    defer stdoutPort = save_stdoutPort;

    if (args.len == 2) {
        // (display <exp> <port>)
        const arg = args[1];
        if (!isOutputPort(arg))
            return EvalError.ExpectedOutputPort;
        const port = portsTable.items[arg >> TagShift];
        stdout = port.writer;
    }

    const arg = args[0];
    const exp = arg >> TagShift;
    const tag = @intToEnum(PtrTag, arg & TagMask);
    switch (tag) {
        .char => {
            const ch = @truncate(u8, exp);
            print("{c}", .{ch});
        },
        .string => {
            // TODO: slashify the string
            print("{s}", .{str.get(exp)});
        },
        else => printSexpr(arg, true),
    }
    return sxVoid;
}

pub fn pNewline(args: []Sexpr) EvalError!Sexpr {
    // (newline)
    const save_stdout = stdout;
    defer stdout = save_stdout;
    const save_stdoutPort = stdoutPort;
    defer stdoutPort = save_stdoutPort;

    if (args.len == 1) {
        // (newline <port>)
        const arg = args[0];
        if (!isOutputPort(arg))
            return EvalError.ExpectedOutputPort;
        const port = portsTable.items[arg >> TagShift];
        stdout = port.writer;
    }

    print("\n", .{});
    return sxVoid;
}

pub fn pWrite(args: []Sexpr) EvalError!Sexpr {
    // (write <exp>)
    const save_stdout = stdout;
    defer stdout = save_stdout;
    const save_stdoutPort = stdoutPort;
    defer stdoutPort = save_stdoutPort;

    if (args.len == 2) {
        // (write <exp> <port>)
        const arg = args[1];
        if (!isOutputPort(arg))
            return EvalError.ExpectedOutputPort;
        const port = portsTable.items[arg >> TagShift];
        stdout = port.writer;
    }

    printSexpr(args[0], true);
    return sxVoid;
}

pub fn pWriteChar(args: []Sexpr) EvalError!Sexpr {
    // (write-char <exp>)
    const save_stdout = stdout;
    defer stdout = save_stdout;
    const save_stdoutPort = stdoutPort;
    defer stdoutPort = save_stdoutPort;

    if (args.len == 2) {
        // (write-char <exp> <port>)
        const arg = args[1];
        if (!isOutputPort(arg))
            return EvalError.ExpectedOutputPort;
        const port = portsTable.items[arg >> TagShift];
        stdout = port.writer;
    }

    // For now, just ASCII characters
    const arg = args[0];
    const tag = @intToEnum(PtrTag, arg & TagMask);
    const exp = @truncate(u8, arg >> TagShift);
    if (tag != .char)
        return EvalError.ExpectedCharacter;
    
    print("{c}", .{exp});
    return sxVoid;
}

// How to print complex numbers
const printZero  = 0b00000; // Both re and im are zero
const printReal  = 0b00001; // re !=0 so print it
const printPlus  = 0b00010; // im > 0 so print "+"
const printMinus = 0b00100; // im = -1 so print "-"
const printImag  = 0b01000; // im !=-1 and im != 0 and im != 1 so print it
const printI     = 0b10000; // im != 0 so print "i"

/// Checks whether the imaginary part of a number is -1 or +1
/// Returns: -1 or 1 if the imaginary part is either of these values
///          0 otherwise
fn isUnit(num: Sexpr) i64 {
    var int: i64 = undefined;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int, .integer => {
            int = getAsInt(num);
        },
        else => {
            int = 0;
        },
    }
    return if (int == -1 or int == 1) int else 0;
}

/// Examines the real and imaginary parts of a complex number
/// and returns a series of bit flags that determine how the
/// number should be printed.
fn complexPrintFlags(re: Sexpr, im: Sexpr) u32 {
    var flags: u32 = printZero;
    const re_sign = getSign(re);
    const im_sign = getSign(im);
    const im_unit = isUnit(im);

    // 0+0i ==> 0
    if (re_sign == 0 and im_sign == 0)
        return flags;

    // 2, 2+3i
    if (re_sign != 0)
        flags |= printReal;

    // 2+3i, +3i, +3/4i, 1+0.75i
    if (im_sign > 0)
        flags |= printPlus;

    // -i
    if (im_unit == -1)
        flags |= printMinus;

    // -3i or 3i
    if (im_sign != 0 and im_unit == 0)
        flags |= printImag;

    // 2+3i, -i, +i
    if (im_sign != 0)
        flags |= printI;

    return flags;
}

pub fn printSexpr(sexpr: Sexpr, quoted: bool) void {
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
            const val = cel.cellArray[exp].int;
            print("{d}", .{val});
        },
        .rational => {
            const num = getAsInt(cel.cellArray[exp].rat.num);
            const den = getAsInt(cel.cellArray[exp].rat.den);
            print("{d}/{d}", .{ num, den });
        },
        .float => {
            const val = cel.cellArray[exp].flt;
            print("{d}", .{val});
        },
        .polar => {
            printSexpr(cel.cellArray[exp].pol.mag, false);
            print("@", .{});
            printSexpr(cel.cellArray[exp].pol.ang, false);
        },
        .complex => {
            const re = cel.cellArray[exp].cmp.re;
            const im = cel.cellArray[exp].cmp.im;
            const flags = complexPrintFlags(re, im);
            if (flags == printZero) {
                print("0", .{});
            } else {
                if (flags & printReal != 0)
                    printSexpr(re, false);
                if (flags & printPlus != 0)
                    print("+", .{});
                if (flags & printMinus != 0)
                    print("-", .{});
                if (flags & printImag != 0)
                    printSexpr(im, false);
                if (flags & printI != 0)
                    print("i", .{});
            }
        },
        .boolean => {
            print("#{s}", .{if (exp == 0) "f" else "t"});
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
            const ptr = cel.cellArray[exp].dot;
            if (quoted)
                print("'", .{});
            print("(", .{});
            printSexpr(ptr.car, false);
            printList(ptr.cdr);
            print(")", .{});
        },
        .primitive => {
            print("#<primitive:{s}>", .{getName(exp)});
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
                printSexpr(v[i], false);
                i += 1;
                if (i < siz)
                    print(" ", .{});
            }
            print(")", .{});
        },
        .port => {
            print("#<port>", .{});
        },
    }
}

pub fn printList(sexpr: Sexpr) void {
    if (sexpr == nil)
        return;
    print("{s}", .{" "});
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    if (tag == .pair) { // (A B...)
        const dot = cel.cellArray[sexpr >> TagShift].dot;
        printSexpr(dot.car, false);
        printList(dot.cdr);
    } else { // (A . B)
        print("{s}", .{". "});
        printSexpr(sexpr, false);
    }
}
