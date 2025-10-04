const std = @import("std");
const cel = @import("cell.zig");
const chr = @import("char.zig");
const lex = @import("lexer.zig");
const nbr = @import("number.zig");
const par = @import("parser.zig");
const pri = @import("primitive.zig");
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
const sxEof = sxp.sxEof;
const sxFalse = sxp.sxFalse;
const sxTrue = sxp.sxTrue;
const sxVoid = sxp.sxVoid;
const SymbolId = sym.SymbolId;
const TaggedInt = sxp.TaggedInt;
const TagMask = sxp.TagMask;
const TagShift = sxp.TagShift;
const UntaggedInt = sxp.UntaggedInt;
const makeChar = sxp.makeChar;
const makePort = sxp.makePort;

const EvalError = @import("error.zig").EvalError;
// const getName = @import("primitive.zig").getName;

const getAsInt = nbr.getAsInt;
const getSign = nbr.getSign;

var ggpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = ggpa.allocator();

pub const PortId = u32;

pub const Reader = std.Io.Reader;
pub const Writer = std.Io.Writer;

const Printer = struct {
    buffer: []u8,                   // Output buffer
    name: []const u8,               // File/device name
    file_writer: std.fs.File.Writer,// File.Writer
    writer: *Writer,                // Writer interface
    isopen: bool,
    isterm: bool,

    const BUFFER_SIZE = 2048;

    /// Create an instance of Printer{} for output to file 'name'.
    /// The special names 'stdout' and 'stderr' are assumed to be a console/terminal.
    pub fn create(name: []const u8) !*Printer {
        var printer: *Printer = try allocator.create(Printer);
        errdefer allocator.destroy(printer);

        const buffer = try allocator.alloc(u8, BUFFER_SIZE);
        errdefer allocator.free(buffer);
        printer.buffer = buffer;

        var file: std.fs.File = undefined;
        if (std.mem.eql(u8, name, "stdout")) {
            printer.isterm = true;
            file = std.fs.File.stdout();
        } else if (std.mem.eql(u8, name, "stderr")) {
            printer.isterm = true;
            file = std.fs.File.stderr();
        } else {
            // Regular file
            printer.isterm = false;
            file = try std.fs.cwd().createFile(name, .{});
        }
        errdefer if (!printer.isterm) file.close();

        printer.name = try lex.strDup(name);
        printer.isopen = true;

        printer.file_writer = file.writer(buffer);
        printer.writer = &printer.file_writer.interface;

        return printer;
    }

    pub fn destroy(self: *Printer) void {
        self.writer.flush() catch {};
        if (!self.isterm)
            self.file_writer.file.close();
        allocator.free(self.buffer);
        allocator.free(self.name);
        allocator.destroy(self);
    }
};

pub const Port = union(enum) {
    reader: *Lexer,
    writer: *Printer,
};

// This crashes the compiler :(
// var portsTable = std.ArrayList(Port).initCapacity(allocator, 20);
var portsTable: std.ArrayList(Port) = .empty;

var stdin:  *Reader = undefined;
var stdout: *Writer = undefined;
var stderr: *Writer = undefined;

var stdinPort:  Sexpr = makePort(0);
var stdoutPort: Sexpr = makePort(1);
var stderrPort: Sexpr = makePort(2);

pub fn init() !void {
    const inpp = try Lexer.create("stdin");
    stdin = inpp.reader;
    const outp = try Printer.create("stdout");
    stdout = outp.writer;
    const errp = try Printer.create("stderr");
    stderr = errp.writer;

    try portsTable.append(allocator, .{ .reader = inpp });   // [0]
    try portsTable.append(allocator, .{ .writer = outp }); // [1]
    try portsTable.append(allocator, .{ .writer = errp }); // [2]
}

pub fn deinit() void {
    portsTable.deinit(allocator);
}

pub fn getName(pid: PortId) []const u8 {
    const port = portsTable.items[pid];
    switch (port) {
        .reader => return port.reader.name,
        .writer => return port.writer.name,
    }
}

// ---------- Input ---------------------------------------

pub fn pCurrentInputPort(_: []Sexpr) EvalError!Sexpr {
    // (current-input-port)
    return stdinPort;
}

pub fn pInputPortPred(args: []Sexpr) EvalError!Sexpr {
    // (input-port? <exp>)
    return if (isInputPort(args[0])) sxTrue else sxFalse;
}

pub fn pOpenInputFile(args: []Sexpr) EvalError!Sexpr {
    // (open-input-file <exp>)
    const arg = args[0];
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;

    const path = str.get(exp);
    const lexer = Lexer.create(path) catch |err| {
        std.debug.print("Error opening '{s}' for input: {any}\n", .{path,err});
        return EvalError.OpenInputFileFailed;
    };
    const pid = try newInputPort(lexer);
    return makePort(pid);
}

pub fn pCloseInputPort(args: []Sexpr) EvalError!Sexpr {
    // (close-input-port <port>)
    const arg = args[0];
    const pid = arg >> TagShift;
    if (!isInputPort(arg))
        return EvalError.ExpectedInputPort;
    const reader = portsTable.items[pid].reader;
    if (!reader.isterm)
        reader.file_reader.file.close();
    reader.isopen = false;
    return sxVoid;
}

pub fn getStdinLexer() *Lexer {
    const lexer: *Lexer = portsTable.items[0].reader;
    
    return lexer;
}

pub fn pRead(args: []Sexpr) EvalError!Sexpr {
    var lexer = try getLexer(args);

    // Advance to next token
    lexer.nextToken() catch |err| {
        lexer.logError(err);
        return sxVoid;
    };

    // Read one S-expression
    const sexpr = par.parseSexpr(lexer) catch |err| {
        lexer.logError(err);
        return sxVoid;
    };

    return sexpr;
}

pub fn pReadChar(args: []Sexpr) EvalError!Sexpr {
    var lexer = try getLexer(args);

    // Advance to next character
    lexer.nextTokenChar() catch |err| {
        lexer.logError(err);
        return sxVoid;
    };

    if (lexer.eof)
        return sxEof;

    // Return current character
    return makeChar(lexer.cchar);
}

pub fn pPeekChar(args: []Sexpr) EvalError!Sexpr {
    var lexer = try getLexer(args);

    const peek = lexer.peekNextChar();

    // FIX: With the current mechanism we return EOF at
    // the end of a line.
    if (peek == 0 or lexer.eof)
        return sxEof;

    // Return next character
    return makeChar(peek);
}

pub fn pCharReadyPred(args: []Sexpr) EvalError!Sexpr {
    _ = try getLexer(args);

    // FIX: in case of interactive terminal, really test
    // if character is available or whether a read-char
    // would block.
    return sxTrue;
}

pub fn pEofPred(args: []Sexpr) EvalError!Sexpr {
    // (eof-object? <exp>)
    return if (args[0] == sxEof) sxTrue else sxFalse;
}

pub fn pLoad(args: []Sexpr) EvalError!Sexpr {
    // (load <exp>)
    const arg = args[0];
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;

    const path = str.get(exp);
    loadFile(path);

    return sxVoid;
}

pub fn loadFile(path: []const u8) void {
    const lexer = Lexer.create(path) catch |err| {
        print("Could not load file \"{s}\"; error: {any}.\n", .{ path, err });
        return;
    };

    par.parseFile(lexer);
    lexer.destroy();
}

fn newInputPort(reader: *Lexer) !PortId {
    const pid: PortId = @truncate(portsTable.items.len);
    try portsTable.append(allocator, .{ .reader = reader });
    return pid;
}

fn isInputPort(arg: Sexpr) bool {
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    if (tag != .port)
        return false;
    switch (portsTable.items[exp]) {
        .reader => return true,
        .writer => return false,
    }
}

fn getLexer(args: []Sexpr) !*Lexer {
    if (args.len == 0) {
        return getStdinLexer();
    } else {
        const arg = args[0];
        if (!isInputPort(arg))
            return EvalError.ExpectedInputPort;
        const port = portsTable.items[arg >> TagShift];
        if (!port.reader.isopen)
            return EvalError.PortIsClosed;
        return port.reader;
    }
}


// ---------- Output --------------------------------------
pub fn print(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch |err| {
        std.debug.print("Error {any} writing to stdout:\n", .{err});
        std.debug.print(format, args);
    };
    stdout.flush() catch |err| {
        std.debug.print("Error {any} flushing stdout:\n", .{err});
    };
}

pub fn pCurrentOutputPort(_: []Sexpr) EvalError!Sexpr {
    // (current-input-port)
    return stdoutPort;
}

pub fn pOutputPortPred(args: []Sexpr) EvalError!Sexpr {
    // (output-port? <exp>)
    return if (isOutputPort(args[0])) sxTrue else sxFalse;
}

pub fn pOpenOutputFile(args: []Sexpr) EvalError!Sexpr {
    // (open-output-file <exp>)
    const arg = args[0];
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;

    const path = str.get(exp);
    const printer = Printer.create(path) catch |err| {
        std.debug.print("Error opening '{s}' for output: {any}\n", .{path,err});
        return EvalError.OpenOutputFileFailed;
    };
    const pid = try newOutputPort(printer);
    return makePort(pid);
}

pub fn pCloseOutputPort(args: []Sexpr) EvalError!Sexpr {
    // (close-output-port <port>)
    const arg = args[0];
    const pid = arg >> TagShift;
    if (!isOutputPort(arg))
        return EvalError.ExpectedOutputPort;
    const writer = portsTable.items[pid].writer;
    if (!writer.isterm)
        writer.file_writer.file.close();
    writer.isopen = false;
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
        if (!port.writer.isopen)
            return EvalError.PortIsClosed;
        stdout = port.writer.writer;
    }

    const arg = args[0];
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    switch (tag) {
        .char => {
            const ch: u8 = @truncate(exp);
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
        if (!port.writer.isopen)
            return EvalError.PortIsClosed;
        stdout = port.writer.writer;
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
        if (!port.writer.isopen)
            return EvalError.PortIsClosed;
        stdout = port.writer.writer;
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
        if (!port.writer.isopen)
            return EvalError.PortIsClosed;
        stdout = port.writer.writer;
    }

    // For now, just ASCII characters
    const arg = args[0];
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    const exp: u8 = @truncate(arg >> TagShift);
    if (tag != .char)
        return EvalError.ExpectedCharacter;
    
    print("{c}", .{exp});
    return sxVoid;
}

pub fn printSexpr(sexpr: Sexpr, quoted: bool) void {
    if (sexpr == nil) {
        if (quoted)
            print("'", .{});
        print("()", .{});
        return;
    }
    const tag: PtrTag = @enumFromInt(sexpr & TagMask);
    const exp = sexpr >> TagShift;
    switch (tag) {
        .small_int => {
            const val: i64 = @as(i64, @as(TaggedInt, @bitCast(sexpr))) >> TagShift;
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
            const code: u8 = @truncate(exp);
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
            print("#<primitive:{s}>", .{pri.getName(exp)});
        },
        .procedure => {
            print("#<procedure>", .{});
        },
        .special => {
            if (sexpr == sxVoid) {
                // Don't print void
            } else if (sexpr == sxEof) {
                print("#<eof>", .{});
            } else if (@as(SpecialTag, @enumFromInt(exp & SpecialTagMask)) == .form) {
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
            print("#<port: id={d}, name=\"{s}\">", .{exp,getName(exp)});
        },
    }
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
    switch (@as(PtrTag, @enumFromInt(num & TagMask))) {
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

fn printList(sexpr: Sexpr) void {
    if (sexpr == nil)
        return;
    print("{s}", .{" "});
    const tag: PtrTag = @enumFromInt(sexpr & TagMask);
    if (tag == .pair) { // (A B...)
        const dot = cel.cellArray[sexpr >> TagShift].dot;
        printSexpr(dot.car, false);
        printList(dot.cdr);
    } else { // (A . B)
        print("{s}", .{". "});
        printSexpr(sexpr, false);
    }
}

fn newOutputPort(writer: *Printer) !PortId {
    const pid: PortId = @truncate(portsTable.items.len);
    try portsTable.append(allocator, .{ .writer = writer });
    return pid;
}

fn isOutputPort(arg: Sexpr) bool {
    const exp = arg >> TagShift;
    const tag: PtrTag = @enumFromInt(arg & TagMask);
    if (tag != .port)
        return false;
    switch (portsTable.items[exp]) {
        .reader => return false,
        .writer => return true,
    }
}

