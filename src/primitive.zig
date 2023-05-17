const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const cell = @import("cell.zig");
const str = @import("string.zig");
const sym = @import("symbol.zig");

const Sexpr = sexp.Sexpr;
const PtrTag = sexp.PtrTag;
const SymbolId = sym.SymbolId;
const sxFalse = sexp.sxFalse;
const sxTrue  = sexp.sxTrue;
const nil = sexp.nil;
const TagShift = sexp.TagShift;
const TagMask = sexp.TagMask;
const EvalError = eval.EvalError;
const UntaggedPtr = sexp.UntaggedPtr;
const TaggedInt = sexp.TaggedInt;
const print = std.debug.print;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makePair = sexp.makePair;
const makeInteger = sexp.makeInteger;
const makeFloat = sexp.makeFloat;
const unlimited = std.math.maxInt(u32);

// All functions named 'pXXX()' are primitives (a.k.a in Scheme as
// standard procedures). They receive a single argument which is a
// slice (counted array) of the actual arguments. For example:
//
// (+ 1 2 3 4 5) causes pPlus() to receive the list []{1 2 3 4 5}.
// This list can have any length, including zero.

pub const PrimitId = u32;

// Function dispatch
const FunDisp = struct {
    name: []const u8, // Primitive name (e.g. "car")
    func: *const fn ([]Sexpr) EvalError!Sexpr, // Function that implements it (e.g. pCar)
    min: u32,         // Minimum # of arguments (1)
    max: u32,         // Maximum # of arguments (1)
};

const PrimitTable = [_]FunDisp{
    .{ .name = "boolean?",      .func = pBoolPred,  .min = 1, .max = 1, },
    .{ .name = "car",           .func = pCar,       .min = 1, .max = 1, },
    .{ .name = "cdr",           .func = pCdr,       .min = 1, .max = 1, },
    .{ .name = "cons",          .func = pCons,      .min = 2, .max = 2, },
    .{ .name = "length",        .func = pLength,    .min = 1, .max = 1, },
    .{ .name = "list",          .func = pList,      .min = 0, .max = unlimited, },
    .{ .name = "list?",         .func = pListPred,  .min = 1, .max = 1, },
    .{ .name = "null?",         .func = pNullPred,  .min = 1, .max = 1, },
    .{ .name = "number?",       .func = pNumPred,   .min = 1, .max = 1, },
    .{ .name = "pair?",         .func = pPairPred,  .min = 1, .max = 1, },
    .{ .name = "procedure?",    .func = pProcPred,  .min = 1, .max = 1, },
    .{ .name = "reverse",       .func = pReverse,   .min = 1, .max = 1, },
    .{ .name = "string?",       .func = pStrPred,   .min = 1, .max = 1, },
    .{ .name = "string-length", .func = pStrLen,    .min = 1, .max = 1, },
    .{ .name = "symbol?",       .func = pSymbPred,  .min = 1, .max = 1, },
    .{ .name = "vector?",       .func = pVecPred,   .min = 1, .max = 1, },
    .{ .name = "zero?",         .func = pZeroPred,  .min = 1, .max = 1, },
    .{ .name = "+",             .func = pPlus,      .min = 0, .max = unlimited, },
    .{ .name = "-",             .func = pMinus,     .min = 1, .max = unlimited, },
    .{ .name = "*",             .func = pTimes,     .min = 0, .max = unlimited, },
    .{ .name = "/",             .func = pDiv,       .min = 1, .max = unlimited, },
    .{ .name = "<",             .func = pLess,      .min = 1, .max = unlimited, },
    .{ .name = "<=",            .func = pLessEq,    .min = 1, .max = unlimited, },
    .{ .name = "=",             .func = pEqual,     .min = 1, .max = unlimited, },
    .{ .name = ">",             .func = pGrt,       .min = 1, .max = unlimited, },
    .{ .name = ">=",            .func = pGrtEq,     .min = 1, .max = unlimited, },
};

pub fn apply(pid: PrimitId, args: []Sexpr) EvalError!Sexpr {
    const nargs = args.len;
    const pt: *const FunDisp = &PrimitTable[pid];

    if (pt.min == pt.max and pt.min != nargs) {
        print("'{s}' expected {d} argument{s}, got {d}\n",
            .{ pt.name, pt.min, if (pt.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs < pt.min) {
        print("'{s}' expected at least {d} argument{s}, got {d}\n",
            .{ pt.name, pt.min, if (pt.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs > pt.max) {
        print("'{s}' expected at most {d} argument{s}, got {d}\n",
            .{ pt.name, pt.max, if (pt.max == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }

    // Call the primitive and return its value
    return pt.func(args);
}

pub fn init() !void {
    // Bind each primitive symbol (e.g. "car") to its index in PrimitTable
    for (PrimitTable) |p, i| {
        // i: PrimitId
        const id: SymbolId = try sym.intern(p.name);
        const exp = makeTaggedPtr(@truncate(UntaggedPtr, i), .primitive);
        try eval.globalEnv.setVar(id, exp);
    }
}

pub fn getName(id: PrimitId) []const u8 {
    return PrimitTable[id].name;
}

fn pBoolPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .boolean) sxTrue else sxFalse;
}

fn pNumPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .small_int or tag == .integer or tag == .float) sxTrue else sxFalse;
}

fn pPairPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .pair) sxTrue else sxFalse;
}

fn pProcPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .procedure or tag == .primitive) sxTrue else sxFalse;
}

fn pStrLen(args: []Sexpr) EvalError!Sexpr {
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;
    const len = str.stringsTable.items[exp >> TagShift].len;
    return makeInteger(len);
}

fn pStrPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .string) sxTrue else sxFalse;
}

fn pSymbPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .symbol) sxTrue else sxFalse;
}

fn pVecPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .vector) sxTrue else sxFalse;
}

fn pZeroPred(args: []Sexpr) EvalError!Sexpr {
    const exp = args[0];
    const ind = exp >> TagShift;
    const tag = @intToEnum(PtrTag, exp & TagMask);
    const isZero: bool = switch (tag) {
        .small_int => ind == 0,
        .integer => cell.cellArray[ind].int == 0,
        .float => cell.cellArray[ind].flt == 0.0,
        else => return EvalError.ExpectedNumber,
    };
    return  if (isZero) sxTrue else sxFalse;
}

fn pCar(args: []Sexpr) EvalError!Sexpr {
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp >> TagShift].dot.car;
}

fn pCdr(args: []Sexpr) EvalError!Sexpr {
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp >> TagShift].dot.cdr;
}

fn pCons(args: []Sexpr) EvalError!Sexpr {
    return makePair(args[0], args[1]);
}

fn pLength(args: []Sexpr) EvalError!Sexpr {
    var list = args[0];
    var len: i64 = 0;

    while (list != nil) {
        const tag = @intToEnum(PtrTag, list & TagMask);
        if (tag != .pair)
            return EvalError.ExpectedList;
        len += 1;
        list = cell.cellArray[list >> TagShift].dot.cdr;
    }

    return makeInteger(len);
}

fn pList(args: []Sexpr) EvalError!Sexpr {
    var list = nil;
    var i = args.len;

    while (i > 0) : (i -= 1) {
        list = try makePair(args[i-1], list);
    }

    return list;
}

fn pListPred(args: []Sexpr) EvalError!Sexpr {
    var arg = args[0];

    while (arg != nil) {
        const tag = @intToEnum(PtrTag, arg & TagMask);
        if (tag != .pair)
            return sxFalse;
        arg = cell.cellArray[arg >> TagShift].dot.cdr;
    }

    return sxTrue;
}

fn pReverse(args: []Sexpr) EvalError!Sexpr {
    var list = nil;
    var arg = args[0];

    while (arg != nil) {
        const tag = @intToEnum(PtrTag, arg & TagMask);
        if (tag != .pair)
            return EvalError.ExpectedList;
        list = try makePair(cell.cellArray[arg >> TagShift].dot.car, list);
        arg = cell.cellArray[arg >> TagShift].dot.cdr;
    }

    return list;
}

fn pNullPred(args: []Sexpr) EvalError!Sexpr {
    return if (args[0] == nil) sxTrue else sxFalse;
}


// Determine the highest number type in a list of arguments
// float > integer
fn maxNumType(args: []Sexpr) EvalError!PtrTag {
    for (args) |arg| {
        switch (@intToEnum(PtrTag, arg & TagMask)) {
            .small_int, .integer => {},
            .float => { return .float; },
            else => { return EvalError.ExpectedNumber; },
        }
    }

    return .integer;
}

fn getAsInt(num: Sexpr) i64 {
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { return @as(i64, @bitCast(TaggedInt, num)) >> TagShift; },
        .integer =>   { return cell.cellArray[num >> TagShift].int; },
        else => unreachable,
    }
}

fn getAsFloat(num: Sexpr) f64 {
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { return @intToFloat(f64, @as(i64, @bitCast(TaggedInt, num)) >> TagShift); },
        .integer =>   { return @intToFloat(f64, cell.cellArray[num >> TagShift].int); },
        .float =>     { return cell.cellArray[num >> TagShift].flt; },
        else => unreachable,
    }
}

fn pPlus(args: []Sexpr) EvalError!Sexpr {
    if (args.len > 0) {
        switch (try maxNumType(args)) {
            .integer => {
                var result: i64 = getAsInt(args[0]);
                for (args[1..]) |arg| {
                    result = result + getAsInt(arg);
                }
                return makeInteger(result);
            },
            .float => {
                var result: f64 =getAsFloat(args[0]);
                for (args[1..]) |arg| {
                    result = result + getAsFloat(arg);
                }
                return makeFloat(result);
            },
            else => unreachable,
        }
    }
    return makeInteger(0);
}

fn pMinus(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var result: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                result = result - getAsInt(arg);
            }
            if (args.len == 1) result = -result;
            return makeInteger(result);
        },
        .float => {
            var result: f64 = getAsFloat(args[0]);
            for (args[1..]) |arg| {
                result = result - getAsFloat(arg);
            }
            if (args.len == 1) result = -result;
            return makeFloat(result);
        },
        else => unreachable,
    }
    return 0;
}

fn pTimes(args: []Sexpr) EvalError!Sexpr {
    if (args.len > 0) {
        switch (try maxNumType(args)) {
            .integer => {
                var result: i64 = getAsInt(args[0]);
                for (args[1..]) |arg| {
                    result = result * getAsInt(arg);
                }
                return makeInteger(result);
            },
            .float => {
                var result: f64 = getAsFloat(args[0]);
                for (args[1..]) |arg| {
                    result = result * getAsFloat(arg);
                }
                return makeFloat(result);
            },
            else => unreachable,
        }
    }
    return makeInteger(1);
}

fn pDiv(args: []Sexpr) EvalError!Sexpr {
    // Perform divisions as floats
    var result: f64 = getAsFloat(args[0]);
    for (args[1..]) |arg| {
        const div = getAsFloat(arg);
        if (div == 0.0)
            return EvalError.DivisionByZero;
        result = result / div;
    }
    if (args.len == 1) {
        if (result == 0.0)
            return EvalError.DivisionByZero;
        result = 1.0 / result;
    }
    const iresult: i64 = @floatToInt(i64, result);
    // Return integer if possible
    if (@intToFloat(f64, iresult) == result)
        return makeInteger(iresult);
    return makeFloat(result);
}

fn pLess(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var previous: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                const next = getAsInt(arg);
                if (previous < next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .float => {
            var previous: f64 =getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const next = getAsFloat(arg);
                if (previous < next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
    unreachable;
}

fn pLessEq(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var previous: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                const next = getAsInt(arg);
                if (previous <= next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .float => {
            var previous: f64 =getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const next = getAsFloat(arg);
                if (previous <= next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
    unreachable;
}

fn pEqual(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var previous: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                const next = getAsInt(arg);
                if (previous == next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .float => {
            var previous: f64 =getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const next = getAsFloat(arg);
                if (previous == next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
    unreachable;
}

fn pGrt(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var previous: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                const next = getAsInt(arg);
                if (previous > next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .float => {
            var previous: f64 =getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const next = getAsFloat(arg);
                if (previous > next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
    unreachable;
}

fn pGrtEq(args: []Sexpr) EvalError!Sexpr {
    switch (try maxNumType(args)) {
        .integer => {
            var previous: i64 = getAsInt(args[0]);
            for (args[1..]) |arg| {
                const next = getAsInt(arg);
                if (previous >= next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .float => {
            var previous: f64 =getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const next = getAsFloat(arg);
                if (previous >= next) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
    unreachable;
}
