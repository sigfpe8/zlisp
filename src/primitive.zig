const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const cell = @import("cell.zig");
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

pub const Primit = struct {
    name: []const u8, // Primitive name (e.g. "car")
    func: *const fn ([]Sexpr) EvalError!Sexpr, // Function that implements it (e.g. pCar)
    minargs: u32,     // Minimum # of arguments (1)
    maxargs: u32,     // Maximum # of arguments (1)
};

const PrimitTable = [_]Primit{
    .{ .name = "boolean?",    .func = pBoolPred, .minargs = 1, .maxargs = 1, },
    .{ .name = "car",         .func = pCar,      .minargs = 1, .maxargs = 1, },
    .{ .name = "cdr",         .func = pCdr,      .minargs = 1, .maxargs = 1, },
    .{ .name = "cons",        .func = pCons,     .minargs = 2, .maxargs = 2, },
    .{ .name = "length",      .func = pLength,   .minargs = 1, .maxargs = 1, },
    .{ .name = "number?",     .func = pNumPred,  .minargs = 1, .maxargs = 1, },
    .{ .name = "pair?",       .func = pPairPred, .minargs = 1, .maxargs = 1, },
    .{ .name = "procedure?",  .func = pProcPred, .minargs = 1, .maxargs = 1, },
    .{ .name = "symbol?",     .func = pSymbPred, .minargs = 1, .maxargs = 1, },
    .{ .name = "vector?",     .func = pVecPred,  .minargs = 1, .maxargs = 1, },
    .{ .name = "+",           .func = pPlus,     .minargs = 0, .maxargs = unlimited, },
    .{ .name = "-",           .func = pMinus,    .minargs = 1, .maxargs = unlimited, },
    .{ .name = "*",           .func = pTimes,    .minargs = 0, .maxargs = unlimited, },
    .{ .name = "/",           .func = pDiv,      .minargs = 1, .maxargs = unlimited, },
    .{ .name = "<",           .func = pLess,     .minargs = 1, .maxargs = unlimited, },
    .{ .name = "<=",          .func = pLessEq,   .minargs = 1, .maxargs = unlimited, },
    .{ .name = "=",           .func = pEqual,    .minargs = 1, .maxargs = unlimited, },
    .{ .name = ">",           .func = pGrt,      .minargs = 1, .maxargs = unlimited, },
    .{ .name = ">=",          .func = pGrtEq,    .minargs = 1, .maxargs = unlimited, },
};

pub fn apply(pid: PrimitId, args: []Sexpr) EvalError!Sexpr {
    const nargs = args.len;
    const pt: *const Primit = &PrimitTable[pid];

    if (pt.minargs == pt.maxargs and pt.minargs != nargs) {
        print("'{s}' expected {d} argument{s}, got {d}\n",
            .{ pt.name, pt.minargs, if (pt.minargs == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs < pt.minargs) {
        print("'{s}' expected at least {d} argument{s}, got {d}\n",
            .{ pt.name, pt.minargs, if (pt.minargs == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs > pt.maxargs) {
        print("'{s}' expected at most {d} argument{s}, got {d}\n",
            .{ pt.name, pt.maxargs, if (pt.maxargs == 1) "" else "s", nargs });
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

pub fn prim_getName(id: PrimitId) []const u8 {
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

fn pSymbPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .symbol) sxTrue else sxFalse;
}

fn pVecPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .vector) sxTrue else sxFalse;
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
        result = result / getAsFloat(arg);
    }
    if (args.len == 1)
        result = 1.0 / result;
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

const expect = @import("std").testing.expect;
test "primitives" {
    try expect(PrimitTable.len == 2);
}
