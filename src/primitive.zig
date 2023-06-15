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
const EvalError = @import("error.zig").EvalError;
const UntaggedPtr = sexp.UntaggedPtr;
const TaggedInt = sexp.TaggedInt;
const print = std.debug.print;
const makeChar = sexp.makeChar;
const makeComplex = sexp.makeComplex;
const makeInteger = sexp.makeInteger;
const makeFloat = sexp.makeFloat;
const makePair = sexp.makePair;
const makePolar = sexp.makePolar;
const makeTaggedPtr = sexp.makeTaggedPtr;
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
    .{ .name = "boolean?",         .func = pBoolPred,        .min = 1, .max = 1, },
    .{ .name = "car",              .func = pCar,             .min = 1, .max = 1, },
    .{ .name = "cdr",              .func = pCdr,             .min = 1, .max = 1, },
    .{ .name = "char?",            .func = pCharPred,        .min = 1, .max = 1, },
    .{ .name = "char->integer",    .func = pCharToInt,       .min = 1, .max = 1, },
    .{ .name = "complex?",         .func = pComplexPred,     .min = 1, .max = 1, },
    .{ .name = "cons",             .func = pCons,            .min = 2, .max = 2, },
    .{ .name = "exact?",           .func = pExactPred,       .min = 1, .max = 1, },
    .{ .name = "inexact?",         .func = pInexactPred,     .min = 1, .max = 1, },
    .{ .name = "integer?",         .func = pIntPred,         .min = 1, .max = 1, },
    .{ .name = "integer->char",    .func = pIntToChar,       .min = 1, .max = 1, },
    .{ .name = "length",           .func = pLength,          .min = 1, .max = 1, },
    .{ .name = "list",             .func = pList,            .min = 0, .max = unlimited, },
    .{ .name = "list?",            .func = pListPred,        .min = 1, .max = 1, },
    .{ .name = "make-polar",       .func = pMakePolar,       .min = 2, .max = 2, },
    .{ .name = "make-rectangular", .func = pMakeRectangular, .min = 2, .max = 2, },
    .{ .name = "null?",            .func = pNullPred,        .min = 1, .max = 1, },
    .{ .name = "number?",          .func = pNumPred,         .min = 1, .max = 1, },
    .{ .name = "pair?",            .func = pPairPred,        .min = 1, .max = 1, },
    .{ .name = "procedure?",       .func = pProcPred,        .min = 1, .max = 1, },
    .{ .name = "rational?",        .func = pRatPred,         .min = 1, .max = 1, },
    .{ .name = "real?",            .func = pRealPred,        .min = 1, .max = 1, },
    .{ .name = "reverse",          .func = pReverse,         .min = 1, .max = 1, },
    .{ .name = "string?",          .func = pStrPred,         .min = 1, .max = 1, },
    .{ .name = "string-length",    .func = pStrLen,          .min = 1, .max = 1, },
    .{ .name = "string-ref",       .func = pStrRef,          .min = 2, .max = 2, },
    .{ .name = "symbol?",          .func = pSymbPred,        .min = 1, .max = 1, },
    .{ .name = "vector?",          .func = pVecPred,         .min = 1, .max = 1, },
    .{ .name = "zero?",            .func = pZeroPred,        .min = 1, .max = 1, },
    .{ .name = "+",                .func = pPlus,            .min = 0, .max = unlimited, },
    .{ .name = "-",                .func = pMinus,           .min = 1, .max = unlimited, },
    .{ .name = "*",                .func = pTimes,           .min = 0, .max = unlimited, },
    .{ .name = "/",                .func = pDiv,             .min = 1, .max = unlimited, },
    .{ .name = "<",                .func = pLess,            .min = 1, .max = unlimited, },
    .{ .name = "<=",               .func = pLessEq,          .min = 1, .max = unlimited, },
    .{ .name = "=",                .func = pEqual,           .min = 1, .max = unlimited, },
    .{ .name = ">",                .func = pGrt,             .min = 1, .max = unlimited, },
    .{ .name = ">=",               .func = pGrtEq,           .min = 1, .max = unlimited, },
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

// -- Booleans --------------------------------------------
// (boolean? <exp>)
fn pBoolPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .boolean) sxTrue else sxFalse;
}


// -- Characters ------------------------------------------
fn pCharPred(args: []Sexpr) EvalError!Sexpr {
    // (char? <exp>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .char) sxTrue else sxFalse;
}

fn pCharToInt(args: []Sexpr) EvalError!Sexpr {
    // (char->integer <char>)
    const val = args[0];
    const tag = @intToEnum(PtrTag, val & TagMask);
    if (tag != .char)
        return EvalError.ExpectedCharacter;
    return makeInteger(val >> TagShift);
}

fn pIntToChar(args: []Sexpr) EvalError!Sexpr {
    // (integer->char <code>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    if (tag != .small_int and tag != .integer)
        return EvalError.ExpectedInteger;
    const code = getAsInt(args[0]);
    if (code > 0x10FFFF or code < 0)
        return EvalError.InvalidUnicodeValue;
    return makeChar(code);
}


// -- Lists ------------------------------------------------
fn pListPred(args: []Sexpr) EvalError!Sexpr {
    // (list? <exp>)
    var arg = args[0];

    while (arg != nil) {
        const tag = @intToEnum(PtrTag, arg & TagMask);
        if (tag != .pair)
            return sxFalse;
        arg = cell.cellArray[arg >> TagShift].dot.cdr;
    }

    return sxTrue;
}

fn pPairPred(args: []Sexpr) EvalError!Sexpr {
    // (pair? <exp>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .pair) sxTrue else sxFalse;
}

fn pNullPred(args: []Sexpr) EvalError!Sexpr {
    // (null? <exp>)
    return if (args[0] == nil) sxTrue else sxFalse;
}

fn pCar(args: []Sexpr) EvalError!Sexpr {
    // (car <pair>)
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp >> TagShift].dot.car;
}

fn pCdr(args: []Sexpr) EvalError!Sexpr {
    // (cdr <pair>)
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp >> TagShift].dot.cdr;
}

fn pCons(args: []Sexpr) EvalError!Sexpr {
    // (cons <car> <cdr>)
    return makePair(args[0], args[1]);
}

fn pLength(args: []Sexpr) EvalError!Sexpr {
    // (length <list>)
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
    // (list <exp>...)
    var list = nil;
    var i = args.len;

    while (i > 0) : (i -= 1) {
        list = try makePair(args[i-1], list);
    }

    return list;
}

fn pReverse(args: []Sexpr) EvalError!Sexpr {
    // (reverse <list>)
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


// -- Numbers ---------------------------------------------
fn pNumPred(args: []Sexpr) EvalError!Sexpr {
    // (number? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.complex);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
}

fn pIntPred(args: []Sexpr) EvalError!Sexpr {
    // (integer? <exp>)
    return if (isInteger(args[0])) sxTrue else sxFalse;
 }

fn pRatPred(args: []Sexpr) EvalError!Sexpr {
    // (rational? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.float);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

fn pRealPred(args: []Sexpr) EvalError!Sexpr {
    // (real? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.float);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

fn pComplexPred(args: []Sexpr) EvalError!Sexpr {
    // (complex? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.complex);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

fn pExactPred(args: []Sexpr) EvalError!Sexpr {
    // (exact? <exp>)
    const exp = args[0];
    const ind = exp >> TagShift;
    const tag = @intToEnum(PtrTag, exp & TagMask);
    var exact: bool = false;

   switch (tag) {
        .small_int, .integer, .rational => exact = true,
        .float => exact = false,
        .polar => exact = isExactReal(cell.cellArray[ind].pol.mag) and
                          isExactReal(cell.cellArray[ind].pol.ang),
        .complex => exact = isExactReal(cell.cellArray[ind].cmp.re) and
                            isExactReal(cell.cellArray[ind].cmp.im),
        else => return EvalError.ExpectedNumber,
   }

    return if (exact) sxTrue else sxFalse;
 }

fn pInexactPred(args: []Sexpr) EvalError!Sexpr {
    // (inexact? <exp>)
    const exact = try pExactPred(args);
    return if (exact == sxTrue) sxFalse else sxTrue;
 }

/// Tests if a number represented as an Sexpr is an integer.
pub fn isInteger(num: Sexpr) bool {
    const tag = @intToEnum(PtrTag, num & TagMask);

   return switch (tag) {
        .small_int, .integer => true,
        .rational => false,
        .float => blk: {
            const ind = num >> TagShift;
            const fnum: f64 = cell.cellArray[ind].flt;
            const inum: i64 = @floatToInt(i64, fnum);
            break :blk if (@intToFloat(f64, inum) == fnum)
                            true
                       else
                            false;
        },
        .polar => blk: {
            const ind = num >> TagShift;
            const mag = cell.cellArray[ind].pol.mag;
            const ang = cell.cellArray[ind].pol.ang;
            // To be honest we should also test if the angle is pi,
            // which would correspond to a negative integer, but
            // the innacuracies introduced by the floating-point
            // representation of the angle in radians make this quite
            // unreliable. Perhaps there should be an epsilon for
            // these situations.
            break :blk if (isInteger(mag) and isZeroReal(ang))
                            true
                       else
                            false;
        },
        .complex => blk: {
            const ind = num >> TagShift;
            const re = cell.cellArray[ind].cmp.re;
            const im = cell.cellArray[ind].cmp.im;
            break :blk if (isInteger(re) and isZeroReal(im))
                            true
                       else
                            false;
        },
        else => unreachable,
   };
}

/// Tests if a real number represented as an Sexpr is exact.
/// Must not be called with complex types.
pub fn isExactReal(num: Sexpr) bool {
    const tag = @intToEnum(PtrTag, num & TagMask);

   return switch (tag) {
        .small_int, .integer, .rational => true,
        .float => false,
        else => unreachable,
   };
}

/// Tests if a real number represented as an Sexpr is zero.
/// Must not be called with complex types.
pub fn isZeroReal(num: Sexpr) bool {
    const ind = num >> TagShift;
    const tag = @intToEnum(PtrTag, num & TagMask);

   return switch (tag) {
        .small_int => ind == 0,
        .integer => cell.cellArray[ind].int == 0,
        .rational => cell.cellArray[ind].rat.num == 0,
        .float => cell.cellArray[ind].flt == 0.0,
        else => unreachable,
   };
}

fn pMakePolar(args: []Sexpr) EvalError!Sexpr {
    // (make-polar mag ang)
    return makePolar(args[0], args[1]);
}

fn pMakeRectangular(args: []Sexpr) EvalError!Sexpr {
    // (make-rectangular re im)
    return makeComplex(args[0], args[1]);
}

fn pZeroPred(args: []Sexpr) EvalError!Sexpr {
    // (zero? <exp>)
    const exp = args[0];
    const ind = exp >> TagShift;
    const tag = @intToEnum(PtrTag, exp & TagMask);
    const zero: bool = switch (tag) {
        .small_int => ind == 0,
        .integer => cell.cellArray[ind].int == 0,
        .rational => cell.cellArray[ind].rat.num == 0,
        .float => cell.cellArray[ind].flt == 0.0,
        .polar => isZeroReal(cell.cellArray[ind].pol.mag),
        .complex => isZeroReal(cell.cellArray[ind].cmp.re) 
                and isZeroReal(cell.cellArray[ind].cmp.re),
        else => return EvalError.ExpectedNumber,
    };
    return  if (zero) sxTrue else sxFalse;
}

// Determine the highest number type in a list of arguments
//   integer < rational < real < complex
fn maxNumType(args: []Sexpr) EvalError!PtrTag {
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.complex);
    var maxtype: u32 = @enumToInt(PtrTag.integer);
    for (args) |arg| {
        const tag = arg & TagMask;
        if (tag < tagLo or tag > tagHi)
            return EvalError.ExpectedNumber;
        if (tag > maxtype)
            maxtype = tag;
    }

    return @intToEnum(PtrTag, maxtype);
}

pub fn getAsInt(num: Sexpr) i64 {
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { return @as(i64, @bitCast(TaggedInt, num)) >> TagShift; },
        .integer =>   { return cell.cellArray[num >> TagShift].int; },
        else => { print("Not implemented yet\n", .{}); return 0;},
    }
}

pub fn getAsFloat(num: Sexpr) f64 {
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { return @intToFloat(f64, @as(i64, @bitCast(TaggedInt, num)) >> TagShift); },
        .integer =>   { return @intToFloat(f64, cell.cellArray[num >> TagShift].int); },
        .float =>     { return cell.cellArray[num >> TagShift].flt; },
        else => { print("Not implemented yet\n", .{}); return 0.0;},
    }
}

/// Return the sign of a real number (-1, 0, 1).
/// For all other types, return 0.
pub fn getSign(num: Sexpr) i64 {
    const exp = num >> TagShift;
    var int: i64 = undefined;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int, .integer => {
            int = getAsInt(num);
        },
        .float => {
            const flt = cell.cellArray[exp].flt;
            if (flt < 0) return -1;
            if (flt > 0) return  1;
            return 0;
        },
        .rational => {
            int = getAsInt(cell.cellArray[exp].rat.num);
        },
        else => return 0,
    }
    if (int < 0) return -1;
    if (int > 0) return  1;
    return 0;
}

fn pPlus(args: []Sexpr) EvalError!Sexpr {
    // (+ <num>...)
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
            else => { print("Not implemented yet\n", .{}); return 0; },
        }
    }
    return makeInteger(0);
}

fn pMinus(args: []Sexpr) EvalError!Sexpr {
    // (- <num>...)
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
        else => { print("Not implemented yet\n", .{}); return 0; },
    }
    return 0;
}

fn pTimes(args: []Sexpr) EvalError!Sexpr {
    // (* <num>...)
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
            else => { print("Not implemented yet\n", .{}); return 0; },
        }
    }
    return makeInteger(1);
}

fn pDiv(args: []Sexpr) EvalError!Sexpr {
    // (/ <num>...)
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
        else => { print("Not implemented yet\n", .{}); return 0; },
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
        else => { print("Not implemented yet\n", .{}); return 0; },
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
        else => { print("Not implemented yet\n", .{}); return 0; },
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
        else => { print("Not implemented yet\n", .{}); return 0; },
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
        else => { print("Not implemented yet\n", .{}); return 0; },
    }
    unreachable;
}


// -- Procedures ------------------------------------------
fn pProcPred(args: []Sexpr) EvalError!Sexpr {
    // (procedure? <exp>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .procedure or tag == .primitive) sxTrue else sxFalse;
}


// -- Strings ---------------------------------------------
fn pStrPred(args: []Sexpr) EvalError!Sexpr {
    // (string? <exp>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .string) sxTrue else sxFalse;
}

fn pStrLen(args: []Sexpr) EvalError!Sexpr {
    // (string-length <string>)
    const exp = args[0];
    const tag = @intToEnum(PtrTag, exp & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;
    const len = str.stringsTable.items[exp >> TagShift].len;
    return makeInteger(len);
}

fn pStrRef(args: []Sexpr) EvalError!Sexpr {
    // (string-ref <string> <position>)
    const strArg = args[0];
    const posArg = args[1];
    var tag = @intToEnum(PtrTag, strArg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;
    const stg = str.get(strArg >> TagShift);
    tag = @intToEnum(PtrTag, posArg & TagMask);
    if (tag != .small_int and tag != .integer)
        return EvalError.ExpectedInteger;
    const pos = getAsInt(posArg);
    if (pos < 0 or pos >= stg.len)
        return EvalError.InvalidReference;
    return makeChar(stg[@bitCast(usize, pos)]);
}

// -- Symbols ---------------------------------------------
fn pSymbPred(args: []Sexpr) EvalError!Sexpr {
    // (symbol? <exp>)
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .symbol) sxTrue else sxFalse;
}


// -- Vectors ---------------------------------------------
fn pVecPred(args: []Sexpr) EvalError!Sexpr {
    const tag = @intToEnum(PtrTag, args[0] & TagMask);
    return if (tag == .vector) sxTrue else sxFalse;
}
