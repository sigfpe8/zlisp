const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const cell = @import("cell.zig");
const iop = @import("inpout.zig");
const nbr = @import("number.zig");
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

const pCloseInputPort = iop.pCloseInputPort;
const pCloseOutputPort = iop.pCloseOutputPort;
const pCharReadyPred = iop.pCharReadyPred;
const pCurrentInputPort = iop.pCurrentInputPort;
const pCurrentOutputPort = iop.pCurrentOutputPort;
const pDisplay = iop.pDisplay;
const pEofPred = iop.pEofPred;
const pInputPortPred = iop.pInputPortPred;
const pNewline = iop.pNewline;
const pOpenInputFile = iop.pOpenInputFile;
const pOpenOutputFile = iop.pOpenOutputFile;
const pOutputPortPred = iop.pOutputPortPred;
const pPeekChar = iop.pPeekChar;
const pRead = iop.pRead;
const pReadChar = iop.pReadChar;
const pWrite = iop.pWrite;
const pWriteChar = iop.pWriteChar;

const getAsInt = nbr.getAsInt;
const getAsFloat = nbr.getAsFloat;
const pNumPred = nbr.pNumPred;
const pIntPred = nbr.pIntPred;
const pRatPred = nbr.pRatPred;
const pRealPred = nbr.pRealPred;
const pComplexPred = nbr.pComplexPred;
const pExactPred = nbr.pExactPred;
const pInexactPred = nbr.pInexactPred;
const pMakePolar = nbr.pMakePolar;
const pMakeRectangular = nbr.pMakeRectangular;
const pZeroPred = nbr.pZeroPred;
const pPlus = nbr.pPlus;
const pMinus = nbr.pMinus;
const pTimes = nbr.pTimes;
const pDiv = nbr.pDiv;
const pLess = nbr.pLess;
const pLessEq = nbr.pLessEq;
const pEqual = nbr.pEqual;
const pGrt = nbr.pGrt;
const pGrtEq = nbr.pGrtEq;

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
    .{ .name = "boolean?",            .func = pBoolPred,          .min = 1, .max = 1, },
    .{ .name = "car",                 .func = pCar,               .min = 1, .max = 1, },
    .{ .name = "cdr",                 .func = pCdr,               .min = 1, .max = 1, },
    .{ .name = "char?",               .func = pCharPred,          .min = 1, .max = 1, },
    .{ .name = "char-ready?",         .func = pCharReadyPred,     .min = 0, .max = 1, },
    .{ .name = "char->integer",       .func = pCharToInt,         .min = 1, .max = 1, },
    .{ .name = "complex?",            .func = pComplexPred,       .min = 1, .max = 1, },
    .{ .name = "cons",                .func = pCons,              .min = 2, .max = 2, },
    .{ .name = "close-input-port",    .func = pCloseInputPort,    .min = 1, .max = 1, },
    .{ .name = "close-output-port",   .func = pCloseOutputPort,   .min = 1, .max = 1, },
    .{ .name = "current-input-port",  .func = pCurrentInputPort,  .min = 0, .max = 0, },
    .{ .name = "current-output-port", .func = pCurrentOutputPort, .min = 0, .max = 0, },
    .{ .name = "display",             .func = pDisplay,           .min = 1, .max = 2, },
    .{ .name = "eof-object?",         .func = pEofPred,           .min = 1, .max = 1, },
    .{ .name = "exact?",              .func = pExactPred,         .min = 1, .max = 1, },
    .{ .name = "inexact?",            .func = pInexactPred,       .min = 1, .max = 1, },
    .{ .name = "input-port?",         .func = pInputPortPred,     .min = 1, .max = 1, },
    .{ .name = "integer?",            .func = pIntPred,           .min = 1, .max = 1, },
    .{ .name = "integer->char",       .func = pIntToChar,         .min = 1, .max = 1, },
    .{ .name = "length",              .func = pLength,            .min = 1, .max = 1, },
    .{ .name = "list",                .func = pList,              .min = 0, .max = unlimited, },
    .{ .name = "list?",               .func = pListPred,          .min = 1, .max = 1, },
    .{ .name = "make-polar",          .func = pMakePolar,         .min = 2, .max = 2, },
    .{ .name = "make-rectangular",    .func = pMakeRectangular,   .min = 2, .max = 2, },
    .{ .name = "newline",             .func = pNewline,           .min = 0, .max = 1, },
    .{ .name = "null?",               .func = pNullPred,          .min = 1, .max = 1, },
    .{ .name = "number?",             .func = pNumPred,           .min = 1, .max = 1, },
    .{ .name = "open-input-file",     .func = pOpenInputFile,     .min = 1, .max = 1, },
    .{ .name = "open-output-file",    .func = pOpenOutputFile,    .min = 1, .max = 1, },
    .{ .name = "output-port?",        .func = pOutputPortPred,    .min = 1, .max = 1, },
    .{ .name = "pair?",               .func = pPairPred,          .min = 1, .max = 1, },
    .{ .name = "peek-char",           .func = pPeekChar,          .min = 0, .max = 1, },
    .{ .name = "procedure?",          .func = pProcPred,          .min = 1, .max = 1, },
    .{ .name = "rational?",           .func = pRatPred,           .min = 1, .max = 1, },
    .{ .name = "read",                .func = pRead,              .min = 0, .max = 1, },
    .{ .name = "read-char",           .func = pReadChar,          .min = 0, .max = 1, },
    .{ .name = "real?",               .func = pRealPred,          .min = 1, .max = 1, },
    .{ .name = "reverse",             .func = pReverse,           .min = 1, .max = 1, },
    .{ .name = "string?",             .func = pStrPred,           .min = 1, .max = 1, },
    .{ .name = "string-length",       .func = pStrLen,            .min = 1, .max = 1, },
    .{ .name = "string-ref",          .func = pStrRef,            .min = 2, .max = 2, },
    .{ .name = "symbol?",             .func = pSymbPred,          .min = 1, .max = 1, },
    .{ .name = "vector?",             .func = pVecPred,           .min = 1, .max = 1, },
    .{ .name = "write",               .func = pWrite,             .min = 1, .max = 2, },
    .{ .name = "write-char",          .func = pWriteChar,         .min = 1, .max = 2, },
    .{ .name = "zero?",               .func = pZeroPred,          .min = 1, .max = 1, },
    .{ .name = "+",                   .func = pPlus,              .min = 0, .max = unlimited, },
    .{ .name = "-",                   .func = pMinus,             .min = 1, .max = unlimited, },
    .{ .name = "*",                   .func = pTimes,             .min = 0, .max = unlimited, },
    .{ .name = "/",                   .func = pDiv,               .min = 1, .max = unlimited, },
    .{ .name = "<",                   .func = pLess,              .min = 1, .max = unlimited, },
    .{ .name = "<=",                  .func = pLessEq,            .min = 1, .max = unlimited, },
    .{ .name = "=",                   .func = pEqual,             .min = 1, .max = unlimited, },
    .{ .name = ">",                   .func = pGrt,               .min = 1, .max = unlimited, },
    .{ .name = ">=",                  .func = pGrtEq,             .min = 1, .max = unlimited, },
};

pub fn apply(pid: PrimitId, args: []Sexpr) EvalError!Sexpr {
    const nargs = args.len;
    const pt: *const FunDisp = &PrimitTable[pid];

    if (pt.min == pt.max and pt.min != nargs) {
        print("'{s}' expected {d} argument{s}, got {d}\n", .{ pt.name, pt.min, if (pt.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs < pt.min) {
        print("'{s}' expected at least {d} argument{s}, got {d}\n", .{ pt.name, pt.min, if (pt.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs > pt.max) {
        print("'{s}' expected at most {d} argument{s}, got {d}\n", .{ pt.name, pt.max, if (pt.max == 1) "" else "s", nargs });
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
        list = try makePair(args[i - 1], list);
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
