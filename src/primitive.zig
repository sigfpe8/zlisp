const std = @import("std");
const cel = @import("cell.zig");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const cell = @import("cell.zig");
const iop = @import("inpout.zig");
const nbr = @import("number.zig");
const str = @import("string.zig");
const sym = @import("symbol.zig");
const vec = @import("vector.zig");

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

const pApply = eval.pApply;
const car = eval.car;
const cdr = eval.cdr;

const pCloseInputPort = iop.pCloseInputPort;
const pCloseOutputPort = iop.pCloseOutputPort;
const pCharReadyPred = iop.pCharReadyPred;
const pCurrentInputPort = iop.pCurrentInputPort;
const pCurrentOutputPort = iop.pCurrentOutputPort;
const pDisplay = iop.pDisplay;
const pEofPred = iop.pEofPred;
const pInputPortPred = iop.pInputPortPred;
const pLoad = iop.pLoad;
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
const isExact = nbr.isExact;
const pAngle = nbr.pAngle;
const pDiv = nbr.pDiv;
const pEvenPred = nbr.pEvenPred;
const pNumPred = nbr.pNumPred;
const pImagPart = nbr.pImagPart;
const pIntPred = nbr.pIntPred;
const pRatPred = nbr.pRatPred;
const pRealPred = nbr.pRealPred;
const pComplexPred = nbr.pComplexPred;
const pExactPred = nbr.pExactPred;
const pInexactPred = nbr.pInexactPred;
const pMakePolar = nbr.pMakePolar;
const pMakeRectangular = nbr.pMakeRectangular;
const pOddPred = nbr.pOddPred;
const pPositivePred = nbr.pPositivePred;
const pPlus = nbr.pPlus;
const pMagnitude = nbr.pMagnitude;
const pMinus = nbr.pMinus;
const pNegativePred = nbr.pNegativePred;
const pRealPart = nbr.pRealPart;
const pTimes = nbr.pTimes;
const pLess = nbr.pLess;
const pLessEq = nbr.pLessEq;
const pEqual = nbr.pEqual;
const pGrt = nbr.pGrt;
const pGrtEq = nbr.pGrtEq;
const pZeroPred = nbr.pZeroPred;

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
    .{ .name = "angle",               .func = pAngle,             .min = 1, .max = 1, },
    .{ .name = "apply",               .func = pApply,             .min = 2, .max = unlimited, },
    .{ .name = "assoc",               .func = pAssoc,             .min = 2, .max = 2, },
    .{ .name = "assq",                .func = pAssq,              .min = 2, .max = 2, },
    .{ .name = "assv",                .func = pAssv,              .min = 2, .max = 2, },
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
    .{ .name = "eq?",                 .func = pEqPred,            .min = 2, .max = 2, },
    .{ .name = "eqv?",                .func = pEqvPred,           .min = 2, .max = 2, },
    .{ .name = "equal?",              .func = pEqualPred,         .min = 2, .max = 2, },
    .{ .name = "even?",               .func = pEvenPred,          .min = 1, .max = 1, },
    .{ .name = "exact?",              .func = pExactPred,         .min = 1, .max = 1, },
    .{ .name = "imag-part",           .func = pImagPart,          .min = 1, .max = 1, },
    .{ .name = "inexact?",            .func = pInexactPred,       .min = 1, .max = 1, },
    .{ .name = "input-port?",         .func = pInputPortPred,     .min = 1, .max = 1, },
    .{ .name = "integer?",            .func = pIntPred,           .min = 1, .max = 1, },
    .{ .name = "integer->char",       .func = pIntToChar,         .min = 1, .max = 1, },
    .{ .name = "length",              .func = pLength,            .min = 1, .max = 1, },
    .{ .name = "list",                .func = pList,              .min = 0, .max = unlimited, },
    .{ .name = "list?",               .func = pListPred,          .min = 1, .max = 1, },
    .{ .name = "load",                .func = pLoad,              .min = 1, .max = 1, },
    .{ .name = "magnitude",           .func = pMagnitude,         .min = 1, .max = 1, },
    .{ .name = "make-polar",          .func = pMakePolar,         .min = 2, .max = 2, },
    .{ .name = "make-rectangular",    .func = pMakeRectangular,   .min = 2, .max = 2, },
    .{ .name = "member",              .func = pMember,            .min = 2, .max = 2, },
    .{ .name = "memq",                .func = pMemq,              .min = 2, .max = 2, },
    .{ .name = "memv",                .func = pMemv,              .min = 2, .max = 2, },
    .{ .name = "negative?",           .func = pNegativePred,      .min = 1, .max = 1, },
    .{ .name = "newline",             .func = pNewline,           .min = 0, .max = 1, },
    .{ .name = "not",                 .func = pNot,               .min = 1, .max = 1, },
    .{ .name = "null?",               .func = pNullPred,          .min = 1, .max = 1, },
    .{ .name = "number?",             .func = pNumPred,           .min = 1, .max = 1, },
    .{ .name = "odd?",                .func = pOddPred,           .min = 1, .max = 1, },
    .{ .name = "open-input-file",     .func = pOpenInputFile,     .min = 1, .max = 1, },
    .{ .name = "open-output-file",    .func = pOpenOutputFile,    .min = 1, .max = 1, },
    .{ .name = "output-port?",        .func = pOutputPortPred,    .min = 1, .max = 1, },
    .{ .name = "pair?",               .func = pPairPred,          .min = 1, .max = 1, },
    .{ .name = "peek-char",           .func = pPeekChar,          .min = 0, .max = 1, },
    .{ .name = "positive?",           .func = pPositivePred,      .min = 1, .max = 1, },
    .{ .name = "procedure?",          .func = pProcPred,          .min = 1, .max = 1, },
    .{ .name = "rational?",           .func = pRatPred,           .min = 1, .max = 1, },
    .{ .name = "read",                .func = pRead,              .min = 0, .max = 1, },
    .{ .name = "read-char",           .func = pReadChar,          .min = 0, .max = 1, },
    .{ .name = "real?",               .func = pRealPred,          .min = 1, .max = 1, },
    .{ .name = "real-part",           .func = pRealPart,          .min = 1, .max = 1, },
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

pub fn apply(pid: PrimitId, args: []Sexpr) EvalError!void {
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

    // Call the primitive and push its result value
    const val = try pt.func(args);
    return eval.stackPush(val);
}

pub fn init() !void {
    // Bind each primitive symbol (e.g. "car") to its index in PrimitTable
    for (PrimitTable, 0..) |p, i| {
        // i: PrimitId
        const id: SymbolId = try sym.intern(p.name);
        const exp = makeTaggedPtr(@as(UntaggedPtr, @truncate(i)), .primitive);
        try eval.globalEnv.setVar(id, exp);
    }
}

pub fn getName(id: PrimitId) []const u8 {
    return PrimitTable[id].name;
}

// -- Equivalence predicates ------------------------------
fn pEqPred(args: []Sexpr) EvalError!Sexpr {
    // (eq? <exp1> <exp2>)
    const eql = areEq(args[0], args[1]);
    return if (eql) sxTrue else sxFalse;
}

fn pEqvPred(args: []Sexpr) EvalError!Sexpr {
    // (eqv? <exp1> <exp2>)
    const eql = areEqv(args[0], args[1]);
    return if (eql) sxTrue else sxFalse;
}

fn pEqualPred(args: []Sexpr) EvalError!Sexpr {
    // (equal? <exp1> <exp2>)
    const eql = areEqual(args[0], args[1]);
    return if (eql) sxTrue else sxFalse;
}

/// Returns true if exp1 and exp2 are equal as in eq?
pub inline fn areEq(exp1: Sexpr, exp2: Sexpr) bool {
    return exp1 == exp2;
}

/// Returns true if exp1 and exp2 are equivalent as in eqv?
pub fn areEqv(exp1: Sexpr, exp2: Sexpr) bool {
    if (areEq(exp1, exp2))
        return true;

    if (areNumbers(exp1, exp2)) {
        const exact1 = isExact(exp1);
        const exact2 = isExact(exp2);
        if ((exact1 and exact2) or (!exact1 and !exact2))
            return areEqvNum(exp1,exp2);
    }

    return false;        
}

/// Returns true if exp1 and exp2 are numbers
fn areNumbers(exp1: Sexpr, exp2: Sexpr) bool {
    const tag1 = exp1 & TagMask;
    const tag2 = exp2 & TagMask;
    const tagLo = @intFromEnum(PtrTag.small_int);
    const tagHi = @intFromEnum(PtrTag.complex);
    return (tag1 >= tagLo and tag1 <= tagHi and tag2 >= tagLo and tag2 <= tagHi);
}

/// Returns true if num1 and num2 are equivalent numbers as in eqv?
fn areEqvNum(num1: Sexpr, num2: Sexpr) bool {
    var args = [2]Sexpr { num1, num2 };
    const eql = pEqual(&args) catch unreachable;
    return eql == sxTrue;
}

/// Returns true if exp1 and exp1 are equal as in equal?
fn areEqual(exp1: Sexpr, exp2: Sexpr) bool {
    if (areEqv(exp1, exp2))
        return true;

    var tag1: PtrTag = @enumFromInt(exp1 & TagMask);
    var tag2: PtrTag = @enumFromInt(exp2 & TagMask);

    if (tag1 != tag2)
        return false;

    return switch(tag1) {
        .pair => blkp: {    // Compare two lists
            var list1 = exp1;
            var list2 = exp2;
            while (list1 != nil and list2 != nil) {
                tag1 = @enumFromInt(list1 & TagMask);
                tag2 = @enumFromInt(list2 & TagMask);
                if (tag1 == tag2) {
                    if (tag1 == .pair) {
                        const dot1 = cel.cellArray[list1 >> TagShift].dot;
                        const dot2 = cel.cellArray[list2 >> TagShift].dot;
                        // Check if corresponing elements are equal
                        if (!areEqual(dot1.car, dot2.car))
                            break :blkp false;
                        list1 = dot1.cdr;
                        list2 = dot2.cdr;
                    } else {
                        break :blkp areEqual(list1, list2);
                    }
                } else {
                    break :blkp false;
                }
            }
            // Equal if both lists terminated at the same point
            break :blkp list1 == nil and list2 == nil;
        },
        .vector => blkv: {  // Compare two vectors
            const id1 = exp1 >> TagShift;
            const id2 = exp2 >> TagShift;
            const len = vec.vecArray[id1];
            // Check if vectors have the same length
            if (vec.vecArray[id2] != len)
                return false;

            // Check all corresponding elements
            var i: usize = 1;
            while (i <= len) : (i += 1) {
                if (!areEqual(vec.vecArray[id1+i], vec.vecArray[id2+i]))
                    break :blkv false;
            }
            break :blkv true;
        },
        .string => blks: {  // Compare two strings
            const id1 = exp1 >> TagShift;
            const id2 = exp2 >> TagShift;
            const str1 = str.get(id1);
            const str2 = str.get(id2);
            if (str1.len != str2.len)
                break :blks false;
            break :blks std.mem.eql(u8, str1, str2);
        },
        else => false,
    };
}

// -- assoc, member and friends ---------------------------
/// (assoc <obj> <alist>)
fn pAssoc(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const pair = try car(list);
        const head = try car(pair);
        if (areEqual(obj, head))
            return pair;        
        list = try cdr(list);
    }

    return sxFalse;
}

/// (assq <obj> <alist>)
 fn pAssq(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const pair = try car(list);
        const head = try car(pair);
        if (areEq(obj, head))
            return pair;        
        list = try cdr(list);
    }

    return sxFalse;
}

/// (assv <obj> <alist>)
fn pAssv(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const pair = try car(list);
        const head = try car(pair);
        if (areEqv(obj, head))
            return pair;        
        list = try cdr(list);
    }

    return sxFalse;
}

/// (member <obj> <list>)
fn pMember(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const elem = try car(list);
        if (areEqual(obj, elem))
            return list;        
        list = try cdr(list);
    }

    return sxFalse;
}

/// (memq <obj> <list>)
fn pMemq(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const elem = try car(list);
        if (areEq(obj, elem))
            return list;        
        list = try cdr(list);
    }

    return sxFalse;
}

/// (memv <obj> <list>)
fn pMemv(args: []Sexpr) EvalError!Sexpr {
    const obj = args[0];
    var list = args[1];
    const tag: PtrTag = @enumFromInt(list & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (list != nil) {
        const elem = try car(list);
        if (areEqv(obj, elem))
            return list;        
        list = try cdr(list);
    }

    return sxFalse;
}

// -- Booleans --------------------------------------------
// (boolean? <exp>)
fn pBoolPred(args: []Sexpr) EvalError!Sexpr {
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .boolean) sxTrue else sxFalse;
}

fn pNot(args: []Sexpr) EvalError!Sexpr {
    return if (args[0] == sxFalse) sxTrue else sxFalse;
}

// -- Characters ------------------------------------------
fn pCharPred(args: []Sexpr) EvalError!Sexpr {
    // (char? <exp>)
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .char) sxTrue else sxFalse;
}

fn pCharToInt(args: []Sexpr) EvalError!Sexpr {
    // (char->integer <char>)
    const val = args[0];
    const tag: PtrTag = @enumFromInt(val & TagMask);
    if (tag != .char)
        return EvalError.ExpectedCharacter;
    return makeInteger(val >> TagShift);
}

fn pIntToChar(args: []Sexpr) EvalError!Sexpr {
    // (integer->char <code>)
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
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
        const tag: PtrTag = @enumFromInt(arg & TagMask);
        if (tag != .pair)
            return sxFalse;
        arg = cell.cellArray[arg >> TagShift].dot.cdr;
    }

    return sxTrue;
}

fn pPairPred(args: []Sexpr) EvalError!Sexpr {
    // (pair? <exp>)
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .pair) sxTrue else sxFalse;
}

fn pNullPred(args: []Sexpr) EvalError!Sexpr {
    // (null? <exp>)
    return if (args[0] == nil) sxTrue else sxFalse;
}

fn pCar(args: []Sexpr) EvalError!Sexpr {
    // (car <pair>)
    const exp = args[0];
    const tag: PtrTag = @enumFromInt(exp & TagMask);
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp >> TagShift].dot.car;
}

fn pCdr(args: []Sexpr) EvalError!Sexpr {
    // (cdr <pair>)
    const exp = args[0];
    const tag: PtrTag = @enumFromInt(exp & TagMask);
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
        const tag: PtrTag = @enumFromInt(list & TagMask);
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
        const tag: PtrTag = @enumFromInt(arg & TagMask);
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
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .procedure or tag == .primitive) sxTrue else sxFalse;
}

// -- Strings ---------------------------------------------
fn pStrPred(args: []Sexpr) EvalError!Sexpr {
    // (string? <exp>)
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .string) sxTrue else sxFalse;
}

fn pStrLen(args: []Sexpr) EvalError!Sexpr {
    // (string-length <string>)
    const exp = args[0];
    const tag: PtrTag = @enumFromInt(exp & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;
    const len = str.stringsTable.items[exp >> TagShift].len;
    return makeInteger(len);
}

fn pStrRef(args: []Sexpr) EvalError!Sexpr {
    // (string-ref <string> <position>)
    const strArg = args[0];
    const posArg = args[1];
    var tag: PtrTag = @enumFromInt(strArg & TagMask);
    if (tag != .string)
        return EvalError.ExpectedString;
    const stg = str.get(strArg >> TagShift);
    tag = @enumFromInt(posArg & TagMask);
    if (tag != .small_int and tag != .integer)
        return EvalError.ExpectedInteger;
    const pos = getAsInt(posArg);
    if (pos < 0 or pos >= stg.len)
        return EvalError.InvalidReference;
    return makeChar(stg[@bitCast(pos)]);
}

// -- Symbols ---------------------------------------------
fn pSymbPred(args: []Sexpr) EvalError!Sexpr {
    // (symbol? <exp>)
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .symbol) sxTrue else sxFalse;
}

// -- Vectors ---------------------------------------------
fn pVecPred(args: []Sexpr) EvalError!Sexpr {
    const tag: PtrTag = @enumFromInt(args[0] & TagMask);
    return if (tag == .vector) sxTrue else sxFalse;
}
