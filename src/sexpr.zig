const std  = @import("std");
const cell = @import("cell.zig");
const evl = @import("eval.zig");
const pro = @import("procedure.zig");
const vec  = @import("vector.zig");

const Cell = cell.Cell;
const Environ = evl.Environ;
const VectorId = vec.VectorId;
const EvalError = @import("error.zig").EvalError;
const isZeroReal = @import("primitive.zig").isZeroReal;

// A symbolic expression (S-expression or Sexpr) is a tagged pointer.
//
// Sexpr (32 bits):
//
//              28               4
//   +------------------------+-----+
//   |      "pointer"         | tag |
//   +------------------------+-----+
//
// Depending on the tag, the "pointer" can mean different things:
//
//     Tag         Pointer
//    -----        ---------------------
//    pair         Index to cellArray[] element that contains a cons cell
//    integer      Index to cellArray[] element that contains an i64
//    float        Index to cellArray[] element that contains an f64
//    small_int    Integer value between -2^27 and 2^27-1
//    symbol       Index to sym_bytes[] (SymbolId)

pub const TaggedPtr = u32;
pub const TaggedInt = i32;
pub const UntaggedPtr = u32;
pub const UntaggedInt = i32;
pub const minSmallInt = std.math.minInt(i28);
pub const maxSmallInt = std.math.maxInt(i28);

pub const TagMask = 0xF;
pub const TagShift = 4;

pub const sxFalse = makeTaggedPtr(0, .boolean);
pub const sxTrue  = makeTaggedPtr(1, .boolean);
pub const sxNullVec = makeTaggedPtr(0, .vector);
pub const sxEnd = makeTaggedPtr(@enumToInt(SpecialTag.end), .special);
pub const sxUndef = makeTaggedPtr(@enumToInt(SpecialTag.undef), .special);
pub const sxVoid = makeTaggedPtr(@enumToInt(SpecialTag.tvoid), .special);
pub const nil = makeTaggedPtr(0, .pair);            // == 0 a.k.a. '()


pub const PtrTag = enum { pair,         // (a . b)
                          small_int,    // 28-bit signed integers (i28)
                          integer,      // 64-bit signed integers (i64)
                          rational,     // (small_int or integer) / (small_int or integer)
                          float,        // f64
                          polar,        // Complex number in polar form (magnitude / angle)
                          complex,      // Complex number in rectangular form (real + imaginary)
                          char,         // Unicode code point
                          boolean,      // #f or #t
                          symbol,       // identifier
                          string,       // "string"
                          vector,       // #(1 2 3...)
                          primitive,    // car, cdr, list, etc
                          procedure,    // lambda
                          special };    // special forms and helper types

pub const SpecialTag = enum { form, tvoid, undef, end };
pub const SpecialTagMask = 0x7;
pub const SpecialTagShift = 3;


pub const Sexpr = TaggedPtr;

pub fn makeTaggedPtr(ptr: UntaggedPtr, tag: PtrTag) TaggedPtr {
    return (ptr << TagShift) | @enumToInt(tag);
}

pub fn makeSpecialPtr(ptr: UntaggedPtr, tag: SpecialTag) TaggedPtr {
    const spc = (ptr << SpecialTagShift) | @enumToInt(tag);
    return makeTaggedPtr(spc, .special);
}

pub fn makePair(pcar: Sexpr, pcdr: Sexpr) !Sexpr {
    const ptr = try Cell.alloc();
    cell.cellArray[ptr].dot.car = pcar;
    cell.cellArray[ptr].dot.cdr = pcdr;
    return makeTaggedPtr(ptr, .pair);
}

pub fn makeInteger(val: i64) !Sexpr {
    if (val >= minSmallInt and val <= maxSmallInt) {
        // Small immediate integer (i28)
        const sval = @truncate(UntaggedInt, val);
        return makeTaggedPtr(@bitCast(UntaggedPtr, sval), .small_int);
    } else {
        // Full i64 integer
        const index = try Cell.alloc();
        cell.cellArray[index].int = val;
        return makeTaggedPtr(index, .integer);
    }
}

pub fn makeRational(num: i64, den: i64) !Sexpr {
    if (den <= 0)
        return EvalError.InvalidDenominator;
    if (num == 0)
        return makeInteger(0);
    
    var rnum = num;
    var rden = den;

    // Can't get the absolute value of minInt
    if (num != std.math.minInt(i64)) {
        // Reduce to lowest terms (-2/4 --> -1/2)
        const gcd: i64 = @bitCast(i64, std.math.gcd(std.math.absCast(num), std.math.absCast(den)));
        if (gcd != 1) {
            rnum = @divExact(num, gcd);
            rden = @divExact(den, gcd);
            // Is this is an integer? (e.g. 4/2 --> 2/1 --> 2)
            if (rden == 1)
                return makeInteger(rnum);
        }
    }

    const ptr = try Cell.alloc();
    cell.cellArray[ptr].rat.num = try makeInteger(rnum);
    cell.cellArray[ptr].rat.den = try makeInteger(rden);
    return makeTaggedPtr(ptr, .rational);
}

pub fn makeFloat(val: f64) !Sexpr {
    const index = try Cell.alloc();
    cell.cellArray[index].flt = val;
    return makeTaggedPtr(index, .float);
}

pub fn makeVector(tvec: []Sexpr) !Sexpr {
    const len: u32 = @truncate(u32, tvec.len);
    if (len == 0)
        return sxNullVec;
    const id = try vec.alloc(len);
    std.mem.copy(Sexpr, vec.vecArray[id+1..id+1+len], tvec[0..len]);
    return makeTaggedPtr(id, .vector);
}

pub fn makeProc(env: *Environ, formals: Sexpr, body: Sexpr) !Sexpr {
    const pid = try pro.Proc.alloc();
    const ptr = &pro.procArray[pid];
    ptr.env     = env;
    ptr.formals = formals;
    ptr.body    = body;
    return makeTaggedPtr(pid, .procedure);
}

pub fn makeChar(code: i64) Sexpr {
    const val = @truncate(UntaggedInt, code);
    return makeTaggedPtr(@bitCast(UntaggedPtr, val), .char);  
}

pub fn makePolar(mag: Sexpr, ang: Sexpr) !Sexpr {
    if (isZeroReal(mag))
        return makeInteger(0);  // Convert 0@ang to 0

    const ptr = try Cell.alloc();
    cell.cellArray[ptr].pol.mag = mag;
    cell.cellArray[ptr].pol.ang = ang;
    return makeTaggedPtr(ptr, .polar);
}

pub fn makeComplex(re: Sexpr, im: Sexpr) !Sexpr {
    if (isZeroReal(re) and isZeroReal(im))
        return makeInteger(0);  // Convert 0+0i to 0

    const ptr = try Cell.alloc();
    cell.cellArray[ptr].cmp.re = re;
    cell.cellArray[ptr].cmp.im = im;
    return makeTaggedPtr(ptr, .complex);
}
