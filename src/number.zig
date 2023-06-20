const std = @import("std");
const sxp = @import("sexpr.zig");
const cel = @import("cell.zig");
const EvalError = @import("error.zig").EvalError;

const makeComplex = sxp.makeComplex;
const makeInteger = sxp.makeInteger;
const makeFloat = sxp.makeFloat;
const makePolar = sxp.makePolar;
const makeRational = sxp.makeRational;
const makeTaggedPtr = sxp.makeTaggedPtr;

const PtrTag = sxp.PtrTag;
const Sexpr = sxp.Sexpr;
const sxFalse = sxp.sxFalse;
const sxTrue  = sxp.sxTrue;
const TaggedInt = sxp.TaggedInt;
const TagShift = sxp.TagShift;
const TagMask = sxp.TagMask;

const print = std.debug.print;

/// Number types used for tokenization and math operations.
/// For the representation of numbers as S-expressions see sexpr.zig.
pub const Number = union(enum) {
    pol: Polar,
    cmp: Complex,
};

pub const Polar = struct {
    mag: Real,
    ang: Real,
};

pub const Complex = struct {
    re: Real,
    im: Real,
};

pub const Real = union(enum) {
    int: i64,
    flt: f64,
    rat: Rational,
};

pub const Rational = struct {
    num: i64,
    den: i64,
};

// -- Helper functions ------------------------------------

/// Compares two i64/f64 numbers
/// Returns -1 if r1  < r2
///          0 if r1 == r2
///          1 if r1  > r2
fn cmpNum(comptime T: type, n1: T, n2: T) callconv(.Inline) isize {
    if (n1 < n2)
        return -1;
    if (n1 > n2)
        return 1;
    return 0;
}

/// Converts a Real{} number to its corresponding S-expr
pub fn makeReal(real: Real) !Sexpr {
    var sexpr: Sexpr = undefined;    
    switch (real) {
        .int => |int| { sexpr = try makeInteger(int); },
        .flt => |flt| { sexpr = try makeFloat(flt); },
        .rat => |rat| { sexpr = try makeRational(rat.num,rat.den); },
    }
    return sexpr;
}

/// Tests if a number represented as an Sexpr is an integer
pub fn isInteger(num: Sexpr) bool {
    const tag = @intToEnum(PtrTag, num & TagMask);

   return switch (tag) {
        .small_int, .integer => true,
        .rational => false,
        .float => blk: {
            const ind = num >> TagShift;
            const fnum: f64 = cel.cellArray[ind].flt;
            const inum: i64 = @floatToInt(i64, fnum);
            break :blk if (@intToFloat(f64, inum) == fnum)
                            true
                       else
                            false;
        },
        .polar => blk: {
            const ind = num >> TagShift;
            const mag = cel.cellArray[ind].pol.mag;
            const ang = cel.cellArray[ind].pol.ang;
            // To be honest we should also test if the angle is +-pi,
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
            const re = cel.cellArray[ind].cmp.re;
            const im = cel.cellArray[ind].cmp.im;
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
        .integer => cel.cellArray[ind].int == 0,
        .rational => cel.cellArray[ind].rat.num == 0,
        .float => cel.cellArray[ind].flt == 0.0,
        else => unreachable,
   };
}

// Determine the highest number type in a list of arguments
//   integer < rational < real < polar < complex
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
        .integer =>   { return cel.cellArray[num >> TagShift].int; },
        else => unreachable,
    }
}

pub fn getAsRational(num: Sexpr) Rational {
    var nu: i64 = 0;
    var de: i64 = 1;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { nu = @as(i64, @bitCast(TaggedInt, num)) >> TagShift; },
        .integer =>   { nu = cel.cellArray[num >> TagShift].int; },
        .rational => {
            nu = getAsInt(cel.cellArray[num >> TagShift].rat.num); 
            de = getAsInt(cel.cellArray[num >> TagShift].rat.den); 
        },
        else => unreachable,
    }

    return Rational{ .num = nu, .den = de };
}

pub fn getAsFloat(num: Sexpr) f64 {
    const ind = num >> TagShift;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => { return @intToFloat(f64, @as(i64, @bitCast(TaggedInt, num)) >> TagShift); },
        .integer =>   { return @intToFloat(f64, cel.cellArray[ind].int); },
        .rational =>  {
            const fnu = @intToFloat(f64, getAsInt(cel.cellArray[ind].rat.num)); 
            const fde = @intToFloat(f64, getAsInt(cel.cellArray[ind].rat.den));
            return fnu / fde; 
        },
        .float =>     { return cel.cellArray[ind].flt; },
        else => unreachable,
    }
}

pub fn getAsReal(num: Sexpr) Real {
    const ind = num >> TagShift;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => {
            return Real{ .int = @as(i64, @bitCast(TaggedInt, num)) >> TagShift };
        },
        .integer => {
            return Real{ .int = cel.cellArray[ind].int };
        },
        .rational =>  {
            return Real{ .rat = Rational{ .num = getAsInt(cel.cellArray[ind].rat.num),
                                          .den = getAsInt(cel.cellArray[ind].rat.den) }};
        },
        .float => {
            return Real{ .flt = cel.cellArray[ind].flt };
        },
        else => unreachable,
    }
}

pub fn getAsPolar(num: Sexpr) Polar {
    const ind = num >> TagShift;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => {
            return Polar{ .mag = Real{ .int = @as(i64, @bitCast(TaggedInt, num)) >> TagShift },
                          .ang = Real{ .int = 0 }};
        },
        .integer => {
            return Polar{ .mag = Real{ .int = cel.cellArray[ind].int },
                          .ang = Real{ .int = 0 }};
        },
        .rational =>  {
            return Polar{ .mag = Real{
                                .rat = Rational{ .num = getAsInt(cel.cellArray[ind].rat.num),
                                                 .den = getAsInt(cel.cellArray[ind].rat.den) }},
                          .ang = Real{
                                .rat = Rational{ .num = 0,
                                                 .den = 1 }}};
        },
        .float => {
            return Polar{ .mag = Real{ .flt = cel.cellArray[ind].flt },
                          .ang = Real{ .flt = 0.0 }};
        },
        .polar => {
            return Polar{ .mag = getAsReal(cel.cellArray[ind].pol.mag),
                          .ang = getAsReal(cel.cellArray[ind].pol.ang)};
        },
        .complex => {
            return complexToPolar(getAsFloat(cel.cellArray[ind].cmp.re),
                                  getAsFloat(cel.cellArray[ind].cmp.im));
        },
        else => unreachable,
    }
}

pub fn getAsComplex(num: Sexpr) Complex {
    const ind = num >> TagShift;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int => {
            return Complex{ .re = Real{ .int = @as(i64, @bitCast(TaggedInt, num)) >> TagShift },
                            .im = Real{ .int = 0 }};
        },
        .integer => {
            return Complex{ .re = Real{ .int = cel.cellArray[ind].int },
                            .im = Real{ .int = 0 }};
        },
        .rational =>  {
            return Complex{ .re = Real{
                                .rat = Rational{ .num = getAsInt(cel.cellArray[ind].rat.num),
                                                 .den = getAsInt(cel.cellArray[ind].rat.den) }},
                            .im = Real{
                                .rat = Rational{ .num = 0,
                                                 .den = 1 }}};
        },
        .float => {
            return Complex{ .re = Real{ .flt = cel.cellArray[ind].flt },
                            .im = Real{ .flt = 0.0 }};
        },
        .polar => {
            return polarToComplex(getAsFloat(cel.cellArray[ind].pol.mag),
                                  getAsFloat(cel.cellArray[ind].pol.ang));
        },
        .complex => {
            return Complex{ .re = getAsReal(cel.cellArray[ind].cmp.re),
                            .im = getAsReal(cel.cellArray[ind].cmp.im) };
        },
        else => unreachable,
    }
}

pub fn complexToPolar(re: f64, im: f64) Polar {
    const mag: f64 = std.math.sqrt(re * re + im * im);
    const ang: f64 = std.math.atan2(f64, im, re);
    return Polar{ .mag = Real{ .flt = mag }, .ang = Real{ .flt = ang }};
}

pub fn polarToComplex(mag: f64, ang: f64) Complex {
    const re = mag * std.math.cos(ang);
    const im = mag * std.math.sin(ang);
    return Complex{ .re = Real{ .flt = re }, .im = Real{ .flt = im }};
}

/// Returns the sign of a real number Sexpr:
///    -1 if number < 0
///     0 if number == 0
///     1 if number > 0
/// For all other types, return 0.
pub fn getSign(num: Sexpr) isize {
    const exp = num >> TagShift;
    var int: i64 = undefined;
    switch (@intToEnum(PtrTag, num & TagMask)) {
        .small_int, .integer => {
            int = getAsInt(num);
        },
        .float => {
            const flt = cel.cellArray[exp].flt;
            return cmpNum(f64, flt, 0.0);
        },
        .rational => {
            int = getAsInt(cel.cellArray[exp].rat.num);
        },
        else => return 0,
    }
    return cmpNum(i64, int, 0);
}

/// Adds two Real{} numbers
fn addReal(r1: Real, r2: Real) Real {
    switch (r1) {
        .int => |int1| {
            switch (r2) {
                .int => |int2| { return Real{ .int = int1 + int2 };},
                .flt => |flt2| { return Real{ .flt = @intToFloat(f64, int1) + flt2 }; },
                .rat => |rat2| {
                    return Real{ .rat = addRational(Rational{.num=int1,.den=1}, rat2)};
                }
            }
        },
        .flt => |flt1| {
            switch (r2) {
                .int => |int2| { return Real{ .flt = flt1 + @intToFloat(f64, int2)}; },
                .flt => |flt2| { return Real{ .flt = flt1 + flt2 }; },
                .rat => |rat2| {
                    const flt2 = @intToFloat(f64, rat2.num) / @intToFloat(f64, rat2.den);
                    return Real{ .flt = flt1 + flt2 };
                }
            }
        },
        .rat => |rat1| {
            switch (r2) {
                .int => |int2| {
                    const rat2 = Rational{ .num = int2, .den = 1 };
                    return Real{ .rat = addRational(rat1, rat2)};
                },
                .flt => |flt2| {
                    const flt1 = @intToFloat(f64, rat1.num) / @intToFloat(f64, rat1.den);
                    return Real{ .flt = flt1 + flt2 };
                },
                .rat => |rat2| { return Real{ . rat = addRational(rat1, rat2)}; },
            }
        },
    }
}

/// Subtracts two Real{} numbers
fn subReal(r1: Real, r2: Real) Real {
    switch (r1) {
        .int => |int1| {
            switch (r2) {
                .int => |int2| { return Real{ .int = int1 - int2 };},
                .flt => |flt2| { return Real{ .flt = @intToFloat(f64, int1) - flt2 }; },
                .rat => |rat2| {
                    return Real{ .rat = subRational(Rational{.num=int1,.den=1}, rat2)};
                }
            }
        },
        .flt => |flt1| {
            switch (r2) {
                .int => |int2| { return Real{ .flt = flt1 - @intToFloat(f64, int2)}; },
                .flt => |flt2| { return Real{ .flt = flt1 - flt2 }; },
                .rat => |rat2| {
                    const flt2 = @intToFloat(f64, rat2.num) / @intToFloat(f64, rat2.den);
                    return Real{ .flt = flt1 - flt2 };
                }
            }
        },
        .rat => |rat1| {
            switch (r2) {
                .int => |int2| {
                    const rat2 = Rational{ .num = int2, .den = 1 };
                    return Real{ .rat = subRational(rat1, rat2)};
                },
                .flt => |flt2| {
                    const flt1 = @intToFloat(f64, rat1.num) / @intToFloat(f64, rat1.den);
                    return Real{ .flt = flt1 - flt2 };
                },
                .rat => |rat2| { return Real{ . rat = subRational(rat1, rat2)}; },
            }
        },
    }
}

/// Negates a Real{} number
fn negReal(r: Real) Real {
    switch (r) {
        .int => |int| { return Real{ .int = -int }; },
        .flt => |flt| { return Real{ .flt = -flt }; },
        .rat => |rat| { return Real{ .rat = negRational(rat) }; },
    }
}

/// Multiplies two Real{} numbers
fn mulReal(r1: Real, r2: Real) Real {
    switch (r1) {
        .int => |int1| {
            switch (r2) {
                .int => |int2| { return Real{ .int = int1 * int2 };},
                .flt => |flt2| { return Real{ .flt = @intToFloat(f64, int1) * flt2 }; },
                .rat => |rat2| {
                    return Real{ .rat = mulRational(Rational{.num=int1,.den=1}, rat2)};
                }
            }
        },
        .flt => |flt1| {
            switch (r2) {
                .int => |int2| { return Real{ .flt = flt1 * @intToFloat(f64, int2)}; },
                .flt => |flt2| { return Real{ .flt = flt1 * flt2 }; },
                .rat => |rat2| {
                    const flt2 = @intToFloat(f64, rat2.num) / @intToFloat(f64, rat2.den);
                    return Real{ .flt = flt1 * flt2 };
                }
            }
        },
        .rat => |rat1| {
            switch (r2) {
                .int => |int2| {
                    const rat2 = Rational{ .num = int2, .den = 1 };
                    return Real{ .rat = mulRational(rat1, rat2)};
                },
                .flt => |flt2| {
                    const flt1 = @intToFloat(f64, rat1.num) / @intToFloat(f64, rat1.den);
                    return Real{ .flt = flt1 * flt2 };
                },
                .rat => |rat2| { return Real{ . rat = mulRational(rat1, rat2)}; },
            }
        },
    }
}

/// Divides two Real{} numbers
fn divReal(r1: Real, r2: Real) !Real {
    // Check if dividing by zero
    switch (r2) {
        .int => |int| { 
            if (int == 0)
                return EvalError.DivisionByZero;
         },
        .flt => |flt| {
            if (flt == 0)
                return EvalError.DivisionByZero;
        },
        .rat => |rat| {
            if (rat.num == 0)
                return EvalError.DivisionByZero;
        },
    }
    switch (r1) {
        .int => |int1| {
            switch (r2) {
                .int => |int2| { return Real{ .rat = Rational{ .num = int1, .den = int2} };},
                .flt => |flt2| { return Real{ .flt = @intToFloat(f64, int1) / flt2 }; },
                .rat => |rat2| {
                    return Real{ .rat = try divRational(Rational{.num=int1,.den=1}, rat2)};
                }
            }
        },
        .flt => |flt1| {
            switch (r2) {
                .int => |int2| { return Real{ .flt = flt1 / @intToFloat(f64, int2)}; },
                .flt => |flt2| { return Real{ .flt = flt1 / flt2 }; },
                .rat => |rat2| {
                    const flt2 = @intToFloat(f64, rat2.num) / @intToFloat(f64, rat2.den);
                    return Real{ .flt = flt1 / flt2 };
                }
            }
        },
        .rat => |rat1| {
            switch (r2) {
                .int => |int2| {
                    const rat2 = Rational{ .num = int2, .den = 1 };
                    return Real{ .rat = try divRational(rat1, rat2)};
                },
                .flt => |flt2| {
                    const flt1 = @intToFloat(f64, rat1.num) / @intToFloat(f64, rat1.den);
                    return Real{ .flt = flt1 / flt2 };
                },
                .rat => |rat2| { return Real{ . rat = try divRational(rat1, rat2)}; },
            }
        },
    }
}

/// Compares two Real{} numbers
/// Returns -1 if r1  < r2
///          0 if r1 == r2
///          1 if r1  > r2
fn cmpReal(r1: Real, r2: Real) isize {
    switch (r1) {
        .int => |int1| {
            switch (r2) {
                .int => |int2| { return cmpNum(i64, int1, int2); },
                .flt => |flt2| { return cmpNum(f64, @intToFloat(f64, int1), flt2); },
                .rat => |rat2| { return cmpRational(Rational{.num=int1,.den=1}, rat2); }
            }
        },
        .flt => |flt1| {
            switch (r2) {
                .int => |int2| { return cmpNum(f64, flt1, @intToFloat(f64, int2)); },
                .flt => |flt2| { return cmpNum(f64, flt1, flt2); },
                .rat => |rat2| {
                    const flt2 = @intToFloat(f64, rat2.num) / @intToFloat(f64, rat2.den);
                    return cmpNum(f64, flt1, flt2);
                }
            }
        },
        .rat => |rat1| {
            switch (r2) {
                .int => |int2| {
                    const rat2 = Rational{ .num = int2, .den = 1 };
                    return cmpRational(rat1, rat2);
                },
                .flt => |flt2| {
                    const flt1 = @intToFloat(f64, rat1.num) / @intToFloat(f64, rat1.den);
                    return cmpNum(f64, flt1, flt2);
                },
                .rat => |rat2| { return cmpRational(rat1, rat2); },
            }
        },
    }
}

/// Inverts a Real{} number
fn invReal(r: Real) !Real {
    switch (r) {
        .int => |int| {
            if (int == 0)
                return EvalError.DivisionByZero;
             return Real{ .rat = Rational{ .num = 1, .den = int } };
        },
        .flt => |flt| {
            if (flt == 0.0)
                return EvalError.DivisionByZero;
            return Real{ .flt = 1.0 / flt };
        },
        .rat => |rat| {
            if (rat.num == 0)
                return EvalError.DivisionByZero;
            return Real{ .rat = Rational{ .num = rat.den, .den = rat.num} };
        },
    }
}

/// Adds two Rational{} numbers
fn addRational(r1: Rational, r2: Rational) Rational {
    const nu1 = r1.num;
    const de1 = r1.den;
    const nu2 = r2.num;
    const de2 = r2.den;

    if (de1 == de2)
        return Rational{ .num = nu1 + nu2, .den = de1 };

    return Rational{ .num = nu1*de2 + nu2*de1, .den = de1 * de2 };
}

/// Subtracts two Rational{} numbers
fn subRational(r1: Rational, r2: Rational) Rational {
    const nu1 = r1.num;
    const de1 = r1.den;
    const nu2 = r2.num;
    const de2 = r2.den;

    if (de1 == de2)
        return Rational{ .num = nu1 - nu2, .den = de1 };

    return Rational{ .num = nu1*de2 - nu2*de1, .den = de1 * de2 };
}

/// Negates a Rational{} number
fn negRational(r: Rational) Rational {
    return Rational{ .num = -r.num, .den = r.den };
}

/// Multiplies two Rational{} numbers
fn mulRational(r1: Rational, r2: Rational) Rational {
    var num: i64 = r1.num * r2.num;
    var den: i64 = r1.den * r2.den;

    // Can't get the absolute value of minInt
    if (num != std.math.minInt(i64)) {
        // Reduce to lowest terms (-2/4 --> -1/2)
        const gcd: i64 = @bitCast(i64, std.math.gcd(std.math.absCast(num), std.math.absCast(den)));
        if (gcd != 1) {
            num = @divExact(num, gcd);
            den = @divExact(den, gcd);
        }
    }
    return Rational{ .num = num, .den = den };
}

/// Divides two Rational{} numbers
fn divRational(r1: Rational, r2: Rational) !Rational {
    var num: i64 = r1.num * r2.den;
    var den: i64 = r1.den * r2.num;

    if (den == 0)
        return EvalError.DivisionByZero;

    // Can't get the absolute value of minInt
    if (num != std.math.minInt(i64)) {
        // Reduce to lowest terms (-2/4 --> -1/2)
        const gcd: i64 = @bitCast(i64, std.math.gcd(std.math.absCast(num), std.math.absCast(den)));
        if (gcd != 1) {
            num = @divExact(num, gcd);
            den = @divExact(den, gcd);
        }
    }
    return Rational{ .num = num, .den = den };
}

/// Inverts a Rational{} number
fn invRational(r: Rational) !Rational {
    if (r.num == 0)
        return EvalError.DivisionByZero;
    return Rational{ .num = r.den, .den = r.num };
}

/// Compares two Rational{} numbers
/// Returns -1 if rat1  < rat2
///          0 if rat1 == rat2
///          1 if rat1  > rat2
fn cmpRational(rat1: Rational, rat2: Rational) isize {
    if (rat1.den == rat2.den) {
        if (rat1.num < rat2.num)
            return -1;
        if (rat1.num > rat2.num)
            return 1;
        return 0;
    }

    const num1 = rat1.num * rat2.den;
    const num2 = rat2.num * rat1.den;
    return cmpNum(i64, num1, num2);
}

/// Multiplies two polar numbers
fn mulPolar(p1: Polar, p2: Polar) Polar {
    return Polar{ .mag = mulReal(p1.mag ,p2.mag),
                  .ang = addReal(p1.ang, p2.ang)};
}

/// Divides two polar numbers
fn divPolar(p1: Polar, p2: Polar) !Polar {
    return Polar{ .mag = try divReal(p1.mag ,p2.mag),
                  .ang = subReal(p1.ang, p2.ang)};
}

/// Inverts a polar number
fn invPolar(p: Polar) !Polar {
    return Polar{ .mag = try invReal(p.mag),
                  .ang = negReal(p.ang) };
}

/// Checks if two polar numbers are equal
fn eqlPolar(p1: Polar, p2: Polar) bool {
    // TODO: reduce the angles to the same range.
    return (cmpReal(p1.mag,p2.mag) == 0) and (cmpReal(p1.ang,p2.ang) == 0);
}

/// Adds two Complex{} numbers
fn addComplex(c1: Complex, c2: Complex) Complex {
    return Complex{ .re = addReal(c1.re, c2.re),
                    .im = addReal(c1.im, c2.im)};
}

/// Subtracts two Complex{} numbers
fn subComplex(c1: Complex, c2: Complex) Complex {
    return Complex{ .re = subReal(c1.re, c2.re),
                    .im = subReal(c1.im, c2.im)};
}

/// Negates a Complex{} number
fn negComplex(c: Complex) Complex {
    return Complex{ .re = negReal(c.re),
                    .im = negReal(c.im)};
}

/// Multiplies two complex numbers
fn mulComplex(c1: Complex, c2: Complex) Complex {
    return Complex{ .re = subReal(mulReal(c1.re,c2.re),mulReal(c1.im,c2.im)),
                    .im = addReal(mulReal(c1.re,c2.im),mulReal(c1.im,c2.re))};
}

/// Divides two complex numbers
fn divComplex(c1: Complex, c2: Complex) !Complex {
    const div = addReal(mulReal(c2.re,c2.re),mulReal(c2.im,c2.im));
    const re = try divReal(addReal(mulReal(c1.re,c2.re),mulReal(c1.im,c2.im)),div);
    const im = try divReal(subReal(mulReal(c1.im,c2.re),mulReal(c1.re,c2.im)),div);
    return Complex{ .re = re, .im = im };
}

/// Inverts a Complex{} number
fn invComplex(c: Complex) !Complex {
    const div = addReal(mulReal(c.re,c.re),mulReal(c.im,c.im));
    const re = try divReal(c.re,div);
    const im = negReal(try divReal(c.im,div));
    return Complex{ .re = re, .im = im };
}

/// Checks if two complex numbers are equal
fn eqlComplex(c1: Complex, c2: Complex) bool {
    return (cmpReal(c1.re,c2.re) == 0) and (cmpReal(c1.im,c2.im) == 0);
}

// -- Number primitives -----------------------------------
pub fn pNumPred(args: []Sexpr) EvalError!Sexpr {
    // (number? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.complex);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
}

pub fn pIntPred(args: []Sexpr) EvalError!Sexpr {
    // (integer? <exp>)
    return if (isInteger(args[0])) sxTrue else sxFalse;
 }

pub fn pRatPred(args: []Sexpr) EvalError!Sexpr {
    // (rational? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.float);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

pub fn pRealPred(args: []Sexpr) EvalError!Sexpr {
    // (real? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.float);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

pub fn pComplexPred(args: []Sexpr) EvalError!Sexpr {
    // (complex? <exp>)
    const tag  = args[0] & TagMask;
    const tagLo = @enumToInt(PtrTag.small_int);
    const tagHi = @enumToInt(PtrTag.complex);
    return if (tag >= tagLo and tag <= tagHi) sxTrue else sxFalse;
 }

pub fn pExactPred(args: []Sexpr) EvalError!Sexpr {
    // (exact? <exp>)
    const exp = args[0];
    const ind = exp >> TagShift;
    const tag = @intToEnum(PtrTag, exp & TagMask);
    var exact: bool = false;

   switch (tag) {
        .small_int, .integer, .rational => exact = true,
        .float => exact = false,
        .polar => exact = isExactReal(cel.cellArray[ind].pol.mag) and
                          isExactReal(cel.cellArray[ind].pol.ang),
        .complex => exact = isExactReal(cel.cellArray[ind].cmp.re) and
                            isExactReal(cel.cellArray[ind].cmp.im),
        else => return EvalError.ExpectedNumber,
   }

    return if (exact) sxTrue else sxFalse;
 }

pub fn pInexactPred(args: []Sexpr) EvalError!Sexpr {
    // (inexact? <exp>)
    const exact = try pExactPred(args);
    return if (exact == sxTrue) sxFalse else sxTrue;
 }

pub fn pMakePolar(args: []Sexpr) EvalError!Sexpr {
    // (make-polar mag ang)
    return makePolar(args[0], args[1]);
}

pub fn pMakeRectangular(args: []Sexpr) EvalError!Sexpr {
    // (make-rectangular re im)
    return makeComplex(args[0], args[1]);
}

pub fn pZeroPred(args: []Sexpr) EvalError!Sexpr {
    // (zero? <exp>)
    const exp = args[0];
    const ind = exp >> TagShift;
    const tag = @intToEnum(PtrTag, exp & TagMask);
    const zero: bool = switch (tag) {
        .small_int => ind == 0,
        .integer => cel.cellArray[ind].int == 0,
        .rational => cel.cellArray[ind].rat.num == 0,
        .float => cel.cellArray[ind].flt == 0.0,
        .polar => isZeroReal(cel.cellArray[ind].pol.mag),
        .complex => isZeroReal(cel.cellArray[ind].cmp.re) 
                and isZeroReal(cel.cellArray[ind].cmp.re),
        else => return EvalError.ExpectedNumber,
    };
    return  if (zero) sxTrue else sxFalse;
}

pub fn pPlus(args: []Sexpr) EvalError!Sexpr {
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
            .rational => {
                var result: Rational = getAsRational(args[0]);
                for (args[1..]) |arg| {
                    result = addRational(result, getAsRational(arg));
                }
                return makeRational(result.num, result.den);
            },
            .float => {
                var result: f64 = getAsFloat(args[0]);
                for (args[1..]) |arg| {
                    result = result + getAsFloat(arg);
                }
                return makeFloat(result);
            },
            .polar, .complex => {
                var result: Complex = getAsComplex(args[0]);
                for (args[1..]) |arg| {
                    result = addComplex(result, getAsComplex(arg));
                }
                const re = try makeReal(result.re);
                const im = try makeReal(result.im);
                return try makeComplex(re, im);
            },
            else => unreachable,
        }
    }
    return makeInteger(0);
}

pub fn pMinus(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var result: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                result = subRational(result, getAsRational(arg));
            }
            if (args.len == 1) result = negRational(result);
            return makeRational(result.num, result.den);
        },
        .float => {
            var result: f64 = getAsFloat(args[0]);
            for (args[1..]) |arg| {
                result = result - getAsFloat(arg);
            }
            if (args.len == 1) result = -result;
            return makeFloat(result);
        },
        .polar, .complex => {
            var result: Complex = getAsComplex(args[0]);
            for (args[1..]) |arg| {
                result = subComplex(result, getAsComplex(arg));
            }
            if (args.len == 1) result = negComplex(result);
            const re = try makeReal(result.re);
            const im = try makeReal(result.im);
            return try makeComplex(re, im);
        },
        else => unreachable,
    }
    return 0;
}

pub fn pTimes(args: []Sexpr) EvalError!Sexpr {
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
            .rational => {
                var result: Rational = getAsRational(args[0]);
                for (args[1..]) |arg| {
                    result = mulRational(result, getAsRational(arg));
                }
                return makeRational(result.num, result.den);
            },
            .float => {
                var result: f64 = getAsFloat(args[0]);
                for (args[1..]) |arg| {
                    result = result * getAsFloat(arg);
                }
                return makeFloat(result);
            },
            .polar => {
                var result: Polar = getAsPolar(args[0]);
                for (args[1..]) |arg| {
                    result = mulPolar(result, getAsPolar(arg));
                }
                const mag = try makeReal(result.mag);
                const ang = try makeReal(result.ang);
                return makePolar(mag, ang);            
            },
            .complex => {
                var result: Complex = getAsComplex(args[0]);
                for (args[1..]) |arg| {
                    result = mulComplex(result, getAsComplex(arg));
                }
                const re = try makeReal(result.re);
                const im = try makeReal(result.im);
                return makeComplex(re, im);            
            },
            else => unreachable,
        }
    }
    return makeInteger(1);
}

pub fn pDiv(args: []Sexpr) EvalError!Sexpr {
    // (/ <num>...)
    switch (try maxNumType(args)) {
        .integer, .rational => {
            var result: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const div = getAsRational(arg);
                result = try divRational(result,div);
            }
            if (args.len == 1)
                result = try invRational(result);
            return makeRational(result.num, result.den);
        },
        .float => {
            var result: f64 = getAsFloat(args[0]);
            for (args[1..]) |arg| {
                const div = getAsFloat(arg);
                if (div == 0.0)
                    return EvalError.DivisionByZero;
                result = result / div;
            }
            if (args.len == 1) {    // 1/x
                if (result == 0.0)
                    return EvalError.DivisionByZero;
                result = 1.0 / result;
            }
            return makeFloat(result);
        },
        .polar => {
            var result: Polar = getAsPolar(args[0]);
            for (args[1..]) |arg| {
                result = try divPolar(result, getAsPolar(arg));
            }
            if (args.len == 1)    // 1/x
                result = try invPolar(result);
            const mag = try makeReal(result.mag);
            const ang = try makeReal(result.ang);
            return makePolar(mag, ang);            
        },
        .complex => {
            var result: Complex = getAsComplex(args[0]);
            for (args[1..]) |arg| {
                result = try divComplex(result, getAsComplex(arg));
            }
            if (args.len == 1)    // 1/x
                result = try invComplex(result);
            const re = try makeReal(result.re);
            const im = try makeReal(result.im);
            return makeComplex(re, im);            
        },
        else => unreachable,   
    }
}

pub fn pLess(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var previous: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const next = getAsRational(arg);
                if (cmpRational(previous, next) < 0) {
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
        else => return EvalError.ExpectedReal,   
    }
}

pub fn pLessEq(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var previous: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const next = getAsRational(arg);
                if (cmpRational(previous, next) <= 0) {
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
        else => return EvalError.ExpectedReal,   
    }
}

pub fn pEqual(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var previous: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const next = getAsRational(arg);
                if (cmpRational(previous, next) == 0) {
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
        .polar => {
            var previous: Polar = getAsPolar(args[0]);
            for (args[1..]) |arg| {
                const next = getAsPolar(arg);
                if (eqlPolar(previous, next)) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        .complex => {
            var previous: Complex = getAsComplex(args[0]);
            for (args[1..]) |arg| {
                const next = getAsComplex(arg);
                if (eqlComplex(previous, next)) {
                    previous = next;
                } else
                    return sxFalse;
            }
            return sxTrue;
        },
        else => unreachable,
    }
}

pub fn pGrt(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var previous: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const next = getAsRational(arg);
                if (cmpRational(previous, next) > 0) {
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
        else => return EvalError.ExpectedReal,   
    }
}

pub fn pGrtEq(args: []Sexpr) EvalError!Sexpr {
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
        .rational => {
            var previous: Rational = getAsRational(args[0]);
            for (args[1..]) |arg| {
                const next = getAsRational(arg);
                if (cmpRational(previous, next) >= 0) {
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
        else => return EvalError.ExpectedReal,   
    }
}
