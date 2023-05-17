const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const prim = @import("primitive.zig");
const sym = @import("symbol.zig");
const vec = @import("vector.zig");

// Debugging
const print = std.debug.print;
const parser = @import("parser.zig");
const printSexpr = parser.printSexpr;

const Sexpr = sexp.Sexpr;
const PtrTag = sexp.PtrTag;
const TagShift = sexp.TagShift;
const TagMask = sexp.TagMask;
const nil = sexp.nil;
const sxFalse = sexp.sxFalse;
const sxTrue = sexp.sxTrue;
const sxUndef = sexp.sxUndef;
const Environ = eval.Environ;
const SymbolId = sym.SymbolId;
const EvalError = eval.EvalError;
const UntaggedPtr = sexp.UntaggedPtr;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeSpecialPtr = sexp.makeSpecialPtr;
const makeProc = sexp.makeProc;
const makeVector = sexp.makeVector;
const MAXVECSIZE = vec.MAXVECSIZE;
const car = eval.car;
const cdr = eval.cdr;
const cons = eval.cons;
const unlimited = std.math.maxInt(u32);

// Function dispatch
const FunDisp = struct {
    name: []const u8, // Special form name (e.g. "if")
    func: *const fn (*Environ, []Sexpr) EvalError!Sexpr, // Function that implements it (e.g. sfIf)
    min: u32,         // Minimum # of arguments (1)
    max: u32,         // Maximum # of arguments (1)
};

pub const SFormId = u32;

const SFormTable = [_]FunDisp{
    .{ .name = "and",         .func = sfAnd,      .min = 0, .max = unlimited, },
    .{ .name = "begin",       .func = sfBegin,    .min = 2, .max = unlimited, },
    .{ .name = "define",      .func = sfDefine,   .min = 2, .max = 2, },
    .{ .name = "if",          .func = sfIf,       .min = 3, .max = 3, },
    .{ .name = "lambda",      .func = sfLambda,   .min = 2, .max = unlimited, },
    .{ .name = "let",         .func = sfLet,      .min = 2, .max = unlimited, },
    .{ .name = "letrec",      .func = sfLetrec,   .min = 2, .max = unlimited, },
    .{ .name = "let*",        .func = sfLetstar,  .min = 2, .max = unlimited, },
    .{ .name = "or",          .func = sfOr,       .min = 0, .max = unlimited, },
    .{ .name = "quote",       .func = sfQuote,    .min = 1, .max = 1, },
};

pub fn apply(env: *Environ, pid: SFormId, args: []Sexpr) EvalError!Sexpr {
    const nargs = args.len;
    const fd: *const FunDisp = &SFormTable[pid];

    if (fd.min == fd.max and fd.min != nargs) {
        print("'{s}' expected {d} argument{s}, got {d}\n",
            .{ fd.name, fd.min, if (fd.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs < fd.min) {
        print("'{s}' expected at least {d} argument{s}, got {d}\n",
            .{ fd.name, fd.min, if (fd.min == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }
    if (nargs > fd.max) {
        print("'{s}' expected at most {d} argument{s}, got {d}\n",
            .{ fd.name, fd.max, if (fd.max == 1) "" else "s", nargs });
        return EvalError.WrongNumberOfArguments;
    }

    // print("{s}:\n", .{fd.name});
    // for (args) |a, i| {
    //     print("  arg[{d}] = ", .{i});
    //     printSexpr(a, true) catch {};
    //     print("\n", .{});
    // }

    // Call the special form and return its value
    return fd.func(env, args);
}

pub fn init() !void {
    // Bind each special form symbol (e.g. "if") to its index in SFormTable[]
    for (SFormTable) |fd, i| {
        // fd: FunDisp
        // i:  SFormId
        const id: SymbolId = try sym.intern(fd.name);
        const exp = makeSpecialPtr(@truncate(UntaggedPtr, i), .form);

        // print("[{d}] = \"{s}\", exp = {x}\n", .{i, fd.name, exp});
        try eval.globalEnv.setVar(id, exp);
    }
}

pub fn getName(id: SFormId) []const u8 {
    return SFormTable[id].name;
}

// Expects a list of variables (formal parameters)
// Puts them into a vector for easy access
fn getFormals(lst: Sexpr) !Sexpr {
    var ptr = lst;
    var tvec: [MAXVECSIZE]Sexpr = undefined;
    var len: u32 = 0;
    var tag: PtrTag = @intToEnum(PtrTag, ptr & TagMask);
    
    if (tag != .pair)
        return EvalError.ExpectedList;

    while (ptr != nil) {
        if (len == MAXVECSIZE)
            return EvalError.TooManyFormals;
        const vname = try car(ptr);
        tag = @intToEnum(PtrTag, vname & TagMask);
        if (tag != .symbol)
            return EvalError.ExpectedVariable;
        tvec[len] = vname;
        len += 1;
        ptr = try cdr(ptr);
    }

    return makeVector(tvec[0..len]);
}

// Gets a slice of Sexprs, which comprise the body's sequence of expressions
// (lambda (<formals>) <exp>+)
// Puts them into a vector for easy access
// TODO: Internal definitions
fn getBody(lst: []Sexpr) !Sexpr {
    return makeVector(lst[0..lst.len]);
}

fn sfAnd(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (and <exp>*)
    var val = sxTrue;

    for (args) |arg| {
        val = try env.eval(arg);
        if (val == sxFalse)
            break;
    }

    return val;
}

fn sfBegin(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (begin <sequence>)
    const bid  = try getBody(args[0..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return try env.evalBody(body);
}

fn sfDefine(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (define <var> <exp>)
    // Use the same (current) environment to evaluate the
    // expressions and bind the variables.
    const vname = args[0];
    const tag = @intToEnum(PtrTag, vname & TagMask);
    if (tag != .symbol)
        return EvalError.ExpectedSymbol;
    var exp = try env.eval(args[1]);
    try env.setVar(vname >> TagShift, exp);
    return nil;
}

fn sfIf(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (if <test-exp> <then-exp> <else-exp>)
    var tstexp = try env.eval(args[0]);
    var exp: Sexpr = undefined;
    // Anything different from #f is true
    if (tstexp != sxFalse) {
        exp = args[1];
    } else {
        exp = args[2];
    }
    return try env.eval(exp);
}

fn sfLambda(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (lambda <formals> <body>)
    const formals = try getFormals(args[0]);
    const body    = try getBody(args[1..]);
    return makeProc(env, formals, body);
}

fn sfLet(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (let ((<var> <exp>)*) <body>)
    const newenv = try env.newBindings(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return try newenv.evalBody(body);
}

fn sfLetrec(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (letrec ((<var> <exp>)*) <body>)
    const newenv = try env.newBindingsRec(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return try newenv.evalBody(body);
}

fn sfLetstar(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (let* ((<var> <exp>)*) <body>)
    const newenv = try env.newBindingsStar(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return try newenv.evalBody(body);
}

fn sfOr(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (or <exp>*)
    var val = sxFalse;

    for (args) |arg| {
        val = try env.eval(arg);
        if (val != sxFalse)
            break;
    }

    return val;
}

fn sfQuote(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (quote <exp>)
    _ = env;
    return args[0];
}
