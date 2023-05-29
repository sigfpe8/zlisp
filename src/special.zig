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
const sxVoid = sexp.sxVoid;
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
const quoteExpr = eval.quoteExpr;
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
    .{ .name = "and",              .func = sfAnd,        .min = 0, .max = unlimited, },
    .{ .name = "begin",            .func = sfBegin,      .min = 2, .max = unlimited, },
    .{ .name = "cond",             .func = sfCond,       .min = 1, .max = unlimited, },
    .{ .name = "define",           .func = sfDefine,     .min = 2, .max = 2, },
    .{ .name = "if",               .func = sfIf,         .min = 2, .max = 3, },
    .{ .name = "lambda",           .func = sfLambda,     .min = 2, .max = unlimited, },
    .{ .name = "let",              .func = sfLet,        .min = 2, .max = unlimited, },
    .{ .name = "letrec",           .func = sfLetrec,     .min = 2, .max = unlimited, },
    .{ .name = "let*",             .func = sfLetstar,    .min = 2, .max = unlimited, },
    .{ .name = "or",               .func = sfOr,         .min = 0, .max = unlimited, },
    .{ .name = "quasiquote",       .func = sfQuasiquote, .min = 1, .max = 1, },
    .{ .name = "quote",            .func = sfQuote,      .min = 1, .max = 1, },
    .{ .name = "unquote",          .func = sfUnquote,    .min = 1, .max = 1, },
    .{ .name = "unquote-splicing", .func = sfUnquote,    .min = 1, .max = 1, },
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

fn sfCond(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (cond <cond clause>+)
    // (cond <cond clause>* (else <tail sequence>))

    // <cond clause> -> (<test> <tail sequence>)
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const val = try env.evalCondClause(args[i], i == (args.len - 1));
        if (val != sxUndef) // sxUndef indicates that the clause test failed
            return val;     // Found a true clause, return its value
    }
    return sxFalse; // No true clause
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
    return sxVoid;
}

fn sfIf(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (if <test-exp> <then-exp> <else-exp>)
    // (if <test-exp> <then-exp>)
    var tstexp = try env.eval(args[0]);
    var exp: Sexpr = undefined;
    // Anything different from #f is true
    if (tstexp != sxFalse) {
        exp = args[1];
    } else {
        if (args.len == 2)
            // (if <test-exp> <then-exp>)
            return sxVoid;
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

fn sfQuasiquote(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (quasiquote <exp>)
    return QuasiquoteRec(env, args[0], 1);
}

// Append list2 to list1
fn append(list1: Sexpr, list2: Sexpr) EvalError!Sexpr {
    if (list1 == nil)
        return list2;
    if (list2 == nil)
        return list1;

    // list1 and list2 are proper lists
    return try cons(try car(list1), try append(try cdr(list1), list2));
}

// Check if argument is (quasiquote | unquote | unquote-solicing <exp>)

const QqRes = union(enum) {
    regular: Sexpr,     // Result from ` or ,
    splicing: Sexpr,    // Result from ,@
    none: bool,         // No quote/unquote found
};

fn qqRec(env: *Environ, arg: Sexpr, level: usize) EvalError!QqRes {
    var qval = arg;
    var tag = @intToEnum(PtrTag, arg & TagMask);
    var splicing = false;
    var foundQuote = false;

    if (tag == .pair) {
        var qcar: Sexpr = try car(arg);
        tag = @intToEnum(PtrTag, qcar & TagMask);
        qcar = qcar >> TagShift;
        if (tag == .symbol) {
            if (qcar == eval.kwQuasiquote or qcar == eval.kwUnquote or qcar == eval.kwUnquote_spl or qcar == eval.kwQuote) {
                var qcdr = try cdr(arg);
                var qarg = try car(qcdr);
                if (try cdr(qcdr) != nil)
                    return EvalError.QuasiquoteExpectsOnly1Argument;
                foundQuote = true;
                if (qcar == eval.kwQuasiquote) {            // (quasiquote qarg)
                    qval = try QuasiquoteRec(env, qarg, level + 1);
                    if (level > 0)
                        qval = try quoteExpr(eval.kwQuasiquote, qval);
                } else if (qcar == eval.kwUnquote) {        // (unquote qarg)
                    qval = try UnquoteRec(env, qarg, level - 1);
                    if (level > 1)
                        qval = try quoteExpr(eval.kwUnquote, qval);
                } else if (qcar == eval.kwUnquote_spl) {    // (unquote-splicing qarg)
                    qval = try UnquoteRec(env, qarg, level - 1);
                    tag = @intToEnum(PtrTag, qval & TagMask);
                    if (tag != .pair)
                        return EvalError.UnquoteSplicingMustBeList;
                    if (level > 1) {
                        qval = try quoteExpr(eval.kwUnquote_spl, qval);
                    } else
                        splicing = true;
                } else {                                    // (quote qarg)
                    qval = try QuasiquoteRec(env, qarg, level);
                    if (level > 0)
                        qval = try quoteExpr(eval.kwQuote, qval);
                }
            }
        }
    }

    if (!foundQuote)
        return QqRes{ .none = true };

    if (splicing)
        return QqRes{ .splicing = qval };

    return QqRes{ .regular = qval };
}

fn QuasiquoteRec(env: *Environ, arg: Sexpr, level: usize) EvalError!Sexpr {
// print("QQRec(level={d}): ", .{level});
// printSexpr(arg, true) catch {};
// print("\n", .{});

    var tag = @intToEnum(PtrTag, arg & TagMask);
    if (tag != .pair or arg == nil)
        return arg;

    // The quasiquote argument is a non-nil list
    // Check if it is one of ` , ,@
    switch (try qqRec(env, arg, level)) {
        .none => {},
        .regular => |val| return val,
        .splicing => return EvalError.InvalidQuasiquoteElement,
    }

    var lst = arg;
    var res: Sexpr = nil;

    while (lst != nil) : (lst = try cdr(lst)) {
        var qarg = try car(lst);
        switch (try qqRec(env, qarg, level)) {
            .none => {
                qarg = try cons(qarg, nil);
            },
            .regular => |val| {
                qarg = try cons(val, nil);
            },
            .splicing => |val| qarg = val,
        }

        res = try append(res, qarg);
    }
    return res;
}

fn UnquoteRec(env: *Environ, arg: Sexpr, level: usize) EvalError!Sexpr {
    if (level == 0)
        return try env.eval(arg);

    var tag = @intToEnum(PtrTag, arg & TagMask);
    if (tag != .pair or arg == nil)
        return arg;

    // The unquote argument is a non-nil list
    // Check if it is one of ` , ,@
    switch (try qqRec(env, arg, level)) {
        .none => {},
        .regular => |val| return val,
        .splicing => return EvalError.InvalidQuasiquoteElement,
    }

    var lst = arg;
    var res: Sexpr = nil;
    var splicing = false;

    while (lst != nil) : (lst = try cdr(lst)) {
        var qarg = try car(lst);
        switch (try qqRec(env, qarg, level)) {
            .none => {
                qarg = try cons(qarg, nil);
            },
            .regular => |val| {
                qarg = try cons(val, nil);
            },
            .splicing => |val| qarg = val,
        }

        res = try append(res, qarg);
        splicing = false;
    }
    return res;
}

fn sfQuote(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    // (quote <exp>)
    _ = env;
    return args[0];
}

fn sfUnquote(env: *Environ, args: []Sexpr) EvalError!Sexpr {
    _ = env;
    _ = args[0];
    return EvalError.UnquoteOutsideQuasiquote;
}
