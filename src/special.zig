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
const UntaggedPtr = sexp.UntaggedPtr;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeSpecialPtr = sexp.makeSpecialPtr;
const makeProc = sexp.makeProc;
const makeVector = sexp.makeVector;
const car = eval.car;
const cdr = eval.cdr;
const cons = eval.cons;
const quoteExpr = eval.quoteExpr;
const unlimited = std.math.maxInt(u32);
const EvalError = @import("error.zig").EvalError;
const stackPush = eval.stackPush;

// Function dispatch
const FunDisp = struct {
    name: []const u8, // Special form name (e.g. "if")
    func: *const fn (*Environ, []Sexpr) EvalError!void, // Function that implements it (e.g. sfIf)
    min: u32,         // Minimum # of arguments (2)
    max: u32,         // Maximum # of arguments (3)
};

pub const SFormId = u32;

const SFormTable = [_]FunDisp{
    .{ .name = "and",              .func = sfAnd,        .min = 0, .max = unlimited, },
    .{ .name = "begin",            .func = sfBegin,      .min = 2, .max = unlimited, },
    .{ .name = "case",             .func = sfCase,       .min = 2, .max = unlimited, },
    .{ .name = "cond",             .func = sfCond,       .min = 1, .max = unlimited, },
    .{ .name = "define",           .func = sfDefine,     .min = 2, .max = unlimited, },
    .{ .name = "if",               .func = sfIf,         .min = 2, .max = 3, },
    .{ .name = "lambda",           .func = sfLambda,     .min = 2, .max = unlimited, },
    .{ .name = "let",              .func = sfLet,        .min = 2, .max = unlimited, },
    .{ .name = "letrec",           .func = sfLetrec,     .min = 2, .max = unlimited, },
    .{ .name = "let*",             .func = sfLetstar,    .min = 2, .max = unlimited, },
    .{ .name = "or",               .func = sfOr,         .min = 0, .max = unlimited, },
    .{ .name = "quasiquote",       .func = sfQuasiquote, .min = 1, .max = 1, },
    .{ .name = "quote",            .func = sfQuote,      .min = 1, .max = 1, },
    .{ .name = "set!",             .func = sfSetBang,    .min = 2, .max = 2, },
    .{ .name = "unquote",          .func = sfUnquote,    .min = 1, .max = 1, },
    .{ .name = "unquote-splicing", .func = sfUnquote,    .min = 1, .max = 1, },
};

const Formals = struct {
    fixed: Sexpr,     // Vector of fixed formal parameters
    rest:  Sexpr,     // Variable with list of rest of parameters
};

pub fn apply(env: *Environ, pid: SFormId, args: []Sexpr) EvalError!void {
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

    // Call the special form and leave its result on the stack
    return fd.func(env, args);
}

pub fn init() !void {
    // Bind each special form symbol (e.g. "if") to its index in SFormTable[]
    for (SFormTable, 0..) |fd, i| {
        // fd: FunDisp
        // i:  SFormId
        const id: SymbolId = try sym.intern(fd.name);
        const exp = makeSpecialPtr(@truncate(i), .form);

        // print("[{d}] = \"{s}\", exp = {x}\n", .{i, fd.name, exp});
        try eval.globalEnv.setVar(id, exp);
    }
}

pub fn getName(id: SFormId) []const u8 {
    return SFormTable[id].name;
}

/// Parse <formals> specification
///   <formals> := (<variable>*) |
///                <variable> |
///                (<variable>+ . <variable>)
fn getFormals(arg: Sexpr) !Formals {
    var tag: PtrTag = @enumFromInt(arg & TagMask);
    
    switch (tag) {
        .symbol => {   // x
            return .{ .fixed = sxUndef, .rest = arg, };
        },
        .pair => {     // (x y z ...)
            var len: usize = 0;
            var list = arg;
            var rest = sxUndef;
            const base = eval.stackGetSP();
            defer eval.stackSetSP(base);

            while (list != nil) {
                const vname = try car(list);
                tag = @enumFromInt(vname & TagMask);
                if (tag != .symbol)
                    return EvalError.ExpectedVariable;
                try eval.stackPush(vname);
                len += 1;
                list = try cdr(list);
                tag = @enumFromInt(list & TagMask);
                if (tag != .pair) {     // (x y . z) ?
                    if (tag != .symbol)
                        return EvalError.ExpectedVariable;
                    rest = list;        // vname = z
                    list = nil;         // Stop loop
                }
            }

            const tvec = eval.stackGetSlice(base, len);
            const fixed = try makeVector(tvec[0..len]);
            return .{ .fixed = fixed, .rest = rest, };
        },
        else => return EvalError.ExpectedFormals,
    }

}

// Gets a slice of Sexprs, which comprise the body's sequence of expressions
// (lambda (<formals>) <exp>+)
// Puts them into a vector for easy access
// TODO: Internal definitions
fn getBody(lst: []Sexpr) !Sexpr {
    return makeVector(lst[0..lst.len]);
}

fn sfAnd(env: *Environ, args: []Sexpr) EvalError!void {
    // (and <exp>*)
    var val = sxTrue;

    for (args) |arg| {
        val = try env.evalPop(arg);
        if (val == sxFalse)
            break;
    }

    return stackPush(val);
}

fn sfBegin(env: *Environ, args: []Sexpr) EvalError!void {
    // (begin <sequence>)
    const bid  = try getBody(args[0..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return env.evalBody(body);
}

fn sfCase(env: *Environ, args: []Sexpr) EvalError!void {
    // (case <key> <case clause>+)
    // <case clause> -> ((<datum>+) <tail sequence>) |
    //                  (else <tail sequence>)
    const key = try env.evalPop(args[0]);
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const val = try env.evalCaseClause(key, args[i], i == (args.len - 1));
        if (val != sxUndef)         // sxUndef indicates that a mathing datum was not found in the <case clause>
            return stackPush(val);  // Found a matching datum, return its value
    }
    return stackPush(sxVoid); // No matching clause
}

fn sfCond(env: *Environ, args: []Sexpr) EvalError!void {
    // (cond <cond clause>+)
    // (cond <cond clause>* (else <tail sequence>))

    // <cond clause> -> (<test> <tail sequence>)
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const val = try env.evalCondClause(args[i], i == (args.len - 1));
        if (val != sxUndef)         // sxUndef indicates that the clause test failed
            return stackPush(val);  // Found a true clause, return its value
    }
    return stackPush(sxVoid); // No true clause
}

fn sfDefine(env: *Environ, args: []Sexpr) EvalError!void {
    // Use the same (current) environment to evaluate the
    // expressions and bind the variables.
    var vname = args[0];
    var tag: PtrTag = @enumFromInt(vname & TagMask);
    var exp: Sexpr = undefined;
    switch (tag) {
        .pair => {
            // (define (<variable> <formals>) <body>) |
            // (define (<variable) . <formal>) <body>)
            const formals = try cdr(vname);
            vname = try car(vname);
            tag = @enumFromInt(vname & TagMask);
            if (tag != .symbol)
                return EvalError.ExpectedVariable;
            exp = try defineLambda(env, formals, args[1..]);
        },
        .symbol => {
            // (define <variable> <expression>)
            if (args.len > 2)
                return EvalError.TooManyArguments;
            exp = try env.evalPop(args[1]);
        },
        else => return EvalError.ExpectedVariable,
    }
    try env.setVar(vname >> TagShift, exp);
    return stackPush(sxVoid);
}

fn sfIf(env: *Environ, args: []Sexpr) EvalError!void {
    // (if <test-exp> <then-exp> <else-exp>)
    // (if <test-exp> <then-exp>)
    const tstexp = try env.evalPop(args[0]);
    var exp: Sexpr = undefined;
    // Anything different from #f is true
    if (tstexp != sxFalse) {
        exp = args[1];  // <then-exp>
    } else {
        if (args.len == 2)
            // (if <test-exp> <then-exp>)
            return stackPush(sxVoid);
        exp = args[2];  // <else-exp>
    }
    return env.eval(exp);
}

fn sfLambda(env: *Environ, args: []Sexpr) EvalError!void {
    // (lambda <formals> <body>)
    const formals = try getFormals(args[0]);
    const body    = try getBody(args[1..]);
    const proc    = try makeProc(env, formals.fixed, formals.rest, body);
    return stackPush(proc);
}

fn defineLambda(env: *Environ, arg0: Sexpr, args: []Sexpr) EvalError!Sexpr {
    // (define (<variable> <formals>) <body>) |
    // (define (<variable) . <formal>) <body>)
    const formals = try getFormals(arg0);
    const body    = try getBody(args[0..]);
    const proc    = try makeProc(env, formals.fixed, formals.rest, body);
    return proc;
}

fn sfLet(env: *Environ, args: []Sexpr) EvalError!void {
    // (let ((<var> <exp>)*) <body>)
    const newenv = try env.newBindings(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return newenv.evalBody(body);
}

fn sfLetrec(env: *Environ, args: []Sexpr) EvalError!void {
    // (letrec ((<var> <exp>)*) <body>)
    const newenv = try env.newBindingsRec(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return newenv.evalBody(body);
}

fn sfLetstar(env: *Environ, args: []Sexpr) EvalError!void {
    // (let* ((<var> <exp>)*) <body>)
    const newenv = try env.newBindingsStar(args[0]);
    const bid  = try getBody(args[1..]) >> TagShift;
    const blen = vec.vecArray[bid];
    const body = vec.vecArray[bid+1..bid+1+blen];
    return newenv.evalBody(body);
}

fn sfOr(env: *Environ, args: []Sexpr) EvalError!void {
    // (or <exp>*)
    var val = sxFalse;

    for (args) |arg| {
        val = try env.evalPop(arg);
        if (val != sxFalse)
            break;
    }

    return stackPush(val);
}

fn sfQuasiquote(env: *Environ, args: []Sexpr) EvalError!void {
    // (quasiquote <exp>)
    const val = try QuasiquoteRec(env, args[0], 1);
    return stackPush(val);
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
    var tag: PtrTag = @enumFromInt(arg & TagMask);
    var splicing = false;
    var foundQuote = false;

    if (tag == .pair) {
        var qcar: Sexpr = try car(arg);
        tag = @enumFromInt(qcar & TagMask);
        qcar = qcar >> TagShift;
        if (tag == .symbol) {
            if (qcar == eval.kwQuasiquote or qcar == eval.kwUnquote or qcar == eval.kwUnquote_spl or qcar == eval.kwQuote) {
                const qcdr = try cdr(arg);
                const qarg = try car(qcdr);
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
                    tag = @enumFromInt(qval & TagMask);
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

    const tag: PtrTag = @enumFromInt(arg & TagMask);
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
        return env.evalPop(arg);

    const tag: PtrTag = @enumFromInt(arg & TagMask);
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

fn sfQuote(_: *Environ, args: []Sexpr) EvalError!void {
    // (quote <exp>)
    return stackPush(args[0]);
}

fn sfSetBang(env: *Environ, args: []Sexpr) EvalError!void {
    // (set! <var> <exp>)
    // Variable <var> must already be bound in the current environment
    const vname = args[0];
    const tag: PtrTag = @enumFromInt(vname & TagMask);
    if (tag != .symbol)
        return EvalError.ExpectedSymbol;
    const exp = try env.evalPop(args[1]);
    try env.setBangVar(vname >> TagShift, exp);
    return stackPush(sxVoid);
}

fn sfUnquote(_: *Environ, _: []Sexpr) EvalError!void {
    return EvalError.UnquoteOutsideQuasiquote;
}
