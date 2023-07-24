//! This module contains the core of the Scheme evaluator.
//! It is implemented as a set of recursive Zig functions whose entry
//! point is `env.eval(expr)`. The result of `env.eval(expr)` is left on
//! the top of the stack. Since the stack is also used to pass arguments
//! to procedures and special forms, these arguments have to be dropped
//! and replaced by the resulting value. This is centralized in `evalList()`.
//! Each procedure or special form called by `evalList()` leaves its result
//! on top of its arguments. It is evalList() that replaces the arguments
//! with the result.
//! 
//! Before calling (fun arg1 ... argn):
//! 
//!   +----+--... --+----+
//!   |arg1|  ...   |argn|
//!   +----+--... --+----+
//! 
//! Returning from `fun` (which returned `value`):
//! 
//!   +----+--... --+----+-----+
//!   |arg1|  ...   |argn|value|
//!   +----+--... --+----+-----+
//! 
//! Returning from evalList():
//! 
//!   +-----+
//!   |value|
//!   +-----+
//! 
//! In addition to `evalList()`, the function that implements the 
//! primitive `apply`, `pApply()`, also needs to drop the arguments
//! from the stack. However, instead of leaving the result of the
//! function called by `apply` on the stack, `pApply()` needs to pop
//! it and return it to its caller, `prim.apply()`, because this is
//! the protocol used by all functions that implement primitives.

const std = @import("std");
const sexp = @import("sexpr.zig");
const sym = @import("symbol.zig");
const cell = @import("cell.zig");
const prim = @import("primitive.zig");
const proc = @import("procedure.zig");
const spc = @import("special.zig");
const vec = @import("vector.zig");

const Sexpr = sexp.Sexpr;
const PtrTag = sexp.PtrTag;
const SpecialTag = sexp.SpecialTag;
const SymbolId = sym.SymbolId;
const VectorId = vec.VectorId;
const ProcId = proc.ProcId;
const TagShift = sexp.TagShift;
const TagMask = sexp.TagMask;
const SpecialTagShift = sexp.SpecialTagShift;
const SpecialTagMask = sexp.SpecialTagMask;
const Cell = cell.Cell;
const Proc = proc.Proc;
const nil = sexp.nil;
const sxFalse = sexp.sxFalse;
const sxTrue = sexp.sxTrue;
const sxUndef = sexp.sxUndef;
const sxVoid = sexp.sxVoid;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeProc = sexp.makeProc;
const makeVector = sexp.makeVector;
const areEqv = prim.areEqv;
const MAXVECSIZE = vec.MAXVECSIZE;

const EvalError = @import("error.zig").EvalError;
const printSexpr = @import("parser.zig").printSexpr;
const assert = std.debug.assert;
const print = std.debug.print;

// Scheme keywords (special forms)
pub var kwElse:        SymbolId = undefined;
pub var kwQuote:       SymbolId = undefined;
pub var kwQuasiquote:  SymbolId = undefined;
pub var kwUnquote:     SymbolId = undefined;
pub var kwUnquote_spl: SymbolId = undefined;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

/// Evaluation stack
var evalStack: [2048]Sexpr = undefined; // This will probably become an ArrayList[]
var evalSP: usize = 0;           // Where next item will go
const evalStackLen = evalStack.len;

pub fn stackPush(item: Sexpr) callconv(.Inline) EvalError!void {
    if (evalSP == evalStackLen)
        return EvalError.EvalStackOverflow;
    evalStack[evalSP] = item;
    evalSP += 1;
}

pub fn stackPop() callconv(.Inline) Sexpr {
    assert(evalSP > 0);
    evalSP -= 1;
    return evalStack[evalSP];
}

pub fn stackDrop() callconv(.Inline) void {
    assert(evalSP > 0);
    evalSP -= 1;
}

pub var globalEnv = Environ{
    .outer = null,
    .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
};

pub fn internKeywords() !void {
    kwElse        = try sym.intern("else");
    kwQuasiquote  = try sym.intern("quasiquote");
    kwQuote       = try sym.intern("quote");
    kwUnquote     = try sym.intern("unquote");
    kwUnquote_spl = try sym.intern("unquote-splicing");
}

pub fn cons(pcar: Sexpr, pcdr: Sexpr) !Sexpr {
    const ptr = try Cell.alloc();
    cell.cellArray[ptr].dot.car = pcar;
    cell.cellArray[ptr].dot.cdr = pcdr;
    return makeTaggedPtr(ptr, .pair);
}

pub fn car(sexpr: Sexpr) !Sexpr {
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    const exp = sexpr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp].dot.car;
}

pub fn cdr(sexpr: Sexpr) !Sexpr {
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    const exp = sexpr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp].dot.cdr;
}

pub fn quoteExpr(name: SymbolId, expr: Sexpr) !Sexpr {
    // (quote <expr>)
    var ptr1 = try Cell.alloc();
    cell.cellArray[ptr1].dot.car = expr;
    cell.cellArray[ptr1].dot.cdr = nil;
    var ptr2 = try Cell.alloc();
    cell.cellArray[ptr2].dot.car = makeTaggedPtr(name, .symbol);
    cell.cellArray[ptr2].dot.cdr = makeTaggedPtr(ptr1, .pair);
    return makeTaggedPtr(ptr2, .pair);
}

// fn cadr(sexpr: Sexpr) !Sexpr {
//     var ptr = sexpr;
//     // cdr
//     var tag = @intToEnum(PtrTag, ptr & TagMask);
//     var exp = ptr >> TagShift;
//     if (tag != .pair)
//         return EvalError.ExpectedPair;
//     ptr = cell.cellArray[exp].dot.cdr;
//     // car
//     tag = @intToEnum(PtrTag, ptr & TagMask);
//     exp = ptr >> TagShift;
//     if (tag != .pair)
//         return EvalError.ExpectedPair;
//     return cell.cellArray[exp].dot.car;
// }

fn apply(pid: ProcId, args: []Sexpr) !void {
    const pt: *Proc = &proc.procArray[pid];

    const env: *Environ = try allocator.create(Environ);
    env.* = .{
        .outer = pt.env,  // Chain it to the closure environment
        .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
    };

    // Both 'formals' and 'body' are vectors
    const fid = pt.formals >> TagShift;
    const bid = pt.body >> TagShift;
    const flen = vec.vecArray[fid]; // Length of formals slice
    const blen = vec.vecArray[bid]; // Length of body slice
    const formals = vec.vecArray[fid+1..fid+1+flen];
    const body    = vec.vecArray[bid+1..bid+1+blen];

    if (args.len < flen)
        return EvalError.TooFewArguments;
    if (args.len > flen)
        return EvalError.TooManyArguments;

    var i: u32 = 0;
    while (i < flen) : (i += 1) {
        // print("formal[{}]={}\n", .{i, formals[i] >> TagShift});
        try env.setVar(formals[i] >> TagShift, args[i]);
    }

    try env.evalBody(body);
}

/// Implements primitive `apply`
/// (apply <proc> <arg1> ... <argn> <rest-args>)
pub fn pApply(args: []Sexpr) EvalError!Sexpr {
    const arg1 = evalSP;    // Slot of 1st argument/result

    // Get the procedure
    const vproc = args[0];
    const tag = @intToEnum(PtrTag, vproc & TagMask);
    const id = vproc >> TagShift;

    if (tag != .primitive and tag != .procedure) {
        print("'apply' expected primitive or procedure.\n", .{});
        return EvalError.ExpectedProcedure;
    }

    // Push arg1 ... argn (maybe zero items)
    var len: u32 = 0;
    for (args[1..args.len-1]) |arg| {
        try stackPush(arg);
        len += 1;
    }

    // Last argument (<rest-args>) must be a list
    var list = args[args.len - 1];
    if (@intToEnum(PtrTag, list & TagMask) != .pair)
        return EvalError.ExpectedList;

    // Push items from <rest-args> list
    while (list != nil) {
        const arg = try car(list);
        try stackPush(arg);
        len += 1;
        list = try cdr(list);
    }

    switch (tag) {
        .primitive => {
            try prim.apply(id, evalStack[arg1..arg1+len]);
        },
        .procedure => {
            try apply(id, evalStack[arg1..arg1+len]);
        },
        else => unreachable,
    }

    // Primitives must return their value.
    // prim.apply() will push it again.
    const val = stackPop();
    // Drop the arguments
    evalSP = arg1;
    return val;
}

pub fn logError(err: anyerror) void {
    var buffer: [64]u8 = undefined;
    const name = std.fmt.bufPrint(&buffer, "{!}", .{err}) catch unreachable;
    print("\nEvaluation error: {s}\n", .{name[6..]});   // Skip "error."
}

pub const Environ = struct {
    const Self = @This();
    // Outer environment, null if global
    outer: ?*Self,
    // Map a variable (symbol) to a value (S-expr)
    assoc: std.AutoHashMap(SymbolId, Sexpr),

    pub fn eval(self: *Self, sexpr: Sexpr) EvalError!void {
        const tag = @intToEnum(PtrTag, sexpr & TagMask);
        const exp = sexpr >> TagShift;
        switch (tag) {
            .pair => {
                const dot = cell.cellArray[exp].dot;
                const cdrtag = @intToEnum(PtrTag, dot.cdr & TagMask);

                // Must be a proper list
                if (cdrtag != .pair)
                    return EvalError.ExpectedList;

                try self.evalList(dot.car, dot.cdr);
            },
            .symbol => {
                const val = try self.getVar(exp);
                try stackPush(val);
            },
            else => {
                // Auto-quote, i.e. evaluate to itself
                try stackPush(sexpr);
            },
        }
    }

    pub fn evalPop(self: *Self, sexpr: Sexpr) callconv(.Inline) EvalError!Sexpr {
        try self.eval(sexpr);
        return stackPop();
    }

    /// Evaluate procedure application/special form
    pub fn evalList(self: *Self, pproc: Sexpr, pargs: Sexpr) EvalError!void {
        const arg1 = evalSP;    // Slot of 1st argument/result
        var len: u32 = 0;

        // Evaluate the procedure
        const vproc = try self.evalPop(pproc);
        const tag = @intToEnum(PtrTag, vproc & TagMask);
        var id = vproc >> TagShift;
        var isProc: bool = true;

        // Special form?
        if (tag == .special and @intToEnum(SpecialTag, id & SpecialTagMask) == .form) {
            id = id >> SpecialTagShift;
            isProc = false;
        } else if (tag != .primitive and tag != .procedure) {
            print("Expected primitive, procedure or special form in list head\n", .{});
            return EvalError.ExpectedProcedure;
        }

        // For procedures, evaluate the arguments and leave the results on the stack
        // For special forms, push the non-evaluated arguments
        var list = pargs;
        while (list != nil) {
            const arg = try car(list);

            if (isProc) {
                try self.eval(arg); // Leave result on the stack
            } else
                try stackPush(arg);  // Push non-evaluated argument

            len += 1;
            list = try cdr(list);
        }

        switch (tag) {
            .special => {
                try spc.apply(self, id, evalStack[arg1..arg1+len]);
            },
            .primitive => {
                try prim.apply(id, evalStack[arg1..arg1+len]);
            },
            .procedure => {
                try apply(id, evalStack[arg1..arg1+len]);
            },
            else => unreachable,
        }

        // Replace the arguments with the result, unless there were no
        // arguments and the result is already at the top of the stack.
        if (evalSP != arg1 + 1) {
            evalStack[arg1] = evalStack[evalSP-1];
            evalSP = arg1 + 1;
        }
    }

    pub fn setVar(self: *Self, symbol: SymbolId, expr: Sexpr) !void {
        // This might trigger an allocation error
        self.assoc.put(symbol, expr) catch |err| {
            print("  {!}\n", .{err});
            return EvalError.DefineFailed;
        };
    }

    pub fn setBangVar(self: *Self, symbol: SymbolId, expr: Sexpr) !void {
        var oenv: ?*Environ = self;

        // Find frame where variable is defined
        while (oenv) |env| {
            if (env.assoc.contains(symbol)) {   // Found it
                // Try to set! it here but this could trigger an allocation error
                env.assoc.put(symbol, expr) catch |err| {
                    print("  {!}\n", .{err});
                    return EvalError.DefineFailed;
                };
                return; // set! succeeded
            }
            // Try outer frame
            oenv = env.outer;
        }
        // If not defined in any frame, give an error
        print("  Variable '{s}' is not bound\n", .{sym.getName(symbol)});
        return EvalError.UnboundVariable;
    }

    pub fn getVar(self: *Self, symbol: SymbolId) !Sexpr {
        var oenv: ?*Environ = self;

        while (oenv) |env| {
            // If variable is defined, return its value
            if (env.assoc.get(symbol)) |value| {
                if (value != sxUndef)
                    return value;
                // sxUndef can only occur in the pre-binding step of letrec
                // and any reference to his special value is an error.
                return EvalError.LetrecUndefVariable;
            }
            // Otherwise try outer frame
            oenv = env.outer;
        }
        // If not defined in any frame, give an error
        print("  Variable '{s}' is not bound\n", .{sym.getName(symbol)});
        return EvalError.UnboundVariable;
    }

    pub fn evalBody(self: *Self, body: []Sexpr) EvalError!void {
        if (body.len > 0) {
            var i: usize = 0;
            // Evaluate and discard all expressions but the last one
            while (i < body.len - 1) : (i += 1) {
                try self.eval(body[i]);
                stackDrop();
            }
            // Leave the value of the last expression on the stack
            try self.eval(body[i]);
        } else {
            // Nothing to evaluate; push a void value
            try stackPush(sxVoid);
        }
    }

    pub fn evalBind(self: *Self, bindenv: *Self, lst: Sexpr) !void {
        const vname = try car(lst);
        const tag = @intToEnum(PtrTag, vname & TagMask);
        if (tag != .symbol)
            return EvalError.ExpectedSymbol;
        var exp = try cdr(lst);
        const pcdr = try cdr(exp);
        if (pcdr != nil)
            return EvalError.ExpectedTwoArguments;
        exp = try car(exp);
        exp = try self.evalPop(exp);
        try bindenv.setVar(vname >> TagShift, exp);
    }

    pub fn evalCaseClause(self: *Self, key: Sexpr, list:Sexpr, last: bool) EvalError!Sexpr {
        // <case clause> -> ((<datum>+) <tail sequence>) |
        //                  (else <tail sequence>)

        // If <key> is found among the <datum> items, return the value of the last expression in <tail sequence>.
        // Otherwise return sxUndef to indicate that no match was found in this clause.
        var lst = try car(list);
        const tag = @intToEnum(PtrTag, lst & TagMask);
        if (tag == .symbol and (lst >> TagShift) == kwElse) {
            if (!last)
                return EvalError.ElseClauseMustBeLast;
        } else {
            if (tag != .pair)
                return EvalError.ExpectedList;
            // Compare <key> with all the <datum> in the list
            while (lst != nil) {
                const datum = try car(lst);
                if (areEqv(key, datum)) {
                    // Found matching datum
                    break;
                }
                lst = try cdr(lst);
            } else {
                // No matching datum was found
                return sxUndef;
            }
        }

        // Either a mathing <datum> was found or this is an `else` clause
        // Evaluate tail sequence
        lst = try cdr(list);
        var val = sxVoid;
        while (lst != nil) {
            val = try self.evalPop(try car(lst));
            lst = try cdr(lst);
        }

        return val;
    }

    pub fn evalCondClause(self: *Self, list:Sexpr, last: bool) EvalError!Sexpr {
        // <cond clause> -> (<test> <tail sequence>) |
        //                  (<test> => <exp>) |
        //                  (else <tail sequence>)

        // If the <test> is true, return the value of the last expression in <tail sequence>.
        // Otherwise return sxUndef to indicate that this clause failed.
        var tst = try car(list);
        const tag = @intToEnum(PtrTag, tst & TagMask);
        if (tag == .symbol and (tst >> TagShift) == kwElse) {
            if (!last)
                return EvalError.ElseClauseMustBeLast;
        } else {
            tst = try self.evalPop(tst);
            if (tst == sxFalse)
                return sxUndef;
        }

        // <tst> is either true or 'else'
        // Evaluate tail sequence
        var lst = try cdr(list);
        var val = tst;
        while (lst != nil) {
            val = try self.evalPop(try car(lst));
            lst = try cdr(lst);
        }

        return val;
    }

    // Expects a list of bindings as ((<var> <exp>)*)
    pub fn newBindings(self: *Self, list: Sexpr) !*Environ {
        var lst = list;
        const newenv: *Environ = try allocator.create(Environ);
        newenv.* = .{
            .outer = self,
            .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
        };

        while (lst != nil) {
            // Evaluate expressions in the current environment but
            // bind them to variables in the new environment
            try self.evalBind(newenv, try car(lst));
            lst = try cdr(lst);
        }

        return newenv;
    }

    // Expects a list of bindings as ((<var> <exp>)*)
    // Similar to newBindings() but uses the new environment
    // to evaluate and bind the variables. Besides, pre-binds
    // each variable with 'undefined' before performing the
    // real bindings. I think this is meaningful only to 
    // compilers because they must refer to the new storage
    // when generating code for any lambdas in the <exp>'s.
    // With interpreters this doesn't matter because the 
    // lambdas are not inspected during this binding, only
    // when they are later executed.
    pub fn newBindingsRec(self: *Self, list: Sexpr) !*Environ {
        var lst = list;
        const newenv: *Environ = try allocator.create(Environ);
        newenv.* = .{
            .outer = self,
            .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
        };

        while (lst != nil) {
            // Pre-bind each variable to 'undefined'
            const bind = try car(lst);
            const vname = try car(bind);
            const tag = @intToEnum(PtrTag, vname & TagMask);
            if (tag != .symbol)
                return EvalError.ExpectedSymbol;
            try newenv.setVar(vname >> TagShift, sxUndef);
            lst = try cdr(lst);
        }

        // Now re-scan the variables and perform the real bindings.
        lst = list;

        while (lst != nil) {
            // Evaluate expressions and make the bindings in the
            // new envrironment. When evaluating the expressions,
            // references to just bound variables can be found in
            // the new environment (as in let*), but References to
            // variables bound to 'undefined' are caught by getVar()
            // and treated as an error. Variables not found are
            // searched for in the outer scope as usual.
            try newenv.evalBind(newenv, try car(lst));
            lst = try cdr(lst);
        }

        return newenv;
    }

    // Expects a list of bindings as ((<var> <exp>)*)
    // Similar to newBindings() but uses the new environment
    // to evaluate and bind the variables.
    pub fn newBindingsStar(self: *Self, list: Sexpr) !*Environ {
        var lst = list;
        const newenv: *Environ = try allocator.create(Environ);
        newenv.* = .{
            .outer = self,
            .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
        };

        while (lst != nil) {
            // Evaluate expressions and make the bindings in the
            // new envrironment. When evaluating the expressions,
            // references to just bound variables can be found in
            // the new environment. Variables not found there are
            // searched for in the outer scope as usual.
            try newenv.evalBind(newenv, try car(lst));
            lst = try cdr(lst);
        }

        return newenv;
    }
};
