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
const print = std.debug.print;
const printSexpr = @import("parser.zig").printSexpr;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeProc = sexp.makeProc;
const makeVector = sexp.makeVector;
const MAXVECSIZE = vec.MAXVECSIZE;

const EvalError = @import("error.zig").EvalError;

// Scheme keywords (special forms)
pub var kwElse:        SymbolId = undefined;
pub var kwQuote:       SymbolId = undefined;
pub var kwQuasiquote:  SymbolId = undefined;
pub var kwUnquote:     SymbolId = undefined;
pub var kwUnquote_spl: SymbolId = undefined;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

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

fn apply(newenv: *Environ, pid: ProcId, args: []Sexpr) !Sexpr {
    const pt: *Proc = &proc.procArray[pid];
    var env: *Environ = newenv; // New environment for the execution
    env.outer = pt.env;         // Chain it to the closure environment
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

    return try env.evalBody(body);
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

    pub fn eval(self: *Self, sexpr: Sexpr) EvalError!Sexpr {
        const tag = @intToEnum(PtrTag, sexpr & TagMask);
        const exp = sexpr >> TagShift;
        switch (tag) {
            .symbol => {
                return self.getVar(exp);
            },
            .pair => {
                const dot = cell.cellArray[exp].dot;
                const cdrtag = @intToEnum(PtrTag, dot.cdr & TagMask);

                // Must be a proper list
                if (cdrtag != .pair)
                    return EvalError.ExpectedList;

                return try self.evalList(dot.car, dot.cdr);
            },
            else => {
                // Auto-quote, i.e. evaluate to itself
                return sexpr;
            },
        }
        unreachable;
    }

    // Evaluate procedure application/special form
    pub fn evalList(self: *Self, pproc: Sexpr, pargs: Sexpr) EvalError!Sexpr {
        var tvec: [MAXVECSIZE]Sexpr = undefined;
        var len: u32 = 0;

        // Evaluate the procedure
        const vproc = try self.eval(pproc);
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

        // Evaluate the arguments and put them in a local array
        var list = pargs;
        while (list != nil) {
            if (len == MAXVECSIZE)
                return EvalError.TooManyArguments;

            var arg = try car(list);
            if (isProc) {
                arg = try self.eval(arg);
            }

            tvec[len] = arg;
            len += 1;
            list = try cdr(list);
        }

        switch (tag) {
            .special => {
                return try spc.apply(self, id, tvec[0..len]);
            },
            .primitive => {
                return try prim.apply(id, tvec[0..len]);
            },
            .procedure => {
                const newenv: *Environ = try allocator.create(Environ);
                newenv.* = .{
                    .outer = null,
                    .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
                };
                return try apply(newenv, id, tvec[0..len]);
            },
            else => unreachable,
        }
        unreachable;
    }

    pub fn setVar(self: *Self, symbol: SymbolId, expr: Sexpr) !void {
        // This might trigger an allocation error
        self.assoc.put(symbol, expr) catch |err| {
            print("  {!}\n", .{err});
            return EvalError.DefineFailed;
        };
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
        print("  Variable '{s}' is undefined\n", .{sym.getName(symbol)});
        return EvalError.UndefinedVariable;
    }

    pub fn evalBody(self: *Self, body: []Sexpr) EvalError!Sexpr {
        var val: Sexpr = nil;
        var i: usize = 0;
    
        // Evaluate all expressions in the sequence
        // Return the value of the last one
        while (i < body.len) : (i += 1) {
            val  = try self.eval(body[i]);
        }

        return val;
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
        exp = try self.eval(exp);
        try bindenv.setVar(vname >> TagShift, exp);
    }

    pub fn evalCondClause(self: *Self, list:Sexpr, last: bool) EvalError!Sexpr {
        // <cond clause> -> (<test> <tail sequence>) |
        //                  (<test> => <exp>) |
        //                  (else <tail sequence>)

        // If the <test> is true, return the value of the last expression in <tail sequence>
        // Otherwise return sxUndef to indicate that the test failed
        var tst = try car(list);
        const tag = @intToEnum(PtrTag, tst & TagMask);
        if (tag == .symbol and (tst >> TagShift) == kwElse) {
            if (!last)
                return EvalError.ElseClauseMustBeLast;
        } else {
            tst = try self.eval(tst);
            if (tst == sxFalse)
                return sxUndef;
        }

        // <tst> is either true or 'else'
        // Evaluate tail sequence
        var lst = try cdr(list);
        var val = tst;
        while (lst != nil) {
            val = try self.eval(try car(lst));
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
