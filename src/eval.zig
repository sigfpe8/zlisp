const std = @import("std");
const sexp = @import("sexpr.zig");
const sym = @import("symbol.zig");
const cell = @import("cell.zig");
const prim = @import("primitive.zig");
const proc = @import("procedure.zig");
const vec = @import("vector.zig");

const Sexpr = sexp.Sexpr;
const PtrTag = sexp.PtrTag;
const SymbolId = sym.SymbolId;
const VectorId = vec.VectorId;
const ProcId = proc.ProcId;
const TagShift = sexp.TagShift;
const TagMask = sexp.TagMask;
const Cell = cell.Cell;
const Proc = proc.Proc;
const nil = sexp.nil;
const sxFalse = sexp.sxFalse;
const print = std.debug.print;
const printSexpr = @import("parser.zig").printSexpr;
const makeTaggedPtr = sexp.makeTaggedPtr;
const makeProc = sexp.makeProc;
const makeVector = sexp.makeVector;
const MAXVECSIZE = vec.MAXVECSIZE;

pub const EvalError = error{
    InvalidSyntax,
    UndefinedVariable,
    ExpectedOneArgument,
    ExpectedTwoArguments,
    ExpectedThreeArguments,
    ExpectedPair,
    ExpectedSymbol,
    ExpectedVariable,
    ExpectedList,
    ExpectedNumber,
    ExpectedProcedure,
    DefineFailed,
    PrintError,
    WrongNumberOfArguments,
    TooManyFormals,
    TooManyArguments,
    TooFewArguments,
    OutOfMemory,
    DivisionByZero,
};

// Scheme keywords (special forms)
pub var kwDefine: SymbolId = undefined;
pub var kwIf:     SymbolId = undefined;
pub var kwLambda: SymbolId = undefined;
pub var kwLet:    SymbolId = undefined;
pub var kwQuote:  SymbolId = undefined;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

pub var globalEnv = Environ{
    .outer = null,
    .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
};

pub fn internKeywords() !void {
    kwDefine = try sym.intern("define");
    kwIf     = try sym.intern("if");
    kwLambda = try sym.intern("lambda");
    kwLet    = try sym.intern("let");
    kwQuote  = try sym.intern("quote");
}

fn cons(pcar: Sexpr, pcdr: Sexpr) !Sexpr {
    const ptr = try Cell.alloc();
    cell.cellArray[ptr].dot.car = pcar;
    cell.cellArray[ptr].dot.cdr = pcdr;
    return makeTaggedPtr(ptr, .pair);
}

fn car(sexpr: Sexpr) !Sexpr {
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    const exp = sexpr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp].dot.car;
}

fn cdr(sexpr: Sexpr) !Sexpr {
    const tag = @intToEnum(PtrTag, sexpr & TagMask);
    const exp = sexpr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp].dot.cdr;
}

fn cadr(sexpr: Sexpr) !Sexpr {
    var ptr = sexpr;
    // cdr
    var tag = @intToEnum(PtrTag, ptr & TagMask);
    var exp = ptr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    ptr = cell.cellArray[exp].dot.cdr;
    // car
    tag = @intToEnum(PtrTag, ptr & TagMask);
    exp = ptr >> TagShift;
    if (tag != .pair)
        return EvalError.ExpectedPair;
    return cell.cellArray[exp].dot.car;
}

fn length(arg: Sexpr) !i64 {
    var list = arg;
    var len: i64 = 0;

    while (list != nil) {
        const tag = @intToEnum(PtrTag, list & TagMask);
        if (tag != .pair)
            return EvalError.ExpectedList;
        len += 1;
        list = cell.cellArray[list >> TagShift].dot.cdr;
    }

    return 
    len;
}

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

pub const Environ = struct {
    const Self = @This();
    // Outer environment, null if global
    outer: ?*Self,
    // Map a variable (symbol) to a value (S-expr)
    assoc: std.AutoHashMap(SymbolId, Sexpr),

    pub fn eval(self: *Self, sexpr: Sexpr) EvalError!Sexpr {
        var tag = @intToEnum(PtrTag, sexpr & TagMask);
        var exp = sexpr >> TagShift;
        switch (tag) {
            .symbol => {
                return self.getVar(exp);
            },
            .pair => {
                var dot = cell.cellArray[exp].dot;
                var cartag = @intToEnum(PtrTag, dot.car & TagMask);
                var cdrtag = @intToEnum(PtrTag, dot.cdr & TagMask);
                var carptr = dot.car >> TagShift;
                var cdrptr = dot.cdr >> TagShift;

                // Must be a proper list
                if (cdrtag != .nil and cdrtag != .pair)
                    return EvalError.InvalidSyntax;

                if (cartag == .symbol) { // Check for special forms
                    if (carptr == kwQuote) { // (quote <exp>)
                        dot = cell.cellArray[cdrptr].dot;
                        if (dot.cdr != nil)
                            return EvalError.ExpectedOneArgument;
                        return dot.car;
                    }

                    if (carptr == kwLambda) {  // (lambda <parms> <body>)
                        const formals = try getFormals(dot.cdr);
                        const body    = try getBody(dot.cdr);
                        return makeProc(self, formals, body);
                    }

                    if (carptr == kwLet) {  // (let ((<var> <exp>)*) <body>)
                        const env  = try self.newBindings(dot.cdr);
                        const bid  = try getBody(dot.cdr) >> TagShift;
                        const blen = vec.vecArray[bid];
                        const body = vec.vecArray[bid+1..bid+1+blen];
                        return try env.evalBody(body);
                    }

                    if (carptr == kwDefine) { // (define <var> <exp>)
                        // const vname = try car(dot.cdr);
                        // tag = @intToEnum(PtrTag, vname & TagMask);
                        // if (tag != .symbol)
                        //     return EvalError.ExpectedSymbol;
                        // exp = try cdr(dot.cdr);
                        // const pcdr = try cdr(exp);
                        // if (pcdr != nil)
                        //     return EvalError.ExpectedTwoArguments;
                        // exp = try car(exp);
                        // exp = try self.eval(exp);
                        // try self.setVar(vname >> TagShift, exp);

                        // Use the same (current) environment to evaluate the
                        // expressions and bind the variables.
                        try self.evalDefine(self, dot.cdr);
                        return nil;
                    }

                    if (carptr == kwIf) {   // (if <test-exp> <then-exp> <else-exp>)
                        // const testExp = try self.eval(try car(dot.cdr));
                        var tstexp: Sexpr = dot.cdr;
                        var thnexp: Sexpr = try cdr(tstexp);
                        var elsexp: Sexpr = try cdr(thnexp);
                        const end:  Sexpr = try cdr(elsexp);
                        if (end != nil)
                            return EvalError.ExpectedThreeArguments;
                        tstexp = try self.eval(try car(tstexp));
                        // Anything different from #f is true
                        if (tstexp != sxFalse) {
                            exp = try car(thnexp);
                        } else {
                            exp = try car(elsexp);
                        }
                        return try self.eval(exp);
                    }
                }
                // Must be function application
                return try self.evalApplication(dot.car, dot.cdr);
            },
            else => {
                // Auto-quote, i.e. evaluate to itself
                return sexpr;
            },
        }
        return 0;
    }

    fn evalDefine(self: *Self, bindenv: *Self, lst: Sexpr) !void {
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

    // Evaluate procedure application
    pub fn evalApplication(self: *Self, pproc: Sexpr, pargs: Sexpr) EvalError!Sexpr {
        var tvec: [MAXVECSIZE]Sexpr = undefined;
        var len: u32 = 0;

        // Evaluate the procedure
        const vproc = try self.eval(pproc);
        const tag = @intToEnum(PtrTag, vproc & TagMask);
        if (tag != .primitive and tag != .procedure) {
            print("Expected primitive or procedure in list head\n", .{});
            return EvalError.ExpectedProcedure;
        }
        const pid = vproc >> TagShift;

        // Evaluate the arguments and put them in a local array
        var list = pargs;
        while (list != nil) {
            if (len == MAXVECSIZE)
                return EvalError.TooManyArguments;

            const arg = try self.eval(try car(list));
            tvec[len] = arg;
            len += 1;
            list = try cdr(list);
        }

        if (tag == .primitive) {
            return try prim.apply(pid, tvec[0..len]);
        } else { // .procedure
            const newenv: *Environ = try allocator.create(Environ);
            newenv.* = .{
                .outer = null,
                .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
            };
            return try apply(newenv, pid, tvec[0..len]);
        }
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
            if (env.assoc.get(symbol)) |value| return value;
            // Otherwise try outer frame
            oenv = env.outer;
        }
        // If not defined in any frame, give an error
        print("  Variable '{s}' is undefined\n", .{sym.getName(symbol)});
        return EvalError.UndefinedVariable;
    }

    // Expects a list of variables (formal parameters)
    // Puts them into a vector for easy access
    fn getFormals(lst: Sexpr) !Sexpr {
        var ptr = try car(lst);
        var tvec: [MAXVECSIZE]Sexpr = undefined;
        var len: u32 = 0;
        var tag: PtrTag = @intToEnum(PtrTag, ptr & TagMask);
        
        if (tag != .pair and tag != .nil)
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

    // Expects a sequence of expressions
    // (lambda (<formals>) <exp>+)
    // Puts them into a vector for easy access
    fn getBody(lst: Sexpr) !Sexpr {
        var ptr = try cdr(lst);
        var tvec: [MAXVECSIZE]Sexpr = undefined;
        var len: u32 = 0;

        while (ptr != nil) {
            if (len == MAXVECSIZE)
                return EvalError.TooManyFormals;
            const exp = try car(ptr);
            tvec[len] = exp;
            len += 1;
            ptr = try cdr(ptr);
        }

        return makeVector(tvec[0..len]);
    }

    // Expects a list of bindings as ((<var> <exp>)*)
    fn newBindings(self: *Self, list: Sexpr) !*Environ {
        var lst = try car(list);
        const newenv: *Environ = try allocator.create(Environ);
        newenv.* = .{
            .outer = null,
            .assoc = std.AutoHashMap(SymbolId, Sexpr).init(allocator),
        };

        while (lst != nil) {
            // Evaluate expressions in the current environment but
            // bind them to variables in the new environment
            try self.evalDefine(newenv, try car(lst));
            lst = try cdr(lst);
        }

        newenv.outer = self;
        return newenv;
    }

    fn applyLet(self: *Self, bindings: Sexpr, body: Sexpr) !Sexpr {
        _ = self;
        _ = bindings;
        _ = body;
        return 0;
    }
};
