const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const vec  = @import("vector.zig");
const Sexpr = sexp.Sexpr;
const VectorId = vec.VectorId;
const Environ = eval.Environ;
const EvalError = eval.EvalError;

const allocator = std.heap.page_allocator;

pub const ProcId = u32;

pub var procArray: []Proc = undefined;
var freeProcs: u32 = undefined;

// On a 64-bit machine this struct should be 16 bytes long
pub const Proc = struct {
    env:     *Environ,  // Enclosing environment
    formals: Sexpr,     // Vector of formal parameters
    body:    Sexpr,     // Procedure body (a vector of expressions)

    /// Allocate and initialize memory for `n` procs
    pub fn init(n: u32) !void {
        // One page is typically 4096 bytes = 256 Procs, so
        // a good choice for 'n' would be a multiple of 256
        // since this would completely fill an integer number
        // of pages. Some examples:
        //
        // # of Procs   # of pages   # of bytes
        // ----------   ----------   ----------
        //       1024            4        16 KB
        //       8192           32       128 KB
        //     131072          512         2 MB

        // Allocate n Procs
        procArray = try allocator.alloc(Proc, n);
        // Initialize the free list.
        // Proc zero is not used because index 0 means end of list.
        freeProcs = 1; // First free proc
        var i: ProcId = 1;
        const end: ProcId = n - 1;
        // Chain free procs using the body ptr
        while (i < end) : (i += 1) {
            procArray[i].body = i + 1;
        }
        // Last proc marks the end of the free list
        procArray[end].body = 0;
    }

    pub fn deinit() void {
        allocator.free(procArray);
    }

    pub fn alloc() !ProcId {
        if (freeProcs == 0) // TODO: GC
            return EvalError.OutOfMemory;
        const ptr = freeProcs;
        freeProcs = procArray[ptr].body;
        return ptr;
    }

    pub fn free(ptr: ProcId) void {
        procArray[ptr].body = freeProcs;
        freeProcs = ptr;
    }

    pub fn freeCount() u32 {
        // Count # of free procs
        var n: u32 = 0;
        var i = freeProcs;
        while (i != 0) : (i = procArray[i].body)
            n += 1;
        return n;
    }
};

const expect = @import("std").testing.expect;
test "tesing proc allocation" {
    const print = std.debug.print;
    print("{s}", .{"\n"});

    try expect(@sizeOf(Proc) == 16);

    const total: u32 = 1024;
    const used: u32 = 715;

    try Proc.init(total);
    defer Proc.deinit();

    var n = Proc.freeCount();

    try expect(n == (total - 1)); // Proc zero is not available

    // Allocate some procs
    var i: ProcId = 1;
    while (i <= used) : (i += 1)
        _ = try Proc.alloc();

    n = Proc.freeCount();
    try expect(n == (total - 1 - used));

    // Now free them
    i = 1;
    while (i <= used) : (i += 1)
        Proc.free(i);

    n = Proc.freeCount();
    try expect(n == (total - 1));

    // Allocate all procs
    i = 1;
    while (i < total) : (i += 1)
        _ = try Proc.alloc();

    try expect(freeProcs == 0);

    // Try to allocate one more...
    _ = Proc.alloc() catch |err| {
        try expect(err == EvalError.OutOfMemory);
    };
}
