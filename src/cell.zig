const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const EvalError = eval.EvalError;

// A LISP cons cell is made of two parts, a `car` and a `cdr`, each of which
// should be wide enough to hold a pointer to another cell. In this implementation
// the cells are allocated from an array and addresed by their indices. So a
// "cell pointer" is just an integer, in this case a u32, a.k.a a CellId. These 32
// bits are divided into a 28-bit pointer and a 4-bit tag. See sexpr.zig for details.
const CellId = sexp.Sexpr;

pub const Pair = packed struct {
    car: CellId,
    cdr: CellId,
};

const allocator = std.heap.page_allocator;

pub var cellArray: []Cell = undefined;
var freeCells: u32 = undefined;

//
pub const Cell = packed union {
    int: i64,
    flt: f64,
    sym: u32,
    str: u32,
    dot: Pair,

    /// Allocate and initialize memory for `n` cells
    pub fn init(n: u32) !void {
        // One page is typically 4096 bytes = 512 Cells, so
        // a good choice for 'n' would be a multiple of 512
        // since this would completely fill an integer number
        // of pages. Some examples:
        //
        // # of Cells   # of pages   # of bytes
        // ----------   ----------   ----------
        //       1024            2         8 KB
        //       8192           16        64 KB
        //     131072          256         1 MB

        // Allocate n Cells
        cellArray = try allocator.alloc(Cell, n);
        // Initialize the free list.
        // Cell zero is not used because index 0 means end of list.
        freeCells = 1; // First free cell
        var i: CellId = 1;
        const end: CellId = n - 1;
        // Chain free cells using the cdr ptr
        while (i < end) : (i += 1) {
            cellArray[i].dot.cdr = i + 1;
        }
        // Last cell marks the end of the free list
        cellArray[end].dot.cdr = 0;
    }

    pub fn deinit() void {
        allocator.free(cellArray);
    }

    pub fn alloc() !CellId {
        if (freeCells == 0) // TODO: GC
            return EvalError.OutOfMemory;
        const ptr = freeCells;
        freeCells = cellArray[ptr].dot.cdr;
        return ptr;
    }

    pub fn free(ptr: CellId) void {
        cellArray[ptr].dot.cdr = freeCells;
        freeCells = ptr;
    }

    pub fn freeCount() u32 {
        // Count # of free cells
        var n: u32 = 0;
        var i = freeCells;
        while (i != 0) : (i = cellArray[i].dot.cdr)
            n += 1;
        return n;
    }
};

const expect = @import("std").testing.expect;
test "tesing cell allocation" {
    const print = std.debug.print;
    print("{s}", .{"\n"});

    try expect(@sizeOf(Cell) == 8);

    const total: u32 = 1024;
    const used: u32 = 715;

    try Cell.init(total);
    defer Cell.deinit();

    var n = Cell.freeCount();

    try expect(n == (total - 1)); // Cell zero is not available

    // Allocate some cells
    var i: CellId = 1;
    while (i <= used) : (i += 1)
        _ = try Cell.alloc();

    n = Cell.freeCount();
    try expect(n == (total - 1 - used));

    // Now free them
    i = 1;
    while (i <= used) : (i += 1)
        Cell.free(i);

    n = Cell.freeCount();
    try expect(n == (total - 1));

    // Allocate all cells
    i = 1;
    while (i < total) : (i += 1)
        _ = try Cell.alloc();

    try expect(freeCells == 0);

    // Try to allocate one more...
    _ = Cell.alloc() catch |err| {
        try expect(err == EvalError.OutOfMemory);
    };
}
