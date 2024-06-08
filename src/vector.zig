const std = @import("std");
const sexp = @import("sexpr.zig");
const eval = @import("eval.zig");
const Sexpr = sexp.Sexpr;
const EvalError = @import("error.zig").EvalError;
const print = std.debug.print;

const allocator = std.heap.page_allocator;

// A vector of length n is implemented as a block of n+1 contiguous
// Sexprs. The first Sexpr contains the vector length (i.e. the size
// of the block not counting the length node itself).
//
// #(100 200 300)
//
//                 +------+
//            -->  |    3 |
//                 +------+
//                 |  100 |
//                 +------+
//                 |  200 |
//                 +------+
//                 |  300 |
//                 +------+
//
// All vectors are allocated from the vecArray[] and the VectorId is
// its offset in this array. The empty vector, #(), is unique and has
// id 0. This means that (eq? #() x) will always be true when x is
// also bound to #().
//
// When vectors are freed they are put in the free list (freeBlks),
// possibly coalescing adjacent free vectors. When in the free list
// the vector still keeps its length in the first node. The second
// node contains the index of the next free block in the free list or
// 0 if it's the end of the list. When the above vector is freed, it
// would look like this:
//
//                 +------+
//  freeBlks: -->  |    3 |
//                 +------+
//                 |  nxt | --> index of next free block in chain
//                 +------+
//                 |  200 |
//                 +------+
//                 |  300 |
//                 +------+

pub const MAXVECSIZE = 256; // Maximum # of nodes in a literal vector #(1 2 ... 256)

pub const VectorId = u32;

pub var vecArray: []Sexpr = undefined;
var freeBlks: u32 = 0;
var zeroBlks: u32 = 0;


/// Allocate and initialize memory for 'n' vector nodes (Sexprs)
pub fn init(n: u32) !void {
    // Allocate n nodes
    vecArray = try allocator.alloc(Sexpr, n);
    logicalInit(n);
}

fn logicalInit(n: u32) void {
    vecArray[0] = 0;        // This is #()
    // Initially there's a single block at 1 of size n - 2
    vecArray[1] = n - 2;    // Size of this block
    vecArray[2] = 0;        // End of free list
    freeBlks = 1;
    zeroBlks = 0;
}

pub fn deinit() void {
    allocator.free(vecArray);
}

pub fn alloc(siz: u32) !VectorId {
    var id = freeBlks;
    var pl: *u32 = &freeBlks;
    if (siz == 0)
        return 0;
    while (id != 0) {
        const rest = vecArray[id] - siz;
        // Is block the exact same size?
        if (rest == 0) {
            break;  // Yes, just use it!
        }
        // If it leaves more than 1 node, chain the rest
        // as a separate free block
        if (rest > 1) {
            const ir = id + 1 + siz;  // Index of rest node
            vecArray[ir] = rest - 1;
            vecArray[ir + 1] = vecArray[id + 1];
            vecArray[id + 1] = ir;
            break;
        }
        // If the remaining part is too small (we need two
        // nodes in order to chain it as a free block) chain
        // it to zeroBlks hoping to coalesce it to a larger
        // block in the future.
        if (rest == 1) {
            const ir = id + 1 + siz;  // Index of zero-length rest node
            insertZeroBlk(ir);
            break;
        }
        // Not enough room in this block; try the next one.
        pl = &vecArray[id + 1];
        id = pl.*;
    } else {
        // TODO: GC
        return EvalError.OutOfMemory;
    }

    // Remove block from the free list
    pl.* = vecArray[id + 1];
    vecArray[id] = siz;
    return id;
}

pub fn free(vid: VectorId) void {
    const id = mergeZeroBlks(vid);
    if (!mergeFreeBlks(id))
        insertFreeBlk(id);
}

/// Insert a block in the free list keeping it ordered by id
fn insertFreeBlk(id: VectorId) void {
    var fb = freeBlks;
    var pl = &freeBlks;

    while (fb != 0) {
        if (fb > id) {
            pl.* = id;
            vecArray[id + 1] = fb;
            break;
        }
        pl = &vecArray[fb + 1];
        fb = pl.*;
    } else {
        // All other blocks come before us; insert us at the end of the list
        pl.* = id;
        vecArray[id + 1] = 0;
    }
}

fn mergeFreeBlks(vid: VectorId) bool {
    const id = vid;
    var fb = freeBlks;
    var pl = &freeBlks;
    const len = vecArray[id];
    var flen: u32 = 0;
    var merged = false;

// print("\n",.{});
    while (fb != 0) {
        flen = vecArray[fb];
        // print("while: fb={}, id={}, flen={}, len={}, freeBlks={}\n", .{fb,id,flen,len,freeBlks});
        // Is this free block just under us?
        if (fb + 1 + flen == id) {
            // Yes, merge with it
            // print("fb under1: fb={}, id={}, flen={}, len={}, freeBlks={}\n", .{fb,id,flen,len,freeBlks});
            flen += len + 1;
            vecArray[fb] = flen;
            merged = true;
            // print("fb under2: fb={}, id={}, flen={}, len={}, freeBlks={}\n", .{fb,id,flen,len,freeBlks});
            // Is the next free block just above us?
            const nfb = vecArray[fb + 1];
            if (id + 1 + len == nfb) {
                // Yes, merge with it too
                vecArray[fb] += vecArray[nfb] + 1;
                vecArray[fb + 1] = vecArray[nfb + 1];
            }
            break;
        }
        // Is this free block just above us?
        if (id + 1 + len == fb) {
            // print("fb above1: fb={}, id={}, flen={}, len={}, freeBlks={}\n", .{fb,id,flen,len,freeBlks});
            // Yes, merge with it
            flen += len + 1;
            vecArray[id] = flen;
            vecArray[id + 1] = vecArray[fb + 1];
            pl.* = id;
            merged = true;
            // print("fb above2: fb={}, id={}, flen={}, len={}, freeBlks={}\n", .{fb,id,flen,len,freeBlks});
            break;
        }
        // At this point fb may be 0 but we don't need a special check
        // bcause vecArray[0] is always 0, so the loop will end normally
        // anyway.
        pl = &vecArray[fb + 1];
        fb = pl.*;           // Next fb in the list
    }

    // Return true if we did merge with anothe free block. Otherwise return
    // false and in this case the caller must insert us in the free list.    
    return merged;
}

/// Insert a block in the zero length list keeping it ordered by id
fn insertZeroBlk(id: VectorId) void {
    var zb = zeroBlks;
    var pl = &zeroBlks;

    while (zb != 0) {
        if (zb > id) {
            pl.* = id;
            vecArray[id] = zb;
            break;
        }
        pl = &vecArray[zb];
        zb = pl.*;
    } else {
        // All other blocks come before us; insert us at the end of the list
        pl.* = id;
        vecArray[id] = 0;
    }
}

fn mergeZeroBlks(vid: VectorId) VectorId {
    var id = vid;
    var zb = zeroBlks;
    var pl = &zeroBlks;
    var len = vecArray[id];

// print("\n",.{});
    while (zb != 0) {
        // print("while: zb={}, id={}, len={}, zeroBlks={}\n", .{zb,id,len,zeroBlks});
        // Is this zero block just under us?
        if (zb + 1 == id) {
            // Yes, merge with it
            // print("zb under1: zb={}, id={}, len={}, zeroBlks={}\n", .{zb,id,len,zeroBlks});
            pl.* = vecArray[zb]; // Remove zb from the list
            zb = pl.*;           // Next zb in the list
            id -= 1;                // Grow down by 1
            len += 1;
            vecArray[id] = len;
            // print("zb under2: zb={}, id={}, len={}, zeroBlks={}\n", .{zb,id,len,zeroBlks});
        }
        // Is this zero block just above us?
        if (id + 1 + len == zb) {
            // print("zb above1: zb={}, id={}, len={}, zeroBlks={}\n", .{zb,id,len,zeroBlks});
            // Yes, merge with it
            pl.* = vecArray[zb]; // Remove zb from the list
            zb = pl.*;           // Next zb in the list
            len += 1;
            vecArray[id] = len;
            // print("zb above2: zb={}, id={}, len={}, zeroBlks={}\n", .{zb,id,len,zeroBlks});
        }
        // At this point zb may be 0 but we don't need a special check
        // bcause vecArray[0] is always 0, so the loop will end normally
        // anyway.
        pl = &vecArray[zb];
        zb = pl.*;           // Next zb in the list
    }
    
    // The id changes if we merge with a zero block
    // just below us so we have to return the new one
    return id;
}

test "allocating and freeing vectors" {
    const expect = std.testing.expect;
    const n = 128;
    try init(n);
    defer deinit();

    // When we start there's only 1 free block
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 0: general tests
    var s1 = try alloc(20);
    try expect(freeBlks == 22);
    try expect(zeroBlks == 0);
    try expect(vecArray[1] == 20);
    try expect(vecArray[22] == 105);    // Remaining space
    try expect(vecArray[23] == 0);      // End of free list

    var s2 = try alloc(105);
    try expect(freeBlks == 0);
    try expect(zeroBlks == 0);

    // Part 1: merge with free block before
    free(s1);                           // Creates a hole before s2
    try expect(freeBlks == 1);          // 1 block in free list
    try expect(vecArray[1] == 20);      // Size of free block
    try expect(vecArray[2] == 0);       // end of list
    free(s2);                           // Should merge with s1 before it
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 2: merge with free block after
    s1 = try alloc(20);
    s2 = try alloc(30);
    try expect(freeBlks == 53);
    try expect(vecArray[53] == 74);     // Size of free block
    try expect(vecArray[54] == 0);      // end of list
    free(s2);                           // Should merge with free block after it
    try expect(freeBlks == 22);
    try expect(vecArray[22] == 105);    // Size of free block
    try expect(vecArray[23] == 0);      // end of list
    free(s1);                           // Should merge with s2 after it
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 3: merge with free blocks before and after
    s1 = try alloc(20);
    s2 = try alloc(30);
    free(s1);                           // Should merge with free block after it
    try expect(freeBlks == 1);
    try expect(vecArray[1] == 20);     // Size of free block
    try expect(vecArray[2] == 53);     // Second free block
    try expect(vecArray[53] == 74);     // Size of free block
    try expect(vecArray[54] == 0);      // end of list
    free(s2);
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 4: merge with zero-length block before
    s1 = try alloc(20);
    s2 = try alloc(30);
    free(s1);
    s1 = try alloc(19);
    try expect(zeroBlks == 21);
    try expect(vecArray[21] == 0);
    free(s2);                               // Should merge with zero block @ 21
    try expect(zeroBlks == 0);
    try expect(freeBlks == 21);
    try expect(vecArray[21] == 106);
    try expect(vecArray[22] == 0);
    free(s1);
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 5: merge with zero-length block after
    s1 = try alloc(20);
    s2 = try alloc(30);
    free(s1);
    s1 = try alloc(19);
    try expect(zeroBlks == 21);
    try expect(vecArray[21] == 0);
    free(s1);                               // Should merge with zero block @ 21
    try expect(zeroBlks == 0);
    try expect(freeBlks == 1);
    try expect(vecArray[1] == 20);
    try expect(vecArray[2] == 53);
    try expect(vecArray[53] == 74);
    try expect(vecArray[54] == 0);
    free(s2);
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list

    // Part 6: merge with zero-length blocks before and after
    s1 = try alloc(20);
    s2 = try alloc(30);
    const s3 = try alloc(50);
    try expect(s3 == 53);
    free(s2);
    s2 = try alloc(29);
    try expect(s2 == 22);
    free(s1);
    s1 = try alloc(19);
    try expect(zeroBlks == 21);         // Two zero-length blocks chained
    try expect(vecArray[21] == 52);
    try expect(vecArray[52] == 0);
    free(s2);                           // Should merge with both zero blocks (@21 and @52)
    try expect(zeroBlks == 0);
    try expect(freeBlks == 21);
    try expect(vecArray[21] == 31);
    try expect(vecArray[22] == 104);
    free(s1);
    free(s3);
    try expect(zeroBlks == 0);
    try expect(freeBlks == 1);          // Initial condition
    try expect(vecArray[1] == 126);     // Size of free block
    try expect(vecArray[2] == 0);       // end of list
}