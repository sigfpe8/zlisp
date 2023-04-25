const std = @import("std");
const ascii = std.ascii;
const print = std.debug.print;
const sym = @import("symbol.zig");
const mem = std.mem;

pub const Token = enum { lparens,       // (
                         rparens,       // )
                         dot,           // .
                         quote,         // '
                         hash_f,        // #f
                         hash_t,        // #t
                         hash_vec,      // #(
                         hash_char,     // #\
                         symbol,        // name
                         integer,       // 1234567890
                         float,         // 1.23456789
                         end, unknown };

//const extendedChars = "+-.*/<=>!?:$%_&~^";

fn isSymbolStart(ch: u8) bool {
    switch (ch) {
        '$'...'&' => return true,   // $ % &
        '*','+' => return true,
        '-','/' => return true, 
        ':' => return true,
        '<'...'>' => return true,   // < = >
        'A'...'Z' => return true,
        '^','_' => return true,
        'a'...'z' => return true,
        '~' => return true,
        else => return false,
    }
    return false;
}

fn isSymbolChar(ch: u8) bool {
    switch (ch) {
        '$'...'&' => return true,   // $ % &
        '*','+' => return true,
        '-'...':' => return true,   // - . / 0 1 2 3 4 5 6 7 8 9 :
        '<'...'>' => return true,   // < = >
        'A'...'Z' => return true,
        '^','_' => return true,
        'a'...'z' => return true,
        '~' => return true,
        else => return false,
    }
    return false;
}

pub const Lexer = struct {
    line: []const u8, // Line of source code
    cpos: usize = 0, // Current character index
    cchar: u8 = 0, // Current character
    schar: u8 = 0, // Single byte token
    token: Token = undefined, // Current token type
    ivalue: i64 = 0, // Value of integer token
    fvalue: f64 = 0.0, // Value of float token
    xvalue: u32 = 0, // Index if for a symbol token
    svalue: []const u8 = undefined, // Value of string/symbol token

    pub fn nextChar(self: *Lexer) void {
        if (self.cpos >= self.line.len) {
            self.cpos = self.line.len + 1; // So that the token slice works when at the end of the line
            self.cchar = 0;
            return;
        }

        // Assume ASCII
        self.cchar = self.line[self.cpos];
        self.cpos += 1;
    }

    pub fn nextToken(self: *Lexer) void {
        // Skip whitespace
        while (ascii.isWhitespace(self.cchar))
            self.nextChar();

        // If it's the real end of the line (0) or the beginning
        // of a comment (';'), return end-of-line token.
        if (self.cchar == 0 or self.cchar == ';') {
            self.token = .end;
            return;
        }

        if (ascii.isDigit(self.cchar) or
            ((self.cchar == '-' or self.cchar == '+') and ascii.isDigit(self.peekNextChar())))
            return self.parseNumber();

        if (isSymbolStart(self.cchar))
            return self.parseSymbol();

        if (self.cchar == '#')
            return self.parseHash();

        switch (self.cchar) {
            '(' => self.token = .lparens,
            ')' => self.token = .rparens,
            '.' => self.token = .dot,
            '\'' => self.token = .quote,
            else => self.token = .unknown,
        }

        self.schar = self.cchar; // Save token
        self.nextChar();
        return;
    }

    pub fn printToken(self: *Lexer) void {
        switch (self.token) {
            Token.lparens => {
                print("(\n", .{});
            },
            Token.rparens => {
                print(")\n", .{});
            },
            Token.integer => {
                print("INTEGER {d}\n", .{self.ivalue});
            },
            Token.symbol => {
                print("SYMBOL {s}\n", .{self.svalue});
            },
            else => {
                print("UNKNOWN {c}\n", .{self.schar});
            },
        }
    }

    fn peekNextChar(self: *Lexer) u8 {
        if (self.cpos >= self.line.len)
            return 0;
        return self.line[self.cpos];
    }

    fn parseHash(self: *Lexer) void {
        self.nextChar();    // Skip #
        switch (self.cchar) {
            'f' => self.token = .hash_f,
            't' => self.token = .hash_t,
            '(' => self.token = .hash_vec,
            '\\' => {
                self.token = .hash_char;
            },
            else => self.token = .unknown,
        }
        self.nextChar();
        return;
    }

    fn parseNumber(self: *Lexer) void {
        const begin = self.cpos - 1;
        self.nextChar();
        while (ascii.isDigit(self.cchar))
            self.nextChar();
        if (self.cchar == '.')
            return self.parseFloat(begin);
        const end = self.cpos - 1;
        const value = std.fmt.parseInt(i64, self.line[begin..end], 0) catch |err| {
            print("Error {} parsing integer: {s}\n", .{ err, self.line[begin..end] });
            self.token = .end;
            return;
        };
        self.ivalue = value;
        self.token = .integer;
        return;
    }

    // Parse floating-point number after decimal point
    fn parseFloat(self: *Lexer, begin: usize) void {
        self.nextChar(); // Skip .
        while (ascii.isDigit(self.cchar)) // Decimal part
            self.nextChar();
        if (self.cchar == 'e' or self.cchar == 'E') { // Exponent indicator
            self.nextChar(); // Skip e/E
            if (self.cchar == '-' or self.cchar == '+') // Exponent sign
                self.nextChar(); // Skip sign
            while (ascii.isDigit(self.cchar)) // Exponent digits
                self.nextChar();
        }
        const end = self.cpos - 1;
        const value = std.fmt.parseFloat(f64, self.line[begin..end]) catch |err| {
            print("Error {} parsing floating-point number: {s}\n", .{ err, self.line[begin..end] });
            self.token = .end;
            return;
        };
        self.fvalue = value;
        self.token = .float;
    }

    fn parseSymbol(self: *Lexer) void {
        const begin = self.cpos - 1;
        self.nextChar();
//        while (ascii.isAlphanumeric(self.cchar) or mem.containsAtLeast(u8, extendedChars, 1, self.cchar))
        while (isSymbolChar(self.cchar))
            self.nextChar();
        const end = self.cpos - 1;
        if (end - begin > 255) {
            print("Symbol length is limited to 255 characters: {s}\n", .{self.line[begin..end]});
            self.token = .end;
            return;
        }
        self.svalue = self.line[begin..end];
        self.xvalue = sym.intern(self.line[begin..end]) catch |err| {
            print("Error {} parsing symbol: {s}\n", .{ err, self.line[begin..end] });
            self.token = .end;
            return;
        };
        self.token = .symbol;
    }
};
