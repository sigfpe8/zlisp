const std = @import("std");
const ascii = std.ascii;
const chr = @import("char.zig");
const erz = @import("error.zig");
const print = std.debug.print;
const sym = @import("symbol.zig");
const str = @import("string.zig");
const mem = std.mem;

var ggpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = ggpa.allocator();

pub const Token = enum { lparens,       // ( [ {
                         rparens,       // ) ] }
                         dot,           // .
                         quote,         // '
                         qquote,        // `
                         unquote,       // ,
                         unquote_spl,   // ,@
                         hash_f,        // #f
                         hash_t,        // #t
                         hash_vec,      // #(
                         hash_char,     // #\
                         symbol,        // name
                         integer,       // 1234567890
                         rational,      // 2/3
                         float,         // 1.23456789
                         string,        // "string"
                         end, unknown };

//const extendedChars = "+-.*/<=>!?:$%_&~^";

const ReadError = erz.ReadError;
const SchemeError = erz.SchemeError;

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
        '<'...'?' => return true,   // < = > ?
        'A'...'Z' => return true,
        '^','_' => return true,
        'a'...'z' => return true,
        '~' => return true,
        else => return false,
    }
    return false;
}

pub const Lexer = struct {
    eof: bool = false,      // At eof?
    inexpr: bool = false,   // Inside an expression?
    silent: bool = false,   // If true, don't print anything
    lnum: usize = 1,        // Current line number
    cpos: usize = 0,        // Current character index
    begin: usize = 0,       // Index of token beginning
    cchar: u8 = 0,          // Current character
    schar: u8 = 0,          // Single byte token
    rparens: u8 = 0,        // Matching right parenthesis
    ivalue: i64 = 0,        // Value of integer token
    dvalue: i64 = 1,        // Value of denominator (for rational)
    fvalue: f64 = 0.0,      // Value of float token
    xvalue: u32 = 0,        // Index if for a symbol token
    token: Token = .end,    // Current token type
    svalue: []const u8 = undefined, // Value of string/symbol token
    line: []const u8 = undefined,   // Current source line
    buffer: []u8 = undefined,       // Line buffer
    nextLine: *const fn(lexer: *Lexer) ReadError!?[]const u8,

    const Self = @This();

    /// Advances to next char; stops at eol.
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

    /// Advances to next char; reads new line(s) if necessary.
    pub fn nextTokenChar(self: *Lexer) !void {
        while (self.cpos >= self.line.len) {
            self.cpos = 0;
            self.cchar = 0;
            self.line = try self.nextLine(self)
                        orelse {    // EOF
                            self.eof = true;
                            return;
                        };
        }

        // Assume ASCII
        self.cchar = self.line[self.cpos];
        self.cpos += 1;
    }

    pub fn nextToken(self: *Lexer) !void {
        // Skip whitespace, empty lines and comments
        while (true) {
            // Skip whitespace (and 0 = eol)
            while ((ascii.isWhitespace(self.cchar) or self.cchar == 0) and !self.eof)
                try self.nextTokenChar();

            // End of file?
            if (self.eof) {
                self.token = .end;
                return;
            }

            // Skip comment (force nextTokenChar() to read next line)
            if (self.cchar == ';') {
                self.cpos = self.line.len;
                self.cchar = 0;
                continue;
            }

            break;  // Start of a new token
        }

        self.begin = self.cpos - 1;    // Remember where the token begins

        if (ascii.isDigit(self.cchar) or
            ((self.cchar == '-' or self.cchar == '+') and ascii.isDigit(self.peekNextChar())))
            return try self.parseNumber();

        if (isSymbolStart(self.cchar))
            return try self.parseSymbol();

        if (self.cchar == '#')
            return try self.parseHash();

        if (self.cchar == '"')
            return try self.parseString();

        switch (self.cchar) {
            '(' => { self.token = .lparens; self.rparens = ')'; },
            '[' => { self.token = .lparens; self.rparens = ']'; },
            '{' => { self.token = .lparens; self.rparens = '}'; },
            ')', ']', '}' => self.token = .rparens,
            '.' => self.token = .dot,
            '\'' => self.token = .quote,
            '`' => self.token = .qquote,
            ',' => {
                if (self.peekNextChar() == '@') {
                    self.token = .unquote_spl;
                    self.nextChar();
                } else {
                    self.token = .unquote;
                }
            },
            else => self.token = .unknown,
        }

        self.schar = self.cchar; // Save token
        self.nextChar();
    }

    fn peekNextChar(self: *Lexer) u8 {
        if (self.cpos >= self.line.len)
            return 0;
        return self.line[self.cpos];
    }

    fn parseHash(self: *Lexer) !void {
        self.nextChar();    // Skip #
        switch (self.cchar) {
            'f' => self.token = .hash_f,
            't' => self.token = .hash_t,
            '(' => { self.token = .hash_vec; self.rparens = ')'; },
            '[' => { self.token = .hash_vec; self.rparens = ']'; },
            '{' => { self.token = .hash_vec; self.rparens = '}'; },
            '\\' => { return try self.parseChar(); },
            else => self.token = .unknown,
        }
        self.nextChar();
    }

    fn parseChar(self: *Lexer) !void {
        self.nextChar();    // Skip \ 
        self.token = .hash_char;
        self.xvalue = self.cchar;   // Assume it's a single character
        self.nextChar();            // Skip it

        // Check if this is #\name
        if (ascii.isAlphabetic(@truncate(u8, self.xvalue))) {
            const begin = self.cpos - 2;
            while (ascii.isAlphabetic(self.cchar))
                self.nextChar();
            const end = self.cpos - 1;
            if (end - begin > 1) {  // If not a single letter
                if (chr.codeFromName(self.line[begin..end])) |code| {
                    self.xvalue = code;
                } else {
                    return SchemeError.InvalidCharName;
                }
            }
        }
    }

    fn parseNumber(self: *Lexer) !void {
        const begin = self.cpos - 1;
        self.nextChar();
        while (ascii.isDigit(self.cchar))
            self.nextChar();
        if (self.cchar == '.')
            return try self.parseFloat(begin);
        if (self.cchar == '/')
            return try self.parseRational(begin);
        const end = self.cpos - 1;
        const value = try std.fmt.parseInt(i64, self.line[begin..end], 0);
        self.ivalue = value;
        self.token = .integer;
    }

    fn parseRational(self: *Lexer, numBegin: usize) !void {
        var begin = numBegin;
        var end = self.cpos - 1;

        const num = try std.fmt.parseInt(i64, self.line[begin..end], 0);

        self.nextChar();        // Skip /
        begin = self.cpos - 1;
        // if (!ascii.isDigit(self.cchar)) --> TODO: treat as symbol
        if (!ascii.isDigit(self.cchar))
            return SchemeError.InvalidDenominator;

        while (ascii.isDigit(self.cchar))
            self.nextChar();

        end = self.cpos - 1;
        const den = try std.fmt.parseInt(i64, self.line[begin..end], 0);

        self.ivalue = num;
        self.dvalue = den;
        self.token  = .rational;
    }

    // Parse floating-point number after decimal point
    fn parseFloat(self: *Lexer, begin: usize) !void {
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
        const value = try std.fmt.parseFloat(f64, self.line[begin..end]);
        self.fvalue = value;
        self.token = .float;
    }

    fn parseSymbol(self: *Lexer) !void {
        const begin = self.cpos - 1;
        self.nextChar();
        while (isSymbolChar(self.cchar))
            self.nextChar();
        const end = self.cpos - 1;
        if (end - begin > 255)
            return SchemeError.SymbolIsTooLong;
        self.svalue = self.line[begin..end];
        self.xvalue = try sym.intern(self.line[begin..end]);
        self.token = .symbol;
    }

    fn parseString(self: *Lexer) !void {
        var lit = std.ArrayList(u8).init(allocator);
        defer lit.deinit();
        self.nextChar();    // Skip starting "
        while (self.cchar != '"') {
            var cchar = self.cchar;
            if (cchar == '\\') {
                self.nextChar();
                cchar = self.cchar;
                cchar = switch(cchar) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    else => cchar,
                };
            }
            if (cchar == 0) {   // End of line
                const save_inexpr = self.inexpr;
                self.inexpr = true;
                try lit.append('\n');
                try self.nextTokenChar();
                self.inexpr = save_inexpr;
                continue;
            }
            try lit.append(cchar);
            self.nextChar();
        }
        self.nextChar();    // Skip ending "
        self.xvalue = try str.add(lit.items);
        self.token = .string;
    }

    pub fn logError(self: *Lexer, err: anyerror) void {
        if (!self.silent) {
            print("\nSyntax error: {!}\n{s}\n", .{ err, self.line[0..] });
            var i: usize = 0;
            while (i < self.begin) : (i += 1) {
                if (self.line[i] != '\t') {
                    print(" ", .{});
                } else {
                    print("\t", .{});
                }
            }
            print("^", .{});
            i += 2;
            while (i < self.cpos) : (i += 1) {
                print("~", .{});
            }
            print("\n", .{});
        }
    }
};
