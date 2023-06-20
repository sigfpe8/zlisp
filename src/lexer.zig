const std = @import("std");
const chr = @import("char.zig");
const erz = @import("error.zig");
const nbr = @import("number.zig");
const sxp = @import("sexpr.zig");
const sym = @import("symbol.zig");
const str = @import("string.zig");

const ascii = std.ascii;

const Number = nbr.Number;
const Complex = nbr.Complex;
const Polar = nbr.Polar;
const Rational = nbr.Rational;
const Real = nbr.Real;

const makeComplex = sxp.makeComplex;
const makeFloat = sxp.makeFloat;
const makeInteger = sxp.makeInteger;
const makePolar = sxp.makePolar;
const makeRational = sxp.makeRational;
const makeReal = nbr.makeReal;

const mem = std.mem;
const print = std.debug.print;
const Sexpr = sxp.Sexpr;

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
                         number,        // Any number                         
                         string,        // "string"
                         end, unknown };

//const extendedChars = "+-.*/<=>!?:$%_&~^";

const ReadError = erz.ReadError;
const SchemeError = erz.SchemeError;
const TokenError = erz.TokenError;

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

fn isRadixDigit(radix: u8, ch: u8) bool {
    return switch (radix) {
         2 => ch == '0' or ch == '1',
         8 => ch >= '0' and ch <= '7',
        10 => ch >= '0' and ch <= '9',
        16 => (ch >= '0' and ch <= '9') or
                      (ch >= 'a' and ch <= 'f') or
                      (ch >= 'A' and ch <= 'F'),
        else => unreachable,
    };
}

fn isExpMarker(ch: u8) bool {
    // <exponent marker> --> e | E | s | S | f | F | d | D | l | L
    return switch (ch) {
        'd', 'D' => true,
        'e', 'E' => true,
        'f', 'F' => true,
        'l', 'L' => true,
        's', 'S' => true,
        else => false,
    };
}

fn invalidNumber(radix: u8) TokenError {
    switch (radix) {
         2 => return TokenError.InvalidBase2Number,
         8 => return TokenError.InvalidBase8Number,
        10 => return TokenError.InvalidBase10Number,
        16 => return TokenError.InvalidBase16Number,
        else => unreachable,
    }
    unreachable;
}

fn signReal(sign: u8, real: Real) Real {
    if (sign == '+')
        return real;

    return switch (real) {
        .int => |int| Real{ .int = -int },
        .flt => |flt| Real{ .flt = -flt },
        .rat => |rat| Real{ .rat =
            Rational{ .num = -rat.num, .den = rat.den }
        },
    };
}

fn getSign(real: Real) i64 {
    var sign: i64 = undefined;
    switch (real) {
        .int => |int| { sign = int; },
        .rat => |rat| { sign = rat.num; },
        .flt => |flt| {
             if (flt < 0) return -1;
             if (flt > 0) return  1;
             return 0;
        },
    }
    if (sign < 0) return -1;
    if (sign > 0) return  1;
    return 0;
}

pub fn makeNumber(num: Number) !Sexpr {
    var sexpr: Sexpr = undefined;
    switch (num) {
        .pol => |pol| {
            const mag = try makeReal(pol.mag);
            const ang = try makeReal(pol.ang);
            sexpr = try makePolar(mag,ang);
        },
        .cmp => |cmp| {
            const re = try makeReal(cmp.re);
            if (getSign(cmp.im) == 0) {
                sexpr = re;
            } else {
                const im = try makeReal(cmp.im);
                sexpr = try makeComplex(re,im);
            }
        },
    }
    return sexpr;
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
    xvalue: u32 = 0,        // Index if for a symbol token
    token: Token = .end,    // Current token type
    number: Number = undefined,     // Value of number token
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

        if (ascii.isDigit(self.cchar) or self.isNumberStart(self.cchar))
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

    fn peekChar(self: *Lexer, next: usize) u8 {
        if (self.cpos + next - 1 >= self.line.len)
            return 0;
        return self.line[self.cpos + next - 1];
    }

    fn peekNextChar(self: *Lexer) u8 {
        if (self.cpos >= self.line.len)
            return 0;
        return self.line[self.cpos];
    }

    fn nextMatch(self: *Lexer, next: []const u8) bool {
        const len = next.len;
        const pos = self.cpos - 1;
        if (pos + len <= self.line.len and
            std.mem.eql(u8, self.line[pos..pos+len], next)) {
            self.cpos += len - 1;   // Consume 'next' characters
            self.nextChar();
            return true;
        }
        return false;
    }

    fn isNumberStart(self: *Lexer, ch: u8) bool {
        const ch1 = self.peekNextChar();
        if (ch == '#') {
            switch (ch1) {
                // Radix prefix
                'b', 'B', 'o', 'O', 'd', 'D', 'x', 'X' => return true,
                // Exactness prefix
                'i', 'I', 'e', 'E' => return true,
                else => return false,
            }
        } else if (ch == '+' or ch == '-') {
            // i or inf.0 or nan.0
            if (ascii.isDigit(ch1) or ch1 == 'i' or ch1 == 'n')
                return true;
            // .12345
            if (ch1 == '.' and ascii.isDigit(self.peekChar(2)))
                return true;
        } else if (ch == '.' and ascii.isDigit(ch1))
            return true;
        return false;
    }

    // R6RS number grammar
    //
    // <number> --> <num 2> | <num 8> | <num 10> | <num 16>
    // <num R> --> <prefix R> <complex R>
    // <complex R> --> <real R> | <real R> @ <real R>
    //     | <realR> + <urealR> i | <realR> - <ureal R> i | <real R> + <naninf> i | <real R> - <naninf> i
    //     | <real R> + i | <real R> - i
    //     | + <ureal R> i | - <ureal R> i
    //     | + <naninf> i | - <naninf> i
    //     | + i | -i
    // <real R> --> <sign> <ureal R>
    //     | + <naninf> | - <naninf>
    // <naninf> --> nan.0 | inf.0
    // <ureal R> --> <uinteger R>
    //     | <uinteger R> / <uinteger R>
    //     | <decimal R> <mantissa width>
    // <decimal 10> --> <uinteger 10> <suffix>
    //     | .<digit 10>+ <suffix>
    //     | <digit 10>+ . <digit 10>* <suffix>
    //     | <digit 10>+ . <suffix>
    // <uinteger R> --> <digit R>+
    // <prefix R> --> <radix R> <exactness> | <exactness> <radix R>
    // <suffix> --> <empty> | <exponent marker> <sign> <digit 10>+
    // <exponent marker>--> e | E | s | S | f | F | d | D | l | L
    // <mantissa width> --> <empty> | | <digit 10>+
    // <sign> --> <empty> | + | -
    // <exactness> --> <empty> | #i | #I | #e | #E
    // <radix 2> --> #b | #B
    // <radix 8> --> #o | #O
    // <radix 10> --> <empty> | #d | #D
    // <radix 16> --> #x | #X
    // <digit 2> --> 0 | 1
    // <digit 8> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
    // <digit 10> --> <digit>
    // <digit 16> --> <hex digit>
    // <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
    // <hex digit> --> <digit> | a | A | b | B | c | C | d | D | e | E | f | F

    fn parseNumber(self: *Lexer) !void {
        var radix: u8 = 10;
        var exact: bool = true;

        self.token = .number;

        // <num R> --> <prefix R> <complex R>
        // <prefix R> --> <radix R> <exactness> | <exactness> <radix R>
        if (self.cchar == '#') {
            var prefix: i32 = 0;
            self.nextChar();    // Skip first #
            switch (self.cchar) {
                'b', 'B' => { radix = 2;  prefix = 1; }, 
                'o', 'O' => { radix = 8;  prefix = 1; },
                'd', 'D' => { radix = 10; prefix = 1; },
                'x', 'X' => { radix = 16; prefix = 1; },
                'e', 'E' => { exact = true;  prefix = 4; },
                'i', 'I' => { exact = false; prefix = 4; },
                else => unreachable,
            }
            self.nextChar();    // Skip first prefix letter
            if (self.cchar == '#') {
                self.nextChar();    // Skip second #
                switch (self.cchar) {
                    'b', 'B' => { radix = 2;  prefix += 2; }, 
                    'o', 'O' => { radix = 8;  prefix += 2; },
                    'd', 'D' => { radix = 10; prefix += 2; },
                    'x', 'X' => { radix = 16; prefix += 2; },
                    'e', 'E' => { exact = true;  prefix += 8; },
                    'i', 'I' => { exact = false; prefix += 8; },
                    else => return TokenError.InvalidNumberPrefix,
                }
                self.nextChar();    // Skip second prefix letter
                // Duplicated radix or exactness?
                if (prefix == 3 or prefix == 12)
                    return TokenError.InvalidNumberPrefix;
            }
        }

        var sign: u8 = '+';
        var real: Real = undefined;
        var imag: Real = undefined;

        if (self.cchar == '+' or self.cchar == '-') {
            sign = self.cchar;
            self.nextChar();        // Skip sign
            real = try self.parseUreal(radix);
            real = signReal(sign,real);
            if (self.cchar == 'i') {
                // Only imaginary part
                //     | + <ureal R> i | - <ureal R> i
                //     | + <naninf> i | - <naninf> i
                //     | + i | - i
                self.nextChar();    // Skip i
                self.number = Number{ .cmp = Complex{ .re = Real{.int = 0}, .im = real }};
                return;
            }
        } else {
            real = try self.parseReal(radix);
        }

        // Complex number in polar form: magnitude @ angle
        if (self.cchar == '@') {
            self.nextChar();    // Skip @
            imag = try self.parseReal(radix);
            self.number = Number{ .pol = Polar{ .mag = real, .ang = imag }};
            return;
        }

        // Complex number in rectangular form: real +- imag i
        if (self.cchar == '+' or self.cchar == '-') {
            sign = self.cchar;
            self.nextChar();        // Skip sign
            imag = try self.parseUreal(radix);
            imag = signReal(sign,imag);
            if (self.cchar == 'i') {
                self.nextChar();    // Skip i
                self.number = Number{ .cmp = Complex{ .re = real, .im = imag }};
                return;
            }
            return TokenError.InvalidComplexNumber;
        }

        // Only real part
        self.number = Number{ .cmp = Complex{ .re = real, .im = Real{ .int = 0 }}};
    }

    fn parseRational(self: *Lexer, radix: u8, numBegin: usize) !Real {
        var begin = numBegin;
        var end = self.cpos - 1;

        // Numerator
        const num = try std.fmt.parseInt(i64, self.line[begin..end], radix);

        self.nextChar();        // Skip /
        begin = self.cpos - 1;
        // if (!ascii.isDigit(self.cchar)) --> TODO: treat as symbol
        if (!isRadixDigit(radix, self.cchar))
            return SchemeError.InvalidDenominator;

        // Denominator
        while (isRadixDigit(radix, self.cchar))
            self.nextChar();

        end = self.cpos - 1;
        const den = try std.fmt.parseInt(i64, self.line[begin..end], radix);

        return Real{ .rat = Rational{.num = num, .den = den} };
    }

    fn parseReal(self: *Lexer, radix: u8) !Real {
        var sign: u8 = '+';
        if (self.cchar == '+' or self.cchar == '-') {
            sign = self.cchar;
            self.nextChar();
        }

        const ureal = try self.parseUreal(radix);
        return signReal(sign, ureal);
    }

    fn parseUreal(self: *Lexer, radix: u8) !Real {
        // <ureal R> --> <uinteger R>
        //     | <uinteger R> / <uinteger R>
        //     | <decimal R> <mantissa width>
        //     | nan.0 | inf.0

        const begin = self.cpos - 1;

        // <uinteger R> ?
        if (isRadixDigit(radix, self.cchar)) {
            while (isRadixDigit(radix, self.cchar))
                self.nextChar();

            if (self.cchar == '.' or isExpMarker(self.cchar))
                return try self.parseDecimal(radix, begin);

            if (self.cchar == '/')
                return try self.parseRational(radix, begin);

            const end = self.cpos - 1;
            const num: i64 = try std.fmt.parseInt(i64, self.line[begin..end], radix);
            return Real{ .int = num };
        }

        // . <digit 10>+ <suffix> ?
        if (self.cchar == '.')
            return try self.parseDecimal(radix, begin);

        // i or inf.0 ?
        if (self.cchar == 'i') {
            if (self.nextMatch("inf.0"))
                return Real{ .flt = std.math.inf(f64) };
            return Real{ .int = 1 };  // Fake 1i
        }

        // nan.0 ?
        if (self.cchar == 'n' and self.nextMatch("nan.0"))
            return Real{ .flt = std.math.nan(f64) };

        return TokenError.InvalidNumber;
    }

    // Called with cchar == '.' or cchar == exponent marker.
    // If integer part is present, 'begin' points to its 1st digit.
    // Otherwise it points to '.' or the exponent marker.
    fn parseDecimal(self: *Lexer, radix: u8, begin: usize) !Real {
        if (radix != 10)
            return TokenError.InvalidBase10Number;
        // <decimal 10> --> <uinteger 10> <suffix>
        //     | . <digit 10>+ <suffix>
        //     | <digit 10>+ . <digit 10>* <suffix>
        //     | <digit 10>+ . <suffix>
        // <suffix> --> <empty> | <exponent marker> <sign> <digit 10>+
        // <exponent marker>--> e | E | s | S | f | F | d | D | l | L

        // We need to copy the number to a buffer in order to map
        // all Scheme exponent markers to 'e'. This is because
        // we use std.fmt.parseFloat() to do the actual conversion
        // from string to f64 and it only understands 'e'.
        var decimal = try std.ArrayList(u8).initCapacity(allocator, 64);
        defer decimal.deinit();

        if (self.cchar == '.') {
            // Copy integer part (if any) + decimal point
            const end = self.cpos;
            try decimal.appendSlice(self.line[begin..end]);

            // Skip .
            self.nextChar();

            // Copy decimal part (if any)
            while (ascii.isDigit(self.cchar)) {
                try decimal.append(self.cchar);
                self.nextChar();
            }
            // Number must have at least 1 digit, like "1." or ".5".
            // A single "." is not valid.
            if (decimal.items.len == 1)
                return TokenError.InvalidBase10Number;
        } else {    // <uinteger 10> <suffix>
            // Copy integer part
            const end = self.cpos - 1;
            try decimal.appendSlice(self.line[begin..end]);
        }

        // Parse exponent
        if (isExpMarker(self.cchar)) {
            try decimal.append('e');
            self.nextChar();
            if (self.cchar == '+' or self.cchar == '-') {
                try decimal.append(self.cchar);
                self.nextChar();
            }
            // Exponent must have at least 1 digit
            if (!ascii.isDigit(self.cchar))
                return TokenError.ExpectedExponent;
            while (ascii.isDigit(self.cchar)) {
                try decimal.append(self.cchar);
                self.nextChar();
            }
        }

        // Parse matissa width (just ignore it)
        // <mantissa width> --> <empty> | | <digit 10>+
        if (self.cchar == '|') {
            self.nextChar(); // Skip |
            if (!ascii.isDigit(self.cchar))
                return TokenError.InvalidMatissaWidth;
            while (ascii.isDigit(self.cchar))
                self.nextChar();
        }

        const num: f64 = try std.fmt.parseFloat(f64, decimal.items[0..]);
        return Real{ .flt = num };
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
            i += 1;
            while (i < self.cpos) : (i += 1) {
                print("~", .{});
            }
            print("\n", .{});
        }
    }
};
