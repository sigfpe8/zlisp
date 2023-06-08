const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

pub const ReadError = error {
    StreamTooLong,
    AccessDenied,
    BrokenPipe,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    InputOutput,
    IsDir,
    NotOpenForReading,
    OperationAborted,
    SystemResources,
    Unexpected,
    WouldBlock,
};

pub const ParsingError = error{
    ExpectedRightParenthesis,
    ExpectedRightBracket,
    ExpectedRightBrace,
    VectorIsTooLong,
    SymbolIsTooLong,
    InvalidCharName,
};

pub const EvalError = error{
    UndefinedVariable,
    LetrecUndefVariable,
    ExpectedOneArgument,
    ExpectedTwoArguments,
    ExpectedThreeArguments,
    ExpectedPair,
    ExpectedSymbol,
    ExpectedVariable,
    ExpectedList,
    ExpectedInteger,
    ExpectedNumber,
    ExpectedCharacter,
    ExpectedString,
    ExpectedProcedure,
    DefineFailed,
    PrintError,
    WrongNumberOfArguments,
    TooManyFormals,
    TooManyArguments,
    TooFewArguments,
    OutOfMemory,
    DivisionByZero,
    ElseClauseMustBeLast,
    QuasiquoteExpectsOnly1Argument,
    UnquoteExpectsOnly1Argument,
    UnquoteSplExpectsOnly1Argument,
    UnquoteSplicingMustBeList,
    InvalidQuasiquoteElement,
    UnquoteOutsideQuasiquote,
    InvalidUnicodeValue,
    InvalidReference,
    InvalidDenominator,
};

pub const SchemeError = ReadError || ParsingError || EvalError;

test "error sets" {
    print("\n", .{});
    print("@errorToInt(ReadError.StreamTooLong)     = {d}\n", .{@errorToInt(ReadError.StreamTooLong)});
    print("@errorToInt(anyerror.StreamTooLong)      = {d}\n", .{@errorToInt(anyerror.StreamTooLong)});
    print("@errorToInt(SchemeError.StreamTooLong)   = {d}\n", .{@errorToInt(SchemeError.StreamTooLong)});

    var err: u16 = 1;
    while (err < 100) : (err += 1) {
        print("{d}  {!}\n", .{err, @intToError(err)});
    }
}