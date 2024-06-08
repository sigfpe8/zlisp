const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

pub const ReadError = error{
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
    NetNameDeleted,
    NoSpaceLeft,
    SocketNotConnected,
};

pub const ParsingError = error{
    ExpectedRightParenthesis,
    ExpectedRightBracket,
    ExpectedRightBrace,
    VectorIsTooLong,
    SymbolIsTooLong,
    InvalidCharName,
};

pub const TokenError = error{
    InvalidNumber,
    InvalidComplexNumber,
    InvalidNumberPrefix,
    InvalidBase2Number,
    InvalidBase8Number,
    InvalidBase10Number,
    InvalidBase16Number,
    InvalidMatissaWidth,
    ExpectedExponent,
};

pub const EvalError = error{
    UnboundVariable,
    LetrecUndefVariable,
    ExpectedOneArgument,
    ExpectedTwoArguments,
    ExpectedThreeArguments,
    ExpectedPair,
    ExpectedSymbol,
    ExpectedVariable,
    ExpectedList,
    ExpectedInteger,
    ExpectedReal,
    ExpectedNumber,
    ExpectedCharacter,
    ExpectedString,
    ExpectedProcedure,
    ExpectedInputPort,
    ExpectedOutputPort,
    ExpectedFormals,
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
    OpenOutputFileFailed,
    OpenInputFileFailed,
    PortIsClosed,
    EvalStackOverflow,
};

pub const SchemeError = ReadError || TokenError || ParsingError || EvalError;

test "error sets" {
    print("\n", .{});
    print("@errorToInt(ReadError.StreamTooLong)     = {d}\n", .{@intFromError(ReadError.StreamTooLong)});
    print("@errorToInt(anyerror.StreamTooLong)      = {d}\n", .{@intFromError(anyerror.StreamTooLong)});
    print("@errorToInt(SchemeError.StreamTooLong)   = {d}\n", .{@intFromError(SchemeError.StreamTooLong)});

    var err: u16 = 1;
    while (err < 100) : (err += 1) {
        print("{d}  {!}\n", .{ err, @errorFromInt(err) });
    }
}
