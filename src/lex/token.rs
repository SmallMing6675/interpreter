use crate::parse::ast::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Variable assignment
    Mod,
    Empty,
    Assign,
    Del,
    // Comparison operators
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    // Arithmetic operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    And,
    Or,
    Not,
    Xor,

    Pipe,

    FatArrow,
    Underscore,

    FloorDiv,
    // Keywords
    If,
    Then,
    Else,
    End,
    Fn,
    Do,
    // Types
    Type(Type),
    Match,

    // Functions
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    // Other
    Comment(String),
    EOF,

    True,
    False,

    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    Semicolon,
    Comma,
    Colon,

    LeftArrow,
    Start,
}
