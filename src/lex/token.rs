#[derive(Debug, PartialEq)]
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
    FloorDiv,
    // Keywords
    If,
    Then,
    Else,
    End,
    Fn,
    Do,
    // Types
    Int,
    Float,
    Str,
    Bool,
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

    And,
    Pipe,
    Not,

    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftParenthesis,
    RightParenthesis,
    Semicolon,
    Comma,

    LeftArrow,
}
