use crate::lex::cursor::Cursor;
use crate::lex::token::Token;
use crate::parse::ast::Type;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    InvalidNumberLiteral,
    EOF,
    InvalidEscapeSequence,
    InvalidCharacter(char),
}

/// Parses a Comment.
/// Example:
/// start
///   v
///   #This is a comment
///                    ^
///                   End
///
fn parse_comment(cursor: &mut Cursor) -> Result<Token, LexerError> {
    cursor.next();
    Ok(Token::Comment(cursor.peek_until(|string| string == '\n')))
}
/// Parses a string literal.
/// Example:
/// start
///   v
///   "Hello World!\n"
///                  ^
///                 End
///
fn parse_string_literal(cursor: &mut Cursor, quote: char) -> Result<Token, LexerError> {
    let mut result = String::new();
    while let Some(character) = cursor.next() {
        if character == quote {
            break;
        }

        if character == '\\' {
            match cursor.next().ok_or(LexerError::EOF)? {
                '\\' => result.push('\\'),
                'n' => result.push('\n'),
                't' => result.push('\t'),
                'r' => result.push('\r'),
                '"' => result.push('"'),
                '\'' => result.push('\''),

                _ => return Err(LexerError::InvalidEscapeSequence),
            }
            continue;
        }
        result.push(character);
    }
    Ok(Token::StringLiteral(result))
}

/// Parses a number literal.
/// Example:
/// start
///   v
///   1.2
///     ^
///    End
///
fn parse_number_literal(cursor: &mut Cursor) -> Result<Token, LexerError> {
    cursor.back();

    let mut number_value = String::new();
    while let Some(character) = cursor.next() {
        if character.is_digit(10) || character == '.' {
            number_value.push(character);
        } else {
            break;
        }
    }

    cursor.back();

    let number_token = if number_value.contains('.') {
        match number_value.parse::<f64>() {
            Ok(float_value) => Token::FloatLiteral(float_value),
            Err(_) => return Err(LexerError::InvalidNumberLiteral),
        }
    } else {
        match number_value.parse::<i64>() {
            Ok(int_value) => Token::IntegerLiteral(int_value),
            Err(_) => return Err(LexerError::InvalidNumberLiteral),
        }
    };

    Ok(number_token)
}

/// Parses a identntifier.
/// Example:
/// start
///   v
///   variable other_variable
///          ^
///         End
///
fn parse_identifier(cursor: &mut Cursor) -> Result<Token, LexerError> {
    let identifier = cursor.peek_until(|ch| !(ch.is_alphanumeric() || ch == '_'));
    match identifier.as_str() {
        "do" => Ok(Token::Do),
        "end" => Ok(Token::End),
        "int" => Ok(Token::Type(Type::Int)),
        "float" => Ok(Token::Type(Type::Float)),
        "str" => Ok(Token::Type(Type::Str)),
        "bool" => Ok(Token::Type(Type::Bool)),
        "match" => Ok(Token::Match),
        "del" => Ok(Token::Del),

        "if" => Ok(Token::If),
        "then" => Ok(Token::Then),
        "else" => Ok(Token::Else),
        "fn" => Ok(Token::Fn),
        "_" => Ok(Token::Underscore),

        "True" | "true" => Ok(Token::True),
        "False" | "false" => Ok(Token::False),
        _ => Ok(Token::Identifier(identifier)),
    }
}
///Returns a single token while taking the cursor into account.
fn parse_string(cursor: &mut Cursor) -> Option<Result<Token, LexerError>> {
    let next = cursor.next()?;
    Some(match next {
        '"' => parse_string_literal(cursor, '"'),
        '\'' => parse_string_literal(cursor, '\''),
        '0'..='9' => parse_number_literal(cursor),
        'A'..='Z' | 'a'..='z' | '_' => parse_identifier(cursor),
        '+' => Ok(Token::Plus),
        '-' => {
            cursor.next();
            match cursor.peek()? {
                '>' => Ok(Token::LeftArrow),
                '0'..='9' => Ok(match parse_number_literal(cursor) {
                    Ok(Token::FloatLiteral(number)) => Token::FloatLiteral(-number),
                    Ok(Token::IntegerLiteral(number)) => Token::IntegerLiteral(-number),
                    Err(err) => return Some(Err(err)),
                    _ => unreachable!(),
                }),
                _ => Ok(Token::Minus),
            }
        }

        '*' => Ok(Token::Multiply),
        '/' => match cursor.next()? {
            '/' => Ok(Token::FloorDiv),
            _ => {
                cursor.back();
                Ok(Token::Divide)
            }
        },
        '>' => match cursor.next()? {
            '=' => Ok(Token::GreaterThanOrEqual),
            _ => {
                cursor.back();
                Ok(Token::GreaterThan)
            }
        },
        '<' => match cursor.next()? {
            '=' => Ok(Token::LessThanOrEqual),
            _ => {
                cursor.back();
                Ok(Token::LessThan)
            }
        },
        '=' => match cursor.next()? {
            '=' => Ok(Token::Equal),
            '>' => Ok(Token::FatArrow),
            _ => {
                cursor.back();
                Ok(Token::Assign)
            }
        },
        '[' => Ok(Token::LeftSquareBracket),
        ']' => Ok(Token::RightSquareBracket),
        '{' => Ok(Token::LeftCurlyBracket),
        '}' => Ok(Token::RightCurlyBracket),
        '(' => Ok(Token::LeftParenthesis),
        ')' => Ok(Token::RightParenthesis),
        ';' => Ok(Token::Semicolon),
        ':' => Ok(Token::Colon),
        ',' => Ok(Token::Comma),
        '|' => Ok(Token::Pipe),
        '&' => Ok(Token::And),
        '%' => Ok(Token::Modulo),
        '!' => {
            cursor.next();
            match cursor.peek_increment()? {
                '=' => Ok(Token::NotEqual),
                _ => {
                    cursor.back();
                    Ok(Token::Not)
                }
            }
        }
        ' ' | '\t' | '\n' | '\r' => Ok(Token::Empty),
        '#' => parse_comment(cursor),
        _ => Err(LexerError::InvalidCharacter(next)),
    })
}
/// Lexs a String as source codede and returns a vector of tokens.
pub fn lex(source_code: &str) -> Result<Vec<Token>, LexerError> {
    let mut cursor = Cursor::new(" ".to_string() + source_code);
    let mut tokens = Vec::new();

    while let Some(token) = parse_string(&mut cursor) {
        let token = token?;
        match token {
            Token::Empty => continue,
            _ => tokens.push(token),
        }
    }

    tokens.push(Token::EOF);

    Ok(tokens)
}
