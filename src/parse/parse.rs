use crate::lex::token::Token;
use crate::parse::ast::*;
use crate::parse::cursor::Cursor;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    MismatchedParens,
    ExpectedType(Token),
    ExpectedVariable(Token, Token),
    InvalidToken,
    InvalidAssign(Token),
    UnexpectedEOF,
}
/// Parses a type of a variable.
/// @param &mut cursor: the cursor to look for.
/// @return: A parse type.
///
/// The cursor must start at the beginning of a type,
/// and the cursor lands on the start of the next token.
///
/// Example:
/// start
/// vvv
/// int -> int
///        ^^^
///        end
///
/// int -> [int] -> str
///
fn parse_type(cursor: &mut Cursor) -> Result<Type, ParseError> {
    let next = cursor.peek_increment().ok_or(ParseError::UnexpectedEOF)?;
    match next {
        Token::Type(t) => {
            if cursor.peek_next().ok_or(ParseError::UnexpectedEOF)? == &Token::LeftArrow {
                cursor.next();
                Ok(Type::Function(
                    Box::new(t.clone()),
                    Box::new(parse_type(cursor)?),
                ))
            } else {
                Ok(t.clone())
            }
        }
        Token::LeftSquareBracket => {
            let inner_type = parse_type(cursor)?;
            cursor.expect_token(Token::RightSquareBracket)?;

            Ok(Type::List(Box::new(inner_type)))
        }
        _ => Err(ParseError::ExpectedType(next)),
    }
}
/// Parses a expresssion.
/// @param &mut cursor: the cursor to parse the expression from.
/// @return: the parsed AST Node.
///
/// The cursor must land on the start of the expression,
/// the cursor will land at the end of the expression.
///
/// Example:
/// start
///   v
///   1+2*3
///       ^
///      end
///
fn parse_expression(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    fn parse_binary_expression(
        cursor: &mut Cursor,
        precedence: usize,
    ) -> Result<ASTNode, ParseError> {
        let mut left = parse_primary_expression(cursor)?;

        // Loop to handle binary operators based on precedence
        while let Some(operator) = get_operator(cursor.peek()) {
            let operator_precedence = get_precedence(&operator);

            if operator_precedence <= precedence {
                break;
            }

            cursor.next(); // Consume the operator
            let right = parse_primary_expression(cursor)?;

            left = ASTNode::BinaryOperation(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    // Helper function to parse primary expressions (literals or parenthesized expressions)
    fn parse_primary_expression(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
        let current = cursor.peek_increment().ok_or(ParseError::UnexpectedEOF)?;
        match current {
            Token::IntegerLiteral(value) => Ok(ASTNode::Literal(Literal::Int(value))),
            Token::FloatLiteral(value) => Ok(ASTNode::Literal(Literal::Float(value))),
            Token::StringLiteral(value) => Ok(ASTNode::Literal(Literal::Str(value))),
            Token::LeftSquareBracket => parse_list(cursor),
            Token::Identifier(name) => {
                let next = cursor.peek().ok_or(ParseError::UnexpectedEOF)?;
                match next {
                    Token::Type(variable_type) => Ok(ASTNode::VariableUsage(
                        name.to_string(),
                        Some(variable_type.clone()),
                    )),
                    _ => Ok(ASTNode::VariableUsage(name.to_string(), None)),
                }
            }

            Token::True => Ok(ASTNode::Literal(Literal::True)),
            Token::False => Ok(ASTNode::Literal(Literal::False)),
            _ => {
                cursor.back();

                cursor.expect_token(Token::LeftParenthesis)?;
                let inner_expression = parse_expression(cursor);
                if cursor.peek().ok_or(ParseError::UnexpectedEOF)? == &Token::RightParenthesis {
                    cursor.next(); // Consume the right bracket
                    Ok(inner_expression?)
                } else {
                    Err(ParseError::MismatchedParens)
                }
            }
        }
    }

    // Helper function to get the precedence of an operator
    fn get_precedence(operator: &BinaryOperator) -> usize {
        match operator {
            BinaryOperator::Plus | BinaryOperator::Minus => 1,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => 2,

            BinaryOperator::And
            | BinaryOperator::Or
            | BinaryOperator::Not
            | BinaryOperator::Xor => 3,

            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual
            | BinaryOperator::Assign => 1,
        }
    }

    // Helper function to get the operator from a token
    fn get_operator(token: Option<&Token>) -> Option<BinaryOperator> {
        match token {
            Some(Token::Plus) => Some(BinaryOperator::Plus),
            Some(Token::Minus) => Some(BinaryOperator::Minus),
            Some(Token::Multiply) => Some(BinaryOperator::Multiply),
            Some(Token::Divide) => Some(BinaryOperator::Divide),
            Some(Token::Modulo) => Some(BinaryOperator::Modulo),

            Some(Token::And) => Some(BinaryOperator::And),
            Some(Token::Or) => Some(BinaryOperator::Or),
            Some(Token::Not) => Some(BinaryOperator::Not),
            Some(Token::Xor) => Some(BinaryOperator::Xor),

            Some(Token::Equal) => Some(BinaryOperator::Equal),
            Some(Token::NotEqual) => Some(BinaryOperator::NotEqual),
            Some(Token::LessThan) => Some(BinaryOperator::LessThan),
            Some(Token::LessThanOrEqual) => Some(BinaryOperator::LessThanOrEqual),
            Some(Token::GreaterThan) => Some(BinaryOperator::GreaterThan),
            Some(Token::GreaterThanOrEqual) => Some(BinaryOperator::GreaterThanOrEqual),
            Some(Token::Assign) => Some(BinaryOperator::Assign),
            // Add other operators as needed
            _ => None,
        }
    }
    parse_binary_expression(cursor, 0)
}

/// Parses a inline function.
/// @param &mut cursor: the cursor to parse the inline function from.
/// @return: a ASTNode of a function.
///
/// The cursor must line of the start of the function (the Fn keyword)
/// The cursor will land of the end of the function.
///
/// Examples:
///      start
///       vv
///  map |fn element = element + 1| arr
///                                  ^
///                                 end
///
fn parse_function(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    cursor.next();
    cursor.expect_token(Token::Fn)?;
    cursor.back();
    let mut function_params = Vec::new();
    while cursor.next().ok_or(ParseError::UnexpectedEOF)? != &Token::Assign {
        eprintln!("{:?}", cursor.peek());
        match cursor.peek().ok_or(ParseError::UnexpectedEOF)? {
            Token::Identifier(name) => {
                function_params.push(name.clone());
            }
            token => {
                return Err(ParseError::ExpectedVariable(
                    Token::Identifier(String::new()),
                    token.clone(),
                ));
            }
        }
    }
    cursor.next();
    let expression = parse_expression(cursor)?;
    Ok(ASTNode::InlineFunction(
        function_params,
        Box::new(expression),
    ))
}

fn parse_list(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    cursor.expect_token(Token::LeftSquareBracket)?;

    let mut elements = Vec::new();
    loop {
        let element = parse_expression(cursor)?;
        elements.push(element);

        if cursor.peek().ok_or(ParseError::UnexpectedEOF)? == &Token::RightSquareBracket {
            break;
        }
        if cursor.peek().ok_or(ParseError::UnexpectedEOF)? == &Token::Comma {
            cursor.expect_token(Token::Comma)?;
        }
    }

    cursor.expect_token(Token::RightSquareBracket)?;

    Ok(ASTNode::Literal(Literal::List(elements)))
}

fn parse_match_expression(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    fn parse_match_case(cursor: &mut Cursor) -> Result<ASTMatchArm, ParseError> {
        let pattern = if cursor.peek().ok_or(ParseError::UnexpectedEOF)? == &Token::Underscore {
            cursor.next();
            None
        } else {
            Some(parse_pattern(cursor)?)
        };

        cursor.expect_token(Token::FatArrow)?;
        let result = parse_expression(cursor)?;

        Ok(ASTMatchArm::MatchCondition(pattern, result))
    }
    fn parse_pattern(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
        parse_expression(cursor)
    }

    cursor.expect_token(Token::Match)?; // Expect the "match" keyword

    let expression_to_match = parse_expression(cursor)?;

    let mut match_cases = Vec::new();

    while cursor.peek_increment().ok_or(ParseError::UnexpectedEOF)? == Token::Pipe {
        match_cases.push(parse_match_case(cursor)?);

        println!("{:?}", cursor);
    }

    cursor.back();
    cursor.back();

    Ok(ASTNode::Match(Box::new(expression_to_match), match_cases))
}

fn parse_variable_delete(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    cursor.expect_token(Token::Delete)?;
    let variable_name = cursor.expect_variable()?;
    Ok(ASTNode::VariableDeletion(variable_name))
}
fn parse_tokens(cursor: &mut Cursor) -> Option<Result<ASTNode, ParseError>> {
    let next = cursor.next()?;
    Some(match next {
        Token::IntegerLiteral(_)
        | Token::FloatLiteral(_)
        | Token::StringLiteral(_)
        | Token::True
        | Token::False => parse_expression(cursor),

        Token::LeftSquareBracket => parse_list(cursor),
        Token::Match => parse_match_expression(cursor),
        _ => Err(ParseError::InvalidToken),
    })
}
pub fn parse(tokens: Vec<Token>) -> Result<Vec<ASTNode>, ParseError> {
    let mut nodes = Vec::new();
    let mut cursor = Cursor::new(tokens);
    while let Some(token) = parse_tokens(&mut cursor) {
        nodes.push(token?);
    }
    Ok(nodes)
}
