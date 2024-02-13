use crate::lex::token::Token;
use crate::parse::ast::*;
use crate::parse::cursor::Cursor;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    MismatchedParens,
    ExpectedType(Token),
    ExpectedVariable(Token, Token),
    InvalidToken(Token),
    InvalidAssign(Token),
    UnexpectedEOF,
    EOF,

    Empty,
    Underflow,
    ExpectedIdentifier,
    NotAFunction,
    InvalidExpression,
    EmptyFunctionCall,
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
    if cursor.peek() == Some(&Token::Fn) {
        return parse_function(cursor);
    }
    fn parse_binary_expression(
        cursor: &mut Cursor,
        precedence: usize,
    ) -> Result<ASTNode, ParseError> {
        let mut left = parse_primary_expression(cursor)?;
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
            Token::LeftSquareBracket => {
                cursor.back();
                parse_list(cursor)
            }
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
                if cursor.peek().ok_or(ParseError::UnexpectedEOF)? != &Token::LeftParenthesis {
                    return Err(ParseError::InvalidExpression);
                }
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
    let mut function_params = Vec::new();

    while cursor.get_next()? != &Token::Assign {
        match cursor.peek().ok_or(ParseError::UnexpectedEOF)? {
            Token::Identifier(name) => function_params.push(Parameter {
                identifier: name.to_string(),
                type_: None,
            }),
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
/// Parses a list like expression.
/// start
///   v
///   [1,2,3]
///         ^
///        end
///
fn parse_list(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    let mut elements = Vec::new();

    cursor.next();
    loop {
        let element = parse_expression(cursor).unwrap();
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

    cursor.expect_token(Token::Match)?;
    let expression_to_match = parse_expression(cursor)?;

    let mut match_cases = Vec::new();

    while cursor.peek_increment() == Some(Token::Pipe) {
        match cursor.peek_next() {
            Some(Token::Pipe) => break,
            Some(_) => match_cases.push(parse_match_case(cursor)?),
            None => return Err(ParseError::UnexpectedEOF),
        }
    }

    Ok(ASTNode::Match(Box::new(expression_to_match), match_cases))
}

fn parse_variable_delete(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    cursor.expect_token(Token::Del)?;
    cursor.back();
    let variable_name = cursor.expect_variable()?;
    Ok(ASTNode::VariableDeletion(variable_name))
}

fn parse_if_expression(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    cursor.expect_token(Token::If)?;
    let condition = parse_expression(cursor)?;
    cursor.expect_token(Token::Then)?;
    cursor.back();

    let then_block = parse_tokens(cursor).ok_or(ParseError::UnexpectedEOF)??;
    if cursor.expect_token(Token::Else).is_ok() {
        cursor.back();

        let else_block = parse_tokens(cursor).ok_or(ParseError::UnexpectedEOF)??;
        let _ = cursor.expect_token(Token::End);
        return Ok(ASTNode::If(
            Box::new(condition),
            Box::new(then_block),
            Some(Box::new(else_block)),
        ));
    }

    cursor.expect_token(Token::End)?;
    cursor.back();

    Ok(ASTNode::If(Box::new(condition), Box::new(then_block), None))
}

fn parse_function_call(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    fn parse_argument_list(cursor: &mut Cursor) -> Result<Vec<ASTNode>, ParseError> {
        let mut arguments = Vec::new();
        cursor.back();

        loop {
            match cursor.next().ok_or(ParseError::UnexpectedEOF)? {
                Token::Identifier(identifier) => {
                    arguments.push(ASTNode::VariableUsage(identifier.to_string(), None))
                }
                Token::IntegerLiteral(number) => {
                    arguments.push(ASTNode::Literal(Literal::Int(*number)))
                }
                Token::FloatLiteral(number) => {
                    arguments.push(ASTNode::Literal(Literal::Float(*number)))
                }

                Token::StringLiteral(string) => {
                    arguments.push(ASTNode::Literal(Literal::Str(string.to_string())))
                }

                _ => {
                    return Ok(arguments);
                }
            }
        }
    }

    let arguments = parse_argument_list(cursor)?;

    // Modify this function  so that it parse `add 2 3` as fcall(fcall(add, 2), 3)
    // add_three 3 4 5 -> fcall(fcall(fcall(add_three, 3), 4), 5)
    fn build_function_call_chain(arguments: Vec<ASTNode>) -> Result<ASTNode, ParseError> {
        match arguments.len() {
            1 => Ok(arguments[0].clone()),
            _ => Ok(ASTNode::FunctionCall(
                Box::new(build_function_call_chain(
                    arguments[..arguments.len() - 1].to_vec(),
                )?),
                Box::new(arguments.last().unwrap().clone()),
            )),
        }
    }
    let node = build_function_call_chain(arguments)?;
    return Ok(node);
}

fn parse_identifier(cursor: &mut Cursor) -> Result<ASTNode, ParseError> {
    let identifier = match cursor.peek().ok_or(ParseError::UnexpectedEOF)? {
        Token::Identifier(identifier) => String::from(identifier),
        token => return Err(ParseError::InvalidToken(token.clone())),
    };

    match cursor.next().ok_or(ParseError::UnexpectedEOF)? {
        Token::Assign => {
            cursor.next();
            let result = parse_expression(cursor)?;
            cursor.back();
            Ok(ASTNode::VariableDeclaration(
                identifier,
                Box::new(result),
                None,
            ))
        }

        Token::Colon => {
            cursor.next();

            let type_ = parse_type(cursor)?;
            match cursor.peek().ok_or(ParseError::UnexpectedEOF)? {
                Token::Assign => {
                    cursor.next();

                    let result = parse_expression(cursor)?;
                    cursor.back();
                    Ok(ASTNode::VariableDeclaration(
                        identifier,
                        Box::new(result),
                        Some(type_),
                    ))
                }
                _ => return Err(ParseError::UnexpectedToken(Token::Assign)),
            }
        }

        Token::Plus
        | Token::Minus
        | Token::Multiply
        | Token::Divide
        | Token::And
        | Token::Or
        | Token::Not
        | Token::Xor
        | Token::Equal
        | Token::NotEqual
        | Token::Modulo
        | Token::LessThan
        | Token::GreaterThan
        | Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => {
            let left = ASTNode::VariableUsage(identifier.clone(), None); // Create ASTNode for variable usage
            let operator = match cursor.peek().ok_or(ParseError::UnexpectedEOF)? {
                Token::Plus => BinaryOperator::Plus,
                Token::Minus => BinaryOperator::Minus,
                Token::Multiply => BinaryOperator::Multiply,
                Token::Divide => BinaryOperator::Divide,
                Token::And => BinaryOperator::And,

                Token::Or => BinaryOperator::Or,
                Token::Not => BinaryOperator::Not,
                Token::Xor => BinaryOperator::Xor,
                Token::Equal => BinaryOperator::Equal,
                Token::NotEqual => BinaryOperator::NotEqual,
                Token::Modulo => BinaryOperator::Modulo,
                Token::LessThan => BinaryOperator::LessThan,
                Token::GreaterThan => BinaryOperator::GreaterThan,
                Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
                Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
                _ => unreachable!(), // These tokens are already handled by the match arm
            };
            cursor.next();

            let right = parse_expression(cursor)?;
            cursor.back();
            Ok(ASTNode::BinaryOperation(
                Box::new(left),
                operator,
                Box::new(right),
            ))
        }

        Token::EOF => Ok(ASTNode::VariableUsage(identifier, None)),
        _ => {
            cursor.back();
            parse_function_call(cursor)
        }
    }
}
fn parse_tokens(cursor: &mut Cursor) -> Option<Result<ASTNode, ParseError>> {
    let next = cursor.next()?;
    Some(match next {
        Token::IntegerLiteral(_)
        | Token::FloatLiteral(_)
        | Token::StringLiteral(_)
        | Token::True
        | Token::False => parse_expression(cursor),

        Token::If => parse_if_expression(cursor),
        Token::Fn => parse_function(cursor),
        Token::Del => parse_variable_delete(cursor),
        Token::Identifier(_) => parse_identifier(cursor),

        Token::LeftSquareBracket => parse_list(cursor),
        Token::Match => parse_match_expression(cursor),
        Token::EOF => Err(ParseError::EOF),
        Token::Newline | Token::Comment(_) => Err(ParseError::Empty),
        token => Err(ParseError::InvalidToken(token.clone())),
    })
}
pub fn parse(tokens: Vec<Token>) -> Result<Vec<ASTNode>, ParseError> {
    let mut nodes = Vec::new();

    let mut cursor = Cursor::new(tokens);

    while let Some(token) = parse_tokens(&mut cursor) {
        if token == Err(ParseError::EOF) {
            break;
        }
        if token == Err(ParseError::Empty) {
            continue;
        }

        nodes.push(token?);
    }
    Ok(nodes)
}
