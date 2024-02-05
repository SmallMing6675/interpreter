#[cfg(test)]
mod tests {
    use crate::lex::token::Token;
    use crate::parse::ast::*;
    use crate::parse::parse::parse;

    #[test]
    fn test_parse_arithmetic_expression() {
        // Valid arithmetic expression
        let tokens = vec![
            Token::IntegerLiteral(1),
            Token::Plus,
            Token::IntegerLiteral(2),
            Token::Multiply,
            Token::IntegerLiteral(3),
        ];
        let expected_ast = vec![ASTNode::BinaryOperation(
            Box::new(ASTNode::BinaryOperation(
                Box::new(ASTNode::Literal(Literal::Int(1))),
                BinaryOperator::Plus,
                Box::new(ASTNode::Literal(Literal::Int(2))),
            )),
            BinaryOperator::Multiply,
            Box::new(ASTNode::Literal(Literal::Int(3))),
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_parse_list_creation() {
        // Valid list creation
        let tokens = vec![
            Token::LeftSquareBracket,
            Token::IntegerLiteral(1),
            Token::Comma,
            Token::IntegerLiteral(2),
            Token::RightSquareBracket,
        ];
        let expected_ast = vec![ASTNode::Literal(Literal::List(vec![
            ASTNode::Literal(Literal::Int(1)),
            ASTNode::Literal(Literal::Int(2)),
        ]))];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_parse_match_expression() {
        // Valid match expression
        let tokens = vec![
            Token::Match,
            Token::IntegerLiteral(3),
            Token::Pipe,
            Token::IntegerLiteral(2),
            Token::FatArrow,
            Token::Identifier("even".to_string()),
            Token::Pipe,
            Token::Underscore,
            Token::FatArrow,
            Token::Identifier("odd".to_string()),
        ];
        let expected_ast = vec![ASTNode::Match(
            Box::new(ASTNode::Literal(Literal::Int(3))),
            vec![
                ASTMatchArm::MatchCondition(
                    Some(ASTNode::Literal(Literal::Int(2))),
                    ASTNode::VariableUsage("even".to_string(), None),
                ),
                ASTMatchArm::MatchCondition(None, ASTNode::VariableUsage("odd".to_string(), None)),
            ],
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_parse_invalid_token_sequence() {
        // Invalid token sequence
        let tokens = vec![Token::Identifier("x".to_string())];
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn test_parse_unexpected_eof() {
        // Unexpected EOF
        let tokens = vec![Token::IntegerLiteral(1), Token::Plus];
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn test_parse_mismatched_parentheses() {
        // Mismatched parentheses
        let tokens = vec![Token::LeftParenthesis, Token::IntegerLiteral(1)];
        assert!(parse(tokens).is_err());
    }
}
