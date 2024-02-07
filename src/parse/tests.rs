#[cfg(test)]
mod tests {
    use crate::lex::lex::lex;
    use crate::parse::ast::*;
    use crate::parse::parse::parse;

    #[test]
    fn test_parse_arithmetic_expression() {
        // Valid arithmetic expression
        let source_code = "1 + 2 * 3";
        let tokens = lex(source_code).unwrap();
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
        let source_code = "[1, 2]";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::Literal(Literal::List(vec![
            ASTNode::Literal(Literal::Int(1)),
            ASTNode::Literal(Literal::Int(2)),
        ]))];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_parse_match_expression() {
        // Valid match expression
        let source_code = "match 3 | 2 => even | _ => odd";
        let tokens = lex(source_code).unwrap();
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
        let source_code = "x";
        let tokens = lex(source_code).unwrap();
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn test_parse_unexpected_eof() {
        // Unexpected EOF
        let source_code = "1 +";
        let tokens = lex(source_code).unwrap();
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn test_parse_mismatched_parentheses() {
        // Mismatched parentheses
        let source_code = "(1";
        let tokens = lex(source_code).unwrap();
        assert!(parse(tokens).is_err());
    }
    #[test]
    fn test_if_expression() {
        let source_code = r#"
        if x == 3 then 
            print "x is 3"
        else 
            print "x is not 3"
        end
    "#;

        let expected_ast = vec![ASTNode::If(
            Box::new(ASTNode::BinaryOperation(
                Box::new(ASTNode::VariableUsage("x".to_string(), None)),
                BinaryOperator::Equal,
                Box::new(ASTNode::Literal(Literal::Int(3))),
            )),
            Box::new(ASTNode::FunctionCall(
                "print".to_string(),
                Box::new(ASTNode::Literal(Literal::Str("x is 3".to_string()))),
            )),
            Some(Box::new(ASTNode::FunctionCall(
                "print".to_string(),
                Box::new(ASTNode::Literal(Literal::Str("x is not 3".to_string()))),
            ))),
        )];

        let tokens = lex(source_code).unwrap();
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }
}
