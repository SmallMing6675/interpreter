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
            Box::new(ASTNode::Literal(Literal::Int(1))),
            BinaryOperator::Plus,
            Box::new(ASTNode::BinaryOperation(
                Box::new(ASTNode::Literal(Literal::Int(2))),
                BinaryOperator::Multiply,
                Box::new(ASTNode::Literal(Literal::Int(3))),
            )),
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
    fn test_parse_unexpected_eof() {
        // Unexpected EOF
        let source_code = "1 +";
        let tokens = lex(source_code).unwrap();
        assert!(parse(tokens).is_err());
    }

    // #[test]
    // fn test_parse_mismatched_parentheses() {
    //     // Mismatched parentheses
    //     let source_code = "(1";
    //     let tokens = lex(source_code).unwrap();
    //     assert!(parse(tokens).is_err());
    // }
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
                Box::new(ASTNode::VariableUsage("print".to_string(), None)),
                Box::new(ASTNode::Literal(Literal::Str("x is 3".to_string()))),
            )),
            Some(Box::new(ASTNode::FunctionCall(
                Box::new(ASTNode::VariableUsage("print".to_string(), None)),
                Box::new(ASTNode::Literal(Literal::Str("x is not 3".to_string()))),
            ))),
        )];

        let tokens = lex(source_code).unwrap();
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_variable_declaration() {
        let source_code = "x = 3";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::VariableDeclaration(
            "x".to_string(),
            Box::new(ASTNode::Literal(Literal::Int(3))),
            Some(Type::Int),
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_variable_deletion() {
        let source_code = "del x";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::VariableDeletion("x".to_string())];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_function_call() {
        let source_code = "print \"Hello World\"";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::FunctionCall(
            Box::new(ASTNode::VariableUsage("print".to_string(), None)),
            Box::new(ASTNode::Literal(Literal::Str("Hello World".to_string()))),
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_type_declaration() {
        let source_code = "x: int = 3";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::VariableDeclaration(
            "x".to_string(),
            Box::new(ASTNode::Literal(Literal::Int(3))),
            Some(Type::Int),
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_if_expression_without_else() {
        let source_code = "if x == 3 then print \"x is 3\" end";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::If(
            Box::new(ASTNode::BinaryOperation(
                Box::new(ASTNode::VariableUsage("x".to_string(), None)),
                BinaryOperator::Equal,
                Box::new(ASTNode::Literal(Literal::Int(3))),
            )),
            Box::new(ASTNode::FunctionCall(
                Box::new(ASTNode::VariableUsage("print".to_string(), None)),
                Box::new(ASTNode::Literal(Literal::Str("x is 3".to_string()))),
            )),
            None,
        )];
        assert_eq!(parse(tokens).unwrap(), expected_ast);
    }

    #[test]
    fn test_list_type_declaration() {
        let source_code = "x: [int] = [3]";
        let tokens = lex(source_code).unwrap();
        let expected_ast = vec![ASTNode::VariableDeclaration(
            "x".to_string(),
            Box::new(ASTNode::Literal(Literal::List(vec![ASTNode::Literal(
                Literal::Int(3),
            )]))),
            Some(Type::List(Box::new(Type::Int))),
        )];
        let ast = parse(tokens).unwrap();
        assert_eq!(ast, expected_ast);
    }
    #[test]
    fn test_recursive_functions() {
        let source_code = "fib = fn x -> fib(x-1) + fib(x-2)";
        let tokens = lex(source_code).unwrap();
        let ast = parse(tokens).unwrap();
        panic!("{:#?}", ast);
    }
}
