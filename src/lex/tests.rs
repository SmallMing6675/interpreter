#[cfg(test)]
mod tests {
    use crate::lex::lex::lex;
    use crate::lex::lex::LexerError;
    use crate::lex::token::*;

    #[test]
    fn test_lex_valid_input() {
        let source_code = "1 + 2 * 3";
        let expected_tokens = vec![
            Token::IntegerLiteral(1),
            Token::Plus,
            Token::IntegerLiteral(2),
            Token::Multiply,
            Token::IntegerLiteral(3),
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_invalid_character() {
        let source_code = "1 + @ 2";
        let result = lex(source_code);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), LexerError::InvalidCharacter('@'));
    }

    #[test]
    fn test_lex_string_literal() {
        let source_code = r#"'Hello, World!'"#;
        let expected_tokens = vec![
            Token::StringLiteral("Hello, World!".to_string()),
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_comment() {
        let source_code = "# This is a comment \n 1 + 2";
        let expected_tokens = vec![
            Token::Comment(" This is a comment ".to_string()),
            Token::IntegerLiteral(1),
            Token::Plus,
            Token::IntegerLiteral(2),
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_number_literal() {
        let source_code = "123.456";
        let expected_tokens = vec![Token::FloatLiteral(123.456), Token::EOF];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_variable_assignment() {
        let source_code = "x = 42";
        let expected_tokens = vec![
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::IntegerLiteral(42),
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_variable_usage() {
        let source_code = "x + y";
        let expected_tokens = vec![
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }

    #[test]
    fn test_lex_list() {
        let source_code = "[1, 2, 3]";
        let expected_tokens = vec![
            Token::LeftSquareBracket,
            Token::IntegerLiteral(1),
            Token::Comma,
            Token::IntegerLiteral(2),
            Token::Comma,
            Token::IntegerLiteral(3),
            Token::RightSquareBracket,
            Token::EOF,
        ];
        assert_eq!(lex(source_code).unwrap(), expected_tokens);
    }
}
