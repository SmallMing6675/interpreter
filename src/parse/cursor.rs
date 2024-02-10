use crate::lex::token::Token;
use crate::parse::parse::ParseError;

#[derive(Debug)]
pub struct Cursor {
    tokens: Vec<Token>,
    current: usize,
}

impl Cursor {
    pub fn new(tokens: Vec<Token>) -> Cursor {
        Cursor {
            tokens,
            current: usize::MAX,
        }
    }

    pub fn get_index(&self) -> usize {
        return self.current;
    }

    pub fn set_index(&mut self, index: usize) {
        self.current = index;
    }

    fn increment_index(&mut self) {
        if self.current == usize::MAX {
            self.current = 0;
        } else {
            self.current += 1;
        }
    }

    fn decrement_index(&mut self) {
        if self.current == 0 {
            self.current = usize::MAX;
        } else {
            self.current -= 1;
        }
    }

    /// increments the index and return the next token in the cursor.
    pub fn next(&mut self) -> Option<&Token> {
        self.increment_index();

        self.peek()
    }

    pub fn get_next(&mut self) -> Result<&Token, ParseError> {
        self.increment_index();
        match self.peek().ok_or(ParseError::UnexpectedEOF)? {
            Token::EOF => Err(ParseError::EOF),
            token => Ok(token),
        }
    }

    /// Peeks the current index and  increments the index.
    pub fn peek_increment(&mut self) -> Option<Token> {
        if let Some(element) = self.peek().cloned() {
            self.increment_index();
            Some(element)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.get_index())
    }

    /// Decrements the index and returns the previous token in the cursor.
    pub fn back(&mut self) -> Option<&Token> {
        self.decrement_index();
        self.peek()
    }

    /// Peek until the given condition is met.
    /// Example:
    /// @param condition The condition to be met.
    /// start
    ///   v
    ///   T:String("Hello") T:Assign T:Int(4)
    ///                                     ^
    ///                                    End
    /// @return: The Vec Tokens being globbed
    ///
    pub fn peek_until(&mut self, condition: fn(&Token) -> bool) -> Vec<Token> {
        self.back();
        let mut result = Vec::new();
        while let Some(token) = self.next() {
            if condition(token) {
                self.back();
                break;
            }
            result.push(token.clone());
        }
        result
    }

    /// Globs the tokens until the given condition is met.
    /// Example:
    ///
    /// @param condition The condition to be met.
    /// start
    ///   v
    ///   T:String("Hello") T:Assign T:Int(4)
    ///                                     ^
    ///                                    End
    /// @return: The tokens being constructed.
    pub fn glob_until(&mut self, condition: fn(Vec<Token>) -> bool) -> Vec<Token> {
        let mut result = Vec::new();
        while let Some(token) = self.next() {
            if condition(result.clone()) {
                self.back();
                break;
            }
            result.push(token.clone());
        }
        result
    }
    pub fn expect_token(&mut self, expected: Token) -> Result<(), ParseError> {
        let current = self.peek().ok_or(ParseError::UnexpectedEOF)?;
        if current == &expected {
            self.next(); // Consume the expected token
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(expected))
        }
    }

    pub fn peek_next(&mut self) -> Option<&Token> {
        self.tokens.get(self.get_index() + 1)
    }

    pub fn expect_variable(&mut self) -> Result<String, ParseError> {
        match self.next() {
            Some(Token::Identifier(name)) => Ok(name.to_string()),
            Some(unexpected_token) => Err(ParseError::ExpectedVariable(
                Token::Identifier(String::new()),
                unexpected_token.clone(),
            )),
            None => Err(ParseError::UnexpectedToken(Token::EOF)),
        }
    }
}
