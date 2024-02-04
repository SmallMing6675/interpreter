#[derive(Debug)]
pub struct Cursor {
    string: String,
    current: usize,
}

impl Cursor {
    pub fn new(string: String) -> Cursor {
        Cursor {
            string,
            current: usize::MAX,
        }
    }

    fn get_index(&self) -> usize {
        return self.current;
    }

    fn increment_index(&mut self) {
        if self.current == usize::MAX {
            self.current = 0;
        } else {
            self.current += 1;
        }
    }

    fn decrement_index(&mut self) {
        self.current -= 1;
    }

    /// increments the index and return the next character in the cursor.
    pub fn next(&mut self) -> Option<char> {
        self.increment_index();
        self.peek()
    }

    /// Peeks the current index and  increments the index.
    pub fn peek_increment(&mut self) -> Option<char> {
        let element = self.peek();
        self.increment_index();
        element
    }

    pub fn peek(&self) -> Option<char> {
        self.string.chars().nth(self.get_index())
    }

    /// Decrements the index and returns the previous character in the cursor.
    pub fn back(&mut self) -> Option<char> {
        self.decrement_index();
        self.peek()
    }

    /// Peek until the given condition is met.
    /// Example:
    /// @param condition The condition to be met.
    /// start
    ///   v
    ///   "Hello"
    ///         ^
    ///        End
    /// @return: The string being constructed.
    ///
    pub fn peek_until(&mut self, condition: fn(char) -> bool) -> String {
        self.back();
        let mut result = String::new();
        while let Some(character) = self.next() {
            if condition(character) {
                self.back();
                break;
            }
            result.push(character);
        }
        result
    }

    /// Globs the substring until the given condition is met.
    /// Example:
    ///
    /// @param condition The condition to be met.
    /// start
    ///   v
    ///   "Hello"
    ///         ^
    ///        End
    /// @return: The string being constructed.
    pub fn glob_until(&mut self, condition: fn(&str) -> bool) -> String {
        let mut result = String::new();
        while let Some(character) = self.next() {
            if condition(&result) {
                self.back();
                break;
            }
            result.push(character);
        }
        result
    }
}
