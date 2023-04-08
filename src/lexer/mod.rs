use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

/// Returns true if character is recognized whitespace.
#[inline(always)]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

/// Lexer/tokenizer implementation
pub struct Lexer<'a> {
    pub code: &'a str,
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from a code string slice.
    ///
    /// # Arguments
    ///
    /// * `code` - A string slice representing a program
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::lexer::Lexer;
    /// 
    /// let code = String::from("5 + 5");
    /// let lexer = Lexer::new(&code[..]);
    /// ```
    pub fn new(code: &'a str) -> Self {
        Lexer {
            code,
            chars: code.chars().peekable(),
            line: 0 as usize,
            column: 0 as usize,
        }
    }

    /// Private method to fetch next non-whitespace character.
    fn read_char(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
            if !is_whitespace(c) {
                Some(c)
            } else {
                None
            }
        } else {
            Some('\0')
        }
    }

    /// Private method to create a new Token instance at the current position.
    fn create_token(&self, token_type: TokenType, length: usize) -> Option<Token> {
        let column = self.column.checked_sub(length);
        if column.is_none() {
            panic!("Lexer error: Negative column detected");
        }
        Some(Token {
            token_type,
            line: self.line,
            column: column.unwrap(),
            length,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut opt = self.read_char();
        while opt.is_none() {
            opt = self.read_char();
        }

        let c = opt.unwrap();
        match c {
            '=' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::Equals, 2)
                } else {
                    self.create_token(TokenType::Assign, 1)
                }
            }
            '!' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::NotEquals, 2)
                } else {
                    self.create_token(TokenType::Bang, 1)
                }
            }
            '+' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::PlusEquals, 2)
                } else {
                    self.create_token(TokenType::Plus, 1)
                }
            }
            '-' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::MinusEquals, 2)
                } else {
                    self.create_token(TokenType::Minus, 1)
                }
            }
            '*' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::AsteriskEquals, 2)
                } else {
                    self.create_token(TokenType::Asterisk, 1)
                }
            }
            '/' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::SlashEquals, 2)
                } else if self.chars.peek() == Some(&'/') {
                    self.read_char();
                    self.create_token(TokenType::Comment, 2)
                } else {
                    self.create_token(TokenType::Slash, 1)
                }
            }
            '%' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::PercentEquals, 2)
                } else {
                    self.create_token(TokenType::Percent, 1)
                }
            }
            '>' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::GreaterThanEquals, 2)
                } else {
                    self.create_token(TokenType::GreaterThan, 1)
                }
            }
            '<' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::LessThanEquals, 2)
                } else {
                    self.create_token(TokenType::LessThan, 1)
                }
            }
            '|' => {
                if self.chars.peek() == Some(&'|') {
                    self.read_char();
                    self.create_token(TokenType::Or, 2)
                } else {
                    self.create_token(TokenType::Illegal, 1)
                }
            }
            '&' => {
                if self.chars.peek() == Some(&'&') {
                    self.read_char();
                    self.create_token(TokenType::And, 2)
                } else {
                    self.create_token(TokenType::Illegal, 1)
                }
            }
            '(' => self.create_token(TokenType::Lparen, 1),
            ')' => self.create_token(TokenType::Rparen, 1),
            '[' => self.create_token(TokenType::Lbracket, 1),
            ']' => self.create_token(TokenType::Rbracket, 1),
            '{' => self.create_token(TokenType::Lbrace, 1),
            '}' => self.create_token(TokenType::Rbrace, 1),
            ',' => self.create_token(TokenType::Comma, 1),
            ';' => self.create_token(TokenType::Semicolon, 1),
            ':' => {
                if self.chars.peek() == Some(&'=') {
                    self.read_char();
                    self.create_token(TokenType::Declare, 2)
                } else {
                    self.create_token(TokenType::Illegal, 1)
                }
            }
            '\0' => None,
            _ => {
                let mut ident: Vec<char> = vec![c];
                let mut legal = true;
                if c.is_digit(10) {
                    while let Some(n) = self.chars.peek() {
                        if n.is_alphabetic() {
                            legal = false;
                            ident.push(self.read_char().unwrap());
                        } else if n.is_digit(10) {
                            ident.push(self.read_char().unwrap());
                        } else {
                            break;
                        }
                    }
                    let len = ident.len();
                    let mut token_type = TokenType::Illegal;
                    if legal {
                        token_type = TokenType::Integer(
                            ident
                                .into_iter()
                                .collect::<String>()
                                .parse::<i32>()
                                .unwrap(),
                        );
                    }
                    self.create_token(token_type, len)
                } else if c.is_alphabetic() {
                    while let Some(n) = self.chars.peek() {
                        if n.is_alphanumeric() {
                            ident.push(self.read_char().unwrap());
                        } else {
                            break;
                        }
                    }
                    let len = ident.len();
                    let ident: String = ident.into_iter().collect();
                    let token_type = match &ident[..] {
                        "fn" => TokenType::Function,
                        "gen" => TokenType::Generator,
                        "seq" => TokenType::Sequence,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "return" => TokenType::Return,
                        "yield" => TokenType::Yield,
                        "true" => TokenType::True,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "in" => TokenType::In,
                        "while" => TokenType::While,
                        "loop" => TokenType::Loop,
                        "continue" => TokenType::Continue,
                        "break" => TokenType::Break,
                        "note" => TokenType::Note,
                        "rest" => TokenType::Rest,
                        "cc" => TokenType::Control,
                        "next" => TokenType::Next,
                        _ => TokenType::Identifier(ident),
                    };
                    self.create_token(token_type, len)
                } else {
                    self.create_token(TokenType::Illegal, 1)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::lexer::{TokenType};

    #[test]
    fn test_next_token() {
        let input = "varA =+ += -=
        * *= 1a /= (55 - 44){}
        a := 3;
        abc := fn(x, y)
        true && false;";
        let expected: Vec<TokenType> = vec![
            TokenType::Identifier(String::from("varA")),
            TokenType::Assign,
            TokenType::Plus,
            TokenType::PlusEquals,
            TokenType::MinusEquals,
            TokenType::Asterisk,
            TokenType::AsteriskEquals,
            TokenType::Illegal,
            TokenType::SlashEquals,
            TokenType::Lparen,
            TokenType::Integer(55_i32),
            TokenType::Minus,
            TokenType::Integer(44_i32),
            TokenType::Rparen,
            TokenType::Lbrace,
            TokenType::Rbrace,
            TokenType::Identifier(String::from("a")),
            TokenType::Declare,
            TokenType::Integer(3_i32),
            TokenType::Semicolon,
            TokenType::Identifier(String::from("abc")),
            TokenType::Declare,
            TokenType::Function,
            TokenType::Lparen,
            TokenType::Identifier(String::from("x")),
            TokenType::Comma,
            TokenType::Identifier(String::from("y")),
            TokenType::Rparen,
            TokenType::True,
            TokenType::And,
            TokenType::False,
        ];

        let mut lexer = Lexer::new(input);
        for token in expected.iter() {
            let next_token = lexer.next();
            assert!(!next_token.is_none());
            assert_eq!(&next_token.unwrap().token_type, token);
        }
    }
}
