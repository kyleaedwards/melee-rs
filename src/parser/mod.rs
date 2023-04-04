use std::fmt;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use crate::ast;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Nil = 1,
    Or,
    And,
    Assign,
    Equals,
    Compare,
    Add,
    Multiply,
    Prefix,
    Function,
    Index,
    Error,
}

fn get_token_precedence(token: &Token) -> Precedence {
    match token.token_type {
        TokenType::Assign => Precedence::Assign,
        TokenType::PlusEquals => Precedence::Assign,
        TokenType::MinusEquals => Precedence::Assign,
        TokenType::AsteriskEquals => Precedence::Assign,
        TokenType::SlashEquals => Precedence::Assign,
        TokenType::PercentEquals => Precedence::Assign,
        TokenType::Or => Precedence::Or,
        TokenType::And => Precedence::And,
        TokenType::Equals => Precedence::Equals,
        TokenType::NotEquals => Precedence::Equals,
        TokenType::LessThan => Precedence::Compare,
        TokenType::LessThanEquals => Precedence::Compare,
        TokenType::GreaterThan => Precedence::Compare,
        TokenType::GreaterThanEquals => Precedence::Compare,
        TokenType::Plus => Precedence::Add,
        TokenType::Minus => Precedence::Add,
        TokenType::Asterisk => Precedence::Multiply,
        TokenType::Slash => Precedence::Multiply,
        TokenType::Percent => Precedence::Multiply,
        TokenType::Lparen => Precedence::Function,
        TokenType::Lbracket => Precedence::Index,
        TokenType::Identifier(_) => Precedence::Error,
        TokenType::Integer(_) => Precedence::Error,
        TokenType::Note => Precedence::Error,
        TokenType::Rest => Precedence::Error,
        TokenType::Control => Precedence::Error,
        _ => Precedence::Nil,
    }
}

pub struct ParserError {
    pub message: String,
    pub token: Option<Token>,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.message)
    }
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    curr: Option<Token>,
    peek: Option<Token>,
    pub errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            curr: None,
            peek: None,
            errors: Vec::new(),
        };
        p.step();
        p.step();
        p
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };
        while !self.curr.is_none() {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.step();
        }
        program
    }

    fn parse_statement_block(&mut self) -> Vec<ast::Statement> {
        let mut statements = Vec::new();
        while !self.curr.is_none() {
            if &self.curr.as_ref().unwrap().token_type == &TokenType::Rbrace {
                break;
            }
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.step();
        }
        statements
    }

    /* Statement Parsers */

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        self.skip_semicolons();
        if self.curr.is_none() {
            return None
        }
        let mut require_semicolon = true;
        let output = match &self.curr.as_ref().unwrap().token_type {
            TokenType::Return => self.parse_return_statement(),
            TokenType::Yield => self.parse_yield_statement(),
            TokenType::Continue => self.parse_continue_statement(),
            TokenType::Break => self.parse_break_statement(),
            TokenType::Identifier(ident) => {
                if self.peek_is(TokenType::Declare) {
                    self.parse_declare_statement(ident.clone())
                } else {
                    self.parse_expression_statement()
                }
            },
            TokenType::For => {
                require_semicolon = false;
                self.parse_for_statement()
            },
            TokenType::While => {
                require_semicolon = false;
                self.parse_while_statement()
            },
            TokenType::Loop => {
                require_semicolon = false;
                self.parse_while_statement()
            },
            TokenType::If => {
                require_semicolon = false;
                self.parse_conditional_statement()
            },
            _ => self.parse_expression_statement(),
        };

        // Currently expression statements require a semicolon.
        if require_semicolon {
            let _ = self.expect_peek(TokenType::Semicolon);
        }

        output
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        if self.peek_is(TokenType::Semicolon) {
            self.step();
            Some(ast::Statement::Return { token, value: None })
        } else {
            self.step();
            let value = if let Some(val) = self.parse_expression(Precedence::Nil) {
                Some(Box::new(val))
            } else {
                None
            };
            Some(ast::Statement::Return { token, value })
        }
    }

    fn parse_yield_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        if self.peek_is(TokenType::Semicolon) {
            self.step();
            Some(ast::Statement::Yield { token, value: None })
        } else {
            self.step();
            let value = if let Some(val) = self.parse_expression(Precedence::Nil) {
                Some(Box::new(val))
            } else {
                None
            };
            Some(ast::Statement::Yield { token, value })
        }
    }

    fn parse_continue_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        Some(ast::Statement::Continue{ token })
    }

    fn parse_break_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        Some(ast::Statement::Break{ token })
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let expr = self.parse_expression(Precedence::Nil)?;
        Some(ast::Statement::Expression(Box::new(expr)))
    }

    fn parse_declare_statement(&mut self, ident: String) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        let name = ast::Identifier {
            token: Token {
                token_type: TokenType::Identifier(ident.clone()),
                line: token.line,
                length: token.length,
                column: token.column,
            },
            value: ident
        };
        self.step();
        let token = self.curr.take().unwrap();
        self.step();
        let value = Box::new(self.parse_expression(Precedence::Nil)?);
        Some(ast::Statement::Declare { token, name, value })
    }

    fn parse_for_statement(&mut self) -> Option<ast::Statement> {
        let for_token = self.curr.take().unwrap();
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        self.step();
        let token = self.curr.take().unwrap();
        let identifier = if let TokenType::Identifier(ref ident) = token.token_type {
            ast::Identifier {
                token: Token {
                    token_type: TokenType::Identifier(ident.clone()),
                    line: token.line,
                    length: token.length,
                    column: token.column,
                },
                value: ident.clone()
            }
        } else {
            panic!("Must follow pattern for (identifier in collection) {{");
        };
        if !self.expect_peek(TokenType::In) {
            return None;
        }
        self.step();
        let collection = Box::new(self.parse_expression(Precedence::Nil)?);
        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }
        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }
        self.step();
        let block = self.parse_statement_block();
        Some(ast::Statement::For { token: for_token, identifier, collection, block })
    }

    fn parse_while_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        if token.token_type == TokenType::Loop {
            if !self.expect_peek(TokenType::Lbrace) {
                return None;
            }
            self.step();
            let block = self.parse_statement_block();
            let null_token = Token { token_type: TokenType::True, line: 0, column: 0, length: 0 };
            let condition = Box::new(ast::Expression::Boolean{token: null_token, value: true});
            return Some(ast::Statement::While { token, condition, block })
        }
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        self.step();
        let condition = Box::new(self.parse_expression(Precedence::Nil)?);
        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }
        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }
        self.step();
        let block = self.parse_statement_block();
        Some(ast::Statement::While { token, condition, block })
    }

    fn parse_conditional_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr.take().unwrap();
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        self.step();
        let condition = Box::new(self.parse_expression(Precedence::Nil)?);
        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }
        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }
        self.step();
        let consequence = self.parse_statement_block();
        let mut alternative = None;
        if self.peek_is(TokenType::Else) { // } else
            self.step(); // else 
            if self.peek_is(TokenType::If) {
                self.step();
                alternative = Some(vec![self.parse_conditional_statement()?]);
            } else {
                if !self.expect_peek(TokenType::Lbrace) {
                    return None;
                }
                self.step();
                alternative = Some(self.parse_statement_block());
            }
        }
        return Some(ast::Statement::Conditional {
            token,
            condition,
            consequence,
            alternative,
        });
    }

    /* Expression Parsers */

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        if self.curr.is_none() {
            return None;
        }
        let res: Result<Option<ast::Expression>, ()> = self.parse_prefix_expression();
        if res.is_err() {
            let token = self.curr.take().unwrap();
            let message = format!("Unexpected token: {}", token.token_type);
            self.errors.push(ParserError {
                token: Some(token),
                message,
            });
            return None;
        }
        let mut curr = res.unwrap();

        loop {
            if self.curr.is_none() || self.peek_is(TokenType::Semicolon) {
                break;
            }
            if precedence >= get_token_precedence(self.peek.as_ref().unwrap()) {
                break;
            }
            // let left = curr.take();
            // let token = self.
        }
        curr
    }

    fn parse_prefix_expression(&mut self) -> Result<Option<ast::Expression>, ()> {
        let token_type = &self.curr.as_ref().unwrap().token_type;
        match token_type {
            TokenType::Identifier(ident) => Ok(self.parse_identifier(ident.clone())),
            TokenType::True => Ok(self.parse_boolean(true)),
            TokenType::False => Ok(self.parse_boolean(false)),
            TokenType::Integer(value) => Ok(self.parse_integer(*value)),
            TokenType::Plus => Ok(self.parse_prefix_operator(ast::Prefix::Plus)),
            TokenType::Bang => Ok(self.parse_prefix_operator(ast::Prefix::Not)),
            TokenType::Minus => Ok(self.parse_prefix_operator(ast::Prefix::Minus)),
            TokenType::Next => Ok(self.parse_next()),
            TokenType::Lparen => Ok(self.parse_parenthetical_expression()),
            _ => Ok(None),
        }
    }

    fn parse_integer(&mut self, value: i64) -> Option<ast::Expression> {
        let token = self.curr.take()?;
        Some(ast::Expression::Integer {
            token,
            value,
        })
    }

    fn parse_boolean(&mut self, value: bool) -> Option<ast::Expression> {
        let token = self.curr.take()?;
        Some(ast::Expression::Boolean { token, value })
    }

    fn parse_identifier(&mut self, value: String) -> Option<ast::Expression> {
        let token = self.curr.take()?;
        Some(ast::Expression::Identifier(ast::Identifier {
            token,
            value,
        }))
    }

    fn parse_next(&mut self) -> Option<ast::Expression> {
        let token = self.curr.take()?;
        self.step();
        let value = self.parse_expression(Precedence::Nil)?;
        Some(ast::Expression::Next {
            token,
            value: Box::new(value),
        })
    }

    fn parse_prefix_operator(&mut self, operator: ast::Prefix) -> Option<ast::Expression> {
        let token = self.curr.take()?;
        self.step();
        let right = self.parse_expression(Precedence::Prefix)?;
        Some(ast::Expression::Prefix {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_expression_list(&mut self, end_token: TokenType) -> Option<Vec<Box<ast::Expression>>> {
        let mut collection: Vec<Box<ast::Expression>> = Vec::new();
        let mut token_type: &TokenType;
        loop {
            if self.curr.is_none() { break; }
            token_type = &self.curr.as_ref().unwrap().token_type;
            if token_type == &end_token { break; }
            let expr = self.parse_expression(Precedence::Nil);
            if expr.is_none() {
                return None;
            }
            collection.push(Box::new(expr.unwrap()));
        }
        Some(collection)
    }

    fn parse_parenthetical_expression(&mut self) -> Option<ast::Expression> {
        self.step();
        let expr = self.parse_expression(Precedence::Nil);
        let token_type = &self.peek.as_ref()?.token_type;
        if token_type != &TokenType::Rparen {
            self.errors.push(ParserError {
                token: None,
                message: format!("Expected {}, received {}", ')', token_type),
            });
            return None;
        }
        self.step();
        expr
    }

    /* Utilities */

    pub fn step(&mut self) {
        let mut is_comment = self.peek_is(TokenType::Comment);
        let mut is_illegal = self.peek_is(TokenType::Illegal);
        while is_comment || is_illegal {
            self.curr = self.peek.take();
            self.peek = self.lexer.next();
            if is_illegal {
                let token = self.curr.take().unwrap();
                let message = format!("Unexpected token {}", token.token_type);
                self.errors.push(ParserError { message, token: Some(token) });
            }
            is_comment = self.peek_is(TokenType::Comment);
            is_illegal = self.peek_is(TokenType::Illegal);
        }
        self.curr = self.peek.take();
        self.peek = self.lexer.next();
    }

    pub fn peek_is(&self, token_type: TokenType) -> bool {
        !self.peek.is_none() && self.peek.as_ref().unwrap().token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        let matches = !self.peek.is_none() && self.peek.as_ref().unwrap().token_type == token_type;
        if !matches {
            self.errors.push(ParserError { message: format!("Expected token {}", token_type), token: None });
            false
        } else {
            self.step();
            true
        }
    }

    fn skip_semicolons(&mut self) {
        while !self.curr.is_none() && self.curr.as_ref().unwrap().token_type == TokenType::Semicolon {
            self.step();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast::{Statement, Expression, Prefix};

    #[test]
    fn test_integer() {
        let input = "1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Expression(expr) = stmt {
            expr
        } else {
            panic!("Expression statement parsed incorrectly");
        };

        let int = if let Expression::Integer { value, .. } = **exprs {
            value
        } else {
            panic!("Integer expression parsed incorrectly");
        };

        assert_eq!(int, 1_i64);
    }

    #[test]
    fn test_boolean() {
        let input = "false;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Expression(expr) = stmt {
            expr
        } else {
            panic!("Expression statement parsed incorrectly");
        };

        let b = if let Expression::Boolean { value, .. } = **exprs {
            value
        } else {
            panic!("Boolean expression parsed incorrectly");
        };

        assert_eq!(b, false);
    }

    #[test]
    fn test_identifier() {
        let input = "var;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Expression(expr) = stmt {
            expr
        } else {
            panic!("Expression statement parsed incorrectly");
        };

        if let Expression::Identifier(ident) = &**exprs {
            assert_eq!(*ident.value, String::from("var"));
        } else {
            panic!("Identifier expression parsed incorrectly");
        }
    }

    #[test]
    fn test_next() {
        let input = "next 1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Expression(expr) = stmt {
            expr
        } else {
            panic!("Expression statement parsed incorrectly");
        };

        assert!(match **exprs {
            Expression::Next { .. } => true,
            _ => false,
        });
    }

    #[test]
    fn test_prefix() {
        let input = "-1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Expression(expr) = stmt {
            expr
        } else {
            panic!("Expression statement parsed incorrectly");
        };

        assert!(match **exprs {
            Expression::Prefix { operator: Prefix::Minus, .. } => true,
            _ => false,
        });
    }

    #[test]
    fn test_return_statement() {
        let input = "return 1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Return{ value, .. } = stmt {
            if let Some(val) = value {
                val
            } else {
                panic!("Return statement parsed incorrectly");
            }
        } else {
            panic!("Return statement parsed incorrectly");
        };

        let int = if let Expression::Integer { value, .. } = **exprs {
            value
        } else {
            panic!("Integer expression parsed incorrectly");
        };

        assert_eq!(int, 1_i64);
    }

    #[test]
    fn test_yield_statement() {
        let input = "yield 1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        let exprs = if let Statement::Yield{ value, .. } = stmt {
            if let Some(val) = value {
                val
            } else {
                panic!("Yield statement parsed incorrectly");
            }
        } else {
            panic!("Yield statement parsed incorrectly");
        };

        let int = if let Expression::Integer { value, .. } = **exprs {
            value
        } else {
            panic!("Integer expression parsed incorrectly");
        };

        assert_eq!(int, 1_i64);
    }

    #[test]
    fn test_continue_statement() {
        let input = "continue;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::Continue{ .. } = stmt {
            assert!(true);
        } else {
            panic!("Continue statement parsed incorrectly");
        };
    }

    #[test]
    fn test_break_statement() {
        let input = "break;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::Break{ .. } = stmt {
            assert!(true);
        } else {
            panic!("Break statement parsed incorrectly");
        };
    }

    #[test]
    fn test_declare_statement() {
        let input = "a := 1;";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        let expr = if let Statement::Declare{ name, value, .. } = stmt {
            assert!(name.value.eq("a"));
            value
        } else {
            panic!("Declare statement parsed incorrectly");
        };

        let int = if let Expression::Integer { value, .. } = **expr {
            value
        } else {
            panic!("Integer parsed incorrectly");
        };

        assert_eq!(int, 1);
    }

    #[test]
    fn test_for_statement() {
        let input = "for (i in range) {};";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::For{ identifier, .. } = stmt {
            assert!(identifier.value.eq("i"));
        } else {
            panic!("For statement parsed incorrectly");
        }
    }

    #[test]
    fn test_while_statement() {
        let input = "while (true) {}";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::While{ .. } = stmt {
            assert!(true);
        } else {
            panic!("While statement parsed incorrectly");
        }
    }

    #[test]
    fn test_loop_statement() {
        let input = "loop {}";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::While{ .. } = stmt {
            assert!(true);
        } else {
            panic!("While statement parsed incorrectly");
        }
    }

    #[test]
    fn test_if_statement() {
        let input = "if (false) {}";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::Conditional{ alternative, .. } = stmt {
            assert!(alternative.is_none());
        } else {
            panic!("If statement parsed incorrectly");
        }
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (false) {} else {}";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::Conditional{ alternative, .. } = stmt {
            assert!(alternative.is_some());
        } else {
            panic!("If-else statement parsed incorrectly");
        }
    }

    #[test]
    fn test_if_else_if_statement() {
        let input = "if (false) {} else if (true) {} else {}";
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements.first().unwrap();
        
        if let Statement::Conditional{ alternative, .. } = stmt {
            assert!(alternative.is_some());
            if let Some(stmts) = alternative {
                assert_eq!(stmts.len(), 1);
                if let Statement::Conditional{ alternative, .. } = &stmts[0] {
                    assert!(alternative.is_some());
                }
            }
        } else {
            panic!("If-else-if statement parsed incorrectly");
        }
    }
}

