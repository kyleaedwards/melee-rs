use crate::token::{Token, TokenType};
use std::cell::RefCell;
use std::fmt;

thread_local! {
    static GLOBAL_INDENT_LEN: RefCell<i8> = RefCell::new(-2);
}

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Debug)]
pub enum Infix {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,
    Equals,
    NotEquals,
    And,
    Or
}

#[derive(PartialEq, Debug)]
pub enum CompoundAssign {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
}

pub type StatementBlock = Vec<Statement>;

#[derive(Debug)]
pub struct Program {
    pub statements: StatementBlock,
}

pub enum Node {
    Program(Program),
    StatementBlock(StatementBlock),
    Statement(Statement),
    Expression(Expression)
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // println!("{}", self.statements.len());
        display_statement_block(f, &self.statements)?;
        Ok(())
    }
}

fn display_statement_block(f: &mut fmt::Formatter, block: &StatementBlock) -> fmt::Result {
    let mut indent_len = 0;
    GLOBAL_INDENT_LEN.with(|i| {
        let mut len = i.borrow_mut();
        *len += 2;
        indent_len = *len;
    });
    let indent = (0..indent_len).map(|_| " ").collect::<String>();
    block
        .iter()
        .try_for_each(|stmt| {
            writeln!(f, "{}{}", indent, stmt)
        })?;
    GLOBAL_INDENT_LEN.with(|i| {
        let mut len = i.borrow_mut();
        *len -= 2;
    });
    fmt::Result::Ok(())
}

#[derive(Debug)]
pub enum Statement {
    Declare {
        token: Token,
        name: Identifier,
        value: Box<Expression>,
    },
    Return {
        token: Token,
        value: Option<Box<Expression>>,
    },
    Yield {
        token: Token,
        value: Option<Box<Expression>>,
    },
    Continue {
        token: Token,
    },
    Break {
        token: Token,
    },
    For {
        token: Token,
        identifier: Identifier,
        collection: Box<Expression>,
        block: StatementBlock,
    },
    While {
        token: Token,
        condition: Box<Expression>,
        block: StatementBlock,
    },
    Conditional {
        token: Token,
        condition: Box<Expression>,
        consequence: StatementBlock,
        alternative: Option<StatementBlock>,
    },
    Expression(Box<Expression>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Declare { name, value, .. } => write!(f, "{} := {};", name.value, value),
            Statement::Return { value, .. } => {
                if let Some(val) = value {
                    write!(f, "return {};", val)
                } else {
                    write!(f, "return;")
                }
            },
            Statement::Yield { value, .. } => {
                if let Some(val) = value {
                    write!(f, "yield {};", val)
                } else {
                    write!(f, "yield;")
                }
            },
            Statement::Continue { .. } => write!(f, "continue;"),
            Statement::Break { .. } => write!(f, "break;"),
            Statement::For { identifier, collection, block, .. } => {
                writeln!(f, "for ({} in {}) {{", identifier.value, collection)?;
                display_statement_block(f, &block)?;
                write!(f, "}}")
            },
            Statement::While { condition, block, token } => {
                if token.token_type == TokenType::Loop {
                    writeln!(f, "loop {{")?;
                } else {
                    writeln!(f, "while ({}) {{", condition)?;
                }
                display_statement_block(f, &block)?;
                write!(f, "}}")
            },
            Statement::Conditional { condition, consequence, alternative, .. } => {
                let mut indent_len = 0;
                GLOBAL_INDENT_LEN.with(|i| {
                    indent_len = *(i.borrow_mut());
                });
                let indent = (0..indent_len).map(|_| " ").collect::<String>();
                writeln!(f, "{}if ({}) {{", &indent, condition)?;
                display_statement_block(f, &consequence)?;
                if let Some(alt) = alternative {
                    if alt.len() == 1_usize {
                        if let Some(first) = alt.get(0) {
                            if let Statement::Conditional { .. } = first {
                                return write!(f, "{}}} else ", &indent)
                            }
                        }
                    }
                    writeln!(f, "{}}} else {{", &indent)?;
                    display_statement_block(f, &alt)?;
                }
                write!(f, "{}}}", &indent)
            },
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

pub type ExpressionList = Vec<Box<Expression>>;

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Assign {
        token: Token,
        left: Box<Expression>,
        value: Box<Expression>,
    },
    CompoundAssign {
        token: Token,
        operator: CompoundAssign,
        left: Box<Expression>,
        value: Box<Expression>,
    },
    Integer {
        token: Token,
        value: i32,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Array {
        token: Token,
        values: ExpressionList,
    },
    Infix {
        token: Token,
        left: Box<Expression>,
        operator: Infix,
        right: Box<Expression>,
    },
    Prefix {
        token: Token,
        operator: Prefix,
        right: Box<Expression>,
    },
    Function {
        token: Token,
        parameters: Vec<Identifier>,
        body: StatementBlock,
    },
    Generator {
        token: Token,
        parameters: Vec<Identifier>,
        body: StatementBlock,
    },
    Next {
        token: Token,
        value: Box<Expression>,
    },
    Index {
        token: Token,
        collection: Box<Expression>,
        index: Box<Expression>,
    },
    Call {
        token: Token,
        callee: Box<Expression>,
        arguments: ExpressionList,
    },
    Note {
        token: Token,
        arguments: ExpressionList,
    },
    Rest {
        token: Token,
        arguments: ExpressionList,
    },
    ControlChange {
        token: Token,
        arguments: ExpressionList,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(identifier) => write!(f, "{}", identifier.value),
            Expression::Assign { left, value, .. } => write!(f, "{} = {}", left, value),
            Expression::CompoundAssign { left, value, operator, .. } => {
                let op = match operator {
                    CompoundAssign::Add => "+=",
                    CompoundAssign::Subtract => "-=",
                    CompoundAssign::Multiply => "*=",
                    CompoundAssign::Divide => "/=",
                    CompoundAssign::Modulus => "%=",
                };
                write!(f, "{} {} {}", left, op, value)
            },
            Expression::Integer { value, .. } => write!(f, "{}", value),
            Expression::Boolean { value, .. } => write!(f, "{}", value),
            Expression::Array { values, .. } => {
                write!(f, "[")?;
                for value in values.iter().take(1) {
                    write!(f, "{}", value)?;
                }
                for value in values.iter().skip(1) {
                    write!(f, ", {}", value)?;
                }
                write!(f, "]")
            },
            Expression::Infix { left, right, operator, .. } => {
                let op = match operator {
                    Infix::Add => "+",
                    Infix::Subtract => "-",
                    Infix::Multiply => "*",
                    Infix::Divide => "/",
                    Infix::Modulus => "%",
                    Infix::Equals => "==",
                    Infix::NotEquals => "!=",
                    Infix::GreaterThan => ">",
                    Infix::GreaterThanEquals => ">=",
                    Infix::LessThan => ">=",
                    Infix::LessThanEquals => ">=",
                    Infix::And => "&&",
                    Infix::Or => "||"
                };
                write!(f, "{} {} {}", left, op, right)
            },
            Expression::Prefix { right, operator, .. } => {
                let op = match operator {
                    Prefix::Plus => "-+",
                    Prefix::Minus => "-",
                    Prefix::Not => "!",
                };
                write!(f, "{}{}", op, right)
            },
            Expression::Function { parameters, body, .. } => {
                write!(f, "fn (")?;
                for parameter in parameters.iter().take(1) {
                    write!(f, "{}", parameter.value)?;
                }
                for parameter in parameters.iter().skip(1) {
                    write!(f, ", {}", parameter.value)?;
                }
                writeln!(f, ") {{")?;
                display_statement_block(f, &body)?;
                write!(f, "}}")
            },
            Expression::Generator { parameters, body, .. } => {
                write!(f, "gen (")?;
                for parameter in parameters.iter().take(1) {
                    write!(f, "{}", parameter.value)?;
                }
                for parameter in parameters.iter().skip(1) {
                    write!(f, ", {}", parameter.value)?;
                }
                writeln!(f, ") {{")?;
                display_statement_block(f, &body)?;
                write!(f, "}}")
            },
            Expression::Next { value, .. } => write!(f, "next {}", value),
            Expression::Index { collection, index, .. } => write!(f, "{}[{}]", collection, index),
            Expression::Call { callee, arguments, .. } => {
                write!(f, "{}(", callee)?;
                for argument in arguments.iter().take(1) {
                    write!(f, "{}", argument)?;
                }
                for argument in arguments.iter().skip(1) {
                    write!(f, ", {}", argument)?;
                }
                write!(f, ")")
            },
            Expression::Note { arguments, .. } => {
                write!(f, "note(")?;
                for argument in arguments.iter().take(1) {
                    write!(f, "{}", argument)?;
                }
                for argument in arguments.iter().skip(1) {
                    write!(f, ", {}", argument)?;
                }
                write!(f, ")")
            },
            Expression::Rest { arguments, .. } => {
                write!(f, "rest(")?;
                for argument in arguments.iter().take(1) {
                    write!(f, "{}", argument)?;
                }
                for argument in arguments.iter().skip(1) {
                    write!(f, ", {}", argument)?;
                }
                write!(f, ")")
            },
            Expression::ControlChange { arguments, .. } => {
                write!(f, "cc(")?;
                for argument in arguments.iter().take(1) {
                    write!(f, "{}", argument)?;
                }
                for argument in arguments.iter().skip(1) {
                    write!(f, ", {}", argument)?;
                }
                write!(f, ")")
            },
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
