use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Illegal,

    Declare,
    Assign,
    Bang,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    PercentEquals,

    Or,
    And,

    Comma,
    Semicolon,

    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,

    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,

    Function,
    Generator,
    Sequence,
    If,
    Else,
    True,
    False,
    Return,
    Yield,
    For,
    In,
    While,
    Loop,
    Continue,
    Break,
    Next,

    Note,
    Rest,
    Control,

    Comment,

    Identifier(String),
    Integer(i64),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Declare => write!(f, "{}", ":="),
            TokenType::Assign => write!(f, "{}", "="),
            TokenType::Bang => write!(f, "{}", "!"),
            TokenType::Plus => write!(f, "{}", "+"),
            TokenType::Minus => write!(f, "{}", "-"),
            TokenType::Asterisk => write!(f, "{}", "*"),
            TokenType::Slash => write!(f, "{}", "/"),
            TokenType::Percent => write!(f, "{}", "%"),
            TokenType::PlusEquals => write!(f, "{}", "+="),
            TokenType::MinusEquals => write!(f, "{}", "-="),
            TokenType::AsteriskEquals => write!(f, "{}", "*="),
            TokenType::SlashEquals => write!(f, "{}", "/="),
            TokenType::PercentEquals => write!(f, "{}", "%="),
            TokenType::And => write!(f, "{}", "&&"),
            TokenType::Or => write!(f, "{}", "||"),
            TokenType::Comma => write!(f, "{}", ","),
            TokenType::Semicolon => write!(f, "{}", ";"),
            TokenType::Equals => write!(f, "{}", "=="),
            TokenType::NotEquals => write!(f, "{}", "!="),
            TokenType::GreaterThan => write!(f, "{}", ">"),
            TokenType::GreaterThanEquals => write!(f, "{}", ">="),
            TokenType::LessThan => write!(f, "{}", "<"),
            TokenType::LessThanEquals => write!(f, "{}", "<="),
            TokenType::Lparen => write!(f, "{}", "("),
            TokenType::Rparen => write!(f, "{}", ")"),
            TokenType::Lbracket => write!(f, "{}", "["),
            TokenType::Rbracket => write!(f, "{}", "]"),
            TokenType::Lbrace => write!(f, "{}", "{"),
            TokenType::Rbrace => write!(f, "{}", "}"),
            TokenType::Function => write!(f, "{}", "fn"),
            TokenType::Generator => write!(f, "{}", "gen"),
            TokenType::Sequence => write!(f, "{}", "seq"),
            TokenType::If => write!(f, "{}", "if"),
            TokenType::Else => write!(f, "{}", "else"),
            TokenType::True => write!(f, "{}", "true"),
            TokenType::False => write!(f, "{}", "false"),
            TokenType::Return => write!(f, "{}", "return"),
            TokenType::Yield => write!(f, "{}", "yield"),
            TokenType::For => write!(f, "{}", "for"),
            TokenType::While => write!(f, "{}", "while"),
            TokenType::Loop => write!(f, "{}", "loop"),
            TokenType::Continue => write!(f, "{}", "continue"),
            TokenType::Break => write!(f, "{}", "break"),
            TokenType::Next => write!(f, "{}", "next"),
            TokenType::Note => write!(f, "{}", "note"),
            TokenType::Rest => write!(f, "{}", "rest"),
            TokenType::Control => write!(f, "{}", "cc"),
            TokenType::In => write!(f, "{}", "in"),
            TokenType::Comment => write!(f, "{}", "//"),
            TokenType::Identifier(s) => write!(f, "{}", s),
            TokenType::Integer(n) => write!(f, "{}", n),
            _ => write!(f, "{}", "Illegal token")
        }
    }
}
