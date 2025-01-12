use crate::ternary;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF,
    NUMBER(f64),
    STRING(String),
    KEYWORD(Keyword),
    OPERATOR(Operator),
    IDENTIFIER(String),
    PUNCTUATION(Punctuation),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::EOF => write!(f, "EOF"),
            Self::NUMBER(n) => write!(f, "NUMBER[{}]", n),
            Self::STRING(s) => write!(f, "STRING[{}]", s),
            Self::KEYWORD(k) => write!(f, "{}", k),
            Self::IDENTIFIER(i) => write!(f, "IDENTIFIER[{}]", i),
            Self::OPERATOR(o) => write!(f, "OPERATOR[{}]", o),
            Self::PUNCTUATION(p) => write!(f, "PUNCTUATION[{}]", p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Keyword {
    IF,
    OR,
    FUN,
    AND,
    FOR,
    NIL,
    VAR,
    ELSE,
    TRUE,
    THIS,
    CLASS,
    PRINT,
    WHILE,
    SUPER,
    FALSE,
    RETURN,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IF => write!(f, "IF"),
            Self::OR => write!(f, "OR"),
            Self::FUN => write!(f, "FUN"),
            Self::AND => write!(f, "AND"),
            Self::FOR => write!(f, "FOR"),
            Self::NIL => write!(f, "NIL"),
            Self::VAR => write!(f, "VAR"),
            Self::ELSE => write!(f, "ELSE"),
            Self::TRUE => write!(f, "TRUE"),
            Self::THIS => write!(f, "THIS"),
            Self::CLASS => write!(f, "CLASS"),
            Self::PRINT => write!(f, "PRINT"),
            Self::WHILE => write!(f, "WHILE"),
            Self::SUPER => write!(f, "SUPER"),
            Self::FALSE => write!(f, "FALSE"),
            Self::RETURN => write!(f, "RETURN"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Punctuation {
    DOT,
    COMMA,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    SEMICOLON,
}

impl std::fmt::Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::COMMA => write!(f, ","),
            Self::DOT => write!(f, "."),
            Self::LBRACE => write!(f, "{{"),
            Self::LPAREN => write!(f, "("),
            Self::RBRACE => write!(f, "}}"),
            Self::RPAREN => write!(f, ")"),
            Self::SEMICOLON => write!(f, ";"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Operator {
    BANG,
    LESS,
    PLUS,
    STAR,
    EQUAL,
    SLASH,
    MINUS,
    LESSEQ,
    BANGEQ,
    GREATER,
    EQUALEQ,
    GREATEREQ,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BANG => write!(f, "!"),
            Self::LESS => write!(f, "<"),
            Self::PLUS => write!(f, "+"),
            Self::STAR => write!(f, "*"),
            Self::EQUAL => write!(f, "="),
            Self::SLASH => write!(f, "/"),
            Self::MINUS => write!(f, "-"),
            Self::LESSEQ => write!(f, "<="),
            Self::BANGEQ => write!(f, "!="),
            Self::GREATER => write!(f, ">"),
            Self::EQUALEQ => write!(f, "=="),
            Self::GREATEREQ => write!(f, ">="),
        }
    }
}

fn tokenize_string(program: &mut VecDeque<(usize, char)>) -> Result<Token, String> {
    let mut chars: Vec<String> = vec![];
    program.pop_front();

    while let Some((_, char)) = program.front() {
        if char == &'"' {
            return Ok(Token::STRING(chars.join("")));
        } else {
            chars.push(char.to_string());
            program.pop_front();
        }
    }
    Err(format!("Unterminated string: {}", chars.join("")))
}

fn tokenize_logical_operator(program: &mut VecDeque<(usize, char)>) -> Result<Token, String> {
    let curr = program.front();
    let mut next_is_equal_char = false;
    if let Some((i, _)) = curr {
        next_is_equal_char = matches!(program.get(i + 1), Some((_, '=')));
    };

    let token = match curr {
        Some((_, '!')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::BANGEQ),
            Token::OPERATOR(Operator::BANG)
        )),
        Some((_, '=')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::EQUALEQ),
            Token::OPERATOR(Operator::EQUAL)
        )),
        Some((_, '<')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::LESSEQ),
            Token::OPERATOR(Operator::LESS)
        )),
        Some((_, '>')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::GREATEREQ),
            Token::OPERATOR(Operator::GREATER)
        )),
        _ => Err(format!("Expected logical operator or assignment")),
    };
    if next_is_equal_char {
        program.pop_front();
    };
    token
}

fn tokenize_number(program: &mut VecDeque<(usize, char)>) -> Result<Token, String> {
    todo!();
}

pub fn tokenize(program: &str) -> Vec<Result<Token, String>> {
    let mut program: VecDeque<(usize, char)> = program.chars().enumerate().collect();
    let mut tokens: Vec<Result<Token, String>> = vec![];

    while let Some((i, char)) = program.front() {
        let token = match char {
            '(' => Some(Ok(Token::PUNCTUATION(Punctuation::LPAREN))),
            ')' => Some(Ok(Token::PUNCTUATION(Punctuation::RPAREN))),
            '{' => Some(Ok(Token::PUNCTUATION(Punctuation::LBRACE))),
            '}' => Some(Ok(Token::PUNCTUATION(Punctuation::RBRACE))),
            ',' => Some(Ok(Token::PUNCTUATION(Punctuation::COMMA))),
            '.' => Some(Ok(Token::PUNCTUATION(Punctuation::DOT))),
            ';' => Some(Ok(Token::PUNCTUATION(Punctuation::SEMICOLON))),
            '+' => Some(Ok(Token::OPERATOR(Operator::PLUS))),
            '-' => Some(Ok(Token::OPERATOR(Operator::MINUS))),
            '*' => Some(Ok(Token::OPERATOR(Operator::STAR))),
            '/' => Some(Ok(Token::OPERATOR(Operator::SLASH))),
            '!' | '=' | '<' | '>' => Some(tokenize_logical_operator(&mut program)),
            '"' => Some(tokenize_string(&mut program)),
            '0'..='9' => Some(tokenize_number(&mut program)),
            _ => None,
        };
        program.pop_front();
        token.map(|token| tokens.push(token));
    }
    tokens.push(Ok(Token::EOF));
    tokens
}
