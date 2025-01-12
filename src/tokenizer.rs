use crate::ternary;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF(usize, usize),
    NUMBER(f64, usize, usize),
    STRING(String, usize, usize),
    KEYWORD(Keyword, usize, usize),
    OPERATOR(Operator, usize, usize),
    IDENTIFIER(String, usize, usize),
    PUNCTUATION(Punctuation, usize, usize),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::EOF(line, col) => write!(f, "EOF[line: {}, column: {}]", line, col),
            Self::NUMBER(n, line, col) => write!(f, "NUMBER[value: {}, line: {}, column: {}]", n, line, col),
            Self::STRING(s, line, col) => write!(f, "STRING[value: {}, line: {}, column: {}]", s, line, col),
            Self::KEYWORD(k, line, col) => write!(f, "KEYWORD[value: {}, line: {}, column: {}]", k, line, col),
            Self::IDENTIFIER(i, line, col) => write!(f, "IDENTIFIER[value: {}, line: {}, column: {}]", i, line, col),
            Self::OPERATOR(o, line, col) => write!(f, "OPERATOR[value: {}, line: {}, column: {}]", o, line, col),
            Self::PUNCTUATION(p, line, col) => write!(f, "PUNCTUATION[value: {}, line: {}, column: {}]", p, line, col),
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

fn tokenize_string(program: &mut VecDeque<(usize, char)>, line: &mut usize, col: &mut usize) -> Result<Token, String> {
    let mut chars: Vec<String> = vec![];
    program.pop_front();

    while let Some((_, char)) = program.front() {
        if char == &'"' {
            return Ok(Token::STRING(chars.join(""), *line, *col));
        } else {
            chars.push(char.to_string());
            program.pop_front();
        }
    }
    Err(format!("Unterminated string: {}", chars.join("")))
}

fn tokenize_logical_operator(program: &mut VecDeque<(usize, char)>, line: &mut usize, col: &mut usize) -> Result<Token, String> {
    let curr = program.front();
    let mut next_is_equal_char = false;
    if let Some((i, _)) = curr {
        next_is_equal_char = matches!(program.get(i + 1), Some((_, '=')));
    };

    let token = match curr {
        Some((_, '!')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::BANGEQ, *line, *col),
            Token::OPERATOR(Operator::BANG, *line, *col)
        )),
        Some((_, '=')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::EQUALEQ, *line, *col),
            Token::OPERATOR(Operator::EQUAL, *line, *col)
        )),
        Some((_, '<')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::LESSEQ, *line, *col),
            Token::OPERATOR(Operator::LESS, *line, *col)
        )),
        Some((_, '>')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::GREATEREQ, *line, *col),
            Token::OPERATOR(Operator::GREATER, *line, *col)
        )),
        _ => Err(format!("Expected logical operator or assignment")),
    };
    if next_is_equal_char {
        program.pop_front();
    };
    token
}

fn tokenize_number(program: &mut VecDeque<(usize, char)>, line: &mut usize, col: &mut usize) -> Result<Token, String> {
    todo!();
}

fn tokenize_kw_or_id(program: &mut VecDeque<(usize, char)>, line: &mut usize, col: &mut usize) -> Result<Token, String> {
    todo!();
}

fn increase_line_count<T>(line: &mut usize, count: usize) -> Option<T> {
    *line += count;
    None
}

fn increase_column_count<T>(column: &mut usize, count: usize) -> Option<T> {
    *column += count;
    None
}

pub fn tokenize(program: &str) -> Vec<Result<Token, String>> {
    let mut program: VecDeque<(usize, char)> = program.chars().enumerate().collect();
    let mut tokens: Vec<Result<Token, String>> = vec![];
    let mut line: usize = 0;
    let mut col: usize = 0;

    while let Some((_, char)) = program.front() {
        let token = match char {
            '(' => Some(Ok(Token::PUNCTUATION(Punctuation::LPAREN, line, col))),
            ')' => Some(Ok(Token::PUNCTUATION(Punctuation::RPAREN, line, col))),
            '{' => Some(Ok(Token::PUNCTUATION(Punctuation::LBRACE, line, col))),
            '}' => Some(Ok(Token::PUNCTUATION(Punctuation::RBRACE, line, col))),
            ',' => Some(Ok(Token::PUNCTUATION(Punctuation::COMMA, line, col))),
            '.' => Some(Ok(Token::PUNCTUATION(Punctuation::DOT, line, col))),
            ';' => Some(Ok(Token::PUNCTUATION(Punctuation::SEMICOLON, line, col))),
            '+' => Some(Ok(Token::OPERATOR(Operator::PLUS, line, col))),
            '-' => Some(Ok(Token::OPERATOR(Operator::MINUS, line, col))),
            '*' => Some(Ok(Token::OPERATOR(Operator::STAR, line, col))),
            '/' => Some(Ok(Token::OPERATOR(Operator::SLASH, line, col))),
            '!' | '=' | '<' | '>' => Some(tokenize_logical_operator(&mut program, &mut line, &mut col)),
            '"' => Some(tokenize_string(&mut program, &mut line, &mut col)),
            '0'..='9' => Some(tokenize_number(&mut program, &mut line, &mut col)),
            'a'..='z' | 'A'..='Z' | '_' => Some(tokenize_kw_or_id(&mut program, &mut line, &mut col)),
            ' ' => increase_column_count(&mut col, 1),
            '\t' => increase_column_count(&mut col, 4),
            '\n' => increase_line_count(&mut line, 1),
            _ => None,
        };
        program.pop_front();
        token.map(|token| tokens.push(token));
    }
    tokens.push(Ok(Token::EOF(line, col)));
    tokens
}
