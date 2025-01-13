use crate::ternary;
use core::num;
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

impl Token {
    fn new_op(op: Operator, line: usize, col: usize) -> Self {
        Self::OPERATOR(op, line, col)
    }

    fn new_num(num: f64, line: usize, col: usize) -> Self {
        Self::NUMBER(num, line, col)
    } 

    fn new_str(str: String, line: usize, col: usize) -> Self {
        Self::STRING(str, line, col)
    }

    fn new_kw(kw: Keyword, line: usize, col: usize) -> Self {
        Self::KEYWORD(kw, line, col)
    }

    fn new_id(id: String, line: usize, col: usize) -> Self {
        Self::IDENTIFIER(id, line, col)
    }

    fn new_punc(punc: Punctuation, line: usize, col: usize) -> Self {
        Self::PUNCTUATION(punc, line, col)
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

fn tokenize_string(program: &VecDeque<(usize, char)>, line: usize, offset: usize) -> (Option<Result<Token, String>>, usize) {
    let mut chars: Vec<char> = vec![];
    if let Some((idx, '"')) = program.front() {
        let mut i = *idx;
        while program.get(i).map(|(_, c)| *c != '"').unwrap_or(false) {
            chars.push(program.get(i).unwrap().1);
            i += 1;
        };
        if let Some((_, '"')) = program.get(i) {
            (Some(Ok(Token::STRING(chars.iter().collect::<String>(), line, idx - offset))), chars.len() + 2)
        } else {
            (Some(Err(format!("Unterminated string in line {} col {}", line, idx - offset))), chars.len() + 1)
        }
    } else {
        (Some(Err(String::from("Expected a string, but found no starting '\"'"))), 0)
    }
}

fn tokenize_logical_op(program: &VecDeque<(usize, char)>, line: usize, offset: usize) -> (Option<Result<Token, String>>, usize) {
    let curr = program.front();
    let mut next_is_equal_char = false;

    if let Some((i, _)) = curr {
        next_is_equal_char = matches!(program.get(i + 1), Some((_, '=')));
    };

    let token = match curr {
        Some((idx, '!')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::BANGEQ, line, *idx - offset),
            Token::OPERATOR(Operator::BANG, line, *idx - offset)
        )),
        Some((idx, '=')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::EQUALEQ, line, *idx - offset),
            Token::OPERATOR(Operator::EQUAL, line, *idx - offset)
        )),
        Some((idx, '<')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::LESSEQ, line, *idx - offset),
            Token::OPERATOR(Operator::LESS, line, *idx - offset)
        )),
        Some((idx, '>')) => Ok(ternary!(
            next_is_equal_char,
            Token::OPERATOR(Operator::GREATEREQ, line, *idx - offset),
            Token::OPERATOR(Operator::GREATER, line, *idx - offset)
        )),
        Some((idx, char)) => Err(format!("Expected logical operator or assignment in line {} column {}, found '{}' instead", line, *idx - offset, char)),
        None => Err(format!("Expected logical operator or assignment in line {}", line))
    };
    if token.is_ok() {
        (Some(token), ternary!(next_is_equal_char, 2, 1))
    } else {
        (Some(token), 0)
    }
}

fn tokenize_number(program: &VecDeque<(usize, char)>, line: usize, offset: usize) -> (Option<Result<Token, String>>, usize) {    
    if let Some((idx, _)) = program.front() {
        let mut digits: Vec<char> = vec![];
        let mut i = *idx;
        while program.get(i).map(|(_, char)| char.is_numeric() || char == &'.').unwrap_or(false) {
            digits.push(program.get(i).unwrap().1);
            i += 1;
        };
        match digits.iter().collect::<String>().parse() {
            Ok(num) => (Some(Ok(Token::NUMBER(num, line, idx - offset))), digits.len()),
            Err(_) => (Some(Err(format!("Invalid number in line {} column {}", line, idx))), digits.len()),
        }
    } else {
        (None, 0)
    }
}

fn tokenize_kw_or_id(program: &VecDeque<(usize, char)>, line: usize, offset: usize) -> (Option<Result<Token, String>>, usize) {
    todo!();
}

pub fn tokenize(program: &str) -> Vec<Result<Token, String>> {
    let mut program: VecDeque<(usize, char)> = program.chars().enumerate().collect();
    let mut tokens: Vec<Result<Token, String>> = vec![];
    let mut line: usize = 0;
    let mut offset: usize = 0;

    let mut num_iters: usize = 0;
    let length = program.len();

    while let Some((idx, char)) = program.front() {
        if num_iters == length {
            break;
        };
        let ts = match char {
            '(' => (Some(Ok(Token::PUNCTUATION(Punctuation::LPAREN, line, *idx - offset))), 1),
            ')' => (Some(Ok(Token::PUNCTUATION(Punctuation::RPAREN, line, *idx - offset))), 1),
            '{' => (Some(Ok(Token::PUNCTUATION(Punctuation::LBRACE, line, *idx - offset))), 1),
            '}' => (Some(Ok(Token::PUNCTUATION(Punctuation::RBRACE, line, *idx - offset))), 1),
            ',' => (Some(Ok(Token::PUNCTUATION(Punctuation::COMMA, line, *idx - offset))), 1),
            '.' => (Some(Ok(Token::PUNCTUATION(Punctuation::DOT, line, *idx - offset))), 1),
            ';' => (Some(Ok(Token::PUNCTUATION(Punctuation::SEMICOLON, line, *idx - offset))), 1),
            '+' => (Some(Ok(Token::OPERATOR(Operator::PLUS, line, *idx - offset))), 1),
            '-' => (Some(Ok(Token::OPERATOR(Operator::MINUS, line, *idx - offset))), 1),
            '*' => (Some(Ok(Token::OPERATOR(Operator::STAR, line, *idx - offset))), 1),
            '/' => (Some(Ok(Token::OPERATOR(Operator::SLASH, line, *idx - offset))), 1),
            '!' | '=' | '<' | '>' => tokenize_logical_op(&program, line, offset),
            '"' => tokenize_string(&program, line, offset),
            '0'..='9' => tokenize_number(&mut program, line, offset),
            'a'..='z' | 'A'..='Z' | '_' => tokenize_kw_or_id(&program, line, offset),
            '\r' => {
                offset += 1;
                (None, 1)
            },
            '\t' => {
                offset -= 4;
                (None, 1)
            },
            '\n' => {
                line += 1;
                (None, 1)
            },
            _ => (None, 1),
        };
        let (token, skips) = ts;
        token.map(|t| tokens.push(t));
        program.drain(0..skips.min(length));
        num_iters += 1;
    };
    tokens.push(Ok(Token::EOF(line + 1, 0)));
    tokens
}
