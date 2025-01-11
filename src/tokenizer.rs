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

fn tokenize_string_literal(program: &mut VecDeque<char>) -> Result<Token, String> {
    let mut chars: Vec<String> = vec![];
    while let Some(char) = program.front() {
        if char == &'"' {
            return Ok(Token::STRING(chars.join("")));
        } else {
            chars.push(char.to_string());
        }
    };
    Err(format!("Unterminated string: {}", chars.join("")))
}

pub fn tokenize(program: &str) -> Vec<Result<Token, String>> {
    let mut program: VecDeque<char> = program.chars().collect();
    let mut tokens: Vec<Result<Token, String>> = vec![];

    while let Some(char) = program.pop_front() {
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
            '!' => Some(Ok(ternary!(
                program.front() == Some(&'='),
                Token::OPERATOR(Operator::BANGEQ),
                Token::OPERATOR(Operator::BANG)
            ))),
            '=' => Some(Ok(ternary!(
                program.front() == Some(&'='),
                Token::OPERATOR(Operator::EQUALEQ),
                Token::OPERATOR(Operator::EQUAL)
            ))),
            '<' => Some(Ok(ternary!(
                program.front() == Some(&'='),
                Token::OPERATOR(Operator::LESSEQ),
                Token::OPERATOR(Operator::LESS)
            ))),
            '>' => Some(Ok(ternary!(
                program.front() == Some(&'='),
                Token::OPERATOR(Operator::GREATEREQ),
                Token::OPERATOR(Operator::GREATER)
            ))),
            '"' => Some(tokenize_string_literal(&mut program)),
            _ => None,
        };
        if let Some(Ok(Token::OPERATOR(
            Operator::BANGEQ | Operator::EQUALEQ | Operator::LESSEQ | Operator::GREATEREQ,
        ))) = token
        {
            program.pop_front();
        };
        token.map(|token| tokens.push(token));
    }
    tokens.push(Ok(Token::EOF));
    tokens
}
