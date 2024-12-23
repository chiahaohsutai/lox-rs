use crate::tokens::{Keyword, Operator, Punctuation, Token};
use std::{char, iter::Peekable, str::CharIndices};

fn match_punctuation(ch: char, line: usize) -> Option<Token> {
    let token = match ch {
        '(' => Token::PUNC(Punctuation::LPAREN, line),
        ')' => Token::PUNC(Punctuation::RPAREN, line),
        '{' => Token::PUNC(Punctuation::LBRACE, line),
        '}' => Token::PUNC(Punctuation::RBRACE, line),
        ',' => Token::PUNC(Punctuation::COMMA, line),
        '.' => Token::PUNC(Punctuation::DOT, line),
        ';' => Token::PUNC(Punctuation::SEMICOLON, line),
        _ => return None,
    };
    Some(token)
}

fn match_arithmetic_operator(
    ch: char,
    line: &mut usize,
    chars: &mut Peekable<CharIndices>,
) -> Option<Token> {
    if ch == '/' && chars.peek().map(|(_, c)| *c == '/').unwrap_or(false) {
        return match_comment(chars, line);
    }
    let token = match ch {
        '+' => Token::OPER(Operator::PLUS, *line),
        '-' => Token::OPER(Operator::MINUS, *line),
        '*' => Token::OPER(Operator::STAR, *line),
        '/' => Token::OPER(Operator::SLASH, *line),
        _ => return None,
    };
    Some(token)
}

fn match_logical_operator(
    ch: char,
    line: usize,
    chars: &mut Peekable<CharIndices>,
) -> Option<Token> {
    let binary = chars.peek().map(|(_, c)| *c == '=').unwrap_or(false);
    if binary {
        chars.next();
    }
    let token = match (ch, binary) {
        ('!', false) => Token::OPER(Operator::BANG, line),
        ('<', false) => Token::OPER(Operator::LESS, line),
        ('=', false) => Token::OPER(Operator::EQUAL, line),
        ('>', false) => Token::OPER(Operator::GREATER, line),
        ('<', true) => Token::OPER(Operator::LESSEQ, line),
        ('!', true) => Token::OPER(Operator::BANGEQ, line),
        ('>', true) => Token::OPER(Operator::GREATER, line),
        ('=', true) => Token::OPER(Operator::EQUALEQ, line),
        _ => return None,
    };
    Some(token)
}

fn match_comment(chars: &mut Peekable<CharIndices>, line: &mut usize) -> Option<Token> {
    chars.by_ref().take_while(|(_, c)| {
        if *c == '\n' {
            *line += 1;
        };
        *c != '\n'
    });
    None
}

fn match_whitespace(ch: char, line: &mut usize) -> Option<Token> {
    if ch == '\n' {
        *line += 1;
    };
    None
}

fn match_string_literal(chars: &mut Peekable<CharIndices>, line: usize) -> Option<Token> {
    let mut last = '\0';
    let string = chars
        .by_ref()
        .take_while(|(_, c)| {
            last = *c;
            *c != '"'
        })
        .map(|(_, c)| c)
        .collect();
    if last != '"' {
        Some(Token::INVALID(string, line))
    } else {
        Some(Token::STR(string, line))
    }
}

fn match_kw_or_identifier(
    ch: char,
    chars: &mut Peekable<CharIndices>,
    line: &mut usize,
) -> Option<Token> {
    let string: String = chars
        .by_ref()
        .take_while(|(_, c)| {
            if *c == '\n' {
                *line += 1;
            };
            c.is_alphanumeric() || *c == '_'
        })
        .map(|(_, c)| c.to_string())
        .collect();
    let string = format!("{}{}", ch, string);
    if Keyword::list().contains(&string) {
        Keyword::new(string).and_then(|kw| Some(Token::KWD(kw, *line)))
    } else {
        Some(Token::IDEN(string, *line))
    }
}

fn match_number(ch: char, chars: &mut Peekable<CharIndices>, line: &mut usize) -> Option<Token> {
    let number: String = chars
        .by_ref()
        .take_while(|(_, c)| c.is_digit(10) || *c == '.')
        .map(|(_, c)| c.to_string())
        .collect();
    let number = format!("{}{}", ch, number);
    match number.parse::<f64>() {
        Ok(n) => Some(Token::NUM(n, *line)),
        Err(_) => Some(Token::INVALID(number, *line)),
    }
}

pub fn scan(text: String) -> Vec<Token> {
    let mut chars = text.char_indices().peekable();
    let mut tokens = vec![];
    let mut line = 0;

    while let Some((idx, ch)) = chars.next() {
        let token = match ch {
            '(' | ')' | '{' | '}' | ',' | '.' | ';' => match_punctuation(ch, line),
            '+' | '-' | '*' | '/' => match_arithmetic_operator(ch, &mut line, &mut chars),
            '!' | '=' | '<' | '>' => match_logical_operator(ch, line, &mut chars),
            '"' => match_string_literal(&mut chars, line),
            ' ' | '\t' | '\r' => match_whitespace(ch, &mut line),
            '0'..='9' => match_number(ch, &mut chars, &mut line),
            'a'..='z' | 'A'..='Z' | '_' => match_kw_or_identifier(ch, &mut chars, &mut line),
            _ => None,
        };
        token.map(|t| tokens.push(t));
    }
    tokens.push(Token::EOF(line));
    tokens
}
