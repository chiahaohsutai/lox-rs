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
    let _ = chars.by_ref().take_while(|(_, c)| {
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
    let mut number: Vec<char> = vec![];
    while chars.peek().map(|(_, c)| c.is_numeric() || *c == '_').unwrap_or(false) {
        let (_, c) = chars.next().unwrap();
        number.push(c);
    };
    let number = format!("{}{}", ch, number.iter().collect::<String>());
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
            ' ' | '\t' | '\r' | '\n' => match_whitespace(ch, &mut line),
            '0'..='9' => match_number(ch, &mut chars, &mut line),
            'a'..='z' | 'A'..='Z' | '_' => match_kw_or_identifier(ch, &mut chars, &mut line),
            _ => None,
        };
        token.map(|t| tokens.push(t));
    }
    tokens.push(Token::EOF(line));
    tokens
}

mod Test {
    use super::*;

    #[test]
    fn test_scan() {
        let text = "var x = 10;".to_string();
        let tokens = scan(text);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0], Token::KWD(Keyword::VAR, 0));
        assert_eq!(tokens[1], Token::IDEN("x".to_string(), 0));
        assert_eq!(tokens[2], Token::OPER(Operator::EQUAL, 0));
        assert_eq!(tokens[3], Token::NUM(10.0, 0));
        assert_eq!(tokens[4], Token::PUNC(Punctuation::SEMICOLON, 0));
        assert_eq!(tokens[5], Token::EOF(0));
    }

    #[test]
    fn test_scan_string() {
        let text = r#""hello world""#.to_string();
        let tokens = scan(text);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::STR("hello world".to_string(), 0));
        assert_eq!(tokens[1], Token::EOF(0));
    }

    #[test]
    fn test_scan_newlines() {
        let text = "\n\"hello world\"\n".to_string();
        let tokens = scan(text);
        print!("{:?}", tokens);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::STR(String::from("hello world"), 1));
        assert_eq!(tokens[1], Token::EOF(2));
    }

    #[test]
    fn scan_invalid_string() {
        let text = r#""hello world"#.to_string();
        let tokens = scan(text);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::INVALID("hello world".to_string(), 0));
        assert_eq!(tokens[1], Token::EOF(0));
    }
}