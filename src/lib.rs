use core::fmt;
use miette::{Error, LabeledSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'de> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Dot,
    Str(&'de str),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token = match self {
            Token::LeftParen => "LEFT_PAREN ( null",
            Token::RightParen => "RIGHT_PAREN ) null",
            Token::LeftBrace => "LEFT_BRACE { null",
            Token::RightBrace => "RIGHT_BRACE } null",
            Token::Semicolon => "SEMICOLON ; null",
            Token::Comma => "COMMA , null",
            Token::Plus => "PLUS + null",
            Token::Minus => "MINUS - null",
            Token::Star => "STAR *",
            Token::BangEqual => "BANG_EQUAL != null",
            Token::EqualEqual => "EQUAL_EQUAL == null",
            Token::LessEqual => "LESS_EQUAL <= null",
            Token::GreaterEqual => "GREATER_EQUAL >= null",
            Token::Less => "LESS < null",
            Token::Greater => "GREATER > null",
            Token::Slash => "SLASH / null",
            Token::Dot => "DOT . null",
            Token::Str(s) => return write!(fmt, r#"STRING "{s}" {s}"#),
        };
        write!(fmt, "{}", token)
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        let token = match c {
            '(' => Some(Ok(Token::LeftParen)),
            ')' => Some(Ok(Token::RightParen)),
            '{' => Some(Ok(Token::LeftBrace)),
            '}' => Some(Ok(Token::RightBrace)),
            ',' => Some(Ok(Token::Comma)),
            '.' => Some(Ok(Token::Dot)),
            '-' => Some(Ok(Token::Minus)),
            '+' => Some(Ok(Token::Plus)),
            ';' => Some(Ok(Token::Semicolon)),
            '*' => Some(Ok(Token::Star)),
            '"' => todo!(),
            _ => {
                let span = self.byte..self.byte + c.len_utf8();
                let labels = vec![LabeledSpan::at(span, "")];
                let err = miette::miette!(labels = labels, "Encountered unexpected token '{c}'")
                    .with_source_code(String::from(self.whole));
                Some(Err(err))
            }
        };
        self.rest = chars.as_str();
        self.byte += c.len_utf8();
        token
    }
}
