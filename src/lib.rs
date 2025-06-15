use core::fmt;
use turnip::ifelse;
use miette::{Error, LabeledSpan};


pub struct Token<'de> {
    pub kind: TokenKind<'de>,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind)
    }
}

impl<'de> Token<'de> {
    pub fn new(kind: TokenKind<'de>) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'de> {
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
    Ident(&'de str),
    Num(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Bang,
    Equal,
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self {
            TokenKind::LeftParen => "LEFT_PAREN ( null",
            TokenKind::RightParen => "RIGHT_PAREN ) null",
            TokenKind::LeftBrace => "LEFT_BRACE { null",
            TokenKind::RightBrace => "RIGHT_BRACE } null",
            TokenKind::Semicolon => "SEMICOLON ; null",
            TokenKind::Comma => "COMMA , null",
            TokenKind::Plus => "PLUS + null",
            TokenKind::Minus => "MINUS - null",
            TokenKind::Star => "STAR * null",
            TokenKind::BangEqual => "BANG_EQUAL != null",
            TokenKind::EqualEqual => "EQUAL_EQUAL == null",
            TokenKind::LessEqual => "LESS_EQUAL <= null",
            TokenKind::GreaterEqual => "GREATER_EQUAL >= null",
            TokenKind::Less => "LESS < null",
            TokenKind::Greater => "GREATER > null",
            TokenKind::Slash => "SLASH / null",
            TokenKind::Dot => "DOT . null",
            TokenKind::And => "AND and null",
            TokenKind::Class => "CLASS class null",
            TokenKind::Else => "ELSE else null",
            TokenKind::False => "FALSE false null",
            TokenKind::For => "FOR for null",
            TokenKind::Fun => "FUN fun null",
            TokenKind::If => "IF if null",
            TokenKind::Nil => "NIL nil null",
            TokenKind::Or => "OR or null",
            TokenKind::Return => "RETURN return null",
            TokenKind::Super => "SUPER super null",
            TokenKind::This => "THIS this null",
            TokenKind::True => "TRUE true null",
            TokenKind::Var => "VAR var null",
            TokenKind::While => "WHILE while null",
            TokenKind::Bang => "BANG ! null",
            TokenKind::Equal => "EQUAL = null",
            TokenKind::Str(s) => return write!(fmt, r#"STRING "{s}" {s}"#),
            TokenKind::Num(num) => return write!(fmt, r#"NUMBER {num} {num}"#),
            TokenKind::Ident(i) => return write!(fmt, "IDENTIFIER {i} null"),
            
        };
        write!(fmt, "{}", kind)
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
    fn advance(&mut self, n: usize) {
        self.rest = &self.rest[n..];
        self.byte += n;
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.rest.len();
        self.rest = self.rest.trim_start();
        self.byte += len - self.rest.len();

        let mut chars = self.rest.chars();
        let c = chars.next()?;

        self.advance(c.len_utf8());
        let next_iseq = self.rest.starts_with('=');

        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,
            '>' => ifelse!(next_iseq, TokenKind::GreaterEqual, TokenKind::Greater),
            '<' => ifelse!(next_iseq, TokenKind::LessEqual, TokenKind::Less),
            '/' => ifelse!(next_iseq, TokenKind::EqualEqual, TokenKind::Slash),
            '!' => ifelse!(next_iseq, TokenKind::BangEqual, TokenKind::Bang),
            '=' => ifelse!(next_iseq, TokenKind::EqualEqual, TokenKind::Equal),
            '"' => todo!(),
            '0'..='9' => todo!(),
            'a'..='z' | '_' => todo!(),
            _ => {
                let span = self.byte..self.byte + c.len_utf8();
                let labels = vec![LabeledSpan::at(span, "")];
                let err = miette::miette!(labels = labels, "Encountered unexpected token '{c}'")
                    .with_source_code(String::from(self.whole));
                return Some(Err(err))
            }
        };
        self.advance(next_iseq as usize);
        Some(Ok(Token::new(kind)))
    }
}
