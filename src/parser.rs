use crate::tokenizer::{Keyword, Operator, Punctuation, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Number(f64),
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Literal(Literal),
    Grouping(Box<Expression>),
    Variable(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Self::Unary(op, expr) => write!(f, "({} {})", op, expr),
            Self::Grouping(expr) => write!(f, "(group {})", expr),
            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Variable(name) => write!(f, "(variable {})", name),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Expression(expr) => write!(f, "{}", expr),
            Self::Print(expr) => write!(f, "print {}", expr),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, String> {
    let mut declarations: Vec<Statement> = vec![];
    let idx = 0;
    match tokens.get(idx) {
        Some(Token::KEYWORD(Keyword::PRINT, _, _)) => {
            match apply_expression_rule(&tokens, idx + 1) {
                Ok((expr, idx)) => {

                }
                Err(_) => todo!(),
            }
        }
        _ => todo!(),
    }
    Ok(declarations)
}

fn synchronize(tokens: &Vec<Token>, idx: usize) -> usize {
    let mut idx = idx;
    while idx < tokens.len() {
        let next = &tokens[idx];
        if let Token::KEYWORD(kw, _, _) = next {
            if matches!(
                kw,
                Keyword::FUN
                    | Keyword::VAR
                    | Keyword::FOR
                    | Keyword::IF
                    | Keyword::WHILE
                    | Keyword::PRINT
                    | Keyword::RETURN
            ) {
                break;
            };
        } else if let Token::PUNCTUATION(Punctuation::SEMICOLON, _, _) = next {
            break;
        } else {
            idx += 1;
        }
    }
    idx
}

fn apply_expression_rule(tokens: &Vec<Token>, idx: usize) -> Result<(Expression, usize), String> {
    let (expr, idx) = apply_equality_rule(tokens, idx)?;
    Ok((expr, idx))
}

fn apply_equality_rule(tokens: &Vec<Token>, idx: usize) -> Result<(Expression, usize), String> {
    todo!();
}

fn apply_comparison_rule() {
    todo!();
}
