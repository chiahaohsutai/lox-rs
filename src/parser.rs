use std::{fmt::Display, slice::Iter, iter::Peekable};
use crate::tokens::{self, Keyword, Operator, Punctuation, Token};

#[derive(Debug)]
pub enum LiteralExpr {
    String(String),
    Boolean(bool),
    Number(f64),
    Nil,
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, Operator, Box<Expr>),
    UnaryExpr(Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralExpr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Self::UnaryExpr(op, expr) => write!(f, "({} {})", op, expr),
            Self::Grouping(expr) => write!(f, "(group {})", expr),
            Self::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Expr {
    let tokens = tokens.iter().peekable();
    expression(tokens).0
}

fn expression(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
   equality(tokens)
}

fn equality(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let (mut expr, mut tokens) = comparison(tokens);
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::EQUALEQ | Operator::BANGEQ => {
                let _ = tokens.next();
                let rhs = comparison(tokens);
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    };
    (expr, tokens)
}

fn comparison(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let (mut expr, mut tokens) = term(tokens);
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::LESS | Operator::LESSEQ | Operator::GREATER | Operator::GREATEREQ => {
                let _ = tokens.next();
                let rhs = term(tokens);
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    };
    (expr, tokens)
}

fn term(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let (mut expr, mut tokens) = factor(tokens);
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::PLUS | Operator::MINUS => {
                let _ = tokens.next();
                let rhs = factor(tokens);
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    };
    (expr, tokens)
}

fn factor(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let (mut expr, mut tokens) = unary(tokens);
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::STAR | Operator::SLASH => {
                let _ = tokens.next();
                let rhs = unary(tokens);
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    };
    (expr, tokens)
}

fn unary(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let mut tokens = tokens;
    if let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::MINUS | Operator::BANG => {
                let _ = tokens.next();
                let (expr, tokens) = unary(tokens);
                return (Expr::UnaryExpr(*op, Box::new(expr)), tokens);
            }
            _ => primary(tokens),
        }
    } else {
        primary(tokens)
    }
}

fn primary(tokens: Peekable<Iter<Token>>) -> (Expr, Peekable<Iter<Token>>) {
    let mut tokens = tokens;
    if let Some(token) = tokens.next() {
        match token {
            tokens::Token::NUM(n, _) => (Expr::Literal(LiteralExpr::Number(*n)), tokens),
            tokens::Token::STR(s, _) => (Expr::Literal(LiteralExpr::String(String::from(s))), tokens),
            tokens::Token::KWD(Keyword::TRUE, _) => (Expr::Literal(LiteralExpr::Boolean(true)), tokens),
            tokens::Token::KWD(Keyword::FALSE, _) => (Expr::Literal(LiteralExpr::Boolean(false)), tokens),
            tokens::Token::KWD(Keyword::NIL, _) => (Expr::Literal(LiteralExpr::Nil), tokens),
            tokens::Token::PUNC(Punctuation::LPAREN, _) => {
                let (expr, mut tokens) = expression(tokens);
                if let Some(Token::PUNC(Punctuation::RPAREN, _)) = tokens.peek() {
                    return (Expr::Grouping(Box::new(expr)), tokens);
                } else {
                    panic!("Expected closing parenthesis"); // TODO: Better error handling
                }
            }
            _ => panic!("Unexpected token {:?}", token), // TODO: Better error handling
        }
    } else {
        panic!("Unexpected end of input"); // TODO: Better error handling
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_expression() {
        let tokens = vec![
            crate::tokens::Token::NUM(1.0, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::PLUS, 0),
            crate::tokens::Token::NUM(2.0, 0),
        ];
        let ast = crate::parser::parse(tokens);
        assert_eq!(format!("{}", ast), "(+ 1 2)");
    }
    #[test]
    fn test_expression_with_parenthesis() {
        let tokens = vec![
            crate::tokens::Token::PUNC(crate::tokens::Punctuation::LPAREN, 0),
            crate::tokens::Token::NUM(1.0, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::PLUS, 0),
            crate::tokens::Token::NUM(2.0, 0),
            crate::tokens::Token::PUNC(crate::tokens::Punctuation::RPAREN, 0),
        ];
        let ast = crate::parser::parse(tokens);
        assert_eq!(format!("{}", ast), "(group (+ 1 2))");
    }
}