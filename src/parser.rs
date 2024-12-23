use std::fmt::Display;
use crate::tokens::{Token, Operator};

enum LiteralExpr {
    Expression(Box<Expr>),
    String(String),
    Boolean(bool),
    Number(f64),
    Nil,
}
impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Expression(expr) => write!(f, "{}", expr),
            Self::String(s) => write!(f, "{}", s),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::Nil => write!(f, "nil"),
        }
    }
}

enum Expr {
    Binary(Box<Expr>, Operator, Box<Expr>),
    UnaryExpr(Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralExpr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({} ({}) ({}))", op, left, right),
            Self::UnaryExpr(op, expr) => write!(f, "({} ({}))", op, expr),
            Self::Grouping(expr) => write!(f, "(group {})", expr),
            Self::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

fn parse(text: String)  {
    
}

fn expression(tokens: Vec<Token>) {
    equality(tokens);
}

fn equality(tokens: Vec<Token>) {

}

fn comparison(tokens: Vec<Token>) {

}

fn term(tokens: Vec<Token>) {

}

fn factor(tokens: Vec<Token>) {

}

fn unary(tokens: Vec<Token>) {

}

fn primary(tokens: Vec<Token>) {

}