use std::result::Result;
use crate::{parser::{Expr, LiteralExpr, Stmt}, tokens::Operator};

pub enum Literal {
    Number(f32),
    Boolean(bool),
    String(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
        }
    }
}

pub trait Evaluate {
    fn evaluate(self) -> Result<Option<Literal>, String>;
}

impl Evaluate for LiteralExpr {
    fn evaluate(self) -> Result<Option<Literal>, String> {
        match self {
            LiteralExpr::Number(n) => Ok(Some(Literal::Number(n as f32))),
            LiteralExpr::Boolean(b) => Ok(Some(Literal::Boolean(b))),
            LiteralExpr::String(s) => Ok(Some(Literal::String(s))),
            LiteralExpr::Nil => Ok(None),
        }
    }
}

fn evaluate_plus(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Number(l + r))),
        (Some(Literal::String(l)), Some(Literal::String(r))) => Ok(Some(Literal::String(format!("{}{}", l, r)))),
        _ => Err(String::from("Invalid operands for + operator.")),
    }
}

fn evaluate_minus(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Number(l - r))),
        _ => Err(String::from("Invalid operands for - operator.")),
    }
}

fn evaluate_star(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Number(l * r))),
        _ => Err(String::from("Invalid operands for * operator.")),
    }
}

fn evaluate_slash(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => {
            if r == 0.0 {
                Err(String::from("Division by zero."))
            } else {
                Ok(Some(Literal::Number(l / r)))
            }
        },
        _ => Err(String::from("Invalid operands for / operator.")),
    }
}

fn evaluate_equaleq(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Boolean(l == r))),
        (Some(Literal::Boolean(l)), Some(Literal::Boolean(r))) => Ok(Some(Literal::Boolean(l == r))),
        (Some(Literal::String(l)), Some(Literal::String(r))) => Ok(Some(Literal::Boolean(l == r))),
        (None, None) => Ok(Some(Literal::Boolean(true))),
        _ => Ok(Some(Literal::Boolean(false))),
    }
}

fn evaluate_bangeq(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    let equality = evaluate_equaleq(left, right)?;
    match equality {
        Some(Literal::Boolean(b)) => Ok(Some(Literal::Boolean(!b))),
        _ => Err(String::from("Invalid operands for != operator.")),
    }
}

fn evaluate_less(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Boolean(l < r))),
        _ => Err(String::from("Invalid operands for < operator.")),
    }
}

fn evaluate_lesseq(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Boolean(l <= r))),
        _ => Err(String::from("Invalid operands for <= operator.")),
    }
}

fn evaluate_greater(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Boolean(l > r))),
        _ => Err(String::from("Invalid operands for > operator.")),
    }
}

fn evaluate_greatereq(left: Option<Literal>, right: Option<Literal>) -> Result<Option<Literal>, String> {
    match (left, right) {
        (Some(Literal::Number(l)), Some(Literal::Number(r))) => Ok(Some(Literal::Boolean(l >= r))),
        _ => Err(String::from("Invalid operands for >= operator.")),
    }
}

fn evaluate_binary_expression(left: Box<Expr>, op: Operator, right: Box<Expr>) -> Result<Option<Literal>, String> {
    let left = left.evaluate()?;
    let right = right.evaluate()?;
    match op {
        Operator::PLUS => evaluate_plus(left, right),
        Operator::MINUS => evaluate_minus(left, right),
        Operator::STAR => evaluate_star(left, right),
        Operator::SLASH => evaluate_slash(left, right),
        Operator::EQUALEQ => evaluate_equaleq(left, right),
        Operator::BANGEQ => evaluate_bangeq(left, right),
        Operator::LESS => evaluate_less(left, right),
        Operator::LESSEQ => evaluate_lesseq(left, right),
        Operator::GREATER => evaluate_greater(left, right),
        Operator::GREATEREQ => evaluate_greatereq(left, right),
        _ => Err(String::from("Found invalid operator in binary expresion.")),
    }
}

fn evaluate_unary_expression(op: Operator, expression: Box<Expr>) -> Result<Option<Literal>, String> {
    let expr = expression.evaluate()?;
    match op {
        Operator::MINUS => {
            match expr {
                Some(Literal::Number(n)) => Ok(Some(Literal::Number(-n))),
                _ => Err(String::from("Invalid operand for - operator.")),
            }
        }
        Operator::BANG => {
            match expr {
                Some(Literal::Boolean(b)) => Ok(Some(Literal::Boolean(!b))),
                Some(Literal::String(s)) => Ok(Some(Literal::Boolean(s.is_empty()))),
                Some(Literal::Number(n)) => Ok(Some(Literal::Boolean(n == 0.0))),
                None => Ok(Some(Literal::Boolean(true))),
            }
        }
        _ => Err(String::from("Found invalid operator in unary expresion.")),
    }
}

impl Evaluate for Expr {
    fn evaluate(self) -> Result<Option<Literal>, String> {
        match self {
            Expr::Binary(left, op, right) => evaluate_binary_expression(left, op, right),
            Expr::UnaryExpr(op, expr) => evaluate_unary_expression(op, expr),
            Expr::Grouping(expr) => expr.evaluate(),
            Expr::Literal(literal) => literal.evaluate(),
        }
    }
}

impl Evaluate for Stmt {
    fn evaluate(self) -> Result<Option<Literal>, String> {
        match self {
            Stmt::Expression(expr) => {
                let _ = expr.evaluate();
                Ok(None)
            },
            Stmt::Print(expr) => {
                let result = expr.evaluate()?;
                println!("{}", result.unwrap());
                Ok(None)
            }
        }
    }
}