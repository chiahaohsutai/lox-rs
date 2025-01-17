use crate::tokens::{self, Keyword, Operator, Punctuation, Token};
use std::{
    fmt::Display,
    iter::Peekable,
    result::Result,
    slice::Iter,
};

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


#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Operator, Box<Expr>),
    UnaryExpr(Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralExpr),
    Variable(String),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Self::UnaryExpr(op, expr) => write!(f, "({} {})", op, expr),
            Self::Grouping(expr) => write!(f, "(group {})", expr),
            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Variable(name) => write!(f, "(variable {})", name),
        }
    }
}

fn expression(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    Ok(equality(tokens)?)
}

fn equality(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (mut expr, mut tokens) = comparison(tokens)?;
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::EQUALEQ | Operator::BANGEQ => {
                let _ = tokens.next();
                let rhs = comparison(tokens)?;
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    }
    Ok((expr, tokens))
}

fn comparison(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (mut expr, mut tokens) = term(tokens)?;
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::LESS | Operator::LESSEQ | Operator::GREATER | Operator::GREATEREQ => {
                let _ = tokens.next();
                let rhs = term(tokens)?;
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    }
    Ok((expr, tokens))
}

fn term(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (mut expr, mut tokens) = factor(tokens)?;
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::PLUS | Operator::MINUS => {
                let _ = tokens.next();
                let rhs = factor(tokens)?;
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    }
    Ok((expr, tokens))
}

fn factor(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (mut expr, mut tokens) = unary(tokens)?;
    while let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::STAR | Operator::SLASH => {
                let _ = tokens.next();
                let rhs = unary(tokens)?;
                expr = Expr::Binary(Box::new(expr), *op, Box::new(rhs.0));
                tokens = rhs.1;
            }
            _ => break,
        }
    }
    Ok((expr, tokens))
}

fn unary(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let mut tokens = tokens;
    if let Some(&Token::OPER(op, _)) = tokens.peek() {
        match op {
            Operator::MINUS | Operator::BANG => {
                let _ = tokens.next();
                let (expr, tokens) = unary(tokens)?;
                return Ok((Expr::UnaryExpr(*op, Box::new(expr)), tokens));
            }
            _ => primary(tokens),
        }
    } else {
        primary(tokens)
    }
}

fn primary(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Expr, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let mut tokens = tokens;
    if let Some(token) = tokens.next() {
        match token {
            tokens::Token::NUM(n, _) => Ok((Expr::Literal(LiteralExpr::Number(*n)), tokens)),
            tokens::Token::STR(s, _) => {
                Ok((Expr::Literal(LiteralExpr::String(String::from(s))), tokens))
            }
            tokens::Token::KWD(Keyword::TRUE, _) => {
                Ok((Expr::Literal(LiteralExpr::Boolean(true)), tokens))
            }
            tokens::Token::KWD(Keyword::FALSE, _) => {
                Ok((Expr::Literal(LiteralExpr::Boolean(false)), tokens))
            }
            tokens::Token::KWD(Keyword::NIL, _) => Ok((Expr::Literal(LiteralExpr::Nil), tokens)),
            tokens::Token::IDEN(identifier, _) => {
                Ok((Expr::Variable(String::from(identifier)), tokens))
            }
            tokens::Token::PUNC(Punctuation::LPAREN, _) => {
                let (expr, mut tokens) = expression(tokens)?;
                if let Some(Token::PUNC(Punctuation::RPAREN, _)) = tokens.peek() {
                    let _ = tokens.next();
                    Ok((Expr::Grouping(Box::new(expr)), tokens))
                } else {
                    Err((String::from("Expected closing parenthesis"), tokens))
                }
            }
            _ => Err((format!("Unexpected token {:?}", token), tokens)),
        }
    } else {
        Err((String::from("Unexpected end of input"), tokens))
    }
}

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}

fn statement(
    mut tokens: Peekable<Iter<Token>>,
) -> Result<(Stmt, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    match tokens.peek() {
        Some(Token::KWD(Keyword::PRINT, _)) => {
            let _ = tokens.next();
            print_stmt(tokens)
        }
        Some(_) => expr_stmt(tokens),
        None => Err((String::from("Unexpected end of input"), tokens)),
    }
}

fn print_stmt(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Stmt, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (expr, mut tokens) = expression(tokens)?;
    if let Some(Token::PUNC(Punctuation::SEMICOLON, _)) = tokens.peek() {
        let _ = tokens.next();
        Ok((Stmt::Print(expr), tokens))
    } else {
        Err((String::from("Expected semicolon"), tokens))
    }
}

fn expr_stmt(
    tokens: Peekable<Iter<Token>>,
) -> Result<(Stmt, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    let (expr, mut tokens) = expression(tokens)?;
    if let Some(Token::PUNC(Punctuation::SEMICOLON, _)) = tokens.peek() {
        let _ = tokens.next();
        Ok((Stmt::Expression(expr), tokens))
    } else {
        Err((String::from("Expected semicolon"), tokens))
    }
}

pub enum Declaration {
    Variable(String, Option<Expr>),
    Statement(Stmt),
}

fn declaration(
    mut tokens: Peekable<Iter<Token>>,
) -> Result<(Declaration, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    match tokens.peek() {
        Some(Token::KWD(Keyword::VAR, _)) => {
            let _ = tokens.next();
            let (declaration, tokens) = variable(tokens)?;
            Ok((declaration, tokens))
        }
        _ => {
            let (stmt, tokens) = statement(tokens)?;
            Ok((Declaration::Statement(stmt), tokens))
        }
    }
}

fn variable(
    mut tokens: Peekable<Iter<Token>>,
) -> Result<(Declaration, Peekable<Iter<Token>>), (String, Peekable<Iter<Token>>)> {
    if let Some(Token::IDEN(name, _)) = tokens.peek() {
        let _ = tokens.next();
        if let Some(Token::OPER(Operator::EQUAL, _)) = tokens.peek() {
            let _ = tokens.next();
            let (expr, mut tokens) = expression(tokens)?;
            if let Some(Token::PUNC(Punctuation::SEMICOLON, _)) = tokens.peek() {
                let _ = tokens.next();
                Ok((
                    Declaration::Variable(String::from(name), Some(expr)),
                    tokens,
                ))
            } else {
                Err((String::from("Expected semicolon"), tokens))
            }
        } else if let Some(Token::PUNC(Punctuation::SEMICOLON, _)) = tokens.peek() {
            let _ = tokens.next();
            Ok((Declaration::Variable(String::from(name), None), tokens))
        } else {
            Err((String::from("Expected semicolon"), tokens))
        }
    } else {
        Err((String::from("Expected identifier"), tokens))
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Declaration>, String> {
    let mut declarations: Vec<Declaration> = vec![];
    let mut tokens = tokens.iter().peekable();

    while tokens.peek().is_some() {
        let token = tokens.peek();
        match token {
            Some(Token::EOF(_)) | None => break,
            _ => {
                match declaration(tokens) {
                    Ok((declaration, new_tokens)) => {
                        declarations.push(declaration);
                        tokens = new_tokens;
                    }
                    Err((_, mut new_tokens)) => {
                        synchronize(&mut new_tokens);
                        tokens = new_tokens;
                    }
                }
            }
        }
    }
    Ok(declarations)
}

fn synchronize(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(token) = tokens.peek() {
        match token {
            Token::PUNC(Punctuation::SEMICOLON, _) => break,
            Token::KWD(Keyword::CLASS, _) => break,
            Token::KWD(Keyword::FUN, _) => break,
            Token::KWD(Keyword::VAR, _) => break,
            Token::KWD(Keyword::FOR, _) => break,
            Token::KWD(Keyword::IF, _) => break,
            Token::KWD(Keyword::WHILE, _) => break,
            Token::KWD(Keyword::PRINT, _) => break,
            Token::KWD(Keyword::RETURN, _) => break,
            Token::EOF(_) => break,
            _ => (),
        }
        let _ = tokens.next();
    }
}
