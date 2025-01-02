use std::{fmt::Display, slice::Iter, iter::Peekable, result::Result};
use crate::tokens::{self, Keyword, Operator, Punctuation, Token};

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

pub fn parse(tokens: Vec<Token>) -> Result<Expr, String> {
    let tokens = tokens.iter().peekable();
    Ok(expression(tokens)?.0)
}

fn expression(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
   Ok(equality(tokens)?)
}

fn equality(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
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
    };
    Ok((expr, tokens))
}

fn comparison(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
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
    };
    Ok((expr, tokens))
}

fn term(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
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
    };
    Ok((expr, tokens))
}

fn factor(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
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
    };
    Ok((expr, tokens))
}

fn unary(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
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

fn primary(tokens: Peekable<Iter<Token>>) -> Result<(Expr, Peekable<Iter<Token>>), String> {
    let mut tokens = tokens;
    if let Some(token) = tokens.next() {
        match token {
            tokens::Token::NUM(n, _) => Ok((Expr::Literal(LiteralExpr::Number(*n)), tokens)),
            tokens::Token::STR(s, _) => Ok((Expr::Literal(LiteralExpr::String(String::from(s))), tokens)),
            tokens::Token::KWD(Keyword::TRUE, _) => Ok((Expr::Literal(LiteralExpr::Boolean(true)), tokens)),
            tokens::Token::KWD(Keyword::FALSE, _) => Ok((Expr::Literal(LiteralExpr::Boolean(false)), tokens)),
            tokens::Token::KWD(Keyword::NIL, _) => Ok((Expr::Literal(LiteralExpr::Nil), tokens)),
            tokens::Token::PUNC(Punctuation::LPAREN, _) => {
                let (expr, mut tokens) = expression(tokens)?;
                if let Some(Token::PUNC(Punctuation::RPAREN, _)) = tokens.peek() {
                    let _ = tokens.next();
                    Ok((Expr::Grouping(Box::new(expr)), tokens))
                } else {
                    Err(String::from("Expected closing parenthesis"))
                }
            }
            _ => Err(format!("Unexpected token {:?}", token)),
        }
    } else {
        Err(String::from("Unexpected end of input"))
    }
}

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}

pub fn parse_stmt(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
    let mut statements: Vec<Stmt> = vec![];
    let mut tokens = tokens.iter().peekable();

    while tokens.peek().is_some() {
        let token = tokens.next().unwrap();
        match token {
            Token::EOF(_) => break,
            _ => {
                let (stmt, tkns) = statement(tokens)?;
                statements.push(stmt);
                tokens = tkns;
            },
        }
    };
    Ok(statements)
}

fn statement(mut tokens: Peekable<Iter<Token>>) -> Result<(Stmt, Peekable<Iter<Token>>), String> {
    match tokens.next() {
        Some(Token::KWD(Keyword::PRINT, _)) => print_stmt(tokens), 
        Some(_) => todo!(),
        None => todo!(),
    }
}

fn print_stmt(mut tokens: Peekable<Iter<Token>>) -> Result<(Stmt, Peekable<Iter<Token>>), String> {
    let (expr, mut tokens) = expression(tokens)?;
    if let Some(Token::PUNC(Punctuation::SEMICOLON, _)) = tokens.peek() {
        let _ = tokens.next();
        Ok((Stmt::Print(expr), tokens))
    } else {
        Err(String::from("Expected semicolon"))
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
        let ast = crate::parser::parse(tokens).unwrap();
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
        let ast = crate::parser::parse(tokens).unwrap();
        assert_eq!(format!("{}", ast), "(group (+ 1 2))");
    }
    #[test]
    fn test_expression_with_multiple_operators() {
        let tokens = vec![
            crate::tokens::Token::NUM(1.0, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::PLUS, 0),
            crate::tokens::Token::NUM(2.0, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::STAR, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::MINUS, 0),
            crate::tokens::Token::NUM(3.0, 0),
        ];
        let ast = crate::parser::parse(tokens).unwrap();
        assert_eq!(format!("{}", ast), "(+ 1 (* 2 (- 3)))");
    }
    #[test]
    fn test_unterminated_parenthesis() {
        let tokens = vec![
            crate::tokens::Token::PUNC(crate::tokens::Punctuation::LPAREN, 0),
            crate::tokens::Token::NUM(1.0, 0),
            crate::tokens::Token::OPER(crate::tokens::Operator::PLUS, 0),
            crate::tokens::Token::NUM(2.0, 0),
        ];
        let ast = crate::parser::parse(tokens);
        assert_eq!(ast, Err(String::from("Expected closing parenthesis")));
    }
}