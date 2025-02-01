use crate::{tokenizer::Token, Enviorment, Object};
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum LogicalOp {
    And,
    Or,
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Bang => write!(f, "!"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    EqualEqual,
    BangEqual,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::EqualEqual => write!(f, "=="),
            Self::BangEqual => write!(f, "!="),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Option<Object>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Grouping(Box<Expression>),
    Variable(String),
    Assignment(String, Box<Expression>),
    Logical(Box<Expression>, LogicalOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(obj) => write!(f, "{:?}", obj),
            Self::Unary(op, expr) => write!(f, "({:?} {})", op, expr),
            Self::Binary(left, op, right) => write!(f, "({} {:?} {})", left, op, right),
            Self::Grouping(expr) => write!(f, "(group {})", expr),
            Self::Variable(name) => write!(f, "{}", name),
            Self::Assignment(name, expr) => write!(f, "({} = {})", name, expr),
            Self::Logical(left, op, right) => write!(f, "({} {:?} {})", left, op, right),
            Self::Call(callee, args) => {
                let args: Vec<String> = args.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "({} {})", callee, args.join(", "))
            }
        }
    }
}

impl TryFrom<Vec<Token>> for Expression {
    type Error = String;
    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        let mut tokens: Vec<Token> = tokens.iter().cloned().rev().collect();
        produce_expression(&mut tokens)
    }
}

impl Expression {
    pub fn eval(&self, env: &mut Enviorment) -> Result<Option<Object>, String> {
        match self {
            Self::Literal(obj) => todo!(),
            Self::Unary(op, expr) => todo!(),
            Self::Binary(left, op, right) => todo!(),
            Self::Grouping(expr) => todo!(),
            Self::Variable(name) => todo!(),
            Self::Assignment(name, expr) => todo!(),
            Self::Logical(left, op, right) => todo!(),
            Self::Call(callee, args) => todo!(),
        }
    }
}

fn produce_expression(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    produce_assignment(tokens)
}

fn produce_assignment(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let lhs = produce_logical_or(tokens)?;
    let next = tokens.pop();
    if let Some(Token::Equal(line)) = next {
        let rhs = produce_assignment(tokens)?;
        if let Expression::Variable(name) = lhs {
            Ok(Expression::Assignment(name, Box::new(rhs)))
        } else {
            Err(format!("Invalid assignment target at line {}", line))
        }
    } else if let Some(token) = next {
        tokens.push(token);
        Ok(lhs)
    } else {
        Ok(lhs)
    }
}

fn produce_logical_or(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let mut expr = produce_logical_and(tokens)?;
    while let Some(Token::Or(line)) = tokens.last() {
        tokens.pop();
        let right = produce_logical_and(tokens)?;
        expr = Expression::Logical(Box::new(expr), LogicalOp::Or, Box::new(right));
    }
    Ok(expr)
}

fn produce_logical_and(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let mut expr = produce_equality(tokens)?;
    while let Some(Token::And(line)) = tokens.last() {
        tokens.pop();
        let right = produce_equality(tokens)?;
        expr = Expression::Logical(Box::new(expr), LogicalOp::And, Box::new(right));
    }
    Ok(expr)
}

fn produce_equality(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let mut expr = produce_comparison(tokens)?;
    while let Some(Token::BangEqual(line)) | Some(Token::EqualEqual(line)) = tokens.last() {
        let op = match tokens.pop().unwrap() {
            Token::BangEqual(_) => BinaryOp::BangEqual,
            Token::EqualEqual(_) => BinaryOp::EqualEqual,
            _ => unreachable!(),
        };
        let right = produce_comparison(tokens)?;
        expr = Expression::Binary(Box::new(expr), op, Box::new(right));
    }
    Ok(expr)
}

fn produce_comparison(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let mut expr = produce_term(tokens)?;
    while let Some(Token::Greater(line))
    | Some(Token::GreaterEqual(line))
    | Some(Token::Less(line))
    | Some(Token::LessEqual(line)) = tokens.last()
    {
        let op = match tokens.pop().unwrap() {
            Token::Greater(_) => BinaryOp::Greater,
            Token::GreaterEqual(_) => BinaryOp::GreaterEqual,
            Token::Less(_) => BinaryOp::Less,
            Token::LessEqual(_) => BinaryOp::LessEqual,
            _ => unreachable!(),
        };
        let right = produce_term(tokens)?;
        expr = Expression::Binary(Box::new(expr), op, Box::new(right));
    }
    Ok(expr)
}

fn produce_term(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let mut expr = produce_factor(tokens)?;
    while let Some(Token::Minus(line)) | Some(Token::Plus(line)) = tokens.last() {
        let op = match tokens.pop().unwrap() {
            Token::Minus(_) => BinaryOp::Minus,
            Token::Plus(_) => BinaryOp::Plus,
            _ => unreachable!(),
        };
        let right = produce_factor(tokens)?;
        expr = Expression::Binary(Box::new(expr), op, Box::new(right));
    }
    Ok(expr)
}

fn produce_factor(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let expr = produce_unary(tokens)?;
    while let Some(Token::Star(line)) | Some(Token::Slash(line)) = tokens.last() {
        let op = match tokens.pop().unwrap() {
            Token::Star(_) => BinaryOp::Star,
            Token::Slash(_) => BinaryOp::Slash,
            _ => unreachable!(),
        };
        let right = produce_unary(tokens)?;
        return Ok(Expression::Binary(Box::new(expr), op, Box::new(right)));
    }
    Ok(expr)
}

fn produce_unary(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    if let Some(Token::Minus(line)) | Some(Token::Bang(line)) = tokens.last() {
        let op = match tokens.pop().unwrap() {
            Token::Minus(_) => UnaryOp::Minus,
            Token::Bang(_) => UnaryOp::Bang,
            _ => unreachable!(),
        };
        let expr = produce_unary(tokens)?;
        Ok(Expression::Unary(op, Box::new(expr)))
    } else {
        produce_call(tokens)
    }
}

fn produce_call(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let expr = produce_primary(tokens)?;
    if let Some(Token::LeftParen(_)) = tokens.last() {
        tokens.pop();
        let mut args = vec![];
        args.push(produce_expression(tokens)?);
        while let Some(Token::Comma(_)) = tokens.last() {
            tokens.pop();
            args.push(produce_expression(tokens)?);
            if let Some(Token::RightParen(_)) = tokens.last() {
                break;
            }
        }
        if let Some(Token::RightParen(_)) = tokens.pop() {
            Ok(Expression::Call(Box::new(expr), args))
        } else {
            Err(String::from("Expected ')'"))
        }
    } else {
        Ok(expr)
    }
}

fn produce_primary(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    match tokens.pop() {
        Some(Token::True(_)) => Ok(Expression::Literal(Some(Object::Boolean(true)))),
        Some(Token::False(_)) => Ok(Expression::Literal(Some(Object::Boolean(false)))),
        Some(Token::Nil(_)) => Ok(Expression::Literal(None)),
        Some(Token::Number(num, _)) => Ok(Expression::Literal(Some(Object::Number(num)))),
        Some(Token::String(string, _)) => Ok(Expression::Literal(Some(Object::String(string)))),
        Some(Token::Identifier(name, _)) => Ok(Expression::Variable(name)),
        Some(Token::LeftParen(_)) => {
            let expr = produce_expression(tokens)?;
            if let Some(Token::RightParen(_)) = tokens.pop() {
                Ok(Expression::Grouping(Box::new(expr)))
            } else {
                Err(String::from("Expected ')'"))
            }
        }
        Some(token) => Err(format!("Unexpected token '{:?}'", token)),
        None => Err(String::from("Unexpected end of input")),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_produce_arithmetic_expression() {
        let program = "1 + 2 * 3;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Binary(
                Box::new(Expression::Literal(Some(Object::Number(1.0)))),
                BinaryOp::Plus,
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Some(Object::Number(2.0)))),
                    BinaryOp::Star,
                    Box::new(Expression::Literal(Some(Object::Number(3.0))))
                ))
            )
        );
    }

    #[test]
    fn test_produce_logical_expression() {
        let program = "true and false or true;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Logical(
                Box::new(Expression::Logical(
                    Box::new(Expression::Literal(Some(Object::Boolean(true)))),
                    LogicalOp::And,
                    Box::new(Expression::Literal(Some(Object::Boolean(false)))
                    )
                )),
                LogicalOp::Or,
                Box::new(Expression::Literal(Some(Object::Boolean(true)))
            )
        ));
    }

    #[test]
    fn test_produce_grouped_expression() {
        let program = "(1 + 2) * 3;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Binary(
                Box::new(Expression::Grouping(Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Some(Object::Number(1.0)))),
                    BinaryOp::Plus,
                    Box::new(Expression::Literal(Some(Object::Number(2.0)))
                    )
                )))),
                BinaryOp::Star,
                Box::new(Expression::Literal(Some(Object::Number(3.0)))
            )
        ));
    }

    #[test]
    fn test_produce_assignment_expression() {
        let program = "a = 1;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Assignment("a".to_string(), Box::new(Expression::Literal(Some(Object::Number(1.0))))
        ));
    }

    #[test]
    fn test_produce_call_expression() {
        let program = "foo(1, 2);";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Call(
                Box::new(Expression::Variable("foo".to_string())),
                vec![
                    Expression::Literal(Some(Object::Number(1.0))),
                    Expression::Literal(Some(Object::Number(2.0)))
                ]
            )
        );
    }

    #[test]
    fn test_produce_unary_expression() {
        let program = "-1;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Unary(UnaryOp::Minus, Box::new(Expression::Literal(Some(Object::Number(1.0))))
        ));
    }

    #[test]
    fn test_produce_logical_not_expression() {
        let program = "!true;";
        let tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::try_from(tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Unary(UnaryOp::Bang, Box::new(Expression::Literal(Some(Object::Boolean(true))))
        ));
    }
}
