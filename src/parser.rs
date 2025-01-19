use crate::tokenizer::{Lexeme, Token};

#[derive(PartialEq, Debug, Clone, Copy)]
enum UnaryOp {
    Minus,
    Bang,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum BinaryOp {
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

#[derive(PartialEq, Debug, Clone)]
enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}

enum Expression {
    Literal(Option<Literal>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Grouping(Box<Expression>),
    Variable(String),
}

pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Var(String, Expression),
}

pub fn parse(tokens: Vec<Lexeme>) -> (Vec<Statement>, Vec<String>) {
    let mut tokens: Vec<_> = tokens.into_iter().rev().collect();
    let mut statements = Vec::new();
    let mut errors = Vec::new();

    while let Some(lexeme) = tokens.pop() {
        let stmt = match lexeme.token() {
            Token::Print => parse_print_stmt(&mut tokens, lexeme.line()),
            _ => todo!(),
        };
        match stmt {
            Ok(stmt) => statements.push(stmt),
            Err(err) => {
                errors.push(err);
                synchronize(&mut tokens);
            }
        };
    }
    (statements, errors)
}

fn synchronize(tokens: &mut Vec<Lexeme>) {
    while let Some(lexeme) = tokens.last() {
        match lexeme.token() {
            Token::Semicolon
            | Token::Class
            | Token::Fun
            | Token::Var
            | Token::For
            | Token::If
            | Token::While
            | Token::Print
            | Token::Return => return,
            _ => {
                tokens.pop();
                ()
            }
        }
    }
}

fn parse_print_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    let expression = parse_expression(tokens)?;
    match tokens.last() {
        Some(lexeme) if *lexeme.token() == Token::Semicolon => {
            tokens.pop();
            Ok(Statement::Print(expression))
        }
        _ => Err(format!("Expected semicolon in line {line}")),
    }
}

fn parse_expression(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    parse_equality(tokens)
}

fn parse_equality(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_comparison(tokens)?;
    while let Some(lexeme) = tokens.last() {
        let op = match lexeme.token() {
            Token::EqualEqual => BinaryOp::EqualEqual,
            Token::BangEqual => BinaryOp::BangEqual,
            _ => return Ok(expression),
        };
        tokens.pop();
        let right = Box::new(parse_comparison(tokens)?);
        expression = Expression::Binary(Box::new(expression), op, right);
    }
    Ok(expression)
}

fn parse_comparison(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_term(tokens)?;
    while let Some(lexeme) = tokens.last() {
        let op = match lexeme.token() {
            Token::Greater => BinaryOp::Greater,
            Token::GreaterEqual => BinaryOp::GreaterEqual,
            Token::Less => BinaryOp::Less,
            Token::LessEqual => BinaryOp::LessEqual,
            _ => return Ok(expression),
        };
        tokens.pop();
        let right = Box::new(parse_term(tokens)?);
        expression = Expression::Binary(Box::new(expression), op, right);
    }
    Ok(expression)
}

fn parse_term(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_factor(tokens)?;
    while let Some(lexeme) = tokens.last() {
        let op = match lexeme.token() {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
            _ => return Ok(expression),
        };
        tokens.pop();
        let right = Box::new(parse_factor(tokens)?);
        expression = Expression::Binary(Box::new(expression), op, right);
    }
    Ok(expression)
}

fn parse_factor(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_unary(tokens)?;
    while let Some(lexeme) = tokens.last() {
        let op = match lexeme.token() {
            Token::Star => BinaryOp::Star,
            Token::Slash => BinaryOp::Slash,
            _ => return Ok(expression),
        };
        tokens.pop();
        let right = Box::new(parse_unary(tokens)?);
        expression = Expression::Binary(Box::new(expression), op, right);
    }
    Ok(expression)
}

fn parse_unary(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    if let Some(lexeme) = tokens.last() {
        let op = match lexeme.token() {
            Token::Minus => UnaryOp::Minus,
            Token::Bang => UnaryOp::Bang,
            _ => return parse_primary(tokens),
        };
        tokens.pop();
        let right = Box::new(parse_unary(tokens)?);
        Ok(Expression::Unary(op, right))
    } else {
        parse_primary(tokens)
    }
}

fn parse_primary(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    if let Some(lexeme) = tokens.pop() {
        match lexeme.token() {
            Token::Number(n) => Ok(Expression::Literal(Some(Literal::Number(*n)))),
            Token::String(s) => Ok(Expression::Literal(Some(Literal::String(s.clone())))),
            Token::True => Ok(Expression::Literal(Some(Literal::Bool(true)))),
            Token::False => Ok(Expression::Literal(Some(Literal::Bool(false)))),
            Token::Nil => Ok(Expression::Literal(None)),
            Token::LeftParen => {
                let expression = parse_expression(tokens)?;
                match tokens.pop() {
                    Some(lexeme) if *lexeme.token() == Token::RightParen => {
                        Ok(Expression::Grouping(Box::new(expression)))
                    }
                    _ => Err(format!("Expected ')' in line {}", lexeme.line())),
                }
            }
            Token::Identifier(s) => Ok(Expression::Variable(s.clone())),
            _ => Err(format!("Unexpected token in line {}", lexeme.line())),
        }
    } else {
        Err("Unexpected end of file".to_string())
    }
}

trait Evaluate {
    fn evaluate(self) -> Result<Option<Literal>, String>;
}

impl Evaluate for Expression {
    fn evaluate(self) -> Result<Option<Literal>, String> {
        match self {
            Self::Literal(literal) => Ok(literal),
            Self::Grouping(expression) => expression.evaluate(),
            Self::Unary(op, right) => eval_unary_expr(op, *right),
            Self::Binary(left, op, right) => eval_binary_expr(*left, op, *right),
            Self::Variable(name) => todo!(),
        }
    }
}

fn eval_unary_expr(op: UnaryOp, right: Expression) -> Result<Option<Literal>, String> {
    let right = right.evaluate()?;
    match (op, right) {
        (UnaryOp::Minus, Some(Literal::Number(n))) => Ok(Some(Literal::Number(-n))),
        (UnaryOp::Bang, Some(Literal::Number(_))) => Ok(Some(Literal::Bool(false))),
        (UnaryOp::Bang, Some(Literal::String(_))) => Ok(Some(Literal::Bool(false))),
        (UnaryOp::Bang, Some(Literal::Bool(b))) => Ok(Some(Literal::Bool(!b))),
        (UnaryOp::Bang, None) => Ok(Some(Literal::Bool(true))),
        _ => Err("Invalid unary operation".to_string()),
    }
}

fn eval_binary_expr(
    left: Expression,
    op: BinaryOp,
    right: Expression,
) -> Result<Option<Literal>, String> {
    let left = left.evaluate()?;
    let right = right.evaluate()?;
    match (left, right) {
        (Some(Literal::Number(left)), Some(Literal::Number(right))) => match op {
            BinaryOp::Plus => Ok(Some(Literal::Number(left + right))),
            BinaryOp::Minus => Ok(Some(Literal::Number(left - right))),
            BinaryOp::Star => Ok(Some(Literal::Number(left * right))),
            BinaryOp::Slash => Ok(Some(Literal::Number(left / right))),
            BinaryOp::Greater => Ok(Some(Literal::Bool(left > right))),
            BinaryOp::GreaterEqual => Ok(Some(Literal::Bool(left >= right))),
            BinaryOp::Less => Ok(Some(Literal::Bool(left < right))),
            BinaryOp::LessEqual => Ok(Some(Literal::Bool(left <= right))),
            BinaryOp::EqualEqual => Ok(Some(Literal::Bool(left == right))),
            BinaryOp::BangEqual => Ok(Some(Literal::Bool(left != right))),
        },
        (Some(Literal::String(left)), Some(Literal::String(right))) => match op {
            BinaryOp::Plus => Ok(Some(Literal::String(left + &right))),
            BinaryOp::EqualEqual => Ok(Some(Literal::Bool(left == right))),
            BinaryOp::BangEqual => Ok(Some(Literal::Bool(left != right))),
            _ => Err("Invalid binary operation".to_string()),
        },
        (Some(Literal::Bool(left)), Some(Literal::Bool(right))) => match op {
            BinaryOp::EqualEqual => Ok(Some(Literal::Bool(left == right))),
            BinaryOp::BangEqual => Ok(Some(Literal::Bool(left != right))),
            _ => Err("Invalid binary operation".to_string()),
        },
        _ => Err("Invalid binary operation".to_string()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval_addition() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Plus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_subtraction() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(-1.0))));
    }

    #[test]
    fn test_eval_multiplication() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Star,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(6.0))));
    }

    #[test]
    fn test_eval_division() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(6.0)))),
            BinaryOp::Slash,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_greater() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::Greater,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_greater_equal() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::GreaterEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Less,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less_equal() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::LessEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_equal_equal() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::EqualEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_bang_equal() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::BangEqual,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_unary_minus() {
        let expression = Expression::Unary(
            UnaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(-3.0))));
    }

    #[test]
    fn test_eval_unary_bang() {
        let expression = Expression::Unary(
            UnaryOp::Bang,
            Box::new(Expression::Literal(Some(Literal::Bool(true)))),
        );
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Bool(false))));
    }

    #[test]
    fn test_eval_grouping() {
        let expression =
            Expression::Grouping(Box::new(Expression::Literal(Some(Literal::Number(3.0)))));
        assert_eq!(expression.evaluate(), Ok(Some(Literal::Number(3.0))));
    }
}
