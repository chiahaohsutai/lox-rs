use crate::{tokenizer::Token, Create, Enviorment, Eval, Object};
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

impl Eval for Expression {
    type Output = Option<Object>;
    fn eval(&self, env: &mut Enviorment) -> Result<Self::Output, String> {
        match self {
            Self::Literal(obj) => Ok(obj.clone()),
            Self::Grouping(expr) => expr.eval(env),
            Self::Variable(name) => Ok(env.get(name.clone())?),
            Self::Assignment(name, expr) => eval_assignment_expr(name, expr, env),
            Self::Unary(op, expr) => eval_unary_expr(op, expr, env),
            Self::Binary(left, op, right) => eval_binary_expr(op, left, right, env),
            Self::Logical(left, op, right) => eval_logical_expr(left, op, right, env),
            Self::Call(callee, args) => eval_call_expr(callee, args, env),
        }
    }
}

impl Create for Expression {
    type Output = Expression;
    fn create(tokens: &mut Vec<Token>) -> Result<Self::Output, String> {
        let mut tkns = tokens.iter().cloned().rev().collect();
        let expr = produce_expression(&mut tkns);
        *tokens = tkns.iter().cloned().rev().collect();
        expr
    }
}

fn eval_unary_expr(
    op: &UnaryOp,
    expr: &Expression,
    env: &mut Enviorment,
) -> Result<Option<Object>, String> {
    let obj = expr.eval(env)?;
    match op {
        UnaryOp::Minus => match obj {
            Some(Object::Number(num)) => Ok(Some(Object::Number(-num))),
            _ => Err(format!("Unary '-' expects a number, got {:?}", obj)),
        },
        UnaryOp::Bang => match obj {
            Some(Object::Boolean(b)) => Ok(Some(Object::Boolean(!b))),
            _ => Err(format!("Unary '!' expects a boolean, got {:?}", obj)),
        },
    }
}

fn eval_binary_expr(
    op: &BinaryOp,
    left: &Expression,
    right: &Expression,
    env: &mut Enviorment,
) -> Result<Option<Object>, String> {
    let left = left
        .eval(env)?
        .ok_or_else(|| String::from("Left operand is nil"))?;
    let right = right
        .eval(env)?
        .ok_or_else(|| String::from("Right operand is nil"))?;
    match op {
        BinaryOp::Plus => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Number(l + r))),
            (Object::String(l), Object::String(r)) => {
                Ok(Some(Object::String(format!("{}{}", l, r))))
            }
            _ => Err(format!(
                "Binary '+' expects two numbers or two strings, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::Minus => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Number(l - r))),
            _ => Err(format!(
                "Binary '-' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::Star => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Number(l * r))),
            _ => Err(format!(
                "Binary '*' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::Slash => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Number(l / r))),
            _ => Err(format!(
                "Binary '/' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::Greater => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Boolean(l > r))),
            _ => Err(format!(
                "Binary '>' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::GreaterEqual => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Boolean(l >= r))),
            _ => Err(format!(
                "Binary '>=' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::Less => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Boolean(l < r))),
            _ => Err(format!(
                "Binary '<' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::LessEqual => match (&left, &right) {
            (Object::Number(l), Object::Number(r)) => Ok(Some(Object::Boolean(l <= r))),
            _ => Err(format!(
                "Binary '<=' expects two numbers, got {:?} and {:?}",
                left, right
            )),
        },
        BinaryOp::EqualEqual => Ok(Some(Object::Boolean(left == right))),
        BinaryOp::BangEqual => Ok(Some(Object::Boolean(left != right))),
    }
}

fn eval_call_expr(
    callee: &Expression,
    args: &Vec<Expression>,
    env: &mut Enviorment,
) -> Result<Option<Object>, String> {
    let callee = callee.eval(env)?;
    let mut args_values = vec![];
    for arg in args {
        args_values.push(arg.eval(env)?);
    }

    match callee {
        Some(Object::Function(fun, _, arity)) => {
            if args.len() != arity {
                return Err(format!("Expected {} arguments, got {}", arity, args.len()));
            }
            fun(args_values)
        }
        Some(obj) => Err(format!("{} is not a function", obj)),
        None => Err(String::from("Callee is nil")),
    }
}

fn eval_assignment_expr(
    name: &str,
    expr: &Expression,
    env: &mut Enviorment,
) -> Result<Option<Object>, String> {
    let value = expr.eval(env)?;
    env.assign(name.to_string(), value.clone())?;
    Ok(value)
}

fn eval_logical_expr(
    left: &Expression,
    op: &LogicalOp,
    right: &Expression,
    env: &mut Enviorment,
) -> Result<Option<Object>, String> {
    let left = left.eval(env)?;
    let right = right.eval(env)?;
    match op {
        LogicalOp::And => match (&left, &right) {
            (Some(Object::Boolean(l)), Some(Object::Boolean(r))) => {
                Ok(Some(Object::Boolean(*l && *r)))
            }
            _ => Err(format!(
                "Logical 'and' expects two booleans, got {:?} and {:?}",
                left, right
            )),
        },
        LogicalOp::Or => match (&left, &right) {
            (Some(Object::Boolean(l)), Some(Object::Boolean(r))) => {
                Ok(Some(Object::Boolean(*l || *r)))
            }
            _ => Err(format!(
                "Logical 'or' expects two booleans, got {:?} and {:?}",
                left, right
            )),
        },
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
    while let Some(Token::Greater(_))
    | Some(Token::GreaterEqual(_))
    | Some(Token::Less(_))
    | Some(Token::LessEqual(_)) = tokens.last()
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
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
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
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Logical(
                Box::new(Expression::Logical(
                    Box::new(Expression::Literal(Some(Object::Boolean(true)))),
                    LogicalOp::And,
                    Box::new(Expression::Literal(Some(Object::Boolean(false))))
                )),
                LogicalOp::Or,
                Box::new(Expression::Literal(Some(Object::Boolean(true))))
            )
        );
    }

    #[test]
    fn test_produce_grouped_expression() {
        let program = "(1 + 2) * 3;";
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Binary(
                Box::new(Expression::Grouping(Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Some(Object::Number(1.0)))),
                    BinaryOp::Plus,
                    Box::new(Expression::Literal(Some(Object::Number(2.0))))
                )))),
                BinaryOp::Star,
                Box::new(Expression::Literal(Some(Object::Number(3.0))))
            )
        );
    }

    #[test]
    fn test_produce_assignment_expression() {
        let program = "a = 1;";
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Assignment(
                "a".to_string(),
                Box::new(Expression::Literal(Some(Object::Number(1.0))))
            )
        );
    }

    #[test]
    fn test_produce_call_expression() {
        let program = "foo(1, 2);";
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
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
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Unary(
                UnaryOp::Minus,
                Box::new(Expression::Literal(Some(Object::Number(1.0))))
            )
        );
    }

    #[test]
    fn test_produce_logical_not_expression() {
        let program = "!true;";
        let mut tokens: Vec<Token> = tokenize(program)
            .iter()
            .map(|t| t.clone().unwrap())
            .collect();
        let expr = Expression::create(&mut tokens).unwrap();
        assert_eq!(
            expr,
            Expression::Unary(
                UnaryOp::Bang,
                Box::new(Expression::Literal(Some(Object::Boolean(true))))
            )
        );
    }
    #[test]
    fn test_eval_literal() {
        let expr = Expression::Literal(Some(Object::Number(1.0)));
        let mut env = Enviorment::default();
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(1.0)));
    }

    #[test]
    fn test_eval_grouping() {
        let expr = Expression::Grouping(Box::new(Expression::Literal(Some(Object::Number(1.0)))));
        let mut env = Enviorment::default();
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(1.0)));
    }

    #[test]
    fn test_eval_variable() {
        let expr = Expression::Variable("a".to_string());
        let mut env = Enviorment::default();
        env.define("a".to_string(), Some(Object::Number(1.0)));
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(1.0)));
    }

    #[test]
    fn test_eval_assignment() {
        let expr = Expression::Assignment(
            "a".to_string(),
            Box::new(Expression::Literal(Some(Object::Number(1.0)))),
        );
        let mut env = Enviorment::default();
        env.define("a".to_string(), Some(Object::Number(10.0)));
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(1.0)));
        assert_eq!(env.get("a".to_string()).unwrap(), Some(Object::Number(1.0)));
    }

    #[test]
    fn test_eval_unary() {
        let expr = Expression::Unary(
            UnaryOp::Minus,
            Box::new(Expression::Literal(Some(Object::Number(1.0)))),
        );
        let mut env = Enviorment::default();
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(-1.0)));
    }

    #[test]
    fn test_eval_binary() {
        let expr = Expression::Binary(
            Box::new(Expression::Literal(Some(Object::Number(1.0)))),
            BinaryOp::Plus,
            Box::new(Expression::Literal(Some(Object::Number(2.0)))),
        );
        let mut env = Enviorment::default();
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Number(3.0)));
    }

    #[test]
    fn test_eval_logical() {
        let expr = Expression::Logical(
            Box::new(Expression::Literal(Some(Object::Boolean(true)))),
            LogicalOp::And,
            Box::new(Expression::Literal(Some(Object::Boolean(false)))),
        );
        let mut env = Enviorment::default();
        assert_eq!(expr.eval(&mut env).unwrap(), Some(Object::Boolean(false)));
    }
}
