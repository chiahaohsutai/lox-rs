use std::collections::HashMap;
use crate::parser::{Expression, Literal, UnaryOp, BinaryOp, Statement};

trait Evaluate {
    type Output;
    fn evaluate(self, enviorment: &HashMap<String, Option<Literal>>) -> Self::Output;
}

impl Evaluate for Expression {
    type Output = Result<Option<Literal>, String>;
    fn evaluate(self, enviorment: &HashMap<String, Option<Literal>>) -> Self::Output {
        match self {
            Self::Literal(literal) => Ok(literal),
            Self::Grouping(expression) => expression.evaluate(enviorment),
            Self::Unary(op, right) => eval_unary_expr(op, *right, enviorment),
            Self::Binary(left, op, right) => eval_binary_expr(*left, op, *right, enviorment),
            Self::Variable(name) => eval_var_expr(name, enviorment),
        }
    }
}

fn eval_var_expr(name: String, enviorment: &HashMap<String, Option<Literal>>) -> Result<Option<Literal>, String> {
    match enviorment.get(&name) {
        Some(value) => Ok(value.clone()),
        None => Err("Variable not found".to_string()),
    }
}

fn eval_unary_expr(op: UnaryOp, right: Expression, enviorment: &HashMap<String, Option<Literal>>) -> Result<Option<Literal>, String> {
    let right = right.evaluate(enviorment)?;
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
    enviorment: &HashMap<String, Option<Literal>>
) -> Result<Option<Literal>, String> {
    let left = left.evaluate(enviorment)?;
    let right = right.evaluate(enviorment)?;
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

pub fn evaluate(stmt: Statement, enviorment: &mut HashMap<String, Option<Literal>>) -> Result<(), String> {
    match stmt {
        Statement::Var(name, expression) => eval_var_stmt(name, expression, enviorment),
        Statement::Expression(expression) => eval_expr_stmt(expression, enviorment),
        Statement::Print(expression) => eval_print_stmt(expression, enviorment),
    }
}

fn eval_var_stmt(
    name: String,
    expression: Option<Expression>,
    enviorment: &mut HashMap<String, Option<Literal>>,
) -> Result<(), String> {
    let value = match expression {
        Some(expression) => expression.evaluate(enviorment)?,
        None => None,
    };
    enviorment.insert(name, value);
    Ok(())
}

fn eval_expr_stmt(expression: Expression, enviorment: &HashMap<String, Option<Literal>>) -> Result<(), String> {
    let _ = expression.evaluate(enviorment)?;
    Ok(())
}

fn eval_print_stmt(expression: Expression, enviorment: &HashMap<String, Option<Literal>>) -> Result<(), String> {
    let result = expression.evaluate(enviorment)?;
    match result {
        Some(Literal::Number(n)) => Ok(println!("{}", n)),
        Some(Literal::String(s)) => Ok(println!("{}", s)),
        Some(Literal::Bool(b)) => Ok(println!("{}", b)),
        None => Ok(println!("nil")),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval_addition() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Plus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_subtraction() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(-1.0))));
    }

    #[test]
    fn test_eval_multiplication() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Star,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(6.0))));
    }

    #[test]
    fn test_eval_division() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(6.0)))),
            BinaryOp::Slash,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_greater() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::Greater,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_greater_equal() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::GreaterEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Less,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less_equal() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::LessEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_equal_equal() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::EqualEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_bang_equal() {
        let env = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::BangEqual,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_unary_minus() {
        let env = HashMap::new();
        let expression = Expression::Unary(
            UnaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(-3.0))));
    }

    #[test]
    fn test_eval_unary_bang() {
        let env = HashMap::new();
        let expression = Expression::Unary(
            UnaryOp::Bang,
            Box::new(Expression::Literal(Some(Literal::Bool(true)))),
        );
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Bool(false))));
    }

    #[test]
    fn test_eval_grouping() {
        let env = HashMap::new();
        let expression =
            Expression::Grouping(Box::new(Expression::Literal(Some(Literal::Number(3.0)))));
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_var() {
        let mut env = HashMap::new();
        let expression = Expression::Variable("a".to_string());
        env.insert("a".to_string(), Some(Literal::Number(3.0)));
        assert_eq!(expression.evaluate(&env), Ok(Some(Literal::Number(3.0))));
    }

    #[test]
    fn test_eval_var_stmt() {
        let mut env = HashMap::new();
        let stmt = Statement::Var(
            "a".to_string(),
            Some(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        let _ = evaluate(stmt, &mut env);
        assert_eq!(env.get("a"), Some(&Some(Literal::Number(3.0))));
    }
}