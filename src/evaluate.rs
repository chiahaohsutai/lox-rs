use crate::parser::{BinaryOp, Expression, Literal, LogicalOp, Statement, UnaryOp};
use std::collections::HashMap;

trait Evaluate {
    type Output;
    fn evaluate(self, environment: &mut Vec<HashMap<String, Option<Literal>>>) -> Self::Output;
}

impl Evaluate for Expression {
    type Output = Result<Option<Literal>, String>;
    fn evaluate(self, environment: &mut Vec<HashMap<String, Option<Literal>>>) -> Self::Output {
        match self {
            Self::Literal(literal) => Ok(literal),
            Self::Grouping(expression) => expression.evaluate(environment),
            Self::Unary(op, right) => eval_unary_expr(op, *right, environment),
            Self::Binary(left, op, right) => eval_binary_expr(*left, op, *right, environment),
            Self::Variable(name) => eval_var_expr(name, environment),
            Self::Assignment(name, expression) => eval_assign_expr(name, *expression, environment),
            Self::Logical(lhs, op, rhs) => eval_logical_expr(lhs, op, rhs, environment),
            Self::Call(callee, args) => eval_call_expr(callee, args, environment),
        }
    }
}

fn eval_call_expr(
    callee: Box<Expression>,
    args: Vec<Expression>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    let callee = callee.evaluate(environment)?;
    let mut arguments = Vec::new();
    for arg in args {
        arguments.push(arg.evaluate(environment)?);
    }
    todo!();
}

fn eval_logical_expr(
    lhs: Box<Expression>,
    op: LogicalOp,
    rhs: Box<Expression>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    let lhs = lhs.evaluate(environment)?;
    if op == LogicalOp::Or {
        if let Some(Literal::Bool(true)) = lhs {
            return Ok(lhs);
        }
    } else {
        if let Some(Literal::Bool(false)) = lhs {
            return Ok(lhs);
        }
    }
    rhs.evaluate(environment)
}

fn eval_var_expr(
    name: String,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    for env in environment.iter().rev() {
        if let Some(value) = env.get(&name) {
            return Ok(value.clone());
        }
    }
    Err(format!("Undefined variable '{}'", name))
}

fn eval_assign_expr(
    name: String,
    expression: Expression,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    let value = expression.evaluate(environment)?;
    for values in environment.iter_mut().rev() {
        if values.contains_key(&name) {
            values.insert(name, value.clone());
            return Ok(value);
        }
    }
    Err(format!("Undefined variable '{}' during assignment", name))
}

fn eval_unary_expr(
    op: UnaryOp,
    right: Expression,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    let right = right.evaluate(environment)?;
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
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<Option<Literal>, String> {
    let left = left.evaluate(environment)?;
    let right = right.evaluate(environment)?;
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

pub fn evaluate(
    stmt: Statement,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    match stmt {
        Statement::Var(name, expression) => eval_var_stmt(name, expression, environment),
        Statement::Function(name, args, body) => todo!(),
        Statement::Expression(expression) => eval_expr_stmt(expression, environment),
        Statement::Print(expression) => eval_print_stmt(expression, environment),
        Statement::Block(stmts) => eval_block_stmt(stmts, environment),
        Statement::While(condition, body) => eval_while_stmt(condition, body, environment),
        Statement::If(condition, then_branch, else_branch) => {
            eval_if_stmt(condition, then_branch, else_branch, environment)
        }
    }
}

fn eval_while_stmt(
    condition: Expression,
    body: Box<Statement>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    while let Some(Literal::Bool(true)) = condition.clone().evaluate(environment)? {
        evaluate(*body.clone(), environment)?;
    }
    Ok(())
}

fn eval_if_stmt(
    condition: Expression,
    then_branch: Box<Statement>,
    else_branch: Option<Box<Statement>>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    let condition = condition.evaluate(environment)?;
    match condition {
        Some(Literal::Bool(true)) => evaluate(*then_branch, environment),
        Some(Literal::Bool(false)) => {
            if let Some(else_branch) = else_branch {
                evaluate(*else_branch, environment)
            } else {
                Ok(())
            }
        }
        _ => Err("Invalid if condition".to_string()),
    }
}

fn eval_block_stmt(
    stmts: Vec<Box<Statement>>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    environment.push(HashMap::new());
    for stmt in stmts {
        let result = evaluate(*stmt, environment);
        if let Result::Err(e) = result {
            environment.pop();
            return Result::Err(e);
        }
    }
    environment.pop();
    Ok(())
}

fn eval_var_stmt(
    name: String,
    expression: Option<Expression>,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    let value = match expression {
        Some(expression) => expression.evaluate(environment)?,
        None => None,
    };
    // environment.define(name, value);
    if let Some(values) = environment.last_mut() {
        values.insert(name, value);
    }
    Ok(())
}

fn eval_expr_stmt(
    expression: Expression,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    let _ = expression.evaluate(environment)?;
    Ok(())
}

fn eval_print_stmt(
    expression: Expression,
    environment: &mut Vec<HashMap<String, Option<Literal>>>,
) -> Result<(), String> {
    let result = expression.evaluate(environment)?;
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
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Plus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_eval_subtraction() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(1.0)))),
            BinaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(-1.0)))
        );
    }

    #[test]
    fn test_eval_multiplication() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Star,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(6.0)))
        );
    }

    #[test]
    fn test_eval_division() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(6.0)))),
            BinaryOp::Slash,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_eval_greater() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::Greater,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_greater_equal() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::GreaterEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
            BinaryOp::Less,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_less_equal() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::LessEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_equal_equal() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::EqualEqual,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_bang_equal() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
            BinaryOp::BangEqual,
            Box::new(Expression::Literal(Some(Literal::Number(2.0)))),
        );
        assert_eq!(expression.evaluate(&mut env), Ok(Some(Literal::Bool(true))));
    }

    #[test]
    fn test_eval_unary_minus() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Unary(
            UnaryOp::Minus,
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(-3.0)))
        );
    }

    #[test]
    fn test_eval_unary_bang() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Unary(
            UnaryOp::Bang,
            Box::new(Expression::Literal(Some(Literal::Bool(true)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Bool(false)))
        );
    }

    #[test]
    fn test_eval_grouping() {
        let mut env = vec![HashMap::new()];
        let expression =
            Expression::Grouping(Box::new(Expression::Literal(Some(Literal::Number(3.0)))));
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_eval_var() {
        let mut env = vec![HashMap::new()];
        let expression = Expression::Variable("a".to_string());
        env[0].insert("a".to_string(), Some(Literal::Number(3.0)));
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_assign_expr() {
        let mut env = vec![HashMap::new()];
        env[0].insert(String::from("a"), Some(Literal::Number(2.0)));
        assert_eq!(
            env[0].get(&String::from("a")),
            Some(&Some(Literal::Number(2.0)))
        );
        let expression = Expression::Assignment(
            "a".to_string(),
            Box::new(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        assert_eq!(
            expression.evaluate(&mut env),
            Ok(Some(Literal::Number(3.0)))
        );
        assert_eq!(
            env[0].get(&String::from("a")),
            Some(&Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_eval_var_stmt() {
        let mut env = vec![HashMap::new()];
        let stmt = Statement::Var(
            "a".to_string(),
            Some(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        let _ = evaluate(stmt, &mut env);
        assert_eq!(
            env[0].get(&String::from("a")),
            Some(&Some(Literal::Number(3.0)))
        );
    }

    #[test]
    fn test_block_stmt_scope() {
        let mut env = vec![HashMap::new()];
        let stmt = Statement::Block(vec![
            Box::new(Statement::Var(
                "a".to_string(),
                Some(Expression::Literal(Some(Literal::Number(3.0)))),
            )),
            Box::new(Statement::Var(
                "b".to_string(),
                Some(Expression::Literal(Some(Literal::Number(2.0)))),
            )),
        ]);
        let _ = evaluate(stmt, &mut env);
        assert_eq!(env[0].get(&String::from("a")), None);
        assert_eq!(env[0].get(&String::from("b")), None);
    }

    #[test]
    fn text_block_stmt_with_assignments() {
        let mut env = vec![HashMap::new()];
        let var = Statement::Var(
            "a".to_string(),
            Some(Expression::Literal(Some(Literal::Number(3.0)))),
        );
        let stmt = Statement::Block(vec![Box::new(Statement::Expression(
            Expression::Assignment(
                "a".to_string(),
                Box::new(Expression::Literal(Some(Literal::Number(4.0)))),
            ),
        ))]);
        let _ = evaluate(var, &mut env);
        assert_eq!(
            env[0].get(&String::from("a")),
            Some(&Some(Literal::Number(3.0)))
        );
        let _ = evaluate(stmt, &mut env);
        assert_eq!(
            env[0].get(&String::from("a")),
            Some(&Some(Literal::Number(4.0)))
        );
    }
}
