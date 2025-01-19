use crate::parser::{Expression, Literal, UnaryOp, BinaryOp};

pub trait Evaluate {
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