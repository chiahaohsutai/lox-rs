use crate::tokenizer::{Lexeme, Token};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Bang,
}

#[derive(PartialEq, Debug, Clone)]
pub enum LogicalOp {
    And,
    Or,
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

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Option<Literal>),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Grouping(Box<Expression>),
    Variable(String),
    Assignment(String, Box<Expression>),
    Logical(Box<Expression>, LogicalOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Var(String, Option<Expression>),
    Block(Vec<Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
}

pub fn parse(tokens: Vec<Lexeme>) -> (Vec<Statement>, Vec<String>) {
    let mut tokens: Vec<_> = tokens.into_iter().rev().collect();
    let mut statements = Vec::new();
    let mut errors = Vec::new();

    while tokens.len() > 0 {
        if let Token::Eof = tokens.last().unwrap().token() {
            break;
        };
        let stmt = parse_stmt(&mut tokens);
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

fn parse_stmt(tokens: &mut Vec<Lexeme>) -> Result<Statement, String> {
    let lexeme = tokens.pop();
    match lexeme {
        Some(lexeme) => {
            let line = lexeme.line();
            match lexeme.token() {
                Token::Var => parse_var_stmt(tokens, line),
                Token::If => parse_if_stmt(tokens, line),
                Token::Print => parse_print_stmt(tokens, line),
                Token::While => parse_while_stmt(tokens, line),
                Token::For => parse_for_stmt(tokens, line),
                Token::LeftBrace => parse_block_stmt(tokens, line),
                _ => {
                    tokens.push(lexeme);
                    parse_expr_stmt(tokens, line)
                }
            }
        }
        None => Err("Unexpected end of file".to_string()),
    }
}

fn parse_for_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    if tokens.last().map(|t| t.token()) == Some(&Token::LeftParen) {
        tokens.pop();
        let initializer = match tokens.pop() {
            Some(lexeme) if *lexeme.token() == Token::Semicolon => None,
            Some(lexeme) if *lexeme.token() == Token::Var => Some(parse_var_stmt(tokens, line)?),
            Some(_) => Some(parse_expr_stmt(tokens, line)?),
            None => return Err("Unexpected end of file".to_string()),
        };
        let mut condition = match tokens.last() {
            Some(lexeme) if *lexeme.token() == Token::Semicolon => None,
            Some(_) => Some(parse_expression(tokens)?),
            None => return Err("Unexpected end of file".to_string()),
        };
        if tokens.last().map(|t| t.token()) == Some(&Token::Semicolon) {
            tokens.pop();
            let increment = match tokens.last() {
                Some(lexeme) if *lexeme.token() == Token::RightParen => None,
                Some(_) => Some(Box::new(parse_expression(tokens)?)),
                None => return Err("Unexpected end of file".to_string()),
            };
            if tokens.last().map(|t| t.token()) == Some(&Token::RightParen) {
                tokens.pop();
                let mut body = parse_stmt(tokens)?;
                if increment.is_some() {
                    body = Statement::Block(vec![
                        Box::new(body),
                        Box::new(Statement::Expression(*increment.unwrap())),
                    ]);
                }
                if condition.is_none() {
                    condition = Some(Expression::Literal(Some(Literal::Bool(true))));
                }
                body = Statement::While(condition.unwrap(), Box::new(body));
                if initializer.is_some() {
                    return Ok(Statement::Block(vec![
                        Box::new(initializer.unwrap()),
                        Box::new(body),
                    ]));
                }
                Ok(body)
            } else {
                Err(format!("Expected ')' in line {}", line))
            }
        } else {
            Err(format!("Expected ';' in line {}", line))
        }
    } else {
        Err(format!("Expected '(' in line {}", line))
    }
}

fn parse_while_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    if tokens.last().map(|t| t.token()) == Some(&Token::LeftParen) {
        tokens.pop();
        let condition = parse_expression(tokens)?;
        if tokens.last().map(|t| t.token()) == Some(&Token::RightParen) {
            tokens.pop();
            let body = Box::new(parse_stmt(tokens)?);
            if tokens.last().map(|t| t.token()) == Some(&Token::Semicolon) {
                tokens.pop();
                Ok(Statement::While(condition, body))
            } else {
                Err(format!("Expected ';' in line {}", line))
            }
        } else {
            Err(format!("Expected ')' in line {}", line))
        }
    } else {
        Err(format!("Expected '(' in line {}", line))
    }
}

fn parse_if_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    if tokens.last().map(|t| t.token().clone()) == Some(Token::LeftParen) {
        tokens.pop();
        let condition = parse_expression(tokens)?;
        if tokens.last().map(|t| t.token().clone()) == Some(Token::RightParen) {
            tokens.pop();
            let then_branch = Box::new(parse_stmt(tokens)?);
            let else_branch = match tokens.last() {
                Some(lexeme) if *lexeme.token() == Token::Else => {
                    tokens.pop();
                    Some(Box::new(parse_stmt(tokens)?))
                }
                _ => None,
            };
            if tokens.last().map(|t| t.token().clone()) == Some(Token::Semicolon) {
                tokens.pop();
                Ok(Statement::If(condition, then_branch, else_branch))
            } else {
                Err(format!("Expected ';' in line {}", line))
            }
        } else {
            return Err(format!("Expected ')' in line {}", line));
        }
    } else {
        Err(format!("Expected '(' in line {}", line))
    }
}

fn parse_block_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    let mut statements = Vec::new();
    while let Some(lexeme) = tokens.last() {
        if *lexeme.token() == Token::RightBrace {
            tokens.pop();
            return Ok(Statement::Block(statements));
        }
        match parse_stmt(tokens) {
            Ok(stmt) => statements.push(Box::new(stmt)),
            Err(err) => return Err(err),
        }
    }
    Err(format!("Expected '}}' in line {}", line))
}

fn parse_expr_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    let expression = parse_expression(tokens)?;
    match tokens.last() {
        Some(lexeme) if *lexeme.token() == Token::Semicolon => {
            tokens.pop();
            Ok(Statement::Expression(expression))
        }
        _ => Err(format!("Expected semicolon in line {}", line)),
    }
}

fn parse_var_stmt(tokens: &mut Vec<Lexeme>, line: usize) -> Result<Statement, String> {
    if let Some(lexeme) = tokens.pop() {
        match lexeme.token() {
            Token::Identifier(name) => match tokens.last() {
                Some(lexeme) if *lexeme.token() == Token::Equal => {
                    tokens.pop();
                    let expr = parse_expression(tokens)?;
                    match tokens.last() {
                        Some(lexeme) if *lexeme.token() == Token::Semicolon => {
                            tokens.pop();
                            Ok(Statement::Var(name.clone(), Some(expr)))
                        }
                        _ => Err(format!("Expected ';' in line {}", line)),
                    }
                }
                Some(lexeme) if *lexeme.token() == Token::Semicolon => {
                    tokens.pop();
                    Ok(Statement::Var(name.clone(), None))
                }
                _ => {
                    tokens.push(lexeme);
                    Err(format!("Expected '=' or ';' in line {}", line))
                }
            },
            _ => Err(format!("Expected identifier in line {}", line)),
        }
    } else {
        Err("Unexpected end of file".to_string())
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
    parse_assignment(tokens)
}

fn parse_assignment(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let lhs = parse_logical_or(tokens)?;
    if tokens.last().map(|t| t.token().clone()) == Some(Token::Equal) {
        tokens.pop();
        let rhs = Box::new(parse_assignment(tokens)?);
        match lhs {
            Expression::Variable(name) => Ok(Expression::Assignment(name, rhs)),
            _ => Err("Invalid assignment target".to_string()),
        }
    } else {
        Ok(lhs)
    }
}

fn parse_logical_or(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_logical_and(tokens)?;
    while tokens.last().map(|t| t.token().clone()) == Some(Token::Or) {
        tokens.pop();
        let right = Box::new(parse_logical_and(tokens)?);
        expression = Expression::Logical(Box::new(expression), LogicalOp::Or, right);
    }
    Ok(expression)
}

fn parse_logical_and(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_equality(tokens)?;
    while tokens.last().map(|t| t.token().clone()) == Some(Token::And) {
        tokens.pop();
        let right = Box::new(parse_equality(tokens)?);
        expression = Expression::Logical(Box::new(expression), LogicalOp::And, right);
    }
    Ok(expression)
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
            _ => return parse_call(tokens),
        };
        tokens.pop();
        let right = Box::new(parse_unary(tokens)?);
        Ok(Expression::Unary(op, right))
    } else {
        parse_call(tokens)
    }
}

fn parse_call(tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut expression = parse_primary(tokens)?;
    loop {
        if tokens.last().map(|t| t.token().clone()) == Some(Token::LeftParen) {
            tokens.pop();
            expression = parse_arguments(expression, tokens)?;
        } else {
            break;
        }
    };
    Ok(expression)
}

fn parse_arguments(callee: Expression, tokens: &mut Vec<Lexeme>) -> Result<Expression, String> {
    let mut arguments = Vec::new();
    if tokens.last().map(|t| t.token().clone()) != Some(Token::RightParen) {
        tokens.pop();
        loop {
            arguments.push(parse_expression(tokens)?);
            if tokens.last().map(|t| t.token().clone()) == Some(Token::Comma) {
                tokens.pop();
            } else {
                break;
            }
        };
        if tokens.last().map(|t| t.token().clone()) != Some(Token::RightParen) {
            return Err("Expected ')'".to_string());
        } else {
            tokens.pop();
            Ok(Expression::Call(Box::new(callee), arguments))
        }
    } else {
        tokens.pop();
        Ok(Expression::Call(Box::new(callee), arguments))
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
            Token::Identifier(s) => Ok(Expression::Variable(s.clone())),
            Token::LeftParen => {
                let expression = parse_expression(tokens)?;
                match tokens.pop() {
                    Some(lexeme) if *lexeme.token() == Token::RightParen => {
                        Ok(Expression::Grouping(Box::new(expression)))
                    }
                    _ => Err(format!("Expected ')' in line {}", lexeme.line())),
                }
            }
            _ => Err(format!(
                "Unexpected token {} in line {}",
                lexeme.token(),
                lexeme.line(),
            )),
        }
    } else {
        Err("Unexpected end of file".to_string())
    }
}
