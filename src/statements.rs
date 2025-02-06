use crate::{expressions::Expression, tokenizer::Token, Create, Enviorment, Eval};

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Var(String, Option<Expression>),
    Block(Vec<Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Function(String, Vec<String>, Vec<Box<Statement>>),
}

impl Eval for Statement {
    type Output = ();
    fn eval(&self, env: &mut Enviorment) -> Result<Self::Output, String> {
        todo!();
    }
}

impl Create for Statement {
    type Output = Statement;
    fn create(tokens: &mut Vec<Token>) -> Result<Self::Output, String> {
        let mut tkns = tokens.iter().cloned().rev().collect::<Vec<Token>>();
        let stmt = parse_statement(&mut tkns)?;
        *tokens = tkns.iter().cloned().rev().collect::<Vec<Token>>();
        Ok(stmt)
    }
}

fn parse_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    match tokens.last() {
        Some(Token::Print(_)) => parse_print_statement(tokens),
        Some(Token::Var(_)) => parse_var_statement(tokens),
        Some(Token::LeftBrace(_)) => parse_block_statement(tokens),
        Some(Token::If(_)) => parse_if_statement(tokens),
        Some(Token::While(_)) => parse_while_statement(tokens),
        // Some(Token::Fun(_)) => parse_function_statement(tokens),
        _ => parse_expression_statement(tokens),
    }
}

fn parse_print_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    tokens.pop();
    let expr = Expression::create(tokens)?;
    match tokens.pop() {
        Some(Token::Semicolon(_)) => Ok(Statement::Print(expr)),
        _ => Err("Expected ';' after expression".to_string()),
    }
}

fn parse_var_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    tokens.pop();
    let name = match tokens.pop() {
        Some(Token::Identifier(name, _)) => name,
        _ => return Err("Expected identifier after 'var'".to_string()),
    };
    let expr = match tokens.last() {
        Some(Token::Equal(_)) => {
            tokens.pop();
            Some(Expression::create(tokens)?)
        }
        _ => None,
    };
    match tokens.pop() {
        Some(Token::Semicolon(_)) => Ok(Statement::Var(name, expr)),
        _ => Err("Expected ';' after variable declaration".to_string()),
    }
}

fn parse_block_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    tokens.pop();
    let mut statements = Vec::new();
    while let Some(token) = tokens.last() {
        if token == &Token::RightBrace(0) {
            tokens.pop();
            return Ok(Statement::Block(statements));
        }
        statements.push(Box::new(parse_statement(tokens)?));
    }
    Err("Expected '}' at the end of block".to_string())
}

fn parse_if_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    tokens.pop();
    let condition = Expression::create(tokens)?;
    if let Some(Token::RightParen(_)) = tokens.last() {
        tokens.pop();
    } else {
        return Err("Expected ')' after if condition".to_string());
    }
    let then_branch = Box::new(parse_statement(tokens)?);
    let else_branch = match tokens.last() {
        Some(Token::Else(_)) => {
            tokens.pop();
            Some(Box::new(parse_statement(tokens)?))
        }
        _ => None,
    };
    Ok(Statement::If(condition, then_branch, else_branch))
}

fn parse_while_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    tokens.pop();
    if let Some(Token::LeftParen(_)) = tokens.last() {
        tokens.pop();
    } else {
        return Err("Expected '(' after while".to_string());
    }
    let condition = Expression::create(tokens)?;
    if let Some(Token::RightParen(_)) = tokens.last() {
        tokens.pop();
    } else {
        return Err("Expected ')' after while condition".to_string());
    }
    let body = Box::new(parse_statement(tokens)?);
    Ok(Statement::While(condition, body))
}

fn parse_expression_statement(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    let expr = Expression::create(tokens)?;
    match tokens.pop() {
        Some(Token::Semicolon(_)) => Ok(Statement::Expression(expr)),
        _ => Err("Expected ';' after expression".to_string()),
    }
}
