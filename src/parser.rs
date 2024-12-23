use crate::tokens::Token;

enum Expression {
    EXPRESSION,
    EQUALITY,
    COMPARISON,
    TERM,
    UNARY,
    PRIMARY(Box<Expression>),
    NUMBER(f64),
    STRING(String),
    BOOLEAN(bool),
    NIL(())
}

fn parse(text: String)  {
    
}

fn expression(tokens: Vec<Token>) {
    equality(tokens);
}

fn equality(tokens: Vec<Token>) {

}

fn comparison(tokens: Vec<Token>) {

}

fn term(tokens: Vec<Token>) {

}

fn factor(tokens: Vec<Token>) {

}

fn unary(tokens: Vec<Token>) {

}

fn primary(tokens: Vec<Token>) {

}