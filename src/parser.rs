use crate::tokens::Token;

enum Expr {
    Binary(BinaryExpr),
    UnaryExpr(UnaryExpr),
    Literal(LiteralExpr),
}

struct BinaryExpr {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

struct UnaryExpr {
    operator: Token,
    right: Box<Expr>,
}

struct LiteralExpr {
    value: Token,
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