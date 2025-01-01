mod lexer;
mod parser;
mod tokens;
mod interpreter;

use crate::interpreter::Evaluate;

pub fn interpret<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    let ast = parser::parse(tokens).unwrap();
    let result = ast.evaluate();
    println!("{}", result.unwrap().unwrap());
}
