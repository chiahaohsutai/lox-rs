mod lexer;
mod parser;
mod tokens;
mod interpreter;

use crate::interpreter::Evaluate;

pub fn interpret<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    let statements = parser::parse(tokens).unwrap();
    for stmt in statements {
        stmt.evaluate().unwrap();
    }
}
