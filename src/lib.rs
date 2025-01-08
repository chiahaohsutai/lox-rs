mod interpreter;
mod lexer;
mod parser;
mod tokens;

use crate::interpreter::Evaluate;

pub fn interpret<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    let statements = parser::parse(tokens);
    match statements {
        Ok(statements) => {
            for stmt in statements {
                let result = stmt.evaluate();
                match result {
                    Ok(Some(literal)) => println!("{}", literal),
                    Ok(None) => (),
                    Err(e) => println!("{}", e),
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
