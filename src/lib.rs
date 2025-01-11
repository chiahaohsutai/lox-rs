mod interpreter;
mod lexer;
mod parser;
mod tokens;

use crate::interpreter::Evaluate;

pub fn interpret<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    let declarations = parser::parse(tokens);
    match declarations {
        Ok(declarations) => {
            for decl in declarations {
                let result = decl.evaluate();
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
