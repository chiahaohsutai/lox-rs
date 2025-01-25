mod evaluate;
mod parser;
mod tokenizer;

use crate::evaluate::evaluate;
use std::collections::HashMap;
use std::vec;

pub fn interpret<T: AsRef<str>>(program: T) {
    let tokens = tokenizer::tokenize(program.as_ref());
    let has_errors = tokens.iter().any(|t| t.is_err());
    if has_errors {
        let errors: Vec<_> = tokens.into_iter().filter_map(|t| t.err()).collect();
        for error in errors {
            eprintln!("{}", error);
            std::process::exit(65)
        }
    } else {
        let tokens: Vec<_> = tokens.into_iter().map(|t| t.unwrap()).collect();
        let (statements, errors) = parser::parse(tokens);
        if !errors.is_empty() {
            for error in errors {
                eprintln!("{}", error);
            }
            std::process::exit(65)
        }
        let mut enviormnent = vec![HashMap::new()];
        for statement in statements {
            let result = evaluate(statement, &mut enviormnent);
            if let Result::Err(e) = result {
                eprintln!("{}", e);
                std::process::exit(70)
            }
        }
    };
}
