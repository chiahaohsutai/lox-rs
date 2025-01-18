mod tokenizer;
mod parser;

pub fn interpret<T: AsRef<str>>(program: T) {
    let tokens = tokenizer::tokenize(program.as_ref());
    let has_errors = tokens.iter().any(|t| t.is_err());
    if has_errors {
        let errors: Vec<_> = tokens.into_iter().filter_map(|t| t.err()).collect();
        for error in errors {
            eprintln!("{}", error);
            std::process::exit(65)
        };
    } else {
        let tokens: Vec<_> = tokens.into_iter().map(|t| t.unwrap()).collect();
        let (statements, errors) = parser::parse(tokens);
        if !errors.is_empty() {
            for error in errors {
                eprintln!("{}", error);
            }
            std::process::exit(70)
        }
    };
}