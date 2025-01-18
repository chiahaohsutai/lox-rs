mod tokenizer;

fn print_errors(errors: Vec<String>) {
    for error in errors {
        eprintln!("{}", error);
    }
}

pub fn interpret<T: AsRef<str>>(program: T) -> Result<(), String> {
    let tokens = tokenizer::tokenize(program.as_ref());
    let has_errors = tokens.iter().any(|t| t.is_err());
    if has_errors {
        let errors: Vec<_> = tokens.into_iter().filter_map(|t| t.err()).collect();
        for error in errors {
            eprintln!("{}", error);
            std::process::exit(65)
        };
        
    };
    Ok(())
}