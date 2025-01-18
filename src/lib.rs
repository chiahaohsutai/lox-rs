mod tokenizer;

fn interpret<T: AsRef<str>>(program: T) -> Result<(), String> {
    let tokens = tokenizer::tokenize(program.as_ref());
    println!("{:?}", tokens);
    Ok(())
}