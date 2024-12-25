mod lexer;
mod parser;
mod tokens;

pub fn interpret<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    let ast = parser::parse(tokens);
    println!("{}", ast);
}
