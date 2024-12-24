mod lexer;
mod parser;
mod tokens;

pub fn parse<T: AsRef<str>>(text: T) {
    let tokens = lexer::scan(text.as_ref().to_string());
    print!("{:?}", tokens);
}
