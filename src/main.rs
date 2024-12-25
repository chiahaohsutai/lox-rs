use lox_rs::interpret;

fn main() {
    let code = "1 + (1 + 2 - (1))".to_string();
    interpret(code);
}
