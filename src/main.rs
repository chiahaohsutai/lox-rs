use lox_rs::interpret;

fn main() {
    let code = "1 + (1 + 2 - 13 * 2) - 0".to_string();
    interpret(code);
}
