use lox_rs::interpret;

fn main() {
    let code = "var a = 1;".to_string();
    interpret(code);
}
