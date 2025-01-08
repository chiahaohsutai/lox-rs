use lox_rs::interpret;

fn main() {
    let code = "print 1+ -2 ;".to_string();
    interpret(code);
}
