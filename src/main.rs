use lox_rs::interpret;

fn main() {
    let program = r#"print "Hello World!";"#;
    interpret(program);
}
