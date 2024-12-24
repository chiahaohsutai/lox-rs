use lox_rs::parse;

fn main() {
    let code = "var a = 1;".to_string();
    parse(code);
}
