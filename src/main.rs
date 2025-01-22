use lox_rs::interpret;

fn main() {
    let lines = vec![
        "var a = 1;",
        "print a = 2;",
    ];
    let program = lines.join("\n");
    interpret(program);
}
