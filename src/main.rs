use lox_rs::interpret;

fn main() {
    let lines = vec![
        "var a = 1;",
        "a = 2;",
        "var b = 2;",
        "var c = b;",
        "print a + b + c;",
    ];
    let program = lines.join("\n");
    interpret(program);
}
