use lox_rs::interpret;

fn main() {
    let lines = vec![
        "var a = 1;",
        "var b = -1;",
        "if (a > b) {",
        "  print a;",
        "} else {",
        "  print b;",
        "};",
    ];
    let program = lines.join("\n");
    println!("{}", program);
    interpret(program);
}
