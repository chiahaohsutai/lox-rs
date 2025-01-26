use lox_rs::interpret;

fn main() {
    let lines = vec![
        "var a = 4;",
        "var b = 0 and 3;",
        "if (a > b) {",
        "  print a;",
        "} else {",
        "  print b;",
        "};",
        "while (a > 0) {",
        "  print a;",
        "  a = a - 1;",
        "};",
    ];
    let program = lines.join("\n");
    println!("===start\n\n{}\n\n===end\n", program);
    interpret(program);
}
