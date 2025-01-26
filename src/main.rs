use lox_rs::interpret;

fn main() {
    let lines = vec![
        "var a = 2.7;",
        "var b = 0 and 3;",
        "if (a > b) {",
        "  print a;",
        "} else {",
        "  print b;",
        "};",
    ];
    let program = lines.join("\n");
    println!("===start\n\n{}\n\n===end\n", program);
    interpret(program);
}
