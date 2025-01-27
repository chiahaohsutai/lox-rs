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
        "for (var i = 0; i < 10; i = i + 1) {",
        "  print i;",
        "}",
    ];
    let program = lines.join("\n");
    interpret(program);
}
