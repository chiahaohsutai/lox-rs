use lox_rs::interpret;

fn main() {
    let stmts = vec!["print true;", "print 2 + 1;", "print\"one\";"];
    let program = stmts.join("\n");
    interpret(program);
}
