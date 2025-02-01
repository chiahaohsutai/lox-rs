#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String, usize),
    String(String, usize),
    Number(f64, usize),
    LeftParen(usize),
    RightParen(usize),
    LeftBrace(usize),
    RightBrace(usize),
    Comma(usize),
    Dot(usize),
    Minus(usize),
    Plus(usize),
    Semicolon(usize),
    Slash(usize),
    Star(usize),
    Bang(usize),
    BangEqual(usize),
    Equal(usize),
    EqualEqual(usize),
    Greater(usize),
    GreaterEqual(usize),
    Less(usize),
    LessEqual(usize),
    And(usize),
    Class(usize),
    Else(usize),
    False(usize),
    Fun(usize),
    For(usize),
    If(usize),
    Nil(usize),
    Or(usize),
    Print(usize),
    Return(usize),
    Super(usize),
    This(usize),
    True(usize),
    Var(usize),
    While(usize),
    Eof(usize),
}

pub fn tokenize<T: AsRef<str>>(program: T) -> Vec<Result<Token, String>> {
    let mut program: Vec<char> = program.as_ref().chars().rev().collect();
    let mut tokens: Vec<Result<Token, String>> = vec![];
    let mut line: usize = 0;

    while program.len() > 0 {
        let curr = program.pop().unwrap();
        match curr {
            '\n' => line += 1,
            '(' => tokens.push(Ok(Token::LeftParen(line))),
            ')' => tokens.push(Ok(Token::RightParen(line))),
            '{' => tokens.push(Ok(Token::LeftBrace(line))),
            '}' => tokens.push(Ok(Token::RightBrace(line))),
            ',' => tokens.push(Ok(Token::Comma(line))),
            '.' => tokens.push(Ok(Token::Dot(line))),
            '-' => tokens.push(Ok(Token::Minus(line))),
            '+' => tokens.push(Ok(Token::Plus(line))),
            ';' => tokens.push(Ok(Token::Semicolon(line))),
            '*' => tokens.push(Ok(Token::Star(line))),
            '!' | '=' | '<' | '>' => {
                let next = program.pop().unwrap();
                match (curr, next) {
                    ('!', '=') => tokens.push(Ok(Token::BangEqual(line))),
                    ('=', '=') => tokens.push(Ok(Token::EqualEqual(line))),
                    ('<', '=') => tokens.push(Ok(Token::LessEqual(line))),
                    ('>', '=') => tokens.push(Ok(Token::GreaterEqual(line))),
                    ('!', _) => {
                        program.push(next);
                        tokens.push(Ok(Token::Bang(line)));
                    }
                    ('=', _) => {
                        program.push(next);
                        tokens.push(Ok(Token::Equal(line)));
                    }
                    ('<', _) => {
                        program.push(next);
                        tokens.push(Ok(Token::Less(line)));
                    }
                    ('>', _) => {
                        program.push(next);
                        tokens.push(Ok(Token::Greater(line)));
                    }
                    _ => tokens.push(Err(format!(
                        "Unexpected character '{}' at line {}",
                        next, line
                    ))),
                }
            }
            '"' => {
                let mut string = String::new();
                while program.len() > 0 && program[program.len() - 1] != '"' {
                    string.push(program.pop().unwrap());
                }
                if program.len() == 0 {
                    tokens.push(Err(format!("Unterminated string at line {}", line)));
                } else {
                    program.pop();
                    tokens.push(Ok(Token::String(string, line)));
                }
            }
            '0'..='9' => {
                let mut number = String::new();
                number.push(curr);
                while program.len() > 0 && program[program.len() - 1].is_digit(10) {
                    number.push(program.pop().unwrap());
                }
                if program.len() > 0 && program[program.len() - 1] == '.' {
                    number.push(program.pop().unwrap());
                    while program.len() > 0 && program[program.len() - 1].is_digit(10) {
                        number.push(program.pop().unwrap());
                    }
                }
                tokens.push(Ok(Token::Number(number.parse().unwrap(), line)));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut identifier = String::new();
                identifier.push(curr);
                while program.len() > 0 && program[program.len() - 1].is_alphanumeric() {
                    identifier.push(program.pop().unwrap());
                }
                match identifier.as_str() {
                    "and" => tokens.push(Ok(Token::And(line))),
                    "class" => tokens.push(Ok(Token::Class(line))),
                    "else" => tokens.push(Ok(Token::Else(line))),
                    "false" => tokens.push(Ok(Token::False(line))),
                    "for" => tokens.push(Ok(Token::For(line))),
                    "fun" => tokens.push(Ok(Token::Fun(line))),
                    "if" => tokens.push(Ok(Token::If(line))),
                    "nil" => tokens.push(Ok(Token::Nil(line))),
                    "or" => tokens.push(Ok(Token::Or(line))),
                    "print" => tokens.push(Ok(Token::Print(line))),
                    "return" => tokens.push(Ok(Token::Return(line))),
                    "super" => tokens.push(Ok(Token::Super(line))),
                    "this" => tokens.push(Ok(Token::This(line))),
                    "true" => tokens.push(Ok(Token::True(line))),
                    "var" => tokens.push(Ok(Token::Var(line))),
                    "while" => tokens.push(Ok(Token::While(line))),
                    _ => tokens.push(Ok(Token::Identifier(identifier, line))),
                }
            }
            _ => continue,
        }
    }
    tokens.push(Ok(Token::Eof(line)));
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let program = "var a = 1;";
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Ok(Token::Number(1.0, 0)),
                Ok(Token::Semicolon(0)),
                Ok(Token::Eof(0)),
            ]
        );
    }

    #[test]
    fn test_tokenize_string() {
        let program = r#"var a = "hello";"#;
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Ok(Token::String("hello".to_string(), 0)),
                Ok(Token::Semicolon(0)),
                Ok(Token::Eof(0)),
            ]
        );
    }

    #[test]
    fn test_tokenize_string_unterminated() {
        let program = r#"var a = "hello;"#;
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Err("Unterminated string at line 0".to_string()),
                Ok(Token::Eof(0))
            ]
        );
    }

    #[test]
    fn test_tokenize_number() {
        let program = "var a = 1.5;";
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Ok(Token::Number(1.5, 0)),
                Ok(Token::Semicolon(0)),
                Ok(Token::Eof(0)),
            ]
        );
    }

    #[test]
    fn test_tokenize_number_with_dot() {
        let program = "var a = 1.;";
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Ok(Token::Number(1.0, 0)),
                Ok(Token::Semicolon(0)),
                Ok(Token::Eof(0)),
            ]
        );
    }

    #[test]
    fn test_tokenize_number_with_dot_and_decimal() {
        let program = "var a = 1.5;";
        let tokens = tokenize(program);
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Var(0)),
                Ok(Token::Identifier("a".to_string(), 0)),
                Ok(Token::Equal(0)),
                Ok(Token::Number(1.5, 0)),
                Ok(Token::Semicolon(0)),
                Ok(Token::Eof(0)),
            ]
        );
    }
}
