#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF(usize, usize),
    NUMBER(f64, usize, usize),
    STRING(String, usize, usize),
    KEYWORD(Keyword, usize, usize),
    OPERATOR(Operator, usize, usize),
    IDENTIFIER(String, usize, usize),
    PUNCTUATION(Punctuation, usize, usize),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::EOF(line, col) => write!(f, "EOF[line: {}, column: {}]", line, col),
            Self::NUMBER(n, line, col) => {
                write!(f, "NUMBER[value: {}, line: {}, column: {}]", n, line, col)
            }
            Self::STRING(s, line, col) => {
                write!(f, "STRING[value: {}, line: {}, column: {}]", s, line, col)
            }
            Self::KEYWORD(k, line, col) => {
                write!(f, "KEYWORD[value: {}, line: {}, column: {}]", k, line, col)
            }
            Self::IDENTIFIER(i, line, col) => write!(
                f,
                "IDENTIFIER[value: {}, line: {}, column: {}]",
                i, line, col
            ),
            Self::OPERATOR(o, line, col) => {
                write!(f, "OPERATOR[value: {}, line: {}, column: {}]", o, line, col)
            }
            Self::PUNCTUATION(p, line, col) => write!(
                f,
                "PUNCTUATION[value: {}, line: {}, column: {}]",
                p, line, col
            ),
        }
    }
}

impl Token {
    fn new_op(op: Operator, line: usize, col: usize) -> Self {
        Self::OPERATOR(op, line, col)
    }

    fn new_num(num: f64, line: usize, col: usize) -> Self {
        Self::NUMBER(num, line, col)
    }

    fn new_str(str: String, line: usize, col: usize) -> Self {
        Self::STRING(str, line, col)
    }

    fn new_kw(kw: Keyword, line: usize, col: usize) -> Self {
        Self::KEYWORD(kw, line, col)
    }

    fn new_id(id: String, line: usize, col: usize) -> Self {
        Self::IDENTIFIER(id, line, col)
    }

    fn new_punc(punc: Punctuation, line: usize, col: usize) -> Self {
        Self::PUNCTUATION(punc, line, col)
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Keyword {
    IF,
    OR,
    FUN,
    AND,
    FOR,
    NIL,
    VAR,
    ELSE,
    TRUE,
    THIS,
    CLASS,
    PRINT,
    WHILE,
    SUPER,
    FALSE,
    RETURN,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IF => write!(f, "IF"),
            Self::OR => write!(f, "OR"),
            Self::FUN => write!(f, "FUN"),
            Self::AND => write!(f, "AND"),
            Self::FOR => write!(f, "FOR"),
            Self::NIL => write!(f, "NIL"),
            Self::VAR => write!(f, "VAR"),
            Self::ELSE => write!(f, "ELSE"),
            Self::TRUE => write!(f, "TRUE"),
            Self::THIS => write!(f, "THIS"),
            Self::CLASS => write!(f, "CLASS"),
            Self::PRINT => write!(f, "PRINT"),
            Self::WHILE => write!(f, "WHILE"),
            Self::SUPER => write!(f, "SUPER"),
            Self::FALSE => write!(f, "FALSE"),
            Self::RETURN => write!(f, "RETURN"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Punctuation {
    DOT,
    COMMA,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    SEMICOLON,
}

impl std::fmt::Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::COMMA => write!(f, ","),
            Self::DOT => write!(f, "."),
            Self::LBRACE => write!(f, "{{"),
            Self::LPAREN => write!(f, "("),
            Self::RBRACE => write!(f, "}}"),
            Self::RPAREN => write!(f, ")"),
            Self::SEMICOLON => write!(f, ";"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Operator {
    BANG,
    LESS,
    PLUS,
    STAR,
    EQUAL,
    SLASH,
    MINUS,
    LESSEQ,
    BANGEQ,
    GREATER,
    EQUALEQ,
    GREATEREQ,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BANG => write!(f, "!"),
            Self::LESS => write!(f, "<"),
            Self::PLUS => write!(f, "+"),
            Self::STAR => write!(f, "*"),
            Self::EQUAL => write!(f, "="),
            Self::SLASH => write!(f, "/"),
            Self::MINUS => write!(f, "-"),
            Self::LESSEQ => write!(f, "<="),
            Self::BANGEQ => write!(f, "!="),
            Self::GREATER => write!(f, ">"),
            Self::EQUALEQ => write!(f, "=="),
            Self::GREATEREQ => write!(f, ">="),
        }
    }
}

pub fn tokenize<T: AsRef<str>>(program: T) -> Vec<Result<Token, String>> {
    let chars = program.as_ref().chars().collect::<Vec<_>>();
    let mut tokens = vec![];
    let mut line = 0;
    let mut col = 0;
    let mut i = 0;

    while let Some(char) = chars.get(i) {
        match char {
            '(' => tokens.push(Ok(Token::new_punc(Punctuation::LPAREN, line, col))),
            ')' => tokens.push(Ok(Token::new_punc(Punctuation::RPAREN, line, col))),
            '{' => tokens.push(Ok(Token::new_punc(Punctuation::LBRACE, line, col))),
            '}' => tokens.push(Ok(Token::new_punc(Punctuation::RBRACE, line, col))),
            ',' => tokens.push(Ok(Token::new_punc(Punctuation::COMMA, line, col))),
            '.' => tokens.push(Ok(Token::new_punc(Punctuation::DOT, line, col))),
            ';' => tokens.push(Ok(Token::new_punc(Punctuation::SEMICOLON, line, col))),
            '+' => tokens.push(Ok(Token::new_op(Operator::PLUS, line, col))),
            '-' => tokens.push(Ok(Token::new_op(Operator::MINUS, line, col))),
            '/' => {
                if let Some('/') = chars.get(i + 1) {
                    while chars.get(i + 1).map(|&c| c != '\n').unwrap_or(false)
                    {
                        i += 1;
                    }
                } else {
                    tokens.push(Ok(Token::new_op(Operator::SLASH, line, col)));
                }
            }
            '!' => {
                if let Some('=') = chars.get(i + 1) {
                    tokens.push(Ok(Token::new_op(Operator::BANGEQ, line, col)));
                    i += 1;
                    col += 1;
                } else {
                    tokens.push(Ok(Token::new_op(Operator::BANG, line, col)));
                }
            }
            '=' => {
                if let Some('=') = chars.get(i + 1) {
                    tokens.push(Ok(Token::new_op(Operator::EQUALEQ, line, col)));
                    i += 1;
                    col += 1;
                } else {
                    tokens.push(Ok(Token::new_op(Operator::EQUAL, line, col)));
                }
            }
            '<' => {
                if let Some('=') = chars.get(i + 1) {
                    tokens.push(Ok(Token::new_op(Operator::LESSEQ, line, col)));
                    i += 1;
                    col += 1;
                } else {
                    tokens.push(Ok(Token::new_op(Operator::LESS, line, col)));
                }
            }
            '>' => {
                if let Some('=') = chars.get(i + 1) {
                    tokens.push(Ok(Token::new_op(Operator::GREATEREQ, line, col)));
                    i += 1;
                    col += 1;
                } else {
                    tokens.push(Ok(Token::new_op(Operator::GREATER, line, col)));
                }
            }
            '"' => {
                let mut string = vec![];
                let offset = string.len();
                while chars.get(i + 1).map(|&char| char != '"').unwrap_or(false)
                {
                    string.push(*chars.get(i + 1).unwrap());
                    i += 1;
                }
                let string: String = string.iter().collect();
                if let Some('"') = chars.get(i + 1) {
                    tokens.push(Ok(Token::new_str(string, line, col)));
                    col += offset + 1;
                    i += 1;
                } else {
                    tokens.push(Err(format!(
                        "Unterminated string literal in line {} col {}",
                        line, col
                    )));
                    col += string.len();
                };
            }
            '0'..='9' => {
                let mut number = vec![];
                while chars
                    .get(i)
                    .map(|&char| char.is_numeric() || char == '.')
                    .unwrap_or(false)
                {
                    number.push(*chars.get(i).unwrap());
                    i += 1;
                }
                i -= 1;
                let number: String = number.iter().collect();
                if let Ok(number) = number.parse() {
                    tokens.push(Ok(Token::new_num(number, line, col)));
                } else {
                    tokens.push(Err(format!("Invalid number in line {} col {}", line, col)));
                }
                col += number.len() - 1;
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut word = vec![];
                while chars
                    .get(i)
                    .map(|&char| char.is_alphanumeric() || char == '_')
                    .unwrap_or(false)
                {
                    word.push(*chars.get(i).unwrap());
                    i += 1
                }
                i -= 1;
                let word: String = word.iter().collect();
                let offset = word.len();
                let token = match word.as_str() {
                    "return" => Token::new_kw(Keyword::RETURN, line, col),
                    "false" => Token::new_kw(Keyword::FALSE, line, col),
                    "super" => Token::new_kw(Keyword::SUPER, line, col),
                    "while" => Token::new_kw(Keyword::WHILE, line, col),
                    "print" => Token::new_kw(Keyword::PRINT, line, col),
                    "class" => Token::new_kw(Keyword::CLASS, line, col),
                    "this" => Token::new_kw(Keyword::THIS, line, col),
                    "true" => Token::new_kw(Keyword::TRUE, line, col),
                    "else" => Token::new_kw(Keyword::ELSE, line, col),
                    "var" => Token::new_kw(Keyword::VAR, line, col),
                    "nil" => Token::new_kw(Keyword::NIL, line, col),
                    "for" => Token::new_kw(Keyword::FOR, line, col),
                    "and" => Token::new_kw(Keyword::AND, line, col),
                    "fun" => Token::new_kw(Keyword::FUN, line, col),
                    "or" => Token::new_kw(Keyword::OR, line, col),
                    _ => Token::new_id(word, line, col),
                };
                tokens.push(Ok(token));
                col += offset - 1;
            }
            '\t' => {
                col += 4;
                i += 1;
                continue;
            }
            '\n' => {
                line += 1;
                col = 0;
                i += 1;
                continue;
            }
            _ => (),
        };
        i += 1;
        col += 1;
    }
    tokens.push(Ok(Token::EOF(line + 1, 0)));
    tokens
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_string() {
        let program = r#"var = "Hello World!""#;
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::VAR, 0, 0)),
            Ok(Token::new_op(Operator::EQUAL, 0, 4)),
            Ok(Token::new_str("Hello World!".to_string(), 0, 6)),
            Ok(Token::EOF(1, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_tokenize_arithmetic_expr() {
        let program = "1.2 + 2 - 4;";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_num(1.2, 0, 0)),
            Ok(Token::new_op(Operator::PLUS, 0, 4)),
            Ok(Token::new_num(2.0, 0, 6)),
            Ok(Token::new_op(Operator::MINUS, 0, 8)),
            Ok(Token::new_num(4.0, 0, 10)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 0, 11)),
            Ok(Token::EOF(1, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_tokenize_with_comments() {
        let program = "// comment\nprint 1 + 2;";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::PRINT, 1, 0)),
            Ok(Token::new_num(1.0, 1, 6)),
            Ok(Token::new_op(Operator::PLUS, 1, 8)),
            Ok(Token::new_num(2.0, 1, 10)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 1, 11)),
            Ok(Token::EOF(2, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_tokenize_with_tabs() {
        let program = "\n\tprint 1 + 2;";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::PRINT, 1, 4)),
            Ok(Token::new_num(1.0, 1, 10)),
            Ok(Token::new_op(Operator::PLUS, 1, 12)),
            Ok(Token::new_num(2.0, 1, 14)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 1, 15)),
            Ok(Token::EOF(2, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_tokenize_identifier() {
        let program = "var a = b;\nprint a;";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::VAR, 0, 0)),
            Ok(Token::new_id("a".to_string(), 0, 4)),
            Ok(Token::new_op(Operator::EQUAL, 0, 6)),
            Ok(Token::new_id("b".to_string(), 0, 8)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 0, 9)),
            Ok(Token::new_kw(Keyword::PRINT, 1, 0)),
            Ok(Token::new_id("a".to_string(), 1, 6)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 1, 7)),
            Ok(Token::EOF(2, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_tokenize_keywords() {
        let program = "fun foo_o() {\n\tprint 1;\n}";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::FUN, 0, 0)),
            Ok(Token::new_id("foo_o".to_string(), 0, 4)),
            Ok(Token::new_punc(Punctuation::LPAREN, 0, 9)),
            Ok(Token::new_punc(Punctuation::RPAREN, 0, 10)),
            Ok(Token::new_punc(Punctuation::LBRACE, 0, 12)),
            Ok(Token::new_kw(Keyword::PRINT, 1, 4)),
            Ok(Token::new_num(1.0, 1, 10)),
            Ok(Token::new_punc(Punctuation::SEMICOLON, 1, 11)),
            Ok(Token::new_punc(Punctuation::RBRACE, 2, 0)),
            Ok(Token::EOF(3, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_incomplete_string() {
        let program = r#"var = "Hello World!"#;
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::VAR, 0, 0)),
            Ok(Token::new_op(Operator::EQUAL, 0, 4)),
            Err("Unterminated string literal in line 0 col 6".to_string()),
            Ok(Token::EOF(1, 0)),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_invalid_number() {
        let program = "var = 1.2.3;";
        let tokens = tokenize(program);
        let expected = vec![
            Ok(Token::new_kw(Keyword::VAR, 0, 0)),
            Ok(Token::new_op(Operator::EQUAL, 0, 4)),
            Err("Invalid number in line 0 col 6".to_string()),
            Ok(Token::EOF(1, 0)),
        ];
        assert_eq!(tokens, expected);
    }
}
