#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(f64),
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Mimus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Mimus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Semicolon => write!(f, ";"),
            Self::Slash => write!(f, "/"),
            Self::Star => write!(f, "*"),
            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::And => write!(f, "and"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::Fun => write!(f, "fun"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Nil => write!(f, "nil"),
            Self::Or => write!(f, "or"),
            Self::Print => write!(f, "print"),
            Self::Return => write!(f, "return"),
            Self::Super => write!(f, "super"),
            Self::This => write!(f, "this"),
            Self::True => write!(f, "true"),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Lexeme {
    line: usize,
    token: Token,
}
impl Lexeme {
    fn new(line: usize, token: Token) -> Self {
        Self { line, token }
    }
}

impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at line {}", self.token, self.line)
    }
}

fn tokenize<'a, T: AsRef<str>>(program: T) -> Vec<Result<Lexeme, String>> {
    let mut program = program.as_ref().chars().rev().collect::<Vec<char>>();
    let mut lexemes = Vec::new();
    let mut ln = 0;

    while program.len() > 0 {
        let c = program.pop().unwrap();
        if c == '\n' {
            ln += 1;
            continue;
        };
        let lexeme = match c {
            '(' => Ok(Lexeme::new(ln, Token::LeftParen)),
            ')' => Ok(Lexeme::new(ln, Token::RightParen)),
            '{' => Ok(Lexeme::new(ln, Token::LeftBrace)),
            '}' => Ok(Lexeme::new(ln, Token::RightBrace)),
            ',' => Ok(Lexeme::new(ln, Token::Comma)),
            '.' => Ok(Lexeme::new(ln, Token::Dot)),
            '-' => Ok(Lexeme::new(ln, Token::Mimus)),
            '+' => Ok(Lexeme::new(ln, Token::Plus)),
            ';' => Ok(Lexeme::new(ln, Token::Semicolon)),
            '*' => Ok(Lexeme::new(ln, Token::Star)),
            '/' => {
                if let Some(&'/') = program.last() {
                    while program.last() != Some(&'\n') {
                        program.pop();
                    }
                    continue;
                } else {
                    Ok(Lexeme::new(ln, Token::Slash))
                }
            },
            '!' | '=' | '<' | '>' => {
                if let Some(&'=') = program.last() {
                    program.pop();
                    match c {
                        '!' => Ok(Lexeme::new(ln, Token::BangEqual)),
                        '=' => Ok(Lexeme::new(ln, Token::EqualEqual)),
                        '<' => Ok(Lexeme::new(ln, Token::LessEqual)),
                        '>' => Ok(Lexeme::new(ln, Token::GreaterEqual)),
                        _ => unreachable!(),
                    }
                } else {
                    match c {
                        '!' => Ok(Lexeme::new(ln, Token::Bang)),
                        '=' => Ok(Lexeme::new(ln, Token::Equal)),
                        '<' => Ok(Lexeme::new(ln, Token::Less)),
                        '>' => Ok(Lexeme::new(ln, Token::Greater)),
                        _ => unreachable!(),
                    }
                }
            }
            '"' => tokenize_string(&mut program, ln),
            '0'..='9' => tokenize_number(c, &mut program, ln),
            'a'..='z' | 'A'..='Z' | '_' => tokenize_kwid(c, &mut program, ln),
            _ => continue,
        };
    }
    lexemes
}

fn tokenize_number<'a>(curr: char, rest: &mut Vec<char>, ln: usize) -> Result<Lexeme, String> {
    let mut number = String::new();
    number.push(curr);
    while let Some(&c) = rest.last() {
        match c {
            '0'..='9' | '.' => number.push(c),
            _ => break,
        }
        rest.pop();
    }
    match number.parse::<f64>() {
        Ok(number) => Ok(Lexeme::new(ln, Token::Number(number))),
        Err(_) => Err(format!("Invalid number at line {}", ln)),
    }
}

fn tokenize_string<'a>(rest: &mut Vec<char>, ln: usize) -> Result<Lexeme, String> {
    let mut string = String::new();
    loop {
        let c = rest.pop();
        match c {
            Some('"') => break,
            Some(c) => string.push(c),
            None => return Err(format!("Unterminated string at line {}", ln)),
        }
    }
    Ok(Lexeme::new(ln, Token::String(string)))
}

fn tokenize_kwid<'a>(curr: char, rest: &mut Vec<char>, ln: usize) -> Result<Lexeme, String> {
    let mut kwid = String::new();
    kwid.push(curr);
    while let Some(&c) = rest.last() {
        match c {
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => kwid.push(c),
            _ => break,
        }
        rest.pop();
    }
    match kwid.as_str() {
        "and" => Ok(Lexeme::new(ln, Token::And)),
        "class" => Ok(Lexeme::new(ln, Token::Class)),
        "else" => Ok(Lexeme::new(ln, Token::Else)),
        "false" => Ok(Lexeme::new(ln, Token::False)),
        "fun" => Ok(Lexeme::new(ln, Token::Fun)),
        "for" => Ok(Lexeme::new(ln, Token::For)),
        "if" => Ok(Lexeme::new(ln, Token::If)),
        "nil" => Ok(Lexeme::new(ln, Token::Nil)),
        "or" => Ok(Lexeme::new(ln, Token::Or)),
        "print" => Ok(Lexeme::new(ln, Token::Print)),
        "return" => Ok(Lexeme::new(ln, Token::Return)),
        "super" => Ok(Lexeme::new(ln, Token::Super)),
        "this" => Ok(Lexeme::new(ln, Token::This)),
        "true" => Ok(Lexeme::new(ln, Token::True)),
        "var" => Ok(Lexeme::new(ln, Token::Var)),
        "while" => Ok(Lexeme::new(ln, Token::While)),
        _ => Ok(Lexeme::new(ln, Token::Identifier(kwid))),
    }
}
