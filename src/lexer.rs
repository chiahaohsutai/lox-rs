use std::fmt;

pub const KEYWORDS: [&str; 16] = [
    "and", "class", "else", "false", "fun", "for", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

#[derive(Debug)]
pub enum Token {
    EOF,
    NUMBER(f64, u32),
    STRING(String, u32),
    SYMBOL(String, u32),
    KEYWORD(String, u32),
    IDENTIFIER(String, u32),
    INVALID(String, u32),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EOF => write!(f, "EOF"),
            Token::NUMBER(n, _) => write!(f, "NUMBER({})", n),
            Token::STRING(s, _) => write!(f, "STRING({})", s),
            Token::SYMBOL(s, _) => write!(f, "SYMBOL({})", s),
            Token::KEYWORD(s, _) => write!(f, "KEYWORD({})", s),
            Token::IDENTIFIER(s, _) => write!(f, "IDENTIFIER({})", s),
            Token::INVALID(s, _) => write!(f, "INVALID({})", s),
        }
    }
}

pub fn scan(source: String) -> Vec<Token> {
    let mut chars = source.char_indices().peekable();
    let mut tokens = Vec::new();
    let mut line = 0;
    let mut skip = false;

    while let Some((idx, char)) = chars.next() {
        if skip {
            skip = false;
            continue;
        }
        let token = match char {
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
                Some(Token::SYMBOL(char.to_string(), line as u32))
            }
            '/' => {
                if let Some((_, next_char)) = chars.peek() {
                    if next_char == &'/' {
                        chars.by_ref().take_while(|(_, c)| *c != '\n').count();
                        None
                    } else {
                        Some(Token::SYMBOL(char.to_string(), line as u32))
                    }
                } else {
                    Some(Token::SYMBOL(char.to_string(), line as u32))
                }
            }
            '!' | '=' | '>' | '<' => {
                if let Some((_, next_char)) = chars.peek() {
                    if next_char == &'=' {
                        skip = true;
                        Some(Token::SYMBOL(format!("{}{}", char, next_char), line as u32))
                    } else {
                        Some(Token::SYMBOL(char.to_string(), line as u32))
                    }
                } else {
                    Some(Token::SYMBOL(char.to_string(), line as u32))
                }
            }
            '"' => {
                let mut last = '\0';
                let mut string = chars
                    .by_ref()
                    .take_while(|(_, c)| {
                        last = *c;
                        *c != '"'
                    })
                    .map(|(_, c)| c)
                    .collect();
                if last != '"' {
                    Some(Token::INVALID(string, line as u32))
                } else {
                    Some(Token::STRING(string, line as u32))
                }
            }
            '0'..='9' => {
                let number: Vec<String> = chars
                    .by_ref()
                    .take_while(|(_, c)| c.is_digit(10) || *c == '.')
                    .map(|(_, c)| c.to_string())
                    .collect();
                let number = format!("{}{}", char, number.join(""));
                match number.parse::<f64>() {
                    Ok(n) => Some(Token::NUMBER(n, line as u32)),
                    Err(_) => Some(Token::INVALID(number, line as u32)),
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let identifier: Vec<String> = chars
                    .by_ref()
                    .take_while(|(_, c)| c.is_alphanumeric() || *c == '_')
                    .map(|(_, c)| c.to_string())
                    .collect();
                let identifier = format!("{}{}", char, identifier.join(""));
                if KEYWORDS.contains(&&identifier[..]) {
                    Some(Token::KEYWORD(identifier, line as u32))
                } else {
                    Some(Token::IDENTIFIER(identifier, line as u32))
                }
            }
            '\n' => {
                line += 1;
                None
            }
            _ => None,
        };
        if let Some(token) = token {
            tokens.push(token);
        };
    }
    tokens.push(Token::EOF);
    tokens
}
