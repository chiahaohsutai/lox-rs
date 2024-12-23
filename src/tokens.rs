pub enum Token {
    EOF(usize),
    NUM(f64, usize),
    STR(String, usize),
    KWD(Keyword, usize),
    IDEN(String, usize),
    OPER(Operator, usize),
    PUNC(Punctuation, usize),
    INVALID(String, usize),
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::EOF(_) => write!(f, "EOF"),
            Self::NUM(n, _) => write!(f, "NUMBER[{}]", n),
            Self::STR(s, _) => write!(f, "STRING[{}]", s),
            Self::KWD(k, _) => write!(f, "{}", k),
            Self::IDEN(i, _) => write!(f, "IDENTIFIER[{}]", i),
            Self::OPER(o, _) => write!(f, "{}", o),
            Self::PUNC(p, _) => write!(f, "{}", p),
            Self::INVALID(i, _) => write!(f, "INVALID[{}]", i),
        }
    }
}
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
            Self::IF => write!(f, "KEYWORD[IF]"),
            Self::OR => write!(f, "KEYWORD[OR]"),
            Self::FUN => write!(f, "KEYWORD[FUN]"),
            Self::AND => write!(f, "KEYWORD[AND]"),
            Self::FOR => write!(f, "KEYWORD[FOR]"),
            Self::NIL => write!(f, "KEYWORD[NIL]"),
            Self::VAR => write!(f, "KEYWORD[VAR]"),
            Self::ELSE => write!(f, "KEYWORD[ELSE]"),
            Self::TRUE => write!(f, "KEYWORD[TRUE]"),
            Self::THIS => write!(f, "KEYWORD[THIS]"),
            Self::CLASS => write!(f, "KEYWORD[CLASS]"),
            Self::PRINT => write!(f, "KEYWORD[PRINT]"),
            Self::WHILE => write!(f, "KEYWORD[WHILE]"),
            Self::SUPER => write!(f, "KEYWORD[SUPER]"),
            Self::FALSE => write!(f, "KEYWORD[FALSE]"),
            Self::RETURN => write!(f, "KEYWORD[RETURN]"),
        }
    }
}
impl Keyword {
    pub fn list() -> Vec<String> {
        vec![
            "if".to_string(),
            "or".to_string(),
            "fun".to_string(),
            "and".to_string(),
            "for".to_string(),
            "nil".to_string(),
            "var".to_string(),
            "else".to_string(),
            "true".to_string(),
            "this".to_string(),
            "class".to_string(),
            "print".to_string(),
            "while".to_string(),
            "super".to_string(),
            "false".to_string(),
            "return".to_string(),
        ]
    }
    pub fn new(str: String) -> Option<Self> {
        match str.as_str() {
            "if" => Some(Self::IF),
            "or" => Some(Self::OR),
            "fun" => Some(Self::FUN),
            "and" => Some(Self::AND),
            "for" => Some(Self::FOR),
            "nil" => Some(Self::NIL),
            "var" => Some(Self::VAR),
            "else" => Some(Self::ELSE),
            "true" => Some(Self::TRUE),
            "this" => Some(Self::THIS),
            "class" => Some(Self::CLASS),
            "print" => Some(Self::PRINT),
            "while" => Some(Self::WHILE),
            "super" => Some(Self::SUPER),
            "false" => Some(Self::FALSE),
            "return" => Some(Self::RETURN),
            _ => None,
        }
    }
}
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
            Self::DOT => write!(f, "PUNCTUATION[.]"),
            Self::COMMA => write!(f, "PUNCTUATION[,]"),
            Self::LPAREN => write!(f, "PUNCTUATION[(]"),
            Self::RPAREN => write!(f, "PUNCTUATION[)]"),
            Self::LBRACE => write!(f, "PUNCTUATION[{{]"),
            Self::RBRACE => write!(f, "PUNCTUATION[}}]"),
            Self::SEMICOLON => write!(f, "PUNCTUATION[;]"),
        }
    }
}
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
            Self::BANG => write!(f, "OPERATOR[!]"),
            Self::LESS => write!(f, "OPERATOR[<]"),
            Self::PLUS => write!(f, "OPERATOR[+]"),
            Self::STAR => write!(f, "OPERATOR[*]"),
            Self::EQUAL => write!(f, "OPERATOR[=]"),
            Self::SLASH => write!(f, "OPERATOR[/]"),
            Self::MINUS => write!(f, "OPERATOR[-]"),
            Self::LESSEQ => write!(f, "OPERATOR[<=]"),
            Self::BANGEQ => write!(f, "OPERATOR[!=]"),
            Self::GREATER => write!(f, "OPERATOR[>]"),
            Self::EQUALEQ => write!(f, "OPERATOR[==]"),
            Self::GREATEREQ => write!(f, "OPERATOR[>=]"),
        }
    }
}
