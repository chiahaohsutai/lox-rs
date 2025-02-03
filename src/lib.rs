mod expressions;
mod tokenizer;
mod statements;
mod enviorment;
mod interpreter;

use enviorment::Enviorment;
use std::fmt::Display;

type Fun = fn(Vec<Option<Object>>) -> Result<Option<Object>, String>;

#[derive(Debug, Clone, PartialEq)]
enum Object {
    Function(Fun, String, usize),
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Function(_, name, _) => write!(f, "<fn {}>", name),
            Object::Number(num) => write!(f, "{}", num),
            Object::String(s) => write!(f, "{}", s),
            Object::Boolean(b) => write!(f, "{}", b),
        }
    }
}

trait Eval {
    type Output;
    fn eval(&self, env: &mut Enviorment) -> Result<Self::Output, String>;
}

trait Create {
    type Output;
    fn create(tokens: &mut Vec<tokenizer::Token>) -> Result<Self::Output, String>;
}
