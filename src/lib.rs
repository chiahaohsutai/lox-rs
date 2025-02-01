mod expressions;
mod tokenizer;

use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
enum Object {
    Function(String, usize, fn(Vec<Object>) -> Option<Object>),
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Function(name, _, _) => write!(f, "<fn {}>", name),
            Object::Number(num) => write!(f, "{}", num),
            Object::String(s) => write!(f, "{}", s),
            Object::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Enviorment {
    parent: Option<Box<Enviorment>>,
    values: HashMap<String, Object>,
}

impl Enviorment {
    fn define(&mut self, key: String, value: Object) {
        self.values.insert(key, value);
    }

    fn assign(&mut self, key: String, value: Object) -> Result<Object, String> {
        if self.values.contains_key(&key) {
            let prev = self.values.remove(&key);
            self.values.insert(key, value);
            Ok(prev.unwrap())
        } else {
            match &mut self.parent {
                Some(parent) => parent.assign(key, value),
                None => Err(format!("Undefined variable '{}'", key)),
            }
        }
    }

    fn get(&self, key: String) -> Result<Object, String> {
        if self.values.contains_key(&key) {
            Ok(self.values.get(&key).unwrap().clone())
        } else {
            match &self.parent {
                Some(parent) => parent.get(key),
                None => Err(format!("Undefined variable '{}'", key)),
            }
        }
    }
}

impl Default for Enviorment {
    fn default() -> Self {
        Enviorment {
            parent: None,
            values: HashMap::new(),
        }
    }
}

impl From<Box<Enviorment>> for Enviorment {
    fn from(env: Box<Enviorment>) -> Self {
        Enviorment {
            parent: Some(env),
            values: HashMap::new(),
        }
    }
}

struct Interpreter {
    env: Enviorment,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter {
            env: Enviorment::default(),
        }
    }
}

impl Interpreter {
    fn eval(&mut self, program: &str) -> Result<Object, String> {
        // let tokens = tokenizer::tokenize(program);
        // let mut statements = statements::parse(tokens);
        // let mut result = Object::Boolean(false);
        // for statement in statements.iter_mut() {
        //     result = statement.eval(&mut self.env)?;
        // }
        // Ok(result)
        todo!()
    }
}
