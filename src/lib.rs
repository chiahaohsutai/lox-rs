mod evaluate;
mod parser;
mod tokenizer;

use crate::evaluate::evaluate;
use crate::parser::Literal;
use std::collections::HashMap;
use std::default::Default;

struct Enviorment {
    parent: Option<Box<Enviorment>>,
    values: HashMap<String, Option<Literal>>,
}

impl Default for Enviorment {
    fn default() -> Self {
        Self {
            parent: None,
            values: HashMap::new(),
        }
    }
}

impl Enviorment {
    fn new(parent: Enviorment) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, value: Option<Literal>) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: String, value: Option<Literal>) -> Result<Option<Literal>, String> {
        if self.values.contains_key(&name) {
            Ok(self.values.insert(name, value).flatten())
        } else if let Some(parent) = &mut self.parent {
            Ok(parent.assign(name, value)?)
        } else {
            Err(format!("Undefined variable '{}'", name))
        }
    }

    fn get(&self, name: String) -> Result<Option<Literal>, String> {
        if let Some(value) = self.values.get(&name) {
            Ok(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            Err(format!("Undefined variable '{}'", name))
        }
    }
}

pub fn interpret<T: AsRef<str>>(program: T) {
    let tokens = tokenizer::tokenize(program.as_ref());
    let has_errors = tokens.iter().any(|t| t.is_err());
    if has_errors {
        let errors: Vec<_> = tokens.into_iter().filter_map(|t| t.err()).collect();
        for error in errors {
            eprintln!("{}", error);
            std::process::exit(65)
        }
    } else {
        let tokens: Vec<_> = tokens.into_iter().map(|t| t.unwrap()).collect();
        let (statements, errors) = parser::parse(tokens);
        if !errors.is_empty() {
            for error in errors {
                eprintln!("{}", error);
            }
            std::process::exit(65)
        }
        let mut enviormnent = HashMap::new();
        for statement in statements {
            let result = evaluate(statement, &mut enviormnent);
            if let Result::Err(e) = result {
                eprintln!("{}", e);
                std::process::exit(70)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_get() {
        let mut env = Enviorment::default();
        env.define("a".to_string(), Some(Literal::Number(1.0)));
        assert_eq!(env.get("a".to_string()), Ok(Some(Literal::Number(1.0))));
    }

    #[test]
    fn test_env_assign() {
        let mut env = Enviorment::default();
        env.define("a".to_string(), Some(Literal::Number(1.0)));
        assert_eq!(
            env.assign("a".to_string(), Some(Literal::Number(2.0))),
            Ok(Some(Literal::Number(1.0)))
        );
        assert_eq!(env.get("a".to_string()), Ok(Some(Literal::Number(2.0))));
    }

    #[test]
    fn test_env_define() {
        let mut env = Enviorment::default();
        env.define("a".to_string(), Some(Literal::Number(1.0)));
        assert_eq!(env.get("a".to_string()), Ok(Some(Literal::Number(1.0))));
    }

    #[test]
    fn test_env_assign_undefined() {
        let mut env = Enviorment::default();
        assert_eq!(
            env.assign("a".to_string(), Some(Literal::Number(2.0))),
            Err("Undefined variable 'a'".to_string())
        );
    }

    #[test]
    fn test_env_get_undefined() {
        let env = Enviorment::default();
        assert_eq!(
            env.get("a".to_string()),
            Err("Undefined variable 'a'".to_string())
        );
    }

    #[test]
    fn test_env_get_parent() {
        let mut parent = Enviorment::default();
        parent.define("a".to_string(), Some(Literal::Number(1.0)));
        let env = Enviorment::new(parent);
        assert_eq!(env.get("a".to_string()), Ok(Some(Literal::Number(1.0))));
    }

    #[test]
    fn test_env_assign_parent() {
        let mut parent = Enviorment::default();
        parent.define("a".to_string(), Some(Literal::Number(1.0)));
        let mut env = Enviorment::new(parent);
        assert_eq!(
            env.assign("a".to_string(), Some(Literal::Number(2.0))),
            Ok(Some(Literal::Number(1.0)))
        );
        assert_eq!(env.get("a".to_string()), Ok(Some(Literal::Number(2.0))));
    }

    #[test]
    fn test_env_define_parent() {
        let mut parent = Enviorment::default();
        parent.define("a".to_string(), Some(Literal::Number(1.0)));
        let mut env = Enviorment::new(parent);
        env.define("b".to_string(), Some(Literal::Number(2.0)));
        assert_eq!(env.get("b".to_string()), Ok(Some(Literal::Number(2.0))));
    }

    #[test]
    fn test_env_assign_parent_undefined() {
        let mut parent = Enviorment::default();
        parent.define("a".to_string(), Some(Literal::Number(1.0)));
        let mut env = Enviorment::new(parent);
        assert_eq!(
            env.assign("b".to_string(), Some(Literal::Number(2.0))),
            Err("Undefined variable 'b'".to_string())
        );
    }

    #[test]
    fn test_env_get_parent_undefined() {
        let parent = Enviorment::default();
        let env = Enviorment::new(parent);
        assert_eq!(
            env.get("a".to_string()),
            Err("Undefined variable 'a'".to_string())
        );
    }
}
