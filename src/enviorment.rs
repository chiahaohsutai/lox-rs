use crate::Object;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Enviorment {
    parent: Option<Box<Enviorment>>,
    values: HashMap<String, Option<Object>>,
}

impl Enviorment {
    pub fn define(&mut self, key: String, value: Option<Object>) {
        self.values.insert(key, value);
    }

    pub fn assign(&mut self, key: String, value: Option<Object>) -> Result<(), String> {
        if self.values.contains_key(&key) {
            let _ = self.values.remove(&key);
            self.values.insert(key, value);
            Ok(())
        } else {
            match &mut self.parent {
                Some(parent) => parent.assign(key, value),
                None => Err(format!("Undefined variable '{}'", key)),
            }
        }
    }

    pub fn get(&self, key: String) -> Result<Option<Object>, String> {
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
