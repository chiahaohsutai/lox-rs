use crate::{enviorment::Enviorment, Object};

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
