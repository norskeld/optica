use std::collections::HashMap;

use crate::ast::typed::Value;

#[derive(Clone, Debug)]
pub struct Stack {
  frames: Vec<Frame>,
}

#[derive(Clone, Debug)]
pub struct Frame {
  pub values: HashMap<String, Value>,
}

impl Stack {
  pub fn new() -> Self {
    Stack {
      frames: vec![Frame {
        values: HashMap::new(),
      }],
    }
  }

  pub fn add(&mut self, name: &str, value: Value) {
    // TODO: Get rid of unwrap and handle possible failure.
    self
      .frames
      .last_mut()
      .unwrap()
      .values
      .insert(name.to_owned(), value);
  }

  pub fn find(&self, name: &str) -> Option<Value> {
    for frame in self.frames.iter().rev() {
      let value = frame.values.get(name);

      if value.is_some() {
        return value.cloned();
      }
    }

    None
  }

  pub fn enter_block(&mut self) {
    self.frames.push(Frame {
      values: HashMap::new(),
    });
  }

  pub fn exit_block(&mut self) {
    self
      .frames
      .pop()
      .expect("Tried to pop all the stack frames");
  }
}

impl Default for Stack {
  fn default() -> Self {
    Self::new()
  }
}
