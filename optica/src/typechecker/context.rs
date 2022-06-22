use std::collections::HashMap;

use super::generator::Generator;
use crate::ast::untyped::*;

#[derive(Debug, Clone)]
pub struct Context {
  blocks: Vec<HashMap<String, Type>>,
  type_aliases: HashMap<String, TypeAlias>,
  canonical_type_names: HashMap<String, String>,
  ty_bare_generator: Generator,
  ty_prefixed_generator: Generator,
}

impl Context {
  pub fn new() -> Self {
    Context {
      blocks: vec![HashMap::new()],
      type_aliases: HashMap::new(),
      canonical_type_names: HashMap::new(),
      ty_bare_generator: Generator::new(),
      ty_prefixed_generator: Generator::new(),
    }
  }

  // Type aliases.

  pub fn set_type_alias(&mut self, alias: TypeAlias) {
    self.type_aliases.insert(alias.name.clone(), alias);
  }

  pub fn get_type_alias(&self, name: &str) -> Option<&TypeAlias> {
    self.type_aliases.get(name)
  }

  // Canonical type names.

  pub fn set_canonical_type_name(&mut self, name: &str, canonical: String) {
    self
      .canonical_type_names
      .insert(name.to_string(), canonical);
  }

  pub fn get_canonical_type_name(&self, name: &str) -> Option<&str> {
    self.canonical_type_names.get(name).map(|it| it.as_str())
  }

  // Blocks.

  pub fn get(&self, name: &str) -> Option<&Type> {
    for block in self.blocks.iter().rev() {
      if let Some(ty) = block.get(name) {
        return Some(ty);
      }
    }

    None
  }

  pub fn set(&mut self, name: &str, ty: Type) {
    self.blocks.last_mut().unwrap().insert(name.to_string(), ty);
  }

  // Type vars generators.

  pub fn next_type(&mut self) -> Type {
    Type::Var(self.ty_bare_generator.next())
  }

  pub fn next_number_type(&mut self) -> Type {
    Type::Var(self.ty_prefixed_generator.next_with_prefix("number"))
  }

  pub fn next_comparable_type(&mut self) -> Type {
    Type::Var(self.ty_prefixed_generator.next_with_prefix("comparable"))
  }

  pub fn next_appendable_type(&mut self) -> Type {
    Type::Var(self.ty_prefixed_generator.next_with_prefix("appendable"))
  }

  // Blocks.

  pub fn block<T, F, E>(&mut self, mut func: F) -> Result<T, E>
  where
    F: FnMut(&mut Self) -> Result<T, E>,
  {
    self.enter_block();
    let result = func(self);
    self.exit_block();

    result
  }

  pub fn enter_block(&mut self) {
    self.blocks.push(HashMap::new());
  }

  pub fn exit_block(&mut self) {
    self
      .blocks
      .pop()
      .expect("Tried to pop the global environment");
  }
}

impl Default for Context {
  fn default() -> Self {
    Self::new()
  }
}
