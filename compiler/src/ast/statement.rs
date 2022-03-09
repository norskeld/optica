use serde::{Deserialize, Serialize};

use crate::ast::expression::{Expression, Pattern};
use crate::source::Span;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Statement {
  Alias(String, Vec<String>, Type),
  Adt(String, Vec<String>, Vec<(Span, String, Vec<Type>)>),
  FunctionDef(FunctionDef),
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Hash)]
pub enum Type {
  Unit,
  Var(String),
  Tag(String, Vec<Type>),
  Function(Box<Type>, Box<Type>),
  Tuple(Vec<Type>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct TypeAlias {
  pub name: String,
  pub variables: Vec<String>,
  pub replacement: Type,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FunctionDef {
  pub header: Option<Type>,
  pub name: String,
  pub patterns: Vec<Pattern>,
  pub expression: Expression,
}
