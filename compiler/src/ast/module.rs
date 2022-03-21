use serde::{Deserialize, Serialize};

use crate::ast::statement::Statement;

/// Represents a source file's AST.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Default)]
pub struct Module {
  pub header: Option<ModuleHeader>,
  pub imports: Vec<ModuleImport>,
  pub statements: Vec<Statement>,
}

/// Module header with the module name and the list of exported definitions/types.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ModuleHeader {
  pub name: String,
  pub exports: ModuleExports,
}

/// Module exports. Either selected ones or all.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleExports {
  Just(Vec<ModuleExport>),
  All,
}

/// Exported definition, type, or ADT.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleExport {
  Adt(String, AdtExports),
  Type(String),
  Function(String),
  BinaryOperator(String),
}

/// Exported variants of ADT. Either selected ones or all.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum AdtExports {
  Variants(Vec<String>),
  All,
}

/// A module import.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ModuleImport {
  pub path: Vec<String>,
  pub alias: Option<String>,
  pub exports: Option<ModuleExports>,
}
