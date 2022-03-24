use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

use crate::number::{Float, Int};
use crate::source::Span;

/// Unevaluated expression tree.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Expression {
  Unit(Span),
  Literal(Span, Literal),
  Tuple(Span, Vec<Expression>),
  List(Span, Vec<Expression>),
  If(Span, Box<Expression>, Box<Expression>, Box<Expression>),
  Lambda(Span, Vec<Pattern>, Box<Expression>),
  Application(Span, Box<Expression>, Box<Expression>),
  OperatorChain(Span, Vec<Expression>, Vec<String>),
  Ref(Span, String),
  QualifiedRef(Span, Vec<String>, String),
}

impl PartialEq for Expression {
  fn eq(&self, other: &Expression) -> bool {
    match self {
      | Expression::Unit(_) => {
        matches!(other, Expression::Unit(_))
      },
      | Expression::Literal(_, lhs) => {
        if let Expression::Literal(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Expression::Tuple(_, lhs) => {
        if let Expression::Tuple(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Expression::List(_, lhs) => {
        if let Expression::List(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Expression::If(_, lhs_cond, lhs_then, lhs_else) => {
        if let Expression::If(_, rhs_cond, rhs_then, rhs_else) = other {
          lhs_cond == rhs_cond && lhs_then == rhs_then && lhs_else == rhs_else
        } else {
          false
        }
      },
      | Expression::Lambda(_, lhs_pattern, lhs_expr) => {
        if let Expression::Lambda(_, rhs_pattern, rhs_expr) = other {
          lhs_pattern == rhs_pattern && lhs_expr == rhs_expr
        } else {
          false
        }
      },
      | Expression::Application(_, lhs_func, lhs_arg) => {
        if let Expression::Application(_, rhs_func, rhs_arg) = other {
          lhs_func == rhs_func && lhs_arg == rhs_arg
        } else {
          false
        }
      },
      | Expression::OperatorChain(_, lhs_exprs, lhs_ops) => {
        if let Expression::OperatorChain(_, rhs_exprs, rhs_ops) = other {
          lhs_exprs == rhs_exprs && lhs_ops == rhs_ops
        } else {
          false
        }
      },
      | Expression::Ref(_, lhs) => {
        if let Expression::Ref(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Expression::QualifiedRef(_, lhs_path, lhs_ref) => {
        if let Expression::QualifiedRef(_, rhs_path, rhs_ref) = other {
          lhs_path == rhs_path && lhs_ref == rhs_ref
        } else {
          false
        }
      },
    }
  }
}

/// A value literal. Bools will be handled in later stages.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Literal {
  Int(Int),
  Float(Float),
  String(String),
  Char(char),
}

/// [Hash] implementation for function memoization. This is needed to handle floats.
impl Hash for Literal {
  fn hash<H: Hasher>(&self, state: &mut H) {
    // NOTE: Not sure these coercions are correct and won't break something. :thinking:
    match self {
      | Literal::Int(value) => state.write_isize(*value),
      | Literal::Float(value) => state.write_isize(value.to_bits() as isize),
      | Literal::String(value) => value.hash(state),
      | Literal::Char(value) => state.write_usize(*value as usize),
    }
  }
}

/// Blanket [Eq] implementation because we have a custom [PartialEq] implementation.
impl Eq for Literal {}

/// Implementing [PartialEq] because we have a custom [Hash] implementation.
impl PartialEq for Literal {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      | (Literal::Int(lhs), Literal::Int(rhs)) => lhs == rhs,
      | (Literal::Float(lhs), Literal::Float(rhs)) => lhs == rhs,
      | (Literal::String(lhs), Literal::String(rhs)) => lhs == rhs,
      | (Literal::Char(lhs), Literal::Char(rhs)) => lhs == rhs,
      | _ => false,
    }
  }
}

/// A pattern that represents one or more function arguments.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Pattern {
  Var(Span, String),
  Adt(Span, String, Vec<Pattern>),
  BinaryOperator(Span, String, Box<Pattern>, Box<Pattern>),
  Alias(Span, Box<Pattern>, String),
  Wildcard(Span),
  Unit(Span),
  Tuple(Span, Vec<Pattern>),
  List(Span, Vec<Pattern>),
  LitInt(Span, Int),
  LitString(Span, String),
  LitChar(Span, char),
}

impl Pattern {
  pub fn span(&self) -> Span {
    *match self {
      | Pattern::Var(span, _) => span,
      | Pattern::Adt(span, _, _) => span,
      | Pattern::BinaryOperator(span, _, _, _) => span,
      | Pattern::Alias(span, _, _) => span,
      | Pattern::Wildcard(span) => span,
      | Pattern::Unit(span) => span,
      | Pattern::Tuple(span, _) => span,
      | Pattern::List(span, _) => span,
      | Pattern::LitInt(span, _) => span,
      | Pattern::LitString(span, _) => span,
      | Pattern::LitChar(span, _) => span,
    }
  }
}

/// Custom [PartialEq] implementation because we have derived [PartialEq] for [Definition].
impl PartialEq for Pattern {
  fn eq(&self, other: &Pattern) -> bool {
    match self {
      | Pattern::Unit(_) => {
        matches!(other, Pattern::Unit(_))
      },
      | Pattern::Wildcard(_) => {
        matches!(other, Pattern::Wildcard(_))
      },
      | Pattern::Var(_, lhs) => {
        if let Pattern::Var(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::Adt(_, lhs_name, lhs_patterns) => {
        if let Pattern::Adt(_, rhs_name, rhs_patterns) = other {
          lhs_name == rhs_name && lhs_patterns == rhs_patterns
        } else {
          false
        }
      },
      | Pattern::Tuple(_, lhs) => {
        if let Pattern::Tuple(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::List(_, lhs) => {
        if let Pattern::List(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::LitInt(_, lhs) => {
        if let Pattern::LitInt(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::LitString(_, lhs) => {
        if let Pattern::LitString(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::LitChar(_, lhs) => {
        if let Pattern::LitChar(_, rhs) = other {
          lhs == rhs
        } else {
          false
        }
      },
      | Pattern::Alias(_, lhs_pattern, lhs_name) => {
        if let Pattern::Alias(_, rhs_pattern, rhs_name) = other {
          lhs_pattern == rhs_pattern && lhs_name == rhs_name
        } else {
          false
        }
      },
      | Pattern::BinaryOperator(_, lhs_op, lhs_l, lhs_r) => {
        if let Pattern::BinaryOperator(_, rhs_op, rhs_l, rhs_r) = other {
          lhs_op == rhs_op && lhs_l == rhs_l && lhs_r == rhs_r
        } else {
          false
        }
      },
    }
  }
}

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

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Statement {
  Alias(String, Vec<String>, Type),
  Adt(String, Vec<String>, Vec<(Span, String, Vec<Type>)>),
  Function(Function),
  Infix(InfixDirection, Int, String, String),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum InfixDirection {
  Left,
  Right,
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
pub struct Function {
  pub header: Option<Type>,
  pub name: String,
  pub patterns: Vec<Pattern>,
  pub expression: Expression,
}
