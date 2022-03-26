use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::errors::CordError;
use crate::runtime::Interpreter;
use crate::number::{Float, Int};
use crate::source::Span;
use super::untyped::{Literal, Type};

#[derive(Debug, Clone)]
pub enum TypedExpression {
  Const(Span, Type, Value),
  Tuple(Span, Type, Vec<Self>),
  List(Span, Type, Vec<Self>),
  Ref(Span, Type, String),
  If(Span, Type, Box<Self>, Box<Self>, Box<Self>),
  Lambda(Span, Type, Vec<TypedPattern>, Box<Self>),
  Application(Span, Type, Box<Self>, Box<Self>),
}

impl PartialEq for TypedExpression {
  fn eq(&self, other: &TypedExpression) -> bool {
    match self {
      | TypedExpression::Const(_, _, lhs_value) => {
        if let TypedExpression::Const(_, _, rhs_value) = other {
          lhs_value == rhs_value
        } else {
          false
        }
      },
      | TypedExpression::Tuple(_, _, lhs_ty_exprs) => {
        if let TypedExpression::Tuple(_, _, rhs_ty_exprs) = other {
          lhs_ty_exprs == rhs_ty_exprs
        } else {
          false
        }
      },
      | TypedExpression::List(_, _, lhs_ty_exprs) => {
        if let TypedExpression::List(_, _, rhs_ty_exprs) = other {
          lhs_ty_exprs == rhs_ty_exprs
        } else {
          false
        }
      },
      | TypedExpression::If(_, _, lhs_cond, lhs_then, lhs_else) => {
        if let TypedExpression::If(_, _, rhs_cond, rhs_then, rhs_else) = other {
          lhs_cond == rhs_cond && lhs_then == rhs_then && lhs_else == rhs_else
        } else {
          false
        }
      },
      | TypedExpression::Lambda(_, _, lhs_ty_pattern, lhs_ty_expr) => {
        if let TypedExpression::Lambda(_, _, rhs_ty_pattern, rhs_ty_expr) = other {
          lhs_ty_pattern == rhs_ty_pattern && lhs_ty_expr == rhs_ty_expr
        } else {
          false
        }
      },
      | TypedExpression::Application(_, _, lhs_ty_expr_l, lhs_ty_expr_r) => {
        if let TypedExpression::Application(_, _, rhs_ty_expr_l, rhs_ty_expr_r) = other {
          lhs_ty_expr_l == rhs_ty_expr_l && lhs_ty_expr_r == rhs_ty_expr_r
        } else {
          false
        }
      },
      | TypedExpression::Ref(_, _, lhs_ty_expr) => {
        if let TypedExpression::Ref(_, _, rhs_ty_expr) = other {
          lhs_ty_expr == rhs_ty_expr
        } else {
          false
        }
      },
    }
  }
}

/// A typed function definition.
#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunction {
  pub header: Type,
  pub name: String,
  pub patterns: Vec<TypedPattern>,
  pub expression: TypedExpression,
}

/// A pattern that represents 1 or more function arguments.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TypedPattern {
  Var(Span, Type, String),
  Adt(Span, Type, Type, Vec<Self>),
  Alias(Span, Type, Box<Self>, String),
  Wildcard(Span),
  Unit(Span),
  Tuple(Span, Type, Vec<Self>),
  List(Span, Type, Vec<Self>),
  BinaryOperator(Span, Type, String, Box<Self>, Box<Self>),
  LitInt(Span, Int),
  LitString(Span, String),
  LitChar(Span, char),
}

/// Unique id for fast comparison between functions.
pub type FunctionId = usize;

/// Built-in function alias.
pub type BuiltinFunction = fn(&mut Interpreter, &[Value]) -> Result<Value, CordError>;

/// Represents the final value after the evaluation of an expression tree.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Value {
  /// The unit value, similar to void in other languages.
  Unit,
  /// Value that can be automatically converted to Float or Int based on the context.
  Number(Int),
  /// An integer value.
  Int(Int),
  /// A float value.
  Float(Float),
  /// UTF-8 string.
  String(String),
  /// Unicode character.
  Char(char),
  /// Homogeneous collection (collection of values of the same type).
  List(Vec<Self>),
  /// Heterogeneous collection (collection of values of different types).
  Tuple(Vec<Self>),
  /// A map between keys and values, where keys are identifiers.
  Record(Vec<(String, Self)>),
  /// Algebraic Data Type *or* Type alias.
  Adt(String, Vec<Self>, Arc<Adt>),
  /// A function value, contains values from partial application.
  Function {
    arity: u32,
    args: Vec<Self>,
    function: Arc<Function>,
  },
}

/// Values are used in `FunCall`, so they must be valid map keys.
impl Hash for Value {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      | Value::Unit => state.write_isize(0),
      | Value::Number(value) => state.write_isize(*value),
      | Value::Int(value) => state.write_isize(*value),
      | Value::Float(value) => state.write_isize(value.to_bits() as isize),
      | Value::String(value) => value.hash(state),
      | Value::Char(value) => state.write_usize(*value as usize),
      | Value::List(value) => value.hash(state),
      | Value::Tuple(value) => value.hash(state),
      | Value::Record(value) => value.hash(state),
      | Value::Adt(a, b, c) => {
        a.hash(state);
        b.hash(state);
        c.hash(state);
      },
      | Value::Function {
        arity,
        args,
        function,
        ..
      } => {
        state.write_u32(*arity);
        args.hash(state);
        state.write_usize(function.get_id());
      },
    }
  }
}

/// Values are used in `FunCall`, so they must be valid map keys.
impl Eq for Value {}

/// Values are used in `FunCall`, so they must be valid map keys.
impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      // Unit eq Unit. Always `true`.
      | (Value::Unit, Value::Unit) => true,

      // Number eq (Number|Int|Float).
      | (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
      | (Value::Number(lhs), Value::Int(rhs)) => lhs == rhs,
      | (Value::Number(lhs), Value::Float(rhs)) => &(*lhs as Float) == rhs,

      // Int eq (Int|Number|Float).
      | (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
      | (Value::Int(lhs), Value::Number(rhs)) => lhs == rhs,
      | (Value::Int(lhs), Value::Float(rhs)) => &(*lhs as Float) == rhs,

      // Float eq (Float|Number|Int).
      | (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
      | (Value::Float(lhs), Value::Number(rhs)) => lhs == &(*rhs as Float),
      | (Value::Float(lhs), Value::Int(rhs)) => lhs == &(*rhs as Float),

      // Everything else. Trivial checks.
      | (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
      | (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
      | (Value::List(lhs), Value::List(rhs)) => lhs == rhs,
      | (Value::Tuple(lhs), Value::Tuple(rhs)) => lhs == rhs,
      | (Value::Record(lhs), Value::Record(rhs)) => lhs == rhs,

      // NOTE: This probably should use even more checks (for `arg_count` and `args`).
      | (
        Value::Function {
          function: lhs_fun, ..
        },
        Value::Function {
          function: rhs_fun, ..
        },
      ) => lhs_fun == rhs_fun,

      // ADTs eq ADTs. This is trivial since [Adt] derives from [PartialEq] and [Hash].
      | (lhs @ Value::Adt(_, _, _), rhs @ Value::Adt(_, _, _)) => lhs == rhs,

      // All other patters uncomparable, so returning `false`.
      | _ => false,
    }
  }
}

impl From<Literal> for Value {
  fn from(literal: Literal) -> Self {
    match literal {
      | Literal::Int(value) => Value::Number(value),
      | Literal::Float(value) => Value::Float(value),
      | Literal::String(value) => Value::String(value),
      | Literal::Char(value) => Value::Char(value),
    }
  }
}

/// Represents an ADT type with all the information about the variants.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
pub struct Adt {
  pub name: String,
  pub types: Vec<String>,
  pub variants: Vec<AdtVariant>,
}

/// Is a variant in an ADT.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
pub struct AdtVariant {
  pub name: String,
  pub types: Vec<Type>,
}

/// Represents a function that can be a definition or a built-in.
#[derive(Debug)]
pub enum Function {
  External(FunctionId, ExternalFunction, Type),
  Definition {
    id: FunctionId,
    patterns: Vec<TypedPattern>,
    expression: TypedExpression,
    function_type: Type,
    captures: HashMap<String, Value>,
  },
}

impl Function {
  fn get_id(&self) -> FunctionId {
    match self {
      | Function::External(id, ..) => *id,
      | Function::Definition { id, .. } => *id,
    }
  }

  pub fn get_type(&self) -> Type {
    match self {
      | Function::External(_, _, ty, ..) => ty.clone(),
      | Function::Definition { function_type, .. } => function_type.clone(),
    }
  }
}

/// Functions are compared using only the `FunctionId`.
impl Eq for Function {}

impl PartialEq for Function {
  fn eq(&self, other: &Function) -> bool {
    self.get_id() == other.get_id()
  }
}

impl Serialize for Function {
  fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    panic!("ExternalFunction cannot be serialized");
  }
}

impl<'de> Deserialize<'de> for Function {
  fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    panic!("ExternalFunction cannot be deserialized");
  }
}

pub struct ExternalFunction {
  pub name: String,
  pub function: BuiltinFunction,
}

impl Debug for ExternalFunction {
  fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
    write!(f, "<external function '{}'>", self.name)
  }
}
