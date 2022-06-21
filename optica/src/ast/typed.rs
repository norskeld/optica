use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::errors::LangError;
use crate::runtime::Interpreter;
use crate::source::Span;
use super::untyped::*;
use super::{Float, Int};

/// Unique id for fast comparison between functions.
pub type FunctionId = usize;

/// Built-in function alias.
pub type BuiltinFunction = fn(&mut Interpreter, &[Value]) -> Result<Value, LangError>;

#[derive(Clone, Debug, PartialEq)]
pub enum TypedStatement {
  Alias(TypeAlias),
  Adt(String, Arc<Adt>),
  Definition(String, TypedDefinition),
  Infix(String, String, Type),
  Port(String, Type),
}

impl TypedStatement {
  pub fn get_name(&self) -> &str {
    match self {
      | TypedStatement::Port(name, ..) => name,
      | TypedStatement::Definition(name, ..) => name,
      | TypedStatement::Alias(alias, ..) => &alias.name,
      | TypedStatement::Adt(name, ..) => name,
      | TypedStatement::Infix(name, ..) => name,
    }
  }

  pub fn get_type(&self) -> Option<&Type> {
    match self {
      | TypedStatement::Port(_, ty) => Some(ty),
      | TypedStatement::Definition(_, ty) => Some(&ty.header),
      | TypedStatement::Alias(_) => None,
      | TypedStatement::Adt(_, _) => None,
      | TypedStatement::Infix(_, _, ty) => Some(ty),
    }
  }
}

#[derive(Clone, Debug)]
pub enum TypedExpression {
  Const(Span, Type, Value),
  Tuple(Span, Type, Vec<TypedExpression>),
  List(Span, Type, Vec<TypedExpression>),
  Ref(Span, Type, String),
  If(
    Span,
    Type,
    Box<TypedExpression>,
    Box<TypedExpression>,
    Box<TypedExpression>,
  ),
  Case(
    Span,
    Type,
    Box<TypedExpression>,
    Vec<(TypedPattern, TypedExpression)>,
  ),
  Lambda(Span, Type, Vec<TypedPattern>, Box<TypedExpression>),
  Application(Span, Type, Box<TypedExpression>, Box<TypedExpression>),
  Let(Span, Type, Vec<TypedLet>, Box<TypedExpression>),
}

impl TypedExpression {
  pub fn get_span(&self) -> Span {
    *match self {
      | TypedExpression::Const(span, _, _) => span,
      | TypedExpression::Tuple(span, _, _) => span,
      | TypedExpression::List(span, _, _) => span,
      | TypedExpression::Ref(span, _, _) => span,
      | TypedExpression::If(span, _, _, _, _) => span,
      | TypedExpression::Case(span, _, _, _) => span,
      | TypedExpression::Lambda(span, _, _, _) => span,
      | TypedExpression::Application(span, _, _, _) => span,
      | TypedExpression::Let(span, _, _, _) => span,
    }
  }

  pub fn get_type(&self) -> Type {
    match self {
      | TypedExpression::Const(_, ty, _) => ty.clone(),
      | TypedExpression::Tuple(_, ty, _) => ty.clone(),
      | TypedExpression::List(_, ty, _) => ty.clone(),
      | TypedExpression::Ref(_, ty, _) => ty.clone(),
      | TypedExpression::If(_, ty, _, _, _) => ty.clone(),
      | TypedExpression::Case(_, ty, _, _) => ty.clone(),
      | TypedExpression::Lambda(_, ty, _, _) => ty.clone(),
      | TypedExpression::Application(_, ty, _, _) => ty.clone(),
      | TypedExpression::Let(_, ty, _, _) => ty.clone(),
    }
  }
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
      | TypedExpression::Case(_, _, lhs_expr, lhs_branches) => {
        if let TypedExpression::Case(_, _, rhs_expr, rhs_branches) = other {
          lhs_expr == rhs_expr && lhs_branches == rhs_branches
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
      | TypedExpression::Let(_, _, lhs_defs, lhs_body) => {
        if let TypedExpression::Let(_, _, rhs_defs, rhs_body) = other {
          lhs_defs == rhs_defs && lhs_body == rhs_body
        } else {
          false
        }
      },
    }
  }
}

/// A typed function definition.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedDefinition {
  pub header: Type,
  pub name: String,
  pub patterns: Vec<TypedPattern>,
  pub expression: TypedExpression,
}

/// A let declaration.
#[derive(Clone, Debug, PartialEq)]
pub enum TypedLet {
  Definition(TypedDefinition),
  Pattern(TypedPattern, TypedExpression),
}

/// A pattern that represents 1 or more function arguments.
#[derive(Clone, Debug, PartialEq)]
pub enum TypedPattern {
  Var(Span, Type, String),
  Adt(Span, Type, Type, Vec<TypedPattern>),
  Alias(Span, Type, Box<TypedPattern>, String),
  Wildcard(Span),
  Unit(Span),
  Tuple(Span, Type, Vec<TypedPattern>),
  List(Span, Type, Vec<TypedPattern>),
  BinaryOperator(Span, Type, String, Box<TypedPattern>, Box<TypedPattern>),
  LitInt(Span, Int),
  LitString(Span, String),
  LitChar(Span, char),
}

impl TypedPattern {
  pub fn get_span(&self) -> Span {
    *match self {
      | TypedPattern::Unit(span) => span,
      | TypedPattern::Var(span, _, _) => span,
      | TypedPattern::Adt(span, _, _, _) => span,
      | TypedPattern::Alias(span, _, _, _) => span,
      | TypedPattern::Wildcard(span) => span,
      | TypedPattern::Tuple(span, _, _) => span,
      | TypedPattern::List(span, _, _) => span,
      | TypedPattern::BinaryOperator(span, _, _, _, _) => span,
      | TypedPattern::LitInt(span, _) => span,
      | TypedPattern::LitString(span, _) => span,
      | TypedPattern::LitChar(span, _) => span,
    }
  }

  pub fn get_type(&self) -> Type {
    match self {
      | TypedPattern::Unit(_) => Type::Unit,
      | TypedPattern::Var(_, ty, _) => ty.clone(),
      | TypedPattern::Adt(_, ty, _, _) => ty.clone(),
      | TypedPattern::Alias(_, ty, _, _) => ty.clone(),
      | TypedPattern::Wildcard(_) => Type::Var("_".to_string()),
      | TypedPattern::Tuple(_, ty, _) => ty.clone(),
      | TypedPattern::List(_, ty, _) => ty.clone(),
      | TypedPattern::BinaryOperator(_, ty, _, _, _) => ty.clone(),
      | TypedPattern::LitInt(_, _) => Type::Tag("Int".to_string(), vec![]),
      | TypedPattern::LitString(_, _) => Type::Tag("String".to_string(), vec![]),
      | TypedPattern::LitChar(_, _) => Type::Tag("Char".to_string(), vec![]),
    }
  }
}

/// Represents the final value after the evaluation of an expression tree.
#[derive(Clone, Debug)]
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
  List(Vec<Value>),
  /// Heterogeneous collection (collection of values of different types).
  Tuple(Vec<Value>),
  /// Algebraic Data Type *or* Type alias.
  Adt(String, Vec<Value>, Arc<Adt>),
  /// A function value, contains values from partial application.
  Function {
    arity: u32,
    args: Vec<Value>,
    function: Arc<Function>,
  },
}

impl Value {
  pub fn get_type(&self) -> Type {
    match self {
      | Value::Unit => Type::Unit,
      | Value::Number(_) => Type::Var("number".to_string()),
      | Value::Int(_) => Type::Tag("Int".to_string(), vec![]),
      | Value::Float(_) => Type::Tag("Float".to_string(), vec![]),
      | Value::String(_) => Type::Tag("String".to_string(), vec![]),
      | Value::Char(_) => Type::Tag("Char".to_string(), vec![]),
      | Value::List(items) => {
        if items.is_empty() {
          Type::Tag("List".to_string(), vec![Type::Var("a".to_string())])
        } else {
          Type::Tag("List".to_string(), vec![items.first().unwrap().get_type()])
        }
      },
      | Value::Tuple(items) => Type::Tuple(items.iter().map(Value::get_type).collect()),
      | Value::Adt(_, _, adt) => Type::Tag(
        adt.name.clone(),
        adt.types.iter().cloned().map(Type::Var).collect(),
      ),
      | Value::Function { function, args, .. } => {
        Value::reduce_args(args.len(), &function.get_type()).clone()
      },
    }
  }

  fn reduce_args(args: usize, ty: &Type) -> &Type {
    if args == 0 {
      ty
    } else if let Type::Function(_, ref output) = ty {
      Self::reduce_args(args - 1, output)
    } else {
      ty
    }
  }
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
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Adt {
  pub name: String,
  pub types: Vec<String>,
  pub variants: Vec<AdtVariant>,
}

/// Is a variant in an ADT.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct AdtVariant {
  pub name: String,
  pub types: Vec<Type>,
}

/// Represents a function that can be a definition or a built-in.
#[derive(Debug)]
pub enum Function {
  External {
    id: FunctionId,
    function: ExternalFunction,
    function_type: Type,
  },
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
      | Self::External { id, .. } => *id,
      | Self::Definition { id, .. } => *id,
    }
  }

  pub fn get_type(&self) -> Type {
    match self {
      | Self::External { function_type, .. } => function_type.clone(),
      | Self::Definition { function_type, .. } => function_type.clone(),
    }
  }
}

impl Eq for Function {}

impl PartialEq for Function {
  fn eq(&self, other: &Function) -> bool {
    self.get_id() == other.get_id()
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
