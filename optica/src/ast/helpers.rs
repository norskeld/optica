use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::IntoIter;

use crate::source::Span;
use super::typed::*;
use super::untyped::*;

/// Global atomic function ID.
static FUNC_ID: AtomicUsize = AtomicUsize::new(0);

/// Retrieves and increments the next free function ID.
pub fn function_id() -> FunctionId {
  FUNC_ID.fetch_add(1, Ordering::SeqCst)
}

pub fn declaration_name(decl: &TypedStatement) -> &str {
  match decl {
    | TypedStatement::Port(name, ..) => name,
    | TypedStatement::Definition(name, ..) => name,
    | TypedStatement::Alias(alias, ..) => &alias.name,
    | TypedStatement::Adt(name, ..) => name,
    | TypedStatement::Infix(name, ..) => name,
  }
}

pub fn declaration_type(decl: &TypedStatement) -> Option<&Type> {
  match decl {
    | TypedStatement::Port(_, ty) => Some(ty),
    | TypedStatement::Definition(_, ty) => Some(&ty.header),
    | TypedStatement::Alias(_) => None,
    | TypedStatement::Adt(_, _) => None,
    | TypedStatement::Infix(_, _, ty) => Some(ty),
  }
}

/// Returns the `Expression`'s `Span`.
pub fn span(expression: &Expression) -> Span {
  *match expression {
    | Expression::Unit(span) => span,
    | Expression::Tuple(span, _) => span,
    | Expression::List(span, _) => span,
    | Expression::If(span, _, _, _) => span,
    | Expression::Lambda(span, _, _) => span,
    | Expression::Application(span, _, _) => span,
    | Expression::OperatorChain(span, _, _) => span,
    | Expression::Literal(span, _) => span,
    | Expression::Ref(span, _) => span,
    | Expression::QualifiedRef(span, _, _) => span,
  }
}

pub fn type_unary_minus() -> Type {
  Type::Function(
    Box::new(Type::Var("number".to_string())),
    Box::new(Type::Var("number".to_string())),
  )
}

pub fn type_bool() -> Type {
  Type::Tag("Bool".to_string(), vec![])
}

pub fn type_list(var: Type) -> Type {
  Type::Tag(String::from("List"), vec![var])
}

pub fn type_func<T>(types: T) -> Type
where
  T: IntoIterator<Item = Type, IntoIter = IntoIter<Type>>,
{
  let mut types_iter = types.into_iter();

  if types_iter.len() == 1 {
    return types_iter.next().unwrap();
  }

  if types_iter.len() == 2 {
    Type::Function(
      Box::from(types_iter.next().unwrap()),
      Box::from(types_iter.next().unwrap()),
    )
  } else {
    Type::Function(
      Box::from(types_iter.next().unwrap()),
      Box::from(type_func(types_iter)),
    )
  }
}

pub fn type_tag_args(var: &str, args: Vec<Type>) -> Type {
  Type::Tag(String::from(var), args)
}
