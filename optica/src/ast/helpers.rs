use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::IntoIter;

use super::typed::*;
use super::untyped::*;

/// Global atomic function ID.
static FUNC_ID: AtomicUsize = AtomicUsize::new(0);

/// Retrieves and increments the next free function ID.
pub fn function_id() -> FunctionId {
  FUNC_ID.fetch_add(1, Ordering::SeqCst)
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
