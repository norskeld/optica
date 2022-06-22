use std::vec::IntoIter;

use crate::ast::untyped::Type;

pub fn type_unary_minus() -> Type {
  Type::Function(Box::new(type_number()), Box::new(type_number()))
}

pub fn type_unit() -> Type {
  Type::Unit
}

pub fn type_int() -> Type {
  Type::Tag(String::from("Int"), vec![])
}

pub fn type_float() -> Type {
  Type::Tag(String::from("Float"), vec![])
}

pub fn type_number() -> Type {
  Type::Var(String::from("number"))
}

// pub fn type_number_id(num: u32) -> Type {
//   Type::Var(format!("number{num}"))
// }

pub fn type_bool() -> Type {
  Type::Tag(String::from("Bool"), vec![])
}

pub fn type_string() -> Type {
  Type::Tag(String::from("String"), vec![])
}

pub fn type_char() -> Type {
  Type::Tag(String::from("Char"), vec![])
}

pub fn type_list(var: Type) -> Type {
  Type::Tag(String::from("List"), vec![var])
}

pub fn type_var(var: &str) -> Type {
  Type::Var(String::from(var))
}

pub fn type_tag(var: &str) -> Type {
  Type::Tag(String::from(var), vec![])
}

pub fn type_tag_parameterized(var: &str, params: Vec<Type>) -> Type {
  Type::Tag(String::from(var), params)
}

pub fn type_tuple<T>(values: T) -> Type
where
  T: IntoIterator<Item = Type, IntoIter = IntoIter<Type>>,
{
  Type::Tuple(values.into_iter().collect())
}

pub fn type_function<T>(types: T) -> Type
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
      Box::from(type_function(types_iter)),
    )
  }
}
