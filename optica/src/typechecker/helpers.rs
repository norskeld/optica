use crate::ast::untyped::*;

pub fn build_func_type(types: &[Type]) -> Type {
  assert!(!types.is_empty());

  if types.len() == 1 {
    return types[0].clone();
  }

  if types.len() == 2 {
    Type::Function(Box::from(types[0].clone()), Box::from(types[1].clone()))
  } else {
    Type::Function(
      Box::from(types[0].clone()),
      Box::from(build_func_type(&types[1..])),
    )
  }
}
