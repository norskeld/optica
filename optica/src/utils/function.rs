use crate::ast::untyped::*;

pub fn arguments_count(ty: &Type) -> u32 {
  match ty {
    | Type::Function(_, ref rest) => 1 + arguments_count(rest),
    | _ => 0,
  }
}
