use crate::ast::untyped::*;

pub fn arity(ty: &Type) -> u32 {
  match ty {
    | Type::Function(_, ref rest) => 1 + arity(rest),
    | _ => 0,
  }
}
