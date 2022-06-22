use crate::ast::untyped::*;

pub fn traverse_type<S, F>(state: &mut S, root: &Type, traverser: &F)
where
  F: Fn(&mut S, &Type),
{
  traverser(state, root);

  match root {
    | Type::Unit => {},
    | Type::Var(_) => {},
    | Type::Tag(_, params) => {
      params
        .iter()
        .for_each(|param| traverse_type(state, param, traverser));
    },
    | Type::Function(param, rest) => {
      traverse_type(state, param, traverser);
      traverse_type(state, rest, traverser);
    },
    | Type::Tuple(params) => {
      params
        .iter()
        .for_each(|param| traverse_type(state, param, traverser));
    },
  }
}
