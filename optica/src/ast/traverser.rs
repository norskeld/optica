use super::untyped::Type;

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
    | Type::Function(a, b) => {
      traverse_type(state, a, traverser);
      traverse_type(state, b, traverser);
    },
    | Type::Tuple(params) => {
      params
        .iter()
        .for_each(|param| traverse_type(state, param, traverser));
    },
  }
}
