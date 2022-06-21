pub trait VecExt<A> {
  fn for_each<F: FnMut(&A)>(&self, f: F);
  fn map<B, F: FnMut(&A) -> B>(&self, f: F) -> Vec<B>;
  fn join_vec(&self, other: &[A]) -> Vec<A>;
}

impl<A: Clone> VecExt<A> for Vec<A> {
  fn for_each<F: FnMut(&A)>(&self, f: F) {
    self.iter().for_each(f);
  }

  fn map<B, F: FnMut(&A) -> B>(&self, f: F) -> Vec<B> {
    self.iter().map(f).collect()
  }

  fn join_vec(&self, other: &[A]) -> Vec<A> {
    let mut result: Vec<A> = Vec::new();

    for item in self {
      result.push(item.clone());
    }

    for item in other {
      result.push(item.clone());
    }

    result
  }
}

pub fn cons<T>(first: T, rest: Vec<T>) -> Vec<T> {
  let mut vec: Vec<T> = vec![first];
  vec.extend(rest);

  vec
}

pub fn rcons<T: Clone>(init: &[T], last: T) -> Vec<T> {
  let mut vec: Vec<T> = init.to_vec();
  vec.push(last);

  vec
}
