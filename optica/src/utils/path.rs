use std::fmt::Display;

/// Builds a path to a module from path components.
pub fn qualified_name<S: Into<String> + Display>(path: &[S], name: &str) -> String
where
  S: Into<String> + Display,
{
  path
    .iter()
    .fold(String::new(), |acc, segment| format!("{acc}{segment}."))
    + name
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_qualified_name() {
    assert_eq!(
      qualified_name(&["Intrinsic", "List"], "map"),
      "Intrinsic.List.map"
    );

    assert_eq!(
      qualified_name(&["Intrinsic"], "composeL"),
      "Intrinsic.composeL"
    );

    assert_eq!(
      qualified_name::<String>(&[], "__unary_minus"),
      "__unary_minus"
    );
  }
}
