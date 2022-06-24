use std::fmt::Display;
use std::path::Path;

use crate::errors::*;
use crate::loader::EXTENSION;

/// Tries to normalize a path and returns a tuple of the original path and the module name.
pub fn normalize_path(path: &str) -> Result<(String, String), LangError> {
  let path = Path::new(path);

  let file_name = path
    .file_name()
    .and_then(|os_str| os_str.to_str())
    .ok_or_else(|| {
      LoaderError::PathNormalization("Couldn't extract the file name.".to_string()).wrap()
    })?;

  // Capitalize the file name.
  let (file_name, module_name) = {
    let mut chars = file_name.chars();

    let file_name = match chars.next() {
      | Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
      | None => String::new(),
    };

    let module_name = match &file_name.strip_suffix(EXTENSION) {
      | Some(module_name) => module_name.to_string(),
      | None => file_name.clone(),
    };

    (file_name, module_name)
  };

  // Re-construct path.
  let file_path = path.with_file_name(&file_name);

  Ok((file_path.to_string_lossy().to_string(), module_name))
}

/// Builds a path to a module from path components.
pub fn qualified_name<S>(path: &[S], name: &str) -> String
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
