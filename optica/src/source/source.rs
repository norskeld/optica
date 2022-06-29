use std::path::Path;
use std::str::Chars;
use std::sync::Arc;

/// Padding to detect the end of code while tokenizing.
pub const PADDING: usize = 2;

/// Span element `(start, end)`.
pub type Span = (u32, u32);

/// Source code container to avoid large files duplication.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceCode(pub Arc<SourceContainer>);

/// Source file container.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceFile {
  pub name: String,
  pub path: String,
  pub source: SourceCode,
}

/// Internal source code container, used for ergonomics.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceContainer {
  /// File path or 'inline'.
  pub source: String,
  /// Source code.
  pub code: String,
}

impl SourceCode {
  /// Creates a [SourceContainer] instance wrapping a string.
  pub fn from_string(code: String, path: &str) -> Self {
    let mut code = code;

    // This helps to avoid checks for the end of code before reading every character.
    for _ in 0..PADDING {
      code.push('\0');
    }

    SourceCode(Arc::new(SourceContainer {
      code,
      source: path.to_string(),
    }))
  }

  /// Creates a [SourceContainer] instance *cloning* a string slice.
  #[allow(clippy::should_implement_trait)]
  pub fn from_str(code: &str) -> Self {
    Self::from_string(code.to_string(), "inline")
  }

  /// Creates a [SourceContainer] instance from an vector of bytes.
  ///
  /// NOTE: It uses `String::from_utf8_lossy`.
  pub fn from_bytes(bytes: Vec<u8>, path: &str) -> Self {
    Self::from_string(String::from_utf8_lossy(&bytes).to_string(), path)
  }

  /// Creates a [SourceContainer] instance from an slice of bytes.
  ///
  /// NOTE: It uses `String::from_utf8_lossy`.
  pub fn from_slice(bytes: &[u8]) -> Self {
    Self::from_string(String::from_utf8_lossy(bytes).to_string(), "inline")
  }

  /// Returns the real size of the `code`.
  pub fn len(&self) -> usize {
    self.0.code.len() - PADDING
  }

  /// Checks if the source code is empty.
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Returns an iterator over the characters of the `code`.
  pub fn chars(&self) -> Chars {
    self.0.code.chars()
  }

  /// Returns a bytes slice of the `code`.
  pub fn as_bytes(&self) -> &[u8] {
    self.0.code.as_bytes()
  }

  /// Returns a string slice of the `code`.
  pub fn as_str(&self) -> &str {
    self.0.code.as_str()
  }

  /// Returns the file name of source.
  pub fn file_name(&self) -> &str {
    let path = self.0.source.as_str();

    Path::new(path)
      .file_name()
      .and_then(|filename| filename.to_str())
      .unwrap_or(path)
  }

  /// Returns the file path of source.
  pub fn file_path(&self) -> &str {
    self.0.source.as_str()
  }
}
