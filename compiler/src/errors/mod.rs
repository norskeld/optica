pub use interpreter::*;
pub use language::*;
pub use lexer::*;
pub use loader::*;
pub use parser::*;
pub use typechecker::*;

mod interpreter;
mod language;
mod lexer;
mod loader;
mod parser;
mod typechecker;

/// This trait allows for wrapping a given error in a more generic error.
pub trait Wrappable {
  type Wrapper;

  fn wrap(self) -> Self::Wrapper;
}
