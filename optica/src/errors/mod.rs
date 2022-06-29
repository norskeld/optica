use std::ops::Range;

pub use interpreter::*;
pub use language::*;
pub use lexer::*;
pub use loader::*;
pub use parser::*;
pub use typechecker::*;

use crate::source::SourceCode;

#[macro_use]
mod helpers;
mod interpreter;
mod language;
mod lexer;
mod loader;
mod parser;
mod typechecker;

pub type ReportBuilder<'a> = ariadne::ReportBuilder<(&'a str, Range<usize>)>;

pub trait Reportable<'a> {
  fn report(&'a self, source: &'a SourceCode) -> ReportBuilder<'a>;
}

/// This trait allows for wrapping a given error in a more generic error.
pub trait Wrappable {
  type Wrapper;

  fn wrap(self) -> Self::Wrapper;
}
