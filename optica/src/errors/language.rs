use std::fmt;

use ariadne::{Config, Source};

use super::*;
use crate::source::SourceCode;

#[derive(Clone, Debug, PartialEq)]
pub enum LangError {
  Lexer(SourceCode, LexicalError),
  Parser(SourceCode, ParseError),
  Typechecker(SourceCode, TypeError),
  Interpreter(InterpreterError),
  Loader(LoaderError),
  List(Vec<LangError>),
}

impl fmt::Display for LangError {
  fn fmt(&self, writer: &mut fmt::Formatter) -> fmt::Result {
    let buffer = match self {
      | LangError::Lexer(source, err) => report_to_string(err, source)?,
      | LangError::Parser(source, err) => report_to_string(err, source)?,
      | LangError::Typechecker(source, err) => report_to_string(err, source)?,
      | LangError::Interpreter(err) => format!("{err:?}"),
      | LangError::Loader(err) => format!("{err:?}"),
      | LangError::List(err) => format!("{err:?}"),
    };

    write!(writer, "{}", buffer)
  }
}

fn report_to_string<'a>(
  report: &'a impl Reportable<'a>,
  source: &'a SourceCode,
) -> Result<String, fmt::Error> {
  let config = Config::default()
    .with_cross_gap(false)
    .with_underlines(true)
    .with_tab_width(4);

  let mut buffer = vec![];

  report
    .report(source)
    .with_config(config)
    .finish()
    .write(
      (source.file_name(), Source::from(source.as_str())),
      &mut buffer,
    )
    .map_err(|_| fmt::Error)?;

  Ok(String::from_utf8_lossy(&buffer).to_string())
}
