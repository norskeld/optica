use crate::source::SourceCode;
use super::*;

#[derive(PartialEq, Debug, Clone)]
pub enum LangError {
  Lexer(SourceCode, LexicalError),
  Parser(SourceCode, ParseError),
  Typechecker(SourceCode, TypeError),
  Interpreter(InterpreterError),
  Loader(LoaderError),
  List(Vec<LangError>),
}
