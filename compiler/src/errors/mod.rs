use crate::source::{SourceCode, Span};

#[derive(PartialEq, Debug, Clone)]
pub enum CordError {
  Lexer(SourceCode, LexicalError),
  Parser(SourceCode),
  Collection(Vec<CordError>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum LexicalError {
  ReachedEnd { pos: u32 },
  UnableToLex { span: Span },
}
