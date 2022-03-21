use crate::source::{SourceCode, Span};
use crate::lexer::Token;

#[derive(PartialEq, Debug, Clone)]
pub enum CordError {
  Lexer(SourceCode, LexicalError),
  Parser(SourceCode, ParseError),
  Collection(Vec<CordError>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum LexicalError {
  ReachedEnd { pos: u32 },
  UnableToLex { span: Span },
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParseError {
  Expected {
    span: Span,
    expected: Token,
    found: Token,
  },
  ExpectedInt {
    span: Span,
    found: Token,
  },
  ExpectedIdent {
    span: Span,
    found: Token,
  },
  ExpectedUpperIdent {
    span: Span,
    found: Token,
  },
  ExpectedBinaryOperator {
    span: Span,
    found: Token,
  },
  ExpectedIndentationLevel {
    span: Span,
    expected: u32,
    found: u32,
  },
  ExpectedIndentation {
    span: Span,
    found: Token,
  },
  ExpectedQualifiedName {
    span: Span,
    found: Token,
  },
  UnmatchedToken {
    span: Span,
    found: Token,
    options: Vec<Token>,
  },
}
