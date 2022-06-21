use crate::ast::{Float, Int};
use crate::source::Span;

#[derive(PartialEq, Debug, Clone)]
pub struct SpannedToken {
  pub span: Span,
  pub token: Token,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  // Terminals.
  BackSlash,
  PrefixMinus,
  Underscore,
  Dot,
  DoubleDot,
  Comma,
  LeftParen,
  RightParen,
  LeftBracket,
  RightBracket,
  LeftBrace,
  RightBrace,
  Equals,
  Pipe,
  RightArrow,
  LeftArrow,
  FatRightArrow,
  Colon,
  Eof,

  // Non-terminals.
  Ident(String),
  UpperIdent(String),
  BinaryOperator(String),
  LitInt(Int),
  LitFloat(Float),
  LitChar(char),
  LitString(String),
  Indent(u32),

  // Keywords.
  LetKw,
  IfKw,
  ElseKw,
  ThenKw,
  ModuleKw,
  WhereKw,
  ImportKw,
  InfixLeftKw,
  InfixRightKw,
  MatchKw,
  WithKw,
  InKw,
  TypeKw,
  DataKw,
  AsKw,
}
