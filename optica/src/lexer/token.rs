use std::fmt;

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

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | Token::Ident(value) => {
        write!(f, "{}", value)
      },
      | Token::UpperIdent(value) => {
        write!(f, "{}", value)
      },
      | Token::BinaryOperator(value) => {
        write!(f, "{}", value)
      },
      | Token::LitInt(value) => {
        write!(f, "{}", value)
      },
      | Token::LitFloat(value) => {
        write!(f, "{}", value)
      },
      | Token::LitChar(value) => {
        write!(f, "{}", value)
      },
      | Token::LitString(value) => {
        write!(f, "{}", value)
      },
      | Token::Indent(value) => {
        if *value == 0 {
          write!(f, "<newline>")?;
        } else {
          write!(f, "<indentation {}>", *value)?;
        }
        Ok(())
      },
      | Token::BackSlash => {
        write!(f, "\\")
      },
      | Token::PrefixMinus => {
        write!(f, "-")
      },
      | Token::LetKw => {
        write!(f, "let")
      },
      | Token::IfKw => {
        write!(f, "if")
      },
      | Token::ElseKw => {
        write!(f, "else")
      },
      | Token::ThenKw => {
        write!(f, "then")
      },
      | Token::MatchKw => {
        write!(f, "match")
      },
      | Token::WithKw => {
        write!(f, "with")
      },
      | Token::InKw => {
        write!(f, "in")
      },
      | Token::ModuleKw => {
        write!(f, "module")
      },
      | Token::WhereKw => {
        write!(f, "where")
      },
      | Token::ImportKw => {
        write!(f, "import")
      },
      | Token::AsKw => {
        write!(f, "as")
      },
      | Token::TypeKw => {
        write!(f, "type")
      },
      | Token::DataKw => {
        write!(f, "data")
      },
      | Token::InfixLeftKw => {
        write!(f, "infixl")
      },
      | Token::InfixRightKw => {
        write!(f, "infixr")
      },
      | Token::Underscore => {
        write!(f, "_")
      },
      | Token::Dot => {
        write!(f, ".")
      },
      | Token::DoubleDot => {
        write!(f, "..")
      },
      | Token::Comma => {
        write!(f, ",")
      },
      | Token::LeftParen => {
        write!(f, "(")
      },
      | Token::RightParen => {
        write!(f, ")")
      },
      | Token::LeftBracket => {
        write!(f, "[")
      },
      | Token::RightBracket => {
        write!(f, "]")
      },
      | Token::LeftBrace => {
        write!(f, "{{")
      },
      | Token::RightBrace => {
        write!(f, "}}")
      },
      | Token::Equals => {
        write!(f, "=")
      },
      | Token::Pipe => {
        write!(f, "|")
      },
      | Token::RightArrow => {
        write!(f, "->")
      },
      | Token::LeftArrow => {
        write!(f, "<-")
      },
      | Token::FatRightArrow => {
        write!(f, "=>")
      },
      | Token::Colon => {
        write!(f, ":")
      },
      | Token::Eof => {
        write!(f, "<eof>")
      },
    }
  }
}
