use super::combinators;
use super::expression;
use super::module;
use super::pattern;
use super::statement;
use super::types;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::lexer::Lexer;
use crate::source::{Input, SourceCode};

pub struct Parser {
  code: SourceCode,
  lexer: Lexer,
}

/// The `clone`s below are cheap since `SourceCode` is wrapped into an `Arc`.
impl Parser {
  pub fn new(lexer: Lexer) -> Self {
    Parser {
      code: lexer.source(),
      lexer,
    }
  }

  pub fn parse_expression(&mut self) -> Result<Expression, LangError> {
    combinators::complete(&expression::parse_expr, self.fresh_input()?)
      .map_err(|error| LangError::Parser(self.code.clone(), error))
  }

  pub fn parse_statement(&mut self) -> Result<Statement, LangError> {
    combinators::complete(&statement::parse_statement, self.fresh_input()?)
      .map_err(|error| LangError::Parser(self.code.clone(), error))
  }

  pub fn parse_module(&mut self) -> Result<Module, LangError> {
    combinators::complete(&module::parse_module, self.fresh_input()?)
      .map_err(|error| LangError::Parser(self.code.clone(), error))
  }

  #[allow(dead_code)]
  pub fn parse_type(&mut self) -> Result<Type, LangError> {
    combinators::complete(&types::parse_type, self.fresh_input()?)
      .map_err(|error| LangError::Parser(self.code.clone(), error))
  }

  #[allow(dead_code)]
  pub fn parse_pattern(&mut self) -> Result<Pattern, LangError> {
    combinators::complete(&pattern::parse_pattern, self.fresh_input()?)
      .map_err(|error| LangError::Parser(self.code.clone(), error))
  }

  fn fresh_input(&mut self) -> Result<Input, LangError> {
    let input = Input::new(self.code.clone(), self.lexer.lex()?);

    Ok(input)
  }
}
