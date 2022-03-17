use crate::ast::{Expression, Module, Pattern, Statement, Type};
use crate::errors::CordError;
use crate::lexer::Lexer;
use crate::source::{Input, SourceCode};
use super::combinators;
use super::expression;
use super::pattern;

pub struct Parser {
  code: SourceCode,
  lexer: Lexer,
}

impl Parser {
  pub fn new(lexer: Lexer) -> Self {
    Parser {
      code: lexer.source(),
      lexer,
    }
  }

  pub fn parse_expression(&mut self) -> Result<Expression, CordError> {
    let input = Input::new(self.code.clone(), self.lexer.lex()?);
    combinators::complete(&expression::parse_expr, input)
      .map_err(|error| CordError::Parser(self.code.clone(), error))
  }

  pub fn parse_statement(&mut self) -> Result<Statement, CordError> {
    todo!()
  }

  pub fn parse_module(&mut self) -> Result<Module, CordError> {
    todo!()
  }

  pub fn parse_type(&mut self) -> Result<Type, CordError> {
    todo!()
  }

  pub fn parse_pattern(&mut self) -> Result<Pattern, CordError> {
    let input = Input::new(self.code.clone(), self.lexer.lex()?);
    combinators::complete(&pattern::parse_pattern, input)
      .map_err(|error| CordError::Parser(self.code.clone(), error))
  }
}
