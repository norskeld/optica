use std::sync::Arc;

use super::*;
use crate::ast::typed::*;

#[derive(Clone, Debug, PartialEq)]
pub enum InterpreterError {
  MissingModule(Vec<String>),
  MissingDefinition(String),
  MissingExport(String, Vec<TypedStatement>),
  InvalidIfCondition(Value),
  CaseExpressionNonExhaustive(Value, Vec<TypedPattern>),
  FunctionArityMismatch(u32, u32, Arc<Function>),
  ExpectedFunction(Value),
  ExpectedAdt(Value),
  ExpectedTuple(Value),
  ExpectedList(Value),
  ExpectedFloat(Value),
  ExpectedInt(Value),
  ExpectedChar(Value),
  ExpectedString(Value),
  ExpectedBoolean(Value),
  ExpectedNumber(Value),
  ExpectedNonEmptyList(Value),
  UnknownOperatorPattern(String),
  IntrinsicAdtError(Value),
  IntrinsicError,
  CyclicModuleDependency(Vec<Vec<String>>),
}

impl Wrappable for InterpreterError {
  type Wrapper = LangError;

  fn wrap(self) -> LangError {
    LangError::Interpreter(self)
  }
}
