use std::sync::Arc;

use crate::ast::typed::{Declaration, Function, TypedExpression, TypedPattern, Value};
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum InterpreterError {
  MissingModule(Vec<String>),
  MissingDefinition(String),
  IncorrectDefType(TypeError),
  RecordUpdateOnNonRecord(TypedExpression, Value),
  InvalidIfCondition(Value),
  // InvalidExpressionChain(ExprTreeError),
  RecordFieldNotFound(String, Value),
  CaseExpressionNonExhaustive(Value, Vec<TypedPattern>),
  FunArgumentSizeMismatch(u32, u32, Arc<Function>),
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
  InternalErrorRecordAccess(Value),
  InternalErrorAdtCreation(Value),
  UnknownBuiltinFunction(u32),
  BuiltinFunctionError,
  ImpossibleConversion,
  MissingSourceFile,
  CyclicModuleDependency(Vec<Vec<String>>),
  MissingExposing(String, Vec<Declaration>),
  FunctionTodo(String),
  InternalError,
}

impl Wrappable for InterpreterError {
  type Wrapper = LangError;

  fn wrap(self) -> LangError {
    LangError::Interpreter(self)
  }
}
