use crate::ast::untyped::{Expression, Pattern, Type};
use crate::ast::typed::TypedExpression;
use crate::source::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum TypeError {
  PatternMatchingError {
    span: Span,
    info: PatternMatchingError,
  },
  MissingDefinition {
    span: Span,
    name: String,
  },
  ListNotHomogeneous {
    span: Span,
    list_type: Type,
    item_type: Type,
    index: u32,
  },
  IfWithNonBoolCondition {
    span: Span,
    expr: TypedExpression,
  },
  IfBranchesDoesntMatch {
    span: Span,
    true_branch: TypedExpression,
    false_branch: TypedExpression,
  },
  ArgumentsDoNotMatch {
    span: Span,
    expected: Type,
    found: Type,
  },
  NotAFunction {
    span: Span,
    function: Type,
    input: Expression,
    output: Expression,
  },
  InvalidOperandChain {
    span: Span,
    message: String,
  },
  RecordUpdateOnNonRecord {
    span: Span,
    expr: TypedExpression,
  },
  RecordUpdateUnknownField {
    span: Span,
    field: String,
    record_name: String,
    record: TypedExpression,
  },
  CaseBranchDontMatchReturnType {
    span: Span,
    expected: Type,
    found: Type,
  },
  DefinitionTypeAndReturnTypeMismatch {
    span: Span,
    expected: Type,
    found: Type,
  },
  VariableNameShadowed {
    span: Span,
    name: String,
  },
  UndeclaredTypeVariables {
    name: String,
    values: Vec<String>,
  },
  UnusedTypeVariables {
    name: String,
    values: Vec<String>,
  },
  InvalidFunctionPatternAmount {
    expected: usize,
    found: usize,
  },
  CyclicStatementDependency {
    cycle: Vec<String>,
  },
  ExpectingRecordWithName {
    record: TypedExpression,
    name: String,
  },
  TypeMatchingError {
    span: Span,
    expected: Type,
    found: Type,
  },
  RecursiveTypeDefinition {
    span: Span,
    var: String,
    ty: Type,
  },
  UnknownType {
    span: Span,
    name: String,
  },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatternMatchingError {
  ListPatternsAreNotHomogeneous(Type, Type),
  UnknownOperatorPattern(String),
  UnknownAdtVariant(String),
  ExpectedListType(Type),
  ExpectedUnit(Type),
  ExpectedTuple(Pattern, Type),
  ExpectedRecord(Type),
  ExpectedAdt(String, Type),
  PatternNotExhaustive(Pattern),
  InvalidRecordEntryName(String),
  ExpectedLiteral(String, Type),
}
