use ariadne::{Label, Report, ReportKind};

use super::*;
use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::source::{SourceCode, Span};

#[derive(PartialEq, Debug, Clone)]
pub enum TypeError {
  // PatternMatchingError {
  //   span: Span,
  //   info: PatternMatchingError,
  // },
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
  IfBranchesMismatch {
    span: Span,
    true_branch: TypedExpression,
    false_branch: TypedExpression,
  },
  ArgumentsMismatch {
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
  CaseBranchReturnTypeMismatch {
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
    span: Span,
    name: String,
    values: Vec<String>,
  },
  UnusedTypeVariables {
    span: Span,
    name: String,
    values: Vec<String>,
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

impl<'a> Reportable<'a> for TypeError {
  fn report(&'a self, source: &'a SourceCode) -> ReportBuilder<'a> {
    let source = source.file_name();

    match self {
      | TypeError::MissingDefinition { span, name } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("couldn't find `{name}`"))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::ListNotHomogeneous {
        span,
        list_type,
        item_type,
        index,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "expected list of `{list_type}`, but found item {index} with type `{item_type}`"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::IfWithNonBoolCondition { span, expr } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "if condition must be of `Bool` type, but found `{expr_type}` instead",
            expr_type = expr.get_type()
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::IfBranchesMismatch {
        span,
        true_branch,
        false_branch,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("if branches' types do not match")
          .with_label(Label::new((source, span!(span))))
          .with_label(Label::new((source, span!(true_branch.get_span()))))
          .with_label(Label::new((source, span!(false_branch.get_span()))))
      },
      | TypeError::ArgumentsMismatch {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("tried to call a function with incorrect argument types")
          .with_label(Label::new((source, span!(span))))
          .with_note(format!("expected `{expected}`, but found `{found}`"))
      },
      | TypeError::NotAFunction { span, function, .. } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "attempted to call value of type `{function}`, which is not a function"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::InvalidOperandChain { span, message } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(message)
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::CaseBranchReturnTypeMismatch {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("branch type doesn't match the expression type")
          .with_label(Label::new((source, span!(span))))
          .with_note(format!("expected `{expected}`, but found `{found}`"))
      },
      | TypeError::DefinitionTypeAndReturnTypeMismatch {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("type annotation and expresion type do not match")
          .with_label(Label::new((source, span!(span))))
          .with_note(format!("expected `{expected}`, but found `{found}`"))
      },
      | TypeError::VariableNameShadowed { span, name } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("found shadowed variable '{name}'"))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::UndeclaredTypeVariables { span, values, .. } => {
        let variables = values
          .iter()
          .map(|value| format!("'{value}'"))
          .intersperse(", ".to_string())
          .collect::<String>();

        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("use of undeclared type variables: {variables}"))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::UnusedTypeVariables { span, values, .. } => {
        let variables = values
          .iter()
          .map(|value| format!("'{value}'"))
          .intersperse(", ".to_string())
          .collect::<String>();

        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("unused type variables: {variables}"))
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::TypeMatchingError {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("types do not match")
          .with_label(
            Label::new((source, span!(span)))
              .with_message(format!("expected `{expected}`, but found `{found}`")),
          )
      },
      | TypeError::RecursiveTypeDefinition { span, .. } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("found recursive type definition")
          .with_label(Label::new((source, span!(span))))
      },
      | TypeError::UnknownType { span, name } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("found unknown type `{name}`"))
          .with_label(Label::new((source, span!(span))))
      },
    }
  }
}

// #[derive(Clone, Debug, PartialEq)]
// pub enum PatternMatchingError {
//   ListPatternsAreNotHomogeneous(Type, Type),
//   UnknownOperatorPattern(String),
//   UnknownAdtVariant(String),
//   ExpectedListType(Type),
//   ExpectedUnit(Type),
//   ExpectedTuple(Pattern, Type),
//   ExpectedRecord(Type),
//   ExpectedAdt(String, Type),
//   PatternNotExhaustive(Pattern),
//   ExpectedLiteral(String, Type),
// }
