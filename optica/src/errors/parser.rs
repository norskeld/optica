use ariadne::{Label, Report, ReportKind};
use indoc::formatdoc;

use super::*;
use crate::lexer::Token;
use crate::source::{SourceCode, Span};

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

impl<'a> Reportable<'a> for ParseError {
  fn report(&'a self, source: &'a SourceCode) -> ReportBuilder<'a> {
    let source = source.file_name();

    match self {
      | ParseError::Expected {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "expected '{expected}', but instead found '{found}'"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedInt { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("expected integer, but found '{found}'"))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedIdent { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("expected identifier, but found '{found}'"))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedUpperIdent { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "expected capitalized identifier, but found '{found}'"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedBinaryOperator { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("expected binary operator, but found '{found}'"))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedIndentationLevel {
        span,
        expected,
        found,
      } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "expected indentation of {expected}, but found {found}"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedIndentation { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("expected indentation, but found '{found}'"))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::ExpectedQualifiedName { span, found } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!(
            "expected qualified identifier, but found '{found}'"
          ))
          .with_label(Label::new((source, span!(span))))
      },
      | ParseError::UnmatchedToken {
        span,
        found,
        options,
      } => {
        let error = Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message(format!("found unexpected token '{found}'"))
          .with_label(Label::new((source, span!(span))));

        if options.is_empty() {
          error
        } else {
          let options = options
            .iter()
            .map(|token| token.to_string())
            .intersperse(", ".to_string())
            .collect::<String>();

          error.with_note(formatdoc! {"
              possible options: {options}
              or use (..) to expose all definitions and types
            "})
        }
      },
    }
  }
}
