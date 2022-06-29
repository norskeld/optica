use ariadne::{Label, Report, ReportKind};

use super::*;
use crate::source::{SourceCode, Span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LexicalError {
  ReachedEnd { pos: u32 },
  UnableToLex { span: Span },
}

impl<'a> Reportable<'a> for LexicalError {
  fn report(&'a self, source: &'a SourceCode) -> ReportBuilder<'a> {
    let source = source.file_name();

    match self {
      | LexicalError::ReachedEnd { pos } => {
        Report::build(ReportKind::Error, source, *pos as usize)
          .with_message("reached the end while trying to complete reading a token")
          .with_label(Label::new((source, span!((*pos, *pos + 1)))))
      },
      | LexicalError::UnableToLex { span } => {
        Report::build(ReportKind::Error, source, span.0 as usize)
          .with_message("unknown character sequence")
          .with_label(Label::new((source, span!(span))))
      },
    }
  }
}
