use crate::source::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum LexicalError {
  ReachedEnd { pos: u32 },
  UnableToLex { span: Span },
}
