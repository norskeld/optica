use crate::source::Span;
use super::Expression;

pub fn span(expression: &Expression) -> Span {
  *match expression {
    | Expression::Unit(span) => span,
    | Expression::Tuple(span, _) => span,
    | Expression::List(span, _) => span,
    | Expression::If(span, _, _, _) => span,
    | Expression::Lambda(span, _, _) => span,
    | Expression::Application(span, _, _) => span,
    | Expression::OperatorChain(span, _, _) => span,
    | Expression::Literal(span, _) => span,
    | Expression::Ref(span, _) => span,
    | Expression::QualifiedRef(span, _, _) => span,
  }
}
