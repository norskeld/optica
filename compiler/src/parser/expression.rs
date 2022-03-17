use crate::ast::{self, Expression, Literal};
use crate::errors::ParseError;
use crate::lexer::Token;
use crate::source::Input;
use crate::utils;
use super::combinators;
use super::pattern;

pub fn parse_expr(input: Input) -> Result<(Expression, Input), ParseError> {
  let (first, input) = parse_expr_application(input)?;
  let (rest, input) = combinators::many0(&binary_operator_expr, input)?;

  Ok((create_binary_operator_chain(first, rest), input))
}

fn parse_expr_application(input: Input) -> Result<(Expression, Input), ParseError> {
  let (exprs, input) = combinators::many1(&parse_expr_base, input)?;
  let start = input.pos();
  let end = input.pos_end();

  let mut iter = exprs.into_iter();
  let first = iter.next().unwrap();

  let tree = iter.fold(first, |acc, b| {
    Expression::Application((start, end), Box::new(acc), Box::new(b))
  });

  Ok((tree, input))
}

fn parse_expr_base(input: Input) -> Result<(Expression, Input), ParseError> {
  let (expr, input) = match input.read() {
    | Token::LitInt(value) => (
      Expression::Literal(input.span(), Literal::Int(value)),
      input.next(),
    ),
    | Token::LitFloat(value) => (
      Expression::Literal(input.span(), Literal::Float(value)),
      input.next(),
    ),
    | Token::LitChar(value) => (
      Expression::Literal(input.span(), Literal::Char(value)),
      input.next(),
    ),
    | Token::LitString(value) => (
      Expression::Literal(input.span(), Literal::String(value)),
      input.next(),
    ),
    | Token::Ident(name) => (Expression::Ref(input.span(), name), input.next()),
    | Token::UpperIdent(first) => {
      let next_input = input.next();

      match next_input.read() {
        | Token::Dot => {
          // Parsed: Upper.
          let (rest, input) = combinators::many0(&parse_dot_name, next_input)?;

          // Parsed: Upper.A.B.C
          let input = combinators::expect(Token::Dot, input)?;
          let (name, input) = combinators::expect_ident(input)?;

          // Parsed: Upper.A.B.C.func
          (
            Expression::QualifiedRef(
              (input.pos(), input.pos()),
              utils::vec::create_vec(first, rest),
              name,
            ),
            input,
          )
        },
        | _ => (
          Expression::Ref((input.pos(), next_input.pos()), first),
          input.next(),
        ),
      }
    },
    | Token::IfKw => {
      let (condition, input) = parse_expr(input.next())?;

      let input = combinators::expect(Token::ThenKw, input)?;
      let (true_branch, input) = parse_expr(input)?;

      let input = combinators::expect(Token::ElseKw, input)?;
      let (false_branch, input) = parse_expr(input)?;

      (
        Expression::If(
          (input.pos(), input.pos()),
          Box::from(condition),
          Box::from(true_branch),
          Box::from(false_branch),
        ),
        input,
      )
    },
    // Variants:
    // () => Unit
    // (+) => Ref
    // (1) => Literal
    // (1, 2) => Tuple
    | Token::LeftParen => {
      let input = input.next();

      match input.read() {
        // ()
        | Token::RightParen => {
          let input = input.next();
          (Expression::Unit((input.pos(), input.pos())), input)
        },
        // (+)
        | Token::BinaryOperator(op) => {
          let input = combinators::expect(Token::RightParen, input.next())?;
          (Expression::Ref((input.pos(), input.pos()), op), input)
        },
        | _ => {
          let (value, input) = parse_expr(input)?;

          match input.read() {
            // (1)
            | Token::RightParen => (value, input.next()),

            // (1, 2)
            | _ => {
              let input = combinators::expect(Token::Comma, input)?;
              let (rest, input) = combinators::comma1(&parse_expr, input)?;
              let input = combinators::expect(Token::RightParen, input)?;

              (
                Expression::Tuple(
                  (input.pos(), input.pos()),
                  utils::vec::create_vec(value, rest),
                ),
                input,
              )
            },
          }
        },
      }
    },
    | Token::LeftBracket => {
      let input = input.next();

      let (values, input) = combinators::comma0(&parse_expr, input)?;
      let input = combinators::expect(Token::RightBracket, input)?;

      (Expression::List((input.pos(), input.pos()), values), input)
    },
    | Token::BackSlash => {
      let (patterns, input) = combinators::many1(&pattern::parse_pattern, input.next())?;

      let input = combinators::expect(Token::RightArrow, input)?;
      let (expr, input) = parse_expr(input)?;

      (
        Expression::Lambda((input.pos(), input.pos()), patterns, Box::from(expr)),
        input,
      )
    },
    | Token::PrefixMinus => {
      let (expr, input) = parse_expr_base(input.next())?;

      (
        Expression::Application(
          (input.pos(), input.pos()),
          Box::from(Expression::Ref(
            (input.pos(), input.pos() + 1),
            String::from("__internal__minus"),
          )),
          Box::from(expr),
        ),
        input,
      )
    },
    | _ => {
      return Err(ParseError::UnmatchedToken {
        span: input.span(),
        found: input.read(),
        options: vec![],
      });
    },
  };

  Ok((expr, input))
}

fn parse_dot_name(input: Input) -> Result<(String, Input), ParseError> {
  let input = combinators::expect(Token::Dot, input)?;
  let (name, input) = combinators::expect_upper_ident(input)?;

  Ok((name, input))
}

fn binary_operator_expr(input: Input) -> Result<((String, Expression), Input), ParseError> {
  let (op, input) = combinators::expect_binary_operator(input)?;
  let (expr, input) = parse_expr_application(input)?;

  Ok(((op, expr), input))
}

fn create_binary_operator_chain(first: Expression, rest: Vec<(String, Expression)>) -> Expression {
  if rest.is_empty() {
    return first;
  }

  let mut exprs = Vec::new();
  let mut ops = Vec::new();

  exprs.push(first);

  for (op, expr) in rest {
    ops.push(op);
    exprs.push(expr);
  }

  let ((first, _), (_, last)) = (
    ast::span(exprs.first().unwrap()),
    ast::span(exprs.last().unwrap()),
  );

  Expression::OperatorChain((first, last), exprs, ops)
}

#[cfg(test)]
mod tests {
  use indoc::indoc;

  use crate::ast::Pattern;
  use crate::parser::testing;
  use super::*;

  #[test]
  fn test_exprs() {
    testing::is_ok(parse_expr, "42");
    testing::is_ok(parse_expr, "42.42");
    testing::is_ok(parse_expr, "'a'");
    testing::is_ok(parse_expr, r#""Hello World""#);
    testing::is_ok(parse_expr, "if 1 then 2 else 3");
    testing::is_ok(parse_expr, "()");
    testing::is_ok(parse_expr, "(1)");
    testing::is_ok(parse_expr, "(1, 2)");
    testing::is_ok(parse_expr, "[]");
    testing::is_ok(parse_expr, "[1]");
    testing::is_ok(parse_expr, "[1, 2]");
    testing::is_ok(parse_expr, "a");
    testing::is_ok(parse_expr, "variableName0");
    testing::is_ok(parse_expr, "True");
    testing::is_ok(parse_expr, "False");
    testing::is_ok(parse_expr, "List");
    testing::is_ok(parse_expr, "List.map");
    testing::is_ok(parse_expr, "List.A.B.C.a");
    testing::is_ok(parse_expr, r"\x -> 42");
    testing::is_ok(parse_expr, r"\x y z -> [x,y,z]");
    testing::is_ok(parse_expr, "sum 1 2");
    testing::is_ok(parse_expr, "1+2");
    testing::is_ok(parse_expr, "-42");
    testing::is_ok(parse_expr, "-(1+2)");
  }

  #[test]
  fn test_unit() {
    testing::assert_eq(parse_expr, "()", Expression::Unit((0, 0)));
  }

  #[test]
  fn test_parens() {
    testing::assert_eq(parse_expr, "(a)", Expression::Ref((0, 0), "a".to_string()));
  }

  #[test]
  fn test_tuple() {
    testing::assert_eq(
      parse_expr,
      "(a, b)",
      Expression::Tuple(
        (0, 0),
        vec![
          Expression::Ref((0, 0), "a".to_string()),
          Expression::Ref((0, 0), "b".to_string()),
        ],
      ),
    );
  }

  #[test]
  fn test_list() {
    testing::assert_eq(
      parse_expr,
      "[a, b]",
      Expression::List(
        (0, 0),
        vec![
          Expression::Ref((0, 0), "a".to_string()),
          Expression::Ref((0, 0), "b".to_string()),
        ],
      ),
    );
  }

  #[test]
  fn test_empty_list() {
    testing::assert_eq(parse_expr, "[]", Expression::List((0, 0), vec![]));
  }

  #[test]
  fn test_if() {
    testing::assert_eq(
      parse_expr,
      "if a then b else c",
      Expression::If(
        (0, 0),
        Box::new(Expression::Ref((0, 0), "a".to_string())),
        Box::new(Expression::Ref((0, 0), "b".to_string())),
        Box::new(Expression::Ref((0, 0), "c".to_string())),
      ),
    );
  }

  #[test]
  fn test_lambda() {
    testing::assert_eq(
      parse_expr,
      r"\x -> x",
      Expression::Lambda(
        (0, 7),
        vec![Pattern::Var((1, 2), "x".to_string())],
        Box::new(Expression::Ref((6, 7), "x".to_string())),
      ),
    );
  }

  #[test]
  fn test_binop_chain() {
    testing::assert_eq(
      parse_expr,
      "1 + 2 + 3 + 4",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(1)),
          Expression::Literal((0, 0), Literal::Int(2)),
          Expression::Literal((0, 0), Literal::Int(3)),
          Expression::Literal((0, 0), Literal::Int(4)),
        ],
        vec!["+".to_string(), "+".to_string(), "+".to_string()],
      ),
    );
  }

  #[test]
  fn test_binop_chain_multiline() {
    let code = indoc! {"
      1 +
        2 +
        3 +
        4
    "};

    testing::assert_eq(
      parse_expr,
      code,
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(1)),
          Expression::Literal((0, 0), Literal::Int(2)),
          Expression::Literal((0, 0), Literal::Int(3)),
          Expression::Literal((0, 0), Literal::Int(4)),
        ],
        vec!["+".to_string(), "+".to_string(), "+".to_string()],
      ),
    );
  }

  #[test]
  fn test_operator_precedence() {
    testing::assert_eq(
      parse_expr,
      "1 * 2 + 3 * 4",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(1)),
          Expression::Literal((0, 0), Literal::Int(2)),
          Expression::Literal((0, 0), Literal::Int(3)),
          Expression::Literal((0, 0), Literal::Int(4)),
        ],
        vec!["*".to_string(), "+".to_string(), "*".to_string()],
      ),
    );
  }

  #[test]
  fn test_qualified_ref() {
    testing::assert_eq(
      parse_expr,
      "List.map",
      Expression::QualifiedRef((0, 0), vec!["List".to_string()], "map".to_string()),
    );
  }

  #[test]
  fn test_function_application() {
    testing::assert_eq(
      parse_expr,
      "fun 42",
      Expression::Application(
        (0, 0),
        Box::new(Expression::Ref((0, 0), "fun".to_string())),
        Box::new(Expression::Literal((0, 0), Literal::Int(42))),
      ),
    );
  }

  #[test]
  fn test_function_application_multiple_args() {
    testing::assert_eq(
      parse_expr,
      "fun 40 2",
      Expression::Application(
        (0, 0),
        Box::new(Expression::Application(
          (0, 0),
          Box::new(Expression::Ref((0, 0), "fun".to_string())),
          Box::new(Expression::Literal((0, 0), Literal::Int(40))),
        )),
        Box::new(Expression::Literal((0, 0), Literal::Int(2))),
      ),
    );
  }

  #[test]
  fn test_function_application_precedence() {
    testing::assert_eq(
      parse_expr,
      "fun 40 2 + 42",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Application(
            (0, 0),
            Box::new(Expression::Application(
              (0, 0),
              Box::new(Expression::Ref((0, 0), "fun".to_string())),
              Box::new(Expression::Literal((0, 0), Literal::Int(40))),
            )),
            Box::new(Expression::Literal((0, 0), Literal::Int(2))),
          ),
          Expression::Literal((0, 0), Literal::Int(42)),
        ],
        vec!["+".to_string()],
      ),
    );
  }

  #[test]
  fn test_multiline_expr() {
    let code = indoc! {"
      fun []
        []
    "};

    testing::assert_eq(
      parse_expr,
      code,
      Expression::Application(
        (0, 0),
        Box::new(Expression::Application(
          (0, 0),
          Box::new(Expression::Ref((0, 0), "fun".to_string())),
          Box::new(Expression::List((0, 0), vec![])),
        )),
        Box::new(Expression::List((0, 0), vec![])),
      ),
    );
  }

  #[test]
  fn test_prefix_minus() {
    testing::assert_eq(
      parse_expr,
      "-(40+2)",
      Expression::Application(
        (0, 0),
        Box::from(Expression::Ref((0, 0), "__internal__minus".to_string())),
        Box::from(Expression::OperatorChain(
          (0, 0),
          vec![
            Expression::Literal((0, 0), Literal::Int(40)),
            Expression::Literal((0, 0), Literal::Int(2)),
          ],
          vec!["+".to_string()],
        )),
      ),
    );
  }

  #[test]
  fn test_infix_minus() {
    testing::assert_eq(
      parse_expr,
      "42 - 2",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(42)),
          Expression::Literal((0, 0), Literal::Int(2)),
        ],
        vec!["-".to_string()],
      ),
    );
  }

  #[test]
  fn test_infix_minus_precedence() {
    testing::assert_eq(
      parse_expr,
      "42 -2",
      Expression::Application(
        (0, 0),
        Box::new(Expression::Literal((0, 0), Literal::Int(42))),
        Box::new(Expression::Application(
          (0, 0),
          Box::new(Expression::Ref((0, 0), "__internal__minus".to_string())),
          Box::new(Expression::Literal((0, 0), Literal::Int(2))),
        )),
      ),
    );
  }

  #[test]
  fn test_infix_minus_validity() {
    testing::assert_eq(
      parse_expr,
      "42- 2",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(42)),
          Expression::Literal((0, 0), Literal::Int(2)),
        ],
        vec!["-".to_string()],
      ),
    );
  }

  #[test]
  fn test_infix_minus_edge() {
    testing::assert_eq(
      parse_expr,
      "42-2",
      Expression::OperatorChain(
        (0, 0),
        vec![
          Expression::Literal((0, 0), Literal::Int(42)),
          Expression::Literal((0, 0), Literal::Int(2)),
        ],
        vec!["-".to_string()],
      ),
    );
  }

  #[test]
  fn test_prefix_minus_edge() {
    testing::assert_eq(
      parse_expr,
      "-n string",
      Expression::Application(
        (0, 0),
        Box::from(Expression::Application(
          (0, 3),
          Box::from(Expression::Ref((0, 1), "__internal__minus".to_string())),
          Box::from(Expression::Ref((1, 2), "n".to_string())),
        )),
        Box::from(Expression::Ref((0, 0), "string".to_string())),
      ),
    );
  }
}
