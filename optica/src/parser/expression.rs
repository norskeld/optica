use super::combinators;
use super::pattern;
use super::statement;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::lexer::Token;
use crate::source::{Input, Span};
use crate::utils;

pub fn parse_expr(input: Input) -> Result<(Expression, Input), ParseError> {
  let (first, input) = parse_expr_application(input)?;
  let (rest, input) = combinators::many0(&binary_operator_expr, input)?;

  Ok((create_binary_operator_chain(first, rest), input))
}

fn check_swap_span(span: Span) -> Span {
  match span {
    | (l, r) if l > r => (r, l),
    | _ => span,
  }
}

fn parse_expr_application(input: Input) -> Result<(Expression, Input), ParseError> {
  let (exprs, input) = combinators::many1(&parse_expr_base, input)?;
  let span_start = input.pos();

  let mut iter = exprs.into_iter();
  let first = iter.next().unwrap();

  let tree = iter.fold(first, |acc, next| {
    let (_, span_end) = next.get_span();
    let span = check_swap_span((span_start, span_end));

    Expression::Application(span, Box::new(acc), Box::new(next))
  });

  Ok((tree, input))
}

fn parse_expr_base(input: Input) -> Result<(Expression, Input), ParseError> {
  let (expr, input) = match input.read() {
    | Token::LitInt(value) => {
      (
        Expression::Literal(input.span(), Literal::Int(value)),
        input.next(),
      )
    },
    | Token::LitFloat(value) => {
      (
        Expression::Literal(input.span(), Literal::Float(value)),
        input.next(),
      )
    },
    | Token::LitChar(value) => {
      (
        Expression::Literal(input.span(), Literal::Char(value)),
        input.next(),
      )
    },
    | Token::LitString(value) => {
      (
        Expression::Literal(input.span(), Literal::String(value)),
        input.next(),
      )
    },
    | Token::Ident(name) => (Expression::Ref(input.span(), name), input.next()),
    | Token::UpperIdent(first) => {
      let span_start = input.pos();
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
              (span_start, input.pos()),
              utils::vec::cons(first, rest),
              name,
            ),
            input,
          )
        },
        | _ => {
          (
            Expression::Ref((span_start, next_input.pos()), first),
            input.next(),
          )
        },
      }
    },
    | Token::IfKw => {
      let span_start = input.pos();
      let (condition, input) = parse_expr(input.next())?;

      let input = combinators::expect(Token::ThenKw, input)?;
      let (true_branch, input) = parse_expr(input)?;

      let input = combinators::expect(Token::ElseKw, input)?;
      let (false_branch, input) = parse_expr(input)?;

      (
        Expression::If(
          (span_start, input.pos()),
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
      let span_start = input.pos();
      let input = input.next();

      match input.read() {
        // ()
        | Token::RightParen => {
          let input = input.next();
          (Expression::Unit((span_start, input.pos())), input)
        },
        // (+)
        | Token::BinaryOperator(op) => {
          let input = combinators::expect(Token::RightParen, input.next())?;
          (Expression::Ref((span_start, input.pos()), op), input)
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
                Expression::Tuple((span_start, input.pos()), utils::vec::cons(value, rest)),
                input,
              )
            },
          }
        },
      }
    },
    | Token::LeftBracket => {
      let span_start = input.pos();

      let (values, input) = combinators::comma0(&parse_expr, input.next())?;
      let input = combinators::expect(Token::RightBracket, input)?;

      (Expression::List((span_start, input.pos()), values), input)
    },
    | Token::BackSlash => {
      let span_start = input.pos();

      let (patterns, input) = combinators::many1(&pattern::parse_pattern, input.next())?;

      let input = combinators::expect(Token::RightArrow, input)?;
      let (expr, input) = parse_expr(input)?;

      (
        Expression::Lambda((span_start, input.pos()), patterns, Box::from(expr)),
        input,
      )
    },
    | Token::PrefixMinus => {
      let span_start = input.pos();

      let (expr, input) = parse_expr_base(input.next())?;

      (
        Expression::Application(
          (span_start, input.pos()),
          Box::from(Expression::Ref(
            (input.pos(), input.pos() + 1),
            String::from("__minus"),
          )),
          Box::from(expr),
        ),
        input,
      )
    },
    | Token::LetKw => {
      let input = input.next();
      let level = combinators::read_optional_indent(input.clone());

      let input = input.enter_level(level);

      let (let_definitions, input) = combinators::many1(
        &|input| parse_let(level, combinators::expect_indent(level, input)?),
        input,
      )?;

      let input = input.exit_level(level);

      let input = combinators::expect(Token::InKw, input)?;
      let (let_expression, input) = parse_expr(input)?;

      (
        Expression::Let(
          (input.pos(), input.pos()),
          let_definitions,
          Box::from(let_expression),
        ),
        input,
      )
    },
    | Token::MatchKw => {
      let span_start = input.pos();

      let (cond, input) = parse_expr(input.next())?;

      let input = combinators::expect(Token::WithKw, input)?;
      let level = combinators::read_indent(input.clone())?;
      let input = input.enter_level(level);

      let (branches, input) = combinators::many1(&|input| parse_match_branch(level, input), input)?;

      let input = input.exit_level(level);

      (
        Expression::Match((span_start, input.pos()), Box::from(cond), branches),
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

fn parse_match_branch(
  indent: u32,
  input: Input,
) -> Result<((Pattern, Expression), Input), ParseError> {
  let input = combinators::expect_indent(indent, input)?;

  let input = combinators::expect(Token::Pipe, input)?;
  let (pattern, input) = pattern::parse_pattern_expr(input)?;

  let input = combinators::expect(Token::FatRightArrow, input)?;
  let (expr, input) = parse_expr(input)?;

  Ok(((pattern, expr), input))
}

fn parse_let(indent: u32, input: Input) -> Result<(Let, Input), ParseError> {
  match input.read() {
    | Token::Ident(_) => {
      let (definition, input) = statement::parse_definition(indent, input)?;
      Ok((Let::Definition(definition), input))
    },
    | _ => {
      let (pattern, input) = pattern::parse_pattern(input)?;
      let input = combinators::expect(Token::Equals, input)?;
      let (expression, input) = parse_expr(input)?;

      Ok((Let::Pattern(pattern, expression), input))
    },
  }
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
    exprs.first().unwrap().get_span(),
    exprs.last().unwrap().get_span(),
  );

  Expression::OperatorChain((first, last), exprs, ops)
}

#[cfg(test)]
mod tests {
  use indoc::indoc;

  use super::super::testing;
  use super::*;
  use crate::ast::untyped::Pattern;

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
    testing::is_ok(parse_expr, "let life = 42 in life");
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

  #[test]
  fn test_let() {
    testing::assert_eq(
      parse_expr,
      "let life = 42 in life",
      Expression::Let(
        (21, 21),
        vec![Let::Definition(Definition {
          header: None,
          name: "life".to_string(),
          patterns: vec![],
          expression: Expression::Literal((11, 13), Literal::Int(42)),
        })],
        Box::from(Expression::Ref((17, 21), "life".to_string())),
      ),
    );
  }

  #[test]
  fn test_let_indented() {
    // NOTE: `in` is indented because we are parsing expression outside of assignment. In real code
    // everything works as expected.
    let code = indoc! {"
      let
        life = 42
       in
        life
    "};

    testing::assert_eq(
      parse_expr,
      code,
      Expression::Let(
        (26, 26),
        vec![Let::Definition(Definition {
          header: None,
          name: "life".to_string(),
          patterns: vec![],
          expression: Expression::Literal((13, 15), Literal::Int(42)),
        })],
        Box::from(Expression::Ref((22, 26), "life".to_string())),
      ),
    );
  }

  #[test]
  fn test_match_of() {
    let code = indoc! {r#"
      match direction with
        | Up => "Up"
        | Down => "Down"
        | _ => "Nowhere"
    "#};

    testing::assert_eq(
      parse_expr,
      code,
      Expression::Match(
        (0, 0),
        Box::from(Expression::Ref((0, 0), "direction".to_string())),
        vec![
          (
            Pattern::Adt((0, 0), "Up".to_string(), vec![]),
            Expression::Literal((0, 0), Literal::String("Up".to_string())),
          ),
          (
            Pattern::Adt((0, 0), "Down".to_string(), vec![]),
            Expression::Literal((0, 0), Literal::String("Down".to_string())),
          ),
          (
            Pattern::Wildcard((0, 0)),
            Expression::Literal((0, 0), Literal::String("Nowhere".to_string())),
          ),
        ],
      ),
    )
  }
}
