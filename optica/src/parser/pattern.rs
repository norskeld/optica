use super::combinators;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::lexer::Token;
use crate::source::Input;
use crate::utils;

pub fn parse_pattern_expr(input: Input) -> Result<(Pattern, Input), ParseError> {
  let (mut pattern, mut input) = parse_pattern(input)?;
  let start = input.pos();

  if let Token::BinaryOperator(_) = input.read() {
    let mut chain: Vec<(Pattern, String)> = vec![];
    let mut last_pattern = pattern;

    // Read chain of a::(b::(c::(d))).
    while let Token::BinaryOperator(op) = input.read() {
      let (next_pattern, rest) = parse_pattern(input.next())?;

      chain.push((last_pattern, op));

      last_pattern = next_pattern;
      input = rest;
    }

    pattern = chain
      .into_iter()
      .rev()
      .fold(last_pattern, |acc, (pattern, op)| {
        let (start, _) = pattern.span();
        let (_, end) = acc.span();

        Pattern::BinaryOperator((start, end), op, Box::from(pattern), Box::from(acc))
      });
  } else if let Token::AsKw = input.read() {
    let (alias, input) = combinators::expect_ident(input.next())?;

    return Ok((
      Pattern::Alias((start, input.pos_end()), Box::from(pattern), alias),
      input,
    ));
  }

  Ok((pattern, input))
}

pub fn parse_pattern(input: Input) -> Result<(Pattern, Input), ParseError> {
  parse_pattern_base(input, true)
}

pub fn parse_pattern_without_adt(input: Input) -> Result<(Pattern, Input), ParseError> {
  parse_pattern_base(input, false)
}

fn parse_pattern_base(input: Input, adt: bool) -> Result<(Pattern, Input), ParseError> {
  let start = input.pos();

  let (pattern, input) = match input.read() {
    | Token::Ident(name) => (Pattern::Var(input.span(), name), input.next()),
    | Token::UpperIdent(name) => {
      if adt {
        let (params, input) = combinators::many0(&parse_pattern_without_adt, input.next())?;
        let adt = Pattern::Adt((start, input.pos_end()), name, params);

        (adt, input)
      } else {
        let adt = Pattern::Adt(input.span(), name, vec![]);

        (adt, input.next())
      }
    },
    // Unit => ()
    // Parens => (a)
    // Tuple => (a,) (a, b,) (a, b, c)
    | Token::LeftParen => {
      let input = input.next();

      match input.read() {
        // Unit ()
        | Token::RightParen => (Pattern::Unit((start, start + 2)), input.next()),
        | _ => {
          let (first, input) = parse_pattern_expr(input)?;

          match input.read() {
            // Parens (a)
            | Token::RightParen => (first, input.next()),
            // Tuple (a,)
            | _ => {
              let input = combinators::expect(Token::Comma, input)?;
              let (rest, input) = combinators::comma0(&parse_pattern_expr, input)?;

              let input = combinators::expect(Token::RightParen, input)?;

              let adt = Pattern::Tuple((start, input.pos_end()), utils::vec::cons(first, rest));

              (adt, input)
            },
          }
        },
      }
    },
    | Token::LeftBracket => {
      let (values, input) = combinators::comma0(&parse_pattern_expr, input.next())?;
      let input = combinators::expect(Token::RightBracket, input)?;

      (Pattern::List((start, input.pos_end()), values), input)
    },
    | Token::Underscore => {
      (
        Pattern::Wildcard((start, input.next().pos_end())),
        input.next(),
      )
    },
    | Token::LitInt(value) => {
      (
        Pattern::LitInt((start, input.next().pos_end()), value),
        input.next(),
      )
    },
    | Token::LitChar(value) => {
      (
        Pattern::LitChar((start, input.next().pos_end()), value),
        input.next(),
      )
    },
    | Token::LitString(value) => {
      (
        Pattern::LitString((start, input.next().pos_end()), value),
        input.next(),
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

  Ok((pattern, input))
}

#[cfg(test)]
mod tests {
  use super::super::testing;
  use super::*;

  #[test]
  fn test_patterns() {
    testing::is_ok(parse_pattern, "a");
    testing::is_ok(parse_pattern, "A");
    testing::is_ok(parse_pattern, "A a");
    testing::is_ok(parse_pattern, "()");
    testing::is_ok(parse_pattern, "(a)");
    testing::is_ok(parse_pattern, "(a, a)");
    testing::is_ok(parse_pattern, "(a, a, a)");
    testing::is_ok(parse_pattern, "A ()");
    testing::is_ok(parse_pattern, "_");
    testing::is_ok(parse_pattern, "[]");
    testing::is_ok(parse_pattern, "[a]");
    testing::is_ok(parse_pattern, "(x::xs)");
    testing::is_ok(parse_pattern, "[x::xs]");
    testing::is_ok(parse_pattern, "Leaf _");
  }

  #[test]
  fn test_pattern_errors() {
    testing::is_err(parse_pattern, "(a, a,)");
    testing::is_err(parse_pattern, "[a,]");
  }

  #[test]
  fn test_literal() {
    testing::assert_eq(parse_pattern, "1", Pattern::LitInt((0, 1), 1));
  }

  #[test]
  fn test_variable() {
    testing::assert_eq(
      parse_pattern,
      "variable",
      Pattern::Var((0, 0), "variable".to_string()),
    );
  }

  #[test]
  fn test_adt() {
    testing::assert_eq(
      parse_pattern,
      "List a",
      Pattern::Adt(
        (0, 0),
        "List".to_string(),
        vec![Pattern::Var((0, 0), "a".to_string())],
      ),
    );
  }

  #[test]
  fn test_wildcard() {
    testing::assert_eq(parse_pattern, "_", Pattern::Wildcard((0, 0)));
  }

  #[test]
  fn test_unit() {
    testing::assert_eq(parse_pattern, "()", Pattern::Unit((0, 0)));
  }

  #[test]
  fn test_tuple() {
    testing::assert_eq(
      parse_pattern,
      "(a, b)",
      Pattern::Tuple(
        (0, 0),
        vec![
          Pattern::Var((0, 0), "a".to_string()),
          Pattern::Var((0, 0), "b".to_string()),
        ],
      ),
    );
  }

  #[test]
  fn test_empty_list() {
    testing::assert_eq(parse_pattern, "[]", Pattern::List((0, 0), vec![]));
  }

  #[test]
  fn test_list() {
    testing::assert_eq(
      parse_pattern,
      "[a, b]",
      Pattern::List(
        (0, 0),
        vec![
          Pattern::Var((0, 0), "a".to_string()),
          Pattern::Var((0, 0), "b".to_string()),
        ],
      ),
    );
  }

  #[test]
  fn test_operator_associativity() {
    testing::assert_eq(
      parse_pattern_expr,
      "a::b::c::d",
      Pattern::BinaryOperator(
        (0, 0),
        "::".to_string(),
        Box::from(Pattern::Var((0, 0), "a".to_string())),
        Box::from(Pattern::BinaryOperator(
          (0, 0),
          "::".to_string(),
          Box::from(Pattern::Var((0, 0), "b".to_string())),
          Box::from(Pattern::BinaryOperator(
            (0, 0),
            "::".to_string(),
            Box::from(Pattern::Var((0, 0), "c".to_string())),
            Box::from(Pattern::Var((0, 0), "d".to_string())),
          )),
        )),
      ),
    );
  }
}
