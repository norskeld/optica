use crate::ast::untyped::Type;
use crate::errors::ParseError;
use crate::lexer::Token;
use crate::source::Input;
use crate::utils;
use super::combinators;

pub fn parse_type(input: Input) -> Result<(Type, Input), ParseError> {
  let (ty, input) = parse_type_with_adt(input)?;
  let (rest, input) = combinators::many0(&parse_type_chain, input)?;

  Ok((create_function(ty, rest), input))
}

pub fn parse_type_without_adt(input: Input) -> Result<(Type, Input), ParseError> {
  parse_type_base(input, false)
}

pub fn parse_type_with_adt(input: Input) -> Result<(Type, Input), ParseError> {
  parse_type_base(input, true)
}

fn parse_type_base(input: Input, with_adt: bool) -> Result<(Type, Input), ParseError> {
  let (ty, input) = match input.read() {
    | Token::Ident(name) => (Type::Var(name), input.next()),
    | Token::UpperIdent(name) => {
      let input = input.next();

      let (name, input) = match input.read() {
        | Token::Dot => {
          let (second, mut input) = combinators::expect_upper_ident(input.next())?;
          let mut acc = vec![second];

          while let Token::Dot = input.read() {
            let (next, rest) = combinators::expect_upper_ident(input.next())?;
            acc.push(next);

            input = rest;
          }

          let mut names = utils::vec::create_vec(name, acc);
          let names = names.pop().map(|last| (names, last));

          match names {
            | Some((list, last)) => (create_qualified_name(&list, &last), input),
            | None => {
              return Err(ParseError::ExpectedQualifiedName {
                span: input.span(),
                found: input.read(),
              });
            },
          }
        },
        | _ => (name, input),
      };

      if with_adt {
        let (params, input) = combinators::many0(&parse_type_with_adt, input)?;
        (Type::Tag(name, params), input)
      } else {
        (Type::Tag(name, vec![]), input)
      }
    },
    // () => Unit
    // (a) => Paren
    // (a,) or (a, b,) or (a, b, c) => Tuple
    | Token::LeftParen => {
      let input = input.next();

      match input.read() {
        | Token::RightParen => (Type::Unit, input.next()),
        | _ => {
          let (first, input) = parse_type(input)?;

          match input.read() {
            | Token::RightParen => (first, input.next()),
            | _ => {
              let input = combinators::expect(Token::Comma, input)?;
              let (rest, input) = combinators::comma0(&parse_type, input)?;
              let input = combinators::expect(Token::RightParen, input)?;

              (Type::Tuple(utils::vec::create_vec(first, rest)), input)
            },
          }
        },
      }
    },
    | _ => {
      return Err(ParseError::UnmatchedToken {
        span: input.span(),
        found: input.read(),
        options: vec![],
      });
    },
  };

  Ok((ty, input))
}

fn parse_type_chain(input: Input) -> Result<(Type, Input), ParseError> {
  let input = combinators::expect(Token::RightArrow, input)?;
  let (ty, input) = parse_type(input)?;

  Ok((ty, input))
}

fn create_function(ty: Type, types: Vec<Type>) -> Type {
  if types.is_empty() {
    return ty;
  }

  let types = utils::vec::create_vec(ty, types);
  let mut types_reversed = types.into_iter().rev();

  types_reversed
    .next()
    .map(|first| {
      types_reversed.fold(first, |acc, next| {
        Type::Function(Box::new(next), Box::new(acc))
      })
    })
    .unwrap()
}

fn create_qualified_name(path: &[String], name: &str) -> String {
  let mut full_name = String::new();

  for x in path {
    full_name.push_str(x);
    full_name.push('.');
  }

  full_name.push_str(name);
  full_name
}

#[cfg(test)]
mod tests {
  use super::super::testing;
  use super::*;

  #[test]
  fn test_types() {
    testing::is_ok(parse_type, "Int");
    testing::is_ok(parse_type, "()");
    testing::is_ok(parse_type, "(Int)");
    testing::is_ok(parse_type, "(Int, Int)");
    testing::is_ok(parse_type, "List a");
    testing::is_ok(parse_type, "List (Int, a)");
    testing::is_ok(parse_type, "Int -> Int");
    testing::is_ok(parse_type, "Int -> Int -> (Int, Int)");
  }

  #[test]
  fn test_errors() {
    testing::is_err(parse_type, "Int ->");
    testing::is_err(parse_type, "-> Int");
  }

  #[test]
  fn test_maybe_priority() {
    testing::assert_eq(
      parse_type,
      "Maybe a -> Maybe b -> Maybe value",
      Type::Function(
        Box::from(Type::Tag(
          "Maybe".to_string(),
          vec![Type::Var("a".to_string())],
        )),
        Box::from(Type::Function(
          Box::from(Type::Tag(
            "Maybe".to_string(),
            vec![Type::Var("b".to_string())],
          )),
          Box::from(Type::Tag(
            "Maybe".to_string(),
            vec![Type::Var("value".to_string())],
          )),
        )),
      ),
    );
  }

  #[test]
  fn test_unit() {
    testing::assert_eq(parse_type, "()", Type::Unit);
  }

  #[test]
  fn test_variable() {
    testing::assert_eq(parse_type, "a", Type::Var("a".to_string()));
  }

  #[test]
  fn test_tag() {
    testing::assert_eq(
      parse_type,
      "List a",
      Type::Tag("List".to_string(), vec![Type::Var("a".to_string())]),
    );
  }

  #[test]
  fn test_tuple() {
    testing::assert_eq(
      parse_type,
      "(a, b)",
      Type::Tuple(vec![Type::Var("a".to_string()), Type::Var("b".to_string())]),
    );
  }

  #[test]
  fn test_tuple_many() {
    testing::assert_eq(
      parse_type,
      "(a, b, c, d, e, f)",
      Type::Tuple(vec![
        Type::Var("a".to_string()),
        Type::Var("b".to_string()),
        Type::Var("c".to_string()),
        Type::Var("d".to_string()),
        Type::Var("e".to_string()),
        Type::Var("f".to_string()),
      ]),
    );
  }

  #[test]
  fn test_parens() {
    testing::assert_eq(parse_type, "(a)", Type::Var("a".to_string()));
  }

  #[test]
  fn test_function() {
    testing::assert_eq(
      parse_type,
      "Int -> Float -> a",
      Type::Function(
        Box::new(Type::Tag("Int".to_string(), vec![])),
        Box::new(Type::Function(
          Box::new(Type::Tag("Float".to_string(), vec![])),
          Box::new(Type::Var("a".to_string())),
        )),
      ),
    );
  }
}
