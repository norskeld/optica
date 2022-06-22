use super::combinators;
use super::expression;
use super::pattern;
use super::types;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::lexer::Token;
use crate::source::{Input, Span};

pub fn parse_statement(input: Input) -> Result<(Statement, Input), ParseError> {
  let (stmt, input) = match input.read() {
    | Token::DataKw => {
      let input = input.next();

      let (name, input) = combinators::expect_upper_ident(input)?;
      let (params, input) = combinators::many0(&combinators::expect_ident, input)?;
      let input = combinators::expect(Token::Equals, input)?;
      let (branches, input) = combinators::pipe1(&parse_adt_branch, input)?;

      (Statement::Adt(name, params, branches), input)
    },
    | Token::TypeKw => {
      let input = input.next();

      let (name, input) = combinators::expect_upper_ident(input)?;
      let (params, input) = combinators::many0(&combinators::expect_ident, input)?;
      let input = combinators::expect(Token::Equals, input)?;
      let (ty, input) = types::parse_type(input)?;

      (Statement::Alias(name, params, ty), input)
    },
    | Token::Ident(_) => {
      let (func, input) = parse_definition(0, input)?;
      (Statement::Function(func), input)
    },
    | Token::InfixLeftKw => parse_infix(input, InfixDirection::Left)?,
    | Token::InfixRightKw => parse_infix(input, InfixDirection::Right)?,
    | _ => {
      return Err(ParseError::UnmatchedToken {
        span: input.span(),
        found: input.read(),
        options: vec![],
      });
    },
  };

  Ok((stmt, input))
}

pub fn parse_definition(indent: u32, input: Input) -> Result<(Definition, Input), ParseError> {
  let (name, input) = combinators::expect_ident(input)?;

  let (header, input) = match input.read() {
    | Token::Colon => {
      let (ty, input) = types::parse_type(input.next())?;
      let input = combinators::expect_indent(indent, input)?;
      let (func_name, input) = combinators::expect_ident(input)?;

      // TODO: Get rid of this assertion.
      assert_eq!(func_name, name);

      (Some(ty), input)
    },
    | _ => (None, input),
  };

  let (patterns, input) = combinators::many0(&pattern::parse_pattern, input)?;
  let input = combinators::expect(Token::Equals, input)?;
  let (expression, input) = expression::parse_expr(input)?;

  Ok((
    Definition {
      header,
      name,
      patterns,
      expression,
    },
    input,
  ))
}

fn parse_infix(input: Input, direction: InfixDirection) -> Result<(Statement, Input), ParseError> {
  let input = input.next();

  let (level, input) = combinators::expect_int(input)?;
  let input = combinators::expect(Token::LeftParen, input)?;

  let (op, input) = combinators::expect_binary_operator(input)?;
  let input = combinators::expect(Token::RightParen, input)?;
  let input = combinators::expect(Token::Equals, input)?;

  let (func, input) = combinators::expect_ident(input)?;

  Ok((Statement::Infix(direction, level, op, func), input))
}

#[allow(clippy::type_complexity)]
fn parse_adt_branch(input: Input) -> Result<((Span, String, Vec<Type>), Input), ParseError> {
  let start = input.pos();

  let (name, input) = combinators::expect_upper_ident(input)?;
  let (params, input) = combinators::many0(&types::parse_type_without_adt, input)?;

  Ok((((start, input.pos_end()), name, params), input))
}

#[cfg(test)]
mod tests {
  use indoc::indoc;

  use super::super::testing;
  use super::*;
  use crate::ast::untyped::{Expression, Literal, Pattern};

  #[test]
  fn test_statements() {
    testing::is_ok(parse_statement, "const = 0");
    testing::is_ok(parse_statement, "addTuple (a, b) = a + b");
    testing::is_ok(parse_statement, "hello message = message");
    testing::is_ok(parse_statement, "data Bool = True | False");
    testing::is_ok(parse_statement, "data List a = Cons a List | Nil");

    testing::is_ok(
      parse_statement,
      indoc! {"
        hello
          message
            =
              message
      "},
    );

    testing::is_ok(
      parse_statement,
      indoc! {"
        add : Int -> Int -> Int
        add a b = a + b"
      },
    );

    testing::is_ok(
      parse_statement,
      indoc! {"
        data Bool
          = True
          | False
      "},
    );

    testing::is_ok(
      parse_statement,
      indoc! {"
        data List a
          = Cons a List
          | Nil
      "},
    );

    testing::is_ok(
      parse_statement,
      indoc! {"
        const =
          42
      "},
    );
  }

  #[test]
  fn test_errors() {
    testing::is_err(parse_statement, "data Bool");

    testing::is_err(
      parse_statement,
      indoc! {"
        data Bool
        = True
        | False
      "},
    );

    testing::is_err(
      parse_statement,
      indoc! {"
        data List a
        = Cons a List
        | Nil
      "},
    );
  }

  #[test]
  fn test_type_alias() {
    testing::assert_eq(
      parse_statement,
      "type Message = String",
      Statement::Alias(
        "Message".to_string(),
        vec![],
        Type::Tag("String".to_string(), vec![]),
      ),
    );
  }

  #[test]
  fn test_adt() {
    testing::assert_eq(
      parse_statement,
      "data Bool = True | False",
      Statement::Adt(
        "Bool".to_string(),
        vec![],
        vec![
          ((12, 16), "True".to_string(), vec![]),
          ((19, 24), "False".to_string(), vec![]),
        ],
      ),
    );
  }

  #[test]
  fn test_function_definition() {
    testing::assert_eq(
      parse_statement,
      "unit x = ()",
      Statement::Function(Definition {
        header: None,
        name: "unit".to_string(),
        patterns: vec![Pattern::Var((5, 6), "x".to_string())],
        expression: Expression::Unit((11, 11)),
      }),
    );
  }

  #[test]
  fn test_function_definition_const() {
    testing::assert_eq(
      parse_statement,
      "meaningOfLife = 42",
      Statement::Function(Definition {
        header: None,
        name: "meaningOfLife".to_string(),
        patterns: vec![],
        expression: Expression::Literal((16, 18), Literal::Int(42)),
      }),
    );
  }

  #[test]
  fn test_function_definition_const_typed() {
    let code = indoc! {"
      meaningOfLife: Int
      meaningOfLife = 42
    "};

    testing::assert_eq(
      parse_statement,
      code,
      Statement::Function(Definition {
        header: Some(Type::Tag("Int".to_string(), vec![])),
        name: "meaningOfLife".to_string(),
        patterns: vec![],
        expression: Expression::Literal((35, 37), Literal::Int(42)),
      }),
    );
  }
}
