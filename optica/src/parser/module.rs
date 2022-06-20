use crate::ast::untyped::{
  AdtExports, Module, ModuleExport, ModuleExports, ModuleHeader, ModuleImport,
};
use crate::errors::ParseError;
use crate::lexer::Token;
use crate::source::Input;
use crate::utils;
use super::combinators;
use super::statement;

pub fn parse_module(input: Input) -> Result<(Module, Input), ParseError> {
  let input = skip_empty_lines(input)?;

  let (header, input) = match input.read() {
    | Token::ModuleKw => {
      let (header, input) = parse_module_header(input)?;
      (Some(header), input)
    },
    | _ => (None, input),
  };

  let mut imports = vec![];
  let mut input = input;

  loop {
    input = skip_empty_lines(input)?;

    match input.read() {
      | Token::ImportKw => {
        let (import, next_input) = parse_import(input)?;

        imports.push(import);
        input = next_input;
      },
      | _ => {
        break;
      },
    }
  }

  let mut statements = vec![];
  let mut input = input;

  loop {
    input = skip_empty_lines(input)?;

    if let Token::Eof = input.read() {
      break;
    }

    match statement::parse_statement(input.clone()) {
      | Ok((stmt, next_input)) => {
        statements.push(stmt);
        input = next_input;
      },
      | Err(e) => {
        // TODO: Collect all errors.
        return Err(e);
      },
    }
  }

  Ok((
    Module {
      header,
      imports,
      statements,
    },
    input,
  ))
}

fn skip_to_the_next_block(input: Input) -> Input {
  let mut input = input;

  loop {
    match input.read() {
      | Token::Indent(0) => {
        break;
      },
      | _ => {
        input = input.next();
      },
    }
  }

  input
}

fn skip_empty_lines(input: Input) -> Result<Input, ParseError> {
  let mut input = input;

  while combinators::expect_indent(0, input.clone()).is_ok() {
    input = input.next()
  }

  Ok(input)
}

fn parse_module_header(input: Input) -> Result<(ModuleHeader, Input), ParseError> {
  let (name, input) = match input.read() {
    | Token::ModuleKw => {
      let input = input.next();
      combinators::expect_upper_chain(input)?
    },
    | _ => {
      let found = input.read();

      return Err(ParseError::UnmatchedToken {
        span: input.span(),
        found,
        options: vec![Token::ModuleKw],
      });
    },
  };

  let input = combinators::expect(Token::LeftParen, input)?;

  let (exports, input) = match input.read() {
    | Token::DoubleDot => (ModuleExports::All, input.next()),
    | _ => {
      let (exposing, input) = combinators::comma1(&parse_export, input)?;
      (ModuleExports::Just(exposing), input)
    },
  };

  let input = match input.read() {
    | Token::Indent(_) => input.next(),
    | _ => input,
  };

  let input = combinators::expect(Token::RightParen, input)?;
  let input = combinators::expect(Token::WhereKw, input)?;

  Ok((ModuleHeader { name, exports }, input))
}

fn parse_import(input: Input) -> Result<(ModuleImport, Input), ParseError> {
  let input = combinators::expect(Token::ImportKw, input)?;
  let (first, input) = combinators::expect_upper_ident(input)?;
  let (rest, input) = combinators::many0(&parse_dot_name, input)?;

  let path = utils::vec::create_vec(first, rest);

  let (alias, input) = match input.read() {
    | Token::AsKw => {
      let (alias, input) = combinators::expect_upper_ident(input.next())?;
      (Some(alias), input)
    },
    | _ => (None, input),
  };

  let (exports, input) = match input.read() {
    | Token::LeftParen => {
      let input = input.next();

      let (exposing, input) = match input.read() {
        | Token::DoubleDot => (ModuleExports::All, input.next()),
        | _ => {
          let (exposing, input) = combinators::comma1(&parse_export, input)?;
          (ModuleExports::Just(exposing), input)
        },
      };

      (
        Some(exposing),
        combinators::expect(Token::RightParen, input)?,
      )
    },
    | _ => (None, input),
  };

  Ok((
    ModuleImport {
      path,
      alias,
      exports,
    },
    input,
  ))
}

fn parse_dot_name(input: Input) -> Result<(String, Input), ParseError> {
  let input = combinators::expect(Token::Dot, input)?;
  let (name, input) = combinators::expect_upper_ident(input)?;

  Ok((name, input))
}

fn parse_export(input: Input) -> Result<(ModuleExport, Input), ParseError> {
  match input.read() {
    | Token::Ident(def) => Ok((ModuleExport::Function(def), input.next())),
    | Token::UpperIdent(name) => {
      let input = input.next();

      match input.read() {
        | Token::LeftParen => {
          let input = input.next();

          let (exposing, input) = match input.read() {
            | Token::DoubleDot => (AdtExports::All, input.next()),
            | _ => {
              let (variants, input) = combinators::comma1(&combinators::expect_upper_ident, input)?;
              (AdtExports::Just(variants), input)
            },
          };

          let input = combinators::expect(Token::RightParen, input)?;

          Ok((ModuleExport::Adt(name, exposing), input))
        },
        | _ => Ok((ModuleExport::Type(name), input)),
      }
    },
    | Token::LeftParen => {
      let (op, input) = combinators::expect_binary_operator(input.next())?;
      let input = combinators::expect(Token::RightParen, input)?;

      Ok((ModuleExport::BinaryOperator(op), input))
    },
    | _ => {
      let found = input.read();

      let options = vec![
        Token::Ident("definition".to_owned()),
        Token::UpperIdent("type".to_owned()),
        Token::LeftParen,
      ];

      Err(ParseError::UnmatchedToken {
        span: input.span(),
        found,
        options,
      })
    },
  }
}

#[cfg(test)]
mod tests {
  use indoc::indoc;

  use crate::ast::untyped::{AdtExports, Definition, Expression, Literal, Pattern, Statement};
  use super::super::testing;
  use super::*;

  #[test]
  fn test_import() {
    testing::is_ok(parse_import, "import Util");
    testing::is_ok(parse_import, "import Util as U");
    testing::is_ok(parse_import, "import Util (A)");
    testing::is_ok(parse_import, "import Util (func)");
    testing::is_ok(parse_import, "import Util (A, b, C, d)");
    testing::is_ok(parse_import, "import Util (A(..))");
    testing::is_ok(parse_import, "import Util (A(B))");
    testing::is_ok(parse_import, "import Util (A(B, C))");
    testing::is_ok(parse_import, "import Defs (Grid)");
    testing::is_ok(parse_import, "import Util as U (A)");
  }

  #[test]
  fn test_import_error() {
    testing::is_err(parse_import, "import Util (A())");
    testing::is_err(parse_import, "import Util ()");
  }

  #[test]
  fn test_module_header() {
    testing::is_ok(parse_module_header, "module Main (..) where");
    testing::is_ok(parse_module_header, "module Util (func) where");
    testing::is_ok(parse_module_header, "module Util (A) where");
    testing::is_ok(parse_module_header, "module Util (A, func) where");
    testing::is_ok(parse_module_header, "module Util (A(..)) where");
    testing::is_ok(parse_module_header, "module Util (A(..), func) where");
    testing::is_ok(parse_module_header, "module Util (A(B)) where");
    testing::is_ok(parse_module_header, "module Util (A(B), func) where");
    testing::is_ok(parse_module_header, "module Util (A(B, C), func) where");
    testing::is_ok(parse_module_header, "module Util (A(B, C), D(..)) where");
    testing::is_ok(
      parse_module_header,
      "module Util (A(B, C), D(..), func) where",
    );
  }

  #[test]
  fn test_basic_module() {
    let code = indoc! {"
      module Main (..) where

      import Util

      x = 0
      func x = x
    "};

    testing::is_ok(parse_module, code.trim());
  }

  #[test]
  fn test_multiline_exports() {
    let code = indoc! {"
      module Main (
        A,
        B,
        C,
        D
      ) where
    "};

    testing::is_ok(parse_module, code.trim());
  }

  #[test]
  fn test_empty_module() {
    testing::assert_eq(
      parse_module,
      "module Main (..) where",
      Module {
        header: Some(ModuleHeader {
          name: "Main".to_string(),
          exports: ModuleExports::All,
        }),
        imports: vec![],
        statements: vec![],
      },
    );
  }

  #[test]
  fn test_only_defs() {
    testing::assert_eq(
      parse_module,
      "func a = a + 1",
      Module {
        header: None,
        imports: vec![],
        statements: vec![Statement::Function(Definition {
          header: None,
          name: "func".to_string(),
          patterns: vec![Pattern::Var((0, 0), "a".to_string())],
          expression: Expression::OperatorChain(
            (0, 0),
            vec![
              Expression::Ref((0, 0), "a".to_string()),
              Expression::Literal((0, 0), Literal::Int(1)),
            ],
            vec!["+".to_string()],
          ),
        })],
      },
    );
  }

  #[test]
  fn test_module_exports() {
    testing::assert_eq(
      parse_module,
      "module Main (List, Maybe) where",
      Module {
        header: Some(ModuleHeader {
          name: "Main".to_string(),
          exports: ModuleExports::Just(vec![
            ModuleExport::Type("List".to_string()),
            ModuleExport::Type("Maybe".to_string()),
          ]),
        }),
        imports: vec![],
        statements: vec![],
      },
    );
  }

  #[test]
  fn test_module_imports() {
    testing::assert_eq(
      parse_module,
      "import Main (..)",
      Module {
        header: None,
        imports: vec![ModuleImport {
          path: vec!["Main".to_string()],
          exports: Some(ModuleExports::All),
          alias: None,
        }],
        statements: vec![],
      },
    );
  }

  #[test]
  fn test_module_imports_complex() {
    let code = indoc! {"
      import Core (..)
      import Util
      import Util (..)
      import Util as U
      import Util (Enum, Sides(..), UpDown(Up, Down), map)
    "};

    testing::assert_eq(
      parse_module,
      code,
      Module {
        header: None,
        imports: vec![
          ModuleImport {
            path: vec!["Core".to_string()],
            exports: Some(ModuleExports::All),
            alias: None,
          },
          ModuleImport {
            path: vec!["Util".to_string()],
            exports: None,
            alias: None,
          },
          ModuleImport {
            path: vec!["Util".to_string()],
            exports: Some(ModuleExports::All),
            alias: None,
          },
          ModuleImport {
            path: vec!["Util".to_string()],
            exports: None,
            alias: Some("U".to_string()),
          },
          ModuleImport {
            path: vec!["Util".to_string()],
            exports: Some(ModuleExports::Just(vec![
              ModuleExport::Type("Enum".to_string()),
              ModuleExport::Adt("Sides".to_string(), AdtExports::All),
              ModuleExport::Adt(
                "UpDown".to_string(),
                AdtExports::Just(vec!["Up".to_string(), "Down".to_string()]),
              ),
              ModuleExport::Function("map".to_string()),
            ])),
            alias: None,
          },
        ],
        statements: vec![],
      },
    );
  }
}
