//! Testing utilities for parser(s).

use std::fmt::Debug;

use super::combinators;
use crate::errors::*;
use crate::lexer::Lexer;
use crate::source::{Input, SourceCode};

fn create_input(c: &str) -> Input {
  let code = SourceCode::from_str(c);
  let tokens = Lexer::new(&code).lex().expect("Lexer error");

  Input::new(code, tokens)
}

pub fn assert_eq<F, T: Debug + PartialEq>(func: F, code: &str, value: T)
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let input = create_input(code);
  let result = combinators::complete(&func, input);

  match result {
    | Ok(res) => {
      println!("Value: {:?}", res);
      assert_eq!(value, res);
    },
    | Err(error) => {
      println!(
        "Error: {:?}\n",
        LangError::Parser(SourceCode::from_str(code), error)
      );

      panic!();
    },
  }
}

pub fn is_ok<F, T: Debug>(func: F, code: &str)
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let input = create_input(code);
  let result = combinators::complete(&func, input);

  match result {
    | Ok(res) => {
      println!("Value: {:?}", res);
    },
    | Err(error) => {
      println!(
        "Error: {:?}\n",
        LangError::Parser(SourceCode::from_str(code), error)
      );

      panic!();
    },
  }
}

pub fn is_err<F, T: Debug>(func: F, code: &str)
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let input = create_input(code);
  let result = combinators::complete(&func, input);

  match result {
    | Ok(res) => {
      println!("Unexpected success: {:?}\n", res);
      panic!();
    },
    | Err(error) => {
      println!(
        "Error: {:?}\n",
        LangError::Parser(SourceCode::from_str(code), error)
      );
    },
  }
}
