use std::fmt;

use crate::ast::Int;
use crate::errors::*;
use crate::lexer::Token;
use crate::source::Input;

pub fn expect(expected: Token, input: Input) -> Result<Input, ParseError> {
  if expected == input.read() {
    Ok(input.next())
  } else {
    Err(ParseError::Expected {
      span: input.span(),
      expected,
      found: input.read(),
    })
  }
}

pub fn expect_indent(expected: u32, input: Input) -> Result<Input, ParseError> {
  if expected == std::u32::MAX {
    return Ok(input);
  }

  let mut input = if let Token::Indent(found) = input.read() {
    if expected == found {
      input.next()
    } else {
      return Err(ParseError::ExpectedIndentationLevel {
        span: input.span(),
        expected,
        found,
      });
    }
  } else {
    return Err(ParseError::ExpectedIndentation {
      span: input.span(),
      found: input.read(),
    });
  };

  // Ignore all indentations in the same level.
  while let Token::Indent(found) = input.read() {
    if found == expected {
      input = input.next()
    } else {
      break;
    }
  }

  Ok(input)
}

pub fn expect_int(input: Input) -> Result<(Int, Input), ParseError> {
  if let Token::LitInt(value) = input.read() {
    Ok((value, input.next()))
  } else {
    let found = input.read();

    Err(ParseError::ExpectedInt {
      span: input.span(),
      found,
    })
  }
}

pub fn expect_ident(input: Input) -> Result<(String, Input), ParseError> {
  if let Token::Ident(name) = input.read() {
    Ok((name, input.next()))
  } else {
    Err(ParseError::ExpectedIdent {
      span: input.span(),
      found: input.read(),
    })
  }
}

pub fn expect_upper_ident(input: Input) -> Result<(String, Input), ParseError> {
  if let Token::UpperIdent(name) = input.read() {
    Ok((name, input.next()))
  } else {
    Err(ParseError::ExpectedUpperIdent {
      span: input.span(),
      found: input.read(),
    })
  }
}

pub fn expect_upper_chain(input: Input) -> Result<(String, Input), ParseError> {
  let (name, mut input) = expect_upper_ident(input)?;
  let mut names = vec![name];

  while let Token::Dot = input.read() {
    let (name, rest) = expect_upper_ident(input.next())?;

    names.push(name);
    input = rest;
  }

  let complete_name = if names.len() == 1 {
    names.into_iter().next().unwrap()
  } else {
    let mut names_iter = names.iter();
    let mut acc = String::new();

    acc.push_str(names_iter.next().unwrap());

    for name in names_iter {
      acc.push('.');
      acc.push_str(name);
    }

    acc
  };

  Ok((complete_name, input))
}

pub fn expect_binary_operator(input: Input) -> Result<(String, Input), ParseError> {
  if let Token::BinaryOperator(name) = input.read() {
    Ok((name, input.next()))
  } else if let Token::PrefixMinus = input.read() {
    Ok(("-".to_owned(), input.next()))
  } else {
    Err(ParseError::ExpectedBinaryOperator {
      span: input.span(),
      found: input.read(),
    })
  }
}

pub fn comma0<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let (first, mut input) = match func(input.clone()) {
    | Ok(pair) => pair,
    | Err(_) => {
      return Ok((vec![], input));
    },
  };

  let mut acc: Vec<T> = vec![first];

  while let Token::Comma = input.read() {
    let (next, rest) = func(input.next())?;

    acc.push(next);
    input = rest;
  }

  Ok((acc, input))
}

pub fn comma1<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
  T: fmt::Debug,
{
  let (first, mut input): (T, Input) = func(input)?;
  let mut acc: Vec<T> = vec![first];

  while let Token::Comma = input.read() {
    let (next, rest) = func(input.next())?;

    acc.push(next);
    input = rest;
  }

  Ok((acc, input))
}

pub fn many0<T, F>(func: &F, mut input: Input) -> Result<(Vec<T>, Input), ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let mut acc: Vec<T> = vec![];

  while let Ok(pair) = func(input.clone()) {
    let (result, input_next) = pair;
    input = input_next;

    acc.push(result);
  }

  Ok((acc, input))
}

pub fn many1<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let (first, mut input) = func(input)?;
  let mut acc: Vec<T> = vec![first];

  while let Ok(pair) = func(input.clone()) {
    let (result, input_next) = pair;
    input = input_next;

    acc.push(result);
  }

  Ok((acc, input))
}

pub fn pipe1<T, F>(func: &F, input: Input) -> Result<(Vec<T>, Input), ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let (first, mut input) = func(input)?;
  let mut acc: Vec<T> = vec![first];

  while let Token::Pipe = input.read() {
    let (next, rest) = func(input.next())?;
    acc.push(next);
    input = rest;
  }

  Ok((acc, input))
}

pub fn optional<T, F>(func: &F, input: Input) -> (Option<T>, Input)
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  match func(input.clone()) {
    | Ok((result, input)) => (Some(result), input),
    | Err(_) => (None, input),
  }
}

pub fn complete<T, F>(func: &F, input: Input) -> Result<T, ParseError>
where
  F: Fn(Input) -> Result<(T, Input), ParseError>,
{
  let (result, mut input): (T, Input) = func(input)?;

  // Skip empty lines at the end.
  while let Token::Indent(_) = input.read() {
    input = input.next();
  }

  expect(Token::Eof, input)?;

  Ok(result)
}

// Readers.

pub fn read_optional_indent(input: Input) -> u32 {
  if let Token::Indent(found) = input.read_forced() {
    found
  } else {
    std::u32::MAX
  }
}
