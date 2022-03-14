use nom::Err as NomError;
use nom::error::Error as NomWrappedError;
use nom::character;

use crate::errors::{CordError, LexicalError};
use crate::source::{SourceCode, Span};
use super::{SpannedToken, Token};
use super::parser;

pub struct Lexer {
  code: SourceCode,
  pos: usize,
}

impl Lexer {
  pub fn new(code: &SourceCode) -> Self {
    Lexer {
      code: code.clone(),
      pos: 0,
    }
  }

  pub fn lex(&mut self) -> Result<Vec<SpannedToken>, CordError> {
    let (tokens, errors) = self.read_all();

    match errors.len() {
      | 0 => Ok(tokens),
      | 1 => Err(errors.into_iter().next().unwrap()),
      | _ => Err(CordError::Collection(errors)),
    }
  }

  pub fn read_all(&mut self) -> (Vec<SpannedToken>, Vec<CordError>) {
    let mut errors = vec![];
    let mut tokens = vec![];

    while self.pos < self.code.as_bytes().len() && self.byte(0) != b'\0' {
      match self.read_next() {
        | Ok((span, token)) => tokens.push(SpannedToken { span, token }),
        | Err(error) => errors.push(error),
      }
    }

    match self.read_next() {
      | Ok((span, token)) => tokens.push(SpannedToken { span, token }),
      | Err(error) => errors.push(error),
    }

    (tokens, errors)
  }

  pub fn read_next(&mut self) -> Result<(Span, Token), CordError> {
    if self.pos >= self.code.len() {
      return Ok(((self.pos as u32, self.pos as u32), Token::Eof));
    }

    let start = self.pos as u32;
    let opt = self.trim_spaces();

    if let Some(token) = opt {
      return Ok(((start, self.pos as u32), token));
    }

    let start = self.pos as u32;
    let remaining_bytes = &self.code.as_bytes()[self.pos..];
    let result = parser::token(remaining_bytes);

    match result {
      | Ok((rest, token)) => {
        // Unary minus exception.
        let mut should_be_prefix_minus = false;

        if let Token::BinaryOperator(op) = &token {
          if op == "-" {
            let cursor = self.pos.checked_sub(1).unwrap_or(0);
            let byte = self.code.as_bytes()[cursor];

            let after_space = cursor == 0 || character::is_space(byte);
            let before_space = character::is_space(rest[0]);

            // space     (-) space      -> binary
            // not-space (-) space      -> binary
            // space     (-) not-space  -> unary
            // not-space (-) not-space  -> binary
            should_be_prefix_minus = after_space && !before_space;
          }
        };

        let result_token = if should_be_prefix_minus {
          Token::PrefixMinus
        } else {
          token
        };

        let real = self.code.as_bytes().len();
        let consumed = (real - start as usize) - rest.len();

        self.pos += consumed;

        Ok(((start, self.pos as u32), result_token))
      },
      | Err(e) => {
        let result = match e {
          | NomError::Incomplete(_) => Err(CordError::Lexer(
            self.code.clone(),
            LexicalError::ReachedEnd {
              pos: self.pos as u32,
            },
          )),
          | NomError::Error(NomWrappedError { input, .. })
          | NomError::Failure(NomWrappedError { input, .. }) => {
            let new_pos = self.code.as_str().len() - input.len();

            Err(CordError::Lexer(
              self.code.clone(),
              LexicalError::UnableToLex {
                span: (start as u32, new_pos as u32),
              },
            ))
          },
        };

        // We ignore the current character and try to tokenize the rest of characters.
        self.pos += 1;

        result
      },
    }
  }

  fn byte(&self, ptr: usize) -> u8 {
    self.code.as_bytes()[self.pos + ptr]
  }

  fn trim_spaces(&mut self) -> Option<Token> {
    let mut is_eol = false;
    let mut pos = 0;

    while self.byte(pos) == b' ' || self.byte(pos) == b'\n' {
      is_eol |= self.byte(pos) == b'\n';
      pos += 1;
    }

    if is_eol {
      let mut indentation = 0;

      for index in 0..pos {
        if self.byte(pos - index - 1) == b'\n' {
          break;
        }

        indentation += 1;
      }

      let token = Token::Indent(indentation);
      self.pos += pos;

      return Some(token);
    }

    self.pos += pos;
    self.trim_comments()
  }

  fn trim_comments(&mut self) -> Option<Token> {
    let offset = self.trim_multiline_comments();

    if offset == 0 {
      self.trim_line_comments()
    } else {
      self.pos += offset;
      self.trim_spaces()
    }
  }

  fn trim_multiline_comments(&mut self) -> usize {
    let mut nesting = 0;
    let mut offset = 0;

    loop {
      if self.byte(offset) == b'{' && self.byte(offset + 1) == b'-' {
        nesting += 1;
      }

      if nesting == 0 {
        break;
      }

      if self.byte(offset) == b'-' && self.byte(offset + 1) == b'}' {
        nesting -= 1;
        offset += 2;

        if nesting == 0 {
          break;
        }
      }

      offset += 1;
    }

    offset
  }

  fn trim_line_comments(&mut self) -> Option<Token> {
    // Line starts with --
    if self.byte(0) == b'-' && self.byte(1) == b'-' {
      let mut pos = 2;

      // Line ends with \n or \r\n
      while self.byte(pos) != b'\n' && self.byte(pos) != b'\r' && self.byte(pos) != b'\0' {
        pos += 1;
      }

      self.pos += pos;
      self.trim_spaces()
    } else {
      None
    }
  }
}

#[cfg(test)]
mod tests {
  use indoc::indoc;

  use crate::source::SourceCode;
  use crate::utils::vec::VecExt;
  use super::Lexer;
  use super::{SpannedToken, Token};

  fn tokens(code: &str) -> Vec<Token> {
    match Lexer::new(&SourceCode::from_str(code)).lex() {
      | Ok(tokens) => tokens.map(|SpannedToken { token, .. }| token.clone()),
      | Err(err) => panic!("{:?}", err),
    }
  }

  #[test]
  fn test_tokens() {
    let code = "identifier0,42.42";

    assert_eq!(
      tokens(code),
      vec![
        Token::Ident("identifier0".to_string()),
        Token::Comma,
        Token::LitFloat(42.42),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_multiline_comment() {
    let code = "1 + {- Block comment -} 2";

    assert_eq!(
      tokens(code),
      vec![
        Token::LitInt(1),
        Token::BinaryOperator("+".to_string()),
        Token::LitInt(2),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_multiline_comment_recursive() {
    let code = "1 + {- Nested {- block -} Comment -} 2";

    assert_eq!(
      tokens(code),
      vec![
        Token::LitInt(1),
        Token::BinaryOperator("+".to_string()),
        Token::LitInt(2),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_line_comment() {
    let code = indoc! {"
      1 -- This is one.
      2 -- This is two.
      3
    "};

    assert_eq!(
      tokens(code.trim_end()),
      vec![
        Token::LitInt(1),
        Token::Indent(0),
        Token::LitInt(2),
        Token::Indent(0),
        Token::LitInt(3),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_identifiers() {
    let code = "ident, _ident, identWith0, ident_underscored";

    assert_eq!(
      tokens(code),
      vec![
        Token::Ident("ident".to_string()),
        Token::Comma,
        Token::Underscore,
        Token::Ident("ident".to_string()),
        Token::Comma,
        Token::Ident("identWith0".to_string()),
        Token::Comma,
        Token::Ident("ident_underscored".to_string()),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_indentation_token() {
    let code = indoc! {"
      meaning =
        42

      life =
        42
    "};

    assert_eq!(
      tokens(code.trim_end()),
      vec![
        Token::Ident("meaning".to_string()),
        Token::Equals,
        Token::Indent(2),
        Token::LitInt(42),
        Token::Indent(0),
        Token::Ident("life".to_string()),
        Token::Equals,
        Token::Indent(2),
        Token::LitInt(42),
        Token::Eof
      ]
    );
  }

  #[test]
  fn prefix_minus_edge_case() {
    let code = "(+), (-), (*)";

    assert_eq!(
      tokens(code),
      vec![
        Token::LeftParen,
        Token::BinaryOperator("+".to_string()),
        Token::RightParen,
        Token::Comma,
        Token::LeftParen,
        Token::BinaryOperator("-".to_string()),
        Token::RightParen,
        Token::Comma,
        Token::LeftParen,
        Token::BinaryOperator("*".to_string()),
        Token::RightParen,
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_unary_minus() {
    let code = "n-1";

    assert_eq!(
      tokens(code),
      vec![
        Token::Ident("n".to_string()),
        Token::BinaryOperator("-".to_string()),
        Token::LitInt(1),
        Token::Eof
      ]
    );
  }

  #[test]
  fn test_complex() {
    let code = indoc! {"
      module Main ((<<), composeL) where

      data Bool = True | False

      type Int = Int
      type Float = Float

      infixr 0 (|>) = apR
      infixl 0 (<|) = apL

      apR : a -> (a -> b) -> b
      apR x f = f x

      apL : (a -> b) -> a -> b
      apL f x = f x
    "};

    assert_eq!(
      tokens(code),
      vec![
        // Module.
        Token::ModuleKw,
        Token::UpperIdent("Main".to_string()),
        Token::LeftParen,
        Token::LeftParen,
        Token::BinaryOperator("<<".to_string()),
        Token::RightParen,
        Token::Comma,
        Token::Ident("composeL".to_string()),
        Token::RightParen,
        Token::WhereKw,
        // Data.
        Token::Indent(0),
        Token::DataKw,
        Token::UpperIdent("Bool".to_string()),
        Token::Equals,
        Token::UpperIdent("True".to_string()),
        Token::Pipe,
        Token::UpperIdent("False".to_string()),
        // Type alias.
        Token::Indent(0),
        Token::TypeKw,
        Token::UpperIdent("Int".to_string()),
        Token::Equals,
        Token::UpperIdent("Int".to_string()),
        // Type alias.
        Token::Indent(0),
        Token::TypeKw,
        Token::UpperIdent("Float".to_string()),
        Token::Equals,
        Token::UpperIdent("Float".to_string()),
        // Infix right.
        Token::Indent(0),
        Token::InfixRightKw,
        Token::LitInt(0),
        Token::LeftParen,
        Token::BinaryOperator("|>".to_string()),
        Token::RightParen,
        Token::Equals,
        Token::Ident("apR".to_string()),
        // Infix left.
        Token::Indent(0),
        Token::InfixLeftKw,
        Token::LitInt(0),
        Token::LeftParen,
        Token::BinaryOperator("<|".to_string()),
        Token::RightParen,
        Token::Equals,
        Token::Ident("apL".to_string()),
        // Function definition.
        Token::Indent(0),
        Token::Ident("apR".to_string()),
        Token::Colon,
        Token::Ident("a".to_string()),
        Token::RightArrow,
        Token::LeftParen,
        Token::Ident("a".to_string()),
        Token::RightArrow,
        Token::Ident("b".to_string()),
        Token::RightParen,
        Token::RightArrow,
        Token::Ident("b".to_string()),
        Token::Indent(0),
        Token::Ident("apR".to_string()),
        Token::Ident("x".to_string()),
        Token::Ident("f".to_string()),
        Token::Equals,
        Token::Ident("f".to_string()),
        Token::Ident("x".to_string()),
        // Function definition.
        Token::Indent(0),
        Token::Ident("apL".to_string()),
        Token::Colon,
        Token::LeftParen,
        Token::Ident("a".to_string()),
        Token::RightArrow,
        Token::Ident("b".to_string()),
        Token::RightParen,
        Token::RightArrow,
        Token::Ident("a".to_string()),
        Token::RightArrow,
        Token::Ident("b".to_string()),
        Token::Indent(0),
        Token::Ident("apL".to_string()),
        Token::Ident("f".to_string()),
        Token::Ident("x".to_string()),
        Token::Equals,
        Token::Ident("f".to_string()),
        Token::Ident("x".to_string()),
        Token::Indent(0),
        Token::Eof
      ]
    );
  }
}
