use nom::branch::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use super::helpers;
use super::Token;
use crate::ast::Int;
use crate::utils;

pub type ParseResult<'a, T> = IResult<&'a [u8], T>;

// Primitives.

fn lower(input: &[u8]) -> ParseResult<'_, char> {
  one_of("abcdefghijklmnopqrstuvwxyz")(input)
}

fn upper(input: &[u8]) -> ParseResult<'_, char> {
  one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)
}

fn decimal(input: &[u8]) -> ParseResult<'_, char> {
  one_of("0123456789")(input)
}

fn hexadecimal(input: &[u8]) -> ParseResult<'_, char> {
  one_of("0123456789ABCDEFabcdef")(input)
}

fn octal(input: &[u8]) -> ParseResult<'_, char> {
  one_of("01234567")(input)
}

fn binary_operator_char(input: &[u8]) -> ParseResult<'_, char> {
  one_of(":~!@#$%^&*-+=<>/?._|")(input)
}

fn identifier_char(input: &[u8]) -> ParseResult<'_, char> {
  alt((lower, upper, decimal, one_of("_'")))(input)
}

// Non-terminals.

pub fn token(input: &[u8]) -> ParseResult<'_, Token> {
  alt((
    binary_operator,
    comma,
    back_slash,
    identifier,
    upper_identifier,
    literal,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_brace,
    right_brace,
    end_of_file,
  ))(input)
}

fn binary_operator(input: &[u8]) -> ParseResult<'_, Token> {
  map(many1(binary_operator_char), |chars: Vec<char>| {
    let ident = chars.into_iter().collect::<String>();
    let bytes = ident.as_bytes();

    match bytes {
      | b"_" => Token::Underscore,
      | b":" => Token::Colon,
      | b"." => Token::Dot,
      | b".." => Token::DoubleDot,
      | b"=" => Token::Equals,
      | b"<-" => Token::LeftArrow,
      | b"->" => Token::RightArrow,
      | b"=>" => Token::FatRightArrow,
      | b"|" => Token::Pipe,
      | _ => Token::BinaryOperator(ident),
    }
  })(input)
}

fn identifier(input: &[u8]) -> ParseResult<'_, Token> {
  map(
    tuple((lower, many0(identifier_char))),
    |(head, tail): (char, Vec<char>)| {
      let ident = utils::vec::cons(head, tail).into_iter().collect::<String>();

      let bytes = ident.as_bytes();

      match bytes {
        | b"let" => Token::LetKw,
        | b"in" => Token::InKw,
        | b"if" => Token::IfKw,
        | b"else" => Token::ElseKw,
        | b"then" => Token::ThenKw,
        | b"module" => Token::ModuleKw,
        | b"where" => Token::WhereKw,
        | b"import" => Token::ImportKw,
        | b"infixl" => Token::InfixLeftKw,
        | b"infixr" => Token::InfixRightKw,
        | b"match" => Token::MatchKw,
        | b"with" => Token::WithKw,
        | b"type" => Token::TypeKw,
        | b"data" => Token::DataKw,
        | b"as" => Token::AsKw,
        | _ => Token::Ident(ident),
      }
    },
  )(input)
}

fn upper_identifier(input: &[u8]) -> ParseResult<'_, Token> {
  map(
    tuple((upper, many0(identifier_char))),
    |(head, tail): (char, Vec<char>)| {
      Token::UpperIdent(utils::vec::cons(head, tail).into_iter().collect::<String>())
    },
  )(input)
}

fn literal(input: &[u8]) -> ParseResult<'_, Token> {
  alt((float_literal, int_literal, string_literal, char_literal))(input)
}

fn int_literal(input: &[u8]) -> ParseResult<'_, Token> {
  let decimals = map(many1(decimal), |digits| helpers::parse_int(10, digits));

  let hexadecimals = map(
    tuple((char('0'), alt((char('x'), char('X'))), many1(hexadecimal))),
    |(_, _, digits): (char, char, Vec<char>)| helpers::parse_int(16, digits),
  );

  let octals = map(
    tuple((char('0'), alt((char('o'), char('O'))), many1(octal))),
    |(_, _, digits): (char, char, Vec<char>)| helpers::parse_int(8, digits),
  );

  map(
    tuple((opt(char('-')), alt((hexadecimals, octals, decimals)))),
    |(minus, number): (Option<char>, Int)| Token::LitInt(minus.map_or_else(|| number, |_| -number)),
  )(input)
}

fn float_literal(input: &[u8]) -> ParseResult<'_, Token> {
  map(
    tuple((opt(char('-')), many0(decimal), char('.'), many1(decimal))),
    |(minus, whole_part, _, decimal_part)| {
      Token::LitFloat(helpers::parse_float(
        minus.is_some(),
        whole_part,
        decimal_part,
      ))
    },
  )(input)
}

fn char_literal(input: &[u8]) -> ParseResult<'_, Token> {
  delimited(char('\''), map(none_of("\n\'"), Token::LitChar), char('\''))(input)
}

fn string_literal(input: &[u8]) -> ParseResult<'_, Token> {
  let contents = many0(alt((none_of("\n\""), preceded(char('\\'), char('\"')))));

  map(
    tuple((char('\"'), contents, char('\"'))),
    |(_, chars, _): (char, Vec<char>, char)| {
      Token::LitString(chars.into_iter().collect::<String>())
    },
  )(input)
}

// Terminals.

fn left_paren(input: &[u8]) -> ParseResult<'_, Token> {
  map(char('('), |_| Token::LeftParen)(input)
}

fn right_paren(input: &[u8]) -> ParseResult<'_, Token> {
  map(char(')'), |_| Token::RightParen)(input)
}

fn left_bracket(input: &[u8]) -> ParseResult<'_, Token> {
  map(char('['), |_| Token::LeftBracket)(input)
}

fn right_bracket(input: &[u8]) -> ParseResult<'_, Token> {
  map(char(']'), |_| Token::RightBracket)(input)
}

fn left_brace(input: &[u8]) -> ParseResult<'_, Token> {
  map(char('{'), |_| Token::LeftBrace)(input)
}

fn right_brace(input: &[u8]) -> ParseResult<'_, Token> {
  map(char('}'), |_| Token::RightBrace)(input)
}

fn comma(input: &[u8]) -> ParseResult<'_, Token> {
  map(char(','), |_| Token::Comma)(input)
}

fn back_slash(input: &[u8]) -> ParseResult<'_, Token> {
  map(char('\\'), |_| Token::BackSlash)(input)
}

fn end_of_file(input: &[u8]) -> ParseResult<'_, Token> {
  alt((map(eof, |_| Token::Eof), map(char('\0'), |_| Token::Eof)))(input)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::assert_ok;

  #[test]
  fn test_line_string() {
    assert_ok!(
      string_literal("\"Hello World\"".as_bytes()),
      Token::LitString("Hello World".to_string())
    );
  }

  #[test]
  fn test_char() {
    assert_ok!(char_literal(b"'H'"), Token::LitChar('H'));
  }

  #[test]
  fn test_int() {
    assert_ok!(int_literal(b"0|"), Token::LitInt(0));
    assert_ok!(int_literal(b"-1|"), Token::LitInt(-1));
    assert_ok!(int_literal(b"1|"), Token::LitInt(1));
    assert_ok!(int_literal(b"99999|"), Token::LitInt(99999));
    assert_ok!(int_literal(b"-1234|"), Token::LitInt(-1234));
  }

  #[test]
  fn test_int_base() {
    assert_ok!(int_literal(b"0|"), Token::LitInt(0));
    assert_ok!(int_literal(b"0o123|"), Token::LitInt(0o123));
    assert_ok!(int_literal(b"0O123|"), Token::LitInt(0o123));
    assert_ok!(int_literal(b"0x123|"), Token::LitInt(0x123));
    assert_ok!(int_literal(b"0X123|"), Token::LitInt(0x123));
  }

  #[test]
  fn test_float() {
    assert_ok!(float_literal(b"0.0|"), Token::LitFloat(0.0));
    assert_ok!(float_literal(b"-1.0|"), Token::LitFloat(-1.0));
    assert_ok!(float_literal(b".0|"), Token::LitFloat(0.0));
    assert_ok!(float_literal(b"-.0|"), Token::LitFloat(0.0));
    assert_ok!(float_literal(b"1.2|"), Token::LitFloat(1.2));
    assert_ok!(float_literal(b"99999.0|"), Token::LitFloat(99999.0));
    assert_ok!(float_literal(b"-1234.0|"), Token::LitFloat(-1234.0));
  }

  #[test]
  fn test_binop() {
    assert_ok!(token(b"= "), Token::Equals);
    assert_ok!(token(b"== "), Token::BinaryOperator("==".to_string()));
    assert_ok!(token(b"=== "), Token::BinaryOperator("===".to_string()));
    assert_ok!(token(b"- "), Token::BinaryOperator("-".to_string()));
    assert_ok!(token(b"-- "), Token::BinaryOperator("--".to_string()));
    assert_ok!(token(b"--- "), Token::BinaryOperator("---".to_string()));
    assert_ok!(token(b". "), Token::Dot);
    assert_ok!(token(b".. "), Token::DoubleDot);
    assert_ok!(token(b"... "), Token::BinaryOperator("...".to_string()));
    assert_ok!(token(b"-> "), Token::RightArrow);
    assert_ok!(token(b"<- "), Token::LeftArrow);
    assert_ok!(token(b"=> "), Token::FatRightArrow);
    assert_ok!(token(b"<-- "), Token::BinaryOperator("<--".to_string()));
  }
}
