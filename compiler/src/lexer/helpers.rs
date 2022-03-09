use crate::number::{Float, Int};

pub fn parse_int(radix: u32, digits: Vec<char>) -> Int {
  let src: String = digits.into_iter().collect();
  isize::from_str_radix(&src, radix).unwrap() as Int
}

pub fn parse_float(is_negative: bool, whole_part: Vec<char>, decimal_part: Vec<char>) -> Float {
  let int_part: String = whole_part.into_iter().collect();
  let dec_part: String = decimal_part.into_iter().collect();

  let value = format!("{}.{}", int_part, dec_part)
    .parse::<Float>()
    .unwrap();

  if is_negative {
    -value
  } else {
    value
  }
}
