const ALPHABET: &[u8] = b"abcdefghijklmnopqrstuvwxyz";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Generator {
  last: u32,
}

impl Generator {
  pub fn new() -> Self {
    Self { last: 0 }
  }

  pub fn next(&mut self) -> String {
    let mut result = vec![];
    let mut index = self.last as usize;
    let radix = ALPHABET.len();

    self.last += 1;

    loop {
      let remainder = index % radix;
      index /= radix;

      result.push(ALPHABET[remainder as usize]);

      if index == 0 {
        break;
      }
    }

    let next = result.into_iter().rev().collect::<Vec<_>>();

    String::from_utf8_lossy(&next).to_string()
  }

  pub fn next_with_prefix(&mut self, prefix: &str) -> String {
    let prefix = prefix.to_string();
    let index = self.last as usize;
    self.last += 1;

    if index == 0 {
      prefix
    } else {
      format!("{prefix}{index}")
    }
  }
}

impl Default for Generator {
  fn default() -> Self {
    Self::new()
  }
}
