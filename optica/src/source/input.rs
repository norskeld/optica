use std::rc::Rc;

use crate::lexer::{SpannedToken, Token};
use crate::source::{SourceCode, Span};

#[derive(PartialEq, Debug, Clone)]
pub struct Input {
  raw: Rc<RawInput>,
  cursor: usize,
  levels: Rc<Vec<u32>>,
}

#[derive(PartialEq, Debug, Clone)]
struct RawInput {
  string: SourceCode,
  tokens: Vec<SpannedToken>,
}

impl Input {
  pub fn new(string: SourceCode, tokens: Vec<SpannedToken>) -> Self {
    Input {
      raw: Rc::new(RawInput { string, tokens }),
      cursor: 0,
      levels: Rc::new(vec![0]),
    }
  }

  pub fn next(&self) -> Input {
    let cursor = self.skip_indent();

    Input {
      raw: Rc::clone(&self.raw),
      cursor: cursor + 1,
      levels: self.levels.clone(),
    }
  }

  pub fn read(&self) -> Token {
    let cursor = self.skip_indent();
    self.raw.tokens[cursor].token.clone()
  }

  pub fn read_forced(&self) -> Token {
    let cursor = self.cursor.min(self.raw.tokens.len() - 1);
    self.raw.tokens[cursor].token.clone()
  }

  fn skip_indent(&self) -> usize {
    let mut cursor = self.cursor;

    // Ignore indentation that doesn't match the current level.
    if cursor < self.raw.tokens.len() {
      while let Token::Indent(level) = &self.raw.tokens[cursor].token {
        if !self.levels.contains(level) {
          cursor += 1;
        } else {
          break;
        }
      }
    }

    cursor.min(self.raw.tokens.len() - 1)
  }

  pub fn enter_level(&self, level: u32) -> Input {
    let mut copy = (&(*self.levels)).clone();
    copy.push(level);

    Input {
      raw: Rc::clone(&self.raw),
      levels: Rc::new(copy),
      cursor: self.cursor,
    }
  }

  pub fn exit_level(&self, level: u32) -> Input {
    let mut copy = (&(*self.levels)).clone();

    if let Some(index) = copy.iter().position(|lv| *lv == level) {
      copy.remove(index);
    }

    Input {
      raw: Rc::clone(&self.raw),
      levels: Rc::new(copy),
      cursor: self.cursor,
    }
  }

  pub fn span(&self) -> Span {
    let cursor = self.skip_indent();
    self.raw.tokens[cursor].span
  }

  pub fn pos(&self) -> u32 {
    let cursor = self.skip_indent();
    let (pos, _) = self.raw.tokens[cursor].span;

    pos
  }

  pub fn pos_end(&self) -> u32 {
    let cursor = (self.cursor as i32 - 1).max(0) as usize;
    let (_, pos) = self.raw.tokens[cursor].span;

    pos
  }
}

// TODO: impl Display for Input
