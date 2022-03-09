#![allow(clippy::module_inception)]
#![allow(dead_code)]

pub(crate) mod ast;
pub(crate) mod errors;
pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod source;
#[macro_use]
pub(crate) mod utils;
