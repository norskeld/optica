#![allow(clippy::module_inception)]
#![allow(dead_code)]

pub(crate) mod analysis;
pub(crate) mod ast;
pub(crate) mod errors;
pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod parser;
pub(crate) mod runtime;
pub(crate) mod source;
pub(crate) mod utils;
