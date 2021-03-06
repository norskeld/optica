#![allow(clippy::module_inception)]
#![feature(iter_intersperse)]

pub(crate) mod ast;
pub mod cli;
pub mod errors;
pub(crate) mod intrinsics;
pub(crate) mod lexer;
pub(crate) mod loader;
pub(crate) mod parser;
pub mod runtime;
pub(crate) mod source;
pub(crate) mod typechecker;
pub(crate) mod types;
pub mod utils;
