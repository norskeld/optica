#![allow(clippy::module_inception)]

pub(crate) mod ast;
pub mod errors;
pub(crate) mod intrinsics;
pub(crate) mod lexer;
pub(crate) mod loader;
pub(crate) mod parser;
pub mod runtime;
pub(crate) mod source;
pub(crate) mod typechecker;
pub(crate) mod utils;
