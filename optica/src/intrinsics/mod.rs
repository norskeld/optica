use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::loader::*;

pub(crate) mod conversions;
pub(crate) mod helpers;
pub(crate) mod modules;

pub type IntrinsicStatement = (String, Type, Value);
pub type IntrinsicModule = (String, TypedModule, RuntimeModule, ModuleImport);

pub fn modules() -> Result<Vec<IntrinsicModule>, LangError> {
  Ok(vec![modules::math::module()?])
}
