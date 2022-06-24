use std::collections::HashMap;
use std::sync::Arc;

use super::{IntrinsicModule, IntrinsicStatement};
use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::loader::*;
use crate::types;
use crate::utils::function;

/// Builds an intrinsic function with given name, type signature, and a function pointer with
/// actual implementation of that intrinsic.
pub fn create_intrinsic(
  name: &'static str,
  signature: &'static str,
  intrinsic: IntrinsicFn,
) -> Result<IntrinsicStatement, LangError> {
  let function_type = signature.parse::<Type>()?;

  let function = Value::Function {
    arity: function::arguments_count(&function_type),
    args: vec![],
    function: Arc::new(Function::Intrinsic {
      id: types::function_id(),
      function: IntrinsicFunction {
        name: name.to_string(),
        function: intrinsic,
      },
      function_type: function_type.clone(),
    }),
  };

  Ok((name.to_owned(), function_type, function))
}

/// Creates a module of intrinsic functions with given name and function pointer.
pub fn create_module(name: &'static str, intrinsics: Vec<IntrinsicStatement>) -> IntrinsicModule {
  let module_name = format!("Intrinsics.{name}");
  let mut statements = vec![];
  let mut definitions = HashMap::new();

  for (name, ty, value) in intrinsics {
    statements.push(TypedStatement::Port(name.to_string(), ty));
    definitions.insert(name.to_string(), value);
  }

  (
    module_name.clone(),
    TypedModule {
      name: module_name.clone(),
      dependencies: vec![],
      imports: vec![],
      statements,
    },
    RuntimeModule {
      name: module_name.clone(),
      definitions,
      imports: vec![],
    },
    ModuleImport {
      path: vec![module_name],
      alias: None,
      exports: Some(ModuleExports::All),
    },
  )
}

/// Builds an ADT constructor function and produces a [Value].
pub fn create_adt(adt: Arc<Adt>, variant: &AdtVariant) -> Value {
  let mut function_types = vec![types::type_tag(&variant.name)];

  function_types.extend(variant.types.clone().into_iter());
  function_types.push(types::type_tag_parameterized(
    &variant.name,
    variant.types.clone(),
  ));

  let function = IntrinsicFunction {
    name: "ADT".to_string(),
    function: |_, args| {
      if let Value::Adt(var, _, adt) = &args[0] {
        let values = args.iter().skip(1).cloned().collect();
        Ok(Value::Adt(var.to_owned(), values, adt.clone()))
      } else {
        Err(InterpreterError::InternalErrorAdtCreation(args[0].clone()).wrap())
      }
    },
  };

  Value::Function {
    arity: function_types.len() as u32,
    args: vec![Value::Adt(variant.name.to_string(), vec![], adt)],
    function: Arc::new(Function::Intrinsic {
      id: types::function_id(),
      function,
      function_type: types::type_function(function_types),
    }),
  }
}
