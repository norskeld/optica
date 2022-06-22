use std::sync::Arc;

use crate::ast::typed::*;
use crate::errors::*;
use crate::types;

pub fn adt_constructor(adt: Arc<Adt>, variant: &AdtVariant) -> Value {
  let mut function_types = vec![types::type_tag(&variant.name)];

  function_types.extend(variant.types.clone().into_iter());
  function_types.push(types::type_tag_parameterized(
    &variant.name,
    variant.types.clone(),
  ));

  let function = IntrinsicFunction {
    name: "ADT constructor".to_string(),
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
