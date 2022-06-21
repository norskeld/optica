use std::sync::Arc;

use crate::ast;
use crate::ast::typed::*;
use crate::errors::*;

pub fn adt_constructor(adt: Arc<Adt>, variant: &AdtVariant) -> Value {
  let mut func_types = vec![ast::type_tag_args(&variant.name, vec![])];

  func_types.extend(variant.types.clone().into_iter());
  func_types.push(ast::type_tag_args(&variant.name, variant.types.clone()));

  let external_function = ExternalFunction {
    name: "ADT constructor".to_string(),
    function: |_, args| {
      if let Value::Adt(var, _, adt) = &args[0] {
        let mut vals: Vec<Value> = vec![];

        for argument in args.iter().skip(1) {
          vals.push(argument.clone());
        }

        Ok(Value::Adt(var.to_owned(), vals, adt.clone()))
      } else {
        Err(InterpreterError::InternalErrorAdtCreation(args[0].clone()).wrap())
      }
    },
  };

  Value::Function {
    arity: func_types.len() as u32,
    args: vec![Value::Adt(variant.name.to_string(), vec![], adt)],
    function: Arc::new(Function::External {
      id: ast::function_id(),
      function: external_function,
      function_type: ast::type_func(func_types),
    }),
  }
}
