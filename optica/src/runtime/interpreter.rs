use std::borrow::Borrow;
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::ast::typed::{
  Adt, AdtVariant, Declaration, Function, TypedDefinition, TypedExpression, TypedLet, TypedPattern,
  Value,
};
use crate::ast::untyped::Type;
use crate::errors::{InterpreterError, LangError, Wrappable};
use crate::intrinsics;
use crate::loader::{AnalyzedModule, RuntimeModule};
use super::Stack;

#[derive(Clone, Debug)]
pub struct Interpreter {
  pub stack: Stack,
}

impl Interpreter {
  pub fn new() -> Self {
    Interpreter {
      stack: Stack::new(),
    }
  }

  pub fn adt_value(&mut self, adt_name: &str, arguments: &[Value]) -> Result<Value, LangError> {
    let ctor = self
      .find_value(adt_name)
      .ok_or_else(|| InterpreterError::MissingDefinition(adt_name.to_string()).wrap())?;

    self.apply_function(ctor, arguments)
  }

  pub fn find_value(&mut self, name: &str) -> Option<Value> {
    self.stack.find(name)
  }

  pub fn true_value(&mut self) -> Value {
    self
      .eval_expression(&TypedExpression::Ref(
        (0, 0),
        ast::type_bool(),
        "True".to_string(),
      ))
      .unwrap()
  }

  pub fn false_value(&mut self) -> Value {
    self
      .eval_expression(&TypedExpression::Ref(
        (0, 0),
        ast::type_bool(),
        "False".to_string(),
      ))
      .unwrap()
  }

  pub fn eval_expression(&mut self, expr: &TypedExpression) -> Result<Value, LangError> {
    match expr {
      | TypedExpression::Ref(_, _, name) => {
        let stack_value = self.stack.find(name);

        match stack_value {
          | Some(value) => Ok(value),
          | None => Err(InterpreterError::MissingDefinition(name.clone()).wrap()),
        }
      },
      | TypedExpression::Const(_, _, value) => Ok(value.clone()),
      | TypedExpression::Tuple(_, _, items) => {
        let values = items
          .iter()
          .map(|expr| self.eval_expression(expr))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::Tuple(values))
      },
      | TypedExpression::List(_, _, items) => {
        let values = items
          .iter()
          .map(|expr| self.eval_expression(expr))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::List(values))
      },
      | TypedExpression::If(_, _, cond, then_, else_) => {
        let cond = self.eval_expression(cond)?;

        // TODO: This should be investigated.
        let value = if let Value::Function { args, function, .. } = &cond {
          self.exec_function(function, args.to_vec())?
        } else {
          cond
        };

        // TODO: Make `Bool` internal type and value.
        match &value {
          | Value::Adt(ref name, ref values, _) => {
            if name == "True" && values.is_empty() {
              self.eval_expression(then_)
            } else if name == "False" && values.is_empty() {
              self.eval_expression(else_)
            } else {
              Err(InterpreterError::InvalidIfCondition(value.clone()).wrap())
            }
          },
          | _ => Err(InterpreterError::InvalidIfCondition(value.clone()).wrap()),
        }
      },
      | TypedExpression::Case(_, _, cond, branches) => {
        let cond_val = self.eval_expression(cond)?;

        for (patt, expr) in branches {
          if matches_pattern(patt, &cond_val) {
            return self.eval_expression(expr);
          }
        }

        Err(
          InterpreterError::CaseExpressionNonExhaustive(
            cond_val,
            branches
              .into_iter()
              .map(|(p, _)| p.clone())
              .collect::<Vec<_>>(),
          )
          .wrap(),
        )
      },
      | TypedExpression::Lambda(_, ty, pattern, expr) => Ok(Self::create_lambda_closure(
        &mut self.stack,
        ty,
        pattern,
        expr,
      )),
      | TypedExpression::Application(_, _, function, input) => {
        let function = self.eval_expression(function)?;
        let input = self.eval_expression(input)?;

        self.apply(function, input)
      },
      | TypedExpression::Let(_, _, let_defs, let_body) => {
        for let_def in let_defs {
          match let_def {
            | TypedLet::Definition(def) => {
              let (name, value) = self.eval_definition(def);
              let value = self.eval_constant(value)?;

              self.stack.add(&name, value);
            },
            // TODO: Let patterns.
            | _ => todo!(),
          }
        }

        self.eval_expression(let_body)
      },
    }
  }

  pub fn eval_declaration(&mut self, decl: &Declaration) -> Result<Option<Value>, LangError> {
    if let Declaration::Definition(_, definition) = decl {
      let (name, value) = self.eval_definition(definition);
      let value = self.eval_constant(value)?;

      self.stack.add(&name, value.clone());

      Ok(Some(value))
    } else {
      Ok(None)
    }
  }

  fn eval_definition(&mut self, def: &TypedDefinition) -> (String, Value) {
    let name = def.name.clone();
    let value = Self::create_function_closure(&mut self.stack, def);

    self.stack.add(&name, value.clone());

    (name, value)
  }

  pub fn eval_constants(&mut self, module: RuntimeModule) -> Result<RuntimeModule, LangError> {
    let RuntimeModule {
      name,
      definitions,
      imports,
    } = module;

    let mut const_defs = HashMap::new();

    for (name, value) in definitions.into_iter() {
      const_defs.insert(name, self.eval_constant(value)?);
    }

    Ok(RuntimeModule {
      name,
      definitions: const_defs,
      imports,
    })
  }

  fn eval_constant(&mut self, value: Value) -> Result<Value, LangError> {
    let constant = match &value {
      | Value::Function {
        arity, function, ..
      } if *arity == 0 => Some(self.exec_function(function.borrow(), vec![])?),
      | _ => None,
    };

    Ok(constant.unwrap_or(value))
  }

  pub fn eval_module(
    &mut self,
    modules: &HashMap<String, RuntimeModule>,
    module: &AnalyzedModule,
  ) -> Result<RuntimeModule, LangError> {
    let mut definitions = HashMap::new();

    for import in &module.imports {
      let module = modules
        .get(&import.source)
        .ok_or_else(|| InterpreterError::MissingModule(vec![import.source.to_string()]).wrap())?;

      let value = module.definitions.get(&import.source_name).ok_or_else(|| {
        InterpreterError::MissingDefinition(import.source_name.to_string()).wrap()
      })?;

      self.stack.add(&import.destination_name, value.clone());
    }

    for decl in &module.declarations {
      match decl {
        | Declaration::Definition(_, def) => {
          let (name, value) = self.eval_definition(def);
          definitions.insert(name, value);
        },
        | Declaration::Adt(_, adt) => {
          for variant in &adt.variants {
            let (name, value) = self.eval_adt_variant(adt.clone(), variant);
            definitions.insert(name, value);
          }
        },
        | _ => {},
      }
    }

    Ok(RuntimeModule {
      name: module.name.to_string(),
      definitions,
      imports: module.imports.clone(),
    })
  }

  fn eval_adt_variant(&mut self, adt: Arc<Adt>, variant: &AdtVariant) -> (String, Value) {
    let name = variant.name.clone();
    let value = intrinsics::adt::adt_constructor(adt, variant);

    self.stack.add(&name, value.clone());

    (name, value)
  }

  pub fn apply_function(
    &mut self,
    function: Value,
    arguments: &[Value],
  ) -> Result<Value, LangError> {
    let mut value = function;

    for arg in arguments {
      value = self.apply(value, arg.clone())?;
    }

    Ok(value)
  }

  fn apply(&mut self, func_value: Value, input: Value) -> Result<Value, LangError> {
    if let Value::Function {
      arity,
      args,
      function,
    } = &func_value
    {
      let argc = args.len() as u32 + 1;

      if *arity < argc {
        return Err(
          InterpreterError::FunArgumentSizeMismatch(*arity, argc, function.clone()).wrap(),
        );
      }

      let mut arg_vec = args.clone();
      arg_vec.push(input);

      let value = if *arity == argc {
        self.exec_function(function, arg_vec)?
      } else {
        Value::Function {
          args: arg_vec,
          arity: *arity,
          function: function.clone(),
        }
      };

      Ok(value)
    } else {
      Err(InterpreterError::ExpectedFunction(func_value.clone()).wrap())
    }
  }

  fn exec_function(&mut self, func: &Function, args: Vec<Value>) -> Result<Value, LangError> {
    self.stack.enter_block();

    let result = match func {
      | Function::External { function, .. } => {
        // TODO: This probably should not map to `InterpreterError::BuiltinFunctionError` and just
        // propagate produced error.
        (function.function)(self, &args).map_err(|_| InterpreterError::BuiltinFunctionError.wrap())
      },
      | Function::Definition {
        patterns,
        expression,
        captures,
        ..
      } => {
        assert_eq!(patterns.len(), args.len());

        for (name, value) in captures {
          let value = self.eval_constant(value.clone())?;
          self.stack.add(name, value)
        }

        for (pattern, value) in patterns.iter().zip(args) {
          add_pattern_values(self, pattern, value).unwrap();
        }

        self.eval_expression(expression)
      },
    };

    self.stack.exit_block();

    result
  }

  pub fn create_lambda_closure(
    stack: &mut Stack,
    ty: &Type,
    patterns: &[TypedPattern],
    expr: &TypedExpression,
  ) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: patterns.to_owned(),
      expression: expr.clone(),
      captures: Self::extract_captures(stack, expr),
      function_type: ty.clone(),
    });

    Value::Function {
      arity: patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn create_function_closure(stack: &mut Stack, def: &TypedDefinition) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: def.patterns.clone(),
      expression: def.expression.clone(),
      captures: Self::extract_captures(stack, &def.expression),
      function_type: def.header.clone(),
    });

    Value::Function {
      arity: def.patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn extract_captures(stack: &mut Stack, expr: &TypedExpression) -> HashMap<String, Value> {
    let mut map = HashMap::new();
    Self::traverse_expr(&mut map, stack, expr);

    map
  }

  fn traverse_expr(result: &mut HashMap<String, Value>, stack: &mut Stack, expr: &TypedExpression) {
    // TODO: Avoid capturing internal definitions.
    match expr {
      | TypedExpression::Ref(_, _, name) => {
        if let Some(value) = stack.find(name) {
          result.insert(name.to_string(), value);
        }
      },
      | TypedExpression::Tuple(_, _, list) | TypedExpression::List(_, _, list) => {
        for expr in list {
          Self::traverse_expr(result, stack, expr);
        }
      },
      | TypedExpression::If(_, _, a, b, c) => {
        Self::traverse_expr(result, stack, a.as_ref());
        Self::traverse_expr(result, stack, b.as_ref());
        Self::traverse_expr(result, stack, c.as_ref());
      },
      | TypedExpression::Application(_, _, a, b) => {
        Self::traverse_expr(result, stack, a.as_ref());
        Self::traverse_expr(result, stack, b.as_ref());
      },
      | TypedExpression::Lambda(_, _, _, box_expr) => {
        Self::traverse_expr(result, stack, box_expr.as_ref());
      },
      | _ => { /* Ignored. */ },
    }
  }
}

pub fn add_pattern_values(
  interpreter: &mut Interpreter,
  pattern: &TypedPattern,
  value: Value,
) -> Result<(), InterpreterError> {
  match pattern {
    | TypedPattern::Var(_, _, name) => {
      interpreter.stack.add(name, value);
    },
    | TypedPattern::Alias(_, _, typed_pattern, name) => {
      interpreter.stack.add(name, value.clone());

      add_pattern_values(interpreter, typed_pattern, value)?;
    },
    | TypedPattern::Adt(_, _, _, typed_patterns) => {
      if let Value::Adt(_, vars, _) = &value {
        for (typed_pattern, val) in typed_patterns.iter().zip(vars) {
          add_pattern_values(interpreter, typed_pattern, val.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedAdt(value.clone()));
      }
    },
    | TypedPattern::Tuple(_, _, typed_patterns) => {
      if let Value::Tuple(vars) = &value {
        for (typed_pattern, val) in typed_patterns.iter().zip(vars) {
          add_pattern_values(interpreter, typed_pattern, val.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedTuple(value.clone()));
      }
    },
    | TypedPattern::List(_, _, typed_patterns) => {
      if let Value::List(vars) = &value {
        for (typed_pattern, val) in typed_patterns.iter().zip(vars) {
          add_pattern_values(interpreter, typed_pattern, val.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedList(value.clone()));
      }
    },
    | TypedPattern::LitInt(_, _) => {},
    | TypedPattern::LitString(_, _) => {},
    | TypedPattern::LitChar(_, _) => {},
    | TypedPattern::Wildcard(_) => {},
    | TypedPattern::Unit(_) => {},
    | TypedPattern::BinaryOperator(_, _, op, a, b) => {
      if op == "::" {
        if let Value::List(vars) = &value {
          if vars.is_empty() {
            return Err(InterpreterError::ExpectedNonEmptyList(value.clone()));
          }

          let first = vars[0].clone();
          let mut rest: Vec<Value> = Vec::new();

          for item in vars.iter().skip(1) {
            rest.push(item.clone());
          }

          add_pattern_values(interpreter, a, first)?;
          add_pattern_values(interpreter, b, Value::List(rest))?;
        } else {
          return Err(InterpreterError::ExpectedList(value.clone()));
        }
      } else {
        return Err(InterpreterError::UnknownOperatorPattern(op.clone()));
      }
    },
  }

  Ok(())
}

fn matches_pattern(pattern: &TypedPattern, value: &Value) -> bool {
  match pattern {
    | TypedPattern::Var(_, _, _) => true,
    | TypedPattern::Wildcard(_) => true,
    | TypedPattern::Alias(_, _, pattern, _) => matches_pattern(pattern, value),
    | TypedPattern::Adt(_, _, pattern_ty, pattern_sub) => {
      let value_ty = value.get_type();

      if let Value::Adt(_, value_sub, _) = value {
        pattern_ty == &value_ty
          && pattern_sub
            .iter()
            .zip(value_sub)
            .all(|(pattern, value)| matches_pattern(pattern, value))
      } else {
        false
      }
    },
    | TypedPattern::Unit(_) => value == &Value::Unit,
    | TypedPattern::Tuple(_, _, pattern_sub) => {
      if let Value::Tuple(value_sub) = value {
        pattern_sub
          .iter()
          .zip(value_sub)
          .all(|(pattern, value)| matches_pattern(pattern, value))
      } else {
        false
      }
    },
    | TypedPattern::List(_, _, pattern_sub) => {
      if let Value::List(value_sub) = value {
        pattern_sub
          .iter()
          .zip(value_sub)
          .all(|(pattern, value)| matches_pattern(pattern, value))
      } else {
        false
      }
    },
    | TypedPattern::BinaryOperator(_, _, op, first, rest) => {
      assert_eq!(op.as_str(), "::");

      if let Value::List(value_sub) = value {
        if !value_sub.is_empty() {
          matches_pattern(first, &value_sub[0])
            && matches_pattern(rest, &Value::List(value_sub[1..].to_vec()))
        } else {
          false
        }
      } else {
        false
      }
    },
    | TypedPattern::LitInt(_, p) => match value {
      | Value::Int(v) => (*p) == (*v),
      | Value::Number(v) => (*p) == (*v),
      | _ => false,
    },
    | TypedPattern::LitString(_, p) => {
      if let Value::String(v) = value {
        p == v
      } else {
        false
      }
    },
    | TypedPattern::LitChar(_, p) => {
      if let Value::Char(v) = value {
        *p == *v
      } else {
        false
      }
    },
  }
}

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::super::testing;
  use super::*;

  #[test]
  fn test_unit() {
    let expression = testing::typed_expression("()");
    let mut interpreter = Interpreter::new();

    assert_eq!(interpreter.eval_expression(&expression), Ok(Value::Unit));
  }

  #[test]
  fn test_list() {
    let expression = testing::typed_expression("[1, 2, 3]");
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::List(vec![
        Value::Number(1),
        Value::Number(2),
        Value::Number(3),
      ]))
    );
  }

  #[test]
  fn test_number() {
    let expression = testing::typed_expression("42");
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Number(42))
    );
  }

  #[test]
  fn test_float() {
    let expression = testing::typed_expression("42.42");
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Float(42.42))
    );
  }

  #[test]
  fn test_char() {
    let expression = testing::typed_expression("'x'");
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Char('x'))
    );
  }

  #[test]
  fn test_tuple() {
    let expression = testing::typed_expression(r#"(42, "Life", [40, 2])"#);
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Tuple(vec![
        Value::Number(42),
        Value::String("Life".to_string()),
        Value::List(vec![Value::Number(40), Value::Number(2)])
      ]))
    );
  }
}
