use std::borrow::Borrow;
use std::collections::HashMap;
use std::sync::Arc;

use super::Stack;
use crate::ast;
use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::errors::*;
use crate::intrinsics;
use crate::loader::*;

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

  pub fn eval_expression(
    &mut self,
    typed_expression: &TypedExpression,
  ) -> Result<Value, LangError> {
    match typed_expression {
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
      | TypedExpression::Match(_, _, cond, branches) => {
        let cond_val = self.eval_expression(cond)?;

        for (pattern, expression) in branches {
          if matches_pattern(pattern, &cond_val) {
            return self.eval_expression(expression);
          }
        }

        Err(
          InterpreterError::CaseExpressionNonExhaustive(
            cond_val,
            branches
              .iter()
              .map(|(pattern, _)| pattern.clone())
              .collect::<Vec<_>>(),
          )
          .wrap(),
        )
      },
      | TypedExpression::Lambda(_, ty, pattern, expr) => {
        Ok(Self::create_lambda_closure(
          &mut self.stack,
          ty,
          pattern,
          expr,
        ))
      },
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

  pub fn eval_statement(
    &mut self,
    typed_statement: &TypedStatement,
  ) -> Result<Option<Value>, LangError> {
    if let TypedStatement::Definition(_, definition) = typed_statement {
      let (name, value) = self.eval_definition(definition);
      let value = self.eval_constant(value)?;

      self.stack.add(&name, value.clone());

      Ok(Some(value))
    } else {
      Ok(None)
    }
  }

  fn eval_definition(&mut self, typed_definition: &TypedDefinition) -> (String, Value) {
    let name = typed_definition.name.clone();
    let value = Self::create_function_closure(&mut self.stack, typed_definition);

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
    module: &TypedModule,
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

    for declaration in &module.declarations {
      match declaration {
        | TypedStatement::Definition(_, def) => {
          let (name, value) = self.eval_definition(def);
          definitions.insert(name, value);
        },
        | TypedStatement::Adt(_, adt) => {
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

  fn apply(&mut self, function: Value, input: Value) -> Result<Value, LangError> {
    if let Value::Function {
      arity,
      args,
      function,
    } = &function
    {
      let args_count = args.len() as u32 + 1;

      if *arity < args_count {
        return Err(
          InterpreterError::FunArgumentSizeMismatch(*arity, args_count, function.clone()).wrap(),
        );
      }

      let mut args = args.clone();
      args.push(input);

      let value = if *arity == args_count {
        self.exec_function(function, args)?
      } else {
        Value::Function {
          args,
          arity: *arity,
          function: function.clone(),
        }
      };

      Ok(value)
    } else {
      Err(InterpreterError::ExpectedFunction(function.clone()).wrap())
    }
  }

  fn exec_function(
    &mut self,
    function: &Function,
    arguments: Vec<Value>,
  ) -> Result<Value, LangError> {
    self.stack.enter_block();

    let result = match function {
      | Function::External { function, .. } => {
        // TODO: This probably should not map to `InterpreterError::BuiltinFunctionError` and just
        // propagate produced error.
        (function.function)(self, &arguments)
          .map_err(|_| InterpreterError::BuiltinFunctionError.wrap())
      },
      | Function::Definition {
        patterns,
        expression,
        captures,
        ..
      } => {
        assert_eq!(patterns.len(), arguments.len());

        for (name, value) in captures {
          let value = self.eval_constant(value.clone())?;
          self.stack.add(name, value)
        }

        for (pattern, value) in patterns.iter().zip(arguments) {
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
    typed_patterns: &[TypedPattern],
    typed_expression: &TypedExpression,
  ) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: typed_patterns.to_owned(),
      expression: typed_expression.clone(),
      captures: Self::extract_captures(stack, typed_expression),
      function_type: ty.clone(),
    });

    Value::Function {
      arity: typed_patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn create_function_closure(stack: &mut Stack, typed_definition: &TypedDefinition) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: typed_definition.patterns.clone(),
      expression: typed_definition.expression.clone(),
      captures: Self::extract_captures(stack, &typed_definition.expression),
      function_type: typed_definition.header.clone(),
    });

    Value::Function {
      arity: typed_definition.patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn extract_captures(
    stack: &mut Stack,
    typed_expression: &TypedExpression,
  ) -> HashMap<String, Value> {
    let mut map = HashMap::new();
    Self::traverse_expression(&mut map, stack, typed_expression);

    map
  }

  fn traverse_expression(
    result: &mut HashMap<String, Value>,
    stack: &mut Stack,
    typed_expression: &TypedExpression,
  ) {
    // TODO: Avoid capturing internal (intrinsic) definitions.
    match typed_expression {
      | TypedExpression::Ref(_, _, name) => {
        if let Some(value) = stack.find(name) {
          result.insert(name.to_string(), value);
        }
      },
      | TypedExpression::Tuple(_, _, expressions) | TypedExpression::List(_, _, expressions) => {
        for expression in expressions {
          Self::traverse_expression(result, stack, expression);
        }
      },
      | TypedExpression::If(_, _, cond, then_, else_) => {
        Self::traverse_expression(result, stack, cond.as_ref());
        Self::traverse_expression(result, stack, then_.as_ref());
        Self::traverse_expression(result, stack, else_.as_ref());
      },
      // TODO: Remove defined variables from captures, case, lambda and let.
      | TypedExpression::Match(_, _, cond, branches) => {
        Self::traverse_expression(result, stack, cond.as_ref());

        for (_, branch_expression) in branches {
          Self::traverse_expression(result, stack, branch_expression);
        }
      },
      | TypedExpression::Application(_, _, function, input) => {
        Self::traverse_expression(result, stack, function.as_ref());
        Self::traverse_expression(result, stack, input.as_ref());
      },
      | TypedExpression::Lambda(_, _, _, expression) => {
        Self::traverse_expression(result, stack, expression.as_ref());
      },
      | TypedExpression::Let(_, _, let_definitions, let_expression) => {
        Self::traverse_expression(result, stack, let_expression.as_ref());

        for let_definition in let_definitions {
          match let_definition {
            | TypedLet::Definition(definition) => {
              Self::traverse_expression(result, stack, &definition.expression);
            },
            | TypedLet::Pattern(_, expression) => {
              Self::traverse_expression(result, stack, expression);
            },
          }
        }
      },
      | _ => { /* Ignored. */ },
    }
  }
}

pub fn add_pattern_values(
  interpreter: &mut Interpreter,
  typed_pattern: &TypedPattern,
  value: Value,
) -> Result<(), InterpreterError> {
  match typed_pattern {
    | TypedPattern::Var(_, _, name) => {
      interpreter.stack.add(name, value);
    },
    | TypedPattern::Alias(_, _, typed_pattern, name) => {
      interpreter.stack.add(name, value.clone());
      add_pattern_values(interpreter, typed_pattern, value)?;
    },
    | TypedPattern::Adt(_, _, _, typed_patterns) => {
      if let Value::Adt(_, values, _) = &value {
        for (typed_pattern, value) in typed_patterns.iter().zip(values) {
          add_pattern_values(interpreter, typed_pattern, value.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedAdt(value.clone()));
      }
    },
    | TypedPattern::Tuple(_, _, typed_patterns) => {
      if let Value::Tuple(values) = &value {
        for (typed_pattern, value) in typed_patterns.iter().zip(values) {
          add_pattern_values(interpreter, typed_pattern, value.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedTuple(value.clone()));
      }
    },
    | TypedPattern::List(_, _, typed_patterns) => {
      if let Value::List(values) = &value {
        for (typed_pattern, value) in typed_patterns.iter().zip(values) {
          add_pattern_values(interpreter, typed_pattern, value.clone())?;
        }
      } else {
        return Err(InterpreterError::ExpectedList(value.clone()));
      }
    },
    | TypedPattern::BinaryOperator(_, _, op, left, right) => {
      if op == "::" {
        if let Value::List(values) = &value {
          if values.is_empty() {
            return Err(InterpreterError::ExpectedNonEmptyList(value.clone()));
          }

          let first = values[0].clone();
          let rest = values.iter().skip(1).cloned().collect::<Vec<_>>();

          add_pattern_values(interpreter, left, first)?;
          add_pattern_values(interpreter, right, Value::List(rest))?;
        } else {
          return Err(InterpreterError::ExpectedList(value.clone()));
        }
      } else {
        return Err(InterpreterError::UnknownOperatorPattern(op.clone()));
      }
    },
    | TypedPattern::LitInt(..)
    | TypedPattern::LitString(..)
    | TypedPattern::LitChar(..)
    | TypedPattern::Wildcard(_)
    | TypedPattern::Unit(_) => { /* Ignored. */ },
  }

  Ok(())
}

fn matches_pattern(typed_pattern: &TypedPattern, value: &Value) -> bool {
  match typed_pattern {
    | TypedPattern::Var(..) => true,
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
    | TypedPattern::LitInt(_, p) => {
      match value {
        | Value::Int(v) => (*p) == (*v),
        | Value::Number(v) => (*p) == (*v),
        | _ => false,
      }
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
  use indoc::indoc;

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

  #[test]
  fn test_let() {
    let expression = testing::typed_expression("let life = 42 in life");
    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Number(42))
    );
  }

  #[test]
  fn test_let_multiple() {
    let expression = testing::typed_expression(indoc! {"
      let
        life = 42
        blaze = 420
       in
        (life, blaze)
    "});

    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Tuple(vec![Value::Number(42), Value::Number(420)]))
    );
  }

  #[test]
  fn test_let_nested() {
    let expression = testing::typed_expression(indoc! {"
      let life =
        let life' = 42 in life'
       in
        life
    "});

    let mut interpreter = Interpreter::new();

    assert_eq!(
      interpreter.eval_expression(&expression),
      Ok(Value::Number(42))
    );
  }
}
