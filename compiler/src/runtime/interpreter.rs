use std::borrow::Borrow;
use std::collections::HashMap;
use std::sync::Arc;

use crate::ast;
use crate::ast::typed::{
  Adt, AdtVariant, Declaration, Function, TypedDefinition, TypedExpression, TypedPattern, Value,
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
      .eval_expr(&TypedExpression::Ref(
        (0, 0),
        ast::type_bool(),
        "True".to_string(),
      ))
      .unwrap()
  }

  pub fn false_value(&mut self) -> Value {
    self
      .eval_expr(&TypedExpression::Ref(
        (0, 0),
        ast::type_bool(),
        "False".to_string(),
      ))
      .unwrap()
  }

  pub fn eval_expr(&mut self, expr: &TypedExpression) -> Result<Value, LangError> {
    match expr {
      | TypedExpression::Ref(_, _, name) => {
        let opt = self.stack.find(name);

        match opt {
          | Some(val) => Ok(val),
          | None => Err(InterpreterError::MissingDefinition(name.clone()).wrap()),
        }
      },
      | TypedExpression::Const(_, _, value) => Ok(value.clone()),
      | TypedExpression::Tuple(_, _, items) => {
        let values = items
          .iter()
          .map(|e| self.eval_expr(e))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::Tuple(values))
      },
      | TypedExpression::List(_, _, items) => {
        let values = items
          .iter()
          .map(|e| self.eval_expr(e))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::List(values))
      },
      | TypedExpression::If(_, _, cond, a, b) => {
        let cond = self.eval_expr(cond)?;

        match &cond {
          | Value::Adt(ref name, ref vals, _) => {
            if name == "True" && vals.is_empty() {
              self.eval_expr(a)
            } else if name == "False" && vals.is_empty() {
              self.eval_expr(b)
            } else {
              Err(InterpreterError::InvalidIfCondition(cond.clone()).wrap())
            }
          },
          | _ => Err(InterpreterError::InvalidIfCondition(cond.clone()).wrap()),
        }
      },
      | TypedExpression::Lambda(_, ty, patt, expr) => {
        Ok(Self::create_lambda_closure(&mut self.stack, ty, patt, expr))
      },
      | TypedExpression::Application(_, _, fun, input) => {
        let function = self.eval_expr(fun)?;
        let input = self.eval_expr(input)?;

        self.application(function, input)
      },
    }
  }

  pub fn eval_declaration(&mut self, decl: &Declaration) -> Result<Option<Value>, LangError> {
    if let Declaration::Definition(_, def) = decl {
      let (name, value) = self.eval_definition(def);
      let value = self.eval_const(value)?;

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
      definitions: old_definitions,
      imports,
    } = module;
    let mut definitions = HashMap::new();

    for (name, value) in old_definitions.into_iter() {
      let new_value = self.eval_const(value)?;
      definitions.insert(name, new_value);
    }

    Ok(RuntimeModule {
      name,
      definitions,
      imports,
    })
  }

  fn eval_const(&mut self, value: Value) -> Result<Value, LangError> {
    let opt = if let Value::Function {
      arity, function, ..
    } = &value
    {
      if *arity == 0 {
        Some(self.exec_function(function.borrow(), vec![])?)
      } else {
        None
      }
    } else {
      None
    };

    Ok(opt.unwrap_or(value))
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
        eprintln!(
          "Failed to find {} in {} {:#?}",
          import.source_name,
          import.source,
          module.definitions.keys().collect::<Vec<_>>()
        );

        InterpreterError::MissingDefinition(import.source_name.to_string()).wrap()
      })?;

      self.stack.add(&import.destination_name, value.clone());
    }

    for decl in &module.declarations {
      match decl {
        | Declaration::Port(_, _) => {},
        | Declaration::Definition(_, def) => {
          let (name, value) = self.eval_definition(def);
          definitions.insert(name, value);
        },
        | Declaration::Alias(_) => {},
        | Declaration::Adt(_, adt) => {
          for variant in &adt.variants {
            let (name, value) = self.eval_adt_variant(adt.clone(), variant);
            definitions.insert(name, value);
          }
        },
        | Declaration::Infix(_, _, _) => {},
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
      value = self.application(value, arg.clone())?;
    }

    Ok(value)
  }

  fn application(&mut self, func_value: Value, input: Value) -> Result<Value, LangError> {
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
        (function.function)(self, &args).map_err(|_| InterpreterError::BuiltinFunctionError.wrap())
      },
      | Function::Definition {
        patterns,
        expression,
        captures,
        ..
      } => {
        assert_eq!(patterns.len(), args.len());

        for (name, val) in captures {
          self.stack.add(name, val.clone())
        }

        for (patt, val) in patterns.iter().zip(args) {
          add_pattern_values(self, patt, val).unwrap();
        }

        self.eval_expr(expression)
      },
    };

    self.stack.exit_block();

    result
  }

  pub fn create_lambda_closure(
    env: &mut Stack,
    ty: &Type,
    patterns: &[TypedPattern],
    expr: &TypedExpression,
  ) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: patterns.to_owned(),
      expression: expr.clone(),
      captures: Self::extract_captures(env, expr),
      function_type: ty.clone(),
    });

    Value::Function {
      arity: patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn create_function_closure(env: &mut Stack, def: &TypedDefinition) -> Value {
    let function = Arc::new(Function::Definition {
      id: ast::function_id(),
      patterns: def.patterns.clone(),
      expression: def.expression.clone(),
      captures: Self::extract_captures(env, &def.expression),
      function_type: def.header.clone(),
    });

    Value::Function {
      arity: def.patterns.len() as u32,
      args: vec![],
      function,
    }
  }

  pub fn extract_captures(env: &mut Stack, expr: &TypedExpression) -> HashMap<String, Value> {
    let mut map = HashMap::new();
    Self::traverse_expr(&mut map, env, expr);
    map
  }

  fn traverse_expr(result: &mut HashMap<String, Value>, env: &mut Stack, expr: &TypedExpression) {
    // TODO: Avoid capturing internal definitions.
    match expr {
      | TypedExpression::Ref(_, _, name) => {
        if let Some(value) = env.find(name) {
          result.insert(name.to_string(), value);
        }
      },
      | TypedExpression::Tuple(_, _, list) | TypedExpression::List(_, _, list) => {
        for expr in list {
          Self::traverse_expr(result, env, expr);
        }
      },
      | TypedExpression::If(_, _, a, b, c) => {
        Self::traverse_expr(result, env, a.as_ref());
        Self::traverse_expr(result, env, b.as_ref());
        Self::traverse_expr(result, env, c.as_ref());
      },
      | TypedExpression::Application(_, _, a, b) => {
        Self::traverse_expr(result, env, a.as_ref());
        Self::traverse_expr(result, env, b.as_ref());
      },
      | TypedExpression::Lambda(_, _, _, box_expr) => {
        Self::traverse_expr(result, env, box_expr.as_ref());
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

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}
