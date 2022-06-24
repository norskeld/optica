use crate::ast::typed::*;
use crate::ast::{Float, Int};
use crate::errors::*;

#[derive(Debug, Eq, PartialEq)]
enum NumberState {
  Number,
  Int,
  Float,
}

pub fn number_op<F>(value_lhs: &Value, value_rhs: &Value, op: F) -> Result<Value, LangError>
where
  F: FnOnce(Float, Float) -> Float,
{
  let mut strong_type: NumberState;

  let lhs = match value_lhs {
    | Value::Number(value) => {
      strong_type = NumberState::Number;
      *value as Float
    },
    | Value::Int(value) => {
      strong_type = NumberState::Int;
      *value as Float
    },
    | Value::Float(value) => {
      strong_type = NumberState::Float;
      *value
    },
    | _ => {
      return Err(InterpreterError::ExpectedNumber(value_lhs.clone()).wrap());
    },
  };

  let rhs = match value_rhs {
    | Value::Number(value) => {
      strong_type = merge(strong_type, NumberState::Number, value_rhs)?;
      *value as Float
    },
    | Value::Int(value) => {
      strong_type = merge(strong_type, NumberState::Int, value_rhs)?;
      *value as Float
    },
    | Value::Float(value) => {
      strong_type = merge(strong_type, NumberState::Float, value_rhs)?;
      *value
    },
    | _ => {
      return Err(InterpreterError::ExpectedNumber(value_lhs.clone()).wrap());
    },
  };

  let result = op(lhs, rhs);

  Ok(match strong_type {
    | NumberState::Number => Value::Number(result as isize),
    | NumberState::Int => Value::Int(result as isize),
    | NumberState::Float => Value::Float(result),
  })
}

/// Truth table of number for:
///
/// Float, Float -> Float
/// number, Float -> Float
/// Float, number -> Float
///
/// Int, Int -> Int
/// number, Int -> Int
/// Int, number -> Int
///
/// Int, Float -> error
/// Float, Int -> error
fn merge(lhs: NumberState, rhs: NumberState, value: &Value) -> Result<NumberState, LangError> {
  match lhs {
    | NumberState::Number => Ok(rhs),
    | NumberState::Int => {
      if rhs == NumberState::Int || rhs == NumberState::Number {
        Ok(lhs)
      } else {
        Err(InterpreterError::ExpectedInt(value.clone()).wrap())
      }
    },
    | NumberState::Float => {
      if rhs == NumberState::Float || rhs == NumberState::Number {
        Ok(lhs)
      } else {
        Err(InterpreterError::ExpectedFloat(value.clone()).wrap())
      }
    },
  }
}

pub fn float_of(value: &Value) -> Result<Float, LangError> {
  match value {
    | Value::Number(a) => Ok(*a as Float),
    | Value::Float(a) => Ok(*a),
    | _ => Err(InterpreterError::ExpectedFloat(value.clone()).wrap()),
  }
}

pub fn int_of(value: &Value) -> Result<Int, LangError> {
  match value {
    | Value::Number(a) => Ok(*a),
    | Value::Int(a) => Ok(*a),
    | _ => Err(InterpreterError::ExpectedInt(value.clone()).wrap()),
  }
}
