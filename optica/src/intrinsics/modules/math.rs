use std::ops::{Add, Mul, Sub};

use crate::ast::typed::*;
use crate::errors::*;
use crate::intrinsics::conversions;
use crate::intrinsics::helpers::*;
use crate::intrinsics::IntrinsicModule;
use crate::runtime::Interpreter;

pub fn module() -> Result<IntrinsicModule, LangError> {
  Ok(create_module(
    "Math",
    vec![
      // Addition
      create_intrinsic("+", "number -> number -> number", addition)?,
      create_intrinsic("add", "number -> number -> number", addition)?,
      // Subtraction
      create_intrinsic("-", "number -> number -> number", subtraction)?,
      create_intrinsic("sub", "number -> number -> number", subtraction)?,
      // Unary negation
      create_intrinsic("__minus", "number -> number", minus)?,
      // Multiplication
      create_intrinsic("*", "number -> number -> number", multiplication)?,
      create_intrinsic("mul", "number -> number -> number", multiplication)?,
      // Float division
      create_intrinsic("/", "Float -> Float -> Float", fdivision)?,
      create_intrinsic("fdiv", "Float -> Float -> Float", fdivision)?,
      // Integer division
      create_intrinsic("//", "Int -> Int -> Int", idivision)?,
      create_intrinsic("idiv", "Int -> Int -> Int", idivision)?,
      // Exponentiation
      create_intrinsic("^", "number -> number -> number", pow)?,
      create_intrinsic("pow", "number -> number -> number", pow)?,
      // Square root
      create_intrinsic("sqrt", "Float -> Float", sqrt)?,
      // Reducing
      create_intrinsic("ceil", "Float -> Int", ceil)?,
      create_intrinsic("floor", "Float -> Int", floor)?,
      create_intrinsic("round", "Float -> Int", round)?,
      create_intrinsic("truncate", "Float -> Int", truncate)?,
    ],
  ))
}

fn addition(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => conversions::number_op(lhs, rhs, Add::add),
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn subtraction(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => conversions::number_op(lhs, rhs, Sub::sub),
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn minus(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match &args {
    | [Value::Number(value)] => Ok(Value::Number(-*value)),
    | [Value::Int(value)] => Ok(Value::Int(-*value)),
    | [Value::Float(value)] => Ok(Value::Float(-*value)),
    | _ => Err(InterpreterError::ExpectedNumber(args[0].clone()).wrap()),
  }
}

fn multiplication(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => conversions::number_op(lhs, rhs, Mul::mul),
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn fdivision(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => {
      Ok(Value::Float(
        conversions::float_of(lhs)? / conversions::float_of(rhs)?,
      ))
    },
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn idivision(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => {
      let a = conversions::int_of(lhs)?;
      let b = conversions::int_of(rhs)?;

      if b == 0 {
        Ok(Value::Int(0))
      } else {
        Ok(Value::Int(a / b))
      }
    },
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn pow(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  match args {
    | [lhs, rhs] => conversions::number_op(lhs, rhs, |a, b| a.powf(b)),
    | _ => Err(InterpreterError::IntrinsicError.wrap()),
  }
}

fn sqrt(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  let a = conversions::float_of(&args[0])?;

  Ok(Value::Float(a.sqrt()))
}

fn truncate(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  let a = conversions::float_of(&args[0])?;

  Ok(Value::Int(a as isize))
}

fn ceil(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  let a = conversions::float_of(&args[0])?;

  Ok(Value::Int(a.ceil() as isize))
}

fn floor(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  let a = conversions::float_of(&args[0])?;

  Ok(Value::Int(a.floor() as isize))
}

fn round(_: &mut Interpreter, args: &[Value]) -> Result<Value, LangError> {
  let a = conversions::float_of(&args[0])?;

  Ok(Value::Int(a.round() as isize))
}
