use crate::ast::untyped::Expression;
use self::ExpressionTreeError::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionTree {
  Leaf(Expression),
  Branch(String, Box<Self>, Box<Self>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionTreeError {
  InvalidInput,
  AssociativityError,
  InternalError(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Associativity {
  Left,
  Right,
  None,
}

#[derive(Clone, Debug, PartialEq)]
enum ExpressionKind {
  Expression(Expression),
  Operator(String),
}

pub fn to_expression(tree: ExpressionTree) -> Expression {
  match tree {
    | ExpressionTree::Leaf(e) => e,
    | ExpressionTree::Branch(op, left, right) => {
      let left_expr = to_expression(*left);
      let right_expr = to_expression(*right);

      let span = (left_expr.get_span().0, right_expr.get_span().1);

      Expression::Application(
        span,
        Box::new(Expression::Application(
          span,
          Box::new(Expression::Ref(span, op)),
          Box::new(left_expr),
        )),
        Box::new(right_expr),
      )
    },
  }
}

pub fn create_expression_tree(
  exprs: &[Expression],
  ops: &[String],
) -> Result<ExpressionTree, ExpressionTreeError> {
  let tokens = create_token_stream(exprs, ops)?;
  let (rest, tree) = create_tree(&tokens, 0)?;

  assert_eq!(rest.len(), 0);

  Ok(tree)
}

fn create_token_stream(
  expressions: &[Expression],
  operators: &[String],
) -> Result<Vec<ExpressionKind>, ExpressionTreeError> {
  if expressions.len() != operators.len() + 1 {
    return Err(ExpressionTreeError::InvalidInput);
  }

  let mut list = Vec::new();
  let mut expression = expressions.iter();

  list.push(ExpressionKind::Expression(
    expression.next().unwrap().clone(),
  ));

  operators.iter().for_each(|op| {
    list.push(ExpressionKind::Operator(op.clone()));
    list.push(ExpressionKind::Expression(
      expression.next().unwrap().clone(),
    ));
  });

  Ok(list)
}

fn create_tree(
  mut tokens: &[ExpressionKind],
  level: i32,
) -> Result<(&[ExpressionKind], ExpressionTree), ExpressionTreeError> {
  if level == 10 {
    return match &tokens[0] {
      | &ExpressionKind::Expression(ref expression) => {
        Ok((&tokens[1..], ExpressionTree::Leaf(expression.clone())))
      },
      | _ => Err(InternalError(format!(
        "Illegal expression tree state, token: {:#?}",
        tokens
      ))),
    };
  }

  let (rest, first) = create_tree(tokens, level + 1)?;

  tokens = rest;

  let mut operators: Vec<String> = vec![];
  let mut expressions: Vec<ExpressionTree> = vec![first];

  while !tokens.is_empty() {
    let operator = match &tokens[0] {
      | &ExpressionKind::Operator(ref op) => op.clone(),
      | _ => panic!("Illegal tree state"),
    };

    if get_operator_priority(&operator) != level {
      break;
    }

    let (_tk, item) = create_tree(&tokens[1..], level + 1)?;

    expressions.push(item);
    operators.push(operator);

    tokens = _tk;
  }

  if operators.is_empty() {
    return Ok((tokens, expressions[0].clone()));
  }

  let first_operator = operators.first().unwrap();
  let associativity = get_operator_associativity(first_operator);

  match associativity {
    | Associativity::Left => {
      let mut index = 0;
      let mut current_tree = expressions[index].clone();

      index += 1;

      for operator in operators.iter() {
        if get_operator_associativity(operator) != associativity {
          return Err(AssociativityError);
        }

        current_tree = ExpressionTree::Branch(
          operator.clone(),
          Box::new(current_tree),
          Box::new(expressions[index].clone()),
        );

        index += 1;
      }

      Ok((tokens, current_tree))
    },
    | Associativity::Right => {
      let mut index = (expressions.len() - 1) as isize;
      let mut current_tree = expressions[index as usize].clone();

      index -= 1;

      for operator in operators.iter() {
        if get_operator_associativity(operator) != associativity {
          return Err(AssociativityError);
        }

        current_tree = ExpressionTree::Branch(
          operator.clone(),
          Box::new(expressions[index as usize].clone()),
          Box::new(current_tree),
        );

        index -= 1;
      }

      Ok((tokens, current_tree))
    },
    | Associativity::None => {
      if operators.len() == 1 {
        Ok((
          tokens,
          ExpressionTree::Branch(
            operators[0].clone(),
            Box::new(expressions[0].clone()),
            Box::new(expressions[1].clone()),
          ),
        ))
      } else {
        Err(AssociativityError)
      }
    },
  }
}

/// Default priorities.
pub fn get_operator_priority(operator: &str) -> i32 {
  match operator {
    | ">>" | "<<" => 9,
    | "^" => 8,
    | "*" | "/" | "//" | "%" | "rem" => 7,
    | "+" | "-" => 6,
    | "++" | "::" => 5,
    | "==" | "/=" | "<" | ">" | "<=" | ">=" => 4,
    | "&&" => 3,
    | "||" => 2,
    | "|>" | "<|" => 0,
    | _ => 1,
  }
}

/// Default associativities.
pub fn get_operator_associativity(operator: &str) -> Associativity {
  match operator {
    | "|>" | ">>" | "*" | "/" | "//" | "%" | "rem" | "+" | "-" => Associativity::Left,
    | "<|" | "<<" | "^" | "++" | "::" | "&&" | "||" => Associativity::Right,
    | "==" | "/=" | "<" | ">" | "<=" | ">=" => Associativity::None,
    | _ => Associativity::Left,
  }
}
