use crate::ast::typed::*;
use crate::ast::untyped::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::source::SourceCode;
use crate::typechecker::Typechecker;

#[cfg(test)]
pub fn typed_expression(code: &str) -> TypedExpression {
  let source = SourceCode::from_str(code);
  let expression = expression(code);
  let mut tc = Typechecker::new(source);

  match tc.typecheck_expression(&expression) {
    | Ok(result) => result,
    | Err(error) => {
      println!("Error: {error:?}");
      panic!();
    },
  }
}

#[cfg(test)]
pub fn expression(code: &str) -> Expression {
  let source = SourceCode::from_str(code);
  let lexer = Lexer::new(&source);
  let mut parser = Parser::new(lexer);

  match parser.parse_expression() {
    | Ok(result) => result,
    | Err(error) => {
      println!("Error: {error:?}");
      panic!();
    },
  }
}
