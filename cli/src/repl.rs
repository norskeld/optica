use std::io::{self, BufRead, Write};

use compiler::errors::LangError;
use compiler::runtime::Runtime;

pub fn repl() {
  let mut engine = Runtime::new();

  loop {
    // Read
    let line = read_terminal_line().unwrap_or(String::from(""));

    if line.is_empty() {
      continue;
    }

    // Eval
    let result = engine.eval_statement(&line);

    println!("{:?}", &engine.typechecker.env);
    println!("{:?}", &result);

    // Print
    match result {
      | Ok(opt) => {
        if let Some(value) = opt {
          println!("{:?} : {:?}", value, value.get_type());
        }
      },
      | Err(err) => {
        if let LangError::Parser(..) = err {
          let result = engine.eval_expr(&line);

          match result {
            | Ok(value) => {
              println!("{:?} : {:?}", value, value.get_type());
            },
            | Err(error) => {
              println!("{error:?}");
            },
          }

          continue;
        }

        println!("{err:?}");
      },
    }
  }
}

fn read_terminal_line() -> Result<String, ()> {
  let stdin = io::stdin();
  let mut line = String::new();

  print!("> ");

  io::stdout().flush().unwrap();

  loop {
    stdin.lock().read_line(&mut line).map_err(|_| ())?;

    if line.len() < 2 {
      return Err(());
    }

    if line.as_bytes()[line.len() - 2] != b'\\' {
      break;
    }

    line.pop().unwrap();
    line.pop().unwrap();
    line.push('\n');

    print!("| ");

    io::stdout().flush().unwrap();
  }

  Ok(line)
}
