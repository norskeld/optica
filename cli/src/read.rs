use std::path::Path;

use compiler::errors::LangError;
use compiler::runtime::Runtime;

pub fn read(path: &str) -> Result<(), LangError> {
  let mut runtime = Runtime::new();

  let file_name = Path::new(path).file_name().unwrap();
  let file_stem = Path::new(&file_name).file_stem().unwrap();
  let module = file_stem.to_string_lossy().to_string();

  runtime.include_file(path)?;
  runtime.import_module(&module)?;

  match runtime.eval_expr(&format!("{module}.main")) {
    | Ok(value) => {
      println!("{value:?}");
      Ok(())
    },
    | Err(err) => {
      eprintln!("{err:?}");
      Err(err)
    },
  }
}
