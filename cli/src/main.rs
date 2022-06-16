use clap::Parser;
use compiler::errors::LangError;

mod read;
mod repl;

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
pub struct Cli {
  /// File to run.
  #[clap(name = "file")]
  file: Option<String>,
}

fn main() -> Result<(), LangError> {
  let options = Cli::parse();

  if let Some(file) = options.file {
    read::read(file.as_str())?;
  } else {
    repl::repl();
  }

  Ok(())
}
