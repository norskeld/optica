use clap::Parser;
use optica::errors::LangError;
use optica::cli;

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
    cli::read(file.as_str())?;
  } else {
    cli::repl();
  }

  Ok(())
}
