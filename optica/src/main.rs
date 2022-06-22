use clap::Parser;
use optica::cli;
use optica::errors::LangError;

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
