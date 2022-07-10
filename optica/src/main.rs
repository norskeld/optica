use clap::Parser;
use optica::cli;

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
pub struct Cli {
  /// File to run.
  #[clap(name = "file")]
  file: Option<String>,
}

fn main() {
  let options = Cli::parse();

  if let Some(file) = options.file {
    match cli::read(file.as_str()) {
      | Ok(value) => println!("{value}"),
      | Err(err) => eprintln!("{err}"),
    }
  } else {
    cli::repl();
  }
}
