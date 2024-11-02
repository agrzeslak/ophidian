use ophidian::*;
use std::fs;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{Context, IntoDiagnostic, Result};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Lex { filename: PathBuf },
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.command {
        Command::Lex { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("failed to read '{}'", filename.display()))?;
            for token in Lexer::new(&file_contents) {
                println!("{:?}", token?);
            }
        }
    }

    Ok(())
}
