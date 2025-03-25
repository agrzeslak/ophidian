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
    Parse { filename: PathBuf },
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
        Command::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("failed to read '{}'", filename.display()))?;
            let lexer = Lexer::new(&file_contents);
            let mut tokens = Vec::new();
            for token in lexer {
                match token {
                    Ok(token) => tokens.push(token),
                    Err(e) => return Err(e.into()),
                }
            }
            let mut parser = ophidian::Parser::new(tokens.iter());
            parser.parse();
        }
    }

    Ok(())
}
