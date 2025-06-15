use clap::{Parser, Subcommand};
use lox_rs::Lexer;
use std::{fs, path::PathBuf};
use miette::{WrapErr, IntoDiagnostic};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    cmd: Cmds,
}

#[derive(Subcommand, Debug)] 
enum Cmds {
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.cmd {
        Cmds::Tokenize { filename } => {
            eprintln!("Tokenizing {}", filename.display());

            let file_content = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read file {}", filename.display()))?;

            for tkn in Lexer::new(&file_content) {
                let tkn = tkn?;
                println!("{tkn}");
            }
        }
    }
    Ok(())
}
