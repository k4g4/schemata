mod error;
mod eval;

use clap::Parser;
use eval::eval;
use std::{fs, path::PathBuf};

#[derive(Parser)]
struct Args {
    #[arg(short)]
    input: PathBuf,
}

fn main() {
    let Args { input } = Args::parse();

    if let Ok(contents) = fs::read(&input) {
        if let Err(error) = eval(&contents) {
            eprintln!("{error}");
        }
    } else {
        eprintln!("Failed to read '{}'", input.display());
    }
}
