mod error;
mod idents;
mod item;
mod parse;
mod scope;
mod syn;

use clap::Parser;
use std::{fs, path::PathBuf};

#[derive(Parser)]
struct Args {
    #[arg(short)]
    input: PathBuf,
}

fn main() {
    let Args { input } = Args::parse();

    if let Ok(contents) = fs::read(&input) {
        println!("Read {} bytes.\n", contents.len());
        if let Err(error) = parse::repl(&contents) {
            eprintln!("ERROR:\n{error}");
        } else {
            print!("-- Finished --");
        }
    } else {
        eprintln!("ERROR: Failed to read '{}'", input.display());
    }
}
