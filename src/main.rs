mod error;
mod globals;
mod idents;
mod item;
mod parse;
mod proc;
mod scope;
mod syn;

use anyhow::Result;
use clap::Parser;
use std::{fs, path::PathBuf};

/// A Scheme interpreter
#[derive(Parser)]
struct Args {
    /// Path to the input file
    #[arg(short)]
    input: PathBuf,
    /// Enable debug printing
    #[arg(long)]
    debug: bool,
}

fn main() -> Result<()> {
    let Args { input, debug } = Args::parse();
    globals::set_debug(debug);

    let prelude = include_bytes!("../prelude.scm");

    if let Ok(contents) = fs::read(&input) {
        println!("Read {} bytes.", contents.len());
        println!();
        parse::repl(prelude, &contents)?;
        print!("-- Finished --");
    } else {
        eprintln!("ERROR: Failed to read '{}'", input.display());
    }

    Ok(())
}
