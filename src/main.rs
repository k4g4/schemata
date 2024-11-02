mod error;
mod idents;
mod item;
mod parse;
mod scope;
mod syn;

use clap::Parser;
use std::{fs::File, io::Read, path::PathBuf};

#[derive(Parser)]
struct Args {
    #[arg(short)]
    input: PathBuf,
}

fn main() {
    let Args { input } = Args::parse();

    let mut contents = include_bytes!("../prelude.scm").to_vec();

    let Ok(mut file) = File::open(&input) else {
        eprintln!("ERROR: Failed to read '{}'", input.display());
        return;
    };

    if let Ok(bytes) = file.read_to_end(&mut contents) {
        println!("Read {bytes} bytes.\n");
        if let Err(error) = parse::repl(&contents) {
            eprintln!("ERROR:\n{error}");
        } else {
            print!("-- Finished --");
        }
    } else {
        eprintln!("ERROR: Failed to read '{}'", input.display());
    }
}
