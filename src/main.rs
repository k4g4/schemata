mod error;
mod globals;
mod idents;
mod item;
mod memory;
mod parse;
mod proc;
mod sexpr;
mod syn;
mod utils;

use anyhow::Result;
use clap::{Parser, ValueEnum};
use std::{fs, path::PathBuf};

#[derive(Copy, Clone, ValueEnum, Debug)]
#[repr(u64)]
enum GcFreq {
    Off = 0,
    Often = 10_000,
    Rarely = 1_000_000,
}

/// A Scheme interpreter
#[derive(Parser)]
struct Args {
    /// Path to the input file
    #[arg(long, short)]
    input: PathBuf,
    /// Enable debug printing
    #[arg(long, short)]
    debug: bool,
    /// Enable pretty printing
    #[arg(long, short)]
    pretty: bool,
    /// Change garbage collection frequency
    #[arg(long, value_enum, default_value_t = GcFreq::Often)]
    gc_freq: GcFreq,
}

fn main() -> Result<()> {
    let Args {
        input,
        debug,
        pretty,
        gc_freq,
    } = Args::parse();

    globals::set_debug(debug);
    globals::set_pretty(pretty);
    globals::set_gc_freq(gc_freq as u64);

    let prelude = include_bytes!("../prelude.scm");

    if let Ok(contents) = fs::read(&input) {
        if debug {
            println!("Read {} bytes.", contents.len());
        }
        parse::repl(prelude, &contents)?;
        print!("-- Finished --");
    } else {
        eprintln!("ERROR: Failed to read '{}'", input.display());
    }

    Ok(())
}
