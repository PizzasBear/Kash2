use clap::{Arg, Command};
use std::{fs, path::Path};
// use crossterm::style::Stylize;
// use std::io::{self, Write};
// use std::mem;

// artemis fowl vs johan liebert

fn main() -> anyhow::Result<()> {
    let matches = Command::new("Kash 2")
        .version("indev")
        .about("This is a shell language with the goal of making complex actions simpler to read, yet quick to write.")
        .author("Max S.")
        .subcommand(Command::new("run")
            .about("Runs the provided script")
            .arg(Arg::new("SCRIPT")
                .help("Sets the script file to use")
                .required(true)
                .index(1)))
        .get_matches();

    match matches.subcommand() {
        Some(("run", sub_m)) => {
            let script: &String = sub_m.get_one("SCRIPT").unwrap();
            let path = Path::new(&script);
            let _file = fs::read_to_string(path)? + "\n";
        }
        _ => {}
    }

    Ok(())
}
