use clap::Parser;
use std::fs;

pub const FILE_EXT: &str = ".bu";

#[derive(Parser, Default, Debug, Clone)]
#[command(name = "bufo")]
#[command(author = "Philippe Felix Haupt <pfhaupt@gmx.net>")]
#[command(version = "0.0.1")]
#[command(about = "Compiler for the Bufo programming language")]
pub struct Flags {
    #[arg(short, long, value_parser = valid_filepath)]
    pub input: String,
    #[arg(short, long, default_value = "false")]
    pub run: bool,
    #[arg(short, long, default_value = "false")]
    pub debug: bool,
    #[arg(short='A', long="ast", default_value = "false")]
    pub print_ast: bool,
}

fn valid_filepath(filepath: &str) -> Result<String, String> {
    let Ok(_) = fs::read_to_string(filepath) else {
        return Err(format!("File `{}` does not exist", filepath));
    };
    if !filepath.ends_with(FILE_EXT) {
        return Err(format!("File `{}` does not have the correct extension.", filepath));
    }

    Ok(filepath.to_string())
}

impl Flags {
    pub fn parse_flags() -> Self {
        Flags::parse()
    }
}