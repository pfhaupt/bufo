use std::path::PathBuf;

use clap::Parser;

use crate::util::opt_flags::OptimizationFlags;
use crate::compiler::FILE_EXT;

#[derive(Parser, Default, Debug, Clone)]
#[command(name = "bufo")]
#[command(author = "Philippe Felix Haupt <pfhaupt@gmx.net>")]
#[command(version = "0.0.1")]
#[command(about = "Compiler for the Bufo programming language")]
pub struct Flags {
    #[arg(short, long, value_parser = valid_filepath, hide_default_value=true)]
    pub input: PathBuf,
    #[arg(short, long)]
    pub output: Option<String>,
    #[arg(short, long, default_value = "false")]
    pub run: bool,
    #[arg(short, long, default_value = "false")]
    pub debug: bool,
    #[arg(short, long, default_value = "false")]
    pub verbose: bool,
    #[arg(short='A', long="ast", default_value = "false")]
    pub print_ast: bool,
    #[arg(short='O', default_value = "0", value_parser = valid_opt)]
    pub optimizations: OptimizationFlags,
    #[arg(long, value_parser = valid_header)]
    pub gen_bind: Option<String>,
    #[cfg(not(feature = "old_codegen"))]
    #[arg(long, default_value = "false")]
    pub emit_llvm: bool,
    #[arg(long, default_value = "false")]
    pub emit_asm: bool,
    #[arg(short='I', long="import")]
    pub imports: Vec<String>,
    #[arg(trailing_var_arg=true, use_value_delimiter=false)]
    pub exe_args: Vec<String>,
}

fn valid_opt(opt: &str) -> Result<OptimizationFlags, String> {
    if !["0", "1", "2", "3", "s"].contains(&opt) {
        return Err(format!("Optimization level `{}` is not supported.", opt));
    }
    Ok(OptimizationFlags::from(opt))
}

fn valid_header(header: &str) -> Result<String, String> {
    if !std::path::Path::new(header).exists() {
        return Err(format!("Header file `{}` does not exist.", header));
    }
    if !header.ends_with(".h") {
        return Err(format!("Header file `{}` does not have the correct extension.", header));
    }
    Ok(header.to_string())
}

fn valid_filepath(filepath: &str) -> Result<PathBuf, String> {
    if !std::path::Path::new(filepath).exists() {
        return Err(format!("File `{}` does not exist.", filepath));
    }
    if !filepath.ends_with(FILE_EXT) {
        return Err(format!("File `{}` does not have the correct extension.", filepath));
    }
    Ok(PathBuf::from(filepath))
}

impl Flags {
    pub fn parse_flags() -> Self {
        Flags::parse()
    }
}