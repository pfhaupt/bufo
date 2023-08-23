use std::{collections::HashMap, fmt::{Display, Debug, Formatter}};
use crate::codegen::{NOTE_STR, ERR_STR};

pub const FILE_EXT: &str = ".bu";
pub const INPUT_KEY: &str = "-i";
pub const RUN_KEY: &str = "-r";
pub const DEBUG_KEY: &str = "-d";

#[derive(Debug, Clone)]
pub enum Flag {
    InputFlag { path: Option<String> },
    RunFlag { run: bool },
    DebugFlag { debug: bool }
}

impl Display for Flag {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Flag::InputFlag { path } => {
                if let Some(p) = path {
                    writeln!(f, "Input File: {}", p)
                } else {
                    writeln!(f, "No input provided.")
                }
            },
            Flag::RunFlag { run } => {
                writeln!(f, "Run after compilation: {}", run)
            },
            Flag::DebugFlag { debug } => {
                writeln!(f, "Show debug info: {}", debug)
            }
        }
    }
}

pub struct FlagParser {
    flags: HashMap<String, Flag>,
}

impl FlagParser {
    pub fn init_flags() -> Self {
        let mut fp = FlagParser { flags: HashMap::new() };
        // Default values for flags are declared here.
        fp.flags.insert(INPUT_KEY.to_string(), Flag::InputFlag { path: None });
        fp.flags.insert(RUN_KEY.to_string(), Flag::RunFlag { run: false });
        fp.flags.insert(DEBUG_KEY.to_string(), Flag::DebugFlag { debug: false });
        fp
    }

    pub fn parse_flags(&mut self) -> Result<HashMap<String, Flag>, String> {
        assert!(!self.flags.is_empty(), "Did you try to parse flags before initializing them?");
        let mut args = std::env::args();
        let _ = args.next().expect("Expected Program Name"); // program name
        let mut input_found = false;
        while let Some(flag_str) = args.next() {
            if let Some(flag) = self.flags.get(&flag_str) {
                self.flags.insert(flag_str,
                    match flag {
                        Flag::InputFlag { path } => {
                            if path.is_some() { todo!("Two inputs provided!") }
                            match args.next() {
                                Some(p) => {
                                    if !p.ends_with(FILE_EXT) {
                                        return Err(format!("{ERR_STR}: Invalid file provided for input flag. Expected file ending with `{FILE_EXT}`."));
                                    }
                                    input_found = true;
                                    Flag::InputFlag { path: Some(p) }

                                }
                                None => return Err(format!("{ERR_STR}: No file provided for input flag."))
                            }
                        }
                        Flag::RunFlag { .. } => {
                            Flag::RunFlag { run: true }
                        }
                        Flag::DebugFlag { .. } => {
                            Flag::DebugFlag { debug: true }
                        }
                    }
                );
            } else {
                if flag_str.ends_with(FILE_EXT) {
                    return Err(format!(
                        "{}: Unexpected argument ending with `{}`.\n{}: Did you forget to specify the `{}` flag?",
                        ERR_STR,
                        FILE_EXT,
                        NOTE_STR,
                        INPUT_KEY
                    ));
                } else {
                    return Err(format!("Found unknown flag `{flag_str}`."));
                }
            }
        }
        if !input_found {
            return Err(format!("{ERR_STR}: No input flag was found. No input file was provided."));
        }
        Ok(self.flags.clone())
    }
}