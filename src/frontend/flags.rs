use crate::compiler::{ERR_STR, NOTE_STR};
use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
};

pub const FILE_EXT: &str = ".bu";
pub const INPUT_KEY: &str = "-i";
pub const RUN_KEY: &str = "-r";
pub const DEBUG_KEY: &str = "-d";

#[derive(Debug, Clone)]
pub enum Flag {
    Input { path: Option<String> },
    Run { run: bool },
    Debug { debug: bool },
}

impl Display for Flag {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Flag::Input { path } => {
                if let Some(p) = path {
                    writeln!(f, "Input File: {}", p)
                } else {
                    writeln!(f, "No input provided.")
                }
            }
            Flag::Run { run } => {
                writeln!(f, "Run after compilation: {}", run)
            }
            Flag::Debug { debug } => {
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
        let mut fp = FlagParser {
            flags: HashMap::new(),
        };
        // Default values for flags are declared here.
        fp.flags
            .insert(INPUT_KEY.to_string(), Flag::Input { path: None });
        fp.flags
            .insert(RUN_KEY.to_string(), Flag::Run { run: false });
        fp.flags
            .insert(DEBUG_KEY.to_string(), Flag::Debug { debug: false });
        fp
    }

    pub fn parse_flags(&mut self) -> Result<HashMap<String, Flag>, String> {
        assert!(
            !self.flags.is_empty(),
            "Did you try to parse flags before initializing them?"
        );
        let mut args = std::env::args();
        let _ = args.next().expect("Expected Program Name"); // program name
        let mut input_found = false;
        while let Some(flag_str) = args.next() {
            if let Some(flag) = self.flags.get(&flag_str) {
                self.flags.insert(flag_str,
                    match flag {
                        Flag::Input { path } => {
                            if path.is_some() {
                                return Err(format!(
                                    "{}: Providing multiple input files is not supported.\n{}: Already got input file `{}`.",
                                    ERR_STR,
                                    NOTE_STR,
                                    path.as_ref().unwrap()
                                ));
                            }
                            match args.next() {
                                Some(p) => {
                                    if !p.ends_with(FILE_EXT) {
                                        return Err(format!("{ERR_STR}: Invalid file provided for input flag. Expected file ending with `{FILE_EXT}`, got `{p}`."));
                                    }
                                    input_found = true;
                                    Flag::Input { path: Some(p) }

                                }
                                None => return Err(format!("{ERR_STR}: No file provided for input flag."))
                            }
                        }
                        Flag::Run { .. } => {
                            Flag::Run { run: true }
                        }
                        Flag::Debug { .. } => {
                            Flag::Debug { debug: true }
                        }
                    }
                );
            } else if flag_str.ends_with(FILE_EXT) {
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
        if !input_found {
            return Err(format!(
                "{ERR_STR}: No input flag was found. No input file was provided."
            ));
        }
        Ok(self.flags.clone())
    }
}
