use std::time::Instant;

use crate::backend::assembler::Assembler;
use crate::backend::codegen::Codegen;
use crate::frontend::flags::{Flag, FlagParser, DEBUG_KEY, INPUT_KEY, RUN_KEY};
use crate::frontend::parser::Parser;
use crate::middleend::checker::TypeChecker;

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

pub const OUTPUT_FOLDER: &str = "./out/";
pub const FILE_EXT: &str = ".bu";

pub const CONSTRUCTOR_NAME: &str = "new";

pub const BUILT_IN_FEATURES: [&str; 1] = [
    CONSTRUCTOR_NAME
];

#[macro_export]
macro_rules! internal_error {
    ($msg:expr) => {
        Err(format!(
            "INTERNAL ERROR AT {}:{}:{}: {}",
            file!(),
            line!(),
            column!(),
            $msg
        ))
    };
}

pub struct Compiler {
    parser: Parser,
    checker: TypeChecker,
    codegen: Codegen,
    assembler: Assembler,
    print_debug: bool,
    run: bool,
}

impl Compiler {
    pub fn new(path: &str, print_debug: bool, run: bool) -> Result<Self, String> {
        Ok(Self {
            parser: Parser::new().filepath(path)?.debug(print_debug),
            checker: TypeChecker::new(),
            codegen: Codegen::new().debug(print_debug),
            assembler: Assembler::new().debug(print_debug).filepath(path),
            print_debug,
            run,
        })
    }

    pub fn run_everything(&mut self) -> Result<(), String> {
        let now = Instant::now();
        let mut parsed_ast = self.parser.parse_file()?;
        if self.print_debug {
            println!("Parsing took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.checker.type_check_file(&mut parsed_ast)?;
        if self.print_debug {
            println!("Type Checking took {:?}", now.elapsed());
        }

        let now = Instant::now();
        let ir = self.codegen.generate_code(&parsed_ast)?;
        if self.print_debug {
            println!("Codegen took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.assembler.generate_x86_64(ir)?;
        if self.print_debug {
            println!("Assembling took {:?}", now.elapsed());
        }
        if self.run {
            let now = Instant::now();
            self.assembler.run()?;
            if self.print_debug {
                println!("Running took {:?}", now.elapsed());
            }
        }
        Ok(())
    }
}

fn compile() -> Result<(), String> {
    let now = Instant::now();
    let flags = FlagParser::init_flags().parse_flags()?;

    let path = match flags.get(INPUT_KEY).unwrap() {
        Flag::Input { path } => path.as_ref().unwrap(),
        _ => unreachable!(),
    };
    let run = match flags.get(RUN_KEY).unwrap() {
        Flag::Run { run } => *run,
        _ => unreachable!(),
    };
    let debug = match flags.get(DEBUG_KEY).unwrap() {
        Flag::Debug { debug } => *debug,
        _ => unreachable!(),
    };
    if debug {
        println!("Parsing flags took {:?}", now.elapsed());
    }
    let mut compiler = Compiler::new(path, debug, run)?;
    compiler.run_everything()
}
pub fn run() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
