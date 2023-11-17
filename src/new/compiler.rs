
use std::time::Instant;

use crate::parser;
use crate::flags::{FlagParser, Flag, DEBUG_KEY, RUN_KEY, INPUT_KEY};

use super::new_parser::Parser;
use super::new_desugar::Desugarer;
use super::new_checker::TypeChecker;
use super::new_codegen::Codegen;

pub struct Compiler {
    parser: Parser,
    desugarer: Desugarer,
    checker: TypeChecker,
    codegen: Codegen,
    debug: bool,
    run: bool
}

impl Compiler {
    pub fn new(path: &String, debug: bool, run: bool) -> Result<Self, String> {
        Ok(Self {
            parser: Parser::new().filepath(path)?.debug(debug),
            desugarer: Desugarer::new().debug(debug),
            checker: TypeChecker::new(),
            codegen: Codegen::new().debug(debug),
            debug,
            run
        })
    }

    pub fn run_everything(&mut self) -> Result<(), String> {
        let now = Instant::now();
        let mut parsed_ast = self.parser.parse_file()?;
        if self.debug {
            println!("Parsing took {:?}", now.elapsed());
        }

        let now = Instant::now();
        // self.desugarer.desugar_file(&mut parsed_ast)?;
        if self.debug {
            println!("Desugaring took {:?}", now.elapsed());
        }

        // println!("{:#?}", parsed_ast);
        let now = Instant::now();
        self.checker.type_check_file(&mut parsed_ast)?;
        if self.debug {
            println!("Type Checking took {:?}", now.elapsed());
        }
        // println!("{:#?}", parsed_ast);

        let now = Instant::now();
        self.codegen.generate_code(&parsed_ast)?;
        if self.debug {
            println!("Codegen took {:?}", now.elapsed());
        }
        // todo!();
        
        let now = Instant::now();
        self.codegen.compile()?;
        if self.debug {
            println!("Compiling took {:?}", now.elapsed());
        }
        // if self.run {
        //     let now = Instant::now();
        //     self.codegen.run()?;
        //     if self.debug {
        //         println!("Running took {:?}", now.elapsed());
        //     }
        // }
        Err(String::from("Technically yes, but we're not done yet"))
        // Ok(())
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
pub fn main() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}