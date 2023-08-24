mod checker;
mod codegen;
mod flags;
mod lexer;
mod parser;

use std::time::Instant;

use flags::RUN_KEY;

// use crate::codegen::Generator;
use crate::checker::TypeChecker;
use crate::flags::{Flag, FlagParser, DEBUG_KEY, INPUT_KEY};
use crate::lexer::Lexer;
use crate::parser::Parser;

fn compile() -> Result<(), String> {
    let now = Instant::now();
    let flags = FlagParser::init_flags().parse_flags()?;
    let path = match flags.get(INPUT_KEY).unwrap() {
        Flag::InputFlag { path } => path.as_ref().unwrap(),
        _ => unreachable!(),
    };
    let run = match flags.get(RUN_KEY).unwrap() {
        Flag::RunFlag { run } => *run,
        _ => unreachable!(),
    };
    let debug = match flags.get(DEBUG_KEY).unwrap() {
        Flag::DebugFlag { debug } => *debug,
        _ => unreachable!(),
    };
    if debug {
        println!("Parsing flags took {:?}", now.elapsed());
    }

    let now = Instant::now();
    let mut lexer = Lexer::new(path, debug)?;
    lexer.tokenize()?;
    if debug {
        println!("Tokenizing took {:?}", now.elapsed());
    };

    let now = Instant::now();
    let mut parser = Parser::new(path, lexer.get_tokens(), debug);
    let ast = parser.parse_file()?;
    // ast.print_debug();
    if debug {
        println!("Parsing took {:?}", now.elapsed());
    }

    let now = Instant::now();
    let mut type_checker = TypeChecker::new(&ast, debug);
    let ast = type_checker.type_check_program()?;
    // ast.print_debug();
    if debug {
        println!("Type Checking took {:?}", now.elapsed());
    }
    todo!();

    // let now = Instant::now();
    // let mut generator = Generator::new(ast, debug)?;
    // if debug { println!("Generating Code took {:?}", now.elapsed()); }
    // // generator.compile()?;
    // if run {
    //     let now = Instant::now();
    //     generator.interpret()?;
    //     if debug { println!("Interpreting Code took {:?}", now.elapsed()); }
    // };

    Ok(())
}

fn main() {
    if let Err(e) = compile() {
        println!("{}", e);
        return;
    }
}
