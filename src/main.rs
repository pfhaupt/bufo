mod codegen;
mod lexer;
mod parser;
mod flags;

use std::time::Instant;

use flags::RUN_KEY;

use crate::codegen::Generator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::flags::{Flag, FlagParser, INPUT_KEY, DEBUG_KEY};

fn compile() -> Result<(), String> {
    let now = Instant::now();

    let flags = FlagParser::init_flags().parse_flags()?;
    
    let path = match flags.get(INPUT_KEY).unwrap() {
        Flag::InputFlag { path } => path.as_ref().unwrap(),
        _ => unreachable!()
    };
    let run = match flags.get(RUN_KEY).unwrap() {
        Flag::RunFlag { run } => *run,
        _ => unreachable!()
    };
    let debug = match flags.get(DEBUG_KEY).unwrap() {
        Flag::DebugFlag { debug } => *debug,
        _ => unreachable!()
    };
    if debug { println!("Parsing flags took {:?}", now.elapsed()); }

    let mut lexer = Lexer::new(path, debug)?;
    lexer.tokenize()?;
    if debug { println!("Tokenizing took {:?}", now.elapsed()); };

    let mut parser = Parser::new(path, lexer.get_tokens(), debug);
    let ast = parser.parse_file()?;
    // ast.print_debug();
    // exit(1);
    if debug { println!("Parsing took {:?}", now.elapsed()); }

    let mut generator = Generator::new(ast, debug)?;
    if debug { println!("Generating Code took {:?}", now.elapsed()); }
    // generator.compile()?;
    if run {
        generator.interpret()?;
        if debug { println!("Interpreting Code took {:?}", now.elapsed()); }
    };

    
    Ok(())
}

fn main() {
    if let Err(e) = compile() {
        println!("{}", e);
        return;
    }
}
