#![allow(unused, unreachable_code)]

mod checker;
mod codegen;
mod flags;
mod lexer;
mod parser;
mod desugar;

use std::time::Instant;

use flags::RUN_KEY;

use crate::checker::TypeChecker;
use crate::codegen::Generator;
use crate::flags::{Flag, FlagParser, DEBUG_KEY, INPUT_KEY};
use crate::lexer::Lexer;
use crate::parser::{Parser, Tree};
use crate::desugar::Desugarer;

pub struct Compiler {
    lexer: Lexer,
    parser: Parser,
    desugarer: Desugarer,
    checker: TypeChecker,
    codegen: Generator
}

impl Compiler {
    pub fn new(path: &String, debug: bool, run: bool) -> Result<Self, String> {
        Ok(Self {
            lexer: Lexer::new().origin(path)?.debug(debug),
            parser: Parser::new().origin(path).debug(debug),
            desugarer: Desugarer::new(),
            checker: TypeChecker::new(debug),
            codegen: Generator::new(debug)
        })
    }

    pub fn parse_snippet(snippet: &String) -> Result<Tree, String> {
        let mut lexer = Lexer::new();
        lexer.set_source(snippet);
        lexer.set_origin_unchecked(&String::from("anonymous"));
        lexer.tokenize()?;

        let mut parser = Parser::new().origin(&String::from("anonymous"));
        parser.set_tokens(lexer.get_tokens());
        let parsed_ast = parser.parse_snippet()?;

        let mut desugarer = Desugarer::new();
        desugarer.desugar_tree(parsed_ast)
    }

    pub fn run_everything(&mut self) -> Result<(), String> {
        self.lexer.tokenize()?;
        let tokens = self.lexer.get_tokens();
        
        self.parser.set_tokens(tokens);
        let parsed_ast = self.parser.parse_file()?;

        let mut desugared_ast = self.desugarer.desugar_tree(parsed_ast)?;

        self.checker.set_ast(desugared_ast);
        let checked_ast = self.checker.type_check_program()?;

        checked_ast.print_debug();
        println!("{}", checked_ast.rebuild_code());
        todo!();

        self.codegen.set_ast(checked_ast);
        self.codegen.compile()?;

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
    compiler.run_everything()?;

    // let now = Instant::now();
    // let mut lexer = Lexer::new(path, debug)?;
    // lexer.tokenize()?;
    // if debug {
    //     println!("Tokenizing took {:?}", now.elapsed());
    // };
    // // for t in &lexer.get_tokens() {
    // //     println!("{:?} {}", t.get_type(), t.get_value());
    // // }
    // // todo!();

    // let now = Instant::now();
    // let mut parser = Parser::new(path, lexer.get_tokens(), debug);
    // let parsed_ast = parser.parse_file()?;
    // if debug {
    //     println!("Parsing took {:?}", now.elapsed());
    // }
    // parsed_ast.print_debug();

    // let desugared_ast = Desugarer::desugar_tree(parsed_ast)?;
    // desugared_ast.print_debug();
    // println!("{}", desugared_ast.rebuild_code());
    // todo!();

    // let now = Instant::now();
    // let mut type_checker = TypeChecker::new(&desugared_ast, debug);
    // let typed_ast = type_checker.type_check_program()?;
    // if debug {
    //     println!("Type Checking took {:?}", now.elapsed());
    // }

    // // typed_ast.print_debug();
    // // typed_ast.rebuild_code();
    // // todo!();

    // let now = Instant::now();
    // let mut generator = Generator::new(typed_ast, debug)?;
    // if debug {
    //     println!("Generating Code took {:?}", now.elapsed());
    // }
    // let now = Instant::now();
    // generator.interpret()?;
    // if debug {
    //     println!("Compiling Code took {:?}", now.elapsed());
    // }
    // if run {
    //     let now = Instant::now();
    //     generator.run()?;
    //     if debug {
    //         println!("Running Code took {:?}", now.elapsed());
    //     }
    // }

    Ok(())
}

fn main() {
    if let Err(e) = compile() {
        println!("{}", e);
    }
}
