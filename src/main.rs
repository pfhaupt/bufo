mod lexer;
mod parser;
mod codegen;

use std::process::exit;
use std::time::Instant;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::Generator;

fn compile() -> Result<(), String> {
    let path = String::from("src/test.bu");

    let mut lexer = Lexer::new(&path)?;
    lexer.tokenize()?;
    // println!("{:?}", lexer.get_tokens());

    let mut parser = Parser::new(lexer.get_tokens());
    parser.parse_file()?;
    let ast = parser.build_tree();
    // println!("{:#?}", ast);

    let mut generator = Generator::new(ast)?;
    // generator.compile()?;
    generator.interpret()?;
    Ok(())
}

fn main() {
    let now = Instant::now();
    if let Err(e) = compile() {
        println!("{}", e);
        exit(1);
    }
    println!("{:?}", now.elapsed());
}
