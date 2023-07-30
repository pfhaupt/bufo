mod lexer;
mod parser;
mod codegen;

use std::time::Instant;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::Generator;

fn main() -> Result<(), String> {
    let now = Instant::now();
    let path = String::from("src/test.bu");

    let mut lexer = Lexer::new(&path)?;
    lexer.tokenize()?;
    println!("{:?}", lexer.get_tokens());

    let mut parser = Parser::new(lexer.get_tokens());
    parser.parse_file();
    let ast = parser.build_tree();
    println!("{:#?}", ast);
    // let program = parser.get_program();

    // let mut generator = Generator::new(program, 3);
    // generator.interpret()?;

    println!("{:?}", now.elapsed());
    Ok(())
}
