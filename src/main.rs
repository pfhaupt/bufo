mod lexer;
mod parser;
mod codegen;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::Generator;

fn main() -> Result<(), String> {
    let path = String::from("src/test.bu");

    let mut lexer = Lexer::new(&path)?;
    lexer.tokenize()?;

    let mut parser = Parser::new();
    parser.set_context(lexer.get_tokens());
    parser.parse()?;
    let program = parser.get_program();

    let mut generator = Generator::new(program, 3);
    generator.interpret()?;

    Ok(())
}
