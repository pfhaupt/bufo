mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() -> Result<(), String> {
    let path = String::from("src/test.bu");
    let mut lexer = Lexer::new(&path)?;
    println!("{:?}", lexer);
    lexer.tokenize()?;
    let mut parser = Parser::new();
    parser.set_context(lexer.get_tokens());
    parser.parse()?;
    println!("{:#?}", parser);
    Ok(())
}
