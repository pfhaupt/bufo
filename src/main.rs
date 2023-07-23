use crate::lexer::Lexer;

mod lexer;

fn main() -> Result<(), String> {
    let path = String::from("src/test.bu");
    let mut lexer = Lexer::new(&path)?;
    println!("{:?}", lexer);
    lexer.tokenize()?;
    Ok(())
}
