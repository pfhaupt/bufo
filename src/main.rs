mod codegen;
mod lexer;
mod parser;

use std::process::exit;
use std::time::Instant;

use crate::codegen::Generator;
use crate::lexer::Lexer;
use crate::parser::Parser;

/*
TODO-List:
    Rewrite Grammar to make call expr, instead of stmt
        -> currently the compiler crashes if you wanna do something like f(f(10))
        -> this is because fn calls are treated as statements, instead of expressions
        -> this means you can also not do something like `let a = f(10);`
    Introduce Scope
        ->  The following code snippet should not work:
                if (cond) {
                    let a = 0;
                }
                a = 4;
            It should fail in line 4 with "Undefined variable `a`"
*/

fn compile() -> Result<(), String> {
    let path = String::from("src/test.bu");

    let mut lexer = Lexer::new(&path)?;
    lexer.tokenize()?;
    // println!("{:#?}", lexer.get_tokens());

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
