use std::time::Instant;

use tracer::trace_call;

use crate::backend::assembler::Assembler;
use crate::backend::codegen::Codegen;
use crate::frontend::flags::Flags;
use crate::frontend::parser::Parser;
use crate::middleend::type_checker::TypeChecker;

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

pub const OUTPUT_FOLDER: &str = "./out/";
pub const FILE_EXT: &str = ".bu";

pub const CONSTRUCTOR_NAME: &str = "new";

pub const BUILT_IN_FEATURES: [&str; 1] = [
    CONSTRUCTOR_NAME
];

#[macro_export]
macro_rules! internal_error {
    ($msg:expr) => {
        Err(format!(
            "INTERNAL ERROR AT {}:{}:{}: {}",
            file!(),
            line!(),
            column!(),
            $msg
        ))
    };
}

pub struct Compiler {
    parser: Parser,
    checker: TypeChecker,
    codegen: Codegen,
    assembler: Assembler,
    flags: Flags,
}

impl Compiler {
    pub fn new(flags: Flags) -> Self {
        Self {
            parser: Parser::new(flags.clone()),
            checker: TypeChecker::new(flags.clone()),
            codegen: Codegen::new(flags.clone()),
            assembler: Assembler::new(flags.clone()),
            flags: flags.clone()
        }
    }

    #[trace_call(always)]
    pub fn run_everything(&mut self) -> Result<(), String> {
        let now = Instant::now();
        let mut parsed_ast = self.parser.parse_file()?;
        if self.flags.debug {
            println!("[DEBUG] Parsing took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.checker.type_check_file(&mut parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Type Checking took {:?}", now.elapsed());
        }

        let now = Instant::now();
        let ir = self.codegen.generate_code(&parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Codegen took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.assembler.generate_x86_64(ir)?;
        if self.flags.debug {
            println!("[DEBUG] Assembling took {:?}", now.elapsed());
        }
        if self.flags.run {
            let now = Instant::now();
            self.assembler.run()?;
            if self.flags.debug {
                println!("[DEBUG] Running took {:?}", now.elapsed());
            }
        }
        Ok(())
    }
}

#[trace_call(always)]
fn compile() -> Result<(), String> {
    let now = Instant::now();
    let flags = Flags::parse_flags();
    if flags.debug {
        println!("[DEBUG] Parsing flags took {:?}", now.elapsed());
    }
    let mut compiler = Compiler::new(flags);
    compiler.run_everything()
}
#[trace_call(always)]
pub fn run() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
