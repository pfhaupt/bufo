use std::time::Instant;

#[cfg(not(feature = "old_codegen"))]
use inkwell::context::Context;
use tracer::trace_call;

#[cfg(feature = "old_codegen")]
use crate::backend::assembler::Assembler;
#[cfg(feature = "old_codegen")]
use crate::backend::codegen::Codegen;
#[cfg(not(feature = "old_codegen"))]
use crate::backend::codegen_llvm::LLVMCodegen;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::pp;
use crate::middleend::flow_checker::FlowChecker;
use crate::middleend::type_checker::TypeChecker;
use crate::util::printer::Printer;
use crate::util::flags::Flags;

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

pub const FILE_EXT: &str = "bufo";

#[macro_export]
macro_rules! internal_panic {
    () => {
        panic!(
            "INTERNAL PANIC AT {}:{}:{}\n{}: This is a bug in the compiler, please report it in the issue tracker at the GitHub repository.",
            file!(),
            line!(),
            column!(),
            crate::compiler::ERR_STR
        )
    };
    ($($arg:tt)*) => {
        panic!(
            "INTERNAL PANIC AT {}:{}:{}: {}\n{}: This is a bug in the compiler, please report it in the issue tracker at the GitHub repository.",
            file!(),
            line!(),
            column!(),
            format!($($arg)*),
            crate::compiler::ERR_STR
        )
    };
}

#[cfg(feature = "old_codegen")]
pub struct Compiler<'flags> {
    parser: Parser<'flags>,
    type_checker: TypeChecker<'flags>,
    flow_checker: FlowChecker<'flags>,
    codegen: Codegen<'flags>,
    assembler: Assembler<'flags>,
    flags: &'flags Flags,
}

#[cfg(feature = "old_codegen")]
impl<'flags> Compiler<'flags> {
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            parser: Parser::new(flags),
            type_checker: TypeChecker::new(flags),
            flow_checker: FlowChecker::new(flags),
            codegen: Codegen::new(flags),
            assembler: Assembler::new(flags),
            flags: flags
        }
    }

    #[trace_call(always)]
    pub fn run_everything(&mut self) -> Result<(), String> {
        let now = Instant::now();
        let mut parsed_ast = self.parser.parse_file()?;
        if self.flags.verbose {
            println!("[INFO] Parsing took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.type_checker.type_check_file(&mut parsed_ast)?;
        if self.flags.verbose {
            println!("[INFO] Type Checking took {:?}", now.elapsed());
        }

        if self.flags.print_ast {
            Printer::print(&parsed_ast);
        }

        let now = Instant::now();
        self.flow_checker.check_file(&mut parsed_ast)?;
        if self.flags.verbose {
            println!("[INFO] Flow Checking took {:?}", now.elapsed());
        }

        let now = Instant::now();
        let ir = self.codegen.generate_code(&parsed_ast)?;
        if self.flags.verbose {
            println!("[INFO] Codegen took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.assembler.generate_x86_64(ir)?;
        if self.flags.verbose {
            println!("[INFO] Assembling took {:?}", now.elapsed());
        }
        if self.flags.run {
            let now = Instant::now();
            self.assembler.run()?;
            if self.flags.verbose {
                println!("[INFO] Running took {:?}", now.elapsed());
            }
        }
        Ok(())
    }
}

#[cfg(not(feature = "old_codegen"))]
#[trace_call(always)]
pub fn run_everything(
    flags: &Flags,
    context: &Context,
) -> Result<(), String> {
    let source_code: String = pp::load_project(&flags)?;
    if flags.debug {
        println!("[DEBUG] The preprocessed source:\n{source_code}");
    }
    let mut lexer = Lexer::new();
    let mut parser = Parser::new(&flags, &mut lexer, &source_code);
    let mut type_checker = TypeChecker::new(&flags);
    let mut flow_checker = FlowChecker::new(&flags);
    let mut codegen = LLVMCodegen::new(&flags, &context);
    let now = Instant::now();
    let mut parsed_ast = parser.parse_project()?;
    if flags.verbose {
        println!("[INFO] Parsing took {:?}", now.elapsed());
    }

    if flags.print_ast {
        Printer::print(&parsed_ast);
    }

    let now = Instant::now();
    type_checker.type_check_project(&mut parsed_ast)?;
    if flags.verbose {
        println!("[INFO] Type Checking took {:?}", now.elapsed());
    }

    if flags.print_ast {
        Printer::print(&parsed_ast);
    }

    let now = Instant::now();
    flow_checker.check_project(&mut parsed_ast)?;
    if flags.verbose {
        println!("[INFO] Flow Checking took {:?}", now.elapsed());
    }

    let now = Instant::now();
    codegen.codegen_project(&parsed_ast)?;
    if flags.verbose {
        println!("[INFO] Codegen took {:?}", now.elapsed());
    }

    if flags.run {
        let now = Instant::now();
        codegen.run()?;
        if flags.verbose {
            println!("[INFO] Running took {:?}", now.elapsed());
        }
    }
    Ok(())
}
#[trace_call(always)]
fn compile() -> Result<(), String> {
    let now = Instant::now();
    let mut flags = Flags::parse_flags();
    flags.imports.push(String::from("."));
    if flags.verbose {
        println!("[INFO] Parsing flags took {:?}", now.elapsed());
    }
    if flags.gen_bind.is_some() {
        let now = Instant::now();
        let mut bindgen = crate::util::bindgen::Bindgen::new(&flags);
        bindgen.run()?;
        if flags.verbose {
            println!("[INFO] Bindgen took {:?}", now.elapsed());
        }
        return Ok(());
    }
    #[cfg(feature = "old_codegen")]
    {
        let mut compiler = Compiler::new(&flags);
        compiler.run_everything()
    }
    #[cfg(not(feature = "old_codegen"))]
    {
        let context = Context::create();
        run_everything(&flags, &context)
    }
}
#[trace_call(always)]
pub fn run() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
