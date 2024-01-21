use std::time::Instant;

#[cfg(feature = "llvm")]
use inkwell::context::Context;
use tracer::trace_call;

#[cfg(not(feature = "llvm"))]
use crate::backend::assembler::Assembler;
#[cfg(not(feature = "llvm"))]
use crate::backend::codegen::Codegen;
#[cfg(feature = "llvm")]
use crate::backend::codegen_llvm::LLVMCodegen;
use crate::frontend::parser::Parser;
use crate::middleend::flow_checker::FlowChecker;
use crate::middleend::type_checker::TypeChecker;
use crate::util::printer::Printer;
use crate::util::flags::Flags;

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

pub const OUTPUT_FOLDER: &str = "./out/";
pub const FILE_EXT: &str = ".bu";

#[macro_export]
macro_rules! internal_panic {
    ($msg:expr) => {
        panic!(
            "INTERNAL PANIC AT {}:{}:{}: {}\n{}: This is a bug in the compiler, please report it in the issue tracker at the GitHub repository.",
            file!(),
            line!(),
            column!(),
            $msg,
            crate::compiler::ERR_STR
        )
    };
}

#[cfg(not(feature = "llvm"))]
pub struct Compiler<'flags> {
    parser: Parser<'flags>,
    type_checker: TypeChecker<'flags>,
    flow_checker: FlowChecker<'flags>,
    codegen: Codegen<'flags>,
    assembler: Assembler<'flags>,
    flags: &'flags Flags,
}

#[cfg(not(feature = "llvm"))]
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
        if self.flags.debug {
            println!("[DEBUG] Parsing took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.type_checker.type_check_file(&mut parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Type Checking took {:?}", now.elapsed());
        }

        if self.flags.print_ast {
            Printer::print(&parsed_ast);
        }

        let now = Instant::now();
        self.flow_checker.check_file(&mut parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Flow Checking took {:?}", now.elapsed());
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

#[cfg(feature = "llvm")]
pub struct Compiler<'flags, 'ctx> {
    parser: Parser<'flags>,
    type_checker: TypeChecker<'flags>,
    flow_checker: FlowChecker<'flags>,
    codegen: LLVMCodegen<'flags, 'ctx>,
    flags: &'flags Flags,
}

#[cfg(feature = "llvm")]
impl<'flags, 'ctx> Compiler<'flags, 'ctx> {
    pub fn new(flags: &'flags Flags, context: &'ctx Context) -> Self {
        Self {
            parser: Parser::new(flags),
            type_checker: TypeChecker::new(flags),
            flow_checker: FlowChecker::new(flags),
            codegen: LLVMCodegen::new(flags, context),
            flags
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
        self.type_checker.type_check_file(&mut parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Type Checking took {:?}", now.elapsed());
        }

        if self.flags.print_ast {
            Printer::print(&parsed_ast);
        }

        let now = Instant::now();
        self.flow_checker.check_file(&mut parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Flow Checking took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.codegen.create_executable(&parsed_ast)?;
        if self.flags.debug {
            println!("[DEBUG] Codegen took {:?}", now.elapsed());
        }

        if self.flags.run {
            let now = Instant::now();
            self.codegen.run()?;
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
    #[cfg(not(feature = "llvm"))]
    {
        let mut compiler = Compiler::new(&flags);
        compiler.run_everything()
    }
    #[cfg(feature = "llvm")]
    {
        let context = Context::create();
        let mut compiler = Compiler::new(&flags, &context);
        compiler.run_everything()
    }
}
#[trace_call(always)]
pub fn run() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
