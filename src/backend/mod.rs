
#[cfg(feature = "old_codegen")]
pub mod assembler;
#[cfg(feature = "old_codegen")]
pub mod codegen;
#[cfg(feature = "old_codegen")]
pub mod instr;
#[cfg(not(feature = "old_codegen"))]
pub mod codegen_llvm;
