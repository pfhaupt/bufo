use std::ffi::CString;

use crate::frontend::nodes;
use crate::frontend::flags::Flags;
use crate::internal_error;

use llvm_sys_160 as llvm;
use llvm::prelude::*;
use llvm::core::*;

use tracer::trace_call;

pub struct LLVMCodegen {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    flags: Flags,
}

impl LLVMCodegen {
    pub fn new(flags: Flags) -> Self {
        let module_name = flags.input.clone();
        unsafe {
            let context = LLVMContextCreate();
            let mod_name = Self::str_to_cstr(&module_name);
            Self {
                context,
                module: LLVMModuleCreateWithName(mod_name.as_ptr() as *const _),
                builder: LLVMCreateBuilderInContext(context),
                flags,
            }
        }
    }

    #[trace_call(always)]
    unsafe fn str_to_cstr(string: &str) -> CString {
        CString::new(string).unwrap()
    }

    #[trace_call(always)]
    pub fn generate_code(&mut self, root: &nodes::FileNode) -> Result<(), String> {
        unsafe {
            self.codegen_file(root)?;
            LLVMDumpModule(self.module);
        }
        Ok(())
    }

    #[trace_call(always)]
    unsafe fn codegen_file(&mut self, _root: &nodes::FileNode) -> Result<(), String> {
        internal_error!("Codegen not implemented yet!")
    }

    #[trace_call(always)]
    pub fn run(&mut self, _ir: String) -> Result<(), String> {
        internal_error!("Codegen not implemented yet!")
    }
}