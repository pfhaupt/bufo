use std::ffi::CString;
use std::collections::HashMap;
use std::ptr;

use crate::frontend::nodes;
use crate::frontend::flags::Flags;
use crate::internal_error;

use llvm_sys_160 as llvm;
use llvm::prelude::*;
use llvm::core::*;

use tracer::trace_call;
use crate::middleend::type_checker::Type;

struct ClassInfo {
    total_size: usize,
    field_offsets: HashMap<String, usize>,
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            total_size: 0,
            field_offsets: HashMap::new(),
        }
    }

    fn add_field(&mut self, field_name: &str, size: usize) {
        debug_assert!(self.field_offsets.get(field_name).is_none());
        self.field_offsets.insert(field_name.to_string(), self.total_size);
        self.total_size += size;
    }

    fn get_field_offset(&self, field_name: &str) -> usize {
        *self.field_offsets.get(field_name).expect("Field is guaranteed to be in here, because we typechecked before!")
    }
}

struct SizeManager {
    classes: HashMap<String, ClassInfo>,
}

impl SizeManager {
    fn new() -> Self {
        Self {
            classes: HashMap::new(),
        }
    }

    fn add_class(&mut self, class_name: &str) {
        debug_assert!(self.classes.get(class_name).is_none());
        self.classes.insert(class_name.to_string(), ClassInfo::new());
    }

    fn add_field(
        &mut self,
        class_name: &str,
        field_name: &str,
        typ: &Type,
    ) -> Result<(), String> {
        let size = typ.size()?;
        self.classes
            .get_mut(class_name)
            .expect("Class is guaranteed to be in here, because we typechecked before!")
            .add_field(field_name, size);
        Ok(())
    }

    fn get_class_info(&self, class_name: &str) -> &ClassInfo {
        self.classes.get(class_name).expect("Class is guaranteed to be in here, because we typechecked before!")
    }

    fn get_class_size(&self, class_name: &str) -> usize {
        self.get_class_info(class_name).total_size
    }
}

pub struct LLVMCodegen {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    class_defs: HashMap<String, LLVMTypeRef>,

    sm: SizeManager,
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

                class_defs: HashMap::new(),

                sm: SizeManager::new(),
                flags,
            }
        }
    }

    #[trace_call(always)]
    unsafe fn str_to_cstr(string: &str) -> CString {
        CString::new(string).unwrap()
    }

    #[trace_call(always)]
    unsafe fn fill_lookup(&mut self, root: &nodes::FileNode) -> Result<(), String> {
        for class in &root.classes {
            let class_struct_type_def = LLVMStructCreateNamed(
                self.context,
                Self::str_to_cstr(&class.name).as_ptr() as *const _
            );
            self.sm.add_class(&class.name);
            let fields = class.fields.iter().map(|field| {
               let typ = &field.type_def.typ;
                debug_assert!(*typ != Type::None);
                self.sm.add_field(&class.name, &field.name, typ)?;
                self.codegen_type(typ)
            }).collect::<Result<Vec<LLVMTypeRef>, String>>()?.as_mut_ptr();
            LLVMStructSetBody(class_struct_type_def, fields, class.fields.len() as u32, 0);
            self.class_defs.insert(class.name.clone(), class_struct_type_def);
            if self.flags.debug {
                println!("[DEBUG] Class `{}` has size {} bytes",
                    class.name,
                    self.sm.get_class_size(&class.name),
                );
            }
        }
        Ok(())
    }

    #[trace_call(always)]
    pub fn generate_code(&mut self, root: &nodes::FileNode) -> Result<(), String> {
        unsafe {
            self.fill_lookup(root)?;
            self.codegen_file(root)?;
            LLVMDumpModule(self.module);
        }
        Ok(())
    }

    #[trace_call(always)]
    unsafe fn codegen_file(&mut self, root: &nodes::FileNode) -> Result<(), String> {
        for class in &root.classes {
            self.codegen_class(class)?;
        }
        internal_error!("File codegen not implemented yet!")
    }

    #[trace_call(always)]
    unsafe fn codegen_class(&mut self, class: &nodes::ClassNode) -> Result<(), String> {
        for feature in &class.features {
            self.codegen_feature(feature)?;
        }
        internal_error!("Class codegen not implemented yet!")
    }

    #[trace_call(always)]
    unsafe fn codegen_feature(&mut self, feature: &nodes::FeatureNode) -> Result<(), String> {
        let final_name = format!("{}_{}", feature.class_name, feature.name);
        let name = Self::str_to_cstr(&final_name);

        // Prepare return type
        let ret_type = self.codegen_type(&feature.return_type.typ)?;

        // Prepare parameters as C-style array
        let param_count = feature.parameters.len() as u32;
        let params = feature.parameters.iter().map(|param| {
            self.codegen_type(&param.typ.typ).expect("Parameter type is guaranteed to be valid!")
        }).collect::<Vec<LLVMTypeRef>>().as_mut_ptr();

        // Create function type
        let func_type = LLVMFunctionType(ret_type, params, param_count, 0);

        // Create function
        let func = LLVMAddFunction(self.module, name.as_ptr() as *const _, func_type);

        // Create basic block and set it as the current one
        let bb = LLVMAppendBasicBlockInContext(self.context, func, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(self.builder, bb);

        self.codegen_constructor_block(feature)?;

        internal_error!("Feature codegen not implemented yet!")
    }

    #[trace_call(always)]
    unsafe fn codegen_constructor_block(&mut self, feature: &nodes::FeatureNode) -> Result<(), String> {
        debug_assert!(feature.is_constructor);
        // let this: Class = malloc(sizeof(Class));
        let class_struct = self.class_defs.get(&feature.class_name).unwrap();
        let this_alloc = LLVMBuildMalloc(
            self.builder,
            *class_struct,
            b"this\0".as_ptr() as *const _
        );
        // memset(this, 0, sizeof(Class));
        let this_zero = LLVMBuildMemSet(
            self.builder,
            this_alloc,
            LLVMConstInt(LLVMInt8TypeInContext(self.context), 0, 0),
            LLVMSizeOf(*class_struct),
            0
        );
        LLVMDumpModule(self.module);
        internal_error!("Constructor block codegen not implemented yet!")
    }


    #[trace_call(always)]
    unsafe fn codegen_type(&mut self, typ: &Type) -> Result<LLVMTypeRef, String> {
        match typ {
            Type::None => Ok(LLVMVoidTypeInContext(self.context)),
            Type::I32 | Type::U32 => Ok(LLVMInt32TypeInContext(self.context)),
            e => todo!("Type codegen not implemented for {:?}", e)
        }
    }

    #[trace_call(always)]
    pub fn run(&mut self, _ir: String) -> Result<(), String> {
        internal_error!("Codegen not implemented yet!")
    }
}