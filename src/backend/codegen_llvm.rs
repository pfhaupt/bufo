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

/*
Macro to generate all necessary LLVM code for setting up a function:
- Get correct return type
- Codegen parameters
- Generate function type
- Create Function
- Set Builder to insert instructions in the basic block of that function
 */
macro_rules! codegen_function_header {
    ($codegen:ident, $function:ident, $name:ident) => {
        // Prepare return type
        let ret_type = $codegen.codegen_type(&$function.return_type.typ)?;

        // Prepare parameters as C-style array
        let param_count = $function.parameters.len() as u32;
        let params = $function.parameters.iter().map(|param| {
            $codegen.codegen_type(&param.typ.typ).expect("Parameter type is guaranteed to be valid!")
        }).collect::<Vec<LLVMTypeRef>>().as_mut_ptr();

        // Create function type
        let func_type = LLVMFunctionType(ret_type, params, param_count, 0);

        // Create function
        let func = LLVMAddFunction($codegen.module, $name.as_ptr() as *const _, func_type);

        for (i, param) in $function.parameters.iter().enumerate() {
            let param_name = Self::str_to_cstr(&param.name);
            let param_value = LLVMGetParam(func, i as u32);
            LLVMSetValueName2(param_value, param_name.as_ptr() as *const _, param_name.as_bytes().len());
            $codegen.add_variable(&param.name, param_value);
        }

        // Create basic block and set it as the current one
        let bb = LLVMAppendBasicBlockInContext($codegen.context, func, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd($codegen.builder, bb);
    }
}

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
    stack_scopes: Vec<HashMap<String, LLVMValueRef>>,

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
                stack_scopes: vec![],

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

    #[trace_call(extra)]
    fn enter_scope(&mut self) {
        self.stack_scopes.push(HashMap::new())
    }

    #[trace_call(extra)]
    fn leave_scope(&mut self) {
        debug_assert!(self.stack_scopes.len() > 0);
        self.stack_scopes.pop();
    }

    #[trace_call(extra)]
    fn add_variable(&mut self, name: &str, value: LLVMValueRef) {
        debug_assert!(self.stack_scopes.len() > 0);
        self.stack_scopes.last_mut().unwrap().insert(name.to_owned(), value);
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
        self.enter_scope();
        let final_name = format!("{}_{}", feature.class_name, feature.name);
        let name = Self::str_to_cstr(&final_name);

        codegen_function_header!(self, feature, name);
        // LLVM is now set up to codegen instructions for the body of the function

        if feature.is_constructor {
            // LLVM does not malloc by size, but by Struct
            // So we need to get the stored class definition
            let class_struct = self.class_defs.get(&feature.class_name).unwrap();

            // let this: Class = malloc(Class);
            let this_alloc = LLVMBuildMalloc(
                self.builder,
                *class_struct,
                b"this\0".as_ptr() as *const _
            );
            // memset(this, 0, sizeof(Class));
            // Memset does not return a value
            let _this_zero = LLVMBuildMemSet(
                self.builder,
                this_alloc,
                LLVMConstInt(LLVMInt8TypeInContext(self.context), 0, 0),
                LLVMSizeOf(*class_struct),
                0
            );

            // Actual code we wrote
            self.codegen_block(&feature.block)?;

            // return this;
            LLVMBuildRet(self.builder, this_alloc);
        } else {
            todo!("Handle non-constructor feature")
        }
        self.leave_scope();
        Ok(())
    }

    #[trace_call(always)]
    unsafe fn codegen_block(&mut self, block: &nodes::BlockNode) -> Result<(), String> {
        self.enter_scope();
        for stmt in &block.statements {
            self.codegen_statement(stmt)?;
        }
        self.leave_scope();
        Ok(())
    }

    #[trace_call(always)]
    unsafe fn codegen_statement(&mut self, stmt: &nodes::Statement) -> Result<(), String> {
        match stmt {
            nodes::Statement::Return(ret) => self.codegen_return(ret),
            s => todo!("{:?}", s)
        }
    }

    #[trace_call(always)]
    unsafe fn codegen_return(&mut self, ret: &nodes::ReturnNode) -> Result<(), String> {
        if let Some(ret_val) = &ret.return_value {
            let value = self.codegen_expression_node(ret_val)?;
            LLVMBuildRet(self.builder, value);
        } else {
            LLVMBuildRetVoid(self.builder);
        }
        Ok(())
    }

    #[trace_call(always)]
    unsafe fn codegen_expression_node(&mut self, expr: &nodes::ExpressionNode) -> Result<LLVMValueRef, String> {
        self.codegen_expression(&expr.expression)
    }

    #[trace_call(always)]
    unsafe fn codegen_expression(&mut self, expression: &nodes::Expression) -> Result<LLVMValueRef, String> {
        internal_error!("Expression codegen not implemented yet!")
    }

    #[trace_call(always)]
    unsafe fn codegen_type(&mut self, typ: &Type) -> Result<LLVMTypeRef, String> {
        match typ {
            Type::None => Ok(LLVMVoidTypeInContext(self.context)),
            Type::I32 | Type::U32 => Ok(LLVMInt32TypeInContext(self.context)),
            Type::Class(_) => Ok(LLVMPointerTypeInContext(self.context, 0)),
            e => todo!("Type codegen not implemented for {:?}", e)
        }
    }

    #[trace_call(always)]
    pub fn run(&mut self, _ir: String) -> Result<(), String> {
        internal_error!("Codegen not implemented yet!")
    }
}