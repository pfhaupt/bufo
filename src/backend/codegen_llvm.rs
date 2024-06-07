use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::frontend::nodes;
use crate::frontend::parser::Operation;
use crate::util::flags::Flags;
use crate::compiler::ERR_STR;
use crate::internal_panic;
use crate::util::opt_flags::OptimizationLevel;

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::BuilderError;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
use inkwell::targets::{Target, InitializationConfig, CodeModel, RelocMode, TargetTriple, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, InstructionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::values::BasicValueEnum;

use tracer::trace_call;
use crate::middleend::type_checker::Type;

macro_rules! is_int {
    ($typ:expr) => {
        $typ == Type::I8 || $typ == Type::I16 || $typ == Type::I32 || $typ == Type::I64 ||
        $typ == Type::U8 || $typ == Type::U16 || $typ == Type::U32 || $typ == Type::U64 || $typ == Type::Usize
    };
}

macro_rules! assert_is_int {
    ($lhs:expr, $rhs:expr) => {
        if !(is_int!($lhs.get_type()) && is_int!($rhs.get_type())) {
            internal_panic!(
                "Expected integers as operands of binary operation, got:\nlhs: {:?}\nrhs: {:?}",
                $lhs.get_type(),
                $rhs.get_type()
            );
        }
    };
}

macro_rules! fill_function_lookup {
    ($codegen:ident, $function:ident, $name:ident) => {
        {
            // Prepare parameter types
            let mut param_types = Vec::new();
            for param in &$function.parameters {
                let codegened_type = $codegen.codegen_type_node(&param.typ);
                if codegened_type.is_struct_type() {
                    let real_type = $codegen.struct_type_for_arg(&codegened_type);
                    param_types.push(real_type.into());
                } else {
                    param_types.push(codegened_type.into());
                }
            }
            // Prepare return type
            let function_type;
            if $function.return_type.typ == Type::None {
                let return_type = $codegen.context.void_type();
                function_type = return_type.fn_type(&param_types, $function.is_vararg);
            } else {
                let return_type = $codegen.codegen_type(&$function.return_type.typ);
                function_type = return_type.fn_type(&param_types, $function.is_vararg);
            }
            let _function = $codegen.module.add_function(&$name, function_type, None);
            if $codegen.flags.debug {
                println!("[DEBUG] Added function {0} to module", $name);
            }
            _function
        }
    };
}

macro_rules! codegen_function_header {
    ($codegen:ident, $function:ident, $name:ident) => {
        let llvm_func = $codegen.module.get_function(&$name).unwrap();
        let entry = $codegen.context.append_basic_block(llvm_func, "entry");
        $codegen.builder.position_at_end(entry);
        for (i, param) in $function.parameters.iter().enumerate() {
            let func_param = llvm_func.get_nth_param(i as u32).unwrap();
            let param_type = $codegen.codegen_type_node(&param.typ);
            if param_type.is_struct_type() {
                let typ = $codegen.struct_type_for_arg(&param_type);
                if typ.is_pointer_type() {
                    debug_assert!(func_param.is_pointer_value());
                    func_param.set_name(&param.name);
                    let func_param = $codegen.load_value_from_ptr(param_type, func_param.into_pointer_value(), &param.name)?;
                    let param_alloc = $codegen.allocate(func_param.get_type(), &param.name)?;
                    $codegen.add_variable(&param.name, param_alloc.into());
                    $codegen.store_value_in_ptr(param_alloc, func_param)?;
                } else {
                    func_param.set_name(&param.name);
                    let param_alloc = $codegen.allocate(param_type, &param.name)?;
                    $codegen.add_variable(&param.name, param_alloc.into());
                    $codegen.store_value_in_ptr(param_alloc, func_param)?;
                }
            } else {
                func_param.set_name(&param.name);
                let param_alloc = $codegen.allocate(param_type, &param.name)?;
                $codegen.add_variable(&param.name, param_alloc.into());
                $codegen.store_value_in_ptr(param_alloc, func_param)?;
            }
        }
    };
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: Vec<(String, Type)>,
}

impl StructInfo {
    fn new() -> Self {
        Self {
            fields: Vec::new(),
        }
    }

    fn add_field(&mut self, field_name: &str, typ: &Type) {
        for (name, _) in &self.fields {
            debug_assert!(name != field_name);
        }
        self.fields.push((field_name.to_string(), typ.clone()));
    }

    fn get_field_index(&self, field_name: &str) -> usize {
        for (i, (name, _)) in self.fields.iter().enumerate() {
            if name == field_name {
                return i;
            }
        }
        internal_panic!("Field {} not found in struct!", field_name);
    }
}

pub struct LLVMCodegen<'flags, 'ctx> {
    filename: PathBuf,
    exename: PathBuf,
    context: &'ctx Context,
    module_stack: Vec<String>,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,

    stack_scopes: Vec<HashMap<String, BasicValueEnum<'ctx>>>,
    loop_blocks: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>, // (loop, after_loop)
    struct_defs: HashMap<String, StructType<'ctx>>,
    struct_info: HashMap<String, StructInfo>,

    flags: &'flags Flags,
    clang_flags: Vec<String>,
}
impl<'flags, 'ctx> LLVMCodegen<'flags, 'ctx> {
    pub fn new(flags: &'flags Flags, context: &'ctx Context) -> Self {
        let filename = PathBuf::from(&flags.input);
        let filename = filename.file_name().unwrap().to_str().unwrap().to_string();
        let outname = PathBuf::from("./out/");
        let filename = outname.join(filename);
        let exename = filename.with_extension("exe");
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target_str = Self::get_target().unwrap();
        let target = Target::from_name("x86-64").unwrap();
        let target_triple = TargetTriple::create(&target_str);
        let target_machine = target.create_target_machine(
            &target_triple,
            "x86-64",
            "",
            inkwell::OptimizationLevel::from(&flags.optimizations.level),
            RelocMode::Default,
            CodeModel::Default,
        ).unwrap();
        let module_name = flags.input.clone();
        let module = context.create_module(&module_name);
        module.set_triple(&target_triple);
        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        let builder = context.create_builder();
        Self {
            filename,
            exename,
            context,
            module_stack: Vec::new(),
            module,
            builder,
            target_machine,
            stack_scopes: Vec::new(),
            loop_blocks: Vec::new(),
            struct_defs: HashMap::new(),
            struct_info: HashMap::new(),
            flags,
            clang_flags: Vec::new(),
        }
    }

    fn get_target() -> Result<String, String> {
        println!("[INFO] Getting host target");
        let output = std::process::Command::new("rustc")
            .arg("-vV")
            .output()
            .expect("Failed to run rustc to get the host target");
        let output = std::str::from_utf8(&output.stdout).expect("`rustc -vV` didn't return utf8 output");

        let field = "host: ";
        let host = output
            .lines()
            .find(|l| l.starts_with(field))
            .map(|l| &l[field.len()..])
            .ok_or_else(|| {
                format!(
                    "`rustc -vV` didn't have a line for `{}`, got:\n{}",
                    field.trim(),
                    output
                )
            })?
            .to_string();
        println!("[INFO] Host target is {}", host);
        Ok(host)
    }

    #[trace_call(always)]
    fn fill_struct_lookup(&mut self, file: &nodes::ModuleNode) -> Result<(), String> {
        let mut sequence = file.get_all_structs();
        let seq_len = sequence.len();
        let mut lookup: HashMap<String, HashSet<String>> = HashMap::new();
        for strukt in &sequence {
            let strukt_name = strukt.get_full_name();
            let mut fields = HashSet::new();
            for field in &strukt.fields {
                let f_type = &field.type_def.typ;
                if f_type.is_struct() {
                    fields.insert(f_type.get_struct_name());
                }
            }
            if !lookup.contains_key(&strukt_name) {
                lookup.insert(strukt_name, fields);
            } else {
                todo!()
            }
        }
        if self.flags.debug {
            println!("[DEBUG] Struct Lookup: {:#?}", lookup);
        }
        /*
            Code:
                struct A { f: B; }
                struct B { b: C; a: D; }
                struct C { i: i32; }
                struct D { i: C; }
                struct E { f: B; }

            Lookup:
                A -> B
                B -> C
                B -> D
                C -> i32 [ignored]
                D -> C
                E -> B

            Sequence:
                [ A, B, C, D, E ] <- Swap A and B
                [ B, A, C, D, E ] <- Swap B and C
                [ C, B, A, D, E ] <- Swap B and D
                [ C, D, A, B, E ] <- Swap A and B
                [ C, D, B, A, E ] <- Done

            Codegen:
                struct C { i: i32; }
                struct D { i: C; }
                struct B { b: C; a: D; }
                struct A { f: B; }
                struct E { f: B; }
        */
        let mut safety = 0;
        loop {
            if self.flags.debug {
                println!("[DEBUG] {safety} {:?}", sequence.iter().map(|s|s.get_full_name()).collect::<Vec<_>>());
            }
            // REVIEW: Do we ever need n^2 attempts? Do we even need to loop? (I don't think so)
            // If we couldn't sort the sequence after a given amount of attempts, we have a cycle
            if safety > seq_len * seq_len {
                internal_panic!("Could not resolve order of structs");
            }
            let mut done = true;
            for i in 0..seq_len {
                for j in (i+1)..seq_len {
                    let s1 = sequence[i].get_full_name();
                    let s2 = sequence[j].get_full_name();
                    let entry = lookup.get(&s1).expect("Whar?");
                    if entry.contains(&s2) {
                        (sequence[i], sequence[j]) = (sequence[j], sequence[i]);
                        done = false;
                    }
                }
            }
            safety += 1;
            if done {
                break;
            }
        }
        for strukt in &sequence {
            let real_name = strukt.get_full_name();
            let mut fields = Vec::new();
            let mut field_types = Vec::new();
            for field in &strukt.fields {
                let field_type = self.codegen_type(&field.type_def.typ);
                fields.push(field_type);
                field_types.push((field.name.clone(), field.type_def.typ.clone()));
            }
            let struct_type_def = self.context.struct_type(&fields, false);
            self.struct_defs.insert(real_name.clone(), struct_type_def.into());
            let mut struct_info = StructInfo::new();
            for (name, typ) in field_types {
                struct_info.add_field(&name, &typ);
            }
            self.struct_info.insert(real_name, struct_info);
        }
        if self.flags.debug {
            println!("[DEBUG] Struct Definitions: {:#?}", self.struct_defs);
        }
        Ok(())
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, file: &nodes::ModuleNode) -> Result<(), String> {
        for module in &file.modules {
            self.module_stack.push(module.name.clone());
            self.fill_lookup(module)?;
            self.module_stack.pop();
        }

        for external in &file.externs {
            let name = &external.name;
            fill_function_lookup!(self, external, name);
        }
        for strukt in &file.structs {
            for method in &strukt.methods {
                let name = method.get_full_name();
                let llvm_method = fill_function_lookup!(self, method, name);
                if method.is_inlinable() {
                    let id = Attribute::get_named_enum_kind_id("alwaysinline");
                    assert!(id != 0);
                    llvm_method.add_attribute(AttributeLoc::Function, self.context.create_enum_attribute(id, 1));
                }
            }
        }
        for function in &file.functions {
            let name = function.get_full_name();
            let llvm_func = fill_function_lookup!(self, function, name);
            if function.is_inlinable() {
                let id = Attribute::get_named_enum_kind_id("alwaysinline");
                assert!(id != 0);
                llvm_func.add_attribute(AttributeLoc::Function, self.context.create_enum_attribute(id, 1));
            }
        }
        if let Some(compiler_flags) = &file.compiler_flags {
            for comp_flag in &compiler_flags.flags {
                let flag = comp_flag.to_vec();
                self.clang_flags.extend(flag);
            }
        }
        Ok(())
    }

    #[trace_call(extra)]
    fn enter_scope(&mut self) {
        self.stack_scopes.push(HashMap::new());
    }

    #[trace_call(extra)]
    fn exit_scope(&mut self) {
        debug_assert!(self.stack_scopes.len() > 0);
        self.stack_scopes.pop();
    }

    #[trace_call(extra)]
    fn add_variable(&mut self, name: &str, value: BasicValueEnum<'ctx>) {
        debug_assert!(self.stack_scopes.len() > 0);
        self.stack_scopes.last_mut().unwrap().insert(name.to_string(), value);
    }

    #[trace_call(extra)]
    fn known_variable(&self, name: &str) -> bool {
        for scope in &self.stack_scopes {
            if scope.get(name).is_some() {
                return true;
            }
        }
        false
    }

    #[trace_call(extra)]
    fn get_variable(&self, name: &str) -> BasicValueEnum<'ctx> {
        for scope in self.stack_scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return *value;
            }
        }
        internal_panic!("Variable {} not found!", name);
    }

    #[trace_call(always)]
    pub fn codegen_project(&mut self, file: &nodes::ModuleNode) -> Result<(), String> {
        self.fill_struct_lookup(file)?;
        debug_assert!(self.module_stack.is_empty());
        self.fill_lookup(file)?;
        debug_assert!(self.module_stack.is_empty());
        if let Err(e) = self.codegen_intrinsics() {
            internal_panic!("Module verification failed:\n{}", e.to_string());
        }
        if let Err(e) = self.codegen_entrypoint(file) {
            internal_panic!("Module verification failed:\n{}", e.to_string());
        }
        if let Err(e) = self.codegen_module(file) {
            internal_panic!("Module verification failed:\n{}", e.to_string());
        }
        self.finalize_executable()
    }

    #[trace_call(always)]
    fn find_main(&self, file: &nodes::ModuleNode) -> Option<String> {
        // First module with a main function is the entrypoint
        // Which is usually the file provided by the user
        // If not, ¯\_(ツ)_/¯
        for function in &file.functions {
            if function.name == "main" {
                return Some(function.name.clone());
            }
        }
        for module in &file.modules {
            if let Some(main) = self.find_main(module) {
                return Some(format!("{}.{}", module.name, main));
            }
        }
        None
    }

    #[trace_call(always)]
    fn codegen_entrypoint(&mut self, file: &nodes::ModuleNode) -> Result<(), BuilderError> {
        let main_name = self.find_main(file).unwrap_or_else(|| {
            println!("[ERROR] Could not find main function!");
            std::process::exit(1);
        });
        let main_func = self.module.get_function(&main_name).unwrap();
        let ret_type = if main_func.get_type().get_return_type().is_none() {
            self.context.i32_type().as_basic_type_enum()
        } else {
            main_func.get_type().get_return_type().unwrap()
        };
        let ret_fn_type = ret_type.fn_type(&[], false);
        let main = self.module.add_function("main", ret_fn_type, None);
        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);
        let result = self.builder.build_call(main_func, &[], "main_call")?;
        let val = if result.try_as_basic_value().left().is_none() {
            // NOTE: The return value doesn't matter, the function returns None
            //       it's a workaround for `void_type` not being a BasicValueEnum
            self.context.i32_type().const_int(0, false).into()
        } else {
            result.try_as_basic_value().left().unwrap()
        };
        self.builder.build_return(Some(&val))?;
        Ok(())
    }

    #[trace_call(always)]
    fn finalize_executable(&mut self) -> Result<(), String> {
        if !std::path::Path::new("./out").exists() {
            std::fs::create_dir("./out").unwrap();
        }
        match self.module.verify() {
            Ok(_) => (),
            Err(e) => {
                internal_panic!("Module verification failed:\n{}", e.to_string())
            },
        }

        if self.flags.debug {
            println!("{}", "-".repeat(80));
            println!("Module before optimizations:");
            println!("{}", self.module.to_string());
        }

        // TODO: We already have an opt-flag, so we should add optimizations accordingly
        //       instead of just adding all of them
        let pass_manager = PassManager::create(());
        {
            // Alloca -> SSA, very important and should always be run
            pass_manager.add_promote_memory_to_register_pass();
            for _ in 0..5 {
                if !pass_manager.run_on(&self.module) {
                    break;
                }
            }
        }
        // Our heuristic for alwaysinline: frontend::nodes::fn_is_inlinable!()
        pass_manager.add_always_inliner_pass();
        if self.flags.optimizations.level != OptimizationLevel::None {
            pass_manager.add_function_inlining_pass();
            pass_manager.add_instruction_combining_pass();
        }
        if self.flags.optimizations.level == OptimizationLevel::Aggressive {
            if self.flags.verbose {
                println!("[INFO] Running aggressive optimizations");
            }
            pass_manager.add_reassociate_pass();
            pass_manager.add_gvn_pass();
            pass_manager.add_cfg_simplification_pass();
            pass_manager.add_scalar_repl_aggregates_pass_ssa();
            pass_manager.add_early_cse_pass();
            pass_manager.add_loop_unroll_pass();
            pass_manager.add_loop_vectorize_pass();
            pass_manager.add_dead_arg_elimination_pass();
            pass_manager.add_global_dce_pass();
            for _ in 0..10 {
                if !pass_manager.run_on(&self.module) {
                    break;
                }
            }
        }

        if self.flags.debug {
            println!("{}", "-".repeat(80));
            println!("Module after optimizations:");
            println!("{}", self.module.to_string());
            println!("{}", "-".repeat(80));
        }

        let llvmname = self.filename.with_extension("ll");
        let path = std::path::Path::new(&llvmname);
        self.module.print_to_file(path).unwrap();
        if self.flags.emit_llvm {
            println!("[INFO] Created {}", path.to_str().unwrap());
        }

        if self.flags.emit_asm {
            let mut clang_cmd = std::process::Command::new("clang");
            let asmname = self.filename.with_extension("s");
            let path = std::path::Path::new(&asmname);
            clang_cmd.arg("-o").arg(&path).arg(&llvmname).arg("-v").arg("-S");
            let _ = clang_cmd.output().unwrap();
            println!("[INFO] Created {}", path.to_str().unwrap());
        }

        let mut clang_cmd = std::process::Command::new("clang");
        clang_cmd.arg("-o").arg(&self.exename).arg(&llvmname).arg("-v");
        for flag in &self.clang_flags {
            clang_cmd.arg(flag);
        }
        if self.flags.optimizations.level != OptimizationLevel::None {
            clang_cmd.arg(self.flags.optimizations.level.as_clang_str());
        }
        if self.flags.verbose {
            let mut s = String::from("clang");
            for arg in clang_cmd.get_args() {
                s.push(' ');
                s.push_str(arg.to_str().unwrap());
            }
            println!("[INFO] Running `{}`", s);
        }
        let clang_output = clang_cmd.output().unwrap();

        if !self.flags.emit_llvm {
            std::fs::remove_file(&llvmname).unwrap();
        }
        if self.flags.verbose {
            println!("[INFO] Clang output:");
            println!("[INFO] {}", String::from_utf8_lossy(&clang_output.stdout));
        }
        if !clang_output.status.success() {
            println!("[ERROR] Clang failed!");
            println!("[ERROR] {}", String::from_utf8_lossy(&clang_output.stderr));
            return Err("Clang failed!".to_string());
        }
        println!("[INFO] Created {}", self.exename.to_str().unwrap());
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_module(&mut self, file: &nodes::ModuleNode) -> Result<(), BuilderError> {
        for module in &file.modules {
            self.module_stack.push(module.name.clone());
            self.codegen_module(module)?;
            self.module_stack.pop();
        }
        // External functions are already added in `fill_lookup`
        // for external in &file.externs {
        //     self.codegen_extern(external)?;
        // }
        for strukt in &file.structs {
            self.codegen_struct(strukt)?;
        }
        for function in &file.functions {
            self.codegen_function(function)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_struct(&mut self, strukt: &nodes::StructNode) -> Result<(), BuilderError> {
        for method in &strukt.methods {
            self.codegen_method(method)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_method(&mut self, method: &nodes::MethodNode) -> Result<(), BuilderError> {
        self.enter_scope();

        let name = method.get_full_name();
        codegen_function_header!(self, method, name);

        self.codegen_block(&method.block)?;

        if method.return_type.typ == Type::None && !method.block.llvm_has_terminator {
            self.builder.build_return(None)?;
        } else {
            // Method is guaranteed to return a value in block codegen
        }

        self.exit_scope();
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_function(&mut self, function: &nodes::FunctionNode) -> Result<(), BuilderError> {
        self.enter_scope();

        let name = function.get_full_name();
        codegen_function_header!(self, function, name);

        self.codegen_block(&function.block)?;

        if function.return_type.typ == Type::None && !function.block.llvm_has_terminator {
            self.builder.build_return(None)?;
        } else {
            // Function is guaranteed to return a value in block codegen
        }

        self.exit_scope();
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_block(&mut self, block: &nodes::BlockNode) -> Result<(), BuilderError> {
        self.enter_scope();
        for statement in &block.statements {
            self.codegen_statement(statement)?;
        }
        self.exit_scope();
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_statement(&mut self, statement: &nodes::Statement) -> Result<(), BuilderError> {
        match statement {
            nodes::Statement::Block(block) => self.codegen_block(block),
            nodes::Statement::VarDecl(var_decl_node) => self.codegen_stmt_var_decl(var_decl_node),
            nodes::Statement::If(if_node) => self.codegen_stmt_if(if_node),
            nodes::Statement::While(while_node) => self.codegen_stmt_while(while_node),
            nodes::Statement::Return(return_node) => self.codegen_stmt_return(return_node),
            nodes::Statement::Break(break_node) => self.codegen_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => self.codegen_stmt_continue(continue_node),
            nodes::Statement::Expression(expr) => {
                let _ = self.codegen_expression(expr, false)?;
                Ok(())
            },
        }
    }

    #[trace_call(always)]
    fn codegen_stmt_continue(&mut self, _continue_node: &nodes::ContinueNode) -> Result<(), BuilderError> {
        debug_assert!(!self.loop_blocks.is_empty());
        let (loop_block, _) = self.loop_blocks.last().unwrap();
        self.builder.build_unconditional_branch(*loop_block)?;
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_stmt_break(&mut self, _break_node: &nodes::BreakNode) -> Result<(), BuilderError> {
        debug_assert!(!self.loop_blocks.is_empty());
        let (_, after_block) = self.loop_blocks.last().unwrap();
        self.builder.build_unconditional_branch(*after_block)?;
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_stmt_while(&mut self, while_node: &nodes::WhileNode) -> Result<(), BuilderError> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let while_cond = self.context.append_basic_block(function, "codegen_stmt_while_cond");
        self.builder.build_unconditional_branch(while_cond)?;
        self.builder.position_at_end(while_cond);
        let condition = self.codegen_expression(&while_node.condition, false)?;
        let while_body = self.context.append_basic_block(function, "codegen_stmt_while_body");
        let while_after = self.context.append_basic_block(function, "codegen_stmt_while_after");
        self.loop_blocks.push((while_cond, while_after));
        self.builder.build_conditional_branch(condition.into_int_value(), while_body, while_after)?;
        self.builder.position_at_end(while_body);
        self.codegen_block(&while_node.body)?;
        if !while_node.body.llvm_has_terminator {
            self.builder.build_unconditional_branch(while_cond)?;
        }
        self.builder.position_at_end(while_after);
        self.loop_blocks.pop();
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_stmt_return(&mut self, return_node: &nodes::ReturnNode) -> Result<(), BuilderError> {
        match &return_node.return_value {
            Some(value) => {
                let value = self.codegen_expression(value, false)?;
                self.builder.build_return(Some(&value))?;
                Ok(())
            }
            None => {
                self.builder.build_return(None)?;
                Ok(())
            }
        }
    }

    #[trace_call(always)]
    /// Wrapper for storing a value in a given pointer
    /// Equivalent to build_store() for the moment, but this way we can easily modify the expected behavior
    fn store_value_in_ptr<V: BasicValue<'ctx>>(&self, ptr: PointerValue<'ctx>, value: V) -> Result<InstructionValue<'ctx>, BuilderError> {
        // REVIEW: Is setting alignment necessary?
        let v = self.builder.build_store(ptr, value)?;
        // if let Err(e) = v.set_alignment(1) {
        //     internal_panic!("Could not store value in pointer: {e}");
        // }
        Ok(v)
    }

    #[trace_call(always)]
    /// Wrapper for loading a value in a given pointer
    /// Equivalent to build_load() for the moment, but this way we can easily modify the expected behavior
    fn load_value_from_ptr<T: BasicType<'ctx>>(&self, typ: T, value: PointerValue<'ctx>, name: &str) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        // REVIEW: Is setting alignment necessary?
        let value = self.builder.build_load(
            typ,
            value,
            name,
        )?;
        // value.as_instruction_value()
        //     .unwrap()
        //     .set_alignment(1)
        //     .unwrap();
        Ok(value)
    }

    #[trace_call(always)]
    /// Wrapper to ensure that `alloca` is always inserted at the end of the entry block.  
    /// This is very important because `mem2reg` only optimizes allocas in the entry block
    fn allocate(&self, typ: BasicTypeEnum<'ctx>, name: &str) -> Result<PointerValue<'ctx>, BuilderError> {
        let curr_block = self.builder.get_insert_block().unwrap();
        let parent = curr_block.get_parent().unwrap();
        let last_instr = parent.get_first_basic_block().unwrap().get_last_instruction();
        match last_instr {
            None => self.builder.position_at_end(curr_block),
            Some(instr) => self.builder.position_before(&instr),
        }
        let alloca = self.builder.build_alloca(typ, name)?;
        self.builder.position_at_end(curr_block);
        Ok(alloca)
    }

    #[trace_call(always)]
    fn codegen_stmt_var_decl(&mut self, let_node: &nodes::VarDeclNode) -> Result<(), BuilderError> {
        let typ = self.codegen_type_node(&let_node.typ);
        let alloca = self.allocate(typ, &let_node.name)?;
        self.add_variable(&let_node.name, alloca.into());
        let value = self.codegen_expression(&let_node.expression, false)?;
        self.store_value_in_ptr(alloca, value)?;
        Ok(())
    }

    #[trace_call(always)]
    fn codegen_stmt_if(&mut self, if_node: &nodes::IfNode) -> Result<(), BuilderError> {
        let condition = self.codegen_expression(&if_node.condition, false)?;
        let parent = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        if let Some(else_body) = &if_node.else_body {
            let then_bb = self.context.append_basic_block(parent, "codegen_stmt_if_then");
            let else_bb = self.context.append_basic_block(parent, "codegen_stmt_if_else");

            self.builder.build_conditional_branch(condition.into_int_value(), then_bb, else_bb)?;

            // Emit then block.
            self.builder.position_at_end(then_bb);
            self.codegen_block(&if_node.if_body)?;
            let then_bb = self.builder.get_insert_block().unwrap();

            // Emit else block.
            self.builder.position_at_end(else_bb);
            self.codegen_block(else_body)?;
            let else_bb = self.builder.get_insert_block().unwrap();
            match (if_node.if_body.llvm_has_terminator, else_body.llvm_has_terminator) {
                (true, true) => (),
                (true, false) => {
                    let cont_bb = self.context.append_basic_block(parent, "codegen_stmt_if_after");
                    self.builder.position_at_end(else_bb);
                    self.builder.build_unconditional_branch(cont_bb)?;
                    self.builder.position_at_end(cont_bb);
                },
                (false, true) => {
                    self.builder.position_at_end(then_bb);
                    let cont_bb = self.context.append_basic_block(parent, "codegen_stmt_if_after");
                    self.builder.build_unconditional_branch(cont_bb)?;
                    self.builder.position_at_end(cont_bb);
                },
                (false, false) => {
                    let cont_bb = self.context.append_basic_block(parent, "codegen_stmt_if_after");
                    self.builder.position_at_end(then_bb);
                    self.builder.build_unconditional_branch(cont_bb)?;
                    self.builder.position_at_end(else_bb);
                    self.builder.build_unconditional_branch(cont_bb)?;
                    self.builder.position_at_end(cont_bb);
                }
            }
            Ok(())
        } else {
            let then_bb = self.context.append_basic_block(parent, "codegen_stmt_if_then");
            let cont_bb = self.context.append_basic_block(parent, "codegen_stmt_if_after");

            self.builder.build_conditional_branch(condition.into_int_value(), then_bb, cont_bb)?;

            // Emit then block.
            self.builder.position_at_end(then_bb);
            self.codegen_block(&if_node.if_body)?;
            if !if_node.if_body.llvm_has_terminator {
                self.builder.build_unconditional_branch(cont_bb)?;
            }

            // Emit merge block.
            self.builder.position_at_end(cont_bb);
            Ok(())
        }
    }

    #[trace_call(always)]
    fn codegen_expression(&mut self, expression: &nodes::Expression, needs_ptr: bool) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match expression {
            nodes::Expression::Literal(literal) => self.codegen_literal(literal),
            nodes::Expression::Binary(binary) => self.codegen_binary(binary, needs_ptr),
            nodes::Expression::Name(name) => self.codegen_name(name, needs_ptr),
            nodes::Expression::FunctionCall(function_call) => self.codegen_function_call(function_call, needs_ptr),
            nodes::Expression::Unary(unary) => self.codegen_unary(unary, needs_ptr),
            nodes::Expression::StructLiteral(struct_literal) => self.codegen_struct_literal(struct_literal, needs_ptr),
            nodes::Expression::ArrayLiteral(array_literal) => self.codegen_array_literal(array_literal, needs_ptr),
            nodes::Expression::Sizeof(typ) => {
                let t = self.codegen_type_node(&typ);
                let s = self.get_struct_size(&t);
                let v = self.context.i64_type().const_int(s, false);
                Ok(v.into())
            },
        }
    }

    #[trace_call(always)]
    fn codegen_unary(&mut self, unary_node: &nodes::UnaryNode, needs_ptr: bool) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match unary_node.operation {
            Operation::Negate => {
                let value = self.codegen_expression(&unary_node.expression, false)?;
                assert_is_int!(unary_node.expression, unary_node.expression);
                let result = self.builder.build_int_neg(value.into_int_value(), "codegen_unary_negate")?;
                Ok(result.into())
            }
            Operation::Dereference => {
                let value = self.codegen_expression(&unary_node.expression, false)?;
                debug_assert!(value.is_pointer_value());
                if needs_ptr {
                    Ok(value)
                } else {
                    let value = self.load_value_from_ptr(
                        self.codegen_type(&unary_node.typ),
                        value.into_pointer_value(),
                        "codegen_unary_dereference",
                    )?;
                    Ok(value)
                }
            }
            Operation::Reference => {
                if needs_ptr {
                    self.codegen_expression(&unary_node.expression, true)
                } else {
                    let value = self.codegen_expression(&unary_node.expression, true)?;
                    if !value.is_pointer_value() {
                        // Value is not a pointer (e.g. a literal), so we need to create a pointer to it
                        let alloc = self.allocate(
                            self.codegen_type(&unary_node.expression.get_type()),
                            "codegen_unary_reference",
                        )?;
                        self.store_value_in_ptr(alloc, value)?;
                        Ok(alloc.into())
                    } else {
                        // Value is already a pointer, so we can just return it
                        Ok(value)
                    }
                }
            }
            Operation::LogicalNot => {
                let value = self.codegen_expression(&unary_node.expression, false)?;
                debug_assert!(unary_node.typ == Type::Bool);
                let result = self.builder.build_not(value.into_int_value(), "codegen_unary_logical_not")?;
                Ok(result.into())
            }
            e => unimplemented!("codegen_unary: {:?}", e),
        }
    }

    #[trace_call(always)]
    fn codegen_struct_literal(
        &mut self,
        struct_literal: &nodes::StructLiteralNode,
        needs_ptr: bool
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let mut expressions = Vec::new();
        for field in &struct_literal.fields {
            expressions.push(self.codegen_expression(&field.1, false)?);
        }
        let real_name = struct_literal.typ.get_struct_name();
        let struct_type = self.struct_defs.get(&real_name).unwrap();
        let mut struct_instance = struct_type.get_undef();
        for (i, field) in struct_literal.fields.iter().enumerate() {
            let offset = self.struct_info.get(&real_name).unwrap().get_field_index(&field.0);
            struct_instance = self.builder.build_insert_value(
                struct_instance,
                expressions[i],
                offset as u32,
                "struct_elem",
            )?.into_struct_value();
        }
        if needs_ptr {
            let struct_alloc = self.allocate(BasicTypeEnum::StructType(*struct_type), "codegen_struct_literal")?;
            self.store_value_in_ptr(struct_alloc, struct_instance)?;
            Ok(struct_alloc.into())
        } else {
            Ok(struct_instance.as_basic_value_enum())
        }
    }

    #[trace_call(always)]
    fn get_struct_size(&self, typ: &BasicTypeEnum<'ctx>) -> u64 {
        let size_0 = self.target_machine.get_target_data().get_store_size(typ);
        let size_1 = self.target_machine.get_target_data().get_abi_size(typ);
        let size_2 = self.target_machine.get_target_data().get_bit_size(typ) / 8;
        if size_0 != size_1 || size_1 != size_2 || size_0 != size_2 {
            println!("type: {:?}", typ);
            println!("size_0: {:?}", size_0);
            println!("size_1: {:?}", size_1);
            println!("size_2: {:?}", size_2);
            internal_panic!("Struct size mismatch");
        }
        size_0
    }

    #[trace_call(always)]
    fn struct_type_for_arg(&self, typ: &BasicTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        let struct_size = self.get_struct_size(typ);
        if struct_size == 1 {
            self.context.i8_type().into()
        } else if struct_size <= 2 {
            self.context.i16_type().into()
        } else if struct_size <= 4 {
            self.context.i32_type().into()
        } else if struct_size <= 8 {
            self.context.i64_type().into()
        } else {
            self.context.i8_type().ptr_type(AddressSpace::default()).into()
        }
    }

    #[trace_call(always)]
    fn struct_value_for_arg(&mut self, struct_instance: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let struct_type = struct_instance.get_type();
        debug_assert!(struct_type.is_struct_type());
        debug_assert!(struct_instance.is_struct_value());
        let struct_size = self.get_struct_size(&struct_type);
        let struct_alloc = self.allocate(struct_type, "struct_value_for_arg")?;
        self.store_value_in_ptr(struct_alloc, struct_instance)?;
        let int = if struct_size == 1 {
            self.context.i8_type()
        } else if struct_size <= 2 {
            self.context.i16_type()
        } else if struct_size <= 4 {
            self.context.i32_type()
        } else if struct_size <= 8 {
            self.context.i64_type()
        } else {
            return Ok(struct_alloc.into());
        };
        let int = self.load_value_from_ptr(int, struct_alloc, "struct_value_for_arg")?;
        Ok(int)
    }

    #[trace_call(always)]
    fn codegen_array_literal(
        &mut self,
        array_literal: &nodes::ArrayLiteralNode,
        needs_ptr: bool
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let underlying_type = self.codegen_type(&array_literal.typ);
        if let Some(size) = array_literal.size {
            debug_assert!(array_literal.elements.len() == 1);
            let value = self.codegen_expression(&array_literal.elements[0], false)?;
            let array_alloca = self.allocate(underlying_type, "codegen_array_literal")?;
            let current_block = self.builder.get_insert_block().unwrap();
            let block_parent = current_block.get_parent().unwrap();
            let loop_start = self.context.append_basic_block(block_parent, "codegen_array_literal_loop_start");
            let loop_body = self.context.append_basic_block(block_parent, "codegen_array_literal_loop_body");
            let loop_end = self.context.append_basic_block(block_parent, "codegen_array_literal_loop_end");
            self.builder.build_unconditional_branch(loop_start)?;
            self.builder.position_at_end(loop_start);
            let i = self.builder.build_phi(self.context.i64_type(), "codegen_array_literal_i")?;
            i.add_incoming(&[
                (&self.context.i64_type().const_int(0, false), current_block),
            ]);
            let cond = self.builder.build_int_compare(
                inkwell::IntPredicate::ULT,
                i.as_basic_value().into_int_value(),
                self.context.i64_type().const_int(size as u64, false),
                "codegen_array_literal_cond"
            )?;
            self.builder.build_conditional_branch(cond, loop_body, loop_end)?;
            self.builder.position_at_end(loop_body);
            unsafe {
                let array_field_ptr = self.builder.build_gep(
                    underlying_type,
                    array_alloca,
                    &[self.context.i64_type().const_int(0, false), i.as_basic_value().into_int_value()],
                    "codegen_array_literal_array_field_ptr",
                )?;
                self.store_value_in_ptr(array_field_ptr, value)?;
            }
            let next_i = self.builder.build_int_add(i.as_basic_value().into_int_value(), self.context.i64_type().const_int(1, false), "codegen_array_literal_next_i")?;
            i.add_incoming(&[
                (&next_i, loop_body),
            ]);
            self.builder.build_unconditional_branch(loop_start)?;
            self.builder.position_at_end(loop_end);
            if needs_ptr {
                Ok(array_alloca.into())
            } else {
                let loaded_instance = self.load_value_from_ptr(underlying_type, array_alloca, "codegen_array_literal_loaded_instance")?;
                Ok(loaded_instance.into())
            }
        } else {
            let mut array_instance = underlying_type.into_array_type().get_undef();
            for (i, element) in array_literal.elements.iter().enumerate() {
                let value = self.codegen_expression(element, false)?;
                array_instance = self.builder.build_insert_value(
                    array_instance,
                    value,
                    i as u32,
                    "arr_elem",
                )?.into_array_value();
            }
            if needs_ptr {
                let array_alloc = self.allocate(underlying_type, "codegen_array_literal")?;
                self.store_value_in_ptr(array_alloc, array_instance)?;
                Ok(array_alloc.into())
            } else {
                Ok(array_instance.as_basic_value_enum())
            }
        }
    }

    #[trace_call(always)]
    fn codegen_function_call(&mut self, function_call: &nodes::CallNode, needs_ptr: bool) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let real_name = function_call.get_full_name();
        let Some(function) = self.module.get_function(&real_name) else {
            internal_panic!("Could not find function {}", real_name);
        };
        let mut args = Vec::new();
        for arg in &function_call.arguments {
            let expr = self.codegen_expression(arg, false)?;
            if expr.is_struct_value() {
                let expr = self.struct_value_for_arg(expr)?;
                args.push(expr.into());
            } else {
                args.push(expr.into());
            }
        }
        let result = self.builder.build_call(function, &args, "codegen_function_call")?;
        let val = if result.try_as_basic_value().left().is_none() {
            // NOTE: The return value doesn't matter, the function returns None
            //       it's a workaround for `void_type` not being a BasicValueEnum
            self.context.i32_type().const_int(0, false).into()
        } else {
            result.try_as_basic_value().left().unwrap()
        };
        if needs_ptr {
            let temp_alloc = self.allocate(val.get_type(), "codegen_function_call")?;
            self.store_value_in_ptr(temp_alloc, val)?;
            Ok(temp_alloc.into())
        } else {
            Ok(val)
        }
    }

    #[trace_call(always)]
    fn codegen_name(&mut self, name: &nodes::NameNode, needs_ptr: bool) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        debug_assert!(self.known_variable(&name.name));
        let var = self.get_variable(&name.name);
        if needs_ptr {
            debug_assert!(var.is_pointer_value());
            Ok(var)
        } else {
            let var = self.load_value_from_ptr(
                self.codegen_type(&name.typ),
                var.into_pointer_value(),
                "codegen_name",
            )?;
            Ok(var)
        }
    }

    #[trace_call(always)]
    fn codegen_binary(&mut self, binary: &nodes::BinaryNode, needs_ptr: bool) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match &binary.operation {
            Operation::Add => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                if binary.typ == Type::F32 || binary.typ == Type::F64 {
                    let result = self.builder.build_float_add(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "codegen_binary_add",
                    )?;
                    Ok(result.into())
                } else {
                    if matches!(binary.typ, Type::Ref(..)) {
                        if matches!(binary.lhs.get_type(), Type::Ref(..)) {
                            let Type::Ref(underlying, _) = binary.lhs.get_type() else {
                                unreachable!()
                            };
                            assert!(binary.rhs.get_type() == Type::Usize); // Type Checker guarantees that
                            let gep = unsafe {
                                self.builder.build_gep(
                                    self.codegen_type(&underlying),
                                    lhs.into_pointer_value(),
                                    &[rhs.into_int_value()],
                                    "codegen_ptr_add"
                                )?
                            };
                            Ok(gep.into())
                        } else {
                            let Type::Ref(underlying, _) = binary.rhs.get_type() else {
                                unreachable!()
                            };
                            assert!(binary.lhs.get_type() == Type::Usize); // Type Checker guarantees that
                            let gep = unsafe {
                                self.builder.build_gep(
                                    self.codegen_type(&underlying),
                                    rhs.into_pointer_value(),
                                    &[lhs.into_int_value()],
                                    "codegen_ptr_add"
                                )?
                            };
                            Ok(gep.into())
                        }
                    } else {
                        assert_is_int!(binary.lhs, binary.rhs);
                        let result = self.builder.build_int_add(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "codegen_binary_add",
                        )?;
                        Ok(result.into())
                    }
                }
            }
            Operation::Sub => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                if binary.typ == Type::F32 || binary.typ == Type::F64 {
                    let result = self.builder.build_float_sub(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "codegen_binary_sub",
                    )?;
                    Ok(result.into())
                } else {
                    if matches!(binary.typ, Type::Ref(..)) {
                        let gep = if matches!(binary.lhs.get_type(), Type::Ref(..)) {
                            let Type::Ref(underlying, _) = binary.lhs.get_type() else {
                                unreachable!()
                            };
                            assert!(binary.rhs.get_type() == Type::Usize); // Type Checker guarantees that
                            let rhs = self.builder.build_int_neg(rhs.into_int_value(), "codegen_ptr_sub_neg")?;
                            unsafe {
                                self.builder.build_gep(
                                    self.codegen_type(&underlying),
                                    lhs.into_pointer_value(),
                                    &[rhs],
                                    "codegen_ptr_sub"
                                )?
                            }
                        } else {
                            internal_panic!("Expected Ref-Usize, found Usize-Ref");
                        };
                        Ok(gep.into())
                    } else {
                        assert_is_int!(binary.lhs, binary.rhs);
                        let result = self.builder.build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "codegen_binary_sub",
                        )?;
                        Ok(result.into())
                    }
                }
            }
            Operation::Mul => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                assert_is_int!(binary.lhs, binary.rhs);
                let result = self.builder.build_int_mul(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_mul",
                )?;
                Ok(result.into())
            }
            Operation::Div => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                if binary.typ == Type::F32 || binary.typ == Type::F64 {
                    let result = self.builder.build_float_div(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "codegen_binary_div",
                    )?;
                    Ok(result.into())
                } else {
                    assert_is_int!(binary.lhs, binary.rhs);
                    let result = self.builder.build_int_signed_div(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "codegen_binary_div",
                    )?;
                    Ok(result.into())
                }
            }
            Operation::Modulo => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                if binary.typ == Type::F32 || binary.typ == Type::F64 {
                    let result = self.builder.build_float_rem(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "codegen_binary_modulo",
                    )?;
                    Ok(result.into())
                } else {
                    assert_is_int!(binary.lhs, binary.rhs);
                    let result = self.builder.build_int_signed_rem(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "codegen_binary_modulo",
                    )?;
                    Ok(result.into())
                }
            }
            Operation::LessThan => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                assert_is_int!(binary.lhs, binary.rhs);
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_lessthan",
                )?;
                Ok(result.into())
            }
            Operation::GreaterThan => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                assert_is_int!(binary.lhs, binary.rhs);
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_greaterthan",
                )?;
                Ok(result.into())
            }
            Operation::LessThanOrEqual => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                assert_is_int!(binary.lhs, binary.rhs);
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_lessthanequal",
                )?;
                Ok(result.into())
            }
            Operation::GreaterThanOrEqual => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                assert_is_int!(binary.lhs, binary.rhs);
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_greaterthanequal",
                )?;
                Ok(result.into())
            }
            Operation::Equal => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_equal",
                )?;
                Ok(result.into())
            }
            Operation::NotEqual => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_notequal",
                )?;
                Ok(result.into())
            }
            Operation::BitwiseAnd => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_and(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_and",
                )?;
                Ok(result.into())
            }
            Operation::BitwiseOr => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_or(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_or",
                )?;
                Ok(result.into())
            }
            Operation::BitwiseXor => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_xor(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_xor",
                )?;
                Ok(result.into())
            }
            Operation::LogicalAnd => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_and(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_and",
                )?;
                Ok(result.into())
            }
            Operation::LogicalOr => {
                let lhs = self.codegen_expression(&binary.lhs, false)?;
                let rhs = self.codegen_expression(&binary.rhs, false)?;
                let result = self.builder.build_or(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "codegen_binary_or",
                )?;
                Ok(result.into())
            }
            Operation::Assign => {
                let var = self.codegen_expression(&binary.lhs, true)?;
                let value = self.codegen_expression(&binary.rhs, false)?;
                self.store_value_in_ptr(var.into_pointer_value(), value)?;
                Ok(value)
            }
            Operation::MemberAccess => {
                match ((*binary.lhs).get_type(), &(*binary.rhs)) {
                    (ref typ @ Type::Ref(_, _), nodes::Expression::Name(field))
                    | (ref typ @ Type::Struct(_, _), nodes::Expression::Name(field)) => {
                        let real_name = match typ {
                            Type::Ref(t, _) => {
                                debug_assert!(t.is_struct());
                                t.get_struct_name()
                            }
                            t @ Type::Struct(_, _) => t.get_struct_name(),
                            _ => internal_panic!("Expected struct, found {:?}", typ),
                        };
                        let lhs = self.codegen_expression(&binary.lhs, !typ.is_struct_ref())?;
                        let Some(struct_type) = self.get_struct_type(&real_name) else {
                            internal_panic!("Could not find struct {}", real_name)
                        };
                        let Some(struct_lookup) = self.struct_info.get(&real_name) else {
                            internal_panic!("Could not find struct {}", real_name)
                        };
                        let offset = struct_lookup.get_field_index(&field.name);
                        let field_ptr = self.builder.build_struct_gep(
                            struct_type.clone(),
                            lhs.into_pointer_value(),
                            offset as u32,
                            "field_ptr")?;
                        let field_type = self.codegen_type(&field.typ);
                        if needs_ptr {
                                Ok(field_ptr.into())
                        } else {
                            let field_value = self.load_value_from_ptr(
                                field_type,
                                field_ptr,
                                "field_ptr_load",
                            )?;
                            Ok(field_value)
                        }
                    },
                    (ref typ @ Type::Ref(_, _), nodes::Expression::FunctionCall(method_call))
                    | (ref typ @ Type::Struct(_, _), nodes::Expression::FunctionCall(method_call)) => {
                        let real_name = match typ {
                            Type::Ref(t, _) => {
                                let Type::Struct(ref name, _) = **t else {
                                    internal_panic!("Expected Reference to Struct, found Reference to {t}")
                                };
                                name
                            }
                            Type::Struct(name, _) => name,
                            _ => internal_panic!("Expected struct, found {:?}", typ),
                        };
                        let method_name = method_call.get_method_name(real_name);
                        let Some(method) = self.module.get_function(&method_name) else {
                            internal_panic!("Could not find function {}", method_name)
                        };
                        let needs_ref = matches!(typ, Type::Struct(_, _)) && method.get_first_param().unwrap().get_type().is_pointer_type();
                        let lhs = self.codegen_expression(&binary.lhs, needs_ref)?;
                        let mut args = Vec::new();
                        args.push(lhs);
                        for arg in &method_call.arguments {
                            let expr = self.codegen_expression(arg, false)?;
                            args.push(expr);
                        }
                        let mut new_args = Vec::new();
                        for arg in args {
                            // If the argument is a struct, we need to follow the ABI
                            if arg.is_struct_value() {
                                let new_arg = self.struct_value_for_arg(arg)?;
                                new_args.push(new_arg.into());
                            } else {
                                new_args.push(arg.into());
                            }
                        }
                        let result = self.builder.build_call(method, &new_args, "method_call")?;
                        if result.try_as_basic_value().left().is_none() {
                            // NOTE: The return value doesn't matter, the function returns None
                            Ok(lhs.into())
                        } else {
                            Ok(result.try_as_basic_value().left().unwrap())
                        }
                    },
                    (lhs, rhs) => internal_panic!(
                        "Something went wrong: Found {:?} as lhs and {:?} as rhs of Dot operation!", lhs, rhs
                    )
                }
            }
            Operation::IndexedAccess => {
                let array = self.codegen_expression(&binary.lhs, true)?;
                let index = self.codegen_expression(&binary.rhs, false)?;
                let array_type = self.codegen_type(&binary.lhs.get_type());
                let array_size = match &binary.lhs.get_type() {
                    Type::Array(_, size) => *size,
                    _ => internal_panic!("Expected array, found {:?}", binary.lhs.get_type()),
                };
                debug_assert!(array_type.is_array_type());
                debug_assert!(index.is_int_value());
                debug_assert!(array.is_pointer_value());
                {
                    // if (index >= size) { panic("Index out of bounds"); }
                    let cond = self.builder.build_int_compare(
                        inkwell::IntPredicate::UGE,
                        index.into_int_value(),
                        self.context.i64_type().const_int(array_size as u64, false),
                        "codegen_binary_indexedaccess_cond",
                    )?;
                    let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                    let panic_block = self.context.append_basic_block(current_fn, "codegen_binary_indexedaccess_panic");
                    let normal_block = self.context.append_basic_block(current_fn, "codegen_binary_indexedaccess_normal");
                    self.builder.build_conditional_branch(cond, panic_block, normal_block)?;
                    self.builder.position_at_end(panic_block);
                    let exit_fn = self.module.get_function(&"prelude.index_oob").unwrap();
                    let exit_msg = self.builder.build_global_string_ptr(&format!(
                        "{:?}: RUNTIME ERROR: Index out of bounds: Array of size {} has no index %d\n",
                        binary.location,
                        array_size,
                    ), "oob_lit")?;
                    self.builder.build_call(exit_fn, &[
                        exit_msg.as_pointer_value().into(),
                        index.into(),
                    ], "run_time_error")?;
                    self.builder.build_unreachable()?;
                    self.builder.position_at_end(normal_block);
                }
                let field_ptr = unsafe {
                    self.builder.build_gep(
                        array_type,
                        array.into_pointer_value(),
                        &[self.context.i64_type().const_int(0, false), index.into_int_value()],
                        "field_ptr")
                }?;
                let field_type = self.codegen_type(&binary.typ);
                if needs_ptr {
                    Ok(field_ptr.into())
                } else {
                    let field_value = self.load_value_from_ptr(
                        field_type,
                        field_ptr,
                        "field_ptr_load",
                    )?;
                    Ok(field_value)
                }
            }
            e => unimplemented!("codegen_binary: {:?}", e),
        }
    }

    #[trace_call(always)]
    fn codegen_literal(&mut self, literal: &nodes::LiteralNode) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match &literal.typ {
            Type::Bool => {
                let value = if literal.value == "true" { 1 } else { 0 };
                let value = self.context.bool_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::Char => {
                let value = literal.value.chars().next().unwrap() as u8;
                let value = self.context.i8_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::I8 => {
                let value = literal.value.parse::<i8>().unwrap();
                let value = self.context.i8_type().const_int(value as u64, true);
                Ok(value.into())
            }
            Type::I16 => {
                let value = literal.value.parse::<i16>().unwrap();
                let value = self.context.i16_type().const_int(value as u64, true);
                Ok(value.into())
            }
            Type::I32 => {
                let value = literal.value.parse::<i32>().unwrap();
                let value = self.context.i32_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::I64 => {
                let value = literal.value.parse::<i64>().unwrap();
                let value = self.context.i64_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::U8 => {
                let value = literal.value.parse::<u8>().unwrap();
                let value = self.context.i8_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::U16 => {
                let value = literal.value.parse::<u16>().unwrap();
                let value = self.context.i16_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::U32 => {
                let value = literal.value.parse::<u32>().unwrap();
                let value = self.context.i32_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::U64 => {
                let value = literal.value.parse::<u64>().unwrap();
                let value = self.context.i64_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::Usize => {
                let value = literal.value.parse::<usize>().unwrap();
                let value = self.context.i64_type().const_int(value as u64, false);
                Ok(value.into())
            }
            Type::F32 => {
                let value = literal.value.parse::<f32>().unwrap();
                let value = self.context.f32_type().const_float(value as f64);
                Ok(value.into())
            }
            Type::F64 => {
                let value = literal.value.parse::<f64>().unwrap();
                let value = self.context.f64_type().const_float(value);
                Ok(value.into())
            }
            Type::Ref(t, false) => {
                if **t != Type::Str {
                    unimplemented!("codegen_literal: {:?}", literal);
                }
                let value = self.builder.build_global_string_ptr(&literal.value, "codegen_str_literal")?;
                Ok(value.as_pointer_value().into())
            }
            e => unimplemented!("codegen_literal: {:?}", e),
        }
    }

    #[trace_call(always)]
    fn codegen_type_node(&self, type_node: &nodes::TypeNode) -> BasicTypeEnum<'ctx> {
        self.codegen_type(&type_node.typ)
    }

    #[trace_call(always)]
    fn codegen_type(&self, typ: &Type) -> BasicTypeEnum<'ctx> {
        match typ {
            Type::Char => self.context.i8_type().as_basic_type_enum(),
            Type::I8 => self.context.i8_type().as_basic_type_enum(),
            Type::I16 => self.context.i16_type().as_basic_type_enum(),
            Type::I32 => self.context.i32_type().as_basic_type_enum(),
            Type::I64 => self.context.i64_type().as_basic_type_enum(),
            Type::U8 => self.context.i8_type().as_basic_type_enum(),
            Type::U16 => self.context.i16_type().as_basic_type_enum(),
            Type::U32 => self.context.i32_type().as_basic_type_enum(),
            Type::U64 => self.context.i64_type().as_basic_type_enum(),
            Type::F32 => self.context.f32_type().as_basic_type_enum(),
            Type::F64 => self.context.f64_type().as_basic_type_enum(),
            Type::Usize => self.context.i64_type().as_basic_type_enum(),
            Type::Bool => self.context.bool_type().as_basic_type_enum(),
            Type::Array(ty, size) => {
                let ty = self.codegen_type(&*ty);
                ty.array_type(*size as u32).as_basic_type_enum()
            },
            t @ Type::Struct(..) => {
                let real_name = t.get_struct_name();
                let Some(struct_type) = self.get_struct_type(&real_name) else {
                    if self.flags.debug {
                        println!("[DEBUG] Could not find struct {}", real_name);
                        println!("[DEBUG] Known structs: {:?}", self.struct_defs.keys());
                    }
                    internal_panic!("Could not find struct {}", real_name)
                };
                struct_type.into()
            },
            // Note: As pointers in LLVM are no longer typed, we can just do that
            Type::Ref(..) | Type::Any => self.context.i64_type().ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Type::Str => self.context.i8_type().as_basic_type_enum(),
            // Note: Void does not exist as BasicTypeEnum, so void functions are handled differently
            Type::None => internal_panic!("Type::None should never be used!"),
            Type::Unknown => internal_panic!("Type::Unknown should never be used!"),
        }
    }

    #[trace_call(always)]
    fn get_struct_type(&self, struct_name: &str) -> Option<StructType<'ctx>> {
        if let Some(struct_type) = self.struct_defs.get(struct_name) {
            Some(struct_type.clone())
        } else {
            None
        }
    }

    #[trace_call(always)]
    pub fn run(&mut self) -> Result<(), String> {
        println!("[INFO] Running {}", self.exename.to_str().unwrap());
        let path = std::path::Path::new(&self.exename);
        let output = std::process::Command::new(path)
            .output()
            .expect("Failed to execute program!");
        let exit_code = output.status.code().unwrap();
        println!("[INFO] BEGIN OUTPUT\n{}\n[INFO] END OUTPUT", String::from_utf8(output.stdout).unwrap());
        println!("[INFO] BEGIN ERROR\n{}\n[INFO] END ERROR", String::from_utf8(output.stderr).unwrap());
        println!("[INFO] Program exited with code 0x{:X}.", exit_code);
        if exit_code != 0 {
            Err(format!(
                "{}: Code execution failed with code 0x{:X}.",
                ERR_STR, exit_code
            ))
        } else {
            Ok(())
        }
    }
}