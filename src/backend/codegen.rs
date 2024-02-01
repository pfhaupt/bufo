use std::collections::HashMap;
use std::num::ParseIntError;

use super::instr;
use crate::compiler::{ERR_STR, NOTE_STR};
use crate::frontend::nodes;
use crate::frontend::parser::{Operation, CONSTRUCTOR_KEYWORD};
use crate::middleend::type_checker::Type;
use crate::util::flags::Flags;

use crate::internal_panic;

use tracer::trace_call;

const LBL_STR: &str = "lbl_";

macro_rules! func_assert {
    ($codegen:ident) => {
        debug_assert!($codegen.current_stack_offset == 0);
        debug_assert!($codegen.stack_scopes.is_empty());
        debug_assert!($codegen.current_return_label.is_empty());
        debug_assert!($codegen.current_class.is_empty());
    };
}
macro_rules! func_entry {
    ($codegen:ident,$name:expr) => {
        $codegen.enter_scope();

        // Generate entrypoint for later `call func_name` call
        let label = $codegen.generate_label(Some($name.clone()));
        $codegen.add_ir(label);

        $codegen.current_return_label = $name.clone() + "_return";
    };
}
macro_rules! func_stack {
    ($codegen:ident, $self:ident, $alloc:expr) => {
        // Prepare stack offset
        let mut bytes = $self.stack_size;
        if bytes % 16 != 0 {
            // 16-byte align the stack
            bytes += 16 - bytes % 16;
        }
        if bytes < 32 {
            // Windows ABI requires at least 32 bytes of stack space
            bytes = 32;
        }
        if $alloc {
            $codegen.add_ir(instr::IR::AllocStack { bytes });
        } else {
            $codegen.add_ir(instr::IR::DeallocStack { bytes });
        }
    };
}
macro_rules! func_param {
    ($codegen:ident, $self:ident) => {
        // Prepare parameters
        for param in &$self.parameters {
            param.codegen($codegen)?;
        }
        $codegen.reset_registers();
    };
}
macro_rules! func_body {
    ($codegen:ident, $self:ident) => {
        // Function body
        $self.block.codegen($codegen)?;
    };
}
macro_rules! func_return {
    ($codegen:ident, $self:ident) => {
        // Return procedure
        let label = $codegen.generate_label(Some($codegen.current_return_label.clone()));
        $codegen.add_ir(label);

        // Clean up stack offset
        func_stack!($codegen, $self, false);

        $codegen.add_ir(instr::IR::Return);
    };
}
macro_rules! func_leave {
    ($codegen:ident) => {
        $codegen.current_stack_offset = 0;
        $codegen.current_return_label.clear();
        $codegen.reset_registers();
        $codegen.leave_scope();
    };
}

macro_rules! method {
    ($codegen:ident, $self:ident, $name:expr) => {
        func_assert!($codegen);
        func_entry!($codegen, $name);
        func_stack!($codegen, $self, true);
        func_param!($codegen, $self);
        func_body!($codegen, $self);
        func_return!($codegen, $self);
        func_leave!($codegen);
    };
}

macro_rules! function {
    ($codegen:ident, $self:ident, $name:expr) => {
        func_assert!($codegen);
        func_entry!($codegen, $name);
        func_stack!($codegen, $self, true);
        func_param!($codegen, $self);
        func_body!($codegen, $self);
        func_return!($codegen, $self);
        func_leave!($codegen);
    };
}

#[derive(Debug)]
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

    fn add_field(&mut self, field_name: &String, size: usize) {
        debug_assert!(!self.field_offsets.contains_key(field_name));
        self.field_offsets
            .insert(field_name.clone(), self.total_size);
        self.total_size += size;
    }

    fn get_field_offset(&self, name: &String) -> usize {
        *self.field_offsets.get(name).unwrap()
    }
}

#[derive(Debug)]
struct SizeManager {
    class_sizes: HashMap<String, ClassInfo>,
}

impl SizeManager {
    fn new() -> Self {
        Self {
            class_sizes: HashMap::new(),
        }
    }

    fn add_class(&mut self, class_name: &String) {
        debug_assert!(!self.class_sizes.contains_key(class_name));
        self.class_sizes
            .insert(class_name.clone(), ClassInfo::new());
    }

    fn add_field(
        &mut self,
        class_name: &String,
        field_name: &String,
        typ: &Type,
    ) -> Result<(), String> {
        let size = typ.size();
        let Some(class) = self.class_sizes.get_mut(class_name) else {
            // Two Options: Forgot to add class to Manager, or Type Checker f*ed up.
            unreachable!();
        };
        class.add_field(field_name, size);
        Ok(())
    }

    fn get_class_info(&self, name: &String) -> &ClassInfo {
        self.class_sizes.get(name).unwrap()
    }

    fn get_class_size(&self, class_name: &String) -> usize {
        debug_assert!(self.class_sizes.contains_key(class_name));
        self.class_sizes.get(class_name).unwrap().total_size
    }
}

#[derive(Debug)]
pub struct Codegen<'flags> {
    sm: SizeManager,
    current_class: String,
    current_return_label: String,
    ir: Vec<instr::IR>,
    label_counter: usize,
    current_stack_offset: usize,
    stack_scopes: Vec<HashMap<String, usize>>,
    // FIXME: Figure out a better way to do this
    loop_stack: Vec<(String, String)>,
    register_counter: usize,
    flags: &'flags Flags
}
impl<'flags> Codegen<'flags> {
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            sm: SizeManager::new(),
            current_class: String::new(),
            current_return_label: String::new(),
            ir: Vec::new(),
            label_counter: 0,
            current_stack_offset: 0,
            stack_scopes: Vec::new(),
            loop_stack: Vec::new(),
            register_counter: 0,
            flags
        }
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, ast: &nodes::FileNode) {
        for class in &ast.classes {
            self.add_class(&class.name);
            self.current_class = class.name.clone();
            for field in &class.fields {
                field.codegen(self).unwrap();
            }
            if self.flags.debug {
                println!(
                    "[DEBUG] Struct `{}` has size {} bytes",
                    class.name,
                    self.sm.get_class_size(&class.name)
                );
            }
        }
        self.current_class.clear();
    }

    #[trace_call(always)]
    pub fn generate_code(&mut self, ast: &nodes::FileNode) -> Result<Vec<instr::IR>, String> {
        self.fill_lookup(ast);
        ast.codegen(self)?;
        Ok(self.ir.clone())
    }

    #[trace_call(extra)]
    fn generate_label(&mut self, name: Option<String>) -> instr::IR {
        if let Some(name) = name {
            instr::IR::Label { name }
        } else {
            let label = LBL_STR.to_string() + &self.label_counter.to_string();
            self.label_counter += 1;
            instr::IR::Label { name: label }
        }
    }

    #[trace_call(extra)]
    fn add_class(&mut self, class_name: &String) {
        if self.flags.debug {
            println!("[DEBUG] Added new Struct `{}`", class_name);
        }
        self.sm.add_class(class_name);
    }

    #[trace_call(extra)]
    fn add_field(&mut self, name: &String, typ: &Type) -> Result<(), String> {
        debug_assert!(!self.current_class.is_empty());
        self.sm.add_field(&self.current_class, name, typ)?;
        if self.flags.debug {
            println!(
                "[DEBUG] Added new field `{}` to class `{}`",
                name, self.current_class
            );
        }
        Ok(())
    }

    #[trace_call(extra)]
    fn enter_scope(&mut self) {
        self.stack_scopes.push(HashMap::new())
    }

    #[trace_call(extra)]
    fn leave_scope(&mut self) {
        debug_assert!(!self.stack_scopes.is_empty());
        self.stack_scopes.pop();
    }

    #[trace_call(extra)]
    fn get_register(&mut self) -> Result<instr::Register, String> {
        self.register_counter += 1;
        let v = self.register_counter;
        if self.register_counter == instr::Register::__COUNT as usize {
            internal_panic!("We have run out of registers to assign!!")
        } else {
            let reg = instr::Register::from(v);
            if reg == instr::Register::Rsp || reg == instr::Register::Rbp {
                // We do not want to mess with RSP and RBP
                self.get_register()
            } else {
                Ok(instr::Register::from(v))
            }
        }
    }

    #[trace_call(extra)]
    fn reset_registers(&mut self) {
        // even if register 0 (RAX) is reserved, get_register() increases this counter before returning it, so it's ok.
        self.register_counter = 0;
    }

    #[allow(unused)]
    #[trace_call(extra)]
    fn push_preserved_registers(&mut self) {
        for reg in instr::Register::PRESERVED {
            self.add_ir(instr::IR::PushReg { reg });
        }
    }

    #[allow(unused)]
    #[trace_call(extra)]
    fn pop_preserved_registers(&mut self) {
        for reg in instr::Register::PRESERVED.iter().rev() {
            self.add_ir(instr::IR::PopReg { reg: *reg });
        }
    }

    #[trace_call(extra)]
    fn push_registers(&mut self, reg_ctr: usize) {
        for index in 1..reg_ctr {
            self.add_ir(instr::IR::PushReg {
                reg: instr::Register::from(index),
            });
        }
    }

    #[trace_call(extra)]
    fn pop_registers(&mut self, reg_ctr: usize) {
        for index in (1..reg_ctr).rev() {
            self.add_ir(instr::IR::PopReg {
                reg: instr::Register::from(index),
            });
        }
    }

    #[trace_call(always)]
    fn add_ir(&mut self, ir: instr::IR) {
        if self.flags.debug {
            println!("[DEBUG] Added IR: {:?}", ir);
        }
        self.ir.push(ir)
    }

    #[trace_call(extra)]
    fn add_variable_offset(&mut self, name: &str, offset: usize) {
        debug_assert!(!self.stack_scopes.is_empty());
        self.stack_scopes
            .last_mut()
            .unwrap()
            .insert(name.to_owned(), offset);
    }

    #[trace_call(extra)]
    fn update_stack_offset(&mut self, typ_size: usize) -> usize {
        let v = self.current_stack_offset;
        self.current_stack_offset += typ_size;
        v
    }

    #[trace_call(extra)]
    fn known_variable(&mut self, name: &str) -> bool {
        for scope in self.stack_scopes.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }
        false
    }

    #[trace_call(extra)]
    fn get_stack_offset(&mut self, name: &str) -> usize {
        for scope in self.stack_scopes.iter().rev() {
            if let Some(offset) = scope.get(name) {
                return *offset;
            }
        }
        println!(
            "Could not find {name} in {scopes:?}",
            scopes = self.stack_scopes
        );
        panic!()
    }

    #[trace_call(extra)]
    fn store_variable_in_stack(&mut self, name: &str, typ: &Type) -> Result<(), String> {
        let offset = self.update_stack_offset(typ.size());
        self.add_variable_offset(name, offset);
        let reg = self.get_register()?;
        self.add_ir(instr::IR::Store {
            addr: instr::Operand::offset(offset, typ.size()),
            value: instr::Operand::reg(reg, instr::RegMode::from(typ)),
        });
        Ok(())
    }
}

trait Codegenable {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String>;
}

impl Codegenable for nodes::FileNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        for externs in &self.externs {
            externs.codegen(codegen)?;
        }
        for class in &self.classes {
            class.codegen(codegen)?;
        }
        for function in &self.functions {
            function.codegen(codegen)?;
        }
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ExternNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        codegen.add_ir(instr::IR::External {
            name: self.name.clone(),
        });
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ClassNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        for constructor in &self.constructors {
            constructor.codegen(codegen)?;
        }

        for method in &self.methods {
            method.codegen(codegen)?;
        }

        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::FieldNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let typ = &self.type_def.typ;
        debug_assert!(*typ != Type::None);
        codegen.add_field(&self.name, typ)?;
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ConstructorNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        func_assert!(codegen);
        func_entry!(codegen, self.class_name.clone() + "_" + &CONSTRUCTOR_KEYWORD);
        func_stack!(codegen, self, true);
        func_param!(codegen, self);

        // let this: Struct = calloc(1, sizeof(Struct));
        let calloc_number = instr::Operand::reg(instr::Register::ARG1, instr::RegMode::BIT64);
        codegen.add_ir(instr::IR::LoadImm {
            dst: calloc_number,
            imm: instr::Operand::imm_u64(1),
        });
        let calloc_size = instr::Operand::reg(instr::Register::ARG2, instr::RegMode::BIT64);
        codegen.add_ir(instr::IR::LoadImm {
            dst: calloc_size,
            imm: instr::Operand::imm_u64(codegen.sm.get_class_size(&self.class_name) as u64),
        });
        codegen.add_ir(instr::IR::Call {
            name: String::from("calloc"),
        });

        let offset = codegen.update_stack_offset(8);
        codegen.add_variable_offset(&String::from("this"), offset);
        codegen.add_ir(instr::IR::Store {
            addr: instr::Operand::offset(offset, 8),
            value: instr::Operand::reg(instr::Register::RET, instr::RegMode::BIT64),
        });

        func_body!(codegen, self);

        // Return procedure
        let label = codegen.generate_label(Some(codegen.current_return_label.clone()));
        codegen.add_ir(label);

        let offset = codegen.get_stack_offset(&String::from("this"));
        codegen.add_ir(instr::IR::Load {
            dst: instr::Operand::reg(instr::Register::RET, instr::RegMode::BIT64),
            addr: instr::Operand::offset(offset, 8),
        });

        func_stack!(codegen, self, false);

        codegen.add_ir(instr::IR::Return);

        func_leave!(codegen);

        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::FunctionNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        function!(codegen, self, self.name);
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::MethodNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        method!(codegen, self, self.class_name.clone() + "_" + &self.name);
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ParameterNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        codegen.store_variable_in_stack(&self.name, &self.typ.typ)?;
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::BlockNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        codegen.enter_scope();
        for stmt in &self.statements {
            stmt.codegen(codegen)?;
        }
        codegen.leave_scope();
        Ok(instr::Operand::none())
    }
}

impl Codegenable for nodes::Statement {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let reg = match self {
            Self::Block(block_node) => block_node.codegen(codegen),
            Self::Expression(expr_node) => expr_node.codegen(codegen),
            Self::If(if_node) => if_node.codegen(codegen),
            Self::Let(let_node) => let_node.codegen(codegen),
            Self::Return(ret_node) => ret_node.codegen(codegen),
            Self::While(while_node) => while_node.codegen(codegen),
            Self::Break(break_node) => break_node.codegen(codegen),
            Self::Continue(continue_node) => continue_node.codegen(codegen),
        };
        codegen.reset_registers();
        reg
    }
}
impl Codegenable for nodes::LetNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let mut rhs = self.expression.codegen(codegen)?;
        debug_assert!(rhs != instr::Operand::none());
        if rhs.typ != instr::OperandType::Reg {
            let reg = codegen.get_register()?;
            let reg_mode = instr::RegMode::from(&self.typ.typ);
            let reg = instr::Operand::reg(reg, reg_mode);
            codegen.add_ir(instr::IR::Move { dst: reg, src: rhs });
            rhs = reg;
        }

        let offset = codegen.update_stack_offset(self.typ.typ.size());
        codegen.add_variable_offset(&self.name, offset);

        codegen.add_ir(instr::IR::Store {
            addr: instr::Operand::offset(offset, self.typ.typ.size()),
            value: rhs,
        });

        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::IfNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let cond_false = codegen.generate_label(None);
        let lbl_name = cond_false.get_lbl();
        let cond = self.condition.codegen(codegen)?;
        codegen.add_ir(instr::IR::Test {
            src1: cond,
            src2: cond,
        });
        // Test sets ZF to 1 if cond is false, so we need to jump if ZF is 1
        codegen.add_ir(instr::IR::JmpEq { name: lbl_name });
        self.if_body.codegen(codegen)?;

        if let Some(else_branch) = &self.else_body {
            let fin = codegen.generate_label(None);
            let lbl_name = fin.get_lbl();
            codegen.add_ir(instr::IR::Jmp { name: lbl_name });

            codegen.add_ir(cond_false);

            else_branch.codegen(codegen)?;

            codegen.add_ir(fin);
        } else {
            codegen.add_ir(cond_false);
        }
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ReturnNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        debug_assert!(self.typ != Type::Unknown);
        if let Some(return_expr) = &self.return_value {
            let reg = return_expr.codegen(codegen)?;
            debug_assert!(reg != instr::Operand::none());
            codegen.add_ir(instr::IR::Move {
                dst: instr::Operand::reg(instr::Register::RET, instr::RegMode::from(&self.typ)),
                src: reg,
            });
        }
        codegen.add_ir(instr::IR::Jmp {
            name: codegen.current_return_label.clone(),
        });
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::WhileNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        // jump to condition
        let cond_lbl = codegen.generate_label(None);
        let cond_name = cond_lbl.get_lbl();
        codegen.add_ir(instr::IR::Jmp { name: cond_name.clone() });

        // label for break statements to jump to
        let break_lbl = codegen.generate_label(None);
        let break_name = break_lbl.get_lbl();
        codegen.loop_stack.push((break_name, cond_name));

        // block code
        let block_lbl = codegen.generate_label(None);
        let block_name = block_lbl.get_lbl();
        codegen.add_ir(block_lbl);
        self.body.codegen(codegen)?;

        // condition code
        codegen.add_ir(cond_lbl);
        let cond = self.condition.codegen(codegen)?;
        codegen.add_ir(instr::IR::Test {
            src1: cond,
            src2: cond,
        });
        // Test sets ZF to 0 if cond is true, so we need to jump if ZF is 0
        codegen.add_ir(instr::IR::JmpNeq { name: block_name });

        codegen.add_ir(break_lbl);
        codegen.loop_stack.pop();
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::BreakNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        debug_assert!(!codegen.loop_stack.is_empty());
        let loop_lbl = codegen.loop_stack.last().unwrap();

        let break_lbl = loop_lbl.0.clone();
        codegen.add_ir(instr::IR::Jmp { name: break_lbl });
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ContinueNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        debug_assert!(!codegen.loop_stack.is_empty());
        let loop_lbl = codegen.loop_stack.last().unwrap();

        let cond_lbl = loop_lbl.1.clone();
        codegen.add_ir(instr::IR::Jmp { name: cond_lbl });
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::TypeNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_panic!("TypeNode::codegen() is not implemented yet.")
    }
}
impl Codegenable for nodes::Expression {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        match self {
            Self::Name(expr) => expr.codegen(codegen),
            Self::ArrayLiteral(expr) => expr.codegen(codegen),
            Self::ArrayAccess(expr) => expr.codegen(codegen),
            Self::Literal(expr) => expr.codegen(codegen),
            Self::Unary(expr) => expr.codegen(codegen),
            Self::Binary(expr) => expr.codegen(codegen),
            Self::FunctionCall(expr) => expr.codegen(codegen),
            Self::BuiltIn(expr) => expr.codegen(codegen),
        }
    }
}
impl Codegenable for nodes::ArrayLiteralNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_panic!("ExpressionArrayLiteralNode::codegen() is not implemented yet")
    }
}
impl Codegenable for nodes::ArrayAccessNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_panic!("ExpressionArrayAccessNode::codegen() is not implemented yet")
    }
}
impl Codegenable for nodes::LiteralNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let err_to_str = |e: ParseIntError| {
            let s = e.to_string();
            format!(
                "{}: {:?}: Integer Literal could not be parsed.\n{}: {}",
                ERR_STR, self.location, NOTE_STR, s
            )
        };
        macro_rules! parse_num {
            ($typ:ty, $fn:ident) => {{
                let v = self.value.parse::<$typ>().map_err(|e| err_to_str(e))?;
                Ok(instr::Operand::$fn(v))
            }};
        }
        let parse_bool = |s: &str| -> Result<instr::Operand, String> {
            match s {
                "true" => Ok(instr::Operand::imm_u8(1)),
                "false" => Ok(instr::Operand::imm_u8(0)),
                e => Err(format!(
                    "{}: {:?}: Boolean Literal could not be parsed.\n{}: {}",
                    ERR_STR, self.location, NOTE_STR, e
                )),
            }
        };
        match &self.typ {
            Type::I32 => parse_num!(i32, imm_i32),
            Type::I64 => parse_num!(i64, imm_i64),
            Type::U32 => parse_num!(u32, imm_u32),
            Type::U64 => parse_num!(u64, imm_u64),
            Type::Usize => parse_num!(u64, imm_u64),
            Type::Bool => parse_bool(&self.value),
            t => internal_panic!(format!(
                "Unexpected Type {:?} in ExpressionLiteralNode::codegen()!",
                t
            )),
        }
    }
}

impl Codegenable for nodes::UnaryNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        match self.operation {
            Operation::Negate => {
                let expr = self.expression.codegen(codegen)?;
                debug_assert!(expr != instr::Operand::none());
                debug_assert!(self.typ == Type::I32 || self.typ == Type::I64);
                let reg = codegen.get_register()?;
                let reg_mode = instr::RegMode::from(&self.typ);
                let reg = instr::Operand::reg(reg, reg_mode);
                codegen.add_ir(instr::IR::Move { dst: reg, src: expr });
                codegen.add_ir(instr::IR::Negate {
                    dst: reg,
                    src: reg,
                });
                Ok(reg)
            },
            e => internal_panic!(format!(
                "Unexpected Operation {:?} in ExpressionUnaryNode::codegen()!",
                e
            )),
        }
    }
}

impl Codegenable for nodes::BinaryNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        macro_rules! gen_bin {
            ($op1:expr, $op2:expr) => {
                {
                    let mut op1 = $op1.codegen(codegen)?;
                    debug_assert!(op1 != instr::Operand::none());
                    if op1.typ != instr::OperandType::Reg {
                        let reg = codegen.get_register()?;
                        let reg_mode = instr::RegMode::from(&$op1.get_type());
                        let reg = instr::Operand::reg(reg, reg_mode);
                        codegen.add_ir(instr::IR::Move { dst: reg, src: op1 });
                        op1 = reg;
                    }
                    debug_assert!(op1.typ == instr::OperandType::Reg);

                    let op2 = $op2.codegen(codegen)?;
                    debug_assert!(op2 != instr::Operand::none());
                    Ok::<(instr::Operand, instr::Operand), String>((op1, op2))
                }
            };
        }
        match &self.operation {
            Operation::Add => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Add {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
                Ok(lhs)
            }
            Operation::Sub => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Sub {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
                Ok(lhs)
            }
            Operation::Mul => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Mul {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                    signed: self.typ == Type::I32 || self.typ == Type::I64,
                });
                Ok(lhs)
            }
            Operation::Div => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Div {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                    signed: self.typ == Type::I32 || self.typ == Type::I64,
                });
                Ok(lhs)
            }
            Operation::Modulo => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Mod {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                    signed: self.typ == Type::I32 || self.typ == Type::I64,
                });
                Ok(lhs)
            }
            Operation::BitwiseAnd => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::And {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
                Ok(lhs)
            }
            Operation::BitwiseOr => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Or {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
                Ok(lhs)
            }
            Operation::BitwiseXor => {
                let (lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                codegen.add_ir(instr::IR::Xor {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
                Ok(lhs)
            }
            _ if self.is_comparison() => {
                let (mut lhs, rhs) = gen_bin!(self.lhs, self.rhs)?;
                let cmp_mode = self.operation;
                codegen.add_ir(instr::IR::Cmp {
                    dst: lhs,
                    src: rhs,
                });
                lhs.reg_mode = instr::RegMode::BIT8;
                match cmp_mode {
                    Operation::Equal => codegen.add_ir(instr::IR::SetEq { dst: lhs }),
                    Operation::NotEqual => codegen.add_ir(instr::IR::SetNeq { dst: lhs }),
                    Operation::GreaterThan => codegen.add_ir(instr::IR::SetGt { dst: lhs }),
                    Operation::GreaterThanOrEqual => codegen.add_ir(instr::IR::SetGte { dst: lhs }),
                    Operation::LessThan => codegen.add_ir(instr::IR::SetLt { dst: lhs }),
                    Operation::LessThanOrEqual => codegen.add_ir(instr::IR::SetLte { dst: lhs }),
                    _ => unreachable!(),
                }
                Ok(lhs)
            }
            Operation::Assign => {
                // This case is reversed because Assignment is right-associative
                // FIXME: Prove that this is correct
                let (rhs, lhs) = gen_bin!(self.rhs, self.lhs)?;
                codegen.add_ir(instr::IR::Store {
                    addr: lhs,
                    value: rhs,
                });
                Ok(rhs)
            }
            Operation::Dot => {
                let class_name = self.lhs.get_type().get_class_name();
                let instance = self.lhs.codegen(codegen)?;
                debug_assert!(instance != instr::Operand::none());

                let instance = match instance.typ {
                    instr::OperandType::Reg => {
                        // in a nested field access, the lhs is a register
                        // containing the address of the field, so we need
                        // to dereference it before we can use it
                        // codegen.add_ir(instr::IR::Load {
                        //     dst: instance,
                        //     addr: instance,
                        // });
                        instance
                    },
                    instr::OperandType::Address | instr::OperandType::Offset => {
                        let reg = codegen.get_register()?;
                        let reg = instr::Operand::reg(reg, instr::RegMode::BIT64);
                        codegen.add_ir(instr::IR::Load {
                            dst: reg,
                            addr: instance,
                        });
                        reg
                    },
                    e => todo!("BinaryNode::codegen() for {:?}", e),
                };
                debug_assert!(instance.typ == instr::OperandType::Reg);
                // At this point, instance is a pointer to the instance

                // if (instance == null) { exit(2); }
                let valid_reference = codegen.generate_label(None);
                let lbl_name = valid_reference.get_lbl();
                codegen.add_ir(instr::IR::Cmp {
                    dst: instance,
                    src: instr::Operand::imm_u64(0),
                });
                codegen.add_ir(instr::IR::JmpNeq { name: lbl_name });
                codegen.add_ir(instr::IR::Exit {
                    code: instr::Operand::imm_u64(2),
                });
                codegen.add_ir(valid_reference);

                match &(*self.rhs) {
                    nodes::Expression::Name(field) => {
                        let offset = codegen.sm.get_class_info(&class_name).get_field_offset(&field.name);
                        let imm = instr::Operand::imm_u64(offset as u64);
                        codegen.add_ir(instr::IR::Add {
                            dst: instance,
                            src1: instance,
                            src2: imm,
                        });
                        // instance now contains the address of the field
                        let op = instr::Operand::addr(instance.reg);
                        Ok(op)
                    }
                    nodes::Expression::FunctionCall(method) => {
                        // store registers
                        let counter = codegen.register_counter;
                        codegen.push_registers(counter);

                        // move `this` pointer into ARG1
                        codegen.add_ir(instr::IR::Move {
                            dst: instr::Operand::reg(instr::Register::ARG1, instr::RegMode::BIT64),
                            src: instance,
                        });
                        // codegen each argument and move it into the correct registers
                        for (index, arg) in method.arguments.iter().enumerate() {
                            let op = arg.codegen(codegen)?;
                            debug_assert!(op != instr::Operand::none());
                            let target_reg = instr::Register::arg(index + 1); // +1 because ARG1 is `this`
                            match op.typ {
                                instr::OperandType::Reg => {
                                    if op.reg != target_reg {
                                        // Move to correct register if necessary
                                        let target_reg = instr::Operand::reg(target_reg, op.reg_mode);
                                        codegen.add_ir(instr::IR::Move {
                                            dst: target_reg,
                                            src: op,
                                        });
                                    }
                                }
                                instr::OperandType::Address => {
                                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT64);
                                    codegen.add_ir(instr::IR::Move {
                                        dst: target_reg,
                                        src: op,
                                    });
                                }
                                instr::OperandType::ImmI32 | instr::OperandType::ImmU32 => {
                                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT32);
                                    codegen.add_ir(instr::IR::LoadImm {
                                        dst: target_reg,
                                        imm: op,
                                    });
                                }
                                instr::OperandType::ImmI64 | instr::OperandType::ImmU64 => {
                                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT64);
                                    codegen.add_ir(instr::IR::LoadImm {
                                        dst: target_reg,
                                        imm: op,
                                    });
                                }
                                instr::OperandType::Offset => {
                                    let target_reg =
                                        instr::Operand::reg(target_reg, instr::RegMode::from(&arg.get_type()));
                                    codegen.add_ir(instr::IR::Load {
                                        dst: target_reg,
                                        addr: op,
                                    });
                                }
                                op => {
                                    internal_panic!(format!(
                                        "ExpressionCallNode::codegen() can't handle argument type `{:?}` yet.",
                                        op
                                    ))
                                }
                            }
                        }
                        // call method
                        let name = class_name + "_" + &method.function_name;
                        codegen.add_ir(instr::IR::Call {
                            name,
                        });
                        // move the result into reg
                        if method.typ != Type::None {
                            let mut reg = instance.clone();
                            reg.reg_mode = instr::RegMode::from(&method.typ);
                            codegen.add_ir(instr::IR::Move {
                                dst: reg,
                                src: instr::Operand::reg(instr::Register::RET, instr::RegMode::from(&method.typ)),
                            });
                            // restore registers
                            codegen.pop_registers(counter);

                            // reg now contains the result of the method call
                            Ok(reg)
                        } else {
                            // restore registers
                            codegen.pop_registers(counter);

                            // method returns void, so we don't need to do anything
                            Ok(instr::Operand::none())
                        }
                    }
                    e => todo!("BinaryNode::codegen() for {:?}", e),
                }
            }
            e => todo!("BinaryNode::codegen() for {:?}", e),
        }
    }
}
impl Codegenable for nodes::CallNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        debug_assert!(self.typ != Type::Unknown);

        let result = if self.typ == Type::None {
            None
        } else {
            let result = codegen.get_register()?;
            let result_mode = instr::RegMode::from(&self.typ);
            let result = instr::Operand::reg(result, result_mode);
            Some(result)
        };

        let counter = codegen.register_counter;
        codegen.push_registers(counter);

        // Codegen each argument and move it into the correct registers
        for (index, arg) in self.arguments.iter().enumerate() {
            let op = arg.codegen(codegen)?;
            debug_assert!(op != instr::Operand::none());
            let target_reg = instr::Register::arg(index);
            match op.typ {
                instr::OperandType::Reg => {
                    if op.reg != target_reg {
                        // Move to correct register if necessary
                        let target_reg = instr::Operand::reg(target_reg, op.reg_mode);
                        codegen.add_ir(instr::IR::Move {
                            dst: target_reg,
                            src: op,
                        });
                    }
                }
                instr::OperandType::Address => {
                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT64);
                    codegen.add_ir(instr::IR::Move {
                        dst: target_reg,
                        src: op,
                    });
                }
                instr::OperandType::ImmI32 | instr::OperandType::ImmU32 => {
                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT32);
                    codegen.add_ir(instr::IR::LoadImm {
                        dst: target_reg,
                        imm: op,
                    });
                }
                instr::OperandType::ImmI64 | instr::OperandType::ImmU64 => {
                    let target_reg = instr::Operand::reg(target_reg, instr::RegMode::BIT64);
                    codegen.add_ir(instr::IR::LoadImm {
                        dst: target_reg,
                        imm: op,
                    });
                }
                instr::OperandType::Offset => {
                    let target_reg =
                        instr::Operand::reg(target_reg, instr::RegMode::from(&arg.get_type()));
                    codegen.add_ir(instr::IR::Load {
                        dst: target_reg,
                        addr: op,
                    });
                }
                op => {
                    internal_panic!(format!(
                        "ExpressionCallNode::codegen() can't handle argument type `{:?}` yet.",
                        op
                    ))
                }
            }
        }

        codegen.add_ir(instr::IR::Call {
            name: self.function_name.clone(),
        });

        if self.typ != Type::None {
            debug_assert!(result.is_some());
            let result = result.unwrap();
            codegen.pop_registers(counter);
            Ok(instr::Operand::reg(instr::Register::RET, result.reg_mode))
        } else {
            codegen.pop_registers(counter);
            Ok(instr::Operand::none())
        }
    }
}
impl Codegenable for nodes::NameNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let offset = codegen.get_stack_offset(&self.name);
        debug_assert!(self.typ != Type::None || self.typ != Type::Unknown);
        Ok(instr::Operand::offset(offset, self.typ.size()))
    }
}
impl Codegenable for nodes::BuiltInNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_panic!("ExpressionBuiltInNode::codegen() is not implemented yet")
    }
}
