use std::collections::HashMap;
use std::num::ParseIntError;

use super::instr;
use crate::compiler::{CONSTRUCTOR_NAME, ERR_STR, NOTE_STR};
use crate::frontend::flags::Flags;
use crate::frontend::nodes;
use crate::frontend::parser::Operation;
use crate::middleend::checker::Type;

use crate::internal_error;

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
        let size = typ.size()?;
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
pub struct Codegen {
    sm: SizeManager,
    current_class: String,
    current_return_label: String,
    ir: Vec<instr::IR>,
    label_counter: usize,
    current_stack_offset: usize,
    stack_scopes: Vec<HashMap<String, usize>>,
    field_stack: Vec<Type>,
    // FIXME: Figure out a better way to do this
    loop_stack: Vec<(String, String)>,
    register_counter: usize,
    flags: Flags
}
impl Codegen {
    pub fn new(flags: Flags) -> Self {
        Self {
            sm: SizeManager::new(),
            current_class: String::new(),
            current_return_label: String::new(),
            ir: Vec::new(),
            label_counter: 0,
            current_stack_offset: 0,
            stack_scopes: Vec::new(),
            field_stack: Vec::new(),
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
                    "[DEBUG] Class `{}` has size {} bytes",
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
            println!("[DEBUG] Added new Class `{}`", class_name);
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
    fn get_field_offset(&self, class: &String, name: &String) -> usize {
        let info = self.sm.get_class_info(class);
        info.get_field_offset(name)
    }

    #[trace_call(extra)]
    fn get_register(&mut self) -> Result<instr::Register, String> {
        self.register_counter += 1;
        let v = self.register_counter;
        if self.register_counter == instr::Register::__COUNT as usize {
            internal_error!("We have run out of registers to assign!!")
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
        let offset = self.update_stack_offset(typ.size()?);
        self.add_variable_offset(name, offset);
        let reg = self.get_register()?;
        self.add_ir(instr::IR::Store {
            addr: instr::Operand::offset(offset),
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
        for class in &self.classes {
            class.codegen(codegen)?;
        }
        for function in &self.functions {
            function.codegen(codegen)?;
        }
        Ok(instr::Operand::none())
    }
}

impl Codegenable for nodes::ClassNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        for feature in &self.features {
            feature.codegen(codegen)?;
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
impl Codegenable for nodes::FeatureNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        func_assert!(codegen);
        func_entry!(codegen, self.class_name.clone() + "_" + &self.name);
        func_stack!(codegen, self, true);
        func_param!(codegen, self);

        if self.is_constructor {
            // let this: Class = calloc(1, sizeof(Class));
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
                addr: instr::Operand::offset(offset),
                value: instr::Operand::reg(instr::Register::RET, instr::RegMode::BIT64),
            });
        }

        func_body!(codegen, self);

        // Return procedure
        let label = codegen.generate_label(Some(codegen.current_return_label.clone()));
        codegen.add_ir(label);

        if self.is_constructor {
            let offset = codegen.get_stack_offset(&String::from("this"));
            codegen.add_ir(instr::IR::Load {
                dst: instr::Operand::reg(instr::Register::RET, instr::RegMode::BIT64),
                addr: instr::Operand::offset(offset),
            });
        }

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
            Self::Assign(assign_node) => assign_node.codegen(codegen),
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

impl Codegenable for nodes::ExpressionNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        self.expression.codegen(codegen)
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

        let offset = codegen.update_stack_offset(self.typ.typ.size()?);
        codegen.add_variable_offset(&self.name, offset);

        codegen.add_ir(instr::IR::Store {
            addr: instr::Operand::offset(offset),
            value: rhs,
        });

        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::AssignNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let mut rhs = self.expression.codegen(codegen)?;
        debug_assert!(rhs != instr::Operand::none());
        if rhs.typ != instr::OperandType::Reg {
            let reg = codegen.get_register()?;
            // FIXME: Why does AssigNode not have a type?
            let reg_mode = instr::RegMode::from(&self.expression.expression.get_type());
            let reg = instr::Operand::reg(reg, reg_mode);
            codegen.add_ir(instr::IR::Move { dst: reg, src: rhs });
            rhs = reg;
        }

        let lhs = self.name.codegen(codegen)?;
        debug_assert!(lhs != instr::Operand::none());

        codegen.add_ir(instr::IR::Store {
            addr: lhs,
            value: rhs,
        });
        Ok(instr::Operand::none())
    }
}
impl Codegenable for nodes::ExpressionIdentifierNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        self.expression.codegen(codegen)
    }
}
impl Codegenable for nodes::IfNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let cond = self.condition.codegen(codegen)?;
        let instr::OperandType::Cmp(cmp_mode) = cond.typ else { unreachable!() };

        let cond_false = codegen.generate_label(None);
        let lbl_name = cond_false.get_lbl();
        match cmp_mode {
            Operation::Eq => codegen.add_ir(instr::IR::JmpNeq { name: lbl_name }),
            Operation::Neq => codegen.add_ir(instr::IR::JmpEq { name: lbl_name }),
            Operation::Gt => codegen.add_ir(instr::IR::JmpLte { name: lbl_name }),
            Operation::Gte => codegen.add_ir(instr::IR::JmpLt { name: lbl_name }),
            Operation::Lt => codegen.add_ir(instr::IR::JmpGte { name: lbl_name }),
            Operation::Lte => codegen.add_ir(instr::IR::JmpGt { name: lbl_name }),
            _ => unreachable!(),
        }
        self.if_branch.codegen(codegen)?;

        if let Some(else_branch) = &self.else_branch {
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
        self.block.codegen(codegen)?;

        // condition code
        // if condition is true, jump to block code
        codegen.add_ir(cond_lbl);
        let cond = self.condition.codegen(codegen)?;
        let instr::OperandType::Cmp(cmp_mode) = cond.typ else { unreachable!() };
        match cmp_mode {
            Operation::Eq => codegen.add_ir(instr::IR::JmpEq { name: block_name }),
            Operation::Neq => codegen.add_ir(instr::IR::JmpNeq { name: block_name }),
            Operation::Gt => codegen.add_ir(instr::IR::JmpGt { name: block_name }),
            Operation::Gte => codegen.add_ir(instr::IR::JmpGte { name: block_name }),
            Operation::Lt => codegen.add_ir(instr::IR::JmpLt { name: block_name }),
            Operation::Lte => codegen.add_ir(instr::IR::JmpLte { name: block_name }),
            _ => unreachable!(),
        }
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
        internal_error!("TypeNode::codegen() is not implemented yet.")
    }
}
impl Codegenable for nodes::ArgumentNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        self.expression.codegen(codegen)
    }
}
impl Codegenable for nodes::Expression {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        match self {
            Self::Name(expr) => expr.codegen(codegen),
            Self::Identifier(expr) => expr.codegen(codegen),
            Self::ArrayLiteral(expr) => expr.codegen(codegen),
            Self::ArrayAccess(expr) => expr.codegen(codegen),
            Self::Literal(expr) => expr.codegen(codegen),
            Self::Binary(expr) => expr.codegen(codegen),
            Self::Comparison(expr) => expr.codegen(codegen),
            Self::FieldAccess(expr) => expr.codegen(codegen),
            Self::FunctionCall(expr) => expr.codegen(codegen),
            Self::ConstructorCall(expr) => expr.codegen(codegen),
            Self::BuiltIn(expr) => expr.codegen(codegen),
        }
    }
}
impl Codegenable for nodes::ExpressionArrayLiteralNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_error!("ExpressionArrayLiteralNode::codegen() is not implemented yet")
    }
}
impl Codegenable for nodes::ExpressionArrayAccessNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_error!("ExpressionArrayAccessNode::codegen() is not implemented yet")
    }
}
impl Codegenable for nodes::ExpressionLiteralNode {
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
        match &self.typ {
            Type::I32 => parse_num!(i32, imm_i32),
            Type::I64 => parse_num!(i64, imm_i64),
            Type::U32 => parse_num!(u32, imm_u32),
            Type::U64 => parse_num!(u64, imm_u64),
            Type::Usize => parse_num!(u64, imm_u64),
            t => internal_error!(format!(
                "Unexpected Type {:?} in ExpressionLiteralNode::codegen()!",
                t
            )),
        }
    }
}
impl Codegenable for nodes::ExpressionBinaryNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let mut lhs = self.lhs.codegen(codegen)?;
        debug_assert!(lhs != instr::Operand::none());
        if lhs.typ != instr::OperandType::Reg {
            let reg = codegen.get_register()?;
            let reg_mode = instr::RegMode::from(&self.typ);
            let reg = instr::Operand::reg(reg, reg_mode);
            codegen.add_ir(instr::IR::Move { dst: reg, src: lhs });
            lhs = reg;
        }
        debug_assert!(lhs.typ == instr::OperandType::Reg);

        let rhs = self.rhs.codegen(codegen)?;
        debug_assert!(rhs != instr::Operand::none());
        match &self.operation {
            Operation::Add => {
                codegen.add_ir(instr::IR::Add {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
            }
            Operation::Sub => {
                codegen.add_ir(instr::IR::Sub {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                });
            }
            Operation::Mul => {
                codegen.add_ir(instr::IR::Mul {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                    signed: self.typ == Type::I32 || self.typ == Type::I64,
                });
            }
            Operation::Div => {
                codegen.add_ir(instr::IR::Div {
                    dst: lhs,
                    src1: lhs,
                    src2: rhs,
                    signed: self.typ == Type::I32 || self.typ == Type::I64,
                });
            }
            _ => unreachable!(),
        }
        Ok(lhs)
    }
}
impl Codegenable for nodes::ExpressionComparisonNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let mut lhs = self.lhs.codegen(codegen)?;
        debug_assert!(lhs != instr::Operand::none());
        if lhs.typ != instr::OperandType::Reg {
            let reg = codegen.get_register()?;
            let reg_mode = instr::RegMode::from(&self.lhs.get_type());
            let reg = instr::Operand::reg(reg, reg_mode);
            codegen.add_ir(instr::IR::Move { dst: reg, src: lhs });
            lhs = reg;
        }
        debug_assert!(lhs.typ == instr::OperandType::Reg);

        let rhs = self.rhs.codegen(codegen)?;
        debug_assert!(rhs != instr::Operand::none());
        codegen.add_ir(instr::IR::Cmp { dst: lhs, src: rhs });
        Ok(instr::Operand::cmp(self.operation))
    }
}
impl Codegenable for nodes::ExpressionCallNode {
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
                        instr::Operand::reg(target_reg, instr::RegMode::from(&arg.typ));
                    codegen.add_ir(instr::IR::Load {
                        dst: target_reg,
                        addr: op,
                    });
                }
                op => {
                    return internal_error!(format!(
                        "ExpressionCallNode::codegen() can't handle argument type `{:?}` yet.",
                        op
                    ));
                }
            }
        }

        codegen.add_ir(instr::IR::Call {
            name: self.function_name.clone(),
        });

        if self.typ != Type::None {
            debug_assert!(result.is_some());
            let result = result.unwrap();
            // FIXME: Just return the RET register, no need to move it
            codegen.add_ir(instr::IR::Move {
                dst: result,
                src: instr::Operand::reg(instr::Register::RET, result.reg_mode),
            });
            codegen.pop_registers(counter);
            Ok(result)
        } else {
            codegen.pop_registers(counter);
            Ok(instr::Operand::none())
        }
    }
}
impl Codegenable for nodes::ExpressionConstructorNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let constructor = self.class_name.clone() + "_" + CONSTRUCTOR_NAME;

        let result = codegen.get_register()?;
        let result_mode = instr::RegMode::from(&self.typ);
        let result = instr::Operand::reg(result, result_mode);

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
                op => {
                    return internal_error!(format!(
                        "ExpressionConstructorNode::codegen() can't handle argument type `{:?}` yet.",
                        op
                    ));
                }
            }
        }

        codegen.add_ir(instr::IR::Call { name: constructor });

        codegen.add_ir(instr::IR::Move {
            dst: result,
            src: instr::Operand::reg(instr::Register::RET, result_mode),
        });

        codegen.pop_registers(counter);
        Ok(result)
    }
}
impl Codegenable for nodes::ExpressionFieldAccessNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        // This is only ever called at the root of the access, so it's always a variable
        // for nesting, we call self.codegen_field()
        debug_assert!(codegen.known_variable(&self.name));

        // load stack variable, which is a pointer to the instance
        let offset = codegen.get_stack_offset(&self.name);
        let reg = codegen.get_register()?;
        let reg = instr::Operand::reg(reg, instr::RegMode::BIT64);
        let var = instr::Operand::offset(offset);
        codegen.add_ir(instr::IR::Load {
            dst: reg,
            addr: var,
        });

        // reg now contains a reference to the instance

        // codegen field access
        codegen.field_stack.push(self.typ.clone());
        self.codegen_field(codegen, &self.field, &reg)
        // reg contains the address of the field
        // it is up to the caller to load or store the field
    }
}
impl nodes::ExpressionFieldAccessNode {
    #[trace_call(always)]
    fn codegen_field(
        &self,
        codegen: &mut Codegen,
        field: &nodes::ExpressionIdentifierNode,
        reg: &instr::Operand,
    ) -> Result<instr::Operand, String> {
        let class_type = codegen.field_stack.pop().unwrap();
        let Type::Class(class_name) = class_type else { panic!() };

        // if (reg == null) { exit(2); }
        let valid_reference = codegen.generate_label(None);
        codegen.add_ir(instr::IR::Cmp {
            dst: *reg,
            src: instr::Operand::imm_u32(0),
        });
        codegen.add_ir(instr::IR::JmpNeq {
            name: valid_reference.get_lbl(),
        });
        codegen.add_ir(instr::IR::Exit {
            code: instr::Operand::imm_i32(2),
        });
        codegen.add_ir(valid_reference);

        match &(*field.expression) {
            nodes::Expression::FieldAccess(field_access) => {
                let offset = codegen.get_field_offset(&class_name, &field_access.name);
                let imm = if self.typ.size()? == 4 {
                    instr::Operand::imm_u32(offset as u32)
                } else {
                    instr::Operand::imm_u64(offset as u64)
                };
                // add offset of field to base address
                codegen.add_ir(instr::IR::Add {
                    dst: *reg,
                    src1: *reg,
                    src2: imm,
                });

                codegen.add_ir(instr::IR::Load {
                    dst: *reg,
                    addr: *reg,
                });
                // reg now holds the address of the field

                // unwind field access by one level
                codegen.field_stack.push(field_access.typ.clone());
                field_access.codegen_field(codegen, &field_access.field, reg)
            }
            nodes::Expression::Name(name_node) => {
                let offset = codegen.get_field_offset(&class_name, &name_node.name);
                let imm = if name_node.typ.size()? == 4 {
                    instr::Operand::imm_u32(offset as u32)
                } else {
                    instr::Operand::imm_u64(offset as u64)
                };

                // add both together
                codegen.add_ir(instr::IR::Add {
                    dst: *reg,
                    src1: *reg,
                    src2: imm,
                });

                // reg now holds the address of the field
                let op = instr::Operand::addr(reg.reg);
                Ok(op)
            }
            nodes::Expression::FunctionCall(fn_call) => {
                // Method call on instance

                // store registers
                let counter = codegen.register_counter;
                codegen.push_registers(counter);

                // reg contains the address of the instance, which is the first argument
                codegen.add_ir(instr::IR::Move {
                    dst: instr::Operand::reg(instr::Register::ARG1, instr::RegMode::BIT64),
                    src: *reg,
                });
                // codegen the rest of the arguments
                for (index, arg) in fn_call.arguments.iter().enumerate() {
                    let op = arg.codegen(codegen)?;
                    debug_assert!(op != instr::Operand::none());
                    let target_reg = instr::Register::arg(index + 1);
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
                        instr::OperandType::Offset => {
                            let target_reg =
                                instr::Operand::reg(target_reg, instr::RegMode::from(&arg.typ));
                            codegen.add_ir(instr::IR::Load {
                                dst: target_reg,
                                addr: op,
                            });
                        }
                        instr::OperandType::ImmI32 | instr::OperandType::ImmU32 => {
                            let target_reg =
                                instr::Operand::reg(target_reg, instr::RegMode::BIT32);
                            codegen.add_ir(instr::IR::LoadImm {
                                dst: target_reg,
                                imm: op,
                            });
                        }
                        instr::OperandType::ImmI64 | instr::OperandType::ImmU64 => {
                            let target_reg =
                                instr::Operand::reg(target_reg, instr::RegMode::BIT64);
                            codegen.add_ir(instr::IR::LoadImm {
                                dst: target_reg,
                                imm: op,
                            });
                        }
                        op => {
                            return internal_error!(format!(
                                "ExpressionFieldAccessNode::codegen() can't handle argument type `{:?}` yet.",
                                op
                            ));
                        }
                    }
                }
                // call the method
                let name = class_name + "_" + &fn_call.function_name;
                codegen.add_ir(instr::IR::Call {
                    name,
                });

                // move the result into reg
                // FIXME: Handle methods that return void
                codegen.add_ir(instr::IR::Move {
                    dst: *reg,
                    src: instr::Operand::reg(instr::Register::RET, instr::RegMode::from(&fn_call.typ)),
                });

                codegen.pop_registers(counter);
                // reg now contains the result of the method call
                Ok(*reg)
            }
            _ => unreachable!(),
        }
    }
}
impl Codegenable for nodes::NameNode {
    #[trace_call(always)]
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let offset = codegen.get_stack_offset(&self.name);
        debug_assert!(self.typ != Type::None || self.typ != Type::Unknown);
        Ok(instr::Operand::offset(offset))
    }
}
impl Codegenable for nodes::ExpressionBuiltInNode {
    #[trace_call(always)]
    fn codegen(&self, _codegen: &mut Codegen) -> Result<instr::Operand, String> {
        internal_error!("ExpressionBuiltInNode::codegen() is not implemented yet")
    }
}
