
use std::collections::{HashMap, VecDeque};

use super::nodes;
use crate::checker::Type;
use super::instr;

const LBL_STR: &str = "lbl_";

#[derive(Debug)]
struct ClassInfo {
    total_size: usize,
    field_offsets: HashMap<String, usize>,
}

impl ClassInfo {
    fn new() -> Self {
        Self {
            total_size: 0,
            field_offsets: HashMap::new()
        }
    }
    fn add_field(&mut self, field_name: &String, size: usize) {
        debug_assert!(!self.field_offsets.contains_key(field_name));
        self.field_offsets.insert(field_name.clone(), self.total_size);
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
            class_sizes: HashMap::new()
        }
    }

    fn add_class(&mut self, class_name: &String) {
        debug_assert!(!self.class_sizes.contains_key(class_name));
        self.class_sizes.insert(class_name.clone(), ClassInfo::new());
    }

    fn add_field(&mut self, class_name: &String, field_name: &String, typ: &Type) {
        let size = typ.size();
        let Some(class) = self.class_sizes.get_mut(class_name) else {
            // Two Options: Forgot to add class to Manager, or Type Checker f*ed up.
            unreachable!();
        };
        class.add_field(field_name, size);
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
    ir: Vec<instr::IR>,
    label_counter: usize,
    current_stack_offset: usize,
    stack_scopes: Vec<HashMap<String, usize>>,
    field_stack: Vec<Type>,
    register_counter: usize,
    print_debug: bool
}
impl Codegen {
    pub fn new() -> Self {
        Self {
            sm: SizeManager::new(),
            current_class: String::new(),
            ir: Vec::new(),
            label_counter: 0,
            current_stack_offset: 0,
            stack_scopes: Vec::new(),
            field_stack: Vec::new(),
            register_counter: 1,
            print_debug: false
         }
    }

    pub fn debug(self, print_debug: bool) -> Self {
        Self {
            print_debug,
            ..self
        }
    }

    pub fn generate_code(&mut self, ast: &nodes::FileNode) -> Result<(), String> {
        ast.codegen(self)?;
        Ok(())
    }

    fn generate_label(&mut self, name: Option<String>) -> instr::IR {
        if let Some(name) = name {
            instr::IR::Label { name }
        } else {
            let label = LBL_STR.to_string() + &self.label_counter.to_string();
            self.label_counter += 1;
            instr::IR::Label { name: label }
        }
    }

    fn add_class(&mut self, class_name: &String) {
        if self.print_debug {
            println!("[DEBUG] Added new Class `{}`", class_name);
        }
        self.sm.add_class(class_name);
    }

    fn add_field(&mut self, name: &String, typ: &Type) {
        debug_assert!(!self.current_class.is_empty());
        self.sm.add_field(&self.current_class, name, typ);
        if self.print_debug {
            println!("[DEBUG] Added new field `{}` to class `{}`", name, self.current_class);
            println!("[DEBUG] Class `{}` has a size of {} bytes now.", self.current_class, self.sm.get_class_size(&self.current_class));
        }
    }

    fn enter_scope(&mut self) {
        self.stack_scopes.push(HashMap::new())
    }

    fn leave_scope(&mut self) {
        debug_assert!(!self.stack_scopes.is_empty());
        self.stack_scopes.pop();
    }

    fn get_field_offset(&self, name: &String) -> usize {
        debug_assert!(!self.current_class.is_empty());
        let info = self.sm.get_class_info(&self.current_class);
        info.get_field_offset(name)
    }

    fn get_register(&mut self) -> Result<usize, String> {
        let v = self.register_counter;
        self.register_counter += 1;
        if self.register_counter == 100 {
            todo!()
        } else {
            Ok(v)
        }
    }

    fn reset_registers(&mut self) {
        self.register_counter = 1;
    }

    fn add_ir(&mut self, ir: instr::IR) {
        println!("{:?}", ir);
        self.ir.push(ir)
    }

    fn add_variable_offset(&mut self, name: &String, offset: usize) {
        debug_assert!(!self.stack_scopes.is_empty());
        self.stack_scopes.last_mut().unwrap().insert(name.clone(), offset);
    }

    fn update_stack_offset(&mut self, typ_size: usize) -> usize {
        let v = self.current_stack_offset;
        self.current_stack_offset += typ_size;
        v
    }

    fn known_variable(&mut self, name: &String) -> bool {
        for scope in self.stack_scopes.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }
        false
    }

    fn get_stack_offset(&mut self, name: &String) -> usize {
        for scope in self.stack_scopes.iter().rev() {
            if let Some(offset) = scope.get(name) {
                return *offset;
            }
        }
        println!("Could not find {name} in {scopes:?}", scopes=self.stack_scopes);
        unreachable!()
    }

    fn store_variable_in_stack(&mut self, name: &String, typ: &Type) -> Result<(), String> {
        let offset = self.update_stack_offset(typ.size());
        let reg = self.get_register()?;
        self.add_variable_offset(name, offset);
        self.add_ir(instr::IR::Store {
            addr: instr::Operand::StackOffset(offset),
            value: instr::Operand::Reg(reg, instr::RegMode::from(typ))
        });
        Ok(())
    }

    pub fn compile(&mut self) -> Result<(), String> {
        todo!()
    }

    pub fn run(&mut self) -> Result<(), String> {
        todo!()
    }
}

trait Codegenable {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String>;
}

impl Codegenable for nodes::FileNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        for class in &self.classes {
            class.codegen(codegen)?;
        }
        for function in &self.functions {
            function.codegen(codegen)?;
        }
        todo!()
    }
}

impl Codegenable for nodes::ClassNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        debug_assert!(codegen.current_class.is_empty());
        codegen.add_class(&self.name);
        codegen.current_class = self.name.clone();

        for field in &self.fields {
            field.codegen(codegen)?;
        }

        for feature in &self.features {
            feature.codegen(codegen)?;
        }

        for method in &self.methods {
            method.codegen(codegen)?;
        }

        codegen.current_class.clear();
        todo!()
    }
}
impl Codegenable for nodes::FieldNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let typ = &self.type_def.typ;
        debug_assert!(*typ != Type::None);
        codegen.add_field(&self.name, typ);
        Ok(instr::Operand::None)
    }
}
impl Codegenable for nodes::FieldAccess {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::FeatureNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        // TODO: Make this a macro, it's the same thing for Functions and Methods
        debug_assert!(!codegen.current_class.is_empty());
        debug_assert!(codegen.current_stack_offset == 0);
        debug_assert!(codegen.stack_scopes.is_empty());

        codegen.enter_scope();

        // Generate entrypoint for later `call func_name` call
        let name = self.class_name.clone() + "_" + &self.name;
        let label = codegen.generate_label(Some(name.clone()));
        codegen.add_ir(label);

        // Prepare stack offset
        let mut bytes = self.stack_size;
        if bytes % 16 != 0 {
            // 16-byte align the stack
            bytes += 16 - bytes % 16;
        }
        codegen.add_ir(instr::IR::AllocStack { bytes });

        // Prepare parameters
        for param in &self.parameters {
            param.codegen(codegen)?;
        }

        if self.is_constructor {
            // let this: Class = alloc(sizeof(Class));
            let alloc_size = instr::Operand::Reg(instr::Operand::ARG1, instr::RegMode::BIT64);
            codegen.add_ir(instr::IR::LoadImm {
                dst: alloc_size,
                imm: instr::Operand::Imm64(codegen.sm.get_class_size(&self.class_name) as u64)
            });
            codegen.add_ir(instr::IR::Call { name: String::from("malloc") });
            let offset = codegen.update_stack_offset(8);
            codegen.add_variable_offset(&String::from("this"), offset);
            codegen.add_ir(instr::IR::Store {
                addr: instr::Operand::StackOffset(offset),
                value: instr::Operand::Reg(instr::Operand::RET, instr::RegMode::BIT64)
            });
        }

        // Function body
        self.block.codegen(codegen)?;

        // Return procedure
        let label = codegen.generate_label(Some(name + "_return"));
        codegen.add_ir(label);

        if self.is_constructor {
            let offset = codegen.get_stack_offset(&String::from("this"));
            codegen.add_ir(instr::IR::Load {
                dst: instr::Operand::Reg(instr::Operand::RET, instr::RegMode::BIT64),
                addr: instr::Operand::StackOffset(offset)
            });
        }

        // Clean up stack offset
        codegen.add_ir(instr::IR::DeallocStack { bytes });

        codegen.add_ir(instr::IR::Return);

        codegen.current_stack_offset = 0;
        codegen.reset_registers();
        codegen.leave_scope();
        Ok(instr::Operand::None)
    }
}
impl Codegenable for nodes::FunctionNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::MethodNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        // TODO: Make this a macro, it's the same thing for Functions and Methods
        debug_assert!(!codegen.current_class.is_empty());
        debug_assert!(codegen.current_stack_offset == 0);
        debug_assert!(codegen.stack_scopes.is_empty());

        codegen.enter_scope();

        // Generate entrypoint for later `call func_name` call
        let name = self.class_name.clone() + "_" + &self.name;
        let label = codegen.generate_label(Some(name.clone()));
        codegen.add_ir(label);

        // Prepare stack offset
        let mut bytes = self.stack_size;
        if bytes % 16 != 0 {
            // 16-byte align the stack
            bytes += 16 - bytes % 16;
        }
        codegen.add_ir(instr::IR::AllocStack { bytes });

        // Add `this` param
        // FIXME: This is unreliable once we add static methods and all that
        //        It'd be 100 times better and easier if the parser would just add the parameter directly
        //        instead of the implicit use in the Type Checker and Codegen.
        codegen.store_variable_in_stack(&String::from("this"), &Type::Class(self.class_name.clone()));

        // Prepare parameters
        for param in &self.parameters {
            param.codegen(codegen)?;
        }

        // Function body
        self.block.codegen(codegen)?;

        // Return procedure
        let label = codegen.generate_label(Some(name + "_return"));
        codegen.add_ir(label);

        // Clean up stack offset
        codegen.add_ir(instr::IR::DeallocStack { bytes });

        codegen.add_ir(instr::IR::Return);

        codegen.current_stack_offset = 0;
        codegen.reset_registers();
        codegen.leave_scope();
        Ok(instr::Operand::None)
    }
}
impl Codegenable for nodes::ReturnTypeNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ParameterNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        codegen.store_variable_in_stack(&self.name, &self.typ.typ)?;
        Ok(instr::Operand::None)
    }
}
impl Codegenable for nodes::BlockNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        codegen.enter_scope();
        for stmt in &self.statements {
            stmt.codegen(codegen)?;
        }
        codegen.leave_scope();
        Ok(instr::Operand::None)
    }
}

impl Codegenable for nodes::Statement {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let reg = match self {
            Self::Assign(assign_node) => assign_node.codegen(codegen),
            Self::Expression(expr_node) => expr_node.codegen(codegen),
            Self::If(if_node) => if_node.codegen(codegen),
            Self::Let(let_node) => let_node.codegen(codegen),
            Self::Return(ret_node) => ret_node.codegen(codegen)
        };
        codegen.reset_registers();
        reg
    }
}

impl Codegenable for nodes::ExpressionNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        self.expression.codegen(codegen)
    }
}impl Codegenable for nodes::LetNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::AssignNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let rhs = self.expression.codegen(codegen)?;
        debug_assert!(rhs != instr::Operand::None);
        
        let lhs = self.name.codegen(codegen)?;
        debug_assert!(lhs != instr::Operand::None);

        codegen.add_ir(instr::IR::Store {
            addr: lhs,
            value: rhs
        });
        Ok(instr::Operand::None)
    }
}
impl Codegenable for nodes::ExpressionIdentifierNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        self.expression.codegen(codegen)
    }
}
impl Codegenable for nodes::IfNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ReturnNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::TypeNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ArgumentNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::Expression {
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
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionArrayAccessNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionLiteralNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionBinaryNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionComparisonNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionCallNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionConstructorNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionFieldAccessNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        // This is only ever called at the root of the access, so it's always a variable
        // for nesting, we call self.codegen_field()
        debug_assert!(codegen.known_variable(&self.name));

        // load stack variable, which is a pointer
        let offset = codegen.get_stack_offset(&self.name);
        let reg = codegen.get_register()?;
        let reg = instr::Operand::Reg(reg, instr::RegMode::BIT64);
        let var = instr::Operand::StackOffset(offset);
        codegen.add_ir(instr::IR::Load {
            dst: reg,
            addr: var
        });
        codegen.field_stack.push(self.typ.clone());

        // codegen field access
        let field = self.codegen_field(codegen, &self.field)?;

        // add both together
        codegen.add_ir(instr::IR::Add {
            dst: reg,
            src1: reg,
            src2: field
        });

        // reg now (hopefully) contains Pointer to field of actual instance
        codegen.add_ir(instr::IR::Load {
            dst: reg,
            addr: reg
        });

        // reg should now contain the value of the field
        Ok(reg)
    }
}
impl nodes::ExpressionFieldAccessNode {
    fn codegen_field(&self, codegen: &mut Codegen, field: &nodes::ExpressionIdentifierNode) -> Result<instr::Operand, String> {
        match &(*field.expression) {
            nodes::Expression::FieldAccess(field_access) => {
                todo!()
            }
            nodes::Expression::Name(name_node) => {
                let class_type = codegen.field_stack.pop().unwrap();
                let Type::Class(class_name) = class_type else { panic!() };

                let current_class = codegen.current_class.clone();
                codegen.current_class = class_name;
                let offset = codegen.get_field_offset(&name_node.name);
                let reg = codegen.get_register()?;
                let reg = instr::Operand::Reg(reg, instr::RegMode::from(&name_node.typ));
                codegen.add_ir(instr::IR::LoadImm {
                    dst: reg,
                    // FIXME: Improve this
                    imm: if name_node.typ.size() == 4 {
                        instr::Operand::Imm32(offset as u32)
                    } else {
                        instr::Operand::Imm64(offset as u64)
                    }
                });
                codegen.current_class = current_class;
                Ok(reg)
            }
            nodes::Expression::FunctionCall(fn_call) => {
                todo!()
            }
            _ => unreachable!()
        }
    }
}
impl Codegenable for nodes::NameNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        let offset = codegen.get_stack_offset(&self.name);
        debug_assert!(self.typ != Type::None || self.typ != Type::Unknown);
        let reg = codegen.get_register()?;
        let reg = instr::Operand::Reg(reg, instr::RegMode::from(&self.typ));
        codegen.add_ir(instr::IR::Load {
            dst: reg,
            addr: instr::Operand::StackOffset(offset)
        });
        Ok(reg)
    }
}
impl Codegenable for nodes::ExpressionBuiltInNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<instr::Operand, String> {
        todo!()
    }
}