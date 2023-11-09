
use std::collections::HashMap;

use super::nodes;
use crate::checker::Type;

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
    print_debug: bool
}
impl Codegen {
    pub fn new() -> Self {
        Self {
            sm: SizeManager::new(),
            current_class: String::new(),
            ir: Vec::new(),
            label_counter: 0,
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
        ast.codegen(self)
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

    fn add_ir(&mut self, ir: instr::IR) {
        self.ir.push(ir)
    }

    pub fn compile(&mut self) -> Result<(), String> {
        todo!()
    }

    pub fn run(&mut self) -> Result<(), String> {
        todo!()
    }
}

trait Codegenable {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String>;
}

impl Codegenable for nodes::FileNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
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
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        debug_assert!(codegen.current_class.is_empty());
        codegen.add_class(&self.name);
        codegen.current_class = self.name.clone();

        for field in &self.fields {
            field.codegen(codegen)?;
        }

        codegen.current_class.clear();
        todo!()
    }
}
impl Codegenable for nodes::FieldNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        let typ = &self.type_def.typ;
        debug_assert!(*typ != Type::None);
        codegen.add_field(&self.name, typ);
        Ok(())
    }
}
impl Codegenable for nodes::FieldAccess {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::FeatureNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::FunctionNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::MethodNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ReturnTypeNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ParameterNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::BlockNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}

impl Codegenable for nodes::Statement {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}

impl Codegenable for nodes::ExpressionNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}impl Codegenable for nodes::LetNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::AssignNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionIdentifierNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::IfNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ReturnNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::TypeNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ArgumentNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::Expression {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionArrayLiteralNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionArrayAccessNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionLiteralNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionBinaryNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionComparisonNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionCallNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionConstructorNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionFieldAccessNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::NameNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}
impl Codegenable for nodes::ExpressionBuiltInNode {
    fn codegen(&self, codegen: &mut Codegen) -> Result<(), String> {
        todo!()
    }
}