use std::collections::HashMap;
use std::fmt::format;

use crate::checker::Type;

use crate::codegen::{SizeManager, ERR_STR};
use crate::parse_snippet;
use super::nodes::{self, BlockNodeBuilder};

pub struct Desugarer {
    current_class: String,
    // TODO: Is Lookup in Desugaring necessary, or can we save time here?
    class_sizes: HashMap<String, usize>,
    sm: SizeManager,
    print_debug: bool,
}

impl Desugarer {
    pub fn new() -> Self {
        Self {
            current_class: String::new(),
            class_sizes: HashMap::new(),
            sm: SizeManager {},
            print_debug: false,
        }
    }

    pub fn debug(self, debug: bool) -> Self {
        Self {
            print_debug: debug,
            ..self
        }
    }

    fn fill_lookup(&mut self, file: &nodes::FileNode) -> Result<(), String> {
        for class in &file.classes {
            let name = class.name.clone();
            self.class_sizes.insert(name.clone(), 0);
            for f in &class.fields {
                let typ = &f.type_def.typ;
                *self.class_sizes.get_mut(&name).unwrap() += self.sm.get_type_size(typ);
            }
        }
        if self.print_debug {
            println!("[DEBUG] Filled Lookup! Found: ");
            if self.class_sizes.is_empty() {
                println!(" > No classes found!");
            }
            for c in &self.class_sizes {
                println!(" > {} -> {} bytes", c.0, c.1);
            }
        }
        Ok(())
    }

    pub fn desugar_file(&mut self, file: nodes::FileNode) -> Result<nodes::FileNode, String> {
        self.fill_lookup(&file)?;
        file.desugar(self)
    }
}

trait Desugarable {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized;
}

impl Desugarable for nodes::FileNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        let mut desugared_functions = vec![];
        for f in self.functions {
            desugared_functions.push(f.desugar(desugarer)?);
        }

        let mut desugared_classes = vec![];
        let mut desugared_features = vec![];
        for c in self.classes {
            let mut desugared_class = c.desugar(desugarer)?;
            
            desugared_functions.extend(desugared_class.functions);
            desugared_features.extend(desugared_class.features);
            
            // TODO: Is this necessary?
            desugared_class.functions = vec![];
            desugared_class.features = vec![];

            desugared_classes.push(desugared_class);
        }
        self.classes = desugared_classes;
        self.features = desugared_features;
        self.functions = desugared_functions;
        Ok(self)
    }
}
impl Desugarable for nodes::ClassNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        desugarer.current_class = self.name.clone();
        let mut fields = vec![];
        for f in self.fields {
            fields.push(f.desugar(desugarer)?);
        }
        
        let mut functions = vec![];
        for f in self.functions {
            functions.push(f.desugar(desugarer)?);
        }

        let mut features = vec![];
        for f in self.features {
            features.push(f.desugar(desugarer)?);
        }
        desugarer.current_class.clear();
        self.fields = fields;
        self.functions = functions;
        self.features = features;
        Ok(self)        
    }
}
impl Desugarable for nodes::FieldNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        debug_assert!(!desugarer.current_class.is_empty());
        self.type_def = self.type_def.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::FieldAccess {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FeatureNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        debug_assert!(!desugarer.current_class.is_empty());
        match self.name.as_str() {
            "new" => {
                if self.return_type.typ != Type::None {
                    return Err(format!(
                        "{}: {:?}: Unexpected return type for feature `new`: {}",
                        ERR_STR,
                        self.return_type.location,
                        self.return_type.typ
                    ));
                }
                self.return_type.typ = Type::Class(desugarer.current_class.clone());
                let inc_index = parse_snippet!(
                    "INTERNAL_FunctionNode::desugar()",
                    nodes::BlockNode,
                    "{
                        FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                        if (FUNCTION_COUNTER >= FUNCTION_LIMIT) {
                            EXIT(STACK_OVERFLOW_CODE);
                        }
                    }"
                );
                for s in inc_index.statements.iter().rev() {
                    self.block.statements.insert(0, s.clone());
                }

                let this_stmt = parse_snippet!(
                    "INTERNAL_FeatureNode::desugar()",
                    nodes::Statement,
                    &format!("let this: {class_name} = MALLOC({size});",
                        class_name = desugarer.current_class,
                        size = desugarer.class_sizes.get(&desugarer.current_class).unwrap())
                );
                self.block.statements.insert(0, this_stmt);
                let dec_index = parse_snippet!(
                    "INTERNAL_FunctionNode::desugar()",
                    nodes::Statement,
                    "FUNCTION_COUNTER = FUNCTION_COUNTER - 1;"
                );
                self.block.statements.push(dec_index);
                let return_this_stmt = parse_snippet!(
                    "INTERNAL_FeatureNode::desugar()",
                    nodes::Statement,
                    "return this;"
                );
                self.block.statements.push(return_this_stmt);
                let mut statements = vec![];
                for s in self.block.statements {
                    statements.push(s.desugar(desugarer)?);
                }
                self.block.statements = statements;
                self.name = format!("{}_{}", desugarer.current_class, self.name);
                Ok(self)
            }
            e => Err(format!(
                "{}: {:?}: Unknown feature `{e}`", ERR_STR, self.location
            ))
        }
    }
}
impl Desugarable for nodes::FunctionNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.return_type = self.return_type.desugar(desugarer)?;

        let old_block = self.block;
        let mut statements = old_block.statements;
        // TODO: Should we keep this here, or inline it at the last stage (when generating ASM)?
        let inc_index = parse_snippet!(
            "INTERNAL_FunctionNode::desugar()",
            nodes::BlockNode,
            "{
                FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                if (FUNCTION_COUNTER >= FUNCTION_LIMIT) {
                    EXIT(STACK_OVERFLOW_CODE);
                }
            }"
        );
        for s in inc_index.statements.iter().rev() {
            statements.insert(0, s.clone());
        }
        // TODO: We can't always push this at the end, we need to consider cases where return is last statement
        let dec_index = parse_snippet!(
            "INTERNAL_FunctionNode::desugar()",
            nodes::Statement,
            "FUNCTION_COUNTER = FUNCTION_COUNTER - 1;"
        );
        statements.push(dec_index);
        let mut new_stmt = vec![];
        for s in statements {
            new_stmt.push(s.desugar(desugarer)?);
        }
        let body = BlockNodeBuilder::default()
            .location(old_block.location)
            .statements(new_stmt)
            .build()
            .unwrap();
        self.block = body;
        if !desugarer.current_class.is_empty() {
            let this_param = parse_snippet!(
                "INTERNAL_FunctionNode::desugar()",
                nodes::ParameterNode,
                &format!("this: {}", desugarer.current_class)
            );
            self.parameters.insert(0, this_param);
            self.name = format!("{}_{}", desugarer.current_class, self.name);
        }
        Ok(self)
    }
}
impl Desugarable for nodes::ReturnTypeNode {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        Ok(self)
    }
}
impl Desugarable for nodes::ParameterNode {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::BlockNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        let mut statements = vec![];
        for s in self.statements {
            statements.push(s.desugar(desugarer)?);
        }
        self.statements = statements;
        Ok(self)
    }
}
impl Desugarable for nodes::Statement {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        match self {
            Self::Assign(assign_expr) =>
                Ok(Self::Assign(assign_expr.desugar(desugarer)?)),
            Self::Expression(expr) =>
                Ok(Self::Expression(expr.desugar(desugarer)?)),
            Self::If(if_block) =>
                Ok(Self::If(if_block.desugar(desugarer)?)),
            Self::Let(let_block) =>
                Ok(Self::Let(let_block.desugar(desugarer)?)),
            Self::Return(return_block) =>
                Ok(Self::Return(return_block.desugar(desugarer)?)),
        }
    }
}
impl Desugarable for nodes::ExpressionNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.expression = self.expression.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::LetNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.typ = self.typ.desugar(desugarer)?;
        self.expression = self.expression.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::AssignNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.name = self.name.desugar(desugarer)?;
        self.expression = self.expression.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionIdentifierNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.expression = Box::new((*self.expression).desugar(desugarer)?);
        Ok(self)
    }
}
impl Desugarable for nodes::IfNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.condition = self.condition.desugar(desugarer)?;
        self.if_branch = self.if_branch.desugar(desugarer)?;
        if let Some(else_branch) = self.else_branch {
            self.else_branch = Some(else_branch.desugar(desugarer)?);
        }
        Ok(self)
    }
}
impl Desugarable for nodes::ReturnNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        if let Some(return_value) = self.return_value {
            self.return_value = Some(return_value.desugar(desugarer)?);
        }
        Ok(self)
    }
}
impl Desugarable for nodes::TypeNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        Ok(self)
    }
}
impl Desugarable for nodes::ArgumentNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.expression = self.expression.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::Expression {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        match self {
            Self::Name(name) => 
                Ok(Self::Name(name.desugar(desugarer)?)),
            Self::Binary(binary) => 
                Ok(Self::Binary(binary.desugar(desugarer)?)),
            Self::ArrayAccess(access) =>
                Ok(Self::ArrayAccess(access.desugar(desugarer)?)),
            Self::ArrayLiteral(literal) =>
                Ok(Self::ArrayLiteral(literal.desugar(desugarer)?)),
            Self::BuiltIn(builtin) =>
                Ok(Self::BuiltIn(builtin.desugar(desugarer)?)),
            Self::FieldAccess(field) =>
                Ok(Self::FieldAccess(field.desugar(desugarer)?)),
            Self::FunctionCall(fn_call) =>
                Ok(Self::FunctionCall(fn_call.desugar(desugarer)?)),
            Self::Identifier(identifier) =>
                Ok(Self::Identifier(identifier.desugar(desugarer)?)),
            Self::Literal(literal) =>
                Ok(Self::Literal(literal.desugar(desugarer)?)),
            Self::None => unreachable!()
        }
    }
}
impl Desugarable for nodes::ExpressionArrayLiteralNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        let mut elements = vec![];
        for e in self.elements {
            elements.push(e.desugar(desugarer)?);
        }
        self.elements = elements;
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionArrayAccessNode {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionLiteralNode {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionBinaryNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.lhs = Box::new(self.lhs.desugar(desugarer)?);
        self.rhs = Box::new(self.rhs.desugar(desugarer)?);
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionCallNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        if desugarer.class_sizes.contains_key(&self.function_name) {
            // Assume that this is Class::new() call
            self.function_name.push_str("_new");
        }
        let mut arguments = vec![];
        for a in self.arguments {
            arguments.push(a.desugar(desugarer)?);
        }
        self.arguments = arguments;
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionFieldAccessNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        self.field = self.field.desugar(desugarer)?;
        Ok(self)
    }
}
impl Desugarable for nodes::NameNode {
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        Ok(self)
    }
}
impl Desugarable for nodes::ExpressionBuiltInNode {
    fn desugar(mut self, desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        let mut arguments = vec![];
        for a in self.arguments {
            arguments.push(a.desugar(desugarer)?);
        }
        self.arguments = arguments;
        Ok(self)
    }
}