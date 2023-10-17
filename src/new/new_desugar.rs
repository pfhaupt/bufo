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

    pub fn desugar_file(&mut self, file: &mut nodes::FileNode) -> Result<(), String> {
        self.fill_lookup(&file)?;
        file.desugar(self)
    }
}

trait Desugarable {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized;
}

impl Desugarable for nodes::FileNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        for f in &mut self.functions {
            f.desugar(desugarer)?;
        }
        for c in &mut self.classes {
            c.desugar(desugarer)?;
            self.functions.append(&mut c.functions);
            self.features.append(&mut c.features);
        }
        Ok(())
    }
}
impl Desugarable for nodes::ClassNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        desugarer.current_class = self.name.clone();
        for f in &mut self.fields {
            f.desugar(desugarer)?;
        }
        for f in &mut self.functions {
            f.desugar(desugarer)?;
        }
        for f in &mut self.features {
            f.desugar(desugarer)?;
        }
        desugarer.current_class.clear();
        Ok(())        
    }
}
impl Desugarable for nodes::FieldNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        debug_assert!(!desugarer.current_class.is_empty());
        self.type_def.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::FieldAccess {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FeatureNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
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
                for mut s in &mut self.block.statements {
                    s.desugar(desugarer)?;
                    statements.push(s.clone());
                }
                self.block.statements = statements;
                self.name = format!("{}_{}", desugarer.current_class, self.name);
                Ok(())
            }
            e => Err(format!(
                "{}: {:?}: Unknown feature `{e}`", ERR_STR, self.location
            ))
        }
    }
}
impl Desugarable for nodes::FunctionNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.return_type.desugar(desugarer)?;

        let old_block = &mut self.block.statements;
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
            self.block.statements.insert(0, s.clone());
        }
        // TODO: We can't always push this at the end, we need to consider cases where return is last statement
        let dec_index = parse_snippet!(
            "INTERNAL_FunctionNode::desugar()",
            nodes::Statement,
            "FUNCTION_COUNTER = FUNCTION_COUNTER - 1;"
        );
        self.block.statements.push(dec_index);
        for mut s in &mut self.block.statements {
            s.desugar(desugarer)?;
        }
        if !desugarer.current_class.is_empty() {
            let this_param = parse_snippet!(
                "INTERNAL_FunctionNode::desugar()",
                nodes::ParameterNode,
                &format!("this: {}", desugarer.current_class)
            );
            self.parameters.insert(0, this_param);
            self.name = format!("{}_{}", desugarer.current_class, self.name);
        }
        Ok(())
    }
}
impl Desugarable for nodes::ReturnTypeNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        Ok(())
    }
}
impl Desugarable for nodes::ParameterNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::BlockNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        for mut s in &mut self.statements {
            s.desugar(desugarer)?;
        }
        Ok(())
    }
}
impl Desugarable for nodes::Statement {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        match self {
            Self::Assign(assign_expr) =>
                assign_expr.desugar(desugarer)?,
            Self::Expression(expr) =>
                expr.desugar(desugarer)?,
            Self::If(if_block) =>
                if_block.desugar(desugarer)?,
            Self::Let(let_block) =>
                let_block.desugar(desugarer)?,
            Self::Return(return_block) =>
                return_block.desugar(desugarer)?,
        }
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.expression.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::LetNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.typ.desugar(desugarer)?;
        self.expression.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::AssignNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.name.desugar(desugarer)?;
        self.expression.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionIdentifierNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.expression.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::IfNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.condition.desugar(desugarer)?;
        self.if_branch.desugar(desugarer)?;
        if let Some(else_branch) = &mut self.else_branch {
            else_branch.desugar(desugarer)?;
        }
        Ok(())
    }
}
impl Desugarable for nodes::ReturnNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        if let Some(return_value) = &mut self.return_value {
            return_value.desugar(desugarer)?;
        }
        Ok(())
    }
}
impl Desugarable for nodes::TypeNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        Ok(())
    }
}
impl Desugarable for nodes::ArgumentNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.expression.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::Expression {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        match self {
            Self::Name(name) => 
                name.desugar(desugarer)?,
            Self::Binary(binary) => 
                binary.desugar(desugarer)?,
            Self::ArrayAccess(access) =>
                access.desugar(desugarer)?,
            Self::ArrayLiteral(literal) =>
                literal.desugar(desugarer)?,
            Self::BuiltIn(builtin) =>
                builtin.desugar(desugarer)?,
            Self::FieldAccess(field) =>
                field.desugar(desugarer)?,
            Self::FunctionCall(fn_call) =>
                fn_call.desugar(desugarer)?,
            Self::Identifier(identifier) =>
                identifier.desugar(desugarer)?,
            Self::Literal(literal) =>
                literal.desugar(desugarer)?,
            Self::None => unreachable!()
        }
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionArrayLiteralNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        for e in &mut self.elements {
            e.desugar(desugarer)?;
        }
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionArrayAccessNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionLiteralNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionBinaryNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.lhs.desugar(desugarer)?;
        self.rhs.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionCallNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        if desugarer.class_sizes.contains_key(&self.function_name) {
            // Assume that this is Class::new() call
            self.function_name.push_str("_new");
        }
        for a in &mut self.arguments {
            a.desugar(desugarer)?;
        }
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionFieldAccessNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        self.field.desugar(desugarer)?;
        Ok(())
    }
}
impl Desugarable for nodes::NameNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        Ok(())
    }
}
impl Desugarable for nodes::ExpressionBuiltInNode {
    fn desugar(&mut self, desugarer: &mut Desugarer) -> Result<(), String> where Self: Sized {
        for a in &mut self.arguments {
            a.desugar(desugarer)?;
        }
        Ok(())
    }
}