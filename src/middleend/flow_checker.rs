use tracer::trace_call;

use crate::{
    compiler::{ERR_STR, WARN_STR},
    frontend::{flags::Flags, nodes},
    internal_error,
};
use crate::compiler::NOTE_STR;

use super::type_checker::Type;

#[derive(Debug, PartialEq, Copy, Clone)]
#[allow(unused)]
enum FlowType {
    Linear,
    MayReturn,
    AlwaysReturn,
    MayBreak,
    AlwaysBreak,
    MayContinue,
    AlwaysContinue,
}

pub struct FlowChecker {
    #[allow(unused)]
    flags: Flags,
}

impl FlowChecker {
    pub fn new(flags: Flags) -> FlowChecker {
        FlowChecker { flags }
    }

    pub fn check(&self, ast: &nodes::FileNode) -> Result<(), String> {
        let flow = ast.check(self)?;
        debug_assert!(flow == FlowType::Linear);
        Ok(())
    }
}

trait Flowable {
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String>;
}

impl Flowable for nodes::FileNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        for class in &self.classes {
            class.check(checker)?;
        }
        for function in &self.functions {
            function.check(checker)?;
        }
        Ok(FlowType::Linear)
    }
}
impl Flowable for nodes::ClassNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        for feature in &self.features {
            feature.check(checker)?;
        }
        for method in &self.methods {
            method.check(checker)?;
        }
        Ok(FlowType::Linear)
    }
}
impl Flowable for nodes::FeatureNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let flow = self.block.check(checker)?;
        if self.is_constructor {
            // Constructors never return a value
            // It's all implicit at the end of the block
            if flow != FlowType::Linear {
                Err(format!(
                    "{}: {:?}: Return values are not allowed in constructors.\n{}: Returning `this` is implicit in constructors.",
                    ERR_STR, self.location, NOTE_STR
                ))
            } else {
                Ok(FlowType::Linear)
            }
        } else {
            internal_error!("There's only one feature now, and it's a constructor")
        }
    }
}
impl Flowable for nodes::MethodNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let flow = self.block.check(checker)?;
        if flow != FlowType::AlwaysReturn {
            if self.return_type.typ != Type::None {
                Err(format!(
                    "{}: {:?}: Method `{}` does not always return a value",
                    ERR_STR, self.location, self.name
                ))
            } else {
                // Implicit return statement.
                Ok(FlowType::AlwaysReturn)
            }
        } else {
            Ok(flow)
        }
    }
}
impl Flowable for nodes::FunctionNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let flow = self.block.check(checker)?;
        if flow != FlowType::AlwaysReturn {
            if self.return_type.typ != Type::None {
                Err(format!(
                    "{}: {:?}: Function `{}` does not always return a value",
                    ERR_STR, self.location, self.name
                ))
            } else {
                // Implicit return statement.
                Ok(FlowType::AlwaysReturn)
            }
        } else {
            Ok(flow)
        }
    }
}
impl Flowable for nodes::BlockNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let mut flow = FlowType::Linear;
        for index in 0..self.statements.len() {
            let statement = &self.statements[index];
            flow = statement.check(checker)?;
            if flow == FlowType::AlwaysReturn {
                if index != self.statements.len() - 1 {
                    println!(
                        "{}: {:?}: Unreachable code",
                        WARN_STR,
                        self.statements[index + 1].get_loc()
                    );
                }
                break;
            }
        }
        Ok(flow)
    }
}
impl Flowable for nodes::Statement {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        match self {
            Self::Expression(expression_node) => expression_node.check(checker),
            Self::Let(let_node) => let_node.check(checker),
            Self::Assign(assign_node) => assign_node.check(checker),
            Self::If(if_node) => if_node.check(checker),
            Self::Return(return_node) => return_node.check(checker),
            Self::While(while_node) => while_node.check(checker),
            Self::Break(break_node) => break_node.check(checker),
            Self::Continue(continue_node) => continue_node.check(checker),
        }
    }
}

impl Flowable for nodes::LetNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.expression.check(checker)
    }
}

impl Flowable for nodes::AssignNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.name.check(checker)?;
        self.expression.check(checker)?;
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::IfNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let cond_flow = self.condition.check(checker)?;
        // Later on we might want to check if the condition always exits
        debug_assert!(cond_flow == FlowType::Linear);
        let if_flow = self.if_branch.check(checker)?;
        if let Some(else_branch) = &self.else_branch {
            let else_flow = else_branch.check(checker)?;
            if checker.flags.debug {
                println!(
                    "[DEBUG] IfNode::check: if_flow: {:?}, else_flow: {:?}",
                    if_flow, else_flow
                );
            }
            match (if_flow, else_flow) {
                (FlowType::AlwaysReturn, FlowType::AlwaysReturn) => Ok(FlowType::AlwaysReturn),
                (FlowType::AlwaysReturn, _) => Ok(FlowType::MayReturn),
                (_, FlowType::AlwaysReturn) => Ok(FlowType::MayReturn),
                (FlowType::Linear, FlowType::Linear) => Ok(FlowType::Linear),
                (i, e) => todo!("{i:?}, {e:?}"),
            }
        } else {
            match if_flow {
                FlowType::AlwaysReturn | FlowType::MayReturn => Ok(FlowType::MayReturn),
                FlowType::Linear => Ok(FlowType::Linear),
                i => todo!("{i:?}"),
            }
        }
    }
}

impl Flowable for nodes::ReturnNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        // FIXME: Value might cause an exit
        Ok(FlowType::AlwaysReturn)
    }
}
impl Flowable for nodes::WhileNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        let cond_flow = self.condition.check(checker)?;
        // Later on we might want to check if the condition always exits
        debug_assert!(cond_flow == FlowType::Linear);
        let block_flow = self.block.check(checker)?;
        if checker.flags.debug {
            println!(
                "[DEBUG] WhileNode::check: block_flow: {:?}",
                block_flow
            );
        }
        if block_flow == FlowType::AlwaysReturn {
            Ok(FlowType::MayReturn)
        } else {
            Ok(block_flow)
        }
    }
}

impl Flowable for nodes::BreakNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::AlwaysBreak)
    }
}

impl Flowable for nodes::ContinueNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::AlwaysContinue)
    }
}

impl Flowable for nodes::Expression {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        match self {
            Self::Name(name_node) => name_node.check(checker),
            Self::Identifier(identifier_node) => identifier_node.check(checker),
            Self::ArrayLiteral(array_literal_node) => array_literal_node.check(checker),
            Self::ArrayAccess(array_access_node) => array_access_node.check(checker),
            Self::Literal(literal_node) => literal_node.check(checker),
            Self::Binary(binary_node) => binary_node.check(checker),
            Self::Comparison(comparison_node) => comparison_node.check(checker),
            Self::FieldAccess(field_access_node) => field_access_node.check(checker),
            Self::FunctionCall(function_call_node) => function_call_node.check(checker),
            Self::BuiltIn(built_in_node) => built_in_node.check(checker),
        }
    }
}

impl Flowable for nodes::NameNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::IdentifierNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::ArrayLiteralNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        for element in &self.elements {
            element.check(checker)?;
        }
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::ArrayAccessNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.indices.check(checker)
    }
}

impl Flowable for nodes::BinaryNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        // FIXME: LHS and RHS might cause an exit
        self.lhs.check(checker)?;
        self.rhs.check(checker)?;
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::FieldAccessNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.field.check(checker)
    }
}

impl Flowable for nodes::CallNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        for arg in &self.arguments {
            arg.check(checker)?;
        }
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::BuiltInNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        // FIXME: Later on we might want to check if the builtin always exits
        //        for example, we could check if the builtin is `exit`
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::ExpressionNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.expression.check(checker)
    }
}
impl Flowable for nodes::ComparisonNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        // FIXME: LHS and RHS might cause an exit
        self.lhs.check(checker)?;
        self.rhs.check(checker)?;
        Ok(FlowType::Linear)
    }
}


impl Flowable for nodes::ArgumentNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.expression.check(checker)
    }
}

impl Flowable for nodes::LiteralNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::TypeNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }
}