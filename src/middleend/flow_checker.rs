use tracer::trace_call;

use crate::{
    compiler::{ERR_STR, WARN_STR},
    frontend::{flags::Flags, nodes},
    internal_error,
};

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
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        internal_error!("ClassNode::check not implemented")
    }
}
impl Flowable for nodes::MethodNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        internal_error!("MethodNode::check not implemented")
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
            Self::Let(let_node) => let_node.check(checker),
            Self::If(if_node) => if_node.check(checker),
            Self::Return(return_node) => return_node.check(checker),
            Self::While(while_node) => while_node.check(checker),
            _ => todo!(),
        }
    }
}

impl Flowable for nodes::LetNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.expression.check(checker)
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
                _ => todo!(),
            }
        } else {
            match if_flow {
                FlowType::AlwaysReturn => Ok(FlowType::MayReturn),
                FlowType::Linear => Ok(FlowType::Linear),
                _ => todo!(),
            }
        }
    }
}

impl Flowable for nodes::ReturnNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        if let Some(_) = &self.return_value {
            // TODO: Value might cause an exit
            Ok(FlowType::AlwaysReturn)
        } else {
            Ok(FlowType::AlwaysReturn)
        }
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
        Ok(block_flow)
    }
}

impl Flowable for nodes::Expression {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        match self {
            Self::Name(name_node) => name_node.check(checker),
            Self::Literal(literal_node) => literal_node.check(checker),
            e => todo!("{:#?}", e),
        }
    }
}

impl Flowable for nodes::ExpressionNode {
    #[trace_call(always)]
    fn check(&self, checker: &FlowChecker) -> Result<FlowType, String> {
        self.expression.check(checker)
    }
}
impl Flowable for nodes::ExpressionComparisonNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        // TODO: LHS and RHS might cause an exit
        Ok(FlowType::Linear)
    }
}

impl Flowable for nodes::NameNode {
    #[trace_call(always)]
    fn check(&self, _checker: &FlowChecker) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }
}
impl Flowable for nodes::ExpressionLiteralNode {
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
