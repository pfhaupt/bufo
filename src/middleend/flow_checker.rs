use tracer::trace_call;

use crate::compiler::NOTE_STR;
use crate::{
    compiler::{ERR_STR, WARN_STR},
    frontend::{flags::Flags, nodes},
    internal_error, internal_warning,
};

use super::type_checker::Type;

#[derive(Debug, Copy, Clone, PartialEq)]
#[allow(unused)]
enum FlowType {
    Linear,
    MayReturn,
    AlwaysReturn,
    MayBreak,
    AlwaysBreak,
    MayContinue,
    AlwaysContinue
}

pub struct FlowChecker {
    loop_stack: Vec<()>,
    #[allow(unused)]
    flags: Flags,
}

impl FlowChecker {
    pub fn new(flags: Flags) -> FlowChecker {
        FlowChecker {
            loop_stack: Vec::new(),
            flags,
        }
    }

    #[trace_call(always)]
    pub fn check_file(&mut self, file: &nodes::FileNode) -> Result<(), String> {
        for class in &file.classes {
            self.check_class(class)?;
        }
        for function in &file.functions {
            self.check_function(function)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_class(&mut self, class: &nodes::ClassNode) -> Result<(), String> {
        for feature in &class.features {
            self.check_feature(feature)?;
        }
        for method in &class.methods {
            self.check_method(method)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_feature(&mut self, feature: &nodes::FeatureNode) -> Result<(), String> {
        let flow = self.check_block(&feature.block, &[FlowType::AlwaysReturn])?;
        if feature.is_constructor {
            // Constructors never return a value
            // It's all implicit at the end of the block
            if flow != FlowType::Linear {
                Err(format!(
                    "{}: {:?}: Return values are not allowed in constructors.\n{}: Returning `this` is implicit in constructors.",
                    ERR_STR, feature.location, NOTE_STR
                ))
            } else {
                // Implicit return statement.
                Ok(())
            }
        } else {
            internal_error!("There's only one feature now, and it's a constructor")
        }
    }

    #[trace_call(always)]
    fn check_method(&mut self, method: &nodes::MethodNode) -> Result<(), String> {
        let flow = self.check_block(&method.block, &[FlowType::AlwaysReturn])?;
        if flow != FlowType::AlwaysReturn {
            if method.return_type.typ != Type::None {
                Err(format!(
                    "{}: {:?}: Method `{}` does not always return a value",
                    ERR_STR, method.location, method.name
                ))
            } else {
                // Implicit return statement.
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    fn check_function(&mut self, function: &nodes::FunctionNode) -> Result<(), String> {
        let flow = self.check_block(&function.block, &[FlowType::AlwaysReturn])?;
        if flow != FlowType::AlwaysReturn {
            if function.return_type.typ != Type::None {
                Err(format!(
                    "{}: {:?}: Function `{}` does not always return a value",
                    ERR_STR, function.location, function.name
                ))
            } else {
                // Implicit return statement.
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    fn check_block(&mut self, block: &nodes::BlockNode, early_exit: &[FlowType]) -> Result<FlowType, String> {
        // println!("{:?}: check_block: {:?}", block.location, early_exit);
        let mut flow = FlowType::Linear;
        for index in 0..block.statements.len() {
            let statement = &block.statements[index];
            flow = self.check_statement(statement, early_exit)?;
            if early_exit.contains(&flow) {
                if index != block.statements.len() - 1 {
                    println!(
                        "{}: {:?}: Unreachable code",
                        WARN_STR,
                        block.statements[index + 1].get_loc()
                    );
                }
                break;
            }
        }
        Ok(flow)
    }

    #[trace_call(always)]
    fn check_statement(&mut self, statement: &nodes::Statement, early_exit: &[FlowType]) -> Result<FlowType, String> {
        match statement {
            nodes::Statement::Expression(_expr_node) => {
                internal_warning!(
                    "We're not checking expressions for control flow yet, assuming linear flow"
                );
                Ok(FlowType::Linear)
            }
            nodes::Statement::Let(let_node) => self.check_stmt_let(let_node),
            nodes::Statement::Assign(assign_node) => self.check_stmt_assign(assign_node),
            nodes::Statement::If(if_node) => self.check_stmt_if(if_node, early_exit),
            nodes::Statement::Return(return_node) => self.check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.check_stmt_while(while_node),
            nodes::Statement::Break(break_node) => self.check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => self.check_stmt_continue(continue_node),
        }
    }

    #[trace_call(always)]
    fn check_stmt_let(&mut self, _let_node: &nodes::LetNode) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }

    #[trace_call(always)]
    fn check_stmt_assign(&mut self, _assign_node: &nodes::AssignNode) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }

    #[trace_call(always)]
    fn check_stmt_if(&mut self, if_node: &nodes::IfNode, early_exit: &[FlowType]) -> Result<FlowType, String> {
        // let cond_flow = self.check_expression_node(&if_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        let if_flow = self.check_block(&if_node.if_branch, early_exit)?;
        if let Some(else_branch) = &if_node.else_branch {
            let else_flow = self.check_block(else_branch, early_exit)?;
            if self.flags.debug {
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

    #[trace_call(always)]
    fn check_stmt_return(&mut self, _return_node: &nodes::ReturnNode) -> Result<FlowType, String> {
        Ok(FlowType::AlwaysReturn)
    }

    #[trace_call(always)]
    fn check_stmt_while(&mut self, while_node: &nodes::WhileNode) -> Result<FlowType, String> {
        // let cond_flow = self.check_expression_node(&while_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        self.loop_stack.push(());
        let block_flow = self.check_block(
            &while_node.block,
            &[FlowType::AlwaysReturn, FlowType::AlwaysBreak, FlowType::AlwaysContinue]
        )?;
        self.loop_stack.pop();
        if self.flags.debug {
            println!("[DEBUG] WhileNode::check: block_flow: {:?}", block_flow);
        }
        if block_flow == FlowType::AlwaysReturn {
            Ok(FlowType::MayReturn)
        } else {
            Ok(block_flow)
        }
    }

    #[trace_call(always)]
    fn check_stmt_break(&mut self, break_node: &nodes::BreakNode) -> Result<FlowType, String> {
        if self.loop_stack.is_empty() {
            Err(format!(
                "{}: {:?}: Break statement outside of loop",
                ERR_STR, break_node.location
            ))
        } else {
            Ok(FlowType::AlwaysBreak)
        }
    }

    #[trace_call(always)]
    fn check_stmt_continue(
        &mut self,
        continue_node: &nodes::ContinueNode,
    ) -> Result<FlowType, String> {
        if self.loop_stack.is_empty() {
            Err(format!(
                "{}: {:?}: Continue statement outside of loop",
                ERR_STR, continue_node.location
            ))
        } else {
            Ok(FlowType::AlwaysContinue)
        }
    }
}
