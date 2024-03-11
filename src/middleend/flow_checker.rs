use tracer::trace_call;

use crate::{
    compiler::{ERR_STR, WARN_STR},
    frontend::nodes,
    util::flags::Flags
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

pub struct FlowChecker<'flags> {
    loop_stack: Vec<()>,
    #[allow(unused)]
    flags: &'flags Flags,
}

impl<'flags> FlowChecker<'flags> {
    pub fn new(flags: &'flags Flags) -> FlowChecker {
        FlowChecker {
            loop_stack: Vec::new(),
            flags,
        }
    }

    #[trace_call(always)]
    pub fn check_file(&mut self, file: &mut nodes::ModuleNode) -> Result<(), String> {
        for _import in &mut file.imports {
            // self.check_import(import)?;
        }
        for module in &mut file.modules {
            self.check_file(module)?;
        }
        for _ext in &mut file.externs {
            // self.check_extern(ext)?;
        }
        for strukt in &mut file.structs {
            self.check_struct(strukt)?;
        }
        for function in &mut file.functions {
            self.check_function(function)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_struct(&mut self, strukt: &mut nodes::StructNode) -> Result<(), String> {
        for method in &mut strukt.methods {
            self.check_method(method)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_method(&mut self, method: &mut nodes::MethodNode) -> Result<(), String> {
        let flow = self.check_block(&mut method.block, &[FlowType::AlwaysReturn])?;
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
    fn check_function(&mut self, function: &mut nodes::FunctionNode) -> Result<(), String> {
        let flow = self.check_block(&mut function.block, &[FlowType::AlwaysReturn])?;
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
    fn check_block(&mut self, block: &mut nodes::BlockNode, early_exit: &[FlowType]) -> Result<FlowType, String> {
        // println!("{:?}: check_block: {:?}", block.location, early_exit);
        let mut flow = FlowType::Linear;
        let mut exit_index = None;
        for index in 0..block.statements.len() {
            let statement = &mut block.statements[index];
            flow = self.check_statement(statement, early_exit)?;
            if early_exit.contains(&flow) {
                if index != block.statements.len() - 1 {
                    println!(
                        "{}: {:?}: Unreachable code",
                        WARN_STR,
                        block.statements[index + 1].get_loc()
                    );
                }
                exit_index = Some(index);
                break;
            }
        }
        #[cfg(not(feature = "old_codegen"))]
        if let Some(index) = exit_index {
            block.statements.truncate(index + 1);
            block.llvm_has_terminator = true;
        }
        #[cfg(feature = "old_codegen")]
        if let Some(index) = exit_index {
            block.statements.truncate(index + 1);
        }
        Ok(flow)
    }

    #[trace_call(always)]
    fn check_statement(&mut self, statement: &mut nodes::Statement, early_exit: &[FlowType]) -> Result<FlowType, String> {
        match statement {
            nodes::Statement::Expression(_expr_node) => Ok(FlowType::Linear),
            nodes::Statement::Block(block_node) => self.check_block(block_node, early_exit),
            nodes::Statement::VarDecl(var_node) => self.check_stmt_var_decl(var_node),
            nodes::Statement::If(if_node) => self.check_stmt_if(if_node, early_exit),
            nodes::Statement::Return(return_node) => self.check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.check_stmt_while(while_node),
            nodes::Statement::Break(break_node) => self.check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => self.check_stmt_continue(continue_node),
        }
    }

    #[trace_call(always)]
    fn check_stmt_var_decl(&mut self, _let_node: &nodes::VarDeclNode) -> Result<FlowType, String> {
        Ok(FlowType::Linear)
    }

    #[trace_call(always)]
    fn check_stmt_if(&mut self, if_node: &mut nodes::IfNode, early_exit: &[FlowType]) -> Result<FlowType, String> {
        // let cond_flow = self.check_expression_node(&if_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        let if_flow = self.check_block(&mut if_node.if_body, early_exit)?;
        if let Some(else_branch) = &mut if_node.else_body {
            let else_flow = self.check_block(else_branch, early_exit)?;
            if self.flags.debug {
                println!(
                    "[DEBUG] IfNode::check: if_flow: {:?}, else_flow: {:?}",
                    if_flow, else_flow
                );
            }
            match (if_flow, else_flow) {
                (FlowType::AlwaysReturn, FlowType::AlwaysReturn) => Ok(FlowType::AlwaysReturn),
                (FlowType::AlwaysContinue, FlowType::AlwaysContinue) => Ok(FlowType::AlwaysContinue),
                (FlowType::Linear, FlowType::Linear) => Ok(FlowType::Linear),
                (FlowType::AlwaysBreak, FlowType::AlwaysBreak) => Ok(FlowType::AlwaysBreak),
                (FlowType::AlwaysReturn, _) => Ok(FlowType::MayReturn),
                (_, FlowType::AlwaysReturn) => Ok(FlowType::MayReturn),
                (FlowType::MayReturn, _) => Ok(FlowType::MayReturn),
                (_, FlowType::MayReturn) => Ok(FlowType::MayReturn),
                (i, e) => todo!("{i:?}, {e:?}"),
            }
        } else {
            match if_flow {
                FlowType::AlwaysReturn | FlowType::MayReturn => Ok(FlowType::MayReturn),
                FlowType::AlwaysContinue | FlowType::MayContinue => Ok(FlowType::MayContinue),
                FlowType::AlwaysBreak | FlowType::MayBreak => Ok(FlowType::MayBreak),
                FlowType::Linear => Ok(FlowType::Linear),
                // i => todo!("{i:?}"),
            }
        }
    }

    #[trace_call(always)]
    fn check_stmt_return(&mut self, _return_node: &nodes::ReturnNode) -> Result<FlowType, String> {
        Ok(FlowType::AlwaysReturn)
    }

    #[trace_call(always)]
    fn check_stmt_while(&mut self, while_node: &mut nodes::WhileNode) -> Result<FlowType, String> {
        // let cond_flow = self.check_expression_node(&while_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        self.loop_stack.push(());
        let block_flow = self.check_block(
            &mut while_node.body,
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
