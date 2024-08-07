use std::{collections::HashMap, fmt::Display};

use tracer::{trace_call, trace_panic};

use crate::util::flags::Flags;
use crate::compiler::{ERR_STR, WARN_STR, NOTE_STR};
use crate::frontend::nodes;
use crate::frontend::tokens::KEYWORD_COMPTIME;
use crate::frontend::tokens::Location;
use crate::internal_panic;

use crate::middleend::type_checker::Type;

#[derive(Debug)]
enum FlowError<'src> {
    /// Flow kind, Error Loc
    BreakOrContinueOutsideLoop(&'static str, Location),
    /// Function Kind, Error Loc, Name
    DoesntAlwaysReturn(&'static str, Location, &'src str),
    /// Error Loc, Func Name, Decl Loc
    NormalFuncInComptime(Location, &'src str, Location),
    /// Error Loc, Func Name, Decl Loc
    ComptimeFuncInNormal(Location, &'src str, Location),
}

impl Display for FlowError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BreakOrContinueOutsideLoop(kind, loc)
                => write!(f, "{ERR_STR}: {loc}: {kind} statement outside of loop"),
            Self::DoesntAlwaysReturn(what, loc, fn_name)
                => write!(f, "{ERR_STR}: {loc}: {what} `{fn_name}` does not always return a value"),
            Self::NormalFuncInComptime(loc, name, decl)
                => write!(f, "{ERR_STR}: {loc}: Call to normal function in a {KEYWORD_COMPTIME} function.\n{NOTE_STR}: {decl}: Function `{name}` declared here."),
            Self::ComptimeFuncInNormal(loc, name, decl)
                => write!(f, "{ERR_STR}: {loc}: Call to {KEYWORD_COMPTIME} function in runtime context.\n{NOTE_STR}: {decl}: Function `{name}` declared to be {KEYWORD_COMPTIME} here.")
        }
    }
}

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

pub struct FlowChecker<'flags, 'src> {
    loop_stack: Vec<()>,
    flags: &'flags Flags,
    functions: HashMap<&'src str, (bool, Location)>,
    errors: Vec<FlowError<'src>>,
}

impl<'flags, 'src> FlowChecker<'flags, 'src> {
    pub fn new(flags: &'flags Flags) -> FlowChecker {
        FlowChecker {
            loop_stack: Vec::new(),
            flags,
            functions: HashMap::new(),
            errors: Vec::new()
        }
    }

    fn report_error(&mut self, error: FlowError<'src>) {
        if self.flags.debug {
            println!("[DEBUG] {error:?}")
        }
        trace_panic!();
        self.errors.push(error);
    }

    fn fill_lookup<'s>(&mut self, file: &'s nodes::FileNode<'src>) {
        for ext in &file.externs {
            self.functions.insert(&ext.name, (false, ext.location));
        }
        for func in &file.functions {
            self.functions.insert(&func.get_full_name(), (func.is_comptime, func.location));
        }
    }

    #[trace_call(always)]
    fn stringify_errors(&self) -> String {
        let mut errors = String::new();
        for e in &self.errors {
            errors.push_str(&format!("{}\n", e));
        }
        errors
    }
    #[trace_call(always)]
    pub fn check_project<'s>(&mut self, file: &'s mut nodes::FileNode<'src>) -> Result<(), String> {
        self.fill_lookup(file);
        let root = self.check_file(file);

        if !self.errors.is_empty() || root.is_err() {
            Err(self.stringify_errors())
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    pub fn check_file<'s>(&mut self, file: &'s mut nodes::FileNode<'src>) -> Result<(), ()> {
        self.fill_lookup(file);
        for _ext in &mut file.externs {
            // self.check_extern(ext)?;
        }
        for strukt in &mut file.structs {
            let _ = self.check_struct(strukt);
        }
        for function in &mut file.functions {
            // No ? because we want to gather all errors
            // A single flow error doesn't break anything.
            let _ = self.check_function(function);
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_struct(&mut self, strukt: &mut nodes::StructNode<'src>) -> Result<(), ()> {
        for method in &mut strukt.methods {
            // No ? because we want to gather all errors
            // A single flow error doesn't break anything.
            let _ = self.check_method(method);
        }
        Ok(())
    }

    #[trace_call(always)]
    fn check_method(&mut self, method: &mut nodes::MethodNode<'src>) -> Result<(), ()> {
        let flow = self.check_block(&mut method.block, &[FlowType::AlwaysReturn], false)?;
        if flow != FlowType::AlwaysReturn {
            if method.return_type.typ != Type::None {
                self.report_error(FlowError::DoesntAlwaysReturn(
                    "Method",
                    method.location,
                    method.name,
                ));
                Err(())
            } else {
                // Implicit return statement.
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    fn check_function(&mut self, function: &mut nodes::FunctionNode<'src>) -> Result<(), ()> {
        let flow = self.check_block(&mut function.block, &[FlowType::AlwaysReturn], function.is_comptime)?;
        if flow != FlowType::AlwaysReturn {
            if function.return_type.typ != Type::None {
                self.report_error(FlowError::DoesntAlwaysReturn(
                    "Function",
                    function.location,
                    function.name,
                ));
                Err(())
            } else {
                // Implicit return statement.
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    fn check_block(&mut self, block: &mut nodes::BlockNode<'src>, early_exit: &[FlowType], is_comptime: bool) -> Result<FlowType, ()> {
        // println!("{:?}: check_block: {:?}", block.location, early_exit);
        let mut flow = FlowType::Linear;
        let mut exit_index = None;
        for index in 0..block.statements.len() {
            let statement = &mut block.statements[index];
            if let Ok(f) = self.check_statement(statement, early_exit, is_comptime) {
                flow = f;
            };
            if early_exit.contains(&flow) {
                if index != block.statements.len() - 1 {
                    eprintln!(
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
    fn check_statement(&mut self, statement: &mut nodes::Statement<'src>, early_exit: &[FlowType], is_comptime: bool) -> Result<FlowType, ()> {
        match statement {
            nodes::Statement::Expression(expr_node) => self.check_expression_node(expr_node, is_comptime),
            nodes::Statement::Block(block_node) => self.check_block(block_node, early_exit, is_comptime),
            nodes::Statement::VarDecl(var_node) => self.check_stmt_var_decl(var_node, is_comptime),
            nodes::Statement::If(if_node) => self.check_stmt_if(if_node, early_exit, is_comptime),
            nodes::Statement::Return(return_node) => self.check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.check_stmt_while(while_node, is_comptime),
            nodes::Statement::Break(break_node) => self.check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => self.check_stmt_continue(continue_node),
        }
    }

    #[trace_call(always)]
    fn check_expression_node(&mut self, expr: &nodes::Expression<'src>, is_comptime: bool) -> Result<FlowType, ()> {
        if let nodes::Expression::FunctionCall(call) = expr {
            let Some(func) = self.functions.get(call.get_full_name()) else {
                internal_panic!("FlowChecker could not find function {}", call.get_full_name())
            };
            if is_comptime && !func.0 {
                self.report_error(FlowError::NormalFuncInComptime(
                    call.location,
                    call.get_full_name(),
                    func.1
                ));
                return Err(());
            } else if !is_comptime && func.0 {
                self.report_error(FlowError::ComptimeFuncInNormal(
                    call.location,
                    call.get_full_name(),
                    func.1
                ));
                return Err(());
            }
        }
        Ok(FlowType::Linear)
    }

    #[trace_call(always)]
    fn check_stmt_var_decl(&mut self, let_node: &nodes::VarDeclNode<'src>, is_comptime: bool) -> Result<FlowType, ()> {
        if is_comptime && let_node.is_comptime {
            eprintln!(
                "{WARN_STR}: {}: The {} specifier for variables has no use in {} functions.",
                let_node.location,
                KEYWORD_COMPTIME,
                KEYWORD_COMPTIME,
            );
        }
        if !is_comptime && !let_node.is_comptime {
            let _ = self.check_expression_node(&let_node.expression, false);
        }
        Ok(FlowType::Linear)
    }

    #[trace_call(always)]
    fn check_stmt_if(&mut self, if_node: &mut nodes::IfNode<'src>, early_exit: &[FlowType], is_comptime: bool) -> Result<FlowType, ()> {
        // let cond_flow = self.check_expression_node(&if_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        let if_flow = self.check_block(&mut if_node.if_body, early_exit, is_comptime)?;
        if let Some(else_branch) = &mut if_node.else_body {
            let else_flow = self.check_block(else_branch, early_exit, is_comptime)?;
            if self.flags.debug {
                println!(
                    "[DEBUG] IfNode::check: if_flow: {:?}, else_flow: {:?}",
                    if_flow, else_flow
                );
            }
            match (if_flow, else_flow) {
                (FlowType::AlwaysReturn, FlowType::AlwaysReturn) => Ok(FlowType::AlwaysReturn),
                (FlowType::AlwaysContinue, FlowType::AlwaysContinue) => Ok(FlowType::AlwaysContinue),
                (FlowType::AlwaysBreak, FlowType::AlwaysBreak) => Ok(FlowType::AlwaysBreak),
                (FlowType::Linear, FlowType::Linear) => Ok(FlowType::Linear),
                (FlowType::AlwaysReturn, _) => Ok(FlowType::MayReturn),
                (_, FlowType::AlwaysReturn) => Ok(FlowType::MayReturn),
                (FlowType::AlwaysContinue, _) => Ok(FlowType::MayContinue),
                (_, FlowType::AlwaysContinue) => Ok(FlowType::MayContinue),
                (FlowType::AlwaysBreak, _) => Ok(FlowType::MayBreak),
                (_, FlowType::AlwaysBreak) => Ok(FlowType::MayBreak),
                (FlowType::MayReturn, _) => Ok(FlowType::MayReturn),
                (_, FlowType::MayReturn) => Ok(FlowType::MayReturn),
                (FlowType::MayContinue, _) => Ok(FlowType::MayContinue),
                (_, FlowType::MayContinue) => Ok(FlowType::MayContinue),
                (FlowType::MayBreak, _) => Ok(FlowType::MayBreak),
                (_, FlowType::MayBreak) => Ok(FlowType::MayBreak),
            }
        } else {
            match if_flow {
                FlowType::AlwaysReturn | FlowType::MayReturn => Ok(FlowType::MayReturn),
                FlowType::AlwaysContinue | FlowType::MayContinue => Ok(FlowType::MayContinue),
                FlowType::AlwaysBreak | FlowType::MayBreak => Ok(FlowType::MayBreak),
                FlowType::Linear => Ok(FlowType::Linear),
            }
        }
    }

    #[trace_call(always)]
    fn check_stmt_return(&mut self, _return_node: &nodes::ReturnNode) -> Result<FlowType, ()> {
        Ok(FlowType::AlwaysReturn)
    }

    #[trace_call(always)]
    fn check_stmt_while(&mut self, while_node: &mut nodes::WhileNode<'src>, is_comptime: bool) -> Result<FlowType, ()> {
        // let cond_flow = self.check_expression_node(&while_node.condition)?;
        // debug_assert!(cond_flow == FlowType::Linear);
        // Later on we might want to check if the condition always exits
        self.loop_stack.push(());
        let block_flow = self.check_block(
            &mut while_node.body,
            &[FlowType::AlwaysReturn, FlowType::AlwaysBreak, FlowType::AlwaysContinue],
            is_comptime
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
    fn check_stmt_break(&mut self, break_node: &nodes::BreakNode) -> Result<FlowType, ()> {
        if self.loop_stack.is_empty() {
            self.report_error(FlowError::BreakOrContinueOutsideLoop("break", break_node.location));
            Err(())
            // Err(format!(
            //     "{}: {:?}: Break statement outside of loop",
            //     ERR_STR, break_node.location
            // ))
        } else {
            Ok(FlowType::AlwaysBreak)
        }
    }

    #[trace_call(always)]
    fn check_stmt_continue(
        &mut self,
        continue_node: &nodes::ContinueNode,
    ) -> Result<FlowType, ()> {
        if self.loop_stack.is_empty() {
            self.report_error(FlowError::BreakOrContinueOutsideLoop("continue", continue_node.location));
            Err(())
        } else {
            Ok(FlowType::AlwaysContinue)
        }
    }
}
