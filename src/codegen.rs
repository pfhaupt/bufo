#![allow(unused)]

use std::collections::{BTreeMap, HashMap, VecDeque};

use crate::lexer::TokenType;
use crate::parser::{Tree, TreeType};

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";

macro_rules! ref_unwrap {
    ($str:expr) => {
        $str.as_ref().unwrap()
    };
    ($str:expr, $msg:expr) => {
        $str.as_ref().expect($msg)
    };
}

#[derive(Debug)]
enum Instruction {
    Load { dest: usize, val: usize },
    Add { dest: usize, src: usize },
    Sub { dest: usize, src: usize },
    Mul { dest: usize, src: usize },
    Div { dest: usize, src: usize },
    Cmp { dest: usize, src: usize },
    Jmp { dest: usize },
    JmpEq { dest: usize },
    JmpNeq { dest: usize },
    JmpGt { dest: usize },
    JmpGte { dest: usize },
    JmpLt { dest: usize },
    JmpLte { dest: usize },
    Move { dest: usize, src: usize },
    LoadMem { mem: usize, reg: usize },
    StoreMem { mem: usize, reg: usize },
    Call { fn_name: String },
    PushRet { reg: usize },
    PopRet { reg: usize },
    Return {},
    // Print { src: usize },
}

#[derive(Debug)]
struct Function {
    ip: usize,
    param_variables: BTreeMap<String, usize>,
    local_variables: Vec<BTreeMap<String, usize>>,
    stack_size: usize,
}

impl Function {
    fn new(ip: usize) -> Self {
        Self {
            ip,
            param_variables: BTreeMap::new(),
            local_variables: vec![BTreeMap::new()],
            stack_size: 0,
        }
    }

    fn get_ip(&self) -> usize {
        self.ip
    }

    fn get_stack_size(&self) -> usize {
        self.stack_size
    }

    fn get_params(&self) -> &BTreeMap<String, usize> {
        &self.param_variables
    }

    fn get_param_location(&self, var_name: &String) -> Option<usize> {
        self.param_variables.get(var_name).copied()
    }

    fn get_local_location(&self, var_name: &String) -> Option<usize> {
        for scope in (0..self.local_variables.len()).rev() {
            if let Some(mem) = self.local_variables.get(scope).unwrap().get(var_name) {
                return Some(*mem);
            }
        }
        None
    }

    fn get_scope_location(&self, var_name: &String) -> Option<usize> {
        self.local_variables.last().unwrap().get(var_name).copied()
    }

    fn get_variable_location(&self, var_name: &String) -> Option<usize> {
        match self.get_param_location(var_name) {
            Some(i) => Some(i),
            None => self.get_local_location(var_name),
        }
    }

    fn add_local_variable(&mut self, var_name: String, mem: usize, scope_depth: usize) {
        let mut scope_var = self.local_variables.get_mut(scope_depth).unwrap();
        scope_var.insert(var_name, mem);
        self.stack_size += 1;
        // self.local_variables.insert(var_name, mem);
    }

    fn add_param(&mut self, param_name: &str) -> usize {
        let mem = self.get_stack_size();
        self.param_variables.insert(param_name.to_owned(), mem);
        self.stack_size += 1;
        mem
    }

    fn add_scope(&mut self) {
        self.local_variables.push(BTreeMap::new());
    }

    fn remove_scope(&mut self) {
        self.local_variables.pop().unwrap();
    }
}

#[derive(Debug)]
pub struct Generator {
    ast: Tree,
    registers: Vec<usize>,
    register_ctr: usize,
    functions: HashMap<String, Function>,
    code: Vec<Instruction>,
    current_fn: String,
    unresolved_jmp_instr: VecDeque<usize>,
    scope_depth: usize,
}

impl Generator {
    pub fn new(ast: Tree) -> Result<Self, String> {
        let mut gen = Self {
            ast,
            registers: vec![0; 100],
            register_ctr: 0,
            functions: HashMap::new(),
            code: vec![],
            current_fn: String::new(),
            unresolved_jmp_instr: VecDeque::new(),
            scope_depth: 0,
        };
        gen.generate_code()?;
        Ok(gen)
    }

    fn add_local_var(&mut self, var_name: &str, scope_depth: usize) -> usize {
        assert!(!self.current_fn.is_empty());
        assert!(self.functions.contains_key(&self.current_fn));
        let mem = self.get_stack_offset();
        self.functions
            .get_mut(&self.current_fn)
            .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
            .add_local_variable(var_name.to_owned(), mem, scope_depth);
        mem
    }

    fn get_stack_offset(&self) -> usize {
        assert!(!self.current_fn.is_empty());
        self.functions
            .get(&self.current_fn)
            .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
            .get_stack_size()
    }

    fn get_function_stack_size(&self, func_name: &String) -> usize {
        assert!(self.functions.contains_key(func_name));
        self.functions
            .get(func_name)
            .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
            .get_stack_size()
    }

    fn reset_registers(&mut self) {
        // self.register_ctr = 0;
    }

    fn get_register(&mut self) -> usize {
        let r = self.register_ctr;
        if r >= self.registers.len() {
            self.registers.push(0);
        }
        self.register_ctr += 1;
        r
    }

    fn convert_expr_atomic(&mut self, instr: &Tree) -> Result<usize, String> {
        let instr_children = &instr.children;
        match ref_unwrap!(instr.typ, "Atomic Expression Head can't be None.") {
            TreeType::ExprLiteral => {
                assert!(instr_children.len() == 1, "Expected {{Literal}}");
                let val = ref_unwrap!(
                    instr_children[0].tkn,
                    "Expected valid ExprLiteral, got None instead. This might be a bug in parsing."
                )
                .get_value()
                .parse()
                .expect("At this point value of ExprLiteral should only contain valid digits.");
                let dest = self.get_register();
                self.code.push(Instruction::Load { dest, val });
                Ok(dest)
            }
            TreeType::ExprParen => {
                assert!(instr_children.len() == 1, "Expected {{Expr}}");
                self.convert_expr_atomic(&instr_children[0])
            }
            TreeType::ExprBinary => {
                assert!(*ref_unwrap!(instr.typ) == TreeType::ExprBinary);
                assert!(instr_children.len() == 3, "Expected {{Expr}} {{Op}} {{Expr}}");
                let lhs = &instr_children[0];
                let op = &instr_children[1];
                let rhs = &instr_children[2];
                let dest = self.convert_expr_atomic(lhs)?;
                let src = self.convert_expr_atomic(rhs)?;
                match ref_unwrap!(op.tkn, "Expected valid ExprBinary operator, got None instead. This might be a bug in parsing.").get_type() {
                    TokenType::Plus => self.code.push(Instruction::Add { dest, src }),
                    TokenType::Minus => self.code.push(Instruction::Sub { dest, src }),
                    TokenType::Mult => self.code.push(Instruction::Mul { dest, src }),
                    TokenType::Div => self.code.push(Instruction::Div { dest, src }),
                    TokenType::CmpEq => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpNeq { dest: usize::MAX });
                    },
                    TokenType::CmpNeq => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpEq { dest: usize::MAX });
                    },
                    TokenType::CmpGt => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpLte { dest: usize::MAX });
                    },
                    TokenType::CmpGte => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpLt { dest: usize::MAX });
                    },
                    TokenType::CmpLt => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpGte { dest: usize::MAX });
                    },
                    TokenType::CmpLte => {
                        self.code.push(Instruction::Cmp { dest, src });
                        self.unresolved_jmp_instr.push_back(self.code.len());
                        self.code.push(Instruction::JmpGt { dest: usize::MAX });
                    },
                    e => todo!("Handle {:?} in convert_expr_atomic()", e)
                }
                Ok(dest)
            }
            TreeType::ExprName => {
                assert!(instr_children.len() == 1, "Expected {{name}}");
                let val_name = &instr_children[0];
                let name = ref_unwrap!(
                    val_name.tkn,
                    "Expected valid ExprName, got None instead. This might be a bug in parsing"
                )
                .get_value();

                let reg = self.get_register();
                let curr_fn = self
                    .functions
                    .get(&self.current_fn)
                    .expect("At this point, function table is guaranteed to contain current_fn.");
                match curr_fn.get_variable_location(&name) {
                    Some(mem) => {
                        self.code.push(Instruction::LoadMem { reg, mem });
                        Ok(reg)
                    }
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`",
                        ERR_STR,
                        ref_unwrap!(val_name.tkn).get_loc(),
                        name
                    )),
                }
            }
            TreeType::ExprCall => {
                assert!(instr_children.len() == 2, "Expected {{name}} {{ArgList}}");
                let fn_instr = &instr_children[0];
                assert!(ref_unwrap!(fn_instr.tkn).get_type() == TokenType::Name);
                let fn_name = ref_unwrap!(fn_instr.tkn).get_value();
                println!("{}", fn_name);
                let fn_args = &instr_children[1];
                if let Some(func) = self.functions.get(&fn_name) {
                    let params = func.get_params();
                    if fn_args.children.len() != params.len() {
                        todo!()
                    }
                    let mut reg_ctr = 50;
                    for arg in &fn_args.children {
                        let reg = self.convert_arg(arg)?;
                        self.code.push(Instruction::Move { dest: reg_ctr, src: reg });
                        reg_ctr += 1;
                    }
                    self.code.push(Instruction::Call { fn_name });
                    let reg = self.get_register();
                    self.code.push(Instruction::PopRet { reg });
                    Ok( reg )
                } else {
                    Err(format!(
                        "{} {:?}: Unknown function `{}`",
                        ERR_STR,
                        ref_unwrap!(fn_instr.tkn).get_loc(),
                        fn_name
                    ))
                }
            }
            e => todo!("Handle {:?} in convert_expr_atomic. Got:\n{:?}", e, instr),
        }
    }

    fn convert_expr(&mut self, expr_tree: &Tree) -> Result<usize, String> {
        let r = match &expr_tree.typ {
            Some(t) => match t {
                TreeType::ExprLiteral
                | TreeType::ExprBinary
                | TreeType::ExprName
                | TreeType::ExprParen
                | TreeType::ExprCall => self.convert_expr_atomic(expr_tree),
                e => todo!("Handle {:?} in convert_expr", e),
            },
            e => todo!("Handle {:?} in convert_expr", e),
        };
        self.reset_registers();
        r
    }

    fn convert_stmt_let(&mut self, let_tree: &Tree) -> Result<(), String> {
        let instr_children = &let_tree.children;
        assert!(instr_children.len() == 4);
        let let_keyword = &instr_children[0];
        assert!(ref_unwrap!(let_keyword.tkn).get_type() == TokenType::LetKeyword);
        let let_name = &instr_children[1];
        assert!(ref_unwrap!(let_name.tkn).get_type() == TokenType::Name);
        let let_eq = &instr_children[2];
        assert!(ref_unwrap!(let_eq.tkn).get_type() == TokenType::Equal);
        let let_expr = &instr_children[3];

        let let_name = ref_unwrap!(
            let_name.tkn,
            "Expected Some Function Name Token, got None instead. This might be a bug in parsing."
        );
        let local_lookup = self
            .functions
            .get(&self.current_fn)
            .expect("At this point, function table is guaranteed to contain current_fn.");
        if local_lookup
            .get_scope_location(&let_name.get_value())
            .is_some()
        {
            return Err(format!(
                "{}: {:?}: Variable redefinition",
                ERR_STR,
                let_name.get_loc()
            ));
        }

        let mem = self.add_local_var(&let_name.get_value(), self.scope_depth);
        let reg = self.convert_expr(let_expr)?;
        self.code.push(Instruction::StoreMem { reg, mem });
        Ok(())
    }

    fn convert_stmt_assign(&mut self, assign_tree: &Tree) -> Result<(), String> {
        let instr_children = &assign_tree.children;
        assert!(instr_children.len() == 3);
        let assign_name = &instr_children[0];
        assert!(ref_unwrap!(assign_name.tkn).get_type() == TokenType::Name);
        let var_name = ref_unwrap!(assign_name.tkn).get_value();
        let local_lookup = self
            .functions
            .get(&self.current_fn)
            .expect("At this point, function table is guaranteed to contain current_fn.");
        let mem = match local_lookup.get_variable_location(&var_name) {
            Some(i) => i,
            None => {
                return Err(format!(
                    "{}: {:?}: Unknown variable `{}`",
                    ERR_STR,
                    ref_unwrap!(assign_name.tkn).get_loc(),
                    var_name
                ))
            }
        };
        let assign_eq = &instr_children[1];
        assert!(ref_unwrap!(assign_eq.tkn).get_type() == TokenType::Equal);
        let assign_expr = &instr_children[2];
        let reg = self.convert_expr(assign_expr)?;
        self.code.push(Instruction::StoreMem { mem, reg });
        Ok(())
    }

    fn convert_arg(&mut self, arg: &Tree) -> Result<usize, String> {
        assert!(*ref_unwrap!(arg.typ) == TreeType::Arg);
        let arg_children = &arg.children;
        assert!(arg_children.len() == 1);
        let arg_expr = &arg_children[0];
        self.convert_expr(arg_expr)
    }

    fn convert_stmt_if(&mut self, if_tree: &Tree) -> Result<(), String> {
        let if_children = &if_tree.children;
        let if_keyword = &if_children[0];
        assert!(ref_unwrap!(if_keyword.tkn).get_type() == TokenType::IfKeyword);
        let if_cond = &if_children[1];
        assert!(*ref_unwrap!(if_cond.typ) == TreeType::ExprBinary);
        let if_block = &if_children[2];
        self.convert_expr(if_cond)?;

        self.convert_block(if_block)?;
        let if_jmp = self.resolve_last_jmp();

        if if_children.len() == 5 {
            let else_keyword = &if_children[3];
            assert!(ref_unwrap!(else_keyword.tkn).get_type() == TokenType::ElseKeyword);
            let else_block = &if_children[4];
            assert!(*ref_unwrap!(else_block.typ) == TreeType::Block);

            self.unresolved_jmp_instr.push_back(self.code.len());
            self.code.push(Instruction::Jmp { dest: usize::MAX });
            self.set_jmp_lbl(if_jmp, self.code.len());

            self.convert_block(else_block)?;
            self.resolve_last_jmp();
        }
        Ok(())
    }

    fn convert_stmt_return(&mut self, ret_tree: &Tree) -> Result<(), String> {
        let child_count = ret_tree.children.len();
        assert!(&[1,2].contains(&child_count));
        if child_count == 2 {
            let reg = self.convert_expr(&ret_tree.children[1])?;
            // Assertion for two reasons:
            // 1: Register 0 is always the result for the outermost expr
            // 2: Calling conventions -> Reg 0 (RAX/EAX/...) always contains return value
            // assert!(reg == 0);
            self.code.push(Instruction::PushRet { reg });
        }
        self.code.push(Instruction::Return {} );
        Ok(())
    }

    fn convert_stmt_expr(&mut self, expr_tree: &Tree) -> Result<(), String> {
        let expr_children = &expr_tree.children;
        assert!(expr_children.len() == 1, "Expected {{Expr}}");
        let expr = &expr_children[0];
        self.convert_expr(expr)?;
        Ok(())
    }

    fn resolve_last_jmp(&mut self) -> usize {
        assert!(!self.unresolved_jmp_instr.is_empty());
        let ip = self.unresolved_jmp_instr.pop_back().unwrap();
        self.set_jmp_lbl(ip, self.code.len());
        ip
    }

    fn set_jmp_lbl(&mut self, index: usize, dest: usize) {
        self.code[index] = match self.code[index] {
            Instruction::JmpEq { .. } => Instruction::JmpEq { dest },
            Instruction::JmpNeq { .. } => Instruction::JmpNeq { dest },
            Instruction::JmpGt { .. } => Instruction::JmpGt { dest },
            Instruction::JmpGte { .. } => Instruction::JmpGte { dest },
            Instruction::JmpLt { .. } => Instruction::JmpLt { dest },
            Instruction::JmpLte { .. } => Instruction::JmpLte { dest },
            Instruction::Jmp { .. } => Instruction::Jmp { dest },
            _ => todo!(),
        };
    }

    fn convert_fn(&mut self, func: &Tree) -> Result<(), String> {
        let fn_children = &func.children;
        assert!(
            fn_children.len() == 4,
            "fnKeyword fnName {{Param}} {{Block}} expected."
        );
        let fn_keyword = ref_unwrap!(fn_children[0].tkn);
        assert!(fn_keyword.get_type() == TokenType::FnKeyword);
        let fn_name = ref_unwrap!(fn_children[1].tkn);
        assert!(fn_name.get_type() == TokenType::Name);
        if self.functions.contains_key(&fn_name.get_value()) {
            return Err(format!(
                "{}: {:?}: Function redefinition",
                ERR_STR,
                fn_name.get_loc()
            ));
        }
        let name = fn_name.get_value();

        self.current_fn = name.clone();
        let f = Function::new(self.code.len());
        self.functions.insert(name.clone(), f);
        self.convert_fn_param(&fn_children[2])?;
        self.convert_block(&fn_children[3])?;
        self.current_fn.clear();
        self.code.push(Instruction::Return {});

        Ok(())
    }

    fn convert_fn_param(&mut self, param: &Tree) -> Result<(), String> {
        let param_children = &param.children;
        let mut reg_ctr = 50;
        for p in param_children {
            match &p.typ {
                Some(t) => {
                    match t {
                        TreeType::Param => {
                            assert!(p.children.len() == 1);
                            let param_node = &p.children[0];
                            let param_name = ref_unwrap!(param_node.tkn).get_value();
                            let local_lookup = self.functions.get_mut(&self.current_fn).expect("At this point, function table is guaranteed to contain current_fn.");
                            if local_lookup.get_param_location(&param_name).is_some() {
                                return Err(format!(
                                    "{}: {:?}: Parameter redefinition `{}`",
                                    ERR_STR,
                                    ref_unwrap!(param_node.tkn).get_loc(),
                                    param_name
                                ));
                            }
                            let mem = local_lookup.add_param(&param_name);
                            self.code.push(Instruction::StoreMem { mem, reg: reg_ctr });
                            reg_ctr += 1;
                        }
                        _ => {
                            todo!(
                                "Handle t not TreeType::Param in convert_fn_param() - Got: {:?}",
                                t
                            )
                        }
                    }
                }
                None => panic!(),
            }
        }
        Ok(())
    }

    fn convert_block(&mut self, block: &Tree) -> Result<(), String> {
        assert!(*ref_unwrap!(block.typ) == TreeType::Block);
        self.enter_scope();
        for instr in &block.children {
            match &instr.typ {
                Some(t) => match t {
                    TreeType::StmtLet => self.convert_stmt_let(instr)?,
                    TreeType::StmtAssign => self.convert_stmt_assign(instr)?,
                    TreeType::StmtIf => self.convert_stmt_if(instr)?,
                    TreeType::StmtReturn => self.convert_stmt_return(instr)?,
                    TreeType::StmtExpr => self.convert_stmt_expr(instr)?,
                    e => todo!("convert {:?} inside block", e),
                },
                _ => panic!(),
            }
        }
        self.leave_scope();
        Ok(())
    }

    fn enter_scope(&mut self) {
        assert!(!self.current_fn.is_empty());
        // println!("Entering scope");
        self.scope_depth += 1;
        self.functions
            .get_mut(&self.current_fn)
            .unwrap()
            .add_scope();
    }

    fn leave_scope(&mut self) {
        assert!(!self.current_fn.is_empty());
        // println!("Leaving scope");
        self.scope_depth -= 1;
        self.functions
            .get_mut(&self.current_fn)
            .unwrap()
            .remove_scope();
    }

    fn convert_ast(&mut self, ast: &Tree) -> Result<(), String> {
        match &ast.typ {
            Some(typ) => match typ {
                TreeType::File => {
                    for func in &ast.children {
                        self.convert_fn(func)?;
                    }
                }
                _ => todo!("handle other types"),
            },
            None => {
                panic!()
            }
        }
        Ok(())
    }

    // fn add_instruction(&mut self, instr: Instruction) {
    //     self.code.push(instr);
    // }

    fn generate_code(&mut self) -> Result<(), String> {
        self.convert_ast(&self.ast.clone())
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        // for (i, c) in self.code.iter().enumerate() {
        //     println!("{i:3} -> {c:?}");
        // }
        // todo!();
        const RETURN_STACK_LIMIT: usize = 4096;
        const STACK_SIZE: usize = 1_000_000;
        let entry_point = String::from("main");

        let mut return_stack = VecDeque::<usize>::new();
        let mut return_values = VecDeque::<usize>::new();
        let mut stack = vec![0; STACK_SIZE];
        let mut stack_ptr = STACK_SIZE - 1 - self.get_function_stack_size(&entry_point);

        let mut flags = 0;
        const EQ: usize = 1;
        const LT: usize = 2;
        const GT: usize = 4;

        if !self.functions.contains_key(&entry_point) {
            return Err(format!(
                "{}: Missing entry point - Could not find function {}()",
                ERR_STR, entry_point
            ));
        }

        let mut ip = self
            .functions
            .get(&entry_point)
            .unwrap() // Can safely unwrap because map is guaranteed to contain entry_point
            .get_ip();

        while ip < self.code.len() {
            if return_stack.len() > RETURN_STACK_LIMIT {
                return Err(format!(
                    "{}: Recursion Limit reached when interpreting!",
                    ERR_STR
                ));
            }
            let instr = &self.code[ip];
            println!("{:3} {:?} {:?}", ip, flags, instr);
            let mut add_ip = true;
            match instr {
                Instruction::Add { dest, src } => {
                    self.registers[*dest] += self.registers[*src];
                }
                Instruction::Sub { dest, src } => {
                    self.registers[*dest] -= self.registers[*src];
                }
                Instruction::Mul { dest, src } => {
                    self.registers[*dest] *= self.registers[*src];
                }
                Instruction::Div { dest, src } => {
                    self.registers[*dest] /= self.registers[*src];
                }
                Instruction::Cmp { dest, src } => {
                    let lhs = self.registers[*dest];
                    let rhs = self.registers[*src];
                    flags = 0;
                    flags |= (lhs == rhs) as usize * EQ;
                    flags |= (lhs < rhs) as usize * LT;
                    flags |= (lhs > rhs) as usize * GT;
                    // >= is flags & EQ || flags & GT
                    // <= is flags & EQ || flags & LT
                    // != is not flags & EQ
                }
                Instruction::JmpEq { dest } => {
                    if flags & EQ != 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::JmpNeq { dest } => {
                    if flags & EQ == 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::JmpGt { dest } => {
                    if flags & GT != 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::JmpGte { dest } => {
                    if flags & EQ != 0 || flags & GT != 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::JmpLt { dest } => {
                    if flags & LT != 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::JmpLte { dest } => {
                    if flags & EQ != 0 || flags & LT != 0 {
                        ip = *dest;
                        add_ip = false;
                    }
                }
                Instruction::Jmp { dest } => {
                    ip = *dest;
                    add_ip = false;
                }
                Instruction::Move { dest, src } => {
                    self.registers[*dest] = self.registers[*src];
                }
                Instruction::LoadMem { reg, mem } => {
                    self.registers[*reg] = stack[stack_ptr + *mem + 1];
                }
                Instruction::StoreMem { reg, mem } => {
                    stack[stack_ptr + *mem + 1] = self.registers[*reg];
                }
                Instruction::Load { dest, val } => {
                    self.registers[*dest] = *val;
                }
                Instruction::Call { fn_name } => {
                    let stack_size = self.get_function_stack_size(fn_name);
                    println!("{fn_name} has a stack size of {stack_size}");
                    return_stack.push_back(ip);
                    return_stack.push_back(stack_size);
                    if stack_ptr < stack_size {
                        return Err(format!("{}: Stack Overflow when interpreting!", ERR_STR));
                    }
                    stack_ptr -= stack_size;
                    ip = self
                        .functions
                        .get(fn_name)
                        .unwrap_or_else(|| panic!("Could not find {fn_name} in function table"))
                        .get_ip();
                    add_ip = false;
                }
                Instruction::PushRet { reg } => {
                    return_values.push_back(self.registers[*reg]);
                }
                Instruction::PopRet { reg } => {
                    self.registers[*reg] = return_values.pop_back().unwrap();
                }
                Instruction::Return {} => {
                    if return_stack.is_empty() {
                        println!("Finished!");
                        break;
                    }
                    // Can safely unwrap because I just checked return_stack length
                    // Whenever I add something, I add packs of 2 values, so there's never a situation
                    // where only one element is on the return_stack
                    stack_ptr += return_stack.pop_back().unwrap();
                    ip = return_stack.pop_back().unwrap();
                }
            }
            if add_ip {
                ip += 1;
            }
            // for i in 0..10 {
            //     println!("{:5} {:5}", stack[STACK_SIZE - i - 1], self.registers[i]);
            // }
        }
        for i in 0..10 {
            println!("{}", stack[STACK_SIZE - i - 1]);
        }
        Ok(())
    }

    #[allow(unused)]
    pub fn compile(&mut self) -> Result<(), String> {
        todo!("Restructure the program -> Functions should use the same instruction space, and same memory")
    }
}
