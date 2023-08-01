use std::collections::{HashMap, VecDeque};

use crate::parser::{Tree, TreeType};
use crate::lexer::TokenType;

pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";

#[derive(Debug)]
enum Instruction {
    Load { dest: usize, val: usize },
    Add { dest: usize, src1: usize, src2: usize },
    Sub { dest: usize, src1: usize, src2: usize },
    Mul { dest: usize, src1: usize, src2: usize },
    Div { dest: usize, src1: usize, src2: usize },
    Copy { dest: usize, src: usize },
    Return {},
    Call { func_name: String },
    Print { src: usize }
}

#[derive(Debug)]
pub struct Generator {
    ast: Tree,
    memory: Vec<usize>,
    function_lookup: HashMap<String, usize>,
    local_lookup: HashMap<String, HashMap<String, usize>>,
    code: Vec<Instruction>,
    current_fn: Option<String>
}

impl Generator {
    pub fn new(ast: Tree) -> Result<Self, String> {
        let mut gen = Self {
            ast,
            memory: vec![],
            function_lookup: HashMap::new(),
            local_lookup: HashMap::new(),
            code: vec![],
            current_fn: None
        };
        gen.generate_code()?;
        Ok(gen)
    }

    fn get_mem(&mut self) -> usize {
        let r = self.memory.len();
        self.memory.push(0);
        r
    }    

    fn convert_expr_atomic(&mut self, instr: &Tree) -> Result<usize, String> {
        match instr.typ.as_ref().unwrap() {
            TreeType::ExprLiteral => {
                assert!(instr.children.len() == 1);
                let val = instr.children[0].tkn.as_ref().unwrap().get_value().parse().unwrap();
                let dest = self.get_mem();
                self.code.push(Instruction::Load { dest, val });
                return Ok(dest);
            },
            TreeType::ExprParen => {
                assert!(instr.children.len() == 1);
                return self.convert_expr_atomic(&instr.children[0]);
            }
            TreeType::ExprBinary => {
                assert!(*instr.typ.as_ref().unwrap() == TreeType::ExprBinary);
                assert!(instr.children.len() == 3);
                let lhs = instr.children[0].as_ref();
                let op = instr.children[1].as_ref();
                let rhs = instr.children[2].as_ref();
                let lhs_src = self.convert_expr_atomic(lhs)?;
                let rhs_src = self.convert_expr_atomic(rhs)?;
                let dst = self.get_mem();
                match op.tkn.as_ref().unwrap().get_type() {
                    TokenType::Plus => self.code.push(Instruction::Add { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Minus => self.code.push(Instruction::Sub { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Mult => self.code.push(Instruction::Mul { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Div => self.code.push(Instruction::Div { dest: dst, src1: lhs_src, src2: rhs_src }),
                    e => todo!("Handle {:?} in convert_expr_atomic()", e)
                }
                return Ok(dst);
            },
            TreeType::ExprName => {
                assert!(instr.children.len() == 1);
                let val_name = &instr.children[0];
                let name = val_name.tkn.as_ref().unwrap().get_value();
                
                let dst = self.get_mem();
                let local_vars = self.local_lookup.get_mut(self.current_fn.as_ref().unwrap()).unwrap();
                if local_vars.contains_key(&name) {
                    let src = *local_vars.get(&name).unwrap();
                    self.code.push(Instruction::Copy { dest: dst, src });
                    return Ok(dst);
                }
                return Err(format!("{}: {:?}: Undefined variable `{}`", ERR_STR, val_name.tkn.as_ref().unwrap().get_loc(), name));
            },
            e => todo!("Handle {:?} in convert_expr_atomic. Got:\n{:?}", e, instr)
        }
    }

    fn convert_expr(&mut self, expr_tree: &Tree) -> Result<usize, String> {
        match &expr_tree.typ {
            Some(t) => {
                match t {
                    TreeType::ExprLiteral
                    | TreeType::ExprBinary
                    | TreeType::ExprName
                    | TreeType::ExprParen => {
                        return self.convert_expr_atomic(expr_tree);
                    }
                    e => todo!("Handle {:?} in convert_expr", e)
                }
            },
            e => todo!("Handle {:?} in convert_expr", e)
        }
    }


    fn convert_stmt_let(&mut self, let_tree: &Tree) -> Result<(), String> {
        let instr_children = &let_tree.children;
        let let_keyword = &instr_children[0];
        assert!(let_keyword.tkn.as_ref().unwrap().get_type() == TokenType::LetKeyword);
        let let_name = &instr_children[1];
        assert!(let_name.tkn.as_ref().unwrap().get_type() == TokenType::Name);
        let let_eq = &instr_children[2];
        assert!(let_eq.tkn.as_ref().unwrap().get_type() == TokenType::Equal);
        let let_expr = &instr_children[3];
        
        let let_name = let_name.tkn.as_ref().unwrap();
        let local_lookup = self.local_lookup.get(self.current_fn.as_ref().unwrap()).unwrap();
        if local_lookup.contains_key(&let_name.get_value()) {
            return Err(format!("{}: {:?}: Variable redefinition", ERR_STR, let_name.get_loc()));
        }

        let dest = self.get_mem();
        let src = self.convert_expr(&let_expr)?;
        let local_vars = self.local_lookup.get_mut(self.current_fn.as_ref().unwrap()).unwrap();
        local_vars.insert(let_name.get_value(), dest);
        self.code.push(Instruction::Copy { dest, src });
        Ok(())
    }

    fn convert_stmt_assign(&mut self, assign_tree: &Tree) -> Result<(), String> {
        let instr_children = &assign_tree.children;
        let assign_name = &instr_children[0];
        assert!(assign_name.tkn.as_ref().unwrap().get_type() == TokenType::Name);
        let var_name = assign_name.tkn.as_ref().unwrap().get_value();
        let local_lookup = self.local_lookup.get(self.current_fn.as_ref().unwrap()).unwrap();
        if !local_lookup.contains_key(&var_name) {
            return Err(format!("{}: {:?}: Unknown variable `{}`", ERR_STR, assign_name.tkn.as_ref().unwrap().get_loc(), var_name));
        }
        let dest = *local_lookup.get(&var_name).unwrap();
        let assign_eq = &instr_children[1];
        assert!(assign_eq.tkn.as_ref().unwrap().get_type() == TokenType::Equal);
        let assign_expr = &instr_children[2];
        let src = self.convert_expr(assign_expr)?;
        self.code.push(Instruction::Copy { dest, src });
        Ok(())
    }
    
    fn convert_stmt_call(&mut self, call_tree: &Tree) -> Result<(), String> {
        let instr_children = &call_tree.children;
        let call_name = &instr_children[0];
        assert!(call_name.tkn.as_ref().unwrap().get_type() == TokenType::Name);
        let name = call_name.tkn.as_ref().unwrap().get_value();
        if !self.function_lookup.contains_key(&name) {
            return Err(format!("{} {:?}: Unknown function `{}`",ERR_STR, call_name.tkn.as_ref().unwrap().get_loc(), name));
        }
        self.code.push(Instruction::Call { func_name: name });
        Ok(())
    }

    fn convert_fn(&mut self, func: &Tree) -> Result<(), String> {
        let fn_children = &func.children;
        assert!(fn_children.len() == 3, "fnKeyword fnName {{block}} expected.");
        let fn_keyword = fn_children[0].tkn.as_ref().unwrap();
        assert!(fn_keyword.get_type() == TokenType::FnKeyword);
        let fn_name = fn_children[1].tkn.as_ref().unwrap();
        assert!(fn_name.get_type() == TokenType::Name);
        if self.function_lookup.contains_key(&fn_name.get_value()) {
            return Err(format!("{}: {:?}: Function redefinition", ERR_STR, fn_name.get_loc()));
        }
        let block = fn_children[2].typ.as_ref().unwrap();
        let name = fn_name.get_value();

        self.current_fn = Some(name.clone());
        self.function_lookup.insert(name.clone(), self.code.len());
        self.local_lookup.insert(name.clone(), HashMap::new());
        self.convert_fn_helper(&fn_children[2])?;
        self.current_fn = None;

        assert!(*block == TreeType::Block);
        Ok(())
    }

    
    fn convert_fn_helper(&mut self, block: &Tree) -> Result<(), String> {
        assert!(*block.typ.as_ref().unwrap() == TreeType::Block);
        for instr in &block.children {
            match &instr.typ {
                Some(t) => {
                    match t {
                        TreeType::StmtLet => self.convert_stmt_let(instr)?,
                        TreeType::StmtAssign => self.convert_stmt_assign(instr)?,
                        TreeType::StmtCall => self.convert_stmt_call(instr)?,
                        e => todo!("convert {:?} inside function", e)
                    }
                },
                _ => panic!()
            }
        }
        self.code.push(Instruction::Return {});
        Ok(())
    }

    fn convert_ast(&mut self, ast: &Tree) -> Result<(), String> {
        match &ast.typ {
            Some(typ) => {
                match typ {
                    TreeType::File => {
                        for func in &ast.children {
                            self.convert_fn(func)?;
                        }
                    },
                    _ => todo!("handle other types")
                }
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
        let entry_point = String::from("main");
        let mut return_stack = VecDeque::<usize>::new();
        const RETURN_STACK_SIZE: usize = 4096;

        if !self.function_lookup.contains_key(&entry_point) {
            return Err(format!("{}: Missing entry point - Could not find function {}()", ERR_STR, entry_point));
        }
        let mut ip = *self.function_lookup.get(&entry_point).unwrap();

        while ip < self.code.len() {
            if return_stack.len() > RETURN_STACK_SIZE {
                return Err(format!("{}: Stack Overflow when interpreting!", ERR_STR));
            }
            let instr = &self.code[ip];
            println!("{:3} {:?}", ip, instr);
            let mut add_ip = true;
            match instr {
                Instruction::Add { dest, src1, src2 } => {
                    self.memory[*dest] = self.memory[*src1] + self.memory[*src2];
                },
                Instruction::Sub { dest, src1, src2 } => {
                    self.memory[*dest] = self.memory[*src1] - self.memory[*src2];
                },
                Instruction::Mul { dest, src1, src2 } => {
                    self.memory[*dest] = self.memory[*src1] * self.memory[*src2];
                },
                Instruction::Div { dest, src1, src2 } => {
                    self.memory[*dest] = self.memory[*src1] / self.memory[*src2];
                },
                Instruction::Copy { dest, src } => {
                    self.memory[*dest] = self.memory[*src];
                },
                Instruction::Load { dest, val } => {
                    self.memory[*dest] = *val;
                },
                Instruction::Call { func_name } => {
                    return_stack.push_back(ip);
                    println!("Entering {}", func_name);
                    ip = *self.function_lookup.get(func_name).unwrap();
                    add_ip = false;
                }
                Instruction::Return {} => {
                    if return_stack.len() == 0 {
                        println!("Finished!");
                        break;
                    }
                    ip = return_stack.pop_back().unwrap();
                }
                Instruction::Print { src: _src } => todo!(),
            }
            if add_ip {
                ip += 1;
            }
        }
        for (fn_name, fn_local) in &self.local_lookup {
            println!("Local variables for `{}()`", fn_name);
            for (name, ip) in fn_local {
                println!("{:10} -> {}", name, self.memory[*ip]);
            }
        }
        Ok(())
    }

    pub fn compile(&mut self) -> Result<(), String> {
        todo!("Restructure the program -> Functions should use the same instruction space, and same memory")
    }
}