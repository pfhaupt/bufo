use std::collections::{HashMap, HashSet, VecDeque};

use crate::parser::{Tree, TreeType};
use crate::lexer::TokenType;

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
    pub fn new(ast: Tree) -> Self {
        let mut gen = Self {
            ast,
            memory: vec![],
            function_lookup: HashMap::new(),
            local_lookup: HashMap::new(),
            code: vec![],
            current_fn: None
        };
        gen.generate_code();
        gen
    }

    fn get_mem(&mut self) -> usize {
        let r = self.memory.len();
        self.memory.push(0);
        r
    }    

    fn convert_expr(&mut self, instr: &Tree) -> usize {
        match instr.typ.as_ref().unwrap() {
            TreeType::ExprLiteral => {
                assert!(instr.children.len() == 1);
                let val = instr.children[0].tkn.as_ref().unwrap().get_value().parse().unwrap();
                let dest = self.get_mem();
                self.code.push(Instruction::Load { dest, val });
                return dest;
            },
            TreeType::ExprParen => {
                assert!(instr.children.len() == 1);
                return self.convert_expr(&instr.children[0]);
            }
            TreeType::ExprBinary => {
                assert!(*instr.typ.as_ref().unwrap() == TreeType::ExprBinary);
                assert!(instr.children.len() == 3);
                let lhs = instr.children[0].as_ref();
                let op = instr.children[1].as_ref();
                let rhs = instr.children[2].as_ref();
                let lhs_src = self.convert_expr(lhs);
                let rhs_src = self.convert_expr(rhs);
                let dst = self.get_mem();
                match op.tkn.as_ref().unwrap().get_type() {
                    TokenType::Plus => self.code.push(Instruction::Add { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Minus => self.code.push(Instruction::Sub { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Mult => self.code.push(Instruction::Mul { dest: dst, src1: lhs_src, src2: rhs_src }),
                    TokenType::Div => self.code.push(Instruction::Div { dest: dst, src1: lhs_src, src2: rhs_src }),
                    e => todo!("Handle {:?} in convert_expr()", e)
                }
                return dst;
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
                    return dst;
                }
                panic!("Local lookup does not contain the variable!");
            },
            e => todo!("Handle {:?} in convert_expr. Got:\n{:?}", e, instr)
        }
    }

    fn convert_assignment(&mut self, instr: &Tree) -> usize {
        let eq = &instr.children[2];
        assert!(eq.tkn.as_ref().unwrap().get_type() == TokenType::Equal);
        let rhs = &instr.children[3];
        match &rhs.typ {
            Some(t) => {
                match t {
                    TreeType::ExprLiteral
                    | TreeType::ExprBinary => {
                        return self.convert_expr(rhs);
                    }
                    e => todo!("Handle {:?} in convert_assignment", e)
                }
            },
            e => todo!("Handle {:?} in convert_assignment", e)
        }
    }


    fn convert_let(&mut self, instr: &Tree) {
        let instr_children = &instr.children;
        let let_keyword = &instr_children[0];
        assert!(let_keyword.tkn.as_ref().unwrap().get_type() == TokenType::LetKeyword);
        let let_name = &instr_children[1];
        assert!(let_name.tkn.as_ref().unwrap().get_type() == TokenType::Name);
        let let_name = let_name.tkn.as_ref().unwrap();
        if self.local_lookup.contains_key(&let_name.get_value()) {
            panic!("Variable redefinition in function");
        }
        let dest = self.get_mem();
        let local_vars = self.local_lookup.get_mut(self.current_fn.as_ref().unwrap()).unwrap();
        local_vars.insert(let_name.get_value(), dest);
        let src = self.convert_assignment(instr);
        self.code.push(Instruction::Copy { dest, src });
    }

    fn convert_fn(&mut self, func: &Tree) {
        let fn_children = &func.children;
        assert!(fn_children.len() == 3, "fnKeyword fnName {{block}} expected.");
        let fn_keyword = fn_children[0].tkn.as_ref().unwrap();
        assert!(fn_keyword.get_type() == TokenType::FnKeyword);
        let fn_name = fn_children[1].tkn.as_ref().unwrap();
        assert!(fn_name.get_type() == TokenType::Name);
        if self.function_lookup.contains_key(&fn_name.get_value()) {
            panic!("Function redefinition!!");
        }
        let block = fn_children[2].typ.as_ref().unwrap();
        let name = fn_name.get_value();
        let mem_size = self.memory.len();
        self.current_fn = Some(name.clone());
        self.function_lookup.insert(name.clone(), self.code.len());
        self.local_lookup.insert(name.clone(), HashMap::new());
        self.convert_fn_helper(&fn_children[2]);
        self.current_fn = None;
        // let local_mem_size = f.get_offset() - mem_size;
        // for _ in 0..local_mem_size { self.memory.push(0); }
        // self.functions.insert(fn_name.get_value(), f);
        assert!(*block == TreeType::Block);
    }

    
    fn convert_fn_helper(&mut self, block: &Tree) {
        assert!(*block.typ.as_ref().unwrap() == TreeType::Block);
        for instr in &block.children {
            match &instr.typ {
                Some(t) => {
                    match t {
                        TreeType::StmtLet => self.convert_let(instr),
                        e => todo!("convert {:?} to function", e)
                    }
                },
                _ => panic!()
            }
        }
        self.code.push(Instruction::Return {});
    }

    fn convert_ast(&mut self, ast: &Tree) {
        match &ast.typ {
            Some(typ) => {
                match typ {
                    TreeType::File => {
                        for func in &ast.children {
                            self.convert_fn(func);
                        }
                    },
                    _ => todo!("handle other types")
                }
            },
            None => {
                panic!()
            }
        }
    }

    // fn add_instruction(&mut self, instr: Instruction) {
    //     self.code.push(instr);
    // }

    fn generate_code(&mut self) {
        self.convert_ast(&self.ast.clone());
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        let entry_point = String::from("main");
        let mut return_stack = VecDeque::<usize>::new();
        if !self.function_lookup.contains_key(&entry_point) {
            return Err(format!("Missing entry point - Could not find function {}()", entry_point));
        }
        let mut ip = *self.function_lookup.get(&entry_point).unwrap();

        while ip < self.code.len() {
            let instr = &self.code[ip];
            println!("{} {:?}", ip, instr);
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
                    // return_stack.push(ip);
                    todo!()
                    // ip = self.function_lookup.get(func_name);
                }
                Instruction::Return {} => {
                    if return_stack.len() == 0 {
                        println!("Finished!");
                        break;
                    }
                    ip = return_stack.pop_back().unwrap();
                    todo!()
                }
                Instruction::Print { src: _src } => todo!(),
            }
            ip += 1;
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