use std::collections::{HashMap, HashSet};

use crate::parser::Ast;

#[derive(Debug)]
enum Instruction {
    Load { dest: usize, val: u64 },
    Add { dest: usize, src1: usize, src2: usize },
    Sub { dest: usize, src1: usize, src2: usize },
    Copy { dest: usize, src: usize },
    Print { src: usize }
}

#[derive(Debug)]
pub struct Generator {
    ast: Ast,
    memory: Vec<u64>,
    intrinsics: HashSet<String>,
    code: Vec<Instruction>,
    register_count: usize
}

impl Generator {
    pub fn new(program: (Ast, Vec<u64>, HashMap<String, usize>, HashSet<String>), local_count: usize) -> Self {
        let mut gen = Self { ast: program.0, memory: program.1, intrinsics: program.3, code: vec![], register_count: local_count };
        gen.generate_code();
        gen
    }

    fn convert_ast(&mut self, ast: &Ast) -> usize {
        match ast {
            Ast::Const(value) => {
                let dest = self.allocate_register();
                self.add_instruction(Instruction::Load { dest, val: *value } );
                dest
            },
            Ast::Assign(mem, value) => {
                let src = self.convert_ast(value);
                let dest = *mem;
                self.add_instruction(Instruction::Copy { dest, src } );
                usize::MAX
            },
            Ast::Var(mem) => {
                let src = *mem;
                let dest = self.allocate_register();
                self.add_instruction(Instruction::Copy { dest, src });
                dest
            },
            Ast::Add(lhs, rhs) => {
                let src1 = self.convert_ast(lhs);
                let src2 = self.convert_ast(rhs);
                let dest = self.allocate_register();
                self.add_instruction(Instruction::Add { dest, src1, src2 } );
                dest
            },
            Ast::Sub(lhs, rhs) => {
                let src1 = self.convert_ast(lhs);
                let src2 = self.convert_ast(rhs);
                let dest = self.allocate_register();
                self.add_instruction(Instruction::Sub { dest, src1, src2 } );
                dest
            },
            Ast::Func(name, ast) => {
                match name {
                    intrinsic if self.intrinsics.contains(name) => {
                        match intrinsic.as_str() {
                            "print"=> {
                                let src = self.convert_ast(ast);
                                self.add_instruction(Instruction::Print { src } );
                                usize::MAX
                            },
                            _ => {
                                unreachable!("There's only {} intrinsic, and it's all handled.", self.intrinsics.len());
                            }
                        }
                    },
                    _ => {
                        todo!("Handle generic functions");
                    }
                }
            }
            Ast::Block(ast_vec) => {
                let mut max = 0;
                for a in ast_vec {
                    max = max.max(self.convert_ast(a));
                }
                max
            },
        }
    }

    fn allocate_register(&mut self) -> usize {
        let result = self.register_count;
        self.register_count += 1;
        result
    }

    fn add_instruction(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    fn generate_code(&mut self) {
        self.convert_ast(&self.ast.clone());
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        // println!("{:?}", self.memory);
        for instr in &self.code {
            // println!("{:?}", instr);
            match *instr {
                Instruction::Copy { dest, src } => {
                    self.memory[dest] = self.memory[src];
                },
                Instruction::Load { dest, val } => {
                    self.memory[dest] = val;
                },
                Instruction::Add { dest, src1, src2 } => {
                    let l = self.memory[src1];
                    let r = self.memory[src2];
                    self.memory[dest] = l + r;
                },
                Instruction::Sub { dest, src1, src2 } => {
                    let l = self.memory[src1];
                    let r = self.memory[src2];
                    self.memory[dest] = l - r;
                },
                Instruction::Print { src } => {
                    println!("{}", self.memory[src]);
                }
            }
        }
        Ok(())
    }
}