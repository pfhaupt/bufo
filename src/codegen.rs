use std::collections::{HashMap, HashSet};

use crate::parser::{Tree, TreeType, Node};
use crate::lexer::{Token, TokenType};

#[derive(Debug)]
enum Instruction {
    Load { dest: usize, val: u64 },
    Add { dest: usize, src1: usize, src2: usize },
    Sub { dest: usize, src1: usize, src2: usize },
    Copy { dest: usize, src: usize },
    Print { src: usize }
}

#[derive(Debug)]
struct Function {
    code: Vec<Instruction>
}

#[derive(Debug)]
pub struct Generator {
    ast: Tree,
    memory: Vec<u64>,
    functions: HashMap<String, Function>,
    register_count: usize
}

impl Generator {
    pub fn new(ast: Tree, local_count: usize) -> Self {
        let mut gen = Self { ast, memory: vec![], functions: HashMap::new(), register_count: local_count };
        gen.generate_code();
        gen
    }

    fn convert_ast(&mut self, ast: &Tree) {
        match ast.typ {
            TreeType::File => {
                // for func in &ast.children {
                //     self.convert_func(func);
                // }
                todo!()
            }
            _ => todo!("handle {ast:?}")
        }
    }

    fn allocate_register(&mut self) -> usize {
        let result = self.register_count;
        self.register_count += 1;
        result
    }

    // fn add_instruction(&mut self, instr: Instruction) {
    //     self.code.push(instr);
    // }

    fn generate_code(&mut self) {
        self.convert_ast(&self.ast.clone());
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        todo!()
    }
}