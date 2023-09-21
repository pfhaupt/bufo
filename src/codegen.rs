use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use crate::checker::Type;
use crate::lexer::{Location, TokenType};
use crate::parser::{Tree, TreeType};

pub const RUNTIME_ERR: &str = "\x1b[91mRuntime Exception\x1b[0m";
pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
#[allow(unused)]
pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

const REGISTERS_64BIT: [&str; 12] = [
    "rax", "rbx", "rcx", "rdx", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
];
const REGISTERS_32BIT: [&str; 12] = [
    "eax", "ebx", "ecx", "edx", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
];

macro_rules! perform_op {
    ($dest: expr, $reg1:expr, $reg2:expr, $typ:expr, $op:tt) => {
        match $typ {
            Type::I32 => unsafe { $dest.i32 = $reg1.i32 $op $reg2.i32 },
            Type::I64 => unsafe { $dest.i64 = $reg1.i64 $op $reg2.i64 },
            Type::U32 => unsafe { $dest.u32 = $reg1.u32 $op $reg2.u32 },
            Type::U64 => unsafe { $dest.u64 = $reg1.u64 $op $reg2.u64 },
            Type::Usize => unsafe { $dest.ptr = $reg1.ptr $op $reg2.ptr },
            Type::F32 => unsafe { $dest.f32 = $reg1.f32 $op $reg2.f32 },
            Type::F64 => unsafe { $dest.f64 = $reg1.f64 $op $reg2.f64 },
            // Type::Ptr(..) => unsafe { $dest.ptr = $reg1.ptr $op $reg2.ptr },
            _ => todo!()
        }
    };
}

macro_rules! perform_cmp {
    ($reg1:expr, $reg2:expr, $typ:expr, $op:tt) => {
        match $typ {
            Type::I32 => unsafe { $reg1.i32 $op $reg2.i32 },
            Type::I64 => unsafe { $reg1.i64 $op $reg2.i64 },
            Type::U32 => unsafe { $reg1.u32 $op $reg2.u32 },
            Type::U64 => unsafe { $reg1.u64 $op $reg2.u64 },
            Type::F32 => unsafe { $reg1.f32 $op $reg2.f32 },
            Type::F64 => unsafe { $reg1.f64 $op $reg2.f64 },
            Type::Usize => unsafe { $reg1.ptr $op $reg2.ptr },
            _ => todo!("{:?}", $typ)
        }
    };
}

// parse_type!(i32, loc, val, typ);
macro_rules! parse_type {
    ($parse_typ:ty, $loc:expr, $val:expr, $typ: expr) => {
        match $val.parse::<$parse_typ>() {
            Ok(val) => Ok(val),
            Err(_) => Err(format!(
                "{}: {:?}: Integer Literal `{}` too big for Type `{:?}`",
                ERR_STR, $loc, $val, $typ
            )),
        }
    };
}

#[derive(Copy, Clone)]
union Memory {
    i32: i32,
    i64: i64,
    u32: u32,
    u64: u64,
    f32: f32,
    f64: f64,
    ptr: usize,
}

impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", unsafe { self.u64 })
    }
}

impl std::fmt::Debug for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", unsafe { self.u64 })
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    typ: Type,
    mem: usize,
}

#[allow(unused)]
#[derive(Debug, PartialEq)]
enum Instruction {
    LoadUnknown {
        dest: usize,
        val: String,
        loc: Location,
    },
    LoadI32 {
        dest: usize,
        val: i32,
    },
    LoadI64 {
        dest: usize,
        val: i64,
    },
    LoadU32 {
        dest: usize,
        val: u32,
    },
    LoadU64 {
        dest: usize,
        val: u64,
    },
    LoadUsize {
        dest: usize,
        val: usize,
    },
    LoadPtrRel {
        dest: usize,
        src: usize,
        typ: Type,
    },
    LoadPtr {
        dest: usize,
        src: usize,
        typ: Type,
    },
    StorePtr {
        dest: usize,
        src: usize,
        typ: Type,
    },
    LoadF32 {
        dest: usize,
        val: f32,
    },
    LoadF64 {
        dest: usize,
        val: f64,
    },
    Add {
        dest: usize,
        src: usize,
        typ: Type,
    },
    Sub {
        dest: usize,
        src: usize,
        typ: Type,
    },
    Mul {
        dest: usize,
        src: usize,
        typ: Type,
    },
    Div {
        dest: usize,
        src: usize,
        typ: Type,
    },
    Cmp {
        dest: usize,
        src: usize,
        typ: Type,
    },
    Jmp {
        dest: String,
    },
    JmpEq {
        dest: String,
    },
    JmpNeq {
        dest: String,
    },
    JmpGt {
        dest: String,
    },
    JmpGte {
        dest: String,
    },
    JmpLt {
        dest: String,
    },
    JmpLte {
        dest: String,
    },
    Move {
        dest: usize,
        src: usize,
    },
    LoadMem {
        reg: usize,
        var: Variable,
    },
    StoreMem {
        reg: usize,
        var: Variable,
    },
    Call {
        fn_name: String,
    },
    Push {
        reg: usize,
    },
    Pop {
        reg: usize,
    },
    Return {
        from: String,
    },
    Exit {
        code: usize,
    },
    Label {
        name: String,
    },
    // Print { src: usize },
}

enum ExitCode {
    Normal,
    OobAccess,
}

impl ExitCode {
    fn print_exit_msg(code: usize) {
        match code {
            0 => unreachable!(),
            1 => println!("{}: Attempted to index out of bounds!", RUNTIME_ERR),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Function {
    ip: usize,
    param_variables: BTreeMap<String, Variable>,
    local_variables: Vec<BTreeMap<String, Variable>>,
    return_scopes: Vec<usize>,
    return_type: Variable,
    stack_size: usize,
}

impl Function {
    fn new(ip: usize) -> Self {
        Self {
            ip,
            param_variables: BTreeMap::new(),
            local_variables: vec![BTreeMap::new()],
            return_scopes: vec![],
            return_type: Variable { typ: Type::None, mem: 0 },
            stack_size: 0,
        }
    }

    fn get_ip(&self) -> usize {
        self.ip
    }

    fn get_stack_size(&self) -> usize {
        self.stack_size
    }

    fn get_params(&self) -> &BTreeMap<String, Variable> {
        &self.param_variables
    }

    fn get_param_location(&self, var_name: &String) -> Option<Variable> {
        self.param_variables.get(var_name).cloned()
    }

    fn get_local_location(&self, var_name: &String) -> Option<Variable> {
        for scope in (0..self.local_variables.len()).rev() {
            if let Some(mem) = self.local_variables.get(scope).unwrap().get(var_name) {
                return Some(mem.clone());
            }
        }
        None
    }

    fn get_scope_location(&self, var_name: &String) -> Option<Variable> {
        self.local_variables.last().unwrap().get(var_name).cloned()
    }

    fn get_variable_location(&self, var_name: &String) -> Option<Variable> {
        match self.get_param_location(var_name) {
            Some(i) => Some(i),
            None => self.get_local_location(var_name),
        }
    }

    fn add_local_variable(&mut self, sm: &SizeManager, var_name: String, mem: usize, scope_depth: usize, typ: &Type) {
        let scope_var = self.local_variables.get_mut(scope_depth).unwrap();
        scope_var.insert(
            var_name,
            Variable {
                mem,
                typ: typ.clone(),
            },
        );
        self.stack_size += sm.get_type_size(typ);
    }

    fn add_param(&mut self, sm: &SizeManager, param_name: &str, typ: &Type) -> Variable {
        let mem = self.get_stack_size();
        let var = Variable {
            mem,
            typ: typ.clone(),
        };
        self.param_variables
            .insert(param_name.to_owned(), var.clone());
        self.stack_size += sm.get_type_size(typ);
        var.clone()
    }

    fn add_scope(&mut self) {
        self.local_variables.push(BTreeMap::new());
    }

    fn remove_scope(&mut self) {
        self.local_variables.pop().unwrap();
    }

    fn set_return_type(&mut self, sm: &SizeManager, typ: &Type) {
        if self.return_type.typ != Type::None {
            panic!()
        }
        self.return_type = Variable {
            typ: typ.clone(),
            mem: self.stack_size
        };
        self.stack_size += sm.get_type_size(typ);
    }

    fn get_return_loc(&self) -> usize {
        self.return_type.mem
    }

    fn get_return_type(&self) -> Type {
        self.return_type.typ.clone()
    }
}

#[derive(Debug)]
struct SizeManager {
}

impl SizeManager {
    fn new() -> Self {
        Self {
        }
    }

    fn get_type_size(&self, typ: &Type) -> usize {
        match typ {
            Type::Arr(t, size) => self.get_type_size(t) * size.iter().product::<usize>(),
            Type::I32 | Type::U32 => 4,
            Type::I64 | Type::U64 | Type::Usize => 8,
            // Type::Ptr(..) => 8,
            e => todo!("Figure out size of type {:?} in bytes", e),
        }
    }
}

#[derive(Debug)]
pub struct Generator {
    ast: Tree,
    entry_point: String,
    registers: Vec<Memory>,
    register_ctr: usize,
    functions: HashMap<String, Function>,
    code: Vec<Instruction>,
    current_fn: String,
    current_str: String,
    unresolved_jmp_instr: VecDeque<usize>,
    label_lookup: HashMap<String, usize>,
    unresolved_typ_instr: VecDeque<usize>,
    size_manager: SizeManager,
    scope_depth: usize,
    memory_offset: usize,
    print_debug: bool,
}

impl Generator {
    pub fn new(ast: Tree, print_debug: bool) -> Result<Self, String> {
        let mut gen = Self {
            ast,
            entry_point: String::from("main"),
            registers: vec![Memory { u64: 0 }; REGISTERS_32BIT.len()],
            register_ctr: 1,
            functions: HashMap::new(),
            code: vec![],
            current_fn: String::new(),
            current_str: String::new(),
            unresolved_jmp_instr: VecDeque::new(),
            label_lookup: HashMap::new(),
            unresolved_typ_instr: VecDeque::new(),
            size_manager: SizeManager::new(),
            scope_depth: 0,
            memory_offset: 0,
            print_debug,
        };
        gen.generate_code()?;
        Ok(gen)
    }

    fn create_label(&mut self) -> String {
        format!("lbl_{}", self.label_lookup.len())
    }

    fn add_label(&mut self, lbl: &str) {
        self.label_lookup.insert(lbl.to_owned(), self.code.len());
        self.code.push(Instruction::Label {
            name: lbl.to_owned(),
        });
    }

    fn get_label_ip(&self, lbl: &String) -> usize {
        *self
            .label_lookup
            .get(lbl)
            .expect("At this point label_lookup should contain the label")
    }

    fn add_local_var(&mut self, var_name: &str, scope_depth: usize, typ: &Type) -> usize {
        assert!(!self.current_fn.is_empty());
        assert!(self.functions.contains_key(&self.current_fn));
        let mem = self.get_stack_offset();
        self.functions
            .get_mut(&self.current_fn)
            .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
            .add_local_variable(&self.size_manager, var_name.to_owned(), mem, scope_depth, typ);
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
        self.register_ctr = 1;
    }

    fn get_register(&mut self) -> Result<usize, String> {
        let r = self.register_ctr;
        if r >= self.registers.len() {
            return Err(
                "Oopsie Woopsie: The Compiler ran out of registers to allocate.".to_string(),
            );
            // self.registers.push(Memory { u64: 0 });
        }
        self.register_ctr += 1;
        Ok(r)
    }

    fn convert_expr(&mut self, expr_tree: &Tree) -> Result<Variable, String> {
        match &expr_tree.typ {
            TreeType::ExprParen { expression, .. } => self.convert_expr(expression),
            TreeType::ExprBinary { lhs, rhs, typ } => {
                let dest = self.convert_expr(lhs)?;
                let src = self.convert_expr(rhs)?;
                let dest = dest.mem;
                let src = src.mem;
                match expr_tree.tkn.get_type() {
                    TokenType::Plus => self.code.push(Instruction::Add {
                        dest,
                        src,
                        typ: typ.clone(),
                    }),
                    TokenType::Minus => self.code.push(Instruction::Sub {
                        dest,
                        src,
                        typ: typ.clone(),
                    }),
                    TokenType::Asterisk => self.code.push(Instruction::Mul {
                        dest,
                        src,
                        typ: typ.clone(),
                    }),
                    TokenType::ForwardSlash => self.code.push(Instruction::Div {
                        dest,
                        src,
                        typ: typ.clone(),
                    }),
                    e => todo!("Handle {:?} in convert_expr()", e),
                }
                Ok(Variable {
                    typ: typ.clone(),
                    mem: dest,
                })
            }
            TreeType::ExprComp { lhs, rhs, .. } => {
                let dest = self.convert_expr(lhs)?;
                let src = self.convert_expr(rhs)?;

                let typ = dest.typ;
                assert!(typ == src.typ);

                let dest = dest.mem;
                let src = src.mem;

                let lbl_name = self.create_label();
                self.add_label(&lbl_name);

                self.code.push(Instruction::Cmp {
                    dest,
                    src,
                    typ: typ.clone(),
                });
                self.unresolved_jmp_instr.push_back(self.code.len());

                match expr_tree.tkn.get_type() {
                    TokenType::CmpEq => self.code.push(Instruction::JmpNeq {
                        dest: String::new(),
                    }),
                    TokenType::CmpNeq => self.code.push(Instruction::JmpEq {
                        dest: String::new(),
                    }),
                    TokenType::CmpGt => self.code.push(Instruction::JmpLte {
                        dest: String::new(),
                    }),
                    TokenType::CmpGte => self.code.push(Instruction::JmpLt {
                        dest: String::new(),
                    }),
                    TokenType::CmpLt => self.code.push(Instruction::JmpGte {
                        dest: String::new(),
                    }),
                    TokenType::CmpLte => self.code.push(Instruction::JmpGt {
                        dest: String::new(),
                    }),
                    e => todo!("Handle {:?} in convert_expr()", e),
                }
                Ok(Variable {
                    typ: Type::Bool,
                    mem: dest,
                })
            }
            TreeType::ExprName { name, .. } => {
                let reg = self.get_register()?;
                let curr_fn = self
                    .functions
                    .get(&self.current_fn)
                    .expect("At this point, function table is guaranteed to contain current_fn.");
                match curr_fn.get_variable_location(name) {
                    Some(var) => {
                        self.code.push(Instruction::LoadMem {
                            reg,
                            var: Variable {
                                typ: var.typ.clone(),
                                mem: var.mem,
                            },
                        });
                        Ok(Variable {
                            mem: reg,
                            typ: var.typ.clone(),
                        })
                    }
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        name
                    )),
                }
            }
            TreeType::ExprLiteral { typ } => {
                let val = expr_tree.tkn.get_value();
                let loc = expr_tree.tkn.get_loc();
                let dest = self.get_register()?;
                match typ {
                    Type::I32 => {
                        let val = parse_type!(i32, loc, val, typ)?;
                        self.code.push(Instruction::LoadI32 { dest, val });
                    }
                    Type::I64 => {
                        let val = parse_type!(i64, loc, val, typ)?;
                        self.code.push(Instruction::LoadI64 { dest, val });
                    }
                    Type::U32 => {
                        let val = parse_type!(u32, loc, val, typ)?;
                        self.code.push(Instruction::LoadU32 { dest, val });
                    }
                    Type::U64 => {
                        let val = parse_type!(u64, loc, val, typ)?;
                        self.code.push(Instruction::LoadU64 { dest, val });
                    }
                    Type::Usize => {
                        let val = parse_type!(usize, loc, val, typ)?;
                        self.code.push(Instruction::LoadUsize { dest, val });
                    }
                    Type::Unknown => panic!(
                        "Type Checker failed to recover type, found unknown Literal at {loc:?}"
                    ),
                    _ => todo!(),
                };
                Ok(Variable {
                    mem: dest,
                    typ: typ.clone(),
                })
            }
            TreeType::ExprCall {
                function_name,
                args,
                typ,
            } => {
                if let Some(func) = self.functions.get(function_name) {
                    let fn_return_type = func.get_return_type();
                    let params = func.get_params();
                    match &args.typ {
                        TreeType::ArgList { arguments } => {
                            // Save important registers that are still needed but can be destroyed by fn call
                            let curr_reg_ctr = self.register_ctr;
                            for reg in 1..curr_reg_ctr {
                                self.code.push(Instruction::Push { reg });
                            }

                            // Convert argument expressions
                            let mut reg_ctr = 1; // Register 0 (RAX) is reserved
                            for arg in arguments.iter() {
                                let reg = self.convert_expr(arg)?;
                                
                                if reg_ctr != reg.mem {
                                    self.code.push(Instruction::Move {
                                        dest: reg_ctr,
                                        src: reg.mem,
                                    });
                                }
                            }
                            self.code.push(Instruction::Call {
                                fn_name: function_name.clone(),
                            });
                            let reg = self.get_register()?;
                            if fn_return_type != Type::None {
                                self.code.push(Instruction::Move { dest: reg, src: 0 });
                            }

                            // Restore important registers from above
                            for reg in (1..curr_reg_ctr).rev() {
                                self.code.push(Instruction::Pop { reg });
                            }
                            Ok(Variable {
                                typ: fn_return_type,
                                mem: reg,
                            })
                        }
                        _ => panic!(),
                    }
                } else {
                    Err(format!(
                        "{} {:?}: Unknown function `{}`",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        function_name
                    ))
                }
            }
            TreeType::Arg { expression } => self.convert_expr(expression),
            TreeType::ExprArrAccess {
                arr_name,
                indices,
                typ,
            } => {
                let function_stack = self
                    .functions
                    .get(&self.current_fn)
                    .expect("At this point, function lookup is guaranteed to contain current_fn");
                match function_stack.get_variable_location(arr_name) {
                    Some(variable) => {
                        match &variable.typ {
                            Type::Arr(typ, size) => {
                                match &indices.typ {
                                    TreeType::ExprArrLiteral { elements } => {
                                        let index_reg = self.get_register()?;
                                        let mem_offset = self.get_register()?;
                                        let size_reg = self.get_register()?;
                                        // Prepare index register
                                        self.code.push(Instruction::LoadUsize {
                                            dest: index_reg,
                                            val: 0,
                                        });
                                        // Stores final offset for stack addressing
                                        self.code.push(Instruction::LoadUsize {
                                            dest: mem_offset,
                                            val: variable.mem,
                                        });
                                        for (count, child) in elements.iter().enumerate() {
                                            let reg = self.convert_expr(child)?;

                                            // Index Out of Bounds check
                                            self.code.push(Instruction::LoadUsize {
                                                dest: size_reg,
                                                val: size[count],
                                            });
                                            self.code.push(Instruction::Cmp {
                                                dest: reg.mem,
                                                src: size_reg,
                                                typ: Type::Usize,
                                            });
                                            let lbl = self.create_label();
                                            self.code
                                                .push(Instruction::JmpLt { dest: lbl.clone() });
                                            self.code.push(Instruction::Exit {
                                                code: ExitCode::OobAccess as usize,
                                            });
                                            self.add_label(&lbl);

                                            // index_reg contains our index
                                            // in each step, multiply the index by the dimension of the array
                                            self.code.push(Instruction::Mul {
                                                dest: index_reg,
                                                src: size_reg,
                                                typ: Type::Usize,
                                            });
                                            // in each step, add to that ^ the position we're indexing (result of expression)
                                            self.code.push(Instruction::Add {
                                                dest: index_reg,
                                                src: reg.mem,
                                                typ: Type::Usize,
                                            });
                                        }

                                        self.code.push(Instruction::Add {
                                            dest: index_reg,
                                            src: mem_offset,
                                            typ: Type::Usize,
                                        });
                                        let reg_tmp = self.get_register()?;
                                        self.code.push(Instruction::LoadPtrRel {
                                            dest: reg_tmp,
                                            src: index_reg,
                                            typ: *typ.clone(),
                                        });
                                        self.code.push(Instruction::LoadPtr {
                                            dest: reg_tmp,
                                            src: reg_tmp,
                                            typ: *typ.clone(),
                                        });
                                        Ok(Variable {
                                            typ: *typ.clone(),
                                            mem: reg_tmp,
                                        })
                                    }
                                    _ => panic!(),
                                }
                            }
                            _ => panic!(),
                        }
                    }
                    None => panic!(),
                }
            }
            e => {
                expr_tree.print_debug();
                expr_tree.rebuild_code();
                println!();
                todo!("Handle Expr Type")
            },
        }
    }


    fn convert_let_arr(
        &mut self,
        arr_tree: &Tree,
        expected_type: &Type,
    ) -> Result<(), String> {
        match &arr_tree.typ {
            TreeType::ExprArrLiteral { elements } => {
                for elem in elements {
                    self.convert_let_arr(elem, expected_type)?;
                }
            }
            _ => {
                let reg = self.convert_expr(arr_tree)?;
                self.code.push(Instruction::StoreMem {
                    reg: reg.mem,
                    var: Variable {
                        typ: expected_type.clone(),
                        mem: self.memory_offset
                    },
                });
                self.memory_offset += self.size_manager.get_type_size(expected_type);
                self.reset_registers();
            }
        }
        Ok(())
    }

    fn convert_stmt_let(&mut self, let_tree: &Tree) -> Result<(), String> {
        match &let_tree.typ {
            TreeType::StmtLet {
                name,
                typ,
                expression,
            } => {
                let local_lookup = self
                    .functions
                    .get(&self.current_fn)
                    .expect("At this point, function table is guaranteed to contain current_fn.");
                if local_lookup.get_scope_location(name).is_some() {
                    return Err(format!(
                        "{}: {:?}: Variable redefinition",
                        ERR_STR,
                        let_tree.tkn.get_loc()
                    ));
                }
                match &typ.typ {
                    TreeType::TypeDecl { typ } => {
                        let mem = self.add_local_var(name, self.scope_depth, typ);
                        match typ {
                            Type::Arr(expected_type, expected_size) => {
                                self.convert_let_arr(expression, expected_type)?;
                            }
                            _ => {
                                let reg = self.convert_expr(expression)?;
                                self.code.push(Instruction::StoreMem {
                                    reg: reg.mem,
                                    var: Variable {
                                        typ: typ.clone(),
                                        mem,
                                    },
                                });
                                self.memory_offset += mem;
                            }
                        }
                    }
                    _ => panic!(),
                };
            }
            _ => panic!(),
        }
        self.reset_registers();
        Ok(())
    }

    fn convert_stmt_assign(&mut self, assign_tree: &Tree) -> Result<(), String> {
        match &assign_tree.typ {
            TreeType::StmtAssign { name, expression } => {
                let reg = self.convert_expr(expression)?;
                let local_lookup = self
                    .functions
                    .get(&self.current_fn)
                    .expect("At this point, function table is guaranteed to contain current_fn.");
                match &name.typ {
                    TreeType::Name { name } => {
                        let var = match local_lookup.get_variable_location(name) {
                            Some(i) => i,
                            None => panic!(),
                        };
                        match &var.typ {
                            Type::Arr(typ, size) => {
                                todo!()
                            }
                            _ => self.code.push(Instruction::StoreMem { reg: reg.mem, var }),
                        }
                    }
                    TreeType::ExprArrAccess {
                        arr_name,
                        indices,
                        typ,
                    } => {
                        let var = match local_lookup.get_variable_location(arr_name) {
                            Some(i) => i,
                            None => panic!(),
                        };
                        match &typ {
                            Type::Arr(typ, size) => {
                                match &indices.typ {
                                    TreeType::ExprArrLiteral { elements } => {
                                        let index_reg = self.get_register()?;
                                        let mem_offset = self.get_register()?;
                                        let size_reg = self.get_register()?;
                                        // Prepare index register
                                        self.code.push(Instruction::LoadUsize {
                                            dest: index_reg,
                                            val: 0,
                                        });
                                        // Stores final offset for stack addressing
                                        self.code.push(Instruction::LoadUsize {
                                            dest: mem_offset,
                                            val: var.mem,
                                        });

                                        for (count, child) in elements.iter().enumerate() {
                                            let reg = self.convert_expr(child)?;

                                            // Index Out of Bounds check
                                            self.code.push(Instruction::LoadUsize {
                                                dest: size_reg,
                                                val: size[count],
                                            });
                                            self.code.push(Instruction::Cmp {
                                                dest: reg.mem,
                                                src: size_reg,
                                                typ: Type::Usize,
                                            });
                                            let lbl = self.create_label();
                                            self.code
                                                .push(Instruction::JmpLt { dest: lbl.clone() });
                                            self.code.push(Instruction::Exit {
                                                code: ExitCode::OobAccess as usize,
                                            });
                                            self.add_label(&lbl);

                                            // index_reg contains our index
                                            // in each step, multiply the index by the dimension of the array
                                            self.code.push(Instruction::Mul {
                                                dest: index_reg,
                                                src: size_reg,
                                                typ: Type::Usize,
                                            });
                                            // in each step, add to that ^ the position we're indexing (result of expression)
                                            self.code.push(Instruction::Add {
                                                dest: index_reg,
                                                src: reg.mem,
                                                typ: Type::Usize,
                                            });
                                        }
                                        self.code.push(Instruction::Add {
                                            dest: index_reg,
                                            src: mem_offset,
                                            typ: Type::Usize,
                                        });
                                        let reg_tmp = self.get_register()?;
                                        self.code.push(Instruction::LoadPtrRel {
                                            dest: reg_tmp,
                                            src: index_reg,
                                            typ: *typ.clone(),
                                        });
                                        self.code.push(Instruction::StorePtr {
                                            dest: reg_tmp,
                                            src: reg.mem,
                                            typ: *typ.clone(),
                                        });
                                    }
                                    _ => panic!(),
                                }
                            }
                            _ => panic!(),
                        }
                    }
                    _ => {
                        for c in &self.code {
                            println!("{:?}", c);
                        }
                        panic!()
                    },
                }
            }
            _ => panic!(),
        }
        /*
        if assign_name.tkn.get_type() == TokenType::Asterisk {
            let var_name = &assign_name.children[0].tkn;
            assert!(var_name.get_type() == TokenType::Name);

            match var.clone().typ {
                Type::Ptr(t) => {
                    if *t != reg.typ {
                        return Err(format!(
                            "{}: {:?}: Type Mismatch when assigning to Pointer. Pointer expected Type `{:?}`, but got Type `{:?}`.",
                            ERR_STR,
                            assign_name.tkn.get_loc(),
                            t,
                            reg.typ
                        ));
                    }
                    self.resolve_types(*t.clone())?;
                    let reg_mov = self.get_register()?;
                    self.code.push(Instruction::LoadMem { reg: reg_mov, var });
                    self.code.push(Instruction::StorePtr {
                        src: reg.mem,
                        dest: reg_mov,
                        typ: *t,
                    });
                }
                e => {
                    return Err(format!(
                        "{}: {:?}: Can not dereference Type `{:?}`.",
                        ERR_STR,
                        assign_name.tkn.get_loc(),
                        e
                    ))
                }
            }
        } else {
            assert!(assign_name.tkn.get_type() == TokenType::Name);
                (Type::Arr(typ, size), _) => {
                    for (count, child) in indices.iter().enumerate() {
                        let reg = self.convert_expr(child)?;
                        if reg.typ != Type::Unknown && reg.typ != Type::Usize {
                            return Err(format!(
                                "{}: {:?}: Type Mismatch: Array indices are expected to be of Type usize. Got `{:?}`",
                                ERR_STR,
                                child.tkn.get_loc(),
                                reg.typ
                            ));
                        }

                        self.resolve_types(Type::Usize)?;

                        // Index Out of Bounds check
                        self.code.push(Instruction::LoadUsize { dest: size_reg, val: size[count] });
                        self.code.push(Instruction::Cmp { dest: reg.mem, src: size_reg, typ: Type::Usize });
                        self.code.push(Instruction::JmpLt { dest: self.code.len() + 2 });
                        self.code.push(Instruction::Exit { code: ExitCode::OobAccess as usize });

                        // index_reg contains our index
                        // in each step, multiply the index by the dimension of the array
                        self.code.push(Instruction::Mul { dest: index_reg, src: size_reg, typ: Type::Usize });
                        // in each step, add to that ^ the position we're indexing (result of expression)
                        self.code.push(Instruction::Add { dest: index_reg, src: reg.mem, typ: Type::Usize });
                    }
                    self.code.push(Instruction::Add { dest: index_reg, src: mem_offset, typ: Type::Usize });
                    let reg_tmp = self.get_register()?;
                    self.code.push(Instruction::LoadPtrRel {
                        dest: reg_tmp,
                        src: index_reg,
                        typ: *typ.clone(),
                    });
                    self.code.push(Instruction::StorePtr {
                        dest: reg_tmp,
                        src: reg.mem,
                        typ: *typ.clone(),
                    });
                }
                (typ, Type::Unknown) => {
                    self.resolve_types(typ.clone())?;
                    self.code.push(Instruction::StoreMem { reg: reg.mem, var });
                }
                (var_typ, expr_typ) => {
                    if var_typ != expr_typ {
                        return Err(format!(
                            "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
                            ERR_STR,
                            assign_name.tkn.get_loc(),
                            var.typ.clone(),
                            reg.typ.clone()
                        ));
                    }
                    self.code.push(Instruction::StoreMem { reg: reg.mem, var });
                }
            }
        } */
        self.reset_registers();
        Ok(())
    }

    fn convert_stmt_if(&mut self, if_tree: &Tree) -> Result<(), String> {
        match &if_tree.typ {
            TreeType::StmtIf {
                condition,
                if_branch,
                else_branch,
            } => {
                self.convert_expr(condition)?;
                self.reset_registers();

                self.convert_block(if_branch)?;
                let if_jmp = self.resolve_last_jmp();

                if let Some(else_br) = else_branch {
                    self.unresolved_jmp_instr.push_back(self.code.len());
                    self.code.push(Instruction::Jmp {
                        dest: String::new(),
                    });

                    let lbl_name = self.create_label();
                    self.add_label(&lbl_name);
                    self.set_jmp_lbl(if_jmp, lbl_name);

                    self.convert_block(else_br)?;
                    self.resolve_last_jmp();
                }
            }
            _ => panic!(),
        }
        self.reset_registers();
        Ok(())
    }

    fn convert_stmt_return(&mut self, ret_tree: &Tree) -> Result<(), String> {
        match &ret_tree.typ {
            TreeType::StmtReturn { return_value } => {
                self.functions
                    .get_mut(&self.current_fn)
                    .unwrap()
                    .return_scopes
                    .push(self.scope_depth);
                let fn_return_type = self
                    .functions
                    .get(&self.current_fn)
                    .unwrap()
                    .get_return_type();
                if let Some(ret_val) = return_value {
                    let mut reg = self.convert_expr(ret_val)?;
                    
                    self.code.push(Instruction::Move {
                        src: reg.mem,
                        dest: 0,
                    });
                }
                self.code.push(Instruction::Return {
                    from: self.current_fn.clone(),
                });
            }
            _ => panic!(),
        }
        self.reset_registers();
        Ok(())
    }

    fn convert_stmt_expr(&mut self, expr_tree: &Tree) -> Result<(), String> {
        match &expr_tree.typ {
            TreeType::StmtExpr { expression } => {
                self.convert_expr(expression)?;
            }
            _ => panic!(),
        }
        self.reset_registers();
        Ok(())
    }

    fn resolve_last_jmp(&mut self) -> usize {
        assert!(!self.unresolved_jmp_instr.is_empty());
        let ip = self.unresolved_jmp_instr.pop_back().unwrap();

        let lbl_name = self.create_label();
        self.add_label(&lbl_name);

        self.set_jmp_lbl(ip, lbl_name);
        ip
    }

    fn resolve_types(&mut self, expected_type: Type) -> Result<(), String> {
        if expected_type == Type::None {
            todo!()
        }
        while let Some(index) = self.unresolved_typ_instr.pop_back() {
            self.set_load_instr(index, expected_type.clone())?;
        }
        Ok(())
    }

    fn set_jmp_lbl(&mut self, index: usize, dest: String) {
        self.code[index] = match &self.code[index] {
            Instruction::JmpEq { .. } => Instruction::JmpEq { dest },
            Instruction::JmpNeq { .. } => Instruction::JmpNeq { dest },
            Instruction::JmpGt { .. } => Instruction::JmpGt { dest },
            Instruction::JmpGte { .. } => Instruction::JmpGte { dest },
            Instruction::JmpLt { .. } => Instruction::JmpLt { dest },
            Instruction::JmpLte { .. } => Instruction::JmpLte { dest },
            Instruction::Jmp { .. } => Instruction::Jmp { dest },
            e => todo!("{:?}", e),
        };
    }

    fn set_load_instr(&mut self, index: usize, typ: Type) -> Result<(), String> {
        self.code[index] = match &self.code[index] {
            Instruction::LoadUnknown { dest, val, loc } => match typ {
                Type::I32 => {
                    let val = parse_type!(i32, *loc, *val, typ)?;
                    Instruction::LoadI32 { dest: *dest, val }
                }
                Type::I64 => {
                    let val = parse_type!(i64, loc, val, typ)?;
                    Instruction::LoadI64 { dest: *dest, val }
                }
                Type::U32 => {
                    let val = parse_type!(u32, loc, val, typ)?;
                    Instruction::LoadU32 { dest: *dest, val }
                }
                Type::U64 => {
                    let val = parse_type!(u64, loc, val, typ)?;
                    Instruction::LoadU64 { dest: *dest, val }
                }
                Type::Usize => {
                    let val = parse_type!(usize, loc, val, typ)?;
                    Instruction::LoadUsize { dest: *dest, val }
                }
                // Type::Ptr(t, ..) => {
                //     let val = parse_type!(usize, loc, val, t)?;
                //     Instruction::LoadUsize { dest: *dest, val }
                // }
                _ => todo!("{:?}", loc),
            },
            Instruction::Add {
                dest,
                src,
                typ: _typ,
            } => Instruction::Add {
                dest: *dest,
                src: *src,
                typ,
            },
            Instruction::Sub {
                dest,
                src,
                typ: _typ,
            } => Instruction::Sub {
                dest: *dest,
                src: *src,
                typ,
            },
            Instruction::Mul {
                dest,
                src,
                typ: _typ,
            } => Instruction::Mul {
                dest: *dest,
                src: *src,
                typ,
            },
            Instruction::Div {
                dest,
                src,
                typ: _typ,
            } => Instruction::Div {
                dest: *dest,
                src: *src,
                typ,
            },
            Instruction::Cmp {
                dest,
                src,
                typ: _typ,
            } => Instruction::Cmp {
                dest: *dest,
                src: *src,
                typ,
            },
            e => todo!("Handle Instruction {:?}", e),
        };
        Ok(())
    }

    fn convert_fn(&mut self, func: &Tree) -> Result<(), String> {
        match &func.typ {
            TreeType::Func {
                name,
                return_type,
                param,
                block,
            } => {
                if self.functions.contains_key(name) {
                    return Err(format!(
                        "{}: {:?}: Function redefinition",
                        ERR_STR,
                        func.tkn.get_loc()
                    ));
                }
                self.current_fn = name.clone();
                let f = Function::new(self.code.len());
                self.functions.insert(name.clone(), f);

                self.code.push(Instruction::Label { name: name.clone() });
                self.convert_fn_param(param)?;

                if let Some(ret_tree) = return_type {
                    match &ret_tree.typ {
                        TreeType::TypeDecl { typ } => {
                            self.functions.get_mut(name).unwrap().set_return_type(&self.size_manager, typ);
                        }
                        _ => panic!(),
                    }
                }

                self.convert_block(block)?;

                let return_scopes = &self.functions.get(&self.current_fn).unwrap().return_scopes;
                let return_found = return_scopes.contains(&1);
                let return_type = self
                    .functions
                    .get(&self.current_fn)
                    .unwrap()
                    .return_type
                    .clone();
                if name == self.entry_point.as_str() {
                    self.code.push(Instruction::Exit {
                        code: ExitCode::Normal as usize,
                    });
                }
                match (return_found, return_type.typ) {
                    (false, Type::None) => {
                        self.code.push(Instruction::Return {
                            from: self.current_fn.clone(),
                        });
                    }
                    (true, Type::None) => {
                        if self.code.is_empty()
                            || !(*self.code.last().unwrap()
                                == Instruction::Return {
                                    from: self.current_fn.clone(),
                                })
                        {
                            self.code.push(Instruction::Return {
                                from: self.current_fn.clone(),
                            });
                        }
                    }
                    (false, t) => {
                        return Err(
                            format!("{}: Function `{}` is declared to return `{:?}`, but no return statements found.\n{}: There's no Control Flow check yet, so even if it's unreachable because of if-else or anything else, it won't be caught.",
                            ERR_STR,
                            self.current_fn,
                            t,
                            NOTE_STR)
                        );
                    }
                    (true, t) => {
                        if !(*self.code.last().unwrap()
                            == Instruction::Return {
                                from: self.current_fn.clone(),
                            })
                        {
                            return Err(
                                format!("{}: {:?}: Function `{}` is declared to return `{:?}`, expected `return` as last instruction.",
                                ERR_STR,
                                func.tkn.get_loc(),
                                self.current_fn,
                                t
                            ));
                        }
                    }
                }
            }
            _ => panic!(),
        }
        self.current_fn.clear();
        Ok(())
    }

    fn convert_fn_param(&mut self, param: &Tree) -> Result<(), String> {
        let mut reg_ctr = 1;
        match &param.typ {
            TreeType::ParamList { parameters } => {
                for p in parameters {
                    match &p.typ {
                        TreeType::Param { name, typ } => match &typ.typ {
                            TreeType::TypeDecl { typ } => {
                                let local_lookup = self.functions.get_mut(&self.current_fn).expect(
                                        "At this point, function table is guaranteed to contain current_fn.",
                                    );
                                if local_lookup.get_param_location(name).is_some() {
                                    return Err(format!(
                                        "{}: {:?}: Parameter redefinition `{}`",
                                        ERR_STR,
                                        p.tkn.get_loc(),
                                        name
                                    ));
                                }
                                let var = local_lookup.add_param(&self.size_manager, name, typ);
                                self.code.push(Instruction::StoreMem { reg: reg_ctr, var });
                                reg_ctr += 1;
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        }
        Ok(())
    }

    fn convert_block(&mut self, block: &Tree) -> Result<(), String> {
        self.enter_scope();
        match &block.typ {
            TreeType::Block { statements } => {
                for instr in statements {
                    match &instr.typ {
                        TreeType::StmtLet { .. } => self.convert_stmt_let(instr)?,
                        TreeType::StmtAssign { .. } => self.convert_stmt_assign(instr)?,
                        TreeType::StmtIf { .. } => self.convert_stmt_if(instr)?,
                        TreeType::StmtReturn { .. } => self.convert_stmt_return(instr)?,
                        TreeType::StmtExpr { .. } => self.convert_stmt_expr(instr)?,
                        e => todo!("convert {:?} inside block", e),
                    }
                }
            }
            _ => panic!(),
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
            TreeType::File { functions } => {
                for func in functions {
                    self.convert_ast(func)?;
                }
            }
            TreeType::Func { .. } => {
                self.memory_offset = 0;
                self.convert_fn(ast)?;
            }
            _ => todo!("handle other types"),
        }
        Ok(())
    }

    fn generate_code(&mut self) -> Result<(), String> {
        self.convert_ast(&self.ast.clone())
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        // for (i, c) in self.code.iter().enumerate() {
        //     println!("{i:3} -> {c:?}");
        // }
        // todo!();

        if !self.functions.contains_key(&self.entry_point) {
            return Err(format!(
                "{}: Missing entry point - Could not find function {}()",
                ERR_STR, self.entry_point
            ));
        }

        const RETURN_STACK_LIMIT: usize = 4096 * 4096;
        const STACK_SIZE: usize = 1_000_000;

        let mut return_stack = VecDeque::<usize>::new();
        let mut return_values = VecDeque::<Memory>::new();
        let mut stack = vec![Memory { u64: 0 }; STACK_SIZE];
        let mut stack_ptr = STACK_SIZE - 1 - self.get_function_stack_size(&self.entry_point);

        let mut flags = 0;
        const EQ: usize = 1;
        const LT: usize = 2;
        const GT: usize = 4;

        let mut ip = self
            .functions
            .get(&self.entry_point)
            .unwrap() // Can safely unwrap because map is guaranteed to contain entry_point
            .get_ip();

        while ip < self.code.len() {
            // todo!("Interpretation with type system in place.");
            if return_stack.len() > RETURN_STACK_LIMIT {
                return Err(format!(
                    "{}: Recursion Limit reached when interpreting!",
                    ERR_STR
                ));
            }
            let instr = &self.code[ip];
            let mut add_ip = true;
            match instr {
                Instruction::LoadUnknown { .. } => panic!(),
                Instruction::LoadI32 { dest, val } => {
                    self.registers[*dest].i32 = *val;
                }
                Instruction::LoadI64 { dest, val } => {
                    self.registers[*dest].i64 = *val;
                }
                Instruction::LoadU32 { dest, val } => {
                    self.registers[*dest].u32 = *val;
                }
                Instruction::LoadU64 { dest, val } => {
                    self.registers[*dest].u64 = *val;
                }
                Instruction::LoadUsize { dest, val } => {
                    self.registers[*dest].ptr = *val;
                }
                Instruction::LoadPtrRel { dest, src, typ } => {
                    self.registers[*dest].ptr = stack_ptr + unsafe { self.registers[*src].ptr } + 1;
                }
                Instruction::LoadPtr { dest, src, typ } => {
                    self.registers[*dest] = unsafe { stack[self.registers[*src].ptr] };
                }
                Instruction::StorePtr { dest, src, typ } => {
                    stack[unsafe { self.registers[*dest].ptr }] = self.registers[*src];
                }
                Instruction::LoadF32 { .. } => {
                    todo!()
                }
                Instruction::LoadF64 { .. } => {
                    todo!()
                }
                Instruction::Add { dest, src, typ } => {
                    let v1 = self.registers[*dest];
                    let v2 = self.registers[*src];
                    perform_op!(self.registers[*dest], v1, v2, typ, +);
                }
                Instruction::Sub { dest, src, typ } => {
                    let v1 = self.registers[*dest];
                    let v2 = self.registers[*src];
                    perform_op!(self.registers[*dest], v1, v2, typ, -);
                }
                Instruction::Mul { dest, src, typ } => {
                    let v1 = self.registers[*dest];
                    let v2 = self.registers[*src];
                    perform_op!(self.registers[*dest], v1, v2, typ, *);
                }
                Instruction::Div { dest, src, typ } => {
                    let v1 = self.registers[*dest];
                    let v2 = self.registers[*src];
                    perform_op!(self.registers[*dest], v1, v2, typ, /);
                }
                Instruction::Cmp { dest, src, typ } => {
                    let v1 = self.registers[*dest];
                    let v2 = self.registers[*src];
                    flags = 0;
                    flags |= perform_cmp!(v1, v2, typ, ==) as usize * EQ;
                    flags |= perform_cmp!(v1, v2, typ, <) as usize * LT;
                    flags |= perform_cmp!(v1, v2, typ, >) as usize * GT;
                    // >= is flags & EQ || flags & GT
                    // <= is flags & EQ || flags & LT
                    // != is not flags & EQ
                }
                Instruction::JmpEq { dest } => {
                    if flags & EQ != 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::JmpNeq { dest } => {
                    if flags & EQ == 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::JmpGt { dest } => {
                    if flags & GT != 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::JmpGte { dest } => {
                    if flags & EQ != 0 || flags & GT != 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::JmpLt { dest } => {
                    if flags & LT != 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::JmpLte { dest } => {
                    if flags & EQ != 0 || flags & LT != 0 {
                        ip = self.get_label_ip(dest);
                        add_ip = false;
                    }
                }
                Instruction::Jmp { dest } => {
                    ip = self.get_label_ip(dest);
                    add_ip = false;
                }
                Instruction::Move { dest, src } => {
                    self.registers[*dest] = self.registers[*src];
                }
                Instruction::LoadMem {
                    reg,
                    var: Variable { typ, mem },
                } => {
                    self.registers[*reg] = stack[stack_ptr + *mem + 1];
                }
                Instruction::StoreMem {
                    reg,
                    var: Variable { typ, mem },
                } => {
                    stack[stack_ptr + *mem + 1] = self.registers[*reg];
                }
                Instruction::Call { fn_name } => {
                    let stack_size = self.get_function_stack_size(fn_name);
                    // println!("{fn_name} has a stack size of {stack_size}");
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
                Instruction::Push { reg } => {
                    return_values.push_back(self.registers[*reg]);
                }
                Instruction::Pop { reg } => {
                    self.registers[*reg] = return_values.pop_back().unwrap();
                }
                Instruction::Return { .. } => {
                    if return_stack.is_empty() {
                        todo!();
                    }
                    // Can safely unwrap because I just checked return_stack length
                    // Whenever I add something, I add packs of 2 values, so there's never a situation
                    // where only one element is on the return_stack
                    stack_ptr += return_stack.pop_back().unwrap();
                    ip = return_stack.pop_back().unwrap();
                }
                Instruction::Exit { code } => {
                    println!("Program exited with Exit Code {code}.");
                    if *code != 0 {
                        // TODO: Add better runtime error messages
                        ExitCode::print_exit_msg(*code);
                    }
                    break;
                }
                Instruction::Label { .. } => {}
            }
            if add_ip {
                ip += 1;
            }
            if self.print_debug {
                let mut reg_string = String::from("[");
                const DISP_REG: usize = 10;
                for i in 0..=DISP_REG {
                    reg_string.push_str(format!("{:6}", unsafe { self.registers[i].u64 }).as_str());
                    if i < DISP_REG {
                        reg_string.push_str(", ");
                    }
                }
                reg_string.push(']');
                println!("{:4} {:?} {} {:?}", ip, flags, reg_string, instr);
            }
        }

        for i in 0..20 {
            let id = STACK_SIZE - (i + 1) * 4;
            println!("{} {}", id, unsafe {
                stack[id].u32
            });
        }
        Ok(())
    }

    pub fn compile(&mut self) -> Result<(), String> {
        if !self.functions.contains_key(&self.entry_point) {
            return Err(format!(
                "{}: Missing entry point - Could not find function {}()",
                ERR_STR, self.entry_point
            ));
        }

        let mut asm = String::new();
        let mut push_asm = |s: &str| {
            asm.push_str(s.clone());
            asm.push('\n')
        };

        let get_word = |t: &Type| -> &str {
            match t {
                Type::I32 | Type::U32 => "DWORD",
                Type::I64 | Type::U64 | Type::Usize => "QWORD",
                _ => todo!("{:?}", t),
            }
        };

        let get_register = |reg: usize, typ: &Type| -> &str {
            match typ {
                Type::I32 | Type::U32 => REGISTERS_32BIT[reg],
                Type::I64 | Type::U64 | Type::Usize => REGISTERS_64BIT[reg],
                _ => todo!("{:?}", typ),
            }
        };

        push_asm("  ; Generated code for some program - TODO: Find out which one");
        push_asm("default rel");
        push_asm("");

        push_asm("segment .text");
        push_asm("  global main");
        // TODO: Clarify extern stuff - See https://github.com/pfhaupt/bufo/issues/6
        push_asm("  extern ExitProcess");
        push_asm("  extern printf");

        for instr in &self.code {
            if self.print_debug {
                push_asm(format!("; -- {instr:?} --").as_str());
            }
            match instr {
                Instruction::Label { name } => {
                    push_asm(format!("{}:", name).as_str());
                    if let Some(func) = self.functions.get(name) {
                        push_asm("  push rbp");
                        push_asm("  mov rbp, rsp");
                        push_asm(format!("  sub rsp, {}", func.get_stack_size()).as_str());
                    }
                }
                Instruction::Move { dest, src } => {
                    let dest = REGISTERS_64BIT[*dest];
                    let src = REGISTERS_64BIT[*src];
                    push_asm(format!("  mov {dest}, {src}").as_str());
                }
                Instruction::StoreMem { reg, var } => {
                    let word = get_word(&var.typ);
                    let reg = get_register(*reg, &var.typ);
                    let stack_offset = var.mem;
                    let type_size = self.size_manager.get_type_size(&var.typ);
                    push_asm(
                        format!("  mov {word} [rbp-{stack_offset}-{type_size}], {reg}").as_str(),
                    );
                }
                Instruction::LoadMem { reg, var } => {
                    let word = get_word(&var.typ);
                    let reg = get_register(*reg, &var.typ);
                    let stack_offset = var.mem;
                    let type_size = self.size_manager.get_type_size(&var.typ);
                    push_asm(
                        format!("  mov {word} {reg}, [rbp-{stack_offset}-{type_size}]").as_str(),
                    );
                }
                Instruction::LoadUnknown { dest, val, loc } => {
                    panic!("At the point of generating ASM, there should never be a single LoadUnknown!\nFailed to resolve type at: {loc:?}");
                }
                Instruction::LoadU32 { dest, val } => {
                    let reg = get_register(*dest, &Type::U32);
                    push_asm(format!("  mov {reg}, {val}").as_str());
                }
                Instruction::LoadI32 { dest, val } => {
                    let reg = get_register(*dest, &Type::I32);
                    push_asm(format!("  mov {reg}, {val}").as_str());
                }
                Instruction::LoadU64 { dest, val } => {
                    let reg = get_register(*dest, &Type::U64);
                    push_asm(format!("  mov {reg}, {val}").as_str());
                }
                Instruction::LoadI64 { dest, val } => {
                    let reg = get_register(*dest, &Type::I64);
                    push_asm(format!("  mov {reg}, {val}").as_str());
                }
                Instruction::LoadF32 { dest, val } => {
                    todo!()
                }
                Instruction::LoadF64 { dest, val } => {
                    todo!()
                }
                Instruction::LoadUsize { dest, val } => {
                    let reg = get_register(*dest, &Type::Usize);
                    push_asm(format!("  mov {reg}, {val}").as_str());
                }
                Instruction::LoadPtrRel { dest, src, typ } => {
                    // self.registers[*dest].ptr = stack_ptr + unsafe { self.registers[*src].ptr } + 1;
                    let dest = get_register(*dest, &Type::Usize);
                    let src = get_register(*src, &Type::Usize);
                    push_asm(format!("  add {src}, rsp").as_str());
                    push_asm(format!("  mov {dest}, {src}").as_str());
                }
                Instruction::LoadPtr { dest, src, typ } => {
                    let dest = get_register(*dest, &Type::Usize);
                    let src = get_register(*src, &Type::Usize);
                    push_asm(format!("  mov {dest}, [{src}]").as_str());
                }
                Instruction::StorePtr { dest, src, typ } => {
                    let dest = get_register(*dest, &Type::Usize);
                    let src = get_register(*src, &Type::Usize);
                    push_asm(format!("  mov [{dest}], {src}").as_str());
                }
                Instruction::Add { dest, src, typ } => {
                    let dest = get_register(*dest, typ);
                    let src = get_register(*src, typ);
                    match typ {
                        Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::Usize => {
                            push_asm(format!("  add {dest}, {src}").as_str());
                        }
                        e => todo!("Add {:?}", e),
                    }
                }
                Instruction::Sub { dest, src, typ } => {
                    let dest = get_register(*dest, typ);
                    let src = get_register(*src, typ);
                    match typ {
                        Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::Usize => {
                            push_asm(format!("  sub {dest}, {src}").as_str());
                        }
                        e => todo!("Sub {:?}", e),
                    }
                }
                Instruction::Mul { dest, src, typ } => {
                    let dest = get_register(*dest, typ);
                    let src = get_register(*src, typ);
                    match typ {
                        Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::Usize => {
                            push_asm(format!("  imul {dest}, {src}").as_str());
                        }
                        e => todo!("Mul {:?}", e),
                    }
                }
                Instruction::Div { dest, src, typ } => {
                    let dest = get_register(*dest, typ);
                    let src = get_register(*src, typ);
                    match typ {
                        Type::I32 | Type::U32 => {
                            push_asm(format!("  mov eax, {dest}").as_str());
                            push_asm("  cqo");
                            push_asm(format!("  idiv {src}").as_str());
                            push_asm(format!("  mov {dest}, eax").as_str());
                        }
                        Type::I64 | Type::U64 | Type::Usize => {
                            push_asm(format!("  mov rax, {dest}").as_str());
                            push_asm("  cqo");
                            push_asm(format!("  idiv {src}").as_str());
                            push_asm(format!("  mov {dest}, rax").as_str());
                        }
                        e => todo!("Div {:?}", e),
                    }
                }
                Instruction::Cmp { dest, src, typ } => {
                    let dest = get_register(*dest, typ);
                    let src = get_register(*src, typ);
                    match typ {
                        Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::Usize => {
                            push_asm(format!("  cmp {dest}, {src}").as_str());
                        }
                        e => todo!("Cmp {:?}", e),
                    }
                }
                Instruction::Jmp { dest } => {
                    push_asm(format!("  jmp {dest}").as_str());
                }
                Instruction::JmpEq { dest } => {
                    push_asm(format!("  je {dest}").as_str());
                }
                Instruction::JmpNeq { dest } => {
                    push_asm(format!("  jne {dest}").as_str());
                }
                Instruction::JmpGt { dest } => {
                    push_asm(format!("  jg {dest}").as_str());
                }
                Instruction::JmpGte { dest } => {
                    push_asm(format!("  jge {dest}").as_str());
                }
                Instruction::JmpLt { dest } => {
                    push_asm(format!("  jl {dest}").as_str());
                }
                Instruction::JmpLte { dest } => {
                    push_asm(format!("  jle {dest}").as_str());
                }
                Instruction::Push { reg } => {
                    let reg = REGISTERS_64BIT[*reg];
                    push_asm(format!("  push {reg}").as_str());
                }
                Instruction::Pop { reg } => {
                    let reg = REGISTERS_64BIT[*reg];
                    push_asm(format!("  pop {reg}").as_str());
                }
                Instruction::Call { fn_name } => {
                    push_asm(format!("  call {fn_name}").as_str());
                }
                Instruction::Return { from } => {
                    if let Some(func) = self.functions.get(from) {
                        push_asm("  mov rsp, rbp");
                        push_asm("  pop rbp");
                    } else {
                        panic!();
                    }
                    push_asm("  ret");
                }
                Instruction::Exit { code } => {
                    push_asm(format!("  mov rcx, {code}").as_str());
                    push_asm("  call ExitProcess");
                }
            }
        }
        push_asm("");
        push_asm("segment .data");
        push_asm("");
        push_asm("segment .bss");
        match Path::new("./out/").try_exists() {
            Ok(b) => {
                if !b {
                    // Create folder
                    if self.print_debug {
                        println!("Could not find output folder, creating now");
                    }
                    std::fs::create_dir(Path::new("./out/"));
                }
            }
            Err(e) => panic!("{}", e),
        }
        if self.print_debug {
            println!("Writing to ./out/output.asm");
        }
        match File::create("./out/output.asm") {
            Ok(mut file) => {
                file.write_all(asm.as_bytes());
            }
            Err(e) => panic!("{}", e),
        }
        if self.print_debug {
            println!("Running `nasm -f win64 ./out/output.asm -o ./out/output.obj`");
        }
        Command::new("nasm")
            .args(["-f", "win64", "./out/output.asm", "-o", "./out/output.obj"])
            .output()
            .expect("failed to execute process");
        if self.print_debug {
            println!(
                "Running `golink /no /console /entry main output.obj MSVCRT.dll kernel32.dll`"
            );
        }
        Command::new("golink")
            .args([
                "/no",
                "/console",
                "/entry",
                "main",
                "./out/output.obj",
                "MSVCRT.dll",
                "kernel32.dll",
            ])
            .output()
            .expect("failed to execute process");
        Ok(())
    }

    pub fn run(&self) -> Result<(), String> {
        if self.print_debug {
            println!("Running `./out/output.exe`");
        }
        Command::new("./out/output.exe")
            .output()
            .expect("failed to execute process");
        Ok(())
    }
}
