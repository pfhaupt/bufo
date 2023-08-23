// use std::cmp::Ordering;
// use std::collections::{BTreeMap, HashMap, VecDeque};

// use crate::lexer::{Location, TokenType};
// use crate::parser::{Tree, TreeType};

// pub const RUNTIME_ERR: &str = "\x1b[91mRuntime Exception\x1b[0m";
pub const ERR_STR: &str = "\x1b[91merror\x1b[0m";
// #[allow(unused)]
// pub const WARN_STR: &str = "\x1b[93mwarning\x1b[0m";
pub const NOTE_STR: &str = "\x1b[92mnote\x1b[0m";

// macro_rules! perform_op {
//     ($dest: expr, $reg1:expr, $reg2:expr, $typ:expr, $op:tt) => {
//         match $typ {
//             Type::I32 => unsafe { $dest.i32 = $reg1.i32 $op $reg2.i32 },
//             Type::I64 => unsafe { $dest.i64 = $reg1.i64 $op $reg2.i64 },
//             Type::U32 => unsafe { $dest.u32 = $reg1.u32 $op $reg2.u32 },
//             Type::U64 => unsafe { $dest.u64 = $reg1.u64 $op $reg2.u64 },
//             Type::Usize => unsafe { $dest.ptr = $reg1.ptr $op $reg2.ptr },
//             Type::F32 => unsafe { $dest.f32 = $reg1.f32 $op $reg2.f32 },
//             Type::F64 => unsafe { $dest.f64 = $reg1.f64 $op $reg2.f64 },
//             Type::Ptr(..) => unsafe { $dest.ptr = $reg1.ptr $op $reg2.ptr },
//             _ => todo!()
//         }
//     };
// }

// macro_rules! perform_cmp {
//     ($reg1:expr, $reg2:expr, $typ:expr, $op:tt) => {
//         match $typ {
//             Type::I32 => unsafe { $reg1.i32 $op $reg2.i32 },
//             Type::I64 => unsafe { $reg1.i64 $op $reg2.i64 },
//             Type::U32 => unsafe { $reg1.u32 $op $reg2.u32 },
//             Type::U64 => unsafe { $reg1.u64 $op $reg2.u64 },
//             Type::F32 => unsafe { $reg1.f32 $op $reg2.f32 },
//             Type::F64 => unsafe { $reg1.f64 $op $reg2.f64 },
//             Type::Usize => unsafe { $reg1.ptr $op $reg2.ptr },
//             _ => todo!()
//         }
//     };
// }

// // parse_type!(i32, loc, val, typ);
// macro_rules! parse_type {
//     ($parse_typ:ty, $loc:expr, $val:expr, $typ: expr) => {
//         match $val.parse::<$parse_typ>() {
//             Ok(val) => Ok(val),
//             Err(_) => Err(format!(
//                 "{}: {:?}: Integer Literal `{}` too big for Type `{:?}`",
//                 ERR_STR, $loc, $val, $typ
//             )),
//         }
//     };
// }

// #[derive(Copy, Clone)]
// union Memory {
//     i32: i32,
//     i64: i64,
//     u32: u32,
//     u64: u64,
//     f32: f32,
//     f64: f64,
//     ptr: usize,
// }

// impl std::fmt::Display for Memory {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
//         write!(f, "{}", unsafe { self.u64 })
//     }
// }

// impl std::fmt::Debug for Memory {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
//         write!(f, "{}", unsafe { self.u64 })
//     }
// }

// #[derive(Debug, PartialEq, Clone)]
// enum Type {
//     None, // For functions that return nothing
//     Unknown,
//     I32,
//     I64,
//     U32,
//     U64,
//     Usize,
//     Bool,
//     Ptr(Box<Type>),
//     Arr(Box<Type>, Vec<usize>),
//     // Reserved for later use
//     F32,
//     F64,
// }

// #[derive(Debug, Clone, PartialEq)]
// struct Variable {
//     typ: Type,
//     mem: usize,
// }

// #[allow(unused)]
// #[derive(Debug, PartialEq)]
// enum Instruction {
//     LoadUnknown {
//         dest: usize,
//         val: String,
//         loc: Location,
//     },
//     LoadI32 {
//         dest: usize,
//         val: i32,
//     },
//     LoadI64 {
//         dest: usize,
//         val: i64,
//     },
//     LoadU32 {
//         dest: usize,
//         val: u32,
//     },
//     LoadU64 {
//         dest: usize,
//         val: u64,
//     },
//     LoadUsize {
//         dest: usize,
//         val: usize,
//     },
//     LoadPtrRel {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     LoadPtr {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     StorePtr {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     LoadF32 {
//         dest: usize,
//         val: f32,
//     },
//     LoadF64 {
//         dest: usize,
//         val: f64,
//     },
//     Add {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     Sub {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     Mul {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     Div {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     Cmp {
//         dest: usize,
//         src: usize,
//         typ: Type,
//     },
//     Jmp {
//         dest: usize,
//     },
//     JmpEq {
//         dest: usize,
//     },
//     JmpNeq {
//         dest: usize,
//     },
//     JmpGt {
//         dest: usize,
//     },
//     JmpGte {
//         dest: usize,
//     },
//     JmpLt {
//         dest: usize,
//     },
//     JmpLte {
//         dest: usize,
//     },
//     Move {
//         dest: usize,
//         src: usize,
//     },
//     LoadMem {
//         reg: usize,
//         var: Variable,
//     },
//     StoreMem {
//         reg: usize,
//         var: Variable,
//     },
//     Call {
//         fn_name: String,
//     },
//     Push {
//         reg: usize,
//     },
//     Pop {
//         reg: usize,
//     },
//     Return {},
//     Exit { code: usize},
//     // Print { src: usize },
// }

// enum ExitCode {
//     Normal,
//     OobAccess,
// }

// impl ExitCode {
//     fn print_exit_msg(code: usize) {
//         match code {
//             0 => unreachable!(),
//             1 => println!("{}: Attempted to index out of bounds!", RUNTIME_ERR),
//             _ => unreachable!()
//         }
//     }
// }

// #[derive(Debug)]
// struct Function {
//     ip: usize,
//     param_variables: BTreeMap<String, Variable>,
//     local_variables: Vec<BTreeMap<String, Variable>>,
//     return_scopes: Vec<usize>,
//     return_type: Type,
//     stack_size: usize,
// }

// impl Function {
//     fn new(ip: usize) -> Self {
//         Self {
//             ip,
//             param_variables: BTreeMap::new(),
//             local_variables: vec![BTreeMap::new()],
//             return_scopes: vec![],
//             return_type: Type::None,
//             stack_size: 0,
//         }
//     }

//     fn get_ip(&self) -> usize {
//         self.ip
//     }

//     fn get_stack_size(&self) -> usize {
//         self.stack_size
//     }

//     fn get_params(&self) -> &BTreeMap<String, Variable> {
//         &self.param_variables
//     }

//     fn get_param_location(&self, var_name: &String) -> Option<Variable> {
//         self.param_variables.get(var_name).cloned()
//     }

//     fn get_local_location(&self, var_name: &String) -> Option<Variable> {
//         for scope in (0..self.local_variables.len()).rev() {
//             if let Some(mem) = self.local_variables.get(scope).unwrap().get(var_name) {
//                 return Some(mem.clone());
//             }
//         }
//         None
//     }

//     fn get_scope_location(&self, var_name: &String) -> Option<Variable> {
//         self.local_variables.last().unwrap().get(var_name).cloned()
//     }

//     fn get_variable_location(&self, var_name: &String) -> Option<Variable> {
//         match self.get_param_location(var_name) {
//             Some(i) => Some(i),
//             None => self.get_local_location(var_name),
//         }
//     }

//     fn add_local_variable(&mut self, var_name: String, mem: usize, scope_depth: usize, typ: &Type) {
//         let scope_var = self.local_variables.get_mut(scope_depth).unwrap();
//         scope_var.insert(
//             var_name,
//             Variable {
//                 mem,
//                 typ: typ.clone(),
//             },
//         );
//         self.stack_size += match typ {
//             Type::Arr(_, size) => size.iter().product(),
//             _ => 1,
//         };
//     }

//     fn add_param(&mut self, param_name: &str, typ: &Type) -> Variable {
//         let mem = self.get_stack_size();
//         let var = Variable {
//             mem,
//             typ: typ.clone(),
//         };
//         self.param_variables
//             .insert(param_name.to_owned(), var.clone());
//         self.stack_size += 1;
//         var.clone()
//     }

//     fn add_scope(&mut self) {
//         self.local_variables.push(BTreeMap::new());
//     }

//     fn remove_scope(&mut self) {
//         self.local_variables.pop().unwrap();
//     }

//     fn set_return_type(&mut self, typ: &Type) {
//         if self.return_type != Type::None {
//             panic!()
//         }
//         self.return_type = typ.clone();
//     }

//     fn get_return_type(&self) -> Type {
//         self.return_type.clone()
//     }
// }

// #[derive(Debug)]
// pub struct Generator {
//     ast: Tree,
//     registers: Vec<Memory>,
//     register_ctr: usize,
//     functions: HashMap<String, Function>,
//     code: Vec<Instruction>,
//     current_fn: String,
//     unresolved_jmp_instr: VecDeque<usize>,
//     unresolved_typ_instr: VecDeque<usize>,
//     scope_depth: usize,
//     print_debug: bool
// }

// impl Generator {
//     pub fn new(ast: Tree, print_debug: bool) -> Result<Self, String> {
//         let mut gen = Self {
//             ast,
//             registers: vec![Memory { u64: 0 }; 100],
//             register_ctr: 0,
//             functions: HashMap::new(),
//             code: vec![],
//             current_fn: String::new(),
//             unresolved_jmp_instr: VecDeque::new(),
//             unresolved_typ_instr: VecDeque::new(),
//             scope_depth: 0,
//             print_debug
//         };
//         gen.generate_code()?;
//         Ok(gen)
//     }

//     fn add_local_var(&mut self, var_name: &str, scope_depth: usize, typ: &Type) -> usize {
//         assert!(!self.current_fn.is_empty());
//         assert!(self.functions.contains_key(&self.current_fn));
//         let mem = self.get_stack_offset();
//         self.functions
//             .get_mut(&self.current_fn)
//             .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
//             .add_local_variable(var_name.to_owned(), mem, scope_depth, typ);
//         mem
//     }

//     fn get_stack_offset(&self) -> usize {
//         assert!(!self.current_fn.is_empty());
//         self.functions
//             .get(&self.current_fn)
//             .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
//             .get_stack_size()
//     }

//     fn get_function_stack_size(&self, func_name: &String) -> usize {
//         assert!(self.functions.contains_key(func_name));
//         self.functions
//             .get(func_name)
//             .expect("Function table does not contain current_fn! This might be a bug in Codegen.")
//             .get_stack_size()
//     }

//     fn reset_registers(&mut self) {
//         self.register_ctr = 0;
//     }

//     fn get_register(&mut self) -> usize {
//         let r = self.register_ctr;
//         if r >= self.registers.len() {
//             self.registers.push(Memory { u64: 0 });
//         }
//         self.register_ctr += 1;
//         r
//     }

//     fn convert_expr_literal(&mut self, lit: String) -> Result<(String, Type), String> {
//         match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
//             Some(index) => {
//                 let typ_str = &lit[index..];
//                 let typ = self.convert_type_str(typ_str)?;
//                 Ok((lit[0..index].to_owned(), typ))
//             }
//             None => Ok((lit, Type::Unknown)),
//         }
//     }

//     fn convert_expr_atomic(&mut self, instr: &Tree) -> Result<Variable, String> {
//         let instr_children = &instr.children;
//         match &instr.typ {
//             TreeType::ExprLiteral {..} => {
//                 assert!(instr_children.len() == 1, "Expected {{Literal}}");
//                 let val = instr_children[0].tkn.get_value();
//                 let (val, typ) = match self.convert_expr_literal(val) {
//                     Ok((v, t)) => (v, t),
//                     Err(e) => {
//                         return Err(format!(
//                             "{}: {:?}: {}",
//                             ERR_STR,
//                             instr_children[0].tkn.get_loc(),
//                             e
//                         ));
//                     }
//                 };
//                 let dest = self.get_register();
//                 let loc = instr_children[0].tkn.get_loc();
//                 match typ {
//                     Type::I32 => {
//                         let val = parse_type!(i32, loc, val, typ)?;
//                         self.code.push(Instruction::LoadI32 { dest, val });
//                     }
//                     Type::I64 => {
//                         let val = parse_type!(i64, loc, val, typ)?;
//                         self.code.push(Instruction::LoadI64 { dest, val });
//                     }
//                     Type::U32 => {
//                         let val = parse_type!(u32, loc, val, typ)?;
//                         self.code.push(Instruction::LoadU32 { dest, val });
//                     }
//                     Type::U64 => {
//                         let val = parse_type!(u64, loc, val, typ)?;
//                         self.code.push(Instruction::LoadU64 { dest, val });
//                     }
//                     Type::Usize => {
//                         let val = parse_type!(usize, loc, val, typ)?;
//                         self.code.push(Instruction::LoadUsize { dest, val });
//                     }
//                     Type::Unknown => {
//                         self.unresolved_typ_instr.push_back(self.code.len());
//                         self.code.push(Instruction::LoadUnknown { dest, val, loc });
//                     }
//                     _ => todo!(),
//                 };
//                 Ok(Variable { mem: dest, typ })
//             }
//             TreeType::ExprParen => {
//                 assert!(instr_children.len() == 1, "Expected {{Expr}}");
//                 self.convert_expr_atomic(&instr_children[0])
//             }
//             TreeType::ExprBinary => {
//                 assert!(instr.typ == TreeType::ExprBinary);
//                 assert!(
//                     instr_children.len() == 2,
//                     "Expected {{Expr}} {{Op}} {{Expr}}"
//                 );
//                 let op = instr;
//                 let lhs = &instr_children[0];
//                 let rhs = &instr_children[1];
//                 let dest = self.convert_expr_atomic(lhs)?;
//                 let src = self.convert_expr_atomic(rhs)?;
//                 let typ = match (dest.typ, src.typ) {
//                     (Type::Unknown, Type::Unknown) => Type::Unknown,
//                     (Type::Unknown, other_type) | (other_type, Type::Unknown) => {
//                         self.resolve_types(other_type.clone())?;
//                         other_type.clone()
//                     }
//                     (left_type, right_type) => {
//                         if right_type != left_type {
//                             return Err(
//                                 format!("{}: {:?}: Type mismatch for binary expression `{}`! Left hand side got type `{:?}`, right hand side got type `{:?}`.",
//                                 ERR_STR,
//                                 op.tkn.get_loc(),
//                                 op.tkn.get_value(),
//                                 left_type,
//                                 right_type));
//                         }
//                         right_type
//                     }
//                 };
//                 let dest = dest.mem;
//                 let src = src.mem;
//                 if typ == Type::Unknown {
//                     self.unresolved_typ_instr.push_back(self.code.len())
//                 }
//                 let mut is_cmp = false;
//                 match op.tkn.get_type() {
//                     TokenType::Plus => self.code.push(Instruction::Add {
//                         dest,
//                         src,
//                         typ: typ.clone(),
//                     }),
//                     TokenType::Minus => self.code.push(Instruction::Sub {
//                         dest,
//                         src,
//                         typ: typ.clone(),
//                     }),
//                     TokenType::Asterisk => self.code.push(Instruction::Mul {
//                         dest,
//                         src,
//                         typ: typ.clone(),
//                     }),
//                     TokenType::ForwardSlash => self.code.push(Instruction::Div {
//                         dest,
//                         src,
//                         typ: typ.clone(),
//                     }),
//                     TokenType::CmpEq => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpNeq { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     TokenType::CmpNeq => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpEq { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     TokenType::CmpGt => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpLte { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     TokenType::CmpGte => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpLt { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     TokenType::CmpLt => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpGte { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     TokenType::CmpLte => {
//                         self.code.push(Instruction::Cmp {
//                             dest,
//                             src,
//                             typ: typ.clone(),
//                         });
//                         self.unresolved_jmp_instr.push_back(self.code.len());
//                         self.code.push(Instruction::JmpGt { dest: usize::MAX });
//                         is_cmp = true;
//                     }
//                     e => todo!("Handle {:?} in convert_expr_atomic()", e),
//                 }
//                 if is_cmp { Ok(Variable { typ: Type::Bool, mem: dest }) }
//                 else { Ok(Variable { typ, mem: dest }) }
//             }
//             TreeType::ExprArrLiteral => Err(format!(
//                 "{}: {:?}: Unexpected Array Literal",
//                 ERR_STR,
//                 instr.tkn.get_loc()
//             )),
//             TreeType::ExprArrAccess => {
//                 let instr_children = &instr.children;
//                 let name_tkn = &instr.tkn;
//                 assert!(name_tkn.get_type() == TokenType::Name);
//                 let name = name_tkn.get_value();
//                 let arr_literal = &instr_children[0];
//                 let function_stack = self
//                     .functions
//                     .get(&self.current_fn)
//                     .expect("At this point, function lookup is guaranteed to contain current_fn");
//                 match function_stack.get_variable_location(&name) {
//                     Some(variable) => {
//                         match variable.typ {
//                             Type::Arr(typ, size) => {
//                                 let indices = &arr_literal.children;
//                                 match indices.len().cmp(&size.len()) {
//                                     Ordering::Greater => {
//                                         return Err(format!(
//                                             "{}: {:?}: Attempted to index array with too many indices! Expected {} indices, got {}.",
//                                             ERR_STR,
//                                             arr_literal.tkn.get_loc(),
//                                             size.len(),
//                                             indices.len()
//                                         ));
//                                     }
//                                     Ordering::Less => {
//                                         return Err(format!(
//                                             "{}: {:?}: Attempted to index array with too few indices! Expected {} indices, got {}.",
//                                             ERR_STR,
//                                             arr_literal.tkn.get_loc(),
//                                             size.len(),
//                                             indices.len()
//                                         ));
//                                     }
//                                     _ => ()
//                                 }
//                                 let index_reg = self.get_register();
//                                 let mem_offset = self.get_register();
//                                 let size_reg = self.get_register();
//                                 // Prepare index register
//                                 self.code.push(Instruction::LoadUsize {
//                                     dest: index_reg,
//                                     val: 0,
//                                 });
//                                 // Stores final offset for stack addressing
//                                 self.code.push(Instruction::LoadUsize {
//                                     dest: mem_offset,
//                                     val: variable.mem,
//                                 });
//                                 for (count, child) in indices.iter().enumerate() {
//                                     let reg = self.convert_expr(child)?;
//                                     if reg.typ != Type::Unknown && reg.typ != Type::Usize {
//                                         return Err(format!(
//                                             "{}: {:?}: Type Mismatch: Array indices are expected to be of Type usize. Got `{:?}`",
//                                             ERR_STR,
//                                             child.tkn.get_loc(),
//                                             reg.typ
//                                         ));
//                                     }

//                                     self.resolve_types(Type::Usize)?;
                                    

//                                     // Index Out of Bounds check
//                                     self.code.push(Instruction::LoadUsize { dest: size_reg, val: size[count] });
//                                     self.code.push(Instruction::Cmp { dest: reg.mem, src: size_reg, typ: Type::Usize });
//                                     self.code.push(Instruction::JmpLt { dest: self.code.len() + 2 });
//                                     self.code.push(Instruction::Exit { code: ExitCode::OobAccess as usize });

//                                     // index_reg contains our index
//                                     // in each step, multiply the index by the dimension of the array
//                                     self.code.push(Instruction::Mul { dest: index_reg, src: size_reg, typ: Type::Usize });
//                                     // in each step, add to that ^ the position we're indexing (result of expression)
//                                     self.code.push(Instruction::Add { dest: index_reg, src: reg.mem, typ: Type::Usize });
//                                 }
                                
//                                 self.code.push(Instruction::Add { dest: index_reg, src: mem_offset, typ: Type::Usize });
//                                 let reg_tmp = self.get_register();
//                                 self.code.push(Instruction::LoadPtrRel {
//                                     dest: reg_tmp,
//                                     src: index_reg,
//                                     typ: *typ.clone(),
//                                 });
//                                 self.code.push(Instruction::LoadPtr {
//                                     dest: reg_tmp,
//                                     src: reg_tmp,
//                                     typ: *typ.clone(),
//                                 });
//                                 Ok(Variable {
//                                     typ: *typ.clone(),
//                                     mem: reg_tmp,
//                                 })
//                             }
//                             _ => Err(format!(
//                                 "{}: {:?}: Indexed variable `{}` is not an Array.",
//                                 ERR_STR,
//                                 name_tkn.get_loc(),
//                                 name
//                             )),
//                         }
//                     }
//                     None => Err(format!(
//                         "{}: {:?}: Undefined variable `{}`",
//                         ERR_STR,
//                         name_tkn.get_loc(),
//                         name
//                     )),
//                 }
//             }
//             TreeType::ExprName => {
//                 assert!(instr_children.len() == 1, "Expected {{name}}");
//                 let val_name = &instr_children[0];
//                 let name = val_name.tkn.get_value();
//                 let reg = self.get_register();
//                 let curr_fn = self
//                     .functions
//                     .get(&self.current_fn)
//                     .expect("At this point, function table is guaranteed to contain current_fn.");
//                 match curr_fn.get_variable_location(&name) {
//                     Some(var) => {
//                         self.code.push(Instruction::LoadMem {
//                             reg,
//                             var: Variable {
//                                 typ: var.typ.clone(),
//                                 mem: var.mem,
//                             },
//                         });
//                         Ok(Variable {
//                             mem: reg,
//                             typ: var.typ.clone(),
//                         })
//                     }
//                     None => Err(format!(
//                         "{}: {:?}: Undefined variable `{}`",
//                         ERR_STR,
//                         val_name.tkn.get_loc(),
//                         name
//                     )),
//                 }
//             }
//             TreeType::ExprCall => {
//                 assert!(instr_children.len() == 1, "Expected {{name}} {{ArgList}}");
//                 assert!(instr.tkn.get_type() == TokenType::Name);
//                 let fn_name = instr.tkn.get_value();
//                 let fn_args = &instr_children[0];
//                 if let Some(func) = self.functions.get(&fn_name) {
//                     let fn_return_type = func.get_return_type();
//                     let params = func.get_params();
//                     let mut params_as_list = params.clone();

//                     match fn_args.children.len().cmp(&params.len()) {
//                         std::cmp::Ordering::Less => Err(format!(
//                             "{}: {:?}: Too few arguments specified for function call. Expected {} arguments, got {}.",
//                             ERR_STR,
//                             instr.tkn.get_loc(),
//                             params.len(),
//                             fn_args.children.len()
//                         )),
//                         std::cmp::Ordering::Greater => Err(format!(
//                             "{}: {:?}: Too many arguments specified for function call. Expected {} arguments, got {}.",
//                             ERR_STR,
//                             instr.tkn.get_loc(),
//                             params.len(),
//                             fn_args.children.len()
//                         )),
//                         _ => {
//                             let curr_reg_ctr = self.register_ctr;
//                             for reg in 0..curr_reg_ctr {
//                                 self.code.push(Instruction::Push { reg });
//                             }
//                             for (reg_ctr, arg) in fn_args.children.iter().enumerate() {
//                                 let reg = self.convert_arg(arg)?;
//                                 let param = params_as_list.pop_first().unwrap();
//                                 let param_type = param.1;
//                                 if reg.typ == Type::Unknown {
//                                     self.resolve_types(param_type.typ)?;
//                                 } else if reg.typ != param_type.typ {
//                                     return Err(format!("{}: {:?}: Type mismatch in Function Call. Argument {} is expected to be type `{:?}`, found `{:?}`.",
//                                         ERR_STR,
//                                         instr.tkn.get_loc(),
//                                         reg_ctr + 1,
//                                         param_type.typ,
//                                         reg.typ)
//                                     );
//                                 }
//                                 self.code.push(Instruction::Move {
//                                     dest: reg_ctr,
//                                     src: reg.mem,
//                                 });
//                             }
//                             self.code.push(Instruction::Call { fn_name });
//                             let reg = self.get_register();
//                             if fn_return_type != Type::None {
//                                 self.code.push(Instruction::Pop { reg });
//                             }
//                             for reg in (0..curr_reg_ctr).rev() {
//                                 self.code.push(Instruction::Pop { reg });
//                             }
//                             Ok(Variable {
//                                 typ: fn_return_type,
//                                 mem: reg,
//                             })
//                         }
//                     }
//                 } else {
//                     Err(format!(
//                         "{} {:?}: Unknown function `{}`",
//                         ERR_STR,
//                         instr.tkn.get_loc(),
//                         fn_name
//                     ))
//                 }
//             }
//             TreeType::Pointer => {
//                 let val_name = &instr_children[0];
//                 let name = val_name.tkn.get_value();
//                 let reg = self.get_register();
//                 let curr_fn = self
//                     .functions
//                     .get(&self.current_fn)
//                     .expect("At this point, function table is guaranteed to contain current_fn.");
//                 match curr_fn.get_variable_location(&name) {
//                     Some(mem) => {
//                         let reg_tmp = self.get_register();
//                         self.code.push(Instruction::LoadUsize {
//                             dest: reg_tmp,
//                             val: mem.mem,
//                         });
//                         self.code.push(Instruction::LoadPtrRel {
//                             dest: reg,
//                             src: reg_tmp,
//                             typ: mem.typ.clone(),
//                         });
//                         Ok(Variable {
//                             typ: Type::Ptr(Box::new(mem.typ)),
//                             mem: reg,
//                         })
//                     }
//                     None => Err(format!(
//                         "{}: {:?}: Undefined variable `{}`",
//                         ERR_STR,
//                         val_name.tkn.get_loc(),
//                         name
//                     )),
//                 }
//             }
//             TreeType::Deref => {
//                 let val_name = &instr_children[0];
//                 let name = val_name.tkn.get_value();
//                 let reg = self.get_register();
//                 let curr_fn = self
//                     .functions
//                     .get(&self.current_fn)
//                     .expect("At this point, function table is guaranteed to contain current_fn.");
//                 match curr_fn.get_variable_location(&name) {
//                     Some(mem) => match mem.clone().typ {
//                         Type::Ptr(t) => {
//                             let address = self.get_register();
//                             self.code.push(Instruction::LoadMem {
//                                 reg: address,
//                                 var: mem,
//                             });
//                             self.code.push(Instruction::LoadPtr {
//                                 dest: reg,
//                                 src: address,
//                                 typ: *t.clone(),
//                             });
//                             Ok(Variable {
//                                 typ: *t.clone(),
//                                 mem: reg,
//                             })
//                         }
//                         _ => panic!(),
//                     },
//                     None => {
//                         let fp_str = if self.functions.contains_key(&name) {
//                             format!("\n{}: Function pointers are not supported yet.", NOTE_STR)
//                         } else {
//                             "".to_string()
//                         };
//                         Err(format!(
//                             "{}: {:?}: Undefined variable `{}`.{}",
//                             ERR_STR,
//                             val_name.tkn.get_loc(),
//                             name,
//                             fp_str
//                         ))
//                     }
//                 }
//             }
//             e => {
//                 println!(
//                     "{}: Unreachable Error when generating code! Could not convert:",
//                     ERR_STR
//                 );
//                 todo!("Handle {:?} in convert_expr_atomic!", e)
//             }
//         }
//     }

//     fn convert_expr(&mut self, expr_tree: &Tree) -> Result<Variable, String> {
//         match &expr_tree.typ {
//             TreeType::ExprLiteral {..}
//             | TreeType::ExprBinary
//             | TreeType::ExprName
//             | TreeType::Pointer
//             | TreeType::Deref
//             | TreeType::ExprArrAccess
//             | TreeType::ExprArrLiteral
//             | TreeType::ExprParen
//             | TreeType::ExprCall => self.convert_expr_atomic(expr_tree),
//             e => {
//                 println!(
//                     "{}: {:?}: Unreachable Error when generating code! Could not convert:",
//                     ERR_STR,
//                     expr_tree.tkn.get_loc()
//                 );
//                 expr_tree.print_debug();
//                 todo!("Handle {:?} in convert_expr!", e)
//             }
//         }
//     }

//     fn convert_type_str(&self, val: &str) -> Result<Type, String> {
//         match val {
//             "i32" => Ok(Type::I32),
//             "i64" => Ok(Type::I64),
//             "u32" => Ok(Type::U32),
//             "u64" => Ok(Type::U64),
//             "usize" => Ok(Type::Usize),
//             // Reserved for future use
//             "f32" => Ok(Type::F32),
//             "f64" => Ok(Type::F64),
//             t => Err(format!(
//                 "Unexpected Type `{}`. Expected one of {{i32, i64, u32, u64}}",
//                 t
//             )),
//         }
//     }

//     fn convert_type(&mut self, typ_tree: &Tree) -> Result<Type, String> {
//         let typ_children = &typ_tree.children;
//         assert!(typ_tree.tkn.get_type() == TokenType::TypeDecl);
//         assert!(typ_children.len() == 1);
//         let typ_type = &typ_children[0];
//         if typ_type.typ == TreeType::Pointer {
//             let actual_type = &typ_type.children[0];
//             if !actual_type.children.is_empty() && actual_type.children[0].typ == TreeType::ArrSize
//             {
//                 return Err(format!(
//                     "{}: {:?}: Can not declare pointer to an Array.",
//                     ERR_STR,
//                     actual_type.tkn.get_loc()
//                 ));
//             }
//             let actual_type_name = actual_type.tkn.get_value();
//             match self.convert_type_str(&actual_type_name) {
//                 Ok(t) => return Ok(Type::Ptr(Box::new(t))),
//                 Err(e) => {
//                     return Err(format!(
//                         "{}: {:?}: {}",
//                         ERR_STR,
//                         actual_type.tkn.get_loc(),
//                         e
//                     ))
//                 }
//             }
//         }
//         assert!(typ_type.tkn.get_type() == TokenType::Name);
//         let typ_type_value = typ_type.tkn.get_value();
//         let typ_size = if typ_type.children.is_empty() {
//             vec![]
//         } else {
//             let mut size = vec![];
//             for c in &typ_type.children[0].children {
//                 assert!(c.typ == TreeType::ArrSize);
//                 let arr_size = c
//                     .tkn
//                     .get_value()
//                     .parse::<usize>()
//                     .unwrap_or_else(|e| panic!("{}", e));
//                 if arr_size == 0 {
//                     return Err(format!(
//                         "{}: {:?}: Attempted to create array with a dimension of size 0!",
//                         ERR_STR,
//                         c.tkn.get_loc()
//                     ));
//                 }
//                 size.push(arr_size);
//             }
//             size
//         };
//         match self.convert_type_str(typ_type_value.as_str()) {
//             Ok(t) => {
//                 if typ_size.is_empty() {
//                     Ok(t)
//                 } else {
//                     Ok(Type::Arr(Box::new(t), typ_size))
//                 }
//             }
//             Err(e) => Err(format!("{}: {:?}: {}", ERR_STR, typ_type.tkn.get_loc(), e)),
//         }
//     }

//     fn convert_let_arr(
//         &mut self,
//         mem: usize,
//         mem_offset: &mut usize,
//         let_tree: &Tree,
//         size_def_loc: &Location,
//         expected_size: &Vec<usize>,
//         expected_type: &Type,
//         depth: usize
//     ) -> Result<(), String> {
//         if let_tree.children.len() != expected_size[expected_size.len() - depth - 1] {
//             return Err(format!(
//                 "{}: {:?}: Unexpected Size for Array Literal. Expected Array with {} elements, got {}.\n{}: Size declared here: {:?}",
//                 ERR_STR,
//                 let_tree.tkn.get_loc(),
//                 expected_size[expected_size.len() - depth - 1],
//                 let_tree.children.len(),
//                 NOTE_STR,
//                 size_def_loc
//             ));
//         }
//         for expr in &let_tree.children {
//             match expr.typ {
//                 TreeType::ExprArrLiteral => {
//                     if depth == 0 {
//                         return Err(format!(
//                             "{}: {:?}: Unexpected Array Literal in Array Initialization. Expected Array with depth {}, but found another layer.",
//                             ERR_STR,
//                             expr.tkn.get_loc(),
//                             expected_size.len()
//                         ));
//                     }
//                     self.convert_let_arr(
//                         mem,
//                         mem_offset,
//                         expr,
//                         size_def_loc,
//                         expected_size,
//                         expected_type,
//                         depth - 1
//                     )?;
//                 },
//                 _ => {
//                     if expected_size.len() == depth {
//                         return Err(format!(
//                             "{}: {:?}: Unexpected Expression in Array Initialization.",
//                             ERR_STR,
//                             expr.tkn.get_loc()
//                         ));
//                     }
//                     let reg = self.convert_expr(expr)?;
//                     if reg.typ != Type::Unknown && reg.typ != *expected_type {
//                         return Err(format!(
//                             "{}: {:?}: Type Mismatch in Array Initialization! Array is declared to be Type `{:?}`, but got Type `{:?}`\n{}: Type declared here: {:?}",
//                             ERR_STR,
//                             expr.tkn.get_loc(),
//                             expected_type,
//                             reg.typ,
//                             NOTE_STR,
//                             size_def_loc,
//                         ));
//                     }
//                     self.code.push(Instruction::StoreMem {
//                         reg: reg.mem,
//                         var: Variable {
//                             typ: expected_type.clone(),
//                             mem: mem + *mem_offset,
//                         },
//                     });
//                     *mem_offset += 1;
//                     self.reset_registers();
//                 },
//             }
//         }
//         Ok(())
//     }

//     fn convert_stmt_let(&mut self, let_tree: &Tree) -> Result<(), String> {
//         let instr_children = &let_tree.children;
//         assert!(instr_children.len() == 3, "{:#?}", instr_children);
//         let let_name = &instr_children[0];
//         assert!(let_name.tkn.get_type() == TokenType::Name);
//         let let_type = &instr_children[1];
//         assert!(let_type.typ == TreeType::TypeDecl);
//         let mut expected_type = self.convert_type(let_type)?;
//         let let_expr = &instr_children[2];

//         let let_name_tkn = &let_name.tkn;
//         let local_lookup = self
//             .functions
//             .get(&self.current_fn)
//             .expect("At this point, function table is guaranteed to contain current_fn.");
//         if local_lookup
//             .get_scope_location(&let_name_tkn.get_value())
//             .is_some()
//         {
//             return Err(format!(
//                 "{}: {:?}: Variable redefinition",
//                 ERR_STR,
//                 let_name_tkn.get_loc()
//             ));
//         }
//         if let_expr.typ == TreeType::ExprArrLiteral {
//             let mem =
//                 self.add_local_var(&let_name_tkn.get_value(), self.scope_depth, &expected_type);
//             match expected_type {
//                 Type::Arr(t, size) => {
//                     self.convert_let_arr(
//                         mem,
//                         &mut 0,
//                         let_expr,
//                         &let_type.tkn.get_loc(),
//                         &size,
//                         &t,
//                         size.len() - 1
//                     )?;
//                     self.resolve_types(*t.clone())?;
//                 }
//                 e => todo!("{:?}", e),
//             }
//         } else {
//             let reg = self.convert_expr(let_expr)?;
//             match (expected_type.clone(), reg.typ.clone()) {
//                 (e, Type::Unknown) => {
//                     self.resolve_types(e)?;
//                 }
//                 (Type::Ptr(t1, ..), Type::Ptr(t2, ..)) => {
//                     if t1 != t2 {
//                         return Err(format!(
//                             "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
//                             ERR_STR,
//                             let_name_tkn.get_loc(),
//                             expected_type,
//                             reg.typ
//                         ));
//                     }
//                     expected_type = reg.typ.clone();
//                 }
//                 (e, t) => {
//                     if e != t {
//                         return Err(format!(
//                             "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
//                             ERR_STR,
//                             let_name_tkn.get_loc(),
//                             e,
//                             t
//                         ));
//                     }
//                 }
//             }
//             let mem =
//                 self.add_local_var(&let_name_tkn.get_value(), self.scope_depth, &expected_type);
//             self.code.push(Instruction::StoreMem {
//                 reg: reg.mem,
//                 var: Variable {
//                     typ: expected_type.clone(),
//                     mem,
//                 },
//             });
//         }
//         self.reset_registers();
//         Ok(())
//         // self.code.push(Instruction::StoreMem { reg, mem, typ: Type::U64 });
//         // self.reset_registers();
//         // Ok(())
//     }

//     fn convert_stmt_assign(&mut self, assign_tree: &Tree) -> Result<(), String> {
//         let instr_children = &assign_tree.children;
//         assert!(instr_children.len() == 2);
//         let assign_name = &instr_children[0];
//         let assign_expr = &instr_children[1];
//         let reg = self.convert_expr(assign_expr)?;
//         if assign_name.tkn.get_type() == TokenType::Asterisk {
//             let var_name = &assign_name.children[0].tkn;
//             assert!(var_name.get_type() == TokenType::Name);
//             let local_lookup = self
//                 .functions
//                 .get(&self.current_fn)
//                 .expect("At this point, function table is guaranteed to contain current_fn.");

//             let var = match local_lookup.get_variable_location(&var_name.get_value()) {
//                 Some(i) => i,
//                 None => {
//                     return Err(format!(
//                         "{}: {:?}: Unknown variable `{}`",
//                         ERR_STR,
//                         assign_name.tkn.get_loc(),
//                         var_name.get_value()
//                     ))
//                 }
//             };
//             match var.clone().typ {
//                 Type::Ptr(t) => {
//                     if *t != reg.typ {
//                         return Err(format!(
//                             "{}: {:?}: Type Mismatch when assigning to Pointer. Pointer expected Type `{:?}`, but got Type `{:?}`.",
//                             ERR_STR,
//                             assign_name.tkn.get_loc(),
//                             t,
//                             reg.typ
//                         ));
//                     }
//                     self.resolve_types(*t.clone())?;
//                     let reg_mov = self.get_register();
//                     self.code.push(Instruction::LoadMem { reg: reg_mov, var });
//                     self.code.push(Instruction::StorePtr {
//                         src: reg.mem,
//                         dest: reg_mov,
//                         typ: *t,
//                     });
//                 }
//                 e => {
//                     return Err(format!(
//                         "{}: {:?}: Can not dereference Type `{:?}`.",
//                         ERR_STR,
//                         assign_name.tkn.get_loc(),
//                         e
//                     ))
//                 }
//             }
//         } else {
//             assert!(assign_name.tkn.get_type() == TokenType::Name);
//             let var_name = assign_name.tkn.get_value();
//             let local_lookup = self
//                 .functions
//                 .get(&self.current_fn)
//                 .expect("At this point, function table is guaranteed to contain current_fn.");
//             let var = match local_lookup.get_variable_location(&var_name) {
//                 Some(i) => i,
//                 None => {
//                     return Err(format!(
//                         "{}: {:?}: Unknown variable `{}`",
//                         ERR_STR,
//                         assign_name.tkn.get_loc(),
//                         var_name
//                     ))
//                 }
//             };
//             match (&var.typ, &reg.typ) {
//                 (Type::Unknown, Type::Unknown) => panic!(),
//                 (Type::Unknown, _) => todo!(),
//                 (Type::Arr(typ, size), _) => {
//                     let indices = &assign_name.children[0].children;
//                     match indices.len().cmp(&size.len()) {
//                         Ordering::Greater => {
//                             return Err(format!(
//                                 "{}: {:?}: Attempted to index array with too many indices! Expected {} indices, got {}.",
//                                 ERR_STR,
//                                 assign_name.tkn.get_loc(),
//                                 size.len(),
//                                 indices.len()
//                             ));
//                         }
//                         Ordering::Less => {
//                             return Err(format!(
//                                 "{}: {:?}: Attempted to index array with too few indices! Expected {} indices, got {}.",
//                                 ERR_STR,
//                                 assign_name.tkn.get_loc(),
//                                 size.len(),
//                                 indices.len()
//                             ));
//                         }
//                         _ => ()
//                     }
//                     let index_reg = self.get_register();
//                     let mem_offset = self.get_register();
//                     let size_reg = self.get_register();
//                     // Prepare index register
//                     self.code.push(Instruction::LoadUsize {
//                         dest: index_reg,
//                         val: 0,
//                     });
//                     // Stores final offset for stack addressing
//                     self.code.push(Instruction::LoadUsize {
//                         dest: mem_offset,
//                         val: var.mem,
//                     });
//                     for (count, child) in indices.iter().enumerate() {
//                         let reg = self.convert_expr(child)?;
//                         if reg.typ != Type::Unknown && reg.typ != Type::Usize {
//                             return Err(format!(
//                                 "{}: {:?}: Type Mismatch: Array indices are expected to be of Type usize. Got `{:?}`",
//                                 ERR_STR,
//                                 child.tkn.get_loc(),
//                                 reg.typ
//                             ));
//                         }

//                         self.resolve_types(Type::Usize)?;

//                         // Index Out of Bounds check
//                         self.code.push(Instruction::LoadUsize { dest: size_reg, val: size[count] });
//                         self.code.push(Instruction::Cmp { dest: reg.mem, src: size_reg, typ: Type::Usize });
//                         self.code.push(Instruction::JmpLt { dest: self.code.len() + 2 });
//                         self.code.push(Instruction::Exit { code: ExitCode::OobAccess as usize });

//                         // index_reg contains our index
//                         // in each step, multiply the index by the dimension of the array
//                         self.code.push(Instruction::Mul { dest: index_reg, src: size_reg, typ: Type::Usize });
//                         // in each step, add to that ^ the position we're indexing (result of expression)
//                         self.code.push(Instruction::Add { dest: index_reg, src: reg.mem, typ: Type::Usize });
//                     }
//                     self.code.push(Instruction::Add { dest: index_reg, src: mem_offset, typ: Type::Usize });
//                     let reg_tmp = self.get_register();
//                     self.code.push(Instruction::LoadPtrRel {
//                         dest: reg_tmp,
//                         src: index_reg,
//                         typ: *typ.clone(),
//                     });
//                     self.code.push(Instruction::StorePtr {
//                         dest: reg_tmp,
//                         src: reg.mem,
//                         typ: *typ.clone(),
//                     });
//                 }
//                 (typ, Type::Unknown) => {
//                     self.resolve_types(typ.clone())?;
//                     self.code.push(Instruction::StoreMem { reg: reg.mem, var });
//                 }
//                 (var_typ, expr_typ) => {
//                     if var_typ != expr_typ {
//                         return Err(format!(
//                             "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
//                             ERR_STR,
//                             assign_name.tkn.get_loc(),
//                             var.typ.clone(),
//                             reg.typ.clone()
//                         ));
//                     }
//                     self.code.push(Instruction::StoreMem { reg: reg.mem, var });
//                 }
//             }
//         }
//         self.reset_registers();
//         Ok(())
//     }

//     fn convert_arg(&mut self, arg: &Tree) -> Result<Variable, String> {
//         assert!(arg.typ == TreeType::Arg);
//         let arg_children = &arg.children;
//         assert!(arg_children.len() == 1);
//         let arg_expr = &arg_children[0];
//         self.convert_expr(arg_expr)
//     }

//     fn convert_stmt_if(&mut self, if_tree: &Tree) -> Result<(), String> {
//         let if_children = &if_tree.children;
//         assert!(&[2, 3].contains(&if_children.len()));
//         let if_cond = &if_children[0];
//         if if_cond.typ != TreeType::ExprBinary {
//             return Err(format!(
//                 "{}: {:?}: Unexpected Symbol `{}`.",
//                 ERR_STR,
//                 if_cond.tkn.get_loc(),
//                 if_cond.tkn.get_value()
//             ));
//         }
//         let if_block = &if_children[1];
//         self.convert_expr(if_cond)?;
//         self.reset_registers();

//         self.convert_block(if_block)?;
//         let if_jmp = self.resolve_last_jmp();

//         if if_children.len() == 3 {
//             let else_block = &if_children[2];
//             assert!(else_block.typ == TreeType::Block);

//             self.unresolved_jmp_instr.push_back(self.code.len());
//             self.code.push(Instruction::Jmp { dest: usize::MAX });
//             self.set_jmp_lbl(if_jmp, self.code.len());

//             self.convert_block(else_block)?;
//             self.resolve_last_jmp();
//         }
//         self.reset_registers();
//         Ok(())
//     }

//     fn convert_stmt_return(&mut self, ret_tree: &Tree) -> Result<(), String> {
//         let child_count = ret_tree.children.len();
//         assert!(&[0, 1].contains(&child_count));
//         self.functions
//             .get_mut(&self.current_fn)
//             .unwrap()
//             .return_scopes
//             .push(self.scope_depth);
//         let fn_return_type = self
//             .functions
//             .get(&self.current_fn)
//             .unwrap()
//             .get_return_type();
//         if child_count == 1 {
//             let reg = self.convert_expr(&ret_tree.children[0])?;
//             match (&fn_return_type, &reg.typ) {
//                 (Type::None, _) => {
//                     return Err(
//                         format!(
//                         "{}: {:?}: Function is declared to not return anything, but found return type `{:?}`.",
//                         ERR_STR,
//                         ret_tree.children[0].tkn.get_loc(),
//                         reg.typ.clone()
//                         )
//                     );
//                 }
//                 (ret_type, expr_type) => {
//                     if expr_type == &Type::Unknown {
//                         self.resolve_types(ret_type.clone())?;
//                     } else if expr_type != ret_type {
//                         return Err(format!(
//                             "{}: {:?}: Function is declared to return `{:?}`, but found `{:?}`.",
//                             ERR_STR,
//                             ret_tree.children[0].tkn.get_loc(),
//                             ret_type,
//                             expr_type
//                         ));
//                     }
//                 }
//             }
//             self.code.push(Instruction::Push { reg: reg.mem });
//         } else if fn_return_type != Type::None {
//             return Err(format!(
//                 "{}: {:?}: Function is declared to return `{:?}`, but found blank return statement.",
//                 ERR_STR,
//                 ret_tree.tkn.get_loc(),
//                 fn_return_type,
//             ));
//         }
//         self.code.push(Instruction::Return {});
//         self.reset_registers();
//         Ok(())
//     }

//     fn convert_stmt_expr(&mut self, expr_tree: &Tree) -> Result<(), String> {
//         let expr_children = &expr_tree.children;
//         assert!(expr_children.len() == 1, "Expected {{Expr}}");
//         let expr = &expr_children[0];
//         self.convert_expr(expr)?;
//         if !self.unresolved_typ_instr.is_empty() {
//             todo!()
//         }
//         self.reset_registers();
//         Ok(())
//     }

//     fn resolve_last_jmp(&mut self) -> usize {
//         assert!(!self.unresolved_jmp_instr.is_empty());
//         let ip = self.unresolved_jmp_instr.pop_back().unwrap();
//         self.set_jmp_lbl(ip, self.code.len());
//         ip
//     }

//     fn resolve_types(&mut self, expected_type: Type) -> Result<(), String> {
//         if expected_type == Type::None {
//             todo!()
//         }
//         while let Some(index) = self.unresolved_typ_instr.pop_back() {
//             self.set_load_instr(index, expected_type.clone())?;
//         }
//         Ok(())
//     }

//     fn set_jmp_lbl(&mut self, index: usize, dest: usize) {
//         self.code[index] = match self.code[index] {
//             Instruction::JmpEq { .. } => Instruction::JmpEq { dest },
//             Instruction::JmpNeq { .. } => Instruction::JmpNeq { dest },
//             Instruction::JmpGt { .. } => Instruction::JmpGt { dest },
//             Instruction::JmpGte { .. } => Instruction::JmpGte { dest },
//             Instruction::JmpLt { .. } => Instruction::JmpLt { dest },
//             Instruction::JmpLte { .. } => Instruction::JmpLte { dest },
//             Instruction::Jmp { .. } => Instruction::Jmp { dest },
//             _ => todo!(),
//         };
//     }

//     fn set_load_instr(&mut self, index: usize, typ: Type) -> Result<(), String> {
//         self.code[index] = match &self.code[index] {
//             Instruction::LoadUnknown { dest, val, loc } => match typ {
//                 Type::I32 => {
//                     let val = parse_type!(i32, *loc, *val, typ)?;
//                     Instruction::LoadI32 { dest: *dest, val }
//                 }
//                 Type::I64 => {
//                     let val = parse_type!(i64, loc, val, typ)?;
//                     Instruction::LoadI64 { dest: *dest, val }
//                 }
//                 Type::U32 => {
//                     let val = parse_type!(u32, loc, val, typ)?;
//                     Instruction::LoadU32 { dest: *dest, val }
//                 }
//                 Type::U64 => {
//                     let val = parse_type!(u64, loc, val, typ)?;
//                     Instruction::LoadU64 { dest: *dest, val }
//                 }
//                 Type::Usize => {
//                     let val = parse_type!(usize, loc, val, typ)?;
//                     Instruction::LoadUsize { dest: *dest, val }
//                 }
//                 Type::Ptr(t, ..) => {
//                     let val = parse_type!(usize, loc, val, t)?;
//                     Instruction::LoadUsize { dest: *dest, val }
//                 }
//                 _ => todo!("{:?}", loc),
//             },
//             Instruction::Add {
//                 dest,
//                 src,
//                 typ: _typ,
//             } => Instruction::Add {
//                 dest: *dest,
//                 src: *src,
//                 typ,
//             },
//             Instruction::Sub {
//                 dest,
//                 src,
//                 typ: _typ,
//             } => Instruction::Sub {
//                 dest: *dest,
//                 src: *src,
//                 typ,
//             },
//             Instruction::Mul {
//                 dest,
//                 src,
//                 typ: _typ,
//             } => Instruction::Mul {
//                 dest: *dest,
//                 src: *src,
//                 typ,
//             },
//             Instruction::Div {
//                 dest,
//                 src,
//                 typ: _typ,
//             } => Instruction::Div {
//                 dest: *dest,
//                 src: *src,
//                 typ,
//             },
//             Instruction::Cmp {
//                 dest,
//                 src,
//                 typ: _typ,
//             } => Instruction::Cmp {
//                 dest: *dest,
//                 src: *src,
//                 typ,
//             },
//             e => todo!("Handle Instruction {:?}", e),
//         };
//         Ok(())
//     }

//     fn convert_fn(&mut self, func: &Tree) -> Result<(), String> {
//         let fn_children = &func.children;
//         assert!(
//             &[3, 4].contains(&fn_children.len()),
//             "fnKeyword fnName {{Param}} [ReturnType] {{Block}} expected."
//         );
//         let fn_name = &fn_children[0].tkn;
//         assert!(fn_name.get_type() == TokenType::Name);
//         if self.functions.contains_key(&fn_name.get_value()) {
//             return Err(format!(
//                 "{}: {:?}: Function redefinition",
//                 ERR_STR,
//                 fn_name.get_loc()
//             ));
//         }
//         let name = fn_name.get_value();

//         self.current_fn = name.clone();
//         let f = Function::new(self.code.len());
//         self.functions.insert(name.clone(), f);
//         self.convert_fn_param(&fn_children[1])?;
//         if fn_children.len() == 3 {
//             self.convert_block(&fn_children[2])?;
//         } else if fn_children.len() == 4 {
//             let fn_return = &fn_children[2];
//             assert!(fn_return.typ == TreeType::TypeDecl);
//             let return_children = &fn_return.children;
//             if return_children.len() == 1 {
//                 let type_name = &return_children[0];
//                 assert!(type_name.tkn.get_type() == TokenType::Name);

//                 let type_name_str = type_name.tkn.get_value();

//                 let typ = match self.convert_type_str(&type_name_str) {
//                     Ok(t) => t,
//                     Err(e) => return Err(format!("{}: {:?}: {}", ERR_STR, type_name.tkn.get_loc(), e)),
//                 };
//                 self.functions.get_mut(&name).unwrap().set_return_type(&typ);
//             }

//             self.convert_block(&fn_children[3])?;
//         } else {
//             panic!();
//         }
//         let return_scopes = &self.functions.get(&self.current_fn).unwrap().return_scopes;
//         let return_found = return_scopes.contains(&1);
//         let return_type = self
//             .functions
//             .get(&self.current_fn)
//             .unwrap()
//             .return_type
//             .clone();
//         if fn_name.get_value() == "main" {
//             self.code.push(Instruction::Exit { code: ExitCode::Normal as usize });
//         }
//         match (return_found, return_type) {
//             (false, Type::None) => {
//                 self.code.push(Instruction::Return {});
//             }
//             (true, Type::None) => {
//                 if self.code.is_empty() || !(*self.code.last().unwrap() == Instruction::Return {}) {
//                     self.code.push(Instruction::Return {});
//                 }
//             }
//             (false, t) => {
//                 return Err(
//                     format!("{}: Function `{}` is declared to return `{:?}`, but no return statements found.\n{}: There's no Control Flow check yet, so even if it's unreachable because of if-else or anything else, it won't be caught.",
//                     ERR_STR,
//                     self.current_fn,
//                     t,
//                     NOTE_STR)
//                 );
//             }
//             (true, t) => {
//                 if !(*self.code.last().unwrap() == Instruction::Return {}) {
//                     return Err(
//                         format!("{}: {:?}: Function `{}` is declared to return `{:?}`, expected `return` as last instruction.",
//                         ERR_STR,
//                         fn_name.get_loc(),
//                         self.current_fn,
//                         t
//                     ));
//                 }
//             }
//         }
//         self.current_fn.clear();
//         Ok(())
//     }

//     fn convert_fn_param(&mut self, param: &Tree) -> Result<(), String> {
//         let param_children = &param.children;
//         let mut reg_ctr = 0;
//         for p in param_children {
//             match &p.typ {
//                 TreeType::Param => {
//                     assert!(p.children.len() == 1);
//                     let param_name = p.tkn.get_value();
//                     let param_type = &p.children[0].typ;
//                     assert!(*param_type == TreeType::TypeDecl);
//                     let param_type_child = &p.children[0].children;
//                     let type_name = &param_type_child[0];
//                     let typ = if type_name.tkn.get_type() == TokenType::Ampersand {
//                         let type_name = &type_name.children[0];
//                         assert!(type_name.typ == TreeType::Name);
//                         let type_name_str = type_name.tkn.get_value();

//                         match self.convert_type_str(&type_name_str) {
//                             Ok(t) => Type::Ptr(Box::new(t)),
//                             Err(e) => {
//                                 return Err(format!(
//                                     "{}: {:?}: {}",
//                                     ERR_STR,
//                                     type_name.tkn.get_loc(),
//                                     e
//                                 ))
//                             }
//                         }
//                     } else {
//                         assert!(type_name.tkn.get_type() == TokenType::Name);
//                         let type_name_str = type_name.tkn.get_value();

//                         match self.convert_type_str(&type_name_str) {
//                             Ok(t) => t,
//                             Err(e) => {
//                                 return Err(format!(
//                                     "{}: {:?}: {}",
//                                     ERR_STR,
//                                     type_name.tkn.get_loc(),
//                                     e
//                                 ))
//                             }
//                         }
//                     };

//                     let local_lookup = self.functions.get_mut(&self.current_fn).expect(
//                         "At this point, function table is guaranteed to contain current_fn.",
//                     );
//                     if local_lookup.get_param_location(&param_name).is_some() {
//                         return Err(format!(
//                             "{}: {:?}: Parameter redefinition `{}`",
//                             ERR_STR,
//                             p.tkn.get_loc(),
//                             param_name
//                         ));
//                     }
//                     let var = local_lookup.add_param(&param_name, &typ);
//                     self.code.push(Instruction::StoreMem { reg: reg_ctr, var });
//                     reg_ctr += 1;
//                 }
//                 t => {
//                     todo!(
//                         "Handle t not TreeType::Param in convert_fn_param() - Got: {:?}",
//                         t
//                     )
//                 }
//             }
//         }
//         Ok(())
//     }

//     fn convert_block(&mut self, block: &Tree) -> Result<(), String> {
//         assert!(block.typ == TreeType::Block);
//         self.enter_scope();
//         for instr in &block.children {
//             match &instr.typ {
//                 TreeType::StmtLet => self.convert_stmt_let(instr)?,
//                 TreeType::StmtAssign => self.convert_stmt_assign(instr)?,
//                 TreeType::StmtIf => self.convert_stmt_if(instr)?,
//                 TreeType::StmtReturn => self.convert_stmt_return(instr)?,
//                 TreeType::StmtExpr => self.convert_stmt_expr(instr)?,
//                 e => todo!("convert {:?} inside block", e),
//             }
//         }
//         self.leave_scope();
//         Ok(())
//     }

//     fn enter_scope(&mut self) {
//         assert!(!self.current_fn.is_empty());
//         // println!("Entering scope");
//         self.scope_depth += 1;
//         self.functions
//             .get_mut(&self.current_fn)
//             .unwrap()
//             .add_scope();
//     }

//     fn leave_scope(&mut self) {
//         assert!(!self.current_fn.is_empty());
//         // println!("Leaving scope");
//         self.scope_depth -= 1;
//         self.functions
//             .get_mut(&self.current_fn)
//             .unwrap()
//             .remove_scope();
//     }

//     fn convert_ast(&mut self, ast: &Tree) -> Result<(), String> {
//         match &ast.typ {
//             TreeType::File => {
//                 for func in &ast.children {
//                     self.convert_fn(func)?;
//                 }
//             }
//             _ => todo!("handle other types"),
//         }
//         Ok(())
//     }

//     // fn add_instruction(&mut self, instr: Instruction) {
//     //     self.code.push(instr);
//     // }

//     fn generate_code(&mut self) -> Result<(), String> {
//         self.convert_ast(&self.ast.clone())
//     }

//     pub fn interpret(&mut self) -> Result<(), String> {
//         // for (i, c) in self.code.iter().enumerate() {
//         //     println!("{i:3} -> {c:?}");
//         // }
//         // todo!();
//         let entry_point = String::from("main");

//         if !self.functions.contains_key(&entry_point) {
//             return Err(format!(
//                 "{}: Missing entry point - Could not find function {}()",
//                 ERR_STR, entry_point
//             ));
//         }

//         const RETURN_STACK_LIMIT: usize = 4096 * 4096;
//         const STACK_SIZE: usize = 1_000_000;

//         let mut return_stack = VecDeque::<usize>::new();
//         let mut return_values = VecDeque::<Memory>::new();
//         let mut stack = vec![Memory { u64: 0 }; STACK_SIZE];
//         let mut stack_ptr = STACK_SIZE - 1 - self.get_function_stack_size(&entry_point);

//         let mut flags = 0;
//         const EQ: usize = 1;
//         const LT: usize = 2;
//         const GT: usize = 4;

//         let mut ip = self
//             .functions
//             .get(&entry_point)
//             .unwrap() // Can safely unwrap because map is guaranteed to contain entry_point
//             .get_ip();

//         while ip < self.code.len() {
//             // todo!("Interpretation with type system in place.");
//             if return_stack.len() > RETURN_STACK_LIMIT {
//                 return Err(format!(
//                     "{}: Recursion Limit reached when interpreting!",
//                     ERR_STR
//                 ));
//             }
//             let instr = &self.code[ip];
//             let mut add_ip = true;
//             match instr {
//                 Instruction::LoadUnknown { .. } => panic!(),
//                 Instruction::LoadI32 { dest, val } => {
//                     self.registers[*dest].i32 = *val;
//                 }
//                 Instruction::LoadI64 { dest, val } => {
//                     self.registers[*dest].i64 = *val;
//                 }
//                 Instruction::LoadU32 { dest, val } => {
//                     self.registers[*dest].u32 = *val;
//                 }
//                 Instruction::LoadU64 { dest, val } => {
//                     self.registers[*dest].u64 = *val;
//                 }
//                 Instruction::LoadUsize { dest, val } => {
//                     self.registers[*dest].ptr = *val;
//                 }
//                 Instruction::LoadPtrRel { dest, src, .. } => {
//                     self.registers[*dest].ptr = stack_ptr + unsafe { self.registers[*src].ptr } + 1;
//                 }
//                 Instruction::LoadPtr { dest, src, .. } => {
//                     self.registers[*dest] = unsafe { stack[self.registers[*src].ptr] };
//                 }
//                 Instruction::StorePtr { dest, src, .. } => {
//                     stack[unsafe { self.registers[*dest].ptr }] = self.registers[*src];
//                 }
//                 Instruction::LoadF32 { .. } => {
//                     todo!()
//                 }
//                 Instruction::LoadF64 { .. } => {
//                     todo!()
//                 }
//                 Instruction::Add { dest, src, typ } => {
//                     let v1 = self.registers[*dest];
//                     let v2 = self.registers[*src];
//                     perform_op!(self.registers[*dest], v1, v2, typ, +);
//                 }
//                 Instruction::Sub { dest, src, typ } => {
//                     let v1 = self.registers[*dest];
//                     let v2 = self.registers[*src];
//                     perform_op!(self.registers[*dest], v1, v2, typ, -);
//                 }
//                 Instruction::Mul { dest, src, typ } => {
//                     let v1 = self.registers[*dest];
//                     let v2 = self.registers[*src];
//                     perform_op!(self.registers[*dest], v1, v2, typ, *);
//                 }
//                 Instruction::Div { dest, src, typ } => {
//                     let v1 = self.registers[*dest];
//                     let v2 = self.registers[*src];
//                     perform_op!(self.registers[*dest], v1, v2, typ, /);
//                 }
//                 Instruction::Cmp { dest, src, typ } => {
//                     let v1 = self.registers[*dest];
//                     let v2 = self.registers[*src];
//                     flags = 0;
//                     flags |= perform_cmp!(v1, v2, typ, ==) as usize * EQ;
//                     flags |= perform_cmp!(v1, v2, typ, <) as usize * LT;
//                     flags |= perform_cmp!(v1, v2, typ, >) as usize * GT;
//                     // >= is flags & EQ || flags & GT
//                     // <= is flags & EQ || flags & LT
//                     // != is not flags & EQ
//                 }
//                 Instruction::JmpEq { dest } => {
//                     if flags & EQ != 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::JmpNeq { dest } => {
//                     if flags & EQ == 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::JmpGt { dest } => {
//                     if flags & GT != 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::JmpGte { dest } => {
//                     if flags & EQ != 0 || flags & GT != 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::JmpLt { dest } => {
//                     if flags & LT != 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::JmpLte { dest } => {
//                     if flags & EQ != 0 || flags & LT != 0 {
//                         ip = *dest;
//                         add_ip = false;
//                     }
//                 }
//                 Instruction::Jmp { dest } => {
//                     ip = *dest;
//                     add_ip = false;
//                 }
//                 Instruction::Move { dest, src } => {
//                     self.registers[*dest] = self.registers[*src];
//                 }
//                 Instruction::LoadMem {
//                     reg,
//                     var: Variable { typ: _typ, mem },
//                 } => {
//                     self.registers[*reg] = stack[stack_ptr + *mem + 1];
//                 }
//                 Instruction::StoreMem {
//                     reg,
//                     var: Variable { typ: _typ, mem },
//                 } => {
//                     stack[stack_ptr + *mem + 1] = self.registers[*reg];
//                 }
//                 Instruction::Call { fn_name } => {
//                     let stack_size = self.get_function_stack_size(fn_name);
//                     // println!("{fn_name} has a stack size of {stack_size}");
//                     return_stack.push_back(ip);
//                     return_stack.push_back(stack_size);
//                     if stack_ptr < stack_size {
//                         return Err(format!("{}: Stack Overflow when interpreting!", ERR_STR));
//                     }
//                     stack_ptr -= stack_size;
//                     ip = self
//                         .functions
//                         .get(fn_name)
//                         .unwrap_or_else(|| panic!("Could not find {fn_name} in function table"))
//                         .get_ip();
//                     add_ip = false;
//                 }
//                 Instruction::Push { reg } => {
//                     return_values.push_back(self.registers[*reg]);
//                 }
//                 Instruction::Pop { reg } => {
//                     self.registers[*reg] = return_values.pop_back().unwrap();
//                 }
//                 Instruction::Return {} => {
//                     if return_stack.is_empty() {
//                         todo!();
//                     }
//                     // Can safely unwrap because I just checked return_stack length
//                     // Whenever I add something, I add packs of 2 values, so there's never a situation
//                     // where only one element is on the return_stack
//                     stack_ptr += return_stack.pop_back().unwrap();
//                     ip = return_stack.pop_back().unwrap();
//                 }
//                 Instruction::Exit { code } => {
//                     println!("Program exited with Exit Code {code}.");
//                     if *code != 0 {
//                         // TODO: Add better runtime error messages
//                         ExitCode::print_exit_msg(*code);
//                     }
//                     break;
//                 }
//             }
//             if add_ip {
//                 ip += 1;
//             }
//             if self.print_debug {
//                 let mut reg_string = String::from("[");
//                 const DISP_REG: usize = 10;
//                 for i in 0..=DISP_REG {
//                     reg_string.push_str(format!("{:6}", unsafe {self.registers[i].u64 }).as_str());
//                     if i < DISP_REG { reg_string.push_str(", "); }
//                 }
//                 reg_string.push(']');
//                 println!("{:4} {:?} {} {:?}", ip, flags, reg_string, instr);
//             }
//         }

//         for i in 0..20 {
//             println!("{} {}", STACK_SIZE - i - 1, unsafe { stack[STACK_SIZE - i - 1].u64 });
//         }
//         Ok(())
//     }

//     #[allow(unused)]
//     pub fn compile(&mut self) -> Result<(), String> {
//         todo!("Restructure the program -> Functions should use the same instruction space, and same memory")
//     }
// }
