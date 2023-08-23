use std::collections::{HashMap, BTreeMap};

use crate::lexer::{Location, Token, TokenType};
use crate::parser::{Tree, TreeType};
use crate::codegen::ERR_STR;

macro_rules! get_and_assert {
    ($tree: expr, $typ: expr) => {
        {
            assert!($tree.typ == $typ);
            $tree
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    None, // For functions that return nothing
    Unknown,
    I32,
    I64,
    U32,
    U64,
    Usize,
    Bool,
    Ptr(Box<Type>),
    Arr(Box<Type>, Vec<usize>),
    // Reserved for later use
    F32,
    F64,
}

struct TCFunction {
    variables: BTreeMap<String, Type>,
    return_type: Type
}

impl TCFunction {
    fn new() -> Self {
        return Self {
            variables: BTreeMap::new(),
            return_type: Type::None
        }
    }

    fn set_return_type(&mut self, typ: &Type) {
        if self.return_type != Type::None {
            panic!()
        }
        self.return_type = typ.clone();
    }

    fn add_variable(&mut self, v_name: &String, v_type: &Type) -> Result<(), String> {
        if self.variables.contains_key(v_name) {
            Err(format!("Variable redefinition."))
        } else {
            self.variables.insert(v_name.clone(), v_type.clone());
            Ok(())
        }
    }
}

pub struct TypeChecker {
    ast: Tree,
    functions: HashMap<String, TCFunction>,
    current_fn: String,
    #[allow(unused)]
    print_debug: bool
}

impl TypeChecker {
    pub fn new(ast: &Tree, print_debug: bool) -> Self {
        Self {
            ast: ast.clone(),
            functions: HashMap::new(),
            current_fn: String::new(),
            print_debug
        }
    }

    pub fn type_check_program(&mut self) -> Result<Tree, String> {
        todo!()
        // self.type_check(&self.ast.clone())
    }

    // fn type_check(&mut self, tree: &Tree) -> Result<Tree, String> {
    //     println!("{:?}", tree.typ);
    //     let mut new_tree = Tree { children: vec![], ..tree.clone()};
    //     let mut push = |node| new_tree.children.push(node);
    //     match tree.typ {
    //         TreeType::File => {
    //             for c in &tree.children {
    //                 push(self.type_check(&c)?);
    //             }
    //         }
    //         TreeType::Func => {
    //             assert!(self.current_fn.is_empty());
    //             let mut func = TCFunction::new();
                
    //             let name = get_and_assert!(&tree.children[0], TreeType::Name);
    //             self.current_fn = name.tkn.get_value();
    //             if self.functions.contains_key(&self.current_fn) {
    //                 return Err(format!(
    //                     "{}: {:?}: Function redefinition",
    //                     ERR_STR,
    //                     name.tkn.get_loc()
    //                 ));
    //             }
    //             push(name.clone());
                
    //             self.functions.insert(self.current_fn.clone(), func);

    //             let params = get_and_assert!(&tree.children[1], TreeType::ParamList);
    //             let mut param_node = Tree { children: vec![], ..params.clone() };
    //             for param in &params.children {
    //                 param_node.children.push(self.type_check(param)?);
    //             }
    //             push(param_node);      
                
    //             let fn_type = get_and_assert!(&tree.children[2], TreeType::TypeDecl);
    //             let typ = if fn_type.children.len() == 1 { 
    //                 self.convert_type(fn_type)?
    //             } else {
    //                 Type::None
    //             };
    //             let func = self.functions.get_mut(&self.current_fn).unwrap();
    //             func.set_return_type(&typ);
    //             let type_tree = Tree {
    //                 typ: TreeType::TypeDecl,
    //                 tkn: Token::new(
    //                     TokenType::Name,
    //                     format!("{:?}", typ),
    //                     fn_type.tkn.get_loc()
    //                 ),
    //                 children: vec![]
    //             };
    //             push(type_tree);

    //             let block = get_and_assert!(&tree.children[3], TreeType::Block);
    //             push(self.type_check(&block)?);

    //             self.current_fn.clear();
    //         }
    //         TreeType::Param => {
    //             let p_tree = get_and_assert!(&tree, TreeType::Param);
    //             let p_name = p_tree.tkn.get_value();
    //             let p_type = self.convert_type(&p_tree.children[0])?;

    //             let func = self.functions.get_mut(&self.current_fn).unwrap();
    //             if let Err(e) = func.add_variable(&p_name, &p_type) {
    //                 return Err(format!(
    //                     "{}: {:?}: {}",
    //                     ERR_STR,
    //                     p_tree.tkn.get_loc(),
    //                     e
    //                 ));
    //             }
    //         }
    //         TreeType::Block => {
    //             assert!(!self.current_fn.is_empty());
    //             for c in &tree.children {
    //                 self.type_check(c)?;
    //             }
    //         }
    //         TreeType::StmtLet => {
    //             tree.print_debug();

    //             assert!(!self.current_fn.is_empty());
                
    //             let var_name_tree = get_and_assert!(&tree.children[0], TreeType::Name);
    //             let var_name = var_name_tree.tkn.get_value();
    //             push(var_name_tree.clone());
                
    //             let var_type_tree = get_and_assert!(&tree.children[1], TreeType::TypeDecl);
    //             let var_type = self.convert_type(var_type_tree)?;
    //             push(var_type_tree.clone());
                
    //             let func = self.functions.get_mut(&self.current_fn).unwrap();
    //             func.add_variable(&var_name, &var_type)?;

    //             let var_expr_tree = &tree.children[2];
    //             push(self.type_check_expr(&var_type, var_expr_tree)?);

    //             todo!();
    //         }
    //         _ => {
    //             tree.print_debug();
    //             todo!("{:?}", tree.typ);
    //         }
    //     }
    //     Ok(new_tree)
    // }

    // fn type_check_expr(&self, expected_type: &Type, expr_tree: &Tree) -> Result<Tree, String> {
    //     match &expr_tree.typ {
    //         TreeType::ExprLiteral {..} => {
    //             todo!()
    //         }
    //         e => todo!("{:?}", e)
    //     }
    //     todo!();
    // }

    // fn convert_type(&self, tree: &Tree) -> Result<Type, String> {
    //     let typ = get_and_assert!(&tree, TreeType::TypeDecl);
    //     let t_name = get_and_assert!(&typ.children[0], TreeType::Name);
    //     self.convert_type_str(&t_name.tkn.get_loc(), &t_name.tkn.get_value())
    // }

    // fn convert_type_str(&self, loc: &Location, val: &str) -> Result<Type, String> {
    //     match val {
    //         "i32" => Ok(Type::I32),
    //         "i64" => Ok(Type::I64),
    //         "u32" => Ok(Type::U32),
    //         "u64" => Ok(Type::U64),
    //         "usize" => Ok(Type::Usize),
    //         // Reserved for future use
    //         "f32" => Ok(Type::F32),
    //         "f64" => Ok(Type::F64),
    //         t => Err(format!(
    //             "{}: {:?}: Unexpected Type `{}`. Expected one of {{i32, i64, u32, u64}}",
    //             ERR_STR,
    //             loc,
    //             t
    //         )),
    //     }
    // }
}