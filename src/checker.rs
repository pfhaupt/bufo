use indexmap::IndexMap;
use lazy_static::lazy_static;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::codegen::{ERR_STR, NOTE_STR, WARN_STR};
use crate::lexer::{Location, BUILT_IN_VARIABLES, BUILT_IN_FUNCTIONS};
use crate::parser::{Tree, TreeType};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Any, // Can be everything, `Any+Usize`->Usize, `let a: ClassType = Any`->ClassType
    None, // For functions that return nothing
    Unknown,
    I32,
    I64,
    U32,
    U64,
    Usize,
    Bool,
    // Ptr(Box<Type>),
    Arr(Box<Type>, Vec<usize>),
    Class(String),
    // Reserved for later use
    F32,
    F64,
}

lazy_static!{
    static ref BUILT_IN_FUNC: HashMap<String, TCFunction> = {
        assert!(BUILT_IN_FUNCTIONS.len() == 3);
        type TCF = TCFunction;
        type TCV = TCVariable;
        type LOC = Location;
        let mut h = HashMap::new();
        
        let mut alloc_fn = TCF::new(LOC::builtin(), TCV::new(&LOC::builtin(), &Type::Any));
        alloc_fn.add_parameter(&String::from("size"), TCV::new(&LOC::builtin(), &Type::Usize));
        h.insert(String::from("MALLOC"), alloc_fn);

        let mut sizeof_fn = TCF::new(LOC::builtin(), TCV::new(&LOC::builtin(), &Type::Usize));
        sizeof_fn.add_parameter(&String::from("obj"), TCV::new(&LOC::builtin(), &Type::Any));
        h.insert(String::from("SIZEOF"), sizeof_fn);

        let mut exit_fn = TCF::new(LOC::builtin(), TCV::new(&LOC::builtin(), &Type::None));
        exit_fn.add_parameter(&String::from("code"), TCV::new(&LOC::builtin(), &Type::Usize));
        h.insert(String::from("EXIT"), exit_fn);

        h
    };

    pub static ref BUILT_IN_VARS: HashMap<String, TCVariable> = {
        assert!(BUILT_IN_VARIABLES.len() == 3);
        type TCF = TCFunction;
        type TCV = TCVariable;
        type LOC = Location;
        let mut h = HashMap::new();
        h.insert(String::from("STACK_OVERFLOW_CODE"), TCV::new(&LOC::builtin(), &Type::Usize));
        h.insert(String::from("FUNCTION_COUNTER"), TCV::new(&LOC::builtin(), &Type::Usize));
        h.insert(String::from("FUNCTION_LIMIT"), TCV::new(&LOC::builtin(), &Type::Usize));

        h
    };
}

impl Display for Type {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            // Type::Ptr(t) => write!(fmt, "&{}", t),
            Type::Class(str) => write!(fmt, "{}", str),
            Type::Arr(t, s) => write!(fmt, "{}", format!("{t}{s:?}")),
            _ => write!(fmt, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TCVariable {
    loc: Location,
    pub typ: Type,
}

impl TCVariable {
    fn new(loc: &Location, typ: &Type) -> Self {
        Self {
            loc: loc.clone(),
            typ: typ.clone(),
        }
    }
}

#[derive(Debug)]
struct TCFunction {
    parameters: IndexMap<String, TCVariable>,
    variables: Vec<IndexMap<String, TCVariable>>,
    return_type: TCVariable,
    loc: Location,
}

impl TCFunction {
    fn new(loc: Location, return_type: TCVariable) -> Self {
        Self {
            parameters: IndexMap::new(),
            variables: vec![IndexMap::new()],
            return_type,
            loc,
        }
    }

    fn get_return_type(&self) -> TCVariable {
        self.return_type.clone()
    }

    fn add_scope(&mut self) {
        self.variables.push(IndexMap::new());
    }

    fn remove_scope(&mut self) {
        self.variables.pop().unwrap();
    }

    fn add_parameter(&mut self, v_name: &String, var: TCVariable) -> Result<(), String> {
        match self.get_variable(v_name) {
            Some(v) => Err(format!(
                "Variable redefinition!\n{}: Variable already declared here: {:?}",
                NOTE_STR, v.loc
            )),
            None => {
                self.parameters.insert(v_name.clone(), var);
                Ok(())
            }
        }
    }

    fn add_variable(
        &mut self,
        v_name: &String,
        var: TCVariable,
        scope_depth: usize,
    ) -> Result<(), String> {
        match self.get_scope_location(v_name) {
            Some(v) => Err(format!(
                "Variable redefinition!\n{}: Variable already declared here: {:?}",
                NOTE_STR, v.loc
            )),
            None => {
                self.variables
                    .get_mut(scope_depth)
                    .unwrap()
                    .insert(v_name.clone(), var);
                Ok(())
            }
        }
    }

    fn get_scope_location(&self, var_name: &String) -> Option<TCVariable> {
        self.variables.last().unwrap().get(var_name).cloned()
    }

    fn get_variable(&self, var_name: &String) -> Option<&TCVariable> {
        match self.parameters.get(var_name) {
            Some(p) => Some(p),
            None => {
                for scope in (0..self.variables.len()).rev() {
                    if let Some(mem) = self.variables.get(scope).unwrap().get(var_name) {
                        return Some(mem);
                    }
                }
                None
            }
        }
    }
}

#[derive(Debug)]
struct TCClass {
    loc: Location,
    fields: HashMap<String, TCVariable>
}

impl TCClass {
    fn new(loc: Location) -> Self {
        Self {
            loc,
            fields: HashMap::new()
        }
    }

    fn add_field(&mut self, name: &String, var: TCVariable) -> Result<(), String> {
        match self.fields.get(name) {
            Some(f) => {
                Err(format!(
                    "Class Field redeclaration!\n{}: {:?}: Field `{}` already defined here.",
                    NOTE_STR,
                    f.loc,
                    name
                ))
            }
            None => {
                self.fields.insert(name.clone(), var);
                Ok(())
            }
        }
    }

    fn get_field(&self, name: &String) -> Option<TCVariable> {
        self.fields.get(name).cloned()
    }

}

pub struct TypeChecker {
    ast: Option<Tree>,
    classes: HashMap<String, TCClass>,
    current_class: String,
    functions: HashMap<String, TCFunction>,
    current_fn: String,
    current_str: String,
    scope_depth: usize,
    #[allow(unused)]
    print_debug: bool,
}

impl TypeChecker {
    pub fn new(print_debug: bool) -> Self {
        Self {
            ast: None,
            classes: HashMap::new(),
            current_class: String::new(),
            functions: HashMap::new(),
            current_fn: String::new(),
            current_str: String::new(),
            scope_depth: 0,
            print_debug,
        }
    }

    pub fn set_ast(&mut self, ast: Tree) {
        self.ast = Some(ast);
    }

    fn get_current_function(&self) -> &TCFunction {
        self.functions.get(&self.current_fn).expect("method get_current_function() is only ever called when functions lookup contains current_func")
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

    pub fn type_check_program(&mut self) -> Result<Tree, String> {
        assert!(self.ast.is_some());
        self.fill_lookup(self.ast.clone().unwrap())?;
        self.type_check(&self.ast.as_ref().unwrap().clone())
    }

    fn fill_lookup(&mut self, ast: Tree) -> Result<(), String> {
        match ast.typ {
            TreeType::File { functions, classes } => {
                for c in classes {
                    self.fill_lookup(c)?;
                }
                for f in functions {
                    self.fill_lookup(f)?;
                }
                Ok(())
            }
            TreeType::Class { name, fields, functions, features } => {
                assert!(functions.is_empty());
                assert!(features.is_empty());
                self.current_class = name.clone();
                if let Some(class) = self.classes.get(&self.current_class) {
                    return Err(format!(
                        "{}: {:?}: Class redefinition\n{}: {:?}: Class already defined here",
                        ERR_STR,
                        ast.tkn.get_loc(),
                        NOTE_STR,
                        class.loc
                    ));
                }
                self.classes.insert(name.clone(), TCClass::new(ast.tkn.get_loc()));
                for f in fields {
                    self.fill_lookup(f)?;
                }
                self.current_class.clear();
                Ok(())
            }
            TreeType::Field { name, typ } => {
                let f_type = self.get_type(&typ)?;
                let class = self.classes.get_mut(&self.current_class).unwrap();
                if let Err(e) =
                    class.add_field(&name, TCVariable::new(&ast.tkn.get_loc(), &f_type))
                {
                    Err(format!("{}: {:?}: {}", ERR_STR, ast.tkn.get_loc(), e))
                } else {
                    Ok(())
                }
            }
            TreeType::Feature { name, return_type, param, .. }
            | TreeType::Func { name, return_type, param, .. } => {
                assert!(self.current_fn.is_empty());
                assert!(self.current_class.is_empty());

                self.current_fn = name.clone();
                if let Some(func) = self.functions.get(&self.current_fn) {
                    return Err(format!(
                        "{}: {:?}: Function redefinition\n{}: {:?}: Function already defined here",
                        ERR_STR,
                        ast.tkn.get_loc(),
                        NOTE_STR,
                        func.loc
                    ));
                }
                let typ = if let Some(rt) = return_type {
                    self.get_type(&rt)?
                } else {
                    Type::None
                };
                let mut func = TCFunction::new(
                    ast.tkn.get_loc(),
                    TCVariable {
                        loc: ast.tkn.get_loc(),
                        typ,
                    },
                );
                self.functions.insert(self.current_fn.clone(), func);
                self.fill_lookup(*param)?;

                self.current_fn.clear();
                Ok(())
            }
            TreeType::ParamList { parameters } => {
                for p in parameters {
                    self.fill_lookup(p)?;
                }
                Ok(())
            }
            TreeType::Param { name, typ } => {
                let p_type = self.get_type(&typ)?;

                let func = self.functions.get_mut(&self.current_fn).unwrap();
                if let Err(e) =
                    func.add_parameter(&name, TCVariable::new(&ast.tkn.get_loc(), &p_type))
                {
                    Err(format!("{}: {:?}: {}", ERR_STR, ast.tkn.get_loc(), e))
                } else {
                    Ok(())
                }
            }
            _ => panic!("{}", ast.rebuild_code())
        }
    }
    fn type_check(&mut self, tree: &Tree) -> Result<Tree, String> {
        // println!("{}", tree.rebuild_code());
        match &tree.typ {
            TreeType::File { functions, classes } => {
                let mut checked_classes = vec![];
                let mut checked_functions = vec![];
                for c in classes {
                    checked_classes.push(self.type_check(c)?);
                }
                for f in functions {
                    checked_functions.push(self.type_check(f)?);
                }
                Ok(Tree {
                    typ: TreeType::File {
                        classes: checked_classes,
                        functions: checked_functions,
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::Class { name, fields, functions, features } => {
                Ok(tree.clone())
            }
            TreeType::Field { name, typ } => {
                Ok(tree.clone())
            }
            TreeType::Feature { name, return_type, param, block } => {
                self.current_fn = name.clone();
                let b_t = self.type_check(block)?;

                self.current_fn.clear();
                Ok(Tree {
                    typ: TreeType::Func {
                        name: name.clone(),
                        return_type: return_type.clone(),
                        param: param.clone(),
                        block: Box::new(b_t),
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::Func {
                name,
                return_type,
                param,
                block,
            } => {
                self.current_fn = name.clone();
                let b_t = self.type_check(block)?;

                self.current_fn.clear();
                Ok(Tree {
                    typ: TreeType::Func {
                        name: name.clone(),
                        return_type: return_type.clone(),
                        param: param.clone(),
                        block: Box::new(b_t),
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::ParamList { parameters } => {
                Ok(tree.clone())
            }
            TreeType::Param { name, typ } => {
                Ok(tree.clone())
            }
            TreeType::Block { statements } => {
                assert!(!self.current_fn.is_empty());
                self.enter_scope();
                let mut children = vec![];
                for c in statements {
                    children.push(self.type_check(c)?);
                }
                self.leave_scope();
                Ok(Tree {
                    typ: TreeType::Block {
                        statements: children,
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::StmtLet {
                name,
                typ,
                expression,
            } => {
                assert!(!self.current_fn.is_empty());

                let var_type = self.get_type(typ)?;

                let e_t = Box::new(self.type_check_expr(&var_type, expression)?);

                let func = self.functions.get_mut(&self.current_fn).unwrap();
                if let Err(e) = func.add_variable(
                    name,
                    TCVariable::new(&tree.tkn.get_loc(), &var_type),
                    self.scope_depth,
                ) {
                    return Err(format!("{}: {:?}: {}", ERR_STR, tree.tkn.get_loc(), e));
                }
                
                Ok(Tree {
                    typ: TreeType::StmtLet {
                        name: name.clone(),
                        typ: typ.clone(),
                        expression: e_t,
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::StmtAssign { name, expression } => {
                assert!(!self.current_fn.is_empty());

                let var_type = self.get_expr_type(name, true)?;
                let n_t = self.type_check_expr(&var_type, name)?;
                let e_t = self.type_check_expr(&var_type, expression)?;

                Ok(Tree {
                    typ: TreeType::StmtAssign {
                        name: Box::new(n_t),
                        expression: Box::new(e_t),
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::StmtIf {
                condition,
                if_branch,
                else_branch,
            } => {
                let condition = Box::new(self.type_check_expr(&Type::Bool, condition)?);
                let if_branch = Box::new(self.type_check(if_branch)?);
                let else_branch = if let Some(else_branch) = else_branch {
                    Some(Box::new(self.type_check(else_branch)?))
                } else {
                    None
                };
                Ok(Tree {
                    typ: TreeType::StmtIf {
                        condition,
                        if_branch,
                        else_branch,
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::StmtExpr { expression } => {
                let e_t = match &expression.typ {
                    TreeType::ExprCall { function_name, .. } => {
                        match self.functions.get(function_name) {
                            Some(func) => {
                                let ret = &func.return_type.typ;
                                if !self.match_type(&Type::None, ret) {
                                    println!("{}: {:?}: Ignoring return value of function call to `{}()`",
                                        WARN_STR,
                                        tree.tkn.get_loc(),
                                        function_name
                                    );
                                }
                                self.type_check_expr(ret, expression)?
                            }
                            None => return Err(format!(
                                "{}: {:?}: Unknown function `{}`",
                                ERR_STR,
                                expression.tkn.get_loc(),
                                function_name
                            )),
                        }
                    }
                    TreeType::BuiltInFunction { function_name, .. } => {
                        match BUILT_IN_FUNC.get(function_name) {
                            Some(func) => {
                                let ret = &func.return_type.typ;
                                if !self.match_type(&Type::None, ret) {
                                    println!("{}: {:?}: Ignoring return value of function call to `{}()`",
                                        WARN_STR,
                                        tree.tkn.get_loc(),
                                        function_name
                                    );
                                }
                                self.type_check_expr(ret, expression)?
                            }
                            None => todo!()
                        }
                    }
                    _ => {
                        return Err(format!(
                            "{}: {:?}: Unexpected Expression",
                            ERR_STR,
                            tree.tkn.get_loc()
                        ))
                    }
                };
                Ok(Tree {
                    typ: TreeType::StmtExpr {
                        expression: Box::new(e_t),
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::StmtReturn { return_value } => {
                let func_return_value = self.get_current_function().get_return_type();
                match return_value {
                    Some(ret_val) => {
                        let provided_return = &self.get_expr_type(ret_val, true)?;
                        let new_ret = match (func_return_value.typ, provided_return) {
                            (Type::None, _) => {
                                return Err(format!(
                                    "{}: {:?}: Unexpected return value. Function is declared to return nothing, found return value.",
                                    ERR_STR,
                                    tree.tkn.get_loc()
                                ));
                            }
                            (func_ret, Type::Unknown) => {
                                self.type_check_expr(&func_ret, ret_val)?
                            }
                            (func_ret, prov_ret) => {
                                if func_ret == *prov_ret {
                                    self.type_check_expr(prov_ret, ret_val)?
                                } else {
                                    return Err(format!(
                                        "{}: {:?}: Type Mismatch! Function is declared to return `{:?}`, got `{:?}`.",
                                        ERR_STR,
                                        tree.tkn.get_loc(),
                                        func_ret,
                                        prov_ret
                                    ));
                                }
                            }
                        };
                        Ok(Tree {
                            typ: TreeType::StmtReturn {
                                return_value: Some(Box::new(new_ret)),
                            },
                            tkn: tree.tkn.clone(),
                        })
                    }
                    None => {
                        if !self.match_type(&Type::None, &func_return_value.typ) {
                            Err(format!(
                                "{}: {:?}: Expected return value. Function is declared to return `{:?}`, found empty return.",
                                ERR_STR,
                                tree.tkn.get_loc(),
                                func_return_value.typ
                            ))
                        } else {
                            Ok(Tree {
                                typ: TreeType::StmtReturn { return_value: None },
                                tkn: tree.tkn.clone(),
                            })
                        }
                    }
                }
            }
            _ => {
                tree.print_debug();
                todo!("{:?}", tree.typ)
            }
        }
    }

    fn type_check_expr(&self, expected_type: &Type, expr_tree: &Tree) -> Result<Tree, String> {
        match &expr_tree.typ {
            TreeType::ExprLiteral { typ } => {
                if !self.match_type(&Type::Unknown, typ)
                && !self.match_type(expected_type, typ) {
                    Err(format!(
                        "{}: {:?}: Type Mismatch! Expected type `{:?}`, got type `{:?}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        expected_type,
                        typ
                    ))
                } else {
                    match expected_type {
                        Type::Arr(..) => Err(format!(
                            "{}: {:?}: Attempted to assign ExprLiteral to Array.",
                            ERR_STR,
                            expr_tree.tkn.get_loc()
                        )),
                        Type::Class(..) => Err(format!(
                            "{}: {:?}: Attempted to assign ExprLiteral to Class.",
                            ERR_STR,
                            expr_tree.tkn.get_loc()
                        )),
                        _ => Ok(Tree {
                            typ: TreeType::ExprLiteral {
                                typ: expected_type.clone(),
                            },
                            tkn: expr_tree.tkn.clone(),
                        }),
                    }
                }
            }
            TreeType::ExprBinary { lhs, rhs, typ } => {
                assert!(*typ == Type::Unknown);
                let lhs_type = self.get_expr_type(lhs, true)?;
                let rhs_type = self.get_expr_type(rhs, true)?;

                let (new_lhs, new_rhs) = match (lhs_type, rhs_type) {
                    (Type::Unknown, Type::Unknown) => {
                        let lhs_deep = Box::new(self.type_check_expr(expected_type, lhs)?);
                        let rhs_deep = Box::new(self.type_check_expr(expected_type, rhs)?);
                        (lhs_deep, rhs_deep)
                    }
                    (Type::Unknown, some_type) | (some_type, Type::Unknown) => {
                        let lhs_deep = Box::new(self.type_check_expr(&some_type, lhs)?);
                        let rhs_deep = Box::new(self.type_check_expr(&some_type, rhs)?);
                        (lhs_deep, rhs_deep)
                    }
                    (lhs_type, rhs_type) => {
                        if !self.match_type(&lhs_type, &rhs_type) {
                            return Err(format!(
                                "{}: {:?}: Type Mismatch! Left hand side has type `{:?}`, right side `{:?}`.",
                                ERR_STR,
                                expr_tree.tkn.get_loc(),
                                lhs_type,
                                rhs_type
                            ));
                        } else {
                            let lhs_deep = Box::new(self.type_check_expr(&lhs_type, lhs)?);
                            let rhs_deep = Box::new(self.type_check_expr(&lhs_type, rhs)?);
                            (lhs_deep, rhs_deep)
                        }
                    }
                };
                let lhs_type = self.get_expr_type(&new_lhs, true)?;
                let rhs_type = self.get_expr_type(&new_rhs, true)?;
                if !self.match_type(expected_type, &lhs_type) {
                    Err(format!(
                        "{}: {:?}: Type Mismatch! Expected type `{:?}`, got type `{:?}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        expected_type,
                        lhs_type,
                    ))
                } else if !self.match_type(expected_type, &rhs_type) {
                    Err(format!(
                        "{}: {:?}: Type Mismatch! Expected type `{:?}`, got type `{:?}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        expected_type,
                        rhs_type,
                    ))
                } else {
                    Ok(Tree {
                        typ: TreeType::ExprBinary {
                            lhs: new_lhs,
                            rhs: new_rhs,
                            typ: expected_type.clone(),
                        },
                        tkn: expr_tree.tkn.clone(),
                    })
                }
            }
            TreeType::ExprComp { lhs, rhs, typ } => {
                assert!(*typ == Type::Unknown);
                let lhs_type = self.get_expr_type(lhs, true)?;
                let rhs_type = self.get_expr_type(rhs, true)?;
                let (new_lhs, new_rhs) = match (lhs_type, rhs_type) {
                    (Type::Unknown, Type::Unknown) => {
                        let lhs_type = self.get_expr_type(lhs, false)?;
                        let rhs_type = self.get_expr_type(rhs, false)?;
                        let expected_type = match (lhs_type, rhs_type) {
                            (Type::Unknown, Type::Unknown) => Type::I64,
                            (Type::Unknown, s) | (s, Type::Unknown) => s,
                            (lhs_t, rhs_t) => {
                                if !self.match_type(&lhs_t, &rhs_t) {
                                    return Err(format!(
                                        "{}: {:?}: Type Mismatch! Left hand side has type `{:?}`, right side `{:?}`.",
                                        ERR_STR,
                                        expr_tree.tkn.get_loc(),
                                        lhs_t,
                                        rhs_t
                                    ));
                                }
                                lhs_t
                            }
                        };
                        let lhs_deep = Box::new(self.type_check_expr(&expected_type, lhs)?);
                        let rhs_deep = Box::new(self.type_check_expr(&expected_type, rhs)?);
                        (lhs_deep, rhs_deep)
                    }
                    (Type::Unknown, some_type) | (some_type, Type::Unknown) => {
                        let lhs_deep = Box::new(self.type_check_expr(&some_type, lhs)?);
                        let rhs_deep = Box::new(self.type_check_expr(&some_type, rhs)?);
                        (lhs_deep, rhs_deep)
                    }
                    (lhs_type, rhs_type) => {
                        if !self.match_type(&lhs_type, &rhs_type) {
                            return Err(format!(
                                "{}: {:?}: Type Mismatch! Left hand side has type `{:?}`, right side `{:?}`.",
                                ERR_STR,
                                expr_tree.tkn.get_loc(),
                                lhs_type,
                                rhs_type
                            ));
                        } else {
                            let lhs_deep = Box::new(self.type_check_expr(&lhs_type, lhs)?);
                            let rhs_deep = Box::new(self.type_check_expr(&lhs_type, rhs)?);
                            (lhs_deep, rhs_deep)
                        }
                    }
                };
                let lhs_type = self.get_expr_type(&new_lhs, true)?;
                let rhs_type = self.get_expr_type(&new_rhs, true)?;
                assert!(
                    !self.match_type(&Type::Unknown, &lhs_type),
                    "If this fails, there's a bug in type checking"
                );
                assert!(
                    !self.match_type(&Type::Unknown, &rhs_type),
                    "If this fails, there's a bug in type checking"
                );
                assert!(
                    self.match_type(&lhs_type, &rhs_type),
                    "If this fails, there's a bug in type checking"
                );
                Ok(Tree {
                    typ: TreeType::ExprComp {
                        lhs: new_lhs,
                        rhs: new_rhs,
                        typ: expected_type.clone(),
                    },
                    tkn: expr_tree.tkn.clone(),
                })
            }
            TreeType::ExprParen { expression, typ } => {
                assert!(*typ == Type::Unknown);
                let e_t = self.type_check_expr(expected_type, expression)?;
                Ok(Tree {
                    typ: TreeType::ExprParen {
                        expression: Box::new(e_t),
                        typ: expected_type.clone(),
                    },
                    tkn: expr_tree.tkn.clone(),
                })
            }
            TreeType::ExprName { name, typ } => {
                assert!(*typ == Type::Unknown);
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var_type) => {
                        let typ = &var_type.typ;
                        if !self.match_type(expected_type, typ) {
                            Err(format!(
                                "{}: {:?}: Type Mismatch! Expected type `{:?}`, got type `{:?}`.\n{}: Variable declared here: {:?}",
                                ERR_STR,
                                expr_tree.tkn.get_loc(),
                                expected_type,
                                typ,
                                NOTE_STR,
                                var_type.loc
                            ))
                        } else {
                            Ok(Tree {
                                typ: TreeType::ExprName {
                                    name: name.clone(),
                                    typ: expected_type.clone(),
                                },
                                tkn: expr_tree.tkn.clone(),
                            })
                        }
                    }
                    None => {
                        if self.classes.contains_key(name) {
                            Ok(Tree {
                                typ: TreeType::ExprName {
                                    name: name.clone(),
                                    typ: Type::Class(name.clone())
                                },
                                tkn: expr_tree.tkn.clone()
                            })
                        } else {
                            match BUILT_IN_VARS.get(name) {
                                Some(var) => Ok(Tree {
                                    typ: TreeType::ExprName {
                                        name: name.clone(),
                                        typ: var.typ.clone()
                                    },
                                    tkn: expr_tree.tkn.clone()
                                    }
                                ),
                                None => Err(format!(
                                    "{}: {:?}: Undefined variable `{}`.",
                                    ERR_STR,
                                    expr_tree.tkn.get_loc(),
                                    name
                                ))
                            }
                        }
                    },
                }
            }
            TreeType::ExprCall {
                function_name,
                args,
                typ,
            } => {
                match self.functions.get(function_name) {
                    Some(func) => {
                        let params = &func.parameters;
                        match &args.typ {
                            TreeType::ArgList { arguments } => {
                                let args = self.type_check_args(function_name, args, params)?;
                                let typ = &func.return_type.typ;
                                if !self.match_type(expected_type, typ) {
                                    Err(format!(
                                        "{}: {:?}: Type Mismatch! Expected Type `{:?}`, got Type `{:?}`.\n{}: {:?}: Function `{}` declared to return `{:?}` here.",
                                        ERR_STR,
                                        expr_tree.tkn.get_loc(),
                                        expected_type,
                                        typ,
                                        NOTE_STR,
                                        func.loc,
                                        function_name,
                                        typ
                                    ))
                                } else {
                                    Ok(Tree {
                                        typ: TreeType::ExprCall {
                                            function_name: function_name.clone(),
                                            args: Box::new(args),
                                            typ: typ.clone(),
                                        },
                                        tkn: expr_tree.tkn.clone(),
                                    })
                                }
                            }
                            _ => panic!(),
                        }
                    }
                    None => Err(format!(
                        "{}: {:?}: Unknown function `{}`",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        function_name
                    )),
                }
            }
            TreeType::Arg { expression } => {
                let e_t = self.type_check_expr(expected_type, expression)?;
                Ok(Tree {
                    typ: TreeType::Arg {
                        expression: Box::new(e_t),
                    },
                    tkn: expr_tree.tkn.clone(),
                })
            }
            TreeType::ExprArrLiteral { elements } => {
                let (typ, size) = match expected_type {
                    Type::Arr(t, s) => {
                        if s.len() == 1 {
                            (t.clone(), s[0])
                        } else {
                            (Box::new(Type::Arr(t.clone(), s[1..].to_vec())), s[0])
                        }
                    }
                    _ => {
                        return Err(format!(
                            "{}: {:?}: Unexpected Array Literal",
                            ERR_STR,
                            expr_tree.tkn.get_loc()
                        ))
                    }
                };
                if size == 0 {
                    return Err(format!(
                        "{}: {:?}: Attempted to initialize Array of Size 0.",
                        ERR_STR,
                        expr_tree.tkn.get_loc()
                    ));
                }
                match elements.len().cmp(&size) {
                    std::cmp::Ordering::Less | std::cmp::Ordering::Greater => Err(format!(
                        "{}: {:?}: Size Mismatch for Array Literal. Expected {} elements, got {} elements.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        size,
                        elements.len()
                    )),
                    std::cmp::Ordering::Equal => {
                        let mut children = vec![];
                        for e in elements {
                            match (&e.typ, *typ.clone()) {
                                (TreeType::ExprArrLiteral { .. }, Type::Arr(_, _)) => {
                                    children.push(self.type_check_expr(&typ, e)?);
                                }
                                (_, Type::Arr(_, _)) => {
                                    return Err(format!(
                                        "{}: {:?}: Unexpected Expression, expected Array Literal",
                                        ERR_STR,
                                        expr_tree.tkn.get_loc()
                                    ))
                                }
                                (_, _) => children.push(self.type_check_expr(&typ, e)?)
                            }
                        }
                        Ok(Tree { typ: TreeType::ExprArrLiteral { elements: children }, tkn: expr_tree.tkn.clone() })
                    }
                }
            }
            TreeType::ExprArrAccess {
                arr_name,
                indices,
                typ,
            } => {
                assert!(*typ == Type::Unknown);
                let func = self.get_current_function();
                match func.get_variable(arr_name) {
                    Some(var_type) => {
                        let index_type = match &var_type.typ {
                            Type::Arr(at, size) => {
                                if !self.match_type(expected_type, at) {
                                    return Err(format!(
                                        "{}: {:?}: Type Mismatch! Expected type `{:?}`, got type `{:?}`.\n{}: Variable declared here: {:?}",
                                        ERR_STR,
                                        expr_tree.tkn.get_loc(),
                                        expected_type,
                                        at,
                                        NOTE_STR,
                                        var_type.loc
                                    ));
                                }
                                Type::Arr(Box::new(Type::Usize), vec![size.len()])
                            }
                            _ => todo!(),
                        };
                        let i_t = self.type_check_expr(&index_type, indices)?;
                        Ok(Tree {
                            typ: TreeType::ExprArrAccess {
                                arr_name: arr_name.clone(),
                                indices: Box::new(i_t),
                                typ: var_type.typ.clone(),
                            },
                            tkn: expr_tree.tkn.clone(),
                        })
                    }
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        arr_name
                    )),
                }
            }
            TreeType::FieldAccess { name, field, typ } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => {
                        match &var.typ {
                            Type::Class(class_name) => {
                                let a_t = self.type_check_field_access(class_name, field)?;
                                let t = self.get_field_type(class_name, field)?;
                                if !self.match_type(expected_type, &t) {
                                    Err(format!(
                                        "{}: {:?}: Type Mismatch when accessing class field. Expected type `{:?}`, got `{:?}`.",
                                        ERR_STR,
                                        expr_tree.tkn.get_loc(),
                                        expected_type,
                                        t
                                    ))
                                } else {
                                    Ok(Tree {
                                        typ: TreeType::FieldAccess {
                                            name: name.clone(),
                                            field: Box::new(a_t),
                                            typ: var.typ.clone()
                                        },
                                        tkn: expr_tree.tkn.clone()
                                    })
                                }
                            }
                            _ => todo!()
                        }
                    }
                    None => todo!()
                }
            }
            TreeType::BuiltInFunction { function_name, args, typ } => {
                match BUILT_IN_FUNC.get(function_name) {
                    Some(builtin) => {
                        let args = self.type_check_args(function_name, args, &builtin.parameters)?;
                        let typ = builtin.get_return_type().typ;
                        let hit = self.match_type(expected_type, &typ);
                        if !hit {
                            Err(format!(
                                "{}: {:?}: Type Mismatch! Expected Type `{:?}`, got Type `{:?}`.\n{}: {:?}: Function `{}` declared to return `{:?}` here.",
                                ERR_STR,
                                expr_tree.tkn.get_loc(),
                                expected_type,
                                typ,
                                NOTE_STR,
                                builtin.loc,
                                function_name,
                                typ
                            ))
                        } else {
                            Ok( Tree {
                                typ: TreeType::BuiltInFunction {
                                    function_name: function_name.clone(),
                                    args: Box::new(args),
                                    typ: expected_type.clone()
                                },
                                tkn: expr_tree.tkn.clone()
                            })
                        }
                    },
                    None => panic!("FOR SOME REASON BUILT_IN_FUNC `{}` WAS NOT FOUND AHHHHHH", function_name)
                }
            }
            _ => {
                expr_tree.print_debug();
                print!("Caused by expression `");
                print!("{}", expr_tree.rebuild_code());
                println!("` here: {:?}", expr_tree.tkn.get_loc());
                todo!()
            }
        }
    }

    fn type_check_field_access(&self, class_name: &String, field: &Tree) -> Result<Tree, String> {
        match self.classes.get(class_name) {
            Some(class) => {
                match &field.typ {
                    TreeType::FieldAccess { name, field: fld, typ } => {
                        match class.get_field(name) {
                            Some(f) => {
                                if let Type::Class(c) = &f.typ {
                                    let a_t = self.type_check_field_access(c, fld)?;
                                    Ok(Tree {
                                        typ: TreeType::FieldAccess {
                                            name: name.clone(),
                                            field: Box::new(a_t),
                                            typ: f.typ
                                        },
                                        tkn: field.tkn.clone()
                                    })
                                } else {
                                    todo!()
                                }
                            }
                            None => todo!()
                        }
                    }
                    TreeType::ExprName { name, typ } => {
                        assert!(self.match_type(&Type::Unknown, typ));
                        match class.get_field(name) {
                            Some(f) => {
                                Ok(Tree {
                                    typ: TreeType::ExprName {
                                        name: name.clone(),
                                        typ: f.typ
                                    },
                                    tkn: field.tkn.clone()
                                })
                            }
                            None => todo!()
                        }
                    }
                    _ => {
                        Err(format!(
                            "{}: {:?}: Calling functions on class instances is not supported yet.",
                            ERR_STR,
                            field.tkn.get_loc()
                        ))
                    }
                }
            }
            None => panic!()
        }
    }

    fn type_check_args(
        &self,
        function_name: &String,
        args: &Tree,
        params: &IndexMap<String, TCVariable>,
    ) -> Result<Tree, String> {
        match &args.typ {
            TreeType::ArgList { arguments } => {
                let mut children = vec![];
                match arguments.len().cmp(&params.len()) {
                    std::cmp::Ordering::Less => return Err(format!(
                        "{}: {:?}: Too few arguments specified for function call. Expected {} arguments, got {}.",
                        ERR_STR,
                        args.tkn.get_loc(),
                        params.len(),
                        arguments.len()
                    )),
                    std::cmp::Ordering::Greater => return Err(format!(
                        "{}: {:?}: Too many arguments specified for function call. Expected {} arguments, got {}.",
                        ERR_STR,
                        args.tkn.get_loc(),
                        params.len(),
                        arguments.len()
                    )),
                    std::cmp::Ordering::Equal => {
                        for i in 0..arguments.len() {
                            let arg = &arguments[i];
                            let par = &params[i];
                            let param_type = &par.typ;
                            let a_t = match self.type_check_expr(param_type, arg) {
                                Ok(t) => t,
                                Err(e) => return Err(format!(
                                    "{e}\n{}: {:?}: Error when evaluating type of argument.\n{}: {:?}: Parameter `{}` for function `{}` declared here.",
                                    NOTE_STR,
                                    arg.tkn.get_loc(),
                                    NOTE_STR,
                                    par.loc,
                                    params.keys().collect::<Vec<_>>()[i],
                                    function_name,
                                ))
                            };
                            children.push(a_t);
                        }
                    },
                }

                Ok(Tree {
                    typ: TreeType::ArgList {
                        arguments: children,
                    },
                    tkn: args.tkn.clone(),
                })
            }
            _ => panic!(),
        }
    }

    fn get_expr_type(&self, expr: &Tree, strict: bool) -> Result<Type, String> {
        match &expr.typ {
            TreeType::ExprBinary { lhs, rhs, typ } => {
                if !self.match_type(&Type::Unknown, typ) {
                    Ok(typ.clone())
                } else {
                    let l = self.get_expr_type(lhs, strict)?;
                    let r = self.get_expr_type(rhs, strict)?;
                    match (l, r) {
                        (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
                        (Type::Unknown, s) | (s, Type::Unknown) => {
                            if strict {
                                Ok(Type::Unknown)
                            } else {
                                Ok(s)
                            }
                        }
                        (left_type, right_type) => {
                            if !self.match_type(&left_type, &right_type) {
                                Err(format!(
                                    "{}: {:?}: Type Mismatch! Left hand side has type `{:?}`, right side `{:?}`.",
                                    ERR_STR,
                                    expr.tkn.get_loc(),
                                    left_type,
                                    right_type
                                ))
                            } else {
                                Ok(left_type)
                            }
                        }
                    }
                }
            }
            TreeType::ExprComp { lhs, rhs, typ } => {
                if !self.match_type(&Type::Unknown, typ) {
                    Ok(typ.clone())
                } else {
                    let l = self.get_expr_type(lhs, strict)?;
                    let r = self.get_expr_type(rhs, strict)?;
                    match (l, r) {
                        (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
                        (Type::Unknown, s) | (s, Type::Unknown) => {
                            if strict {
                                Ok(Type::Unknown)
                            } else {
                                Ok(s)
                            }
                        }
                        (left_type, right_type) => {
                            if !self.match_type(&left_type, &right_type) {
                                todo!()
                            } else {
                                Ok(Type::Bool)
                            }
                        }
                    }
                }
            }
            TreeType::ExprName { name, typ } => {
                if !self.match_type(&Type::Unknown, typ) {
                    Ok(typ.clone())
                } else {
                    let func = self.get_current_function();
                    match func.get_variable(name) {
                        Some(var_type) => {
                            let typ = &var_type.typ;
                            Ok(typ.clone())
                        }
                        None => {
                            match BUILT_IN_VARS.get(name) {
                                Some(var) => Ok(var.typ.clone()),
                                None => Err(format!(
                                    "{}: {:?}: Undefined variable `{}`.",
                                    ERR_STR,
                                    expr.tkn.get_loc(),
                                    name
                                ))
                            }
                        },
                    }
                }
            }
            TreeType::ExprCall { function_name, .. } => {
                match self.functions.get(function_name) {
                    Some(func) => Ok(func.get_return_type().typ),
                    None => Err(format!(
                        "{}: {:?}: Unknown function `{}`",
                        ERR_STR,
                        expr.tkn.get_loc(),
                        function_name
                    )),
                }
            }
            TreeType::ExprLiteral { typ } => Ok(typ.clone()),
            TreeType::ExprParen { expression, typ } => {
                if !self.match_type(&Type::Unknown, typ) {
                    Ok(typ.clone())
                } else {
                    self.get_expr_type(expression, strict)
                }
            }
            TreeType::ExprArrAccess { arr_name, .. } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(arr_name) {
                    Some(var) => match &var.typ {
                        Type::Arr(t, _) => Ok(*t.clone()),
                        t => Err(format!(
                            "{}: {:?}: Attempted to index into non-array variable `{}`.\n{}: {:?}: Variable declared to be of type `{:?}` here.",
                            ERR_STR,
                            expr.tkn.get_loc(),
                            arr_name,
                            NOTE_STR,
                            var.loc,
                            t
                        )),
                    },
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`.",
                        ERR_STR,
                        expr.tkn.get_loc(),
                        arr_name
                    )),
                }
            }
            TreeType::ExprArrLiteral { elements } => {
                todo!()
            }
            TreeType::FieldAccess { name, field, typ } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => {
                        if let Type::Class(class) = &var.typ {
                            self.get_field_type(class, field)
                        } else {
                            todo!()
                        }
                    }
                    None => {
                        Err(format!(
                            "{}: {:?}: Undefined variable `{}`.",
                            ERR_STR,
                            expr.tkn.get_loc(),
                            name
                        ))
                    }
                }
            }
            _ => {
                expr.print_debug();
                todo!();
            }
        }
    }

    fn get_field_type(&self, class_name: &String, field: &Tree) -> Result<Type, String> {
        match self.classes.get(class_name) {
            Some(class) => {
                match &field.typ {
                    TreeType::FieldAccess { name, field: f, typ } => {
                        if let Some(var) = class.get_field(name) {
                            if let Type::Class(cls) = &var.typ {
                                self.get_field_type(cls, f)
                            } else {
                                Err(format!(
                                    "{}: {:?}: Attempted to get field of non-class element `{}`.\n{}: {:?}: Variable declared here.",
                                    ERR_STR,
                                    field.tkn.get_loc(),
                                    name,
                                    NOTE_STR,
                                    var.loc
                                ))
                            }
                        } else {
                            Err(format!(
                                "{}: {:?}: Class `{}` has no field `{}`.\n{}: {:?}: Class declared here.",
                                ERR_STR,
                                field.tkn.get_loc(),
                                class_name,
                                name,
                                NOTE_STR,
                                class.loc
                            ))
                        }
                    }
                    TreeType::ExprName { name, typ } => {
                        if let Some(t) = class.get_field(name) {
                            Ok(t.typ)
                        } else {
                            Err(format!(
                                "{}: {:?}: Class `{}` has no field `{}`.\n{}: {:?}: Class declared here.",
                                ERR_STR,
                                field.tkn.get_loc(),
                                class_name,
                                name,
                                NOTE_STR,
                                class.loc
                            ))
                        }
                    }
                    _ => panic!()
                }
            }
            None => panic!()
        }
    }

    fn get_type(&self, tree: &Tree) -> Result<Type, String> {
        match &tree.typ {
            TreeType::TypeDecl { typ } => {
                Ok(typ.clone())
            }
            _ => todo!(),
        }
    }

    fn match_type(&self, expected: &Type, received: &Type) -> bool {
        match (expected, received) {
            (Type::Any, Type::Any) => panic!(),
            (Type::Any, other) | (other, Type::Any) => true,
            (lhs, rhs) => lhs == rhs
        }
    }
}
