use indexmap::IndexMap;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::codegen::{ERR_STR, NOTE_STR, WARN_STR};
use crate::lexer::Location;
use crate::parser::{Tree, TreeType};

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
    // Ptr(Box<Type>),
    Custom(String),
    Arr(Box<Type>, Vec<usize>),
    // Reserved for later use
    F32,
    F64,
}

impl Display for Type {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            // Type::Ptr(t) => write!(fmt, "&{}", t),
            Type::Arr(t, s) => write!(fmt, "{}", format!("{t}{s:?}")),
            Type::Custom(s) => write!(fmt, "{}", s),
            _ => write!(fmt, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Debug, Clone)]
struct TCVariable {
    loc: Location,
    typ: Type,
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
struct TCStruct {
    fields: IndexMap<String, TCVariable>,
    loc: Location
}

impl TCStruct {
    fn new(loc: Location) -> Self {
        Self {
            fields: IndexMap::new(),
            loc,
        }
    }

    fn has_field(&self, name: &String) -> bool {
        self.fields.contains_key(name)
    }

    fn get_field(&self, name: &String) -> Option<TCVariable> {
        self.fields.get(name).cloned()
    }
    fn add_field(&mut self, v_name: &String, var: TCVariable) -> Result<(), String> {
        match self.get_field(v_name) {
            Some(v) => Err(format!(
                "Struct Field redefinition!\n{}: Field already declared here: {:?}",
                NOTE_STR, v.loc
            )),
            None => {
                self.fields.insert(v_name.clone(), var);
                Ok(())
            }
        }
    }
}

pub struct TypeChecker {
    ast: Tree,
    functions: HashMap<String, TCFunction>,
    structs: HashMap<String, TCStruct>,
    current_fn: String,
    current_str: String,
    scope_depth: usize,
    #[allow(unused)]
    print_debug: bool,
}

impl TypeChecker {
    pub fn new(ast: &Tree, print_debug: bool) -> Self {
        Self {
            ast: ast.clone(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            current_fn: String::new(),
            current_str: String::new(),
            scope_depth: 0,
            print_debug,
        }
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
        self.type_check(&self.ast.clone())
    }

    fn type_check(&mut self, tree: &Tree) -> Result<Tree, String> {
        // println!("{:?}", tree.tkn);
        match &tree.typ {
            TreeType::File { functions } => {
                let mut children = vec![];
                for c in functions {
                    children.push(self.type_check(c)?);
                }
                Ok(Tree {
                    typ: TreeType::File {
                        functions: children,
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::Struct { name, fields } => {
                assert!(self.current_fn.is_empty());
                assert!(self.current_str.is_empty());

                self.current_str = name.clone();
                if let Some(s) = self.structs.get(name) {
                    return Err(format!(
                        "{}: {:?}: Struct redefinition\n{}: {:?}: Struct already defined here",
                        ERR_STR,
                        tree.tkn.get_loc(),
                        NOTE_STR,
                        s.loc
                    ));
                }
                let mut str = TCStruct::new(tree.tkn.get_loc());
                self.structs.insert(name.clone(), str);
                
                let f_t = self.type_check(fields)?;

                self.current_str.clear();
                Ok(Tree {
                    typ: TreeType::Struct {
                        name: name.clone(),
                        fields: Box::new(f_t)
                    },
                    tkn: tree.tkn.clone()
                })
            }
            TreeType::FieldList { elements } => {
                for e in elements {
                    self.type_check(e)?;
                }
                Ok(tree.clone())
            }
            TreeType::Field { name, expr } => {
                let f_type = self.get_type(expr)?;
                let str = self.structs.get_mut(&self.current_str).unwrap();
                if let Err(e) = str.add_field(name, TCVariable::new(&tree.tkn.get_loc(), &f_type)) {
                    Err(format!("{}: {:?}: {}", ERR_STR, tree.tkn.get_loc(), e))
                } else {
                    Ok(tree.clone())
                }
            }
            TreeType::Func {
                name,
                return_type,
                param,
                block,
            } => {
                assert!(self.current_fn.is_empty());

                self.current_fn = name.clone();
                if let Some(func) = self.functions.get(&self.current_fn) {
                    return Err(format!(
                        "{}: {:?}: Function redefinition\n{}: {:?}: Function already defined here",
                        ERR_STR,
                        tree.tkn.get_loc(),
                        NOTE_STR,
                        func.loc
                    ));
                }

                let typ = if let Some(rt) = return_type {
                    self.get_type(rt)?
                } else {
                    Type::None
                };
                let mut func = TCFunction::new(
                    tree.tkn.get_loc(),
                    TCVariable {
                        loc: tree.tkn.get_loc(),
                        typ,
                    },
                );

                self.functions.insert(self.current_fn.clone(), func);

                let p_t = self.type_check(param)?;

                let b_t = self.type_check(block)?;

                self.current_fn.clear();
                Ok(Tree {
                    typ: TreeType::Func {
                        name: name.clone(),
                        return_type: return_type.clone(),
                        param: Box::new(p_t),
                        block: Box::new(b_t),
                    },
                    tkn: tree.tkn.clone(),
                })
            }
            TreeType::ParamList { parameters } => {
                for p in parameters {
                    self.type_check(p)?;
                }
                Ok(tree.clone())
            }
            TreeType::Param { name, typ } => {
                let p_type = self.get_type(typ)?;

                let func = self.functions.get_mut(&self.current_fn).unwrap();
                if let Err(e) =
                    func.add_parameter(name, TCVariable::new(&tree.tkn.get_loc(), &p_type))
                {
                    Err(format!("{}: {:?}: {}", ERR_STR, tree.tkn.get_loc(), e))
                } else {
                    Ok(tree.clone())
                }
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
                                if *ret != Type::None {
                                    println!("{}: {:?}: Ignoring return value of function call to `{}()`",
                                        WARN_STR,
                                        tree.tkn.get_loc(),
                                        function_name
                                    );
                                }
                                self.type_check_expr(ret, expression)?
                            }
                            None => todo!(),
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
                                        "{}: {:?}: Type Mismatch. Function is declared to return `{:?}`, got `{:?}`.",
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
                        if func_return_value.typ != Type::None {
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
                if *typ != Type::Unknown && typ != expected_type {
                    Err(format!(
                        "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
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
                        Type::Custom(s) => Err(format!(
                            "{}: {:?}: Attempted to assign ExprLiteral to Struct.",
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
                        if lhs_type != rhs_type {
                            return Err(format!(
                                "{}: {:?}: Type Mismatch. Left hand side has type `{:?}`, right side `{:?}`.",
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
                if lhs_type != *expected_type {
                    Err(format!(
                        "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        expected_type,
                        lhs_type,
                    ))
                } else if rhs_type != *expected_type {
                    Err(format!(
                        "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.",
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
                                if lhs_t != rhs_t {
                                    return Err(format!(
                                        "{}: {:?}: Type Mismatch. Left hand side has type `{:?}`, right side `{:?}`.",
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
                        if lhs_type != rhs_type {
                            return Err(format!(
                                "{}: {:?}: Type Mismatch. Left hand side has type `{:?}`, right side `{:?}`.",
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
                    lhs_type != Type::Unknown,
                    "If this fails, there's a bug in type checking"
                );
                assert!(
                    rhs_type != Type::Unknown,
                    "If this fails, there's a bug in type checking"
                );
                assert!(
                    lhs_type == rhs_type,
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
                        if typ != expected_type {
                            Err(format!(
                                "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.\n{}: Variable declared here: {:?}",
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
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`.",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        name
                    )),
                }
            }
            TreeType::ExprCall {
                function_name,
                args,
                typ,
            } => {
                assert!(*typ == Type::Unknown);
                match self.functions.get(function_name) {
                    Some(func) => {
                        let params = &func.parameters;
                        match &args.typ {
                            TreeType::ArgList { arguments } => {
                                let args = self.type_check_args(function_name, args, params)?;
                                let typ = &func.return_type.typ;
                                if typ != expected_type {
                                    Err(format!(
                                        "{}: {:?}: Type Mismatch. Expected Type `{:?}`, got Type `{:?}`.\n{}: {:?}: Function `{}` declared to return `{:?}` here.",
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
            TreeType::Name { name } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => {
                        let var_type = &var.typ;
                        if var_type != expected_type {
                            todo!();
                        }
                        Ok(Tree {
                            typ: TreeType::Name { name: name.clone() },
                            tkn: expr_tree.tkn.clone(),
                        })
                    }
                    None => todo!(),
                }
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
                                if *at.to_owned() != *expected_type {
                                    return Err(format!(
                                        "{}: {:?}: Type mismatch! Expected type `{:?}`, got type `{:?}`.\n{}: Variable declared here: {:?}",
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
            TreeType::ExprStructDecl { name, fields } => {
                match self.structs.get(name) {
                    Some(str) => {
                        if *expected_type != Type::Custom(name.clone()) {
                            return Err(format!(
                                "{}: {:?}: Type mismatch! Expected type `{:?}`, got struct `{}`.\n{}: Struct declared here: {:?}.",
                                ERR_STR,
                                expr_tree.tkn.get_loc(),
                                expected_type,
                                Type::Custom(name.clone()),
                                NOTE_STR,
                                str.loc
                            ));
                        }
                        let def_fields = &str.fields;
                        match &fields.typ {
                            TreeType::FieldList { elements } => {
                                let f_t = self.type_check_fields(name, def_fields, fields)?;
                                Ok(Tree {
                                    typ: TreeType::ExprStructDecl {
                                        name: name.clone(),
                                        fields: Box::new(f_t)
                                    },
                                    tkn: expr_tree.tkn.clone(),
                                })
                            }
                            _ => panic!(),
                        }
                    },
                    None => Err(format!(
                        "{}: {:?}: Unknown struct `{}`",
                        ERR_STR,
                        expr_tree.tkn.get_loc(),
                        name
                    ))
                }
                // todo!();
            }
            TreeType::ExprStructAccess { name, field, typ } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => {
                        match &var.typ {
                            Type::Custom(struct_name) => {
                                let a_t = self.type_check_struct_access(struct_name, field)?;
                                Ok(Tree {
                                    typ: TreeType::ExprStructAccess {
                                        name: name.clone(),
                                        field: Box::new(a_t),
                                        typ: var.typ.clone()
                                    },
                                    tkn: expr_tree.tkn.clone()
                                })
                            }
                            _ => todo!()
                        }
                    }
                    None => todo!(),
                }
            }
            _ => {
                expr_tree.print_debug();
                print!("Caused by expression `");
                expr_tree.rebuild_code();
                println!("` here: {:?}", expr_tree.tkn.get_loc());
                todo!()
            }
        }
    }

    fn type_check_struct_access(&self, struct_name: &String, field: &Tree) -> Result<Tree, String> {
        match self.structs.get(struct_name) {
            Some(str) => {
                match &field.typ {
                    TreeType::ExprStructAccess { name, field: fld, typ } => {
                        match str.get_field(name) {
                            Some(f) => {
                                if let Type::Custom(s) = &f.typ {
                                    let a_t = self.type_check_struct_access(s, fld)?;
                                    Ok(Tree {
                                        typ: TreeType::ExprStructAccess {
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
                        assert!(*typ == Type::Unknown);
                        match str.get_field(name) {
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
                    _ => todo!()
                }
            },
            None => panic!()
        }
    }

    fn type_check_fields(
        &self,
        struct_name: &String,
        fields: &IndexMap<String, TCVariable>,
        list: &Tree,
    ) -> Result<Tree, String> {
        match &list.typ {
            TreeType::FieldList { elements } => {
                let mut children = vec![];
                let mut found = IndexMap::new();
                for f in fields {
                    found.insert(f.0, false);
                }
                
                for i in 0..elements.len() {
                    let (name, field) = match &elements[i].typ {
                        TreeType::Field { name, expr } => {
                            if !fields.contains_key(name) {
                                return Err(format!(
                                    "{}: {:?}: Struct `{}` has no field `{}`.\n{}: {:?}: Struct declared here.",
                                    ERR_STR,
                                    elements[i].tkn.get_loc(),
                                    struct_name,
                                    name,
                                    NOTE_STR,
                                    self.structs.get(struct_name).unwrap().loc,
                                ))
                            }
                            (name, expr)
                        }
                        _ => todo!()
                    };
                    let found_field = found.get_mut(name).unwrap();
                    if *found_field {
                        return Err(format!(
                            "{}: {:?}: Field `{}` of struct `{}` already initialized.",
                            ERR_STR,
                            field.tkn.get_loc(),
                            name,
                            struct_name
                        ))
                    } else {
                        *found_field = true;
                    }
                    let def_field = &fields[i];
                    let field_type = &def_field.typ;
                    let a_t = match self.type_check_expr(field_type, field) {
                        Ok(t) => t,
                        Err(e) => return Err(format!(
                            "{e}\n{}: {:?}: Error when evaluating type of struct field.\n{}: {:?}: Field `{}` for Struct `{}` declared here.",
                            NOTE_STR,
                            list.tkn.get_loc(),
                            NOTE_STR,
                            def_field.loc,
                            fields.keys().collect::<Vec<_>>()[i],
                            struct_name,
                        ))
                    };
                    let a_t = Tree {
                        typ: TreeType::Field { name: name.clone(), expr: Box::new(a_t) },
                        tkn: field.tkn.clone()
                    };
                    children.push(a_t);
                }
                if let Some(index) = found.iter().position(|(k, v)| !v) {
                    Err(format!(
                        "{}: {:?}: Struct Initialization is missing field `{}`.\n{}: {:?}: Struct `{}` declared here.",
                        ERR_STR,
                        list.tkn.get_loc(),
                        found.keys().collect::<Vec<_>>()[index],
                        NOTE_STR,
                        self.structs.get(struct_name).unwrap().loc,
                        struct_name
                    ))
                } else {
                    Ok(Tree {
                        typ: TreeType::FieldList { elements: children },
                        tkn: list.tkn.clone()
                    })
                }
            }
            _ => panic!()
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
                if *typ != Type::Unknown {
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
                            if left_type != right_type {
                                Err(format!(
                                    "{}: {:?}: Type Mismatch. Left hand side has type `{:?}`, right side `{:?}`.",
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
                if *typ != Type::Unknown {
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
                            if left_type != right_type {
                                todo!()
                            } else {
                                Ok(Type::Bool)
                            }
                        }
                    }
                }
            }
            TreeType::ExprName { name, typ } => {
                if *typ != Type::Unknown {
                    Ok(typ.clone())
                } else {
                    let func = self.get_current_function();
                    match func.get_variable(name) {
                        Some(var_type) => {
                            let typ = &var_type.typ;
                            Ok(typ.clone())
                        }
                        None => Err(format!(
                            "{}: {:?}: Undefined variable `{}`.",
                            ERR_STR,
                            expr.tkn.get_loc(),
                            name
                        )),
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
                if *typ != Type::Unknown {
                    Ok(typ.clone())
                } else {
                    self.get_expr_type(expression, strict)
                }
            }
            TreeType::Name { name } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => Ok(var.typ.clone()),
                    None => panic!(),
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
            TreeType::ExprStructAccess { name, field, typ } => {
                assert!(!self.current_fn.is_empty());
                let func = self.get_current_function();
                match func.get_variable(name) {
                    Some(var) => {
                        if let Type::Custom(str) = &var.typ {
                            self.unwind_field_type(str, field)
                        } else {
                            todo!()
                        }
                    },
                    None => Err(format!(
                        "{}: {:?}: Undefined variable `{}`.",
                        ERR_STR,
                        expr.tkn.get_loc(),
                        name
                    ))
                }

            }
            _ => {
                expr.print_debug();
                todo!();
            }
        }
    }

    fn unwind_field_type(&self, struct_name: &String, field: &Tree) -> Result<Type, String> {
        match self.structs.get(struct_name) {
            Some(custom_struct) => {
                match &field.typ {
                    TreeType::ExprStructAccess { name, field: f, typ } => {
                        if let Some(var) = custom_struct.get_field(name) {
                            if let Type::Custom(str) = &var.typ {
                                self.unwind_field_type(str, f)
                            } else {
                                Err(format!(
                                    "{}: {:?}: Attempted to get field of non-struct element `{}`.\n{}: {:?}: Variable declared here.",
                                    ERR_STR,
                                    field.tkn.get_loc(),
                                    name,
                                    NOTE_STR,
                                    var.loc
                                ))    
                            }
                        } else {
                            Err(format!(
                                "{}: {:?}: Struct `{}` has no field `{}`.\n{}: {:?}: Struct declared here.",
                                ERR_STR,
                                field.tkn.get_loc(),
                                struct_name,
                                name,
                                NOTE_STR,
                                custom_struct.loc                                    
                            ))
                        }
                    }
                    TreeType::ExprName { name, typ } => {
                        if let Some(t) = custom_struct.get_field(name) {
                            Ok(t.typ)
                        } else {
                            Err(format!(
                                "{}: {:?}: Struct `{}` has no field `{}`.\n{}: {:?}: Struct declared here.",
                                ERR_STR,
                                field.tkn.get_loc(),
                                struct_name,
                                name,
                                NOTE_STR,
                                custom_struct.loc                                    
                            ))
                        }
                    }
                    _ => panic!()
                }
            },
            None => panic!()
        }
        
    }

    fn get_type(&self, tree: &Tree) -> Result<Type, String> {
        match &tree.typ {
            TreeType::TypeDecl { typ } => Ok(typ.clone()),
            _ => todo!(),
        }
    }
}
