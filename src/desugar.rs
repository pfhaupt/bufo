use std::collections::HashMap;

use crate::Compiler;
use crate::parser::{Tree, TreeType, Parser};
use crate::lexer::TokenType;
use crate::checker::Type;
use crate::codegen::SizeManager;

pub struct Desugarer {
    current_class: String,
    class_sizes: HashMap<String, usize>,
    sm: SizeManager,
}

impl Desugarer {
    pub fn new() -> Self {
        Self {
            current_class: String::new(),
            class_sizes: HashMap::new(),
            sm: SizeManager {},
        }
    }
    fn get_type(&self, typ: Tree) -> Type {
        match typ.typ {
            TreeType::TypeDecl { typ } => typ,
            _ => panic!()
        }
    }
    fn fill_lookup(&mut self, ast: Tree) -> Result<(), String> {
        match ast.typ {
            TreeType::File { functions, classes } => {
                for c in classes {
                    self.fill_lookup(c)?;
                }
            }
            TreeType::Class { name, fields, functions, features } => {
                assert!(self.current_class.is_empty());
                self.current_class = name;
                self.class_sizes.insert(self.current_class.clone(), 0);

                for f in fields {
                    self.fill_lookup(f)?;
                }

                self.current_class.clear();
            }
            TreeType::Field { name, typ } => {
                let typ = self.get_type(*typ);
                let c = *self.class_sizes
                    .get(&self.current_class)
                    .unwrap();
                self.class_sizes.insert(self.current_class.clone(), c + self.sm.get_type_size(&typ));
            }
            _ => {}
        }
        Ok(())
    }

    pub fn desugar_tree(&mut self, ast: Tree) -> Result<Tree, String> {
        self.fill_lookup(ast.clone())?;
        self.desugar_tree_internal(ast)
    }
    fn desugar_tree_internal(&mut self, ast: Tree) -> Result<Tree, String> {
        let tkn = ast.tkn.clone();
        Ok(match ast.typ.clone() {
            TreeType::File { functions, classes } => {
                let mut desugared_functions = vec![];
                let mut desugared_classes = vec![];
                for f in functions {
                    desugared_functions.push(self.desugar_tree_internal(f)?);
                }
                for c in classes {
                    if let TreeType::Class { name, fields: this, functions, features } = self.desugar_tree_internal(c)?.typ {
                        assert!(this.len() == 1);
                        desugared_classes.extend(this);
                        let mut df = functions;
                        df.extend(desugared_functions);
                        desugared_functions = df;
                        
                        let mut df = features;
                        df.extend(desugared_functions);
                        desugared_functions = df;
                    } else {
                        unreachable!();
                    }
                }
                Tree {
                    typ: TreeType::File { functions: desugared_functions, classes: desugared_classes },
                    tkn
                }
            }
            TreeType::Class { name, fields, functions, features } => {
                let added_this = Tree {
                    typ: TreeType::Param {
                        typ: Box::new(Tree {
                            typ: TreeType::TypeDecl {
                                typ: Type::Class(name.clone())
                            },
                            tkn: tkn.clone()
                        }),
                        name: String::from("this")
                    },
                    tkn: tkn.clone()
                };
                let stripped_class = Tree {
                    typ: TreeType::Class {
                        name: name.clone(),
                        fields,
                        functions: vec![],
                        features: vec![]
                    },
                    tkn: tkn.clone()
                };
                let mut class_functions = vec![];
                for f in functions {
                    let desugared_func = self.desugar_tree_internal(f)?;
                    if let TreeType::Func { name: fn_name, return_type, param, block } = desugared_func.typ {
                        let new_params = if let TreeType::ParamList { parameters } = param.typ {
                            let mut params = vec![added_this.clone()];
                            params.extend(parameters);
                            params
                        } else {
                            unreachable!()
                        };
                        let new_params = Tree {
                            typ: TreeType::ParamList { parameters: new_params },
                            tkn: param.tkn
                        };
                        let desugared_func = Tree {
                            typ: TreeType::Func {
                                name: name.clone() + "_" + &fn_name,
                                return_type,
                                param: Box::new(new_params),
                                block
                            },
                            tkn: desugared_func.tkn
                        };
                        class_functions.push(desugared_func);
                    } else {
                        unreachable!();
                    }
                }
                let mut class_features = vec![];
                for f in features {
                    let desugared_feat = self.desugar_tree_internal(f)?;
                    if let TreeType::Feature { name: feat_name, return_type, param, block } = desugared_feat.typ {
                        let new_params = if let TreeType::ParamList { parameters } = param.typ {
                            if feat_name == "new" { parameters}
                            else {
                                let mut params = vec![added_this.clone()];
                                params.extend(parameters);
                                params
                            }
                        } else {
                            unreachable!()
                        };
                        let new_params = Tree {
                            typ: TreeType::ParamList { parameters: new_params },
                            tkn: param.tkn
                        };
                        let (return_type, block) = if feat_name == "new" {
                            let block = if let TreeType::Block { statements } = block.typ {
                                let this_str = format!("let this: {} = MALLOC({});", name, self.get_size(&Type::Class(name.clone())));
                                let this_alloc = Compiler::parse_snippet(&format!("this_{name}_desugar"), &this_str)?;
                                let mut s = vec![this_alloc];
                                s.extend(statements);
                                s.push(Compiler::parse_snippet(&format!("return_{name}_desugar"), &"return this;".to_string())?);
                                Tree { typ: TreeType::Block {
                                        statements: s
                                    },
                                    tkn: block.tkn
                                }
                            } else {
                                println!("{}", block.rebuild_code());
                                unreachable!();
                            };
                            (Some(Box::new(
                                Tree { typ: TreeType::TypeDecl {
                                        typ: Type::Class(name.clone())
                                    },
                                    tkn: tkn.clone()
                                }
                            )), Box::new(block))
                        } else {
                            (return_type, block)
                        };
                        let desugared_feat = Tree {
                            typ: TreeType::Feature {
                                name: name.clone() + "_" + &feat_name,
                                return_type,
                                param: Box::new(new_params),
                                block
                            },
                            tkn: desugared_feat.tkn
                        };
                        class_features.push(desugared_feat);
                    }
                }
                Tree { typ: TreeType::Class {
                        name,
                        fields: vec![stripped_class],
                        functions: class_functions,
                        features: class_features
                    },
                    tkn
                }
            }
            TreeType::Field { name, typ } => todo!(),
            TreeType::FieldAccess { name, field, typ } => {
                Tree { typ: TreeType::FieldAccess {
                        name,
                        field: Box::new(self.desugar_tree_internal(*field)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::Feature { name, return_type, param, block } => {
                
                let desugared_block = self.desugar_tree_internal(*block)?;
                let new_feature = format!(
                    "{{FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                        if (FUNCTION_COUNTER >= FUNCTION_LIMIT) {{
                        EXIT(STACK_OVERFLOW_CODE);
                    }} else {{
                        {}
                    }}}}", desugared_block.rebuild_code()
                );
                let new_block = Compiler::parse_snippet(&format!("feature_{name}_desugar"), &new_feature)?;
                
                let return_type = if let Some(ret) = return_type {
                    Some(Box::new(self.desugar_tree_internal(*ret)?))
                } else {
                    None
                };
                Tree { typ: TreeType::Feature {
                        name,
                        return_type,
                        param: Box::new(self.desugar_tree_internal(*param)?),
                        block: Box::new(new_block)
                    },
                    tkn
                }
            }
            TreeType::Func { name, return_type, param, block } => {
                let return_type = if let Some(ret) = return_type {
                    Some(Box::new(self.desugar_tree_internal(*ret)?))
                } else {
                    None
                };
                
                let TreeType::Block { mut statements } = block.typ else { panic!() };
                let dec_str = format!("{{FUNCTION_COUNTER = FUNCTION_COUNTER - 1;}}");
                let dec_fn_ctr = Compiler::parse_snippet(&format!("dec_fn_{name}_desugar"), &dec_str)?;
                
                let dec_index = if return_type.is_none() { statements.len() } else { statements.len() - 1};
                statements.insert(dec_index, dec_fn_ctr);
                
                let block = Tree {
                    typ: TreeType::Block {
                        statements
                    },
                    tkn: block.tkn
                };

                let new_function = format!(
                    "{{
                        FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                        if (FUNCTION_COUNTER >= FUNCTION_LIMIT) {{
                        EXIT(STACK_OVERFLOW_CODE);
                    }} else {{
                        {}
                    }}}}", block.rebuild_code()
                );
                let new_block = Compiler::parse_snippet(&format!("function_{name}_desugar"), &new_function)?;
                let desugared_block = self.desugar_tree_internal(new_block)?;
                
                Tree { typ: TreeType::Func {
                        name,
                        return_type,
                        param: Box::new(self.desugar_tree_internal(*param)?),
                        block: Box::new(desugared_block)
                    },
                    tkn
                }
            }
            TreeType::ParamList { parameters} => {
                let mut desugared_parameters = vec![];
                for p in parameters {
                    desugared_parameters.push(self.desugar_tree_internal(p)?);
                }
                Tree { typ: TreeType::ParamList { parameters: desugared_parameters },
                    tkn
                }
            }
            TreeType::Param { name, typ } => {
                Tree { typ: TreeType::Param { name, typ: Box::new(self.desugar_tree_internal(*typ)?) },
                    tkn
                }
            }
            TreeType::Block { statements } => {
                let mut desugared_statements = vec![];
                for s in statements {
                    desugared_statements.push(self.desugar_tree_internal(s)?);
                }
                Tree { typ: TreeType::Block { statements: desugared_statements },
                    tkn
                }
            }
            TreeType::StmtExpr { expression } => {
                Tree { typ: TreeType::StmtExpr {
                    expression: Box::new(self.desugar_tree_internal(*expression)?)
                    },
                    tkn
                }
            },
            TreeType::StmtLet { name, typ, expression } => {
                let desugared_type = self.desugar_tree_internal(*typ)?;
                let desugared_expr = self.desugar_tree_internal(*expression)?;
                Tree { typ: TreeType::StmtLet {
                        name,
                        typ: Box::new(desugared_type),
                        expression: Box::new(desugared_expr)
                    },
                    tkn
                }
            }
            TreeType::StmtAssign { name, expression } => {
                let desugared_name = self.desugar_tree_internal(*name)?;
                let desugared_expr = self.desugar_tree_internal(*expression)?;
                Tree { typ: TreeType::StmtAssign {
                        name: Box::new(desugared_name),
                        expression: Box::new(desugared_expr)
                    },
                    tkn
                }
            }
            TreeType::StmtIf { condition, if_branch, else_branch } => {
                let desugared_condition = self.desugar_tree_internal(*condition)?;
                let desugared_if = self.desugar_tree_internal(*if_branch)?;
                let desugared_else = match else_branch {
                    Some(else_branch) => Some(Box::new(self.desugar_tree_internal(*else_branch)?)),
                    None => None
                };
                Tree {
                    typ: TreeType::StmtIf {
                        condition: Box::new(desugared_condition),
                        if_branch: Box::new(desugared_if),
                        else_branch: desugared_else
                    },
                tkn
            }
            },
            TreeType::StmtReturn { return_value } => {
                let return_value = if let Some(ret) = return_value {
                    Some(Box::new(self.desugar_tree_internal(*ret)?))
                } else {
                    None
                };
                Tree { typ: TreeType::StmtReturn { return_value },
                    tkn
                }
            }
            TreeType::TypeDecl { typ } => {
                Tree { typ: TreeType::TypeDecl { typ },
                    tkn
                }
            }
            TreeType::ExprCall { function_name, args, typ } => {
                if function_name.as_bytes()[0].is_ascii_uppercase() {
                    // Function Call is assumed to be ClassName(args), which desugars to ClassName::new(args)
                    let new_name = if function_name.contains("_") {
                        function_name.clone()
                    } else {
                        function_name.clone() + "_new"
                    };
                    // let args = self.desugar_tree_internal(*args)?;
                    Tree { typ: TreeType::ExprCall {
                            function_name: new_name,
                            args: Box::new(self.desugar_tree_internal(*args)?),
                            typ: Type::Class(function_name.clone())
                        },
                        tkn
                    }
                } else {
                    Tree { typ: TreeType::ExprCall {
                            function_name,
                            args: Box::new(self.desugar_tree_internal(*args)?),
                            typ
                        },
                        tkn
                    }
                }
            }
            TreeType::ArgList { arguments } => {
                let mut desugared_arguments = vec![];
                for a in arguments {
                    desugared_arguments.push(self.desugar_tree_internal(a)?);
                }
                Tree { typ: TreeType::ArgList {
                        arguments: desugared_arguments
                    },
                    tkn
                }
            }
            TreeType::Arg { expression } => {
                Tree { typ: TreeType::Arg {
                        expression: Box::new(self.desugar_tree_internal(*expression)?),
                    },
                    tkn
                }
            }
            TreeType::ExprName { name, typ } => {
                Tree { typ: TreeType::ExprName {
                        name,
                        typ
                    },
                    tkn
                }
            }
            TreeType::ExprArrLiteral { elements } => {
                let mut desugared_elements = vec![];
                for e in elements {
                    desugared_elements.push(self.desugar_tree_internal(e)?);
                }
                Tree { typ: TreeType::ExprArrLiteral {
                    elements: desugared_elements
                    },
                    tkn
                }
            }
            TreeType::ExprArrAccess { arr_name, indices, typ } => {
                Tree { typ: TreeType::ExprArrAccess {
                        arr_name,
                        indices: Box::new(self.desugar_tree_internal(*indices)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::ExprLiteral { typ } => {
                Tree { typ: TreeType::ExprLiteral { typ }, tkn }
            }
            TreeType::ExprBinary { lhs, rhs, typ } => {
                let desugared_lhs = self.desugar_tree_internal(*lhs)?;
                let desugared_rhs = self.desugar_tree_internal(*rhs)?;
                Tree { typ: TreeType::ExprBinary {
                        lhs: Box::new(desugared_lhs),
                        rhs: Box::new(desugared_rhs),
                        typ
                    },
                    tkn
                }
                // Code below replaces a+b with a.plus(b), a-b with a.minus(b) etc.
                // Useful later on when we can define everything as class instances
                // and define operator overloads/functions
                // for now, not very useful, but still worthy to keep
                // let typ = TreeType::FieldAccess {
                //     name: desugared_lhs.rebuild_code(),
                //     field: Box::new(Tree {
                //         typ: TreeType::ExprCall {
                //             function_name: match tkn.get_type() {
                //                 TokenType::Plus => String::from("plus"),
                //                 TokenType::Minus => String::from("minus"),
                //                 TokenType::Asterisk => String::from("mult"),
                //                 TokenType::ForwardSlash => String::from("div"),
                //                 _ => todo!()
                //             },
                //             args: Box::new(Tree {
                //                 typ: TreeType::ArgList {
                //                     arguments: vec![desugared_rhs],
                //                 },
                //                 tkn: tkn.clone()
                //             }),
                //             typ: Type::Unknown
                //         },
                //         tkn: tkn.clone()
                //     }),
                //     typ: Type::Unknown
                // };
                // Tree {
                //     typ,
                //     tkn
                // }
            }
            TreeType::ExprComp { lhs, rhs, typ } => {
                let desugared_lhs = self.desugar_tree_internal(*lhs)?;
                let desugared_rhs = self.desugar_tree_internal(*rhs)?;
                Tree { typ: TreeType::ExprComp {
                        lhs: Box::new(desugared_lhs),
                        rhs: Box::new(desugared_rhs),
                        typ
                    },
                    tkn
                }
            },
            TreeType::ExprParen { expression, typ } => {
                Tree { typ: TreeType::ExprParen {
                        expression: Box::new(self.desugar_tree_internal(*expression)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::BuiltInFunction { function_name, args, typ } => {
                Tree { typ: TreeType::BuiltInFunction {
                        function_name,
                        args: Box::new(self.desugar_tree_internal(*args)?),
                        typ
                    },
                    tkn
                }
            }
        })
    }

    fn get_size(&self, typ: &Type) -> usize {
        match typ {
            Type::Class(c) => *self.class_sizes.get(c).unwrap(),
            _ => self.sm.get_type_size(typ)
        }
    }
}