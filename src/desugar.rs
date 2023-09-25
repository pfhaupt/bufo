use std::collections::HashMap;

use lazy_static::lazy_static;
use regex::Regex;

use crate::Compiler;
use crate::parser::{Tree, TreeType, Parser};
use crate::lexer::TokenType;
use crate::checker::Type;

lazy_static!{
    static ref REGEX_LOOKUP: HashMap<&'static str, Regex> = {
        let mut lookup = HashMap::new();
        lookup.insert("plus", Regex::new(r"(<lhs>.*)").unwrap());
        lookup
    };
}

pub struct Desugarer {
}

impl Desugarer {
    pub fn new() -> Self {
        Self { }
    }
    pub fn desugar_tree(&mut self, ast: Tree) -> Result<Tree, String> {
        let tkn = ast.tkn;
        Ok(match ast.typ {
            TreeType::File { functions, classes } => {
                let mut desugared_functions = vec![];
                let mut desugared_classes = vec![];
                for f in functions {
                    desugared_functions.push(self.desugar_tree(f)?);
                }
                for c in classes {
                    if let TreeType::Class { name, fields: this, functions, features } = self.desugar_tree(c)?.typ {
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
                    let desugared_func = self.desugar_tree(f)?;
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
                                name: name.clone() + "::" + &fn_name,
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
                    let desugared_feat = self.desugar_tree(f)?;
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
                                let this_str = format!("let this: {} = ALLOC(SIZEOF({}));", name, name);
                                let this_alloc = Compiler::parse_snippet(&this_str)?;
                                let mut s = vec![this_alloc];
                                s.extend(statements);
                                s.push(Compiler::parse_snippet(&"return this;".to_string())?);
                                Tree { typ: TreeType::Block {
                                        statements: s
                                    },
                                    tkn: block.tkn
                                }
                            } else {
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
                                name: name.clone() + "::" + &feat_name,
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
                        field: Box::new(self.desugar_tree(*field)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::Feature { name, return_type, param, block } => {
                let return_type = if let Some(ret) = return_type {
                    Some(Box::new(self.desugar_tree(*ret)?))
                } else {
                    None
                };
                Tree { typ: TreeType::Feature {
                        name,
                        return_type,
                        param: Box::new(self.desugar_tree(*param)?),
                        block: Box::new(self.desugar_tree(*block)?)
                    },
                    tkn
                }
            }
            TreeType::Func { name, return_type, param, block } => {
                let return_type = if let Some(ret) = return_type {
                    Some(Box::new(self.desugar_tree(*ret)?))
                } else {
                    None
                };
                Tree { typ: TreeType::Func {
                        name,
                        return_type,
                        param: Box::new(self.desugar_tree(*param)?),
                        block: Box::new(self.desugar_tree(*block)?)
                    },
                    tkn
                }
            }
            TreeType::ParamList { parameters} => {
                let mut desugared_parameters = vec![];
                for p in parameters {
                    desugared_parameters.push(self.desugar_tree(p)?);
                }
                Tree { typ: TreeType::ParamList { parameters: desugared_parameters },
                    tkn
                }
            }
            TreeType::Param { name, typ } => {
                Tree { typ: TreeType::Param { name, typ: Box::new(self.desugar_tree(*typ)?) },
                    tkn
                }
            }
            TreeType::Block { statements } => {
                let mut desugared_statements = vec![];
                for s in statements {
                    desugared_statements.push(self.desugar_tree(s)?);
                }
                Tree { typ: TreeType::Block { statements: desugared_statements },
                    tkn
                }
            }
            TreeType::StmtExpr { expression } => todo!(),
            TreeType::StmtLet { name, typ, expression } => {
                let desugared_type = self.desugar_tree(*typ)?;
                let desugared_expr = self.desugar_tree(*expression)?;
                Tree { typ: TreeType::StmtLet {
                        name,
                        typ: Box::new(desugared_type),
                        expression: Box::new(desugared_expr)
                    },
                    tkn
                }
            }
            TreeType::StmtAssign { name, expression } => {
                let desugared_name = self.desugar_tree(*name)?;
                let desugared_expr = self.desugar_tree(*expression)?;
                Tree { typ: TreeType::StmtAssign {
                        name: Box::new(desugared_name),
                        expression: Box::new(desugared_expr)
                    },
                    tkn
                }
            }
            TreeType::StmtIf { condition, if_branch, else_branch } => todo!(),
            TreeType::StmtReturn { return_value } => {
                let return_value = if let Some(ret) = return_value {
                    Some(Box::new(self.desugar_tree(*ret)?))
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
                    let new_name = if function_name.contains("::") {
                        function_name.clone()
                    } else {
                        function_name.clone() + "::new"
                    };
                    Tree { typ: TreeType::ExprCall {
                            function_name: new_name,
                            args,
                            typ: Type::Class(function_name.clone())
                        },
                        tkn
                    }
                } else {
                    Tree { typ: TreeType::ExprCall {
                            function_name,
                            args: Box::new(self.desugar_tree(*args)?),
                            typ
                        },
                        tkn
                    }
                }
            }
            TreeType::ArgList { arguments } => {
                let mut desugared_arguments = vec![];
                for a in arguments {
                    desugared_arguments.push(self.desugar_tree(a)?);
                }
                Tree { typ: TreeType::ArgList {
                        arguments: desugared_arguments
                    },
                    tkn
                }
            }
            TreeType::Arg { expression } => {
                Tree { typ: TreeType::Arg {
                        expression: Box::new(self.desugar_tree(*expression)?),
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
                    desugared_elements.push(self.desugar_tree(e)?);
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
                        indices: Box::new(self.desugar_tree(*indices)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::ExprLiteral { typ } => {
                Tree { typ: TreeType::ExprLiteral { typ }, tkn }
            }
            TreeType::ExprBinary { lhs, rhs, typ } => {
                let desugared_lhs = self.desugar_tree(*lhs)?;
                let desugared_rhs = self.desugar_tree(*rhs)?;
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
            TreeType::ExprComp { lhs, rhs, typ } => todo!(),
            TreeType::ExprParen { expression, typ } => {
                Tree { typ: TreeType::ExprParen {
                        expression: Box::new(self.desugar_tree(*expression)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::Identifier { name } => todo!(),
            TreeType::BuiltInFunction { function_name, args, typ } => {
                Tree { typ: TreeType::BuiltInFunction {
                        function_name,
                        args: Box::new(self.desugar_tree(*args)?),
                        typ
                    },
                    tkn
                }
            }
            TreeType::BuiltInVariable { variable_name, typ } => todo!(),
        })
    }
}