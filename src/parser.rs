use std::cell::Cell;

use crate::codegen::ERR_STR;
use crate::lexer::{Location, Token, TokenType};
use crate::checker::Type;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TreeType {
    File { functions: Vec<Tree> },
    Func { name: String, return_type: Option<Box<Tree>>, param: Box<Tree>, block: Box<Tree> },
    Name { name: String },
    ParamList { parameters: Vec<Tree> },
    Param { name: String, typ: Box<Tree> },
    ArgList { arguments: Vec<Tree> },
    Arg { expression: Box<Tree> },
    Block { statements: Vec<Tree> },
    StmtExpr { expression: Box<Tree> },
    StmtLet { name: String, typ: Box<Tree>, expression: Box<Tree> },
    StmtAssign { name: Box<Tree>, expression: Box<Tree> },
    StmtIf { condition: Box<Tree>, if_branch: Box<Tree>, else_branch: Option<Box<Tree>>},
    StmtReturn { return_value: Option<Box<Tree>> },
    ExprName { name: String, typ: Type },
    ExprArrLiteral { elements: Vec<Tree> },
    ExprArrAccess { arr_name: String, indices: Box<Tree>, typ: Type }, // indices contains ArrLiteral
    ExprLiteral { typ: Type },
    ExprBinary { lhs: Box<Tree>, rhs: Box<Tree>, typ: Type },
    ExprParen { expression: Box<Tree>, typ: Type },
    ExprCall { function_name: String, args: Box<Tree>, typ: Type },
    TypeDecl { typ: Type },
    Pointer { var_name: Box<Tree> },
    Deref { var_name: Box<Tree> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree {
    pub typ: TreeType,
    pub tkn: Token,
}

impl Tree {
    #[allow(unused)]
    pub fn print_debug(&self) {
        self.print_internal(0);
    }

    fn print_internal(&self, indent: usize) {
        let tab = " ".repeat(indent);
        match &self.typ {
            TreeType::File { functions } => {
                for f in functions {
                    f.print_internal(indent + 2);
                }
            }
            TreeType::Func { name, return_type, param, block } => {
                println!("{tab}Function {name}");
                println!("{tab}Return", tab=" ".repeat(indent + 2));
                if let Some(s) = return_type {
                    s.print_internal(indent + 4);
                } else {
                    println!("{tab}None", tab=" ".repeat(indent + 4));
                }
                println!("{tab}Param", tab=" ".repeat(indent + 2));
                param.print_internal(indent + 4);
                println!("{tab}Block", tab=" ".repeat(indent + 2));
                block.print_internal(indent + 4);
            }
            TreeType::Name { name } => {
                println!("{tab}{}", name);
            }
            TreeType::ParamList { parameters } => {
                for p in parameters {
                    p.print_internal(indent);
                }
            }
            TreeType::Param { name, typ } => {
                println!("{tab}{name}");
                typ.print_internal(indent + 2);
            }
            TreeType::ArgList { arguments } => {
                for a in arguments {
                    a.print_internal(indent);
                }
            }
            TreeType::Arg { expression } => {
                println!("{tab}Arg");
                expression.print_internal(indent + 2);
            }
            TreeType::Block { statements } => {
                for s in statements {
                    s.print_internal(indent);
                }
            }
            TreeType::StmtExpr { expression } => {
                expression.print_internal(indent);
            }
            TreeType::StmtLet { name, typ, expression } => {
                println!("{tab}StmtLet {name}");
                typ.print_internal(indent + 4);
                expression.print_internal(indent + 2);
            }
            TreeType::StmtAssign { name, expression } => {
                println!("{tab}StmtAssign");
                name.print_internal(indent + 2);
                expression.print_internal(indent + 2);
            }
            TreeType::StmtIf { condition, if_branch, else_branch } => {
                println!("{tab}StmtIf");
                println!("{tab}Condition", tab=" ".repeat(indent + 2));
                condition.print_internal(indent + 4);
                println!("{tab}If-Branch", tab=" ".repeat(indent + 2));
                if_branch.print_internal(indent + 4);
                if let Some(e) = else_branch {
                    println!("{tab}Else-Branch", tab=" ".repeat(indent + 2));
                    e.print_internal(indent + 4);
                }
            }
            TreeType::StmtReturn { return_value } => {
                println!("{tab}StmtReturn");
                if let Some(r) = return_value {
                    r.print_internal(indent + 2);
                }
            }
            TreeType::ExprName { name, typ } => {
                println!("{tab}ExprName {name} {typ:?}");
            }
            TreeType::ExprArrLiteral { elements } => {
                println!("{tab}ArrLiteral");
                for e in elements {
                    e.print_internal(indent + 2);
                }
            }
            TreeType::ExprArrAccess { arr_name, indices, typ } => {
                println!("{tab}ArrAccess {arr_name} {typ:?}");
                indices.print_internal(indent + 2);
            }
            TreeType::ExprLiteral { typ } => {
                println!("{tab}ExprLiteral {} {typ:?}", self.tkn.get_value());
            }
            TreeType::ExprBinary { lhs, rhs, typ } => {
                println!("{tab}ExprBinary {} {typ:?}", self.tkn.get_value());
                lhs.print_internal(indent + 2);
                rhs.print_internal(indent + 2);
            }
            TreeType::ExprParen { expression, typ } => {
                println!("{tab}ExprParen {typ:?}");
                expression.print_internal(indent + 2);
            }
            TreeType::ExprCall { function_name, args, typ } => {
                println!("{tab}ExprCall {function_name} {typ:?}");
                args.print_internal(indent + 2);
            }
            TreeType::TypeDecl { typ } => {
                println!("{tab}{typ:?}");
            }
            TreeType::Pointer { .. } => {
                todo!();
            }
            TreeType::Deref { .. } => {
                todo!();
            }
        }
    }
}

pub struct Parser {
    file_path: String,
    tokens: Vec<Token>,
    ptr: usize,
    fuel: Cell<u32>,
    root: Option<Tree>,
    #[allow(unused)]
    print_debug: bool
}

impl Parser {
    pub fn new(file_path: &String, tokens: Vec<Token>, print_debug: bool) -> Self {
        Self {
            file_path: file_path.to_owned(),
            tokens,
            ptr: 0,
            fuel: Cell::new(256),
            root: None,
            print_debug
        }
    }

    fn open(&self) -> Token {
        self.tokens[self.ptr].clone()
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.ptr += 1;
    }

    fn eof(&self) -> bool {
        self.ptr >= self.tokens.len()
    }

    fn nth(&self, lookahead: usize) -> TokenType {
        if self.fuel.get() == 0 {
            panic!("Parser is stuck at {}", self.ptr);
        }
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.ptr + lookahead)
            .map_or(TokenType::Eof, |it| it.get_type())
    }

    fn at(&self, typ: TokenType) -> bool {
        self.nth(0) == typ
    }

    fn eat(&mut self, typ: TokenType) -> bool {
        if self.at(typ) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, typ: TokenType) -> Result<Token, String> {
        let t = typ;
        if self.eat(typ) {
            return Ok(self.tokens[self.ptr - 1].clone());
        }
        // TODO: Improve error handling
        let (prev, typ) = match self.tokens.get(self.ptr) {
            None => (self.tokens.get(self.ptr - 1).expect("Expected Some value, found None instead. This might be a bug in Parsing or Lexing."), TokenType::Eof),
            Some(prev) => (prev, prev.get_type())
        };
        Err(format!(
            "{}: {:?}: Expected {:?}, found {:?}",
            ERR_STR,
            prev.get_loc(),
            t,
            typ
        ))
    }

    fn parse_type_literal(&mut self, lit_tkn: &Token) -> Result<(String, Type), String> {
        let lit = lit_tkn.get_value();
        let loc = &lit_tkn.get_loc();
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ_str = &lit[index..];
                let typ = self.parse_type_str(loc, typ_str)?;
                Ok((lit[0..index].to_owned(), typ))
            }
            None => Ok((lit, Type::Unknown)),
        }
    }

    fn parse_type(&self, token: &Token) -> Result<Type, String> {
        self.parse_type_str(&token.get_loc(), &token.get_value())
    }

    fn parse_type_str(&self, loc: &Location, val: &str) -> Result<Type, String> {
        match val {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "usize" => Ok(Type::Usize),
            // Reserved for future use
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            t => Err(format!(
                "{}: {:?}: Unexpected Type `{}`. Expected one of {{i32, i64, u32, u64}}",
                ERR_STR,
                loc,
                t
            )),
        }
    }

    fn parse_expr_call(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::Name)?;
        let args = self.parse_arg_list()?;
        Ok(Tree {
            typ: TreeType::ExprCall {
                function_name: tkn.get_value(),
                args: Box::new(args),
                typ: Type::Unknown
            },
            tkn,
        })
    }

    fn parse_expr_array(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut elements = vec![];
        self.expect(TokenType::OpenSquare)?;
        while !self.at(TokenType::ClosingSquare) && !self.eof() {
            let elem = self.parse_expr()?;
            elements.push(elem);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(Tree {
            typ: TreeType::ExprArrLiteral {
                elements
            },
            tkn,
        })
    }

    fn parse_name(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        Ok(match self.nth(1) {
            TokenType::OpenRound => self.parse_expr_call()?,
            TokenType::OpenSquare => {
                self.expect(TokenType::Name)?;
                let indices = self.parse_expr_array()?;
                Tree {
                    typ: TreeType::ExprArrAccess {
                        arr_name: tkn.get_value(),
                        indices: Box::new(indices),
                        typ: Type::Unknown
                    },
                    tkn,
                }
            }
            _ => {
                self.expect(TokenType::Name)?;
                Tree {
                    typ: TreeType::ExprName { name: tkn.get_value(), typ: Type::Unknown },
                    tkn,
                }
            }
        })
    }

    fn parse_expr_delim(&mut self) -> Result<Tree, String> {
        Ok(match self.nth(0) {
            TokenType::IntLiteral => {
                // let tkn = self.open();
                let number_token = self.expect(TokenType::IntLiteral)?;
                let (number, typ) = self.parse_type_literal(&number_token)?;
                let tkn = Token::new(TokenType::IntLiteral, number, number_token.get_loc());
                Tree {
                    typ: TreeType::ExprLiteral {
                        typ
                    },
                    tkn,
                }
            }
            TokenType::Name => self.parse_name()?,
            TokenType::OpenRound => {
                let tkn = self.open();
                self.expect(TokenType::OpenRound)?;
                let expr = self.parse_expr()?;
                self.expect(TokenType::ClosingRound)?;
                Tree {
                    typ: TreeType::ExprParen {
                        expression: Box::new(expr),
                        typ: Type::Unknown
                    },
                    tkn,
                }
            }
            TokenType::OpenSquare => self.parse_expr_array()?,
            TokenType::Ampersand => {
                let tkn = self.open();
                self.expect(TokenType::Ampersand)?;
                let name = self.parse_name()?;
                Tree {
                    typ: TreeType::Pointer {
                        var_name: Box::new(name)
                    },
                    tkn,
                }
            }
            TokenType::Asterisk => {
                let tkn = self.open();
                self.expect(TokenType::Asterisk)?;
                let name = self.parse_name()?;
                Tree {
                    typ: TreeType::Deref {
                        var_name: Box::new(name)
                    },
                    tkn,
                }
            }
            e => {
                let ptr = if self.ptr == self.tokens.len() {
                    self.ptr - 1
                } else {
                    self.ptr
                };
                return Err(format!(
                    "{}: {:?}: Expected Expr, found {:?}",
                    ERR_STR,
                    self.tokens.get(ptr).unwrap().get_loc(),
                    e
                ));
            }
        })
    }

    fn parse_expr_rec(&mut self, left: TokenType) -> Result<Tree, String> {
        let mut lhs = self.parse_expr_delim()?;

        loop {
            let right = self.nth(0);
            if Self::right_binds_tighter(left, right) {
                let tkn = self.open();
                self.advance();
                let rhs = self.parse_expr_rec(right)?;
                lhs = Tree {
                    typ: TreeType::ExprBinary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        typ: Type::Unknown
                    },
                    tkn,
                };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn right_binds_tighter(left: TokenType, right: TokenType) -> bool {
        fn tightness(typ: TokenType) -> Option<usize> {
            [
                [TokenType::Plus, TokenType::Minus].as_slice(),
                &[TokenType::Asterisk, TokenType::ForwardSlash],
                &[
                    TokenType::CmpEq,
                    TokenType::CmpNeq,
                    TokenType::CmpGt,
                    TokenType::CmpGte,
                    TokenType::CmpLt,
                    TokenType::CmpLte,
                ],
            ]
            .iter()
            .position(|l| l.contains(&typ))
        }
        let Some(right_tight) = tightness(right) else {
            return false
        };
        let Some(left_tight) = tightness(left) else {
            assert!(left == TokenType::Eof);
            return true;
        };
        right_tight > left_tight
    }

    fn parse_expr(&mut self) -> Result<Tree, String> {
        self.parse_expr_rec(TokenType::Eof)
    }

    fn parse_type_arr_size(&mut self) -> Result<Vec<usize>, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenSquare)?;
        while !self.at(TokenType::ClosingSquare) && !self.eof() {
            let size = self.expect(TokenType::IntLiteral)?;
            let size = match size.get_value().parse::<usize>() {
                Ok(i) => i,
                Err(e) => return Err(format!(
                    "{}: {:?}: {}",
                    ERR_STR,
                    tkn.get_loc(),
                    e
                ))
            };
            children.push(size);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(children)
    }

    fn parse_type_decl(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::TypeDecl)?;
        let name = match self.nth(0) {
            TokenType::Ampersand => {
                self.expect(TokenType::Ampersand)?;
                let typ_tkn = self.expect(TokenType::Name)?;
                let typ = self.parse_type(&typ_tkn)?;
                if self.at(TokenType::OpenSquare) {
                    let size = self.parse_type_arr_size()?;
                    Tree {
                        typ: TreeType::TypeDecl {
                            typ: Type::Ptr(Box::new(
                                Type::Arr(
                                    Box::new(typ),
                                    size
                        )))},
                        tkn
                    }
                } else {
                    Tree {
                        typ: TreeType::TypeDecl {
                            typ: Type::Ptr(Box::new(typ))
                        },
                        tkn
                    }
                }
            }
            _ => {
                let name = self.expect(TokenType::Name)?;
                let typ = self.parse_type(&name)?;
                if self.at(TokenType::OpenSquare) {
                    let size = self.parse_type_arr_size()?;
                    Tree {
                        typ: TreeType::TypeDecl {
                                typ: Type::Arr(
                                    Box::new(typ),
                                    size
                        )},
                        tkn
                    }
                } else {
                    Tree {
                        typ: TreeType::TypeDecl {
                            typ
                        },
                        tkn
                    }
                }
            }
        };
        Ok(name)
    }

    fn parse_stmt_let(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::LetKeyword)?;
        let name_tkn = self.expect(TokenType::Name)?;
        let type_tree = self.parse_type_decl()?;
        self.expect(TokenType::Equal)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtLet {
                name: name_tkn.get_value(),
                typ: Box::new(type_tree),
                expression: Box::new(expr)
            },
            tkn,
        })
    }

    fn parse_stmt_assign(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let node = match self.nth(1) {
            TokenType::Equal => {
                let name = self.expect(TokenType::Name)?;
                Tree {
                    typ: TreeType::Name { name: name.get_value()},
                    tkn: tkn.clone()
                }
            },
            TokenType::OpenSquare => {
                let name = self.expect(TokenType::Name)?;
                Tree {
                    typ: TreeType::ExprArrAccess {
                        arr_name: name.get_value(),
                        indices: Box::new(self.parse_expr_array()?),
                        typ: Type::Unknown
                    },
                    tkn: tkn.clone()
                }
            },
            _ => todo!(),
        };
        self.expect(TokenType::Equal)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtAssign {
                name: Box::new(node),
                expression: Box::new(expr)
            },
            tkn,
        })
    }

    fn parse_stmt_assign_ptr(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let ptr_tkn = self.expect(TokenType::Asterisk)?;
        let var_tkn = self.expect(TokenType::Name)?;
        self.expect(TokenType::Equal)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtAssign {
                name: Box::new(Tree {
                    typ: TreeType::Pointer { var_name:
                        Box::new(Tree {
                            typ: TreeType::Name { name: var_tkn.get_value() },
                            tkn: var_tkn
                        })
                    },
                    tkn: ptr_tkn
                }),
                expression: Box::new(expr)
            },
            tkn,
        })
    }

    fn parse_stmt_expr(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtExpr {
                expression: Box::new(expr)
            },
            tkn
        })
    }

    fn parse_arg(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let expr = self.parse_expr()?;
        Ok(Tree {
            typ: TreeType::Arg {
                expression: Box::new(expr)
            },
            tkn,
        })
    }

    fn parse_arg_list(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound) && !self.eof() {
            children.push(self.parse_arg()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ArgList {
                arguments: children
            },
            tkn
        })
    }

    fn parse_stmt_if(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenRound)?;
        let condition = self.parse_expr()?;
        self.expect(TokenType::ClosingRound)?;
        let if_block = self.parse_block()?;
        let else_block = if self.eat(TokenType::ElseKeyword) {
            Some(Box::new(self.parse_block()?))
        } else {
            None
        };
        Ok(Tree {
            typ: TreeType::StmtIf {
                condition: Box::new(condition),
                if_branch: Box::new(if_block),
                else_branch: else_block
            },
            tkn,
        })
    }

    fn parse_stmt_return(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::ReturnKeyword)?;
        let ret_value = if !self.at(TokenType::Semi) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtReturn {
                return_value: ret_value
            },
            tkn
        })
    }

    fn parse_block(&mut self) -> Result<Tree, String> {
        if !self.at(TokenType::OpenCurly) {
            let ptr = if self.ptr == self.tokens.len() {
                self.ptr - 1
            } else {
                self.ptr
            };
            let tkn = self.tokens.get(ptr).unwrap();
            return Err(format!(
                "{}: {:?}: Expected `{{`, found `{:?}`",
                ERR_STR,
                tkn.get_loc(),
                self.nth(0)
            ));
        }
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenCurly)?;
        while !self.at(TokenType::ClosingCurly) && !self.eof() {
            match self.nth(0) {
                TokenType::LetKeyword => children.push(self.parse_stmt_let()?),
                TokenType::IfKeyword => children.push(self.parse_stmt_if()?),
                TokenType::ReturnKeyword => children.push(self.parse_stmt_return()?),
                TokenType::Name => match self.nth(1) {
                    TokenType::Equal | TokenType::OpenSquare => {
                        children.push(self.parse_stmt_assign()?)
                    }
                    _ => children.push(self.parse_stmt_expr()?),
                },
                TokenType::Asterisk => children.push(self.parse_stmt_assign_ptr()?),
                _ => children.push(self.parse_stmt_expr()?),
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(Tree {
            typ: TreeType::Block {
                statements: children
            },
            tkn
        })
    }

    fn parse_param(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let name_tkn = self.expect(TokenType::Name)?;
        let type_decl = self.parse_type_decl()?;
        Ok(Tree {
            typ: TreeType::Param {
                name: name_tkn.get_value(),
                typ: Box::new(type_decl)
            },
            tkn,
        })
    }

    fn parse_param_list(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound) && !self.eof() {
            children.push(self.parse_param()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ParamList {
                parameters: children
            },
            tkn,
        })
    }

    fn parse_return_type(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::Arrow)?;
        let name_tkn = self.expect(TokenType::Name)?;
        let typ = self.parse_type(&name_tkn)?;
        Ok(Tree {
            typ: TreeType::TypeDecl {
                typ,
            },
            tkn,
        })
    }

    fn parse_func(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::FnKeyword)?;
        let fn_name = self.expect(TokenType::Name)?;
        let params = self.parse_param_list()?;
        let return_type = if self.at(TokenType::Arrow) {
            Some(Box::new(self.parse_return_type()?))
        } else {
            None
        };
        let block = self.parse_block()?;
        Ok(Tree {
            typ: TreeType::Func {
                name: fn_name.get_value(),
                return_type,
                param: Box::new(params),
                block: Box::new(block)
            },
            tkn,
        })
    }

    pub fn parse_file(&mut self) -> Result<Tree, String> {
        assert_eq!(
            TokenType::Eof as u8 + 1,
            31,
            "Not all TokenTypes are handled in parse_file()"
        );
        let tkn = Token::new(
            TokenType::File,
            self.file_path.clone(),
            Location::new(self.file_path.clone(), 0, 0),
        );
        let mut children = vec![];
        while !self.eof() {
            if self.at(TokenType::FnKeyword) {
                children.push(self.parse_func()?);
            } else {
                let prev = self.tokens.get(self.ptr).unwrap();
                return Err(format!(
                    "{}: {:?}: Expected Function, found {:?}",
                    ERR_STR,
                    prev.get_loc(),
                    prev.get_type()
                ));
            }
        }
        self.root = Some(Tree {
            typ: TreeType::File {
                functions: children
            },
            tkn,
        });
        Ok(self.root.clone().unwrap())
    }
}
