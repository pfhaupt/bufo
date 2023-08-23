use std::cell::Cell;

use crate::codegen::ERR_STR;
use crate::lexer::{Location, Token, TokenType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TreeType {
    File,
    Func,
    Name,
    ParamList,
    Param,
    ArgList,
    Arg,
    Block,
    StmtExpr,
    StmtLet,
    StmtAssign,
    StmtIf,
    StmtReturn,
    ExprName,
    ExprArrLiteral,
    ExprArrAccess,
    ExprLiteral,
    ExprBinary,
    ExprParen,
    ExprCall,
    TypeDecl,
    ArrSize,
    Pointer,
    Deref,
}

#[derive(Debug, Clone)]
pub struct Tree {
    pub typ: TreeType,
    pub tkn: Token,
    pub children: Vec<Tree>,
}

impl Tree {
    #[allow(unused)]
    pub fn print_debug(&self) {
        self.print_internal(0);
    }

    fn print_internal(&self, indent: usize) {
        let tab = " ".repeat(indent);
        println!("{tab}{:?} `{}`", self.typ, self.tkn.get_value());
        for c in &self.children {
            c.print_internal(indent + 2);
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

    fn open(&self) -> (Token, Vec<Tree>) {
        (self.tokens[self.ptr].clone(), vec![])
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

    fn parse_expr_call(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::Name)?;
        children.push(self.parse_arg_list()?);
        Ok(Tree {
            typ: TreeType::ExprCall,
            tkn,
            children,
        })
    }

    fn parse_expr_array(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::OpenSquare)?;
        while !self.at(TokenType::ClosingSquare) && !self.eof() {
            let elem = self.parse_expr()?;
            children.push(elem);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(Tree {
            typ: TreeType::ExprArrLiteral,
            tkn,
            children,
        })
    }

    fn parse_name(&mut self) -> Result<Tree, String> {
        Ok(match self.nth(1) {
            TokenType::OpenRound => self.parse_expr_call()?,
            TokenType::OpenSquare => {
                let (tkn, mut children) = self.open();
                self.expect(TokenType::Name)?;
                children.push(self.parse_expr_array()?);
                Tree {
                    typ: TreeType::ExprArrAccess,
                    tkn,
                    children,
                }
            }
            _ => {
                let (tkn, mut children) = self.open();
                children.push(Tree {
                    typ: TreeType::Name,
                    tkn: self.expect(TokenType::Name)?,
                    children: vec![],
                });
                Tree {
                    typ: TreeType::ExprName,
                    tkn,
                    children,
                }
            }
        })
    }

    fn parse_expr_delim(&mut self) -> Result<Tree, String> {
        Ok(match self.nth(0) {
            TokenType::IntLiteral => {
                let (tkn, mut children) = self.open();
                children.push(Tree {
                    typ: TreeType::ExprLiteral,
                    tkn: self.expect(TokenType::IntLiteral)?,
                    children: vec![],
                });
                Tree {
                    typ: TreeType::ExprLiteral,
                    tkn,
                    children,
                }
            }
            TokenType::Name => self.parse_name()?,
            TokenType::OpenRound => {
                let (tkn, mut children) = self.open();
                self.expect(TokenType::OpenRound)?;
                children.push(self.parse_expr()?);
                self.expect(TokenType::ClosingRound)?;
                Tree {
                    typ: TreeType::ExprParen,
                    tkn,
                    children,
                }
            }
            TokenType::OpenSquare => self.parse_expr_array()?,
            TokenType::Ampersand => {
                let (tkn, mut children) = self.open();
                self.expect(TokenType::Ampersand)?;
                children.push(self.parse_name()?);
                Tree {
                    typ: TreeType::Pointer,
                    tkn,
                    children,
                }
            }
            TokenType::Asterisk => {
                let (tkn, mut children) = self.open();
                self.expect(TokenType::Asterisk)?;
                children.push(self.parse_name()?);
                Tree {
                    typ: TreeType::Deref,
                    tkn,
                    children,
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
                let (tkn, mut children) = self.open();
                self.advance();
                children.push(lhs);
                children.push(self.parse_expr_rec(right)?);
                lhs = Tree {
                    typ: TreeType::ExprBinary,
                    tkn,
                    children,
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

    fn parse_type_arr(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::OpenSquare)?;
        while !self.at(TokenType::ClosingSquare) && !self.eof() {
            let size = Tree {
                typ: TreeType::ArrSize,
                tkn: self.expect(TokenType::IntLiteral)?,
                children: vec![],
            };
            children.push(size);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(Tree {
            typ: TreeType::ArrSize,
            tkn,
            children,
        })
    }

    fn parse_type_decl(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::TypeDecl)?;
        let name = match self.nth(0) {
            TokenType::Ampersand => {
                let mut ptr = Tree {
                    typ: TreeType::Pointer,
                    tkn: self.expect(TokenType::Ampersand)?,
                    children: vec![],
                };
                let mut typ_name = Tree {
                    typ: TreeType::Name,
                    tkn: self.expect(TokenType::Name)?,
                    children: vec![],
                };
                if self.at(TokenType::OpenSquare) {
                    typ_name.children.push(self.parse_type_arr()?);
                }
                ptr.children.push(typ_name);
                ptr
            }
            _ => {
                let mut name = Tree {
                    typ: TreeType::Name,
                    tkn: self.expect(TokenType::Name)?,
                    children: vec![],
                };
                if self.at(TokenType::OpenSquare) {
                    name.children.push(self.parse_type_arr()?);
                }
                name
            }
        };
        children.push(name);
        Ok(Tree {
            typ: TreeType::TypeDecl,
            tkn,
            children,
        })
    }

    fn parse_stmt_let(&mut self) -> Result<Tree, String> {
        assert!(self.at(TokenType::LetKeyword));
        let (tkn, mut children) = self.open();
        self.expect(TokenType::LetKeyword)?;
        children.push(Tree {
            typ: TreeType::Name,
            tkn: self.expect(TokenType::Name)?,
            children: vec![],
        });
        children.push(self.parse_type_decl()?);
        self.expect(TokenType::Equal)?;
        children.push(self.parse_expr()?);
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtLet,
            tkn,
            children,
        })
    }

    fn parse_stmt_assign(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        let node = match self.nth(1) {
            TokenType::Equal => Tree {
                typ: TreeType::Name,
                tkn: self.expect(TokenType::Name)?,
                children: vec![],
            },
            TokenType::OpenSquare => Tree {
                typ: TreeType::ExprArrAccess,
                tkn: self.expect(TokenType::Name)?,
                children: vec![self.parse_expr_array()?],
            },
            _ => todo!(),
        };
        self.expect(TokenType::Equal)?;
        children.push(node);
        children.push(self.parse_expr()?);
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtAssign,
            tkn,
            children,
        })
    }

    fn parse_stmt_assign_ptr(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        let mut var_ptr = Tree {
            typ: TreeType::Pointer,
            tkn: self.expect(TokenType::Asterisk)?,
            children: vec![],
        };
        let var_name = Tree {
            typ: TreeType::Name,
            tkn: self.expect(TokenType::Name)?,
            children: vec![],
        };
        var_ptr.children.push(var_name);
        children.push(var_ptr);
        self.expect(TokenType::Equal)?;
        children.push(self.parse_expr()?);
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtAssign,
            tkn,
            children,
        })
    }

    fn parse_stmt_expr(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        children.push(self.parse_expr()?);
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtExpr,
            tkn,
            children,
        })
    }

    fn parse_arg(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        children.push(self.parse_expr()?);
        Ok(Tree {
            typ: TreeType::Arg,
            tkn,
            children,
        })
    }

    fn parse_arg_list(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound) && !self.eof() {
            children.push(self.parse_arg()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ArgList,
            tkn,
            children,
        })
    }

    fn parse_stmt_if(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenRound)?;
        children.push(self.parse_expr()?);
        self.expect(TokenType::ClosingRound)?;
        children.push(self.parse_block()?);
        if self.eat(TokenType::ElseKeyword) {
            children.push(self.parse_block()?);
        }
        Ok(Tree {
            typ: TreeType::StmtIf,
            tkn,
            children,
        })
    }

    fn parse_stmt_return(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::ReturnKeyword)?;
        if !self.eat(TokenType::Semi) {
            children.push(self.parse_expr()?);
            self.expect(TokenType::Semi)?;
        }
        Ok(Tree {
            typ: TreeType::StmtReturn,
            tkn,
            children,
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
        let (tkn, mut children) = self.open();
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
            typ: TreeType::Block,
            tkn,
            children,
        })
    }

    fn parse_param(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::Name)?;
        children.push(self.parse_type_decl()?);
        Ok(Tree {
            typ: TreeType::Param,
            tkn,
            children,
        })
    }

    fn parse_param_list(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound) && !self.eof() {
            children.push(self.parse_param()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ParamList,
            tkn,
            children,
        })
    }

    fn parse_return_type(&mut self) -> Result<Tree, String> {
        assert!(self.at(TokenType::Arrow));
        let (tkn, mut children) = self.open();
        self.expect(TokenType::Arrow)?;
        children.push(Tree {
            typ: TreeType::Name,
            tkn: self.expect(TokenType::Name)?,
            children: vec![],
        });
        Ok(Tree {
            typ: TreeType::TypeDecl,
            tkn,
            children,
        })
    }

    fn parse_func(&mut self) -> Result<Tree, String> {
        assert!(self.at(TokenType::FnKeyword));
        let (tkn, mut children) = self.open();
        self.expect(TokenType::FnKeyword)?;
        children.push(Tree {
            typ: TreeType::Name,
            tkn: self.expect(TokenType::Name)?,
            children: vec![],
        });
        children.push(self.parse_param_list()?);
        if self.at(TokenType::Arrow) {
            children.push(self.parse_return_type()?);
        }
        children.push(self.parse_block()?);
        Ok(Tree {
            typ: TreeType::Func,
            tkn,
            children,
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
            typ: TreeType::File,
            tkn,
            children,
        });
        Ok(self.root.clone().unwrap())
    }
}
