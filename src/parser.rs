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
    ExprLiteral,
    ExprBinary,
    ExprParen,
    ExprCall,
    TypeDecl,
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
    tokens: Vec<Token>,
    ptr: usize,
    fuel: Cell<u32>,
    root: Option<Tree>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ptr: 0,
            fuel: Cell::new(256),
            root: None,
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
            TokenType::Name => match self.nth(1) {
                TokenType::OpenParenthesis => self.parse_expr_call()?,
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
            },
            TokenType::OpenParenthesis => {
                let (tkn, mut children) = self.open();
                self.expect(TokenType::OpenParenthesis)?;
                children.push(self.parse_expr()?);
                self.expect(TokenType::ClosingParenthesis)?;
                Tree {
                    typ: TreeType::ExprParen,
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
                &[TokenType::Mult, TokenType::Div],
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

    fn parse_type_decl(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::TypeDecl)?;
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
        assert!(self.at(TokenType::Name));
        let (tkn, mut children) = self.open();
        children.push(Tree {
            typ: TreeType::Name,
            tkn: self.expect(TokenType::Name)?,
            children: vec![],
        });
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
        self.expect(TokenType::OpenParenthesis)?;
        while !self.at(TokenType::ClosingParenthesis) && !self.eof() {
            children.push(self.parse_arg()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingParenthesis)?;
        Ok(Tree {
            typ: TreeType::ArgList,
            tkn,
            children,
        })
    }

    fn parse_stmt_if(&mut self) -> Result<Tree, String> {
        let (tkn, mut children) = self.open();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenParenthesis)?;
        children.push(self.parse_expr()?);
        self.expect(TokenType::ClosingParenthesis)?;
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
        if !self.at(TokenType::OpenBracket) {
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
        self.expect(TokenType::OpenBracket)?;
        while !self.at(TokenType::ClosingBracket) && !self.eof() {
            match self.nth(0) {
                TokenType::LetKeyword => children.push(self.parse_stmt_let()?),
                TokenType::IfKeyword => children.push(self.parse_stmt_if()?),
                TokenType::ReturnKeyword => children.push(self.parse_stmt_return()?),
                TokenType::Name => match self.nth(1) {
                    TokenType::Equal => children.push(self.parse_stmt_assign()?),
                    _ => children.push(self.parse_stmt_expr()?),
                },
                _ => children.push(self.parse_stmt_expr()?),
            }
        }
        self.expect(TokenType::ClosingBracket)?;
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
        self.expect(TokenType::OpenParenthesis)?;
        while !self.at(TokenType::ClosingParenthesis) && !self.eof() {
            children.push(self.parse_param()?);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingParenthesis)?;
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
            28,
            "Not all TokenTypes are handled in parse_file()"
        );
        let tkn = Token::new(
            TokenType::File,
            String::from("root"),
            Location::new(String::from("idk"), 0, 0),
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
