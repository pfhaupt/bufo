use std::cell::Cell;

use crate::lexer::{Token, TokenType};
use crate::codegen::ERR_STR;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TreeType {
    ErrorTree,
    File,
    Func,
    ParamList, Param,
    ArgList, Arg,
    Block,
    Stmt, StmtExpr, StmtLet, StmtAssign, StmtCall, StmtIf,
    Expr, ExprName, ExprLiteral, ExprBinary, ExprParen
}

#[derive(Debug, Clone)]
pub struct Tree {
    pub typ: Option<TreeType>,
    pub tkn: Option<Token>,
    pub children: Vec<Box<Tree>>,
}

enum Event {
    Open { typ: TreeType },
    Close,
    Advance,
}

struct MarkOpened {
    index: usize
}

struct MarkClosed {
    index: usize,
}

pub struct Parser {
    tokens: Vec<Token>,
    ptr: usize,
    fuel: Cell<u32>,
    events: Vec<Event>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, ptr: 0, fuel: Cell::new(256), events: vec![] }
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened { index: self.events.len() };
        self.events.push(Event::Open { typ: TreeType::ErrorTree });
        mark
    }

    fn close(&mut self, m: MarkOpened, typ: TreeType) -> MarkClosed {
        self.events[m.index] = Event::Open { typ };
        self.events.push(Event::Close);
        MarkClosed { index: m.index }
    }

    fn open_before(&mut self, m: MarkClosed) -> MarkOpened { 
        let mark = MarkOpened { index: m.index };
        self.events.insert(
          m.index,
          Event::Open { typ: TreeType::ErrorTree },
        );
        mark
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
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
        self.tokens.get(self.ptr + lookahead).map_or(TokenType::EOF, |it|it.get_type())
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

    fn expect(&mut self, typ: TokenType) -> Result<(), String> {
        let t = typ.clone();
        if self.eat(typ) {
            return Ok(());
        }
        // TODO: Improve error handling
        let (prev, typ) = match self.tokens.get(self.ptr) {
            None => (self.tokens.get(self.ptr - 1).expect("Expected Some value, found None instead. This might be a bug in Parsing or Lexing."), TokenType::EOF),
            Some(prev) => (prev, prev.get_type())
        };
        Err(format!("{}: {:?}: Expected {:?}, found {:?}", ERR_STR, prev.get_loc(), t, typ))
    }

    fn parse_expr_delim(&mut self) -> Result<MarkClosed, String> {
        let m = self.open();
        Ok(match self.nth(0) {
            TokenType::IntLiteral => {
                self.advance();
                self.close(m, TreeType::ExprLiteral)
            }
            TokenType::Name => {
                self.advance();
                self.close(m, TreeType::ExprName)
            },
            TokenType::OpenParenthesis => {
                self.expect(TokenType::OpenParenthesis)?;
                self.parse_expr()?;
                self.expect(TokenType::ClosingParenthesis)?;
                self.close(m, TreeType::ExprParen)
            }
            e => {
                let ptr = if self.ptr == self.tokens.len() { self.ptr - 1 } else { self.ptr };
                return Err(
                    format!("{}: {:?}: Expected Expr, found {:?}",
                    ERR_STR,
                    self.tokens.get(ptr).unwrap().get_loc(),
                    e));
            }
        })
    }

    fn parse_expr_rec(&mut self, left: TokenType) -> Result<(), String> {
        let mut lhs = self.parse_expr_delim()?;
        
        loop {
            let right = self.nth(0);
            if Self::right_binds_tighter(left, right) {
                let m = self.open_before(lhs);
                self.advance();
                self.parse_expr_rec(right)?;
                lhs = self.close(m, TreeType::ExprBinary);
            } else {
                break;
            }
        }
        Ok(())
    }

    fn right_binds_tighter(left: TokenType, right: TokenType) -> bool {
        fn tightness(typ: TokenType) -> Option<usize> {
            [
                [TokenType::Plus, TokenType::Minus].as_slice(),
                &[TokenType::Mult, TokenType::Div],
            ]
            .iter()
            .position(|l|l.contains(&typ))
        }
        let Some(right_tight) = tightness(right) else {
            return false
        };
        let Some(left_tight) = tightness(left) else {
            assert!(left == TokenType::EOF);
            return true;
        };
        right_tight > left_tight
    }
    

    fn parse_expr(&mut self) -> Result<(), String> {
        self.parse_expr_rec(TokenType::EOF)
    }

    fn parse_expr_cmp(&mut self) -> Result<(), String> {
        let m = self.open();
        self.parse_expr()?;
        let mut found = false;
        for typ in
        vec![TokenType::CmpEq,
            TokenType::CmpNeq,
            TokenType::CmpGt,
            TokenType::CmpGte,
            TokenType::CmpLt,
            TokenType::CmpLte] {
                if self.eat(typ) {
                    found = true;
                    break;
                }
        }
        if !found {
            let ptr = if self.ptr == self.tokens.len() { self.ptr - 1 } else { self.ptr };
            let tkn = self.tokens.get(ptr).unwrap();
            return Err(format!("{}: {:?}: Expected Comparison, found {:?}", ERR_STR, tkn.get_loc(), tkn.get_type()));
        }
        self.parse_expr()?;
        self.close(m, TreeType::ExprBinary);
        Ok(())
    }
    
    fn parse_stmt_let(&mut self) -> Result<(), String> {
        assert!(self.at(TokenType::LetKeyword));
        let m = self.open();
        self.expect(TokenType::LetKeyword)?;
        self.expect(TokenType::Name)?;
        self.expect(TokenType::Equal)?;
        self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        self.close(m, TreeType::StmtLet);
        Ok(())
    }

    fn parse_stmt_assign(&mut self) -> Result<(), String> {
        assert!(self.at(TokenType::Name));
        let m = self.open();
        self.expect(TokenType::Name)?;
        self.expect(TokenType::Equal)?;
        self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        self.close(m, TreeType::StmtAssign);
        Ok(())
    }

    fn parse_stmt_expr(&mut self) -> Result<(), String> {
        let m = self.open();
        self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        self.close(m, TreeType::StmtExpr);
        Ok(())
    }

    fn parse_arg(&mut self) -> Result<(), String> {
        let m = self.open();
        self.parse_expr()?;
        self.close(m, TreeType::Arg);
        if self.eat(TokenType::Comma) {
            self.parse_arg()?;
        }
        Ok(())
    }

    fn parse_arg_list(&mut self) -> Result<(), String> {
        let m = self.open();
        self.expect(TokenType::OpenParenthesis)?;
        while !self.at(TokenType::ClosingParenthesis) && !self.eof() {
            self.parse_arg()?;
        }
        self.expect(TokenType::ClosingParenthesis)?;
        self.close(m, TreeType::ArgList);
        Ok(())
    }

    fn parse_stmt_call(&mut self) -> Result<(), String> {
        let m = self.open();
        self.expect(TokenType::Name)?;
        self.parse_arg_list()?;
        self.expect(TokenType::Semi)?;
        self.close(m, TreeType::StmtCall);
        Ok(())
    }

    fn parse_stmt_if(&mut self) -> Result<(), String> {
        let m = self.open();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenParenthesis)?;
        self.parse_expr_cmp()?;
        self.expect(TokenType::ClosingParenthesis)?;
        self.parse_block()?;
        if self.eat(TokenType::ElseKeyword) {
            self.parse_block()?;
        }
        self.close(m, TreeType::StmtIf);
        Ok(())
    }

    fn parse_block(&mut self) -> Result<(), String> {
        if !self.at(TokenType::OpenBracket) {
            let ptr = if self.ptr == self.tokens.len() { self.ptr - 1 } else { self.ptr };
            let tkn = self.tokens.get(ptr).unwrap();
            return Err(format!("{}: {:?}: Expected `{{`, found `{:?}`",
                ERR_STR, tkn.get_loc(), self.nth(0)));
        }
        let m = self.open();
        self.expect(TokenType::OpenBracket)?;
        while !self.at(TokenType::ClosingBracket) && !self.eof() {
            match self.nth(0) {
                TokenType::LetKeyword => self.parse_stmt_let()?,
                TokenType::IfKeyword => self.parse_stmt_if()?,
                TokenType::Name => {
                    match self.nth(1) {
                        TokenType::OpenParenthesis => self.parse_stmt_call()?,
                        _ => self.parse_stmt_assign()?,
                    }
                },
                _ => self.parse_stmt_expr()?
            }
        }
        self.expect(TokenType::ClosingBracket)?;
        self.close(m, TreeType::Block);
        Ok(())
    }

    fn parse_param(&mut self) -> Result<(), String> {
        let m = self.open();
        self.expect(TokenType::Name)?;
        if self.at(TokenType::Comma) {
            self.advance();
        }
        self.close(m, TreeType::Param);
        Ok(())
    }

    fn parse_param_list(&mut self) -> Result<(), String> {
        let m = self.open();
        self.expect(TokenType::OpenParenthesis)?;
        while !self.at(TokenType::ClosingParenthesis) && !self.eof() {
            self.parse_param()?;
        }
        self.expect(TokenType::ClosingParenthesis)?;
        self.close(m, TreeType::ParamList);
        Ok(())
    }

    fn parse_func(&mut self) -> Result<(), String> {
        assert!(self.at(TokenType::FnKeyword));
        let m = self.open();
        self.expect(TokenType::FnKeyword)?;
        self.expect(TokenType::Name)?;
        self.parse_param_list()?;
        self.parse_block()?;
        self.close(m, TreeType::Func);
        Ok(())
    }

    pub fn parse_file(&mut self) -> Result<(), String> {
        assert_eq!(TokenType::EOF as u8 + 1, 25, "Not all TokenTypes are handled in parse_file()");
        let m = self.open();
        while !self.eof() {
            if self.at(TokenType::FnKeyword) {
                self.parse_func()?;
            } else {
                let prev = self.tokens.get(self.ptr).unwrap();
                return Err(format!("{}: {:?}: Expected Function, found {:?}", ERR_STR, prev.get_loc(), prev.get_type()))
            }
        }
        self.close(m, TreeType::File);
        Ok(())
    }

    pub fn build_tree(self) -> Tree {
        let mut tokens = self.tokens.into_iter();
        let mut events = self.events;
        let mut stack = Vec::new();
        
        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                Event::Open { typ } => {
                    stack.push(Tree { typ: Some(typ), tkn: None, children: Vec::new() })
                },
                Event::Close => {
                    let t = stack.pop().unwrap();
                    stack.last_mut().unwrap().children.push(Box::new(t))
                },
                Event::Advance => {
                    let t = tokens.next().unwrap();
                    match t.get_type() {
                        TokenType::ClosingBracket
                        | TokenType::ClosingParenthesis
                        | TokenType::OpenBracket
                        | TokenType::OpenParenthesis
                        | TokenType::Semi
                        | TokenType::Comma => {}, // There's no reason to clutter the AST with this
                        _ => stack.last_mut().unwrap().children.push(Box::new(Tree { typ: None, tkn: Some(t), children: vec![] }))
                    }
                }
            }
        }
        assert!(stack.len() == 1);
        assert!(tokens.next().is_none(), "{:?}", tokens.next().unwrap());

        stack.pop().unwrap()
    }
}