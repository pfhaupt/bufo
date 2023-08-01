use std::cell::Cell;

use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TreeType {
    ErrorTree,
    File,
    Func,
    Block,
    Stmt, StmtExpr, StmtLet,
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

    fn expect(&mut self, typ: TokenType) {
        let t = typ.clone();
        if self.eat(typ) {
            return;
        }
        // TODO: Improve error handling
        eprintln!("expected {t:?}");
    }

    fn advance_with_error(&mut self, error: &str) {
        let m = self.open();
        eprintln!("{error}");
        self.advance();
        self.close(m, TreeType::ErrorTree);
    }

    fn parse_expr_delim(&mut self) -> MarkClosed {
        let m = self.open();
        match self.nth(0) {
            TokenType::IntLiteral => {
                self.advance();
                self.close(m, TreeType::ExprLiteral)
            }
            TokenType::Name => {
                self.advance();
                self.close(m, TreeType::ExprName)
            },
            TokenType::OpenParenthesis => {
                self.expect(TokenType::OpenParenthesis);
                self.parse_expr();
                self.expect(TokenType::ClosingParenthesis);
                self.close(m, TreeType::ExprParen)
            }
            _ => {
                if !self.eof() {
                    self.advance();
                }
                self.close(m, TreeType::ErrorTree)
            }
        }
    }

    fn parse_expr_rec(&mut self, left: TokenType) {
        let mut lhs = self.parse_expr_delim();
        
        loop {
            let right = self.nth(0);
            if Self::right_binds_tighter(left, right) {
                let m = self.open_before(lhs);
                self.advance();
                self.parse_expr_rec(right);
                lhs = self.close(m, TreeType::ExprBinary);
            } else {
                break;
            }
        }
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
    

    fn parse_expr(&mut self) {
        self.parse_expr_rec(TokenType::EOF);
    }
    
    fn parse_let_expr(&mut self) {
        assert!(self.at(TokenType::LetKeyword));
        let m = self.open();
        self.expect(TokenType::LetKeyword);
        self.expect(TokenType::Name);
        self.expect(TokenType::Equal);
        self.parse_expr();
        self.expect(TokenType::Semi);
        self.close(m, TreeType::StmtLet);
    }

    fn parse_stmt_expr(&mut self) {
        let m = self.open();
        self.parse_expr();
        self.expect(TokenType::Semi);
        self.close(m, TreeType::StmtExpr);
    }

    fn parse_block(&mut self) {
        assert!(self.at(TokenType::OpenBracket));
        let m = self.open();
        self.expect(TokenType::OpenBracket);
        while !self.at(TokenType::ClosingBracket) && !self.eof() {
            match self.nth(0) {
                TokenType::LetKeyword => self.parse_let_expr(),
                _ => self.parse_stmt_expr()
            }
        }
        self.expect(TokenType::ClosingBracket);
        self.close(m, TreeType::Block);
    }

    fn parse_func(&mut self) {
        assert!(self.at(TokenType::FnKeyword));
        let m = self.open();
        self.expect(TokenType::FnKeyword);
        self.expect(TokenType::Name);
        self.expect(TokenType::OpenParenthesis);
        self.expect(TokenType::ClosingParenthesis);
        if self.at(TokenType::OpenBracket) {
            self.parse_block();
        }
        self.close(m, TreeType::Func);
    }

    pub fn parse_file(&mut self) {
        let m = self.open();
        while !self.eof() {
            if self.at(TokenType::FnKeyword) {
                self.parse_func();
            } else {
                self.advance_with_error("expected a function");
            }
        }
        self.close(m, TreeType::File);
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
                        | TokenType::Semi => {}, // There's no reason to clutter the AST with this
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