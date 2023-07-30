use std::collections::{HashMap, HashSet};
use std::cell::Cell;

use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum Ast {
    Const(u64),
    Var(usize),
    Block(Vec<Ast>),
    Assign(usize, Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    Sub(Box<Ast>, Box<Ast>),
    Func(String, Box<Ast>)
}

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
    pub typ: TreeType,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {
    Token(Token),
    Tree(Tree)
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

#[derive(Debug)]
enum ParserError {
    InvalidToken,
    UnmatchedBracket,
    UnexpectedVarValue,
}

fn error_to_string(error: ParserError) -> String {
    match error {
        ParserError::InvalidToken => { String::from("Found invalid Token when parsing!") },
        ParserError::UnexpectedVarValue => { String::from("Found invalid Token when assigning value to variable") },
        ParserError::UnmatchedBracket => { String::from("Could not find matching bracket") },
        e => { unimplemented!("no error message for {:?}", e) }
    }
}

const TWO_OP: [&str; 4] = ["+", "-", "*", "/"];

/* #[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    ast: Option<Ast>,
    memory_size: usize,
    memory: Vec<u64>,
    memory_ptr: usize,
    mem_lookup: HashMap<String, usize>,
    intrinsics: HashSet<String>
}

impl Parser {
    pub fn new() -> Self {
        let mem_lookup = HashMap::new();
        let mut intrinsics = HashSet::new();
        intrinsics.extend(vec!["print"].iter().map(|s| String::from(*s)).collect::<Vec<String>>());
        let memory_size = 128;
        Self { tokens: vec![], memory_size, memory: vec![0; memory_size], intrinsics, memory_ptr: 0, mem_lookup, ast: None }
    }

    pub fn set_context(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
    }

    pub fn get_program(&self) -> (Ast, Vec<u64>, HashMap<String, usize>, HashSet<String>) {
        if self.ast.is_none() {
            panic!("Attempted to fetch program before parsing!");
        }
        (self.ast.clone().unwrap(), self.memory.clone(), self.mem_lookup.clone(), self.intrinsics.clone())
    }

    fn get_mem_id(&mut self) -> usize {
        let m = self.memory_ptr;
        self.memory_ptr += 1;
        m
    }

    fn get_op_count(&self, token: &Token) -> Result<usize, String> {
        match token.get_type() {
            TokenType::Symbol => {
                match token.get_value().as_str() {
                    s if TWO_OP.contains(&s) => {
                        Ok(2)
                    },
                    _ => { Err("uhh x2".to_uppercase() )}
                }
            },
            _ => { Err("uhh".to_ascii_lowercase()) }
        }
    }

    fn get_matching(&self, token_id: usize) -> Result<usize, String> {
        let t = &self.tokens[token_id];
        assert_eq!(t.get_type(), TokenType::OpenParenthesis);
        let mut cnt = 1;
        for i in (token_id+1)..self.tokens.len() {
            let o = &self.tokens[i];
            match o.get_type() {
                TokenType::OpenParenthesis => { cnt += 1 },
                TokenType::ClosingParenthesis => {
                    cnt -= 1;
                    if cnt == 0 {
                        return Ok(i);
                    }
                },
                _ => {}
            }
        }
        Err(error_to_string(ParserError::UnmatchedBracket))
    }

    fn parse_number(&self, val: &String) -> Result<Ast, String> {
        match val.parse() {
            Ok(v) => { Ok(Ast::Const(v)) },
            Err(e) => { return Err(e.to_string()); }
        }
    }

    fn parse_variable(&mut self, start_id: usize) -> Result<(usize, Ast), String> {
        let mem_id = self.get_mem_id();
        let defvar = &self.tokens[start_id];
        assert_eq!(defvar.get_value(), String::from("defvar"));
        let varname = &self.tokens[start_id + 1];
        let name_as_str = varname.get_value();
        assert_eq!(varname.get_type(), TokenType::Name);
        let varvalue = &self.tokens[start_id + 2];
        let (i, r) = match varvalue.get_type() {
            TokenType::OpenParenthesis => {
                self.parse_block(start_id + 2)?
            },
            TokenType::Name => {
                todo!("Assign other variable to variable")
            },
            TokenType::Number => {
                (start_id + 3, self.parse_number(&self.tokens[start_id + 2].get_value())?)
            },
            e => {
                return Err(format!("Error: {}: Found `{:?}`, expected String, Number or (.", error_to_string(ParserError::UnexpectedVarValue), e).to_string());
            }
        };
        self.mem_lookup.insert(name_as_str, mem_id);
        Ok((i, Ast::Assign(mem_id, Box::new(r))))
    }

    fn parse_symbol(&mut self, start_id: usize) -> Result<(usize, Ast), String> {
        let symbol = &self.tokens[start_id].clone();
        let operators = self.get_op_count(symbol)?;
        assert_eq!(operators, 2, "There is currently no support for operations with more than 2 operands.");
        let mut operands = vec![];
        let mut id = start_id;
        for _ in 0..operators {
            let (i, ast) = match self.tokens[id + 1].get_type() {
                TokenType::Name => {
                    if let Some(mem_id) = self.mem_lookup.get(&self.tokens[id + 1].get_value()) {
                        (id + 1, Ast::Var(*mem_id))
                    } else {
                        todo!("Handle this situation in parse_symbol: Could not find symbol in mem_lookup!")
                    }
                },
                TokenType::OpenParenthesis => {
                    self.parse_block(id + 1)?
                },
                TokenType::Number => {
                    (id + 1, self.parse_number(&self.tokens[id + 1].get_value())?)
                },
                e => {
                    return Err(error_to_string(ParserError::InvalidToken) + &format!(" Got: {:?}, expected String, Number or (.", e))
                }
            };
            id = i;
            operands.push(ast);
        }
        match symbol.get_value().as_str() {
            "+" => { Ok((id, Ast::Add(Box::new(operands[0].clone()), Box::new(operands[1].clone())))) }
            "-" => { Ok((id, Ast::Sub(Box::new(operands[0].clone()), Box::new(operands[1].clone())))) }
            e => { todo!("parse_symbol() for Symbol {}", e) }
        }
    }

    fn parse_function(&mut self, start_id: usize) -> Result<(usize, Ast), String> {
        let name = &self.tokens[start_id];
        if name.get_type() != TokenType::Name {
            return Err(error_to_string(ParserError::InvalidToken));
        }
        let name = name.get_value();
        let (id, ast) = self.parse_block(start_id + 1)?;
        Ok((id + 1, Ast::Func(name, Box::new(ast))))
    }

    fn parse_block(&mut self, start_id: usize) -> Result<(usize, Ast), String> {
        let end_id = self.get_matching(start_id)?;
        // println!("Parsing tokens from indices {} to {}", start_id, end_id);
        let mut id = start_id + 1;
        let mut ast_vec = vec![];
        while id < end_id {
            let t = &self.tokens[id];
            match t.get_type() {
                TokenType::OpenParenthesis => {
                    let (block_end, ast) = self.parse_block(id)?;
                    ast_vec.push(ast);
                    id = block_end;
                },
                TokenType::Name => {
                    match t.get_value() {
                        s if s == String::from("defvar") => {
                            let (i, ast) = self.parse_variable(id)?;
                            ast_vec.push(ast);
                            id = i;
                        },
                        s if s == String::from("print") => {
                            let (i, ast) = self.parse_function(id)?;
                            ast_vec.push(ast);
                            id = i;
                        },
                        v if self.mem_lookup.contains_key(&v) => {
                            let mem_id = self.mem_lookup.get(&v).unwrap();
                            ast_vec.push(Ast::Var(*mem_id));
                            id += 1;
                        }
                        v => { return Err(error_to_string(ParserError::InvalidToken) + &format!(" Got: `{}`", v)) }
                    }
                },
                TokenType::Symbol => {
                    let (i, ast) = self.parse_symbol(id)?;
                    ast_vec.push(ast);
                    id = i;
                },
                _ => { return Err(error_to_string(ParserError::InvalidToken) + &format!(" Got: `{}`", t.get_value()))},
            }
            id += 1;
        }
        Ok((end_id, Ast::Block(ast_vec)))
    }

    pub fn parse(&mut self) -> Result<(), String> {
        let mut id = 0;
        let mut ast_vec = vec![];
        while id < self.tokens.len() {
            let (next_id, ast) = self.parse_block(id)?;
            ast_vec.push(ast);
            id = next_id + 1;
        }
        self.ast = Some(Ast::Block(ast_vec));
        Ok(())
    }
}
 */

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
        self.tokens.get(self.ptr + lookahead).map_or(TokenType::EOF, |it|it.typ)
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
                    stack.push(Tree { typ, children: Vec::new() })
                },
                Event::Close => {
                    let t = stack.pop().unwrap();
                    stack.last_mut().unwrap().children.push(Node::Tree(t))
                },
                Event::Advance => {
                    let t = tokens.next().unwrap();
                    match t.typ {
                        TokenType::ClosingBracket
                        | TokenType::ClosingParenthesis
                        | TokenType::OpenBracket
                        | TokenType::OpenParenthesis
                        | TokenType::Semi => {}, // There's no reason to clutter the AST with this
                        _ => stack.last_mut().unwrap().children.push(Node::Token(t))
                    }
                }
            }
        }
        assert!(stack.len() == 1);
        assert!(tokens.next().is_none(), "{:?}", tokens.next().unwrap());

        stack.pop().unwrap()
    }
}