use std::cell::Cell;
use std::collections::VecDeque;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use crate::parser::{Location, Tree, TreeType, TokenType, COMPARATOR_TYPES};
use crate::codegen::{ERR_STR, NOTE_STR, WARN_STR};
use crate::checker::Type;
use super::nodes::{self, Expression};

// We always store the N-1 next tokens for lookahead purposes, even if we only use 1 right now
const LOOKAHEAD_LIMIT: usize = 3;

pub const BUILT_IN_VARIABLES: [&str; 3] = [
    "STACK_OVERFLOW_CODE",
    "FUNCTION_COUNTER",
    "FUNCTION_LIMIT"
];
pub const BUILT_IN_FUNCTIONS: [&str; 3] = [
    "EXIT",
    "MALLOC",
    "SIZEOF"
];

#[macro_export]
macro_rules! parse_snippet {
    ($filepath: expr, $node: ty, $snippet: expr) => {
        {
            use crate::new::new_parser::{Parser, Parsable};
            let mut parser = Parser::new();
            parser.set_filepath_unchecked($filepath);
            parser.set_source($snippet);
            parser = parser.initialize()?;
            <$node>::parse(&mut parser)?
        }
    };
}

#[derive(Debug)]
pub struct Token {
    location: Location,
    value: String,
    token_type: TokenType
}

impl Token {
    fn new() -> Self {
        Self {
            location: Location::anonymous(),
            value: String::new(),
            token_type: TokenType::Eof
        }
    }
    fn location(self, location: Location) -> Self { Self { location, ..self } }
    fn value(self, value: String) -> Self { Self { value, ..self } }
    fn token_type(self, token_type: TokenType) -> Self { Self { token_type, ..self } }
}


#[derive(Debug, Clone)]
pub enum Operation {
    PLUS,
    MINUS,
    MULT,
    DIV,
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE
}

impl Operation {
    fn from(s: String) -> Self {
        debug_assert_eq!(Operation::GTE as u8 + 1, 10);
        match s.as_str() {
            "+" => Self::PLUS,
            "-" => Self::MINUS,
            "*" => Self::MULT,
            "/" => Self::DIV,
            "==" => Self::EQ,
            "!=" => Self::NEQ,
            "<" => Self::LT,
            "<=" => Self::LTE,
            ">" => Self::GT,
            ">=" => Self::GTE,
            _ => unreachable!()
        }
    }
}

#[derive(Default)]
pub struct Parser {
    filepath: PathBuf,
    filename: String,
    source: Vec<char>,
    fuel: Cell<u32>,
    lookahead: VecDeque<Token>,
    current_class: String,
    current_char: usize,
    current_line: usize,
    line_start: usize,
    print_debug: bool,
}

impl Parser {
    // ---------- Start of Builder Pattern ----------
    pub fn new() -> Self {
        Self {
            current_line: 1,
            ..Default::default()
        }
    }

    pub fn filepath(self, filepath: &str) -> Result<Self, String> {
        match fs::read_to_string(filepath) {
            Ok(source) => {
                let pb = PathBuf::from(filepath);
                let p = Self {
                    filepath: pb.clone(),
                    filename: pb.into_os_string().into_string().unwrap(),
                    source: source.chars().collect(),
                    ..self
                };
                Ok(p)
            }
            Err(e) => {
                Err(format!(
                    "{}: Parsing file failed because: {}", ERR_STR, e,
                ))
            }
        }
    }

    pub fn debug(self, print_debug: bool) -> Self {
        Self {
            print_debug,
            ..self
        }
    }

    pub fn initialize(self) -> Result<Self, String> {
        let mut new = self;
        new.fill_lookup()?;
        Ok(new)
    }

    pub fn set_filepath_unchecked(&mut self, filepath: &str) {
        self.filepath = PathBuf::from(filepath);
        self.filename = self.filepath.clone().into_os_string().into_string().unwrap();
    }

    pub fn set_source(&mut self, source: &str) {
        self.source = source.chars().collect();
    }
    // ---------- End of Builder Pattern ----------
    // ---------- Start of Lexer ----------
    fn lexed_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn trim_whitespace(&mut self) -> Result<(), String> {
        while !self.lexed_eof() && self.source[self.current_char].is_whitespace() {
            match self.source[self.current_char] {
                '\r' => {
                    self.current_char += 2;
                    self.current_line += 1;
                    self.line_start = self.current_char;
                } // Windows
                '\n' => {
                    self.current_char += 1;
                    self.current_line += 1;
                    self.line_start = self.current_char;
                } // Linux
                _ => {
                    self.current_char += 1;
                } // Normal space
            }
        }
        Ok(())
    }

    fn current_char(&self) -> char {
        self.source[self.current_char]
    }

    fn next_char(&mut self) -> char {
        if self.lexed_eof() {
            '\0'
        } else {
            let c = self.current_char();
            self.current_char += 1;
            c
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        macro_rules! fill_buffer {
            ($start: expr, $cond: expr, $stepback: expr) => {
                {
                    let mut value = String::from($start);
                    loop {
                        let nc = self.next_char();
                        if $cond(nc) {
                            if $stepback {
                                self.current_char -= 1;
                            } else {
                                value.push(nc);
                            }
                            break value;
                        }
                        value.push(nc);
                    }
                }
            };
            ($start: expr, $cond: expr) => {
                fill_buffer!($start, $cond, true)
            }
        }
        debug_assert_eq!(
            TokenType::Eof as u8 + 1,
            36,
            "Not all TokenTypes are handled in next_token()"
        );
        self.trim_whitespace();
        let c = self.next_char();
        let loc = self.get_location();
        let (typ, value) = match c {
            '0'..='9' => {
                let value = fill_buffer!(c, 
                    |c: char| { !c.is_alphanumeric() || c == '\0' }
                );
                (TokenType::IntLiteral, value)
            }
            'A'..='Z' | 'a'..='z' => {
                let value = fill_buffer!(c,
                    |c: char| { (!c.is_alphanumeric() && c != '_') || c == '\0' }
                );
                let typ = match value.as_str() {
                    "class" => TokenType::ClassKeyword,
                    "func" => TokenType::FunctionKeyword,
                    "feat" => TokenType::FeatureKeyword,
                    "let" => TokenType::LetKeyword,
                    "if" => TokenType::IfKeyword,
                    "else" => TokenType::ElseKeyword,
                    "return" => TokenType::ReturnKeyword,
                    _ if BUILT_IN_FUNCTIONS.contains(&value.as_str()) => TokenType::BuiltInFunction,
                    _ => TokenType::Identifier,
                };
                (typ, value)
            }
            '\"' => {
                let mut value = fill_buffer!(c,
                    |c: char| { c == '"' || c == '\0' },
                    false
                );
                if value.contains("\\") {
                    println!("{}: {:?}: Escape sequences in Strings are not supported yet.", WARN_STR, loc);
                }
                if value.chars().filter(|c| *c == '"').count() != 2 {
                    return Err(format!(
                        "{}: {:?}: Unterminated String Literal.",
                        ERR_STR,
                        loc
                    ));
                }
                (TokenType::StrLiteral, value)
            }
            '\'' => {
                let mut value = fill_buffer!(c,
                    |c: char| { c == '\'' || c == '\0' },
                    false
                );
                if value.contains("\\") {
                    println!("{}: {:?}: Escape sequences in Char Literals are not supported yet.", WARN_STR, loc);
                }
                if value.len() != 3 {
                    return Err(format!("{}: {:?}: Char Literal is expected to be a single char, got {}.",
                        ERR_STR,
                        loc,
                        value,
                    ))
                }
                (TokenType::CharLiteral, value)
            }
            '!' => {
                match self.next_char() {
                    '=' => (TokenType::CmpNeq, String::from("!=")),
                    _ => return Err(format!(
                        "{}: {:?}: Unexpected Symbol `{}`",
                        ERR_STR,
                        loc,
                        c
                    ))
                }
            }
            '=' => {
                match self.next_char() {
                    '=' => (TokenType::CmpEq, String::from("==")),
                    _ => {
                        self.current_char -= 1;
                        (TokenType::Equal, String::from(c))
                    }
                }
            }
            '<' => {
                match self.next_char() {
                    '=' => (TokenType::CmpLte, String::from("<=")),
                    _ => {
                        self.current_char -= 1;
                        (TokenType::CmpLt, String::from(c))
                    }
                }
            }
            '>' => {
                match self.next_char() {
                    '=' => (TokenType::CmpGte, String::from(">=")),
                    _ => {
                        self.current_char -= 1;
                        (TokenType::CmpGt, String::from(c))
                    }
                }
            }
            '-' => {
                match self.next_char() {
                    '>' => (TokenType::Arrow, String::from("->")),
                    _ => {
                        self.current_char -= 1;
                        (TokenType::Minus, String::from("-"))
                    }
                }
            }
            '/' => {
                match self.next_char() {
                    '/' => {
                        let v = fill_buffer!(c, 
                            |c: char| { c == '\r' || c == '\n' || c == '\0' }
                        );
                        return self.next_token();
                    }
                    _ => {
                        self.current_char -= 1;
                        (TokenType::ForwardSlash, String::from("/"))
                    }
                }
            }
            '(' => (TokenType::OpenRound, String::from(c)),
            ')' => (TokenType::ClosingRound, String::from(c)),
            '{' => (TokenType::OpenCurly, String::from(c)),
            '}' => (TokenType::ClosingCurly, String::from(c)),
            '[' => (TokenType::OpenSquare, String::from(c)),
            ']' => (TokenType::ClosingSquare, String::from(c)),
            ';' => (TokenType::Semi, String::from(c)),
            ':' => (TokenType::Colon, String::from(c)),
            ',' => (TokenType::Comma, String::from(c)),
            '.' => (TokenType::Dot, String::from(c)),
            '+' => (TokenType::Plus, String::from(c)),
            '*' => (TokenType::Asterisk, String::from(c)),
            '\0'=> (TokenType::Eof, String::new()),
            e => return Err(format!(
                "{}: {:?}: Unexpected Symbol `{}`",
                ERR_STR,
                self.get_location(),
                e
            )),
        };
        Ok(Token::new()
            .location(loc)
            .token_type(typ)
            .value(value))
    }

    fn get_location(&self) -> Location {
        Location::new(self.filename.clone(), self.current_line, self.current_char - self.line_start)
    }

    fn current_location(&self) -> Location {
        debug_assert!(!self.lookahead.is_empty());
        self.lookahead[0].location.clone()
    }

    fn fill_lookup(&mut self) -> Result<(), String> {
        while self.lookahead.len() < LOOKAHEAD_LIMIT {
            let n = self.next_token()?;
            if self.print_debug {
                // println!("[DEBUG] Found {:?}", n);
            }
            self.lookahead.push_back(n);
        }
        Ok(())
    }
    // ---------- End of Lexer ----------
    // ---------- Start of Parser ----------
    pub fn parse_file(&mut self) -> Result<nodes::FileNode, String> {
        self.fill_lookup()?;
        nodes::FileNode::parse(self)
    }

    fn parse_type(&self, token: Token) -> Type {
        self.parse_type_str(token.location, token.value)
    }
    fn parse_type_str(&self, loc: Location, val: String) -> Type {
        match val.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "usize" => Type::Usize,
            // Reserved for future use
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Class(val)
        }
    }
    fn parse_type_literal(&self, lit_tkn: Token) -> Result<(String, Type), String> {
        let lit = lit_tkn.value;
        let loc = lit_tkn.location;
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ_str = String::from(&lit[index..]);
                let typ = self.parse_type_str(loc, typ_str);
                Ok((lit[0..index].to_owned(), typ))
            }
            None => Ok((lit, Type::Unknown)),
        }
    }


    fn parsed_eof(&self) -> bool {
        self.lookahead[0].token_type == TokenType::Eof
    }

    fn eat(&mut self, token_type: TokenType) -> bool {
        if self.at(token_type) {
            self.next();
            true
        } else {
            false
        }
    }

    fn at(&self, token_type: TokenType) -> bool {
        self.nth(0) == token_type
    }

    fn nth(&self, lookahead: usize) -> TokenType {
        debug_assert!(self.lookahead.len() >= lookahead);
        self.lookahead[lookahead].token_type
    }

    fn next(&mut self) -> Result<Token, String> {
        self.fill_lookup()?;
        debug_assert!(!self.lookahead.is_empty());
        Ok(self.lookahead.pop_front().unwrap())
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token, String> {
        let n = self.next()?;
        if n.token_type != token_type {
            Err(format!(
                "{}: {:?}: Expected {:?}, found {:?}",
                ERR_STR,
                n.location,
                token_type,
                n.token_type
            ))
        } else {
            Ok(n)
        }
    }
    // ---------- End of Parser ----------
}

pub trait Parsable {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized;
}

impl Parsable for nodes::FileNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> {
        let location = parser.current_location();
        let mut classes = vec![];
        let mut functions = vec![];
        while !parser.parsed_eof() {
            match parser.nth(0) {
                TokenType::ClassKeyword => {
                    let parsed_class = nodes::ClassNode::parse(parser)?;
                    classes.push(parsed_class);
                },
                TokenType::FunctionKeyword => {
                    let parsed_function = nodes::FunctionNode::parse(parser)?;
                    functions.push(parsed_function);
                },
                _ => {
                    let tkn = parser.next()?;
                    return Err(format!(
                        "{}: {:?}: Expected one of {{Function, Class}}, found {:?}",
                        ERR_STR,
                        tkn.location,
                        tkn.token_type
                    ));
                }
            }
        }
        Ok(nodes::FileNode {
            location,
            filepath: parser.filepath.file_stem().unwrap().to_str().unwrap().to_string(),
            functions,
            classes
        })
    }
}

impl Parsable for nodes::ClassNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::ClassKeyword)?;
        
        let class_name = parser.expect(TokenType::Identifier)?;
        let name = class_name.value;
        parser.current_class = name.clone();

        parser.expect(TokenType::OpenCurly)?;

        let mut fields = vec![];
        let mut methods = vec![];
        let mut features = vec![];
        let mut has_constructor = false;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingCurly) {
            match parser.nth(0) {
                TokenType::Identifier => {
                    let parsed_field = nodes::FieldNode::parse(parser)?;
                    fields.push(parsed_field);
                }
                TokenType::FeatureKeyword => {
                    let parsed_feature = nodes::FeatureNode::parse(parser)?;
                    has_constructor |= parsed_feature.is_constructor;
                    features.push(parsed_feature);
                }
                TokenType::FunctionKeyword => {
                    let parsed_function = nodes::MethodNode::parse(parser)?;
                    methods.push(parsed_function);
                }
                e => todo!("{:?}", e)
            }
        }
        parser.expect(TokenType::ClosingCurly)?;
        parser.current_class.clear();
        Ok(nodes::ClassNode {
            location,
            name,
            fields,
            methods,
            features,
            has_constructor
        })
    }
}

impl Parsable for nodes::FieldNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        let type_def = nodes::TypeNode::parse(parser)?;
        parser.expect(TokenType::Semi);
        Ok(nodes::FieldNode {
            location,
            name,
            type_def
        })
    }
}

impl Parsable for nodes::FieldAccess {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}

impl Parsable for nodes::FeatureNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        
        parser.expect(TokenType::FeatureKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        
        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        
        let return_type = nodes::ReturnTypeNode::parse(parser)?;

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::FeatureNode {
            is_constructor: name == String::from("new"),
            location,
            name,
            return_type,
            parameters,
            block,
        })
    }
}

impl Parsable for nodes::FunctionNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        
        parser.expect(TokenType::FunctionKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        
        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        
        let return_type = nodes::ReturnTypeNode::parse(parser)?;

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::FunctionNode {
            location,
            name,
            return_type,
            parameters,
            block,
        })
    }
}

impl Parsable for nodes::MethodNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        debug_assert!(!parser.current_class.is_empty());
        let location = parser.current_location();
        
        parser.expect(TokenType::FunctionKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        
        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        
        let return_type = nodes::ReturnTypeNode::parse(parser)?;

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::MethodNode {
            location,
            class_name: parser.current_class.clone(),
            name,
            return_type,
            parameters,
            block,
        })
    }
}

impl Parsable for nodes::ReturnTypeNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let typ = if parser.eat(TokenType::Arrow) {
            let name_token = parser.expect(TokenType::Identifier)?;
            let typ = parser.parse_type(name_token);
            if parser.at(TokenType::OpenSquare) {
                todo!()
                // let size = ExpressionArrayLiteralNode::parse(parser)?;
                // Ok(TypeNodeBuilder::default()
                //     .location(location)
                //     .name(name)
                //     .typ(typ)
                //     .build()
                //     .unwrap())
            } else {
                typ
            }
        } else {
            Type::None
        };
        Ok(nodes::ReturnTypeNode {
            location,
            typ
        })
    }
}

impl Parsable for nodes::ParameterNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        let typ = nodes::TypeNode::parse(parser)?;
        Ok(nodes::ParameterNode {
            location,
            name,
            typ
        })
    }
}

impl Parsable for nodes::BlockNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();

        parser.expect(TokenType::OpenCurly)?;
        let mut statements = vec![];
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingCurly) {
            let parsed_statement = nodes::Statement::parse(parser)?;
            statements.push(parsed_statement);
        }
        parser.expect(TokenType::ClosingCurly)?;
        Ok(nodes::BlockNode {
            location,
            statements
        })
    }
}

impl Parsable for nodes::Statement {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        Ok(match parser.nth(0) {
            TokenType::LetKeyword =>
                nodes::Statement::Let(nodes::LetNode::parse(parser)?),
            TokenType::IfKeyword =>
                nodes::Statement::If(nodes::IfNode::parse(parser)?),
            TokenType::ReturnKeyword =>
                nodes::Statement::Return(nodes::ReturnNode::parse(parser)?),
            TokenType::Identifier => match parser.nth(1) {
                TokenType::Dot | TokenType::Equal | TokenType::OpenSquare =>
                    nodes::Statement::Assign(nodes::AssignNode::parse(parser)?),
                _ => {
                    let expr = nodes::Statement::Expression(nodes::ExpressionNode::parse(parser)?);
                    parser.expect(TokenType::Semi)?;
                    expr
                }
            }
            _ => {
                let expr = nodes::Statement::Expression(nodes::ExpressionNode::parse(parser)?);
                parser.expect(TokenType::Semi)?;
                expr
            }
        })
    }
}

impl Parsable for nodes::ExpressionNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = nodes::Expression::parse(parser)?;
        Ok(nodes::ExpressionNode {
            location,
            expression
        })
    }
}

impl Parsable for nodes::LetNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::LetKeyword)?;
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        let typ = nodes::TypeNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = nodes::ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;

        Ok(nodes::LetNode {
            location,
            name,
            typ,
            expression
        })
    }
}

impl Parsable for nodes::AssignNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name = nodes::ExpressionIdentifierNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = nodes::ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;
        Ok(nodes::AssignNode {
            location,
            name,
            expression
        })
    }
}

impl Parsable for nodes::ExpressionIdentifierNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = match parser.nth(1) {
            TokenType::OpenRound =>
                nodes::Expression::FunctionCall(nodes::ExpressionCallNode::parse(parser)?),
            TokenType::OpenSquare =>
                todo!(),
            TokenType::Dot =>
                nodes::Expression::FieldAccess(nodes::ExpressionFieldAccessNode::parse(parser)?),
            _ => nodes::Expression::Name(nodes::NameNode::parse(parser)?)
        };
        Ok(nodes::ExpressionIdentifierNode {
            location,
            expression: Box::new(expression),
            typ: Type::Unknown
        })
    }
}

impl Parsable for nodes::IfNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::IfKeyword)?;
        parser.expect(TokenType::OpenRound)?;
        let nodes::Expression::Binary(condition) = nodes::Expression::parse(parser)? else { todo!() };
        parser.expect(TokenType::ClosingRound)?;
        let if_branch = nodes::BlockNode::parse(parser)?;
        let else_branch = if parser.eat(TokenType::ElseKeyword) {
            Some(nodes::BlockNode::parse(parser)?)
        } else {
            None
        };
        Ok(nodes::IfNode {
            location,
            condition,
            if_branch,
            else_branch
        })
    }
}

impl Parsable for nodes::ReturnNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::ReturnKeyword)?;
        let return_value = if !parser.at(TokenType::Semi) {
            Some(nodes::ExpressionNode::parse(parser)?)
        } else {
            None
        };
        parser.expect(TokenType::Semi)?;
        Ok(nodes::ReturnNode {
            location,
            return_value
        })
    }
}

impl Parsable for nodes::TypeNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        parser.expect(TokenType::Colon)?;
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let typ = parser.parse_type(name_token);
        let typ = if parser.eat(TokenType::OpenSquare) {
            let mut dimensions = vec![];
            while !parser.parsed_eof() && !parser.at(TokenType::ClosingSquare) {
                let size = parser.expect(TokenType::IntLiteral)?;
                let loc = size.location.clone();
                let (value, typ) = parser.parse_type_literal(size)?;
                if typ != Type::Unknown && typ != Type::Usize {
                    return Err(format!(
                        "{}: {:?}: Unexpected type for integer literal. Expected Usize, found `{}`.",
                        ERR_STR,
                        loc,
                        typ
                    ))
                }
                let value = match value.parse() {
                    Ok(v) => v,
                    Err(e) => return Err(format!(
                        "{}: {:?}: Error when parsing number as Usize: {e}",
                        ERR_STR,
                        loc
                    ))
                };
                dimensions.push(value);
                if !parser.eat(TokenType::Comma) {
                    break;
                }
            }
            if dimensions.is_empty() {
                return Err(format!(
                    "{}: {:?}: Expected size for array type, found ClosingSquare.",
                    ERR_STR,
                    parser.current_location()
                ));
            }
            parser.expect(TokenType::ClosingSquare)?;
            Type::Arr(Box::new(typ), dimensions)
        } else {
            typ
        };
        Ok(nodes::TypeNode {
            location,
            typ
        })
    }
}

impl Parsable for nodes::ArgumentNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = nodes::ExpressionNode::parse(parser)?;
        Ok(nodes::ArgumentNode {
            location,
            expression
        })
    }
}

impl Parsable for nodes::Expression {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        Self::parse_rec(parser, TokenType::Eof)
    }
}

impl nodes::Expression {
    fn parse_delim(parser: &mut Parser) -> Result<Self, String> {
        Ok(match parser.nth(0) {
            TokenType::IntLiteral => {
                let expression = nodes::ExpressionLiteralNode::parse(parser)?;
                Self::Literal(expression)
            }
            TokenType::Identifier => {
                let expression = nodes::ExpressionIdentifierNode::parse(parser)?;
                Self::Identifier(expression)
            }
            TokenType::OpenRound => {
                parser.expect(TokenType::OpenRound)?;
                let expression = Self::parse(parser)?;
                parser.expect(TokenType::ClosingRound)?;
                expression
            }
            TokenType::OpenSquare => {
                let expression = nodes::ExpressionArrayLiteralNode::parse(parser)?;
                Self::ArrayLiteral(expression)
            }
            TokenType::BuiltInFunction => {
                let expression = nodes::ExpressionBuiltInNode::parse(parser)?;
                Self::BuiltIn(expression)
            }            
            e => {
                return Err(format!(
                    "{}: {:?}: Expected Expr, found {:?}",
                    ERR_STR,
                    parser.current_location(),
                    e
                ));
            }
            e => todo!("{:?}", e)
        })
    }
    fn parse_rec(parser: &mut Parser, left: TokenType) -> Result<Self, String> {
        let mut lhs = Self::parse_delim(parser)?;
        loop {
            let right = parser.nth(0);
            if Self::right_binds_tighter(left, right) {
                let token = parser.next()?;
                let rhs = Self::parse_rec(parser, right)?;
                lhs = nodes::Expression::Binary(
                    nodes::ExpressionBinaryNode {
                        location: token.location,
                        operation: Operation::from(token.value),
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        typ: Type::Unknown
                    });
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn right_binds_tighter(left: TokenType, right: TokenType) -> bool {
        fn tightness(typ: TokenType) -> Option<usize> {
            [
                &COMPARATOR_TYPES,
                [TokenType::Plus, TokenType::Minus].as_slice(),
                &[TokenType::Asterisk, TokenType::ForwardSlash],
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
}

impl Parsable for nodes::ExpressionBinaryNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}

impl Parsable for nodes::ExpressionBuiltInNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::BuiltInFunction)?;
        let function_name = name_token.value;

        let mut arguments = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let arg = nodes::ArgumentNode::parse(parser)?;
            arguments.push(arg);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        Ok(nodes::ExpressionBuiltInNode {
            location,
            function_name,
            arguments,
            typ: Type::Unknown
        })
    }
}

impl Parsable for nodes::ExpressionArrayLiteralNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let mut elements = vec![];
        parser.expect(TokenType::OpenSquare)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingSquare) {
            let elem = nodes::Expression::parse(parser)?;
            elements.push(elem);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingSquare)?;
        Ok(nodes::ExpressionArrayLiteralNode {
            location,
            elements
        })
    }
}

impl Parsable for nodes::ExpressionLiteralNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let number_token = parser.expect(TokenType::IntLiteral)?;
        let (value, typ) = parser.parse_type_literal(number_token)?;
        Ok(nodes::ExpressionLiteralNode {
            location,
            value,
            typ
        })
    }
}

impl Parsable for nodes::ExpressionCallNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let function_name = name_token.value;

        let mut arguments = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let arg = nodes::ArgumentNode::parse(parser)?;
            arguments.push(arg);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        Ok(nodes::ExpressionCallNode {
            function_name,
            location,
            arguments,
            typ: Type::Unknown
        })
    }
}

impl Parsable for nodes::ExpressionFieldAccessNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        parser.expect(TokenType::Dot)?;
        let field = nodes::ExpressionIdentifierNode::parse(parser)?;
        Ok(nodes::ExpressionFieldAccessNode {
            location,
            name,
            field,
            typ: Type::Unknown
        })
    }
}

impl Parsable for nodes::NameNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        
        let name = name_token.value;
        Ok(nodes::NameNode {
            location,
            name,
            typ: Type::Unknown
        })
    }
}
