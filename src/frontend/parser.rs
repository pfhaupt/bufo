use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::path::PathBuf;

use super::flags::Flags;
use super::nodes;
use crate::compiler::{BUILT_IN_FEATURES, CONSTRUCTOR_NAME, ERR_STR, NOTE_STR, WARN_STR};
use crate::middleend::type_checker::Type;

const LOOKAHEAD_LIMIT: usize = 3;

use tracer::trace_call;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    CharLiteral,
    StrLiteral,
    IntLiteral,
    OpenRound,
    ClosingRound,
    OpenCurly,
    ClosingCurly,
    OpenSquare,
    ClosingSquare,
    ClassKeyword,
    FunctionKeyword,
    FeatureKeyword,
    LetKeyword,
    IfKeyword,
    ElseKeyword,
    ReturnKeyword,
    WhileKeyword,
    BreakKeyword,
    ContinueKeyword,
    BuiltInFunction,
    Colon,
    Semi,
    Comma,
    Dot,
    Arrow,
    Equal,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpLte,
    CmpGt,
    CmpGte,
    Identifier,
    Eof,
}

pub const COMPARATOR_TYPES: [TokenType; 6] = [
    TokenType::CmpEq,
    TokenType::CmpNeq,
    TokenType::CmpGt,
    TokenType::CmpGte,
    TokenType::CmpLt,
    TokenType::CmpLte,
];
pub const BUILT_IN_FUNCTIONS: [&str; 3] = ["EXIT", "MALLOC", "SIZEOF"];

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Location {
    file: String,
    row: usize,
    col: usize,
}

impl Location {
    #[trace_call(extra)]
    pub fn new(file: String, row: usize, col: usize) -> Self {
        Self { file, row, col }
    }

    #[trace_call(extra)]
    pub fn anonymous() -> Self {
        Self::new(String::from("anonymous"), 0, 0)
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    location: Location,
    value: String,
    token_type: TokenType,
}

impl Token {
    #[trace_call(extra)]
    fn new() -> Self {
        Self {
            location: Location::anonymous(),
            value: String::new(),
            token_type: TokenType::Eof,
        }
    }
    #[trace_call(extra)]
    fn location(self, location: Location) -> Self {
        Self { location, ..self }
    }
    #[trace_call(extra)]
    fn value(self, value: String) -> Self {
        Self { value, ..self }
    }
    #[trace_call(extra)]
    fn token_type(self, token_type: TokenType) -> Self {
        Self { token_type, ..self }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

const COMPARISONS: [Operation; 6] = [
    Operation::Eq,
    Operation::Neq,
    Operation::Lt,
    Operation::Lte,
    Operation::Gt,
    Operation::Gte,
];

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_assert_eq!(Operation::Gte as u8 + 1, 10);
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
        }
    }
}

impl Operation {
    #[trace_call(extra)]
    fn from(s: String) -> Self {
        debug_assert_eq!(Operation::Gte as u8 + 1, 10);
        match s.as_str() {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            "<" => Self::Lt,
            "<=" => Self::Lte,
            ">" => Self::Gt,
            ">=" => Self::Gte,
            _ => unreachable!(),
        }
    }
}

#[derive(Default)]
pub struct Parser {
    filepath: PathBuf,
    filename: String,
    source: Vec<char>,
    lookahead: VecDeque<Token>,
    current_class: String,
    current_char: usize,
    current_line: usize,
    line_start: usize,
    flags: Flags,
}

impl Parser {
    // ---------- Start of Builder Pattern ----------
    pub fn new(flags: Flags) -> Self {
        Self {
            current_line: 1,
            flags: flags.clone(),
            ..Default::default()
        }
        .filepath(&flags.input)
    }

    pub fn filepath(self, filepath: &str) -> Self {
        let pb = PathBuf::from(filepath);
        let source = match fs::read_to_string(filepath) {
            Ok(source) => source.chars().collect(),
            // Error Handling is done by the Flags struct
            // At this point, the file is guaranteed to exist
            Err(_) => unreachable!(),
        };
        Self {
            filepath: pb.clone(),
            filename: pb.into_os_string().into_string().unwrap(),
            source,
            ..self
        }
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

    #[trace_call(extra)]
    fn next_token(&mut self) -> Result<Token, String> {
        macro_rules! fill_buffer {
            ($start: expr, $cond: expr, $stepback: expr) => {{
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
            }};
            ($start: expr, $cond: expr) => {
                fill_buffer!($start, $cond, true)
            };
        }
        debug_assert_eq!(
            TokenType::Eof as u8 + 1,
            38,
            "Not all TokenTypes are handled in next_token()"
        );
        self.trim_whitespace()?;
        let c = self.next_char();
        let loc = self.get_location();
        let (typ, value) = match c {
            '0'..='9' => {
                let value = fill_buffer!(c, |c: char| { !c.is_alphanumeric() || c == '\0' });
                (TokenType::IntLiteral, value)
            }
            'A'..='Z' | 'a'..='z' => {
                let value = fill_buffer!(c, |c: char| {
                    (!c.is_alphanumeric() && c != '_') || c == '\0'
                });
                let typ = match value.as_str() {
                    "class" => TokenType::ClassKeyword,
                    "func" => TokenType::FunctionKeyword,
                    "feat" => TokenType::FeatureKeyword,
                    "let" => TokenType::LetKeyword,
                    "if" => TokenType::IfKeyword,
                    "else" => TokenType::ElseKeyword,
                    "return" => TokenType::ReturnKeyword,
                    "while" => TokenType::WhileKeyword,
                    "break" => TokenType::BreakKeyword,
                    "continue" => TokenType::ContinueKeyword,
                    _ if BUILT_IN_FUNCTIONS.contains(&value.as_str()) => TokenType::BuiltInFunction,
                    _ => TokenType::Identifier,
                };
                (typ, value)
            }
            '\"' => {
                let value = fill_buffer!(c, |c: char| { c == '"' || c == '\0' }, false);
                if value.contains('\\') {
                    println!(
                        "{}: {:?}: Escape sequences in Strings are not supported yet.",
                        WARN_STR, loc
                    );
                }
                if value.chars().filter(|c| *c == '"').count() != 2 {
                    return Err(format!(
                        "{}: {:?}: Unterminated String Literal.",
                        ERR_STR, loc
                    ));
                }
                (TokenType::StrLiteral, value)
            }
            '\'' => {
                let value = fill_buffer!(c, |c: char| { c == '\'' || c == '\0' }, false);
                if value.contains('\\') {
                    println!(
                        "{}: {:?}: Escape sequences in Char Literals are not supported yet.",
                        WARN_STR, loc
                    );
                }
                if value.len() != 3 {
                    return Err(format!(
                        "{}: {:?}: Char Literal is expected to be a single char, found {}.",
                        ERR_STR, loc, value,
                    ));
                }
                (TokenType::CharLiteral, value)
            }
            '!' => match self.next_char() {
                '=' => (TokenType::CmpNeq, String::from("!=")),
                _ => return Err(format!("{}: {:?}: Unexpected Symbol `{}`", ERR_STR, loc, c)),
            },
            '=' => match self.next_char() {
                '=' => (TokenType::CmpEq, String::from("==")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Equal, String::from(c))
                }
            },
            '<' => match self.next_char() {
                '=' => (TokenType::CmpLte, String::from("<=")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::CmpLt, String::from(c))
                }
            },
            '>' => match self.next_char() {
                '=' => (TokenType::CmpGte, String::from(">=")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::CmpGt, String::from(c))
                }
            },
            '-' => match self.next_char() {
                '>' => (TokenType::Arrow, String::from("->")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Minus, String::from("-"))
                }
            },
            '/' => match self.next_char() {
                '/' => {
                    let _ = fill_buffer!(c, |c: char| { c == '\r' || c == '\n' || c == '\0' });
                    return self.next_token();
                }
                _ => {
                    self.current_char -= 1;
                    (TokenType::ForwardSlash, String::from("/"))
                }
            },
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
            '\0' => (TokenType::Eof, String::new()),
            e => {
                return Err(format!(
                    "{}: {:?}: Unexpected Symbol `{}`",
                    ERR_STR,
                    self.get_location(),
                    e
                ))
            }
        };
        Ok(Token::new().location(loc).token_type(typ).value(value))
    }

    #[trace_call(extra)]
    fn get_location(&self) -> Location {
        Location::new(
            self.filename.clone(),
            self.current_line,
            self.current_char - self.line_start,
        )
    }

    #[trace_call(extra)]
    fn current_location(&self) -> Location {
        debug_assert!(!self.lookahead.is_empty());
        self.lookahead[0].location.clone()
    }

    #[trace_call(extra)]
    fn fill_lookup(&mut self) -> Result<(), String> {
        while self.lookahead.len() < LOOKAHEAD_LIMIT {
            let n = self.next_token()?;
            if self.flags.debug {
                // println!("[DEBUG] Found {:?}", n);
            }
            self.lookahead.push_back(n);
        }
        Ok(())
    }
    // ---------- End of Lexer ----------
    // ---------- Start of Parser ----------
    #[trace_call(always)]
    pub fn parse_file(&mut self) -> Result<nodes::FileNode, String> {
        self.fill_lookup()?;
        nodes::FileNode::parse(self)
    }

    #[trace_call(always)]
    fn parse_type(&self, token: Token) -> Type {
        self.parse_type_str(token.location, token.value)
    }
    #[trace_call(always)]
    fn parse_type_str(&self, _loc: Location, val: String) -> Type {
        match val.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "usize" => Type::Usize,
            // Reserved for future use
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Class(val),
        }
    }
    #[trace_call(always)]
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

    #[trace_call(extra)]
    fn parsed_eof(&self) -> bool {
        self.lookahead[0].token_type == TokenType::Eof
    }

    #[trace_call(extra)]
    fn eat(&mut self, token_type: TokenType) -> Result<bool, String> {
        if self.at(token_type) {
            self.next()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[trace_call(extra)]
    fn at(&self, token_type: TokenType) -> bool {
        self.nth(0) == token_type
    }

    #[trace_call(extra)]
    fn peek(&self, lookahead: usize) -> Token {
        self.lookahead[lookahead].clone()
    }

    #[trace_call(extra)]
    fn nth(&self, lookahead: usize) -> TokenType {
        debug_assert!(self.lookahead.len() >= lookahead);
        self.lookahead[lookahead].token_type
    }

    #[trace_call(extra)]
    fn next(&mut self) -> Result<Token, String> {
        self.fill_lookup()?;
        debug_assert!(!self.lookahead.is_empty());
        Ok(self.lookahead.pop_front().unwrap())
    }

    #[trace_call(extra)]
    fn expect(&mut self, token_type: TokenType) -> Result<Token, String> {
        let n = self.next()?;
        if n.token_type != token_type {
            Err(format!(
                "{}: {:?}: Expected {:?}, found {:?}",
                ERR_STR, n.location, token_type, n.token_type
            ))
        } else {
            Ok(n)
        }
    }
    // ---------- End of Parser ----------
}

pub trait Parsable {
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized;
}

impl Parsable for nodes::FileNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String> {
        let location = parser.current_location();
        let mut classes = vec![];
        let mut functions = vec![];
        while !parser.parsed_eof() {
            match parser.nth(0) {
                TokenType::ClassKeyword => {
                    let parsed_class = nodes::ClassNode::parse(parser)?;
                    classes.push(parsed_class);
                }
                TokenType::FunctionKeyword => {
                    let parsed_function = nodes::FunctionNode::parse(parser)?;
                    functions.push(parsed_function);
                }
                _ => {
                    let tkn = parser.next()?;
                    return Err(format!(
                        "{}: {:?}: Expected one of {{Function, Class}}, found {:?}",
                        ERR_STR, tkn.location, tkn.token_type
                    ));
                }
            }
        }
        Ok(nodes::FileNode {
            location,
            filepath: parser
                .filepath
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
            functions,
            classes,
        })
    }
}

impl Parsable for nodes::ClassNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
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
                e => return Err(format!(
                    "{}: {:?}: Unexpected Token. Expected one of (Field, Feature, Method), found {:?}.",
                    ERR_STR,
                    parser.current_location(),
                    e
                )),
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
            has_constructor,
        })
    }
}

impl Parsable for nodes::FieldNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        parser.expect(TokenType::Colon)?;
        let type_def = nodes::TypeNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;
        Ok(nodes::FieldNode {
            location,
            name,
            type_def,
        })
    }
}

impl Parsable for nodes::FeatureNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        debug_assert!(!parser.current_class.is_empty());
        let location = parser.current_location();

        parser.expect(TokenType::FeatureKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        if name.as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Feature names are not allowed to be capitalized.",
                ERR_STR, feature_name.location
            ));
        }

        if !BUILT_IN_FEATURES.contains(&name.as_str()) {
            return Err(format!(
                "{}: {:?}: Unknown feature `{}`.\n{}: This is a list of all features: {:?}",
                ERR_STR, feature_name.location, name, NOTE_STR, BUILT_IN_FEATURES
            ));
        }

        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;

        let return_type = if parser.eat(TokenType::Arrow)? {
            nodes::TypeNode::parse(parser)?
        } else {
            nodes::TypeNode::none(parser.current_location())
        };

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::FeatureNode {
            is_constructor: name == *CONSTRUCTOR_NAME,
            class_name: parser.current_class.clone(),
            location,
            name,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }
}

impl Parsable for nodes::FunctionNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();

        parser.expect(TokenType::FunctionKeyword)?;
        let function_name = parser.expect(TokenType::Identifier)?;
        let name = function_name.value;
        if name.as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Function names are not allowed to be capitalized.",
                ERR_STR, function_name.location
            ));
        }

        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;

        let return_type = if parser.eat(TokenType::Arrow)? {
            nodes::TypeNode::parse(parser)?
        } else {
            nodes::TypeNode::none(parser.current_location())
        };

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::FunctionNode {
            location,
            name,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }
}

impl Parsable for nodes::MethodNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        debug_assert!(!parser.current_class.is_empty());
        let location = parser.current_location();

        parser.expect(TokenType::FunctionKeyword)?;
        let method_name = parser.expect(TokenType::Identifier)?;
        let name = method_name.value;
        if name.as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Method names are not allowed to be capitalized.",
                ERR_STR, method_name.location
            ));
        }

        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = nodes::ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;

        let return_type = if parser.eat(TokenType::Arrow)? {
            nodes::TypeNode::parse(parser)?
        } else {
            nodes::TypeNode::none(parser.current_location())
        };

        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::MethodNode {
            location,
            class_name: parser.current_class.clone(),
            name,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }
}

impl Parsable for nodes::ParameterNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        parser.expect(TokenType::Colon)?;
        let typ = nodes::TypeNode::parse(parser)?;
        Ok(nodes::ParameterNode {
            location,
            name,
            typ,
        })
    }
}

impl Parsable for nodes::BlockNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
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
            statements,
        })
    }
}

impl Parsable for nodes::Statement {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        Ok(match parser.nth(0) {
            TokenType::LetKeyword => nodes::Statement::Let(nodes::LetNode::parse(parser)?),
            TokenType::IfKeyword => nodes::Statement::If(nodes::IfNode::parse(parser)?),
            TokenType::ReturnKeyword => nodes::Statement::Return(nodes::ReturnNode::parse(parser)?),
            TokenType::WhileKeyword => nodes::Statement::While(nodes::WhileNode::parse(parser)?),
            TokenType::BreakKeyword => nodes::Statement::Break(nodes::BreakNode::parse(parser)?),
            TokenType::ContinueKeyword => nodes::Statement::Continue(nodes::ContinueNode::parse(parser)?),
            TokenType::Identifier => match parser.nth(1) {
                // FIXME: Simple void function calls are not handled correctly
                //        Currently, they are parsed as assignments
                TokenType::Dot | TokenType::Equal | TokenType::OpenSquare => {
                    nodes::Statement::Assign(nodes::AssignNode::parse(parser)?)
                }
                _ => {
                    let expr = nodes::Statement::Expression(nodes::ExpressionNode::parse(parser)?);
                    parser.expect(TokenType::Semi)?;
                    expr
                }
            },
            _ => {
                let expr = nodes::Statement::Expression(nodes::ExpressionNode::parse(parser)?);
                parser.expect(TokenType::Semi)?;
                expr
            }
        })
    }
}

impl Parsable for nodes::ExpressionNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let expression = nodes::Expression::parse(parser)?;
        Ok(nodes::ExpressionNode {
            location,
            expression,
        })
    }
}

impl Parsable for nodes::LetNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        parser.expect(TokenType::LetKeyword)?;
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        parser.expect(TokenType::Colon)?;
        let typ = nodes::TypeNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = nodes::ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;

        Ok(nodes::LetNode {
            location,
            name,
            typ,
            expression,
        })
    }
}

impl Parsable for nodes::AssignNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name = nodes::IdentifierNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = nodes::ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;
        Ok(nodes::AssignNode {
            location,
            name,
            expression,
        })
    }
}

impl Parsable for nodes::IdentifierNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let expression = match parser.nth(1) {
            TokenType::OpenRound => {
                nodes::Expression::FunctionCall(nodes::CallNode::parse(parser)?)
            }
            TokenType::OpenSquare => {
                nodes::Expression::ArrayAccess(nodes::ArrayAccessNode::parse(parser)?)
            }
            TokenType::Dot => {
                nodes::Expression::FieldAccess(nodes::FieldAccessNode::parse(parser)?)
            }
            _ => nodes::Expression::Name(nodes::NameNode::parse(parser)?),
        };
        Ok(nodes::IdentifierNode {
            location,
            expression: Box::new(expression),
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::IfNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        parser.expect(TokenType::IfKeyword)?;
        parser.expect(TokenType::OpenRound)?;
        let nodes::Expression::Comparison(condition) = nodes::Expression::parse(parser)? else {
            return Err(format!(
                "{}: {:?}: if-condition is expected to be a comparison.",
                ERR_STR,
                location
            ))
        };
        parser.expect(TokenType::ClosingRound)?;
        let if_branch = nodes::BlockNode::parse(parser)?;
        let else_branch = if parser.eat(TokenType::ElseKeyword)? {
            Some(nodes::BlockNode::parse(parser)?)
        } else {
            None
        };
        Ok(nodes::IfNode {
            location,
            condition,
            if_branch,
            else_branch,
        })
    }
}

impl Parsable for nodes::ReturnNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
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
            return_value,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::WhileNode {
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        parser.expect(TokenType::WhileKeyword)?;
        parser.expect(TokenType::OpenRound)?;
        let nodes::Expression::Comparison(condition) = nodes::Expression::parse(parser)? else {
            return Err(format!(
                "{}: {:?}: while-condition is expected to be a comparison.",
                ERR_STR,
                location
            ))
        };
        parser.expect(TokenType::ClosingRound)?;
        let block = nodes::BlockNode::parse(parser)?;
        Ok(nodes::WhileNode {
            location,
            condition,
            block,
        })
    }
}

impl Parsable for nodes::BreakNode {
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        parser.expect(TokenType::BreakKeyword)?;
        parser.expect(TokenType::Semi)?;
        Ok(nodes::BreakNode { location })
    }
}

impl Parsable for nodes::ContinueNode {
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        parser.expect(TokenType::ContinueKeyword)?;
        parser.expect(TokenType::Semi)?;
        Ok(nodes::ContinueNode { location })
    }
}

impl Parsable for nodes::TypeNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let typ = parser.parse_type(name_token);
        let typ = if parser.eat(TokenType::OpenSquare)? {
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
                    ));
                }
                let value = match value.parse() {
                    Ok(v) => v,
                    Err(e) => {
                        return Err(format!(
                            "{}: {:?}: Error when parsing number as Usize: {e}",
                            ERR_STR, loc
                        ))
                    }
                };
                dimensions.push(value);
                if !parser.eat(TokenType::Comma)? {
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
        Ok(nodes::TypeNode { location, typ })
    }
}

impl Parsable for nodes::ArgumentNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let expression = nodes::ExpressionNode::parse(parser)?;
        Ok(nodes::ArgumentNode {
            location,
            expression,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::Expression {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        Self::parse_rec(parser, TokenType::Eof)
    }
}

impl nodes::Expression {
    #[trace_call(always)]
    fn parse_delim(parser: &mut Parser) -> Result<Self, String> {
        Ok(match parser.nth(0) {
            TokenType::IntLiteral => {
                let expression = nodes::LiteralNode::parse(parser)?;
                Self::Literal(expression)
            }
            TokenType::Identifier => {
                let expression = nodes::IdentifierNode::parse(parser)?;
                Self::Identifier(expression)
            }
            TokenType::OpenRound => {
                parser.expect(TokenType::OpenRound)?;
                let expression = Self::parse(parser)?;
                parser.expect(TokenType::ClosingRound)?;
                expression
            }
            TokenType::OpenSquare => {
                let expression = nodes::ArrayLiteralNode::parse(parser)?;
                Self::ArrayLiteral(expression)
            }
            TokenType::BuiltInFunction => {
                let expression = nodes::BuiltInNode::parse(parser)?;
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
        })
    }
    #[trace_call(always)]
    fn parse_rec(parser: &mut Parser, left: TokenType) -> Result<Self, String> {
        let mut lhs = Self::parse_delim(parser)?;
        loop {
            let right = parser.nth(0);
            if Self::right_binds_tighter(left, right) {
                let token = parser.next()?;
                let rhs = Self::parse_rec(parser, right)?;
                let operation = Operation::from(token.value);
                lhs = if COMPARISONS.contains(&operation) {
                    nodes::Expression::Comparison(nodes::ComparisonNode {
                        location: token.location,
                        operation,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        typ: Type::Bool,
                    })
                } else {
                    nodes::Expression::Binary(nodes::BinaryNode {
                        location: token.location,
                        operation,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        typ: Type::Unknown,
                    })
                }
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    #[trace_call(always)]
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

impl Parsable for nodes::BinaryNode {
    #[trace_call(always)]
    fn parse(_parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        Err(format!(
            "INTERNAL ERROR AT {}:{}:{}: Not implemented yet.",
            file!(),
            line!(),
            column!()
        ))
    }
}

impl Parsable for nodes::BuiltInNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::BuiltInFunction)?;
        let function_name = name_token.value;

        let mut arguments = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let arg = nodes::ArgumentNode::parse(parser)?;
            arguments.push(arg);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        Ok(nodes::BuiltInNode {
            location,
            function_name,
            arguments,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::ArrayLiteralNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let mut elements = vec![];
        parser.expect(TokenType::OpenSquare)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingSquare) {
            let elem = nodes::Expression::parse(parser)?;
            elements.push(elem);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingSquare)?;
        Ok(nodes::ArrayLiteralNode {
            location,
            elements,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::ArrayAccessNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let array_name = parser.expect(TokenType::Identifier)?;
        let array_name = array_name.value;
        let indices = nodes::ArrayLiteralNode::parse(parser)?;
        Ok(nodes::ArrayAccessNode {
            location,
            array_name,
            indices,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::LiteralNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let number_token = parser.expect(TokenType::IntLiteral)?;
        let (value, typ) = parser.parse_type_literal(number_token)?;
        Ok(nodes::LiteralNode {
            location,
            value,
            typ,
        })
    }
}

impl Parsable for nodes::CallNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let function_name = name_token.value;

        let mut arguments = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let arg = nodes::ArgumentNode::parse(parser)?;
            arguments.push(arg);
            if !parser.eat(TokenType::Comma)? {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        Ok(nodes::CallNode {
            is_constructor: function_name.as_bytes()[0].is_ascii_uppercase(),
            function_name,
            location,
            arguments,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::FieldAccessNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        parser.expect(TokenType::Dot)?;
        let field = nodes::IdentifierNode::parse(parser)?;
        Ok(nodes::FieldAccessNode {
            location,
            name,
            field,
            typ: Type::Unknown,
        })
    }
}

impl Parsable for nodes::NameNode {
    #[trace_call(always)]
    fn parse(parser: &mut Parser) -> Result<Self, String>
    where
        Self: Sized,
    {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;

        let name = name_token.value;
        Ok(nodes::NameNode {
            location,
            name,
            typ: Type::Unknown,
        })
    }
}
