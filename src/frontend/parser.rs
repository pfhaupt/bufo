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
    // ---------- Start of Parser Utility ----------
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

    #[trace_call(extra)]
    fn expect_lowercase_ident(&mut self) -> Result<Token, String> {
        let token = self.expect(TokenType::Identifier)?;
        if token.value.as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Expected lowercase identifier, found {:?}",
                ERR_STR, token.location, token.value
            ));
        }
        Ok(token)
    }
    // ---------- End of Parser Utility ----------
    // ---------- Start of Parser ----------
    #[trace_call(always)]
    pub fn parse_file(&mut self) -> Result<nodes::FileNode, String> {
        self.fill_lookup()?;
        let location = self.current_location();
        let mut classes = vec![];
        let mut functions = vec![];
        while !self.parsed_eof() {
            match self.nth(0) {
                TokenType::ClassKeyword => {
                    let parsed_class = self.parse_class()?;
                    classes.push(parsed_class);
                }
                TokenType::FunctionKeyword => {
                    let parsed_function = self.parse_function()?;
                    functions.push(parsed_function);
                }
                _ => {
                    let tkn = self.next()?;
                    return Err(format!(
                        "{}: {:?}: Expected one of {{Function, Class}}, found {:?}",
                        ERR_STR, tkn.location, tkn.token_type
                    ));
                }
            }
        }
        Ok(nodes::FileNode {
            location,
            filepath: self
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

    #[trace_call(always)]
    fn parse_class(&mut self) -> Result<nodes::ClassNode, String> {
        let location = self.current_location();
        self.expect(TokenType::ClassKeyword)?;

        let class_name = self.expect(TokenType::Identifier)?;
        let name = class_name.value;
        if !name.as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Class names are expected to be capitalized.",
                ERR_STR, class_name.location
            ));
        }
        self.expect(TokenType::OpenCurly)?;
        let mut fields = vec![];
        let mut methods = vec![];
        let mut features = vec![];
        let mut has_constructor = false;
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            match self.nth(0) {
                TokenType::Identifier => {
                    let parsed_field = self.parse_field()?;
                    fields.push(parsed_field);
                }
                TokenType::FeatureKeyword => {
                    let parsed_feature = self.parse_feature(&name)?;
                    has_constructor |= parsed_feature.is_constructor;
                    features.push(parsed_feature);
                }
                TokenType::FunctionKeyword => {
                    let parsed_function = self.parse_method(&name)?;
                    methods.push(parsed_function);
                }
                e => return Err(format!(
                    "{}: {:?}: Unexpected Token. Expected one of (Field, Feature, Method), found {:?}.",
                    ERR_STR,
                    self.current_location(),
                    e
                )),
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(nodes::ClassNode {
            location,
            name,
            fields,
            methods,
            features,
            has_constructor,
        })
    }

    #[trace_call(always)]
    fn parse_field(&mut self) -> Result<nodes::FieldNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::Identifier)?;
        let name = name_token.value;
        self.expect(TokenType::Colon)?;
        let type_def = self.parse_type_node()?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::FieldNode {
            location,
            name,
            type_def,
        })
    }

    #[trace_call(always)]
    fn parse_feature(&mut self, class_name: &str) -> Result<nodes::FeatureNode, String> {
        let location = self.current_location();
        self.expect(TokenType::FeatureKeyword)?;

        let name_token = self.expect_lowercase_ident()?;
        let name = name_token.value;
        if !BUILT_IN_FEATURES.contains(&name.as_str()) {
            return Err(format!(
                "{}: {:?}: Unknown feature `{}`.\n{}: This is a list of all features: {:?}",
                ERR_STR, name_token.location, name, NOTE_STR, BUILT_IN_FEATURES
            ));
        }

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters()?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        let block = self.parse_block()?;
        Ok(nodes::FeatureNode {
            is_constructor: name == *CONSTRUCTOR_NAME,
            class_name: class_name.to_string(),
            location,
            name,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_function(&mut self) -> Result<nodes::FunctionNode, String> {
        let location = self.current_location();
        self.expect(TokenType::FunctionKeyword)?;

        let name = self.expect_lowercase_ident()?;

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters()?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;
        let block = self.parse_block()?;
        Ok(nodes::FunctionNode {
            location,
            name: name.value,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_method(&mut self, class_name: &str) -> Result<nodes::MethodNode, String> {
        let location = self.current_location();
        self.expect(TokenType::FunctionKeyword)?;

        let name = self.expect_lowercase_ident()?;

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters()?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;
        let block = self.parse_block()?;
        Ok(nodes::MethodNode {
            location,
            class_name: class_name.to_string(),
            name: name.value,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_return_type(&mut self) -> Result<nodes::TypeNode, String> {
        if self.eat(TokenType::Arrow)? {
            self.parse_type_node()
        } else {
            Ok(nodes::TypeNode::none(self.current_location()))
        }
    }

    #[trace_call(always)]
    fn parse_parameters(&mut self) -> Result<Vec<nodes::ParameterNode>, String> {
        let mut parameters = vec![];
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            let location = self.current_location();
            let name_token = self.expect(TokenType::Identifier)?;
            let name = name_token.value;
            self.expect(TokenType::Colon)?;
            let typ = self.parse_type_node()?;
            let param = nodes::ParameterNode {
                location,
                name,
                typ,
            };
            parameters.push(param);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        Ok(parameters)
    }

    #[trace_call(always)]
    fn parse_block(&mut self) -> Result<nodes::BlockNode, String> {
        let location = self.current_location();
        self.expect(TokenType::OpenCurly)?;
        let mut statements = vec![];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            let parsed_statement = self.parse_statement()?;
            statements.push(parsed_statement);
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(nodes::BlockNode {
            location,
            statements,
        })
    }

    #[trace_call(always)]
    fn parse_statement(&mut self) -> Result<nodes::Statement, String> {
        Ok(match self.nth(0) {
            TokenType::LetKeyword => 
                nodes::Statement::Let(self.parse_stmt_let()?),
            TokenType::IfKeyword => 
                nodes::Statement::If(self.parse_stmt_if()?),
            TokenType::ReturnKeyword =>
                nodes::Statement::Return(self.parse_stmt_return()?),
            TokenType::WhileKeyword => 
                nodes::Statement::While(self.parse_stmt_while()?),
            TokenType::BreakKeyword =>
                nodes::Statement::Break(self.parse_stmt_break()?),
            TokenType::ContinueKeyword =>
                nodes::Statement::Continue(self.parse_stmt_continue()?),
            TokenType::Identifier => match self.nth(1) {
                // FIXME: Simple void function calls are not handled correctly
                //        Currently, they are parsed as assignments
                TokenType::Dot | TokenType::Equal | TokenType::OpenSquare =>
                    nodes::Statement::Assign(self.parse_assignment()?),
                _ => {
                    let expr = self.parse_expression()?;
                    self.expect(TokenType::Semi)?;
                    nodes::Statement::Expression(expr)
                }
            }
            s => {
                eprintln!("FIXME: Attempted to parse {:?} as statement", s);
                eprintln!("       Proceeding to parse as expression!");
                let expr = self.parse_expression()?;
                self.expect(TokenType::Semi)?;
                nodes::Statement::Expression(expr)
            }
        })
    }

    #[trace_call(always)]
    fn parse_stmt_let(&mut self) -> Result<nodes::LetNode, String> {
        let location = self.current_location();
        self.expect(TokenType::LetKeyword)?;
        let name_token = self.expect(TokenType::Identifier)?;
        let name = name_token.value;

        self.expect(TokenType::Colon)?;
        let typ = self.parse_type_node()?;
        self.expect(TokenType::Equal)?;
        let expression = self.parse_expression()?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::LetNode{
            location,
            name,
            typ,
            expression,
        })
    }

    #[trace_call(always)]
    fn parse_assignment(&mut self) -> Result<nodes::AssignNode, String> {
        let location = self.current_location();
        let name = self.parse_expr_identifier()?;
        self.expect(TokenType::Equal)?;
        let expression = self.parse_expression()?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::AssignNode {
            location,
            name,
            expression,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_if(&mut self) -> Result<nodes::IfNode, String> {
        let location = self.current_location();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenRound)?;
        let nodes::Expression::Comparison(condition) = self.parse_expression_enum()? else {
            return Err(format!(
                "{}: {:?}: if-condition is expected to be a comparison.",
                ERR_STR,
                location
            ))
        };
        self.expect(TokenType::ClosingRound)?;
        let if_branch = self.parse_block()?;
        let else_branch = if self.eat(TokenType::ElseKeyword)? {
            Some(self.parse_block()?)
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

    #[trace_call(always)]
    fn parse_stmt_return(&mut self) -> Result<nodes::ReturnNode, String> {
        let location = self.current_location();
        self.expect(TokenType::ReturnKeyword)?;

        let return_value = if !self.at(TokenType::Semi) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(TokenType::Semi)?;
        Ok(nodes::ReturnNode {
            location,
            return_value,
            typ: Type::Unknown
        })
    }

    #[trace_call(always)]
    fn parse_stmt_while(&mut self) -> Result<nodes::WhileNode, String> {
        let location = self.current_location();
        self.expect(TokenType::WhileKeyword)?;

        self.expect(TokenType::OpenRound)?;
        let nodes::Expression::Comparison(condition) = self.parse_expression_enum()? else {
            return Err(format!(
                "{}: {:?}: while-condition is expected to be a comparison.",
                ERR_STR,
                location
            ))
        };
        self.expect(TokenType::ClosingRound)?;

        let block = self.parse_block()?;
        Ok(nodes::WhileNode {
            location,
            condition,
            block,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_break(&mut self) -> Result<nodes::BreakNode, String> {
        let location = self.current_location();
        self.expect(TokenType::BreakKeyword)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::BreakNode { location })
    }

    #[trace_call(always)]
    fn parse_stmt_continue(&mut self) -> Result<nodes::ContinueNode, String> {
        let location = self.current_location();
        self.expect(TokenType::ContinueKeyword)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::ContinueNode { location })
    }

    #[trace_call(always)]
    fn parse_expression(&mut self) -> Result<nodes::ExpressionNode, String> {
        let location = self.current_location();
        // TODO: Deprecate ExpressionNode, we can store all important information in Expression-enum
        let expression = self.parse_expression_enum()?;
        Ok(nodes::ExpressionNode {
            location,
            expression,
        })
    }

    #[trace_call(always)]
    fn parse_expression_enum(&mut self) -> Result<nodes::Expression, String> {
        // TODO: Refactor expressions
        //       Every expression always consists of two things:
        //       - Mandatory primary expression
        //       - Optional secondary expression, if we find an operator after primary
        //       We could simplify many things by always storing expressions as binary nodes
        //       instead of whatever we're doing right now
        self.parse_expression_rec(TokenType::Eof)
    }

    #[trace_call(always)]
    fn parse_expression_delim(&mut self) -> Result<nodes::Expression, String> {
        Ok(match self.nth(0) {
            TokenType::IntLiteral => {
                let expression = self.parse_expr_int_literal()?;
                nodes::Expression::Literal(expression)
            }
            TokenType::Identifier => {
                let expression = self.parse_expr_identifier()?;
                nodes::Expression::Identifier(expression)
            }
            TokenType::OpenRound => {
                self.expect(TokenType::OpenRound)?;
                let expression = self.parse_expression_enum()?;
                self.expect(TokenType::ClosingRound)?;
                expression
            }
            TokenType::OpenSquare => {
                let expression = self.parse_expr_array_literal()?;
                nodes::Expression::ArrayLiteral(expression)
            }
            TokenType::BuiltInFunction => {
                let expression = self.parse_expr_builtin()?;
                nodes::Expression::BuiltIn(expression)
            }
            e => {
                return Err(format!(
                    "{}: {:?}: Expected Expr, found {:?}",
                    ERR_STR,
                    self.current_location(),
                    e
                ));
            }
        })
    }

    #[trace_call(always)]
    fn parse_expression_rec(&mut self, left: TokenType) -> Result<nodes::Expression, String> {
        let mut lhs = self.parse_expression_delim()?;
        loop {
            let right = self.nth(0);
            if self.right_binds_tighter(left, right) {
                let token = self.next()?;
                let rhs = self.parse_expression_rec(right)?;
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
    fn right_binds_tighter(&self, left: TokenType, right: TokenType) -> bool {
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

    #[trace_call(always)]
    fn parse_expr_identifier(&mut self) -> Result<nodes::IdentifierNode, String> {
        let location = self.current_location();
        let expression = match self.nth(1) {
            TokenType::OpenRound =>
                nodes::Expression::FunctionCall(self.parse_expr_function_call()?),
            TokenType::OpenSquare =>
                nodes::Expression::ArrayAccess(self.parse_expr_array_access()?),
            TokenType::Dot =>
                nodes::Expression::FieldAccess(self.parse_expr_field_access()?),
            _ =>
                nodes::Expression::Name(self.parse_expr_name()?),
        };
        let expression = Box::new(expression);
        Ok(nodes::IdentifierNode {
            location,
            expression,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_literal(&mut self) -> Result<nodes::ArrayLiteralNode, String> {
        let location = self.current_location();
        let mut elements = vec![];
        self.expect(TokenType::OpenSquare)?;
        while !self.parsed_eof() && !self.at(TokenType::ClosingSquare) {
            let elem = self.parse_expression_enum()?;
            elements.push(elem);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(nodes::ArrayLiteralNode {
            location,
            elements,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_access(&mut self) -> Result<nodes::ArrayAccessNode, String> {
        let location = self.current_location();
        let array_name = self.expect(TokenType::Identifier)?;
        let array_name = array_name.value;
        let indices = self.parse_expr_array_literal()?;
        Ok(nodes::ArrayAccessNode {
            location,
            array_name,
            indices,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_builtin(&mut self) -> Result<nodes::BuiltInNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::BuiltInFunction)?;
        let function_name = name_token.value;

        self.expect(TokenType::OpenRound)?;
        let arguments = self.parse_arguments()?;
        self.expect(TokenType::ClosingRound)?;
        Ok(nodes::BuiltInNode {
            location,
            function_name,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_int_literal(&mut self) -> Result<nodes::LiteralNode, String> {
        let location = self.current_location();
        let number_token = self.expect(TokenType::IntLiteral)?;
        let (value, typ) = self.parse_type_literal(number_token)?;
        Ok(nodes::LiteralNode {
            location,
            value,
            typ,
        })
    }

    #[trace_call(always)]
    fn parse_expr_name(&mut self) -> Result<nodes::NameNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::Identifier)?;
        let name = name_token.value;
        Ok(nodes::NameNode {
            location,
            name,
            typ: Type::Unknown
        })
    }

    #[trace_call(always)]
    fn parse_expr_function_call(&mut self) -> Result<nodes::CallNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::Identifier)?;
        let function_name = name_token.value;

        self.expect(TokenType::OpenRound)?;
        let arguments = self.parse_arguments()?;
        self.expect(TokenType::ClosingRound)?;

        Ok(nodes::CallNode {
            is_constructor: function_name.as_bytes()[0].is_ascii_uppercase(),
            function_name,
            location,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_field_access(&mut self) -> Result<nodes::FieldAccessNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::Identifier)?;
        let name = name_token.value;
        self.expect(TokenType::Dot)?;
        let field = self.parse_expr_identifier()?;
        Ok(nodes::FieldAccessNode {
            location,
            name,
            field,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_arguments(&mut self) -> Result<Vec<nodes::ArgumentNode>, String> {
        let mut arguments = Vec::new();
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            let arg = self.parse_argument()?;
            arguments.push(arg);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        Ok(arguments)
    }

    #[trace_call(always)]
    fn parse_argument(&mut self) -> Result<nodes::ArgumentNode, String> {
        let location = self.current_location();
        let expression = self.parse_expression()?;
        Ok(nodes::ArgumentNode {
            location,
            expression,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_type_node(&mut self) -> Result<nodes::TypeNode, String> {
        let location = self.current_location();
        let name_token = self.expect(TokenType::Identifier)?;
        let typ = self.parse_type(name_token);
        let typ = if self.eat(TokenType::OpenSquare)? {
            let mut dimensions = vec![];
            // FIXME: This loop is a bit ugly
            while !self.parsed_eof() && !self.at(TokenType::ClosingSquare) {
                let size = self.expect(TokenType::IntLiteral)?;
                let location = size.location.clone();
                let (value, typ) = self.parse_type_literal(size)?;
                if typ != Type::Unknown && typ != Type::Usize {
                    return Err(format!(
                        "{}: {:?}: Unexpected type for integer literal. Expected Usize, found `{}`.",
                        ERR_STR,
                        location,
                        typ
                    ));
                }
                let value = match value.parse() {
                    Ok(v) => v,
                    Err(e) => {
                        return Err(format!(
                            "{}: {:?}: Error when parsing number as Usize: {e}",
                            ERR_STR, location
                        ))
                    }
                };
                dimensions.push(value);
                if !self.eat(TokenType::Comma)? {
                    break;
                }
            }
            if dimensions.is_empty() {
                return Err(format!(
                    "{}: {:?}: Expected size for array type, found ClosingSquare.",
                    ERR_STR,
                    self.current_location()
                ));
            }
            self.expect(TokenType::ClosingSquare)?;
            Type::Arr(Box::new(typ), dimensions)
        } else {
            typ
        };
        Ok(nodes::TypeNode { location, typ })
    }
}