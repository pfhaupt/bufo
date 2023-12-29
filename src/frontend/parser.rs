use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::path::PathBuf;

use super::nodes;
use crate::compiler::{BUILT_IN_FEATURES, CONSTRUCTOR_NAME, WARN_STR, ERR_STR, NOTE_STR};
use crate::middleend::type_checker::Type;
use crate::util::flags::Flags;

const LOOKAHEAD_LIMIT: usize = 3;

pub const CLASS_KEYWORD: &str = "class";
pub const FUNCTION_KEYWORD: &str = "func";
pub const FEATURE_KEYWORD: &str = "feat";
pub const LET_KEYWORD: &str = "let";
pub const IF_KEYWORD: &str = "if";
pub const ELSE_KEYWORD: &str = "else";
pub const RETURN_KEYWORD: &str = "return";
pub const WHILE_KEYWORD: &str = "while";
pub const BREAK_KEYWORD: &str = "break";
pub const CONTINUE_KEYWORD: &str = "continue";

use tracer::trace_call;

enum ParserError {
    InvalidCharLiteral(Location, String),
    UnterminatedStringLiteral(Location),
    // Syntax: Expected, Found
    UnexpectedTokenSingle(Location, TokenType, TokenType),
    // Syntax: Expected, Found
    UnexpectedTokenMany(Location, Vec<TokenType>, TokenType),
    UnexpectedSymbol(Location, char),
    InvalidFunctionName(Location, String),
    InvalidClassName(Location, String),
    UnknownFeature(Location, String),
    ExpectedCondition(Location, &'static str),
    ExpectedExpression(Location, TokenType),
    InvalidIntegerLiteral(Location, String, Type),
    STDParseIntError(Location, String, std::num::ParseIntError),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let error_msg = match self {
            Self::InvalidCharLiteral(l, s) => format!("{l:?}: Invalid Char Literal: {}", s),
            Self::UnterminatedStringLiteral(l) => format!("{l:?}: Unterminated String Literal"),
            Self::UnexpectedTokenSingle(l, expected, found) => format!(
                "{l:?}: Expected {}, found {}",
                expected, found
            ),
            Self::UnexpectedTokenMany(l, expected, found) =>  {
                let mut expected_str = String::from("");
                for (i, e) in expected.iter().enumerate() {
                    expected_str.push_str(&format!("{}", e));
                    if i == expected.len() - 2 {
                        expected_str.push_str(" or ");
                    } else if i != expected.len() - 1 {
                        expected_str.push_str(", ");
                    }
                }
                format!(
                    "{l:?}: Expected one of {}, found {}",
                    expected_str, found
                )
            },
            Self::UnexpectedSymbol(l, c) => format!("{l:?}: Unexpected Symbol `{}`", c),
            Self::InvalidFunctionName(l, s) => format!("{l:?}: Invalid Function Name: {}", s),
            Self::InvalidClassName(l, s) => format!("{l:?}: Invalid Class Name: {}", s),
            Self::UnknownFeature(l, s) => format!("{l:?}: Unknown Feature: {}", s),
            Self::ExpectedCondition(l, s) => format!("{l:?}: Expected Comparison for {}-condition", s),
            Self::ExpectedExpression(l, e) => format!("{l:?}: Expected Expression, found {}", e),
            Self::InvalidIntegerLiteral(l, s, typ) => format!("{l:?}: Invalid Integer Literal {} for type {}", s, typ),
            Self::STDParseIntError(l, s, e) => format!("{l:?}: Failed to parse integer literal {}\n{}: Reason: {}", s, NOTE_STR, e),
        };
        let message = format!("{}: {}", ERR_STR, error_msg);
        write!(f, "{}", message)
    }
}

macro_rules! try_parse {
    ($func:expr) => {
        if $func.is_none() {
            return None;
        }
    };
    ($var_name:ident, $func:expr) => {
        let $var_name = $func;
        if $var_name.is_none() {
            return None;
        }
        let $var_name = $var_name.unwrap();
    };
}

macro_rules! parse_or_recover {
    ($var_name:ident, $func:expr, $recover:expr) => {
        let $var_name = $func;
        if $var_name.is_none() {
            $recover;
            continue;
        }
        let $var_name = $var_name.unwrap();
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    CharLiteral,
    StrLiteral,
    IntLiteral,
    Identifier,
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
    Eof,
}

impl TokenType {
    fn is_opening_bracket(&self) -> bool {
        match self {
            Self::OpenRound | Self::OpenCurly | Self::OpenSquare => true,
            _ => false,
        }
    }
    fn is_closing_bracket(&self) -> bool {
        match self {
            Self::ClosingRound | Self::ClosingCurly | Self::ClosingSquare => true,
            _ => false,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CharLiteral => write!(f, "Char Literal"),
            Self::StrLiteral => write!(f, "String Literal"),
            Self::IntLiteral => write!(f, "Integer Literal"),
            Self::Identifier => write!(f, "Identifier"),
            Self::OpenRound => write!(f, "`(`"),
            Self::ClosingRound => write!(f, "`)`"),
            Self::OpenCurly => write!(f, "`{{`"),
            Self::ClosingCurly => write!(f, "`}}`"),
            Self::OpenSquare => write!(f, "`[`"),
            Self::ClosingSquare => write!(f, "`]`"),
            Self::ClassKeyword => write!(f, "`{}`", CLASS_KEYWORD),
            Self::FunctionKeyword => write!(f, "`{}`", FUNCTION_KEYWORD),
            Self::FeatureKeyword => write!(f, "`{}`", FEATURE_KEYWORD),
            Self::LetKeyword => write!(f, "`{}`", LET_KEYWORD),
            Self::IfKeyword => write!(f, "`{}`", IF_KEYWORD),
            Self::ElseKeyword => write!(f, "`{}`", ELSE_KEYWORD),
            Self::ReturnKeyword => write!(f, "`{}`", RETURN_KEYWORD),
            Self::WhileKeyword => write!(f, "`{}`", WHILE_KEYWORD),
            Self::BreakKeyword => write!(f, "`{}`", BREAK_KEYWORD),
            Self::ContinueKeyword => write!(f, "`{}`", CONTINUE_KEYWORD),
            Self::BuiltInFunction => write!(f, "`built-in function`"),
            Self::Colon => write!(f, "`:`"),
            Self::Semi => write!(f, "`;`"),
            Self::Comma => write!(f, "`,`"),
            Self::Dot => write!(f, "`.`"),
            Self::Arrow => write!(f, "`->`"),
            Self::Equal => write!(f, "`=`"),
            Self::Plus => write!(f, "`+`"),
            Self::Minus => write!(f, "`-`"),
            Self::Asterisk => write!(f, "`*`"),
            Self::ForwardSlash => write!(f, "`/`"),
            Self::CmpEq => write!(f, "`==`"),
            Self::CmpNeq => write!(f, "`!=`"),
            Self::CmpLt => write!(f, "`<`"),
            Self::CmpLte => write!(f, "`<=`"),
            Self::CmpGt => write!(f, "`>`"),
            Self::CmpGte => write!(f, "`>=`"),
            Self::Eof => write!(f, "End of File"),
        }
    }
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
    current_function: Option<String>,
    current_class: Option<String>,
    line_start: usize,
    errors: Vec<ParserError>,
    bracket_level: usize,
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
            filename: pb.as_os_str().to_str().unwrap().to_string(),
            filepath: pb,
            source,
            ..self
        }
    }
    // ---------- End of Builder Pattern ----------
    // ---------- Start of Lexer ----------
    fn lexed_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn trim_whitespace(&mut self) {
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
    fn next_token(&mut self) -> Token {
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
        self.trim_whitespace();
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
                    CLASS_KEYWORD => TokenType::ClassKeyword,
                    FUNCTION_KEYWORD => TokenType::FunctionKeyword,
                    FEATURE_KEYWORD => TokenType::FeatureKeyword,
                    LET_KEYWORD => TokenType::LetKeyword,
                    IF_KEYWORD => TokenType::IfKeyword,
                    ELSE_KEYWORD => TokenType::ElseKeyword,
                    RETURN_KEYWORD => TokenType::ReturnKeyword,
                    WHILE_KEYWORD => TokenType::WhileKeyword,
                    BREAK_KEYWORD => TokenType::BreakKeyword,
                    CONTINUE_KEYWORD => TokenType::ContinueKeyword,
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
                    self.report_error(ParserError::UnterminatedStringLiteral(
                        loc,
                    ));
                    return self.next_token();
                } else {
                    (TokenType::StrLiteral, value)
                }
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
                    self.report_error(ParserError::InvalidCharLiteral(
                        loc,
                        value
                    ));
                    return self.next_token();
                } else {
                    (TokenType::CharLiteral, value)
                }
            }
            '!' => match self.next_char() {
                '=' => (TokenType::CmpNeq, String::from("!=")),
                _ => {
                    self.report_error(ParserError::UnexpectedSymbol(
                        loc,
                        c
                    ));
                    return self.next_token();
                },
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
                    self.current_char += 1;
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
                self.report_error(ParserError::UnexpectedSymbol(
                    loc,
                    e
                ));
                return self.next_token();
            }
        };
        Token::new().location(loc).token_type(typ).value(value)
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
    fn fill_lookup(&mut self) {
        while self.lookahead.len() < LOOKAHEAD_LIMIT {
            let n = self.next_token();
            if self.flags.debug {
                // println!("[DEBUG] Found {:?}", n);
            }
            self.lookahead.push_back(n);
        }
    }
    // ---------- End of Lexer ----------
    // ---------- Start of Parser Utility ----------
    #[trace_call(always)]
    fn parse_type(&self, token: &Token) -> Type {
        self.parse_type_str(&token.value)
    }
    #[trace_call(always)]
    fn parse_type_str(&self, val: &str) -> Type {
        match val {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "usize" => Type::Usize,
            // Reserved for future use
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Class(val.to_string()),
        }
    }
    #[trace_call(always)]
    fn parse_type_literal(&self, lit_tkn: Token) -> Option<(String, Type, Location)> {
        let lit = lit_tkn.value;
        let loc = lit_tkn.location;
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ = self.parse_type_str(&lit[index..]);
                Some((lit[0..index].to_owned(), typ, loc))
            }
            None => Some((lit, Type::Unknown, loc)),
        }
    }

    #[trace_call(extra)]
    fn parsed_eof(&self) -> bool {
        self.lookahead[0].token_type == TokenType::Eof
    }

    #[trace_call(extra)]
    fn eat(&mut self, token_type: TokenType) -> bool {
        if self.at(token_type) {
            self.next();
            true
        } else {
            false
        }
    }

    #[trace_call(extra)]
    fn at(&self, token_type: TokenType) -> bool {
        self.nth(0) == token_type
    }

    #[trace_call(extra)]
    fn peek(&self, lookahead: usize) -> &Token {
        &self.lookahead[lookahead]
    }

    #[trace_call(extra)]
    fn nth(&self, lookahead: usize) -> TokenType {
        debug_assert!(self.lookahead.len() >= lookahead);
        self.lookahead[lookahead].token_type
    }

    #[trace_call(extra)]
    fn next(&mut self) -> Token {
        self.fill_lookup();
        debug_assert!(!self.lookahead.is_empty());
        let tkn = self.lookahead.pop_front().unwrap();
        if tkn.token_type.is_opening_bracket() {
            self.bracket_level += 1;
        } else if tkn.token_type.is_closing_bracket() {
            self.bracket_level -= 1;
        }
        if self.flags.debug {
            println!("[DEBUG] Consumed {:?}", tkn);
        }
        tkn
    }

    #[trace_call(always)]
    fn expect(&mut self, token_type: TokenType) -> Option<Token> {
        let n = self.peek(0);
        if n.token_type != token_type {
            self.report_error(ParserError::UnexpectedTokenSingle(
                n.location.clone(),
                token_type,
                n.token_type,
            ));
            None
        } else {
            let n = n.clone();
            self.next();
            Some(n)
        }
    }

    #[trace_call(always)]
    fn report_error(&mut self, error: ParserError) {
        if self.flags.debug {
            println!("[DEBUG] Error: {}", error);
        }
        self.errors.push(error);
    }

    #[trace_call(always)]
    fn recover(&mut self, tokens: &[TokenType]) {
        // Skip tokens until we find a recovery point
        // FIXME: I don't think bracket_level is working properly
        let bracket_level = self.bracket_level;
        while !self.parsed_eof() {
            let tkn = self.nth(0);
            if tokens.contains(&tkn) && self.bracket_level - 1 <= bracket_level {
                break;
            }
            if self.flags.debug {
                println!("[DEBUG] Recovering from {:?}", tkn);
            }
            self.next();
        }
        // Skip the recovery token
        assert!(self.parsed_eof() || tokens.contains(&self.nth(0)));
        if !self.parsed_eof() {
            self.next();
            if self.flags.debug {
                println!("[DEBUG] Continuing at {:?}", self.nth(0));
            }
        }
    }
    // ---------- End of Parser Utility ----------
    // ---------- Start of Parser ----------
    #[trace_call(always)]
    pub fn parse_file(&mut self) -> Result<nodes::FileNode, String> {
        self.fill_lookup();
        let location = self.current_location();
        let mut classes = vec![];
        let mut functions = vec![];
        const RECOVER_TOKENS: [TokenType; 1] = [
            TokenType::ClosingCurly,
        ];
        while !self.parsed_eof() {
            match self.nth(0) {
                TokenType::ClassKeyword => {
                    parse_or_recover!(parsed_class, self.parse_class(), self.recover(&RECOVER_TOKENS));
                    classes.push(parsed_class);
                }
                TokenType::FunctionKeyword => {
                    parse_or_recover!(parsed_function, self.parse_function(), self.recover(&[TokenType::ClosingCurly]));
                    functions.push(parsed_function);
                }
                _ => {
                    let tkn = self.next();
                    self.report_error(ParserError::UnexpectedTokenMany(
                        tkn.location,
                        vec![TokenType::ClassKeyword, TokenType::FunctionKeyword],
                        tkn.token_type,
                    ));
                    self.recover(&RECOVER_TOKENS);
                }
            }
        }
        if self.errors.is_empty() {
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
        } else {
            let mut error_string = String::from(self.errors[0].to_string());
            for error in &self.errors[1..] {
                error_string.push_str("\n");
                error_string.push_str(&error.to_string());
            }
            Err(error_string)
        }
    }

    #[trace_call(always)]
    fn parse_class(&mut self) -> Option<nodes::ClassNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::ClassKeyword));

        try_parse!(class_name, self.expect(TokenType::Identifier));
        let name = class_name.value;
        if !name.as_bytes()[0].is_ascii_uppercase() {
            self.report_error(ParserError::InvalidClassName(
                class_name.location,
                name
            ));
            return None;
        }
        self.current_class = Some(name.clone());
        try_parse!(self.expect(TokenType::OpenCurly));
        let mut fields = vec![];
        let mut methods = vec![];
        let mut features = vec![];
        let mut has_constructor = false;
        const RECOVER_TOKENS: [TokenType; 2] = [
            TokenType::ClosingCurly,
            TokenType::Semi,
        ];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            match self.nth(0) {
                TokenType::Identifier => {
                    parse_or_recover!(parsed_field, self.parse_field(), self.recover(&RECOVER_TOKENS));
                    fields.push(parsed_field);
                }
                TokenType::FeatureKeyword => {
                    parse_or_recover!(parsed_feature, self.parse_feature(&name), self.recover(&RECOVER_TOKENS));
                    has_constructor |= parsed_feature.is_constructor;
                    features.push(parsed_feature);
                }
                TokenType::FunctionKeyword => {
                    parse_or_recover!(parsed_method, self.parse_method(&name), self.recover(&RECOVER_TOKENS));
                    methods.push(parsed_method);
                }
                e => {
                    self.report_error(ParserError::UnexpectedTokenMany(
                        self.current_location(),
                        vec![TokenType::Identifier, TokenType::FeatureKeyword, TokenType::FunctionKeyword],
                        e,
                    ));
                    self.recover(&RECOVER_TOKENS);
                },
            }
        }
        try_parse!(self.expect(TokenType::ClosingCurly));

        self.current_class = None;
        Some(nodes::ClassNode {
            location,
            name,
            fields,
            methods,
            features,
            has_constructor,
        })
    }

    #[trace_call(always)]
    fn parse_field(&mut self) -> Option<nodes::FieldNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::Identifier));
        let name = name_token.value;
        try_parse!(self.expect(TokenType::Colon));
        try_parse!(type_def, self.parse_type_node());
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::FieldNode {
            location,
            name,
            type_def,
        })
    }

    #[trace_call(always)]
    fn parse_feature(&mut self, class_name: &str) -> Option<nodes::FeatureNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::FeatureKeyword));

        try_parse!(name_token, self.expect(TokenType::Identifier));
        if !name_token.value.as_bytes()[0].is_ascii_lowercase() {
            self.report_error(ParserError::InvalidFunctionName(
                name_token.location,
                name_token.value
            ));
            return None;
        }
        let name = name_token.value;
        self.current_function = Some(name.clone());

        if !BUILT_IN_FEATURES.contains(&name.as_str()) {
            self.report_error(ParserError::UnknownFeature(
                name_token.location,
                name
            ));
            return None;
        }

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(parameters, self.parse_parameters());
        // TODO: Don't forget to handle static methods later
        // Add `this` parameter
        let mut parameters = parameters;
        if name != CONSTRUCTOR_NAME {
            parameters.insert(
                0,
                nodes::ParameterNode::this(
                    location.clone(),
                    Type::Class(class_name.to_string()),
                ),
            );
        }
        try_parse!(self.expect(TokenType::ClosingRound));

        try_parse!(return_type, self.parse_return_type());

        try_parse!(block, self.parse_block());

        self.current_function = None;

        Some(nodes::FeatureNode {
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
    fn parse_function(&mut self) -> Option<nodes::FunctionNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::FunctionKeyword));

        try_parse!(name, self.expect(TokenType::Identifier));
        if !name.value.as_bytes()[0].is_ascii_lowercase() {
            self.report_error(ParserError::InvalidFunctionName(
                name.location,
                name.value
            ));
            return None;
        }

        self.current_function = Some(name.value.clone());

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(parameters, self.parse_parameters());
        try_parse!(self.expect(TokenType::ClosingRound));

        try_parse!(return_type, self.parse_return_type());

        try_parse!(block, self.parse_block());

        self.current_function = None;
        Some(nodes::FunctionNode {
            location,
            name: name.value,
            return_type,
            parameters,
            block,
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_method(&mut self, class_name: &str) -> Option<nodes::MethodNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::FunctionKeyword));

        try_parse!(name, self.expect(TokenType::Identifier));
        if !name.value.as_bytes()[0].is_ascii_lowercase() {
            self.report_error(ParserError::InvalidFunctionName(
                name.location,
                name.value
            ));
            return None;
        }

        self.current_function = Some(name.value.clone());

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(parameters, self.parse_parameters());
        // TODO: Don't forget to handle static methods later
        // Add `this` parameter
        let mut parameters = parameters;
        parameters.insert(
            0,
            nodes::ParameterNode::this(
                location.clone(),
                Type::Class(class_name.to_string()),
            ),
        );
        try_parse!(self.expect(TokenType::ClosingRound));

        try_parse!(return_type, self.parse_return_type());

        try_parse!(block, self.parse_block());

        self.current_function = None;

        Some(nodes::MethodNode {
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
    fn parse_return_type(&mut self) -> Option<nodes::TypeNode> {
        if self.eat(TokenType::Arrow) {
            self.parse_type_node()
        } else {
            Some(nodes::TypeNode::none(self.current_location()))
        }
    }

    #[trace_call(always)]
    fn parse_parameters(&mut self) -> Option<Vec<nodes::ParameterNode>> {
        let mut parameters = vec![];
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            let location = self.current_location();
            try_parse!(name_token, self.expect(TokenType::Identifier));
            let name = name_token.value;
            try_parse!(self.expect(TokenType::Colon));
            try_parse!(typ, self.parse_type_node());
            let param = nodes::ParameterNode {
                location,
                name,
                typ,
            };
            parameters.push(param);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        Some(parameters)
    }

    #[trace_call(always)]
    fn parse_block(&mut self) -> Option<nodes::BlockNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::OpenCurly));
        let mut statements = vec![];
        const RECOVER_TOKENS: [TokenType; 2] = [
            TokenType::Semi,
            TokenType::ClosingCurly,
        ];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            parse_or_recover!(parsed_statement, self.parse_statement(), self.recover(&RECOVER_TOKENS));
            statements.push(parsed_statement);
        }
        try_parse!(self.expect(TokenType::ClosingCurly));
        Some(nodes::BlockNode {
            location,
            statements,
        })
    }

    #[trace_call(always)]
    fn parse_statement(&mut self) -> Option<nodes::Statement> {
        Some(match self.nth(0) {
            TokenType::LetKeyword => {
                try_parse!(let_stmt, self.parse_stmt_let());
                nodes::Statement::Let(let_stmt)
            }
            TokenType::IfKeyword => {
                try_parse!(if_stmt, self.parse_stmt_if());
                nodes::Statement::If(if_stmt)
            }
            TokenType::ReturnKeyword => {
                try_parse!(return_stmt, self.parse_stmt_return());
                nodes::Statement::Return(return_stmt)
            }
            TokenType::WhileKeyword => {
                try_parse!(while_stmt, self.parse_stmt_while());
                nodes::Statement::While(while_stmt)
            }
            TokenType::BreakKeyword => {
                try_parse!(break_stmt, self.parse_stmt_break());
                nodes::Statement::Break(break_stmt)
            }
            TokenType::ContinueKeyword => {
                try_parse!(continue_stmt, self.parse_stmt_continue());
                nodes::Statement::Continue(continue_stmt)
            }
            TokenType::Identifier => match self.nth(1) {
                // FIXME: Simple void function calls are not handled correctly
                //        Currently, they are parsed as assignments
                TokenType::Dot | TokenType::Equal | TokenType::OpenSquare => {
                    try_parse!(assign_stmt, self.parse_assignment());
                    nodes::Statement::Assign(assign_stmt)
                }
                _ => {
                    try_parse!(expr, self.parse_expression());
                    try_parse!(self.expect(TokenType::Semi));
                    nodes::Statement::Expression(expr)
                }
            }
            s => {
                eprintln!("FIXME: Attempted to parse {:?} as statement", s);
                eprintln!("       Proceeding to parse as expression!");
                try_parse!(expr, self.parse_expression());
                try_parse!(self.expect(TokenType::Semi));
                nodes::Statement::Expression(expr)
            }
        })
    }

    #[trace_call(always)]
    fn parse_stmt_let(&mut self) -> Option<nodes::LetNode> {
        let location = self.current_location();
        self.expect(TokenType::LetKeyword).expect("This is guaranteed by parse_statement()");
        try_parse!(name_token, self.expect(TokenType::Identifier));
        try_parse!(self.expect(TokenType::Colon));
        try_parse!(typ, self.parse_type_node());
        try_parse!(self.expect(TokenType::Equal));
        try_parse!(expression, self.parse_expression());
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::LetNode{
            location,
            name: name_token.value,
            typ,
            expression,
        })
    }

    #[trace_call(always)]
    fn parse_assignment(&mut self) -> Option<nodes::AssignNode> {
        let location = self.current_location();
        try_parse!(name, self.parse_expr_identifier());
        try_parse!(self.expect(TokenType::Equal));
        try_parse!(expression, self.parse_expression());
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::AssignNode {
            location,
            name,
            expression,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_if(&mut self) -> Option<nodes::IfNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::IfKeyword));
        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(condition, self.parse_expression());
        let nodes::Expression::Comparison(condition) = condition else {
            self.report_error(ParserError::ExpectedCondition(
                location,
                IF_KEYWORD,
            ));
            return None;
        };
        try_parse!(self.expect(TokenType::ClosingRound));
        try_parse!(if_branch, self.parse_block());
        let else_branch = if self.eat(TokenType::ElseKeyword) {
            try_parse!(eb, self.parse_block());
            Some(eb)
        } else {
            None
        };
        Some(nodes::IfNode {
            location,
            condition,
            if_branch,
            else_branch,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_return(&mut self) -> Option<nodes::ReturnNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::ReturnKeyword));

        let return_value = if !self.at(TokenType::Semi) {
            try_parse!(rv, self.parse_expression());
            Some(rv)
        } else {
            None
        };
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::ReturnNode {
            location,
            return_value,
            typ: Type::Unknown,
            function: self.current_function.clone().expect("This is guaranteed by the recursive nature of the parser"),
            class: self.current_class.clone(),
        })
    }

    #[trace_call(always)]
    fn parse_stmt_while(&mut self) -> Option<nodes::WhileNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::WhileKeyword));

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(condition, self.parse_expression());
        let nodes::Expression::Comparison(condition) = condition else {
            self.report_error(ParserError::ExpectedCondition(
                location,
                WHILE_KEYWORD,
            ));
            return None;
        };
        try_parse!(self.expect(TokenType::ClosingRound));

        try_parse!(block, self.parse_block());
        Some(nodes::WhileNode {
            location,
            condition,
            block,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_break(&mut self) -> Option<nodes::BreakNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::BreakKeyword));
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::BreakNode { location })
    }

    #[trace_call(always)]
    fn parse_stmt_continue(&mut self) -> Option<nodes::ContinueNode> {
        let location = self.current_location();
        try_parse!(self.expect(TokenType::ContinueKeyword));
        try_parse!(self.expect(TokenType::Semi));
        Some(nodes::ContinueNode { location })
    }

    #[trace_call(always)]
    fn parse_expression(&mut self) -> Option<nodes::Expression> {
        // TODO: Refactor expressions
        //       Every expression always consists of two things:
        //       - Mandatory primary expression
        //       - Optional secondary expression, if we find an operator after primary
        //       We could simplify many things by always storing expressions as binary nodes
        //       instead of whatever we're doing right now
        self.parse_expression_rec(TokenType::Eof)
    }

    #[trace_call(always)]
    fn parse_expression_delim(&mut self) -> Option<nodes::Expression> {
        Some(match self.nth(0) {
            TokenType::IntLiteral => {
                try_parse!(expression, self.parse_expr_int_literal());
                nodes::Expression::Literal(expression)
            }
            TokenType::Identifier => {
                try_parse!(expression, self.parse_expr_identifier());
                nodes::Expression::Identifier(expression)
            }
            TokenType::OpenRound => {
                try_parse!(self.expect(TokenType::OpenRound));
                try_parse!(expression, self.parse_expression());
                try_parse!(self.expect(TokenType::ClosingRound));
                expression
            }
            TokenType::OpenSquare => {
                try_parse!(expression, self.parse_expr_array_literal());
                nodes::Expression::ArrayLiteral(expression)
            }
            TokenType::BuiltInFunction => {
                try_parse!(expression, self.parse_expr_builtin());
                nodes::Expression::BuiltIn(expression)
            }
            e => {
                self.report_error(ParserError::ExpectedExpression(
                    self.current_location(),
                    e
                ));
                return None;
            }
        })
    }

    #[trace_call(always)]
    fn parse_expression_rec(&mut self, left: TokenType) -> Option<nodes::Expression> {
        try_parse!(lhs, self.parse_expression_delim());
        let mut lhs = lhs;
        loop {
            let right = self.nth(0);
            if self.right_binds_tighter(left, right) {
                let token = self.next();
                try_parse!(rhs, self.parse_expression_rec(right));
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
        Some(lhs)
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
    fn parse_expr_identifier(&mut self) -> Option<nodes::IdentifierNode> {
        let location = self.current_location();
        let expression = match self.nth(1) {
            TokenType::OpenRound => {
                try_parse!(fn_call, self.parse_expr_function_call());
                nodes::Expression::FunctionCall(fn_call)
            }
            TokenType::OpenSquare => {
                try_parse!(array_access, self.parse_expr_array_access());
                nodes::Expression::ArrayAccess(array_access)
            }
            TokenType::Dot => {
                try_parse!(field_access, self.parse_expr_field_access());
                nodes::Expression::FieldAccess(field_access)
            }
            _ => {
                try_parse!(name_token, self.parse_expr_name());
                nodes::Expression::Name(name_token)
            }
        };
        let expression = Box::new(expression);
        Some(nodes::IdentifierNode {
            location,
            expression,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_literal(&mut self) -> Option<nodes::ArrayLiteralNode> {
        let location = self.current_location();
        let mut elements = vec![];
        try_parse!(self.expect(TokenType::OpenSquare));
        while !self.parsed_eof() && !self.at(TokenType::ClosingSquare) {
            try_parse!(elem, self.parse_expression());
            elements.push(elem);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        try_parse!(self.expect(TokenType::ClosingSquare));
        Some(nodes::ArrayLiteralNode {
            location,
            elements,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_access(&mut self) -> Option<nodes::ArrayAccessNode> {
        let location = self.current_location();
        try_parse!(array_name, self.expect(TokenType::Identifier));
        let array_name = array_name.value;
        try_parse!(indices, self.parse_expr_array_literal());
        Some(nodes::ArrayAccessNode {
            location,
            array_name,
            indices,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_builtin(&mut self) -> Option<nodes::BuiltInNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::BuiltInFunction));
        let function_name = name_token.value;

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(arguments, self.parse_arguments());
        try_parse!(self.expect(TokenType::ClosingRound));
        Some(nodes::BuiltInNode {
            location,
            function_name,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_int_literal(&mut self) -> Option<nodes::LiteralNode> {
        try_parse!(number_token, self.expect(TokenType::IntLiteral));
        try_parse!(value, self.parse_type_literal(number_token));
        let (value, typ, location) = value;
        Some(nodes::LiteralNode {
            location,
            value,
            typ,
        })
    }

    #[trace_call(always)]
    fn parse_expr_name(&mut self) -> Option<nodes::NameNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::Identifier));
        let name = name_token.value;
        Some(nodes::NameNode {
            location,
            name,
            typ: Type::Unknown
        })
    }

    #[trace_call(always)]
    fn parse_expr_function_call(&mut self) -> Option<nodes::CallNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::Identifier));
        let function_name = name_token.value;

        try_parse!(self.expect(TokenType::OpenRound));
        try_parse!(arguments, self.parse_arguments());
        try_parse!(self.expect(TokenType::ClosingRound));

        Some(nodes::CallNode {
            is_constructor: function_name.as_bytes()[0].is_ascii_uppercase(),
            function_name,
            location,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_expr_field_access(&mut self) -> Option<nodes::FieldAccessNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::Identifier));
        let name = name_token.value;
        try_parse!(self.expect(TokenType::Dot));
        try_parse!(field, self.parse_expr_identifier());
        Some(nodes::FieldAccessNode {
            location,
            name,
            field,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_arguments(&mut self) -> Option<Vec<nodes::Expression>> {
        let mut arguments = Vec::new();
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            try_parse!(arg, self.parse_expression());
            arguments.push(arg);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        Some(arguments)
    }

    #[trace_call(always)]
    fn parse_type_node(&mut self) -> Option<nodes::TypeNode> {
        let location = self.current_location();
        try_parse!(name_token, self.expect(TokenType::Identifier));
        let typ = self.parse_type(&name_token);
        let typ = if self.eat(TokenType::OpenSquare) {
            let mut dimensions = vec![];
            // FIXME: This loop is a bit ugly
            while !self.parsed_eof() && !self.at(TokenType::ClosingSquare) {
                try_parse!(size, self.expect(TokenType::IntLiteral));
                try_parse!(size, self.parse_type_literal(size));
                let (value, typ, location) = size;
                if typ != Type::Unknown && typ != Type::Usize {
                    self.report_error(ParserError::InvalidIntegerLiteral(
                        location,
                        value,
                        typ,
                    ));
                    return None;
                }
                let value = match value.parse() {
                    Ok(v) => v,
                    Err(e) => {
                        self.report_error(ParserError::STDParseIntError(
                            location,
                            value,
                            e,
                        ));
                        return None;
                    }
                };
                dimensions.push(value);
                if !self.eat(TokenType::Comma) {
                    break;
                }
            }
            if dimensions.is_empty() {
                self.report_error(ParserError::UnexpectedTokenSingle(
                    self.current_location(),
                    TokenType::IntLiteral,
                    self.nth(0)
                ));
                return None;
            }
            try_parse!(self.expect(TokenType::ClosingSquare));
            Type::Arr(Box::new(typ), dimensions)
        } else {
            typ
        };
        Some(nodes::TypeNode { location, typ })
    }
}