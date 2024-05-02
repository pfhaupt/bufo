use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::path::PathBuf;

use super::nodes::{self, CompilerFlag};
use crate::compiler::{ERR_STR, FILE_EXT, NOTE_STR, WARN_STR};
use crate::internal_panic;
use crate::middleend::type_checker::Type;
use crate::util::flags::Flags;
use tracer::trace_call;
use once_cell::sync::Lazy;

static FILE_ANONYMOUS: usize = 0;
static mut FILENAMES: Lazy<Vec<PathBuf>> = Lazy::new(|| {
    vec![PathBuf::from("anonymous")]
});

// fill_lookup() fills up to < LOOKAHEAD_LIMIT, we always only check one token ahead
// This makes our Parser LL(1), wooh! :D
const LOOKAHEAD_LIMIT: usize = 2;

pub const KEYWORD_BREAK: &str = "break";
pub const KEYWORD_COMPILER_FLAGS: &str = "compiler_flags";
pub const KEYWORD_CONTINUE: &str = "continue";
pub const KEYWORD_ELSE: &str = "else";
pub const KEYWORD_EXTERN: &str = "extern";
pub const KEYWORD_FALSE: &str = "false";
pub const KEYWORD_FOR: &str = "for";
pub const KEYWORD_FUNCTION: &str = "func";
pub const KEYWORD_IF: &str = "if";
pub const KEYWORD_IMPORT: &str = "import";
pub const KEYWORD_LET: &str = "let";
pub const KEYWORD_MUT: &str = "mut";
pub const KEYWORD_RETURN: &str = "return";
pub const KEYWORD_STRUCT: &str = "struct";
pub const KEYWORD_THIS: &str = "this";
pub const KEYWORD_TRUE: &str = "true";
pub const KEYWORD_TYPE: &str = "type"; // Reserved for future use
pub const KEYWORD_UNSAFE: &str = "unsafe";
pub const KEYWORD_WHILE: &str = "while";


enum ParserError {
    FileNotFound(Location, String, String, String),
    InvalidCharLiteral(Location, String),
    UnterminatedStringLiteral(Location),
    UnterminatedCharLiteral(Location),
    // Syntax: Expected, Found
    UnexpectedTokenSingle(Location, TokenType, TokenType),
    // Syntax: Expected, Found
    UnexpectedTokenMany(Location, Vec<TokenType>, TokenType),
    UnexpectedSymbol(Location, char),
    ExpectedExpression(Location, TokenType),
    ExpectedUnaryOperator(Location, TokenType),
    ExpectedBinaryOperator(Location, TokenType),
    ThisParameterHasType(Location),
    ThisParameterNotFirst(Location),
    ForbiddenThisParameter(Location),
    ThisOutsideClass(Location),
    InvalidCompilerFlag(String),
    CompilerFlagsNotFirst(Location),
    InvalidArraySize(Location),
    ArrayWithSpecifiedSizeMoreThanOneElement(Location),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let error_msg = match self {
            Self::FileNotFound(l, s, e, i) => format!("{l:?}: File not found: {s}\n{NOTE_STR}: {e} when looking for file `{i}`."),
            Self::InvalidCharLiteral(l, s) => format!("{l:?}: Invalid Char Literal: `{}`", s),
            Self::UnterminatedStringLiteral(l) => format!("{l:?}: Unterminated String Literal"),
            Self::UnterminatedCharLiteral(l) => format!("{l:?}: Unterminated Char Literal"),
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
            Self::ExpectedExpression(l, e) => format!("{l:?}: Expected Expression, found {}", e),
            Self::ExpectedUnaryOperator(l, t) => format!("{l:?}: Expected Unary Operator, found {}", t),
            Self::ExpectedBinaryOperator(l, t) => format!("{l:?}: Expected Binary Operator, found {}", t),
            Self::ThisParameterHasType(l) => format!("{l:?}: Unexpected type for `this` parameter.\n{}: The type of `this` is always the struct the method is defined in.", NOTE_STR),
            Self::ThisParameterNotFirst(l) => format!("{l:?}: `this` parameter must be the first parameter of a method."),
            Self::ForbiddenThisParameter(l) => format!("{l:?}: Unexpected `this` parameter.\n{}: `this` parameters are only allowed in methods.", NOTE_STR),
            Self::ThisOutsideClass(l) => format!("{l:?}: Unexpected `this` outside of a struct.\n{}: `this` is only allowed in methods.", NOTE_STR),
            Self::InvalidCompilerFlag(s) => format!("{}", s),
            Self::CompilerFlagsNotFirst(l) => format!("{l:?}: `{KEYWORD_COMPILER_FLAGS}` must be the first statement in a file."),
            Self::InvalidArraySize(l) => format!("{l:?}: Invalid array size."),
            Self::ArrayWithSpecifiedSizeMoreThanOneElement(l) => format!("{l:?}: Arrays with a specified size can only have one element."),
        };
        let message = format!("{}: {}", ERR_STR, error_msg);
        write!(f, "{}", message)
    }
}

// NOTE: When adding a new TokenType, search for TOKEN_TYPE_HANDLE_HERE for places that need to be updated
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    KeywordBreak,
    KeywordCompilerFlags,
    KeywordContinue,
    KeywordElse,
    KeywordExtern,
    KeywordFalse,
    KeywordFor,
    KeywordFunc,
    KeywordIf,
    KeywordImport,
    KeywordLet,
    KeywordMut,
    KeywordReturn,
    KeywordStruct,
    KeywordThis,
    KeywordTrue,
    KeywordType,
    KeywordUnsafe,
    KeywordWhile,
    LiteralChar,
    LiteralString,
    LiteralInteger,
    Identifier,
    OpenRound,
    ClosingRound,
    OpenCurly,
    ClosingCurly,
    OpenSquare,
    ClosingSquare,
    Colon,
    DoubleColon,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Semi,
    Comma,
    Dot,
    Exclamation,
    VarArg,
    Arrow,
    Equal,
    Plus,
    Minus,
    Asterisk,
    Percent,
    Caret,
    ForwardSlash,
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpLte,
    CmpGt,
    CmpGte,
    Eof,
}


macro_rules! attach_module_specifier {
    ($node:ident, $name:expr) => {
        {
            let module_spec = nodes::ModuleSpecifier {
                name: $name,
                sub_module: None
            };
            if let Some(sub_mod) = &mut $node.module_path {
                let mut last = sub_mod;
                while let Some(ref mut next) = last.sub_module {
                    last = next;
                }
                last.sub_module = Some(Box::new(module_spec));
            } else {
                $node.module_path = Some(Box::new(module_spec));
            }
        }
    };
}

impl TokenType {
    fn is_opening_bracket(&self) -> bool {
        match self {
            Self::OpenRound | Self::OpenCurly => true,
            _ => false,
        }
    }
    fn is_closing_bracket(&self) -> bool {
        match self {
            Self::ClosingRound | Self::ClosingCurly => true,
            _ => false,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TOKEN_TYPE_HANDLE_HERE
        match self {
            Self::LiteralChar => write!(f, "Char Literal"),
            Self::LiteralString => write!(f, "String Literal"),
            Self::LiteralInteger => write!(f, "Integer Literal"),
            Self::Identifier => write!(f, "Identifier"),
            Self::OpenRound => write!(f, "`(`"),
            Self::ClosingRound => write!(f, "`)`"),
            Self::OpenCurly => write!(f, "`{{`"),
            Self::ClosingCurly => write!(f, "`}}`"),
            Self::OpenSquare => write!(f, "`[`"),
            Self::ClosingSquare => write!(f, "`]`"),
            Self::KeywordBreak => write!(f, "`{}`", KEYWORD_BREAK),
            Self::KeywordCompilerFlags => write!(f, "`{}`", KEYWORD_COMPILER_FLAGS),
            Self::KeywordContinue => write!(f, "`{}`", KEYWORD_CONTINUE),
            Self::KeywordElse => write!(f, "`{}`", KEYWORD_ELSE),
            Self::KeywordExtern => write!(f, "`{}`", KEYWORD_EXTERN),
            Self::KeywordFalse => write!(f, "`{}`", KEYWORD_FALSE),
            Self::KeywordFor => write!(f, "`{}`", KEYWORD_FOR),
            Self::KeywordFunc => write!(f, "`{}`", KEYWORD_FUNCTION),
            Self::KeywordIf => write!(f, "`{}`", KEYWORD_IF),
            Self::KeywordImport => write!(f, "`{}`", KEYWORD_IMPORT),
            Self::KeywordLet => write!(f, "`{}`", KEYWORD_LET),
            Self::KeywordMut => write!(f, "`{}`", KEYWORD_MUT),
            Self::KeywordReturn => write!(f, "`{}`", KEYWORD_RETURN),
            Self::KeywordStruct => write!(f, "`{}`", KEYWORD_STRUCT),
            Self::KeywordTrue => write!(f, "`{}`", KEYWORD_TRUE),
            Self::KeywordThis => write!(f, "`{}`", KEYWORD_THIS),
            Self::KeywordType => write!(f, "`{}`", KEYWORD_TYPE),
            Self::KeywordUnsafe => write!(f, "`{}`", KEYWORD_UNSAFE),
            Self::KeywordWhile => write!(f, "`{}`", KEYWORD_WHILE),
            Self::Colon => write!(f, "`:`"),
            Self::DoubleColon => write!(f, "`::`"),
            Self::Ampersand => write!(f, "`&`"),
            Self::DoubleAmpersand => write!(f, "`&&`"),
            Self::Pipe => write!(f, "`|`"),
            Self::DoublePipe => write!(f, "`||`"),
            Self::Semi => write!(f, "`;`"),
            Self::Comma => write!(f, "`,`"),
            Self::Dot => write!(f, "`.`"),
            Self::Exclamation => write!(f, "`!`"),
            Self::VarArg => write!(f, "`...`"),
            Self::Arrow => write!(f, "`->`"),
            Self::Equal => write!(f, "`=`"),
            Self::Plus => write!(f, "`+`"),
            Self::Minus => write!(f, "`-`"),
            Self::Asterisk => write!(f, "`*`"),
            Self::ForwardSlash => write!(f, "`/`"),
            Self::Percent => write!(f, "`%`"),
            Self::Caret => write!(f, "`^`"),
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

#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct Location {
    file_id: usize,
    row: usize,
    col: usize,
}

impl Location {
    #[trace_call(extra)]
    pub fn new(file_id: usize, row: usize, col: usize) -> Self {
        Self { file_id, row, col }
    }

    #[trace_call(extra)]
    pub fn anonymous() -> Self {
        Self::new(FILE_ANONYMOUS, 0, 0)
    }
}



impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let file = unsafe { FILENAMES.get(self.file_id) };
        if file.is_none() {
            internal_panic!("Invalid file_id {}", self.file_id)
        } else {
            let file = file.unwrap();
            write!(f, "{}:{}:{}", file.to_str().unwrap(), self.row, self.col)
        }
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
    Assign,
    Negate,
    MemberAccess,
    IndexedAccess,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Reference,
    Dereference,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::Add => write!(f, "+"),
            Self::Sub | Self::Negate => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseXor => write!(f, "^"),
            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalNot => write!(f, "!"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::IndexedAccess => write!(f, "[]"),
            Self::MemberAccess => write!(f, "."),
            Self::Reference => write!(f, "&"),
            Self::Dereference => write!(f, "*"),
        }
    }
}

impl Operation {
    #[trace_call(extra)]
    fn from(s: &str) -> Option<Self> {
        debug_assert_eq!(Operation::GreaterThanOrEqual as u8 + 1, 21, "Not all Operations are handled in from()");
        // TOKEN_TYPE_HANDLE_HERE (if new token is an operator)
        match s {
            "=" => Some(Self::Assign),
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "%" => Some(Self::Modulo),
            "&" => Some(Self::BitwiseAnd),
            "|" => Some(Self::BitwiseOr),
            "^" => Some(Self::BitwiseXor),
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            "<" => Some(Self::LessThan),
            "<=" => Some(Self::LessThanOrEqual),
            ">" => Some(Self::GreaterThan),
            ">=" => Some(Self::GreaterThanOrEqual),
            "." => Some(Self::MemberAccess),
            "&&" => Some(Self::LogicalAnd),
            "||" => Some(Self::LogicalOr),
            "!" => Some(Self::LogicalNot),
            "[" => Some(Self::IndexedAccess),
            _ => None,
        }
    }

    #[trace_call(extra)]
    pub fn is_comparison(&self) -> bool {
        match self {
            Self::Equal => true,
            Self::NotEqual => true,
            Self::LessThan => true,
            Self::LessThanOrEqual => true,
            Self::GreaterThan => true,
            Self::GreaterThanOrEqual => true,
            _ => false,
        }
    }

    #[trace_call(extra)]
    pub fn is_arithmetic(&self) -> bool {
        match self {
            Self::Add => true,
            Self::Sub => true,
            Self::Mul => true,
            Self::Div => true,
            Self::Modulo => true,
            _ => false,
        }
    }

    #[trace_call(extra)]
    pub fn is_bitwise(&self) -> bool {
        match self {
            Self::BitwiseAnd => true,
            Self::BitwiseOr => true,
            Self::BitwiseXor => true,
            _ => false,
        }
    }

    #[trace_call(extra)]
    pub fn is_logical(&self) -> bool {
        match self {
            Self::LogicalAnd => true,
            Self::LogicalOr => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Associativity {
    Left,
    Right,
}

pub struct Parser<'flags> {
    filepath: PathBuf,
    files_to_parse: Vec<PathBuf>,
    module_stack: Vec<String>,
    parsed_files: HashSet<PathBuf>,
    include_locations: HashMap<PathBuf, Vec<Location>>,
    file_id: usize,
    source: Vec<char>,
    lookahead: VecDeque<Token>,
    current_char: usize,
    current_line: usize,
    current_function: Option<String>,
    current_struct: Option<String>,
    known_externs: Vec<String>,
    line_start: usize,
    errors: Vec<ParserError>,
    bracket_level: i32,
    flags: &'flags Flags,
}

impl<'flags> Parser<'flags> {
    // ---------- Start of Builder Pattern ----------
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            filepath: PathBuf::new(),
            module_stack: Vec::new(),
            files_to_parse: Vec::new(),
            parsed_files: HashSet::new(),
            include_locations: HashMap::new(),
            file_id: 0,
            source: Vec::new(),
            lookahead: VecDeque::new(),
            current_char: 0,
            current_line: 1,
            current_function: None,
            current_struct: None,
            known_externs: Vec::new(),
            line_start: 0,
            errors: Vec::new(),
            bracket_level: 0,
            flags,
        }
    }

    fn filepath(&mut self, filepath: &str) {
        let pb = PathBuf::from(filepath);
        let source = match fs::read_to_string(filepath) {
            Ok(source) => source.chars().collect(),
            Err(e) => {
                let locations = self.include_locations.get(&pb).expect("Filepath not found").clone();
                for loc in locations {
                    self.report_error(ParserError::FileNotFound(
                        loc.clone(),
                        filepath.to_string(),
                        e.to_string(),
                        filepath.to_string(),
                    ));
                }
                Vec::new()
            },
        };
        self.file_id = unsafe { FILENAMES.len() };
        unsafe {
            FILENAMES.push(pb.clone());
        }
        self.filepath = pb;
        self.source = source;
        self.reset_state(false);
    }

    fn reset_state(&mut self, reset_errors: bool) {
        self.lookahead.clear();
        self.current_char = 0;
        self.current_line = 1;
        self.current_function = None;
        self.current_struct = None;
        self.line_start = 0;
        if reset_errors {
            self.errors.clear();
        }
        self.bracket_level = 0;
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
                        if $stepback && nc != '\0' {
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
            55,
            "Not all TokenTypes are handled in next_token()"
        );
        self.trim_whitespace();
        let c = self.next_char();
        let loc = self.get_location();
        let (typ, value) = match c {
            '0'..='9' => {
                let value = fill_buffer!(c, |c: char| { !c.is_alphanumeric() });
                (TokenType::LiteralInteger, value)
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let value = fill_buffer!(c, |c: char| {
                    !c.is_alphanumeric() && c != '_'
                });
                // TOKEN_TYPE_HANDLE_HERE (if new token is a keyword)
                let typ = match value.as_str() {
                    KEYWORD_BREAK => TokenType::KeywordBreak,
                    KEYWORD_COMPILER_FLAGS => TokenType::KeywordCompilerFlags,
                    KEYWORD_CONTINUE => TokenType::KeywordContinue,
                    KEYWORD_ELSE => TokenType::KeywordElse,
                    KEYWORD_EXTERN => TokenType::KeywordExtern,
                    KEYWORD_FALSE => TokenType::KeywordFalse,
                    KEYWORD_FOR => TokenType::KeywordFor,
                    KEYWORD_FUNCTION => TokenType::KeywordFunc,
                    KEYWORD_IF => TokenType::KeywordIf,
                    KEYWORD_IMPORT => TokenType::KeywordImport,
                    KEYWORD_LET => TokenType::KeywordLet,
                    KEYWORD_MUT => TokenType::KeywordMut,
                    KEYWORD_RETURN => TokenType::KeywordReturn,
                    KEYWORD_STRUCT => TokenType::KeywordStruct,
                    KEYWORD_THIS => TokenType::KeywordThis,
                    KEYWORD_TRUE => TokenType::KeywordTrue,
                    KEYWORD_TYPE => TokenType::KeywordType,
                    KEYWORD_UNSAFE => TokenType::KeywordUnsafe,
                    KEYWORD_WHILE => TokenType::KeywordWhile,
                    _ => TokenType::Identifier,
                };
                (typ, value)
            }
            '\"' => {
                let value = fill_buffer!(c, |c: char| { c == '"' || c == '\0' }, false);
                if value.chars().filter(|c| *c == '"').count() != 2 {
                    self.report_error(ParserError::UnterminatedStringLiteral(
                        loc,
                    ));
                    return self.next_token();
                }
                let value = value[1..value.len() - 1].to_string(); // Remove quotes
                let mut new_value: Vec<u8> = vec![];
                let mut escape = false;
                for c in value.chars() {
                    if escape {
                        match c {
                            'n' => new_value.push('\n' as u8),
                            'r' => new_value.push('\r' as u8),
                            't' => new_value.push('\t' as u8),
                            '\\' => new_value.push('\\' as u8),
                            '"' => new_value.push('"' as u8),
                            _ => {
                                self.report_error(ParserError::UnexpectedSymbol(
                                    loc,
                                    c
                                ));
                            }
                        }
                        escape = false;
                    } else if c == '\\' {
                        escape = true;
                    } else {
                        new_value.push(c as u8);
                    }
                }
                let value = String::from_utf8(new_value).unwrap();
                (TokenType::LiteralString, value)
            }
            '\'' => {
                let value = fill_buffer!(c, |c: char| { c == '\'' || c == '\0' }, false);
                if value.chars().filter(|c| *c == '\'').count() != 2 {
                    self.report_error(ParserError::UnterminatedCharLiteral(
                        loc,
                    ));
                    return self.next_token();
                }
                let value = value[1..value.len() - 1].to_string(); // Remove quotes
                let mut new_value: Vec<u8> = vec![];
                let mut escape = false;
                for c in value.chars() {
                    if escape {
                        match c {
                            'n' => new_value.push('\n' as u8),
                            'r' => new_value.push('\r' as u8),
                            't' => new_value.push('\t' as u8),
                            '\\' => new_value.push('\\' as u8),
                            '\'' => new_value.push('\'' as u8),
                            _ => {
                                self.report_error(ParserError::UnexpectedSymbol(
                                    loc,
                                    c
                                ));
                            }
                        }
                        escape = false;
                    } else if c == '\\' {
                        escape = true;
                    } else {
                        new_value.push(c as u8);
                    }
                }
                let value = String::from_utf8(new_value).unwrap();
                if value.len() != 1 {
                    self.report_error(ParserError::InvalidCharLiteral(
                        loc,
                        value
                    ));
                    return self.next_token();
                }
                (TokenType::LiteralChar, value)
            }
            '!' => match self.next_char() {
                '=' => (TokenType::CmpNeq, String::from("!=")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Exclamation, String::from(c))
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
                '*' => {
                    while !self.lexed_eof() {
                        let nc = self.next_char();
                        if nc == '*' {
                            if self.next_char() == '/' {
                                break;
                            }
                        }
                    }
                    return self.next_token();
                }
                _ => {
                    self.current_char -= 1;
                    (TokenType::ForwardSlash, String::from("/"))
                }
            },
            ':' => match self.next_char() {
                ':' => (TokenType::DoubleColon, String::from("::")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Colon, String::from(c))
                }
            },
            '&' => match self.next_char() {
                '&' => (TokenType::DoubleAmpersand, String::from("&&")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Ampersand, String::from(c))
                }
            },
            '|' => match self.next_char() {
                '|' => (TokenType::DoublePipe, String::from("||")),
                _ => {
                    self.current_char -= 1;
                    (TokenType::Pipe, String::from(c))
                }
            },
            '^' => (TokenType::Caret, String::from(c)),
            '(' => (TokenType::OpenRound, String::from(c)),
            ')' => (TokenType::ClosingRound, String::from(c)),
            '{' => (TokenType::OpenCurly, String::from(c)),
            '}' => (TokenType::ClosingCurly, String::from(c)),
            '[' => (TokenType::OpenSquare, String::from(c)),
            ']' => (TokenType::ClosingSquare, String::from(c)),
            ';' => (TokenType::Semi, String::from(c)),
            ',' => (TokenType::Comma, String::from(c)),
            '.' => {
                let value = fill_buffer!(c, |c: char| { c != '.' || c == '\0' });
                if value.len() == 3 {
                    (TokenType::VarArg, String::from("..."))
                } else if value.len() == 1 {
                    (TokenType::Dot, String::from(c))
                } else {
                    self.report_error(ParserError::UnexpectedSymbol(
                        loc,
                        c
                    ));
                    return self.next_token();
                }
            }
            '+' => (TokenType::Plus, String::from(c)),
            '*' => (TokenType::Asterisk, String::from(c)),
            '%' => (TokenType::Percent, String::from(c)),
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
            self.file_id,
            self.current_line,
            self.current_char - self.line_start,
        )
    }

    #[trace_call(extra)]
    fn current_location(&self) -> Location {
        debug_assert!(!self.lookahead.is_empty());
        self.lookahead[0].location
    }

    #[trace_call(extra)]
    fn fill_lookup(&mut self) {
        while self.lookahead.len() < LOOKAHEAD_LIMIT {
            let n = self.next_token();
            self.lookahead.push_back(n);
        }
    }
    // ---------- End of Lexer ----------
    // ---------- Start of Parser Utility ----------
    #[trace_call(always)]
    fn parse_type_str(&self, val: &str) -> Type {
        match val {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "usize" => Type::Usize,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "Any" => Type::Any,
            "str" => Type::Str,
            // Reserved for future use
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Struct(val.to_string(), None),
        }
    }
    #[trace_call(always)]
    fn parse_type_literal(&self, lit_tkn: Token)-> (String, Type, Location) {
        let lit = lit_tkn.value;
        let loc = lit_tkn.location;
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ = self.parse_type_str(&lit[index..]);
                (lit[0..index].to_owned(), typ, loc)
            }
            None => (lit, Type::Unknown, loc),
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
    fn expect(&mut self, token_type: TokenType)-> Result<Token, ()> {
        let n = self.peek(0);
        if n.token_type != token_type {
            self.report_error(ParserError::UnexpectedTokenSingle(
                n.location,
                token_type,
                n.token_type,
            ));
            Err(())
        } else {
            let n = n.clone();
            self.next();
            Ok(n)
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
                println!(
                    "[DEBUG] Recovering from {:?} at {:?}",
                    tkn,
                    self.current_location()
                );
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

    #[trace_call(always)]
    fn stringify_errors(&self) -> String {
        let mut errors = String::new();
        for e in &self.errors {
            errors.push_str(&format!("{}\n", e));
        }
        errors
    }
    // ---------- End of Parser Utility ----------
    // ---------- Start of Parser ----------
    #[trace_call(always)]
    pub fn parse_project(&mut self) -> Result<nodes::ModuleNode, String> {
        self.filepath(&self.flags.input);
        let root_path = PathBuf::from(&self.flags.input);
        let mut root = nodes::ModuleNode {
            location: Location::anonymous(),
            name: String::from("root"),
            modules: vec![],
            structs: vec![],
            functions: vec![],
            externs: vec![],
            imports: vec![],
            compiler_flags: None,
        };

        {
            // FIXME: Better way to handle prelude
            let prelude_path = PathBuf::from("./prelude/prelude.bu");
            let prelude = self.parse_file(&prelude_path, 0, false);
            assert!(prelude.is_ok(), "Failed to parse prelude");
            let prelude_module = prelude.unwrap();
            root.modules.push(prelude_module);
        }

        self.files_to_parse.push(root_path.clone());
        let root_path = root_path.parent().unwrap();
        let root_path_count = root_path.components().count();
        while let Some(file) = self.files_to_parse.pop() {
            if self.parsed_files.contains(&file) {
                continue;
            }
            let module = self.parse_file(&file, root_path_count, true);
            if let Ok(module) = module {
                self.parsed_files.insert(file.clone());
                root.modules.push(module);
            }
        }
        // let root = self.parse_directory(&root_path.parent().unwrap());
        if self.errors.is_empty() {
            Ok(root)
        } else {
            Err(self.stringify_errors())
        }
    }

    #[trace_call(always)]
    fn parse_file(&mut self, filepath: &PathBuf, root_path_count: usize, file_is_module: bool) -> Result<nodes::ModuleNode, ()> {
        if self.flags.verbose {
            println!("[INFO] Parsing `{}`", filepath.to_str().unwrap());
        }
        let modules = &filepath.components().map(
            |c| match c {
                std::path::Component::Normal(c) => {
                    let modified = c.to_str().unwrap().to_string();
                    if modified.ends_with(&format!(".{}", FILE_EXT)) {
                        modified.strip_suffix(&format!(".{}", FILE_EXT)).unwrap().to_string()
                    } else {
                        modified
                    }
                },
                _ => String::new(),
            }
        ).collect::<Vec<_>>()[root_path_count..];
        let modules = &modules[..modules.len() - 1];
        self.filepath(filepath.to_str().unwrap());
        self.fill_lookup();
        if file_is_module {
            for module in modules {
                self.module_stack.push(module.to_string());
            }
        }
        let m = self.parse_module(filepath.file_stem().unwrap().to_str().unwrap().to_string());
        if file_is_module {
            for _ in modules {
                self.module_stack.pop();
            }
        }
        let Ok(mut m) = m else {
            return Err(());
        };
        // FIXME: This shuffle breaks ModuleSpecifier
        // It's better to just "parse" directories too :^)
        if file_is_module {
            for module in modules.iter().rev() {
                let new_module = nodes::ModuleNode {
                    location: Location::anonymous(),
                    name: module.clone(),
                    modules: vec![m],
                    structs: vec![],
                    functions: vec![],
                    externs: vec![],
                    imports: vec![],
                    compiler_flags: None,
                };
                m = new_module;
            }
        }
        m.imports.push(nodes::ImportNode {
            location: Location::anonymous(),
            trace: vec![(String::from("prelude"), Location::anonymous())],
        });
        Ok(m)
    }

    #[trace_call(always)]
    fn parse_module(&mut self, module_name: String)-> Result<nodes::ModuleNode, ()> {
        self.module_stack.push(module_name.clone());
        let location = self.current_location();
        let modules = vec![];
        let mut structs = vec![];
        let mut functions = vec![];
        let mut imports = vec![];
        let mut externs = vec![];

        const RECOVER_TOKENS: [TokenType; 3] = [
            TokenType::ClosingCurly,
            TokenType::Semi,
            TokenType::Eof,
        ];
        let mut valid = true;
        let compiler_flags = match self.parse_compiler_flags() {
            Ok(compiler_flags) => Some(compiler_flags),
            Err(_) => {
                self.recover(&RECOVER_TOKENS);
                valid = false;
                None
            }
        };
        while !self.parsed_eof() {
            match self.nth(0) {
                TokenType::KeywordCompilerFlags => {
                    self.expect(TokenType::KeywordCompilerFlags)?;
                    self.report_error(ParserError::CompilerFlagsNotFirst(
                        self.current_location(),
                    ));
                    self.recover(&RECOVER_TOKENS);
                    valid = false;
                }
                TokenType::KeywordExtern => {
                    let Ok(parsed_extern) = self.parse_extern(false) else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    self.known_externs.push(parsed_extern.name.clone());
                    externs.push(parsed_extern);
                }
                TokenType::KeywordFunc => {
                    let Ok(parsed_function) = self.parse_function(false) else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    functions.push(parsed_function);
                }
                TokenType::KeywordImport => {
                    let Ok(parsed_import) = self.parse_import() else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    imports.push(parsed_import);
                }
                TokenType::KeywordStruct => {
                    let Ok(parsed_struct) = self.parse_struct() else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    structs.push(parsed_struct);
                }
                TokenType::KeywordUnsafe => {
                    self.expect(TokenType::KeywordUnsafe)?;
                    match self.nth(0) {
                        TokenType::KeywordFunc => {
                            let Ok(parsed_function) = self.parse_function(true) else {
                                self.recover(&RECOVER_TOKENS);
                                valid = false;
                                continue;
                            };
                            functions.push(parsed_function);
                        }
                        TokenType::KeywordExtern => {
                            let Ok(parsed_extern) = self.parse_extern(true) else {
                                self.recover(&RECOVER_TOKENS);
                                valid = false;
                                continue;
                            };
                            self.known_externs.push(parsed_extern.name.clone());
                            externs.push(parsed_extern);
                        }
                        _ => {
                            let tkn = self.next();
                            valid = false;
                            self.report_error(ParserError::UnexpectedTokenMany(
                                tkn.location,
                                vec![TokenType::KeywordExtern, TokenType::KeywordFunc],
                                tkn.token_type,
                            ));
                            self.recover(&RECOVER_TOKENS);
                        }
                    }
                }
                _ => {
                    let tkn = self.next();
                    valid = false;
                    self.report_error(ParserError::UnexpectedTokenMany(
                        tkn.location,
                        vec![
                            TokenType::KeywordExtern,
                            TokenType::KeywordFunc,
                            TokenType::KeywordImport,
                            TokenType::KeywordStruct,
                            TokenType::KeywordUnsafe
                        ],
                        tkn.token_type,
                    ));
                    self.recover(&RECOVER_TOKENS);
                },
            }
        }
        self.module_stack.pop();
        if !valid {
            return Err(());
        }

        Ok(nodes::ModuleNode {
            location,
            name: module_name.to_string(),
            structs,
            functions,
            externs,
            modules,
            imports,
            compiler_flags,
        })
    }

    #[trace_call(always)]
    fn parse_import(&mut self) -> Result<nodes::ImportNode, ()> {
        self.expect(TokenType::KeywordImport)?;
        let location = self.current_location();
        let module = self.expect(TokenType::Identifier)?;
        let mut trace = vec![(module.value, module.location)];
        while self.eat(TokenType::DoubleColon) {
            let next = self.expect(TokenType::Identifier)?;
            trace.push((next.value, next.location));
        }
        self.expect(TokenType::Semi)?;
        let filename = trace.iter().map(|(s, _)| s.clone()).collect::<Vec<String>>().join("/");
        let root_path = self.filepath.parent().unwrap();
        let path = root_path.join(&filename);
        let path = path.with_extension(FILE_EXT);
        if self.flags.verbose {
            println!("[INFO] Importing `{}`", path.to_str().unwrap());
        }
        self.include_locations.entry(path.clone()).or_insert(vec![]).push(location.clone());
        self.files_to_parse.push(path.clone());
        Ok(nodes::ImportNode {
            location,
            trace,
        })
    }

    #[trace_call(always)]
    fn parse_compiler_flags(&mut self)-> Result<nodes::CompilerFlagsNode, ()> {
        let mut compiler_flags = vec![];
        if self.eat(TokenType::KeywordCompilerFlags) {
            let location = self.current_location();
            self.expect(TokenType::OpenCurly)?;
            while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
                let flag = self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Colon)?;
                let value = self.expect(TokenType::LiteralString)?;
                self.expect(TokenType::Semi)?;
                let comp_flag = CompilerFlag::from(flag.location, flag.value.clone(), value.value);
                match comp_flag {
                    Ok(flag) => compiler_flags.push(flag),
                    Err(e) => {
                        self.report_error(ParserError::InvalidCompilerFlag(e));
                        self.recover(&[TokenType::Semi, TokenType::ClosingCurly]);
                    }
                }
            }
            self.expect(TokenType::ClosingCurly)?;
            Ok(nodes::CompilerFlagsNode {
                location,
                flags: compiler_flags,
            })
        } else {
            Ok(nodes::CompilerFlagsNode {
                location: Location::anonymous(),
                flags: vec![],
            })
        }
    }

    #[trace_call(always)]
    fn parse_extern(&mut self, is_unsafe: bool)-> Result<nodes::ExternNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordExtern)?;

        let name_token = self.expect(TokenType::Identifier)?;

        self.expect(TokenType::OpenRound)?;
        let mut parameters = vec![];
        let mut is_vararg = false;
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            if self.eat(TokenType::VarArg) {
                is_vararg = true;
                break;
            }
            let param = self.parse_single_parameter(false)?;
            parameters.push(param);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        self.expect(TokenType::Semi)?;

        Ok(nodes::ExternNode {
            location,
            name: name_token.value,
            return_type,
            parameters,
            is_unsafe,
            is_vararg
        })
    }

    #[trace_call(always)]
    fn parse_struct(&mut self)-> Result<nodes::StructNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordStruct)?;

        let struct_name = self.expect(TokenType::Identifier)?;
        let name = struct_name.value;
        if !name.as_bytes()[0].is_ascii_uppercase() {
            println!("{}: {:?}: Struct names must start with an uppercase letter.", WARN_STR, struct_name.location);
        }

        self.current_struct = Some(name.clone());
        self.expect(TokenType::OpenCurly)?;
        let mut fields = vec![];
        let mut methods = vec![];
        let mut valid = true;
        const RECOVER_TOKENS: [TokenType; 2] = [
            TokenType::ClosingCurly,
            TokenType::Semi,
        ];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            match self.nth(0) {
                TokenType::Identifier => {
                    let parsed_field = self.parse_field();
                    if parsed_field.is_err() {
                        self.recover(&RECOVER_TOKENS);
                    } else {
                        fields.push(parsed_field.unwrap());
                    }
                }
                TokenType::KeywordFunc => {
                    let parsed_method = self.parse_method(&name, false);
                    if parsed_method.is_err() {
                        self.recover(&RECOVER_TOKENS);
                    } else {
                        methods.push(parsed_method.unwrap());
                    }
                }
                TokenType::KeywordUnsafe => {
                    self.expect(TokenType::KeywordUnsafe)?;
                    match self.nth(0) {
                        TokenType::KeywordFunc => {
                            let parsed_method = self.parse_method(&name, true);
                            if parsed_method.is_err() {
                                self.recover(&RECOVER_TOKENS);
                            } else {
                                methods.push(parsed_method.unwrap());
                            }
                        }
                        _ => {
                            valid = false;
                            let tkn = self.next();
                            self.report_error(ParserError::UnexpectedTokenMany(
                                tkn.location,
                                vec![TokenType::KeywordFunc],
                                tkn.token_type,
                            ));
                            self.recover(&RECOVER_TOKENS);
                        }
                    }
                }
                e => {
                    self.report_error(ParserError::UnexpectedTokenMany(
                        self.current_location(),
                        vec![TokenType::Identifier, TokenType::KeywordFunc],
                        e,
                    ));
                    self.recover(&RECOVER_TOKENS);
                },
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        self.current_struct = None;
        if !valid {
            return Err(());
        }
        Ok(nodes::StructNode {
            location,
            name,
            fields,
            methods,
            module_path: Some(Box::new(nodes::ModuleSpecifier::from_resolved(&self.module_stack)))
        })
    }

    #[trace_call(always)]
    fn parse_field(&mut self)-> Result<nodes::FieldNode, ()> {
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
    fn parse_function(&mut self, is_unsafe: bool) -> Result<nodes::FunctionNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordFunc)?;

        let name = self.expect(TokenType::Identifier)?;

        self.current_function = Some(name.value.clone());

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters(false)?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        let block = self.parse_block(is_unsafe)?;

        self.current_function = None;
        Ok(nodes::FunctionNode {
            location,
            name: name.value,
            module_path: Some(Box::new(nodes::ModuleSpecifier::from_resolved(&self.module_stack))),
            return_type,
            parameters,
            block,
            is_unsafe,
            is_vararg: false,
            #[cfg(feature = "old_codegen")]
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_method(&mut self, struct_name: &str, is_unsafe: bool) -> Result<nodes::MethodNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordFunc)?;

        let name = self.expect(TokenType::Identifier)?;

        self.current_function = Some(name.value.clone());

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters(true)?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        let block = self.parse_block(false)?;

        self.current_function = None;

        Ok(nodes::MethodNode {
            location,
            struct_name: struct_name.to_string(),
            name: name.value,
            module_path: Some(Box::new(nodes::ModuleSpecifier::from_resolved(&self.module_stack))),
            return_type,
            parameters,
            block,
            is_unsafe,
            is_vararg: false,
            #[cfg(feature = "old_codegen")]
            stack_size: 0,
        })
    }

    #[trace_call(always)]
    fn parse_return_type(&mut self)-> Result<nodes::TypeNode, ()> {
        if self.eat(TokenType::Arrow) {
            self.parse_type_node()
        } else {
            Ok(nodes::TypeNode::none(self.current_location()))
        }
    }

    #[trace_call(always)]
    fn parse_parameters(&mut self, allowed_this: bool)-> Result<Vec<nodes::ParameterNode>, ()> {
        /*
        Rules of `this` usage:
        - In a method:
            - Either the first parameter is `this` and has no type
            - Or there is no `this` parameter (static method)
        - In a function:
            - There is no `this` parameter
        - In an extern:
            - There is no `this` parameter
         */
        let mut parameters = vec![];
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            let param = self.parse_single_parameter(allowed_this)?;
            if param.name == "this" && parameters.len() != 0 {
                debug_assert!(allowed_this, "this parameter not allowed");
                self.report_error(ParserError::ThisParameterNotFirst(
                    param.location,
                ));
                return Err(());
            }
            parameters.push(param);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        Ok(parameters)
    }

    #[trace_call(always)]
    fn parse_single_parameter(&mut self, allowed_this: bool) -> Result<nodes::ParameterNode, ()> {
        let location = self.current_location();
            let is_reference = self.eat(TokenType::Ampersand);
            let is_mutable = self.eat(TokenType::KeywordMut);
            let is_this = self.eat(TokenType::KeywordThis);
            if is_this {
                if !allowed_this {
                    self.report_error(ParserError::ForbiddenThisParameter(
                        location,
                    ));
                    return Err(());
                }
                if self.eat(TokenType::Colon) {
                    self.report_error(ParserError::ThisParameterHasType(
                        location,
                    ));
                    return Err(());
                }
                let struct_typ = Type::Struct(self.current_struct.as_ref().unwrap().clone(), None);
                let typ = if is_reference {
                    Type::Ref(Box::new(struct_typ), is_mutable)
                } else {
                    struct_typ
                };
                Ok(nodes::ParameterNode {
                    location,
                    name: String::from("this"),
                    typ: nodes::TypeNode::this(location, typ),
                    is_mutable,
                })
            } else {
                let name_token = self.expect(TokenType::Identifier)?;
                let name = name_token.value;
                self.expect(TokenType::Colon)?;
                let typ = self.parse_type_node()?;
                Ok(nodes::ParameterNode {
                    location,
                    name,
                    typ,
                    is_mutable,
                })
            }
    }

    #[trace_call(always)]
    fn parse_block(&mut self, is_unsafe: bool) -> Result<nodes::BlockNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::OpenCurly)?;
        let mut statements = vec![];
        const RECOVER_TOKENS: [TokenType; 2] = [
            TokenType::Semi,
            TokenType::ClosingCurly,
        ];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            let parsed_statement = self.parse_statement(is_unsafe);
            if parsed_statement.is_err() {
                self.recover(&RECOVER_TOKENS);
                continue;
            }
            statements.push(parsed_statement.unwrap());
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(nodes::BlockNode {
            location,
            statements,
            is_unsafe,
            #[cfg(not(feature = "old_codegen"))]
            llvm_has_terminator: false,
        })
    }

    #[trace_call(always)]
    fn parse_statement(&mut self, is_unsafe: bool) -> Result<nodes::Statement, ()> {
        Ok(match self.nth(0) {
            TokenType::KeywordMut => {
                let mut_stmt = self.parse_stmt_var_decl(true)?;
                nodes::Statement::VarDecl(mut_stmt)
            }
            TokenType::KeywordLet => {
                let let_stmt = self.parse_stmt_var_decl(false)?;
                nodes::Statement::VarDecl(let_stmt)
            }
            TokenType::KeywordIf => {
                let if_stmt = self.parse_stmt_if(is_unsafe)?;
                nodes::Statement::If(if_stmt)
            }
            TokenType::KeywordReturn => {
                let return_stmt = self.parse_stmt_return()?;
                nodes::Statement::Return(return_stmt)
            }
            TokenType::KeywordWhile => {
                let while_stmt = self.parse_stmt_while(is_unsafe)?;
                nodes::Statement::While(while_stmt)
            }
            TokenType::KeywordFor => {
                self.parse_stmt_for(is_unsafe)?
            }
            TokenType::KeywordBreak => {
                let break_stmt = self.parse_stmt_break()?;
                nodes::Statement::Break(break_stmt)
            }
            TokenType::KeywordContinue => {
                let continue_stmt = self.parse_stmt_continue()?;
                nodes::Statement::Continue(continue_stmt)
            }
            TokenType::KeywordUnsafe => {
                self.expect(TokenType::KeywordUnsafe)?;
                let block = self.parse_block(true)?;
                nodes::Statement::Block(block)
            }
            TokenType::OpenCurly => {
                let block = self.parse_block(is_unsafe)?;
                nodes::Statement::Block(block)
            }
            _ => {
                let expr = self.parse_expression(0, Associativity::Left)?;
                self.expect(TokenType::Semi)?;
                nodes::Statement::Expression(expr)
            }
        })
    }

    #[trace_call(always)]
    fn parse_stmt_var_decl(&mut self, is_mutable: bool)-> Result<nodes::VarDeclNode, ()> {
        let location = self.current_location();
        if is_mutable {
            self.expect(TokenType::KeywordMut)?;
        } else {
            self.expect(TokenType::KeywordLet)?;
        }
        let name_token = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let typ = self.parse_type_node()?;
        self.expect(TokenType::Equal)?;
        let expression = self.parse_expression(0, Associativity::Left)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::VarDeclNode{
            location,
            name: name_token.value,
            typ,
            expression,
            is_mutable,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_for(&mut self, is_unsafe: bool) -> Result<nodes::Statement, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordFor)?;
        if self.eat(TokenType::OpenRound) {
            let init = if self.eat(TokenType::Semi) {
                None
            } else {
                let init = self.parse_statement(is_unsafe)?;
                Some(init)
            };
            let condition = if self.eat(TokenType::Semi) {
                None
            } else {
                let condition = self.parse_expression(0, Associativity::Left)?;
                self.expect(TokenType::Semi)?;
                Some(condition)
            };
            let increment = if self.eat(TokenType::ClosingRound) {
                None
            } else {
                let increment = self.parse_expression(0, Associativity::Left)?;
                self.expect(TokenType::ClosingRound)?;
                Some(increment)
            };
            let body = self.parse_statement(is_unsafe)?;
            let mut body = match body {
                nodes::Statement::Block(body) => body,
                _ => {
                    let mut statements = vec![];
                    let location = body.get_loc();
                    statements.push(body);
                    nodes::BlockNode {
                        location,
                        statements,
                        is_unsafe,
                        #[cfg(not(feature = "old_codegen"))]
                        llvm_has_terminator: false,
                    }
                }
            };
            if increment.is_some() {
                body.statements.push(nodes::Statement::Expression(increment.unwrap()));
            }
            let desugared_while = nodes::WhileNode {
                location,
                condition: condition.unwrap_or_else(|| nodes::Expression::Literal(nodes::LiteralNode {
                    location: location.clone(),
                    value: String::from("true"),
                    typ: Type::Bool,
                })),
                body,
            };
            let mut desugared_block = nodes::BlockNode {
                location,
                statements: vec![],
                is_unsafe,
                #[cfg(not(feature = "old_codegen"))]
                llvm_has_terminator: false,
            };
            if init.is_some() {
                desugared_block.statements.push(init.unwrap());
            }
            desugared_block.statements.push(nodes::Statement::While(desugared_while));
            Ok(nodes::Statement::Block(desugared_block))
        } else {
            // FIXME: Implement for-in loop
            todo!()
        }
    }

    #[trace_call(always)]
    fn parse_stmt_if(&mut self, is_unsafe: bool)-> Result<nodes::IfNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordIf)?;
        self.expect(TokenType::OpenRound)?;
        let condition = self.parse_expression(0, Associativity::Left)?;
        self.expect(TokenType::ClosingRound)?;
        let if_body = self.parse_statement(is_unsafe)?;
        let if_body = match if_body {
            nodes::Statement::Block(if_body) => if_body,
            _ => {
                let mut statements = vec![];
                let location = if_body.get_loc();
                statements.push(if_body);
                nodes::BlockNode {
                    location,
                    statements,
                    is_unsafe,
                    #[cfg(not(feature = "old_codegen"))]
                    llvm_has_terminator: false,
                }
            }
        };
        let else_body = if self.eat(TokenType::KeywordElse) {
            let eb = self.parse_statement(is_unsafe)?;
            Some(Box::new(eb))
        } else {
            None
        };
        let else_body = match else_body {
            Some(eb) => {
                match *eb {
                    nodes::Statement::Block(ref eb) => Some(eb.clone()),
                    _ => {
                        let mut statements = vec![];
                        let location = eb.get_loc();
                        statements.push(*eb);
                        Some(nodes::BlockNode {
                            location,
                            statements,
                            is_unsafe,
                            #[cfg(not(feature = "old_codegen"))]
                            llvm_has_terminator: false,
                        })
                    }
                }
            }
            None => None,
        };
        Ok(nodes::IfNode {
            location,
            condition,
            if_body: Box::new(if_body),
            else_body,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_return(&mut self)-> Result<nodes::ReturnNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordReturn)?;

        let return_value = if !self.at(TokenType::Semi) {
            let rv = self.parse_expression(0, Associativity::Left)?;
            Some(rv)
        } else {
            None
        };
        self.expect(TokenType::Semi)?;
        Ok(nodes::ReturnNode {
            location,
            return_value,
            typ: Type::Unknown,
            function: self.current_function.clone().expect("This is guaranteed by the recursive nature of the parser"),
            strukt: self.current_struct.clone(),
        })
    }

    #[trace_call(always)]
    fn parse_stmt_while(&mut self, is_unsafe: bool)-> Result<nodes::WhileNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordWhile)?;

        self.expect(TokenType::OpenRound)?;
        let condition = self.parse_expression(0, Associativity::Left)?;
        self.expect(TokenType::ClosingRound)?;

        let body = self.parse_statement(is_unsafe)?;
        let body = match body {
            nodes::Statement::Block(body) => body,
            _ => {
                let mut statements = vec![];
                let location = body.get_loc();
                statements.push(body);
                nodes::BlockNode {
                    location,
                    statements,
                    is_unsafe,
                    #[cfg(not(feature = "old_codegen"))]
                    llvm_has_terminator: false,
                }
            }
        };
        Ok(nodes::WhileNode {
            location,
            condition,
            body: body,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_break(&mut self)-> Result<nodes::BreakNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordBreak)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::BreakNode { location })
    }

    #[trace_call(always)]
    fn parse_stmt_continue(&mut self)-> Result<nodes::ContinueNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::KeywordContinue)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::ContinueNode { location })
    }

    // Roughly inspired by:
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
    // If it works for JS, it should work for us too
    #[trace_call(always)]
    fn get_binary_precedence(&self, token_type: TokenType) -> usize {
        // TOKEN_TYPE_HANDLE_HERE
        match token_type {
            TokenType::Dot => 17,
            TokenType::OpenSquare => 16, // Array index
            TokenType::ForwardSlash => 12,
            TokenType::Asterisk => 12,
            TokenType::Percent => 12,
            TokenType::Plus => 11,
            TokenType::Minus => 11,
            TokenType::CmpLt => 9,
            TokenType::CmpLte => 9,
            TokenType::CmpGt => 9,
            TokenType::CmpGte => 9,
            TokenType::CmpEq => 8,
            TokenType::CmpNeq => 8,
            TokenType::Ampersand => 7,
            TokenType::Caret => 6,
            TokenType::Pipe => 5,
            TokenType::DoubleAmpersand => 4,
            TokenType::DoublePipe => 3,
            TokenType::Equal => 2,
            e => internal_panic!("get_binary_precedence({:?}) is not implemented", e),
        }
    }

    #[trace_call(always)]
    fn get_unary_precedence(&self, token_type: TokenType) -> usize {
        // TOKEN_TYPE_HANDLE_HERE
        match token_type {
            TokenType::Minus => 14,
            TokenType::Ampersand => 14,
            TokenType::Asterisk => 14,
            TokenType::DoubleAmpersand => 14,
            TokenType::Exclamation => 14,
            e => internal_panic!("get_unary_precedence({:?}) is not implemented", e),
        }
    }

    #[trace_call(always)]
    fn get_associativity(&self, token_type: TokenType) -> Associativity {
        // TOKEN_TYPE_HANDLE_HERE
        match token_type {
            TokenType::Plus => Associativity::Left,
            TokenType::Minus => Associativity::Left,
            TokenType::Asterisk => Associativity::Left,
            TokenType::ForwardSlash => Associativity::Left,
            TokenType::Percent => Associativity::Left,
            TokenType::Ampersand => Associativity::Left,
            TokenType::Pipe => Associativity::Left,
            TokenType::Caret => Associativity::Left,
            TokenType::CmpEq => Associativity::Left,
            TokenType::CmpNeq => Associativity::Left,
            TokenType::CmpLt => Associativity::Left,
            TokenType::CmpLte => Associativity::Left,
            TokenType::CmpGt => Associativity::Left,
            TokenType::CmpGte => Associativity::Left,
            TokenType::Dot => Associativity::Left,
            TokenType::DoubleAmpersand => Associativity::Left,
            TokenType::DoublePipe => Associativity::Left,
            TokenType::OpenSquare => Associativity::Left,
            TokenType::Equal => Associativity::Right,
            e => internal_panic!("Entered unreachable code: get_associativity({:?})", e),
        }
    }

    // Inspired by the awesome work done by the SerenityOS team:
    // Credit: https://github.com/SerenityOS/serenity/blob/master/Userland/Libraries/LibJS/Parser.cpp
    #[trace_call(always)]
    fn parse_expression(&mut self, min_precedence: usize, associativity: Associativity)-> Result<nodes::Expression, ()> {
        let mut expression = self.parse_primary_expression()?;

        while self.matches_binary_expression() {
            let new_precedence = self.get_binary_precedence(self.nth(0));
            if new_precedence < min_precedence {
                break;
            }
            if new_precedence == min_precedence && associativity == Associativity::Left {
                break;
            }
            let new_associativity = self.get_associativity(self.nth(0));
            let result = self.parse_secondary_expression(expression, new_precedence, new_associativity)?;
            expression = result;
        }
        Ok(expression)
    }

    #[trace_call(always)]
    fn parse_primary_expression(&mut self)-> Result<nodes::Expression, ()> {
        // let location = self.current_location();
        if self.matches_unary_expression() {
            let unary = self.parse_unary_expression()?;
            return Ok(nodes::Expression::Unary(unary));
        }
        match self.nth(0) {
            TokenType::LiteralInteger => {
                let int_literal = self.parse_expr_int_literal()?;
                Ok(nodes::Expression::Literal(int_literal))
            }
            TokenType::LiteralString => {
                let str_literal = self.parse_expr_str_literal()?;
                Ok(nodes::Expression::Literal(str_literal))
            }
            TokenType::LiteralChar => {
                let char_literal = self.parse_expr_char_literal()?;
                Ok(nodes::Expression::Literal(char_literal))
            }
            TokenType::OpenSquare => {
                let array_literal = self.parse_expr_array_literal()?;
                Ok(nodes::Expression::ArrayLiteral(array_literal))
            }
            TokenType::Identifier => {
                let identifier = self.parse_expr_identifier()?;
                Ok(identifier)
            }
            TokenType::OpenRound => {
                self.expect(TokenType::OpenRound)?;
                let expression = self.parse_expression(0, Associativity::Left)?;
                self.expect(TokenType::ClosingRound)?;
                Ok(expression)
            }
            TokenType::KeywordTrue | TokenType::KeywordFalse => {
                let bool_literal = self.parse_expr_bool_literal()?;
                Ok(nodes::Expression::Literal(bool_literal))
            }
            TokenType::KeywordThis => {
                let this_token = self.next();
                if self.current_struct.is_none() {
                    self.report_error(ParserError::ThisOutsideClass(
                        this_token.location,
                    ));
                    return Err(());
                }
                let this_literal = nodes::NameNode {
                    location: this_token.location,
                    name: String::from("this"),
                    typ: Type::Struct(self.current_struct.as_ref().unwrap().clone(), None),
                };
                Ok(nodes::Expression::Name(this_literal))
            }
            e => {
                self.report_error(ParserError::ExpectedExpression(
                    self.current_location(),
                    e
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn matches_unary_expression(&mut self) -> bool {
        // TOKEN_TYPE_HANDLE_HERE
        match self.nth(0) {
            TokenType::Minus => true,
            TokenType::Ampersand => true,
            TokenType::Asterisk => true,
            TokenType::DoubleAmpersand => true,
            TokenType::Exclamation => true,
            _ => false,
        }
    }

    #[trace_call(always)]
    fn matches_binary_expression(&mut self) -> bool {
        // TOKEN_TYPE_HANDLE_HERE
        match self.nth(0) {
            TokenType::Equal => true,
            TokenType::Plus => true,
            TokenType::Minus => true,
            TokenType::Asterisk => true,
            TokenType::ForwardSlash => true,
            TokenType::Percent => true,
            TokenType::Ampersand => true,
            TokenType::Pipe => true,
            TokenType::Caret => true,
            TokenType::CmpEq => true,
            TokenType::CmpNeq => true,
            TokenType::CmpLt => true,
            TokenType::CmpLte => true,
            TokenType::CmpGt => true,
            TokenType::CmpGte => true,
            TokenType::Dot => true,
            TokenType::DoubleAmpersand => true,
            TokenType::DoublePipe => true,
            TokenType::OpenSquare => true,
            _ => false,
        }
    }

    #[trace_call(always)]
    fn parse_secondary_expression(
        &mut self,
        lhs: nodes::Expression,
        precedence: usize,
        associativity: Associativity,
    )-> Result<nodes::Expression, ()> {
        let location = self.current_location();
        debug_assert!(self.matches_binary_expression());
        let op_token = self.next();
        let op = Operation::from(&op_token.value);
        if op.is_none() {
            self.report_error(ParserError::ExpectedBinaryOperator(
                location,
                self.nth(0),
            ));
            return Err(());
        }
        let op = op.unwrap();
        let rhs = if op == Operation::IndexedAccess {
            // Precedence 0 is like an imaginary bracket around the expression
            // This is to ensure that the expression is parsed as a single unit
            // i.e. a[0-3] is parsed as a[(0-3)] and not a[(3)-0], which causes an error (Found -, expected ])
            let rhs = self.parse_expression(0, associativity)?;
            self.expect(TokenType::ClosingSquare)?;
            rhs
        } else {
            self.parse_expression(precedence, associativity)?
        };
        Ok(nodes::Expression::Binary(nodes::BinaryNode {
            location,
            operation: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            typ: Type::Unknown,
        }))
    }

    #[trace_call(always)]
    fn parse_unary_expression(&mut self)-> Result<nodes::UnaryNode, ()> {
        let location = self.current_location();
        let precedence = self.get_unary_precedence(self.nth(0));
        match self.nth(0) {
            TokenType::Minus => {
                self.expect(TokenType::Minus)?;
                let expression = self.parse_expression(precedence, Associativity::Left)?;
                Ok(nodes::UnaryNode {
                    location,
                    operation: Operation::Negate,
                    expression: Box::new(expression),
                    typ: Type::Unknown,
                })
            }
            TokenType::Ampersand => {
                self.expect(TokenType::Ampersand)?;
                let is_mutable = self.eat(TokenType::KeywordMut);
                let expression = self.parse_expression(precedence, Associativity::Left)?;
                Ok(nodes::UnaryNode {
                    location,
                    operation: Operation::Reference,
                    expression: Box::new(expression),
                    typ: Type::Ref(Box::new(Type::Unknown), is_mutable),
                })
            }
            TokenType::DoubleAmpersand => {
                self.expect(TokenType::DoubleAmpersand)?;
                let is_mutable = self.eat(TokenType::KeywordMut);
                let expression = self.parse_expression(precedence, Associativity::Left)?;
                let inner_type = Type::Ref(Box::new(Type::Unknown), is_mutable);
                let outer_type = Type::Ref(Box::new(inner_type.clone()), false);
                let inner_ref = nodes::UnaryNode {
                    location,
                    operation: Operation::Reference,
                    expression: Box::new(expression),
                    typ: inner_type,
                };
                Ok(nodes::UnaryNode {
                    location,
                    operation: Operation::Reference,
                    expression: Box::new(nodes::Expression::Unary(inner_ref)),
                    typ: outer_type,
                })
            }
            TokenType::Asterisk => {
                self.expect(TokenType::Asterisk)?;
                let expression = self.parse_expression(precedence, Associativity::Left)?;
                Ok(nodes::UnaryNode {
                    location,
                    operation: Operation::Dereference,
                    expression: Box::new(expression),
                    typ: Type::Unknown,
                })
            }
            TokenType::Exclamation => {
                self.expect(TokenType::Exclamation)?;
                let expression = self.parse_expression(precedence, Associativity::Left)?;
                Ok(nodes::UnaryNode {
                    location,
                    operation: Operation::LogicalNot,
                    expression: Box::new(expression),
                    typ: Type::Bool,
                })
            }
            _ => {
                self.report_error(ParserError::ExpectedUnaryOperator(
                    self.current_location(),
                    self.nth(0),
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn parse_expr_identifier(&mut self)-> Result<nodes::Expression, ()> {
        let ident = self.expect(TokenType::Identifier)?;
        if self.eat(TokenType::DoubleColon) {
            // REVIEW: Re-introduce ParenExprNode
            // Expressions such as X::(y) will work here :^)
            let mut expr = self.parse_expression(0, Associativity::Left)?;
            match expr {
                nodes::Expression::StructLiteral(ref mut struct_lit) => {
                    attach_module_specifier!(struct_lit, ident.value);
                }
                nodes::Expression::FunctionCall(ref mut func_call) => {
                    attach_module_specifier!(func_call, ident.value);
                }
                _ => {
                    // Error
                    todo!()
                }
            }
            Ok(expr)
        } else if self.at(TokenType::OpenRound) {
            let fn_call = self.parse_expr_function_call(ident)?;
            Ok(nodes::Expression::FunctionCall(fn_call))
        } else if self.at(TokenType::OpenCurly) {
            let struct_literal = self.parse_expr_struct_literal(ident)?;
            Ok(nodes::Expression::StructLiteral(struct_literal))
        } else {
            let name_token = self.parse_expr_name(ident)?;
            Ok(nodes::Expression::Name(name_token))
        }
    }

    #[trace_call(always)]
    fn parse_expr_struct_literal(&mut self, ident: Token)-> Result<nodes::StructLiteralNode, ()> {
        let location = ident.location;
        self.expect(TokenType::OpenCurly)?;
        let mut fields = vec![];
        while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
            let name_token = self.expect(TokenType::Identifier)?;
            let name = name_token.value;
            self.expect(TokenType::Colon)?;
            let expression = self.parse_expression(0, Associativity::Left)?;
            fields.push((name, expression));
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(nodes::StructLiteralNode {
            location,
            struct_name: ident.value.clone(),
            module_path: None,
            fields,
            typ: Type::Struct(ident.value, None),
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_literal(&mut self) -> Result<nodes::ArrayLiteralNode, ()> {
        let location = self.current_location();
        self.expect(TokenType::OpenSquare)?;
        let mut elements = vec![];
        let mut size_init = false;
        while !self.parsed_eof() && !self.at(TokenType::ClosingSquare) {
            let element = self.parse_expression(0, Associativity::Left)?;
            elements.push(element);
            if self.eat(TokenType::Semi) {
                if elements.len() != 1 {
                    self.report_error(ParserError::ArrayWithSpecifiedSizeMoreThanOneElement(
                        location,
                    ));
                    return Err(());
                }
                size_init = true;
                break;
            }
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        if size_init {
            let size = self.expect(TokenType::LiteralInteger)?;
            let size = match size.value.parse::<usize>() {
                Ok(s) => {
                    if s == 0 {
                        self.report_error(ParserError::InvalidArraySize(
                            size.location,
                        ));
                        return Err(());
                    }
                    s
                },
                Err(_) => {
                    self.report_error(ParserError::InvalidArraySize(
                        size.location,
                    ));
                    return Err(());
                }
            };
            self.expect(TokenType::ClosingSquare)?;
            debug_assert!(elements.len() == 1);
            // The Codegen is responsible for copying the elements to the correct size
            // That's an optimization because we only need to type check the first element
            Ok(nodes::ArrayLiteralNode {
                location,
                elements,
                typ: Type::Array(Box::new(Type::Unknown), size),
                size: Some(size),
            })
        } else {
            self.expect(TokenType::ClosingSquare)?;
            Ok(nodes::ArrayLiteralNode {
                location,
                typ: Type::Array(Box::new(Type::Unknown), elements.len()),
                elements,
                size: None,
            })
        }
    }

    #[trace_call(always)]
    fn parse_expr_bool_literal(&mut self)-> Result<nodes::LiteralNode, ()> {
        let location = self.current_location();
        let bool_token = if self.at(TokenType::KeywordFalse) {
            let bool_token = self.expect(TokenType::KeywordFalse)?;
            bool_token
        } else {
            let bool_token = self.expect(TokenType::KeywordTrue)?;
            bool_token
        };
        Ok(nodes::LiteralNode {
            location,
            value: bool_token.value,
            typ: Type::Bool,
        })
    }

    #[trace_call(always)]
    fn parse_expr_char_literal(&mut self)-> Result<nodes::LiteralNode, ()> {
        let location = self.current_location();
        let char_token = self.expect(TokenType::LiteralChar)?;
        Ok(nodes::LiteralNode {
            location,
            value: char_token.value,
            typ: Type::Char,
        })
    }

    #[trace_call(always)]
    fn parse_expr_str_literal(&mut self)-> Result<nodes::LiteralNode, ()> {
        let location = self.current_location();
        let str_token = self.expect(TokenType::LiteralString)?;
        Ok(nodes::LiteralNode {
            location,
            value: str_token.value,
            typ: Type::Ref(Box::new(Type::Str), false),
        })
    }

    #[trace_call(always)]
    fn parse_expr_int_literal(&mut self)-> Result<nodes::LiteralNode, ()> {
        let number_token = self.expect(TokenType::LiteralInteger)?;
        let (value, typ, location) = self.parse_type_literal(number_token);
        Ok(nodes::LiteralNode {
            location,
            value,
            typ,
        })
    }

    #[trace_call(always)]
    fn parse_expr_name(&mut self, ident: Token)-> Result<nodes::NameNode, ()> {
        let name = ident.value;
        Ok(nodes::NameNode {
            location: ident.location,
            name,
            typ: Type::Unknown
        })
    }

    #[trace_call(always)]
    fn parse_expr_function_call(&mut self, ident: Token)-> Result<nodes::CallNode, ()> {
        let location = ident.location;
        let function_name = ident.value;

        self.expect(TokenType::OpenRound)?;
        let arguments = self.parse_arguments()?;
        self.expect(TokenType::ClosingRound)?;

        let is_extern = self.known_externs.contains(&function_name);

        Ok(nodes::CallNode {
            is_extern,
            function_name,
            module_path: None,
            location,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_arguments(&mut self)-> Result<Vec<nodes::Expression>, ()> {
        let mut arguments = Vec::new();
        while !self.parsed_eof() && !self.at(TokenType::ClosingRound) {
            let arg = self.parse_expression(0, Associativity::Left)?;
            arguments.push(arg);
            if !self.eat(TokenType::Comma) {
                break;
            }
        }
        Ok(arguments)
    }

    #[trace_call(always)]
    fn parse_type_node(&mut self)-> Result<nodes::TypeNode, ()> {
        let location = self.current_location();
        if self.eat(TokenType::DoubleAmpersand) {
            let is_mutable = self.eat(TokenType::KeywordMut);
            let typ = self.parse_type_node()?;
            let typ = Type::Ref(Box::new(typ.typ), is_mutable);
            let typ = Type::Ref(Box::new(typ), is_mutable);
            Ok(nodes::TypeNode {
                location,
                typ,
                module_path: None
            })
        } else if self.eat(TokenType::Ampersand) {
            let is_mutable = self.eat(TokenType::KeywordMut);
            let typ = self.parse_type_node()?;
            let typ = Type::Ref(Box::new(typ.typ), is_mutable);
            Ok(nodes::TypeNode {
                location,
                typ,
                module_path: None
            })
        } else if self.eat(TokenType::OpenSquare) {
            let typ = self.parse_type_node()?;
            self.expect(TokenType::Semi)?;
            let size = self.expect(TokenType::LiteralInteger)?;
            self.expect(TokenType::ClosingSquare)?;
            let size = match size.value.parse::<usize>() {
                Ok(size) => size,
                Err(_) => {
                    self.report_error(ParserError::InvalidArraySize(
                        size.location,
                    ));
                    return Err(());
                }
            };
            Ok(nodes::TypeNode {
                location,
                typ: Type::Array(Box::new(typ.typ), size),
                module_path: None
            })
        } else {
            let name_token = self.expect(TokenType::Identifier)?;
            if self.eat(TokenType::DoubleColon) {
                let mut sub_type = self.parse_type_node()?;
                let Type::Struct(strukt, _) = &sub_type.typ else {
                    internal_panic!("parse_type_node() returned invalid submodule type")
                };
                attach_module_specifier!(sub_type, name_token.value);
                sub_type.typ = Type::Struct(strukt.clone(), sub_type.module_path.clone());
                return Ok(sub_type)
            }
            let typ = self.parse_type_str(&name_token.value);
            Ok(nodes::TypeNode {
                location,
                typ,
                module_path: None
            })
        }
    }
}