use std::collections::{HashSet, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;

use super::lexer::Lexer;
use super::nodes::{self, CompilerFlag};
use crate::compiler::{ERR_STR, NOTE_STR, WARN_STR};
use crate::internal_panic;
use crate::frontend::tokens::*;
use crate::middleend::type_checker::Type;
use crate::util::flags::Flags;
use tracer::{trace_call, trace_panic};

pub enum ParserError<'src> {
    UnexpectedEOF(Location),
    UnexpectedTokenSingle(Location, TokenType, TokenType),
    // Syntax: Expected, Found
    UnexpectedTokenMany(Location, Vec<TokenType>, TokenType),
    ExpectedExpression(Location, TokenType),
    ExpectedUnaryOperator(Location, TokenType),
    ThisParameterHasType(Location),
    ThisParameterNotFirst(Location),
    ForbiddenThisParameter(Location),
    ThisOutsideClass(Location),
    InvalidCompilerFlag(String),
    InvalidArraySize(Location),
    ArrayWithSpecifiedSizeMoreThanOneElement(Location),
    InvalidCharLiteral(Location, &'src str),
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let error_msg = match self {
            Self::UnexpectedEOF(loc) => format!("{loc:?}: Unexpected End Of File while parsing."),
            Self::UnexpectedTokenSingle(l, expected, found) => format!(
                "{l:?}: Expected {}, found {}",
                expected, found
            ),
            Self::UnexpectedTokenMany(l, expected, found) =>  {
                let mut expected_str = String::from("");
                for (i, e) in expected.iter().enumerate() {
                    expected_str.push_str(&format!("{}", e));
                    if expected.len() > 2 && i == expected.len() - 2 {
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
            Self::ExpectedExpression(l, e) => format!("{l:?}: Expected Expression, found {}", e),
            Self::ExpectedUnaryOperator(l, t) => format!("{l:?}: Expected Unary Operator, found {}", t),
            Self::ThisParameterHasType(l) => format!("{l:?}: Unexpected type for `this` parameter.\n{}: The type of `this` is always the struct the method is defined in.", NOTE_STR),
            Self::ThisParameterNotFirst(l) => format!("{l:?}: `this` parameter must be the first parameter of a method."),
            Self::ForbiddenThisParameter(l) => format!("{l:?}: Unexpected `this` parameter.\n{}: `this` parameters are only allowed in methods.", NOTE_STR),
            Self::ThisOutsideClass(l) => format!("{l:?}: Unexpected `this` outside of a struct.\n{}: `this` is only allowed in methods.", NOTE_STR),
            Self::InvalidCompilerFlag(s) => format!("{}", s),
            Self::InvalidArraySize(l) => format!("{l:?}: Invalid array size."),
            Self::ArrayWithSpecifiedSizeMoreThanOneElement(l) => format!("{l:?}: Arrays with a specified size can only have one element."),
            Self::InvalidCharLiteral(loc, lit) => format!("{loc:?}: Invalid character literal `{lit}`."),
        };
        let message = format!("{}: {}", ERR_STR, error_msg);
        write!(f, "{}", message)
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
    As,
    Reference,
    Dereference,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::IndexedAccess => write!(f, "[]"),
            Self::MemberAccess => write!(f, "."),
            Self::As => write!(f, "{KEYWORD_AS}"),
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
            Self::Reference => write!(f, "&"),
            Self::Dereference => write!(f, "*"),
        }
    }
}

impl Operation {
    #[trace_call(extra)]
    pub fn from(s: &str) -> Option<Self> {
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
            "&&" => Some(Self::LogicalAnd),
            "||" => Some(Self::LogicalOr),
            "!" => Some(Self::LogicalNot),
            "[" => Some(Self::IndexedAccess),
            "." => Some(Self::MemberAccess),
            KEYWORD_AS => Some(Self::As),
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

pub struct Parser<'flags, 'lexer, 'src> {
    filemarkers: VecDeque<(usize, usize)>,
    parsed_files: HashSet<PathBuf>,
    lexer: &'lexer mut Lexer<'src>,
    current_function: Option<&'src str>,
    current_struct: Option<&'src str>,
    known_externs: Vec<&'src str>,
    errors: Vec<ParserError<'src>>,
    bracket_level: i32,
    flags: &'flags Flags,
}

impl<'flags: 'src, 'lexer, 'src> Parser<'flags, 'lexer, 'src> {
    // ---------- Start of Builder Pattern ----------
    pub fn new(flags: &'flags Flags, lexer: &'lexer mut Lexer<'src>, source: &'src str) -> Self {
        lexer.load(&source);
        Self {
            filemarkers: VecDeque::new(),
            parsed_files: HashSet::new(),
            lexer,
            current_function: None,
            current_struct: None,
            known_externs: Vec::new(),
            errors: Vec::new(),
            bracket_level: 0,
            flags,
        }
    }

    // ---------- End of Builder Pattern ----------
    // ---------- Start of Lexer ----------
    #[trace_call(extra)]
    fn get_location(&self) -> Location {
        let mut loc = self.lexer.get_location();
        let (file, offset) = self.filemarkers[self.filemarkers.len() - 1];
        loc.file_id = file;
        loc.byte -= offset;
        loc
    }
    
    fn fix_token_location(&self, token: &mut Token<'src>) {
        // FIXME: Locations are still wrong
        let (file, offset) = self.filemarkers[self.filemarkers.len() - 1];
        token.location.file_id = file;
        token.location.byte -= offset;
    }

    // ---------- End of Lexer ----------
    // ---------- Start of Parser Utility ----------
    #[trace_call(always)]
    fn parse_type_str(&self, val: &'src str) -> Type<'src> {
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
            // Reserved for future use
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Struct(val),
        }
    }
    #[trace_call(always)]
    fn parse_type_literal(&self, lit_tkn: Token<'src>)-> (&'src str, Type<'src>, Location) {
        let lit = lit_tkn.value;
        let loc = lit_tkn.location;
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ = self.parse_type_str(&lit[index..]);
                (&lit[0..index], typ, loc)
            }
            None => (lit, Type::Unknown, loc),
        }
    }

    fn parsed_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
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
    fn at(&mut self, token_type: TokenType) -> bool {
        let Some(tkn) = self.peek() else {
            return false;
        };
        tkn.token_type == token_type
    }

    #[trace_call(extra)]
    fn peek(&mut self) -> Option<Token<'src>> {
        self.lexer.peek()
    }

    #[trace_call(extra)]
    fn next(&mut self) -> Option<Token<'src>> {
        match self.lexer.next() {
            None => None,
            Some(mut tkn) => {
                if self.flags.debug {
                    println!("[DEBUG] Parser::next() -> {tkn:?}");
                }
                if tkn.token_type.is_opening_bracket() {
                    self.bracket_level += 1;
                } else if tkn.token_type.is_closing_bracket() {
                    self.bracket_level -= 1;
                }
                if !self.filemarkers.is_empty() {
                    self.fix_token_location(&mut tkn);
                }
                Some(tkn)
            }
        }
    }

    fn handle_filemarker(&mut self) {
        let filemarker = self.next().unwrap();
        assert!(filemarker.token_type == TokenType::KeywordFilemarker);
        let marker = self.next().unwrap();
        assert!(marker.token_type == TokenType::Identifier);
        let name = self.next().unwrap();
        assert!(name.token_type == TokenType::LiteralString);
        let filename = PathBuf::from(name.value);
        let id = Location::add_or_get_filename(&filename);
        match marker.value {
            "START" => {
                let offset = filemarker.location.byte
                    + filemarker.value.len()
                    + marker.value.len()
                    + name.value.len()
                    + 2 // spaces between tokens
                    + 2;
                self.filemarkers.push_back((id, offset));
                if self.flags.debug {
                    println!("[DEBUG] FILEMARKER START {id}");
                }
            },
            "END" => {
                let last = self.filemarkers.pop_back();
                let mapped = last.map(|(v, _)|v).unwrap();
                if self.flags.debug {
                    println!("[DEBUG] FILEMARKER END {mapped}");
                }
                debug_assert!(mapped == id)
            },
            m => internal_panic!("Internal Marker has unknown state {m}")
        }
    }

    #[trace_call(always)]
    fn expect(&mut self, token_type: TokenType)-> Result<Token<'src>, ()> {
        // REVIEW / FIXME: Why don't we return &Token?
        let Some(mut tkn) = self.peek() else {
            self.report_error(ParserError::UnexpectedEOF(self.get_location()));
            return Err(());
        };
        if tkn.token_type == TokenType::KeywordFilemarker {
            self.handle_filemarker();
            return self.expect(token_type);
        }
        self.fix_token_location(&mut tkn);
        if tkn.token_type != token_type {
            self.report_error(ParserError::UnexpectedTokenSingle(tkn.get_location(), token_type, tkn.token_type));
            Err(())
        } else {
            self.next().expect("We just peeked Some(Token)");
            Ok(tkn)
        }
    }

    #[trace_call(always)]
    fn report_error(&mut self, error: ParserError<'src>) {
        if self.flags.debug {
            println!("[DEBUG] Error: {}", error);
        }
        trace_panic!();
        self.errors.push(error);
    }

    #[trace_call(always)]
    fn recover(&mut self, tokens: &[TokenType]) {
        // Skip tokens until we find a recovery point
        // FIXME: I don't think bracket_level is working properly
        let bracket_level = self.bracket_level;
        while let Some(tkn) = self.peek() {
            if tokens.contains(&tkn.token_type) && self.bracket_level - 1 <= bracket_level {
                break;
            }
            if self.flags.debug {
                println!(
                    "[DEBUG] Recovering from {:?} at {:?}",
                    tkn,
                    self.get_location()
                );
                println!("[DEBUG] Recover tokens: {tokens:?}");
            }
            self.next();
        }
        if let Some(tkn) = self.next() {
            assert!(tokens.contains(&tkn.token_type));
            if self.flags.debug {
                println!("[DEBUG] Continuing at {:?}", tkn);
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
    pub fn parse_project(&mut self) -> Result<nodes::FileNode, String> {
        let root_path = PathBuf::from(&self.flags.input);
        self.parsed_files.insert(root_path.clone());
        let root = self.parse_file(&root_path);
        if !self.errors.is_empty() || root.is_err() {
            return Err(self.stringify_errors());
        }
        let root = root.unwrap();
        assert!(self.errors.is_empty());
        Ok(root)
    }

    #[trace_call(always)]
    fn parse_file(&mut self, filepath: &PathBuf) -> Result<nodes::FileNode<'src>, ()> {
        if self.flags.verbose {
            println!("[INFO] Parsing `{}`", filepath.to_str().unwrap());
        }
        let mut globals = vec![];
        let mut externs = vec![];
        let mut structs = vec![];
        let mut functions = vec![];

        const RECOVER_TOKENS: [TokenType; 3] = [
            TokenType::ClosingCurly,
            TokenType::Semi,
            TokenType::Eof,
        ];
        let mut valid = true;
        let mut compiler_flags = self.parse_compiler_flags()?;
        while let Some(tkn) = self.peek() {
           match tkn.token_type {
                TokenType::KeywordFilemarker => self.handle_filemarker(),
                TokenType::KeywordCompilerFlags => {
                    let Ok(flags) = self.parse_compiler_flags() else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    compiler_flags.flags.extend(flags.flags);
                }
                TokenType::KeywordExtern => {
                    let Ok(parsed_extern) = self.parse_extern(false) else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    self.known_externs.push(parsed_extern.name);
                    externs.push(parsed_extern);
                }
                TokenType::KeywordFunc => {
                    let Ok(parsed_function) = self.parse_function(false, false) else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    functions.push(parsed_function);
                }
                TokenType::KeywordImport => {
                    todo!("Error: Imports are handled by the Preprocessor.")
                }
                TokenType::KeywordStruct => {
                    let Ok(parsed_struct) = self.parse_struct() else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    structs.push(parsed_struct);
                }
                t @ TokenType::KeywordMut | t @ TokenType::KeywordLet => {
                    self.expect(t)?;
                    let Ok(parsed_global) = self.parse_stmt_var_decl(t == TokenType::KeywordMut, false) else {
                        self.recover(&RECOVER_TOKENS);
                        valid = false;
                        continue;
                    };
                    globals.push(parsed_global);
                }
                TokenType::KeywordComptime => {
                    self.expect(TokenType::KeywordComptime)?;
                    let Some(tkn) = self.peek() else {
                        self.report_error(ParserError::UnexpectedEOF(self.get_location()));
                        return Err(());
                    };
                    match tkn.token_type {
                        TokenType::Identifier => {
                            let Ok(parsed_global) = self.parse_stmt_var_decl(false, true) else {
                                self.recover(&RECOVER_TOKENS);
                                valid = false;
                                continue;
                            };
                            globals.push(parsed_global);
                        }
                        TokenType::KeywordFunc => {
                            let Ok(parsed_function) = self.parse_function(false, true) else {
                                self.recover(&RECOVER_TOKENS);
                                valid = false;
                                continue;
                            };
                            functions.push(parsed_function);
                        }
                        _ => {
                            let tkn = self.next().unwrap();
                            valid = false;
                            self.report_error(ParserError::UnexpectedTokenMany(
                                tkn.location,
                                vec![TokenType::KeywordFunc, TokenType::Identifier],
                                tkn.token_type,
                            ));
                            self.recover(&RECOVER_TOKENS);
                        }
                    }
                }
                TokenType::KeywordUnsafe => {
                    self.expect(TokenType::KeywordUnsafe)?;
                    let Some(tkn) = self.peek() else {
                        self.report_error(ParserError::UnexpectedEOF(self.get_location()));
                        return Err(());
                    };
                    match tkn.token_type {
                        TokenType::KeywordComptime => {
                            self.expect(TokenType::KeywordComptime)?;
                            let Some(tkn) = self.peek() else {
                                self.report_error(ParserError::UnexpectedEOF(self.get_location()));
                                return Err(());
                            };
                            match tkn.token_type {
                                TokenType::KeywordFunc => {
                                    let Ok(parsed_function) = self.parse_function(true, true) else {
                                        self.recover(&RECOVER_TOKENS);
                                        valid = false;
                                        continue;
                                    };
                                    functions.push(parsed_function);
                                }
                                _ => {
                                    let tkn = self.next().unwrap();
                                    valid = false;
                                    self.report_error(ParserError::UnexpectedTokenSingle(
                                        tkn.location,
                                        TokenType::KeywordFunc,
                                        tkn.token_type,
                                    ));
                                    self.recover(&RECOVER_TOKENS);
                                }
                            }
                        }
                        TokenType::KeywordFunc => {
                            let Ok(parsed_function) = self.parse_function(true, false) else {
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
                            self.known_externs.push(parsed_extern.name);
                            externs.push(parsed_extern);
                        }
                        _ => {
                            let tkn = self.next().unwrap();
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
                    let tkn = self.next().unwrap();
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
        if !valid {
            return Err(());
        }

        let file = nodes::FileNode {
            globals,
            externs,
            structs,
            functions,
            compiler_flags
        };
        Ok(file)
    }

    #[trace_call(always)]
    fn parse_compiler_flags(&mut self)-> Result<nodes::CompilerFlagsNode<'src>, ()> {
        let mut compiler_flags = vec![];
        if self.eat(TokenType::KeywordCompilerFlags) {
            let location = self.get_location();
            self.expect(TokenType::OpenCurly)?;
            while !self.parsed_eof() && !self.at(TokenType::ClosingCurly) {
                let flag = self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Colon)?;
                let value = self.expect(TokenType::LiteralString)?;
                self.expect(TokenType::Semi)?;
                let comp_flag = CompilerFlag::from(flag.location, flag.value, value.value);
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

    // #[trace_call(always)]
    fn parse_extern(&mut self, is_unsafe: bool)-> Result<nodes::ExternNode<'src>, ()> {
        let location = self.get_location();
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
    fn parse_struct(&mut self)-> Result<nodes::StructNode<'src>, ()> {
        let location = self.get_location();
        self.expect(TokenType::KeywordStruct)?;

        let struct_name = self.expect(TokenType::Identifier)?;
        let name = struct_name.value;
        if !name.as_bytes()[0].is_ascii_uppercase() {
            eprintln!("{}: {:?}: Struct names must start with an uppercase letter.", WARN_STR, struct_name.location);
        }

        self.current_struct = Some(name);
        self.expect(TokenType::OpenCurly)?;
        let mut fields = vec![];
        let mut methods = vec![];
        let mut valid = true;
        const RECOVER_TOKENS: [TokenType; 2] = [
            TokenType::ClosingCurly,
            TokenType::Semi,
        ];
        while let Some(tkn) = self.peek() {
            if tkn.token_type == TokenType::ClosingCurly {
                break;
            }
            match tkn.token_type {
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
                    let Some(tkn) = self.peek() else {
                        self.report_error(ParserError::UnexpectedEOF(self.get_location()));
                        return Err(());
                    };
                    match tkn.token_type {
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
                            let tkn = self.next().unwrap();
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
                        self.get_location(),
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
        })
    }

    // #[trace_call(always)]
    fn parse_field(&mut self)-> Result<nodes::FieldNode<'src>, ()> {
        let location = self.get_location();
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

    // #[trace_call(always)]
    fn parse_function(&mut self, is_unsafe: bool, is_comptime: bool) -> Result<nodes::FunctionNode<'src>, ()> {
        let location = self.get_location();
        self.expect(TokenType::KeywordFunc)?;

        let name = self.expect(TokenType::Identifier)?;

        self.current_function = Some(name.value);

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters(false)?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        let block = self.parse_block(is_unsafe)?;

        self.current_function = None;
        Ok(nodes::FunctionNode {
            location,
            name: name.value, 
            return_type,
            parameters,
            block,
            is_unsafe,
            is_vararg: false,
            is_comptime,
            #[cfg(feature = "old_codegen")]
            stack_size: 0,
        })
    }

    // #[trace_call(always)]
    fn parse_method(&mut self, struct_name: &'src str, is_unsafe: bool) -> Result<nodes::MethodNode<'src>, ()> {
        let location = self.get_location();
        self.expect(TokenType::KeywordFunc)?;

        let name = self.expect(TokenType::Identifier)?;

        self.current_function = Some(name.value);

        self.expect(TokenType::OpenRound)?;
        let parameters = self.parse_parameters(true)?;
        self.expect(TokenType::ClosingRound)?;

        let return_type = self.parse_return_type()?;

        let block = self.parse_block(is_unsafe)?;

        self.current_function = None;

        Ok(nodes::MethodNode {
            location,
            struct_name,
            name: name.value,
            return_type,
            parameters,
            block,
            is_unsafe,
            is_vararg: false,
            #[cfg(feature = "old_codegen")]
            stack_size: 0,
        })
    }

    // #[trace_call(always)]
    fn parse_return_type(&mut self)-> Result<nodes::TypeNode<'src>, ()> {
        if self.eat(TokenType::Arrow) {
            self.parse_type_node()
        } else {
            Ok(nodes::TypeNode::none(self.get_location()))
        }
    }

    // #[trace_call(always)]
    fn parse_parameters(&mut self, allowed_this: bool)-> Result<Vec<nodes::ParameterNode<'src>>, ()> {
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
    fn parse_single_parameter(&mut self, allowed_this: bool) -> Result<nodes::ParameterNode<'src>, ()> {
        let location = self.get_location();
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
                let struct_typ = Type::Struct(self.current_struct.as_ref().unwrap());
                let typ = if is_reference {
                    Type::Ref(Box::new(struct_typ), is_mutable)
                } else {
                    struct_typ
                };
                Ok(nodes::ParameterNode {
                    location,
                    name: "this",
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

    // #[trace_call(always)]
    fn parse_block(&mut self, is_unsafe: bool) -> Result<nodes::BlockNode<'src>, ()> {
        let location = self.get_location();
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

    // #[trace_call(always)]
    fn parse_statement(&mut self, is_unsafe: bool) -> Result<nodes::Statement<'src>, ()> {
        let Some(tkn) = self.peek() else {
            self.report_error(ParserError::UnexpectedEOF(self.get_location()));
            return Err(());
        };
        Ok(match tkn.token_type {
            TokenType::KeywordComptime => {
                self.expect(TokenType::KeywordComptime)?;
                let comp_stmt = self.parse_stmt_var_decl(false, true)?;
                nodes::Statement::VarDecl(comp_stmt)
            }
            TokenType::KeywordMut => {
                self.expect(TokenType::KeywordMut)?;
                let mut_stmt = self.parse_stmt_var_decl(true, false)?;
                nodes::Statement::VarDecl(mut_stmt)
            }
            TokenType::KeywordLet => {
                self.expect(TokenType::KeywordLet)?;
                let let_stmt = self.parse_stmt_var_decl(false, false)?;
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

    // #[trace_call(always)]
    fn parse_stmt_var_decl(&mut self, is_mutable: bool, is_comptime: bool)-> Result<nodes::VarDeclNode<'src>, ()> {
        let location = self.get_location();
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
            is_comptime,
        })
    }

    // #[trace_call(always)]
    fn parse_stmt_for(&mut self, is_unsafe: bool) -> Result<nodes::Statement<'src>, ()> {
        let location = self.get_location();
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
            let desugared_while = nodes::WhileNode {
                location,
                condition: condition.unwrap_or_else(|| nodes::Expression::Literal(nodes::LiteralNode {
                    location: location.clone(),
                    value: "true",
                    typ: Type::Bool,
                })),
                body,
                step: increment,
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

    // #[trace_call(always)]
    fn parse_stmt_if(&mut self, is_unsafe: bool)-> Result<nodes::IfNode<'src>, ()> {
        let location = self.get_location();
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

    // #[trace_call(always)]
    fn parse_stmt_return(&mut self)-> Result<nodes::ReturnNode<'src>, ()> {
        let location = self.get_location();
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

    // #[trace_call(always)]
    fn parse_stmt_while(&mut self, is_unsafe: bool)-> Result<nodes::WhileNode<'src>, ()> {
        let location = self.get_location();
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
            body,
            step: None,
        })
    }

    #[trace_call(always)]
    fn parse_stmt_break(&mut self)-> Result<nodes::BreakNode, ()> {
        let location = self.get_location();
        self.expect(TokenType::KeywordBreak)?;
        self.expect(TokenType::Semi)?;
        Ok(nodes::BreakNode { location })
    }

    #[trace_call(always)]
    fn parse_stmt_continue(&mut self)-> Result<nodes::ContinueNode, ()> {
        let location = self.get_location();
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
            TokenType::KeywordAs => 13,
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
            TokenType::KeywordAs => Associativity::Left,
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
    // #[trace_call(always)]
    fn parse_expression(&mut self, min_precedence: usize, associativity: Associativity)-> Result<nodes::Expression<'src>, ()> {
        let mut expression = self.parse_primary_expression()?;

        while self.matches_binary_expression() {
            let tkn_type = self.peek().expect("We matched a binary expression").token_type;
            let new_precedence = self.get_binary_precedence(tkn_type);
            if new_precedence < min_precedence {
                break;
            }
            if new_precedence == min_precedence && associativity == Associativity::Left {
                break;
            }
            let new_associativity = self.get_associativity(tkn_type);
            let result = self.parse_secondary_expression(expression, new_precedence, new_associativity)?;
            expression = result;
        }

        Ok(expression)
    }

    // #[trace_call(always)]
    fn parse_primary_expression(&mut self)-> Result<nodes::Expression<'src>, ()> {
        // let location = self.get_location();
        if self.eat(TokenType::KeywordSizeof) {
            let typ = self.parse_type_node()?;
            return Ok(nodes::Expression::Sizeof(typ));
        }
        if self.matches_unary_expression() {
            let unary = self.parse_unary_expression()?;
            return Ok(nodes::Expression::Unary(unary));
        }
        let Some(tkn) = self.peek() else {
            self.report_error(ParserError::UnexpectedEOF(self.get_location()));
            return Err(());
        };
        match tkn.token_type {
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
            TokenType::KeywordNull => {
                let null_token = self.next().expect("We just checked EOF a few lines higher");
                let null_literal = nodes::LiteralNode {
                    location: null_token.location,
                    value: "null",
                    typ: Type::Any,
                };
                Ok(nodes::Expression::Literal(null_literal))
            }
            TokenType::KeywordBlank => {
                let blank_token = self.next().expect("We just checked EOF a few lines higher");
                let blank_literal = nodes::LiteralNode {
                    location: blank_token.location,
                    value: "blank",
                    typ: Type::Blank,
                };
                Ok(nodes::Expression::Literal(blank_literal))
            }
            TokenType::KeywordThis => {
                let this_token = self.next().expect("We just checked EOF a few lines higher");
                if self.current_struct.is_none() {
                    self.report_error(ParserError::ThisOutsideClass(
                        this_token.location,
                    ));
                    return Err(());
                }
                let this_literal = nodes::NameNode {
                    location: this_token.location,
                    name: "this",
                    typ: Type::Struct(self.current_struct.as_ref().unwrap()),
                };
                Ok(nodes::Expression::Name(this_literal))
            }
            e => {
                self.report_error(ParserError::ExpectedExpression(
                    self.get_location(),
                    e
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn matches_unary_expression(&mut self) -> bool {
        // TOKEN_TYPE_HANDLE_HERE
        let Some(tkn) = self.peek() else {
            return false
        };
        match tkn.token_type {
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
        let Some(tkn) = self.peek() else {
            return false
        };
        match tkn.token_type {
            TokenType::KeywordAs => true,
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
        lhs: nodes::Expression<'src>,
        precedence: usize,
        associativity: Associativity,
    )-> Result<nodes::Expression<'src>, ()> {
        let location = self.get_location();
        debug_assert!(self.matches_binary_expression());
        let op_token = self.next().expect("Operator in parse_secondary_expression() is safe");
        let op = Operation::from(&op_token.value).expect("matches_binary_expression() is true");
        if op == Operation::As {
            let typ = self.parse_type_node()?;
            return Ok(nodes::Expression::As(Box::new(lhs), typ))
        }
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
    fn parse_unary_expression(&mut self)-> Result<nodes::UnaryNode<'src>, ()> {
        let location = self.get_location();
        let Some(tkn) = self.peek() else {
            self.report_error(ParserError::UnexpectedEOF(location));
            return Err(());
        };
        let precedence = self.get_unary_precedence(tkn.token_type);
        match tkn.token_type {
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
            tt => {
                self.report_error(ParserError::ExpectedUnaryOperator(
                    self.get_location(),
                    tt,
                ));
                Err(())
            }
        }
    }

    // #[trace_call(always)]
    fn parse_expr_identifier(&mut self)-> Result<nodes::Expression<'src>, ()> {
        let ident = self.expect(TokenType::Identifier)?;
        if self.at(TokenType::OpenRound) {
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
    fn parse_expr_struct_literal(&mut self, ident: Token<'src>)-> Result<nodes::StructLiteralNode<'src>, ()> {
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
            struct_name: ident.value,
            fields,
            typ: Type::Struct(ident.value),
        })
    }

    #[trace_call(always)]
    fn parse_expr_array_literal(&mut self) -> Result<nodes::ArrayLiteralNode<'src>, ()> {
        let location = self.get_location();
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
    fn parse_expr_bool_literal(&mut self)-> Result<nodes::LiteralNode<'src>, ()> {
        let location = self.get_location();
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
    fn parse_expr_char_literal(&mut self)-> Result<nodes::LiteralNode<'src>, ()> {
        let location = self.get_location();
        let char_token = self.expect(TokenType::LiteralChar)?;
        let ch = char_token.value;
        if !((ch.starts_with('\\') && ch.len() == 2) || ch.len() == 1) {
            self.report_error(ParserError::InvalidCharLiteral(
                char_token.location,
                char_token.value
            ));
            return Err(());
        }
        Ok(nodes::LiteralNode {
            location,
            value: char_token.value,
            typ: Type::Char,
        })
    }

    #[trace_call(always)]
    fn parse_expr_str_literal(&mut self)-> Result<nodes::LiteralNode<'src>, ()> {
        let location = self.get_location();
        let str_token = self.expect(TokenType::LiteralString)?;
        Ok(nodes::LiteralNode {
            location,
            value: str_token.value,
            typ: Type::Ref(Box::new(Type::Char), false),
        })
    }

    #[trace_call(always)]
    fn parse_expr_int_literal(&mut self)-> Result<nodes::LiteralNode<'src>, ()> {
        let number_token = self.expect(TokenType::LiteralInteger)?;
        let (value, typ, location) = self.parse_type_literal(number_token);
        Ok(nodes::LiteralNode {
            location,
            value,
            typ,
        })
    }

    #[trace_call(always)]
    fn parse_expr_name(&mut self, ident: Token<'src>)-> Result<nodes::NameNode<'src>, ()> {
        let name = ident.value;
        Ok(nodes::NameNode {
            location: ident.location,
            name,
            typ: Type::Unknown
        })
    }

    // #[trace_call(always)]
    fn parse_expr_function_call(&mut self, ident: Token<'src>)-> Result<nodes::CallNode<'src>, ()> {
        let location = ident.location;
        let function_name = ident.value;

        self.expect(TokenType::OpenRound)?;
        let arguments = self.parse_arguments()?;
        self.expect(TokenType::ClosingRound)?;

        Ok(nodes::CallNode {
            is_extern: false,
            function_name,
            location,
            arguments,
            typ: Type::Unknown,
        })
    }

    #[trace_call(always)]
    fn parse_arguments(&mut self)-> Result<Vec<nodes::Expression<'src>>, ()> {
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
    fn parse_type_node(&mut self)-> Result<nodes::TypeNode<'src>, ()> {
        let location = self.get_location();
        if self.eat(TokenType::DoubleAmpersand) {
            let is_mutable = self.eat(TokenType::KeywordMut);
            let typ = self.parse_type_node()?;
            let typ = Type::Ref(Box::new(typ.typ), is_mutable);
            let typ = Type::Ref(Box::new(typ), is_mutable);
            Ok(nodes::TypeNode {
                location,
                typ,
            })
        } else if self.eat(TokenType::Ampersand) {
            let is_mutable = self.eat(TokenType::KeywordMut);
            let typ = self.parse_type_node()?;
            let typ = Type::Ref(Box::new(typ.typ), is_mutable);
            Ok(nodes::TypeNode {
                location,
                typ,
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
            })
        } else {
            let name_token = self.expect(TokenType::Identifier)?;
            let typ = self.parse_type_str(&name_token.value);
            Ok(nodes::TypeNode {
                location,
                typ,
            })
        }
    }
}
