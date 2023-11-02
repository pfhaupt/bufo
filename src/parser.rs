use std::cell::Cell;
use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::fs;

use crate::checker::{Type, TypeChecker};
use crate::codegen::ERR_STR;

const LOOKAHEAD_LIMIT: usize = 3;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TreeType {
    File {
        functions: Vec<Tree>,
        classes: Vec<Tree>,
    },
    Class {
        name: String,
        fields: Vec<Tree>,
        functions: Vec<Tree>,
        features: Vec<Tree>,
    },
    Field {
        name: String,
        typ: Box<Tree>,
    },
    FieldAccess {
        name: String,
        field: Box<Tree>,
        typ: Type,
    },
    Feature {
        name: String,
        return_type: Option<Box<Tree>>,
        param: Box<Tree>,
        block: Box<Tree>,
    },
    Func {
        name: String,
        return_type: Option<Box<Tree>>,
        param: Box<Tree>,
        block: Box<Tree>,
    },
    Param {
        name: String,
        typ: Box<Tree>,
    },
    ParamList {
        parameters: Vec<Tree>,
    },
    Block {
        statements: Vec<Tree>,
    },
    StmtExpr {
        expression: Box<Tree>,
    },
    StmtLet {
        name: String,
        typ: Box<Tree>,
        expression: Box<Tree>,
    },
    StmtAssign {
        name: Box<Tree>,
        expression: Box<Tree>,
    },
    StmtIf {
        condition: Box<Tree>,
        if_branch: Box<Tree>,
        else_branch: Option<Box<Tree>>,
    },
    StmtReturn {
        return_value: Option<Box<Tree>>,
    },
    TypeDecl {
        typ: Type,
    },
    ArgList {
        arguments: Vec<Tree>,
    },
    Arg {
        expression: Box<Tree>,
    },
    ExprName {
        name: String,
        typ: Type,
    },
    ExprArrLiteral {
        elements: Vec<Tree>,
    },
    ExprArrAccess {
        arr_name: String,
        indices: Box<Tree>,
        typ: Type,
    },
    ExprLiteral {
        typ: Type,
    },
    ExprBinary {
        lhs: Box<Tree>,
        rhs: Box<Tree>,
        typ: Type,
    },
    ExprComp {
        lhs: Box<Tree>,
        rhs: Box<Tree>,
        typ: Type,
    },
    ExprParen {
        expression: Box<Tree>,
        typ: Type,
    },
    ExprCall {
        function_name: String,
        args: Box<Tree>,
        typ: Type,
    },
    BuiltInFunction {
        function_name: String,
        args: Box<Tree>,
        typ: Type
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree {
    pub typ: TreeType,
    pub tkn: Token,
}

impl Tree {
    #[allow(unused)]
    pub fn print_debug(&self) {
        self.print_internal(0);
    }

    fn print_internal(&self, indent: usize) {
        const EXTRA_INDENT: usize = 2;
        let tab = " ".repeat(indent);
        match &self.typ {
            TreeType::File { functions, classes } => {
                for c in classes {
                    c.print_internal(indent + EXTRA_INDENT);
                }
                for f in functions {
                    f.print_internal(indent + EXTRA_INDENT);
                }
            }
            TreeType::Class { name, fields, functions, features } => {
                println!("{tab}Class {name}");
                for f in fields {
                    f.print_internal(indent + EXTRA_INDENT);
                }
                for f in features {
                    f.print_internal(indent + EXTRA_INDENT);
                }
                for f in functions {
                    f.print_internal(indent + EXTRA_INDENT);
                }
            }
            TreeType::Field { name, typ } => {
                println!("{tab}Field {name}");
                typ.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::FieldAccess { name, field, typ } => {
                println!("{tab}FieldAccess {name} {typ:?}");
                field.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::Feature {
                name,
                return_type,
                param,
                block,
            } => {
                println!("{tab}Feature {name}");
                println!("{tab}Return", tab = " ".repeat(indent + EXTRA_INDENT));
                if let Some(s) = return_type {
                    s.print_internal(indent + 2 * EXTRA_INDENT);
                } else {
                    println!("{tab}None", tab = " ".repeat(indent + 2 * EXTRA_INDENT));
                }
                println!("{tab}Param", tab = " ".repeat(indent + EXTRA_INDENT));
                param.print_internal(indent + 2 * EXTRA_INDENT);
                println!("{tab}Block", tab = " ".repeat(indent + EXTRA_INDENT));
                block.print_internal(indent + 2 * EXTRA_INDENT);
            }
            TreeType::Func {
                name,
                return_type,
                param,
                block,
            } => {
                println!("{tab}Function {name}");
                println!("{tab}Return", tab = " ".repeat(indent + EXTRA_INDENT));
                if let Some(s) = return_type {
                    s.print_internal(indent + 2 * EXTRA_INDENT);
                } else {
                    println!("{tab}None", tab = " ".repeat(indent + 2 * EXTRA_INDENT));
                }
                param.print_internal(indent + EXTRA_INDENT);
                println!("{tab}Block", tab = " ".repeat(indent + EXTRA_INDENT));
                block.print_internal(indent + 2 * EXTRA_INDENT);
            }
            TreeType::ParamList { parameters } => {
                println!("{tab}ParamList");
                for p in parameters {
                    p.print_internal(indent + EXTRA_INDENT);
                }
            }
            TreeType::Param { name, typ } => {
                println!("{tab}Param {name}");
                typ.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::ArgList { arguments } => {
                for a in arguments {
                    a.print_internal(indent);
                }
            }
            TreeType::Arg { expression } => {
                println!("{tab}Arg");
                expression.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::Block { statements } => {
                for s in statements {
                    s.print_internal(indent);
                }
            }
            TreeType::StmtExpr { expression } => {
                expression.print_internal(indent);
            }
            TreeType::StmtLet {
                name,
                typ,
                expression,
            } => {
                println!("{tab}StmtLet {name}");
                typ.print_internal(indent + 2 * EXTRA_INDENT);
                expression.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::StmtAssign { name, expression } => {
                println!("{tab}StmtAssign");
                name.print_internal(indent + EXTRA_INDENT);
                expression.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::StmtIf {
                condition,
                if_branch,
                else_branch,
            } => {
                println!("{tab}StmtIf");
                println!("{tab}Condition", tab = " ".repeat(indent + EXTRA_INDENT));
                condition.print_internal(indent + 2 * EXTRA_INDENT);
                println!("{tab}If-Branch", tab = " ".repeat(indent + EXTRA_INDENT));
                if_branch.print_internal(indent + 2 * EXTRA_INDENT);
                if let Some(e) = else_branch {
                    println!("{tab}Else-Branch", tab = " ".repeat(indent + EXTRA_INDENT));
                    e.print_internal(indent + 2 * EXTRA_INDENT);
                }
            }
            TreeType::StmtReturn { return_value } => {
                println!("{tab}StmtReturn");
                if let Some(r) = return_value {
                    r.print_internal(indent + EXTRA_INDENT);
                }
            }
            TreeType::ExprName { name, typ } => {
                println!("{tab}ExprName {name} {typ:?}");
            }
            TreeType::ExprArrLiteral { elements } => {
                println!("{tab}ExprArrLiteral");
                for e in elements {
                    e.print_internal(indent + EXTRA_INDENT);
                }
            }
            TreeType::ExprArrAccess {
                arr_name,
                indices,
                typ,
            } => {
                println!("{tab}ArrAccess {arr_name} {typ:?}");
                indices.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::ExprLiteral { typ } => {
                println!("{tab}ExprLiteral {} {typ:?}", self.tkn.get_value());
            }
            TreeType::ExprBinary { lhs, rhs, typ } => {
                println!("{tab}ExprBinary {} {typ:?}", self.tkn.get_value());
                lhs.print_internal(indent + EXTRA_INDENT);
                rhs.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::ExprComp { lhs, rhs, typ } => {
                println!("{tab}ExprComp {} {typ:?}", self.tkn.get_value());
                lhs.print_internal(indent + EXTRA_INDENT);
                rhs.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::ExprParen { expression, typ } => {
                println!("{tab}ExprParen {typ:?}");
                expression.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::ExprCall {
                function_name,
                args,
                typ,
            } => {
                println!("{tab}ExprCall {function_name} {typ:?}");
                args.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::BuiltInFunction {
                function_name,
                args,
                typ,
            } => {
                println!("{tab}BuiltInFunction {function_name} {typ:?}");
                args.print_internal(indent + EXTRA_INDENT);
            }
            TreeType::TypeDecl { typ } => {
                println!("{tab}TypeDecl {typ:?}");
            }
        }
    }

    pub fn rebuild_code(&self) -> String {
        self.rebuild_code_internal(0)
    }

    fn rebuild_code_internal(&self, indent: usize) -> String {
        const EXTRA_INDENT: usize = 4;
        let tab = " ".repeat(indent);
        let mut result = String::new();
        match &self.typ {
            TreeType::File { functions, classes } => {
                for c in classes {
                    result.push_str(&c.rebuild_code_internal(indent));
                }
                for f in functions {
                    result.push_str(&f.rebuild_code_internal(indent));
                }
            }
            TreeType::Class { name, fields, functions, features } => {
                result.push_str(format!("class {name} {{\n").as_str());
                for f in fields {
                    result.push_str(&f.rebuild_code_internal(indent + EXTRA_INDENT));
                }
                for f in features {
                    result.push_str(&f.rebuild_code_internal(indent + EXTRA_INDENT));
                }
                for f in functions {
                    result.push_str(&f.rebuild_code_internal(indent + EXTRA_INDENT));
                }
                result.push_str("}\n");
            }
            TreeType::Field { name, typ } => {
                result.push_str(format!("{tab}{name}: ").as_str());
                result.push_str(&typ.rebuild_code());
                result.push_str(";\n");
            }
            TreeType::FieldAccess { name, field, .. } => {
                result.push_str(format!("{name}.").as_str());
                result.push_str(&field.rebuild_code());
            }
            TreeType::Feature {
                name,
                return_type,
                param,
                block,
            } => {
                result.push_str(format!("{tab}feat {name}(").as_str());
                result.push_str(&param.rebuild_code());
                result.push_str(")");
                if let Some(ret) = return_type {
                    result.push_str(" -> ");
                    result.push_str(&ret.rebuild_code());
                }
                result.push_str(" {\n");
                result.push_str(&block.rebuild_code_internal(indent));
                result.push_str(format!("{tab}}}\n").as_str());
            }
            TreeType::Func {
                name,
                return_type,
                param,
                block,
            } => {
                result.push_str(format!("{tab}func {name}(").as_str());
                result.push_str(&param.rebuild_code());
                result.push_str(")");
                if let Some(ret) = return_type {
                    result.push_str(" -> ");
                    result.push_str(&ret.rebuild_code());
                }
                result.push_str(" {\n");
                result.push_str(&block.rebuild_code_internal(indent));
                result.push_str(format!("{tab}}}\n").as_str());
            }
            TreeType::ParamList { parameters } => {
                if !parameters.is_empty() {
                    for p in parameters.iter().take(parameters.len() - 1) {
                        result.push_str(&p.rebuild_code());
                        result.push_str(", ");
                    }
                    result.push_str(&parameters.last().unwrap().rebuild_code());
                }
            }
            TreeType::Param { name, typ } => {
                result.push_str(format!("{name}: ").as_str());
                result.push_str(&typ.rebuild_code());
            }
            TreeType::ArgList { arguments } => {
                if !arguments.is_empty() {
                    for a in arguments.iter().take(arguments.len() - 1) {
                        result.push_str(&a.rebuild_code());
                        result.push_str(", ");
                    }
                    result.push_str(&arguments.last().unwrap().rebuild_code());
                }
            }
            TreeType::Arg { expression } => {
                result.push_str(&expression.rebuild_code());
            }
            TreeType::Block { statements } => {
                for s in statements {
                    result.push_str(&s.rebuild_code_internal(indent + EXTRA_INDENT));
                }
            }
            TreeType::StmtIf {
                condition,
                if_branch,
                else_branch,
            } => {
                result.push_str(format!("{tab}if (").as_str());
                result.push_str(&condition.rebuild_code());
                result.push_str(") {\n");
                result.push_str(&if_branch.rebuild_code_internal(indent));
                result.push_str(format!("{tab}}}").as_str());
                if let Some(els) = else_branch {
                    result.push_str(" else {\n");
                    result.push_str(&els.rebuild_code_internal(indent));
                    result.push_str(format!("{tab}}}\n").as_str());
                } else {
                    result.push('\n');
                }
            }
            TreeType::StmtReturn { return_value } => {
                result.push_str(format!("{tab}return").as_str());
                if let Some(ret) = return_value {
                    result.push_str(" ");
                    result.push_str(&ret.rebuild_code());
                }
                result.push_str(";\n");
            }
            TreeType::StmtLet {
                name,
                typ,
                expression,
            } => {
                result.push_str(format!("{tab}let {name}: ").as_str());
                result.push_str(&typ.rebuild_code());
                result.push_str(" = ");
                result.push_str(&expression.rebuild_code());
                result.push_str(";\n");
            }
            TreeType::StmtAssign { name, expression } => {
                result.push_str(format!("{tab}").as_str());
                result.push_str(&name.rebuild_code());
                result.push_str(" = ");
                result.push_str(&expression.rebuild_code());
                result.push_str(";\n");
            }
            TreeType::StmtExpr { expression } => {
                result.push_str(format!("{tab}").as_str());
                result.push_str(&expression.rebuild_code_internal(indent));
                result.push_str(";\n");
            }

            TreeType::ExprComp { lhs, rhs, .. } => {
                result.push_str(&lhs.rebuild_code());
                result.push_str(format!(" {} ", self.tkn.get_value()).as_str());
                result.push_str(&rhs.rebuild_code());
            }
            TreeType::ExprBinary { lhs, rhs, .. } => {
                result.push_str(&lhs.rebuild_code());
                result.push_str(format!(" {} ", self.tkn.get_value()).as_str());
                result.push_str(&rhs.rebuild_code());
            }
            TreeType::ExprParen { expression, .. } => {
                result.push_str("(");
                result.push_str(&expression.rebuild_code());
                result.push_str(")");
            }
            TreeType::ExprName { name, .. } => {
                result.push_str(format!("{name}").as_str());
            }
            TreeType::ExprLiteral { typ } => {
                if *typ != Type::Unknown {
                    result.push_str(format!("{}{}", self.tkn.get_value(), typ).as_str());
                } else {
                    result.push_str(format!("{}", self.tkn.get_value()).as_str());
                }
            }
            TreeType::ExprArrLiteral { elements } => {
                result.push_str("[");
                if !elements.is_empty() {
                    for e in elements.iter().take(elements.len() - 1) {
                        result.push_str(&e.rebuild_code());
                        result.push_str(", ");
                    }
                    result.push_str(&elements.last().unwrap().rebuild_code());
                }
                result.push_str("]");
            }
            TreeType::ExprArrAccess {
                arr_name, indices, ..
            } => {
                result.push_str(format!("{arr_name}").as_str());
                result.push_str(&indices.rebuild_code());
            }
            TreeType::ExprCall {
                function_name,
                args,
                ..
            } => {
                result.push_str(format!("{function_name}(").as_str());
                result.push_str(&args.rebuild_code());
                result.push_str(")");
            }
            TreeType::BuiltInFunction {
                function_name,
                args,
                ..
            } => {
                result.push_str(format!("{function_name}(").as_str());
                result.push_str(&args.rebuild_code());
                result.push_str(")");
            }
            TreeType::TypeDecl { typ } => {
                result.push_str(format!("{typ}").as_str());
            }
        }
        result
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    File,
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

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Location {
    file: String,
    row: usize,
    col: usize,
}

impl Location {
    pub fn new(file: String, row: usize, col: usize) -> Self {
        Self { file, row, col }
    }

    pub fn anonymous() -> Self {
        Self::new(String::from("anonymous"), 0, 0)
    }

    pub fn builtin() -> Self {
        Self::new(String::from("builtin"), 0, 0)
    }
}


impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    typ: TokenType, // type is a reserved keyword in Rust :(
    value: String,
    loc: Location,
}

impl Token {
    pub fn new(typ: TokenType, value: String, loc: Location) -> Self {
        Self { typ, value, loc }
    }

    pub fn get_type(&self) -> TokenType {
        self.typ
    }

    pub fn get_value(&self) -> String {
        self.value.clone()
    }

    pub fn get_loc(&self) -> Location {
        self.loc.clone()
    }
}

#[derive(Clone)]
pub struct Parser {
    origin: String,
    ptr: usize,
    fuel: Cell<u32>,
    root: Option<Tree>,
    #[allow(unused)]
    print_debug: bool,
    lookahead: VecDeque<Token>,
    // Lexer fields
    source: Vec<char>,
    current_char: usize,
    current_line: usize,
    line_start: usize,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            origin: String::new(),
            ptr: 0,
            fuel: Cell::new(256),
            root: None,
            print_debug: false,
            lookahead: VecDeque::new(),
            // Lexer fields
            source: vec![],
            current_char: 0,
            current_line: 1,
            line_start: 0,
        }
    }

    pub fn set_origin_unchecked(&mut self, origin_path: &String) {
        self.origin = origin_path.clone();
    }
    pub fn set_source(&mut self, source: &String) {
        self.source = source.chars().collect();
    }

    pub fn origin(&mut self, origin_path: &String) -> Result<Self, String> {
        match fs::read_to_string(origin_path) {
            Ok(source) => {
                self.source = source.chars().collect();
                self.origin = origin_path.to_string();
                Ok(self.clone())
            }
            Err(_) => {
                Err(format!(
                    "{}: Could not find input file `{}`.",
                    ERR_STR, origin_path
                ))
            }
        }
    }


    pub fn debug(&mut self, debug: bool) -> Self {
        self.print_debug = debug;
        self.clone()
    }

    // ---------- End of Builder Pattern ----------

    // ---------- Start of Lexer ----------
    fn get_location(&self) -> Location {
        Location {
            file: self.origin.clone(),
            row: self.current_line,
            col: self.current_char - self.line_start,
        }
    }

    fn lexer_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn parser_eof(&mut self) -> bool {
        self.nth(0).unwrap() == TokenType::Eof
    }

    fn next_char(&mut self) -> Result<char, String> {
        if self.lexer_eof() {
            Err(format!("{}: Reached End Of File while lexing.", ERR_STR))
        } else {
            let c = self.source[self.current_char];
            self.current_char += 1;
            Ok(c)
        }
    }

    fn trim_whitespace(&mut self) -> Result<(), String> {
        while !self.lexer_eof() && self.source[self.current_char].is_whitespace() {
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

    fn next_token(&mut self) -> Result<Token, String> {
        assert_eq!(
            TokenType::Eof as u8 + 1,
            36,
            "Not all TokenTypes are handled in next_token()"
        );
        self.trim_whitespace()?;
        let loc = self.get_location();
        let c = self.next_char();
        if c.is_err() {
            return Ok(Token {
                value: String::new(),
                typ: TokenType::Eof,
                loc
            });
        }
        let c = c.unwrap();
        match c {
            '0'..='9' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_alphanumeric() {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
                Ok(Token {
                    typ: TokenType::IntLiteral,
                    value,
                    loc,
                })
            }
            'A'..='Z' | 'a'..='z' => {
                let mut value = String::from(c);
                while let Ok(nc) = self.next_char() {
                    if !nc.is_alphanumeric() && !(nc == '_') {
                        self.current_char -= 1; // Went too far, go a step back
                        break;
                    }
                    value.push(nc);
                }
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
                Ok(Token { typ, value, loc })
            }
            '"' => {
                let mut value = String::new();
                while let Ok(nc) = self.next_char() {
                    if nc == '"' {
                        break;
                    }
                    value.push(nc);
                }
                Ok(Token {
                    typ: TokenType::StrLiteral,
                    value,
                    loc
                })
            }
            '\'' => {
                let mut value = String::new();
                while let Ok(nc) = self.next_char() {
                    if nc == '\'' {
                        break;
                    }
                    value.push(nc);
                }
                if value.len() != 1 {
                    Err(format!("{}: {:?}: Char Literal is expected to be a single char, got `{}`.",
                        ERR_STR,
                        loc,
                        value
                    ))
                } else {
                    Ok(Token {
                        typ: TokenType::CharLiteral,
                        value,
                        loc
                    })
                }
            } 
            '(' => Ok(Token {
                typ: TokenType::OpenRound,
                value: String::from("("),
                loc,
            }),
            ')' => Ok(Token {
                typ: TokenType::ClosingRound,
                value: String::from(")"),
                loc,
            }),
            '{' => Ok(Token {
                typ: TokenType::OpenCurly,
                value: String::from("{"),
                loc,
            }),
            '}' => Ok(Token {
                typ: TokenType::ClosingCurly,
                value: String::from("}"),
                loc,
            }),
            '[' => Ok(Token {
                typ: TokenType::OpenSquare,
                value: String::from("["),
                loc,
            }),
            ']' => Ok(Token {
                typ: TokenType::ClosingSquare,
                value: String::from("]"),
                loc,
            }),
            ';' => Ok(Token {
                typ: TokenType::Semi,
                value: String::from(";"),
                loc,
            }),
            ',' => Ok(Token {
                typ: TokenType::Comma,
                value: String::from(","),
                loc,
            }),
            '!' => {
                if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => Ok(Token {
                            typ: TokenType::CmpNeq,
                            value: String::from("!="),
                            loc,
                        }),
                        _ => {
                            let mut v = String::from(c);
                            v.push(nc);
                            Err(format!(
                                "{}: {:?}: Unexpected Symbol `{}`",
                                ERR_STR,
                                self.get_location(),
                                v
                            ))
                        }
                    }
                } else {
                    Err(format!("{}: Reached End Of File while lexing.", ERR_STR))
                }
            }
            '=' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpEq, String::from("==")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::Equal, String::from("="))
                        }
                    }
                } else {
                    (TokenType::Equal, String::from("="))
                };
                Ok(Token { typ, value, loc })
            }
            '<' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpLte, String::from("<=")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::CmpLt, String::from("<"))
                        }
                    }
                } else {
                    (TokenType::CmpLt, String::from("<"))
                };
                Ok(Token { typ, value, loc })
            }
            '>' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '=' => (TokenType::CmpGte, String::from(">=")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::CmpGt, String::from(">"))
                        }
                    }
                } else {
                    (TokenType::CmpGt, String::from(">"))
                };
                Ok(Token { typ, value, loc })
            }
            ':' => Ok(Token {
                typ: TokenType::Colon,
                value: String::from(c),
                loc,
            }),
            '.' => Ok(Token {
                typ: TokenType::Dot,
                value: String::from(c),
                loc,
            }),
            '+' => Ok(Token {
                typ: TokenType::Plus,
                value: String::from(c),
                loc,
            }),
            '-' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '>' => (TokenType::Arrow, String::from("->")),
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::Minus, String::from("-"))
                        }
                    }
                } else {
                    (TokenType::Minus, String::from("-"))
                };
                Ok(Token { typ, value, loc })
            }
            '*' => Ok(Token {
                typ: TokenType::Asterisk,
                value: String::from(c),
                loc,
            }),
            '/' => {
                let (typ, value) = if let Ok(nc) = self.next_char() {
                    match nc {
                        '/' => {
                            while let Ok(c) = self.next_char() {
                                if c == '\r' || c == '\n' {
                                    self.trim_whitespace();
                                    break;
                                }
                            }
                            return self.next_token();
                        },
                        _ => {
                            self.current_char -= 1; // Went too far, go a step back
                            (TokenType::ForwardSlash, String::from("/"))
                        }
                    }
                } else {
                    (TokenType::ForwardSlash, String::from("/"))
                };
                Ok(Token { typ, value, loc })
            },
            e => Err(format!(
                "{}: {:?}: Unexpected Symbol `{}`",
                ERR_STR,
                self.get_location(),
                e
            )),
        }
    }

    // ---------- End of Lexer ----------

    fn reset(&mut self) {
        self.current_char = 0;
        self.current_line = 1;
        self.line_start = 0;
        self.ptr = 0;
        self.fuel.set(256);
        self.root = None;
    }
    fn open(&self) -> Token {
        self.lookahead[0].clone()
    }
    fn advance(&mut self) -> Result<Token, String> {
        // assert!(!self.parser_eof());
        self.fuel.set(256);
        self.next()
    }
    
    fn fill_lookahead(&mut self) -> Result<(), String> {
        while self.lookahead.len() < LOOKAHEAD_LIMIT {
            let tkn = self.next_token()?;
            self.lookahead.push_back(tkn);
        }
        Ok(())
    }

    fn nth(&mut self, lookahead: usize) -> Result<TokenType, String> {
        self.fill_lookahead()?;
        if self.fuel.get() == 0 {
            panic!("Parser is stuck at {}", self.ptr);
        }
        self.fuel.set(self.fuel.get() - 1);
        Ok(self.lookahead[lookahead].get_type())
    }

    // // fn next_token(&mut self) -> Result<Token, String> {
    // //     self.fill_lookahead()?;
    // //     assert!(self.lookahead.len() > 0);
    // //     Ok(self.lookahead[0].clone())
    // // }

    fn at(&mut self, typ: TokenType) -> Result<bool, String> {
        Ok(self.nth(0)? == typ)
    }

    fn next(&mut self) -> Result<Token, String> {
        self.fill_lookahead()?;
        assert!(self.lookahead.len() > 0);
        Ok(self.lookahead.pop_front().unwrap())
    }

    fn eat(&mut self, typ: TokenType) -> Result<bool, String> {
        if self.at(typ)? {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn expect(&mut self, typ: TokenType) -> Result<Token, String> {
        let t = typ;
        if self.at(typ)? {
            return self.advance();
        }
        let tkn = self.next()?;
        Err(format!(
            "{}: {:?}: Expected {:?}, found {:?}",
            ERR_STR,
            tkn.loc,
            t,
            tkn.typ
        ))
    }

    pub fn dump(&mut self) {
        let mut t = self.next().unwrap();
        while t.typ != TokenType::Eof {
            println!("{:?}", t);
            t = self.next().unwrap();
        }

    }

    pub fn parse_snippet(&mut self) -> Result<Tree, String> {
        // Only ever call this in combination with Compiler::throwaway_compiler_parse()
        // Assumes there's a single statement or function or whatever
        // like `return this;` that's getting inserted in desugaring
        match self.nth(0)? {
            TokenType::FunctionKeyword => self.parse_func(),
            TokenType::FeatureKeyword => self.parse_feature(),
            TokenType::ClassKeyword => self.parse_class(),
            TokenType::LetKeyword => self.parse_stmt_let(),
            TokenType::IfKeyword => self.parse_stmt_if(),
            TokenType::ReturnKeyword => self.parse_stmt_return(),
            TokenType::OpenCurly => self.parse_block(),
            e => todo!("`parse_snippet()` does not support `{:?}` yet", e)
        }
    }

    pub fn parse_file(&mut self) -> Result<Tree, String> {
        assert_eq!(
            TokenType::Eof as u8 + 1,
            36,
            "Not all TokenTypes are handled in parse_file()"
        );
        let tkn = Token::new(
            TokenType::File,
            self.origin.clone(),
            Location::new(self.origin.clone(), 0, 0),
        );
        let mut functions = vec![];
        let mut classes = vec![];
        while !self.parser_eof() {
            match self.nth(0)? {
                TokenType::FunctionKeyword => functions.push(self.parse_func()?),
                TokenType::ClassKeyword => classes.push(self.parse_class()?),
                _ => {
                    let tkn = self.next()?;
                    return Err(format!(
                        "{}: {:?}: Expected one of {{Function, Class}}, found {:?}",
                        ERR_STR,
                        tkn.get_loc(),
                        tkn.get_type()
                    ));
                }
            }
        }
        assert!(self.nth(0)? == TokenType::Eof);
        self.root = Some(Tree {
            typ: TreeType::File {
                functions,
                classes
            },
            tkn,
        });
        Ok(self.root.clone().unwrap())
    }

    fn parse_class(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::ClassKeyword)?;
        let class_name = self.expect(TokenType::Identifier)?;
        if !class_name.get_value().as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Class names are expected to be capitalized, found `{}`.",
                ERR_STR,
                class_name.get_loc(),
                class_name.get_value()
            ));
        }
        self.expect(TokenType::OpenCurly)?;
        let mut fields = vec![];
        let mut functions = vec![];
        let mut features = vec![];
        while !self.at(TokenType::ClosingCurly)? && !self.parser_eof() {
            // println!("{:#?}\n{:#?}\n{:#?}", fields, features, functions);
            match self.nth(0)? {
                TokenType::Identifier => fields.push(self.parse_class_field()?),
                TokenType::FunctionKeyword => functions.push(self.parse_func()?),
                TokenType::FeatureKeyword => features.push(self.parse_feature()?),
                _ => {
                    todo!()
                    // let prev = self.tokens.get(self.ptr).unwrap();
                    // return Err(format!(
                    //     "{}: {:?}: Expected one of {{Field, Function}}, found {:?}.",
                    //     ERR_STR,
                    //     prev.get_loc(),
                    //     prev.get_type()
                    // ));
                }
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(Tree {
            typ: TreeType::Class {
                name: class_name.get_value(),
                fields,
                functions,
                features
            },
            tkn
        })
    }

    fn parse_class_field(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let name = self.expect(TokenType::Identifier)?;
        let typ = self.parse_type_decl()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::Field {
                name: name.get_value(),
                typ: Box::new(typ)
            },
            tkn
        })
    }

    fn parse_feature(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::FeatureKeyword)?;
        let feature_name = self.expect(TokenType::Identifier)?;
        let params = self.parse_param_list()?;
        let return_type = if self.at(TokenType::Arrow)? {
            Some(Box::new(self.parse_return_type()?))
        } else {
            None
        };
        let block = self.parse_block()?;
        Ok(Tree {
            typ: TreeType::Feature {
                name: feature_name.get_value(),
                return_type,
                param: Box::new(params),
                block: Box::new(block),
            },
            tkn,
        })
    }

    fn parse_func(&mut self) -> Result<Tree, String> {
        let tkn = self.expect(TokenType::FunctionKeyword)?;
        let fn_name = self.expect(TokenType::Identifier)?;
        if fn_name.get_value().as_bytes()[0].is_ascii_uppercase() {
            return Err(format!(
                "{}: {:?}: Function names are not expected to be capitalized, found `{}`.",
                ERR_STR,
                fn_name.get_loc(),
                fn_name.get_value()
            ));
        }
        let params = self.parse_param_list()?;
        let return_type = if self.at(TokenType::Arrow)? {
            Some(Box::new(self.parse_return_type()?))
        } else {
            None
        };
        let block = self.parse_block()?;
        Ok(Tree {
            typ: TreeType::Func {
                name: fn_name.get_value(),
                return_type,
                param: Box::new(params),
                block: Box::new(block),
            },
            tkn,
        })
    }

    fn parse_param_list(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound)? && !self.parser_eof() {
            children.push(self.parse_param()?);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ParamList {
                parameters: children,
            },
            tkn,
        })
    }

    fn parse_param(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let name_tkn = self.expect(TokenType::Identifier)?;
        let type_decl = self.parse_type_decl()?;
        Ok(Tree {
            typ: TreeType::Param {
                name: name_tkn.get_value(),
                typ: Box::new(type_decl),
            },
            tkn
        })
    }

    fn parse_block(&mut self) -> Result<Tree, String> {
        if !self.at(TokenType::OpenCurly)? {
            let tkn = self.next()?;
            return Err(format!(
                "{}: {:?}: Expected `{{`, found `{:?}`",
                ERR_STR,
                tkn.get_loc(),
                tkn.get_type()
            ));
        }
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenCurly)?;
        while !self.at(TokenType::ClosingCurly)? && !self.parser_eof() {
            // Parse Statements
            match self.nth(0)? {
                TokenType::LetKeyword => children.push(self.parse_stmt_let()?),
                TokenType::IfKeyword => children.push(self.parse_stmt_if()?),
                TokenType::ReturnKeyword => children.push(self.parse_stmt_return()?),
                TokenType::Identifier => match self.nth(1)? {
                    TokenType::Dot | TokenType::Equal | TokenType::OpenSquare => {
                        children.push(self.parse_stmt_assign()?)
                    }
                    _ => children.push(self.parse_stmt_expr()?),
                },
                _ => children.push(self.parse_stmt_expr()?),
            }
        }
        self.expect(TokenType::ClosingCurly)?;
        Ok(Tree {
            typ: TreeType::Block {
                statements: children,
            },
            tkn,
        })
    }

    fn parse_stmt_expr(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtExpr {
                expression: Box::new(expr.clone()),
            },
            tkn: tkn,
        })
    }

    fn parse_stmt_let(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::LetKeyword)?;
        let name_tkn = self.expect(TokenType::Identifier)?;
        let type_tree = self.parse_type_decl()?;
        self.expect(TokenType::Equal)?;
        let expr = match self.nth(0)? {
            TokenType::OpenSquare => self.parse_expr_array_literal()?,
            _ => self.parse_expr()?
        };
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtLet {
                name: name_tkn.get_value(),
                typ: Box::new(type_tree),
                expression: Box::new(expr),
            },
            tkn,
        })
    }

    fn parse_stmt_assign(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let node = self.parse_identifier()?;
        self.expect(TokenType::Equal)?;
        let expr = self.parse_expr()?;
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtAssign {
                name: Box::new(node.clone()),
                expression: Box::new(expr),
            },
            tkn,
        })
    }

    fn parse_stmt_if(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::IfKeyword)?;
        self.expect(TokenType::OpenRound)?;
        let condition = self.parse_expr()?;
        self.expect(TokenType::ClosingRound)?;
        let if_block = self.parse_block()?;
        let else_block = if self.eat(TokenType::ElseKeyword)? {
            Some(Box::new(self.parse_block()?))
        } else {
            None
        };
        Ok(Tree {
            typ: TreeType::StmtIf {
                condition: Box::new(condition),
                if_branch: Box::new(if_block),
                else_branch: else_block,
            },
            tkn,
        })
    }

    fn parse_stmt_return(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::ReturnKeyword)?;
        let ret_value = if !self.at(TokenType::Semi)? {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect(TokenType::Semi)?;
        Ok(Tree {
            typ: TreeType::StmtReturn {
                return_value: ret_value,
            },
            tkn,
        })
    }

    fn parse_return_type(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::Arrow)?;
        let name_tkn = self.expect(TokenType::Identifier)?;
        let typ = self.parse_type(&name_tkn)?;
        Ok(Tree {
            typ: TreeType::TypeDecl { typ },
            tkn,
        })
    }

    fn parse_type_decl(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::Colon)?;
        let name = self.expect(TokenType::Identifier)?;
        let typ = self.parse_type(&name)?;
        let name = if self.at(TokenType::OpenSquare)? {
            let size = self.parse_type_arr_size()?;
            Tree {
                typ: TreeType::TypeDecl {
                    typ: Type::Arr(Box::new(typ), size),
                },
                tkn,
            }
        } else {
            Tree {
                typ: TreeType::TypeDecl { typ },
                tkn,
            }
        };
        Ok(name)
    }

    fn parse_type_literal(&mut self, lit_tkn: &Token) -> Result<(String, Type), String> {
        let lit = lit_tkn.get_value();
        let loc = &lit_tkn.get_loc();
        match lit.bytes().position(|c| c.is_ascii_alphabetic()) {
            Some(index) => {
                let typ_str = &lit[index..];
                let typ = self.parse_type_str(loc, typ_str)?;
                Ok((lit[0..index].to_owned(), typ))
            }
            None => Ok((lit, Type::Unknown)),
        }
    }

    fn parse_type(&self, token: &Token) -> Result<Type, String> {
        self.parse_type_str(&token.get_loc(), &token.get_value())
    }

    fn parse_type_str(&self, loc: &Location, val: &str) -> Result<Type, String> {
        match val {
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "usize" => Ok(Type::Usize),
            // Reserved for future use
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            _ => Ok(Type::Class(val.to_owned()))
        }
    }

    fn parse_type_arr_size(&mut self) -> Result<Vec<usize>, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenSquare)?;
        let size = self.expect(TokenType::IntLiteral)?;
        let size = match size.get_value().parse::<usize>() {
            Ok(i) => i,
            Err(e) => return Err(format!("{}: {:?}: {}", ERR_STR, tkn.get_loc(), e)),
        };
        children.push(size);
        while !self.at(TokenType::ClosingSquare)? && !self.parser_eof() {
            self.expect(TokenType::Comma);
            let size = self.expect(TokenType::IntLiteral)?;
            let size = match size.get_value().parse::<usize>() {
                Ok(i) => i,
                Err(e) => return Err(format!("{}: {:?}: {}", ERR_STR, tkn.get_loc(), e)),
            };
            children.push(size);
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(children)
    }

    fn parse_expr_call(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::Identifier)?;
        let args = self.parse_arg_list()?;
        Ok(Tree {
            typ: TreeType::ExprCall {
                function_name: tkn.get_value(),
                args: Box::new(args),
                typ: Type::Unknown,
            },
            tkn,
        })
    }
    
    fn parse_arg_list(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut children = vec![];
        self.expect(TokenType::OpenRound)?;
        while !self.at(TokenType::ClosingRound)? && !self.parser_eof() {
            children.push(self.parse_arg()?);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        self.expect(TokenType::ClosingRound)?;
        Ok(Tree {
            typ: TreeType::ArgList {
                arguments: children,
            },
            tkn,
        })
    }

    fn parse_arg(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let expr = self.parse_expr()?;
        Ok(Tree {
            typ: TreeType::Arg {
                expression: Box::new(expr.clone()),
            },
            tkn,
        })
    }

    fn parse_expr(&mut self) -> Result<Tree, String> {
        self.parse_expr_rec(TokenType::Eof)
    }

    fn parse_expr_rec(&mut self, left: TokenType) -> Result<Tree, String> {
        let mut lhs = self.parse_expr_delim()?;

        loop {
            let right = self.nth(0)?;
            if Self::right_binds_tighter(left, right) {
                let tkn = self.open();
                self.advance();
                let rhs = self.parse_expr_rec(right)?;
                if COMPARATOR_TYPES.contains(&right) {}
                lhs = if COMPARATOR_TYPES.contains(&right) {
                    Tree {
                        typ: TreeType::ExprComp {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            typ: Type::Unknown,
                        },
                        tkn,
                    }
                } else {
                    Tree {
                        typ: TreeType::ExprBinary {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            typ: Type::Unknown,
                        },
                        tkn,
                    }
                };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_expr_delim(&mut self) -> Result<Tree, String> {
        Ok(match self.nth(0)? {
            TokenType::IntLiteral => {
                // let tkn = self.open();
                let number_token = self.expect(TokenType::IntLiteral)?;
                let (number, typ) = self.parse_type_literal(&number_token)?;
                let tkn = Token::new(TokenType::IntLiteral, number, number_token.get_loc());
                Tree {
                    typ: TreeType::ExprLiteral { typ },
                    tkn,
                }
            }
            TokenType::Identifier => self.parse_identifier()?,
            TokenType::OpenRound => {
                let tkn = self.open();
                self.expect(TokenType::OpenRound)?;
                let expr = self.parse_expr()?;
                self.expect(TokenType::ClosingRound)?;
                Tree {
                    typ: TreeType::ExprParen {
                        expression: Box::new(expr),
                        typ: Type::Unknown,
                    },
                    tkn,
                }
            }
            TokenType::OpenSquare => self.parse_expr_array_literal()?,
            TokenType::BuiltInFunction => self.parse_expr_built_in_func()?,
            e => {
                let tkn = self.next()?;
                return Err(format!(
                    "{}: {:?}: Expected Expr, found {:?}",
                    ERR_STR,
                    tkn.get_loc(),
                    e
                ));
            }
        })
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
    
    fn parse_expr_array_literal(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        let mut elements = vec![];
        self.expect(TokenType::OpenSquare)?;
        while !self.at(TokenType::ClosingSquare)? && !self.parser_eof() {
            let elem = self.parse_expr()?;
            elements.push(elem);
            if !self.eat(TokenType::Comma)? {
                break;
            }
        }
        self.expect(TokenType::ClosingSquare)?;
        Ok(Tree {
            typ: TreeType::ExprArrLiteral { elements },
            tkn,
        })
    }

    fn parse_expr_built_in_func(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        self.expect(TokenType::BuiltInFunction)?;
        let args = self.parse_arg_list()?;
        Ok(Tree {
            typ: TreeType::BuiltInFunction {
                function_name: tkn.get_value(),
                args: Box::new(args),
                typ: Type::Unknown,
            },
            tkn,
        })
    }

    fn parse_identifier(&mut self) -> Result<Tree, String> {
        let tkn = self.open();
        Ok(match self.nth(1)? {
            TokenType::OpenRound => self.parse_expr_call()?,
            TokenType::OpenSquare => {
                self.expect(TokenType::Identifier)?;
                let indices = self.parse_expr_array_literal()?;
                Tree {
                    typ: TreeType::ExprArrAccess {
                        arr_name: tkn.get_value(),
                        indices: Box::new(indices),
                        typ: Type::Unknown,
                    },
                    tkn,
                }
            }
            TokenType::Dot => {
                self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Dot)?;
                let access = self.parse_identifier()?;
                Tree {
                    typ: TreeType::FieldAccess {
                        name: tkn.get_value(),
                        field: Box::new(access),
                        typ: Type::Unknown
                    },
                    tkn
                }
            }
            _ => {
                self.expect(TokenType::Identifier)?;
                Tree {
                    typ: TreeType::ExprName {
                        name: tkn.get_value(),
                        typ: Type::Unknown,
                    },
                    tkn,
                }                
            }
        })
    }
}
