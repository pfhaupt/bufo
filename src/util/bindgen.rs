// TODO: Write C compliant Parser
// TODO: Check out clang's AST output
// NOTE: Tools such as rs-bindgen parse the output of clang's AST
use std::collections::{HashMap, VecDeque};

use tracer::trace_call;

use super::flags::Flags;
use crate::{compiler::{FILE_EXT, WARN_STR}, frontend::{nodes::CompilerFlag, parser::{self, Location}}, internal_panic};

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
enum CType {
    Array(Box<CType>, CToken),
    Byte,
    Bool,
    Char,
    Const(Box<CType>),
    Double,
    Float,
    Int,
    Long,
    Pointer(Box<CType>),
    Signed(Box<CType>),
    Short,
    Struct(String),
    Unsigned(Box<CType>),
    Void,
    VarArgs,
}

#[allow(unused)]
#[derive(Debug, Clone)]
enum CNode {
    Flags(Vec<CompilerFlag>),
    AST(Vec<CNode>),
    Enum {
        name: String,
        values: Vec<CNode>,
    },
    EnumVariant {
        name: String,
        value: Option<Box<CNode>>,
    },
    Field {
        name: String,
        ty: CType,
    },
    FuncPtr {
        name: String,
        ret: Box<CNode>,
        args: Vec<CNode>,
    },
    FuncDecl {
        name: String,
        ret: Box<CNode>,
        args: Vec<CNode>,
        vararg: bool,
    },
    Number(String),
    Type(CType),
    TypeAlias {},
    Typedef {
        name: String,
        ty: Box<CNode>,
    },
    Struct {
        name: String,
        fields: Vec<CNode>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CToken {
    Asterisk,
    Bool,
    Char,
    CloseCurly,
    CloseRound,
    CloseSquare,
    Comma,
    Const,
    Double,
    Enum,
    EOF,
    Equals,
    Float,
    Identifier(String),
    Int,
    Long,
    Number(String),
    OpenCurly,
    OpenRound,
    OpenSquare,
    Semicolon,
    Short,
    Signed,
    Struct,
    Typedef,
    Unsigned,
    VarArgs,
    Void,
}

fn filter_string(s: &str) -> &str {
    if s == "type" {
        "typ"
    } else {
        s
    }
}

#[allow(unused)]
pub struct Bindgen<'flags> {
    input_path: String,
    output_path: String,
    source: Vec<char>,
    current_char: usize,
    lookahead: VecDeque<CToken>,
    known_types: Vec<String>,
    aliases: HashMap<String, CType>,
    #[allow(unused)]
    flags: &'flags Flags,
}

impl<'flags> Bindgen<'flags> {
    pub fn new(flags: &'flags Flags) -> Bindgen<'flags> {
        Bindgen {
            input_path: String::new(),
            output_path: String::new(),
            source: Vec::new(),
            current_char: 0,
            lookahead: VecDeque::new(),
            aliases: HashMap::new(),
            known_types: vec!["__builtin_va_list".to_string()],
            flags,
        }.filepath(&flags.gen_bind.as_ref().unwrap())
    }

    fn filepath(self, filepath: &str) -> Self {
        debug_assert!(filepath.ends_with(".h"));
        let source = std::fs::read_to_string(filepath).unwrap().chars().collect();
        Bindgen {
            source,
            input_path: filepath.to_string(),
            output_path: filepath.replace(".h", &(".".to_string() + FILE_EXT)),
            ..self
        }
    }

    fn lexed_eof(&self) -> bool {
        self.current_char >= self.source.len()
    }

    fn trim_whitespace(&mut self) {
        while !self.lexed_eof() && self.source[self.current_char].is_whitespace() {
            self.current_char += 1;
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
    fn next_token(&mut self) -> CToken {
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
        self.trim_whitespace();
        let c = self.next_char();
        let token = match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let id = fill_buffer!(c, |c: char| {
                    (!c.is_alphanumeric() && c != '_') || c == '\0'
                });
                match id.as_str() {
                    "bool" | "_Bool" => CToken::Bool,
                    "char" => CToken::Char,
                    "const" => CToken::Const,
                    "double" => CToken::Double,
                    "enum" => CToken::Enum,
                    "float" => CToken::Float,
                    "int" => CToken::Int,
                    "long" => CToken::Long,
                    "short" => CToken::Short,
                    "signed" => CToken::Signed,
                    "struct" => CToken::Struct,
                    "typedef" => CToken::Typedef,
                    "unsigned" => CToken::Unsigned,
                    "void" => CToken::Void,
                    _ => CToken::Identifier(id),
                }
            }
            '0'..='9' => {
                let num = fill_buffer!(c, |c: char| !c.is_numeric() || c == '\0');
                if num == "0" && self.current_char() == 'x' {
                    let mut num = num;
                    num.push(self.next_char());
                    let num = fill_buffer!(num, |c: char| !c.is_numeric() || c == '\0');
                    CToken::Number(num)
                } else {
                    CToken::Number(num)
                }
            }
            '\0' => CToken::EOF,
            '(' => CToken::OpenRound,
            ')' => CToken::CloseRound,
            '{' => CToken::OpenCurly,
            '}' => CToken::CloseCurly,
            '[' => CToken::OpenSquare,
            ']' => CToken::CloseSquare,
            ';' => CToken::Semicolon,
            ',' => CToken::Comma,
            '*' => CToken::Asterisk,
            '=' => CToken::Equals,
            '#' => {
                let _ = fill_buffer!(c, |c: char| { c == '\r' || c == '\n' || c == '\0' });
                self.current_char += 1;
                return self.next_token();
            }
            '.' => {
                let v = fill_buffer!(c, |c: char| c != '.');
                if v == "..." {
                    CToken::VarArgs
                } else {
                    todo!("Implement tokenization for `{}`", v)
                }
            }
            ch => todo!("Implement tokenization for `{}`", ch),
        };
        if self.flags.debug {
            println!("[DEBUG] Found token: {:?}", token);
        }
        token
    }

    #[trace_call(extra)]
    fn fill_lookup(&mut self) {
        while self.lookahead.len() < 2 {
            let tkn = self.next_token();
            self.lookahead.push_back(tkn);
        }
    }

    #[trace_call(extra)]
    fn parsed_eof(&self) -> bool {
        self.lookahead[0] == CToken::EOF
    }

    #[trace_call(extra)]
    fn eat(&mut self, token: CToken) -> bool {
        if self.at(token) {
            self.next();
            true
        } else {
            false
        }
    }

    #[trace_call(extra)]
    fn at(&self, token: CToken) -> bool {
        self.lookahead[0] == token
    }

    #[trace_call(extra)]
    fn peek(&self, n: usize) -> &CToken {
        &self.lookahead[n]
    }

    #[trace_call(extra)]
    fn nth(&self, n: usize) -> &CToken {
        debug_assert!(n < self.lookahead.len());
        &self.lookahead[n]
    }

    #[trace_call(extra)]
    fn next(&mut self) -> CToken {
        self.fill_lookup();
        debug_assert!(!self.lookahead.is_empty());
        let tkn = self.lookahead.pop_front().unwrap();
        if self.flags.debug {
            println!("[DEBUG] Consumed token: {:?}", tkn);
        }
        tkn
    }

    #[trace_call(always)]
    fn expect(&mut self, token: CToken) -> Result<CToken, String> {
        let n = self.peek(0);
        if n == &token {
            Ok(self.next())
        } else {
            Err(format!("Expected token {:?}, found {:?}", token, n))
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        if self.flags.verbose {
            println!("[INFO] Running bindgen");
        }
        let now = std::time::Instant::now();
        let c_ast = self.parse_header()?;
        if self.flags.verbose {
            println!("[INFO] Parsing header took {:?}", now.elapsed());
        }
        
        let now = std::time::Instant::now();
        let mut genned = self.ast_to_string(&c_ast);
        genned = "/* Auto-generated by Bufo's bindgen */\n".to_string() + &genned;
        if self.flags.verbose {
            println!("[INFO] Generating code took {:?}", now.elapsed());
        }
        let now = std::time::Instant::now();
        std::fs::write(&self.output_path, genned).unwrap();
        if self.flags.verbose {
            println!("[INFO] Writing to file took {:?}", now.elapsed());
        }
        Ok(())
    }

    #[trace_call(always)]
    fn type_to_string(&self, typ: &CType) -> String {
        match typ {
            CType::Struct(name) => {
                if self.aliases.contains_key(name) {
                    self.type_to_string(self.aliases.get(name).unwrap())
                } else {
                    format!("{}", name)
                }
            },
            CType::Byte => format!("8"),
            CType::Char => format!("char"),
            CType::Double => format!("f64"),
            CType::Float => format!("f32"),
            CType::Int => format!("32"),
            CType::Long => format!("64"),
            CType::Short => format!("16"),
            CType::Signed(ty) => format!("i{}", self.type_to_string(ty)),
            CType::Unsigned(ty) => format!("u{}", self.type_to_string(ty)),
            CType::Void => unreachable!(), // TODO: Is this unreachable?
            CType::Bool => format!("bool"),
            CType::Const(ty) => format!("{}", self.type_to_string(ty)), // we dont care about const for C-funcs in headers
            CType::Pointer(ty) => {
                match **ty {
                    CType::Void => format!("Any"),
                    CType::Char => format!("&str"),
                    _ => format!("&{}", self.type_to_string(ty))
                }
            },
            CType::Array(ty, size) => {
                let CToken::Number(n) = size else {
                    internal_panic!("Expected number, found {:?}", size);
                };
                format!("[{}; {}]", self.type_to_string(ty), n)
            },
            CType::VarArgs => internal_panic!("Unreachable: VarArgs in type_to_string"),
        }
    }

    #[trace_call(always)]
    fn ast_to_string(&self, ast: &CNode) -> String {
        let mut result = String::new();
        match ast {
            CNode::Flags(flags) => {
                result += &format!( "{} {{\n", parser::KEYWORD_COMPILER_FLAGS);
                for flag in flags {
                    match flag {
                        CompilerFlag::LibPath(_, val) => result += &format!( "    libpath: \"{}\"", val),
                        CompilerFlag::Library(_, val) => result += &format!( "    library: \"{}\"", val),
                        CompilerFlag::Linker(_, val) => result += &format!( "    linker: \"{}\"", val),
                    }
                    result += &format!( ";\n");
                }
                result += &format!( "}}");
            }
            CNode::AST(ast) => {
                for node in ast {
                    result += &format!( "{}\n", self.ast_to_string(node));
                }
            }
            CNode::Enum { name, .. } => {
                result += &format!( "/* TODO: Codegen Enum */ struct {} {{}}", name);
                // result += &format!( "enum {} {{\n", name);
                // for value in values {
                //     result += &format!( "    {},\n", value);
                // }
                // write!(f, "}}")
            }
            CNode::EnumVariant { name, value } => {
                if let Some(value) = value {
                    result += &format!("{} = {}", name, self.ast_to_string(value))
                } else {
                    result += &format!("{}", name)
                }
            }
            CNode::Field { name, ty } => {
                result += &format!("{}: {}", filter_string(&name), self.type_to_string(ty))
            }
            CNode::FuncPtr { name, .. } => {
                result += &format!("/* TODO: Codegen FuncPtr */ struct {} {{}}", name)
            }
            CNode::FuncDecl { name, ret, args, vararg } => {
                let mut args = args.iter().map(|a| format!("{}", self.ast_to_string(a))).collect::<Vec<String>>().join(", ");
                if *vararg {
                    args.push_str(", ...");
                }
                if let CNode::Type(CType::Void) = ret.as_ref() {
                    result += &format!("unsafe extern {}({});", filter_string(name), args)
                } else {
                    result += &format!("unsafe extern {}({}) -> {};", filter_string(name), args, self.ast_to_string(ret))
                }
            }
            CNode::Number(n) => {
                if n.starts_with("0x") {
                    let n = i64::from_str_radix(n.strip_prefix("0x").unwrap(), 16).unwrap();
                    result += &format!("{}", n)
                } else {
                    result += &format!("{}", n)
                }
            },
            CNode::Type(ty) => {
                result += &format!("{}", self.type_to_string(ty))
            },
            CNode::TypeAlias { .. } => {
                // When binding we replace the alias with the original type
            },
            CNode::Typedef { ty, .. } => {
                result += &format!("{}", self.ast_to_string(ty))
            },
            CNode::Struct { name, fields } => {
                result += &format!( "struct {} {{\n", filter_string(name));
                for field in fields {
                    result += &format!( "    {};\n", self.ast_to_string(field));
                }
                result += &format!("}}")
            }
        }
        result
    }

    #[trace_call(extra)]
    fn is_type(&self, token: &CToken) -> bool {
        match token {
            CToken::Bool => true,
            CToken::Char => true,
            CToken::Const => true,
            CToken::Double => true,
            CToken::Float => true,
            CToken::Identifier(ident) => self.known_types.contains(ident),
            CToken::Int => true,
            CToken::Long => true,
            CToken::Short => true,
            CToken::Signed | CToken::Unsigned => true,
            CToken::Void => true,
            CToken::VarArgs => true,
            _ => false,
        }
    }

    #[trace_call(always)]
    fn ask_user_for_flags(&self) -> Result<CNode, String> {
        if self.flags.gen_bind.is_none() {
            return Err("gen_bind is not set".to_string());
        }
        let mut flags = Vec::new();
        if self.flags.debug {
            println!("[DEBUG] Asking user for flags");
        }
        println!("The compiler is about to parse the header file `{}`", self.input_path);
        println!("Please only enter one flag per line, or leave empty to skip this step");

        println!("Please enter the library paths that should be used to compile this file");
        println!("Note: This is the same as the `-L` flag in gcc or clang.");
        loop {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let flag = input.trim();
            if flag.is_empty() {
                break;
            }
            let flag = CompilerFlag::LibPath(Location::anonymous(), flag.to_string());
            flags.push(flag);            
        }

        println!("Please enter the libraries that should be linked to compile this file");
        println!("Note: This is the same as the `-l` flag in gcc or clang.");
        loop {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let flag = input.trim();
            if flag.is_empty() {
                break;
            }
            let flag = CompilerFlag::Library(Location::anonymous(), flag.to_string());
            flags.push(flag);            
        }

        println!("Please enter the linker flags that should be used to compile this file");
        println!("Note: This is the same as the `-Xlinker` flag in gcc or clang.");
        loop {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let flag = input.trim();
            if flag.is_empty() {
                break;
            }
            let flag = CompilerFlag::Linker(Location::anonymous(), flag.to_string());
            flags.push(flag);            
        }
        Ok(CNode::Flags(flags))
    }

    #[trace_call(always)]
    fn parse_header(&mut self) -> Result<CNode, String> {
        self.fill_lookup();
        let mut ast = Vec::new();
        let flags = self.ask_user_for_flags()?;
        ast.push(flags);
        while !self.parsed_eof() {
            match self.nth(0) {
                CToken::Typedef => {
                    let typedef = self.parse_typedef()?;
                    if self.flags.debug {
                        println!("[DEBUG] Parsed typedef:\n{:?}", typedef);
                    }
                    ast.push(typedef);
                }
                _ if self.is_type(self.peek(0)) => {
                    let func = self.parse_func_decl()?;
                    if self.flags.debug {
                        println!("[DEBUG] Parsed function declaration:\n{:?}", func);
                    }
                    ast.push(func);
                }
                t => return Err(format!("parse_header: Unexpected token {:?}", t)),
            }
        }
        Ok(CNode::AST(ast))
    }

    #[trace_call(always)]
    fn parse_func_decl(&mut self) -> Result<CNode, String> {
        let ty = self.parse_type(true)?;
        let name = self.next();
        let name = match name {
            CToken::Identifier(name) => name,
            _ => return Err(format!("Expected identifier, found {:?}", name)),
        };
        if self.eat(CToken::OpenRound) {
            let mut args = Vec::new();
            let mut vararg = false;
            while !self.at(CToken::CloseRound) {
                let ty = self.parse_type(true)?;
                if ty == CType::Void {
                    break;
                }
                if ty == CType::VarArgs {
                    vararg = true;
                    break;
                }
                let name = self.next();
                let name = match name {
                    CToken::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, found {:?}", name)),
                };
                let param = CNode::Field { name, ty };
                args.push(param);
                if self.eat(CToken::Comma) {
                    continue;
                }
            }
            self.expect(CToken::CloseRound)?;
            self.expect(CToken::Semicolon)?;
            Ok(CNode::FuncDecl { name, ret: Box::new(CNode::Type(ty)), args, vararg })
        } else {
            // Sounds like a variable declaration
            todo!()
        }
    }

    #[trace_call(always)]
    fn parse_typedef(&mut self) -> Result<CNode, String> {
        self.expect(CToken::Typedef)?;
        let next = self.peek(0);
        let tkn = match next {
            CToken::Struct => {
                let strukt = self.parse_struct()?;
                let CNode::Struct { name, fields } = strukt else {
                    unreachable!();
                };
                let def_name = self.next();
                let def_name = match def_name {
                    CToken::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, found {:?}", def_name)),
                };
                if def_name != name {
                    return Err("Typedef name does not match struct name".to_string());
                }
                CNode::Typedef {
                    name: name.clone(),
                    ty: Box::new(CNode::Struct { name, fields }),
                }
            }
            CToken::Enum => {
                let ewnum = self.parse_enum()?;
                let CNode::Enum { name, values } = ewnum else {
                    unreachable!();
                };
                debug_assert!(name.is_empty());
                let def_name = self.next();
                let def_name = match def_name {
                    CToken::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, found {:?}", def_name)),
                };
                let ewnum = CNode::Enum { name: def_name.clone(), values };
                println!("{}: Enums (here: `{}`) are not supported yet and will be ignored", WARN_STR, def_name);
                CNode::Typedef {
                    name: def_name,
                    ty: Box::new(ewnum),
                }
            }
            CToken::Identifier(ident) => {
                if ident == "__builtin_va_list" {
                    println!("{}: __builtin_va_list is not supported yet and will be ignored", WARN_STR);
                }
                let ty = self.parse_type(true)?;
                let name = self.next();
                let name = match name {
                    CToken::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, found {:?}", name)),
                };
                self.aliases.insert(name.clone(), ty.clone());
                self.known_types.push(name.clone());
                CNode::TypeAlias {}
            }
            typ if self.is_type(&typ) => {
                let ty = self.parse_type(true)?;
                let name = if self.eat(CToken::OpenRound) {
                    self.expect(CToken::Asterisk)?;
                    let name = self.next();
                    self.expect(CToken::CloseRound)?;
                    name
                } else {
                    let name = self.next();
                    name
                };
                let name = match name {
                    CToken::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, found {:?}", name)),
                };
                self.expect(CToken::OpenRound)?;
                let mut args = Vec::new();
                while !self.at(CToken::CloseRound) {
                    let ty = self.parse_type(true)?;
                    let name = self.next();
                    let name = match name {
                        CToken::Identifier(name) => name,
                        _ => return Err(format!("Expected identifier, found {:?}", name)),
                    };
                    let param = CNode::Field { name, ty };
                    args.push(param);
                    if self.eat(CToken::Comma) {
                        continue;
                    }
                }
                self.expect(CToken::CloseRound)?;
                println!("{}: Function pointers (here: `{}`) are not supported yet and will be ignored", WARN_STR, name);
                CNode::Typedef {
                    name: name.clone(),
                    ty: Box::new(CNode::FuncPtr {
                        name,
                        ret: Box::new(CNode::Type(ty)),
                        args
                    }),
                }
            }
            _ => return Err(format!("parse_typedef: Unexpected token {:?}", next)),
        };
        self.expect(CToken::Semicolon)?;
        if let CNode::Typedef { name, .. } = &tkn {
            self.known_types.push(name.clone());
        }
        Ok(tkn)
    }

    #[trace_call(always)]
    fn parse_enum(&mut self) -> Result<CNode, String> {
        self.expect(CToken::Enum)?;
        if !self.at(CToken::OpenCurly) {
            return Err(format!("Expected open curly, found {:?}", self.peek(0)));
        }
        self.expect(CToken::OpenCurly)?;
        let mut values = Vec::new();
        while !self.at(CToken::CloseCurly) {
            let value = match self.next() {
                CToken::Identifier(ident) => ident,
                e => return Err(format!("Expected identifier, found {:?}", e)),
            };
            if self.eat(CToken::Equals) {
                let mut expr = Vec::new();
                loop {
                    let n = self.peek(0);
                    if n == &CToken::Comma || n == &CToken::CloseCurly {
                        break;
                    }
                    expr.push(self.next());
                }
                if expr.len() == 1 {
                    let n = expr.pop().unwrap();
                    match n {
                        CToken::Number(n) => values.push(CNode::EnumVariant {
                            name: value,
                            value: Some(Box::new(CNode::Number(n)))
                        }),
                        _ => return Err(format!("Expected number, found {:?}", n)),
                    }
                } else {
                    // TODO: Const-Expr support
                    return Err(format!("Expected single value, found {:?}", expr));
                }
            } else {
                values.push(CNode::EnumVariant { name: value, value: None });
            }
            if self.eat(CToken::Comma) {
                continue;
            }
        }
        self.expect(CToken::CloseCurly)?;
        Ok(CNode::Enum { name: "".to_string(), values })
    }

    #[trace_call(always)]
    fn parse_struct(&mut self) -> Result<CNode, String> {
        self.expect(CToken::Struct)?;
        let name = self.next();
        let name = match name {
            CToken::Identifier(name) => name,
            _ => return Err(format!("Expected identifier, found {:?}", name)),
        };
        if !self.at(CToken::OpenCurly) {
            return Ok(CNode::Struct { name, fields: Vec::new() });
        }
        self.expect(CToken::OpenCurly)?;
        let mut fields = Vec::new();
        let mut last_type = None;
        while !self.at(CToken::CloseCurly) {
            match self.peek(0) {
                typ if self.is_type(&typ) => {
                    let ty = self.parse_type(true)?;
                    let name = self.next();
                    let name = match name {
                        CToken::Identifier(name) => name,
                        _ => return Err(format!("Expected identifier, found {:?}", name)),
                    };
                    last_type = Some(ty.clone());
                    fields.push(CNode::Field { name, ty });
                }
                CToken::Semicolon => {
                    last_type = None;
                    self.next();
                }
                CToken::Comma => {
                    self.next();
                }
                CToken::OpenSquare => {
                    self.expect(CToken::OpenSquare)?;
                    let mut sizes = Vec::new();
                    while !self.at(CToken::CloseSquare) {
                        let size = self.next();
                        match size {
                            CToken::Number(_) => {
                                sizes.push(size);
                            }
                            _ => return Err(format!("Expected number, found {:?}", size)),
                        }
                    }
                    self.expect(CToken::CloseSquare)?;
                    let ty = last_type.as_ref().unwrap().clone();
                    let last_field = fields.pop().unwrap();
                    let name = match last_field {
                        CNode::Field { name, .. } => name,
                        _ => unreachable!(),
                    };
                    let mut ty = ty;
                    for size in sizes.iter().rev() {
                        ty = CType::Array(Box::new(ty), size.clone());
                    }
                    fields.push(CNode::Field { name, ty });
                }
                _ if last_type.is_some() => {
                    let ty = last_type.as_ref().unwrap().clone();
                    let name = self.next();
                    let name = match name {
                        CToken::Identifier(name) => name,
                        _ => return Err(format!("Expected identifier, found {:?}", name)),
                    };
                    fields.push(CNode::Field { name, ty });
                }
                t => return Err(format!("parse_struct: Unexpected token {:?}", t)),
            
            }
        }
        self.expect(CToken::CloseCurly)?;
        Ok(CNode::Struct { name, fields })
    }

    #[trace_call(always)]
    fn parse_type(&mut self, signed: bool) -> Result<CType, String> {
        let typ = match self.next() {
            CToken::Bool => CType::Bool,
            CToken::Char => if signed {
                CType::Char
            } else {
                CType::Unsigned(Box::new(CType::Byte))
            }
            CToken::Const => {
                let ty = self.parse_type(true)?;
                CType::Const(Box::new(ty))
            }
            CToken::Double => CType::Double,
            CToken::Float => CType::Float,
            CToken::Identifier(ident) => {
                let name = ident.clone();
                if self.known_types.contains(&name) {
                    CType::Struct(name)
                } else {
                    return Err(format!("Unknown struct {:?}", name));
                }
            }
            CToken::Int => if signed {
                CType::Signed(Box::new(CType::Int))
            } else {
                CType::Unsigned(Box::new(CType::Int))
            },
            CToken::Long => if signed {
                CType::Signed(Box::new(CType::Long))
            } else {
                CType::Unsigned(Box::new(CType::Long))
            },
            CToken::Unsigned => {
                self.parse_type(false)?
            }
            CToken::Short => if signed {
                CType::Signed(Box::new(CType::Short))
            } else {
                CType::Unsigned(Box::new(CType::Short))
            },
            CToken::Void => CType::Void,
            CToken::VarArgs => CType::VarArgs,
            t => {
                debug_assert!(!self.is_type(&t), "parse_type: Unexpected token {:?}", t);
                return Err(format!("parse_type: Unexpected token {:?}", t));
            }
        };
        let mut typ = typ;
        while self.at(CToken::Asterisk) {
            self.next();
            typ = CType::Pointer(Box::new(typ));
        }
        Ok(typ)
    }
}