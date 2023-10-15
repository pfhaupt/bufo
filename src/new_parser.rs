use std::cell::Cell;
use std::collections::VecDeque;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use derive_builder::Builder;

use crate::codegen::ERR_STR;
use crate::{checker::Type, parser::Location};

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

trait Parsable {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized;
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FileNode {
    location: Location,
    filepath: String,
    functions: Vec<FunctionNode>,
    classes: Vec<ClassNode>,
}

impl Parsable for FileNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> {
        let location = parser.current_location();
        let mut classes = vec![];
        let mut functions = vec![];
        while !parser.parsed_eof() {
            match parser.nth(0) {
                TokenType::ClassKeyword => {
                    let parsed_class = ClassNode::parse(parser)?;
                    classes.push(parsed_class);
                },
                TokenType::FunctionKeyword => {
                    let parsed_function = FunctionNode::parse(parser)?;
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
        Ok(FileNodeBuilder::default()
            .filepath(parser.filepath.file_stem().unwrap().to_str().unwrap().to_string())
            .location(location)
            .classes(classes)
            .functions(functions)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ClassNode {
    location: Location,
    name: String,
    fields: Vec<FieldNode>,
    functions: Vec<FunctionNode>,
    features: Vec<FeatureNode>,
}

impl Parsable for ClassNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::ClassKeyword)?;

        let class_name = parser.expect(TokenType::Identifier)?;
        let name = class_name.value;

        parser.expect(TokenType::OpenCurly)?;

        let mut fields = vec![];
        let mut functions = vec![];
        let mut features = vec![];
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingCurly) {
            match parser.nth(0) {
                TokenType::Identifier => {
                    let parsed_field = FieldNode::parse(parser)?;
                    fields.push(parsed_field);
                }
                TokenType::FeatureKeyword => {
                    let parsed_feature = FeatureNode::parse(parser)?;
                    features.push(parsed_feature);
                }
                TokenType::FunctionKeyword => {
                    let parsed_function = FunctionNode::parse(parser)?;
                    functions.push(parsed_function);
                }
                e => todo!("{:?}", e)
            }
        }
        parser.expect(TokenType::ClosingCurly)?;
        Ok(ClassNodeBuilder::default()
            .name(name)
            .location(location)
            .features(features)
            .fields(fields)
            .functions(functions)
            .build().unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FieldNode {
    location: Location,
    name: String,
    typ: TypeNode,
}

impl Parsable for FieldNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        let typ = TypeNode::parse(parser)?;
        parser.expect(TokenType::Semi);
        Ok(FieldNodeBuilder::default()
            .location(location)
            .name(name)
            .typ(typ)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct FieldAccess {
    location: Location,
    name: String,
    field: Expression,
    typ: Type
}

impl Parsable for FieldAccess {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}

#[derive(Debug, Builder, Clone)]
pub struct FeatureNode {
    location: Location,
    name: String,
    return_type: ReturnTypeNode,
    parameters: Vec<ParameterNode>,
    block: BlockNode,
}

impl Parsable for FeatureNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        
        parser.expect(TokenType::FeatureKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        
        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        
        let return_type = ReturnTypeNode::parse(parser)?;

        let block = BlockNode::parse(parser)?;
        Ok(FeatureNodeBuilder::default()
            .location(location)
            .name(name)
            .return_type(return_type)
            .parameters(parameters)
            .block(block)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FunctionNode {
    location: Location,
    name: String,
    return_type: ReturnTypeNode,
    parameters: Vec<ParameterNode>,
    block: BlockNode
}

impl Parsable for FunctionNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        
        parser.expect(TokenType::FunctionKeyword)?;
        let feature_name = parser.expect(TokenType::Identifier)?;
        let name = feature_name.value;
        
        let mut parameters = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let parsed_param = ParameterNode::parse(parser)?;
            parameters.push(parsed_param);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        
        let return_type = ReturnTypeNode::parse(parser)?;

        let block = BlockNode::parse(parser)?;
        Ok(FunctionNodeBuilder::default()
            .location(location)
            .name(name)
            .return_type(return_type)
            .parameters(parameters)
            .block(block)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnTypeNode {
    location: Location,
    typ: Type
}

impl Parsable for ReturnTypeNode {
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
        Ok(ReturnTypeNodeBuilder::default()
            .location(location)
            .typ(typ)
            .build()
            .unwrap())
    }
}


#[derive(Debug, Builder, Clone)]
pub struct ParameterNode {
    location: Location,
    name: String,
    typ: TypeNode
}

impl Parsable for ParameterNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        let type_declaration = TypeNode::parse(parser)?;
        Ok(ParameterNodeBuilder::default()
            .location(location)
            .name(name)
            .typ(type_declaration)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct BlockNode {
    location: Location,
    statements: Vec<Statement>,
}

impl Parsable for BlockNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();

        parser.expect(TokenType::OpenCurly)?;
        let mut statements = vec![];
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingCurly) {
            let parsed_statement = Statement::parse(parser)?;
            statements.push(parsed_statement);
        }
        parser.expect(TokenType::ClosingCurly)?;
        Ok(BlockNodeBuilder::default()
            .location(location)
            .statements(statements)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionNode),
    Let(LetNode),
    Assign(AssignNode),
    If(IfNode),
    Return(ReturnNode)
}

impl Parsable for Statement {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        Ok(match parser.nth(0) {
            TokenType::LetKeyword => Statement::Let(LetNode::parse(parser)?),
            TokenType::IfKeyword => Statement::If(IfNode::parse(parser)?),
            TokenType::ReturnKeyword => Statement::Return(ReturnNode::parse(parser)?),
            TokenType::Identifier => match parser.nth(1) {
                TokenType::Dot | TokenType::Equal | TokenType::OpenSquare => {
                    Statement::Assign(AssignNode::parse(parser)?)
                }
                _ => Statement::Expression(ExpressionNode::parse(parser)?)
            }
            _ => Statement::Expression(ExpressionNode::parse(parser)?)
        })
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ExpressionNode {
    location: Location,
    expression: Expression
}

impl Parsable for ExpressionNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = Expression::parse(parser)?;
        Ok(ExpressionNodeBuilder::default()
            .location(location)
            .expression(expression)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct LetNode {
    location: Location,
    name: String,
    typ: TypeNode,
    expression: ExpressionNode,
}

impl Parsable for LetNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::LetKeyword)?;
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        let type_tree = TypeNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;
        Ok(LetNodeBuilder::default()
            .location(location)
            .name(name)
            .typ(type_tree)
            .expression(expression)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct AssignNode {
    location: Location,
    name: ExpressionIdentifierNode,
    expression: ExpressionNode,
}

impl Parsable for AssignNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name = ExpressionIdentifierNode::parse(parser)?;
        parser.expect(TokenType::Equal)?;
        let expression = ExpressionNode::parse(parser)?;
        parser.expect(TokenType::Semi)?;
        Ok(AssignNodeBuilder::default()
            .location(location)
            .name(name)
            .expression(expression)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionIdentifierNode {
    location: Location,
    expression: Box<Expression>,
    typ: Type
}

impl Parsable for ExpressionIdentifierNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = match parser.nth(1) {
            TokenType::OpenRound => Expression::FunctionCall(ExpressionCallNode::parse(parser)?),
            TokenType::OpenSquare => todo!(),
            TokenType::Dot => Expression::FieldAccess(ExpressionFieldAccessNode::parse(parser)?),
            _ => Expression::Name(NameNode::parse(parser)?)
        };
        Ok(ExpressionIdentifierNodeBuilder::default()
            .location(location)
            .expression(Box::new(expression))
            .typ(Type::Unknown)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct IfNode {
    location: Location,
    condition: ExpressionBinaryNode,
    if_branch: BlockNode,
    else_branch: Option<BlockNode>,
}

impl Parsable for IfNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnNode {
    location: Location,
    return_value: Option<ExpressionNode>,
}

impl Parsable for ReturnNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::ReturnKeyword)?;
        let ret_value = if !parser.at(TokenType::Semi) {
            Some(ExpressionNode::parse(parser)?)
        } else {
            None
        };
        parser.expect(TokenType::Semi)?;
        Ok(ReturnNodeBuilder::default()
            .location(location)
            .return_value(ret_value)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Builder, Clone)]
pub struct TypeNode {
    location: Location,
    typ: Type,
    array_dimensions: Option<ExpressionArrayLiteralNode>
}

impl Parsable for TypeNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        parser.expect(TokenType::Colon)?;
        let name_token = parser.expect(TokenType::Identifier)?;
        let typ = parser.parse_type(name_token);
        let dims = if parser.at(TokenType::OpenSquare) {
            Some(ExpressionArrayLiteralNode::parse(parser)?)
        } else {
            None
        };
        Ok(TypeNodeBuilder::default()
                .location(location)
                .typ(typ)
                .array_dimensions(dims)
                .build()
                .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ArgumentNode {
    location: Location,
    expression: ExpressionNode
}

impl Parsable for ArgumentNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let expression = ExpressionNode::parse(parser)?;
        Ok(ArgumentNodeBuilder::default()
            .location(location)
            .expression(expression)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Default, Clone)]
pub enum Expression {
    Name(NameNode),
    Identifier(ExpressionIdentifierNode),
    ArrayLiteral(ExpressionArrayLiteralNode),
    ArrayAccess(ExpressionArrayAccessNode),
    Literal(ExpressionLiteralNode),
    Binary(ExpressionBinaryNode),
    FieldAccess(ExpressionFieldAccessNode),
    // Parenthesis(ExpressionNode),
    FunctionCall(ExpressionCallNode),
    BuiltIn(ExpressionBuiltInNode),
    #[default] None,
}

impl Parsable for Expression {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        Self::parse_rec(parser, TokenType::Eof)
    }
}
impl Expression {
    fn parse_delim(parser: &mut Parser) -> Result<Self, String> {
        Ok(match parser.nth(0) {
            TokenType::IntLiteral => {
                let expression = ExpressionLiteralNode::parse(parser)?;
                Self::Literal(expression)
            }
            TokenType::Identifier => {
                let expression = ExpressionIdentifierNode::parse(parser)?;
                Self::Identifier(expression)
            }
            TokenType::OpenSquare => {
                let expression = ExpressionArrayLiteralNode::parse(parser)?;
                Self::ArrayLiteral(expression)
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
                lhs = Expression::Binary(ExpressionBinaryNode {
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

#[derive(Debug, Builder, Clone)]
pub struct ExpressionArrayLiteralNode {
    location: Location,
    elements: Vec<Expression>
}

impl Parsable for ExpressionArrayLiteralNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let mut elements = vec![];
        parser.expect(TokenType::OpenSquare)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingSquare) {
            let elem = Expression::parse(parser)?;
            elements.push(elem);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingSquare)?;
        Ok(ExpressionArrayLiteralNodeBuilder::default()
            .location(location)
            .elements(elements)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionArrayAccessNode {
    location: Location,
    array_name: String,
    indices: ExpressionArrayLiteralNode,
    typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionLiteralNode {
    location: Location,
    value: String,
    typ: Type,
}

impl Parsable for ExpressionLiteralNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let number_token = parser.expect(TokenType::IntLiteral)?;
        let (number, typ) = parser.parse_type_literal(number_token)?;
        Ok(ExpressionLiteralNodeBuilder::default()
            .location(location)
            .value(number)
            .typ(typ)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionBinaryNode {
    location: Location,
    operation: Operation,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionCallNode {
    location: Location,
    function_name: String,
    arguments: Vec<ArgumentNode>,
    typ: Type
}

impl Parsable for ExpressionCallNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;

        let mut arguments = vec![];
        parser.expect(TokenType::OpenRound)?;
        while !parser.parsed_eof() && !parser.at(TokenType::ClosingRound) {
            let arg = ArgumentNode::parse(parser)?;
            arguments.push(arg);
            if !parser.eat(TokenType::Comma) {
                break;
            }
        }
        parser.expect(TokenType::ClosingRound)?;
        Ok(ExpressionCallNodeBuilder::default()
            .function_name(name)
            .location(location)
            .arguments(arguments)
            .typ(Type::Unknown)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionFieldAccessNode {
    location: Location,
    name: String,
    field: ExpressionIdentifierNode,
    typ: Type
}

impl Parsable for ExpressionFieldAccessNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        let name = name_token.value;
        parser.expect(TokenType::Dot)?;
        let field = ExpressionIdentifierNode::parse(parser)?;
        Ok(ExpressionFieldAccessNodeBuilder::default()
            .location(location)
            .name(name)
            .field(field)
            .typ(Type::Unknown)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct NameNode {
    location: Location,
    name: String,
    typ: Type,
}

impl Parsable for NameNode {
    fn parse(parser: &mut Parser) -> Result<Self, String> where Self: Sized {
        let location = parser.current_location();
        let name_token = parser.expect(TokenType::Identifier)?;
        
        let name = name_token.value;
        Ok(NameNodeBuilder::default()
            .location(location)
            .name(name)
            .typ(Type::Unknown)
            .build()
            .unwrap())
    }
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionBuiltInNode {
    location: Location,
    function_name: String,
    arguments: Vec<ArgumentNode>,
    typ: Type
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

use crate::parser::{Tree, TreeType, TokenType, COMPARATOR_TYPES};

#[derive(Default)]
pub struct Parser {
    filepath: PathBuf,
    filename: String,
    source: Vec<char>,
    fuel: Cell<u32>,
    lookahead: VecDeque<Token>,
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

    fn next_char(&mut self) -> char {
        if self.lexed_eof() {
            '\0'
        } else {
            let c = self.source[self.current_char];
            self.current_char += 1;
            c
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        macro_rules! fill_buffer {
            ($start: expr, $cond: expr) => {
                {
                    let mut value = String::from($start);
                    loop {
                        let nc = self.next_char();
                        if !($cond)(nc) {
                            self.current_char -= 1;
                            break value;
                        }
                        value.push(nc);
                    }
                }
            };
        }
        debug_assert_eq!(
            TokenType::Eof as u8 + 1,
            36,
            "Not all TokenTypes are handled in next_token()"
        );
        self.trim_whitespace();
        let loc = self.get_location();
        let c = self.next_char();
        let (typ, value) = match c {
            '0'..='9' => {
                let value = fill_buffer!(c, 
                    |c: char| { c.is_numeric() }
                );
                (TokenType::IntLiteral, value)
            }
            'A'..='Z' | 'a'..='z' => {
                let value = fill_buffer!(c,
                    |c: char| { c.is_alphanumeric() || c == '_' }
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
            '"' => {
                let value = fill_buffer!(c,
                    |c: char| { c != '"' }
                );
                (TokenType::StrLiteral, value)
            }
            '\'' => {
                let value = fill_buffer!(c,
                    |c: char| { c == '\'' }
                );
                if value.len() != 1 {
                    return Err(format!("{}: {:?}: Char Literal is expected to be a single char, got `{}`.",
                        ERR_STR,
                        loc,
                        value
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
                            |c: char| { c != '\r' && c != '\n' }
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
            println!("Found {:?}", n);
            self.lookahead.push_back(n);
        }
        Ok(())
    }
    // ---------- End of Lexer ----------
    // ---------- Start of Parser ----------
    pub fn parse_file(&mut self) -> Result<FileNode, String> {
        FileNode::parse(self)
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
        self.lookahead.iter().any(|t| t.token_type == TokenType::Eof)
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

fn compile() -> Result<(), String> {
    let mut parser = Parser::new()
        .filepath("src/test.bu")?
        .debug(true)
        .initialize()?;
    let ast = parser.parse_file()?;
    println!("{:#?}", ast);
    Ok(())
}
pub fn main() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}