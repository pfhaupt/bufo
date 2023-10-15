use std::cell::Cell;
use std::collections::VecDeque;
use std::fs;
use std::path::{Path, PathBuf};

use crate::codegen::ERR_STR;
use crate::{checker::Type, parser::Location};

#[derive(Debug)]
struct FileNode<'a> {
    location: Location,
    filename: String,
    functions: Vec<FunctionNode<'a>>,
    classes: Vec<ClassNode<'a>>,
}

#[derive(Debug)]
struct ClassNode<'a> {
    location: Location,
    name: String,
    fields: Vec<FieldNode>,
    functions: Vec<FunctionNode<'a>>,
    features: Vec<FeatureNode<'a>>,
}

#[derive(Debug)]
struct FieldNode {
    location: Location,
    name: String,
    typ: TypeNode,
}

#[derive(Debug)]
struct FieldAccess {
    location: Location,
    name: String,
    // field: todo!(),
    typ: Type
}

#[derive(Debug)]
struct FeatureNode<'a> {
    location: Location,
    name: String,
    return_type: ReturnNode<'a>,
    parameters: Vec<ParameterNode>,
    block: BlockNode<'a>,
}

#[derive(Debug)]
struct FunctionNode<'a> {
    location: Location,
    name: String,
    return_type: ReturnNode<'a>,
    parameters: Vec<ParameterNode>,
    block: BlockNode<'a>
}

#[derive(Debug)]
struct ParameterNode {
    location: Location,
    name: String,
    typ: TypeNode
}

#[derive(Debug)]
struct BlockNode<'a> {
    location: Location,
    statements: Vec<Statement<'a>>,
}

#[derive(Debug)]
enum Statement<'a> {
    Expression(ExpressionNode<'a>),
    Let(LetNode<'a>),
    Assign(AssignNode<'a>),
    If(IfNode<'a>),
    Return(ReturnNode<'a>)
}

#[derive(Debug)]
struct ExpressionNode<'a> {
    location: Location,
    expression: Expression<'a>
}

#[derive(Debug)]
struct LetNode<'a> {
    location: Location,
    name: String,
    typ: TypeNode,
    expression: ExpressionNode<'a>,
}

#[derive(Debug)]
struct AssignNode<'a> {
    location: Location,
    name: ExpressionNode<'a>,
    expression: ExpressionNode<'a>,
}

#[derive(Debug)]
struct IfNode<'a> {
    location: Location,
    condition: ExpressionBinaryNode<'a>,
    if_branch: BlockNode<'a>,
    else_branch: Option<BlockNode<'a>>,
}

#[derive(Debug)]
struct ReturnNode<'a> {
    location: Location,
    return_value: Option<ExpressionNode<'a>>,
}

#[derive(Debug)]
struct TypeNode {
    location: Location,
    name: String,
    typ: Type
}

#[derive(Debug)]
struct ArgumentNode<'a> {
    location: Location,
    expression: ExpressionNode<'a>
}

#[derive(Debug)]
enum Expression<'a> {
    Name(NameNode),
    ArrayLiteral(ExpressionArrayLiteralNode<'a>),
    ArrayAccess(ExpressionArrayAccessNode<'a>),
    Literal(ExpressionLiteralNode),
    Binary(ExpressionBinaryNode<'a>),
    // Parenthesis(ExpressionNode),
    FunctionCall(ExpressionCallNode<'a>),
    BuiltIn(ExpressionBuiltInNode<'a>),
}

#[derive(Debug)]
struct NameNode {
    location: Location,
    name: String,
    typ: Type,
}

#[derive(Debug)]
struct ExpressionArrayLiteralNode<'a> {
    location: Location,
    elements: Vec<Expression<'a>>
}

#[derive(Debug)]
struct ExpressionArrayAccessNode<'a> {
    location: Location,
    array_name: String,
    indices: ExpressionArrayLiteralNode<'a>,
    typ: Type
}

#[derive(Debug)]
struct ExpressionLiteralNode {
    location: Location,
    value: String,
    typ: Type,
}

#[derive(Debug)]
struct ExpressionBinaryNode<'a> {
    location: Location,
    operation: Operation,
    lhs: &'a Expression<'a>,
    rhs: &'a Expression<'a>,
    typ: Type
}

#[derive(Debug)]
struct ExpressionCallNode<'a> {
    location: Location,
    function_name: String,
    arguments: Vec<ArgumentNode<'a>>,
    typ: Type
}

#[derive(Debug)]
struct ExpressionBuiltInNode<'a> {
    location: Location,
    function_name: String,
    arguments: Vec<ArgumentNode<'a>>,
    typ: Type
}

#[derive(Debug)]
enum Operation {
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

use crate::parser::{Token, Tree, TreeType, TokenType};

#[derive(Default)]
struct Parser<'a> {
    filepath: PathBuf,
    source: Vec<char>,
    fuel: Cell<u32>,
    root: Option<FileNode<'a>>,
    lookahead: VecDeque<Token>,
    current_char: usize,
    current_line: usize,
    line_start: usize,
    print_debug: bool,
}

impl<'a> Parser<'a> {
    // ---------- Start of Builder Pattern ----------
    pub fn new() -> Self {
        Default::default()
    }

    pub fn filepath(&mut self, filepath: &str) -> Result<&mut Self, String> {
        match fs::read_to_string(filepath) {
            Ok(source) => {
                self.set_filepath_unchecked(filepath);
                self.set_source(&source);
                Ok(self)
            }
            Err(e) => {
                Err(format!(
                    "{}: Parsing file failed because: {}", ERR_STR, e,
                ))
            }
        }
    }

    pub fn debug(&mut self, debug: bool) -> &mut Self {
        self.print_debug = debug;
        self
    }

    pub fn set_filepath_unchecked(&mut self, filepath: &str) {
        self.filepath = PathBuf::from(filepath);
    }

    pub fn set_source(&mut self, source: &str) {
        self.source = source.chars().collect();
    }    
    // ---------- End of Builder Pattern ----------
    // ---------- Start of Lexer ----------

    // ---------- End of Lexer ----------
    // ---------- Start of Parser ----------

    // ---------- End of Parser ----------
}

fn compile() -> Result<(), String> {
    let mut tmp = Parser::new();
    let parser = tmp.filepath("src/test.bu")?.debug(true);
    println!("{}", parser.source.iter().collect::<String>());
    Ok(())
}
pub fn main() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
    // let old_expr = Tree {
    //     typ: TreeType::ExprBinary {
    //         lhs: Box::new(Tree {
    //             typ: TreeType::ExprLiteral {
    //                 typ: Type::I32,
    //             },
    //             tkn: Token::new(TokenType::IntLiteral, "1".to_string(), Location::anonymous())
    //         }),
    //         rhs: Box::new(Tree {
    //             typ: TreeType::ExprLiteral {
    //                 typ: Type::I32,
    //             },
    //             tkn: Token::new(TokenType::IntLiteral, "1".to_string(), Location::anonymous())
    //         }),
    //         typ: Type::Unknown
    //     },
    //     tkn: Token::new(TokenType::Plus, "+".to_string(), Location::anonymous())
    // };
    // let new_expr = ExpressionBinaryNode {
    //     location: Location::anonymous(),
    //     operation: Operation::PLUS,
    //     lhs: &Expression::Literal(ExpressionLiteralNode {
    //         location: Location::anonymous(),
    //         value: "1".to_string(),
    //         typ: Type::I32
    //     }),
    //     rhs: &Expression::Literal(ExpressionLiteralNode {
    //         location: Location::anonymous(),
    //         value: "1".to_string(),
    //         typ: Type::I32
    //     }),
    //     typ: Type::Unknown
    // };
    // println!("{:#?}", new_expr);
    // todo!()
}