use crate::frontend::parser::{Location, Operation};
use crate::middleend::type_checker::Type;

use tracer::trace_call;

#[derive(Debug, Clone)]
pub struct FileNode {
    pub location: Location,
    pub filepath: String,
    pub functions: Vec<FunctionNode>,
    pub classes: Vec<ClassNode>,
    pub externs: Vec<ExternNode>,
}

#[derive(Debug, Clone)]
pub struct ExternNode {
    pub location: Location,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
}

#[derive(Debug, Clone)]
pub struct ClassNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<FieldNode>,
    pub methods: Vec<MethodNode>,
    pub constructors: Vec<ConstructorNode>,
}

#[derive(Debug, Clone)]
pub struct FieldNode {
    pub location: Location,
    pub name: String,
    pub type_def: TypeNode,
}

#[derive(Debug, Clone)]
pub struct ConstructorNode {
    pub location: Location,
    pub class_name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub location: Location,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct MethodNode {
    pub location: Location,
    pub class_name: String,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
}

impl ParameterNode {
    #[trace_call(extra)]
    pub fn this(location: Location, typ: Type) -> Self {
        Self {
            location: location.clone(),
            name: String::from("this"),
            typ: TypeNode {
                location,
                typ
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub location: Location,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockNode),
    Expression(Expression),
    Let(LetNode),
    If(IfNode),
    Return(ReturnNode),
    While(WhileNode),
    Break(BreakNode),
    Continue(ContinueNode),
}

impl Statement {
    pub fn get_loc(&self) -> Location {
        match self {
            Self::Block(e) => e.location.clone(),
            Self::Expression(e) => e.get_loc(),
            Self::Let(e) => e.location.clone(),
            Self::If(e) => e.location.clone(),
            Self::Return(e) => e.location.clone(),
            Self::While(e) => e.location.clone(),
            Self::Break(e) => e.location.clone(),
            Self::Continue(e) => e.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: Expression,
    pub if_body: Box<Statement>,
    pub else_body: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub return_value: Option<Expression>,
    pub typ: Type,
    pub function: String,
    pub class: Option<String>,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub location: Location,
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct BreakNode {
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct ContinueNode {
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    pub location: Location,
    pub typ: Type,
}

impl TypeNode {
    #[trace_call(extra)]
    pub fn none(location: Location) -> Self {
        Self {
            location,
            typ: Type::None
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expression {
    Name(NameNode),
    ArrayLiteral(ArrayLiteralNode),
    ArrayAccess(ArrayAccessNode),
    Literal(LiteralNode),
    Unary(UnaryNode),
    Binary(BinaryNode),
    // Parenthesis(Expression),
    FunctionCall(CallNode),
    BuiltIn(BuiltInNode),
}

impl Expression {
    #[trace_call(extra)]
    pub fn get_loc(&self) -> Location {
        match &self {
            Self::Name(e) => e.location.clone(),
            Self::ArrayLiteral(e) => e.location.clone(),
            Self::ArrayAccess(e) => e.location.clone(),
            Self::Literal(e) => e.location.clone(),
            Self::Unary(e) => e.location.clone(),
            Self::Binary(e) => e.location.clone(),
            Self::FunctionCall(e) => e.location.clone(),
            Self::BuiltIn(e) => e.location.clone()
        }
    }

    #[trace_call(extra)]
    pub fn get_type(&self) -> Type {
        match &self {
            Self::Name(e) => e.typ.clone(),
            Self::ArrayLiteral(e) => e.typ.clone(),
            Self::ArrayAccess(e) => e.typ.clone(),
            Self::Literal(e) => e.typ.clone(),
            Self::Unary(e) => e.typ.clone(),
            Self::Binary(e) => e.typ.clone(),
            Self::FunctionCall(e) => e.typ.clone(),
            Self::BuiltIn(e) => e.typ.clone()
        }
    }

    #[trace_call(extra)]
    pub fn is_lvalue(&self) -> bool {
        match &self {
            Self::Name(_) => true,
            Self::ArrayAccess(_) => true,
            Self::Binary(e) => {
                e.operation == Operation::Dot
            }
            _ => false
        }
    }

    #[trace_call(extra)]
    pub fn is_arithmetic(&self) -> bool {
        match &self {
            Self::Unary(e) => e.operation.is_arithmetic(),
            Self::Binary(e) => e.operation.is_arithmetic(),
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralNode {
    pub location: Location,
    pub elements: Vec<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ArrayAccessNode {
    pub location: Location,
    pub array_name: String,
    pub indices: ArrayLiteralNode,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct LiteralNode {
    pub location: Location,
    pub value: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct UnaryNode {
    pub location: Location,
    pub operation: Operation,
    pub expression: Box<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct BinaryNode {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub typ: Type,
}

impl BinaryNode {
    #[trace_call(extra)]
    pub fn is_comparison(&self) -> bool {
        self.operation.is_comparison()
    }

    #[trace_call(extra)]
    pub fn is_arithmetic(&self) -> bool {
        self.operation.is_arithmetic()
    }
}

#[derive(Debug, Clone)]
pub struct CallNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub typ: Type,
    pub is_constructor: bool,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct NameNode {
    pub location: Location,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct BuiltInNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub typ: Type,
}
