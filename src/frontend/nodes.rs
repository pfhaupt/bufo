use crate::frontend::parser::{Location, Operation};
use crate::middleend::type_checker::Type;

use tracer::trace_call;

#[derive(Debug, Clone)]
pub struct FileNode {
    pub location: Location,
    pub filepath: String,
    pub functions: Vec<FunctionNode>,
    pub structs: Vec<StructNode>,
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
pub struct StructNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<FieldNode>,
    pub methods: Vec<MethodNode>,
}

#[derive(Debug, Clone)]
pub struct FieldNode {
    pub location: Location,
    pub name: String,
    pub type_def: TypeNode,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub location: Location,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    #[cfg(not(feature = "llvm"))]
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct MethodNode {
    pub location: Location,
    pub struct_name: String,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    #[cfg(not(feature = "llvm"))]
    pub stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub location: Location,
    pub statements: Vec<Statement>,
    #[cfg(feature = "llvm")]
    pub llvm_has_terminator: bool,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockNode),
    Expression(Expression),
    VarDecl(VarDeclNode),
    If(IfNode),
    Return(ReturnNode),
    While(WhileNode),
    Break(BreakNode),
    Continue(ContinueNode),
}

impl Statement {
    pub fn get_loc(&self) -> Location {
        match self {
            Self::Block(e) => e.location,
            Self::Expression(e) => e.get_loc(),
            Self::VarDecl(e) => e.location,
            Self::If(e) => e.location,
            Self::Return(e) => e.location,
            Self::While(e) => e.location,
            Self::Break(e) => e.location,
            Self::Continue(e) => e.location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub expression: Expression,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: Expression,
    pub if_body: Box<BlockNode>,
    pub else_body: Option<BlockNode>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub return_value: Option<Expression>,
    pub typ: Type,
    pub function: String,
    pub strukt: Option<String>,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub location: Location,
    pub condition: Expression,
    pub body: BlockNode,
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
    #[trace_call(extra)]
    pub fn this(location: Location, struct_name: &str) -> Self {
        Self {
            location,
            typ: Type::Struct(struct_name.to_string())
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
    StructLiteral(StructLiteralNode),
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
            Self::Name(e) => e.location,
            Self::ArrayLiteral(e) => e.location,
            Self::ArrayAccess(e) => e.location,
            Self::Literal(e) => e.location,
            Self::StructLiteral(e) => e.location,
            Self::Unary(e) => e.location,
            Self::Binary(e) => e.location,
            Self::FunctionCall(e) => e.location,
            Self::BuiltIn(e) => e.location
        }
    }

    #[trace_call(extra)]
    pub fn get_type(&self) -> Type {
        match &self {
            Self::Name(e) => e.typ.clone(),
            Self::ArrayLiteral(e) => e.typ.clone(),
            Self::ArrayAccess(e) => e.typ.clone(),
            Self::StructLiteral(e) => e.typ.clone(),
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
pub struct StructLiteralNode {
    pub location: Location,
    pub struct_name: String,
    pub fields: Vec<(String, Expression)>,
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

    #[trace_call(extra)]
    pub fn is_bitwise(&self) -> bool {
        self.operation.is_bitwise()
    }
}

#[derive(Debug, Clone)]
pub struct CallNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub typ: Type,
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
