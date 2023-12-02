use crate::frontend::parser::{Location, Operation};
use crate::middleend::checker::Type;

use tracer::trace_call;

#[derive(Debug, Clone)]
pub struct FileNode {
    pub location: Location,
    pub filepath: String,
    pub functions: Vec<FunctionNode>,
    pub classes: Vec<ClassNode>,
}

#[derive(Debug, Clone)]
pub struct ClassNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<FieldNode>,
    pub methods: Vec<MethodNode>,
    pub features: Vec<FeatureNode>,
    pub has_constructor: bool,
}

#[derive(Debug, Clone)]
pub struct FieldNode {
    pub location: Location,
    pub name: String,
    pub type_def: TypeNode,
}

#[derive(Debug, Clone)]
pub struct FeatureNode {
    pub location: Location,
    pub class_name: String,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub stack_size: usize,
    pub is_constructor: bool,
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
    Expression(ExpressionNode),
    Let(LetNode),
    Assign(AssignNode),
    If(IfNode),
    Return(ReturnNode),
    While(WhileNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode {
    pub location: Location,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct LetNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub expression: ExpressionNode,
}

#[derive(Debug, Clone)]
pub struct AssignNode {
    pub location: Location,
    pub name: ExpressionIdentifierNode,
    pub expression: ExpressionNode,
}

#[derive(Debug, Clone)]
pub struct ExpressionIdentifierNode {
    pub location: Location,
    pub expression: Box<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: ExpressionComparisonNode,
    pub if_branch: BlockNode,
    pub else_branch: Option<BlockNode>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub return_value: Option<ExpressionNode>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub location: Location,
    pub condition: ExpressionComparisonNode,
    pub block: BlockNode,
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

#[derive(Debug, Clone)]
pub struct ArgumentNode {
    pub location: Location,
    pub expression: ExpressionNode,
    pub typ: Type,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expression {
    Name(NameNode),
    Identifier(ExpressionIdentifierNode),
    ArrayLiteral(ExpressionArrayLiteralNode),
    ArrayAccess(ExpressionArrayAccessNode),
    Literal(ExpressionLiteralNode),
    Binary(ExpressionBinaryNode),
    Comparison(ExpressionComparisonNode),
    FieldAccess(ExpressionFieldAccessNode),
    // Parenthesis(ExpressionNode),
    FunctionCall(ExpressionCallNode),
    ConstructorCall(ExpressionConstructorNode),
    BuiltIn(ExpressionBuiltInNode),
}

impl Expression {
    #[trace_call(extra)]
    pub fn get_loc(&self) -> Location {
        match &self {
            Self::Name(e) => e.location.clone(),
            Self::Identifier(e) => e.location.clone(),
            Self::ArrayLiteral(e) => e.location.clone(),
            Self::ArrayAccess(e) => e.location.clone(),
            Self::Literal(e) => e.location.clone(),
            Self::Binary(e) => e.location.clone(),
            Self::Comparison(e) => e.location.clone(),
            Self::FieldAccess(e) => e.location.clone(),
            Self::FunctionCall(e) => e.location.clone(),
            Self::ConstructorCall(e) => e.location.clone(),
            Self::BuiltIn(e) => e.location.clone()
        }
    }

    #[trace_call(extra)]
    pub fn get_type(&self) -> Type {
        match &self {
            Self::Name(e) => e.typ.clone(),
            Self::Identifier(e) => e.typ.clone(),
            Self::ArrayLiteral(e) => e.typ.clone(),
            Self::ArrayAccess(e) => e.typ.clone(),
            Self::Literal(e) => e.typ.clone(),
            Self::Binary(e) => e.typ.clone(),
            Self::Comparison(e) => e.typ.clone(),
            Self::FieldAccess(e) => e.typ.clone(),
            Self::FunctionCall(e) => e.typ.clone(),
            Self::ConstructorCall(e) => e.typ.clone(),
            Self::BuiltIn(e) => e.typ.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionArrayLiteralNode {
    pub location: Location,
    pub elements: Vec<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionArrayAccessNode {
    pub location: Location,
    pub array_name: String,
    pub indices: ExpressionArrayLiteralNode,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionLiteralNode {
    pub location: Location,
    pub value: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionBinaryNode {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionComparisonNode {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionCallNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<ArgumentNode>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionConstructorNode {
    pub location: Location,
    pub class_name: String,
    pub arguments: Vec<ArgumentNode>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionFieldAccessNode {
    pub location: Location,
    pub name: String,
    pub field: ExpressionIdentifierNode,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct NameNode {
    pub location: Location,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ExpressionBuiltInNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<ArgumentNode>,
    pub typ: Type,
}
