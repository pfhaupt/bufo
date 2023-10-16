use crate::parser::Location;
use crate::checker::Type;
use crate::new::new_parser::Operation;
use derive_builder::Builder;

#[derive(Debug, Default, Builder, Clone)]
pub struct FileNode {
    location: Location,
    filepath: String,
    pub functions: Vec<FunctionNode>,
    pub features: Vec<FeatureNode>,
    pub classes: Vec<ClassNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ClassNode {
    location: Location,
    pub name: String,
    pub fields: Vec<FieldNode>,
    pub functions: Vec<FunctionNode>,
    pub features: Vec<FeatureNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FieldNode {
    location: Location,
    name: String,
    pub type_def: TypeNode,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FieldAccess {
    location: Location,
    name: String,
    field: Expression,
    typ: Type
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FeatureNode {
    pub location: Location,
    pub name: String,
    pub return_type: ReturnTypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FunctionNode {
    pub location: Location,
    pub name: String,
    pub return_type: ReturnTypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnTypeNode {
    pub location: Location,
    pub typ: Type
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ParameterNode {
    location: Location,
    name: String,
    typ: TypeNode
}

#[derive(Debug, Default, Builder, Clone)]
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
    Return(ReturnNode)
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ExpressionNode {
    pub location: Location,
    pub expression: Expression
}

#[derive(Debug, Default, Builder, Clone)]
pub struct LetNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub expression: ExpressionNode,
}

#[derive(Debug, Builder, Clone)]
pub struct AssignNode {
    pub location: Location,
    pub name: ExpressionIdentifierNode,
    pub expression: ExpressionNode,
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionIdentifierNode {
    pub location: Location,
    pub expression: Box<Expression>,
    pub typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: ExpressionBinaryNode,
    pub if_branch: BlockNode,
    pub else_branch: Option<BlockNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub return_value: Option<ExpressionNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct TypeNode {
    pub location: Location,
    pub typ: Type,
}

#[derive(Debug, Builder, Clone)]
pub struct ArgumentNode {
    pub location: Location,
    pub expression: ExpressionNode
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

#[derive(Debug, Builder, Clone)]
pub struct ExpressionArrayLiteralNode {
    pub location: Location,
    pub elements: Vec<Expression>
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

#[derive(Debug, Builder, Clone)]
pub struct ExpressionBinaryNode {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionCallNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<ArgumentNode>,
    pub typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionFieldAccessNode {
    pub location: Location,
    pub name: String,
    pub field: ExpressionIdentifierNode,
    pub typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct NameNode {
    location: Location,
    name: String,
    typ: Type,
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionBuiltInNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<ArgumentNode>,
    pub typ: Type
}
