use crate::parser::Location;
use crate::checker::Type;
use crate::new::new_parser::Operation;
use derive_builder::Builder;

#[derive(Debug, Default, Builder, Clone)]
pub struct FileNode {
    location: Location,
    filepath: String,
    functions: Vec<FunctionNode>,
    classes: Vec<ClassNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ClassNode {
    location: Location,
    name: String,
    fields: Vec<FieldNode>,
    functions: Vec<FunctionNode>,
    features: Vec<FeatureNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FieldNode {
    location: Location,
    name: String,
    typ: TypeNode,
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
    location: Location,
    name: String,
    return_type: ReturnTypeNode,
    parameters: Vec<ParameterNode>,
    block: BlockNode,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct FunctionNode {
    location: Location,
    name: String,
    return_type: ReturnTypeNode,
    parameters: Vec<ParameterNode>,
    block: BlockNode
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnTypeNode {
    location: Location,
    typ: Type
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ParameterNode {
    location: Location,
    name: String,
    typ: TypeNode
}

#[derive(Debug, Default, Builder, Clone)]
pub struct BlockNode {
    location: Location,
    statements: Vec<Statement>,
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
    location: Location,
    expression: Expression
}

#[derive(Debug, Default, Builder, Clone)]
pub struct LetNode {
    location: Location,
    name: String,
    typ: TypeNode,
    expression: ExpressionNode,
}

#[derive(Debug, Builder, Clone)]
pub struct AssignNode {
    location: Location,
    name: ExpressionIdentifierNode,
    expression: ExpressionNode,
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionIdentifierNode {
    location: Location,
    expression: Box<Expression>,
    typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct IfNode {
    location: Location,
    condition: ExpressionBinaryNode,
    if_branch: BlockNode,
    else_branch: Option<BlockNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct ReturnNode {
    location: Location,
    return_value: Option<ExpressionNode>,
}

#[derive(Debug, Default, Builder, Clone)]
pub struct TypeNode {
    location: Location,
    typ: Type,
    array_dimensions: Option<ExpressionArrayLiteralNode>
}

#[derive(Debug, Builder, Clone)]
pub struct ArgumentNode {
    location: Location,
    expression: ExpressionNode
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
    location: Location,
    elements: Vec<Expression>
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

#[derive(Debug, Builder, Clone)]
pub struct ExpressionFieldAccessNode {
    location: Location,
    name: String,
    field: ExpressionIdentifierNode,
    typ: Type
}

#[derive(Debug, Builder, Clone)]
pub struct NameNode {
    location: Location,
    name: String,
    typ: Type,
}

#[derive(Debug, Builder, Clone)]
pub struct ExpressionBuiltInNode {
    location: Location,
    function_name: String,
    arguments: Vec<ArgumentNode>,
    typ: Type
}
