use crate::frontend::nodes;

const INDENT_PER_LEVEL: usize = 2;

pub struct Printer {
}

impl Printer {
    pub fn print(ast: &nodes::FileNode) {
        ast.print_ast(0);
    }
}

pub trait Printable {
    fn print_ast(&self, indent: usize);
}

impl Printable for nodes::FileNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}FileNode", " ".repeat(indent));
        for ext in &self.externs {
            ext.print_ast(indent + INDENT_PER_LEVEL);
        }
        for strukt in &self.structs {
            strukt.print_ast(indent + INDENT_PER_LEVEL);
        }
        for function in &self.functions {
            function.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ExternNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExternNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print_ast(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print_ast(indent + 2 * INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::StructNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}StructNode {}", " ".repeat(indent), self.name);
        for field in &self.fields {
            field.print_ast(indent + INDENT_PER_LEVEL);
        }
        for method in &self.methods {
            method.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::FieldNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}FieldNode {}", " ".repeat(indent), self.name);
        self.type_def.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::FunctionNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}FunctionNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print_ast(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print_ast(indent + 2 * INDENT_PER_LEVEL);
        }

        self.block.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::MethodNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}MethodNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print_ast(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print_ast(indent + 2 * INDENT_PER_LEVEL);
        }

        self.block.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::ParameterNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ParameterNode {}", " ".repeat(indent), self.name);
        self.typ.print_ast(indent + INDENT_PER_LEVEL);
        println!("{}Mutable {}", " ".repeat(indent + INDENT_PER_LEVEL), self.is_mutable);
    }
}

impl Printable for nodes::BlockNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}BlockNode", " ".repeat(indent));
        for statement in &self.statements {
            statement.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::Statement<'_> {
    fn print_ast(&self, indent: usize) {
        match self {
            nodes::Statement::Block(node) => node.print_ast(indent),
            nodes::Statement::Expression(node) => node.print_ast(indent),
            nodes::Statement::VarDecl(node) => node.print_ast(indent),
            nodes::Statement::If(node) => node.print_ast(indent),
            nodes::Statement::Return(node) => node.print_ast(indent),
            nodes::Statement::While(node) => node.print_ast(indent),
            nodes::Statement::Break(node) => node.print_ast(indent),
            nodes::Statement::Continue(node) => node.print_ast(indent),
        }
    }
}

impl Printable for nodes::VarDeclNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}VarDeclNode {}", " ".repeat(indent), self.name);
        self.typ.print_ast(indent + INDENT_PER_LEVEL);
        println!("{}Mutable {}", " ".repeat(indent + INDENT_PER_LEVEL), self.is_mutable);
        self.expression.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::IfNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}IfNode", " ".repeat(indent));
        self.condition.print_ast(indent + INDENT_PER_LEVEL);
        self.if_body.print_ast(indent + INDENT_PER_LEVEL);
        if let Some(else_block) = &self.else_body {
            else_block.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ReturnNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ReturnNode", " ".repeat(indent));
        if let Some(return_value) = &self.return_value {
            return_value.print_ast(indent + INDENT_PER_LEVEL);
        } else {
            println!("{}None", " ".repeat(indent + INDENT_PER_LEVEL));
        }
    }
}

impl Printable for nodes::WhileNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}WhileNode", " ".repeat(indent));
        println!("{}Condition", " ".repeat(indent + INDENT_PER_LEVEL));
        self.condition.print_ast(indent + 2 * INDENT_PER_LEVEL);
        println!("{}Body", " ".repeat(indent + INDENT_PER_LEVEL));
        self.body.print_ast(indent + 2 * INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BreakNode {
    fn print_ast(&self, indent: usize) {
        println!("{}BreakNode", " ".repeat(indent));
    }
}

impl Printable for nodes::ContinueNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ContinueNode", " ".repeat(indent));
    }
}

impl Printable for nodes::TypeNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}TypeNode {}", " ".repeat(indent), self.typ);
    }
}

impl Printable for nodes::Expression<'_> {
    fn print_ast(&self, indent: usize) {
        match self {
            Self::Name(node) => node.print_ast(indent),
            Self::Literal(node) => node.print_ast(indent),
            Self::StructLiteral(node) => node.print_ast(indent),
            Self::ArrayLiteral(node) => node.print_ast(indent),
            Self::Unary(node) => node.print_ast(indent),
            Self::Binary(node) => node.print_ast(indent),
            Self::FunctionCall(node) => node.print_ast(indent),
            Self::Sizeof(node) => node.print_ast(indent),
            Self::As(_, _) => todo!("Expression::As.print_ast()"),
        }
    }
}

impl Printable for nodes::NameNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}NameNode {}", " ".repeat(indent), self.name);
    }
}

impl Printable for nodes::LiteralNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        println!("{}Value {}", " ".repeat(indent + INDENT_PER_LEVEL), self.value);
    }
}

impl Printable for nodes::StructLiteralNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionStructLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        for field in &self.fields {
            println!("{}Field {}", " ".repeat(indent + INDENT_PER_LEVEL), field.0);
            field.1.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ArrayLiteralNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionArrayLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        for element in &self.elements {
            element.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::UnaryNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionUnaryNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        self.expression.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BinaryNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionBinaryNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        self.lhs.print_ast(indent + INDENT_PER_LEVEL);
        self.rhs.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::CallNode<'_> {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionCallNode", " ".repeat(indent));
        println!("{}Function {}", " ".repeat(indent + INDENT_PER_LEVEL), self.function_name);
        for argument in &self.arguments {
            argument.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}
