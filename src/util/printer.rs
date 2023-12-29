use crate::frontend::nodes;

const INDENT_PER_LEVEL: usize = 2;

pub struct Printer {
}

impl Printer {
    pub fn print(ast: &nodes::FileNode) {
        ast.print(0);
    }
}

trait Printable {
    fn print(&self, indent: usize);
}

impl Printable for nodes::FileNode {
    fn print(&self, indent: usize) {
        println!("{}FileNode", " ".repeat(indent));
        for class in &self.classes {
            class.print(indent + INDENT_PER_LEVEL);
        }
        for function in &self.functions {
            function.print(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ClassNode {
    fn print(&self, indent: usize) {
        println!("{}ClassNode {}", " ".repeat(indent), self.name);
        println!("{}Fields", " ".repeat(indent + INDENT_PER_LEVEL));
        for field in &self.fields {
            field.print(indent + 2 * INDENT_PER_LEVEL);
        }
        println!("{}Methods", " ".repeat(indent + INDENT_PER_LEVEL));
        for method in &self.methods {
            method.print(indent + 2 * INDENT_PER_LEVEL);
        }
        println!("{}Features", " ".repeat(indent + INDENT_PER_LEVEL));
        for feature in &self.features {
            feature.print(indent + 2 * INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::FieldNode {
    fn print(&self, indent: usize) {
        println!("{}FieldNode {}", " ".repeat(indent), self.name);
        self.type_def.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::FeatureNode {
    fn print(&self, indent: usize) {
        println!("{}FeatureNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print(indent + 2 * INDENT_PER_LEVEL);
        }
        self.block.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::FunctionNode {
    fn print(&self, indent: usize) {
        println!("{}FunctionNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print(indent + 2 * INDENT_PER_LEVEL);
        }

        self.block.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::MethodNode {
    fn print(&self, indent: usize) {
        println!("{}MethodNode {}", " ".repeat(indent), self.name);
        println!("{}Return Type", " ".repeat(indent + INDENT_PER_LEVEL));
        self.return_type.print(indent + 2 * INDENT_PER_LEVEL);

        println!("{}Parameters", " ".repeat(indent + INDENT_PER_LEVEL));
        for parameter in &self.parameters {
            parameter.print(indent + 2 * INDENT_PER_LEVEL);
        }

        self.block.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::ParameterNode {
    fn print(&self, indent: usize) {
        println!("{}ParameterNode {}", " ".repeat(indent), self.name);
        self.typ.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BlockNode {
    fn print(&self, indent: usize) {
        println!("{}BlockNode", " ".repeat(indent));
        for statement in &self.statements {
            statement.print(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::Statement {
    fn print(&self, indent: usize) {
        match self {
            nodes::Statement::Expression(node) => node.print(indent),
            nodes::Statement::Let(node) => node.print(indent),
            nodes::Statement::Assign(node) => node.print(indent),
            nodes::Statement::If(node) => node.print(indent),
            nodes::Statement::Return(node) => node.print(indent),
            nodes::Statement::While(node) => node.print(indent),
            nodes::Statement::Break(node) => node.print(indent),
            nodes::Statement::Continue(node) => node.print(indent),
        }
    }
}

impl Printable for nodes::LetNode {
    fn print(&self, indent: usize) {
        println!("{}LetNode {}", " ".repeat(indent), self.name);
        self.typ.print(indent + INDENT_PER_LEVEL);
        self.expression.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::AssignNode {
    fn print(&self, indent: usize) {
        println!("{}AssignNode", " ".repeat(indent));
        self.name.print(indent + INDENT_PER_LEVEL);
        self.expression.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::IfNode {
    fn print(&self, indent: usize) {
        println!("{}IfNode", " ".repeat(indent));
        self.condition.print(indent + INDENT_PER_LEVEL);
        self.if_branch.print(indent + INDENT_PER_LEVEL);
        if let Some(else_block) = &self.else_branch {
            else_block.print(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ReturnNode {
    fn print(&self, indent: usize) {
        println!("{}ReturnNode", " ".repeat(indent));
        if let Some(return_value) = &self.return_value {
            return_value.print(indent + INDENT_PER_LEVEL);
        } else {
            println!("{}None", " ".repeat(indent + INDENT_PER_LEVEL));
        }
    }
}

impl Printable for nodes::WhileNode {
    fn print(&self, indent: usize) {
        println!("{}WhileNode", " ".repeat(indent));
        println!("{}Condition", " ".repeat(indent + INDENT_PER_LEVEL));
        self.condition.print(indent + 2 * INDENT_PER_LEVEL);
        println!("{}Body", " ".repeat(indent + INDENT_PER_LEVEL));
        self.block.print(indent + 2 * INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BreakNode {
    fn print(&self, indent: usize) {
        println!("{}BreakNode", " ".repeat(indent));
    }
}

impl Printable for nodes::ContinueNode {
    fn print(&self, indent: usize) {
        println!("{}ContinueNode", " ".repeat(indent));
    }
}

impl Printable for nodes::TypeNode {
    fn print(&self, indent: usize) {
        println!("{}TypeNode {}", " ".repeat(indent), self.typ);
    }
}

impl Printable for nodes::Expression {
    fn print(&self, indent: usize) {
        match self {
            Self::Name(node) => node.print(indent),
            Self::Identifier(node) => node.print(indent),
            Self::ArrayLiteral(node) => node.print(indent),
            Self::ArrayAccess(node) => node.print(indent),
            Self::Literal(node) => node.print(indent),
            Self::Binary(node) => node.print(indent),
            Self::Comparison(node) => node.print(indent),
            Self::FieldAccess(node) => node.print(indent),
            Self::FunctionCall(node) => node.print(indent),
            Self::BuiltIn(node) => node.print(indent),
        }
    }
}

impl Printable for nodes::ExpressionNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionNode", " ".repeat(indent));
        self.expression.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::NameNode {
    fn print(&self, indent: usize) {
        println!("{}NameNode {}", " ".repeat(indent), self.name);
    }
}

impl Printable for nodes::IdentifierNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionIdentifierNode", " ".repeat(indent));
        self.expression.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::ArrayLiteralNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionArrayLiteralNode", " ".repeat(indent));
        for expression in &self.elements {
            expression.print(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ArrayAccessNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionArrayAccessNode", " ".repeat(indent));
        println!("{}Array {}", " ".repeat(indent + INDENT_PER_LEVEL), self.array_name);
        self.indices.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::LiteralNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        println!("{}Value {}", " ".repeat(indent + INDENT_PER_LEVEL), self.value);
    }
}

impl Printable for nodes::BinaryNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionBinaryNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        self.lhs.print(indent + INDENT_PER_LEVEL);
        self.rhs.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::ComparisonNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionComparisonNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        self.lhs.print(indent + INDENT_PER_LEVEL);
        self.rhs.print(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::CallNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionCallNode", " ".repeat(indent));
        println!("{}Function {}", " ".repeat(indent + INDENT_PER_LEVEL), self.function_name);
        for argument in &self.arguments {
            argument.print(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::FieldAccessNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionFieldAccessNode", " ".repeat(indent));
        println!("{}Name {}", " ".repeat(indent + INDENT_PER_LEVEL), self.name);
        println!("{}Field", " ".repeat(indent + INDENT_PER_LEVEL));
        self.field.print(indent + 2 * INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BuiltInNode {
    fn print(&self, indent: usize) {
        println!("{}ExpressionBuiltInNode", " ".repeat(indent));
        println!("{}Function {}", " ".repeat(indent + INDENT_PER_LEVEL), self.function_name);
        println!("{}Arguments", " ".repeat(indent + INDENT_PER_LEVEL));
        for argument in &self.arguments {
            argument.print(indent + 2 * INDENT_PER_LEVEL);
        }
    }
}