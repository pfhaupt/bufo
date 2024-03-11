use crate::frontend::nodes;

const INDENT_PER_LEVEL: usize = 2;

pub struct Printer {
}

impl Printer {
    pub fn print(ast: &nodes::ModuleNode) {
        ast.print_ast(0);
    }
}

pub trait Printable {
    fn print(&self) {
        self.print_ast(0);
    }
    fn print_ast(&self, indent: usize);
}

impl Printable for nodes::ModuleNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ModuleNode", " ".repeat(indent));
        println!("{}Name {}", " ".repeat(indent + INDENT_PER_LEVEL), self.name);
        for import in &self.imports {
            import.print_ast(indent + INDENT_PER_LEVEL);
        }
        for module in &self.modules {
            module.print_ast(indent + INDENT_PER_LEVEL);
        }
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

impl Printable for nodes::ImportNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ImportNode", " ".repeat(indent));
        println!("{}Module", " ".repeat(indent + INDENT_PER_LEVEL));
        for module in &self.trace {
            println!("{}{}", " ".repeat(indent + 2 * INDENT_PER_LEVEL), module.0);
        }
    }
}

impl Printable for nodes::ExternNode {
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

impl Printable for nodes::StructNode {
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

impl Printable for nodes::FieldNode {
    fn print_ast(&self, indent: usize) {
        println!("{}FieldNode {}", " ".repeat(indent), self.name);
        self.type_def.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::FunctionNode {
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

impl Printable for nodes::MethodNode {
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

impl Printable for nodes::ParameterNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ParameterNode {}", " ".repeat(indent), self.name);
        self.typ.print_ast(indent + INDENT_PER_LEVEL);
        println!("{}Mutable {}", " ".repeat(indent + INDENT_PER_LEVEL), self.is_mutable);
    }
}

impl Printable for nodes::BlockNode {
    fn print_ast(&self, indent: usize) {
        println!("{}BlockNode", " ".repeat(indent));
        for statement in &self.statements {
            statement.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::Statement {
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

impl Printable for nodes::VarDeclNode {
    fn print_ast(&self, indent: usize) {
        println!("{}VarDeclNode {}", " ".repeat(indent), self.name);
        self.typ.print_ast(indent + INDENT_PER_LEVEL);
        println!("{}Mutable {}", " ".repeat(indent + INDENT_PER_LEVEL), self.is_mutable);
        self.expression.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::IfNode {
    fn print_ast(&self, indent: usize) {
        println!("{}IfNode", " ".repeat(indent));
        self.condition.print_ast(indent + INDENT_PER_LEVEL);
        self.if_body.print_ast(indent + INDENT_PER_LEVEL);
        if let Some(else_block) = &self.else_body {
            else_block.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ReturnNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ReturnNode", " ".repeat(indent));
        if let Some(return_value) = &self.return_value {
            return_value.print_ast(indent + INDENT_PER_LEVEL);
        } else {
            println!("{}None", " ".repeat(indent + INDENT_PER_LEVEL));
        }
    }
}

impl Printable for nodes::WhileNode {
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

impl Printable for nodes::TypeNode {
    fn print_ast(&self, indent: usize) {
        println!("{}TypeNode {}", " ".repeat(indent), self.typ);
    }
}

impl Printable for nodes::Expression {
    fn print_ast(&self, indent: usize) {
        match self {
            Self::Name(node) => node.print_ast(indent),
            Self::Literal(node) => node.print_ast(indent),
            Self::StructLiteral(node) => node.print_ast(indent),
            Self::ArrayLiteral(node) => node.print_ast(indent),
            Self::Unary(node) => node.print_ast(indent),
            Self::Binary(node) => node.print_ast(indent),
            Self::FunctionCall(node) => node.print_ast(indent),
        }
    }
}

impl Printable for nodes::NameNode {
    fn print_ast(&self, indent: usize) {
        println!("{}NameNode {}", " ".repeat(indent), self.name);
    }
}

impl Printable for nodes::LiteralNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        println!("{}Value {}", " ".repeat(indent + INDENT_PER_LEVEL), self.value);
    }
}

impl Printable for nodes::StructLiteralNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionStructLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        for field in &self.fields {
            println!("{}Field {}", " ".repeat(indent + INDENT_PER_LEVEL), field.0);
            field.1.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::ArrayLiteralNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionArrayLiteralNode", " ".repeat(indent));
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        for element in &self.elements {
            element.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}

impl Printable for nodes::UnaryNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionUnaryNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        self.expression.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::BinaryNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionBinaryNode", " ".repeat(indent));
        println!("{}Operator {}", " ".repeat(indent + INDENT_PER_LEVEL), self.operation);
        println!("{}Type {}", " ".repeat(indent + INDENT_PER_LEVEL), self.typ);
        self.lhs.print_ast(indent + INDENT_PER_LEVEL);
        self.rhs.print_ast(indent + INDENT_PER_LEVEL);
    }
}

impl Printable for nodes::CallNode {
    fn print_ast(&self, indent: usize) {
        println!("{}ExpressionCallNode", " ".repeat(indent));
        println!("{}Function {}", " ".repeat(indent + INDENT_PER_LEVEL), self.function_name);
        for argument in &self.arguments {
            argument.print_ast(indent + INDENT_PER_LEVEL);
        }
    }
}
