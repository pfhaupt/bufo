use std::collections::HashMap;

use crate::{new::{new_parser, nodes}, codegen::SizeManager};

pub struct Desugarer {
    current_class: String,
    class_sizes: HashMap<String, usize>,
    sm: SizeManager,
}

trait Desugarable {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized;
}

impl Desugarable for nodes::FileNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ClassNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FieldNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FieldAccess {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FeatureNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::FunctionNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ReturnTypeNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ParameterNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::BlockNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::LetNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::AssignNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionIdentifierNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::IfNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ReturnNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::TypeNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ArgumentNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionArrayLiteralNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionArrayAccessNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionLiteralNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionBinaryNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionCallNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionFieldAccessNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::NameNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Desugarable for nodes::ExpressionBuiltInNode {
    fn desugar(desugarer: &mut Desugarer) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}