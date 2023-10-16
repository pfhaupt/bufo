use super::nodes;

pub struct TypeChecker {

}

impl TypeChecker {
    pub fn new() -> Self {
        Self {  }
    }

    pub fn type_check_file(&mut self, ast: nodes::FileNode) -> Result<nodes::FileNode, String> {
        todo!()
    }
}

trait Typecheckable {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized;
}

impl Typecheckable for nodes::FileNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ClassNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FieldNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FieldAccess {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FeatureNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FunctionNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ParameterNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::BlockNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::LetNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::AssignNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionIdentifierNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::IfNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ReturnNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::TypeNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ArgumentNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionArrayLiteralNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionArrayAccessNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionLiteralNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionBinaryNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionCallNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionFieldAccessNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::NameNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionBuiltInNode {
    fn type_check(self, checker: &mut TypeChecker) -> Result<Self, String> where Self: Sized {
        todo!()
    }
}