use std::collections::HashMap;
use crate::{checker::Type, parser::Location, codegen::{ERR_STR, NOTE_STR}};

use super::nodes;

const BUILT_IN_FEATURES: [&str; 1] = ["new"];

pub struct Function {
    name: String,
    location: Location,
    return_type: Type,
    parameters: Vec<Type>,
}

pub struct Class {
    location: Location,
    fields: HashMap<String, (Location, Type)>,
    known_functions: HashMap<String, Function>,
    // FIXME: Maybe we should have an own struct for Features
    known_features: HashMap<String, Function>,
    has_constructor: bool,
}

impl Class {
    fn new() -> Self {
        Self {
            location: Location::anonymous(),
            fields: HashMap::new(),
            known_functions: HashMap::new(),
            known_features: HashMap::new(),
            has_constructor: false,
        }
    }

    fn add_field(&mut self, field: &nodes::FieldNode) -> Result<(), String> {
        let name = &field.name;
        let location = &field.location;
        
        match self.fields.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Field redeclaration.\n{}: {:?}: Field already declared here.",
                ERR_STR,
                location,
                NOTE_STR,
                f.0
            )),
            None => {
                let typ = &field.type_def.typ;
                self.fields.insert(name.to_string(), (location.clone(), typ.clone()));
                Ok(())
            }
        }
    }

    fn add_function(&mut self, function: &nodes::FunctionNode) -> Result<(), String> {
        debug_assert!(function.is_method);
        let name = &function.name;
        let location = &function.location;
        match self.known_functions.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Method redeclaration.\n{}: {:?}: Method already declared here.",
                ERR_STR,
                location,
                NOTE_STR,
                f.location
            )),
            None => {
                let return_type = &function.return_type.typ;
                let parameters: Vec<Type> = function.parameters
                    .iter()
                    .map(|param|param.typ.typ.clone())
                    .collect();
                let func = Function {
                    name: name.clone(),
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters: parameters
                };
                self.known_functions.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    fn add_feature(&mut self, feature: &nodes::FeatureNode) -> Result<(), String> {
        let name = &feature.name;
        let location = &feature.location;
        match self.known_features.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Feature redeclaration.\n{}: {:?}: Feature already declared here.",
                ERR_STR,
                location,
                NOTE_STR,
                f.location
            )),
            None => {
                if !BUILT_IN_FEATURES.contains(&name.as_str()) {
                    // FIXME: Do we need to show all features? If yes, how do we display them nicely?
                    return Err(format!(
                        "{}: {:?}: Unknown feature `{}`.\n{}: This is a list of all features: {:?}",
                        ERR_STR,
                        location,
                        name,
                        NOTE_STR,
                        BUILT_IN_FEATURES
                    ));
                }
                let return_type = &feature.return_type.typ;
                let parameters: Vec<Type> = feature.parameters
                    .iter()
                    .map(|param|param.typ.typ.clone())
                    .collect();
                let func = Function {
                    name: name.clone(),
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters: parameters
                };
                self.known_features.insert(name.clone(), func);
                Ok(())
            }
        }
    }
}

pub struct TypeChecker {
    known_functions: HashMap<String, Function>,
    known_classes: HashMap<String, Class>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self { 
            known_classes: HashMap::new(),
            known_functions: HashMap::new(),
         }
    }

    fn fill_lookup(&mut self, ast: &nodes::FileNode) -> Result<(), String> {
        for c in &ast.classes {
            match self.known_classes.get(&c.name) {
                Some(class) => return Err(format!(
                    "{}: {:?}: Class redeclaration.\n{}: {:?}: Class already declared here.",
                    ERR_STR,
                    c.location,
                    NOTE_STR,
                    class.location
                )),
                None => {
                    let mut class = Class::new();
                    class.location = c.location.clone();
                    for field in &c.fields {
                        class.add_field(field)?;
                    }
                    for function in &c.functions {
                        class.add_function(function)?;
                    }
                    for feature in &c.features {
                        class.add_feature(feature)?;
                    }
                    self.known_classes.insert(c.name.clone(), class);
                }
            }
        }


        todo!()
    }

    pub fn type_check_file(&mut self, ast: &mut nodes::FileNode) -> Result<(), String> {
        self.fill_lookup(ast)?;
        ast.type_check(self)
    }
}

trait Typecheckable {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized;
}

impl Typecheckable for nodes::FileNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        for c in &mut self.classes {
            c.type_check(checker)?;
        }
        for f in &mut self.functions {
            f.type_check(checker)?;
        }
        Ok(())
    }
}
impl Typecheckable for nodes::ClassNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FieldNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FieldAccess {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FeatureNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::FunctionNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ParameterNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::BlockNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::LetNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::AssignNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionIdentifierNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::IfNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ReturnNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::TypeNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ArgumentNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionArrayLiteralNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionArrayAccessNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionLiteralNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionBinaryNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionCallNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionFieldAccessNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::NameNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionBuiltInNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<(), String> where Self: Sized {
        todo!()
    }
}