use std::collections::{HashMap, VecDeque};
use crate::{checker::Type, parser::Location, codegen::{ERR_STR, NOTE_STR, WARN_STR}};

use super::nodes;

const BUILT_IN_FEATURES: [&str; 1] = ["new"];

#[derive(Debug)]
pub struct Parameter {
    name: String,
    location: Location,
    typ: Type
}

#[derive(Debug)]
pub struct Method {
    name: String,
    location: Location,
    return_type: Type,
    parameters: Vec<Parameter>,
}

impl Method {
    fn add_this_param(&mut self, class_type: &Type) -> Result<(), String> {
        // FIXME: Should this be part of the Parser?
        
        // FIXME: Calculate correct location for this param
        if self.parameters.len() == 0 {
            self.parameters.push(Parameter {
                name: String::from("this"),
                location: self.location.clone(),
                typ: class_type.clone()
            });
        } else {
            let first_param = &mut self.parameters[0];
            if first_param.name == String::from("this") {
                if first_param.typ != *class_type {
                    return Err(format!(
                        "{}: {:?}: Unexpected Type for parameter `this`.\n{}: `this` is an implicit parameter. Specifying it is not necessary.",
                        ERR_STR,
                        first_param.location,
                        NOTE_STR
                    ));
                }
                println!("{}: {:?}: Use of implicit parameter `this`.", WARN_STR, first_param.location);
                return Ok(());
            } else {
                for param in &self.parameters {
                    if param.name == String::from("this") {
                        return Err(format!(
                            "{}: {:?}: Unexpected Parameter. `this` is either implicit or the first parameter.",
                            ERR_STR,
                            param.location
                        ))
                    }
                }
            }
            // We have >0 parameters, none of them are named `this`
            self.parameters.insert(0, Parameter {
                name: String::from("this"),
                location: self.location.clone(),
                typ: class_type.clone()
            });
        }
        Ok(())
    }
}


#[derive(Debug)]
pub struct Function {
    name: String,
    location: Location,
    // FIXME: Also store return declaration for better error reporting
    return_type: Type,
    parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Class {
    name: String,
    class_type: Type,
    location: Location,
    fields: HashMap<String, (Location, Type)>,
    known_methods: HashMap<String, Method>,
    // FIXME: Maybe we should have an own struct for Features
    known_features: HashMap<String, Function>,
    has_constructor: bool,
}

impl Class {
    fn new(name: String) -> Self {
        Self {
            name: name.clone(),
            class_type: Type::Class(name),
            location: Location::anonymous(),
            fields: HashMap::new(),
            known_methods: HashMap::new(),
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

    fn add_method(&mut self, method: &nodes::MethodNode) -> Result<(), String> {
        let name = &method.name;
        let location = &method.location;
        match self.known_methods.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Method redeclaration.\n{}: {:?}: Method already declared here.",
                ERR_STR,
                location,
                NOTE_STR,
                f.location
            )),
            None => {
                let return_type = &method.return_type.typ;
                let parameters: Vec<_> = method.parameters
                    .iter()
                    .map(|param| {
                        Parameter {
                            name: param.name.clone(),
                            location: param.location.clone(),
                            typ: param.typ.typ.clone()
                        }
                    })
                    .collect();
                let method = Method {
                    name: name.clone(),
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters
                };
                self.known_methods.insert(name.clone(), method);
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
                let parameters: Vec<_> = feature.parameters
                    .iter()
                    .map(|param| {
                        Parameter {
                            name: param.name.clone(),
                            location: param.location.clone(),
                            typ: param.typ.typ.clone()
                        }
                    })
                    .collect();
                let func = Function {
                    name: name.clone(),
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters
                };
                self.known_features.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    fn resolve_new_return_type(&mut self) -> Result<(), String> {
        for feat in &mut self.known_features {
            if *feat.0 == String::from("new") {
                if feat.1.return_type == self.class_type {
                    // FIXME: Also store location of return type declaration for better warnings
                    println!(
                        "{}: {:?}: Use of implicit return type for constructor.",
                        WARN_STR,
                        feat.1.location
                    );
                    return Ok(());
                }
                if feat.1.return_type != Type::None {
                    return Err(format!(
                        "{}: {:?}: Feature `new` is expected to return None, found {}.",
                        ERR_STR,
                        feat.1.location,
                        feat.1.return_type
                    ));
                }
                feat.1.return_type = self.class_type.clone();
                return Ok(());
            }
        }
        assert!(false, "Class has constructor but no new feature found");
        unreachable!()
    }

    fn add_this_param(&mut self) -> Result<(), String> {
        // FIXME: Also add this parameter to features, once we have more of them
        for function in &mut self.known_methods {
            function.1.add_this_param(&self.class_type)?;
        }
        Ok(())
    }

    fn get_field(&self, name: &String) -> Option<(Location, Type)> {
        self.fields.get(name).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    location: Location,
    typ: Type,
}

impl Variable {
    fn new(name: String, location: Location, typ: Type) -> Self {
        Self {
            name,
            location,
            typ
        }
    }
    fn is_class_instance(&self) -> bool {
        match &self.typ {
            Type::Class(..) => true,
            _ => false
        }
    }
    fn get_class_name(&self) -> String {
        match &self.typ {
            Type::Class(name) => name.clone(),
            _ => panic!()
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    known_functions: HashMap<String, Function>,
    known_classes: HashMap<String, Class>,
    known_variables: VecDeque<HashMap<String, Variable>>,
    current_function: String,
    current_class: String,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self { 
            known_classes: HashMap::new(),
            known_functions: HashMap::new(),
            known_variables: VecDeque::new(),
            current_function: String::new(),
            current_class: String::new()
         }
    }

    fn add_function(&mut self, function: &nodes::FunctionNode) -> Result<(), String> {
        let name = &function.name;
        let location = &function.location;
        match self.known_functions.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Function redeclaration.\n{}: {:?}: Function already declared here.",
                ERR_STR,
                location,
                NOTE_STR,
                f.location
            )),
            None => {
                let return_type = &function.return_type.typ;
                // FIXME: Check if parameter names repeat
                let parameters: Vec<_> = function.parameters
                .iter()
                .map(|param| {
                    Parameter {
                        name: param.name.clone(),
                        location: param.location.clone(),
                        typ: param.typ.typ.clone()
                    }
                })
                .collect();
                let func = Function {
                    name: name.clone(),
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters
                };
                self.known_functions.insert(name.clone(), func);
                Ok(())
            }
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
                    let mut class = Class::new(c.name.clone());
                    class.location = c.location.clone();
                    class.has_constructor = c.has_constructor;
                    for field in &c.fields {
                        class.add_field(field)?;
                    }
                    for method in &c.methods {
                        class.add_method(method)?;
                    }
                    for feature in &c.features {
                        class.add_feature(feature)?;
                    }
                    if class.has_constructor {
                        class.resolve_new_return_type()?;
                    }
                    class.add_this_param()?;
                    self.known_classes.insert(c.name.clone(), class);
                }
            }
        }
        for function in &ast.functions {
            self.add_function(function)?;
        }
        Ok(())
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