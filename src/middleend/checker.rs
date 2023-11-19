use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::Location;

use crate::compiler::{ERR_STR, NOTE_STR};
use crate::compiler::CONSTRUCTOR_NAME;
use crate::internal_error;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum Type {
    None, // For functions that return nothing
    #[default]
    Unknown,
    I32,
    I64,
    U32,
    U64,
    Usize,
    Bool,
    // Ptr(Box<Type>),
    Arr(Box<Type>, Vec<usize>),
    Class(String),
    // Reserved for later use
    F32,
    F64,
}

impl Type {
    pub fn size(&self) -> Result<usize, String> {
        match self {
            Type::Arr(t, size) => Ok(t.size()? * size.iter().product::<usize>()),
            Type::I32 | Type::U32 | Type::F32 | Type::Bool => Ok(4),
            Type::I64 | Type::U64 | Type::F64 | Type::Usize => Ok(8),
            Type::Class(..) => Ok(4),
            Type::None => internal_error!("Something attempted to get the size of Type::None!"),
            Type::Unknown => internal_error!("Something attempted to get the size of Type::Unknown!"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            // Type::Ptr(t) => write!(fmt, "&{}", t),
            Type::Class(str) => write!(fmt, "{}", str),
            Type::Arr(t, s) => write!(fmt, "{t}{s:?}"),
            _ => write!(fmt, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Parameter {
    name: String,
    location: Location,
    typ: Type,
}

#[derive(Debug)]
pub struct Method {
    location: Location,
    return_type: Type,
    parameters: Vec<Parameter>,
}

impl Method {}

#[derive(Debug)]
pub struct Function {
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
                ERR_STR, location, NOTE_STR, f.0
            )),
            None => {
                let typ = &field.type_def.typ;
                self.fields
                    .insert(name.to_string(), (location.clone(), typ.clone()));
                Ok(())
            }
        }
    }

    fn add_method(&mut self, method: &mut nodes::MethodNode) -> Result<(), String> {
        let name = &method.name;
        let location = &method.location;
        match self.known_methods.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Method redeclaration.\n{}: {:?}: Method already declared here.",
                ERR_STR, location, NOTE_STR, f.location
            )),
            None => {
                let return_type = &method.return_type.typ;
                method.parameters.insert(0, nodes::ParameterNode::this(
                    method.location.clone(),
                    Type::Class(method.class_name.clone())
                ));
                let parameters: Vec<_> = method
                    .parameters
                    .iter()
                    .map(|param| Parameter {
                        name: param.name.clone(),
                        location: param.location.clone(),
                        typ: param.typ.typ.clone(),
                    })
                    .collect();
                let method = Method {
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters,
                };
                self.known_methods.insert(name.clone(), method);
                Ok(())
            }
        }
    }

    fn add_feature(&mut self, feature: &mut nodes::FeatureNode) -> Result<(), String> {
        let name = &feature.name;
        let location = &feature.location;
        match self.known_features.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Feature redeclaration.\n{}: {:?}: Feature already declared here.",
                ERR_STR, location, NOTE_STR, f.location
            )),
            None => {
                let return_type = &feature.return_type.typ;
                if name != CONSTRUCTOR_NAME {
                    feature.parameters.insert(0, nodes::ParameterNode::this(
                        feature.location.clone(),
                        Type::Class(feature.class_name.clone())
                    ));
                }
                let parameters: Vec<_> = feature
                    .parameters
                    .iter()
                    .map(|param| Parameter {
                        name: param.name.clone(),
                        location: param.location.clone(),
                        typ: param.typ.typ.clone(),
                    })
                    .collect();
                let func = Function {
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters,
                };
                self.known_features.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    fn resolve_new_return_type(&mut self) -> Result<(), String> {
        for feat in &mut self.known_features {
            if *feat.0 == CONSTRUCTOR_NAME {
                if feat.1.return_type == self.class_type {
                    // FIXME: Also store location of return type declaration for better warnings
                    return Err(format!(
                        "{}: {:?}: Use of implicit return type for constructor.",
                        ERR_STR,
                        feat.1.location
                    ));
                } else if feat.1.return_type != Type::None {
                    return Err(format!(
                        "{}: {:?}: Feature `{CONSTRUCTOR_NAME}` is expected to return None, found {}.",
                        ERR_STR, feat.1.location, feat.1.return_type
                    ));
                } else {
                    feat.1.return_type = self.class_type.clone();
                    return Ok(());
                }
            }
        }
        panic!("Class has constructor but no feature `{CONSTRUCTOR_NAME}` found");
    }

    fn get_field(&self, name: &str) -> Option<(Location, Type)> {
        self.fields.get(name).cloned()
    }

    fn get_feature(&self, name: &str) -> Option<&Function> {
        self.known_features.get(name)
    }

    fn get_method(&self, name: &str) -> Option<&Method> {
        self.known_methods.get(name)
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
            typ,
        }
    }
    fn is_class_instance(&self) -> bool {
        matches!(&self.typ, Type::Class(..))
    }
    fn get_class_name(&self) -> String {
        match &self.typ {
            Type::Class(name) => name.clone(),
            _ => panic!(),
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
    current_stack_size: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            known_classes: HashMap::new(),
            known_functions: HashMap::new(),
            known_variables: VecDeque::new(),
            current_function: String::new(),
            current_class: String::new(),
            current_stack_size: 0,
        }
    }

    fn add_function(&mut self, function: &nodes::FunctionNode) -> Result<(), String> {
        let name = &function.name;
        let location = &function.location;
        match self.known_functions.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Function redeclaration.\n{}: {:?}: Function already declared here.",
                ERR_STR, location, NOTE_STR, f.location
            )),
            None => {
                let return_type = &function.return_type.typ;
                // FIXME: Check if parameter names repeat
                let parameters: Vec<_> = function
                    .parameters
                    .iter()
                    .map(|param| Parameter {
                        name: param.name.clone(),
                        location: param.location.clone(),
                        typ: param.typ.typ.clone(),
                    })
                    .collect();
                let func = Function {
                    location: location.clone(),
                    return_type: return_type.clone(),
                    parameters,
                };
                self.known_functions.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    fn fill_lookup(&mut self, ast: &mut nodes::FileNode) -> Result<(), String> {
        for c in &mut ast.classes {
            match self.known_classes.get(&c.name) {
                Some(class) => {
                    return Err(format!(
                        "{}: {:?}: Class redeclaration.\n{}: {:?}: Class already declared here.",
                        ERR_STR, c.location, NOTE_STR, class.location
                    ))
                }
                None => {
                    let mut class = Class::new(c.name.clone());
                    class.location = c.location.clone();
                    class.has_constructor = c.has_constructor;
                    for field in &c.fields {
                        class.add_field(field)?;
                    }
                    for method in &mut c.methods {
                        class.add_method(method)?;
                    }
                    for feature in &mut c.features {
                        class.add_feature(feature)?;
                    }
                    if class.has_constructor {
                        class.resolve_new_return_type()?;
                    }
                    self.known_classes.insert(c.name.clone(), class);
                }
            }
        }
        for function in &ast.functions {
            self.add_function(function)?;
        }
        Ok(())
    }

    pub fn type_check_file(&mut self, ast: &mut nodes::FileNode) -> Result<Type, String> {
        self.fill_lookup(ast)?;
        ast.type_check(self)
    }

    fn add_scope(&mut self) {
        self.known_variables.push_back(HashMap::new());
    }

    fn remove_scope(&mut self) {
        self.known_variables.pop_back();
    }

    fn get_current_scope(&mut self) -> &mut HashMap<String, Variable> {
        let len = self.known_variables.len();
        &mut self.known_variables[len - 1]
    }

    fn get_variable(&self, name: &String) -> Option<Variable> {
        for scope in self.known_variables.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }
}

trait Typecheckable {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized;
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized;
}

impl Typecheckable for nodes::FileNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        for c in &mut self.classes {
            c.type_check(checker)?;
        }
        for f in &mut self.functions {
            f.type_check(checker)?;
        }
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("FileNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::ClassNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(checker.current_class.is_empty());
        debug_assert!(checker.current_function.is_empty());
        checker.current_class = self.name.clone();
        // At this point, all known classes have entered the lookup
        // and all fields are unique
        // Only thing left to test is that the field types exist
        // and that functions and methods type check
        for field in &mut self.fields {
            field.type_check(checker)?;
        }
        for function in &mut self.methods {
            function.type_check(checker)?;
        }
        for feature in &mut self.features {
            feature.type_check(checker)?;
        }
        checker.current_class.clear();
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ClassNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::FieldNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        self.type_def.type_check(checker)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("FieldNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::FeatureNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(checker.current_function.is_empty());
        debug_assert!(checker.known_variables.is_empty());
        debug_assert!(checker.known_classes.contains_key(&self.class_name));
        debug_assert!(checker.current_stack_size == 0);

        for param in &mut self.parameters {
            param.type_check(checker)?;
        }

        checker.current_function = self.name.clone();
        let Some(class_info) = checker.known_classes.get(&self.class_name) else { unreachable!() };
        let Some(feature) = class_info.known_features.get(&self.name) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = HashMap::new();
        if self.is_constructor {
            parameters.insert(String::from("this"),
            Variable::new(
                String::from("this"),
                self.location.clone(),
                Type::Class(self.class_name.clone()),
            ));
        }
        for param in &feature.parameters {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.insert(param.name.clone(), var) {
                if p.name == *"this" {
                    return Err(format!(
                        "{}: {:?}: Use of implicit parameter `this`.",
                        ERR_STR, param.location
                    ));
                }
                return Err(format!(
                    "{}: {:?}: Parameter redeclaration!\n{}: {:?}: Parameter `{}` already declared here.",
                    ERR_STR,
                    param.location,
                    NOTE_STR,
                    p.location,
                    p.name
                ));
            }
        }
        if parameters.len() > 4 {
            return Err(format!(
                "{}: {:?}: Features can have at most 4 parameters.\n{}: The implicit parameter `this` counts towards that limit.",
                ERR_STR,
                self.location,
                NOTE_STR
            ));
        }
        checker.known_variables.push_back(parameters);

        self.block.type_check(checker)?;
        self.stack_size = checker.current_stack_size;

        checker.current_stack_size = 0;
        checker.current_function.clear();
        checker.known_variables.clear();
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("FeatureNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::FunctionNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(checker.current_function.is_empty());
        debug_assert!(checker.current_class.is_empty());
        debug_assert!(checker.known_variables.is_empty());
        debug_assert!(checker.known_functions.contains_key(&self.name));
        debug_assert!(checker.current_stack_size == 0);

        for param in &mut self.parameters {
            param.type_check(checker)?;
        }

        checker.current_function = self.name.clone();
        let Some(function) = checker.known_functions.get(&self.name) else { unreachable!() };

        let mut parameters = HashMap::new();
        for param in &function.parameters {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.insert(param.name.clone(), var) {
                return Err(format!(
                    "{}: {:?}: Parameter redeclaration!\n{}: {:?}: Parameter `{}` already declared here.",
                    ERR_STR,
                    param.location,
                    NOTE_STR,
                    p.location,
                    p.name
                ));
            }
        }
        if parameters.len() > 4 {
            return Err(format!(
                "{}: {:?}: Functions can have at most 4 parameters.",
                ERR_STR,
                self.location
            ));
        }
        checker.known_variables.push_back(parameters);

        self.block.type_check(checker)?;
        self.stack_size = checker.current_stack_size;

        checker.current_stack_size = 0;
        checker.current_function.clear();
        checker.known_variables.clear();
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("FunctionNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::MethodNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(checker.current_function.is_empty());
        debug_assert!(checker.known_variables.is_empty());
        debug_assert!(checker.known_classes.contains_key(&self.class_name));
        debug_assert!(checker.current_stack_size == 0);

        for param in &mut self.parameters {
            param.type_check(checker)?;
        }
        checker.current_function = self.name.clone();
        let Some(class_info) = checker.known_classes.get(&self.class_name) else { unreachable!() };
        // FIXME: Is the else actually unreachable for the method?
        let Some(method) = class_info.known_methods.get(&self.name) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = HashMap::new();

        for param in &method.parameters {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.insert(param.name.clone(), var) {
                if param.name == *"this" {
                    return Err(format!(
                        "{}: {:?}: Use of implicit parameter `this`.",
                        ERR_STR, param.location
                    ));
                } else {
                    return Err(format!(
                        "{}: {:?}: Parameter redeclaration.\n{}: {:?}: Parameter `{}` already declared here.",
                        ERR_STR,
                        param.location,
                        NOTE_STR,
                        p.location,
                        param.name
                    ))
                }
            }
        }
        if parameters.len() > 4 {
            return Err(format!(
                "{}: {:?}: Methods can have at most 4 parameters.\n{}: The implicit parameter `this` counts towards that limit.",
                ERR_STR,
                self.location,
                NOTE_STR
            ));
        }
        checker.known_variables.push_back(parameters);

        self.block.type_check(checker)?;
        self.stack_size = checker.current_stack_size;

        checker.current_stack_size = 0;
        checker.current_function.clear();
        checker.known_variables.clear();
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::ParameterNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        self.typ.type_check(checker)?;
        let var_size = self.typ.typ.size()?;
        checker.current_stack_size += var_size;
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::BlockNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        checker.add_scope();
        for statement in &mut self.statements {
            statement.type_check(checker)?;
        }
        checker.remove_scope();
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        self.expression.type_check(checker)
    }
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        self.expression.type_check_with_type(checker, typ)
    }
}
impl Typecheckable for nodes::Statement {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        match self {
            Self::Expression(expression) => expression.type_check(checker),
            Self::Let(let_node) => let_node.type_check(checker),
            Self::Assign(assignment) => assignment.type_check(checker),
            Self::If(if_node) => if_node.type_check(checker),
            Self::Return(return_node) => return_node.type_check(checker),
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("Statement::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::LetNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        match checker.get_variable(&self.name) {
            Some(var) => Err(format!(
                "{}: {:?}: Variable redeclaration.\n{}: {:?}: Variable `{}` already declared here.",
                ERR_STR, self.location, NOTE_STR, var.location, var.name
            )),
            None => {
                self.typ.type_check(checker)?;

                let var_size = self.typ.typ.size()?;
                checker.current_stack_size += var_size;

                let current_scope = checker.get_current_scope();
                let var = Variable {
                    name: self.name.clone(),
                    location: self.location.clone(),
                    typ: self.typ.typ.clone(),
                };
                debug_assert!(current_scope.insert(self.name.clone(), var).is_none());
                let expected_type = &self.typ.typ;

                let expr_type = self.expression.type_check(checker)?;
                debug_assert!(expr_type != Type::None);

                if expr_type == Type::Unknown {
                    // Couldnt determine type of expression
                    // We need to `infer` it
                    self.expression
                        .type_check_with_type(checker, expected_type)?;
                    Ok(expr_type)
                } else if expr_type != *expected_type {
                    Err(format!(
                        "{}: {:?}: Type Mismatch! Expected type `{}`, found type `{}`.",
                        ERR_STR, self.location, expected_type, expr_type
                    ))
                } else {
                    Ok(expr_type)
                }
            }
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::AssignNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let expected_type = self.name.type_check(checker)?;
        let rhs_type = self.expression.type_check(checker)?;
        if rhs_type == Type::Unknown {
            // We need to try and force the type of LHS to RHS
            self.expression
                .type_check_with_type(checker, &expected_type)?;
            Ok(rhs_type)
        } else if rhs_type != expected_type {
            Err(format!(
                "{}: {:?}: Type Mismatch! Expected type `{}`, found type `{}`.",
                ERR_STR, self.expression.location, expected_type, rhs_type,
            ))
        } else {
            Ok(rhs_type)
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("AssignNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::IfNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let condition_type = self.condition.type_check(checker)?;
        if condition_type != Type::Bool {
            return Err(format!(
                "{}: {:?}: if-condition is expected to evaluate to boolean, found {}.",
                ERR_STR,
                self.location,
                condition_type
            ))
        }
        let if_type = self.if_branch.type_check(checker)?;
        debug_assert!(if_type == Type::None);
        if let Some(else_branch) = &mut self.else_branch {
            let else_type = else_branch.type_check(checker)?;
            debug_assert!(else_type == Type::None);
        }
        Ok(Type::None)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::ReturnNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(!checker.current_function.is_empty());
        debug_assert!(self.typ == Type::Unknown);
        let (expected_return_type, location) = if checker.current_class.is_empty() {
            // We're returning from a normal function
            let Some(function) = checker.known_functions.get(&checker.current_function) else { unreachable!() };
            let expected_return_type = function.return_type.clone();
            let location = function.location.clone();
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(class) = checker.known_classes.get(&checker.current_class) else { unreachable!() };
            let (mut location, mut expected_return_type) = (Location::anonymous(), Type::Unknown);
            if let Some(feature) = class.known_features.get(&checker.current_function) {
                expected_return_type = feature.return_type.clone();
                location = feature.location.clone();
            }
            if let Some(method) = class.known_methods.get(&checker.current_function) {
                expected_return_type = method.return_type.clone();
                location = method.location.clone();
            }
            debug_assert!(expected_return_type != Type::Unknown);
            debug_assert!(location != Location::anonymous());
            (expected_return_type, location)
        };
        if let Some(ret_expr) = &mut self.return_value {
            if expected_return_type == Type::None {
                // Found expression but expected none, makes no sense
                return Err(format!(
                    "{}: {:?}: Unexpected Return expression.\n{}: {:?}: Function is declared to return nothing here.",
                    ERR_STR,
                    self.location,
                    NOTE_STR,
                    location,
                ));
            }
            let expr_type = ret_expr.type_check(checker)?;
            let t = if expr_type == Type::Unknown {
                // we have something like `return 5;`, where we couldn't determine the type
                // so we now have to `infer` the type, and set it accordingly
                if let Err(e) = ret_expr.type_check_with_type(checker, &expected_return_type) {
                    return Err(format!(
                        "{}\n{}: {:?}: Function declared to return {} here.",
                        e,
                        NOTE_STR,
                        location,
                        expected_return_type
                    ));
                }
                // Successfully `inferred` type, we can now proceed as normal
                expected_return_type
            } else if expr_type != expected_return_type {
                // Signature expects `expected_return_type`, `return {expr}` has other type for expr
                return Err(format!(
                    "{}: {:?}: Type Mismatch! Function is declared to return `{}`, found `{}`.\n{}: {:?}: Function declared to return `{}` here.",
                    ERR_STR,
                    self.location,
                    expected_return_type,
                    expr_type,
                    NOTE_STR,
                    location,
                    expr_type
                ));
            } else {
                // Everything is fine, correct return type was provided
                expr_type
            };
            self.typ = t.clone();
            Ok(t)
        } else if expected_return_type != Type::None {
            // No return expression, but we expected return value
            Err(format!(
                "{}: {:?}: Expected Return value, found nothing.\n{}: {:?}: Function is declared to return {} here.",
                ERR_STR,
                self.location,
                NOTE_STR,
                location,
                expected_return_type
            ))
        } else {
            todo!()
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ReturnNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::TypeNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        match &self.typ {
            Type::Class(class_name) => {
                if !checker.known_classes.contains_key(class_name) {
                    Err(format!(
                        "{}: {:?}: Unknown Type `{}`.",
                        ERR_STR, self.location, self.typ
                    ))
                } else {
                    Ok(Type::None)
                }
            }
            _ => Ok(Type::None),
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("TypeNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::ArgumentNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        self.expression.type_check(checker)
    }
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        self.expression.type_check_with_type(checker, typ)
    }
}
impl Typecheckable for nodes::Expression {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        match self {
            Self::Name(name_node) => name_node.type_check(checker),
            Self::Binary(binary_expr) => binary_expr.type_check(checker),
            Self::Comparison(comp_expr) => comp_expr.type_check(checker),
            Self::Identifier(ident_expr) => ident_expr.type_check(checker),
            Self::FunctionCall(func_call) => func_call.type_check(checker),
            Self::ConstructorCall(cons_call) => cons_call.type_check(checker),
            Self::BuiltIn(built_in) => built_in.type_check(checker),
            Self::ArrayAccess(access) => access.type_check(checker),
            Self::ArrayLiteral(literal) => literal.type_check(checker),
            Self::FieldAccess(access) => access.type_check(checker),
            Self::Literal(literal) => literal.type_check(checker),
        }
    }
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        match self {
            Self::Name(name_node) => name_node.type_check_with_type(checker, typ),
            Self::Binary(binary_expr) => binary_expr.type_check_with_type(checker, typ),
            Self::Comparison(comp_expr) => comp_expr.type_check_with_type(checker, typ),
            Self::Identifier(ident_expr) => ident_expr.type_check_with_type(checker, typ),
            Self::FunctionCall(func_call) => func_call.type_check_with_type(checker, typ),
            Self::ConstructorCall(cons_call) => cons_call.type_check_with_type(checker, typ),
            Self::BuiltIn(built_in) => built_in.type_check_with_type(checker, typ),
            Self::ArrayAccess(access) => access.type_check_with_type(checker, typ),
            Self::ArrayLiteral(literal) => literal.type_check_with_type(checker, typ),
            Self::FieldAccess(access) => access.type_check_with_type(checker, typ),
            Self::Literal(literal) => literal.type_check_with_type(checker, typ),
        }
    }
}
impl Typecheckable for nodes::ExpressionComparisonNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(self.typ == Type::Bool);
        let lhs_type = self.lhs.type_check(checker)?;
        let rhs_type = self.rhs.type_check(checker)?;
        match (&lhs_type, &rhs_type) {
            // FIXME: Show which side is Class/Array/Booleans
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of classes.",
                    ERR_STR, self.location, self.operation
                ));
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of arrays.",
                    ERR_STR, self.location, self.operation
                ));
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of booleans.",
                    ERR_STR, self.location, self.operation
                ))
            }
            (Type::Unknown, Type::Unknown) => (),
            (Type::Unknown, other) => self.lhs.type_check_with_type(checker, other)?,
            (other, Type::Unknown) => self.rhs.type_check_with_type(checker, other)?,
            (lhs, rhs) => {
                let lhs_loc = self.lhs.get_loc();
                let rhs_loc = self.rhs.get_loc();
                if lhs != rhs {
                    return Err(format!(
                        "{}: {:?}: Type Mismatch in comparison. LHS has type `{}`, RHS has type `{}`.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                        ERR_STR,
                        self.location,
                        lhs,
                        rhs,
                        NOTE_STR,
                        lhs_loc,
                        NOTE_STR,
                        rhs_loc
                    ));
                }
            }
        }
        Ok(Type::Bool)
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ExpressionComparisonNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::ExpressionIdentifierNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let typ = self.expression.type_check(checker)?;
        self.typ = typ.clone();
        Ok(typ)
    }
    fn type_check_with_type(
        &mut self,
        checker: &mut TypeChecker,
        typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        self.expression.type_check_with_type(checker, typ)?;
        self.typ = typ.clone();
        Ok(())
    }
}
impl Typecheckable for nodes::ExpressionArrayLiteralNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let mut array_type = Type::Unknown;
        for elem in &mut self.elements {
            let elem_type = elem.type_check(checker)?;
            if elem_type == Type::Unknown {
                return Ok(Type::Unknown)
            } else if array_type == Type::Unknown {
                array_type = elem_type;
            } else if array_type != elem_type {
                return Err(format!(
                    "{}: {:?}: Type Mismatch in Array Literal. Expected Type {}, found {}.",
                    ERR_STR,
                    elem.get_loc(),
                    array_type,
                    elem_type
                ))
            }
        }
        if let Type::Arr(t, mut size) = array_type {
            size.push(self.elements.len());
            Ok(Type::Arr(t, size))
        } else if array_type != Type::Unknown {
            Ok(Type::Arr(Box::new(array_type), vec![self.elements.len()]))
        } else {
            Ok(array_type)
        }
    }
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        if let Type::Arr(expected_type, expected_size) = typ {
            let expected_count = expected_size[0];
            if expected_count != self.elements.len() {
                return Err(format!(
                    "{}: {:?}: Size Mismatch when evaluating Array Literal. Expected {} element(s), found {}.",
                    ERR_STR,
                    self.location,
                    expected_count,
                    self.elements.len()
                ));
            }
            let sub_expected: Vec<_> = expected_size[1..].to_vec();
            let sub_type = if sub_expected.is_empty() {
                *expected_type.clone()
            } else {
                Type::Arr(expected_type.clone(), sub_expected)
            };
            for element in &mut self.elements {
                if let Err(e) = element.type_check_with_type(checker, &sub_type) {
                    return Err(format!(
                        "{}: {:?}: Error when evaluating type of Array Literal:\n{}",
                        ERR_STR, self.location, e
                    ));
                }
            }
            // FIXME: Why does ExpressionArrayLiteralNode not have a field type??
            Ok(())
        } else {
            Err(format!(
                "{}: {:?}: Type Mismatch! Expected {}, found Array Literal.",
                ERR_STR,
                self.location,
                typ
            ))
        }
    }
}
impl Typecheckable for nodes::ExpressionArrayAccessNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let Some(var) = checker.get_variable(&self.array_name) else {
            return Err(format!(
                "{}: {:?}: Unknown variable `{}`.",
                ERR_STR,
                self.location,
                self.array_name
            ));
        };
        let Type::Arr(elem, arr_size) = &var.typ else {
            return Err(format!(
                "{}: {:?}: Attempted to index into non-array variable `{}`.\n{}: {:?}: Variable `{}` declared here.",
                ERR_STR,
                self.location,
                self.array_name,
                NOTE_STR,
                var.location,
                var.name,
            ));
        };
        if arr_size.len() != self.indices.elements.len() {
            let i = if arr_size.len() == 1 { "index" } else { "indices" };
            return Err(format!(
                "{}: {:?}: Dimension Mismatch in Array indexing. Expected {} {i}, found {}.\n{}: Getting a subarray is not supported yet, you can only get single elements.",
                ERR_STR,
                self.location,
                arr_size.len(),
                self.indices.elements.len(),
                NOTE_STR
            ))
        }
        let t = self.indices.type_check(checker)?;
        if let Type::Arr(elem_index, _) = t {
            if *elem_index != Type::Usize {
                Err(format!(
                    "{}: {:?}: Array Indices are expected to be type usize, found array of {}.",
                    ERR_STR,
                    self.location,
                    elem_index
                ))
            } else {
                todo!()
            }
        } else if t == Type::Unknown {
            let index_arr = Type::Arr(
                Box::new(Type::Usize),
                vec![arr_size.len()]
            );
            if let Err(e) = self.indices.type_check_with_type(checker, &index_arr) {
                Err(format!(
                    "{}\n{}: Array Indices are expected to be of type usize.",
                    e,
                    NOTE_STR,
                ))
            } else {
                let final_type = *elem.clone();
                self.typ = final_type.clone();
                return Ok(final_type)
            }
        } else {
            todo!()
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ExpressionArrayAccessNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::ExpressionLiteralNode {
    fn type_check(&mut self, _checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        Ok(self.typ.clone())
    }
    fn type_check_with_type(&mut self, _checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        if self.typ != Type::Unknown && self.typ != *typ {
            return Err(format!(
                "{}: {:?}: Type Mismatch! Expected {}, found {}.",
                ERR_STR, self.location, typ, self.typ
            ));
        } else if let Type::Arr(_, _) = typ {
            Err(format!(
                "{}: {:?} Type Mismatch! Expected Array Literal, found Integer Literal `{}`",
                ERR_STR,
                self.location,
                self.value
            ))
        } else if let Type::Class(class_name) = typ {
            Err(format!(
                "{}: {:?} Type Mismatch! Expected instance of class `{}`, found Integer Literal `{}`",
                ERR_STR,
                self.location,
                class_name,
                self.value
            ))
        } else {
            self.typ = typ.clone();
            Ok(())
        }
    }
}
impl Typecheckable for nodes::ExpressionBinaryNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let lhs_type = self.lhs.type_check(checker)?;
        let rhs_type = self.rhs.type_check(checker)?;
        debug_assert!(lhs_type != Type::None);
        debug_assert!(rhs_type != Type::None);
        // FIXME: Make this easier
        match (&lhs_type, &rhs_type) {
            // FIXME: Show which side is Class/Array/Bool
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of classes.",
                    ERR_STR, self.location, self.operation
                ))
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of arrays.",
                    ERR_STR, self.location, self.operation
                ))
            }
            (Type::Bool, _) | (_, Type::Bool) => Err(format!(
                "{}: {:?}: Binary Operation `{}` is not defined in the context of booleans.",
                ERR_STR, self.location, self.operation
            )),
            (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
            (Type::Unknown, other) => {
                self.lhs.type_check_with_type(checker, other)?;
                self.typ = other.clone();
                Ok(other.clone())
            }
            (other, Type::Unknown) => {
                self.rhs.type_check_with_type(checker, other)?;
                self.typ = other.clone();
                Ok(other.clone())
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    return Err(format!(
                        "{}: {:?}: Type Mismatch in binary expression.\n{}: {:?}: LHS has type {}.\n{}: {:?}: RHS has type {}.",
                        ERR_STR,
                        self.location,
                        NOTE_STR,
                        self.lhs.get_loc(),
                        lhs,
                        NOTE_STR,
                        self.rhs.get_loc(),
                        rhs
                    ));
                }
                self.typ = lhs.clone();
                Ok(lhs.clone())
            }
        }
    }
    fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
    where
        Self: Sized,
    {
        self.lhs.type_check_with_type(checker, typ)?;
        self.rhs.type_check_with_type(checker, typ)?;
        self.typ = typ.clone();
        Ok(())
    }
}
impl Typecheckable for nodes::ExpressionCallNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        let Some(function) = checker.known_functions.get(&self.function_name) else {
            return Err(format!(
                "{}: {:?}: Call to unknown function `{}`.",
                ERR_STR,
                self.location,
                self.function_name
            ));
        };
        let return_type = function.return_type.clone();
        match self.arguments.len().cmp(&function.parameters.len()) {
            std::cmp::Ordering::Less => {
                return Err(format!(
                    "{}: {:?}: Not enough arguments for call to function `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Function declared here.",
                    ERR_STR,
                    self.location,
                    self.function_name,
                    function.parameters.len(),
                    self.arguments.len(),
                    NOTE_STR,
                    function.location
                ));
            }
            std::cmp::Ordering::Greater => {
                return Err(format!(
                    "{}: {:?}: Too many arguments for call to function `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Function declared here.",
                    ERR_STR,
                    self.location,
                    self.function_name,
                    function.parameters.len(),
                    self.arguments.len(),
                    NOTE_STR,
                    function.location
                ));
            }
            std::cmp::Ordering::Equal => (),
        }
        let params = function.parameters.clone();
        for (arg, param) in self.arguments.iter_mut().zip(params) {
            let expected = param.typ;
            let arg_type = arg.type_check(checker)?;
            debug_assert!(arg_type != Type::None);
            if arg_type == Type::Unknown {
                // We need to `infer` the type again
                arg.type_check_with_type(checker, &expected)?;
            } else if arg_type != expected {
                return Err(format!(
                    "{}: {:?}: Type Mismatch in argument evaluation. Expected type `{}`, found type `{}`.\n{}: {:?}: Parameter declared here.",
                    ERR_STR,
                    arg.location,
                    expected,
                    arg_type,
                    NOTE_STR,
                    param.location
                ));
            } else {
                // Everything is cool
            }
        }
        self.typ = return_type;
        Ok(self.typ.clone())
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ExpressionCallNode::type_check_with_type() is not implemented yet")
    }
}
impl Typecheckable for nodes::ExpressionConstructorNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(!checker.current_function.is_empty());
        if !checker.known_classes.contains_key(&self.class_name) {
            return Err(format!(
                "{}: {:?}: Call to constructor of unknown class `{}`.\n{}: Capitalized function calls are always assumed to be constructor calls.",
                ERR_STR,
                self.location,
                self.class_name,
                NOTE_STR
            ));
        }
        let Some(class) = checker.known_classes.get(&self.class_name) else { unreachable!() };
        if !class.has_constructor {
            return Err(format!(
                "{}: {:?}: Class `{}` has no constructor.\n{}: {:?}: Could not find feature `{CONSTRUCTOR_NAME}` in class `{}`.",
                ERR_STR,
                self.location,
                self.class_name,
                NOTE_STR,
                class.location,
                self.class_name
            ));
        }
        let class_type = class.class_type.clone();
        let Some(constructor) = class.get_feature(CONSTRUCTOR_NAME) else { unreachable!() };

        let params = constructor.parameters.clone();
        match self.arguments.len().cmp(&params.len()) {
            std::cmp::Ordering::Less => {
                return Err(format!(
                    "{}: {:?}: Not enough arguments for call to constructor `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Constructor declared here.",
                    ERR_STR,
                    self.location,
                    self.class_name,
                    params.len(),
                    self.arguments.len(),
                    NOTE_STR,
                    constructor.location
                ));
            }
            std::cmp::Ordering::Greater => {
                return Err(format!(
                    "{}: {:?}: Too many arguments for call to constructor `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Constructor declared here.",
                    ERR_STR,
                    self.location,
                    self.class_name,
                    params.len(),
                    self.arguments.len(),
                    NOTE_STR,
                    constructor.location
                ));
            }
            std::cmp::Ordering::Equal => (),
        }
        for (arg, param) in self.arguments.iter_mut().zip(params) {
            let expected = param.typ;
            let arg_type = arg.type_check(checker)?;
            debug_assert!(arg_type != Type::None);
            if arg_type == Type::Unknown {
                // We need to `infer` the type again
                arg.type_check_with_type(checker, &expected)?;
            } else if arg_type != expected {
                return Err(format!(
                    "{}: {:?}: Type Mismatch in argument evaluation. Expected type `{}`, found type `{}`.\n{}: {:?}: Parameter declared here.",
                    ERR_STR,
                    arg.location,
                    expected,
                    arg_type,
                    NOTE_STR,
                    param.location
                ));
            } else {
                // Everything is cool
            }
        }
        self.typ = class_type;
        Ok(Type::Class(self.class_name.clone()))
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
impl Typecheckable for nodes::ExpressionFieldAccessNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        debug_assert!(!checker.current_function.is_empty());
        match checker.get_variable(&self.name) {
            Some(var) => {
                if !var.is_class_instance() {
                    return Err(format!(
                        "{}: {:?}: Variable `{}` is not a class instance, it has no fields.",
                        ERR_STR, self.location, self.name
                    ));
                }
                let typ = self.type_check_field(checker, var.clone())?;
                self.typ = var.typ;
                Ok(typ)
            }
            None => Err(format!(
                "{}: {:?}: Undeclared variable `{}`.",
                ERR_STR, self.location, self.name
            )),
        }
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        internal_error!("ExpressionFieldAccessNode::type_check_with_type() is not implemented yet")
    }
}
impl nodes::ExpressionFieldAccessNode {
    fn type_check_field(
        &mut self,
        checker: &mut TypeChecker,
        var: Variable,
    ) -> Result<Type, String> {
        match &mut (*self.field.expression) {
            nodes::Expression::FieldAccess(field_access) => {
                /*
                get type of current variable
                get fields of current variable, if its class (if not, error)
                check if field_access.name is field (if not, error)
                create temporary variable with name field_access.name and type field.typ
                recursively call type_check_field
                 */
                let typ = var.typ;
                match typ {
                    Type::Class(class_name) => {
                        let Some(class) = checker.known_classes.get(&class_name) else {
                            // Lookup of classes and their fields is done before ever evaluating any field access
                            // So at this point, field access only consists of known classes
                            unreachable!()
                        };
                        let Some(field) = class.get_field(&field_access.name) else {
                            return Err(format!(
                                "{}: {:?}: Class `{}` has no field `{}`.\n{}: {:?}: Class declared here.",
                                ERR_STR,
                                field_access.location,
                                class.name,
                                field_access.name,
                                NOTE_STR,
                                class.location
                            ));
                        };
                        let var = Variable {
                            name: field_access.name.clone(),
                            location: field.0,
                            typ: field.1,
                        };
                        let typ = field_access.type_check_field(checker, var.clone())?;
                        // FIXME: var.typ does not sound logical here, but it works for now
                        field_access.typ = var.typ.clone();
                        self.field.typ = var.typ.clone();
                        Ok(typ)
                    }
                    _ => Err(format!(
                        "{}: {:?}: Can't access field `{}` of non-class identifier `{}`.\n{}: {:?}: Field `{}` declared here.",
                        ERR_STR,
                        self.location,
                        field_access.name,
                        var.name,
                        NOTE_STR,
                        var.location,
                        var.name
                    ))
                }
            }
            nodes::Expression::Name(name_node) => {
                if !var.is_class_instance() {
                    return Err(format!(
                        "{}: {:?}: Can't access field `{}` of non-class identifier `{}`.\n{}: {:?}: Field `{}` declared here.",
                        ERR_STR,
                        self.location,
                        name_node.name,
                        var.name,
                        NOTE_STR,
                        var.location,
                        var.name
                    ));
                }
                let class_name = var.get_class_name();
                if let Some(class) = checker.known_classes.get(&class_name) {
                    match class.get_field(&name_node.name) {
                        Some(field) => {
                            name_node.typ = field.1.clone();
                            self.field.typ = field.1.clone();
                            Ok(field.1)
                        },
                        None => Err(format!(
                            "{}: {:?}: Identifier `{}` has no field `{}`.\n{}: {:?}: Class declared here.",
                            ERR_STR,
                            self.location,
                            self.name,
                            name_node.name,
                            NOTE_STR,
                            class.location
                        ))
                    }
                } else {
                    // Lookup of classes and their fields is done before ever evaluating any field access
                    // So at this point, field access only consists of known classes
                    unreachable!()
                }
            }
            nodes::Expression::FunctionCall(function_node) => {
                let class_name = var.get_class_name();
                let Some(class) = checker.known_classes.get(&class_name) else { todo!() };
                let Some(method) = class.get_method(&function_node.function_name) else {
                    return Err(format!(
                        "{}: {:?}: Class `{}` has no method `{}`.\n{}: {:?}: Class declared here.",
                        ERR_STR,
                        function_node.location,
                        class_name,
                        function_node.function_name,
                        NOTE_STR,
                        class.location
                    ));
                };
                let return_type = method.return_type.clone();
                match function_node.arguments.len().cmp(&method.parameters.len()) {
                    std::cmp::Ordering::Less => {
                        return Err(format!(
                            "{}: {:?}: Not enough arguments for call to method `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Method declared here.",
                            ERR_STR,
                            self.location,
                            function_node.function_name,
                            method.parameters.len(),
                            function_node.arguments.len(),
                            NOTE_STR,
                            method.location
                        ));
                    }
                    std::cmp::Ordering::Greater => {
                        return Err(format!(
                            "{}: {:?}: Too many arguments for call to method `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Method declared here.",
                            ERR_STR,
                            self.location,
                            function_node.function_name,
                            method.parameters.len(),
                            function_node.arguments.len(),
                            NOTE_STR,
                            method.location
                        ));
                    }
                    std::cmp::Ordering::Equal => (),
                }
                let params = method.parameters.clone();
                for (arg, param) in function_node.arguments.iter_mut().zip(params) {
                    let expected = param.typ;
                    let arg_type = arg.type_check(checker)?;
                    debug_assert!(arg_type != Type::None);
                    if arg_type == Type::Unknown {
                        // We need to `infer` the type again
                        arg.type_check_with_type(checker, &expected)?;
                    } else if arg_type != expected {
                        return Err(format!(
                            "{}: {:?}: Type Mismatch in argument evaluation. Expected type `{}`, found type `{}`.\n{}: {:?}: Parameter declared here.",
                            ERR_STR,
                            arg.location,
                            expected,
                            arg_type,
                            NOTE_STR,
                            param.location
                        ));
                    } else {
                        // Everything is cool
                    }
                }
                function_node.typ = return_type.clone();
                self.field.typ = return_type.clone();
                self.typ = return_type.clone();
                Ok(return_type)
            }
            _ => unreachable!(),
        }
    }
}
impl Typecheckable for nodes::NameNode {
    fn type_check(&mut self, checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        match checker.get_variable(&self.name) {
            Some(var) => {
                self.typ = var.typ.clone();
                Ok(var.typ)
            }
            None => Err(format!(
                "{}: {:?}: Undeclared variable `{}`.",
                ERR_STR, self.location, self.name
            )),
        }
    }
    fn type_check_with_type(
        &mut self,
        checker: &mut TypeChecker,
        typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        if self.typ == Type::Unknown {
            self.type_check(checker)?;
        }
        if self.typ != *typ {
            return Err(format!(
                "{}: {:?}: Type Mismatch! Expected Type {}, found {}.",
                ERR_STR,
                self.location,
                typ,
                self.typ
            ))
        }
        Ok(())
    }
}
impl Typecheckable for nodes::ExpressionBuiltInNode {
    fn type_check(&mut self, _checker: &mut TypeChecker) -> Result<Type, String>
    where
        Self: Sized,
    {
        todo!()
    }
    fn type_check_with_type(
        &mut self,
        _checker: &mut TypeChecker,
        _typ: &Type,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        todo!()
    }
}
