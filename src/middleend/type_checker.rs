use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::flags::Flags;
use crate::frontend::nodes;
use crate::frontend::parser::Location;

use crate::compiler::CONSTRUCTOR_NAME;
use crate::compiler::{ERR_STR, NOTE_STR};
use crate::{internal_error, internal_warning};

use tracer::trace_call;

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
    #[trace_call(extra)]
    pub fn size(&self) -> Result<usize, String> {
        match self {
            Type::Arr(t, size) => Ok(t.size()? * size.iter().product::<usize>()),
            Type::I32 | Type::U32 | Type::F32 | Type::Bool => Ok(4),
            Type::I64 | Type::U64 | Type::F64 | Type::Usize => Ok(8),
            Type::Class(..) => Ok(8),
            Type::None => internal_error!("Something attempted to get the size of Type::None!"),
            Type::Unknown => {
                internal_error!("Something attempted to get the size of Type::Unknown!")
            }
        }
    }
}

impl Display for Type {
    #[trace_call(extra)]
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

#[derive(Debug, Clone)]
struct TypeLoc {
    t: Type,
    l: Location,
}

impl TypeLoc {
    #[trace_call(extra)]
    fn new(l: Location, t: Type) -> Self {
        Self { l, t }
    }
}

#[derive(Debug)]
pub struct Method {
    location: Location,
    return_type: TypeLoc,
    parameters: Vec<Parameter>,
}

impl Method {}

#[derive(Debug)]
pub struct Function {
    location: Location,
    return_type: TypeLoc,
    parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Class {
    name: String,
    class_type: Type,
    location: Location,
    fields: HashMap<String, TypeLoc>,
    known_methods: HashMap<String, Method>,
    // NOTE: Maybe it's better to add a custom Feature struct later on, once more features are implemented
    known_features: HashMap<String, Function>,
    has_constructor: bool,
}

impl Class {
    #[trace_call(extra)]
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

    #[trace_call(extra)]
    fn add_field(&mut self, field: &nodes::FieldNode) -> Result<(), String> {
        let name = &field.name;
        let location = &field.location;

        match self.fields.get(name) {
            Some(f) => Err(format!(
                "{}: {:?}: Field redeclaration.\n{}: {:?}: Field already declared here.",
                ERR_STR, location, NOTE_STR, f.l
            )),
            None => {
                let typ = &field.type_def.typ;
                self.fields.insert(
                    name.to_string(),
                    TypeLoc::new(location.clone(), typ.clone()),
                );
                Ok(())
            }
        }
    }

    #[trace_call(extra)]
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
                let return_loc = &method.return_type.location;
                method.parameters.insert(
                    0,
                    nodes::ParameterNode::this(
                        method.location.clone(),
                        Type::Class(method.class_name.clone()),
                    ),
                );
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
                    return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                    parameters,
                };
                self.known_methods.insert(name.clone(), method);
                Ok(())
            }
        }
    }

    #[trace_call(extra)]
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
                let return_loc = &feature.return_type.location;
                if name != CONSTRUCTOR_NAME {
                    feature.parameters.insert(
                        0,
                        nodes::ParameterNode::this(
                            feature.location.clone(),
                            Type::Class(feature.class_name.clone()),
                        ),
                    );
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
                    return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                    parameters,
                };
                self.known_features.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    #[trace_call(extra)]
    fn resolve_new_return_type(&mut self) -> Result<(), String> {
        for feat in &mut self.known_features {
            if *feat.0 == CONSTRUCTOR_NAME {
                if feat.1.return_type.t == self.class_type {
                    return Err(format!(
                        "{}: {:?}: Use of implicit return type for constructor.\n{}: {:?}: Constructor declared to return {} here.",
                        ERR_STR,
                        feat.1.location,
                        NOTE_STR,
                        feat.1.return_type.l,
                        feat.1.return_type.t
                    ));
                } else if feat.1.return_type.t != Type::None {
                    return Err(format!(
                        "{}: {:?}: Feature `{CONSTRUCTOR_NAME}` is expected to return None, found {}.",
                        ERR_STR, feat.1.location, feat.1.return_type.t
                    ));
                } else {
                    feat.1.return_type.t = self.class_type.clone();
                    return Ok(());
                }
            }
        }
        panic!("Class has constructor but no feature `{CONSTRUCTOR_NAME}` found");
    }

    #[trace_call(extra)]
    fn get_field(&self, name: &str) -> Option<TypeLoc> {
        self.fields.get(name).cloned()
    }

    #[trace_call(extra)]
    fn get_feature(&self, name: &str) -> Option<&Function> {
        self.known_features.get(name)
    }

    #[trace_call(extra)]
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
    #[trace_call(extra)]
    fn new(name: String, location: Location, typ: Type) -> Self {
        Self {
            name,
            location,
            typ,
        }
    }
    #[trace_call(extra)]
    fn is_class_instance(&self) -> bool {
        matches!(&self.typ, Type::Class(..))
    }
    #[trace_call(extra)]
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
    #[allow(unused)]
    flags: Flags,
}

impl TypeChecker {
    #[trace_call(extra)]
    pub fn new(flags: Flags) -> Self {
        Self {
            known_classes: HashMap::new(),
            known_functions: HashMap::new(),
            known_variables: VecDeque::new(),
            current_function: String::new(),
            current_class: String::new(),
            current_stack_size: 0,
            flags,
        }
    }

    #[trace_call(extra)]
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
                let return_loc = &function.return_type.location;
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
                    return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                    parameters,
                };
                self.known_functions.insert(name.clone(), func);
                Ok(())
            }
        }
    }

    #[trace_call(always)]
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

    #[trace_call(extra)]
    fn add_scope(&mut self) {
        self.known_variables.push_back(HashMap::new());
    }

    #[trace_call(extra)]
    fn remove_scope(&mut self) {
        self.known_variables.pop_back();
    }

    fn get_current_scope(&mut self) -> &mut HashMap<String, Variable> {
        let len = self.known_variables.len();
        &mut self.known_variables[len - 1]
    }

    #[trace_call(extra)]
    fn get_variable_in_current_scope(&self, name: &str) -> Option<Variable> {
        self.known_variables[self.known_variables.len() - 1]
            .get(name)
            .cloned()
    }

    #[trace_call(extra)]
    fn get_variable(&self, name: &String) -> Option<Variable> {
        for scope in self.known_variables.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    #[trace_call(always)]
    pub fn type_check_file(&mut self, file: &mut nodes::FileNode) -> Result<(), String> {
        self.fill_lookup(file)?;
        for c in &mut file.classes {
            self.type_check_class(c)?;
        }
        for f in &mut file.functions {
            self.type_check_function(f)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_class(&mut self, class: &mut nodes::ClassNode) -> Result<(), String> {
        self.current_class = class.name.clone();
        for field in &mut class.fields {
            self.type_check_field(field)?;
        }
        for method in &mut class.methods {
            self.type_check_method(method, &class.name)?;
        }
        for feature in &mut class.features {
            self.type_check_feature(feature, &class.name)?;
        }
        self.current_class.clear();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_field(&mut self, field: &mut nodes::FieldNode) -> Result<(), String> {
        self.type_check_type_node(&mut field.type_def)
    }

    // FIXME: Repeating code of features, methods and functions can be extracted into macros
    #[trace_call(always)]
    fn type_check_feature(
        &mut self,
        feature: &mut nodes::FeatureNode,
        class_name: &str,
    ) -> Result<(), String> {
        debug_assert!(self.current_function.is_empty());
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_classes.contains_key(class_name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut feature.parameters {
            self.type_check_parameter(param)?;
        }

        self.current_function = feature.name.clone();
        let Some(class_info) = self.known_classes.get(class_name) else { unreachable!() };
        let Some(feature_info) = class_info.known_features.get(&feature.name) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = HashMap::new();
        if feature.is_constructor {
            parameters.insert(
                String::from("this"),
                Variable::new(
                    String::from("this"),
                    feature.location.clone(),
                    Type::Class(class_name.to_string()),
                ),
            );
            self.current_stack_size += 8;
        }
        for param in &feature_info.parameters {
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
                feature.location,
                NOTE_STR
            ));
        }
        self.known_variables.push_back(parameters);
        self.type_check_block(&mut feature.block)?;
        feature.stack_size = self.current_stack_size;

        self.current_function.clear();
        self.current_stack_size = 0;
        self.known_variables.clear();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_method(
        &mut self,
        method: &mut nodes::MethodNode,
        class_name: &str,
    ) -> Result<(), String> {
        debug_assert!(self.current_function.is_empty());
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_classes.contains_key(class_name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut method.parameters {
            self.type_check_parameter(param)?;
        }

        self.current_function = method.name.clone();
        let Some(class_info) = self.known_classes.get(class_name) else {
            // Lookup of classes and methods is done before ever evaluating any methods,
            // so known_classes should always contain the class
            unreachable!()
        };
        let Some(method_info) = class_info.known_methods.get(&method.name) else {
            // Lookup of classes and methods is done before ever evaluating any methods,
            // so known_methods should always contain the method
            unreachable!()
        };

        // Parameters are now known variables
        let mut parameters = HashMap::new();
        for param in &method_info.parameters {
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
                    ));
                }
            }
        }
        if parameters.len() > 4 {
            return Err(format!(
                "{}: {:?}: Methods can have at most 4 parameters.\n{}: The implicit parameter `this` counts towards that limit.",
                ERR_STR,
                method.location,
                NOTE_STR
            ));
        }
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut method.block)?;
        method.stack_size = self.current_stack_size;

        self.current_stack_size = 0;
        self.current_function.clear();
        self.known_variables.clear();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_function(&mut self, function: &mut nodes::FunctionNode) -> Result<(), String> {
        debug_assert!(self.current_function.is_empty());
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_functions.contains_key(&function.name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut function.parameters {
            self.type_check_parameter(param)?;
        }

        self.current_function = function.name.clone();
        let Some(function_info) = self.known_functions.get(&function.name) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = HashMap::new();
        for param in &function_info.parameters {
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
                ERR_STR, function.location
            ));
        }
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut function.block)?;
        function.stack_size = self.current_stack_size;

        self.current_function.clear();
        self.current_stack_size = 0;
        self.known_variables.clear();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_parameter(&mut self, parameter: &mut nodes::ParameterNode) -> Result<(), String> {
        self.type_check_type_node(&mut parameter.typ)?;
        let var_size = parameter.typ.typ.size()?;
        self.current_stack_size += var_size;
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_block(&mut self, block: &mut nodes::BlockNode) -> Result<(), String> {
        self.add_scope();
        for statement in &mut block.statements {
            self.type_check_statement(statement)?;
        }
        self.remove_scope();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_statement(&mut self, statement: &mut nodes::Statement) -> Result<(), String> {
        match statement {
            nodes::Statement::Let(let_node) => self.type_check_stmt_let(let_node),
            nodes::Statement::Assign(assign_node) => self.type_check_stmt_assign(assign_node),
            nodes::Statement::If(if_node) => self.type_check_stmt_if(if_node),
            nodes::Statement::Return(return_node) => self.type_check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.type_check_stmt_while(while_node),
            nodes::Statement::Break(break_node) => self.type_check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => {
                self.type_check_stmt_continue(continue_node)
            }
            nodes::Statement::Expression(expression_node) => {
                self.type_check_stmt_expression(expression_node)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_let(&mut self, let_node: &mut nodes::LetNode) -> Result<(), String> {
        match self.get_variable_in_current_scope(&let_node.name) {
            Some(var) => Err(format!(
                "{}: {:?}: Variable redeclaration.\n{}: {:?}: Variable `{}` already declared here.",
                ERR_STR, let_node.location, NOTE_STR, var.location, var.name
            )),
            None => {
                self.type_check_type_node(&mut let_node.typ)?;
                let var_size = let_node.typ.typ.size()?;
                self.current_stack_size += var_size;

                let current_scope = self.get_current_scope();
                let var = Variable {
                    name: let_node.name.clone(),
                    location: let_node.location.clone(),
                    typ: let_node.typ.typ.clone(),
                };
                debug_assert!(current_scope.insert(let_node.name.clone(), var).is_none());
                let expected_type = &let_node.typ.typ;

                let expr_type = self.type_check_expression_node(&mut let_node.expression)?;
                debug_assert!(expr_type != Type::None);

                if expr_type == Type::Unknown {
                    // Couldnt determine type of expression
                    // We need to `infer` it
                    self.type_check_expression_with_type(
                        &mut let_node.expression.expression,
                        expected_type,
                    )?;
                    Ok(())
                } else {
                    if expr_type != *expected_type {
                        Err(format!(
                            "{}: {:?}: Type Mismatch! Expected type `{}`, found type `{}`.",
                            ERR_STR, let_node.location, expected_type, expr_type
                        ))
                    } else {
                        Ok(())
                    }
                }
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_assign(
        &mut self,
        assign_node: &mut nodes::AssignNode,
    ) -> Result<(), String> {
        let expected_type = self.type_check_expr_identifier(&mut assign_node.name)?;
        let expr_type = self.type_check_expression_node(&mut assign_node.expression)?;
        if expr_type == Type::Unknown {
            // We need to try and force the type of LHS to RHS
            self.type_check_expression_with_type(
                &mut assign_node.expression.expression,
                &expected_type,
            )?;
            Ok(())
        } else if expr_type != expected_type {
            Err(format!(
                "{}: {:?}: Type Mismatch! Expected type `{}`, found type `{}`.",
                ERR_STR, assign_node.expression.location, expected_type, expr_type
            ))
        } else {
            Ok(())
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_if(&mut self, if_node: &mut nodes::IfNode) -> Result<(), String> {
        let cond = self.type_check_expr_comparison(&mut if_node.condition)?;
        if cond != Type::Bool {
            return Err(format!(
                "{}: {:?}: if-condition is expected to evaluate to boolean, found {}.",
                ERR_STR, if_node.condition.location, cond
            ));
        }
        self.type_check_block(&mut if_node.if_branch)?;
        if let Some(else_branch) = &mut if_node.else_branch {
            self.type_check_block(else_branch)?;
        }
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_stmt_return(
        &mut self,
        return_node: &mut nodes::ReturnNode,
    ) -> Result<(), String> {
        debug_assert!(!self.current_function.is_empty());
        debug_assert!(return_node.typ == Type::Unknown);

        let (expected_return_type, location) = if self.current_class.is_empty() {
            // We're returning from a normal function
            let Some(function) = self.known_functions.get(&self.current_function) else { unreachable!() };
            let expected_return_type = function.return_type.t.clone();
            let location = function.location.clone();
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(class) = self.known_classes.get(&self.current_class) else { unreachable!() };
            let (mut location, mut expected_return_type) = (Location::anonymous(), Type::Unknown);
            if let Some(feature) = class.known_features.get(&self.current_function) {
                expected_return_type = feature.return_type.t.clone();
                location = feature.location.clone();
            } else if let Some(method) = class.known_methods.get(&self.current_function) {
                expected_return_type = method.return_type.t.clone();
                location = method.location.clone();
            }
            debug_assert!(expected_return_type != Type::Unknown);
            debug_assert!(location != Location::anonymous());
            (expected_return_type, location)
        };

        if let Some(ret_expr) = &mut return_node.return_value {
            if expected_return_type == Type::None {
                // Found expression but expected none, makes no sense
                return Err(format!(
                    "{}: {:?}: Unexpected Return expression.\n{}: {:?}: Function is declared to return nothing here.",
                    ERR_STR,
                    return_node.location,
                    NOTE_STR,
                    location,
                ));
            }
            let expr_type = self.type_check_expression_node(ret_expr)?;
            let t = if expr_type == Type::Unknown {
                // we have something like `return 5;`, where we couldn't determine the type
                // so we now have to `infer` the type, and set it accordingly
                if let Err(e) = self.type_check_expression_with_type(
                    &mut ret_expr.expression,
                    &expected_return_type,
                ) {
                    return Err(format!(
                        "{}\n{}: {:?}: Function declared to return {} here.",
                        e, NOTE_STR, location, expected_return_type
                    ));
                }
                // Successfully `inferred` type, we can now proceed as normal
                expected_return_type
            } else if expr_type != expected_return_type {
                // Signature expects `expected_return_type`, `return {expr}` has other type for expr
                return Err(format!(
                    "{}: {:?}: Type Mismatch! Function is declared to return `{}`, found `{}`.\n{}: {:?}: Function declared to return `{}` here.",
                    ERR_STR,
                    return_node.location,
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
            return_node.typ = t.clone();
            Ok(())
        } else if expected_return_type != Type::None {
            // No return expression, but we expected return value
            Err(format!(
                "{}: {:?}: Expected Return value, found nothing.\n{}: {:?}: Function is declared to return {} here.",
                ERR_STR,
                return_node.location,
                NOTE_STR,
                location,
                expected_return_type
            ))
        } else {
            // No return expression
            // No return type expected
            return_node.typ = Type::None;
            Ok(())
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_while(&mut self, while_node: &mut nodes::WhileNode) -> Result<(), String> {
        let cond = self.type_check_expr_comparison(&mut while_node.condition)?;
        if cond != Type::Bool {
            return Err(format!(
                "{}: {:?}: while-condition is expected to evaluate to boolean, found {}.",
                ERR_STR, while_node.condition.location, cond
            ));
        }
        self.type_check_block(&mut while_node.block)?;
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_stmt_break(&mut self, _break_node: &mut nodes::BreakNode) -> Result<(), String> {
        internal_warning!("TODO: Check if we're in a loop for break");
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_stmt_continue(
        &mut self,
        _continue_node: &mut nodes::ContinueNode,
    ) -> Result<(), String> {
        internal_warning!("TODO: Check if we're in a loop for continue");
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_stmt_expression(
        &mut self,
        expression_node: &mut nodes::ExpressionNode,
    ) -> Result<(), String> {
        self.type_check_expression(&mut expression_node.expression)?;
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_expression_node(
        &mut self,
        expression_node: &mut nodes::ExpressionNode,
    ) -> Result<Type, String> {
        self.type_check_expression(&mut expression_node.expression)
    }

    #[trace_call(always)]
    fn type_check_expression(
        &mut self,
        expression: &mut nodes::Expression,
    ) -> Result<Type, String> {
        match expression {
            nodes::Expression::Name(name_node) => self.type_check_expr_name(name_node),
            nodes::Expression::Binary(binary_expr) => self.type_check_expr_binary(binary_expr),
            nodes::Expression::Comparison(comp_expr) => self.type_check_expr_comparison(comp_expr),
            nodes::Expression::Identifier(ident_expr) => {
                self.type_check_expr_identifier(ident_expr)
            }
            nodes::Expression::FunctionCall(func_call) => {
                self.type_check_expr_function_call(func_call)
            }
            nodes::Expression::BuiltIn(built_in) => self.type_check_expr_builtin(built_in),
            nodes::Expression::ArrayAccess(access) => self.type_check_expr_array_access(access),
            nodes::Expression::ArrayLiteral(literal) => self.type_check_expr_array_literal(literal),
            nodes::Expression::FieldAccess(access) => self.type_check_expr_field_access(access),
            nodes::Expression::Literal(literal) => self.type_check_expr_literal(literal),
        }
    }

    #[trace_call(always)]
    fn type_check_expression_with_type(
        &mut self,
        expression: &mut nodes::Expression,
        typ: &Type,
    ) -> Result<Type, String> {
        match expression {
            nodes::Expression::Binary(binary_node) => {
                self.type_check_expression_with_type(&mut binary_node.lhs, typ)?;
                self.type_check_expression_with_type(&mut binary_node.rhs, typ)?;
                binary_node.typ = typ.clone();
                Ok(typ.clone())
            }
            nodes::Expression::Literal(lit_node) => {
                if lit_node.typ != Type::Unknown && lit_node.typ != *typ {
                    Err(format!(
                        "{}: {:?}: Type Mismatch! Expected {}, found {}.",
                        ERR_STR, lit_node.location, typ, lit_node.typ
                    ))
                } else if let Type::Arr(_, _) = typ {
                    Err(format!(
                        "{}: {:?} Type Mismatch! Expected Array Literal, found Integer Literal `{}`",
                        ERR_STR, lit_node.location, lit_node.value
                    ))
                } else if let Type::Class(class_name) = typ {
                    Err(format!(
                        "{}: {:?} Type Mismatch! Expected instance of class `{}`, found Integer Literal `{}`",
                        ERR_STR,
                        lit_node.location,
                        class_name,
                        lit_node.value
                    ))
                } else {
                    lit_node.typ = typ.clone();
                    Ok(typ.clone())
                }
            }
            e => internal_error!(format!(
                "type_check_expression_with_type for {:?} is not implemented yet!",
                e
            )),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_name(&mut self, name_node: &mut nodes::NameNode) -> Result<Type, String> {
        let var = self.get_variable(&name_node.name);
        match var {
            Some(var) => {
                name_node.typ = var.typ.clone();
                Ok(var.typ)
            }
            None => Err(format!(
                "{}: {:?}: Undeclared variable `{}`.",
                ERR_STR, name_node.location, name_node.name
            )),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Result<Type, String> {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs)?;
        debug_assert!(lhs_type != Type::None);
        debug_assert!(rhs_type != Type::None);
        match (&lhs_type, &rhs_type) {
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of classes.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, binary_expr.location, binary_expr.operation,
                    NOTE_STR, binary_expr.lhs.get_loc(),
                    NOTE_STR, binary_expr.rhs.get_loc(),
                ));
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of arrays.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, binary_expr.location, binary_expr.operation,
                    NOTE_STR, binary_expr.lhs.get_loc(),
                    NOTE_STR, binary_expr.rhs.get_loc(),
                ));
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                return Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of booleans.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, binary_expr.location, binary_expr.operation,
                    NOTE_STR, binary_expr.lhs.get_loc(),
                    NOTE_STR, binary_expr.rhs.get_loc(),
                ))
            }
            (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut binary_expr.lhs, other)?;
                binary_expr.typ = other.clone();
                Ok(other.clone())
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other)?;
                binary_expr.typ = other.clone();
                Ok(other.clone())
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    return Err(format!(
                        "{}: {:?}: Type Mismatch in binary expression.\n{}: {:?}: LHS has type {}.\n{}: {:?}: RHS has type {}.",
                        ERR_STR,
                        binary_expr.location,
                        NOTE_STR,
                        binary_expr.lhs.get_loc(),
                        lhs,
                        NOTE_STR,
                        binary_expr.rhs.get_loc(),
                        rhs
                    ));
                }
                binary_expr.typ = lhs.clone();
                Ok(lhs.clone())
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_comparison(
        &mut self,
        comp_expr: &mut nodes::ComparisonNode,
    ) -> Result<Type, String> {
        let lhs_type = self.type_check_expression(&mut comp_expr.lhs)?;
        let rhs_type = self.type_check_expression(&mut comp_expr.rhs)?;
        debug_assert!(lhs_type != Type::None);
        debug_assert!(rhs_type != Type::None);
        match (&lhs_type, &rhs_type) {
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of classes.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, comp_expr.location, comp_expr.operation,
                    NOTE_STR, comp_expr.lhs.get_loc(),
                    NOTE_STR, comp_expr.rhs.get_loc(),
                ))
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of arrays.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, comp_expr.location, comp_expr.operation,
                    NOTE_STR, comp_expr.lhs.get_loc(),
                    NOTE_STR, comp_expr.rhs.get_loc(),
                ))
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                Err(format!(
                    "{}: {:?}: Binary Operation `{}` is not defined in the context of booleans.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                    ERR_STR, comp_expr.location, comp_expr.operation,
                    NOTE_STR, comp_expr.lhs.get_loc(),
                    NOTE_STR, comp_expr.rhs.get_loc(),
                ))
            }
            (Type::Unknown, Type::Unknown) => {
                // We can't determine the type of either side, let's try to force it
                // FIXME: This is a bit hacky, but it works for now
                self.type_check_expression_with_type(&mut comp_expr.lhs, &Type::I64)?;
                self.type_check_expression_with_type(&mut comp_expr.rhs, &Type::I64)?;
                comp_expr.typ = Type::Bool;
                Ok(Type::Bool)
            },
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut comp_expr.lhs, other)?;
                comp_expr.typ = Type::Bool;
                Ok(Type::Bool)
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut comp_expr.rhs, other)?;
                comp_expr.typ = Type::Bool;
                Ok(Type::Bool)
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    return Err(format!(
                        "{}: {:?}: Type Mismatch in comparison. LHS has type `{}`, RHS has type `{}`.\n{}: {:?}: LHS is here.\n{}: {:?}: RHS is here.",
                        ERR_STR,
                        comp_expr.location,
                        lhs,
                        rhs,
                        NOTE_STR,
                        comp_expr.lhs.get_loc(),
                        NOTE_STR,
                        comp_expr.rhs.get_loc()
                    ));
                }
                comp_expr.typ = Type::Bool;
                Ok(Type::Bool)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_identifier(
        &mut self,
        ident_expr: &mut nodes::IdentifierNode,
    ) -> Result<Type, String> {
        let typ = self.type_check_expression(&mut ident_expr.expression)?;
        ident_expr.typ = typ.clone();
        Ok(typ)
    }

    #[trace_call(always)]
    fn type_check_expr_array_literal(
        &mut self,
        literal: &mut nodes::ArrayLiteralNode,
    ) -> Result<Type, String> {
        let mut array_type = Type::Unknown;
        for elem in &mut literal.elements {
            let elem_type = self.type_check_expression(elem)?;
            if elem_type == Type::Unknown {
                return Ok(Type::Unknown);
            } else if array_type == Type::Unknown {
                array_type = elem_type;
            } else if array_type != elem_type {
                return Err(format!(
                    "{}: {:?}: Type Mismatch in Array Literal. Expected Type {}, found {}.",
                    ERR_STR,
                    elem.get_loc(),
                    array_type,
                    elem_type
                ));
            }
        }
        if let Type::Arr(t, mut size) = array_type {
            size.push(literal.elements.len());
            literal.typ = Type::Arr(t, size);
            Ok(literal.typ.clone())
        } else if array_type != Type::Unknown {
            literal.typ = Type::Arr(Box::new(array_type), vec![literal.elements.len()]);
            Ok(literal.typ.clone())
        } else {
            Ok(array_type)
        }
    }

    #[trace_call(always)]
    fn type_check_expr_array_access(
        &mut self,
        access: &mut nodes::ArrayAccessNode,
    ) -> Result<Type, String> {
        // FIXME: This is a mess
        let Some(var) = self.get_variable(&access.array_name) else {
            return Err(format!(
                "{}: {:?}: Unknown variable `{}`.",
                ERR_STR,
                access.location,
                access.array_name
            ));
        };
        let Type::Arr(elem, arr_size) = &var.typ else {
            return Err(format!(
                "{}: {:?}: Attempted to index into non-array variable `{}`.\n{}: {:?}: Variable `{}` declared here.",
                ERR_STR,
                access.location,
                access.array_name,
                NOTE_STR,
                var.location,
                var.name,
            ));
        };
        if arr_size.len() != access.indices.elements.len() {
            let i = if arr_size.len() == 1 {
                "index"
            } else {
                "indices"
            };
            return Err(format!(
                "{}: {:?}: Dimension Mismatch in Array indexing. Expected {} {i}, found {}.\n{}: Getting a subarray is not supported yet, you can only get single elements.",
                ERR_STR,
                access.location,
                arr_size.len(),
                access.indices.elements.len(),
                NOTE_STR
            ));
        }
        let t = self.type_check_expr_array_literal(&mut access.indices)?;
        if let Type::Arr(elem_index, _) = t {
            if *elem_index != Type::Usize {
                Err(format!(
                    "{}: {:?}: Array Indices are expected to be type usize, found array of {}.",
                    ERR_STR, access.location, elem_index
                ))
            } else {
                // Indices is Array of Usizes, all is well
                access.typ = *elem.clone();
                Ok(*elem.clone())
            }
        } else if t == Type::Unknown {
            let _index_arr = Type::Arr(Box::new(Type::Usize), vec![arr_size.len()]);
            todo!("access.indices is ArrayLiteralNode, type_check_expression_with_type wants Expression")
            // if let Err(e) = self.type_check_expression_with_type(&mut access.indices, &index_arr) {
            //     Err(format!(
            //         "{}\n{}: {:?}: Array Indices are expected to be type usize.",
            //         e, ERR_STR, access.location
            //     ))
            // } else {
            //     access.typ = *elem.clone();
            //     Ok(*elem.clone())
            // }
        } else {
            // Type of indices is not an Array and not Unknown, what happened??
            // This is unreachable
            unreachable!()
        }
    }

    #[trace_call(always)]
    fn type_check_expr_literal(
        &mut self,
        literal: &mut nodes::LiteralNode,
    ) -> Result<Type, String> {
        Ok(literal.typ.clone())
    }

    #[trace_call(always)]
    fn type_check_expr_function_call(
        &mut self,
        func_call: &mut nodes::CallNode,
    ) -> Result<Type, String> {
        let function = if func_call.is_constructor {
            if let Some(class) = self.known_classes.get(&func_call.function_name) {
                let Some(feat) = class.known_features.get(CONSTRUCTOR_NAME) else {
                    return Err(format!(
                        "{}: {:?}: Class `{}` has no constructor.\n{}: {:?}: Class declared here.\n{}: Implement the {} feature to create a constructor.",
                        ERR_STR, func_call.location, func_call.function_name,
                        NOTE_STR, class.location,
                        NOTE_STR, CONSTRUCTOR_NAME
                    ));
                };
                func_call.function_name =
                    format!("{}_{}", func_call.function_name, CONSTRUCTOR_NAME);
                debug_assert!(
                    class.has_constructor,
                    "Class has constructor feature, but has_constructor is false"
                );
                feat
            } else {
                return Err(format!(
                    "{}: {:?}: Call to unknown class `{}`.\n{}: Capitalized function calls are reserved for class constructors.",
                    ERR_STR,
                    func_call.location,
                    func_call.function_name,
                    NOTE_STR
                ));
            }
        } else {
            let Some(function) = self.known_functions.get(&func_call.function_name) else {
                return Err(format!(
                    "{}: {:?}: Call to unknown function `{}`.",
                    ERR_STR,
                    func_call.location,
                    func_call.function_name
                ));
            };
            function
        };
        let return_type = function.return_type.clone();
        match func_call.arguments.len().cmp(&function.parameters.len()) {
            std::cmp::Ordering::Less => {
                return Err(format!(
                    "{}: {:?}: Not enough arguments for call to function `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Function declared here.",
                    ERR_STR,
                    func_call.location,
                    func_call.function_name,
                    function.parameters.len(),
                    func_call.arguments.len(),
                    NOTE_STR,
                    function.location
                ));
            }
            std::cmp::Ordering::Greater => {
                return Err(format!(
                    "{}: {:?}: Too many arguments for call to function `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Function declared here.",
                    ERR_STR,
                    func_call.location,
                    func_call.function_name,
                    function.parameters.len(),
                    func_call.arguments.len(),
                    NOTE_STR,
                    function.location
                ));
            }
            std::cmp::Ordering::Equal => (),
        }
        let params = function.parameters.clone();
        for (arg, param) in func_call.arguments.iter_mut().zip(params) {
            let expected = param.typ;
            let arg_type = self.type_check_argument(arg)?;
            debug_assert!(arg_type != Type::None);
            if arg_type == Type::Unknown {
                // We need to `infer` the type again
                self.type_check_argument_with_type(arg, &expected)?;
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
        func_call.typ = return_type.t;
        Ok(func_call.typ.clone())
    }

    #[trace_call(always)]
    fn type_check_argument(&mut self, arg: &mut nodes::ArgumentNode) -> Result<Type, String> {
        let typ = self.type_check_expression_node(&mut arg.expression)?;
        arg.typ = typ.clone();
        Ok(typ)
    }

    #[trace_call(always)]
    fn type_check_argument_with_type(
        &mut self,
        arg: &mut nodes::ArgumentNode,
        typ: &Type,
    ) -> Result<(), String> {
        let typ = self.type_check_expression_with_type(&mut arg.expression.expression, typ)?;
        arg.typ = typ.clone();
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_expr_field_access(
        &mut self,
        access: &mut nodes::FieldAccessNode,
    ) -> Result<Type, String> {
        debug_assert!(!self.current_function.is_empty());
        match self.get_variable(&access.name) {
            Some(var) => {
                if !var.is_class_instance() {
                    return Err(format!(
                        "{}: {:?}: Variable `{}` is not a class instance, it has no fields.",
                        ERR_STR, access.location, access.name
                    ));
                }
                let typ = self.type_check_expr_field_access_rec(access, &var)?;
                access.typ = var.typ;
                Ok(typ)
            }
            None => Err(format!(
                "{}: {:?}: Undeclared variable `{}`.",
                ERR_STR, access.location, access.name
            )),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_field_access_rec(
        &mut self,
        access: &mut nodes::FieldAccessNode,
        var: &Variable,
    ) -> Result<Type, String> {
        match &mut (*access.field.expression) {
            nodes::Expression::FieldAccess(field_access) => {
                /*
                get type of current variable
                get fields of current variable, if its class (if not, error)
                check if field_access.name is field (if not, error)
                create temporary variable with name field_access.name and type field.typ
                recursively call type_check_field
                 */
                let typ = &var.typ;
                match typ {
                    Type::Class(class_name) => {
                        let Some(class) = self.known_classes.get(class_name) else {
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
                            location: field.l,
                            typ: field.t,
                        };
                        let typ = self.type_check_expr_field_access_rec(field_access, &var)?;
                        field_access.typ = var.typ.clone();
                        access.field.typ = var.typ.clone();
                        Ok(typ)
                    }
                    _ => Err(format!(
                        "{}: {:?}: Can't access field `{}` of non-class identifier `{}`.\n{}: {:?}: Field `{}` declared here.",
                        ERR_STR,
                        access.location,
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
                        access.location,
                        name_node.name,
                        var.name,
                        NOTE_STR,
                        var.location,
                        var.name
                    ));
                }
                let class_name = var.get_class_name();
                if let Some(class) = self.known_classes.get(&class_name) {
                    match class.get_field(&name_node.name) {
                        Some(field) => {
                            name_node.typ = field.t.clone();
                            access.field.typ = field.t.clone();
                            Ok(field.t)
                        },
                        None => Err(format!(
                            "{}: {:?}: Identifier `{}` has no field `{}`.\n{}: {:?}: Class declared here.",
                            ERR_STR,
                            access.location,
                            access.name,
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
                let Some(class) = self.known_classes.get(&class_name) else {
                    // Lookup of classes and their fields is done before ever evaluating any field access
                    // So at this point, field access only consists of known classes
                    unreachable!()
                };
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
                let mut params = method.parameters.clone();
                // FIXME: Once we introduce static methods, we need to check for that here
                let this = params.remove(0);
                debug_assert!(this.typ == Type::Class(class_name.clone()));
                match function_node.arguments.len().cmp(&params.len()) {
                    std::cmp::Ordering::Less => {
                        return Err(format!(
                            "{}: {:?}: Not enough arguments for call to method `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Method declared here.",
                            ERR_STR,
                            access.location,
                            function_node.function_name,
                            params.len(),
                            function_node.arguments.len(),
                            NOTE_STR,
                            method.location
                        ));
                    }
                    std::cmp::Ordering::Greater => {
                        return Err(format!(
                            "{}: {:?}: Too many arguments for call to method `{}`. Expected {} argument(s), found {}.\n{}: {:?}: Method declared here.",
                            ERR_STR,
                            access.location,
                            function_node.function_name,
                            params.len(),
                            function_node.arguments.len(),
                            NOTE_STR,
                            method.location
                        ));
                    }
                    std::cmp::Ordering::Equal => (),
                }
                for (arg, param) in function_node.arguments.iter_mut().zip(params) {
                    let expected = param.typ;
                    let arg_type = self.type_check_argument(arg)?;
                    debug_assert!(arg_type != Type::None);
                    if arg_type == Type::Unknown {
                        // We need to `infer` the type again
                        self.type_check_argument_with_type(arg, &expected)?;
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
                function_node.typ = return_type.t.clone();
                access.field.typ = return_type.t.clone();
                access.typ = return_type.t.clone();
                Ok(return_type.t)
            }
            _ => unreachable!(),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_builtin(
        &mut self,
        _builtin: &mut nodes::BuiltInNode,
    ) -> Result<Type, String> {
        internal_error!("type_check_expr_builtin is not implemented yet")
    }

    #[trace_call(always)]
    fn type_check_type_node(&mut self, type_node: &mut nodes::TypeNode) -> Result<(), String> {
        match &mut type_node.typ {
            Type::Class(class_name) => {
                if !self.known_classes.contains_key(class_name) {
                    Err(format!(
                        "{}: {:?}: Unknown Type `{}`.",
                        ERR_STR, type_node.location, type_node.typ
                    ))
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

// impl Typecheckable for nodes::IdentifierNode {
//     #[trace_call(always)]
//     fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
//     where
//         Self: Sized,
//     {
//         self.expression.type_check_with_type(checker, typ)?;
//         self.typ = typ.clone();
//         Ok(())
//     }
// }
// impl Typecheckable for nodes::ArrayLiteralNode {
//     #[trace_call(always)]
//     fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
//     where
//         Self: Sized,
//     {
//         if let Type::Arr(expected_type, expected_size) = typ {
//             let expected_count = expected_size[0];
//             if expected_count != self.elements.len() {
//                 return Err(format!(
//                     "{}: {:?}: Size Mismatch when evaluating Array Literal. Expected {} element(s), found {}.",
//                     ERR_STR,
//                     self.location,
//                     expected_count,
//                     self.elements.len()
//                 ));
//             }
//             let sub_expected: Vec<_> = expected_size[1..].to_vec();
//             let sub_type = if sub_expected.is_empty() {
//                 *expected_type.clone()
//             } else {
//                 Type::Arr(expected_type.clone(), sub_expected)
//             };
//             for element in &mut self.elements {
//                 if let Err(e) = element.type_check_with_type(checker, &sub_type) {
//                     return Err(format!(
//                         "{}: {:?}: Error when evaluating type of Array Literal:\n{}",
//                         ERR_STR, self.location, e
//                     ));
//                 }
//             }
//             self.typ = typ.clone();
//             Ok(())
//         } else {
//             Err(format!(
//                 "{}: {:?}: Type Mismatch! Expected {}, found Array Literal.",
//                 ERR_STR, self.location, typ
//             ))
//         }
//     }
// }
// impl Typecheckable for nodes::LiteralNode {
//     #[trace_call(always)]
//     fn type_check_with_type(&mut self, _checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
//     where
//         Self: Sized,
//     {
//         if self.typ != Type::Unknown && self.typ != *typ {
//             return Err(format!(
//                 "{}: {:?}: Type Mismatch! Expected {}, found {}.",
//                 ERR_STR, self.location, typ, self.typ
//             ));
//         } else if let Type::Arr(_, _) = typ {
//             Err(format!(
//                 "{}: {:?} Type Mismatch! Expected Array Literal, found Integer Literal `{}`",
//                 ERR_STR, self.location, self.value
//             ))
//         } else if let Type::Class(class_name) = typ {
//             Err(format!(
//                 "{}: {:?} Type Mismatch! Expected instance of class `{}`, found Integer Literal `{}`",
//                 ERR_STR,
//                 self.location,
//                 class_name,
//                 self.value
//             ))
//         } else {
//             self.typ = typ.clone();
//             Ok(())
//         }
//     }
// }

// impl Typecheckable for nodes::NameNode {
//     #[trace_call(always)]
//     fn type_check_with_type(&mut self, checker: &mut TypeChecker, typ: &Type) -> Result<(), String>
//     where
//         Self: Sized,
//     {
//         if self.typ == Type::Unknown {
//             self.type_check(checker)?;
//         }
//         if self.typ != *typ {
//             return Err(format!(
//                 "{}: {:?}: Type Mismatch! Expected Type {}, found {}.",
//                 ERR_STR, self.location, typ, self.typ
//             ));
//         }
//         Ok(())
//     }
// }
