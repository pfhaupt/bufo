use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::{Location, Operation};

use crate::compiler::CONSTRUCTOR_NAME;
use crate::compiler::{ERR_STR, NOTE_STR};
use crate::util::flags::Flags;
use crate::internal_panic;

use tracer::trace_call;

// FIXME: All this info is in the AST, there's no need to clone this
//        We can just use references to the AST and lifetime parameters
#[derive(Debug)]
enum TypeError {
    /// Syntax: Decl Type, Error Loc, Name, Decl Loc
    Redeclaration(&'static str, Location, String, Location),
    /// Syntax: Error Loc, Type Name
    UnknownType(Location, Type),
    /// Syntax: Error Loc
    ImplicitThisParam(Location),
    /// Syntax: Fn Type, Error Loc, Has This
    TooManyParameters(&'static str, Location, bool),
    /// Syntax: Error Loc, Expected, Found
    TypeMismatch(Location, Type, Type),
    /// Syntax: Error Loc, Binary Op, LHS Loc, LHS Type, RHS Loc, RHS Type
    BinaryTypeMismatch(Location, Operation, Location, Type, Location, Type),
    /// Syntax: Error Loc, Var Name
    UndeclaredVariable(Location, String),
    /// Syntax: Error Loc, Fn Name
    UndeclaredFunction(Location, String),
    /// Syntax: Error Loc, Class Name
    UndeclaredClass(Location, String),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    NotEnoughArguments(&'static str, Location, String, usize, Location, usize),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    TooManyArguments(&'static str, Location, String, usize, Location, usize),
    /// Syntax: Arg Decl, Arg Type, Param Decl, Param Name, Param Type
    ArgParamTypeMismatch(Location, Type, Location, String, Type),
    /// Syntax: Error Loc, Found Type, Decl Loc, Decl Type
    WrongReturnType(Location, Type, Location, Type),
    /// Syntax: Error Loc, Found Type
    ConstructorReturnsValue(Location, Type),
    /// Syntax: Error Loc, Decl Loc, Decl Type
    /// NOTE: This is a special case, where the return type is implicit
    ImplicitReturnType(Location),
    /// Syntax: Error Loc, Expected Size, Found Size
    DimensionMismatch(Location, usize, usize),
    /// Syntax: Error Loc, Decl Loc, Decl Type
    MissingReturn(Location, Location, Type),
    /// Syntax: Error Loc, Var Name, Field Loc, Field Name
    NonClassFieldAccess(Location, String, Location, String),
    /// Syntax: Error Loc, Var Decl, Var Name, Field Name, Class Loc, Class Name
    UnknownField(Location, Location, String, String, Location, String),
    /// Syntax: Error Loc, Var Name, Class Loc, Class Name
    UnknownMethod(Location, String, Location, String),
    /// Syntax: Expected, Error Loc, Found Literal
    UnexpectedLiteral(&'static str, Location, String),
    /// Syntax: Error Loc, Var Name, Decl Loc
    IndexIntoNonArray(Location, String, Location),
    /// Syntax: Error Loc, Class Decl, Class Name
    NoConstructor(Location, Location, String)
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::Redeclaration(kind, loc1, name, loc2) => {
                write!(
                    f,
                    "{}: {:?}: {} redeclaration.\n{}: {:?}: {} `{}` already declared here.",
                    ERR_STR, loc1, kind, NOTE_STR, loc2, kind, name
                )
            }
            TypeError::UnknownType(loc, name) => {
                write!(
                    f,
                    "{}: {:?}: Unknown type `{}`.",
                    ERR_STR, loc, name
                )
            }
            TypeError::ImplicitThisParam(loc) => {
                write!(
                    f,
                    "{}: {:?}: Use of implicit parameter `this`.",
                    ERR_STR, loc
                )
            }
            TypeError::TooManyParameters(kind, loc, has_this) => {
                if *has_this {
                    write!(
                        f,
                        "{}: {:?}: {}s can have at most 4 parameters.\n{}: The implicit parameter `this` counts towards that limit.",
                        ERR_STR, loc, kind, NOTE_STR
                    )
                } else {
                    write!(
                        f,
                        "{}: {:?}: {}s can have at most 4 parameters.",
                        ERR_STR, loc, kind
                    )
                }
            }
            TypeError::TypeMismatch(loc, expected, found) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Expected type `{}`, found type `{}`.",
                    ERR_STR, loc, expected, found
                )
            }
            TypeError::BinaryTypeMismatch(error_loc, op, lhs_loc, lhs_typ, rhs_loc, rhs_typ) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch in binary expression! Operation `{} {} {}` is not defined.\n{}: {:?}: LHS has type `{}`.\n{}: {:?}: RHS has type `{}`.",
                    ERR_STR, error_loc, lhs_typ, op, rhs_typ,
                    ERR_STR, lhs_loc, lhs_typ,
                    ERR_STR, rhs_loc, rhs_typ,
                )
            }
            TypeError::UndeclaredVariable(loc, name) => {
                write!(
                    f,
                    "{}: {:?}: Use of undeclared variable `{}`.",
                    ERR_STR, loc, name
                )
            }
            TypeError::UndeclaredFunction(error_loc, fn_name) => {
                write!(
                    f,
                    "{}: {:?}: Use of undeclared function `{}`.",
                    ERR_STR, error_loc, fn_name
                )
            }
            TypeError::UndeclaredClass(error_loc, class_name) => {
                write!(
                    f,
                    "{}: {:?}: Use of undeclared class `{}`.\n{}: Capitalized function calls are reserved for class constructors.",
                    ERR_STR, error_loc, class_name, NOTE_STR
                )
            }
            TypeError::NotEnoughArguments(fn_kind, error_loc, fn_name, arg_count, fn_loc, param_count) => {
                write!(
                    f,
                    "{}: {:?}: Not enough arguments for {} `{}`.\n{}: {:?}: {} `{}` expects {} arguments, found {}.",
                    ERR_STR, error_loc, fn_kind, fn_name, NOTE_STR, fn_loc, fn_kind, fn_name, param_count, arg_count
                )
            }
            TypeError::TooManyArguments(fn_kind, error_loc, fn_name, arg_count, fn_loc, param_count) => {
                write!(
                    f,
                    "{}: {:?}: Too many arguments for {} `{}`.\n{}: {:?}: {} `{}` expects {} arguments, found {}.",
                    ERR_STR, error_loc, fn_kind, fn_name, NOTE_STR, fn_loc, fn_kind, fn_name, param_count, arg_count
                )
            }
            TypeError::ArgParamTypeMismatch(arg_loc, arg_type, param_loc, param_name, param_type) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Argument is expected to be of type `{}`, found type `{}`.\n{}: {:?}: Parameter `{}` is declared to be of type `{}`.",
                    ERR_STR, arg_loc, param_type, arg_type, NOTE_STR, param_loc, param_name, param_type
                )
            }
            TypeError::WrongReturnType(error_loc, found, decl_loc, decl_type) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Expected type `{}`, found type `{}`.\n{}: {:?}: Function is declared to return `{}` here.",
                    ERR_STR, error_loc, decl_type, found, NOTE_STR, decl_loc, decl_type
                )
            }
            TypeError::ConstructorReturnsValue(error_loc, found) => {
                write!(
                    f,
                    "{}: {:?}: Feature `{CONSTRUCTOR_NAME}` is expected to return None, found {}.\n{}: The return type of constructors is implicit, and should not be specified.",
                    ERR_STR, error_loc, found, NOTE_STR
                )
            }
            TypeError::ImplicitReturnType(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Use of implicit return type for constructor.",
                    ERR_STR, error_loc
                )
            }
            TypeError::DimensionMismatch(loc, expected, found) => {
                let i = if *found == 1 {
                    "index"
                } else {
                    "indices"
                };
                write!(
                    f,
                    "{}: {:?}: Dimension Mismatch in Array indexing! Expected  {} {i}, found {}.\n{}: Getting a subarray is not supported yet, you can only get single elements.",
                    ERR_STR, loc, expected, found, NOTE_STR
                )
            }
            TypeError::MissingReturn(error_loc, decl_loc, decl_type) => {
                write!(
                    f,
                    "{}: {:?}: Missing Return value.\n{}: {:?}: Function is declared to return {} here.",
                    ERR_STR, error_loc, NOTE_STR, decl_loc, decl_type
                )
            }
            TypeError::NonClassFieldAccess(error_loc, var_name, field_loc, field_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access field `{}` of non-class variable `{}`.\n{}: {:?}: Field `{}` is declared here.",
                    ERR_STR, error_loc, field_name, var_name, NOTE_STR, field_loc, field_name
                )
            }
            TypeError::UnknownField(error_loc, var_loc, var_name, field_name, class_loc, class_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access unknown field `{}` of variable `{}`.\n{}: {:?}: Variable `{}` is declared here.\n{}: {:?}: Class `{}` is declared here.",
                    ERR_STR, error_loc, field_name, var_name, NOTE_STR, var_loc, var_name, NOTE_STR, class_loc, class_name
                )
            }
            TypeError::UnknownMethod(error_loc, method_name, class_loc, class_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to call unknown method `{}`.\n{}: {:?}: Class `{}` is declared here.",
                    ERR_STR, error_loc, method_name, NOTE_STR, class_loc, class_name
                )
            }
            TypeError::UnexpectedLiteral(expected, error_loc, found) => {
                write!(
                    f,
                    "{}: {:?}: Unexpected Literal! Expected {}, found `{}`.",
                    ERR_STR, error_loc, expected, found
                )
            }
            TypeError::IndexIntoNonArray(error_loc, var_name, var_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to index into non-array variable `{}`.\n{}: {:?}: Variable `{}` is declared here.",
                    ERR_STR, error_loc, var_name, NOTE_STR, var_loc, var_name
                )
            }
            TypeError::NoConstructor(error_loc, class_loc, class_name) => {
                write!(
                    f,
                    "{}: {:?}: Class `{}` has no constructor.\n{}: {:?}: Class `{}` is declared here.\n{}: Implement the {} feature to create a constructor.",
                    ERR_STR, error_loc, class_name, NOTE_STR, class_loc, class_name, NOTE_STR, CONSTRUCTOR_NAME
                )
            }
        }
    }
}

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
    pub fn size(&self) -> usize {
        match self {
            Type::Arr(t, size) => t.size() * size.iter().product::<usize>(),
            Type::I32 | Type::U32 | Type::F32 | Type::Bool => 4,
            Type::I64 | Type::U64 | Type::F64 | Type::Usize => 8,
            Type::Class(..) => 8,
            Type::None => internal_panic!("Something attempted to get the size of Type::None!"),
            Type::Unknown => {
                internal_panic!("Something attempted to get the size of Type::Unknown!")
            }
        }
    }

    #[trace_call(extra)]
    pub fn is_class(&self) -> bool {
        matches!(self, Type::Class(..))
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
    fn new(name: String, location: Location, has_constructor: bool) -> Self {
        Self {
            name: name.clone(),
            class_type: Type::Class(name),
            location,
            has_constructor,
            fields: HashMap::new(),
            known_methods: HashMap::new(),
            known_features: HashMap::new(),
        }
    }

    #[trace_call(extra)]
    fn add_field(&mut self, field: &nodes::FieldNode) -> Result<(), TypeError> {
        let name = &field.name;
        let location = &field.location;

        match self.fields.get(name) {
            Some(f) => Err(TypeError::Redeclaration(
                "Field",
                location.clone(),
                name.clone(),
                f.l.clone(),
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
    fn add_method(&mut self, method: &nodes::MethodNode) -> Result<(), TypeError> {
        let name = &method.name;
        let location = &method.location;
        match self.known_methods.get(name) {
            Some(f) => Err(TypeError::Redeclaration(
                "Method",
                location.clone(),
                name.clone(),
                f.location.clone(),
            )),
            None => {
                let return_type = &method.return_type.typ;
                let return_loc = &method.return_type.location;
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
    fn add_feature(&mut self, feature: &nodes::FeatureNode) -> Result<(), TypeError> {
        let name = &feature.name;
        let location = &feature.location;
        match self.known_features.get(name) {
            Some(f) => Err(TypeError::Redeclaration(
                "Feature",
                location.clone(),
                name.clone(),
                f.location.clone(),
            )),
            None => {
                let return_type = &feature.return_type.typ;
                let return_loc = &feature.return_type.location;
                let parameters: Vec<_> = feature
                    .parameters
                    .iter()
                    .map(|param| Parameter {
                        name: param.name.clone(),
                        location: param.location.clone(),
                        typ: param.typ.typ.clone(),
                    })
                    .collect();
                let mut func = Function {
                    location: location.clone(),
                    return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                    parameters,
                };
                if feature.is_constructor {
                    if *return_type == self.class_type {
                        self.known_features.insert(name.clone(), func);
                        return Err(TypeError::ImplicitReturnType(
                            return_loc.clone(),
                        ));
                    } else if *return_type != Type::None {
                        self.known_features.insert(name.clone(), func);
                        return Err(TypeError::ConstructorReturnsValue(
                            location.clone(),
                            return_type.clone(),
                        ));
                    } else {
                        func.return_type.t = self.class_type.clone();
                    }
                }
                self.known_features.insert(name.clone(), func);
                Ok(())
            }
        }
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
    current_stack_size: usize,
    errors: Vec<TypeError>,
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
            current_stack_size: 0,
            errors: Vec::new(),
            flags,
        }
    }

    #[trace_call(extra)]
    fn add_function(&mut self, function: &nodes::FunctionNode) {
        let name = &function.name;
        let location = &function.location;
        match self.known_functions.get(name) {
            Some(f) => self.report_error(TypeError::Redeclaration(
                "Function",
                location.clone(),
                name.clone(),
                f.location.clone(),
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
            }
        }
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, ast: &nodes::FileNode) {
        for c in &ast.classes {
            match self.known_classes.get(&c.name) {
                Some(class) => {
                    let err = TypeError::Redeclaration(
                        "Class",
                        c.location.clone(),
                        c.name.clone(),
                        class.location.clone(),
                    );
                    self.report_error(err);
                },
                None => {
                    let mut class = Class::new(c.name.clone(), c.location.clone(), c.has_constructor);
                    for field in &c.fields {
                        if let Err(error) = class.add_field(field) {
                            self.report_error(error);
                        }
                    }
                    for method in &c.methods {
                        if let Err(error) = class.add_method(method) {
                            self.report_error(error);
                        }
                    }
                    for feature in &c.features {
                        if let Err(error) = class.add_feature(feature) {
                            self.report_error(error);
                        }
                    }
                    self.known_classes.insert(c.name.clone(), class);
                }
            }
        }
        for function in &ast.functions {
            self.add_function(function);
        }
    }

    #[trace_call(always)]
    fn report_error(&mut self, error: TypeError) {
        if self.flags.debug {
            println!("[DEBUG] Error: {}", error);
        }
        self.errors.push(error);
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
        self.fill_lookup(file);
        for c in &mut file.classes {
            self.type_check_class(c);
        }
        for f in &mut file.functions {
            self.type_check_function(f);
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            let mut error_string = String::from(self.errors[0].to_string());
            for error in &self.errors[1..] {
                error_string.push_str("\n");
                error_string.push_str(&error.to_string());
            }
            Err(error_string)
        }
    }

    #[trace_call(always)]
    fn type_check_class(&mut self, class: &mut nodes::ClassNode) {
        for field in &mut class.fields {
            self.type_check_field(field);
        }
        for method in &mut class.methods {
            self.type_check_method(method, &class.name);
        }
        for feature in &mut class.features {
            self.type_check_feature(feature, &class.name);
        }
    }

    #[trace_call(always)]
    fn type_check_field(&mut self, field: &mut nodes::FieldNode) {
        self.type_check_type_node(&mut field.type_def)
    }

    // FIXME: Repeating code of features, methods and functions can be extracted into macros
    #[trace_call(always)]
    fn type_check_feature(
        &mut self,
        feature: &mut nodes::FeatureNode,
        class_name: &str,
    ) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_classes.contains_key(class_name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut feature.parameters {
            self.type_check_parameter(param);
        }

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
        // FIXME: Rust is bad, there should be a way to do this without cloning
        for param in &feature_info.parameters.clone() {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.get(&param.name) {
                if p.name == *"this" {
                    self.report_error(TypeError::ImplicitThisParam(
                        param.location.clone(),
                    ));
                } else {
                    self.report_error(TypeError::Redeclaration(
                        "Parameter",
                        param.location.clone(),
                        param.name.clone(),
                        p.location.clone(),
                    ));
                }
            } else {
                parameters.insert(param.name.clone(), var);
            }
        }
        if parameters.len() > 4 {
            self.report_error(TypeError::TooManyParameters(
                "Features",
                feature.location.clone(),
                true // this counts towards the limit
            ));
        }
        self.known_variables.push_back(parameters);
        self.type_check_block(&mut feature.block);
        feature.stack_size = self.current_stack_size;

        if feature.is_constructor {
            feature.return_type.typ = Type::Class(feature.class_name.clone());
        }

        self.current_stack_size = 0;
        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_method(
        &mut self,
        method: &mut nodes::MethodNode,
        class_name: &str,
    ) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_classes.contains_key(class_name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut method.parameters {
            self.type_check_parameter(param);
        }

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
        // FIXME: Rust is bad, there should be a way to do this without cloning
        for param in &method_info.parameters.clone() {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.insert(param.name.clone(), var) {
                if param.name == *"this" {
                    self.report_error(TypeError::ImplicitThisParam(
                        param.location.clone(),
                    ));
                } else {
                    self.report_error(TypeError::Redeclaration(
                        "Parameter",
                        param.location.clone(),
                        param.name.clone(),
                        p.location.clone(),
                    ));
                }
            }
        }
        if parameters.len() > 4 {
            self.report_error(TypeError::TooManyParameters(
                "Methods",
                method.location.clone(),
                true // this counts towards the limit
            ));
        }
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut method.block);
        method.stack_size = self.current_stack_size;

        self.current_stack_size = 0;
        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_function(&mut self, function: &mut nodes::FunctionNode) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_functions.contains_key(&function.name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut function.parameters {
            self.type_check_parameter(param);
        }

        let Some(function_info) = self.known_functions.get(&function.name) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = HashMap::new();
        // FIXME: Rust is bad, there should be a way to do this without cloning
        for param in &function_info.parameters.clone() {
            let var = Variable::new(
                param.name.clone(),
                param.location.clone(),
                param.typ.clone(),
            );
            if let Some(p) = parameters.insert(param.name.clone(), var) {
                self.report_error(TypeError::Redeclaration(
                    "Parameter",
                    param.location.clone(),
                    param.name.clone(),
                    p.location.clone(),
                ));
            }
        }
        if parameters.len() > 4 {
            self.report_error(TypeError::TooManyParameters(
                "Functions",
                function.location.clone(),
                false // Functions do not have an implicit `this` parameter
            ));
        }
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut function.block);
        function.stack_size = self.current_stack_size;

        self.current_stack_size = 0;
        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_parameter(&mut self, parameter: &mut nodes::ParameterNode) {
        self.type_check_type_node(&parameter.typ);
        debug_assert!(parameter.typ.typ != Type::Unknown);
        let var_size = parameter.typ.typ.size();
        self.current_stack_size += var_size;
    }

    #[trace_call(always)]
    fn type_check_block(&mut self, block: &mut nodes::BlockNode) {
        self.add_scope();
        for statement in &mut block.statements {
            self.type_check_statement(statement);
        }
        self.remove_scope();
    }

    #[trace_call(always)]
    fn type_check_statement(&mut self, statement: &mut nodes::Statement) {
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
    fn type_check_stmt_let(&mut self, let_node: &mut nodes::LetNode) {
        match self.get_variable_in_current_scope(&let_node.name) {
            Some(var) => {
                self.report_error(TypeError::Redeclaration(
                    "Variable",
                    let_node.location.clone(),
                    let_node.name.clone(),
                    var.location.clone(),
                ));
            },
            None => {
                self.type_check_type_node(&let_node.typ);
                let var_size = let_node.typ.typ.size();
                self.current_stack_size += var_size;

                let current_scope = self.get_current_scope();
                let var = Variable {
                    name: let_node.name.clone(),
                    location: let_node.location.clone(),
                    typ: let_node.typ.typ.clone(),
                };
                if current_scope.insert(let_node.name.clone(), var).is_some() {
                    internal_panic!(format!("Variable `{}` already exists in current scope", let_node.name));
                }
                let expected_type = &let_node.typ.typ;

                let expr_type = self.type_check_expression_node(&mut let_node.expression);

                if expr_type == Type::Unknown {
                    // Couldnt determine type of expression
                    // We need to `infer` it
                    self.type_check_expression_with_type(
                        &mut let_node.expression.expression,
                        expected_type,
                    );
                } else {
                    if expr_type != *expected_type {
                        self.report_error(TypeError::TypeMismatch(
                            let_node.expression.location.clone(),
                            expected_type.clone(),
                            expr_type,
                        ));
                    }
                }
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_assign(
        &mut self,
        assign_node: &mut nodes::AssignNode,
    ) {
        let expected_type = self.type_check_expr_identifier(&mut assign_node.name);
        let expr_type = self.type_check_expression_node(&mut assign_node.expression);
        if expr_type == Type::Unknown {
            // We need to try and force the type of LHS to RHS
            self.type_check_expression_with_type(
                &mut assign_node.expression.expression,
                &expected_type,
            );
        } else if expr_type != expected_type {
            self.report_error(TypeError::TypeMismatch(
                assign_node.expression.location.clone(),
                expected_type.clone(),
                expr_type,
            ));
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_if(&mut self, if_node: &mut nodes::IfNode) {
        let cond = self.type_check_expr_comparison(&mut if_node.condition);
        if cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                if_node.condition.location.clone(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_block(&mut if_node.if_branch);
        if let Some(else_branch) = &mut if_node.else_branch {
            self.type_check_block(else_branch);
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_return(
        &mut self,
        return_node: &mut nodes::ReturnNode,
    ) {
        debug_assert!(return_node.typ == Type::Unknown);

        let (expected_return_type, location) = if return_node.class.is_none() {
            // We're returning from a normal function
            let Some(function) = self.known_functions.get(&return_node.function) else { unreachable!() };
            let expected_return_type = function.return_type.t.clone();
            let location = function.location.clone();
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(class) = self.known_classes.get(return_node.class.as_ref().unwrap()) else { unreachable!() };
            let (mut location, mut expected_return_type) = (Location::anonymous(), Type::Unknown);
            if let Some(feature) = class.known_features.get(&return_node.function) {
                expected_return_type = feature.return_type.t.clone();
                location = feature.location.clone();
            } else if let Some(method) = class.known_methods.get(&return_node.function) {
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
                self.report_error(TypeError::WrongReturnType(
                    return_node.location.clone(),
                    Type::None,
                    location.clone(),
                    expected_return_type.clone(),
                ));
            }
            let expr_type = self.type_check_expression_node(ret_expr);
            let t = if expr_type == Type::Unknown {
                // we have something like `return 5;`, where we couldn't determine the type
                // so we now have to `infer` the type, and set it accordingly
                self.type_check_expression_with_type(
                    &mut ret_expr.expression,
                    &expected_return_type,
                );
                // Successfully `inferred` type, we can now proceed as normal
                expected_return_type
            } else if expr_type != expected_return_type {
                // Signature expects `expected_return_type`, `return {expr}` has other type for expr
                self.report_error(TypeError::WrongReturnType(
                    return_node.location.clone(),
                    expr_type,
                    location.clone(),
                    expected_return_type.clone(),
                ));
                expected_return_type
            } else {
                // Everything is fine, correct return type was provided
                expected_return_type
            };
            return_node.typ = t.clone();
        } else if expected_return_type != Type::None {
            // No return expression, but we expected return value
            self.report_error(TypeError::MissingReturn(
                return_node.location.clone(),
                location.clone(),
                expected_return_type.clone(),
            ));
            return_node.typ = expected_return_type.clone();
        } else {
            // No return expression
            // No return type expected
            return_node.typ = Type::None;
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_while(&mut self, while_node: &mut nodes::WhileNode) {
        let cond = self.type_check_expr_comparison(&mut while_node.condition);
        if cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                while_node.condition.location.clone(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_block(&mut while_node.block);
    }

    #[trace_call(always)]
    fn type_check_stmt_break(&mut self, _break_node: &mut nodes::BreakNode) {
    }

    #[trace_call(always)]
    fn type_check_stmt_continue(
        &mut self,
        _continue_node: &mut nodes::ContinueNode,
    ) {
    }

    #[trace_call(always)]
    fn type_check_stmt_expression(
        &mut self,
        expression_node: &mut nodes::ExpressionNode,
    ) {
        self.type_check_expression(&mut expression_node.expression);
    }

    #[trace_call(always)]
    fn type_check_expression_node(
        &mut self,
        expression_node: &mut nodes::ExpressionNode,
    ) -> Type {
        self.type_check_expression(&mut expression_node.expression)
    }

    #[trace_call(always)]
    fn type_check_expression(
        &mut self,
        expression: &mut nodes::Expression,
    ) -> Type {
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
    ) -> Type {
        match expression {
            nodes::Expression::Binary(binary_node) => {
                self.type_check_expression_with_type(&mut binary_node.lhs, typ);
                self.type_check_expression_with_type(&mut binary_node.rhs, typ);
                binary_node.typ = typ.clone();
                typ.clone()
            }
            nodes::Expression::Literal(lit_node) => {
                if lit_node.typ != Type::Unknown && lit_node.typ != *typ {
                    self.report_error(TypeError::TypeMismatch(
                        lit_node.location.clone(),
                        typ.clone(),
                        lit_node.typ.clone(),
                    ));
                } else if let Type::Arr(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "array",
                        lit_node.location.clone(),
                        lit_node.value.clone(),
                    ));
                } else if let Type::Class(_) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "class instance",
                        lit_node.location.clone(),
                        lit_node.value.clone(),
                    ));
                } else {
                    lit_node.typ = typ.clone();
                }
                typ.clone()
            }
            nodes::Expression::Identifier(ident_node) => {
                self.type_check_expression_with_type(&mut ident_node.expression, typ);
                ident_node.typ = typ.clone();
                typ.clone()
            }
            nodes::Expression::Name(name_node) => {
                if name_node.typ == Type::Unknown {
                    name_node.typ = typ.clone();
                } else if name_node.typ != *typ {
                    self.report_error(TypeError::TypeMismatch(
                        name_node.location.clone(),
                        typ.clone(),
                        name_node.typ.clone(),
                    ));
                }
                typ.clone()
            }
            e => internal_panic!(format!(
                "type_check_expression_with_type for {:?} is not implemented yet!",
                e
            )),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_name(&mut self, name_node: &mut nodes::NameNode) -> Type {
        let var = self.get_variable(&name_node.name);
        match var {
            Some(var) => {
                name_node.typ = var.typ.clone();
                var.typ
            }
            None => {
                self.report_error(TypeError::UndeclaredVariable(
                    name_node.location.clone(),
                    name_node.name.clone(),
                ));
                Type::None
            },
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs);
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs);
        match (&lhs_type, &rhs_type) {
            (typ @ Type::Class(..), _) | (_, typ @ Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location.clone(),
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                typ.clone()
            }
            (typ @ Type::Arr(..), _) | (_, typ @ Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location.clone(),
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                typ.clone()
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location.clone(),
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::Bool
            }
            (Type::Unknown, Type::Unknown) => Type::Unknown,
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut binary_expr.lhs, other);
                binary_expr.typ = other.clone();
                other.clone()
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other);
                binary_expr.typ = other.clone();
                other.clone()
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        binary_expr.location.clone(),
                        binary_expr.operation.clone(),
                        binary_expr.lhs.get_loc(),
                        lhs.clone(),
                        binary_expr.rhs.get_loc(),
                        rhs.clone(),
                    ));
                }
                binary_expr.typ = lhs.clone();
                lhs.clone()
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_comparison(
        &mut self,
        comp_expr: &mut nodes::ComparisonNode,
    ) -> Type {
        let lhs_type = self.type_check_expression(&mut comp_expr.lhs);
        let rhs_type = self.type_check_expression(&mut comp_expr.rhs);
        match (&lhs_type, &rhs_type) {
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    comp_expr.location.clone(),
                    comp_expr.operation.clone(),
                    comp_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    comp_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::Bool
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    comp_expr.location.clone(),
                    comp_expr.operation.clone(),
                    comp_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    comp_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::Bool
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                self.report_error(TypeError::BinaryTypeMismatch(
                    comp_expr.location.clone(),
                    comp_expr.operation.clone(),
                    comp_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    comp_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::Bool
            }
            (Type::Unknown, Type::Unknown) => {
                // We can't determine the type of either side, let's try to force it
                // FIXME: This is a bit hacky, but it works for now
                self.type_check_expression_with_type(&mut comp_expr.lhs, &Type::I64);
                self.type_check_expression_with_type(&mut comp_expr.rhs, &Type::I64);
                comp_expr.typ = Type::Bool;
                Type::Bool
            },
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut comp_expr.lhs, other);
                comp_expr.typ = Type::Bool;
                Type::Bool
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut comp_expr.rhs, other);
                comp_expr.typ = Type::Bool;
                Type::Bool
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        comp_expr.location.clone(),
                        comp_expr.operation.clone(),
                        comp_expr.lhs.get_loc(),
                        lhs.clone(),
                        comp_expr.rhs.get_loc(),
                        rhs.clone(),
                    ));
                }
                comp_expr.typ = Type::Bool;
                Type::Bool
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_identifier(
        &mut self,
        ident_expr: &mut nodes::IdentifierNode,
    ) -> Type {
        let typ = self.type_check_expression(&mut ident_expr.expression);
        ident_expr.typ = typ.clone();
        typ
    }

    #[trace_call(always)]
    fn type_check_expr_array_literal(
        &mut self,
        literal: &mut nodes::ArrayLiteralNode,
    ) -> Type {
        let mut array_type = Type::Unknown;
        for elem in &mut literal.elements {
            let elem_type = self.type_check_expression(elem);
            if elem_type == Type::Unknown {
                return Type::Unknown;
            } else if array_type == Type::Unknown {
                array_type = elem_type;
            } else if array_type != elem_type {
                self.report_error(TypeError::TypeMismatch(
                    elem.get_loc(),
                    array_type.clone(),
                    elem_type,
                ));
            }
        }
        if let Type::Arr(t, mut size) = array_type {
            size.push(literal.elements.len());
            literal.typ = Type::Arr(t, size);
            literal.typ.clone()
        } else if array_type != Type::Unknown {
            literal.typ = Type::Arr(Box::new(array_type), vec![literal.elements.len()]);
            literal.typ.clone()
        } else {
            array_type
        }
    }

    #[trace_call(always)]
    fn type_check_expr_array_access(
        &mut self,
        access: &mut nodes::ArrayAccessNode,
    ) -> Type {
        // FIXME: This is a mess
        let Some(var) = self.get_variable(&access.array_name) else {
            self.report_error(TypeError::UndeclaredVariable(
                access.location.clone(),
                access.array_name.clone(),
            ));
            return Type::None;
        };
        let Type::Arr(elem, arr_size) = &var.typ else {
            self.report_error(TypeError::IndexIntoNonArray(
                access.location.clone(),
                access.array_name.clone(),
                var.location.clone(),
            ));
            return Type::None;
        };
        if arr_size.len() != access.indices.elements.len() {
            self.report_error(TypeError::DimensionMismatch(
                access.location.clone(),
                arr_size.len(),
                access.indices.elements.len(),
            ));
        }
        let t = self.type_check_expr_array_literal(&mut access.indices);
        if let Type::Arr(elem_index, _) = t {
            if *elem_index != Type::Usize {
                self.report_error(TypeError::TypeMismatch(
                    access.indices.location.clone(),
                    Type::Usize,
                    *elem_index.clone(),
                ));
                Type::None
            } else {
                // Indices is Array of Usizes, all is well
                access.typ = *elem.clone();
                *elem.clone()
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
    ) -> Type {
        literal.typ.clone()
    }

    #[trace_call(always)]
    fn type_check_expr_function_call(
        &mut self,
        func_call: &mut nodes::CallNode,
    ) -> Type {
        let function = if func_call.is_constructor {
            if let Some(class) = self.known_classes.get(&func_call.function_name) {
                let Some(feat) = class.known_features.get(CONSTRUCTOR_NAME) else {
                    self.report_error(TypeError::NoConstructor(
                        func_call.location.clone(),
                        class.location.clone(),
                        class.name.clone(),
                    ));
                    return Type::None;
                };
                func_call.function_name =
                    format!("{}_{}", func_call.function_name, CONSTRUCTOR_NAME);
                debug_assert!(
                    class.has_constructor,
                    "Class has constructor feature, but has_constructor is false"
                );
                feat
            } else {
                self.report_error(TypeError::UndeclaredClass(
                    func_call.location.clone(),
                    func_call.function_name.clone(),
                ));
                return Type::None;
            }
        } else {
            let Some(function) = self.known_functions.get(&func_call.function_name) else {
                self.report_error(TypeError::UndeclaredFunction(
                    func_call.location.clone(),
                    func_call.function_name.clone(),
                ));
                return Type::None;
            };
            function
        };
        let return_type = function.return_type.clone();
        match func_call.arguments.len().cmp(&function.parameters.len()) {
            std::cmp::Ordering::Less => {
                self.report_error(TypeError::NotEnoughArguments(
                    "Function",
                    func_call.location.clone(),
                    func_call.function_name.clone(),
                    func_call.arguments.len(),
                    function.location.clone(),
                    function.parameters.len(),
                ));
                return return_type.t.clone();
            }
            std::cmp::Ordering::Greater => {
                self.report_error(TypeError::TooManyArguments(
                    "Function",
                    func_call.location.clone(),
                    func_call.function_name.clone(),
                    func_call.arguments.len(),
                    function.location.clone(),
                    function.parameters.len(),
                ));
                return return_type.t.clone();
            }
            std::cmp::Ordering::Equal => (),
        }
        let params = function.parameters.clone();
        for (arg, param) in func_call.arguments.iter_mut().zip(params) {
            let expected = param.typ;
            let arg_type = self.type_check_expression_node(arg);
            if arg_type == Type::Unknown {
                // We need to `infer` the type again
                self.type_check_expression_with_type(&mut arg.expression, &expected);
            } else if arg_type != expected {
                self.report_error(TypeError::ArgParamTypeMismatch(
                    arg.location.clone(),
                    arg_type.clone(),
                    param.location.clone(),
                    param.name.clone(),
                    expected.clone(),
                ));
            } else {
                // Everything is cool
            }
        }
        func_call.typ = return_type.t;
        func_call.typ.clone()
    }

    #[trace_call(always)]
    fn type_check_expr_field_access(
        &mut self,
        access: &mut nodes::FieldAccessNode,
    ) -> Type {
        match self.get_variable(&access.name) {
            Some(var) => {
                if !var.is_class_instance() {
                    self.report_error(TypeError::NonClassFieldAccess(
                        access.location.clone(),
                        access.name.clone(),
                        var.location.clone(),
                        var.name.clone(),
                    ));
                }
                let typ = self.type_check_expr_field_access_rec(access, &var);
                access.typ = var.typ;
                typ
            }
            None => {
                self.report_error(TypeError::UndeclaredVariable(
                    access.location.clone(),
                    access.name.clone(),
                ));
                Type::None
            },
        }
    }

    #[trace_call(always)]
    fn type_check_expr_field_access_rec(
        &mut self,
        access: &mut nodes::FieldAccessNode,
        var: &Variable,
    ) -> Type {
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
                            self.report_error(TypeError::UnknownField(
                                access.location.clone(),
                                var.location.clone(),
                                var.name.clone(),
                                field_access.name.clone(),
                                class.location.clone(),
                                class.name.clone(),
                            ));
                            return Type::None;
                        };
                        let var = Variable {
                            name: field_access.name.clone(),
                            location: field.l,
                            typ: field.t,
                        };
                        let typ = self.type_check_expr_field_access_rec(field_access, &var);
                        field_access.typ = var.typ.clone();
                        access.field.typ = var.typ.clone();
                        typ
                    }
                    _ => {
                        self.report_error(TypeError::NonClassFieldAccess(
                            access.location.clone(),
                            field_access.name.clone(),
                            var.location.clone(),
                            var.name.clone(),
                        ));
                        Type::None
                    }
                }
            }
            nodes::Expression::Name(name_node) => {
                if !var.is_class_instance() {
                    self.report_error(TypeError::NonClassFieldAccess(
                        access.location.clone(),
                        name_node.name.clone(),
                        var.location.clone(),
                        var.name.clone(),
                    ));
                }
                let class_name = var.get_class_name();
                if let Some(class) = self.known_classes.get(&class_name) {
                    match class.get_field(&name_node.name) {
                        Some(field) => {
                            name_node.typ = field.t.clone();
                            access.field.typ = field.t.clone();
                            field.t
                        },
                        None => {
                            self.report_error(TypeError::UnknownField(
                                access.location.clone(),
                                var.location.clone(),
                                var.name.clone(),
                                name_node.name.clone(),
                                class.location.clone(),
                                class.name.clone(),
                            ));
                            Type::None
                        }
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
                    self.report_error(TypeError::UnknownMethod(
                        function_node.location.clone(),
                        function_node.function_name.clone(),
                        class.location.clone(),
                        class.name.clone(),
                    ));
                    return Type::None;
                };
                let return_type = method.return_type.clone();
                let mut params = method.parameters.clone();
                // FIXME: Once we introduce static methods, we need to check for that here
                let this = params.remove(0);
                debug_assert!(this.typ == Type::Class(class_name.clone()));
                match function_node.arguments.len().cmp(&params.len()) {
                    std::cmp::Ordering::Less => {
                        self.report_error(TypeError::NotEnoughArguments(
                            "Method",
                            access.location.clone(),
                            function_node.function_name.clone(),
                            function_node.arguments.len(),
                            method.location.clone(),
                            params.len(),
                        ));
                        return return_type.t.clone();
                    }
                    std::cmp::Ordering::Greater => {
                        self.report_error(TypeError::TooManyArguments(
                            "Method",
                            access.location.clone(),
                            function_node.function_name.clone(),
                            function_node.arguments.len(),
                            method.location.clone(),
                            params.len(),
                        ));
                        return return_type.t.clone();
                    }
                    std::cmp::Ordering::Equal => (),
                }
                for (arg, param) in function_node.arguments.iter_mut().zip(params) {
                    let expected = param.typ;
                    let arg_type = self.type_check_expression_node(arg);
                    if arg_type == Type::Unknown {
                        // We need to `infer` the type again
                        self.type_check_expression_with_type(&mut arg.expression, &expected);
                    } else if arg_type != expected {
                        self.report_error(TypeError::ArgParamTypeMismatch(
                            arg.location.clone(),
                            arg_type.clone(),
                            param.location.clone(),
                            param.name.clone(),
                            expected.clone(),
                        ));
                    } else {
                        // Everything is cool
                    }
                }
                function_node.typ = return_type.t.clone();
                access.field.typ = return_type.t.clone();
                access.typ = return_type.t.clone();
                return_type.t
            }
            _ => unreachable!(),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_builtin(
        &mut self,
        _builtin: &mut nodes::BuiltInNode,
    ) -> Type {
        internal_panic!("type_check_expr_builtin is not implemented yet")
    }

    #[trace_call(always)]
    fn type_check_type_node(&mut self, type_node: &nodes::TypeNode) {
        if let Type::Class(class_name) = &type_node.typ {
            if !self.known_classes.contains_key(class_name) {
                self.report_error(TypeError::UnknownType(
                    type_node.location.clone(),
                    type_node.typ.clone(),
                ));
            }
        }
    }
}

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
