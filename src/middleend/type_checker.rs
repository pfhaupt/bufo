use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::{Location, Operation, CONSTRUCTOR_KEYWORD};

use crate::compiler::{ERR_STR, NOTE_STR};
use crate::util::flags::Flags;
use crate::internal_panic;

use tracer::trace_call;


macro_rules! check_function {
    ($tc:ident, $call_node:ident, $func_info:ident, $typ:expr, $implicit_this:expr) => {
        {
            let mut params = $func_info.parameters.clone();
            if $implicit_this {
                params.remove(0);
            }
            let return_type = $func_info.return_type.t.clone();
            match $call_node.arguments.len().cmp(&params.len()) {
                std::cmp::Ordering::Less => {
                    $tc.report_error(TypeError::NotEnoughArguments(
                        $typ,
                        $call_node.location.clone(),
                        $call_node.function_name.clone(),
                        $call_node.arguments.len(),
                        $func_info.location.clone(),
                        params.len(),
                    ));
                    return return_type;
                }
                std::cmp::Ordering::Greater => {
                    $tc.report_error(TypeError::TooManyArguments(
                        $typ,
                        $call_node.location.clone(),
                        $call_node.function_name.clone(),
                        $call_node.arguments.len(),
                        $func_info.location.clone(),
                        params.len(),
                    ));
                    return return_type;
                }
                std::cmp::Ordering::Equal => (),
            }

            for (mut arg, param) in $call_node.arguments.iter_mut().zip(params) {
                let expected = param.typ;
                let arg_type = $tc.type_check_expression(arg);
                if arg_type == Type::Unknown {
                    // We need to `infer` the type again
                    $tc.type_check_expression_with_type(&mut arg, &expected);
                } else if arg_type != expected {
                    $tc.report_error(TypeError::ArgParamTypeMismatch(
                        arg.get_loc(),
                        arg_type.clone(),
                        param.location.clone(),
                        param.name.clone(),
                        expected.clone(),
                    ));
                } else {
                    // Everything is cool
                }
            }

            $call_node.typ = return_type;
            $call_node.typ.clone()
        }
    };
}


// FIXME: All this info is in the AST, there's no need to clone this
//        We can just use references to the AST and lifetime parameters
#[derive(Debug)]
enum TypeError {
    /// Syntax: Decl Type, Error Loc, Name, Decl Loc
    Redeclaration(&'static str, Location, String, Location),
    /// Syntax: Error Loc, Fn Name, Extern Loc
    ExternFunction(Location, String, Location),
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
    /// Syntax: Error Loc, Expected Size, Found Size
    DimensionMismatch(Location, usize, usize),
    /// Syntax: Error Loc, Decl Loc, Decl Type
    MissingReturn(Location, Location, Type),
    /// Syntax: Error Loc, Var Decl, Var Name, Field Name, Class Loc, Class Name
    UnknownField(Location, String, Location, String),
    /// Syntax: Error Loc, Var Name, Class Loc, Class Name
    UnknownMethod(Location, String, Location, String),
    /// Syntax: Expected, Error Loc, Found Literal
    UnexpectedLiteral(&'static str, Location, String),
    /// Syntax: Error Loc, Var Name, Decl Loc
    IndexIntoNonArray(Location, String, Location),
    /// Syntax: Error Loc, Class Decl, Class Name
    NoConstructor(Location, Location, String),
    /// Syntax: Error Loc
    DotOnNonClass(Location),
    /// Syntax: Error Loc
    InvalidLValue(Location),
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
            TypeError::ExternFunction(error_loc, fn_name, fn_loc) => {
                // TODO: Change this to a warning once the compiler is more mature
                //       This should be a warning because the external simply shadows the function
                write!(
                    f,
                    "{}: {:?}: Function redeclaration.\n{}: {:?}: Intrinsic `{}` already declared here.",
                    ERR_STR, error_loc, NOTE_STR, fn_loc, fn_name
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
            TypeError::UnknownField(error_loc, field_name, class_loc, class_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access unknown field `{}` of instance of class `{}`.\n{}: {:?}: Class `{}` is declared here.",
                    ERR_STR, error_loc, field_name, class_name, NOTE_STR, class_loc, class_name
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
                    "{}: {:?}: Class `{}` has no constructor.\n{}: {:?}: Class `{}` is declared here.\n{}: Implement the special `{}() {{}}` method to create a constructor.",
                    ERR_STR, error_loc, class_name, NOTE_STR, class_loc, class_name, NOTE_STR, CONSTRUCTOR_KEYWORD
                )
            }
            TypeError::DotOnNonClass(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access field of non-class value.",
                    ERR_STR, error_loc
                )
            }
            TypeError::InvalidLValue(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to assign to non-assignable value.",
                    ERR_STR, error_loc
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

    #[trace_call(extra)]
    pub fn get_class_name(&self) -> String {
        match self {
            Type::Class(name) => name.clone(),
            _ => panic!(),
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
struct Function {
    location: Location,
    return_type: TypeLoc,
    parameters: Vec<Variable>,
}

impl Function {
    #[trace_call(extra)]
    fn get_parameters_as_hashmap(&self) -> HashMap<String, Variable> {
        let mut parameters = HashMap::new();
        for param in &self.parameters {
            parameters.insert(param.name.clone(), param.clone());
        }
        parameters
    }
}
// FIXME: constructor(this: ClassName) { ... } should not be allowed because ImplicitThisParam
macro_rules! check_parameters {
    ($tc:ident, $function:ident, $implicit_this:expr) => {
        {
            let mut parameters: HashMap<String, Variable> = HashMap::new();
            let mut errors: Vec<TypeError> = Vec::new();
            for param in &$function.parameters {
                let p = Variable {
                    name: param.name.clone(),
                    location: param.location.clone(),
                    typ: param.typ.typ.clone(),
                };
                if let Some(p) = parameters.get(&param.name) {
                    if p.name == *"this" {
                        errors.push(TypeError::ImplicitThisParam(
                            param.location.clone(),
                        ));
                    } else {
                        errors.push(TypeError::Redeclaration(
                            "Parameter",
                            param.location.clone(),
                            param.name.clone(),
                            p.location.clone(),
                        ));
                    }
                } else {
                    parameters.insert(param.name.clone(), p);
                }
            }
            if parameters.len() > 4 {
                errors.push(TypeError::TooManyParameters(
                    "Method",
                    $function.location.clone(),
                    $implicit_this,
                ));
            }
            (parameters.into_iter().map(|(_, v)| v).collect(), errors)
        }
    };
}


#[derive(Debug)]
pub struct Class {
    name: String,
    location: Location,
    fields: HashMap<String, TypeLoc>,
    known_methods: HashMap<String, Function>,
    known_constructors: Vec<Function>,
    has_constructor: bool,
}

impl Class {
    #[trace_call(extra)]
    fn new(name: String, location: Location, has_constructor: bool) -> Self {
        Self {
            name,
            location,
            has_constructor,
            fields: HashMap::new(),
            known_methods: HashMap::new(),
            known_constructors: Vec::new(),
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
    fn get_field(&self, name: &str) -> Option<TypeLoc> {
        self.fields.get(name).cloned()
    }

    #[trace_call(extra)]
    fn get_method(&self, name: &str) -> Option<&Function> {
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
pub struct TypeChecker<'flags> {
    known_externs: HashMap<String, Function>,
    known_classes: HashMap<String, Class>,
    known_functions: HashMap<String, Function>,
    known_variables: VecDeque<HashMap<String, Variable>>,
    current_stack_size: usize,
    errors: Vec<TypeError>,
    #[allow(unused)]
    flags: &'flags Flags,
}

impl<'flags> TypeChecker<'flags> {
    #[trace_call(extra)]
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            known_externs: HashMap::new(),
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
        if let Some(external) = self.known_externs.get(name) {
            self.report_error(TypeError::ExternFunction(
                location.clone(),
                name.clone(),
                external.location.clone(),
            ));
        }
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
                    .map(|param| Variable {
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

    #[trace_call(extra)]
    fn add_extern(&mut self, extern_node: &nodes::ExternNode) {
        let name = &extern_node.name;
        let location = &extern_node.location;
        match self.known_externs.get(name) {
            Some(f) => self.report_error(TypeError::Redeclaration(
                "Extern",
                location.clone(),
                name.clone(),
                f.location.clone(),
            )),
            None => {
                let return_type = &extern_node.return_type.typ;
                let return_loc = &extern_node.return_type.location;
                let parameters: Vec<_> = extern_node
                    .parameters
                    .iter()
                    .map(|param| Variable {
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
                self.known_externs.insert(name.clone(), func);
            }
        }
    }

    #[trace_call(extra)]
    fn add_method_to_class(&mut self, method: &nodes::MethodNode, class: &mut Class) {
        let name = &method.name;
        let location = &method.location;
        if class.known_methods.get(name).is_some() {
            self.report_error(TypeError::Redeclaration(
                "Method",
                location.clone(),
                name.clone(),
                class.location.clone(),
            ));
        } else {
            let return_type = &method.return_type.typ;
            let return_loc = &method.return_type.location;
            let (parameters, errors) = check_parameters!(self, method, true);
            let method = Function {
                location: location.clone(),
                return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                parameters,
            };
            class.known_methods.insert(name.clone(), method);
            for error in errors {
                self.report_error(error);
            }
        }
    }

    #[trace_call(extra)]
    fn add_constructor_to_class(
        &mut self,
        constructor: &nodes::ConstructorNode,
        class: &mut Class
    ) {
        let location = &constructor.location;
        if class.known_constructors.len() > 0 {
            self.report_error(TypeError::Redeclaration(
                "Constructor",
                location.clone(),
                class.name.to_string(),
                class.location.clone(),
            ));
        } else {
            let return_type = &constructor.return_type.typ;
            let return_loc = &constructor.return_type.location;
            let (parameters, errors) = check_parameters!(self, constructor, true);
            let constructor = Function {
                location: location.clone(),
                return_type: TypeLoc::new(return_loc.clone(), return_type.clone()),
                parameters,
            };
            class.known_constructors.push(constructor);
            for error in errors {
                self.report_error(error);
            }
        }
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, ast: &nodes::FileNode) {
        for extern_node in &ast.externs {
            self.add_extern(extern_node);
        }
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
                    let mut class = Class::new(c.name.clone(), c.location.clone(), c.constructors.len() > 0);
                    for field in &c.fields {
                        if let Err(error) = class.add_field(field) {
                            self.report_error(error);
                        }
                    }
                    for method in &c.methods {
                        self.add_method_to_class(method, &mut class);
                    }
                    for constructor in &c.constructors {
                        self.add_constructor_to_class(constructor, &mut class);
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
        for feature in &mut class.constructors {
            self.type_check_constructor(feature, &class.name);
        }
    }

    #[trace_call(always)]
    fn type_check_field(&mut self, field: &mut nodes::FieldNode) {
        self.type_check_type_node(&mut field.type_def)
    }

    #[trace_call(always)]
    fn type_check_constructor(
        &mut self,
        constructor: &mut nodes::ConstructorNode,
        class_name: &str,
    ) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_classes.contains_key(class_name));
        debug_assert!(self.current_stack_size == 0);

        for param in &mut constructor.parameters {
            self.type_check_parameter(param);
        }

        let Some(class_info) = self.known_classes.get(class_name) else { unreachable!() };
        let Some(constructor_info) = class_info.known_constructors.get(0) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = constructor_info.get_parameters_as_hashmap();
        parameters.insert(
            "this".to_string(),
            Variable::new(
                "this".to_string(),
                constructor.location.clone(),
                Type::Class(class_name.to_string()),
            ),
        );
        self.current_stack_size += 8; // `this` is always 8 bytes

        self.known_variables.push_back(parameters);
        self.type_check_block(&mut constructor.block);
        constructor.stack_size = self.current_stack_size;

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
        let parameters = method_info.get_parameters_as_hashmap();
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
        let parameters = function_info.get_parameters_as_hashmap();
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
            nodes::Statement::Block(block_node) => self.type_check_block(block_node),
            nodes::Statement::Let(let_node) => self.type_check_stmt_let(let_node),
            nodes::Statement::If(if_node) => self.type_check_stmt_if(if_node),
            nodes::Statement::Return(return_node) => self.type_check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.type_check_stmt_while(while_node),
            nodes::Statement::Break(break_node) => self.type_check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => {
                self.type_check_stmt_continue(continue_node)
            }
            nodes::Statement::Expression(expression_node) => {
                self.type_check_expression(expression_node);
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

                let expr_type = self.type_check_expression(&mut let_node.expression);

                if expr_type == Type::Unknown {
                    // Couldnt determine type of expression
                    // We need to `infer` it
                    self.type_check_expression_with_type(
                        &mut let_node.expression,
                        expected_type,
                    );
                } else {
                    if expr_type != *expected_type {
                        self.report_error(TypeError::TypeMismatch(
                            let_node.expression.get_loc(),
                            expected_type.clone(),
                            expr_type,
                        ));
                    }
                }
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_if(&mut self, if_node: &mut nodes::IfNode) {
        let cond = self.type_check_expression(&mut if_node.condition);
        if cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                if_node.condition.get_loc(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_statement(&mut if_node.if_body);
        if let Some(else_branch) = &mut if_node.else_body {
            self.type_check_statement(else_branch);
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
            if let Some(method) = class.known_methods.get(&return_node.function) {
                expected_return_type = method.return_type.t.clone();
                location = method.location.clone();
            }
            debug_assert!(expected_return_type != Type::Unknown);
            debug_assert!(location != Location::anonymous());
            (expected_return_type, location)
        };

        if let Some(ref mut ret_expr) = &mut return_node.return_value {
            if expected_return_type == Type::None {
                // Found expression but expected none, makes no sense
                self.report_error(TypeError::WrongReturnType(
                    return_node.location.clone(),
                    Type::None,
                    location.clone(),
                    expected_return_type.clone(),
                ));
            }
            let expr_type = self.type_check_expression(ret_expr);
            let t = if expr_type == Type::Unknown {
                // we have something like `return 5;`, where we couldn't determine the type
                // so we now have to `infer` the type, and set it accordingly
                self.type_check_expression_with_type(
                    ret_expr,
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
        let cond = self.type_check_expression(&mut while_node.condition);
        if cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                while_node.condition.get_loc(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_statement(&mut while_node.body);
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
    fn type_check_expression(
        &mut self,
        expression: &mut nodes::Expression,
    ) -> Type {
        match expression {
            nodes::Expression::Name(name_node) => self.type_check_expr_name(name_node),
            nodes::Expression::Unary(unary_expr) => self.type_check_expr_unary(unary_expr),
            nodes::Expression::Binary(binary_expr) => self.type_check_expr_binary(binary_expr),
            nodes::Expression::FunctionCall(func_call) => {
                self.type_check_expr_function_call(func_call)
            }
            nodes::Expression::BuiltIn(built_in) => self.type_check_expr_builtin(built_in),
            nodes::Expression::ArrayAccess(access) => self.type_check_expr_array_access(access),
            nodes::Expression::ArrayLiteral(literal) => self.type_check_expr_array_literal(literal),
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
    fn type_check_expr_unary(
        &mut self,
        _unary_expr: &mut nodes::UnaryNode
    ) -> Type {
        todo!()
    }

    #[trace_call(always)]
    fn type_check_expr_binary(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        match binary_expr.operation {
            Operation::Dot => self.type_check_expr_dot(binary_expr),
            Operation::Assign => self.type_check_expr_assign(binary_expr),
            _ if binary_expr.is_comparison() => self.type_check_expr_binary_comparison(binary_expr),
            _ if binary_expr.is_arithmetic() => self.type_check_expr_binary_arithmetic(binary_expr),
            o => todo!("type_check_expr_binary for {:?} is not implemented yet!", o),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_arithmetic(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs);
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs);
        assert!(binary_expr.is_arithmetic());
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
                binary_expr.typ = typ.clone();
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
                binary_expr.typ = typ.clone();
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
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                typ.clone()
            }
            (Type::Unknown, Type::Unknown) => Type::Unknown,
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut binary_expr.lhs, other);
                let typ = other;
                binary_expr.typ = typ.clone();
                typ.clone()
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other);
                let typ = other.clone();
                binary_expr.typ = typ.clone();
                typ
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
                let typ = lhs.clone();
                binary_expr.typ = typ.clone();
                typ
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_comparison(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs);
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs);
        assert!(binary_expr.is_comparison());
        match (&lhs_type, &rhs_type) {
            (Type::Class(..), _) | (_, Type::Class(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
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
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
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
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                typ
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other);
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                typ
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
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                typ
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_assign(
        &mut self,
        assign_expr: &mut nodes::BinaryNode,
    ) -> Type {
        if !assign_expr.lhs.is_lvalue() {
            self.report_error(TypeError::InvalidLValue(
                assign_expr.lhs.get_loc(),
            ));
            return Type::None;
        }
        let lhs_type = self.type_check_expression(&mut assign_expr.lhs);
        if lhs_type == Type::None {
            // Error propagation, for example LHS is unknown field of instance
            return Type::None;
        }
        let rhs_type = self.type_check_expression(&mut assign_expr.rhs);
        debug_assert!(rhs_type != Type::None);

        if lhs_type == Type::Unknown {
            // Note: This is an error, how can we assign to something we don't know the type of?
            todo!()
        } else if rhs_type == Type::Unknown {
            // Infer type of rhs from lhs
            let typ = self.type_check_expression_with_type(&mut assign_expr.rhs, &lhs_type);
            assign_expr.typ = typ.clone();
            typ
        } else if lhs_type != rhs_type {
            self.report_error(TypeError::TypeMismatch(
                assign_expr.location.clone(),
                lhs_type.clone(),
                rhs_type.clone(),
            ));
            Type::None
        } else {
            assign_expr.typ = rhs_type.clone();
            rhs_type
        }


    }

    #[trace_call(always)]
    fn type_check_expr_dot(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs);
        if let Type::Class(class_name) = lhs_type {
            let Some(class) = self.known_classes.get(&class_name) else {
                self.report_error(TypeError::UndeclaredClass(
                    binary_expr.lhs.get_loc(),
                    class_name.clone(),
                ));
                return Type::None;
            };
            match &mut (*binary_expr.rhs) {
                nodes::Expression::Name(name_node) => {
                    if let Some(field) = class.get_field(&name_node.name) {
                        binary_expr.typ = field.t.clone();
                        name_node.typ = field.t.clone();
                        field.t.clone()
                    } else {
                        self.report_error(TypeError::UnknownField(
                            name_node.location.clone(),
                            name_node.name.clone(),
                            class.location.clone(),
                            class.name.clone(),
                        ));
                        Type::None
                    }
                }
                nodes::Expression::FunctionCall(call_node) => {
                    // FIXME: Error Log shows wrong location
                    if let Some(method) = class.get_method(&call_node.function_name) {
                        let result = check_function!(self, call_node, method, "Method", true);
                        binary_expr.typ = result.clone();
                        result
                    } else {
                        self.report_error(TypeError::UnknownMethod(
                            call_node.location.clone(),
                            call_node.function_name.clone(),
                            class.location.clone(),
                            class.name.clone(),
                        ));
                        Type::None
                    }
                }
                _ => {
                    // Not a field, not a method, this is an error
                    todo!()
                },
            }
        } else {
            self.report_error(TypeError::DotOnNonClass(
                binary_expr.lhs.get_loc(),
            ));
            Type::None
        }
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
        let function = if func_call.is_extern {
            debug_assert!(!func_call.is_constructor);
            let Some(external) = self.known_externs.get(&func_call.function_name) else {
                // is_builtin -> Parser says this is an external function
                // known_externs -> Filled by type_check_file, so it should contain all externs
                // This is unreachable, or the parser is broken
                unreachable!()
            };
            external
        } else if func_call.is_constructor {
            debug_assert!(!func_call.is_extern);
            if let Some(class) = self.known_classes.get(&func_call.function_name) {
                let Some(constructor) = class.known_constructors.get(0) else {
                    self.report_error(TypeError::NoConstructor(
                        func_call.location.clone(),
                        class.location.clone(),
                        class.name.clone(),
                    ));
                    return Type::None;
                };
                func_call.function_name =
                    format!("{}_{}", func_call.function_name, CONSTRUCTOR_KEYWORD);
                debug_assert!(
                    class.has_constructor,
                    "Class has constructor feature, but has_constructor is false"
                );
                constructor
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
        if let Type::Class(class_name) = &return_type.t {
            if !self.known_classes.contains_key(class_name) {
                self.report_error(TypeError::UndeclaredClass(
                    func_call.location.clone(),
                    class_name.clone(),
                ));
                self.report_error(TypeError::UnknownType(
                    return_type.l.clone(),
                    return_type.t.clone(),
                ));
                return Type::None;
            }
        }
        check_function!(self, func_call, function, "Function", false)
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
