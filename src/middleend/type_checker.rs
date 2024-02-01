use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::{Location, Operation, CONSTRUCTOR_KEYWORD};

use crate::compiler::{ERR_STR, NOTE_STR};
use crate::util::flags::Flags;
use crate::internal_panic;

use tracer::trace_call;


macro_rules! check_function {
    ($tc:ident, $call_node:ident, $func_info:ident, $typ:expr) => {
        {
            let return_type = $func_info.return_type.t.clone();
            match $call_node.arguments.len().cmp(&$func_info.parameters.len()) {
                std::cmp::Ordering::Less => {
                    $tc.report_error(TypeError::NotEnoughArguments(
                        $typ,
                        $call_node.location,
                        $call_node.function_name.clone(),
                        $call_node.arguments.len(),
                        $func_info.location,
                        $func_info.parameters.len(),
                    ));
                    return return_type;
                }
                std::cmp::Ordering::Greater => {
                    $tc.report_error(TypeError::TooManyArguments(
                        $typ,
                        $call_node.location,
                        $call_node.function_name.clone(),
                        $call_node.arguments.len(),
                        $func_info.location,
                        $func_info.parameters.len(),
                    ));
                    return return_type;
                }
                std::cmp::Ordering::Equal => (),
            }

            for (mut arg, param) in $call_node.arguments.iter_mut().zip($func_info.parameters.clone()) {
                let expected = param.typ;
                // TODO: Mutability error should specify that this is a parameter
                let arg_type = $tc.type_check_expression(arg, param.is_mutable && expected.is_struct());
                if arg_type == Type::None {
                    continue;
                }
                if arg_type == Type::Unknown {
                    // We need to `infer` the type again
                    $tc.type_check_expression_with_type(&mut arg, &expected);
                    debug_assert!(!expected.is_struct());
                } else if arg_type != expected {
                    $tc.report_error(TypeError::ArgParamTypeMismatch(
                        arg.get_loc(),
                        arg_type.clone(),
                        param.location,
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

macro_rules! check_parameters {
    ($tc:ident, $function:ident) => {
        {
            let mut parameters: Vec<Variable> = Vec::new();
            let mut errors: Vec<TypeError> = Vec::new();
            for param in &$function.parameters {
                let p = Variable {
                    name: param.name.clone(),
                    location: param.location,
                    typ: param.typ.typ.clone(),
                    is_mutable: param.is_mutable,
                };
                let index_of = parameters.iter().position(|p| p.name == param.name);
                if index_of.is_some() {
                    let index_of = index_of.unwrap();
                    let p2 = &parameters[index_of];
                    errors.push(TypeError::Redeclaration(
                        "Parameter",
                        param.location,
                        param.name.clone(),
                        p2.location,
                    ));
                } else {
                    parameters.push(p);
                }
            }
            // in llvm we're not limited to 4 parameters
            #[cfg(not(feature = "llvm"))]
            if parameters.len() > 4 {
                errors.push(TypeError::TooManyParameters(
                    "Method",
                    $function.location,
                ));
            }
            (parameters, errors)
        }
    };
}

macro_rules! check_or_abort {
    ($lhs:ident, $lhs_func:expr, $rhs:ident, $rhs_func:expr) => {
        let $lhs = $lhs_func;
        let $rhs = $rhs_func;
        if $lhs == Type::None || $rhs == Type::None {
            return Type::None;
        }
    };
    ($val:ident, $func:expr) => {
        let $val = $func;
        if $val == Type::None {
            return Type::None;
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
    /// Syntax: Fn Type, Error Loc
    #[cfg(not(feature = "llvm"))]
    TooManyParameters(&'static str, Location),
    /// Syntax: Error Loc, Expected, Found
    TypeMismatch(Location, Type, Type),
    /// Syntax: Error Loc, Binary Op, LHS Loc, LHS Type, RHS Loc, RHS Type
    BinaryTypeMismatch(Location, Operation, Location, Type, Location, Type),
    /// Syntax: Error Loc, Var Name
    UndeclaredVariable(Location, String),
    /// Syntax: Error Loc, Fn Name
    UndeclaredFunction(Location, String),
    /// Syntax: Error Loc, Struct Name
    UndeclaredStruct(Location, String),
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
    /// Syntax: Error Loc, Var Decl, Var Name, Field Name, Struct Loc, Struct Name
    UnknownField(Location, String, Location, String),
    /// Syntax: Error Loc, Var Name, Struct Loc, Struct Name
    UnknownMethod(Location, String, Location, String),
    /// Syntax: Expected, Error Loc, Found Literal
    UnexpectedLiteral(&'static str, Location, String),
    /// Syntax: Error Loc, Var Name, Decl Loc
    IndexIntoNonArray(Location, String, Location),
    /// Syntax: Error Loc, Struct Decl, Struct Name
    NoConstructor(Location, Location, String),
    /// Syntax: Error Loc
    DotOnNonStruct(Location),
    /// Syntax: Error Loc
    InvalidLValue(Location),
    /// Syntax: Error Loc, Type
    NegationTypeMismatch(Location, Type),
    /// Syntax: Error Loc, Var Name, Decl Loc
    ImmutableModification(Location, String, Location),
    /// Syntax: Error Loc
    CantMutateTemporary(Location),
    /// Syntax: Error Loc, Struct Name, Previous Struct Info
    RecursiveStruct(Location, String, Vec<(Location, String)>),
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
            #[cfg(not(feature = "llvm"))]
            TypeError::TooManyParameters(kind, loc) => {
                write!(
                    f,
                    "{}: {:?}: {}s can have at most 4 parameters.",
                    ERR_STR, loc, kind
                )
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
            TypeError::UndeclaredStruct(error_loc, struct_name) => {
                write!(
                    f,
                    "{}: {:?}: Use of undeclared struct `{}`.\n{}: Capitalized function calls are reserved for struct constructors.",
                    ERR_STR, error_loc, struct_name, NOTE_STR
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
            TypeError::UnknownField(error_loc, field_name, struct_loc, struct_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access unknown field `{}` of instance of struct `{}`.\n{}: {:?}: Struct `{}` is declared here.",
                    ERR_STR, error_loc, field_name, struct_name, NOTE_STR, struct_loc, struct_name
                )
            }
            TypeError::UnknownMethod(error_loc, method_name, struct_loc, struct_name) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to call unknown method `{}`.\n{}: {:?}: Struct `{}` is declared here.",
                    ERR_STR, error_loc, method_name, NOTE_STR, struct_loc, struct_name
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
            TypeError::NoConstructor(error_loc, struct_loc, struct_name) => {
                write!(
                    f,
                    "{}: {:?}: Struct `{}` has no constructor.\n{}: {:?}: Struct `{}` is declared here.\n{}: Implement the special `{}() {{}}` method to create a constructor.",
                    ERR_STR, error_loc, struct_name, NOTE_STR, struct_loc, struct_name, NOTE_STR, CONSTRUCTOR_KEYWORD
                )
            }
            TypeError::DotOnNonStruct(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to access field of non-struct value.",
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
            TypeError::NegationTypeMismatch(error_loc, typ) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Negation is not defined for type `{}`.",
                    ERR_STR, error_loc, typ
                )
            }
            TypeError::ImmutableModification(error_loc, var_name, var_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to modify immutable variable `{}`.\n{}: {:?}: Variable `{}` is declared here.",
                    ERR_STR, error_loc, var_name, NOTE_STR, var_loc, var_name
                )
            }
            TypeError::CantMutateTemporary(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to mutate temporary value.\n{}: Temporary values are only valid for the duration of the statement they are declared in.\n{}: If you want to mutate a value, declare it as a variable instead.",
                    ERR_STR, error_loc, NOTE_STR, NOTE_STR
                )
            }
            TypeError::RecursiveStruct(error_loc, struct_name, struct_locs) => {
                let mut message = format!(
                    "{}: {:?}: Recursive struct `{}`.", ERR_STR, error_loc, struct_name
                );
                for (loc, name) in struct_locs.iter().skip(1) {
                    message.push_str(&format!("\n{}: {:?}: Chain of recursion also includes Struct `{}`.", NOTE_STR, loc, name));
                }
                write!(f, "{}", message)
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
    Struct(String),
    // Reserved for later use
    F32,
    F64,
}

impl Type {
    #[trace_call(extra)]
    pub fn size(&self) -> usize {
        match self {
            Type::Arr(t, size) => t.size() * size.iter().product::<usize>(),
            Type::I32 | Type::U32 | Type::F32 => 4,
            Type::I64 | Type::U64 | Type::F64 | Type::Usize => 8,
            Type::Bool => 1,
            Type::Struct(..) => internal_panic!("Something attempted to get the size of a struct!"),
            Type::None => internal_panic!("Something attempted to get the size of Type::None!"),
            Type::Unknown => {
                internal_panic!("Something attempted to get the size of Type::Unknown!")
            }
        }
    }

    #[trace_call(extra)]
    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(..))
    }

    #[trace_call(extra)]
    pub fn get_struct_name(&self) -> String {
        match self {
            Type::Struct(name) => name.clone(),
            _ => panic!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Struct(str) => write!(fmt, "{}", str),
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
    has_this: bool,
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

#[derive(Debug)]
pub struct Struct {
    name: String,
    location: Location,
    fields: HashMap<String, TypeLoc>,
    known_methods: HashMap<String, Function>,
    known_constructors: Vec<Function>,
}

impl Struct {
    #[trace_call(extra)]
    fn new(name: String, location: Location) -> Self {
        Self {
            name,
            location,
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
                *location,
                name.clone(),
                f.l,
            )),
            None => {
                let typ = &field.type_def.typ;
                self.fields.insert(
                    name.to_string(),
                    TypeLoc::new(*location, typ.clone()),
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
    is_mutable: bool,
}

impl Variable {
    #[trace_call(extra)]
    fn new(name: String, location: Location, typ: Type, is_mutable: bool) -> Self {
        Self {
            name,
            location,
            typ,
            is_mutable
        }
    }
    #[trace_call(extra)]
    fn is_struct_instance(&self) -> bool {
        matches!(&self.typ, Type::Struct(..))
    }
    #[trace_call(extra)]
    fn get_struct_name(&self) -> String {
        match &self.typ {
            Type::Struct(name) => name.clone(),
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker<'flags> {
    known_externs: HashMap<String, Function>,
    known_structs: HashMap<String, Struct>,
    known_functions: HashMap<String, Function>,
    known_variables: VecDeque<HashMap<String, Variable>>,
    #[cfg(not(feature = "llvm"))]
    current_stack_size: usize,
    errors: Vec<TypeError>,
    flags: &'flags Flags,
}

impl<'flags> TypeChecker<'flags> {
    #[trace_call(extra)]
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            known_externs: HashMap::new(),
            known_structs: HashMap::new(),
            known_functions: HashMap::new(),
            known_variables: VecDeque::new(),
            #[cfg(not(feature = "llvm"))]
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
                *location,
                name.clone(),
                external.location,
            ));
        }
        match self.known_functions.get(name) {
            Some(f) => self.report_error(TypeError::Redeclaration(
                "Function",
                *location,
                name.clone(),
                f.location,
            )),
            None => {
                let return_type = &function.return_type.typ;
                let return_loc = &function.return_type.location;
                let (parameters, errors) = check_parameters!(self, function);
                let func = Function {
                    location: *location,
                    return_type: TypeLoc::new(*return_loc, return_type.clone()),
                    parameters,
                    has_this: false,
                };
                self.known_functions.insert(name.clone(), func);
                for error in errors {
                    self.report_error(error);
                }
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
                *location,
                name.clone(),
                f.location,
            )),
            None => {
                let return_type = &extern_node.return_type.typ;
                let return_loc = &extern_node.return_type.location;
                let (parameters, errors) = check_parameters!(self, extern_node);
                let func = Function {
                    location: *location,
                    return_type: TypeLoc::new(*return_loc, return_type.clone()),
                    parameters,
                    has_this: false,
                };
                self.known_externs.insert(name.clone(), func);
                for error in errors {
                    self.report_error(error);
                }
            }
        }
    }

    #[trace_call(extra)]
    fn add_method_to_struct(&mut self, method: &nodes::MethodNode, strukt: &mut Struct) {
        let name = &method.name;
        let location = &method.location;
        if strukt.known_methods.get(name).is_some() {
            self.report_error(TypeError::Redeclaration(
                "Method",
                *location,
                name.clone(),
                strukt.location,
            ));
        } else {
            let return_type = &method.return_type.typ;
            let return_loc = &method.return_type.location;
            let (parameters, errors) = check_parameters!(self, method);
            let method = Function {
                location: *location,
                return_type: TypeLoc::new(*return_loc, return_type.clone()),
                has_this: parameters.iter().find(|p| p.name == "this").is_some(),
                parameters,
            };
            strukt.known_methods.insert(name.clone(), method);
            for error in errors {
                self.report_error(error);
            }
        }
    }

    #[trace_call(extra)]
    fn add_constructor_to_struct(
        &mut self,
        constructor: &nodes::ConstructorNode,
        strukt: &mut Struct
    ) {
        let location = &constructor.location;
        if strukt.known_constructors.len() > 0 {
            self.report_error(TypeError::Redeclaration(
                "Constructor",
                *location,
                strukt.name.to_string(),
                strukt.location,
            ));
        } else {
            let return_type = &constructor.return_type.typ;
            let return_loc = &constructor.return_type.location;
            let (parameters, errors) = check_parameters!(self, constructor);
            let constructor = Function {
                location: *location,
                return_type: TypeLoc::new(*return_loc, return_type.clone()),
                parameters,
                has_this: false,
            };
            strukt.known_constructors.push(constructor);
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
        for s in &ast.structs {
            match self.known_structs.get(&s.name) {
                Some(strukt) => {
                    let err = TypeError::Redeclaration(
                        "Struct",
                        s.location,
                        s.name.clone(),
                        strukt.location,
                    );
                    self.report_error(err);
                },
                None => {
                    let mut strukt = Struct::new(s.name.clone(), s.location);
                    for field in &s.fields {
                        if let Err(error) = strukt.add_field(field) {
                            self.report_error(error);
                        }
                    }
                    for method in &s.methods {
                        self.add_method_to_struct(method, &mut strukt);
                    }
                    for constructor in &s.constructors {
                        self.add_constructor_to_struct(constructor, &mut strukt);
                    }
                    self.known_structs.insert(s.name.clone(), strukt);
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
    fn find_recursive_structs(&mut self) {
        if self.known_structs.is_empty() {
            return;
        }
        let mut errors = Vec::new();
        let mut queue = VecDeque::new();
        let mut visited = Vec::new();

        let first = self.known_structs.iter().next().unwrap();
        queue.push_back((first.0.clone(), first.1));

        while let Some((name, strukt)) = queue.pop_front() {
            if visited.contains(&name) {
                errors.push(TypeError::RecursiveStruct(
                    strukt.location,
                    strukt.name.clone(),
                    visited.iter().map(|s| {
                        let strukt = self.known_structs.get(s).unwrap();
                        (strukt.location, strukt.name.clone())
                    }).collect(),
                ));
                continue;
            }
            visited.push(name.clone());
            for (_, field) in &strukt.fields {
                if field.t.is_struct() {
                    let field_name = field.t.get_struct_name();
                    if let Some(field_strukt) = self.known_structs.get(&field_name) {
                        queue.push_back((field_name, field_strukt));
                    } else {
                        unreachable!();
                    }
                }
            }
        }
        for error in errors {
            self.report_error(error);
        }
    }

    #[trace_call(always)]
    pub fn type_check_file(&mut self, file: &mut nodes::FileNode) -> Result<(), String> {
        self.fill_lookup(file);
        self.find_recursive_structs();
        for e in &mut file.externs {
            self.type_check_extern(e);
        }
        for s in &mut file.structs {
            self.type_check_struct(s);
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
    fn type_check_extern(&mut self, extern_node: &mut nodes::ExternNode) {
        for p in &mut extern_node.parameters {
            self.type_check_parameter(p);
        }
        self.type_check_type_node(&mut extern_node.return_type);
        #[cfg(not(feature = "llvm"))]
        {
            self.current_stack_size = 0;
        }
    }

    #[trace_call(always)]
    fn type_check_struct(&mut self, struct_node: &mut nodes::StructNode) {
        for field in &mut struct_node.fields {
            self.type_check_field(field);
        }
        for constructor in &mut struct_node.constructors {
            self.type_check_constructor(constructor, &struct_node.name);
        }
        for method in &mut struct_node.methods {
            self.type_check_method(method, &struct_node.name);
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
        struct_name: &str,
    ) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_structs.contains_key(struct_name));
        #[cfg(not(feature = "llvm"))]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut constructor.parameters {
            self.type_check_parameter(param);
        }

        let Some(struct_info) = self.known_structs.get(struct_name) else { unreachable!() };
        let Some(constructor_info) = struct_info.known_constructors.get(0) else { unreachable!() };

        // Parameters are now known variables
        let mut parameters = constructor_info.get_parameters_as_hashmap();
        parameters.insert(
            "this".to_string(),
            Variable::new(
                "this".to_string(),
                constructor.location,
                Type::Struct(struct_name.to_string()),
                true,
            ),
        );

        #[cfg(not(feature = "llvm"))]
        {
            self.current_stack_size += 8; // `this` is always 8 bytes
        }

        self.known_variables.push_back(parameters);
        self.type_check_block(&mut constructor.block);
        #[cfg(not(feature = "llvm"))]
        {
            constructor.stack_size = self.current_stack_size;
            self.current_stack_size = 0;
        }

        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_method(
        &mut self,
        method: &mut nodes::MethodNode,
        struct_name: &str,
    ) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_structs.contains_key(struct_name));
        #[cfg(not(feature = "llvm"))]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut method.parameters {
            self.type_check_parameter(param);
        }

        let Some(struct_info) = self.known_structs.get(struct_name) else {
            // Lookup of structs and methods is done before ever evaluating any methods,
            // so known_structs should always contain the struct
            unreachable!()
        };
        let Some(method_info) = struct_info.known_methods.get(&method.name) else {
            // Lookup of structs and methods is done before ever evaluating any methods,
            // so known_methods should always contain the method
            unreachable!()
        };

        // Parameters are now known variables
        let parameters = method_info.get_parameters_as_hashmap();
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut method.block);
        #[cfg(not(feature = "llvm"))]
        {
            method.stack_size = self.current_stack_size;
            self.current_stack_size = 0;
        }

        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_function(&mut self, function: &mut nodes::FunctionNode) {
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(self.known_functions.contains_key(&function.name));
        #[cfg(not(feature = "llvm"))]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut function.parameters {
            self.type_check_parameter(param);
        }

        let Some(function_info) = self.known_functions.get(&function.name) else { unreachable!() };

        // Parameters are now known variables
        let parameters = function_info.get_parameters_as_hashmap();
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut function.block);
        #[cfg(not(feature = "llvm"))]
        {
            function.stack_size = self.current_stack_size;
            self.current_stack_size = 0;
        }

        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_parameter(&mut self, parameter: &mut nodes::ParameterNode) {
        self.type_check_type_node(&parameter.typ);
        debug_assert!(parameter.typ.typ != Type::Unknown);
        #[cfg(not(feature = "llvm"))]
        {
            let var_size = parameter.typ.typ.size();
            self.current_stack_size += var_size;
        }
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
            nodes::Statement::VarDecl(var_node) => self.type_check_stmt_var_decl(var_node),
            nodes::Statement::If(if_node) => self.type_check_stmt_if(if_node),
            nodes::Statement::Return(return_node) => self.type_check_stmt_return(return_node),
            nodes::Statement::While(while_node) => self.type_check_stmt_while(while_node),
            nodes::Statement::Break(break_node) => self.type_check_stmt_break(break_node),
            nodes::Statement::Continue(continue_node) => {
                self.type_check_stmt_continue(continue_node)
            }
            nodes::Statement::Expression(expression_node) => {
                self.type_check_expression(expression_node, false);
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_var_decl(&mut self, let_node: &mut nodes::VarDeclNode) {
        match self.get_variable_in_current_scope(&let_node.name) {
            Some(var) => {
                self.report_error(TypeError::Redeclaration(
                    "Variable",
                    let_node.location,
                    let_node.name.clone(),
                    var.location,
                ));
            },
            None => {
                self.type_check_type_node(&let_node.typ);
                #[cfg(not(feature = "llvm"))]
                {
                    let var_size = let_node.typ.typ.size();
                    self.current_stack_size += var_size;
                }

                let var = Variable {
                    name: let_node.name.clone(),
                    location: let_node.location,
                    typ: let_node.typ.typ.clone(),
                    is_mutable: let_node.is_mutable,
                };
                let expected_type = &let_node.typ.typ;

                let expr_type = self.type_check_expression(&mut let_node.expression, false);
                let current_scope = self.get_current_scope();
                if current_scope.insert(let_node.name.clone(), var).is_some() {
                    internal_panic!(format!("Variable `{}` already exists in current scope", let_node.name));
                }
                if expr_type == Type::None {
                    // Error in expression, we can't continue
                    return;
                }

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
        let cond = self.type_check_expression(&mut if_node.condition, false);
        if cond != Type::None && cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                if_node.condition.get_loc(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_block(&mut if_node.if_body);
        if let Some(else_branch) = &mut if_node.else_body {
            self.type_check_block(else_branch);
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_return(
        &mut self,
        return_node: &mut nodes::ReturnNode,
    ) {
        debug_assert!(return_node.typ == Type::Unknown);

        let (expected_return_type, location) = if return_node.strukt.is_none() {
            // We're returning from a normal function
            let Some(function) = self.known_functions.get(&return_node.function) else { unreachable!() };
            let expected_return_type = function.return_type.t.clone();
            let location = function.location;
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(strukt) = self.known_structs.get(return_node.strukt.as_ref().unwrap()) else { unreachable!() };
            let (mut location, mut expected_return_type) = (Location::anonymous(), Type::Unknown);
            if let Some(method) = strukt.known_methods.get(&return_node.function) {
                expected_return_type = method.return_type.t.clone();
                location = method.location;
            }
            debug_assert!(expected_return_type != Type::Unknown);
            debug_assert!(location != Location::anonymous());
            (expected_return_type, location)
        };

        if let Some(ref mut ret_expr) = &mut return_node.return_value {
            if expected_return_type == Type::None {
                // Found expression but expected none, makes no sense
                self.report_error(TypeError::WrongReturnType(
                    return_node.location,
                    Type::None,
                    location,
                    expected_return_type.clone(),
                ));
            }
            let expr_type = self.type_check_expression(ret_expr, false);
            if expr_type == Type::None {
                // Error in expression, we can't continue
                return;
            }
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
                    return_node.location,
                    expr_type,
                    location,
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
                return_node.location,
                location,
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
        let cond = self.type_check_expression(&mut while_node.condition, false);
        if cond != Type::None && cond != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                while_node.condition.get_loc(),
                Type::Bool,
                cond,
            ));
        }
        self.type_check_block(&mut while_node.body);
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
        needs_mutability: bool,
    ) -> Type {
        match expression {
            nodes::Expression::Name(name_node) => self.type_check_expr_name(name_node, needs_mutability),
            nodes::Expression::Unary(unary_expr) => self.type_check_expr_unary(unary_expr),
            nodes::Expression::Binary(binary_expr) => self.type_check_expr_binary(binary_expr, needs_mutability),
            nodes::Expression::FunctionCall(func_call) => {
                self.type_check_expr_function_call(func_call, needs_mutability)
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
                        lit_node.location,
                        typ.clone(),
                        lit_node.typ.clone(),
                    ));
                } else if let Type::Arr(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "array",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                } else if let Type::Struct(_) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "struct instance",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                } else if *typ == Type::Bool {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "boolean",
                        lit_node.location,
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
                        name_node.location,
                        typ.clone(),
                        name_node.typ.clone(),
                    ));
                }
                typ.clone()
            }
            nodes::Expression::Unary(unary_node) => {
                match unary_node.operation {
                    Operation::Negate => {
                        let expr_type = self.type_check_expression_with_type(
                            &mut unary_node.expression,
                            typ,
                        );
                        if expr_type != Type::I32 && expr_type != Type::I64 {
                            self.report_error(TypeError::NegationTypeMismatch(
                                unary_node.location,
                                typ.clone(),
                            ));
                        } else if expr_type == Type::Unknown {
                            unary_node.typ = typ.clone();
                        } else if expr_type != *typ {
                            self.report_error(TypeError::TypeMismatch(
                                unary_node.location,
                                typ.clone(),
                                expr_type.clone(),
                            ));
                        } else {
                            unary_node.typ = typ.clone();
                        }
                    }
                    _ => internal_panic!(format!(
                        "type_check_expression_with_type for {:?} is not implemented yet!",
                        unary_node.operation
                    )),
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
    fn type_check_expr_name(&mut self, name_node: &mut nodes::NameNode, needs_mutability: bool) -> Type {
        let var = self.get_variable(&name_node.name);
        match var {
            Some(var) => {
                if needs_mutability && !var.is_mutable {
                    self.report_error(TypeError::ImmutableModification(
                        name_node.location,
                        name_node.name.clone(),
                        var.location,
                    ));
                }
                name_node.typ = var.typ.clone();
                var.typ
            }
            None => {
                self.report_error(TypeError::UndeclaredVariable(
                    name_node.location,
                    name_node.name.clone(),
                ));
                Type::None
            },
        }
    }

    #[trace_call(always)]
    fn type_check_expr_unary(
        &mut self,
        unary_expr: &mut nodes::UnaryNode
    ) -> Type {
        check_or_abort!(expr_type, self.type_check_expression(&mut unary_expr.expression, false));
        match unary_expr.operation {
            Operation::Negate => {
                if expr_type == Type::Unknown {
                    return Type::Unknown;
                } else if expr_type != Type::I32 && expr_type != Type::I64 {
                    self.report_error(TypeError::NegationTypeMismatch(
                        unary_expr.location,
                        expr_type.clone(),
                    ));
                }
                unary_expr.typ = expr_type.clone();
                expr_type
            }
            _ => {
                internal_panic!(format!(
                    "type_check_expr_unary for {:?} is not implemented yet!",
                    unary_expr.operation
                ))
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
        needs_mutability: bool,
    ) -> Type {
        match binary_expr.operation {
            Operation::Dot => self.type_check_expr_dot(binary_expr, needs_mutability),
            Operation::Assign => self.type_check_expr_assign(binary_expr),
            _ if binary_expr.is_comparison() => self.type_check_expr_binary_comparison(binary_expr),
            _ if binary_expr.is_arithmetic() => self.type_check_expr_binary_arithmetic(binary_expr),
            _ if binary_expr.is_bitwise() => self.type_check_expr_binary_bitwise(binary_expr),
            o => todo!("type_check_expr_binary for {:?} is not implemented yet!", o),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_bitwise(
        &mut self,
        binary_expr: &mut nodes::BinaryNode
    ) -> Type {
        check_or_abort!(
            lhs_type, self.type_check_expression(&mut binary_expr.lhs, false),
            rhs_type, self.type_check_expression(&mut binary_expr.rhs, false)
        );
        assert!(binary_expr.is_bitwise());
        match (&lhs_type, &rhs_type) {
            (Type::Struct(..), _) | (_, Type::Struct(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::None
            }
            (Type::Arr(..), _) | (_, Type::Arr(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::None
            }
            (Type::Bool, _) | (_, Type::Bool) => {
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::None
            }
            (Type::Unknown, Type::Unknown) => Type::Unknown,
            (Type::Unknown, other) => {
                let typ = self.type_check_expression_with_type(&mut binary_expr.lhs, other);
                binary_expr.typ = typ.clone();
                typ
            }
            (other, Type::Unknown) => {
                let typ = self.type_check_expression_with_type(&mut binary_expr.rhs, other);
                binary_expr.typ = typ.clone();
                typ
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        binary_expr.location,
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
    fn type_check_expr_binary_arithmetic(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
    ) -> Type {
        check_or_abort!(
            lhs_type, self.type_check_expression(&mut binary_expr.lhs, false),
            rhs_type, self.type_check_expression(&mut binary_expr.rhs, false)
        );
        assert!(binary_expr.is_arithmetic());
        match (&lhs_type, &rhs_type) {
            (typ @ Type::Struct(..), _) | (_, typ @ Type::Struct(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
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
                    binary_expr.location,
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
                    binary_expr.location,
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
                        binary_expr.location,
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
        check_or_abort!(
            lhs_type, self.type_check_expression(&mut binary_expr.lhs, false),
            rhs_type, self.type_check_expression(&mut binary_expr.rhs, false)
        );
        assert!(binary_expr.is_comparison());
        match (&lhs_type, &rhs_type) {
            (Type::Struct(..), _) | (_, Type::Struct(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
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
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Type::Bool
            }
            (Type::Unknown, Type::Unknown) => {
                // FIXME: Better error handling, is it always a good idea to infer i64?
                let force_type = Type::I64;
                self.type_check_expression_with_type(&mut binary_expr.lhs, &force_type);
                self.type_check_expression_with_type(&mut binary_expr.rhs, &force_type);
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                typ
            },
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
                        binary_expr.location,
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

        check_or_abort!(lhs_type, self.type_check_expression(&mut assign_expr.lhs, true));
        check_or_abort!(rhs_type, self.type_check_expression(&mut assign_expr.rhs, false));

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
                assign_expr.location,
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
        needs_mutability: bool,
    ) -> Type {
        check_or_abort!(lhs_type, self.type_check_expression(&mut binary_expr.lhs, needs_mutability));
        if let Type::Struct(struct_name) = lhs_type {
            let Some(strukt) = self.known_structs.get(&struct_name) else {
                self.report_error(TypeError::DotOnNonStruct(
                    binary_expr.lhs.get_loc(),
                ));
                return Type::None;
            };
            match &mut (*binary_expr.rhs) {
                nodes::Expression::Name(name_node) => {
                    if let Some(field) = strukt.get_field(&name_node.name) {
                        binary_expr.typ = field.t.clone();
                        name_node.typ = field.t.clone();
                        field.t.clone()
                    } else {
                        self.report_error(TypeError::UnknownField(
                            name_node.location,
                            name_node.name.clone(),
                            strukt.location,
                            strukt.name.clone(),
                        ));
                        Type::None
                    }
                }
                nodes::Expression::FunctionCall(call_node) => {
                    // FIXME: Error Log shows wrong location
                    if let Some(method) = strukt.get_method(&call_node.function_name) {
                        let result = if method.has_this {
                            // FIXME: This is not a good solution, but it works for now
                            call_node.arguments.insert(
                                0,
                                *binary_expr.lhs.clone(),
                            );
                            let result = check_function!(self, call_node, method, "Method");
                            call_node.arguments.remove(0);
                            result
                        } else {
                            check_function!(self, call_node, method, "Method")
                        };
                        binary_expr.typ = result.clone();
                        result
                    } else {
                        self.report_error(TypeError::UnknownMethod(
                            call_node.location,
                            call_node.function_name.clone(),
                            strukt.location,
                            strukt.name.clone(),
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
            self.report_error(TypeError::DotOnNonStruct(
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
            check_or_abort!(elem_type, self.type_check_expression(elem, false));
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
                access.location,
                access.array_name.clone(),
            ));
            return Type::None;
        };
        let Type::Arr(elem, arr_size) = &var.typ else {
            self.report_error(TypeError::IndexIntoNonArray(
                access.location,
                access.array_name.clone(),
                var.location,
            ));
            return Type::None;
        };
        if arr_size.len() != access.indices.elements.len() {
            self.report_error(TypeError::DimensionMismatch(
                access.location,
                arr_size.len(),
                access.indices.elements.len(),
            ));
        }
        check_or_abort!(t, self.type_check_expr_array_literal(&mut access.indices));
        if let Type::Arr(elem_index, _) = t {
            if *elem_index != Type::Usize {
                self.report_error(TypeError::TypeMismatch(
                    access.indices.location,
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
        needs_mutability: bool,
    ) -> Type {
        if needs_mutability {
            self.report_error(TypeError::CantMutateTemporary(
                func_call.location,
            ));
        }
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
            if let Some(strukt) = self.known_structs.get(&func_call.function_name) {
                let Some(constructor) = strukt.known_constructors.get(0) else {
                    self.report_error(TypeError::NoConstructor(
                        func_call.location,
                        strukt.location,
                        strukt.name.clone(),
                    ));
                    return Type::None;
                };
                debug_assert!(
                    strukt.known_constructors.len() > 0,
                    "Struct has constructor feature, but has_constructor is false"
                );
                constructor
            } else {
                self.report_error(TypeError::UndeclaredStruct(
                    func_call.location,
                    func_call.function_name.clone(),
                ));
                return Type::None;
            }
        } else {
            let Some(function) = self.known_functions.get(&func_call.function_name) else {
                self.report_error(TypeError::UndeclaredFunction(
                    func_call.location,
                    func_call.function_name.clone(),
                ));
                return Type::None;
            };
            function
        };
        let return_type = function.return_type.clone();
        if let Type::Struct(struct_name) = &return_type.t {
            if !self.known_structs.contains_key(struct_name) {
                self.report_error(TypeError::UndeclaredStruct(
                    func_call.location,
                    struct_name.clone(),
                ));
                self.report_error(TypeError::UnknownType(
                    return_type.l.clone(),
                    return_type.t.clone(),
                ));
                return Type::None;
            }
        }
        check_function!(self, func_call, function, "Function")
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
        if let Type::Struct(struct_name) = &type_node.typ {
            if !self.known_structs.contains_key(struct_name) {
                self.report_error(TypeError::UnknownType(
                    type_node.location,
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
//         } else if let Type::Struct(struct_name) = typ {
//             Err(format!(
//                 "{}: {:?} Type Mismatch! Expected instance of struct `{}`, found Integer Literal `{}`",
//                 ERR_STR,
//                 self.location,
//                 struct_name,
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
