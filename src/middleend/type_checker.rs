use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::{Location, Operation};

use crate::compiler::{ERR_STR, NOTE_STR, WARN_STR};
use crate::internal_panic;
use crate::util::flags::Flags;

use tracer::trace_call;
#[allow(unused)]
use tracer::trace_panic;

type MutStateVal = u8;

#[allow(non_upper_case_globals, non_snake_case)]
mod MutState {
    use super::MutStateVal;

    pub const Immut: MutStateVal = 0b0;
    pub const MutVar: MutStateVal = 0b1;
    pub const MutRef: MutStateVal = MutVar << 1;

    pub fn mutable(var_mut: bool, ref_mut: bool) -> MutStateVal {
        let mut val = Immut;
        if var_mut { val |= MutVar; }
        if ref_mut { val |= MutRef; }
        val
    }
}

macro_rules! check_function {
    ($tc:ident, $call_node:ident, $func_info:ident, $typ:expr) => {{
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
                return Ok(return_type);
            }
            std::cmp::Ordering::Greater => {
                if !$func_info.is_vararg {
                    $tc.report_error(TypeError::TooManyArguments(
                        $typ,
                        $call_node.location,
                        $call_node.function_name.clone(),
                        $call_node.arguments.len(),
                        $func_info.location,
                        $func_info.parameters.len(),
                    ));
                    return Ok(return_type);
                }
            }
            std::cmp::Ordering::Equal => (),
        }
        if $func_info.is_vararg {
            let params = $func_info.parameters.clone();
            for i in 0..params.len() {
                let mut arg = &mut $call_node.arguments[i];
                let param = &params[i];
                let expected = param.typ.clone();
                let Ok(arg_type) = $tc.type_check_expression(&mut arg, param.mut_state) else {
                    continue;
                };
                if arg_type == Type::Unknown {
                    // We need to `infer` the type again
                    $tc.type_check_expression_with_type(&mut arg, &expected)?;
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
            for i in params.len()..$call_node.arguments.len() {
                let mut arg = &mut $call_node.arguments[i];
                let Ok(arg_type) = $tc.type_check_expression(&mut arg, MutState::Immut) else {
                    continue;
                };
                if arg_type == Type::Unknown {
                    $tc.type_check_expression_with_type(&mut arg, &Type::I32)?;
                }
                // TODO: https://www.gnu.org/software/libc/manual/html_node/Calling-Variadics.html
                //       https://en.cppreference.com/w/c/language/variadic
                //       https://en.cppreference.com/w/c/language/conversion#Default_argument_promotions
                // match arg_type {
                //     Type::Unknown => {
                //         $tc.type_check_expression_with_type(&mut arg, &Type::I32)?;
                //     }
                //     Type::I8 | Type::U8 | Type::I16 | Type::U16 => arg.set_type(Type::I32),
                //     Type::F32 => arg.set_type(Type::F64),
                //     _ => (),
                // }
            }
        } else {
            for (mut arg, param) in $call_node
                .arguments
                .iter_mut()
                .zip($func_info.parameters.clone())
            {
                let expected = param.typ;
                let Ok(arg_type) = $tc.type_check_expression(arg, MutState::Immut) else {
                    continue;
                };
                if arg_type == Type::Unknown {
                    // We need to `infer` the type again
                    $tc.type_check_expression_with_type(&mut arg, &expected)?;
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
        }

        $call_node.typ = return_type;
        Ok($call_node.typ.clone())
    }};
}

macro_rules! check_parameters {
    ($tc:ident, $function:ident) => {{
        let mut parameters: Vec<Variable> = Vec::new();
        let mut errors: Vec<TypeError> = Vec::new();
        for param in &$function.parameters {
            let p = Variable {
                name: param.name.clone(),
                location: param.location,
                typ: $tc.unwrap_type_node_typ(&param.typ),
                mut_state: MutState::mutable(param.is_mutable, param.typ.typ.is_mutable_ref()),
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
        #[cfg(feature = "old_codegen")]
        if parameters.len() > 4 {
            errors.push(TypeError::TooManyParameters("Method", $function.location));
        }
        (parameters, errors)
    }};
}

#[derive(Debug)]
enum TypeError {
    /// Syntax: Decl Type, Error Loc, Name, Decl Loc
    Redeclaration(&'static str, Location, String, Location),
    /// Syntax: Error Loc, Fn Name, Extern Loc
    ExternFunction(Location, String, Location),
    /// Syntax: Error Loc, Type Name
    UnknownType(Location, Type),
    /// Syntax: Fn Type, Error Loc
    #[cfg(feature = "old_codegen")]
    TooManyParameters(&'static str, Location),
    /// Syntax: Error Loc, Expected, Found
    TypeMismatch(Location, Type, Type),
    /// Syntax: Error Loc, Binary Op, LHS Loc, LHS Type, RHS Loc, RHS Type
    BinaryTypeMismatch(Location, Operation, Location, Type, Location, Type),
    /// Syntax: Error Loc, Var Name
    UndeclaredVariable(Location, String),
    /// Syntax: Error Loc, Fn Name
    UndeclaredFunction(Location, String),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    NotEnoughArguments(&'static str, Location, String, usize, Location, usize),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    TooManyArguments(&'static str, Location, String, usize, Location, usize),
    /// Syntax: Arg Decl, Arg Type, Param Decl, Param Name, Param Type
    ArgParamTypeMismatch(Location, Type, Location, String, Type),
    /// Syntax: Error Loc, Found Type, Decl Loc, Decl Type
    WrongReturnType(Location, Type, Location, Type),
    /// Syntax: Error Loc, Decl Loc, Decl Type
    MissingReturn(Location, Location, Type),
    /// Syntax: Error Loc, Var Decl, Var Name, Field Name, Struct Loc, Struct Name
    UnknownField(Location, String, Location, String),
    /// Syntax: Error Loc, Var Name, Struct Loc, Struct Name
    UnknownMethod(Location, String, Location, String),
    /// Syntax: Expected, Error Loc, Found Literal
    UnexpectedLiteral(&'static str, Location, String),
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
    /// Syntax: Error Loc, Field Name, Struct Loc, Struct Name
    MissingField(Location, String, Location, String),
    /// Syntax: Error Loc, Type, Type
    DereferenceTypeMismatch(Location, Type),
    /// Syntax: Error Loc
    DereferenceIntegerLiteral(Location),
    /// Syntax: Error Loc
    NestedReferenceNotAllowedYet(Location),
    /// Syntax: Kind, Error Loc, Function Name, Function Loc
    UnsafeCallInSafeContext(&'static str, Location, String, Location),
    /// Syntax: Error Loc, Module Name
    UnknownModule(Location, String),
    /// Syntax: Error Loc, Module Specifier
    UnknownModuleSpecifier(Location, nodes::ModuleSpecifier),
    /// Syntax: Error Loc
    UnsafeAny(Location),
    /// Syntax: Error Loc, Element Type, Inferred Loc, Inferred Type
    ArrayLiteralElementTypeMismatch(Location, Type, Location, Type),
    /// Syntax: Error Loc, Expected Size, Found Size
    ArraySizeMismatch(Location, usize, usize),
    /// Syntax: Error Loc, Type
    InvalidIndexedAccess(Location, Type),
    /// Syntax: Error Loc, Type
    ArrayIndexRequiresUsize(Location, Type),
    /// Syntax: Error Loc, Type
    LogicalNotTypeMismatch(Location, Type),
    /// Syntax: Error Loc, Message
    InvalidMemberAccess(Location, &'static str),
    /// Syntax: Error Loc
    ImmutDerefInMutContext(Location, Location),
    /// Syntax: Error Loc
    UnsafePointerArithmetics(Location),
    /// Syntax: Error Loc, Binary Op
    InvalidPointerArithmetics(Location, Operation)
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
                    "{}: {:?}: Function redeclaration.\n{}: {:?}: Extern function `{}` already declared here.",
                    ERR_STR, error_loc, NOTE_STR, fn_loc, fn_name
                )
            }
            TypeError::UnknownType(loc, name) => {
                write!(f, "{}: {:?}: Unknown type `{}`.", ERR_STR, loc, name)
            }
            #[cfg(feature = "old_codegen")]
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
            TypeError::NotEnoughArguments(
                fn_kind,
                error_loc,
                fn_name,
                arg_count,
                fn_loc,
                param_count,
            ) => {
                write!(
                    f,
                    "{}: {:?}: Not enough arguments for {} `{}`.\n{}: {:?}: {} `{}` expects {} arguments, found {}.",
                    ERR_STR, error_loc, fn_kind, fn_name, NOTE_STR, fn_loc, fn_kind, fn_name, param_count, arg_count
                )
            }
            TypeError::TooManyArguments(
                fn_kind,
                error_loc,
                fn_name,
                arg_count,
                fn_loc,
                param_count,
            ) => {
                write!(
                    f,
                    "{}: {:?}: Too many arguments for {} `{}`.\n{}: {:?}: {} `{}` expects {} arguments, found {}.",
                    ERR_STR, error_loc, fn_kind, fn_name, NOTE_STR, fn_loc, fn_kind, fn_name, param_count, arg_count
                )
            }
            TypeError::ArgParamTypeMismatch(
                arg_loc,
                arg_type,
                param_loc,
                param_name,
                param_type,
            ) => {
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
                    "{}: {:?}: Recursive struct `{}`.",
                    ERR_STR, error_loc, struct_name
                );
                for (loc, name) in struct_locs.iter().skip(1) {
                    message.push_str(&format!(
                        "\n{}: {:?}: Chain of recursion also includes struct `{}`.",
                        NOTE_STR, loc, name
                    ));
                }
                write!(f, "{}", message)
            }
            TypeError::MissingField(error_loc, field_name, struct_loc, struct_name) => {
                write!(
                    f,
                    "{}: {:?}: Missing field `{}` in instantiation of struct `{}`.\n{}: {:?}: Struct `{}` is declared here.",
                    ERR_STR, error_loc, field_name, struct_name, NOTE_STR, struct_loc, struct_name
                )
            }
            TypeError::DereferenceTypeMismatch(error_loc, typ) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Dereference is not defined for type `{}`.",
                    ERR_STR, error_loc, typ
                )
            }
            TypeError::DereferenceIntegerLiteral(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to dereference an integer literal.",
                    ERR_STR, error_loc
                )
            }
            TypeError::NestedReferenceNotAllowedYet(error_loc) => {
                write!(
                    f,
                    "{}: {:?}: Nested references are not allowed yet.",
                    ERR_STR, error_loc
                )
            }
            TypeError::UnsafeCallInSafeContext(kind, error_loc, fn_name, fn_loc) => {
                let lowercase_kind = kind.to_lowercase();
                write!(
                    f,
                    "{}: {:?}: Unsafe {} `{}` called in safe context.\n{}: {:?}: {} `{}` is declared here.\n{}: Use an `unsafe {{}}` block to fix this.",
                    ERR_STR, error_loc, lowercase_kind, fn_name, NOTE_STR, fn_loc, kind, fn_name, NOTE_STR
                )
            }
            TypeError::UnknownModule(loc, name) => {
                write!(f, "{}: {:?}: Unknown module `{}`.", ERR_STR, loc, name)
            }
            TypeError::UnknownModuleSpecifier(loc, mod_spec) => {
                write!(f, "{}: {:?}: Unknown module `{}`.", ERR_STR, loc, mod_spec)
            }
            TypeError::UnsafeAny(loc) => {
                write!(f, "{}: {:?}: Use of `Any` is unsafe.\n{}: Use an `unsafe {{}}` block if you really want to use `Any`.", ERR_STR, loc, NOTE_STR)
            }
            TypeError::ArrayLiteralElementTypeMismatch(loc, typ, inferred_loc, inferred_typ) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Expected type `{}`, found type `{}`.\n{}: {:?}: Inferred type for array elements to be `{}` here.",
                    ERR_STR, loc, inferred_typ, typ, NOTE_STR, inferred_loc, inferred_typ
                )
            }
            TypeError::ArraySizeMismatch(loc, expected, found) => {
                write!(
                    f,
                    "{}: {:?}: Array size mismatch! Expected size to be `{}`, found size to be `{}`.",
                    ERR_STR, loc, expected, found
                )
            }
            TypeError::InvalidIndexedAccess(loc, typ) => {
                write!(
                    f,
                    "{}: {:?}: Invalid indexed access! Indexed access is not defined for type `{}`.",
                    ERR_STR, loc, typ
                )
            }
            TypeError::ArrayIndexRequiresUsize(loc, typ) => {
                write!(
                    f,
                    "{}: {:?}: Array index requires type `usize`, found type `{}`.",
                    ERR_STR, loc, typ
                )
            }
            TypeError::LogicalNotTypeMismatch(loc, typ) => {
                write!(
                    f,
                    "{}: {:?}: Type mismatch! Logical not is not defined for type `{}`.",
                    ERR_STR, loc, typ
                )
            }
            TypeError::InvalidMemberAccess(loc, msg) => {
                write!(f, "{}: {:?}: {} is not allowed.", ERR_STR, loc, msg)
            }
            TypeError::ImmutDerefInMutContext(loc, sub_expr_loc) => {
                write!(
                    f,
                    "{}: {:?}: Attempted to dereference immutable reference where mutability is required.\n{}: {:?}: Subexpression is not mutable.",
                    ERR_STR, loc, NOTE_STR, sub_expr_loc
                )
            }
            TypeError::UnsafePointerArithmetics(loc) => {
                write!(f, "{}: {:?}: Pointer Arithmetics are unsafe.\n{}: Use an `unsafe {{}}` block if you really want to do that.", ERR_STR, loc, NOTE_STR)
            }
            TypeError::InvalidPointerArithmetics(loc, op) => {
                write!(f, "{}: {:?}: Operation `{}` is not allowed in the context of pointer arithmetics.", ERR_STR, loc, op)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Type {
    None, // For functions that return nothing
    Any,  // C's void*, expected to be used for FFI, not for general use, infers to all references
    #[default]
    Unknown,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Usize,
    Bool,
    Char,
    // Ptr(Box<Type>),
    Struct(String, Option<Box<nodes::ModuleSpecifier>>),
    // TODO: More unit tests for references
    Ref(Box<Type>, bool), // bool is mutability
    Array(Box<Type>, usize),
    Str,
    // Reserved for later use
    F32,
    F64,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::I8, Type::I8) => true,
            (Type::I16, Type::I16) => true,
            (Type::I32, Type::I32) => true,
            (Type::I64, Type::I64) => true,
            (Type::U8, Type::U8) => true,
            (Type::U16, Type::U16) => true,
            (Type::U32, Type::U32) => true,
            (Type::U64, Type::U64) => true,
            (Type::Usize, Type::Usize) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::Str, Type::Str) => true,
            (Type::F32, Type::F32) => true,
            (Type::F64, Type::F64) => true,
            (Type::Unknown, Type::Unknown) => true,
            (Type::None, Type::None) => true,
            (Type::Any, Type::Any) => true,
            (Type::Unknown, _) | (_, Type::Unknown) => false,
            (Type::None, _) | (_, Type::None) => false,
            (Type::Any, Type::Ref(_, _)) | (Type::Ref(_, _), Type::Any) => true, // Any is void*, so it can be inferred to any reference
            (Type::Any, _) | (_, Type::Any) => false,
            (Type::Struct(lhs, lhs_mod), Type::Struct(rhs, rhs_mod)) => {
                lhs == rhs && lhs_mod == rhs_mod
            },
            (Type::Ref(lhs, l), Type::Ref(rhs, r)) => lhs == rhs && l == r,
            (Type::Array(lhs, l), Type::Array(rhs, r)) => lhs == rhs && l == r,
            _ => false,
        }
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Type {
    #[trace_call(extra)]
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
                | Type::Bool
                | Type::Char
                | Type::F32
                | Type::F64
                | Type::Str
                | Type::Any
        )
    }

    #[trace_call(extra)]
    pub fn is_compound(&self) -> bool {
        self.is_struct() || self.is_array() || self.is_struct_ref()
    }

    #[trace_call(extra)]
    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(..))
    }

    #[trace_call(extra)]
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(..))
    }

    #[trace_call(extra)]
    pub fn is_struct_ref(&self) -> bool {
        match self {
            Type::Ref(t, _) => match **t {
                Type::Struct(..) => true,
                Type::Ref(..) => t.is_struct_ref(),
                _ => false,
            },
            _ => false,
        }
    }

    #[trace_call(extra)]
    pub fn is_mutable_ref(&self) -> bool {
        match self {
            Type::Ref(_, is_mut) => *is_mut,
            _ => false,
        }
    }

    #[trace_call(extra)]
    pub fn get_underlying_type_ref(&self) -> &Type {
        match self {
            Type::Ref(t, _) => t.get_underlying_type_ref(),
            Type::Array(t, _) => t.get_underlying_type_ref(),
            _ => self,
        }
    }

    pub fn get_underlying_type_ref_mut(&mut self) -> &mut Type {
        match self {
            Type::Ref(ref mut t, _) => t.get_underlying_type_ref_mut(),
            Type::Array(ref mut t, _) => t.get_underlying_type_ref_mut(),
            _ => self,
        }
    }

    #[trace_call(extra)]
    pub fn get_underlying_type(&self) -> Type {
        match self {
            Type::Ref(t, _) => t.get_underlying_type(),
            Type::Array(t, _) => t.get_underlying_type(),
            _ => self.clone(),
        }
    }

    #[trace_call(extra)]
    pub fn get_struct_name(&self) -> String {
        match self {
            Type::Struct(struct_name, mod_path) => {
                if let Some(path) = &mod_path {
                    format!("{}.{}", path.to_codegen_name(), struct_name)
                } else {
                    struct_name.clone()
                }
            }
            _ => internal_panic!("Expected Struct")
        }
    }
}

impl Display for Type {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Struct(str, module_spec) => {
                if let Some(module_spec) = module_spec {
                    write!(fmt, "{}::{}", module_spec, str)
                } else {
                    write!(fmt, "{}", str)
                }
            },
            Type::Ref(t, true) => write!(fmt, "&mut {}", t),
            Type::Ref(t, false) => write!(fmt, "&{}", t),
            Type::Array(t, len) => write!(fmt, "[{}; {}]", t, len),
            Type::Any => write!(fmt, "Any"),
            _ => write!(fmt, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
struct Function {
    location: Location,
    return_type: TypeLoc,
    parameters: Vec<Variable>,
    module_path: Option<Box<nodes::ModuleSpecifier>>,
    has_this: bool,
    is_unsafe: bool,
    is_vararg: bool,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: String,
    location: Location,
    fields: HashMap<String, TypeLoc>,
    known_methods: HashMap<String, Function>,
}

impl Struct {
    #[trace_call(extra)]
    fn new(name: String, location: Location) -> Self {
        Self {
            name,
            location,
            fields: HashMap::new(),
            known_methods: HashMap::new(),
        }
    }

    #[trace_call(extra)]
    fn add_field(&mut self, tc: &TypeChecker, field: &nodes::FieldNode) -> Result<(), TypeError> {
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
                let typ = tc.unwrap_type_node_typ(&field.type_def);
                self.fields.insert(name.to_string(), TypeLoc::new(*location, typ.clone()));
                Ok(())
            }
        }
    }

    #[trace_call(extra)]
    fn add_method(&mut self, tc: &TypeChecker, method: &nodes::MethodNode) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        let name = &method.name;
        let location = &method.location;
        let return_loc = method.return_type.location;
        let (parameters, mut param_errors) = check_parameters!(tc, method);
        errors.append(&mut param_errors);
        let func = Function {
            location: method.location,
            module_path: method.module_path.clone(),
            return_type: TypeLoc::new(return_loc, tc.unwrap_type_node_typ(&method.return_type)),
            parameters,
            has_this: true,
            is_unsafe: method.is_unsafe,
            is_vararg: false,
        };
        if self.known_methods.contains_key(name) {
            let m = &self.known_methods[name];
            errors.push(TypeError::Redeclaration(
                "Method",
                *location,
                name.clone(),
                m.location,
            ));
        } else {
            self.known_methods.insert(name.clone(), func);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
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

#[derive(Debug, Clone, PartialEq)]
struct StructInfo {
    location: Location,
    fields: BTreeMap<String, (Location, String)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    location: Location,
    typ: Type,
    mut_state: MutStateVal,
}

impl Variable {
    #[trace_call(extra)]
    fn new(name: String, location: Location, typ: Type, mut_state: MutStateVal) -> Self {
        Self {
            name,
            location,
            typ,
            mut_state,
        }
    }
    #[trace_call(extra)]
    fn is_struct_instance(&self) -> bool {
        matches!(&self.typ, Type::Struct(..))
    }
}

#[derive(Debug, Clone)]
struct Module {
    name: String,
    modules: HashMap<String, Module>,
    externs: HashMap<String, Function>,
    struct_indices: HashMap<String, usize>,
    structs: Vec<Struct>,
    functions: HashMap<String, Function>,
}

impl Module {
    #[trace_call(extra)]
    fn new(name: String) -> Self {
        Self {
            name,
            modules: HashMap::new(),
            externs: HashMap::new(),
            struct_indices: HashMap::new(),
            structs: Vec::new(),
            functions: HashMap::new(),
        }
    }

    #[trace_call(always)]
    fn get_sub_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    #[trace_call(always)]
    fn add_module(&mut self, module: Module) {
        self.modules.insert(module.name.clone(), module);
    }

    #[trace_call(extra)]
    fn get_struct(&self, name: &str) -> Option<&Struct> {
        match self.struct_indices.get(name) {
            Some(index) => Some(&self.structs[*index]),
            None => None,
        }
    }

    #[trace_call(extra)]
    fn insert_struct(&mut self, strukt: Struct) {
        let index = self.structs.len();
        self.struct_indices.insert(strukt.name.clone(), index);
        self.structs.push(strukt.clone());

        // FIXME: There should be a better way of doing this
        // This allows us to have static functions
        let mut struct_module = Module::new(strukt.name);
        struct_module.functions = strukt.known_methods;
        // FIXME: By adding the module here, we always have to also specify the filename, e.g
        // if we have file X, and a struct A with A::new(), we need to call X::A::new()
        // A proper refactor would be cool, but is not a priority due to selfhost
        // it gets the job done :sunglasses: :^)
        self.add_module(struct_module);
    }

    #[trace_call(extra)]
    fn has_struct(&self, name: &str) -> bool {
        self.struct_indices.contains_key(name)
    }

    #[trace_call(extra)]
    fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    #[trace_call(extra)]
    fn is_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    #[trace_call(extra)]
    fn is_extern(&self, name: &str) -> bool {
        self.externs.contains_key(name)
    }

    #[trace_call(extra)]
    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    #[trace_call(extra)]
    fn get_extern(&self, name: &str) -> Option<&Function> {
        self.externs.get(name)
    }

    #[trace_call(always)]
    fn add_extern(&mut self, tc: &TypeChecker, extern_node: &nodes::ExternNode) -> Vec<TypeError> {
        let return_type = &extern_node.return_type.typ;
        let return_loc = extern_node.return_type.location;
        let (parameters, errors) = check_parameters!(tc, extern_node);
        let func = Function {
            location: extern_node.location,
            module_path: None,
            return_type: TypeLoc::new(return_loc, return_type.clone()),
            parameters,
            has_this: false,
            is_unsafe: extern_node.is_unsafe,
            is_vararg: extern_node.is_vararg,
        };
        self.externs.insert(extern_node.name.clone(), func);
        errors
    }

    #[trace_call(always)]
    fn add_struct(&mut self, tc: &TypeChecker, struct_node: &nodes::StructNode) -> Vec<TypeError> {
        let mut errors = Vec::new();
        let name = struct_node.name.clone();
        let location = struct_node.location;
        let mut strukt = Struct::new(name.clone(), location);
        for field in &struct_node.fields {
            match strukt.add_field(tc, field) {
                Ok(()) => (),
                Err(e) => errors.push(e),
            }
        }
        for method in &struct_node.methods {
            match strukt.add_method(tc, method) {
                Ok(()) => (),
                Err(e) => errors.extend(e),
            }
        }
        if self.has_struct(&name) {
            let strukt = self.get_struct(&name).unwrap();
            errors.push(TypeError::Redeclaration(
                "Struct",
                location,
                name.clone(),
                strukt.location,
            ));
        } else {
            self.insert_struct(strukt);
        }
        errors
    }

    #[trace_call(always)]
    fn add_function(&mut self, tc: &TypeChecker, function: &nodes::FunctionNode) -> Vec<TypeError> {
        let name = function.name.clone();
        let location = function.location;
        let return_type = &function.return_type;
        let return_loc = function.return_type.location;
        let (parameters, mut errors) = check_parameters!(tc, function);
        let func = Function {
            location,
            module_path: function.module_path.clone(),
            return_type: TypeLoc::new(return_loc, tc.unwrap_type_node_typ(return_type)),
            parameters,
            has_this: false,
            is_unsafe: function.is_unsafe,
            is_vararg: false,
        };
        if self.externs.contains_key(&name) {
            let external = &self.externs[&name];
            errors.push(TypeError::ExternFunction(
                location,
                name.clone(),
                external.location,
            ));
        }
        if self.functions.contains_key(&name) {
            let f = &self.functions[&name];
            errors.push(TypeError::Redeclaration(
                "Function",
                location,
                name.clone(),
                f.location,
            ));
        } else {
            self.functions.insert(name.clone(), func);
        }
        errors
    }
}

#[derive(Debug)]
pub struct TypeChecker<'flags> {
    module_tree: Option<Module>,
    module_stack: Vec<String>,
    import_stack: Vec<Vec<Vec<String>>>,
    known_variables: VecDeque<HashMap<String, Variable>>,
    unsafe_depth: usize,
    #[cfg(feature = "old_codegen")]
    current_stack_size: usize,
    errors: Vec<TypeError>,
    flags: &'flags Flags,
}

impl<'flags> TypeChecker<'flags> {
    #[trace_call(extra)]
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            module_tree: None,
            module_stack: Vec::new(),
            import_stack: Vec::new(),
            known_variables: VecDeque::new(),
            unsafe_depth: 0,
            #[cfg(feature = "old_codegen")]
            current_stack_size: 0,
            errors: Vec::new(),
            flags,
        }
    }

    #[trace_call(always)]
    fn get_full_name(&self, struct_name: &str) -> String {
        let module_name = self.module_stack.join(".");
        format!("{}.{}", module_name, struct_name)
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, project: &nodes::ModuleNode, root: bool) -> Module {
        if !root {
            self.module_stack.push(project.name.clone());
        }
        let mut module = Module::new(project.name.clone());
        for sub_module in &project.modules {
            let m = self.fill_lookup(sub_module, false);
            module.add_module(m);
        }
        for extern_node in &project.externs {
            let extern_errors = module.add_extern(self, extern_node);
            for error in extern_errors {
                self.report_error(error);
            }
        }
        for strukt in &project.structs {
            let struct_errors = module.add_struct(self, strukt);
            for error in struct_errors {
                self.report_error(error);
            }
        }
        for func in &project.functions {
            let func_errors = module.add_function(self, func);
            for error in func_errors {
                self.report_error(error);
            }
        }
        if !root {
            self.module_stack.pop();
        }
        module
    }

    #[trace_call(always)]
    fn report_error(&mut self, error: TypeError) {
        if self.flags.debug {
            println!("[DEBUG] Error: {}", error);
        }
        self.errors.push(error);
    }

    #[trace_call(extra)]
    fn add_scope(&mut self, is_unsafe: bool) {
        self.unsafe_depth += if is_unsafe { 1 } else { 0 };
        self.known_variables.push_back(HashMap::new());
    }

    #[trace_call(extra)]
    fn remove_scope(&mut self, is_unsafe: bool) {
        self.unsafe_depth -= if is_unsafe { 1 } else { 0 };
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

    #[trace_call(extra)]
    fn get_module(&self, name: &str) -> Option<&Module> {
        self.module_tree.as_ref().unwrap().get_sub_module(name)
    }

    #[trace_call(always)]
    fn get_current_module(&self) -> &Module {
        let mut module = self.module_tree.as_ref().unwrap();
        for name in &self.module_stack {
            module = module.get_sub_module(name).unwrap();
        }
        module
    }

    #[trace_call(always)]
    fn resolve_module<'a>(&'a self, trace: &'a[String]) -> Option<(&Module, Vec<String>)> {
        let mut current: &Module = self.module_tree.as_ref().unwrap();
        let mut path = vec![];
        for s in trace {
            if let Some(sub_module) = current.get_sub_module(s) {
                path.push(s.clone());
                current = sub_module;
            } else {
                return None;
            }
        }
        path.reverse();
        Some((current, path))
    }

    #[trace_call(always)]
    fn gather_struct_info(
        &mut self,
        module: &nodes::ModuleNode,
        root_module: bool
    ) -> BTreeMap<String, StructInfo> {
        if !root_module {
            self.module_stack.push(module.name.clone());
        }
        let mut struct_info = BTreeMap::new();
        for modu in &module.modules {
            let info = self.gather_struct_info(modu, false);
            struct_info.extend(info);
        }
        let module = self.get_current_module();
        for strukt in &module.structs {
            let real_name = self.get_full_name(&strukt.name);
            let mut field_types = BTreeMap::new();
            for (name, typ) in &strukt.fields {
                let type_name = match &typ.t {
                    Type::Struct(strukt_name, _) => self.get_full_name(strukt_name),
                    typ => typ.to_string(),
                };
                field_types.insert(name.clone(), (typ.l.clone(), type_name));
            }
            let info = StructInfo {
                location: strukt.location.clone(),
                fields: field_types,
            };
            struct_info.insert(real_name, info);
        }
        if !root_module {
            self.module_stack.pop();
        }
        struct_info
    }

    fn dfs(strukt: &String, lookup: &BTreeMap<String, BTreeSet<String>>, visited: &mut Vec<String>, finished: &mut BTreeSet<String>) -> bool {
        if finished.contains(strukt) {
            return false;
        }
        if visited.contains(strukt) {
            return true;
        }
        visited.push(strukt.clone());
        let mut done = false;
        for neighbor in lookup.get(strukt).unwrap() {
            if Self::dfs(neighbor, lookup, visited, finished) {
                done = true;
            }
        }
        finished.insert(strukt.clone());
        done
    }

    #[trace_call(always)]
    fn find_recursive_structs(&mut self, current_module: &nodes::ModuleNode) {
        /*
        Adapted version of https://en.wikipedia.org/wiki/Cycle_(graph_theory)#Algorithm
        Key difference is that we also keep track of where we went, for better error reporting
         */
        let all_structs = current_module.get_all_structs();
        if all_structs.is_empty() {
            return;
        }
        let mut lookup: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for strukt in &all_structs {
            let strukt_name = strukt.get_full_name();
            let mut fields = BTreeSet::new();
            for field in &strukt.fields {
                let f_type = &field.type_def.typ;
                if f_type.is_struct() {
                    fields.insert(f_type.get_struct_name());
                }
            }
            if !lookup.contains_key(&strukt_name) {
                lookup.insert(strukt_name, fields);
            } else {
                todo!()
            }
        }
        if self.flags.debug {
            println!("[DEBUG] All Structs: {:#?}", all_structs);
            println!("[DEBUG] Lookup: {:#?}", lookup);
        }

        let mut visited = Vec::new();
        let mut finished = BTreeSet::new();

        let mut cycles = vec![];
        for strukt in lookup.keys() {
            if Self::dfs(&strukt, &lookup, &mut visited, &mut finished) {
                cycles.push(visited.clone());
                visited.clear();
                finished.clear();
            }
        }

        for cycle in &cycles {
            if self.flags.debug {
                println!("[DEBUG] Found a cycle: {:?}", cycle);
            }
            debug_assert!(cycle.len() > 0);
            let name = cycle.first().unwrap();
            let first = all_structs.iter().find(|e|e.get_full_name() == *name).unwrap();
            self.report_error(TypeError::RecursiveStruct(
                first.location,
                name.clone(),
                cycle.iter().map(|name| {
                    let strukt = all_structs.iter().find(|e|e.get_full_name() == *name).unwrap();
                    (strukt.location, name.clone())
                }).collect()
            ));
        }
    }

    #[trace_call(always)]
    pub fn type_check_project(&mut self, project: &mut nodes::ModuleNode) -> Result<(), String> {
        macro_rules! perform_step {
            ($step:expr) => {
                {
                    debug_assert!(self.module_stack.is_empty());
                    let res = $step;
                    debug_assert!(self.module_stack.is_empty());
                    if self.errors.len() > 0 {
                        let mut error_string = String::new();
                        for error in &self.errors {
                            error_string.push_str(&format!("{}\n", error));
                        }
                        return Err(error_string);
                    }
                    res
                }
            }
        }
        let root_module = perform_step!(self.fill_lookup(project, true));
        self.module_tree = Some(root_module);
        perform_step!(self.type_check_module(project, true));
        perform_step!(self.find_recursive_structs(project));
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_module(&mut self, module: &mut nodes::ModuleNode, root_module: bool) {
        if !root_module {
            self.module_stack.push(module.name.clone());
            let mut import_errors = Vec::new();
            let mut allowed_imports: Vec<Vec<(String, Location)>> = Vec::new();
            for import in &module.imports {
                let trace = import.trace.clone();
                let mut module = self.module_tree.as_ref().unwrap();
                let mut error = false;
                for (name, location) in &trace {
                    match module.get_sub_module(name) {
                        None => {
                            error = true;
                            import_errors.push(TypeError::UnknownModule(location.clone(), name.clone()));
                            break;
                        }
                        Some(m) => module = m,
                    }
                }
                if !error {
                    for imported in &allowed_imports {
                        if imported.len() == trace.len() {
                            let mut equal = true;
                            for (i, (name, _)) in imported.iter().enumerate() {
                                if name != &trace[i].0 {
                                    equal = false;
                                    break;
                                }
                            }
                            if equal {
                                println!("{}: {:?}: Module already imported.\n{}: {:?}: Here.", WARN_STR, trace[0].1.clone(), NOTE_STR, imported[0].1.clone());
                                break;
                            }
                        }

                    }
                    allowed_imports.push(trace);
                }
            }
            for error in import_errors {
                self.report_error(error);
            }
            let mut v = vec![];
            for imp in allowed_imports {
                let mut v1 = vec![];
                for (s, _) in imp {
                    v1.push(s);
                }
                v.push(v1);
            }
            self.import_stack.push(v);
        } else {
            debug_assert!(module.imports.is_empty());
        }
        for module in &mut module.modules {
            self.type_check_module(module, false);
        }
        for extern_node in &mut module.externs {
            self.type_check_extern(extern_node);
        }
        for s in &mut module.structs {
            self.type_check_struct(s);
        }
        for f in &mut module.functions {
            self.type_check_function(f);
        }
        if !root_module {
            self.module_stack.pop();
            self.import_stack.pop();
        }
    }

    #[trace_call(always)]
    fn type_check_extern(&mut self, extern_node: &mut nodes::ExternNode) {
        for p in &mut extern_node.parameters {
            self.type_check_parameter(p);
        }
        self.type_check_type_node(&mut extern_node.return_type);
        #[cfg(feature = "old_codegen")]
        {
            self.current_stack_size = 0;
        }
    }

    #[trace_call(always)]
    fn type_check_struct(&mut self, struct_node: &mut nodes::StructNode) {
        for field in &mut struct_node.fields {
            self.type_check_field(field);
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
    fn type_check_method(&mut self, method: &mut nodes::MethodNode, struct_name: &str) {
        let current_module = self.get_current_module();
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(current_module.has_struct(&struct_name));
        #[cfg(feature = "old_codegen")]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut method.parameters {
            self.type_check_parameter(param);
        }

        let current_module = self.get_current_module();
        let Some(struct_info) = current_module.get_struct(&struct_name) else {
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
        #[cfg(feature = "old_codegen")]
        {
            method.stack_size = self.current_stack_size;
            self.current_stack_size = 0;
        }

        self.type_check_type_node(&mut method.return_type);

        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_function(&mut self, function: &mut nodes::FunctionNode) {
        let current_module = self.get_current_module();
        debug_assert!(self.known_variables.is_empty());
        debug_assert!(current_module.has_function(&function.name));
        #[cfg(feature = "old_codegen")]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut function.parameters {
            self.type_check_parameter(param);
        }

        let function_info = self.get_current_module().get_function(&function.name).unwrap();

        // Parameters are now known variables
        let parameters = function_info.get_parameters_as_hashmap();
        self.known_variables.push_back(parameters);

        self.type_check_block(&mut function.block);
        #[cfg(feature = "old_codegen")]
        {
            function.stack_size = self.current_stack_size;
            self.current_stack_size = 0;
        }

        self.type_check_type_node(&mut function.return_type);

        self.known_variables.clear();
    }

    #[trace_call(always)]
    fn type_check_parameter(&mut self, parameter: &mut nodes::ParameterNode) {
        self.type_check_type_node(&mut parameter.typ);
        debug_assert!(parameter.typ.typ != Type::Unknown);
        #[cfg(feature = "old_codegen")]
        {
            let var_size = parameter.typ.typ.size();
            self.current_stack_size += var_size;
        }
    }

    #[trace_call(always)]
    fn type_check_block(&mut self, block: &mut nodes::BlockNode) {
        self.add_scope(block.is_unsafe);
        for statement in &mut block.statements {
            self.type_check_statement(statement);
        }
        self.remove_scope(block.is_unsafe);
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
                let _ = self.type_check_expression(expression_node, MutState::Immut);
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
            }
            None => {
                self.type_check_type_node(&mut let_node.typ);
                #[cfg(feature = "old_codegen")]
                {
                    let var_size = let_node.typ.typ.size();
                    self.current_stack_size += var_size;
                }

                let var = Variable {
                    name: let_node.name.clone(),
                    location: let_node.location,
                    typ: self.unwrap_type_node_typ(&let_node.typ),
                    mut_state: MutState::mutable(let_node.is_mutable, let_node.typ.typ.is_mutable_ref()),
                };

                // Need to use `matches` because `==` treats `Type::Any` as a wildcard for reference types
                if matches!(var.typ, Type::Any) && self.unsafe_depth == 0 {
                    // Unsafe to use `Any` outside of unsafe block
                    self.report_error(TypeError::UnsafeAny(let_node.location));
                }

                let needs_mutable = var.typ.is_mutable_ref();
                let mut_state = if needs_mutable { MutState::MutRef } else { MutState::Immut };
                let expr_type = self.type_check_expression(&mut let_node.expression, mut_state);

                let current_scope = self.get_current_scope();
                if current_scope.insert(let_node.name.clone(), var.clone()).is_some() {
                    internal_panic!(
                        "Variable `{}` already exists in current scope",
                        let_node.name
                    );
                }
                let Ok(expr_type) = expr_type else {
                    // Error on expression, no need to continue
                    return;
                };
                if expr_type == Type::Unknown {
                    // Couldnt determine type of expression
                    // We need to `infer` it
                    let _res = self.type_check_expression_with_type(&mut let_node.expression, &var.typ);
                } else {
                    if expr_type != var.typ {
                        self.report_error(TypeError::TypeMismatch(
                            let_node.expression.get_loc(),
                            var.typ.clone(),
                            expr_type,
                        ));
                    }
                }
            }
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_if(&mut self, if_node: &mut nodes::IfNode) {
        let Ok(cond_type) = self.type_check_expression(&mut if_node.condition, MutState::Immut) else {
            return;
        };
        if cond_type != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                if_node.condition.get_loc(),
                Type::Bool,
                cond_type,
            ));
        }
        self.type_check_block(&mut if_node.if_body);
        if let Some(else_branch) = &mut if_node.else_body {
            self.type_check_block(else_branch);
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_return(&mut self, return_node: &mut nodes::ReturnNode) {
        debug_assert!(return_node.typ == Type::Unknown);

        let (expected_return_type, location) = if return_node.strukt.is_none() {
            // We're returning from a normal function
            let Some(function) = self.get_current_module().get_function(&return_node.function) else {
                unreachable!()
            };
            let expected_return_type = function.return_type.t.clone();
            let location = function.location;
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(strukt) = self.get_current_module().get_struct(return_node.strukt.as_ref().unwrap()) else { unreachable!() };
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
            let Ok(expr_type) = self.type_check_expression(ret_expr, MutState::Immut) else {
                return;
            };
            let t = if expr_type == Type::Unknown {
                // we have something like `return 5;`, where we couldn't determine the type
                // so we now have to `infer` the type, and set it accordingly
                let _res = self.type_check_expression_with_type(
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
        let Ok(cond_type) = self.type_check_expression(&mut while_node.condition, MutState::Immut) else {
            return;
        };
        if cond_type != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                while_node.condition.get_loc(),
                Type::Bool,
                cond_type,
            ));
        }
        self.type_check_block(&mut while_node.body);
    }

    #[trace_call(always)]
    fn type_check_stmt_break(&mut self, _break_node: &mut nodes::BreakNode) {}

    #[trace_call(always)]
    fn type_check_stmt_continue(&mut self, _continue_node: &mut nodes::ContinueNode) {}

    #[trace_call(always)]
    fn type_check_expression(
        &mut self,
        expression: &mut nodes::Expression,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        match expression {
            nodes::Expression::Name(name_node) => {
                self.type_check_expr_name(name_node, mut_state)
            }
            nodes::Expression::Unary(unary_expr) => self.type_check_expr_unary(unary_expr, mut_state),
            nodes::Expression::Binary(binary_expr) => {
                self.type_check_expr_binary(binary_expr, mut_state)
            }
            nodes::Expression::FunctionCall(func_call) => {
                self.type_check_expr_function_call(func_call, mut_state)
            }
            nodes::Expression::Literal(literal) => self.type_check_expr_literal(literal),
            nodes::Expression::StructLiteral(literal) => {
                self.type_check_expr_struct_literal(literal, mut_state)
            }
            nodes::Expression::ArrayLiteral(literal) => {
                self.type_check_expr_array_literal(literal, mut_state)
            }
            nodes::Expression::Sizeof(typ) => {
                self.type_check_type_node(typ);
                Ok(Type::Usize)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expression_with_type(
        &mut self,
        expression: &mut nodes::Expression,
        typ: &Type,
    ) -> Result<Type, ()> {
        match expression {
            nodes::Expression::Binary(binary_node) => {
                if binary_node.operation == Operation::IndexedAccess {
                    self.type_check_expr_indexed_access_with_type(expression, typ)
                } else {
                    self.type_check_expression_with_type(&mut binary_node.lhs, typ)?;
                    self.type_check_expression_with_type(&mut binary_node.rhs, typ)?;
                    binary_node.typ = typ.clone();
                    Ok(typ.clone())
                }
            }
            nodes::Expression::Literal(lit_node) => {
                if lit_node.typ != Type::Unknown && lit_node.typ != *typ {
                    self.report_error(TypeError::TypeMismatch(
                        lit_node.location,
                        typ.clone(),
                        lit_node.typ.clone(),
                    ));
                    Err(())
                } else if let Type::Ref(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "reference",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                    Err(())
                } else if let Type::Array(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "array",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                    Err(())
                } else if let Type::Struct(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "struct instance",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                    Err(())
                } else if *typ == Type::Bool {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "boolean",
                        lit_node.location,
                        lit_node.value.clone(),
                    ));
                    Err(())
                } else {
                    lit_node.typ = typ.clone();
                    Ok(typ.clone())
                }
            }
            nodes::Expression::Name(name_node) => {
                debug_assert!(name_node.typ != Type::Unknown);
                if name_node.typ != *typ {
                    self.report_error(TypeError::TypeMismatch(
                        name_node.location,
                        typ.clone(),
                        name_node.typ.clone(),
                    ));
                    Err(())
                } else {
                    Ok(typ.clone())
                }
            }
            nodes::Expression::Unary(unary_node) => {
                match unary_node.operation {
                    Operation::Negate => {
                        let expr_type =
                            self.type_check_expression_with_type(&mut unary_node.expression, typ)?;
                        debug_assert!(expr_type != Type::Unknown);
                        if expr_type != Type::I32 && expr_type != Type::I64 {
                            self.report_error(TypeError::NegationTypeMismatch(
                                unary_node.location,
                                typ.clone(),
                            ));
                            Err(())
                        } else if expr_type != *typ {
                            self.report_error(TypeError::TypeMismatch(
                                unary_node.location,
                                typ.clone(),
                                expr_type.clone(),
                            ));
                            Err(())
                        } else {
                            unary_node.typ = typ.clone();
                            Ok(typ.clone())
                        }
                    }
                    Operation::Reference => {
                        // FIXME: Do we need to consider mutability here?
                        let Type::Ref(typ, _mutable) = typ else {
                            internal_panic!("UnaryNode with Operation::Reference has wrong type! Got: {:?}", typ);
                        };
                        let typ = typ.as_ref();
                        let expr_type =
                            self.type_check_expression_with_type(&mut unary_node.expression, typ)?;
                        debug_assert!(expr_type != Type::Unknown);
                        if expr_type != *typ {
                            self.report_error(TypeError::TypeMismatch(
                                unary_node.location,
                                typ.clone(),
                                expr_type.clone(),
                            ));
                            Err(())
                        } else {
                            unary_node.typ = typ.clone();
                            Ok(typ.clone())
                        }
                    }
                    Operation::LogicalNot => {
                        let expr_type =
                            self.type_check_expression_with_type(&mut unary_node.expression, &Type::Bool)?;
                        if expr_type == Type::Unknown {
                            unary_node.typ = Type::Bool;
                            Ok(Type::Bool)
                        } else if expr_type != Type::Bool {
                            self.report_error(TypeError::TypeMismatch(
                                unary_node.location,
                                Type::Bool,
                                expr_type.clone(),
                            ));
                            Err(())
                        } else {
                            unary_node.typ = Type::Bool;
                            Ok(Type::Bool)
                        }
                    }
                    Operation::Dereference => {
                        let expr_type =
                            self.type_check_expression_with_type(&mut unary_node.expression, &Type::Ref(Box::new(typ.clone()), true))?;
                        debug_assert!(matches!(expr_type, Type::Ref(..)));
                        unary_node.typ = typ.clone();
                        Ok(typ.clone())
                    }
                    _ => internal_panic!(
                        "type_check_expression_with_type for {:?} is not implemented yet!",
                        unary_node.operation
                    ),
                }
            }
            nodes::Expression::ArrayLiteral(array_literal) => {
                let Type::Array(ref elem_type, size) = *typ else {
                    internal_panic!("ArrayLiteral with wrong type! Got: {:?}", typ);
                };
                let Type::Array(_, literal_size) = array_literal.typ else {
                    internal_panic!("ArrayLiteral with wrong type! Got: {:?}", array_literal.typ);
                };
                if size != literal_size {
                    self.report_error(TypeError::ArraySizeMismatch(
                        array_literal.location,
                        size,
                        literal_size,
                    ));
                    return Err(());
                }
                for expr in &mut array_literal.elements {
                    let Ok(expr_type) = self.type_check_expression_with_type(expr, elem_type) else {
                        continue;
                    };
                    if expr_type != **elem_type {
                        self.report_error(TypeError::TypeMismatch(
                            expr.get_loc(),
                            *elem_type.clone(),
                            expr_type.clone(),
                        ));
                    }
                }
                array_literal.typ = Type::Array(Box::new(*elem_type.clone()), literal_size);
                Ok(array_literal.typ.clone())
            }
            e => internal_panic!(
                "type_check_expression_with_type for {:?} is not implemented yet!",
                e
            ),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_indexed_access_with_type(
        &mut self,
        indexed_access: &mut nodes::Expression,
        typ: &Type,
    ) -> Result<Type, ()> {
        match indexed_access {
            nodes::Expression::Binary(indexed_access) => {
                // LHS can be anything (Expression::Binary, etc), so we call this function again
                // All we care about is that the type of LHS is an array, that's why we pass `usize::MAX`
                let _lhs = self.type_check_expr_indexed_access_with_type(&mut indexed_access.lhs, &Type::Array(Box::new(typ.clone()), usize::MAX))?;
                // RHS is an index, so it should be usize
                let _rhs = self.type_check_expression_with_type(&mut indexed_access.rhs, &Type::Usize)?;
                Ok(typ.clone())
            }
            nodes::Expression::Name(name_node) => {
                let Type::Array(ref wanted_typ, wanted_size) = typ else {
                    internal_panic!("IndexedAccess with wrong type! Got: {:?}", typ);
                };
                debug_assert!(*wanted_size == usize::MAX);
                let Some(var) = self.get_variable(&name_node.name) else {
                    self.report_error(TypeError::UndeclaredVariable(
                        name_node.location,
                        name_node.name.clone(),
                    ));
                    return Err(());
                };
                let Type::Array(ref elem_type, _) = var.typ else {
                    self.report_error(TypeError::InvalidIndexedAccess(
                        name_node.location,
                        var.typ.clone(),
                    ));
                    return Err(());
                };
                if elem_type != wanted_typ {
                    self.report_error(TypeError::TypeMismatch(
                        name_node.location,
                        typ.clone(),
                        *elem_type.clone(),
                    ));
                    return Err(());
                }
                name_node.typ = var.typ.clone();
                Ok(var.typ)
            }
            e => internal_panic!(
                "type_check_expr_indexed_access_with_type for {:?} is not implemented yet!",
                e
            ),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_name(
        &mut self,
        name_node: &mut nodes::NameNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        let var = self.get_variable(&name_node.name);
        match var {
            Some(var) => {
                if mut_state != MutState::Immut && mut_state & var.mut_state == 0 {
                    // mut_state & var.mut_state == 0 means mismatch in required mutability
                    // e.g. if mut_state = MutRef, that means original Variable isn't MutRef
                    // (it still might be MutVar, but in this case that doesn't matter)
                    self.report_error(TypeError::ImmutableModification(
                        name_node.location,
                        name_node.name.clone(),
                        var.location,
                    ));
                    return Err(());
                }
                name_node.typ = var.typ.clone();
                Ok(var.typ)
            }
            None => {
                self.report_error(TypeError::UndeclaredVariable(
                    name_node.location,
                    name_node.name.clone(),
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_unary(&mut self, unary_expr: &mut nodes::UnaryNode, mut_state: MutStateVal) -> Result<Type, ()> {
        match unary_expr.operation {
            Operation::Negate => {
                let expr_type = self.type_check_expression(&mut unary_expr.expression, MutState::Immut)?;
                if expr_type == Type::Unknown {
                    return Ok(Type::Unknown);
                }
                if expr_type != Type::I32 && expr_type != Type::I64 {
                    self.report_error(TypeError::NegationTypeMismatch(
                        unary_expr.location,
                        expr_type.clone(),
                    ));
                }
                unary_expr.typ = expr_type.clone();
                Ok(expr_type)
            }
            Operation::Reference => {
                let Type::Ref(_, is_mutable) = unary_expr.typ else {
                    internal_panic!("UnaryNode with Operation::Reference has wrong type!");
                };
                let exp_mut_state = MutState::mutable(is_mutable, is_mutable);
                let expr_type = self.type_check_expression(&mut unary_expr.expression, exp_mut_state)?;
                if expr_type == Type::Unknown {
                    return Ok(Type::Unknown);
                }
                if matches!(expr_type, Type::Ref(..)) {
                    // TODO: Should we allow references to references?
                    self.report_error(TypeError::NestedReferenceNotAllowedYet(unary_expr.location));
                }
                unary_expr.typ = Type::Ref(Box::new(expr_type.clone()), is_mutable);
                Ok(unary_expr.typ.clone())
            }
            Operation::Dereference => {
                let new_mut = if mut_state == MutState::MutVar {
                    mut_state | MutState::MutRef
                } else {
                    MutState::Immut
                };
                let expr_type = self.type_check_expression(&mut unary_expr.expression, new_mut)?;
                match expr_type {
                    Type::Ref(t, is_mut) => {
                        if mut_state != MutState::Immut && !is_mut {
                            self.report_error(TypeError::ImmutDerefInMutContext(
                                unary_expr.location,
                                unary_expr.expression.get_loc(),
                            ));
                            return Err(());
                        }
                        unary_expr.typ = *t.clone();
                        Ok(*t.clone())
                    }
                    Type::Any => {
                        Ok(Type::Unknown)
                    }
                    Type::Unknown => {
                        // Only case where a type is unknown is integer literals
                        // We can't dereference an integer literal
                        self.report_error(TypeError::DereferenceIntegerLiteral(
                            unary_expr.location,
                        ));
                        Err(())
                    }
                    _ => {
                        self.report_error(TypeError::DereferenceTypeMismatch(
                            unary_expr.location,
                            expr_type.clone(),
                        ));
                        Err(())
                    }
                }
            }
            Operation::LogicalNot => {
                let expr_type = self.type_check_expression(&mut unary_expr.expression, MutState::Immut)?;
                if expr_type == Type::Unknown {
                    return Ok(Type::Unknown);
                }
                if expr_type != Type::Bool {
                    self.report_error(TypeError::LogicalNotTypeMismatch(
                        unary_expr.location,
                        expr_type.clone(),
                    ));
                }
                unary_expr.typ = Type::Bool;
                Ok(Type::Bool)
            }
            _ => {
                internal_panic!(
                    "type_check_expr_unary for {:?} is not implemented yet!",
                    unary_expr.operation
                )
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        match binary_expr.operation {
            Operation::MemberAccess => self.type_check_expr_member_access(binary_expr, mut_state),
            Operation::IndexedAccess => self.type_check_expr_indexed_access(binary_expr, mut_state),
            Operation::Assign => self.type_check_expr_assign(binary_expr),
            _ if binary_expr.is_comparison() => self.type_check_expr_binary_comparison(binary_expr),
            _ if binary_expr.is_arithmetic() => self.type_check_expr_binary_arithmetic(binary_expr),
            _ if binary_expr.is_bitwise() => self.type_check_expr_binary_bitwise(binary_expr),
            _ if binary_expr.is_logical() => self.type_check_expr_binary_logical(binary_expr),
            op => internal_panic!("Entered type_check_expr_binary with unknown operation: {:?}", op),
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_logical(&mut self, binary_expr: &mut nodes::BinaryNode) -> Result<Type, ()> {
        assert!(binary_expr.is_logical());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, MutState::Immut)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
        if lhs_type != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                binary_expr.lhs.get_loc(),
                Type::Bool,
                lhs_type.clone(),
            ));
            return Err(());
        }
        if rhs_type != Type::Bool {
            self.report_error(TypeError::TypeMismatch(
                binary_expr.rhs.get_loc(),
                Type::Bool,
                rhs_type.clone(),
            ));
            return Err(());
        }
        binary_expr.typ = Type::Bool;
        Ok(Type::Bool)
    }

    #[trace_call(always)]
    fn type_check_expr_binary_bitwise(&mut self, binary_expr: &mut nodes::BinaryNode) -> Result<Type, ()> {
        assert!(binary_expr.is_bitwise());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, MutState::Immut)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
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
                return Err(());
            }
            (Type::Ref(..), _) | (_, Type::Ref(..)) => {
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                return Err(());
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
                return Err(());
            }
            (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
            (Type::Unknown, other) => {
                let typ = self.type_check_expression_with_type(&mut binary_expr.lhs, other)?;
                binary_expr.typ = typ.clone();
                Ok(typ)
            }
            (other, Type::Unknown) => {
                let typ = self.type_check_expression_with_type(&mut binary_expr.rhs, other)?;
                binary_expr.typ = typ.clone();
                Ok(typ)
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
                Ok(lhs.clone())
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_arithmetic(&mut self, binary_expr: &mut nodes::BinaryNode) -> Result<Type, ()> {
        assert!(binary_expr.is_arithmetic());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, MutState::Immut)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
        match (&lhs_type, &rhs_type) {
            (typ @ Type::Ref(..), other) => {
                if *other == Type::Unknown {
                    self.type_check_expression_with_type(&mut binary_expr.rhs, &Type::Usize)?;
                } else if *other != Type::Usize {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        binary_expr.location,
                        binary_expr.operation.clone(),
                        binary_expr.lhs.get_loc(),
                        lhs_type.clone(),
                        binary_expr.rhs.get_loc(),
                        rhs_type.clone(),
                    ));
                    return Err(());
                }
                let other = binary_expr.rhs.get_type();
                debug_assert!(other == Type::Usize);
                if self.unsafe_depth == 0 {
                    self.report_error(TypeError::UnsafePointerArithmetics(
                        binary_expr.location
                    ));
                    return Err(());
                }
                if binary_expr.operation != Operation::Add && binary_expr.operation != Operation::Sub {
                    self.report_error(TypeError::InvalidPointerArithmetics(
                        binary_expr.location,
                        binary_expr.operation.clone()
                    ));
                    return Err(());
                }
                binary_expr.typ = typ.clone();
                Ok(typ.clone())
            }
            (other, typ @ Type::Ref(..)) => {
                if *other == Type::Unknown {
                    self.type_check_expression_with_type(&mut binary_expr.lhs, &Type::Usize)?;
                } else if *other != Type::Usize {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        binary_expr.location,
                        binary_expr.operation.clone(),
                        binary_expr.lhs.get_loc(),
                        lhs_type.clone(),
                        binary_expr.rhs.get_loc(),
                        rhs_type.clone(),
                    ));
                    return Err(());
                }
                let other = binary_expr.lhs.get_type();
                debug_assert!(other == Type::Usize);
                if self.unsafe_depth == 0 {
                    self.report_error(TypeError::UnsafePointerArithmetics(
                        binary_expr.location
                    ));
                    return Err(());
                }
                if binary_expr.operation != Operation::Add && binary_expr.operation != Operation::Sub {
                    self.report_error(TypeError::InvalidPointerArithmetics(
                        binary_expr.location,
                        binary_expr.operation.clone()
                    ));
                    return Err(());
                }
                binary_expr.typ = typ.clone();
                Ok(typ.clone())
            }
            (Type::Struct(..), _) | (_, Type::Struct(..))
            | (Type::Array(..), _) | (_, Type::Array(..))
            | (Type::Bool, _) | (_, Type::Bool) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Err(())
            }
            (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut binary_expr.lhs, other)?;
                let typ = other;
                binary_expr.typ = typ.clone();
                Ok(typ.clone())
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other)?;
                let typ = other.clone();
                binary_expr.typ = typ.clone();
                Ok(typ)
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
                Ok(typ)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_binary_comparison(&mut self, binary_expr: &mut nodes::BinaryNode) -> Result<Type, ()> {
        assert!(binary_expr.is_comparison());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, MutState::Immut)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
        match (&lhs_type, &rhs_type) {
            (Type::Struct(..), _) | (_, Type::Struct(..))
            | (Type::Array(..), _) | (_, Type::Array(..)) => {
                // NOTE: Modify this once more features (ahem, operator overload) exist
                self.report_error(TypeError::BinaryTypeMismatch(
                    binary_expr.location,
                    binary_expr.operation.clone(),
                    binary_expr.lhs.get_loc(),
                    lhs_type.clone(),
                    binary_expr.rhs.get_loc(),
                    rhs_type.clone(),
                ));
                Err(())
            }
            (Type::Unknown, Type::Unknown) => {
                let force_type = Type::I32;
                self.type_check_expression_with_type(&mut binary_expr.lhs, &force_type)?;
                self.type_check_expression_with_type(&mut binary_expr.rhs, &force_type)?;
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                Ok(typ)
            }
            (Type::Unknown, other) => {
                self.type_check_expression_with_type(&mut binary_expr.lhs, other)?;
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                Ok(typ)
            }
            (other, Type::Unknown) => {
                self.type_check_expression_with_type(&mut binary_expr.rhs, other)?;
                let typ = Type::Bool;
                binary_expr.typ = typ.clone();
                Ok(typ)
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
                Ok(typ)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_assign(&mut self, assign_expr: &mut nodes::BinaryNode) -> Result<Type, ()> {
        if !assign_expr.lhs.is_lvalue() {
            self.report_error(TypeError::InvalidLValue(assign_expr.lhs.get_loc()));
            return Err(());
        }
        let lhs_type = self.type_check_expression(&mut assign_expr.lhs, MutState::MutVar)?;
        let rhs_type = self.type_check_expression(&mut assign_expr.rhs, MutState::Immut)?;
        debug_assert!(lhs_type != Type::Unknown);

        if rhs_type == Type::Unknown {
            // Infer type of rhs from lhs
            let typ = self.type_check_expression_with_type(&mut assign_expr.rhs, &lhs_type)?;
            assign_expr.typ = typ.clone();
            Ok(typ)
        } else if lhs_type != rhs_type {
            self.report_error(TypeError::TypeMismatch(
                assign_expr.location,
                lhs_type.clone(),
                rhs_type.clone(),
            ));
            Err(())
        } else {
            assign_expr.typ = rhs_type.clone();
            Ok(rhs_type)
        }
    }

    #[trace_call(always)]
    fn type_check_expr_member_access(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, mut_state)?;
        let (is_ref, strukt) = match &lhs_type {
            Type::Ref(orig_type, _) => {
                let Type::Struct(ref struct_name, ref module_path) = **orig_type else {
                    self.report_error(TypeError::DotOnNonStruct(binary_expr.lhs.get_loc()));
                    return Err(());
                };
                let module = if let Some(module_spec) = module_path {
                    let Some(module) = self.resolve_module_specifier(module_spec) else {
                        todo!() // Error, probably from the Compiler
                    };
                    module
                } else {
                    self.get_current_module()
                };
                let Some(strukt) = module.get_struct(struct_name) else {
                    self.report_error(TypeError::UnknownType(
                        binary_expr.lhs.get_loc(),
                        (**orig_type).clone()
                    ));
                    return Err(());
                };
                (true, strukt)
            }
            ref struct_type @ Type::Struct(ref struct_name, module_path) => {
                let module = if let Some(module_spec) = module_path {
                    let Some(module) = self.resolve_module_specifier(module_spec) else {
                        todo!() // Error, probably from the Compiler
                    };
                    module
                } else {
                    self.get_current_module()
                };
                let Some(strukt) = module.get_struct(struct_name) else {
                    self.report_error(TypeError::UnknownType(
                        binary_expr.lhs.get_loc(),
                        (*struct_type).clone()
                    ));
                    return Err(());
                };
                (false, strukt)
            }
            _ => {
                self.report_error(TypeError::DotOnNonStruct(binary_expr.lhs.get_loc()));
                return Err(());
            }
        };
        let strukt = strukt.clone();
        match &mut (*binary_expr.rhs) {
            nodes::Expression::Name(name_node) => {
                if let Some(field) = strukt.get_field(&name_node.name) {
                    binary_expr.typ = field.t.clone();
                    name_node.typ = field.t.clone();
                    Ok(field.t.clone())
                } else {
                    self.report_error(TypeError::UnknownField(
                        name_node.location,
                        name_node.name.clone(),
                        strukt.location,
                        strukt.name.clone(),
                    ));
                    Err(())
                }
            }
            nodes::Expression::FunctionCall(call_node) => {
                let Some(method) = strukt.get_method(&call_node.function_name) else {
                    self.report_error(TypeError::UnknownMethod(
                        call_node.location,
                        call_node.function_name.clone(),
                        strukt.location,
                        strukt.name.clone(),
                    ));
                    return Err(());
                };
                if method.is_unsafe && self.unsafe_depth == 0 {
                    self.report_error(TypeError::UnsafeCallInSafeContext(
                        "Method",
                        call_node.location,
                        call_node.function_name.clone(),
                        method.location,
                    ));
                    return Err(());
                }
                let result = if method.has_this {
                    // FIXME: This is not a good solution, but it works for now
                    if let expected_this @ Type::Ref(_, expected_mutable) = &method.parameters[0].typ {
                        let this_mut_state = MutState::mutable(*expected_mutable, *expected_mutable);
                        self.type_check_expression(&mut binary_expr.lhs, this_mut_state)?;
                        let this_arg = if is_ref {
                            *binary_expr.lhs.clone()
                        } else {
                            let new_node = nodes::UnaryNode {
                                location: binary_expr.lhs.get_loc(),
                                operation: Operation::Reference,
                                expression: binary_expr.lhs.clone(),
                                typ: expected_this.clone(),
                            };
                            nodes::Expression::Unary(new_node)
                        };
                        if self.flags.debug {
                            println!("[DEBUG] MemberAcces::FunctionCall with this_arg: {:?}", this_arg);
                        }
                        call_node.arguments.insert(0, this_arg);
                    } else {
                        call_node.arguments.insert(0, *binary_expr.lhs.clone());
                    }
                    let result = check_function!(self, call_node, method, "Method")?;
                    call_node.arguments.remove(0);
                    result
                } else {
                    check_function!(self, call_node, method, "Method")?
                };
                call_node.module_path = method.module_path.clone();
                binary_expr.typ = result.clone();
                Ok(result)
            }
            _ => {
                self.report_error(TypeError::InvalidMemberAccess(
                    binary_expr.rhs.get_loc(),
                    "Non-NameNode or Non-FunctionCall on RHS"
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_indexed_access(
        &mut self,
        binary_expr: &mut nodes::BinaryNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        assert!(binary_expr.is_indexed_access());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, mut_state)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
        match (lhs_type, rhs_type) {
            (Type::Unknown, Type::Unknown) => Ok(Type::Unknown),
            (Type::Array(typ, _), Type::Unknown) => {
                let rhs_typ = self.type_check_expression_with_type(&mut binary_expr.rhs, &Type::Usize)?;
                debug_assert!(rhs_typ == Type::Usize);
                binary_expr.typ = *typ.clone();
                Ok(*typ)
            }
            (Type::Array(typ, _), Type::Usize) => {
                binary_expr.typ = *typ.clone();
                Ok(*typ)
            }
            (Type::Array(_, _), rhs_type) => {
                self.report_error(TypeError::ArrayIndexRequiresUsize(
                    binary_expr.location,
                    rhs_type.clone(),
                ));
                Err(())
            }
            (l, r) => internal_panic!("type_check_expr_indexed_access for {:?} is not implemented yet!", (l, r)),
        }
    }

    #[trace_call(always)]
    fn rebuild_module_from_stack(&self, location: &Location, module_stack: &Vec<String>) -> Result<&Module, TypeError> {
        let Some(mut module) = self.get_module(&module_stack[0]) else {
            return Err(TypeError::UnknownModule(
                location.clone(),
                module_stack[0].clone()
            ));
        };
        for module_name in module_stack.iter().skip(1) {
            if let Some(sub_module) = module.get_sub_module(module_name) {
                module = sub_module;
            } else {
                return Err(TypeError::UnknownModule(
                    location.clone(),
                    module_name.clone()
                ));
            }
        }
        Ok(module)
    }

    #[trace_call(always)]
    fn type_check_expr_module_function_call(
        &mut self,
        call_node: &mut nodes::CallNode,
        module_stack: &mut Vec<String>,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        if mut_state != MutState::Immut {
            self.report_error(TypeError::CantMutateTemporary(call_node.location.clone()));
        }
        let Ok(module) = self.rebuild_module_from_stack(&call_node.location, module_stack) else {
            self.report_error(TypeError::UnknownModule(
                call_node.location.clone(),
                module_stack.join("::")
            ));
            return Err(());
        };
        if let Some(external) = module.get_extern(&call_node.function_name) {
            let result = check_function!(self, call_node, external, "External Function")?;
            call_node.typ = result.clone();
            Ok(result)
        } else if let Some(function) = module.get_function(&call_node.function_name) {
            let result = check_function!(self, call_node, function, "Function")?;
            call_node.typ = result.clone();
            Ok(result)
        } else {
            self.report_error(TypeError::UndeclaredFunction(
                call_node.location.clone(),
                call_node.function_name.clone()
            ));
            Err(())
        }
    }

    #[trace_call(always)]
    fn type_check_expr_literal(&mut self, literal: &mut nodes::LiteralNode) -> Result<Type, ()> {
        Ok(literal.typ.clone())
    }

    #[trace_call(always)]
    fn type_check_expr_struct_literal(
        &mut self,
        literal: &mut nodes::StructLiteralNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        if mut_state != MutState::Immut {
            self.report_error(TypeError::CantMutateTemporary(literal.location));
        }
        let (strukt, module_path) = if let Some(module_path) = &literal.module_path {
            let Some(module) = self.resolve_module_specifier(module_path) else {
                todo!() // Error
            };
            let Some(strukt) = module.get_struct(&literal.struct_name) else {
                todo!() // Error
            };
            // REVIEW: All should be clear here? Nothing else to do?
            (strukt, module_path.clone())
        } else {
            let Some((strukt, path)) = self.resolve_struct(&literal.struct_name) else {
                self.report_error(TypeError::UnknownType(
                    literal.location,
                    Type::Struct(literal.struct_name.clone(), None),
                ));
                return Err(());
            };
            let module_spec = nodes::ModuleSpecifier::from_resolved(&path);
            literal.module_path = Some(Box::new(module_spec.clone()));
            (strukt, Box::new(module_spec))
        };
        let strukt = strukt.clone();
        self.type_check_struct_literal_fields(literal, &strukt)?;
        literal.typ = Type::Struct(literal.struct_name.clone(), Some(module_path));
        Ok(literal.typ.clone())
    }

    #[trace_call(always)]
    fn type_check_struct_literal_fields(
        &mut self,
        literal: &mut nodes::StructLiteralNode,
        strukt: &Struct,
    ) -> Result<(), ()> {
        let mut fields = HashMap::new();
        for field in &mut literal.fields {
            let Some(field_info) = strukt.get_field(&field.0) else {
                self.report_error(TypeError::UnknownField(
                    field.1.get_loc(),
                    field.0.clone(),
                    strukt.location,
                    strukt.name.clone(),
                ));
                continue;
            };
            let field_type = field_info.t.clone();
            let Ok(expr_type) = self.type_check_expression(&mut field.1, MutState::Immut) else {
                continue;
            };
            if expr_type == Type::Unknown {
                // We couldn't determine the type of the expression
                // We need to `infer` it
                let t1 = self.type_check_expression_with_type(&mut field.1, &field_type)?;
                debug_assert!(t1 == field_type);
            } else if expr_type != field_type {
                self.report_error(TypeError::TypeMismatch(
                    field.1.get_loc(),
                    field_type.clone(),
                    expr_type,
                ));
            }
            fields.insert(field.0.clone(), field_type);
        }
        let mut errors = vec![];
        for field in strukt.fields.iter() {
            if !fields.contains_key(field.0) {
                errors.push(TypeError::MissingField(
                    literal.location,
                    field.0.clone(),
                    strukt.location,
                    literal.struct_name.clone(),
                ));
            }
        }
        for error in errors {
            self.report_error(error);
        }
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_expr_array_literal(
        &mut self,
        literal: &mut nodes::ArrayLiteralNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        if mut_state != MutState::Immut {
            self.report_error(TypeError::CantMutateTemporary(
                literal.location
            ));
        }
        if let Some(size) = literal.size {
            // [elem; N] syntax
            debug_assert!(literal.elements.len() == 1);
            let Ok(elem_type) = self.type_check_expression(&mut literal.elements[0], MutState::Immut) else {
                return Ok(Type::Unknown);
            };
            if elem_type == Type::Unknown {
                return Ok(Type::Unknown);
            }
            literal.typ = Type::Array(Box::new(elem_type), size);
            Ok(literal.typ.clone())
        } else {
            // [elem, elem, elem] syntax
            let mut typ = Type::Unknown;
            let mut unknown_indices = vec![];
            let mut inferred_location = None;
            for (index, elem) in literal.elements.iter_mut().enumerate() {
                let Ok(elem_type) = self.type_check_expression(elem, MutState::Immut) else {
                    continue;
                };
                if elem_type == Type::Unknown {
                    unknown_indices.push(index);
                } else if typ == Type::Unknown {
                    typ = elem_type;
                    inferred_location = Some(elem.get_loc());
                } else if elem_type != typ {
                    debug_assert!(inferred_location.is_some());
                    self.report_error(TypeError::ArrayLiteralElementTypeMismatch(
                        elem.get_loc(),
                        elem_type,
                        inferred_location.unwrap(),
                        typ.clone(),
                    ));
                }
            }
            if typ == Type::Unknown {
                debug_assert!(unknown_indices.len() == literal.elements.len());
                // We're done here, all elements are unknown
                return Ok(Type::Unknown)
            }
            if unknown_indices.len() > 0 {
                debug_assert!(inferred_location.is_some());
            }
            for index in unknown_indices {
                let elem = &mut literal.elements[index];
                let elem_type = self.type_check_expression_with_type(elem, &typ)?;
                if elem_type != typ {
                    self.report_error(TypeError::ArrayLiteralElementTypeMismatch(
                        elem.get_loc(),
                        elem_type,
                        inferred_location.unwrap(),
                        typ.clone(),
                    ));
                }
            }
            literal.typ = Type::Array(Box::new(typ), literal.elements.len());
            Ok(literal.typ.clone())
        }
    }

    #[trace_call(always)]
    fn resolve_function(
        &self,
        func_name: &str,
    ) -> Result<(&Function, Vec<String>), ()> {
        let current_module: &Module = self.get_current_module();
        if let Some(func) = current_module.get_function(func_name) {
            if self.flags.debug {
                println!("[DEBUG] `{func_name}` is a function defined in this file.");
            }
            return Ok((func, self.module_stack.clone()))
        } else if let Some(ext) = current_module.get_extern(func_name) {
            if self.flags.debug {
                println!("[DEBUG] `{func_name}` is an extern defined in this file.");
            }
            return Ok((ext, self.module_stack.clone()))
        }
        for stack in &self.import_stack {
            for import in stack {
                if let Some((module, mod_path)) = self.resolve_module(import) {
                    if let Some(func) = module.get_function(func_name) {
                        if self.flags.debug {
                            println!("[DEBUG] `{func_name}` is a function defined in module `{0}`.", mod_path.join("."));
                        }
                        return Ok((func, mod_path))
                    } else if let Some(ext) = module.get_extern(func_name) {
                        if self.flags.debug {
                            println!("[DEBUG] `{func_name}` is an extern defined in module `{0}`.", mod_path.join("."));
                        }
                        return Ok((ext, mod_path))
                    }
                }
            }
        }
        Err(())
    }

    #[trace_call(always)]
    fn resolve_struct(
        &self,
        struct_name: &str
    ) -> Option<(&Struct, Vec<String>)> {
        // REVIEW: We want relative to root
        let current_module: &Module = self.get_current_module();
        if let Some(strukt) = current_module.get_struct(struct_name) {
            if self.flags.debug {
                println!("[DEBUG] `{struct_name}` is a struct defined in this file.");
            }
            return Some((strukt, self.module_stack.clone()))
        }
        for stack in &self.import_stack {
            for import in stack {
                if let Some((module, mod_path)) = self.resolve_module(import) {
                    if let Some(strukt) = module.get_struct(struct_name) {
                        if self.flags.debug {
                            println!("[DEBUG] `{struct_name}` is a struct defined in module `{0}`.", module.name);
                        }
                        return Some((strukt, mod_path))
                    }
                }
            }
        }
        None
    }

    #[trace_call(always)]
    fn resolve_module_specifier(&self, module_spec: &nodes::ModuleSpecifier) -> Option<&Module> {
        if let Some(sub_module) = &module_spec.sub_module {
            let module = self.resolve_module_specifier(sub_module)?;
            module.get_sub_module(&module_spec.name)
        } else {
            self.get_module(&module_spec.name)
        }
    }

    #[trace_call(always)]
    fn type_check_expr_function_call(
        &mut self,
        func_call: &mut nodes::CallNode,
        mut_state: MutStateVal,
    ) -> Result<Type, ()> {
        if mut_state != MutState::Immut {
            self.report_error(TypeError::CantMutateTemporary(
                func_call.location,
            ));
        }
        let (function, mod_path) = if let Some(specified_path) = &func_call.module_path {
            let Some(module) = self.resolve_module_specifier(specified_path) else {
                self.report_error(TypeError::UnknownModuleSpecifier(
                    func_call.location.clone(),
                    *specified_path.clone()
                ));
                return Err(());
            };
            let function = if module.is_function(&func_call.function_name) {
                let Some(function) = module.get_function(&func_call.function_name) else {
                    todo!() // Error
                };
                function
            } else if module.is_extern(&func_call.function_name) {
                let Some(external) = module.get_extern(&func_call.function_name) else {
                    todo!() // Error
                };
                external
            } else {
                todo!() // Error
            };
            (function, specified_path.to_path())
        } else {
            let Ok((func, modp)) = self.resolve_function(&func_call.function_name) else {
                self.report_error(TypeError::UndeclaredFunction(
                    func_call.location.clone(),
                    func_call.function_name.clone()
                ));
                return Err(());
            };
            (func, modp)
        };
        let mut module_path = None;
        for sub_mod in mod_path.iter().rev() {
            module_path = Some(Box::new(nodes::ModuleSpecifier {
                name: sub_mod.clone(),
                sub_module: module_path
            }));
        }
        func_call.module_path = module_path;
        if function.is_unsafe && self.unsafe_depth == 0 {
            self.report_error(TypeError::UnsafeCallInSafeContext(
                "Function",
                func_call.location,
                func_call.function_name.clone(),
                function.location,
            ));
            return Err(());
        }
        let return_type = function.return_type.clone();
        if let Type::Struct(struct_name, _) = &return_type.t {
            if self.resolve_struct(struct_name).is_none() {
                self.report_error(TypeError::UnknownType(
                    return_type.l.clone(),
                    return_type.t.clone(),
                ));
                return Err(());
            }
        }
        check_function!(self, func_call, function, "Function")
    }

    #[trace_call(always)]
    fn type_check_type_node(&mut self, type_node: &mut nodes::TypeNode) {
        let mut bottom_type = &mut type_node.typ;
        debug_assert!(self.module_tree.is_some());
        match bottom_type {
            Type::Struct(name, path) => {
                if let Some(module_spec) = &path {
                    let Some(module) = self.resolve_module_specifier(module_spec) else {
                        self.report_error(TypeError::UnknownModuleSpecifier(
                            type_node.location,
                            *module_spec.clone()
                        ));
                        return;
                    };
                    if !module.has_struct(name) {
                        todo!() // Error
                    }
                    // REVIEW: All should be clear here? Nothing else to do?
                } else {
                    let Some((_strukt, path)) = self.resolve_struct(name) else {
                        self.report_error(TypeError::UnknownType(
                            type_node.location,
                            bottom_type.clone(),
                        ));
                        return;
                    };
                    type_node.module_path = Some(Box::new(nodes::ModuleSpecifier::from_resolved(&path)));
                    type_node.typ = Type::Struct(name.to_string(), Some(Box::new(nodes::ModuleSpecifier::from_resolved(&path))));
                }
            },
            ref mut typ @ Type::Array(..)
            | ref mut typ @ Type::Ref(..) => {
                let underlying = typ.get_underlying_type_ref_mut();
                if let Type::Struct(name, path) = underlying {
                    if let Some(module_spec) = &path {
                        let Some(module) = self.resolve_module_specifier(module_spec) else {
                            self.report_error(TypeError::UnknownModuleSpecifier(
                                type_node.location,
                                *module_spec.clone()
                            ));
                            return;
                        };
                        if !module.has_struct(name) {
                            todo!() // Error
                        }
                        // REVIEW: All should be clear here? Nothing else to do?
                    } else {
                        let Some((_strukt, path)) = self.resolve_struct(name) else {
                            self.report_error(TypeError::UnknownType(
                                type_node.location,
                                bottom_type.clone(),
                            ));
                            return;
                        };
                        *underlying = Type::Struct(name.to_string(), Some(Box::new(nodes::ModuleSpecifier::from_resolved(&path))));
                    }
                }
            },
            t => {
                debug_assert!(!t.is_compound(), "{t}");
            }
        }
    }

    #[trace_call(always)]
    fn unwrap_type_node_typ(&self, param_typ: &nodes::TypeNode) -> Type {
        let module_path = if param_typ.module_path.is_some() {
            param_typ.module_path.clone()
        } else {
            Some(Box::new(nodes::ModuleSpecifier::from_resolved(&self.module_stack)))
        };
        if let Type::Struct(name, _path) = &param_typ.typ {
            Type::Struct(name.clone(), module_path)
        } else if let Type::Ref(..) = &param_typ.typ {
            let mut under = param_typ.typ.clone();
            let underlying = under.get_underlying_type_ref_mut();
            if let Type::Struct(name, _) = underlying {
                *underlying = Type::Struct(name.clone(), module_path)
            }
            under.clone()
        } else {
            param_typ.typ.clone()
        }
    }
}
