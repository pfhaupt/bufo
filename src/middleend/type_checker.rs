use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::{Display, Formatter};

use crate::frontend::nodes;
use crate::frontend::parser::Operation;
use crate::frontend::tokens::{Location, KEYWORD_NULL};

use crate::compiler::{ERR_STR, WARN_STR, NOTE_STR};
use crate::internal_panic;
use crate::util::flags::Flags;
use crate::frontend::tokens::KEYWORD_BLANK;

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveKind {
    Integer,
    Reference,
    Float
}

macro_rules! check_function {
    ($tc:ident, $call_node:ident, $func_info:ident, $typ:expr) => {{
        let return_type = $func_info.return_type.t.clone();
        match $call_node.arguments.len().cmp(&$func_info.parameters.len()) {
            std::cmp::Ordering::Less => {
                $tc.report_error(TypeError::NotEnoughArguments(
                    $typ,
                    $call_node.location,
                    $call_node.function_name,
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
                        $call_node.function_name,
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
                    // debug_assert!(!expected.is_struct());
                } else if arg_type != expected {
                    $tc.report_error(TypeError::ArgParamTypeMismatch(
                        arg.get_loc(),
                        arg_type.clone(),
                        param.location,
                        param.name,
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
                    // debug_assert!(!expected.is_struct());
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
                name: param.name,
                location: param.location,
                typ: param.typ.typ.clone(),
                mut_state: MutState::mutable(param.is_mutable, param.typ.typ.is_mutable_ref()),
            };
            let index_of = parameters.iter().position(|p| p.name == param.name);
            if index_of.is_some() {
                let index_of = index_of.unwrap();
                let p2 = &parameters[index_of];
                errors.push(TypeError::Redeclaration(
                    "Parameter",
                    param.location,
                    param.name,
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
enum TypeError<'src> {
    /// Syntax: Decl Type, Error Loc, Name, Decl Loc
    Redeclaration(&'static str, Location, &'src str, Location),
    /// Syntax: Error Loc, Fn Name, Extern Loc
    ExternFunction(Location, &'src str, Location),
    /// Syntax: Error Loc, Type Name
    UnknownType(Location, Type<'src>),
    /// Syntax: Fn Type, Error Loc
    #[cfg(feature = "old_codegen")]
    TooManyParameters(&'static str, Location),
    /// Syntax: Error Loc, Expected, Found
    TypeMismatch(Location, Type<'src>, Type<'src>),
    /// Syntax: Error Loc, Binary Op, LHS Loc, LHS Type, RHS Loc, RHS Type
    BinaryTypeMismatch(Location, Operation, Location, Type<'src>, Location, Type<'src>),
    /// Syntax: Error Loc, Var Name
    UndeclaredVariable(Location, &'src str),
    /// Syntax: Error Loc, Fn Name
    UndeclaredFunction(Location, &'src str),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    NotEnoughArguments(&'static str, Location, &'src str, usize, Location, usize),
    /// Syntax: Fn Type, Error Loc, Fn Name, Arg Count, Fn Decl, Param Count
    TooManyArguments(&'static str, Location, &'src str, usize, Location, usize),
    /// Syntax: Arg Decl, Arg Type, Param Decl, Param Name, Param Type
    ArgParamTypeMismatch(Location, Type<'src>, Location, &'src str, Type<'src>),
    /// Syntax: Error Loc, Found Type, Decl Loc, Decl Type
    WrongReturnType(Location, Type<'src>, Location, Type<'src>),
    /// Syntax: Error Loc, Decl Loc, Decl Type
    MissingReturn(Location, Location, Type<'src>),
    /// Syntax: Error Loc, Var Decl, Var Name, Field Name, Struct Loc, Struct Name
    UnknownField(Location, &'src str, Location, &'src str),
    /// Syntax: Error Loc, Var Name, Struct Loc, Struct Name
    UnknownMethod(Location, &'src str, Location, &'src str),
    /// Syntax: Expected, Error Loc, Found Literal
    UnexpectedLiteral(&'static str, Location, &'src str),
    /// Syntax: Error Loc
    DotOnNonStruct(Location),
    /// Syntax: Error Loc
    InvalidLValue(Location),
    /// Syntax: Error Loc, Type
    NegationTypeMismatch(Location, Type<'src>),
    /// Syntax: Error Loc, Var Name, Decl Loc
    ImmutableModification(Location, &'src str, Location),
    /// Syntax: Error Loc
    CantMutateTemporary(Location),
    /// Syntax: Error Loc, Struct Name, Previous Struct Info
    RecursiveStruct(Location, String, Vec<(Location, String)>),
    /// Syntax: Error Loc, Field Name, Struct Loc, Struct Name
    MissingField(Location, &'src str, Location, &'src str),
    /// Syntax: Error Loc, Type, Type
    DereferenceTypeMismatch(Location, Type<'src>),
    /// Syntax: Error Loc
    DereferenceIntegerLiteral(Location),
    /// Syntax: Error Loc
    NestedReferenceNotAllowedYet(Location),
    /// Syntax: Kind, Error Loc, Function Name, Function Loc
    UnsafeCallInSafeContext(&'static str, Location, &'src str, Location),
    /// Syntax: Error Loc
    UnsafeAny(Location),
    /// Syntax: Error Loc
    UnsafeNull(Location),
    /// Syntax: Error Loc, Element Type, Inferred Loc, Inferred Type
    ArrayLiteralElementTypeMismatch(Location, Type<'src>, Location, Type<'src>),
    /// Syntax: Error Loc, Expected Size, Found Size
    ArraySizeMismatch(Location, usize, usize),
    /// Syntax: Error Loc, Type
    InvalidIndexedAccess(Location, Type<'src>),
    /// Syntax: Error Loc, Type
    ArrayIndexRequiresUsize(Location, Type<'src>),
    /// Syntax: Error Loc, Type
    LogicalNotTypeMismatch(Location, Type<'src>),
    /// Syntax: Error Loc, Message
    InvalidMemberAccess(Location, &'static str),
    /// Syntax: Error Loc
    ImmutDerefInMutContext(Location, Location),
    /// Syntax: Error Loc
    UnsafePointerArithmetics(Location),
    /// Syntax: Error Loc, Binary Op
    InvalidPointerArithmetics(Location, Operation),
    /// Syntax: Error Loc
    UnsafePointerCast(Location),
    /// Syntax: Error Loc, Expr Loc, Expr Type, Type Loc, Type
    NonPrimitiveTypeCast(Location, Location, Type<'src>, Location, Type<'src>),
    /// Syntax: Error Loc, Type
    BlankReference(Location, Type<'src>),
}

impl<'src> Display for TypeError<'src> {
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
                    NOTE_STR, lhs_loc, lhs_typ,
                    NOTE_STR, rhs_loc, rhs_typ,
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
            TypeError::UnsafeAny(loc) => {
                write!(f, "{}: {:?}: Use of `Any` is unsafe.\n{}: Use an `unsafe {{}}` block if you really want to use `Any`.", ERR_STR, loc, NOTE_STR)
            }
            TypeError::UnsafeNull(loc) => {
                write!(f, "{}: {:?}: Use of `{}` is unsafe.\n{}: Use an `unsafe {{}}` block if you really want to use `{}`.", ERR_STR, loc, KEYWORD_NULL, NOTE_STR, KEYWORD_NULL)
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
            TypeError::UnsafePointerCast(loc) => {
                write!(f, "{}: {:?}: Pointer type casts are unsafe.\n{}: Use an `unsafe {{}}` block if you really want to do that.", ERR_STR, loc, NOTE_STR)
            }
            TypeError::NonPrimitiveTypeCast(err, e_loc, e_type, t_loc, t_type) => {
                write!(
                    f,
                    "{}: {:?}: Non primitive cast from type {} to {}.\n{}: {:?}: Expression to cast is here.\n{}: {:?}: Type to cast to is here.",
                    ERR_STR, err, e_type, t_type, NOTE_STR, e_loc, NOTE_STR, t_loc
                )
            }
            TypeError::BlankReference(err, typ) => {
                write!(
                    f,
                    "{}: {:?}: Invalid initialization of reference of type {}. Use `{}` instead.",
                    ERR_STR, err, typ, KEYWORD_NULL
                )
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Type<'src> {
    None, // For functions that return nothing
    Any,  // C's void*, expected to be used for FFI, not for general use, infers to all references
    Blank,
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
    Struct(&'src str),
    // TODO: More unit tests for references
    Ref(Box<Type<'src>>, bool), // bool is mutability
    Array(Box<Type<'src>>, usize),
    // Reserved for later use
    F32,
    F64,
}

impl<'src> PartialEq for Type<'src> {
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
            (Type::F32, Type::F32) => true,
            (Type::F64, Type::F64) => true,
            (Type::Unknown, Type::Unknown) => true,
            (Type::None, Type::None) => true,
            (Type::Any, Type::Any) => true,
            (Type::Unknown, _) | (_, Type::Unknown) => false,
            (Type::None, _) | (_, Type::None) => false,
            (Type::Any, Type::Ref(_, _)) | (Type::Ref(_, _), Type::Any) => true, // Any is void*, so it can be inferred to any reference
            (Type::Any, _) | (_, Type::Any) => false,
            (Type::Struct(lhs), Type::Struct(rhs)) => lhs == rhs,
            (Type::Ref(lhs, l), Type::Ref(rhs, r)) => lhs == rhs && l == r,
            (Type::Array(lhs, l), Type::Array(rhs, r)) => lhs == rhs && l == r,
            (Type::Blank, Type::Blank) => true,
            _ => false,
        }
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl<'src> Type<'src> {
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
                | Type::Any
        )
    }

    #[trace_call(extra)]
    pub fn is_float(&self) -> bool {
        *self == Type::F32 || *self == Type::F64
    }

    #[trace_call(extra)]
    pub fn is_integer(&self) -> bool {
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
                | Type::Char
        )
    }

    #[trace_call(extra)]
    pub fn is_signed(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Usize => false,
            _ => internal_panic!("Expected integer")
        }
    }

    #[trace_call(extra)]
    pub fn is_compound(&self) -> bool {
        self.is_struct() || self.is_array() || self.is_struct_ref()
    }

    #[trace_call(extra)]
    pub fn is_struct_array(&self) -> bool {
        if let Type::Array(t, _) = self {
            t.is_struct() || t.is_struct_array()
        } else {
            false
        }
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
    pub fn is_reference(&self) -> bool {
        matches!(self, Type::Ref(..)) || *self == Type::Any
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

    pub fn get_underlying_type_ref_mut(&mut self) -> &mut Type<'src> {
        match self {
            Type::Ref(ref mut t, _) => t.get_underlying_type_ref_mut(),
            Type::Array(ref mut t, _) => t.get_underlying_type_ref_mut(),
            _ => self,
        }
    }

    #[trace_call(extra)]
    pub fn get_underlying_type(&self) -> &Type {
        match self {
            Type::Ref(t, _) => t.get_underlying_type(),
            Type::Array(t, _) => t.get_underlying_type(),
            _ => self,
        }
    }

    #[trace_call(extra)]
    pub fn get_underlying_struct_name(&self) -> &'src str {
        match self {
            Type::Struct(struct_name) => struct_name,
            Type::Array(t, _) => t.get_underlying_struct_name(),
            _ => internal_panic!("Expected Struct")
        }
    }

    #[trace_call(extra)]
    pub fn get_bit_size(&self) -> usize {
        match self {
            Type::I8 | Type::U8 | Type::Char => 8,
            Type::I16 | Type::U16 => 16,
            Type::I32 | Type::U32 | Type::F32 => 32,
            Type::I64 | Type::U64 | Type::F64 | Type::Usize => 64,
            _ => {
                debug_assert!(self.is_primitive());
                internal_panic!("Expected primitive type, got {self}")
            }
        }
    }

    #[trace_call(extra)]
    pub fn get_primitive_kind(&self) -> PrimitiveKind {
        if self.is_integer() { PrimitiveKind::Integer }
        else if self.is_reference() { PrimitiveKind::Reference }
        else if self.is_float() { PrimitiveKind::Float }
        else {
            internal_panic!("Unhandled case in Type::get_primitive_kind")
        }
    }
}

impl<'src> Display for Type<'src> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Struct(str) => write!(fmt, "{}", str),
            Type::Ref(t, true) => write!(fmt, "&mut {}", t),
            Type::Ref(t, false) => write!(fmt, "&{}", t),
            Type::Array(t, len) => write!(fmt, "[{}; {}]", t, len),
            Type::Any => write!(fmt, "Any"),
            _ => write!(fmt, "{}", format!("{:?}", self).to_lowercase()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct TypeLoc<'src> {
    t: Type<'src>,
    l: Location,
}

impl<'src> TypeLoc<'src> {
    #[trace_call(extra)]
    fn new(l: Location, t: Type<'src>) -> Self {
        Self { l, t }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Function<'src> {
    location: Location,
    return_type: TypeLoc<'src>,
    parameters: Vec<Variable<'src>>,
    has_this: bool,
    is_unsafe: bool,
    is_vararg: bool,
    is_extern: bool,
    is_used: bool,
}

impl<'src> Function<'src> {
    #[trace_call(extra)]
    fn get_parameters_as_hashmap(&self) -> HashMap<&'src str, Variable<'src>> {
        let mut parameters = HashMap::new();
        for param in &self.parameters {
            parameters.insert(param.name, param.clone());
        }
        parameters
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct<'src> {
    name: &'src str,
    location: Location,
    fields: HashMap<&'src str, TypeLoc<'src>>,
    known_methods: HashMap<&'src str, Function<'src>>,
}

impl<'src> Struct<'src> {
    #[trace_call(extra)]
    fn new(name: &'src str, location: Location) -> Self {
        Self {
            name,
            location,
            fields: HashMap::new(),
            known_methods: HashMap::new(),
        }
    }

    #[trace_call(extra)]
    fn add_field(&mut self, field: &nodes::FieldNode<'src>) -> Result<(), TypeError<'src>> {
        let name = &field.name;
        let location = &field.location;

        match self.fields.get(name) {
            Some(f) => Err(TypeError::Redeclaration(
                "Field",
                *location,
                name,
                f.l,
            )),
            None => {
                let typ = field.type_def.typ.clone();
                self.fields.insert(name, TypeLoc::new(*location, typ.clone()));
                Ok(())
            }
        }
    }

    #[trace_call(extra)]
    fn add_method(&mut self, method: &nodes::MethodNode<'src>) -> Result<(), Vec<TypeError<'src>>> {
        let mut errors = Vec::new();
        let name = &method.name;
        let location = &method.location;
        let return_loc = method.return_type.location;
        let (parameters, mut param_errors) = check_parameters!(tc, method);
        errors.append(&mut param_errors);
        let func = Function {
            location: method.location,
            return_type: TypeLoc::new(return_loc, method.return_type.typ.clone()),
            parameters,
            has_this: true,
            is_unsafe: method.is_unsafe,
            is_vararg: false,
            is_extern: false,
            is_used: false,
        };
        if self.known_methods.contains_key(name) {
            let m = &self.known_methods[name];
            errors.push(TypeError::Redeclaration(
                "Method",
                *location,
                name,
                m.location,
            ));
        } else {
            self.known_methods.insert(name, func);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    #[trace_call(extra)]
    fn get_field(&self, name: &str) -> Option<TypeLoc<'src>> {
        self.fields.get(name).cloned()
    }

    #[trace_call(extra)]
    fn get_method(&self, name: &str) -> Option<&Function<'src>> {
        self.known_methods.get(name)
    }

    fn get_method_mut(&mut self, name: &str) -> Option<&mut Function<'src>> {
        self.known_methods.get_mut(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'src> {
    name: &'src str,
    location: Location,
    typ: Type<'src>,
    mut_state: MutStateVal,
}

impl<'src> Variable<'src> {
    #[trace_call(extra)]
    fn new(name: &'src str, location: Location, typ: Type<'src>, mut_state: MutStateVal) -> Self {
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

#[derive(Debug)]
pub struct TypeChecker<'flags, 'src> {
    externs: HashMap<&'src str, Function<'src>>,
    struct_indices: HashMap<&'src str, usize>,
    structs: Vec<Struct<'src>>,
    functions: HashMap<&'src str, Function<'src>>,
    known_variables: VecDeque<HashMap<&'src str, Variable<'src>>>,
    unsafe_depth: usize,
    #[cfg(feature = "old_codegen")]
    current_stack_size: usize,
    errors: Vec<TypeError<'src>>,
    flags: &'flags Flags,
}

impl<'flags, 'src> TypeChecker<'flags, 'src> {
    #[trace_call(extra)]
    pub fn new(flags: &'flags Flags) -> Self {
        let mut known_variables = VecDeque::new();
        known_variables.push_back(HashMap::new()); // Global variables
        Self {
            externs: HashMap::new(),
            struct_indices: HashMap::new(),
            structs: Vec::new(),
            functions: HashMap::new(),
            known_variables,
            unsafe_depth: 0,
            #[cfg(feature = "old_codegen")]
            current_stack_size: 0,
            errors: Vec::new(),
            flags,
        }
    }
    #[trace_call(extra)]
    fn get_struct(&self, name: &str) -> Option<&Struct<'src>> {
        match self.struct_indices.get(name) {
            Some(index) => Some(&self.structs[*index]),
            None => None,
        }
    }

    fn get_struct_mut(&mut self, name: &str) -> Option<&mut Struct<'src>> {
        match self.struct_indices.get(name) {
            Some(index) => Some(&mut self.structs[*index]),
            None => None,
        }
    }

    #[trace_call(extra)]
    fn insert_struct(&mut self, strukt: Struct<'src>) {
        let index = self.structs.len();
        self.struct_indices.insert(strukt.name, index);
        self.structs.push(strukt.clone());
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
    fn get_function(&self, name: &str) -> Option<&Function<'src>> {
        self.functions.get(name)
    }

    #[trace_call(extra)]
    fn get_extern(&self, name: &str) -> Option<&Function<'src>> {
        self.externs.get(name)
    }

    fn get_function_mut(&mut self, name: &str) -> Option<&mut Function<'src>> {
        self.functions.get_mut(name)
    }

    fn get_extern_mut(&mut self, name: &str) -> Option<&mut Function<'src>> {
        self.externs.get_mut(name)
    }

    #[trace_call(always)]
    fn add_extern(&mut self, extern_node: &nodes::ExternNode<'src>) -> Vec<TypeError<'src>> {
        let return_type = &extern_node.return_type.typ;
        let return_loc = extern_node.return_type.location;
        let (parameters, errors) = check_parameters!(tc, extern_node);
        let func = Function {
            location: extern_node.location,
            return_type: TypeLoc::new(return_loc, return_type.clone()),
            parameters,
            has_this: false,
            is_unsafe: extern_node.is_unsafe,
            is_vararg: extern_node.is_vararg,
            is_extern: true,
            is_used: true,
        };
        self.externs.insert(extern_node.name, func);
        errors
    }

    #[trace_call(always)]
    fn add_struct(&mut self, struct_node: &nodes::StructNode<'src>) -> Vec<TypeError<'src>> {
        let mut errors = Vec::new();
        let name = struct_node.name;
        let location = struct_node.location;
        let mut strukt = Struct::new(name, location);
        for field in &struct_node.fields {
            match strukt.add_field(field) {
                Ok(()) => (),
                Err(e) => errors.push(e),
            }
        }
        for method in &struct_node.methods {
            match strukt.add_method(method) {
                Ok(()) => (),
                Err(e) => errors.extend(e),
            }
        }
        if self.has_struct(&name) {
            let strukt = self.get_struct(&name).unwrap();
            errors.push(TypeError::Redeclaration(
                "Struct",
                location,
                name,
                strukt.location,
            ));
        } else {
            self.insert_struct(strukt);
        }
        errors
    }

    #[trace_call(always)]
    fn add_function(&mut self, function: &nodes::FunctionNode<'src>) -> Vec<TypeError<'src>> {
        let name = function.name;
        let location = function.location;
        let return_type = &function.return_type;
        let return_loc = function.return_type.location;
        let (parameters, mut errors) = check_parameters!(tc, function);
        let func = Function {
            location,
            return_type: TypeLoc::new(return_loc, return_type.typ.clone()),
            parameters,
            has_this: false,
            is_unsafe: function.is_unsafe,
            is_vararg: false,
            is_extern: false,
            is_used: function.name == "main" || function.name == "index_oob",
        };
        if self.externs.contains_key(&name) {
            let external = &self.externs[&name];
            errors.push(TypeError::ExternFunction(
                location,
                name,
                external.location,
            ));
        }
        if self.functions.contains_key(&name) {
            let f = &self.functions[&name];
            errors.push(TypeError::Redeclaration(
                "Function",
                location,
                name,
                f.location,
            ));
        } else {
            self.functions.insert(name, func);
        }
        errors
    }

    #[trace_call(always)]
    fn fill_lookup(&mut self, project: &nodes::FileNode<'src>) {
        for extern_node in &project.externs {
            let extern_errors = self.add_extern(extern_node);
            for error in extern_errors {
                self.report_error(error);
            }
        }
        for strukt in &project.structs {
            let struct_errors = self.add_struct(strukt);
            for error in struct_errors {
                self.report_error(error);
            }
        }
        for func in &project.functions {
            let func_errors = self.add_function(func);
            for error in func_errors {
                self.report_error(error);
            }
        }
    }

    #[trace_call(always)]
    fn report_error(&mut self, error: TypeError<'src>) {
        if self.flags.debug {
            println!("[DEBUG] Error: {}", error);
        }
        trace_panic!();
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

    fn get_current_scope(&mut self) -> &mut HashMap<&'src str, Variable<'src>> {
        let len = self.known_variables.len();
        &mut self.known_variables[len - 1]
    }

    #[trace_call(extra)]
    fn get_variable_in_current_scope(&self, name: &str) -> Option<Variable<'src>> {
        self.known_variables[self.known_variables.len() - 1]
            .get(name)
            .cloned()
    }

    #[trace_call(extra)]
    fn get_variable(&self, name: &str) -> Option<Variable<'src>> {
        for scope in self.known_variables.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    fn dfs<'s>(strukt: &'s str, lookup: &BTreeMap<&'s str, BTreeSet<&'s str>>, visited: &mut Vec<&'s str>, finished: &mut BTreeSet<&'s str>) -> bool {
        if finished.contains(strukt) {
            return false;
        }
        if visited.contains(&strukt) {
            return true;
        }
        visited.push(strukt);
        let mut done = false;
        for neighbor in lookup.get(strukt).unwrap() {
            if Self::dfs(neighbor, lookup, visited, finished) {
                done = true;
            }
        }
        finished.insert(strukt);
        done
    }

    #[trace_call(always)]
    fn find_recursive_structs<'s>(&mut self, current_module: &'s nodes::FileNode<'src>) {
        /*
        Adapted version of https://en.wikipedia.org/wiki/Cycle_(graph_theory)#Algorithm
        Key difference is that we also keep track of where we went, for better error reporting
         */
        let all_structs = current_module.get_all_structs();
        if all_structs.is_empty() {
            return;
        }
        let mut lookup: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
        for strukt in &all_structs {
            let strukt_name = strukt.get_full_name();
            let mut fields = BTreeSet::new();
            for field in &strukt.fields {
                let f_type = &field.type_def.typ;
                if f_type.is_struct() || f_type.is_struct_array() {
                    fields.insert(f_type.get_underlying_struct_name());
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
            if Self::dfs(strukt, &lookup, &mut visited, &mut finished) {
                cycles.push(visited.clone());
            }
            visited.clear();
            finished.clear();
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
                name.to_string(),
                cycle.iter().map(|name| {
                    let strukt = all_structs.iter().find(|e|e.get_full_name() == *name).unwrap();
                    (strukt.location, name.to_string())
                }).collect()
            ));
        }
    }

    #[trace_call(always)]
    fn warn_and_remove_unused(&self, project: &mut nodes::FileNode<'src>) {
        let mut unused = vec![];
        for (i, extern_node) in project.externs.iter().enumerate() {
            let Some(func) = self.get_extern(extern_node.name) else {
                unreachable!()
            };
            if !func.is_used {
                unused.push(i);
                eprintln!(
                    "{}: {:?}: Unused external function {}",
                    WARN_STR,
                    extern_node.location,
                    extern_node.name
                );
            }
        }
        for i in unused.iter().rev() {
            project.externs.remove(*i);
        }
        for (_i, struct_node) in project.structs.iter_mut().enumerate() {
            let Some(strukt) = self.get_struct(struct_node.name) else {
                unreachable!()
            };
            unused.clear();
            for (i, method_node) in struct_node.methods.iter().enumerate() {
                let Some(method) = strukt.get_method(&method_node.name) else {
                    unreachable!()
                };
                if !method.is_used {
                    unused.push(i);
                    eprintln!(
                        "{}: {:?}: Unused method {}",
                        WARN_STR,
                        method_node.location,
                        method_node.get_full_name()
                    );
                }
            }
            for i in unused.iter().rev() {
                struct_node.methods.remove(*i);
            }
        }
        unused.clear();
        for (i, func_node) in project.functions.iter().enumerate() {
            let Some(func) = self.get_function(func_node.name) else {
                unreachable!()
            };
            if !func.is_used {
                unused.push(i);
                eprintln!(
                    "{}: {:?}: Unused function {}",
                    WARN_STR,
                    func_node.location,
                    func_node.get_full_name()
                );
            }
        }
        for i in unused.iter().rev() {
            project.functions.remove(*i);
        }
    }

    // #[trace_call(always)]
    pub fn type_check_project<'s>(&mut self, project: &'s mut nodes::FileNode<'src>) -> Result<(), String> {
        macro_rules! perform_step {
            ($step:expr) => {
                {
                    let res = $step;
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
        perform_step!(self.fill_lookup(project));
        perform_step!(self.type_check_file(project));
        perform_step!(self.find_recursive_structs(project));
        perform_step!(self.warn_and_remove_unused(project));
        Ok(())
    }

    #[trace_call(always)]
    fn type_check_file(&mut self, module: &mut nodes::FileNode<'src>) {
        for global in &mut module.globals {
            self.type_check_stmt_var_decl(global);
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
    }

    #[trace_call(always)]
    fn type_check_extern(&mut self, extern_node: &mut nodes::ExternNode<'src>) {
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
    fn type_check_struct(&mut self, struct_node: &mut nodes::StructNode<'src>) {
        for field in &mut struct_node.fields {
            self.type_check_field(field);
        }
        for method in &mut struct_node.methods {
            self.type_check_method(method, &struct_node.name);
        }
    }

    #[trace_call(always)]
    fn type_check_field(&mut self, field: &mut nodes::FieldNode<'src>) {
        self.type_check_type_node(&mut field.type_def)
    }

    #[trace_call(always)]
    fn type_check_method(&mut self, method: &mut nodes::MethodNode<'src>, struct_name: &str) {
        debug_assert!(self.known_variables.len() == 1); // Global variables
        debug_assert!(self.has_struct(&struct_name));
        #[cfg(feature = "old_codegen")]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut method.parameters {
            self.type_check_parameter(param);
        }

        let Some(struct_info) = self.get_struct(&struct_name) else {
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

        self.known_variables.pop_back();
        debug_assert!(self.known_variables.len() == 1); // Global variables
    }

    #[trace_call(always)]
    fn type_check_function(&mut self, function: &mut nodes::FunctionNode<'src>) {
        debug_assert!(self.known_variables.len() == 1); // Global variables
        debug_assert!(self.has_function(&function.name));
        #[cfg(feature = "old_codegen")]
        debug_assert!(self.current_stack_size == 0);

        for param in &mut function.parameters {
            self.type_check_parameter(param);
        }

        let function_info = self.get_function(&function.name).unwrap();

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

        self.known_variables.pop_back();
        debug_assert!(self.known_variables.len() == 1); // Global variables
    }

    #[trace_call(always)]
    fn type_check_parameter(&mut self, parameter: &mut nodes::ParameterNode<'src>) {
        self.type_check_type_node(&mut parameter.typ);
        #[cfg(feature = "old_codegen")]
        {
            let var_size = parameter.typ.typ.size();
            self.current_stack_size += var_size;
        }
    }

    #[trace_call(always)]
    fn type_check_block(&mut self, block: &mut nodes::BlockNode<'src>) {
        self.add_scope(block.is_unsafe);
        for statement in &mut block.statements {
            self.type_check_statement(statement);
        }
        self.remove_scope(block.is_unsafe);
    }

    #[trace_call(always)]
    fn type_check_statement(&mut self, statement: &mut nodes::Statement<'src>) {
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
    fn type_check_stmt_var_decl(&mut self, let_node: &mut nodes::VarDeclNode<'src>) {
        match self.get_variable_in_current_scope(&let_node.name) {
            Some(var) => {
                self.report_error(TypeError::Redeclaration(
                    "Variable",
                    let_node.location,
                    let_node.name,
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
                    name: let_node.name,
                    location: let_node.location,
                    typ: let_node.typ.typ.clone(),
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
                if current_scope.insert(let_node.name, var.clone()).is_some() {
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
    fn type_check_stmt_if(&mut self, if_node: &mut nodes::IfNode<'src>) {
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
    fn type_check_stmt_return(&mut self, return_node: &mut nodes::ReturnNode<'src>) {
        debug_assert!(return_node.typ == Type::Unknown);

        let (expected_return_type, location) = if return_node.strukt.is_none() {
            // We're returning from a normal function
            let Some(function) = self.get_function(&return_node.function) else {
                unreachable!()
            };
            let expected_return_type = function.return_type.t.clone();
            let location = function.location;
            debug_assert!(expected_return_type != Type::Unknown);
            (expected_return_type, location)
        } else {
            // We're returning from a method or feature
            let Some(strukt) = self.get_struct(return_node.strukt.as_ref().unwrap()) else { unreachable!() };
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
    fn type_check_stmt_while(&mut self, while_node: &mut nodes::WhileNode<'src>) {
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
        if let Some(ref mut step) = while_node.step {
            let _ = self.type_check_expression(step, MutState::Immut);
        }
    }

    #[trace_call(always)]
    fn type_check_stmt_break(&mut self, _break_node: &mut nodes::BreakNode) {}

    #[trace_call(always)]
    fn type_check_stmt_continue(&mut self, _continue_node: &mut nodes::ContinueNode) {}

    #[trace_call(always)]
    fn type_check_expression(
        &mut self,
        expression: &mut nodes::Expression<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
        let _error_loc = expression.get_loc();
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
            nodes::Expression::As(expr, typ) => {
                self.type_check_type_node(typ);
                if typ.typ == Type::Unknown {
                    return Err(());
                }
                let expr_type = self.type_check_expression(expr, mut_state)?;
                if expr_type == Type::Unknown {
                    todo!() // Can't cast Type::Unknown to anything
                }
                let new_type = typ.typ.clone();
                match (&expr_type, &new_type) {
                    (Type::Array(_, _), _) | (_, Type::Array(_, _)) | (Type::Struct(_), _) | (_, Type::Struct(_)) => {
                        self.report_error(TypeError::NonPrimitiveTypeCast(
                            _error_loc,
                            expr.get_loc(),
                            expr.get_type(),
                            typ.location,
                            typ.typ.clone()
                        ));
                    },
                    (Type::Ref(_, _), other) | (other, Type::Ref(_, _)) => {
                        if *other != Type::Any && *other != Type::Usize && other.is_reference() {
                            self.report_error(TypeError::NonPrimitiveTypeCast(
                                _error_loc,
                                expr.get_loc(),
                                expr.get_type(),
                                typ.location,
                                typ.typ.clone()
                            ))
                        }
                        if self.unsafe_depth == 0 {
                            self.report_error(TypeError::UnsafePointerCast(
                                _error_loc,
                            ))
                        }
                    },
                    (Type::Usize, Type::Any) | (Type::Any, Type::Usize) => {
                        if self.unsafe_depth == 0 {
                            self.report_error(TypeError::UnsafePointerCast(
                                _error_loc,
                            ))
                        }
                    }
                    (Type::Any, _) | (_, Type::Any) => todo!(), // Unsafe Operation
                    (e, n) => {
                        debug_assert!(e.is_primitive());
                        debug_assert!(n.is_primitive());
                        let from_size = e.get_bit_size();
                        let to_size = n.get_bit_size();
                        if to_size < from_size {
                            eprintln!("{}: {:?}: Lossy type cast: Target type ({}) is smaller than original type ({}).", WARN_STR, _error_loc, n, e);
                        }
                    }
                }
                Ok(new_type)
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expression_with_type(
        &mut self,
        expression: &mut nodes::Expression<'src>,
        typ: &Type<'src>,
    ) -> Result<Type<'src>, ()> {
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
                if lit_node.typ == Type::Blank {
                    if typ.is_integer() || typ.is_float() {
                        lit_node.value = "0";
                        eprintln!(
                            "{}: {:?}: Using `{}` to initialize value of type {}. Please use `0` instead.",
                            WARN_STR,
                            lit_node.location,
                            KEYWORD_BLANK,
                            typ
                        );
                    } else if typ.is_reference() {
                        self.report_error(TypeError::BlankReference(
                            lit_node.location,
                            typ.clone()
                        ));
                        return Err(());
                    }
                    lit_node.typ = typ.clone();
                    Ok(typ.clone())
                } else if lit_node.typ != Type::Unknown && lit_node.typ != *typ {
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
                        lit_node.value,
                    ));
                    Err(())
                } else if let Type::Array(_, _) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "array",
                        lit_node.location,
                        lit_node.value,
                    ));
                    Err(())
                } else if let Type::Struct(_) = typ {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "struct instance",
                        lit_node.location,
                        lit_node.value,
                    ));
                    Err(())
                } else if *typ == Type::Bool {
                    self.report_error(TypeError::UnexpectedLiteral(
                        "boolean",
                        lit_node.location,
                        lit_node.value,
                    ));
                    Err(())
                } else {
                    lit_node.typ = typ.clone();
                    Ok(typ.clone())
                }
            }
            nodes::Expression::Name(name_node) => {
                if name_node.typ == Type::Unknown {
                    return Err(());
                }
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
                        if expr_type != Type::I8 && expr_type != Type::I16
                        && expr_type != Type::I32 && expr_type != Type::I64
                        && expr_type != Type::F32 && expr_type != Type::F64 {
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
                        if expr_type == Type::Unknown {
                            return Err(())
                        }
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
        indexed_access: &mut nodes::Expression<'src>,
        typ: &Type<'src>,
    ) -> Result<Type<'src>, ()> {
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
                        name_node.name,
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
        name_node: &mut nodes::NameNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
        let var = self.get_variable(&name_node.name);
        match var {
            Some(var) => {
                if mut_state != MutState::Immut && mut_state & var.mut_state == 0 {
                    // mut_state & var.mut_state == 0 means mismatch in required mutability
                    // e.g. if mut_state = MutRef, that means original Variable isn't MutRef
                    // (it still might be MutVar, but in this case that doesn't matter)
                    self.report_error(TypeError::ImmutableModification(
                        name_node.location,
                        name_node.name,
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
                    name_node.name,
                ));
                Err(())
            }
        }
    }

    #[trace_call(always)]
    fn type_check_expr_unary(&mut self, unary_expr: &mut nodes::UnaryNode<'src>, mut_state: MutStateVal) -> Result<Type<'src>, ()> {
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
        binary_expr: &mut nodes::BinaryNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_binary_logical(&mut self, binary_expr: &mut nodes::BinaryNode<'src>) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_binary_bitwise(&mut self, binary_expr: &mut nodes::BinaryNode<'src>) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_binary_arithmetic(&mut self, binary_expr: &mut nodes::BinaryNode<'src>) -> Result<Type<'src>, ()> {
        assert!(binary_expr.is_arithmetic());
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, MutState::Immut)?;
        let rhs_type = self.type_check_expression(&mut binary_expr.rhs, MutState::Immut)?;
        match (&lhs_type, &rhs_type) {
            (Type::Ref(lhs_t, _), Type::Ref(rhs_t, _)) => {
                if lhs_t != rhs_t {
                    self.report_error(TypeError::BinaryTypeMismatch(
                        binary_expr.location,
                        binary_expr.operation.clone(),
                        binary_expr.lhs.get_loc(),
                        lhs_type.clone(),
                        binary_expr.rhs.get_loc(),
                        rhs_type.clone(),
                    ));
                    return Err(())
                }
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
                binary_expr.typ = Type::Usize;
                Ok(Type::Usize)
            }
            (typ @ Type::Ref(..), other @ Type::Usize) | (typ @ Type::Ref(..), other @ Type::Unknown) => {
                if *other == Type::Unknown {
                    self.type_check_expression_with_type(&mut binary_expr.rhs, &Type::Usize)?;
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
            (other @ Type::Usize, typ @ Type::Ref(..)) | (other @ Type::Unknown, typ @ Type::Ref(..)) => {
                if *other == Type::Unknown {
                    self.type_check_expression_with_type(&mut binary_expr.lhs, &Type::Usize)?;
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
    fn type_check_expr_binary_comparison(&mut self, binary_expr: &mut nodes::BinaryNode<'src>) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_assign(&mut self, assign_expr: &mut nodes::BinaryNode<'src>) -> Result<Type<'src>, ()> {
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
        binary_expr: &mut nodes::BinaryNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
        let lhs_type = self.type_check_expression(&mut binary_expr.lhs, mut_state)?;
        let (is_ref, strukt) = match &lhs_type {
            Type::Ref(orig_type, _) => {
                let Type::Struct(ref struct_name) = **orig_type else {
                    self.report_error(TypeError::DotOnNonStruct(binary_expr.lhs.get_loc()));
                    return Err(());
                };
                let Some(strukt) = self.get_struct(struct_name) else {
                    self.report_error(TypeError::UnknownType(
                        binary_expr.lhs.get_loc(),
                        (**orig_type).clone()
                    ));
                    return Err(());
                };
                (true, strukt)
            }
            ref struct_type @ Type::Struct(ref struct_name) => {
                let Some(strukt) = self.get_struct(struct_name) else {
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
                        name_node.name,
                        strukt.location,
                        strukt.name,
                    ));
                    Err(())
                }
            }
            nodes::Expression::FunctionCall(call_node) => {
                let Some(method) = strukt.get_method(&call_node.function_name) else {
                    self.report_error(TypeError::UnknownMethod(
                        call_node.location,
                        call_node.function_name,
                        strukt.location,
                        strukt.name,
                    ));
                    return Err(());
                };
                if let Some(strukt) = self.get_struct_mut(strukt.name) {
                    let Some(method) = strukt.get_method_mut(call_node.function_name) else {
                        unreachable!()
                    };
                    method.is_used = true;
                }

                if method.is_unsafe && self.unsafe_depth == 0 {
                    self.report_error(TypeError::UnsafeCallInSafeContext(
                        "Method",
                        call_node.location,
                        call_node.function_name,
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
        binary_expr: &mut nodes::BinaryNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_literal(&mut self, literal: &mut nodes::LiteralNode<'src>) -> Result<Type<'src>, ()> {
        // Need to use `matches` because `==` treats `Type::Any` as a wildcard for reference types
        if matches!(literal.typ, Type::Any) && self.unsafe_depth == 0 {
            debug_assert!(literal.value == KEYWORD_NULL);
            // Unsafe to use `null` outside of unsafe block
            self.report_error(TypeError::UnsafeNull(literal.location));
            Err(())
        } else if literal.typ == Type::Blank {
            Ok(Type::Unknown)
        } else {
            Ok(literal.typ.clone())
        }
    }

    #[trace_call(always)]
    fn type_check_expr_struct_literal(
        &mut self,
        literal: &mut nodes::StructLiteralNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
        if mut_state != MutState::Immut {
            self.report_error(TypeError::CantMutateTemporary(literal.location));
        }
        let Some(strukt) = self.get_struct(&literal.struct_name) else {
            self.report_error(TypeError::UnknownType(
                literal.location,
                Type::Struct(literal.struct_name),
            ));
            return Err(());
        };
        let strukt = strukt.clone();
        self.type_check_struct_literal_fields(literal, &strukt)?;
        literal.typ = Type::Struct(literal.struct_name);
        Ok(literal.typ.clone())
    }

    #[trace_call(always)]
    fn type_check_struct_literal_fields(
        &mut self,
        literal: &mut nodes::StructLiteralNode<'src>,
        strukt: &Struct<'src>,
    ) -> Result<(), ()> {
        let mut fields = HashMap::new();
        for field in &mut literal.fields {
            let Some(field_info) = strukt.get_field(&field.0) else {
                self.report_error(TypeError::UnknownField(
                    field.1.get_loc(),
                    field.0,
                    strukt.location,
                    strukt.name,
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
            fields.insert(field.0, field_type);
        }
        let mut errors = vec![];
        for field in strukt.fields.iter() {
            if !fields.contains_key(field.0) {
                errors.push(TypeError::MissingField(
                    literal.location,
                    field.0,
                    strukt.location,
                    literal.struct_name,
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
        literal: &mut nodes::ArrayLiteralNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
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
    fn type_check_expr_function_call(
        &mut self,
        func_call: &mut nodes::CallNode<'src>,
        mut_state: MutStateVal,
    ) -> Result<Type<'src>, ()> {
        if mut_state == MutState::MutVar {
            self.report_error(TypeError::CantMutateTemporary(
                func_call.location,
            ));
        }
        if let Some(e) = self.get_extern_mut(&func_call.function_name) {
            e.is_used = true;
        } else if let Some(f) = self.get_function_mut(&func_call.function_name) {
            f.is_used = true;
        } else {
            self.report_error(TypeError::UndeclaredFunction(
                func_call.location,
                func_call.function_name
            ));
            return Err(());
        }
        let function;
        if let Some(e) = self.get_extern(&func_call.function_name) {
            function = e;
        } else if let Some(f) = self.get_function(&func_call.function_name) {
            function = f;
        } else {
            unreachable!()
        }
        func_call.is_extern = function.is_extern;
        if function.is_unsafe && self.unsafe_depth == 0 {
            self.report_error(TypeError::UnsafeCallInSafeContext(
                "Function",
                func_call.location,
                func_call.function_name,
                function.location,
            ));
            return Err(());
        }
        let return_type = function.return_type.clone();
        if let Type::Struct(struct_name) = &return_type.t {
            if self.get_struct(struct_name).is_none() {
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
    fn type_check_type_node(&mut self, type_node: &mut nodes::TypeNode<'src>) {
        let mut bottom_type = &mut type_node.typ;
        let bt = bottom_type.clone();
        match bottom_type {
            Type::Struct(name) => {
                if !self.has_struct(name) {
                    self.report_error(TypeError::UnknownType(
                        type_node.location,
                        bottom_type.clone(),
                    ));
                    type_node.typ = Type::Unknown;
                    return;
                }
                type_node.typ = Type::Struct(name);
            },
            ref mut typ @ Type::Array(..)
            | ref mut typ @ Type::Ref(..) => {
                let underlying = typ.get_underlying_type_ref_mut();
                if let Type::Struct(name) = underlying {
                    if !self.has_struct(name) {
                        self.report_error(TypeError::UnknownType(
                            type_node.location,
                            bt,
                        ));
                        *underlying = Type::Unknown;
                        return;
                    }
                    *underlying = Type::Struct(name);
                }
            },
            t => {
                debug_assert!(!t.is_compound(), "{t}");
            }
        }
    }
}
