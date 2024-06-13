use std::fs;

use crate::frontend::parser::{Location, Operation};
use crate::middleend::type_checker::Type;

use tracer::trace_call;

use super::parser;

macro_rules! fn_is_inlinable {
    ($func:ident) => {
        {
            $func.block.total_len() < 3
        }
    };
}

#[derive(Debug, Clone)]
pub struct FileNode {
    pub externs: Vec<ExternNode>,
    pub structs: Vec<StructNode>,
    pub functions: Vec<FunctionNode>,
    pub compiler_flags: CompilerFlagsNode,
}

impl FileNode {
    #[trace_call(extra)]
    pub fn get_all_structs(&self) -> Vec<&StructNode> {
        let mut structs = vec![];
        for strukt in &self.structs {
            structs.push(strukt);
        }
        structs
    }

    #[trace_call(always)]
    pub fn merge_file(&mut self, other: FileNode) -> Result<(), Vec<parser::ParserError>> {
        let mut errors = vec![];
        for oe in &other.externs {
            if let Some(elem) = self.externs.iter().find(|e|e.name == oe.name) {
                errors.push(parser::ParserError::Redeclaration(
                    "Extern",
                    elem.name.clone(),
                    oe.location.clone(),
                    elem.location.clone(),
                ));
            }
        }
        self.externs.extend(other.externs);
        for os in &other.structs {
            if let Some(elem) = self.structs.iter().find(|e|e.name == os.name) {
                errors.push(parser::ParserError::Redeclaration(
                    "Struct",
                    elem.name.clone(),
                    os.location.clone(),
                    elem.location.clone(),
                ));
            }
        }
        self.structs.extend(other.structs);
        for of in &other.functions {
            if let Some(elem) = self.functions.iter().find(|e|e.name == of.name) {
                errors.push(parser::ParserError::Redeclaration(
                    "Function",
                    elem.name.clone(),
                    of.location.clone(),
                    elem.location.clone(),
                ));
            }
        }
        self.functions.extend(other.functions);
        for oc in &other.compiler_flags.flags {
            if self.compiler_flags.flags.iter().find(|e|e.to_vec() == oc.to_vec()).is_some() {
                todo!()
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }
}


#[allow(unused)]
#[derive(Debug, Clone)]
pub enum CompilerFlag {
    LibPath(Location, String),
    Library(Location, String),
    Linker(Location, String),
}

impl CompilerFlag {
    #[trace_call(extra)]
    pub fn from(location: Location, flag: String, value: String) -> Result<Self, String> {
        match flag.as_str() {
            "library" => Ok(Self::Library(location, value)),
            "libpath" => {
                if fs::metadata(&value).is_err() {
                    return Err(format!("{:?}: Library path '{}' does not exist", location, value));
                }
                Ok(Self::LibPath(location, value))
            },
            "linker" => Ok(Self::Linker(location, value)),
            _ => Err(format!("{:?}: Unknown compiler flag '{}'", location, flag)),
        }
    }

    #[trace_call(extra)]
    pub fn to_vec(&self) -> Vec<String> {
        match self {
            Self::LibPath(_, value) => vec![format!("-L{}", value.clone())],
            Self::Library(_, value) => vec![format!("-l{}", value.clone())],
            Self::Linker(_, value) => vec!["-Xlinker".to_string(), value.clone()],
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct CompilerFlagsNode {
    pub location: Location,
    pub flags: Vec<CompilerFlag>,
}

#[derive(Debug, Clone)]
pub struct ExternNode {
    pub location: Location,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub is_unsafe: bool,
    pub is_vararg: bool,
}

#[derive(Debug, Clone)]
pub struct StructNode {
    pub location: Location,
    pub name: String,
    pub fields: Vec<FieldNode>,
    pub methods: Vec<MethodNode>,
}

impl StructNode {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct FieldNode {
    pub location: Location,
    pub name: String,
    pub type_def: TypeNode,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub location: Location,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub is_unsafe: bool,
    pub is_vararg: bool,
    #[cfg(feature = "old_codegen")]
    pub stack_size: usize,
}

impl FunctionNode {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> String {
        self.name.clone()
    }

    pub fn is_inlinable(&self) -> bool {
        fn_is_inlinable!(self)
    }
}

#[derive(Debug, Clone)]
pub struct MethodNode {
    pub location: Location,
    pub struct_name: String,
    pub name: String,
    pub return_type: TypeNode,
    pub parameters: Vec<ParameterNode>,
    pub block: BlockNode,
    pub is_unsafe: bool,
    pub is_vararg: bool,
    #[cfg(feature = "old_codegen")]
    pub stack_size: usize,
}

impl MethodNode {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> String {
        format!("{}.{}", self.struct_name, self.name)
    }

    pub fn is_inlinable(&self) -> bool {
        fn_is_inlinable!(self)
    }
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub location: Location,
    pub statements: Vec<Statement>,
    pub is_unsafe: bool,
    #[cfg(not(feature = "old_codegen"))]
    pub llvm_has_terminator: bool,
}

impl BlockNode {
    fn total_len(&self) -> usize {
        let mut size = 0;
        for stmt in &self.statements {
            match stmt {
                Statement::Block(b) => size += b.total_len(),
                Statement::If(ifn) => {
                    let t = &ifn.if_body;
                    size += t.total_len();
                    if let Some(e) = &ifn.else_body {
                        size += e.total_len();
                    }
                    size += 1;
                }
                Statement::While(whl) => size += whl.body.total_len() + 1,
                _ => size += 1
            }
        }
        debug_assert!(size >= self.statements.len(), "There are at least {} statements in this block, but recursion only found {size}.", self.statements.len());
        size
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockNode),
    Expression(Expression),
    VarDecl(VarDeclNode),
    If(IfNode),
    Return(ReturnNode),
    While(WhileNode),
    Break(BreakNode),
    Continue(ContinueNode),
}

impl Statement {
    pub fn get_loc(&self) -> Location {
        match self {
            Self::Block(e) => e.location,
            Self::Expression(e) => e.get_loc(),
            Self::VarDecl(e) => e.location,
            Self::If(e) => e.location,
            Self::Return(e) => e.location,
            Self::While(e) => e.location,
            Self::Break(e) => e.location,
            Self::Continue(e) => e.location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub location: Location,
    pub name: String,
    pub typ: TypeNode,
    pub expression: Expression,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub location: Location,
    pub condition: Expression,
    pub if_body: Box<BlockNode>,
    pub else_body: Option<BlockNode>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub location: Location,
    pub return_value: Option<Expression>,
    pub typ: Type,
    pub function: String,
    pub strukt: Option<String>,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub location: Location,
    pub condition: Expression,
    pub body: BlockNode,
}

#[derive(Debug, Clone)]
pub struct BreakNode {
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct ContinueNode {
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    pub location: Location,
    pub typ: Type,
}

impl TypeNode {
    #[trace_call(extra)]
    pub fn none(location: Location) -> Self {
        Self {
            location,
            typ: Type::None,
        }
    }
    #[trace_call(extra)]
    pub fn this(location: Location, typ: Type) -> Self {
        Self {
            location,
            typ,
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expression {
    Name(NameNode),
    Literal(LiteralNode),
    StructLiteral(StructLiteralNode),
    ArrayLiteral(ArrayLiteralNode),
    Unary(UnaryNode),
    Binary(BinaryNode),
    // Parenthesis(Expression),
    FunctionCall(CallNode),
    Sizeof(TypeNode),
}

impl Expression {
    #[trace_call(extra)]
    pub fn get_loc(&self) -> Location {
        match &self {
            Self::Name(e) => e.location,
            Self::Literal(e) => e.location,
            Self::StructLiteral(e) => e.location,
            Self::ArrayLiteral(e) => e.location,
            Self::Unary(e) => e.location,
            Self::Binary(e) => e.location,
            Self::FunctionCall(e) => e.location,
            Self::Sizeof(e) => e.location,
        }
    }

    #[trace_call(extra)]
    pub fn get_type(&self) -> Type {
        match &self {
            Self::Name(e) => e.typ.clone(),
            Self::StructLiteral(e) => e.typ.clone(),
            Self::ArrayLiteral(e) => e.typ.clone(),
            Self::Literal(e) => e.typ.clone(),
            Self::Unary(e) => e.typ.clone(),
            Self::Binary(e) => e.typ.clone(),
            Self::FunctionCall(e) => e.typ.clone(),
            Self::Sizeof(_e) => Type::Usize,
        }
    }

    #[trace_call(extra)]
    #[allow(unused)]
    pub fn set_type(&mut self, typ: Type) {
        match self {
            Self::Name(e) => e.typ = typ,
            Self::StructLiteral(e) => e.typ = typ,
            Self::ArrayLiteral(e) => e.typ = typ,
            Self::Literal(e) => e.typ = typ,
            Self::Unary(e) => e.typ = typ,
            Self::Binary(e) => e.typ = typ,
            Self::FunctionCall(e) => e.typ = typ,
            Self::Sizeof(e) => todo!(),
        }
    }

    #[trace_call(extra)]
    pub fn is_lvalue(&self) -> bool {
        match &self {
            Self::Name(_) => true,
            Self::Unary(e) => {
                e.operation == Operation::Dereference
            }
            Self::Binary(e) => {
                e.operation == Operation::MemberAccess ||
                e.operation == Operation::IndexedAccess
            }
            _ => false
        }
    }

    #[trace_call(extra)]
    pub fn is_arithmetic(&self) -> bool {
        match &self {
            Self::Unary(e) => e.operation.is_arithmetic(),
            Self::Binary(e) => e.operation.is_arithmetic(),
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralNode {
    pub location: Location,
    pub value: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct StructLiteralNode {
    pub location: Location,
    pub struct_name: String,
    pub fields: Vec<(String, Expression)>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralNode {
    pub location: Location,
    pub elements: Vec<Expression>,
    pub typ: Type,
    // Small optimization so we don't need to typecheck
    // and codegen the same array elements multiple times
    // [elem; n] => size = Some(n), len(elements)=1
    // [elem, elem, ...] => size = None, len(elements)=n
    pub size: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct UnaryNode {
    pub location: Location,
    pub operation: Operation,
    pub expression: Box<Expression>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct BinaryNode {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub typ: Type,
}

impl BinaryNode {
    #[trace_call(extra)]
    pub fn is_comparison(&self) -> bool {
        self.operation.is_comparison()
    }

    #[trace_call(extra)]
    pub fn is_arithmetic(&self) -> bool {
        self.operation.is_arithmetic()
    }

    #[trace_call(extra)]
    pub fn is_bitwise(&self) -> bool {
        self.operation.is_bitwise()
    }

    #[trace_call(extra)]
    pub fn is_logical(&self) -> bool {
        self.operation.is_logical()
    }

    #[trace_call(extra)]
    pub fn is_indexed_access(&self) -> bool {
        self.operation == Operation::IndexedAccess
    }
}

#[derive(Debug, Clone)]
pub struct CallNode {
    pub location: Location,
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub typ: Type,
    pub is_extern: bool,
}

impl CallNode {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> String {
        self.function_name.clone()
    }

    #[trace_call(extra)]
    pub fn get_method_name(&self, strukt_name: &str) -> String {
        format!("{}.{}", strukt_name, self.function_name)
    }
}

#[derive(Debug, Clone)]
pub struct NameNode {
    pub location: Location,
    pub name: String,
    pub typ: Type,
}