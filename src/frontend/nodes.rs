use std::fs;

use crate::frontend::tokens::Location;
use crate::frontend::parser::Operation;
use crate::middleend::type_checker::Type;

use tracer::trace_call;

macro_rules! fn_is_inlinable {
    ($func:ident) => {
        {
            $func.block.total_len() < 3
        }
    };
}

#[derive(Debug, Clone)]
pub struct FileNode<'src> {
    pub globals: Vec<VarDeclNode<'src>>,
    pub externs: Vec<ExternNode<'src>>,
    pub structs: Vec<StructNode<'src>>,
    pub functions: Vec<FunctionNode<'src>>,
    pub compiler_flags: CompilerFlagsNode<'src>,
}

impl<'src> FileNode<'src> {
    #[trace_call(extra)]
    pub fn get_all_structs(&self) -> Vec<&StructNode> {
        let mut structs = vec![];
        for strukt in &self.structs {
            structs.push(strukt);
        }
        structs
    }
}


#[allow(unused)]
#[derive(Debug, Clone)]
pub enum CompilerFlag<'src> {
    LibPath(Location, &'src str),
    Library(Location, &'src str),
    Linker(Location, &'src str),
}

impl<'src> CompilerFlag<'src> {
    #[trace_call(extra)]
    pub fn from(location: Location, flag: &'src str, value: &'src str) -> Result<Self, String> {
        match flag {
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

    #[cfg(windows)]
    #[trace_call(extra)]
    pub fn to_vec(&self) -> Vec<String> {
        match self {
            Self::LibPath(_, value) => vec![format!("/LIBPATH:{}", value)],
            Self::Library(_, value) => vec![format!("{}.lib", value)],
            Self::Linker(_, value) => vec!["-Xlinker".to_string(), value.to_string()],
        }
    }
    #[cfg(unix)]
    #[trace_call(extra)]
    pub fn to_vec(&self) -> Vec<String> {
        match self {
            Self::LibPath(_, value) => vec![format!("-L{}", value)],
            Self::Library(_, value) => vec![format!("{}", value)],
            Self::Linker(_, _) => unimplemented!(),
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct CompilerFlagsNode<'src> {
    pub location: Location,
    pub flags: Vec<CompilerFlag<'src>>,
}

#[derive(Debug, Clone)]
pub struct ExternNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub return_type: TypeNode<'src>,
    pub parameters: Vec<ParameterNode<'src>>,
    pub is_unsafe: bool,
    pub is_vararg: bool,
}

#[derive(Debug, Clone)]
pub struct StructNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub fields: Vec<FieldNode<'src>>,
    pub methods: Vec<MethodNode<'src>>,
}

impl<'src> StructNode<'src> {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> &'src str {
        self.name
    }
}

#[derive(Debug, Clone)]
pub struct FieldNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub type_def: TypeNode<'src>,
}

#[derive(Debug, Clone)]
pub struct FunctionNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub return_type: TypeNode<'src>,
    pub parameters: Vec<ParameterNode<'src>>,
    pub block: BlockNode<'src>,
    pub is_unsafe: bool,
    pub is_vararg: bool,
    pub is_comptime: bool,
    #[cfg(feature = "old_codegen")]
    pub stack_size: usize,
}

impl<'src> FunctionNode<'src> {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> &'src str {
        self.name
    }

    pub fn is_inlinable(&self) -> bool {
        fn_is_inlinable!(self)
    }
}

#[derive(Debug, Clone)]
pub struct MethodNode<'src> {
    pub location: Location,
    pub struct_name: &'src str,
    pub name: &'src str,
    pub return_type: TypeNode<'src>,
    pub parameters: Vec<ParameterNode<'src>>,
    pub block: BlockNode<'src>,
    pub is_unsafe: bool,
    pub is_vararg: bool,
    #[cfg(feature = "old_codegen")]
    pub stack_size: usize,
}

impl<'src> MethodNode<'src> {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> String {
        format!("{}.{}", self.struct_name, self.name)
    }

    pub fn is_inlinable(&self) -> bool {
        fn_is_inlinable!(self)
    }
}

#[derive(Debug, Clone)]
pub struct ParameterNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub typ: TypeNode<'src>,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct BlockNode<'src> {
    pub location: Location,
    pub statements: Vec<Statement<'src>>,
    pub is_unsafe: bool,
    #[cfg(not(feature = "old_codegen"))]
    pub llvm_has_terminator: bool,
}

impl<'src> BlockNode<'src> {
    fn total_len(&self) -> usize {
        let mut size = 0;
        for stmt in &self.statements {
            match stmt {
                Statement::Block(b) => size += b.total_len() + b.is_unsafe as usize,
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
pub enum Statement<'src> {
    Block(BlockNode<'src>),
    Expression(Expression<'src>),
    VarDecl(VarDeclNode<'src>),
    If(IfNode<'src>),
    Return(ReturnNode<'src>),
    While(WhileNode<'src>),
    Break(BreakNode),
    Continue(ContinueNode),
}

impl<'src> Statement<'src> {
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
pub struct VarDeclNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub typ: TypeNode<'src>,
    pub expression: Expression<'src>,
    pub is_mutable: bool,
    pub is_comptime: bool,
}

#[derive(Debug, Clone)]
pub struct IfNode<'src> {
    pub location: Location,
    pub condition: Expression<'src>,
    pub if_body: Box<BlockNode<'src>>,
    pub else_body: Option<BlockNode<'src>>,
}

#[derive(Debug, Clone)]
pub struct ReturnNode<'src> {
    pub location: Location,
    pub return_value: Option<Expression<'src>>,
    pub typ: Type<'src>,
    pub function: &'src str,
    pub strukt: Option<&'src str>,
}

#[derive(Debug, Clone)]
pub struct WhileNode<'src> {
    pub location: Location,
    pub condition: Expression<'src>,
    pub body: BlockNode<'src>,
    pub step: Option<Expression<'src>>,
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
pub struct TypeNode<'src> {
    pub location: Location,
    pub typ: Type<'src>,
}

impl<'src> TypeNode<'src> {
    #[trace_call(extra)]
    pub fn none(location: Location) -> Self {
        Self {
            location,
            typ: Type::None,
        }
    }
    #[trace_call(extra)]
    pub fn this(location: Location, typ: Type<'src>) -> Self {
        Self {
            location,
            typ,
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expression<'src> {
    Name(NameNode<'src>),
    Literal(LiteralNode<'src>),
    StructLiteral(StructLiteralNode<'src>),
    ArrayLiteral(ArrayLiteralNode<'src>),
    Unary(UnaryNode<'src>),
    Binary(BinaryNode<'src>),
    // Parenthesis(Expression),
    FunctionCall(CallNode<'src>),
    Sizeof(TypeNode<'src>),
    As(Box<Expression<'src>>, TypeNode<'src>),
}

impl<'src> Expression<'src> {
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
            Self::As(e, _) => e.get_loc(),
        }
    }

    #[trace_call(extra)]
    pub fn get_type(&self) -> Type<'src> {
        match &self {
            Self::Name(e) => e.typ.clone(),
            Self::StructLiteral(e) => e.typ.clone(),
            Self::ArrayLiteral(e) => e.typ.clone(),
            Self::Literal(e) => e.typ.clone(),
            Self::Unary(e) => e.typ.clone(),
            Self::Binary(e) => e.typ.clone(),
            Self::FunctionCall(e) => e.typ.clone(),
            Self::Sizeof(_e) => Type::Usize,
            Self::As(_, t) => t.typ.clone(),
        }
    }

    #[trace_call(extra)]
    #[allow(unused)]
    pub fn set_type(&mut self, typ: Type<'src>) {
        match self {
            Self::Name(e) => e.typ = typ,
            Self::StructLiteral(e) => e.typ = typ,
            Self::ArrayLiteral(e) => e.typ = typ,
            Self::Literal(e) => e.typ = typ,
            Self::Unary(e) => e.typ = typ,
            Self::Binary(e) => e.typ = typ,
            Self::FunctionCall(e) => e.typ = typ,
            Self::Sizeof(e) => todo!(),
            Self::As(e, t) => todo!(),
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
pub struct LiteralNode<'src> {
    pub location: Location,
    pub value: &'src str,
    pub typ: Type<'src>,
}

#[derive(Debug, Clone)]
pub struct StructLiteralNode<'src> {
    pub location: Location,
    pub struct_name: &'src str,
    pub fields: Vec<(&'src str, Expression<'src>)>,
    pub typ: Type<'src>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralNode<'src> {
    pub location: Location,
    pub elements: Vec<Expression<'src>>,
    pub typ: Type<'src>,
    // Small optimization so we don't need to typecheck
    // and codegen the same array elements multiple times
    // [elem; n] => size = Some(n), len(elements)=1
    // [elem, elem, ...] => size = None, len(elements)=n
    pub size: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct UnaryNode<'src> {
    pub location: Location,
    pub operation: Operation,
    pub expression: Box<Expression<'src>>,
    pub typ: Type<'src>,
}

#[derive(Debug, Clone)]
pub struct BinaryNode<'src> {
    pub location: Location,
    pub operation: Operation,
    pub lhs: Box<Expression<'src>>,
    pub rhs: Box<Expression<'src>>,
    pub typ: Type<'src>,
}

impl<'src> BinaryNode<'src> {
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
pub struct CallNode<'src> {
    pub location: Location,
    pub function_name: &'src str,
    pub arguments: Vec<Expression<'src>>,
    pub typ: Type<'src>,
    pub is_extern: bool,
}

impl<'src> CallNode<'src> {
    #[trace_call(extra)]
    pub fn get_full_name(&self) -> &'src str {
        self.function_name
    }

    #[trace_call(extra)]
    pub fn get_method_name(&self, strukt_name: &str) -> String {
        format!("{}.{}", strukt_name, self.function_name)
    }
}

#[derive(Debug, Clone)]
pub struct NameNode<'src> {
    pub location: Location,
    pub name: &'src str,
    pub typ: Type<'src>,
}
