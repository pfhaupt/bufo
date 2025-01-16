use std::collections::{HashMap, VecDeque};
use std::fmt::Display;

use crate::frontend::nodes;
use crate::frontend::tokens::KEYWORD_COMPTIME;
use crate::frontend::tokens::Location;
use crate::frontend::parser::Operation;
use crate::middleend::type_checker::Type;
use crate::backend::codegen_llvm::StructInfo;
use crate::internal_panic;
use crate::util::flags::Flags;

const COMPTIME_SCOPE_LIMIT: usize = 64;
const COMPTIME_MEMORY: usize = 64_000;

#[derive(Debug)]
pub enum EvalError<'src> {
    /// Error Loc, Binary Op, LHS, RHS
    BinaryNotImplemented(Location, Operation, Value, Value),
    /// Error Loc
    StatementNotImplemented(Location),
    /// Error Loc
    ExpressionNotImplemented(Location),
    /// Error Loc, Variable Name
    UnknownVariable(Location, &'src str),
    /// Error Loc, Function Name
    UnknownFunction(Location, &'src str),
    /// Error Loc, Value, Type
    ValueOutOfBounds(Location, Value, Type<'src>),
    /// Error Loc
    ScopeOverflow(Location),
    /// Mem Addr, Byte Count
    OutOfMemory(usize, usize),
}

impl Display for EvalError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BinaryNotImplemented(loc, op, lhs, rhs) =>
                write!(f, "{loc}: Operation `{lhs} {op} {rhs}` is currently not supported in a {KEYWORD_COMPTIME} context."),
            Self::StatementNotImplemented(loc) =>
                write!(f, "{loc}: The given statement is currently not supported in a {KEYWORD_COMPTIME} context."),
            Self::ExpressionNotImplemented(loc) =>
                write!(f, "{loc}: The given expression is currently not supported in a {KEYWORD_COMPTIME} context."),
            Self::UnknownVariable(loc, name) =>
                write!(f, "{loc}: Unknown comptime variable `{name}`."),
            Self::UnknownFunction(loc, name) =>
                write!(f, "{loc}: Unknown comptime function `{name}`."),
            Self::ValueOutOfBounds(loc, val, typ) =>
                write!(f, "{loc}: Evaluated value {val} is out of bounds for type {typ}."),
            Self::ScopeOverflow(loc) =>
                write!(f, "{loc}: Scope Overflow when trying to call {KEYWORD_COMPTIME} function."),
            Self::OutOfMemory(ptr, size) =>
                write!(f, "Evaluator Out Of Memory. Could not allocate {size} bytes at addr {ptr}."),
        }
    }
}

type StructType = Vec<Value>;
type ArrayType = Vec<Value>;
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined, // "value" of a block
    None, // Similar to Type::None
    Bool(bool),
    Char(u8),
    I128(i128),
    F64(f64),
    Ptr(usize),
    Struct(StructType),
    Array(ArrayType),
}

impl Value {
    const BOOL_KIND: u8 = 0;
    const PTR_KIND: u8 = 1;
    const I128_KIND: u8 = 2;
    const STRUCT_KIND: u8 = 3;
    const CHAR_KIND: u8 = 4;
    const ARRAY_KIND: u8 = 5;
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined => write!(f, "Undefined"),
            Self::None => write!(f, "None"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Char(v) => write!(f, "{v}"),
            Self::I128(v) => write!(f, "{v}"),
            Self::F64(v) => write!(f, "{v}"),
            Self::Ptr(v) => write!(f, "{v}"),
            Self::Struct(vals) => {
                write!(f, "{{{}}}", vals.iter().map(|v|format!("{v}")).collect::<Vec<_>>().join(", "))
            },
            Self::Array(vals) => {
                write!(f, "[{}]", vals.iter().map(|v|format!("{v}")).collect::<Vec<_>>().join(", "))
            },
        }
    }
}

impl Value {
    pub fn in_type_bounds(&self, typ: &Type<'_>) -> bool {
        match (self, typ) {
            (Self::Bool(_), Type::Bool) => true,
            (Self::Char(_), Type::Char) => true,
            (Self::I128(_), Type::I64) => true,
            (Self::I128(val), Type::Usize) => *val >= 0,
            (Self::I128(val), Type::I32) => *val >= i32::MIN as i128 && *val <= i32::MAX as i128,
            (Self::I128(val), Type::I16) => *val >= i16::MIN as i128 && *val <= i16::MAX as i128,
            (Self::I128(val), Type::I8) => *val >= i8::MIN as i128 && *val <= i8::MAX as i128,
            (Self::I128(val), Type::U32) => *val >= u32::MIN as i128 && *val <= u32::MAX as i128,
            (Self::I128(val), Type::U16) => *val >= u16::MIN as i128 && *val <= u16::MAX as i128,
            (Self::I128(val), Type::U8) => *val >= u8::MIN as i128 && *val <= u8::MAX as i128,
            (Self::Ptr(_), Type::Ref(_, _)) => true,
            (Self::Ptr(_), Type::Usize) => true,
            // FIXME: We need better in_type_bounds-checks for Structs and Arrays
            (Self::Struct(_), Type::Struct(_)) => true,
            (Self::Array(e), Type::Array(_, s)) => e.len() == *s,
            (v, t) => todo!("{v:?} {t:?}"),
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => internal_panic!("Expected Value::Bool in Value::as_bool!")
        }
    }
}

pub struct Evaluator<'flags, 'src, 'ast> {
    flags: &'flags Flags,
    functions: HashMap<&'src str, &'ast nodes::FunctionNode<'src>>,
    variables: VecDeque<HashMap<&'src str, usize>>,
    memory: Vec<u8>,
    blobs: HashMap<usize, usize>,
    struct_info: HashMap<&'src str, StructInfo<'src>>,
}

impl<'flags, 'src, 'ast> Evaluator<'flags, 'src, 'ast> {
    pub fn new(flags: &'flags Flags) -> Self {
        let mut variables = VecDeque::new();
        variables.push_back(HashMap::new()); // Global variables
        Self {
            flags,
            functions: HashMap::new(),
            variables,
            memory: Vec::with_capacity(COMPTIME_MEMORY),
            blobs: HashMap::new(),
            struct_info: HashMap::new(),
        }
    }

    pub fn set_struct_lookup(&mut self, struct_info: &HashMap<&'src str, StructInfo<'src>>) {
        self.struct_info = struct_info.clone();
    }

    fn check_scope_limit(&self, location: &Location) -> Result<(), EvalError<'src>> {
        if self.variables.len() >= COMPTIME_SCOPE_LIMIT {
            Err(EvalError::ScopeOverflow(*location))
        } else {
            Ok(())
        }
    }
    pub fn enter_scope(&mut self) {
        if self.flags.debug {
            println!("[DEBUG] ComptimeEval: Entering scope {}", self.variables.len())
        }
        self.variables.push_back(HashMap::new());
    }
    pub fn exit_scope(&mut self) {
        let freed = self.variables.pop_back().expect("Stack imbalance in Evaluator!");
        if self.flags.debug {
            println!("[DEBUG] ComptimeEval: Leaving scope {}", self.variables.len())
        }
        let mut vars = freed.into_values().collect::<Vec<_>>();
        vars.sort();
        vars.reverse();
        for ptr in vars {
            self.free(ptr);
        }
    }

    pub fn add_function(&mut self, function: &'ast nodes::FunctionNode<'src>) {
        debug_assert!(function.is_comptime);
        self.functions.insert(function.name, function);
    }

    pub fn add_variable(&mut self, variable: &nodes::VarDeclNode<'src>) -> Result<(), EvalError<'src>> {
        debug_assert!(variable.is_comptime);
        let value = self.evaluate(&variable.expression, false)?;
        self.add_variable_by_name(&variable.name, &value)
    }

    fn add_variable_by_name(&mut self, name: &'src str, value: &Value) -> Result<(), EvalError<'src>>{
        let encoded = self.encode_value(value);
        let size = encoded.len();
        let addr = self.alloc(size)?;
        self.memset(addr, &encoded);
        let len = self.variables.len() - 1;
        self.variables[len].insert(name, addr);
        if self.flags.debug {
            println!("[DEBUG] Added comptime variable `{}` with a value of {} at addr {addr}.", name, value);
        }
        Ok(())
    }

    fn get_variable_ptr(&self, name: &nodes::NameNode<'src>) -> Result<usize, EvalError<'src>> {
        for scope in self.variables.iter().rev() {
            if let Some(ptr) = scope.get(name.name) {
                return Ok(*ptr)
            }
        }
        Err(EvalError::UnknownVariable(name.location, name.name))
    }

    pub fn get_variable_value(&self, name: &nodes::NameNode<'src>) -> Result<Value, EvalError<'src>> {
        let addr = self.get_variable_ptr(name)?;
        let value = self.get_memory_by_ptr(addr);
        self.decode_value(value, &name.typ)
    }

    fn alloc(&mut self, bytes: usize) -> Result<usize, EvalError<'src>> {
        let ptr = self.memory.len();
        if ptr + bytes > COMPTIME_MEMORY {
            return Err(EvalError::OutOfMemory(ptr, bytes));
        }
        self.memory.resize(ptr + bytes, 0);
        self.blobs.insert(ptr, bytes);
        self.align_memory();
        Ok(ptr)
    }

    fn free(&mut self, ptr: usize) {
        let Some(_) = self.blobs.remove(&ptr) else {
            internal_panic!("Comptime Evaluator tried to free unallocated memory!")
        };
        self.memory.resize(ptr, 0);
    }

    fn align_memory(&mut self) {
        while self.memory.len() % 8 != 0 { self.memory.push(0); }
    }

    fn memset(&mut self, start: usize, bytes: &[u8]) {
        let area = &mut self.memory[start..(start + bytes.len())];
        area.copy_from_slice(bytes);
    }

    fn decode_value(&self, bytes: &[u8], underlying: &Type<'src>) -> Result<Value, EvalError<'src>> {
        match underlying {
            Type::Char => {
                debug_assert!(bytes.len() == std::mem::size_of::<u8>() + 1);
                debug_assert!(bytes[0] == Value::CHAR_KIND);
                let bytes = &bytes[1..];
                Ok(Value::Char(u8::from_ne_bytes(*bytes.first_chunk().unwrap())))
            }
            Type::Usize |
            Type::I8 | Type::U8 |
            Type::I16 | Type::U16 |
            Type::I32 | Type::U32 |
            Type::I64 | Type::U64 => {
                debug_assert!(bytes.len() == std::mem::size_of::<i128>() + 1);
                debug_assert!(bytes[0] == Value::I128_KIND);
                let bytes = &bytes[1..];
                Ok(Value::I128(i128::from_ne_bytes(*bytes.first_chunk().unwrap())))
            }
            Type::Ref(_, _) => {
                debug_assert!(bytes.len() == std::mem::size_of::<usize>() + 1);
                debug_assert!(bytes[0] == Value::PTR_KIND);
                let bytes = &bytes[1..];
                Ok(Value::Ptr(usize::from_ne_bytes(*bytes.first_chunk().unwrap())))
            }
            Type::Struct(name) => {
                let info = self.struct_info.get(name).unwrap();
                debug_assert!(bytes[0] == Value::STRUCT_KIND);
                let bytes = &bytes[1..];
                let len = usize::from_ne_bytes(*bytes.first_chunk::<8>().unwrap());
                debug_assert!(info.fields.len() == len);
                let mut offset = 8;
                let mut fields: Vec<Value> = Vec::with_capacity(len);
                for (_, field_type) in &info.fields {
                    let size = &bytes[offset..(offset + 8)];
                    let size = usize::from_ne_bytes(size.try_into().unwrap());
                    offset += 8;
                    let field_value = &bytes[offset..(offset + size)];
                    offset += size;
                    let val = self.decode_value(field_value, &field_type)?;
                    fields.push(val);
                }
                Ok(Value::Struct(fields))
            }
            Type::Bool => {
                debug_assert!(bytes.len() == 2);
                debug_assert!(bytes[0] == Value::BOOL_KIND);
                Ok(Value::Bool(bytes[1] != 0))
            }
            Type::Array(typ, size) => {
                debug_assert!(bytes[0] == Value::ARRAY_KIND);
                let bytes = &bytes[1..];
                let len = usize::from_ne_bytes(*bytes.first_chunk::<8>().unwrap());
                debug_assert!(*size == len);
                let mut offset = 8;
                let mut elements: Vec<Value> = Vec::with_capacity(len);
                for _ in 0..*size {
                    let size = &bytes[offset..(offset + 8)];
                    let size = usize::from_ne_bytes(size.try_into().unwrap());
                    offset += 8;
                    let element_value = &bytes[offset..(offset + size)];
                    offset += size;
                    let val = self.decode_value(element_value, typ)?;
                    elements.push(val);
                }
                Ok(Value::Array(elements))
            }
            t => todo!("{t:?}")
        }
    }

    fn encode_value(&self, value: &Value) -> Vec<u8> {
        match value {
            Value::Bool(b) => vec![Value::BOOL_KIND, *b as u8],
            Value::Ptr(addr) => {
                let mut v = vec![Value::PTR_KIND];
                v.extend(usize::to_ne_bytes(*addr).to_vec());
                v
            }
            Value::I128(val) => {
                let mut v = vec![Value::I128_KIND];
                v.extend(i128::to_ne_bytes(*val).to_vec());
                v
            }
            Value::Struct(fields) => {
                let count = fields.len();
                let mut v = vec![Value::STRUCT_KIND];
                v.extend(count.to_ne_bytes().to_vec());
                for f in fields {
                    let field = self.encode_value(&f);
                    v.extend(&field.len().to_ne_bytes());
                    v.extend(field);
                }
                v
            }
            Value::Array(elements) => {
                let count = elements.len();
                let mut v = vec![Value::ARRAY_KIND];
                v.extend(count.to_ne_bytes().to_vec());
                for e in elements {
                    let elem = self.encode_value(&e);
                    v.extend(&elem.len().to_ne_bytes());
                    v.extend(elem);
                }
                v
            }
            v => todo!("{v:?}")
        }
    }

    pub fn get_memory_by_ptr(&self, start: usize) -> &[u8] {
        let Some(length) = self.blobs.get(&start) else {
            internal_panic!("Could not get size for allocd memory starting at byte {start}")
        };
        &self.memory[start..(start + length)]
    }

    fn evaluate_block(&mut self, block: &nodes::BlockNode<'src>) -> Result<Value, EvalError<'src>> {
        for stmt in &block.statements {
            let stmt = self.evaluate_statement(stmt)?;
            if stmt != Value::Undefined {
                // Statement (for example `if` or `while`) returned
                return Ok(stmt)
            }
        }
        Ok(Value::Undefined)
    }

    fn evaluate_statement(&mut self, stmt: &nodes::Statement<'src>) -> Result<Value, EvalError<'src>> {
        match stmt {
            nodes::Statement::VarDecl(var_decl) => {
                let value = self.evaluate(&var_decl.expression, false)?;
                self.add_variable_by_name(var_decl.name, &value)?;
                Ok(Value::Undefined)
            }
            nodes::Statement::While(whyle) => {
                while self.evaluate(&whyle.condition, false)?.as_bool() {
                    let result = self.evaluate_block(&whyle.body)?;
                    if result != Value::Undefined {
                        return Ok(result)
                    }
                }
                Ok(Value::Undefined)
            }
            nodes::Statement::If(iff) => {
                let cond = self.evaluate(&iff.condition, false)?.as_bool();
                if cond {
                    self.evaluate_block(&iff.if_body)
                } else if let Some(else_body) = &iff.else_body {
                    self.evaluate_block(&else_body)
                } else {
                    Ok(Value::Undefined)
                }
            }
            nodes::Statement::Expression(expr) => {
                self.evaluate(expr, false)?;
                Ok(Value::Undefined)
            },
            nodes::Statement::Return(ret) => {
                if let Some(ret_expr) = &ret.return_value {
                    self.evaluate(ret_expr, false)
                } else {
                    Ok(Value::None)
                }
            }
            nodes::Statement::Block(block) => self.evaluate_block(block),
            stmt => {
                if self.flags.debug {
                    println!("[DEBUG] Not supported: {stmt:?}")
                }
                Err(EvalError::StatementNotImplemented(stmt.get_loc()))
            }
        }
    }

    fn evaluate(&mut self, expression: &nodes::Expression<'src>, needs_ptr: bool) -> Result<Value, EvalError<'src>> {
        let intermediate = match expression {
            nodes::Expression::Literal(lit) => self.evaluate_literal(&lit),
            nodes::Expression::Binary(bin) => self.evaluate_binary(&bin),
            nodes::Expression::Name(name) => self.evaluate_name(&name, needs_ptr),
            nodes::Expression::FunctionCall(call) => self.evaluate_call(&call),
            nodes::Expression::StructLiteral(strukt) => self.evaluate_struct(&strukt),
            nodes::Expression::Unary(unary) => self.evaluate_unary(&unary),
            nodes::Expression::ArrayLiteral(lit) => self.evaluate_array_literal(&lit),
            _ => Err(EvalError::ExpressionNotImplemented(expression.get_loc())),
        };
        let value = intermediate?;
        if !value.in_type_bounds(&expression.get_type()) {
            Err(EvalError::ValueOutOfBounds(expression.get_loc(), value, expression.get_type()))
        } else {
            Ok(value)
        }
    }
    fn evaluate_array_literal(&mut self, literal: &nodes::ArrayLiteralNode<'src>) -> Result<Value, EvalError<'src>> {
        let mut res = Vec::with_capacity(literal.elements.len());
        for e in &literal.elements {
            let val = self.evaluate(&e, false)?;
            res.push(val);
        }
        Ok(Value::Array(res))
    }
    fn evaluate_unary(&mut self, unary: &nodes::UnaryNode<'src>) -> Result<Value, EvalError<'src>> {
        let value = self.evaluate(&unary.expression, false)?;
        match &unary.operation {
            Operation::Dereference => {
                let Value::Ptr(addr) = value else {
                    internal_panic!("Unary Operation expects a pointer, the Type Checker should've caught this!")
                };
                match &unary.typ {
                    Type::Char => Ok(Value::Char(self.memory[addr])),
                    t => todo!("Unary Dereference for {t} at addr {addr}")
                }
            },
            Operation::Negate => {
                let Value::I128(val) = value else {
                    internal_panic!("Unary Operation expects an integer, the Type Checker should've caught this!")
                };
                Ok(Value::I128(-val))
            }
            o => todo!("{o}")
        }
    }
    fn evaluate_struct(&mut self, strukt: &nodes::StructLiteralNode<'src>) -> Result<Value, EvalError<'src>> {
        let mut res = Vec::with_capacity(strukt.fields.len());
        for f in &strukt.fields {
            let val = self.evaluate(&f.1, false)?;
            res.push(val);
        }
        Ok(Value::Struct(res))
    }
    fn evaluate_call(&mut self, call: &nodes::CallNode<'src>) -> Result<Value, EvalError<'src>> {
        let Some(func) = self.functions.get(call.get_full_name()) else {
            return Err(EvalError::UnknownFunction(call.location, call.get_full_name()))
        };
        debug_assert!(func.is_comptime);
        debug_assert!(func.parameters.len() == call.arguments.len());
        let arguments = call.arguments
            .iter().map(|e|self.evaluate(e, false)).collect::<Result<Vec<Value>, _>>();
        let vals = arguments?;
        self.enter_scope();
        self.check_scope_limit(&call.location)?;
        // Safety: We checked if it exists a few lines earlier.
        // Second access is needed because of borrowing rules.
        let func = unsafe { self.functions.get(call.get_full_name()).unwrap_unchecked() };
        for (arg, param) in vals.iter().zip(&func.parameters) {
            self.add_variable_by_name(param.name, arg)?;
        }
        // Safety: We checked if it exists a few lines earlier.
        // Another access is needed because of borrowing rules.
        let func = unsafe { self.functions.get(call.get_full_name()).unwrap_unchecked() };
        let result = self.evaluate_block(&func.block)?;
        self.exit_scope();
        Ok(result)
    }

    fn evaluate_binary(&mut self, binary: &nodes::BinaryNode<'src>) -> Result<Value, EvalError<'src>> {
        let lhs = self.evaluate(&binary.lhs, binary.operation == Operation::Assign)?;
        let rhs = self.evaluate(&binary.rhs, false)?;
        macro_rules! enumerate_ops {
            ($([$($vals:ident)*] [$($res:ident)*] $name:ident $op:tt)+) => {
                {
                    match (&binary.operation, &lhs, &rhs) {
                        (Operation::Assign, Value::Ptr(addr), rhs) => {
                            let vals = self.encode_value(rhs);
                            self.memset(*addr, &vals);
                            Ok(rhs.clone())
                        }
                        (Operation::Add, Value::Ptr(l), Value::I128(r)) => {
                            if !rhs.in_type_bounds(&Type::Usize) {
                                Err(EvalError::ValueOutOfBounds(binary.location, rhs, Type::Usize))
                            } else {
                                Ok(Value::Ptr(l + *r as usize))
                            }
                        },
                        $($((Operation::$name, Value::$vals(l), Value::$vals(r)) => Ok(Value::$res(l $op r)),)*)+
                        (o, l, r) => Err(EvalError::BinaryNotImplemented(binary.location, *o, l.clone(), r.clone()))
                    }
                }
            };
        }
        enumerate_ops!(
            [Char I128 F64 Ptr] [Char I128 F64 Ptr] Add +
            [Char I128 F64 Ptr] [Char I128 F64 Ptr] Sub -
            [I128 F64] [I128 F64] Mul *
            [Bool Char I128 F64 Ptr] [Bool Bool Bool Bool Bool] NotEqual !=
            [Bool Char I128 F64 Ptr] [Bool Bool Bool Bool Bool] Equal ==
        )
    }

    fn evaluate_name(&self, name: &nodes::NameNode<'src>, needs_ptr: bool) -> Result<Value, EvalError<'src>> {
        let Ok(var) = self.get_variable_ptr(name) else {
            internal_panic!("Evaluator encountered unknown identifier {}. This should've been caught by the Type Checker!", name.name);
        };
        if needs_ptr {
            Ok(Value::Ptr(var))
        } else {
            let mem = self.get_memory_by_ptr(var);
            self.decode_value(mem, &name.typ)
        }
    }

    fn evaluate_literal(&mut self, literal: &nodes::LiteralNode<'src>) -> Result<Value, EvalError<'src>> {
        match &literal.typ {
            Type::Bool => Ok(Value::Bool(literal.value == "true")),
            Type::Char => {
                let escaped = self.escape_string_or_char_value(&literal.value, false);
                debug_assert!(escaped.len() == 1);
                let value = escaped.chars().next().unwrap() as u8;
                Ok(Value::Char(value as u8))
            }
            Type::Usize
            | Type::U8 | Type::I8
            | Type::U16 | Type::I16
            | Type::U32 | Type::I32
            | Type::U64 | Type::I64 => Ok(Value::I128(literal.value.parse::<i128>().unwrap())),
            Type::F32 | Type::F64 => Ok(Value::F64(literal.value.parse::<f64>().unwrap())),
            Type::Ref(t, false) => {
                if **t != Type::Char {
                    unimplemented!("codegen_literal: {:?}", literal);
                }
                let escaped = self.escape_string_or_char_value(&literal.value, true);
                let ptr = self.alloc(escaped.len())?;
                self.memset(ptr, escaped.as_bytes());
                Ok(Value::Ptr(ptr))
            }
            e => unimplemented!("codegen_literal: {:?}", e),
        }
    }

    fn escape_string_or_char_value(&self, value: &str, null_terminated: bool) -> String {
        let mut new_value = Vec::new();
        let mut escaping = false;
        let mut index = 0;
        let chars = value.chars().collect::<Vec<_>>();
        while index < value.len() {
            let ch = chars[index];
            if escaping {
                match ch {
                    '\\' => new_value.push('\\' as u8),
                    '0' => new_value.push('\0' as u8),
                    'r' => new_value.push('\r' as u8),
                    'n' => new_value.push('\n' as u8),
                    't' => new_value.push('\t' as u8),
                    'x' => {
                        let lower = chars[index + 1];
                        let upper = chars[index + 2];
                        debug_assert!(lower.is_ascii_hexdigit());
                        debug_assert!(upper.is_ascii_hexdigit());
                        new_value.push((lower.to_digit(16).unwrap() * 16 + upper.to_digit(16).unwrap()) as u8);
                        index += 2;
                    }
                    c =>internal_panic!("Can't escape character `{c}`.")
                }
                escaping = false;
            } else if ch == '\\' {
                escaping = true;
            } else {
                new_value.push(ch as u8);
            }
            index += 1;
        }
        if null_terminated {
            new_value.push('\0' as u8);
        }
        String::from_utf8(new_value).unwrap()
    }
}
