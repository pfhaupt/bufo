use crate::middleend::type_checker::Type;

use std::fmt::Debug;

use tracer::trace_call;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RegMode {
    BIT8,
    BIT32,
    BIT64,
}

impl From<&Type> for RegMode {
    #[trace_call(extra)]
    fn from(value: &Type) -> Self {
        Self::from(value.size())
    }
}

impl From<usize> for RegMode {
    #[trace_call(extra)]
    fn from(bytes: usize) -> Self {
        match bytes {
            1 => Self::BIT8,
            4 => Self::BIT32,
            8 => Self::BIT64,
            _ => panic!(),
        }
    }
}

impl RegMode {
    #[trace_call(extra)]
    pub fn size(&self) -> usize {
        match self {
            Self::BIT8 => 1,
            Self::BIT32 => 4,
            Self::BIT64 => 8,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Register {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    __COUNT,
    None,
}

impl From<usize> for Register {
    #[trace_call(extra)]
    fn from(value: usize) -> Self {
        match value {
            0 => Register::Rax,
            1 => Register::Rcx,
            2 => Register::Rdx,
            3 => Register::Rbx,
            4 => Register::Rsp,
            5 => Register::Rbp,
            6 => Register::Rsi,
            7 => Register::Rdi,
            8 => Register::R8,
            9 => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::R13,
            14 => Register::R14,
            15 => Register::R15,
            _ => panic!(),
        }
    }
}

impl Register {
    // We're using the Windows x86-64 convention for arguments:
    pub const RET: Self = Self::Rax; // Return value in RAX
    pub const ARG1: Self = Self::Rcx; // First argument in RCX
    pub const ARG2: Self = Self::Rdx; // Second argument in RDX
    pub const ARG3: Self = Self::R8; // Third argument in R8
    pub const ARG4: Self = Self::R9; // Fourth argument in R9

    // https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    #[allow(unused)]
    pub const PRESERVED: [Self; 9] = [
        Self::Rbx,
        Self::Rbp,
        Self::Rsp,
        Self::Rsi,
        Self::Rdi,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
    ];

    #[trace_call(extra)]
    pub fn arg(index: usize) -> Self {
        // NOTE: The Type Checker prevents the user from ever declaring
        //       Methods, Functions or Features with more than 4 arguments.
        //       This is an easy fix, we still may want a more sophisticated algorithm later.
        debug_assert!(index <= 3);
        const ARGS: [Register; 4] = [
            Register::ARG1,
            Register::ARG2,
            Register::ARG3,
            Register::ARG4,
        ];
        ARGS[index]
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(unused)]
pub enum OperandType {
    Reg,
    ImmU8,
    ImmI8,
    ImmU32,
    ImmU64,
    ImmI32,
    ImmI64,
    Offset,
    Address, // For nodes that return an address
    None,    // For nodes that do not return anything
}

#[derive(PartialEq, Clone, Copy)]
pub struct Operand {
    pub typ: OperandType,
    pub off_or_imm: usize,
    pub reg: Register,
    pub reg_mode: RegMode,
}

impl Operand {
    #[trace_call(extra)]
    pub fn none() -> Self {
        Self {
            typ: OperandType::None,
            off_or_imm: 0,
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    #[trace_call(extra)]
    pub fn reg(r: Register, rm: RegMode) -> Self {
        Self {
            typ: OperandType::Reg,
            off_or_imm: 0,
            reg: r,
            reg_mode: rm,
        }
    }
    #[trace_call(extra)]
    pub fn imm_u8(immediate: u8) -> Self {
        Self {
            typ: OperandType::ImmU8,
            off_or_imm: unsafe { std::mem::transmute(immediate as u64) },
            reg: Register::None,
            reg_mode: RegMode::BIT8,
        }
    }
    #[trace_call(extra)]
    pub fn imm_u32(immediate: u32) -> Self {
        Self {
            typ: OperandType::ImmU32,
            off_or_imm: unsafe { std::mem::transmute(immediate as u64) },
            reg: Register::None,
            reg_mode: RegMode::BIT32,
        }
    }
    #[trace_call(extra)]
    pub fn imm_u64(immediate: u64) -> Self {
        Self {
            typ: OperandType::ImmU64,
            off_or_imm: unsafe { std::mem::transmute(immediate) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    #[trace_call(extra)]
    pub fn imm_i8(immediate: i8) -> Self {
        // FIXME: Add overflow check, this is a bug waiting to happen.
        Self {
            typ: OperandType::ImmI8,
            off_or_imm: unsafe { std::mem::transmute(immediate as i64) },
            reg: Register::None,
            reg_mode: RegMode::BIT8,
        }
    }
    #[trace_call(extra)]
    pub fn imm_i32(immediate: i32) -> Self {
        Self {
            typ: OperandType::ImmI32,
            off_or_imm: unsafe { std::mem::transmute(immediate as i64) },
            reg: Register::None,
            reg_mode: RegMode::BIT32,
        }
    }
    #[trace_call(extra)]
    pub fn imm_i64(immediate: i64) -> Self {
        Self {
            typ: OperandType::ImmI64,
            off_or_imm: unsafe { std::mem::transmute(immediate) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    #[trace_call(extra)]
    pub fn offset(offset: usize, size: usize) -> Self {
        Self {
            typ: OperandType::Offset,
            off_or_imm: offset,
            reg: Register::None,
            reg_mode: RegMode::from(size),
        }
    }
    #[trace_call(extra)]
    pub fn addr(reg: Register) -> Self {
        Self {
            typ: OperandType::Address,
            off_or_imm: 0,
            reg,
            reg_mode: RegMode::BIT64,
        }
    }
}

impl Debug for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            OperandType::Reg => write!(f, "{:?}", self.reg),
            OperandType::ImmU8 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, u64>(self.off_or_imm) as u8
            }),
            OperandType::ImmU32 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, u64>(self.off_or_imm) as u32
            }),
            OperandType::ImmU64 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, u64>(self.off_or_imm)
            }),
            OperandType::ImmI8 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, i64>(self.off_or_imm) as i8
            }),
            OperandType::ImmI32 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, i64>(self.off_or_imm) as i32
            }),
            OperandType::ImmI64 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, i64>(self.off_or_imm)
            }),
            OperandType::Offset => write!(f, "stack+{}", self.off_or_imm),
            OperandType::Address => write!(f, "addr"),
            OperandType::None => write!(f, ""),
        }
    }
}

impl IR {
    #[trace_call(extra)]
    pub fn get_lbl(&self) -> String {
        match &self {
            Self::Label { name } => name.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
#[allow(unused)]
pub enum IR {
    // Memory
    LoadImm {
        dst: Operand,
        imm: Operand,
    },
    Store {
        addr: Operand,
        value: Operand,
    },
    Load {
        dst: Operand,
        addr: Operand,
    },
    Move {
        dst: Operand,
        src: Operand,
    },

    // Arithmetics
    Add {
        dst: Operand,
        src1: Operand,
        src2: Operand,
    },
    Sub {
        dst: Operand,
        src1: Operand,
        src2: Operand,
    },
    Mul {
        dst: Operand,
        src1: Operand,
        src2: Operand,
        signed: bool,
    },
    Div {
        dst: Operand,
        src1: Operand,
        src2: Operand,
        signed: bool,
    },
    Mod {
        dst: Operand,
        src1: Operand,
        src2: Operand,
        signed: bool,
    },

    // Bitwise
    And {
        dst: Operand,
        src1: Operand,
        src2: Operand,
    },
    Or {
        dst: Operand,
        src1: Operand,
        src2: Operand,
    },
    Xor {
        dst: Operand,
        src1: Operand,
        src2: Operand,
    },

    // Control Flow
    Label {
        name: String,
    },
    Cmp {
        dst: Operand,
        src: Operand,
    },
    Test {
        src1: Operand,
        src2: Operand,
    },
    SetEq {
        dst: Operand,
    },
    SetNeq {
        dst: Operand,
    },
    SetLt {
        dst: Operand,
    },
    SetLte {
        dst: Operand,
    },
    SetGt {
        dst: Operand,
    },
    SetGte {
        dst: Operand,
    },
    Jmp {
        name: String,
    },
    JmpEq {
        name: String,
    },
    JmpNeq {
        name: String,
    },
    Exit {
        code: Operand,
    },

    // Functions
    External {
        name: String,
    },
    Call {
        name: String,
    },
    AllocStack {
        bytes: usize,
    },
    DeallocStack {
        bytes: usize,
    },
    PushReg {
        reg: Register,
    },
    PopReg {
        reg: Register,
    },
    Return,
}

impl Debug for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Memory
            Self::LoadImm { dst, imm } => write!(f, "LoadImm dst: {:?}, imm: {:?}", dst, imm),
            Self::Store { addr, value } => write!(f, "Store addr: {:?}, value: {:?}", addr, value),
            Self::Load { dst, addr } => write!(f, "Load dst: {:?}, addr: {:?}", dst, addr),
            Self::Move { dst, src } => write!(f, "Move dst: {:?}, src: {:?}", dst, src),

            // Arithmetics
            Self::Add { dst, src1, src2 } => {
                write!(f, "Add dst: {:?}, src1: {:?}, src2: {:?}", dst, src1, src2)
            }
            Self::Sub { dst, src1, src2 } => {
                write!(f, "Sub dst: {:?}, src1: {:?}, src2: {:?}", dst, src1, src2)
            }
            Self::Mul {
                dst,
                src1,
                src2,
                signed,
            } => write!(
                f,
                "Mul dst: {:?}, src1: {:?}, src2: {:?}, signed: {:?}",
                dst, src1, src2, signed
            ),
            Self::Div {
                dst,
                src1,
                src2,
                signed,
            } => write!(
                f,
                "Div dst: {:?}, src1: {:?}, src2: {:?}, signed: {:?}",
                dst, src1, src2, signed
            ),
            Self::Mod {
                dst,
                src1,
                src2,
                signed,
            } => write!(
                f,
                "Mod dst: {:?}, src1: {:?}, src2: {:?}, signed: {:?}",
                dst, src1, src2, signed
            ),

            // Bitwise
            Self::And { dst, src1, src2 } => {
                write!(f, "BitwiseAnd dst: {:?}, src1: {:?}, src2: {:?}", dst, src1, src2)
            }
            Self::Or { dst, src1, src2 } => {
                write!(f, "BitwiseOr dst: {:?}, src1: {:?}, src2: {:?}", dst, src1, src2)
            }
            Self::Xor { dst, src1, src2 } => {
                write!(f, "BitwiseXor dst: {:?}, src1: {:?}, src2: {:?}", dst, src1, src2)
            }

            // Control Flow
            Self::Label { name } => write!(f, "Label name: {:?}", name),
            Self::Test { src1, src2 } => write!(f, "Test src1: {:?}, src2: {:?}", src1, src2),
            Self::Cmp { dst, src } => write!(f, "Cmp dst: {:?}, src: {:?}", dst, src),
            Self::SetEq { dst } => write!(f, "SetEq dst: {:?}", dst),
            Self::SetNeq { dst } => write!(f, "SetNeq dst: {:?}", dst),
            Self::SetLt { dst } => write!(f, "SetLt dst: {:?}", dst),
            Self::SetLte { dst } => write!(f, "SetLte dst: {:?}", dst),
            Self::SetGt { dst } => write!(f, "SetGt dst: {:?}", dst),
            Self::SetGte { dst } => write!(f, "SetGte dst: {:?}", dst),
            Self::Jmp { name } => write!(f, "Jmp name: {:?}", name),
            Self::JmpEq { name } => write!(f, "JmpEq name: {:?}", name),
            Self::JmpNeq { name } => write!(f, "JmpNeq name: {:?}", name),
            Self::Exit { code } => write!(f, "Exit code: {:?}", code),

            // Functions
            Self::External { name } => write!(f, "External: {:?}", name),
            Self::Call { name } => write!(f, "Call name: {:?}", name),
            Self::AllocStack { bytes } => write!(f, "AllocStack bytes: {:?}", bytes),
            Self::DeallocStack { bytes } => write!(f, "DeallocStack bytes: {:?}", bytes),
            Self::PushReg { reg } => write!(f, "PushReg reg: {:?}", reg),
            Self::PopReg { reg } => write!(f, "PopReg reg: {:?}", reg),
            Self::Return => write!(f, "Return"),
        }
    }
}
