use crate::frontend::parser::Operation;

use crate::middleend::checker::Type;

use std::fmt::Debug;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RegMode {
    BIT64,
    BIT32,
}

impl From<&Type> for RegMode {
    fn from(value: &Type) -> Self {
        match value.size() {
            Ok(v) => Self::from(v),
            Err(e) => panic!("{}", e)
        }
    }
}

impl From<usize> for RegMode {
    fn from(bytes: usize) -> Self {
        match bytes {
            4 => Self::BIT32,
            8 => Self::BIT64,
            _ => panic!(),
        }
    }
}

impl RegMode {
    pub fn size(&self) -> usize {
        match self {
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
pub enum OperandType {
    Reg,
    Cmp(Operation),
    ImmU32,
    ImmU64,
    ImmI32,
    ImmI64,
    Offset,
    Address, // For nodes that return an address
    None, // For nodes that do not return anything
}

#[derive(PartialEq, Clone, Copy)]
pub struct Operand {
    pub typ: OperandType,
    pub off_or_imm: usize,
    pub reg: Register,
    pub reg_mode: RegMode,
}

impl Operand {
    pub fn none() -> Self {
        Self {
            typ: OperandType::None,
            off_or_imm: 0,
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn reg(r: Register, rm: RegMode) -> Self {
        Self {
            typ: OperandType::Reg,
            off_or_imm: 0,
            reg: r,
            reg_mode: rm,
        }
    }

    pub fn imm_u32(immediate: u32) -> Self {
        Self {
            typ: OperandType::ImmU32,
            off_or_imm: unsafe { std::mem::transmute(immediate as u64) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn imm_u64(immediate: u64) -> Self {
        Self {
            typ: OperandType::ImmU64,
            off_or_imm: unsafe { std::mem::transmute(immediate) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn imm_i32(immediate: i32) -> Self {
        Self {
            typ: OperandType::ImmI32,
            off_or_imm: unsafe { std::mem::transmute(immediate as i64) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn imm_i64(immediate: i64) -> Self {
        Self {
            typ: OperandType::ImmI64,
            off_or_imm: unsafe { std::mem::transmute(immediate) },
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn offset(offset: usize) -> Self {
        Self {
            typ: OperandType::Offset,
            off_or_imm: offset,
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }
    pub fn cmp(cmp: Operation) -> Self {
        Self {
            typ: OperandType::Cmp(cmp),
            off_or_imm: 0,
            reg: Register::None,
            reg_mode: RegMode::BIT64,
        }
    }

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
            OperandType::Cmp(cmp) => write!(f, "{:?}", cmp),
            OperandType::ImmU32 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, u64>(self.off_or_imm) as u32
            }),
            OperandType::ImmU64 => write!(f, "{}", unsafe {
                std::mem::transmute::<usize, u64>(self.off_or_imm)
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

    // Control Flow
    Label {
        name: String,
    },
    Cmp {
        dst: Operand,
        src: Operand,
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
    JmpLt {
        name: String,
    },
    JmpLte {
        name: String,
    },
    JmpGt {
        name: String,
    },
    JmpGte {
        name: String,
    },

    // Functions
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
            Self::LoadImm { dst, imm } => write!(f, "LoadImm dst: {:?}, imm: {:?}", dst, imm),
            Self::Store { addr, value } => write!(f, "Store addr: {:?}, value: {:?}", addr, value),
            Self::Load { dst, addr } => write!(f, "Load dst: {:?}, addr: {:?}", dst, addr),
            Self::Move { dst, src } => write!(f, "Move dst: {:?}, src: {:?}", dst, src),
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
            Self::Label { name } => write!(f, "Label name: {:?}", name),
            Self::Cmp { dst, src } => write!(f, "Cmp dst: {:?}, src: {:?}", dst, src),
            Self::Jmp { name } => write!(f, "Jmp name: {:?}", name),
            Self::JmpEq { name } => write!(f, "JmpEq name: {:?}", name),
            Self::JmpNeq { name } => write!(f, "JmpNeq name: {:?}", name),
            Self::JmpLt { name } => write!(f, "JmpLt name: {:?}", name),
            Self::JmpLte { name } => write!(f, "JmpLte name: {:?}", name),
            Self::JmpGt { name } => write!(f, "JmpGt name: {:?}", name),
            Self::JmpGte { name } => write!(f, "JmpGte name: {:?}", name),
            Self::Call { name } => write!(f, "Call name: {:?}", name),
            Self::AllocStack { bytes } => write!(f, "AllocStack bytes: {:?}", bytes),
            Self::DeallocStack { bytes } => write!(f, "DeallocStack bytes: {:?}", bytes),
            Self::PushReg { reg } => write!(f, "PushReg reg: {:?}", reg),
            Self::PopReg { reg } => write!(f, "PopReg reg: {:?}", reg),
            Self::Return => write!(f, "Return"),
        }
    }
}