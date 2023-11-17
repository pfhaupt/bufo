#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RegMode {
    BIT64,
    BIT32
}

impl From<&crate::checker::Type> for RegMode {
    fn from(value: &crate::checker::Type) -> Self {
        Self::from(value.size())
    }
}

impl From<usize> for RegMode {
    fn from(bytes: usize) -> Self {
        match bytes {
            4 => Self::BIT32,
            8 => Self::BIT64,
            _ => panic!()
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    __COUNT,
}

impl From<usize> for Register {
    fn from(value: usize) -> Self {
        match value {
            0  => Register::RAX,
            1  => Register::RCX,
            2  => Register::RDX,
            3  => Register::RBX,
            4  => Register::RSP,
            5  => Register::RBP,
            6  => Register::RSI,
            7  => Register::RDI,
            8  => Register::R8,
            9  => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::R13,
            14 => Register::R14,
            15 => Register::R15,
            _ => panic!()
        }
    }
}

impl Register {
    pub const RET: Self = Self::RAX; // Return value in RAX
    // We're using the Windows x86-64 convention for arguments:
    pub const ARG1: Self = Self::RCX; // First argument in RCX
    pub const ARG2: Self = Self::RDX; // Second argument in RDX
    pub const ARG3: Self = Self::R8;  // Third argument in R8
    pub const ARG4: Self = Self::R9;  // Fourth argument in R9

    // https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170#callercallee-saved-registers
    pub const PRESERVED: [Self; 9] = [
        Self::RBX,
        Self::RBP,
        Self::RSP,
        Self::RSI,
        Self::RDI,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
    ];

    pub fn arg(index: usize) -> Self {
        // FIXME: Handle this case
        debug_assert!(index <= 3);
        const ARGS: [Register; 4] = [
            Register::ARG1,
            Register::ARG2,
            Register::ARG3,
            Register::ARG4
        ];
        ARGS[index]
    }
}


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operand {
    Reg(Register, RegMode),
    StackOffset(usize), // e.g., stack offset
    HeapAddr(usize),
    Imm32(u32),
    Imm64(u64),
    None, // for nodes that do not return any registers
}

#[derive(Debug)]
pub enum IR {
    LoadImm { dst: Operand, imm: Operand },
    Store { addr: Operand, value: Operand },
    Load { dst: Operand, addr: Operand },
    Move { dst: Operand, src: Operand },

    // Arithmetics
    Add { dst: Operand, src1: Operand, src2: Operand },
    Sub { dst: Operand, src1: Operand, src2: Operand },
    Mul { dst: Operand, src1: Operand, src2: Operand },
    Div { dst: Operand, src1: Operand, src2: Operand },
    
    // Control Flow
    Label { name: String },
    Cmp { dst: Operand, src: Operand },
    Jmp { name: String },
    JmpEq { name: String },
    JmpNeq { name: String },
    JmpLt { name: String },
    JmpLte { name: String },
    JmpGt { name: String },
    JmpGte { name: String },

    // Functions
    Call { name: String },
    AllocStack { bytes: usize },
    DeallocStack { bytes: usize },
    PushReg { reg: Register },
    PopReg { reg: Register },
    Return,
}