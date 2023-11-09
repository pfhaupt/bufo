
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
pub enum Operand {
    Reg(usize, RegMode),
    StackOffset(usize), // e.g., stack offset
    HeapAddr(usize),
    Imm32(u32),
    Imm64(u64),
    None, // for nodes that do not return any registers
}

impl Operand {
    pub const RET: usize = 0;
    pub const ARG1: usize = 1;
    pub const ARG2: usize = 2;
    pub const ARG3: usize = 3;
    pub const ARG4: usize = 4;
}

#[derive(Debug)]
pub enum IR {
    LoadImm { dst: Operand, imm: Operand },
    Store { addr: Operand, value: Operand },
    Load { dst: Operand, addr: Operand },

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
    PushReg,
    PopReg,
    Return,
}