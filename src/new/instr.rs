
#[derive(Debug)]
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

#[derive(Debug)]
pub enum Operand {
    Reg(usize, RegMode),
    MemOffset(usize), // e.g., stack offset
    MemAddr(usize),
    Imm32(u32),
    Imm64(u64),
}

#[derive(Debug)]
pub enum IR {
    LoadImm { dst: Operand, imm: Operand },
    StoreStack { offset: Operand, value: Operand },
    LoadStack { dst: Operand, offset: Operand },

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