
#[derive(Debug)]
pub enum Operand {
    Reg(usize),
    MemOffset(usize), // e.g., stack offset
    MemAddr(usize),
    Imm32(u32),
    Imm64(u64),
}

#[derive(Debug)]
pub enum IR {
    LoadImm { dst: Operand, imm: Operand },

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
    PushReg,
    PopReg,
    Return,
}