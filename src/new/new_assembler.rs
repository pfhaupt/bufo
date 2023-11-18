
use crate::new::instr;
use crate::new::instr::{OperandType, IR};

use super::instr::{Register, RegMode};

const REG_64BIT: [&str; 16] = [
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
];
const REG_32BIT: [&str; 16] = [
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
];

fn reg(r: Register, rm: RegMode) -> &'static str {
    let index = r as usize;
    debug_assert!(index < REG_32BIT.len());
    if rm == RegMode::BIT32 {
        REG_32BIT[index]
    } else if rm == RegMode::BIT64 {
        REG_64BIT[index]
    } else {
        unreachable!()
    }
}

pub struct Assembler {
    print_debug: bool,
    path: String,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            print_debug: false,
            path: String::new()
        }
    }

    pub fn filepath(self, path: &String) -> Self {
        Self {
            path: path.clone(),
            ..self
        }
    }

    pub fn debug(self, print_debug: bool) -> Self {
        Self {
            print_debug,
            ..self
        }
    }

    pub fn generate_x86_64(&self, ir: Vec<instr::IR>) -> Result<(), String> {
        println!("{}", "-".repeat(50));
        let mut output = String::new();
        let mut push_asm = |s: &str| {
            output.push_str(s.clone());
            output.push('\n');
        };

        let mut invalid = false;

        push_asm(format!("  ; Generated code for {}", self.path).as_str());
        push_asm("default rel");
        push_asm("");

        push_asm("segment .text");
        push_asm("  global main");
        push_asm("  extern ExitProcess");
        push_asm("  extern printf");
        push_asm("  extern malloc");
        push_asm("");

        for ir in &ir {
            if self.print_debug {
                push_asm(format!("; -- {ir:?} --").as_str());
            }
            match ir {
                // Memory
                IR::LoadImm { dst, imm } => {
                    debug_assert!(dst.typ == OperandType::Reg);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match imm.typ {
                        OperandType::ImmI32
                        | OperandType::ImmI64
                        | OperandType::ImmU32
                        | OperandType::ImmU64 => {
                            let immediate = imm.off_or_imm;
                            push_asm(format!("  mov {dst_reg}, {immediate}").as_str());
                        }
                        _ => todo!()
                    }
                }
                IR::Store { addr, value } => {
                    match (addr.typ, value.typ) {
                        (OperandType::Offset, OperandType::Reg) => {
                            let offset = addr.off_or_imm;
                            let reg = reg(value.reg, value.reg_mode);
                            // FIXME: I think this is incorrect and doesnt account for type size
                            push_asm(format!("  mov [rbp-{offset}], {reg}").as_str());
                        }
                        (OperandType::Offset, OperandType::ImmI32
                                            | OperandType::ImmI64
                                            | OperandType::ImmU32
                                            | OperandType::ImmU64) => {
                            let offset = addr.off_or_imm;
                            let val = value.off_or_imm;
                            // FIXME: I think this is incorrect and doesnt account for type size
                            // FIXME: Add WORD-size
                            push_asm(format!("  mov [rbp-{offset}], {val}").as_str());
                        }
                        (OperandType::Reg, OperandType::Reg) => {
                            let dst = reg(addr.reg, addr.reg_mode);
                            let src = reg(value.reg, value.reg_mode);
                            push_asm(format!("  mov [{dst}], {src}").as_str());
                        }
                        (lhs, rhs) => todo!("{lhs:?} {rhs:?}")
                    }
                }
                IR::Load { dst, addr } => {
                    match (dst.typ, addr.typ) {
                        (OperandType::Reg, OperandType::Offset) => {
                            let reg = reg(dst.reg, dst.reg_mode);
                            let offset = addr.off_or_imm;
                            // FIXME: I think this is incorrect and doesnt account for type size
                            push_asm(format!("  mov {reg}, [rbp-{offset}]").as_str());
                        }
                        (OperandType::Reg, OperandType::Reg) => {
                            let dst = reg(dst.reg, dst.reg_mode);
                            let src = reg(addr.reg, addr.reg_mode);
                            push_asm(format!("  mov {dst}, [{src}]").as_str());
                        }
                        (lhs, rhs) => todo!("{lhs:?} {rhs:?}")
                    }
                }
                IR::Move { dst, src } => {
                    match (dst.typ, src.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let dst = reg(dst.reg, dst.reg_mode);
                            let src = reg(src.reg, src.reg_mode);
                            push_asm(format!("  mov {dst}, {src}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32
                                            | OperandType::ImmI64
                                            | OperandType::ImmU32
                                            | OperandType::ImmU64) => {
                            let dst = reg(dst.reg, dst.reg_mode);
                            let imm = src.off_or_imm;
                            push_asm(format!("  mov {dst}, {imm}").as_str());
                        }
                        (lhs, rhs) => todo!("{lhs:?} {rhs:?}")
                    }
                }

                // Arithmetics
                IR::Add { dst, src1, src2 } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  add {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            push_asm(format!("  add {dst_reg}, {offset}").as_str());
                        }
                        (dst, src) => {
                            todo!("add {dst:?} {src:?}")
                        }
                    }
                }
                IR::Sub { dst, src1, src2 } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  sub {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            push_asm(format!("  sub {dst_reg}, {offset}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32
                                            | OperandType::ImmI64
                                            | OperandType::ImmU32
                                            | OperandType::ImmU64) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  sub {dst_reg}, {immediate}").as_str());
                        }
                        (dst, src) => {
                            todo!("sub {dst:?} {src:?}")
                        }
                    }
                }
                IR::Label { name } => push_asm(format!("{name}:").as_str()),

                // Functions
                IR::Call { name } => {
                    push_asm(format!("  call {name}").as_str());
                }
                IR::AllocStack { bytes } => {
                    push_asm("  push rbp");
                    push_asm("  mov rbp, rsp");
                    push_asm(format!("  sub rsp, {}", bytes).as_str());
                }
                IR::DeallocStack { bytes } => {
                    push_asm("  mov rsp, rbp");
                    push_asm("  pop rbp");
                }
                IR::PushReg { reg } => {
                    let reg = *reg as usize;
                    let reg = REG_64BIT[reg];
                    push_asm(format!("  push {reg}").as_str());
                }
                IR::PopReg { reg } => {
                    let reg = *reg as usize;
                    let reg = REG_64BIT[reg];
                    push_asm(format!("  pop {reg}").as_str());
                }
                IR::Return => {
                    push_asm("  ret");
                }

                _ => {
                    // FIXME: Later on this must crash
                    push_asm("; Can't generate ASM for that yet");
                    invalid = true;
                }
            }
        }

        println!("{}", output);
        if invalid {
            println!("INVALID!!!");
            std::process::exit(1);
        }

        Ok(())
    }
}