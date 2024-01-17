use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use super::instr;
use super::instr::{OperandType, IR};

use super::instr::{RegMode, Register};

use crate::compiler::{ERR_STR, FILE_EXT, OUTPUT_FOLDER};
use crate::util::flags::Flags;
use crate::internal_error;

use tracer::trace_call;

const REG_64BIT: [&str; 16] = [
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13",
    "r14", "r15",
];
const REG_32BIT: [&str; 16] = [
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d",
    "r13d", "r14d", "r15d",
];
const REG_8BIT: [&str; 16] = [
    "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b",
    "r14b", "r15b",
];

#[trace_call(extra)]
fn reg(r: Register, rm: RegMode) -> &'static str {
    let index = r as usize;
    debug_assert!(index < REG_32BIT.len());
    match rm {
        RegMode::BIT8 => REG_8BIT[index],
        RegMode::BIT32 => REG_32BIT[index],
        RegMode::BIT64 => REG_64BIT[index],
    }
}

pub struct Assembler<'flags> {
    path: String,
    flags: &'flags Flags,
}

impl<'flags> Assembler<'flags> {
    pub fn new(flags: &'flags Flags) -> Self {
        Self {
            path: String::new(),
            flags
        }.filepath(&flags.input)
    }

    pub fn filepath(self, path: &str) -> Self {
        let mut path = path.to_owned();
        if path.contains('/') {
            path = path.split('/').last().unwrap().to_string();
        }
        if path.contains('\\') {
            path = path.split('\\').last().unwrap().to_string();
        }
        Self { path, ..self }
    }

    #[trace_call(always)]
    pub fn generate_x86_64(&self, ir: Vec<instr::IR>) -> Result<(), String> {
        let mut output = String::new();
        let mut push_asm = |s: &str| {
            output.push_str(s);
            output.push('\n');
        };

        push_asm(format!("  ; Generated code for {}", self.path).as_str());
        push_asm("default rel");
        push_asm("");

        push_asm("segment .text");
        push_asm("  global main");
        push_asm("  extern ExitProcess");
        push_asm("  extern printf");
        push_asm("  extern calloc");
        push_asm("");

        for ir in &ir {
            if self.flags.debug {
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
                        i => {
                            return internal_error!(format!("Can't generate ASM for `LoadImm {i:?}"));
                        },
                    }
                }
                IR::Store { addr, value } => match (addr.typ, value.typ) {
                    (OperandType::Offset, OperandType::Reg) => {
                        let offset = addr.off_or_imm;
                        let reg = reg(value.reg, value.reg_mode);
                        let size = value.reg_mode.size();
                        push_asm(format!("  mov [rbp-{offset}-{size}], {reg}").as_str());
                    }
                    (OperandType::Offset, OperandType::ImmI32 | OperandType::ImmU32) => {
                        let offset = addr.off_or_imm;
                        let val = value.off_or_imm;
                        push_asm(format!("  mov DWORD [rbp-{offset}-4], {val}").as_str());
                    }
                    (OperandType::Offset, OperandType::ImmI64 | OperandType::ImmU64) => {
                        let offset = addr.off_or_imm;
                        let val = value.off_or_imm;
                        push_asm(format!("  mov QWORD [rbp-{offset}-8], {val}").as_str());
                    }
                    (OperandType::Reg, OperandType::Reg) => {
                        let dst = reg(addr.reg, addr.reg_mode);
                        let src = reg(value.reg, value.reg_mode);
                        push_asm(format!("  mov [{dst}], {src}").as_str());
                    }
                    (OperandType::Reg,
                    OperandType::ImmI32
                    | OperandType::ImmU32) => {
                        let dst = reg(addr.reg, addr.reg_mode);
                        let imm = value.off_or_imm;
                        push_asm(format!("  mov DWORD [{dst}], {imm}").as_str());
                    }
                    (OperandType::Reg,
                    OperandType::ImmI64
                    | OperandType::ImmU64) => {
                        let dst = reg(addr.reg, addr.reg_mode);
                        let imm = value.off_or_imm;
                        push_asm(format!("  mov QWORD [{dst}], {imm}").as_str());
                    }
                    (OperandType::Address, OperandType::Reg) => {
                        debug_assert!(addr.reg != instr::Register::None);
                        let src = reg(value.reg, value.reg_mode);
                        let dst = addr.reg;
                        let dst = reg(dst, RegMode::BIT64);
                        push_asm(format!("  mov [{dst}], {src}").as_str());
                    }
                    (lhs, rhs) => {
                        return internal_error!(format!("Can't generate ASM for `store {lhs:?}, {rhs:?}"));
                    },
                },
                IR::Load { dst, addr } => match (dst.typ, addr.typ) {
                    (OperandType::Reg, OperandType::Offset) => {
                        let reg = reg(dst.reg, dst.reg_mode);
                        let offset = addr.off_or_imm;
                        let size = dst.reg_mode.size();
                        push_asm(format!("  mov {reg}, [rbp-{offset}-{size}]").as_str());
                    }
                    (OperandType::Reg, OperandType::Reg) => {
                        let dst = reg(dst.reg, dst.reg_mode);
                        let src = reg(addr.reg, addr.reg_mode);
                        push_asm(format!("  mov {dst}, [{src}]").as_str());
                    }
                    (OperandType::Reg, OperandType::Address) => {
                        debug_assert!(addr.reg != instr::Register::None);
                        let src = reg(addr.reg, addr.reg_mode);
                        let dst = dst.reg;
                        let dst = reg(dst, RegMode::BIT64);
                        push_asm(format!("  mov {dst}, [{src}]").as_str());
                    }
                    (lhs, rhs) => {
                        return internal_error!(format!("Can't generate ASM for `load {lhs:?}, {rhs:?}"));
                    },
                },
                IR::Move { dst, src } => match (dst.typ, src.typ) {
                    (OperandType::Reg, OperandType::Reg) => {
                        let dst = reg(dst.reg, dst.reg_mode);
                        let src = reg(src.reg, src.reg_mode);
                        push_asm(format!("  mov {dst}, {src}").as_str());
                    }
                    (OperandType::Reg, OperandType::Offset) => {
                        let reg = reg(dst.reg, dst.reg_mode);
                        let offset = src.off_or_imm;
                        let size = dst.reg_mode.size();
                        push_asm(format!("  mov {reg}, [rbp-{offset}-{size}]").as_str());
                    }
                    (OperandType::Reg, OperandType::Address) => {
                        debug_assert!(src.reg != instr::Register::None);
                        let src = reg(src.reg, src.reg_mode);
                        let dst = dst.reg;
                        let dst = reg(dst, RegMode::BIT64);
                        push_asm(format!("  mov {dst}, [{src}]").as_str());
                    }
                    (
                        OperandType::Reg,
                        OperandType::ImmI8
                        | OperandType::ImmI32
                        | OperandType::ImmI64
                        | OperandType::ImmU8
                        | OperandType::ImmU32
                        | OperandType::ImmU64,
                    ) => {
                        let dst = reg(dst.reg, dst.reg_mode);
                        let imm = src.off_or_imm;
                        push_asm(format!("  mov {dst}, {imm}").as_str());
                    }
                    (lhs, rhs) => {
                        return internal_error!(format!("Can't generate ASM for `move {lhs:?}, {rhs:?}"));
                    },
                },

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
                            let size = dst.reg_mode.size();
                            push_asm(format!("  add {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (OperandType::Reg, OperandType::Address) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  add {dst_reg}, [{src_reg}]").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32 | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  add {dst_reg}, {immediate}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI64 | OperandType::ImmU64) => {
                            // ADD has no 64bit immediate mode, we need to use a register
                            let immediate = src2.off_or_imm;
                            push_asm("  push rax");
                            push_asm(format!("  mov rax, {immediate}").as_str());
                            push_asm(format!("  add {dst_reg}, rax").as_str());
                            push_asm("  pop rax");
                        }
                        (dst, src) => {
                            return internal_error!(format!("Can't generate ASM for `add {dst:?}, {src:?}"));
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
                            let size = dst.reg_mode.size();
                            push_asm(format!("  sub {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32 | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  sub {dst_reg}, {immediate}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI64 | OperandType::ImmU64) => {
                            // SUB has no 64bit immediate mode, we need to use a register
                            let immediate = src2.off_or_imm;
                            push_asm("  push rax");
                            push_asm(format!("  mov rax, {immediate}").as_str());
                            push_asm(format!("  sub {dst_reg}, rax").as_str());
                            push_asm("  pop rax");
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `sub {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                IR::Mul {
                    dst,
                    src1,
                    src2,
                    signed,
                } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    if *signed {
                        match (dst.typ, src2.typ) {
                            (OperandType::Reg, OperandType::Reg) => {
                                // IMUL is simpler, it has 2-operand form
                                let src_reg = reg(src2.reg, src2.reg_mode);
                                push_asm(format!("  imul {dst_reg}, {src_reg}").as_str());
                            }
                            (
                                OperandType::Reg,
                                OperandType::ImmI32
                                | OperandType::ImmU32
                            ) => {
                                let value = src2.off_or_imm;
                                push_asm(format!("  imul {dst_reg}, {dst_reg}, {value}").as_str());
                            }
                            (
                                OperandType::Reg,
                                | OperandType::ImmI64
                                | OperandType::ImmU64,
                            ) => {
                                // IMUL has no 64bit immediate mode, we need to use a register
                                let value = src2.off_or_imm;
                                push_asm("  push rax");
                                push_asm(format!("  mov rax, {value}").as_str());
                                push_asm(format!("  imul {dst_reg}, rax").as_str());
                                push_asm("  pop rax");
                            }
                            (OperandType::Reg, OperandType::Offset) => {
                                let offset = src2.off_or_imm;
                                let size = dst.reg_mode.size();
                                push_asm(format!("  imul {dst_reg}, [rbp-{offset}-{size}]").as_str());
                            }
                            (dst, src) => {
                                return internal_error!(format!(
                                    "Can't generate ASM for `imul {dst:?}, {src:?}"
                                ));
                            }
                        }
                    } else {
                        match (dst.typ, src2.typ) {
                            (OperandType::Reg, OperandType::Reg) => {
                                // MUL always puts result in accumulator
                                // need to accomodate for that
                                let src_reg = reg(src2.reg, src2.reg_mode);
                                let acc = if dst.reg_mode == RegMode::BIT32 {
                                    "eax"
                                } else {
                                    "rax"
                                };
                                push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                                push_asm(format!("  mul {src_reg}").as_str());
                                push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                            }
                            (
                                OperandType::Reg,
                                OperandType::ImmI32
                                | OperandType::ImmU32
                            ) => {
                                let value = src2.off_or_imm;
                                push_asm("push rax");
                                push_asm("push rdx");
                                push_asm("push rbx");

                                push_asm(format!("mov eax, {dst_reg}").as_str());
                                push_asm(format!("mov ebx, {value}").as_str());
                                push_asm("mul ebx");
                                push_asm("pop rbx");
                                push_asm("pop rdx");
                                push_asm(format!("mov {dst_reg}, eax").as_str());
                                push_asm("pop rax");

                            }
                            (
                                OperandType::Reg,
                                | OperandType::ImmI64
                                | OperandType::ImmU64,
                            ) => {
                                let value = src2.off_or_imm;
                                push_asm("push rax");
                                push_asm("push rdx");
                                push_asm("push rbx");
                                push_asm(format!("mov rax, {dst_reg}").as_str());
                                push_asm(format!("mov rbx, {value}").as_str());
                                push_asm("mul rbx");
                                push_asm("pop rbx");
                                push_asm("pop rdx");
                                push_asm(format!("mov {dst_reg}, rax").as_str());
                                push_asm("pop rax");
                            }
                            (
                                OperandType::Reg,
                                OperandType::Offset,
                            ) => {
                                let offset = src2.off_or_imm;
                                let size = dst.reg_mode.size();
                                push_asm("push rax");
                                push_asm("push rdx");
                                push_asm("push rbx");
                                push_asm(format!("mov rax, {dst_reg}").as_str());
                                push_asm(format!("mov rbx, [rbp-{offset}-{size}]").as_str());
                                push_asm("mul rbx");
                                push_asm("pop rbx");
                                push_asm("pop rdx");
                                push_asm(format!("mov {dst_reg}, rax").as_str());
                                push_asm("pop rax");
                            }
                            (dst, src) => {
                                return internal_error!(format!(
                                    "Can't generate ASM for `mul {dst:?}, {src:?}"
                                ));
                            }
                        }
                    }
                }
                IR::Div {
                    dst,
                    src1,
                    src2,
                    signed,
                } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);

                    let d = if *signed { "idiv" } else { "div" };
                    let word = if dst.reg_mode == RegMode::BIT32 {
                        "DWORD"
                    } else {
                        "QWORD"
                    };
                    let acc = if dst.reg_mode == RegMode::BIT32 {
                        "eax"
                    } else {
                        "rax"
                    };

                    let cq = if dst.reg_mode == RegMode::BIT32 {
                        "  cdq"
                    } else {
                        "  cqo"
                    };

                    // IDIV and DIV put remainder in DX, need to preserve it
                    let rem = "rdx";
                    push_asm(format!("  push {rem}").as_str());

                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            // As with MUL, DIV uses accumulator
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm(cq);
                            push_asm(format!("  {d} {src_reg}").as_str());
                            // Restore DX
                            push_asm(format!("  pop {rem}").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32) => {
                            // IDIV and DIV have no immediate mode, we need to use a register
                            let value = src2.off_or_imm;
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm("  push rcx");
                            push_asm(format!("  mov ecx, {value}").as_str());
                            push_asm(cq);
                            push_asm(format!("  {d} ecx").as_str());
                            push_asm("  pop rcx");
                            // Restore DX
                            push_asm(format!("  pop {rem}").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm(cq);
                            push_asm(format!("  {d} {word} [rbp-{offset}-{size}]").as_str());
                            // Restore DX
                            push_asm(format!("  pop {rem}").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `{d} {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                IR::Mod { dst, src1, src2, signed } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);

                    let d = if *signed { "idiv" } else { "div" };
                    let word = if dst.reg_mode == RegMode::BIT32 {
                        "DWORD"
                    } else {
                        "QWORD"
                    };
                    let acc = if dst.reg_mode == RegMode::BIT32 {
                        "eax"
                    } else {
                        "rax"
                    };
                    let cq = if dst.reg_mode == RegMode::BIT32 {
                        "  cdq"
                    } else {
                        "  cqo"
                    };
                    // IDIV and DIV put remainder in DX, need to preserve it
                    let rem = if dst.reg_mode == RegMode::BIT32 {
                        "edx"
                    } else {
                        "rdx"
                    };
                    push_asm(format!("  push rdx").as_str());

                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            if src2.reg == Register::Rdx {
                                // We can't use RDX as source, need to use a register
                                // because IDIV and DIV sign-extend into RDX
                                let reserve = if dst.reg_mode == RegMode::BIT32 {
                                    "ecx"
                                } else {
                                    "rcx"
                                };
                                push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                                push_asm(format!("  push rcx").as_str());
                                push_asm(format!("  mov {reserve}, {src_reg}").as_str());
                                push_asm(cq);
                                push_asm(format!("  {d} {reserve}").as_str());
                                push_asm(format!("  pop rcx").as_str());
                                push_asm(format!("  mov {acc}, {rem}").as_str());
                                // Restore DX
                                push_asm(format!("  pop rdx").as_str());
                                push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                            } else {
                                // As with MUL, DIV uses accumulator
                                push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                                push_asm(cq);
                                push_asm(format!("  {d} {src_reg}").as_str());
                                push_asm(format!("  mov {acc}, {rem}").as_str());
                                // Restore DX
                                push_asm(format!("  pop rdx").as_str());
                                push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                            }
                        }
                        (OperandType::Reg, OperandType::ImmI32) |
                        (OperandType::Reg, OperandType::ImmU32) |
                        (OperandType::Reg, OperandType::ImmI64) |
                        (OperandType::Reg, OperandType::ImmU64) => {
                            // IDIV and DIV have no immediate mode, we need to use a register
                            let cx = if dst.reg_mode == RegMode::BIT32 {
                                "ecx"
                            } else {
                                "rcx"
                            };
                            let value = src2.off_or_imm;
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm("  push rcx");
                            push_asm(format!("  mov {cx}, {value}").as_str());
                            push_asm(cq);
                            push_asm(format!("  {d} {cx}").as_str());
                            push_asm(format!("  mov {acc}, {rem}").as_str());
                            push_asm("  pop rcx");
                            // Restore DX
                            push_asm(format!("  pop rdx").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm(cq);
                            push_asm(format!("  {d} {word} [rbp-{offset}-{size}]").as_str());
                            push_asm(format!("  mov {acc}, {rem}").as_str());
                            // Restore DX
                            push_asm(format!("  pop rdx").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `{d} {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }

                // Bitwise
                IR::And { dst, src1, src2 } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  and {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  and {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32 | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  and {dst_reg}, {immediate}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI64 | OperandType::ImmU64) => {
                            // AND has no 64bit immediate mode, we need to use a register
                            let immediate = src2.off_or_imm;
                            push_asm("  push rax");
                            push_asm(format!("  mov rax, {immediate}").as_str());
                            push_asm(format!("  and {dst_reg}, rax").as_str());
                            push_asm("  pop rax");
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `and {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                IR::Or { dst, src1, src2 } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  or {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  or {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32 | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  or {dst_reg}, {immediate}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI64 | OperandType::ImmU64) => {
                            // OR has no 64bit immediate mode, we need to use a register
                            let immediate = src2.off_or_imm;
                            push_asm("  push rax");
                            push_asm(format!("  mov rax, {immediate}").as_str());
                            push_asm(format!("  or {dst_reg}, rax").as_str());
                            push_asm("  pop rax");
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `or {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                IR::Xor { dst, src1, src2 } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            push_asm(format!("  xor {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src2.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  xor {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32 | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  xor {dst_reg}, {immediate}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI64 | OperandType::ImmU64) => {
                            // XOR has no 64bit immediate mode, we need to use a register
                            let immediate = src2.off_or_imm;
                            push_asm("  push rax");
                            push_asm(format!("  mov rax, {immediate}").as_str());
                            push_asm(format!("  xor {dst_reg}, rax").as_str());
                            push_asm("  pop rax");
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `xor {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                // Control Flow
                IR::Label { name } => push_asm(format!("{name}:").as_str()),
                IR::Test { src1, src2 } => {
                    debug_assert!(src1 == src2);
                    debug_assert!(src1.reg_mode.size() == 1);
                    match src1.typ {
                        OperandType::Reg => {
                            let reg = reg(src1.reg, src1.reg_mode);
                            push_asm(format!("  test {reg}, {reg}").as_str());
                        }
                        OperandType::Offset => {
                            let offset = src1.off_or_imm;
                            let size = src1.reg_mode.size();
                            push_asm(format!("  push rax").as_str());
                            push_asm(format!("  mov al, [rbp-{offset}-{size}]",).as_str());
                            push_asm(format!("  test al, al").as_str());
                            push_asm(format!("  pop rax").as_str());
                        }
                        src => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `test {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                },
                IR::Cmp { dst, src } => {
                    debug_assert!(dst.typ == OperandType::Reg);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src.reg, src.reg_mode);
                            push_asm(format!("  cmp {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::Offset) => {
                            let offset = src.off_or_imm;
                            let size = dst.reg_mode.size();
                            push_asm(format!("  cmp {dst_reg}, [rbp-{offset}-{size}]").as_str());
                        }
                        (
                            OperandType::Reg,
                            OperandType::ImmI8
                            | OperandType::ImmI32
                            | OperandType::ImmI64
                            | OperandType::ImmU8
                            | OperandType::ImmU32
                            | OperandType::ImmU64,
                        ) => {
                            let immediate = src.off_or_imm;
                            push_asm(format!("  cmp {dst_reg}, {immediate}").as_str());
                        }
                        (dst, src) => {
                            return Err(format!(
                                "Internal Error: {}:{}:{}: Can't generate ASM for `cmp {dst:?}, {src:?}",
                                file!(),
                                line!(),
                                column!()
                            ));
                        }
                    }
                }
                IR::SetEq { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  sete {dst_reg}",).as_str());
                }
                IR::SetNeq { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  setne {dst_reg}",).as_str());
                }
                IR::SetLt { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  setl {dst_reg}",).as_str());
                }
                IR::SetLte { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  setle {dst_reg}",).as_str());
                }
                IR::SetGt { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  setg {dst_reg}",).as_str());
                }
                IR::SetGte { dst } => {
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    push_asm(format!("  setge {dst_reg}",).as_str());
                }
                IR::Jmp { name } => push_asm(format!("  jmp {name}").as_str()),
                IR::JmpEq { name } => push_asm(format!("  je {name}").as_str()),
                IR::JmpNeq { name } => push_asm(format!("  jne {name}").as_str()),
                IR::Exit { code } => {
                    let code = code.off_or_imm;
                    push_asm(format!("  mov rcx, {code}").as_str());
                    push_asm("  call ExitProcess");
                }
                // Functions
                IR::External { name } => {
                    push_asm(format!("  extern {name}").as_str());
                }
                IR::Call { name } => {
                    push_asm(format!("  call {name}").as_str());
                }
                IR::AllocStack { bytes } => {
                    push_asm("  push rbp");
                    push_asm("  mov rbp, rsp");
                    push_asm(format!("  sub rsp, {}", bytes).as_str());
                }
                IR::DeallocStack { .. } => {
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
            }
        }
        push_asm("");
        push_asm("segment .data");
        push_asm("STACK_OVERFLOW_CODE dq 2");
        push_asm("FUNCTION_COUNTER dq 0");
        push_asm("FUNCTION_LIMIT dq 4096");
        push_asm("");
        push_asm("segment .bss");

        match Path::new(OUTPUT_FOLDER).try_exists() {
            Ok(b) => {
                if !b {
                    // Create folder
                    if self.flags.debug {
                        println!("Could not find output folder, creating now");
                    }
                    std::fs::create_dir(Path::new("./out/")).unwrap();
                }
            }
            Err(e) => panic!("{}", e),
        }
        let mut filename = String::from(OUTPUT_FOLDER);
        filename.push_str(&self.path);
        let asmname = filename.replace(FILE_EXT, ".asm");
        let objname = asmname.replace(".asm", ".obj");
        let exename = objname.replace(".obj", ".exe");

        if self.flags.debug {
            println!("Writing to {asmname}");
        }
        match File::create(&asmname) {
            Ok(mut file) => {
                file.write_all(output.as_bytes()).unwrap();
            }
            Err(e) => panic!("{}", e),
        }

        // TODO: Figure out how to do this without calling nasm
        //       We're eventually going to fully switch to LLVM/clang, so we need to
        //       figure this out.
        let run_cmd = |args: &[&str], err_msg: &str| {
            if self.flags.debug {
                println!("Running `{}`", args.join(" "));
            }
            let output = Command::new(args[0])
                .args(&args[1..])
                .output()
                .expect("failed to execute process");
            if output.status.code().unwrap() != 0 {
                return Err(format!(
                    "{}: {err_msg}:\n{}",
                    ERR_STR,
                    String::from_utf8(output.stderr).unwrap()
                ));
            }
            Ok(())
        };
        run_cmd(
            &["nasm", "-f", "win64", &asmname, "-o", &objname],
            "Converting assembly to object file failed",
        )?;
        run_cmd(
            &["clang", &objname, "-o", &exename],
            "Creating executable failed",
        )?;

        // Clean up generated files
        // FIXME: Add this flag
        // if !self.flags.keep {
            // std::fs::remove_file(&asmname).unwrap();
            // std::fs::remove_file(&objname).unwrap();
        // }

        Ok(())
    }

    #[trace_call(always)]
    pub fn run(&self) -> Result<(), String> {
        let mut output_path = String::from(OUTPUT_FOLDER);
        output_path.push_str(&self.path.replace(FILE_EXT, ".exe"));

        if self.flags.debug {
            println!("Running `{output_path}`");
        }

        let output = Command::new(output_path)
            .output()
            .expect("failed to execute process");
        let exit_code = output.status.code().unwrap();
        println!("{}", String::from_utf8(output.stdout).unwrap());
        if exit_code != 0 {
            Err(format!(
                "{}: Code execution failed with code 0x{:X}.",
                ERR_STR, exit_code
            ))
        } else {
            Ok(())
        }
    }
}
