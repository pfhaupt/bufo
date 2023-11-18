
use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::path::Path;

use crate::new::instr;
use crate::new::instr::{OperandType, IR};

use super::instr::{Register, RegMode};
use crate::flags::FILE_EXT;

use crate::new::new_codegen::ERR_STR;

const OUTPUT_FOLDER: &str = "./out/";

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
        let mut path = path.clone();
        if path.contains("/") {
            path = path.split("/").last().unwrap().to_string();
        }
        Self {
            path,
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
                            let size = if value.reg_mode == RegMode::BIT32 { 4 } else { 8 };
                            push_asm(format!("  mov [rbp-{offset}-{size}], {reg}").as_str());
                        }
                        (OperandType::Offset, OperandType::ImmI32
                                            | OperandType::ImmU32) => {
                            let offset = addr.off_or_imm;
                            let val = value.off_or_imm;
                            push_asm(format!("  mov DWORD [rbp-{offset}-4], {val}").as_str());
                        }
                        (OperandType::Offset, OperandType::ImmI64
                                            | OperandType::ImmU64) => {
                            let offset = addr.off_or_imm;
                            let val = value.off_or_imm;
                            push_asm(format!("  mov QWORD [rbp-{offset}-8], {val}").as_str());
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
                            let size = if dst.reg_mode == RegMode::BIT32 { 4 } else { 8 };
                            push_asm(format!("  mov {reg}, [rbp-{offset}-{size}]").as_str());
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
                        (OperandType::Reg, OperandType::ImmI32
                                            | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  add {dst_reg}, {immediate}").as_str());
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
                                            | OperandType::ImmU32) => {
                            let immediate = src2.off_or_imm;
                            push_asm(format!("  sub {dst_reg}, {immediate}").as_str());
                        }
                        (dst, src) => {
                            todo!("sub {dst:?} {src:?}")
                        }
                    }
                }
                IR::Mul { dst, src1, src2, signed } => {
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
                            (OperandType::Reg, OperandType::ImmI32
                                                | OperandType::ImmI64
                                                | OperandType::ImmU32
                                                | OperandType::ImmU64) => {
                                // IMUL only has up to 32bit immediates, need to use temp reg
                                todo!()
                            }
                            (dst, src) => {
                                todo!("imul {dst:?} {src:?}")
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
                            (OperandType::Reg, OperandType::ImmI32
                                                | OperandType::ImmI64
                                                | OperandType::ImmU32
                                                | OperandType::ImmU64) => {
                                // MUL has no Immediate mode, need to use temp reg
                                todo!()
                            }
                            (dst, src) => {
                                todo!("mul {dst:?} {src:?}")
                            }
                        }
                    }
                }
                IR::Div { dst, src1, src2, signed } => {
                    debug_assert!(dst == src1);
                    debug_assert!(dst.reg != instr::Register::None);
                    let dst_reg = reg(dst.reg, dst.reg_mode);

                    let d = if *signed { "idiv" } else { "div" };
                    let acc = if dst.reg_mode == RegMode::BIT32 {
                        "eax"
                    } else {
                        "rax"
                    };

                    // IDIV and DIV put remainder in DX, need to preserve it
                    let rem = "rdx";
                    push_asm(format!("  push {rem}").as_str());

                    match (dst.typ, src2.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src2.reg, src2.reg_mode);
                            // As with MUL, DIV uses accumulator
                            push_asm(format!("  mov {acc}, {dst_reg}").as_str());
                            push_asm("  cqo");
                            push_asm(format!("  {d} {src_reg}").as_str());
                            // Restore DX
                            push_asm(format!("  pop {rem}").as_str());
                            push_asm(format!("  mov {dst_reg}, {acc}").as_str());
                        }
                        (dst, src) => {
                            todo!("{d} {dst:?} {src:?}")
                        }
                    }
                }

                // Control Flow
                IR::Label { name } => push_asm(format!("{name}:").as_str()),
                IR::Cmp { dst, src } => {
                    debug_assert!(dst.typ == OperandType::Reg);
                    let dst_reg = reg(dst.reg, dst.reg_mode);
                    match (dst.typ, src.typ) {
                        (OperandType::Reg, OperandType::Reg) => {
                            let src_reg = reg(src.reg, src.reg_mode);
                            push_asm(format!("  cmp {dst_reg}, {src_reg}").as_str());
                        }
                        (OperandType::Reg, OperandType::ImmI32
                            | OperandType::ImmI64
                            | OperandType::ImmU32
                            | OperandType::ImmU64) => {
                            let immediate = src.off_or_imm;
                            push_asm(format!("  cmp {dst_reg}, {immediate}").as_str());
                        }
                        (dst, src) => {
                            todo!("cmp {dst:?} {src:?}")
                        }
                    }
                }
                IR::Jmp { name } => push_asm(format!("  jmp {name}").as_str()),
                IR::JmpEq { name } => push_asm(format!("  je {name}").as_str()),
                IR::JmpNeq { name } => push_asm(format!("  jne {name}").as_str()),
                IR::JmpLt { name } => push_asm(format!("  jl {name}").as_str()),
                IR::JmpLte { name } => push_asm(format!("  jle {name}").as_str()),
                IR::JmpGt { name } => push_asm(format!("  jg {name}").as_str()),
                IR::JmpGte { name } => push_asm(format!("  jge {name}").as_str()),

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
        push_asm("");
        push_asm("segment .data");
        push_asm("STACK_OVERFLOW_CODE dq 2");
        push_asm("FUNCTION_COUNTER dq 0");
        push_asm("FUNCTION_LIMIT dq 4096");
        push_asm("");
        push_asm("segment .bss");

        // println!("{}", output);
        if invalid {
            println!("INVALID!!!");
            std::process::exit(1);
        }
        match Path::new(OUTPUT_FOLDER).try_exists() {
            Ok(b) => {
                if !b {
                    // Create folder
                    if self.print_debug {
                        println!("Could not find output folder, creating now");
                    }
                    std::fs::create_dir(Path::new("./out/"));
                }
            }
            Err(e) => panic!("{}", e),
        }
        let mut filename = String::from(OUTPUT_FOLDER);
        filename.push_str(&self.path);
        let asmname = filename.replace(FILE_EXT, ".asm");
        let objname = asmname.replace(".asm", ".obj");

        if self.print_debug {
            println!("Writing to {asmname}");
        }
        match File::create(format!("{asmname}")) {
            Ok(mut file) => {
                file.write_all(output.as_bytes());
            }
            Err(e) => panic!("{}", e),
        }

        filename.replace(FILE_EXT, ".asm");
        if self.print_debug {
            println!("Running `nasm -f win64 {asmname} -o {objname}`");
        }
        let nasm_output = Command::new("nasm")
            .args(["-f", "win64", &asmname, "-o", &objname])
            .output()
            .expect("failed to execute process");
        if nasm_output.status.code().unwrap() != 0 {
            return Err(format!(
                "{}: Converting assembly to object file failed with:\n{}", ERR_STR, String::from_utf8(nasm_output.stderr).unwrap()
            ));
        }
        if self.print_debug {
            println!(
                "Running `golink /console /entry main {objname} MSVCRT.dll kernel32.dll`"
            );
        }
        let golink_output = Command::new("golink")
            .args([
                "/console",
                "/entry",
                "main",
                &objname,
                "MSVCRT.dll",
                "kernel32.dll",
            ])
            .output()
            .expect("failed to execute process");
        if golink_output.status.code().unwrap() != 0 {
            return Err(format!(
                "{}: Converting linking object files failed with:\n{}", ERR_STR, String::from_utf8(golink_output.stderr).unwrap()
            ));
        }

        Ok(())
    }

    pub fn run(&self) -> Result<(), String> {
        let mut output_path = String::from(OUTPUT_FOLDER);
        output_path.push_str(&self.path.replace(FILE_EXT, ".exe"));

        if self.print_debug {
            println!("Running `{output_path}`");
        }

        let output = Command::new(output_path)
            .output()
            .expect("failed to execute process");
        let exit_code = output.status.code().unwrap();
        if exit_code != 0 {
            Err(format!(
                "{}: Code execution failed with code 0x{:X}.", ERR_STR, exit_code
            ))
        } else {
            Ok(())
        }
    }
}