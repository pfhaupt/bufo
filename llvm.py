
from typing import List
import subprocess
def call_cmd(cmd: List) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(cmd, capture_output=True)

def get(what: str, split_by: str = "\n") -> List[str]:
    out = call_cmd(["llvm-config", what])
    assert out.returncode == 0
    output = out.stdout.decode('utf-8')
    return output.split(split_by)

def enumeratePtrs(ptrs: List[str]) -> str:
    result = ""
    for p in ptrs:
        result += f"""
struct {p} {{
    ptr: Any;
    func isNull(&this) -> bool {{
        unsafe {{
            return this.ptr == null;
        }}
    }}
}}
"""
    return result

def compile_wrapper(llvm_path, libs):
    include = llvm_path + "\\..\\include\\"
    cmd = ["cl.exe", "/c", ".\\wrapper\\target.c", f"/I{include}", "/LIBPATH{llvm_path}"]
    for l in libs:
        cmd.append(llvm_path + "\\" + l.replace("\n", ""))
    proc = call_cmd(cmd)
    assert proc.returncode == 0, proc.stdout.decode("utf-8")
    cmd = ["lib.exe", "/out:llvm_wrapper.lib", "target.obj"]
    proc = call_cmd(cmd)
    assert proc.returncode == 0, proc.stdout.decode("utf-8")

with open("./stage1/backend/LLVM/bindings.bufo", "w") as file:
    path = get("--libdir")[0]
    libs = get("--libnames", " ")
    content = "compiler_flags {\n"
    compile_wrapper(path, libs)
    content += "  library: \"llvm_wrapper\";\n"
    for l in libs:
        _l = l.replace("\n", "")
        if _l.endswith(".lib"):
            _l = _l.removesuffix(".lib")
        content += f"  library: \"{_l}\";\n"
    path = path.replace("\\", "\\\\")
    content += f"  libpath: \"{path}\";\n"
    content += "}\n"
    content += enumeratePtrs([
        "LLVMTargetMachineRef",
        "LLVMTargetRef",
        "LLVMTargetDataRef",
        "LLVMPassManagerRef",
        "LLVMContextRef",
        "LLVMModuleRef",
        "LLVMBuilderRef",
        "LLVMBasicBlockRef",
        "LLVMValueRef",
        "LLVMTypeRef",
    ])
    content += """
// LLVMTarget
extern LLVMGetTargetFromName(name: &char) -> LLVMTargetRef;
extern LLVM_InitializeNativeAsmParser() -> i32;
extern LLVM_InitializeNativeAsmPrinter() -> i32;
extern LLVM_InitializeNativeDisassembler() -> i32;
extern LLVM_InitializeNativeTarget() -> i32;
extern LLVMCreateTargetMachine(T: LLVMTargetRef, Triple: &char, CPU: &char, Features: &char, Level: i32, Reloc: i32, CodeModel: i32) -> LLVMTargetMachineRef;
extern LLVMTargetMachineEmitToFile(T: LLVMTargetMachineRef, M: LLVMModuleRef, path: &char, opts: i32, err: &mut LLVMString) -> i32;
extern LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
extern LLVMStoreSizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
extern LLVMABISizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
extern LLVMSizeOfTypeInBits(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;

// LLVMContext
extern LLVMContextCreate() -> LLVMContextRef;
extern LLVMContextDispose(context: LLVMContextRef);
extern LLVMModuleCreateWithNameInContext(id: &char, C: LLVMContextRef) -> LLVMModuleRef;
extern LLVMIntTypeInContext(c: LLVMContextRef, bits: u32) -> LLVMTypeRef;
extern LLVMFloatTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
extern LLVMDoubleTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
extern LLVMVoidTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
extern LLVMStructTypeInContext(c: LLVMContextRef, ElementTypes: &LLVMTypeRef, ElementCount: u32, Packed: LLVMBool) -> LLVMTypeRef;

// LLVMModule
extern LLVMPrintModuleToString(M: LLVMModuleRef) -> &char;
extern LLVMPrintModuleToFile(M: LLVMModuleRef, Filename: &char, ErrorMessage: &mut LLVMString) -> LLVMBool;
extern LLVMGetNamedFunction(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
extern LLVMAddFunction(M: LLVMModuleRef, name: &char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
extern LLVMAddGlobalInAddressSpace(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: &char, AddressSpace: u32) -> LLVMValueRef;
extern LLVMGetNamedGlobal(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
extern LLVMVerifyModule(M: LLVMModuleRef, mode: i32, code: &mut LLVMString) -> LLVMBool;

// LLVMPassManager
extern LLVMCreatePassManager() -> LLVMPassManagerRef;
extern LLVMAddPromoteMemoryToRegisterPass(P: LLVMPassManagerRef);
extern LLVMAddAlwaysInlinerPass(P: LLVMPassManagerRef);
extern LLVMAddCFGSimplificationPass(P: LLVMPassManagerRef);
extern LLVMAddGlobalDCEPass(P: LLVMPassManagerRef);
extern LLVMRunPassManager(P: LLVMPassManagerRef, M: LLVMModuleRef) -> i32;

// LLVMBasicBlock
extern LLVMAppendBasicBlockInContext(C: LLVMContextRef, FnRef: LLVMValueRef, name: &char) -> LLVMBasicBlockRef;
extern LLVMGetInsertBlock(Builder: LLVMBuilderRef) -> LLVMBasicBlockRef;
extern LLVMGetBasicBlockParent(Block: LLVMBasicBlockRef) -> LLVMValueRef;
extern LLVMGetFirstBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
extern LLVMGetLastInstruction(Block: LLVMBasicBlockRef) -> LLVMValueRef;

// LLVMBuilder
extern LLVMCreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef;
extern LLVMCreateBuilder() -> LLVMBuilderRef;
extern LLVMBuildRetVoid(B: LLVMBuilderRef) -> LLVMValueRef;
extern LLVMBuildRet(B: LLVMBuilderRef, Value: LLVMValueRef) -> LLVMValueRef;
extern LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);
extern LLVMPositionBuilderBefore(Builder: LLVMBuilderRef, Instr: LLVMValueRef);
extern LLVMBuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: &char) -> LLVMValueRef;
extern LLVMBuildStore(B: LLVMBuilderRef, Value: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
extern LLVMBuildInsertValue(B: LLVMBuilderRef, AggVal: LLVMValueRef, EltVal: LLVMValueRef, Index: u32, Name: &char) -> LLVMValueRef;
extern LLVMBuildBr(B: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;
extern LLVMBuildCondBr(B: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef) -> LLVMValueRef;
extern LLVMBuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Ptr: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildICmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFCmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildNot(B: LLVMBuilderRef, Val: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildOr(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildAnd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildXor(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
extern LLVMBuildPtrToInt(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildIntToPtr(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildSIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
extern LLVMBuildUIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
extern LLVMBuildFPToSI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
extern LLVMBuildFPToUI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
extern LLVMBuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: &LLVMValueRef, NumArgs: u32, Name: &char) -> LLVMValueRef;
extern LLVMBuildGlobalStringPtr(B: LLVMBuilderRef, Str: &char, Name: &char) -> LLVMValueRef;
extern LLVMBuildSExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildZExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildStructGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Idx: u32, Name: &char) -> LLVMValueRef;
extern LLVMBuildGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: &LLVMValueRef, Length: u32, Name: &char) -> LLVMValueRef;
extern LLVMBuildFPExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildFPTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
extern LLVMBuildUnreachable(B: LLVMBuilderRef) -> LLVMValueRef;

// LLVMValue
extern LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
extern LLVMConstInt(IntTy: LLVMTypeRef, N: usize, SignExtend: LLVMBool) -> LLVMValueRef;
extern LLVMConstReal(RealTy: LLVMTypeRef, F: f64) -> LLVMValueRef;
extern LLVMConstNull(Ty: LLVMTypeRef) -> LLVMValueRef;
extern LLVMDumpValue(V: LLVMValueRef);
extern LLVMGlobalGetValueType(Val: LLVMValueRef) -> LLVMTypeRef;

// LLVM Globals
extern LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef);

extern LLVMGetParam(FnRef: LLVMValueRef, index: u32) -> LLVMValueRef;
extern LLVMSetValueName2(Val: LLVMValueRef, Name: &char, NameLen: usize);
extern LLVMSetValueName(Val: LLVMValueRef, Name: &char);
extern LLVMGetValueName2(Val: LLVMValueRef, Length: &mut usize) -> &char;
extern LLVMGetValueName(Val: LLVMValueRef) -> &char;

struct LLVMBool { val: i32; }
func newLLVMBool(b: bool) -> LLVMBool {
    if (b) return LLVMBool { val: 1 };
    return LLVMBool { val: 0 };
}

struct LLVMString { chars: &char; }

// LLVMType
extern LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: &LLVMTypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVMTypeRef;
extern LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: u32) -> LLVMTypeRef;
extern LLVMArrayType(ElementType: LLVMTypeRef, size: u32) -> LLVMTypeRef;
extern LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: &mut LLVMTypeRef);
extern LLVMCountParamTypes(FunctionTy: LLVMTypeRef) -> u32;
extern LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
extern LLVMPrintTypeToString(Ty: LLVMTypeRef) -> &char;

comptime LLVMVoidTypeKind: i32 = 0;
comptime LLVMHalfTypeKind: i32 = 1;
comptime LLVMFloatTypeKind: i32 = 2;
comptime LLVMDoubleTypeKind: i32 = 3;
comptime LLVMX86_FP80TypeKind: i32 = 4;
comptime LLVMFP128TypeKind: i32 = 5;
comptime LLVMPPC_FP128TypeKind: i32 = 6;
comptime LLVMLabelTypeKind: i32 = 7;
comptime LLVMIntegerTypeKind: i32 = 8;
comptime LLVMFunctionTypeKind: i32 = 9;
comptime LLVMStructTypeKind: i32 = 10;
comptime LLVMArrayTypeKind: i32 = 11;
comptime LLVMPointerTypeKind: i32 = 12;
comptime LLVMVectorTypeKind: i32 = 13;
comptime LLVMMetadataTypeKind: i32 = 14;
// Unused according to https://llvm.org/doxygen/llvm-c_2Core_8h_source.html line 164
// comptime LLVMX86_MMXTypeKind: i32 = 15;
comptime LLVMTokenTypeKind: i32 = 16;
comptime LLVMScalableVectorTypeKind: i32 = 17;
comptime LLVMBFloatTypeKind: i32 = 18;
comptime LLVMX86_AMXTypeKind: i32 = 19;
comptime LLVMTargetExtTypeKind: i32 = 20;
extern LLVMGetTypeKind(Ty: LLVMTypeRef) -> i32;

comptime LLVMIntEQ : i32 = 32;
comptime LLVMIntNE : i32 = 33;
comptime LLVMIntUGT: i32 = 34;
comptime LLVMIntUGE: i32 = 35;
comptime LLVMIntULT: i32 = 36;
comptime LLVMIntULE: i32 = 37;
comptime LLVMIntSGT: i32 = 38;
comptime LLVMIntSGE: i32 = 39;
comptime LLVMIntSLT: i32 = 40;
comptime LLVMIntSLE: i32 = 41;

comptime LLVMRealPredicateFalse: i32 = 0;  /**< Always false (always folded) */
comptime LLVMRealOEQ           : i32 = 1;  /**< True if ordered and equal */
comptime LLVMRealOGT           : i32 = 2;  /**< True if ordered and greater than */
comptime LLVMRealOGE           : i32 = 3;  /**< True if ordered and greater than or equal */
comptime LLVMRealOLT           : i32 = 4;  /**< True if ordered and less than */
comptime LLVMRealOLE           : i32 = 5;  /**< True if ordered and less than or equal */
comptime LLVMRealONE           : i32 = 6;  /**< True if ordered and operands are unequal */
comptime LLVMRealORD           : i32 = 7;  /**< True if ordered (no nans) */
comptime LLVMRealUNO           : i32 = 8;  /**< True if unordered: isnan(X) | isnan(Y) */
comptime LLVMRealUEQ           : i32 = 9;  /**< True if unordered or equal */
comptime LLVMRealUGT           : i32 = 10; /**< True if unordered or greater than */
comptime LLVMRealUGE           : i32 = 11; /**< True if unordered, greater than, or equal */
comptime LLVMRealULT           : i32 = 12; /**< True if unordered or less than */
comptime LLVMRealULE           : i32 = 13; /**< True if unordered, less than, or equal */
comptime LLVMRealUNE           : i32 = 14; /**< True if unordered or not equal */
comptime LLVMRealPredicateTrue : i32 = 15; /**< Always true (always folded) */

comptime LLVMAbortProcessAction: i32 = 0; /* verifier will print to stderr and abort() */
comptime LLVMPrintMessageAction: i32 = 1; /* verifier will print to stderr and return 1 */
comptime LLVMReturnStatusAction: i32 = 2; /* verifier will just return 1 */

comptime LLVMAssemblyFile: i32 = 0;
comptime LLVMObjectFile: i32 = 1;
"""
    file.write(content)
    print("[INFO] Generated LLVM bindings")
