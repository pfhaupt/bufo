
from typing import List
import subprocess
import sys

def print_cmd(cmd: List, verbose = False) -> None:
    if verbose or "--verbose" in sys.argv:
        print("CMD: " + " ".join(cmd))

def call_cmd(cmd: List) -> subprocess.CompletedProcess[bytes]:
    print_cmd(cmd)
    return subprocess.run(cmd, capture_output=True)

def try_call(cmd: List) -> subprocess.CompletedProcess[bytes]:
    try:
        return call_cmd(cmd)
    except Exception as e:
        print_cmd(cmd, verbose=True)
        print("error: Could not run command!")
        print(e)
        exit(1)

def get(what: List[str], split_by: str = "\n") -> List[str]:
    if sys.platform == "linux":
        c = ["llvm-config-16"]
    elif sys.platform == "Windows":
        c = ["llvm-config"]
    else:
        assert False, f"Unsupported OS {sys.platform}, can't call llvm-config"
    for w in what:
        c.append(w)
    out = try_call(c)
    assert out.returncode == 0, out.stderr.decode('utf-8')
    output = out.stdout.decode('utf-8')
    return output.split(split_by)

def enumeratePtrs(ptrs: List[str]) -> str:
    result = ""
    for p in ptrs:
        result += f"""
struct {p} {{
    ptr: Any;
}}
func isNull(this: {p}) -> bool {{
    return this.ptr == null;
}}
"""
    return result

def compile_wrapper(llvm_path, libs):
    if sys.platform == "linux":
        include = llvm_path + "/../include/"
        cmd = ["cc", "-c", "./wrapper/target.c", f"-I{include}", f"-L{llvm_path}"]
        for l in libs:
            cmd.append("-l" + l.replace("\n", ""))
    elif sys.platform == "Windows":
        include = llvm_path + "\\..\\include\\"
        cmd = ["cl.exe", "/c", ".\\wrapper\\target.c", f"/I{include}", "/LIBPATH{llvm_path}"]
        for l in libs:
            cmd.append(llvm_path + "\\" + l.replace("\n", ""))
    else:
        assert False, f"Unsupported OS {sys.platform}, can't compile wrapper"
    proc = try_call(cmd)
    assert proc.returncode == 0, proc.stderr.decode("utf-8")
    if sys.platform == "linux":
        cmd = ["ar", "rcs", "llvm_wrapper.a", "target.o"]
    elif sys.platform == "Windows":
        cmd = ["lib.exe", "/out:llvm_wrapper.lib", "target.obj"]
    else:
        assert False, f"Unsupported OS {sys.platform}, can't link wrapper"
    proc = try_call(cmd)
    assert proc.returncode == 0, proc.stderr.decode("utf-8")

with open("./src/backend/LLVM/bindings.bufo", "w") as file:
    path = get(["--libdir"])[0]
    libs = get(["--libnames"], " ")
    content = ""
    if sys.platform == "linux":
        content += "@os(LINUX)\n"
    elif sys.platform == "Windows":
        content += "@os(WINDOWS)\n"
    else:
        assert False, f"Unsupported OS {sys.platform}, can't specify @os() attribute"
    content += "compiler_flags {\n"
    compile_wrapper(path, libs)
    if sys.platform == "linux":
        content += "  library: \":llvm_wrapper.a\";\n"
    elif sys.platform == "Windows":
        content += "  library: \"llvm_wrapper\";\n"
    for l in libs:
        _l = l.replace("\n", "")
        if _l.endswith(".lib"):
            _l = _l.removesuffix(".lib")
        if sys.platform == "linux":
            content += f"  library: \":{_l}\";\n"
        elif sys.platform == "Windows":
            content += f"  library: \"{_l}\";\n"
    if sys.platform == "Windows":
        path = path.replace("\\", "\\\\")
    content += "  libpath: \".\";\n"
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
        "LLVMAttributeRef",
    ])
    content += """
// LLVMTarget
@extern("LLVMGetTargetFromName") func LLVMGetTargetFromName(name: &char) -> LLVMTargetRef;
@extern("LLVM_InitializeNativeAsmParser") func LLVM_InitializeNativeAsmParser() -> i32;
@extern("LLVM_InitializeNativeAsmPrinter") func LLVM_InitializeNativeAsmPrinter() -> i32;
@extern("LLVM_InitializeNativeDisassembler") func LLVM_InitializeNativeDisassembler() -> i32;
@extern("LLVM_InitializeNativeTarget") func LLVM_InitializeNativeTarget() -> i32;
@extern("LLVMCreateTargetMachine") func LLVMCreateTargetMachine(T: LLVMTargetRef, Triple: &char, CPU: &char, Features: &char, Level: i32, Reloc: i32, CodeModel: i32) -> LLVMTargetMachineRef;
@extern("LLVMTargetMachineEmitToFile") func LLVMTargetMachineEmitToFile(T: LLVMTargetMachineRef, M: LLVMModuleRef, path: &char, opts: i32, err: &LLVMString) -> i32;
@extern("LLVMCreateTargetDataLayout") func LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
@extern("LLVMStoreSizeOfType") func LLVMStoreSizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
@extern("LLVMABISizeOfType") func LLVMABISizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
@extern("LLVMSizeOfTypeInBits") func LLVMSizeOfTypeInBits(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
@extern("LLVMCopyStringRepOfTargetData") func LLVMCopyStringRepOfTargetData(T: LLVMTargetDataRef) -> &char;

// LLVMContext
@extern("LLVMContextCreate") func LLVMContextCreate() -> LLVMContextRef;
@extern("LLVMContextDispose") func LLVMContextDispose(context: LLVMContextRef);
@extern("LLVMModuleCreateWithNameInContext") func LLVMModuleCreateWithNameInContext(id: &char, C: LLVMContextRef) -> LLVMModuleRef;
@extern("LLVMIntTypeInContext") func LLVMIntTypeInContext(c: LLVMContextRef, bits: u32) -> LLVMTypeRef;
@extern("LLVMFloatTypeInContext") func LLVMFloatTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
@extern("LLVMDoubleTypeInContext") func LLVMDoubleTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
@extern("LLVMVoidTypeInContext") func LLVMVoidTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
@extern("LLVMStructTypeInContext") func LLVMStructTypeInContext(c: LLVMContextRef, ElementTypes: &LLVMTypeRef, ElementCount: u32, Packed: LLVMBool) -> LLVMTypeRef;
@extern("LLVMCreateEnumAttribute") func LLVMCreateEnumAttribute(c: LLVMContextRef, KindID: u32, Val: u64) -> LLVMAttributeRef;

// LLVMModule
@extern("LLVMPrintModuleToString") func LLVMPrintModuleToString(M: LLVMModuleRef) -> &char;
@extern("LLVMPrintModuleToFile") func LLVMPrintModuleToFile(M: LLVMModuleRef, Filename: &char, ErrorMessage: &LLVMString) -> LLVMBool;
@extern("LLVMGetNamedFunction") func LLVMGetNamedFunction(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
@extern("LLVMAddFunction") func LLVMAddFunction(M: LLVMModuleRef, name: &char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
@extern("LLVMAddGlobalInAddressSpace") func LLVMAddGlobalInAddressSpace(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: &char, AddressSpace: u32) -> LLVMValueRef;
@extern("LLVMGetNamedGlobal") func LLVMGetNamedGlobal(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
@extern("LLVMVerifyModule") func LLVMVerifyModule(M: LLVMModuleRef, mode: i32, code: &LLVMString) -> LLVMBool;
@extern("LLVMSetDataLayout") func LLVMSetDataLayout(M: LLVMModuleRef, Data: &char);
@extern("LLVMSetTarget") func LLVMSetTarget(M: LLVMModuleRef, Target: &char);

// LLVMPassManager
@extern("LLVMCreatePassManager") func LLVMCreatePassManager() -> LLVMPassManagerRef;
@extern("LLVMAddPromoteMemoryToRegisterPass") func LLVMAddPromoteMemoryToRegisterPass(P: LLVMPassManagerRef);
@extern("LLVMAddAlwaysInlinerPass") func LLVMAddAlwaysInlinerPass(P: LLVMPassManagerRef);
@extern("LLVMAddCFGSimplificationPass") func LLVMAddCFGSimplificationPass(P: LLVMPassManagerRef);
@extern("LLVMAddGlobalDCEPass") func LLVMAddGlobalDCEPass(P: LLVMPassManagerRef);
@extern("LLVMRunPassManager") func LLVMRunPassManager(P: LLVMPassManagerRef, M: LLVMModuleRef) -> i32;

// LLVMBasicBlock
@extern("LLVMAppendBasicBlockInContext") func LLVMAppendBasicBlockInContext(C: LLVMContextRef, FnRef: LLVMValueRef, name: &char) -> LLVMBasicBlockRef;
@extern("LLVMGetInsertBlock") func LLVMGetInsertBlock(Builder: LLVMBuilderRef) -> LLVMBasicBlockRef;
@extern("LLVMGetBasicBlockParent") func LLVMGetBasicBlockParent(Block: LLVMBasicBlockRef) -> LLVMValueRef;
@extern("LLVMGetFirstBasicBlock") func LLVMGetFirstBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
@extern("LLVMGetLastInstruction") func LLVMGetLastInstruction(Block: LLVMBasicBlockRef) -> LLVMValueRef;

// LLVMBuilder
@extern("LLVMCreateBuilderInContext") func LLVMCreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef;
@extern("LLVMCreateBuilder") func LLVMCreateBuilder() -> LLVMBuilderRef;
@extern("LLVMBuildRetVoid") func LLVMBuildRetVoid(B: LLVMBuilderRef) -> LLVMValueRef;
@extern("LLVMBuildRet") func LLVMBuildRet(B: LLVMBuilderRef, Value: LLVMValueRef) -> LLVMValueRef;
@extern("LLVMPositionBuilderAtEnd") func LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);
@extern("LLVMPositionBuilderBefore") func LLVMPositionBuilderBefore(Builder: LLVMBuilderRef, Instr: LLVMValueRef);
@extern("LLVMBuildAlloca") func LLVMBuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildStore") func LLVMBuildStore(B: LLVMBuilderRef, Value: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
@extern("LLVMBuildInsertValue") func LLVMBuildInsertValue(B: LLVMBuilderRef, AggVal: LLVMValueRef, EltVal: LLVMValueRef, Index: u32, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildBr") func LLVMBuildBr(B: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;
@extern("LLVMBuildCondBr") func LLVMBuildCondBr(B: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef) -> LLVMValueRef;
@extern("LLVMBuildLoad2") func LLVMBuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Ptr: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildAdd") func LLVMBuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildSub") func LLVMBuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildMul") func LLVMBuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildSDiv") func LLVMBuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildUDiv") func LLVMBuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildSRem") func LLVMBuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildURem") func LLVMBuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFAdd") func LLVMBuildFAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFSub") func LLVMBuildFSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFMul") func LLVMBuildFMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFDiv") func LLVMBuildFDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFRem") func LLVMBuildFRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildICmp") func LLVMBuildICmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFCmp") func LLVMBuildFCmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildNot") func LLVMBuildNot(B: LLVMBuilderRef, Val: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildOr") func LLVMBuildOr(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildAnd") func LLVMBuildAnd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildXor") func LLVMBuildXor(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildPtrToInt") func LLVMBuildPtrToInt(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildIntToPtr") func LLVMBuildIntToPtr(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildSIToFP") func LLVMBuildSIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildUIToFP") func LLVMBuildUIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildFPToSI") func LLVMBuildFPToSI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildFPToUI") func LLVMBuildFPToUI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildCall2") func LLVMBuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: &LLVMValueRef, NumArgs: u32, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildGlobalStringPtr") func LLVMBuildGlobalStringPtr(B: LLVMBuilderRef, Str: &char, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildSExt") func LLVMBuildSExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildZExt") func LLVMBuildZExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildTrunc") func LLVMBuildTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildStructGEP2") func LLVMBuildStructGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Idx: u32, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildGEP2") func LLVMBuildGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: &LLVMValueRef, Length: u32, Name: &char) -> LLVMValueRef;
@extern("LLVMBuildFPExt") func LLVMBuildFPExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildFPTrunc") func LLVMBuildFPTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
@extern("LLVMBuildUnreachable") func LLVMBuildUnreachable(B: LLVMBuilderRef) -> LLVMValueRef;

// LLVMValue
@extern("LLVMTypeOf") func LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
@extern("LLVMConstInt") func LLVMConstInt(IntTy: LLVMTypeRef, N: usize, SignExtend: LLVMBool) -> LLVMValueRef;
@extern("LLVMConstReal") func LLVMConstReal(RealTy: LLVMTypeRef, F: f64) -> LLVMValueRef;
@extern("LLVMConstNull") func LLVMConstNull(Ty: LLVMTypeRef) -> LLVMValueRef;
@extern("LLVMDumpValue") func LLVMDumpValue(V: LLVMValueRef);
@extern("LLVMGlobalGetValueType") func LLVMGlobalGetValueType(Val: LLVMValueRef) -> LLVMTypeRef;
@extern("LLVMAddAttributeAtIndex") func LLVMAddAttributeAtIndex(Val: LLVMValueRef, index: u32, Attr: LLVMAttributeRef);

// LLVM Globals
@extern("LLVMSetInitializer") func LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef);

@extern("LLVMGetParam") func LLVMGetParam(FnRef: LLVMValueRef, index: u32) -> LLVMValueRef;
@extern("LLVMSetValueName2") func LLVMSetValueName2(Val: LLVMValueRef, Name: &char, NameLen: usize);
@extern("LLVMSetValueName") func LLVMSetValueName(Val: LLVMValueRef, Name: &char);
@extern("LLVMGetValueName2") func LLVMGetValueName2(Val: LLVMValueRef, Length: &usize) -> &char;
@extern("LLVMGetValueName") func LLVMGetValueName(Val: LLVMValueRef) -> &char;

// LLVMAttribute
@extern("LLVMGetEnumAttributeKindForName") func LLVMGetEnumAttributeKindForName(Name: &char, SLen: usize) -> u32;

struct LLVMBool { val: i32; }
func newLLVMBool(b: bool) -> LLVMBool {
    if (b) return LLVMBool { val: 1 };
    return LLVMBool { val: 0 };
}

struct LLVMString { chars: &char; }

// LLVMType
@extern("LLVMFunctionType") func LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: &LLVMTypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVMTypeRef;
@extern("LLVMPointerType") func LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: u32) -> LLVMTypeRef;
@extern("LLVMArrayType") func LLVMArrayType(ElementType: LLVMTypeRef, size: u32) -> LLVMTypeRef;
@extern("LLVMGetParamTypes") func LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: &LLVMTypeRef);
@extern("LLVMCountParamTypes") func LLVMCountParamTypes(FunctionTy: LLVMTypeRef) -> u32;
@extern("LLVMGetReturnType") func LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
@extern("LLVMPrintTypeToString") func LLVMPrintTypeToString(Ty: LLVMTypeRef) -> &char;

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
@extern("LLVMGetTypeKind") func LLVMGetTypeKind(Ty: LLVMTypeRef) -> i32;

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
