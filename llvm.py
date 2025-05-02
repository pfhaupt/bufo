
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
    elif sys.platform == "win32":
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
    elif sys.platform == "win32":
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
    elif sys.platform == "win32":
        cmd = ["lib.exe", "/out:llvm_wrapper.lib", "target.obj"]
    else:
        assert False, f"Unsupported OS {sys.platform}, can't link wrapper"
    proc = try_call(cmd)
    assert proc.returncode == 0, proc.stderr.decode("utf-8")

with open("./src/backend/LLVM/bindings.bufo", "w") as file:
    path = get(["--libdir"])[0]
    libs = get(["--libnames"], " ")
    content = ""
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
    content += " module LLVM {\n"
    if sys.platform == "linux":
        content += "    @os(LINUX) "
    elif sys.platform == "win32":
        content += "    @os(WINDOWS) "
    else:
        assert False, f"Unsupported OS {sys.platform}, can't specify @os() attribute"
    content += "config {\n"
    compile_wrapper(path, libs)
    if sys.platform == "linux":
        content += "        library: \":llvm_wrapper.a\";\n"
    elif sys.platform == "win32":
        content += "        library: \"llvm_wrapper\",\n"
    for l in libs:
        _l = l.replace("\n", "")
        if _l.endswith(".lib"):
            _l = _l.removesuffix(".lib")
        if sys.platform == "linux":
            content += f"        library: \":{_l}\";\n"
        elif sys.platform == "win32":
            content += f"        library: \"{_l}\",\n"
    if sys.platform == "win32":
        path = path.replace("\\", "\\\\")
    content += "        libpath: \".\",\n"
    content += f"        libpath: \"{path}\",\n"
    content += "    }\n"
    content += """
    // LLVMTarget
    @extern("LLVMGetTargetFromName") func GetTargetFromName(name: &char) -> LLVMTargetRef;
    @extern("LLVM_InitializeNativeAsmParser") func _InitializeNativeAsmParser() -> i32;
    @extern("LLVM_InitializeNativeAsmPrinter") func _InitializeNativeAsmPrinter() -> i32;
    @extern("LLVM_InitializeNativeDisassembler") func _InitializeNativeDisassembler() -> i32;
    @extern("LLVM_InitializeNativeTarget") func _InitializeNativeTarget() -> i32;
    @extern("LLVMCreateTargetMachine") func CreateTargetMachine(T: LLVMTargetRef, Triple: &char, CPU: &char, Features: &char, Level: i32, Reloc: i32, CodeModel: i32) -> LLVMTargetMachineRef;
    @extern("LLVMTargetMachineEmitToFile") func TargetMachineEmitToFile(T: LLVMTargetMachineRef, M: LLVMModuleRef, path: &char, opts: i32, err: &LLVMString) -> i32;
    @extern("LLVMCreateTargetDataLayout") func CreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
    @extern("LLVMStoreSizeOfType") func StoreSizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
    @extern("LLVMABISizeOfType") func ABISizeOfType(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
    @extern("LLVMSizeOfTypeInBits") func SizeOfTypeInBits(T: LLVMTargetDataRef, Ty: LLVMTypeRef) -> usize;
    @extern("LLVMCopyStringRepOfTargetData") func CopyStringRepOfTargetData(T: LLVMTargetDataRef) -> &char;

    // LLVMContext
    @extern("LLVMContextCreate") func ContextCreate() -> LLVMContextRef;
    @extern("LLVMContextDispose") func ContextDispose(context: LLVMContextRef);
    @extern("LLVMModuleCreateWithNameInContext") func ModuleCreateWithNameInContext(id: &char, C: LLVMContextRef) -> LLVMModuleRef;
    @extern("LLVMIntTypeInContext") func IntTypeInContext(c: LLVMContextRef, bits: u32) -> LLVMTypeRef;
    @extern("LLVMFloatTypeInContext") func FloatTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
    @extern("LLVMDoubleTypeInContext") func DoubleTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
    @extern("LLVMVoidTypeInContext") func VoidTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
    @extern("LLVMStructTypeInContext") func StructTypeInContext(c: LLVMContextRef, ElementTypes: &LLVMTypeRef, ElementCount: u32, Packed: LLVMBool) -> LLVMTypeRef;
    @extern("LLVMCreateEnumAttribute") func CreateEnumAttribute(c: LLVMContextRef, KindID: u32, Val: u64) -> LLVMAttributeRef;

    // LLVMModule
    @extern("LLVMPrintModuleToString") func PrintModuleToString(M: LLVMModuleRef) -> &char;
    @extern("LLVMPrintModuleToFile") func PrintModuleToFile(M: LLVMModuleRef, Filename: &char, ErrorMessage: &LLVMString) -> LLVMBool;
    @extern("LLVMGetNamedFunction") func GetNamedFunction(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMAddFunction") func AddFunction(M: LLVMModuleRef, name: &char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
    @extern("LLVMAddGlobalInAddressSpace") func AddGlobalInAddressSpace(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: &char, AddressSpace: u32) -> LLVMValueRef;
    @extern("LLVMGetNamedGlobal") func GetNamedGlobal(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMVerifyModule") func VerifyModule(M: LLVMModuleRef, mode: i32, code: &LLVMString) -> LLVMBool;
    @extern("LLVMSetDataLayout") func SetDataLayout(M: LLVMModuleRef, Data: &char);
    @extern("LLVMSetTarget") func SetTarget(M: LLVMModuleRef, Target: &char);

    // LLVMPassManager
    @extern("LLVMCreatePassManager") func CreatePassManager() -> LLVMPassManagerRef;
    @extern("LLVMAddPromoteMemoryToRegisterPass") func AddPromoteMemoryToRegisterPass(P: LLVMPassManagerRef);
    @extern("LLVMAddAlwaysInlinerPass") func AddAlwaysInlinerPass(P: LLVMPassManagerRef);
    @extern("LLVMAddCFGSimplificationPass") func AddCFGSimplificationPass(P: LLVMPassManagerRef);
    @extern("LLVMAddGlobalDCEPass") func AddGlobalDCEPass(P: LLVMPassManagerRef);
    @extern("LLVMRunPassManager") func RunPassManager(P: LLVMPassManagerRef, M: LLVMModuleRef) -> i32;

    // LLVMBasicBlock
    @extern("LLVMAppendBasicBlockInContext") func AppendBasicBlockInContext(C: LLVMContextRef, FnRef: LLVMValueRef, name: &char) -> LLVMBasicBlockRef;
    @extern("LLVMGetInsertBlock") func GetInsertBlock(Builder: LLVMBuilderRef) -> LLVMBasicBlockRef;
    @extern("LLVMGetBasicBlockParent") func GetBasicBlockParent(Block: LLVMBasicBlockRef) -> LLVMValueRef;
    @extern("LLVMGetFirstBasicBlock") func GetFirstBasicBlock(Fn: LLVMValueRef) -> LLVMBasicBlockRef;
    @extern("LLVMGetLastInstruction") func GetLastInstruction(Block: LLVMBasicBlockRef) -> LLVMValueRef;

    // LLVMBuilder
    @extern("LLVMCreateBuilderInContext") func CreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef;
    @extern("LLVMCreateBuilder") func CreateBuilder() -> LLVMBuilderRef;
    @extern("LLVMBuildRetVoid") func BuildRetVoid(B: LLVMBuilderRef) -> LLVMValueRef;
    @extern("LLVMBuildRet") func BuildRet(B: LLVMBuilderRef, Value: LLVMValueRef) -> LLVMValueRef;
    @extern("LLVMPositionBuilderAtEnd") func PositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);
    @extern("LLVMPositionBuilderBefore") func PositionBuilderBefore(Builder: LLVMBuilderRef, Instr: LLVMValueRef);
    @extern("LLVMBuildAlloca") func BuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildStore") func BuildStore(B: LLVMBuilderRef, Value: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
    @extern("LLVMBuildInsertValue") func BuildInsertValue(B: LLVMBuilderRef, AggVal: LLVMValueRef, EltVal: LLVMValueRef, Index: u32, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildBr") func BuildBr(B: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;
    @extern("LLVMBuildCondBr") func BuildCondBr(B: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef) -> LLVMValueRef;
    @extern("LLVMBuildLoad2") func BuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Ptr: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildAdd") func BuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildSub") func BuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildMul") func BuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildSDiv") func BuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildUDiv") func BuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildSRem") func BuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildURem") func BuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFAdd") func BuildFAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFSub") func BuildFSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFMul") func BuildFMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFDiv") func BuildFDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFRem") func BuildFRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildICmp") func BuildICmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFCmp") func BuildFCmp(B: LLVMBuilderRef, pred: i32, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildNot") func BuildNot(B: LLVMBuilderRef, Val: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildOr") func BuildOr(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildAnd") func BuildAnd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildXor") func BuildXor(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildPtrToInt") func BuildPtrToInt(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildIntToPtr") func BuildIntToPtr(B: LLVMBuilderRef, Ptr: LLVMValueRef, Int: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildSIToFP") func BuildSIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildUIToFP") func BuildUIToFP(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFPToSI") func BuildFPToSI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFPToUI") func BuildFPToUI(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildCall2") func BuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: &LLVMValueRef, NumArgs: u32, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildGlobalStringPtr") func BuildGlobalStringPtr(B: LLVMBuilderRef, Str: &char, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildSExt") func BuildSExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildZExt") func BuildZExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildTrunc") func BuildTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildStructGEP2") func BuildStructGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Idx: u32, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildGEP2") func BuildGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: &LLVMValueRef, Length: u32, Name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFPExt") func BuildFPExt(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildFPTrunc") func BuildFPTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, Typ: LLVMTypeRef, name: &char) -> LLVMValueRef;
    @extern("LLVMBuildUnreachable") func BuildUnreachable(B: LLVMBuilderRef) -> LLVMValueRef;

    // LLVMValue
    @extern("LLVMTypeOf") func TypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
    @extern("LLVMConstInt") func ConstInt(IntTy: LLVMTypeRef, N: usize, SignExtend: LLVMBool) -> LLVMValueRef;
    @extern("LLVMConstReal") func ConstReal(RealTy: LLVMTypeRef, F: f64) -> LLVMValueRef;
    @extern("LLVMConstNull") func ConstNull(Ty: LLVMTypeRef) -> LLVMValueRef;
    @extern("LLVMDumpValue") func DumpValue(V: LLVMValueRef);
    @extern("LLVMGlobalGetValueType") func GlobalGetValueType(Val: LLVMValueRef) -> LLVMTypeRef;
    @extern("LLVMAddAttributeAtIndex") func AddAttributeAtIndex(Val: LLVMValueRef, index: u32, Attr: LLVMAttributeRef);

    // LLVM Globals
    @extern("LLVMSetInitializer") func SetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef);

    @extern("LLVMGetParam") func GetParam(FnRef: LLVMValueRef, index: u32) -> LLVMValueRef;
    @extern("LLVMSetValueName2") func SetValueName2(Val: LLVMValueRef, Name: &char, NameLen: usize);
    @extern("LLVMSetValueName") func SetValueName(Val: LLVMValueRef, Name: &char);
    @extern("LLVMGetValueName2") func GetValueName2(Val: LLVMValueRef, Length: &usize) -> &char;
    @extern("LLVMGetValueName") func GetValueName(Val: LLVMValueRef) -> &char;

    // LLVMAttribute
    @extern("LLVMGetEnumAttributeKindForName") func GetEnumAttributeKindForName(Name: &char, SLen: usize) -> u32;

    // LLVMType
    @extern("LLVMFunctionType") func FunctionType(ReturnType: LLVMTypeRef, ParamTypes: &LLVMTypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVMTypeRef;
    @extern("LLVMPointerType") func PointerType(ElementType: LLVMTypeRef, AddressSpace: u32) -> LLVMTypeRef;
    @extern("LLVMArrayType") func ArrayType(ElementType: LLVMTypeRef, size: u32) -> LLVMTypeRef;
    @extern("LLVMGetParamTypes") func GetParamTypes(FunctionTy: LLVMTypeRef, Dest: &LLVMTypeRef);
    @extern("LLVMCountParamTypes") func CountParamTypes(FunctionTy: LLVMTypeRef) -> u32;
    @extern("LLVMGetReturnType") func GetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
    @extern("LLVMPrintTypeToString") func PrintTypeToString(Ty: LLVMTypeRef) -> &char;

    @extern("LLVMGetTypeKind") func GetTypeKind(Ty: LLVMTypeRef) -> i32;
}
struct LLVMBool { val: i32; }
func newLLVMBool(b: bool) -> LLVMBool {
    if (b) return LLVMBool { val: 1 };
    return LLVMBool { val: 0 };
}

struct LLVMString { chars: &char; }


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
