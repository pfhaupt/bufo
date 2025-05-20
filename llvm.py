
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
    func isNull(this: LLVM::{p}) -> bool {{
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
    content += " module LLVM {\n"
    content += enumeratePtrs([
        "TargetMachineRef",
        "TargetRef",
        "TargetDataRef",
        "PassManagerRef",
        "ContextRef",
        "ModuleRef",
        "BuilderRef",
        "BasicBlockRef",
        "ValueRef",
        "TypeRef",
        "AttributeRef",
    ])
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
    @extern("LLVMGetTargetFromName") func GetTargetFromName(name: &char) -> LLVM::TargetRef;
    @extern("LLVM_InitializeNativeAsmParser") func _InitializeNativeAsmParser() -> i32;
    @extern("LLVM_InitializeNativeAsmPrinter") func _InitializeNativeAsmPrinter() -> i32;
    @extern("LLVM_InitializeNativeDisassembler") func _InitializeNativeDisassembler() -> i32;
    @extern("LLVM_InitializeNativeTarget") func _InitializeNativeTarget() -> i32;
    @extern("LLVMCreateTargetMachine") func CreateTargetMachine(T: LLVM::TargetRef, Triple: &char, CPU: &char, Features: &char, Level: i32, Reloc: i32, CodeModel: i32) -> LLVM::TargetMachineRef;
    @extern("LLVMTargetMachineEmitToFile") func TargetMachineEmitToFile(T: LLVM::TargetMachineRef, M: LLVM::ModuleRef, path: &char, opts: i32, err: &LLVMString) -> i32;
    @extern("LLVMCreateTargetDataLayout") func CreateTargetDataLayout(T: LLVM::TargetMachineRef) -> LLVM::TargetDataRef;
    @extern("LLVMStoreSizeOfType") func StoreSizeOfType(T: LLVM::TargetDataRef, Ty: LLVM::TypeRef) -> usize;
    @extern("LLVMABISizeOfType") func ABISizeOfType(T: LLVM::TargetDataRef, Ty: LLVM::TypeRef) -> usize;
    @extern("LLVMSizeOfTypeInBits") func SizeOfTypeInBits(T: LLVM::TargetDataRef, Ty: LLVM::TypeRef) -> usize;
    @extern("LLVMCopyStringRepOfTargetData") func CopyStringRepOfTargetData(T: LLVM::TargetDataRef) -> &char;

    // LLVMContext
    @extern("LLVMContextCreate") func ContextCreate() -> LLVM::ContextRef;
    @extern("LLVMContextDispose") func ContextDispose(context: LLVM::ContextRef);
    @extern("LLVMModuleCreateWithNameInContext") func ModuleCreateWithNameInContext(id: &char, C: LLVM::ContextRef) -> LLVM::ModuleRef;
    @extern("LLVMIntTypeInContext") func IntTypeInContext(c: LLVM::ContextRef, bits: u32) -> LLVM::TypeRef;
    @extern("LLVMFloatTypeInContext") func FloatTypeInContext(c: LLVM::ContextRef) -> LLVM::TypeRef;
    @extern("LLVMDoubleTypeInContext") func DoubleTypeInContext(c: LLVM::ContextRef) -> LLVM::TypeRef;
    @extern("LLVMVoidTypeInContext") func VoidTypeInContext(c: LLVM::ContextRef) -> LLVM::TypeRef;
    @extern("LLVMStructTypeInContext") func StructTypeInContext(c: LLVM::ContextRef, ElementTypes: &LLVM::TypeRef, ElementCount: u32, Packed: LLVMBool) -> LLVM::TypeRef;
    @extern("LLVMCreateEnumAttribute") func CreateEnumAttribute(c: LLVM::ContextRef, KindID: u32, Val: u64) -> LLVM::AttributeRef;

    // LLVMModule
    @extern("LLVMPrintModuleToString") func PrintModuleToString(M: LLVM::ModuleRef) -> &char;
    @extern("LLVMPrintModuleToFile") func PrintModuleToFile(M: LLVM::ModuleRef, Filename: &char, ErrorMessage: &LLVMString) -> LLVMBool;
    @extern("LLVMGetNamedFunction") func GetNamedFunction(M: LLVM::ModuleRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMAddFunction") func AddFunction(M: LLVM::ModuleRef, name: &char, FunctionTy: LLVM::TypeRef) -> LLVM::ValueRef;
    @extern("LLVMAddGlobalInAddressSpace") func AddGlobalInAddressSpace(M: LLVM::ModuleRef, Ty: LLVM::TypeRef, Name: &char, AddressSpace: u32) -> LLVM::ValueRef;
    @extern("LLVMGetNamedGlobal") func GetNamedGlobal(M: LLVM::ModuleRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMVerifyModule") func VerifyModule(M: LLVM::ModuleRef, mode: i32, code: &LLVMString) -> LLVMBool;
    @extern("LLVMSetDataLayout") func SetDataLayout(M: LLVM::ModuleRef, Data: &char);
    @extern("LLVMSetTarget") func SetTarget(M: LLVM::ModuleRef, Target: &char);

    // LLVMPassManager
    @extern("LLVMCreatePassManager") func CreatePassManager() -> LLVM::PassManagerRef;
    @extern("LLVMAddPromoteMemoryToRegisterPass") func AddPromoteMemoryToRegisterPass(P: LLVM::PassManagerRef);
    @extern("LLVMAddAlwaysInlinerPass") func AddAlwaysInlinerPass(P: LLVM::PassManagerRef);
    @extern("LLVMAddCFGSimplificationPass") func AddCFGSimplificationPass(P: LLVM::PassManagerRef);
    @extern("LLVMAddGlobalDCEPass") func AddGlobalDCEPass(P: LLVM::PassManagerRef);
    @extern("LLVMRunPassManager") func RunPassManager(P: LLVM::PassManagerRef, M: LLVM::ModuleRef) -> i32;

    // LLVMBasicBlock
    @extern("LLVMAppendBasicBlockInContext") func AppendBasicBlockInContext(C: LLVM::ContextRef, FnRef: LLVM::ValueRef, name: &char) -> LLVM::BasicBlockRef;
    @extern("LLVMGetInsertBlock") func GetInsertBlock(Builder: LLVM::BuilderRef) -> LLVM::BasicBlockRef;
    @extern("LLVMGetBasicBlockParent") func GetBasicBlockParent(Block: LLVM::BasicBlockRef) -> LLVM::ValueRef;
    @extern("LLVMGetFirstBasicBlock") func GetFirstBasicBlock(Fn: LLVM::ValueRef) -> LLVM::BasicBlockRef;
    @extern("LLVMGetLastInstruction") func GetLastInstruction(Block: LLVM::BasicBlockRef) -> LLVM::ValueRef;

    // LLVMBuilder
    @extern("LLVMCreateBuilderInContext") func CreateBuilderInContext(context: LLVM::ContextRef) -> LLVM::BuilderRef;
    @extern("LLVMCreateBuilder") func CreateBuilder() -> LLVM::BuilderRef;
    @extern("LLVMBuildRetVoid") func BuildRetVoid(B: LLVM::BuilderRef) -> LLVM::ValueRef;
    @extern("LLVMBuildRet") func BuildRet(B: LLVM::BuilderRef, Value: LLVM::ValueRef) -> LLVM::ValueRef;
    @extern("LLVMPositionBuilderAtEnd") func PositionBuilderAtEnd(Builder: LLVM::BuilderRef, Block: LLVM::BasicBlockRef);
    @extern("LLVMPositionBuilderBefore") func PositionBuilderBefore(Builder: LLVM::BuilderRef, Instr: LLVM::ValueRef);
    @extern("LLVMBuildAlloca") func BuildAlloca(B: LLVM::BuilderRef, Ty: LLVM::TypeRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildStore") func BuildStore(B: LLVM::BuilderRef, Value: LLVM::ValueRef, Ptr: LLVM::ValueRef) -> LLVM::ValueRef;
    @extern("LLVMBuildInsertValue") func BuildInsertValue(B: LLVM::BuilderRef, AggVal: LLVM::ValueRef, EltVal: LLVM::ValueRef, Index: u32, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildBr") func BuildBr(B: LLVM::BuilderRef, Dest: LLVM::BasicBlockRef) -> LLVM::ValueRef;
    @extern("LLVMBuildCondBr") func BuildCondBr(B: LLVM::BuilderRef, If: LLVM::ValueRef, Then: LLVM::BasicBlockRef, Else: LLVM::BasicBlockRef) -> LLVM::ValueRef;
    @extern("LLVMBuildLoad2") func BuildLoad2(B: LLVM::BuilderRef, Ty: LLVM::TypeRef, Ptr: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildAdd") func BuildAdd(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildSub") func BuildSub(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildMul") func BuildMul(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildSDiv") func BuildSDiv(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildUDiv") func BuildUDiv(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildSRem") func BuildSRem(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildURem") func BuildURem(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFAdd") func BuildFAdd(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFSub") func BuildFSub(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFMul") func BuildFMul(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFDiv") func BuildFDiv(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFRem") func BuildFRem(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildICmp") func BuildICmp(B: LLVM::BuilderRef, pred: i32, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFCmp") func BuildFCmp(B: LLVM::BuilderRef, pred: i32, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildNot") func BuildNot(B: LLVM::BuilderRef, Val: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildOr") func BuildOr(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildAnd") func BuildAnd(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildXor") func BuildXor(B: LLVM::BuilderRef, LHS: LLVM::ValueRef, RHS: LLVM::ValueRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildPtrToInt") func BuildPtrToInt(B: LLVM::BuilderRef, Ptr: LLVM::ValueRef, Int: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildIntToPtr") func BuildIntToPtr(B: LLVM::BuilderRef, Ptr: LLVM::ValueRef, Int: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildSIToFP") func BuildSIToFP(B: LLVM::BuilderRef, Val: LLVM::ValueRef, DestTy: LLVM::TypeRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildUIToFP") func BuildUIToFP(B: LLVM::BuilderRef, Val: LLVM::ValueRef, DestTy: LLVM::TypeRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFPToSI") func BuildFPToSI(B: LLVM::BuilderRef, Val: LLVM::ValueRef, DestTy: LLVM::TypeRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFPToUI") func BuildFPToUI(B: LLVM::BuilderRef, Val: LLVM::ValueRef, DestTy: LLVM::TypeRef, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildCall2") func BuildCall2(B: LLVM::BuilderRef, Ty: LLVM::TypeRef, Fn: LLVM::ValueRef, Args: &LLVM::ValueRef, NumArgs: u32, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildGlobalStringPtr") func BuildGlobalStringPtr(B: LLVM::BuilderRef, Str: &char, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildSExt") func BuildSExt(B: LLVM::BuilderRef, Val: LLVM::ValueRef, Typ: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildZExt") func BuildZExt(B: LLVM::BuilderRef, Val: LLVM::ValueRef, Typ: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildTrunc") func BuildTrunc(B: LLVM::BuilderRef, Val: LLVM::ValueRef, Typ: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildStructGEP2") func BuildStructGEP2(B: LLVM::BuilderRef, Ty: LLVM::TypeRef, Pointer: LLVM::ValueRef, Idx: u32, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildGEP2") func BuildGEP2(B: LLVM::BuilderRef, Ty: LLVM::TypeRef, Pointer: LLVM::ValueRef, Indices: &LLVM::ValueRef, Length: u32, Name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFPExt") func BuildFPExt(B: LLVM::BuilderRef, Val: LLVM::ValueRef, Typ: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildFPTrunc") func BuildFPTrunc(B: LLVM::BuilderRef, Val: LLVM::ValueRef, Typ: LLVM::TypeRef, name: &char) -> LLVM::ValueRef;
    @extern("LLVMBuildUnreachable") func BuildUnreachable(B: LLVM::BuilderRef) -> LLVM::ValueRef;
    @extern("LLVMBuildMemCpy") func BuildMemcpy(B: LLVM::BuilderRef, Dst: LLVM::ValueRef, DstAlign: u32, Src: LLVM::ValueRef, SrcAlign: u32, Size: LLVM::ValueRef) -> LLVM::ValueRef;

    // LLVMValue
    @extern("LLVMTypeOf") func TypeOf(Val: LLVM::ValueRef) -> LLVM::TypeRef;
    @extern("LLVMConstInt") func ConstInt(IntTy: LLVM::TypeRef, N: usize, SignExtend: LLVMBool) -> LLVM::ValueRef;
    @extern("LLVMConstReal") func ConstReal(RealTy: LLVM::TypeRef, F: f64) -> LLVM::ValueRef;
    @extern("LLVMConstNull") func ConstNull(Ty: LLVM::TypeRef) -> LLVM::ValueRef;
    @extern("LLVMDumpValue") func DumpValue(V: LLVM::ValueRef);
    @extern("LLVMGlobalGetValueType") func GlobalGetValueType(Val: LLVM::ValueRef) -> LLVM::TypeRef;
    @extern("LLVMAddAttributeAtIndex") func AddAttributeAtIndex(Val: LLVM::ValueRef, index: u32, Attr: LLVM::AttributeRef);

    // LLVM Globals
    @extern("LLVMSetInitializer") func SetInitializer(GlobalVar: LLVM::ValueRef, ConstantVal: LLVM::ValueRef);

    @extern("LLVMGetParam") func GetParam(FnRef: LLVM::ValueRef, index: u32) -> LLVM::ValueRef;
    @extern("LLVMSetValueName2") func SetValueName2(Val: LLVM::ValueRef, Name: &char, NameLen: usize);
    @extern("LLVMSetValueName") func SetValueName(Val: LLVM::ValueRef, Name: &char);
    @extern("LLVMGetValueName2") func GetValueName2(Val: LLVM::ValueRef, Length: &usize) -> &char;
    @extern("LLVMGetValueName") func GetValueName(Val: LLVM::ValueRef) -> &char;

    // LLVMAttribute
    @extern("LLVMGetEnumAttributeKindForName") func GetEnumAttributeKindForName(Name: &char, SLen: usize) -> u32;

    // LLVMType
    @extern("LLVMFunctionType") func FunctionType(ReturnType: LLVM::TypeRef, ParamTypes: &LLVM::TypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVM::TypeRef;
    @extern("LLVMPointerType") func PointerType(ElementType: LLVM::TypeRef, AddressSpace: u32) -> LLVM::TypeRef;
    @extern("LLVMArrayType") func ArrayType(ElementType: LLVM::TypeRef, size: u32) -> LLVM::TypeRef;
    @extern("LLVMGetParamTypes") func GetParamTypes(FunctionTy: LLVM::TypeRef, Dest: &LLVM::TypeRef);
    @extern("LLVMCountParamTypes") func CountParamTypes(FunctionTy: LLVM::TypeRef) -> u32;
    @extern("LLVMGetReturnType") func GetReturnType(FunctionTy: LLVM::TypeRef) -> LLVM::TypeRef;
    @extern("LLVMPrintTypeToString") func PrintTypeToString(Ty: LLVM::TypeRef) -> &char;

    @extern("LLVMGetTypeKind") func GetTypeKind(Ty: LLVM::TypeRef) -> i32;
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
