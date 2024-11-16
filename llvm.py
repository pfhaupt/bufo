
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

with open("./stage1/backend/LLVM/bindings.bufo", "w") as file:
    path = get("--libdir")[0]
    libs = get("--libnames", " ")
    content = "compiler_flags {\n"
    for l in libs:
        _l = l.replace("\n", "")
        content += f"  library: \"{_l}\";\n"
    content += f"  libpath: \"{path}\";\n"
    content += "}\n"
    content += enumeratePtrs([
        "LLVMContextRef",
        "LLVMModuleRef",
        "LLVMBuilderRef",
        "LLVMBasicBlockRef",
        "LLVMValueRef",
        "LLVMTypeRef",
    ])
    content += """
// LLVMContext
extern LLVMContextCreate() -> LLVMContextRef;
extern LLVMContextDispose(context: LLVMContextRef);
extern LLVMModuleCreateWithNameInContext(id: &char, C: LLVMContextRef) -> LLVMModuleRef;
extern LLVMIntTypeInContext(c: LLVMContextRef, bits: u32) -> LLVMTypeRef;
extern LLVMVoidTypeInContext(c: LLVMContextRef) -> LLVMTypeRef;
extern LLVMStructTypeInContext(c: LLVMContextRef, ElementTypes: &LLVMTypeRef, ElementCount: u32, Packed: LLVMBool) -> LLVMTypeRef;

// LLVMModule
extern LLVMPrintModuleToString(module: LLVMModuleRef) -> &char;
extern LLVMGetNamedFunction(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
extern LLVMAddFunction(M: LLVMModuleRef, name: &char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;

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

// LLVMValue
extern LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
extern LLVMConstInt(IntTy: LLVMTypeRef, N: usize, SignExtend: LLVMBool) -> LLVMValueRef;
extern LLVMConstNull(Ty: LLVMTypeRef) -> LLVMValueRef;

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

// LLVMType
extern LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: &LLVMTypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVMTypeRef;
extern LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: u32) -> LLVMTypeRef;
extern LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: &mut LLVMTypeRef);
extern LLVMCountParamTypes(FunctioNTy: LLVMTypeRef) -> u32;
extern LLVMPrintTypeToString(Ty: LLVMTypeRef) -> &char;

comptime LLVMVoidTypeKind: u32 = 0;
comptime LLVMHalfTypeKind: u32 = 1;
comptime LLVMFloatTypeKind: u32 = 2;
comptime LLVMDoubleTypeKind: u32 = 3;
comptime LLVMX86_FP80TypeKind: u32 = 4;
comptime LLVMFP128TypeKind: u32 = 5;
comptime LLVMPPC_FP128TypeKind: u32 = 6;
comptime LLVMLabelTypeKind: u32 = 7;
comptime LLVMIntegerTypeKind: u32 = 8;
comptime LLVMFunctionTypeKind: u32 = 9;
comptime LLVMStructTypeKind: u32 = 10;
comptime LLVMArrayTypeKind: u32 = 11;
comptime LLVMPointerTypeKind: u32 = 12;
comptime LLVMVectorTypeKind: u32 = 13;
comptime LLVMMetadataTypeKind: u32 = 14;
// Unused according to https://llvm.org/doxygen/llvm-c_2Core_8h_source.html line 164
// comptime LLVMX86_MMXTypeKind: u32 = 15;
comptime LLVMTokenTypeKind: u32 = 16;
comptime LLVMScalableVectorTypeKind: u32 = 17;
comptime LLVMBFloatTypeKind: u32 = 18;
comptime LLVMX86_AMXTypeKind: u32 = 19;
comptime LLVMTargetExtTypeKind: u32 = 20;
extern LLVMGetTypeKind(Ty: LLVMTypeRef) -> u32;
"""
    file.write(content)
