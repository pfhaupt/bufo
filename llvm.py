
from typing import List
import subprocess
def call_cmd(cmd: List) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(cmd, capture_output=True)

def get(what: str, split_by: str = "\n") -> List[str]:
    out = call_cmd(["llvm-config", what])
    assert out.returncode == 0
    output = out.stdout.decode('utf-8')
    return output.split(split_by)

with open("./stage1/backend/llvm_bindings.bufo", "w") as file:
    path = get("--libdir")[0]
    libs = get("--libnames", " ")
    content = "compiler_flags {\n"
    for l in libs:
        _l = l.replace("\n", "")
        content += f"  library: \"{_l}\";\n"
    content += f"  libpath: \"{path}\";\n"
    content += "}\n"
    content += """
import "std/string.bufo";

struct LLVMContextRef { ptr: Any; }
extern LLVMContextCreate() -> LLVMContextRef;
extern LLVMContextDispose(context: LLVMContextRef);
extern LLVMAppendBasicBlockInContext(C: LLVMContextRef, FnRef: LLVMValueRef, name: &char) -> LLVMBasicBlockRef;

struct LLVMModuleRef { ptr: Any; }
extern LLVMModuleCreateWithNameInContext(id: &char, C: LLVMContextRef) -> LLVMModuleRef;
extern LLVMPrintModuleToString(module: LLVMModuleRef) -> &char;

struct LLVMBuilderRef { ptr: Any; }
extern LLVMCreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef;
extern LLVMCreateBuilder() -> LLVMBuilderRef;
extern LLVMBuildRetVoid(B: LLVMBuilderRef);
extern LLVMBuildRet(B: LLVMBuilderRef, Value: LLVMValueRef);
extern LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef);

struct LLVMBasicBlockRef { ptr: Any; }

struct LLVMValueRef { ptr: Any; }
extern LLVMGetNamedFunction(M: LLVMModuleRef, Name: &char) -> LLVMValueRef;
extern LLVMAddFunction(M: LLVMModuleRef, name: &char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
extern LLVMConstInt(IntTy: LLVMTypeRef, N: usize, SignExtend: LLVMBool) -> LLVMValueRef;

struct LLVMBool { val: i32; }

struct LLVMTypeRef { ptr: Any; }
extern LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: &LLVMTypeRef, ParamCount: u32, IsVarArg: LLVMBool) -> LLVMTypeRef;
extern LLVMIntTypeInContext(c: LLVMContextRef, bits: u32) -> LLVMTypeRef;

func False() -> LLVMBool {
    return LLVMBool { val: 0 };
}
"""
    file.write(content)
