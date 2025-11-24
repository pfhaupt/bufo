
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
        c = ["llvm-config"]
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

def assert_run(cmd: List[str]):
    proc = try_call(cmd)
    if proc.returncode != 0:
        print("stdout", proc.stdout.decode("utf-8"))
        print("stderr", proc.stderr.decode("utf-8"))
        exit(2)

def compile_file(cmds: List[List[str]], path: str, llvm_path: str, libs: List[str], *args) -> str:
    if sys.platform == "linux":
        out = path.replace(".cpp", ".c")
        out = out.replace(".c", ".o")
        include = llvm_path + "/../include/"
        cmd = ["cc", "-c", path, f"-I{include}", f"-L{llvm_path}", "-o", f"{out}"]
        for l in libs:
            cmd.append("-l" + l.replace("\n", ""))
    elif sys.platform == "win32":
        path = path.replace("/", "\\")
        out = path.replace(".cpp", ".c")
        out = out.replace(".c", ".obj")
        include = llvm_path + "\\..\\include\\"
        cmd = ["cl.exe", "/c", path, f"/I{include}", f"/Fo:{out}"]
        for f in args:
            cmd.append(f)
        #for l in libs:
        #    cmd.append(llvm_path + "\\" + l.replace("\n", ""))
    else:
        assert False, f"Unsupported OS {sys.platform}, can't compile wrapper"
    cmds.append(cmd)
    return out

def link_file(cmds: List[List[str]], file: str) -> str:
    if sys.platform == "linux":
        out = file.replace(".o", ".a")
        cmds += [
            ["ar", "rcs", out, file],
        ]
    elif sys.platform == "win32":
        out = file.replace(".obj", ".lib")
        cmds += [
            ["lib.exe", f"/out:{out}", file],
        ]
    else:
        assert False, f"Unsupported OS {sys.platform}, can't link wrapper"
    return out

def compile_wrapper(llvm_path, libs):
    cmds = []
    target_obj = compile_file(cmds, "./wrapper/target.c", llvm_path, libs)
    variant_obj = compile_file(cmds, "./wrapper/variant.cpp", llvm_path, libs, "/std:c++20")
    target_lib = link_file(cmds, target_obj)
    variant_lib = link_file(cmds, variant_obj)
    for cmd in cmds:
        assert_run(cmd)
    print("[INFO] Generated LLVM wrapper")

def main():
    path = get(["--libdir"])[0]
    libs = get(["--libnames"], " ")
    compile_wrapper(path, libs)

if __name__ == "__main__":
    main()
