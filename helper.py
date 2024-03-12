from typing import List
import subprocess
import os
import sys

from multiprocessing import Pool
from typing import Tuple
from dataclasses import dataclass
from enum import Enum

COMPILER_PATH = "./target/debug/bufo"

USE_OLD_CODEGEN = False

def compare(expected: List[str], actual: List[str]) -> bool:
    for line in expected:
        inside = False
        for other in actual:
            if line in other:
                inside = True
                break
        if not inside:
            return False
    return True


def call_cmd(cmd: List, log: bool = True) -> Tuple[str, subprocess.CompletedProcess[bytes]]:
    if log:
        buf = "[CMD] " + " ".join(cmd) + "\n"
        return buf, subprocess.run(cmd, capture_output=True)
    return "\n", subprocess.run(cmd, capture_output=True)

class STATE(Enum):
    SUCCESS = 0
    FAILURE = 1
    PANIC = 2
    INVALID = 3
    IGNORED = 4
    DONT_TEST = 5

@dataclass
class TestResult:
    path: str
    success: STATE

def run_test(path: str, exec: bool) -> TestResult:
    print_buffer = "Running test: " + path + "\n"
    """
    The protocol for tests is as follows:
    //! THIS IS A TEST PROGRAM
    //! {RUNTIME|COMPILER}
    //! {FAILURE|SUCCESS}
    //! CODE: <error code> (only if FAILURE)
    //! ERROR: (only if FAILURE)
    //! <error message> (only if FAILURE)
    //! <error message> (only if FAILURE)
    //! <...>
    <mandatory newline>
    <code>
    """
    with open(path, "r") as f:
        lines = f.readlines()
        if lines[0].startswith("//! IGNORE"):
            print_buffer += "Ignoring test: " + path + "\n"
            print(print_buffer)
            return TestResult(path, STATE.DONT_TEST)
        if not lines[0].startswith("//! THIS IS A TEST PROGRAM"):
            print_buffer += "Invalid test file: " + path + "\n"
            print_buffer += "First line must be `//! THIS IS A TEST PROGRAM`\n"
            print(print_buffer, file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        point_of_failure = lines[1].removeprefix("//! ").upper().strip()
        if point_of_failure not in ["RUNTIME", "COMPILER"]:
            print_buffer += "Invalid test file: " + path + "\n"
            print_buffer += "Second line must be either `//! RUNTIME` or `//! COMPILETIME`\n"
            print(print_buffer, file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        if point_of_failure == "RUNTIME" and not exec:
            print_buffer += "Ignoring test: " + path + "\n"
            print(print_buffer, file=sys.stderr)
            return TestResult(path, STATE.IGNORED)

        expected_mode = lines[2].removeprefix("//! ").upper().strip()
        if expected_mode not in ["FAILURE", "SUCCESS"]:
            print_buffer += "Invalid test file: " + path + "\n"
            print_buffer += "Third line must be either `//! FAILURE` or `//! SUCCESS`\n"
            print(print_buffer, file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        if expected_mode == "FAILURE":
            if not lines[3].startswith("//! CODE: "):
                print_buffer += "Invalid test file: " + path + "\n"
                print_buffer += "For failures, fourth line must be `//! CODE: <error code>`\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            expected_error_code = int(lines[3].removeprefix("//! CODE: ").strip())
        else:
            expected_error_code = 0
        
        if expected_mode == "FAILURE":
            if not lines[4].startswith("//! ERROR:"):
                print_buffer += "Invalid test file: " + path + "\n"
                print_buffer += "For failures, fifth line must be `//! ERROR:`\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            index = 5
            error_lines = []
            while index < len(lines) and lines[index].startswith("//! "):
                error_lines.append(lines[index].removeprefix("//! ").strip())
                index += 1
        else:
            error_lines = []

        buf, output = call_cmd([COMPILER_PATH, "-i", path, "-vd"])
        if point_of_failure == "RUNTIME":
            print_buffer += buf
            if output.returncode == 101:
                print_buffer += "The compiler panicked\n"
                print_buffer += f"Output: {output.stdout.decode('utf-8')}\n"
                print_buffer += f"Error: {output.stderr.decode('utf-8')}\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.PANIC)
            if output.returncode != 0:
                print_buffer += "Compilation failed\n"
                print_buffer += output.stderr.decode("utf-8")
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            # if on Windows, split path by backslashes, otherwise by forward slashes
            filename = path.split("\\")[-1].split(".")[0] if os.name == "nt" else path.split("/")[-1].split(".")[0]
            buf, output = call_cmd(["./out/" + filename + ".exe"])
            print_buffer += buf
            # We're not generating assembly in LLVM mode
            if USE_OLD_CODEGEN: os.remove("./out/" + filename + ".asm")
            os.remove("./out/" + filename + ".exe")
        else:
            print_buffer += buf
        # stdout = output.stdout.decode("utf-8").split('\n')
        stderr = output.stderr.decode("utf-8").split('\n')
        if output.returncode == 101:
            print_buffer += "The compiler panicked\n"
            print_buffer += output.stderr.decode("utf-8")
            print(print_buffer, file=sys.stderr)
            return TestResult(path, STATE.PANIC)
        if expected_mode == "FAILURE":
            if not compare(error_lines, stderr):
                print_buffer += "Expected error message not found\n"
                print_buffer += "Expected:\n"
                print_buffer += "\n".join(error_lines) + "\n"
                print_buffer += "Actual:\n"
                print_buffer += "\n".join(stderr) + "\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.FAILURE)
            if output.returncode != expected_error_code:
                print_buffer += "Expected error code " + str(expected_error_code) + ", but got " + str(output.returncode) + "\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.FAILURE)
        elif expected_mode == "SUCCESS":
            if output.returncode != expected_error_code:
                print_buffer += "Expected error code " + str(expected_error_code) + ", but got " + str(output.returncode) + "\n"
                print(print_buffer, file=sys.stderr)
                return TestResult(path, STATE.FAILURE)
        print(print_buffer)
        return TestResult(path, STATE.SUCCESS)

def recompile_compiler(trace: bool = False) -> None:
    print("Recompiling compiler...")
    cargo = ["cargo", "build"]
    cargo = cargo + ["--features=trace"] if trace else cargo
    cargo = cargo + ["--features=old_codegen"] if USE_OLD_CODEGEN else cargo
    buf, cmd = call_cmd(cargo)
    print(buf)
    if cmd.returncode != 0:
        print("Failed to recompile compiler", file=sys.stderr)
        print(cmd.stderr.decode("utf-8"), file=sys.stderr)
        sys.exit(1)
    print("Recompilation successful")

def run_all_tests(exec: bool = True, exit_first_failure: bool = False):
    total = 0
    failed_tests = []
    panicked_tests = []
    invalid_tests = []
    ignored_tests = []
    all_tests = []
    for root, _, files in os.walk("./tests"):
        for filename in files:
            path = os.path.join(root, filename)
            if os.path.isfile(path) and path.endswith(".bu"):
                all_tests.append(path)

    if exit_first_failure:
        for path in all_tests:
            result = run_test(path, exec)
            total += 1
            match result.success:
                case STATE.SUCCESS:
                    pass
                case STATE.FAILURE:
                    failed_tests.append(result.path)
                    break
                case STATE.PANIC:
                    panicked_tests.append(result.path)
                    break
                case STATE.INVALID:
                    invalid_tests.append(result.path)
                    break
                case STATE.IGNORED:
                    ignored_tests.append(result.path)
                    break
                case STATE.DONT_TEST:
                    total -= 1
    else:
        with Pool(16) as p:
            results = p.starmap(run_test, [(path, exec) for path in all_tests])
            for result in results:
                total += 1
                match result.success:
                    case STATE.SUCCESS:
                        pass
                    case STATE.FAILURE:
                        failed_tests.append(result.path)
                    case STATE.PANIC:
                        panicked_tests.append(result.path)
                    case STATE.INVALID:
                        invalid_tests.append(result.path)
                    case STATE.IGNORED:
                        ignored_tests.append(result.path)
                    case STATE.DONT_TEST:
                        total -= 1

    def print_tests(tests: List[str], s: str) -> None:
        if len(tests) > 0:
            print(f"\n{s} tests:")
            for test in tests:
                print(test)

    print_tests(ignored_tests, "Ignored")
    print_tests(failed_tests, "Failed")
    print_tests(panicked_tests, "Panicked")
    print_tests(invalid_tests, "Invalid")
    ignored = len(ignored_tests)
    failure = len(failed_tests)
    panicked = len(panicked_tests)
    invalid = len(invalid_tests)
    success = total - failure - panicked - invalid - ignored
    print(f"\nTotal: {total}, Success: {success}, Failure: {failure}, Invalid: {invalid}, Panicked: {panicked}, Ignored: {ignored}")
    if failure > 0 or panicked > 0 or invalid > 0:
        sys.exit(1)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print("Usage: python helper.py [test|bench]")
    else:
        mode = sys.argv[1]
        if mode == "test":
            trace = "--trace" in sys.argv
            recompile_compiler(trace=trace)
            print("Running tests...")
            no_exec = "--no-exec" in sys.argv
            exit_first_failure = "--exit-first-failure" in sys.argv
            run_all_tests(exec=not no_exec, exit_first_failure=exit_first_failure)
        elif mode == "bench":
            recompile_compiler()
            print("Running benchmarks...")
            assert False, "Not implemented"
        else:
            print("Usage: python helper.py [test|bench]")
    # main()