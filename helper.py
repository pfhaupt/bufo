from typing import List, Tuple
import subprocess
import os
import sys
import multiprocessing

COMPILER_PATH = ".\\target\\release\\bufo.exe"

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


def call_cmd(cmd: List) -> int:
    return subprocess.run(cmd, capture_output=True)

from dataclasses import dataclass
from enum import Enum

class STATE(Enum):
    SUCCESS = 0
    FAILURE = 1
    PANIC = 2
    INVALID = 3
    IGNORED = 4

@dataclass
class TestResult:
    path: str
    success: STATE

def run_test(info: Tuple[str, bool]) -> TestResult:
    path = info[0]
    exec = info[1]
    print("Running test: " + path)
    """
    The protocol for tests is as follows:
    //! THIS IS A TEST PROGRAM
    //! {RUNTIME|COMPILER}
    //! {FAILURE|SUCCESS}
    //! CODE: <error code> (only if FAILURE)
    //! ERROR: (only if FAILURE and COMPILER)
    //! <error message> (only if FAILURE and COMPILER)
    //! <error message> (only if FAILURE and COMPILER)
    //! <...>
    <mandatory newline>
    <code>
    """
    with open(path, "r") as f:
        lines = f.readlines()
        if not lines[0].startswith("//! THIS IS A TEST PROGRAM"):
            print("Invalid test file: " + path, file=sys.stderr)
            print("First line must be `//! THIS IS A TEST PROGRAM`", file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        point_of_failure = lines[1].removeprefix("//! ").upper().strip()
        if point_of_failure not in ["RUNTIME", "COMPILER"]:
            print("Invalid test file: " + path, file=sys.stderr)
            print("Second line must be either `//! RUNTIME` or `//! COMPILETIME`", file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        expected_mode = lines[2].removeprefix("//! ").upper().strip()
        if expected_mode not in ["FAILURE", "SUCCESS"]:
            print("Invalid test file: " + path, file=sys.stderr)
            print("Second line must be either `//! FAILURE` or `//! SUCCESS`", file=sys.stderr)
            return TestResult(path, STATE.INVALID)

        if expected_mode == "FAILURE":
            if not lines[3].startswith("//! CODE: "):
                print("Invalid test file: " + path, file=sys.stderr)
                print("For failures, fourth line must be `//! CODE: <error code>`", file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            expected_error_code = int(lines[3].removeprefix("//! CODE: ").strip())
        else:
            expected_error_code = 0
        
        if expected_mode == "FAILURE" and point_of_failure == "COMPILER":
            if not lines[4].startswith("//! ERROR:"):
                print("Invalid test file: " + path, file=sys.stderr)
                print("For failures at compiletime, fifth line must be `//! ERROR:`", file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            index = 5
            error_lines = []
            while index < len(lines) and lines[index].startswith("//! "):
                error_lines.append(lines[index].removeprefix("//! ").strip())
                index += 1
        else:
            error_lines = []

        if point_of_failure == "RUNTIME":
            output = call_cmd([COMPILER_PATH, "-i", path])
            if output.returncode != 0:
                print("Compilation failed", file=sys.stderr)
                print(output.stderr.decode("utf-8"), file=sys.stderr)
                return TestResult(path, STATE.INVALID)
            filename = path.split("\\")[-1].split(".")[0]
            if exec: output = call_cmd([".\\out\\" + filename + ".exe"])
            os.remove(".\\out\\" + filename + ".exe")
        else:
            output = call_cmd([COMPILER_PATH, "-i", path])
        # stdout = output.stdout.decode("utf-8").split('\n')
        stderr = output.stderr.decode("utf-8").split('\n')
        if output.returncode == 101:
            print("The compiler panicked", file=sys.stderr)
            print(output.stderr.decode("utf-8"), file=sys.stderr)
            return TestResult(path, STATE.PANIC)
        if expected_mode == "FAILURE":
            if point_of_failure == "COMPILER":
                if not compare(error_lines, stderr):
                    print("Expected error message not found", file=sys.stderr)
                    print("Expected:", file=sys.stderr)
                    print(error_lines, file=sys.stderr)
                    print("Actual:", file=sys.stderr)
                    print(stderr, file=sys.stderr)
                    return TestResult(path, STATE.FAILURE)
            elif point_of_failure == "RUNTIME" and not exec:
                return TestResult(path, STATE.IGNORED)
            if output.returncode != expected_error_code:
                print("Expected failure, but program succeeded")
                return TestResult(path, STATE.FAILURE)
        elif expected_mode == "SUCCESS":
            if output.returncode != expected_error_code:
                print("Expected success, but program failed")
                return TestResult(path, STATE.FAILURE)
        return TestResult(path, STATE.SUCCESS)

def recompile_compiler() -> None:
    print("Recompiling compiler...")
    cmd = call_cmd(["cargo", "build", "--release"])
    if cmd.returncode != 0:
        print("Failed to recompile compiler", file=sys.stderr)
        print(cmd.stderr.decode("utf-8"), file=sys.stderr)
        sys.exit(1)
    print("Recompilation successful")

def run_all_tests(exec: bool = True):
    total = 0
    failed_tests = []
    panicked_tests = []
    invalid_tests = []
    ignored_tests = []
    paths = []
    for root, _, files in os.walk(".\\tests"):
        for filename in files:
            path = os.path.join(root, filename)
            if os.path.isfile(path):
                paths.append((path, exec))

    with multiprocessing.Pool() as pool:
        results = pool.map(run_test, paths)
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
            recompile_compiler()
            print("Running tests...")
            run_all_tests(not (len(sys.argv) > 2 and sys.argv[2] == "--no-exec"))
        elif mode == "bench":
            recompile_compiler()
            print("Running benchmarks...")
            assert False, "Not implemented"
        else:
            print("Usage: python helper.py [test|bench]")
    # main()