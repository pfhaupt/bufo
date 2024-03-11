# General
- `inkwell` and `llvm-sys` are using the LLVM C API to generate LLVM IR
- We're using `clang` to compile our generated LLVM IR into an executable
# Installation
## Windows
- Go to [this cool repository](https://github.com/mun-lang/llvm-package-windows/releases) and download Version 16.0.5
    - From my experiments, md/mt and msvc16/17 does not matter, any work right now
- Unpack to a directory of your choice
- Add that directory to your PATH
## Linux
- TBD

# Checking that everything went right
- `clang --version` should print `clang version 16.0.5`
- `llvm-config` should print `16.0.5`
- `rustc -vV` should contain a line that starts with `host:`