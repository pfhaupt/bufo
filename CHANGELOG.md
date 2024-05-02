# Changelog
## 2024-05-02
### General
- Add ROADMAP.md
### New Features
- Implement smarter function and type resolver
    - Imported modules don't need to be specified using the `::` token anymore
    - The Typechecker will first scan the current file, and then all imported modules
### Fixes
- Refactor the way modules work
    - The process is now systematic and shouldn't break as easily
    - Introduce ModuleSpecifier
        - All expressions of the form `X::Y` are parsed into those ModuleSpecifiers, instead of a BinaryExpr using `Operation::ModuleAccess`
        - Every Node that needs one has one (e.g. StructLiteralNode, CallNode)
        - Is used to resolve the name for Codegen
- Deprecate `Operation::ModuleAccess`
    - There's no reason why it should be a binary expression instead of unfolding it when parsing
### Known Issues
- Typechecker finds recursive structs when there are none
- There's currently no way to tell if a function was shadowed from another module, the Typechecker always goes first come first served
    - An error, or at least a warning would be cool to tell the user "hey, there are multiple functions in the modules X, Y, Z with this name"
## 2024-03-11
### General
- LLVM is now enabled by default
    - The old Codegen and Assembler haven't been updated since forever, and LLVMCodegen is now stable enough for our purposes
    - Check [here](./notes/llvm.md) for information on how to set up LLVM
- Bump Inkwell to Version 0.4.0
- Remove Fuzzer
    - Whereas this was an interesting concept, maintaining it is very rough
    - Eventually I'd like to switch to a proper fuzzer

### New Features
- Introduce notion of Modules
    - Every file is a module now, and you can import other files
    - Very unstable, and there's no shorthand yet (so even with `import foo` you always need to specify `foo::bar`)
- Introduce a prelude which is imported by default
    - Doesn't support much yet, you can print, you can assert, you can open and close files
    - access functions using `prelude::` (no `import` needed)
- Implement a *very* primitive Bindgen
    - Ultimately we want to use the AST produced by clang and do stuff with that, the current bindgen only parses a very specific subset of C headers (for example raylib! :D)
    - Only accepts preprocessed C headers (`clang -E header.h`) for now
- Introduce `unsafe`
    - Some things are unsafe by definition (either because the Compiler is still janky, or because we call to C), which require an `unsafe {}` block to be called
    - For example, the new type `Any` as a `void*` equivalent is unsafe.
- Introduce C-style `for` loops
    - The construct
    ```
    for (mut i: i32 = 0; i < 10; i = i + 1) {
        $work$
    }
    ```
    is now syntactic sugar for
    ```
    mut i: i32 = 0;
    while (i < 10) {
        $work$
        i = i + 1;
    }
    ```
- Introduce String Literals (finally)
- Introduce Arrays
    - Syntax: `[type; size]`, can be nested: `[[type; size1]; size2]`
    - Zero indexed using `<array>[index]`
    - Out of Bounds Checks are performed at runtime
- Introduce new types:
    - Floating Point Numbers `f32` and `f64`
    - Ascii Characters `char`
    - Integers `i8`, `u8`, `i16`, `u16`
- Introduce `&&` and `||` as logical `and` and `or` respectively
- Introduce Block Comments, `$` is the token that starts and ends a block (subject to change)
- New Compiler Flags
    - `-v`/`--verbose` for verbose output such as how long each step took
    - `--gen-bind` for generating bindings for a C header (WIP)
    - `--emit-llvm` for keeping the generated LLVM IR
    - `--emit-asm` for emitting the generated Assembly

### Fixes
- Implement `break` and `continue` for LLVM
- Stabilize Structs
    - We're now following the Windows ABI for passing structs as function arguments
    - We now use better LLVM IR to create structs
- Fix Memory Leaks introduced by `alloca`ting in loops
    - We now place all `alloca`s in the entry block, which are either optimized away by LLVM's `mem2reg` or only allocated once, which is fine
- The Type Checker functions now return `Result<Type>` instead of `Type`
    - This is to prevent ambiguity whether a return value is an error (and we should stop) or an actual type, i.e. `Type::None` from function calls that return nothing
- More unit tests for everything
    - We can now also check runtime panics such as OutOfBounds

### Known Issues
- Modules are still janky
    - The Type Checker may reject valid code
    - The Codegen might panic because of wrong module paths
- The way we create Arrays is suboptimal and actively segfaults `clang` on my machine after a certain size
- We don't need to nearly `alloca`te as much as we do, there's a lot of performance to gain
- There are still many things that aren't covered by unit tests
- A lot of TODOs in the code, which will be covered in the next round of FIXME-Roulette
