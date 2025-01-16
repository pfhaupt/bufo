# Road to selfhost
## Idea
The selfhosted Bufo compiler will be multistaged:
- Original Rust source code
    - Only bugfixes from now on, the initial Compiler is complete
- Stage 1 Compiler written in Bufo
    - The only thing we really want is to compile Stage 2
    - Super simple handwritten Assembler
- Stage 2 Compiler written in Bufo
    - Basis for further work
    - The powerful thing

## Plan
- Rust
    - [x] Module refactor
- Stage 1
    - [ ] Some way of marking non-existant values
        - we don't have `null` or `Option`
    - [ ] Unsafe Pointer arithmetics (e.g. `*(a + 10)` where `a` is `&i32` or something like that)
        - Useful for Vectors where we `unsafe malloc` and need to insert at a given position
    - [ ] Implement basic std-library
        - No generics for stage 1, we will manually copy all needed Vectors etc.
        - Calling other programs will be needed for `clang`
    - [ ] Codegen by doing own Assembly again
- Stage 2
    - [ ] Generic Types
    - [ ] Enums
    - [ ] Better std-library
        - `Vec<T>` would be nice
        - `Option<T>` and `Result<T, E>` would also be nice
    - [ ] Improve Bindgen by parsing Clang AST JSON
    - [ ] Port LLVM Bindings to Bufo
    - [ ] Codegen via LLVM
    - [ ] ???
    - [ ] Profit
