# Bufo
## Overview
Bufo is a programming language that I created for my own personal exploration and experimentation.  
It features a semi-unique syntax, with a focus on imperative and functional programming that encourages creativity and experimentation.

It is not intended to be fast or special (this may be subject to change).

This project is Part 2 of my journey, the first being [HauptLang](https://github.com/MrLagSux/HauptLang).

**All features are highly experimental and might change at any time.**

## Milestones/Roadmap
- [x] Type System
- [ ] Functional Programming capabilities
- [ ] Self-hosting
- [ ] Optimizations

## Quick Start
### Build
Run `cargo build [--release]` to compile the bufo-compiler. You can then find it in `./target/`.
### Usage
Run `bufo -i <input.bu> [flags]` to compile the input file.  
Specify `-d` to generate debug info.  
Specify `-r` to run the code after the compiler has finished.

## Language Support
- Variables:  
`let name: i32 = 15;` assigns `15` to a variable called `name` of type `i32`.  
The compiler checks re-definitions of variables, re-assignments are always possible.  
Variable types are checked at compile time, statements like `let a: i32 = 5u32;` will throw an error.  
Built-in types: `i32, i64, u32, u64, usize`  
Currently there is no way to cast types.
- The language has (multi-dimensional) array support:  
`let arr: i32[2, 3] = [[1, 2, 3], [4, 5, 6]];` creates a `2x3` array and assigns it to `arr`.  
Use `arr[x, y]` to index an element. In this example, `arr[1, 1]` gets the value 5.  
Out-of-bounds checks are performed at runtime.  
Note: Arrays are zero-indexed.
- Functions:  
`func foo(a: i32, x: u32) -> i64 { ... }` defines a function that takes in 2 arguments, one of type `i32`, one of type `u32`, and returns a value of type `i64`.  
Functions inside of functions are not supported yet.
- Supports if-else-statements.  
- `main()` is the entrypoint of the program.
- Currently there are no global variables. On a global level you can only define functions.
