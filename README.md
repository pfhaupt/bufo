# bufo
## Overview
bufo is a programming language that I created for my own personal exploration and experimentation.

It features a semi-unique syntax, with a focus on imperative and functional programming that encourages creativity and experimentation.

The name is a reference to the [common toad](https://en.wikipedia.org/wiki/Common_toad), as its latin name is [bufo bufo](./src/bufo.bufo).

## Milestones/Roadmap
- [x] Self-hosting
- [x] Functional Programming capabilities
- [x] Arbitrary code execution at compile time
- [x] Unions and Pattern Matching
- [x] Inline Assembly
- [ ] Macros
- [ ] Generics

## Quick Start
Note: If you want to build the compiler, check below this section.

bufo is a statically typed and compiled low-level general purpose programming language. If you come from languages like C or Rust, you'll pick up the language pretty quickly.

To get a quick overview over the language features, take a look at the [how_to](./how_to/) directory.

## Building the compiler
### Dependencies
- LLVM 20.1.7
    - If your system doesn't provide LLVM as a package, you can use my [LLVM factory](https://github.com/pfhaupt/llvm-factory) to build it yourself.
### If you already have a copy of bufo
Simply run `make`, the Makefile will do the rest.
```sh
$ make
```
It should be mentioned that the Makefile assumes a pretty normal Unix environment, even though I personally use Windows. Before it runs the actual build script, it verifies that all necessary tools are available on your machine.

To run the test suite, you can run the following command. By default the suite compiles the compiler, `--dont-compile` instructs the suite to not do that.
```sh
$ python helper.py test [--dont-compile]
```

### Bootstrap - If you don't have a copy
The compiler is [selfhosted](https://en.wikipedia.org/wiki/Self-hosting_(compilers)). This means that the compiler is written in bufo itself, without using any other programming languages or compilers to fall back to.

The compiler supports transpiling bufo code to C. As a result, the compiler also has a version "written" in C. You can find it in `./bootstrap/`. It is a single self-contained file.

Depending on your operating system and preferred C compiler, run any of the provided scripts in the bootstrap directory from the root of the project.
They will generate the initial bootstrap compiler, which you can then use to compile the real version.

For example, to compile the compiler on Windows with cl.exe, run:
```console
$ python ./llvm.py
$ .\bootstrap\bufo_windows.c.cl.bat
[INFO] Successfully generated bootstrap/bufo_windows.c.exe
$ .\bootstrap\bufo_windows.c.exe src\bufo.bufo -o .\bufo_windows.exe
$ .\bufo_windows.exe --help
```

As another example, to compile the compiler on Linux with clang, run:
```console
$ python3 ./llvm.py
$ ./bootstrap/bufo_linux.c.clang.sh
[INFO] Successfully generated bootstrap/bufo_linux.c.exe
$ ./bootstrap/bufo_linux.c.exe ./src/bufo.bufo -o ./bufo_linux
$ ./bufo_linux --help
```
