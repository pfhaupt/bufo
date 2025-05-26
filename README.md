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
- [ ] Closures
- [ ] Macros
- [ ] Generics
- [ ] Inline Assembly and/or Inline Machine Code

## Quick Start
Note: If you want to build the compiler, check below this section.

bufo is a statically typed and compiled low-level general purpose programming language. If you come from languages like C or Rust, you'll pick up the language pretty quickly.

To get a quick overview over the language features, take a look at the [how_to](./how_to/) directory.

## Building the compiler
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

Bootstrapping is currently not supported, however it is a high priority. Once such a mechanism exists, bootstrapping the compiler will be as simple as running
```sh
$ make bootstrap
```
in the terminal.
