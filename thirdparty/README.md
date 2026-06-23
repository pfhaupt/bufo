# thirdparty repos
Bufo makes use of some thirdparty tools besides LLVM. Those are optional.

If you want to use certain features of the language, you need local builds of them in this directory.

If you find a dependency that's not listed here, please hit me up with an Issue.

## tracy
- https://github.com/wolfpld/tracy
  - Needed for `--trace`. Check `./thirdparty/tracy.bufo` for the exact libraries needed.
  - Tested with Tracy 0.13.1.
