# Classes
## Factdump
- Class instances are easier to pass around, both in arguments and return values:  
It's a simple pointer to the heap, always 8 bytes in size, always fits in a register
- Needs alloc/free in the backend, and some advanced way of managing memory
    - Garbage Collection?
        - Need extra data structure and overhead for managing GC
    - Drop when out of scope?
        - Problem when assigning 2 instances to 1 variable
        - Fix could be to check reassignment, and free then too?
        - consider  
            ```
            let a: Foo = Foo::new();
            let b: Foo = a;
            a = Foo::new();
            b = Foo::new();
            ```
        - a gets assigned new Pointer X  
        - b gets assigned a -> Pass By Reference, b is X  
        - a gets assigned new Pointer Y  
        - b gets assigned new Pointer Z  
        - Y and Z get freed at the end of the scope, but what happens to X?
        - Could introduce lifetimes: Pointer X is never used after line 2, so we can free it
            - Sounds complicated tho
- Example class Definition:  
    ```
    class Test {
        a: i32;
        b: i32;
        func new(na: i32, nb: i32) -> Test {
            this.a = na;
            this.b = nb;
            return this;
        }

        func getA(this: Test) -> i32 { return this.a; }
        feat str(this: Test) -> String {
            // Implement toString in here, however that may look like
        }
    }
    func main() {
        let t: Test = Test::new(5, 10);
        let a: i32 = t.getA();
        print(t); // No print yet, but this would invoke t.str() aka Test::str(t)
    }
    ```
- Classes should behave just like in other OOP languages: You can define a constructor, you can set attributes, you can define class-specific functions
- Additionally, I'd like to introduce `feat`s:
    - Optional features that can be implemented for a class
    - `feat str() -> String` to stringify the class
    - `feat index(i: Array)` to index a class (For example, String may return the char at index i) and allow `Class[i]` syntax
    - `feat len() -> usize` to get the length of a class, i.e. Array, String, or just any other collection
    - `feat add(other: ...) / feat sub(other: ...)` to overload Addition, etc.
    - and many more
- Resolving method calls:
    - When parsing the class, generate namespace for that class
        - `Test::getA()`, `Foo::getA()`, etc should not collide
    - Get type of instance, and syntactically transform `t.getA()` to `Test::getA(t)`, then treat as normal function
    - When trying to use a feat that was not implemented (f.e. `t[1]`), tell user that feat is missing
- Operator Overloading:
    - `5 + 4` can be syntactically transformed into `i32::add(5, 4)`
    - `(4usize - 2) * 5` can be transformed into `usize::mult(usize::sub(4, 2), 5)`
    - `t1 / t2` can be transformed into `Test::div(t1, t2)`, assuming `t1` and `t2` are instances of Test
    - Type-Checker will prevent any illegal operations, altho it needs to have the context of which `feat`s are implemented (well, that should be easy to add, it already knows of normal functions)