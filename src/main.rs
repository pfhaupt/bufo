mod backend;
mod compiler;
mod frontend;
mod middleend;
mod util;

// This macro injects tracing code into the compiler.
// Must be enabled with the `trace` feature-flag.
extern crate tracer;

fn main() {
    crate::compiler::run();
}

#[cfg(test)]
mod deprecated_test {
    #[test]
    fn abort() {
        panic!("We are using our own test framework now. Please use `helper.py` to run tests.")
    }
}