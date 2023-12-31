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

#[rustfmt::skip]
#[cfg(test)]
mod tests {
    const ALWAYS_FAILS: &str =
        "This is a String we defined to make sure that a test always fails. This is expected.";

    macro_rules! init {
        ($path: expr, $debug: expr, $run: expr) => {
            {
                let mut flags = crate::util::flags::Flags::default();
                flags.input = $path.to_string();
                flags.debug = $debug;
                flags.run = $run;
                Compiler::new(flags)
            }
        };
    }

    macro_rules! fail {
        ($path: expr, $expected: expr) => {
            test!($path, false, false, true, $expected)
        };
    }

    macro_rules! fail_runtime {
        ($path: expr, $exit_code: expr) => {
            test!(
                $path,
                false,
                true,
                true,
                [crate::compiler::ERR_STR, "Code execution failed", $exit_code]
            )
        };
    }

    #[allow(unused)]
    macro_rules! pass {
        ($path: expr) => {
            test!($path, false, false, false, [""])
        };
    }

    macro_rules! clean_up {
        ($path: expr) => {
            use std::fs;
            let mut path = String::from("./out/") + $path.split('/').last().unwrap();
            path = path.replace(".bu", ".asm");
            fs::remove_file(path.clone()).unwrap();
            path = path.replace(".asm", ".obj");
            fs::remove_file(path.clone()).unwrap();
            path = path.replace(".obj", ".exe");
            fs::remove_file(path).unwrap();
        };
    }

    macro_rules! test {
        ($path: expr, $debug: expr, $run: expr, $should_fail: expr, $expected: expr) => {
            {
                let mut c = init!($path, $debug, $run);
                let result = c.run_everything();
                if $should_fail {
                    assert!(result.is_err());
                    let res = result.err().unwrap();
                    for e in $expected {
                        if !res.contains(e) {
                            assert!(false, "Unexpected Error String!\nExpected Substring\n> {e}\nin error message\n{res}\n")
                        }
                    }
                } else {
                    assert!(result.is_ok());
                }
                if $run {
                    clean_up!($path);
                }
            }
        };
    }

    mod syntax_tests {
        use crate::compiler::{Compiler, ERR_STR};

        macro_rules! generate_failing_test {
            ($name:ident, $($err:expr),*) => {
                #[test]
                fn $name() {
                    fail!(concat!("tests/syntax/", stringify!($name), ".bu"), [ERR_STR, $($err),*])
                }
            };
        }

        generate_failing_test!(missing_semicolon, "Expected `;`");
        generate_failing_test!(unclosed_parens, "Expected `}`");
        generate_failing_test!(mismatched_parens, "found `{`");
        generate_failing_test!(reserved_keyword_as_var_name, "found `func`");
        generate_failing_test!(reserved_keyword_as_expr, "found `class`");
        generate_failing_test!(invalid_variable_name_start_with_digit, "Expected Identifier", "found Integer Literal");
        generate_failing_test!(invalid_variable_name_contains_special, "Unexpected Symbol");
        generate_failing_test!(missing_arguments_in_func_call, "Not enough arguments");
        generate_failing_test!(extra_arguments_in_func_call, "Too many arguments");
        generate_failing_test!(operator_without_operands, "Expected Expression");
        generate_failing_test!(too_many_literals, "found Integer Literal");
        generate_failing_test!(if_missing_body, "Expected `{`");
        generate_failing_test!(if_missing_condition, "Expected Expression");
        generate_failing_test!(if_missing_brackets_condition, "Expected `(`", "found Identifier");
        generate_failing_test!(while_missing_body, "Expected `{`");
        generate_failing_test!(while_missing_condition, "Expected Expression");
        generate_failing_test!(while_missing_brackets_condition, "Expected `(`", "found Identifier");
        generate_failing_test!(unexpected_symbol, "Unexpected Symbol `#`");
        generate_failing_test!(char_literal_more_than_one_chars, "Char Literal", "'hello'");
        generate_failing_test!(brackets_in_expressions, "Expected `)`");
        generate_failing_test!(else_missing_if, "found `else`", "Expression");
        generate_failing_test!(many_err, "Expected Identifier, found `;`", "Expected `:`, found Identifier", "Expected Identifier, found `(`", "Expected Expression, found `)`", "Expected `:`, found `}`", "Expected `}`, found End of File");
    }

    mod semantic_tests {
        use crate::compiler::{Compiler, ERR_STR, CONSTRUCTOR_NAME};
        use crate::tests::ALWAYS_FAILS;

        // FIXME: Separate runtime tests from semantic tests
        // FIXME: For runtime tests, differentiate between "working, returned value" and "working, expected crash"
        //        (e.g. null pointer exception is an expected crash)
        //        (e.g. while loop returns a value as exit code)

        macro_rules! generate_failing_test {
            ($name:ident, $($err:expr),*) => {
                #[test]
                fn $name() {
                    fail!(concat!("tests/semantics/", stringify!($name), ".bu"), [ERR_STR, $($err),*])
                }
            };
        }

        macro_rules! generate_runtime_failing_test {
            ($name:ident, $($err:expr),*) => {
                #[test]
                #[cfg_attr(
                    not(feature = "test_exec"),
                    ignore = "Pass the `test_exec` feature-flag to run this test"
                )]
                fn $name() {
                    fail_runtime!(concat!("tests/semantics/", stringify!($name), ".bu"), $($err),*)
                }
            };
            () => {
                
            };
        }

        generate_failing_test!(undeclared_variable, "undeclared variable", "`b`");
        generate_failing_test!(unknown_type_declaration, "Unknown type", "`random`");
        generate_failing_test!(type_mismatch_in_binary_op, "Type mismatch", "LHS", "RHS", "i32", "u32");
        generate_failing_test!(type_mismatch_in_comparison, "Type mismatch", "LHS", "RHS", "i32");
        generate_failing_test!(type_mismatch_in_fn_args, "Type mismatch", "Parameter `a` is declared to be", "`i32`", "Argument is expected to be", "found type `u32`");
        generate_failing_test!(type_mismatch_in_assignment, "Type mismatch", "Expected type", "u64", "found", "i32");
        generate_failing_test!(function_redeclaration, "Function redeclaration", "Function `bla` already declared here");
        generate_failing_test!(wrong_function_return_type, "Type mismatch", "Function is declared to return `i32`");
        generate_failing_test!(calling_undeclared_function, "undeclared function", "testfunction");
        generate_failing_test!(return_mismatch, "Type mismatch", "Function", "declared to return", "found");
        generate_failing_test!(using_out_of_scope_variable, "undeclared variable", "`a`");
        generate_failing_test!(variable_redeclaration, "Variable redeclaration", "already declared here");
        generate_failing_test!(class_field_redeclaration, "Field redeclaration", "Field", "already declared here");
        generate_failing_test!(class_no_such_field, "unknown field", "Variable `this`", "Class `Test`", "is declared here");
        generate_failing_test!(class_nested_field_wrong_type, "Type mismatch", "Expected type", "found type");
        generate_failing_test!(class_no_feat_new, "no constructor", "feature", CONSTRUCTOR_NAME, "Class `Test`");
        generate_failing_test!(incompatible_operands, "binary expression", "not defined", "Operation `Test + Test`", "LHS has type", "RHS has type");
        generate_failing_test!(if_no_comparison, "Expected type `bool`", "found type `i32`");
        generate_failing_test!(while_no_comparison, "Expected type `bool`", "found type `i32`");
        generate_failing_test!(class_constructor_wrong_return_type, "Feature", CONSTRUCTOR_NAME, "is expected to return None", "found", "i32", "implicit", "should not be specified", "implicit return type for constructor");
        generate_runtime_failing_test!(null_pointer_exception, format!("{:X}", 2).as_str());
        generate_runtime_failing_test!(array_out_of_bounds, ALWAYS_FAILS);
        generate_runtime_failing_test!(variable_shadowing, format!("{:X}", 42069).as_str());
        generate_runtime_failing_test!(if_else_flow, "1");
        generate_runtime_failing_test!(if_expression, format!("{:X}", 1337).as_str());
        generate_runtime_failing_test!(if_flow, format!("{:X}", 1290).as_str());
        generate_runtime_failing_test!(nested_if, format!("{:X}", 54).as_str());
        generate_runtime_failing_test!(while_expression, format!("{:X}", 1337).as_str());
        generate_runtime_failing_test!(while_flow, format!("{:X}", 10).as_str());
        generate_runtime_failing_test!(nested_while, format!("{:X}", 10000).as_str());
    }
}
