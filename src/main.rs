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
                let mut flags = crate::frontend::flags::Flags::default();
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

        generate_failing_test!(missing_semicolon, "Expected Semi");
        generate_failing_test!(unclosed_parens, "Expected ClosingCurly");
        generate_failing_test!(mismatched_parens, "found OpenCurly");
        generate_failing_test!(reserved_keyword_as_var_name, "found FunctionKeyword");
        generate_failing_test!(reserved_keyword_as_expr, "found ClassKeyword");
        generate_failing_test!(invalid_variable_name_start_with_digit, "Expected Identifier", "found IntLiteral");
        generate_failing_test!(invalid_variable_name_contains_special, "Unexpected Symbol");
        generate_failing_test!(missing_arguments_in_func_call, "Not enough arguments");
        generate_failing_test!(extra_arguments_in_func_call, "Too many arguments");
        generate_failing_test!(operator_without_operands, "Expected Expr");
        generate_failing_test!(too_many_literals, "found IntLiteral");
        generate_failing_test!(if_missing_body, "Expected OpenCurly");
        generate_failing_test!(if_missing_condition, "Expected Expr");
        generate_failing_test!(if_missing_brackets_condition, "Expected OpenRound", "found Identifier");
        generate_failing_test!(while_missing_body, "Expected OpenCurly");
        generate_failing_test!(while_missing_condition, "Expected Expr");
        generate_failing_test!(while_missing_brackets_condition, "Expected OpenRound", "found Identifier");
        generate_failing_test!(unexpected_symbol, "Unexpected Symbol `#`");
        generate_failing_test!(char_literal_more_than_one_chars, "Char Literal", "single char", "found 'hello'");
        generate_failing_test!(brackets_in_expressions, "Expected ClosingRound");
        generate_failing_test!(else_missing_if, "found ElseKeyword", "Expr");
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

        generate_failing_test!(undeclared_variable, "Undeclared variable");
        generate_failing_test!(unknown_type_declaration, "Unknown Type", "`random`");
        generate_failing_test!(type_mismatch_in_binary_op, "Type Mismatch", "LHS", "RHS", "i32", "u32");
        generate_failing_test!(type_mismatch_in_comparison, "Type Mismatch", "LHS", "RHS", "i32");
        generate_failing_test!(type_mismatch_in_fn_args, "Type Mismatch", "Parameter", "declared here", "i32", "Expected type");
        generate_failing_test!(type_mismatch_in_assignment, "Type Mismatch", "Expected type", "u64", "found", "i32");
        generate_failing_test!(function_redeclaration, "Function redeclaration", "Function already declared here");
        generate_failing_test!(wrong_function_argument_types, "Type Mismatch", "in argument evaluation");
        generate_failing_test!(wrong_function_return_type, "Type Mismatch", "Function is declared to return `i32`");
        generate_failing_test!(calling_undeclared_function, "unknown function", "testfunction");
        generate_failing_test!(return_mismatch, "Type Mismatch", "Function", "declared to return", "found");
        generate_failing_test!(using_out_of_scope_variable, "Undeclared variable");
        generate_failing_test!(variable_redeclaration, "Variable redeclaration", "already declared here");
        generate_failing_test!(class_field_redeclaration, "Field redeclaration", "Field", "already declared here");
        generate_failing_test!(class_no_such_field, "has no field", "Class declared here");
        generate_failing_test!(class_nested_field_wrong_type, "Type Mismatch", "Expected type", "found type");
        generate_failing_test!(class_no_feat_new, "no constructor", "feature", CONSTRUCTOR_NAME, "in class");
        generate_failing_test!(incompatible_operands, "Binary Operation", "not defined", "class", "context");
        generate_failing_test!(if_no_comparison, "if-condition", "comparison");
        generate_failing_test!(while_no_comparison, "while-condition", "comparison");
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
