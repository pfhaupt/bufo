#![allow(unused, unreachable_code)]

mod frontend;
mod middleend;
mod backend;
mod compiler;

fn main() {
    crate::compiler::run();
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;

    use crate::compiler::{Compiler, ERR_STR};

    const ALWAYS_FAILS: &str = "This is a String we defined to make sure that a test always fails. This is expected.";

    macro_rules! init {
        ($path: expr, $debug: expr, $run: expr) => {
            Compiler::new(&String::from($path), $debug, $run)
        };
    }

    macro_rules! fail {
        ($path: expr, $expected: expr) => {
            test!($path, false, false, true, $expected)
        };
    }

    macro_rules! pass {
        ($path: expr) => {
            test!($path, false, false, false, [""])
        };
    }

    macro_rules! test {
        ($path: expr, $debug: expr, $run: expr, $should_fail: expr, $expected: expr) => {
            match init!($path, $debug, $run) {
                Ok(mut c) => {
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
                }
                Err(err) => {
                    assert!($should_fail);
                    for e in $expected {
                        if !err.contains(e) {
                            assert!(false, "Unexpected Error String!\nExpected Substring `{e}`\nin `{err}`")
                        }
                    }
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
        generate_failing_test!(unexpected_symbol, "Unexpected Symbol `#`");
        generate_failing_test!(char_literal_more_than_one_chars, "Char Literal", "single char", "found 'hello'");
        generate_failing_test!(brackets_in_expressions, "Expected ClosingRound");
    }

    mod semantic_tests {
        use crate::compiler::{Compiler, ERR_STR};
        use crate::tests::ALWAYS_FAILS;

        macro_rules! generate_failing_test {
            ($name:ident, $($err:expr),*) => {
                #[test]
                fn $name() {
                    fail!(concat!("tests/semantics/", stringify!($name), ".bu"), [ERR_STR, $($err),*])
                }
            };
        }

        generate_failing_test!(undeclared_variable, "Undeclared variable");
        generate_failing_test!(unknown_type_declaration, "Unknown Type", "`random`");
        generate_failing_test!(type_mismatch_in_binary_op, "Type Mismatch", "LHS", "RHS", "U32");
        generate_failing_test!(type_mismatch_in_comparison, "Type Mismatch", "LHS", "RHS", "I32");
        generate_failing_test!(type_mismatch_in_fn_args, "Type Mismatch", "Parameter", "declared here", "I32", "Expected type");
        generate_failing_test!(type_mismatch_in_assignment, "Type Mismatch", "Expected type", "U64", "found", "I32");
        generate_failing_test!(function_redeclaration, "Function redeclaration", "Function already declared here");
        generate_failing_test!(wrong_function_argument_types, "Type Mismatch", "in argument evaluation");
        generate_failing_test!(wrong_function_return_type, "Type Mismatch", "Function is declared to return `I32`");
        generate_failing_test!(calling_undeclared_function, "unknown function", "testfunction");
        // #[test] #[cfg_attr(not(feature = "test_exec"), ignore = "Pass the `text_exec` feature-flag to run this test")] fn array_out_of_bounds() {
        //     // TODO: Automatically clean up after running code
        //     test!("tests/semantics/array_out_of_bounds.bu", false, true, true, [ERR_STR, "Code execution failed", format!("{:X}", (ExitCode::OobAccess as usize)).as_str()])
        // }
        // generate_failing_test!(return_mismatch, "Type Mismatch", "Function", "declared to return", "found");
        // #[test] #[cfg_attr(not(feature = "test_exec"), ignore = "Pass the `text_exec` feature-flag to run this test")] fn variable_shadowing() {
        //     // TODO: Automatically clean up after running code
        //     test!("tests/semantics/variable_shadowing.bu", false, true, true, [ERR_STR, "Code execution failed", format!("{:X}", 42069).as_str()])
        // }
        generate_failing_test!(using_out_of_scope_variable, "Undeclared variable");

        generate_failing_test!(variable_redeclaration, "Variable redeclaration", "already declared here");
        generate_failing_test!(class_field_redeclaration, "Field redeclaration", "Field", "already declared here");
        generate_failing_test!(class_no_such_field, "has no field", "Class declared here");
        generate_failing_test!(class_nested_field_wrong_type, "Type Mismatch", "Expected type", "found type");
        // #[test] #[ignore = "Error Messages for missing feats are still showing as missing [desugared] functions"] fn class_no_feat_new() {
        //     // TODO: Implement generate_failing_test for this once error messages are better
        // }
        generate_failing_test!(class_no_feat_new, "no constructor", "feature `new`", "in class");
        generate_failing_test!(incompatible_operands, "Binary Operation", "not defined", "class", "context");

        #[test] #[ignore = "NullPointer are still not checked at runtime (very bad)"] fn null_pointer_exception() {
            // TODO: Implement generate_failing_test for this once Nullpointer are handled at runtime
            // test!("tests/semantics/null_pointer_exception.bu", false, true, true, [ERR_STR, ALWAYS_FAILS])
        }
    }
}