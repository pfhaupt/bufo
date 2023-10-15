#![allow(unused, unreachable_code)]

mod checker;
mod codegen;
mod flags;
mod lexer;
mod parser;
mod desugar;

use std::time::Instant;

use flags::RUN_KEY;

use crate::checker::TypeChecker;
use crate::codegen::Generator;
use crate::flags::{Flag, FlagParser, DEBUG_KEY, INPUT_KEY};
use crate::lexer::Lexer;
use crate::parser::{Parser, Tree};
use crate::desugar::Desugarer;

pub struct Compiler {
    lexer: Lexer,
    parser: Parser,
    desugarer: Desugarer,
    checker: TypeChecker,
    codegen: Generator,
    debug: bool,
    run: bool
}

impl Compiler {
    pub fn new(path: &String, debug: bool, run: bool) -> Result<Self, String> {
        Ok(Self {
            lexer: Lexer::new().origin(path)?.debug(debug),
            parser: Parser::new().origin(path).debug(debug),
            desugarer: Desugarer::new(),
            checker: TypeChecker::new(debug),
            codegen: Generator::new(debug),
            debug,
            run
        })
    }

    pub fn parse_snippet(origin: &String, snippet: &String) -> Result<Tree, String> {
        let mut lexer = Lexer::new();
        lexer.set_source(snippet);
        lexer.set_origin_unchecked(origin);
        lexer.tokenize()?;

        let mut parser = Parser::new().origin(origin);
        parser.set_tokens(lexer.get_tokens());
        let parsed_ast = parser.parse_snippet()?;

        let mut desugarer = Desugarer::new();
        desugarer.desugar_tree(parsed_ast)
    }

    pub fn run_everything(&mut self) -> Result<(), String> {
        let now = Instant::now();
        self.lexer.tokenize()?;
        let tokens = self.lexer.get_tokens();
        if self.debug {
            println!("Lexing took {:?}", now.elapsed());
        }
        
        let now = Instant::now();
        self.parser.set_tokens(tokens);
        let parsed_ast = self.parser.parse_file()?;
        if self.debug {
            println!("Parsing took {:?}", now.elapsed());
        }
        
        let now = Instant::now();
        let mut desugared_ast = self.desugarer.desugar_tree(parsed_ast)?;
        if self.debug {
            println!("Desugaring took {:?}", now.elapsed());
        }
        
        let now = Instant::now();
        self.checker.set_ast(desugared_ast);
        let checked_ast = self.checker.type_check_program()?;
        if self.debug {
            println!("Type Checking took {:?}", now.elapsed());
        }

        let now = Instant::now();
        self.codegen.set_ast(checked_ast);
        self.codegen.generate_code()?;
        if self.debug {
            println!("Codegen took {:?}", now.elapsed());
        }
        // todo!();
        
        let now = Instant::now();
        self.codegen.compile()?;
        if self.debug {
            println!("Compiling took {:?}", now.elapsed());
        }
        if self.run {
            let now = Instant::now();
            self.codegen.run()?;
            if self.debug {
                println!("Running took {:?}", now.elapsed());
            }
        }

        Ok(())
    }
}

fn compile() -> Result<(), String> {
    let now = Instant::now();
    let flags = FlagParser::init_flags().parse_flags()?;

    let path = match flags.get(INPUT_KEY).unwrap() {
        Flag::Input { path } => path.as_ref().unwrap(),
        _ => unreachable!(),
    };
    let run = match flags.get(RUN_KEY).unwrap() {
        Flag::Run { run } => *run,
        _ => unreachable!(),
    };
    let debug = match flags.get(DEBUG_KEY).unwrap() {
        Flag::Debug { debug } => *debug,
        _ => unreachable!(),
    };
    if debug {
        println!("Parsing flags took {:?}", now.elapsed());
    }

    let mut compiler = Compiler::new(path, debug, run)?;
    compiler.run_everything()?;
    Ok(())
}

fn main() {
    if let Err(e) = compile() {
        println!("{}", e);
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;

    use crate::{Compiler, codegen::ERR_STR};

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
        use crate::{Compiler, codegen::ERR_STR};

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
        generate_failing_test!(missing_arguments_in_func_call, "Too few arguments");
        generate_failing_test!(extra_arguments_in_func_call, "Too many arguments");
        generate_failing_test!(operator_without_operands, "Expected Expr");
        generate_failing_test!(too_many_literals, "found IntLiteral");
        generate_failing_test!(if_missing_body, "Expected `{`");
        generate_failing_test!(if_missing_condition, "Expected Expr");
        generate_failing_test!(if_missing_brackets_condition, "Expected OpenRound", "found Identifier");
        generate_failing_test!(unexpected_symbol, "Unexpected Symbol `#`");
        generate_failing_test!(char_literal_more_than_one_chars, "Char Literal", "single char", "got `hello`");
        generate_failing_test!(brackets_in_expressions, "Expected ClosingRound");
    }

    mod semantic_tests {
        use crate::{Compiler, codegen::{ERR_STR, ExitCode}};
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
        generate_failing_test!(unknown_type_declaration, "Attempted to assign");
        generate_failing_test!(type_mismatch_in_binary_op, "Type Mismatch!", "Left hand side", "right side", "U32");
        generate_failing_test!(type_mismatch_in_comparison, "Type Mismatch!", "Left hand side", "right side", "I32");
        generate_failing_test!(type_mismatch_in_fn_args, "Type Mismatch", "Parameter", "declared here", "I32", "Expected type");
        generate_failing_test!(type_mismatch_in_assignment, "Type Mismatch", "Expected type", "U64", "got", "I32");
        generate_failing_test!(function_redeclaration, "Function redeclaration", "Function already declared here");
        generate_failing_test!(wrong_function_argument_types, "Type Mismatch!", "Error when evaluating type of argument.");
        generate_failing_test!(wrong_function_return_type, "Type Mismatch!", "Function is declared to return `I32`");
        generate_failing_test!(calling_undeclared_function, "Unknown function", "testfunction");
        #[test] #[cfg_attr(not(feature = "test_exec"), ignore = "Pass the `text_exec` feature-flag to run this test")] fn array_out_of_bounds() {
            // TODO: Automatically clean up after running code
            test!("tests/semantics/array_out_of_bounds.bu", false, true, true, [ERR_STR, "Code execution failed", format!("{:X}", (ExitCode::OobAccess as usize)).as_str()])
        }
        generate_failing_test!(return_mismatch, "Type Mismatch!", "Function", "declared to return", "got");
        #[test] #[cfg_attr(not(feature = "test_exec"), ignore = "Pass the `text_exec` feature-flag to run this test")] fn variable_shadowing() {
            // TODO: Automatically clean up after running code
            test!("tests/semantics/variable_shadowing.bu", false, true, true, [ERR_STR, "Code execution failed", format!("{:X}", 42069).as_str()])
        }
        generate_failing_test!(using_out_of_scope_variable, "Undeclared variable");

        generate_failing_test!(variable_redeclaration, "Variable redeclaration", "Variable already declared here");
        generate_failing_test!(class_field_redeclaration, "Class Field redeclaration", "Field", "already declared here");
        generate_failing_test!(class_no_such_field, "has no field", "Class declared here");
        generate_failing_test!(class_nested_field_wrong_type, "Type Mismatch!", "Expected type", "got type");
        #[test] #[ignore = "Error Messages for missing feats are still showing as missing [desugared] functions"] fn class_no_feat_new() {
            // TODO: Implement generate_failing_test for this once error messages are better
        }
        generate_failing_test!(incompatible_operands, "Binary Operation", "not defined for type", "Class");

        #[test] #[ignore = "NullPointer are still not checked at runtime (very bad)"] fn null_pointer_exception() {
            // TODO: Implement generate_failing_test for this once Nullpointer are handled at runtime
            // test!("tests/semantics/null_pointer_exception.bu", false, true, true, [ERR_STR, ALWAYS_FAILS])
        }
    }
}