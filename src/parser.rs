use crate::ast::Module;
use crate::lexer;
use crate::token::Token;
use lalrpop_util::ErrorRecovery;
use lalrpop_util::ParseError;

lalrpop_mod!(pub parser);

pub fn parse_module(
    input: &str,
) -> (
    Vec<ErrorRecovery<usize, Token, &str>>,
    Result<Module, ParseError<usize, Token, lexer::Error>>,
) {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ModuleParser::new().parse(&mut errors, lexer);
    (errors, result)
}

#[cfg(test)]
mod tests {
    use super::parse_module;
    use indoc::indoc;
    use insta::{self, assert_debug_snapshot};

    #[test]
    fn test_module_header() {
        assert_debug_snapshot!(parse_module(indoc!("
        module Foo where
        ")), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 16,
                    },
                    Commented(
                        [],
                        ModuleInner {
                            name: Symbol(
                                "Foo",
                            ),
                            exports: None,
                            declarations: [],
                        },
                    ),
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_simple_value_decl() {
        assert_debug_snapshot!(parse_module(indoc!("
        module Foo where
        x = 1
        ")), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 23,
                    },
                    Commented(
                        [],
                        ModuleInner {
                            name: Symbol(
                                "Foo",
                            ),
                            exports: None,
                            declarations: [
                                Located(
                                    SourceSpan {
                                        start: 17,
                                        end: 22,
                                    },
                                    Commented(
                                        [],
                                        ValueDeclaration(
                                            ValueDeclaration {
                                                ident: Symbol(
                                                    "x",
                                                ),
                                                expr: [
                                                    GuardedExpr {
                                                        guards: [],
                                                        expr: Located(
                                                            SourceSpan {
                                                                start: 21,
                                                                end: 22,
                                                            },
                                                            Literal(
                                                                Integer(
                                                                    1,
                                                                ),
                                                            ),
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    ),
                                ),
                            ],
                        },
                    ),
                ),
            ),
        )
        "###);
    }
}
