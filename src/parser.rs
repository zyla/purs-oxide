use crate::ast::Module;
use crate::ast::Type;
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

pub fn parse_type(
    input: &str,
) -> (
    Vec<ErrorRecovery<usize, Token, &str>>,
    Result<Type, ParseError<usize, Token, lexer::Error>>,
) {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::TypeParser::new().parse(&mut errors, lexer);
    (errors, result)
}

#[cfg(test)]
mod tests {
    use super::{parse_module, parse_type};
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

    #[test]
    fn test_parse_atomic_type() {
        assert_debug_snapshot!(parse_type("var"), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 3,
                    },
                    Var(
                        Symbol(
                            "var",
                        ),
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("\"string\""), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 8,
                    },
                    TypeLevelString(
                        "string",
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("42"), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 2,
                    },
                    TypeLevelInt(
                        42,
                    ),
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_complex_type() {
        assert_debug_snapshot!(parse_type("Maybe Int"), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 9,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 5,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Maybe",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 6,
                                end: 9,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Int",
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("Either String Int"), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 17,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 13,
                            },
                            TypeApp(
                                Located(
                                    SourceSpan {
                                        start: 0,
                                        end: 6,
                                    },
                                    TypeConstructor(
                                        QualifiedName(
                                            Symbol(
                                                "Either",
                                            ),
                                        ),
                                    ),
                                ),
                                Located(
                                    SourceSpan {
                                        start: 7,
                                        end: 13,
                                    },
                                    TypeConstructor(
                                        QualifiedName(
                                            Symbol(
                                                "String",
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 14,
                                end: 17,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Int",
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("Array (Maybe Int)"), @r###"
        (
            [],
            Ok(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 17,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 5,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Array",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 6,
                                end: 17,
                            },
                            Parens(
                                Located(
                                    SourceSpan {
                                        start: 7,
                                        end: 16,
                                    },
                                    TypeApp(
                                        Located(
                                            SourceSpan {
                                                start: 7,
                                                end: 12,
                                            },
                                            TypeConstructor(
                                                QualifiedName(
                                                    Symbol(
                                                        "Maybe",
                                                    ),
                                                ),
                                            ),
                                        ),
                                        Located(
                                            SourceSpan {
                                                start: 13,
                                                end: 16,
                                            },
                                            TypeConstructor(
                                                QualifiedName(
                                                    Symbol(
                                                        "Int",
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
    }
}
